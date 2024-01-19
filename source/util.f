! $Header: m:/default/source/RCS/util.f,v 1.338 2021/05/11 14:00:55 LC2 Exp $
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     UTIL: MAIN DRIVER FOR EMM
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
      SUBROUTINE UTIL
!
      IMPLICIT NONE
!
!     THIS SUBROUTINE IS THE EMM ROUTINE CALLED FROM MAIN

      include 'parametr'
      include 'ncntrl'
      include 'qblk'
      include 'mpblk'
      include 'emmparm'
      include 'emission'
      include 'uefpout'     ! UEFP output variables
      include 'uefdout'     ! EFD output variables
      include 'udatout'     ! UDAT output variables
      include 'uecpout'     ! UECP output variables
      include 'uldsmout'    ! LDSM output variables
      include 'uettout'     ! UETT output variables
      include 'ecpcntl'
      include 'control'
      include 'entcntl'
      include 'dispin'
      include 'dispinyr'
      include 'dispout'
      include 'dispuse'
      include 'elshrs'
      include 'bildin'
      include 'bildout'
      include 'bldctl'
      include 'efpgen'
      include 'dispett'
      include 'cogen'
      include 'wrenew'
      include 'xpq'
      include 'elcntl'
      include 'elout'
      include 'eusprc'
      include 'efpint'
      include 'efpout'
      include 'dsmdimen'
      include 'dsmsectr'
      include 'dsmhelm'
      include 'dsmtfecp'
      include 'edbdef'
      include 'plntctl'
      include 'emoblk'
      include 'epmbank'
      include 'ecp_coal'
      include 'macout'
      include 'cdsparms'
      include 'coalemm'
      include 'dsmunits' !<< include file with unit number for ldsm message file
      include 'dsmfmgrd' !<< file_mgr variables declarations
      include 'csapr'
      include 'steoblock' !<< include file to access ESTCU_ ----AKN
      include 'emmemis'
!
      COMMON /RSVMRG/ EPMRMIN
      REAL EPMRMIN(MNUMNR)
!
      COMMON /ECPRM/ RMDMD,RMADJ,RMCAP,RMLEV
      REAL*4 RMDMD(MNUMNR,MNUMYR)
      REAL*4 RMADJ(MNUMNR,MNUMYR)
      REAL*4 RMCAP(MNUMNR,MNUMYR)
      REAL*4 RMLEV(MNUMNR,MNUMYR)

      REAL resprqt,comprqt,indprqt,trnprqt,totprqt,resp,comp,indp,trnp,totp,multp
      REAL multprs,multpcm,multpin,multptr
!
      CHARACTER*80 FILENM,DUMMY
      REAL*8 SF,GEN,GEN_SP(6)
      REAL*8 CL_GEN(MAX_CL),CL_CAP(MAX_CL),TGEN(ECP_D_CAP,MNUMNR),TCON(ECP_D_CAP,MNUMNR)
      INTEGER IY,JROW,IFILE,DTYP,ICENSUS,CRG,ICNFG,I_COAL,XYR,XFR, I_TEST, ICL
      INTEGER ICHK,IRG,FULLYR,I_CNTL,IFL,NSP,JRG,IRSEU,ICMEU,IINEU,IR
      INTEGER IDSM,JYR,KYR,BYR,IYR,IREG,IP,IECP,IRNW,IC
      INTEGER*4 CYR_INDEX, LYR_INDEX
      REAL  HRSFAC,HCMFAC,HINFAC,HTRFAC
      INTEGER MAXHRZN
      INTEGER I,F,J,KRG,IGRP,IFPP
!TIME      INTEGER CPU_TIME_BEGIN,CPU_TIME_END,IHR,IMN,ISEC,I100TH
      LOGICAL WHOOPS ! ERROR FLAG

      INTEGER CR,XY,DLTA,IV,ISP,ITMP,IBBN,IOWN
      INTEGER NEWUNIT,NFRST,IFRST,NLAST,ILAST,ITYP
      REAL VCSTTOT,FCSTTOT,SO2PTOT,NOXPTOT,RPSPTOT,HGPTOT
      CHARACTER*2 ASECTR
      INTEGER ICNS,ICLRG,IGSRG,L
      CHARACTER*1 NUMCH(10),COL
      REAL caralloc
      INTEGER IEFD,IPRT,FRG,ORG,GRG,I_NO_CCS,I_NO_CCS_EFD
      REAL*8 PRMEQ_ORIG(ECP_D_CAP),PRMDT_ORIG(ECP_D_CAP)
      REAL OLDITC
      INTEGER h1,i1,j1,k2,k3,l1,IRGN, ICR

      INTEGER RTOVALUE
      EXTERNAL RTOVALUE

      REAL*8 US_AVG_HTRT(ECP_D_CAP), AVG_HTRT_GEN(ECP_D_CAP)
      REAL*8 US_AVG_HTRT_G(ECP_D_CAP), AVG_HTRT_G_GEN(ECP_D_CAP)
      REAL*8 CAPCCR(MNUMNR,ECP_D_CAP)
      REAL*8 TFOSSGEN,TFOSSCON,TFOSSGENCR(MNUMCR),TFOSSCONCR(MNUMCR)

      REAL*8 AVG_HTRT(0:ECP_D_CAP), AVG_HTRT_MR(0:ECP_D_CAP), AVG_HTRT_MOD(0:ECP_D_CAP), AVG_HTRT_MR_MOD(0:ECP_D_CAP)
      REAL*8 ECP_GEN(0:ECP_D_CAP), ECP_GEN_MR(0:ECP_D_CAP), ECP_GEN_MOD(0:ECP_D_CAP), ECP_GEN_MR_MOD(0:ECP_D_CAP)

      CHARACTER*12   FROM_LABEL

      COMMON/BNCHGEN/ BSNCGEN,BSHYGEN,BSSOGEN,BSPVGEN,BSWNGEN,BSGTGEN,BMNCLYR,BMHYLYR,BMSOLYR,BMPVLYR,BMWNLYR,BMGTLYR
      REAL BSNCGEN,BSHYGEN,BSSOGEN,BSPVGEN,BSWNGEN,BSGTGEN
      INTEGER BMNCLYR,BMHYLYR,BMSOLYR,BMPVLYR,BMWNLYR,BMGTLYR

      COMMON /BCHMRKSTEOPRICE/ BCHMRKSTEOPRICEADDER, BCHMRKSTEOPRC_N   !STEO Price benchmark feature --- by AKN
      REAL BCHMRKSTEOPRICEADDER(MNUMCR,MNUMYR), BCHMRKSTEOPRC_N(MNUMNR,MNUMYR)  !STEO benchmark price adjustment adder --- by AKN

      DATA NEWUNIT/3/
      DATA NUMCH/'1','2','3','4','5','6','7','8','9','0'/

      COL=':'
      FNRUN = 1

      DPVDISPATCH = .TRUE.   !change it to .FALSE. if the treatment for distributed PV capacity & generation needs to be turned off
!     DPVDISPATCH = .FALSE.

      IF (CURIRUN .EQ. NUMIRUNS) FNRUN = 1
!     IF (CURIRUN .EQ. NUMIRUNS) PFNRUN = 1
!     IF (CURIRUN .EQ. NUMIRUNS) OFNRUN = 1

!     SET INTERNAL YEAR AND ITERATION VARIABLES USING GLOBAL VARIABLES
      JYR = 0
      DUMMY='TEST'
      I_CNTL = 80
      IF((CURITR .EQ. 1) .AND. (CURIYR .EQ. FIRSYR)) THEN
         ICHK = 0

         WRITE(6,8213) CURIRUN, CURIYR+1989, CURITR, ESIZ$BLD, EBLK$BLD
 8213    FORMAT(1X,"BILDIN_SIZE",5(":",I12))

      ELSE
         ICHK = 1
      END IF
!
!     FIRST TIME THROUGH READ/PROCESS INPUT DATA
!     AND OPEN OUTPUT DDNS FOR PRINTING
!
      IF(ICHK .EQ. 0) THEN

!     UGNMSNR = 0
!     CGNTGEN = 0

!
         OPEN(UNIT=22,FILE='EMMDBUG',BUFFERED='YES', BUFFERCOUNT=10)
         OPEN(UNIT=13,FILE='EMMREPT',BUFFERED='YES', BUFFERCOUNT=10)
         OPEN(UNIT=18,FILE='EMMPRNT',BUFFERED='YES', BUFFERCOUNT=10)

!        Initialize array which specifies if plants with CO2 capture technology must store captured CO2, i.e. Carbon Cap, Carbon Tax, etc.

         MUST_STORE = 0

!        Open the LDSM report file

         NEW = .FALSE.
!         fname = 'LDSMCRS'
!         IOCR = FILE_MGR('O',fname,NEW)  !Open COMMERCIAL RESTART FILE
!         fname = 'LDSMRRS'
!         IORR = FILE_MGR('O',fname,NEW)  !Open RESIDENTIAL RESTART FILE

         NEW = .TRUE.
         fname = 'LDSMRPT'
         IMSG = FILE_MGR('O',fname,NEW)  !Open LDSM REPORT FILE

         WRITE(IMSG,*)'LDSM REPORT FILE (units: GW and GWh)   :'
         write(imsg,*)'USW_POL = ',USW_POL



!        Read the LDSM Structure File

         WHOOPS = .False.

         CALL DSMRST(WHOOPS) ! Read structure file and DSM option database

         IF (WHOOPS) THEN
            WRITE(6,*)'<)) Message from LDSM:'
            WRITE(6,*)'<))) Processing interrupted because of an ERROR'
            WRITE(6,*)'<))) Data passed by LDSM may be CORRUPTED'
            WRITE(6,*)'<))) Control returned to UTIL'
            RETURN
         END IF

!
!        READ THE CONTROL FILE
!
         CALL RDCNTRL(I_CNTL)
!        WRITE(6,8493)'CLOSS  coeffs after RDCNTRL ',UCL_CF1,UCL_CF2,UCL_CF3,UCL_CF4
8493  FORMAT(A25,1x,4(F12.7,1x))
!
         IF(USW_XP .GT. 0) THEN                                  !CANADA
            UNRGNS = EFD_D_PROV - 3
         ENDIF
!        FOR PC AND CANADIAN VERSION READ IN CENSUS REGION ELECTRICITY DEMANDS
!
!        IF(USW_XP .GT. 0) THEN
!        ENDIF
!
!        OPEN THE PLANT DIRECT ACCESS FILE
!
         NEW = .FALSE.
         FILENM='PLNTTMP'
         UF_PLT=FILE_MGR('O',FILENM,NEW)
!
!        READ IN PLANT DATA
!
!        WRITE(6,5311) CURIYR+UHBSYR,CURITR
 5311    FORMAT(1X,"GETPCNTL",2(":",I4))
!
         CALL GETPCNTL
!
!        Initialize EMM_CL arrays
         DO ICL = NUM_CMM_UNITS + 1, MX_UNITS
          DO IY = 1, MNUMYR
           EMM_CL_ECPT(ICL,IY) = 0
           EMM_CL_BTUs(ICL,IY) = 0.0
           EMM_CL_CF(ICL,IY) = 0.0
          ENDDO
          EMM_CL_CLRG(ICL) = 0.0
         ENDDO

!        IF USW_DBS GT 0 OPEN @.EMMDBASE DATABASE FILE
!        AND CALL WEMMDB WRITE THE EMM DATABASE DEFINITION TABLES
!
         ORACLESW = RTOVALUE('ORACLESW',0)
         ORCLEFD  = RTOVALUE('ORCLEFD ',0)
         ORCLECP  = RTOVALUE('ORCLECP ',0)
         ORCLEFP  = RTOVALUE('ORCLEFP ',0)
         ORCLCL   = RTOVALUE('ORCLCL  ',0)


         IF(USW_XP .EQ.1) USW_DBS = 0          !off for canadian run
!        Turn off emmdbase.txt writes if all of emm database writes turned on
!        IF (ORACLESW .GT. 0) USW_DBS=0
         IF (ORCLEFP .GT. 0 .AND. ORCLEFD .GT. 0 .AND. ORCLECP .GT. 0) USW_DBS = 0
         IF (USW_DBS .GT. 0 .OR. ORACLESW .GT. 0) THEN
           FILENM = 'EMMDBASE'
           NEW = .TRUE.
           UF_DBS = FILE_MGR('O',FILENM,NEW)   !regular NEMS run
           CALL WEMMDB
         ENDIF
         IF ( ORACLESW .GT. 0 .AND. ORCLEFD .EQ. 1) THEN
           FILENM = 'EDBPGRP'
           NEW = .TRUE.
           UF_DBPGRP = FILE_MGR('O',FILENM,NEW)
           WRITE(UF_DBPGRP,*) '  ',TRIM(SCEN_DATE)
         ENDIF
         IF (((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1 )) .OR. (USW_DBS .GT. 1)) THEN
             CALL WRPLNT
         END IF


!        OPEN THE EFD INPUT DIRECT ACCESS FILE
!
         IF(USW_DISP .LE. 0) THEN
            NEW = .TRUE.
            FILENM='INPTDAF'
            UF_IN=FILE_MGR('O',FILENM,NEW)
         END IF
!
!        OPEN THE ECP INPUT DIRECT ACCESS FILE
!
         NEW = .TRUE.
         FILENM='ECPIDAF'
         UF_BLD=FILE_MGR('O',FILENM,NEW)
!        UF_BLD=97
!        OPEN(UF_BLD,FILE=FILENM,ACCESS='DIRECT',FORM='UNFORMATTED',
!        +   RECL=16384,STATUS='UNKNOWN')
!
!        OPEN THE EFD OUTPUT DIRECT ACCESS FILE
!
         NEW = .TRUE.
         FILENM='OUTDAF'
         UF_OUT=FILE_MGR('O',FILENM,NEW)
!
!        OPEN THE ECP OUTPUT DIRECT ACCESS FILE
!
         NEW = .TRUE.
         FILENM='ECPODAF'
         UF_BOUT=FILE_MGR('O',FILENM,NEW)
!        UF_BOUT=98
!        OPEN(UF_BOUT,FILE=FILENM,ACCESS='DIRECT',FORM='UNFORMATTED',
!        +   RECL=16384,STATUS='UNKNOWN')
!
!        OPEN THE SO2 DIRECT ACCESS FILE
!
         NEW = .TRUE.
         FILENM='SO2DAF'
         UF_SO2=FILE_MGR('O',FILENM,NEW)
!
!        OPEN THE INTER-REGIONAL TRADE FILES
!
         NEW = .FALSE.
         FILENM='ETTTMP'
         UF_ETDM=FILE_MGR('O',FILENM,NEW)
!        UF_ETDM=95
!        OPEN(UF_ETDM,FILE=FILENM,ACCESS='DIRECT',FORM='UNFORMATTED',
!        +   RECL=16384,STATUS='UNKNOWN')
!
         NEW = .TRUE.
         FILENM='ETTDBUG'
!        UF_ETT=96
!        OPEN(UF_ETT,FILE=FILENM,STATUS='OLD')
         UF_ETT=FILE_MGR('O',FILENM,NEW)
!
!
!         FILENM='CANOUT'
!         NEW = .FALSE.
!         UF_CN2=FILE_MGR('O',FILENM,NEW)

         FILENM='EFDOUT'
         NEW = .TRUE.
         UF_EFD=FILE_MGR('O',FILENM,NEW)

         FILENM='TRANSET'
         NEW = .TRUE.
         UF_TRAN=FILE_MGR('O',FILENM,NEW)

!        OPEN THE EFP DIRECT ACCESS FILES
!
         NEW = .TRUE.
         FILENM='EFPBLDS'
         IBD=FILE_MGR('O',FILENM,NEW)
         FILENM='EFPR'
         UF_R=FILE_MGR('O',FILENM,NEW)
         FILENM='EFPRC'
         UF_RC=FILE_MGR('O',FILENM,NEW)
         FILENM='EFPRCY'
         UF_RCY=FILE_MGR('O',FILENM,NEW)
         FILENM='EFPSTMT'
         UF_STMT=FILE_MGR('O',FILENM,NEW)
         FILENM='EFPSTM2'
         UF_STM2=FILE_MGR('O',FILENM,NEW)

!
!        READ IN AND STORE NUGS HISTORY AND PIPELINE DATA
!
         CALL RDNUGS
!
!        READ IN BENCHMARK DATA AND NUCLEAR CAPACITY FACTORS
!
         CALL RDYRDAT
!
!        READ IN INTERNATIONAL LEARNING DATA
!
         CALL RDINTLRN
!
!        WHAT ARE ELECTRICITY DEMANDS HERE ?
!
!        DO ICENSUS = 1 , 11
!           ASECTR = "AS"
!           WRITE(6,7311) CURIYR+UHBSYR,ICENSUS,ASECTR,(QELAS(ICENSUS,KYR),KYR = CURIYR , MIN(UNYEAR , CURIYR + 20))
!           ASECTR = "RS"
!           WRITE(6,7311) CURIYR+UHBSYR,ICENSUS,ASECTR,(QELRS(ICENSUS,KYR),KYR = CURIYR , MIN(UNYEAR , CURIYR + 20))
!           ASECTR = "CM"
!           WRITE(6,7311) CURIYR+UHBSYR,ICENSUS,ASECTR,(QELCM(ICENSUS,KYR),KYR = CURIYR , MIN(UNYEAR , CURIYR + 20))
!           ASECTR = "IN"
!           WRITE(6,7311) CURIYR+UHBSYR,ICENSUS,ASECTR,(QELIN(ICENSUS,KYR),KYR = CURIYR , MIN(UNYEAR , CURIYR + 20))
!           ASECTR = "TR"
!           WRITE(6,7311) CURIYR+UHBSYR,ICENSUS,ASECTR,(QELTR(ICENSUS,KYR),KYR = CURIYR , MIN(UNYEAR , CURIYR + 20))
!7311       FORMAT(1X,"QEL",":",I4,":",I2,":",A2,20(":",F7.1))
!        END DO
!
!UADD ADD THIS SECTION FOR UADD FIX
!        INITIALIZE RENEW UNPLANNED BUILD ARRAYS TO ZERO
!
         DO IYR = 1,(UNYEAR + ECP_D_XPH)
            DO IRG = 1,MNUMNR
               UADDHYT(IRG,IYR) = 0.0
               UADDGET(IRG,IYR) = 0.0
               UADDMST(IRG,IYR) = 0.0
               UADDWDT(IRG,IYR) = 0.0
               UADDSTT(IRG,IYR) = 0.0
               UADDPVT(IRG,IYR) = 0.0
               UADDPTT(IRG,IYR) = 0.0
               UADDWNT(IRG,IYR) = 0.0
               UADDWLT(IRG,IYR) = 0.0
               UADDWFT(IRG,IYR) = 0.0
               UADDPST(IRG,IYR) = 0.0
               UADDHYD(IRG,IYR) = 0.0
               UADDGED(IRG,IYR) = 0.0
               UADDMSD(IRG,IYR) = 0.0
               UADDWDD(IRG,IYR) = 0.0
               UADDSTD(IRG,IYR) = 0.0
               UADDPVD(IRG,IYR) = 0.0
               UADDPTD(IRG,IYR) = 0.0
               UADDWND(IRG,IYR) = 0.0
               UADDWLD(IRG,IYR) = 0.0
               UADDWFD(IRG,IYR) = 0.0
               UADDPSD(IRG,IYR) = 0.0
            END DO
         END DO
!UADD END OF UADD SECTION
      END IF ! ICHK
!
!     DETERMINE CURRENT YEAR
!
      FULLYR = USYEAR(CURIYR)
      JYR = CURIYR

      IF (FULLYR .LT. UESTYR) RETURN  ! don't run before EMM start year
!
!     CALL ROUTINE FOR EPA TRANSPORT RULE, IF APPROPRIATE
!
      IF (TRANRULE1 .LT. 9999 .AND. CURITR .EQ. 1) CALL EPA$TRANRULE
!
!     CALL ROUTINE FOR EPA TOXICS RULE, IF APPROPRIATE
!
      IF ((CURIYR + UHBSYR) .EQ. UDSI_YR .AND. CURITR .EQ. 1) THEN
         CALL EPA$DSIRMV
      END IF
!
!   High Demand case - adjust QEL__ variables on first iteration
      IF (USW_CEL .eq. 1 .AND. CURITR .eq. 1 .AND. CURIRUN .eq. 1) CALL HIELADJ

      CALL ELDMND(JYR)
!
      IF((CURITR .EQ. 1) .AND. (FULLYR .EQ. UESTYR)) THEN
!
!        CAPTURE FUEL DATA
!
         CALL GETFUEL(FULLYR)

      END IF
!
!        GET TRANSMISSION AND TRANSFER DATA
!
         CALL GETEIJ(CURIYR)
!
!     IN CECA RUNS, CALL ROUTINE TO DETERMINE ADJUSTMENT FACTORS TO
!     LOAD CURVES TO ACCOUNT FOR DSM AND DISTRIBUTED GENERATION
!
         CALL ADJDEMD(1)
!
!     CALL SMART GRID ADJUSTMENT ROUTINE
!
      IF ( (( USW_DMLSADJ .GE. 1 ) .OR. ( USW_SGRID .GE. 1 )) .AND. CURITR .EQ. 1 ) THEN
         CALL SGRIDADJ
      ENDIF
!
!        CALL THE LDSM MODULE (IDSM=1 INDICATES SET UP LOAD CURVES)
!
         IDSM = 1
         CALL ELLDSM(IDSM)

      IF (FCRL .EQ. 1) THEN
      DO IRGN = 1, UNRGNS
      h1=0
      DO i1=1,12
        DO j1=1,3
          l1=h1+1
          h1=h1+24
          WRITE(IMSG,923) CURIYR,IRGN,I1,J1,(SYSLDHR(CURIYR,IRGN,k2),k2=l1,h1)
923   FORMAT(1x,'SYSLDHR',4I4,24F8.2)
          WRITE(IMSG,933) CURIYR,IRGN,I1,J1,(HRTOECPSL(CURIYR,IRGN,I1,J1,k3),k3=1,24)
933   FORMAT(1x,'HRTOECPSL',4I4,24I5)
        ENDDO
      ENDDO
      ENDDO
      ENDIF
!
!     IN CECA RUNS, NOW ADJUST QEL VARIABLES USING ADJUSTMENT FACTORS
!     FOR DSM AND DISTRIBUTED GENERATION
!
         CALL ADJDEMD(2)
!
!     initialize capacity map to new combined fuel regions
!
      IF (CURITR .EQ. 1) THEN
         IF (FULLYR .EQ. UESTYR) EPNFLRG = 0
         CPFLRG = 0.0
         CPFLECP = 0.0
         CPFLEFD = 0.0

!        WRITE(6,3113) CURIRUN, CURIYR+1989, CURITR
!3113    FORMAT("UTIL_CPFKECP",3(":",I4))

!        DO ICNS = 1, MNUMCR
!           DO ICLRG = 1, EFD_D_MFRG
!              DO IGSRG = 1, EFD_D_MFRG
!                 CPFLRG(ICNS,ICLRG,IGSRG) = 0.0
!                 IF (FULLYR .EQ. UESTYR) EPNFLRG(ICNS,ICLRG,IGSRG) = 0
!                 DO IECP = 1, ECP_D_CAP
!                    CPFLECP(IECP,ICNS,ICLRG,IGSRG) = 0.0
!                 ENDDO
!                 DO IEFD = 1, EFD_D_CAP
!                    CPFLEFD(IEFD,ICNS,ICLRG,IGSRG) = 0.0
!                 ENDDO
!              ENDDO
!           ENDDO
!        ENDDO
      ENDIF
!

	

 !     do IRG = 1,MNUMNR
 !        write (6,*) "checkUPBLDREG", IRG, (UPBLDREG(IGRP,IRG),IGRP = 1,ECP_D_CAP)
 !     end do
      
!     LOOP OVER THE UTILITY REGIONS
!
      DO 20 IRG = 1 , UNRGNS

         CALL GETIN(1,IRG)

!        GET EFD LOAD DATA
!
         CALL ELDEFD(CURIYR,CURITR,IRG)
!

!        WRITE(6,3979) CURIRUN, CURIYR+1989, CURITR, IRG, EEITAJ(1), EEITAJ(2), EEITAJ(3)
!3979    FORMAT(1X,"UTIL_00428_EEITAJ_GET",4(":",I4),3(":",F12.3))

         IF((CURITR .EQ. 1) .OR. (I4SITE .EQ. 3)) THEN
!
!           Reset Group Number Assignments for New Groups
!
            IF (US_GRPS(CURIYR) .EQ. 1) THEN
               DO IGRP = 1 , EFD_D_CAP
                  DO IOWN = 1 , USW_OWN
                     DO IBBN = 1 , EFD_D_MFRG
                        WNEWGRP(IRG,IGRP,IOWN,IBBN) = 0
                     END DO
                  END DO
               END DO
            END IF
!
            IF (USW_XP .EQ. 0) THEN                       !OFF FOR CANADA
               IF (FULLYR .EQ. UESTYR) THEN
!
!                 CALL ROUTINE TO READ CAPACITY EXPANSION INPUT DATA
!
                  CALL RDBD_UECP(IRG)

!
               ELSE
!
!                 GET ECP INPUT BLOCK , THIS RESTORES PREVIOUSLY ASSIGNED VALUES

!                 WRITE(6,6330) CURIRUN, CURIYR+1989, CURITR

                  CALL GETBLD(1,IRG)

!                 WRITE(6,6330) CURIRUN, CURIYR+1989, CURITR
!6330             FORMAT(1X,"GETBLD_CALL",3(":",I4))
!
               END IF
            END IF
!
!           LOAD TRANSMISSION AND TRANSFER DATA FOR REGION IRG
!
            JRG = URGNUM(IRG)
            EQTDLS = UQTDLS(JRG)                           ! T&D LOSS
            DO NSP = 1 , EENSP
               TRNCSTEX(NSP) = URNCSTEX_EFD(NSP,JRG)   ! EXPORT CAPABILITY
               TRNCSTIM(NSP) = URNCSTIM_EFD(NSP,JRG)   ! IMPORT CAPABILITY
            END DO
!
!           GET ECP LOAD DATA
!
            CALL ELDECP(IRG)
!
!           GET PLANT DATA FOR CURRENT YEAR

!           I_TEST = 1
!           DO IECP = 1 , ECP_D_DSP
!              IF ((UPVTYP(IECP) .EQ. 1) .OR. (UPTOPR(IECP) .EQ. 3)) THEN
!                 WRITE(18,9331) CURIRUN, CURIYR+1989, CURITR, I_TEST, IRG, IECP, UPVTYP(IECP), UPTOPR(IECP), UPLNTCD(IECP), UPHTRT(IECP), EPPHRT0(IECP), EPPHRTN(IECP)
!9331             FORMAT(1X,"HTRT_UTIL",8(":",I6),":",A2,3(":",F12.0))
!              END IF
!           END DO

!           DO IECP = ECP_D_DSP + 1 , ECP_D_CAP
!              IF ((UPVTYP(IECP) .EQ. 1) .OR. (UPTOPR(IECP) .EQ. 3)) THEN
!                 WRITE(18,9331) CURIRUN, CURIYR+1989, CURITR, I_TEST, IRG, IECP, UPVTYP(IECP), UPTOPR(IECP), UPLNTCD(IECP), UPHTRT(IECP)
!              END IF
!           END DO

            CALL CRPGRP(FULLYR,IRG,MAXHRZN)

!           I_TEST = I_TEST + 1
!           DO IECP = 1 , ECP_D_DSP
!              IF ((UPVTYP(IECP) .EQ. 1) .OR. (UPTOPR(IECP) .EQ. 3)) THEN
!                 WRITE(18,9331) CURIRUN, CURIYR+1989, CURITR, I_TEST, IRG, IECP, UPVTYP(IECP), UPTOPR(IECP), UPLNTCD(IECP), UPHTRT(IECP), EPPHRT0(IECP), EPPHRTN(IECP)
!              END IF
!           END DO

!           DO IECP = ECP_D_DSP + 1 , ECP_D_CAP
!              IF ((UPVTYP(IECP) .EQ. 1) .OR. (UPTOPR(IECP) .EQ. 3)) THEN
!                 WRITE(18,9331) CURIRUN, CURIYR+1989, CURITR, I_TEST, IRG, IECP, UPVTYP(IECP), UPTOPR(IECP), UPLNTCD(IECP), UPHTRT(IECP)
!              END IF
!           END DO
!
!           IF INTEGRATED WITH RENEWABLES CALL RENOVR TO OVERWRITE ECP INPUT
!           VALUES WITH RENEW COMMON VALUES, CALL CFCOVR TO OVERWRITE
!           INTERMITTENT MAX CAP FACTORS AND CALL RENEFD TO OVERWRITE EFD
!           INPUT VALUES.
!
            IF (FULLYR .GE. UESTYR)  THEN
               IF(USW_XP .EQ. 0) THEN                       !OFF FOR CANADA
                  IF(USW_RNW .GE. 1) THEN
                     CALL RENOVR(IRG)
                     CALL CFCOVR(IRG)

!                    I_TEST = I_TEST + 1
!                    DO IECP = 1 , ECP_D_DSP
!                       IF ((UPVTYP(IECP) .EQ. 1) .OR. (UPTOPR(IECP) .EQ. 3)) THEN
!                          WRITE(18,9331) CURIRUN, CURIYR+1989, CURITR, I_TEST, IRG, IECP, UPVTYP(IECP), UPTOPR(IECP), UPLNTCD(IECP), UPHTRT(IECP), EPPHRT0(IECP), EPPHRTN(IECP)
!                       END IF
!                    END DO

!                    DO IECP = ECP_D_DSP + 1 , ECP_D_CAP
!                       IF ((UPVTYP(IECP) .EQ. 1) .OR. (UPTOPR(IECP) .EQ. 3)) THEN
!                          WRITE(18,9331) CURIRUN, CURIYR+1989, CURITR, I_TEST, IRG, IECP, UPVTYP(IECP), UPTOPR(IECP), UPLNTCD(IECP), UPHTRT(IECP)
!                       END IF
!                    END DO

                     CALL RENEFD(IRG)

!                    I_TEST = I_TEST + 1
!                    DO IECP = 1 , ECP_D_DSP
!                       IF ((UPVTYP(IECP) .EQ. 1) .OR. (UPTOPR(IECP) .EQ. 3)) THEN
!                          WRITE(18,9331) CURIRUN, CURIYR+1989, CURITR, I_TEST, IRG, IECP, UPVTYP(IECP), UPTOPR(IECP), UPLNTCD(IECP), UPHTRT(IECP), EPPHRT0(IECP), EPPHRTN(IECP)
!                       END IF
!                    END DO

!                    DO IECP = ECP_D_DSP + 1 , ECP_D_CAP
!                       IF ((UPVTYP(IECP) .EQ. 1) .OR. (UPTOPR(IECP) .EQ. 3)) THEN
!                          WRITE(18,9331) CURIRUN, CURIYR+1989, CURITR, I_TEST, IRG, IECP, UPVTYP(IECP), UPTOPR(IECP), UPLNTCD(IECP), UPHTRT(IECP)
!                       END IF
!                    END DO

                  ENDIF
               ENDIF
            ENDIF
!
!           STORE CAPACITIES TO DISPOUT COMMON
            CALL STROUT(CURIYR,IRG)
!
!           STORE ECP INPUT BLOCK

!           WRITE(6,6331) CURIRUN, CURIYR+1989, CURITR

            CALL STRBLD(1,IRG)

!           WRITE(6,6331) CURIRUN, CURIYR+1989, CURITR
!6331       FORMAT(1X,"STRBLD_CALL",3(":",I4))

!
         END IF
!
!        LOAD TRANSMISSION AND TRANSFER DATA FOR REGION IRG
!
         JRG = URGNUM(IRG)

         IF (FULLYR .EQ. UESTYR) UTNEWTRN = 0.0
         IF (FULLYR .EQ. UESTYR) UTRELADJ = 0.0

         DO 15 NSP = 1 , EENSP

            EEITAJ(NSP) = UEITAJ_EFD(NSP,JRG)                  ! NET EXPORTS

!           WRITE(6,7315) CURIRUN, CURIYR+1989, CURITR, UESTYR, IRG, JRG, NSP, EEITAJ(NSP), UEITAJ_EFD(NSP,JRG)
!7315       FORMAT(1X,"EEITAJ_UEITAJ_EFD",7(":",I4),2(":",F15.3))

            TRNCSTEX(NSP) = URNCSTEX_EFD(NSP,JRG)        ! EXPORT CAPABILITY
            TRNCSTIM(NSP) = URNCSTIM_EFD(NSP,JRG)        ! IMPORT CAPABILITY
   15    CONTINUE
!
!        LOAD IMPORT DATA FROM RESTART FILE FOR CARBON CONSTRAINTS
!
         IF ((CURIYR + UHBSYR) .EQ. UESTYR .AND. CURITR .EQ. 1)THEN
            DO IYR = CURIYR , MNUMYR
               KWH_IM_BY_RG(JRG,IYR) = (UTIMPF(JRG,IYR) + UTIMPE(JRG,IYR)) * 0.001
               KWH_DM_BY_RG(JRG,IYR) = (UTDMMF(JRG,IYR) + UTEXMF(JRG,IYR)) * 0.001
!     if (jrg .eq. 20)write(6,3456) curiyr+1989,iyr+1989,kwh_im_by_rg(jrg,iyr)
!3456 format(1h ,'!caimp',i4,i5,f10.1)
            END DO
         END IF
         EQTDLS = UQTDLS(JRG)                                 ! T&D LOSS
!
!        CALL NUGS SUBMODULE
!
         CALL ELNUGS(IRG,CURIYR)
!
!        GET EFD LOAD DATA
!
!        CALL ELDEFD(CURIYR,CURITR,IRG)  ! moved this up to fill EETIME earlier, needed in other subroutines
!
!        OUTPUT EFD LOAD SHAPE CURVES (LAST ITERATION OF YEAR)
!
         IF (FCRL .EQ. 1 .AND. ULCVSW .EQ. 1) THEN
            DO ISP = 1 , EENSP
               WRITE(18,2225)' EFDLD:C,I,R,S,N,P,T=',CURIYR,CURITR, &
                IRG,ISP,ELNVCT(ISP),ELPEAK(ISP),EETIME(ISP)
               DO IV = 1, ELNVCT(ISP)
                  WRITE(18,2226) ' EFDLD:C,I,R,S,V,H,W=', &
              CURIYR,CURITR,IRG,ISP,IV,ELHGHT(IV,ISP),ELWDTH(IV,ISP)
               END DO
            END DO
         ENDIF
 2225    FORMAT(A21,5(1X,I2),2(1X,F10.5))
 2226    FORMAT(A21,5(1X,I2),2(1X,F10.5))
!
!        STORE INPUT BLOCK
!
         CALL STRIN(1,IRG)

!        WRITE(6,2979) CURIRUN, CURIYR+1989, CURITR, IRG, EEITAJ(1), EEITAJ(2), EEITAJ(3)
!2979    FORMAT(1X,"UTIL_00544_EEITAJ_STR",4(":",I4),3(":",F12.3))

!
   20 CONTINUE

!STEOBM calculate benchmark factors
      IF (CURITR .EQ. 1) THEN
!        add AK/HI to get total US generation for comparison to STEO
          BSNCGEN  = BSNCGEN + UGNURNR(1,MNUMNR-2,CURIYR) + UGNURNR(2,MNUMNR-2,CURIYR) &    ! nuclear
                             + UGNURNR(1,MNUMNR-1,CURIYR) + UGNURNR(2,MNUMNR-1,CURIYR)
         IF (BMNCGEN(CURIYR) .GT. 0.0) THEN
          URNCCFA(CURIYR) = BMNCGEN(CURIYR) / BSNCGEN
         ENDIF
          BSHYGEN  = BSHYGEN + UGNHYNR(1,MNUMNR-2,CURIYR) + UGNHYNR(2,MNUMNR-2,CURIYR) &    ! hydro
                             + UGNHYNR(1,MNUMNR-1,CURIYR) + UGNHYNR(2,MNUMNR-1,CURIYR)
          EXSGEN(WIHY,MNUMNR) = EXSGEN(WIHY,MNUMNR) + UGNHYNR(1,MNUMNR-2,CURIYR) + UGNHYNR(2,MNUMNR-2,CURIYR) &
                                + UGNHYNR(1,MNUMNR-1,CURIYR) + UGNHYNR(2,MNUMNR-1,CURIYR)
         IF (BMHYGEN(CURIYR) .GT. 0.0) THEN
          URHYCFA(CURIYR) = BMHYGEN(CURIYR) / BSHYGEN
         ENDIF
          BSGTGEN  = BSGTGEN + UGNGENR(1,MNUMNR-2,CURIYR) + UGNGENR(2,MNUMNR-2,CURIYR) &    ! geoth
                             + UGNGENR(1,MNUMNR-1,CURIYR) + UGNGENR(2,MNUMNR-1,CURIYR)
          EXSGEN(WIGT,MNUMNR) = EXSGEN(WIGT,MNUMNR) + UGNGENR(1,MNUMNR-2,CURIYR) + UGNGENR(2,MNUMNR-2,CURIYR) &
                                + UGNGENR(1,MNUMNR-1,CURIYR) + UGNGENR(2,MNUMNR-1,CURIYR)
         IF (BMGTGEN(CURIYR) .GT. 0.0) THEN
          URGTCFA(CURIYR) = BMGTGEN(CURIYR) / BSGTGEN
         ENDIF
          BSSOGEN  = BSSOGEN + UGNSONR(1,MNUMNR-2,CURIYR) + UGNSONR(2,MNUMNR-2,CURIYR) &    ! solar th
                             + UGNSONR(1,MNUMNR-1,CURIYR) + UGNSONR(2,MNUMNR-1,CURIYR)
          EXSGEN(WISO,MNUMNR) = EXSGEN(WISO,MNUMNR) + UGNSONR(1,MNUMNR-2,CURIYR) + UGNSONR(2,MNUMNR-2,CURIYR) &
                                + UGNSONR(1,MNUMNR-1,CURIYR) + UGNSONR(2,MNUMNR-1,CURIYR)
!        IF (BMSOGEN(CURIYR) .GT. 0.0) THEN
!         URSOCFA(CURIYR) = BMSOGEN(CURIYR) / BSSOGEN          !calculate based on total solar
!        ENDIF
          BSSOGEN  = BSSOGEN + UGNPVNR(1,MNUMNR-2,CURIYR) + UGNPVNR(2,MNUMNR-2,CURIYR) &    ! solar pv
                             + UGNPVNR(1,MNUMNR-1,CURIYR) + UGNPVNR(2,MNUMNR-1,CURIYR) &
                             + UGNPTNR(1,MNUMNR-2,CURIYR) + UGNPTNR(2,MNUMNR-2,CURIYR) &    ! solar pv - fixed axis
                             + UGNPTNR(1,MNUMNR-1,CURIYR) + UGNPTNR(2,MNUMNR-1,CURIYR)
          EXSGEN(WIPV,MNUMNR) = EXSGEN(WIPV,MNUMNR) + UGNPVNR(1,MNUMNR-2,CURIYR) + UGNPVNR(2,MNUMNR-2,CURIYR) &
                                + UGNPVNR(1,MNUMNR-1,CURIYR) + UGNPVNR(2,MNUMNR-1,CURIYR)
          EXSGEN(WIPT,MNUMNR) = EXSGEN(WIPT,MNUMNR) + UGNPTNR(1,MNUMNR-2,CURIYR) + UGNPTNR(2,MNUMNR-2,CURIYR) &
                                + UGNPTNR(1,MNUMNR-1,CURIYR) + UGNPTNR(2,MNUMNR-1,CURIYR)
         IF (BMSOGEN(CURIYR) .GT. 0.0) THEN
          URSOCFA(CURIYR) = BMSOGEN(CURIYR) / BSSOGEN
         ENDIF
          BSWNGEN  = BSWNGEN + UGNWNNR(1,MNUMNR-2,CURIYR) + UGNWNNR(2,MNUMNR-2,CURIYR) &    ! wind
                             + UGNWNNR(1,MNUMNR-1,CURIYR) + UGNWNNR(2,MNUMNR-1,CURIYR) &
                             + UGNWLNR(1,MNUMNR-2,CURIYR) + UGNWLNR(2,MNUMNR-2,CURIYR) &    ! wind low speed
                             + UGNWLNR(1,MNUMNR-1,CURIYR) + UGNWLNR(2,MNUMNR-1,CURIYR)
          EXSGEN(WIWN,MNUMNR) = EXSGEN(WIWN,MNUMNR) + UGNWNNR(1,MNUMNR-2,CURIYR) + UGNWNNR(2,MNUMNR-2,CURIYR) &
                                + UGNWNNR(1,MNUMNR-1,CURIYR) + UGNWNNR(2,MNUMNR-1,CURIYR)
          EXSGEN(WIWL,MNUMNR) = EXSGEN(WIWL,MNUMNR) + UGNWLNR(1,MNUMNR-2,CURIYR) + UGNWLNR(2,MNUMNR-2,CURIYR) &
                                + UGNWLNR(1,MNUMNR-1,CURIYR) + UGNWLNR(2,MNUMNR-1,CURIYR)
         IF (BMWNGEN(CURIYR) .GT. 0.0) THEN
          URWNCFA(CURIYR) = BMWNGEN(CURIYR) / BSWNGEN
         ENDIF
!   implement phase out if used
        IF (BMPHSNC .GT. 0) THEN
         IF (CURIYR .GT. BMNCLYR .AND. CURIYR .LE. BMNCLYR + BMPHSNC) &
             URNCCFA(CURIYR) = URNCCFA(CURIYR-1) + (1.0 - URNCCFA(BMNCLYR)) / BMPHSNC
        ENDIF
        IF (BMPHSHY .GT. 0) THEN
         IF (CURIYR .GT. BMHYLYR .AND. CURIYR .LE. BMHYLYR + BMPHSHY) &
             URHYCFA(CURIYR) = URHYCFA(CURIYR-1) + (1.0 - URHYCFA(BMHYLYR)) / BMPHSHY
        ENDIF
        IF (BMPHSGT .GT. 0) THEN
         IF (CURIYR .GT. BMGTLYR .AND. CURIYR .LE. BMGTLYR + BMPHSGT) &
             URGTCFA(CURIYR) = URGTCFA(CURIYR-1) + (1.0 - URGTCFA(BMGTLYR)) / BMPHSGT
        ENDIF
        IF (BMPHSSO .GT. 0) THEN
         IF (CURIYR .GT. BMSOLYR .AND. CURIYR .LE. BMSOLYR + BMPHSSO) &
             URSOCFA(CURIYR) = URSOCFA(CURIYR-1) + (1.0 - URSOCFA(BMSOLYR)) / BMPHSSO
        ENDIF
!        IF (CURIYR .GT. BMPVLYR .AND. CURIYR .LE. BMPVLYR + BMPHS) &
!            URPVCFA(CURIYR) = URPVCFA(CURIYR-1) + (1.0 - URPVCFA(BMPVLYR)) / BMPHS
        IF (BMPHSWN .GT. 0) THEN 
        IF (CURIYR .GT. BMWNLYR .AND. CURIYR .LE. BMWNLYR + BMPHSWN) &
             URWNCFA(CURIYR) = URWNCFA(CURIYR-1) + (1.0 - URWNCFA(BMWNLYR)) / BMPHSWN
        ENDIF
        
!       apply STEO benchmarking factors to existing generation

        DO JRG = 1 , UNRGNS
          EXSGEN(WIHY,JRG) = EXSGEN(WIHY,JRG) * URHYCFA(CURIYR)
          EXSGEN(WIGT,JRG) = EXSGEN(WIGT,JRG) * URGTCFA(CURIYR)
          EXSGEN(WISO,JRG) = EXSGEN(WISO,JRG) * URSOCFA(CURIYR)
          EXSGEN(WIPV,JRG) = EXSGEN(WIPV,JRG) * URSOCFA(CURIYR)
          EXSGEN(WIPT,JRG) = EXSGEN(WIPT,JRG) * URSOCFA(CURIYR)
          EXSGEN(WIWN,JRG) = EXSGEN(WIWN,JRG) * URWNCFA(CURIYR)
          EXSGEN(WIWL,JRG) = EXSGEN(WIWL,JRG) * URWNCFA(CURIYR)
        END DO

          IF (FULLYR .GT. UESTYR) write(22,2230) CURIYR,CURITR,BSNCGEN,BSHYGEN,BSGTGEN,BSSOGEN,BSWNGEN
          IF (FULLYR .GT. UESTYR) write(22,2231) CURIYR,CURITR,URNCCFA(CURIYR),URHYCFA(CURIYR),URGTCFA(CURIYR),&
                                  URSOCFA(CURIYR),URWNCFA(CURIYR)
2230  FORMAT(1X,"STEO baseline gen ",2(":",I4),5(":",F12.4))
2231  FORMAT(1X,"STEO CF adj factor",2(":",I4),5(":",F12.4))
      ENDIF

!    CHECK IF CAPACITY exists with CCS then the non-CCS technology also has capacity (for EFD BTU row/col to be generated correctly)
        DO FRG = 1, UNFRGN
           CRG = EPCLMP(FRG)
           GRG = EPGSMP(FRG)
           ORG = EPCSMP(FRG)
           DO IECP = 1, ECP_D_CAP
		IEFD = UPEFDT(IECP)
		I_NO_CCS = NO_CCS_PLNT_NDX(IECP)
		I_NO_CCS_EFD = UPEFDT(I_NO_CCS)
		IF (CPFLECP(IECP,ORG,CRG,GRG) .GT. 0 .AND. CPFLECP(I_NO_CCS,ORG,CRG,GRG) .EQ. 0) THEN 
			CPFLECP(I_NO_CCS,ORG,CRG,GRG) = 1.0
                   write(18,2240) 'Incrementing CPFLECP ',CURIYR,CURITR,FRG,IECP,I_NO_CCS,CPFLECP(IECP,ORG,CRG,GRG),CPFLECP(I_NO_CCS,ORG,CRG,GRG)
		ENDIF
		IF (CPFLEFD(IEFD,ORG,CRG,GRG) .GT. 0 .AND. CPFLEFD(I_NO_CCS_EFD,ORG,CRG,GRG) .EQ. 0) THEN
			CPFLEFD(I_NO_CCS_EFD,ORG,CRG,GRG) = 1.0
                   write(18,2240) 'Incrementing CPFLEFD ',CURIYR,CURITR,FRG,IEFD,I_NO_CCS_EFD,CPFLEFD(IEFD,ORG,CRG,GRG),CPFLEFD(I_NO_CCS_EFD,ORG,CRG,GRG)
		ENDIF
           ENDDO
2240    FORMAT(1x,A30,5I6,2F10.2)
        ENDDO

!     PRINT MUST_STORE ARRAY CURIYR to UNYEAR AND FILL OUT ARRAY FROM UNYEAR TO END OF ARRAY WITH UNYEAR VALUES

!     Change array which specifies if plants with CO2 capture technology must store captured CO2 given a Carbon Cap or Carbon Tax

      DO XYR = 1 , UNYEAR
         IF (EMETAX(1,XYR) .GT. 0.0 .AND. NOT(TRAN_FLAG)) THEN
            DO XFR = 1 , UNFRGN
               MUST_STORE(XFR,XYR) = 1
            END DO
         END IF
      END DO

!     IF((CURITR .EQ. 1) .AND. (FULLYR .EQ. UESTYR)) THEN
!        DO XYR = CURIYR , UNYEAR
!           WRITE(6,7391) CURIRUN, CURIYR+1989, XYR+1989, NOT(TRAN_Flag), EMETAX(1,XYR), (MUST_STORE(XFR,XYR),XFR=1,UNFRGN)
 7391       FORMAT(1X,"MUST_STORE",3(":",I4),":",L1,":",F8.3,<UNFRGN>(":",I1))
!        END DO
!     END IF

!     EXTEND FINAL YEAR RESULTS TO ALL POSSIBLE EXPECTATION YEARS

      DO XYR = UNYEAR + 1 , MNUMYR + ECP_D_FPH
         DO XFR = 1 , UNFRGN
            MUST_STORE(XFR,XYR) = MUST_STORE(XFR,UNYEAR)
         END DO
      END DO

      IF((CURITR .EQ. 1) .AND. (FULLYR .EQ. UESTYR)) THEN
        IF ( (ORACLESW .GE. 1) .AND. (FNRUN .EQ. 1) ) THEN
          IF (ORCLEFD .NE. 0 .OR. ORCLCL .NE. 0 .OR. ORCLEFP .NE. 0 .OR. ORCLECP .NE. 0) THEN
            WRITE (*,3010)
            CALL DEFNTABS
            CALL EMMDEFS
            CALL LOADDEFS
          END IF
        END IF
      END IF
 3010  FORMAT(/,' *********** WRITE DEF TABLES TO ORACLE ')
!
!     assign one character region codes

      IF((CURITR .EQ. 1) .AND. (FULLYR .EQ. UESTYR)) THEN
         DO IRG = 1, MNUMNR + EFD_D_PROV
            IF (IRG .LE. 10) THEN
              URGNCD(IRG) = NUMCH(IRG)
            ELSE
              URGNCD(IRG) = CHAR(54+IRG)
            ENDIF
         ENDDO
      ENDIF
!
      IF (USW_XP .EQ. 0) THEN                             !OFF FOR CANADA

!        CALL ROUTINES TO DETERMINE SHORT-TERM ELASTICITIES

         IF (CURITR .EQ. 1)THEN
            CALL ELSTCAP
            CALL ELSTELA

!           CALL ROUTINE IN NEW TECHNOLOGIES SUBMODULE TO GET
!           TECHNOLOGICAL OPTIMISM AND LEARNING FACTORS FOR CAPITAL COSTS

            CALL ELOPTLC

!           I_TEST = I_TEST + 1
!           DO IRG = 1 , UNRGNS
!              CALL GETBLD(1,IRG)

!              DO IECP = 1 , ECP_D_DSP
!                 IF ((UPVTYP(IECP) .EQ. 1) .OR. (UPTOPR(IECP) .EQ. 3)) THEN
!                    WRITE(18,9331) CURIRUN, CURIYR+1989, CURITR, I_TEST, IRG, IECP, UPVTYP(IECP), UPTOPR(IECP), UPLNTCD(IECP), UPHTRT(IECP), EPPHRT0(IECP), EPPHRTN(IECP)
!                 END IF
!              END DO

!              DO IECP = ECP_D_DSP + 1 , ECP_D_CAP
!                 IF ((UPVTYP(IECP) .EQ. 1) .OR. (UPTOPR(IECP) .EQ. 3)) THEN
!                    WRITE(18,9331) CURIRUN, CURIYR+1989, CURITR, I_TEST, IRG, IECP, UPVTYP(IECP), UPTOPR(IECP), UPLNTCD(IECP), UPHTRT(IECP)
!                 END IF
!              END DO
!           END DO

!           CALL ROUTINE TO DETERMINE IMPROVEMENTS IN HEATRATES

            CALL ELHRTLC

!           I_TEST = I_TEST + 1
!           DO IRG = 1 , UNRGNS
!              CALL GETBLD(1,IRG)

!              DO IECP = 1 , ECP_D_DSP
!                 IF ((UPVTYP(IECP) .EQ. 1) .OR. (UPTOPR(IECP) .EQ. 3)) THEN
!                    WRITE(18,9331) CURIRUN, CURIYR+1989, CURITR, I_TEST, IRG, IECP, UPVTYP(IECP), UPTOPR(IECP), UPLNTCD(IECP), UPHTRT(IECP), EPPHRT0(IECP), EPPHRTN(IECP)
!                 END IF
!              END DO

!              DO IECP = ECP_D_DSP + 1 , ECP_D_CAP
!                 IF ((UPVTYP(IECP) .EQ. 1) .OR. (UPTOPR(IECP) .EQ. 3)) THEN
!                    WRITE(18,9331) CURIRUN, CURIYR+1989, CURITR, I_TEST, IRG, IECP, UPVTYP(IECP), UPTOPR(IECP), UPLNTCD(IECP), UPHTRT(IECP)
!                 END IF
!              END DO
!           END DO

!           CALL ROUTINE TO USE ALTERNATIVE CHARACTERISTICS, IF APPROPRIATE

            CALL ELOVWRT

!           I_TEST = I_TEST + 1
!           DO IRG = 1 , UNRGNS
!              CALL GETBLD(1,IRG)

!              DO IECP = 1 , ECP_D_DSP
!                 IF ((UPVTYP(IECP) .EQ. 1) .OR. (UPTOPR(IECP) .EQ. 3)) THEN
!                    WRITE(18,9331) CURIRUN, CURIYR+1989, CURITR, I_TEST, IRG, IECP, UPVTYP(IECP), UPTOPR(IECP), UPLNTCD(IECP), UPHTRT(IECP), EPPHRT0(IECP), EPPHRTN(IECP)
!                 END IF
!              END DO

!              DO IECP = ECP_D_DSP + 1 , ECP_D_CAP
!                 IF ((UPVTYP(IECP) .EQ. 1) .OR. (UPTOPR(IECP) .EQ. 3)) THEN
!                    WRITE(18,9331) CURIRUN, CURIYR+1989, CURITR, I_TEST, IRG, IECP, UPVTYP(IECP), UPTOPR(IECP), UPLNTCD(IECP), UPHTRT(IECP)
!                 END IF
!              END DO
!           END DO

!           CALL ROUTINE TO ASSIGN RISK PREMIUMS FOR CAPACITY EXPANSION

!           IF (CURIYR .EQ. FIRSYR .AND. CURITR .EQ. 1) CALL ELRSKPR

            IF (FULLYR .EQ. UESTYR) CALL ELRSKPR

         END IF
      ENDIF
!
!     ADD GLOBAL RISK PREMIUM BY YEAR IF APPROPRIATE
      IF (FULLYR .EQ. UPSTYR .AND. CURITR .EQ. 1) THEN
        DO IECP = 1, ECP_D_CAP
          PRMEQ_ORIG(IECP) = UPNRRTEA(IECP)
          PRMDT_ORIG(IECP) = UPNIRTEA(IECP)
        ENDDO
      ENDIF

      IF (CURITR .EQ. 1 .AND. FULLYR .GE. UPSTYR) THEN
       DO IECP = 1, ECP_D_CAP
         IF (UPRSKSW(IECP) .GT. 0.0) THEN
           UPNRRTEA(IECP) = PRMEQ_ORIG(IECP) + EXTRARISK(CURIYR+UHBSYR)
           UPNIRTEA(IECP) = PRMDT_ORIG(IECP) + EXTRARISK(CURIYR+UHBSYR)
         ENDIF
!       write(6,326) ' premiums: ',CURIYR,CURITR,IECP,UPNRRTEA(IECP),UPNIRTEA(IECP),EXTRARISK(CURIYR+UHBSYR)
       ENDDO
      ENDIF
326   FORMAT(1x,a15,3I6,3F12.4)

!
!        CALL PLANNING MODULE (OML) IN UPSTYR AND BEYOND
!
      IF (FULLYR .GE. UPSTYR .AND. ECPSTART .GE. 2) THEN
         IF ((CURITR .EQ. 1) .OR. (I4SITE .EQ. 3)) THEN
!
!           WCDBUG
!           WCDBUG     DUMP ELECTRICITY EXPECTATION VARIABLES TO DEBUG OUTPUT
!           WCDBUG
!
            IF (CURITR .EQ. 1) THEN
               DO XY = 1 , UNYEAR + UNXPH
                  DLTA = XY - CURIYR
                  IF (DLTA .GE. 0) THEN
                     WRITE(UF_MSG,997) 'XQELRS',XY,CURIYR,DLTA,(XQELRS(CR,XY),CR=1,MNUMCR-2)
                     WRITE(UF_MSG,997) 'XQELCM',XY,CURIYR,DLTA,(XQELCM(CR,XY),CR=1,MNUMCR-2)
                     WRITE(UF_MSG,997) 'XQELTR',XY,CURIYR,DLTA,(XQELTR(CR,XY),CR=1,MNUMCR-2)
                     WRITE(UF_MSG,997) 'XQELIN',XY,CURIYR,DLTA,(XQELIN(CR,XY),CR=1,MNUMCR-2)
                     WRITE(UF_MSG,997) 'XQELAS',XY,CURIYR,DLTA,(XQELAS(CR,XY),CR=1,MNUMCR-2)
                  ENDIF
               END DO
            ENDIF
 997        FORMAT(1X,A6,3(1X,I2),9(1X,F12.4))
!
!           WCDBUG
!
            IF(USW_XP .EQ. 0) THEN                      ! OFF FOR CANADA
               CALL ELECP
!
!             OVERWRITE NEMS CARBON FEE WITH ECP RESULT, IF SWITCHES TURNED ON
!
              IF ((USW_CAR .EQ. 1 .OR. USW_CAR .EQ. 3) .AND. (CURIYR + UHBSYR) .GE. UYR_CAR .AND. USW_CAREMM .EQ. 1) THEN
               EMTAX(CURIYR) = ECP_PCAR(0,CURIYR) * 0.001
               EMETAX(1,CURIYR) = ECP_PCAR(0,CURIYR) * 0.001
               CALL SUM_EMISSIONS
              END IF
            ELSE
!
!             WHEN ECP IS TURNED OFF, USE DUMMY CALL TO STRBOUT SO BILDOUT EXISTS
!             AND OTHER MODULES DON'T BOMB
!
              DO IRG = 1 , UNRGNS
                 CALL STRBOUT(CURIYR,IRG)
              END DO
            ENDIF
         END IF
      END IF
!
!     REVISE NET EXPORTS TO REFLECT NEW CANADIAN PROJECTS
!
      IF (CURITR .EQ. 1) THEN
         CALL GETEIJ(CURIYR)
         DO IRG = 1 , UNRGNS
            JRG = URGNUM(IRG)
            CALL GETIN(1,IRG)

!           WRITE(6,3977) CURIRUN, CURIYR+1989, CURITR, IRG, EEITAJ(1), EEITAJ(2), EEITAJ(3)
!3977       FORMAT(1X,"UTIL_00656_EEITAJ_GET",4(":",I4),3(":",F12.3))

            BNW_IMP(JRG) = - UCANBLD(JRG) * 8.760
            DO NSP = 1 , EENSP
               EEITAJ(NSP) = EEITAJ(NSP) - UCANBLD(JRG)

!              WRITE(6,7335) CURIRUN, CURIYR+1989, CURITR, IRG, JRG, NSP, EEITAJ(NSP), UCANBLD(JRG)
!7335          FORMAT(1X,"EEITAJ_UCANBLD",6(":",I4),2(":",F15.3))

            END DO
            CALL STRIN(1,IRG)

!           WRITE(6,2977) CURIRUN, CURIYR+1989, CURITR, IRG, EEITAJ(1), EEITAJ(2), EEITAJ(3)
!2977       FORMAT(1X,"UTIL_00669_EEITAJ_STR",4(":",I4),3(":",F12.3))

         END DO
      END IF
!
!     CAPTURE FUEL DATA
!
      CALL GETFUEL(FULLYR)
!
!     CALL THE LDSM MODULE (IDSM=2 INDICATES GET DSM OUTPUTS FROM ECP)
!
      IF(USW_XP .EQ. 0) THEN                             !NOT FOR CANADA
         IDSM = 2
         CALL ELLDSM(IDSM)
      ENDIF
!
!     EXECUTE THE ECONOMIC FUEL DISPATCH MODULE (ELEFD)
!
!TIME          CALL MPTIM2(CPU_TIME_BEGIN)
!TIME      WRITE(22,60) '***CALLING  ELEFD',
!TIME     +       FLOAT(CPU_TIME_BEGIN)/100.
!
      CALL ELEFD(CURIYR)
!
!     OVERWRITE NEMS CARBON FEE WITH EFD RESULT, IF SWITCHES TURNED ON
!
      IF ((USW_CAR .EQ. 2 .OR. USW_CAR .EQ. 3) .AND. (CURIYR + UHBSYR) .GE. UYR_CAR .AND. USW_CAREMM .EQ. 2) THEN
        EMTAX(CURIYR) = EFD_PCAR(CURIYR) * 0.001
      END IF
!     IF (USW_CAR .GT. 0 .AND. (CURIYR + UHBSYR) .GE. UYR_CAR .AND. USW_CAREMM .GT. 0) THEN
      IF (USW_CAR .GT. 0 .AND. (CURIYR + UHBSYR) .GE. UYR_CAR) THEN
        write(6,1234) curiyr+1989,curitr,  &
                      ecp_scar(curiyr),ecp_qcar(curiyr),ecp_lcar(curiyr),ecp_pcar(0,curiyr) * scalpr,  &
                      EMRSC(11,1,curiyr) + EMCMC(11,1,curiyr) + EMINCC(11,1,curiyr) +  &
                       EMTRC(11,1,curiyr) + EMNT(11,1,curiyr) * 0.001,  &
                      efd_qcar(curiyr),efd_lcar(curiyr),efd_pcar(curiyr) * scalpr,  &
                      (emel(1,1,curiyr) + emel(2,1,curiyr) + emel(3,1,curiyr) + emel(4,1,curiyr)),  &
                      emetax(1,curiyr) * 1000.0 * scalpr
 1234   format(1h ,'!emmcar',2(':',i4),':ecp,s,q,l,p',1(':',4f8.3),':efd,s,q,l,p',1(':',4f8.3),  &
                                       ':nem,q,p',1(':',2f8.3))
      END IF
!
!TIME          CALL MPTIM2(CPU_TIME_END)
!TIME      WRITE(22,60) '***BACK FROM ELEFD',
!TIME     +       FLOAT(CPU_TIME_END)/100.,
!TIME     +   ', Elasped Time=',
!TIME     +       FLOAT(CPU_TIME_END - CPU_TIME_BEGIN)/100.
!
!     STORE CAPACITIES IN EMM OUTPUT COMMON
!
      CALL EMMCAPO
!
!
!     STORE FUEL QUANTITIES IN NEMS GLOBAL ARRAYS
!
      CALL EMMDSPO
!
!     CREATE TABLE OF GENERATION BY PLANT TYPE IN EMMREPT
!
      IF (FCRL .EQ. 1 .OR. CURITR .GT. MAXITR)CALL EMMGENO
!
!     back out generation and capacity additions by distributed PV added through WFLOOR scheme
!
      IF (DPVDISPATCH .EQ. .TRUE. ) THEN
            CALL EMMBODPV
      ENDIF

!
!     STORE EMM FUEL PRICES IN NEMS GLOBAL ARRAYS
!
      CALL EMMPRCO
!
!     CALL ROUTINE TO CREATE SECTORAL ELECTRICITY PRICES BY REGION
!
! add logic to calculate STEO price adjustment in CURITR= 2 only (based on iter 1 price)  -AKN
      IF (CURITR .EQ. 1) THEN
        BCHMRKSTEOPRICEADDER(:,CURIYR) = 0.0
        BCHMRKSTEOPRC_N(:,CURIYR) = 0.0
      ENDIF

      IF (BMELPRC(CURIYR) .GT. 0.0) THEN   !STEO Price benchmark feature --- by AKN
        If (CURITR .EQ. 2) THEN
                            BCHMRKSTEOPRICEADDER(3,CURIYR) = (PELAS(3,CURIYR) * 0.34121416 - ESTCU_ENC (CURIYR)) * QELAS(3,CURIYR) * 2930710
                            BCHMRKSTEOPRICEADDER(6,CURIYR) = (PELAS(6,CURIYR) * 0.34121416  - ESTCU_ESC(CURIYR)) * QELAS(6,CURIYR) * 2930710
                            BCHMRKSTEOPRICEADDER(2,CURIYR) = (PELAS(2,CURIYR) * 0.34121416 - ESTCU_MAC(CURIYR)) * QELAS(2,CURIYR) * 2930710
                            BCHMRKSTEOPRICEADDER(8,CURIYR) = (PELAS(8,CURIYR) * 0.34121416 - ESTCU_MTN(CURIYR)) * QELAS(8,CURIYR) * 2930710
                            BCHMRKSTEOPRICEADDER(1,CURIYR) = (PELAS(1,CURIYR) * 0.34121416 - ESTCU_NEC(CURIYR)) * QELAS(1,CURIYR) * 2930710
                            BCHMRKSTEOPRICEADDER(9,CURIYR) = (PELAS(9,CURIYR) * 0.34121416 - ESTCU_PAC(CURIYR)) * QELAS(9,CURIYR) * 2930710
                            BCHMRKSTEOPRICEADDER(5,CURIYR) = (PELAS(5,CURIYR) * 0.34121416 - ESTCU_SAC(CURIYR)) * QELAS(5,CURIYR) * 2930710
                            BCHMRKSTEOPRICEADDER(4,CURIYR) = (PELAS(4,CURIYR) * 0.34121416 - ESTCU_WNC(CURIYR)) * QELAS(4,CURIYR) * 2930710
                            BCHMRKSTEOPRICEADDER(7,CURIYR) = (PELAS(7,CURIYR) * 0.34121416 - ESTCU_WSC(CURIYR)) * QELAS(7,CURIYR) * 2930710
         DO IRG = 1, UNRGNS
            DO ICR = 1, MNUMCR - 2
               BCHMRKSTEOPRC_N(IRG,CURIYR) = BCHMRKSTEOPRC_N(IRG,CURIYR) + BCHMRKSTEOPRICEADDER(ICR,CURIYR) * MappCtoN(IRG,ICR,4)
            ENDDO
         ENDDO

         ENDIF
         IF (CURITR .GE. 2 .AND. BMELPRC(CURIYR) .GT. 0.0) THEN
             write(22,123) 'STEOPRCADJ ', CURIYR,CURITR,'NEC',PELAS(1,CURIYR)*.3412,ESTCU_NEC(CURIYR),QELAS(1,CURIYR),BCHMRKSTEOPRICEADDER(1,CURIYR)/1000000
             write(22,123) 'STEOPRCADJ ', CURIYR,CURITR,'MAC',PELAS(2,CURIYR)*.3412,ESTCU_MAC(CURIYR),QELAS(2,CURIYR),BCHMRKSTEOPRICEADDER(2,CURIYR)/1000000     
             write(22,123) 'STEOPRCADJ ', CURIYR,CURITR,'ENC',PELAS(3,CURIYR)*.3412,ESTCU_ENC(CURIYR),QELAS(3,CURIYR),BCHMRKSTEOPRICEADDER(3,CURIYR)/1000000
             write(22,123) 'STEOPRCADJ ', CURIYR,CURITR,'WNC',PELAS(4,CURIYR)*.3412,ESTCU_WNC(CURIYR),QELAS(4,CURIYR),BCHMRKSTEOPRICEADDER(4,CURIYR)/1000000             
             write(22,123) 'STEOPRCADJ ', CURIYR,CURITR,'SAC',PELAS(5,CURIYR)*.3412,ESTCU_SAC(CURIYR),QELAS(5,CURIYR),BCHMRKSTEOPRICEADDER(5,CURIYR)/1000000
             write(22,123) 'STEOPRCADJ ', CURIYR,CURITR,'ESC',PELAS(6,CURIYR)*.3412,ESTCU_ESC(CURIYR),QELAS(6,CURIYR),BCHMRKSTEOPRICEADDER(6,CURIYR)/1000000
             write(22,123) 'STEOPRCADJ ', CURIYR,CURITR,'WSC',PELAS(7,CURIYR)*.3412,ESTCU_WSC(CURIYR),QELAS(7,CURIYR),BCHMRKSTEOPRICEADDER(7,CURIYR)/1000000
             write(22,123) 'STEOPRCADJ ', CURIYR,CURITR,'MTN',PELAS(8,CURIYR)*.3412,ESTCU_MTN(CURIYR),QELAS(8,CURIYR),BCHMRKSTEOPRICEADDER(8,CURIYR)/1000000            
             write(22,123) 'STEOPRCADJ ', CURIYR,CURITR,'PAC',PELAS(9,CURIYR)*.3412,ESTCU_PAC(CURIYR),QELAS(9,CURIYR),BCHMRKSTEOPRICEADDER(9,CURIYR)/1000000
             write(22,124) 'STEOPRCADJ NERC',CURIYR,CURITR,(BCHMRKSTEOPRC_N(IRG,CURIYR)/1000000,IRG=1,UNRGNS)
123 FORMAT(1x,A12,2I4,A5,3F12.5,F15.5)
124 FORMAT(1x,A20,2I4,25F15.5)
         ENDIF
      END IF  !STEO Price benchmark feature --- by AKN

      IF(USW_XP .EQ. 0) THEN                             !OFF FOR CANADA
         CALL ELEFP
      ENDIF
!
! If overwrite switch on and historical year then overwrite Electricity Prices by census and emm region.
      IF (USW_OVER .GT. 0) THEN
         IF (((CURIYR + UHBSYR) .LE. UYR_OVER)     &
             .AND. ((CURIYR + UHBSYR) .GE. HSTYEAR)) THEN
           DO IRG = 1 , MNUMNR
             PELRSNR(IRG,CURIYR) = HPELRSNR(IRG,CURIYR)
             PELCMNR(IRG,CURIYR) = HPELCMNR(IRG,CURIYR)
             PELINNR(IRG,CURIYR) = HPELINNR(IRG,CURIYR)
             PELASNR(IRG,CURIYR) = HPELASNR(IRG,CURIYR)
             IF ( HPELTRNR(IRG,CURIYR) .GT. 0.0 ) &
               PELTRNR(IRG,CURIYR) = HPELTRNR(IRG,CURIYR)
             PECGENN(IRG,CURIYR) = PELASNR(IRG,CURIYR) - PECTRNN(IRG,CURIYR) - PECDISN(IRG,CURIYR)
           ENDDO
!
!   replicate average price calculation from main to apply multiplier to price overwrites.
!
           resprqt = 0
           comprqt = 0
           indprqt = 0
           trnprqt = 0
           totprqt = 0
           DO IC = 1 , MNUMCR - 2
            resprqt = resprqt + ((hpelrs(ic,curiyr)/3.412) * qelrs(ic,curiyr))
            comprqt = comprqt + ((hpelcm(ic,curiyr)/3.412) * qelcm(ic,curiyr))
            indprqt = indprqt + ((hpelin(ic,curiyr)/3.412) * qelin(ic,curiyr))
            trnprqt = trnprqt + ((hpeltr(ic,curiyr)/3.412) * qeltr(ic,curiyr))
            totprqt = totprqt + ((hpelrs(ic,curiyr)/3.412) * qelrs(ic,curiyr))        &
                          + ((hpelcm(ic,curiyr)/3.412) * qelcm(ic,curiyr))        &
                          + ((hpelin(ic,curiyr)/3.412) * qelin(ic,curiyr))        &
                          + ((hpeltr(ic,curiyr)/3.412) * qeltr(ic,curiyr))
           ENDDO
           resp = resprqt / qelrs(mnumcr,curiyr)
           comp = comprqt / qelcm(mnumcr,curiyr)
           indp = indprqt / qelin(mnumcr,curiyr)
           trnp = trnprqt / qeltr(mnumcr,curiyr)
           totp = totprqt / qelas(mnumcr,curiyr)
           multp = ( hpelas(mnumcr,curiyr) / 3.412) / totp
           multprs = ( hpelrs(mnumcr,curiyr) / 3.412) / resp
           multpcm = ( hpelcm(mnumcr,curiyr) / 3.412) / comp
           multpin = ( hpelin(mnumcr,curiyr) / 3.412) / indp
           multptr = ( hpeltr(mnumcr,curiyr) / 3.412) / trnp
!
           DO IRG = 1 , MNUMCR
             HRSFAC = 1.0
             HCMFAC = 1.0
             HINFAC = 1.0
             HTRFAC = 1.0
             IF (PELRS(IRG,CURIYR) .GT. 0.0)  &
               HRSFAC = (HPELRS(IRG,CURIYR) * MULTPRS) / 3.412 / PELRS(IRG,CURIYR)
             PELRS(IRG,CURIYR) = HPELRS(IRG,CURIYR) / 3.412
             PELRS(IRG,CURIYR) = PELRS(IRG,CURIYR) * MULTPRS
!           write(6,*) ' resd eu price hrsfac multrs ',hrsfac,multprs
             IF (PELCM(IRG,CURIYR) .GT. 0.0) &
               HCMFAC = (HPELCM(IRG,CURIYR) * multpcm) / 3.412 / PELCM(IRG,CURIYR)
             PELCM(IRG,CURIYR) = HPELCM(IRG,CURIYR) / 3.412
             PELCM(IRG,CURIYR) = PELCM(IRG,CURIYR) * MULTPCM
!           write(6,*) ' comm eu price hcmfac multcm ',hcmfac,multpcm
             IF (PELIN(IRG,CURIYR) .GT. 0.0) &
               HINFAC = (HPELIN(IRG,CURIYR) * multpin) / 3.412 / PELIN(IRG,CURIYR)
             PELIN(IRG,CURIYR) = HPELIN(IRG,CURIYR) / 3.412
             PELIN(IRG,CURIYR) = PELIN(IRG,CURIYR) * MULTPIN
!           write(6,*) ' ind  eu price hinfac multin multp ',hinfac,multpin,multp
             IF ((PELTR(IRG,CURIYR) .GT. 0.0) .AND. (HPELTR(IRG,CURIYR) .GT. 0.0 )) THEN
               HTRFAC = (HPELTR(IRG,CURIYR) * multptr) / 3.412 / PELTR(IRG,CURIYR)
               PELTR(IRG,CURIYR) = HPELTR(IRG,CURIYR) / 3.412
               PELTR(IRG,CURIYR) = PELTR(IRG,CURIYR) * MULTPTR
             ENDIF
!           write(6,*) ' trn  eu price htrfac multtr multp ',htrfac,multptr,multp
             PELAS(IRG,CURIYR) = HPELAS(IRG,CURIYR) / 3.412
             PELAS(IRG,CURIYR) = PELAS(IRG,CURIYR) * MULTP
!            write(6,*) ' hprcfac ',CURIYR,IRG,HRSFAC,HCMFAC,HINFAC
             DO IRSEU = 1, MNEURSGRP
               PELRSOUT(IRG,CURIYR,IRSEU) = PELRS(IRG,CURIYR)
             ENDDO
             DO ICMEU = 1, MNEUCMGRP
               PELCMOUT(IRG,CURIYR,ICMEU) = PELCM(IRG,CURIYR)
             ENDDO
             DO IINEU = 1, MNEUINGRP
               PELINOUT(IRG,CURIYR,IINEU) = PELIN(IRG,CURIYR)
             ENDDO
             PELLTTR(IRG,CURIYR) = PELTR(IRG,CURIYR)
             PELVHTR(IRG,CURIYR) = PELTR(IRG,CURIYR)

!  write(6,8546) ' prctestrs ',curiyr,curitr,irg,(pelrsout(irg,curiyr,irseu),irseu=1,mneursgrp)
!  write(6,8546) ' prctestcm ',curiyr,curitr,irg,(pelcmout(irg,curiyr,icmeu),icmeu=1,mneucmgrp)
!  write(6,8547) ' prctestin ',curiyr,curitr,irg,(pelinout(irg,curiyr,iineu),iineu=1,mneuingrp)
!  write(6,8548) ' prctesttr ',curiyr,curitr,irg,pellttr(irg,curiyr),pelvhtr(irg,curiyr)

           ENDDO
         ENDIF
      ENDIF
 8546 format(1x,a,3I4,10(F8.4))
 8547 format(1x,a,3I4,3(F8.4))
 8548 format(1x,a,3I4,2(F8.4))
!
!     RESERVE MARGIN INFO
!
      IF (CURIYR .EQ. LASTYR .AND. (FCRL .EQ. 1 .OR. CURITR .GT. MAXITR))THEN
!     PEAK DEMAND
         write(13,2100)
 2100 format(1h ,'!rmdmd')
         write(13,2110)
 2110 format(1h ,'!rmdmd',t9,'Peak Demand (Gigawatts)')
         write(13,2100)
         write(13,2120) (usyear(kyr),kyr = 2017 - uhbsyr , unyear)
 2120 format(1h ,'!rmdmd',t9,'Reg',34i6)
         DO IRG = 1 , UNRGNS
            write(13,2130) irg,(rmdmd(irg,kyr),kyr = 2017 - uhbsyr , unyear)
 2130 format(1h ,'!rmdmd',t9,i3,34f6.1)
         END DO
!     RESERVE MARGIN CAPACITY
         write(13,2135)
         write(13,2135)
 2135 format(1h ,'!rmcap')
         write(13,2140)
 2140 format(1h ,'!rmcap',t9,'Capacity Counted Towards Reserve Margin (Gigawatts)')
         write(13,2135)
         write(13,2150) (usyear(kyr),kyr = 2017 - uhbsyr , unyear)
 2150 format(1h ,'!rmcap',t9,'Reg',34i6)
         DO IRG = 1 , UNRGNS
            write(13,2160) irg,(rmcap(irg,kyr),kyr = 2017 - uhbsyr , unyear)
 2160 format(1h ,'!rmcap',t9,i3,34f6.1)
         END DO
!     CAPACITY ADJUSTMENTS FOR TRANSFERS AND COGEN
         write(13,2165)
         write(13,2165)
 2165 format(1h ,'!rmadj')
         write(13,2170)
 2170 format(1h ,'!rmadj',t9,'Capacity Adjustments for Transfers and Cogen (Gigawatts)')
         write(13,2165)
         write(13,2175) (usyear(kyr),kyr = 2017 - uhbsyr , unyear)
 2175 format(1h ,'!rmadj',t9,'Reg',34i6)
         DO IRG = 1 , UNRGNS
            write(13,2180) irg,(rmadj(irg,kyr),kyr = 2017 - uhbsyr , unyear)
 2180 format(1h ,'!rmadj',t9,i3,34f6.1)
         END DO
!     RESERVE MARGIN
         write(13,2184)
         write(13,2184)
 2184 format(1h ,'!rmlev')
         write(13,2185)
 2185 format(1h ,'!rmlev',t9,'Reserve Margins (Percent)')
         write(13,2184)
         write(13,2190) (usyear(kyr),kyr = 2017 - uhbsyr , unyear)
 2190 format(1h ,'!rmlev',t9,'Reg',34i5,'  Min')
         DO IRG = 1 , UNRGNS
            CALL GETBLD(1,IRG)
            write(13,2195) irg,(rmlev(irg,kyr),kyr = 2017 - uhbsyr , unyear),epmrm * 100.0
 2195 format(1h ,'!rmlev',t9,i3,34f5.1,f5.1)
         END DO
      END IF
!
!     RELIABILITY PRICE INFO
!
!     IF (CURIYR .EQ. UNYEAR .AND. (FCRL .EQ. 1 .OR. CURITR .GT. MAXITR))THEN
      IF (CURIYR .EQ. LASTYR .AND. (FCRL .EQ. 1 .OR. CURITR .GT. MAXITR))THEN
           DO IRG = 1 , UNRGNS
           write(13,2222) curiyr+1989,irg,epmrmin(irg),ureltgt(irg) * scalpr,  &
                          urelrsv(1,irg),pecasrln(irg,curiyr) * scalpr,  &
                          urelrsv(2,irg),urelprc(2,irg) * scalpr,  &
                          urelrsv(3,irg),urelprc(3,irg) * scalpr
 2222 format(1h ,'!rel,yr,rg,rmi,tgt,r1,p1,r2,p2,r3,p3',i4,i3,9f10.3,a3)
           ENDDO
      END IF
!
!     CAPITAL COST ESCALATION FACTORS
!
!     IF (CURIYR .EQ. UNYEAR .AND. (FCRL .EQ. 1 .OR. CURITR .GT. MAXITR))THEN
      IF (CURIYR .EQ. LASTYR .AND. (FCRL .EQ. 1 .OR. CURITR .GT. MAXITR))THEN
              write(13,2233)  &
                              uplntcd(wipc),  &
                              uplntcd(wiig),  &
                              uplntcd(wiis),  &
                              uplntcd(wict),  &
                              uplntcd(wiat),  &
                              uplntcd(wicc),  &
                              uplntcd(wiac),  &
                              uplntcd(wics),  &
                              uplntcd(wian),  &
                              uplntcd(wism),  &
                              uplntcd(wiwd),  &
                              uplntcd(wigt),  &
                              uplntcd(wiwn),  &
                              uplntcd(wiso),  &
                              uplntcd(wipv)
 2233 format(1h ,'!capesc',5x,16(5x,a2))
           DO KYR = 1 , UNYEAR
              IF ((KYR + UHBSYR) .GE. UPSTYR)THEN
              write(13,2244) kyr+uhbsyr,  &
                              upannadj(wipc,kyr),  &
                              upannadj(wiig,kyr),  &
                              upannadj(wiis,kyr),  &
                              upannadj(wict,kyr),  &
                              upannadj(wiat,kyr),  &
                              upannadj(wicc,kyr),  &
                              upannadj(wiac,kyr),  &
                              upannadj(wics,kyr),  &
                              upannadj(wian,kyr),  &
                              upannadj(wism,kyr),  &
                              upannadj(wiwd,kyr),  &
                              upannadj(wigt,kyr),  &
                              upannadj(wiwn,kyr),  &
                              upannadj(wiso,kyr),  &
                              upannadj(wipv,kyr)
 2244 format(1h ,'!capesc',i5,16f7.3)
              END IF
           END DO
      END IF
!
!     REGIONAL CARBON PRICES (EFD AND ECP)
!
!     IF (CURIYR .EQ. UNYEAR .AND. (FCRL .EQ. 1 .OR. CURITR .GT. MAXITR))THEN
      IF (CURIYR .EQ. LASTYR .AND. (FCRL .EQ. 1 .OR. CURITR .GT. MAXITR))THEN
           write(13,3301)
 3301 format(1h ,'!co2prc',t25,'Regional Carbon Prices ($/MT CO2)')
           write(13,3302)
 3302 format(1h ,'!co2prc')
           write(13,3303)
 3303 format(1h ,'!co2prc',t10,'Year',4(5x,'EFD',5x,'ECP',1x))
           write(13,3304) ((co2_rg(irg),co2_rg(irg)),irg = 1 , co2_grp)
 3304 format(1h ,'!co2prc',t10,'    ',4(2(6x,a2),1x))
           DO KYR = 1 , UNYEAR
              IF ((KYR + UHBSYR) .GE. UPSTYR)THEN
              write(13,3305) kyr+uhbsyr,((ucarprc(irg,kyr) * (12.0 / 44.0) * scalpr,  &
                                          epcarpr(irg,kyr) * (12.0 / 44.0) * scalpr), irg = 1 , co2_grp)
 3305 format(1h ,'!co2prc',i5,4(2f8.2,1x))
              END IF
           END DO
      END IF
!
!          REGIONAL CARBON TARGETS AND EMISSIONS (EFD AND ECP)
!
!     IF (CURIYR .EQ. UNYEAR .AND. (FCRL .EQ. 1 .OR. CURITR .GT. MAXITR))THEN
      IF (CURIYR .EQ. LASTYR .AND. (FCRL .EQ. 1 .OR. CURITR .GT. MAXITR))THEN
           write(13,4401)
 4401 format(1h ,'!co2ems',t20,'Regional Carbon Targets and Emissions (Million MT CO2)')
           write(13,4402)
 4402 format(1h ,'!co2ems')
           write(13,4403)
 4403 format(1h ,'!co2ems',t10,'Year',4(5x,'   ',5x,'EFD',5x,'ECP',1x))
           write(13,4404)
 4404 format(1h ,'!co2ems',t10,'Year',4(5x,'TGT',5x,'EMS',5x,'EMS',1x))
           write(13,4405) ((co2_rg(irg),co2_rg(irg),co2_rg(irg)),irg = 1 , co2_grp)
 4405 format(1h ,'!co2ems',t10,'    ',4(3(6x,a2),1x))
           DO KYR = 1 , UNYEAR
              IF ((KYR + UHBSYR) .GE. UPSTYR)THEN
              write(13,4406) kyr+uhbsyr,((cartgt(irg,kyr) * (44.0 / 12.0),  &
                                          carefd(irg,kyr) * (44.0 / 12.0),  &
                                          carecp(irg,kyr) * (44.0 / 12.0)), irg = 1 , co2_grp)
 4406 format(1h ,'!co2ems',i5,4(3f8.1,1x))
              END IF
           END DO
      ENDIF
!
!     Print out total demand by emm region for base values of loss adjustment calculations
!
      IF (CURIYR .EQ. LASTYR .AND. (FCRL .EQ. 1 .OR. CURITR .GT. MAXITR))THEN
        DO KYR = (UYR_HIST - UHBSYR) , UNYEAR
         write(13,4457) col,kyr+uhbsyr,col,((qelasn(irg,kyr)*.001),irg = 1 , unrgns)
        ENDDO
      ENDIF
 4457 format(1x,A1,I4,A1,1x,<unrgns>(f7.2,1x),1x,'demloss')

!
!     IF NO CAIR, ZERO OUT EMISSIONS TARGETS
!
      IF (CURIYR .EQ. UNYEAR .AND. (FCRL .EQ. 1 .OR. CURITR .GT. MAXITR))THEN
         IF (UYR_NOCAIR .GT. 0)THEN
            DO KYR = UYR_NOCAIR - UHBSYR , MNUMYR
               DO IGRP = 1 , NUM_SO2_GRP
                  EMRFSA(KYR,IGRP) = 0.0
               END DO
            END DO
         END IF
      ENDIF
!
!
      DO IREG=1,MNUMNR - 2
        TOTREVNW(CURIYR,IREG) = 0.0
      END DO ! IREG
!
      DO IREG = 1 , NDREG
         DO IECP = 1 , ECP_D_DSP
            UEFD_GEN(IECP,IREG,CURIYR) = 0.0
            UEFD_CAP(IECP,IREG,CURIYR) = 0.0
         END DO ! IECP
         DO ICNFG = 1 , MX_CNFG
            CNFG_GEN(ICNFG,IREG) = 0.0
            CNFG_CAP(ICNFG,IREG) = 0.0
         END DO ! ICNFG
      END DO ! IREG
!
!     IF ANNUAL ITC OVERWRITE IS ACTIVE THEN REPLACE CURRENT ITC
!
      IF (FCRL .EQ. 1 .OR. CURITR .GT. MAXITR)THEN
!        DISPATCHABLE
         DO IP = 1 , ECP_D_DSP
            IECP = UCPDSPI(IP)
            IF (UPCSBYR(IECP,MIN(MNUMYR,CURIYR + 1 + UPPLYR(IECP))) .GE. 0.0)THEN
               UPOVR(IECP) = UPOVR(IECP) / (1.0 - UPCSB(IECP))
               UPCSB(IECP) = UPCSBYR(IECP,MIN(MNUMYR,CURIYR + 1 + UPPLYR(IECP)))
               UPOVR(IECP) = UPOVR(IECP) * (1.0 - UPCSB(IECP))
            END IF
         END DO
!        STORAGE
         DO IP = 1 , ECP_D_STO
            IECP = UCPSTOI(IP)
            IF (IECP .GT. 0) THEN
               IF (UPCSBYR(IECP,MIN(MNUMYR,CURIYR + 1 + UPPLYR(IECP))) .GE. 0.0)THEN
                  UPOVR(IECP) = UPOVR(IECP) / (1.0 - UPCSB(IECP))
                  UPCSB(IECP) = UPCSBYR(IECP,MIN(MNUMYR,CURIYR + 1 + UPPLYR(IECP)))
                  UPOVR(IECP) = UPOVR(IECP) * (1.0 - UPCSB(IECP))
               END IF
            END IF
         END DO
!        RENEWABLE
         DO IP = 1 , ECP_D_RNW
            IECP = UCPRNWI(IP)
            IRNW = UIRRNWI(IP)
            IF (IECP .GT. 0) THEN
               IF (UPCSBYR(IECP,MIN(MNUMYR,CURIYR + 1 + UPPLYR(IECP))) .GE. 0.0)THEN
                  OLDITC = UPCSB(IECP)
                  UPCSB(IECP) = UPCSBYR(IECP,MIN(MNUMYR,CURIYR + 1 + UPPLYR(IECP)))
                  DO IRG = 1 , UNRGNS
                     CALL GETBLD(1,IRG)
                     EPIROVR(IRNW) = (EPIROVR(IRNW) / (1.0 - OLDITC)) * (1.0 - UPCSB(IECP))
                     CALL STRBLD(1,IRG)
                  END DO
               END IF
            END IF
         END DO
!        INTERMITTENT
         DO IP = 1 , ECP_D_INT
            IECP = UCPINTI(IP)
            IRNW = UIRINTI(IP)
            IF (UPCSBYR(IECP,MIN(MNUMYR,CURIYR + 1 + UPPLYR(IECP))) .GE. 0.0)THEN
               OLDITC = UPCSB(IECP)
               UPCSB(IECP) = UPCSBYR(IECP,MIN(MNUMYR,CURIYR + 1 + UPPLYR(IECP)))
               DO IRG = 1 , UNRGNS
                  CALL GETBLD(1,IRG)
                  EPIROVR(IRNW) = (EPIROVR(IRNW) / (1.0 - OLDITC)) * (1.0 - UPCSB(IECP))
                  CALL STRBLD(1,IRG)
               END DO
            END IF
         END DO
!        DISTRIBUTED
         DO IP = 1 , ECP_D_DGN
            IECP = UCPDGNI(IP)
            IF (UPCSBYR(IECP,MIN(MNUMYR,CURIYR + 1 + UPPLYR(IECP))) .GT. 0.0)THEN
               UPOVR(IECP) = UPOVR(IECP) / (1.0 - UPCSB(IECP))
               UPCSB(IECP) = UPCSBYR(IECP,MIN(MNUMYR,CURIYR + 1 + UPPLYR(IECP)))
               UPOVR(IECP) = UPOVR(IECP) * (1.0 - UPCSB(IECP))
            END IF
         END DO
      END IF

!
!     fill in capacity credit by region/type to assign capacity payments
!
      DO IREG = 1, UNRGNS
        CALL GETBLD(1,IREG)
        DO IP = 1, ECP_D_CAP
          CAPCCR(IREG,IP) = UPCCR(IP)
        ENDDO

        DO IP = 1, ECP_D_RNW + ECP_D_INT + ECP_D_STO
          CAPCCR(IREG,ECP_D_DSP+IP) = EPIRCCR(IP) ! Edt: this is where the capacity credit is assigned per region
        ENDDO

        CAPCCR(IREG,WIHY) = RNWFAC

      ENDDO

!     Initialize Arrays Needed to Calculate Capacity Factors By Coal Unit
!
      CL_GEN = 0.0
      CL_CAP = 0.0
!
!     initialize summary costs
!
      VCSTTOT=0.0
      FCSTTOT=0.0
      SO2PTOT=0.0
      NOXPTOT=0.0
      RPSPTOT=0.0
      HGPTOT =0.0
      TGEN = 0.0
      TCON = 0.0
      US_AVG_HTRT = 0.0
      AVG_HTRT_GEN = 0.0
      US_AVG_HTRT_G = 0.0
      AVG_HTRT_G_GEN = 0.0
      TFOSSGEN = 0.0
      TFOSSCON = 0.0
      TFOSSGENCR = 0.0
      TFOSSCONCR = 0.0

      LYR_INDEX = (CURIYR + UHBSYR) * 100 + 12
      CYR_INDEX = (CURIYR + UHBSYR) * 100

      DO IGRP = 1 , EMM_D_GRP
         IF (ULSINDX(IGRP) .LE. LYR_INDEX .AND. ULRINDX(IGRP) .GE. CYR_INDEX .AND.  ULTGEN(IGRP) .GT. 0.0) THEN
            KRG = ULORGN(IGRP)
            GEN = 0.0
!
!           Calculate Total Generation and Consumption in order to calculate an average heatrate by ECP Type and EMM Region (Owner Region)
!
            IECP = ULECPT(IGRP)
            ITYP = UPTTYP(IECP)
            IC = ULCENS(IGRP)

            IF (ULOWNT(IGRP) .LT. EFD_D_OWN .AND. KRG .GT. 0 .AND. IECP .GT. 0) THEN

               AVG_HTRT_GEN(IECP) = AVG_HTRT_GEN(IECP) + ULTGEN(IGRP)
               AVG_HTRT_G_GEN(ITYP) = AVG_HTRT_G_GEN(ITYP) + ULTGEN(IGRP)

               TGEN(IECP,KRG) = TGEN(IECP,KRG) + ULTGEN(IGRP)
               DO IFPP = 1 , EFD_D_FPP
                  US_AVG_HTRT(IECP) = US_AVG_HTRT(IECP) + ULBTUE(IFPP,IGRP)
                  US_AVG_HTRT_G(ITYP) = US_AVG_HTRT_G(ITYP) + ULBTUE(IFPP,IGRP)
                  TCON(IECP,KRG) = TCON(IECP,KRG) + ULBTUE(IFPP,IGRP)
                  IFL = ULFUEL(IFPP,IGRP)
                  IF (IFL .EQ. 0 .AND. ULBTUE(IFPP,IGRP) .GT. 0.0) THEN
                     WRITE(6,3753) CURIYR+UHBSYR,CURITR,ULIGRP(IGRP),IGRP,KRG,IECP,IFPP,ULTGEN(IGRP),ULBTUE(IFPP,IGRP),ULCAPC(IGRP)
 3753                FORMAT(1X,"ERROR_BTU_BUT_NO_FUEL_TYPE",7(":",I5),3(":",E15.3))
                  END IF

 !                IF fossil fuel sum for census region fossil fuel heat rate calculation
                  IF ( IFL .LE. 44 ) THEN
                    TFOSSGENCR(IC) = TFOSSGENCR(IC) + ULGENE(IFPP,IGRP)
                    TFOSSCONCR(IC) = TFOSSCONCR(IC) + ULBTUE(IFPP,IGRP)
                    TFOSSGEN = TFOSSGEN + ULGENE(IFPP,IGRP)
                    TFOSSCON = TFOSSCON + ULBTUE(IFPP,IGRP)
                  ENDIF

               END DO
            END IF

            IF (KRG .EQ. 0) THEN
!              WRITE(6,2753) CURIYR+UHBSYR,CURITR,IGRP,ULCAPC(IGRP)
 2753          FORMAT(1X,"OOPS4",3(":",I5),":",F9.1)
               KRG = 1
            END IF
            DO ISP = 1 , EENSP
               GEN_SP(ISP) = 0.0
               DO IV = 1 , ELNVCT(ISP)
                  ULREVS(IGRP) = ULREVS(IGRP) +  &
                     ELGENE(IV,ISP,IGRP) * 0.001 *  &
                     ELGENP(IV,ISP,KRG)
                  ULVCST(IGRP) = ULVCST(IGRP) +  &
                     ELGENE(IV,ISP,IGRP) * 0.001 *  &
                     ULCSTR(ISP,IGRP)
                  ULSO2P(IGRP) = ULSO2P(IGRP) +  &
                     ELGENE(IV,ISP,IGRP) * 0.001 *  &
                     ELSO2P(ISP,IGRP)
                  ULNOXP(IGRP) = ULNOXP(IGRP) +  &
                     ELGENE(IV,ISP,IGRP) * 0.001 *  &
                     ELNOXP(ISP,IGRP)
                  ULRPSP(IGRP) = ULRPSP(IGRP) +  &
                     ELGENE(IV,ISP,IGRP) * 0.001 *  &
                     ELRPSP(ISP,IGRP)
                  ULHGP(IGRP) = ULHGP(IGRP) +  &
                     ELGENE(IV,ISP,IGRP) * 0.001 *  &
                     ELHGP(ISP,IGRP)
                  GEN = GEN + ELGENE(IV,ISP,IGRP)
                  GEN_SP(ISP) = GEN_SP(ISP) + ELGENE(IV,ISP,IGRP)
               END DO
            END DO
           IF (FCRL .EQ. 1)WRITE(18,4920)'0-ULREVS b4 ',CURIYR,KRG,IGRP,IECP,ULREVS(IGRP), ULCAPC(IGRP),RMAVG(CURIYR,KRG),  MC_JPGDP(CURIYR)
           ULENGREVS(IGRP) = ULREVS(IGRP)
!          IF (ULTGEN(IGRP) .GT. 0.0) ULREVS(IGRP) = ULREVS(IGRP) + (ULCAPC(IGRP) * RMAVG(CURIYR,KRG) *  MC_JPGDP(CURIYR))/1000
           IF (ULTGEN(IGRP) .GT. 0.0) ULREVS(IGRP) = ULREVS(IGRP) + (ULCAPC(IGRP) * CAPCCR(KRG,IECP) * RMAVG(CURIYR,KRG))/1000
           IF (FCRL .EQ. 1)WRITE(18,4920)'0-ULREVS af ',CURIYR,KRG,IGRP,IECP,ULREVS(IGRP), ULCAPC(IGRP),RMAVG(CURIYR,KRG), CAPCCR(KRG,IECP),ULENGREVS(IGRP)
4920  FORMAT(A25,1x,4(I5,1x),5(F20.9,1x))
         ELSE
            ULREVS(IGRP) = 0.0
            ULENGREVS(IGRP) = 0.0
            ULVCST(IGRP) = 0.0
            ULSO2P(IGRP) = 0.0
            ULNOXP(IGRP) = 0.0
            ULRPSP(IGRP) = 0.0
            ULHGP(IGRP) = 0.0
            DO ISP = 1 , EENSP
               GEN_SP(ISP) = 0.0
            END DO
         END IF
!
!        Check if new unit and not a screwy hydro
!
         IREG = ULORGN(IGRP)
         IF( ULVINT(IGRP) .eq. NEWUNIT .and. ULREVS(IGRP) .eq. ULREVS(IGRP)) THEN
           TOTREVNW(CURIYR,IREG) = TOTREVNW(CURIYR,IREG) + ULREVS(IGRP) - ULVCST(IGRP) - ULCCST(IGRP) - ULFCST(IGRP) - ULSO2P(IGRP) - ULNOXP(IGRP) - ULRPSP(IGRP) - ULHGP(IGRP)
         ENDIF

         IECP = ULECPT(IGRP)
         IF (ULOWNT(IGRP) .LT. EFD_D_OWN .AND. IREG .GT. 0 .AND. IECP .GT. 0) THEN
            IF (ULSINDX(IGRP) .LE. LYR_INDEX .AND. ULRINDX(IGRP) .GE. CYR_INDEX .AND.  ULTGEN(IGRP) .GT. 0.0) THEN
               IF (UPTTYP(IECP) .LE. NW_COAL) THEN
                  CRG = 0
                  DO IFPP = 1 , EFD_D_FPP
                     IF (ULFUEL(IFPP,IGRP) .EQ. UIB1 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIB2 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIB3 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIB4 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIB5 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIB6 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIB7 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIB8 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIC1 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIC2 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIC3 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIC4 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIC5 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIC6 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIC7 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIC8 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIC9 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UICX .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UICY .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UICZ .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIH1 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIH2 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIH3 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIH4 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIH5 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIH6 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIH7 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIH8 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIH9 .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIHA .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIHB .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIHC .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIPC .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIIG .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIPQ .OR.  &
                         ULFUEL(IFPP,IGRP) .EQ. UIIS) CRG = ULFLRG(IFPP,IGRP)
                  END DO
                  IF (CRG .GT. 0 .AND. CRG .LE. NDREG) THEN
                     UEFD_CAP(IECP,CRG,CURIYR) = UEFD_CAP(IECP,CRG,CURIYR) + ULCAPC(IGRP)
                     UEFD_GEN(IECP,CRG,CURIYR) = UEFD_GEN(IECP,CRG,CURIYR) + ULTGEN(IGRP)
                  END IF
                  I_COAL = UCL_CGRP2(IGRP)
                  IF (I_COAL .GT. 0) THEN
                     CL_GEN(I_COAL) = CL_GEN(I_COAL) + ULTGEN(IGRP)
                     CL_CAP(I_COAL) = CL_CAP(I_COAL) + ULCAPC(IGRP)
                     ICNFG = ECL_RCFG(1,I_COAL)
                     IF (CRG .GT. 0) THEN
                        IF (ICNFG .GT. 0) THEN
                           CNFG_CAP(ICNFG,CRG) = CNFG_CAP(ICNFG,CRG) + ULCAPC(IGRP)
                           CNFG_GEN(ICNFG,CRG) = CNFG_GEN(ICNFG,CRG) + ULTGEN(IGRP)
                        END IF
                     ELSE

                        WRITE(6,9731) CURIRUN, CURIYR+1989, CYR_INDEX, LYR_INDEX, ULSINDX(IGRP), ULRINDX(IGRP), IGRP, I_COAL, IECP, ULCAPC(IGRP), ULSCAP_ECP(IGRP,1), ULTGEN(IGRP), (ULFUEL(IFPP,IGRP),IFPP=1,EFD_D_FPP)
 9731                   FORMAT(1X,"CRG_OOPS_UTIL",9(":",I6),3(":",F21.6),<EFD_D_FPP>(":",F21.6))

                     END IF
                  END IF
               END IF
            END IF
         END IF
!
         IF (FCRL .EQ. 1 .AND. ULORGN(IGRP) .GT. 0 &
            .AND. UF_DBG .GT. 0) THEN
            WRITE(UF_DBG,2391) CURIYR,CURITR,IGRP,ULIGRP(IGRP), &
               ULORGN(IGRP),ULOPER(IGRP),ULCENS(IGRP),ULECPT(IGRP), &
               ULEFDT(IGRP),ULEFPT(IGRP),ULVINT(IGRP),ULOWNT(IGRP), &
               UP_FL_RG(IGRP), &
               (ULFUEL(IFPP,IGRP),ULFLRG(IFPP,IGRP),IFPP=1,EFD_D_FPP), &
               ULMRUN(IGRP),ULRPS(IGRP),ULCAPC(IGRP),ULRETC(IGRP), &
               ULTGEN(IGRP),(ULGENE(IFPP,IGRP),IFPP=1,EFD_D_FPP), &
               ULREVS(IGRP),ULCCST(IGRP),ULFCST(IGRP),ULVCST(IGRP), &
               ULSO2P(IGRP),ULNOXP(IGRP),ULRPSP(IGRP),ULHGP(IGRP),ULGHG(IGRP), &
               ((ELGENE(IV,ISP,IGRP),IV=1,3),ISP=1,3)
!
 2391       FORMAT(1X,"WGRP",2(":",I2),2(":",I5),2(":",I3), &
               13(":",I2),2(":",I4),15(":",F10.3),9(":",F10.3))
!
            GEN = 0.0
            DO IFPP = 1 , EFD_D_FPP
               GEN = GEN + ULFUEL(IFPP,IGRP)
            END DO
!
            WRITE(UF_DBG,2492) CURIYR,CURITR,IGRP,ULIGRP(IGRP), &
            GEN,ULTGEN(IGRP),(ELNOXP(ISP,IGRP),ISP=1,EENSP),(GEN_SP(IV),IV=1,EENSP)
!
 2492       FORMAT(1X,"WNOX",2(":",I2),2(":",I5),14(":",F10.3))
         END IF
         VCSTTOT = VCSTTOT + ULVCST(IGRP)
         FCSTTOT = FCSTTOT + ULFCST(IGRP)
         SO2PTOT = SO2PTOT + ULSO2P(IGRP)
         NOXPTOT = NOXPTOT + ULNOXP(IGRP)
         RPSPTOT = RPSPTOT + ULRPSP(IGRP)
         HGPTOT = HGPTOT + ULHGP(IGRP)
!
      END DO ! igrp
!
!     Store Average Heatrates
!
      DO IECP = 1 , ECP_D_CAP
         ITYP = UPTTYP(IECP)
         IF (AVG_HTRT_GEN(IECP) .GT. 0.0) THEN
            US_AVG_HTRT(IECP) = US_AVG_HTRT(IECP) / (AVG_HTRT_GEN(IECP) * 0.001)
         ELSE IF (AVG_HTRT_G_GEN(ITYP) .GT. 0.0) THEN
            US_AVG_HTRT_G(IECP) = US_AVG_HTRT_G(ITYP) / (AVG_HTRT_G_GEN(ITYP) * 0.001)
         ELSE
            US_AVG_HTRT(IECP) = 9999.999
         END IF

!        IF overwrite year set wood and msw heat rates to aer hrates

         IF (USW_OVER .GT. 0) THEN
           IF (((CURIYR + UHBSYR) .LE. UYR_OVER)     &
             .AND. ((CURIYR + UHBSYR) .GE. HSTYEAR)) THEN
             US_AVG_HTRT(WIWD) = HWODHR(CURIYR)
             US_AVG_HTRT(WIMS) = HMSWHR(CURIYR)
           ENDIF
         ENDIF
      END DO
!
!     Store fossil fuel heat rates
!
!     IF (FCRL .EQ. 1 .OR. CURITR .GT. MAXITR) THEN
!     IF ( CURITR .EQ. 1 ) THEN
      IF ( (CURIYR + UHBSYR) .GT. UYR_OVER) THEN
!
        DO IC = 1 , MNUMCR - 2
          IF ( TFOSSGENCR(IC) .GT. 0.0 ) THEN
                  WHRFOSS(IC,CURIYR) = TFOSSCONCR(IC) / (TFOSSGENCR(IC) * 0.001)
          ENDIF
        ENDDO

        WHRFOSS(10,CURIYR) = TFOSSCON / (TFOSSGEN * 0.001)
        WHRFOSS(11,CURIYR) = TFOSSCON / (TFOSSGEN * 0.001)

 1294   FORMAT(1X,a,i4,i4,3(F12.4))

        DO KRG = 1 , UNRGNS
          CALL GETBLD(1,KRG)
          IF (TFOSSGEN.GT.0.0) EPHTRT_AER(WIGT) = TFOSSCON / (TFOSSGEN * 0.001)
          IF (TFOSSGEN.GT.0.0 )EPHTRT_AER(WIHY) = TFOSSCON / (TFOSSGEN * 0.001)
          IF (TFOSSGEN.GT.0.0) EPHTRT_AER(WIPS) = TFOSSCON / (TFOSSGEN * 0.001)
          IF (TFOSSGEN.GT.0.0) EPHTRT_AER(WIWN) = TFOSSCON / (TFOSSGEN * 0.001)
          IF (TFOSSGEN.GT.0.0) EPHTRT_AER(WIWF) = TFOSSCON / (TFOSSGEN * 0.001)
          IF (TFOSSGEN.GT.0.0) EPHTRT_AER(WISO) = TFOSSCON / (TFOSSGEN * 0.001)
          IF (TFOSSGEN.GT.0.0) EPHTRT_AER(WIPV) = TFOSSCON / (TFOSSGEN * 0.001)
          CALL STRBLD(1,KRG)
        ENDDO
!
!       Set UPHTRT heat rates to EPHTRT_AER current values
!
        DO IECP = 1, ECP_D_CAP
         IF (IECP .EQ. WIGT .OR. IECP .EQ. WIHY .OR. IECP .EQ. WIPS .OR.          &
            IECP .EQ. WIWN .OR. IECP .EQ. WIWF .OR. IECP .EQ. WISO .OR. IECP .EQ. WIPV ) THEN

            UPHTRT(IECP) = EPHTRT_AER(IECP)
         ELSEIF (IECP .EQ. WIWL) THEN
            UPHTRT(IECP) = EPHTRT_AER(WIWN)
         ELSEIF (IECP .EQ. WIPT) THEN
            UPHTRT(IECP) = EPHTRT_AER(WIPV)
         ENDIF
        ENDDO

      ENDIF
!
!     Calculate Average Heatrate by ECP Type and EMM Region
!
      IF (FCRL .EQ. 1) THEN
         FROM_LABEL = "UTIL_US_AVG"
         DO KRG = 1 , UNRGNS
            CALL GETBLD(1,KRG)
            DO IECP = 1 , ECP_D_CAP
               CALL ECP_AVG_HTRT(FROM_LABEL, KRG, 0, IECP, 1, AVG_HTRT, AVG_HTRT_MR, AVG_HTRT_MOD, AVG_HTRT_MR_MOD, ECP_GEN, ECP_GEN_MR, ECP_GEN_MOD, ECP_GEN_MR_MOD)
               WRITE(18,4492) CURIYR+UHBSYR, CURITR, KRG, IECP, TGEN(IECP,KRG), TCON(IECP,KRG), AVG_HTRT(IECP), US_AVG_HTRT(IECP), US_AVG_HTRT_G(IECP), UPHTRT(IECP)
 4492          FORMAT(1X,"UECP_HTRT_UTIL",4(":",I4),6(":",E15.8))
            END DO
         END DO
      END IF
!
!     Store Coal Utilization by Unit
!
      DO I_COAL = 1 , NUM_CL
         IF (CL_CAP(I_COAL) .GT. 0.0) THEN
            EMM_CL_CF(I_COAL,CURIYR) = CL_GEN(I_COAL) / (CL_CAP(I_COAL) * 8.76)
            IF (FCRL .EQ. 1) THEN
               WRITE(18,3492) CURIYR+UHBSYR,CURITR,I_COAL,CL_GEN(I_COAL),CL_CAP(I_COAL),EMM_CL_CF(I_COAL,CURIYR)
 3492          FORMAT(1X,"EMM_CL_CF",3(":",I4),3(":",E13.6))
            END IF
         ELSE
            EMM_CL_CF(I_COAL,CURIYR) = 0.0
         END IF
      END DO

      IF (FCRL .EQ. 1) THEN
!
!        Collect Resource Cost Data - Total_Vcst
!
         Total_Vcst(MNUMNR,CURIYR) = VCSTTOT
!
         Carbon_Rev(MNUMNR,CURIYR) = (EMEL(1,1,CURIYR) + EMEL(2,1,CURIYR) + EMEL(3,1,CURIYR)) * EMETAX(1,CURIYR) * 1000.0
         if (PCAP_CAR .EQ. 2) then
            if (TAX_FLAG .OR. OFFSET_FLAG) then
               caralloc = EMLIM(1,CURIYR) * EMETAX(1,CURIYR) * 1000.0
               Carbon_Rev(MNUMNR,CURIYR) = Carbon_Rev(MNUMNR,CURIYR) - caralloc
            else
               Carbon_Rev(MNUMNR,CURIYR) = 0.0
            endif
         endif
         if(BANK_FLAG .AND. CURCALYR .LT. BANK_STARTYR) then
            Carbon_Rev(MNUMNR,CURIYR) = 0.0
         endif
         write(18,2332) CURIYR,CURITR,VCSTTOT,FCSTTOT,SO2PTOT,NOXPTOT,RPSPTOT,HGPTOT
 2332    FORMAT(1x,'CSTSUMRY: ',2(":",I4),6(":",F10.2))
!
!        Resource Data Debug
!
        DO IR = 1, MNUMNR
         IF (IR .NE. MNUMNR - 1 .AND. IR .NE. MNUMNR - 2) THEN
         WRITE(18,3331) CURIYR+UHBSYR,CURITR,IR,CP_ADD(IR,CURIYR),Carbon_Rev(IR,CURIYR),Ret_Cst(IR,CURIYR),    &
            Total_Vcst(IR,CURIYR),Total_RCC(IR,CURIYR),Total_RIC(IR,CURIYR),Total_RPS(IR,CURIYR),              &
            Fuel_VOM(IR,CURIYR),Non_Fuel_VOM(IR,CURIYR),Total_FOM(IR,CURIYR),Total_VOM(IR,CURIYR),             &
            T_DomEcon(IR,CURIYR),T_DomFirm(IR,CURIYR),T_IntExp(IR,CURIYR),T_IntImp(IR,CURIYR),Ret_Inv(IR,CURIYR), &
            New_Cap_EL(IR,CURIYR),G_INST_ALL(IR,CURIYR),T_OVR(IR,CURIYR),G_ANN(IR,CURIYR),T_ANN(IR,CURIYR)
 3331    FORMAT(1X,"RCst",":",I4,":",I2,":",I2,21(":",F8.1))
!        Label:RCst:CYEAR:CURITR:IRG:CP_ADD:Carbon_Rev:Ret_Cst:Total_Vcst:Total_RCC:Total_RIC:Total_RPS:Fuel_VOM:Non_Fuel_VOM:Total_FOM:Total_VOM:T_DomEcon:T_DomFirm:T_IntExp:T_IntImp:Ret_Inv:New_Cap_EL:G_INST_ALL:T_OVR:G_ANN:T_ANN
         ENDIF
        ENDDO
      ENDIF
!
      IF (FCRL .EQ. 1   ) THEN
         WRITE(UF_MSG,2392) CURIYR,(TOTREVNW(CURIYR,IREG),IREG=1,MNUMNR - 2)
 2392    FORMAT(1X,"WREV",1(":",I2),13(":",F10.3))
!        Label:WREV:CYR:REV1:REV2:REV3:REV4:REV5:REV6:REV7:REV8:REV9:REV10:REV11:REV12:REV13:REV14
         DO IREG = 1 , NDREG
            DO IECP = 1 , ECP_D_DSP
               IF (UPTTYP(IECP) .LE. NW_COAL) THEN
                  ITYP = UPTTYP(IECP)
                  IF (UEFD_CAP(IECP,IREG,CURIYR) .GT. 0.0 .OR. UEFD_GEN(IECP,IREG,CURIYR) .GT. 0.0) &
                     WRITE(18,2393) CURIYR+UHBSYR,CURITR,IREG,IECP,UEFD_CAP(IECP,IREG,CURIYR),UEFD_GEN(IECP,IREG,CURIYR)
 2393             FORMAT(1X,"UEFD_CL",4(":",I4),2(":",F12.3))
!                    Label:UEFD_CL:CYEAR:CURITR:IREG:IECP:UEFD_CAP:UEFD_GEN
                  DO KYR = 1 , UNXPH
                     IF (UECP_CAP(KYR,ITYP,IREG) .GT. 0.0 .OR. UECP_GEN(KYR,ITYP,IREG) .GT. 0.0) &
                        WRITE(18,2394) CURIYR+UHBSYR,CURIYR+UHBSYR+KYR-1,CURITR,IREG,IECP,UECP_CAP(KYR,ITYP,IREG),UECP_GEN(KYR,ITYP,IREG)
 2394                FORMAT(1X,"UECP_CL",5(":",I4),2(":",F12.3))
!                       Label:UECP_CL:CYEAR:PYEAR:CURITR:IREG:IECP:UECP_CAP:UECP_GEN
                  END DO
               END IF
            END DO
            DO ICNFG = 1 , MX_CNFG
               IECP = UCL_ECP(ICNFG)
               IF (CNFG_CAP(ICNFG,IREG) .GT. 0.0 .OR. CNFG_GEN(ICNFG,IREG) .GT. 0.0) &
                  WRITE(18,2395) CURIYR+UHBSYR,CURITR,IREG,IECP,ICNFG,CNFG_CAP(ICNFG,IREG),CNFG_GEN(ICNFG,IREG)
 2395          FORMAT(1X,"CNFG_CL",5(":",I4),2(":",F12.3))
!                 Label:CNFG_CL:CYEAR:CURITR:IREG:IECP:ICNFG:CNFG_CAP:CNFG_GEN
            END DO
         END DO
      END IF
!
!
!     STORE ELECTRICITY PRICES
!
!     IF LAST YEAR AND LAST ITERATION THEN CLEAR PLNTDAF BUFFER AND
!     SAVE PLANT FILE CONTROL INFORMATION AND PRINT END OF RUN PLANT
!     FILE TO EMMDBASE
!
      IF (FCRL .EQ. 1 .AND. (CURIYR .EQ. LASTYR .OR. CURIYR .EQ. 21)) THEN
         CALL STRPLT(0)
         CALL STRPCNTL
         IF  (CURIYR .EQ. LASTYR) THEN
           IF ( (USW_DBS .GT. 1) .OR. ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) ) CALL WRPLNT
         ENDIF
      ENDIF
!
!     IF FIRST OR LAST YEAR AND LAST ITERATION THEN CALL WRGRPDB TO
!     WRITE PLANT GROUP INFORMATION TO EMMDBASE.
!
      IF (FCRL .EQ. 1) THEN
!        IF (((USW_DBS .GT. 0) .OR. ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1))) .AND.  &
!             (USW_DBGRP(CURIYR) .EQ. 1)) THEN
         IF ( (ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1) .AND. (CURIYR .GE. 6) ) THEN
            CALL WRGRPDB
         ENDIF
!        write historical overwrite and alaska/hawaii information to database
         IF ( (ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1) ) THEN
            CALL WRHISDB
         ENDIF
      ENDIF
!
      RETURN
      END
!
!
      SUBROUTINE WEMMDB
!
      IMPLICIT NONE

!     THIS SUBROUTINE READS AND WRITES THE EMM DATABASE DEFINITION TABLE

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'ecpcntl'
      include 'eusprc'
      include 'edbdef'
      include 'control'
      include 'wrenew'
      include 'uefdout'
!
      LOGICAL NEW
!
!     USING RD$* FUNCTIONS
!
      INTEGER*4    RD$TBL,RD$I1,RD$R1,RD$R81,RD$C1,RET_CODE
      INTEGER*4    RD$I2,RD$R2,RD$R82,RD$C2,RD$I3,RD$R3
      INTEGER*4    COLLB,NCOL,CPT,L_COLLB
      INTEGER*4    DMSZ
      PARAMETER (DMSZ = 100)
      CHARACTER*40 DUMMY(DMSZ)             ! DUMMY COLUMN IN DATA TABLES
      CHARACTER*2 COL
      CHARACTER*40 FILENM
      INTEGER*4 CHK$RSUM,CHK$PRF
      INTEGER*4 I,ILD,IFL,ICAP,IOWN,IYR,IRG,ISTP
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
!
      COL = ' :'                                                       !//EMMDB//
!
!     WRITE OUT HEADER LINE FOR EMM DATABASE                      !//EMMDB//
!                                                                 !//EMMDB//
      IF (USW_DBS .GT. 0) THEN                                     !//EMMDB//
         COL = ' :'                                               !//EMMDB//
         WRITE(UF_DBS,*)                                           & !//EMMDB//
            'VER :'// TRIM(SCEN_DATE)   !//EMMDB//
!                                                                 !//EMMDB//
!        WRITE OUT YEAR TABLE (CY) FOR EMM DATABASE               !//EMMDB//
!                                                                 !//EMMDB//
         DO IYR = 1 , UNYEAR                                      !//EMMDB//
            WRITE(UF_DBS,5400) COL,IYR,COL,USYEAR(IYR),COL,TRIM(SCEN_DATE)  !//EMMDB//
 5400       FORMAT(1X,'CY',A2,I2,A2,I4,A2,A)                      !//EMMDB//
         END DO                                                   !//EMMDB//
!                                                                 !//EMMDB//
!        WRITE OUT DOLLAR YEAR TABLE (DY) FOR EMM DATABASE        !//EMMDB//
!                                                                 !//EMMDB//
         DO IYR = 1 , UNYEAR                                      !//EMMDB//
            WRITE(UF_DBS,5450) COL,IYR,COL,USYEAR(IYR),COL,TRIM(SCEN_DATE)  !//EMMDB//
 5450       FORMAT(1X,'DY',A2,I2,A2,I4,A2,A)                      !//EMMDB//
         END DO                                                   !//EMMDB//
!CC                                                               !//EMMDB//
!CC        WRITE OUT ECP SUPPLY STEP TABLE                        !//EMMDB//
!CC                                                               !//EMMDB//
         DO ISTP = 1 , MSPTMX                                     !//EMMDB//
             WRITE(UF_DBS,5465) COL,ISTP,COL,ISTP,COL,TRIM(SCEN_DATE)       !//EMMDB//
         ENDDO
 5465      FORMAT(1X,'SSTEP',A2,I2,A2,                             & !//EMMDB//
                        'ECP Supply Step #',I2,A2,A)              !//EMMDB//
!                                                                 !//EMMDB//
!        WRITE OUT REGION NAME TABLE (RG) FOR EMM DATABASE        !//EMMDB//
!                                                                 !//EMMDB//
         DO IRG = 1 , UNRGNS                                      !//EMMDB//
            WRITE(UF_DBS,5100) COL,IRG,COL,URGNME(IRG),COL,TRIM(SCEN_DATE)  !//EMMDB//
 5100       FORMAT(1X,'RG',A2,I2,A2,A10,A2,A)                     !//EMMDB//
         ENDDO                                                    !//EMMDB//

         WRITE(UF_DBS,5105) COL,COL,COL,TRIM(SCEN_DATE)           !//EMMDB//
         WRITE(UF_DBS,5110) COL,COL,COL,TRIM(SCEN_DATE)           !//EMMDB//
         WRITE(UF_DBS,5115) COL,COL,COL,TRIM(SCEN_DATE)           !//EMMDB//
 5105    FORMAT(1x,'RG',A2,'26',A2,'Alaska    ',A2,A)             !//EMMDB//
 5110    FORMAT(1x,'RG',A2,'27',A2,'Hawaii    ',A2,A)             !//EMMDB//
 5115    FORMAT(1x,'RG',A2,'28',A2,'Total U.S.',A2,A)             !//EMMDB//
!                                                                 !//EMMDB//
!        WRITE OUT EXPORT REGION NAME TABLE (EXPRG) FOR EMM DATABASE  !//EMMDB//
!                                                                 !//EMMDB//
         DO IRG = 1 , UNRGNS                                      !//EMMDB//
            WRITE(UF_DBS,5150) COL,IRG,COL,URGNME(IRG),COL,TRIM(SCEN_DATE)  !//EMMDB//
 5150       FORMAT(1X,'EXPRG',A2,I2,A2,A10,A2,A)                  !//EMMDB//
         ENDDO                                                    !//EMMDB//
         WRITE(UF_DBS,5155) COL,COL,COL,TRIM(SCEN_DATE)           !//EMMDB//
         WRITE(UF_DBS,5160) COL,COL,COL,TRIM(SCEN_DATE)           !//EMMDB//
         WRITE(UF_DBS,5165) COL,COL,COL,TRIM(SCEN_DATE)           !//EMMDB//
 5155    FORMAT(1x,'EXPRG',A2,'26',A2,'Alaska    ',A2,A)          !//EMMDB//
 5160    FORMAT(1x,'EXPRG',A2,'27',A2,'Hawaii    ',A2,A)          !//EMMDB//
 5165    FORMAT(1x,'EXPRG',A2,'28',A2,'Total U.S.',A2,A)          !//EMMDB//
!                                                                 !//EMMDB//
!        WRITE OUT PLANT LOCATION REGION NAME TABLE (DPLTRG) FOR EMM DATABASE  !//EMMDB//
!                                                                 !//EMMDB//
         DO IRG = 1 , UNRGNS                                      !//EMMDB//
            WRITE(UF_DBS,6150) COL,IRG,COL,URGNME(IRG),COL,TRIM(SCEN_DATE)  !//EMMDB//
 6150       FORMAT(1X,'DPLTRG',A2,I2,A2,A10,A2,A)                  !//EMMDB//
         ENDDO                                                    !//EMMDB//
         WRITE(UF_DBS,6155) COL,COL,COL,TRIM(SCEN_DATE)           !//EMMDB//
         WRITE(UF_DBS,6160) COL,COL,COL,TRIM(SCEN_DATE)           !//EMMDB//
         WRITE(UF_DBS,6165) COL,COL,COL,TRIM(SCEN_DATE)           !//EMMDB//
 6155    FORMAT(1x,'DPLTRG',A2,'26',A2,'Alaska    ',A2,A)          !//EMMDB//
 6160    FORMAT(1x,'DPLTRG',A2,'27',A2,'Hawaii    ',A2,A)          !//EMMDB//
 6165    FORMAT(1x,'DPLTRG',A2,'28',A2,'Total U.S.',A2,A)          !//EMMDB//
      END IF                                                      !//EMMDB//
!
!     WRITE OUT EFD CAPACITY TYPE TABLE (DCAP) FOR EMM DATABASE   !//EMMDB//
!                                                                 !//EMMDB//
      IF (USW_DBS .GT. 0) THEN                                     !//EMMDB//
         DO ICAP = 1 , EFD_D_CAP                                    !//EMMDB//
            WRITE(UF_DBS,5200) COL,ICAP,COL,EPPLCD(ICAP),COL,      & !//EMMDB//
               EFDNAME(ICAP),COL,TRIM(SCEN_DATE)                  !//EMMDB//
 5200       FORMAT(1X,'DCAP',A2,I2,A2,A3,A2,A55,A2,A)             !//EMMDB//
         END DO                                                   !//EMMDB//
      END IF                                                      !//EMMDB//
!
!     WRITE OUT EFD FUEL TYPE TABLE (DFL) FOR EMM DATABASE                  !//EMMDB//
!                                                                           !//EMMDB//
      IF (USW_DBS .GT. 0) THEN                                               !//EMMDB//
         DO IFL = 1 , UNFUELS                                               !//EMMDB//
            WRITE(UF_DBS,5300) COL,IFL,COL,UFLCODE(IFL),COL,                 & !//EMMDB//
               UNMFL(IFL),COL,TRIM(SCEN_DATE)                               !//EMMDB//
 5300       FORMAT(1X,'DFL',A2,I2,A2,A2,A2,A30,A2,A)                        !//EMMDB//
         END DO                                                             !//EMMDB//
      END IF                                                                !//EMMDB//
!CC
!CC  WRITE OUT ECP CAPACITY TYPE TABLE (CCAP) FOR EMM DATABASE    !//EMMDB//
!CC                                                               !//EMMDB//
      IF (USW_DBS .GT. 0) THEN                                     !//EMMDB//
         DO ICAP = 1 , ECP_D_CAP                                    !//EMMDB//
            WRITE(UF_DBS,5500) COL,ICAP,COL,UPLNTCD(ICAP),         & !//EMMDB//
               COL,UPLNAME(ICAP),COL,PTYPE(ICAP),COL,TRIM(SCEN_DATE)  !//EMMDB//
 5500       FORMAT(1X,'CCAP',A2,I2,A2,A2,A2,A31,A2,A1,A2,A)       !//EMMDB//
         END DO                                                   !//EMMDB//
      END IF                                                      !//EMMDB//
!CC
!CC  WRITE OUT ECP PLANT TYPE TO EFD PLANT TYPE MAPPING TABLE FOR EMMDBASE
!CC                                                               !//EMMDB//
      IF (USW_DBS .GT. 0) THEN                                     !//EMMDB//
         DO ICAP = 1 , ECP_D_CAP                                    !//EMMDB//
            WRITE(UF_DBS,5510) COL,ICAP,COL,UPEFDT(ICAP),         & !//EMMDB//
               COL,TRIM(SCEN_DATE)                                !//EMMDB//
 5510       FORMAT(1X,'DECPEFDPT',A2,I2,A2,I2,A2,A)                !//EMMDB//
         END DO                                                   !//EMMDB//
      END IF                                                      !//EMMDB//
!
!     WRITE OUT ECP FUEL TYPE TABLE (CFL) FOR EMM DATABASE        !//EMMDB//
!                                                                 !//EMMDB//
      IF (USW_DBS .GT. 0) THEN                                     !//EMMDB//
         DO IFL = 1 , 13                                          !//EMMDB//
            WRITE(UF_DBS,5600) COL,IFL,COL,UPFLCD(IFL),            & !//EMMDB//
               COL,FLNAME(IFL),COL,TRIM(SCEN_DATE)                !//EMMDB//
 5600       FORMAT(1X,'CFL',A2,I2,A2,A2,A2,A30,A2,A)              !//EMMDB//
         END DO                                                   !//EMMDB//
      END IF                                                      !//EMMDB//
!
!     WRITE OUT ECP OWNER TYPE TABLE (COWN) FOR EMM DATABASE      !//EMMDB//
!                                                                 !//EMMDB//
      IF (USW_DBS .GT. 0) THEN                                     !//EMMDB//
         DO IOWN = 1 , ECP_D_OWN                                    !//EMMDB//
            WRITE(UF_DBS,5700) COL,IOWN,COL,UPOWNCD(IOWN),COL,     & !//EMMDB//
               OWNNAME(IOWN),COL,TRIM(SCEN_DATE)                  !//EMMDB//
 5700       FORMAT(1X,'COWN',A2,I1,A2,A1,A2,A20,A2,A)             !//EMMDB//
         END DO                                                   !//EMMDB//
      END IF                                                      !//EMMDB//
!
         WRITE (6,*)'*** BEGINNING READ FOR EMMDB DEFINITION TABLES ***'
         WRITE (UF_MSG,*) &
               '*** BEGINNING READ FOR EMMDB DEFINITION TABLES ***'
!
!        OPEN FILE
!
         FILENM = 'EMMDBDEF'
         NEW = .FALSE.
         UF_TMP = FILE_MGR('O',FILENM,NEW)
!
!        READ ECP PLANNING YEAR DEFINITION TABLE
!
         RET_CODE = RD$TBL(UF_TMP,'%ECP PYEAR  %',ECP_D_XPH,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$C1(ECPPYD,1,ECP_D_XPH)                     ! ECP PLANNING YR DEF
!
!        WRITE ECP PLANNING YEAR DEFINITION TABLE
!
         DO I = 1 , UNXPH
           WRITE(UF_DBS,300) COL,I,COL,ECPPYD(I),COL,TRIM(SCEN_DATE)  !//EMMDB//
  300 FORMAT(1X,'PY',A2,I1,A2,A40,A2,A)
         ENDDO
!
!        READ EFD OWNER DEFINITION TABLE
!
         RET_CODE = RD$TBL(UF_TMP,'%EFD DOWN   %',EFD_D_OWN,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$C1(EFDOWND,1,EFD_D_OWN)                     ! EFD OWNER DEF
         RET_CODE = RD$I1(EOWNFTAB,1,EFD_D_OWN)                     ! EFD OWNER MAP TO FTAB OWNER TYPE
!
!        WRITE EFD OWNER DEFINITION TABLE
!
         DO I = 1 , EFD_D_OWN
           WRITE(UF_DBS,500) COL,I,COL,EFDOWND(I),COL,EOWNFTAB(I),COL,TRIM(SCEN_DATE)
  500 FORMAT(1X,'DOWN',A2,I1,A2,A25,A2,I5,A2,A)
         ENDDO
!
!        READ EFD FINANCIAL OWNER DEFINITION TABLE
!
         RET_CODE = RD$TBL(UF_TMP,'%EFD DWFOWN %',EFD_D_OWN,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$C1(EFDPOWND,1,EFD_D_OWN)                     ! EFD FINANCIAL OWNER DEF
!
!        WRITE EFD FINANCIAL OWNER DEFINITION TABLE
!
         DO I = 1 , EFD_D_OWN
           WRITE(UF_DBS,600) COL,I,COL,EFDPOWND(I),COL,TRIM(SCEN_DATE)
  600 FORMAT(1X,'DWFOWN',A2,I1,A2,A25,A2,A)
         ENDDO
!
!        READ EFD PLANT FUEL USE DEFINITION TABLE
!
         RET_CODE = RD$TBL(UF_TMP,'%EFD DCAPFL %',EFD_D_FPP,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$C1(EFDFUD,1,EFD_D_FPP)                     ! EFD PLANT FUEL USE DEF
!
!        WRITE EFD PLANT FUEL USE DEFINITION TABLE
!
         DO I = 1 , EFD_D_FPP
           WRITE(UF_DBS,700) COL,I,COL,EFDFUD(I),COL,TRIM(SCEN_DATE)
  700 FORMAT(1X,'DCAPFL',A2,I1,A2,A25,A2,A)
         ENDDO
!
!        READ EFD PLANT FILE VINTAGE DEFINITION TABLE
!
         RET_CODE = RD$TBL(UF_TMP,'%EFD DWFVIN %',12,UF_DBG,UF_MSG)
         RET_CODE = RD$I1(EFDPVINI,1,12)                     ! EFD PLANT VINTAGE INDEX
         RET_CODE = RD$C1(EFDPVIND,1,12)                     ! EFD PLANT VINTAGE DEF
!
!        WRITE EFD PLANT FILE VINTAGE DEFINITION TABLE
!
         DO I = 1 , 12
           WRITE(UF_DBS,1100) COL,EFDPVINI(I),COL,EFDPVIND(I),COL,TRIM(SCEN_DATE)
 1100 FORMAT(1X,'DWFVIN',A2,I2,A2,A25,A2,A)
         ENDDO
!
!        READ EFD VINTAGE DEFINITION TABLE
!
         RET_CODE = RD$TBL(UF_TMP,'%EFD DVIN   %',EFD_D_VIN,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$C1(EFDVIND,1,EFD_D_VIN)                     ! EFD VINTAGE DEF
!
!        WRITE EFD VINTAGE DEFINITION TABLE
!
         DO I = 1 , EFD_D_VIN
           WRITE(UF_DBS,1200) COL,I,COL,EFDVIND(I),COL,TRIM(SCEN_DATE)
 1200 FORMAT(1X,'DVIN',A2,I1,A2,A25,A2,A)
         ENDDO
!
!        READ NEMS ITERATION DEFINITION TABLE
!
         RET_CODE = RD$TBL(UF_TMP,'%NEMS ITER  %',10,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$C1(ENITRD,1,10)                     ! NEMS ITER DEF
!
!        WRITE NEMS ITERATION DEFINITION TABLE
!
         DO I = 1 , 10
           WRITE(UF_DBS,1300) COL,I,COL,ENITRD(I),COL,TRIM(SCEN_DATE)
 1300 FORMAT(1X,'NITR',A2,I2,A2,A25,A2,A)
         ENDDO
!
!        READ EFD ITERATION DEFINITION TABLE
!
         RET_CODE = RD$TBL(UF_TMP,'%EFD ITER   %',10,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$C1(EFDITRD,1,10)                     ! EFD ITER DEF
!
!        WRITE EFD ITERATION DEFINITION TABLE
!
         DO I = 1 , 10
           WRITE(UF_DBS,1400) COL,I,COL,EFDITRD(I),COL,TRIM(SCEN_DATE)
 1400 FORMAT(1X,'DITR',A2,I2,A2,A25,A2,A)
         ENDDO
!
!        READ EFD TO EFP PLANT TYPE MAPPING TABLE
!
         RET_CODE = RD$TBL(UF_TMP,'%EFD EFP PT %',EFD_D_CAP,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$I1(EFDEFPT,1,EFD_D_CAP)                     !EFP PLANT TYPE DEF
!
!        WRITE EFD TO EFP PLANT TYPE MAPPING TABLE
!
         DO I = 1 , EFD_D_CAP
           WRITE(UF_DBS,1420) COL,I,COL,EFDEFPT(I),COL,TRIM(SCEN_DATE)
 1420 FORMAT(1X,'EFDEFPT',A2,I2,A2,I2,A2,A)
         ENDDO
!
!        READ CENSUS REGION DEFINITION TABLE
!
         RET_CODE = RD$TBL(UF_TMP,'%CENSUS REG %',MNUMCR,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$C1(ECREGD,1,MNUMCR)                     ! CENSUS REGION DEF
!
!        WRITE CENSUS REGION DEFINITION TABLE
!
         DO I = 1 , MNUMCR
           WRITE(UF_DBS,1500) COL,I,COL,ECREGD(I),COL,TRIM(SCEN_DATE)
 1500 FORMAT(1X,'CENRG',A2,I2,A2,A25,A2,A)
         ENDDO
!
!        READ SO2 COMPLIANCE GROUP DEFINITION TABLE
!
         RET_CODE = RD$TBL(UF_TMP,'%SO2 CMPGRP %',EFD_D_SO2,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$C1(ECPSO2D,1,EFD_D_SO2)                     !SO2 COMP GROUP DEF
!
!        WRITE SO2 COMPLIANCE GROUP DEFINITION TABLE
!
         DO I = 1 , EFD_D_SO2
           WRITE(UF_DBS,1600) COL,I,COL,ECPSO2D(I),COL,TRIM(SCEN_DATE)
 1600 FORMAT(1X,'DSOCGI',A2,I1,A2,A40,A2,A)
         ENDDO
!
!        READ EFP END USE DEFINITION TABLE
!
         RET_CODE = RD$TBL(UF_TMP,'%EFP EUSE   %',MNEUGRP,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$C1(EFPEUSD,1,MNEUGRP)                     !EFP END USE DEF
!
!        WRITE EFP END USE DEFINITION TABLE
!
         DO I = 1 , MNEUGRP
           WRITE(UF_DBS,1700) COL,I,COL,EFPEUSD(I),COL,TRIM(SCEN_DATE)
 1700 FORMAT(1X,'FEUSE',A2,I2,A2,A40,A2,A)
         ENDDO
!
!        READ EFP DEMAND SECTOR DEFINITION TABLE
!
         RET_CODE = RD$TBL(UF_TMP,'%EFP DSECT  %',5,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$C1(EFPDSTD,1,5)                     !EFP DEMAND SECTOR DEF
!
!        WRITE EFP DEMAND SECTOR DEFINITION TABLE
!
         DO I = 1 , 5
           WRITE(UF_DBS,1800) COL,I,COL,EFPDSTD(I),COL,TRIM(SCEN_DATE)
 1800 FORMAT(1X,'FDSECT',A2,I1,A2,A25,A2,A)
         ENDDO
!
!        READ EFP STAGE OF PRODUCTION DEFINITION TABLE
!
         RET_CODE = RD$TBL(UF_TMP,'%EFP SPROD  %',4,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$C1(EFPSPRD,1,4)                     !EFP STAGE OF PRODUCTION DEF
!
!        WRITE EFP STAGE OF PRODUCTION DEFINITION TABLE
!
         DO I = 1 , 4
           WRITE(UF_DBS,1900) COL,I,COL,EFPSPRD(I),COL,TRIM(SCEN_DATE)
 1900 FORMAT(1X,'FSPROD',A2,I1,A2,A25,A2,A)
         ENDDO
!
!        READ EFP OWNER DEFINITION TABLE
!
         RET_CODE=RD$TBL(UF_TMP,'%EFP OWNER  %',EFP_D_OWN+1,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$C1(EFPOWND,1,EFP_D_OWN+1)                     !EFP OWNER DEF
!
!        WRITE EFP OWNER DEFINITION TABLE
!
         DO I = 1 , EFP_D_OWN + 1
           WRITE(UF_DBS,2000) COL,I,COL,EFPOWND(I),COL,TRIM(SCEN_DATE)
 2000 FORMAT(1X,'FOWN',A2,I1,A2,A25,A2,A)
         ENDDO
!
!        READ EFP PLANT TYPE DEFINITION TABLE
!
         RET_CODE = RD$TBL(UF_TMP,'%EFP PTYPE  %',EFP_D_CAP,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$C1(EFPPTYD,1,EFP_D_CAP)                     !EFP PLANT TYPE DEF
!
!        WRITE EFP PLANT TYPE DEFINITION TABLE
!
         DO I = 1 , EFP_D_CAP
           WRITE(UF_DBS,2100) COL,I,COL,EFPPTYD(I),COL,TRIM(SCEN_DATE)
 2100 FORMAT(1X,'FCAP',A2,I2,A2,A40,A2,A)
         ENDDO
!
!        READ EMM FTAB CAPACITY INDEX TABLE
!
         RET_CODE = RD$TBL(UF_TMP,'%EMM FTABCAP%',9,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$C1(ECAPFTABD,1,9)                     !EFP PLANT TYPE DEF
!
!        WRITE EMM FTAB CAPACITY INDEX TABLE
!
         DO I = 1 , 9
           WRITE(UF_DBS,2200) COL,I,COL,ECAPFTABD(I),COL,TRIM(SCEN_DATE)
 2200 FORMAT(1X,'ECAPFTABD',A2,I2,A2,A40,A2,A)
         ENDDO
!
!        READ EMM FTAB FUEL INDEX TABLE
!
         RET_CODE = RD$TBL(UF_TMP,'%EMM FTABFUL%',7,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$C1(FUELFTABD,1,7)                     !EFP PLANT TYPE DEF
!
!        WRITE EMM FTAB FUEL INDEX TABLE
!
         DO I = 1 , 7
           WRITE(UF_DBS,2300) COL,I,COL,FUELFTABD(I),COL,TRIM(SCEN_DATE)
 2300 FORMAT(1X,'FUELFTABD',A2,I2,A2,A40,A2,A)
         ENDDO
!
!        READ EMM FTAB OWN INDEX TABLE
!
         RET_CODE = RD$TBL(UF_TMP,'%EMM FTABOWN%',3,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$C1(FTABOWND,1,3)                     !FTAB OWNER TYPE DESCRIPTION
         RET_CODE = RD$C1(FTABOWNS,1,3)                     !FTAB OWNER TYPE SHORT NAME
!
!        WRITE EMM FTAB OWN INDEX TABLE
!
         DO I = 1 , 3
           WRITE(UF_DBS,2400) COL,I,COL,FTABOWND(I),COL,FTABOWNS(I),COL,TRIM(SCEN_DATE)
 2400 FORMAT(1X,'FUELFOWND',A2,I2,A2,A40,A2,A25,A2,A)
         ENDDO
!
!        READ IEA CAPACITY CATEGORY TABLE
!
         RET_CODE = RD$TBL(UF_TMP,'%IEA CAPCAT %',8,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$C1(IEACATD,1,8)                     !IEA CAPACITY CATEGORY DESCRIPTION
!
!        WRITE IEA CAPACITY CATEGORY TABLE
!
         DO I = 1 , 8
           WRITE(UF_DBS,2500) COL,I,COL,IEACATD(I),COL,TRIM(SCEN_DATE)
 2500 FORMAT(1X,'IEACAPCAT',A2,I2,A2,A25,A2,A)
         ENDDO
!
!        READ EFD MAPPING TO IEA CAPACITY CATEGORY TABLE
!
         RET_CODE = RD$TBL(UF_TMP,'%IEADCAPMAP %',EFD_D_CAP,UF_DBG,UF_MSG)
         RET_CODE = RD$C1(DUMMY,1,DMSZ)
         RET_CODE = RD$I1(EFDIEAMAP,1,EFD_D_CAP)                     !IEA CAPACITY CATEGORY DESCRIPTION
!
!        WRITE IEA CAPACITY CATEGORY TABLE
!
         DO I = 1 , EFD_D_CAP
           WRITE(UF_DBS,2600) COL,I,COL,EFDIEAMAP(I),COL,TRIM(SCEN_DATE)
 2600 FORMAT(1X,'EFDIEAMAP',A2,I2,A2,I5,A2,A)
         ENDDO
!
!
!
!
!
      CLOSE (UF_TMP)
!
      RETURN
      END
!
!
      SUBROUTINE DEBUG(IYR,IRG)
!
      IMPLICIT NONE
!
!     THIS SUBROUTINE WRITES OUTPUT FOR KEY EFD VARIABLES TO EMMRPT.txt

      include 'parametr'
      include 'emmparm'
      include 'control'
      include 'dispin'
      include 'dispout'
      include 'ncntrl'
      include 'emission'
!
      INTEGER IYR,IRG,ISP,I,J,IOWN,K,IVIN
      REAL*8 CF,DUMMY,QPGN,QFGN,QHGN,SCAP,QFFL,DMY(EFD_D_OWN)
      CHARACTER*1 PAGE
      CHARACTER*24 TOTAL
      CHARACTER*40 TITLE(4)

!
      DUMMY = 0.0
      TOTAL='Totals'
      PAGE=''
!
      DO IOWN = 1 , EFD_D_OWN
         DMY(IOWN) = 0.0
      END DO
!
      IF (IYR .EQ. 1 .AND. IRG .EQ. 1) THEN
         WRITE(UF_RPT,5000) (IOWN,IOWN,IOWN,IOWN,IOWN=1,USW_OWN)
 5000    FORMAT(1X,"RGNM:RGN:YEAR:IT:PLANT_TYPE",22X,":TYP", &
            ":CF   ", ":CAP      ", &
            ":SCAP     ", ":FL_BTU   ",  &
            ":SO2      ", 4(":GEN_",I1,"    ",  &
            ":FOM_",I1,"    ", ":VOM_",I1,"    ", &
            ":FL_CST_",I1," "), ":SCEN")
      END IF
!
      DO J = 1 , EIPGRP
         QPGN = 0.0
         SCAP = 0.0
         DO IOWN = 1 , USW_OWN
            QPGN = QPGN + EQPGN(J,IOWN)
            DO IVIN = 1 , EFD_D_VIN
               SCAP = SCAP + ECSCAP(J,IVIN,IOWN)
            END DO  ! IVIN
         END DO  ! IOWN
         CF = 0.0
         IF(EQPCP(J) .GT. 0.0) THEN
            CF = QPGN / (EQPCP(J) * 0.001 * 8760)
         END IF
!
         WRITE(UF_RPT,2000) URGNME(IRG),IRG,USYEAR(IYR),CURITR, &
            ENPGRP(J),J,CF,EQPCP(J),SCAP,EQPFL(J)*0.001, &
            EQPSO2(J)*0.001,(EQPGN(J,IOWN)*0.001,ERTOMF(J,IOWN), &
            ERPOM(J,IOWN),ERPFL(J,IOWN),IOWN=1,USW_OWN),TRIM(SCEN_DATE)
 2000    FORMAT(1X,A4,":",I3,":",I4,":",I2,":",A32,":",I3, &
            ":",F5.3,2(":",F9.3),18(":",F9.3),":",A)
      END DO  ! J
!
      DO J = 1 , EIHGRP
         K = J + EIPGRP
         QHGN = 0.0
         SCAP = 0.0
         DO IOWN = 1 , USW_OWN
            QHGN = QHGN + EQHGN(J,IOWN)
            DO IVIN = 1 , EFD_D_VIN
               SCAP = SCAP + EHSCAP(J,IVIN,IOWN)
            END DO  ! IVIN
         END DO  ! IOWN
         CF = 0.0
         IF(EQHCP(J) .GT. 0.0) CF = QHGN / &
             (EQHCP(J) * 0.001 * 8760)
         WRITE(UF_RPT,2000) URGNME(IRG),IRG,USYEAR(IYR),CURITR, &
            ENHGRP(J),K,CF,EQHCP(J),SCAP,DUMMY,DUMMY, &
            (EQHGN(J,IOWN)*0.001,ERTOMF(K,IOWN),ERHOM(J,IOWN), &
            DMY(IOWN),IOWN=1,USW_OWN),TRIM(SCEN_DATE)
      END DO  ! J
!
!     SM ONLY WRITE OUT @.EMMRP2 WHEN UF_RP2 HAS BEEN SET
!
      IF (UF_RP2 .GT. 0) THEN
         WRITE(UF_RP2, * ) PAGE
         WRITE(UF_RP2,5001) UF_SCEN,URGNME(IRG),USYEAR(IYR)
5001     FORMAT(1X,'Scenario Name: ',A4,5X,'Region: ',A20, &
          5X,'Year: ',I4)
         WRITE(UF_RP2, * )
         TITLE(1)='                                        '
         TITLE(2)='       Fuel Cost   Fuel Used  Fuel Price'
         TITLE(3)='  Generation SO2 Emissions  MMBtu per   '
         TITLE(4)=' Lbs SO2                                '
         WRITE(UF_RP2,2002) (TITLE(I),I = 1,4)
 2002    FORMAT(4A40)
         TITLE(1)='  YR  RG  FT  Fuel Name                 '
         TITLE(2)='     (Millions $)   (MMMBtu)   ($/MMBtu)'
         TITLE(3)='    (MM KWH)     (Tons)    Ton,Bbl,Mcf  '
         TITLE(4)=' / MMBtu                                '
         WRITE(UF_RP2,2002) (TITLE(I),I = 1,4)
!
         DO 40 J = 1 , ENFLTP
            QFGN = 0.0
            QFFL = 0.0
            DO 41 IOWN = 1 , USW_OWN - 1    !don't include cogen in sum
               QFGN = QFGN + EQFGN(J,IOWN)
               QFFL = QFFL + EQFFL(J,IOWN)
   41       CONTINUE
            WRITE(UF_RP2,3000) IYR,IRG,J,ENMFL(J),ERFFL(J),QFFL,EPFUEL(J),QFGN,EQFSO2(J),EFHCNT(J),EFRSO2(J)
 3000       FORMAT(3I4,2X,A32,2F12.3,F12.4,2F12.1,2F12.4)
   40    CONTINUE
!
         DO 45 J = 1 , USW_OWN - 1    !don't include cogen in sum
            WRITE(UF_RP2,3045) ERTFL(J)
   45    CONTINUE
 3045    FORMAT(46X,F12.3)
!
         TITLE(1)='                                        '
         TITLE(2)='                                        '
         TITLE(3)='                                        '
         WRITE(UF_RP2,2002) (TITLE(I),I = 1,3)
         TITLE(1)='              Allowances      Price  SO2'
         TITLE(2)=' Emissions                              '
         TITLE(3)='                                        '
         WRITE(UF_RP2,2002) (TITLE(I),I = 1,3)
         TITLE(1)='  YR  RG  CG    (Tons)       ($/Ton)    '
         TITLE(2)=' (Tons)                                 '
         TITLE(3)='                                        '
         WRITE(UF_RP2,2002) (TITLE(I),I = 1,3)
         DO 50 J = 1 , NUM_SO2_GRP
            WRITE(UF_RP2,4000) IYR,IRG,J,EGALLW(J),EMELPSO2(CURIYR,J),EGSO2(J)
 4000       FORMAT(3I4,F12.1,F12.4,F12.1)
   50    CONTINUE
      END IF
      RETURN
      END
!
      SUBROUTINE EMMDSPO
!
      IMPLICIT NONE
!
!     THIS SUBROUTINE STORES EMM DISPATCHED GENERATION OUTPUTS IN NEMS GLOBAL ARRAYS

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'cdsparms'
      include 'control'
      include 'plntctl'
      include 'dispin'
      include 'dispuse'
      include 'emmcnv'
      include 'emission'
      include 'fuelin'
      include 'pq'
      include 'xpq'
      include 'angtdm'
      include 'ecpcntl'
      include 'ecp_coal'
      include 'uefpout'     ! UEFP output variables
      include 'uefdout'     ! EFD output variables
      include 'udatout'     ! UDAT output variables
      include 'uecpout'     ! UECP output variables
      include 'uldsmout'    ! LDSM output variables
      include 'uettout'     ! UETT output variables
      include 'dispinyr'
      include 'cogen'
      include 'wrenew'
      include 'eusprc'
      include 'efpint'
      include 'efpout'
      include 'coalemm'
      include 'coalrep'
      include 'acoalprc'
      include 'dsmdimen'
      include 'dsmsectr'
      include 'dsmtoefd'
      include 'bildout'
      include 'elout'
      include 'elcntl'
      include 'uso2grp'
      include 'coalout'
      include 'bildin'
      include 'ab32'
      include 'rggi'
      include 'csapr'
      include 'e111d'
      include 'edbdef'
      include 'emmemis'
      include 'ecp_nuc'
      include 'emm_aimms'
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
!
      COMMON /ECPPROUT/ECPCO2FY,ECPCO2FL,ECPCO2NY,ECPCO2NL
      REAL*4 ECPCO2FY(EFD_D_MFRG + 1,MNUMYR)
      REAL*4 ECPCO2FL(EFD_D_MFRG + 1,MNUMYR)
      REAL*4 ECPCO2NY(MNUMNR,MNUMYR)
      REAL*4 ECPCO2NL(MNUMNR,MNUMYR)
!
!     COMMON/ERCOUT/ERCQTYDN(MNUMNR,MNUMNR,MNUMYR),ERCPRCDN(MNUMNR,MNUMYR),  &
!                   ERCQTYPN(MNUMNR,MNUMNR,MNUMYR),ERCPRCPN(MNUMNR,MNUMYR)
!     REAL*4 ERCQTYDN
!     REAL*4 ERCPRCDN
!     REAL*4 ERCQTYPN
!     REAL*4 ERCPRCPN
!
      INTEGER IRG,REG,ICENSUS,ICR,IFL,IYR,IOWNER,IPOL,RYR,IGRD,IGR,ISP,IECP,ISO2,IT
      INTEGER IGRP,IPLT,IRNK,ISTEP,ALASYR,AB_RSVYR
      INTEGER NWGRP,NERC,IEFD,IOWN,BBN,ALASKA,HAWAII,IHIST,J,I
      INTEGER HDEMSW,IRANK,ICLSEC,CRG
      REAL FACTO
      REAL*8 FACT2,FACT3,FACTE,RNWEMS(MNUMCR,MNPOLLUT,MNUMYR)
      REAL*8 AVG$C,AVG$U,TOT$C,TGEN,TSUM,TOTSO2O
      REAL IMPDAT,IMPEMM,NETIMP
      REAL TNGCAPC,TNGCONC,TOTGEN,TOTCON,TOTSCON,HCO2FAC(MNUMNR),TOTUTSECT
      REAL TOTCOLC,TOTGASC,TOTOILC,TOTCOLN,TOTGASN,TOTOILN
      REAL TOTNUCC,TOTNUCN,TOTHYDC,TOTHYDN,TOTMSWC,TOTMSWN,TOTWODC,TOTWODN
      REAL TOTGEOC,TOTGEON,TOTSOLC,TOTSOLN,TOTPVTC,TOTPVTN,TOTWNDC,TOTWNDN
      REAL NUCHRT,TDEM
      REAL TOTNTGEN
      REAL ALGAS(5)
      REAL RPSPCT
      REAL TOTADJNONNEG, TOTMSWGEN, FRACADJ(MNUMNR), MSADJ(4), ADJUSTMSNR(4), ADJNEG(MNUMNR), ADJSUM
      REAL UGNNBNR(2,MNUMNR,MNUMYR)
      REAL HRTF(EFD_D_MFRG + 1,MNUMYR),HRTN(MNUMNR,MNUMYR)
      INTEGER MSADJNEG(MNUMNR)
      CHARACTER*4 RECTYP
      CHARACTER*2 PLTTYP(ECP_D_CAP + 2)
      DATA ALGAS/35.122,32.019,29.590,28.642,29.687/
!
!     MAP PLANT CODES FOR OUTPUT TOTALS
!
      DO IFL = 1 , ECP_D_CAP
         PLTTYP(IFL) = UPLNTCD(IFL)
      END DO
         PLTTYP(ECP_D_CAP + 1) = 'TL'
         PLTTYP(ECP_D_CAP + 2) = 'AF'
!
!     INITIALIZE NATIONAL COGEN TOTALS
!
      DO IFL = 1, MNUMCGF
        CGTOTGENNR(MNUMNR,CURIYR,IFL,1) = 0.0
        CGTOTGENNR(MNUMNR,CURIYR,IFL,2) = 0.0
        CGTOTCAPNR(MNUMNR,CURIYR,IFL) = 0.0
        CGTOTQNR(MNUMNR,CURIYR,IFL) = 0.0
      ENDDO
!
!     ASSIGN HISTORICAL ALASKA GAS
!
        ALASKA = MNUMNR - 2
      DO IYR = 1 , 5
        HFLNGNR(1,ALASKA,IYR) = ALGAS(IYR)
      END DO
!
!     assign last historical year
!
      IF (USW_OVER .GT. 0) THEN
         IF (USW_OVER .EQ. 1) THEN
            UYR_OVER = UYR_HIST
         ELSE
            UYR_OVER = UYR_STEO
         END IF
!
!        ASSIGN MER HEATRATE FOR NUCLEAR FUEL CONSUMPTION AND FOSSIL FUEL EQ FOR DEMAND MODULES
!        Use historical overwrites value for historical years and last historical year for forecast years.
!
         IF (((CURIYR + UHBSYR) .LE. UYR_OVER)     &
              .AND. ((CURIYR + UHBSYR) .GE. HSTYEAR)) THEN
           NUCHRT = HNUCHR(CURIYR)                                         ! historical years
           DO ICR = 1 , MNUMCR
             WHRFOSS(ICR,CURIYR) = HFOSHRCR(ICR,CURIYR)
           ENDDO
         ELSEIF ((CURIYR + UHBSYR) .GT. UYR_OVER) THEN
           NUCHRT = HNUCHR(UYR_OVER - UHBSYR)                           ! forecast years
!          DO ICR = 1 , MNUMCR
!            WHRFOSS(ICR,CURIYR) = HFOSHRCR(ICR,UYR_OVER - UHBSYR)
!          ENDDO
         ENDIF
      ELSE
        NUCHRT = 10442.0                                               ! if historical overwrites not turned on.
!       WHRFOSS = 9450.0
      ENDIF
!
!     STORE FUEL QUANTITIES IN NEMS ARRAYS
!
      FACT2 = 0.001
      FACT3 = 0.000001
!     FACTOR TO CONVERT EMISSIONS FROM SHORT TONS TO MILLION METRIC TONS
      FACTE = (2000.0/2204.0)/1000000.0
!     INSURE THAT ROUNDOFF ERRORS DON'T RESULT IN NEGATIVE FUEL CONSUMPTION
      DO IFL = 1 , EFD_D_NFL
         DO IRG = 1 , EFD_D_MFRG
            DO ICR = 1 , UNFLRG(IFL)
               IF(UQFUEL(IFL,IRG,ICR) .LT. 0.0) THEN
                  UQFUEL(IFL,IRG,ICR) = 0.0
                  IF((UQFUEL(IFL,IRG,ICR) * FACT2) .LT. - 0.1) &
                      WRITE(22,5555) CURIYR + &
                       UHBSYR,IFL,IRG,UQFUEL(IFL,IRG,ICR) * FACT2
5555              FORMAT(1H ,'YR,FL,RG,UQFUEL',3I10,F10.3)
               END IF
            END DO
         END DO
      END DO
!
!  SO2 (COAL ONLY) AND CARBON REMOVAL RATES BY FUEL REGION
!
      ICR = 1
!  OIL
      DO IRG = 1 , MNUMCR - 1
         IF ( (UQFUEL(UIDS,IRG,ICR) .GT. 0.000) .OR.  &
            (UQFUEL(UIDD,IRG,ICR) .GT. 0.000) ) THEN
            XDSEL(IRG,CURIYR) = ( UXFUEL(UIDS,IRG) + UXFUEL(UIDD,IRG) ) /  &
               ( UQFUEL(UIDS,IRG,ICR) + UQFUEL(UIDD,IRG,ICR) )
         ELSE
            XDSEL(IRG,CURIYR) = 0.0
         END IF
         IF (UQFUEL(UIRL,IRG,ICR) .GT. 0.000) THEN
            XRLEL(IRG,CURIYR) = UXFUEL(UIRL,IRG) /  &
               UQFUEL(UIRL,IRG,ICR)
         ELSE
            XRLEL(IRG,CURIYR) = 0.0
         END IF
         IF (UQFUEL(UIRH,IRG,ICR) .GT. 0.000) THEN
            XRHEL(IRG,CURIYR) = UXFUEL(UIRH,IRG) /  &
               UQFUEL(UIRH,IRG,ICR)
         ELSE
            XRHEL(IRG,CURIYR) = 0.0
         END IF
      END DO
!
!     BIOMASS
!
      DO IRG = 1 , NDREG
         QBMELCL(IRG,CURIYR) = UQFUEL(UIWD,IRG,ICR) * FACT2
      END DO
!
!     COAL
!
      DO IRG = 1 , NDRGG
         DO IFL = 1 , NCLUT1
            QCLCLNR(IRG,CURIYR,IFL) = UQFUEL(IFL,IRG,ICR) * FACT2
            RCLCLNR(IRG,CURIYR,IFL) = UPPSEF(IFL)
            IF (UQFUEL(IFL,IRG,ICR) .GT. 0.000) THEN

               WRITE(22,2537) CURIRUN, CURIYR+1989, CURITR, ICR, IRG, IFL, QCLCLNR(IRG,CURIYR,IFL), UQFUEL(IFL,IRG,ICR), FACT2
 2537          FORMAT(1X,"UQFUEL_QCLCLNR",6(",",I4),3(",",F15.3))

!              RCLCLNR(IRG,CURIYR,IFL) = URFUEL(IFL,IRG) / UQFUEL(IFL,IRG,ICR)
               XCLCLNR(IRG,CURIYR,IFL) = UXFUEL(IFL,IRG) / UQFUEL(IFL,IRG,ICR)
            ELSE
!              IF (ECP_SCRUB(IFL,CURIYR) .EQ. 1) THEN
!                 RCLCLNR(IRG,CURIYR,IFL) = UPPSEF(IFL)
!              ELSE
!                 RCLCLNR(IRG,CURIYR,IFL) = 0.0
!              END IF
               XCLCLNR(IRG,CURIYR,IFL) = 0.0
            END IF
         END DO
      END DO
!
!  GAS
!
      DO IRG = 1 , NNGEM
      QNGELGR(IRG,CURIYR) = (UQFUEL(UIGF,IRG,ICR) + UQFUEL(UIGI,IRG,ICR) + UQFUEL(UIDG,IRG,ICR) + UQFUEL(UIGC,IRG,ICR)) * FACT2
      QGFELGR(IRG,CURIYR) = UQFUEL(UIGF,IRG,ICR) * FACT2
      QGIELGR(IRG,CURIYR) = ( UQFUEL(UIGI,IRG,ICR)  + UQFUEL(UIDG,IRG,ICR) ) * FACT2
      QGCELGR(IRG,CURIYR) = UQFUEL(UIGC,IRG,ICR) * FACT2
!
!  STORE EMM GAS CONSUMPTION FOR HISTORICAL/STEO YEARS
!
      EQGFELGR(IRG,CURIYR) = QGFELGR(IRG,CURIYR)
      EQGIELGR(IRG,CURIYR) = QGIELGR(IRG,CURIYR)
      EQGCELGR(IRG,CURIYR) = QGCELGR(IRG,CURIYR)
!
!   INITIALIZE SEASONAL GAS CONSUMPTION
!
        SQGFELGR(IRG,CURIYR,1) = 0.0
        SQGFELGR(IRG,CURIYR,2) = 0.0
        SQGIELGR(IRG,CURIYR,1) = 0.0
        SQGIELGR(IRG,CURIYR,2) = 0.0
        DO ISP = 1 , EFDns
            SQNGELGR(IRG,CURIYR,ISP) = 0.0
            IF (UNGSSN(ISP) .EQ. 0) THEN
               WRITE(6,3319) CURIRUN, CURIYR+1989, CURITR, IRG, ISP
 3319          FORMAT(1X,"UNGSSN_OOPS",5(":",I6))

               UNGSSN(ISP) = 2

            END IF

            SQGFELGR(IRG,CURIYR,UNGSSN(ISP)) = SQGFELGR(IRG,CURIYR,UNGSSN(ISP)) + SQFUEL(UIGF,IRG,ISP) * FACT2
            SQGIELGR(IRG,CURIYR,UNGSSN(ISP)) = SQGIELGR(IRG,CURIYR,UNGSSN(ISP)) + (SQFUEL(UIGI,IRG,ISP) + SQFUEL(UIGC,IRG,ISP) + SQFUEL(UIDG,IRG,ISP)) * FACT2
            SQNGELGR(IRG,CURIYR,ISP) = SQNGELGR(IRG,CURIYR,ISP) +  &
                                       (SQFUEL(UIGF,IRG,ISP) + SQFUEL(UIGI,IRG,ISP) + SQFUEL(UIGC,IRG,ISP) + SQFUEL(UIDG,IRG,ISP)) * FACT2
        END DO
!
!  STORE CARBON REMOVAL RATES
!
         IF (UQFUEL(UIGF,IRG,ICR) .GT. 0.000) THEN
            XQGFELGR(IRG,CURIYR) = UXFUEL(UIGF,IRG) / UQFUEL(UIGF,IRG,ICR)
         ELSE
            XQGFELGR(IRG,CURIYR) = 0.00
         END IF
         IF ( (UQFUEL(UIGI,IRG,ICR) .GT. 0.000) .OR. (UQFUEL(UIDG,IRG,ICR) .GT. 0.000) ) THEN
            XQGIELGR(IRG,CURIYR) = ( UXFUEL(UIGI,IRG) + UXFUEL(UIDG,IRG) )/ ( UQFUEL(UIGI,IRG,ICR) + UQFUEL(UIDG,IRG,ICR) )
         ELSE
            XQGIELGR(IRG,CURIYR) = 0.00
         END IF
         IF (UQFUEL(UIGC,IRG,ICR) .GT. 0.000) THEN
            XQGCELGR(IRG,CURIYR) = UXFUEL(UIGC,IRG) / UQFUEL(UIGC,IRG,ICR)
         ELSE
            XQGCELGR(IRG,CURIYR) = 0.00
         END IF
!
!        EXPECTATIONS FOR FUTURE GAS CONSUMPTION WILL BE ESTIMATED
!        BY MULTIPLYING CURRENT GAS CONSUMPTION BY THE RATIO OF
!        FUTURE CAPACITY TO CURRENT CAPACITY.  IF THERE IS NO CURRENT
!        CAPACITY FOR A PARTICULAR CATEGORY OF GAS USE, THE AVERAGE RATE
!        OF CONSUMPTION PER UNIT OF CAPACITY WILL BE USED
!
      IF(I4SITE .NE. 3) THEN
!
!  GET CURRENT TOTAL GAS-FIRED CAPACITY AND GAS CONSUMPTION
!
      TNGCAPC = EGFCAP(IRG,1) + EGICAP(IRG,1) + EGCCAP(IRG,1)
            TNGCONC = QGFELGR(IRG,CURIYR) + QGIELGR(IRG,CURIYR) + QGCELGR(IRG,CURIYR)
      DO IYR = 2,UNXPH
!
!     FIRM
!
      IF(EGFCAP(IRG,1) .GT. 0.0) THEN
                  XQNGELFN(IRG,CURIYR + IYR - 1) = QGFELGR(IRG,CURIYR) * EGFCAP(IRG,IYR) / EGFCAP(IRG,1)
      ELSE
         XQNGELFN(IRG,CURIYR + IYR - 1) = 0.0
                  IF(TNGCAPC .GT. 0.0) XQNGELFN(IRG,CURIYR + IYR - 1) = EGFCAP(IRG,IYR) * TNGCONC / TNGCAPC
      ENDIF
!
!     INTERRUPTIBLE
!
      IF(EGICAP(IRG,1) .GT. 0.0) THEN
         XQNGELIN(IRG,CURIYR + IYR - 1) = QGIELGR(IRG,CURIYR) * &
          EGICAP(IRG,IYR) / EGICAP(IRG,1)
      ELSE
         XQNGELIN(IRG,CURIYR + IYR - 1) = 0.0
                  IF(TNGCAPC .GT. 0.0) XQNGELIN(IRG,CURIYR + IYR - 1) = EGICAP(IRG,IYR) * TNGCONC / TNGCAPC
      ENDIF
!
!     COMPETITIVE
!
      IF(EGCCAP(IRG,1) .GT. 0.0) THEN
         XQNGELCN(IRG,CURIYR + IYR - 1) = QGCELGR(IRG,CURIYR) * &
          EGCCAP(IRG,IYR) / EGCCAP(IRG,1)
      ELSE
         XQNGELCN(IRG,CURIYR + IYR - 1) = 0.0
                  IF(TNGCAPC .GT. 0.0) XQNGELCN(IRG,CURIYR + IYR - 1) = EGCCAP(IRG,IYR) * TNGCONC / TNGCAPC
      END IF
      END DO
      END IF
      END DO
!
!     CENSUS REGION OUTPUT
!
      ICR = 2
!
!     INITIALIZE NATIONAL TOTALS (Q ARRAYS ARE TOTALED IN MAIN)
!
      DO IOWNER = 1 , 2
       DO IRG = 1 , MNUMNR
!GENERATION
          UGNCLNR(IOWNER,IRG,CURIYR) = 0.0
          UGNGFNR(IOWNER,IRG,CURIYR) = 0.0
          UGNGINR(IOWNER,IRG,CURIYR) = 0.0
          UGNGCNR(IOWNER,IRG,CURIYR) = 0.0
          UGNDSNR(IOWNER,IRG,CURIYR) = 0.0
          UGNRLNR(IOWNER,IRG,CURIYR) = 0.0
          UGNRHNR(IOWNER,IRG,CURIYR) = 0.0
          UGNURNR(IOWNER,IRG,CURIYR) = 0.0
          UGNPSNR(IOWNER,IRG,CURIYR) = 0.0
          UGNSDNR(IOWNER,IRG,CURIYR) = 0.0
          UGNHYNR(IOWNER,IRG,CURIYR) = 0.0
          UGNGENR(IOWNER,IRG,CURIYR) = 0.0
          UGNMSNR(IOWNER,IRG,CURIYR) = 0.0
          UGNWDNR(IOWNER,IRG,CURIYR) = 0.0
          UGNCFNR(IOWNER,IRG,CURIYR) = 0.0
          UGNSONR(IOWNER,IRG,CURIYR) = 0.0
          UGNPVNR(IOWNER,IRG,CURIYR) = 0.0
          UGNPTNR(IOWNER,IRG,CURIYR) = 0.0
          UGNWNNR(IOWNER,IRG,CURIYR) = 0.0
          UGNWLNR(IOWNER,IRG,CURIYR) = 0.0
          UGNWFNR(IOWNER,IRG,CURIYR) = 0.0
          UGNHONR(IOWNER,IRG,CURIYR) = 0.0
          UGNDGNR(IOWNER,IRG,CURIYR) = 0.0
          UGNDDNR(IOWNER,IRG,CURIYR) = 0.0
          UGNTLNR(IOWNER,IRG,CURIYR) = 0.0
!FUEL CONSUMPTION
          UFLCLNR(IOWNER,IRG,CURIYR) = 0.0
          UFLGFNR(IOWNER,IRG,CURIYR) = 0.0
          UFLGINR(IOWNER,IRG,CURIYR) = 0.0
          UFLGCNR(IOWNER,IRG,CURIYR) = 0.0
          UFLDSNR(IOWNER,IRG,CURIYR) = 0.0
          UFLRLNR(IOWNER,IRG,CURIYR) = 0.0
          UFLRHNR(IOWNER,IRG,CURIYR) = 0.0
          UFLURNR(IOWNER,IRG,CURIYR) = 0.0
          UFLPSNR(IOWNER,IRG,CURIYR) = 0.0
          UFLHYNR(IOWNER,IRG,CURIYR) = 0.0
          UFLOTNR(IOWNER,IRG,CURIYR) = 0.0
          UFLDGNR(IOWNER,IRG,CURIYR) = 0.0
          UFLDDNR(IOWNER,IRG,CURIYR) = 0.0
          UFLGTNR(IOWNER,IRG,CURIYR) = 0.0
          UFLWDNR(IOWNER,IRG,CURIYR) = 0.0
          UFLMSNR(IOWNER,IRG,CURIYR) = 0.0
          UFLSONR(IOWNER,IRG,CURIYR) = 0.0
          UFLPVNR(IOWNER,IRG,CURIYR) = 0.0
          UFLWNNR(IOWNER,IRG,CURIYR) = 0.0
          UFLWFNR(IOWNER,IRG,CURIYR) = 0.0
          UFLTLNR(IOWNER,IRG,CURIYR) = 0.0
!RPS GENERATION/CREDITS
          UCRCLNR(IOWNER,IRG,CURIYR) = 0.0
          UCRNGNR(IOWNER,IRG,CURIYR) = 0.0
          UCRNUNR(IOWNER,IRG,CURIYR) = 0.0
          UCRHYNR(IOWNER,IRG,CURIYR) = 0.0
          UCRGENR(IOWNER,IRG,CURIYR) = 0.0
          UCRMSNR(IOWNER,IRG,CURIYR) = 0.0
          UCRWDNR(IOWNER,IRG,CURIYR) = 0.0
          UCRCFNR(IOWNER,IRG,CURIYR) = 0.0
          UCRSONR(IOWNER,IRG,CURIYR) = 0.0
          UCRPVNR(IOWNER,IRG,CURIYR) = 0.0
          UCRPTNR(IOWNER,IRG,CURIYR) = 0.0
          UCRWNNR(IOWNER,IRG,CURIYR) = 0.0
          UCRWLNR(IOWNER,IRG,CURIYR) = 0.0
          UCRWFNR(IOWNER,IRG,CURIYR) = 0.0
          UCRTLNR(IOWNER,IRG,CURIYR) = 0.0
       END DO
      END DO
      UCRBSNR(MNUMNR,CURIYR) = 0.0
!RPS/CHP GENERATION/CREDITS
      DO IOWNER = 1 , 4
       DO IRG = 1 , MNUMNR
          UCRCHNR(IOWNER,IRG,CURIYR) = 0.0
       END DO
      END DO
!PTC GENERATION/COSTS
      DO IRG = 1 , MNUMNR
         UPTCLNR(IRG,CURIYR) = 0.0
         UPTNGNR(IRG,CURIYR) = 0.0
         UPTNUNR(IRG,CURIYR) = 0.0
         UPTHYNR(IRG,CURIYR) = 0.0
         UPTGENR(IRG,CURIYR) = 0.0
         UPTMSNR(IRG,CURIYR) = 0.0
         UPTWDNR(IRG,CURIYR) = 0.0
         UPTCFNR(IRG,CURIYR) = 0.0
         UPTSONR(IRG,CURIYR) = 0.0
         UPTPVNR(IRG,CURIYR) = 0.0
         UPTPTNR(IRG,CURIYR) = 0.0
         UPTWNNR(IRG,CURIYR) = 0.0
         UPTWLNR(IRG,CURIYR) = 0.0
         UPTWFNR(IRG,CURIYR) = 0.0
         UPTTLNR(IRG,CURIYR) = 0.0
         UPTCCST(IRG,CURIYR) = 0.0
      END DO
!NON-TRADITIONAL COGEN NATIONAL TOTALS
      IF(USW_OWN .GT. 3) THEN
      DO IRG = 1,MNUMNR
       DO IFL = 1,10
         CGNTGEN(IRG,CURIYR,IFL,1) = 0
         CGNTGEN(IRG,CURIYR,IFL,2) = 0
         CGNTQ(IRG,CURIYR,IFL) = 0
         IF (IRG .EQ. MNUMNR) CGOTGEN(IRG,CURIYR,IFL) = 0
       END DO
      END DO
      END IF
!
!     ELECTRICITY DEMAND BY SECTOR
!
!     US TOTAL
!
      QELRSN(MNUMNR,CURIYR) = 0.0
      QELCMN(MNUMNR,CURIYR) = 0.0
      QELINN(MNUMNR,CURIYR) = 0.0
      QELTRN(MNUMNR,CURIYR) = 0.0
      QELASN(MNUMNR,CURIYR) = 0.0
!
!     ALASKA
!
      ALASKA = MNUMNR - 2                          ! ALASKA
      QELRSN(ALASKA,CURIYR) = 0.0
      QELCMN(ALASKA,CURIYR) = 0.0
      QELINN(ALASKA,CURIYR) = 0.0
      QELTRN(ALASKA,CURIYR) = 0.0
      QELASN(ALASKA,CURIYR) = 0.0
      ULELRS(ALASKA) = 0.0
      ULELCM(ALASKA) = 0.0
      ULELIN(ALASKA) = 0.0
      ULELTR(ALASKA) = 0.0
!
!     HAWAII
!
      HAWAII = MNUMNR - 1                          ! HAWAII
      QELRSN(HAWAII,CURIYR) = 0.0
      QELCMN(HAWAII,CURIYR) = 0.0
      QELINN(HAWAII,CURIYR) = 0.0
      QELTRN(HAWAII,CURIYR) = 0.0
      QELASN(HAWAII,CURIYR) = 0.0
      ULELRS(HAWAII) = 0.0
      ULELCM(HAWAII) = 0.0
      ULELIN(HAWAII) = 0.0
      ULELTR(HAWAII) = 0.0
!
!     NUCLEAR GENERATION BY BWR AND PWR
!
!     UGNUBCR(MNUMCR,CURIYR) = 0.0
!     UGNUPCR(MNUMCR,CURIYR) = 0.0
!
!     INITIALIZE EMISSIONS
!     EMISSIONS BY CENSUS DIVISION
!
      DO IPOL = 1 , MNPOLLUT
         DO ICENSUS = 1 , MNUMCR
            EMELC(ICENSUS,IPOL,CURIYR) = 0.0
         END DO
!
!        EMISSIONS BY FUEL (1=NG,2=OL,3=CL,4=REN)
!
         DO IFL = 1 , 4
            EMEL(IFL,IPOL,CURIYR) = 0.0
         END DO
      END DO
!
!     CAPTURE SO2 AND MERCURY EMISSIONS FROM NON-COAL PLANTS
!
      DO ISO2 = 1 , NUM_SO2_GRP
         SO2OTHER(CURIYR,ISO2) = 0.0
      END DO
      DO ISO2 = 1 , NDREG
         SO2OTHCL(CURIYR,ISO2) = 0.0
      END DO
      HGOTHER(CURIYR) = 0.0
!
      DO IRG = 1 , MNUMNR
         UTCO2(IRG,CURIYR) = 0.0
         UTCO1(IRG,CURIYR) = 0.0
         UTCAR(IRG,CURIYR) = 0.0
         UTSO2(IRG,CURIYR) = 0.0
         UTNOX(IRG,CURIYR) = 0.0
         UTHG(IRG,CURIYR) = 0.0
      END DO
!
      UCO2INR(MNUMNR,CURIYR) = 0.0
      UCARINR(MNUMNR,CURIYR) = 0.0
      USO2INR(MNUMNR,CURIYR) = 0.0
      UNOXINR(MNUMNR,CURIYR) = 0.0
      UCO2OTR(MNUMNR,CURIYR) = 0.0
      UCAROTR(MNUMNR,CURIYR) = 0.0
      USO2OTR(MNUMNR,CURIYR) = 0.0
      UNOXOTR(MNUMNR,CURIYR) = 0.0
!
!     FILL NET IMPORT ARRAY
!
         NETIMP = UTIMPF(MNUMNR,CURIYR) + UTIMPE(MNUMNR,CURIYR) - &
            UTEXPF(MNUMNR,CURIYR) - UTEXPE(MNUMNR,CURIYR)
         QEIEL(MNUMCR,CURIYR) = NETIMP * .003412
!
      DO ICENSUS = 1 , MNUMCR - 1
         QEIEL(ICENSUS,CURIYR) = QEIEL(MNUMCR,CURIYR) * &
                                  NIMPCRSH(ICENSUS,(UYR_HIST-UHBSYR))
         QCLEL(ICENSUS,CURIYR) = (UQFUEL(UIB1,ICENSUS,ICR) + UQFUEL(UIB2,ICENSUS,ICR) +               &
            UQFUEL(UIB3,ICENSUS,ICR) + UQFUEL(UIB4,ICENSUS,ICR) + UQFUEL(UIB5,ICENSUS,ICR) +          &
            UQFUEL(UIB6,ICENSUS,ICR) + UQFUEL(UIB7,ICENSUS,ICR) + UQFUEL(UIB8,ICENSUS,ICR) +          &
            UQFUEL(UIC1,ICENSUS,ICR) + UQFUEL(UIC2,ICENSUS,ICR) + UQFUEL(UIC3,ICENSUS,ICR) +          &
            UQFUEL(UIC4,ICENSUS,ICR) + UQFUEL(UIC5,ICENSUS,ICR) + UQFUEL(UIC6,ICENSUS,ICR) +          &
            UQFUEL(UIC7,ICENSUS,ICR) + UQFUEL(UIC8,ICENSUS,ICR) + UQFUEL(UIC9,ICENSUS,ICR) +          &
            UQFUEL(UICX,ICENSUS,ICR) + UQFUEL(UICY,ICENSUS,ICR) + UQFUEL(UICZ,ICENSUS,ICR) +          &
            UQFUEL(UIH1,ICENSUS,ICR) + UQFUEL(UIH2,ICENSUS,ICR) + UQFUEL(UIH3,ICENSUS,ICR) +          &
            UQFUEL(UIH4,ICENSUS,ICR) + UQFUEL(UIH5,ICENSUS,ICR) + UQFUEL(UIH6,ICENSUS,ICR) +          &
            UQFUEL(UIH7,ICENSUS,ICR) + UQFUEL(UIH8,ICENSUS,ICR) + UQFUEL(UIH9,ICENSUS,ICR) +          &
            UQFUEL(UIHA,ICENSUS,ICR) + UQFUEL(UIHB,ICENSUS,ICR) + UQFUEL(UIHC,ICENSUS,ICR) +          &
            UQFUEL(UIPC,ICENSUS,ICR) + UQFUEL(UIIG,ICENSUS,ICR) + UQFUEL(UIPQ,ICENSUS,ICR) + UQFUEL(UIIS,ICENSUS,ICR) ) * FACT2
!
         QDSEL(ICENSUS,CURIYR) = (UQFUEL(UIDS,ICENSUS,ICR) + UQFUEL(UIDD,ICENSUS,ICR))* FACT2
         QRLEL(ICENSUS,CURIYR) = UQFUEL(UIRL,ICENSUS,ICR) * FACT2
         QRHEL(ICENSUS,CURIYR) = UQFUEL(UIRH,ICENSUS,ICR) * FACT2
         QPCEL(ICENSUS,CURIYR) = 0.0
         QRSEL(ICENSUS,CURIYR) = QRLEL(ICENSUS,CURIYR) + &
            QRHEL(ICENSUS,CURIYR)
         QTPEL(ICENSUS,CURIYR) = QDSEL(ICENSUS,CURIYR) + &
            QRLEL(ICENSUS,CURIYR) + &
            QRHEL(ICENSUS,CURIYR) + &
            QPCEL(ICENSUS,CURIYR)
         QGFEL(ICENSUS,CURIYR) = UQFUEL(UIGF,ICENSUS,ICR) * FACT2
         QGIEL(ICENSUS,CURIYR) = (UQFUEL(UIGI,ICENSUS,ICR) + &
         UQFUEL(UIGC,ICENSUS,ICR) + UQFUEL(UIDG,ICENSUS,ICR) ) * FACT2
         QNGEL(ICENSUS,CURIYR) = QGFEL(ICENSUS,CURIYR) + &
            QGIEL(ICENSUS,CURIYR)
         QUREL(ICENSUS,CURIYR) = UQFGENC(UIUF,ICENSUS) * NUCHRT * FACT3
         QBMEL(ICENSUS,CURIYR) = UQFUEL(UIWD,ICENSUS,ICR) * FACT2
         QHOEL(ICENSUS,CURIYR) = (UQFUEL(UIWA,ICENSUS,ICR) +  &
             UQFUEL(UITI,ICENSUS,ICR)) * FACT2
         QGEEL(ICENSUS,CURIYR) = UQFUEL(UIGT,ICENSUS,ICR) * FACT2
         QMSEL(ICENSUS,CURIYR) = UQFUEL(UISW,ICENSUS,ICR) * FACT2
         QSTEL(ICENSUS,CURIYR) = UQFUEL(UISO,ICENSUS,ICR) * FACT2
         QPVEL(ICENSUS,CURIYR) = UQFUEL(UIPV,ICENSUS,ICR) * FACT2 + UQFUEL(UIPT,ICENSUS,ICR) * FACT2
!
!        instead of adding new variable, add offshore into QWIEL
!
         QWIEL(ICENSUS,CURIYR) = UQFUEL(UIWN,ICENSUS,ICR) * FACT2  +  &
                                 UQFUEL(UIWL,ICENSUS,ICR) * FACT2 + &
                                 UQFUEL(UIWF,ICENSUS,ICR) * FACT2
!
!        FILL NERC REGION DEMANDS FOR ALASKA AND HAWAII
!
         IF (ICENSUS .EQ. 9) THEN
!
!     use last overwrites year for base year
!
            IF (USW_OVER .GT. 0) THEN
              IF (USW_OVER .EQ. 1) THEN
                ALASYR = UYR_HIST
              ELSE
                ALASYR = UYR_STEO
              END IF
            ELSE
              ALASYR = UYR_HIST
            ENDIF
            IF (CURIYR .LE. (ALASYR - UHBSYR)) THEN
               IYR = CURIYR
               FACTO = 1.0
            ELSE
               IYR = ALASYR - UHBSYR
               IF (QELAS(ICENSUS,IYR) .NE. 0.0) &
                  FACTO = QELAS(ICENSUS,CURIYR) / &
                  QELAS(ICENSUS,IYR)
            END IF
!
!           CALL ROUTINES TO DO ALASKA AND HAWAII DEMANDS, GENERATION, AND FUEL CONSUMPTION
!           ASSUME ALASKA RESULTS GROW ACCORDING TO THE DEMAND GROWTH IN CENSUS DIVISION 9
!           ASSUME HAWAII DEMANDS GROW ACCORDING TO THE DEMAND GROWTH IN CENSUS DIVISION 9 BUT
!           USE ANNOUNCED RPS TARGETS TO DETERMINE HAWAII GENERATION AND CONSUMPTION
!           ADD TO CENSUS DIVISION 9 TOTALS
!
            CALL EDSPOAK(ALASKA,ICENSUS,IYR,FACTO,NUCHRT)
            CALL EDSPOHI(HAWAII,ICENSUS,IYR,FACTO,NUCHRT)
         END IF

!
!SET RENEWABLE INFO YEAR
!
      IF((CURIYR .EQ. FIRSYR) .OR. (CURITR .GT. 1)) THEN
         RYR = CURIYR
      ELSE
         RYR = CURIYR - 1
      ENDIF
!
!     CALCULATE RENEWABLE EMISSIONS BY CENSUS REGION.
!
      RNWEMS(ICENSUS,1,CURIYR) = (                                                &
         (UQPGENC(UIMSW,ICENSUS) * .001 * WHRMSEL(1,RYR) * WEMMSEL(1,RYR) * .5)   &
                                 ) * FACTE
      RNWEMS(ICENSUS,2,CURIYR) = (                                                &
         (UQPGENC(UIMSW,ICENSUS) * .001 * WHRMSEL(1,RYR) * WEMMSEL(2,RYR) * .5)   &
                                 ) * FACTE
      RNWEMS(ICENSUS,3,CURIYR) = (                                                &
         (UQPGENC(UIMSW,ICENSUS) * .001 * WHRMSEL(1,RYR) * WEMMSEL(3,RYR) * .5)   &
                                 ) * FACTE
      RNWEMS(ICENSUS,4,CURIYR) = (                                                &
         (UQPGENC(UIMSW,ICENSUS) * .001 * WHRMSEL(1,RYR) * WEMMSEL(4,RYR) * .5)   &
                                 ) * FACTE
      RNWEMS(ICENSUS,5,CURIYR) = (                                                &
         (UQPGENC(UIMSW,ICENSUS) * .001 * WHRMSEL(1,RYR) * WEMMSEL(5,RYR) * .5)   &
                                 ) * FACTE
      RNWEMS(ICENSUS,6,CURIYR) = (                                                &
         (UQPGENC(UIMSW,ICENSUS) * .001 * WHRMSEL(1,RYR) * WEMMSEL(6,RYR) * .5)   &
                                 ) * FACTE
!
!
!EMISSIONS BY FUEL TYPE (1=NG,2=OL,3=CL,4=REN)
!
!
      EMEL(1,1,CURIYR) = EMEL(1,1,CURIYR) + &
         (UTCARC(UIGF,ICENSUS) + &
         UTCARC(UIGI,ICENSUS) + UTCARC(UIDG,ICENSUS) + &
         UTCARC(UIGC,ICENSUS)) * FACTE
      EMEL(2,1,CURIYR) = EMEL(2,1,CURIYR) + &
         (UTCARC(UIDS,ICENSUS) + UTCARC(UIDD,ICENSUS) + &
         UTCARC(UIRL,ICENSUS) + &
         UTCARC(UIRH,ICENSUS)) * FACTE
      EMEL(3,1,CURIYR) = EMEL(3,1,CURIYR) +  &
         (UTCARC(UIB1,ICENSUS) +             &
          UTCARC(UIB2,ICENSUS) +             &
          UTCARC(UIB3,ICENSUS) +             &
          UTCARC(UIB4,ICENSUS) +             &
          UTCARC(UIB5,ICENSUS) +             &
          UTCARC(UIB6,ICENSUS) +             &
          UTCARC(UIB7,ICENSUS) +             &
          UTCARC(UIB8,ICENSUS) +             &
          UTCARC(UIC1,ICENSUS) +             &
          UTCARC(UIC2,ICENSUS) +             &
          UTCARC(UIC3,ICENSUS) +             &
          UTCARC(UIC4,ICENSUS) +             &
          UTCARC(UIC5,ICENSUS) +             &
          UTCARC(UIC6,ICENSUS) +             &
          UTCARC(UIC7,ICENSUS) +             &
          UTCARC(UIC8,ICENSUS) +             &
          UTCARC(UIC9,ICENSUS) +             &
          UTCARC(UICX,ICENSUS) +             &
          UTCARC(UICY,ICENSUS) +             &
          UTCARC(UICZ,ICENSUS) +             &
          UTCARC(UIH1,ICENSUS) +             &
          UTCARC(UIH2,ICENSUS) +             &
          UTCARC(UIH3,ICENSUS) +             &
          UTCARC(UIH4,ICENSUS) +             &
          UTCARC(UIH5,ICENSUS) +             &
          UTCARC(UIH6,ICENSUS) +             &
          UTCARC(UIH7,ICENSUS) +             &
          UTCARC(UIH8,ICENSUS) +             &
          UTCARC(UIH9,ICENSUS) +             &
          UTCARC(UIHA,ICENSUS) +             &
          UTCARC(UIHB,ICENSUS) +             &
          UTCARC(UIHC,ICENSUS) +             &
          UTCARC(UIPC,ICENSUS) +             &
          UTCARC(UIIG,ICENSUS) +             &
          UTCARC(UIPQ,ICENSUS) +             &
          UTCARC(UIIS,ICENSUS) ) * FACTE
!
      EMEL(4,1,CURIYR) = EMEL(4,1,CURIYR) + RNWEMS(ICENSUS,1,CURIYR)
      EMEL(1,2,CURIYR) = EMEL(1,2,CURIYR) + &
         (UTCO1C(UIGF,ICENSUS) + UTCO1C(UIDG,ICENSUS) + &
         UTCO1C(UIGI,ICENSUS) + &
         UTCO1C(UIGC,ICENSUS)) * FACTE
      EMEL(2,2,CURIYR) = EMEL(2,2,CURIYR) + &
         (UTCO1C(UIDS,ICENSUS) + UTCO1C(UIDD,ICENSUS) + &
         UTCO1C(UIRL,ICENSUS) + &
         UTCO1C(UIRH,ICENSUS)) * FACTE
      EMEL(3,2,CURIYR) = EMEL(3,2,CURIYR) + &
         (UTCO1C(UIB1,ICENSUS) +             &
          UTCO1C(UIB2,ICENSUS) +             &
          UTCO1C(UIB3,ICENSUS) +             &
          UTCO1C(UIB4,ICENSUS) +             &
          UTCO1C(UIB5,ICENSUS) +             &
          UTCO1C(UIB6,ICENSUS) +             &
          UTCO1C(UIB7,ICENSUS) +             &
          UTCO1C(UIB8,ICENSUS) +             &
          UTCO1C(UIC1,ICENSUS) +             &
          UTCO1C(UIC2,ICENSUS) +             &
          UTCO1C(UIC3,ICENSUS) +             &
          UTCO1C(UIC4,ICENSUS) +             &
          UTCO1C(UIC5,ICENSUS) +             &
          UTCO1C(UIC6,ICENSUS) +             &
          UTCO1C(UIC7,ICENSUS) +             &
          UTCO1C(UIC8,ICENSUS) +             &
          UTCO1C(UIC9,ICENSUS) +             &
          UTCO1C(UICX,ICENSUS) +             &
          UTCO1C(UICY,ICENSUS) +             &
          UTCO1C(UICZ,ICENSUS) +             &
          UTCO1C(UIH1,ICENSUS) +             &
          UTCO1C(UIH2,ICENSUS) +             &
          UTCO1C(UIH3,ICENSUS) +             &
          UTCO1C(UIH4,ICENSUS) +             &
          UTCO1C(UIH5,ICENSUS) +             &
          UTCO1C(UIH6,ICENSUS) +             &
          UTCO1C(UIH7,ICENSUS) +             &
          UTCO1C(UIH8,ICENSUS) +             &
          UTCO1C(UIH9,ICENSUS) +             &
          UTCO1C(UIHA,ICENSUS) +             &
          UTCO1C(UIHB,ICENSUS) +             &
          UTCO1C(UIHC,ICENSUS) +             &
          UTCO1C(UIPC,ICENSUS) +             &
          UTCO1C(UIIG,ICENSUS) +             &
          UTCO1C(UIPQ,ICENSUS) +             &
          UTCO1C(UIIS,ICENSUS) ) * FACTE
      EMEL(4,2,CURIYR) = EMEL(4,2,CURIYR) + RNWEMS(ICENSUS,2,CURIYR)
      EMEL(1,3,CURIYR) = EMEL(1,3,CURIYR) + &
         (UTCO2C(UIGF,ICENSUS) + UTCO2C(UIDG,ICENSUS) + &
         UTCO2C(UIGI,ICENSUS) + &
         UTCO2C(UIGC,ICENSUS)) * FACTE
      EMEL(2,3,CURIYR) = EMEL(2,3,CURIYR) + &
         (UTCO2C(UIDS,ICENSUS) + UTCO2C(UIDD,ICENSUS) + &
         UTCO2C(UIRL,ICENSUS) + &
         UTCO2C(UIRH,ICENSUS)) * FACTE
      EMEL(3,3,CURIYR) = EMEL(3,3,CURIYR) + &
         (UTCO2C(UIB1,ICENSUS) +             &
          UTCO2C(UIB2,ICENSUS) +             &
          UTCO2C(UIB3,ICENSUS) +             &
          UTCO2C(UIB4,ICENSUS) +             &
          UTCO2C(UIB5,ICENSUS) +             &
          UTCO2C(UIB6,ICENSUS) +             &
          UTCO2C(UIB7,ICENSUS) +             &
          UTCO2C(UIB8,ICENSUS) +             &
          UTCO2C(UIC1,ICENSUS) +             &
          UTCO2C(UIC2,ICENSUS) +             &
          UTCO2C(UIC3,ICENSUS) +             &
          UTCO2C(UIC4,ICENSUS) +             &
          UTCO2C(UIC5,ICENSUS) +             &
          UTCO2C(UIC6,ICENSUS) +             &
          UTCO2C(UIC7,ICENSUS) +             &
          UTCO2C(UIC8,ICENSUS) +             &
          UTCO2C(UIC9,ICENSUS) +             &
          UTCO2C(UICX,ICENSUS) +             &
          UTCO2C(UICY,ICENSUS) +             &
          UTCO2C(UICZ,ICENSUS) +             &
          UTCO2C(UIH1,ICENSUS) +             &
          UTCO2C(UIH2,ICENSUS) +             &
          UTCO2C(UIH3,ICENSUS) +             &
          UTCO2C(UIH4,ICENSUS) +             &
          UTCO2C(UIH5,ICENSUS) +             &
          UTCO2C(UIH6,ICENSUS) +             &
          UTCO2C(UIH7,ICENSUS) +             &
          UTCO2C(UIH8,ICENSUS) +             &
          UTCO2C(UIH9,ICENSUS) +             &
          UTCO2C(UIHA,ICENSUS) +             &
          UTCO2C(UIHB,ICENSUS) +             &
          UTCO2C(UIHC,ICENSUS) +             &
          UTCO2C(UIPC,ICENSUS) +             &
          UTCO2C(UIIG,ICENSUS) +             &
          UTCO2C(UIPQ,ICENSUS) +             &
          UTCO2C(UIIS,ICENSUS) ) * FACTE
      EMEL(4,3,CURIYR) = EMEL(4,3,CURIYR) + &
      RNWEMS(ICENSUS,3,CURIYR)
      EMEL(1,4,CURIYR) = EMEL(1,4,CURIYR) + &
      (UTSO2C(UIGF,ICENSUS) + UTSO2C(UIDG,ICENSUS) + &
      UTSO2C(UIGI,ICENSUS) + &
      UTSO2C(UIGC,ICENSUS)) * FACTE
      EMEL(2,4,CURIYR) = EMEL(2,4,CURIYR) + &
      (UTSO2C(UIDS,ICENSUS) + UTSO2C(UIDD,ICENSUS) + &
      UTSO2C(UIRL,ICENSUS) + &
      UTSO2C(UIRH,ICENSUS)) * FACTE
      EMEL(3,4,CURIYR) = EMEL(3,4,CURIYR) + &
         (UTSO2C(UIB1,ICENSUS) +             &
          UTSO2C(UIB2,ICENSUS) +             &
          UTSO2C(UIB3,ICENSUS) +             &
          UTSO2C(UIB4,ICENSUS) +             &
          UTSO2C(UIB5,ICENSUS) +             &
          UTSO2C(UIB6,ICENSUS) +             &
          UTSO2C(UIB7,ICENSUS) +             &
          UTSO2C(UIB8,ICENSUS) +             &
          UTSO2C(UIC1,ICENSUS) +             &
          UTSO2C(UIC2,ICENSUS) +             &
          UTSO2C(UIC3,ICENSUS) +             &
          UTSO2C(UIC4,ICENSUS) +             &
          UTSO2C(UIC5,ICENSUS) +             &
          UTSO2C(UIC6,ICENSUS) +             &
          UTSO2C(UIC7,ICENSUS) +             &
          UTSO2C(UIC8,ICENSUS) +             &
          UTSO2C(UIC9,ICENSUS) +             &
          UTSO2C(UICX,ICENSUS) +             &
          UTSO2C(UICY,ICENSUS) +             &
          UTSO2C(UICZ,ICENSUS) +             &
          UTSO2C(UIH1,ICENSUS) +             &
          UTSO2C(UIH2,ICENSUS) +             &
          UTSO2C(UIH3,ICENSUS) +             &
          UTSO2C(UIH4,ICENSUS) +             &
          UTSO2C(UIH5,ICENSUS) +             &
          UTSO2C(UIH6,ICENSUS) +             &
          UTSO2C(UIH7,ICENSUS) +             &
          UTSO2C(UIH8,ICENSUS) +             &
          UTSO2C(UIH9,ICENSUS) +             &
          UTSO2C(UIHA,ICENSUS) +             &
          UTSO2C(UIHB,ICENSUS) +             &
          UTSO2C(UIHC,ICENSUS) +             &
          UTSO2C(UIPC,ICENSUS) +             &
          UTSO2C(UIIG,ICENSUS) +             &
          UTSO2C(UIPQ,ICENSUS) +             &
          UTSO2C(UIIS,ICENSUS) ) * FACTE
      EMEL(4,4,CURIYR) = EMEL(4,4,CURIYR) + &
      RNWEMS(ICENSUS,4,CURIYR)
      EMEL(1,5,CURIYR) = EMEL(1,5,CURIYR) + &
      (UTNOXC(UIGF,ICENSUS) + UTNOXC(UIDG,ICENSUS) + &
      UTNOXC(UIGI,ICENSUS) + &
      UTNOXC(UIGC,ICENSUS)) * FACTE
      EMEL(2,5,CURIYR) = EMEL(2,5,CURIYR) + &
      (UTNOXC(UIDS,ICENSUS) + UTNOXC(UIDD,ICENSUS) + &
      UTNOXC(UIRL,ICENSUS) + &
      UTNOXC(UIRH,ICENSUS)) * FACTE
      EMEL(3,5,CURIYR) = EMEL(3,5,CURIYR) + &
         (UTNOXC(UIB1,ICENSUS) +             &
          UTNOXC(UIB2,ICENSUS) +             &
          UTNOXC(UIB3,ICENSUS) +             &
          UTNOXC(UIB4,ICENSUS) +             &
          UTNOXC(UIB5,ICENSUS) +             &
          UTNOXC(UIB6,ICENSUS) +             &
          UTNOXC(UIB7,ICENSUS) +             &
          UTNOXC(UIB8,ICENSUS) +             &
          UTNOXC(UIC1,ICENSUS) +             &
          UTNOXC(UIC2,ICENSUS) +             &
          UTNOXC(UIC3,ICENSUS) +             &
          UTNOXC(UIC4,ICENSUS) +             &
          UTNOXC(UIC5,ICENSUS) +             &
          UTNOXC(UIC6,ICENSUS) +             &
          UTNOXC(UIC7,ICENSUS) +             &
          UTNOXC(UIC8,ICENSUS) +             &
          UTNOXC(UIC9,ICENSUS) +             &
          UTNOXC(UICX,ICENSUS) +             &
          UTNOXC(UICY,ICENSUS) +             &
          UTNOXC(UICZ,ICENSUS) +             &
          UTNOXC(UIH1,ICENSUS) +             &
          UTNOXC(UIH2,ICENSUS) +             &
          UTNOXC(UIH3,ICENSUS) +             &
          UTNOXC(UIH4,ICENSUS) +             &
          UTNOXC(UIH5,ICENSUS) +             &
          UTNOXC(UIH6,ICENSUS) +             &
          UTNOXC(UIH7,ICENSUS) +             &
          UTNOXC(UIH8,ICENSUS) +             &
          UTNOXC(UIH9,ICENSUS) +             &
          UTNOXC(UIHA,ICENSUS) +             &
          UTNOXC(UIHB,ICENSUS) +             &
          UTNOXC(UIHC,ICENSUS) +             &
          UTNOXC(UIPC,ICENSUS) +             &
          UTNOXC(UIIG,ICENSUS) +             &
          UTNOXC(UIPQ,ICENSUS) +             &
          UTNOXC(UIIS,ICENSUS) ) * FACTE
      EMEL(4,5,CURIYR) = EMEL(4,5,CURIYR) + &
      RNWEMS(ICENSUS,5,CURIYR)
      EMEL(1,6,CURIYR) = EMEL(1,6,CURIYR) + &
      (UTVOCC(UIGF,ICENSUS) + UTVOCC(UIDG,ICENSUS) + &
      UTVOCC(UIGI,ICENSUS) + &
      UTVOCC(UIGC,ICENSUS)) * FACTE
      EMEL(2,6,CURIYR) = EMEL(2,6,CURIYR) + &
      (UTVOCC(UIDS,ICENSUS) + UTVOCC(UIDD,ICENSUS) + &
      UTVOCC(UIRL,ICENSUS) + &
      UTVOCC(UIRH,ICENSUS)) * FACTE
      EMEL(3,6,CURIYR) = EMEL(3,6,CURIYR) + &
         (UTVOCC(UIB1,ICENSUS) +             &
          UTVOCC(UIB2,ICENSUS) +             &
          UTVOCC(UIB3,ICENSUS) +             &
          UTVOCC(UIB4,ICENSUS) +             &
          UTVOCC(UIB5,ICENSUS) +             &
          UTVOCC(UIB6,ICENSUS) +             &
          UTVOCC(UIB7,ICENSUS) +             &
          UTVOCC(UIB8,ICENSUS) +             &
          UTVOCC(UIC1,ICENSUS) +             &
          UTVOCC(UIC2,ICENSUS) +             &
          UTVOCC(UIC3,ICENSUS) +             &
          UTVOCC(UIC4,ICENSUS) +             &
          UTVOCC(UIC5,ICENSUS) +             &
          UTVOCC(UIC6,ICENSUS) +             &
          UTVOCC(UIC7,ICENSUS) +             &
          UTVOCC(UIC8,ICENSUS) +             &
          UTVOCC(UIC9,ICENSUS) +             &
          UTVOCC(UICX,ICENSUS) +             &
          UTVOCC(UICY,ICENSUS) +             &
          UTVOCC(UICZ,ICENSUS) +             &
          UTVOCC(UIH1,ICENSUS) +             &
          UTVOCC(UIH2,ICENSUS) +             &
          UTVOCC(UIH3,ICENSUS) +             &
          UTVOCC(UIH4,ICENSUS) +             &
          UTVOCC(UIH5,ICENSUS) +             &
          UTVOCC(UIH6,ICENSUS) +             &
          UTVOCC(UIH7,ICENSUS) +             &
          UTVOCC(UIH8,ICENSUS) +             &
          UTVOCC(UIH9,ICENSUS) +             &
          UTVOCC(UIHA,ICENSUS) +             &
          UTVOCC(UIHB,ICENSUS) +             &
          UTVOCC(UIHC,ICENSUS) +             &
          UTVOCC(UIPC,ICENSUS) +             &
          UTVOCC(UIIG,ICENSUS) +             &
          UTVOCC(UIPQ,ICENSUS) +             &
          UTVOCC(UIIS,ICENSUS) ) * FACTE
      EMEL(4,6,CURIYR) = EMEL(4,6,CURIYR) + &
      RNWEMS(ICENSUS,6,CURIYR)
!
      DO IFL = 1 , EFD_D_NFL
        EMELC(ICENSUS,1,CURIYR) = EMELC(ICENSUS,1,CURIYR) + UTCARC(IFL,ICENSUS) * FACTE
        EMELC(ICENSUS,2,CURIYR) = EMELC(ICENSUS,2,CURIYR) + UTCO1C(IFL,ICENSUS) * FACTE
        EMELC(ICENSUS,3,CURIYR) = EMELC(ICENSUS,3,CURIYR) + UTCO2C(IFL,ICENSUS) * FACTE
        EMELC(ICENSUS,4,CURIYR) = EMELC(ICENSUS,4,CURIYR) + UTSO2C(IFL,ICENSUS) * FACTE
        EMELC(ICENSUS,5,CURIYR) = EMELC(ICENSUS,5,CURIYR) + UTNOXC(IFL,ICENSUS) * FACTE
        EMELC(ICENSUS,6,CURIYR) = EMELC(ICENSUS,6,CURIYR) + UTVOCC(IFL,ICENSUS) * FACTE
!                                IF (CURIYR.eq.27) WRITE(6,3823)'EMELC/UTCARC ', CURIYR,CURITR,ICENSUS,EMELC(ICENSUS,1,CURIYR), UTCARC(IFL,ICENSUS)
!                                IF (CURIYR.eq.27) WRITE(6,3823)'EMELC/UTCO1C ', CURIYR,CURITR,ICENSUS,EMELC(ICENSUS,1,CURIYR), UTCO1C(IFL,ICENSUS)
!                                IF (CURIYR.eq.27) WRITE(6,3823)'EMELC/UTCO2C ', CURIYR,CURITR,ICENSUS,EMELC(ICENSUS,3,CURIYR), UTCO2C(IFL,ICENSUS)
!                                IF (CURIYR.eq.27) WRITE(6,3823)'EMELC/UTSO2C ', CURIYR,CURITR,ICENSUS,EMELC(ICENSUS,3,CURIYR), UTSO2C(IFL,ICENSUS)
!                                IF (CURIYR.eq.27) WRITE(6,3823)'EMELC/UTNOXC ', CURIYR,CURITR,ICENSUS,EMELC(ICENSUS,5,CURIYR), UTNOXC(IFL,ICENSUS)
!                                IF (CURIYR.eq.27) WRITE(6,3823)'EMELC/UTVOCC ', CURIYR,CURITR,ICENSUS,EMELC(ICENSUS,6,CURIYR), UTVOCC(IFL,ICENSUS)
3823  FORMAT(A25,1x,3(I4,1x),3(F12.3,1x))

!  ACCUMULATE NATIONAL TOTALS
        EMELC(MNUMCR,1,CURIYR) = EMELC(MNUMCR,1,CURIYR) + UTCARC(IFL,ICENSUS) * FACTE
        EMELC(MNUMCR,2,CURIYR) = EMELC(MNUMCR,2,CURIYR) + UTCO1C(IFL,ICENSUS) * FACTE
        EMELC(MNUMCR,3,CURIYR) = EMELC(MNUMCR,3,CURIYR) + UTCO2C(IFL,ICENSUS) * FACTE
        EMELC(MNUMCR,4,CURIYR) = EMELC(MNUMCR,4,CURIYR) + UTSO2C(IFL,ICENSUS) * FACTE
        EMELC(MNUMCR,5,CURIYR) = EMELC(MNUMCR,5,CURIYR) + UTNOXC(IFL,ICENSUS) * FACTE
        EMELC(MNUMCR,6,CURIYR) = EMELC(MNUMCR,6,CURIYR) + UTVOCC(IFL,ICENSUS) * FACTE
      END DO
!
!ADD RENEWABLE EMISSIONS TO NATIONAL & CENSUS TOTALS
      EMELC(ICENSUS,1,CURIYR) = EMELC(ICENSUS,1,CURIYR) + RNWEMS(ICENSUS,1,CURIYR)
      EMELC(ICENSUS,2,CURIYR) = EMELC(ICENSUS,2,CURIYR) + RNWEMS(ICENSUS,2,CURIYR)
      EMELC(ICENSUS,3,CURIYR) = EMELC(ICENSUS,3,CURIYR) + RNWEMS(ICENSUS,3,CURIYR)
      EMELC(ICENSUS,4,CURIYR) = EMELC(ICENSUS,4,CURIYR) + RNWEMS(ICENSUS,4,CURIYR)
      EMELC(ICENSUS,5,CURIYR) = EMELC(ICENSUS,5,CURIYR) + RNWEMS(ICENSUS,5,CURIYR)
      EMELC(ICENSUS,6,CURIYR) = EMELC(ICENSUS,6,CURIYR) + RNWEMS(ICENSUS,6,CURIYR)
!ACCUMULATE NATIONAL TOTALS
      EMELC(MNUMCR,1,CURIYR) = EMELC(MNUMCR,1,CURIYR) + RNWEMS(ICENSUS,1,CURIYR)
      EMELC(MNUMCR,2,CURIYR) = EMELC(MNUMCR,2,CURIYR) + RNWEMS(ICENSUS,2,CURIYR)
      EMELC(MNUMCR,3,CURIYR) = EMELC(MNUMCR,3,CURIYR) + RNWEMS(ICENSUS,3,CURIYR)
      EMELC(MNUMCR,4,CURIYR) = EMELC(MNUMCR,4,CURIYR) + RNWEMS(ICENSUS,4,CURIYR)
      EMELC(MNUMCR,5,CURIYR) = EMELC(MNUMCR,5,CURIYR) + RNWEMS(ICENSUS,5,CURIYR)
      EMELC(MNUMCR,6,CURIYR) = EMELC(MNUMCR,6,CURIYR) + RNWEMS(ICENSUS,6,CURIYR)
!
!     CAPTURE SO2 EMISSIONS FROM NON-COAL PLANTS
!
       DO ISO2 = 1 , NUM_SO2_GRP
        IF (SO2_SHR_BY_OLRG(ICENSUS,ISO2) .GT. 0.0)THEN
          SO2OTHER(CURIYR,ISO2) = SO2OTHER(CURIYR,ISO2) +  &
            (UTSO2C(UIDS,ICENSUS) + &
             UTSO2C(UIRL,ICENSUS) + &
             UTSO2C(UIRH,ICENSUS) + &
             UTSO2C(UIGF,ICENSUS) + &
             UTSO2C(UIGI,ICENSUS) + &
             UTSO2C(UIGC,ICENSUS) + &
             UTSO2C(UIDG,ICENSUS) + &
             UTSO2C(UIDD,ICENSUS) + &
             UTSO2C(UIUF,ICENSUS) + &
             RNWEMS(ICENSUS,4,CURIYR) / FACTE) * SO2_SHR_BY_OLRG(ICENSUS,ISO2)
        END IF
       END DO
       IF (TSO2_YR_BY_CLRG .LT. 9999)THEN
         DO ISO2 = 1 , NDREG
          IF (TSO2_OSH_BY_OLCL(ICENSUS,ISO2,1) .GT. 0.0)THEN
            SO2OTHCL(CURIYR,ISO2) = SO2OTHCL(CURIYR,ISO2) +  &
              (UTSO2C(UIDS,ICENSUS) + &
               UTSO2C(UIRL,ICENSUS) + &
               UTSO2C(UIRH,ICENSUS) + &
               UTSO2C(UIGF,ICENSUS) + &
               UTSO2C(UIGI,ICENSUS) + &
               UTSO2C(UIGC,ICENSUS) + &
               UTSO2C(UIDG,ICENSUS) + &
               UTSO2C(UIDD,ICENSUS) + &
               UTSO2C(UIUF,ICENSUS)) * TSO2_OSH_BY_OLCL(ICENSUS,ISO2,1)
          END IF
         END DO
       END IF
!     if (curitr .gt. maxitr)write(6,3231) curiyr+1989,icensus,  &
!             (UTSO2C(UIDS,ICENSUS) + &
!              UTSO2C(UIRL,ICENSUS) + &
!              UTSO2C(UIRH,ICENSUS) + &
!              UTSO2C(UIGF,ICENSUS) + &
!              UTSO2C(UIGI,ICENSUS) + &
!              UTSO2C(UIGC,ICENSUS) + &
!              UTSO2C(UIDG,ICENSUS) + &
!              UTSO2C(UIDD,ICENSUS)) * 0.001
!3231 format(1h  ,'!so2olcl',i4,i3,13f10.3)
      END DO
!     if (curitr .gt. maxitr)write(6,3232) curiyr+1989,curitr,(so2othcl(curiyr,iso2) * 0.001,iso2 = 1 , 12),   &
!                                          so2other(curiyr,1) * 0.001,  &
!                                          so2other(curiyr,2) * 0.001
!3232 format(1h  ,'!so2olcl',i4,i3,14f8.3)
!EMISSIONS BY CENSUS DIVISION
!DO IPOL=1 , MNPOLLUT
!WRITE(22,6767) IPOL,EMELC(MNUMCR,IPOL,CURIYR),
!+   (EMEL(IFL,IPOL,CURIYR),IFL=1,4)
!6767  FORMAT(1H ,'EMISSIONS,TL,NG,OL,CL,RN',I5,5F12.3)
!END DO
!
!
!
!NERC REGION OUTPUT
!
      ICR = 3
!
      DO IRG = 1 , MNUMNR - 1
!
!        SKIP GENERATION BY FUEL TYPE FOR ALASKA AND HAWAII, WHICH WAS DETERMINED ABOVE
!        INCLUDE NONTRADITIONAL COGEN IN AEO03
!
         IF (IRG .LE. (UNRGNS)) THEN
!
            CALL GETIN(1,IRG)

!           WRITE(6,3977) CURIRUN, CURIYR+1989, CURITR, IRG, EEITAJ(1), EEITAJ(2), EEITAJ(3)
!3977       FORMAT(1X,"UTIL_03129_EEITAJ_GET",4(":",I4),3(":",F12.3))

!
            UGNCLNR(1,IRG,CURIYR) =                                   &
               (UQFGENN(UIB1,IRG,1) + UQFGENN(UIB1,IRG,2) +           &
                UQFGENN(UIB2,IRG,1) + UQFGENN(UIB2,IRG,2) +           &
                UQFGENN(UIB3,IRG,1) + UQFGENN(UIB3,IRG,2) +           &
                UQFGENN(UIB4,IRG,1) + UQFGENN(UIB4,IRG,2) +           &
                UQFGENN(UIB5,IRG,1) + UQFGENN(UIB5,IRG,2) +           &
                UQFGENN(UIB6,IRG,1) + UQFGENN(UIB6,IRG,2) +           &
                UQFGENN(UIB7,IRG,1) + UQFGENN(UIB7,IRG,2) +           &
                UQFGENN(UIB8,IRG,1) + UQFGENN(UIB8,IRG,2) +           &
                UQFGENN(UIC1,IRG,1) + UQFGENN(UIC1,IRG,2) +           &
                UQFGENN(UIC2,IRG,1) + UQFGENN(UIC2,IRG,2) +           &
                UQFGENN(UIC3,IRG,1) + UQFGENN(UIC3,IRG,2) +           &
                UQFGENN(UIC4,IRG,1) + UQFGENN(UIC4,IRG,2) +           &
                UQFGENN(UIC5,IRG,1) + UQFGENN(UIC5,IRG,2) +           &
                UQFGENN(UIC6,IRG,1) + UQFGENN(UIC6,IRG,2) +           &
                UQFGENN(UIC7,IRG,1) + UQFGENN(UIC7,IRG,2) +           &
                UQFGENN(UIC8,IRG,1) + UQFGENN(UIC8,IRG,2) +           &
                UQFGENN(UIC9,IRG,1) + UQFGENN(UIC9,IRG,2) +           &
                UQFGENN(UICX,IRG,1) + UQFGENN(UICX,IRG,2) +           &
                UQFGENN(UICY,IRG,1) + UQFGENN(UICY,IRG,2) +           &
                UQFGENN(UICZ,IRG,1) + UQFGENN(UICZ,IRG,2) +           &
                UQFGENN(UIH1,IRG,1) + UQFGENN(UIH1,IRG,2) +           &
                UQFGENN(UIH2,IRG,1) + UQFGENN(UIH2,IRG,2) +           &
                UQFGENN(UIH3,IRG,1) + UQFGENN(UIH3,IRG,2) +           &
                UQFGENN(UIH4,IRG,1) + UQFGENN(UIH4,IRG,2) +           &
                UQFGENN(UIH5,IRG,1) + UQFGENN(UIH5,IRG,2) +           &
                UQFGENN(UIH6,IRG,1) + UQFGENN(UIH6,IRG,2) +           &
                UQFGENN(UIH7,IRG,1) + UQFGENN(UIH7,IRG,2) +           &
                UQFGENN(UIH8,IRG,1) + UQFGENN(UIH8,IRG,2) +           &
                UQFGENN(UIH9,IRG,1) + UQFGENN(UIH9,IRG,2) +           &
                UQFGENN(UIHA,IRG,1) + UQFGENN(UIHA,IRG,2) +           &
                UQFGENN(UIHB,IRG,1) + UQFGENN(UIHB,IRG,2) +           &
                UQFGENN(UIHC,IRG,1) + UQFGENN(UIHC,IRG,2) +           &
                UQFGENN(UIPC,IRG,1) + UQFGENN(UIPC,IRG,2) +           &
                UQFGENN(UIIG,IRG,1) + UQFGENN(UIIG,IRG,2) +           &
                UQFGENN(UIPQ,IRG,1) + UQFGENN(UIPQ,IRG,2) +           &
                UQFGENN(UIIS,IRG,1) + UQFGENN(UIIS,IRG,2) ) * FACT2
            UGNCLNR(2,IRG,CURIYR) =                  &
               (UQFGENN(UIB1,IRG,3) +                &
                UQFGENN(UIB2,IRG,3) +                &
                UQFGENN(UIB3,IRG,3) +                &
                UQFGENN(UIB4,IRG,3) +                &
                UQFGENN(UIB5,IRG,3) +                &
                UQFGENN(UIB6,IRG,3) +                &
                UQFGENN(UIB7,IRG,3) +                &
                UQFGENN(UIB8,IRG,3) +                &
                UQFGENN(UIC1,IRG,3) +                &
                UQFGENN(UIC2,IRG,3) +                &
                UQFGENN(UIC3,IRG,3) +                &
                UQFGENN(UIC4,IRG,3) +                &
                UQFGENN(UIC5,IRG,3) +                &
                UQFGENN(UIC6,IRG,3) +                &
                UQFGENN(UIC7,IRG,3) +                &
                UQFGENN(UIC8,IRG,3) +                &
                UQFGENN(UIC9,IRG,3) +                &
                UQFGENN(UICX,IRG,3) +                &
                UQFGENN(UICY,IRG,3) +                &
                UQFGENN(UICZ,IRG,3) +                &
                UQFGENN(UIH1,IRG,3) +                &
                UQFGENN(UIH2,IRG,3) +                &
                UQFGENN(UIH3,IRG,3) +                &
                UQFGENN(UIH4,IRG,3) +                &
                UQFGENN(UIH5,IRG,3) +                &
                UQFGENN(UIH6,IRG,3) +                &
                UQFGENN(UIH7,IRG,3) +                &
                UQFGENN(UIH8,IRG,3) +                &
                UQFGENN(UIH9,IRG,3) +                &
                UQFGENN(UIHA,IRG,3) +                &
                UQFGENN(UIHB,IRG,3) +                &
                UQFGENN(UIHC,IRG,3) +                &
                UQFGENN(UIPC,IRG,3) +                &
                UQFGENN(UIIG,IRG,3) +                &
                UQFGENN(UIPQ,IRG,3) +                &
                UQFGENN(UIIS,IRG,3) ) * FACT2
      UGNDSNR(1,IRG,CURIYR) =(UQFGENN(UIDS,IRG,1) + UQFGENN(UIDS,IRG,2)) * FACT2
      UGNDSNR(2,IRG,CURIYR) = UQFGENN(UIDS,IRG,3) * FACT2
      UGNRLNR(1,IRG,CURIYR) =(UQFGENN(UIRL,IRG,1) + UQFGENN(UIRL,IRG,2)) * FACT2
      UGNRLNR(2,IRG,CURIYR) = UQFGENN(UIRL,IRG,3) * FACT2
      UGNRHNR(1,IRG,CURIYR) =(UQFGENN(UIRH,IRG,1) + UQFGENN(UIRH,IRG,2)) * FACT2
      UGNRHNR(2,IRG,CURIYR) = UQFGENN(UIRH,IRG,3) * FACT2
      UGNGFNR(1,IRG,CURIYR) =(UQFGENN(UIGF,IRG,1) + UQFGENN(UIGF,IRG,2)) * FACT2
      UGNGFNR(2,IRG,CURIYR) = UQFGENN(UIGF,IRG,3) * FACT2
      UGNGINR(1,IRG,CURIYR) =(UQFGENN(UIGI,IRG,1) + UQFGENN(UIGI,IRG,2)) * FACT2
      UGNGINR(2,IRG,CURIYR) = UQFGENN(UIGI,IRG,3) * FACT2
      UGNGCNR(1,IRG,CURIYR) =(UQFGENN(UIGC,IRG,1) + UQFGENN(UIGC,IRG,2)) * FACT2
      UGNGCNR(2,IRG,CURIYR) = UQFGENN(UIGC,IRG,3) * FACT2
      UGNURNR(1,IRG,CURIYR) =(UQFGENN(UIUF,IRG,1) + UQFGENN(UIUF,IRG,2)) * FACT2
      UGNURNR(2,IRG,CURIYR) = UQFGENN(UIUF,IRG,3) * FACT2
      UGNDGNR(1,IRG,CURIYR) =(UQFGENN(UIDG,IRG,1) + UQFGENN(UIDG,IRG,2)) * FACT2
      UGNDGNR(2,IRG,CURIYR) = UQFGENN(UIDG,IRG,3) * FACT2
      UGNDDNR(1,IRG,CURIYR) =(UQFGENN(UIDD,IRG,1) + UQFGENN(UIDD,IRG,2)) * FACT2
      UGNDDNR(2,IRG,CURIYR) = UQFGENN(UIDD,IRG,3) * FACT2
!  RENEWABLES
      UGNHYNR(1,IRG,CURIYR) =(UQPGENN(UIHYC,IRG,1) + UQPGENN(UIHYC,IRG,2)) * FACT2
      UGNHYNR(2,IRG,CURIYR) = UQPGENN(UIHYC,IRG,3) * FACT2
      UGNPSNR(1,IRG,CURIYR) =(UQPGENN(UIHYR,IRG,1) + UQPGENN(UIHYR,IRG,2)) * FACT2
      UGNPSNR(2,IRG,CURIYR) = UQPGENN(UIHYR,IRG,3) * FACT2
      UGNSDNR(1,IRG,CURIYR) =(UQPGENN(UIDST,IRG,1) + UQPGENN(UIDST,IRG,2)) * FACT2
      UGNSDNR(2,IRG,CURIYR) = UQPGENN(UIDST,IRG,3) * FACT2
      UGNGENR(1,IRG,CURIYR) =(UQPGENN(UIGTH,IRG,1) + UQPGENN(UIGTH,IRG,2)) * FACT2
      UGNGENR(2,IRG,CURIYR) = UQPGENN(UIGTH,IRG,3) * FACT2
      UGNMSNR(1,IRG,CURIYR) =(UQPGENN(UIMSW,IRG,1) + UQPGENN(UIMSW,IRG,2)) * FACT2
      UGNMSNR(2,IRG,CURIYR) = UQPGENN(UIMSW,IRG,3) * FACT2
      UGNWDNR(1,IRG,CURIYR) =(UQFGENN( UIWD,IRG,1) + UQFGENN( UIWD,IRG,2)) * FACT2
      UGNWDNR(2,IRG,CURIYR) = UQFGENN(UIWD,IRG,3) * FACT2

    WRITE(22,2394) CURIYR+UHBSYR,CURITR,IRG, UGNMSNR(1,IRG,CURIYR),UGNMSNR(2,IRG,CURIYR),UQPGENN(UIMSW,IRG,1),UQPGENN(UIMSW,IRG,2),UQPGENN(UIMSW,IRG,3),FACT2
2394  FORMAT (1X,"CHK UGNMSNR ",3(":",I4),6(":",F6.3))

!  ACCOUNT FOR BIOMASS COFIRING IN FOSSIL PLANTS
      UGNCFNR(1,IRG,CURIYR) = UGNWDNR(1,IRG,CURIYR) - &
          (UQPGENN(UIBMS,IRG,1) + UQPGENN(UIBMS,IRG,2)) * FACT2
      UGNCFNR(2,IRG,CURIYR) = UGNWDNR(2,IRG,CURIYR) - UQPGENN(UIBMS,IRG,3) * FACT2 + &
         (UQFGENN(UIWD,IRG,4) - UQPGENN(UIBMS,IRG,4)) * FACT2
!  INSURE THAT ROUNDING DOES NOT GIVE NEGATIVE COFIRING
      UGNCFNR(1,IRG,CURIYR) = MAX(UGNCFNR(1,IRG,CURIYR),0.0)
      UGNCFNR(2,IRG,CURIYR) = MAX(UGNCFNR(2,IRG,CURIYR),0.0)
      UGNCFNR(1,MNUMNR,CURIYR) = UGNCFNR(1,MNUMNR,CURIYR) + UGNCFNR(1,IRG,CURIYR)
      UGNCFNR(2,MNUMNR,CURIYR) = UGNCFNR(2,MNUMNR,CURIYR) + UGNCFNR(2,IRG,CURIYR)
      UGNSONR(1,IRG,CURIYR) =(UQPGENN(UISTH,IRG,1) + UQPGENN(UISTH,IRG,2)) * FACT2
      UGNSONR(2,IRG,CURIYR) = UQPGENN(UISTH,IRG,3) * FACT2
      UGNPVNR(1,IRG,CURIYR) =(UQPGENN(UISPV,IRG,1) + UQPGENN(UISPV,IRG,2)) * FACT2
      UGNPTNR(1,IRG,CURIYR) =(UQPGENN(UIPVT,IRG,1) + UQPGENN(UIPVT,IRG,2)) * FACT2
      UGNPVNR(2,IRG,CURIYR) = UQPGENN(UISPV,IRG,3) * FACT2
      UGNPTNR(2,IRG,CURIYR) = UQPGENN(UIPVT,IRG,3) * FACT2
      UGNWNNR(1,IRG,CURIYR) =(UQPGENN(UIWND,IRG,1) + UQPGENN(UIWND,IRG,2)) * FACT2
      UGNWLNR(1,IRG,CURIYR) =(UQPGENN(UIWNL,IRG,1) + UQPGENN(UIWNL,IRG,2)) * FACT2
      UGNWFNR(1,IRG,CURIYR) =(UQPGENN(UIWFS,IRG,1) + UQPGENN(UIWFS,IRG,2)) * FACT2
      UGNWNNR(2,IRG,CURIYR) = UQPGENN(UIWND,IRG,3) * FACT2
      UGNWLNR(2,IRG,CURIYR) = UQPGENN(UIWNL,IRG,3) * FACT2
      UGNWFNR(2,IRG,CURIYR) = UQPGENN(UIWFS,IRG,3) * FACT2
      END IF                             ! END IF SKIPPING ALASKA/HAWAII
      UGNHONR(1,IRG,CURIYR) = UGNHYNR(1,IRG,CURIYR) + &
                              UGNGENR(1,IRG,CURIYR) + &
                              UGNMSNR(1,IRG,CURIYR) + &
                              UGNWDNR(1,IRG,CURIYR) + &
                              UGNSONR(1,IRG,CURIYR) + &
                              UGNPVNR(1,IRG,CURIYR) + &
                              UGNPTNR(1,IRG,CURIYR) + &
                              UGNWNNR(1,IRG,CURIYR) + &
                              UGNWLNR(1,IRG,CURIYR) + &
                              UGNWFNR(1,IRG,CURIYR)
      UGNHONR(2,IRG,CURIYR) = UGNHYNR(2,IRG,CURIYR) + &
                              UGNGENR(2,IRG,CURIYR) + &
                              UGNMSNR(2,IRG,CURIYR) + &
                              UGNWDNR(2,IRG,CURIYR) + &
                              UGNSONR(2,IRG,CURIYR) + &
                              UGNPVNR(2,IRG,CURIYR) + &
                              UGNPTNR(2,IRG,CURIYR) + &
                              UGNWNNR(2,IRG,CURIYR) + &
                              UGNWLNR(2,IRG,CURIYR) + &
                              UGNWFNR(2,IRG,CURIYR)
!
      UGNTLNR(1,IRG,CURIYR) = UGNCLNR(1,IRG,CURIYR) + &
                              UGNDSNR(1,IRG,CURIYR) + &
                              UGNRLNR(1,IRG,CURIYR) + &
                              UGNRHNR(1,IRG,CURIYR) + &
                              UGNGFNR(1,IRG,CURIYR) + &
                              UGNGINR(1,IRG,CURIYR) + &
                              UGNGCNR(1,IRG,CURIYR) + &
                              UGNURNR(1,IRG,CURIYR) + &
                              UGNDGNR(1,IRG,CURIYR) + &
                              UGNDDNR(1,IRG,CURIYR) + &
                              UGNHONR(1,IRG,CURIYR) + &
                              UGNPSNR(1,IRG,CURIYR)
!
      UGNTLNR(2,IRG,CURIYR) = UGNCLNR(2,IRG,CURIYR) + &
                              UGNDSNR(2,IRG,CURIYR) + &
                              UGNRLNR(2,IRG,CURIYR) + &
                              UGNRHNR(2,IRG,CURIYR) + &
                              UGNGFNR(2,IRG,CURIYR) + &
                              UGNGINR(2,IRG,CURIYR) + &
                              UGNGCNR(2,IRG,CURIYR) + &
                              UGNURNR(2,IRG,CURIYR) + &
                              UGNDGNR(2,IRG,CURIYR) + &
                              UGNDDNR(2,IRG,CURIYR) + &
                              UGNHONR(2,IRG,CURIYR) + &
                              UGNPSNR(2,IRG,CURIYR)
!RPS GENERATION AND CREDITS
      IF (IRG .LE. UNRGNS) THEN
      UCRCLNR(1,IRG,CURIYR) = UGNRPSN(UICAS,IRG) + UGNRPSN(UICOQ,IRG)
      UCRNGNR(1,IRG,CURIYR) = UGNRPSN(UIACS,IRG) + UGNRPSN(UIACC,IRG) + UGNRPSN(UICCG,IRG) + UGNRPSN(UICCX,IRG)
      UCRNUNR(1,IRG,CURIYR) = UGNRPSN(UICNU,IRG) + UGNRPSN(UIANC,IRG) + UGNRPSN(UISMR,IRG)
      UCRHYNR(1,IRG,CURIYR) = UGNRPSN(UIHYC,IRG)
      UCRGENR(1,IRG,CURIYR) = UGNRPSN(UIGTH,IRG)
      UCRMSNR(1,IRG,CURIYR) = UGNRPSN(UIMSW,IRG)
      UCRWDNR(1,IRG,CURIYR) = UGNRPSN(UIBMS,IRG)
      UCRSONR(1,IRG,CURIYR) = UGNRPSN(UISTH,IRG)
      UCRPVNR(1,IRG,CURIYR) = UGNRPSN(UISPV,IRG)
      UCRPTNR(1,IRG,CURIYR) = UGNRPSN(UIPVT,IRG)
      UCRWNNR(1,IRG,CURIYR) = UGNRPSN(UIWND,IRG)
      UCRWLNR(1,IRG,CURIYR) = UGNRPSN(UIWNL,IRG)
      UCRWFNR(1,IRG,CURIYR) = UGNRPSN(UIWFS,IRG)
      IF (UPRNWCAS .GT. 0)  &
        UCRCFNR(1,IRG,CURIYR) = UGNCFNR(1,IRG,CURIYR) + UGNCFNR(2,IRG,CURIYR)
      UCRCLNR(2,IRG,CURIYR) = UGNRPSN(UICAS,IRG) * UPRNWSHR(WIIS) + &
                              UGNRPSN(UICOQ,IRG) * UPRNWSHR(WIPQ)
      UCRNGNR(2,IRG,CURIYR) = UGNRPSN(UIACS,IRG) * UPRNWSHR(WICS) +  &
                              UGNRPSN(UIACC,IRG) * UPRNWSHR(WIAC) +  &
                              UGNRPSN(UICCG,IRG) * UPRNWSHR(WIEC) +  &
                              UGNRPSN(UICCX,IRG) * UPRNWSHR(WIEC) +  &
                              UGNRPSN(UIACT,IRG) * UPRNWSHR(WIAT) +  &
                              UGNRPSN(UICTG,IRG) * UPRNWSHR(WIET) +  &
                              UGNRPSN(UICTX,IRG) * UPRNWSHR(WIET)
      UCRNUNR(2,IRG,CURIYR) = UGNRPSN(UICNU,IRG) * UPRNWSHR(WICN) + UGNRPSN(UIANC,IRG) * UPRNWSHR(WIAN) + UGNRPSN(UISMR,IRG) * UPRNWSHR(WISM)
      UCRHYNR(2,IRG,CURIYR) = UGNRPSN(UIHYC,IRG) * UPRNWSHR(WIHY)
      UCRGENR(2,IRG,CURIYR) = UGNRPSN(UIGTH,IRG) * UPRNWSHR(WIGT)
      UCRMSNR(2,IRG,CURIYR) = UGNRPSN(UIMSW,IRG) * UPRNWSHR(WIMS)
      UCRWDNR(2,IRG,CURIYR) = UGNRPSN(UIBMS,IRG) * UPRNWSHR(WIWD)
      UCRSONR(2,IRG,CURIYR) = UGNRPSN(UISTH,IRG) * UPRNWSHR(WISO)
      UCRPVNR(2,IRG,CURIYR) = UGNRPSN(UISPV,IRG) * UPRNWSHR(WIPV)
      UCRPTNR(2,IRG,CURIYR) = UGNRPSN(UIPVT,IRG) * UPRNWSHR(WIPT)

!     ACCOUNT FOR BONUS CREDITS FOR END USE PV, IF APPROPRIATE

      IF (UCSPVSHR(IRG) .NE. UPRNWSHR(WIPV)) UCRPVNR(2,IRG,CURIYR) = UCRPVNR(2,IRG,CURIYR) + (ECRPSRG(WIPV) + ECRPSRO(WIPV)) * (UCSPVSHR(IRG) - UPRNWSHR(WIPV))
      UCRWNNR(2,IRG,CURIYR) = UGNRPSN(UIWND,IRG) * UPRNWSHR(WIWN)
      UCRWLNR(2,IRG,CURIYR) = UGNRPSN(UIWNL,IRG) * UPRNWSHR(WIWL)

!     ACCOUNT FOR BONUS CREDITS FOR END USE WIND, IF APPROPRIATE

      IF (UCWNDSHR(IRG) .NE. UPRNWSHR(WIWN)) UCRWNNR(2,IRG,CURIYR) = UCRWNNR(2,IRG,CURIYR) + (ECRPSRG(WIWN) + ECRPSRO(WIWN)) * (UCWNDSHR(IRG) - UPRNWSHR(WIWN))
      UCRWFNR(2,IRG,CURIYR) = UGNRPSN(UIWFS,IRG) * UPRNWSHR(WIWF)
      IF (UPRNWCAS .GT. 0)  &
      UCRCFNR(2,IRG,CURIYR) = (UGNCFNR(1,IRG,CURIYR) + UGNCFNR(2,IRG,CURIYR)) * UPRNWSHR(WIWD)
!
      UCRTLNR(1,IRG,CURIYR) = UCRCLNR(1,IRG,CURIYR) + &
                              UCRNGNR(1,IRG,CURIYR) + &
                              UCRNUNR(1,IRG,CURIYR) + &
                              UCRHYNR(1,IRG,CURIYR) + &
                              UCRGENR(1,IRG,CURIYR) + &
                              UCRMSNR(1,IRG,CURIYR) + &
                              UCRWDNR(1,IRG,CURIYR) + &
                              UCRSONR(1,IRG,CURIYR) + &
                              UCRPVNR(1,IRG,CURIYR) + &
                              UCRPTNR(1,IRG,CURIYR) + &
                              UCRWNNR(1,IRG,CURIYR) + &
                              UCRWLNR(1,IRG,CURIYR) + &
                              UCRWFNR(1,IRG,CURIYR) + &
                              UCRCFNR(1,IRG,CURIYR)
      UCRTLNR(2,IRG,CURIYR) = UCRCLNR(2,IRG,CURIYR) + &
                              UCRNGNR(2,IRG,CURIYR) + &
                              UCRNUNR(2,IRG,CURIYR) + &
                              UCRHYNR(2,IRG,CURIYR) + &
                              UCRGENR(2,IRG,CURIYR) + &
                              UCRMSNR(2,IRG,CURIYR) + &
                              UCRWDNR(2,IRG,CURIYR) + &
                              UCRSONR(2,IRG,CURIYR) + &
                              UCRPVNR(2,IRG,CURIYR) + &
                              UCRPTNR(2,IRG,CURIYR) + &
                              UCRWNNR(2,IRG,CURIYR) + &
                              UCRWLNR(2,IRG,CURIYR) + &
                              UCRWFNR(2,IRG,CURIYR) + &
                              UCRCFNR(2,IRG,CURIYR)
!PTC GENERATION AND PAYMENTS
      UPTCLNR(IRG,CURIYR) = UGNPTCN(UICNC,IRG) + UGNPTCN(UICAV,IRG) + UGNPTCN(UICOQ,IRG) + UGNPTCN(UICAS,IRG)
      UPTNGNR(IRG,CURIYR) = UGNPTCN(UICTX,IRG) + UGNPTCN(UIACT,IRG) + UGNPTCN(UICCX,IRG) +  &
                            UGNPTCN(UIACC,IRG) + UGNPTCN(UIACS,IRG) + UGNPTCN(UIFCG,IRG)
      UPTNUNR(IRG,CURIYR) = UGNPTCN(UICNU,IRG) + UGNPTCN(UIANC,IRG) +  UGNPTCN(UISMR,IRG)
      UPTHYNR(IRG,CURIYR) = UGNPTCN(UIHYC,IRG)
      UPTGENR(IRG,CURIYR) = UGNPTCN(UIGTH,IRG)
      UPTMSNR(IRG,CURIYR) = UGNPTCN(UIMSW,IRG)
      UPTWDNR(IRG,CURIYR) = UGNPTCN(UIBMS,IRG)
      UPTSONR(IRG,CURIYR) = UGNPTCN(UISTH,IRG)
      UPTPVNR(IRG,CURIYR) = UGNPTCN(UISPV,IRG)
      UPTPTNR(IRG,CURIYR) = UGNPTCN(UIPVT,IRG)
      UPTWNNR(IRG,CURIYR) = UGNPTCN(UIWND,IRG)
      UPTWLNR(IRG,CURIYR) = UGNPTCN(UIWNL,IRG)
      UPTWFNR(IRG,CURIYR) = UGNPTCN(UIWFS,IRG)
!     UPTCFNR(IRG,CURIYR) =
      UPTTLNR(IRG,CURIYR) = UPTCLNR(IRG,CURIYR) + &
                            UPTNGNR(IRG,CURIYR) + &
                            UPTNUNR(IRG,CURIYR) + &
                            UPTHYNR(IRG,CURIYR) + &
                            UPTGENR(IRG,CURIYR) + &
                            UPTMSNR(IRG,CURIYR) + &
                            UPTWDNR(IRG,CURIYR) + &
!                           UPTCFNR(IRG,CURIYR) + &
                            UPTSONR(IRG,CURIYR) + &
                            UPTPVNR(IRG,CURIYR) + &
                            UPTPTNR(IRG,CURIYR) + &
                            UPTWNNR(IRG,CURIYR) + &
                            UPTWLNR(IRG,CURIYR) + &
                            UPTWFNR(IRG,CURIYR)
      UPTCCST(IRG,CURIYR) = UPYPTCN(UICNC,IRG) + UPYPTCN(UICAV,IRG) + UPYPTCN(UICOQ,IRG) +   + UPYPTCN(UICAS,IRG) +  &
                            UPYPTCN(UICTX,IRG) + UPYPTCN(UIACT,IRG) + UPYPTCN(UICCX,IRG) +  &
                            UPYPTCN(UIACC,IRG) + UPYPTCN(UIACS,IRG) + UPYPTCN(UIFCG,IRG) +  &
                            UPYPTCN(UICNU,IRG) +  &
                            UPYPTCN(UIANC,IRG) +  &
                            UPYPTCN(UISMR,IRG) +  &
                            UPYPTCN(UIHYC,IRG) +  &
                            UPYPTCN(UIGTH,IRG) +  &
                            UPYPTCN(UIMSW,IRG) +  &
                            UPYPTCN(UIBMS,IRG) +  &
                            UPYPTCN(UISTH,IRG) +  &
                            UPYPTCN(UISPV,IRG) +  &
                            UPYPTCN(UIPVT,IRG) +  &
                            UPYPTCN(UIWND,IRG) +  &
                            UPYPTCN(UIWNL,IRG) +  &
                            UPYPTCN(UIWFS,IRG)
!     if (curitr .gt. maxitr)write(6,2222) curiyr+1989,irg,curitr,  &
!     UPTTLNR(IRG,CURIYR) , UGNPTCN(UICNC,IRG) + UGNPTCN(UICAV,IRG) + UGNPTCN(UICOQ,IRG) ,+ UGNPTCN(UICAS,IRG) ,  &
!                           UGNPTCN(UICTX,IRG) + UGNPTCN(UIACT,IRG) + UGNPTCN(UICCX,IRG) +  &
!                           UGNPTCN(UIACC,IRG) + UGNPTCN(UIACS,IRG) + UGNPTCN(UIFCG,IRG) ,  &
!                           UGNPTCN(UIANC,IRG) ,  &
!                           UGNPTCN(UIHYC,IRG) ,  &
!                           UGNPTCN(UIGTH,IRG) ,  &
!                           UGNPTCN(UIMSW,IRG) ,  &
!                           UGNPTCN(UIBMS,IRG) ,  &
!                           UGNPTCN(UISTH,IRG) ,  &
!                           UGNPTCN(UISPV,IRG) ,  &
!                           UGNPTCN(UIPVT,IRG) ,  &
!                           UGNPTCN(UIWND,IRG) ,  &
!                           UGNPTCN(UIWNL,IRG)
!2222 format(1h ,'!genptc',i4,i4,i3,11f10.2)
!     if (curitr .gt. maxitr)write(6,3333) curiyr+1989,irg,curitr,  &
!     UPTCCST(IRG,CURIYR) , UPYPTCN(UICNC,IRG) + UPYPTCN(UICAV,IRG) + UPYPTCN(UICOQ,IRG) , + UPYPTCN(UICAS,IRG) , &
!                           UPYPTCN(UICTX,IRG) + UPYPTCN(UIACT,IRG) + UPYPTCN(UICCX,IRG) +  &
!                           UPYPTCN(UIACC,IRG) + UPYPTCN(UIACS,IRG) + UPYPTCN(UIFCG,IRG) ,  &
!                           UPYPTCN(UIANC,IRG) ,  &
!                           UPYPTCN(UIHYC,IRG) ,  &
!                           UPYPTCN(UIGTH,IRG) ,  &
!                           UPYPTCN(UIMSW,IRG) ,  &
!                           UPYPTCN(UIBMS,IRG) ,  &
!                           UPYPTCN(UISTH,IRG) ,  &
!                           UPYPTCN(UISPV,IRG) ,  &
!                           UPYPTCN(UIPVT,IRG) ,  &
!                           UPYPTCN(UIWNL,IRG) ,  &
!                           UPYPTCN(UIWND,IRG)
!3333 format(1h ,'!cstptc',i4,i4,i3,11f10.2)
      END IF                        ! IRG LE (UNRGNS)
!
!     FUEL CONSUMPTION
!     SKIP FUEL USE FOR ALASKA/HAWAII, WHICH WAS DETERMINED ABOVE
!     INCLUDE NONTRADITIONAL COGEN FOR AEO03
!
      IF (IRG .LE. UNRGNS) THEN
         UFLCLNR(1,IRG,CURIYR) = &
            (UQFCONN(UIB1,IRG,1) + UQFCONN(UIB1,IRG,2) +           &
             UQFCONN(UIB2,IRG,1) + UQFCONN(UIB2,IRG,2) +           &
             UQFCONN(UIB3,IRG,1) + UQFCONN(UIB3,IRG,2) +           &
             UQFCONN(UIB4,IRG,1) + UQFCONN(UIB4,IRG,2) +           &
             UQFCONN(UIB5,IRG,1) + UQFCONN(UIB5,IRG,2) +           &
             UQFCONN(UIB6,IRG,1) + UQFCONN(UIB6,IRG,2) +           &
             UQFCONN(UIB7,IRG,1) + UQFCONN(UIB7,IRG,2) +           &
             UQFCONN(UIB8,IRG,1) + UQFCONN(UIB8,IRG,2) +           &
             UQFCONN(UIC1,IRG,1) + UQFCONN(UIC1,IRG,2) +           &
             UQFCONN(UIC2,IRG,1) + UQFCONN(UIC2,IRG,2) +           &
             UQFCONN(UIC3,IRG,1) + UQFCONN(UIC3,IRG,2) +           &
             UQFCONN(UIC4,IRG,1) + UQFCONN(UIC4,IRG,2) +           &
             UQFCONN(UIC5,IRG,1) + UQFCONN(UIC5,IRG,2) +           &
             UQFCONN(UIC6,IRG,1) + UQFCONN(UIC6,IRG,2) +           &
             UQFCONN(UIC7,IRG,1) + UQFCONN(UIC7,IRG,2) +           &
             UQFCONN(UIC8,IRG,1) + UQFCONN(UIC8,IRG,2) +           &
             UQFCONN(UIC9,IRG,1) + UQFCONN(UIC9,IRG,2) +           &
             UQFCONN(UICX,IRG,1) + UQFCONN(UICX,IRG,2) +           &
             UQFCONN(UICY,IRG,1) + UQFCONN(UICY,IRG,2) +           &
             UQFCONN(UICZ,IRG,1) + UQFCONN(UICZ,IRG,2) +           &
             UQFCONN(UIH1,IRG,1) + UQFCONN(UIH1,IRG,2) +           &
             UQFCONN(UIH2,IRG,1) + UQFCONN(UIH2,IRG,2) +           &
             UQFCONN(UIH3,IRG,1) + UQFCONN(UIH3,IRG,2) +           &
             UQFCONN(UIH4,IRG,1) + UQFCONN(UIH4,IRG,2) +           &
             UQFCONN(UIH5,IRG,1) + UQFCONN(UIH5,IRG,2) +           &
             UQFCONN(UIH6,IRG,1) + UQFCONN(UIH6,IRG,2) +           &
             UQFCONN(UIH7,IRG,1) + UQFCONN(UIH7,IRG,2) +           &
             UQFCONN(UIH8,IRG,1) + UQFCONN(UIH8,IRG,2) +           &
             UQFCONN(UIH9,IRG,1) + UQFCONN(UIH9,IRG,2) +           &
             UQFCONN(UIHA,IRG,1) + UQFCONN(UIHA,IRG,2) +           &
             UQFCONN(UIHB,IRG,1) + UQFCONN(UIHB,IRG,2) +           &
             UQFCONN(UIHC,IRG,1) + UQFCONN(UIHC,IRG,2) +           &
             UQFCONN(UIPC,IRG,1) + UQFCONN(UIPC,IRG,2) +           &
             UQFCONN(UIIG,IRG,1) + UQFCONN(UIIG,IRG,2) +           &
             UQFCONN(UIPQ,IRG,1) + UQFCONN(UIPQ,IRG,2) +           &
             UQFCONN(UIIS,IRG,1) + UQFCONN(UIIS,IRG,2) ) * FACT2
         UFLCLNR(2,IRG,CURIYR) =                  &
            (UQFCONN(UIB1,IRG,3) + UQFCONN(UIB1,IRG,4) +          &
             UQFCONN(UIB2,IRG,3) + UQFCONN(UIB2,IRG,4) +          &
             UQFCONN(UIB3,IRG,3) + UQFCONN(UIB3,IRG,4) +          &
             UQFCONN(UIB4,IRG,3) + UQFCONN(UIB4,IRG,4) +          &
             UQFCONN(UIB5,IRG,3) + UQFCONN(UIB5,IRG,4) +          &
             UQFCONN(UIB6,IRG,3) + UQFCONN(UIB6,IRG,4) +          &
             UQFCONN(UIB7,IRG,3) + UQFCONN(UIB7,IRG,4) +          &
             UQFCONN(UIB8,IRG,3) + UQFCONN(UIB8,IRG,4) +          &
             UQFCONN(UIC1,IRG,3) + UQFCONN(UIC1,IRG,4) +          &
             UQFCONN(UIC2,IRG,3) + UQFCONN(UIC2,IRG,4) +          &
             UQFCONN(UIC3,IRG,3) + UQFCONN(UIC3,IRG,4) +          &
             UQFCONN(UIC4,IRG,3) + UQFCONN(UIC4,IRG,4) +          &
             UQFCONN(UIC5,IRG,3) + UQFCONN(UIC5,IRG,4) +          &
             UQFCONN(UIC6,IRG,3) + UQFCONN(UIC6,IRG,4) +          &
             UQFCONN(UIC7,IRG,3) + UQFCONN(UIC7,IRG,4) +          &
             UQFCONN(UIC8,IRG,3) + UQFCONN(UIC8,IRG,4) +          &
             UQFCONN(UIC9,IRG,3) + UQFCONN(UIC9,IRG,4) +          &
             UQFCONN(UICX,IRG,3) + UQFCONN(UICX,IRG,4) +          &
             UQFCONN(UICY,IRG,3) + UQFCONN(UICY,IRG,4) +          &
             UQFCONN(UICZ,IRG,3) + UQFCONN(UICZ,IRG,4) +          &
             UQFCONN(UIH1,IRG,3) + UQFCONN(UIH1,IRG,4) +          &
             UQFCONN(UIH2,IRG,3) + UQFCONN(UIH2,IRG,4) +          &
             UQFCONN(UIH3,IRG,3) + UQFCONN(UIH3,IRG,4) +          &
             UQFCONN(UIH4,IRG,3) + UQFCONN(UIH4,IRG,4) +          &
             UQFCONN(UIH5,IRG,3) + UQFCONN(UIH5,IRG,4) +          &
             UQFCONN(UIH6,IRG,3) + UQFCONN(UIH6,IRG,4) +          &
             UQFCONN(UIH7,IRG,3) + UQFCONN(UIH7,IRG,4) +          &
             UQFCONN(UIH8,IRG,3) + UQFCONN(UIH8,IRG,4) +          &
             UQFCONN(UIH9,IRG,3) + UQFCONN(UIH9,IRG,4) +          &
             UQFCONN(UIHA,IRG,3) + UQFCONN(UIHA,IRG,4) +          &
             UQFCONN(UIHB,IRG,3) + UQFCONN(UIHB,IRG,4) +          &
             UQFCONN(UIHC,IRG,3) + UQFCONN(UIHC,IRG,4) +          &
             UQFCONN(UIPC,IRG,3) + UQFCONN(UIPC,IRG,4) +          &
             UQFCONN(UIIG,IRG,3) + UQFCONN(UIIG,IRG,4) +          &
             UQFCONN(UIPQ,IRG,3) + UQFCONN(UIPQ,IRG,4) +          &
             UQFCONN(UIIS,IRG,3) + UQFCONN(UIIS,IRG,4) ) * FACT2
      UFLDSNR(1,IRG,CURIYR) =(UQFCONN(UIDS,IRG,1) + UQFCONN(UIDS,IRG,2)) * FACT2
      UFLDSNR(2,IRG,CURIYR) =(UQFCONN(UIDS,IRG,3) + UQFCONN(UIDS,IRG,4)) * FACT2
      UFLRLNR(1,IRG,CURIYR) =(UQFCONN(UIRL,IRG,1) + UQFCONN(UIRL,IRG,2)) * FACT2
      UFLRLNR(2,IRG,CURIYR) =(UQFCONN(UIRL,IRG,3) + UQFCONN(UIRL,IRG,4)) * FACT2
      UFLRHNR(1,IRG,CURIYR) =(UQFCONN(UIRH,IRG,1) + UQFCONN(UIRH,IRG,2)) * FACT2
      UFLRHNR(2,IRG,CURIYR) =(UQFCONN(UIRH,IRG,3) + UQFCONN(UIRH,IRG,4)) * FACT2
!  UFLPCNR(1,IRG,CURIYR)=0.0
!  UFLPCNR(2,IRG,CURIYR)=0.0
      UFLGFNR(1,IRG,CURIYR) =(UQFCONN(UIGF,IRG,1) + UQFCONN(UIGF,IRG,2)) * FACT2
      UFLGFNR(2,IRG,CURIYR) =(UQFCONN(UIGF,IRG,3) + UQFCONN(UIGF,IRG,4)) * FACT2
      UFLGINR(1,IRG,CURIYR) =(UQFCONN(UIGI,IRG,1) + UQFCONN(UIGI,IRG,2)) * FACT2
      UFLGINR(2,IRG,CURIYR) =(UQFCONN(UIGI,IRG,3) + UQFCONN(UIGI,IRG,4)) * FACT2
      UFLGCNR(1,IRG,CURIYR) =(UQFCONN(UIGC,IRG,1) + UQFCONN(UIGC,IRG,2)) * FACT2
      UFLGCNR(2,IRG,CURIYR) =(UQFCONN(UIGC,IRG,3) + UQFCONN(UIGC,IRG,4)) * FACT2
!  UFLURNR(1,IRG,CURIYR)=(UQFCONN(UIUF,IRG,1) + UQFCONN(UIUF,IRG,2)) * FACT2
!  UFLURNR(2,IRG,CURIYR)= UQFCONN(UIUF,IRG,3) * FACT2
      UFLURNR(1,IRG,CURIYR) =(UQFGENN(UIUF,IRG,1) + UQFGENN(UIUF,IRG,2)) * NUCHRT * FACT3
      UFLURNR(2,IRG,CURIYR) = UQFGENN(UIUF,IRG,3) * NUCHRT * FACT3
      UFLDGNR(1,IRG,CURIYR) =(UQFCONN(UIDG,IRG,1) + UQFCONN(UIDG,IRG,2)) * FACT2
      UFLDGNR(2,IRG,CURIYR) =(UQFCONN(UIDG,IRG,3) + UQFCONN(UIDG,IRG,4)) * FACT2
      UFLDDNR(1,IRG,CURIYR) =(UQFCONN(UIDD,IRG,1) + UQFCONN(UIDD,IRG,2)) * FACT2
      UFLDDNR(2,IRG,CURIYR) =(UQFCONN(UIDD,IRG,3) + UQFCONN(UIDD,IRG,4)) * FACT2
      UFLGTNR(1,IRG,CURIYR) =(UQFCONN(UIGT,IRG,1) + UQFCONN(UIGT,IRG,2)) * FACT2
      UFLGTNR(2,IRG,CURIYR) =(UQFCONN(UIGT,IRG,3) + UQFCONN(UIGT,IRG,4)) * FACT2
      UFLWDNR(1,IRG,CURIYR) =(UQFCONN(UIWD,IRG,1) + UQFCONN(UIWD,IRG,2)) * FACT2
      UFLWDNR(2,IRG,CURIYR) =(UQFCONN(UIWD,IRG,3) + UQFCONN(UIWD,IRG,4)) * FACT2
      UFLMSNR(1,IRG,CURIYR) =(UQFCONN(UISW,IRG,1) + UQFCONN(UISW,IRG,2)) * FACT2
      UFLMSNR(2,IRG,CURIYR) =(UQFCONN(UISW,IRG,3) + UQFCONN(UISW,IRG,4)) * FACT2
      UFLPSNR(1,IRG,CURIYR) =(UQFCONN(UIPS,IRG,1) + UQFCONN(UIPS,IRG,2)) * FACT2
      UFLPSNR(2,IRG,CURIYR) =(UQFCONN(UIPS,IRG,3) + UQFCONN(UIPS,IRG,4)) * FACT2
      UFLHYNR(1,IRG,CURIYR) =(UQFCONN(UIWA,IRG,1) + UQFCONN(UIWA,IRG,2)  +     &
                              UQFCONN(UITI,IRG,1) + UQFCONN(UITI,IRG,2)) * FACT2
      UFLHYNR(2,IRG,CURIYR) =(UQFCONN(UIWA,IRG,3) + UQFCONN(UIWA,IRG,4)  +     &
                              UQFCONN(UITI,IRG,3) + UQFCONN(UITI,IRG,4)) * FACT2
      UFLWNNR(1,IRG,CURIYR) =(UQFCONN(UIWN,IRG,1) + UQFCONN(UIWN,IRG,2)  +     &
                              UQFCONN(UIWL,IRG,1) + UQFCONN(UIWL,IRG,2)) * FACT2
      UFLWNNR(2,IRG,CURIYR) =(UQFCONN(UIWN,IRG,3) + UQFCONN(UIWN,IRG,4)  +     &
                              UQFCONN(UIWL,IRG,3) + UQFCONN(UIWL,IRG,4)) * FACT2
      UFLWFNR(1,IRG,CURIYR) =(UQFCONN(UIWF,IRG,1) + UQFCONN(UIWF,IRG,2)) * FACT2
      UFLWFNR(2,IRG,CURIYR) =(UQFCONN(UIWF,IRG,3) + UQFCONN(UIWF,IRG,4)) * FACT2
      UFLSONR(1,IRG,CURIYR) =(UQFCONN(UISO,IRG,1) + UQFCONN(UISO,IRG,2)) * FACT2
      UFLSONR(2,IRG,CURIYR) =(UQFCONN(UISO,IRG,3) + UQFCONN(UISO,IRG,4)) * FACT2
      UFLPVNR(1,IRG,CURIYR) =(UQFCONN(UIPV,IRG,1) + UQFCONN(UIPV,IRG,2)  +     &
                              UQFCONN(UIPT,IRG,1) + UQFCONN(UIPT,IRG,2)) * FACT2
      UFLPVNR(2,IRG,CURIYR) =(UQFCONN(UIPV,IRG,3) + UQFCONN(UIPV,IRG,4)  +     &
                              UQFCONN(UIPT,IRG,3) + UQFCONN(UIPT,IRG,4)) * FACT2
!
      UTSO2(IRG,CURIYR) = UTSO2(IRG,CURIYR) + &
         (UTSO2N(UIGF,IRG) +             &
          UTSO2N(UIGI,IRG) +             &
          UTSO2N(UIGC,IRG) +             &
          UTSO2N(UIDG,IRG) +             &
          UTSO2N(UIDD,IRG) +             &
          UTSO2N(UIDS,IRG) +             &
          UTSO2N(UIRL,IRG) +             &
          UTSO2N(UIRH,IRG) +             &
          UTSO2N(UIB1,IRG) +             &
          UTSO2N(UIB2,IRG) +             &
          UTSO2N(UIB3,IRG) +             &
          UTSO2N(UIB4,IRG) +             &
          UTSO2N(UIB5,IRG) +             &
          UTSO2N(UIB6,IRG) +             &
          UTSO2N(UIB7,IRG) +             &
          UTSO2N(UIB8,IRG) +             &
          UTSO2N(UIC1,IRG) +             &
          UTSO2N(UIC2,IRG) +             &
          UTSO2N(UIC3,IRG) +             &
          UTSO2N(UIC4,IRG) +             &
          UTSO2N(UIC5,IRG) +             &
          UTSO2N(UIC6,IRG) +             &
          UTSO2N(UIC7,IRG) +             &
          UTSO2N(UIC8,IRG) +             &
          UTSO2N(UIC9,IRG) +             &
          UTSO2N(UICX,IRG) +             &
          UTSO2N(UICY,IRG) +             &
          UTSO2N(UICZ,IRG) +             &
          UTSO2N(UIH1,IRG) +             &
          UTSO2N(UIH2,IRG) +             &
          UTSO2N(UIH3,IRG) +             &
          UTSO2N(UIH4,IRG) +             &
          UTSO2N(UIH5,IRG) +             &
          UTSO2N(UIH6,IRG) +             &
          UTSO2N(UIH7,IRG) +             &
          UTSO2N(UIH8,IRG) +             &
          UTSO2N(UIH9,IRG) +             &
          UTSO2N(UIHA,IRG) +             &
          UTSO2N(UIHB,IRG) +             &
          UTSO2N(UIHC,IRG) +             &
          UTSO2N(UIPC,IRG) +             &
          UTSO2N(UIIG,IRG) +             &
          UTSO2N(UIPQ,IRG) +             &
          UTSO2N(UIIS,IRG) +             &
         ((UGNMSNR(1,IRG,CURIYR) + UGNMSNR(2,IRG,CURIYR)) * WHRMSEL(IRG,RYR) * WEMMSEL(4,RYR) * .5)) * FACT3
!
      UTNOX(IRG,CURIYR) = UTNOX(IRG,CURIYR) + &
         (UTNOXN(UIGF,IRG) +             &
          UTNOXN(UIGI,IRG) +             &
          UTNOXN(UIGC,IRG) +             &
          UTNOXN(UIDG,IRG) +             &
          UTNOXN(UIDD,IRG) +             &
          UTNOXN(UIDS,IRG) +             &
          UTNOXN(UIRL,IRG) +             &
          UTNOXN(UIRH,IRG) +             &
          UTNOXN(UIB1,IRG) +             &
          UTNOXN(UIB2,IRG) +             &
          UTNOXN(UIB3,IRG) +             &
          UTNOXN(UIB4,IRG) +             &
          UTNOXN(UIB5,IRG) +             &
          UTNOXN(UIB6,IRG) +             &
          UTNOXN(UIB7,IRG) +             &
          UTNOXN(UIB8,IRG) +             &
          UTNOXN(UIC1,IRG) +             &
          UTNOXN(UIC2,IRG) +             &
          UTNOXN(UIC3,IRG) +             &
          UTNOXN(UIC4,IRG) +             &
          UTNOXN(UIC5,IRG) +             &
          UTNOXN(UIC6,IRG) +             &
          UTNOXN(UIC7,IRG) +             &
          UTNOXN(UIC8,IRG) +             &
          UTNOXN(UIC9,IRG) +             &
          UTNOXN(UICX,IRG) +             &
          UTNOXN(UICY,IRG) +             &
          UTNOXN(UICZ,IRG) +             &
          UTNOXN(UIH1,IRG) +             &
          UTNOXN(UIH2,IRG) +             &
          UTNOXN(UIH3,IRG) +             &
          UTNOXN(UIH4,IRG) +             &
          UTNOXN(UIH5,IRG) +             &
          UTNOXN(UIH6,IRG) +             &
          UTNOXN(UIH7,IRG) +             &
          UTNOXN(UIH8,IRG) +             &
          UTNOXN(UIH9,IRG) +             &
          UTNOXN(UIHA,IRG) +             &
          UTNOXN(UIHB,IRG) +             &
          UTNOXN(UIHC,IRG) +             &
          UTNOXN(UIPC,IRG) +             &
          UTNOXN(UIIG,IRG) +             &
          UTNOXN(UIPQ,IRG) +             &
          UTNOXN(UIIS,IRG) +             &
         ((UGNMSNR(1,IRG,CURIYR) + UGNMSNR(2,IRG,CURIYR)) * WHRMSEL(IRG,RYR) * WEMMSEL(5,RYR) * .5)) * FACT3
!
      UTCO2(IRG,CURIYR) = UTCO2(IRG,CURIYR) + &
         (UTCO2N(UIGF,IRG) +             &
          UTCO2N(UIGI,IRG) +             &
          UTCO2N(UIGC,IRG) +             &
          UTCO2N(UIDS,IRG) +             &
          UTCO2N(UIDG,IRG) +             &
          UTCO2N(UIDD,IRG) +             &
          UTCO2N(UIRL,IRG) +             &
          UTCO2N(UIRH,IRG) +             &
          UTCO2N(UIB1,IRG) +             &
          UTCO2N(UIB2,IRG) +             &
          UTCO2N(UIB3,IRG) +             &
          UTCO2N(UIB4,IRG) +             &
          UTCO2N(UIB5,IRG) +             &
          UTCO2N(UIB6,IRG) +             &
          UTCO2N(UIB7,IRG) +             &
          UTCO2N(UIB8,IRG) +             &
          UTCO2N(UIC1,IRG) +             &
          UTCO2N(UIC2,IRG) +             &
          UTCO2N(UIC3,IRG) +             &
          UTCO2N(UIC4,IRG) +             &
          UTCO2N(UIC5,IRG) +             &
          UTCO2N(UIC6,IRG) +             &
          UTCO2N(UIC7,IRG) +             &
          UTCO2N(UIC8,IRG) +             &
          UTCO2N(UIC9,IRG) +             &
          UTCO2N(UICX,IRG) +             &
          UTCO2N(UICY,IRG) +             &
          UTCO2N(UICZ,IRG) +             &
          UTCO2N(UIH1,IRG) +             &
          UTCO2N(UIH2,IRG) +             &
          UTCO2N(UIH3,IRG) +             &
          UTCO2N(UIH4,IRG) +             &
          UTCO2N(UIH5,IRG) +             &
          UTCO2N(UIH6,IRG) +             &
          UTCO2N(UIH7,IRG) +             &
          UTCO2N(UIH8,IRG) +             &
          UTCO2N(UIH9,IRG) +             &
          UTCO2N(UIHA,IRG) +             &
          UTCO2N(UIHB,IRG) +             &
          UTCO2N(UIHC,IRG) +             &
          UTCO2N(UIPC,IRG) +             &
          UTCO2N(UIIG,IRG) +             &
          UTCO2N(UIPQ,IRG) +             &
          UTCO2N(UIIS,IRG) +             &
         ((UGNMSNR(1,IRG,CURIYR) + UGNMSNR(2,IRG,CURIYR)) * WHRMSEL(IRG,RYR) * WEMMSEL(3,RYR) * .5)) * FACTE
!
      UTCO1(IRG,CURIYR) = UTCO1(IRG,CURIYR) + &
         (UTCO1N(UIGF,IRG) +             &
          UTCO1N(UIGI,IRG) +             &
          UTCO1N(UIGC,IRG) +             &
          UTCO1N(UIDS,IRG) +             &
          UTCO1N(UIDG,IRG) +             &
          UTCO1N(UIDD,IRG) +             &
          UTCO1N(UIRL,IRG) +             &
          UTCO1N(UIRH,IRG) +             &
          UTCO1N(UIB1,IRG) +             &
          UTCO1N(UIB2,IRG) +             &
          UTCO1N(UIB3,IRG) +             &
          UTCO1N(UIB4,IRG) +             &
          UTCO1N(UIB5,IRG) +             &
          UTCO1N(UIB6,IRG) +             &
          UTCO1N(UIB7,IRG) +             &
          UTCO1N(UIB8,IRG) +             &
          UTCO1N(UIC1,IRG) +             &
          UTCO1N(UIC2,IRG) +             &
          UTCO1N(UIC3,IRG) +             &
          UTCO1N(UIC4,IRG) +             &
          UTCO1N(UIC5,IRG) +             &
          UTCO1N(UIC6,IRG) +             &
          UTCO1N(UIC7,IRG) +             &
          UTCO1N(UIC8,IRG) +             &
          UTCO1N(UIC9,IRG) +             &
          UTCO1N(UICX,IRG) +             &
          UTCO1N(UICY,IRG) +             &
          UTCO1N(UICZ,IRG) +             &
          UTCO1N(UIH1,IRG) +             &
          UTCO1N(UIH2,IRG) +             &
          UTCO1N(UIH3,IRG) +             &
          UTCO1N(UIH4,IRG) +             &
          UTCO1N(UIH5,IRG) +             &
          UTCO1N(UIH6,IRG) +             &
          UTCO1N(UIH7,IRG) +             &
          UTCO1N(UIH8,IRG) +             &
          UTCO1N(UIH9,IRG) +             &
          UTCO1N(UIHA,IRG) +             &
          UTCO1N(UIHB,IRG) +             &
          UTCO1N(UIHC,IRG) +             &
          UTCO1N(UIPC,IRG) +             &
          UTCO1N(UIIG,IRG) +             &
          UTCO1N(UIPQ,IRG) +             &
          UTCO1N(UIIS,IRG) +             &
         ((UGNMSNR(1,IRG,CURIYR) + UGNMSNR(2,IRG,CURIYR)) * WHRMSEL(IRG,RYR) * WEMMSEL(2,RYR) * .5)) * FACTE
!
      UTCAR(IRG,CURIYR) = UTCAR(IRG,CURIYR) + &
         (UTCARN(UIGF,IRG) +             &
          UTCARN(UIGI,IRG) +             &
          UTCARN(UIGC,IRG) +             &
          UTCARN(UIDS,IRG) +             &
          UTCARN(UIDD,IRG) +             &
          UTCARN(UIDG,IRG) +             &
          UTCARN(UIRL,IRG) +             &
          UTCARN(UIRH,IRG) +             &
          UTCARN(UIB1,IRG) +             &
          UTCARN(UIB2,IRG) +             &
          UTCARN(UIB3,IRG) +             &
          UTCARN(UIB4,IRG) +             &
          UTCARN(UIB5,IRG) +             &
          UTCARN(UIB6,IRG) +             &
          UTCARN(UIB7,IRG) +             &
          UTCARN(UIB8,IRG) +             &
          UTCARN(UIC1,IRG) +             &
          UTCARN(UIC2,IRG) +             &
          UTCARN(UIC3,IRG) +             &
          UTCARN(UIC4,IRG) +             &
          UTCARN(UIC5,IRG) +             &
          UTCARN(UIC6,IRG) +             &
          UTCARN(UIC7,IRG) +             &
          UTCARN(UIC8,IRG) +             &
          UTCARN(UIC9,IRG) +             &
          UTCARN(UICX,IRG) +             &
          UTCARN(UICY,IRG) +             &
          UTCARN(UICZ,IRG) +             &
          UTCARN(UIH1,IRG) +             &
          UTCARN(UIH2,IRG) +             &
          UTCARN(UIH3,IRG) +             &
          UTCARN(UIH4,IRG) +             &
          UTCARN(UIH5,IRG) +             &
          UTCARN(UIH6,IRG) +             &
          UTCARN(UIH7,IRG) +             &
          UTCARN(UIH8,IRG) +             &
          UTCARN(UIH9,IRG) +             &
          UTCARN(UIHA,IRG) +             &
          UTCARN(UIHB,IRG) +             &
          UTCARN(UIHC,IRG) +             &
          UTCARN(UIPC,IRG) +             &
          UTCARN(UIIG,IRG) +             &
          UTCARN(UIIS,IRG) +             &
          UTCARN(UIPQ,IRG) +             &
         ((UGNMSNR(1,IRG,CURIYR) + UGNMSNR(2,IRG,CURIYR)) * WHRMSEL(IRG,RYR) * WEMMSEL(1,RYR) * .5)) * FACTE
!
      UTHG(IRG,CURIYR) = UTHG(IRG,CURIYR) + &
         (UTHGN(UIGF,IRG) +             &
          UTHGN(UIGI,IRG) +             &
          UTHGN(UIGC,IRG) +             &
          UTHGN(UIDG,IRG) +             &
          UTHGN(UIDD,IRG) +             &
          UTHGN(UIDS,IRG) +             &
          UTHGN(UIRL,IRG) +             &
          UTHGN(UIRH,IRG) +             &
          UTHGN(UIB1,IRG) +             &
          UTHGN(UIB2,IRG) +             &
          UTHGN(UIB3,IRG) +             &
          UTHGN(UIB4,IRG) +             &
          UTHGN(UIB5,IRG) +             &
          UTHGN(UIB6,IRG) +             &
          UTHGN(UIB7,IRG) +             &
          UTHGN(UIB8,IRG) +             &
          UTHGN(UIC1,IRG) +             &
          UTHGN(UIC2,IRG) +             &
          UTHGN(UIC3,IRG) +             &
          UTHGN(UIC4,IRG) +             &
          UTHGN(UIC5,IRG) +             &
          UTHGN(UIC6,IRG) +             &
          UTHGN(UIC7,IRG) +             &
          UTHGN(UIC8,IRG) +             &
          UTHGN(UIC9,IRG) +             &
          UTHGN(UICX,IRG) +             &
          UTHGN(UICY,IRG) +             &
          UTHGN(UICZ,IRG) +             &
          UTHGN(UIH1,IRG) +             &
          UTHGN(UIH2,IRG) +             &
          UTHGN(UIH3,IRG) +             &
          UTHGN(UIH4,IRG) +             &
          UTHGN(UIH5,IRG) +             &
          UTHGN(UIH6,IRG) +             &
          UTHGN(UIH7,IRG) +             &
          UTHGN(UIH8,IRG) +             &
          UTHGN(UIH9,IRG) +             &
          UTHGN(UIHA,IRG) +             &
          UTHGN(UIHB,IRG) +             &
          UTHGN(UIHC,IRG) +             &
          UTHGN(UIPC,IRG) +             &
          UTHGN(UIIG,IRG) +             &
          UTHGN(UIPQ,IRG) +             &
          UTHGN(UIIS,IRG))
!
      USO2INR(IRG,CURIYR) = USO2INR(IRG,CURIYR) + ( &
         ((UGNMSNR(1,IRG,CURIYR) + UGNMSNR(2,IRG,CURIYR)) * WHRMSEL(IRG,RYR) * WEMMSEL(4,RYR) * .5) ) * FACT3
      UCO2INR(IRG,CURIYR) = UCO2INR(IRG,CURIYR) + ( &
         ((UGNMSNR(1,IRG,CURIYR) + UGNMSNR(2,IRG,CURIYR)) * WHRMSEL(IRG,RYR) * WEMMSEL(3,RYR) * .5) ) * FACT3
      UCARINR(IRG,CURIYR) = UCARINR(IRG,CURIYR) + ( &
         ((UGNMSNR(1,IRG,CURIYR) + UGNMSNR(2,IRG,CURIYR)) * WHRMSEL(IRG,RYR) * WEMMSEL(1,RYR) * .5) ) * FACT3

!     write(6,*) 'UTIL MSW SO2',CURIYR,CURITR,IRG,USO2INR(IRG,CURIYR),(((UGNMSNR(1,IRG,CURIYR) + UGNMSNR(2,IRG,CURIYR)) * WHRMSEL(IRG,RYR) * WEMMSEL(4,RYR) * .5) ) * FACT3
!
      UTCAR(MNUMNR,CURIYR) = UTCAR(MNUMNR,CURIYR) + UTCAR(IRG,CURIYR)
      UTSO2(MNUMNR,CURIYR) = UTSO2(MNUMNR,CURIYR) + UTSO2(IRG,CURIYR)
      UTNOX(MNUMNR,CURIYR) = UTNOX(MNUMNR,CURIYR) + UTNOX(IRG,CURIYR)
      UTCO2(MNUMNR,CURIYR) = UTCO2(MNUMNR,CURIYR) + UTCO2(IRG,CURIYR)
      UTCO1(MNUMNR,CURIYR) = UTCO1(MNUMNR,CURIYR) + UTCO1(IRG,CURIYR)
      UTHG(MNUMNR,CURIYR) = UTHG(MNUMNR,CURIYR) + UTHG(IRG,CURIYR)
      UCARINR(MNUMNR,CURIYR) = UCARINR(MNUMNR,CURIYR) + UCARINR(IRG,CURIYR)
      UCO2INR(MNUMNR,CURIYR) = UCO2INR(MNUMNR,CURIYR) + UCO2INR(IRG,CURIYR)
      USO2INR(MNUMNR,CURIYR) = USO2INR(MNUMNR,CURIYR) + USO2INR(IRG,CURIYR)
      UNOXINR(MNUMNR,CURIYR) = UNOXINR(MNUMNR,CURIYR) + UNOXINR(IRG,CURIYR)
      UHGINR(MNUMNR,CURIYR) = UHGINR(MNUMNR,CURIYR) + UHGINR(IRG,CURIYR)
      UCAROTR(MNUMNR,CURIYR) = UCAROTR(MNUMNR,CURIYR) + UCAROTR(IRG,CURIYR)
      UCO2OTR(MNUMNR,CURIYR) = UCO2OTR(MNUMNR,CURIYR) + UCO2OTR(IRG,CURIYR)
      USO2OTR(MNUMNR,CURIYR) = USO2OTR(MNUMNR,CURIYR) + USO2OTR(IRG,CURIYR)
      UNOXOTR(MNUMNR,CURIYR) = UNOXOTR(MNUMNR,CURIYR) + UNOXOTR(IRG,CURIYR)
      UHGOTR(MNUMNR,CURIYR) = UHGOTR(MNUMNR,CURIYR) + UHGOTR(IRG,CURIYR)
!
!     CAPTURE MERCURY EMISSIONS FROM NON-COAL PLANTS
!
      HGOTHER(CURIYR) = HGOTHER(CURIYR) +  &
         UTHGN(UIDS,IRG) + &
         UTHGN(UIRL,IRG) + &
         UTHGN(UIRH,IRG) + &
         UTHGN(UIGF,IRG) + &
         UTHGN(UIGI,IRG) + &
         UTHGN(UIGC,IRG) + &
         UTHGN(UIDG,IRG) + &
         UTHGN(UIDD,IRG) + &
         UTHGN(UIUF,IRG)
         WRITE(18,2337) CURIYR+UHBSYR,CURITR,IRG,HGOTHER(CURIYR),UIDS,UTHGN(UIDS,IRG),UIRL,UTHGN(UIRL,IRG),UIRH,UTHGN(UIRH,IRG), &
            UIGF,UTHGN(UIGF,IRG),UIGI,UTHGN(UIGI,IRG),UIGC,UTHGN(UIGC,IRG),UIDG,UTHGN(UIDG,IRG),UIDD,UTHGN(UIDD,IRG),UIUF,UTHGN(UIUF,IRG)
 2337    FORMAT(1X,"HGOTHER",3(":",I4),(":",F6.3),9(":",I3,":",F6.3))
      END IF                         ! END IF FOR SKIPPING ALASKA/HAWAII
!
!NON-TRADITIONAL COGENERATION - OWN USE AND SALES TO GRID
!THE SPLIT BETWEEN OWN USE AND SALES TO GRID IS BASED ON 1993
!DATA...WILL ADD AS INPUT AFTER AEO96
!
      IF (USW_OWN .GT. 3) THEN
!
!        SOLD TO GRID - COAL
!
         CGNTGEN(IRG,CURIYR,1,1) = &
            (UQFGENN(UIB1,IRG,4) +                &
             UQFGENN(UIB2,IRG,4) +                &
             UQFGENN(UIB3,IRG,4) +                &
             UQFGENN(UIB4,IRG,4) +                &
             UQFGENN(UIB5,IRG,4) +                &
             UQFGENN(UIB6,IRG,4) +                &
             UQFGENN(UIB7,IRG,4) +                &
             UQFGENN(UIB8,IRG,4) +                &
             UQFGENN(UIC1,IRG,4) +                &
             UQFGENN(UIC2,IRG,4) +                &
             UQFGENN(UIC3,IRG,4) +                &
             UQFGENN(UIC4,IRG,4) +                &
             UQFGENN(UIC5,IRG,4) +                &
             UQFGENN(UIC6,IRG,4) +                &
             UQFGENN(UIC7,IRG,4) +                &
             UQFGENN(UIC8,IRG,4) +                &
             UQFGENN(UIC9,IRG,4) +                &
             UQFGENN(UICX,IRG,4) +                &
             UQFGENN(UICY,IRG,4) +                &
             UQFGENN(UICZ,IRG,4) +                &
             UQFGENN(UIH1,IRG,4) +                &
             UQFGENN(UIH2,IRG,4) +                &
             UQFGENN(UIH3,IRG,4) +                &
             UQFGENN(UIH4,IRG,4) +                &
             UQFGENN(UIH5,IRG,4) +                &
             UQFGENN(UIH6,IRG,4) +                &
             UQFGENN(UIH7,IRG,4) +                &
             UQFGENN(UIH8,IRG,4) +                &
             UQFGENN(UIH9,IRG,4) +                &
             UQFGENN(UIHA,IRG,4) +                &
             UQFGENN(UIHB,IRG,4) +                &
             UQFGENN(UIHC,IRG,4) +                &
             UQFGENN(UIPC,IRG,4) +                &
             UQFGENN(UIIG,IRG,4) +                &
             UQFGENN(UIPQ,IRG,4) +                &
             UQFGENN(UIIS,IRG,4) )
!
!  SOLD TO GRID - OIL GENERATION
      CGNTGEN(IRG,CURIYR,2,1) =(UQFGENN(UIDS,IRG,4) + UQFGENN(UIRL,IRG,4) + UQFGENN(UIRH,IRG,4))
!
!  SOLD TO GRID - GAS GENERATION
      CGNTGEN(IRG,CURIYR,3,1) =(UQFGENN(UIGF,IRG,4) + UQFGENN(UIGC,IRG,4) + UQFGENN(UIGI,IRG,4))
!
!  RENEWABLE NON-TRADITIONAL COGEN
!
!  HYDROELECTRIC
!
      CGNTGEN(IRG,CURIYR,4,1) = UQPGENN(UIHYC,IRG,4)
!
!  GEOTHERMAL
!
      CGNTGEN(IRG,CURIYR,5,1) = UQPGENN(UIGTH,IRG,4)
!
!  MSW
!
      CGNTGEN(IRG,CURIYR,6,1) = UQPGENN(UIMSW,IRG,4)
!
!  BIOMASS
!
!     CGNTGEN(IRG,CURIYR,7,1) = UQPGENN(UIBMS,IRG,4)
      CGNTGEN(IRG,CURIYR,7,1) = UQFGENN(UIWD,IRG,4)
!
      CGNTGEN(IRG,CURIYR,8,1) = UQPGENN(UISPV,IRG,4) + UQPGENN(UIPVT,IRG,4)
!
!  OTHER GAS FUELS
!
      CGNTGEN(IRG,CURIYR,9,1) = UQFGENN(UIOG,IRG,4)
!  CGNTGEN(IRG,CURIYR,9,2)=(UQFGENN(23,IRG,4)  * .05)
!
!  OTHER FUELS
!
!     CGNTGEN(IRG,CURIYR,10,1) = (UQFGENN(UIPV,IRG,4))

!  SHARE OUT OWN-USE NONTRAD COGEN BY FUEL USING RATIO OF REGIONAL
!  OWNUSE TO REGIONAL TOTAL

      TOTNTGEN = 0.0
      DO IFL=1,10
         TOTNTGEN = CGNTGEN(IRG,CURIYR,IFL,1) + TOTNTGEN
      ENDDO ! IFL

      TOTNTGEN = TOTNTGEN/1000.0
      IF (TOTNTGEN .gt. 0.0) THEN
        DO IFL=1,10
           CGNTGEN(IRG,CURIYR,IFL,2) = CGNTGEN(IRG,CURIYR,IFL,1) * BNTCOWN(IRG)/TOTNTGEN
           CGNTGEN(IRG,CURIYR,IFL,1) = CGNTGEN(IRG,CURIYR,IFL,1) * (1.0 - BNTCOWN(IRG)/TOTNTGEN)
        END DO ! IFL
      ENDIF ! TOTNTGEN

!  NATIONAL TOTALS - NONTRADITIONAL COGEN
      DO IFL = 1,10
         CGNTGEN(MNUMNR,CURIYR,IFL,1) = CGNTGEN(MNUMNR,CURIYR,IFL,1) + CGNTGEN(IRG,CURIYR,IFL,1)
         CGNTGEN(MNUMNR,CURIYR,IFL,2) = CGNTGEN(MNUMNR,CURIYR,IFL,2) + CGNTGEN(IRG,CURIYR,IFL,2)
         CGOTGEN(MNUMNR,CURIYR,IFL) = CGOTGEN(MNUMNR,CURIYR,IFL) + CGOTGEN(IRG,CURIYR,IFL)
      END DO
!
!     COAL CONSUMPTION
!
      CGNTQ(IRG,CURIYR,1) = &
         (UQFCONN(UIB1,IRG,4) +                &
          UQFCONN(UIB2,IRG,4) +                &
          UQFCONN(UIB3,IRG,4) +                &
          UQFCONN(UIB4,IRG,4) +                &
          UQFCONN(UIB5,IRG,4) +                &
          UQFCONN(UIB6,IRG,4) +                &
          UQFCONN(UIB7,IRG,4) +                &
          UQFCONN(UIB8,IRG,4) +                &
          UQFCONN(UIC1,IRG,4) +                &
          UQFCONN(UIC2,IRG,4) +                &
          UQFCONN(UIC3,IRG,4) +                &
          UQFCONN(UIC4,IRG,4) +                &
          UQFCONN(UIC5,IRG,4) +                &
          UQFCONN(UIC6,IRG,4) +                &
          UQFCONN(UIC7,IRG,4) +                &
          UQFCONN(UIC8,IRG,4) +                &
          UQFCONN(UIC9,IRG,4) +                &
          UQFCONN(UICX,IRG,4) +                &
          UQFCONN(UICY,IRG,4) +                &
          UQFCONN(UICZ,IRG,4) +                &
          UQFCONN(UIH1,IRG,4) +                &
          UQFCONN(UIH2,IRG,4) +                &
          UQFCONN(UIH3,IRG,4) +                &
          UQFCONN(UIH4,IRG,4) +                &
          UQFCONN(UIH5,IRG,4) +                &
          UQFCONN(UIH6,IRG,4) +                &
          UQFCONN(UIH7,IRG,4) +                &
          UQFCONN(UIH8,IRG,4) +                &
          UQFCONN(UIH9,IRG,4) +                &
          UQFCONN(UIHA,IRG,4) +                &
          UQFCONN(UIHB,IRG,4) +                &
          UQFCONN(UIHC,IRG,4) +                &
          UQFCONN(UIPC,IRG,4) +                &
          UQFCONN(UIIG,IRG,4) +                &
          UQFCONN(UIPQ,IRG,4) +                &
          UQFCONN(UIIS,IRG,4) ) * 0.001
!
!     OIL CONSUMPTION AT NON-TRADITIONAL COGENERATORS
!
      CGNTQ(IRG,CURIYR,2) = (UQFCONN(UIDS,IRG,4) + &
         UQFCONN(UIRL,IRG,4) + &
         UQFCONN(UIRH,IRG,4)) /1000.
!     GAS CONSUMPTION AT NONTRADITIONAL COGENERATORS
      CGNTQ(IRG,CURIYR,3) = (UQFCONN(UIGF,IRG,4) + &
         UQFCONN(UIGI,IRG,4) + &
         UQFCONN(UIGC,IRG,4)) /1000.
!
!     CONSUMPTION EQUIVALENT AT RENEWABLE SOURCES
      CGNTQ(IRG,CURIYR,4) = (UQFCONN(UIWA,IRG,4) + UQFCONN(UITI,IRG,4)) / 1000.
      CGNTQ(IRG,CURIYR,5) = UQFCONN(UIGT,IRG,4) / 1000.
      CGNTQ(IRG,CURIYR,6) = UQFCONN(UISW,IRG,4) / 1000.
      CGNTQ(IRG,CURIYR,7) = UQFCONN(UIWD,IRG,4) / 1000.
      CGNTQ(IRG,CURIYR,8) = (UQFCONN(UIPV,IRG,4) + UQFCONN(UIPT,IRG,4)) / 1000.
      CGNTQ(IRG,CURIYR,9) = (CGNTGEN(IRG,CURIYR,9,1) + &
          CGNTGEN(IRG,CURIYR,9,2)) * WHRFOSS(MNUMCR,CURIYR)/1000000.
      CGNTQ(IRG,CURIYR,10) = (CGNTGEN(IRG,CURIYR,10,1) + &
          CGNTGEN(IRG,CURIYR,10,2)) * WHRFOSS(MNUMCR,CURIYR)/1000000.
!
      END IF
      DO IFL = 1,10
         CGNTQ(MNUMNR,CURIYR,IFL) = CGNTQ(MNUMNR,CURIYR,IFL) + CGNTQ(IRG,CURIYR,IFL)
      END DO
!ACCUMULATE NAIONAL TOTALS
!
      DO IOWNER = 1 , 2
!GENERATION
        UGNCLNR(IOWNER,MNUMNR,CURIYR) = UGNCLNR(IOWNER,MNUMNR,CURIYR) + UGNCLNR(IOWNER,IRG,CURIYR)
        UGNDSNR(IOWNER,MNUMNR,CURIYR) = UGNDSNR(IOWNER,MNUMNR,CURIYR) + UGNDSNR(IOWNER,IRG,CURIYR)
        UGNRLNR(IOWNER,MNUMNR,CURIYR) = UGNRLNR(IOWNER,MNUMNR,CURIYR) + UGNRLNR(IOWNER,IRG,CURIYR)
        UGNRHNR(IOWNER,MNUMNR,CURIYR) = UGNRHNR(IOWNER,MNUMNR,CURIYR) + UGNRHNR(IOWNER,IRG,CURIYR)
        UGNGFNR(IOWNER,MNUMNR,CURIYR) = UGNGFNR(IOWNER,MNUMNR,CURIYR) + UGNGFNR(IOWNER,IRG,CURIYR)
        UGNGINR(IOWNER,MNUMNR,CURIYR) = UGNGINR(IOWNER,MNUMNR,CURIYR) + UGNGINR(IOWNER,IRG,CURIYR)
        UGNGCNR(IOWNER,MNUMNR,CURIYR) = UGNGCNR(IOWNER,MNUMNR,CURIYR) + UGNGCNR(IOWNER,IRG,CURIYR)
        UGNURNR(IOWNER,MNUMNR,CURIYR) = UGNURNR(IOWNER,MNUMNR,CURIYR) + UGNURNR(IOWNER,IRG,CURIYR)
        UGNDGNR(IOWNER,MNUMNR,CURIYR) = UGNDGNR(IOWNER,MNUMNR,CURIYR) + UGNDGNR(IOWNER,IRG,CURIYR)
        UGNDDNR(IOWNER,MNUMNR,CURIYR) = UGNDDNR(IOWNER,MNUMNR,CURIYR) + UGNDDNR(IOWNER,IRG,CURIYR)
        UGNPSNR(IOWNER,MNUMNR,CURIYR) = UGNPSNR(IOWNER,MNUMNR,CURIYR) + UGNPSNR(IOWNER,IRG,CURIYR)
        UGNHYNR(IOWNER,MNUMNR,CURIYR) = UGNHYNR(IOWNER,MNUMNR,CURIYR) + UGNHYNR(IOWNER,IRG,CURIYR)
        UGNGENR(IOWNER,MNUMNR,CURIYR) = UGNGENR(IOWNER,MNUMNR,CURIYR) + UGNGENR(IOWNER,IRG,CURIYR)
        UGNMSNR(IOWNER,MNUMNR,CURIYR) = UGNMSNR(IOWNER,MNUMNR,CURIYR) + UGNMSNR(IOWNER,IRG,CURIYR)
        UGNWDNR(IOWNER,MNUMNR,CURIYR) = UGNWDNR(IOWNER,MNUMNR,CURIYR) + UGNWDNR(IOWNER,IRG,CURIYR)
        UGNSONR(IOWNER,MNUMNR,CURIYR) = UGNSONR(IOWNER,MNUMNR,CURIYR) + UGNSONR(IOWNER,IRG,CURIYR)
        UGNSDNR(IOWNER,MNUMNR,CURIYR) = UGNSDNR(IOWNER,MNUMNR,CURIYR) + UGNSDNR(IOWNER,IRG,CURIYR)
        UGNPVNR(IOWNER,MNUMNR,CURIYR) = UGNPVNR(IOWNER,MNUMNR,CURIYR) + UGNPVNR(IOWNER,IRG,CURIYR)
        UGNPTNR(IOWNER,MNUMNR,CURIYR) = UGNPTNR(IOWNER,MNUMNR,CURIYR) + UGNPTNR(IOWNER,IRG,CURIYR)
        UGNWNNR(IOWNER,MNUMNR,CURIYR) = UGNWNNR(IOWNER,MNUMNR,CURIYR) + UGNWNNR(IOWNER,IRG,CURIYR)
        UGNWLNR(IOWNER,MNUMNR,CURIYR) = UGNWLNR(IOWNER,MNUMNR,CURIYR) + UGNWLNR(IOWNER,IRG,CURIYR)
        UGNWFNR(IOWNER,MNUMNR,CURIYR) = UGNWFNR(IOWNER,MNUMNR,CURIYR) + UGNWFNR(IOWNER,IRG,CURIYR)
        UGNHONR(IOWNER,MNUMNR,CURIYR) = UGNHONR(IOWNER,MNUMNR,CURIYR) + UGNHONR(IOWNER,IRG,CURIYR)
        UGNTLNR(IOWNER,MNUMNR,CURIYR) = UGNTLNR(IOWNER,MNUMNR,CURIYR) + UGNTLNR(IOWNER,IRG,CURIYR)
!FUEL CONSUMPTION
        UFLCLNR(IOWNER,MNUMNR,CURIYR) = UFLCLNR(IOWNER,MNUMNR,CURIYR) + UFLCLNR(IOWNER,IRG,CURIYR)
        UFLDSNR(IOWNER,MNUMNR,CURIYR) = UFLDSNR(IOWNER,MNUMNR,CURIYR) + UFLDSNR(IOWNER,IRG,CURIYR)
        UFLRLNR(IOWNER,MNUMNR,CURIYR) = UFLRLNR(IOWNER,MNUMNR,CURIYR) + UFLRLNR(IOWNER,IRG,CURIYR)
        UFLRHNR(IOWNER,MNUMNR,CURIYR) = UFLRHNR(IOWNER,MNUMNR,CURIYR) + UFLRHNR(IOWNER,IRG,CURIYR)
        UFLGFNR(IOWNER,MNUMNR,CURIYR) = UFLGFNR(IOWNER,MNUMNR,CURIYR) + UFLGFNR(IOWNER,IRG,CURIYR)
        UFLGINR(IOWNER,MNUMNR,CURIYR) = UFLGINR(IOWNER,MNUMNR,CURIYR) + UFLGINR(IOWNER,IRG,CURIYR)
        UFLGCNR(IOWNER,MNUMNR,CURIYR) = UFLGCNR(IOWNER,MNUMNR,CURIYR) + UFLGCNR(IOWNER,IRG,CURIYR)
        UFLURNR(IOWNER,MNUMNR,CURIYR) = UFLURNR(IOWNER,MNUMNR,CURIYR) + UFLURNR(IOWNER,IRG,CURIYR)
        UFLDGNR(IOWNER,MNUMNR,CURIYR) = UFLDGNR(IOWNER,MNUMNR,CURIYR) + UFLDGNR(IOWNER,IRG,CURIYR)
        UFLDDNR(IOWNER,MNUMNR,CURIYR) = UFLDDNR(IOWNER,MNUMNR,CURIYR) + UFLDDNR(IOWNER,IRG,CURIYR)
        UFLGTNR(IOWNER,MNUMNR,CURIYR) = UFLGTNR(IOWNER,MNUMNR,CURIYR) + UFLGTNR(IOWNER,IRG,CURIYR)
        UFLWDNR(IOWNER,MNUMNR,CURIYR) = UFLWDNR(IOWNER,MNUMNR,CURIYR) + UFLWDNR(IOWNER,IRG,CURIYR)
        UFLMSNR(IOWNER,MNUMNR,CURIYR) = UFLMSNR(IOWNER,MNUMNR,CURIYR) + UFLMSNR(IOWNER,IRG,CURIYR)
        UFLPSNR(IOWNER,MNUMNR,CURIYR) = UFLPSNR(IOWNER,MNUMNR,CURIYR) + UFLPSNR(IOWNER,IRG,CURIYR)
        UFLHYNR(IOWNER,MNUMNR,CURIYR) = UFLHYNR(IOWNER,MNUMNR,CURIYR) + UFLHYNR(IOWNER,IRG,CURIYR)
        UFLWNNR(IOWNER,MNUMNR,CURIYR) = UFLWNNR(IOWNER,MNUMNR,CURIYR) + UFLWNNR(IOWNER,IRG,CURIYR)
        UFLWFNR(IOWNER,MNUMNR,CURIYR) = UFLWFNR(IOWNER,MNUMNR,CURIYR) + UFLWFNR(IOWNER,IRG,CURIYR)
        UFLSONR(IOWNER,MNUMNR,CURIYR) = UFLSONR(IOWNER,MNUMNR,CURIYR) + UFLSONR(IOWNER,IRG,CURIYR)
        UFLPVNR(IOWNER,MNUMNR,CURIYR) = UFLPVNR(IOWNER,MNUMNR,CURIYR) + UFLPVNR(IOWNER,IRG,CURIYR)
        UFLOTNR(IOWNER,MNUMNR,CURIYR) = UFLOTNR(IOWNER,MNUMNR,CURIYR) + UFLOTNR(IOWNER,IRG,CURIYR)
        UFLTLNR(IOWNER,MNUMNR,CURIYR) = UFLTLNR(IOWNER,MNUMNR,CURIYR) + UFLTLNR(IOWNER,IRG,CURIYR)
!RPS CREDITS/GENERATION
        IF (IRG .LE. UNRGNS) THEN
          UCRCLNR(IOWNER,MNUMNR,CURIYR) = UCRCLNR(IOWNER,MNUMNR,CURIYR) + UCRCLNR(IOWNER,IRG,CURIYR)
          UCRNGNR(IOWNER,MNUMNR,CURIYR) = UCRNGNR(IOWNER,MNUMNR,CURIYR) + UCRNGNR(IOWNER,IRG,CURIYR)
          UCRNUNR(IOWNER,MNUMNR,CURIYR) = UCRNUNR(IOWNER,MNUMNR,CURIYR) + UCRNUNR(IOWNER,IRG,CURIYR)
          UCRHYNR(IOWNER,MNUMNR,CURIYR) = UCRHYNR(IOWNER,MNUMNR,CURIYR) + UCRHYNR(IOWNER,IRG,CURIYR)
          UCRGENR(IOWNER,MNUMNR,CURIYR) = UCRGENR(IOWNER,MNUMNR,CURIYR) + UCRGENR(IOWNER,IRG,CURIYR)
          UCRMSNR(IOWNER,MNUMNR,CURIYR) = UCRMSNR(IOWNER,MNUMNR,CURIYR) + UCRMSNR(IOWNER,IRG,CURIYR)
          UCRWDNR(IOWNER,MNUMNR,CURIYR) = UCRWDNR(IOWNER,MNUMNR,CURIYR) + UCRWDNR(IOWNER,IRG,CURIYR)
          UCRSONR(IOWNER,MNUMNR,CURIYR) = UCRSONR(IOWNER,MNUMNR,CURIYR) + UCRSONR(IOWNER,IRG,CURIYR)
          UCRPVNR(IOWNER,MNUMNR,CURIYR) = UCRPVNR(IOWNER,MNUMNR,CURIYR) + UCRPVNR(IOWNER,IRG,CURIYR)
          UCRPTNR(IOWNER,MNUMNR,CURIYR) = UCRPTNR(IOWNER,MNUMNR,CURIYR) + UCRPTNR(IOWNER,IRG,CURIYR)
          UCRWNNR(IOWNER,MNUMNR,CURIYR) = UCRWNNR(IOWNER,MNUMNR,CURIYR) + UCRWNNR(IOWNER,IRG,CURIYR)
          UCRWLNR(IOWNER,MNUMNR,CURIYR) = UCRWLNR(IOWNER,MNUMNR,CURIYR) + UCRWLNR(IOWNER,IRG,CURIYR)
          UCRWFNR(IOWNER,MNUMNR,CURIYR) = UCRWFNR(IOWNER,MNUMNR,CURIYR) + UCRWFNR(IOWNER,IRG,CURIYR)
          UCRCFNR(IOWNER,MNUMNR,CURIYR) = UCRCFNR(IOWNER,MNUMNR,CURIYR) + UCRCFNR(IOWNER,IRG,CURIYR)
        END IF
      END DO
!RPS CHP CREDITS
      IF (IRG .LE. UNRGNS)THEN
      DO IOWNER = 1 , 4
         UCRCHNR(IOWNER,IRG,CURIYR) = UGNCHPN(IOWNER,IRG)
         UCRCHNR(IOWNER,MNUMNR,CURIYR) = UCRCHNR(IOWNER,MNUMNR,CURIYR) + UGNCHPN(IOWNER,IRG)
      END DO
!
      UCRTLNR(1,MNUMNR,CURIYR) = UCRTLNR(1,MNUMNR,CURIYR) + &
                                 UCRCLNR(1,IRG,CURIYR) + &
                                 UCRNGNR(1,IRG,CURIYR) + &
                                 UCRNUNR(1,IRG,CURIYR) + &
                                 UCRHYNR(1,IRG,CURIYR) + &
                                 UCRGENR(1,IRG,CURIYR) + &
                                 UCRMSNR(1,IRG,CURIYR) + &
                                 UCRWDNR(1,IRG,CURIYR) + &
                                 UCRSONR(1,IRG,CURIYR) + &
                                 UCRPVNR(1,IRG,CURIYR) + &
                                 UCRPTNR(1,IRG,CURIYR) + &
                                 UCRWNNR(1,IRG,CURIYR) + &
                                 UCRWLNR(1,IRG,CURIYR) + &
                                 UCRWFNR(1,IRG,CURIYR) + &
                                 UCRCFNR(1,IRG,CURIYR)
      UCRTLNR(2,MNUMNR,CURIYR) = UCRTLNR(2,MNUMNR,CURIYR) + &
                                 UCRCLNR(2,IRG,CURIYR) + &
                                 UCRNGNR(2,IRG,CURIYR) + &
                                 UCRNUNR(2,IRG,CURIYR) + &
                                 UCRHYNR(2,IRG,CURIYR) + &
                                 UCRGENR(2,IRG,CURIYR) + &
                                 UCRMSNR(2,IRG,CURIYR) + &
                                 UCRWDNR(2,IRG,CURIYR) + &
                                 UCRSONR(2,IRG,CURIYR) + &
                                 UCRPVNR(2,IRG,CURIYR) + &
                                 UCRPTNR(2,IRG,CURIYR) + &
                                 UCRWNNR(2,IRG,CURIYR) + &
                                 UCRWLNR(2,IRG,CURIYR) + &
                                 UCRWFNR(2,IRG,CURIYR) + &
                                 UCRCFNR(2,IRG,CURIYR)
! PTC GENERATION/PAYMENTS
      UPTCLNR(MNUMNR,CURIYR) = UPTCLNR(MNUMNR,CURIYR) + UPTCLNR(IRG,CURIYR)
      UPTNGNR(MNUMNR,CURIYR) = UPTNGNR(MNUMNR,CURIYR) + UPTNGNR(IRG,CURIYR)
      UPTNUNR(MNUMNR,CURIYR) = UPTNUNR(MNUMNR,CURIYR) + UPTNUNR(IRG,CURIYR)
      UPTHYNR(MNUMNR,CURIYR) = UPTHYNR(MNUMNR,CURIYR) + UPTHYNR(IRG,CURIYR)
      UPTGENR(MNUMNR,CURIYR) = UPTGENR(MNUMNR,CURIYR) + UPTGENR(IRG,CURIYR)
      UPTMSNR(MNUMNR,CURIYR) = UPTMSNR(MNUMNR,CURIYR) + UPTMSNR(IRG,CURIYR)
      UPTWDNR(MNUMNR,CURIYR) = UPTWDNR(MNUMNR,CURIYR) + UPTWDNR(IRG,CURIYR)
!     UPTCFNR(MNUMNR,CURIYR) = UPTCFNR(MNUMNR,CURIYR) + UPTCFNR(IRG,CURIYR)
      UPTSONR(MNUMNR,CURIYR) = UPTSONR(MNUMNR,CURIYR) + UPTSONR(IRG,CURIYR)
      UPTPVNR(MNUMNR,CURIYR) = UPTPVNR(MNUMNR,CURIYR) + UPTPVNR(IRG,CURIYR)
      UPTPTNR(MNUMNR,CURIYR) = UPTPTNR(MNUMNR,CURIYR) + UPTPTNR(IRG,CURIYR)
      UPTWNNR(MNUMNR,CURIYR) = UPTWNNR(MNUMNR,CURIYR) + UPTWNNR(IRG,CURIYR)
      UPTWLNR(MNUMNR,CURIYR) = UPTWLNR(MNUMNR,CURIYR) + UPTWLNR(IRG,CURIYR)
      UPTWFNR(MNUMNR,CURIYR) = UPTWFNR(MNUMNR,CURIYR) + UPTWFNR(IRG,CURIYR)
      UPTTLNR(MNUMNR,CURIYR) = UPTTLNR(MNUMNR,CURIYR) + &
                               UPTCLNR(IRG,CURIYR) + &
                               UPTNGNR(IRG,CURIYR) + &
                               UPTNUNR(IRG,CURIYR) + &
                               UPTHYNR(IRG,CURIYR) + &
                               UPTGENR(IRG,CURIYR) + &
                               UPTMSNR(IRG,CURIYR) + &
                               UPTWDNR(IRG,CURIYR) + &
!                              UPTCFNR(IRG,CURIYR) + &
                               UPTSONR(IRG,CURIYR) + &
                               UPTPVNR(IRG,CURIYR) + &
                               UPTPTNR(IRG,CURIYR) + &
                               UPTWNNR(IRG,CURIYR) + &
                               UPTWLNR(IRG,CURIYR) + &
                               UPTWFNR(IRG,CURIYR)
      UPTCCST(MNUMNR,CURIYR) = UPTCCST(MNUMNR,CURIYR) + UPTCCST(IRG,CURIYR)
      END IF                     ! IRG LE (UNRGNS)
      END DO
!ADJUST EFD NOX EMISSIONS TO AGREE WITH ECP
      IF (NOXBYGRP(2,CURIYR) .GT. 0.0 .OR. NOXBYGRP(3,CURIYR) .GT. 0.0) THEN
        DO IRG = 1 , MNUMNR
          UNOXINR(IRG,CURIYR) = UNOXINR(IRG,CURIYR) *  &
          (ECP_QNOX(2,CURIYR) + ECP_QNOX(3,CURIYR)) * 0.001 /  &
          (UNOXINR(MNUMNR,CURIYR) + UNOXOTR(MNUMNR,CURIYR))
          UNOXOTR(IRG,CURIYR) = UNOXOTR(IRG,CURIYR) *  &
          (ECP_QNOX(2,CURIYR) + ECP_QNOX(3,CURIYR)) * 0.001 /  &
          (UNOXINR(MNUMNR,CURIYR) + UNOXOTR(MNUMNR,CURIYR))
        END DO
      END IF

!RPS CREDITS PRICE
      URPSPRC(CURIYR) = RENEWCR(CURIYR)
!     write(6,3456) curiyr+1989,eprpslm(curiyr),urpsprc(curiyr)
!3456 format(1h ,'!cesout',i4,2f10.3)
      DO IRG = 1 , UNRGNS
         URPSCST(IRG,CURIYR) = URPSPRC(CURIYR) * UCRTLNR(2,IRG,CURIYR)
         URPSPEN(IRG,CURIYR) = EPRPSCP(CURIYR) * UCRPNNR(IRG,CURIYR)
      END DO
!RPS CREDIT/PENALTY PAYMENTS
      URPSCST(MNUMNR,CURIYR) = URPSPRC(CURIYR) * UCRTLNR(2,MNUMNR,CURIYR)
      URPSPEN(MNUMNR,CURIYR) = EPRPSCP(CURIYR) * EPRPSCR(CURIYR)
!
!NATIONAL TOTAL - DEMAND BY SECTOR
      DO IRG = 1,UNRGNS
         QELRSN(MNUMNR,CURIYR) = QELRSN(MNUMNR,CURIYR) + QELRSN(IRG,CURIYR)
         QELCMN(MNUMNR,CURIYR) = QELCMN(MNUMNR,CURIYR) + QELCMN(IRG,CURIYR)
         QELINN(MNUMNR,CURIYR) = QELINN(MNUMNR,CURIYR) + QELINN(IRG,CURIYR)
         QELTRN(MNUMNR,CURIYR) = QELTRN(MNUMNR,CURIYR) + QELTRN(IRG,CURIYR)
         QELASN(MNUMNR,CURIYR) = QELASN(MNUMNR,CURIYR) + QELASN(IRG,CURIYR)
      END DO
      QELRSN(MNUMNR,CURIYR) = QELRSN(MNUMNR,CURIYR) + &
      QELRSN(ALASKA,CURIYR) + QELRSN(HAWAII,CURIYR)
      QELCMN(MNUMNR,CURIYR) = QELCMN(MNUMNR,CURIYR) + &
      QELCMN(ALASKA,CURIYR) + QELCMN(HAWAII,CURIYR)
      QELINN(MNUMNR,CURIYR) = QELINN(MNUMNR,CURIYR) + &
      QELINN(ALASKA,CURIYR) + QELINN(HAWAII,CURIYR)
      QELTRN(MNUMNR,CURIYR) = QELTRN(MNUMNR,CURIYR) + &
      QELTRN(ALASKA,CURIYR) + QELTRN(HAWAII,CURIYR)
      QELASN(MNUMNR,CURIYR) = QELASN(MNUMNR,CURIYR) + &
      QELASN(ALASKA,CURIYR) + QELASN(HAWAII,CURIYR)
! RPS CREDIT BASELINE (WITH EXCLUDED GENERATION, IF ANY)
      DO IRG = 1 , UNRGNS
         UCRBSNR(IRG,CURIYR) = URPSTGN(IRG)
      END DO
      UCRBSNR(MNUMNR,CURIYR) = URPSTGN(MNUMNR)
! RPS PENALTY CREDITS
      IF (EPRPSCR(CURIYR) .GT. 0.0)THEN
         RPSPCT = (UCRTLNR(2,MNUMNR,CURIYR) + EPRPSCR(CURIYR)) /  &
          ((QELASN(MNUMNR,CURIYR) - QELASN(ALASKA,CURIYR) - QELASN(HAWAII,CURIYR)) * 0.001)
         UCRPNNR(MNUMNR,CURIYR) = 0.0
         DO IRG = 1 , UNRGNS
            UCRPNNR(IRG,CURIYR) = MAX(0.0,RPSPCT * (QELASN(IRG,CURIYR) * 0.001) - UCRTLNR(2,IRG,CURIYR))
            UCRPNNR(MNUMNR,CURIYR) = UCRPNNR(MNUMNR,CURIYR) + UCRPNNR(IRG,CURIYR)
         END DO
         DO IRG = 1 , UNRGNS
            UCRPNNR(IRG,CURIYR) = EPRPSCR(CURIYR) * UCRPNNR(IRG,CURIYR) / UCRPNNR(MNUMNR,CURIYR)
         END DO
      ELSE
         DO IRG = 1 , UNRGNS
            UCRPNNR(IRG,CURIYR) = 0.0
         END DO
      END IF
         UCRPNNR(MNUMNR,CURIYR) = EPRPSCR(CURIYR)

!WRITE(*,19)UFLCLNR(1,MNUMNR,CURIYR),
!+ UFLDSNR(1,MNUMNR,CURIYR),UFLRLNR(1,MNUMNR,CURIYR),
!+ UFLRHNR(1,MNUMNR,CURIYR),UFLGFNR(1,MNUMNR,CURIYR),
!+ UFLGINR(1,MNUMNR,CURIYR),
!+ UFLGCNR(1,MNUMNR,CURIYR),UFLURNR(1,MNUMNR,CURIYR),
!+ UFLPSNR(1,MNUMNR,CURIYR),UFLHYNR(1,MNUMNR,CURIYR),
!+ UFLOTNR(1,MNUMNR,CURIYR),UFLTLNR(1,MNUMNR,CURIYR)
!19 FORMAT('CL=',F12.2,'DS=',F12.2,'RL=',F12.2,'RH=',F12.2,'GF=',
!+  F12.2,'FS=',F12.2,'GI=',F12.2,'GC=',F12.2,'UF=',F12.2,
!+ 'PS=',F12.2,'HY=',F12.2,'OT=',F12.2,'TL=',F12.2)
!
!
!     FOR HISTORICAL AND/OR STEO YEARS, OVERWRITE GENERATION BY FUEL TYPE
!
      IF (USW_OVER .GT. 0) THEN
         IF (USW_OVER .EQ. 1) THEN
            UYR_OVER = UYR_HIST
         ELSE
            UYR_OVER = UYR_STEO
         END IF
         IF (((CURIYR + UHBSYR) .LE. UYR_OVER)     &
             .AND. ((CURIYR + UHBSYR) .GE. HSTYEAR)) THEN
!
!           GENERATION BY FUEL TYPE AND GENERATION BY PLANT TYPE FOR TABLE 59
!
            DO IRG = 1 , MNUMNR
               DO IOWNER = 1 , 2
                  UGNCLNR(IOWNER,IRG,CURIYR) =  &
                     HGNCLNR(IOWNER,IRG,CURIYR)
!
!        add historical other generation to oil starting aeo2006.
!        add historical other generation to msw starting aeo2008.
!        add historical other to psgen, put nonbio back to msw. aeo2019
!
                  IF (CURITR .EQ. 1) THEN
                    HGNPSNR(IOWNER,IRG,CURIYR) =  &
                      HGNPSNR(IOWNER,IRG,CURIYR) + HGNOTNR(IOWNER,IRG,CURIYR)
                  ENDIF
!
!                 SHARE TOTAL GAS GENERATION AMONG TYPES USING EMM SHARES
!
                  TOTGEN = UGNGFNR(IOWNER,IRG,CURIYR) + &
                     UGNGINR(IOWNER,IRG,CURIYR) +  &
                     UGNGCNR(IOWNER,IRG,CURIYR)
                  IF (TOTGEN .GT. 0.0) THEN
                     UGNGFNR(IOWNER,IRG,CURIYR) =  &
                        HGNNGNR(IOWNER,IRG,CURIYR) * &
                        UGNGFNR(IOWNER,IRG,CURIYR) / TOTGEN
                     UGNGINR(IOWNER,IRG,CURIYR) =  &
                        HGNNGNR(IOWNER,IRG,CURIYR) * &
                        UGNGINR(IOWNER,IRG,CURIYR) / TOTGEN
                     UGNGCNR(IOWNER,IRG,CURIYR) =  &
                        HGNNGNR(IOWNER,IRG,CURIYR) * &
                        UGNGCNR(IOWNER,IRG,CURIYR) / TOTGEN
                  ELSE
                     UGNGFNR(IOWNER,IRG,CURIYR) = 0.0
                     UGNGINR(IOWNER,IRG,CURIYR) = 0.0
                     UGNGCNR(IOWNER,IRG,CURIYR) =  &
                        HGNNGNR(IOWNER,IRG,CURIYR)
                  END IF
!
!                 SHARE TOTAL OIL GENERATION AMONG PRODUCTS USING EMM SHARES
!
                  TOTGEN = UGNDSNR(IOWNER,IRG,CURIYR) + &
                     UGNRLNR(IOWNER,IRG,CURIYR) +  &
                     UGNRHNR(IOWNER,IRG,CURIYR)
                  IF (TOTGEN .GT. 0.0) THEN
                     UGNDSNR(IOWNER,IRG,CURIYR) =  &
                        HGNOLNR(IOWNER,IRG,CURIYR) * &
                        UGNDSNR(IOWNER,IRG,CURIYR) / TOTGEN
                     UGNRLNR(IOWNER,IRG,CURIYR) =  &
                        HGNOLNR(IOWNER,IRG,CURIYR) * &
                        UGNRLNR(IOWNER,IRG,CURIYR) / TOTGEN
                     UGNRHNR(IOWNER,IRG,CURIYR) =  &
                        HGNOLNR(IOWNER,IRG,CURIYR) * &
                        UGNRHNR(IOWNER,IRG,CURIYR) / TOTGEN
                  ELSE
                     UGNDSNR(IOWNER,IRG,CURIYR) = 0.0
                     UGNRLNR(IOWNER,IRG,CURIYR) = HGNOLNR(IOWNER,IRG,CURIYR)
                     UGNRHNR(IOWNER,IRG,CURIYR) = 0.0
                  END IF
!
                  UGNURNR(IOWNER,IRG,CURIYR) =  HGNURNR(IOWNER,IRG,CURIYR)
                  UGNPSNR(IOWNER,IRG,CURIYR) =  HGNPSNR(IOWNER,IRG,CURIYR)
                  UGNHYNR(IOWNER,IRG,CURIYR) =  HGNHYNR(IOWNER,IRG,CURIYR)
                  UGNGENR(IOWNER,IRG,CURIYR) =  HGNGENR(IOWNER,IRG,CURIYR)
                  UGNMSNR(IOWNER,IRG,CURIYR) =  HGNMSNR(IOWNER,IRG,CURIYR)
                  UGNWDNR(IOWNER,IRG,CURIYR) =  HGNWDNR(IOWNER,IRG,CURIYR)
                  UGNSONR(IOWNER,IRG,CURIYR) =  HGNSONR(IOWNER,IRG,CURIYR)
                  UGNPVNR(IOWNER,IRG,CURIYR) =  HGNPVNR(IOWNER,IRG,CURIYR)
                  UGNPTNR(IOWNER,IRG,CURIYR) =  HGNPTNR(IOWNER,IRG,CURIYR)
                  UGNWNNR(IOWNER,IRG,CURIYR) =  HGNWNNR(IOWNER,IRG,CURIYR)
                  UGNWLNR(IOWNER,IRG,CURIYR) =  HGNWLNR(IOWNER,IRG,CURIYR)
                  UGNWFNR(IOWNER,IRG,CURIYR) =  HGNWFNR(IOWNER,IRG,CURIYR)
                  UGNHONR(IOWNER,IRG,CURIYR) = &
                     HGNHYNR(IOWNER,IRG,CURIYR) + &
                     HGNGENR(IOWNER,IRG,CURIYR) + &
                     HGNMSNR(IOWNER,IRG,CURIYR) + &
                     HGNWDNR(IOWNER,IRG,CURIYR) + &
                     HGNSONR(IOWNER,IRG,CURIYR) + &
                     HGNPVNR(IOWNER,IRG,CURIYR) + &
                     HGNPTNR(IOWNER,IRG,CURIYR) + &
                     HGNWNNR(IOWNER,IRG,CURIYR) + &
                     HGNWLNR(IOWNER,IRG,CURIYR) + &
                     HGNWFNR(IOWNER,IRG,CURIYR)
                  UGNTLNR(IOWNER,IRG,CURIYR) = &
                     UGNCLNR(IOWNER,IRG,CURIYR) + &
                     UGNDSNR(IOWNER,IRG,CURIYR) + &
                     UGNRLNR(IOWNER,IRG,CURIYR) + &
                     UGNRHNR(IOWNER,IRG,CURIYR) + &
                     UGNGFNR(IOWNER,IRG,CURIYR) + &
                     UGNGINR(IOWNER,IRG,CURIYR) + &
                     UGNGCNR(IOWNER,IRG,CURIYR) + &
                     UGNURNR(IOWNER,IRG,CURIYR) + &
                     UGNHONR(IOWNER,IRG,CURIYR) + &
                     UGNPSNR(IOWNER,IRG,CURIYR)
               END DO
!
!              Generation by Plant Type for Table 59
!
               UGENPC(IRG,CURIYR) = HGENPC(4,IRG,CURIYR)
               UGENIG(IRG,CURIYR) = HGENIG(4,IRG,CURIYR)
               UGENIS(IRG,CURIYR) = HGENIS(4,IRG,CURIYR)
               UGENIS_ALT(IRG,CURIYR) = 0.0
               UGENOS(IRG,CURIYR) = HGENST(4,IRG,CURIYR)
               UGENCC(IRG,CURIYR) = HGENCC(4,IRG,CURIYR)
               UGENAC(IRG,CURIYR) = HGENAC(4,IRG,CURIYR)
               UGENCS(IRG,CURIYR) = HGENAS(4,IRG,CURIYR)
               UGENCS_ALT(IRG,CURIYR) = 0.0
               UGENIC(IRG,CURIYR) = HGENIC(4,IRG,CURIYR)
               UGENCT(IRG,CURIYR) = HGENCT(4,IRG,CURIYR)
               UGENT2(IRG,CURIYR) = 0.0
               UGENAT(IRG,CURIYR) = HGENAT(4,IRG,CURIYR)
               UGENNU(IRG,CURIYR) = HGENNU(4,IRG,CURIYR)
               UGENPS(IRG,CURIYR) = HGENPS(4,IRG,CURIYR) + WNGMSEL(CURIYR,IRG)
               UGENFC(IRG,CURIYR) = HGENFC(4,IRG,CURIYR)
               UGENRN(IRG,CURIYR) = HGENRN(4,IRG,CURIYR) - WNGMSEL(CURIYR,IRG)
               UGENDG(IRG,CURIYR) = HGENDG(4,IRG,CURIYR)
               UGENDS(IRG,CURIYR) = HGENDS(4,IRG,CURIYR)
!
            END DO
!
!NON-TRADITIONAL COGENERATION   (moved to here to add nt consumption for renewable q calcs.
!
        DO IRG = 1,MNUMNR
          CGOTGEN(IRG,CURIYR,2) = 0.0
          DO IFL=1,10
            DO IGRD = 1,2
!
!   msw splits now taken care of in historical data
!             HCNTGEN(IRG,CURIYR,6,IGRD) = HCNTGEN(IRG,CURIYR,6,IGRD) + HCNTGEN(IRG,CURIYR,10,IGRD)
!             HCNTGEN(IRG,CURIYR,10,IGRD) = 0.0
!
              CGNTGEN(IRG,CURIYR,IFL,IGRD) = HCNTGEN(IRG,CURIYR,IFL,IGRD)
              IF (IGRD .EQ. 2) THEN
                CGOTGEN(IRG,CURIYR,2) = CGOTGEN(IRG,CURIYR,2) &
                                        + HCNTGEN(IRG,CURIYR,IFL,IGRD) * .001
              ENDIF
            END DO ! IGRD
          END DO ! IFL
!
!     Overwrite NT consumption variables (coal,oil,gas)
!
          CGNTQ(IRG,CURIYR,1) = HCNTQNR(IRG,CURIYR,1)
          CGNTQ(IRG,CURIYR,2) = HCNTQNR(IRG,CURIYR,2) + HCNTQNR(IRG,CURIYR,3)
          CGNTQ(IRG,CURIYR,3) = HCNTQNR(IRG,CURIYR,4)
!
!      Calculate renewable consumption based on overwritten historical ntgen numbers.
!       replace heat rates AER hr to match aer historical consumption
!         CONSUMPTION EQUIVALENT AT RENEWABLE SOURCES
!
          CGNTQ(IRG,CURIYR,4) = (CGNTGEN(IRG,CURIYR,4,1) + &
             CGNTGEN(IRG,CURIYR,4,2)) * HFOSHR(CURIYR)/1000000.
          CGNTQ(IRG,CURIYR,5) = (CGNTGEN(IRG,CURIYR,5,1) + &
             CGNTGEN(IRG,CURIYR,5,2)) * HFOSHR(CURIYR)/1000000.
          CGNTQ(IRG,CURIYR,6) = (CGNTGEN(IRG,CURIYR,6,1) + &
             CGNTGEN(IRG,CURIYR,6,2)) * HMSWHR(CURIYR)/1000000.
          CGNTQ(IRG,CURIYR,7) = (CGNTGEN(IRG,CURIYR,7,1) + &
             CGNTGEN(IRG,CURIYR,7,2)) * HWODHR(CURIYR)/1000000.
          CGNTQ(IRG,CURIYR,8) = (CGNTGEN(IRG,CURIYR,8,1) + &
             CGNTGEN(IRG,CURIYR,8,2)) * HFOSHR(CURIYR)/1000000.

!
!         IPP Generation own-use
!
          CGOTGEN(IRG,CURIYR,1) = 0.0
          DO IFL=1,17
             CGOTGEN(IRG,CURIYR,1) = CGOTGEN(IRG,CURIYR,1) + HCIPGEN(IRG,CURIYR,IFL,2) * .001
          END DO ! IFL
!
        END DO  ! IRG
!
!
!           CENSUS REGION RENEWABLE CONSUMPTION EQUIVALENT
!           THE CODE WAS MOVED HERE TO OVERWRITE THE CENSUS REGION VARIABLE
!           TO MATCH THE NERC REGION VARIABLE FOR AEO95 - WILL FIX INPUTS
!           FOR NEXT YEAR
!
!           FUEL CONSUMPTION
!
!NERC REGIONS
!
      DO IRG = 1 , MNUMNR
!
!       Add non-traditional cogen to HFL IPP variables 
!
        IF ( CURITR .EQ. 1 ) THEN
          HFLCLNR(2,IRG,CURIYR) = HFLCLNR(2,IRG,CURIYR) + HCNTQNR(IRG,CURIYR,1)
          HFLDSNR(2,IRG,CURIYR) = HFLDSNR(2,IRG,CURIYR) + HCNTQNR(IRG,CURIYR,2)
          HFLRSNR(2,IRG,CURIYR) = HFLRSNR(2,IRG,CURIYR) + HCNTQNR(IRG,CURIYR,3)
          HFLNGNR(2,IRG,CURIYR) = HFLNGNR(2,IRG,CURIYR) + HCNTQNR(IRG,CURIYR,4)
        ENDIF
!
        DO IOWNER = 1 , 2

         UFLCLNR(IOWNER,IRG,CURIYR) = HFLCLNR(IOWNER,IRG,CURIYR)
!
!     SHARE TOTAL GAS GENERATION AMONG TYPES USING EMM SHARES
!
         TOTCON = UFLGFNR(IOWNER,IRG,CURIYR) + &
           UFLGINR(IOWNER,IRG,CURIYR) + UFLGCNR(IOWNER,IRG,CURIYR)
         IF(TOTCON .GT. 0.0) THEN
           UFLGFNR(IOWNER,IRG,CURIYR) = HFLNGNR(IOWNER,IRG,CURIYR) * &
            UFLGFNR(IOWNER,IRG,CURIYR) / TOTCON
           UFLGINR(IOWNER,IRG,CURIYR) = HFLNGNR(IOWNER,IRG,CURIYR) * &
            UFLGINR(IOWNER,IRG,CURIYR) / TOTCON
           UFLGCNR(IOWNER,IRG,CURIYR) = HFLNGNR(IOWNER,IRG,CURIYR) * &
            UFLGCNR(IOWNER,IRG,CURIYR) / TOTCON
         ELSE
           UFLGFNR(IOWNER,IRG,CURIYR) = 0.0
           UFLGINR(IOWNER,IRG,CURIYR) = 0.0
           UFLGCNR(IOWNER,IRG,CURIYR) = HFLNGNR(IOWNER,IRG,CURIYR)
         END IF
!
!        SHARE TOTAL OIL GENERATION AMONG PRODUCTS USING EMM SHARES
!        TOTCON = UFLDSNR(IOWNER,IRG,CURIYR) + &
         UFLDSNR(IOWNER,IRG,CURIYR) = HFLDSNR(IOWNER,IRG,CURIYR)
         TOTCON = UFLRLNR(IOWNER,IRG,CURIYR) + UFLRHNR(IOWNER,IRG,CURIYR)
         IF(TOTCON .GT. 0.0) THEN
           UFLRLNR(IOWNER,IRG,CURIYR) = HFLRSNR(IOWNER,IRG,CURIYR) * &
            UFLRLNR(IOWNER,IRG,CURIYR) / TOTCON
           UFLRHNR(IOWNER,IRG,CURIYR) = HFLRSNR(IOWNER,IRG,CURIYR) * &
            UFLRHNR(IOWNER,IRG,CURIYR) / TOTCON
         ELSE
           UFLRLNR(IOWNER,IRG,CURIYR) = HFLRSNR(IOWNER,IRG,CURIYR)
           UFLRHNR(IOWNER,IRG,CURIYR) = 0.0
         END IF
!        NUCLEAR FUEL USE IS GENERATION TIME MER HEAT RATE
!        renewable fuel use is generation times aer heat rate, geo hrate or fossil fuel hr
         UFLURNR(IOWNER,IRG,CURIYR) = UGNURNR(IOWNER,IRG,CURIYR) * &
             HNUCHR(CURIYR) * FACT2
         UFLGTNR(IOWNER,IRG,CURIYR) = (UGNGENR(IOWNER,IRG,CURIYR) * &
             HFOSHR(CURIYR) * FACT2)
         UFLWDNR(IOWNER,IRG,CURIYR) = (UGNWDNR(IOWNER,IRG,CURIYR) * &
             HWODHR(CURIYR) * FACT2)
         UFLMSNR(IOWNER,IRG,CURIYR) = (UGNMSNR(IOWNER,IRG,CURIYR) * &
             HMSWHR(CURIYR) * FACT2)
         UFLHYNR(IOWNER,IRG,CURIYR) = (UGNHYNR(IOWNER,IRG,CURIYR) * &
             HFOSHR(CURIYR) * FACT2)
         UFLWNNR(IOWNER,IRG,CURIYR) = ((UGNWNNR(IOWNER,IRG,CURIYR) + UGNWLNR(IOWNER,IRG,CURIYR)) * &
             HFOSHR(CURIYR) * FACT2)
         UFLWFNR(IOWNER,IRG,CURIYR) = (UGNWFNR(IOWNER,IRG,CURIYR) * &
             HFOSHR(CURIYR) * FACT2)
         UFLSONR(IOWNER,IRG,CURIYR) = (UGNSONR(IOWNER,IRG,CURIYR) * &
             HFOSHR(CURIYR) * FACT2)
         UFLPVNR(IOWNER,IRG,CURIYR) = ((UGNPVNR(IOWNER,IRG,CURIYR) + UGNPTNR(IOWNER,IRG,CURIYR)) * &
             HFOSHR(CURIYR) * FACT2)
!
!       add nonbio back in to ms cons for ftab balancing
!
         UFLMSNR(IOWNER,IRG,CURIYR) = UFLMSNR(IOWNER,IRG,CURIYR) +   &
             (WNCMSELN(CURIYR,IRG) * 1000. * (UFLMSNR(IOWNER,IRG,CURIYR)/(UFLMSNR(1,IRG,CURIYR) + UFLMSNR(2,IRG,CURIYR))))


       END DO

!        adjust nonbio consumption so matches historical heat rate
!      WNCMSELN(CURIYR,IRG) = WNGMSEL(CURIYR,IRG) * HMSWHR(CURIYR) * FACT3
!
!     add nt consumptions to ufl variables
!
        UFLGTNR(2,IRG,CURIYR) = UFLGTNR(2,IRG,CURIYR) + CGNTQ(IRG,CURIYR,5)
        UFLWDNR(2,IRG,CURIYR) = UFLWDNR(2,IRG,CURIYR) + CGNTQ(IRG,CURIYR,7)
        UFLMSNR(2,IRG,CURIYR) = UFLMSNR(2,IRG,CURIYR) + CGNTQ(IRG,CURIYR,6)
        UFLHYNR(2,IRG,CURIYR) = UFLHYNR(2,IRG,CURIYR) + CGNTQ(IRG,CURIYR,4)
        UFLPVNR(2,IRG,CURIYR) = UFLPVNR(2,IRG,CURIYR) + CGNTQ(IRG,CURIYR,8)
!
      END DO
!GET NERC REGION NUCLEAR TOTALS FROM HISTORICAL DATA
      TOTCOLN = UFLCLNR(1,MNUMNR,CURIYR) + UFLCLNR(2,MNUMNR,CURIYR)
!     TOTNUCN = UGNURNR(1,MNUMNR,CURIYR) * NUCHRT * FACT2
      TOTNUCN = UFLURNR(1,MNUMNR,CURIYR) + UFLURNR(2,MNUMNR,CURIYR)
      TOTGEON = UFLGTNR(1,MNUMNR,CURIYR) + UFLGTNR(2,MNUMNR,CURIYR)
      TOTWODN = UFLWDNR(1,MNUMNR,CURIYR) + UFLWDNR(2,MNUMNR,CURIYR)
      TOTHYDN = UFLHYNR(1,MNUMNR,CURIYR) + UFLHYNR(2,MNUMNR,CURIYR)
      TOTMSWN = UFLMSNR(1,MNUMNR,CURIYR) + UFLMSNR(2,MNUMNR,CURIYR)
      TOTSOLN = UFLSONR(1,MNUMNR,CURIYR) + UFLSONR(2,MNUMNR,CURIYR)
      TOTPVTN = UFLPVNR(1,MNUMNR,CURIYR) + UFLPVNR(2,MNUMNR,CURIYR)
      TOTWNDN = UFLWNNR(1,MNUMNR,CURIYR) + UFLWNNR(2,MNUMNR,CURIYR) +   &
                UFLWFNR(1,MNUMNR,CURIYR) + UFLWFNR(2,MNUMNR,CURIYR)
!
      TOTCOLC = 0.0
      TOTNUCC = 0.0
      TOTGEOC = 0.0
      TOTHYDC = 0.0
      TOTMSWC = 0.0
      TOTWODC = 0.0
      TOTSOLC = 0.0
      TOTPVTC = 0.0
      TOTWNDC = 0.0
!DETERMINE CENSUS DIVISION NUCLEAR TOTALS FROM MODEL OUTPUT
      DO ICENSUS = 1 , MNUMCR - 1
        TOTCOLC = TOTCOLC + QCLEL(ICENSUS,CURIYR)
        TOTNUCC = TOTNUCC + QUREL(ICENSUS,CURIYR)
        TOTGEOC = TOTGEOC + QGEEL(ICENSUS,CURIYR)
        TOTHYDC = TOTHYDC + QHOEL(ICENSUS,CURIYR)
        TOTMSWC = TOTMSWC + QMSEL(ICENSUS,CURIYR)
        TOTWODC = TOTWODC + QBMEL(ICENSUS,CURIYR)
        TOTSOLC = TOTSOLC + QSTEL(ICENSUS,CURIYR)
        TOTPVTC = TOTPVTC + QPVEL(ICENSUS,CURIYR)
        TOTWNDC = TOTWNDC + QWIEL(ICENSUS,CURIYR)
      END DO
!OVERWRITE CENSUS DIVISION TOTALS IN HISTORICAL YEARS FOR ALL
!FUELS
     DO ICENSUS = 1 , MNUMCR - 1
!  ADJUST CENSUS DIVISION COAL AND NUCLEAR CONSUMPTION BY RATIO OF
!  NATIONAL OVERWRITE NUMBER TO NATIONAL MODEL NUMBER
!     IF(TOTCOLC .GT. 0.0) THEN                      ! $$$ ADDED WCW $$$
!     QCLEL(ICENSUS,CURIYR) = QCLEL(ICENSUS,CURIYR) *
!    + TOTCOLN / TOTCOLC
!     ELSE
!     WRITE(6,*) '** EMMDSPO ERR: TOTCOLC IS 0.0 FOR CURIYR=',
!    + CURIYR
!     ENDIF
      QCLEL(ICENSUS,CURIYR) = HFLCLCR(ICENSUS,CURIYR)
!
      IF(TOTNUCC .GT. 0.0) THEN                      ! $$$ ADDED WCW $$$
        QUREL(ICENSUS,CURIYR) = QUREL(ICENSUS,CURIYR) * TOTNUCN / TOTNUCC
      ELSE
        WRITE(6,'(A,I4)') '** EMMDSPO ERR: TOTNUCC IS 0.0 FOR CURIYR=',CURCALYR
        QUREL(ICENSUS,CURIYR) = 1.0
      ENDIF
      IF (TOTGEOC .GT. 0.0) QGEEL(ICENSUS,CURIYR) = QGEEL(ICENSUS,CURIYR) * (TOTGEON/TOTGEOC)
      IF (TOTHYDC .GT. 0.0) QHOEL(ICENSUS,CURIYR) = QHOEL(ICENSUS,CURIYR) * (TOTHYDN/TOTHYDC)
      IF (TOTMSWC .GT. 0.0) QMSEL(ICENSUS,CURIYR) = QMSEL(ICENSUS,CURIYR) * (TOTMSWN/TOTMSWC)
      IF (TOTWODC .GT. 0.0) QBMEL(ICENSUS,CURIYR) = QBMEL(ICENSUS,CURIYR) * (TOTWODN/TOTWODC)
      IF (TOTSOLC .GT. 0.0) QSTEL(ICENSUS,CURIYR) = QSTEL(ICENSUS,CURIYR) * (TOTSOLN/TOTSOLC)
      IF (TOTPVTC .GT. 0.0) QPVEL(ICENSUS,CURIYR) = QPVEL(ICENSUS,CURIYR) * (TOTPVTN/TOTPVTC)
      IF (TOTWNDC .GT. 0.0) QWIEL(ICENSUS,CURIYR) = QWIEL(ICENSUS,CURIYR) * (TOTWNDN/TOTWNDC)


!  SHARE TOTAL GAS GENERATION AMONG TYPES USING EMM SHARES
      TOTCON = QGFEL(ICENSUS,CURIYR) + &
      QGIEL(ICENSUS,CURIYR)
      IF(TOTCON .GT. 0.0) THEN
        QGFEL(ICENSUS,CURIYR) = HFLNGCR(ICENSUS,CURIYR) * &
          QGFEL(ICENSUS,CURIYR) / TOTCON
        QGIEL(ICENSUS,CURIYR) = HFLNGCR(ICENSUS,CURIYR) * &
          QGIEL(ICENSUS,CURIYR) / TOTCON
      ELSE
        QGFEL(ICENSUS,CURIYR) = 0.0
        QGIEL(ICENSUS,CURIYR) = HFLNGCR(ICENSUS,CURIYR)
      END IF
      QNGEL(ICENSUS,CURIYR) = HFLNGCR(ICENSUS,CURIYR)
!  USE EMM DS TOTALS AND ALLOCATE REST OF HISTORICAL OIL TO RESID
!     TOTCON = QDSEL(ICENSUS,CURIYR) + &
!     QRLEL(ICENSUS,CURIYR) + QRHEL(ICENSUS,CURIYR)
      QDSEL(ICENSUS,CURIYR) = HFLDSCR(ICENSUS,CURIYR)
      TOTCON = QRLEL(ICENSUS,CURIYR) + QRHEL(ICENSUS,CURIYR)
!     IF(TOTCON .GT. 0.0) THEN
!       QDSEL(ICENSUS,CURIYR) = HFLOLCR(ICENSUS,CURIYR) * &
!       QDSEL(ICENSUS,CURIYR) / TOTCON
      IF(TOTCON .GT. 0.0) THEN
        QRLEL(ICENSUS,CURIYR) = HFLRSCR(ICENSUS,CURIYR) * &
                                   QRLEL(ICENSUS,CURIYR) / TOTCON
        QRHEL(ICENSUS,CURIYR) = HFLRSCR(ICENSUS,CURIYR)  * &
                                   QRHEL(ICENSUS,CURIYR) / TOTCON
      ELSE
        QRLEL(ICENSUS,CURIYR) = HFLRSCR(ICENSUS,CURIYR)
        QRHEL(ICENSUS,CURIYR) = 0.0
      END IF
!     ELSE
!       QDSEL(ICENSUS,CURIYR) = 0.0
!       QRLEL(ICENSUS,CURIYR) = HFLOLCR(ICENSUS,CURIYR)
!       QRHEL(ICENSUS,CURIYR) = 0.0
!     END IF
      QPCEL(ICENSUS,CURIYR) = 0.0
      QRSEL(ICENSUS,CURIYR) = QRLEL(ICENSUS,CURIYR) + &
      QRHEL(ICENSUS,CURIYR)
      QTPEL(ICENSUS,CURIYR) = HFLOLCR(ICENSUS,CURIYR)
!
!JJ       IF(CURIYR.EQ.1) THEN
!JJ       WRITE(22,*) '  CENSUS REG ', ICENSUS, 'FIRM GAS=',
!JJ     +  QGFEL(ICENSUS,CURIYR),' INTERRUPT=',QGIEL(ICENSUS,CURIYR),
!JJ     +  ' TOTAL=', QNGEL(ICENSUS,CURIYR)
!JJ       END IF
!
!        adjust nonbio consumption so matches historical heat rate
!      WNCMSEL(CURIYR,ICENSUS) = WNGMSELC(CURIYR,ICENSUS) * HMSWHR(CURIYR) * FACT3
!
     END DO


!     WNCMSEL(CURIYR,MNUMCR) = WNGMSELC(CURIYR,MNUMCR) * HMSWHR(CURIYR) * FACT3
!
!GAS CONSUMPTION OVERWRITES BY GAS REGION
      DO IRG = 1,NNGEM
!  DETERMINE SHARE OF TOTAL FOR FIRM, INTERRUPT & COMPET GAS
!  TO SHARE BY GAS REGION
!
        TOTCON = QGFELGR(IRG,CURIYR) + &
        QGIELGR(IRG,CURIYR) + QGCELGR(IRG,CURIYR)
!
!  SHARE REGIONAL CONSUMPTION FROM 759 TO FIRM, INTER, COMP GAS
!
!    FILL IN NEW NGMM TOTAL CONSUMPTION VARIABLE
        QNGELGR(IRG,CURIYR) = HFLNGGR(IRG,CURIYR)
!
        IF(TOTCON .GT. 0.0) THEN
          QGFELGR(IRG,CURIYR) = HFLNGGR(IRG,CURIYR) * &
            QGFELGR(IRG,CURIYR) / TOTCON
          QGIELGR(IRG,CURIYR) = HFLNGGR(IRG,CURIYR) * &
            QGIELGR(IRG,CURIYR) / TOTCON
          QGCELGR(IRG,CURIYR) = HFLNGGR(IRG,CURIYR) * &
            QGCELGR(IRG,CURIYR) / TOTCON
        ELSE
          QGFELGR(IRG,CURIYR) = 0.0
          QGIELGR(IRG,CURIYR) = 0.0
          QGCELGR(IRG,CURIYR) = HFLNGGR(IRG,CURIYR)
        END IF
!JJ        WRITE(*,1234) IRG,QGFELGR(IRG,CURIYR),QGIELGR(IRG,CURIYR),
!JJ     +  QGCELGR(IRG,CURIYR),
!JJ     +  HFLNGGR(IRG,CURIYR)

!JJ1234    FORMAT('   REG=',I4,'   FIRM=',F10.2,
!JJ     +        '  INT=',F10.2,'  COMP =',F10.2,
!JJ     +  ' TOTAL =',F10.2)
!
!       SHARE REGIONAL GAS CONSUMPTION AMONG SEASONS USING HISTORICAL SEASONAL GAS PERCENTAGES
!
        DO ISP = 1 , EFDns
          SQNGELGR(IRG,CURIYR,ISP) = HFLNGGR(IRG,CURIYR) * HSEAGASPER(IRG,ISP,CURIYR)
        END DO
!
      END DO
!
    END IF
!ACCUMULATE NAIONAL TOTALS
    IF (((CURIYR + UHBSYR) .LE. UYR_EMIS)     &
          .AND. ((CURIYR + UHBSYR) .GE. HSTYEAR)) THEN

!EMISSIONS
      DO IPOL = 1 , 6
        EMEL(1,IPOL,CURIYR) = HEMNGTL(IPOL,CURIYR)
        EMEL(2,IPOL,CURIYR) = HEMOLTL(IPOL,CURIYR)
        EMEL(3,IPOL,CURIYR) = HEMCLTL(IPOL,CURIYR)
        EMEL(4,IPOL,CURIYR) = HEMOTTL(IPOL,CURIYR)
       DO ICENSUS = 1 , MNUMCR
        IF(EMELC(MNUMCR,IPOL,CURIYR) .GT. 0.0) THEN
          EMELC(ICENSUS,IPOL,CURIYR) = EMELC(ICENSUS,IPOL,CURIYR) * &
          (HEMNGTL(IPOL,CURIYR) + HEMOLTL(IPOL,CURIYR) + &
          HEMCLTL(IPOL,CURIYR) + HEMOTTL(IPOL,CURIYR)) / &
          EMELC(MNUMCR,IPOL,CURIYR)
        ELSE                                           ! $$$ WCW ADDED $$$
          WRITE(6,'(A,I4,A,I4)') '** EMMDSPO ERR: EMELC(MNUMCR,IPOL,CURIYR)=0.0 FOR IPOL= ', &
                   IPOL,' YEAR= ',CURCALYR
          EMELC(ICENSUS,IPOL,CURIYR) = 0.001
          EMELC(MNUMCR,IPOL,CURIYR) = EMELC(MNUMCR,IPOL,CURIYR) + 0.001
        ENDIF
       END DO
      END DO

!     write(6,*) ' before so2other over ',(hemngtl(4,curiyr)+hemoltl(4,curiyr)+hemottl(4,curiyr))
!     IF (CURIYR .LE. 11) THEN
        TOTSO2O = SO2OTHER(CURIYR,1) + SO2OTHER(CURIYR,2)
        SO2OTHER(CURIYR,1) = ((HEMNGTL(4,CURIYR) + HEMOLTL(4,CURIYR) + HEMOTTL(4,CURIYR))/.000001) *     &
                          (SO2OTHER(CURIYR,1) / TOTSO2O)
        SO2OTHER(CURIYR,2) = ((HEMNGTL(4,CURIYR) + HEMOLTL(4,CURIYR) + HEMOTTL(4,CURIYR))/.000001) *     &
                          (SO2OTHER(CURIYR,2) / TOTSO2O)
!     ENDIF


!   overwrites so2 emissions from coal model

      TOTUTSECT = 0.0
      DO J = 1 , NUTSEC
        TOTUTSECT = TOTUTSECT + UTTSO2(J,CURIYR)
      ENDDO

      DO J = 1 , NUTSEC
!      write(6,'(a,2I4,F12.6)') ' before clhist yr old uttso2 year isect= ',curiyr,j,uttso2(j,curiyr)
       UTTSO2(J,CURIYR) = (UTTSO2(J,curiyr) / TOTUTSECT) * HEMCLTL(4,curiyr)
!      write(6,'(a,2I4,F12.6)') ' before clhist yr new uttso2 year isect= ',curiyr,j,uttso2(j,curiyr)
      ENDDO

!     REGIONAL NOX EMISSIONS
      DO IRG = 1 , MNUMNR
          IF(UTNOX(IRG,CURIYR).GT.0.0)THEN
             HNOXFAC(IRG) = HEMNOXR(IRG,CURIYR) / &
                ((UNOXINR(IRG,CURIYR) + UNOXOTR(IRG,CURIYR)) * 1000.0)
          ELSE
             HNOXFAC(IRG) = 1.0
          END IF
         DO IFL = 1 , EFD_D_NFL
            UTNOXN(IFL,IRG) = UTNOXN(IFL,IRG) * HNOXFAC(IRG)
         END DO
            UNOXINR(IRG,CURIYR) = UNOXINR(IRG,CURIYR) * HNOXFAC(IRG)
            UNOXOTR(IRG,CURIYR) = UNOXOTR(IRG,CURIYR) * HNOXFAC(IRG)
            UTNOX(IRG,CURIYR) = UTNOX(IRG,CURIYR) * HNOXFAC(IRG)
      END DO

!     REGIONAL CO2 EMISSIONS
      DO IRG = 1 , MNUMNR
          IF(UTCO2(IRG,CURIYR).GT.0.0)THEN
             HCO2FAC(IRG) = HEMCO2R(IRG,CURIYR) / &
                ((UCO2INR(IRG,CURIYR) + UCO2OTR(IRG,CURIYR)) * 1000000.0)
          ELSE
             HCO2FAC(IRG) = 1.0
          END IF
         DO IFL = 1 , EFD_D_NFL
            UTCO2N(IFL,IRG) = UTCO2N(IFL,IRG) * HCO2FAC(IRG)
         END DO
            UCO2INR(IRG,CURIYR) = UCO2INR(IRG,CURIYR) * HCO2FAC(IRG)
            UCO2OTR(IRG,CURIYR) = UCO2OTR(IRG,CURIYR) * HCO2FAC(IRG)
            UCARINR(IRG,CURIYR) = UCARINR(IRG,CURIYR) * HCO2FAC(IRG)
            UCAROTR(IRG,CURIYR) = UCAROTR(IRG,CURIYR) * HCO2FAC(IRG)
            UTCO2(IRG,CURIYR) = UTCO2(IRG,CURIYR) * HCO2FAC(IRG)
      ENDDO

!  REGIONAL SO2 EMISSIONS
      DO IRG = 1 , MNUMNR
          IF(UTSO2(IRG,CURIYR).GT.0.0)THEN
             HSO2FAC(IRG) = HEMSO2R(IRG,CURIYR) / &
                ((USO2INR(IRG,CURIYR) + USO2OTR(IRG,CURIYR)) * 1000.0)
          ELSE
             HSO2FAC(IRG) = 1.0
          END IF
         DO IFL = 1 , EFD_D_NFL
            UTSO2N(IFL,IRG) = UTSO2N(IFL,IRG) * HSO2FAC(IRG)
         END DO

!      WRITE(6,5545) CURIYR+UHBSYR,CURITR,IRG,HEMSO2R(IRG,CURIYR),   &
!       UTSO2(IRG,CURIYR),USO2INR(IRG,CURIYR),USO2OTR(IRG,CURIYR),    &
!       HSO2FAC(IRG)
5545  FORMAT(1H ,'SO2,YR,IT,RG,HSO2,UTSO2,USO2IN,USO2OT,HSO2FAC',    &
         I5,2I3,5F10.3)
            USO2INR(IRG,CURIYR) = USO2INR(IRG,CURIYR) * HSO2FAC(IRG)
            USO2OTR(IRG,CURIYR) = USO2OTR(IRG,CURIYR) * HSO2FAC(IRG)
            UTSO2(IRG,CURIYR) = UTSO2(IRG,CURIYR) * HSO2FAC(IRG)
!      write(6,5572) ' after so2in so2ot utso2 ',uso2inr(irg,curiyr),uso2otr(irg,curiyr),utso2(irg,curiyr)
5572   format(x,a,3(F10.3))

      END DO
     END IF
    END IF
!
!     IMPORTS
!
      IF (((CURIYR + UHBSYR) .LE. UYR_OVER)     &
           .AND. ((CURIYR + UHBSYR) .GE. HSTYEAR)) THEN
         NETIMP = HTIMPF(MNUMNR,CURIYR) + HTIMPE(MNUMNR,CURIYR) &
                - HTEXPF(MNUMNR,CURIYR) - HTEXPE(MNUMNR,CURIYR)
         IMPEMM = UTIMPF(MNUMNR,CURIYR) + UTIMPE(MNUMNR,CURIYR) - &
            UTEXPF(MNUMNR,CURIYR) - UTEXPE(MNUMNR,CURIYR)
         DO IRG = 1 , MNUMNR
            UTIMPF(IRG,CURIYR) = HTIMPF(IRG,CURIYR)
            UTIMPE(IRG,CURIYR) = HTIMPE(IRG,CURIYR)
            UTEXPF(IRG,CURIYR) = HTEXPF(IRG,CURIYR)
            UTEXPE(IRG,CURIYR) = HTEXPE(IRG,CURIYR)
            ULFIME(IRG) = HTIMPF(IRG,CURIYR)
            ULEIME(IRG) = HTIMPE(IRG,CURIYR)
            ULFIXE(IRG) = HTEXPF(IRG,CURIYR)
            ULEIXE(IRG) = HTEXPE(IRG,CURIYR)
         END DO
!
!        DO IRG = 1 , MNUMCR
!           QEIEL(IRG,CURIYR) = QEIEL(IRG,CURIYR) *
!    +         NETIMP / IMPEMM
!        END DO
         QEIEL(MNUMCR,CURIYR) = NETIMP * .003412
         DO ICENSUS = 1 , MNUMCR - 1
           QEIEL(ICENSUS,CURIYR) = QEIEL(MNUMCR,CURIYR) * &
                                  NIMPCRSH(ICENSUS,CURIYR)
         ENDDO
!
      END IF
!
! end-use cogen / prices
!
      IF (((CURIYR + UHBSYR) .LE. UYR_OVER)     &
          .AND. ((CURIYR + UHBSYR) .GE. HSTYEAR)) THEN
!
!
!All end-use cogen overwrites turned off aeo2015 due to parallel runs.
!COGENERATION - REFINERY
!       DO ICENSUS = 1,MNUMCR
!        DO IFL = 1,MNUMCGF
!         DO IGRD = 1,2
!           CGREFGEN(ICENSUS,CURIYR,IFL,IGRD) = HCREGEN(ICENSUS,CURIYR,IFL,IGRD)
!         END DO
!        END DO
!COGENERATION - EOR
!Per request from Joe benneche - turn off eor overwrites starting in aeo2003
!
!        DO IFL = 1,4
!         DO IGRD = 1,2
!           CGOGGEN(ICENSUS,CURIYR,IFL,IGRD) = &
!             HCOGGEN(ICENSUS,CURIYR,IFL,IGRD)
!         END DO
!        END DO
!COGENERATION - INDUSTRIAL
!        DO IFL = 1,10
!         DO IGRD = 1,2
!          CGINDLGEN(ICENSUS,CURIYR,IFL,IGRD) = HCINDGEN(ICENSUS,CURIYR,IFL,IGRD)
!         END DO
!        END DO
!COGENERATION - COMMERCIAL
!Per request from Erin Boedecker - turn off commercial generation overwrites for gas, pv, wind and solar starting in aeo2004.
!  beginning in aeo2014 for parallel run turn off all commercial generation overwrites
!        DO IFL = 1,MNUMCGF
!          IF ( (IFL .NE. 3) .AND. (IFL .NE. 8) .AND. (IFL .NE. 11) .AND. (IFL .NE. 12) ) THEN
!          DO IGRD = 1,2
!            CGCOMMGEN(ICENSUS,CURIYR,IFL,IGRD) = HCCOMGEN(ICENSUS,CURIYR,IFL,IGRD)
!          ENDDO
!          ENDIF
!        END DO
!
!      END DO                        ! end census region do
!
! Overwrite Electricity Prices by census and emm region.
!  also overwriting in util after efp call
!
       DO IRG = 1 , MNUMNR
         PELRSNR(IRG,CURIYR) = HPELRSNR(IRG,CURIYR)
         PELCMNR(IRG,CURIYR) = HPELCMNR(IRG,CURIYR)
         PELINNR(IRG,CURIYR) = HPELINNR(IRG,CURIYR)
         IF ( HPELTRNR(IRG,CURIYR) .GT. 0.0 ) THEN
           PELTRNR(IRG,CURIYR) = HPELTRNR(IRG,CURIYR)
         ENDIF
         PELASNR(IRG,CURIYR) = HPELASNR(IRG,CURIYR)
       ENDDO
!
       DO IRG = 1 , MNUMCR
         PELRS(IRG,CURIYR) = HPELRS(IRG,CURIYR) / 3.412
         PELCM(IRG,CURIYR) = HPELCM(IRG,CURIYR) / 3.412
         PELIN(IRG,CURIYR) = HPELIN(IRG,CURIYR) / 3.412
         IF ( HPELTR(IRG,CURIYR) .GT. 0.0 ) &
           PELTR(IRG,CURIYR) = HPELTR(IRG,CURIYR) / 3.412
         PELAS(IRG,CURIYR) = HPELAS(IRG,CURIYR) / 3.412
       ENDDO
!
      END IF
!END OF OVERWRITE SECTION
!   IF NOT AN OVERWRITE YEAR, REMOVE NONBIO MSW FROM GEN AND ADD TO OTHER
   IF ((CURIYR + UHBSYR) .GT. UYR_OVER)  THEN
!
!  First check for nonbiogenic bing greater than total municipal waste in regions and adjust
!  The adjustment will be to proprtionately remove the negative from the rest of the regions
      MSADJNEG = 0
      ADJNEG = 0.0
      ADJSUM = 0.0
      TOTADJNONNEG = 0.0
      FRACADJ = 0.0
! testing when values are less than 0
      DO IRG = 1, MNUMNR-1
        TOTMSWGEN = UGNMSNR(1,IRG,CURIYR) + UGNMSNR(2,IRG,CURIYR) + ( (CGNTGEN(IRG,CURIYR,6,1) + CGNTGEN(IRG,CURIYR,6,2)) * .001 )
!   write(6,8756) ' mswgen ',totmswgen,ugnmsnr(1,irg,curiyr),ugnmsnr(2,irg,curiyr),cgntgen(irg,curiyr,6,1),cgntgen(irg,curiyr,6,2)
!    write(6,*) ' curiyr irg wngmsel ',curiyr,irg,wngmsel(curiyr,irg)
 8756 format(a,5(2x,F12.5))
         IF (WNGMSEL(CURIYR,IRG) .GT. TOTMSWGEN) THEN
            MSADJNEG(IRG) = 1    ! 1 means this was a problem region
            ADJNEG(IRG) = TOTMSWGEN - WNGMSEL(CURIYR,IRG)
            ADJSUM = ADJSUM + ADJNEG(IRG)
      write(6,'( " In ",I4," region ",I2," municipal waste needs adjusting by ",F8.4,"   Running total=",F8.4)') CURCALYR, IRG, ADJNEG(IRG), ADJSUM
            WNGMSEL(CURIYR,IRG) = TOTMSWGEN
         ELSE
            TOTADJNONNEG = TOTADJNONNEG + WNGMSEL(CURIYR,IRG)
            FRACADJ(IRG) = WNGMSEL(CURIYR,IRG)
         ENDIF
      ENDDO
      IF (sum(MSADJNEG(:)) .NE. 0) &
          write(6,'( " the sum of adjneg for adjusting is  ",F8.4)') sum(ADJNEG(1:MNUMNR-1))
      FRACADJ = FRACADJ / TOTADJNONNEG    ! convert amount in nonnegative regions to fraction of total
!  now adjust
      DO IRG = 1, MNUMNR-1
         IF (MSADJNEG(IRG) .EQ. 0) &        ! if WNGMSEL is too big, set it to:
            WNGMSEL(CURIYR,IRG) = WNGMSEL(CURIYR,IRG) - FRACADJ(IRG) * sum(ADJNEG(1:MNUMNR-1))
         UGNNBNR(1,IRG,CURIYR) = WNGMSEL(CURIYR,IRG) * &
                     (UGNMSNR(1,IRG,CURIYR)/ TOTMSWGEN)
         UGNNBNR(2,IRG,CURIYR) = WNGMSEL(CURIYR,IRG) * &
                     (UGNMSNR(2,IRG,CURIYR)/ TOTMSWGEN)
      ENDDO
      WNGMSEL(CURIYR,MNUMNR) = sum(WNGMSEL(CURIYR,1:MNUMNR-1))
      UGNNBNR(1,MNUMNR,CURIYR) = sum(UGNNBNR(1,1:MNUMNR-1,CURIYR))
      UGNNBNR(2,MNUMNR,CURIYR) = sum(UGNNBNR(2,1:MNUMNR-1,CURIYR))
!
!  Remove non-bio generation from msw generation and total renew generation starting in 2005
!   subtract from util/nonutil/ntgen in proportion to current generation.
!
      DO IRG = 1 , MNUMNR - 1
        TOTMSWGEN = UGNMSNR(1,IRG,CURIYR) + UGNMSNR(2,IRG,CURIYR) + ( (CGNTGEN(IRG,CURIYR,6,1) + CGNTGEN(IRG,CURIYR,6,2)) * .001 )
        IF (TOTMSWGEN .GT. 0.0) THEN
          MSADJ(1) = (UGNMSNR(1,IRG,CURIYR)/TOTMSWGEN) * WNGMSEL(CURIYR,IRG)
          MSADJ(2) = (UGNMSNR(2,IRG,CURIYR)/TOTMSWGEN) * WNGMSEL(CURIYR,IRG)
          MSADJ(3) = (CGNTGEN(IRG,CURIYR,6,1)*.001/TOTMSWGEN) * WNGMSEL(CURIYR,IRG)
          MSADJ(4) = (CGNTGEN(IRG,CURIYR,6,2)*.001/TOTMSWGEN) * WNGMSEL(CURIYR,IRG)
!        write(6,4793) ' masdj ',msadj(1),msadj(2),msadj(3),msadj(4),wngmsel(curiyr,irg)
  4793 format(a,5(2x,F12.5))
          IF (UGNMSNR(1,IRG,CURIYR) .GT. MSADJ(1)) THEN
             ADJUSTMSNR(1) = MSADJ(1)
          ELSE
             ADJUSTMSNR(1) = UGNMSNR(1,IRG,CURIYR)
          ENDIF
          IF (UGNMSNR(2,IRG,CURIYR) .GT. MSADJ(2)) THEN
             ADJUSTMSNR(2) = MSADJ(2)
          ELSE
             ADJUSTMSNR(2) = UGNMSNR(2,IRG,CURIYR)
          ENDIF
          IF (CGNTGEN(IRG,CURIYR,6,1) .GT. MSADJ(3)) THEN
             ADJUSTMSNR(3) = MSADJ(3)
          ELSE
             ADJUSTMSNR(3) = CGNTGEN(IRG,CURIYR,6,1)
          ENDIF
          IF (CGNTGEN(IRG,CURIYR,6,2) .GT. MSADJ(4)) THEN
             ADJUSTMSNR(4) = MSADJ(4)
          ELSE
             ADJUSTMSNR(4) = CGNTGEN(IRG,CURIYR,6,2)
          ENDIF
          UGNMSNR(1,IRG,CURIYR) = UGNMSNR(1,IRG,CURIYR) - MSADJ(1)
          UGNMSNR(2,IRG,CURIYR) = UGNMSNR(2,IRG,CURIYR) - MSADJ(2)
          UGNHONR(1,IRG,CURIYR) = UGNHONR(1,IRG,CURIYR) - MSADJ(1)
          UGNHONR(2,IRG,CURIYR) = UGNHONR(2,IRG,CURIYR) - MSADJ(2)
          CGNTGEN(IRG,CURIYR,6,1) = CGNTGEN(IRG,CURIYR,6,1) - (MSADJ(3) * 1000.)
          CGNTGEN(IRG,CURIYR,6,2) = CGNTGEN(IRG,CURIYR,6,2) - (MSADJ(4) * 1000.)
!  add nonbio to other variables
          UGNPSNR(1,IRG,CURIYR) = UGNPSNR(1,IRG,CURIYR) + MSADJ(1)
          UGNPSNR(2,IRG,CURIYR) = UGNPSNR(2,IRG,CURIYR) + MSADJ(2)
          CGNTGEN(IRG,CURIYR,10,1) = CGNTGEN(IRG,CURIYR,10,1) + (MSADJ(3) * 1000.)
          CGNTGEN(IRG,CURIYR,10,2) = CGNTGEN(IRG,CURIYR,10,2) + (MSADJ(4) * 1000.)
        ENDIF
      ENDDO
! sum up national:
      UGNMSNR(1,MNUMNR,CURIYR) = sum(UGNMSNR(1,1:MNUMNR-1,CURIYR))
      UGNMSNR(2,MNUMNR,CURIYR) = sum(UGNMSNR(2,1:MNUMNR-1,CURIYR))
      UGNPSNR(1,MNUMNR,CURIYR) = sum(UGNPSNR(1,1:MNUMNR-1,CURIYR))
      UGNPSNR(2,MNUMNR,CURIYR) = sum(UGNPSNR(2,1:MNUMNR-1,CURIYR))
      UGNHONR(1,MNUMNR,CURIYR) = sum(UGNHONR(1,1:MNUMNR-1,CURIYR))
      UGNHONR(2,MNUMNR,CURIYR) = sum(UGNHONR(2,1:MNUMNR-1,CURIYR))
      CGNTGEN(MNUMNR,CURIYR,6,1) = sum(CGNTGEN(1:MNUMNR-1,CURIYR,6,1))
      CGNTGEN(MNUMNR,CURIYR,6,2) = sum(CGNTGEN(1:MNUMNR-1,CURIYR,6,2))
      CGNTGEN(MNUMNR,CURIYR,10,1) = sum(CGNTGEN(1:MNUMNR-1,CURIYR,10,1))
      CGNTGEN(MNUMNR,CURIYR,10,2) = sum(CGNTGEN(1:MNUMNR-1,CURIYR,10,2))
!
   ENDIF                                 ! end msw nonbio adjustments in non historical overwrite years.
!
!
      DO IFL = 1 , NCLUT1
         TSUM = 0.0
         DO IRG = 1 , NDRGG
            TSUM = TSUM + QCLCLNR(IRG,CURIYR,IFL)
         END DO
         IF (TSUM .GT. 0.0) THEN
!  the temptation is to remove this rather than swap in PCLELCDR because PCLELCDR is not by IFL
!           WRITE(22,1212) CURIYR+UHBSYR,CURITR,IFL,(PCLCLNR(IRG,CURIYR,IFL),IRG = 1,NDRGG)
!1212       FORMAT(1X,'CLPRC',3(":",I5),<NDREG>(":",F9.3))
            WRITE(22,1313) CURIYR+UHBSYR,CURITR,IFL,(QCLCLNR(IRG,CURIYR,IFL),IRG = 1,NDRGG)
 1313       FORMAT(1X,'CLQTY',3(":",I5),<NDREG>(":",F9.1))
            WRITE(22,1413) CURIYR+UHBSYR,CURITR,IFL,(RCLCLNR(IRG,CURIYR,IFL),IRG = 1,NDRGG)
 1413       FORMAT(1X,'CLRMV',3(":",I5),<NDREG>(":",F9.3))
            WRITE(22,1523) CURIYR+UHBSYR,CURITR,IFL,(SCLCLNR(IRG,CURIYR,IFL),IRG = 1,NDRGG)
 1523       FORMAT(1X,'CLSO2',3(":",I5),<NDREG>(":",F9.3))
            WRITE(22,7613) CURIYR+UHBSYR,CURITR,IFL,(PHGCLNR(IRG,CURIYR,IFL),IRG = 1,NDRGG)
 7613       FORMAT(1X,'CLPHG',3(":",I5),<NDREG>(":",F9.3))
            WRITE(22,7614) CURIYR+UHBSYR,CURITR,IFL,(PSLCLNR(IRG,CURIYR,IFL),IRG = 1,NDRGG)
 7614       FORMAT(1X,'CLPSL',3(":",I5),<NDREG>(":",F9.3))
            WRITE(22,7615) CURIYR+UHBSYR,CURITR,IFL,(PCACLNR(IRG,CURIYR,IFL),IRG = 1,NDRGG)
 7615       FORMAT(1X,'CLPCA',3(":",I5),<NDREG>(":",F9.3))
            WRITE(22,7616) CURIYR+UHBSYR,CURITR,IFL,(HRTCLNR(IRG,CURIYR,IFL),IRG = 1,NDRGG)
 7616       FORMAT(1X,'CLHRT',3(":",I5),<NDREG>(":",F9.1))
         END IF
      END DO
      DO IECP = 1 , ECP_D_DSP
         IF (UPTTYP(IECP) .LE. NW_COAL) THEN
            TSUM = 0.0
            DO IRG = 1 , NDREG
               TSUM = TSUM + UQCOAL(IECP,IRG,CURIYR)
            END DO
            IF (TSUM .GT. 0.0) THEN
               WRITE(22,1314) CURIYR+UHBSYR,CURITR,IECP,(UQCOAL(IECP,IRG,CURIYR),IRG=1,NDREG),TSUM
 1314          FORMAT(1X,"CLECP",3(":",I4),<NDREG>(":",F8.1),":",F8.1)
            END IF
         END IF
      END DO
!
      DO ISO2 = 1 , NUM_SO2_GRP
         WRITE(22,1113) CURIYR+UHBSYR,CURITR,ISO2,EMRFSA(CURIYR,ISO2),EMELBNK(CURIYR,ISO2),SO2OTHER(CURIYR,ISO2),EMELPSO2(CURIYR,ISO2)
      END DO
 1113 FORMAT(1H ,' IYR,ITR,SO2_INFO ',3(":",I4),4(":",F16.3))
!
      DO IRG = 1,MNUMCR
      WRITE(22,1713) CURIYR,CURITR,IRG, &
      UPFUEL(UIDS,IRG), &
      UPFUEL(UIRL,IRG), &
      UPFUEL(UIRH,IRG)
1713  FORMAT(1H ,' IYR,ITR,IRG,OLPRC',3I5,3F8.2)
      WRITE(22,1513) CURIYR,CURITR,IRG, &
      UQFUEL(UIDS,IRG,1) * FACT2, &
      UQFUEL(UIRL,IRG,1) * FACT2, &
      UQFUEL(UIRH,IRG,1) * FACT2
1513  FORMAT(1H ,' IYR,ITR,IRG,OLQTY',3I5,3F8.2)
      END DO
      DO IRG = 1,NNGEM
      WRITE(18,1813) CURIRUN,CURIYR+UHBSYR,CURITR,IRG, &
      UPFUEL(UIGF,IRG), &
      UPFUEL(UIGI,IRG), &
      UPFUEL(UIGC,IRG), &
      UQFUEL(UIGF,IRG,1) * FACT2, &
      UQFUEL(UIGI,IRG,1) * FACT2, &
      UQFUEL(UIGC,IRG,1) * FACT2, &
      SPNGELGR(IRG,CURIYR,1),SPNGELGR(IRG,CURIYR,2),SPNGELGR(IRG,CURIYR,3), &
      SQNGELGR(IRG,CURIYR,1),SQNGELGR(IRG,CURIYR,2),SQNGELGR(IRG,CURIYR,3)
1813  FORMAT(1H ,' IYR,ITR,IRG,NG_PQ',4(":",I5),3(":",F8.4),3(":",F8.2),3(":",F8.4),3(":",F8.2))
      END DO
!
      DO NERC = 1 , UNRGNS
         CALL GETBLD(1,NERC)
         DO IRG = 1 , NNGEM
            IF (EPFLRG(UIGF,IRG) + EPFLRG(UIGI,IRG) + EPFLRG(UIGC,IRG) .GT. 0.0) THEN
               WRITE(22,5813) CURIYR+UHBSYR,CURITR,NERC,IRG, EPFLRG(UIGF,IRG) * FACT2, EPFLRG(UIGI,IRG) * FACT2, EPFLRG(UIGC,IRG) * FACT2
 5813          FORMAT(1X,'EMM_GR_NGQ',4(":",I5),3(":",F8.2))
            END IF
         END DO
         WRITE(18,6813) CURIRUN,CURIYR+UHBSYR,CURITR,NERC, UPGASPRC(NERC,CURIYR), TPGASWGT(NERC) * FACT2, &
                                                   UPDISPRC(NERC,CURIYR), TPDISWGT(NERC) * FACT2, &
                                                   UPRESPRC(NERC,CURIYR), TPRESWGT(NERC) * FACT2, &
                                                   UPCOLPRC(NERC,CURIYR), TPCOLWGT(NERC) * FACT2, &
                                                   UPNUCPRC(NERC,CURIYR), TPNUCWGT(NERC) * FACT2
 6813    FORMAT(1X,"ECP_FL_PRC",4(":",I4),5(":",F7.3,":",E12.3))
      END DO
      DO IRG = 1 , UNFRGN
         DO IECP = 1 , ECP_D_DSP
            IF (UPMXGAS(IECP,IRG,CURIYR) + UPMXOIL(IECP,IRG,CURIYR) + UPMXCOL(IECP,IRG,CURIYR) .GT. 0.0) &
               WRITE(18,7813) CURIRUN,CURIYR+UHBSYR,CURITR,IRG,IECP, UPMXGAS(IECP,IRG,CURIYR), UPMXOIL(IECP,IRG,CURIYR), UPMXCOL(IECP,IRG,CURIYR)
 7813       FORMAT(1X,"ECP_FL_SHR",5(":",I4),3(":",F9.6))
         END DO
      END DO
!
      DO IRG = 1 , NDREG
         WRITE(22,1613) CURIYR+UHBSYR,CURITR,IRG,UPFUEL(UIWD,IRG),UQFUEL(UIWD,IRG,1) * FACT2
      END DO
 1613 FORMAT(1X,"RNEW_WD",":",I4,":",I2,":",I3,2(":",F9.3))
      DO IRG = 1 , UNRGNS
         WRITE(22,2613) CURIYR+UHBSYR,CURITR,IRG, &
            UQFUEL(UIWD,IRG,3) * FACT2, &
            UQFUEL(UIGT,IRG,1) * FACT2, &
            UQFUEL(UISW,IRG,1) * FACT2, &
            UQFUEL(UIWA,IRG,1) * FACT2, &
            UQFUEL(UIPS,IRG,1) * FACT2, &
            UQFUEL(UISO,IRG,1) * FACT2, &
            UQFUEL(UIPV,IRG,1) * FACT2, &
            UQFUEL(UIPT,IRG,1) * FACT2, &
            UQFUEL(UIWN,IRG,1) * FACT2, &
            UQFUEL(UIWL,IRG,1) * FACT2, &
            UQFUEL(UIWF,IRG,1) * FACT2
      END DO
 2613 FORMAT(1X,"RNEW_EL",":",I4,":",I2,":",I3,11(":",F9.3))
!
      DO IRG = 1 , MNUMCR
         WRITE(22,3613) CURIYR+UHBSYR,CURITR,IRG, &
            QBMEL(IRG,CURIYR),UQFUEL(UIWD,IRG,2) * FACT2, &
            QGEEL(IRG,CURIYR),UQFUEL(UIGT,IRG,2) * FACT2, &
            QMSEL(IRG,CURIYR),UQFUEL(UISW,IRG,2) * FACT2, &
            QHOEL(IRG,CURIYR),UQFUEL(UIWA,IRG,2) * FACT2, &
            UQFUEL(UIPS,IRG,2) * FACT2, &
            QSTEL(IRG,CURIYR),UQFUEL(UISO,IRG,2) * FACT2, &
            QPVEL(IRG,CURIYR),(UQFUEL(UIPV,IRG,2) + UQFUEL(UIPT,IRG,2)) * FACT2, &
            QWIEL(IRG,CURIYR),(UQFUEL(UIWN,IRG,2) + UQFUEL(UIWL,IRG,2)) * FACT2, &
            UQFUEL(UIWF,IRG,2) * FACT2
      END DO
 3613 FORMAT(1X,"RNEW_CR",":",I4,":",I2,":",I3,16(":",F9.3))
!
!  WRITE OUT RENEWABLE PORTFOLIO STANDARD COSTS AND PERCENTAGES
!
      IF (EPRPSPR(CURIYR) .GT. 0.0 .AND. &
         (FCRL .EQ. 1 .OR. CURITR .GT. MAXITR)) THEN
      DO IRG = 1 , UNRGNS
         WRITE(22,3131) CURIYR+1989,IRG, &
                       URPSRGN(IRG),URPSTGN(IRG),URPSPCT(IRG)*100.0, &
                       URPSCRD(IRG),URPSCRD(IRG) * EPRPSPR(CURIYR)
3131  FORMAT(1H ,'RPSCOSTS, YR = ',I4,'  REG = ',I2, &
         '  RPSREN = ',F8.1,'  RPSTOT = ',F8.1,'  RPSPCT = ',F5.1, &
         '  RPSCRD = ',F10.1,'  RPSREV = ',F10.1)
      END DO
         WRITE(22,3132) CURIYR+1989,UPRNWBND(CURIYR)*100.0, &
                        URPSRGN(MNUMNR),URPSTGN(MNUMNR), &
                        URPSPCT(MNUMNR)*100.0,EPRPSPR(CURIYR)
3132  FORMAT(1H ,'RPSDATA, YR = ',I4,'  RPSSTD = ',F5.1, &
          '  RPSREN = ',F8.1,'  RPSTOT = ',F8.1, &
          '  RPSPCT = ',F5.1,'  CRDCST = ',F5.2)
         WRITE(22,3133) CURIYR+1989,URPSRGL(MNUMNR),URPSTGL(MNUMNR)
3133  FORMAT(1H ,'RPSDATA, YR = ',I4, &
          '  LAGREN = ',F8.1,'  LAGTOT = ',F8.1)
      END IF
      DO IRG = 1 , MNUMNR
      IF(UF_RP2 .GT.0 .AND. FCRL .GT. 0) &
        WRITE(UF_RP2,1234) CURIYR+1989,IRG, &
        UGNCFNR(1,IRG,CURIYR) + UGNCFNR(2,IRG,CURIYR)
1234  FORMAT(1H ,'WD COFIRING, YR = ',I4,'  REG = ',I2,'  BKWH = ',F8.3)
      END DO
!
!     Process Traditional Cogen Generation Values
!
!     National totals for cogen for ftab
      DO IRG = 1, UNRGNS
       DO IFL = 1, MNUMCGF
        CGTOTGENNR(MNUMNR,CURIYR,IFL,1) = CGTOTGENNR(MNUMNR,CURIYR,IFL,1) + CGTOTGENNR(IRG,CURIYR,IFL,1)
        CGTOTGENNR(MNUMNR,CURIYR,IFL,2) = CGTOTGENNR(MNUMNR,CURIYR,IFL,2) + CGTOTGENNR(IRG,CURIYR,IFL,2)
        CGTOTCAPNR(MNUMNR,CURIYR,IFL) = CGTOTCAPNR(MNUMNR,CURIYR,IFL) + CGTOTCAPNR(IRG,CURIYR,IFL)
        CGTOTQNR(MNUMNR,CURIYR,IFL) = CGTOTQNR(MNUMNR,CURIYR,IFL) + CGTOTQNR(IRG,CURIYR,IFL)
       ENDDO
      ENDDO

!
!     Account for SO2 and HG emissions from CTL in so2other and hgother for coal model
!
!     DO CRG = 1, NDREG
!       DO ISO2 = 1 , NUM_SO2_GRP
!       IF (SO2_SHR_BY_CLRG(CRG,ISO2) .GT. 0.0) THEN
!          SO2OTHER(CURIYR,ISO2) = SO2OTHER(CURIYR,ISO2) + CTLSO2EM(CRG,CURIYR) * SO2_SHR_BY_CLRG(CRG,ISO2)
!       ENDIF
!       ENDDO
!       HGOTHER(CURIYR) = HGOTHER(CURIYR) + CTLHGEM(CRG,CURIYR)
!     ENDDO
!     WRITE(22,1668) CURIYR,(SO2OTHER(CURIYR,ISO2),ISO2=1,NUM_SO2_GRP),HGOTHER(CURIYR)
!1668 FORMAT(1x,'AFTER CTL - SO2OTHER',I6,2F10.3,'HGOTHER',F10.3)
!
!     Store AB32 Output in Global Arrays
!     Reset Cap to 0 if no AB32
      IF (AB_CAP_TOT(CURIYR) .GT. 9000.0)AB_CAP_TOT(CURIYR) = 0.0
      AB_ALLOW_P(CURIYR) = UCARPRC(CARGRP_CA,CURIYR) * 0.001
!     AB_CAP_ELE(CURIYR) = CARTGT(CARGRP_CA,CURIYR)
!     AB_COVD_EM_ELE(CURIYR) = CAREFD(CARGRP_CA,CURIYR)
      AB_COVD_EM_ELE(CURIYR) = AB32UTIL
      AB_OFFSET_USED(CURIYR) = AB32OUSE
      AB_CSTCONT_USE(CURIYR) = AB32RUSE
      IF ((CURIYR + UHBSYR) .EQ. UESTYR .AND. CURITR .EQ. 1)THEN
         AB_ALLBANK_AVL = 0.0
         AB_RSVYR = 9999
      END IF
      IF (AB_RSVYR .EQ. 9999 .AND. (AB_CSTCONT_AVL(CURIYR) .GT. 0.0 .OR. AB_CSTCONT_FRAC(CURIYR) .GT. 0.0))AB_RSVYR = CURIYR + UHBSYR
      IF ((CURIYR + UHBSYR) .GE. AB_RSVYR .AND. (CURIYR + 1) .LE. MNUMYR)THEN
         AB_CSTCONT_AVL(CURIYR + 1) = MAX(0.0,AB32RAVL - AB32RUSE)
      END IF
      IF (AB_CAP_TOT(CURIYR) .GT. 0.0 .AND. AB_CAP_TOT(CURIYR) .LT. 9000.0)THEN
!     if (curitr .gt. 1)write(6,3434) curiyr+1989,  &
!        AB32BAVL , AB32BBNK , AB32BUSE,  &
!        AB_ALLBANK_AVL(CURIYR)
!3434 format(1h ,'!ab32bnk',i4,4f10.3)
!        IF ((CURIYR + UHBSYR) .LT. UPSTYR)THEN
         IF ((CURIYR + UHBSYR) .LT. 2015)THEN
            AB_ALLBANK_AVL(CURIYR) = 0.0
         ELSE
            AB_ALLBANK_AVL(CURIYR) = MAX(0.0,(AB32BAVL + AB32BBNK - AB32BUSE))
         END IF
      END IF
!     Add other sector emissions to get total for output table in emmrept
!     write(6,4444) curiyr+1989,  &
!     CAREFD(CARGRP_CA,CURIYR) ,  &
!     AB_COVD_EM_IND(CURIYR) , AB_COVD_EM_REF(CURIYR) , AB_COVD_EM_FUE(CURIYR) , AB_COVD_EM_OTH(CURIYR),  &
!     CAREFD(CARGRP_CA,CURIYR) +  &
!     AB_COVD_EM_IND(CURIYR) + AB_COVD_EM_REF(CURIYR) + AB_COVD_EM_FUE(CURIYR) + AB_COVD_EM_OTH(CURIYR)
!4444 format(1h ,'!ab32',i4,8f10.3)
!     CAREFD(CARGRP_CA,CURIYR) = CAREFD(CARGRP_CA,CURIYR) +  &
!     AB_COVD_EM_IND(CURIYR) + AB_COVD_EM_REF(CURIYR) + AB_COVD_EM_FUE(CURIYR) + AB_COVD_EM_OTH(CURIYR)
!
!     Store RGGI Output in Global Arrays
!     Reset Cap to 0 if no RGGI
      IF (RG_CAP_TOT(CURIYR) .GT. 9000.0)RG_CAP_TOT(CURIYR) = 0.0
      RG_ALLOW_P(CURIYR) = UCARPRC(CARGRP_RG,CURIYR)
      RG_COVD_EM_ELE(CURIYR) = RGGIUTIL
      RG_OFFSETS_USE(CURIYR) = RGGIOUSE
      RG_RESERVE_USE(CURIYR) = RGGIRUSE
      RG_RSRVECR_USE(CURIYR) = RGGIEUSE
!
!     ACCUMULATE CARBON SHARES BY FUEL REGION/STATE TO USE FOR UNPLANNED BUILDS
!
!     ECARCLSH = 0.0
!     ECAROGSH = 0.0
!     DO IFL = 1 , UNFRGN
!        DO IRG = 1 , EMM_D_ST
!           if (curitr .gt. 1 .and. ifl .eq. 1)write(6,7878) curiyr+1989,flrgcode(ifl),ustnme(irg), ECARCLS(IRG,CURIYR) , ECARCLF(IFL,CURIYR),  &
!                                           ECARCLS(IRG,CURIYR) / ECARCLF(IFL,CURIYR)
!7878 format(1h ,'!carshr',i4,a4,a4,3f10.3)
!           IF (EST_FRG(IRG) .EQ. IFL)THEN
!              IF (ECARCLF(IFL,CURIYR) .GT. 0.0)ECARCLSH(IFL,IRG) = ECARCLS(IRG,CURIYR) / ECARCLF(IFL,CURIYR)
!              IF (ECAROGF(IFL,CURIYR) .GT. 0.0)ECAROGSH(IFL,IRG) = ECAROGS(IRG,CURIYR) / ECAROGF(IFL,CURIYR)
!           END IF
!        END DO
!     END DO
!     do irg = 1 , EMM_D_ST
!     if (curitr .gt. 1)write(6,8989) curiyr+1989,ustnme(irg),(ecarclsh(ifl,irg),ifl=1,unfrgn)
!8989 format(1h ,'!clshr',i4,a3,23f5.2)
!     if (curitr .gt. 1)write(6,8990) curiyr+1989,ustnme(irg),(ecarogsh(ifl,irg),ifl=1,unfrgn)
!8990 format(1h ,'!ogshr',i4,a3,23f5.2)
!     end do
!
      IF (FCRL .EQ. 1 .OR. CURITR .GT. MAXITR)THEN
!     AB32 Results
         IF ((CURIYR + UHBSYR) .EQ. UESTYR)write(13,3455)
 3455 format(1h ,'!ab32efd',T10,'YEAR',T15,'ITER',T21,'  EMIS LMT',T31,'   UTL EMS',T41,'   BNK AVL',T51,'   BNK USE',  &
                                               T61,'  OFFS AVL',T71,'  OFFS USE',T81,'   RSV AVL',T91,'   RSV USE',  &
                                              T101,'   ESC QTY',T111,'  ALLW PRC')
         IF (AB_CAP_TOT(CURIYR) .GT. 0.0 .AND. AB_CAP_TOT(CURIYR) .LT. 9000.0)THEN
            write(13,3456) curiyr+1989,curitr,ab_cap_tot(curiyr)*(1.0 - ab_cstcont_frac(curiyr))*44.0/12.0,  &
                           ab32util*44.0/12.0,ab32bavl*44.0/12.0,ab32buse*44.0/12.0,  &
                           ab32oavl*44.0/12.0,ab32ouse*44.0/12.0,ab32ravl*44.0/12.0,ab32ruse*44.0/12.0,ab32escp*44.0/12.0,  &
                           ucarprc(cargrp_ca,curiyr)*(12.0/44.0)*scalpr
 3456 format(1h ,'!ab32efd',t10,i4,t15,i4,t21,10f10.1)
         END IF
         IF (CURIYR .EQ. LASTYR)THEN
!           State Level Carbon -- Coal
            write(13,4000) ((USYEAR(IYR),USYEAR(IYR)), IYR = 22 , 31)
 4000 format(1h ,'!stcarcl',T15,'  ',3X,20I6)
            write(13,4050)
 4050 format(1h ,'!stcarcl',T15,'ST',3X,10('   EFD','   ECP'))
            DO IRG = 1 , UNSTAS
               write(13,4100) USTNME(IRG),((ECARCLS(IRG,IYR),PCARCLS(IRG,IYR)),IYR = 22 , 31)
 4100 format(1h ,'!stcarcl',T15,A2,3X,20F6.1)
            END DO
               write(13,4150) ((ECARCLS(EMM_D_ST + 1,IYR),PCARCLS(EMM_D_ST + 1,IYR)),IYR = 22 , 31)
 4150 format(1h ,'!stcarcl',T15,'US',3X,20F6.1)
               write(13,4200)
 4200 format(1h ,'!stcarcl')
            write(13,4000) ((USYEAR(IYR),USYEAR(IYR)), IYR = 32 , 41)
            write(13,4050)
            DO IRG = 1 , UNSTAS
               write(13,4100) USTNME(IRG),((ECARCLS(IRG,IYR),PCARCLS(IRG,IYR)),IYR = 32 , 41)
            END DO
               write(13,4150) ((ECARCLS(EMM_D_ST + 1,IYR),PCARCLS(EMM_D_ST + 1,IYR)),IYR = 32 , 41)
               write(13,4200)
            write(13,4000) ((USYEAR(IYR),USYEAR(IYR)), IYR = 42 , 51)
            write(13,4050)
            DO IRG = 1 , UNSTAS
               write(13,4100) USTNME(IRG),((ECARCLS(IRG,IYR),PCARCLS(IRG,IYR)),IYR = 42 , 51)
            END DO
               write(13,4150) ((ECARCLS(EMM_D_ST + 1,IYR),PCARCLS(EMM_D_ST + 1,IYR)),IYR = 42 , 51)
               write(13,4200)
               write(13,4200)
!           State Level Carbon -- Gas/Oil
            write(13,5000) ((USYEAR(IYR),USYEAR(IYR)), IYR = 22 , 31)
 5000 format(1h ,'!stcarog',T15,'  ',3X,20I6)
            write(13,5050)
 5050 format(1h ,'!stcarog',T15,'ST',3X,10('   EFD','   ECP'))
            DO IRG = 1 , UNSTAS
               write(13,5100) USTNME(IRG),((ECAROGS(IRG,IYR),PCAROGS(IRG,IYR)),IYR = 22 , 31)
 5100 format(1h ,'!stcarog',T15,A2,3X,20F6.1)
            END DO
               write(13,5150) ((ECAROGS(EMM_D_ST + 1,IYR),PCAROGS(EMM_D_ST + 1,IYR)),IYR = 22 , 31)
 5150 format(1h ,'!stcarog',T15,'US',3X,20F6.1)
               write(13,5200)
 5200 format(1h ,'!stcarog')
            write(13,5000) ((USYEAR(IYR),USYEAR(IYR)), IYR = 32 , 41)
            write(13,5050)
            DO IRG = 1 , UNSTAS
               write(13,5100) USTNME(IRG),((ECAROGS(IRG,IYR),PCAROGS(IRG,IYR)),IYR = 32 , 41)
            END DO
               write(13,5150) ((ECAROGS(EMM_D_ST + 1,IYR),PCAROGS(EMM_D_ST + 1,IYR)),IYR = 32 , 41)
               write(13,5200)
            write(13,5000) ((USYEAR(IYR),USYEAR(IYR)), IYR = 42 , 51)
            write(13,5050)
            DO IRG = 1 , UNSTAS
               write(13,5100) USTNME(IRG),((ECAROGS(IRG,IYR),PCAROGS(IRG,IYR)),IYR = 42 , 51)
            END DO
               write(13,5150) ((ECAROGS(EMM_D_ST + 1,IYR),PCAROGS(EMM_D_ST + 1,IYR)),IYR = 42 , 51)
               write(13,5200)
               write(13,5200)

!           State Level Carbon -- Total

            write(13,5500) ((USYEAR(IYR),USYEAR(IYR)), IYR = 22 , 31)
 5500 format(1h ,'!stcartl',T15,'  ',3X,20I6)
            write(13,5550)
 5550 format(1h ,'!stcartl',T15,'ST',3X,10('   EFD','   ECP'))
            DO IRG = 1 , UNSTAS
               write(13,5600) USTNME(IRG),(((ECARCLS(IRG,IYR) + ECAROGS(IRG,IYR)),(PCARCLS(IRG,IYR) + PCAROGS(IRG,IYR))),IYR = 22 , 31)
 5600 format(1h ,'!stcartl',T15,A2,3X,20F6.1)
            END DO
               write(13,5650) (((ECARCLS(EMM_D_ST + 1,IYR) + ECAROGS(EMM_D_ST + 1,IYR)),(PCARCLS(EMM_D_ST + 1,IYR) + PCAROGS(EMM_D_ST + 1,IYR))),IYR = 22 , 31)
 5650 format(1h ,'!stcartl',T15,'US',3X,20F6.1)
               write(13,5700)
 5700 format(1h ,'!stcartl')
            write(13,5500) ((USYEAR(IYR),USYEAR(IYR)), IYR = 32 , 41)
            write(13,5550)
            DO IRG = 1 , UNSTAS
               write(13,5600) USTNME(IRG),(((ECARCLS(IRG,IYR) + ECAROGS(IRG,IYR)),(PCARCLS(IRG,IYR) + PCAROGS(IRG,IYR))),IYR = 32 , 41)
            END DO
               write(13,5650) (((ECARCLS(EMM_D_ST + 1,IYR) + ECAROGS(EMM_D_ST + 1,IYR)),(PCARCLS(EMM_D_ST + 1,IYR) + PCAROGS(EMM_D_ST + 1,IYR))),IYR = 32 , 41)
               write(13,5700)
            write(13,5500) ((USYEAR(IYR),USYEAR(IYR)), IYR = 42 , 51)
            write(13,5550)
            DO IRG = 1 , UNSTAS
               write(13,5600) USTNME(IRG),(((ECARCLS(IRG,IYR) + ECAROGS(IRG,IYR)),(PCARCLS(IRG,IYR) + PCAROGS(IRG,IYR))),IYR = 42 , 51)
            END DO
               write(13,5650) (((ECARCLS(EMM_D_ST + 1,IYR) + ECAROGS(EMM_D_ST + 1,IYR)),(PCARCLS(EMM_D_ST + 1,IYR) + PCAROGS(EMM_D_ST + 1,IYR))),IYR = 42 , 51)
               write(13,5700)
               write(13,5700)
!
!           FlRgn Level Carbon and Fuel Consumption
!
            write(13,6000) (USYEAR(IYR), IYR = 22 , 36)
 6000 format(1h ,'!flcarcl',T15,'FR',3X,15I8)
            DO IRG = 1 , UNFRGN
               write(13,6100) FLRGCODE(IRG),(ECARCLF(IRG,IYR),IYR = 22 , 36)
 6100 format(1h ,'!flcarcl',T15,A2,3X,15F8.1)
            END DO
               write(13,6150) (ECARCLF(EFD_D_MFRG + 1,IYR),IYR = 22 , 36)
 6150 format(1h ,'!flcarcl',T15,'US',3X,15F8.1)
               write(13,6200)
 6200 format(1h ,'!flcarcl')
               write(13,6300) (USYEAR(IYR), IYR = 37 , 51)
 6300 format(1h ,'!flcarcl',T15,'ST',3X,15I8)
            DO IRG = 1 , UNFRGN
               write(13,6400) FLRGCODE(IRG),(ECARCLF(IRG,IYR),IYR = 37 , 51)
 6400 format(1h ,'!flcarcl',T15,A2,3X,15F8.1)
            END DO
               write(13,6500) (ECARCLF(EFD_D_MFRG + 1,IYR),IYR = 37 , 51)
 6500 format(1h ,'!flcarcl',T15,'US',3X,15F8.1)
               write(13,6200)
               write(13,6200)
!
               write(13,7000) (USYEAR(IYR), IYR = 22 , 36)
 7000 format(1h ,'!flcarog',T15,'FR',3X,15I8)
            DO IRG = 1 , UNFRGN
               write(13,7100) FLRGCODE(IRG),(ECAROGF(IRG,IYR),IYR = 22 , 36)
 7100 format(1h ,'!flcarog',T15,A2,3X,15F8.1)
            END DO
               write(13,7150) (ECAROGF(EFD_D_MFRG + 1,IYR),IYR = 22 , 36)
 7150 format(1h ,'!flcarog',T15,'US',3X,15F8.1)
               write(13,7200)
 7200 format(1h ,'!flcarog')
               write(13,7300) (USYEAR(IYR), IYR = 37 , 51)
 7300 format(1h ,'!flcarog',T15,'ST',3X,15I8)
            DO IRG = 1 , UNFRGN
               write(13,7400) FLRGCODE(IRG),(ECAROGF(IRG,IYR),IYR = 37 , 51)
 7400 format(1h ,'!flcarog',T15,A2,3X,15F8.1)
            END DO
               write(13,7500) (ECAROGF(EFD_D_MFRG + 1,IYR),IYR = 37 , 51)
 7500 format(1h ,'!flcarog',T15,'US',3X,15F8.1)
               write(13,7200)
               write(13,7200)
         END IF
         DO IFL = 1 , ECP_D_CAP + 2
            IF (IFL .EQ. WICN .OR. IFL .EQ. WIAN  .OR. IFL .EQ. WISM .OR. IFL .EQ. WIWD .OR.  &
                IFL .EQ. WIHY .OR. IFL .EQ. WIWN .OR. IFL .EQ. WISO .OR.  &
                IFL .EQ. WIPV .OR. IFL .EQ. WIGT .OR. IFL .GE. (ECP_D_CAP + 1))THEN
               write(13,8000) CURIYR + UHBSYR,PLTTYP(IFL),(URGNME(IYR)(1:4),IYR = 1 , UNRGNS)
 8000 format(1h ,'!nrfrg',I4,A3,T17,2X,<unrgns>A6,'     US')
               DO IRG = 1 , UNFRGN
                  write(13,8100) CURIYR + UHBSYR,PLTTYP(IFL),FLRGCODE(IRG),(EGEN_NRFR(IFL,IYR,IRG),IYR = 1 , UNRGNS),EGEN_NRFR(IFL,MNUMNR,IRG)
 8100 format(1h ,'!nrfrg',I4,A3,T17,A2,<unrgns>F6.1,F7.1)
               END DO
               write(13,8200) CURIYR + UHBSYR,PLTTYP(IFL),(EGEN_NRFR(IFL,IYR,IRG),IYR = 1 , UNRGNS),EGEN_NRFR(IFL,MNUMNR,IRG)
 8200 format(1h ,'!nrfrg',I4,A3,T17,'US',<unrgns>F6.1,F7.1)
               write(13,8500) CURIYR + UHBSYR,PLTTYP(IFL),(URGNME(IYR)(1:4),IYR = 1 , UNRGNS)
 8500 format(1h ,'!nrstg',I4,A3,T17,2X,<unrgns>A6,'     US')
               DO IRG = 1 , EMM_D_ST
                  write(13,8600) CURIYR + UHBSYR,PLTTYP(IFL),USTNME(IRG),(EGEN_NRST(IFL,IYR,IRG),IYR = 1 , UNRGNS),EGEN_NRST(IFL,MNUMNR,IRG)
 8600 format(1h ,'!nrstg',I4,A3,T17,A2,<unrgns>F6.1,F7.1)
               END DO
               write(13,8700) CURIYR + UHBSYR,PLTTYP(IFL),(EGEN_NRST(IFL,IYR,IRG),IYR = 1 , UNRGNS),EGEN_NRST(IFL,MNUMNR,IRG)
 8700 format(1h ,'!nrstg',I4,A3,T17,'US',<unrgns>F6.1,F7.1)
               write(13,9000) CURIYR + UHBSYR,PLTTYP(IFL),(FLRGCODE(IYR),IYR = 1 , UNFRGN)
 9000 format(1h ,'!frstg',I4,A3,T17,2X,23A6,'     US')
               DO IRG = 1 , EMM_D_ST
                  write(13,9100) CURIYR + UHBSYR,PLTTYP(IFL),USTNME(IRG),(EGEN_FRST(IFL,IYR,IRG),IYR = 1 , UNFRGN + 1)
 9100 format(1h ,'!frstg',I4,A3,T17,A2,23F6.1,F7.1)
               END DO
               write(13,9200) CURIYR + UHBSYR,PLTTYP(IFL),(EGEN_FRST(IFL,IYR,IRG),IYR = 1 , UNFRGN + 1)
 9200 format(1h ,'!frstg',I4,A3,T17,'US',23F6.1,F7.1)
            END IF
         END DO
         IF (CURIYR .EQ. LASTYR)THEN
!
!           111d CO2 Target Performance Standards
!
            write(13,9400) (USYEAR(IYR), IYR = 22 , 36)
 9400 format(1h ,'!111dstd',T15,'FR',3X,15I8)
            DO IRG = 1 , UNFRGN
               write(13,9410) FLRGCODE(IRG),(CO2_STDRF(IRG,IYR),IYR = 22 , 36)
 9410 format(1h ,'!111dstd',T15,A2,3X,15F8.0)
            END DO
               write(13,9420)
 9420 format(1h ,'!111dstd')
               write(13,9430) (USYEAR(IYR), IYR = 37 , 51)
 9430 format(1h ,'!111dstd',T15,'FR',3X,15I8)
            DO IRG = 1 , UNFRGN
               write(13,9440) FLRGCODE(IRG),(CO2_STDRF(IRG,IYR),IYR = 37 , 51)
 9440 format(1h ,'!111dstd',T15,A2,3X,15F8.0)
            END DO
               write(13,9420)
               write(13,9420)
!
!           111d CO2 Performance Intensity Results
!
            write(13,9500)
 9500 format(1h ,'!111drte',T25,'CO2 Performance Intensity Results (lbs CO2/Mwh)')
               write(13,9520)
            write(13,9505) (USYEAR(IYR), IYR = 22 , 36)
 9505 format(1h ,'!111drte',T15,'FR',3X,15I8)
            DO IRG = 1 , UNFRGN
               write(13,9510) FLRGCODE(IRG),(ECO2FRQF(IRG,IYR) * 1000.0 / EGENFRQF(IRG,IYR),IYR = 22 , 36)
 9510 format(1h ,'!111drte',T15,A2,3X,15F8.0)
            END DO
               write(13,9520)
 9520 format(1h ,'!111drte')
               write(13,9530) (USYEAR(IYR), IYR = 37 , 51)
 9530 format(1h ,'!111drte',T15,'FR',3X,15I8)
            DO IRG = 1 , UNFRGN
               write(13,9540) FLRGCODE(IRG),(ECO2FRQF(IRG,IYR) * 1000.0 / EGENFRQF(IRG,IYR),IYR = 37 , 51)
 9540 format(1h ,'!111drte',T15,A2,3X,15F8.0)
            END DO
               write(13,9520)
               write(13,9520)
!
!           111d CO2 Intensity Standard Allowance Prices
!
            write(13,9600)
 9600 format(1h ,'!111dprc',T25,'CO2 Performance Intensity Standards Allowance Prices (1000$ Per (lb/CO2/Mwh))')
               write(13,9620)
            write(13,9605) (USYEAR(IYR), IYR = 22 , 36)
 9605 format(1h ,'!111dprc',T15,'FR',3X,15I8)
            DO IRG = 1 , UNFRGN
               write(13,9610) FLRGCODE(IRG),(ECO2FRPR(IRG,IYR) * 1000.0 * SCALPR,IYR = 22 , 36)
 9610 format(1h ,'!111dprc',T15,A2,3X,15F8.0)
            END DO
               write(13,9620)
 9620 format(1h ,'!111dprc')
               write(13,9630) (USYEAR(IYR), IYR = 37 , 51)
 9630 format(1h ,'!111dprc',T15,'FR',3X,15I8)
            DO IRG = 1 , UNFRGN
               write(13,9640) FLRGCODE(IRG),(ECO2FRPR(IRG,IYR) * 1000.0 * SCALPR,IYR = 37 , 51)
 9640 format(1h ,'!111dprc',T15,A2,3X,15F8.0)
            END DO
               write(13,9620)
               write(13,9620)
!
!           111d CO2 Emissions from Affected Plants
!
            write(13,9700)
 9700 format(1h ,'!111deqf',T25,'CO2 Emissions from Affected Plants (MM Metric Tons CO2)')
            write(13,9705) (USYEAR(IYR), IYR = 22 , 36)
 9705 format(1h ,'!111deqf',T15,'FR',3X,15I8)
            DO IRG = 1 , UNFRGN
               write(13,9710) FLRGCODE(IRG),(ECO2FRQF(IRG,IYR) / 2.204,IYR = 22 , 36)
 9710 format(1h ,'!111deqf',T15,A2,3X,15F8.0)
            END DO
               write(13,9715) (ECO2FRQF(EFD_D_MFRG + 1,IYR) / 2.204,IYR = 22 , 36)
 9715 format(1h ,'!111deqf',T15,'US',3X,15F8.0)
               write(13,9720)
 9720 format(1h ,'!111deqf')
               write(13,9730) (USYEAR(IYR), IYR = 37 , 51)
 9730 format(1h ,'!111deqf',T15,'FR',3X,15I8)
            DO IRG = 1 , UNFRGN
               write(13,9740) FLRGCODE(IRG),(ECO2FRQF(IRG,IYR) / 2.204,IYR = 37 , 51)
 9740 format(1h ,'!111deqf',T15,A2,3X,15F8.0)
            END DO
               write(13,9745) (ECO2FRQF(EFD_D_MFRG + 1,IYR) / 2.204,IYR = 37 , 51)
 9745 format(1h ,'!111deqf',T15,'US',3X,15F8.0)
               write(13,9720)
               write(13,9720)
!
!           111d CO2 Emissions from All Plants
!
            write(13,9800)
 9800 format(1h ,'!111deqf',T25,'CO2 Emissions from All Plants (MM Metric Tons CO2)')
            write(13,9805) (USYEAR(IYR), IYR = 22 , 36)
 9805 format(1h ,'!111detl',T15,'FR',3X,15I8)
            DO IRG = 1 , UNFRGN
               write(13,9810) FLRGCODE(IRG),(ECO2FRTL(IRG,IYR) / 2.204,IYR = 22 , 36)
 9810 format(1h ,'!111detl',T15,A2,3X,15F8.0)
            END DO
               write(13,9815) (ECO2FRTL(EFD_D_MFRG + 1,IYR) / 2.204,IYR = 22 , 36)
 9815 format(1h ,'!111detl',T15,'US',3X,15F8.0)
               write(13,9820)
 9820 format(1h ,'!111detl')
               write(13,9830) (USYEAR(IYR), IYR = 37 , 51)
 9830 format(1h ,'!111detl',T15,'FR',3X,15I8)
            DO IRG = 1 , UNFRGN
               write(13,9840) FLRGCODE(IRG),(ECO2FRTL(IRG,IYR) / 2.204,IYR = 37 , 51)
 9840 format(1h ,'!111detl',T15,A2,3X,15F8.0)
            END DO
               write(13,9845) (ECO2FRTL(EFD_D_MFRG + 1,IYR) / 2.204,IYR = 37 , 51)
 9845 format(1h ,'!111detl',T15,'US',3X,15F8.0)
               write(13,9820)
               write(13,9820)
!
!           FlRgn Level Heat Rate Improvements
!
            HRTF = 0.0
            DO IRG = 1 , UNFRGN
               DO IYR = (UPSTYR - UHBSYR) , MNUMYR
                  HRTF(IRG,IYR) = HRTF(IRG,IYR - 1) + ECAPFRHR(IRG,IYR)
                  IF (IRG .EQ. UNFRGN)HRTF(EFD_D_MFRG + 1,IYR) = HRTF(EFD_D_MFRG + 1,IYR - 1) + ECAPFRHR(EFD_D_MFRG + 1,IYR)
               END DO
            END DO
            write(13,9900) (USYEAR(IYR), IYR = 22 , 36)
 9900 format(1h ,'!flhri',T15,'FR',3X,15I8)
            DO IRG = 1 , UNFRGN
               write(13,9910) FLRGCODE(IRG),(HRTF(IRG,IYR),IYR = 22 , 36)
 9910 format(1h ,'!flhri',T15,A2,3X,15F8.1)
            END DO
               write(13,9920) (HRTF(EFD_D_MFRG + 1,IYR),IYR = 22 , 36)
 9920 format(1h ,'!flhri',T15,'US',3X,15F8.1)
               write(13,9925)
 9925 format(1h ,'!flhri')
               write(13,9930) (USYEAR(IYR), IYR = 37 , 51)
 9930 format(1h ,'!flhri',T15,'FR',3X,15I8)
            DO IRG = 1 , UNFRGN
               write(13,9940) FLRGCODE(IRG),(HRTF(IRG,IYR),IYR = 37 , 51)
 9940 format(1h ,'!flhri',T15,A2,3X,15F8.1)
            END DO
               write(13,9945) (HRTF(EFD_D_MFRG + 1,IYR),IYR = 37 , 51)
 9945 format(1h ,'!flhri',T15,'US',3X,15F8.1)
               write(13,9925)
               write(13,9925)
!
!           EMM Level Heat Rate Improvements
!
            HRTN = 0.0
            DO IRG = 1 , UNRGNS
               DO IYR = (UPSTYR - UHBSYR) , MNUMYR
                  HRTN(IRG,IYR) = HRTN(IRG,IYR - 1) + ECAPNRHR(IRG,IYR)
                  IF (IRG .EQ. UNRGNS)HRTN(MNUMNR,IYR) = HRTN(MNUMNR,IYR - 1) + ECAPNRHR(MNUMNR,IYR)
               END DO
            END DO
            write(13,9950) (USYEAR(IYR), IYR = 22 , 36)
 9950 format(1h ,'!nrhri',T15,'NR  ',1X,15I8)
            DO IRG = 1 , UNRGNS
               write(13,9960) URGNME(IRG)(1:4),(HRTN(IRG,IYR),IYR = 22 , 36)
 9960 format(1h ,'!nrhri',T15,A4,1X,15F8.1)
            END DO
               write(13,9970) (HRTN(MNUMNR,IYR),IYR = 22 , 36)
 9970 format(1h ,'!nrhri',T15,'US  ',1X,15F8.1)
               write(13,9975)
 9975 format(1h ,'!nrhri')
               write(13,9980) (USYEAR(IYR), IYR = 37 , 51)
 9980 format(1h ,'!nrhri',T15,'NR  ',1X,15I8)
            DO IRG = 1 , UNRGNS
               write(13,9990) URGNME(IRG)(1:4),(HRTN(IRG,IYR),IYR = 37 , 51)
 9990 format(1h ,'!nrhri',T15,A4,1X,15F8.1)
            END DO
               write(13,9995) (HRTN(MNUMNR,IYR),IYR = 37 , 51)
 9995 format(1h ,'!nrhri',T15,'US  ',1X,15F8.1)
               write(13,9975)
               write(13,9975)
!
!           FlRgn Level Efficiency Savings
!
!           Residential
            write(13,4205) (USYEAR(IYR), IYR = 22 , 36)
 4205 format(1h ,'!fleer',T15,'FR',3X,15I8)
            DO IRG = 1 , UNFRGN
               write(13,4210) FLRGCODE(IRG),(EERF(IRG,IYR),IYR = 22 , 36)
 4210 format(1h ,'!fleer',T15,A2,3X,15F8.1)
            END DO
               write(13,4220) (EERF(EFD_D_MFRG + 1,IYR),IYR = 22 , 36)
 4220 format(1h ,'!fleer',T15,'US',3X,15F8.1)
               write(13,4225)
 4225 format(1h ,'!fleer')
               write(13,4230) (USYEAR(IYR), IYR = 37 , 51)
 4230 format(1h ,'!fleer',T15,'FR',3X,15I8)
            DO IRG = 1 , UNFRGN
               write(13,4240) FLRGCODE(IRG),(EERF(IRG,IYR),IYR = 37 , 51)
 4240 format(1h ,'!fleer',T15,A2,3X,15F8.1)
            END DO
               write(13,4245) (EERF(EFD_D_MFRG + 1,IYR),IYR = 37 , 51)
 4245 format(1h ,'!fleer',T15,'US',3X,15F8.1)
               write(13,4225)
               write(13,4225)
!           Commercial
            write(13,4305) (USYEAR(IYR), IYR = 22 , 36)
 4305 format(1h ,'!fleec',T15,'FR',3X,15I8)
            DO IRG = 1 , UNFRGN
               write(13,4310) FLRGCODE(IRG),(EECF(IRG,IYR),IYR = 22 , 36)
 4310 format(1h ,'!fleec',T15,A2,3X,15F8.1)
            END DO
               write(13,4320) (EECF(EFD_D_MFRG + 1,IYR),IYR = 22 , 36)
 4320 format(1h ,'!fleec',T15,'US',3X,15F8.1)
               write(13,4325)
 4325 format(1h ,'!fleec')
               write(13,4330) (USYEAR(IYR), IYR = 37 , 51)
 4330 format(1h ,'!fleec',T15,'FR',3X,15I8)
            DO IRG = 1 , UNFRGN
               write(13,4340) FLRGCODE(IRG),(EECF(IRG,IYR),IYR = 37 , 51)
 4340 format(1h ,'!fleec',T15,A2,3X,15F8.1)
            END DO
               write(13,4345) (EECF(EFD_D_MFRG + 1,IYR),IYR = 37 , 51)
 4345 format(1h ,'!fleec',T15,'US',3X,15F8.1)
               write(13,4325)
               write(13,4325)
!           Industrial
            write(13,4405) (USYEAR(IYR), IYR = 22 , 36)
 4405 format(1h ,'!fleei',T15,'FR',3X,15I8)
            DO IRG = 1 , UNFRGN
               write(13,4410) FLRGCODE(IRG),(EEIF(IRG,IYR),IYR = 22 , 36)
 4410 format(1h ,'!fleei',T15,A2,3X,15F8.1)
            END DO
               write(13,4420) (EEIF(EFD_D_MFRG + 1,IYR),IYR = 22 , 36)
 4420 format(1h ,'!fleei',T15,'US',3X,15F8.1)
               write(13,4425)
 4425 format(1h ,'!fleei')
               write(13,4430) (USYEAR(IYR), IYR = 37 , 51)
 4430 format(1h ,'!fleei',T15,'FR',3X,15I8)
            DO IRG = 1 , UNFRGN
               write(13,4440) FLRGCODE(IRG),(EEIF(IRG,IYR),IYR = 37 , 51)
 4440 format(1h ,'!fleei',T15,A2,3X,15F8.1)
            END DO
               write(13,4445) (EEIF(EFD_D_MFRG + 1,IYR),IYR = 37 , 51)
 4445 format(1h ,'!fleei',T15,'US',3X,15F8.1)
               write(13,4425)
               write(13,4425)
!           Total
            write(13,4805) (USYEAR(IYR), IYR = 22 , 36)
 4805 format(1h ,'!fleet',T15,'FR',3X,15I8)
            DO IRG = 1 , UNFRGN
               write(13,4810) FLRGCODE(IRG),(EGENFREE(IRG,IYR),IYR = 22 , 36)
 4810 format(1h ,'!fleet',T15,A2,3X,15F8.1)
            END DO
               write(13,4820) (EGENFREE(EFD_D_MFRG + 1,IYR),IYR = 22 , 36)
 4820 format(1h ,'!fleet',T15,'US',3X,15F8.1)
               write(13,4825)
 4825 format(1h ,'!fleet')
               write(13,4830) (USYEAR(IYR), IYR = 37 , 51)
 4830 format(1h ,'!fleet',T15,'FR',3X,15I8)
            DO IRG = 1 , UNFRGN
               write(13,4840) FLRGCODE(IRG),(EGENFREE(IRG,IYR),IYR = 37 , 51)
 4840 format(1h ,'!fleet',T15,A2,3X,15F8.1)
            END DO
               write(13,4845) (EGENFREE(EFD_D_MFRG + 1,IYR),IYR = 37 , 51)
 4845 format(1h ,'!fleet',T15,'US',3X,15F8.1)
               write(13,4825)
               write(13,4825)
!
!           EMM Rgn Level Efficiency Savings
!
!           Residential
            write(13,4505) (USYEAR(IYR), IYR = 22 , 36)
 4505 format(1h ,'!nreer',T15,'NR  ',1X,15I8)
            DO IRG = 1 , UNRGNS
               write(13,4510) URGNME(IRG)(1:4),(EERN(IRG,IYR),IYR = 22 , 36)
 4510 format(1h ,'!nreer',T15,A4,1X,15F8.1)
            END DO
               write(13,4520) (EERN(MNUMNR,IYR),IYR = 22 , 36)
 4520 format(1h ,'!nreer',T15,'US  ',1X,15F8.1)
               write(13,4525)
 4525 format(1h ,'!nreer')
               write(13,4530) (USYEAR(IYR), IYR = 37 , 51)
 4530 format(1h ,'!nreer',T15,'NR  ',1X,15I8)
            DO IRG = 1 , UNRGNS
               write(13,4540) URGNME(IRG)(1:4),(EERN(IRG,IYR),IYR = 37 , 51)
 4540 format(1h ,'!nreer',T15,A4,1X,15F8.1)
            END DO
               write(13,4545) (EERN(MNUMNR,IYR),IYR = 37 , 51)
 4545 format(1h ,'!nreer',T15,'US  ',1X,15F8.1)
               write(13,4525)
               write(13,4525)
!           Commercial
            write(13,4605) (USYEAR(IYR), IYR = 22 , 36)
 4605 format(1h ,'!nreec',T15,'NR  ',1X,15I8)
            DO IRG = 1 , UNRGNS
               write(13,4610) URGNME(IRG)(1:4),(EECN(IRG,IYR),IYR = 22 , 36)
 4610 format(1h ,'!nreec',T15,A4,1X,15F8.1)
            END DO
               write(13,4620) (EECN(MNUMNR,IYR),IYR = 22 , 36)
 4620 format(1h ,'!nreec',T15,'US  ',1X,15F8.1)
               write(13,4625)
 4625 format(1h ,'!nreec')
               write(13,4630) (USYEAR(IYR), IYR = 37 , 51)
 4630 format(1h ,'!nreec',T15,'NR  ',1X,15I8)
            DO IRG = 1 , UNRGNS
               write(13,4640) URGNME(IRG)(1:4),(EECN(IRG,IYR),IYR = 37 , 51)
 4640 format(1h ,'!nreec',T15,A4,1X,15F8.1)
            END DO
               write(13,4645) (EECN(MNUMNR,IYR),IYR = 37 , 51)
 4645 format(1h ,'!nreec',T15,'US  ',1X,15F8.1)
               write(13,4625)
               write(13,4625)
!           Industrial
            write(13,4705) (USYEAR(IYR), IYR = 22 , 36)
 4705 format(1h ,'!nreei',T15,'NR',3X,15I8)
            DO IRG = 1 , UNRGNS
               write(13,4710) URGNME(IRG)(1:4),(EEIN(IRG,IYR),IYR = 22 , 36)
 4710 format(1h ,'!nreei',T15,A2,3X,15F8.1)
            END DO
               write(13,4720) (EEIN(MNUMNR,IYR),IYR = 22 , 36)
 4720 format(1h ,'!nreei',T15,'US',3X,15F8.1)
               write(13,4725)
 4725 format(1h ,'!nreei')
               write(13,4730) (USYEAR(IYR), IYR = 37 , 51)
 4730 format(1h ,'!nreei',T15,'NR',3X,15I8)
            DO IRG = 1 , UNRGNS
               write(13,4740) URGNME(IRG)(1:4),(EEIN(IRG,IYR),IYR = 37 , 51)
 4740 format(1h ,'!nreei',T15,A2,3X,15F8.1)
            END DO
               write(13,4745) (EEIN(MNUMNR,IYR),IYR = 37 , 51)
 4745 format(1h ,'!nreei',T15,'US',3X,15F8.1)
               write(13,4725)
               write(13,4725)
!           Total
            write(13,4905) (USYEAR(IYR), IYR = 22 , 36)
 4905 format(1h ,'!nreet',T15,'NR',3X,15I8)
            DO IRG = 1 , UNRGNS
               write(13,4910) URGNME(IRG)(1:4),(EGENNREE(IRG,IYR),IYR = 22 , 36)
 4910 format(1h ,'!nreet',T15,A2,3X,15F8.1)
            END DO
               write(13,4920) (EGENNREE(MNUMNR,IYR),IYR = 22 , 36)
 4920 format(1h ,'!nreet',T15,'US',3X,15F8.1)
               write(13,4925)
 4925 format(1h ,'!nreet')
               write(13,4930) (USYEAR(IYR), IYR = 37 , 51)
 4930 format(1h ,'!nreet',T15,'NR',3X,15I8)
            DO IRG = 1 , UNRGNS
               write(13,4940) URGNME(IRG)(1:4),(EGENNREE(IRG,IYR),IYR = 37 , 51)
 4940 format(1h ,'!nreet',T15,A2,3X,15F8.1)
            END DO
               write(13,4945) (EGENNREE(MNUMNR,IYR),IYR = 37 , 51)
 4945 format(1h ,'!nreet',T15,'US',3X,15F8.1)
               write(13,4925)
               write(13,4925)
!
            DO IYR = 31 , LASTYR
              IF (CO2_STDRS(1,IYR) .GT. 0.0) THEN
               IF (CO2_STDSW .EQ. 2) THEN          !fuel regions
                 DO IRG = 1 , UNFRGN
                  write(13,3434) iyr+1989,irg,flrgcode(irg),eco2frpr(irg,iyr) * 2204.0 * scalpr, &
                                 ecpco2fy(irg,iyr) * 2.204 * scalpr,ecpco2fl(irg,iyr) * 2.204 * scalpr
                  IF (IRG .eq. UNFRGN) &
                    write(13,3434) iyr+1989,EFD_D_MFRG+1,'US',eco2frpr(EFD_D_MFRG+1,iyr) * 2204.0 * scalpr, &
                                 ecpco2fy(EFD_D_MFRG+1,iyr) * 2.204 * scalpr,ecpco2fl(EFD_D_MFRG+1,iyr) * 2.204 * scalpr

 3434 format(1h ,'!efdpfr',i4,i3,a4,3f10.1)
                 END DO
               ELSEIF (CO2_STDSW .EQ. 3) THEN     !EMM regions
                 DO IRG = 1, UNRGNS
                  write(13,3434) iyr+1989,irg,urgnme(irg)(6:7),eco2nrpr(irg,iyr) * 2204.0 * scalpr, &
                                 ecpco2ny(irg,iyr) * 2.204 * scalpr,ecpco2nl(irg,iyr) * 2.204 * scalpr
                  IF (IRG .eq. UNRGNS) &
                    write(13,3434) iyr+1989,MNUMNR,'US',eco2nrpr(mnumnr,iyr) * 2204.0 * scalpr, &
                                 ecpco2ny(mnumnr,iyr) * 2.204 * scalpr,ecpco2nl(mnumnr,iyr) * 2.204 * scalpr
                 ENDDO
               ENDIF
              ENDIF
            END DO
         END IF
         IF (CO2_STDSW .EQ. 3 .AND. CO2_ERCSW .GT. 0)THEN
!        EMISSION REDUCTION CREDIT (ERC) TRADING -- RATE BASE
            DO IRG = 1 , UNRGNS
               if (irg .eq. 1)then
                  write(13,4950) curiyr+uhbsyr,(urgnme(reg)(1:4),reg = 1 , unrgns)
 4950  format(1h ,'!ercrt',i4,'   REG',22a6,' TLSAL',' TLAVL',' PRICE')
               end if
               write(13,4955) curiyr+1989,urgnme(irg)(1:4),(eercnrqr(irg,reg,curiyr),reg = 1 , unrgns),eercnrqr(irg,mnumnr,curiyr),ercqavln(irg,curiyr),eercnrpr(irg,curiyr) * scalpr
 4955  format(1h ,'!ercrt',i4,a6,25f6.1)
            END DO
            write(13,4960) curiyr+1989,(eercnrqr(mnumnr,reg,curiyr),reg = 1 , unrgns),eercnrqr(mnumnr,mnumnr,curiyr),ercqavln(mnumnr,curiyr),eercnrpr(mnumnr,curiyr) * scalpr
 4960  format(1h ,'!ercrt',i4,' TLPUR',25f6.1)
!           EMISSION REDUCTION CREDIT (ERC) TRADING - MASS BASE
            DO IRG = 1 , UNRGNS
               if (irg .eq. 1)then
                  write(13,4970) curiyr+uhbsyr,(urgnme(reg)(1:4),reg = 1 , unrgns)
 4970  format(1h ,'!ercms',i4,'   REG',22a6,' TLSAL',' PRICE')
               end if
               write(13,4980) curiyr+1989,urgnme(irg)(1:4),(eercnrqm(irg,reg,curiyr),reg = 1 , unrgns),eercnrqm(irg,mnumnr,curiyr),eercnrpm(irg,curiyr) * scalpr
 4980  format(1h ,'!ercms',i4,a6,25f6.1)
            END DO
            write(13,4990) curiyr+1989,(eercnrqm(mnumnr,reg,curiyr),reg = 1 , unrgns),eercnrqm(mnumnr,mnumnr,curiyr),eercnrpm(mnumnr,curiyr) * scalpr
 4990  format(1h ,'!ercms',i4,' TLPUR',25f6.1)
         END IF
      END IF
!
      RETURN
      END
!
!
      SUBROUTINE EMMBODPV

      IMPLICIT NONE
!
!     THIS SUBROUTINE BACKS OUT ANNUAL DISTRIBUTED SOLAR PV GENERATIONS AND CAPACITY ADDITIONS FROM ALL AFFECTED EMM ARRAYS
!     IT USES THE WFLOOR INPUT FEATURE

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'cdsparms'
      include 'control'
      include 'ecpcntl'
      include 'plntctl'
      include 'udatout'     ! UDAT output variables
      include 'uefdout'     ! UEFD output variables
      include 'qblk'
      include 'bildin'
      include 'dispinyr'
      include 'cogen'

      INTEGER IRG,ICENSUS
      REAL QPVELCYR, BO_UCAPPV, BO_DGPVGEN, BO_DGPVCON(MNUMCR)
      REAL BO_UCAPUS, BO_GENUS, BO_CONUS, BO_CONNR, BO_CONNRUS, CMULT
!
      WRITE(22,5010) 'EMMBODPV CAP ',CURIYR,CURITR,(DPVTOTCAPNR(IRG,CURIYR),IRG=1,MNUMNR)
      WRITE(22,5010) 'EMMBODPV GEN ',CURIYR,CURITR,(DPVTOTGENNR(IRG,CURIYR),IRG=1,MNUMNR)
5010  FORMAT(1x,A,2I5,<MNUMNR>F12.2)
      BO_UCAPUS = 0.0
      BO_GENUS = 0.0
      BO_CONUS = 0.0
      BO_CONNRUS = 0.0

      IF (CURIYR .GE. (UESTYR-UHBSYR+2) ) THEN
          DO IRG = 1 , MNUMNR -1
              IF (DPVTOTCAPNR(IRG,CURIYR) * 0.001 .GT. UCAPPVU(IRG,CURIYR)) THEN
                BO_UCAPPV = UCAPPVU(IRG,CURIYR)
              ELSE
                BO_UCAPPV = DPVTOTCAPNR(IRG,CURIYR) * 0.001
              ENDIF
              BO_UCAPUS = BO_UCAPUS + BO_UCAPPV

              UCAPPVU(IRG,CURIYR) = UCAPPVU(IRG,CURIYR) - BO_UCAPPV
              UCAPTLU(IRG,CURIYR) = UCAPTLU(IRG,CURIYR) - BO_UCAPPV

! back out if past overwrite year
            IF (USW_OVER .GT. 0 .AND. CURIYR .GT. (UYR_OVER - UHBSYR)) THEN
              IF (DPVTOTGENNR(IRG,CURIYR) * 0.001  .GT. UGNPVNR(1,IRG,CURIYR)) THEN
                BO_DGPVGEN = UGNPVNR(1,IRG,CURIYR)
              ELSE
                BO_DGPVGEN = DPVTOTGENNR(IRG,CURIYR) * 0.001
              ENDIF
              BO_GENUS = BO_GENUS + BO_DGPVGEN
              BO_CONNR = BO_DGPVGEN * WHRFOSS(MNUMCR,CURIYR)* 0.001
              BO_CONNRUS = BO_CONNRUS + BO_CONNR

              UGNPVNR(1,IRG,CURIYR) = UGNPVNR(1,IRG,CURIYR) - BO_DGPVGEN
              UGNHONR(1,IRG,CURIYR) = UGNHONR(1,IRG,CURIYR) - BO_DGPVGEN
              UGENRN(IRG,CURIYR) = UGENRN(IRG,CURIYR) - BO_DGPVGEN
              UFLPVNR(1,IRG,CURIYR) = UFLPVNR(1,IRG,CURIYR) - BO_CONNR

            ENDIF
          ENDDO
          UCAPPVU(MNUMNR,CURIYR) = UCAPPVU(MNUMNR,CURIYR) - BO_UCAPUS
          UGNPVNR(1,MNUMNR,CURIYR) = UGNPVNR(1,MNUMNR,CURIYR) - BO_GENUS
          UGNHONR(1,MNUMNR,CURIYR) = UGNHONR(1,MNUMNR,CURIYR) - BO_GENUS
          UGENRN(MNUMNR,CURIYR) = UGENRN(MNUMNR,CURIYR) - BO_GENUS
          UFLPVNR(1,MNUMNR,CURIYR) = UFLPVNR(1,MNUMNR,CURIYR) - BO_CONNRUS

          IF (USW_OVER .GT. 0 .AND. CURIYR .GT. (UYR_OVER - UHBSYR)) THEN
! back out if past overwrite year
            DO ICENSUS = 1,MNUMCR - 1
              QPVELCYR = ((DPVCOMMGEN(ICENSUS,CURIYR) + DPVRESGEN(ICENSUS,CURIYR)))  * WHRFOSS(MNUMCR,CURIYR)*0.000001

              IF (QPVELCYR .GT. QPVEL(ICENSUS,CURIYR)) THEN
                 BO_DGPVCON(ICENSUS) = QPVEL(ICENSUS,CURIYR)
              ELSE
                 BO_DGPVCON(ICENSUS) = QPVELCYR
              ENDIF
              BO_CONUS = BO_CONUS + BO_DGPVCON(ICENSUS)
            ENDDO

            CMULT = BO_CONNRUS / BO_CONUS

            DO ICENSUS = 1,MNUMCR - 1
              QPVEL(ICENSUS,CURIYR) = QPVEL(ICENSUS,CURIYR) - (BO_DGPVCON(ICENSUS) * CMULT)
              QTREL(ICENSUS,CURIYR) = QTREL(ICENSUS,CURIYR) - (BO_DGPVCON(ICENSUS) * CMULT)

            ENDDO

            QPVEL(MNUMCR,CURIYR) = QPVEL(MNUMCR,CURIYR) - BO_CONNRUS
            QTREL(MNUMCR,CURIYR) = QTREL(MNUMCR,CURIYR) - BO_CONNRUS
          ENDIF
      END IF
!
      RETURN
      END

!
      SUBROUTINE EDSPOAK(ALASKA,ICENSUS,IYR,FACTO,NUCHRT)
!
      IMPLICIT NONE
!
!     THIS SUBROUTINE FILLS GENERATION AND CONSUMPTION ARRAYS FOR ALASKA

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'pq'
      include 'xpq'
      include 'ecpcntl'
      include 'dispinyr'
      include 'dsmdimen'
      include 'dsmsectr'
      include 'elout'
      include 'udatout'
      include 'uefdout'
      include 'cdsparms'
      include 'coalemm'
      include 'dsmtoefd'
!
      INTEGER NERC,ICENSUS,ALASKA,IYR,IRG,ISP,IOWNER
      REAL FACTO
      REAL NUCHRT,TDEM
!
!     FILL NERC REGION DEMANDS FOR ALASKA
!
      TDEM = MappCtoN(ALASKA,ICENSUS,1) +  &
             MappCtoN(ALASKA,ICENSUS,2) +  &
             MappCtoN(ALASKA,ICENSUS,3) +  &
             MappCtoN(ALASKA,ICENSUS,4)
      IF (TDEM .GT. 0.0) THEN
         QELRSN(ALASKA,CURIYR) = QELRSN(ALASKA,CURIYR) +  &
            QELRS(ICENSUS,CURIYR) / 0.003412 *  &
            MappCtoN(ALASKA,ICENSUS,1) /  &
            MappCtoN(MNUMNR,ICENSUS,1)
         QELCMN(ALASKA,CURIYR) = QELCMN(ALASKA,CURIYR) +  &
            QELCM(ICENSUS,CURIYR) / 0.003412 *  &
            MappCtoN(ALASKA,ICENSUS,2) /  &
            MappCtoN(MNUMNR,ICENSUS,2)
         QELINN(ALASKA,CURIYR) = QELINN(ALASKA,CURIYR) +  &
            QELIN(ICENSUS,CURIYR) / 0.003412 *  &
            MappCtoN(ALASKA,ICENSUS,3) /  &
            MappCtoN(MNUMNR,ICENSUS,3)
         QELTRN(ALASKA,CURIYR) = QELTRN(ALASKA,CURIYR) +  &
            QELTR(ICENSUS,CURIYR) / 0.003412 *  &
            MappCtoN(ALASKA,ICENSUS,4) /  &
            MappCtoN(MNUMNR,ICENSUS,4)
         QELASN(ALASKA,CURIYR) = QELASN(ALASKA,CURIYR) +  &
            QELRS(ICENSUS,CURIYR) / 0.003412 *  &
            MappCtoN(ALASKA,ICENSUS,1) /  &
            MappCtoN(MNUMNR,ICENSUS,1) + &
            QELCM(ICENSUS,CURIYR) / 0.003412 *  &
            MappCtoN(ALASKA,ICENSUS,2) /  &
            MappCtoN(MNUMNR,ICENSUS,2) + &
            QELIN(ICENSUS,CURIYR) / 0.003412 *  &
            MappCtoN(ALASKA,ICENSUS,3) /  &
            MappCtoN(MNUMNR,ICENSUS,3) + &
            QELTR(ICENSUS,CURIYR) / 0.003412 *  &
            MappCtoN(ALASKA,ICENSUS,4) /  &
            MappCtoN(MNUMNR,ICENSUS,4)
         ULELRS(ALASKA) = ULELRS(ALASKA) + QELRS(ICENSUS,CURIYR) *  &
            MappCtoN(ALASKA,ICENSUS,1) / MappCtoN(MNUMNR,ICENSUS,1)
         ULELCM(ALASKA) = ULELCM(ALASKA) + QELRS(ICENSUS,CURIYR) *  &
            MappCtoN(ALASKA,ICENSUS,1) / MappCtoN(MNUMNR,ICENSUS,1)
         ULELIN(ALASKA) = ULELIN(ALASKA) + QELRS(ICENSUS,CURIYR) *  &
            MappCtoN(ALASKA,ICENSUS,1) / MappCtoN(MNUMNR,ICENSUS,1)
         ULELTR(ALASKA) = ULELTR(ALASKA) + QELRS(ICENSUS,CURIYR) *  &
            MappCtoN(ALASKA,ICENSUS,1) / MappCtoN(MNUMNR,ICENSUS,1)
      END IF
!
!     ADD ALASKA FUEL CONSUMPTION TO CENSUS REGION 9
!
!     IF (ICENSUS .EQ. (MNUMCR - 2))THEN
!
         IF (QELAS(ICENSUS,IYR) .NE. 0.0) THEN
             DO IOWNER = 1, 2
!
               QCLEL(ICENSUS,CURIYR) = QCLEL(ICENSUS,CURIYR) + HFLCLNR(IOWNER,ALASKA,IYR) * FACTO

!              ALSO ASSIGN TO COAL REGION ARRAY FOR PLANT TYPE H1 AND DEMAND REGION 14

               QCLH8NR(NDREG,CURIYR) = QCLH8NR(NDREG,CURIYR) + HFLCLNR(IOWNER,ALASKA,IYR) * FACTO
!
!              ALASKAN OIL IS DISTILLATE
!
               QDSEL(ICENSUS,CURIYR) = QDSEL(ICENSUS,CURIYR) + &
                  HFLOLNR(IOWNER,ALASKA,IYR) * FACTO
               QTPEL(ICENSUS,CURIYR) = QTPEL(ICENSUS,CURIYR) + &
                  HFLOLNR(IOWNER,ALASKA,IYR) * FACTO
!
!              ALASKAN GAS SPLIT 50% FIRM, 50% INTERRUPTIBLE
!
               QGFEL(ICENSUS,CURIYR) = QGFEL(ICENSUS,CURIYR) + &
                  HFLNGNR(IOWNER,ALASKA,IYR) * 0.5 * FACTO
               QGIEL(ICENSUS,CURIYR) = QGIEL(ICENSUS,CURIYR) + &
                  HFLNGNR(IOWNER,ALASKA,IYR) * 0.5 * FACTO
               QNGEL(ICENSUS,CURIYR) = QNGEL(ICENSUS,CURIYR) + &
                  HFLNGNR(IOWNER,ALASKA,IYR) * FACTO
              ENDDO
!
!              ALSO STORE ALASKAN GAS IN GAS REGION ARRAYS
!
               QNGELGR(NNGEM,CURIYR) = (HFLNGNR(1,ALASKA,IYR) + HFLNGNR(2,ALASKA,IYR)) * FACTO
               QGFELGR(NNGEM,CURIYR) = (HFLNGNR(1,ALASKA,IYR) + &
                 HFLNGNR(2,ALASKA,IYR)) * 0.5 * FACTO
               QGIELGR(NNGEM,CURIYR) = (HFLNGNR(1,ALASKA,IYR) + &
                 HFLNGNR(2,ALASKA,IYR)) * 0.5 * FACTO
               QGCELGR(NNGEM,CURIYR) = 0.0
!              ASSUME 50/50 PEAK/OFFPEAK SPLIT
               SQGFELGR(NNGEM,CURIYR,1) = QGFELGR(17,CURIYR) * 0.5
               SQGFELGR(NNGEM,CURIYR,2) = QGFELGR(17,CURIYR) * 0.5
               SQGIELGR(NNGEM,CURIYR,1) = QGIELGR(17,CURIYR) * 0.5
               SQGIELGR(NNGEM,CURIYR,2) = QGIELGR(17,CURIYR) * 0.5
               DO ISP = 1 , EFDns
                 SQNGELGR(NNGEM,CURIYR,ISP) = HFLNGGR(17,IYR) * (1.0/EFDns) * FACTO
               ENDDO
!
!              ALSO STORE GENERATION AND FUEL CONSUMPTION IN NERC REGION ARRAYS
!              INITIALIZE ALL TO ZERO THEN OVERWRITE APPROPRIATE ONES
!
               IRG = ALASKA
                  DO IOWNER = 1 , 2
!
!                    COAL
!
                     WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UICOU,HGNCLNR(IOWNER,IRG,IYR)*FACTO,HFLCLNR(IOWNER,IRG,IYR)*FACTO
 1353                FORMAT(1X,"ALASKA_HAWAII",5(":",I4),2(":",F15.3))
                     UGNCLNR(IOWNER,IRG,CURIYR) = HGNCLNR(IOWNER,IRG,IYR) * FACTO
!                    ADD NT GENERATION
                     IF (IOWNER .EQ. 2)UGNCLNR(IOWNER,IRG,CURIYR) = UGNCLNR(IOWNER,IRG,CURIYR) + (HCNTGEN(IRG,IYR,1,1) + HCNTGEN(IRG,IYR,1,2)) * 0.001 * FACTO
                     UFLCLNR(IOWNER,IRG,CURIYR) = HFLCLNR(IOWNER,IRG,IYR) * FACTO
!
!                    OIL
!
                     WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UISTO,HGNOLNR(IOWNER,IRG,IYR)*FACTO,HFLOLNR(IOWNER,IRG,IYR)*FACTO
                     IF (HFLOLNR(IOWNER,IRG,IYR) .GT. 0.0)THEN
                        UGNDSNR(IOWNER,IRG,CURIYR) = HGNOLNR(IOWNER,IRG,IYR) * (HFLDSNR(IOWNER,IRG,IYR) / HFLOLNR(IOWNER,IRG,IYR)) * FACTO
                        UGNRLNR(IOWNER,IRG,CURIYR) = HGNOLNR(IOWNER,IRG,IYR) * (HFLRSNR(IOWNER,IRG,IYR) / HFLOLNR(IOWNER,IRG,IYR)) * FACTO
                        UGNRHNR(IOWNER,IRG,CURIYR) = 0.0
                        UFLDSNR(IOWNER,IRG,CURIYR) = HFLDSNR(IOWNER,IRG,IYR) * FACTO
                        UFLRLNR(IOWNER,IRG,CURIYR) = HFLRSNR(IOWNER,IRG,IYR) * FACTO
                        UFLRHNR(IOWNER,IRG,CURIYR) = 0.0
                     ELSE
                        UGNDSNR(IOWNER,IRG,CURIYR) = 0.0
                        UGNRLNR(IOWNER,IRG,CURIYR) = 0.0
                        UGNRHNR(IOWNER,IRG,CURIYR) = 0.0
                        UFLDSNR(IOWNER,IRG,CURIYR) = 0.0
                        UFLRLNR(IOWNER,IRG,CURIYR) = 0.0
                        UFLRHNR(IOWNER,IRG,CURIYR) = 0.0
                     END IF
!                    ADD NT GENERATION
                     IF (IOWNER .EQ. 2)UGNDSNR(IOWNER,IRG,CURIYR) = UGNDSNR(IOWNER,IRG,CURIYR) + (HCNTGEN(IRG,IYR,2,1) + HCNTGEN(IRG,IYR,2,2)) * 0.001 * FACTO
!
!
!                    GAS
!
                        WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UISTG,HGNNGNR(IOWNER,IRG,IYR)*FACTO,HFLNGNR(IOWNER,IRG,IYR)*FACTO
                        UGNGFNR(IOWNER,IRG,CURIYR) = HGNNGNR(IOWNER,IRG,IYR) * FACTO * 0.5
                        UGNGINR(IOWNER,IRG,CURIYR) = HGNNGNR(IOWNER,IRG,IYR) * FACTO * 0.5
                        UGNGCNR(IOWNER,IRG,CURIYR) = 0.0
!                       ADD NT GENERATION
                        IF (IOWNER .EQ. 2)THEN
                           UGNGFNR(IOWNER,IRG,CURIYR) = UGNGFNR(IOWNER,IRG,CURIYR) + (HCNTGEN(IRG,IYR,3,1) + HCNTGEN(IRG,IYR,3,2)) * 0.5 * 0.001 * FACTO
                           UGNGINR(IOWNER,IRG,CURIYR) = UGNGINR(IOWNER,IRG,CURIYR) + (HCNTGEN(IRG,IYR,3,1) + HCNTGEN(IRG,IYR,3,2)) * 0.5 * 0.001 * FACTO
                        END IF
                        UFLGFNR(IOWNER,IRG,CURIYR) = HFLNGNR(IOWNER,IRG,IYR) * FACTO * 0.5
                        UFLGINR(IOWNER,IRG,CURIYR) = HFLNGNR(IOWNER,IRG,IYR) * FACTO * 0.5
                        UFLGCNR(IOWNER,IRG,CURIYR) = 0.0
!
!                    NUCLEAR
!
                     WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UICNU,HGNURNR(IOWNER,IRG,IYR)*FACTO,HGNURNR(IOWNER,IRG,IYR) * FACTO * NUCHRT
                     UGNURNR(IOWNER,IRG,CURIYR) = HGNURNR(IOWNER,IRG,IYR) * FACTO
                     UFLURNR(IOWNER,IRG,CURIYR) = HGNURNR(IOWNER,IRG,IYR) * FACTO * NUCHRT * .001
                     QUREL(ICENSUS,CURIYR) = QUREL(ICENSUS,CURIYR) + (HGNURNR(IOWNER,IRG,IYR) * FACTO * NUCHRT * .001)
!
!                    Conventional Hydro
!
                     IF (HGNHYNR(IOWNER,IRG,IYR) .GT. 0.0) WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UIHYC,HGNHYNR(IOWNER,IRG,IYR)*FACTO,0.0
                     UGNHYNR(IOWNER,IRG,CURIYR) = HGNHYNR(IOWNER,IRG,IYR) * FACTO
                     UFLHYNR(IOWNER,IRG,CURIYR) = HGNHYNR(IOWNER,IRG,IYR) * FACTO * HFOSHR(IYR) * .001
                     QHOEL(ICENSUS,CURIYR) = QHOEL(ICENSUS,CURIYR) + (HGNHYNR(IOWNER,IRG,IYR) * FACTO * HFOSHR(IYR) * .001)
!
!                    Pump Storage Hydro
!
                     IF (HGNPSNR(IOWNER,IRG,IYR) .GT. 0.0) WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UIHYR,HGNPSNR(IOWNER,IRG,IYR)*FACTO,0.0
                     UGNPSNR(IOWNER,IRG,CURIYR) = HGNPSNR(IOWNER,IRG,IYR) * FACTO
!                    QHOEL(ICENSUS,CURIYR) = QHOEL(ICENSUS,CURIYR) + (HGNPSNR(IOWNER,IRG,IYR) * FACTO * HFOSHR(IYR) * .001)
!
!                    Geothermal
!
                     IF (HGNGENR(IOWNER,IRG,IYR) .GT. 0.0) WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UIGTH,HGNGENR(IOWNER,IRG,IYR)*FACTO,0.0
                     UGNGENR(IOWNER,IRG,CURIYR) = HGNGENR(IOWNER,IRG,IYR) * FACTO
                     UFLGTNR(IOWNER,IRG,CURIYR) = HGNGENR(IOWNER,IRG,IYR) * FACTO * HFOSHR(IYR) * .001
                     QGEEL(ICENSUS,CURIYR) = QGEEL(ICENSUS,CURIYR) + (HGNGENR(IOWNER,IRG,IYR) * FACTO * HFOSHR(IYR) * .001)
!
!                    Municiple Solid Waste
!
                     IF (HGNMSNR(IOWNER,IRG,IYR) .GT. 0.0) WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UIMSW,HGNMSNR(IOWNER,IRG,IYR)*FACTO,0.0
                     UGNMSNR(IOWNER,IRG,CURIYR) = HGNMSNR(IOWNER,IRG,IYR) * FACTO
                     UFLMSNR(IOWNER,IRG,CURIYR) = HGNMSNR(IOWNER,IRG,IYR) * FACTO * HMSWHR(IYR) * .001
                     QMSEL(ICENSUS,CURIYR) = QMSEL(ICENSUS,CURIYR) + (HGNMSNR(IOWNER,IRG,IYR) * FACTO * HMSWHR(IYR) * .001)
!
!                    Biomass
!
                     IF (HGNWDNR(IOWNER,IRG,IYR) .GT. 0.0) WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UIBMS,HGNWDNR(IOWNER,IRG,IYR)*FACTO,0.0
                     UGNWDNR(IOWNER,IRG,CURIYR) = HGNWDNR(IOWNER,IRG,IYR) * FACTO
                     UFLWDNR(IOWNER,IRG,CURIYR) = HGNWDNR(IOWNER,IRG,IYR) * FACTO * HWODHR(IYR) * .001
                     QBMEL(ICENSUS,CURIYR) = QBMEL(ICENSUS,CURIYR) + (HGNWDNR(IOWNER,IRG,IYR) * FACTO * HWODHR(IYR) * .001)
!
!                    Solar Thermal
!
                     IF (HGNSONR(IOWNER,IRG,IYR) .GT. 0.0) WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UISTH,HGNSONR(IOWNER,IRG,IYR)*FACTO,0.0
                     UGNSONR(IOWNER,IRG,CURIYR) =  HGNSONR(IOWNER,IRG,IYR) * FACTO
                     UFLSONR(IOWNER,IRG,CURIYR) = HGNSONR(IOWNER,IRG,IYR) * FACTO * HFOSHR(IYR) * .001
                     QSTEL(ICENSUS,CURIYR) = QSTEL(ICENSUS,CURIYR) + (HGNSONR(IOWNER,IRG,IYR) * FACTO * HFOSHR(IYR) * .001)
!
!                    Solar PV
!
                     IF (HGNPVNR(IOWNER,IRG,IYR) .GT. 0.0) WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UISPV,HGNPVNR(IOWNER,IRG,IYR)*FACTO,0.0
                     UGNPVNR(IOWNER,IRG,CURIYR) =  HGNPVNR(IOWNER,IRG,IYR) * FACTO
                     UFLPVNR(IOWNER,IRG,CURIYR) = HGNPVNR(IOWNER,IRG,IYR) * FACTO * HFOSHR(IYR) * .001
                     QPVEL(ICENSUS,CURIYR) = QPVEL(ICENSUS,CURIYR) + (HGNPVNR(IOWNER,IRG,IYR) * FACTO * HFOSHR(IYR) * .001)
!
!                    Wind
!
                     IF (HGNWNNR(IOWNER,IRG,IYR) .GT. 0.0) WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UIWND,HGNWNNR(IOWNER,IRG,IYR)*FACTO,0.0
                     UGNWNNR(IOWNER,IRG,CURIYR) = HGNWNNR(IOWNER,IRG,IYR) * FACTO
                     UFLWNNR(IOWNER,IRG,CURIYR) = HGNWNNR(IOWNER,IRG,IYR) * FACTO * HFOSHR(IYR) * .001
                     QWIEL(ICENSUS,CURIYR) = QWIEL(ICENSUS,CURIYR) + (HGNWNNR(IOWNER,IRG,IYR) * FACTO * HFOSHR(IYR) * .001)
!
!                    OffShore Wind
!
                     IF (HGNWFNR(IOWNER,IRG,IYR) .GT. 0.0) WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UIWFS,HGNWFNR(IOWNER,IRG,IYR)*FACTO,0.0
                     UGNWFNR(IOWNER,IRG,CURIYR) = HGNWFNR(IOWNER,IRG,IYR) * FACTO
                     UFLWFNR(IOWNER,IRG,CURIYR) = HGNWFNR(IOWNER,IRG,IYR) * FACTO * HFOSHR(IYR) * .001
                     QWIEL(ICENSUS,CURIYR) = QWIEL(ICENSUS,CURIYR) + (HGNWFNR(IOWNER,IRG,IYR) * FACTO * HFOSHR(IYR) * .001)
                  END DO
            ELSE
               WRITE(6,'(A,I4,A)') '** EMMDSPO ERR CENSUS= ',ICENSUS,' QELAS(ICENSUS,IYR) IS 0.0'
         END IF
!     END IF

      RETURN
      END
!
!
      SUBROUTINE EDSPOHI(HAWAII,ICENSUS,IYR,FACTO,NUCHRT)
!
      IMPLICIT NONE
!
!     THIS SUBROUTINE FILLS GENERATION AND CONSUMPTION ARRAYS FOR HAWAII

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'pq'
      include 'xpq'
      include 'ecpcntl'
      include 'dispinyr'
      include 'dsmdimen'
      include 'dsmsectr'
      include 'elout'
      include 'udatout'
      include 'uefdout'
      include 'cdsparms'
      include 'coalemm'
!
      INTEGER NERC,ICENSUS,HAWAII,IYR,IRG,IOWNER,IOWN
      REAL FACTO
      REAL NUCHRT,TDEM
      REAL HRTCL(2),HRTOL(2),HRTNG(2)
      REAL GENCL(2),GENDS(2),GENRS(2),GENOL(2),GENNG(2),GENOG(2),GENUR(2),GENPS(2),GENOT(2),GENTL(2),GENNR(2)
      REAL GENRN(2),GENGE(2),GENMS(2),GENWD(2),GENSO(2),GENPV(2),GENWN(2),GENWF(2),GENHY(2)
      REAL GENCLY(2),GENDSY(2),GENRSY(2),GENOLY(2),GENNGY(2),GENOTY(2),GENOGY(2),GENURY(2),GENPSY(2),GENTLY(2)
      REAL GENRNY(2),GENGEY(2),GENMSY(2),GENWDY(2),GENSOY(2),GENPVY(2),GENWNY(2),GENWFY(2),GENHYY(2)
      REAL CONCLY(2),CONDSY(2),CONRSY(2),CONOLY(2),CONNGY(2)
!     REAL CONRNY(2),CONGEY(2),CONMSY(2),CONWDY(2),CONSOY(2)
      REAL TGEN,RGEN,OGEN
!
!     FILL NERC REGION DEMANDS FOR HAWAII AND HAWAII
!
      TDEM = MappCtoN(HAWAII,ICENSUS,1) +  &
             MappCtoN(HAWAII,ICENSUS,2) +  &
             MappCtoN(HAWAII,ICENSUS,3) +  &
             MappCtoN(HAWAII,ICENSUS,4)
      IF (TDEM .GT. 0.0) THEN
         QELRSN(HAWAII,CURIYR) = QELRSN(HAWAII,CURIYR) +  &
            QELRS(ICENSUS,CURIYR) / 0.003412 *  &
            MappCtoN(HAWAII,ICENSUS,1) /  &
            MappCtoN(MNUMNR,ICENSUS,1)
         QELCMN(HAWAII,CURIYR) = QELCMN(HAWAII,CURIYR) +  &
            QELCM(ICENSUS,CURIYR) / 0.003412 *  &
            MappCtoN(HAWAII,ICENSUS,2) /  &
            MappCtoN(MNUMNR,ICENSUS,2)
         QELINN(HAWAII,CURIYR) = QELINN(HAWAII,CURIYR) +  &
            QELIN(ICENSUS,CURIYR) / 0.003412 *  &
            MappCtoN(HAWAII,ICENSUS,3) /  &
            MappCtoN(MNUMNR,ICENSUS,3)
         QELTRN(HAWAII,CURIYR) = QELTRN(HAWAII,CURIYR) +  &
            QELTR(ICENSUS,CURIYR) / 0.003412 *  &
            MappCtoN(HAWAII,ICENSUS,4) /  &
            MappCtoN(MNUMNR,ICENSUS,4)
         QELASN(HAWAII,CURIYR) = QELASN(HAWAII,CURIYR) +  &
            QELRS(ICENSUS,CURIYR) / 0.003412 *  &
            MappCtoN(HAWAII,ICENSUS,1) /  &
            MappCtoN(MNUMNR,ICENSUS,1) + &
            QELCM(ICENSUS,CURIYR) / 0.003412 *  &
            MappCtoN(HAWAII,ICENSUS,2) /  &
            MappCtoN(MNUMNR,ICENSUS,2) + &
            QELIN(ICENSUS,CURIYR) / 0.003412 *  &
            MappCtoN(HAWAII,ICENSUS,3) /  &
            MappCtoN(MNUMNR,ICENSUS,3) + &
            QELTR(ICENSUS,CURIYR) / 0.003412 *  &
            MappCtoN(HAWAII,ICENSUS,4) /  &
            MappCtoN(MNUMNR,ICENSUS,4)
         ULELRS(HAWAII) = ULELRS(HAWAII) + QELRS(ICENSUS,CURIYR) *  &
            MappCtoN(HAWAII,ICENSUS,1) / MappCtoN(MNUMNR,ICENSUS,1)
         ULELCM(HAWAII) = ULELCM(HAWAII) + QELRS(ICENSUS,CURIYR) *  &
            MappCtoN(HAWAII,ICENSUS,1) / MappCtoN(MNUMNR,ICENSUS,1)
         ULELIN(HAWAII) = ULELIN(HAWAII) + QELRS(ICENSUS,CURIYR) *  &
            MappCtoN(HAWAII,ICENSUS,1) / MappCtoN(MNUMNR,ICENSUS,1)
         ULELTR(HAWAII) = ULELTR(HAWAII) + QELRS(ICENSUS,CURIYR) *  &
            MappCtoN(HAWAII,ICENSUS,1) / MappCtoN(MNUMNR,ICENSUS,1)
      END IF

!
!     ADD HAWAII FUEL CONSUMPTION TO CENSUS REGION 9
!
!     IF (ICENSUS .EQ. 9)THEN
!
         IF (QELAS(ICENSUS,IYR) .NE. 0.0) THEN
             TGEN = 0.0
             RGEN = 0.0
             OGEN = 0.0
             DO IOWNER = 1, 2
!              IF USING HAWAII RPS RATES, GET GENERATION AND CONSUMPTION INFO FROM LAST HISTORICAL YEAR
               IF (HIRPSSW .GT. 0 .AND. (CURIYR + UHBSYR) .EQ. UYR_HIST)THEN
                  HRTCL(IOWNER) = 0.0
                  HRTOL(IOWNER) = 0.0
                  HRTNG(IOWNER) = 0.0
                  GENRN(IOWNER) = 0.0
                  GENCL(IOWNER) = HGNCLNR(IOWNER,HAWAII,CURIYR)
                  GENOL(IOWNER) = HGNOLNR(IOWNER,HAWAII,CURIYR)
                  GENDS(IOWNER) = HGNOLNR(IOWNER,HAWAII,CURIYR) * HFLDSNR(IOWNER,HAWAII,CURIYR) /  &
                                  HFLOLNR(IOWNER,HAWAII,CURIYR)
                  GENRS(IOWNER) = HGNOLNR(IOWNER,HAWAII,CURIYR) * HFLRSNR(IOWNER,HAWAII,CURIYR) /  &
                                  HFLOLNR(IOWNER,HAWAII,CURIYR)
                  GENNG(IOWNER) = HGNNGNR(IOWNER,HAWAII,CURIYR)
                  GENOG(IOWNER) = HGNOGNR(IOWNER,HAWAII,CURIYR)
                  GENUR(IOWNER) = HGNURNR(IOWNER,HAWAII,CURIYR)
                  GENPS(IOWNER) = HGNPSNR(IOWNER,HAWAII,CURIYR)
                  GENGE(IOWNER) = HGNGENR(IOWNER,HAWAII,CURIYR)
                  GENMS(IOWNER) = HGNMSNR(IOWNER,HAWAII,CURIYR)
                  GENWD(IOWNER) = HGNWDNR(IOWNER,HAWAII,CURIYR)
                  GENSO(IOWNER) = HGNSONR(IOWNER,HAWAII,CURIYR)
                  GENPV(IOWNER) = HGNPVNR(IOWNER,HAWAII,CURIYR)
                  GENWN(IOWNER) = HGNWNNR(IOWNER,HAWAII,CURIYR)
                  GENWF(IOWNER) = HGNWFNR(IOWNER,HAWAII,CURIYR)
                  GENHY(IOWNER) = HGNHYNR(IOWNER,HAWAII,CURIYR)
                  GENOT(IOWNER) = HGNNGNR(IOWNER,HAWAII,CURIYR)
!                 ADD NT COGEN TO OWNER TYPE 2
                  IF (IOWNER .EQ. 2)THEN
                     GENCL(IOWNER) = GENCL(IOWNER) + (HCNTGEN(HAWAII,CURIYR,1,1) + HCNTGEN(HAWAII,CURIYR,1,2)) * 0.001
                     GENOL(IOWNER) = GENOL(IOWNER) + (HCNTGEN(HAWAII,CURIYR,2,1) + HCNTGEN(HAWAII,CURIYR,2,2)) * 0.001
                     GENDS(IOWNER) = GENDS(IOWNER) + (HCNTGEN(HAWAII,CURIYR,2,1) + HCNTGEN(HAWAII,CURIYR,2,2)) * 0.001 *  &
                                     HFLDSNR(IOWNER,HAWAII,CURIYR) / HFLOLNR(IOWNER,HAWAII,CURIYR)
                     GENRS(IOWNER) = GENRS(IOWNER) + (HCNTGEN(HAWAII,CURIYR,2,1) + HCNTGEN(HAWAII,CURIYR,2,2)) * 0.001 *  &
                                     HFLRSNR(IOWNER,HAWAII,CURIYR) / HFLOLNR(IOWNER,HAWAII,CURIYR)
                     GENNG(IOWNER) = GENNG(IOWNER) + (HCNTGEN(HAWAII,CURIYR,3,1) + HCNTGEN(HAWAII,CURIYR,3,2)) * 0.001
                     GENOG(IOWNER) = GENOG(IOWNER) + (HCNTGEN(HAWAII,CURIYR,9,1) + HCNTGEN(HAWAII,CURIYR,9,2)) * 0.001
                     GENGE(IOWNER) = GENGE(IOWNER) + (HCNTGEN(HAWAII,CURIYR,5,1) + HCNTGEN(HAWAII,CURIYR,5,2)) * 0.001
                     GENMS(IOWNER) = GENMS(IOWNER) + (HCNTGEN(HAWAII,CURIYR,6,1) + HCNTGEN(HAWAII,CURIYR,6,2)) * 0.001
                     GENWD(IOWNER) = GENWD(IOWNER) + (HCNTGEN(HAWAII,CURIYR,7,1) + HCNTGEN(HAWAII,CURIYR,7,2)) * 0.001
                     GENPV(IOWNER) = GENPV(IOWNER) + (HCNTGEN(HAWAII,CURIYR,8,1) + HCNTGEN(HAWAII,CURIYR,8,2)) * 0.001
                     GENHY(IOWNER) = GENHY(IOWNER) + (HCNTGEN(HAWAII,CURIYR,4,1) + HCNTGEN(HAWAII,CURIYR,4,2)) * 0.001
                     GENOT(IOWNER) = GENOT(IOWNER) + (HCNTGEN(HAWAII,CURIYR,10,1) + HCNTGEN(HAWAII,CURIYR,10,2)) * 0.001
                  END IF
                  GENRN(IOWNER) = GENGE(IOWNER) + GENMS(IOWNER) + GENWD(IOWNER) + GENSO(IOWNER) +  &
                                  GENPV(IOWNER) + GENWN(IOWNER) + GENWF(IOWNER) + GENHY(IOWNER)
                  GENTL(IOWNER) = GENCL(IOWNER) + GENOL(IOWNER) + GENNG(IOWNER) + GENOG(IOWNER) +  &
                                  GENUR(IOWNER) + GENPS(IOWNER) + GENRN(IOWNER) + GENOT(IOWNER)
                  GENNR(IOWNER) = GENTL(IOWNER) - GENRN(IOWNER)
                  IF (GENCL(IOWNER) .GT. 0.0)HRTCL(IOWNER) = HFLCLNR(IOWNER,HAWAII,CURIYR) * 1000.0 / GENCL(IOWNER)
                  IF (GENOL(IOWNER) .GT. 0.0)HRTOL(IOWNER) = HFLOLNR(IOWNER,HAWAII,CURIYR) * 1000.0 / GENOL(IOWNER)
                  IF (GENNG(IOWNER) .GT. 0.0)HRTNG(IOWNER) = HFLNGNR(IOWNER,HAWAII,CURIYR) * 1000.0 / GENNG(IOWNER)
!     if (curitr .eq. 1)write(6,4455) curiyr+1989,iowner,hrtcl(iowner),hrtol(iowner),hrtng(iowner),  &
!                 gencl(iowner),genol(iowner),genng(iowner),genrn(iowner),gentl(iowner),genge(iowner),genms(iowner),genwd(iowner),genwn(iowner),genhy(iowner),genso(iowner),genpv(iowner)
!4455 format(1h ,'!hiflh',i4,i3,16f10.3)
               END IF
!
!              APPLY HAWAII RPS REQUIREMENT TO TOTAL GEN AND THEN SHARE OUT REMAINDER TO OTHER TYPES)
!
               IF (HIRPSSW .GT. 0 .AND. HIRPSRT(CURIYR) .GT. 0.0 .AND. (CURIYR + UHBSYR) .GT. UYR_HIST)THEN
                  TGEN = TGEN + GENTL(IOWNER) * FACTO
                  RGEN = RGEN + HIRPSRT(CURIYR) * GENTL(IOWNER) * FACTO
                  IF (IOWNER .EQ. 2)THEN
                     OGEN = TGEN - RGEN
                     DO IOWN = 1 , 2
!                    ALLOCATE REQUIRED RENEWABLES ACCORDING TO HISTORICAL SHARES
                        GENGEY(IOWN) = RGEN * GENGE(IOWN) / (GENRN(1) + GENRN(2))
                        GENMSY(IOWN) = RGEN * GENMS(IOWN) / (GENRN(1) + GENRN(2))
                        GENWDY(IOWN) = RGEN * GENWD(IOWN) / (GENRN(1) + GENRN(2))
                        GENSOY(IOWN) = RGEN * GENSO(IOWN) / (GENRN(1) + GENRN(2))
                        GENPVY(IOWN) = RGEN * GENPV(IOWN) / (GENRN(1) + GENRN(2))
                        GENWNY(IOWN) = RGEN * GENWN(IOWN) / (GENRN(1) + GENRN(2))
                        GENWFY(IOWN) = RGEN * GENWF(IOWN) / (GENRN(1) + GENRN(2))
                        GENHYY(IOWN) = RGEN * GENHY(IOWN) / (GENRN(1) + GENRN(2))
!                    ALLOCATE REMAINING NONRENEWABLES ACCORDING TO HISTORICAL SHARES
                        GENCLY(IOWN) = OGEN * GENCL(IOWN) / (GENNR(1) + GENNR(2))
                        GENOLY(IOWN) = OGEN * GENOL(IOWN) / (GENNR(1) + GENNR(2))
                        GENDSY(IOWN) = GENOLY(IOWN) * GENDS(IOWN) / (GENDS(IOWN) + GENRS(IOWN))
                        GENRSY(IOWN) = GENOLY(IOWN) * GENRS(IOWN) / (GENDS(IOWN) + GENRS(IOWN))
                        GENNGY(IOWN) = OGEN * GENNG(IOWN) / (GENNR(1) + GENNR(2))
                        GENOGY(IOWN) = OGEN * GENOG(IOWN) / (GENNR(1) + GENNR(2))
                        GENURY(IOWN) = OGEN * GENUR(IOWN) / (GENNR(1) + GENNR(2))
                        GENPSY(IOWN) = OGEN * GENPS(IOWN) / (GENNR(1) + GENNR(2))
                        GENOTY(IOWN) = OGEN * GENOT(IOWN) / (GENNR(1) + GENNR(2))
                        CONCLY(IOWN) = GENCLY(IOWN) * HRTCL(IOWN) * 0.001
                        CONDSY(IOWN) = GENDSY(IOWN) * HRTOL(IOWN) * 0.001
                        CONRSY(IOWN) = GENRSY(IOWN) * HRTOL(IOWN) * 0.001
                        CONNGY(IOWN) = GENNGY(IOWN) * HRTNG(IOWN) * 0.001
                     END DO
!                    if (curitr .eq. 1)then
!                       write(6,3050) curiyr+1989,gentl(1),gentl(2),gentl(1) + gentl(2),facto,tgen,rgen,ogen
!3050 format(1h ,'!hiflytl',i4,8f10.3)
!                       write(6,3100) curiyr+1989,gencly(1),gencly(2),concly(1),concly(2)
!3100 format(1h ,'!hiflycl',i4,8f10.3)
!                       write(6,3200) curiyr+1989,gendsy(1),gendsy(2),genrsy(1),genrsy(2),condsy(1),condsy(2),conrsy(1),conrsy(2)
!3200 format(1h ,'!hiflyol',i4,8f10.3)
!                       write(6,3300) curiyr+1989,genngy(1),genngy(2),conngy(1),conngy(2)
!3300 format(1h ,'!hiflyng',i4,8f10.3)
!                       write(6,3400) curiyr+1989,gengey(1),gengey(2),genmsy(1),genmsy(2),genwdy(1),genwdy(2),genhyy(1),genhyy(2)
!3400 format(1h ,'!hiflygmwh',i4,8f10.3)
!                       write(6,3500) curiyr+1989,genwny(1),genwny(2),genwfy(1),genwfy(2),gensoy(1),gensoy(2),genpvy(1),genpvy(2)
!3500 format(1h ,'!hiflyws',i4,8f10.3)
!                    endif
                  END IF
               ELSE
                  GENCLY(IOWNER) = HGNCLNR(IOWNER,HAWAII,IYR) * FACTO
                  CONCLY(IOWNER) = HFLCLNR(IOWNER,HAWAII,IYR) * FACTO
                  GENDSY(IOWNER) = HGNOLNR(IOWNER,HAWAII,IYR) * FACTO * HFLDSNR(IOWNER,HAWAII,IYR) /  &
                                   (HFLDSNR(IOWNER,HAWAII,IYR) + HFLRSNR(IOWNER,HAWAII,IYR))
                  CONDSY(IOWNER) = HFLDSNR(IOWNER,HAWAII,IYR) * FACTO
                  GENRSY(IOWNER) = HGNOLNR(IOWNER,HAWAII,IYR) * FACTO
                  GENRSY(IOWNER) = HGNOLNR(IOWNER,HAWAII,IYR) * FACTO * HFLRSNR(IOWNER,HAWAII,IYR) /  &
                                   (HFLDSNR(IOWNER,HAWAII,IYR) + HFLRSNR(IOWNER,HAWAII,IYR))
                  CONRSY(IOWNER) = HFLRSNR(IOWNER,HAWAII,IYR) * FACTO
                  GENNGY(IOWNER) = HGNNGNR(IOWNER,HAWAII,IYR) * FACTO
                  CONNGY(IOWNER) = HFLNGNR(IOWNER,HAWAII,IYR) * FACTO
                  GENURY(IOWNER) = HGNURNR(IOWNER,HAWAII,IYR) * FACTO
                  GENHYY(IOWNER) = HGNHYNR(IOWNER,HAWAII,IYR) * FACTO
                  GENPSY(IOWNER) = HGNPSNR(IOWNER,HAWAII,IYR) * FACTO
                  GENGEY(IOWNER) = HGNGENR(IOWNER,HAWAII,IYR) * FACTO
                  GENMSY(IOWNER) = HGNMSNR(IOWNER,HAWAII,IYR) * FACTO
                  GENWDY(IOWNER) = HGNWDNR(IOWNER,HAWAII,IYR) * FACTO
                  GENSOY(IOWNER) = HGNSONR(IOWNER,HAWAII,IYR) * FACTO
                  GENPVY(IOWNER) = HGNPVNR(IOWNER,HAWAII,IYR) * FACTO
                  GENWNY(IOWNER) = HGNWNNR(IOWNER,HAWAII,IYR) * FACTO
                  GENWFY(IOWNER) = HGNWFNR(IOWNER,HAWAII,IYR) * FACTO
               END IF
!
               QCLEL(ICENSUS,CURIYR) = QCLEL(ICENSUS,CURIYR) + CONCLY(IOWNER)

!              ALSO ASSIGN TO COAL REGION ARRAY FOR PLANT TYPE H1 AND DEMAND REGION 14

               QCLH8NR(NDREG,CURIYR) = QCLH8NR(NDREG,CURIYR) + CONCLY(IOWNER)
!
!              HAWAIIAN OIL
!
               QDSEL(ICENSUS,CURIYR) = QDSEL(ICENSUS,CURIYR) + CONDSY(IOWNER)
               QRLEL(ICENSUS,CURIYR) = QRLEL(ICENSUS,CURIYR) + CONRSY(IOWNER)
               QRSEL(ICENSUS,CURIYR) = QRSEL(ICENSUS,CURIYR) + CONRSY(IOWNER)
               QTPEL(ICENSUS,CURIYR) = QTPEL(ICENSUS,CURIYR) + CONDSY(IOWNER) + CONRSY(IOWNER)
             ENDDO
!
!              ALSO STORE GENERATION AND FUEL CONSUMPTION IN NERC REGION ARRAYS
!              INITIALIZE ALL TO ZERO THEN OVERWRITE APPROPRIATE ONES
!
               IRG = HAWAII
                  DO IOWNER = 1 , 2
!
!                    COAL
!
                     WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UICOU,HGNCLNR(IOWNER,IRG,IYR)*FACTO,HFLCLNR(IOWNER,IRG,IYR)*FACTO
 1353                FORMAT(1X,"ALASKA_HAWAII",5(":",I4),2(":",F15.3))
                     UGNCLNR(IOWNER,IRG,CURIYR) = GENCLY(IOWNER)
                     UFLCLNR(IOWNER,IRG,CURIYR) = CONCLY(IOWNER)
!
!                    OIL
!
                     WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UISTO,HGNOLNR(IOWNER,IRG,IYR)*FACTO,HFLOLNR(IOWNER,IRG,IYR)*FACTO
                        UGNDSNR(IOWNER,IRG,CURIYR) = GENDSY(IOWNER)
                        UGNRLNR(IOWNER,IRG,CURIYR) = GENRSY(IOWNER)
                        UGNRHNR(IOWNER,IRG,CURIYR) = 0.0
                        UFLDSNR(IOWNER,IRG,CURIYR) = CONDSY(IOWNER)
                        UFLRLNR(IOWNER,IRG,CURIYR) = CONRSY(IOWNER)
                        UFLRHNR(IOWNER,IRG,CURIYR) = 0.0
!
!                    GAS
!
                        UGNGFNR(IOWNER,IRG,CURIYR) = 0.0
                        UGNGINR(IOWNER,IRG,CURIYR) = GENNGY(IOWNER)
                        UGNGCNR(IOWNER,IRG,CURIYR) = 0.0
                        UFLGFNR(IOWNER,IRG,CURIYR) = 0.0
                        UFLGINR(IOWNER,IRG,CURIYR) = CONNGY(IOWNER)
                        UFLGCNR(IOWNER,IRG,CURIYR) = 0.0
!
!                    NUCLEAR
!
                     WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UICNU,HGNURNR(IOWNER,IRG,IYR)*FACTO,HGNURNR(IOWNER,IRG,IYR) * FACTO * NUCHRT
                     UGNURNR(IOWNER,IRG,CURIYR) = GENURY(IOWNER)
                     UFLURNR(IOWNER,IRG,CURIYR) = GENURY(IOWNER) * NUCHRT * .001
                     QUREL(ICENSUS,CURIYR) = QUREL(ICENSUS,CURIYR) + (GENURY(IOWNER) * NUCHRT * .001)
!
!                    Conventional Hydro
!
                     IF (HGNHYNR(IOWNER,IRG,IYR) .GT. 0.0) WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UIHYC,HGNHYNR(IOWNER,IRG,IYR)*FACTO,0.0
                     UGNHYNR(IOWNER,IRG,CURIYR) = GENHYY(IOWNER)
                     UFLHYNR(IOWNER,IRG,CURIYR) = GENHYY(IOWNER) * HFOSHR(IYR) * .001
                     QHOEL(ICENSUS,CURIYR) = QHOEL(ICENSUS,CURIYR) + (GENHYY(IOWNER) * HFOSHR(IYR) * .001)
!
!                    Pump Storage Hydro
!
                     IF (HGNPSNR(IOWNER,IRG,IYR) .GT. 0.0) WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UIHYR,HGNPSNR(IOWNER,IRG,IYR)*FACTO,0.0
                     UGNPSNR(IOWNER,IRG,CURIYR) = GENPSY(IOWNER)
!                    QHOEL(ICENSUS,CURIYR) = QHOEL(ICENSUS,CURIYR) + (GENPSY(IOWNER) * HFOSHR(IYR) * .001)
!
!                    Geothermal
!
                     IF (HGNGENR(IOWNER,IRG,IYR) .GT. 0.0) WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UIGTH,HGNGENR(IOWNER,IRG,IYR)*FACTO,0.0
                     UGNGENR(IOWNER,IRG,CURIYR) = GENGEY(IOWNER)
                     UFLGTNR(IOWNER,IRG,CURIYR) = GENGEY(IOWNER) * HFOSHR(IYR) * .001
                     QGEEL(ICENSUS,CURIYR) = QGEEL(ICENSUS,CURIYR) + (GENGEY(IOWNER) * HFOSHR(IYR) * .001)
!
!                    Municiple Solid Waste
!
                     IF (HGNMSNR(IOWNER,IRG,IYR) .GT. 0.0) WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UIMSW,HGNMSNR(IOWNER,IRG,IYR)*FACTO,0.0
                     UGNMSNR(IOWNER,IRG,CURIYR) = GENMSY(IOWNER)
                     UFLMSNR(IOWNER,IRG,CURIYR) = GENMSY(IOWNER) * HMSWHR(IYR) * .001
                     QMSEL(ICENSUS,CURIYR) = QMSEL(ICENSUS,CURIYR) + (GENMSY(IOWNER) * HMSWHR(IYR) * .001)
!
!                    Biomass
!
                     IF (HGNWDNR(IOWNER,IRG,IYR) .GT. 0.0) WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UIBMS,HGNWDNR(IOWNER,IRG,IYR)*FACTO,0.0
                     UGNWDNR(IOWNER,IRG,CURIYR) = HGNWDNR(IOWNER,IRG,IYR)
                     UFLWDNR(IOWNER,IRG,CURIYR) = GENWDY(IOWNER) * HWODHR(IYR) * .001
                     QBMEL(ICENSUS,CURIYR) = QBMEL(ICENSUS,CURIYR) + (GENWDY(IOWNER) * HWODHR(IYR) * .001)
!
!                    Solar Thermal
!
                     IF (HGNSONR(IOWNER,IRG,IYR) .GT. 0.0) WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UISTH,HGNSONR(IOWNER,IRG,IYR)*FACTO,0.0
                     UGNSONR(IOWNER,IRG,CURIYR) =  GENSOY(IOWNER)
                     UFLSONR(IOWNER,IRG,CURIYR) = GENSOY(IOWNER) * HFOSHR(IYR) * .001
                     QSTEL(ICENSUS,CURIYR) = QSTEL(ICENSUS,CURIYR) + (GENSOY(IOWNER) * HFOSHR(IYR) * .001)
!
!                    Solar PV
!
                     IF (HGNPVNR(IOWNER,IRG,IYR) .GT. 0.0) WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UISPV,HGNPVNR(IOWNER,IRG,IYR)*FACTO,0.0
                     UGNPVNR(IOWNER,IRG,CURIYR) =  GENPVY(IOWNER)
                     UFLPVNR(IOWNER,IRG,CURIYR) = GENPVY(IOWNER) * HFOSHR(IYR) * .001
                     QPVEL(ICENSUS,CURIYR) = QPVEL(ICENSUS,CURIYR) + (GENPVY(IOWNER) * HFOSHR(IYR) * .001)
!
!                    Wind
!
                     IF (HGNWNNR(IOWNER,IRG,IYR) .GT. 0.0) WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UIWND,HGNWNNR(IOWNER,IRG,IYR)*FACTO,0.0
                     UGNWNNR(IOWNER,IRG,CURIYR) = GENWNY(IOWNER)
                     UFLWNNR(IOWNER,IRG,CURIYR) = GENWNY(IOWNER) * HFOSHR(IYR) * .001
                     QWIEL(ICENSUS,CURIYR) = QWIEL(ICENSUS,CURIYR) + (GENWNY(IOWNER) * HFOSHR(IYR) * .001)
!
!                    OffShore Wind
!
                     IF (HGNWFNR(IOWNER,IRG,IYR) .GT. 0.0) WRITE(UF_DBS,1353) CURIYR+UHBSYR,CURITR,IRG,IOWNER,UIWFS,HGNWFNR(IOWNER,IRG,IYR)*FACTO,0.0
                     UGNWFNR(IOWNER,IRG,CURIYR) = GENWFY(IOWNER)
                     UFLWFNR(IOWNER,IRG,CURIYR) = GENWFY(IOWNER) * HFOSHR(IYR) * .001
                     QWIEL(ICENSUS,CURIYR) = QWIEL(ICENSUS,CURIYR) + (GENWFY(IOWNER) * HFOSHR(IYR) * .001)

!                    if (curitr .eq. 1 .and. iowner .eq. 2)then
!                       write(6,4400) curiyr+1989, HFOSHR(IYR),GENHYY(1),GENHYY(2),(GENHYY(1) + GENHYY(2)) * HFOSHR(IYR) * .001
!4400 format(1h ,'!hignhy',i4,5f10.3)
!                       write(6,4500) curiyr+1989, HFOSHR(IYR),GENGEY(1),GENGEY(2),(GENGEY(1) + GENGEY(2)) * HFOSHR(IYR) * .001
!4500 format(1h ,'!hignge',i4,5f10.3)
!                       write(6,4600) curiyr+1989, HFOSHR(IYR),GENWNY(1),GENWNY(2),(GENWNY(1) + GENWNY(2)) * HFOSHR(IYR) * .001
!4600 format(1h ,'!hignwn',i4,5f10.3)
!                    end if
                  END DO
            ELSE
               WRITE(6,'(A,I4,A)') '** EMMDSPO ERR CENSUS= ',ICENSUS,' QELAS(ICENSUS,IYR) IS 0.0'
         END IF
!     END IF

      RETURN
      END
!
!
      SUBROUTINE EMMPRCO
!
      IMPLICIT NONE
!
!     THIS SUBROUTINE STORES EMM NERC REGION FUEL PRICES IN NEMS GLOBAL ARRAYS
!     IT ALSO COMPUTES BIOMASS PRICES BY CENSUS REGIONS

      include 'parametr'
      include 'ncntrl'
      include 'mpblk'
      include 'emmparm'
      include 'control'
      include 'ecpcntl'
      include 'fuelin'
      include 'dispuse'
      include 'dispout'
      include 'bildin'
      include 'uefdout'     ! EFD output variables
!
      INTEGER IRG,IFL,OWN,TYP
      REAL PRC(5),QTY(5),TOTWD,QBM,PBM
! INITIALIZE
          TOTWD = 0.0
          UPRCLNR(MNUMNR,CURIYR) = 0.0
          UPRNGNR(MNUMNR,CURIYR) = 0.0
          UPRRSNR(MNUMNR,CURIYR) = 0.0
          UPRDSNR(MNUMNR,CURIYR) = 0.0
          UPRWDNR(MNUMNR,CURIYR) = 0.0
      DO IRG = 1 , UNRGNS
        DO TYP = 1 , 5
          PRC(TYP) = 0.0
          QTY(TYP) = 0.0
        END DO
! GET REGIONAL DISPATCH RESULTS AND INPUTS
        CALL GETOUT(CURIYR,IRG)
        CALL GETBLD(1,IRG)
! LOOP OVER EFD FUEL TYPES
        DO IFL = 1 , UNFUELS
! CLASSIFY EFD FUEL TYPES
! COAL
            IF (IFL .EQ. UIB1 .OR. IFL .EQ. UIB2 .OR. IFL .EQ. UIB3 .OR. &
              IFL .EQ. UIB4 .OR. IFL .EQ. UIB5 .OR. IFL .EQ. UIB6 .OR. &
              IFL .EQ. UIB7 .OR. IFL .EQ. UIB8 .OR. IFL .EQ. UIC1 .OR. &
              IFL .EQ. UIC2 .OR. IFL .EQ. UIC3 .OR. IFL .EQ. UIC4 .OR. &
              IFL .EQ. UIC5 .OR. IFL .EQ. UIC6 .OR. IFL .EQ. UIC7 .OR. &
              IFL .EQ. UIC8 .OR. IFL .EQ. UIC9 .OR. IFL .EQ. UICX .OR. &
              IFL .EQ. UICY .OR. IFL .EQ. UICZ .OR. IFL .EQ. UIH1 .OR. &
              IFL .EQ. UIH2 .OR. IFL .EQ. UIH3 .OR. IFL .EQ. UIH4 .OR. &
              IFL .EQ. UIH5 .OR. IFL .EQ. UIH6 .OR. IFL .EQ. UIH7 .OR. &
              IFL .EQ. UIH8 .OR. IFL .EQ. UIH9 .OR. IFL .EQ. UIHA .OR. &
              IFL .EQ. UIHB .OR. IFL .EQ. UIHC .OR. IFL .EQ. UIPC .OR. &
              IFL .EQ. UIIG .OR. IFL .EQ. UIPQ .OR. IFL .EQ. UIIS) THEN
                TYP = 1
! GAS
            ELSE IF (IFL .EQ. UIGF .OR. IFL .EQ. UIGI .OR. IFL .EQ. UIGC) THEN
                TYP = 2
! RESID
            ELSE IF (IFL .EQ. UIRL .OR. IFL .EQ. UIRH) THEN
                TYP = 3
! DISTILLATE
            ELSE IF (IFL .EQ. UIDS) THEN
                TYP = 4
! BIOMASS
            ELSE IF (IFL .EQ. UIWD) THEN
                TYP = 5
            ELSE
                TYP = 0
            END IF
            IF (TYP .GT. 0) THEN
                  PRC(TYP) = PRC(TYP) + ERFFC(IFL)
              DO OWN = 1 , USW_OWN
!               IF (EPFUEL(IFL) .LT. 99.0) THEN
                  QTY(TYP) = QTY(TYP) + EQFFL(IFL,OWN)
!               END IF
              END DO                                         ! OWN
            END IF
        END DO                                               ! IFL
        DO TYP = 1 , 5
          IF (QTY(TYP) .GT. 0.0) THEN
              PRC(TYP) = PRC(TYP) / QTY(TYP)
          ELSE
! IF NO CONSUMPTION, THEN USE PRICE FROM DESIGNATED FUEL REGION
            IF (TYP .EQ. 1) THEN
              PRC(TYP) = UPFUEL(UIB1,EPCLRG)
            ELSE IF (TYP .EQ. 2) THEN
              PRC(TYP) = UPFUEL(UIGF,EPNGRG)
            ELSE IF (TYP .EQ. 3) THEN
              PRC(TYP) = UPFUEL(UIRL,EPCENSUS(WIST))
            ELSE IF (TYP .EQ. 4) THEN
              PRC(TYP) = UPFUEL(UIDS,EPCENSUS(WIST))
            ELSE IF (TYP .EQ. 5) THEN
              PRC(TYP) = UPFUEL(UIWD,EPCLRG)
            END IF
          END IF
        END DO                                               ! TYP
              UPRCLNR(IRG,CURIYR) = PRC(1)
              UPRNGNR(IRG,CURIYR) = PRC(2)
              UPRRSNR(IRG,CURIYR) = PRC(3)
              UPRDSNR(IRG,CURIYR) = PRC(4)
              UPRWDNR(IRG,CURIYR) = PRC(5)
              UPRWDNR(MNUMNR,CURIYR) = UPRWDNR(MNUMNR,CURIYR) + PRC(5) * QTY(5)
              TOTWD = TOTWD + QTY(5)
      END DO                                                 ! IRG
! NATIONAL TOTALS
              UPRCLNR(MNUMNR,CURIYR) = PCLEL(MNUMCR,CURIYR)
              UPRNGNR(MNUMNR,CURIYR) = PNGEL(MNUMCR,CURIYR)
              UPRRSNR(MNUMNR,CURIYR) = PRSEL(MNUMCR,CURIYR)
              UPRDSNR(MNUMNR,CURIYR) = PDSEL(MNUMCR,CURIYR)
              UPRWDNR(MNUMNR,CURIYR) = UPRWDNR(MNUMNR,CURIYR) / TOTWD
! BIOMASS PRICES BY CENSUS REGION
      DO IRG = 1 , MNUMCR - 2
              PBM = 0.0
              QBM = 0.0
        DO OWN = 1 , USW_OWN
              PBM = PBM + URBIOM(IRG,OWN)
              QBM = QBM + UQBIOM(IRG,OWN)
        END DO
        IF (QBM .GT. 0.0) THEN
              UPRWDCR(IRG,CURIYR) = PBM / QBM
        ELSE
              UPRWDCR(IRG,CURIYR) = UPFUEL(UIWD,EPCLRG)
        END IF
! NATIONAL TOTALS
              UPRWDCR(MNUMCR,CURIYR) = UPRWDNR(MNUMNR,CURIYR)
      END DO
!
      RETURN
      END
!
!
      SUBROUTINE EMMCAPO
!
      IMPLICIT NONE
!
!     THIS SUBROUTINE STORES EMM PLANT CAPACITY OUTPUT IN NEMS GLOBAL ARRAYS

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'cdsparms'
      include 'control'
      include 'ecpcntl'
      include 'uefpout'     ! UEFP output variables
      include 'uefdout'     ! EFD output variables
      include 'udatout'     ! UDAT output variables
      include 'uecpout'     ! UECP output variables
      include 'uldsmout'    ! LDSM output variables
      include 'uettout'     ! UETT output variables
      include 'emmcnv'
      include 'dispout'
      include 'cogen'
      include 'coalemm'
!
!
      INTEGER IRG                                   ! REGION (16=NATION)
      INTEGER IFL              ! NON TRADITIONAL COGEN FUEL TYPE 1 TO 10
      INTEGER IPT                                  ! PLANT TYPE, 1 TO 24
!
      INTEGER IOWN                 ! OWNERSHIP - 1,2, OR 3 (PRV,PUB,EWG)
      INTEGER IVIN                   ! PLANT VINTAGE (EX,PIPE,UNPLANNED)
      INTEGER YEAR                                          ! YEAR INDEX
      INTEGER YYRR                              ! INDEX FOR CURIYR + CLT
!
      REAL*8 CAP,ADD,RCAP
      REAL   NUCADJ(MNUMNR)

      COMMON /NUCRTX/ URTNUCX
      REAL URTNUCX(MNUMYR,MNUMNR)
!
!------------------------------------------------------------------
!
!     IVIN: (VINTAGE)  1=EXISTING, 2=PIPELINE, 3=NEW BUILD
!
!     CALCULATE SUBTOTALS FOR FTAB CAPACITY REPORT CATEGORIES
!     SUBTOT(12,14,3,2)
!     PLANT 1-5    -- COAL STEAM
!     PLANT 6-8    -- OIL/GAS STEAM
!     PLANT 9-12   -- COMBUSTION TURBINE
!     PLANT 13-16  -- COMBINED CYCLE
!     PLANT 17     -- FUEL CELL
!     PLANT 18-19  -- NUCLEAR
!     PLANT 23     -- CONVENTIONAL HYDRO
!     PLANT 24     -- PUMPED STORAGE/OTHER
!     PLANT 20/22,25/27 -- RENEWABLE
!     SUBTOT 8    -- TOTAL
!------------------------------------------------------------------
!     VARIABLES USED:
!     CGNTCAP(MNUMNR,MNUMYR,10):   Non-traditional Planned + Existing Cogen Cap
!     10 cogen fuels
!     UADDaaT(MNUMNR,MNUMYR+10):   Total Capacity Additions (Calc in uecp.f)
!     UADDbbN(2,MNUMNR,MNUMYR+10): Non-Utility Capacity Additions (planned/unplanned)
!     UADDbbU(2,MNUMNR,MNUMYR+10): Utility Capacity Additions (planned/unplanned)
!     UCAPccC(MNUMNR,MNUMYR):      Non-traditional Cogen Unplanned Cap
!     UCAPddN(MNUMNR,MNUMYR):      Non-Utility Capacity
!     UCAPddU(MNUMNR,MNUMYR):      Utility Capacity
!     URETTLU(MNUMNR,MNUMYR):      Total Utility Retirements
!
!     WHERE:
!     aa = HY,GE,MS,WD,ST,PV,WN,PS,CS(total coal fired)
!     UADDCST(NDRGG,MNUMYR+10)
!     bb = CS,OS,CC,CT,NU,FC,RN(non-PS RNW),PS,TL
!     cc = CS,OS,CC,CT,HY,GE,MS,WD,ST,PV,WN,TL(total)
!     dd = CS,OS,CC,CT,NU,FC,PS,HY(conv non-PS hydro),GE,MS,WD,
!     ST,PV,WN,RN(non-PS RNW),TL(total)
!
!     10 cogen fuels: 1=coal,2=oil,3=gas,4=hydro,5=geo,
!     for CGNTCAP     6=msw,7=biomass,8=solar,9=wind,10=other
!
!------------------------------------------------------------------
!
!     INITIALIZE SUMMING ARRAYS TO 0 FOR EACH ITERATION
!
      DO IRG = 1,MNUMNR
!
!        GENERATING CAPABILITY
!
         UCAPCSU(IRG,CURIYR) = 0.0
         UCAPPQU(IRG,CURIYR) = 0.0
         UCAPSQU(IRG,CURIYR) = 0.0
         UCAPIGU(IRG,CURIYR) = 0.0
         UCAPISU(IRG,CURIYR) = 0.0
         UCAPOSU(IRG,CURIYR) = 0.0
         UCAPICU(IRG,CURIYR) = 0.0
         UCAPCTU(IRG,CURIYR) = 0.0
         UCAPT2U(IRG,CURIYR) = 0.0
         UCAPATU(IRG,CURIYR) = 0.0
         UCAPCCU(IRG,CURIYR) = 0.0
         UCAPACU(IRG,CURIYR) = 0.0
         UCAPA2U(IRG,CURIYR) = 0.0
         UCAPASU(IRG,CURIYR) = 0.0
         UCAPNUU(IRG,CURIYR) = 0.0
         UCAPSMU(IRG,CURIYR) = 0.0
         UCAPFCU(IRG,CURIYR) = 0.0
         UCAPPSU(IRG,CURIYR) = 0.0
         UCAPRNU(IRG,CURIYR) = 0.0
         UCAPDBU(IRG,CURIYR) = 0.0
         UCAPDPU(IRG,CURIYR) = 0.0
         UCAPTLU(IRG,CURIYR) = 0.0
         UCAPHYU(IRG,CURIYR) = 0.0
         UCAPGEU(IRG,CURIYR) = 0.0
         UCAPMSU(IRG,CURIYR) = 0.0
         UCAPWDU(IRG,CURIYR) = 0.0
         UCAPSTU(IRG,CURIYR) = 0.0
         UCAPPVU(IRG,CURIYR) = 0.0
         UCAPPTU(IRG,CURIYR) = 0.0
         UCAPWNU(IRG,CURIYR) = 0.0
         UCAPWLU(IRG,CURIYR) = 0.0
         UCAPWFU(IRG,CURIYR) = 0.0
         UCAPDSU(IRG,CURIYR) = 0.0
!
         UCAPCSN(IRG,CURIYR) = 0.0
         UCAPPQN(IRG,CURIYR) = 0.0
         UCAPSQN(IRG,CURIYR) = 0.0
         UCAPIGN(IRG,CURIYR) = 0.0
         UCAPISN(IRG,CURIYR) = 0.0
         UCAPOSN(IRG,CURIYR) = 0.0
         UCAPICN(IRG,CURIYR) = 0.0
         UCAPCTN(IRG,CURIYR) = 0.0
         UCAPT2N(IRG,CURIYR) = 0.0
         UCAPATN(IRG,CURIYR) = 0.0
         UCAPCCN(IRG,CURIYR) = 0.0
         UCAPACN(IRG,CURIYR) = 0.0
         UCAPA2N(IRG,CURIYR) = 0.0
         UCAPASN(IRG,CURIYR) = 0.0
         UCAPNUN(IRG,CURIYR) = 0.0
         UCAPSMN(IRG,CURIYR) = 0.0
         UCAPFCN(IRG,CURIYR) = 0.0
         UCAPPSN(IRG,CURIYR) = 0.0
         UCAPRNN(IRG,CURIYR) = 0.0
         UCAPDBN(IRG,CURIYR) = 0.0
         UCAPDPN(IRG,CURIYR) = 0.0
         UCAPTLN(IRG,CURIYR) = 0.0
         UCAPHYN(IRG,CURIYR) = 0.0
         UCAPGEN(IRG,CURIYR) = 0.0
         UCAPMSN(IRG,CURIYR) = 0.0
         UCAPWDN(IRG,CURIYR) = 0.0
         UCAPSTN(IRG,CURIYR) = 0.0
         UCAPPVN(IRG,CURIYR) = 0.0
         UCAPPTN(IRG,CURIYR) = 0.0
         UCAPWNN(IRG,CURIYR) = 0.0
         UCAPWLN(IRG,CURIYR) = 0.0
         UCAPWFN(IRG,CURIYR) = 0.0
         UCAPDSN(IRG,CURIYR) = 0.0
!
         UCAPCSC(IRG,CURIYR) = 0.0
         UCAPIGC(IRG,CURIYR) = 0.0
         UCAPOSC(IRG,CURIYR) = 0.0
         UCAPCTC(IRG,CURIYR) = 0.0
         UCAPATC(IRG,CURIYR) = 0.0
         UCAPCCC(IRG,CURIYR) = 0.0
         UCAPACC(IRG,CURIYR) = 0.0
         UCAPHYC(IRG,CURIYR) = 0.0
         UCAPGEC(IRG,CURIYR) = 0.0
         UCAPMSC(IRG,CURIYR) = 0.0
         UCAPWDC(IRG,CURIYR) = 0.0
         UCAPSTC(IRG,CURIYR) = 0.0
         UCAPPVC(IRG,CURIYR) = 0.0
         UCAPPTC(IRG,CURIYR) = 0.0
         UCAPWNC(IRG,CURIYR) = 0.0
         UCAPWLC(IRG,CURIYR) = 0.0
         UCAPWFC(IRG,CURIYR) = 0.0
         UCAPRNC(IRG,CURIYR) = 0.0
         UCAPTLC(IRG,CURIYR) = 0.0

         UCAPP2(IRG,CURIYR)  = 0.0
!
         DO IFL = 1, 10
            CGNTCAP(IRG,CURIYR,IFL) = 0.0
         END DO
!
!        RETIREMENTS
!
         URETTLU(IRG,CURIYR) = 0.0
         URETCSU(IRG,CURIYR) = 0.0
         URETA2U(IRG,CURIYR) = 0.0
         URETPQU(IRG,CURIYR) = 0.0
         URETSQU(IRG,CURIYR) = 0.0
         URETIGU(IRG,CURIYR) = 0.0
         URETISU(IRG,CURIYR) = 0.0
         URETOSU(IRG,CURIYR) = 0.0
         URETICU(IRG,CURIYR) = 0.0
         URETCTU(IRG,CURIYR) = 0.0
         URETT2U(IRG,CURIYR) = 0.0
         URETATU(IRG,CURIYR) = 0.0
         URETCCU(IRG,CURIYR) = 0.0
         URETACU(IRG,CURIYR) = 0.0
         URETASU(IRG,CURIYR) = 0.0
         URETNUU(IRG,CURIYR) = 0.0
         URETSMU(IRG,CURIYR) = 0.0
         URETFCU(IRG,CURIYR) = 0.0
         URETPSU(IRG,CURIYR) = 0.0
         URETRNU(IRG,CURIYR) = 0.0
         URETTLU(IRG,CURIYR) = 0.0
         URETHYU(IRG,CURIYR) = 0.0
         URETDSU(IRG,CURIYR) = 0.0
         URETGEU(IRG,CURIYR) = 0.0
         URETMSU(IRG,CURIYR) = 0.0
         URETWDU(IRG,CURIYR) = 0.0
         URETSTU(IRG,CURIYR) = 0.0
         URETPVU(IRG,CURIYR) = 0.0
         URETPTU(IRG,CURIYR) = 0.0
         URETWNU(IRG,CURIYR) = 0.0
         URETWLU(IRG,CURIYR) = 0.0
         URETWFU(IRG,CURIYR) = 0.0
         URETRNU(IRG,CURIYR) = 0.0
         URETAGU(IRG,CURIYR) = 0.0
         URETBIU(IRG,CURIYR) = 0.0
         URETGNU(IRG,CURIYR) = 0.0
         URETHIU(IRG,CURIYR) = 0.0
         URETHOU(IRG,CURIYR) = 0.0
         URETI2U(IRG,CURIYR) = 0.0
         URETINU(IRG,CURIYR) = 0.0
         URETOCU(IRG,CURIYR) = 0.0
         URETS2U(IRG,CURIYR) = 0.0
         URETSSU(IRG,CURIYR) = 0.0
         URETTIU(IRG,CURIYR) = 0.0
         URETZSU(IRG,CURIYR) = 0.0

         NUCADJ(IRG) = 0.0
!
!        CAPACITY ADDITIONS
!
         DO IVIN = 1,2
            UADDCSU(IVIN,IRG,CURIYR) = 0.0
            UADDPQU(IVIN,IRG,CURIYR) = 0.0
            UADDIGU(IVIN,IRG,CURIYR) = 0.0
            UADDISU(IVIN,IRG,CURIYR) = 0.0
            UADDOSU(IVIN,IRG,CURIYR) = 0.0
            UADDICU(IVIN,IRG,CURIYR) = 0.0
            UADDCTU(IVIN,IRG,CURIYR) = 0.0
            UADDT2U(IVIN,IRG,CURIYR) = 0.0
            UADDATU(IVIN,IRG,CURIYR) = 0.0
            UADDCCU(IVIN,IRG,CURIYR) = 0.0
            UADDACU(IVIN,IRG,CURIYR) = 0.0
            UADDASU(IVIN,IRG,CURIYR) = 0.0
            UADDNUU(IVIN,IRG,CURIYR) = 0.0
            UADDSMU(IVIN,IRG,CURIYR) = 0.0
            UADDFCU(IVIN,IRG,CURIYR) = 0.0
            UADDPSU(IVIN,IRG,CURIYR) = 0.0
            UADDRNU(IVIN,IRG,CURIYR) = 0.0
            UADDHYU(IVIN,IRG,CURIYR) = 0.0
            UADDGEU(IVIN,IRG,CURIYR) = 0.0
            UADDMSU(IVIN,IRG,CURIYR) = 0.0
            UADDWDU(IVIN,IRG,CURIYR) = 0.0
            UADDSTU(IVIN,IRG,CURIYR) = 0.0
            UADDPVU(IVIN,IRG,CURIYR) = 0.0
            UADDPTU(IVIN,IRG,CURIYR) = 0.0
            UADDWNU(IVIN,IRG,CURIYR) = 0.0
            UADDWLU(IVIN,IRG,CURIYR) = 0.0
            UADDWFU(IVIN,IRG,CURIYR) = 0.0
            UADDDSU(IVIN,IRG,CURIYR) = 0.0
            UADDDBU(IVIN,IRG,CURIYR) = 0.0
            UADDDPU(IVIN,IRG,CURIYR) = 0.0
            UADDTLU(IVIN,IRG,CURIYR) = 0.0
!
            UADDCSN(IVIN,IRG,CURIYR) = 0.0
            UADDPQN(IVIN,IRG,CURIYR) = 0.0
            UADDIGN(IVIN,IRG,CURIYR) = 0.0
            UADDISN(IVIN,IRG,CURIYR) = 0.0
            UADDOSN(IVIN,IRG,CURIYR) = 0.0
            UADDICN(IVIN,IRG,CURIYR) = 0.0
            UADDCTN(IVIN,IRG,CURIYR) = 0.0
            UADDATN(IVIN,IRG,CURIYR) = 0.0
            UADDT2N(IVIN,IRG,CURIYR) = 0.0
            UADDCCN(IVIN,IRG,CURIYR) = 0.0
            UADDACN(IVIN,IRG,CURIYR) = 0.0
            UADDASN(IVIN,IRG,CURIYR) = 0.0
            UADDNUN(IVIN,IRG,CURIYR) = 0.0
            UADDSMN(IVIN,IRG,CURIYR) = 0.0
            UADDFCN(IVIN,IRG,CURIYR) = 0.0
            UADDPSN(IVIN,IRG,CURIYR) = 0.0
            UADDRNN(IVIN,IRG,CURIYR) = 0.0
            UADDHYN(IVIN,IRG,CURIYR) = 0.0
            UADDGEN(IVIN,IRG,CURIYR) = 0.0
            UADDMSN(IVIN,IRG,CURIYR) = 0.0
            UADDWDN(IVIN,IRG,CURIYR) = 0.0
            UADDSTN(IVIN,IRG,CURIYR) = 0.0
            UADDPVN(IVIN,IRG,CURIYR) = 0.0
            UADDPTN(IVIN,IRG,CURIYR) = 0.0
            UADDWNN(IVIN,IRG,CURIYR) = 0.0
            UADDWLN(IVIN,IRG,CURIYR) = 0.0
            UADDWFN(IVIN,IRG,CURIYR) = 0.0
            UADDDSN(IVIN,IRG,CURIYR) = 0.0
            UADDDBN(IVIN,IRG,CURIYR) = 0.0
            UADDDPN(IVIN,IRG,CURIYR) = 0.0
            UADDTLN(IVIN,IRG,CURIYR) = 0.0

            UADDP2(IVIN,IRG,CURIYR) = 0.0
         END DO
!        NONTRADITIONAL COGEN
            UADDCSC(IRG,CURIYR) = 0.0
            UADDIGC(IRG,CURIYR) = 0.0
            UADDOSC(IRG,CURIYR) = 0.0
            UADDCTC(IRG,CURIYR) = 0.0
            UADDATC(IRG,CURIYR) = 0.0
            UADDCCC(IRG,CURIYR) = 0.0
            UADDACC(IRG,CURIYR) = 0.0
            UADDRNC(IRG,CURIYR) = 0.0
            UADDHYC(IRG,CURIYR) = 0.0
            UADDGEC(IRG,CURIYR) = 0.0
            UADDMSC(IRG,CURIYR) = 0.0
            UADDWDC(IRG,CURIYR) = 0.0
            UADDSTC(IRG,CURIYR) = 0.0
            UADDPVC(IRG,CURIYR) = 0.0
            UADDPTC(IRG,CURIYR) = 0.0
            UADDWNC(IRG,CURIYR) = 0.0
            UADDWLC(IRG,CURIYR) = 0.0
            UADDWFC(IRG,CURIYR) = 0.0
            UADDTLC(IRG,CURIYR) = 0.0
      END DO                                                  ! IRG LOOP
!
      DO IRG = 1,UNRGNS                              ! BEGIN REGION LOOP
!
!        GETOUT READS DATA FOR CURIYR, REGION IRG, FROM THE OUTDAF FILE.
!
         CALL GETOUT(CURIYR,IRG)
!
         DO IOWN = 1,USW_OWN                   ! OWNERSHIP LOOP
            IF (IOWN .LE. 2)THEN
               UCAPSQU(IRG,CURIYR) = UCAPSQU(IRG,CURIYR) + ECSCAPSQ(IOWN) * 0.001
               UCAPSQU(MNUMNR,CURIYR) = UCAPSQU(MNUMNR,CURIYR) + ECSCAPSQ(IOWN) * 0.001
            ELSEIF(IOWN .EQ. 3) THEN
               UCAPSQN(IRG,CURIYR) = UCAPSQN(IRG,CURIYR) + ECSCAPSQ(IOWN) * 0.001
               UCAPSQN(MNUMNR,CURIYR) = UCAPSQN(MNUMNR,CURIYR) + ECSCAPSQ(IOWN) * 0.001
            END IF
         END DO
         DO IVIN = 1,EFD_D_VIN                      ! BEGIN VINTAGE LOOP
!
            DO IOWN = 1,USW_OWN                   ! BEGIN OWNERSHIP LOOP
!
               DO IPT = 1, EFD_D_CAP                ! BEGIN PLANT LOOP
!
!                 DISPATCHABLE PLANTS
!
                  IF(IPT .LE. EFD_D_DSP) THEN
!
!                    DISPATCHABLE PLANTS TYPES 1-19
!                    ECSCAP IS THE CAPACITY ARRAY FROM THE COMMON
!                    BLOCK (DISPOUT).  PLANTS ARE ALSO GROUPED
!                    ACCORDING TO TYPE.
!
!                    -- ACCUMULATE TOTAL RETIREMENTS FOR EXISTING CAPACITY
!
                     IF(IVIN .EQ. 1) THEN
                        RCAP = ECSRET(IPT,IOWN) * .001
!                       IF(IOWN .LE. 3 .AND. RCAP .GT. 0.0) THEN
                        IF(IOWN .LE. 4 .AND. RCAP .GT. 0.0) THEN
!                          WRITE(22,2525) IRG,IPT,IOWN,RCAP
!2525  FORMAT(1H ,'UTCRET=',3I5,F10.3)
                           URETTLU(IRG,CURIYR) = URETTLU(IRG,CURIYR) + RCAP
                           URETTLU(MNUMNR,CURIYR) = URETTLU(MNUMNR,CURIYR) + RCAP
                        END IF
                     END IF
                     CAP = ECSCAP(IPT,IVIN,IOWN) * .001
                     ADD = ECSADD(IPT,IVIN,IOWN) * .001
!
!                    -- ACCUMULATE TOTAL CAPACITY AND ADDITIONS
!
                     IF((CAP .GT. 0.0) .OR.  &
                        (CAP .LE. 0.0 .AND. RCAP .GT. 0.0) .OR.  &
                        (CAP .LE. 0.0 .AND.  ADD .GT. 0.0)) THEN
                        IF(IOWN .LE. 2) THEN
                           UCAPTLU(IRG,CURIYR) = UCAPTLU(IRG,CURIYR) + CAP
                           UCAPTLU(MNUMNR,CURIYR) = UCAPTLU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDTLU(1,IRG,CURIYR) = UADDTLU(1,IRG,CURIYR) + ADD
                              UADDTLU(1,MNUMNR,CURIYR) = UADDTLU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDTLU(2,IRG,CURIYR) = UADDTLU(2,IRG,CURIYR) + ADD
                              UADDTLU(2,MNUMNR,CURIYR) = UADDTLU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPTLN(IRG,CURIYR) = UCAPTLN(IRG,CURIYR) + CAP
                           UCAPTLN(MNUMNR,CURIYR) = UCAPTLN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDTLN(1,IRG,CURIYR) = UADDTLN(1,IRG,CURIYR) + ADD
                              UADDTLN(1,MNUMNR,CURIYR) = UADDTLN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDTLN(2,IRG,CURIYR) = UADDTLN(2,IRG,CURIYR) + ADD
                              UADDTLN(2,MNUMNR,CURIYR) = UADDTLN(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 4) THEN
!                        IF(IVIN .EQ. 3) THEN
                           UCAPTLC(IRG,CURIYR) = UCAPTLC(IRG,CURIYR) + CAP
                           UCAPTLC(MNUMNR,CURIYR) = UCAPTLC(MNUMNR,CURIYR) + CAP
!                        END IF
                         IF(IVIN .EQ. 2) THEN
                           UADDTLC(IRG,CURIYR) = UADDTLC(IRG,CURIYR) + ADD
                           UADDTLC(MNUMNR,CURIYR) = UADDTLC(MNUMNR,CURIYR) + ADD
                         END IF
                        END IF                                    ! IOWN
!
!                       LOAD ARRAYS WITH CAPACITY AND CAPACITY ADDITIONS
!
!                       WRITE(22,*)' ECSCAP(PT,VN,OW)=',ECSCAP(IPT,IVN,IOWN)
!
!                       -- CALCULATE COAL STEAM CAPACITY (PLANTS 1-5)
!
                        IF (EPPLCD(IPT) .EQ. 'COU' .OR. &
                            EPPLCD(IPT) .EQ. 'CSU' .OR. &
                            EPPLCD(IPT) .EQ. 'CSC' .OR. &
                            EPPLCD(IPT) .EQ. 'CNC' .OR. &
                            EPPLCD(IPT) .EQ. 'CAV' .OR. &
                            EPPLCD(IPT) .EQ. 'COQ' .OR. &
                            EPPLCD(IPT) .EQ. 'CAS') THEN
!                         IF (IVIN .EQ. 1 .AND. IOWN .LE. 3 .AND.  &
                          IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND.  &
                              RCAP .GT. 0.0) THEN
                            URETCSU(IRG,CURIYR) = URETCSU(IRG,CURIYR) + RCAP
                            URETCSU(MNUMNR,CURIYR) = URETCSU(MNUMNR,CURIYR) + RCAP
                          END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPCSU(IRG,CURIYR) = UCAPCSU(IRG,CURIYR) + CAP
                           UCAPCSU(MNUMNR,CURIYR) = UCAPCSU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDCSU(1,IRG,CURIYR) = UADDCSU(1,IRG,CURIYR) + ADD
                              UADDCSU(1,MNUMNR,CURIYR) = UADDCSU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDCSU(2,IRG,CURIYR) = UADDCSU(2,IRG,CURIYR) + ADD
                              UADDCSU(2,MNUMNR,CURIYR) = UADDCSU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPCSN(IRG,CURIYR) = UCAPCSN(IRG,CURIYR) + CAP
                           UCAPCSN(MNUMNR,CURIYR) = UCAPCSN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDCSN(1,IRG,CURIYR) = UADDCSN(1,IRG,CURIYR) + ADD
                              UADDCSN(1,MNUMNR,CURIYR) = UADDCSN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDCSN(2,IRG,CURIYR) = UADDCSN(2,IRG,CURIYR) + ADD
                              UADDCSN(2,MNUMNR,CURIYR) = UADDCSN(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 4) THEN
!                          IF(IVIN .LE. 2) THEN
                              CGNTCAP(IRG,CURIYR,1) = CGNTCAP(IRG,CURIYR,1) + (CAP * 1000)
                              CGNTCAP(MNUMNR,CURIYR,1) = CGNTCAP(MNUMNR,CURIYR,1) + (CAP * 1000)
!                          ELSEIF(IVIN .EQ. 3) THEN
                              UCAPCSC(IRG,CURIYR) = UCAPCSC(IRG,CURIYR) + CAP
                              UCAPCSC(MNUMNR,CURIYR) = UCAPCSC(MNUMNR,CURIYR) + CAP
!                          END IF
                           IF(IVIN .EQ. 2) THEN
                              UADDCSC(IRG,CURIYR) = UADDCSC(IRG,CURIYR) + ADD
                              UADDCSC(MNUMNR,CURIYR) = UADDCSC(MNUMNR,CURIYR) + ADD
                           END IF
                        END IF                                    ! IOWN

!                      ---  CALCULATE ADVANCED COAL CAPACITY W/Partial SEQUESTRATION
!
                        IF  (EPPLCD(IPT) .EQ. 'COQ') THEN        ! second subgroup of coal EPPLCD
!                         IF (IVIN .EQ. 1 .AND. IOWN .LE. 3 .AND.  &
                          IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND.  &
                              RCAP .GT. 0.0) THEN
                            URETPQU(IRG,CURIYR) = URETPQU(IRG,CURIYR) + RCAP
                            URETPQU(MNUMNR,CURIYR) = URETPQU(MNUMNR,CURIYR) + RCAP
                          END IF
                          IF(IOWN .LE. 2) THEN
                           UCAPPQU(IRG,CURIYR) = UCAPPQU(IRG,CURIYR) + CAP
                           UCAPPQU(MNUMNR,CURIYR) = UCAPPQU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDPQU(1,IRG,CURIYR) = UADDPQU(1,IRG,CURIYR) + ADD
                              UADDPQU(1,MNUMNR,CURIYR) = UADDPQU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDPQU(2,IRG,CURIYR) = UADDPQU(2,IRG,CURIYR) + ADD
                              UADDPQU(2,MNUMNR,CURIYR) = UADDPQU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPPQN(IRG,CURIYR) = UCAPPQN(IRG,CURIYR) + CAP
                           UCAPPQN(MNUMNR,CURIYR) = UCAPPQN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDPQN(1,IRG,CURIYR) = UADDPQN(1,IRG,CURIYR) + ADD
                              UADDPQN(1,MNUMNR,CURIYR) = UADDPQN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDPQN(2,IRG,CURIYR) = UADDPQN(2,IRG,CURIYR) + ADD
                              UADDPQN(2,MNUMNR,CURIYR) = UADDPQN(2,MNUMNR,CURIYR) + ADD
                           END IF
!                       ELSEIF(IOWN .EQ. 4) THEN
!                          IF(IVIN .EQ. 3) THEN
!                             UCAPISC(IRG,CURIYR) = UCAPISC(IRG,CURIYR) + CAP
!                             UCAPISC(MNUMNR,CURIYR) = UCAPISC(MNUMNR,CURIYR) + CAP
!                          END IF
                        END IF                                    ! IOWN
                      END IF                                      ! COQ
!

!
!                      ---  CALCULATE ADVANCED COAL CAPACITY
!
                        IF  (EPPLCD(IPT) .EQ. 'CAV') THEN        ! first subgroup of coal EPPLCD
!                         IF (IVIN .EQ. 1 .AND. IOWN .LE. 3 .AND.  &
                          IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND.  &
                              RCAP .GT. 0.0) THEN
                            URETIGU(IRG,CURIYR) = URETIGU(IRG,CURIYR) + RCAP
                            URETIGU(MNUMNR,CURIYR) = URETIGU(MNUMNR,CURIYR) + RCAP
                          END IF
                          IF(IOWN .LE. 2) THEN
                           UCAPIGU(IRG,CURIYR) = UCAPIGU(IRG,CURIYR) + CAP
                           UCAPIGU(MNUMNR,CURIYR) = UCAPIGU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDIGU(1,IRG,CURIYR) = UADDIGU(1,IRG,CURIYR) + ADD
                              UADDIGU(1,MNUMNR,CURIYR) = UADDIGU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDIGU(2,IRG,CURIYR) = UADDIGU(2,IRG,CURIYR) + ADD
                              UADDIGU(2,MNUMNR,CURIYR) = UADDIGU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPIGN(IRG,CURIYR) = UCAPIGN(IRG,CURIYR) + CAP
                           UCAPIGN(MNUMNR,CURIYR) = UCAPIGN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDIGN(1,IRG,CURIYR) = UADDIGN(1,IRG,CURIYR) + ADD
                              UADDIGN(1,MNUMNR,CURIYR) = UADDIGN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDIGN(2,IRG,CURIYR) = UADDIGN(2,IRG,CURIYR) + ADD
                              UADDIGN(2,MNUMNR,CURIYR) = UADDIGN(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 4) THEN
!                          IF(IVIN .EQ. 3) THEN
                              UCAPIGC(IRG,CURIYR) = UCAPIGC(IRG,CURIYR) + CAP
                              UCAPIGC(MNUMNR,CURIYR) = UCAPIGC(MNUMNR,CURIYR) + CAP
!                          END IF
                           IF(IVIN .EQ. 2) THEN
                              UADDIGC(IRG,CURIYR) = UADDIGC(IRG,CURIYR) + ADD
                              UADDIGC(MNUMNR,CURIYR) = UADDIGC(MNUMNR,CURIYR) + ADD
                           END IF
                        END IF                                    ! IOWN
                      END IF                                      ! CAV
!
!                      ---  CALCULATE ADVANCED COAL CAPACITY W/SEQUESTRATION
!
                        IF  (EPPLCD(IPT) .EQ. 'CAS') THEN        ! second subgroup of coal EPPLCD
!                         IF (IVIN .EQ. 1 .AND. IOWN .LE. 3 .AND.  &
                          IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND.  &
                              RCAP .GT. 0.0) THEN
                            URETISU(IRG,CURIYR) = URETISU(IRG,CURIYR) + RCAP
                            URETISU(MNUMNR,CURIYR) = URETISU(MNUMNR,CURIYR) + RCAP
                          END IF
                          IF(IOWN .LE. 2) THEN
                           UCAPISU(IRG,CURIYR) = UCAPISU(IRG,CURIYR) + CAP
                           UCAPISU(MNUMNR,CURIYR) = UCAPISU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDISU(1,IRG,CURIYR) = UADDISU(1,IRG,CURIYR) + ADD
                              UADDISU(1,MNUMNR,CURIYR) = UADDISU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDISU(2,IRG,CURIYR) = UADDISU(2,IRG,CURIYR) + ADD
                              UADDISU(2,MNUMNR,CURIYR) = UADDISU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPISN(IRG,CURIYR) = UCAPISN(IRG,CURIYR) + CAP
                           UCAPISN(MNUMNR,CURIYR) = UCAPISN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDISN(1,IRG,CURIYR) = UADDISN(1,IRG,CURIYR) + ADD
                              UADDISN(1,MNUMNR,CURIYR) = UADDISN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDISN(2,IRG,CURIYR) = UADDISN(2,IRG,CURIYR) + ADD
                              UADDISN(2,MNUMNR,CURIYR) = UADDISN(2,MNUMNR,CURIYR) + ADD
                           END IF
!                       ELSEIF(IOWN .EQ. 4) THEN
!                          IF(IVIN .EQ. 3) THEN
!                             UCAPISC(IRG,CURIYR) = UCAPISC(IRG,CURIYR) + CAP
!                             UCAPISC(MNUMNR,CURIYR) = UCAPISC(MNUMNR,CURIYR) + CAP
!                          END IF
                        END IF                                    ! IOWN
                      END IF                                      ! CAS
!
!                       -- CALCULATE OIL/GAS STEAM CAPACITY (PLANTS 6-8)
!                       NON-TRAD COGEN ASSIGNED TO OIL
!
                     ELSEIF (EPPLCD(IPT) .EQ. 'STO' .OR. &           ! else EPPLCD main group
                             EPPLCD(IPT) .EQ. 'STG' .OR. &
                             EPPLCD(IPT) .EQ. 'STX') THEN

!                       IF (IVIN .EQ. 1 .AND. IOWN .LE. 3 .AND.  &

                        IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND. RCAP .GT. 0.0) THEN
                           URETOSU(IRG,CURIYR) = URETOSU(IRG,CURIYR) + RCAP
                           URETOSU(MNUMNR,CURIYR) = URETOSU(MNUMNR,CURIYR) + RCAP
                        END IF

                        IF(IOWN .LE. 2) THEN
                           UCAPOSU(IRG,CURIYR) = UCAPOSU(IRG,CURIYR) + CAP
                           UCAPOSU(MNUMNR,CURIYR) = UCAPOSU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDOSU(1,IRG,CURIYR) = UADDOSU(1,IRG,CURIYR) + ADD
                              UADDOSU(1,MNUMNR,CURIYR) = UADDOSU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDOSU(2,IRG,CURIYR) = UADDOSU(2,IRG,CURIYR) + ADD
                              UADDOSU(2,MNUMNR,CURIYR) = UADDOSU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPOSN(IRG,CURIYR) = UCAPOSN(IRG,CURIYR) + CAP
                           UCAPOSN(MNUMNR,CURIYR) = UCAPOSN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDOSN(1,IRG,CURIYR) = UADDOSN(1,IRG,CURIYR) + ADD
                              UADDOSN(1,MNUMNR,CURIYR) = UADDOSN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDOSN(2,IRG,CURIYR) = UADDOSN(2,IRG,CURIYR) + ADD
                              UADDOSN(2,MNUMNR,CURIYR) = UADDOSN(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 4) THEN

!                          IF(IVIN .LE. 2) THEN

                           CGNTCAP(IRG,CURIYR,2) = CGNTCAP(IRG,CURIYR,2) + (CAP * 1000)
                           CGNTCAP(MNUMNR,CURIYR,2) = CGNTCAP(MNUMNR,CURIYR,2) + &
						   (CAP * 1000)

!                          ELSEIF(IVIN .EQ. 3) THEN

                           UCAPOSC(IRG,CURIYR) = UCAPOSC(IRG,CURIYR) + CAP
                           UCAPOSC(MNUMNR,CURIYR) = UCAPOSC(MNUMNR,CURIYR) + CAP

!                          END IF

                           IF(IVIN .EQ. 2) THEN
                              UADDOSC(IRG,CURIYR) = UADDOSC(IRG,CURIYR) + ADD
                              UADDOSC(MNUMNR,CURIYR) = UADDOSC(MNUMNR,CURIYR) + ADD
                           END IF
                        END IF                                    ! IOWN
!
!                       -- CALCULATE COMBUSTION TURBINE CAPACITY (PLANTS 14-19)
!
                     ELSEIF (EPPLCD(IPT) .EQ. 'CTO' .OR. &           ! else EPPLCD main group
                             EPPLCD(IPT) .EQ. 'CTG' .OR. &
                             EPPLCD(IPT) .EQ. 'CTX' .OR. &
                             EPPLCD(IPT) .EQ. 'ICE' .OR. &
                             EPPLCD(IPT) .EQ. 'CTA' .OR. &
                             EPPLCD(IPT) .EQ. 'ACT') THEN
!                         IF (IVIN .EQ. 1 .AND. IOWN .LE. 3 .AND.  &
                          IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND.  &
                              RCAP .GT. 0.0) THEN
                            URETCTU(IRG,CURIYR) = URETCTU(IRG,CURIYR) + RCAP
                            URETCTU(MNUMNR,CURIYR) = URETCTU(MNUMNR,CURIYR) + RCAP
                          END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPCTU(IRG,CURIYR) = UCAPCTU(IRG,CURIYR) + CAP
                           UCAPCTU(MNUMNR,CURIYR) = UCAPCTU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDCTU(1,IRG,CURIYR) = UADDCTU(1,IRG,CURIYR) + ADD
                              UADDCTU(1,MNUMNR,CURIYR) = UADDCTU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDCTU(2,IRG,CURIYR) = UADDCTU(2,IRG,CURIYR) + ADD
                              UADDCTU(2,MNUMNR,CURIYR) = UADDCTU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPCTN(IRG,CURIYR) = UCAPCTN(IRG,CURIYR) + CAP
                           UCAPCTN(MNUMNR,CURIYR) = UCAPCTN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDCTN(1,IRG,CURIYR) = UADDCTN(1,IRG,CURIYR) + ADD
                              UADDCTN(1,MNUMNR,CURIYR) = UADDCTN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDCTN(2,IRG,CURIYR) = UADDCTN(2,IRG,CURIYR) + ADD
                              UADDCTN(2,MNUMNR,CURIYR) = UADDCTN(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 4) THEN
!                          IF(IVIN .LE. 2) THEN
                              CGNTCAP(IRG,CURIYR,3) = CGNTCAP(IRG,CURIYR,3) + (CAP * 1000)
                              CGNTCAP(MNUMNR,CURIYR,3) = CGNTCAP(MNUMNR,CURIYR,3) + (CAP * 1000)
!                          ELSEIF(IVIN .EQ. 3) THEN
                              UCAPCTC(IRG,CURIYR) = UCAPCTC(IRG,CURIYR) + CAP
                              UCAPCTC(MNUMNR,CURIYR) = UCAPCTC(MNUMNR,CURIYR) + CAP
!                          END IF
                           IF(IVIN .EQ. 2) THEN
                              UADDCTC(IRG,CURIYR) = UADDCTC(IRG,CURIYR) + ADD
                              UADDCTC(MNUMNR,CURIYR) = UADDCTC(MNUMNR,CURIYR) + ADD
                           END IF
                        END IF                                    ! IOWN
!
!                       -- CALCULATE ADVANCED COMBUSTION TURBINE CAPACITY
!
                        IF (EPPLCD(IPT) .EQ. 'ACT') THEN           ! combustion turbine EPPLCD subgroup
!                         IF (IVIN .EQ. 1 .AND. IOWN .LE. 3 .AND.  &
                          IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND.  &
                              RCAP .GT. 0.0) THEN
                            URETATU(IRG,CURIYR) = URETATU(IRG,CURIYR) + RCAP
                            URETATU(MNUMNR,CURIYR) = URETATU(MNUMNR,CURIYR) + RCAP
                          END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPATU(IRG,CURIYR) = UCAPATU(IRG,CURIYR) + CAP
                           UCAPATU(MNUMNR,CURIYR) = UCAPATU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDATU(1,IRG,CURIYR) = UADDATU(1,IRG,CURIYR) + ADD
                              UADDATU(1,MNUMNR,CURIYR) = UADDATU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDATU(2,IRG,CURIYR) = UADDATU(2,IRG,CURIYR) + ADD
                              UADDATU(2,MNUMNR,CURIYR) = UADDATU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPATN(IRG,CURIYR) = UCAPATN(IRG,CURIYR) + CAP
                           UCAPATN(MNUMNR,CURIYR) = UCAPATN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDATN(1,IRG,CURIYR) = UADDATN(1,IRG,CURIYR) + ADD
                              UADDATN(1,MNUMNR,CURIYR) = UADDATN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDATN(2,IRG,CURIYR) = UADDATN(2,IRG,CURIYR) + ADD
                              UADDATN(2,MNUMNR,CURIYR) = UADDATN(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 4) THEN
!                          IF(IVIN .EQ. 3) THEN
                              UCAPATC(IRG,CURIYR) = UCAPATC(IRG,CURIYR) + CAP
                              UCAPATC(MNUMNR,CURIYR) = UCAPATC(MNUMNR,CURIYR) + CAP
!                          END IF
                           IF(IVIN .EQ. 2) THEN
                              UADDATC(IRG,CURIYR) = UADDATC(IRG,CURIYR) + ADD
                              UADDATC(MNUMNR,CURIYR) = UADDATC(MNUMNR,CURIYR) + ADD
                           END IF
                        END IF                                    ! IOWN
                        END IF                                    ! ACT
!
!                       -- CALCULATE INTERNAL COMBUSTION ENGINE  CAPACITY
!
                        IF (EPPLCD(IPT) .EQ. 'ICE') THEN           ! combustion turbine EPPLCD subgroup
!                         IF (IVIN .EQ. 1 .AND. IOWN .LE. 3 .AND.  &
                          IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND.  &
                              RCAP .GT. 0.0) THEN
                            URETICU(IRG,CURIYR) = URETICU(IRG,CURIYR) + RCAP
                            URETICU(MNUMNR,CURIYR) = URETICU(MNUMNR,CURIYR) + RCAP
                          END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPICU(IRG,CURIYR) = UCAPICU(IRG,CURIYR) + CAP
                           UCAPICU(MNUMNR,CURIYR) = UCAPICU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDICU(1,IRG,CURIYR) = UADDICU(1,IRG,CURIYR) + ADD
                              UADDICU(1,MNUMNR,CURIYR) = UADDICU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDICU(2,IRG,CURIYR) = UADDICU(2,IRG,CURIYR) + ADD
                              UADDICU(2,MNUMNR,CURIYR) = UADDICU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPICN(IRG,CURIYR) = UCAPICN(IRG,CURIYR) + CAP
                           UCAPICN(MNUMNR,CURIYR) = UCAPICN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDICN(1,IRG,CURIYR) = UADDICN(1,IRG,CURIYR) + ADD
                              UADDICN(1,MNUMNR,CURIYR) = UADDICN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDICN(2,IRG,CURIYR) = UADDICN(2,IRG,CURIYR) + ADD
                              UADDICN(2,MNUMNR,CURIYR) = UADDICN(2,MNUMNR,CURIYR) + ADD
                           END IF
!!                      ELSEIF(IOWN .EQ. 4) THEN
!                          IF(IVIN .EQ. 3) THEN
!!                            UCAPICC(IRG,CURIYR) = UCAPICC(IRG,CURIYR) + CAP
!!                            UCAPICC(MNUMNR,CURIYR) = UCAPICC(MNUMNR,CURIYR) + CAP
!                          END IF
!!                         IF(IVIN .EQ. 2) THEN
!!                            UADDICC(IRG,CURIYR) = UADDICC(IRG,CURIYR) + ADD
!!                            UADDICC(MNUMNR,CURIYR) = UADDICC(MNUMNR,CURIYR) + ADD
!!                         END IF
                        END IF                                    ! IOWN
                        END IF                                    ! ICE
!
!                       -- CALCULATE AERODERIVATIVE CAPACITY
!
                        IF (EPPLCD(IPT) .EQ. 'CTA') THEN           ! combustion turbine EPPLCD subgroup
!                         IF (IVIN .EQ. 1 .AND. IOWN .LE. 3 .AND.  &
                          IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND.  &
                              RCAP .GT. 0.0) THEN
                            URETT2U(IRG,CURIYR) = URETT2U(IRG,CURIYR) + RCAP
                            URETT2U(MNUMNR,CURIYR) = URETT2U(MNUMNR,CURIYR) + RCAP
                          END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPT2U(IRG,CURIYR) = UCAPT2U(IRG,CURIYR) + CAP
                           UCAPT2U(MNUMNR,CURIYR) = UCAPT2U(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDT2U(1,IRG,CURIYR) = UADDT2U(1,IRG,CURIYR) + ADD
                              UADDT2U(1,MNUMNR,CURIYR) = UADDT2U(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDT2U(2,IRG,CURIYR) = UADDT2U(2,IRG,CURIYR) + ADD
                              UADDT2U(2,MNUMNR,CURIYR) = UADDT2U(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPT2N(IRG,CURIYR) = UCAPT2N(IRG,CURIYR) + CAP
                           UCAPT2N(MNUMNR,CURIYR) = UCAPT2N(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDT2N(1,IRG,CURIYR) = UADDT2N(1,IRG,CURIYR) + ADD
                              UADDT2N(1,MNUMNR,CURIYR) = UADDT2N(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDT2N(2,IRG,CURIYR) = UADDT2N(2,IRG,CURIYR) + ADD
                              UADDT2N(2,MNUMNR,CURIYR) = UADDT2N(2,MNUMNR,CURIYR) + ADD
                           END IF
!!                      ELSEIF(IOWN .EQ. 4) THEN
!                          IF(IVIN .EQ. 3) THEN
!!                            UCAPT2C(IRG,CURIYR) = UCAPT2C(IRG,CURIYR) + CAP
!!                            UCAPT2C(MNUMNR,CURIYR) = UCAPT2C(MNUMNR,CURIYR) + CAP
!                          END IF
!!                         IF(IVIN .EQ. 2) THEN
!!                            UADDT2C(IRG,CURIYR) = UADDT2C(IRG,CURIYR) + ADD
!!                            UADDT2C(MNUMNR,CURIYR) = UADDT2C(MNUMNR,CURIYR) + ADD
!!                         END IF
                        END IF                                    ! IOWN
                        END IF                                    ! CTA
!
!                       -- CALCULATE COMBINED CYCLE CAPACITY (PLANTS 13-16)
!
                     ELSEIF (EPPLCD(IPT) .EQ. 'CCO' .OR. &           ! else EPPLCD main group
                             EPPLCD(IPT) .EQ. 'CCG' .OR. &
                             EPPLCD(IPT) .EQ. 'CCX' .OR. &
                             EPPLCD(IPT) .EQ. 'ACC' .OR. &
                             EPPLCD(IPT) .EQ. 'AC2' .OR. &
                             EPPLCD(IPT) .EQ. 'ACS') THEN
!                         IF (IVIN .EQ. 1 .AND. IOWN .LE. 3 .AND.  &
                          IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND.  &
                              RCAP .GT. 0.0) THEN
                            URETCCU(IRG,CURIYR) = URETCCU(IRG,CURIYR) + RCAP
                            URETCCU(MNUMNR,CURIYR) = URETCCU(MNUMNR,CURIYR) + RCAP
                          END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPCCU(IRG,CURIYR) = UCAPCCU(IRG,CURIYR) + CAP
                           UCAPCCU(MNUMNR,CURIYR) = UCAPCCU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDCCU(1,IRG,CURIYR) = UADDCCU(1,IRG,CURIYR) + ADD
                              UADDCCU(1,MNUMNR,CURIYR) = UADDCCU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDCCU(2,IRG,CURIYR) = UADDCCU(2,IRG,CURIYR) + ADD
                              UADDCCU(2,MNUMNR,CURIYR) = UADDCCU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPCCN(IRG,CURIYR) = UCAPCCN(IRG,CURIYR) + CAP
                           UCAPCCN(MNUMNR,CURIYR) = UCAPCCN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDCCN(1,IRG,CURIYR) = UADDCCN(1,IRG,CURIYR) + ADD
                              UADDCCN(1,MNUMNR,CURIYR) = UADDCCN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDCCN(2,IRG,CURIYR) = UADDCCN(2,IRG,CURIYR) + ADD
                              UADDCCN(2,MNUMNR,CURIYR) = UADDCCN(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 4) THEN
!                          IF(IVIN .LE. 2) THEN
                              CGNTCAP(IRG,CURIYR,3) = CGNTCAP(IRG,CURIYR,3) + (CAP * 1000)
                              CGNTCAP(MNUMNR,CURIYR,3) = CGNTCAP(MNUMNR,CURIYR,3) + (CAP * 1000)
!                          ELSEIF(IVIN .EQ. 3) THEN
                              UCAPCCC(IRG,CURIYR) = UCAPCCC(IRG,CURIYR) + CAP
                              UCAPCCC(MNUMNR,CURIYR) = UCAPCCC(MNUMNR,CURIYR) + CAP
!                          END IF
                           IF(IVIN .EQ. 2) THEN
                              UADDCCC(IRG,CURIYR) = UADDCCC(IRG,CURIYR) + ADD
                              UADDCCC(MNUMNR,CURIYR) = UADDCCC(MNUMNR,CURIYR) + ADD
                           END IF
                        END IF                                    ! IOWN
!
!                       -- CALCULATE ADVANCED COMBINED CYCLE CAPACITY
!
                        IF (EPPLCD(IPT) .EQ. 'ACC') THEN             ! combined cycle EPPLCD subgroup
!                         IF (IVIN .EQ. 1 .AND. IOWN .LE. 3 .AND.  &
                          IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND.  &
                              RCAP .GT. 0.0) THEN
                            URETACU(IRG,CURIYR) = URETACU(IRG,CURIYR) + RCAP
                            URETACU(MNUMNR,CURIYR) = URETACU(MNUMNR,CURIYR) + RCAP
                          END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPACU(IRG,CURIYR) = UCAPACU(IRG,CURIYR) + CAP
                           UCAPACU(MNUMNR,CURIYR) = UCAPACU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDACU(1,IRG,CURIYR) = UADDACU(1,IRG,CURIYR) + ADD
                              UADDACU(1,MNUMNR,CURIYR) = UADDACU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDACU(2,IRG,CURIYR) = UADDACU(2,IRG,CURIYR) + ADD
                              UADDACU(2,MNUMNR,CURIYR) = UADDACU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPACN(IRG,CURIYR) = UCAPACN(IRG,CURIYR) + CAP
                           UCAPACN(MNUMNR,CURIYR) = UCAPACN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDACN(1,IRG,CURIYR) = UADDACN(1,IRG,CURIYR) + ADD
                              UADDACN(1,MNUMNR,CURIYR) = UADDACN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDACN(2,IRG,CURIYR) = UADDACN(2,IRG,CURIYR) + ADD
                              UADDACN(2,MNUMNR,CURIYR) = UADDACN(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 4) THEN
!                          IF(IVIN .EQ. 3) THEN
                              UCAPACC(IRG,CURIYR) = UCAPACC(IRG,CURIYR) + CAP
                              UCAPACC(MNUMNR,CURIYR) = UCAPACC(MNUMNR,CURIYR) + CAP
!                          END IF
                           IF(IVIN .EQ. 2) THEN
                              UADDACC(IRG,CURIYR) = UADDACC(IRG,CURIYR) + ADD
                              UADDACC(MNUMNR,CURIYR) = UADDACC(MNUMNR,CURIYR) + ADD
                           END IF
                        END IF                                    ! IOWN
                        END IF                                     ! ACC
!
!                       -- CALCULATE ADVANCED COMBINED CYCLE CAPACITY W/SEQUESTRATION
!
                        IF (EPPLCD(IPT) .EQ. 'ACS') THEN             ! combined cycle EPPLCD subgroup
!                         IF (IVIN .EQ. 1 .AND. IOWN .LE. 3 .AND.  &
                          IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND.  &
                              RCAP .GT. 0.0) THEN
                            URETASU(IRG,CURIYR) = URETASU(IRG,CURIYR) + RCAP
                            URETASU(MNUMNR,CURIYR) = URETASU(MNUMNR,CURIYR) + RCAP
                          END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPASU(IRG,CURIYR) = UCAPASU(IRG,CURIYR) + CAP
                           UCAPASU(MNUMNR,CURIYR) = UCAPASU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDASU(1,IRG,CURIYR) = UADDASU(1,IRG,CURIYR) + ADD
                              UADDASU(1,MNUMNR,CURIYR) = UADDASU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDASU(2,IRG,CURIYR) = UADDASU(2,IRG,CURIYR) + ADD
                              UADDASU(2,MNUMNR,CURIYR) = UADDASU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPASN(IRG,CURIYR) = UCAPASN(IRG,CURIYR) + CAP
                           UCAPASN(MNUMNR,CURIYR) = UCAPASN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDASN(1,IRG,CURIYR) = UADDASN(1,IRG,CURIYR) + ADD
                              UADDASN(1,MNUMNR,CURIYR) = UADDASN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDASN(2,IRG,CURIYR) = UADDASN(2,IRG,CURIYR) + ADD
                              UADDASN(2,MNUMNR,CURIYR) = UADDASN(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 4) THEN
!                          IF(IVIN .EQ. 3) THEN
!                             UCAPASC(IRG,CURIYR) = UCAPASC(IRG,CURIYR) + CAP
!                             UCAPASC(MNUMNR,CURIYR) = UCAPASC(MNUMNR,CURIYR) + CAP
!                          END IF
                        END IF                                    ! IOWN
                        END IF                                     ! ACS

 !                       -- CALCULATE ADVANCED COMBINED CYCLE CAPACITY W/SEQUESTRATION
!
                        IF (EPPLCD(IPT) .EQ. 'AC2') THEN             ! combined cycle EPPLCD subgroup
!                         IF (IVIN .EQ. 1 .AND. IOWN .LE. 3 .AND.  &
                          IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND.  &
                              RCAP .GT. 0.0) THEN
                            URETA2U(IRG,CURIYR) = URETA2U(IRG,CURIYR) + RCAP
                            URETA2U(MNUMNR,CURIYR) = URETA2U(MNUMNR,CURIYR) + RCAP
                          END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPA2U(IRG,CURIYR) = UCAPA2U(IRG,CURIYR) + CAP
                           UCAPA2U(MNUMNR,CURIYR) = UCAPA2U(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDA2U(1,IRG,CURIYR) = UADDA2U(1,IRG,CURIYR) + ADD
                              UADDA2U(1,MNUMNR,CURIYR) = UADDA2U(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDA2U(2,IRG,CURIYR) = UADDA2U(2,IRG,CURIYR) + ADD
                              UADDA2U(2,MNUMNR,CURIYR) = UADDA2U(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPA2N(IRG,CURIYR) = UCAPA2N(IRG,CURIYR) + CAP
                           UCAPA2N(MNUMNR,CURIYR) = UCAPA2N(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDA2N(1,IRG,CURIYR) = UADDA2N(1,IRG,CURIYR) + ADD
                              UADDA2N(1,MNUMNR,CURIYR) = UADDA2N(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDA2N(2,IRG,CURIYR) = UADDA2N(2,IRG,CURIYR) + ADD
                              UADDA2N(2,MNUMNR,CURIYR) = UADDA2N(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 4) THEN
!                          IF(IVIN .EQ. 3) THEN
!                             UCAPA2C(IRG,CURIYR) = UCAPA2C(IRG,CURIYR) + CAP
!                             UCAPA2C(MNUMNR,CURIYR) = UCAPA2C(MNUMNR,CURIYR) + CAP
!                          END IF
                        END IF                                    ! IOWN
                        END IF                                     ! AC2
!
!                       -- CALCULATE NUCLEAR CAPACITY (PLANTS 18 & 19)
!                       NO NON-TRAD COGEN
!
                     ELSEIF (EPPLCD(IPT) .EQ. 'CNU' .OR. &           ! else EPPLCD main group
                             EPPLCD(IPT) .EQ. 'ANC') THEN
!                         IF (IVIN .EQ. 1 .AND. IOWN .LE. 3 .AND.  &
                          IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND.  &
                              RCAP .GT. 0.0) THEN
                            URETNUU(IRG,CURIYR) = URETNUU(IRG,CURIYR) + RCAP
                            URETNUU(MNUMNR,CURIYR) = URETNUU(MNUMNR,CURIYR) + RCAP
                          END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPNUU(IRG,CURIYR) = UCAPNUU(IRG,CURIYR) + CAP
                           UCAPNUU(MNUMNR,CURIYR) = UCAPNUU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDNUU(1,IRG,CURIYR) = UADDNUU(1,IRG,CURIYR) + ADD
                              UADDNUU(1,MNUMNR,CURIYR) = UADDNUU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDNUU(2,IRG,CURIYR) = UADDNUU(2,IRG,CURIYR) + ADD
                              UADDNUU(2,MNUMNR,CURIYR) = UADDNUU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPNUN(IRG,CURIYR) = UCAPNUN(IRG,CURIYR) + CAP
                           UCAPNUN(MNUMNR,CURIYR) = UCAPNUN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDNUN(1,IRG,CURIYR) = UADDNUN(1,IRG,CURIYR) + ADD
                              UADDNUN(1,MNUMNR,CURIYR) = UADDNUN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDNUN(2,IRG,CURIYR) = UADDNUN(2,IRG,CURIYR) + ADD
                              UADDNUN(2,MNUMNR,CURIYR) = UADDNUN(2,MNUMNR,CURIYR) + ADD
                           END IF
                        END IF                                    ! IOWN

!                       -- CALCULATE NUCLEAR CAPACITY (SMR)
!                       NO NON-TRAD COGEN
                     ELSEIF (EPPLCD(IPT) .EQ. 'SMR') THEN      ! else EPPLCD main group
!                         IF (IVIN .EQ. 1 .AND. IOWN .LE. 3 .AND.  &
                          IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND.  &
                              RCAP .GT. 0.0) THEN
                            URETSMU(IRG,CURIYR) = URETSMU(IRG,CURIYR) + RCAP
                            URETSMU(MNUMNR,CURIYR) = URETSMU(MNUMNR,CURIYR) + RCAP
                          END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPSMU(IRG,CURIYR) = UCAPSMU(IRG,CURIYR) + CAP
                           UCAPSMU(MNUMNR,CURIYR) = UCAPSMU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDSMU(1,IRG,CURIYR) = UADDSMU(1,IRG,CURIYR) + ADD
                              UADDSMU(1,MNUMNR,CURIYR) = UADDSMU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDSMU(2,IRG,CURIYR) = UADDSMU(2,IRG,CURIYR) + ADD
                              UADDSMU(2,MNUMNR,CURIYR) = UADDSMU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPSMN(IRG,CURIYR) = UCAPSMN(IRG,CURIYR) + CAP
                           UCAPSMN(MNUMNR,CURIYR) = UCAPSMN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDSMN(1,IRG,CURIYR) = UADDSMN(1,IRG,CURIYR) + ADD
                              UADDSMN(1,MNUMNR,CURIYR) = UADDSMN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDSMN(2,IRG,CURIYR) = UADDSMN(2,IRG,CURIYR) + ADD
                              UADDSMN(2,MNUMNR,CURIYR) = UADDSMN(2,MNUMNR,CURIYR) + ADD
                           END IF
                        END IF                                    ! IOWN
!
!
!                       -- CALCULATE FUEL CELLS (PLANTS 17)
!                       NO NON-TRAD COGEN
!
                     ELSEIF(EPPLCD(IPT).EQ.'FCG') THEN           ! else EPPLCD main group
!                         IF (IVIN .EQ. 1 .AND. IOWN .LE. 3 .AND.  &
                          IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND.  &
                              RCAP .GT. 0.0) THEN
                            URETFCU(IRG,CURIYR) = URETFCU(IRG,CURIYR) + RCAP
                            URETFCU(MNUMNR,CURIYR) = URETFCU(MNUMNR,CURIYR) + RCAP
                          END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPFCU(IRG,CURIYR) = UCAPFCU(IRG,CURIYR) + CAP
                           UCAPFCU(MNUMNR,CURIYR) = UCAPFCU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDFCU(1,IRG,CURIYR) = UADDFCU(1,IRG,CURIYR) + ADD
                              UADDFCU(1,MNUMNR,CURIYR) = UADDFCU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDFCU(2,IRG,CURIYR) = UADDFCU(2,IRG,CURIYR) + ADD
                              UADDFCU(2,MNUMNR,CURIYR) = UADDFCU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPFCN(IRG,CURIYR) = UCAPFCN(IRG,CURIYR) + CAP
                           UCAPFCN(MNUMNR,CURIYR) = UCAPFCN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDFCN(1,IRG,CURIYR) = UADDFCN(1,IRG,CURIYR) + ADD
                              UADDFCN(1,MNUMNR,CURIYR) = UADDFCN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDFCN(2,IRG,CURIYR) = UADDFCN(2,IRG,CURIYR) + ADD
                              UADDFCN(2,MNUMNR,CURIYR) = UADDFCN(2,MNUMNR,CURIYR) + ADD
                           END IF
                        END IF                                    ! IOWN
!
!                       GEOTHERMAL, also put in RN total renewable variables
!
                     ELSEIF(EPPLCD(IPT).EQ.'GTH') THEN           ! else EPPLCD main group
!                         IF (IVIN .EQ. 1 .AND. IOWN .LE. 3 .AND.  &
                          IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND.  &
                              RCAP .GT. 0.0) THEN
                            URETGEU(IRG,CURIYR) = URETGEU(IRG,CURIYR) + RCAP
                            URETGEU(MNUMNR,CURIYR) = URETGEU(MNUMNR,CURIYR) + RCAP
                            URETRNU(IRG,CURIYR) = URETRNU(IRG,CURIYR) + RCAP
                            URETRNU(MNUMNR,CURIYR) = URETRNU(MNUMNR,CURIYR) + RCAP
                          END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPRNU(IRG,CURIYR) = UCAPRNU(IRG,CURIYR) + CAP
                           UCAPRNU(MNUMNR,CURIYR) = UCAPRNU(MNUMNR,CURIYR) + CAP
                           UCAPGEU(IRG,CURIYR) = UCAPGEU(IRG,CURIYR) + CAP
                           UCAPGEU(MNUMNR,CURIYR) = UCAPGEU(MNUMNR,CURIYR) + CAP
                            IF(IVIN .EQ. 2) THEN
                              UADDGEU(1,IRG,CURIYR) = UADDGEU(1,IRG,CURIYR) + ADD
                              UADDGEU(1,MNUMNR,CURIYR) = UADDGEU(1,MNUMNR,CURIYR) + ADD
                              UADDRNU(1,IRG,CURIYR) = UADDRNU(1,IRG,CURIYR) + ADD
                              UADDRNU(1,MNUMNR,CURIYR) = UADDRNU(1,MNUMNR,CURIYR) + ADD
                            ELSEIF(IVIN .EQ. 3) THEN
                              UADDGEU(2,IRG,CURIYR) = UADDGEU(2,IRG,CURIYR) + ADD
                              UADDGEU(2,MNUMNR,CURIYR) = UADDGEU(2,MNUMNR,CURIYR) + ADD
                              UADDRNU(2,IRG,CURIYR) = UADDRNU(2,IRG,CURIYR) + ADD
                              UADDRNU(2,MNUMNR,CURIYR) = UADDRNU(2,MNUMNR,CURIYR) + ADD
                            END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPGEN(IRG,CURIYR) = UCAPGEN(IRG,CURIYR) + CAP
                           UCAPGEN(MNUMNR,CURIYR) = UCAPGEN(MNUMNR,CURIYR) + CAP
                           UCAPRNN(IRG,CURIYR) = UCAPRNN(IRG,CURIYR) + CAP
                           UCAPRNN(MNUMNR,CURIYR) = UCAPRNN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                             UADDGEN(1,IRG,CURIYR) = UADDGEN(1,IRG,CURIYR) + ADD
                             UADDGEN(1,MNUMNR,CURIYR) = UADDGEN(1,MNUMNR,CURIYR) + ADD
                             UADDRNN(1,IRG,CURIYR) = UADDRNN(1,IRG,CURIYR) + ADD
                             UADDRNN(1,MNUMNR,CURIYR) = UADDRNN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                             UADDGEN(2,IRG,CURIYR) = UADDGEN(2,IRG,CURIYR) + ADD
                             UADDGEN(2,MNUMNR,CURIYR) = UADDGEN(2,MNUMNR,CURIYR) + ADD
                             UADDRNN(2,IRG,CURIYR) = UADDRNN(2,IRG,CURIYR) + ADD
                             UADDRNN(2,MNUMNR,CURIYR) = UADDRNN(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 4) THEN
!                          IF(IVIN .LE. 2) THEN
                              CGNTCAP(IRG,CURIYR,5) = CGNTCAP(IRG,CURIYR,5) + (CAP * 1000)
                              CGNTCAP(MNUMNR,CURIYR,5) = CGNTCAP(MNUMNR,CURIYR,5) + CAP
!                          ELSEIF(IVIN .EQ. 3) THEN
                              UCAPGEC(IRG,CURIYR) = UCAPGEC(IRG,CURIYR) + CAP
                              UCAPGEC(MNUMNR,CURIYR) = UCAPGEC(MNUMNR,CURIYR) + CAP
                              UCAPRNC(IRG,CURIYR) = UCAPRNC(IRG,CURIYR) + CAP
                              UCAPRNC(MNUMNR,CURIYR) = UCAPRNC(MNUMNR,CURIYR) + CAP
!                          END IF
                           IF(IVIN .EQ. 2) THEN
                              UADDGEC(IRG,CURIYR) = UADDGEC(IRG,CURIYR) + ADD
                              UADDGEC(MNUMNR,CURIYR) = UADDGEC(MNUMNR,CURIYR) + ADD
                              UADDRNC(IRG,CURIYR) = UADDRNC(IRG,CURIYR) + ADD
                              UADDRNC(MNUMNR,CURIYR) = UADDRNC(MNUMNR,CURIYR) + ADD
                           END IF
                        END IF                                    ! IOWN
!
!                       WOOD, also put in RN total renewable variables
!
                     ELSEIF(EPPLCD(IPT).EQ.'BMS') THEN           ! else EPPLCD main group
!                         IF (IVIN .EQ. 1 .AND. IOWN .LE. 3 .AND.  &
                          IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND.  &
                              RCAP .GT. 0.0) THEN
                            URETWDU(IRG,CURIYR) = URETWDU(IRG,CURIYR) + RCAP
                            URETWDU(MNUMNR,CURIYR) = URETWDU(MNUMNR,CURIYR) + RCAP
                            URETRNU(IRG,CURIYR) = URETRNU(IRG,CURIYR) + RCAP
                            URETRNU(MNUMNR,CURIYR) = URETRNU(MNUMNR,CURIYR) + RCAP
                          END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPWDU(IRG,CURIYR) = UCAPWDU(IRG,CURIYR) + CAP
                           UCAPWDU(MNUMNR,CURIYR) = UCAPWDU(MNUMNR,CURIYR) + CAP
                           UCAPRNU(IRG,CURIYR) = UCAPRNU(IRG,CURIYR) + CAP
                           UCAPRNU(MNUMNR,CURIYR) = UCAPRNU(MNUMNR,CURIYR) + CAP
                            IF(IVIN .EQ. 2) THEN
                              UADDWDU(1,IRG,CURIYR) = UADDWDU(1,IRG,CURIYR) + ADD
                              UADDWDU(1,MNUMNR,CURIYR) = UADDWDU(1,MNUMNR,CURIYR) + ADD
                              UADDRNU(1,IRG,CURIYR) = UADDRNU(1,IRG,CURIYR) + ADD
                              UADDRNU(1,MNUMNR,CURIYR) = UADDRNU(1,MNUMNR,CURIYR) + ADD
                            ELSEIF(IVIN .EQ. 3) THEN
                              UADDWDU(2,IRG,CURIYR) = UADDWDU(2,IRG,CURIYR) + ADD
                              UADDWDU(2,MNUMNR,CURIYR) = UADDWDU(2,MNUMNR,CURIYR) + ADD
                              UADDRNU(2,IRG,CURIYR) = UADDRNU(2,IRG,CURIYR) + ADD
                              UADDRNU(2,MNUMNR,CURIYR) = UADDRNU(2,MNUMNR,CURIYR) + ADD
                            END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPWDN(IRG,CURIYR) = UCAPWDN(IRG,CURIYR) + CAP
                           UCAPWDN(MNUMNR,CURIYR) = UCAPWDN(MNUMNR,CURIYR) + CAP
                           UCAPRNN(IRG,CURIYR) = UCAPRNN(IRG,CURIYR) + CAP
                           UCAPRNN(MNUMNR,CURIYR) = UCAPRNN(MNUMNR,CURIYR) + CAP
                            IF(IVIN .EQ. 2) THEN
                              UADDWDN(1,IRG,CURIYR) = UADDWDN(1,IRG,CURIYR) + ADD
                              UADDWDN(1,MNUMNR,CURIYR) = UADDWDN(1,MNUMNR,CURIYR) + ADD
                              UADDRNN(1,IRG,CURIYR) = UADDRNN(1,IRG,CURIYR) + ADD
                              UADDRNN(1,MNUMNR,CURIYR) = UADDRNN(1,MNUMNR,CURIYR) + ADD
                            ELSEIF(IVIN .EQ. 3) THEN
                              UADDWDN(2,IRG,CURIYR) = UADDWDN(2,IRG,CURIYR) + ADD
                              UADDWDN(2,MNUMNR,CURIYR) = UADDWDN(2,MNUMNR,CURIYR) + ADD
                              UADDRNN(2,IRG,CURIYR) = UADDRNN(2,IRG,CURIYR) + ADD
                              UADDRNN(2,MNUMNR,CURIYR) = UADDRNN(2,MNUMNR,CURIYR) + ADD
                            END IF
                        ELSEIF(IOWN .EQ. 4) THEN
!                          IF(IVIN .LE. 2) THEN
                              CGNTCAP(IRG,CURIYR,7) = CGNTCAP(IRG,CURIYR,7) + (CAP * 1000)
                              CGNTCAP(MNUMNR,CURIYR,7) = CGNTCAP(MNUMNR,CURIYR,7) + (CAP * 1000)
!                          ELSEIF(IVIN .EQ. 3) THEN
                              UCAPWDC(IRG,CURIYR) = UCAPWDC(IRG,CURIYR) + CAP
                              UCAPWDC(MNUMNR,CURIYR) = UCAPWDC(MNUMNR,CURIYR) + CAP
                              UCAPRNC(IRG,CURIYR) = UCAPRNC(IRG,CURIYR) + CAP
                              UCAPRNC(MNUMNR,CURIYR) = UCAPRNC(MNUMNR,CURIYR) + CAP
!                          END IF
                           IF(IVIN .EQ. 2) THEN
                              UADDWDC(IRG,CURIYR) = UADDWDC(IRG,CURIYR) + ADD
                              UADDWDC(MNUMNR,CURIYR) = UADDWDC(MNUMNR,CURIYR) + ADD
                              UADDRNC(IRG,CURIYR) = UADDRNC(IRG,CURIYR) + ADD
                              UADDRNC(MNUMNR,CURIYR) = UADDRNC(MNUMNR,CURIYR) + ADD
                           END IF
                        END IF                                    ! IOWN
                     ELSE             ! fell through, no matching EPPLCD IF
                        write(6,'("  Warn DSP:  Plant type ",A," with capacity, addtions= ",F12.5,F9.5," fell through.")') EPPLCD(IPT), CAP, ADD
                     END IF                            ! IPT COMPARISONS (end EPPLCD main group)
                  END IF                                    ! CAP .GT. 0
               ELSEIF(IPT .GT. EFD_D_DSP .AND.  &
                      IPT .LE. (EFD_D_DSP + EFD_D_RNW)) THEN    ! RNW PLANTS
!
!                 NON-DISPATCHABLE PLANTS TYPES 20-26
!                 EHSCAP IS THE CAPACITY ARRAY FOR PLANTS USING
!                 RENEWABLE FUELS (FROM THE COMMON BLOCK)
!
!                 -- ACCUMULATE TOTAL RETIREMENTS OF EXISTING CAPACITY
!
                  RCAP = EHSRET((IPT - EFD_D_DSP),IOWN) * .001
                  IF(IVIN .EQ. 1) THEN
!                    IF(IOWN .LE. 3 .AND. RCAP .GT. 0.0) THEN
                     IF(IOWN .LE. 4 .AND. RCAP .GT. 0.0) THEN
!                       WRITE(22,2626) CURIYR,IRG,IPT,IOWN,IVIN,RCAP,EPPLCD(IPT)
!2626  FORMAT(1H ,'UTHRET=',5I5,F10.3,A10)
                        URETTLU(IRG,CURIYR) = URETTLU(IRG,CURIYR) + RCAP
                        URETTLU(MNUMNR,CURIYR) = URETTLU(MNUMNR,CURIYR) + RCAP
                     END IF
                  END IF
                  CAP = EHSCAP((IPT - EFD_D_DSP),IVIN,IOWN) * .001
                  ADD = EHSADD((IPT - EFD_D_DSP),IVIN,IOWN) * .001
                  IF(CAP .GT. 0.0 .OR. ADD .GT. 0.0 .OR. RCAP .GT. 0.0) THEN
!                    WRITE(22,3535) IRG,IPT,IVIN,IOWN,CAP
!3535  FORMAT(1H ,' RG,PT,VIN,OWN,CAP',4I5,F10.3)
!
!                    -- ACCUMULATE TOTAL CAPACITY AND ADDITIONS
!
                     IF(IOWN .LE. 2) THEN
                        UCAPTLU(IRG,CURIYR) = UCAPTLU(IRG,CURIYR) + CAP
                        UCAPTLU(MNUMNR,CURIYR) = UCAPTLU(MNUMNR,CURIYR) + CAP
                        IF(IVIN .EQ. 2) THEN
                           UADDTLU(1,IRG,CURIYR) = UADDTLU(1,IRG,CURIYR) + ADD
                           UADDTLU(1,MNUMNR,CURIYR) = UADDTLU(1,MNUMNR,CURIYR) + ADD
                        ELSEIF(IVIN .EQ. 3) THEN
                           UADDTLU(2,IRG,CURIYR) = UADDTLU(2,IRG,CURIYR) + ADD
                           UADDTLU(2,MNUMNR,CURIYR) = UADDTLU(2,MNUMNR,CURIYR) + ADD
                        END IF
                     ELSEIF(IOWN .EQ. 3) THEN
                        UCAPTLN(IRG,CURIYR) = UCAPTLN(IRG,CURIYR) + CAP
                        UCAPTLN(MNUMNR,CURIYR) = UCAPTLN(MNUMNR,CURIYR) + CAP
                        IF(IVIN .EQ. 2) THEN
                           UADDTLN(1,IRG,CURIYR) = UADDTLN(1,IRG,CURIYR) + ADD
                           UADDTLN(1,MNUMNR,CURIYR) = UADDTLN(1,MNUMNR,CURIYR) + ADD
                        ELSEIF(IVIN .EQ. 3) THEN
                           UADDTLN(2,IRG,CURIYR) = UADDTLN(2,IRG,CURIYR) + ADD
                           UADDTLN(2,MNUMNR,CURIYR) = UADDTLN(2,MNUMNR,CURIYR) + ADD
                        END IF
                     ELSEIF(IOWN .EQ. 4) THEN
!                       IF(IVIN .EQ. 3) THEN
                           UCAPTLC(IRG,CURIYR) = UCAPTLC(IRG,CURIYR) + CAP
                           UCAPTLC(MNUMNR,CURIYR) = UCAPTLC(MNUMNR,CURIYR) + CAP
!                       END IF
                        IF(IVIN .EQ. 2) THEN
                           UADDTLC(IRG,CURIYR) = UADDTLC(IRG,CURIYR) + ADD
                           UADDTLC(MNUMNR,CURIYR) = UADDTLC(MNUMNR,CURIYR) + ADD
                        END IF
                     END IF                                       ! IOWN
!
!                    -- CALCULATE PUMPED STORAGE CAPACITY
!
                     IF(EPPLCD(IPT).EQ.'HYR') THEN
!                         IF (IVIN .EQ. 1 .AND. IOWN .LE. 3 .AND.  &
                          IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND.  &
                              RCAP .GT. 0.0) THEN
                            URETPSU(IRG,CURIYR) = URETPSU(IRG,CURIYR) + RCAP
                            URETPSU(MNUMNR,CURIYR) = URETPSU(MNUMNR,CURIYR) + RCAP
                          END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPPSU(IRG,CURIYR) = UCAPPSU(IRG,CURIYR) + CAP
                           UCAPPSU(MNUMNR,CURIYR) = UCAPPSU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDPSU(1,IRG,CURIYR) = UADDPSU(1,IRG,CURIYR) + ADD
                              UADDPSU(1,MNUMNR,CURIYR) = UADDPSU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDPSU(2,IRG,CURIYR) = UADDPSU(2,IRG,CURIYR) + ADD
                              UADDPSU(2,MNUMNR,CURIYR) = UADDPSU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPPSN(IRG,CURIYR) = UCAPPSN(IRG,CURIYR) + CAP
                           UCAPPSN(MNUMNR,CURIYR) = UCAPPSN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDPSN(1,IRG,CURIYR) = UADDPSN(1,IRG,CURIYR) + ADD
                              UADDPSN(1,MNUMNR,CURIYR) = UADDPSN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDPSN(2,IRG,CURIYR) = UADDPSN(2,IRG,CURIYR) + ADD
                              UADDPSN(2,MNUMNR,CURIYR) = UADDPSN(2,MNUMNR,CURIYR) + ADD
                           END IF
                        END IF                                    ! IOWN
                     END IF                    ! STORAGE VS. NON-STORAGE
!
!                       -- OTHER RENEWABLES GROUPING (All But Storage)
!
!                    NON-STORAGE HYDRO
!
                     IF(EPPLCD(IPT).EQ.'HYC') THEN
                        IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND. RCAP .GT. 0.0) THEN
                          URETHYU(IRG,CURIYR) = URETHYU(IRG,CURIYR) + RCAP
                          URETHYU(MNUMNR,CURIYR) = URETHYU(MNUMNR,CURIYR) + RCAP
                          URETRNU(IRG,CURIYR) = URETRNU(IRG,CURIYR) + RCAP
                          URETRNU(MNUMNR,CURIYR) = URETRNU(MNUMNR,CURIYR) + RCAP
                        END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPHYU(IRG,CURIYR) = UCAPHYU(IRG,CURIYR) + CAP
                           UCAPHYU(MNUMNR,CURIYR) = UCAPHYU(MNUMNR,CURIYR) + CAP
                           UCAPRNU(IRG,CURIYR) = UCAPRNU(IRG,CURIYR) + CAP
                           UCAPRNU(MNUMNR,CURIYR) = UCAPRNU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDHYU(1,IRG,CURIYR) = UADDHYU(1,IRG,CURIYR) + ADD
                              UADDHYU(1,MNUMNR,CURIYR) = UADDHYU(1,MNUMNR,CURIYR) + ADD
                              UADDRNU(1,IRG,CURIYR) = UADDRNU(1,IRG,CURIYR) + ADD
                              UADDRNU(1,MNUMNR,CURIYR) = UADDRNU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDHYU(2,IRG,CURIYR) = UADDHYU(2,IRG,CURIYR) + ADD
                              UADDHYU(2,MNUMNR,CURIYR) = UADDHYU(2,MNUMNR,CURIYR) + ADD
                              UADDRNU(2,IRG,CURIYR) = UADDRNU(2,IRG,CURIYR) + ADD
                              UADDRNU(2,MNUMNR,CURIYR) = UADDRNU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPHYN(IRG,CURIYR) = UCAPHYN(IRG,CURIYR) + CAP
                           UCAPHYN(MNUMNR,CURIYR) = UCAPHYN(MNUMNR,CURIYR) + CAP
                           UCAPRNN(IRG,CURIYR) = UCAPRNN(IRG,CURIYR) + CAP
                           UCAPRNN(MNUMNR,CURIYR) = UCAPRNN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDHYN(1,IRG,CURIYR) = UADDHYN(1,IRG,CURIYR) + ADD
                              UADDHYN(1,MNUMNR,CURIYR) = UADDHYN(1,MNUMNR,CURIYR) + ADD
                              UADDRNN(1,IRG,CURIYR) = UADDRNN(1,IRG,CURIYR) + ADD
                              UADDRNN(1,MNUMNR,CURIYR) = UADDRNN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDHYN(2,IRG,CURIYR) = UADDHYN(2,IRG,CURIYR) + ADD
                              UADDHYN(2,MNUMNR,CURIYR) = UADDHYN(2,MNUMNR,CURIYR) + ADD
                              UADDRNN(2,IRG,CURIYR) = UADDRNN(2,IRG,CURIYR) + ADD
                              UADDRNN(2,MNUMNR,CURIYR) = UADDRNN(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 4) THEN
                           CGNTCAP(IRG,CURIYR,4) = CGNTCAP(IRG,CURIYR,4) + (CAP * 1000)
                           CGNTCAP(MNUMNR,CURIYR,4) = CGNTCAP(MNUMNR,CURIYR,4) + (CAP * 1000)
                           UCAPHYC(IRG,CURIYR) = UCAPHYC(IRG,CURIYR) + CAP
                           UCAPHYC(MNUMNR,CURIYR) = UCAPHYC(MNUMNR,CURIYR) + CAP
                           UCAPRNC(IRG,CURIYR) = UCAPRNC(IRG,CURIYR) + CAP
                           UCAPRNC(MNUMNR,CURIYR) = UCAPRNC(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDHYC(IRG,CURIYR) = UADDHYC(IRG,CURIYR) + ADD
                              UADDHYC(MNUMNR,CURIYR) = UADDHYC(MNUMNR,CURIYR) + ADD
                              UADDRNC(IRG,CURIYR) = UADDRNC(IRG,CURIYR) + ADD
                              UADDRNC(MNUMNR,CURIYR) = UADDRNC(MNUMNR,CURIYR) + ADD
                           END IF
                        END IF                                    ! IOWN
!
!                       MSW
!
                     ELSEIF(EPPLCD(IPT).EQ.'MSW') THEN
                        IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND. RCAP .GT. 0.0) THEN
                          URETMSU(IRG,CURIYR) = URETMSU(IRG,CURIYR) + RCAP
                          URETMSU(MNUMNR,CURIYR) = URETMSU(MNUMNR,CURIYR) + RCAP
                          URETRNU(IRG,CURIYR) = URETRNU(IRG,CURIYR) + RCAP
                          URETRNU(MNUMNR,CURIYR) = URETRNU(MNUMNR,CURIYR) + RCAP
                        END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPMSU(IRG,CURIYR) = UCAPMSU(IRG,CURIYR) + CAP
                           UCAPMSU(MNUMNR,CURIYR) = UCAPMSU(MNUMNR,CURIYR) + CAP
                           UCAPRNU(IRG,CURIYR) = UCAPRNU(IRG,CURIYR) + CAP
                           UCAPRNU(MNUMNR,CURIYR) = UCAPRNU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDMSU(1,IRG,CURIYR) = UADDMSU(1,IRG,CURIYR) + ADD
                              UADDMSU(1,MNUMNR,CURIYR) = UADDMSU(1,MNUMNR,CURIYR) + ADD
                              UADDRNU(1,IRG,CURIYR) = UADDRNU(1,IRG,CURIYR) + ADD
                              UADDRNU(1,MNUMNR,CURIYR) = UADDRNU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDMSU(2,IRG,CURIYR) = UADDMSU(2,IRG,CURIYR) + ADD
                              UADDMSU(2,MNUMNR,CURIYR) = UADDMSU(2,MNUMNR,CURIYR) + ADD
                              UADDRNU(2,IRG,CURIYR) = UADDRNU(2,IRG,CURIYR) + ADD
                              UADDRNU(2,MNUMNR,CURIYR) = UADDRNU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPMSN(IRG,CURIYR) = UCAPMSN(IRG,CURIYR) + CAP
                           UCAPMSN(MNUMNR,CURIYR) = UCAPMSN(MNUMNR,CURIYR) + CAP
                           UCAPRNN(IRG,CURIYR) = UCAPRNN(IRG,CURIYR) + CAP
                           UCAPRNN(MNUMNR,CURIYR) = UCAPRNN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDMSN(1,IRG,CURIYR) = UADDMSN(1,IRG,CURIYR) + ADD
                              UADDMSN(1,MNUMNR,CURIYR) = UADDMSN(1,MNUMNR,CURIYR) + ADD
                              UADDRNN(1,IRG,CURIYR) = UADDRNN(1,IRG,CURIYR) + ADD
                              UADDRNN(1,MNUMNR,CURIYR) = UADDRNN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDMSN(2,IRG,CURIYR) = UADDMSN(2,IRG,CURIYR) + ADD
                              UADDMSN(2,MNUMNR,CURIYR) = UADDMSN(2,MNUMNR,CURIYR) + ADD
                              UADDRNN(2,IRG,CURIYR) = UADDRNN(2,IRG,CURIYR) + ADD
                              UADDRNN(2,MNUMNR,CURIYR) = UADDRNN(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 4) THEN
                           CGNTCAP(IRG,CURIYR,6) = CGNTCAP(IRG,CURIYR,6) + (CAP * 1000)
                           CGNTCAP(MNUMNR,CURIYR,6) = CGNTCAP(MNUMNR,CURIYR,6) + (CAP * 1000)
                           UCAPMSC(IRG,CURIYR) = UCAPMSC(IRG,CURIYR) + CAP
                           UCAPMSC(MNUMNR,CURIYR) = UCAPMSC(MNUMNR,CURIYR) + CAP
                           UCAPRNC(IRG,CURIYR) = UCAPRNC(IRG,CURIYR) + CAP
                           UCAPRNC(MNUMNR,CURIYR) = UCAPRNC(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDMSC(IRG,CURIYR) = UADDMSC(IRG,CURIYR) + ADD
                              UADDMSC(MNUMNR,CURIYR) = UADDMSC(MNUMNR,CURIYR) + ADD
                              UADDRNC(IRG,CURIYR) = UADDRNC(IRG,CURIYR) + ADD
                              UADDRNC(MNUMNR,CURIYR) = UADDRNC(MNUMNR,CURIYR) + ADD
                           END IF
                        END IF                                    ! IOWN
!
!                       SOLAR THERMAL
!
                     ELSEIF(EPPLCD(IPT).EQ.'STH') THEN
                        IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND. RCAP .GT. 0.0) THEN
                          URETSTU(IRG,CURIYR) = URETSTU(IRG,CURIYR) + RCAP
                          URETSTU(MNUMNR,CURIYR) = URETSTU(MNUMNR,CURIYR) + RCAP
                          URETRNU(IRG,CURIYR) = URETRNU(IRG,CURIYR) + RCAP
                          URETRNU(MNUMNR,CURIYR) = URETRNU(MNUMNR,CURIYR) + RCAP
                        END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPSTU(IRG,CURIYR) = UCAPSTU(IRG,CURIYR) + CAP
                           UCAPSTU(MNUMNR,CURIYR) = UCAPSTU(MNUMNR,CURIYR) + CAP
                           UCAPRNU(IRG,CURIYR) = UCAPRNU(IRG,CURIYR) + CAP
                           UCAPRNU(MNUMNR,CURIYR) = UCAPRNU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDSTU(1,IRG,CURIYR) = UADDSTU(1,IRG,CURIYR) + ADD
                              UADDSTU(1,MNUMNR,CURIYR) = UADDSTU(1,MNUMNR,CURIYR) + ADD
                              UADDRNU(1,IRG,CURIYR) = UADDRNU(1,IRG,CURIYR) + ADD
                              UADDRNU(1,MNUMNR,CURIYR) = UADDRNU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDSTU(2,IRG,CURIYR) = UADDSTU(2,IRG,CURIYR) + ADD
                              UADDSTU(2,MNUMNR,CURIYR) = UADDSTU(2,MNUMNR,CURIYR) + ADD
                              UADDRNU(2,IRG,CURIYR) = UADDRNU(2,IRG,CURIYR) + ADD
                              UADDRNU(2,MNUMNR,CURIYR) = UADDRNU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPSTN(IRG,CURIYR) = UCAPSTN(IRG,CURIYR) + CAP
                           UCAPSTN(MNUMNR,CURIYR) = UCAPSTN(MNUMNR,CURIYR) + CAP
                           UCAPRNN(IRG,CURIYR) = UCAPRNN(IRG,CURIYR) + CAP
                           UCAPRNN(MNUMNR,CURIYR) = UCAPRNN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDSTN(1,IRG,CURIYR) = UADDSTN(1,IRG,CURIYR) + ADD
                              UADDSTN(1,MNUMNR,CURIYR) = UADDSTN(1,MNUMNR,CURIYR) + ADD
                              UADDRNN(1,IRG,CURIYR) = UADDRNN(1,IRG,CURIYR) + ADD
                              UADDRNN(1,MNUMNR,CURIYR) = UADDRNN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDSTN(2,IRG,CURIYR) = UADDSTN(2,IRG,CURIYR) + ADD
                              UADDSTN(2,MNUMNR,CURIYR) = UADDSTN(2,MNUMNR,CURIYR) + ADD
                              UADDRNN(2,IRG,CURIYR) = UADDRNN(2,IRG,CURIYR) + ADD
                              UADDRNN(2,MNUMNR,CURIYR) = UADDRNN(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 4) THEN
                           CGNTCAP(IRG,CURIYR,8) = CGNTCAP(IRG,CURIYR,8) + (CAP * 1000)
                           CGNTCAP(MNUMNR,CURIYR,8) = CGNTCAP(MNUMNR,CURIYR,8) + (CAP * 1000)
                           UCAPSTC(IRG,CURIYR) = UCAPSTC(IRG,CURIYR) + CAP
                           UCAPSTC(MNUMNR,CURIYR) = UCAPSTC(MNUMNR,CURIYR) + CAP
                           UCAPRNC(IRG,CURIYR) = UCAPRNC(IRG,CURIYR) + CAP
                           UCAPRNC(MNUMNR,CURIYR) = UCAPRNC(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDSTC(IRG,CURIYR) = UADDSTC(IRG,CURIYR) + ADD
                              UADDSTC(MNUMNR,CURIYR) = UADDSTC(MNUMNR,CURIYR) + ADD
                              UADDRNC(IRG,CURIYR) = UADDRNC(IRG,CURIYR) + ADD
                              UADDRNC(MNUMNR,CURIYR) = UADDRNC(MNUMNR,CURIYR) + ADD
                           END IF
                        END IF                                    ! IOWN
!
!                       WIND
!
                     ELSEIF(EPPLCD(IPT).EQ.'WND') THEN
                          IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND. RCAP .GT. 0.0) THEN
                            URETWNU(IRG,CURIYR) = URETWNU(IRG,CURIYR) + RCAP
                            URETWNU(MNUMNR,CURIYR) = URETWNU(MNUMNR,CURIYR) + RCAP
                            URETRNU(IRG,CURIYR) = URETRNU(IRG,CURIYR) + RCAP
                            URETRNU(MNUMNR,CURIYR) = URETRNU(MNUMNR,CURIYR) + RCAP
                          END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPWNU(IRG,CURIYR) = UCAPWNU(IRG,CURIYR) + CAP
                           UCAPWNU(MNUMNR,CURIYR) = UCAPWNU(MNUMNR,CURIYR) + CAP
                           UCAPRNU(IRG,CURIYR) = UCAPRNU(IRG,CURIYR) + CAP
                           UCAPRNU(MNUMNR,CURIYR) = UCAPRNU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDWNU(1,IRG,CURIYR) = UADDWNU(1,IRG,CURIYR) + ADD
                              UADDWNU(1,MNUMNR,CURIYR) = UADDWNU(1,MNUMNR,CURIYR) + ADD
                              UADDRNU(1,IRG,CURIYR) = UADDRNU(1,IRG,CURIYR) + ADD
                              UADDRNU(1,MNUMNR,CURIYR) = UADDRNU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDWNU(2,IRG,CURIYR) = UADDWNU(2,IRG,CURIYR) + ADD
                              UADDWNU(2,MNUMNR,CURIYR) = UADDWNU(2,MNUMNR,CURIYR) + ADD
                              UADDRNU(2,IRG,CURIYR) = UADDRNU(2,IRG,CURIYR) + ADD
                              UADDRNU(2,MNUMNR,CURIYR) = UADDRNU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPWNN(IRG,CURIYR) = UCAPWNN(IRG,CURIYR) + CAP
                           UCAPWNN(MNUMNR,CURIYR) = UCAPWNN(MNUMNR,CURIYR) + CAP
                           UCAPRNN(IRG,CURIYR) = UCAPRNN(IRG,CURIYR) + CAP
                           UCAPRNN(MNUMNR,CURIYR) = UCAPRNN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDWNN(1,IRG,CURIYR) = UADDWNN(1,IRG,CURIYR) + ADD
                              UADDWNN(1,MNUMNR,CURIYR) = UADDWNN(1,MNUMNR,CURIYR) + ADD
                              UADDRNN(1,IRG,CURIYR) = UADDRNN(1,IRG,CURIYR) + ADD
                              UADDRNN(1,MNUMNR,CURIYR) = UADDRNN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDWNN(2,IRG,CURIYR) = UADDWNN(2,IRG,CURIYR) + ADD
                              UADDWNN(2,MNUMNR,CURIYR) = UADDWNN(2,MNUMNR,CURIYR) + ADD
                              UADDRNN(2,IRG,CURIYR) = UADDRNN(2,IRG,CURIYR) + ADD
                              UADDRNN(2,MNUMNR,CURIYR) = UADDRNN(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 4) THEN
                           CGNTCAP(IRG,CURIYR,9) = CGNTCAP(IRG,CURIYR,9) + (CAP * 1000)
                           CGNTCAP(MNUMNR,CURIYR,9) = CGNTCAP(MNUMNR,CURIYR,9) + (CAP * 1000)
                           UCAPWNC(IRG,CURIYR) = UCAPWNC(IRG,CURIYR) + CAP
                           UCAPWNC(MNUMNR,CURIYR) = UCAPWNC(MNUMNR,CURIYR) + CAP
                           UCAPRNC(IRG,CURIYR) = UCAPRNC(IRG,CURIYR) + CAP
                           UCAPRNC(MNUMNR,CURIYR) = UCAPRNC(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDWNC(IRG,CURIYR) = UADDWNC(IRG,CURIYR) + ADD
                              UADDWNC(MNUMNR,CURIYR) = UADDWNC(MNUMNR,CURIYR) + ADD
                              UADDRNC(IRG,CURIYR) = UADDRNC(IRG,CURIYR) + ADD
                              UADDRNC(MNUMNR,CURIYR) = UADDRNC(MNUMNR,CURIYR) + ADD
                           END IF
                        END IF

!                       Onshore Wind Low Speed

                     ELSEIF(EPPLCD(IPT).EQ.'WNL') THEN
                          IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND. RCAP .GT. 0.0) THEN
                            URETWLU(IRG,CURIYR) = URETWLU(IRG,CURIYR) + RCAP
                            URETWLU(MNUMNR,CURIYR) = URETWLU(MNUMNR,CURIYR) + RCAP
                            URETRNU(IRG,CURIYR) = URETRNU(IRG,CURIYR) + RCAP
                            URETRNU(MNUMNR,CURIYR) = URETRNU(MNUMNR,CURIYR) + RCAP
                          END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPWLU(IRG,CURIYR) = UCAPWLU(IRG,CURIYR) + CAP
                           UCAPWLU(MNUMNR,CURIYR) = UCAPWLU(MNUMNR,CURIYR) + CAP
                           UCAPRNU(IRG,CURIYR) = UCAPRNU(IRG,CURIYR) + CAP
                           UCAPRNU(MNUMNR,CURIYR) = UCAPRNU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDWLU(1,IRG,CURIYR) = UADDWLU(1,IRG,CURIYR) + ADD
                              UADDWLU(1,MNUMNR,CURIYR) = UADDWLU(1,MNUMNR,CURIYR) + ADD
                              UADDRNU(1,IRG,CURIYR) = UADDRNU(1,IRG,CURIYR) + ADD
                              UADDRNU(1,MNUMNR,CURIYR) = UADDRNU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDWLU(2,IRG,CURIYR) = UADDWLU(2,IRG,CURIYR) + ADD
                              UADDWLU(2,MNUMNR,CURIYR) = UADDWLU(2,MNUMNR,CURIYR) + ADD
                              UADDRNU(2,IRG,CURIYR) = UADDRNU(2,IRG,CURIYR) + ADD
                              UADDRNU(2,MNUMNR,CURIYR) = UADDRNU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPWLN(IRG,CURIYR) = UCAPWLN(IRG,CURIYR) + CAP
                           UCAPWLN(MNUMNR,CURIYR) = UCAPWLN(MNUMNR,CURIYR) + CAP
                           UCAPRNN(IRG,CURIYR) = UCAPRNN(IRG,CURIYR) + CAP
                           UCAPRNN(MNUMNR,CURIYR) = UCAPRNN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDWLN(1,IRG,CURIYR) = UADDWLN(1,IRG,CURIYR) + ADD
                              UADDWLN(1,MNUMNR,CURIYR) = UADDWLN(1,MNUMNR,CURIYR) + ADD
                              UADDRNN(1,IRG,CURIYR) = UADDRNN(1,IRG,CURIYR) + ADD
                              UADDRNN(1,MNUMNR,CURIYR) = UADDRNN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDWLN(2,IRG,CURIYR) = UADDWLN(2,IRG,CURIYR) + ADD
                              UADDWLN(2,MNUMNR,CURIYR) = UADDWLN(2,MNUMNR,CURIYR) + ADD
                              UADDRNN(2,IRG,CURIYR) = UADDRNN(2,IRG,CURIYR) + ADD
                              UADDRNN(2,MNUMNR,CURIYR) = UADDRNN(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 4) THEN
                           CGNTCAP(IRG,CURIYR,9) = CGNTCAP(IRG,CURIYR,9) + (CAP * 1000)
                           CGNTCAP(MNUMNR,CURIYR,9) = CGNTCAP(MNUMNR,CURIYR,9) + (CAP * 1000)
                           UCAPWLC(IRG,CURIYR) = UCAPWLC(IRG,CURIYR) + CAP
                           UCAPWLC(MNUMNR,CURIYR) = UCAPWLC(MNUMNR,CURIYR) + CAP
                           UCAPRNC(IRG,CURIYR) = UCAPRNC(IRG,CURIYR) + CAP
                           UCAPRNC(MNUMNR,CURIYR) = UCAPRNC(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDWLC(IRG,CURIYR) = UADDWLC(IRG,CURIYR) + ADD
                              UADDWLC(MNUMNR,CURIYR) = UADDWLC(MNUMNR,CURIYR) + ADD
                              UADDRNC(IRG,CURIYR) = UADDRNC(IRG,CURIYR) + ADD
                              UADDRNC(MNUMNR,CURIYR) = UADDRNC(MNUMNR,CURIYR) + ADD
                           END IF
                        END IF
!
!                       OFFSHORE WIND
!
                     ELSEIF(EPPLCD(IPT).EQ.'WFS') THEN
!           write(*,'(a,5i4,f10.4)')'UCAPWF,y,i,r,v,own,cap: ',curiyr,curitr,irg,ivin,iown,CAP
                        IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND. RCAP .GT. 0.0) THEN
                          URETWFU(IRG,CURIYR) = URETWFU(IRG,CURIYR) + RCAP
                          URETWFU(MNUMNR,CURIYR) = URETWFU(MNUMNR,CURIYR) + RCAP
                          URETRNU(IRG,CURIYR) = URETRNU(IRG,CURIYR) + RCAP
                          URETRNU(MNUMNR,CURIYR) = URETRNU(MNUMNR,CURIYR) + RCAP
                        END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPWFU(IRG,CURIYR) = UCAPWFU(IRG,CURIYR) + CAP
                           UCAPWFU(MNUMNR,CURIYR) = UCAPWFU(MNUMNR,CURIYR) + CAP
                           UCAPRNU(IRG,CURIYR) = UCAPRNU(IRG,CURIYR) + CAP
                           UCAPRNU(MNUMNR,CURIYR) = UCAPRNU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDWFU(1,IRG,CURIYR) = UADDWFU(1,IRG,CURIYR) + ADD
                              UADDWFU(1,MNUMNR,CURIYR) = UADDWFU(1,MNUMNR,CURIYR) + ADD
                              UADDRNU(1,IRG,CURIYR) = UADDRNU(1,IRG,CURIYR) + ADD
                              UADDRNU(1,MNUMNR,CURIYR) = UADDRNU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDWFU(2,IRG,CURIYR) = UADDWFU(2,IRG,CURIYR) + ADD
                              UADDWFU(2,MNUMNR,CURIYR) = UADDWFU(2,MNUMNR,CURIYR) + ADD
                              UADDRNU(2,IRG,CURIYR) = UADDRNU(2,IRG,CURIYR) + ADD
                              UADDRNU(2,MNUMNR,CURIYR) = UADDRNU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPWFN(IRG,CURIYR) = UCAPWFN(IRG,CURIYR) + CAP
                           UCAPWFN(MNUMNR,CURIYR) = UCAPWFN(MNUMNR,CURIYR) + CAP
                           UCAPRNN(IRG,CURIYR) = UCAPRNN(IRG,CURIYR) + CAP
                           UCAPRNN(MNUMNR,CURIYR) = UCAPRNN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDWFN(1,IRG,CURIYR) = UADDWFN(1,IRG,CURIYR) + ADD
                              UADDWFN(1,MNUMNR,CURIYR) = UADDWFN(1,MNUMNR,CURIYR) + ADD
                              UADDRNN(1,IRG,CURIYR) = UADDRNN(1,IRG,CURIYR) + ADD
                              UADDRNN(1,MNUMNR,CURIYR) = UADDRNN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDWFN(2,IRG,CURIYR) = UADDWFN(2,IRG,CURIYR) + ADD
                              UADDWFN(2,MNUMNR,CURIYR) = UADDWFN(2,MNUMNR,CURIYR) + ADD
                              UADDRNN(2,IRG,CURIYR) = UADDRNN(2,IRG,CURIYR) + ADD
                              UADDRNN(2,MNUMNR,CURIYR) = UADDRNN(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 4) THEN
                           CGNTCAP(IRG,CURIYR,9) = CGNTCAP(IRG,CURIYR,9) + (CAP * 1000)
                           CGNTCAP(MNUMNR,CURIYR,9) = CGNTCAP(MNUMNR,CURIYR,9) + (CAP * 1000)
                           UCAPWFC(IRG,CURIYR) = UCAPWFC(IRG,CURIYR) + CAP
                           UCAPWFC(MNUMNR,CURIYR) = UCAPWFC(MNUMNR,CURIYR) + CAP
                           UCAPRNC(IRG,CURIYR) = UCAPRNC(IRG,CURIYR) + CAP
                           UCAPRNC(MNUMNR,CURIYR) = UCAPRNC(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDWFC(IRG,CURIYR) = UADDWFC(IRG,CURIYR) + ADD
                              UADDWFC(MNUMNR,CURIYR) = UADDWFC(MNUMNR,CURIYR) + ADD
                              UADDRNC(IRG,CURIYR) = UADDRNC(IRG,CURIYR) + ADD
                              UADDRNC(MNUMNR,CURIYR) = UADDRNC(MNUMNR,CURIYR) + ADD
                           END IF
                        END IF
!
!                       SOLAR PHOTOVOLTAIC - TILT AXIS
!
                     ELSEIF(EPPLCD(IPT).EQ.'SPV') THEN
                          IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND. RCAP .GT. 0.0) THEN
                            URETPVU(IRG,CURIYR) = URETPVU(IRG,CURIYR) + RCAP
                            URETPVU(MNUMNR,CURIYR) = URETPVU(MNUMNR,CURIYR) + RCAP
                            URETRNU(IRG,CURIYR) = URETRNU(IRG,CURIYR) + RCAP
                            URETRNU(MNUMNR,CURIYR) = URETRNU(MNUMNR,CURIYR) + RCAP
                          END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPPVU(IRG,CURIYR) = UCAPPVU(IRG,CURIYR) + CAP
                           UCAPPVU(MNUMNR,CURIYR) = UCAPPVU(MNUMNR,CURIYR) + CAP
                           UCAPRNU(IRG,CURIYR) = UCAPRNU(IRG,CURIYR) + CAP
                           UCAPRNU(MNUMNR,CURIYR) = UCAPRNU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDPVU(1,IRG,CURIYR) = UADDPVU(1,IRG,CURIYR) + ADD
                              UADDPVU(1,MNUMNR,CURIYR) = UADDPVU(1,MNUMNR,CURIYR) + ADD
                              UADDRNU(1,IRG,CURIYR) = UADDRNU(1,IRG,CURIYR) + ADD
                              UADDRNU(1,MNUMNR,CURIYR) = UADDRNU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDPVU(2,IRG,CURIYR) = UADDPVU(2,IRG,CURIYR) + ADD
                              UADDPVU(2,MNUMNR,CURIYR) = UADDPVU(2,MNUMNR,CURIYR) + ADD
                              UADDRNU(2,IRG,CURIYR) = UADDRNU(2,IRG,CURIYR) + ADD
                              UADDRNU(2,MNUMNR,CURIYR) = UADDRNU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPPVN(IRG,CURIYR) = UCAPPVN(IRG,CURIYR) + CAP
                           UCAPPVN(MNUMNR,CURIYR) = UCAPPVN(MNUMNR,CURIYR) + CAP
                           UCAPRNN(IRG,CURIYR) = UCAPRNN(IRG,CURIYR) + CAP
                           UCAPRNN(MNUMNR,CURIYR) = UCAPRNN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDPVN(1,IRG,CURIYR) = UADDPVN(1,IRG,CURIYR) + ADD
                              UADDPVN(1,MNUMNR,CURIYR) = UADDPVN(1,MNUMNR,CURIYR) + ADD
                              UADDRNN(1,IRG,CURIYR) = UADDRNN(1,IRG,CURIYR) + ADD
                              UADDRNN(1,MNUMNR,CURIYR) = UADDRNN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDPVN(2,IRG,CURIYR) = UADDPVN(2,IRG,CURIYR) + ADD
                              UADDPVN(2,MNUMNR,CURIYR) = UADDPVN(2,MNUMNR,CURIYR) + ADD
                              UADDRNN(2,IRG,CURIYR) = UADDRNN(2,IRG,CURIYR) + ADD
                              UADDRNN(2,MNUMNR,CURIYR) = UADDRNN(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 4) THEN
                           CGNTCAP(IRG,CURIYR,8) = CGNTCAP(IRG,CURIYR,8) + (CAP * 1000)
                           CGNTCAP(MNUMNR,CURIYR,8) = CGNTCAP(MNUMNR,CURIYR,8) + (CAP * 1000)
                           UCAPPVC(IRG,CURIYR) = UCAPPVC(IRG,CURIYR) + CAP
                           UCAPPVC(MNUMNR,CURIYR) = UCAPPVC(MNUMNR,CURIYR) + CAP
                           UCAPRNC(IRG,CURIYR) = UCAPRNC(IRG,CURIYR) + CAP
                           UCAPRNC(MNUMNR,CURIYR) = UCAPRNC(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDPVC(IRG,CURIYR) = UADDPVC(IRG,CURIYR) + ADD
                              UADDPVC(MNUMNR,CURIYR) = UADDPVC(MNUMNR,CURIYR) + ADD
                              UADDRNC(IRG,CURIYR) = UADDRNC(IRG,CURIYR) + ADD
                              UADDRNC(MNUMNR,CURIYR) = UADDRNC(MNUMNR,CURIYR) + ADD
                           END IF
                        END IF                                    ! IOWN

!                       DIRUNAL STORAGE

                     ELSEIF(EPPLCD(IPT).EQ.'DST') THEN
                          IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND. RCAP .GT. 0.0) THEN
                            URETDSU(IRG,CURIYR) = URETDSU(IRG,CURIYR) + RCAP
                            URETDSU(MNUMNR,CURIYR) = URETDSU(MNUMNR,CURIYR) + RCAP
                          END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPDSU(IRG,CURIYR) = UCAPDSU(IRG,CURIYR) + CAP
                           UCAPDSU(MNUMNR,CURIYR) = UCAPDSU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDDSU(1,IRG,CURIYR) = UADDDSU(1,IRG,CURIYR) + ADD
                              UADDDSU(1,MNUMNR,CURIYR) = UADDDSU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDDSU(2,IRG,CURIYR) = UADDDSU(2,IRG,CURIYR) + ADD
                              UADDDSU(2,MNUMNR,CURIYR) = UADDDSU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPDSN(IRG,CURIYR) = UCAPDSN(IRG,CURIYR) + CAP
                           UCAPDSN(MNUMNR,CURIYR) = UCAPDSN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDDSN(1,IRG,CURIYR) = UADDDSN(1,IRG,CURIYR) + ADD
                              UADDDSN(1,MNUMNR,CURIYR) = UADDDSN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDDSN(2,IRG,CURIYR) = UADDDSN(2,IRG,CURIYR) + ADD
                              UADDDSN(2,MNUMNR,CURIYR) = UADDDSN(2,MNUMNR,CURIYR) + ADD
                           END IF

!                       ELSEIF(IOWN .EQ. 4) THEN
!                          CGNTCAP(IRG,CURIYR,8) = CGNTCAP(IRG,CURIYR,8) + (CAP * 1000)
!                          CGNTCAP(MNUMNR,CURIYR,8) = CGNTCAP(MNUMNR,CURIYR,8) + (CAP * 1000)
!                          UCAPDSC(IRG,CURIYR) = UCAPDSC(IRG,CURIYR) + CAP
!                          UCAPDSC(MNUMNR,CURIYR) = UCAPDSC(MNUMNR,CURIYR) + CAP
!                          UCAPRNC(IRG,CURIYR) = UCAPRNC(IRG,CURIYR) + CAP
!                          UCAPRNC(MNUMNR,CURIYR) = UCAPRNC(MNUMNR,CURIYR) + CAP
!                          IF(IVIN .EQ. 2) THEN
!                             UADDDSC(IRG,CURIYR) = UADDDSC(IRG,CURIYR) + ADD
!                             UADDDSC(MNUMNR,CURIYR) = UADDDSC(MNUMNR,CURIYR) + ADD
!                             UADDRNC(IRG,CURIYR) = UADDRNC(IRG,CURIYR) + ADD
!                             UADDRNC(MNUMNR,CURIYR) = UADDRNC(MNUMNR,CURIYR) + ADD
!                          END IF

                        END IF                                    ! IOWN

!                       SOLAR PHOTOVOLTAIC - FIXED AXIS

                     ELSEIF(EPPLCD(IPT).EQ.'PVT') THEN
                          IF (IVIN .EQ. 1 .AND. IOWN .LE. 4 .AND. RCAP .GT. 0.0) THEN
                            URETPTU(IRG,CURIYR) = URETPTU(IRG,CURIYR) + RCAP
                            URETPTU(MNUMNR,CURIYR) = URETPTU(MNUMNR,CURIYR) + RCAP
                            URETRNU(IRG,CURIYR) = URETRNU(IRG,CURIYR) + RCAP
                            URETRNU(MNUMNR,CURIYR) = URETRNU(MNUMNR,CURIYR) + RCAP
                          END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPPTU(IRG,CURIYR) = UCAPPTU(IRG,CURIYR) + CAP
                           UCAPPTU(MNUMNR,CURIYR) = UCAPPTU(MNUMNR,CURIYR) + CAP
                           UCAPRNU(IRG,CURIYR) = UCAPRNU(IRG,CURIYR) + CAP
                           UCAPRNU(MNUMNR,CURIYR) = UCAPRNU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDPTU(1,IRG,CURIYR) = UADDPTU(1,IRG,CURIYR) + ADD
                              UADDPTU(1,MNUMNR,CURIYR) = UADDPTU(1,MNUMNR,CURIYR) + ADD
                              UADDRNU(1,IRG,CURIYR) = UADDRNU(1,IRG,CURIYR) + ADD
                              UADDRNU(1,MNUMNR,CURIYR) = UADDRNU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDPTU(2,IRG,CURIYR) = UADDPTU(2,IRG,CURIYR) + ADD
                              UADDPTU(2,MNUMNR,CURIYR) = UADDPTU(2,MNUMNR,CURIYR) + ADD
                              UADDRNU(2,IRG,CURIYR) = UADDRNU(2,IRG,CURIYR) + ADD
                              UADDRNU(2,MNUMNR,CURIYR) = UADDRNU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPPTN(IRG,CURIYR) = UCAPPTN(IRG,CURIYR) + CAP
                           UCAPPTN(MNUMNR,CURIYR) = UCAPPTN(MNUMNR,CURIYR) + CAP
                           UCAPRNN(IRG,CURIYR) = UCAPRNN(IRG,CURIYR) + CAP
                           UCAPRNN(MNUMNR,CURIYR) = UCAPRNN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDPTN(1,IRG,CURIYR) = UADDPTN(1,IRG,CURIYR) + ADD
                              UADDPTN(1,MNUMNR,CURIYR) = UADDPTN(1,MNUMNR,CURIYR) + ADD
                              UADDRNN(1,IRG,CURIYR) = UADDRNN(1,IRG,CURIYR) + ADD
                              UADDRNN(1,MNUMNR,CURIYR) = UADDRNN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDPTN(2,IRG,CURIYR) = UADDPTN(2,IRG,CURIYR) + ADD
                              UADDPTN(2,MNUMNR,CURIYR) = UADDPTN(2,MNUMNR,CURIYR) + ADD
                              UADDRNN(2,IRG,CURIYR) = UADDRNN(2,IRG,CURIYR) + ADD
                              UADDRNN(2,MNUMNR,CURIYR) = UADDRNN(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 4) THEN
                           CGNTCAP(IRG,CURIYR,8) = CGNTCAP(IRG,CURIYR,8) + (CAP * 1000)
                           CGNTCAP(MNUMNR,CURIYR,8) = CGNTCAP(MNUMNR,CURIYR,8) + (CAP * 1000)
                           UCAPPTC(IRG,CURIYR) = UCAPPTC(IRG,CURIYR) + CAP
                           UCAPPTC(MNUMNR,CURIYR) = UCAPPTC(MNUMNR,CURIYR) + CAP
                           UCAPRNC(IRG,CURIYR) = UCAPRNC(IRG,CURIYR) + CAP
                           UCAPRNC(MNUMNR,CURIYR) = UCAPRNC(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDPTC(IRG,CURIYR) = UADDPTC(IRG,CURIYR) + ADD
                              UADDPTC(MNUMNR,CURIYR) = UADDPTC(MNUMNR,CURIYR) + ADD
                              UADDRNC(IRG,CURIYR) = UADDRNC(IRG,CURIYR) + ADD
                              UADDRNC(MNUMNR,CURIYR) = UADDRNC(MNUMNR,CURIYR) + ADD
                           END IF
                        END IF                                    ! IOWN
                     END IF                  ! IPT FOR EACH RNW CAP TYPE
                  END IF                                    ! CAP .GT. 0
!
               ELSEIF (IPT .GT. (EFD_D_DSP + EFD_D_RNW)) THEN    ! Distributed Generation Plants
!
!                 DISTRIBUTED GENERATION PLANTS (30 & 31)
!                 EDSCAP IS THE CAPACITY ARRAY FOR DISTRIBUTED GENERATION PLANTS
!
!                 -- ACCUMULATE TOTAL RETIREMENTS OF EXISTING CAPACITY
!                 CURRENTLY NO DGEN RETIREMENTS
!
!                 RCAP = EDSRET((IPT - (EFD_D_DSP + EFD_D_RNW)),IOWN) * .001
!                 IF(IVIN .EQ. 1) THEN
!                    IF(IOWN .LE. 3 .AND. RCAP .GT. 0.0) THEN
!                       URETTLU(IRG,CURIYR) = URETTLU(IRG,CURIYR) + RCAP
!                       URETTLU(MNUMNR,CURIYR) = &
!                        URETTLU(MNUMNR,CURIYR) + RCAP
!                    END IF
!                 END IF
!
                  CAP = EDSCAP((IPT - (EFD_D_DSP + EFD_D_RNW)),IVIN,IOWN) * .001
                  ADD = EDSADD((IPT - (EFD_D_DSP + EFD_D_RNW)),IVIN,IOWN) * .001
                  IF(CAP .GT. 0.0 .OR. ADD .GT. 0.0) THEN
!                    WRITE(22,3535) IRG,IPT,IVIN,IOWN,CAP
!3535  FORMAT(1H ,' RG,PT,VIN,OWN,CAP',4I5,F10.3)
!
!                    -- ACCUMULATE TOTAL CAPACITY AND ADDITIONS
!
                     IF(IOWN .LE. 2) THEN
                        UCAPTLU(IRG,CURIYR) = UCAPTLU(IRG,CURIYR) + CAP
                        UCAPTLU(MNUMNR,CURIYR) = &
                         UCAPTLU(MNUMNR,CURIYR) + CAP
                        IF(IVIN .EQ. 2) THEN
                           UADDTLU(1,IRG,CURIYR) = &
                            UADDTLU(1,IRG,CURIYR) + ADD
                           UADDTLU(1,MNUMNR,CURIYR) = &
                            UADDTLU(1,MNUMNR,CURIYR) + ADD
                        ELSEIF(IVIN .EQ. 3) THEN
                           UADDTLU(2,IRG,CURIYR) = &
                            UADDTLU(2,IRG,CURIYR) + ADD
                           UADDTLU(2,MNUMNR,CURIYR) = &
                            UADDTLU(2,MNUMNR,CURIYR) + ADD
                        END IF
                     ELSEIF(IOWN .EQ. 3) THEN
                        UCAPTLN(IRG,CURIYR) = UCAPTLN(IRG,CURIYR) + CAP
                        UCAPTLN(MNUMNR,CURIYR) = &
                         UCAPTLN(MNUMNR,CURIYR) + CAP
                        IF(IVIN .EQ. 2) THEN
                           UADDTLN(1,IRG,CURIYR) = &
                            UADDTLN(1,IRG,CURIYR) + ADD
                           UADDTLN(1,MNUMNR,CURIYR) = &
                            UADDTLN(1,MNUMNR,CURIYR) + ADD
                        ELSEIF(IVIN .EQ. 3) THEN
                           UADDTLN(2,IRG,CURIYR) = &
                            UADDTLN(2,IRG,CURIYR) + ADD
                           UADDTLN(2,MNUMNR,CURIYR) = &
                            UADDTLN(2,MNUMNR,CURIYR) + ADD
                        END IF
                     ELSEIF(IOWN .EQ. 4) THEN
                        IF(IVIN .EQ. 3) THEN
                           UCAPTLC(IRG,CURIYR) = &
                            UCAPTLC(IRG,CURIYR) + CAP
                           UCAPTLC(MNUMNR,CURIYR) = &
                            UCAPTLC(MNUMNR,CURIYR) + CAP
                        END IF
                     END IF                                       ! IOWN
!
!                    -- CALCULATE BASE LOAD DISTRIBUTED GENERATION
!
                     IF(EPPLCD(IPT).EQ.'DGB') THEN
!                         IF (IVIN .EQ. 1 .AND. IOWN .LE. 3 .AND.  &
!                             RCAP .GT. 0.0) THEN
!                           URETDBU(IRG,CURIYR) = &
!                             URETDBU(IRG,CURIYR) + RCAP
!                           URETDBU(MNUMNR,CURIYR) = &
!                             URETDBU(MNUMNR,CURIYR) + RCAP
!                         END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPDBU(IRG,CURIYR) = &
                            UCAPDBU(IRG,CURIYR) + CAP
                           UCAPDBU(MNUMNR,CURIYR) = &
                            UCAPDBU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDDBU(1,IRG,CURIYR) = &
                               UADDDBU(1,IRG,CURIYR) + ADD
                              UADDDBU(1,MNUMNR,CURIYR) = &
                               UADDDBU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDDBU(2,IRG,CURIYR) = &
                               UADDDBU(2,IRG,CURIYR) + ADD
                              UADDDBU(2,MNUMNR,CURIYR) = &
                               UADDDBU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPDBN(IRG,CURIYR) = &
                            UCAPDBN(IRG,CURIYR) + CAP
                           UCAPDBN(MNUMNR,CURIYR) = &
                            UCAPDBN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDDBN(1,IRG,CURIYR) = &
                               UADDDBN(1,IRG,CURIYR) + ADD
                              UADDDBN(1,MNUMNR,CURIYR) = &
                               UADDDBN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDDBN(2,IRG,CURIYR) = &
                               UADDDBN(2,IRG,CURIYR) + ADD
                              UADDDBN(2,MNUMNR,CURIYR) = &
                               UADDDBN(2,MNUMNR,CURIYR) + ADD
                           END IF
                        END IF                                    ! IOWN
!
!                    -- CALCULATE PEAK LOAD DISTRIBUTED GENERATION
!
                     ELSEIF ( EPPLCD(IPT) .EQ. 'DGP' ) THEN
!                         IF (IVIN .EQ. 1 .AND. IOWN .LE. 3 .AND.  &
!                             RCAP .GT. 0.0) THEN
!                           URETDPU(IRG,CURIYR) = &
!                             URETDPU(IRG,CURIYR) + RCAP
!                           URETDPU(MNUMNR,CURIYR) = &
!                             URETDPU(MNUMNR,CURIYR) + RCAP
!                         END IF
                        IF(IOWN .LE. 2) THEN
                           UCAPDPU(IRG,CURIYR) = &
                            UCAPDPU(IRG,CURIYR) + CAP
                           UCAPDPU(MNUMNR,CURIYR) = &
                            UCAPDPU(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDDPU(1,IRG,CURIYR) = &
                               UADDDPU(1,IRG,CURIYR) + ADD
                              UADDDPU(1,MNUMNR,CURIYR) = &
                               UADDDPU(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDDPU(2,IRG,CURIYR) = &
                               UADDDPU(2,IRG,CURIYR) + ADD
                              UADDDPU(2,MNUMNR,CURIYR) = &
                               UADDDPU(2,MNUMNR,CURIYR) + ADD
                           END IF
                        ELSEIF(IOWN .EQ. 3) THEN
                           UCAPDPN(IRG,CURIYR) = &
                            UCAPDPN(IRG,CURIYR) + CAP
                           UCAPDPN(MNUMNR,CURIYR) = &
                            UCAPDPN(MNUMNR,CURIYR) + CAP
                           IF(IVIN .EQ. 2) THEN
                              UADDDPN(1,IRG,CURIYR) = &
                               UADDDPN(1,IRG,CURIYR) + ADD
                              UADDDPN(1,MNUMNR,CURIYR) = &
                               UADDDPN(1,MNUMNR,CURIYR) + ADD
                           ELSEIF(IVIN .EQ. 3) THEN
                              UADDDPN(2,IRG,CURIYR) = &
                               UADDDPN(2,IRG,CURIYR) + ADD
                              UADDDPN(2,MNUMNR,CURIYR) = &
                               UADDDPN(2,MNUMNR,CURIYR) + ADD
                           END IF
                        END IF                                    ! IOWN
                     END IF                          ! END DGEN PTYPE IF
                  END IF                                    ! CAP .GT. 0
               END IF                               ! IPT IS DSP/RNW/DGN
!
            END DO                                   ! END OF PLANT LOOP
         END DO                                  ! END OF OWNERSHIP LOOP
!      store P2 capacity
        IF (EHSCAPP2(IVIN) .GT. 0.0) THEN
            CAP = EHSCAPP2(IVIN) * 0.001
            ADD = EHSADDP2(IVIN) * 0.001
            UCAPP2(IRG,CURIYR) = UCAPP2(IRG,CURIYR) + CAP
            UCAPP2(MNUMNR,CURIYR) = UCAPP2(MNUMNR,CURIYR) + CAP
            IF (IVIN .EQ. 2) THEN
               UADDP2(1,IRG,CURIYR) = UADDP2(1,IRG,CURIYR) + ADD
               UADDP2(1,MNUMNR,CURIYR) = UADDP2(1,MNUMNR,CURIYR) + ADD
            ELSEIF (IVIN .EQ. 3) THEN
               UADDP2(2,IRG,CURIYR) = UADDP2(2,IRG,CURIYR) + ADD
               UADDP2(2,MNUMNR,CURIYR) = UADDP2(2,MNUMNR,CURIYR) + ADD
            ENDIF
        ENDIF
      END DO                                       ! END OF VINTAGE LOOP
      END DO                                        ! END OF REGION LOOP
!
!  WRITE OUT FUTURE COAL-FIRED CAPACITY ADDITIONS FOR COAL SUPPLY SUBMODULE
!
      DO IRG = 1,NDRGG
      DO YEAR = 1 , UNXPH
         UADDCST(IRG,CURIYR + YEAR - 1) = ECLCAP(IRG,YEAR)
      END DO
!     PRINT *,'UADDCST',(UADDCST(IRG,CURIYR+YEAR-1),YEAR=1,UNXPH)
      END DO

!  CALCULATE ADJUSTMENT FOR NUCLEAR RETIREMENTS FROM DERATES
      IF (NUCDRPT .EQ. 1) THEN
      DO IRG = 1, UNRGNS
        DO YEAR = 1, CURIYR
           NUCADJ(IRG) = NUCADJ(IRG) + URTNUCX(YEAR,IRG)
           NUCADJ(MNUMNR) = NUCADJ(MNUMNR) + URTNUCX(YEAR,IRG)
        ENDDO
        URETNUU(IRG,CURIYR) = URETNUU(IRG,CURIYR) + NUCADJ(IRG)/1000.0
        URETTLU(IRG,CURIYR) = URETTLU(IRG,CURIYR) + NUCADJ(IRG)/1000.0
      ENDDO
      URETNUU(MNUMNR,CURIYR) = URETNUU(MNUMNR,CURIYR) + NUCADJ(MNUMNR)/1000.0
      URETTLU(MNUMNR,CURIYR) = URETTLU(MNUMNR,CURIYR) + NUCADJ(MNUMNR)/1000.0
      ENDIF
!
!
!  WRITE STATEMENTS FOR CHECKING
      IF (FCRL .eq. 1) THEN
      WRITE(22,101)'P2CAP ',CURIYR,UCAPP2(MNUMNR,CURIYR),(UCAPP2(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,101)'P2PLAN',CURIYR,UADDP2(1,MNUMNR,CURIYR),(UADDP2(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,101)'P2ADD ',CURIYR,UADDP2(2,MNUMNR,CURIYR),(UADDP2(2,IRG,CURIYR),IRG = 1,UNRGNS)
      ENDIF
!
      IF (PRTDBGE .GE. 4) THEN
      WRITE(22, * )
      WRITE(22,*) ' *EMMCAPO DEBUG FOR CURIYR,CURITR=',CURIYR,CURITR
      WRITE(22, * )
      WRITE(22,*) ' 1. UTILITY TOTAL CAPACITY CHECKS'
      WRITE(22, * )
      WRITE(22,100)UCAPCSU(MNUMNR,CURIYR),(UCAPCSU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPIGU(MNUMNR,CURIYR),(UCAPIGU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPOSU(MNUMNR,CURIYR),(UCAPOSU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPCCU(MNUMNR,CURIYR),(UCAPCCU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPACU(MNUMNR,CURIYR),(UCAPACU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPCTU(MNUMNR,CURIYR),(UCAPCTU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPATU(MNUMNR,CURIYR),(UCAPATU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPNUU(MNUMNR,CURIYR),(UCAPNUU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPFCU(MNUMNR,CURIYR),(UCAPFCU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPPSU(MNUMNR,CURIYR),(UCAPPSU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPRNU(MNUMNR,CURIYR),(UCAPRNU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPDBU(MNUMNR,CURIYR),(UCAPDBU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPDPU(MNUMNR,CURIYR),(UCAPDPU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPTLU(MNUMNR,CURIYR),(UCAPTLU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22, * )
      WRITE(22,*) ' 2. UTILITY PLANNED CAPACITY ADDITIONS'
      WRITE(22, * )
      WRITE(22,100)UADDCSU(1,MNUMNR,CURIYR),(UADDCSU(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDIGU(1,MNUMNR,CURIYR),(UADDIGU(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDOSU(1,MNUMNR,CURIYR),(UADDOSU(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDCCU(1,MNUMNR,CURIYR),(UADDCCU(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDACU(1,MNUMNR,CURIYR),(UADDACU(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDCTU(1,MNUMNR,CURIYR),(UADDCTU(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDATU(1,MNUMNR,CURIYR),(UADDATU(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDNUU(1,MNUMNR,CURIYR),(UADDNUU(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDFCU(1,MNUMNR,CURIYR),(UADDFCU(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDPSU(1,MNUMNR,CURIYR),(UADDPSU(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDRNU(1,MNUMNR,CURIYR),(UADDRNU(1,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDHYU(1,MNUMNR,CURIYR),(UADDHYU(1,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDGEU(1,MNUMNR,CURIYR),(UADDGEU(1,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDMSU(1,MNUMNR,CURIYR),(UADDMSU(1,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDWDU(1,MNUMNR,CURIYR),(UADDWDU(1,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDSTU(1,MNUMNR,CURIYR),(UADDSTU(1,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDPVU(1,MNUMNR,CURIYR),(UADDPVU(1,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDWNU(1,MNUMNR,CURIYR),(UADDWNU(1,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDWFU(1,MNUMNR,CURIYR),(UADDWFU(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDDBU(1,MNUMNR,CURIYR),(UADDDBU(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDDPU(1,MNUMNR,CURIYR),(UADDDPU(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDTLU(1,MNUMNR,CURIYR),(UADDTLU(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22, * )
      WRITE(22,*) ' 3. UTILITY UNPLANNED CAPACITY ADDITIONS'
      WRITE(22, * )
      WRITE(22,100)UADDCSU(2,MNUMNR,CURIYR),(UADDCSU(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDIGU(2,MNUMNR,CURIYR),(UADDIGU(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDOSU(2,MNUMNR,CURIYR),(UADDOSU(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDCCU(2,MNUMNR,CURIYR),(UADDCCU(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDACU(2,MNUMNR,CURIYR),(UADDACU(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDCTU(2,MNUMNR,CURIYR),(UADDCTU(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDATU(2,MNUMNR,CURIYR),(UADDATU(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDNUU(2,MNUMNR,CURIYR),(UADDNUU(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDFCU(2,MNUMNR,CURIYR),(UADDFCU(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDPSU(2,MNUMNR,CURIYR),(UADDPSU(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDRNU(2,MNUMNR,CURIYR),(UADDRNU(2,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDHYU(2,MNUMNR,CURIYR),(UADDHYU(2,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDGEU(2,MNUMNR,CURIYR),(UADDGEU(2,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDMSU(2,MNUMNR,CURIYR),(UADDMSU(2,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDWDU(2,MNUMNR,CURIYR),(UADDWDU(2,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDSTU(2,MNUMNR,CURIYR),(UADDSTU(2,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDPVU(2,MNUMNR,CURIYR),(UADDPVU(2,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDWNU(2,MNUMNR,CURIYR),(UADDWNU(2,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDWFU(2,MNUMNR,CURIYR),(UADDWFU(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDDBU(2,MNUMNR,CURIYR),(UADDDBU(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDDPU(2,MNUMNR,CURIYR),(UADDDPU(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDTLU(2,MNUMNR,CURIYR),(UADDTLU(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22, * )
      WRITE(22, * )
      WRITE(22,*) ' 4. UTILITY RENEWABLES TOTAL CAPACITIES'
      WRITE(22, * )
      WRITE(22,100)UCAPHYU(MNUMNR,CURIYR),(UCAPHYU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPGEU(MNUMNR,CURIYR),(UCAPGEU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPMSU(MNUMNR,CURIYR),(UCAPMSU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPWDU(MNUMNR,CURIYR),(UCAPWDU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPSTU(MNUMNR,CURIYR),(UCAPSTU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPPVU(MNUMNR,CURIYR),(UCAPPVU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPPTU(MNUMNR,CURIYR),(UCAPPTU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPWNU(MNUMNR,CURIYR),(UCAPWNU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPWLU(MNUMNR,CURIYR),(UCAPWLU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPWFU(MNUMNR,CURIYR),(UCAPWFU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPRNU(MNUMNR,CURIYR),(UCAPRNU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPDBU(MNUMNR,CURIYR),(UCAPDBU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPDPU(MNUMNR,CURIYR),(UCAPDPU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22, * )
      WRITE(22,*) ' 5. NON-UTILITY TOTAL CAPACITY CHECKS'
      WRITE(22, * )
      WRITE(22,100)UCAPCSN(MNUMNR,CURIYR),(UCAPCSN(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPIGN(MNUMNR,CURIYR),(UCAPIGN(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPOSN(MNUMNR,CURIYR),(UCAPOSN(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPCCN(MNUMNR,CURIYR),(UCAPCCN(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPACN(MNUMNR,CURIYR),(UCAPACN(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPCTN(MNUMNR,CURIYR),(UCAPCTN(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPATN(MNUMNR,CURIYR),(UCAPATN(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPNUN(MNUMNR,CURIYR),(UCAPNUN(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPFCN(MNUMNR,CURIYR),(UCAPFCN(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPPSN(MNUMNR,CURIYR),(UCAPPSN(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPRNN(MNUMNR,CURIYR),(UCAPRNN(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPDBN(MNUMNR,CURIYR),(UCAPDBN(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPDPN(MNUMNR,CURIYR),(UCAPDPN(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22, * )
      WRITE(22,*) ' 6.NON-UTILITY PLANNED CAPACITY ADDITIONS'
      WRITE(22, * )
      WRITE(22,100)UADDCSN(1,MNUMNR,CURIYR),(UADDCSN(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDIGN(1,MNUMNR,CURIYR),(UADDIGN(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDOSN(1,MNUMNR,CURIYR),(UADDOSN(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDCCN(1,MNUMNR,CURIYR),(UADDCCN(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDACN(1,MNUMNR,CURIYR),(UADDACN(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDCTN(1,MNUMNR,CURIYR),(UADDCTN(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDATN(1,MNUMNR,CURIYR),(UADDATN(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDNUN(1,MNUMNR,CURIYR),(UADDNUN(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDFCN(1,MNUMNR,CURIYR),(UADDFCN(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDPSN(1,MNUMNR,CURIYR),(UADDPSN(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDRNN(1,MNUMNR,CURIYR),(UADDRNN(1,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDHYN(1,MNUMNR,CURIYR),(UADDHYN(1,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDGEN(1,MNUMNR,CURIYR),(UADDGEN(1,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDMSN(1,MNUMNR,CURIYR),(UADDMSN(1,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDWDN(1,MNUMNR,CURIYR),(UADDWDN(1,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDSTN(1,MNUMNR,CURIYR),(UADDSTN(1,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDPVN(1,MNUMNR,CURIYR),(UADDPVN(1,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDWNN(1,MNUMNR,CURIYR),(UADDWNN(1,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDWFN(1,MNUMNR,CURIYR),(UADDWFN(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDDBN(1,MNUMNR,CURIYR),(UADDDBN(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDDPN(1,MNUMNR,CURIYR),(UADDDPN(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDTLN(1,MNUMNR,CURIYR),(UADDTLN(1,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22, * )
      WRITE(22,*) ' 7. NON-UTILITY UNPLANNED CAPACITY ADDITIONS'
      WRITE(22, * )
      WRITE(22,100)UADDCSN(2,MNUMNR,CURIYR),(UADDCSN(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDIGN(2,MNUMNR,CURIYR),(UADDIGN(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDOSN(2,MNUMNR,CURIYR),(UADDOSN(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDCCN(2,MNUMNR,CURIYR),(UADDCCN(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDACN(2,MNUMNR,CURIYR),(UADDACN(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDCTN(2,MNUMNR,CURIYR),(UADDCTN(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDATN(2,MNUMNR,CURIYR),(UADDATN(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDNUN(2,MNUMNR,CURIYR),(UADDNUN(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDFCN(2,MNUMNR,CURIYR),(UADDFCN(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDPSN(2,MNUMNR,CURIYR),(UADDPSN(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDRNN(2,MNUMNR,CURIYR),(UADDRNN(2,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDHYN(2,MNUMNR,CURIYR),(UADDHYN(2,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDGEN(2,MNUMNR,CURIYR),(UADDGEN(2,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDMSN(2,MNUMNR,CURIYR),(UADDMSN(2,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDWDN(2,MNUMNR,CURIYR),(UADDWDN(2,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDSTN(2,MNUMNR,CURIYR),(UADDSTN(2,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDPVN(2,MNUMNR,CURIYR),(UADDPVN(2,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDWNN(2,MNUMNR,CURIYR),(UADDWNN(2,IRG,CURIYR),IRG = 1,UNRGNS)
!     WRITE(22,100)UADDWFN(2,MNUMNR,CURIYR),(UADDWFN(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDDBN(2,MNUMNR,CURIYR),(UADDDBN(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDDPN(2,MNUMNR,CURIYR),(UADDDPN(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDTLN(2,MNUMNR,CURIYR),(UADDTLN(2,IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22, * )
      WRITE(22, * )
      WRITE(22,*) ' 8. NON-UTILITY RENEWABLES TOTAL CAPACITIES'
      WRITE(22, * )
      WRITE(22,100)UCAPHYN(MNUMNR,CURIYR),(UCAPHYN(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPGEN(MNUMNR,CURIYR),(UCAPGEN(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPMSN(MNUMNR,CURIYR),(UCAPMSN(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPWDN(MNUMNR,CURIYR),(UCAPWDN(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPSTN(MNUMNR,CURIYR),(UCAPSTN(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPPVN(MNUMNR,CURIYR),(UCAPPVN(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPPTN(MNUMNR,CURIYR),(UCAPPTN(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPWNN(MNUMNR,CURIYR),(UCAPWNN(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPWLN(MNUMNR,CURIYR),(UCAPWLN(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPWFN(MNUMNR,CURIYR),(UCAPWFN(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPRNN(MNUMNR,CURIYR),(UCAPRNN(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22, * )
      WRITE(22,*) ' 9. NON-TRAD COGEN EXIST+PLN CAP BY FUEL TYPE'
      WRITE(22, * )
      WRITE(22,100)CGNTCAP(MNUMNR,CURIYR,1),(CGNTCAP(IRG,CURIYR,1),IRG = 1,UNRGNS)
      WRITE(22,100)CGNTCAP(MNUMNR,CURIYR,2),(CGNTCAP(IRG,CURIYR,2),IRG = 1,UNRGNS)
      WRITE(22,100)CGNTCAP(MNUMNR,CURIYR,3),(CGNTCAP(IRG,CURIYR,3),IRG = 1,UNRGNS)
      WRITE(22,100)CGNTCAP(MNUMNR,CURIYR,4),(CGNTCAP(IRG,CURIYR,4),IRG = 1,UNRGNS)
      WRITE(22,100)CGNTCAP(MNUMNR,CURIYR,5),(CGNTCAP(IRG,CURIYR,5),IRG = 1,UNRGNS)
      WRITE(22,100)CGNTCAP(MNUMNR,CURIYR,6),(CGNTCAP(IRG,CURIYR,6),IRG = 1,UNRGNS)
      WRITE(22,100)CGNTCAP(MNUMNR,CURIYR,7),(CGNTCAP(IRG,CURIYR,7),IRG = 1,UNRGNS)
      WRITE(22,100)CGNTCAP(MNUMNR,CURIYR,8),(CGNTCAP(IRG,CURIYR,8),IRG = 1,UNRGNS)
      WRITE(22,100)CGNTCAP(MNUMNR,CURIYR,9),(CGNTCAP(IRG,CURIYR,9),IRG = 1,UNRGNS)
      WRITE(22,100)CGNTCAP(MNUMNR,CURIYR,10),(CGNTCAP(IRG,CURIYR,10),IRG = 1,UNRGNS)
      WRITE(22, * )
      WRITE(22,*) ' 10. NON-TRADITIONAL COGEN CAPACITY'
      WRITE(22, * )
      WRITE(22,100)UCAPCSC(MNUMNR,CURIYR),(UCAPCSC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPIGC(MNUMNR,CURIYR),(UCAPIGC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPOSC(MNUMNR,CURIYR),(UCAPOSC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPCCC(MNUMNR,CURIYR),(UCAPCCC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPACC(MNUMNR,CURIYR),(UCAPACC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPCTC(MNUMNR,CURIYR),(UCAPCTC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPATC(MNUMNR,CURIYR),(UCAPATC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPHYC(MNUMNR,CURIYR),(UCAPHYC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPGEC(MNUMNR,CURIYR),(UCAPGEC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPMSC(MNUMNR,CURIYR),(UCAPMSC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPWDC(MNUMNR,CURIYR),(UCAPWDC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPSTC(MNUMNR,CURIYR),(UCAPSTC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPPVC(MNUMNR,CURIYR),(UCAPPVC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPPTC(MNUMNR,CURIYR),(UCAPPTC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPWNC(MNUMNR,CURIYR),(UCAPWNC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPWLC(MNUMNR,CURIYR),(UCAPWLC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPWFC(MNUMNR,CURIYR),(UCAPWFC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UCAPTLC(MNUMNR,CURIYR),(UCAPTLC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22, * )
      WRITE(22,*) ' 10. NON-TRADITIONAL COGEN PLANNED CAPACITY'
      WRITE(22, * )
      WRITE(22,100)UADDCSC(MNUMNR,CURIYR),(UCAPCSC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDIGC(MNUMNR,CURIYR),(UCAPIGC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDOSC(MNUMNR,CURIYR),(UCAPOSC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDCCC(MNUMNR,CURIYR),(UCAPCCC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDACC(MNUMNR,CURIYR),(UCAPACC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDCTC(MNUMNR,CURIYR),(UCAPCTC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDATC(MNUMNR,CURIYR),(UCAPATC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDHYC(MNUMNR,CURIYR),(UCAPHYC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDGEC(MNUMNR,CURIYR),(UCAPGEC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDMSC(MNUMNR,CURIYR),(UCAPMSC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDWDC(MNUMNR,CURIYR),(UCAPWDC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDSTC(MNUMNR,CURIYR),(UCAPSTC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDPVC(MNUMNR,CURIYR),(UCAPPVC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDPTC(MNUMNR,CURIYR),(UCAPPTC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDWNC(MNUMNR,CURIYR),(UCAPWNC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDWLC(MNUMNR,CURIYR),(UCAPWLC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDWFC(MNUMNR,CURIYR),(UCAPWFC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)UADDTLC(MNUMNR,CURIYR),(UCAPTLC(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22, * )
      WRITE(22,*) ' 11. TOTAL ECP CAP ADDITIONS BY CLT FOR CURIYR'
      WRITE(22, * )
      DO YYRR = CURIYR, CURIYR + 5
         WRITE(22,100)UADDPST(MNUMNR,YYRR),(UADDPST(IRG,YYRR),IRG = 1,UNRGNS)
      ENDDO
      DO YYRR = CURIYR, CURIYR + 5
         WRITE(22,100)UADDHYT(MNUMNR,YYRR),(UADDHYT(IRG,YYRR),IRG = 1,UNRGNS)
      ENDDO
      DO YYRR = CURIYR, CURIYR + 5
         WRITE(22,100)UADDGET(MNUMNR,YYRR),(UADDGET(IRG,YYRR),IRG = 1,UNRGNS)
      ENDDO
      DO YYRR = CURIYR, CURIYR + 5
         WRITE(22,100)UADDMST(MNUMNR,YYRR),(UADDMST(IRG,YYRR),IRG = 1,UNRGNS)
      ENDDO
      DO YYRR = CURIYR, CURIYR + 5
         WRITE(22,100)UADDWDT(MNUMNR,YYRR),(UADDWDT(IRG,YYRR),IRG = 1,UNRGNS)
      ENDDO
      DO YYRR = CURIYR, CURIYR + 5
         WRITE(22,100)UADDSTT(MNUMNR,YYRR),(UADDSTT(IRG,YYRR),IRG = 1,UNRGNS)
      ENDDO
      DO YYRR = CURIYR, CURIYR + 5
         WRITE(22,100)UADDPVT(MNUMNR,YYRR),(UADDPVT(IRG,YYRR),IRG = 1,UNRGNS)
      ENDDO
      DO YYRR = CURIYR, CURIYR + 5
         WRITE(22,100)UADDPTT(MNUMNR,YYRR),(UADDPTT(IRG,YYRR),IRG = 1,UNRGNS)
      ENDDO
      DO YYRR = CURIYR, CURIYR + 5
         WRITE(22,100)UADDWNT(MNUMNR,YYRR),(UADDWNT(IRG,YYRR),IRG = 1,UNRGNS)
      ENDDO
      DO YYRR = CURIYR, CURIYR + 5
         WRITE(22,100)UADDWLT(MNUMNR,YYRR),(UADDWLT(IRG,YYRR),IRG = 1,UNRGNS)
      ENDDO
      DO YYRR = CURIYR, CURIYR + 5
         WRITE(22,100)UADDWFT(MNUMNR,YYRR),(UADDWFT(IRG,YYRR),IRG = 1,UNRGNS)
      ENDDO
      WRITE(22, * )
      WRITE(22,*) ' 12. TOTAL UTILITY RETIREMENTS'
      WRITE(22, * )
      WRITE(22,100)URETTLU(MNUMNR,CURIYR),(URETTLU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22, * )
      WRITE(22,*) ' 13. RETIREMENT CHECKS '
      WRITE(22, * )
      WRITE(22,100)URETCSU(MNUMNR,CURIYR),(URETCSU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)URETIGU(MNUMNR,CURIYR),(URETIGU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)URETOSU(MNUMNR,CURIYR),(URETOSU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)URETCCU(MNUMNR,CURIYR),(URETCCU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)URETACU(MNUMNR,CURIYR),(URETACU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)URETCTU(MNUMNR,CURIYR),(URETCTU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)URETATU(MNUMNR,CURIYR),(URETATU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)URETNUU(MNUMNR,CURIYR),(URETNUU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)URETFCU(MNUMNR,CURIYR),(URETFCU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)URETPSU(MNUMNR,CURIYR),(URETPSU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)URETHYU(MNUMNR,CURIYR),(URETHYU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)URETGEU(MNUMNR,CURIYR),(URETGEU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)URETMSU(MNUMNR,CURIYR),(URETMSU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)URETWDU(MNUMNR,CURIYR),(URETWDU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)URETSTU(MNUMNR,CURIYR),(URETSTU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)URETPVU(MNUMNR,CURIYR),(URETPVU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)URETPTU(MNUMNR,CURIYR),(URETPTU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)URETWNU(MNUMNR,CURIYR),(URETWNU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)URETWLU(MNUMNR,CURIYR),(URETWLU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)URETWFU(MNUMNR,CURIYR),(URETWFU(IRG,CURIYR),IRG = 1,UNRGNS)
      WRITE(22,100)URETRNU(MNUMNR,CURIYR),(URETRNU(IRG,CURIYR),IRG = 1,UNRGNS)
      END IF
 100  FORMAT(2X,<MNUMNR-2>(F9.3))
 101  FORMAT(2X,A8,I4,<MNUMNR-2>(F9.3))
!
      RETURN
      END
!
      SUBROUTINE EMMGENO
!
      IMPLICIT NONE
!
!     THIS SUBROUTINE STORES EMM NERC REGION GENERATION BY PLANT TYPE IN NEMS GLOBAL ARRAYS
!     IT ALSO COMPUTES BIOMASS PRICES BY CENSUS REGIONS

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'ecpcntl'
      include 'dispuse'
      include 'dispout'
      include 'wrenew'
      include 'uefdout'     ! EFD output variables
      include 'udatout'
      include 'dispinyr'
!
      INTEGER IRG,PLT,OWN,TYP,IYR

      REAL PGEN(EFD_D_CAP + 1,MNUMNR,MNUMYR)
      REAL CGEN(MNUMNR,MNUMYR)
      REAL SGEN(MNUMNR,MNUMYR)
      REAL TGEN(MNUMNR,MNUMYR)
      REAL YGEN(MNUMNR,MNUMYR)
      REAL NGEN(MNUMNR,MNUMYR)
      REAL WGEN(MNUMNR,MNUMYR)
      REAL DGEN(MNUMNR,MNUMYR)

      CHARACTER*15 PLTNAME(EFD_D_CAP)

      DATA PLTNAME/'Coal Stm < 1965',  &
                   'Coal Stm > 1965',  &
                   'Coal Stm W/Scr ',  &
                   'New PC Steam   ',  &
                   'Adv Coal       ',  &
                   'New Adv Coal   ',  &
                   'IGCC w/NG Cof  ',  &
                   'Adv Coal w/pCCS',  &
                   'Adv Coal w/CCS ',  &
                   'Coal to NG Retr',  &
                   'Oil Stm        ',  &
                   'Gas Stm        ',  &
                   'Dual Stm       ',  &
                   'Int Comb Eng   ',  &
                   'Oil CT         ',  &
                   'Gas CT         ',  &
                   'Dual CT        ',  &
                   'Adv CT         ',  &
                   'New Adv CT     ',  &
                   'Oil CC         ',  &
                   'Gas CC         ',  &
                   'Dual CC        ',  &
                   'Adv CC         ',  &
                   'New Adv CC     ',  &
                   'Adv CC W/CCS   ',  &
                   'Fuel Cell      ',  &
                   'Existing Nuc   ',  &
                   'Adv Nuc        ',  &
                   'SMR Nuc        ',  &
                   'Grn Fld Nuc    ',  &
                   'Biomass        ',  &
                   'Biomass IGCC   ',  &
                   'Geothermal     ',  &
                   'Adv Geothermal ',  &
                   'MSW            ',  &
                   'Hydro          ',  &
                   'Adv Hydro      ',  &
                   'Hydro in Strm  ',  &
                   'Tidal Hydro    ',  &
                   'Pump Storage   ',  &
                   'Quick Resp Stor',  &
                   'Diurnal Stor   ',  &
                   'Other Storage  ',  &
                   'Wind           ',  &
                   'Wind Low Speed ',  &
                   'Wind OffShore  ',  &
                   'Solar Thermal  ',  &
                   'So Thrm w/Stor ',  &
                   'So Thrm w/Stor2',  &
                   'Solar PV       ',  &
                   'So PV w/Axis Tr',  &
                   'Other Intermitt',  &
                   'DG-Base        ',  &
                   'DG-Peak        '/

!                  '  Total Coal   '
!                  '  Total O/G Stm'
!                  '  Total O/G CT '
!                  '  Total O/G CC '
!                  '  Total Nuclear'
!                  '  Total Hydro  '
!                  '  Total Wind   '
!                  '  Total DG     '
!                  '  Total Gen    '

! INITIALIZE
      DO PLT = 1 , EFD_D_CAP + 1
         DO IRG = 1 , MNUMNR
            PGEN(PLT,IRG,CURIYR) = 0.0
         END DO
      END DO
      DO IRG = 1 , MNUMNR
         CGEN(IRG,CURIYR) = 0.0
         SGEN(IRG,CURIYR) = 0.0
         TGEN(IRG,CURIYR) = 0.0
         YGEN(IRG,CURIYR) = 0.0
         NGEN(IRG,CURIYR) = 0.0
         WGEN(IRG,CURIYR) = 0.0
         DGEN(IRG,CURIYR) = 0.0
         UGENPC(IRG,CURIYR) = 0.0
         UGENPQ(IRG,CURIYR) = 0.0
         UGENPQ_ALT(IRG,CURIYR) = 0.0
         UGENSQ(IRG,CURIYR) = 0.0
         UGENSQ_ALT(IRG,CURIYR) = 0.0
         UGENIG(IRG,CURIYR) = 0.0
         UGENIS(IRG,CURIYR) = 0.0
         UGENIS_ALT(IRG,CURIYR) = 0.0
         UGENOS(IRG,CURIYR) = 0.0
         UGENCC(IRG,CURIYR) = 0.0
         UGENAC(IRG,CURIYR) = 0.0
         UGENA2(IRG,CURIYR) = 0.0
         UGENA2_ALT(IRG,CURIYR) = 0.0
         UGENCS(IRG,CURIYR) = 0.0
         UGENCS_ALT(IRG,CURIYR) = 0.0
         UGENIC(IRG,CURIYR) = 0.0
         UGENCT(IRG,CURIYR) = 0.0
         UGENT2(IRG,CURIYR) = 0.0
         UGENAT(IRG,CURIYR) = 0.0
         UGENNU(IRG,CURIYR) = 0.0
         UGENSM(IRG,CURIYR) = 0.0
         UGENPS(IRG,CURIYR) = 0.0
         UGENDS(IRG,CURIYR) = 0.0
         UGENFC(IRG,CURIYR) = 0.0
         UGENRN(IRG,CURIYR) = 0.0
         UGENDG(IRG,CURIYR) = 0.0
      END DO

      DO IRG = 1 , MNUMNR
       IF (IRG .LE. UNRGNS)THEN
! GET REGIONAL DISPATCH RESULTS AND INPUTS
        CALL GETOUT(CURIYR,IRG)
! LOOP OVER PLANT AND OWNER TYPES
        DO PLT = 1 , EFD_D_CAP
           DO OWN = 1 , EFD_D_OWN - 1
! DISPATCHABLE
              IF (PLT .LE. EFD_D_DSP)THEN
                 TYP = PLT
                 PGEN(PLT,IRG,CURIYR) = PGEN(PLT,IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                 PGEN(EFD_D_CAP + 1,IRG,CURIYR) = PGEN(EFD_D_CAP + 1,IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                 PGEN(PLT,MNUMNR,CURIYR) = PGEN(PLT,MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                 PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) = PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                 IF (PLT .EQ. UICOU .OR. PLT .EQ. UICSU .OR. PLT .EQ. UICSC .OR.  &
                     PLT .EQ. UICNC .OR. PLT .EQ. UICAV .OR. PLT .EQ. UICOQ .OR. PLT .EQ. UICAS)THEN
                    CGEN(IRG,CURIYR) = CGEN(IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    CGEN(MNUMNR,CURIYR) = CGEN(MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    IF (PLT .EQ. UICAV) THEN
                      UGENIG(IRG,CURIYR) = UGENIG(IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                      UGENIG(MNUMNR,CURIYR) = UGENIG(MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    ELSEIF (PLT .EQ. UICAS) THEN
                      UGENIS(IRG,CURIYR) = UGENIS(IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                      UGENIS(MNUMNR,CURIYR) = UGENIS(MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                      UGENIS_ALT(IRG,CURIYR) = UGENIS_ALT(IRG,CURIYR) + EQPGN_ALT(TYP,OWN) * 0.001
                      UGENIS_ALT(MNUMNR,CURIYR) = UGENIS_ALT(MNUMNR,CURIYR) + EQPGN_ALT(TYP,OWN) * 0.001
                    ELSEIF (PLT .EQ. UICOQ) THEN
                      UGENPQ(IRG,CURIYR) = UGENPQ(IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                      UGENPQ(MNUMNR,CURIYR) = UGENPQ(MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                      UGENPQ_ALT(IRG,CURIYR) = UGENPQ_ALT(IRG,CURIYR) + EQPGN_ALT(TYP,OWN) * 0.001
                      UGENPQ_ALT(MNUMNR,CURIYR) = UGENPQ_ALT(MNUMNR,CURIYR) + EQPGN_ALT(TYP,OWN) * 0.001                      
                    ELSE
                      UGENPC(IRG,CURIYR) = UGENPC(IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                      UGENPC(MNUMNR,CURIYR) = UGENPC(MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    ENDIF
                 ELSE IF (PLT .EQ. UISTO .OR. PLT .EQ. UISTG .OR. PLT .EQ. UISTX)THEN
                    SGEN(IRG,CURIYR) = SGEN(IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    SGEN(MNUMNR,CURIYR) = SGEN(MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    UGENOS(IRG,CURIYR) = UGENOS(IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    UGENOS(MNUMNR,CURIYR) = UGENOS(MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                 ELSE IF (PLT .EQ. UICTO .OR. PLT .EQ. UICTG .OR. PLT .EQ. UICTX .OR. PLT .EQ. UIACT .OR.  &
                          PLT .EQ. UIICE .OR. PLT .EQ. UICTA) THEN
                    TGEN(IRG,CURIYR) = TGEN(IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    TGEN(MNUMNR,CURIYR) = TGEN(MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    IF (PLT .EQ. UIACT) THEN
                      UGENAT(IRG,CURIYR) = UGENAT(IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                      UGENAT(MNUMNR,CURIYR) = UGENAT(MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    ELSE IF (PLT .EQ. UIICE) THEN
                      UGENIC(IRG,CURIYR) = UGENIC(IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                      UGENIC(MNUMNR,CURIYR) = UGENIC(MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    ELSE IF (PLT .EQ. UICTA) THEN
                      UGENT2(IRG,CURIYR) = UGENT2(IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                      UGENT2(MNUMNR,CURIYR) = UGENT2(MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    ELSE
                      UGENCT(IRG,CURIYR) = UGENCT(IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                      UGENCT(MNUMNR,CURIYR) = UGENCT(MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    ENDIF
                 ELSE IF (PLT .EQ. UICCO .OR. PLT .EQ. UICCG .OR. PLT .EQ. UICCX .OR.  &
                     PLT .EQ. UIACC .OR. PLT .EQ. UIAC2 .OR. PLT .EQ. UIACS)THEN
                    YGEN(IRG,CURIYR) = YGEN(IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    YGEN(MNUMNR,CURIYR) = YGEN(MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    IF (PLT .EQ. UIACC) THEN
                      UGENAC(IRG,CURIYR) = UGENAC(IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                      UGENAC(MNUMNR,CURIYR) = UGENAC(MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    ELSEIF (PLT .EQ. UIAC2) THEN
                      UGENA2(IRG,CURIYR) = UGENA2(IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                      UGENA2(MNUMNR,CURIYR) = UGENA2(MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                      UGENA2_ALT(IRG,CURIYR) = UGENA2_ALT(IRG,CURIYR) + EQPGN_ALT(TYP,OWN) * 0.001
                      UGENA2_ALT(MNUMNR,CURIYR) = UGENA2_ALT(MNUMNR,CURIYR) + EQPGN_ALT(TYP,OWN) * 0.001
                   ELSEIF (PLT .EQ. UIACS) THEN
                      UGENCS(IRG,CURIYR) = UGENCS(IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                      UGENCS(MNUMNR,CURIYR) = UGENCS(MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                      UGENCS_ALT(IRG,CURIYR) = UGENCS_ALT(IRG,CURIYR) + EQPGN_ALT(TYP,OWN) * 0.001
                      UGENCS_ALT(MNUMNR,CURIYR) = UGENCS_ALT(MNUMNR,CURIYR) + EQPGN_ALT(TYP,OWN) * 0.001
                    ELSE
                      UGENCC(IRG,CURIYR) = UGENCC(IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                      UGENCC(MNUMNR,CURIYR) = UGENCC(MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    ENDIF
                 ELSE IF (PLT .EQ. UICNU .OR. PLT .EQ. UIANC)THEN
                    NGEN(IRG,CURIYR) = NGEN(IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    NGEN(MNUMNR,CURIYR) = NGEN(MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    UGENNU(IRG,CURIYR) = UGENNU(IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    UGENNU(MNUMNR,CURIYR) = UGENNU(MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                ELSE IF (PLT .EQ. UISMR)THEN
                    NGEN(IRG,CURIYR) = NGEN(IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    NGEN(MNUMNR,CURIYR) = NGEN(MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    UGENSM(IRG,CURIYR) = UGENSM(IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    UGENSM(MNUMNR,CURIYR) = UGENSM(MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                 ELSE IF (PLT .EQ. UIFCG) THEN
                    UGENFC(IRG,CURIYR) = UGENFC(IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    UGENFC(MNUMNR,CURIYR) = UGENFC(MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                 ELSE IF ((PLT .EQ. UIGTH) .OR. (PLT .EQ. UIBMS)) THEN
                    UGENRN(IRG,CURIYR) = UGENRN(IRG,CURIYR) + EQPGN(TYP,OWN) * 0.001
                    UGENRN(MNUMNR,CURIYR) = UGENRN(MNUMNR,CURIYR) + EQPGN(TYP,OWN) * 0.001
                 END IF
! RENEWABLE
              ELSE IF (PLT .LE. EFD_D_DSP + EFD_D_RNW)THEN
                 TYP = PLT - EFD_D_DSP
                 PGEN(PLT,IRG,CURIYR) = PGEN(PLT,IRG,CURIYR) + EQHGN(TYP,OWN) * 0.001
                 PGEN(EFD_D_CAP + 1,IRG,CURIYR) = PGEN(EFD_D_CAP + 1,IRG,CURIYR) + EQHGN(TYP,OWN) * 0.001
                 PGEN(PLT,MNUMNR,CURIYR) = PGEN(PLT,MNUMNR,CURIYR) + EQHGN(TYP,OWN) * 0.001
                 PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) = PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) + EQHGN(TYP,OWN) * 0.001
                 IF (PLT .EQ. UIWND .OR. PLT .EQ. UIWNL .OR. PLT .EQ. UIWFS)THEN
                    WGEN(IRG,CURIYR) = WGEN(IRG,CURIYR) + EQHGN(TYP,OWN) * 0.001
                    WGEN(MNUMNR,CURIYR) = WGEN(MNUMNR,CURIYR) + EQHGN(TYP,OWN) * 0.001
                 END IF
!             MSW -- EXCLUDES NONBIOGENIC MSW
                 IF (PLT .EQ. UIMSW) THEN
                    IF (OWN .EQ. 1)THEN
                       PGEN(PLT,IRG,CURIYR) = PGEN(PLT,IRG,CURIYR) - WNGMSEL(CURIYR,IRG)
                       PGEN(EFD_D_CAP + 1,IRG,CURIYR) = PGEN(EFD_D_CAP + 1,IRG,CURIYR) - WNGMSEL(CURIYR,IRG)
                       PGEN(PLT,MNUMNR,CURIYR) = PGEN(PLT,MNUMNR,CURIYR) - WNGMSEL(CURIYR,IRG)
                       PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) = PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) - WNGMSEL(CURIYR,IRG)
                       UGENRN(IRG,CURIYR) = UGENRN(IRG,CURIYR) - WNGMSEL(CURIYR,IRG)
                       UGENRN(MNUMNR,CURIYR) = UGENRN(MNUMNR,CURIYR) - WNGMSEL(CURIYR,IRG)
                    END IF
                 END IF
!             PUMP STORAGE / OTHER INCLUDES NONBIOGENIC MSW
                 IF (PLT .EQ. UIHYR) THEN
                    IF (OWN .EQ. 1)THEN
                       PGEN(PLT,IRG,CURIYR) = PGEN(PLT,IRG,CURIYR) + WNGMSEL(CURIYR,IRG)
                       PGEN(EFD_D_CAP + 1,IRG,CURIYR) = PGEN(EFD_D_CAP + 1,IRG,CURIYR) + WNGMSEL(CURIYR,IRG)
                       PGEN(PLT,MNUMNR,CURIYR) = PGEN(PLT,MNUMNR,CURIYR) + WNGMSEL(CURIYR,IRG)
                       PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) = PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) + WNGMSEL(CURIYR,IRG)
                       UGENPS(IRG,CURIYR) = UGENPS(IRG,CURIYR) + WNGMSEL(CURIYR,IRG)
                       UGENPS(MNUMNR,CURIYR) = UGENPS(MNUMNR,CURIYR) + WNGMSEL(CURIYR,IRG)
                    END IF
                    UGENPS(IRG,CURIYR) = UGENPS(IRG,CURIYR) + EQHGN(TYP,OWN) * 0.001
                    UGENPS(MNUMNR,CURIYR) = UGENPS(MNUMNR,CURIYR) + EQHGN(TYP,OWN) * 0.001
!             DIURNAL STORAGE
                 ELSE IF (PLT .EQ. UIDST) THEN
                    UGENDS(IRG,CURIYR) = UGENDS(IRG,CURIYR) + EQHGN(TYP,OWN) * 0.001
                    UGENDS(MNUMNR,CURIYR) = UGENDS(MNUMNR,CURIYR) + EQHGN(TYP,OWN) * 0.001
                 ELSE
                    UGENRN(IRG,CURIYR) = UGENRN(IRG,CURIYR) + EQHGN(TYP,OWN) * 0.001
                    UGENRN(MNUMNR,CURIYR) = UGENRN(MNUMNR,CURIYR) + EQHGN(TYP,OWN) * 0.001
                 ENDIF
              ELSE
                 TYP = PLT - EFD_D_DSP - EFD_D_RNW
                 PGEN(PLT,IRG,CURIYR) = PGEN(PLT,IRG,CURIYR) + EQDGN(TYP,OWN) * 0.001
                 PGEN(EFD_D_CAP + 1,IRG,CURIYR) = PGEN(EFD_D_CAP + 1,IRG,CURIYR) + EQDGN(TYP,OWN) * 0.001
                 PGEN(PLT,MNUMNR,CURIYR) = PGEN(PLT,MNUMNR,CURIYR) + EQDGN(TYP,OWN) * 0.001
                 PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) = PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) + EQDGN(TYP,OWN) * 0.001
                 DGEN(IRG,CURIYR) = DGEN(IRG,CURIYR) + EQDGN(TYP,OWN) * 0.001
                 DGEN(MNUMNR,CURIYR) = DGEN(MNUMNR,CURIYR) + EQDGN(TYP,OWN) * 0.001
                 UGENDG(IRG,CURIYR) = UGENDG(IRG,CURIYR) + EQDGN(TYP,OWN) * 0.001
                 UGENDG(MNUMNR,CURIYR) = UGENDG(MNUMNR,CURIYR) + EQDGN(TYP,OWN) * 0.001
              END IF
           END DO
        END DO
!       GENERATION FROM PLANTS RETROFITTED WITH CCS
        UGENSQ(IRG,CURIYR) = UGENSQ(IRG,CURIYR) + EQPGNSQ * 0.001
        UGENSQ(MNUMNR,CURIYR) = UGENSQ(MNUMNR,CURIYR) + EQPGNSQ * 0.001
        UGENSQ_ALT(IRG,CURIYR) = UGENSQ_ALT(IRG,CURIYR) + EQPGNSQ_ALT * 0.001
        UGENSQ_ALT(MNUMNR,CURIYR) = UGENSQ_ALT(MNUMNR,CURIYR) + EQPGNSQ_ALT * 0.001
       END IF
! ADD IN ALASKA AND HAWAII
       IF (IRG .EQ. (MNUMNR - 1) .OR. IRG .EQ. (MNUMNR - 2))THEN
          DO OWN = 1 , 2
! COAL
             PLT = UICSU
             PGEN(PLT,IRG,CURIYR) = PGEN(PLT,IRG,CURIYR) + UGNCLNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,IRG,CURIYR) = PGEN(EFD_D_CAP + 1,IRG,CURIYR) + UGNCLNR(OWN,IRG,CURIYR)
             PGEN(PLT,MNUMNR,CURIYR) = PGEN(PLT,MNUMNR,CURIYR) + UGNCLNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) = PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) + UGNCLNR(OWN,IRG,CURIYR)
             CGEN(IRG,CURIYR) = CGEN(IRG,CURIYR) + UGNCLNR(OWN,IRG,CURIYR)
             CGEN(MNUMNR,CURIYR) = CGEN(MNUMNR,CURIYR) + UGNCLNR(OWN,IRG,CURIYR)
             UGENPC(IRG,CURIYR) = UGENPC(IRG,CURIYR) + UGNCLNR(OWN,IRG,CURIYR)
             UGENPC(MNUMNR,CURIYR) = UGENPC(MNUMNR,CURIYR) + UGNCLNR(OWN,IRG,CURIYR)
! OIL
             PLT = UISTO
             PGEN(PLT,IRG,CURIYR) = PGEN(PLT,IRG,CURIYR) + UGNRLNR(OWN,IRG,CURIYR) + UGNRHNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,IRG,CURIYR) = PGEN(EFD_D_CAP + 1,IRG,CURIYR) + UGNRLNR(OWN,IRG,CURIYR) + UGNRHNR(OWN,IRG,CURIYR)
             PGEN(PLT,MNUMNR,CURIYR) = PGEN(PLT,MNUMNR,CURIYR) + UGNRLNR(OWN,IRG,CURIYR) + UGNRHNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) = PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) + UGNRLNR(OWN,IRG,CURIYR) + UGNRHNR(OWN,IRG,CURIYR)
             SGEN(IRG,CURIYR) = SGEN(IRG,CURIYR) + UGNRLNR(OWN,IRG,CURIYR) + UGNRHNR(OWN,IRG,CURIYR)
             SGEN(MNUMNR,CURIYR) = SGEN(MNUMNR,CURIYR) + UGNRLNR(OWN,IRG,CURIYR) + UGNRHNR(OWN,IRG,CURIYR)
             UGENOS(IRG,CURIYR) = UGENOS(IRG,CURIYR) + UGNRLNR(OWN,IRG,CURIYR) + UGNRHNR(OWN,IRG,CURIYR)
             UGENOS(MNUMNR,CURIYR) = UGENOS(MNUMNR,CURIYR) + UGNRLNR(OWN,IRG,CURIYR) + UGNRHNR(OWN,IRG,CURIYR)
             PLT = UICTO
             PGEN(PLT,IRG,CURIYR) = PGEN(PLT,IRG,CURIYR) + UGNDSNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,IRG,CURIYR) = PGEN(EFD_D_CAP + 1,IRG,CURIYR) + UGNDSNR(OWN,IRG,CURIYR)
             PGEN(PLT,MNUMNR,CURIYR) = PGEN(PLT,MNUMNR,CURIYR) + UGNDSNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) = PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) + UGNDSNR(OWN,IRG,CURIYR)
             TGEN(IRG,CURIYR) = TGEN(IRG,CURIYR) + UGNDSNR(OWN,IRG,CURIYR)
             TGEN(MNUMNR,CURIYR) = TGEN(MNUMNR,CURIYR) + UGNDSNR(OWN,IRG,CURIYR)
             UGENCT(IRG,CURIYR) = UGENCT(IRG,CURIYR) + UGNDSNR(OWN,IRG,CURIYR)
             UGENCT(MNUMNR,CURIYR) = UGENCT(MNUMNR,CURIYR) + UGNDSNR(OWN,IRG,CURIYR)
! GAS
             PLT = UISTG
             PGEN(PLT,IRG,CURIYR) = PGEN(PLT,IRG,CURIYR) + UGNGFNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,IRG,CURIYR) = PGEN(EFD_D_CAP + 1,IRG,CURIYR) + UGNGFNR(OWN,IRG,CURIYR)
             PGEN(PLT,MNUMNR,CURIYR) = PGEN(PLT,MNUMNR,CURIYR) + UGNGFNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) = PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) + UGNGFNR(OWN,IRG,CURIYR)
             SGEN(IRG,CURIYR) = SGEN(IRG,CURIYR) + UGNGFNR(OWN,IRG,CURIYR)
             SGEN(MNUMNR,CURIYR) = SGEN(MNUMNR,CURIYR) + UGNGFNR(OWN,IRG,CURIYR)
             UGENOS(IRG,CURIYR) = UGENOS(IRG,CURIYR) + UGNGFNR(OWN,IRG,CURIYR)
             UGENOS(MNUMNR,CURIYR) = UGENOS(MNUMNR,CURIYR) + UGNGFNR(OWN,IRG,CURIYR)
             PLT = UICTG
             PGEN(PLT,IRG,CURIYR) = PGEN(PLT,IRG,CURIYR) + UGNGINR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,IRG,CURIYR) = PGEN(EFD_D_CAP + 1,IRG,CURIYR) + UGNGINR(OWN,IRG,CURIYR)
             PGEN(PLT,MNUMNR,CURIYR) = PGEN(PLT,MNUMNR,CURIYR) + UGNGINR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) = PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) + UGNGINR(OWN,IRG,CURIYR)
             TGEN(IRG,CURIYR) = TGEN(IRG,CURIYR) + UGNGINR(OWN,IRG,CURIYR)
             TGEN(MNUMNR,CURIYR) = TGEN(MNUMNR,CURIYR) + UGNGINR(OWN,IRG,CURIYR)
             UGENCT(IRG,CURIYR) = UGENCT(IRG,CURIYR) + UGNGINR(OWN,IRG,CURIYR)
             UGENCT(MNUMNR,CURIYR) = UGENCT(MNUMNR,CURIYR) + UGNGINR(OWN,IRG,CURIYR)
! HYDRO
             PLT = UIHYC
             PGEN(PLT,IRG,CURIYR) = PGEN(PLT,IRG,CURIYR) + UGNHYNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,IRG,CURIYR) = PGEN(EFD_D_CAP + 1,IRG,CURIYR) + UGNHYNR(OWN,IRG,CURIYR)
             PGEN(PLT,MNUMNR,CURIYR) = PGEN(PLT,MNUMNR,CURIYR) + UGNHYNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) = PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) + UGNHYNR(OWN,IRG,CURIYR)
             UGENRN(IRG,CURIYR) = UGENRN(IRG,CURIYR) + UGNHYNR(OWN,IRG,CURIYR)
             UGENRN(MNUMNR,CURIYR) = UGENRN(MNUMNR,CURIYR) + UGNHYNR(OWN,IRG,CURIYR)
! PUMP STORAGE
             PLT = UIHYR
             PGEN(PLT,IRG,CURIYR) = PGEN(PLT,IRG,CURIYR) + UGNPSNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,IRG,CURIYR) = PGEN(EFD_D_CAP + 1,IRG,CURIYR) + UGNPSNR(OWN,IRG,CURIYR)
             PGEN(PLT,MNUMNR,CURIYR) = PGEN(PLT,MNUMNR,CURIYR) + UGNPSNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) = PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) + UGNPSNR(OWN,IRG,CURIYR)
             UGENPS(IRG,CURIYR) = UGENPS(IRG,CURIYR) + UGNPSNR(OWN,IRG,CURIYR)
             UGENPS(MNUMNR,CURIYR) = UGENPS(MNUMNR,CURIYR) + UGNPSNR(OWN,IRG,CURIYR)
! DIURNAL STORAGE
             PLT = UIDST
             PGEN(PLT,IRG,CURIYR) = PGEN(PLT,IRG,CURIYR) + UGNSDNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,IRG,CURIYR) = PGEN(EFD_D_CAP + 1,IRG,CURIYR) + UGNSDNR(OWN,IRG,CURIYR)
             PGEN(PLT,MNUMNR,CURIYR) = PGEN(PLT,MNUMNR,CURIYR) + UGNSDNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) = PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) + UGNSDNR(OWN,IRG,CURIYR)
             UGENDS(IRG,CURIYR) = UGENDS(IRG,CURIYR) + UGNSDNR(OWN,IRG,CURIYR)
             UGENDS(MNUMNR,CURIYR) = UGENDS(MNUMNR,CURIYR) + UGNSDNR(OWN,IRG,CURIYR)
! GEOTHERMAL
             PLT = UIGTH
             PGEN(PLT,IRG,CURIYR) = PGEN(PLT,IRG,CURIYR) + UGNGENR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,IRG,CURIYR) = PGEN(EFD_D_CAP + 1,IRG,CURIYR) + UGNGENR(OWN,IRG,CURIYR)
             PGEN(PLT,MNUMNR,CURIYR) = PGEN(PLT,MNUMNR,CURIYR) + UGNGENR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) = PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) + UGNGENR(OWN,IRG,CURIYR)
             UGENRN(IRG,CURIYR) = UGENRN(IRG,CURIYR) + UGNGENR(OWN,IRG,CURIYR)
             UGENRN(MNUMNR,CURIYR) = UGENRN(MNUMNR,CURIYR) + UGNGENR(OWN,IRG,CURIYR)
! MSW
             PLT = UIMSW
             PGEN(PLT,IRG,CURIYR) = PGEN(PLT,IRG,CURIYR) + UGNMSNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,IRG,CURIYR) = PGEN(EFD_D_CAP + 1,IRG,CURIYR) + UGNMSNR(OWN,IRG,CURIYR)
             PGEN(PLT,MNUMNR,CURIYR) = PGEN(PLT,MNUMNR,CURIYR) + UGNMSNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) = PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) + UGNMSNR(OWN,IRG,CURIYR)
             UGENRN(IRG,CURIYR) = UGENRN(IRG,CURIYR) + UGNMSNR(OWN,IRG,CURIYR)
             UGENRN(MNUMNR,CURIYR) = UGENRN(MNUMNR,CURIYR) + UGNMSNR(OWN,IRG,CURIYR)
! BIOMASS
             PLT = UIBMS
             PGEN(PLT,IRG,CURIYR) = PGEN(PLT,IRG,CURIYR) + UGNWDNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,IRG,CURIYR) = PGEN(EFD_D_CAP + 1,IRG,CURIYR) + UGNWDNR(OWN,IRG,CURIYR)
             PGEN(PLT,MNUMNR,CURIYR) = PGEN(PLT,MNUMNR,CURIYR) + UGNWDNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) = PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) + UGNWDNR(OWN,IRG,CURIYR)
             UGENRN(IRG,CURIYR) = UGENRN(IRG,CURIYR) + UGNWDNR(OWN,IRG,CURIYR)
             UGENRN(MNUMNR,CURIYR) = UGENRN(MNUMNR,CURIYR) + UGNWDNR(OWN,IRG,CURIYR)
! SOLAR THERMAL
             PLT = UISTH
             PGEN(PLT,IRG,CURIYR) = PGEN(PLT,IRG,CURIYR) + UGNSONR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,IRG,CURIYR) = PGEN(EFD_D_CAP + 1,IRG,CURIYR) + UGNSONR(OWN,IRG,CURIYR)
             PGEN(PLT,MNUMNR,CURIYR) = PGEN(PLT,MNUMNR,CURIYR) + UGNSONR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) = PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) + UGNSONR(OWN,IRG,CURIYR)
             UGENRN(IRG,CURIYR) = UGENRN(IRG,CURIYR) + UGNSONR(OWN,IRG,CURIYR)
             UGENRN(MNUMNR,CURIYR) = UGENRN(MNUMNR,CURIYR) + UGNSONR(OWN,IRG,CURIYR)
! PV
             PLT = UISPV
             PGEN(PLT,IRG,CURIYR) = PGEN(PLT,IRG,CURIYR) + UGNPVNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,IRG,CURIYR) = PGEN(EFD_D_CAP + 1,IRG,CURIYR) + UGNPVNR(OWN,IRG,CURIYR)
             PGEN(PLT,MNUMNR,CURIYR) = PGEN(PLT,MNUMNR,CURIYR) + UGNPVNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) = PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) + UGNPVNR(OWN,IRG,CURIYR)
             UGENRN(IRG,CURIYR) = UGENRN(IRG,CURIYR) + UGNPVNR(OWN,IRG,CURIYR)
             UGENRN(MNUMNR,CURIYR) = UGENRN(MNUMNR,CURIYR) + UGNPVNR(OWN,IRG,CURIYR)
! PV - Fixed Axis
             PLT = UIPVT
             PGEN(PLT,IRG,CURIYR) = PGEN(PLT,IRG,CURIYR) + UGNPTNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,IRG,CURIYR) = PGEN(EFD_D_CAP + 1,IRG,CURIYR) + UGNPTNR(OWN,IRG,CURIYR)
             PGEN(PLT,MNUMNR,CURIYR) = PGEN(PLT,MNUMNR,CURIYR) + UGNPTNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) = PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) + UGNPTNR(OWN,IRG,CURIYR)
             UGENRN(IRG,CURIYR) = UGENRN(IRG,CURIYR) + UGNPTNR(OWN,IRG,CURIYR)
             UGENRN(MNUMNR,CURIYR) = UGENRN(MNUMNR,CURIYR) + UGNPTNR(OWN,IRG,CURIYR)
! WIND
             PLT = UIWND
             PGEN(PLT,IRG,CURIYR) = PGEN(PLT,IRG,CURIYR) + UGNWNNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,IRG,CURIYR) = PGEN(EFD_D_CAP + 1,IRG,CURIYR) + UGNWNNR(OWN,IRG,CURIYR)
             PGEN(PLT,MNUMNR,CURIYR) = PGEN(PLT,MNUMNR,CURIYR) + UGNWNNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) = PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) + UGNWNNR(OWN,IRG,CURIYR)
             WGEN(IRG,CURIYR) = WGEN(IRG,CURIYR) + UGNWNNR(OWN,IRG,CURIYR)
             WGEN(MNUMNR,CURIYR) = WGEN(MNUMNR,CURIYR) + UGNWNNR(OWN,IRG,CURIYR)
             UGENRN(IRG,CURIYR) = UGENRN(IRG,CURIYR) + UGNWNNR(OWN,IRG,CURIYR)
             UGENRN(MNUMNR,CURIYR) = UGENRN(MNUMNR,CURIYR) + UGNWNNR(OWN,IRG,CURIYR)
! WIND LOW SPEED
             PLT = UIWNL
             PGEN(PLT,IRG,CURIYR) = PGEN(PLT,IRG,CURIYR) + UGNWLNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,IRG,CURIYR) = PGEN(EFD_D_CAP + 1,IRG,CURIYR) + UGNWLNR(OWN,IRG,CURIYR)
             PGEN(PLT,MNUMNR,CURIYR) = PGEN(PLT,MNUMNR,CURIYR) + UGNWLNR(OWN,IRG,CURIYR)
             PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) = PGEN(EFD_D_CAP + 1,MNUMNR,CURIYR) + UGNWLNR(OWN,IRG,CURIYR)
             WGEN(IRG,CURIYR) = WGEN(IRG,CURIYR) + UGNWLNR(OWN,IRG,CURIYR)
             WGEN(MNUMNR,CURIYR) = WGEN(MNUMNR,CURIYR) + UGNWLNR(OWN,IRG,CURIYR)
          END DO
       END IF
!
!      IF (FCRL .EQ. 1) THEN
!        write(6,999) CURIYR,IRG,UGENPC(IRG,CURIYR),UGENIG(IRG,CURIYR),UGENIS(IRG,CURIYR),&
!               UGENOS(IRG,CURIYR),UGENCC(IRG,CURIYR),UGENAC(IRG,CURIYR),UGENCS(IRG,CURIYR), &
!               UGENCT(IRG,CURIYR),UGENAT(IRG,CURIYR),UGENNU(IRG,CURIYR),UGENPS(IRG,CURIYR), &
!               UGENFC(IRG,CURIYR),UGENRN(IRG,CURIYR),UGENDG(IRG,CURIYR)
!      ENDIF
!999   FORMAT(1x,'!UGEN:',2I4,14F8.1)
!
!       IF LAST YEAR THEN OUTPUT TABLE TO EMMREPT
!
!          IF (CURIYR .EQ. UNYEAR)THEN
           IF (CURIYR .EQ. LASTYR .AND. (FCRL .EQ. 1 .OR. CURITR .GT. MAXITR))THEN
            IF (IRG .LE. UNRGNS .OR. IRG .EQ. MNUMNR)THEN
              WRITE(13,1000) URGNME(IRG)(1:4)
 1000 FORMAT(1h1,t40,'Generation by Plant Type and Year in Region ',a4,', 2015 - 2030',t200,'!pgen')
              WRITE(13,1100)
 1100 FORMAT(1h ,t200,'!pgen')
              WRITE(13,1200) (USYEAR(IYR),IYR = 2015 - UHBSYR , 2030 - UHBSYR)
 1200 FORMAT(1h ,' PLANT TYPE',t22,16i8,t200,'!pgen')
!             COAL STEAM
              DO PLT = UICOU , UICAS
                 WRITE(13,1300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2015 - UHBSYR , 2030 - UHBSYR)
 1300 FORMAT(1h ,1x,a15,t22,16f8.1,t200,'!pgen')
              END DO
                 WRITE(13,1400) (CGEN(IRG,IYR),IYR = 2015 - UHBSYR , 2030 - UHBSYR)
 1400 FORMAT(1h ,1x,'  Total Coal',t22,16f8.1,t200,'!pgen')
!             O/G  STEAM
              DO PLT = UISTO , UISTX
                 WRITE(13,1300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2015 - UHBSYR , 2030 - UHBSYR)
              END DO
                 WRITE(13,1500) (SGEN(IRG,IYR),IYR = 2015 - UHBSYR , 2030 - UHBSYR)
 1500 FORMAT(1h ,1x,'  Total O/G Stm',t22,16f8.1,t200,'!pgen')
!             O/G  CT
              DO PLT = UICTO , UICTX
                 WRITE(13,1300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2015 - UHBSYR , 2030 - UHBSYR)
              END DO
                 WRITE(13,1600) (TGEN(IRG,IYR),IYR = 2015 - UHBSYR , 2030 - UHBSYR)
 1600 FORMAT(1h ,1x,'  Total O/G CT ',t22,16f8.1,t200,'!pgen')
!             O/G  CC
              DO PLT = UICCO , UIACS
                 WRITE(13,1300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2015 - UHBSYR , 2030 - UHBSYR)
              END DO
                 WRITE(13,1700) (YGEN(IRG,IYR),IYR = 2015 - UHBSYR , 2030 - UHBSYR)
 1700 FORMAT(1h ,1x,'  Total O/G CC ',t22,16f8.1,t200,'!pgen')
!             FUEL CELLS
              DO PLT = UIFCG , UIFCG
                 WRITE(13,1300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2015 - UHBSYR , 2030 - UHBSYR)
              END DO
!             NUCLEAR
              DO PLT = UICNU , UIANC
                 WRITE(13,1300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2015 - UHBSYR , 2030 - UHBSYR)
              END DO
                 WRITE(13,1800) (NGEN(IRG,IYR),IYR = 2015 - UHBSYR , 2030 - UHBSYR)
 1800 FORMAT(1h ,1x,'  Total Nuclear',t22,16f8.1,t200,'!pgen')
!             BIOMASS
              DO PLT = UIBMS , UIBMS
                 WRITE(13,1300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2015 - UHBSYR , 2030 - UHBSYR)
              END DO
!             GEOTHERMAL
              DO PLT = UIGTH , UIGTH
                 WRITE(13,1300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2015 - UHBSYR , 2030 - UHBSYR)
              END DO
!             MSW
              DO PLT = UIMSW , UIMSW
                 WRITE(13,1300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2015 - UHBSYR , 2030 - UHBSYR)
              END DO
!             HYDRO
              DO PLT = UIHYC , UIHYC
                 WRITE(13,1300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2015 - UHBSYR , 2030 - UHBSYR)
              END DO
!             PUMP STORAGE
              DO PLT = UIHYR , UIHYR
                 WRITE(13,1300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2015 - UHBSYR , 2030 - UHBSYR)
              END DO
!             WIND
              DO PLT = UIWND , UIWFS
                 WRITE(13,1300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2015 - UHBSYR , 2030 - UHBSYR)
              END DO
                 WRITE(13,1900) (WGEN(IRG,IYR),IYR = 2015 - UHBSYR , 2030 - UHBSYR)
 1900 FORMAT(1h ,1x,'  Total Wind   ',t22,16f8.1,t200,'!pgen')
!             SOLAR THERMAL
              DO PLT = UISTH , UISTH
                 WRITE(13,1300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2015 - UHBSYR , 2030 - UHBSYR)
              END DO
!             PV
              DO PLT = UISPV , UIPVT
                 WRITE(13,1300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2015 - UHBSYR , 2030 - UHBSYR)
              END DO
!             DG
              DO PLT = UIDGB , UIDGP
                 WRITE(13,1300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2015 - UHBSYR , 2030 - UHBSYR)
              END DO
                 WRITE(13,2000) (DGEN(IRG,IYR),IYR = 2015 - UHBSYR , 2030 - UHBSYR)
 2000 FORMAT(1h ,1x,'  Total DG     ',t22,16f8.1,t200,'!pgen')
!             TOTAL GENERATION
                 WRITE(13,2100) (PGEN(EFD_D_CAP + 1,IRG,IYR),IYR = 2015 - UHBSYR , 2030 - UHBSYR)
 2100 FORMAT(1h ,1x,'  Total Gen    ',t22,16f8.1,t200,'!pgen')
!
              WRITE(13,3000) URGNME(IRG)(1:4)
 3000 FORMAT(1h1,t40,'Generation by Plant Type and Year in Region ',a4,', 2031 - 2050',t200,'!pgen')
              WRITE(13,3100)
 3100 FORMAT(1h ,t200,'!pgen')
              WRITE(13,3200) (USYEAR(IYR),IYR = 2031 - UHBSYR , 2050 - UHBSYR)
 3200 FORMAT(1h ,' PLANT TYPE',t22,20i8,t200,'!pgen')
!             COAL STEAM
              DO PLT = UICOU , UICAS
                 WRITE(13,3300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2031 - UHBSYR , 2050 - UHBSYR)
 3300 FORMAT(1h ,1x,a15,t22,20f8.1,t200,'!pgen')
              END DO
                 WRITE(13,3400) (CGEN(IRG,IYR),IYR = 2031 - UHBSYR , 2050 - UHBSYR)
 3400 FORMAT(1h ,1x,'  Total Coal',t22,20f8.1,t200,'!pgen')
!             O/G  STEAM
              DO PLT = UISTO , UISTX
                 WRITE(13,3300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2031 - UHBSYR , 2050 - UHBSYR)
              END DO
                 WRITE(13,3500) (SGEN(IRG,IYR),IYR = 2031 - UHBSYR , 2050 - UHBSYR)
 3500 FORMAT(1h ,1x,'  Total O/G Stm',t22,20f8.1,t200,'!pgen')
!             O/G  CT
              DO PLT = UICTO , UICTX
                 WRITE(13,3300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2031 - UHBSYR , 2050 - UHBSYR)
              END DO
                 WRITE(13,3600) (TGEN(IRG,IYR),IYR = 2031 - UHBSYR , 2050 - UHBSYR)
 3600 FORMAT(1h ,1x,'  Total O/G CT ',t22,20f8.1,t200,'!pgen')
!             O/G  CC
              DO PLT = UICCO , UIACS
                 WRITE(13,3300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2031 - UHBSYR , 2050 - UHBSYR)
              END DO
                 WRITE(13,3700) (YGEN(IRG,IYR),IYR = 2031 - UHBSYR , 2050 - UHBSYR)
 3700 FORMAT(1h ,1x,'  Total O/G CC ',t22,20f8.1,t200,'!pgen')
!             FUEL CELLS
              DO PLT = UIFCG , UIFCG
                 WRITE(13,3300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2031 - UHBSYR , 2050 - UHBSYR)
              END DO
!             NUCLEAR
              DO PLT = UICNU , UIANC
                 WRITE(13,3300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2031 - UHBSYR , 2050 - UHBSYR)
              END DO
                 WRITE(13,3800) (NGEN(IRG,IYR),IYR = 2031 - UHBSYR , 2050 - UHBSYR)
 3800 FORMAT(1h ,1x,'  Total Nuclear',t22,20f8.1,t200,'!pgen')
!             BIOMASS
              DO PLT = UIBMS , UIBMS
                 WRITE(13,3300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2031 - UHBSYR , 2050 - UHBSYR)
              END DO
!             GEOTHERMAL
              DO PLT = UIGTH , UIGTH
                 WRITE(13,3300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2031 - UHBSYR , 2050 - UHBSYR)
              END DO
!             MSW
              DO PLT = UIMSW , UIMSW
                 WRITE(13,3300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2031 - UHBSYR , 2050 - UHBSYR)
              END DO
!             HYDRO
              DO PLT = UIHYC , UIHYC
                 WRITE(13,3300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2031 - UHBSYR , 2050 - UHBSYR)
              END DO
!             PUMP STORAGE
              DO PLT = UIHYR , UIHYR
                 WRITE(13,3300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2031 - UHBSYR , 2050 - UHBSYR)
              END DO
!             WIND
              DO PLT = UIWND , UIWFS
                 WRITE(13,3300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2031 - UHBSYR , 2050 - UHBSYR)
              END DO
                 WRITE(13,3900) (WGEN(IRG,IYR),IYR = 2031 - UHBSYR , 2050 - UHBSYR)
 3900 FORMAT(1h ,1x,'  Total Wind   ',t22,20f8.1,t200,'!pgen')
!             SOLAR THERMAL
              DO PLT = UISTH , UISTH
                 WRITE(13,3300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2031 - UHBSYR , 2050 - UHBSYR)
              END DO
!             PV
              DO PLT = UISPV , UIPVT
                 WRITE(13,3300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2031 - UHBSYR , 2050 - UHBSYR)
              END DO
!             DG
              DO PLT = UIDGB , UIDGP
                 WRITE(13,3300) PLTNAME(PLT),(PGEN(PLT,IRG,IYR),IYR = 2031 - UHBSYR , 2050 - UHBSYR)
              END DO
                 WRITE(13,4000) (DGEN(IRG,IYR),IYR = 2031 - UHBSYR , 2050 - UHBSYR)
 4000 FORMAT(1h ,1x,'  Total DG     ',t22,20f8.1,t200,'!pgen')
!             TOTAL GENERATION
                 WRITE(13,4100) (PGEN(EFD_D_CAP + 1,IRG,IYR),IYR = 2031 - UHBSYR , 2050 - UHBSYR)
 4100 FORMAT(1h ,1x,'  Total Gen    ',t22,20f8.1,t200,'!pgen')
!
            END IF
           END IF
!
!     FOR HISTORICAL AND/OR STEO YEARS, OVERWRITE GENERATION BY PLANT TYPE
!
       IF (USW_OVER .GT. 0) THEN
         IF (USW_OVER .EQ. 1) THEN
            UYR_OVER = UYR_HIST
         ELSE
            UYR_OVER = UYR_STEO
         END IF
         IF (((CURIYR + UHBSYR) .LE. UYR_OVER)     &
             .AND. ((CURIYR + UHBSYR) .GE. HSTYEAR)) THEN
!
!           GENERATION BY FUEL TYPE AND GENERATION BY PLANT TYPE FOR TABLE 59
!
!              Generation by Plant Type for Table 59
!
               UGENPC(IRG,CURIYR) = HGENPC(4,IRG,CURIYR)
               UGENIG(IRG,CURIYR) = HGENIG(4,IRG,CURIYR)
               UGENIS(IRG,CURIYR) = HGENIS(4,IRG,CURIYR)
               UGENIS_ALT(IRG,CURIYR) = 0.0
               UGENOS(IRG,CURIYR) = HGENST(4,IRG,CURIYR)
               UGENCC(IRG,CURIYR) = HGENCC(4,IRG,CURIYR)
               UGENAC(IRG,CURIYR) = HGENAC(4,IRG,CURIYR)
               UGENCS(IRG,CURIYR) = HGENAS(4,IRG,CURIYR)
               UGENCS_ALT(IRG,CURIYR) = 0.0
               UGENIC(IRG,CURIYR) = 0.0
               UGENCT(IRG,CURIYR) = HGENCT(4,IRG,CURIYR)
               UGENT2(IRG,CURIYR) = 0.0
               UGENAT(IRG,CURIYR) = HGENAT(4,IRG,CURIYR)
               UGENFC(IRG,CURIYR) = HGENFC(4,IRG,CURIYR)
               UGENNU(IRG,CURIYR) = HGENNU(4,IRG,CURIYR)
               UGENDG(IRG,CURIYR) = HGENDG(4,IRG,CURIYR)
               UGENRN(IRG,CURIYR) = HGENRN(4,IRG,CURIYR) - WNGMSEL(CURIYR,IRG)
               UGENPS(IRG,CURIYR) = HGENPS(4,IRG,CURIYR) + WNGMSEL(CURIYR,IRG)
               UGENDS(IRG,CURIYR) = HGENDS(4,IRG,CURIYR)
!
          ENDIF
        ENDIF
!
      END DO

      RETURN
      END


      SUBROUTINE EFDBASE(IYR)
!
      IMPLICIT NONE
!
!     THIS SUBROUTINE WRITES KEY EFD VARIABLES TO THE OUTPUT ACCESS DATABASE

      include 'parametr'
      include 'emmparm'
      include 'control'
      include 'dispin'
      include 'dispout'
      include 'dispett'
      include 'dispcrv'
      include 'plntctl'
      include 'ncntrl'
      include 'eusprc'
      include 'edbdef'
      include 'e111d'
      include 'emission'
      include 'uecpout'
      include 'elout'
      include 'elcntl'
      include 'dsmdimen'
      include 'dsmtoefd'
      include 'postpr'
      include 'uettout'
      include 'uefdout'
      include 'cdsparms'
      include 'csapr'
      include 'emmemis'
      include 'ecp_nuc'
      include 'emm_aimms'
!

      INTEGER IYR,IRG,ISP,I,J,IOWN,K,IVIN,IS,ILD,IV,ISL,GRP,SEG,FSL
      INTEGER IPROV,KRG,IFP,IFLTP,LTNUM,IGRP,ISEG,IECP,ICRROW,IFUEL
      INTEGER NUMTABS,FULLYR
      PARAMETER (NUMTABS = 17)        ! total number of database tables
      REAL*8 CF,DUMMY,QPGN,QFGN,QHGN,QDGN,SCAP,QFFL,DMY(EFD_D_OWN)
      REAL*8 RGEN(EFD_D_RNW), RBTU(EFD_D_RNW), RNOX(EFD_D_RNW), RSO2(EFD_D_RNW), RCO2(EFD_D_RNW), RCAR(EFD_D_RNW)
      REAL*4 TLHG,TLHGP,TLHGF
      CHARACTER*1 PAGE
      CHARACTER*24 TOTAL
      CHARACTER*40 TITLE(4)
      CHARACTER*2 COL
      CHARACTER*4 TRDTYPE
!
      LOOPING = 0
      NUMCOLS = 0
      DYNSTM = ' '
      WRTSTM = ' '
      COLVALS = 0.0
      COLV = 0.0
      CHCOLVALS = ' '
      CHCOLV = ' '
!
      DUMMY = 0.0
      COL = ' :'
!
      DO IOWN = 1 , EFD_D_OWN
         DMY(IOWN) = 0.0
      END DO

      FULLYR = USYEAR(CURIYR)
!
!     Collect Resource Cost Data - Fuel_VOM, Non_Fuel_VOM, Total_FOM, Total_VOM
!                                 - T_DomEcon,T_DomFirm,T_Intexp,T_Intimp
!
      DO IRG = 1, MNUMNR

      Fuel_VOM(IRG,CURIYR) = 0.0
      Non_Fuel_VOM(IRG,CURIYR) = 0.0
      Total_FOM(IRG,CURIYR) = 0.0
      Total_VOM(IRG,CURIYR) = 0.0
!
      Total_RCC(IRG,CURIYR) = 0.0
      Total_RIC(IRG,CURIYR) = 0.0
      Total_RPS(IRG,CURIYR) = 0.0
!
      T_DomEcon(IRG,CURIYR) = 0.0
      T_DomFirm(IRG,CURIYR) = 0.0
      T_Intexp(IRG,CURIYR) = 0.0
      T_Intimp(IRG,CURIYR) = 0.0

      ENDDO
!
!     Add ACI Costs
!
!     Fuel_VOM(MNUMNR,CURIYR) = Fuel_VOM(MNUMNR,CURIYR) + ACICST(CURIYR)
!     Non_Fuel_VOM(MNUMNR,CURIYR) = Non_Fuel_VOM(MNUMNR,CURIYR)  + ACIOAM(CURIYR)
!
!     LOOP OVER REGIONS AND GET DISPIN INFORMATION
!
        TLHG = 0.0
        TLHGP = 0.0
        TLHGF = 0.0
!
      DO IRG = 1 , UNRGNS
!
        CALL GETIN(1,IRG)

!       WRITE(6,3977) CURIRUN, CURIYR+1989, CURITR, IRG, EEITAJ(1), EEITAJ(2), EEITAJ(3)
!3977   FORMAT(1X,"UTIL_08500_EEITAJ_GET",4(":",I4),3(":",F12.3))

        CALL GETOUT(IYR,IRG)
!
        TLHG = TLHG + ETHG
!
!       print *,'!hgtl',curiyr+1989,irg,ethg,tlhg
!
        DO J = 1 , EFD_D_DSP
          TLHGP = TLHGP + EQPHG(J)
!
!         print *,'!hgpl',curiyr+1989,irg,j,eqphg(j),tlhgp
!
        END DO
        DO J = 1 , EFD_D_NFL
          TLHGF = TLHGF + EQFHG(J)
!
!         print *,'!hgfl',curiyr+1989,irg,j,eqfhg(j),tlhgf
!
        END DO

!      Add ACI costs
       Non_Fuel_VOM(IRG,CURIYR) = Non_Fuel_VOM(IRG,CURIYR)  + ACITOTB(IRG,CURIYR)
       Non_Fuel_VOM(MNUMNR,CURIYR) = Non_Fuel_VOM(MNUMNR,CURIYR) + ACITOTB(IRG,CURIYR)
!
       DO J = 1 , EIPGRP
         QPGN = 0.0
         SCAP = 0.0
         DO IOWN = 1 , USW_OWN
            QPGN = QPGN + EQPGN(J,IOWN)
            DO IVIN = 1 , EFD_D_VIN
!
!                 WRITE OUT EFD DISPOUT VARIABLES BY PLANT TYPE, OWNER AND VINTAGE
!
              IF ( ECSCAP(J,IVIN,IOWN) .NE. 0.0 ) THEN
                IF (USW_DBS .GT. 0) THEN
                  WRITE(UF_DBS,2050) COL,IYR,COL,IRG,COL,CURITR, &
                   COL,J,COL,IOWN,COL, &
                   IVIN,COL,ECSCAP(J,IVIN,IOWN),COL,TRIM(SCEN_DATE)
                ENDIF
 2050    FORMAT(1X,'DOSPTOV',6(A2,I2),A2,F12.3,A2,A)

!               TNUM = 1
!               IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
!                 IF (LOOPING(TNUM) .EQ. 0) THEN
!                   NUMCOLS(TNUM) = 7
!                   DYNSTM(TNUM) =  'INSERT INTO EFD_DOSPTOV VALUES(?,?,?,?,?,?,?,?)'
!                 ENDIF
!                 LOOPING(TNUM) = LOOPING(TNUM) + 1
!                 COLV(TNUM,1,LOOPING(TNUM)) = IYR
!                 COLV(TNUM,2,LOOPING(TNUM)) = IRG
!                 COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!                 COLV(TNUM,4,LOOPING(TNUM)) = J
!                 COLV(TNUM,5,LOOPING(TNUM)) = IOWN
!                 COLV(TNUM,6,LOOPING(TNUM)) = IVIN
!                 COLV(TNUM,7,LOOPING(TNUM)) = ECSCAP(J,IVIN,IOWN)
!               IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!                  COLVALS(:,:) = COLV(TNUM,:,:)
!                  CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!                  CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!                  LOOPING(TNUM) = 0
!                 ENDIF
!               ENDIF
!
!               --- END efd_DOSPTOV ---
!
              END IF
              SCAP = SCAP + ECSCAP(J,IVIN,IOWN)
            END DO  ! IVIN
!
!              Collect Resource Cost Data - Fuel_VOM, Non_Fuel_VOM, Total_FOM, Total_VOM
!
               Fuel_VOM(IRG,CURIYR) = Fuel_VOM(IRG,CURIYR) + ERPFL(J,IOWN)
               Non_Fuel_VOM(IRG,CURIYR) = Non_Fuel_VOM(IRG,CURIYR)  + ERPOM(J,IOWN)
               Total_FOM(IRG,CURIYR) = Total_FOM(IRG,CURIYR) + ERTOMF(J,IOWN) + ERTOMX(J,IOWN)
               Total_VOM(IRG,CURIYR) = Total_VOM(IRG,CURIYR) + ERPOM(J,IOWN) + ERPFL(J,IOWN)

               Fuel_VOM(MNUMNR,CURIYR) = Fuel_VOM(MNUMNR,CURIYR) + ERPFL(J,IOWN)
               Non_Fuel_VOM(MNUMNR,CURIYR) = Non_Fuel_VOM(MNUMNR,CURIYR)  + ERPOM(J,IOWN)
               Total_FOM(MNUMNR,CURIYR) = Total_FOM(MNUMNR,CURIYR) + ERTOMF(J,IOWN) + ERTOMX(J,IOWN)
               Total_VOM(MNUMNR,CURIYR) = Total_VOM(MNUMNR,CURIYR) + ERPOM(J,IOWN) + ERPFL(J,IOWN)

!
!              WRITE OUT EFD DISPOUT VARIABLES BY PLANT TYPE AND OWNER
!
              IF (( EQPGN(J,IOWN) .NE. 0.0 ) .OR. &
                 ( ERTOMF(J,IOWN) .NE. 0.0 ) .OR. &
                 ( ERPOM(J,IOWN) .NE. 0.0 ) .OR. &
                 ( ERPFL(J,IOWN) .NE. 0.0 ) .OR. &
                 ( ECSRET(J,IOWN) .NE. 0.0 )) THEN
                IF (USW_DBS .GT. 0) THEN
            WRITE(UF_DBS,2100) COL,IYR,COL,IRG,COL,CURITR,COL, &
               J,COL,IOWN,COL, &
               EQPGN(J,IOWN)*0.001,COL,ERTOMF(J,IOWN),COL, &
               ERPOM(J,IOWN),COL,ERPFL(J,IOWN),COL,ECSRET(J,IOWN), &
               COL,TRIM(SCEN_DATE)
                ENDIF
!
!                --- efd_DOSPTO ---
!
!               TNUM = 2
!                IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
!                   IF (LOOPING(TNUM) .EQ. 0) THEN
!                     NUMCOLS(TNUM) = 10
!                     DYNSTM(TNUM) = 'INSERT INTO EFD_DOSPTO VALUES(?,?,?,?,?,?,?,?,?,?,?)'
!                   ENDIF
!                   LOOPING(TNUM) = LOOPING(TNUM) + 1
!                   COLV(TNUM,1,LOOPING(TNUM)) = IYR
!                   COLV(TNUM,2,LOOPING(TNUM)) = IRG
!                   COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!                   COLV(TNUM,4,LOOPING(TNUM)) = J
!                   COLV(TNUM,5,LOOPING(TNUM)) = IOWN
!                   COLV(TNUM,6,LOOPING(TNUM)) = EQPGN(J,IOWN)*0.001
!                   COLV(TNUM,7,LOOPING(TNUM)) = ERTOMF(J,IOWN)
!                   COLV(TNUM,8,LOOPING(TNUM)) = ERPOM(J,IOWN)
!                   COLV(TNUM,9,LOOPING(TNUM)) = ERPFL(J,IOWN)
!                   COLV(TNUM,10,LOOPING(TNUM)) = ECSRET(J,IOWN)
!                    IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!                    COLVALS(:,:) = COLV(TNUM,:,:)
!                    CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!                    CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!                    LOOPING(TNUM) = 0
!                   ENDIF
!                ENDIF
!
!                --- END efd_DOSPTO ---
!
              END IF
 2100    FORMAT(1X,'DOSPTO',5(A2,I2),A2,3(F9.3,A2),2(F12.3,A2),A)
!
         END DO  ! IOWN
!
!           WRITE OUT EFD DISPOUT VARIABLES BY PLANT TYPE AND SEASON
!
         IF (((USW_DBS .GT. 0) .OR. (ORCLEFD .EQ. 1)) .AND. &
             (USW_DBGRP(CURIYR) .EQ. 1)) THEN
             DO IS = 1 , EENSP
               IF (( EGENPS(J,IS) .NE. 0.0 ) .OR. &
                 ( EAVLPS(J,IS) .NE. 0.0 ) .OR. &
                 ( ECAPPS(J,IS) .NE. 0.0 )) THEN
                 IF (USW_DBS .GT. 0) THEN
                  WRITE(UF_DBS,2150) COL,IYR,COL,IRG,COL,CURITR, &
                   COL,J,COL,IS,COL, &
                   EGENPS(J,IS)*.001,COL,ECAPPS(J,IS),COL, &
                   EAVLPS(J,IS),COL,TRIM(SCEN_DATE)
                 END IF
 2150    FORMAT(1X,'DOSPTS',5(A2,I2),A2,3(F12.3,A2),A)
!
!        --- START efd_DOSPTS ---
!
!             TNUM = 3
!             IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
!                 IF (LOOPING(TNUM) .EQ. 0) THEN
!                   NUMCOLS(TNUM) = 8
!                   DYNSTM(TNUM) = 'INSERT INTO EFD_DOSPTS VALUES(?,?,?,?,?,?,?,?,?)'
!                 ENDIF
!                 LOOPING(TNUM) = LOOPING(TNUM) + 1
!                 COLV(TNUM,1,LOOPING(TNUM)) = IYR
!                 COLV(TNUM,2,LOOPING(TNUM)) = IRG
!                 COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!                 COLV(TNUM,4,LOOPING(TNUM)) = J
!                 COLV(TNUM,5,LOOPING(TNUM)) = IS
!                 COLV(TNUM,6,LOOPING(TNUM)) = EGENPS(J,IS) * .001
!                 COLV(TNUM,7,LOOPING(TNUM)) = ECAPPS(J,IS)
!                 COLV(TNUM,8,LOOPING(TNUM)) = EAVLPS(J,IS)
!                       IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!                    COLVALS(:,:) = COLV(TNUM,:,:)
!                    CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!                    CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!                    LOOPING(TNUM) = 0
!                 ENDIF
!              ENDIF
              ENDIF
            ENDDO
         ENDIF
!
!              --- END efd_DOSPTS ---
!
!           WRITE OUT EFD DISPOUT VARIABLES BY PLANT TYPE AND FUEL
!
         DO IFLTP = 1,UNFUELS
           DO IOWN = 1 , USW_OWN
             IF (( EQPFLGN(J,IOWN,IFLTP) .NE. 0.0 ) .OR. &
                 ( EQPFLCN(J,IOWN,IFLTP) .NE. 0.0 )) THEN
               IF (USW_DBS .GT. 0) THEN
                WRITE(UF_DBS,2160) COL,IYR,COL,IRG,COL,CURITR, &
                 COL,J,COL,IOWN,COL,IFLTP, &
                 COL,EQPFLGN(J,IOWN,IFLTP)*0.001,COL,    &
                 EQPFLCN(J,IOWN,IFLTP)*0.001,COL,TRIM(SCEN_DATE)
               ENDIF
!
!                --- START efd_DOSPTF ---
!
!              TNUM = 4
!
!                    IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
!                 IF (LOOPING(TNUM) .EQ. 0) THEN
!                   NUMCOLS(TNUM) = 8
!                   DYNSTM(TNUM) = 'INSERT INTO EFD_DOSPTF VALUES(?,?,?,?,?,?,?,?,?)'
!                 ENDIF
!                 LOOPING(TNUM) = LOOPING(TNUM) + 1
!                 COLV(TNUM,1,LOOPING(TNUM)) = IYR
!                 COLV(TNUM,2,LOOPING(TNUM)) = IRG
!                 COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!                 COLV(TNUM,4,LOOPING(TNUM)) = J
!                       COLV(TNUM,5,LOOPING(TNUM)) = IOWN
!                       COLV(TNUM,6,LOOPING(TNUM)) = IFLTP
!                       COLV(TNUM,7,LOOPING(TNUM)) = EQPFLGN(J,IOWN,IFLTP)*0.001
!                       COLV(TNUM,8,LOOPING(TNUM)) = EQPFLCN(J,IOWN,IFLTP)*0.001
!                       IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!                    COLVALS(:,:) = COLV(TNUM,:,:)
!                    CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!                CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!                    LOOPING(TNUM) = 0
!                 ENDIF
!              ENDIF
!
!                --- END efd_DOSPTF ---
!
             END IF
           ENDDO
         ENDDO
 2160    FORMAT(1X,'DOSPTF',6(A2,I2),A2,2(F12.3,A2),A)

         CF = 0.0
         IF(EQPCP(J) .GT. 0.0) THEN
            CF = QPGN / (EQPCP(J) * 0.001 * 8760)
         END IF
!
!           WRITE OUT EFD DISPOUT VARIABLES BY PLANT TYPE ONLY
!
         IF (USW_DBS .GT. 0) THEN
         WRITE(UF_DBS,2200) COL,IYR,COL,IRG,COL,CURITR, &
            COL,J,COL,EQPCP(J),COL, &
            CF,COL,EQPFL(J)*0.001,COL,EQPSO2(J)*0.001,COL, &
            EQPNOX(J)*0.001,COL,EQPCO2(J)*0.001,COL,&
            EQPHG(J),COL,TRIM(SCEN_DATE)   ! mercury in tons, others in thousand tons!!
         ENDIF
 2200    FORMAT(1X,'DOSPT',4(A2,I2),A2,F12.3,A2,   &
                     F5.3,A2,5(F12.3,A2),A)
!
!        --- START efd_DOSPT ---
!
!        TNUM = 5
!        IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
!           IF (LOOPING(TNUM) .EQ. 0) THEN
!              NUMCOLS(TNUM) = 11
!              DYNSTM(TNUM) = 'INSERT INTO EFD_DOSPT VALUES(?,?,?,?,?,?,?,?,?,?,?,?)'
!           ENDIF
!           LOOPING(TNUM) = LOOPING(TNUM) + 1
!           COLV(TNUM,1,LOOPING(TNUM)) = IYR
!           COLV(TNUM,2,LOOPING(TNUM)) = IRG
!           COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!           COLV(TNUM,4,LOOPING(TNUM)) = J
!           COLV(TNUM,5,LOOPING(TNUM)) = EQPCP(J)
!           COLV(TNUM,6,LOOPING(TNUM)) = CF
!           COLV(TNUM,7,LOOPING(TNUM)) = EQPFL(J)*0.001
!           COLV(TNUM,8,LOOPING(TNUM)) = EQPSO2(J)*0.001
!           COLV(TNUM,9,LOOPING(TNUM)) = EQPNOX(J)*0.001
!           COLV(TNUM,10,LOOPING(TNUM)) = EQPCO2(J)*0.001
!              COLV(TNUM,11,LOOPING(TNUM)) = EQPHG(J)
!              IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!              COLVALS(:,:) = COLV(TNUM,:,:)
!                    CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!              CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!              LOOPING(TNUM) = 0
!           ENDIF
!        ENDIF
!
!        --- END efd_DOSPT ---
!
      END DO  ! J
!
!     GET RENEWABLE EMISSIONS FROM WGRP INFO AND SUM TO PLANT TYPE LEVEL
!
      RGEN = 0.0
      RBTU = 0.0
      RNOX = 0.0
      RSO2 = 0.0
      RCO2 = 0.0
      RCAR = 0.0
!
      DO I = 1 , EHNTP
         IGRP = EHDBID(I)
         K = ULEFDT(IGRP) - EIPGRP
!
         DO J = 1 , EIFPLT
            IFUEL = EHFLTP(I,J)
            IF (IFUEL .GT. 0) THEN
               IF (K .GT. 0 .AND. K .LE. EFD_D_RNW .AND. IGRP .GT. 0 .AND. IGRP .LE. EMM_D_GRP) THEN
                  RGEN(K) = RGEN(K) + ULGENE(J,IGRP)
                  RBTU(K) = RBTU(K) + ULBTUE(J,IGRP)
                  RNOX(K) = RNOX(K) + ULNOXW(J,IGRP)
                  RSO2(K) = RSO2(K) + ULSO2W(J,IGRP)
                  RCO2(K) = RCO2(K) + ULCO2W(J,IGRP)
                  RCAR(K) = RCAR(K) + ULCARW(J,IGRP)
               ELSE
                  WRITE(6,3715) CURIYR+UHBSYR,CURITR,I,IGRP,K,J,IFUEL
 3715             FORMAT(1X,"RNEW_OOPS",7(":",I12))
               END IF
            END IF
         END DO
      END DO
!
      DO J = 1 , EIHGRP
         K = J + EIPGRP
         QHGN = 0.0
         SCAP = 0.0
         DO IOWN = 1 , USW_OWN
            QHGN = QHGN + EQHGN(J,IOWN)
            DO IVIN = 1 , EFD_D_VIN
!
!                 WRITE OUT EFD DISPOUT VARIABLES BY PLANT TYPE, OWNER AND VINTAGE
!
              IF ( EHSCAP(J,IVIN,IOWN) .NE. 0.0) THEN
                IF (USW_DBS .GT. 0) THEN
                WRITE(UF_DBS,2050) COL,IYR,COL,IRG,COL,CURITR, &
                 COL,K,COL,IOWN,COL, &
                 IVIN,COL,EHSCAP(J,IVIN,IOWN),COL,TRIM(SCEN_DATE)
                ENDIF
!
!               --- START efd_DOSPTOV ---
!
!               TNUM = 1
!               IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
!                  LOOPING(TNUM) = LOOPING(TNUM) + 1
!                  COLV(TNUM,1,LOOPING(TNUM)) = IYR
!                  COLV(TNUM,2,LOOPING(TNUM)) = IRG
!                  COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!                  COLV(TNUM,4,LOOPING(TNUM)) = K
!                  COLV(TNUM,5,LOOPING(TNUM)) = IOWN
!                  COLV(TNUM,6,LOOPING(TNUM)) = IVIN
!                  COLV(TNUM,7,LOOPING(TNUM)) = EHSCAP(J,IVIN,IOWN)
!                       IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!                   COLVALS(:,:) = COLV(TNUM,:,:)
!                    CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!                   CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!                   LOOPING(TNUM) = 0
!                  ENDIF
!               ENDIF
!
!               --- END efd_DOSPTOV ---
!
              END IF
              SCAP = SCAP + EHSCAP(J,IVIN,IOWN)
            END DO  ! IVIN
!
!              Collect Resource Cost Data - Fuel_VOM, Non_Fuel_VOM, Total_FOM, Total_VOM
!
               Non_Fuel_VOM(IRG,CURIYR) = Non_Fuel_VOM(IRG,CURIYR)  + ERHOM(J,IOWN)
               Total_FOM(IRG,CURIYR) = Total_FOM(IRG,CURIYR) + ERTOMF(K,IOWN) + ERTOMX(K,IOWN)
               Total_VOM(IRG,CURIYR) = Total_VOM(IRG,CURIYR) + ERHOM(J,IOWN)

               Non_Fuel_VOM(MNUMNR,CURIYR) = Non_Fuel_VOM(MNUMNR,CURIYR)  + ERHOM(J,IOWN)
               Total_FOM(MNUMNR,CURIYR) = Total_FOM(MNUMNR,CURIYR) + ERTOMF(K,IOWN) + ERTOMX(K,IOWN)
               Total_VOM(MNUMNR,CURIYR) = Total_VOM(MNUMNR,CURIYR) + ERHOM(J,IOWN)
!
!              WRITE OUT EFD RENEW DISPOUT VARIABLES BY PLANT TYPE AND OWNER
!
              IF (( EQHGN(J,IOWN) .NE. 0.0 ) .OR. &
                 ( ERTOMF(K,IOWN) .NE. 0.0 ) .OR. &
                 ( ERHOM(J,IOWN) .NE. 0.0 ) .OR. &
                 ( EHSRET(J,IOWN) .NE. 0.0 )) THEN
                IF (USW_DBS .GT. 0) THEN
            WRITE(UF_DBS,2100) COL,IYR,COL,IRG,COL,CURITR, &
              COL,K,COL,IOWN,COL, &
              EQHGN(J,IOWN)*0.001,COL,ERTOMF(K,IOWN),COL, &
              ERHOM(J,IOWN),COL,DUMMY,COL,EHSRET(J,IOWN),COL,TRIM(SCEN_DATE)
                ENDIF
!
!                --- START efd_DOSPTO ---
!
!                TNUM = 2
!                IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
!                   LOOPING(TNUM) = LOOPING(TNUM) + 1
!                   COLV(TNUM,1,LOOPING(TNUM)) = IYR
!                   COLV(TNUM,2,LOOPING(TNUM)) = IRG
!                   COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!                   COLV(TNUM,4,LOOPING(TNUM)) = K
!                   COLV(TNUM,5,LOOPING(TNUM)) = IOWN
!                   COLV(TNUM,6,LOOPING(TNUM)) = EQHGN(J,IOWN)*0.001
!                   COLV(TNUM,7,LOOPING(TNUM)) = ERTOMF(K,IOWN)
!                   COLV(TNUM,8,LOOPING(TNUM)) = ERHOM(J,IOWN)
!                   COLV(TNUM,9,LOOPING(TNUM)) = DUMMY
!                   COLV(TNUM,10,LOOPING(TNUM)) = EHSRET(J,IOWN)
!                    IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!                    COLVALS(:,:) = COLV(TNUM,:,:)
!                    CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!                    CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!                    LOOPING(TNUM) = 0
!                   ENDIF
!                ENDIF
!
!                --- END efd_DOSPTO ---
!
              END IF
         END DO  ! IOWN
!
!           WRITE OUT EFD DISPOUT VARIALBES BY PLANT TYPE AND SEASON
!
         IF (((USW_DBS .GT. 0) .OR. (ORCLEFD .EQ. 1)) .AND. &
              (USW_DBGRP(CURIYR) .EQ. 1)) THEN
           DO IS = 1 , EENSP
             IF (( EGENHS(J,IS) .NE. 0.0 ) .OR. &
               ( ECAPHS(J,IS) .NE. 0.0 )) THEN
               IF (USW_DBS .GT. 0) THEN
                 WRITE(UF_DBS,2150) COL,IYR,COL,IRG,COL, &
                 CURITR,COL,K,COL,IS,COL, &
                 EGENHS(J,IS)*.001,COL,ECAPHS(J,IS),COL,ECAPHS(J,IS), &
                 COL,TRIM(SCEN_DATE)
               ENDIF
!
!        --- START efd_DOSPTS ---
!
!              TNUM = 3
!              IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
!                 LOOPING(TNUM) = LOOPING(TNUM) + 1
!                 COLV(TNUM,1,LOOPING(TNUM)) = IYR
!                 COLV(TNUM,2,LOOPING(TNUM)) = IRG
!                 COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!                 COLV(TNUM,4,LOOPING(TNUM)) = K
!                 COLV(TNUM,5,LOOPING(TNUM)) = IS
!                 COLV(TNUM,6,LOOPING(TNUM)) = EGENHS(J,IS) * .001
!                 COLV(TNUM,7,LOOPING(TNUM)) = ECAPHS(J,IS)
!                 COLV(TNUM,8,LOOPING(TNUM)) = ECAPHS(J,IS)
!                       IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!                  COLVALS(:,:) = COLV(TNUM,:,:)
!                    CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!                  CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!                  LOOPING(TNUM) = 0
!                 ENDIF
!              ENDIF
             ENDIF
           END DO
         END IF
!
!              --- END efd_DOSPTS ---
!
!         WRITE OUT EFD DISPOUT VARIABLES BY PLANT TYPE AND FUEL
!
         DO IFLTP = 1,UNFUELS
           DO IOWN = 1 , USW_OWN
             IF (( EQPFLGN(K,IOWN,IFLTP) .NE. 0.0 ) .OR.    &
                 ( EQPFLCN(K,IOWN,IFLTP) .NE. 0.0 )) THEN
               IF (USW_DBS .GT. 0) THEN
                WRITE(UF_DBS,2160) COL,IYR,COL,IRG,COL,CURITR,  &
                 COL,K,COL,IOWN,COL,IFLTP,  &
                 COL,EQPFLGN(K,IOWN,IFLTP)*0.001,COL,  &
                 EQPFLCN(K,IOWN,IFLTP)*0.001,COL,TRIM(SCEN_DATE)
               ENDIF
!
!                --- START efd_DOSPTF ---
!
!                TNUM = 4
!                    IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
!                   LOOPING(TNUM) = LOOPING(TNUM) + 1
!                   COLV(TNUM,1,LOOPING(TNUM)) = IYR
!                   COLV(TNUM,2,LOOPING(TNUM)) = IRG
!                   COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!                   COLV(TNUM,4,LOOPING(TNUM)) = K
!                       COLV(TNUM,5,LOOPING(TNUM)) = IOWN
!                       COLV(TNUM,6,LOOPING(TNUM)) = IFLTP
!                       COLV(TNUM,7,LOOPING(TNUM)) = EQPFLGN(K,IOWN,IFLTP)*0.001
!                       COLV(TNUM,8,LOOPING(TNUM)) = EQPFLCN(K,IOWN,IFLTP)*0.001
!                       IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!                     COLVALS(:,:) = COLV(TNUM,:,:)
!                    CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!               CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!                     LOOPING(TNUM) = 0
!                   ENDIF
!                 ENDIF
!
!                --- END efd_DOSPTF ---
!
             END IF
           ENDDO
         ENDDO
!
         CF = 0.0
         IF(EQHCP(J) .GT. 0.0) CF = QHGN / (EQHCP(J) * 0.001 * 8760)
!
!           WRITE OUT EFD RENEW DISPOUT VARIABLES BY PLANT TYPE ONLY
!
         IF (USW_DBS .GT. 0) THEN
          WRITE(UF_DBS,2200) COL,IYR,COL,IRG,COL, &
            CURITR,COL,K,COL,EQHCP(J),COL, &
            CF,COL,RBTU(J)*0.001,COL,RSO2(J)*0.001,COL, &
            RNOX(J)*0.001,COL,RCO2(J)*0.001,COL,DUMMY,COL,TRIM(SCEN_DATE)
         ENDIF
!
!        --- START efd_DOSPT ---
!
!        TNUM = 5
!        IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
!           LOOPING(TNUM) = LOOPING(TNUM) + 1
!           COLV(TNUM,1,LOOPING(TNUM)) = IYR
!           COLV(TNUM,2,LOOPING(TNUM)) = IRG
!           COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!           COLV(TNUM,4,LOOPING(TNUM)) = K
!           COLV(TNUM,5,LOOPING(TNUM)) = EQHCP(J)
!           COLV(TNUM,6,LOOPING(TNUM)) = CF
!           COLV(TNUM,7,LOOPING(TNUM)) = DUMMY
!           COLV(TNUM,8,LOOPING(TNUM)) = DUMMY
!           COLV(TNUM,9,LOOPING(TNUM)) = DUMMY
!           COLV(TNUM,10,LOOPING(TNUM)) = DUMMY
!              COLV(TNUM,11,LOOPING(TNUM)) = DUMMY
!              IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!             COLVALS(:,:) = COLV(TNUM,:,:)
!                    CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!             CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!             LOOPING(TNUM) = 0
!           ENDIF
!        ENDIF
!
!        --- END efd_DOSPT ---
!
      END DO  ! J
!
!        WRITE OUT DISTRIBUTED GENERATION RECORDS
!
      DO J = 1 , EIDGRP
         K = J + EIPGRP + EIHGRP
         QDGN = 0.0
         SCAP = 0.0
         DO IOWN = 1 , USW_OWN
            QDGN = QDGN + EQDGN(J,IOWN)
            DO IVIN = 1 , EFD_D_VIN
!
!                 WRITE OUT EFD DISPOUT VARIABLES BY PLANT TYPE, OWNER AND VINTAGE
!
              IF ( EDSCAP(J,IVIN,IOWN) .NE. 0.0) THEN
                IF (USW_DBS .GT. 0) THEN
                WRITE(UF_DBS,2050) COL,IYR,COL,IRG,COL,CURITR, &
                 COL,K,COL,IOWN,COL, &
                 IVIN,COL,EDSCAP(J,IVIN,IOWN),COL,TRIM(SCEN_DATE)
                ENDIF
!
!               --- START efd_DOSPTOV ---
!
!               TNUM = 1
!               IF ( (ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1) ) THEN
!                 LOOPING(TNUM) = LOOPING(TNUM) + 1
!                 COLV(TNUM,1,LOOPING(TNUM)) = IYR
!                 COLV(TNUM,2,LOOPING(TNUM)) = IRG
!                 COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!                 COLV(TNUM,4,LOOPING(TNUM)) = K
!                 COLV(TNUM,5,LOOPING(TNUM)) = IOWN
!                 COLV(TNUM,6,LOOPING(TNUM)) = IVIN
!                 COLV(TNUM,7,LOOPING(TNUM)) = EDSCAP(J,IVIN,IOWN)
!                       IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!                   COLVALS(:,:) = COLV(TNUM,:,:)
!                    CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!                   CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!                   LOOPING(TNUM) = 0
!                 ENDIF
!               ENDIF
!
!               --- END efd_DOSPTOV ---
!
              END IF
              SCAP = SCAP + EDSCAP(J,IVIN,IOWN)
            END DO  ! IVIN
!
!              Collect Resource Cost Data - Fuel_VOM, Non_Fuel_VOM, Total_FOM, Total_VOM
!
               Fuel_VOM(IRG,CURIYR) = Fuel_VOM(IRG,CURIYR) + ERDFL(J,IOWN)
               Non_Fuel_VOM(IRG,CURIYR) = Non_Fuel_VOM(IRG,CURIYR)  + ERDOM(J,IOWN)
               Total_FOM(IRG,CURIYR) = Total_FOM(IRG,CURIYR) + ERTOMF(K,IOWN) + ERTOMX(K,IOWN)
               Total_VOM(IRG,CURIYR) = Total_VOM(IRG,CURIYR) + ERDOM(J,IOWN) + ERDFL(J,IOWN)

               Fuel_VOM(MNUMNR,CURIYR) = Fuel_VOM(MNUMNR,CURIYR) + ERDFL(J,IOWN)
               Non_Fuel_VOM(MNUMNR,CURIYR) = Non_Fuel_VOM(MNUMNR,CURIYR)  + ERDOM(J,IOWN)
               Total_FOM(MNUMNR,CURIYR) = Total_FOM(MNUMNR,CURIYR) + ERTOMF(K,IOWN) + ERTOMX(K,IOWN)
               Total_VOM(MNUMNR,CURIYR) = Total_VOM(MNUMNR,CURIYR) + ERDOM(J,IOWN) + ERDFL(J,IOWN)
!
!
!              WRITE OUT EFD DGEN DISPOUT VARIABLES BY PLANT TYPE AND OWNER
!
              IF (( EQDGN(J,IOWN) .NE. 0.0 ) .OR. &
                 ( ERTOMF(K,IOWN) .NE. 0.0 ) .OR. &
                 ( ERDOM(J,IOWN) .NE. 0.0 ) .OR. &
                 ( ERDFL(J,IOWN) .NE. 0.0 ) .OR. &
                 ( EDSRET(J,IOWN) .NE. 0.0 )) THEN
                IF (USW_DBS .GT. 0) THEN
                  WRITE(UF_DBS,2100) COL,IYR,COL,IRG,COL,CURITR, &
                    COL,K,COL,IOWN,COL, &
                    EQDGN(J,IOWN)*0.001,COL,ERTOMF(K,IOWN),COL, &
                  ERDOM(J,IOWN),COL,ERDFL(J,IOWN),COL,EDSRET(J,IOWN),COL,TRIM(SCEN_DATE)
                ENDIF
!
!                --- START efd_DOSPTO ---
!
!               TNUM = 2
!               IF ( (ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1) ) THEN
!                 LOOPING(TNUM) = LOOPING(TNUM) + 1
!                 COLV(TNUM,1,LOOPING(TNUM)) = IYR
!                 COLV(TNUM,2,LOOPING(TNUM)) = IRG
!                 COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!                 COLV(TNUM,4,LOOPING(TNUM)) = K
!                 COLV(TNUM,5,LOOPING(TNUM)) = IOWN
!                 COLV(TNUM,6,LOOPING(TNUM)) = EQDGN(J,IOWN)*0.001
!                 COLV(TNUM,7,LOOPING(TNUM)) = ERTOMF(K,IOWN)
!                 COLV(TNUM,8,LOOPING(TNUM)) = ERDOM(J,IOWN)
!                 COLV(TNUM,9,LOOPING(TNUM)) = ERDFL(J,IOWN)
!                 COLV(TNUM,10,LOOPING(TNUM)) = EDSRET(J,IOWN)
!                    IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!                   COLVALS(:,:) = COLV(TNUM,:,:)
!                    CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!                   CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!                   LOOPING(TNUM) = 0
!                 ENDIF
!               ENDIF
!
!                --- END efd_DOSPTO ---
!
              END IF
         END DO  ! IOWN
!
!           WRITE OUT EFD DISPOUT VARIALBES BY PLANT TYPE AND SEASON
!
         IF (((USW_DBS .GT. 0) .OR. (ORCLEFD .EQ. 1)) .AND. &
              (USW_DBGRP(CURIYR) .EQ. 1)) THEN
           DO IS = 1 , EENSP
             IF (( EGENDS(J,IS) .NE. 0.0 ) .OR. &
               ( ECAPDS(J,IS) .NE. 0.0 )) THEN
               IF (USW_DBS .GT. 0) THEN
                 WRITE(UF_DBS,2150) COL,IYR,COL,IRG,COL, &
                 CURITR,COL,K,COL,IS,COL, &
                 EGENDS(J,IS)*.001,COL,ECAPDS(J,IS),COL,ECAPDS(J,IS), &
                 COL,TRIM(SCEN_DATE)
               ENDIF
!
!        --- START efd_DOSPTS ---
!
!               TNUM = 3
!               IF ( (ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1) ) THEN
!                 LOOPING(TNUM) = LOOPING(TNUM) + 1
!                 COLV(TNUM,1,LOOPING(TNUM)) = IYR
!                 COLV(TNUM,2,LOOPING(TNUM)) = IRG
!                 COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!                 COLV(TNUM,4,LOOPING(TNUM)) = K
!                 COLV(TNUM,5,LOOPING(TNUM)) = IS
!                 COLV(TNUM,6,LOOPING(TNUM)) = EGENDS(J,IS) * .001
!                 COLV(TNUM,7,LOOPING(TNUM)) = ECAPDS(J,IS)
!                 COLV(TNUM,8,LOOPING(TNUM)) = ECAPDS(J,IS)
!                       IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!                   COLVALS(:,:) = COLV(TNUM,:,:)
!                    CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!                   CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!                   LOOPING(TNUM) = 0
!                 ENDIF
!              ENDIF
!
!                    --- END efd_DOSPTS ---
!
             ENDIF
           END DO
         END IF
!
!         WRITE OUT EFD DISPOUT VARIABLES BY PLANT TYPE AND FUEL
!
         DO IFLTP = 1,UNFUELS
           DO IOWN = 1 , USW_OWN
             IF (( EQPFLGN(K,IOWN,IFLTP) .NE. 0.0 ) .OR.    &
                 ( EQPFLCN(K,IOWN,IFLTP) .NE. 0.0 )) THEN
               IF (USW_DBS .GT. 0) THEN
                WRITE(UF_DBS,2160) COL,IYR,COL,IRG,COL,CURITR,  &
                 COL,K,COL,IOWN,COL,IFLTP,  &
                 COL,EQPFLGN(K,IOWN,IFLTP)*0.001,COL,  &
                 EQPFLCN(K,IOWN,IFLTP)*0.001,COL,TRIM(SCEN_DATE)
               ENDIF
!
!                --- START efd_DOSPTF ---
!
!              TNUM = 4
!                    IF ( (ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1) ) THEN
!                LOOPING(TNUM) = LOOPING(TNUM) + 1
!                COLV(TNUM,1,LOOPING(TNUM)) = IYR
!                COLV(TNUM,2,LOOPING(TNUM)) = IRG
!                COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!                COLV(TNUM,4,LOOPING(TNUM)) = K
!                       COLV(TNUM,5,LOOPING(TNUM)) = IOWN
!                       COLV(TNUM,6,LOOPING(TNUM)) = IFLTP
!                       COLV(TNUM,7,LOOPING(TNUM)) = EQPFLGN(K,IOWN,IFLTP)*0.001
!                       COLV(TNUM,8,LOOPING(TNUM)) = EQPFLCN(K,IOWN,IFLTP)*0.001
!                       IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!                  COLVALS(:,:) = COLV(TNUM,:,:)
!                    CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!              CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!                  LOOPING(TNUM) = 0
!                ENDIF
!              ENDIF
!
!                --- END efd_DOSPTF ---
!
             END IF
           ENDDO
         ENDDO
!
         CF = 0.0
         IF(EQDCP(J) .GT. 0.0) CF = QDGN / &
             (EQDCP(J) * 0.001 * 8760)
!
!           WRITE OUT EFD DGEN DISPOUT VARIABLES BY PLANT TYPE ONLY
!
         IF (USW_DBS .GT. 0) THEN
          WRITE(UF_DBS,2200) COL,IYR,COL,IRG,COL, &
            CURITR,COL,K,COL,EQDCP(J),COL, &
            CF,COL,EQDFL(J)*0.001,COL,EQDSO2(J)*0.001,COL, &
               EQDNOX(J)*0.001,COL,EQDCO2(J)*0.001,COL,EQDHG(J),COL,TRIM(SCEN_DATE)
         ENDIF
!
!        --- START efd_DOSPT ---
!
!        TNUM = 5
!        IF ( (ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1) ) THEN
!          LOOPING(TNUM) = LOOPING(TNUM) + 1
!          COLV(TNUM,1,LOOPING(TNUM)) = IYR
!          COLV(TNUM,2,LOOPING(TNUM)) = IRG
!          COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!          COLV(TNUM,4,LOOPING(TNUM)) = K
!          COLV(TNUM,5,LOOPING(TNUM)) = EQDCP(J)
!          COLV(TNUM,6,LOOPING(TNUM)) = CF
!          COLV(TNUM,7,LOOPING(TNUM)) = EQDFL(J) * 0.001
!          COLV(TNUM,8,LOOPING(TNUM)) = EQDSO2(J) * 0.001
!          COLV(TNUM,9,LOOPING(TNUM)) = EQDNOX(J) * 0.001
!          COLV(TNUM,10,LOOPING(TNUM)) = EQDCO2(J) * 0.001
!              COLV(TNUM,11,LOOPING(TNUM)) = EQDHG(J)
!              IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!            COLVALS(:,:) = COLV(TNUM,:,:)
!                    CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!            CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!            LOOPING(TNUM) = 0
!          ENDIF
!        ENDIF
!
!        --- END efd_DOSPT ---
!
      END DO  ! J
!
!        WRITE OUT EFD FUEL DISPOUT VARIABLES
!
         DO J = 1 , ENFLTP
            QFGN = 0.0
            QFFL = 0.0
            DO IOWN = 1 , USW_OWN
               QFGN = QFGN + EQFGN(J,IOWN)
               QFFL = QFFL + EQFFL(J,IOWN)
            END DO
!
!           WRITE OUT EFD DISPOUT VARIABLES BY FUEL TYPE ONLY
!
            IF (USW_DBS .GT. 0) THEN
              WRITE(UF_DBS,3000) COL,IYR,COL,IRG,COL, &
               CURITR,COL,J,COL,        &
               EPFUEL(J),COL,ERFFL(J),COL,EQFSO2(J),COL,    &
               EQFNOX(J),COL,EQFCO2(J),COL,EQFHG(J),COL,EFRSO2(J),COL, &
               EFRNOX(J),COL,EFRCO2(J),COL,EFRHG(J),COL,EFHCNT(J),COL, &
               TRIM(SCEN_DATE)
            ENDIF
 3000    FORMAT(1X,'DOSFT',4(A2,I2),A2,F12.4,A2,   &
                     F12.3,A2,4(F12.1,A2),5(F12.4,A2),A)
!
!           --- START efd_DOSFT ---
!
!           TNUM = 6
!           IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
!             IF (LOOPING(TNUM) .EQ. 0) THEN
!                NUMCOLS(TNUM) = 15
!                DYNSTM(TNUM) = 'INSERT INTO EFD_DOSFT VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
!             ENDIF
!              LOOPING(TNUM) = LOOPING(TNUM) + 1
!              COLV(TNUM,1,LOOPING(TNUM)) = IYR
!              COLV(TNUM,2,LOOPING(TNUM)) = IRG
!              COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!              COLV(TNUM,4,LOOPING(TNUM)) = J
!              COLV(TNUM,5,LOOPING(TNUM)) = EPFUEL(J)
!              COLV(TNUM,6,LOOPING(TNUM)) = ERFFL(J)
!              COLV(TNUM,7,LOOPING(TNUM)) = EQFSO2(J)
!              COLV(TNUM,8,LOOPING(TNUM)) = EQFNOX(J)
!              COLV(TNUM,9,LOOPING(TNUM)) = EQFCO2(J)
!              COLV(TNUM,10,LOOPING(TNUM)) = EQFHG(J)
!              COLV(TNUM,11,LOOPING(TNUM)) = EFRSO2(J)
!              COLV(TNUM,12,LOOPING(TNUM)) = EFRNOX(J)
!              COLV(TNUM,13,LOOPING(TNUM)) = EFRCO2(J)
!              COLV(TNUM,14,LOOPING(TNUM)) = EFRHG(J)
!              COLV(TNUM,15,LOOPING(TNUM)) = EFHCNT(J)
!              IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!                COLVALS(:,:) = COLV(TNUM,:,:)
!                    CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!                CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!                LOOPING(TNUM) = 0
!              ENDIF
!           ENDIF
!
            DO IOWN = 1 , USW_OWN
!
!              WRITE OUT EFD DISPOUT VARIABLES BY FUEL TYPE AND OWNER
!
              IF (( EQFGN(J,IOWN) .NE. 0.0 ) .OR. &
                       ( EQFFL(J,IOWN) .NE. 0.0 )) THEN
                IF (USW_DBS .GT. 0) THEN
               WRITE(UF_DBS,3100) COL,IYR,COL,IRG,COL,CURITR,COL,J,COL,  & !
                   IOWN,COL,EQFGN(J,IOWN)*.001,COL, &
                   EQFFL(J,IOWN)*.001,COL,TRIM(SCEN_DATE)
                ENDIF
 3100    FORMAT(1X,'DOSFTO',5(A2,I2),A2,F12.3,A2,F12.3,A2,A)
!
!             --- START efd_DOSFTO ---
!
!             TNUM = 7
!             IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
!               IF (LOOPING(TNUM) .EQ. 0) THEN
!                 NUMCOLS(TNUM) = 7
!                 DYNSTM(TNUM) = 'INSERT INTO EFD_DOSFTO VALUES(?,?,?,?,?,?,?,?)'
!               ENDIF
!                LOOPING(TNUM) = LOOPING(TNUM) + 1
!                COLV(TNUM,1,LOOPING(TNUM)) = IYR
!                COLV(TNUM,2,LOOPING(TNUM)) = IRG
!                COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!                COLV(TNUM,4,LOOPING(TNUM)) = J
!                COLV(TNUM,5,LOOPING(TNUM)) = IOWN
!                COLV(TNUM,6,LOOPING(TNUM)) = EQFGN(J,IOWN) * .001
!                COLV(TNUM,7,LOOPING(TNUM)) = EQFFL(J,IOWN) * .001
!                    IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!                 COLVALS(:,:) = COLV(TNUM,:,:)
!                    CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!                 CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!                 LOOPING(TNUM) = 0
!                ENDIF
!             ENDIF
!
!             --- END efd_DOSFTO ---
!
              END IF
           END DO
         END DO
!
!        WRITE OUT EFD SO2 REGIONAL ALLOWANCE TABLE
!
         DO J = 1 , NUM_SO2_GRP
            IF (USW_DBS .GT. 0) THEN
              WRITE(UF_DBS,3200) COL,IYR,COL,IRG,COL,CURITR,COL,J,COL, &
                     EGALLW(J),COL,EMELPSO2(CURIYR,J),COL,EGSO2(J),COL,TRIM(SCEN_DATE)
            ENDIF
 3200       FORMAT(1X,'DSO2A',4(A2,I2),A2,F12.1,A2,F12.4,A2,F12.1,A2,A)
!
!           --- START efd_DSO2A ---
!
!           TNUM = 8
!           IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
!              IF (LOOPING(TNUM) .EQ. 0) THEN
!                NUMCOLS(TNUM) = 7
!                DYNSTM(TNUM) = 'INSERT INTO EFD_DSO2A VALUES(?,?,?,?,?,?,?,?)'
!              ENDIF
!              LOOPING(TNUM) = LOOPING(TNUM) + 1
!              COLV(TNUM,1,LOOPING(TNUM)) = IYR
!              COLV(TNUM,2,LOOPING(TNUM)) = IRG
!              COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!              COLV(TNUM,4,LOOPING(TNUM)) = J
!              COLV(TNUM,5,LOOPING(TNUM)) = EGALLW(J)
!              COLV(TNUM,6,LOOPING(TNUM)) = EMELPSO2(CURIYR,J)
!              COLV(TNUM,7,LOOPING(TNUM)) = EGSO2(J)
!           IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!                COLVALS(:,:) = COLV(TNUM,:,:)
!                    CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!                CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!                LOOPING(TNUM) = 0
!              ENDIF
!           ENDIF
         END DO
!
!           --- END efd_DSO2A ---
!
!        WRITE OUT MARGINAL COST VARIABLES BY SEASON AND LOAD SLICE
!
         DO IS = 1 , EENSP
           IF (USW_DBS .GT. 0) THEN
             WRITE(UF_DBS,3700)COL,CURIYR,COL,IRG,COL,IS,COL,CURITR, &
                 COL,ELPEAK(IS),COL,EETIME(IS),COL,EEITAJ(IS), &
                 COL,TRNCSTEX(IS),COL,TRNCSTIM(IS),COL,TRIM(SCEN_DATE)
           ENDIF
!
!            --- START efd_DSEASINFO ---
!
!            TNUM = 9
!            IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
!               IF (LOOPING(TNUM) .EQ. 0) THEN
!                 NUMCOLS(TNUM) = 9
!                 DYNSTM(TNUM) = 'INSERT INTO EFD_DSEASINFO VALUES(?,?,?,?,?,?,?,?,?,?)'
!               ENDIF
!               LOOPING(TNUM) = LOOPING(TNUM) + 1
!               COLV(TNUM,1,LOOPING(TNUM)) = CURIYR
!               COLV(TNUM,2,LOOPING(TNUM)) = IRG
!               COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!               COLV(TNUM,4,LOOPING(TNUM)) = IS
!               COLV(TNUM,5,LOOPING(TNUM)) = ELPEAK(IS)
!               COLV(TNUM,6,LOOPING(TNUM)) = EETIME(IS)
!               COLV(TNUM,7,LOOPING(TNUM)) = EEITAJ(IS)
!               COLV(TNUM,8,LOOPING(TNUM)) = TRNCSTEX(IS)
!               COLV(TNUM,9,LOOPING(TNUM)) = TRNCSTIM(IS)
!              IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!                COLVALS(:,:) = COLV(TNUM,:,:)
!                    CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!                CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!                LOOPING(TNUM) = 0
!               ENDIF
!            ENDIF
!
!            --- END efd_DSEASINFO ---
!
             DO ILD = 1 , ELNVCT(IS)
               IF ((USW_DBS .GT. 0) .OR. (ORCLEFD .EQ. 1)) THEN
!              IF (((USW_DBS .GT. 0) .OR. (ORCLEFD .EQ. 1)) .AND. &
!                  (USW_DBGRP(CURIYR) .EQ. 1)) THEN
                  IF (USW_DBS .GT. 0) THEN
                    WRITE(UF_DBS,3300) COL,IYR,COL,IRG,COL,CURITR,COL, &
                     IS,COL,ILD,COL,ELGRP(ILD,IS),COL,ELSEG(ILD,IS), &
                     COL,NMARCST(IRG,IS,ILD,IYR),COL, &
                     NMARREG(IRG,IS,ILD,IYR),COL, &
                     NMARTYP(IRG,IS,ILD,IYR),COL,ELHGHT(ILD,IS),COL, &
                     ELWDTH(ILD,IS),COL,TRIM(SCEN_DATE)
                  ENDIF
!
!              --- START efd_DLOAD ---
!
                  TNUM = 10
                  IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
                    IF (LOOPING(TNUM) .EQ. 0) THEN
                     NUMCOLS(TNUM) = 16
                     DYNSTM(TNUM) = 'INSERT INTO EFD_DLOAD VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
                     WRTSTM(TNUM) = 'EFD_DLOAD'
                    ENDIF
                    LOOPING(TNUM) = LOOPING(TNUM) + 1
                    COLV(TNUM,1,LOOPING(TNUM)) = IYR
                    COLV(TNUM,2,LOOPING(TNUM)) = IRG
                    COLV(TNUM,3,LOOPING(TNUM)) = CURITR
                    COLV(TNUM,4,LOOPING(TNUM)) = IS
                    COLV(TNUM,5,LOOPING(TNUM)) = ILD
                    COLV(TNUM,6,LOOPING(TNUM)) = EFD_SLICE_ID(ELGRP(ILD,IS),ELSEG(ILD,IS))
                    COLV(TNUM,7,LOOPING(TNUM)) = ELGRP(ILD,IS)
                    COLV(TNUM,8,LOOPING(TNUM)) = ELSEG(ILD,IS)
                    COLV(TNUM,9,LOOPING(TNUM)) = NMARCST(IRG,IS,ILD,IYR)
                    COLV(TNUM,10,LOOPING(TNUM)) = ELHGHT(ILD,IS)
                    COLV(TNUM,11,LOOPING(TNUM)) = ELWDTH(ILD,IS)
                    COLV(TNUM,12,LOOPING(TNUM)) = UTHTP2(ELSEG(ILD,IS),ELGRP(ILD,IS),IRG)
                    COLV(TNUM,13,LOOPING(TNUM)) = UTHTS2(ELSEG(ILD,IS),ELGRP(ILD,IS),IRG)
                    COLV(TNUM,14,LOOPING(TNUM)) = SP_RES_REQ(ELGRP(ILD,IS),ELSEG(ILD,IS),IRG)
                    COLV(TNUM,15,LOOPING(TNUM)) = SR_INT_REQ(ELGRP(ILD,IS),ELSEG(ILD,IS),IRG)
                    COLV(TNUM,16,LOOPING(TNUM)) = SP_RES_DUAL(ELGRP(ILD,IS),ELSEG(ILD,IS),IRG)
!
                    IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
                      COLVALS(:,:) = COLV(TNUM,:,:)
                      CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!                     CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                       write(6,*)  ' calling write db in efddload ',wrtstm(tnum),numcols(tnum)
                      CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                      LOOPING(TNUM) = 0
                    ENDIF
                  ENDIF
!
!              --- END efd_DLOAD ---
!
                ENDIF
             END DO
         END DO
!
 3300    FORMAT(1X,'DLOAD',7(A2,I2),A2,F9.3,A2,I2,A2,F3.0,A2, &
                          2(F12.3,A2),A)
 3700    FORMAT(1X,'DSEASINFO',4(A2,I2),A2,5(F12.3,A2),A)
!
!        WRITE OUT ANNUAL REGIONAL TRADE DATA TABLE
!
           IF (USW_DBS .GT. 0) THEN
             WRITE(UF_DBS,3400) COL,IYR,COL,IRG,COL,CURITR,COL, &
                  ETDMMF,COL,ETDMME,COL,ETDMDF,COL,ETDMDE,COL, &
                  ETIMPF,COL,ETIMPE,COL,ETIMPD,COL,ETEXPF,COL, &
                  ETEXPE,COL,ETDMPE,COL,ETEXPD,COL,TRIM(SCEN_DATE)
           ENDIF
 3400    FORMAT(1X,'DTRADE',3(A2,I2),A2,11(F12.3,A2),A)
!
!        --- START efd_DTRADE ---
!
         TNUM = 11
         IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
            IF (LOOPING(TNUM) .EQ. 0) THEN
              NUMCOLS(TNUM) = 14
              DYNSTM(TNUM) = 'INSERT INTO EFD_DTRADE VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
              WRTSTM(TNUM) = 'EFD_DTRADE'
            ENDIF
            LOOPING(TNUM) = LOOPING(TNUM) + 1
            COLV(TNUM,1,LOOPING(TNUM)) = IYR
            COLV(TNUM,2,LOOPING(TNUM)) = IRG
            COLV(TNUM,3,LOOPING(TNUM)) = CURITR
            COLV(TNUM,4,LOOPING(TNUM)) = ETDMMF
            COLV(TNUM,5,LOOPING(TNUM)) = ETDMME
            COLV(TNUM,6,LOOPING(TNUM)) = ETDMDF
            COLV(TNUM,7,LOOPING(TNUM)) = ETDMDE
            COLV(TNUM,8,LOOPING(TNUM)) = ETIMPF
            COLV(TNUM,9,LOOPING(TNUM)) = ETIMPE
            COLV(TNUM,10,LOOPING(TNUM)) = ETIMPD
            COLV(TNUM,11,LOOPING(TNUM)) = ETEXPF
            COLV(TNUM,12,LOOPING(TNUM)) = ETEXPE
            COLV(TNUM,13,LOOPING(TNUM)) = ETDMPE
            COLV(TNUM,14,LOOPING(TNUM)) = ETEXPD
            IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
              COLVALS(:,:) = COLV(TNUM,:,:)
              CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!             CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
              CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
              LOOPING(TNUM) = 0
            ENDIF
         ENDIF
!
!        --- END efd_DTRADE ---
!
!        Collect Resource Cost Data - T_DomEcon,T_DomFirm,T_Intexp,T_Intimp
!
         T_DomEcon(IRG,CURIYR) = T_DomEcon(IRG,CURIYR) + ETDMDE
         T_DomFirm(IRG,CURIYR) = T_DomFirm(IRG,CURIYR) + ETDMDF
         T_Intexp(IRG,CURIYR) = T_Intexp(IRG,CURIYR) + ETEXPD
         T_Intimp(IRG,CURIYR) = T_Intimp(IRG,CURIYR) + ETIMPD

         T_DomEcon(MNUMNR,CURIYR) = T_DomEcon(MNUMNR,CURIYR) + ETDMDE
         T_DomFirm(MNUMNR,CURIYR) = T_DomFirm(MNUMNR,CURIYR) + ETDMDF
         T_Intexp(MNUMNR,CURIYR) = T_Intexp(MNUMNR,CURIYR) + ETEXPD
         T_Intimp(MNUMNR,CURIYR) = T_Intimp(MNUMNR,CURIYR) + ETIMPD
!
!        WRITE DETAILED FIRM TRADE DATA TO ETT FILE AND DATABASE FILE
!
         DO KRG = 1 , MNUMNR + EFD_D_PROV
           IF ( UTFIRM(IRG,KRG) .NE. 0.0 ) THEN
             write(uf_ett,2591) ' trdrep firm ',curiyr,curitr,irg,krg,utfirm(irg,krg) * .001,utfirm(irg,krg)*.001
             IF (USW_DBS .GT. 0) THEN
               WRITE(UF_DBS,3452) COL,CURIYR,COL,CURITR,COL, &
                    IRG,COL,KRG,COL,UTFIRM(IRG,KRG)*.001,COL,    &
                    UTFIRM(IRG,KRG)*.001,COL,TRIM(SCEN_DATE)
             ENDIF
           ENDIF
           IF ( UTFIRM(KRG,IRG) .NE. 0.0 ) THEN
             IF ( KRG .GT. MNUMNR ) THEN
               write(uf_ett,2591) ' trdrep firm ',curiyr,curitr,krg,irg,utfirm(krg,irg) * .001,utfirm(krg,irg)*.001
               IF (USW_DBS .GT. 0) THEN
                 WRITE(UF_DBS,3452) COL,CURIYR,COL,CURITR,COL, &
                    KRG,COL,IRG,COL,UTFIRM(KRG,IRG)*.001,COL,    &
                    UTFIRM(KRG,IRG)*.001,COL,TRIM(SCEN_DATE)
               ENDIF
             ENDIF
           ENDIF
2591  FORMAT(a,4(I4,2X),2(F15.3,2x))
3452  FORMAT(1X,'DETAILEDTRADE',4(A2,I2),A2,2(F15.3,A2),A)
!
           TRDTYPE = 'FIRM'
           TNUM = 12
           IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
             IF (LOOPING(TNUM) .EQ. 0) THEN
               NUMCOLS(TNUM) = 9
               DYNSTM(TNUM) = 'INSERT INTO EFD_DETAILED_TRADE VALUES(?,?,?,?,?,?,?,?,?,?)'
               WRTSTM(TNUM) = 'EFD_DETAILED_TRADE'
             ENDIF
             IF ( UTFIRM(IRG,KRG) .NE. 0.0 ) THEN
               LOOPING(TNUM) = LOOPING(TNUM) + 1
               COLV(TNUM,1,LOOPING(TNUM)) = CURIYR
               COLV(TNUM,2,LOOPING(TNUM)) = CURITR
               CHCOLV(TNUM,3,LOOPING(TNUM)) = TRDTYPE
               COLV(TNUM,4,LOOPING(TNUM)) = IRG
               COLV(TNUM,5,LOOPING(TNUM)) = KRG
               COLV(TNUM,6,LOOPING(TNUM)) = UTFIRM(IRG,KRG) * .001
               COLV(TNUM,7,LOOPING(TNUM)) = UTFIRM(IRG,KRG) * .001
               IF ( KRG .LT. MNUMNR ) THEN
                 COLV(TNUM,8,LOOPING(TNUM)) = (ZTEXDF(KRG) / ZTEXMF(KRG)) * 1000.0
                 COLV(TNUM,9,LOOPING(TNUM)) = (ZTEXDF(KRG) / ZTEXMF(KRG)) * 1000.0
               ELSEIF ( KRG .GT. MNUMNR ) THEN
                 COLV(TNUM,8,LOOPING(TNUM)) = ( FTIMPD(IRG) + CTIMPD(IRG) ) / ETIMPF
                 COLV(TNUM,9,LOOPING(TNUM)) = ( FTIMPD(IRG) + CTIMPD(IRG) ) / ETIMPF
               ENDIF
!
               IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
                 COLVALS(:,:) = COLV(TNUM,:,:)
                 CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!                CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                 CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                 LOOPING(TNUM) = 0
               ENDIF
             ENDIF
! firm exports to canada and mexico
             IF ( UTFIRM(KRG,IRG) .NE. 0.0 ) THEN
               IF ( KRG .GT. MNUMNR ) THEN
                 LOOPING(TNUM) = LOOPING(TNUM) + 1
                 COLV(TNUM,1,LOOPING(TNUM)) = CURIYR
                 COLV(TNUM,2,LOOPING(TNUM)) = CURITR
                 CHCOLV(TNUM,3,LOOPING(TNUM)) = TRDTYPE
                 COLV(TNUM,4,LOOPING(TNUM)) = KRG
                 COLV(TNUM,5,LOOPING(TNUM)) = IRG
                 COLV(TNUM,6,LOOPING(TNUM)) = UTFIRM(KRG,IRG) * .001
                 COLV(TNUM,7,LOOPING(TNUM)) = UTFIRM(KRG,IRG) * .001
                 COLV(TNUM,8,LOOPING(TNUM)) = ( FTEXPD(IRG) + CTEXPD(IRG) ) / ETEXPF
                 COLV(TNUM,9,LOOPING(TNUM)) = ( FTEXPD(IRG) + CTEXPD(IRG) ) / ETEXPF
!
                 IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
                   COLVALS(:,:) = COLV(TNUM,:,:)
                   CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!                  CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                   CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                   LOOPING(TNUM) = 0
                 ENDIF
               ENDIF
             ENDIF

           ENDIF
         ENDDO
!
!     write out expci data to detailed trade table
!
          TRDTYPE = 'ECON'
          TNUM = 12
          IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
           IF (LOOPING(TNUM) .EQ. 0) THEN
             NUMCOLS(TNUM) = 9
             DYNSTM(TNUM) = 'INSERT INTO EFD_DETAILED_TRADE VALUES(?,?,?,?,?,?,?,?,?,?)'
             WRTSTM(TNUM) = 'EFD_DETAILED_TRADE'
           ENDIF
           IF ( EXPCI(CURIYR,IRG) .GT. 0.0 ) THEN
            DO IPROV = 1 , EFD_D_PROV
              IF ( MAPEXPCI(IRG,IPROV) .NE. 0.0) THEN
                write(uf_ett,2591) ' trdrep econ ',curiyr,curitr,iprov+mnumnr,irg,                 &
                  (EXPCI(curiYR,irg)*MAPEXPCI(irg,iPROV)*.001),(EXPCI(CuriYR,IRG)*MAPEXPCI(irg,iPROV)*.001)
!               write(6,*) ' trdrep econ ',curiyr,curitr,iprov,iprov+mnumnr,irg,               &
!                 (EXPCI(curiYR,irg)*MAPEXPCI(irg,IPROV)*.001)
!
                LOOPING(TNUM) = LOOPING(TNUM) + 1
                COLV(TNUM,1,LOOPING(TNUM)) = CURIYR
                COLV(TNUM,2,LOOPING(TNUM)) = CURITR
                CHCOLV(TNUM,3,LOOPING(TNUM)) = TRDTYPE
                COLV(TNUM,4,LOOPING(TNUM)) = IPROV + MNUMNR
                COLV(TNUM,5,LOOPING(TNUM)) = IRG
                COLV(TNUM,6,LOOPING(TNUM)) = EXPCI(CURIYR,IRG)*MAPEXPCI(IRG,IPROV) * .001
                COLV(TNUM,7,LOOPING(TNUM)) = EXPCI(CURIYR,IRG)*MAPEXPCI(IRG,IPROV) * .001
                COLV(TNUM,8,LOOPING(TNUM)) = ( ETEXPD - FTEXPD(IRG) - CTEXPD(IRG) ) / ETEXPE
                COLV(TNUM,9,LOOPING(TNUM)) = ( ETEXPD - FTEXPD(IRG) - CTEXPD(IRG) ) / ETEXPE
!
                IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
                  COLVALS(:,:) = COLV(TNUM,:,:)
                  CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!                 CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                  CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                  LOOPING(TNUM) = 0
                ENDIF
!
              ENDIF
            ENDDO
           ENDIF
          ENDIF
!
!     IF first emm year print out 1990 to curiyr expci data to detailed trade table
!
!       IF ( FULLYR .EQ. UESTYR) THEN
!         DO IYR = 1 , CURIYR -1
!         IF ( EXPCI(IYR,IRG) .GT. 0.0 ) THEN
!           DO IPROV = 1 , EFD_D_PROV
!             IF ( MAPEXPCI(IRG,IPROV) .NE. 0.0) THEN
!               write(uf_ett,2591) ' trdrep econ ',iyr,curitr,iprov+mnumnr,irg,                 &
!                 (EXPCI(iYR,irg)*MAPEXPCI(irg,iPROV)*.001),(EXPCI(iYR,IRG)*MAPEXPCI(irg,iPROV)*.001)
!               write(6,*) ' trdrep econ ',iyr,curitr,iprov,iprov+mnumnr,irg,               &
!                 (EXPCI(iYR,irg)*MAPEXPCI(irg,IPROV)*.001)
!
!               TNUM = 12
!               LOOPING(TNUM) = LOOPING(TNUM) + 1
!               COLV(TNUM,1,LOOPING(TNUM)) = IYR
!               COLV(TNUM,2,LOOPING(TNUM)) = CURITR
!               CHCOLV(TNUM,3,LOOPING(TNUM)) = TRDTYPE
!               COLV(TNUM,4,LOOPING(TNUM)) = IPROV + MNUMNR
!               COLV(TNUM,5,LOOPING(TNUM)) = IRG
!               COLV(TNUM,6,LOOPING(TNUM)) = EXPCI(IYR,IRG)*MAPEXPCI(IRG,IPROV) * .001
!               COLV(TNUM,7,LOOPING(TNUM)) = EXPCI(IYR,IRG)*MAPEXPCI(IRG,IPROV) * .001
!               COLV(TNUM,8,LOOPING(TNUM)) = 0.0
!               COLV(TNUM,9,LOOPING(TNUM)) = 0.0
!
!               IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!                 COLVALS(:,:) = COLV(TNUM,:,:)
!                 CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!                 CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!                 LOOPING(TNUM) = 0
!               ENDIF
!
!             ENDIF
!           ENDDO
!         ENDIF
!        ENDDO
!      ENDIF
!
! write economy trade data from EFD
            DO KRG=1, MNUMNR + EFD_D_PROV
               IF ( UTECON(IRG,KRG) .GT. 0.0 ) THEN
                 write(uf_ett,2595) ' trdrep econ ',curiyr,curitr,irg,krg,utecon(irg,krg),   &
                    utecon(irg,krg)/(1.0 - lineloss)
!
                TNUM = 12
                IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
                  IF (LOOPING(TNUM) .EQ. 0) THEN
                   NUMCOLS(TNUM) = 9
                   DYNSTM(TNUM) = 'INSERT INTO EFD_DETAILED_TRADE VALUES(?,?,?,?,?,?,?,?,?,?)'
                   WRTSTM(TNUM) = 'EFD_DETAILED_TRADE'
                  ENDIF

                  LOOPING(TNUM) = LOOPING(TNUM) + 1
                  COLV(TNUM,1,LOOPING(TNUM)) = CURIYR
                  COLV(TNUM,2,LOOPING(TNUM)) = CURITR
                  CHCOLV(TNUM,3,LOOPING(TNUM)) = TRDTYPE
                  COLV(TNUM,4,LOOPING(TNUM)) = IRG
                  COLV(TNUM,5,LOOPING(TNUM)) = KRG
                  COLV(TNUM,6,LOOPING(TNUM)) = UTECON(IRG,KRG)
                  COLV(TNUM,7,LOOPING(TNUM)) = UTECON(IRG,KRG)/(1.0 - LINELOSS)
                  IF ( KRG .LT. MNUMNR ) THEN
                    COLV(TNUM,8,LOOPING(TNUM)) = -(UTSALES(IRG,KRG)/UTECON(IRG,KRG))
                    COLV(TNUM,9,LOOPING(TNUM)) = -(UTSALES(IRG,KRG)/UTECON(IRG,KRG))
                  ELSEIF ( KRG .GT. MNUMNR ) THEN
                    COLV(TNUM,8,LOOPING(TNUM)) = ( ETIMPD - FTIMPD(IRG) - CTIMPD(IRG) ) / ETIMPE
                    COLV(TNUM,9,LOOPING(TNUM)) = (UTSALES(IRG,KRG)/UTECON(IRG,KRG))
                  ENDIF
!
                  IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
                    COLVALS(:,:) = COLV(TNUM,:,:)
                    CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!                   CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                    CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                    LOOPING(TNUM) = 0
                  ENDIF
!
                ENDIF
               ENDIF
            END DO
2590  FORMAT(I4,1X,I4,1X,A5,2(I4,7X),F4.1,F10.3,2F4.1,F10.3,5F4.1)
2595  FORMAT(a,4(I4,2X),2(F15.3,2x))
! write economy trade data by load slice,etc from EFD
            DO KRG=1, MNUMNR + EFD_D_PROV
              DO IS = 1, EENSP
                DO ISL = 1 , ELNVCT(IS)
                 GRP = ELGRP(ISL,IS)
                 SEG = ELSEG(ISL,IS)
                 FSL = EFD_Slice_ID(GRP,SEG)

                 IF ( CNSTRNTS_EFD(IS,CURIYR,IRG,KRG) .GT. 0.0 ) THEN
                   write(uf_ett,2595) ' trdrep econ ',curiyr,curitr,irg,krg,utecon(irg,krg),   &
                      utecon(irg,krg)/(1.0 - lineloss)
!
                  TNUM = 13
                  IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
                    IF (LOOPING(TNUM) .EQ. 0) THEN
                     NUMCOLS(TNUM) = 15
                     DYNSTM(TNUM) = 'INSERT INTO EFD_DETAILED_ECON_TRADE VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
                     WRTSTM(TNUM) = 'EFD_DETAILED_ECON_TRADE'
                    ENDIF

                    LOOPING(TNUM) = LOOPING(TNUM) + 1
                    COLV(TNUM,1,LOOPING(TNUM)) = CURIYR
                    COLV(TNUM,2,LOOPING(TNUM)) = CURITR
                    COLV(TNUM,3,LOOPING(TNUM)) = IS
                    COLV(TNUM,4,LOOPING(TNUM)) = ISL
                    COLV(TNUM,5,LOOPING(TNUM)) = GRP
                    COLV(TNUM,6,LOOPING(TNUM)) = SEG
                    COLV(TNUM,7,LOOPING(TNUM)) = UTWDTH(SEG,GRP)
                    COLV(TNUM,8,LOOPING(TNUM)) = IRG
                    COLV(TNUM,9,LOOPING(TNUM)) = KRG
                    COLV(TNUM,10,LOOPING(TNUM)) = UTECONSL(IS,ISL,IRG,KRG) * UTWDTH(SEG,GRP)
                    COLV(TNUM,11,LOOPING(TNUM)) = UTECONSL(IS,ISL,IRG,KRG)
                    COLV(TNUM,12,LOOPING(TNUM)) = CNSTRNTS_EFD(IS,CURIYR,IRG,KRG)
                    COLV(TNUM,13,LOOPING(TNUM)) = CNSTRNTS_EFD(IS,CURIYR,IRG,KRG) - UTECONSL(IS,ISL,IRG,KRG)
                    COLV(TNUM,14,LOOPING(TNUM)) = CNSTRNTS_PREFIRM_EFD(IS,CURIYR,IRG,KRG)
                    IF ( ( KRG .LT. MNUMNR ) .AND. (IRG .LT. MNUMNR) ) THEN
                      COLV(TNUM,15,LOOPING(TNUM)) = UTNEWTRN(KRG,IRG,CURIYR)
                    ELSE
                      COLV(TNUM,15,LOOPING(TNUM)) = 0.0
                    ENDIF
!
                    IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
                      COLVALS(:,:) = COLV(TNUM,:,:)
                      CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!                     CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                      CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                      LOOPING(TNUM) = 0
                    ENDIF
!o

                  ENDIF
                 ENDIF
                ENDDO
              ENDDO
            END DO

!
!        WRITE OUT ANNUAL REGIONAL EMISSIONS TABLE
!
           IF (USW_DBS .GT. 0) THEN
             WRITE(UF_DBS,3500) COL,IYR,COL,IRG,COL,CURITR,COL, &
                  ETALLW,COL,ETSO2,COL,ETNOX,COL,ETCO2,COL,   &
                  ETCAR,COL,ETCO1,COL,ETVOC,COL,ETHG,COL,ERSO2,COL,TRIM(SCEN_DATE)
           ENDIF
 3500    FORMAT(1X,'DEMIS',3(A2,I2),A2,8(F12.1,A2),F12.4,A2,A)
!
!        --- START efd_DEMIS ---
!
!        TNUM = 14
!        IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
!           IF (LOOPING(TNUM) .EQ. 0) THEN
!              NUMCOLS(TNUM) = 12
!              DYNSTM(TNUM) = 'INSERT INTO EFD_DEMIS VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?)'
!           ENDIF
!           LOOPING(TNUM) = LOOPING(TNUM) + 1
!           COLV(TNUM,1,LOOPING(TNUM)) = IYR
!           COLV(TNUM,2,LOOPING(TNUM)) = IRG
!           COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!           COLV(TNUM,4,LOOPING(TNUM)) = ETALLW
!           COLV(TNUM,5,LOOPING(TNUM)) = ETSO2
!           COLV(TNUM,6,LOOPING(TNUM)) = ETNOX
!           COLV(TNUM,7,LOOPING(TNUM)) = ETCO2
!           COLV(TNUM,8,LOOPING(TNUM)) = ETCAR
!           COLV(TNUM,9,LOOPING(TNUM)) = ETCO1
!           COLV(TNUM,10,LOOPING(TNUM)) = ETVOC
!           COLV(TNUM,11,LOOPING(TNUM)) = ETHG
!           COLV(TNUM,12,LOOPING(TNUM)) = ERSO2
!           IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!             COLVALS(:,:) = COLV(TNUM,:,:)
!                    CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!             CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!             LOOPING(TNUM) = 0
!           ENDIF
!        ENDIF
!
!        --- END efd_DEMIS ---
!
!        WRITE OUT ANNUAL REGIONAL MISC??    TABLE
!
          IF (USW_DBS .GT. 0) THEN
             WRITE(UF_DBS,3600) COL,IYR,COL,IRG,COL,CURITR,COL, &
                EEMRM,COL,ETGEN*.001,COL,EWGOWN*.001,COL,EWGREV,COL,   &
                EWGFIX,COL,EWGRCC*.001,COL,EWGRIC*.001,COL,EWGRNW*.001, &
                COL,EWGRXO,COL,EWGRYO,COL,PPCAP,COL,PPOM,COL, &
                PPFUEL,COL,ERRPS,COL,TRIM(SCEN_DATE)
          ENDIF
 3600    FORMAT(1X,'DOS',3(A2,I2),A2,14(F12.3,A2),A)
!
!        --- START efd_DOS ---
!
!        Collect Data Resource Cost Calculation - EWGRCC,EWGRIC,RPSCST
!
         Total_RCC(IRG,CURIYR) = Total_RCC(IRG,CURIYR) + EWGRCC
         Total_RIC(IRG,CURIYR) = Total_RIC(IRG,CURIYR) + EWGRIC
         Total_RPS(IRG,CURIYR) = Total_RPS(IRG,CURIYR) + ERRPS

         Total_RCC(MNUMNR,CURIYR) = Total_RCC(MNUMNR,CURIYR) + EWGRCC
         Total_RIC(MNUMNR,CURIYR) = Total_RIC(MNUMNR,CURIYR) + EWGRIC
         Total_RPS(MNUMNR,CURIYR) = Total_RPS(MNUMNR,CURIYR) + ERRPS
!
         TNUM = 15
         IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
            IF (LOOPING(TNUM) .EQ. 0) THEN
              NUMCOLS(TNUM) = 17
              DYNSTM(TNUM) = 'INSERT INTO EFD_DOS VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
              WRTSTM(TNUM) = 'EFD_DOS'
            ENDIF
            LOOPING(TNUM) = LOOPING(TNUM) + 1
            COLV(TNUM,1,LOOPING(TNUM)) = IYR
            COLV(TNUM,2,LOOPING(TNUM)) = IRG
            COLV(TNUM,3,LOOPING(TNUM)) = CURITR
            COLV(TNUM,4,LOOPING(TNUM)) = EEMRM
            COLV(TNUM,5,LOOPING(TNUM)) = ETGEN
            COLV(TNUM,6,LOOPING(TNUM)) = EWGOWN
            COLV(TNUM,7,LOOPING(TNUM)) = EWGREV
            COLV(TNUM,8,LOOPING(TNUM)) = EWGFIX
            COLV(TNUM,9,LOOPING(TNUM)) = EWGRCC
            COLV(TNUM,10,LOOPING(TNUM)) = EWGRIC
            COLV(TNUM,11,LOOPING(TNUM)) = EWGRNW
            COLV(TNUM,12,LOOPING(TNUM)) = EWGRXO
            COLV(TNUM,13,LOOPING(TNUM)) = EWGRYO
            COLV(TNUM,14,LOOPING(TNUM)) = PPCAP
            COLV(TNUM,15,LOOPING(TNUM)) = PPOM
            COLV(TNUM,16,LOOPING(TNUM)) = PPFUEL
            COLV(TNUM,17,LOOPING(TNUM)) = ERRPS
            IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
              COLVALS(:,:) = COLV(TNUM,:,:)
              CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!             CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
              CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
              LOOPING(TNUM) = 0
            ENDIF
         ENDIF
!
!        --- END efd_DOS ---
!
!!       Write out CPP Impact Table
        IF ( (CURIYR + UHBSYR) .GE. (CO2_STDBY + 1) ) THEN
         TNUM = 16
         IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
            IF (LOOPING(TNUM) .EQ. 0) THEN
              NUMCOLS(TNUM) = 11
              DYNSTM(TNUM) = 'INSERT INTO CPP_IMPACTS VALUES(?,?,?,?,?,?,?,?,?,?,?,?)'
              WRTSTM(TNUM) = 'CPP_IMPACTS'
            ENDIF
            LOOPING(TNUM) = LOOPING(TNUM) + 1
            COLV(TNUM,1,LOOPING(TNUM)) = IYR
            COLV(TNUM,2,LOOPING(TNUM)) = CURITR
            COLV(TNUM,3,LOOPING(TNUM)) = IRG
            COLV(TNUM,4,LOOPING(TNUM)) = ECO2NRRT(IRG,IYR)                                                  ! Intensity Rate Target
            IF ( (EGENNRQF(IRG,IYR) + EGENNREE(IRG,IYR)) .NE. 0.0 ) THEN
              COLV(TNUM,5,LOOPING(TNUM)) = MAX(0.0,ECO2NRQF(IRG,IYR) * 1000.0 / (EGENNRQF(IRG,IYR) + EGENNREE(IRG,IYR)))  ! Intensity Rate Achieved
            ELSE
              COLV(TNUM,5,LOOPING(TNUM)) = 0.0                                                                ! Intensity Rate Achieved
            ENDIF
            COLV(TNUM,6,LOOPING(TNUM)) = ECO2NRQY(IRG,IYR)                                                  ! CO2 Emiss Target for Affected Plants
            COLV(TNUM,7,LOOPING(TNUM)) = ECO2NRQF(IRG,IYR) / 2.204                                          ! CO2 Emiss From Affected Plants
            COLV(TNUM,8,LOOPING(TNUM)) = ECO2NRTL(IRG,IYR) / 2.204                                          ! CO2 Emiss From All Plants
            COLV(TNUM,9,LOOPING(TNUM)) = EGENNREE(IRG,IYR)                                               ! Generation savings from energy efficiency
            COLV(TNUM,10,LOOPING(TNUM)) = ECO2NRPP(IRG,IYR) * 2.2040 * SCALPR                            ! ECP Intensity Standard Allowance Price
            COLV(TNUM,11,LOOPING(TNUM)) = ECO2NRPR(IRG,IYR) * 2204.0 * SCALPR                            ! EFD Intensity Standard Allowance Price

!  IF first region write national numbers also

            IF ( IRG .EQ. 1 ) THEN
              LOOPING(TNUM) = LOOPING(TNUM) + 1
              COLV(TNUM,1,LOOPING(TNUM)) = IYR
              COLV(TNUM,2,LOOPING(TNUM)) = CURITR
              COLV(TNUM,3,LOOPING(TNUM)) = MNUMNR
              COLV(TNUM,4,LOOPING(TNUM)) = ECO2NRRT(MNUMNR,IYR)                                                  ! Intensity Rate Target
              IF ( (EGENNRQF(MNUMNR,IYR) + EGENNREE(MNUMNR,IYR)) .NE. 0.0 ) THEN
                COLV(TNUM,5,LOOPING(TNUM)) = ECO2NRQF(MNUMNR,IYR) * 1000.0 / (EGENNRQF(MNUMNR,IYR) + EGENNREE(MNUMNR,IYR))  ! Intensity Rate Achieved
              ELSE
                COLV(TNUM,5,LOOPING(TNUM)) = 0.0                                                                ! Intensity Rate Achieved
              ENDIF
              COLV(TNUM,6,LOOPING(TNUM)) = ECO2NRQY(MNUMNR,IYR)                                                  ! CO2 Emiss Target for Affected Plants
              COLV(TNUM,7,LOOPING(TNUM)) = ECO2NRQF(MNUMNR,IYR) / 2.204                                          ! CO2 Emiss From Affected Plants
              COLV(TNUM,8,LOOPING(TNUM)) = ECO2NRTL(MNUMNR,IYR) / 2.204                                          ! CO2 Emiss From All Plants
              COLV(TNUM,9,LOOPING(TNUM)) = EGENNREE(MNUMNR,IYR)                                               ! Generation savings from energy efficiency
              COLV(TNUM,10,LOOPING(TNUM)) = ECO2NRPP(MNUMNR,IYR) * 2.2040 * SCALPR                            ! ECP Intensity Standard Allowance Price
              COLV(TNUM,11,LOOPING(TNUM)) = ECO2NRPR(MNUMNR,IYR) * 2204.0 * SCALPR                            ! EFD Intensity Standard Allowance Price
            ENDIF

            IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
              COLVALS(:,:) = COLV(TNUM,:,:)
              CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!             CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
              CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
              LOOPING(TNUM) = 0
            ENDIF
         ENDIF
        ENDIF
!
         TNUM = 17
         IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
            IF (LOOPING(TNUM) .EQ. 0) THEN
              NUMCOLS(TNUM) = 13
              DYNSTM(TNUM) = 'INSERT INTO EFD_SPIN_RES_ACH VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
              WRTSTM(TNUM) = 'EFD_SPIN_RES_ACH'
            ENDIF
            DO IECP = 1 , ECP_D_CAP
              ICRROW = 0
              DO IGRP = 1 , EFDNUMSG
                DO ISEG = 1 , EFDSGDNB(IGRP)
                  IF ( SP_ACHBYECP(IGRP,ISEG,IRG,IECP) .NE. 0.0 ) THEN
                    ICRROW = 1
                  ENDIF
                ENDDO
              ENDDO
              IF ( ICRROW .EQ. 1 ) THEN
                  LOOPING(TNUM) = LOOPING(TNUM) + 1
                  COLV(TNUM,1,LOOPING(TNUM)) = IYR
                  COLV(TNUM,2,LOOPING(TNUM)) = CURITR
                  COLV(TNUM,3,LOOPING(TNUM)) = IRG
                  COLV(TNUM,4,LOOPING(TNUM)) = IECP
                  COLV(TNUM,5,LOOPING(TNUM)) = SP_ACHBYECP(1,1,IRG,IECP)
                  COLV(TNUM,6,LOOPING(TNUM)) = SP_ACHBYECP(1,2,IRG,IECP)
                  COLV(TNUM,7,LOOPING(TNUM)) = SP_ACHBYECP(1,3,IRG,IECP)
                  COLV(TNUM,8,LOOPING(TNUM)) = SP_ACHBYECP(2,1,IRG,IECP)
                  COLV(TNUM,9,LOOPING(TNUM)) = SP_ACHBYECP(2,2,IRG,IECP)
                  COLV(TNUM,10,LOOPING(TNUM)) = SP_ACHBYECP(2,3,IRG,IECP)
                  COLV(TNUM,11,LOOPING(TNUM)) = SP_ACHBYECP(3,1,IRG,IECP)
                  COLV(TNUM,12,LOOPING(TNUM)) = SP_ACHBYECP(3,2,IRG,IECP)
                  COLV(TNUM,13,LOOPING(TNUM)) = SP_ACHBYECP(3,3,IRG,IECP)

                  IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
                   COLVALS(:,:) = COLV(TNUM,:,:)
                   CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!                  CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                   CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                   LOOPING(TNUM) = 0
                 ENDIF

              ENDIF
            ENDDO

         ENDIF

       ENDDO    !End region do


!      --- WRITE OUT ANY LEFT OVER RECORDS TO THE NEMS DATA BASE
       IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        DO TNUM = 1 , NUMTABS
         IF (LOOPING(TNUM) .NE. 0) THEN
           COLVALS(:,:) = COLV(TNUM,:,:)
           CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!          CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!   write(6,*) ' calling write_db from left over dload ',tnum,wrtstm(tnum),numcols(tnum)
           CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
           LOOPING(TNUM) = 0
         ENDIF
        ENDDO
       ENDIF
!
!     print *,'!hgt,p,f',curiyr+1989,tlhg,tlhgp,tlhgf
!
      RETURN
      END
!
!
      SUBROUTINE WRPLNT
!
      IMPLICIT NONE
!
!     THIS SUBROUTINE WRITES THE PLANT FILE TO THE OUTPUT ACCESS DATABASE IN FIRST AND LAST YEAR OF RUN

      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'control'
      include 'plntctl'
      include 'plntin'
      include 'eusprc'
      include 'edbdef'
!
      INTEGER I,IRECL,JRG,IGRP,ITMP,IOWN,IBBN,ICOL           ! Indices
      INTEGER NUMTABS
      PARAMETER (NUMTABS = 3)    !total number of database tables written in this subroutine.
      INTEGER RECCOUNT(NUMTABS)
      CHARACTER*2 COL

      LOOPING = 0
      NUMCOLS = 0
      RECCOUNT = 0
      DYNSTM = ' '
      WRTSTM = ' '
      CHCOLVALS = ' '
      CHCOLV = ' '
      COLVALS = 0.0
      COLV = 0.0
!
      COL = ' :'
!
      DO IRECL = 1 , WREC_NXT
        CALL GETPLT(IRECL)
!
        IF (USW_DBS .GT. 0) THEN
          WRITE(UF_DBS,2000) COL,CURIYR,COL,CURITR,COL,W_GRP,COL,W_GRP2, &
                COL,W_IGRP,COL,WVIN,COL,WEFDT,COL,WECPT,COL,WNOWN, &
                COL,WFOWN,COL,WC_NP,COL,WC_SUM,COL, &
                WC_WIN,COL,WHRATE,COL,W_GRID,COL,W_VOM,COL, &
                W_FOM,COL,WSCBEF,COL,W_CF,COL,WSCBCST,COL,WASVL, &
                COL,WBCWP,COL,WPCST,COL,W_SYR,COL,W_SMO,COL, &
                W_RYR,COL,W_RMO,COL,WSCBYR,COL,WSCBGRP,COL, &
                WRFURB,COL,WNOPER,COL,WSTATE,COL, &
                W_CR,COL,W_GR,COL,W_CLRG,COL,WEFPT,COL,CCSRov,COL, &
                CCSf,COL,CCSv,COL,CCShr,COL,W_MRUN,COL, &
                W_CAPAD,COL,WSEQEF,COL,CCSCAPA,COL,CCS_LOC,COL, &
                WNOX_R,COL,WNOX_B4,COL,WCOMB_O,COL,WCOMB_F,COL, &
                WCOMB_V,COL,WCOMB_R,COL,WSNCR_O,COL,WSNCR_F,COL, &
                WSNCR_V,COL,WSNCR_R,COL,WSCR_O,COL,WSCR_F,COL, &
                WSCR_V,COL,WSCR_R,COL,W_CAR,COL,W_COMB,COL, &
                W_POST,COL,WFL(1),COL,WFL(2),COL,WFL(3),COL,TRIM(SCEN_DATE)
        ENDIF
 2000  FORMAT(1X,'DPLT',A2,2(I3,A2),3(I5,A2),5(I3,A2),3(F9.3,A2),F8.1, &
         A2,6(F9.3,A2),3(F12.3,A2),8(I5,A2),2A2,4(I2,A2),F12.3,A2,2(F7.3,A2),F8.1,A2,I4,A2, &
         F12.3,A2,F9.3,A2,F7.4,A2,I5,A2,2(F9.3,A2), &
         3(F12.3,A2,3(F9.3,A2)),6(I2,A2),A)
!      Label:DPLT:CYR:CURITR:W_GRP:W_GRP2:W_IGRP:WVIN:WEFDT:WECPT:WNOWN:WFOWN:WC_NP:WC_SUM:WC_WIN:WHRATE:W_GRID:W_VOM:W_FOM:WSCBEF:W_CF:WSCBCST:WASVL:WBCWP:WPCST:W_SYR:W_SMO:W_RYR:W_RMO:WSCBYR:WSCBGRP:WRFURB:WNOPER:WSTATE:W_CR:W_GR:W_CLRG:WEFPT:CCSROV:CCSF:CCSV:CCSHR:W_MRUN:W_CAPAD:WSEQEF:CCSCAPA:CCS_LOC:WNOX_R:WNOX_B4:WCOMB_O:WCOMB_F:WCOMB_V:WCOMB_R:WSNCR_O:WSNCR_F:WSNCR_V:WSNCR_R:WSCR_O:WSCR_F:WSCR_V:WSCR_R:W_CAR:W_COMB:W_POST:WFL1:WFL2:WFL3:TRIM(SCEN_DATE)
!
!         --- START efd_DOSPT ---
         IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
           TNUM = 1
           IF (LOOPING(TNUM) .EQ. 0) THEN
             NUMCOLS(TNUM) = 87
             DYNSTM(TNUM) = 'INSERT INTO EFD_PLANT_DETAIL_CHAR VALUES(?,?,?,?,?,?,?,?,?,?,'  &
                         //'?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,' &
                         //'?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,'  &
                         //'?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
             WRTSTM(TNUM) = 'EFD_PLANT_DETAIL_CHAR'
           ENDIF
           LOOPING(TNUM) = LOOPING(TNUM) + 1
           RECCOUNT(TNUM) = RECCOUNT(TNUM) + 1
           COLV(TNUM,1,LOOPING(TNUM)) = CURIYR
           COLV(TNUM,2,LOOPING(TNUM)) = CURITR
           COLV(TNUM,3,LOOPING(TNUM)) = RECCOUNT(TNUM)
           COLV(TNUM,4,LOOPING(TNUM)) = W_GRP
           COLV(TNUM,5,LOOPING(TNUM)) = W_GRP2
           COLV(TNUM,6,LOOPING(TNUM)) = W_IGRP
           COLV(TNUM,7,LOOPING(TNUM)) = WVIN
           COLV(TNUM,8,LOOPING(TNUM)) = W_SYR
           COLV(TNUM,9,LOOPING(TNUM)) = W_SMO
           COLV(TNUM,10,LOOPING(TNUM)) = W_RYR
           COLV(TNUM,11,LOOPING(TNUM)) = W_RMO
           COLV(TNUM,12,LOOPING(TNUM)) = WNOPER
           COLV(TNUM,13,LOOPING(TNUM)) = W_CF
           COLV(TNUM,14,LOOPING(TNUM)) = WEFDT
           COLV(TNUM,15,LOOPING(TNUM)) = WECPT
           COLV(TNUM,16,LOOPING(TNUM)) = WNOWN
           COLV(TNUM,17,LOOPING(TNUM)) = WFOWN
           COLV(TNUM,18,LOOPING(TNUM)) = W_CR
           COLV(TNUM,19,LOOPING(TNUM)) = WEFPT
           COLV(TNUM,20,LOOPING(TNUM)) = WC_NP
           COLV(TNUM,21,LOOPING(TNUM)) = WC_SUM
           COLV(TNUM,22,LOOPING(TNUM)) = WC_WIN
           COLV(TNUM,23,LOOPING(TNUM)) = WFL(1)
           COLV(TNUM,24,LOOPING(TNUM)) = WFL(2)
           COLV(TNUM,25,LOOPING(TNUM)) = WFL(3)
           COLV(TNUM,26,LOOPING(TNUM)) = W_FSHR(1)
           COLV(TNUM,27,LOOPING(TNUM)) = W_FSHR(2)
           COLV(TNUM,28,LOOPING(TNUM)) = W_FSHR(3)
           COLV(TNUM,29,LOOPING(TNUM)) = WHRATE
           COLV(TNUM,30,LOOPING(TNUM)) = W_GRID
           COLV(TNUM,31,LOOPING(TNUM)) = W_VOM
           COLV(TNUM,32,LOOPING(TNUM)) = W_FOM
           COLV(TNUM,33,LOOPING(TNUM)) = W_GA
           CHCOLV(TNUM,34,LOOPING(TNUM)) = WSTATE
           COLV(TNUM,35,LOOPING(TNUM)) = W_GR
           COLV(TNUM,36,LOOPING(TNUM)) = W_CLRG
           COLV(TNUM,37,LOOPING(TNUM)) = W_CAR
           COLV(TNUM,38,LOOPING(TNUM)) = W_MRUN
           COLV(TNUM,39,LOOPING(TNUM)) = WSCBEF
           COLV(TNUM,40,LOOPING(TNUM)) = WSCBCST
           COLV(TNUM,41,LOOPING(TNUM)) = WASVL
           COLV(TNUM,42,LOOPING(TNUM)) = WBCWP
           COLV(TNUM,43,LOOPING(TNUM)) = WPCST
           COLV(TNUM,44,LOOPING(TNUM)) = WSCBYR
           COLV(TNUM,45,LOOPING(TNUM)) = WSCBGRP
           COLV(TNUM,46,LOOPING(TNUM)) = WRFURB
           COLV(TNUM,47,LOOPING(TNUM)) = W_CAPAD
           COLV(TNUM,48,LOOPING(TNUM)) = WSEQEF
           COLV(TNUM,49,LOOPING(TNUM)) = WNOX_R
           COLV(TNUM,50,LOOPING(TNUM)) = WNOX_B4
           COLV(TNUM,51,LOOPING(TNUM)) = WCOMB_O
           COLV(TNUM,52,LOOPING(TNUM)) = WCOMB_F
           COLV(TNUM,53,LOOPING(TNUM)) = WCOMB_V
           COLV(TNUM,54,LOOPING(TNUM)) = WCOMB_R
           COLV(TNUM,55,LOOPING(TNUM)) = WSNCR_O
           COLV(TNUM,56,LOOPING(TNUM)) = WSNCR_F
           COLV(TNUM,57,LOOPING(TNUM)) = WSNCR_V
           COLV(TNUM,58,LOOPING(TNUM)) = WSNCR_R
           COLV(TNUM,59,LOOPING(TNUM)) = WSCR_O
           COLV(TNUM,60,LOOPING(TNUM)) = WSCR_F
           COLV(TNUM,61,LOOPING(TNUM)) = WSCR_V
           COLV(TNUM,62,LOOPING(TNUM)) = WSCR_R
           COLV(TNUM,63,LOOPING(TNUM)) = W_COMB
           COLV(TNUM,64,LOOPING(TNUM)) = W_POST
           CHCOLV(TNUM,65,LOOPING(TNUM)) = WSCBT
           CHCOLV(TNUM,66,LOOPING(TNUM)) = WPART
           CHCOLV(TNUM,67,LOOPING(TNUM)) = W_ACI
           COLV(TNUM,68,LOOPING(TNUM)) = CCSROV
           COLV(TNUM,69,LOOPING(TNUM)) = CCSF
           COLV(TNUM,70,LOOPING(TNUM)) = CCSV
           COLV(TNUM,71,LOOPING(TNUM)) = CCSHR
           COLV(TNUM,72,LOOPING(TNUM)) = CCSCAPA
           COLV(TNUM,73,LOOPING(TNUM)) = CCS_LOC
           COLV(TNUM,74,LOOPING(TNUM)) = W_COOL
           COLV(TNUM,75,LOOPING(TNUM)) = W_LONG
           COLV(TNUM,76,LOOPING(TNUM)) = W_LAT
           COLV(TNUM,77,LOOPING(TNUM)) = W_DSIOV
           COLV(TNUM,78,LOOPING(TNUM)) = W_DSIF
           COLV(TNUM,79,LOOPING(TNUM)) = W_DSIV
           COLV(TNUM,80,LOOPING(TNUM)) = W_DSIR
           COLV(TNUM,81,LOOPING(TNUM)) = W_FFOV
           COLV(TNUM,82,LOOPING(TNUM)) = W_FFF
           COLV(TNUM,83,LOOPING(TNUM)) = W_FFV
           COLV(TNUM,84,LOOPING(TNUM)) = W_ESPU
           COLV(TNUM,85,LOOPING(TNUM)) = W_CFBU
           COLV(TNUM,86,LOOPING(TNUM)) = W_CFBUF
           COLV(TNUM,87,LOOPING(TNUM)) = W_CFBUV
           IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
             COLVALS(:,:) = COLV(TNUM,:,:)
             CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!            CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
             CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
             LOOPING(TNUM) = 0
           ENDIF

!         --- END efd_DPLT  ---
          ENDIF
      END DO
!
!     --- MAKE SURE ALL RECORDS WRITTEN TO NEMS DB
!
!      --- WRITE OUT ANY LEFT OVER RECORDS TO THE NEMS DATA BASE
       IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        DO TNUM = 1 , NUMTABS
         IF (LOOPING(TNUM) .NE. 0) THEN
           COLVALS(:,:) = COLV(TNUM,:,:)
           CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!          CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
           CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
           LOOPING(TNUM) = 0
         ENDIF
        ENDDO
       ENDIF
!     --- END
      RETURN
      END
!
!
      SUBROUTINE WRGRPDB
!
      IMPLICIT NONE
!
!     THIS SUBROUTINE WRITES OUT THE PLANT GROUP INFORMATION TO THE OUTPUT ACCESS DATABASE

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'ecpcntl'
      include 'dispin'
      include 'dispuse'
      include 'elcntl'
      include 'elout'
      include 'eusprc'
      include 'edbdef'
      include 'udatout'
      include 'uefdout'
      include 'emission'
      include 'cdsparms'
      include 'csapr'
      include 'emmemis'
!
      INTEGER IYR,IRG,IFL,ISP,DUMMYI,INTZERO
      INTEGER I,IGRP,IFP,J,ONE,IS,IFUEL,IECP
      INTEGER NUMTABS
      PARAMETER (NUMTABS = 3)        ! total number of database tables
      INTEGER RECCOUNT(NUMTABS)
      REAL*4 DUMMYR,DUMZERO,DUM9999,NETREV
      CHARACTER*2 COL,DUMCHAR
      INTEGER EHMRUN
!
      LOOPING = 0
      NUMCOLS = 0
      RECCOUNT = 0
      DYNSTM = ' '
      WRTSTM = ' '
      CHCOLVALS = ' '
      CHCOLV = ' '
      COLVALS = 0.0
      COLV = 0.0
!
      IYR = CURIYR
      COL = ' :'                                               !//EMMDB//
!
      DUMZERO = 0.0
      DUM9999 = 9999.99
      DUMCHAR = '  '
      INTZERO = 0
!
!     INITIALIZE NEGATIVE REVENUE ARRAYS
!
      IF ((CURIYR + UHBSYR) .GE. (UPSTYR + UPPLYR(WICT)) .AND. USW_NRVTYP .EQ. 1)THEN
         DO IRG = 1 , MNUMNR
            UNRVCOL(IRG,CURIYR) = 0.0
            UNRVCCY(IRG,CURIYR) = 0.0
            UNRVSTM(IRG,CURIYR) = 0.0
            UNRVNUC(IRG,CURIYR) = 0.0
            UNCPCOL(IRG,CURIYR) = 0.0
            UNCPCCY(IRG,CURIYR) = 0.0
            UNCPSTM(IRG,CURIYR) = 0.0
            UNCPNUC(IRG,CURIYR) = 0.0
         END DO
      END IF
!
!     LOOP OVER REGIONS AND GET DISPIN INFORMATION
!
      DO IRG = 1 , UNRGNS
!
        CALL GETIN(1,IRG)

!       WRITE(6,3977) CURIRUN, CURIYR+1989, CURITR, IRG, EEITAJ(1), EEITAJ(2), EEITAJ(3)
!3977   FORMAT(1X,"UTIL_09800_EEITAJ_GET",4(":",I4),3(":",F12.3))

!
!     WRITE OUT PLANT GROUP TABLES FOR DISPATCHABLE PLANTS
!
        DO I = 1, ECNTP
          IGRP = ECDBID(I)
!
!           DO J = 1 , EIFPLT
!             TNUM = 1
!             IFUEL = ECFLTP(I,J)
!               IF (IFUEL .GT. 0) THEN
!                 RECCOUNT(TNUM) = RECCOUNT(TNUM) + 1
!                 WRITE(UF_DBPGRP,2045) COL,CURIYR,COL,CURITR,COL,RECCOUNT(TNUM),COL,IGRP,COL, &
!                 J,COL,IFUEL,COL,ULIGRP(IGRP), &
!               COL,ECNR(I),COL,ECFOWN(I),COL,ECASTS(I),COL,ECTECP(I), &
!               COL,ULEFPT(IGRP),COL,ULVINT(IGRP),COL,ECCR(I),COL, &
!               ECOPR(I),COL,ECMRUN(I),COL, &
!               ECSCRB(I), &
!               COL,ECALLW(I),COL,ECMXCP(I),COL,ECPMR(IGRP),COL, &
!                 ECFOR(IGRP),COL,ULFCST(IGRP),COL,ULVCST(IGRP),COL, &
!                 ULFLCST(J,IGRP),COL, ULHTRT_EFD(IGRP,1), COL,ECMFSH(I,J),COL,UP_FL_RG(IGRP),COL,  &
!               ULRPS(IGRP),COL,ULCAPC(IGRP),COL,ULRETC(IGRP),COL, &
!                 ULREVS(IGRP),COL,ULCCST(IGRP),COL, &
!                 ULSO2P(IGRP),COL,ULNOXP(IGRP),COL,ULHGP(IGRP),COL,ULRPSP(IGRP),COL,  &
!                 ULGENE(J,IGRP),COL,ULBTUE(J,IGRP), &
!                 COL,ULNOXW(J,IGRP),COL,ULSO2W(J,IGRP),COL,ULCO2W(J,IGRP), &
!                 COL,ULCARW(J,IGRP),COL,ULHGQ(J,IGRP),COL,     &
!                 ECCAP(I,1),COL,ECCOPM(I,1),COL,ECFNOX(I,1), &
!                 COL,ULGENS(1,IGRP),COL,ULCSTR(1,IGRP),COL,INTZERO,COL, &
!                 ECCAP(I,2),COL,ECCOPM(I,2),COL,ECFNOX(I,2), &
!                 COL,ULGENS(2,IGRP),COL,ULCSTR(2,IGRP),COL,INTZERO,COL, &
!                 ECCAP(I,3),COL,ECCOPM(I,3),COL,ECFNOX(I,3), &
!                 COL,ULGENS(3,IGRP),COL,ULCSTR(3,IGRP),COL,INTZERO,COL,TRIM(SCEN_DATE)

 2045  FORMAT(1X,'DPGRP',2(A2,I3),2(A2,I6),2(A2,I3),A2,I6,10(A2,I3), &
                  A2,F12.3,A2,I6,A2,5(F12.6,A2),F12.4,A2,F9.5,A2,2(I3,A2),15(F15.6,A2), &
                  5(F15.6,A2),I6,A2,    &
                  5(F15.6,A2),I6,A2,    &
                  5(F15.6,A2),I6,A2,A)
!               DPGRP:Label:CURIYR:CURITR:IGRP:ULIGRP:ECNR:ECFOWN:ECASTS:ECTECP:ULEFPT:ULVINT:ECCR:ECOPR:ECMRUN:ECSCRB:ECALLW:ECMXCP:ECPMR:ECFOR:ULVCST:ULFCST:ULRPS:ULCAPC:ULRETC:ULTGEN:ULREVS:ULCCST:ULSO2P:ULNOXP:ULHGP:ULRPSP:TRIM(SCEN_DATE)
!            --- START efd_DPGDT ---
        DO J = 1 , EIFPLT
          IFUEL = ECFLTP(I,J)
          IF (IFUEL .GT. 0) THEN

          TNUM = 1
          IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
            IF (LOOPING(TNUM) .EQ. 0) THEN
              NUMCOLS(TNUM) = 72
              DYNSTM(TNUM) = 'INSERT INTO EFD_DPGDT VALUES(?,?,?,?,?,'  &
                             //'?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,'   &
                             //'?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,'   &
                             //'?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,'   &
                             //'?,?,?,?)'
              WRTSTM(TNUM) = 'EFD_DPGDT'
            ENDIF
            LOOPING(TNUM) = LOOPING(TNUM) + 1
            RECCOUNT(TNUM) = RECCOUNT(TNUM) + 1
            COLV(TNUM,1,LOOPING(TNUM))  = CURIYR
            COLV(TNUM,2,LOOPING(TNUM))  = CURITR
            COLV(TNUM,3,LOOPING(TNUM))  = RECCOUNT(TNUM)
            COLV(TNUM,4,LOOPING(TNUM))  = IGRP
            COLV(TNUM,5,LOOPING(TNUM))  = J
            COLV(TNUM,6,LOOPING(TNUM))  = IFUEL
            COLV(TNUM,7,LOOPING(TNUM))  = ULIGRP(IGRP)
            COLV(TNUM,8,LOOPING(TNUM))  = ECNR(I)
            COLV(TNUM,9,LOOPING(TNUM))  = ECFOWN(I)
            COLV(TNUM,10,LOOPING(TNUM)) = ECASTS(I)
            COLV(TNUM,11,LOOPING(TNUM)) = ECTECP(I)
            COLV(TNUM,12,LOOPING(TNUM)) = ULEFPT(IGRP)
            COLV(TNUM,13,LOOPING(TNUM)) = ULVINT(IGRP)
            COLV(TNUM,14,LOOPING(TNUM)) = ECCR(I)
            COLV(TNUM,15,LOOPING(TNUM)) = ECOPR(I)
            COLV(TNUM,16,LOOPING(TNUM)) = ECMRUN(I)
            COLV(TNUM,17,LOOPING(TNUM)) = ECSCRB(I)
            COLV(TNUM,18,LOOPING(TNUM)) = ECALLW(I)
            COLV(TNUM,19,LOOPING(TNUM)) = ECMXCP(I)
            COLV(TNUM,20,LOOPING(TNUM)) = ECPMR(IGRP)
            COLV(TNUM,21,LOOPING(TNUM)) = ECFOR(IGRP)
            COLV(TNUM,22,LOOPING(TNUM)) = ULFCST(IGRP)
            COLV(TNUM,23,LOOPING(TNUM)) = ULVCST(IGRP)
            COLV(TNUM,24,LOOPING(TNUM)) = ULFLCST(J,IGRP)
            COLV(TNUM,25,LOOPING(TNUM)) = ULHTRT_EFD(IGRP,1)
            COLV(TNUM,26,LOOPING(TNUM)) = ECMFSH(I,J)
            COLV(TNUM,27,LOOPING(TNUM)) = UP_FL_RG(IGRP)
            COLV(TNUM,28,LOOPING(TNUM)) = ULRPS(IGRP)
            COLV(TNUM,29,LOOPING(TNUM)) = ULCAPC(IGRP)
            COLV(TNUM,30,LOOPING(TNUM)) = ULRETC(IGRP)
            COLV(TNUM,31,LOOPING(TNUM)) = ULREVS(IGRP)
            COLV(TNUM,32,LOOPING(TNUM)) = ULENGREVS(IGRP)
            COLV(TNUM,33,LOOPING(TNUM)) = ULCCST(IGRP)
            COLV(TNUM,34,LOOPING(TNUM)) = ULSO2P(IGRP)
            COLV(TNUM,35,LOOPING(TNUM)) = ULNOXP(IGRP)
            COLV(TNUM,36,LOOPING(TNUM)) = ULHGP(IGRP)
            COLV(TNUM,37,LOOPING(TNUM)) = ULRPSP(IGRP)
            COLV(TNUM,38,LOOPING(TNUM)) = ULGHG(IGRP)
            COLV(TNUM,39,LOOPING(TNUM)) = ULGENE(J,IGRP)
            COLV(TNUM,40,LOOPING(TNUM)) = ULBTUE(J,IGRP)
            COLV(TNUM,41,LOOPING(TNUM)) = ULNOXW(J,IGRP)
            COLV(TNUM,42,LOOPING(TNUM)) = ULSO2W(J,IGRP)
            COLV(TNUM,43,LOOPING(TNUM)) = ULCO2W(J,IGRP)
            COLV(TNUM,44,LOOPING(TNUM)) = ULCARW(J,IGRP)
            COLV(TNUM,45,LOOPING(TNUM)) = ULHGQ(J,IGRP)
            COLV(TNUM,46,LOOPING(TNUM)) = ECCAP(I,1)
            COLV(TNUM,47,LOOPING(TNUM)) = ECCOPM(I,1)
            COLV(TNUM,48,LOOPING(TNUM)) = ECFNOX(I,1)
            COLV(TNUM,49,LOOPING(TNUM)) = ULGENS(1,IGRP)
            COLV(TNUM,50,LOOPING(TNUM)) = ULCSTR(1,IGRP)
            COLV(TNUM,51,LOOPING(TNUM)) = INTZERO
            COLV(TNUM,52,LOOPING(TNUM)) = ECCAP(I,2)
            COLV(TNUM,53,LOOPING(TNUM)) = ECCOPM(I,2)
            COLV(TNUM,54,LOOPING(TNUM)) = ECFNOX(I,2)
            COLV(TNUM,55,LOOPING(TNUM)) = ULGENS(2,IGRP)
            COLV(TNUM,56,LOOPING(TNUM)) = ULCSTR(2,IGRP)
            COLV(TNUM,57,LOOPING(TNUM)) = INTZERO
            COLV(TNUM,58,LOOPING(TNUM)) = ECCAP(I,3)
            COLV(TNUM,59,LOOPING(TNUM)) = ECCOPM(I,3)
            COLV(TNUM,60,LOOPING(TNUM)) = ECFNOX(I,3)
            COLV(TNUM,61,LOOPING(TNUM)) = ULGENS(3,IGRP)
            COLV(TNUM,62,LOOPING(TNUM)) = ULCSTR(3,IGRP)
            COLV(TNUM,63,LOOPING(TNUM)) = INTZERO
            COLV(TNUM,64,LOOPING(TNUM)) = ELGENE(1,1,IGRP)
            COLV(TNUM,65,LOOPING(TNUM)) = ELGENE(2,1,IGRP)
            COLV(TNUM,66,LOOPING(TNUM)) = ELGENE(3,1,IGRP)
            COLV(TNUM,67,LOOPING(TNUM)) = ELGENE(1,2,IGRP)
            COLV(TNUM,68,LOOPING(TNUM)) = ELGENE(2,2,IGRP)
            COLV(TNUM,69,LOOPING(TNUM)) = ELGENE(3,2,IGRP)
            COLV(TNUM,70,LOOPING(TNUM)) = ELGENE(1,3,IGRP)
            COLV(TNUM,71,LOOPING(TNUM)) = ELGENE(2,3,IGRP)
            COLV(TNUM,72,LOOPING(TNUM)) = ELGENE(3,3,IGRP)
            IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
              COLVALS(:,:) = COLV(TNUM,:,:)
!             CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
              CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
              LOOPING(TNUM) = 0
            ENDIF
          ENDIF
!          --- END efd_DPGDT ---
         END IF
        END DO
!
!       STORE NEGATIVE REVENUE OUTPUT IF CURRENT YEAR SWITCH ACTIVE
!
        IF ((CURIYR + UHBSYR) .GE. (UPSTYR + UPPLYR(WICT)) .AND. USW_NRVTYP .EQ. 1)THEN
           IF (USW_NRVREV .EQ. 1)THEN
              NETREV = ULREVS(IGRP) - ULFCST(IGRP) - ULVCST(IGRP) - ULSO2P(IGRP) - ULNOXP(IGRP) - ULRPSP(IGRP) - ULHGP(IGRP) - ULGHG(IGRP)
           ELSE
              NETREV = ULENGREVS(IGRP) - ULFCST(IGRP) - ULVCST(IGRP) - ULSO2P(IGRP) - ULNOXP(IGRP) - ULRPSP(IGRP) - ULHGP(IGRP) - ULGHG(IGRP)
           END IF
           IF (NETREV .LT. 0.0)THEN
              IECP = ECTECP(I)
!             COAL
              IF (UPTTYP(IECP) .LE. NW_COAL) THEN
                 UNRVCOL(ECNR(I),CURIYR) = UNRVCOL(ECNR(I),CURIYR) - NETREV
                 UNRVCOL(MNUMNR,CURIYR) = UNRVCOL(MNUMNR,CURIYR) - NETREV
                 UNCPCOL(ECNR(I),CURIYR) = UNCPCOL(ECNR(I),CURIYR) + ULCAPC(IGRP)
                 UNCPCOL(MNUMNR,CURIYR) = UNCPCOL(MNUMNR,CURIYR) + ULCAPC(IGRP)
!             CC
              ELSEIF (IECP .EQ. WIEC .OR. IECP .EQ. WICC .OR. IECP .EQ. WIAC .OR. IECP .EQ. WIA2 .OR. IECP .EQ. WICS) THEN
                 UNRVCCY(ECNR(I),CURIYR) = UNRVCCY(ECNR(I),CURIYR) - NETREV
                 UNRVCCY(MNUMNR,CURIYR) = UNRVCCY(MNUMNR,CURIYR) - NETREV
                 UNCPCCY(ECNR(I),CURIYR) = UNCPCCY(ECNR(I),CURIYR) + ULCAPC(IGRP)
                 UNCPCCY(MNUMNR,CURIYR) = UNCPCCY(MNUMNR,CURIYR) + ULCAPC(IGRP)
!             GAS/OIL STEAM
              ELSEIF (IECP .EQ. WIST) THEN
                 UNRVSTM(ECNR(I),CURIYR) = UNRVSTM(ECNR(I),CURIYR) - NETREV
                 UNRVSTM(MNUMNR,CURIYR) = UNRVSTM(MNUMNR,CURIYR) - NETREV
                 UNCPSTM(ECNR(I),CURIYR) = UNCPSTM(ECNR(I),CURIYR) + ULCAPC(IGRP)
                 UNCPSTM(MNUMNR,CURIYR) = UNCPSTM(MNUMNR,CURIYR) + ULCAPC(IGRP)
              ELSEIF (IECP .EQ. WICN .OR. IECP .EQ. WIAN .OR. IECP .EQ. WISM .OR. IECP .EQ. WIGN) THEN
                 UNRVNUC(ECNR(I),CURIYR) = UNRVNUC(ECNR(I),CURIYR) - NETREV
                 UNRVNUC(MNUMNR,CURIYR) = UNRVNUC(MNUMNR,CURIYR) - NETREV
                 UNCPNUC(ECNR(I),CURIYR) = UNCPNUC(ECNR(I),CURIYR) + ULCAPC(IGRP)
                 UNCPNUC(MNUMNR,CURIYR) = UNCPNUC(MNUMNR,CURIYR) + ULCAPC(IGRP)
              END IF
           END IF
        END IF
       END DO           ! dispatchable group do
!
!     WRITE OUT PLANT GROUP TABLES FOR RENEWABLE PLANTS TS
!
        DO I = 1 , EHNTP
          IGRP = EHDBID(I)
          EHMRUN = 0
          IF (ULMRUN(IGRP) .GT. 0) EHMRUN = 1
!
!         DO J = 1 , EIFPLT
!           IFUEL = EHFLTP(I,J)
!           IF (IFUEL .GT. 0) THEN
!             TNUM = 1
!               RECCOUNT(TNUM) = RECCOUNT(TNUM) + 1

!              WRITE(UF_DBPGRP,2045) COL,CURIYR,COL,CURITR,COL,RECCOUNT(TNUM),COL,IGRP,COL, &
!               J,COL,IFUEL,COL,ULIGRP(IGRP), &
!               COL,EHNR(I),COL,EHFOWN(I),COL,EHHYTP(I),COL,EHTECP(I), &
!               COL,ULEFPT(IGRP),COL,ULVINT(IGRP),COL,EHCR(I),COL, &
!               ULOPER(IGRP),COL,EHMRUN,COL, &
!               INTZERO, &
!               COL,DUMZERO,COL,INTZERO,COL,DUMZERO,COL, &
!               DUMZERO,COL,ULFCST(IGRP),COL,ULVCST(IGRP),COL, &
!               ULFLCST(J,IGRP),COL, ULHTRT_EFD(IGRP,1), COL,EHMFSH(I,J),COL,UP_FL_RG(IGRP),COL,   &
!               ULRPS(IGRP),COL,ULCAPC(IGRP),COL,ULRETC(IGRP),COL, &
!               ULREVS(IGRP),COL,ULCCST(IGRP),COL, &
!               ULSO2P(IGRP),COL,ULNOXP(IGRP),COL,ULHGP(IGRP),COL,ULRPSP(IGRP),COL,  &
!               ULGENE(J,IGRP),COL,ULBTUE(J,IGRP), &
!               COL,ULNOXW(J,IGRP),COL,ULSO2W(J,IGRP),COL,ULCO2W(J,IGRP), &
!               COL,ULCARW(J,IGRP),COL,ULHGQ(J,IGRP),COL,     &
!               EHCAP(I,1),COL,EHCAP(I,1),COL,EHFNOX(I,1), &
!               COL,ULGENS(1,IGRP),COL,ULCSTR(1,IGRP),COL,EHHYCF(I,1),COL, &
!               EHCAP(I,2),COL,EHCAP(I,2),COL,EHFNOX(I,2), &
!               COL,ULGENS(2,IGRP),COL,ULCSTR(2,IGRP),COL,EHHYCF(I,2),COL, &
!               EHCAP(I,3),COL,EHCAP(I,3),COL,EHFNOX(I,3), &
!               COL,ULGENS(3,IGRP),COL,ULCSTR(3,IGRP),COL,EHHYCF(I,3),COL,TRIM(SCEN_DATE)

!            --- START efd_DPGDT ---
        DO J = 1 , EIFPLT
          IFUEL = EHFLTP(I,J)
          IF (IFUEL .GT. 0) THEN
          TNUM = 1
          IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
            LOOPING(TNUM) = LOOPING(TNUM) + 1
            RECCOUNT(TNUM) = RECCOUNT(TNUM) + 1
            COLV(TNUM,1,LOOPING(TNUM)) = CURIYR
            COLV(TNUM,2,LOOPING(TNUM)) = CURITR
            COLV(TNUM,3,LOOPING(TNUM)) = RECCOUNT(TNUM)
            COLV(TNUM,4,LOOPING(TNUM)) = IGRP
            COLV(TNUM,5,LOOPING(TNUM)) = J
            COLV(TNUM,6,LOOPING(TNUM)) = IFUEL
            COLV(TNUM,7,LOOPING(TNUM)) = ULIGRP(IGRP)
            COLV(TNUM,8,LOOPING(TNUM)) = EHNR(I)
            COLV(TNUM,9,LOOPING(TNUM)) = EHFOWN(I)
            COLV(TNUM,10,LOOPING(TNUM)) = EHHYTP(I)
            COLV(TNUM,11,LOOPING(TNUM)) = EHTECP(I)
            COLV(TNUM,12,LOOPING(TNUM)) = ULEFPT(IGRP)
            COLV(TNUM,13,LOOPING(TNUM)) = ULVINT(IGRP)
            COLV(TNUM,14,LOOPING(TNUM)) = EHCR(I)
            COLV(TNUM,15,LOOPING(TNUM)) = ULOPER(IGRP)
            COLV(TNUM,16,LOOPING(TNUM)) = EHMRUN
            COLV(TNUM,17,LOOPING(TNUM)) = INTZERO
            COLV(TNUM,18,LOOPING(TNUM)) = DUMZERO
            COLV(TNUM,19,LOOPING(TNUM)) = INTZERO
            COLV(TNUM,20,LOOPING(TNUM)) = DUMZERO
            COLV(TNUM,21,LOOPING(TNUM)) = DUMZERO
            COLV(TNUM,22,LOOPING(TNUM)) = ULFCST(IGRP)
            COLV(TNUM,23,LOOPING(TNUM)) = ULVCST(IGRP)
            COLV(TNUM,24,LOOPING(TNUM)) = ULFLCST(J,IGRP)
            COLV(TNUM,25,LOOPING(TNUM)) = ULHTRT_EFD(IGRP,1)
            COLV(TNUM,26,LOOPING(TNUM)) = EHMFSH(I,J)
            COLV(TNUM,27,LOOPING(TNUM)) = UP_FL_RG(IGRP)
            COLV(TNUM,28,LOOPING(TNUM)) = ULRPS(IGRP)
            COLV(TNUM,29,LOOPING(TNUM)) = ULCAPC(IGRP)
            COLV(TNUM,30,LOOPING(TNUM)) = ULRETC(IGRP)
            COLV(TNUM,31,LOOPING(TNUM)) = ULREVS(IGRP)
            COLV(TNUM,32,LOOPING(TNUM)) = ULENGREVS(IGRP)
            COLV(TNUM,33,LOOPING(TNUM)) = ULCCST(IGRP)
            COLV(TNUM,34,LOOPING(TNUM)) = ULSO2P(IGRP)
            COLV(TNUM,35,LOOPING(TNUM)) = ULNOXP(IGRP)
            COLV(TNUM,36,LOOPING(TNUM)) = ULHGP(IGRP)
            COLV(TNUM,37,LOOPING(TNUM)) = ULRPSP(IGRP)
            COLV(TNUM,38,LOOPING(TNUM)) = ULGHG(IGRP)
            COLV(TNUM,39,LOOPING(TNUM)) = ULGENE(J,IGRP)
            COLV(TNUM,40,LOOPING(TNUM)) = ULBTUE(J,IGRP)
            COLV(TNUM,41,LOOPING(TNUM)) = ULNOXW(J,IGRP)
            COLV(TNUM,42,LOOPING(TNUM)) = ULSO2W(J,IGRP)
            COLV(TNUM,43,LOOPING(TNUM)) = ULCO2W(J,IGRP)
            COLV(TNUM,44,LOOPING(TNUM)) = ULCARW(J,IGRP)
            COLV(TNUM,45,LOOPING(TNUM)) = ULHGQ(J,IGRP)
            COLV(TNUM,46,LOOPING(TNUM)) = EHCAP(I,1)
            COLV(TNUM,47,LOOPING(TNUM)) = EHCAP(I,1)
            COLV(TNUM,48,LOOPING(TNUM)) = EHFNOX(I,1)
            COLV(TNUM,49,LOOPING(TNUM)) = ULGENS(1,IGRP)
            COLV(TNUM,50,LOOPING(TNUM)) = ULCSTR(1,IGRP)
            COLV(TNUM,51,LOOPING(TNUM)) = EHHYCF(I,1)
            COLV(TNUM,52,LOOPING(TNUM)) = EHCAP(I,2)
            COLV(TNUM,53,LOOPING(TNUM)) = EHCAP(I,2)
            COLV(TNUM,54,LOOPING(TNUM)) = EHFNOX(I,2)
            COLV(TNUM,55,LOOPING(TNUM)) = ULGENS(2,IGRP)
            COLV(TNUM,56,LOOPING(TNUM)) = ULCSTR(2,IGRP)
            COLV(TNUM,57,LOOPING(TNUM)) = EHHYCF(I,2)
            COLV(TNUM,58,LOOPING(TNUM)) = EHCAP(I,3)
            COLV(TNUM,59,LOOPING(TNUM)) = EHCAP(I,3)
            COLV(TNUM,60,LOOPING(TNUM)) = EHFNOX(I,3)
            COLV(TNUM,61,LOOPING(TNUM)) = ULGENS(3,IGRP)
            COLV(TNUM,62,LOOPING(TNUM)) = ULCSTR(3,IGRP)
            COLV(TNUM,63,LOOPING(TNUM)) = EHHYCF(I,3)
            COLV(TNUM,64,LOOPING(TNUM)) = ELGENE(1,1,IGRP)
            COLV(TNUM,65,LOOPING(TNUM)) = ELGENE(2,1,IGRP)
            COLV(TNUM,66,LOOPING(TNUM)) = ELGENE(3,1,IGRP)
            COLV(TNUM,67,LOOPING(TNUM)) = ELGENE(1,2,IGRP)
            COLV(TNUM,68,LOOPING(TNUM)) = ELGENE(2,2,IGRP)
            COLV(TNUM,69,LOOPING(TNUM)) = ELGENE(3,2,IGRP)
            COLV(TNUM,70,LOOPING(TNUM)) = ELGENE(1,3,IGRP)
            COLV(TNUM,71,LOOPING(TNUM)) = ELGENE(2,3,IGRP)
            COLV(TNUM,72,LOOPING(TNUM)) = ELGENE(3,3,IGRP)
            IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
              COLVALS(:,:) = COLV(TNUM,:,:)
!             CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
              CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
              LOOPING(TNUM) = 0
            ENDIF
          ENDIF
!          --- END efd_DPGDT ---
!
          END IF
        END DO
!
       END DO           ! renewable group do
!
!     WRITE OUT PLANT GROUP TABLES FOR DISTRIBUTED GENERATION PLANTS
!
        DO I = 1, EDNTP
          IGRP = EDDBID(I)
!
!         DO J = 1 , EIFPLT
!           IFUEL = EDFLTP(I,J)
!           IF (IFUEL .GT. 0) THEN
!           TNUM = 1
!           RECCOUNT(TNUM) = RECCOUNT(TNUM) + 1

!           WRITE(UF_DBPGRP,2045) COL,CURIYR,COL,CURITR,COL,RECCOUNT(TNUM),COL,IGRP,COL, &
!               J,COL,IFUEL,COL,ULIGRP(IGRP), &
!               COL,EDNR(I),COL,EDFOWN(I),COL,EDASTS(I),COL,EDTECP(I), &
!               COL,ULEFPT(IGRP),COL,ULVINT(IGRP),COL,EDCR(I),COL, &
!               ULOPER(IGRP),COL,EDMRUN(I),COL, &
!               INTZERO, &
!               COL,DUMZERO,COL,EDMXCP(I),COL,EDPMR(I)*EFACTR,COL, &
!               EDFOR(I)*EFACTR,COL,ULFCST(IGRP),COL,ULVCST(IGRP),COL, &
!               ULFLCST(J,IGRP),COL, ULHTRT_EFD(IGRP,1), COL,EDMFSH(I,J),COL,UP_FL_RG(IGRP),COL,  &
!               ULRPS(IGRP),COL,ULCAPC(IGRP),COL,ULRETC(IGRP),COL, &
!               ULREVS(IGRP),COL,ULCCST(IGRP),COL, &
!               ULSO2P(IGRP),COL,ULNOXP(IGRP),COL,ULHGP(IGRP),COL,ULRPSP(IGRP),COL,  &
!               ULGENE(J,IGRP),COL,ULBTUE(J,IGRP), &
!               COL,ULNOXW(J,IGRP),COL,ULSO2W(J,IGRP),COL,ULCO2W(J,IGRP), &
!               COL,ULCARW(J,IGRP),COL,ULHGQ(J,IGRP),COL,     &
!               EDCAP(I,1),COL,EDCAP(I,1),COL,EDFNOX(I,1), &
!               COL,ULGENS(1,IGRP),COL,ULCSTR(1,IGRP),COL,INTZERO,COL, &
!               EDCAP(I,2),COL,EDCAP(I,2),COL,EDFNOX(I,2), &
!               COL,ULGENS(2,IGRP),COL,ULCSTR(2,IGRP),COL,INTZERO,COL, &
!               EDCAP(I,3),COL,EDCAP(I,3),COL,EDFNOX(I,3), &
!               COL,ULGENS(3,IGRP),COL,ULCSTR(3,IGRP),COL,INTZERO,COL,TRIM(SCEN_DATE)

!            --- START efd_DPGDT ---
        DO J = 1 , EIFPLT
          IFUEL = EDFLTP(I,J)
          IF (IFUEL .GT. 0) THEN
          TNUM = 1
          IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
            LOOPING(TNUM) = LOOPING(TNUM) + 1
            RECCOUNT(TNUM) = RECCOUNT(TNUM) + 1
            COLV(TNUM,1,LOOPING(TNUM))  = CURIYR
            COLV(TNUM,2,LOOPING(TNUM))  = CURITR
            COLV(TNUM,3,LOOPING(TNUM))  = RECCOUNT(TNUM)
            COLV(TNUM,4,LOOPING(TNUM))  = IGRP
            COLV(TNUM,5,LOOPING(TNUM))  = J
            COLV(TNUM,6,LOOPING(TNUM))  = IFUEL
            COLV(TNUM,7,LOOPING(TNUM))  = ULIGRP(IGRP)
            COLV(TNUM,8,LOOPING(TNUM))  = EDNR(I)
            COLV(TNUM,9,LOOPING(TNUM))  = EDFOWN(I)
            COLV(TNUM,10,LOOPING(TNUM)) = EDASTS(I)
            COLV(TNUM,11,LOOPING(TNUM)) = EDTECP(I)
            COLV(TNUM,12,LOOPING(TNUM)) = ULEFPT(IGRP)
            COLV(TNUM,13,LOOPING(TNUM)) = ULVINT(IGRP)
            COLV(TNUM,14,LOOPING(TNUM)) = EDCR(I)
            COLV(TNUM,15,LOOPING(TNUM)) = ULOPER(IGRP)
            COLV(TNUM,16,LOOPING(TNUM)) = EDMRUN(I)
            COLV(TNUM,17,LOOPING(TNUM)) = INTZERO
            COLV(TNUM,18,LOOPING(TNUM)) = DUMZERO
            COLV(TNUM,19,LOOPING(TNUM)) = EDMXCP(I)
            COLV(TNUM,20,LOOPING(TNUM)) = EDPMR(I) * EFACTR
            COLV(TNUM,21,LOOPING(TNUM)) = EDFOR(I) * EFACTR
            COLV(TNUM,22,LOOPING(TNUM)) = ULFCST(IGRP)
            COLV(TNUM,23,LOOPING(TNUM)) = ULVCST(IGRP)
            COLV(TNUM,24,LOOPING(TNUM)) = ULFLCST(J,IGRP)
            COLV(TNUM,25,LOOPING(TNUM)) = ULHTRT_EFD(IGRP,1)
            COLV(TNUM,26,LOOPING(TNUM)) = EDMFSH(I,J)
            COLV(TNUM,27,LOOPING(TNUM)) = UP_FL_RG(IGRP)
            COLV(TNUM,28,LOOPING(TNUM)) = ULRPS(IGRP)
            COLV(TNUM,29,LOOPING(TNUM)) = ULCAPC(IGRP)
            COLV(TNUM,30,LOOPING(TNUM)) = ULRETC(IGRP)
            COLV(TNUM,31,LOOPING(TNUM)) = ULREVS(IGRP)
            COLV(TNUM,32,LOOPING(TNUM)) = ULENGREVS(IGRP)
            COLV(TNUM,33,LOOPING(TNUM)) = ULCCST(IGRP)
            COLV(TNUM,34,LOOPING(TNUM)) = ULSO2P(IGRP)
            COLV(TNUM,35,LOOPING(TNUM)) = ULNOXP(IGRP)
            COLV(TNUM,36,LOOPING(TNUM)) = ULHGP(IGRP)
            COLV(TNUM,37,LOOPING(TNUM)) = ULRPSP(IGRP)
            COLV(TNUM,38,LOOPING(TNUM)) = ULGHG(IGRP)
            COLV(TNUM,39,LOOPING(TNUM)) = ULGENE(J,IGRP)
            COLV(TNUM,40,LOOPING(TNUM)) = ULBTUE(J,IGRP)
            COLV(TNUM,41,LOOPING(TNUM)) = ULNOXW(J,IGRP)
            COLV(TNUM,42,LOOPING(TNUM)) = ULSO2W(J,IGRP)
            COLV(TNUM,43,LOOPING(TNUM)) = ULCO2W(J,IGRP)
            COLV(TNUM,44,LOOPING(TNUM)) = ULCARW(J,IGRP)
            COLV(TNUM,45,LOOPING(TNUM)) = ULHGQ(J,IGRP)
            COLV(TNUM,46,LOOPING(TNUM)) = EDCAP(I,1)
            COLV(TNUM,47,LOOPING(TNUM)) = EDCAP(I,1)
            COLV(TNUM,48,LOOPING(TNUM)) = EDFNOX(I,1)
            COLV(TNUM,49,LOOPING(TNUM)) = ULGENS(1,IGRP)
            COLV(TNUM,50,LOOPING(TNUM)) = ULCSTR(1,IGRP)
            COLV(TNUM,51,LOOPING(TNUM)) = INTZERO
            COLV(TNUM,52,LOOPING(TNUM)) = EDCAP(I,2)
            COLV(TNUM,53,LOOPING(TNUM)) = EDCAP(I,2)
            COLV(TNUM,54,LOOPING(TNUM)) = EDFNOX(I,2)
            COLV(TNUM,55,LOOPING(TNUM)) = ULGENS(2,IGRP)
            COLV(TNUM,56,LOOPING(TNUM)) = ULCSTR(2,IGRP)
            COLV(TNUM,57,LOOPING(TNUM)) = INTZERO
            COLV(TNUM,58,LOOPING(TNUM)) = EDCAP(I,3)
            COLV(TNUM,59,LOOPING(TNUM)) = EDCAP(I,3)
            COLV(TNUM,60,LOOPING(TNUM)) = EDFNOX(I,3)
            COLV(TNUM,61,LOOPING(TNUM)) = ULGENS(3,IGRP)
            COLV(TNUM,62,LOOPING(TNUM)) = ULCSTR(3,IGRP)
            COLV(TNUM,63,LOOPING(TNUM)) = INTZERO
            COLV(TNUM,64,LOOPING(TNUM)) = ELGENE(1,1,IGRP)
            COLV(TNUM,65,LOOPING(TNUM)) = ELGENE(2,1,IGRP)
            COLV(TNUM,66,LOOPING(TNUM)) = ELGENE(3,1,IGRP)
            COLV(TNUM,67,LOOPING(TNUM)) = ELGENE(1,2,IGRP)
            COLV(TNUM,68,LOOPING(TNUM)) = ELGENE(2,2,IGRP)
            COLV(TNUM,69,LOOPING(TNUM)) = ELGENE(3,2,IGRP)
            COLV(TNUM,70,LOOPING(TNUM)) = ELGENE(1,3,IGRP)
            COLV(TNUM,71,LOOPING(TNUM)) = ELGENE(2,3,IGRP)
            COLV(TNUM,72,LOOPING(TNUM)) = ELGENE(3,3,IGRP)
            IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
              COLVALS(:,:) = COLV(TNUM,:,:)
!             CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
              CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
              LOOPING(TNUM) = 0
            ENDIF
          ENDIF
!          --- END efd_DPGDT ---
!
          END IF
        END DO
!
        END DO           ! end dgen do
!
       END DO            ! END REGION LOOP
!
!      --- WRITE OUT ANY LEFT OVER RECORDS TO THE NEMS DATA BASE
       IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        DO TNUM = 1 , NUMTABS
         IF (LOOPING(TNUM) .NE. 0) THEN
           COLVALS(:,:) = COLV(TNUM,:,:)
!          CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
           CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
           LOOPING(TNUM) = 0
         ENDIF
        ENDDO
       ENDIF
!
      RETURN
      END
!

      SUBROUTINE ADJDEMD(DEMADJ)
      IMPLICIT NONE
!
!     THIS SUBROUTINE ADJUSTS SECTORAL DEMANDS FOR DSM OR DISTRIBUTED GENERATION IF SWITCHES ARE SET ACCORDINGLY
!     FOR EXAMPLE, IN CECA RUNS
!     ONLY DO THIS IN THE FIRST ITERATION AND IF DEMAND MODULES ARE NOT RUN

      include 'parametr'
      include 'ncntrl'
      include 'qblk'
      include 'xpq'
      include 'emmparm'
      include 'control'
      include 'cogen'
      include 'udatout'
      include 'dsmdimen'
      include 'dsmsectr'
!
      INTEGER*4 DEMADJ
      INTEGER*4 JYR,KYR,BYR,JRG,IFL
      REAL*4 DELRS(MNUMCR),DELCM(MNUMCR),DELIN(MNUMCR),DELTR(MNUMCR),DELAS(MNUMCR)
      REAL*4 DFAC
      IF(DEMADJ .EQ. 1)THEN
       IF (USW_DSM .GT. 0) THEN
!
!     FOR STANDALONE RUNS, STORE DEMAND IN FIRST ITERATION
!     AND THEN RESET TO ORIGINAL VALUES IN SUBSEQUENT ITERATIONS
        IF(CURITR .EQ. 1)THEN
          DO JRG = 1 , MNUMCR
            IF(EXR .LE. 0)DELRS(JRG) = QELRS(JRG,CURIYR)
            IF(EXK .LE. 0)DELCM(JRG) = QELCM(JRG,CURIYR)
            IF(EXI .LE. 0)DELIN(JRG) = QELIN(JRG,CURIYR)
            IF(EXT .LE. 0)DELTR(JRG) = QELTR(JRG,CURIYR)
          END DO
        ELSE
          DO JRG = 1 , MNUMCR
            IF(EXR .LE. 0)QELRS(JRG,CURIYR) = DELRS(JRG)
            IF(EXK .LE. 0)QELCM(JRG,CURIYR) = DELCM(JRG)
            IF(EXI .LE. 0)QELIN(JRG,CURIYR) = DELIN(JRG)
            IF(EXT .LE. 0)QELTR(JRG,CURIYR) = DELTR(JRG)
            IF(EXR .LE. 0 .OR. EXK .LE. 0 .OR. &
               EXI .LE. 0 .OR. EXT .LE. 0) &
                          QELAS(JRG,CURIYR) = QELRS(JRG,CURIYR) + &
                                              QELCM(JRG,CURIYR) + &
                                              QELIN(JRG,CURIYR) + &
                                              QELTR(JRG,CURIYR)
          END DO
        END IF
       ENDIF
!     RESIDENTIAL
      IF(EXR .EQ. 1 .OR. CURITR .EQ. 1)THEN
!     INITIALIZE ADJUSTMENT FACTORS
        DO JYR = 1 , ECP_D_FPH
          DEMCERS(JYR) = 1.0
        END DO
!     ADJUST FOR DSM
        IF(USW_DSM .GT. 0)THEN
!     LOOP OVER CURRENT AND PLANNING YEARS
         DO JYR = 1 , ECP_D_FPH
            KYR = CURIYR + JYR - 1
            BYR = MIN(KYR,UNYEAR)
          IF(QELRSDSM(BYR) .GT. 0.0)THEN
!     COMPUTE DSM ADJUSTMENT FACTOR TO CURRENT DEMANDS AND
           IF(JYR .EQ. 1)THEN
            DFAC = QELRSDSM(BYR) * 3.412 / QELRS(MNUMCR,KYR)
           ELSE
!     COMPUTE DSM ADJUSTMENT FACTOR TO EXPECTED DEMANDS
             XQELRS(MNUMCR,KYR) = 0.0
            DO JRG = 1 , MNUMCR - 1
             XQELRS(MNUMCR,KYR) = XQELRS(MNUMCR,KYR) + XQELRS(JRG,KYR)
            END DO
!     COMPUTE ADJUSTMENT FACTOR TO EXPECTED DEMANDS FOR DSM
             DFAC = QELRSDSM(BYR) * 3.412 / XQELRS(MNUMCR,KYR)
           END IF                                ! JYR
             DEMCERS(JYR) = DEMCERS(JYR) - DFAC
          END IF                                 ! QELRSDSM
         END DO                                  ! JYR
        END IF                                   ! USW_DSM
!     REDUCE ALL CURRENT REGIONAL DEMANDS PROPORTIONATELY
      END IF                                     ! EXR
!     COMMERCIAL
      IF(EXK .EQ. 1 .OR. CURITR .EQ. 1)THEN
!     INITIALIZE ADJUSTMENT FACTORS
        DO JYR = 1 , ECP_D_FPH
          DEMCECM(JYR) = 1.0
        END DO
!     ADJUST FOR DSM
        IF(USW_DSM .GT. 0)THEN
!     LOOP OVER CURRENT AND PLANNING YEARS
         DO JYR = 1 , ECP_D_FPH
            KYR = CURIYR + JYR - 1
            BYR = MIN(KYR,UNYEAR)
          IF(QELCMDSM(BYR) .GT. 0.0)THEN
!     COMPUTE DSM ADJUSTMENT FACTOR TO CURRENT DEMANDS
           IF(JYR .EQ. 1)THEN
            DFAC = QELCMDSM(BYR) * 3.412 / QELCM(MNUMCR,KYR)
           ELSE
!     COMPUTE DSM ADJUSTMENT FACTOR TO EXPECTED DEMANDS
             XQELCM(MNUMCR,KYR) = 0.0
            DO JRG = 1 , MNUMCR - 1
             XQELCM(MNUMCR,KYR) = XQELCM(MNUMCR,KYR) + XQELCM(JRG,KYR)
            END DO
!     COMPUTE ADJUSTMENT FACTOR TO EXPECTED DEMANDS FOR DSM
             DFAC = QELCMDSM(BYR) * 3.412 / XQELCM(MNUMCR,KYR)
           END IF                                ! JYR
             DEMCECM(JYR) = DEMCECM(JYR) - DFAC
          END IF                                 ! QELCMDSM
         END DO                                  ! JYR
        END IF                                   ! USW_DSM
      END IF                                     ! EXK
!     INDUSTRIAL
      IF(EXI .EQ. 1 .OR. CURITR .EQ. 1)THEN
!     INITIALIZE ADJUSTMENT FACTORS
        DO JYR = 1 , ECP_D_FPH
          DEMCEIN(JYR) = 1.0
        END DO
!     ADJUST FOR DSM
      IF(USW_DSM .GT. 0)THEN
!     LOOP OVER CURRENT AND PLANNING YEARS
         DO JYR = 1 , ECP_D_FPH
            KYR = CURIYR + JYR - 1
            BYR = MIN(KYR,UNYEAR)
          IF(QELINDSM(BYR) .GT. 0.0)THEN
!     COMPUTE DSM ADJUSTMENT FACTOR TO CURRENT DEMANDS
           IF(JYR .EQ. 1)THEN
            DFAC = QELINDSM(BYR) * 3.412 / QELIN(MNUMCR,KYR)
           ELSE
!     COMPUTE DSM ADJUSTMENT TO EXPECTED DEMANDS
             XQELIN(MNUMCR,KYR) = 0.0
            DO JRG = 1 , MNUMCR - 1
             XQELIN(MNUMCR,KYR) = XQELIN(MNUMCR,KYR) + XQELIN(JRG,KYR)
            END DO
!     COMPUTE ADJUSTMENT FACTOR TO EXPECTED DEMANDS FOR DSM
             DFAC = QELINDSM(BYR) * 3.412 / XQELIN(MNUMCR,KYR)
           END IF                                ! JYR
             DEMCEIN(JYR) = DEMCEIN(JYR) - DFAC
          END IF                                 ! QELINDSM
         END DO                                  ! JYR
        END IF                                   ! USW_DSM
      END IF                                     ! EXI
!     TRANSPORTATION
      IF(EXT .EQ. 1 .OR. CURITR .EQ. 1)THEN
!     INITIALIZE ADJUSTMENT FACTORS
        DO JYR = 1 , ECP_D_FPH
          DEMCETR(JYR) = 1.0
        END DO
!     ADJUST FOR DSM
        IF(USW_DSM .GT. 0)THEN
!     LOOP OVER CURRENT AND PLANNING YEARS
         DO JYR = 1 , ECP_D_FPH
            KYR = CURIYR + JYR - 1
            BYR = MIN(KYR,UNYEAR)
!     INITIALIZE ADJUSTMENT FACTORS
            DEMCETR(JYR) = 1.0
          IF(QELTRDSM(BYR) .GT. 0.0)THEN
!     COMPUTE DSM ADJUSTMENT FACTOR TO CURRENT DEMANDS
           IF(JYR .EQ. 1)THEN
            DFAC = QELTRDSM(BYR) * 3.412 / QELTR(MNUMCR,KYR)
           ELSE
!     COMPUTE DSM ADJUSTMENT FACTOR TO EXPECTED DEMANDS
             XQELTR(MNUMCR,KYR) = 0.0
            DO JRG = 1 , MNUMCR - 1
             XQELTR(MNUMCR,KYR) = XQELTR(MNUMCR,KYR) + XQELTR(JRG,KYR)
            END DO
!     COMPUTE ADJUSTMENT FACTOR TO EXPECTED DEMANDS FOR DSM
             DFAC = QELTRDSM(BYR) * 3.412 / XQELTR(MNUMCR,BYR)
           END IF                                ! JYR
             DEMCETR(JYR) = DEMCETR(JYR) - DFAC
          END IF                                 ! QELTRDSM
         END DO                                  ! JYR
        END IF                                   ! USW_DSM
      END IF                                     ! EXT
!     GET AVERAGE DSM ADJUSTMENT FACTOR FOR CURRENT YEAR
         DEMCEAS(1) = (QELRS(MNUMCR,CURIYR) * DEMCERS(1) + &
                       QELCM(MNUMCR,CURIYR) * DEMCECM(1) +      &
                       QELIN(MNUMCR,CURIYR) * DEMCEIN(1) +      &
                       QELTR(MNUMCR,CURIYR) * DEMCETR(1)) / &
                       QELAS(MNUMCR,CURIYR)
      END IF                                     ! DEMADJ
!
!     ADJUST DEMANDS IN CECA RUNS IF DSM OR DISTRIBUTED GENERATION
!
      IF(DEMADJ .EQ. 2)THEN
      IF(USW_DSM .GT. 0)THEN
!     CENSUS DIVISION DEMANDS
        DO JRG = 1 , MNUMCR
         QELRS(JRG,CURIYR) = QELRS(JRG,CURIYR) * &
                             DEMCERS(1)
         QELCM(JRG,CURIYR) = QELCM(JRG,CURIYR) * &
                             DEMCECM(1)
         QELIN(JRG,CURIYR) = QELIN(JRG,CURIYR) * &
                             DEMCEIN(1)
         QELTR(JRG,CURIYR) = QELTR(JRG,CURIYR) * &
                             DEMCETR(1)
         QELAS(JRG,CURIYR) = QELAS(JRG,CURIYR) * &
                             DEMCEAS(1)
        END DO
       END IF                                     ! USW_DSM
      END IF                                     ! DEMADJ
      RETURN
      END
!
!
      SUBROUTINE HIELADJ

      IMPLICIT NONE

!     THIS SUBROUTINE ADJUSTS DEMAND VARIABLES FOR HIGH ELECTRICITY DEMAND CASE

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'qblk'
      include 'control'
      include 'uefdout'
      INTEGER ICENSUS

      WRITE(6,9601)CURIYR,CURITR,FCRL,MIN(UNYEAR,LASTYR),UNRGNS
9601  FORMAT(2X,'FROM UTIL:CURIYR,CURITR,FCRL,MIN,UNRGNS:',5I5)
9596  FORMAT(1H ,'YR,IRG,RS,CM,IN,TR,AS',I5,2I3,10F8.2)

      DO ICENSUS = 1,MNUMCR
      WRITE(6,9596) CURIYR + UHBSYR,CURITR,ICENSUS, &
      QELRS(ICENSUS,CURIYR), &
      QELRS(ICENSUS,CURIYR) * UQELFAC(CURIYR), &
      QELCM(ICENSUS,CURIYR), &
      QELCM(ICENSUS,CURIYR) * UQELFAC(CURIYR), &
      QELIN(ICENSUS,CURIYR), &
      QELIN(ICENSUS,CURIYR) * UQELFAC(CURIYR), &
      QELTR(ICENSUS,CURIYR), &
      QELTR(ICENSUS,CURIYR) * UQELFAC(CURIYR), &
      QELAS(ICENSUS,CURIYR), &
      QELAS(ICENSUS,CURIYR) * UQELFAC(CURIYR)
      QELRS(ICENSUS,CURIYR) = QELRS(ICENSUS,CURIYR)*UQELFAC(CURIYR)
      QELCM(ICENSUS,CURIYR) = QELCM(ICENSUS,CURIYR)*UQELFAC(CURIYR)
      QELIN(ICENSUS,CURIYR) = QELIN(ICENSUS,CURIYR)*UQELFAC(CURIYR)
      QELTR(ICENSUS,CURIYR) = QELTR(ICENSUS,CURIYR)*UQELFAC(CURIYR)
      QELAS(ICENSUS,CURIYR) = QELAS(ICENSUS,CURIYR)*UQELFAC(CURIYR)
      END DO

      RETURN
      END
!
!
      SUBROUTINE WRHISDB
!
      IMPLICIT NONE
!
!     THIS SUBROUTINE WRITES HISTORICAL OVERWRITES AND ALASKA/HAWAII INFORMATION TO OUTPUT ACCESS DATABASE

      include 'parametr'
      include 'emmparm'
      include 'control'
      include 'dispin'
      include 'dispinyr'
      include 'dispout'
      include 'dispett'
      include 'plntctl'
      include 'ncntrl'
      include 'eusprc'
      include 'edbdef'
      include 'emission'
      include 'uecpout'
      include 'uefdout'
      include 'elout'
      include 'elcntl'
      include 'cogen'
!
      INTEGER IYR,IRG,ISP,I,J,IOWN,K,IVIN,IS,ILD,IV
      INTEGER IFP,IFLTP,LTNUM,IGRP,IFL
      INTEGER NUMTABS,HAWAII,ALASKA
      PARAMETER (NUMTABS = 2)        ! total number of database tables
      REAL*8 CF,DUMMY,QPGN,QFGN,QHGN,QDGN,SCAP,QFFL,DMY(EFD_D_OWN)
      CHARACTER*2 COL
!
      LOOPING = 0
      NUMCOLS = 0
      DYNSTM = ' '
      WRTSTM = ' '
      COLVALS = 0.0
      COLV = 0.0
      CHCOLVALS = ' '
      CHCOLV = ' '

!
      DUMMY = 0.0
      COL = ' :'
!
      ALASKA = MNUMNR - 2
      HAWAII = MNUMNR - 1
!
      DO IRG = 1 , MNUMNR - 1
        DO IOWN = 1 , 3
          DO IFL = 1 , UNFUELS
!
            IF ( (IRG .EQ. HAWAII) .OR. (IRG .EQ. ALASKA) ) THEN
              IF ( IOWN .LE. 2 ) THEN
!
                TNUM = 1
                IF (LOOPING(TNUM) .EQ. 0) THEN
                  NUMCOLS(TNUM) = 7
                  DYNSTM(TNUM) = 'INSERT INTO EFD_ALHA_GENCON VALUES(?,?,?,?,?,?,?,?)'
                  WRTSTM(TNUM) = 'EFD_ALHA_GENCON'
                ENDIF
                LOOPING(TNUM) = LOOPING(TNUM) + 1
                COLV(TNUM,1,LOOPING(TNUM)) = CURIYR
                COLV(TNUM,2,LOOPING(TNUM)) = CURITR
                COLV(TNUM,3,LOOPING(TNUM)) = IRG
                COLV(TNUM,4,LOOPING(TNUM)) = IOWN
                COLV(TNUM,5,LOOPING(TNUM)) = IFL
!
                IF ( IFL .EQ. UIH1 ) THEN
                  COLV(TNUM,6,LOOPING(TNUM)) = UGNCLNR(IOWN,IRG,CURIYR)
                  COLV(TNUM,7,LOOPING(TNUM)) = UFLCLNR(IOWN,IRG,CURIYR)
                ELSEIF ( IFL .EQ. UIDS) THEN
                  COLV(TNUM,6,LOOPING(TNUM)) = UGNDSNR(IOWN,IRG,CURIYR)
                  COLV(TNUM,7,LOOPING(TNUM)) = UFLDSNR(IOWN,IRG,CURIYR)
                ELSEIF ( IFL .EQ. UIRL) THEN
                  COLV(TNUM,6,LOOPING(TNUM)) = UGNRLNR(IOWN,IRG,CURIYR)
                  COLV(TNUM,7,LOOPING(TNUM)) = UFLRLNR(IOWN,IRG,CURIYR)
                ELSEIF ( IFL .EQ. UIRH) THEN
                  COLV(TNUM,6,LOOPING(TNUM)) = UGNRHNR(IOWN,IRG,CURIYR)
                  COLV(TNUM,7,LOOPING(TNUM)) = UFLRHNR(IOWN,IRG,CURIYR)
                ELSEIF ( IFL .EQ. UIGF) THEN
                  COLV(TNUM,6,LOOPING(TNUM)) = UGNGFNR(IOWN,IRG,CURIYR)
                  COLV(TNUM,7,LOOPING(TNUM)) = UFLGFNR(IOWN,IRG,CURIYR)
                ELSEIF ( IFL .EQ. UIGI) THEN
                  COLV(TNUM,6,LOOPING(TNUM)) = UGNGINR(IOWN,IRG,CURIYR)
                  COLV(TNUM,7,LOOPING(TNUM)) = UFLGINR(IOWN,IRG,CURIYR)
                ELSEIF ( IFL .EQ. UIGC) THEN
                  COLV(TNUM,6,LOOPING(TNUM)) = UGNGCNR(IOWN,IRG,CURIYR)
                  COLV(TNUM,7,LOOPING(TNUM)) = UFLGCNR(IOWN,IRG,CURIYR)
                ELSEIF ( IFL .EQ. UIUF) THEN
                  COLV(TNUM,6,LOOPING(TNUM)) = UGNURNR(IOWN,IRG,CURIYR)
                  COLV(TNUM,7,LOOPING(TNUM)) = UFLURNR(IOWN,IRG,CURIYR)
                ELSEIF ( IFL .EQ. UIWA) THEN
                  COLV(TNUM,6,LOOPING(TNUM)) = UGNHYNR(IOWN,IRG,CURIYR)
                  COLV(TNUM,7,LOOPING(TNUM)) = 0.0
                ELSEIF ( IFL .EQ. UIPS) THEN
                  COLV(TNUM,6,LOOPING(TNUM)) = UGNPSNR(IOWN,IRG,CURIYR)
                  COLV(TNUM,7,LOOPING(TNUM)) = 0.0
                ELSEIF ( IFL .EQ. UIGT) THEN
                  COLV(TNUM,6,LOOPING(TNUM)) = UGNGENR(IOWN,IRG,CURIYR)
                  COLV(TNUM,7,LOOPING(TNUM)) = 0.0
                ELSEIF ( IFL .EQ. UISW) THEN
                  COLV(TNUM,6,LOOPING(TNUM)) = UGNMSNR(IOWN,IRG,CURIYR)
                  COLV(TNUM,7,LOOPING(TNUM)) = 0.0
                ELSEIF ( IFL .EQ. UIWD) THEN
                  COLV(TNUM,6,LOOPING(TNUM)) = UGNWDNR(IOWN,IRG,CURIYR)
                  COLV(TNUM,7,LOOPING(TNUM)) = 0.0
                ELSEIF ( IFL .EQ. UISO) THEN
                  COLV(TNUM,6,LOOPING(TNUM)) = UGNSONR(IOWN,IRG,CURIYR)
                  COLV(TNUM,7,LOOPING(TNUM)) = 0.0
                ELSEIF ( IFL .EQ. UIPV) THEN
                  COLV(TNUM,6,LOOPING(TNUM)) = UGNPVNR(IOWN,IRG,CURIYR)
                  COLV(TNUM,7,LOOPING(TNUM)) = 0.0
                ELSEIF ( IFL .EQ. UIPT) THEN
                  COLV(TNUM,6,LOOPING(TNUM)) = UGNPTNR(IOWN,IRG,CURIYR)
                  COLV(TNUM,7,LOOPING(TNUM)) = 0.0
                ELSEIF ( IFL .EQ. UIWN) THEN
                  COLV(TNUM,6,LOOPING(TNUM)) = UGNWNNR(IOWN,IRG,CURIYR)
                  COLV(TNUM,7,LOOPING(TNUM)) = 0.0
                ELSEIF ( IFL .EQ. UIWL) THEN
                  COLV(TNUM,6,LOOPING(TNUM)) = UGNWLNR(IOWN,IRG,CURIYR)
                  COLV(TNUM,7,LOOPING(TNUM)) = 0.0
                ELSE
                  COLV(TNUM,6,LOOPING(TNUM)) = 0.0
                  COLV(TNUM,7,LOOPING(TNUM)) = 0.0
                ENDIF

!
                IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
                  COLVALS(:,:) = COLV(TNUM,:,:)
!                 CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                  CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                  LOOPING(TNUM) = 0
                ENDIF
!
              ENDIF                                          ! end owner if
            ENDIF                                                     ! end if alaska/hawaii
!
!           IF Historical Year then write historical overwrites to database
!
            IF (((CURIYR + UHBSYR) .LE. UYR_OVER)     &
             .AND. ((CURIYR + UHBSYR) .GE. HSTYEAR)) THEN
!
                TNUM = 2
                IF (LOOPING(TNUM) .EQ. 0) THEN
                  NUMCOLS(TNUM) = 7
                  DYNSTM(TNUM) = 'INSERT INTO EFD_HISTOVER_GENCON VALUES(?,?,?,?,?,?,?,?)'
                  WRTSTM(TNUM) = 'EFD_HISTOVER_GENCON'
                ENDIF
                LOOPING(TNUM) = LOOPING(TNUM) + 1
                COLV(TNUM,1,LOOPING(TNUM)) = CURIYR
                COLV(TNUM,2,LOOPING(TNUM)) = CURITR
                COLV(TNUM,3,LOOPING(TNUM)) = IRG
                COLV(TNUM,4,LOOPING(TNUM)) = IOWN
                COLV(TNUM,5,LOOPING(TNUM)) = IFL
!
                IF ( IOWN .LE. 2 ) THEN
                  IF ( IFL .EQ. UIH1 ) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = UGNCLNR(IOWN,IRG,CURIYR)
                    COLV(TNUM,7,LOOPING(TNUM)) = UFLCLNR(IOWN,IRG,CURIYR)
                  ELSEIF ( IFL .EQ. UIDS) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = UGNDSNR(IOWN,IRG,CURIYR)
                    COLV(TNUM,7,LOOPING(TNUM)) = UFLDSNR(IOWN,IRG,CURIYR)
                  ELSEIF ( IFL .EQ. UIRL) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = UGNRLNR(IOWN,IRG,CURIYR)
                    COLV(TNUM,7,LOOPING(TNUM)) = UFLRLNR(IOWN,IRG,CURIYR)
                  ELSEIF ( IFL .EQ. UIRH) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = UGNRHNR(IOWN,IRG,CURIYR)
                    COLV(TNUM,7,LOOPING(TNUM)) = UFLRHNR(IOWN,IRG,CURIYR)
                  ELSEIF ( IFL .EQ. UIGF) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = UGNGFNR(IOWN,IRG,CURIYR)
                    COLV(TNUM,7,LOOPING(TNUM)) = UFLGFNR(IOWN,IRG,CURIYR)
                  ELSEIF ( IFL .EQ. UIGI) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = UGNGINR(IOWN,IRG,CURIYR)
                    COLV(TNUM,7,LOOPING(TNUM)) = UFLGINR(IOWN,IRG,CURIYR)
                  ELSEIF ( IFL .EQ. UIGC) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = UGNGCNR(IOWN,IRG,CURIYR)
                    COLV(TNUM,7,LOOPING(TNUM)) = UFLGCNR(IOWN,IRG,CURIYR)
                  ELSEIF ( IFL .EQ. UIUF) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = UGNURNR(IOWN,IRG,CURIYR)
                    COLV(TNUM,7,LOOPING(TNUM)) = UFLURNR(IOWN,IRG,CURIYR)
                  ELSEIF ( IFL .EQ. UIWA) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = UGNHYNR(IOWN,IRG,CURIYR)
                    COLV(TNUM,7,LOOPING(TNUM)) = 0.0
                  ELSEIF ( IFL .EQ. UIPS) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = UGNPSNR(IOWN,IRG,CURIYR)
                    COLV(TNUM,7,LOOPING(TNUM)) = 0.0
                  ELSEIF ( IFL .EQ. UIGT) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = UGNGENR(IOWN,IRG,CURIYR)
                    COLV(TNUM,7,LOOPING(TNUM)) = 0.0
                  ELSEIF ( IFL .EQ. UISW) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = UGNMSNR(IOWN,IRG,CURIYR)
                    COLV(TNUM,7,LOOPING(TNUM)) = 0.0
                  ELSEIF ( IFL .EQ. UIWD) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = UGNWDNR(IOWN,IRG,CURIYR)
                    COLV(TNUM,7,LOOPING(TNUM)) = 0.0
                  ELSEIF ( IFL .EQ. UISO) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = UGNSONR(IOWN,IRG,CURIYR)
                    COLV(TNUM,7,LOOPING(TNUM)) = 0.0
                  ELSEIF ( IFL .EQ. UIPV) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = UGNPVNR(IOWN,IRG,CURIYR)
                    COLV(TNUM,7,LOOPING(TNUM)) = 0.0
                  ELSEIF ( IFL .EQ. UIPT) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = UGNPTNR(IOWN,IRG,CURIYR)
                    COLV(TNUM,7,LOOPING(TNUM)) = 0.0
                  ELSEIF ( IFL .EQ. UIWN) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = UGNWNNR(IOWN,IRG,CURIYR)
                    COLV(TNUM,7,LOOPING(TNUM)) = 0.0
                  ELSEIF ( IFL .EQ. UIWL) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = UGNWLNR(IOWN,IRG,CURIYR)
                    COLV(TNUM,7,LOOPING(TNUM)) = 0.0
                  ELSE
                    COLV(TNUM,6,LOOPING(TNUM)) = 0.0
                    COLV(TNUM,7,LOOPING(TNUM)) = 0.0
                  ENDIF
                ELSEIF ( IOWN .EQ. 3 ) THEN
                  IF ( IFL .EQ. UIH1 ) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = ( CGNTGEN(IRG,CURIYR,1,1) + CGNTGEN(IRG,CURIYR,1,2) ) * 0.001
                    COLV(TNUM,7,LOOPING(TNUM)) = CGNTQ(IRG,CURIYR,1)
                  ELSEIF ( IFL .EQ. UIDS) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = ( CGNTGEN(IRG,CURIYR,2,1) + CGNTGEN(IRG,CURIYR,2,2) ) * 0.001
                    COLV(TNUM,7,LOOPING(TNUM)) = CGNTQ(IRG,CURIYR,2)
                  ELSEIF ( IFL .EQ. UIGF) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = ( CGNTGEN(IRG,CURIYR,3,1) + CGNTGEN(IRG,CURIYR,3,2) ) * 0.001
                    COLV(TNUM,7,LOOPING(TNUM)) = CGNTQ(IRG,CURIYR,3)
                  ELSEIF ( IFL .EQ. UIWA) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = ( CGNTGEN(IRG,CURIYR,4,1) + CGNTGEN(IRG,CURIYR,4,2) ) * 0.001
                    COLV(TNUM,7,LOOPING(TNUM)) = CGNTQ(IRG,CURIYR,4)
                  ELSEIF ( IFL .EQ. UIGT) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = ( CGNTGEN(IRG,CURIYR,5,1) + CGNTGEN(IRG,CURIYR,5,2) ) * 0.001
                    COLV(TNUM,7,LOOPING(TNUM)) = CGNTQ(IRG,CURIYR,5)
                  ELSEIF ( IFL .EQ. UISW) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = ( CGNTGEN(IRG,CURIYR,6,1) + CGNTGEN(IRG,CURIYR,6,2) ) * 0.001
                    COLV(TNUM,7,LOOPING(TNUM)) = CGNTQ(IRG,CURIYR,6)
                  ELSEIF ( IFL .EQ. UIWD) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = ( CGNTGEN(IRG,CURIYR,7,1) + CGNTGEN(IRG,CURIYR,7,2) ) * 0.001
                    COLV(TNUM,7,LOOPING(TNUM)) = CGNTQ(IRG,CURIYR,7)
                  ELSEIF ( IFL .EQ. UISO) THEN
                    COLV(TNUM,6,LOOPING(TNUM)) = ( CGNTGEN(IRG,CURIYR,8,1) + CGNTGEN(IRG,CURIYR,8,2) ) * 0.001
                    COLV(TNUM,7,LOOPING(TNUM)) = CGNTQ(IRG,CURIYR,8)
                  ELSE
                    COLV(TNUM,6,LOOPING(TNUM)) = 0.0
                    COLV(TNUM,7,LOOPING(TNUM)) = 0.0
                  ENDIF
!
                ENDIF                                     ! End owner if
                IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
                  COLVALS(:,:) = COLV(TNUM,:,:)
!                 CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                  CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                  LOOPING(TNUM) = 0
                ENDIF

            ENDIF                                        ! end if historical year
!
          ENDDO                                         ! end fuel do
       ENDDO                                           ! End Owner do
     ENDDO                                             ! End region do
!
!      --- WRITE OUT ANY LEFT OVER RECORDS TO THE NEMS DATA BASE
     DO TNUM = 1 , NUMTABS
       IF (LOOPING(TNUM) .NE. 0) THEN
         COLVALS(:,:) = COLV(TNUM,:,:)
!        CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
         CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
         LOOPING(TNUM) = 0
       ENDIF
     ENDDO
!
     RETURN
     END
!
!
      SUBROUTINE SGRIDADJ
      IMPLICIT NONE
!
!     THIS SUBROUTINE ADJUSTS LOSSES FOR REDUCTIONS IN DEMAND IN SMART GRID RUNS
!     IT ADJUSTS TRANSMISSION LIMITS BY SMART GRID FACTOR

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'ecpcntl'
      include 'control'
      include 'bildin'
      include 'qblk'
      include 'mxqblk'
      include 'dsmdimen'
      include 'postpr'
      include 'dispett'
      include 'dispinyr'
      include 'dsmsectr'
      include 'dsmnercr'
      include 'eusprc'
      include 'uecpout'
      include 'efpint'
      include 'uefdout'
!
      REAL CensusValues(MNUMCR),demout
      REAL qdem,demchpct,lossadj
      INTEGER CRG,IRG,IECPYR,FULLYR,YR,SGRDYR

      FULLYR = USYEAR(CURIYR)
      SGRDYR = UYR_SGRID - BASEYR
!     write(6,*) ' inside sgrid routine fullyr sgrdyr ',fullyr,sgrdyr

!     write(6,*) ' qdem adj uyrhst ',curiyr,fullyr,usw_dmlsadj,uyr_dmlsadj
!
      DO IRG = 1 , UNRGNS

!       Sum sector demands for each nerc region.

        Qdem = 0.0
        DO CRG = 1 , MNUMCR - 2
           CensusValues(CRG) = QELRS(CRG,CURIYR)
        END DO
        CALL DSMEMMV(CensusValues,IRG,SEC(RES),demout)
        Qdem = Qdem + demout

        DO CRG = 1 , MNUMCR - 2
           CensusValues(CRG) = QELCM(CRG,CURIYR)
        END DO
        CALL DSMEMMV(CensusValues,IRG,SEC(COM),demout)
        Qdem = Qdem + demout

        DO CRG = 1 , MNUMCR - 2
           CensusValues(CRG) = QELIN(CRG,CURIYR)
        END DO
        CALL DSMEMMV(CensusValues,IRG,SEC(IND),demout)
        Qdem = Qdem + demout

        DO CRG = 1 , MNUMCR - 2
           CensusValues(CRG) = QELTR(CRG,CURIYR)
        END DO
         CALL DSMEMMV(CensusValues,IRG,SEC(TRA),demout)
        Qdem = Qdem + demout

!       convert to billion kwh

        Qdem = Qdem * UNCONFA/1000.0
!
!       if switch set calculate loss adjustment factor
!
        LOSSADJ = 0.0
!
        IF ( ( USW_DMLSADJ .GE. 1 ) .AND. ( FULLYR .GE. UYR_DMLSADJ ) ) THEN
!
               CALL GETEIJ(CURIYR)
!                write(6,*) ' after geteij uqtdls ',curiyr,irg,uqtdls(irg)
               DEMCHPCT = QDEM / ULABASEDMD(CURIYR,IRG)
!              write(6,*) ' loss adj 1 ',curiyr,irg,qdem,ulabasedmd(curiyr,irg),udmlosstol,demchpct
               IF ( DEMCHPCT .LT. (1.0 - UDMLOSSTOL) ) THEN
                LOSSADJ = ( 1.0 - DEMCHPCT)
               ELSE
                LOSSADJ = 0.0
               ENDIF
               IF ( USW_DMLSADJ .EQ. 2 ) THEN
                 IF ( DEMCHPCT .GT. (1.0 + UDMLOSSTOL) ) THEN
                  LOSSADJ = ( DEMCHPCT - 1.0 )
                 ELSE
                  LOSSADJ = 0.0
                 ENDIF
               ENDIF
!
!              Apply loss adjustment
!              Convert loss adjustment to percentage
!
!              write(6,*) ' loss adj before ',curiyr,irg,demchpct,lossadj,udmlossadj(irg),uqtdls(Irg)
               IF ( USW_DMLSADJ .EQ. 1 ) THEN
                 UQTDLS(IRG) = UQTDLS(IRG) - ( LOSSADJ * UDMLOSSADJ(IRG) )
               ELSEIF ( USW_DMLSADJ .EQ. 2 ) THEN
                 IF ( DEMCHPCT .LT. 1.0 ) THEN
                   UQTDLS(IRG) = UQTDLS(IRG) - ( LOSSADJ * UDMLOSSADJ(IRG) )
                 ELSEIF ( DEMCHPCT .GT. 1.0 ) THEN
                   UQTDLS(IRG) = UQTDLS(IRG) + ( LOSSADJ * UDMLOSSADJ(IRG) )
                 ENDIF
!
                 NERCTDLOSS(IRG) = UQTDLS(IRG)
!
               ENDIF
!              write(6,*) ' loss adj after ',curiyr,irg,lossadj,uqtdls(Irg)
               CALL STREIJ(CURIYR)
         ENDIF
      END DO
!
!  calculate costs/kwh to be added to O&M costs to account for the cost of implementing smart grid
!
!
      IF ( USW_SGRID .GE. 1 ) THEN
       IF ( (FULLYR .EQ. UESTYR) .AND. (CURITR .EQ. 1) ) THEN
          OMTADDER(:,:) = 0.0
          CPTADDER(:,:) = 0.0
          DO YR = SGRDYR , MNUMYR
            DO IRG = 1 , UNRGNS
!             million $ / billion kwh = mills/kwh
!             OMTADDER(IRG,CURIYR) = USGCOST(CURIYR,IRG) / Qdem
!             do not divide by demand, will be added in to total O&M costs (MM$) in efp before dividing by sales
!             OMTADDER(IRG,YR) = USGCOST(YR,IRG)
              CPTADDER(IRG,YR) = USGCOST(YR,IRG)
            ENDDO
          ENDDO
        ENDIF
      ENDIF
!
!ajdust transmission capacity by improvement factor, first year only??
! only in first year??
!

      IF ( USW_SGRID .GE. 1 ) THEN
       IF ( (FULLYR .EQ. UESTYR) .AND. (CURITR .EQ. 1) ) THEN
         DO IRGEX = 1 ,  UNRGNS
           DO IRGIM = 1 , UNRGNS
             DO YR = SGRDYR , MNUMYR
!              write(6,*) ' sg cnst sdj ',yr,irgim,irgex,usgtcapimp,cnstrnts_efd(1,yr,irgim,irgex)
               CNSTRNTS_ECP(:,YR,IRGIM,IRGEX) = CNSTRNTS_ECP(:,YR,IRGIM,IRGEX) * ( 1.0 + USGTCAPIMP )
               CNSTRNTS_EFD(:,YR,IRGIM,IRGEX) = CNSTRNTS_EFD(:,YR,IRGIM,IRGEX) * ( 1.0 + USGTCAPIMP )
!              write(6,*) ' sg cnst sdj aft ',yr,irgim,irgex,usgtcapimp,cnstrnts_efd(1,yr,irgim,irgex)
             ENDDO
           ENDDO
         ENDDO
       ENDIF
      ENDIF
!
!     adjust EFP transmission variables for smart grid
!
      IF ( (FULLYR .EQ. UESTYR + 1) .AND. (CURITR .EQ. 1) .AND. (USW_SGRID .GE. 1 ) ) THEN
!
          DO IRG = 1 , UNRGNS
!
            DO YR = SGRDYR , MNUMYR
!             write(6,25) 'CAPTSCALE in util before',irg,yr,captscale(irg,yr)
!             write(6,25) 'OMTSCALE in util before',irg,yr,omtscale(irg,yr)
!
              CAPTSCALE(irg,YR) = CAPTSCALE(irg,YR) * (1.0 - USGTDIMP)
              OMTSCALE(irg,YR) = OMTSCALE(irg,YR) * (1.0 - USGTDOMIMP)
!
!             write(6,25) 'OMTSCALE in util after',irg,yr,omtscale(irg,yr)
!             write(6,25) 'CAPTSCALE in util after',irg,yr,captscale(irg,yr)
!
            ENDDO
          ENDDO
!
25      format(1x,a20,2x,i6,2x,I6,1x,f6.2)
      ENDIF
!
!     IF SMART GRID RUN MULTIPLY TRANSMISSION COSTS BY T&D EFFICIENCY IMPROVEMENT FACTOR
!
      IF ( (FULLYR .EQ. UESTYR + 1) .AND. (CURITR .EQ. 1) .AND. (USW_SGRID .GE. 1 ) ) THEN
!
!       write(6,*) ' fl_cnxt_cst before sg ',fl_cnxt_cst
!       write(6,*) ' emm_cnxt_cst before sg ',emm_cnxt_cst
!
           FL_CNXT_CST(:,:) = FL_CNXT_CST(:,:) * (1.0 - USGTDIMP)
           EMM_CNXT_CST(:,:) = EMM_CNXT_CST(:,:) * (1.0 - USGTDIMP)
!
           DO IRG = 1 , UNRGNS
             CALL GETBLD(1,IRG)
!       write(6,*) ' smgrid epcovr before ',epcovr,trctlovr(irg)
             EPCOVR(:) = EPCOVR(:) * ( 1.0 - USGTDIMP)
             TRCTLOVR(IRG) = EPCOVR(WIIG)
!       write(6,*) ' smgrid epcovr after ',epcovr,trctlovr(irg)
             CALL STRBLD(1,IRG)
           END DO
!
!       write(6,*) ' fl_cnxt_cst after sg ',fl_cnxt_cst
!       write(6,*) ' emm_cnxt_cst after sg ',emm_cnxt_cst
!
      ENDIF
!
!     RELIABILITY CHANGES FOR SMART GRID
!
!     OPTION 1 - adjust load uncertainity
!
      IF ( (CURITR .EQ. 1) .AND. (USW_SGRID .GE. 1) .AND. &
        (FULLYR .GE. UYR_SGREL) .AND. (FULLYR .LE. (UYR_SGREL + UNYRSGREL)) )  THEN
!
!       write(6,*) ' loadu sgrid before ',curiyr,loadu,loadecpu
        LOADU = LOADU * ( 1.0 - USGRELIMP )
        LOADECPU = LOADECPU * ( 1.0 - USGRELIMP )
!       write(6,*) ' loadu sgrid after  ',curiyr,loadu,loadecpu
!
!     OPTION 2 - adjust forced outage rate
!
!       DO IP = 1 , EFD_D_CAP
!         write(6,*) ' wfor ip before ',curiyr,ip,usgrelimp,wfor(ip)
!         WFOR(IP) = WFOR(IP) * ( 1.0 - USGRELIMP )
!         write(6,*) ' wfor ip after  ',ip,wfor(ip)
!       ENDDO
!       DO IP = 1 , ECP_D_CAP
!         write(6,*) ' upfort ip before ',curiyr,ip,usgrelimp,upfort(ip)
!         UPFORT(IP) = UPFORT(IP) * ( 1.0 - USGRELIMP )
!         write(6,*) ' upfort ip after ',ip,upfort(ip)
!       ENDDO
!
      ENDIF




      RETURN
      END
!
!
      SUBROUTINE WRITE_DB_DATA (WRTESTMT, NUMCOL, LOOPINGS, COLVAL, CHCOLVAL, MSGFIL)
!
      IMPLICIT NONE

!     THIS SUBROUTINE WRITES DATA TO THE OUTPUT ACCESS DATABASE TABLES

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'ecpcntl'
      include 'eusprc'
      include 'edbdef'

      REAL*8 COLVAL(MAXCOLS,MAXRECS),COLS(MAXCOLS)
      INTEGER NUMCOL,LOOPINGS,MSGFIL,ICOL,I,CHCOLIND(MAXCOLS),NUMCHCOLS
      INTEGER LOAD_TIME_BEGIN,LOAD_TIME_END,IMSG,ICHCOLS
      CHARACTER*40 CHCOLVAL(MAXCOLS,MAXRECS),CHCOLS(MAXCOLS)
      CHARACTER*35 TBLENAME,WRTESTMT
      CHARACTER*1  QUOTE(maxcols)

      numchcols = 0
      tblename = trim(wrtestmt)
      quote = "'"
      IMSG = MSGFIL
!     IMSG = 6    ! for testing send writes to nohup.out
!
      write(imsg,*) ' in write db data   ',wrtestmt,numcol,loopings
!
      CALL MPTIM2(LOAD_TIME_BEGIN)
      WRITE (IMSG,1111) ' Begin write_DB_data table ',tblename,FLOAT(LOAD_TIME_BEGIN)/100.
1111  FORMAT(10X,A,1x,A,1x,' CPU TIME (SECONDS) = ',F7.2,A,F7.2)

!
     DO I = 1 , loopings
       numchcols = 0
       DO icol = 1 , numcol
         cols(icol) = colval(icol,i)
          IF ( CHCOLVAL(ICOL,I) .NE. ' ') THEN
            NUMCHCOLS = NUMCHCOLS + 1
            CHCOLIND(NUMCHCOLS) = ICOL
          ENDIF
         chcols(icol) = trim(chcolval(icol,I))
!        chcols(icol) = "'" // trim(chcols(icol)) // "'"
       enddo  ! end icol do

       IF ( NUMCHCOLS .EQ. 0 ) THEN
         write(UF_DBPGRP,2343)  tblename,numcol,numchcols,(cols(icol),icol=1,numcol)
       ELSE
         write(UF_DBPGRP,2345)  tblename,numcol,numchcols,(cols(icol),icol=1,numcol),       &
            (chcolind(ichcols),ichcols=1,numchcols),(quote(ichcols),trim(chcols(chcolind(ichcols))),quote(ichcols),ichcols=1,numchcols)
       ENDIF
!
     ENDDO   ! end loopings do
!
 2343 format(a,I3,2X,I3,1X<numcol>(f15.6,2x))
 2345 format(a,I3,2X,I3,1X<numcol>(f15.6,2x),2x,<numchcols>(I3,2x),<numchcols>(a1,a,a1,2x))
!
!      write(IMSG,*) ' end load_data '
      CALL MPTIM2(LOAD_TIME_END)
      WRITE (IMSG,2222) '** End write_DB_data table ',wrtestmt,FLOAT(LOAD_TIME_END)/100., &
       FLOAT(LOAD_TIME_END)/100. - FLOAT(LOAD_TIME_BEGIN)/100.
2222  FORMAT(10X,A,1x,a,' CPU TIME (SECONDS) = ',F7.2,', TIME USED =',F7.2)

      RETURN
      END
!
!
      SUBROUTINE DEFNTABS
!
      IMPLICIT NONE

!     THIS SUBROUTINE WRITES THE OUTPUT ACCESS DATABASE GLOBAL DEFINITION TABLES

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'eusprc'
      include 'edbdef'
      include 'macout'

      INTEGER I,IY,NRECORD,LLL,III,ITST,INDEX,INT,DBTYPE
      INTEGER CALYEAR,INDEXCOL
      CHARACTER*2  CI
      CHARACTER*25 CENRG(MNUMCR),NITR

      INTEGER NUMTABS,FULLYR
      PARAMETER (NUMTABS = 6)        ! total number of database tables

      DATA CENRG /'New England       ',  &
                   'Middle Atlantic   ',  &
                   'East North Central',  &
                   'West North Central',  &
                   'South Atlantic    ',  &
                   'East South Central',  &
                   'West South Central',  &
                   'Mountain          ',  &
                   'Pacific           ',  &
                   'California        ',  &
                   'Total US          '/
!
      LOOPING = 0
      NUMCOLS = 0
      DYNSTM = ' '
      WRTSTM = ' '
      COLVALS = 0.0
      COLV = 0.0
      CHCOLVALS = ' '
      CHCOLV = ' '

! ********************** Write to Oracle DEF tables  *******
!
      TNUM = 1
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 1
         DYNSTM(TNUM) = 'INSERT INTO VERSION_NUMBER VALUES(?)'
         WRTSTM(TNUM) = 'VERSION_NUMBER'
        ENDIF
        LOOPING(TNUM) = LOOPING(TNUM) + 1
        CHCOLV(TNUM,1,LOOPING(TNUM)) = Trim(Scen_date)
!     ENDIF
!
      TNUM = 2
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO CY_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'CY_DEF'
        ENDIF
        DO I = 1 , MNUMYR
         LOOPING(TNUM) = LOOPING(TNUM) + 1
         COLV(TNUM,1,LOOPING(TNUM)) = I
         COLV(TNUM,2,LOOPING(TNUM)) = BASEYR + I - 1
        ENDDO
!
!     ENDIF


    !********************** WRITE TO FY_DEF TABLE **************************
      TNUM = 3
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO FY_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'FY_DEF'
        ENDIF
        DO I = 1 , MNUMYR
         LOOPING(TNUM) = LOOPING(TNUM) + 1
         COLV(TNUM,1,LOOPING(TNUM)) = I
         COLV(TNUM,2,LOOPING(TNUM)) = BASEYR + I - 1
        ENDDO
!
!     ENDIF

!********************** WRITE TO NITR_DEF TABLE **************************
      TNUM = 4
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO NITR_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'NITR_DEF'
        ENDIF
        DO I=1,MAXITR + 5
         LOOPING(TNUM) = LOOPING(TNUM) + 1
          write(ci,'(I2)') I
         COLV(TNUM,1,LOOPING(TNUM)) = I
         CHCOLV(TNUM,2,LOOPING(TNUM)) = ' Nems Iteration #' // ci
        ENDDO


!********************** WRITE TO CENSUS_RG_DEF TABLE **************************
      TNUM = 5
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO CENSUS_RG_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'CENSUS_RG_DEF'
        ENDIF
        DO I = 1,MNUMCR
         LOOPING(TNUM) = LOOPING(TNUM) + 1
         COLV(TNUM,1,LOOPING(TNUM)) = I
         CHCOLV(TNUM,2,LOOPING(TNUM)) = CENRG(I)
        ENDDO
!
!     ENDIF
!
!     WRITE GDP DEFLATOR Table to database
!
      TNUM = 6
      IF (LOOPING(TNUM) .EQ. 0) THEN
        NUMCOLS(TNUM) = 2
        DYNSTM(TNUM) = 'INSERT INTO GDP_DEFL VALUES(?,?,?)'
        WRTSTM(TNUM) = 'GDP_DEFL'
      ENDIF
      DO IY = 1 , UNYEAR
        LOOPING(TNUM) = LOOPING(TNUM) + 1
        COLV(TNUM,1,LOOPING(TNUM)) = IY
        COLV(TNUM,2,LOOPING(TNUM)) = MC_JPGDP(IY)
        IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
          COLVALS(:,:) = COLV(TNUM,:,:)
!         CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
          CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
          LOOPING(TNUM) = 0
        ENDIF
      ENDDO
!
!      --- WRITE OUT ANY LEFT OVER RECORDS TO THE NEMS DATA BASE
!      IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        DO TNUM = 1 , NUMTABS
         IF (LOOPING(TNUM) .NE. 0) THEN
           COLVALS(:,:) = COLV(TNUM,:,:)
           CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!          CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
           CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
           LOOPING(TNUM) = 0
         ENDIF
        ENDDO
!      ENDIF


    RETURN
    END
!
!
      SUBROUTINE EMMDEFS

      IMPLICIT NONE

!     THIS SUBROUTINE WRITES THE EMM OUPTUT ACCESS DATABASE DEFINITION TABLES

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'ecpcntl'
      include 'eusprc'
      include 'edbdef'

      INTEGER  I,NRECORD, ISGRP, EMMGRP, IGRR, INTCOL
      INTEGER  LLL,III,ITST,INDEX,INT,DBTYPE

      CHARACTER*2 CSPGI
      CHARACTER*5 CDPGI
      CHARACTER*40 NAMECOL

      INTEGER NUMTABS,FULLYR
      PARAMETER (NUMTABS = 37)        ! total number of database tables

      LOOPING = 0
      NUMCOLS = 0
      DYNSTM = ' '
      WRTSTM = ' '
      COLVALS = 0.0
      COLV = 0.0
      CHCOLVALS = ' '
      CHCOLV = ' '

! ********************** Write to Oracle DEF tables  *******
!
      TNUM = 1
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO ECP_DY_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'ECP_DY_DEF'
        ENDIF
!
        DO I = 1 , MNUMYR + ECP_D_FPH
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
          COLV(TNUM,2,LOOPING(TNUM)) = UHBSYR + I
        ENDDO
!     ENDIF
!
! **********  WRITE TO EMM NERC REGION NAME TABLE --> ORACLE ****
!
      TNUM = 2
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO NERC_RG_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'NERC_RG_DEF'
        ENDIF
        DO I = 1 , MNUMNR
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = URGNME(I)(1:4)
        ENDDO
!     ENDIF
!
! **********  WRITE TO ECP EXPORT REGION NAME (EXPRG) TABLE --> ORACLE ****
!
      TNUM = 3
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO ECP_EXPORT_RG_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'ECP_EXPORT_RG_DEF'
        ENDIF
        DO I = 1 , MNUMNR
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = URGNME(I)(1:4)
        ENDDO
!     ENDIF

! ****  WRITE EFD PLANT LOCATION REGION NAME (DPLTRG) TABLE ****
!
      TNUM = 4
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO EFD_DPLTRG_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'EFD_DPLTRG_DEF'
        ENDIF
        DO I = 1 , MNUMNR
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = URGNME(I)(1:4)
        ENDDO
!     ENDIF

! **********  WRITE TRADE REGION TABLE  -->
!
      TNUM = 5
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 4
         DYNSTM(TNUM) = 'INSERT INTO TRADE_RG_DEF VALUES (?,?,?,?,?)'
         WRTSTM(TNUM) = 'TRADE_RG_DEF'
        ENDIF
        DO I = 1 , MNUMNR + EFD_D_PROV
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
          COLV(TNUM,3,LOOPING(TNUM)) = I
           IF (I .LE. MNUMNR) THEN
             CHCOLV(TNUM,2,LOOPING(TNUM)) = URGNME(I)(1:4)
             CHCOLV(TNUM,4,LOOPING(TNUM)) = URGNME(I)(1:4)
           ELSEIF (I .EQ. MNUMNR+1) THEN
             CHCOLV(TNUM,2,LOOPING(TNUM)) = 'CAN-BC'
             CHCOLV(TNUM,4,LOOPING(TNUM)) = 'CAN-BC'
           ELSEIF (I .EQ.MNUMNR+2) THEN
             CHCOLV(TNUM,2,LOOPING(TNUM)) = 'CAN-MB'
             CHCOLV(TNUM,4,LOOPING(TNUM)) = 'CAN-MB'
           ELSEIF (I .EQ. MNUMNR+3) THEN
             CHCOLV(TNUM,2,LOOPING(TNUM)) = 'CAN-ON'
             CHCOLV(TNUM,4,LOOPING(TNUM)) = 'CAN-ON'
           ELSEIF (I .EQ. MNUMNR+4) THEN
             CHCOLV(TNUM,2,LOOPING(TNUM)) = 'CAN-QB'
             CHCOLV(TNUM,4,LOOPING(TNUM)) = 'CAN-QB'
           ELSEIF (I .EQ. MNUMNR+5) THEN
             CHCOLV(TNUM,2,LOOPING(TNUM)) = 'CAN-NB'
             CHCOLV(TNUM,4,LOOPING(TNUM)) = 'CAN-NB'
           ELSEIF (I .EQ. MNUMNR+EFD_D_PROV) THEN
             CHCOLV(TNUM,2,LOOPING(TNUM)) = 'Mexico'
             CHCOLV(TNUM,4,LOOPING(TNUM)) = 'Mexico'
           ELSE
             CHCOLV(TNUM,2,LOOPING(TNUM)) = 'N/A'
             CHCOLV(TNUM,4,LOOPING(TNUM)) = 'N/A'
           ENDIF
        ENDDO
!     ENDIF

! ************************  WRITE TO ECP CCAP (CCAP) TABLE --> ORACLE ****
!
      TNUM = 6
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 7
         DYNSTM(TNUM) = 'INSERT INTO CCAP_DEF VALUES (?,?,?,?,?,?,?,?)'
         WRTSTM(TNUM) = 'CCAP_DEF'
        ENDIF
        DO I = 1 , ECP_D_CAP
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = UPLNTCD(I)
          CHCOLV(TNUM,3,LOOPING(TNUM)) = UPLNAME(I)
          CHCOLV(TNUM,4,LOOPING(TNUM)) = PTYPE(I)
          COLV(TNUM,5,LOOPING(TNUM)) = UCPFTABI(I)
          COLV(TNUM,6,LOOPING(TNUM)) = UPVTYP(I)
          COLV(TNUM,7,LOOPING(TNUM)) = UPAVLYR(I)
        ENDDO
!     ENDIF

! ************************  WRITE TO EMM CAP  FTAB TABLE *****************
!
      TNUM = 7
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO FTAB_CAP_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'FTAB_CAP_DEF'
        ENDIF
        DO I = 1 , 9
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = ECAPFTABD(I)
        ENDDO
!     ENDIF

! ************************  WRITE TO ECP FUEL (CFL) TABLE --> ORACLE ******
!
      TNUM = 8
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 3
         DYNSTM(TNUM) = 'INSERT INTO ECP_CFL_DEF VALUES (?,?,?,?)'
         WRTSTM(TNUM) = 'ECP_CFL_DEF'
        ENDIF
        DO I = 1 , ECP_D_NFL
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = UPFLCD(I)
          CHCOLV(TNUM,3,LOOPING(TNUM)) = FLNAME(I)
        ENDDO
!     ENDIF

! ************************  WRITE TO EFD/ECP FUEL REGION TABLE --> ORACLE ******
!
      TNUM = 9
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 7
         DYNSTM(TNUM) = 'INSERT INTO EFD_FLREG_DEF VALUES (?,?,?,?,?,?,?,?)'
         WRTSTM(TNUM) = 'EFD_FLREG_DEF'
        ENDIF
        DO I = 1 , UNFRGN
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = FLRGCODE(I)
          CHCOLV(TNUM,3,LOOPING(TNUM)) = FLRGNAME(I)
          COLV(TNUM,4,LOOPING(TNUM)) = EPCSMP(I)
          COLV(TNUM,5,LOOPING(TNUM)) = EPCLMP(I)
          COLV(TNUM,6,LOOPING(TNUM)) = EPGSMP(I)
          COLV(TNUM,7,LOOPING(TNUM)) = EPCAMP(I)
        ENDDO
!     ENDIF

! ******************  WRITE TO ECP OWNER TYPE (COWN) TABLE --> ORACLE *****
!
      TNUM = 10
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 3
         DYNSTM(TNUM) = 'INSERT INTO ECP_COWN_DEF VALUES (?,?,?,?)'
         WRTSTM(TNUM) = 'ECP_COWN_DEF'
        ENDIF
        DO I = 1 , ECP_D_OWN
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = UPOWNCD(I)
          CHCOLV(TNUM,3,LOOPING(TNUM)) = OWNNAME(I)
        ENDDO
!     ENDIF

! *****************  WRITE TO ECP PLANNING YEAR (PY) TABLE --> ORACLE *****
!
      TNUM = 11
!
      IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO ECP_PY_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'ECP_PY_DEF'
        ENDIF
        DO I = 1, ECP_D_XPH
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = ECPPYD(I)
        ENDDO
      ENDIF

!  ************** WRITE EFD OWNER DEFINITION TABLE (DOWN) ********
!
      TNUM = 12
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 3
         DYNSTM(TNUM) = 'INSERT INTO EFD_DOWN_DEF VALUES (?,?,?,?)'
         WRTSTM(TNUM) = 'EFD_DOWN_DEF'
        ENDIF
        DO I = 1 , EFD_D_OWN
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = EFDOWND(I)
          COLV(TNUM,3,LOOPING(TNUM)) = EOWNFTAB(I)
        ENDDO
!     ENDIF

! ********** WRITE EFD PLANT FILE FINANCIAL OWNER DEFINITION TABLE (DWFOWN)**
!
      TNUM = 13
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO EFD_DWFOWN_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'EFD_DWFOWN_DEF'
        ENDIF
        DO I = 1 , EFD_D_OWN
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = EFDPOWND(I)
        ENDDO
!     ENDIF

! ********* WRITE OUT EFD PLANT FUEL USE TABLE (DCAPFL) **************
!
      TNUM = 14
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO EFD_DCAPFL_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'EFD_DCAPFL_DEF'
        ENDIF
        DO I = 1 , EFD_D_FPP
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = EFDFUD(I)
        ENDDO
!     ENDIF

!  **************  WRITE PLANT FILE VINTAGE TABLE (DWFVIN) ****************
!
      TNUM = 15
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO EFD_DWFVIN_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'EFD_DWFVIN_DEF'
        ENDIF
        DO I = 1, 12
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM))   = EFDPVINI(I)
          CHCOLV(TNUM,2,LOOPING(TNUM)) = EFDPVIND(I)
        ENDDO
!     ENDIF

!   ****************  WRITE OUT EFD VINTAGE TABLE (DVIN) ******************
!
      TNUM = 16
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO EFD_DVIN_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'EFD_DVIN_DEF'
        ENDIF
        DO I = 1 , EFD_D_VIN
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM))   = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = EFDVIND(I)
        ENDDO
!     ENDIF

! ****************** WRITE EFD ITERATION DEFINITION TABLE (DITR) ****
!
!     TNUM = 17
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
!       IF (LOOPING(TNUM) .EQ. 0) THEN
!        NUMCOLS(TNUM) = 2
!        DYNSTM(TNUM) = 'INSERT INTO EFD_DITR_DEF VALUES (?,?,?)'
!        WRTSTM(TNUM) = 'EFD_DITR_DEF'
!       ENDIF
!       DO I = 1 , 10
!         LOOPING(TNUM) = LOOPING(TNUM) + 1
!         COLV(TNUM,1,LOOPING(TNUM))   = I
!         CHCOLV(TNUM,2,LOOPING(TNUM)) = EFDITRD(I)
!       ENDDO
!     ENDIF

!  ***************************** WRITE EFD (DCAP) TABLE  ****
!
      TNUM = 18
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 5
         DYNSTM(TNUM) = 'INSERT INTO DCAP_DEF VALUES (?,?,?,?,?,?)'
         WRTSTM(TNUM) = 'DCAP_DEF'
        ENDIF
        DO I = 1 , EFD_D_CAP
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM))   = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = EPPLCD(I)
          CHCOLV(TNUM,3,LOOPING(TNUM)) = EFDNAME(I)
          COLV(TNUM,4,LOOPING(TNUM))   = EPFTABI(I)
          COLV(TNUM,5,LOOPING(TNUM))   = EFDIEAMAP(I)
        ENDDO
!     ENDIF

!   ********************************* WRITE EFD (DFL) TABLE  ****
!
      TNUM = 19
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 4
         DYNSTM(TNUM) = 'INSERT INTO EFD_DFL_DEF VALUES (?,?,?,?,?)'
         WRTSTM(TNUM) = 'EFD_DFL_DEF'
        ENDIF
        DO I = 1 , UNFUELS
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM))   = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = UFLCODE(I)
          CHCOLV(TNUM,3,LOOPING(TNUM)) = UNMFL(I)
          COLV(TNUM,4,LOOPING(TNUM))   = EFLFTABI(I)
        ENDDO
!     ENDIF

! ************************  WRITE TO FTAB FUEL TABLE *****************
!
      TNUM = 20
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO FTAB_FUEL_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'FTAB_FUEL_DEF'
        ENDIF
        DO I = 1 , 7
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM))   = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = FUELFTABD(I)
        ENDDO
!     ENDIF

! ************************  WRITE TO FTAB OWN  TABLE *****************
!
      TNUM = 21
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 3
         DYNSTM(TNUM) = 'INSERT INTO FTAB_OWN_DEF VALUES (?,?,?,?)'
         WRTSTM(TNUM) = 'FTAB_OWN_DEF'
        ENDIF
        DO I = 1 , 3
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM))   = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = FTABOWND(I)
          CHCOLV(TNUM,3,LOOPING(TNUM)) = FTABOWNS(I)
        ENDDO
!     ENDIF

!  ****** WRITE IEA CAPACITY CATEGORY TABLE                 ******
!
      TNUM = 22
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO IEA_CAPCAT_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'IEA_CAPCAT_DEF'
        ENDIF
        DO I = 1 , 8
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM))   = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = IEACATD(I)
        ENDDO
!     ENDIF

!  ****** WRITE SO2 COMPLIANCE GROUP INDEX (DSOCGI) TABLE  *******
!
      TNUM = 23
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO DSOCGI_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'DSOCGI_DEF'
        ENDIF
        DO I = 1 , EFD_D_SO2
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM))   = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = ECPSO2D(I)
        ENDDO
!     ENDIF

!  *************** WRITE Record Count Index TABLE ******************
!
!     TNUM = 24
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
!       IF (LOOPING(TNUM) .EQ. 0) THEN
!        NUMCOLS(TNUM) = 2
!        DYNSTM(TNUM) = 'INSERT INTO REC_COUNT_DEF VALUES (?,?,?)'
!        WRTSTM(TNUM) = 'REC_COUNT_DEF'
!       ENDIF
!       DO I = 1 , EMM_D_GRP
!         LOOPING(TNUM) = LOOPING(TNUM) + 1
!         COLV(TNUM,1,LOOPING(TNUM))   = I
!          WRITE(CDPGI,'(I5)')I
!         CHCOLV(TNUM,2,LOOPING(TNUM))= ' Record Count #'//CDPGI
!       ENDDO
!     ENDIF

!
!  *************** WRITE GROUP INDEX (DPG) TABLE ******************
!
!     TNUM = 25
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
!       IF (LOOPING(TNUM) .EQ. 0) THEN
!        NUMCOLS(TNUM) = 2
!        DYNSTM(TNUM) = 'INSERT INTO EFD_DPG_DEF VALUES (?,?,?)'
!        WRTSTM(TNUM) = 'EFD_DPG_DEF'
!       ENDIF
!       DO I = 1 , EMM_D_GRP
!         LOOPING(TNUM) = LOOPING(TNUM) + 1
!         COLV(TNUM,1,LOOPING(TNUM))   = I
!          WRITE(CDPGI,'(I5)')I
!         CHCOLV(TNUM,2,LOOPING(TNUM))= ' Plant Group #'//CDPGI
!       ENDDO
!     ENDIF

!   *************** WRITE PLANT SUBGROUP INDEX (SPG) TABLE ***
!
      TNUM = 26
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO EFD_SPG_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'EFD_SPG_DEF'
        ENDIF
        ISGRP = EMM_D_REC/EMM_D_GRP
        DO I = 1 , ISGRP
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM))   = I
          WRITE(CSPGI,'(I2)')I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = ' Plant Subgroup #'//CSPGI
        ENDDO
!     ENDIF

!  ******** WRITE ECP PLANT TYPE TO EFD PLANT TYPE MAPPING TABLE ******
!
      TNUM = 28
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO EFD_DECPEFDPT VALUES (?,?,?)'
         WRTSTM(TNUM) = 'EFD_DECPEFDPT'
        ENDIF
        DO I = 1 , ECP_D_CAP
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
          COLV(TNUM,2,LOOPING(TNUM)) = UPEFDT(I)
        ENDDO

!    ***************************  WRITE EFP DEFINITION TABLES **********
!     **************************  WRITE EFP CAP DEFINITION TABLE *******
!
      TNUM = 29
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO FCAP_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'FCAP_DEF'
        ENDIF
        DO I = 1 , EFP_D_CAP
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM))   = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = EFPPTYD(I)
        ENDDO
!     ENDIF

!   **********************  WRITE EFP OWN DEFINITION TABLE *********
!
      TNUM = 30
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO EFP_FOWN_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'EFP_FOWN_DEF'
        ENDIF
        DO I = 1 , EFP_D_OWN + 1
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM))   = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = EFPOWND(I)
        ENDDO
!     ENDIF

!  **************  WRITE EFP DEMAND SECTOR DEFINITION TABLE ********
!
      TNUM = 31
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO EFP_FDSECT_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'EFP_FDSECT_DEF'
        ENDIF
        DO I = 1 , 4
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = EFPDSTD(I)
        ENDDO
!     ENDIF

! ************  WRITE EFP STAGE OF PRODUCTION DEFINITION TABLE  *****
!
      TNUM = 32
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO EFP_FSTPRD_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'EFP_FSTPRD_DEF'
        ENDIF
        DO I = 1 , 3
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = EFPSPRD(I)
        ENDDO
!     ENDIF

! ************************ WRITE EFP END USE DEFINITION TABLE ********
!
      TNUM = 33
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO EFP_FENDUSE_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'EFP_FENDUSE_DEF'
        ENDIF
        DO I = 1 , MNEUGRP
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = EFPEUSD(I)
        ENDDO
!     ENDIF

! ************** WRITE EFP HISTORICAL YEAR DEFINITION TABLE ******
!
!!     TNUM = 34
!
!!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
!      IF (LOOPING(TNUM) .EQ. 0) THEN
!         NUMCOLS(TNUM) = 2
!         DYNSTM(TNUM) = 'INSERT INTO EFP_FHYR_DEF VALUES (?,?,?)'
!         WRTSTM(TNUM) = 'EFP_FHYR_DEF'
!     ENDIF
!        DO I = 1 , 85
!          LOOPING(TNUM) = LOOPING(TNUM) + 1
!          COLV(TNUM,1,LOOPING(TNUM)) = I
!          INTCOL = UHBSYR - (I-1)
!          WRITE(NAMECOL,'(I5)')INTCOL
!          CHCOLV(TNUM,2,LOOPING(TNUM)) = NAMECOL
!        ENDDO
!!     ENDIF

! ****** WRITE ECP PLANT TYPE TO EFP PLANT TYPE MAPPING TABLE*****
!
      TNUM = 35
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO EFP_FCFPT VALUES (?,?,?)'
         WRTSTM(TNUM) = 'EFP_FCFPT'
        ENDIF
        DO I = 1 , ECP_D_CAP
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
          COLV(TNUM,2,LOOPING(TNUM)) = UPEFPT(I)
        ENDDO
!     ENDIF

!  *********** WRITE EFD PLANT TYPE TO EFP PLANT TYPE MAPPING TABLE *****
!
      TNUM = 36
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO EFP_FDFPT VALUES (?,?,?)'
         WRTSTM(TNUM) = 'EFP_FDFPT'
        ENDIF
        DO I = 1 , EFD_D_CAP
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
          COLV(TNUM,2,LOOPING(TNUM)) = EFDEFPT(I)
        ENDDO
!     ENDIF
!
! ************************  WRITE TO STATE RPS DEF Table (ST_RPS_DEF)  --> ORACLE ****
!
      TNUM = 37
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 10
         DYNSTM(TNUM) = 'INSERT INTO ST_RPS_DEF VALUES (?,?,?,?,?,?,?,?,?,?,?)'
         WRTSTM(TNUM) = 'ST_RPS_DEF'
        ENDIF
        DO I = 1 , NM_ST_RPS
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = ST_RPS_TITLE(I)
          CHCOLV(TNUM,3,LOOPING(TNUM)) = ST_RPS_STCD(I)
          COLV(TNUM,4,LOOPING(TNUM)) = ST_RPS_STNM(I)
          CHCOLV(TNUM,5,LOOPING(TNUM)) = ST_RPS_ID(I)
          COLV(TNUM,6,LOOPING(TNUM)) = ST_RPS_SYR(I)
          COLV(TNUM,7,LOOPING(TNUM)) = ST_RPS_EYR(I)
          COLV(TNUM,8,LOOPING(TNUM)) = ST_RPS_POST(I)
          COLV(TNUM,9,LOOPING(TNUM)) = ST_RPS_PCAP_YR(I)
          COLV(TNUM,10,LOOPING(TNUM)) = ST_RPS_PCAP_TYP(I)
        ENDDO
!     ENDIF

!      --- WRITE OUT ANY LEFT OVER RECORDS TO THE NEMS DATA BASE
!      IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        DO TNUM = 1 , NUMTABS
         IF (LOOPING(TNUM) .NE. 0) THEN
           COLVALS(:,:) = COLV(TNUM,:,:)
           CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!          CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
           CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
           LOOPING(TNUM) = 0
         ENDIF
        ENDDO
!      ENDIF

      RETURN
      END
!
!
      SUBROUTINE LOADDEFS

      IMPLICIT NONE

!     THIS SUBROUTINE WRITES LOAD TABLES TO THE OUPUT ACCESS DATABASE

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'ecpcntl'
      include 'eusprc'
      include 'edbdef'
      include 'dsmdimen'
      include 'dsmtoefd'
      include 'dsmtfecp'


      INTEGER  I,J,NRECORD, ISGRP, EMMGRP, IGRR, INTCOL
      INTEGER  NUMSLICES,LLL,III,ITST,INDEX,INT,DBTYPE

      CHARACTER*2 CSPGI
      CHARACTER*5 CDPGI
      CHARACTER*40 NAMECOL
      Character*4 LSTEP(ECP_D_VLS)

      INTEGER NUMTABS,FULLYR
      PARAMETER (NUMTABS = 6)        ! total number of database tables

      DATA LSTEP/' 1st',' 2nd',' 3rd',' 4th',' 5th',' 6th',' 7th',' 8th',' 9th','10th','11th','12th', &
                 '13th','14th','15th','16th','17th','18th','19th','20th','21st','22nd','23rd','24th'/

      LOOPING = 0
      NUMCOLS = 0
      DYNSTM = ' '
      WRTSTM = ' '
      COLVALS = 0.0
      COLV = 0.0
      CHCOLVALS = ' '
      CHCOLV = ' '

! ******************  WRITE TO ECP (CVLS) TABLE --> ORACLE *****
!
      TNUM = 1
      NUMSLICES = 0
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO ECP_CVLS_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'ECP_CVLS_DEF'
        ENDIF
!
        DO I = 1 , ECPNUMSG
          DO J = 1 , ECPSGDNB(I)
            NUMSLICES = NUMSLICES + 1
          ENDDO
        ENDDO
   write(6,*) ' ecp numslices ',numslices
          LOOPING(TNUM) = 1
          ECPLSI(1) = 0
          ECPLSD(1) = 'Peak + Reserve Margin '
          COLV(TNUM,1,LOOPING(TNUM)) = ECPLSI(1)
          CHCOLV(TNUM,2,LOOPING(TNUM)) = ECPLSD(1)

        DO I = 1, NUMSLICES
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          IF ( I .EQ. 1 ) THEN
            ECPLSI(I) =  1
            ECPLSD(I) = 'Highest Demand Slice (Peak) '
          ELSEIF ( I .EQ. NUMSLICES) THEN
            ECPLSI(I) = I
            ECPLSD(I) = 'Lowest Demand Slice (Baseload)'
          ELSE
            ECPLSI(I) = I
            ECPLSD(I) = LSTEP(I) // ' Highest Demand Slice'
          ENDIF

          COLV(TNUM,1,LOOPING(TNUM)) = ECPLSI(I)
          CHCOLV(TNUM,2,LOOPING(TNUM)) = ECPLSD(I)
        ENDDO
!     ENDIF


! *****************  WRITE TO ECP LOAD GROUP (LDGRP) TABLE --> ORACLE *****
!
      TNUM = 2
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 4
         DYNSTM(TNUM) = 'INSERT INTO ECP_LOAD_GROUP VALUES (?,?,?,?,?)'
         WRTSTM(TNUM) = 'ECP_LOAD_GROUP'
        ENDIF
        DO I = 1, ECPNUMSG
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = ECPLGSD(I)
          CHCOLV(TNUM,3,LOOPING(TNUM)) = ECPLGMD(I)
          CHCOLV(TNUM,4,LOOPING(TNUM)) = ECPLGHD(I)
        ENDDO
!     ENDIF

! ******* WRITE EFD LOAD SLICE DEFINITION TABLE (DVLS) **  EFD BEGIN
!
      TNUM = 3
      NUMSLICES = 0
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 2
         DYNSTM(TNUM) = 'INSERT INTO DVLS_DEF VALUES (?,?,?)'
         WRTSTM(TNUM) = 'DVLS_DEF'
        ENDIF

        DO I = 1 , EFDNUMSG
          DO J = 1 , EFDSGDNB(I)
            NUMSLICES = NUMSLICES + 1
          ENDDO
        ENDDO
        write(6,*) ' efd numslices ', numslices
        DO I = 1, NUMSLICES
          LOOPING(TNUM) = LOOPING(TNUM) + 1

          IF ( I .EQ. 1 ) THEN
            EFDLSLD(I) = 'Highest Demand Slice (Peak) '
          ELSEIF ( I .EQ. NUMSLICES) THEN
            EFDLSLD(I) = 'Lowest Demand Slice (Baseload)'
          ELSE
            EFDLSLD(I) = LSTEP(I) // ' Highest Demand Slice'
          ENDIF

          COLV(TNUM,1,LOOPING(TNUM)) = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = EFDLSLD(I)
        ENDDO
!     ENDIF

! ************** WRITE OUT EFD SEASON TABLE (DSEAS) ***********************
!
      TNUM = 4
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 3
         DYNSTM(TNUM) = 'INSERT INTO DSEAS_DEF VALUES (?,?,?,?)'
         WRTSTM(TNUM) = 'DSEAS_DEF'
        ENDIF
        DO I = 1 , EFDns
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
          CHCOLV(TNUM,2,LOOPING(TNUM)) = EFDSEAN(I)
          CHCOLV(TNUM,3,LOOPING(TNUM)) = EFDSEAD(I)
        ENDDO
!     ENDIF

! *************** WRITE OUT EFD LOAD GROUP TABLE (DLDGRP) **************
!
      TNUM = 5
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 3
         DYNSTM(TNUM) = 'INSERT INTO EFD_DLDGRP_DEF VALUES (?,?,?,?)'
         WRTSTM(TNUM) = 'EFD_DLDGRP_DEF'
        ENDIF
        DO I = 1, EFDnumsg
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
          COLV(TNUM,2,LOOPING(TNUM)) = EFDLGSI(I)
          CHCOLV(TNUM,3,LOOPING(TNUM)) = EFDLGD(I)
        ENDDO
!     ENDIF

!   ************ WRITE EFD LOAD SEGMENT TABLE (DLDSEG) *******************
!
      TNUM = 6
!
!     IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        IF (LOOPING(TNUM) .EQ. 0) THEN
         NUMCOLS(TNUM) = 3
         DYNSTM(TNUM) = 'INSERT INTO EFD_DLDSEG_DEF VALUES (?,?,?,?)'
         WRTSTM(TNUM) = 'EFD_DLDSEG_DEF'
        ENDIF
        DO I = 1, EFDnumsg
          DO J = 1 , EFDsgDnB(I)
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = I
            COLV(TNUM,2,LOOPING(TNUM)) = J
            CHCOLV(TNUM,3,LOOPING(TNUM)) = EFDLSGD(I,J)
          ENDDO
        ENDDO
!     ENDIF
!
!      --- WRITE OUT ANY LEFT OVER RECORDS TO THE NEMS DATA BASE
!      IF ((ORCLEFD .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
        DO TNUM = 1 , NUMTABS
         IF (LOOPING(TNUM) .NE. 0) THEN
           COLVALS(:,:) = COLV(TNUM,:,:)
           CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!          CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
           CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
           LOOPING(TNUM) = 0
         ENDIF
        ENDDO
!      ENDIF


    RETURN
    END
