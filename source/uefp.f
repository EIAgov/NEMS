! $Header: m:/default/source/RCS/uefp.f,v 1.248 2020/07/23 11:23:57 LC2 Exp $
!
      SUBROUTINE ELEFP
      IMPLICIT NONE
!*****************************************************************
!        THIS SUBROUTINE IS THE MAIN ELECTRICITY FINANCE AND PRICING MODEL ROUTINE
!        ICF RESOURCES INC.
!*****************************************************************
! INPUT VARIABLES:
!   CURIYR = CURRENT YEAR
!   FIRSYR = FIRST FORECAST YEAR, passed through PARAMETER common
!   UESTYR = FIRST FORECAST YEAR for EMM, passed through CONTROL common
!   UNRGNS = NUMBER OF REGIONS
!   FCRL = INDICATES THE LAST ITERATION (SET TO 1) - run for producing reports
! INTERNAL VARIABLES:
!   IPREG = FLAGS INDICATING THE REGIONS TO BE PRINTED
!   NOPREG = NUMBER OF PRINT REGIONS
!   NRGN = CURRENT REGION
!   IPCOMP = FLAGS INDICATING THE OPERATING COMPONENTS TO PRINT
!*****************************************************************
      include 'parametr'
      include 'emmparm'
      include 'efpcntrl'
      include 'control'
      include 'ncntrl'
      include 'macout'
      include 'efpgen'
      include 'efprp2'
      include 'eusprc'
      include 'efpint'
      include 'efpout'
      include 'mpblk'
      include 'qblk'
      include 'uefdout'
      include 'udatout'
      include 'ecp_nuc'
      include 'emm_aimms'
      LOGICAL NEW
      CHARACTER*8 FILENM
      INTEGER II,ICHK,ISECT
      INTEGER I,IVLFLG     !FLAG TO IDENTIFY DOUBLE CONVERGENCE - adjust valcap output files
      INTEGER NRGN,FULLYR
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
      REAL COSTSERV(4,MNUMNR,MNUMYR)
      common /costofservice/ COSTSERV
      REAL ELNUCSBDY(50,MNUMNR,MNUMYR)    ! ZEC SUBSIDY AMOUNT (NEGATIVE REVENUES) BY STATE, REGION, AND YEAR
      REAL ELNUCSBGN(50,MNUMNR)           ! ZEC SUBSIDY GENERATION BY STATE, REGION, AND YEAR
      COMMON/NUCSUB/ELNUCSBDY,ELNUCSBGN
      REAL ERZECCST(MNUMNR)
      COMMON/ZECCST/ERZECCST
      REAL SV_PRGEN_ESRR(MNUMYR,MNUMNR)
      COMMON /STORE_ROR/SV_PRGEN_ESRR
! FIRST TIME THROUGH -- READ INPUT DATA AND SET UP DATA STRUCTURES
! DATA FILES ARE:
!   Run control file - EFPCNTL
!   Financial regulations file - FINREG
!   Phase in file - PHASIN
!   Sale/leaseback file - SALELB
!   Tax file - EFPTAX
!   Allocation and demands file - EFPALL
      ICHK = 0
      FULLYR = USYEAR(CURIYR)
      IF ((FULLYR .EQ. UESTYR).AND.(CURITR .EQ. 1)) THEN
! read EFPCNTL control file
         CALL INICON
! read remaining input files
         CALL ELRDPI    ! Phase ins
         CALL ELRDSL    ! Sales lease back
         CALL ELRDTR    ! Income tax assumptions
         CALL ELRDAL    ! Rate allocation assumptions
         CALL ELRDFR    ! Financial and regulatory data
! initialize
         SV_PRGEN_ESRR = 0.0
      ENDIF

! initialize expenses and capital expenses for comp pricing analysis
      DO NRGN = 1,MNUMNR
        COMPCAP(NRGN,CURIYR) = 0.0
        COMPEX(NRGN,CURIYR) = 0.0
        DGAPOLD(NRGN) = 1.0
        DO ISECT = 1,4
          COSTSERV(ISECT,NRGN,CURIYR) = 0.0
        ENDDO
      END DO ! NRGN

! fill in costs of nuclear ZEC programs
      IF (CURITR .EQ. 1)THEN
         ERZECCST(MNUMNR) = 0.0
         DO NRGN =1,UNRGNS
            IF (USW_ZECSUB .EQ. 1) THEN    !use ECP unless 0, then use EFD
               IF (EPZECSUB(NRGN,CURIYR) .GT. 0.0) THEN
                  ERZECCST(NRGN) = EPZECSUB(NRGN,CURIYR)
               ELSE
                  ERZECCST(NRGN) = ELNUCSBDY(UNSTAS + 1,NRGN,CURIYR)
               ENDIF
            ELSEIF (USW_ZECSUB .EQ. 2) THEN   ! use ECP only
               ERZECCST(NRGN) = EPZECSUB(NRGN,CURIYR)
            ELSEIF (USW_ZECSUB .EQ. 3 .OR. USW_ZECSUB .EQ. 4) THEN   ! use EFD only
               ERZECCST(NRGN) = ELNUCSBDY(UNSTAS + 1,NRGN,CURIYR)
            ELSE
               ERZECCST(NRGN) = 0.0
            ENDIF
            ERZECCST(MNUMNR) = ERZECCST(MNUMNR) + ERZECCST(NRGN)
            write(13,1650), 'ZECCST ', CURIYR+UHBSYR,NRGN,USW_ZECSUB,EPZECSUB(NRGN,CURIYR) * SCALPR,ELNUCSBDY(UNSTAS + 1,NRGN,CURIYR) * SCALPR,ERZECCST(NRGN) * SCALPR

         ENDDO
1650  FORMAT(1x,A10,3I6,3F10.3)
         IF ((CURIYR + UHBSYR) .EQ. UESTYR)THEN
            write(13,1660), 'ZECSUB ',YEARPR
1660  FORMAT(1x,A7,T30,'ZEC SUBSIDY COSTS BY YEAR AND EMM REGION (MILLIONS OF ',I4,' DOLLARS)')
            write(13,1670), 'ZECSUB ', (URGNME(NRGN)(1:4),NRGN = 1 , UNRGNS)
1670  FORMAT(1x,A7,'Year',<UNRGNS>(2X,A4),'    US')
         ENDIF
            write(13,1680), 'ZECSUB ', CURIYR+UHBSYR,(ERZECCST(NRGN) * SCALPR,NRGN = 1 , UNRGNS),ERZECCST(MNUMNR) * SCALPR
1680  FORMAT(1x,A7,I4,<UNRGNS>f6.0,f6.0)
      ENDIF

      DO NRGN=1,UNRGNS
         CALL EFP(NRGN)
      END DO
!
! SAVE EFP RESULTS INTO NEMS VARIABLES (save electricity prices)
      CALL ELSET
      IF (FCRL .eq. 1) CALL REVGAP
      IF (FCRL .eq. 1) THEN
        DO ISECT = 1,4
         DO NRGN = 1,MNUMNR
          COSTSERV(ISECT,NRGN,CURIYR) = COSTSERV(ISECT,NRGN,CURIYR) / MC_JPGDP(CURIYR)
         ENDDO
         write(22,1668) CURIYR,ISECT,(COSTSERV(ISECT,NRGN,CURIYR),NRGN=1,MNUMNR)
        ENDDO
      ENDIF
1668   FORMAT(1X,'COSTSV',2I5,<MNUMNR>F10.0)

! write final prices each iteration
      DO NRGN = 1, UNRGNS
         WRITE(22,1343) CURIYR+UHBSYR,CURITR,NRGN, &
            EPRICE(1,4,NRGN),PELRSNR(NRGN,CURIYR),SALCLS(NRGN,1),QELRSN(NRGN,CURIYR)/1000., &
            EPRICE(2,4,NRGN),PELCMNR(NRGN,CURIYR),SALCLS(NRGN,2),QELCMN(NRGN,CURIYR)/1000., &
            EPRICE(3,4,NRGN),PELINNR(NRGN,CURIYR),SALCLS(NRGN,3),QELINN(NRGN,CURIYR)/1000., &
            EPRICE(4,4,NRGN),PELTRNR(NRGN,CURIYR),SALCLS(NRGN,4),QELTRN(NRGN,CURIYR)/1000.
 1343    FORMAT(1X,"ELPQ_NR",3(":",I4),4(":",F8.2,":",F8.2,":",F8.1,":",F8.1))
         DO ISECT = 1 , 4
            IF ((EPRICE(ISECT,4,NRGN) .LE. 0.0) .OR. (ISNAN(EPRICE(ISECT,4,NRGN)))) THEN   ! check for NaNQ this way

               ICHK = 1
               write(6,'(A,2I4,F12.3)') '0 or NaNQ Found in EPRICE, nrgn,isect=', nrgn,isect,EPRICE(ISECT,4,NRGN)
               EPRICE(ISECT,4,NRGN)  = 9.999

            ENDIF
         END DO
      END DO

      DO NRGN=1,MNUMCR-1
         WRITE(22,1344) CURIYR+UHBSYR,CURITR,NRGN, &
            PELRS(NRGN,CURIYR),QELRS(NRGN,CURIYR), &
            PELCM(NRGN,CURIYR),QELCM(NRGN,CURIYR), &
            PELIN(NRGN,CURIYR),QELIN(NRGN,CURIYR), &
            PELTR(NRGN,CURIYR),QELTR(NRGN,CURIYR)
 1344    FORMAT(1X,"ELPQ_CR",3(":",I4),4(":",F8.2,":",F8.1))
      END DO

     IF(ICHK.EQ.1) THEN
        Write(6,*) 'this run is doomed, doomed.  writing out the restart file for posterity.'
        call MNFLUSH        ! flush I/O buffers to make sure latest stuff out there
        CALL NDATOT('   ')  ! write restart file
      ENDIF
! PRINT REPORTS ON LAST YEAR AND LAST ITERATION
      IF (FCRL .EQ. 1 .AND. CURIYR .GE. LASTYR) THEN

         DO NRGN=1,UNRGNS
! Sum totals if reports are requested
            IF ((IPOWN(3) .NE. 0) .OR. (IPCOMP(4) .NE. 0)) &
               CALL ADDUP(NRGN)
         ENDDO
         NEW = .TRUE.
         FILENM = 'EFPRPT'
         UF_EFPRPT = FILE_MGR('O',FILENM,NEW)
         REWIND UF_EFPRPT
         DO NRGN=1,(NOPREG+1)
! loop over components : 1. Generation 2. Transm. 3. Distr. 4. Total
! moved if statement for ipcomp switch inside report subroutine to allow database writes for all components.
            DO II=1,4
!                IF (IPCOMP(II) .NE. 0) THEN
              IF (IPREG(NRGN) .NE. 0) CALL EFP_REPORT(NRGN,II)
            END DO  ! II
!
!   pw2 do competitive report
!
           CALL COMPREPORT(NRGN)
         END DO

         UF_EFPRPT = FILE_MGR('C',FILENM,NEW)
      ENDIF

! if last iteration - set valcap/convergence flag to 1
      IF (FCRL .EQ. 1) IVLFLG = 1
      RETURN
      END
!
      SUBROUTINE INICON
      IMPLICIT NONE
!*****************************************************************
!     THIS SUBROUTINE READS IN THE EFPCNTL FILE CONTAINING CONTROL OPTIONS FOR REPORT WRITING
!*****************************************************************
! INPUT VARIABLES:
!     UNYEAR = NUMBER OF YEARS
! INTERNAL VARIABLES:
!     NRGN1 = NUMBER OF REGIONS PLUS ONE FOR NATIONAL Total
!     NUM = # OF PLANT TYPES IN EACH Capacity Requirement Report
!     IYRFFY = first forecast year - use same as in rest of NEMS - UHBSYR (+1)
! OUTPUT VARIABLES:
!     NAME3 = RUN TITLE
!     NOPREG = NUMBER OF PRINT REGIONS
!     FRACRG = FRACTIONS USED TO AGGREGATE/DISAGGREGATE INPUT
!              REGIONS TO PRINT REGIONS
!     IPREG = FLAGS INDICATING WHICH REGIONS TO PRINT
!     IPOWN = FLAGS INDICATING WHICH OWNERSHIP CLASSES TO PRINT
!     IPCOMP = FLAGS INDICATING WHICH OPERATING COMPONENTS TO PRINT
!     IYRRPT = first year to be reported
!     IYRRL = YEAR IN WHICH TO REPORT REAL DOLLARS - use same as in rest of NEMS - YEARPR
!     IYRS = YEARS FOR5REPORT HEADERS
!     EFPREALDOLLARS = FLAG INDICATING IF REPORTS ARE IN REAL OR NOMINAL DOLLARS
!     NOCAP = # OF PLANT TYPE REPORTS
!     CAPTYP = LIST OF PLANT TYPES IN EACH PLANT TYPE REPORT
!     CAPTIT = PLANT TYPE NAMES FOR REPORT HEADERS
!     NUMCAP = # OF PLANT TYPES IN EACH PLANT TYPE REPORT
!*****************************************************************
      include 'parametr'
      include 'emmparm'
      include 'control'
      include 'efpcntrl'
      include 'efpname'
      include 'ncntrl'
      include 'efpgen'
      include 'eusprc'
      include 'efpint'
      include 'uefdout'
      INTEGER I,IEFD
      INTEGER J
      INTEGER YR
      INTEGER IY
      INTEGER IYRFFY
      INTEGER NRGN1
      INTEGER NUM
      INTEGER IN
      LOGICAL NEW             ! IS FILE NEW  .TRUE.) OR OLD (.FALSE.)
      CHARACTER*18 FNAME      ! FILE IDENTIFIER OF INPUT FILE
      INTEGER FILE_MGR        ! INTEGER SUBPROGRAM FOR FILE I/O
      EXTERNAL FILE_MGR

      CALL EFPCNTLSQL

      FNAME = 'EFPCNTL'
      NEW = .FALSE.
      IN = FILE_MGR('O',FNAME,NEW)
      READ(IN,20)NAME3        !  Change into dummy
   20 FORMAT(A45)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      NAME3=SCEN_DATE
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      NRGN1 = NOPREG + 1
      READ(IN,*)(IPOWN(I),I=1,3)
      READ(IN,*)(IPCOMP(I),I=1,4)
      READ(IN,*)(IPRPT(I),I=1,2)
      READ(IN,*)IPRPT(3)
      READ(IN,*)IPRPT(4)
      READ(IN,*)IPRPT(5)
      READ(IN,*)IPRPT(6)
      READ(IN,*)IPRPT(7)
      READ(IN,*)IPRPT(8)
      READ(IN,*)IPRPT(9)
      READ(IN,*)IPRPT(10)
      READ(IN,*)IPRPT(11)
      READ(IN,*)IPRPT(12)
      READ(IN,*)IPRPT(13)
      READ(IN,*)IPRPT(14)
      READ(IN,*)IPRPT(15)
      READ(IN,*)IPRPT(16)
      READ(IN,*)IPRPT(17)
      READ(IN,*)IPRPT(18)
      READ(IN,*)IPRPT(19)
      READ(IN,*)IPRPT(20)
      READ(IN,*)IPRPT(21)
      READ(IN,*)IPRPT(22)
      READ(IN,*)IPRPT(23)
!     READ(IN,*)IYRFFY,IYRRPT,IYRRL
      READ(IN,*)IYRRPT
      IYRFFY = UHBSYR + 1
      IYRRL = YEARPR - IYRFFY + 1
      IYRRPT = IYRRPT - IYRFFY + 1
      DO IY=1,UNYEAR
         IYRS(IY) = IYRFFY + IY - 1
      END DO
      READ(IN,*)IREAL
      READ(IN,*)ISTASST
      READ(IN,*)CAPADJ
      READ(IN,*) RORADJ
!     READ(IN,*)(GAADJ(YR),YR=1,MNUMYR)
      DO IEFD = 1 , EFD_D_CAP
         READ(IN,*)(OMADJ(IEFD,YR),YR=1,MNUMYR)
!  INSURE THAT ADJUSTMENT FACTOR IS NONZERO
        DO YR = 1 , MNUMYR
          IF(OMADJ(IEFD,YR) .LE. 0.0)OMADJ(IEFD,YR) = 1.0
        END DO
      END DO
      READ(IN,*) (OMADJTR(YR),YR=1,MNUMYR)
      READ(IN,*) (OMADJDS(YR),YR=1,MNUMYR)
      READ(IN,*)NOCAP
      DO I=1,NOCAP
         READ(IN,*)NUM,(CAPTYP(I,J),J=1,NUM)

         READ(IN,170)CAPTIT(I)
  170    FORMAT(A50)
         NUMCAP(I) = NUM
      END DO
      READ(IN,*) CL_IND, OG_IND, NC_IND

!  tfac/dfac no longer used - will need to be added to dynamic DB if this logic is used in the future
!     DO J=1,IJUMPEMRGN
!       READ(IN,*)(tfac(IEFD,J),IEFD = 1 , 3)
!     ENDDO
!     DO J=1,IJUMPEMRGN
!       READ(IN,*)(dfac(IEFD,J),IEFD = 1 , 3)
!     ENDDO

      IN = FILE_MGR('C',FNAME,NEW)
! CHECK THAT 1ST CAP REPORT IS GRAND TOTAL
      IF (NUMCAP(1) .LT. NPTYP) THEN
         WRITE(6,*) 'ERROR IN UEFP REPORT WRITER - EFPCNTL IS BAD'
         STOP
      ENDIF
      RETURN
      END
!
      SUBROUTINE ELRDPI
      IMPLICIT NONE
!*****************************************************************
!      THIS SUBROUTINE READS IN THE PHASEIN DATA
!*****************************************************************
! INTERNAL VARIABLES:
!    DISPR1 = FRACTION OF EXCESS CAPACITY DISALLOWANCE TO TREAT AS DISALLOWANCE
!    DISPR2 = FRACTION OF IMPRUDENCE DISALLOWANCE TO TREAT AS DISALLOWANCE
!    DISXCS = EXCESS CAPACITY DISALLOWANCE
!    DISPRU = IMPRUDENCE DISALLOWANCE
!    IASSMP = LOWEST ASSUMPTION CODE TO USE IN THIS RUN (SCREENING VARIABLE)
!    IASMPT = ASSUMPTION CODE FOR EACH PHASE-IN
!    ICT    = FILLS PIDFS with 1 in years after PIDFS is 1 from PHASEIN file
!    ICT1   = FILLS PIRCS with 1 in years after PIRCS is 1 from PHASEIN file
! OUTPUT VARIABLES:
!    PITXBS = TAX BASIS AS A FRACTION OF BOOKED COST OF
!    PIBKLF = BOOK LIFE OF PHASED-IN PLANT
!    IBYRPI = 1ST YEAR OF PHASE-IN PERIOD
!    IROPI(1,IPI) = REGION OF PHASE-IN PLAN
!    IROPI(2,IPI) = OWNERSHIP TYPE OF PHASE-IN PLAN
!    LPI = LENGTH OF PHASE-IN PLAN
!    PIBKVL = BOOK VALUE OF PLANT TO BE PHASED-IN
!    NPI = TOTAL NUMBER OF PHASE-IN PLANS
!    SAVNPI = TOTAL NUMBER OF PHASE-IN PLANS (NEVER CHANGED FROM
!             INITIAL VALUE)
!    IRDPI = CAPITALIZE RETURN ON DEFERRED COST? (1=YES,2=NO)
!    PIRCS = FRACTION OF REMAINING DEFERRED REVENUES TO BE
!    PIDEF = TOTAL CUMULATIVE DEFERRED REVENUES
!    DISPER = FRACTION OF PLANT TOTALLY DISALLOWED
!    PIDFS = CUMULATIVE FRACTION OF TOTAL COST PHASED-IN BY
!*****************************************************************
      include 'parametr'
      include 'emmparm'
      include 'control'
      include 'phasin'
      INTEGER IASSMP
      INTEGER IYR
      INTEGER IASMPT
      INTEGER ICT
      INTEGER ICT1
      REAL*4 DISPR1
      REAL*4 DISPR2
      REAL*4 DISPRU
      REAL*4 DISXCS
      CHARACTER*1 LINE
      CHARACTER*150 PIDATA  ! CHARACTER VARIABLE USED TO READ INPUT FILE
      INTEGER IN
      LOGICAL NEW             ! IS FILE NEW (.TRUE.) OR OLD (.FALSE.)
      CHARACTER*18 FNAME      ! FILE IDENTIFIER OF INPUT FILE
      INTEGER FILE_MGR        ! INTEGER SUBPROGRAM FOR FILE I/O
      EXTERNAL FILE_MGR
!MPTIM3      INTEGER CPU_START1,CPU_END1,WALL_START1,WALL_END1      !//MPTIM3//
!MPTIM3      INTEGER CPU_START2,CPU_END2,WALL_START2,WALL_END2      !//MPTIM3//
!MPTIM3      REAL OTHCPU,OTHWAL
!MPTIM3           OTHCPU = 0.0
!MPTIM3           OTHWAL = 0.0
!MPTIM3      CALL MPTIM3(CPU_START1,WALL_START1)

      CALL PHASEINSQL
      
      CALL ELRDND(IN)
!MPTIM3      CALL MPTIM3(CPU_START1,WALL_START1)
!MPTIM3      CALL MPTIM3(CPU_END1,WALL_END1)
!MPTIM3      WRITE (6,2323) '** TIMING FILE_MGR ** ',
!MPTIM3     2           FLOAT(CPU_END1)/100.-FLOAT(CPU_START1)/100.,
!MPTIM3     3           FLOAT(WALL_END1)/100.-FLOAT(WALL_START1)/100.
      RETURN
      END
!
      SUBROUTINE ELRDSL
      IMPLICIT NONE
!***************************************************************
! THIS SUBROUTINE READS IN THE SALES LEASEBACK DATA
!***************************************************************
! INTERNAL VARIABLES:
!        IYR = YEAR OF SALE/LEASEBACK PLAN
!        BKGAIN = GROSS GAIN ON BOOK VALUE
! OUTPUT VARIABLES:
!        NSL = TOTAL NUMBER OF SALE/LEASEBACK TRANSACTIONS
!        SAVNSL = TOTAL NUMBER OF SALE/LEASEBACK TRANSACTIONS
!                 (NEVER CHANGED THROUGHOUT EXECUTION)
!        IROSL(1,ISL) = REGION OF SALE/LEASEBACK
!        IROSL(2,ISL) = OWNERSHIP TYPE OF SALE/LEASEBACK
!        IBYRSL = 1ST YEAR OF SALE/LEASEBACK PERIOD
!        SLBKVL = BOOK VALUE OF SALE/LEASEBACK PLANT
!        SLPROC = GROSS SALE PROCEEDS
!        SLGAIN = NET OF TAX GAIN OVER BOOK VALUE FROM SALE
!        SLTERM = TERM OF THE LEASE (YEARS)
!        SLLP = ANNUAL LEASE PAYMENT
!        SLTAXS = TRANSACTION EXPENSES AND ESTIMATED TAXES
!***************************************************************
      include 'parametr'
      include 'emmparm'
      include 'control'
      include 'salelb'
      CHARACTER*2 DUMMY
      INTEGER IYR
      REAL*4 BKGAIN
      INTEGER IN
      LOGICAL NEW             ! IS FILE NEW (.TRUE.) OR OLD (.FALSE.)
      CHARACTER*18 FNAME      ! FILE IDENTIFIER OF INPUT FILE
      INTEGER FILE_MGR        ! INTEGER SUBPROGRAM FOR FILE I/O
      EXTERNAL FILE_MGR

      ! get salelb from the sqlite dB.
      call SALELBSQL
       
      RETURN
      END
!
      SUBROUTINE ELRDTR
      IMPLICIT NONE
!**************************************************************
!  THIS SUBROUTINE READS TAX CHANGES TO BE SIMULATED AND INITIALIZES SOME NEW VARIABLES
!**************************************************************
! INPUT VARIABLES:
!     UINUFS = INDEX FOR NUFS OPITIONS
!     UNRGN  = NUMBER OF REGIONS
!     UNYEAR = NUMBER OF YEARS
! INTERNAL VARIABLES:
!     NN = UNIT TO READ FROM
!     IBYEAR = BASE YEAR FOR YEAR INDICES
!     IYRIC1 = YEAT ITC PROPOSAL IS IMPLEMENTED
!     JYRIC1 = FIRST START YEAR OF EFFECTED PLANTS
!     ICPYR1 = YEAR IDC PROPOSAL IS IMPLEMENTED
!     JCPYR1 = FIRST START YEAR OF EFFECTED PLANTS
! OUTPUT VARIABLES:
!     MLAG = Regulatory lag override (unused)
!     IRSYTD = First year transmission and distribution plant uses depreciation schedule
!     IRSYP  = First year producing plant uses depreciation schedule
!     TXRTYR = Federal income tax rate by year
!     IYRITC = INDEX OF YEAR ITC PROPOSAL IS IMPLEMENTED
!     JYRITC = INDEX OF FIRST START YEAR OF EFFECTED PLANTS
!     IEDT = 1 MEANS FLOW BACK EXCESS OVER REMAINING
!     ICAPYR = INDEX OF YEAR IDC PROPOSAL IS IMPLEMENTED
!     IYREDT = FIRST YEAR IN WHICH EXCESS SHOULD BE FLOWED BACK
!     JCAPYR = INDEX OF FIRST START YEAR OF EFFECTED PLANTS
!     FREDTF = FRACTION OF TOTAL FLOWED BACK IN EACH YEAR
!     NYREDT = NUMBER OF YEARS TO FLOW BACK (IF IEDT=2)
!     CONDR = Tax depreciation rates
!     RITCM = Fraction of investment tax credit which nonaffected construction projects receive by year
!**************************************************************
      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'tax'
      include 'control'
      include 'efpgen'
      include 'taxdat'
      INTEGER NN
      LOGICAL NEW             ! IS FILE NEW (.TRUE.) OR OLD (.FALSE.)
      CHARACTER*18 FNAME      ! FILE IDENTIFIER OF INPUT FILE
      INTEGER FILE_MGR        ! INTEGER SUBPROGRAM FOR FILE I/O
      EXTERNAL FILE_MGR
      INTEGER J
      INTEGER I
      INTEGER IYRIC1
      INTEGER JYRIC1
      INTEGER ICPYR1
      INTEGER JCPYR1
      INTEGER IBYEAR
      INTEGER IYR
      INTEGER ITXRP
      INTEGER ICAP,XYR
      REAL CONDRR
      IBYEAR=UHBSYR
      FNAME = 'EFPTAX'
      NEW = .FALSE.
      NN = FILE_MGR('O',FNAME,NEW)
! ASSIGN TAX DEPRECIATION RATES
      DO I=1,2
         READ(NN,*)(CONDR(J,I),J=1,80)
      END DO
! Calcalate revised depreciation schedules for new assets due to Dec 2017 tax law, tied to emmcntl inputs to be consistent with ECP
      DO IYR = UISTYR , MNUMYR
         IF (TAXDEPR1(IYR) .LE. 0.0)THEN
            DO ICAP = 1 , 2
               DO XYR = 1 , 80
                  CONDRN(XYR,ICAP,IYR) = CONDR(XYR,ICAP)
               END DO
            END DO
         ELSE
            DO ICAP = 1 , 2
               IF (CONDR(1,ICAP) .LT. TAXDEPR1(IYR))THEN
                  CONDRR = 0.0
                  DO XYR = 2 , 80
                     CONDRR = CONDRR + CONDR(XYR,ICAP)
                  END DO
                     CONDRN(1,ICAP,IYR) = TAXDEPR1(IYR)
                  DO XYR = 2 , 80
                     CONDRN(XYR,ICAP,IYR) = (1.0 - TAXDEPR1(IYR)) * CONDR(XYR,ICAP) / CONDRR
                  END DO
               ELSE
                  DO XYR = 1 , 80
                     CONDRN(XYR,ICAP,IYR) = CONDR(XYR,ICAP)
                  END DO
               END IF
            END DO
         END IF
      END DO

     write(22,326) 'CONDR :1:',(CONDR(XYR,1),XYR=1,80)
     write(22,326) 'CONDR :2:',(CONDR(XYR,2),XYR=1,80)
     DO IYR = UISTYR,MNUMYR
     write(22,326) 'CONDRN:1:',(CONDRN(XYR,1,IYR),XYR=1,80)
     write(22,326) 'CONDRN:2:',(CONDRN(XYR,2,IYR),XYR=1,80)
     ENDDO

326  FORMAT(1x,A10,80F8.5)

      READ(NN,*)(ICONTP(J),J=1,NPTYP)
! READ ERTA TAX VALUES WHICH APPLY ACROSS REGIONS FOR NEW ASSETS
      DO I=1,3
         READ(NN,*)ITXRP,(ESTXRS(I,J),J=1,ITXRP)
         ESTXRP(I) = ITXRP
      END DO
! READ IN INPUT FILE
      READ(NN,*)MLAG
      READ(NN,*)IRSYP
      READ(NN,*)IRSYTD
      READ(NN,*)IRSYT2
      IRSYP = IRSYP - IBYEAR
      IRSYTD = IRSYTD - IBYEAR
      IRSYT2 = IRSYT2 - IBYEAR
      READ(NN,*)(TXRTYR(I),I=1,MNUMYR)
      READ(NN,*)IYRITC,JYRITC
      READ(NN,*)(RITCM(I),I=1,MNUMYR)
      IYRITC = IYRITC - IBYEAR
      JYRITC = JYRITC - IBYEAR
      READ(NN,*)ICAPYR,JCAPYR
      ICAPYR = ICAPYR - IBYEAR
      JCAPYR = JCAPYR - IBYEAR
      READ(NN,*)IYREDT
      IYREDT = IYREDT - IBYEAR
      READ(NN,*)IEDT
      READ(NN,*)NYREDT,(FREDTF(I),I=1,NYREDT)
      READ(NN,*)(ESRITC(J),J=1,NPTYP)
!   PRINT HEADER SHEET FOR REPORTS
      IYRIC1 = IYRITC + IBYEAR
      JYRIC1 = JYRITC + IBYEAR
      ICPYR1 = ICAPYR + IBYEAR
      JCPYR1 = JCAPYR + IBYEAR
      NN = FILE_MGR('C',FNAME,NEW)
      RETURN
      END
!
      SUBROUTINE ELRDAL
      IMPLICIT NONE
!*************************************************************
! THIS SUBROUTINE READS THE DEMAND QUANITIES AND THE ALLOCATION TECHNIQUES FOR EACH OPERATING COMPONENT
!   THE DEMAND QUANITIES WILL EVENTUALLY BE PASSED BY THE LDSM MODULE
!   THE OPERATING COMPONENTS ARE GENERATION, TRANSMISSION, AND DISTRIBUTION
!   THERE ARE 4 CLASSES OF OPERATING COMPONENTS:
!     1 = RESIDENTIAL
!     2 = COMMERCIAL
!     3 = INDUSTRIAL
!     4 = TRANSPORTATION
!*************************************************************
! OUTPUT VARIABLES:
!     NCLASS = NUMBER OF CUSTOMER CLASSES
!     FUELMR = MARGINAL FUEL BY CUSTOMER CLASS
!     FUELEM = EMBEDDED FUEL BY CUSTOMER CLASS
!     CUST = NUMBER OF CUSTOMERS IN EACH CUSTOMER CLASS
!     CP = COINCIDENT PEAK BY CUSTOMER CLASS
!     PCP = PROBABILIY OF COINCIDENT PEAK BY CUSTOMER CLASS
!     NCP = NON-COINCIDENT PEAK BY CUSTOMER CLASS
!     SYSLF = SYSTEM LOAD FACTOR
!     NTECH = NUMBER OF ALLOCATION TECHNIQUES
!     NCOST = NUMBER OF COST CATEGORIES
!     TECFAC = PERCENT OF EACH COST POOL (E.G., FUEL, VAR O&M, FIXED)
! INTERNAL VARIABLES:
!     IAFIL = UNIT NUMBER FOR ALLOCATION FILE
!*************************************************************
      include 'parametr'
      include 'emmparm'
      include 'efpgen'
      include 'eusprc'
      include 'efpint'
      include 'efprcy'
      include 'ldsm'
      INTEGER IAFIL
      LOGICAL NEW             ! IS FILE NEW (.TRUE.) OR OLD (.FALSE.)
      CHARACTER*80 ALDATA     ! CHARACTER VARIABLE USED TO READ INPUT FILE
      CHARACTER*18 FNAME      ! FILE IDENTIFIER OF INPUT FILE
      INTEGER FILE_MGR        ! INTEGER SUBPROGRAM FOR FILE I/O
      EXTERNAL FILE_MGR
      INTEGER I
      INTEGER J
      INTEGER K
      INTEGER ICLASS
      INTEGER ISECT
      REAL*4 TOTAL
!MPTIM3      INTEGER CPU_START1,CPU_END1,WALL_START1,WALL_END1      !//MPTIM3//
!MPTIM3      INTEGER CPU_START2,CPU_END2,WALL_START2,WALL_END2      !//MPTIM3//
!MPTIM3      REAL OTHCPU,OTHWAL
!MPTIM3           OTHCPU = 0.0
!MPTIM3           OTHWAL = 0.0
!MPTIM3      CALL MPTIM3(CPU_START1,WALL_START1)
      FNAME = 'EFPALL'
      NEW = .FALSE.
      IAFIL = FILE_MGR('O',FNAME,NEW)
! INITIALIZATIONS
      READ(IAFIL,FMT='(A80)') ALDATA
      DO WHILE (ALDATA(1:1).EQ.'*')
      READ(IAFIL,FMT='(A80)') ALDATA
      END DO
      READ(ALDATA,*)NTECH
      DO I=1,NTECH
      READ(IAFIL,FMT='(A80)') ALDATA
      DO WHILE (ALDATA(1:1).EQ.'*')
      READ(IAFIL,FMT='(A80)') ALDATA
      END DO
         READ(ALDATA,'(A25)')TECNAM(I)
      END DO
      READ(IAFIL,FMT='(A80)') ALDATA
      DO WHILE (ALDATA(1:1).EQ.'*')
      READ(IAFIL,FMT='(A80)') ALDATA
      END DO
      READ(ALDATA,*)NCOST
      DO I=1,NCOST
      READ(IAFIL,FMT='(A80)') ALDATA
      DO WHILE (ALDATA(1:1).EQ.'*')
      READ(IAFIL,FMT='(A80)') ALDATA
      END DO
         READ(ALDATA,'(A25)')CCATNAM(I)
      END DO
      NCLASS = 4
! READ ALLOCATION FILE
      DO ISECT=1,3
         DO J=1,NTECH
      READ(IAFIL,FMT='(A80)') ALDATA
      DO WHILE (ALDATA(1:1).EQ.'*')
      READ(IAFIL,FMT='(A80)') ALDATA
      END DO
            READ(ALDATA,10)(TECFAC(J,K,ISECT),K=1,NCOST)
         END DO
      END DO
! READ DEMANDS FILE
      READ(IAFIL,FMT='(A80)') ALDATA
      DO WHILE (ALDATA(1:1).EQ.'*')
      READ(IAFIL,FMT='(A80)') ALDATA
      END DO
! PROCESS DEMAND MODEL RESULTS BY CENSUS REGION INTO NERC REGION
      READ(ALDATA,10)(FUELMR(ICLASS),ICLASS=1,NCLASS)
   10 FORMAT(T28,4F9.1)
      READ(IAFIL,10)(FUELEM(ICLASS),ICLASS=1,NCLASS)
      READ(IAFIL,10)(CUST(ICLASS),ICLASS=1,NCLASS)
      READ(IAFIL,10)(CP(ICLASS),ICLASS=1,NCLASS)
      READ(IAFIL,10)(PCP(ICLASS),ICLASS=1,NCLASS)
      READ(IAFIL,10)(NCP(ICLASS),ICLASS=1,NCLASS)
      READ(IAFIL,10)SYSLF
   20 FORMAT(T28,F9.1)
      SYSLF = SYSLF/100.0
      IAFIL = FILE_MGR('C',FNAME,NEW)
!MPTIM3      CALL MPTIM3(CPU_END1,WALL_END1)
!MPTIM3      WRITE (6,2323) '** TIMING ELRDAL   ** ',
!MPTIM3     2           FLOAT(CPU_END1)/100.-FLOAT(CPU_START1)/100.-OTHCPU,
!MPTIM3     3           FLOAT(WALL_END1)/100.-FLOAT(WALL_START1)/100.-OTHWAL
!MPTIM32323  FORMAT(10X,A,' CPU USED (SECONDS) = ',F10.3,', WALL USED =',F10.3)
      RETURN
      END
!
      SUBROUTINE ELRDFR
!*******************************************************************
!   THIS SUBROUTINE READS INPUT DATA FROM FINREG FILE, CALLS ELEA, AND WRITES OUT DIRECT ACCESS FILES
!      THE DIRECT ACCESS FILES HAVE THREE CATEGORIES OF DATA:
!
!       1) INFORMATION THAT DOES NOT VARY BY REGION OR YEAR
!       2) INFORMATION THAT VARIES BY REGION BUT NOT BY YEAR
!       3) INFORMATION THAT VARIES BY BOTH REGION AND YEAR
!
!*******************************************************************
!
 
      USE SQLITE
      IMPLICIT NONE
      include 'parametr'
      include 'emmparm'
      include 'efpgen'
      include 'efprc'
      include 'efpr'
      include 'efprcy'
      include 'control'
      include 'eusprc'
      include 'efpint'
      include 'ncntrl'
      include 'tax'
      include 'uefdout'
      INTEGER NRGN
      INTEGER M,ICL
      INTEGER J
      INTEGER Y
      INTEGER IY,rgn,yr,id
      INTEGER ICOM
      CHARACTER*1 ICALL
      CHARACTER*230 RDDATA  ! CHARACTER VARIABLE USED TO READ INPUT FILE
      INTEGER IN
      LOGICAL NEW             ! IS FILE NEW (.TRUE.) OR OLD (.FALSE.)
      CHARACTER*18 FNAME      ! FILE IDENTIFIER OF INPUT FILE
      INTEGER I, ICP, ICAP, IOWN
      REAL*4 TOTAL
      REAL*4 EOVDEF(85)      !HISTORICAL DEFLATION FACTORS
      REAL*4 EOCOST(14,2)
      REAL*4 EOVBK(14,2),MORE_BK
      REAL*4 TRABDE(2)
      REAL*4 TROB(2)
      REAL*4 TRST(2)
      REAL*4 TRCS(2)
      REAL*4 TRPS(2)
      REAL*4 TRRE(2)
      REAL*4 TRWC(2)
      REAL*4 TRDITC(2)
      REAL*4 TRPRDF(2)
      REAL*4 TRAMD(2)
      REAL*4 TRRVRQ(2)
      !REAL*4 VDIVRT(MNUMYR,2)  !no longer needed (replaced with a XML input variable)
      !REAL*4 VSCWPP(MNUMYR,2)
      !REAL*4 VSTSHR(MNUMYR,2)
      !REAL*4 VSPRLT(MNUMYR,2)
      !REAL*4 VSPRST(MNUMYR,2)
      !REAL*4 VSPRCE(MNUMYR,2)
      !REAL*4 VSPRPS(MNUMYR,2)
      !REAL*4 VSRTLT(MNUMYR,2)
      !REAL*4 VSRTST(MNUMYR,2)
      !REAL*4 VSRTCE(MNUMYR,2)
      !REAL*4 VSRTPS(MNUMYR,2),VPERDA(MNUMYR,4),VPEREX(MNUMYR,4)
      REAL*4, ALLOCATABLE :: XN_CAPTSCALE(:,:)   !temporary XML input variable to read CAPTSCALE from XML input file  (Row: Year 2000~2050, Column: EMM regions)
      REAL*4, ALLOCATABLE :: XN_CAPDSCALE(:,:)   !temporary XML input variable to read CAPDSCALE from XML input file  (Row: Year 2000~2050, Column: EMM regions)
      REAL*4, ALLOCATABLE :: XN_OMTSCALE(:,:)   !temporary XML input variable to read OMTSCALE from XML input file  (Row: Year 2000~2050, Column: EMM regions)
      REAL*4, ALLOCATABLE :: XN_OMDSCALE(:,:)   !temporary XML input variable to read OMDSCALE from XML input file  (Row: Year 2000~2050, Column: EMM regions)
      REAL*4, ALLOCATABLE :: XN_ERMECLAG(:,:)   !temporary XML input variable to read ERMECLAG from XML input file  (Row: Year 2000~2050, Column: EMM regions)
      REAL*4, ALLOCATABLE :: XN_ESBKLF(:,:), XN_ESDEPR(:,:), XN_ESTXRC(:,:), XN_ESTXLF(:,:)
      REAL*4, ALLOCATABLE :: XR_CPDCON(:), XR_CPDSLP(:)  !temporary XML input variable to read CPDCON, CPDSLP from XML input file  (Row: EMM regions)
      REAL*4, ALLOCATABLE :: XR_CPTCON(:), XR_CPTSLP(:)  !temporary XML input variable to read CPTCON, CPTSLP from XML input file  (Row: EMM regions)
      REAL*4, ALLOCATABLE :: XR_GAADJ_PROD(:,:)   !temporary XML input variable to read GAADJ for production from XML input file  (Row: Year 1990~2050, Column: EMM regions)
      REAL*4, ALLOCATABLE :: XR_GAADJ_TRANS(:,:)   !temporary XML input variable to read GAADJ for transmission from XML input file  (Row: Year 1990~2050, Column: EMM regions)
      REAL*4, ALLOCATABLE :: XR_GAADJ_DIST(:,:)   !temporary XML input variable to read GAADJ for distribution from XML input file  (Row: Year 1990~2050, Column: EMM regions)
      REAL*4, ALLOCATABLE :: XR_ERCCA(:,:)  !temporary XML input variable to read ERCCA from XML input file (Row: EMM regions, Column: plant types)
      REAL*4, ALLOCATABLE :: XR_EOCOST(:,:,:), XR_EOVBK(:,:,:), XR_MORE_BK(:,:,:)
      REAL*4, ALLOCATABLE :: XR_ELTLSL(:,:),XR_TRABDE(:,:),XR_TROB(:,:),XR_TRST(:,:),XR_TRCS(:,:), XR_TRPS(:,:), XR_TRRE(:,:), XR_ESEMDB(:,:), XR_ESEMPB(:,:)
      REAL*4, ALLOCATABLE :: XR_TRRVRQ(:,:), XR_TRWC(:,:), XR_TRDITC(:,:), XR_TRPRDF(:,:), XR_EGTXRT(:,:), XR_ECTXRT(:,:), XR_TRAMD(:,:)
      INTEGER, ALLOCATABLE :: XR_OPAVRB(:,:), XR_OPITCRB(:,:)
      REAL*4, ALLOCATABLE :: XR_ESRBAF(:,:), XR_ESFLPR(:,:), XR_ESFPDB(:,:), XR_ESSTXR(:,:), XR_ESSLTX(:,:), XR_ESPTXR(:,:)
      REAL*4, ALLOCATABLE :: XR_EILAG(:,:,:), XR_EDIVRT(:,:,:), XR_ESCWPP(:,:,:), XR_ESTSHR(:,:,:), XR_ESPRLT(:,:,:), XR_ESPRST(:,:,:)
      REAL*4, ALLOCATABLE :: XR_ESPRCE(:,:,:), XR_ESPRPS(:,:,:), XR_ESRTLT(:,:,:), XR_ESRTST(:,:,:), XR_ESRTCE(:,:,:), XR_ESRTPS(:,:,:)
      REAL*4, ALLOCATABLE :: XR_FRMARG(:,:), XR_COMPRM(:,:), XR_SCSREC(:,:)
      REAL*4, ALLOCATABLE :: XR_THETA(:,:), XR_TDMRG(:,:,:), XR_ESPERDA(:,:,:), XR_ESPEREX(:,:,:)
      INTEGER ILCP        !LENGTH OF CONSTRUCTION PERIOD
      INTEGER VILAG(MNUMYR,2)
      INTEGER IY1,JY,NRGN2
      INTEGER Row_Num_Check, IY_Num_Check
      INTEGER WKUNIT, XN_ROW, XR_ROW, XR_YR_ROW
      character*30 sheet
      character(len=8)                           :: ownertxt
      real                                       :: xtmp_MORE_BK(MNUMNR,EFP_D_CAP,2)
      integer                                    :: efpt
       
!
!
      common/nemswk1/xmlout
      integer xmlout
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
      REAL*4 XN_EOBKLF(NPTYP*2),XN_EOTXLF(NPTYP*2),XN_EODEPR(NPTYP*2)

          type(sqlite_database)                      :: db
          type(sqlite_statement)                     :: stmt
          type(sqlite_statement)                     :: stmt2
          type(sqlite_statement)                     :: stmt3
          type(sqlite_statement)                     :: stmt4
          type(sqlite_statement)                     :: stmt5
          type(sqlite_statement)                     :: stmt6
          type(sqlite_statement)                     :: stmt7
          type(sqlite_column), dimension(:), pointer :: col
          type(sqlite_column), dimension(:), pointer :: col2
          type(sqlite_column), dimension(:), pointer :: col3
          type(sqlite_column), dimension(:), pointer :: col4
          type(sqlite_column), dimension(:), pointer :: col5
          type(sqlite_column), dimension(:), pointer :: col6
          type(sqlite_column), dimension(:), pointer :: col7
          character(len=40), pointer, dimension(:)   :: result
          character(len=80)                          :: errmsg
          character(len=4)                           :: intstr
          character(len=80)                          :: condstr
          logical                                    :: finished      

!td   REAL*4 RTBSADJ(MNUMNR)
!td   DATA RTBSADJ/1.2,1.4,1.5,1.0,1.2,1.5,1.4,1.3,1.1,
!td                1.2,1.3,1.1,1.5,1.0,1.0,1.0/
!MPTIM3      INTEGER CPU_START1,CPU_END1,WALL_START1,WALL_END1      !//MPTIM3//
!MPTIM3      INTEGER CPU_START2,CPU_END2,WALL_START2,WALL_END2      !//MPTIM3//
!MPTIM3      REAL OTHCPU,OTHWAL
!
!MPTIM3           OTHCPU = 0.0
!MPTIM3           OTHWAL = 0.0
!New XML FINREG read starts here  added by AKN 2015

      NEW = .FALSE.
      UF_TMP = FILE_MGR('O','FINREGY',NEW)
      wkunit = uf_tmp

      NEW = .TRUE.
      XMLOUT = FILE_MGR('O','FINREGOUT',NEW)      ! debug file to echo data read from xml file
!MPTIM3      CALL MPTIM3(CPU_START2,WALL_START2)
      !IN = FILE_MGR('O',FNAME,NEW)             !AKN Comment this out after testing XML reading
!MPTIM3      CALL MPTIM3(CPU_END2,WALL_END2)
!MPTIM3      WRITE (6,2323) '** TIMING FILE_MGR ** ',
!MPTIM3     2           FLOAT(CPU_END2)/100.-FLOAT(CPU_START2)/100.-OTHCPU,
!MPTIM3     3           FLOAT(WALL_END2)/100.-FLOAT(WALL_START2)/100.-OTHWAL
! FIRST READ INFORMATION THAT DOES NOT VARY BY REGION OR YEAR:
! READ IN DEFLATION FACTORS FOR EXISTING ASSET CAPITAL COSTS
! (FOR HISTORICAL YEARS (UHBSYR=1989) AND BACK)
!MPTIM3      CALL MPTIM3(CPU_START1,WALL_START1)

      ALLOCATE(XN_CAPTSCALE(MNUMYR-10,MNUMNR))
      ALLOCATE(XN_CAPDSCALE(MNUMYR-10,MNUMNR))
      ALLOCATE(XN_OMTSCALE(MNUMYR-10,MNUMNR))
      ALLOCATE(XN_OMDSCALE(MNUMYR-10,MNUMNR))
      ALLOCATE(XN_ERMECLAG(MNUMYR-10,MNUMNR))
      ALLOCATE(XN_ESBKLF(2,EFP_D_CAP))
      ALLOCATE(XN_ESDEPR(2,EFP_D_CAP))
      ALLOCATE(XN_ESTXRC(2,EFP_D_CAP))
      ALLOCATE(XN_ESTXLF(2,EFP_D_CAP))
      ALLOCATE(XR_CPDCON(MNUMNR))
      ALLOCATE(XR_CPDSLP(MNUMNR))
      ALLOCATE(XR_CPTCON(MNUMNR))
      ALLOCATE(XR_CPTSLP(MNUMNR))
      ALLOCATE(XR_GAADJ_PROD(MNUMYR,MNUMNR))
      ALLOCATE(XR_GAADJ_TRANS(MNUMYR,MNUMNR))
      ALLOCATE(XR_GAADJ_DIST(MNUMYR,MNUMNR))
      ALLOCATE(XR_ERCCA(MNUMNR,EFP_D_CAP))
      ALLOCATE(XR_EOCOST(MNUMNR,EFP_D_CAP,2))
      ALLOCATE(XR_EOVBK(MNUMNR,EFP_D_CAP,2))
      ALLOCATE(XR_MORE_BK(MNUMNR,EFP_D_CAP,2))
      ALLOCATE(XR_ELTLSL(MNUMNR,2))
      ALLOCATE(XR_TRABDE(MNUMNR,2))
      ALLOCATE(XR_TROB(MNUMNR,2))
      ALLOCATE(XR_TRST(MNUMNR,2))
      ALLOCATE(XR_TRCS(MNUMNR,2))
      ALLOCATE(XR_TRPS(MNUMNR,2))
      ALLOCATE(XR_TRRE(MNUMNR,2))
      ALLOCATE(XR_ESEMDB(MNUMNR,2))
      ALLOCATE(XR_ESEMPB(MNUMNR,2))
      ALLOCATE(XR_TRRVRQ(MNUMNR,2))
      ALLOCATE(XR_TRWC(MNUMNR,2))
      ALLOCATE(XR_TRDITC(MNUMNR,2))
      ALLOCATE(XR_TRPRDF(MNUMNR,2))
      ALLOCATE(XR_EGTXRT(MNUMNR,2))
      ALLOCATE(XR_ECTXRT(MNUMNR,2))
      ALLOCATE(XR_TRAMD(MNUMNR,2))
      ALLOCATE(XR_OPAVRB(MNUMNR,2))
      ALLOCATE(XR_OPITCRB(MNUMNR,2))
      ALLOCATE(XR_ESRBAF(MNUMNR,2))
      ALLOCATE(XR_ESFLPR(MNUMNR,2))
      ALLOCATE(XR_ESFPDB(MNUMNR,2))
      ALLOCATE(XR_ESSTXR(MNUMNR,2))
      ALLOCATE(XR_ESSLTX(MNUMNR,2))
      ALLOCATE(XR_ESPTXR(MNUMNR,2))
      ALLOCATE(XR_EILAG(MNUMYR,MNUMNR,2))
      ALLOCATE(XR_EDIVRT(MNUMYR,MNUMNR,2))
      ALLOCATE(XR_ESCWPP(MNUMYR,MNUMNR,2))
      ALLOCATE(XR_ESTSHR(MNUMYR,MNUMNR,2))
      ALLOCATE(XR_ESPRLT(MNUMYR,MNUMNR,2))
      ALLOCATE(XR_ESPRST(MNUMYR,MNUMNR,2))
      ALLOCATE(XR_ESPRCE(MNUMYR,MNUMNR,2))
      ALLOCATE(XR_ESPRPS(MNUMYR,MNUMNR,2))
      ALLOCATE(XR_ESRTLT(MNUMYR,MNUMNR,2))
      ALLOCATE(XR_ESRTST(MNUMYR,MNUMNR,2))
      ALLOCATE(XR_ESRTCE(MNUMYR,MNUMNR,2))
      ALLOCATE(XR_ESRTPS(MNUMYR,MNUMNR,2))
      ALLOCATE(XR_FRMARG(MNUMYR,MNUMNR))
      ALLOCATE(XR_COMPRM(MNUMYR,MNUMNR))
      ALLOCATE(XR_SCSREC(MNUMYR,MNUMNR))
      ALLOCATE(XR_THETA(MNUMNR, 3))
      ALLOCATE(XR_TDMRG(3, MNUMNR, 2))
      ALLOCATE(XR_ESPERDA(MNUMYR,MNUMNR, 4))
      ALLOCATE(XR_ESPEREX(MNUMYR,MNUMNR, 4))

     
!
!GET REGIONAL DATA FROM SQLite db instead of from FINREG spreadsheet
!

!          write( 901, '(a18)') "V_EMM_EFP_REG_ANN"
          
          call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )
          allocate ( col(21) )
        ! QUERY THE DATABASE TABLE
          call sqlite3_column_query( col(1), 'EMM_REG', sqlite_int )
          call sqlite3_column_query( col(2), 'YEAR', sqlite_int )
          call sqlite3_column_query( col(3), 'CAPTSCALE', sqlite_real )
          call sqlite3_column_query( col(4), 'CAPDSCALE', sqlite_real )
          call sqlite3_column_query( col(5), 'OMTSCALE', sqlite_real )
          call sqlite3_column_query( col(6), 'OMDSCALE', sqlite_real )
          call sqlite3_column_query( col(7), 'ERMECLAG', sqlite_real )
          call sqlite3_column_query( col(8), 'GAADJ_PROD', sqlite_real )
          call sqlite3_column_query( col(9), 'GAADJ_TRANS', sqlite_real )
          call sqlite3_column_query( col(10), 'GAADJ_DIST', sqlite_real )
          call sqlite3_column_query( col(11), 'FRMARG', sqlite_real )
          call sqlite3_column_query( col(12), 'COMPRM', sqlite_int )
          call sqlite3_column_query( col(13), 'SCSREC', sqlite_real )
          call sqlite3_column_query( col(14), 'ESPERDA1', sqlite_real )
          call sqlite3_column_query( col(15), 'ESPERDA2', sqlite_real )
          call sqlite3_column_query( col(16), 'ESPERDA3', sqlite_real )
          call sqlite3_column_query( col(17), 'ESPERDA4', sqlite_real )
          call sqlite3_column_query( col(18), 'ESPEREX1', sqlite_real )
          call sqlite3_column_query( col(19), 'ESPEREX2', sqlite_real )
          call sqlite3_column_query( col(20), 'ESPEREX3', sqlite_real )
          call sqlite3_column_query( col(21), 'ESPEREX4', sqlite_real )
          
       ! write(intstr,'(I2.2)') UNRGNS
         write(condstr,'(A,I2.2)') 'where EMM_REG <= ',MNUMNR
         call sqlite3_prepare_select( db, 'V_EMM_EFP_REG_ANN', col, stmt )

        
        ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
          do
             call sqlite3_next_row( stmt, col, finished )
             if ( finished ) exit

             call sqlite3_get_column( col(1), rgn)
             call sqlite3_get_column( col(2), yr)
             call sqlite3_get_column( col(3), CAPTSCALE(rgn,yr-1989))
             call sqlite3_get_column( col(4), CAPDSCALE(rgn,yr-1989))
             call sqlite3_get_column( col(5), OMTSCALE(rgn,yr-1989))
             call sqlite3_get_column( col(6), OMDSCALE(rgn,yr-1989))
             call sqlite3_get_column( col(7), ERMECLAG(yr-1989,rgn))
             call sqlite3_get_column( col(8), GAADJ(1,rgn,yr-1989))
             call sqlite3_get_column( col(9), GAADJ(2,rgn,yr-1989))
             call sqlite3_get_column( col(10), GAADJ(3,rgn,yr-1989))
             call sqlite3_get_column( col(11), FRMARG(yr-1989,rgn)) 
             call sqlite3_get_column( col(12), COMPRM(yr-1989,rgn))
             call sqlite3_get_column( col(13), SCSREC(yr-1989,rgn))
             call sqlite3_get_column( col(14), XR_ESPERDA(yr-1989,rgn,1))
             call sqlite3_get_column( col(15), XR_ESPERDA(yr-1989,rgn,2))
             call sqlite3_get_column( col(16), XR_ESPERDA(yr-1989,rgn,3))
             call sqlite3_get_column( col(17), XR_ESPERDA(yr-1989,rgn,4))
             call sqlite3_get_column( col(18), XR_ESPEREX(yr-1989,rgn,1))
             call sqlite3_get_column( col(19), XR_ESPEREX(yr-1989,rgn,2))
             call sqlite3_get_column( col(20), XR_ESPEREX(yr-1989,rgn,3))
             call sqlite3_get_column( col(21), XR_ESPEREX(yr-1989,rgn,4))
          enddo
     !
         !initialize ESPERDA pre-2000 values
          do i = 1,10
           XR_ESPERDA(i,:,:)=XR_ESPERDA(11,:,:)
           XR_ESPEREX(i,:,:)=XR_ESPEREX(11,:,:)   
           FRMARG(i,:)=FRMARG(11,:)  
           GAADJ(:,:,i)=GAADJ(:,:,11)  
           COMPRM(i,:)=COMPRM(11,:)
           SCSREC(i,:)=SCSREC(11,:)  
          enddo
         
          deallocate ( col )
          finished = .FALSE.
         
          allocate ( col2(15) )
        ! QUERY THE DATABASE TABLE
          call sqlite3_column_query( col2(1), 'EMM_REG', sqlite_int )
          call sqlite3_column_query( col2(2), 'YEAR', sqlite_int )
          call sqlite3_column_query( col2(3), 'OWNER', sqlite_char )
          call sqlite3_column_query( col2(4), 'EILAG', sqlite_real )
          call sqlite3_column_query( col2(5), 'EDIVRT', sqlite_real )
          call sqlite3_column_query( col2(6), 'ESCWPP', sqlite_real )
          call sqlite3_column_query( col2(7), 'ESTSHR', sqlite_real )
          call sqlite3_column_query( col2(8), 'ESPRST', sqlite_real )
          call sqlite3_column_query( col2(9), 'ESPRCE', sqlite_real )
          call sqlite3_column_query( col2(10), 'ESPRPS', sqlite_real )
          call sqlite3_column_query( col2(11), 'ESPRLT', sqlite_real )
          call sqlite3_column_query( col2(12), 'ESRTLT', sqlite_real )
          call sqlite3_column_query( col2(13), 'ESRTST', sqlite_real )
          call sqlite3_column_query( col2(14), 'ESRTCE', sqlite_real )
          call sqlite3_column_query( col2(15), 'ESRTPS', sqlite_real )
          
       ! write(intstr,'(I2.2)') UNRGNS
          write(condstr,'(A,I2.2)') 'where EMM_REG <= ',MNUMNR
          call sqlite3_prepare_select( db, 'V_EMM_EFP_EILAG_ETC', col2, stmt2, condstr )
        
        ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
          do
             call sqlite3_next_row( stmt2, col2, finished )
             if ( finished ) exit

             call sqlite3_get_column( col2(1), rgn)
             call sqlite3_get_column( col2(2), yr)
             call sqlite3_get_column( col2(3), ownertxt)
             if (trim(ownertxt) .eq. 'private') then 
                i = 1
             else
                i = 2
             endif
             call sqlite3_get_column( col2(4), XR_EILAG(yr-1989,rgn, i))
             call sqlite3_get_column( col2(5), XR_EDIVRT(yr-1989,rgn, i))
             call sqlite3_get_column( col2(6), XR_ESCWPP(yr-1989,rgn, i))
             call sqlite3_get_column( col2(7), XR_ESTSHR(yr-1989,rgn, i))
             call sqlite3_get_column( col2(8), XR_ESPRST(yr-1989,rgn, i))
             call sqlite3_get_column( col2(9), XR_ESPRCE(yr-1989,rgn, i))
             call sqlite3_get_column( col2(10), XR_ESPRPS(yr-1989,rgn, i))
             call sqlite3_get_column( col2(11), XR_ESPRLT(yr-1989,rgn, i))
             call sqlite3_get_column( col2(12), XR_ESRTLT(yr-1989,rgn, i))
             call sqlite3_get_column( col2(13), XR_ESRTST(yr-1989,rgn, i))
             call sqlite3_get_column( col2(14), XR_ESRTCE(yr-1989,rgn, i))
             call sqlite3_get_column( col2(15), XR_ESRTPS(yr-1989,rgn, i))

          enddo
         
          deallocate ( col2 )         
          finished = .FALSE.

          allocate ( col3(3) )
        ! QUERY THE DATABASE TABLE
          call sqlite3_column_query( col3(1), 'EMM_REG', sqlite_int )
          call sqlite3_column_query( col3(2), 'EFPCAP', sqlite_int )
          call sqlite3_column_query( col3(3), 'ERCCA', sqlite_real )
          
       ! write(intstr,'(I2.2)') UNRGNS
          write(condstr,'(A,I2.2)') 'where EMM_REG <= ',MNUMNR
          call sqlite3_prepare_select( db, 'V_EMM_EFP_ERCCA', col3, stmt3, condstr )
        
        ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
          do
             call sqlite3_next_row( stmt3, col3, finished )
             if ( finished ) exit

             call sqlite3_get_column( col3(1), rgn)
             call sqlite3_get_column( col3(2), efpt)
             call sqlite3_get_column( col3(3), XR_ERCCA(rgn,efpt))

          enddo
         
          deallocate ( col3 )
          finished = .FALSE.

         allocate ( col4(6) )
        ! QUERY THE DATABASE TABLE
          call sqlite3_column_query( col4(1), 'EMM_REG', sqlite_int )
          call sqlite3_column_query( col4(2), 'EFPCAP', sqlite_int )
          call sqlite3_column_query( col4(3), 'Owner', sqlite_char )
          call sqlite3_column_query( col4(4), 'EOCOST', sqlite_real )
          call sqlite3_column_query( col4(5), 'EOVBK', sqlite_real )
          call sqlite3_column_query( col4(6), 'MORE_BK', sqlite_real )
          
       ! write(intstr,'(I2.2)') UNRGNS
          write(condstr,'(A,I2.2)') 'where EMM_REG <= ',MNUMNR
          call sqlite3_prepare_select( db, 'V_EMM_EFP_EO', col4, stmt4, condstr )
        
        ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
          do
             call sqlite3_next_row( stmt4, col4, finished )
             if ( finished ) exit

             call sqlite3_get_column( col4(1), rgn)
             call sqlite3_get_column( col4(2), efpt)
             call sqlite3_get_column( col4(3), ownertxt)
             if (trim(ownertxt) .eq. 'private') then 
                     i = 1
             else 
                     i = 2
             endif
             call sqlite3_get_column( col4(4), XR_EOCOST(rgn,efpt,i))
             call sqlite3_get_column( col4(5), XR_EOVBK(rgn,efpt,i))
             call sqlite3_get_column( col4(6), XR_MORE_BK(rgn,efpt,i))
             if (efpt .eq. 1) XR_MORE_BK(rgn,efpt,i) = XR_MORE_BK(rgn,efpt,i)

          enddo
         
          deallocate ( col4 )
          finished = .FALSE.


          allocate ( col5(23) )
        ! QUERY THE DATABASE TABLE
          call sqlite3_column_query( col5(1), 'EMM_REG', sqlite_int )
          call sqlite3_column_query( col5(2), 'CPDCON', sqlite_real )
          call sqlite3_column_query( col5(3), 'CPDSLP', sqlite_real )
                    call sqlite3_column_query( col5(4), 'OMDCON', sqlite_real )
	            call sqlite3_column_query( col5(5), 'OMDSLP', sqlite_real )
	            call sqlite3_column_query( col5(6), 'OMDSLP2', sqlite_real )
          call sqlite3_column_query( col5(7), 'CPTCON', sqlite_real )
          call sqlite3_column_query( col5(8), 'CPTSLP', sqlite_real )
                    call sqlite3_column_query( col5(9), 'OMTCON', sqlite_real )
	            call sqlite3_column_query( col5(10), 'OMTSLP', sqlite_real )
	            call sqlite3_column_query( col5(11), 'OMTSLP2', sqlite_real )
          call sqlite3_column_query( col5(12), 'OVERTR', sqlite_real )
          call sqlite3_column_query( col5(13), 'OVERDS', sqlite_real )
          call sqlite3_column_query( col5(14), 'OVERPR', sqlite_real )
          call sqlite3_column_query( col5(15), 'THETA_RES', sqlite_real )
          call sqlite3_column_query( col5(16), 'THETA_COMM', sqlite_real )
          call sqlite3_column_query( col5(17), 'THETA_IND', sqlite_real )
          call sqlite3_column_query( col5(18), 'TDMRG_TX_RES', sqlite_real )
          call sqlite3_column_query( col5(19), 'TDMRG_TX_COM', sqlite_real )
          call sqlite3_column_query( col5(20), 'TDMRG_TX_IND', sqlite_real )
          call sqlite3_column_query( col5(21), 'TDMRG_DST_RES', sqlite_real )
          call sqlite3_column_query( col5(22), 'TDMRG_DST_COM', sqlite_real )
          call sqlite3_column_query( col5(23), 'TDMRG_DST_IND', sqlite_real )

          
       ! write(intstr,'(I2.2)') UNRGNS
          write(condstr,'(A,I2.2)') 'where EMM_REG <= ',MNUMNR
          call sqlite3_prepare_select( db, 'V_EMM_EFP_REG_TD', col5, stmt5 )
        
        ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
          do
             call sqlite3_next_row( stmt5, col5, finished )
             if ( finished ) exit

             call sqlite3_get_column( col5(1), rgn)
             call sqlite3_get_column( col5(2), XR_CPDCON(rgn))
             call sqlite3_get_column( col5(3), XR_CPDSLP(rgn))
                          call sqlite3_get_column( col5(4), OMDCON(rgn))
	                  call sqlite3_get_column( col5(5), OMDSLP(rgn))
	                  call sqlite3_get_column( col5(6), OMDSLP2(rgn))
             call sqlite3_get_column( col5(7), XR_CPTCON(rgn))
             call sqlite3_get_column( col5(8), XR_CPTSLP(rgn))
                          call sqlite3_get_column( col5(9), OMTCON(rgn))
	                  call sqlite3_get_column( col5(10), OMTSLP(rgn))
	                  call sqlite3_get_column( col5(11), OMTSLP2(rgn))
	     call sqlite3_get_column( col5(12), OVERTR(rgn))	                  
             call sqlite3_get_column( col5(13), OVERDS(rgn))
             call sqlite3_get_column( col5(14), OVERPR(rgn))
             call sqlite3_get_column( col5(15), XR_THETA(rgn,1))
             call sqlite3_get_column( col5(16), XR_THETA(rgn,2))
             call sqlite3_get_column( col5(17), XR_THETA(rgn,3))
             call sqlite3_get_column( col5(18), TDMRG(1,rgn,1))
             call sqlite3_get_column( col5(19), TDMRG(2,rgn,1))
             call sqlite3_get_column( col5(20), TDMRG(3,rgn,1))
             call sqlite3_get_column( col5(21), TDMRG(1,rgn,2))
             call sqlite3_get_column( col5(22), TDMRG(2,rgn,2))
             call sqlite3_get_column( col5(23), TDMRG(3,rgn,2))

          enddo
         
          deallocate ( col5 )
          finished = .FALSE.
           
          allocate ( col6(26) )
        ! QUERY THE DATABASE TABLE
          call sqlite3_column_query( col6(1), 'EMM_REG', sqlite_int )
          call sqlite3_column_query( col6(2), 'Owner', sqlite_char )
          call sqlite3_column_query( col6(3), 'EQTLSL', sqlite_real )
          call sqlite3_column_query( col6(4), 'ERABDE', sqlite_real )
          call sqlite3_column_query( col6(5), 'EROB', sqlite_real )
          call sqlite3_column_query( col6(6), 'ERBL_18', sqlite_real )
          call sqlite3_column_query( col6(7), 'ERBL_13', sqlite_real )
          call sqlite3_column_query( col6(8), 'ERPREF', sqlite_real )
          call sqlite3_column_query( col6(9), 'ERBL_15', sqlite_real )
          call sqlite3_column_query( col6(10), 'ESEMDT', sqlite_real )
          call sqlite3_column_query( col6(11), 'ESEMPS', sqlite_real )
          call sqlite3_column_query( col6(12), 'ERRVRQ', sqlite_real )
          call sqlite3_column_query( col6(13), 'ERBL_11', sqlite_real )
          call sqlite3_column_query( col6(14), 'ERDITC', sqlite_real )
          call sqlite3_column_query( col6(15), 'ERPRDF', sqlite_real )
          call sqlite3_column_query( col6(16), 'EGTXRT', sqlite_real )
          call sqlite3_column_query( col6(17), 'ECTXRT', sqlite_real )
          call sqlite3_column_query( col6(18), 'ERAMD', sqlite_real )
          call sqlite3_column_query( col6(19), 'OPAVRB', sqlite_int )
          call sqlite3_column_query( col6(20), 'OPITCRB', sqlite_int )
          call sqlite3_column_query( col6(21), 'ESRBAF', sqlite_int )
          call sqlite3_column_query( col6(22), 'ESFLPR', sqlite_int )
          call sqlite3_column_query( col6(23), 'ESFPDB', sqlite_int )
          call sqlite3_column_query( col6(24), 'ESSTX', sqlite_int )
          call sqlite3_column_query( col6(25), 'ESSLTX', sqlite_int )
          call sqlite3_column_query( col6(26), 'EXPTXR', sqlite_int )
          
       ! write(intstr,'(I2.2)') UNRGNS
          write(condstr,'(A,I2.2)') 'where EMM_REG <= ',MNUMNR
          call sqlite3_prepare_select( db, 'V_EMM_EFP_F1', col6, stmt6, condstr )
        
        ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
          do
             call sqlite3_next_row( stmt6, col6, finished )
             if ( finished ) exit

             call sqlite3_get_column( col6(1), rgn)
             call sqlite3_get_column( col6(2), ownertxt)
             if (trim(ownertxt) .eq. 'private') then
                      i = 1
             else 
                      i = 2
             endif
             call sqlite3_get_column( col6(3), XR_ELTLSL(rgn,i))
             call sqlite3_get_column( col6(4), XR_TRABDE(rgn,i))
             call sqlite3_get_column( col6(5), XR_TROB(rgn,i))
             call sqlite3_get_column( col6(6), XR_TRST(rgn,i))
             call sqlite3_get_column( col6(7), XR_TRCS(rgn,i))
             call sqlite3_get_column( col6(8), XR_TRPS(rgn,i))
             call sqlite3_get_column( col6(9), XR_TRRE(rgn,i))
             call sqlite3_get_column( col6(10), XR_ESEMDB(rgn,i))
             call sqlite3_get_column( col6(11), XR_ESEMPB(rgn,i))
             call sqlite3_get_column( col6(12), XR_TRRVRQ(rgn,i))
             call sqlite3_get_column( col6(13), XR_TRWC(rgn,i))
             call sqlite3_get_column( col6(14), XR_TRDITC(rgn,i))
             call sqlite3_get_column( col6(15), XR_TRPRDF(rgn,i))
             call sqlite3_get_column( col6(16), XR_EGTXRT(rgn,i))
             call sqlite3_get_column( col6(17), XR_ECTXRT(rgn,i))
             call sqlite3_get_column( col6(18), XR_TRAMD(rgn,i))
             call sqlite3_get_column( col6(19), XR_OPAVRB(rgn,i))
             call sqlite3_get_column( col6(20), XR_OPITCRB(rgn,i))
             call sqlite3_get_column( col6(21), XR_ESRBAF(rgn,i))
             call sqlite3_get_column( col6(22), XR_ESFLPR(rgn,i))
             call sqlite3_get_column( col6(23), XR_ESFPDB(rgn,i))
             call sqlite3_get_column( col6(24), XR_ESSTXR(rgn,i))
             call sqlite3_get_column( col6(25), XR_ESSLTX(rgn,i))
             call sqlite3_get_column( col6(26), XR_ESPTXR(rgn,i))
          enddo
         
          deallocate ( col6 )
          finished = .FALSE.
         
           allocate ( col7(6) )
        ! QUERY THE DATABASE TABLE
          call sqlite3_column_query( col7(1), 'OWNERSHIP', sqlite_char )
          call sqlite3_column_query( col7(2), 'EFPCAP', sqlite_int )  
          call sqlite3_column_query( col7(3), 'ESBKLF', sqlite_real )       
          call sqlite3_column_query( col7(4), 'ESDEPR', sqlite_real )  
          call sqlite3_column_query( col7(5), 'ESTXRC', sqlite_real ) 
          call sqlite3_column_query( col7(6), 'ESTXLF', sqlite_real )  
          call sqlite3_prepare_select( db, 'V_EMM_EFP_BKTX', col7, stmt7 )        
          
          do
             call sqlite3_next_row( stmt7, col7, finished )
             if ( finished ) exit
                          
             call sqlite3_get_column( col7(1), ownertxt )
             if (trim(ownertxt) .eq. 'private') then
                      i = 1
             else 
                      i = 2
             endif
             call sqlite3_get_column( col7(2), efpt )
             call sqlite3_get_column( col7(3), XN_ESBKLF(i,efpt) )
             call sqlite3_get_column( col7(4), XN_ESDEPR(i,efpt) )
             call sqlite3_get_column( col7(5), XN_ESTXLF(i,efpt) )
             call sqlite3_get_column( col7(6), XN_ESTXRC(i,efpt) )
          enddo          
          
          deallocate ( col7 )                        
         
          call sqlite3_close( db )
!
!END GET REGIONAL DATA FROM SQLite db instead of from FINREG spreadsheet
      SHEET='ntl_eovdef'
      call readRngXLSX(wkunit,trim(sheet))
      call getrngr('XN_EOVDEF          ',EOVDEF,size(EOVDEF),1,1)  !Read HISTORICAL DEFLATION FACTORS (EOVDEF)
!
! READ IN NUMBER OF VINTAGE YEARS

      SHEET='ntl_eovyrs'
      call readRngXLSX(wkunit,trim(sheet))
      call getrngi4('XN_EOVYRS          ',EOVYRS,1,1,1)  !Read  NUMBER OF VINTAGE YEARS (EOVYRS)

! READ IN CONSTRUCTION PROFILES FOR PRODUCTION ASSETS

      SHEET='ntl_eslcp'
      call readRngXLSX(wkunit,trim(sheet))
      call getrngr('XN_ESLCP          ',ESLCP,size(ESLCP, dim=1),1,1)  !Read  LENGTH OF CONSTRUCTION PERIOD (ESLCP) for each plant type
      call getrngr('XN_ESCPRF          ',ESCPRF,size(ESCPRF, dim=1),size(ESCPRF, dim=2),1)  !Read  SHARE OF COST INCURRED IN EACH CONSTRUCTION YEAR (ESCPRF)


! READ BOOK LIFE, TAX LIFE DEPRECIATION RATE (EXISTING ASSETS)

      SHEET='ntl_life'
      call readRngXLSX(wkunit,trim(sheet))
      call getrngr('XN_EOBKLF          ',XN_EOBKLF,size(XN_EOBKLF),1,1)  !Read  BOOKLIFE (EOBKLF) for each plant type (two rows:1=private, 2=public for each plant type)
      call getrngr('XN_EOTXLF          ',XN_EOTXLF,size(XN_EOTXLF),1,1)  !Read  TAX LIFE (EOTXLF) for each plant type
      call getrngr('XN_EODEPR          ',XN_EODEPR,size(XN_EODEPR),1,1)  !Read  DEPRECIATION RATE (ESCPRF) for each plant type

      XN_ROW = 1
      DO I=1,NPTYP
        EOBKLF(I,1) = XN_EOBKLF(XN_ROW)    !converting column arrays to two dimensional arrays (old format)
        EOBKLF(I,2) = XN_EOBKLF(XN_ROW+1)
        EOTXLF(I,1) = XN_EOTXLF(XN_ROW)
        EOTXLF(I,2) = XN_EOTXLF(XN_ROW+1)
        EODEPR(I,1) = XN_EODEPR(XN_ROW)
        EODEPR(I,2) = XN_EODEPR(XN_ROW+1)
        XN_ROW = XN_ROW + 2
      END DO

! READ NEW ASSET BOOK LIFE

      DO I=1,EFP_D_CAP
           DO J=1, 2
              ESBKLF(I,J) = XN_ESBKLF(J,I)   !transpose XN_ESBKLF to fit the orientation of ESBKLF
           END DO
      END DO

! READ NEW ASSET BOOK DEPRECIATION
!
      DO I=1,EFP_D_CAP
           DO J=1, 2
              ESDEPR(I,J) = XN_ESDEPR(J,I)   !transpose XN_ESDEPR to fit the orientation of ESDEPR
           END DO
      END DO


! READ NEW ASSET TAX RECOVERY CLASS

      DO I=1,EFP_D_CAP
           DO J=1, 2
              ESTXRC(I,J) = XN_ESTXRC(J,I)   !transpose XN_ESTXRC to fit the orientation of ESTXRC
           END DO
      END DO

! READ TAX LIFE

      DO I=1,EFP_D_CAP
           DO J=1, 2
              ESTXLF(I,J) = XN_ESTXLF(J,I)   !transpose XN_ESTXLF to fit the orientation of ESTXLF
           END DO
      END DO


! READ TAX LIFE
! READ EPSILON - elasiticity by sector for t&d pricing

      SHEET='ntl_bypass'
      call readRngXLSX(wkunit,trim(sheet))
      call getrngr('XN_EPSILON          ',EPS,size(EPS, dim=1),1,1)  !Read  elasiticity (EPS) by sector for t&d pricing
!     write(6,*) 'eps - ',(EPS(ICL),ICL=1,3)


      call getrngr('XN_SD               ',SD,size(SD, dim=1),1,1)  !Read  share (SD) of sector bypass in 2020
!     write(6,*) 'sd - ',(SD(ICL),ICL=1,3)

! READ SDMidYr - year to hit midpoint of 2020 sector bypass

      call getrngi4('XN_SDMIDYR          ',SDMidYr,size(SDMidYr, dim=1),1,1)  !Read  year(SDMIDYR) to hit midpoint of 2020 sector bypass
!     write(6,*) 'sdyr - ',(SDMidYr(ICL),ICL=1,3)

! READ NU - elasticity to bypass

      call getrngr('XN_NU               ',NU,size(NU, dim=1),1,1)  !Read elasticity (NU) to bypass
!     write(6,*) 'nu - ', NU
!
! READ CAPTSCALE - capital transmission cost adjustment
!
      do j=11,mnumyr
        write(18,25) 'CAPTSCALE in finreg',j+1989,(captscale(nrgn,j),nrgn=1,unrgns)
25      format(1x,a20,2x,i6,22f6.2)
      enddo

! READ CAPDSCALE - capital distribution cost adjustment
!
      do j=11,mnumyr
        write(18,25) 'CAPDSCALE in finreg',j+1989,(capdscale(nrgn,j),nrgn=1,unrgns)
      enddo
!

! READ OMTSCALE - O&M transmission cost adjustment
!
      do j=11,mnumyr
        write(18,25) 'OMTSCALE in finreg',j+1989,(OMTSCALE(nrgn,j),nrgn=1,unrgns)
      enddo

! READ OMDSCALE - O&M distribution cost adjustment
!
      do j=11,mnumyr
        write(18,25) 'OMDSCALE in finreg',j+1989,(OMDSCALE(nrgn,j),nrgn=1,unrgns)
      enddo
!!
! READ ERMECLAG - lag amount for energy component of price
!
!!
 
 
!      do j=11,mnumyr
!        write(6,25) 'OMDSCALE in finreg',j+1989,(omdscale(nrgn,j),nrgn=1,unrgns)
!      enddo
      do j=1,10                   ! fill early years
        do NRGN = 1,UNRGNS
         CAPTSCALE(NRGN,j) = 1.0
         CAPDSCALE(NRGN,j) = 1.0
         OMTSCALE(NRGN,j) = 1.0
         OMDSCALE(NRGN,j) = 1.0
         ERMECLAG(j,NRGN) = 0.0
        enddo
      enddo

!
!
!
!  reading following XML input variables for those in the regional loop
!
! READ DISTRIBUTION BUILD AND O&M EQUATION CONSTANTS AND SLOPES
!
! Read transmission build and o&m equation constants and slopes!
!

      XR_ROW = 1
! NEXT READ INFORMATION THAT VARIES BY REGION:
! READ IN FINANCIAL DATA FOR EXISTING ASSETS BY PLANT TYPE
      DO NRGN=1,  IJUMPEMRGN              !UNRGNS
            CPDCON = XR_CPDCON(NRGN)
            CPDSLP = XR_CPDSLP(NRGN)
            WRITE(18,*)'cpdslp ',NRGN,CPDCON,CPDSLP

            CPTCON = XR_CPTCON(NRGN)
            CPTSLP = XR_CPTSLP(NRGN)
            WRITE(18,*)'cptslp ',NRGN,CPTCON,CPTSLP

    ! CONVERT UNITS FROM $/KW TO $/MW
            DO J=1,EIPROD
                ERCCA(J)=XR_ERCCA(NRGN,J) /1000.0        !Copy contents of XR_ERCCA into ERCCA for given region NRGN
           WRITE(18,*)'XR_ERCCA ' ,NRGN,J,XR_ERCCA(NRGN,J)
                
                EOCOST(J,1) = XR_EOCOST(NRGN,J,1)        !copy contents of XR_EOCOST for private into EOCOST for private
                EOCOST(J,2) = XR_EOCOST(NRGN,J,2)   !copy contents of XR_EOCOST for public into EOCOST for public
            END DO

            DO J=1,NPTYP
                EOVBK(J,1) = XR_EOVBK(NRGN,J,1)   !copy contents of XR_EOVBK for private into EOVBK for private
           WRITE(18,*)'XR_EOVBK ' ,NRGN,J,1, XR_EOVBK(NRGN,J,1) 
                EOVBK(J,2) = XR_EOVBK(NRGN,J,2)   !copy contents of XR_EOVBK for public into EOVBK for public
            END DO
            EOVBK(EIDIST,1) =    EOVBK(EIDIST,1) + XR_MORE_BK(NRGN,EIDIST,1)
            EOVBK(EIDIST,2) =    EOVBK(EIDIST,2) + XR_MORE_BK(NRGN,EIDIST,1)

    !
    ! READ IN FORM 1 FILE
              do M=1,2
                ELTLSL(M) = XR_ELTLSL(NRGN,M)		    
                TRABDE(M) = XR_TRABDE(NRGN,M)		    
                TROB(M) = XR_TROB(NRGN,M)		    
                TRST(M) = XR_TRST(NRGN,M)		    
                TRCS(M) = XR_TRCS(NRGN,M)		    
                TRPS(M) = XR_TRPS(NRGN,M)		    
                TRRE(M) = XR_TRRE(NRGN,M)		    
                ESEMDB(M) = XR_ESEMDB(NRGN,M)		    
                ESEMDL(M) = ESEMDB(M)	                    
                ESEMPB(M) = XR_ESEMPB(NRGN,M)		    
                ESEMPL(M) = ESEMPB(M)			    
                TRRVRQ(M) = XR_TRRVRQ(NRGN,M)		    
                TRWC(M) = XR_TRWC(NRGN,M)		    
                TRDITC(M) = XR_TRDITC(NRGN,M)		    
                TRPRDF(M) = XR_TRPRDF(NRGN,M)		    
                EGTXRT(M) = XR_EGTXRT(NRGN,M)		    
                ECTXRT(M) = XR_ECTXRT(NRGN,M)		    
                TRAMD(M) = XR_TRAMD(NRGN,M)		    
             WRITE(18,*)'TRAMD ',M,NRGN,XR_TRAMD(NRGN,M)	    
              ENDDO					    
    !td       do M=1,2					    
    !td        TRAMD(M) = TRAMD(M)*RTBSADJ(NRGN)	    
    !td       enddo					    
    !							    
            TOTAL = 0.0					    
            DO M=1,2
     ! update input variables for EFP OPTIONS,  CWIP TREATED AS ADUDC OFFSET, % TAX SAVINGS FLOWED THRU,
     ! % FLOW THRU OF AFUDC DEBT PORTION, AND TAX RATES
              OPAVRB(M) = XR_OPAVRB(NRGN,M)		   
              OPITCRB(M)= XR_OPITCRB(NRGN,M)		   
              ESRBAF(M) = XR_ESRBAF(NRGN,M)		   
              ESFLPR(M) = XR_ESFLPR(NRGN,M)		   
              ESFPDB(M) = XR_ESFPDB(NRGN,M)		   
              ESSTXR(M) = XR_ESSTXR(NRGN,M)		   
              ESSLTX(M) = XR_ESSLTX(NRGN,M)		   
              ESPTXR(M) = XR_ESPTXR(NRGN,M)		   
    ! INITIALIZE EL ARRAYS TO 0.0
              ELCIDC(M) = 0.0
              ELRCWP(M) = 0.0
              ELRBRR(M) = 0.0
              ELTOMN(M) = 0.0
              ELTFLN(M) = 0.0
              ELBDE(M) = 0.0
              ELAITC(M) = 0.0
              ELFITC(M) = 0.0
              ELAFDC(M) = 0.0
              ELFFDC(M) = 0.0
              ELOFFS(M) = 0.0
              ELPRTX(M) = 0.0
              ELSLTX(M) = 0.0
              ELTDWO(M) = 0.0
              ELTIEX(M) = 0.0
              ELLLP(M) = 0.0
              ELLAGN(M) = 0.0

              TOTAL=TOTAL+ELTLSL(M)
            END DO

    !*******************************************************************C
    ! BEFORE GOING TO NEXT REGION READ THE INFORMATION FOR THIS REGION
    ! THAT VARIES BY YEAR AS WELL
    ! Read in annual values for private and public sectors


            DO ICL =1,3
              THETA(ICL,NRGN) = XR_THETA(NRGN, ICL)
            END DO

            XR_YR_ROW = 1
            DO IY=1,MNUMYR

        !         IF (IY .EQ. 1) THEN
        ! INITIALIZE EL ARRAYS
                  DO M=1,2


        !update input variables for CAPITAL STRUCTURE-FRACTION OF EACH FINANCING TYPE
                      EILAG(M) = XR_EILAG(IY,NRGN,M)
                      EDIVRT(M) = XR_EDIVRT(IY,NRGN,M)
                  WRITE(18,*)'XR_DIVRT ',IY,NRGN,M, XR_EDIVRT(IY,NRGN,M)
                      ESCWPP(M) = XR_ESCWPP(IY,NRGN,M)
                      ESTSHR(M) = XR_ESTSHR(IY,NRGN,M)
                      ESPRLT(M) = XR_ESPRLT(IY,NRGN,M)
                      ESPRST(M) = XR_ESPRST(IY,NRGN,M)
                      ESPRCE(M) = XR_ESPRCE(IY,NRGN,M)
                      ESPRPS(M) = XR_ESPRPS(IY,NRGN,M)
                      ESRTLT(M) = XR_ESRTLT(IY,NRGN,M)
                      ESRTST(M) = XR_ESRTST(IY,NRGN,M)
                      ESRTCE(M) = XR_ESRTCE(IY,NRGN,M)
                      ESRTPS(M) = XR_ESRTPS(IY,NRGN,M)
                 WRITE(18,*)'XR_ESRTPS ',IY,NRGN,M, XR_ESRTPS(IY,NRGN,M)                      
                      ELPRLT(M) = ESPRLT(M)
                      ELPRST(M) = ESPRST(M)
                      ELPRPS(M) = ESPRPS(M)
                      ELPRCE(M) = ESPRCE(M)
                      IF (TOTAL .gt. 0.0) THEN
                       ESTSHR(M)=ELTLSL(M)/TOTAL
                      ENDIF
                      ESERCE(M)=ESRTCE(M)
                  ENDDO

                  CALL ELEA(EOVDEF,EOCOST,EOVBK,TRABDE,TROB,TRST,TRCS,TRPS, &
                             TRRE,TRWC,TRDITC,TRPRDF,TRAMD,TRRVRQ,NRGN)
        ! INITIALIZE FEDERAL TAX RATE
                  JY=IY+UHBSYR-1985+1
                  JY=MIN(JY,MNUMYR)
                  ESFTXR(1) = TXRTYR(JY)
                  ESFTXR(2) = 0.0
        ! INITIALIZE STRANDED ASSET VARIABLES
                  DO ICL=1,4
                    ESPERDA(ICL) = XR_ESPERDA(IY,NRGN,ICL)
                    ESPEREX(ICL) = XR_ESPEREX(IY,NRGN,ICL)
                  ENDDO

        ! WRITE DIRECT ACCESS FILES FOR YEARLY DATA
                  DO ICOM=1,3
                    IF (ICOM .EQ. 1) ICALL = 'G'
                    IF (ICOM .EQ. 2) ICALL = 'T'
                    IF (ICOM .EQ. 3) ICALL = 'D'
                    CALL STRRCY(NRGN,ICALL,IY)
                  END DO
                  XR_YR_ROW = XR_YR_ROW + 2
            END DO    ! IY = 1,MNUMYR
            XR_ROW = XR_ROW + 2
      END DO      ! NRGN - loop over regions
!    do nrgn=1,unrgns
!       write(6,'(a,i6,2f8.2)')'New Slopes,omd2,omt2 ',nrgn,  &
!                  omdslp2(nrgn), omtslp2(nrgn)
!    enddo
!MPTIM3      CALL MPTIM3(CPU_END1,WALL_END1)
!MPTIM3      WRITE (6,2323) '** TIMING ELREAD   ** ',
!MPTIM3     2           FLOAT(CPU_END1)/100.-FLOAT(CPU_START1)/100.-OTHCPU,
!MPTIM3     3           FLOAT(WALL_END1)/100.-FLOAT(WALL_START1)/100.-OTHWAL
!MPTIM3      CALL MPTIM3(CPU_START1,WALL_START1)

         DEALLOCATE(XN_CAPTSCALE)
         DEALLOCATE(XN_CAPDSCALE)
         DEALLOCATE(XN_OMTSCALE)
         DEALLOCATE(XN_OMDSCALE)
         DEALLOCATE(XN_ERMECLAG)
         DEALLOCATE(XN_ESBKLF)
         DEALLOCATE(XN_ESDEPR)
         DEALLOCATE(XN_ESTXRC)
         DEALLOCATE(XN_ESTXLF)
         DEALLOCATE(XR_CPDCON)
         DEALLOCATE(XR_CPDSLP)
         DEALLOCATE(XR_CPTCON)
         DEALLOCATE(XR_CPTSLP)
         DEALLOCATE(XR_GAADJ_PROD)
         DEALLOCATE(XR_GAADJ_TRANS)
         DEALLOCATE(XR_GAADJ_DIST)
         DEALLOCATE(XR_ERCCA)
         DEALLOCATE(XR_EOCOST)
         DEALLOCATE(XR_EOVBK)
         DEALLOCATE(XR_MORE_BK)
         DEALLOCATE(XR_ELTLSL)
         DEALLOCATE(XR_TRABDE)
         DEALLOCATE(XR_TROB)
         DEALLOCATE(XR_TRST)
         DEALLOCATE(XR_TRCS)
         DEALLOCATE(XR_TRPS)
         DEALLOCATE(XR_TRRE)
         DEALLOCATE(XR_ESEMDB)
         DEALLOCATE(XR_ESEMPB)
         DEALLOCATE(XR_TRRVRQ)
         DEALLOCATE(XR_TRWC)
         DEALLOCATE(XR_TRDITC)
         DEALLOCATE(XR_TRPRDF)
         DEALLOCATE(XR_EGTXRT)
         DEALLOCATE(XR_ECTXRT)
         DEALLOCATE(XR_TRAMD)
         DEALLOCATE(XR_OPAVRB)
         DEALLOCATE(XR_OPITCRB)
         DEALLOCATE(XR_ESRBAF)
         DEALLOCATE(XR_ESFLPR)
         DEALLOCATE(XR_ESFPDB)
         DEALLOCATE(XR_ESSTXR)
         DEALLOCATE(XR_ESSLTX)
         DEALLOCATE(XR_ESPTXR)
         DEALLOCATE(XR_EILAG)
         DEALLOCATE(XR_EDIVRT)
         DEALLOCATE(XR_ESCWPP)
         DEALLOCATE(XR_ESTSHR)
         DEALLOCATE(XR_ESPRLT)
         DEALLOCATE(XR_ESPRST)
         DEALLOCATE(XR_ESPRCE)
         DEALLOCATE(XR_ESPRPS)
         DEALLOCATE(XR_ESRTLT)
         DEALLOCATE(XR_ESRTST)
         DEALLOCATE(XR_ESRTCE)
         DEALLOCATE(XR_ESRTPS)
         DEALLOCATE(XR_FRMARG)
         DEALLOCATE(XR_COMPRM)
         DEALLOCATE(XR_SCSREC)
         DEALLOCATE(XR_THETA)
         DEALLOCATE(XR_TDMRG)
         DEALLOCATE(XR_ESPERDA)
         DEALLOCATE(XR_ESPEREX)

         UF_TMP = FILE_MGR('C','FINREGY',NEW)
         XMLOUT = FILE_MGR('C','FINREGOUT',NEW)

!MPTIM3      WRITE (6,2323) '** TIMING FILE_MGR ** ',
!MPTIM3     2           FLOAT(CPU_END1)/100.-FLOAT(CPU_START1)/100.-OTHCPU,
!MPTIM3     3           FLOAT(WALL_END1)/100.-FLOAT(WALL_START1)/100.-OTHWAL
!MPTIM32323  FORMAT(10X,A,' CPU USED (SECONDS) = ',F10.3,', WALL USED =',F10.3)
!
!
      RETURN
      END
!
      SUBROUTINE ELEA(EOVDEF,EOCOST,EOVBK,TRABDE,TROB,TRST,TRCS,TRPS, &
                      TRRE,TRWC,TRDITC,TRPRDF,TRAMD,TRRVRQ,NRGN)
      IMPLICIT NONE
!********************************************************************
!  THIS SUBROUTINE CALCULATES ACCOUNTS FOR EXISTING ASSETS
!     THE ASSETS INCLUDE BOOK VALUE, ASSET VALUE, ACCUMULATED BOOK DEPRECIATION, AMORTIZATION OF
!     DEFFERED INVESTMENT TAX CREDITS, AND AMORTIZATION OF DEFERRED TAX SAVINGS FROM THE DEBT
!     PORTION OF ALLOWANCE FOR FUNDS USED DURING CONSTRUCTION
!********************************************************************
! INPUT VARIABLES:
!     EBASVL  = VALUE OF ASSETS NET OF AFUDC
!     EBNUM  = NUMBER OF BUILDS
!     EBPTYP = PLANT TYPE FOR EACH BUILD
!     UINUFS = INDEX FOR NUFS OPITIONS
!     EOVAV  = ASSET VALUE OF EXISTING ASSETS BY VINTAGE CLASS
!     EOVBK  = BOOKED VALUE OF EXISTING ASSETS BY VINTAGE CLASS
!     ERDITC = ACCUMULATED DEFERRED ITC
!     USEMDT = EMBEDDED COST OF DEBT
!     USEMPS = EMBEDDED COST OF PREFERRED STOCK
!     ESFLPR = PERCENT OF DEFERRED TAX FLOWED THROUGH
!     ESFPDB = % FLOW THROUGH OF DEBT PORTION OF AFUDC
!     TXRTYR(1) = FEDERAL STATUTORY TAX RATE
!     ESPRCE = PERCENT OF CAPITALIZATION FROM COMMON EQUITY
!     ESPRLT = PERCENT OF CAPITAL OBTAINED WITH LONG TERM DEBT
!     ESPRPS = PERCENT OF CAPITALIZATION FROM PREFERED STOCK
!     ESPRST = PERCENT OF CAPITAL OBTAINED WITH SHORT TERM DEBT
!     ESRITC = INVESTMENT TAX CREDIT RATE
!     USRTCE = COST OF COMMON EQUITY
!     USRTST = COST OF NEW SHORT TERM DEBT
! INTERNAL VARIABLES:
!     AFDCD  = AVERAGE WEIGHTED EMBEDDED COST OF DEBT
!     AFDCE  = AVERAGE WEIGHTED COST OF EQUITY
!     BDITC  = DEFERRED ITC FROM NEW BUILDS EARNED BEFORE BASE YEAR
!     BKLIFE = BOOK LIFE OF ONE OLD ASSET TYPE (TEMP VARIABLE)
!     EDITC  = ITC LEFTOVER AFTER DEDUCTING NEW BUILDS PORTION
!     FRACTD = FRACTION OF AFDC DUE TO PRE-TAX DEBT COST
!     FRACD1 = FRACD * EFFECT TAX RATE * PERCENT NORMALIZE
!     NPTYP  = UCNTYP + 5
!     NRETIR = YEAR PLANT RETIRES
! OUTPUT VARIABLES:
!     UOASVL = ASSET VALUE OF EXISTING ASSETS BY VINTAGE YEAR
!     UOBKVL = BOOKED VALUE OF EXISTING ASSETS BY VINTAGE YEAR
!     UOABDE = ACCUM BOOK DEP FOR EXISTING ASSETS BY VINTAGE YEAR
!     UOAFDC = AMORT OF AFDC BY PLANT, REGION, AND OWNERSHIP *
!     UOAITC = AMORITIZATION OF DEFERRED ITC FROM EXISTING ASSETS
      CHARACTER*1 ICALL
      REAL*4 EOVDEF(85)    !HISTORICAL DEFLATION FACTORS
      REAL*4 EOCOST(14,2)
      REAL*4 EOVBK(14,2)
      REAL*4 TRABDE(2)
      REAL*4 TROB(2)
      REAL*4 TRST(2)
      REAL*4 TRCS(2)
      REAL*4 TRPS(2)
      REAL*4 TRRE(2)
      REAL*4 TRWC(2)
      REAL*4 TRDITC(2)
      REAL*4 TRPRDF(2)
      REAL*4 TRAMD(2)
      REAL*4 TRRVRQ(2)
      REAL*4 URDITC(2,3)
      REAL*4 URPDTB(2,3)
      REAL*4 URABDE(2,3)
      REAL*4 URAMDL(2,3)
      REAL*4 UROBL(2,3)
      REAL*4 URRBL(2,3)
      REAL*4 URPFL(2,3)
      REAL*4 URRVRL(2,3)
      include 'parametr'
      include 'ncntrl'
      include 'efprep'
      include 'emmparm'
      include 'efpr'
      include 'efpgen'
      include 'efprc'
      include 'efprcy'
      include 'efpin'
      include 'efpbld'
      include 'tax'
      include 'control'
      include 'efpcntrl'
      include 'uefdout'
      REAL*8 FACTOR
      REAL*8 RGRW(EFP_D_VIN)         !REAL GROWTH IN CAPACITY VALUE
      REAL*8 GROWTH(MNUMNR),PROFILE(EFP_D_VIN),TEMP
      REAL*4 HIGHEST
      REAL*4 TOABDE(EFP_D_VIN,EFP_D_CAP,EFP_D_OWN)
      REAL*4 TMPBK(EFP_D_CAP)
      REAL*4 TMPBD(EFP_D_CAP)
      REAL*4 PRLT,PRST,PRCE,PRPS
      REAL*4 PBK
      REAL*4 TBK
      REAL*4 DBK
      REAL*4 ABK
      REAL*4 PBD90
      REAL*4 PBD95
      REAL*4 TBD90
      REAL*4 TBD95
      REAL*4 DBD90
      REAL*4 DBD95
      REAL*4 ABD90
      REAL*4 ABD95
      REAL*4 TCOSTY(40)
      REAL*4 TCOST
      REAL*4 DCOST
      REAL*4 COST(40)
      REAL*4 TBRAT(40,16)
      REAL*4 TB1(40,8)
      REAL*4 TB2(40,8)
      REAL*4 PDT(3)
      REAL*4 PDT2(3)
      REAL*4 PIS(3)
      REAL*4 BDITC
      REAL*4 EDITC
      REAL*4 TDITC
      REAL*4 SUMITC
      REAL*4 SCALE
      REAL*4 AFDCE
      REAL*4 AFDCD
      REAL*4 FRACTD
      REAL*4 FRACD1
      REAL*4 SL
      REAL*4 ACCEL
      REAL*4 TEMP1
      REAL*4 TEMP2
      REAL*4 SYDTDR
      REAL*4 TOTAL,TOTAL2
      REAL*4 ABDE(3)
      REAL*4 Y
      REAL*4 TR
      INTEGER NRGN
      INTEGER N
      INTEGER J,I,K
      INTEGER IY,JY,KY
      INTEGER NB
      INTEGER L
      INTEGER ISEC
      INTEGER IYEAR
      INTEGER ICOM
      INTEGER IYR,JYR
      CHARACTER*6 Label
      EQUIVALENCE (TB1(1,1),TBRAT(1,1)),(TB2(1,1),TBRAT(1,9))
!
!     RTCEAV AND RTNBAV ARE THE AVERAGE ALLOWED RETURN ON EQUITY AND
!     DEBT INTEREST RATE, RESPECTIVELY, BY FEDERAL REGION, OVER THE
!     HISTORICAL PERIOD IN WHICH EXISTING ASSETS WERE BOOKED
!     THESE ARE LEGACY VALUES THAT HAVEN'T VARIED BY REGION IN PAST
!
      RTNBAV = 10.0
      RTCEAV = 13.5

!
!     TBRAT ARE THE RATIO OF TAX BASIS TO BOOK VALUE.  THEY VARY BY
!     PLANT TYPE AND YEAR OF SERVICE.  THESE RATIOS WERE ESTIMATED BY
!     MARK INGLIS BASED ON ASSUMPTIONS FROM AN IFFS RUN
!
      DATA TB1/7*.715,.725,.735,.746,.756,.766,.776,.786,.796,.807, &
               .817,.827,.837,.847,20*.858, &
               7*.750,.759,.768,.777,.786,.795,.804,.813,.821,.830, &
               .839,.848,.857,.866,20*.875, &
               7*.750,.759,.768,.777,.786,.795,.804,.813,.821,.830, &
               .839,.848,.857,.866,20*.875, &
               7*.750,.759,.768,.777,.786,.795,.804,.813,.821,.830, &
               .839,.848,.857,.866,20*.875, &
               7*.750,.759,.768,.777,.786,.795,.804,.813,.821,.830, &
               .839,.848,.857,.866,20*.875, &
               7*.750,.759,.768,.777,.786,.795,.804,.813,.821,.830, &
               .839,.848,.857,.866,20*.875, &
               7*.870,.875,.879,.884,.889,.893,.898,.903,.907,.912, &
               .916,.921,.926,.930,20*.935, &
               7*.870,.875,.879,.884,.889,.893,.898,.903,.907,.912, &
               .916,.921,.926,.930,20*.935/
      DATA TB2/7*.870,.875,.879,.884,.889,.893,.898,.903,.907,.912, &
               .916,.921,.926,.930,20*.935, &
               7*.650,.663,.675,.688,.700,.713,.725,.738,.750,.763, &
               .775,.788,.800,.813,20*.825, &
               7*.750,.759,.768,.777,.786,.795,.804,.813,.821,.830, &
               .839,.848,.857,.866,20*.875, &
               7*.770,.778,.786,.795,.803,.811,.819,.828,.836,.844, &
               .852,.860,.869,.877,20*.885, &
               7*.950,.952,.954,.955,.957,.959,.961,.963,.964,.966, &
               .968,.970,.971,.973,20*.975, &
               7*.950,.952,.954,.955,.957,.959,.961,.963,.964,.966, &
               .968,.970,.971,.973,20*.975, &
               7*.950,.952,.954,.955,.957,.959,.961,.963,.964,.966, &
               .968,.970,.971,.973,20*.975, &
               7*.650,.663,.675,.688,.700,.713,.725,.738,.750,.763, &
               .775,.788,.800,.813,20*.825/
!
!     SET GROWTH RATE
!
      GROWTH(1) = 0.96
      GROWTH(2) = 0.96
      GROWTH(3) = 0.96
      GROWTH(4) = 0.96
      GROWTH(5) = 0.96
      GROWTH(6) = 0.96
      GROWTH(7) = 0.96
      GROWTH(8) = 0.96
      GROWTH(9) = 0.96
      GROWTH(10) = 0.96
      GROWTH(11) = 0.96
      GROWTH(12) = 0.96
      GROWTH(13) = 0.96
      GROWTH(14) = 0.96
      GROWTH(15) = 0.96
      GROWTH(16) = 0.96
      GROWTH(17) = 0.96
      GROWTH(18) = 0.96
      GROWTH(19) = 0.96
      GROWTH(20) = 0.96
      GROWTH(21) = 0.96
      GROWTH(22) = 0.96
      GROWTH(23) = 0.96
      GROWTH(24) = 0.96
      GROWTH(25) = 0.96
!
      RGRW(EFP_D_VIN) = 1.0
      DO IY = EFP_D_VIN - 1 , 1 , -1
         RGRW(IY) = RGRW(IY+1) * GROWTH(NRGN)
      END DO
      TEMP = RGRW(1)
      DO IY = 1 , EFP_D_VIN
         RGRW(IY) = RGRW(IY) / TEMP
      END DO
!
!     CALL TO LOAD IN EB ARRAYS FOR PRODUCTION
!
      ICALL = 'G'
      CALL GETEB(NRGN,ICALL)
!
!     THE 3 MAJOR PARTS OF THIS ROUTINE:
!         1) CALCULATE BOOK VALUE, ASSET VALUE, AND ACCUM BOOK DEP
!         2) CALCULATE AMORTIZATION OF DEFERRED INVEST TAX CREDITS
!         3) CALCULATE AMORTIZATION OF DEFERRED TAX SAVINGS FROM DEBT AFUCD
!         4) SPLIT VARIOUS ACCOUNTS INTO GENERATION, TRANSMISSION, AND DISTRIBUTION
!
!     LOOP OVER OWNERSHIP CLASSES
!
      DO N=1,2
        IF (ESTSHR(N) .GT. 0.0) THEN

         TCOST = TRST(N) + TROB(N) + TRCS(N) + MAX(TRRE(N),0.0) + TRPS(N)
         IF (TCOST .GT. 0.0) THEN
            PRST = TRST(N) / TCOST
            PRLT = TROB(N) / TCOST
            PRCE = (TRCS(N) + MAX(TRRE(N),0.0)) / TCOST
            PRPS = TRPS(N) / TCOST
         ELSE
            PRST = 0.0
            PRLT = 0.0
            PRCE = 0.0
            PRPS = 0.0
         END IF

!        INITIALIZE EL ARRAYS
!
         IF (TCOST .GT. 0.0) THEN
            ESPRLT(N) = PRLT
            ESPRST(N) = PRST
            ESPRPS(N) = PRPS
            ESPRCE(N) = PRCE
         END IF
!
         ELPRLT(N) = ESPRLT(N)
         ELPRST(N) = ESPRST(N)
         ELPRPS(N) = ESPRPS(N)
         ELPRCE(N) = ESPRCE(N)
!
!        INITIALIZE EXISTING ASSET ACCOUNTS TO 0.0
!
         DO J=1,NPTYP
            DO IY=1,EOVYRS
               UOBKVL(IY,J,N)=0.0
               UOABDE(IY,J,N)=0.0
               TOABDE(IY,J,N)=0.0
               UOASVL(IY,J,N)=0.0
            END DO
            TMPBK(J) = 0.0
            TMPBD(J) = 0.0
         END DO
!
!        NOW DIVIDE UP THE VINTAGE CLASS INTO YEARS
!
         DO IY=1,EOVYRS
            TCOSTY(IY)=0.0
         END DO
         JY = EFPSYR - UESTYR + 1
         DO J=1,EIPROD
            IF (EOVBK(J,N) .NE. 0.0) THEN
               TCOST=0.0
               EOMW(EOVYRS,J,N,NRGN) = 0.0
               DO IY=1,EOVYRS
                  COST(IY) = 0.0
                  IF (IY .LT. EOBKLF(J,N)) THEN
                     COST(IY) = EOCOST(J,N) * EOVDEF(IY) * RGRW(IY)* EOMW(IY,J,N,NRGN)
                     TCOST = TCOST + COST(IY)
                  ELSE
                     KY = EOBKLF(J,N) - 1
                     COST(KY) = COST(KY) + EOCOST(J,N) * EOVDEF(IY) * RGRW(IY) * EOMW(IY,J,N,NRGN)
                     TCOST = TCOST + EOCOST(J,N) * EOVDEF(IY) * RGRW(IY) * EOMW(IY,J,N,NRGN)
                  ENDIF
               END DO
               IF (TCOST .EQ. 0.0) THEN
                  UOBKVL(1,J,N)=EOVBK(J,N)
                  UOABDE(1,J,N)=0.0
                  TOABDE(1,J,N) = 1.0 / EOBKLF(J,N) * EOVBK(J,N)
                  TCOSTY(1) = TCOSTY(1) + UOBKVL(1,J,N)
               ELSE
                  DO IY=1,EOVYRS
                     IF (TCOST .GT. 0.0) THEN
                        UOBKVL(IY,J,N) = COST(IY)/TCOST*EOVBK(J,N)
                     ELSE
                        UOBKVL(IY,J,N) = 0.0
                     END IF
                     IF (IY .GT. JY) THEN
                        UOABDE(IY,J,N) = REAL(IY - JY) / EOBKLF(J,N) * UOBKVL(IY,J,N)
                     ELSE
                        UOABDE(IY,J,N) = 0.0
                     END IF
                     TOABDE(IY,J,N) = REAL(IY) / EOBKLF(J,N) * UOBKVL(IY,J,N)
                     TCOSTY(IY) = TCOSTY(IY) + UOBKVL(IY,J,N)
                  END DO
               ENDIF
            ENDIF
         END DO
!
!        ALLOCATE NON-PRODUCTION BASED ON DISTRIBUTION OF PRODUCTION
!
         DO J = EIPROD + 1 , NPTYP
            KY = EOBKLF(J,N) - 1
            TCOST = TCOSTY(KY)
            PROFILE(KY) = 1.0
            HIGHEST = TCOSTY(KY)
            DO IY = KY - 1 , 1 , -1
               PROFILE(IY) = PROFILE(IY + 1) * GROWTH(NRGN)
               HIGHEST = MAX(HIGHEST , TCOSTY(IY))
               TCOST = TCOST + TCOSTY(IY)
            END DO
            TCOST = TCOST / REAL(KY)
            IF (HIGHEST .GT. 0.0 .AND. HIGHEST .GT. TCOST) THEN
               DCOST = 0.0
               DO IY = 1 , KY
                  PROFILE(IY) = PROFILE(IY) * EOVDEF(IY) * (1.0 - (TCOSTY(IY) / HIGHEST))
                  DCOST = DCOST + PROFILE(IY)
               END DO
            ELSE
               DCOST = 0.0
               DO IY = 1 , KY
                  PROFILE(IY) = PROFILE(IY) * EOVDEF(IY)
                  DCOST = DCOST + PROFILE(IY)
               END DO
            END IF
!
            DO IY = 1 , EOVYRS
               IF (IY .LE. KY) THEN
                  UOBKVL(IY,J,N)=PROFILE(IY) / DCOST * EOVBK(J,N)
               ELSE
                  UOBKVL(IY,J,N) = 0.0
               END IF
               IF (IY .GT. JY) THEN
                  UOABDE(IY,J,N) = REAL(IY - JY) / EOBKLF(J,N) * UOBKVL(IY,J,N)
               ELSE
                  UOABDE(IY,J,N) = 0.0
               END IF
               TOABDE(IY,J,N) = REAL(IY) / EOBKLF(J,N) * UOBKVL(IY,J,N)
            END DO
         END DO
         PBK = 0.0
         TBK = 0.0
         DBK = 0.0
         ABK = 0.0
         PBD95 = 0.0
         TBD95 = 0.0
         DBD95 = 0.0
         ABD95 = 0.0
         DO IY = 1 , EOVYRS
            DO J = 1 , NPTYP
               ABK = ABK + UOBKVL(IY,J,N)
               ABD95 = ABD95 + TOABDE(IY,J,N)
               TMPBK(J) = TMPBK(J) + UOBKVL(IY,J,N)
               TMPBD(J) = TMPBD(J) + TOABDE(IY,J,N)
               IF (J .EQ. EITRAN) THEN
                  TBK = TBK + UOBKVL(IY,J,N)
                  TBD95 = TBD95 + TOABDE(IY,J,N)
               ELSE IF (J .EQ. EIDIST) THEN
                  DBK = DBK + UOBKVL(IY,J,N)
                  DBD95 = DBD95 + TOABDE(IY,J,N)
               ELSE
                  PBK = PBK + UOBKVL(IY,J,N)
                  PBD95 = PBD95 + TOABDE(IY,J,N)
               END IF
            END DO
            write(22,9321) curiyr+1989,nrgn,efpsyr-iy+1,n,profile(iy),(uobkvl(iy,j,n),j=1,nptyp)
            write(22,9322) curiyr+1989,nrgn,efpsyr-iy+1,n,profile(iy),(toabde(iy,j,n),j=1,nptyp)
 9321       format(1x,"uobkvl",4(":",I4),15(":",F15.3))
 9322       format(1x,"toabde",4(":",I4),15(":",F15.3))
         END DO
         write(22,2319) CURIYR,NRGN,N,ABK,ABD95,PBK,PBD95,TBK,TBD95,DBK,DBD95
 2319    format(1x,'BKV_ABD95',3(":",I4),8(":",F9.0))
!
!        modify book depreciation to achieve actual efpsyr book depreciation
!
         do iy=1,eovyrs
            tcost=0.0
            dcost=0.0
            do j=1,nptyp
               tcost=tcost+uobkvl(iy,j,n)
               dcost=dcost+uoabde(iy,j,n)
            enddo
         enddo
!
!        CALCULATE TAX BASIS ON HISTORICAL PLANT BASED ON TBRAT
!
         IF (N .NE. 2) THEN
            DO J=1,NPTYP
               DO IY=1,EOVYRS
                  UOASVL(IY,J,N) = TBRAT(IY,J)*UOBKVL(IY,J,N)
               END DO
            END DO
         ENDIF
!
!        INITIALIZE TAX VARIABLES TO ZERO
!
         DO J = 1, NPTYP
            DO IY=1,EOVYRS
               UOAFDC(IY,J,N) = 0.0
               UOAITC(IY,J,N) = 0.0
            END DO
         END DO
!
!        IF TAX RATE IS ZERO, WE ARE ALL DONE
!
         IF (N .NE. 2) THEN
!
!           **** NOW ALLOCATE THE DEFERRED ITC
!           ***     COMPUTE ITC FOR 1) NEW ASSETS (**).
!           ***               2) THE REMAINDER IS FOR OLD ASSETS (**).
!           ***               UOAITC COMPUTED ONLY FOR EXISTING ASSETS.
!           ***               (ADDITIONAL COMMENTS BELOW).
!           *** CALCULATE ITC FOR NEW BUILDS.
!
            BDITC = 0.0
!
!           FOR EACH BUILD, ITC = COST TO DATE NET OF AFDC * ITCRATE * (1 - % FLOTHRU)
!
            DO NB = 1,EBNUM
               J = EBPTYP(NB)
               BDITC = BDITC + EBASVL(NB,N) * ESRITC(J) * (1.0 - ESFLPR(N))

!             write(6,9712) CURIRUN, CURIYR+1989, NRGN, N, NB, J, EBASVL(NB,N), ESRITC(J), ESFLPR(N), BDITC
!9712         FORMAT(1X,"BDITC_INFO",6(":",I4),4(":",F15.3))

            END DO
!
!           *** BEGIN DOING OLD ASSETS
!
            EDITC = TRDITC(N) - BDITC
            IF (EDITC .LE. 0 ) THEN
!
!              IF HERE ERROR
!
              write(22,9713) CURIRUN, CURIYR+1989, NRGN, N, ESFLPR(N), EDITC, TRDITC(N), BDITC
 9713         FORMAT(1X,"EDITC_ERROR",4(":",I4),4(":",F15.3))

            ELSE
!
!              ** CALCULATE AMORTIZATION FOR EXISTING VINTAGE ASSETS
!
               IY = 1
               DO WHILE (IY .LE. EOVYRS .AND. EDITC .GT. 0.0)
                  SUMITC = 0.0
                  DO J=1,NPTYP
                     TDITC = UOASVL(IY,J,N) * ESRITC(J) / EOBKLF(J,N)
                     UOAITC(IY,J,N) = TDITC
                     SUMITC = SUMITC + &
                        TDITC * DBLE(EOBKLF(J,N) - IY + 1)
                  END DO
                  EDITC = EDITC - SUMITC
                  IF (EDITC .LT. 0.0) THEN
                     IF (SUMITC .GT. 0.0) THEN
                        SCALE = 1.0 + EDITC / SUMITC
                     ELSE
                        SCALE = 0.0
                     END IF
                     DO J = 1 , NPTYP
                        UOAITC(IY,J,N) = UOAITC(IY,J,N) * SCALE
                     END DO
                  END IF
                  IY = IY + 1
               END DO
!
            ENDIF
!
!           ****  NOW DO AFDC  ****
!           *** CALCULATE AMORTIZATION OF DEFERRED AFDC.
!           CALCULATE EQUITY RELATED PORTION OF AFUDC COST
!
            AFDCE = ESPRCE(N) * RTCEAV(NRGN)/100.0 + ESPRPS(N) * ESEMPB(N)
!
!           CALCULATE PRE-TAX DEBT RELATED PORTION OF AFUDC COST
!
            AFDCD = ESPRLT(N) * ESEMDB(N) + ESPRST(N) * RTNBAV(NRGN)/100.0
!
!           RATIO OF DEBT COST TO TO TOTAL COST OF AFDC.
!           MUST ASSUME (DUE TO DATA LIMITATIONS THAT THIS APPLIES FOR HISTORICAL PERIOD ALSO.
!
            FRACTD = AFDCD / (AFDCD + AFDCE)
!
!           ** CALCULATE ANNUAL AMORTIZATION
!           ** = AFDC BOOKED * PROPORTION OF PRE-TAX AFDC COST FROM DEBT *
!           ** EFFECTIVE TAX RATE * PERCENT NORMALIZE / BOOK LIFE
!
            FRACD1 = FRACTD * TXRTYR(1) * (1.0 - ESFPDB(N))
            DO J=1,NPTYP
               DO IY=1,EOVYRS
                  UOAFDC(IY,J,N) = (UOBKVL(IY,J,N) - UOASVL(IY,J,N)) * FRACD1 / EOBKLF(J,N)
               END DO
            END DO
         ENDIF
!
!        RE-INITIALIZE ESPR** ARRAYS
!
         ESPRLT(N) = PRLT
         ESPRST(N) = PRST
         ESPRPS(N) = PRPS
         ESPRCE(N) = PRCE
        ELSE
         DO J=1,EIPROD
            DO IY=1,EOVYRS
               UOBKVL(IY,J,N) = 0.0
               UOASVL(IY,J,N) = 0.0
               UOABDE(IY,J,N) = 0.0
               UOAFDC(IY,J,N) = 0.0
               UOAITC(IY,J,N) = 0.0
            END DO
         END DO
         ESPRLT(N) = 0.25
         ESPRST(N) = 0.25
         ESPRPS(N) = 0.25
         ESPRCE(N) = 0.25
       END IF
      END DO
!
!     WRITE OUT REGIONAL DATA BASE
!
      CALL STRR(NRGN)
!
!     *** SPLIT UP THE TOTAL DEFERRED ITC
!
      DO N=1,2
        IF (ESTSHR(N) .GT. 0.0) THEN
         URDITC(N,1) = 0.0
         URDITC(N,2) = 0.0
         URDITC(N,3) = 0.0
         IF (N .NE. 2) THEN
            JY = EFPSYR - UESTYR + 1
            DO J=1,NPTYP
!
!              ASSIGN PLANT TYPE TO SECTOR
!
               ISEC = 1
               IF (J .EQ. EITRAN) ISEC = 2
               IF (J .EQ. EIDIST) ISEC = 3
               DO IY = JY + 1 , EOVYRS
                  URDITC(N,ISEC) = URDITC(N,ISEC) + UOAITC(IY,J,N) * (EOBKLF(J,N) - IY + JY + 1)
               END DO
            END DO
            TOTAL = URDITC(N,1) + URDITC(N,2) + URDITC(N,3)
         ENDIF
!
!        *** SPLIT UP THE DEFERRED INCOME TAXES
!
         IF (N .NE. 2) THEN
            DO ISEC=1,3
               PDT(ISEC) = 0.0
               PDT2(ISEC) = 0.0
            END DO
            DO J=1,NPTYP
!
!              ASSIGN PLANT TYPE TO SECTOR
!
               ISEC = 1
               IF (J .EQ. EITRAN) ISEC = 2
               IF (J .EQ. EIDIST) ISEC = 3
!
!              CALCULATE DEFERRED TAXES FOR EACH VINTAGE
!              1 - Units with start years > UHBSYR & <= EFPSYR
!
               JY = EFPSYR - UESTYR + 1
               DO IYR = 1 , JY
!
!                 CALCULATE STRAIGHT LINE TAX DEP
!
                  SL = EODEPR(J,N)*UOASVL(IYR,J,N)
!
!                 CALCULATE DEFERRED TAXES IN EACH YEAR OF LIFE
!
                  DO IYEAR = 1 , IYR
!
!                    FIRST CALCULATE ACCELERATED DEPRECIATION
!                    USE TRA86, IF VINTAGE YEAR IS 1-8 (1987-1995)
!                    CONDR(1,*) Is value for start year and then forward in time
!                    TXRTYR(1) Is tax rate in 1985; TXRTYR(2) is 1986 etc.
!
                     ACCEL = CONDR(IYEAR,ICONTP(J)) * UOASVL(IYR,J,N)
!                    IF (UOASVL(IYR,J,N) .GT. 0.0) THEN
!                       Label = "2CONDR"
!                       L = ICONTP(J)
!                       WRITE(6,2131) Label,CURIYR+UHBSYR,CURITR,EFPSYR-IYR+1,IYR,IYEAR,NRGN,N,ISEC,J,L,CONDR(IYEAR,L),UOASVL(IYR,J,N),ACCEL
!                    END IF
!
!                    CALCULATE DEFERRED TAXES
!
                     JYR = IYEAR - IYR + EFPSYR - 1985 + 1
                     JYR = MAX( JYR , 1 )
                     TR = TXRTYR(JYR)
                     PDT2(ISEC) = PDT2(ISEC) + TR * (ACCEL - SL) * (1 - ESFLPR(N))
                     Label = "1RPDTB"
!                    WRITE(6,2132) Label,CURIYR+UHBSYR,CURITR,IYR,IYEAR,JYR,NRGN,N,ISEC,J,SL,ACCEL,TR,ESFLPR(N),PDT2(ISEC)
                  END DO
               END DO
!
!              2 - UHBSYR + 1 to EFPSYR Deffered taxes for
!                  Units with start years <= UHBSYR
!
               DO IYR = JY + 1 , EOVYRS
!
!                 CALCULATE STRAIGHT LINE TAX DEP
!
                  SL = EODEPR(J,N)*UOASVL(IYR,J,N)
!
!                 CALCULATE DEFERRED TAXES IN EACH YEAR OF LIFE
!
                  DO IYEAR = IYR - JY + 1 , IYR
!
!                    FIRST CALCULATE ACCELERATED DEPRECIATION
!                    USE TRA86, IF VINTAGE YEAR IS 1-8 (1987-1995)
!
                     IF (IYR .LE. EFPSYR - 1987 + 1) THEN
                        ACCEL = CONDR(IYEAR,ICONTP(J)) * UOASVL(IYR,J,N)
!                       IF (UOASVL(IYR,J,N) .GT. 0.0) THEN
!                          Label = "2CONDR"
!                          L = ICONTP(J)
!                          WRITE(6,2131) Label,CURIYR+UHBSYR,CURITR,EFPSYR-IYR+1,IYR,IYEAR,NRGN,N,ISEC,J,L,CONDR(IYEAR,L),UOASVL(IYR,J,N),ACCEL
!                       END IF
!
                     ELSE IF (IYR .LE. EFPSYR - 1981 + 1) THEN
!
!                       USE ERTA, MODIFIED BY TEFRA,
!                       IF VINTAGE YEAR IS 9-14 (1981-1986)
!
                        L = ESTXRC(J,N)
                        IF (IYEAR .LE. 15) THEN
                           ACCEL = ESTXRS(L,IYEAR) * (0.95 * UOASVL(IYR,J,N))
                        ELSE
                           ACCEL = 0.0
                        END IF
!                       IF (UOASVL(IYR,J,N) .GT. 0.0) THEN
!                          Label = "2ERTA_"
!                          WRITE(6,2131) Label,CURIYR+UHBSYR,CURITR,EFPSYR-IYR+1,IYR,IYEAR,NRGN,N,ISEC,J,L,ESTXRS(L,IYEAR),UOASVL(IYR,J,N),ACCEL
!                       END IF
!
                     ELSE
!
!                       USE SYD IF VINTAGE YEAR IS GREATER THAN 9 (BEFORE 1981)
!
                        TEMP1 = (EOTXLF(J,N) - (IYEAR - 1))
                        TEMP1 = MAX( TEMP1 , 0.0 )
                        TEMP2 = EOTXLF(J,N) * (EOTXLF(J,N) + 1) * 0.5
                        SYDTDR = TEMP1 / TEMP2
                        SYDTDR = AMAX1( SYDTDR , 0.0 )
                        ACCEL = SYDTDR * UOASVL(IYR,J,N)
!                       IF (UOASVL(IYR,J,N) .GT. 0.0) THEN
!                          Label = "2SYD__"
!                          L = EOTXLF(J,N)
!                          WRITE(6,2131) Label,CURIYR+UHBSYR,CURITR,EFPSYR-IYR+1,IYR,IYEAR,NRGN,N,ISEC,J,L,SYDTDR,UOASVL(IYR,J,N),ACCEL
!                       END IF
                     END IF
!
!                    CALCULATE DEFERRED TAXES
!
                     JYR = IYEAR - 2 * IYR + JY + EFPSYR - 1985 + 1
                     JYR = MAX( JYR , 1 )
                     TR = TXRTYR(JYR)
                     PDT2(ISEC) = PDT2(ISEC) + TR * (ACCEL - SL) * (1 - ESFLPR(N))
                     Label = "2RPDTB"
!                    WRITE(6,2132) Label,CURIYR+UHBSYR,CURITR,IYR,IYEAR,JYR,NRGN,N,ISEC,J,SL,ACCEL,TR,ESFLPR(N),PDT2(ISEC)
                  END DO
!
!
!                 3 - Start Year to  UHBSYR Deffered taxes for
!                     Units with start years <= UHBSYR
!
                  DO IYEAR = 1 , IYR - JY + 1
!
!                    FIRST CALCULATE ACCELERATED DEPRECIATION
!                    USE TRA86, IF VINTAGE YEAR IS 1-8 (1987-1995)
!
                     IF (IYR .LE. EFPSYR - 1987 + 1) THEN
                        ACCEL = CONDR(IYEAR,ICONTP(J)) * UOASVL(IYR,J,N)
!                       IF (UOASVL(IYR,J,N) .GT. 0.0) THEN
!                          Label = "3CONDR"
!                          L = ICONTP(J)
!                          WRITE(6,2131) Label,CURIYR+UHBSYR,CURITR,EFPSYR-IYR+1,IYR,IYEAR,NRGN,N,ISEC,J,L,CONDR(IYEAR,L),UOASVL(IYR,J,N),ACCEL
!2131                      FORMAT(1x,A6,10(":",I4),4(":",F11.3))
!                       END IF
!
!                       USE ERTA, MODIFIED BY TEFRA,
!                       IF VINTAGE YEAR IS 9-14 (1981-1986)
!
                     ELSE IF (IYR .LE. EFPSYR - 1981 + 1) THEN
                        L = ESTXRC(J,N)
                        IF (IYEAR .LE. 15) THEN
                           ACCEL = ESTXRS(L,IYEAR) * (0.95 * UOASVL(IYR,J,N))
                        ELSE
                           ACCEL = 0.0
                        END IF
!                       ACCEL = ESTXRS(L,IYEAR) * (0.95 * UOASVL(IYR,J,N))
!                       IF (UOASVL(IYR,J,N) .GT. 0.0) THEN
!                          Label = "3ERTA_"
!                          WRITE(6,2131) Label,CURIYR+UHBSYR,CURITR,EFPSYR-IYR+1,IYR,IYEAR,NRGN,N,ISEC,J,L,ESTXRS(L,IYEAR),UOASVL(IYR,J,N),ACCEL
!                       END IF
                     ELSE
!
!                       USE SYD IF VINTAGE YEAR IS GREATER THAN 9 (BEFORE 1981)
!
                        TEMP1 = (EOTXLF(J,N) - (IYEAR - 1))
                        TEMP2 = EOTXLF(J,N)*(EOTXLF(J,N) + 1) * 0.5
                        SYDTDR = TEMP1/TEMP2
                        SYDTDR = AMAX1(SYDTDR,0.0)
                        ACCEL = SYDTDR*UOASVL(IYR,J,N)
!                       IF (UOASVL(IYR,J,N) .GT. 0.0) THEN
!                          Label = "3SYD__"
!                          L = EOTXLF(J,N)
!                          WRITE(6,2131) Label,CURIYR+UHBSYR,CURITR,EFPSYR-IYR+1,IYR,IYEAR,NRGN,N,ISEC,J,L,SYDTDR,UOASVL(IYR,J,N),ACCEL
!                       END IF
                     ENDIF
!
!                    CALCULATE DEFERRED TAXES
!
                     JYR = IYEAR - IYR + EFPSYR - 1985 + 1
                     JYR = MAX( JYR , 1 )
                     TR = TXRTYR(JYR)
                     PDT(ISEC) = PDT(ISEC) + TR * (ACCEL - SL) * (1 - ESFLPR(N))
                     Label = "3RPDTB"
!                    WRITE(6,2132) Label,CURIYR+UHBSYR,CURITR,IYR,IYEAR,JYR,NRGN,N,ISEC,J,SL,ACCEL,TR,ESFLPR(N),PDT(ISEC)
 2132                FORMAT(1x,A6,9(":",I4),5(":",F11.3))
                  END DO
               END DO
            END DO
!
            URPDTB(N,1) = PDT(1) * (65.0 / 42.7)
            URPDTB(N,2) = PDT(2) * (65.0 / 42.7)
            URPDTB(N,3) = PDT(3) * (65.0 / 42.7)
!
            TOTAL = 0.0
            TOTAL2 = 0.0
            DO ISEC=1,3
               TOTAL = TOTAL + PDT(ISEC)
               TOTAL2 = TOTAL2 + PDT2(ISEC)
            END DO
!
!           TOTAL2 = TRPRDF(N) - TOTAL2
!           IF (TOTAL .GT. 0.0 .AND. TOTAL2 .GT. 0.0) THEN
!              URPDTB(N,1) = TOTAL2 * PDT(1)/TOTAL
!              URPDTB(N,2) = TOTAL2 * PDT(2)/TOTAL
!              URPDTB(N,3) = TOTAL2 * PDT(3)/TOTAL
!           ELSE
!              URPDTB(N,1) = 0.0
!              URPDTB(N,2) = 0.0
!              URPDTB(N,3) = 0.0
!           END IF
!
!           WRITE(*,8451) CURIYR+UHBSYR,NRGN,N,TOTAL,TRPRDF(N),TOTAL2,(PDT(ISEC),URPDTB(N,ISEC),PDT2(ISEC),ISEC=1,3)
 8451       FORMAT(1X,'URPDTB',3(":",I4),12(":",F9.2))
         ENDIF
!
!        *** SPLIT UP THE ACCUMULATED DEPRECIATION AND
!        *** CALCULATE PLANT IN SERVICE
!
         DO ISEC=1,3
            PIS(ISEC) = 0.0
            ABDE(ISEC) = 0.0
         END DO
         JY = EFPSYR - UESTYR + 1
         DO J=1,NPTYP
            ISEC = 1
            IF (J .EQ. EITRAN) ISEC = 2
            IF (J .EQ. EIDIST) ISEC = 3
            DO IYR = JY+1, EOVYRS
               ABDE(ISEC) = ABDE(ISEC) + UOABDE(IYR,J,N)
               PIS(ISEC) = PIS(ISEC) + UOBKVL(IYR,J,N)
            END DO
         END DO
         TOTAL = 0.0
         DO ISEC=1,3
            TOTAL = TOTAL + ABDE(ISEC)
         END DO
         URABDE(N,1) = ABDE(1)
         URABDE(N,2) = ABDE(2)
         URABDE(N,3) = ABDE(3)
!
         URAMDL(N,1) = PIS(1) - URABDE(N,1) - URPDTB(N,1) - URDITC(N,1)
         URAMDL(N,2) = PIS(2) - URABDE(N,2) - URPDTB(N,2) - URDITC(N,2)
         URAMDL(N,3) = PIS(3) - URABDE(N,3) - URPDTB(N,3) - URDITC(N,3)
         TOTAL = URAMDL(N,1) + URAMDL(N,2) + URAMDL(N,3)
!
         URRBL(N,1) = URAMDL(N,1)
         URRBL(N,2) = URAMDL(N,2)
         URRBL(N,3) = URAMDL(N,3)
!
!        *** DO LONG TERM DEBT INITIALIZATION
!
         UROBL(N,1) = URAMDL(N,1) * ESPRLT(N)
         UROBL(N,2) = URAMDL(N,2) * ESPRLT(N)
         UROBL(N,3) = URAMDL(N,3) * ESPRLT(N)
         TOTAL = UROBL(N,1) + UROBL(N,2) + UROBL(N,3)
!
!        *** DO PREFERRED STOCK INITIALIZATION
!
         URPFL(N,1) = URAMDL(N,1) * ESPRPS(N)
         URPFL(N,2) = URAMDL(N,2) * ESPRPS(N)
         URPFL(N,3) = URAMDL(N,3) * ESPRPS(N)
         TOTAL = URPFL(N,1) + URPFL(N,2) + URPFL(N,3)
!
!        SET UP LAST YEARS REVENUE (HARD WIRE FOR NOW)
!
         URRVRL(N,1) = TRRVRQ(N)*0.8
         URRVRL(N,2) = TRRVRQ(N)*0.1
         URRVRL(N,3) = TRRVRQ(N)*0.1
!
       ELSE
         ERDITC(N) = 0.0
         ERPDTB(N) = 0.0
         EROBL(N) = 0.0
         ERBNDL(N) = 0.0
         ERAMDL(N) = 0.0
         ERRBL(N) = 0.0
         ERPFLPS(N) = 0.0
         ERPRFL(N) = 0.0
         ERRVR1(N) = 0.0
         ERABDL(N) = 0.0
         ERPDFL(N) = 0.0
       END IF
      END DO
!
!     WRITE OUT REGIONAL/COMPONENT DATA BASE
!
      DO ICOM=1,3
         DO N=1,2
            ERDITC(N) = URDITC(N,ICOM)
            ERPDTB(N) = URPDTB(N,ICOM)
            EROBL(N) = UROBL(N,ICOM)
            ERBNDL(N) = UROBL(N,ICOM)
            ERAMDL(N) = URAMDL(N,ICOM)
            ERRBL(N) = URRBL(N,ICOM)
            ERPFLPS(N) = URPFL(N,ICOM)
            ERPRFL(N) = URPFL(N,ICOM)
            ERRVR1(N) = URRVRL(N,ICOM)
            ERABDL(N) = URABDE(N,ICOM)
            ERPDFL(N) = URPDTB(N,ICOM)
         END DO
         IF (ICOM .EQ. 1) ICALL='G'
         IF (ICOM .EQ. 2) ICALL='T'
         IF (ICOM .EQ. 3) ICALL='D'
         CALL STRRC(NRGN,ICALL,1)
      END DO
      RETURN
      END
!
      SUBROUTINE STRRCY(NRGN,ICALL,IY)
      IMPLICIT NONE
!********************************************************************
!  THIS SUBROUTINE STORES A CALENDAR-YEAR RECORD TO THE EFPRCY DIRECT ACCESS FILE
!********************************************************************
      include 'parametr'
      include 'emmparm'
      include 'efprcy'
      include 'control'
      include 'efpgen'
      include 'ncntrl'
      include 'uefdout'
      INTEGER NRGN
      INTEGER IY
      CHARACTER*1 ICALL
      INTEGER IRECL
      INTEGER ICOM
      IF (ICALL .EQ. 'G') THEN
         ICOM = 1
      ELSE
         IF (ICALL .EQ. 'T') THEN
            ICOM = 2
         ELSE
            ICOM = 3
         ENDIF
      ENDIF
! DETERMINE RECORD NUMBER
      IRECL = ICOM + (NRGN - 1)*3 + (IY - 1)*IJUMPEMRGN*3
      WRITE(UF_RCY,REC=IRECL) OUTRCY
      RETURN
      END

      SUBROUTINE STRRC(NRGN,ICALL,IDATA)
      IMPLICIT NONE
!********************************************************************
!  THIS SUBROUTINE STORES A RECORD TO THE EFPRC DIRECT ACCESS FILE
!********************************************************************
      include 'parametr'
      include 'emmparm'
      include 'efprc'
      include 'control'
      include 'efpgen'
      include 'uefdout'
      INTEGER NRGN
      CHARACTER*1 ICALL
      INTEGER IRECL
      INTEGER ICOM
      INTEGER IDATA
      IF (ICALL .EQ. 'G') THEN
         ICOM = 1
      ELSE
         IF (ICALL .EQ. 'T') THEN
            ICOM = 2
         ELSE
            ICOM = 3
         ENDIF
      ENDIF
! DETERMINE RECORD NUMBER
      IRECL = IDATA + (ICOM - 1)*2 + (NRGN - 1)*6
      WRITE(UF_RC,REC=IRECL) OUTRC
      RETURN
      END

      SUBROUTINE STRR(NRGN)
      IMPLICIT NONE
!********************************************************************
!  THIS SUBROUTINE STORES A RECORD TO THE EFPR DIRECT ACCESS FILE
!********************************************************************
      include 'parametr'
      include 'emmparm'
      include 'efpr'
      include 'efpgen'
      include 'uefdout'
      INTEGER NRGN
      INTEGER IRECL,JRECL,I
!
!     DETERMINE RECORD NUMBER
!
      IRECL = EBLK$EFP * (NRGN - 1)
!
      DO I = 1 , EBLK$EFP
         JRECL = IRECL + I
         WRITE(UF_R,REC=JRECL) OUTR(I)
      END DO
!
      RETURN
      END

      SUBROUTINE GETRCY(NRGN,ICALL,IY)
      IMPLICIT NONE
!********************************************************************
!  THIS SUBROUTINE GETS A CALENDAR-YEAR RECORD FROM THE EFPRCY DIRECT ACCESS FILE
!********************************************************************
      include 'parametr'
      include 'emmparm'
      include 'efprcy'
      include 'control'
      include 'efpgen'
      include 'ncntrl'
      include 'uefdout'
      INTEGER NRGN,IY
      CHARACTER*1 ICALL
      INTEGER IRECL
      INTEGER ICOM
      IF (ICALL .EQ. 'G') THEN
         ICOM = 1
      ELSE
         IF (ICALL .EQ. 'T') THEN
            ICOM = 2
         ELSE
            ICOM = 3
         ENDIF
      ENDIF
! DETERMINE RECORD NUMBER
      IRECL = ICOM + (NRGN - 1)*3 + (IY - 1)*IJUMPEMRGN*3
      READ(UF_RCY,REC=IRECL) OUTRCY
      RETURN
      END
      SUBROUTINE GETRC(NRGN,ICALL,IDATA)
      IMPLICIT NONE
!********************************************************************
!  THIS SUBROUTINE GETS A RECORD FROM THE EFPRC DIRECT ACCESS FILE
!********************************************************************
      include 'parametr'
      include 'emmparm'
      include 'efprc'
      include 'control'
      include 'efpgen'
      include 'uefdout'
      INTEGER NRGN
      CHARACTER*1 ICALL
      INTEGER IRECL
      INTEGER ICOM
      INTEGER IDATA
      IF (ICALL .EQ. 'G') THEN
         ICOM = 1
      ELSE
         IF (ICALL .EQ. 'T') THEN
            ICOM = 2
         ELSE
            ICOM = 3
         ENDIF
      ENDIF
! DETERMINE RECORD NUMBER
      IRECL = IDATA + (ICOM - 1)*2 + (NRGN - 1)*6
      READ(UF_RC,REC=IRECL) OUTRC
      RETURN
      END
      SUBROUTINE GETR(NRGN)
      IMPLICIT NONE
!********************************************************************
!  THIS SUBROUTINE GETS A RECORD FROM THE EFPR DIRECT ACCESS FILE
!********************************************************************
      include 'parametr'
      include 'emmparm'
      include 'efpr'
      include 'efpgen'
      include 'uefdout'
      INTEGER NRGN
      INTEGER IRECL,JRECL,I
!
!     DETERMINE RECORD NUMBER
!
      IRECL = EBLK$EFP * (NRGN - 1)
!
      DO I = 1 , EBLK$EFP
         JRECL = IRECL + I
         READ(UF_R,REC=JRECL) OUTR(I)
      END DO
!
      RETURN
      END
!
      SUBROUTINE STRREP(N,NRGN,IY,IOC)
      IMPLICIT NONE
!********************************************************************
!  THIS SUBROUTINE STORES A RECORD IN THE EFPSTMT DIRECT ACCESS FILE
!********************************************************************
      include 'parametr'
      include 'emmparm'
      include 'efprep'
      include 'control'
      include 'efpgen'
      include 'ncntrl'
      include 'uefdout'
      INTEGER N
      INTEGER IY
      INTEGER IOC
      INTEGER NRGN
      INTEGER IRECL
! DETERMINE RECORD NUMBER
!     IF ((N .EQ. 1) .AND. (NRGN .EQ. 1)) THEN
!        IF ((IY .EQ. 1) .AND. (IOC .EQ. 1)) THEN
!           WRITE(6,*) 'HELLO5',CAPREQ(1,8)
!        ENDIF
!     ENDIF
      IRECL = N + (IOC - 1)*3 + (NRGN - 1)*3*4 + &
              (IY - 1)*3*4*IJUMPEMRGN
      WRITE(UF_STMT,REC=IRECL) OUTREP
      RETURN
      END
      SUBROUTINE GETREP(N,NRGN,IY,IOC)
      IMPLICIT NONE
!********************************************************************
!  THIS SUBROUTINE GETS A RECORD FROM THE EFPSTMT DIRECT ACCESS FILE
!********************************************************************
      include 'parametr'
      include 'emmparm'
      include 'efprep'
      include 'control'
      include 'efpgen'
      include 'ncntrl'
      include 'uefdout'
      INTEGER N
      INTEGER IY
      INTEGER IOC
      INTEGER NRGN
      INTEGER IRECL
      INTEGER I,L ! local
! DETERMINE RECORD NUMBER
      IRECL = N + (IOC - 1)*3 + (NRGN - 1)*3*4 + &
              (IY - 1)*3*4*IJUMPEMRGN

      READ(UF_STMT,REC=IRECL,ERR=99) OUTREP
      RETURN
99    CONTINUE
      L=LEN(OUTREP)
      DO i=1,l
       outrep(I:I)=char(0)  ! acscii 0 will initialize Real as 0
      ENDDO
      WRITE(6,*) 'Warning, record missing from EFPSTMT:', &
       irecl
      WRITE(6,*) 'ownership type n,nrgn,iy,', &
       'cap type (gtd,tot) ioc=', &
       n,nrgn,iy,ioc
!     IF ((N .EQ. 1) .AND. (NRGN .EQ. 1)) THEN
!        IF ((IY .EQ. 1) .AND. (IOC .EQ. 1)) THEN
!           WRITE(6,*) 'HELLO6',CAPREQ(1,8)
!        ENDIF
!     ENDIF

      RETURN
      END
      SUBROUTINE STRRP2(NRGN,IY)
      IMPLICIT NONE
!********************************************************************
!  THIS SUBROUTINE STORES A RECORD IN THE EFPSTM2 DIRECT ACCESS FILE
!********************************************************************
      include 'parametr'
      include 'emmparm'
      include 'efprp2'
      include 'control'
      include 'efpgen'
      include 'ncntrl'
      include 'uefdout'
      INTEGER IY
      INTEGER NRGN
      INTEGER IRECL
! DETERMINE RECORD NUMBER
      IRECL = NRGN + (IY - 1)*(MNUMNR-3)
      WRITE(UF_STM2,REC=IRECL) OUTRP2

      RETURN
      END
      SUBROUTINE GETRP2(NRGN,IY)
      IMPLICIT NONE
!********************************************************************
!  THIS SUBROUTINE GETS A RECORD FROM THE EFPSTM2 DIRECT ACCESS FILE
!********************************************************************
      include 'parametr'
      include 'emmparm'
      include 'efprp2'
      include 'control'
      include 'efpgen'
      include 'ncntrl'
      include 'uefdout'
      INTEGER IY,I,L
      INTEGER NRGN
      INTEGER IRECL
! DETERMINE RECORD NUMBER
      IRECL = NRGN + (IY - 1)*(MNUMNR-3)
      READ(UF_STM2,REC=IRECL,ERR=99) OUTRP2
      RETURN
99    CONTINUE
      L=LEN(OUTRP2)
      DO I=1,L
       OUTRP2(I:I)=CHAR(0)  ! ascii 0 will initialize Real as 0
      ENDDO
      WRITE(6,*) 'Warning, record missing from EFPSTM2:', &
       IRECL
      WRITE(6,*) 'nrgn,iy,', &
       NRGN,IY

      RETURN
      END
!
!
      SUBROUTINE EFP(NRGN)
      IMPLICIT NONE
!******************************************************************
!     THIS SUBROUTINE IS THE MAIN ROUTINE FOR CONTROL OVER THE THREE OPERATING COMPONENTS OF EFP
!     ALL VARIABLE INITIALIZATION IS PERFORMED PRIOR TO CALLING THE ROUTINES
!     RESPONSIBLE FOR EACH OPERATING COMPONENT.  THUS THE VARIABLES ARE
!     MANAGED IN THIS ROUTINE, AND EACH OPERATING COMPONENT IS CALLED
!     WITH THE PROPER SET UP . IN ADDITION INFORMATION FLOWING OUT OF
!     THE COMPUTATIONS PERFORMED FOR EACH OPERATING COMPONENT ARE ALSO
!     MANAGED BEFORE PROCEEDING TO THE NEXT OPERATING COMPONENT.
!*******************************************************************
! INPUT VARIABLES:
!     NRGN = CURRENT REGION
! INTERNAL VARIABLES:
!     ICALL = OPERATING COMPONENT IDENTIFICATION
!*******************************************************************
      CHARACTER*1 ICALL
      INTEGER NRGN
! GENERATION COMPONENT
      ICALL = 'G'
      CALL ELGLTR(NRGN,ICALL)
      CALL GL(NRGN,ICALL)
      CALL GENERA(NRGN,ICALL)
      CALL ELLGTR(NRGN,ICALL)
! TRANSMISSION COMPONENT
      ICALL = 'T'
      CALL ELGLTR(NRGN,ICALL)
      CALL GL(NRGN,ICALL)
      CALL TRANSM(NRGN,ICALL)
      CALL ELLGTR(NRGN,ICALL)
! DISTRIBUTION COMPONENT
      ICALL = 'D'
      CALL ELGLTR(NRGN,ICALL)
      CALL GL(NRGN,ICALL)
      CALL DISTRI(NRGN,ICALL)
      CALL ELLGTR(NRGN,ICALL)
      RETURN
      END
      SUBROUTINE ALLFAC(NRGN)
      IMPLICIT NONE
!*************************************************************
! THIS SUBROUTINE CALCULATES THE ALLOCATION FACTORS FOR EACH OF THE PRE-DEFINED ALLOCATION TECHNIQUES
!   THE ALLOCATION FACTORS ARE A FUNCTION OF THE DEMAND CHARACTERISTICS OF EACH CLASS
!   THE ALLOCATION TECHNIQUES ARE:
!    1 = SALES
!    2 = MARGINAL FUEL
!    3 = EMBEDDED FUEL
!    4 = CUSTOMER
!    5 = CP
!    6 = PCP
!    7 = NCP
!    8 = AED - CP
!    9 = AED - PCP
!*************************************************************
! INPUT VARIABLES:
!     NCLASS = NUMBER OF CUSTOMER CLASSES
!     SALCLS = SALES BY CUSTOMER CLASS
!     FUELMR = MARGINAL FUEL BY CUSTOMER CLASS
!     FUELEM = EMBEDDED FUEL BY CUSTOMER CLASS
!     CUST = NUMBER OF CUSTOMERS IN EACH CUSTOMER CLASS
!     CP = COINCIDENT PEAK BY CUSTOMER CLASS
!     PCP = PROBABILIY OF COINCIDENT PEAK BY CUSTOMER CLASS
!     NCP = NON-COINCIDENT PEAK BY CUSTOMER CLASS
!     SYSLF = SYSTEM LOAD FACTOR
! INTERNAL VARIABLES:
!     TOTAL = TEMPORARY VARIABLE FOR COMPUTING VARIOUS SUM TOTALS
! OUTPUT VARIABLES:
!     DEMFAC = FACTORS FOR SECTOR SPLITS BY ALLOCATION TECHNIQUE
!*************************************************************
      include 'parametr'
      include 'emmparm'
      include 'dsmdimen'
      include 'dsmnercr'
      include 'efpgen'
      include 'eusprc'
      include 'efpint'
      include 'ldsm'
      include 'control'
      include 'dispett'
      include 'efprp2'
      include 'ncntrl'
      include 'elshrs'
      include 'efprcy'
      include 'qblk'
      include 'dsmtfefp'
      include 'dsmhelm'
      REAL*4 TOTAL
      REAL*4 TDLOSS
      INTEGER ICLS
      INTEGER NRGN
      REAL SALTOTOLD,SALTOTLAST
      COMMON /SALEOLD/ SALTOTOLD,SALTOTLAST
      SALTOT = 0.0
      SALTOTLAST = 0.0
      IF (CURITR .eq. 1) THEN
      DO ICLS=1,NCLASS
        SALTOTLAST = SALTOTLAST + SALCLS(NRGN,ICLS)
      ENDDO
      ENDIF

      DO ICLS=1,NCLASS
! CONVERT SALES FROM 10^6 TO 10^9 KWH AND SUM FOR TOTAL REGION
         SALCLS(NRGN,ICLS) = TOTSECLOAD(NRGN,ICLS)/1000.0
         SALTOT = SALTOT + SALCLS(NRGN,ICLS)
      END DO
      DO ICLS=1,NCLASS
         DEMFAC(1,ICLS) = SALCLS(NRGN,ICLS)/SALTOT
      END DO

          SALTOTOLD=SALTOT



! SPLIT OUT SALES BY OWNERSHIP
      EQTLSL(1) = SALTOT*ESTSHR(1)
      EQTLSL(2) = SALTOT*ESTSHR(2)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!      WRITE(*,10)ESTSHR(1),ESTSHR(2),SALTOT,EQTLSL(1),EQTLSL(2),NRGN
!10    FORMAT(' ALLFAC - ESTSHR(1&2):',F8.4,F8.4,' SALTOT:',F8.1,
!     1       ' EQTLSL(1&2):',F8.1,F8.1,' NRGN:',I3)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! DO MARGINAL FUEL
      TOTAL = 0.0
      DO ICLS=1,NCLASS
         TOTAL = TOTAL + FUELMR(ICLS)
      END DO
      DO ICLS=1,NCLASS
         DEMFAC(2,ICLS) = FUELMR(ICLS)/TOTAL
      END DO
! DO EMBEDDED FUEL
      TOTAL = 0.0
      DO ICLS=1,NCLASS
         TOTAL = TOTAL + FUELEM(ICLS)
      END DO
      DO ICLS=1,NCLASS
         DEMFAC(3,ICLS) = FUELEM(ICLS)/TOTAL
      END DO
! DO CUSTOMERS
      TOTAL = 0.0
      DO ICLS=1,NCLASS
         TOTAL = TOTAL + CUST(ICLS)
      END DO
      DO ICLS=1,NCLASS
         DEMFAC(4,ICLS) = CUST(ICLS)/TOTAL
      END DO
! DO CP
      TOTAL = 0.0
      DO ICLS=1,NCLASS
         TOTAL = TOTAL + SECANNUALPEAK(NRGN,ICLS,1)
         CP(ICLS) = SECANNUALPEAK(NRGN,ICLS,1)
      END DO
      CPNEMS(NRGN) = TOTAL
!     write(*,'(a,2i5,2f12.2)')"in efp, CPeak ",curiyr,NRGN,TOTAL,CPNEMS(NRGN)
      DO ICLS=1,NCLASS
         DEMFAC(5,ICLS) = SECANNUALPEAK(NRGN,ICLS,1)/TOTAL
      END DO
! DO PCP
      TOTAL = 0.0
      DO ICLS=1,NCLASS
         TOTAL = TOTAL + SECANNPEAAVPCP(NRGN,ICLS)
         PCP(ICLS) = SECANNPEAAVPCP(NRGN,ICLS)
      END DO
      DO ICLS=1,NCLASS
         DEMFAC(6,ICLS) = SECANNPEAAVPCP(NRGN,ICLS)/TOTAL
      END DO
! DO NCP
      TOTAL = 0.0
      DO ICLS=1,NCLASS
         TOTAL = TOTAL + SECANNUALPEAK(NRGN,ICLS,2)
         NCP(ICLS) = SECANNUALPEAK(NRGN,ICLS,2)
      END DO
      NCPNEMS(NRGN) = TOTAL
!     write(*,'(a,2i5,2f12.2)')"in efp,NCPeak ",curiyr,NRGN,TOTAL,NCPNEMS(NRGN)
      DO ICLS=1,NCLASS
         DEMFAC(7,ICLS) = SECANNUALPEAK(NRGN,ICLS,2)/TOTAL
      END DO
! DO AED FOR BOTH CP AND PCP
      DO ICLS=1,NCLASS
         SYSLF = SYSTEMLF(NRGN)
         DEMFAC(8,ICLS) = SYSTEMLF(NRGN)*DEMFAC(1,ICLS) + &
                          (1.0 - SYSTEMLF(NRGN))*DEMFAC(5,ICLS)
         DEMFAC(9,ICLS) = SYSTEMLF(NRGN)*DEMFAC(1,ICLS) + &
                          (1.0 - SYSTEMLF(NRGN))*DEMFAC(6,ICLS)
      END DO
      RETURN
      END
      SUBROUTINE CNV1CRNR(INERC,CENSUS,SHARES,NERC)
      IMPLICIT NONE
!*************************************************************
! THIS SUBROUTINE CONVERTS FROM CENSUS DIVISIONS TO EMM (NERC BASED) REGIONS
!*************************************************************
      include 'parametr'
      include 'emmparm'
      include 'control'
      include 'ncntrl'
      include 'uefdout'
      INTEGER ICENSUS
      INTEGER INERC
      INTEGER I
      REAL*4 NERC
      REAL*4 TOT
      REAL*4 CENSUS(MNUMCR)
      REAL*8 SHARES(MNUMNR,MNUMCR)
! CONVERT FROM CENSUS TO NERC DATA
      NERC = 0.0
      DO ICENSUS=1,MNUMCR-1
! COMPUTE SHARE TOTALS FOR NORMALIZING
         TOT = 0.0
         DO I=1,UNRGNS
            TOT = TOT + SHARES(I,ICENSUS)
         ENDDO
         IF (SHARES(MNUMNR,ICENSUS) .GT. 0.000001) THEN
            NERC = NERC + CENSUS(ICENSUS) * &
                   (SHARES(INERC,ICENSUS)/TOT)
         ENDIF
      END DO
      RETURN
      END
      SUBROUTINE ELGLTR(NRGN,ICALL)
      IMPLICIT NONE
!*******************************************************************
!         THIS SUBROUTINE TRANSFERS GLOBAL TO LOCAL VARIABLES
!*******************************************************************
      include 'phasin'
      include 'salelb'
      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'control'
      include 'efpgen'
      include 'efpr'
      include 'efptemp'
      include 'eusprc'
      include 'efpint'
      include 'efprc'
      include 'efprcy'
      CHARACTER*1 ICALL
      INTEGER I
      INTEGER II
      INTEGER III
      INTEGER NRGN
      INTEGER IYR
      INTEGER N
      INTEGER ICOM,FULLYR
! GET YEARLY INPUTS
      FULLYR = USYEAR(CURIYR)
      CALL GETRCY(NRGN,ICALL,CURIYR)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       DO N=1,2
        CALL CAPCOST(NRGN,ICALL,N)
!         IF ((CURITR .EQ. 1) .AND. (NRGN .EQ. 9)) THEN
!        WRITE(*,200)ESRTST(N),ESRTLT(N),ESRTCE(N),ESRTPS(N),
!     1             NRGN,ICALL,N,CURIYR
!200     FORMAT(' ELGLTR2 - ESRTST:',F6.4,
!     1        ' ESRTLT:',F6.4,' ESRTCE:',F6.4,
!     2        ' ESRTPS:',F6.4,' NRGN,ICALL,N,CURIYR:',I3,A3,I3,I3)
!         ENDIF
       END DO
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! READ IN DEMAND FACTORS FROM THE LDSM - ONLY DO THIS FOR GEN,
! SINCE IT IS SAME FOR ALL COMPONENTS
      IF (ICALL .EQ. 'G') CALL ALLFAC(NRGN)
! IN FIRST ITERATION DO CAPITAL MODELING
      IF (CURITR .EQ. 1) THEN
! CREATE A NEW BUILD TO RECORD THIS YEARS CAPITAL ADDITIONS, PHASE-IN
! ADJUSTMENTS, SALE LEASEBACKS, AND LIFE EXTENSION COSTS
         IF (ICALL .EQ. 'G' .and. curiyr+uhbsyr .gt. efpsyr) &
              CALL ANEWBLD(NRGN)
! GET EB ARRAYS (LIST OF NEW ASSET DATA) FROM DIRECT ACCESS FILE
         CALL GETEB(NRGN,ICALL)
! COPY DISTRIBUTION DATA
         IF (ICALL .EQ. 'G') THEN
! SET NUMBER OF PHASE-INS AND SALE LEASEBACKS...
! ALSO CALL TRAN AND DIST ROUTINES
            NPI = SAVNPI
            NSL = SAVNSL
! LOAD IN REGIONAL ARRAYS
            CALL GETR(NRGN)
            if (curiyr+UHBSYR .ge. EFPSYR) CALL ELTRAN(NRGN)
            if (CURIYR+UHBSYR .ge. EFPSYR) CALL ELDIST(NRGN)
         ELSE
            NPI = 0
            NSL = 0
         ENDIF
! SELECT ONLY THE APPLICABLE DATA FOR EXISTING PLANTS
         DO N=1,2
            DO I=1,NPTYP
               DO IYR=1,EOVYRS
                  IF (ICALL .EQ. 'G') THEN
                     IF ((I .NE. EITRAN) .AND. (I .NE. EIDIST)) THEN
                        EOBKVL(IYR,I,N) = UOBKVL(IYR,I,N)
                        EOASVL(IYR,I,N) = UOASVL(IYR,I,N)
                        EOAITC(IYR,I,N) = UOAITC(IYR,I,N)
                        EOAFDC(IYR,I,N) = UOAFDC(IYR,I,N)
                        EOABDE(IYR,I,N) = UOABDE(IYR,I,N)
                     ELSE
                        EOBKVL(IYR,I,N) = 0.0
                        EOASVL(IYR,I,N) = 0.0
                        EOAITC(IYR,I,N) = 0.0
                        EOAFDC(IYR,I,N) = 0.0
                        EOABDE(IYR,I,N) = 0.0
                     ENDIF
                  ELSE
                     IF (ICALL .EQ. 'T') THEN
                        IF (I .EQ. EITRAN) THEN
                           EOBKVL(IYR,I,N) = UOBKVL(IYR,I,N)
                           EOASVL(IYR,I,N) = UOASVL(IYR,I,N)
                           EOAITC(IYR,I,N) = UOAITC(IYR,I,N)
                           EOAFDC(IYR,I,N) = UOAFDC(IYR,I,N)
                           EOABDE(IYR,I,N) = UOABDE(IYR,I,N)
                        ELSE
                           EOBKVL(IYR,I,N) = 0.0
                           EOASVL(IYR,I,N) = 0.0
                           EOAITC(IYR,I,N) = 0.0
                           EOAFDC(IYR,I,N) = 0.0
                           EOABDE(IYR,I,N) = 0.0
                        ENDIF
                     ELSE
                        IF (I .EQ. EIDIST) THEN
                           EOBKVL(IYR,I,N) = UOBKVL(IYR,I,N)
                           EOASVL(IYR,I,N) = UOASVL(IYR,I,N)
                           EOAITC(IYR,I,N) = UOAITC(IYR,I,N)
                           EOAFDC(IYR,I,N) = UOAFDC(IYR,I,N)
                           EOABDE(IYR,I,N) = UOABDE(IYR,I,N)
                        ELSE
                           EOBKVL(IYR,I,N) = 0.0
                           EOASVL(IYR,I,N) = 0.0
                           EOAITC(IYR,I,N) = 0.0
                           EOAFDC(IYR,I,N) = 0.0
                           EOABDE(IYR,I,N) = 0.0
                        ENDIF
                     ENDIF
                  ENDIF
               END DO
            END DO
         END DO
! UPDATE INPUT VARIABLES FROM LAST ITERATION OF PREVIOUS YEAR
         IF (FULLYR .GT. UESTYR) THEN
! GET RESULTS FROM LAST ITERATION OF PREVIOUS YEAR
            CALL GETRC(NRGN,ICALL,2)
! STORE THESE RESULTS AS BEING INITIAL VALUES FOR ALL OF THIS YEAR
            CALL STRRC(NRGN,ICALL,1)
         ELSE
! IN FIRST YEAR, FIRST ITERATION, JUST GET VALUES FROM POSITION 1
            CALL GETRC(NRGN,ICALL,1)
         ENDIF
      ELSE
! GET INITIAL VALUES FROM PREVIOUS YEAR LAST ITERATION
         IF (ICALL .EQ. 'G') THEN
            ICOM = 1
         ELSE
            IF (ICALL .EQ. 'T') THEN
               ICOM = 2
            ELSE
               ICOM = 3
            ENDIF
         ENDIF
         CALL GETRC(NRGN,ICALL,1)
         DO I=1,2
            ERFDCE(I) = URFDCE(I,ICOM,NRGN)
            ERNITC(I) = URNITC(I,ICOM,NRGN)
            ERATSD(I) = URATSD(I,ICOM,NRGN)
            ERXFDC(I) = URXFDC(I,ICOM,NRGN)
            ERFDCD(I) = URFDCD(I,ICOM,NRGN)
            ERPSDV(I) = URPSDV(I,ICOM,NRGN)
            DO II=1,4
               ERBL(II,I) = URBL(II,I,ICOM,NRGN)
            END DO
            ERYCWP(I) = URYCWP(I,ICOM,NRGN)
            DO II=1,16
               EPGW(II,I) = UPGW(II,I,ICOM,NRGN)
               EPCGW(II,I) = UPCGW(II,I,ICOM,NRGN)
               EPAFDC(II,I) = UPAFDC(II,I,ICOM,NRGN)
               EPONLN(II,I) = UPONLN(II,I,ICOM,NRGN)
               DO III=1,2
                  EPYCWP(II,III,I) = UPYCWP(II,III,I,ICOM,NRGN)
               END DO
               EPISOC(II,I) = UPISOC(II,I,ICOM,NRGN)
               EPADOC(II,I) = UPADOC(II,I,ICOM,NRGN)
            END DO
            ERTUP(I) = URTUP(I,ICOM,NRGN)
            ERBCWP(I) = URBCWP(I,ICOM,NRGN)
            ERABDE(I) = URABDE(I,ICOM,NRGN)
            ERPRDF(I) = URPRDF(I,ICOM,NRGN)
            ERDITC(I) = URDITC(I,ICOM,NRGN)
            EDISNT(I) = UDISNT(I,ICOM,NRGN)
            EPIDEF(I) = UPIDEF(I,ICOM,NRGN)
            ESLNDG(I) = USLNDG(I,ICOM,NRGN)
            ERBDE(I) = URBDE(I,ICOM,NRGN)
            ESLPRC(I) = USLPRC(I,ICOM,NRGN)
            ERRCWP(I) = URRCWP(I,ICOM,NRGN)
            ERCNBV(I) = URCNBV(I,ICOM,NRGN)
            ERCNAD(I) = URCNAD(I,ICOM,NRGN)
            ERAFDL(I) = URAFDL(I,ICOM,NRGN)
            ERDLRB(I) = URDLRB(I,ICOM,NRGN)
            ERAITC(I) = URAITC(I,ICOM,NRGN)
            ERFITC(I) = URFITC(I,ICOM,NRGN)
            ERAFDC(I) = URAFDC(I,ICOM,NRGN)
            ERFFDC(I) = URFFDC(I,ICOM,NRGN)
            EROFFS(I) = UROFFS(I,ICOM,NRGN)
            EREDTF(I) = VREDTF(I,ICOM,NRGN)
            EPIND(I) = UPIND(I,ICOM,NRGN)
            ESLLP(I) = USLLP(I,ICOM,NRGN)
            ESLAGN(I) = USLAGN(I,ICOM,NRGN)
            ERPRTX(I) = URPRTX(I,ICOM,NRGN)
            ERSLTX(I) = URSLTX(I,ICOM,NRGN)
            ERTDWO(I) = URTDWO(I,ICOM,NRGN)
            ESLLP(I) = USLLP(I,ICOM,NRGN)
            ERTDRG(I) = URTDRG(I,ICOM,NRGN)
            ERCIDC(I) = URCIDC(I,ICOM,NRGN)
            EDISYR(I) = UDISYR(I,ICOM,NRGN)
            EPIRET(I) = UPIRET(I,ICOM,NRGN)
            ERFDC(I) = URFDC(I,ICOM,NRGN)
            ERATSF(I) = URATSF(I,ICOM,NRGN)
            ERNDFPMT(I) = URQAMT(I,ICOM,NRGN)
            ERTOMD = URTOMD(ICOM,NRGN)
            ERTOMT = URTOMT(ICOM,NRGN)
         END DO
      ENDIF
      RETURN
      END
      SUBROUTINE ELLGTR(NRGN,ICALL)
      IMPLICIT NONE
!*******************************************************************
!         THIS SUBROUTINE TRANSFERS LOCAL TO GLOBAL VARIABLES
!*******************************************************************
      include 'parametr'
      include 'emmparm'
      include 'efpgen'
      include 'efpr'
      include 'efprc'
      include 'eusprc'
      include 'efpint'
      include 'efprcy'
      include 'ncntrl'
      include 'efptemp'
      CHARACTER*1 ICALL
      INTEGER I
      INTEGER II
      INTEGER III
      INTEGER IYR
      INTEGER N
      INTEGER NRGN
      INTEGER ICOM
! SAVE CAPITAL RELATED STUFF THAT ONLY IS USED IN FIRST ITERATION
      IF (CURITR .EQ. 1) THEN
         DO N=1,2
            DO I=1,NPTYP
               DO IYR=1,EOVYRS
                  IF (ICALL .EQ. 'G') THEN
                     IF ((I .NE. EITRAN) .AND. (I .NE. EIDIST)) THEN
                        UOABDE(IYR,I,N) = EOABDE(IYR,I,N)
                        UOBKVL(IYR,I,N) = EOBKVL(IYR,I,N)
                     ENDIF
                  ELSE
                     IF (ICALL .EQ. 'T') THEN
                        IF (I .EQ. EITRAN) THEN
                           UOABDE(IYR,I,N) = EOABDE(IYR,I,N)
                           UOBKVL(IYR,I,N) = EOBKVL(IYR,I,N)
                        ENDIF
                     ELSE
                        IF (I .EQ. EIDIST) THEN
                           UOABDE(IYR,I,N) = EOABDE(IYR,I,N)
                           UOBKVL(IYR,I,N) = EOBKVL(IYR,I,N)
                        ENDIF
                     ENDIF
                  ENDIF
               END DO
            END DO
         END DO
         IF (ICALL .EQ. 'D') CALL STRR(NRGN)
         CALL STREB(NRGN,ICALL)
      ENDIF
! SAVE LAGGED INFORMATION IN ALL ITERATIONS
      IF (ICALL .EQ. 'G') THEN
         ICOM = 1
      ELSE
         IF (ICALL .EQ. 'T') THEN
            ICOM = 2
         ELSE
            ICOM = 3
         ENDIF
      ENDIF
      DO I=1,2
         ESEMDL(I) = ESEMDT(I)
         ESEMPL(I) = ESEMPS(I)
         EROBL(I) = EROB(I)
         ERBNDL(I) = ERBOND(I)
         ERAMDL(I) = ERAMD(I)
         ERRBL(I) = ERRB(I)
         ERPFLPS(I) = ERPF(I)
         ERPRFL(I) = ERPREF(I)
         ERABDL(I) = ERABDE(I)
         ERPDFL(I) = ERPRDF(I)
         ELPRLT(I) = ESPRLT(I)
         ELPRST(I) = ESPRST(I)
         ELPRCE(I) = ESPRCE(I)
         ELPRPS(I) = ESPRPS(I)
         ELRBRR(I) = ERRBRR(I)
         ELRCWP(I) = ERRCWP(I)
         ELCIDC(I) = ERCIDC(I)
         ELTOMN(I) = ERTOMN(I)
           ELTFLN(I) = ERTFLN(I)
         ELBDE(I) = ERBDE(I)
         ELAITC(I) = ERAITC(I)
         ELFITC(I) = ERFITC(I)
         ELAFDC(I) = ERAFDC(I)
         ELFFDC(I) = ERFFDC(I)
         ELOFFS(I) = EROFFS(I)
         ELPRTX(I) = ERPRTX(I)
         ELSLTX(I) = ERSLTX(I)
         ELTDWO(I) = ERTDWO(I)
         ELTIEX(I) = ERTIEX(I)
         ELLLP(I) = ESLLP(I)
         ELLAGN(I) = ESLAGN(I)
         ELTLSL(I) = EQTLSL(I)
! STORE OTHER TEMP JUNK
         URFDCE(I,ICOM,NRGN) = ERFDCE(I)
         URNITC(I,ICOM,NRGN) = ERNITC(I)
         URATSD(I,ICOM,NRGN) = ERATSD(I)
         URXFDC(I,ICOM,NRGN) = ERXFDC(I)
         URFDCD(I,ICOM,NRGN) = ERFDCD(I)
         URPSDV(I,ICOM,NRGN) = ERPSDV(I)
         DO II=1,4
            URBL(II,I,ICOM,NRGN) = ERBL(II,I)
         END DO
         URYCWP(I,ICOM,NRGN) = ERYCWP(I)
         DO II=1,16
            UPGW(II,I,ICOM,NRGN) = EPGW(II,I)
            UPCGW(II,I,ICOM,NRGN) = EPCGW(II,I)
            UPAFDC(II,I,ICOM,NRGN) = EPAFDC(II,I)
            UPONLN(II,I,ICOM,NRGN) = EPONLN(II,I)
            DO III=1,2
               UPYCWP(II,III,I,ICOM,NRGN) = EPYCWP(II,III,I)
            END DO
            UPISOC(II,I,ICOM,NRGN) = EPISOC(II,I)
            UPADOC(II,I,ICOM,NRGN) = EPADOC(II,I)
         END DO
         URTUP(I,ICOM,NRGN) = ERTUP(I)
         URBCWP(I,ICOM,NRGN) = ERBCWP(I)
         URABDE(I,ICOM,NRGN) = ERABDE(I)
         URPRDF(I,ICOM,NRGN) = ERPRDF(I)
         URDITC(I,ICOM,NRGN) = ERDITC(I)
         UDISNT(I,ICOM,NRGN) = EDISNT(I)
         UPIDEF(I,ICOM,NRGN) = EPIDEF(I)
         USLNDG(I,ICOM,NRGN) = ESLNDG(I)
         URBDE(I,ICOM,NRGN) = ERBDE(I)
         USLPRC(I,ICOM,NRGN) = ESLPRC(I)
         URRCWP(I,ICOM,NRGN) = ERRCWP(I)
         URCNBV(I,ICOM,NRGN) = ERCNBV(I)
         URCNAD(I,ICOM,NRGN) = ERCNAD(I)
         URAFDL(I,ICOM,NRGN) = ERAFDL(I)
         URDLRB(I,ICOM,NRGN) = ERDLRB(I)
         URAITC(I,ICOM,NRGN) = ERAITC(I)
         URFITC(I,ICOM,NRGN) = ERFITC(I)
         URAFDC(I,ICOM,NRGN) = ERAFDC(I)
         URFFDC(I,ICOM,NRGN) = ERFFDC(I)
         UROFFS(I,ICOM,NRGN) = EROFFS(I)
         VREDTF(I,ICOM,NRGN) = EREDTF(I)
         UPIND(I,ICOM,NRGN) = EPIND(I)
         USLLP(I,ICOM,NRGN) = ESLLP(I)
         USLAGN(I,ICOM,NRGN) = ESLAGN(I)
         URPRTX(I,ICOM,NRGN) = ERPRTX(I)
         URSLTX(I,ICOM,NRGN) = ERSLTX(I)
         URTDWO(I,ICOM,NRGN) = ERTDWO(I)
         USLLP(I,ICOM,NRGN) = ESLLP(I)
         URTDRG(I,ICOM,NRGN) = ERTDRG(I)
         URCIDC(I,ICOM,NRGN) = ERCIDC(I)
         UDISYR(I,ICOM,NRGN) = EDISYR(I)
         UPIRET(I,ICOM,NRGN) = EPIRET(I)
         URFDC(I,ICOM,NRGN) = ERFDC(I)
         URATSF(I,ICOM,NRGN) = ERATSF(I)
         URQAMT(I,ICOM,NRGN) = ERNDFPMT(I)
         URTOMD(ICOM,NRGN) = ERTOMD
         URTOMT(ICOM,NRGN) = ERTOMT
      END DO
! UPDATE DIRECT ACCESS FILES WITH MODIFIED VALUES
      CALL STRRC(NRGN,ICALL,2)
      RETURN
      END
      SUBROUTINE CAPCOST(NRGN,ICALL,N)
      IMPLICIT NONE
!
!     *******************************************************************
!     THIS SUBROUTINE CALCULATES THE COST OF CAPITAL
!     *******************************************************************
!
!     INPUT VARIABLES:
!     MC_RMCORPPUAA(MNUMYR)=NATIONAL YIELD ON NEW AA UTILITY BONDS
!     MC_RMTCM10Y(MNUMYR) = 10 YEAR TREASURY NOTE YIELD

!     INTERNAL VARIABLES

!     ESRTDL =LAST YEAR'S REGIONAL NEW UTILITY DEBT
!     ESRTCL =LAST YEAR'S REGIONAL RETURN ON COMMON EQUITY
!     ESRTDA =AVERAGE OF ALL REGIONAL NEW LT UTILITY DEBT
!     N = OWNERSHIP CATEGORY (1=PRIVATE 2=PUBLIC)
!     IY = THE CURRENT YEAR OF THE FORECAST ( = CURIYR)

!     OUTPUT VARIABLES

!     ESRTST(N)=COST OF SHORT TERM DEBT
!     ESRTLT(N)=COST OF NEW LONG TERM DEBT
!     ESRTCE(N)=COST OF COMMON EQUITY
!     EWGTCE   = COST OF COMMON EQUITY USED FOR NEW EWG BUILDS (ECP)
!     ESRTPS(N)=COST OF NEW PREFERRED STOCK

      include 'parametr'
      include 'ncntrl'
      include 'efprcy'
      include 'emmparm'
      include 'macout'
      include 'efpgen'
      include 'eusprc'
      include 'efpint'
      include 'bildin'
      include 'control'
      include 'ecpcntl'
      include 'emission'
      include 'efpcntrl'

      REAL*4 ESRTCER
      REAL*4 ESRTPSR
      REAL*4 ESRTLTR
      REAL*4 ESRTSTR
      REAL*4 RETRSK
!
      REAL*4 BRMCF(2)
      REAL*4 PSRCF(2)
      REAL*4 ESRTD1,EWGTCE
      REAL UAAADJ(3),UTAART,TNOTE,MRKPRM
      REAL*4 DUMMY_ROE
      INTEGER NRGN
      INTEGER N
      INTEGER I, IY
      INTEGER J
      INTEGER L
      INTEGER ICOM,FULLYR
      CHARACTER*1 ICALL
      REAL SV_PRGEN_ESRR(MNUMYR,MNUMNR)
      COMMON /STORE_ROR/SV_PRGEN_ESRR
!     real reftnote(MNUMYR),refaabond(MNUMYR),macadj(MNUMYR,3),wopadj(MNUMYR,3)
      REAL CORPPREM
      DATA CORPPREM /0.38/   ! pw2 as per Ron 8/26/99

!     DATA reftnote/ 8.550,7.858,7.010,5.873,7.080,6.580,6.438,&
!          6.352,5.264,5.637,6.029,5.017,4.611,4.015,4.436, &
!          5.491,5.450,5.301,5.992,6.392,6.576,6.562,6.537, &
!          6.513,6.553,6.496,6.544,6.532,6.525,6.476,6.446, &
!          6.552,6.560,6.601,6.612,6.600/

!     DATA refaabond/ 9.655,9.096,8.546,7.433,8.208,7.764,7.566,&
!          7.541,6.911,7.508,8.063,7.572,7.194,6.394,6.234, &
!          7.157,6.885,6.481,7.113,7.398,7.621,7.671,7.749, &
!          7.815,7.973,8.076,8.253,8.302,8.345,8.334,8.336, &
!          8.472,8.515,8.588,8.619,8.621/

!     DATA macadj/16*0.0,0.02,0.04,0.06,0.08,0.10,0.12,0.14,0.16,0.18,0.20, &
!                        0.22,0.24,0.26,0.28,0.30,0.30,0.30,0.30,0.30,0.30, &
!                 36*0.0,  &
!                 16*0.0,-0.02,-0.04,-0.06,-0.08,-0.10,-0.12,-0.14,-0.16,-0.18,-0.20, &
!                        -0.22,-0.24,-0.26,-0.28,-0.30,-0.30,-0.30,-0.30,-0.30,-0.30 /

!     WOP Macro adjustment removed for AEO2003

!     DATA wopadj/36*0.0,  &
!                 36*0.0,  &
!                 36*0.0/

!     DATA wopadj/16*0.0,-.01,-.02,-.03,-.04,-.05,-.06,-.07,-.08,-.09,11*-.10, &
!                 36*0.0,  &
!                 16*0.0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,11*0.10 /

!     REGRESSION COEFFICIENTS FOR LONG TERM PRIVATE DEBT

!     REGRESSION COEFFICIENTS FOR LONG TERM PUBLIC DEBT

      DATA BRMCF/-0.0080,0.8337/

!     REGRESSION COEFFICIENTS FOR COMMON EQUITY



!     REGRESSION COEFFICIENTS FOR PREFERRED STOCK

      DATA PSRCF/0.0083,0.8842/

      DATA UAAADJ/-1.00,0.00,0.60/

      FULLYR = USYEAR(CURIYR)

!     INITIALIZE TOTALS FOR COST OF CAPITAL VARIABLES

      ERBTXIT(NRGN, 4) = 0.0
      XINTCH(NRGN, 4) = 0.0
      TRNPL(NRGN, 4) = 0.0
      DSTPL(NRGN, 4) = 0.0
      TUTPL(NRGN, 4) = 0.0
      CWIPINT(NRGN, 4) = 0.0
      NTUTPL(NRGN, 4) = 0.0


!     SET ICOM FOR USE IN RECORDING LAST YEARS COST OF CAPITAL

      IF (ICALL .EQ. 'G') ICOM=1
      IF (ICALL .EQ. 'T') ICOM=2
      IF (ICALL .EQ. 'D') ICOM=3

! INITIALIZE
      IF (FULLYR .EQ. UESTYR) ESRTDA(ICOM) = ESRTLT(1)

!    update NUG debt premium to difference between industrial BAA and utility AA
      UPNIPRM = (MC_RMCORPBAA(CURIYR) - MC_RMCORPPUAA(CURIYR))/100.0

!     THESE CALCULATIONS SHOULD BE USED ONLY FOR YEARS IN WHICH THE
!     COST OF CAPITAL THAT WAS READ IN IS TO BE OVERWRITTEN -
!     1ST ITERATION ONLY

      IF (FULLYR .GE. UESTYR) THEN
         IF (CURITR .EQ. 1) THEN

!           DO   IY=1,MNUMYR          Output deflator on FMGROUT
!              WRITE(10,*) 'MC_JPGDP(', IY, ' )', MC_JPGDP(IY)
!           ENDDO

!           CALCULATE LONG TERM DEBT (1) PRIVATE AND (2) PUBLIC


!           APPLY ADJUSTMENT TO UTILITY BOND RATE IN MACRO CASE SINCE FEEDBACK
!           FOR CAPITAL INVESTMENT NOT LIKED

            UTAART = MC_RMCORPPUAA(CURIYR)
            ESRTLT(1)=(UTAART)/100

            ESRTLT(2)=(BRMCF(1)+(BRMCF(2)*UTAART))/100
!
!           ADJUST FOR CALIBRATION
!
            ESRTLT(1) = ESRTLT(1) + RORADJ
            ESRTLT(2) = ESRTLT(2) + RORADJ
!
!           INCREASE COST OF LONG TERM DEBT FOR COMPETITION RUN ***
!
            IF ((ICOM .EQ. 1) .AND. (CURIYR .GT. 5) .AND. (USW_POL .GE. 1)) THEN
               ESRTLT(1) = ESRTLT(1) + CAPADJ
               ESRTLT(2) = ESRTLT(2) + CAPADJ
            ENDIF

!           CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!           START NEW COST OF COMMON EQUITY CALCULATION (IOU'S) *****

!           SUM VARIABLES FROM FINANCIAL STMTS OVER OPERATING TYPE -
!           WILL USE SAME VALUES FOR G/T/D

            DO L = 1, 3
               ERBTXIT(NRGN,4) = ERBTXIT(NRGN,4) + ERBTXIT(NRGN,L)
               XINTCH(NRGN,4) = XINTCH(NRGN,4) + XINTCH(NRGN,L)
               TRNPL(NRGN,4) = TRNPL(NRGN,4) + TRNPL(NRGN,L)
               DSTPL(NRGN,4) = DSTPL(NRGN,4) + DSTPL(NRGN,L)
               TUTPL(NRGN,4) = TUTPL(NRGN,4) + TUTPL(NRGN,L)
               CWIPINT(NRGN,4) = CWIPINT(NRGN,4) + CWIPINT(NRGN,L)
               NTUTPL(NRGN,4) = NTUTPL(NRGN,4) + NTUTPL(NRGN,L)
            ENDDO

!           MAKE CALCULATIONS FOR REGRESSION VARIABLES

            CSTCON(NRGN) = MAX(RESPCT(NRGN),COMPCT(NRGN),INDPCT(NRGN))
            TMSINT(NRGN) = ERBTXIT(NRGN,4) / XINTCH(NRGN,4)
            TRNPCT(NRGN) = TRNPL(NRGN,4) / TUTPL(NRGN,4)
            DSTPCT(NRGN) = DSTPL(NRGN,4) / TUTPL(NRGN,4)
            BLDRAT(NRGN) = CWIPINT(NRGN,4) / NTUTPL(NRGN,4)
            NUCPCT(NRGN) = (N_PIS(NRGN)+NCAPEX(NRGN)) / (S_PIS(NRGN) + &
               SCAPEX(NRGN))

            INFRAT = MC_JPGDP(CURIYR)/MC_JPGDP(CURIYR-1) - 1.0
            TNOTE = MC_RMTCM10Y(CURIYR)/100.0

!           CALCULATE BETA FOR THE CAPM METHOD
!           USE JIM'S BETA IF SET TO NONZERO

            MRKPRM = .0575
            INTRFRT = TNOTE
            IF (UTBETA .GT. 0 ) THEN
               INTBETA = UTBETA
               INTROEU = INTRFRT + MRKPRM * INTBETA

!                   write(6,*)'intbeta ',intbeta

            ELSE
               INTBETA = 0.74 - 2.834*TRNPCT(NRGN) - .189*BLDRAT(NRGN) &
                  + .0000024*NTUTPL(NRGN,4) - .983*INFRAT &
                  + .73*DSTPCT(NRGN) - .046*NUCPCT(NRGN) &
                  - .027*TMSINT(NRGN) + .321*CSTCON(NRGN)
               INTROEM = 0.15
               INTROEU = INTRFRT + INTBETA*(INTROEM - INTRFRT)
            ENDIF
            ESRTCE(1) = INTROEU

!           CALCULATE DIFFERENT COST OF EQUITY FOR NEW BUILDS, USE CAPM METHOD, DIFFERENT BETA

            IF (UPBETA .GT. 0.0 ) THEN
               EWGTCE = INTRFRT + MRKPRM * UPBETA
            ELSE
               EWGTCE = ESRTCE(1)
            ENDIF
            WRITE(22,3811) CURIYR+UHBSYR,CURITR,NRGN,N,ICALL,EWGTCE,UPBETA,INTRFRT,MRKPRM,ESRTCE(1),UTBETA,INTROEU
 3811       FORMAT(1X,"EWGTCE",4(":",I4),":",A1,7(":",f9.3))

!           SET COMMON EQUITY FOR PUBLICS EQUAL TO LT DEBT TO REPRESENT
!           SOURCES OF CAPITAL OTHER THAN DEBT (ALREADY ADJ'D FOR COMP.)

            ESRTCE(2)=ESRTLT(2)
!
!           ADJUST FOR CALIBRATION
!
            ESRTCE(1) = ESRTCE(1) + RORADJ

!           CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!           INCREASE COST OF EQUITY FOR IOU'S FOR COMPETITION CASE

            IF ((ICOM .EQ. 1) .AND. (CURIYR .GT. 5) .AND. (USW_POL .GE. 1)) THEN
               ESRTCE(1)=ESRTCE(1) + CAPADJ
            ENDIF

!           CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!           IF ((ICOM .EQ. 1) .AND. (N .EQ. 1)) THEN
!
!              WRITE(6,100) NRGN,CURIYR,TRNPCT(NRGN),BLDRAT(NRGN), &
!                 NTUTPL(NRGN,4),INFRAT,DSTPCT(NRGN),NUCPCT(NRGN), &
!                 TMSINT(NRGN),CSTCON(NRGN),ESRTCE(1)
!              WRITE(6,110) INFRAT,INTBETA,INTRFRT,INTROEU
!110           FORMAT(1X,'C2',4F8.4)
!100           FORMAT (1X,'CC',I4,I3,2F8.4,F10.2,6F8.4)
!
!           ENDIF


!           CALCULATE THE COST OF NEW PREFERRED STOCK FOR IOU'S

            ESRTPS(1)=PSRCF(1)+PSRCF(2)*ESRTDA(ICOM)
!
!           ADJUST FOR CALIBRATION
!
            ESRTPS(1) = ESRTPS(1) + RORADJ

!           CC INCREASE FOR COMPETITION RUN ****************

            IF ((ICOM .EQ. 1) .AND. (CURIYR .GT. 5) .AND. (USW_POL .GE. 1)) ESRTPS(1) = ESRTPS(1) + CAPADJ

!           CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

            ESRTPS(2)=0

!           SET ST DEBT EQUAL TO LT DEBT FOR NOW

            ESRTST(N)=ESRTLT(N)

!           IF NOT THE FIRST ITERATION, MAINTAIN VALUES

         ELSE
            ESRTLT(N)=ESRTDL(N,NRGN,ICOM)
            ESRTCE(N)=ESRTCL(N,NRGN,ICOM)
            ESRTST(N)=ESRTSL(N,NRGN,ICOM)
            ESRTPS(N)=ESRTPL(N,NRGN,ICOM)

         ENDIF
      ENDIF

!     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!     IF (NRGN .EQ. 9) THEN
!
!        WRITE(*,10) ESRTDL(N,NRGN,ICOM),ESRTCL(N,NRGN,ICOM),ESRTCE(N), &
!           ESRTLT(N),ESRTST(N),ESRTPS(N),ESRTDA(ICOM), &
!           N,NRGN,ICOM
!10      FORMAT(/' CAPCOST1 - ESRTDL:',F7.4,' ESRTCL:',F7.4/ &
!           '   ESRTCE:',F7.4,' ESRTLT:',F7.4,' ESRTST:',F7.4/ &
!           '   ESRTPS:',F7.4,' ESRTDA:',F7.4, &
!           ' N,NRGN,ICOM:',I3,I3,I3/)
!
!     ENDIF

!     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!     SET LAST YEARS COST OF DEBT AND STOCK (ALSO FOR MAINTAINING
!     VALUES THROUGH ITERATIONS)

      ESRTDL(N,NRGN,ICOM)=ESRTLT(N)
      ESRTCL(N,NRGN,ICOM)=ESRTCE(N)
      ESRTSL(N,NRGN,ICOM)=ESRTST(N)
      ESRTPL(N,NRGN,ICOM)=ESRTPS(N)

!     DETERMINE NATIONAL AVERAGE COST OF NEW LT IOU DEBT (ESRTDA)
!     ON 2ND ITERATION FOR COST OF PREFERRED STOCK CALCULATION
!     IN FOLLOWING YEAR

      IF (CURITR .EQ. 2) THEN
         ESRTD1=0.0
         J=0
         DO I=1,MNUMNR-1
            IF (ESRTDL(1,I,ICOM) .NE. 0.0) THEN
               ESRTD1=(ESRTD1+ESRTDL(1,I,ICOM))
               J=J+1
            ENDIF

!           WRITE(*,19) ESRTDL(1,I,ICOM),ESRTD1,I,ICOM,J
!19         FORMAT(' CAPCOST1.5 - ESRTDL:',F8.4,' ESRTD1:',F8.4, &
!              ' I,ICOM,J:',I3,I3,I3)

         END DO
         ESRTDA(ICOM)=ESRTD1/J
      END IF


!     WRITE(*,20) ESRTDL(N,NRGN,ICOM),ESRTCL(N,NRGN,ICOM), &
!        ESRTD1,ESRTDA(ICOM),N,NRGN,ICOM,CURIYR,CURITR, &
!        MNUMNR
!20   FORMAT(/' CAPCOST2 - ESRTDL:',F7.4,' ESRTCL:',F7.4, &
!        ' ESRTD1:',F7.4,' ESRTDA:',F7.4, &
!        ' N,NRGN,ICOM,CURIYR,CURITR:',I3,I3,I3,I3,I3, &
!        ' MNUMNR:',I3/)

!     SEND IOU GENERATION COST OF CAPITAL DATA TO CAPACITY PLANNING
!     AFTER 1ST ITERATION (PREF STOCK RATE IS CALC'D IN 2ND ITERATION)

      IF ((ICALL .EQ. 'G') .AND. (N .EQ. 1))THEN

!        FIRST YEAR, FIRST ITERATION, INITIALIZE WITH NOMINAL VALUES FOR RFM

         IF (FULLYR .EQ. UESTYR .AND. CURITR .EQ. 1)THEN
            CALL GETBLD(1,NRGN)
            EPUIRT = 0.08
            EPUCRE = 0.14
            CALL STRBLD(1,NRGN)
         END IF
         IF (CURITR .GT. 1)THEN
            CALL GETBLD(1,NRGN)

!           SEND WEIGHTED AVERAGE COST OF CAPITAL
              EPUROR = SV_PRGEN_ESRR(CURIYR,NRGN)

!           SEND UTILITY INTEREST RATE (WEIGHTED AVERAGE LONG AND SHORT TERM

            EPUIRT=((ESRTLT(1)*ESPRLT(1))+(ESRTST(1)*ESPRST(1)))/(ESPRLT(1)+ESPRST(1))

!           SEND UTILITY DEBT FRACTION

            EPUFDT=ESPRLT(1)+ESPRST(1)

!           SEND EPUFPE

            IF (EPUROR.gt.0) EPUFPE=((ESRTCE(1)*ESPRCE(1))+(ESRTPS(1)*ESPRPS(1)))/EPUROR

!           SEND RETURN ON COMMON EQUITY

            EPUCRE=EWGTCE

!           OVERWRITE DEBT AND EQUITY RATES WITH ASSUMED VALUES, IF SPECIFIED IN ECPDAT

            IF(UPNIRTE .GT. 0.0)EPUIRT = UPNIRTE
            IF(UPNRRTE .GT. 0.0)EPUCRE = UPNRRTE

!           SEND NOM. TAX ADJUSTED WACC

            EPUTDSCRT=ESRTCE(1)*ESPRCE(1)+ESRTPS(1)*ESPRPS(1)+  &
               (ESRTLT(1)*ESPRLT(1)+ESRTST(1)*ESPRST(1))*(1-UPTXRT)
            EPDSCRT=UPNFDT*(EPUIRT+UPNIPRM)*(1-UPTXRT) + (1.0-UPNFDT)*(EPUCRE+UPNRPRM)
            IF (FCRL .eq. 1) THEN
               write(22,19) CURIYR,CURITR,NRGN,EPUIRT,EPUFDT,EPUROR,EPUTDSCRT,EPUFPE
               write(22,20) CURIYR,CURITR,NRGN,ESRTCE(1),ESPRCE(1),ESRTPS(1),ESPRPS(1)
               write(22,121) CURIYR,CURITR,NRGN,EPDSCRT,UPNFDT,EPUIRT,UPNIPRM,UPTXRT,EPUCRE,UPNRPRM
  121          format(1x,'epdscrt',3(":",I4),7(":",F9.4))
   19          format(1x,'util dsc',3I4,5F8.4)
   20          format(1x,'util equ',3I4,4F8.4)
            ENDIF

!           ADD RISK PREMIUM FOR RETROFITS, IF SWITCH IS ON

            IF (USW_RTRSK .GT. 0)THEN
               RETRSK = EXTRARISK(CURIYR + UHBSYR)
            ELSE
               RETRSK = 0.0
            END IF
            EPURORR = EPUROR + RETRSK
            ESRTLTR = ESRTLT(1) + RETRSK
            ESRTSTR = ESRTST(1) + RETRSK
            EPUIRTR = ((ESRTLTR * ESPRLT(1)) + (ESRTSTR * ESPRST(1))) / (ESPRLT(1) + ESPRST(1))
            ESRTCER = ESRTCE(1) + RETRSK
            ESRTPSR = ESRTPS(1) + RETRSK
            EPUFPER = ((ESRTCER * ESPRCE(1)) + (ESRTPSR * ESPRPS(1))) / EPURORR
            EPUCRER = EPUCRE + RETRSK

            CALL STRBLD(1,NRGN)
         ENDIF
      ENDIF

      RETURN
      END
      SUBROUTINE ELDIST(NRGN)
      IMPLICIT NONE
!*******************************************************************
!       THIS SUBROUTINE DETERMINES THE COST OF CONSTRUCTION AND MAINTENANCE OF DISTRIBUTION EQUIPMENT
!*******************************************************************
! INPUT VARIABLES:
!     NRGN   = CURRENT REGION
!     CURIYR = CURRENT YEAR INDEX
!     EIDIST = PLANT CODE FOR DISTRIBUTION CAPITAL
!     ESTSHR = PRIVATE/PUBLIC GENERATION SHARE
!              EQUIPMENT
!     ESLCP  = LENGTH OF THE CONSTRUCTION PERIOD FOR EACH BUILD
!     CPDCON, CPDSLP = constant and slope of capital distribution exp.
!       CAPDSCALE(NRGN,CURIYR)= Multiplier to ramp up or tone down CPDSLP by analyst
! INTERNAL VARIABLES:
!     EDLCP  = length of construction period
!     CPDCST = capital distribution cost in millions of $
! OUTPUT VARIABLES:
!     ERTOMD = TOTAL DISTRIBUTION RELATED O&M EXPENSES REAL DOLLARS
      include 'parametr'
      include 'emmparm'
      include 'efpgen'
      include 'eusprc'
      include 'efpint'
      include 'ncntrl'
      include 'efprcy'
      include 'efpr'
      include 'ldsm'
      INTEGER I
      INTEGER NRGN
      INTEGER EDLCP
      REAL*4 CPDCST
      REAL*4 PCST
      REAL*4 SERP
      INTEGER PTYP
      INTEGER SYR
      REAL*4 PCAP(2)
      REAL*4 BCWP(2)
      REAL*4 ASVL(2)
      REAL*4 DITC(2)
      REAL*4 AFDC(2)
      REAL*4 RCWP(2)
      REAL*4 ABDE(2)
      REAL*4 BKVL(2)
      REAL SALTOTOLD,SALTOTLAST
      COMMON /SALEOLD/ SALTOTOLD,SALTOTLAST
! Length of construction period for each build
      EDLCP = ESLCP(EIDIST) - 1
! CALCULATE THE COST OF NEW DISTRIBUTION EQUIPMENT AND CREATE A NEW
! DISTRIBUTION BUILD
!     CPDCST = (CPDSLP * (EQTLSL(1) + EQTLSL(2))) + CPDCON
!     CPDCST = (CPDSLP * (SALTOTLAST)) + CPDCON
      CPDCST = ((CPDSLP * NCPNEMS(NRGN)) + CPDCON) * CAPDSCALE(NRGN,CURIYR)
      WRITE(18,4025)'CPDCST ',NRGN,CURIYR,CPDCST,CPDSLP,NCPNEMS(NRGN), CPDCON, CAPDSCALE(NRGN,CURIYR)
4025  FORMAT(A20,1x,2(I4,1x),10(F12.3,1x))      
!     write(6,'(a,2i6,3f12.1)')'in eldist - NCPeak     ',CURIYR,NRGN,  &
!             NCPNEMS(NRGN),CPDSLP,NCPNEMS(NRGN)*CPDSLP
!     write(6,'(a,2i6,3f12.1)')'in eldist - SALTOTLAST ',CURIYR,NRGN,SALTOTLAST,CPDSLP,  &
!              CPDSLP * SALTOTLAST
      IF (CPDCST .NE. 0.0) THEN
         PCST = CPDCST
         PTYP = EIDIST
         SYR = CURIYR + EDLCP
         SERP = 1.0
         DO I=1,2
            PCAP(I) = ESTSHR(I)
            BCWP(I) = 0.0
            ASVL(I) = 0.0
            DITC(I) = 0.0
            AFDC(I) = 0.0
            RCWP(I) = 0.0
            ABDE(I) = 0.0
            BKVL(I) = 0.0
         END DO
         CALL UNEWBLD(NRGN,DITC,AFDC,BKVL,ASVL,BCWP,RCWP,PTYP, &
                     SYR,PCST,PCAP,SERP,ABDE)
      ENDIF
      RETURN
      END
      SUBROUTINE ELTRAN(NRGN)
      IMPLICIT NONE
!*******************************************************************
!       THIS SUBROUTINE DETERMINES THE COST OF CONSTRUCTION AND MAINTENANCE OF TRANSMISSION EQUIPMENT
!*******************************************************************
! INPUT VARIABLES:
!     NRGN   = CURRENT REGION
!     CURIYR = CURRENT YEAR INDEX
!     EITRAN = PLANT CODE FOR TRANSMISSION CAPITAL
!     EQTLSL = public and private energy sales (10^6 kWh)
!     ESTSHR = PRIVATE/PUBLIC GENERATION SHARE
!     ESLCP  = LENGTH OF THE CONSTRUCTION PERIOD FOR EACH BUILD
!     CPTCON, CPTSLP = constant and slope of capital transmission exp.
! INTERNAL VARIABLES:
!     ETLCP  = LENGTH OF CONSTRUCTION PERIOD FOR TRANSMISSION
!              EQUIPMENT
!     CPTCST = Capacity cost in millions of $
! OUTPUT VARIABLES:
!     ERTOMT = TOTAL TRANSMISSION RELATED O&M EXPENSES REAL DOLLARS
      include 'parametr'
      include 'emmparm'
      include 'efpgen'
      include 'eusprc'
      include 'efpint'
      include 'ncntrl'
      include 'efpbld'
      include 'efpr'
      include 'efprcy'
      include 'ldsm'
      REAL*4 PCST
      INTEGER PTYP
      INTEGER SYR
      REAL*4 PCAP(2)
      REAL*4 BCWP(2)
      REAL*4 ASVL(2)
      REAL*4 DITC(2)
      REAL*4 EMOM
      REAL*4 AFDC(2)
      REAL*4 RCWP(2)
      REAL*4 ABDE(2)
      REAL*4 BKVL(2)
      INTEGER I
      REAL*4 ETLCP
      INTEGER NRGN
      REAL*4 CPTCST
      REAL*4 SERP
      REAL SALTOTOLD,SALTOTLAST
      COMMON /SALEOLD/ SALTOTOLD,SALTOTLAST
! Length of construction period for each build
      ETLCP = ESLCP(EITRAN) - 1
! Calculate total capital costs of transmission (in millions of $)
!     CPTCST = CPTSLP*(EQTLSL(1) + EQTLSL(2)) + CPTCON
!     CPTCST = (CPTSLP*(SALTOTLAST) + CPTCON) * TSCALE(NRGN,CURIYR)
      CPTCST =  (CPTSLP*NCPNEMS(NRGN) + CPTCON + CPTADDER(NRGN,CURIYR)) * CAPTSCALE(NRGN,CURIYR)
      WRITE(18,4025)'CPDCST ',NRGN,CURIYR,CPTCST,CPTSLP,NCPNEMS(NRGN), CPTCON,CPTADDER(NRGN,CURIYR), CAPTSCALE(NRGN,CURIYR)
4025  FORMAT(A20,1x,2(I4,1x),10(F12.3,1x))      
      
!     write(6,'(a,2i6,3f12.1)') 'in eltran - NCPeak     ',CURIYR,NRGN,  &
!               NCPNEMS(NRGN),CPTSLP,NCPNEMS(NRGN) * CPTSLP
!     write(6,'(a,2i6,3f12.1)')'in eltran - SALTOTLAST ',CURIYR,NRGN,SALTOTLAST,CPTSLP,  &
!                     CPTSLP * SALTOTLAST
! Calculate the COST OF NEW TRANSMISSION EQUIPMENT AND CREATE A NEW
! TRANSMISSION BUILD
      IF (CPTCST .NE. 0.0) THEN
         PCST = CPTCST
         PTYP = EITRAN
         SYR = CURIYR + ETLCP
         DO I=1,2
            PCAP(I) = ESTSHR(I)
            BCWP(I) = 0.0
            ASVL(I) = 0.0
            DITC(I) = 0.0
            AFDC(I) = 0.0
            ABDE(I) = 0.0
            BKVL(I) = 0.0
            RCWP(I) = 0.0
            SERP = 1.0
         END DO
         CALL UNEWBLD(NRGN,DITC,AFDC,BKVL,ASVL,BCWP,RCWP,PTYP, &
                     SYR,PCST,PCAP,SERP,ABDE)
      ENDIF
      RETURN
      END
!
      SUBROUTINE GL(NRGN,ICALL)
      IMPLICIT NONE
!*******************************************************************
!    THIS SUBROUTINE GATHERS INFORMATION NEEDED FROM THE ELECTRICITY DISPATCH MODEL
!*******************************************************************
      include 'parametr'
      include 'dsmdimen'
      include 'emmparm'
      include 'control'
      include 'efpcntrl'
      include 'dsmhelm'
      include 'dispin'
      include 'dispout'
      include 'efpgen'
      include 'eusprc'
      include 'efpint'
      include 'efprcy'
      include 'ncntrl'
      include 'efpr'
      include 'efprp2'
      include 'cogen'
      include 'uefdout'
      include 'bildin'
      include 'ecpcntl'
      include 'macout'
      include 'dsmunits'
      include 'ldsm'
      include 'e111d'
      include 'dsmsectr'     !-- added by AKN
!
      INTEGER I,IOWN
      INTEGER J,IECP,JECP,IEFD
      CHARACTER*1 ICALL
      INTEGER NRGN,ICLASS
      INTEGER CENSUS   !-- added by AKN

      REAL LCVOM, LCFL, LCWS, LCFOM, LCGNA, OMFAC
      REAL SALTOTOLD,SALTOTLAST
      REAL*8 FOM_ALL,FOM_NUC
      COMMON /SALEOLD/ SALTOTOLD,SALTOTLAST
      REAL*8 ERCPPTR(MNUMNR,MNUMYR), ERCPPTRQ(MNUMNR,MNUMYR)
      COMMON /ERCPRC/ERCPPTR,ERCPPTRQ

      COMMON /BCHMRKSTEOPRICE/ BCHMRKSTEOPRICEADDER, BCHMRKSTEOPRC_N   !STEO Price benchmark feature --- by AKN
      REAL BCHMRKSTEOPRICEADDER(MNUMCR,MNUMYR), BCHMRKSTEOPRC_N(MNUMNR,MNUMYR)  !STEO benchmark price adjustment adder --- by AKN
      REAL ERZECCST(MNUMNR)
      COMMON/ZECCST/ERZECCST


!
!     GET INFORMATION ON FUEL AND O&M
!
      IF (ICALL .EQ. 'G') THEN
         CALL GETOUT(CURIYR,NRGN)
         CALL GETIN(1,NRGN)

!        WRITE(6,3979) CURIRUN, CURIYR+1989, CURITR, NRGN, EEITAJ(1), EEITAJ(2), EEITAJ(3)
!3979    FORMAT(1X,"UEFP_03680_EEITAJ_GET",4(":",I4),3(":",F12.3))

         FOM_ALL = 0.0
         DO IEFD = 1,EFD_D_CAP
           FOM_ALL = FOM_ALL + ERTOMF(IEFD,3) + ERTOMX(IEFD,3) + ERTOMF(IEFD,4) + ERTOMX(IEFD,4)
         ENDDO
         EWGREV = EWGFIX + ERTFL(3) + ERTOM(3) + FOM_ALL &
                         + ERTSO2(3) + ERTNOX(3) + ERTHG(3) - ERTGSUB(3) &
                         + ERTFL(4) + ERTOM(4)  &
                         + ERTSO2(4) + ERTNOX(4) + ERTHG(4) - ERTGSUB(4)
!        write(6,3980) CURIYR, CURITR, NRGN, ERTFL(4), ERTOM(4), ERTSO2(4), ERTNOX(4), ERTHG(4), ERTGSUB(4)
!3980    FORMAT(1x,'NT COGEN costs ',3I4,6F12.3)
         EFPBLK(1,1) =  ETDMDF
         EFPBLK(1,2) =  ETDMMF/1000.0
         EFPBLK(2,1) =  ETDMDE
         EFPBLK(2,2) =  ETDMME/1000.0
         EFPBLK(3,1) =  ETIMPD
         EFPBLK(3,2) = (ETIMPF + ETIMPE)/1000.0
         EFPBLK(4,1) =  -1.0 * ETEXPD
         EFPBLK(4,2) = (ETEXPF + ETEXPE)/1000.0

!        EFPBLK(5,1) = EWGREV + EWGRCC + EWGRIC + EWGRNW
!  EWGREV includes costs for all types, adding EWGRNW is double counting renewables!

         EFPBLK(5,1) = EWGREV + EWGRCC + EWGRIC
         EFPBLK(5,2) = UGNTLNR(2,NRGN,CURIYR)

!        EFPBLK(5,2) = UGNTLNR(2,NRGN,CURIYR) &
!                      + (CGTLGEN(NRGN,CURIYR,1)/1000.0)

         PPWRBLK(1) = ERTFL(3) + ERTSO2(3) + ERTNOX(3) + ERTHG(3) - ERTGSUB(3) &
                    + ERTFL(4) + ERTSO2(4) + ERTNOX(4) + ERTHG(4) - ERTGSUB(4)
         PPWRBLK(2) = ERTOM(3) + ERTOM(4) + FOM_ALL
         PPWRBLK(3) = EWGFIX
         PPWRBLK(4) = EWGRCC + EWGRIC + &
               ETDMDE + ETIMPD - ETEXPD + ETDMDF

         WRITE(18,7713) CURIRUN, CURIYR+1989, CURITR, NRGN, PPWRBLK(1), PPWRBLK(2), PPWRBLK(3), PPWRBLK(4), ERTFL(3), ERTSO2(3), ERTNOX(3), ERTHG(3), ERTGSUB(3), &
            ERTFL(4), ERTSO2(4), ERTNOX(4), ERTHG(4), ERTGSUB(4), ERTOM(3), ERTOM(4), FOM_ALL, EWGFIX, EWGRCC, EWGRIC, ETDMDE, ETIMPD, ETEXPD, ETDMDF
 7713    FORMAT(1X,"UEFP_PPWRBLK",4(":",I4),24(":",F21.6))

!        write(*,*)'WHOL :',CURIYR,':',CURITR,':',NRGN,':', EWGRCC,':'
!    +   ,EWGRIC,':',ETDMDE,':',ETIMPD,':',ETEXPD,':',ETDMDF
         BLKSUM = 0.0
         DO I=1,5
            BLKSUM = BLKSUM + EFPBLK(I,1)
         END DO
         LCWS=BLKSUM
         EFPFL = ERTFL(1) + ERTFL(2)
!  ADD COST OF RENEWABLE CREDITS TO FUEL COST
!  ADD COSTS OF SO2 AND NOX ALLOWANCES TO FUEL COST
         IF (ERRPS .LT. 0.0) THEN
            ERRPS = ERRPS * 0.5
         ENDIF
         EFPFL = EFPFL + ERRPS + ERTSO2(1) + ERTSO2(2) &
                               + ERTNOX(1) + ERTNOX(2) &
                               + ERTHG(1) + ERTHG(2)   &
                               - ERTGSUB(1) - ERTGSUB(2) &
                               + ERCPPTR(NRGN,CURIYR)

!      add STEO benchmark adjustment adders right here  (moved to generation component - LC2)
            IF ((BMELPRC(CURIYR) .GT. 0.0) .AND.(CURITR .GE. 2)) THEN
 !                   write(6,*) 'EFPFL b4 STEO ',CURIYR,CURITR,EFPFL
              EFPFL = EFPFL - BCHMRKSTEOPRC_N(NRGN,CURIYR)/1000000
 !                   write(6,*) 'EFPFL af STEO ',CURIYR,CURITR,EFPFL
            ENDIF

         if (FCRL .eq. 1) &
         write(imsg,1668) ' PTC subsidy in EFP', CURIYR,NRGN,(ERTGSUB(I),I=1,3)
1668     format(1x,a20,2I4,3F12.2)
         if (FCRL .eq. 1) &
         write(imsg,1669) ' RPS adjust in EFP', CURIYR,NRGN,ERRPS
         if (FCRL .eq. 1) &
         write(imsg,1669) ' CPP adjust in EFP', CURIYR,NRGN,ERCPPTR(NRGN,CURIYR)
1669     format(1x,a20,2I4,F12.2)
         LCFL=EFPFL

! replace epfblk with nominal values
         DO I=1,5
           EFPBLK(I,1) = EFPBLK(I,1)*MC_JPGDP(CURIYR)
         ENDDO

         EFPOM = ERTOM(1) + ERTOM(2)
         LCVOM = EFPOM
         EFPNUC = ERFFL(UIUF)
!        EFPNUC = 0.0
!
!        DETERMINE FIXED O&M FOR DISPATCHABLE, RENEWABLES AND INTERMITANT
!
         CALL GETBLD(1,NRGN)
!
         FOM_ALL = 0.0
         FOM_NUC = 0.0
         DO IOWN = 1 , 2
            DO IEFD = 1 , EFD_D_CAP
               FOM_ALL = FOM_ALL + ERTOMF(IEFD,IOWN) + ERTOMX(IEFD,IOWN)
               IF (IEFD .EQ. UICNU .OR. IEFD .EQ. UIANC .OR. IEFD .EQ. UISMR) THEN
                  FOM_NUC = FOM_NUC + ERTOMF(IEFD,IOWN) + ERTOMX(IEFD,IOWN)
               END IF
            END DO
         END DO
!
         LCFOM = FOM_ALL - FOM_NUC
!
!        ADD IN PRODUCTION ALLOCATED GNA AND FIXED O&M
!
!
!        DECREASE G&A EXPENSES FOR COMPETITION RUN for generation
!
            EFPOM = EFPOM + (GAADJ(1,NRGN,CURIYR)*OVERPR(NRGN))* &
               SALTOTOLD
            LCGNA = OVERPR(NRGN)*SALTOTOLD          ! no ga adj for valcap
!
         EFPOM = EFPOM + FOM_ALL


      ELSE
         EFPFL = 0.0
         BLKSUM = 0.0
         PPWRBLK(1)=0.0
         PPWRBLK(2)=0.0
         PPWRBLK(3)=0.0
         PPWRBLK(4)=0.0
         EFPNUC = 0.0
         IF (ICALL .EQ. 'T') THEN
!
!           CALCULATE O&M FOR TRANSMISSION EQUIPMENT
!
!           EFPOM = OMTSLP(NRGN) * OMADJTR(CURIYR)
            EFPOM = (OMTSLP(NRGN) &
               * (EQTLSL(1) + EQTLSL(2)) + &
               (OMTSLP2(NRGN) * NCPNEMS(NRGN)) + &
               OMTCON(NRGN) + OMTADDER(NRGN,CURIYR)) * OMTSCALE(NRGN,CURIYR)
             WRITE(18,2047)'OMTCLP ',NRGN,CURIYR,OMTSLP(NRGN),EQTLSL(1),EQTLSL(2), &
               OMTSLP2(NRGN), NCPNEMS(NRGN), OMTCON(NRGN), OMTADDER(NRGN,CURIYR), OMTSCALE(NRGN,CURIYR)
2047  FORMAT(A20,1x,2(I4,1x),10(F12.3,1x))
!     write(*,'(a,2i5,2f12.2)')"in GL, OMTSLP ",curiyr,nrgn,omtslp(nrgn)*  &
!                 (EQTLSL(1)+EQTLSL(2))
!     write(*,'(a,2i5,2f12.2)')"in GL, NCPeak ",curiyr,NRGN,NCPNEMS(NRGN)
!
!           DECREASE G&A EXPENSES FOR COMPETITION RUN for transmission
!
               EFPOM = EFPOM + (GAADJ(2,NRGN,CURIYR)*OVERTR(NRGN))* &
                  (EQTLSL(1) + EQTLSL(2))
         ELSE
!
!           CALCULATE O&M FOR DISTRIBUTION EQUIPMENT
!
!           EFPOM = OMDSLP(NRGN) * OMADJDS(CURIYR)
            EFPOM = (OMDSLP(NRGN) &
               * (EQTLSL(1) + EQTLSL(2)) + &
               (OMDSLP2(NRGN) * NCPNEMS(NRGN)) + &
               OMDCON(NRGN)) * OMDSCALE(NRGN,CURIYR)
             WRITE(18,2047)'OMDCLP ',NRGN,CURIYR,OMDSLP(NRGN),EQTLSL(1),EQTLSL(2), &
               OMDSLP2(NRGN), NCPNEMS(NRGN), OMDCON(NRGN),OMDSCALE(NRGN,CURIYR)             
!     write(*,'(a,2i5,2f12.2)')"in GL, OMDSLP ",curiyr,nrgn,omdslp(nrgn)*  &
!                 (EQTLSL(1)+EQTLSL(2))
!     write(*,'(a,2i5,2f12.2)')"in GL,NCPeak ",curiyr,NRGN,NCPNEMS(NRGN)
!
!           DECREASE G&A EXPENSES FOR COMPETITION RUN for distribution
!
                EFPOM = EFPOM + (GAADJ(3,NRGN,CURIYR)*OVERDS(NRGN)) * &
                  (EQTLSL(1) + EQTLSL(2))
!           ADJUST DISTRIBUTION COSTS FOR CARBON ALLOWANCES GIVEN TO LOAD ENTITIES
               IF (PCAP_CAR .EQ. 3 .OR. PCAP_CAR .EQ. 4) THEN
                CALL GETOUT(CURIYR,NRGN)
                EFPFL = EFPFL - ERDSCAR
               ENDIF

!      adjust distribution for CPP allocation costs, if mass-based
              IF (CO2_STDSW .EQ. 3 .AND. CO2_STDTN(NRGN) .EQ. 2) THEN
                 CALL GETOUT(CURIYR,NRGN)
                 EFPFL = EFPFL - ERDSCAR
              ENDIF

!      add 111d EE expenses to be recovered in distribution costs, ECSTNREE in 1987 billion $
                EFPFL = EFPFL + ECSTNREE(NRGN,CURIYR) * 1000.0
! !      add STEO benchmark adjustment adders right here  AKN - move to generation component - LC2
                ! IF ((BMELPRC(CURIYR) .GT. 0.0) .AND.(CURITR .GE. 2)) THEN
                        ! DO CENSUS = 1, MNUMCR-2
                            ! EFPFL = EFPFL - MappCtoN(NRGN,CENSUS,4) * BCHMRKSTEOPRICEADDER(CENSUS,CURIYR)/1000000
                        ! ENDDO
                ! ENDIF

!      add nuclear ZEC costs in distribution costs - 1987 million $, only nonzero if program is in place
               EFPFL = EFPFL + ERZECCST(NRGN)

         ENDIF

      ENDIF
      RETURN
      END

      SUBROUTINE GENERA(NRGN,ICALL)
      IMPLICIT NONE
!******************************************************************
! THIS SUBROUTINE INVOKES GENERATION ACCOUNTING COMPUTATIONS
!*******************************************************************
! INPUT VARIABLES:
!     ICALL = OPERATING COMPONENT IDENTIFICATION
!     NRGN = CURRENT REGION
!*******************************************************************
      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'control'
      include 'efpcntrl'
      include 'eusprc'
      include 'efpint'
      CHARACTER*1 ICALL
      INTEGER NRGN
      CALL AVRPRC(NRGN,ICALL)
! new comp pricing routine - call for all regions just to have output
!     IF ((USW_POL .eq. 4) .AND. (FRMARG(CURIYR,NRGN) .GT. 0.0))
      IF (USW_POL .eq. 4) &
          CALL COMPPRC(NRGN)
      RETURN
      END
      SUBROUTINE AVRPRC(NRGN,ICALL)
      IMPLICIT NONE
!*****************************************************************
!  THIS SUBROUTINE INVOKES AVERAGE COST PRICING COMPUTATIONS
!*****************************************************************
! INPUT VARIABLES:
!   NRGN = CURRENT REGION
!   ICALL = OPERATING COMPONENT IDENTIFICATION
!*****************************************************************
      INTEGER NRGN,n
      CHARACTER*1 ICALL
      include 'parametr'
      include 'ncntrl'
      include 'eusprc'
      include 'emmparm'
      include 'efpint'
      IF (CURITR .EQ. 1) THEN
         CALL ELADCR
         CALL ELCWIP(NRGN,ICALL)
         CALL ELND(NRGN,6,ICALL)
         CALL ELPHIN(NRGN)
         CALL ELSL(NRGN)
!        do n=1,2
!          erndfpmt(n)=0.0
!        enddo
         CALL ELITC
         CALL ELBKDP(NRGN,ICALL)
         CALL ELTXDP(NRGN,ICALL)
         CALL ELTSAF
      ENDIF
      CALL ELXPNS(NRGN,ICALL)
      CALL ELINEX(NRGN)
      CALL ELREVS(ICALL,NRGN)
      CALL ELRATE(ICALL,NRGN)
      IF (FCRL .EQ. 1) CALL STMTS(NRGN,ICALL)
      RETURN
      END
!******************************************************************
      SUBROUTINE COMPPRC(NRGN)
      implicit none
!*****************************************************************
!   THIS SUBROUTINE CALCULATES THE COMPETITIVE PRICE OF ELECTRICITY BY EFD SLICE USING MARGINAL COST PRICING
!      THIS IS ONLY CALLED FOR THE GENERATION COMPONENT
!*****************************************************************

      include 'parametr'
      include 'emmparm'
      include 'control'
      include 'eusprc'
      include 'efpint'
      include 'efprcy'
      include 'efprc'
      include 'efprp2'
      include 'efpout'
      include 'dispin'
      include 'dispett'
      include 'dispout'
      include 'dsmdimen'
      include 'dsmtfefp'
      include 'dsmtoefd'
      include 'ncntrl'
      include 'macout'
      include 'dsmsectr'
      include 'dsmunits'
      include 'dsmnercr'
      include 'bildout'
      include 'ecpcntl'
      include 'uecpout'
      include 'uefdout'
      include 'elout'
      include 'udatout'
      include 'emission'
      include 'cdsparms'
      include 'csapr'
      include 'emmemis'


      REAL*4 EMBCST(2)
      REAL*4 TAXADJ(2),fixgna,tempadj(MNUMYR)
      REAL*4 MEC,REL,OTH,EMBC,SLCPRC,ADJ,SLS,tempmec,HRS
      INTEGER  ISEA,ISLC,IOWN,NRGN,I,J,IEFD
      INTEGER igrp,iseg,newslc,b1,b2,iblk,k,ise,MTOTEUSGRP
      INTEGER s,sblk,b,N,IY
      REAL*8 FOM_ALL,FOM_NUC
      REAL*4 tempprc(EFD_D_MSP,EFD_D_MVS,2)
      REAL*4 temprev,stdcst,FINREV,totstd,inctax,othtax
      REAL sortefdblk(EFD_D_MSP,EFD_D_MVS,MNEUGRP)
      REAL slcload(EFD_D_MSP,EFD_D_MVS)
      REAL totload,mrprc,co2prc,bmprc,STEOPRC_ADJ
      REAL TOTCRED,achvlev
      REAL*4 BKITAX(2),DEDINT,STFTAX,ADJBDE,ADJADC,STTAX,ERGNTX(2)
      REAL*4 ORIGMECC(MNUMYR,MNUMNR,EFD_D_MSP,EFD_D_MVS)
      REAL*4 INFL1,INFL2,INFL3,INFL4

      COMMON /BCHMRKSTEOPRICE/ BCHMRKSTEOPRICEADDER, BCHMRKSTEOPRC_N   !STEO Price benchmark feature --- by AKN
      REAL BCHMRKSTEOPRICEADDER(MNUMCR,MNUMYR), BCHMRKSTEOPRC_N(MNUMNR,MNUMYR)  !STEO benchmark price adjustment adder --- by AKN

      DATA tempadj/ 1.00,1.00,1.00,1.00,1.00,1.00,0.98,0.95,0.93, &
          0.90,0.88,0.85,0.83,0.80,0.77,0.75,0.75,0.75,0.75,0.75, &
          0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,30*0.75/

!  do regulated tax calculations (needed for stranded cost)
         DO N=1,2
            ERGNTX(N) = ERRVLG(N) &
                        *EGTXRT(N) + ERPRTX(N) + ERSLTX(N)
            BKITAX(N) = 0.0
            IF (N .NE. 2) THEN
               DEDINT = ERTIEX(N) - ERCIDC(N)
               STFTAX = (ERRVLG(N) - ERTFLN(N) - ERTOMN(N)-ERNDFPMT(N) - &
                        ESLLP(N) - ERBDE(N) - ERGNTX(N) - DEDINT + &
                        ERFDCE(N))*ESFTXR(N)
               ADJBDE = (ERBDE(N) - ERTDWO(N))*ESFTXR(N)
               ADJADC = ERFDCE(N)*ESFTXR(N)
               STTAX = (ERRVLG(N) - ERTFLN(N) - ESLLP(N) - &
                       ERTOMN(N) - ERTDWO(N) - ERTIEX(N))*ESSTXR(N)* &
                       (1 - ESFTXR(N))
               BKITAX(N) = STFTAX + ADJBDE - ADJADC &
                        - ERAFDC(N) - ERFFDC(N) - EREDTF(N) &
                        - ERAITC(N) - ERFITC(N) - ERATSF(N) + STTAX

               IF (isnan(BKITAX(N))) &
                  WRITE(6,6331) CURIRUN, CURIYR+1989, CURITR, NRGN, N, &
                  BKITAX(N), STFTAX, ADJBDE, ADJADC, ERAFDC(N), ERFFDC(N), EREDTF(N), ERAITC(N), ERFITC(N), ERATSF(N), STTAX
 6331          FORMAT(1X,"BKITAX_DBG",5(":",I4),11(":",F15.3))

            ENDIF
         ENDDO

!  get load per slice
      do i = 1,EFD_D_MSP
       do j = 1,EFD_D_MVS
         slcload(i,j) = 0.0
       enddo
      enddo
      totload = 0.0
      MTOTEUSGRP = NEUSGRP(1) + NEUSGRP(2)+ NEUSGRP(3) + NEUSGRP(4)

!  fill sortefdblk - efdblkserv data but in slice sorted order - to match other components

      DO k=1,MTOTEUSGRP
         DO ise=1,EFDns    ! seasons
        b1=EFDSEDEF(ise,1)  ! starting and ending block in season
        b2=EFDSEDEF(ise,2)
        DO iblk=b1,b2
          b=iblk-b1+1
          sblk = GBlockNumEFD(iblk,NRGN)   ! sorted by from top to bottom
          s = sblk-b1+1
          sortefdblk(ise,b,k) = efdblkserv(b,ise,NRGN,k)
          slcload(ise,b) = slcload(ise,b) + sortefdblk(ise,b,k)
          totload = totload + sortefdblk(ise,b,k)

!            WRITE(6,3911) CURIRUN, CURIYR+1989, CURITR, NRGN, k, ISE, iblk, b, sblk, s, sortefdblk(ise,b,k), efdblkserv(b,ise,NRGN,k), slcload(ise,b), sortefdblk(ise,b,k), totload
!3911        FORMAT(1X,"SLCLOAD_UEFP",10(":",I5),5(":",F21.5))

        ENDDO
      ENDDO
      ENDDO

!  fill mrun price adjustment
     IF (TOTLOAD .GT. 0.0) THEN
      MRPRC = ULMRCST(NRGN) / QELASN(NRGN,CURIYR) * 1000.0
     ELSE
      MRPRC = 0.0
     ENDIF
     IF (FCRL .EQ. 1) write(22,326) CURIYR,NRGN,ULMRCST(NRGN),totload,QELASN(NRGN,CURIYR),MRPRC
326  FORMAT(1X,'MRPRC ',2I6,3F10.2,F14.5)

!  fill STEO benchmark price adjustment - based on impact of generation/consumption constraints on load row duals
      BMPRC = ULBMCST / QELASN(MNUMNR,CURIYR) * 1000.0       ! same adder for all regions based on national cost
      IF (FCRL .EQ. 1 .AND. BMPRC .NE. 0.0) write(22,327) CURIYR,NRGN,ULBMCST,QELASN(MNUMNR,CURIYR),BMPRC
327  FORMAT(1X,'BMPRC ',2I6,2F14.2,F14.5)

! add STEO price benchmarking adj - based on difference between EMM/STEO prices - added to both regulated fuel and competitive energy to show up in generation price
      STEOPRC_ADJ = 0.0
      IF (CURITR .GE. 2 .AND. BMELPRC(CURIYR) .GT. 0) THEN
         STEOPRC_ADJ = BCHMRKSTEOPRC_N(NRGN,CURIYR)/1000.0 / QELASN(NRGN,CURIYR)
      ENDIF
      IF (FCRL .EQ. 1 .AND. STEOPRC_ADJ .NE. 0.0) write(22,328) CURIYR,NRGN,BCHMRKSTEOPRC_N(NRGN,CURIYR)/1000.0,QELASN(NRGN,CURIYR),STEOPRC_ADJ
328  FORMAT(1X,'STEOPRC ',2I6,2F14.2,F14.5)
!
      FOM_ALL = 0.0
      FOM_NUC = 0.0
      DO IOWN = 1 , 2
         DO IEFD = 1 , EFD_D_CAP
            FOM_ALL = FOM_ALL + ERTOMF(IEFD,IOWN)
            IF (IEFD .EQ. UICNU .OR. IEFD .EQ. UIANC  .OR. IEFD .EQ. UISMR) THEN
               FOM_NUC = FOM_NUC + ERTOMF(IEFD,IOWN)
            END IF
         END DO
      END DO
!
      DO IOWN = 1,2

!        don't include fixed costs

!        fixgna = (FOM_ALL-FOM_NUC)*ESTSHR(IOWN)/EQTLSL(IOWN) + &
!            OVERPR(NRGN)*tempadj(CURIYR)
!        ERFXGNAC(CURIYR,NRGN,IOWN) = fixgna*MC_JPGDP(CURIYR)

         ERFXGNAC(CURIYR,NRGN,IOWN) = 0.0

!        EMBCST(IOWN) = ERTFLN(IOWN) + ERTOMN(IOWN) + ERNDFPMT(IOWN) +
!           ERTDRG(IOWN) + (ERTIEX(IOWN) - ERCIDC(IOWN)) + ESLLP(IOWN)

         EMBCST(IOWN) = ERTFLN(IOWN) + ERTOMN(IOWN) + ERNDFPMT(IOWN) + &
            ERTDRG(IOWN) + ERTIEX(IOWN) + ESLLP(IOWN)
         TAXADJ(IOWN) = ERFITC(IOWN) + EREDTF(IOWN) + ERAITC(IOWN) + &
            ERFFDC(IOWN) + ERAFDC(IOWN)
      ENDDO
      IY = CURIYR
      achvlev = RPSACHV(CURIYR)/QELASN(MNUMNR,CURIYR)
      IF( FCRL .eq. 1)THEN
        write(imsg,*)'RPSGOAL',RPSGOAL(CURIYR),' RPSACHV', &
          RPSACHV(CURIYR), 'sales ',QELASN(MNUMNR,CURIYR)
       if(nrgn .eq. 1)write(imsg,2222) CURIYR+1989,EPRPSPR(CURIYR), &
        UPRNWBND(CURIYR),achvlev,renewcr(CURIYR),credcost(CURIYR)
2222  format(1h ,'credit bound and calc cred',i4,f8.3,f8.3,f8.3, &
        f8.3,f10.3)
      ENDIF
      if (achvlev .gt. UPRNWBND(CURIYR)) achvlev = UPRNWBND(CURIYR)


      DO ISEA = 1, EFDns
       DO ISLC = 1, ELNVCT(ISEA)

       if (NMARCST(NRGN,ISEA,ISLC,CURIYR) .eq. 80.0) then
          tempmec = MARCST2(ISEA,ISLC)
       else
          tempmec = NMARCST(NRGN,ISEA,ISLC,CURIYR) + MRPRC + BMPRC - STEOPRC_ADJ
         IF (CURIYR .GT. 30 .AND. CURIYR .LE. 35) write(22,329) CURIYR, CURITR, ISEA, ISLC, NRGN, NMARCST(NRGN,ISEA,ISLC,CURIYR), MRPRC, BMPRC, STEOPRC_ADJ         
       endif
329  FORMAT(1X,'NMARCST ',5I6,4F14.5)

!  NMARCST includes renewable credit, passed through EFD
!       tempmec = tempmec + renewcr(CURIYR) * UPRNWBND(CURIYR)
!       tempmec = tempmec + renewcr(CURIYR) * achvlev
!  get correct slice number to match demand slices
!      igrp = mod(ulgrp(ISLC,ISEA,NRGN),3)
!      if (igrp .eq. 0) igrp = 3
!      iseg = ulseg(ISLC,ISEA,NRGN)
!      newslc = (igrp-1)*6 + iseg
!      write(6,*) 'COMPPRC  yr rgn sea slc ',CURIYR,NRGN,ISEA,newslc
!  convert to nominal $
!      ERMECC(CURIYR,NRGN,ISEA,newslc) = tempmec*MC_JPGDP(CURIYR)

! adjust energy for losses
      ERMECC(CURIYR,NRGN,ISEA,ISLC) = tempmec*MC_JPGDP(CURIYR)* &
       ( 1.0 +  NERCtdloss(NRGN)*ULOSSADJ(CURIYR))


!adjust energy cost for uplift charges


! adjust energy cost for congestion charges
  IF (UNRGNS .EQ. 22) THEN
  ! PJM Regions 9 & 11
         IF (NRGN .EQ. 9) THEN
          ERMECC(CURIYR,NRGN,ISEA,ISLC)= ERMECC(CURIYR,NRGN,ISEA,ISLC) + 10
         ENDIF

         IF (NRGN .EQ. 11) THEN
          ERMECC(CURIYR,NRGN,ISEA,ISLC)= ERMECC(CURIYR,NRGN,ISEA,ISLC) + 5.0
         ENDIF

 !CA SURCHARGE applicable to customers buying competitive energy + 1.0 for CAISO uplift
   !and ancillary services

         IF ((CURIYR .lt. 34) .AND. (NRGN .EQ. 20)) THEN
           ERMECC(CURIYR,NRGN,ISEA,ISLC)= (ERMECC(CURIYR,NRGN,ISEA,ISLC))+ 1.0
         ENDIF
  ELSEIF (UNRGNS .EQ. 25) THEN
     ! PJM Regions 10 & 11 & 12 (leave out Dominion?)
       IF (NRGN .EQ. 10 ) THEN
             ERMECC(CURIYR,NRGN,ISEA,ISLC)= ERMECC(CURIYR,NRGN,ISEA,ISLC) + 10
       ENDIF
   
       IF (NRGN .EQ. 11 .OR. NRGN .EQ. 12) THEN
          ERMECC(CURIYR,NRGN,ISEA,ISLC)= ERMECC(CURIYR,NRGN,ISEA,ISLC) + 5.0
       ENDIF
   
    !CA SURCHARGE applicable to customers buying competitive energy + 1.0 for CAISO uplift
      !and ancillary services
   
            IF ((CURIYR .lt. 34) .AND. (NRGN .EQ. 21 .OR. NRGN .EQ. 22)) THEN
              ERMECC(CURIYR,NRGN,ISEA,ISLC)= (ERMECC(CURIYR,NRGN,ISEA,ISLC))+ 1.0
            ENDIF
  ENDIF  !UNRGNS


!End of Regional ERMECC adders

      ORIGMECC(CURIYR,NRGN,ISEA,ISLC) = ERMECC(CURIYR,NRGN,ISEA,ISLC)

!   account for lag if appropriate
!   first reassign lag value if not enough history

      IF (ERMECLAG(CURIYR,NRGN) .GT. 0.0 .AND. ERMECLAG(CURIYR,NRGN) .LT. 1.01) THEN
           IF ((CURIYR+UHBSYR) .EQ. UESTYR) ERMECLAG(CURIYR,NRGN) = 0
      ELSE IF (ERMECLAG(CURIYR,NRGN) .LT. 2.01) THEN
           IF ((CURIYR + UHBSYR) .LT. UESTYR + 2) ERMECLAG(CURIYR,NRGN) = 0
      ELSE IF (ERMECLAG(CURIYR,NRGN) .LT. 3.01) THEN
           IF ((CURIYR + UHBSYR) .LT. UESTYR + 4) ERMECLAG(CURIYR,NRGN) = 0
      ENDIF

      IF ((BMELPRC(CURIYR) .EQ. 0.0) .AND. (ERMECLAG(CURIYR,NRGN) .GT. 0.0)) THEN
        INFL1 = MC_JPGDP(CURIYR)/MC_JPGDP(CURIYR-1)
        INFL2 = MC_JPGDP(CURIYR)/MC_JPGDP(CURIYR-2)
        INFL3 = MC_JPGDP(CURIYR)/MC_JPGDP(CURIYR-3)
        INFL4 = MC_JPGDP(CURIYR)/MC_JPGDP(CURIYR-4)
         IF (ERMECLAG(CURIYR,NRGN) .LT. 1.01) THEN
            ERMECC(CURIYR,NRGN,ISEA,ISLC) = ERMECLAG(CURIYR,NRGN) * ORIGMECC(CURIYR-1,NRGN,ISEA,ISLC) * INFL1 + &
                  (1.0 - ERMECLAG(CURIYR,NRGN)) * ORIGMECC(CURIYR,NRGN,ISEA,ISLC)
         ELSEIF (ERMECLAG(CURIYR,NRGN) .LT. 2.01) THEN  ! 3 year rolling average
            ERMECC(CURIYR,NRGN,ISEA,ISLC) = (ORIGMECC(CURIYR - 2,NRGN,ISEA,ISLC) * INFL2  +   &
              ORIGMECC(CURIYR - 1,NRGN,ISEA,ISLC) * INFL1 + ORIGMECC(CURIYR,NRGN,ISEA,ISLC) ) / 3.0
         ELSEIF (ERMECLAG(CURIYR,NRGN) .GT. 2.00) THEN ! 5 year rolling average
                          ERMECC(CURIYR,NRGN,ISEA,ISLC) = (ORIGMECC(CURIYR - 4,NRGN,ISEA,ISLC) * INFL4 +   &
              ORIGMECC(CURIYR - 3,NRGN,ISEA,ISLC) * INFL3 + ORIGMECC(CURIYR - 2,NRGN,ISEA,ISLC) * INFL2 + &
              ORIGMECC(CURIYR - 1,NRGN,ISEA,ISLC) * INFL1 + ORIGMECC(CURIYR,NRGN,ISEA,ISLC) ) / 5.0
         ENDIF
      ENDIF


      ENDDO
      ENDDO

      totstd = 0.0
      EWSPRCN(NRGN,CURIYR) = 0.0
      DO IOWN = 1,2
         OTH = ERFXGNAC(CURIYR,NRGN,IOWN)
         EMBC = EMBCST(IOWN)
         ADJ = TAXADJ(IOWN)
         temprev = 0.0

         DO ISEA = 1, EFDns
            DO ISLC = 1, ELNVCT(ISEA)
               MEC = ERMECC(CURIYR,NRGN,ISEA,ISLC)

!              if (MEC .eq. 0.0) write (6,*) 'mec = 0.0 '

               SLS = slcload(ISEA,ISLC) * 0.001 * ESTSHR(IOWN)
               HRS = ULWDTH(ISLC,ISEA,NRGN)
               IF (IOWN .EQ. 1) THEN
                  EWSPRCN(NRGN,CURIYR) = EWSPRCN(NRGN,CURIYR) +(MEC) * HRS
               ENDIF

               tempprc(ISEA,ISLC,IOWN) = REL + MEC + OTH
               temprev = temprev + tempprc(ISEA,ISLC,IOWN) * SLS

!              WRITE(6,3711) CURIRUN, CURIYR+1989, CURITR, NRGN, IOWN, ISEA, ISLC, EWSPRCN(NRGN,CURIYR), ERMECC(CURIYR,NRGN,ISEA,ISLC), &
!                 SLS, slcload(ISEA,ISLC), ESTSHR(IOWN), ULWDTH(ISLC,ISEA,NRGN), tempprc(ISEA,ISLC,IOWN), REL, MEC, OTH, temprev
!3711          FORMAT(1X,"EWSPRCN_UEFP",7(":",I4),11(":",F21.6))

            ENDDO         !ISLC LOOP
         ENDDO         !ISEA LOOP




         stdcst = temprev-(ERRVRQ(IOWN)-ERGNTX(IOWN)-BKITAX(IOWN))

!        write(6,99) 'revs/costs ',CURIYR,NRGN,IOWN,temprev,EMBC,
!        +      ERRVRQ(IOWN), ERGNTX(IOWN),BKITAX(IOWN)

!        stranded costs in billion nominal dollars are now in finreg

         SCSANN(CURIYR,NRGN,IOWN) = SCSREC(CURIYR,NRGN) * 1000.0 * ESTSHR(IOWN)

  99     format(a15,3i4,5f10.2)

!        c new 9/10/97 - no income tax in competitive price
!        FINREV=(temprev - EMBC*ESFTXR(IOWN) -  ADJ +
!           SCSANN(CURIYR,NRGN,IOWN))/
!           (1.0 - ESFTXR(IOWN)*(1.0 - EGTXRT(IOWN)) - EGTXRT(IOWN))
!        othtax = FINREV*EGTXRT(IOWN)
!        inctax = (FINREV - (EMBC + othtax)) * ESFTXR(IOWN)  -  ADJ
!        IF (inctax .lt. 0.0) then
!           FINREV=(temprev + SCSANN(CURIYR,NRGN,IOWN))/(1.0-EGTXRT(IOWN))
!           othtax = FINREV*EGTXRT(IOWN)
!           inctax = 0.0
!        ENDIF

!        new 9/10 - use regulated income tax

         inctax = BKITAX(IOWN)
         inctax=0.0
         FINREV = (temprev + inctax + SCSANN(CURIYR,NRGN,IOWN))/ &
            (1.0 - ECTXRT(IOWN))
         othtax = FINREV*ECTXRT(IOWN)

!        convert taxes to components of price

         IF (EQTLSL(IOWN) .GT. 0.0) THEN
            EROTAXC(CURIYR,NRGN,IOWN)=othtax/EQTLSL(IOWN)
            ERITAXC(CURIYR,NRGN,IOWN)=inctax/EQTLSL(IOWN)
            SCSPRC(CURIYR,NRGN,IOWN) = SCSANN(CURIYR,NRGN,IOWN)/EQTLSL(IOWN)
         ELSE
            EROTAXC(CURIYR,NRGN,IOWN) = 0.0
            ERITAXC(CURIYR,NRGN,IOWN) = 0.0
            SCSPRC(CURIYR,NRGN,IOWN)  = 0.0
         END IF

      ENDDO  ! IOWN LOOP

      if (fcrl .eq. 1) &
         write(imsg,*) 'full stdcst - 95$ ',CURIYR,NRGN, &
         totstd*MC_JPGDP(6)/MC_JPGDP(CURIYR)

      EWSPRCN(NRGN,CURIYR) = EWSPRCN(NRGN,CURIYR) / 8760.0 / MC_JPGDP(CURIYR)
      write(18,*)'ewsprcn',EWSPRCN(NRGN,CURIYR) , RMAVG(CURIYR,NRGN)
      EWSPRCN(NRGN,CURIYR) = EWSPRCN(NRGN,CURIYR) + (RMAVG(CURIYR,NRGN) / 8.760 )
      write(18,*)'ewsprcn af',EWSPRCN(NRGN,CURIYR)
      EWSPRCN(NRGN,CURIYR) = EWSPRCN(NRGN,CURIYR) - EPRPSPR(CURIYR) * UPRNWBND(CURIYR)

!     calculate final slice prices, with taxes and stranded cost recovery and 1/2-yr lag

      DO ISEA = 1, EFDns
         DO ISLC = 1, ELNVCT(ISEA)

            DO IOWN = 1, 2

!              IF (curiyr .lt. 18 .AND. nrgn .eq. 2 &
!                 .OR. nrgn .eq. 3 .OR. nrgn .eq. 7 .OR. nrgn .eq. 11) then
!                 ERSLCPRC(CURIYR,NRGN,ISEA,ISLC,IOWN)= &
!                 ERMECC(CURIYR-1,NRGN,ISEA,ISLC) + &
!                 ERRELC(CURIYR,NRGN,ISEA,ISLC) + ERFXGNAC(CURIYR,NRGN,IOWN)+ &
!                 EROTAXC(CURIYR,NRGN,IOWN) + ERITAXC(CURIYR,NRGN,IOWN) + &
!                 SCSPRC(CURIYR,NRGN,IOWN)
!              else
!                 ERSLCPRC(CURIYR,NRGN,ISEA,ISLC,IOWN)= &
!                 (0.5 * ERMECC(CURIYR-1,NRGN,ISEA,ISLC)) + &
!                 (0.5 * ERMECC(CURIYR,NRGN,ISEA,ISLC)) + &
!                 ERRELC(CURIYR,NRGN,ISEA,ISLC) + ERFXGNAC(CURIYR,NRGN,IOWN)+ &
!                 EROTAXC(CURIYR,NRGN,IOWN) + ERITAXC(CURIYR,NRGN,IOWN) + &
!                 SCSPRC(CURIYR,NRGN,IOWN)
!              endif

!              commented code above assumes a lag in energy prices passed through to total prices
!              current code (below) assumes no lag at all

               ERSLCPRC(CURIYR,NRGN,ISEA,ISLC,IOWN)= &
                  ERMECC(CURIYR,NRGN,ISEA,ISLC) + &
                  ERFXGNAC(CURIYR,NRGN,IOWN)+ &
                  EROTAXC(CURIYR,NRGN,IOWN) + ERITAXC(CURIYR,NRGN,IOWN) + &
                  SCSPRC(CURIYR,NRGN,IOWN)

               IF (isnan(ERSLCPRC(CURIYR,NRGN,ISEA,ISLC,IOWN))) THEN
                  WRITE(6,8301) CURIRUN, CURIYR+1989, CURITR, NRGN, ISEA, ISLC, IOWN, &
                     ERSLCPRC(CURIYR,NRGN,ISEA,ISLC,IOWN), ERMECC(CURIYR,NRGN,ISEA,ISLC), ERRELC(CURIYR,NRGN,ISEA,ISLC), &
                     ERFXGNAC(CURIYR,NRGN,IOWN), EROTAXC(CURIYR,NRGN,IOWN), ERITAXC(CURIYR,NRGN,IOWN), SCSPRC(CURIYR,NRGN,IOWN)
 8301             FORMAT(1X,"ERSLCPRC_NAN", 7(":",I4),25(":",F12.3))
               END IF

            ENDDO

!           average public & private

            ERSLCPRC(CURIYR,NRGN,ISEA,ISLC,3) = &
               ERSLCPRC(CURIYR,NRGN,ISEA,ISLC,1) * ESTSHR(1) &
               +  ERSLCPRC(CURIYR,NRGN,ISEA,ISLC,2) * ESTSHR(2)
            EROTAXC(CURIYR,NRGN,3) = &
               EROTAXC(CURIYR,NRGN,1) * ESTSHR(1) &
               +  EROTAXC(CURIYR,NRGN,2) * ESTSHR(2)
            ERITAXC(CURIYR,NRGN, 3) = &
               ERITAXC(CURIYR,NRGN,1) * ESTSHR(1) &
               +  ERITAXC(CURIYR,NRGN,2) * ESTSHR(2)
            SCSPRC(CURIYR,NRGN,3) = &
               SCSPRC(CURIYR,NRGN,1) * ESTSHR(1) &
               +  SCSPRC(CURIYR,NRGN,2) * ESTSHR(2)

!           slice price - too much output  - put on switch

            IF ((FCRL.eq.1) .and. (CURIYR.gt.0) .and. (PRTDBGE.ge.3)) THEN
               write(imsg,1111) CURIYR,CURITR,NRGN,ISEA, &
                  (ERSLCPRC(CURIYR,NRGN,ISEA,ISLC,I)/MC_JPGDP(CURIYR),I=1,3), &
                  ERRELC(CURIYR,NRGN,ISEA,ISLC)/MC_JPGDP(CURIYR), &
                  ERMECC(CURIYR,NRGN,ISEA,ISLC)/MC_JPGDP(CURIYR), &
                  ERFXGNAC(CURIYR,NRGN,1)/MC_JPGDP(CURIYR), &
                  ERITAXC(CURIYR,NRGN,1)/MC_JPGDP(CURIYR), &
                  ERITAXC(CURIYR,NRGN,3)/MC_JPGDP(CURIYR), &
                  (EROTAXC(CURIYR,NRGN,I)/MC_JPGDP(CURIYR),I=1,3), &
                  SCSPRC(CURIYR,NRGN,3)/MC_JPGDP(CURIYR), &
                  ORIGMECC(CURIYR,NRGN,ISEA,ISLC)/MC_JPGDP(CURIYR)
 1111          format('COMPPRC  ',4i4,13f10.4)
            endif

!           fill in slice price variable used for endogenous retirements
!           ELGENP(ISLC,ISEA,NRGN) = ( ERSLCPRC(CURIYR,NRGN,ISEA,ISLC,3)
!              +            - ERITAXC(CURIYR,NRGN,3) - EROTAXC(CURIYR,NRGN,3) )/
!              +       MC_JPGDP(CURIYR)

            ELGENP(ISLC,ISEA,NRGN) = ERMECC(CURIYR,NRGN,ISEA,ISLC) &
                / &
               MC_JPGDP(CURIYR)

         ENDDO         !ISLC LOOP
      ENDDO         !ISEA LOOP

      CALL RATES2(NRGN)      !calculate end use service prices

      RETURN
      END

      SUBROUTINE RATES2(NRGN)
      IMPLICIT NONE
!*****************************************************************
!   THIS SUBROUTINE CALCULATES THE AVERAGE COMPETITIVE PRICE BY END USE SERVICE
!     It is calculated by weighting slice price by efdblkserv for each end use service
!     identified. (Currently 23 - identified in eusgrp). efdblkserv represents the
!     percent of total load for the service accounted for in the given efd time slice.
!*****************************************************************
      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'dsmdimen'
      include 'dsmsectr'
      include 'dsmtoefd'
      include 'eusprc'
      include 'efpint'
      include 'efpout'
      include 'efprp2'
      include 'efprcy'
      include 'dsmnercr'
      include 'dispett'
      include 'dispin'
      include 'dsmtfefp'
      include 'uecpout'
      include 'macout'
      include 'udatout'
      include 'emission'
      include 'cdsparms'
      include 'csapr'
      include 'emmemis'
      include 'ecp_nuc'
      include 'emm_aimms'

      INTEGER NRGN,I,k,j,icls,ise,isl,s,b,sblk,iblk,b1,b2
      REAL sortefdblk(EFD_D_MSP,EFD_D_MVS,MNEUGRP),sortefdsum(EFD_D_MSP,EFD_D_MVS)
      real tmpsort(18,6),TMPRM(5),TMPPCP(5),totload,CO2PRC
      real avgrel(MNEUGRP),avgmec(MNEUGRP),avgfix(MNEUGRP), &
           avgitx(MNEUGRP),avgotx(MNEUGRP),avgrelo(MNEUGRP)
      real totallnr(5),demwtnr(MNEUGRP),relcomp(5),meccomp(5)
      real fixcomp(5),taxcomp(5),prcall(5)
      integer l,sectr,eu, MTOTEUSGRP

!     write(6,*) 'in rates2',CURIYR,NRGN,ERSLCPRC(CURIYR,NRGN,1,1,3)

! initialize array
      DO I = 1, MNEUGRP
       EPRIC2(I,NRGN) = 0.0
       avgrel(I) = 0.0
       avgrelo(I) = 0.0
       avgmec(I) = 0.0
       avgfix(I) = 0.0
       avgitx(I) = 0.0
       avgotx(I) = 0.0
      ENDDO

      totload = 0.0
      MTOTEUSGRP = NEUSGRP(1) + NEUSGRP(2)+ NEUSGRP(3) + NEUSGRP(4)
!  fill sortefdblk - efdblkserv data but in slice sorted order - to match other components
      do k=1,6
        do i=1,18
        tmpsort(i,k) = 0.0
      enddo
      enddo
      DO k=1,MTOTEUSGRP
      DO ise=1,EFDns    ! seasons
        b1=EFDSEDEF(ise,1)  ! starting and ending block in season
        b2=EFDSEDEF(ise,2)
        DO iblk=b1,b2
          b=iblk-b1+1
          sblk = GBlockNumEFD(iblk,NRGN)   ! sorted by from top to bottom
          s = sblk-b1+1
          sortefdblk(ise,b,k) = efdblkserv(b,ise,NRGN,k)
          totload = totload + sortefdblk(ise,b,k)
        ENDDO
      ENDDO
      ENDDO

!  fill CO2 capacity price adjustment
     IF (TOTLOAD .GT. 0.0) THEN
      CO2PRC = ULCO2CST(NRGN,CURIYR) / QELASN(NRGN,CURIYR) * 1000.0
     ELSE
      CO2PRC = 0.0
     ENDIF
     IF (FCRL .EQ. 1) write(22,328) CURIYR,NRGN,ULCO2CST(NRGN,CURIYR),QELASN(NRGN,CURIYR),CO2PRC
328  FORMAT(1X,'CO2PRC ',2I6,2F10.2,F14.5)

!     if (curiyr .eq. 6 .and. NRGN .eq. 7) then
!     do k = 1,MNEUGRP
!     write(6,1212) CURIYR,NRGN,k,
!         ((sortefdblk(ise,b,k)/ULWDTH(b,ise,NRGN),b=1,18),ise=1,6),
!          TOTEULOAD(k,NRGN)
!c    write(6,1214) CURIYR,NRGN,k,
!         ((efdblkserv(b,ise,NRGN,k),b=1,18),ise=1,6),TOTEULOAD(k,NRGN)
!     write(6,1213) CURIYR,NRGN,k,
!         (GBlockNumEFD(ise,NRGN),ise=1,108)
!   write out sum of curve
!       do ise=1,6
!        do b = 1,18
!      tmpsort(b,ise) = tmpsort(b,ise) + sortefdblk(ise,b,k)
!          /ULWDTH(b,ise,NRGN)   ! sortefd contains load*   TOTEULOAD(k,NRGN)
!       enddo
!        enddo
!        enddo   ! mneugrp
!     write(6,1215) CURIYR,NRGN,
!         ((ULHGHT(b,ise,NRGN),b=1,18),ise=1,6)
!     write(6,1215) CURIYR,NRGN,
!         ((tmpsort(b,ise),b=1,18),ise=1,6)

1212  format('sortefd ',3i4,108e12.4,f20.4)
1214  format('efdblkserv ',3i4,108e12.4,f20.4)
1213  format('gblocknum ',111(i3,1x))
1215  format('efd ldc  ',2i4,108(1x,e12.4,1x,e12.4))
!     endif

!     do k = 1,MNEUGRP
!     write(6,1212) CURIYR,NRGN,k,
!         ((sortefdblk(ise,b,k),b=1,18),ise=1,6)
!     enddo
!1212  format('sortefd ',3i4,108f7.4)

      DO icls = 1, NCLASS+1
        TMPPCP(icls) = 0.0
        TMPRM(icls) = 0.0
      ENDDO
      DO icls = 1, NCLASS
         TMPPCP(5) = TMPPCP(5) + SECANNPEAAVPCP(NRGN,ICLS)
         IF (fcrl.EQ.1) WRITE(18,8400)'tmppcp5 ',CURIYR,NRGN,icls,TMPPCP(5), SECANNPEAAVPCP(NRGN,ICLS)
8400  format(1x,A10,3I4,2F12.3)
      ENDDO
      DO icls = 1, NCLASS
         TMPPCP(icls) =  SECANNPEAAVPCP(NRGN,ICLS) / TMPPCP(5)
         IF(TMPPCP(icls) .lt.0.0) THEN
             WRITE(6,*)'secannpeaavgpcp less than zero ',NRGN,ICLS,TMPPCP(icls) , SECANNPEAAVPCP(NRGN,ICLS), TMPPCP(5)
             TMPPCP(icls) = 0.0
         ENDIF
         IF (fcrl.EQ.1) WRITE(18,8401)'tmpcp1-4 ',CURIYR,NRGN,icls,TMPPCP(icls), SECANNPEAAVPCP(NRGN,ICLS), TMPPCP(5)
8401  format(1x,A10,3I4,3F12.3)
      ENDDO
      k = 1
      DO icls = 1, NCLASS
      !!!! Northeast adders--used only to assure against huge price dips from STEO prices!!!! -LA2
      !New England

      !IF ((SALCLS(NRGN,ICLS) .GT. 0.0) .AND. (NRGN .eq. 5) .AND. (CURIYR .EQ. 29)) THEN
      !   TMPRM(ICLS) = ((RMPOOL(CURIYR,NRGN)*MC_JPGDP(CURIYR) * TMPPCP(icls)) / SALCLS(NRGN,ICLS) + CO2PRC * MC_JPGDP(CURIYR))* 3
      ! ELSEIF ((SALCLS(NRGN,ICLS) .GT. 0.0) .and. (NRGN .eq. 5) .AND. (CURIYR .EQ. 30)) THEN
      !   TMPRM(ICLS) = ((RMPOOL(CURIYR,NRGN)*MC_JPGDP(CURIYR) * TMPPCP(icls)) / SALCLS(NRGN,ICLS) + CO2PRC * MC_JPGDP(CURIYR))* 2

      !!!!End Northeast adders!!!!

       IF (SALCLS(NRGN,ICLS) .GT. 0.0) THEN
         TMPRM(ICLS) = ((RMPOOL(CURIYR,NRGN)+SRPOOL(CURIYR,NRGN))*MC_JPGDP(CURIYR) * TMPPCP(icls)) / SALCLS(NRGN,ICLS) + CO2PRC * MC_JPGDP(CURIYR)
       ELSE
         TMPRM(ICLS) = 0.0
      ENDIF

       IF (fcrl.EQ.1) WRITE(18,8402)'check tmprm ',CURIYR,NRGN,ICLS,K, TMPRM(ICLS), RMPOOL(CURIYR,NRGN), SRPOOL(CURIYR,NRGN), TMPPCP(icls),SALCLS(NRGN,ICLS),MC_JPGDP(CURIYR),CO2PRC
8402  FORMAT(A25,1x,4(I4,1x),7(F12.6,1x))

       DO j = 1, neusgrp(icls)
        DO ise = 1, EFDns
         DO ISL = 1, ELNVCT(ISE)
           sortefdsum(ise,isl) = sortefdsum(ise,isl) + sortefdblk(ise,isl,k)
           IF (fcrl.EQ.1) write(18,4014)'sortedsum ',curiyr,nrgn,K,ise,isl,sortefdsum(ise,isl),sortefdblk(ise,isl,k)
         ENDDO
        ENDDO
       ENDDO


       DO j = 1, neusgrp(icls)

        DO ise = 1, EFDns
         DO ISL = 1, ELNVCT(ISE)

          IF (TOTEULOAD(K,NRGN) .GT. 0.0) THEN
!           IF (fcrl.EQ.1) write(18,4014)'sortedload ',curiyr,nrgn,K,ise,isl,sortefdblk(ise,isl,k),TOTEULOAD(K,NRGN)
4014 format(A25,1x,5(I4,1x),2(F15.3,1x))
             sortefdblk(ise,isl,k) = sortefdblk(ise,isl,k)/TOTEULOAD(K,NRGN)
          ELSE
             sortefdblk(ise,isl,k) = 0.0
          ENDIF


         EPRIC2(k,NRGN) =  EPRIC2(k,NRGN) + &
          ERSLCPRC(CURIYR,NRGN,ise,isl,3)*        & !use average pub/priv
              sortefdblk(ise,isl,k)

         avgrelo(k) = avgrelo(k) + &
           ERRELC(CURIYR,NRGN,ise,isl)* &
           sortefdblk(ise,isl,k)

!       IF (fcrl.EQ.1) WRITE(18,8403)'check avgrel1 ',CURIYR,NRGN,ICLS,k,ISE,ISL, TMPRM(ICLS), avgrelo(k),ERRELC(CURIYR,NRGN,ise,isl), sortefdblk(ise,isl,k)
8403  FORMAT(A25,1x,6(I4,1x),4(F12.6,1x))

         avgmec(k) = avgmec(k) + &
           ERMECC(CURIYR,NRGN,ise,isl)* &
           sortefdblk(ise,isl,k)


         IF (isnan(TOTEULOAD(K,NRGN)) .OR. isnan(sortefdblk(ise,isl,k)) .OR. isnan(ERSLCPRC(CURIYR,NRGN,ise,isl,3)) .OR. &
            isnan(ERRELC(CURIYR,NRGN,ise,isl)) .OR. isnan(ERMECC(CURIYR,NRGN,ise,isl))) then
!           WRITE(6,7301) CURIRUN, CURIYR+1989, CURITR, k, icls, j, ise, isl, NRGN, TOTEULOAD(K,NRGN), sortefdblk(ise,isl,k), &
!              ERSLCPRC(CURIYR,NRGN,ise,isl,3), ERRELC(CURIYR,NRGN,ise,isl), ERMECC(CURIYR,NRGN,ise,isl)
 7301       FORMAT(1X,"avg_prc_dbg_1", 9(":",I4),25(":",F12.3))
         END IF


!        write(6,*) 'eprc2 ', k,EPRIC2(k,NRGN),
!          ise,isl, ERSLCPRC(CURIYR,NRGN,ise,isl,3),
!          sortefdblk(ise,isl,k)

         ENDDO
        ENDDO

        avgrel(k) = TMPRM(ICLS)

        EPRIC2(k,NRGN) =  EPRIC2(k,NRGN) + &
                     TMPRM(ICLS)

       IF (fcrl.EQ.1) WRITE(18,8408)'check avgrel2 ',CURIYR,NRGN,ICLS,k, TMPRM(ICLS), avgrelo(k)
8408  FORMAT(A25,1x,4(I4,1x),4(F12.6,1x))

        avgfix(k) = avgfix(k) + &
          ERFXGNAC(CURIYR,NRGN,1)*ESTSHR(1) + &
          ERFXGNAC(CURIYR,NRGN,2)*ESTSHR(2)
         avgitx(k) = avgitx(k) + &
           ERITAXC(CURIYR,NRGN,1)*ESTSHR(1) + &
           ERITAXC(CURIYR,NRGN,2)*ESTSHR(2)
         avgotx(k) = avgotx(k) + &
           EROTAXC(CURIYR,NRGN,1)*ESTSHR(1) + &
           EROTAXC(CURIYR,NRGN,2)*ESTSHR(2)

        IF (isnan(EPRIC2(k,NRGN)) .OR. EPRIC2(k,NRGN) .LE. 2.0 .OR. EPRIC2(k,NRGN) .GE. 200.0) THEN
!          WRITE(6,7302) CURIRUN, CURIYR+1989, CURITR, k, icls, j, NRGN, TOTEULOAD(K,NRGN), &
!             EPRIC2(k,NRGN), avgrel(k), avgmec(k), &
!             avgfix(k), ERFXGNAC(CURIYR,NRGN,1), ERFXGNAC(CURIYR,NRGN,2), &
!             avgitx(k), ERITAXC(CURIYR,NRGN,1), ERITAXC(CURIYR,NRGN,2), &
!             avgotx(k), EROTAXC(CURIYR,NRGN,1), EROTAXC(CURIYR,NRGN,2), ESTSHR(1), ESTSHR(2)
 7302         FORMAT(1X,"avg_prc_dbg_2", 7(":",I4),25(":",F12.3))
        END IF

       k = k + 1

      ENDDO
      ENDDO

      do k = 1, MTOTEUSGRP

        COMPCOMP(1,k,NRGN) = avgrel(k)
        COMPCOMP(2,k,NRGN) = avgmec(k)
        COMPCOMP(3,k,NRGN) = avgfix(k)
        COMPCOMP(4,k,NRGN) = avgitx(k) + avgotx(k)
      enddo

! get weights to do component weighting
! start new code - get demand weights for 23 end uses - value will be percent of total for sector
      l = 0
      do sectr = 1,4
      DO EU = 1,NEUSGRP(sectr)
      l = l+1
        demwtnr(l) = 0.0
      ENDDO
      enddo

      do sectr = 1,5
        totallnr(sectr) = 0.0
        relcomp(sectr)=0.0;meccomp(sectr)=0.0
        fixcomp(sectr)=0.0;taxcomp(sectr)=0.0
        prcall(sectr)=0.0
      enddo

      l = 0
      do sectr = 1,4
       do eu = 1,NEUSGRP(sectr)
       l = l+1
       demwtnr(l) = TOTEULOAD(l,NRGN)
       totallnr(sectr) = totallnr(sectr) + &
               demwtnr(l)
       enddo
      totallnr(5) = totallnr(5) + totallnr(sectr)
      enddo

      l = 0
       do sectr = 1,4
        do eu = 1,NEUSGRP(sectr)
          l = l+1
          IF (totallnr(sectr) .GT. 0.0) THEN
             demwtnr(l) = demwtnr(l)/totallnr(sectr)
          ELSE
             demwtnr(l) = 0.0
          END IF
        enddo
       enddo

      l = 0
       do sectr = 1,4
        do eu = 1,NEUSGRP(sectr)
          l = l+1
          relcomp(sectr) = relcomp(sectr) + demwtnr(l)*avgrel(l)
          meccomp(sectr) = meccomp(sectr) + demwtnr(l)*avgmec(l)
          fixcomp(sectr) = fixcomp(sectr) + demwtnr(l)*avgfix(l)
          taxcomp(sectr) = taxcomp(sectr) + &
                             demwtnr(l)*(avgitx(l)+avgotx(l))
          prcall(sectr) = prcall(sectr) + EPRIC2(l,NRGN)*demwtnr(l)
        enddo
       enddo
! average over sectors
       relcomp(5) = (relcomp(1)*totallnr(1) + relcomp(2)*totallnr(2) &
         + relcomp(3)*totallnr(3) + relcomp(4)*totallnr(4))/ &
            totallnr(5)
       meccomp(5) = (meccomp(1)*totallnr(1) + meccomp(2)*totallnr(2) &
         + meccomp(3)*totallnr(3) + meccomp(4)*totallnr(4))/ &
            totallnr(5)
       fixcomp(5) = (fixcomp(1)*totallnr(1) + fixcomp(2)*totallnr(2) &
         + fixcomp(3)*totallnr(3) + fixcomp(4)*totallnr(4))/ &
            totallnr(5)
       taxcomp(5) = (taxcomp(1)*totallnr(1) + taxcomp(2)*totallnr(2) &
         + taxcomp(3)*totallnr(3) + taxcomp(4)*totallnr(4))/ &
            totallnr(5)
       prcall(5) = (prcall(1)*totallnr(1) + prcall(2)*totallnr(2) &
         + prcall(3)*totallnr(3) + prcall(4)*totallnr(4))/ &
            totallnr(5)

!     if (fcrl .eq. 1) then
!      do sectr = 1,5
!     write(6,1313) ' comp nom$ gen ',CURIYR,NRGN,sectr,prcall(sectr),
!         relcomp(sectr),meccomp(sectr),fixcomp(sectr),
!         taxcomp(sectr)
!      enddo
1313  format (a15,3i4,5f7.2)
!     endif

      RETURN
      END



      SUBROUTINE ELADCR
      IMPLICIT NONE
!******************************************************************
!     THIS SUBROUTINE CALCULATES THE AFUDC RATE TO BE USED IN THE CURRENT YEAR
!******************************************************************
! INPUT VARIABLES:
!     ESPRLT = PERCENT OF CAPITAL OBTAINED WITH LONG TERM DEBT
!     ESEMDL = EMBEDDED COST OF LONG TERM DEBT FROM LAST YEAR
!     ESPRST = PERCENT OF CAPITAL OBTAINED WITH SHORT TERM DEBT
!     ESRTST = COST OF NEW SHORT TERM DEBT
!     ESPRCE = PERCENT OF CAPITALIZATION FROM COMMON EQUITY
!     ESRTCE = COST OF COMMON EQUITY
!     ESPRPS = PERCENT OF CAPITALIZATION FROM PREFERED STOCK
!     ESEMPL = EMBEDDED COST OF PREFERED STOCK FROM LAST YEAR
!     ESFTXR = EFFECTIVE FEDERAL TAX RATE
!     ESTSHR = PUBLIC/PRIVATE OWNERSHIP SHARE OF GENERATION
! INTERNAL VARIABLES:
!     TEMPD = TEMPORARY AFUDC DEBT COST %
!     TEMPE = TEMPORARY AFUDC EQUITY COST %
! OUTPUT VARIABLES:
!     ESAFDC = AFUDC RATE
!     ESWACD = DEBT FRACTION OF AFUDC
      include 'parametr'
      include 'emmparm'
      include 'efpgen'
      include 'eusprc'
      include 'efpint'
      include 'efprcy'
      include 'efprc'
      INTEGER N
      REAL*4 TEMPE
      REAL*4 TEMPD

! COMPUTE FOR EACH SECTOR (PUBLIC & PRIVATE)

      DO N=1,2
         IF (ESTSHR(N) .NE. 0.0) THEN

!           CALCULATE EQUITY RELATED PORTION OF AFUDC RATE
!              WEIGHTED SUM OF LONG AND SHORT TERM DEBT WHERE THE WEIGHTS
!              ARE THE PERCENT OF CAPITAL OBTAINED VIA EACH
!              FINANCING METHOD

            TEMPE = (ESPRCE(N)*ESRTCE(N)) + (ESPRPS(N)*ESEMPL(N))

!           CALCULATE PRE-TAX DEBT RELATED PORTION OF AFUDC RATE
!              WEIGHTED SUM OF COMMON EQUITY AND PREFERRED STOCK WHERE THE
!              WEIGHTS ARE THE PERCENT OF CAPITAL OBTAINED VIA EACH
!              FINANCING METHOD

            TEMPD = (ESPRLT(N)*ESEMDL(N)) + (ESPRST(N)*ESRTST(N))

!           CALCULATE AFUDC RATE AS SUM OF DEBT AND EQUITY PORTIONS

            ESAFDC(N) = TEMPD + TEMPE

!           CALCULATE DEBT FRACTION OF AFUDC (% OF AFUDC THAT IS DEBT RELATED)

            IF (ESAFDC(N) .GT. 0.001) THEN
               ESWACD(N) = TEMPD/ESAFDC(N)
            ELSE
               ESWACD(N) = 1.0
            ENDIF
         ELSE

!           IF THERE IS NO GENERATION FOR THIS OWNERSHIP TYPE --
!           ESTABLISH A RATE OF ZERO

            ESAFDC(N) = 0.0
            ESWACD(N) = 0.0
         ENDIF
      END DO
      RETURN
      END

      SUBROUTINE ELCWIP(NRGN,ICALL)

      IMPLICIT NONE

!******************************************************************
!     THIS SUBROUTINE CALCULATES THE FINANCIAL ACCOUNTS RELATED TO CONSTRUCTION WORK IN PROGRESS
!******************************************************************

!     INPUT VARIABLES:
!         EOMW = VINTAGE CAPACITY EXISTING IN BASE YEAR
!         ERCCA = COST OF POST SERVICE CAPITAL EXPENDITURES
!         ESBKLF = BOOK LIFE FOR NEW ASSETS BY PLANT TYPE
!         ESFTXR = FEDERAL STATUTORY TAX RATE
!         ESFPDB = % FLOW THROUGH OF DEBT PORTION OF AFUDC
!         EBLCP = NUCLEAR LENGTH OF CONSTRUCTION PERIOD
!         ESCGRW = ESTIMATED CAPITAL GROWTH FOR LAST YEAR
!         JCAPYR = FIRST START YEAR OF EFFECTED PLANTS
!         ICAPYR = YEAR IDC PROPOSAL IS IMPLEMENTED
!         EOVYRS = NUMBER OF VINTAGE YEARS
!         EIDIST = DISTRIBUTION PLANT TYPE INDEX
!         ESRTLT = COST OF NEW LONG TERM DEBT
!         EINUC = NUCLEAR PLANT TYPE INDEX
!         EBRSTP = INDICATES IF WE HAVE ACTUAL DATA FOR NUCLEAR UNITS
!         EBCWPP = % CWIP IN RATE BASE FOR EACH BUILD
!         EBPCAP = CAPACITY ASSOCIATED WITH EACH BUILD
!         EBPCST = CAPITAL COST ASSOCIATED WITH EACH BUILD
!         EBPTYP = PLANT TYPES FOR BUILDS
!         EBSERP = % OF YEAR PLANT IS IN SERVICE DURING THE FIRST YEAR
!         EBSYR  = YEAR WHICH PLANT COMES ON LINE
!         CURIYR  = CURRENT YEAR INDEX
!         ESAFDC = AFUDC RATE
!         ESCPRF = CONSTRUCTION PROFILE FOR EACH PLANT TYPE
!         ESCWPP = % CWIP IN RATE BASE FOR EACH YEAR
!         MC_JPGDP = DEFLATION RATE, BASE YEAR = 1984
!         ESLCP  = LENGTH OF THE CONSTRUCTION PERIOD FOR EACH BUILD
!         ESRBAF = % OF AFUDC/CWIP WHICH RECEIVES OFFSET TREATMENT
!         ESSLTX = SALES TAX RATE
!         ESTSHR = PUBLIC/PRIVATE OWNERSHIP SHARE OF GENERATION
!         ESWACD = DEBT FRACTION OF AFUDC
!     INTERNAL VARIABLES:
!         CAPAD = ?????
!         CONPER = % OF CONSTRUCTION EXPENDITURE FOR GIVEN YEAR
!         AVDINT = ?????
!         ELASVL = LAGGED TAX BASIS OF NEW ASSETS
!         LCP = CAPACITY FACTOR GROUPING FOR NEW PONDAGE HYDRO
!         DEBTAF = DEPT PORTION OF AFUDC
!         RBCWPL = LAGGED CWIP IN RATE BASE BY BUILD
!         EPCGW = SUM OF CAPACITY ASSOCIATED WITH BUILDS
!         EPGW = SUM OF CAPACITY ASSOCIATED WITH BUILDS
!         INUC = INDICATES WHETHER WE HAVE ACTUAL DATA FOR NUCLEAR UNITS
!         AFUDC  = THIS YEAR'S AFUDC FOR BUILD
!         CWPPER = % CWIP IN RATE BASE
!         CWIPWA = TOTAL BOOK CWIP FOR BUILD W/O THIS YEAR'S AFUDC
!         I      = BUILD NUMBER
!         J      = PLANT TYPE FOR BUILD
!         N      = OWNERSHIP TYPE (PUBLIC/PRIVATE)
!         NSTRT  = YEAR CONSTRUCTION STARTS
!         IYRFC  = YEAR OF CONSTRUCTION
!                  (1=ON-LINE YEAR, 2=YEAR BEFORE, ETC.)
!         WACE   = EQUITY FRACTION OF AFUDC
!     MODIFIED VARIABLES:
!         EBAFDC = AMORITIZATION OF AFUDC TAX SAVINGS FROM NEW ASSETS
!         EOBKVL = BOOKED VALUE OF EXISTING ASSETS
!         ESPTXR = PROPERTY TAX RATE
!         EBASVL = TAX BASIS OF NEW ASSETS
!         EBBCWP = BOOKED CWIP BY BUILD
!     OUTPUT VARIABLES:
!         ERFFDC = AMOUNT OF AFUDC TAX SAVINGS FLOWED THROUGH
!         EPYCWP = DIRECT CONSTRUCTION EXPENDITURES FOR YEAR (NO AFDC)
!         EPAFDC = BOOKED AFUDC - CURRENT YEAR
!         EPONLN = BOOK VALUE OF CAPACITY COMPLETED IN CURRENT YEAR
!         ERFFDC = AMOUNT OF AFUDC TAX SAVINGS FLOWED THROUGH
!         ERXFDC = DEFERRED AFUDC TAX SAVINGS
!         EBBKVL = BOOK VALUE OF ASSETS
!         EBRCWP = CWIP IN RATE BASE BY BUILD
!         EBYCWP = ANNUAL CONSTRUCTION EXPENDITURES BY PLANT
!         ERBCWP = TOTAL BOOKED CWIP 'INCLUDES AFUDC.'
!         ERDLRB = CHANGE IN RATE BASE DUE TO NEW BUILDS GOING INTO
!                  RATE BASE ON JANUARY 1.  (I.E., IF A PLANT GOES
!                  INTO SERVICE ON JAN 1, THE AVERAGE RATE BASE FOR
!                  THE YEAR IS NOT AVERAGE OF DEC 31, YEAR AND DEC 31,
!                  YEAR MINUS 1.)
!         ERFDC  = TOTAL ANNUAL AFUDC
!         ERFDCD = DEBT PORTION OF TOTAL AFUDC
!         ERFDCE = EQUITY PORTION OF TOTAL AFUDC
!         EROFFS = TOTAL AFUDC OFFSET
!         ERRCWP = TOTAL CWIP IN RATE BASE
!         ERSLTX = SALES TAX ON YEARLY CWIP EXPENDITURES
!         ERYCWP = TOTAL YEARLY CWIP W/O AFUDC
!         ERPRTX = PROPERTY TAX ON BOOK VALUE OF PLANTS
!         ERCIDC = CAPITALIZED INTEREST DURING CONSTRUCTION FOR TAXES
!         ERAFDL = RATE BASE ADJUSTMENT FOR PLANTS COMING ON IN MID
!                  YEAR (USED ONLY WHEN MODELING WITH EOY RATE BASE)

      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'tax'
      include 'efple'
      include 'efpgen'
      include 'efprc'
      include 'efpr'
      include 'efprcy'
      include 'eusprc'
      include 'efpint'
      include 'efpbld'
      include 'macout'
      include 'efpin'
      include 'control'
      INTEGER N,LYR
      INTEGER J
      INTEGER NSTRT
      INTEGER INUC
      INTEGER IYRFC
      INTEGER LCP
      INTEGER I
      INTEGER IYR,ISTART
      INTEGER NRGN,ECP_RG
      REAL*4 ELCEXP
      REAL*4 WACE
      REAL*4 ELASVL
      REAL*4 RBCWPL
      REAL*4 CONPER
      REAL*4 CWIPWA
      REAL*4 AFUDC
      REAL*4 AVDINT
      REAL*4 DEBTAF
      REAL*4 CAPAD,TCAPAD,TBKVL
      REAL*4 LE,TLE
      REAL*4 CWPPER
      REAL*4 YCWP
      CHARACTER*1 ICALL

!     COMPUTE FOR EACH SECTOR (PUBLIC & PRIVATE)

      DO N=1,2
         IF (ESTSHR(N) .NE. 0.0) THEN

!           INITIALIZATION

            ERBCWP(N) = 0.0
            ERRCWP(N) = 0.0
            EROFFS(N) = 0.0
            ERFDCE(N) = 0.0
            ERFDCD(N) = 0.0
            ERSLTX(N) = 0.0
            ERPRTX(N) = 0.0
            ERYCWP(N) = 0.0
            ERDLRB(N) = 0.0
            ERAFDL(N) = 0.0
            ERFFDC(N) = 0.0
            ERXFDC(N) = 0.0
            ERCIDC(N) = 0.0
            DO J=1,NPTYP
               EPGW(J,N) = 0.0
               EPCGW(J,N) = 0.0
               EPYCWP(J,N,1) = 0.0
               EPYCWP(J,N,2) = 0.0
               EPAFDC(J,N) = 0.0
               EPONLN(J,N) = 0.0
            END DO

!           CALCULATE EQUITY FRACTION OF AFUDC (INVERSE OF DEBT FRACTION)

            WACE = 1.0 - ESWACD(N)

!           TABULATE THE CWIP ACCOUNTS FOR EACH NEW BUILD

            DO I = 1 , EBNUM
               J = EBPTYP(I)
               NSTRT = EBSYR(I) - ESLCP(J) + 1
               IF (CURIYR .GE. NSTRT) THEN
                  IF (CURIYR .LE. EBSYR(I)) THEN

!                    IF BUILD IS UNDER CONSTRUCTION AND NOT IN SERVICE SET UP CWIP PERCENTAGE IN RATE BASE

                     CWPPER = ESCWPP(N)

!                    COMPUTE CAPITAL ESCALATION RATE BY ESCALATING LAST YEAR'S CAPITAL GROWTH THEN CALCULATE ANNUAL AND CUMULATIVE FLOW INTO THE CWIP ACCOUNT
!                    USE ACTUAL YEAR BY YEAR EXPENDITURE FOR NUCLEAR PLANTS, WHEN AVAILABLE. OTHERWISE USE CONSTRUCTION PROFILES.

                     IYRFC = EBSYR(I) - CURIYR + 1
                     CONPER = ESCPRF(J,IYRFC)

!                    TAKE % OF CAPITAL COST (CAPITAL COST IS GIVEN PER UNIT CAPACITY) THEN ESCALTE THE VALUE

                     YCWP = CONPER * EBPCAP(I,N) * EBPCST(I) * MC_JPGDP(CURIYR)
                     EBYCWP(I,N) = YCWP
                     ERYCWP(N) = ERYCWP(N) + YCWP
                     ELASVL = EBASVL(I,N)
                     EBASVL(I,N) = EBASVL(I,N) + YCWP

!                    CWIPWA = TOTAL BOOKED CWIP, WITHOUT THIS YEAR'S AFUDC ADDED

                     CWIPWA = EBBCWP(I,N) + EBYCWP(I,N)
                     IF (CURIYR .NE. EBSYR(I)) THEN

!                       CALCULATE OTHER CWIP ACCOUNTS AFUDC = THIS YEAR'S AFUDC FOR BUILD

                        AFUDC = (EBBCWP(I,N) + (0.5 * EBYCWP(I,N))) * (1.0 - CWPPER) * ESAFDC(N)
                        AVDINT = (ELASVL + (0.5 * EBYCWP(I,N))) * ESRTLT(N)
                        EBBCWP(I,N) = CWIPWA + AFUDC
                        RBCWPL = EBRCWP(I,N)
                        EBRCWP(I,N) = (CWIPWA * CWPPER) + (CWIPWA * (1.0 - CWPPER) * ESRBAF(N))
                        ERDLRB(N) = ERDLRB(N) + (EBRCWP(I,N) - RBCWPL)

!                       TABULATE TOTAL CWIP ACCOUNTS

                        ERRCWP(N) = EBRCWP(I,N) + ERRCWP(N)
                        ERBCWP(N) = ERBCWP(N) + EBBCWP(I,N)
                        EPGW(J,N) = EPGW(J,N) + EBPCAP(I,N)
                     ELSE

!                       PLANT COMES ON LINE THIS YEAR - THUS IT GETS SPECIAL TREATMENT DUE TO BEING ON-LINE FOR ONLY A PART YEAR

                        AFUDC = (EBBCWP(I,N) + (0.5 * EBYCWP(I,N))) * (1.0 - CWPPER) * (1.0 - EBSERP(I)) * ESAFDC(N)
                        EBBKVL(I,N) = CWIPWA + AFUDC
                        AVDINT = (ELASVL + (0.5 * EBYCWP(I,N))) * ESRTLT(N) * (1.0 - EBSERP(I))
                        ERDLRB(N) = (((2.0 * EBSERP(I)) - 1.0) * (EBBKVL(I,N) - EBRCWP(I,N))) + ERDLRB(N)
                        ERAFDL(N) = ERAFDL(N) + ((1.0 - EBSERP(I)) * (1.0 - CWPPER) * EBBKVL(I,N))
                        EPCGW(J,N) = EPCGW(J,N) + EBPCAP(I,N)
                        EPONLN(J,N) = EPONLN(J,N) + EBBKVL(I,N)
                     ENDIF

                     IF (ISNAN(AFUDC)) THEN
                        WRITE(6,2364) CURIRUN, CURIYR+1989, CURITR, EBSYR(I)+1989, NRGN, I, J, N, ICALL, &
                           AFUDC, EBBCWP(I,N), EBYCWP(I,N), CWPPER, ESAFDC(N), EBSERP(I), &
                           YCWP, CONPER, EBPCAP(I,N), EBPCST(I), MC_JPGDP(CURIYR)
 2364                   FORMAT(1X,"UEFP_AFUDC_NAN",8(":",I4),":",A1,11(":",F20.6))
                     END IF

!                    CALCULATE AFUDC ACCOUNTS

                     ERFDCE(N) = (AFUDC * WACE) + ERFDCE(N)
                     DEBTAF = AFUDC * ESWACD(N)
                     ERFDCD(N) = DEBTAF + ERFDCD(N)
                     EROFFS(N) = (AFUDC * ESRBAF(N)) + EROFFS(N)
                     IF ((NSTRT .GE. JCAPYR) .AND. (CURIYR .GE. ICAPYR)) THEN

!                       CAPITALIZE FOR TAX PURPOSES

                        EBASVL(I,N) = EBASVL(I,N) + AVDINT
                        ERCIDC(N) = ERCIDC(N) + AVDINT
                     ELSE

!                       DON'T CAPITALIZE INTEREST FOR TAX PURPOSES

                        DEBTAF = DEBTAF / (1.0 - ESFTXR(N))
                        ERCIDC(N) = ERCIDC(N) + DEBTAF
                     ENDIF
                     EPYCWP(J,N,1) = EPYCWP(J,N,1) + EBYCWP(I,N)
                     EPAFDC(J,N) = EPAFDC(J,N) + AFUDC

!                    CALCULATE SALES TAX ON CONSTRUCTION COSTS

                     ERSLTX(N) = (ESSLTX(N) * EBYCWP(I,N)) + ERSLTX(N)
                  ENDIF

!                 CALCULATE PROPERTY TAX

                  ERPRTX(N) = (ESPTXR(N) * EBBKVL(I,N)) + ERPRTX(N)
               ENDIF
            END DO

!           CALULATE TOTAL AFUDC

            ERFDC(N) = ERFDCE(N) + ERFDCD(N)

!           CALCULATE CAPITAL ADDITIONS FOR EXISTING PLANTS.

            IF (ICALL .EQ. 'G' .AND. CURIYR + UHBSYR .GT. EFPSYR) THEN
               TCAPAD = 0.0
               TBKVL = EBBKVL(EBNUM,N)
               DO J=1,EIPROD
                  LYR = ESBKLF(J,N) - CURIYR + EFPSYR - UHBSYR
                  LYR = MIN (LYR , EOVYRS)
                  LYR = MAX( LYR , 0 )
                  DO IYR = 1 , LYR
                     CAPAD = EOMW(IYR,J,N,NRGN) * ERCCA(J) * MC_JPGDP(CURIYR) * ESTSHR(N)
                     EBBKVL(EBNUM,N) = CAPAD + EBBKVL(EBNUM,N)
                     EBASVL(EBNUM,N) = EBASVL(EBNUM,N) + CAPAD
                     EBYCWP(EBNUM,N) = EBYCWP(EBNUM,N) + CAPAD
                     EPYCWP(J,N,2) = EPYCWP(J,N,2) + CAPAD
                     ERYCWP(N) = ERYCWP(N) + CAPAD
                     TCAPAD=TCAPAD+CAPAD
                  END DO
               END DO

!              CALCULATE CAPITAL ADDITIONS ON COMPLETED PRODUCTION BUILDS. THESE ADDITIONS ARE MADE EACH YEAR OF SERVICE LIFE.

               TCAPAD = 0.0
               TBKVL = EBBKVL(EBNUM,N)
               DO I=1,EBNUM
                  J = EBPTYP(I)
                  IF (J .LE. EIPROD) THEN
                     IF ((CURIYR .GE. EBSYR(I)) .AND. (CURIYR .LT. (EBSYR(I) + ESBKLF(J,N)))) THEN

!                       CAPACITY IS IN GW, COST IS 10^3/KW, SO MULTIPLY BY 1000

                        CAPAD = EBPCAP(I,N) * ERCCA(J) * MC_JPGDP(CURIYR) * 1000.0
                        EBBKVL(EBNUM,N) = EBBKVL(EBNUM,N) + CAPAD
                        EBASVL(EBNUM,N) = EBASVL(EBNUM,N) + CAPAD
                        EBYCWP(EBNUM,N) = EBYCWP(EBNUM,N) + CAPAD
                        EPYCWP(J,N,2) = EPYCWP(J,N,2) + CAPAD
                        ERYCWP(N) = ERYCWP(N) + CAPAD
                        TCAPAD=TCAPAD+CAPAD
                     ENDIF
                  ENDIF
               END DO

            ENDIF
         ENDIF
      END DO

      RETURN
      END
      SUBROUTINE ELBKDP(NRGN,ICALL)
      IMPLICIT NONE
!*******************************************************************
!        THIS SUBROUTINE CALCULATES BOOK DEPRECIATION AND TAX DEPRECIATION WITHOUT ACCELERATION
!*******************************************************************
! INPUT VARIABLES:
!     CURIYR  = CURRENT YEAR INDEX
!     EBPTYP = PLANT TYPE FOR EACH BUILD
!     EBSYR  = YEAR WHICH PLANT COMES ON LINE
!     EBSERP = % OF YEAR PLANT IS IN SERVICE DURING THE FIRST YEAR
!     EBBKVL  = BOOKED VALUE OF ASSETS
!     ESBKLF = BOOK LIFE FOR NEW ASSETS BY PLANT TYPE
!     EODEPR = BOOK DEPRECIATION RATE FOR OLD ASSETS BY PLANT TYPE
!     EOBKLF = BOOK LIFE FOR OLD ASSETS BY PLANT TYPE
!     ERABDL = ACCUMULATED BOOK DEPRECIATION EXPENSE FOR LAST YEAR
!     EOBKVL  = BOOKED VALUE OF EXISTING ASSETS
!     EOASVL  = VALUE OF EXISTING ASSETS NET OF AFUDC
!     ESTSHR = PUBLIC/PRIVATE GENERATION SHARE
!     EOVYRS = NUMBER OF VINTAGE YEARS
! INTERNAL VARIABLES:
!     BDE = BOOK DEPRECIATION COMPUTATIONS
!     NRETIR = YEAR AFTER PLANT FINISHES DEPRECIATION (RETIRE)
! MODIFIED VARIABLES:
!     ERBL   = BALANCE SHEET ENTRIES
!     EOABDE = ACCUMULATED BOOK DEPRECIATION FOR EXISTING ASSETS
! OUTPUT VARIABLES:
!     ERABDE = ACCUMULATED BOOK DEPRECIATION ALL ASSETS
!     ERBDE  = BOOK DEPRECIATION FOR ALL ASSETS
!     ERBDE1 = BOOK DEPRECIATION EXPENSE FOR EXISTING ASSETS
!     ERBDE2 = BOOK DEPRECIATION FOR NEW ASSETS
!     ERBTIR = BOOK VALUE OF ALL RETIREMENTS
!     ERBVYE = BOOK VALUE OF NEW ASSETS AT THE END OF THE YEAR
!     ERTDWO = TAX DEPRECIATION W/O ACCELERATION FOR ALL ASSETS
!     ERTDW1 = TAX DEPRECIATION W/O ACCELARATION FOR EXISTING ASSETS
!     ERTUP  = TOTAL UTILITY PLANT IN SERVICE
!     ERCNAD = ACCUMULATED AMORTIZATION FOR CANCELLED PLANT
!     ERCNBV = BOOK VALUE OF CANCELLED PLANT
!*******************************************************************
      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'tax'
      include 'efpgen'
      include 'efprc'
      include 'efpr'
      include 'efprcy'
      include 'eusprc'
      include 'efpint'
      include 'efpbld'
      include 'control'
      INTEGER N
      INTEGER K
      INTEGER J
      INTEGER IYR,JY,JYR
      INTEGER NRETIR
      INTEGER I
      INTEGER NRGN
      CHARACTER*1 ICALL
      REAL*4 BDE
      REAL*4 ERBVYE(16,2)
!
!     LOOP OVER PRIVATE AND PUBLIC UTILITIES
!
      DO N=1,2
         IF (ESTSHR(N) .NE. 0.0) THEN
!
!           INTIALIZATION
!
            ERTDW1(N) = 0.0
            ERBDE1(N) = 0.0
            ERBDE2(N) = 0.0
            ERBTIR(N) = 0.0
            ERCNAD(N) = 0.0
            DO J=1,NPTYP
               ERBVYE(J,N) = 0.0
               EPADOC(J,N) = 0.0
               EPISOC(J,N) = 0.0
            END DO
!
!           CALCULATE CURRENT YEAR BOOK AND TAX DEPRECIATION
!           (W/O ACCELERATION) EXPENSE FOR EACH ASSET
!           FIRST ADJUST ACCOUNTS FOR EXISTING ASSETS
!
            JY = EFPSYR - UHBSYR - CURIYR + 1
            JY = MAX(JY , 1)
            DO J=1,NPTYP
               DO IYR=JY,EOVYRS
!
!                 IF THE EXISTING PLANT HAS ALREADY RETIRED GO TO THE NEXT PLANT
!
                  NRETIR = EOBKLF(J,N)-IYR+EFPSYR-UHBSYR
                  IF (NRETIR .GE. CURIYR) THEN
!
!                    IF THE PLANT RETIRES IN THIS YEAR, ADD DEPRECIATED VALUE TO THE
!                    RETIREMENT ACCOUNT
!
                     IF (NRETIR .EQ. CURIYR) THEN
                        ERBTIR(N) = EOBKVL(IYR,J,N) + ERBTIR(N)
                     ELSE
!
!                       IF THE PLANT IS IN SERVICE ALL YEAR CALCULATE THE DEPRECIATION EXPENSE
!
                        ERTDW1(N) = EODEPR(J,N)*EOASVL(IYR,J,N) + ERTDW1(N)
                        BDE = (EOBKVL(IYR,J,N) - EOABDE(IYR,J,N)) /(NRETIR - CURIYR)
                        ERBDE1(N) = BDE + ERBDE1(N)
                        EOABDE(IYR,J,N) = BDE + EOABDE(IYR,J,N)
                        IF (EOBKVL(IYR,J,N) .NE. 0.0 .OR. &
                           EOABDE(IYR,J,N) .NE. 0.0 .AND. &
                           CURIYR+UHBSYR .LE. EFPSYR) THEN
                        END IF
                     ENDIF
                  ENDIF
               END DO
            END DO
!
!           NEXT DETERMINE DEPRECIATION EXPENSE FOR NEW ASSETS
!
            DO I=1,EBNUM
               J = EBPTYP(I)
!
!              IF THE PLANT IS STILL UNDER CONSTRUCTION OR HAS ALREADY BEEN
!              RETIRED SKIP TO THE NEXT BUILD
!
               NRETIR = EBSYR(I) + ESBKLF(J,N)
               IF ((EBSYR(I) .LE. CURIYR) .AND. (NRETIR .GE. CURIYR)) THEN
!
!                 IF THE PLANT COMES ON LINE IN THIS YEAR CALCULATE DEPRECIATION
!                 EXPENSE FOR THE PART OF THE YEAR THE PLANT IS IN SERVICE
!
                  IF (EBSYR(I) .EQ. CURIYR) THEN
                     BDE = (EBBKVL(I,N)/ESBKLF(J,N))*EBSERP(I)
                     ERBVYE(J,N) = EBBKVL(I,N) + ERBVYE(J,N)
!
!                    ACCUMULATE ABDE FOR CANCELLED PLANT THAT IS NOT RETIRED
!
                     IF (J .EQ. EICAN) ERCNAD(N) = ERCNAD(N) + EBABDE(I,N) + BDE
                  ELSE
!
!                    IF THE PLANT RETIRES IN THIS YEAR CALCULATE DEPRECIATION
!                    EXPENSE FOR THE PART OF THE YEAR THE PLANT IS IN SERVICE AND
!                    ADD ITS BOOK VALUE TO THE RETIREMENT ACCOUNT
!
                     IF (NRETIR .EQ. CURIYR) THEN
                        BDE = EBBKVL(I,N) - EBABDE(I,N)
                        ERBTIR(N) = EBBKVL(I,N) + ERBTIR(N)
                     ELSE
!
!                       IF PLANT IS IN SERVICE FOR THE ENTIRE YEAR CALCULATE THE
!                       DEPRECIATION EXPENSES
!
                        BDE = (EBBKVL(I,N) - EBABDE(I,N)) &
                              /(ESBKLF(J,N) - (CURIYR - EBSYR(I) + &
                              EBSERP(I) - 1))
                        ERBVYE(J,N) = EBBKVL(I,N) + ERBVYE(J,N)
!
!                       ACCUMULATE ABDE FOR CANCELLED PLANT THAT IS NOT RETIRED
!
                        IF (J .EQ. EICAN) ERCNAD(N) = ERCNAD(N) + EBABDE(I,N) + BDE
                     ENDIF
                  ENDIF
!
!                 ADD UP DEPRECIATION AND ABDE FOR ALL BUILDS
!
                  ERBDE2(N) = BDE + ERBDE2(N)
                  EBABDE(I,N) = BDE + EBABDE(I,N)
                  IF (NRETIR .NE. CURIYR) &
                     EPADOC(J,N) = EPADOC(J,N) + EBABDE(I,N)
               ENDIF
            END DO
!
!          COMBINE OLD AND NEW ASSET ACCOUNTS AND ADJUST FOR RETIREMENTS
!
            ERTDWO(N) = ERTDW1(N)
            ERBDE(N) = ERBDE2(N) + ERBDE1(N)
!           IF (CURIYR + UHBSYR .LE. EFPSYR)
!    +      write(*,3271) curiyr,nrgn,n,ERBDE2(N),ERBDE1(N),ERBDE(N),
!    +         ERABDE(N),ERABDL(N),ERBTIR(N)
!3271       format(1x,"damico_ERBDE",3(":",I4),6(":",F12.3))
            ERABDE(N) = ERABDL(N) + ERBDE(N) - ERBTIR(N)
!
!           ADD IN DEPRECIATION ON DISALLOWED PLANT TO BDE, BUT NOT TO ABDE
!
            ERBDE(N) = ERBDE(N) + ERBDED(N)
!
!           CALCULATE BALANCE SHEET ASSET ENTRIES
!
            JYR = EFPSYR - UHBSYR - CURIYR + 1
            JYR = MAX (JY , 1)
            ERBL(1,N) = 0.0
            ERBL(2,N) = 0.0
            ERBL(3,N) = 0.0
            ERBL(4,N) = 0.0
            DO I=1,NPTYP
               DO IYR=JYR,EOVYRS
                  NRETIR = EOBKLF(I,N) - IYR + EFPSYR-UHBSYR
                  IF (NRETIR .GT. CURIYR) THEN
                     IF (I .LE. EIPROD) ERBL(1,N) = ERBL(1,N) + EOBKVL(IYR,I,N)
                     IF (I .EQ. EITRAN) ERBL(2,N) = ERBL(2,N) + EOBKVL(IYR,I,N)
                     IF (I .EQ. EIDIST) ERBL(3,N) = ERBL(3,N) + EOBKVL(IYR,I,N)
                     IF (I .GT. EIDIST) ERBL(4,N) = ERBL(4,N) + EOBKVL(IYR,I,N)
                     EPISOC(I,N) = EPISOC(I,N) + EOBKVL(IYR,I,N)
                     EPADOC(I,N) = EPADOC(I,N) + EOABDE(IYR,I,N)
                  ENDIF
               END DO
            END DO
            write(22,9312) curiyr+uhbsyr,curitr,nrgn,n,ICALL,ERBL(1,N) + ERBL(2,N) + ERBL(3,N) + ERBL(4,N),ERBL(1,N),ERBL(2,N),ERBL(3,N),ERABDE(N)
 9312       format(1x,"erbl_e",4(":",i4),":",A1,5(":",f14.2))
            DO K=1,NPTYP
               IF (K .LE. EIPROD) ERBL(1,N) = ERBL(1,N) + ERBVYE(K,N)
               IF (K .EQ. EIDIST) ERBL(3,N) = ERBL(3,N) + ERBVYE(K,N)
               IF (K .EQ. EITRAN) ERBL(2,N) = ERBL(2,N) + ERBVYE(K,N)
               IF (K .GT. EIDIST) ERBL(4,N) = ERBL(4,N) + ERBVYE(K,N)
               EPISOC(K,N) = EPISOC(K,N) + ERBVYE(K,N)
            END DO
            ERCNBV(N) = ERBVYE(EICAN,N)
            ERTUP(N) = ERBL(1,N) + ERBL(2,N) + ERBL(3,N) + ERBL(4,N)
         ENDIF
         write(22,9313) curiyr+uhbsyr,curitr,nrgn,n,ICALL,ERTUP(N),ERBL(1,N),ERBL(2,N),ERBL(3,N),ERABDE(N)
 9313    format(1x,"erbl_t",4(":",i4),":",A1,5(":",f14.2))
      END DO
      RETURN
      END
!
      SUBROUTINE ELTXDP(NRGN,ICALL)
      IMPLICIT NONE
!*******************************************************************
! THIS SUBROUTINE CALCULATES TAX DEPRECIATION
!     IT USES:
!       1) THE SUM-OF-THE-YEARS METHOD
!       2) Economic Recovery Tax Act of 1981 MODIFIED BY Tax Equity and Fiscal Responsibility Act of 1982
!       3) TAX RECEIVABLE AGREEMENT METHOD
!*******************************************************************
! INPUT VARIABLES:
!     CURIYR  = CURRENT YEAR INDEX
!     EBPTYP = PLANT TYPE FOR EACH BUILD
!     EBSYR  = YEAR WHICH PLANT COMES ON LINE
!     EBASVL  = TAX BASIS OF NEW ASSETS
!     EOASVL  = VALUE OF EXISTING ASSETS NET OF AFUDC
!     ESTXRC = TAX RECOVERY CLASS (ERTA) FOR NEW ASSETS BY
!              PLANT TYPE
!     ESTXRP = TAX RECOVERY PERIOD (ERTA) FOR EACH RECOVERY CLASS
!     ESTXRS = TAX RECOVERY SCHEDULE (ERTA) FOR EACH RECOVERY CLASS
!     ESTXLF = TAX LIFE FOR NEW ASSETS UNDER SYD METHOD
!     EOTXLF = TAX LIFE FOR OLD ASSETS
!     ESFTXR = FEDERAL TAX RATE
!     ESFLPR = PERCENT OF TAX SAVINGS FLOWED THROUGH
!              CALCULATING TAX DEPRECIATION)
!     ERTDW1 = TAX DEPRECIATION W/O ACCELERATION FOR EXISTING
!              ASSETS
!     ESTSHR = PUBLIC/PRIVATE GENERATION SHARE
!     ERPDFL = DEFERRED TAXES FOR LAST YEAR
!     EOVYRS = NUMBER OF VINTAGE YEARS
!     ESBKLF = BOOK LIFE FOR NEW ASSETS BY PLANT TYPE
!     EBSERP = % OF YEAR PLANT IS IN SERVICE DURING THE FIRST YEAR
!     ESDEPR = DEPRECIATION RATE FOR BOOK PURPOSES
!     IRSYP = FIRST YEAR IN WHICH NEW PROD PLANT USES Tax Reform Act (?) DEPRECIATION SCHEDULE
!     IRSYTD = FIRST YEAR IN WHICH NEW T&D PLANT USES Tax Reform Act (?) DEPRECIATION SCHEDULE
!     CONDR = Tax Reform Act (?) TAX DEPRECIATION SCHEDULES
! INTERNAL VARIABLES:
!     SYDTDR = SUM OF YEARS DIGITS TAX DEPRECIATION RATE
!     TEMP1  = NUMERATOR IN SYD DEPRECIATION CALCULATION
!     TEMP2  = DENOMINATOR IN SYD DEPRECIATION CALCULATION
!     NRETIR = YEAR AFTER PLANT FINISHES DEPRECIATION (RETIRE)
!     ICURYR = CURRENT YEAR IN RELATION TO START YEAR
! MODIFIED VARIABLES:
!     ERTDWO = TAX DEPRECIATION W/O ACCELERATION FOR ALL ASSETS
!     ERPDTB = ACCUMULATED DEFERRED TAXES, GENERATED PRE-FORECAST PERIOD
! OUTPUT VARIABLES:
!     ERTDE1 = TAX DEPRECIATION EXPENSE FOR EXISTING ASSETS
!     ERTDE2 = TAX DEPRECIATION EXPENSE FOR NEW ASSETS
!     ERTDE  = TOTAL TAX DEPRECIATION
!     ERATSD = ACCELERATED DEPRECIATION TAX SAVINGS - DEFERRED
!     ERATSF = ACCELERATED TAX DEPRECIATION SAVINGS - FLOWED THROUGH
!     ERPRDF = PROVISION FOR DEFERRED TAXES
!     ERTDRG = TAX DEPRECIATION USED FOR REGULATORY PURPOSES
!     ERTDW2 = TAX DEPRECIATION WITHOUT ACCELERATION FOR NEW ASSETS
!*******************************************************************
      include 'parametr'
      include 'emmparm'
      include 'tax'
      include 'ncntrl'
      include 'efpgen'
      include 'efprc'
      include 'efpr'
      include 'efprcy'
      include 'eusprc'
      include 'efpint'
      include 'efpbld'
      include 'control'
      INTEGER N
      INTEGER J,JY
      INTEGER IYR
      INTEGER L
      INTEGER K,ITXRP
      INTEGER I,NRGN
      INTEGER NRETIR
      INTEGER ICURYR
      REAL*4 TEMP1
      REAL*4 TEMP2
      REAL*4 SYDTDR
      CHARACTER*6 Label
      CHARACTER*1 ICALL
!
!  PUBLIC UTILITIES
      N=2
!
      ERTDE1(N) = 0.0
      ERTDE2(N) = 0.0
      ERATSF(N) = 0.0
      ERATSD(N) = 0.0
      ERTDW2(N) = 0.0
      ERPDTB(N) = 0.0
      ERTDWO(N) = 0.0
      ERTDE(N)  = 0.0
      ERATSF(N) = 0.0
      ERPRDF(N) = 0.0
      ERTDRG(N) = 0.0
!
!     PRIVATE UTILITIES
!
      N = 1
      IF (ESTSHR(N) .NE. 0.0) THEN
!
!        INTIALIZATION
!
         ERTDE1(N) = 0.0
         ERTDE2(N) = 0.0
         ERATSF(N) = 0.0
         ERATSD(N) = 0.0
         ERTDW2(N) = 0.0
!
         JY = EFPSYR - (UHBSYR + CURIYR - 1)
         JY = MAX (JY , 1)
!
!        CALCULATE THE TAX DEPRECIATION FOR EXISTING ASSETS USING
!        THE METHOD THAT THE VINTAGE YEAR INDICATES (1=1984, 2=1983, ETC.)
!
         DO J = 1 , NPTYP
            DO IYR = JY , EOVYRS
               IF (EOASVL(IYR,J,N) .GT. 0.0) THEN
                  L = (CURIYR + UHBSYR) - (EFPSYR - IYR + 1) + 1
!
!                 VINTAGE (1987 - 1995) Use CONDR to Determine Accelerated Dep.
!
                  IF (IYR .LE. EFPSYR - 1987 + 1) THEN
                     ERTDE1(N) = ERTDE1(N) + CONDR(L,ICONTP(J)) * EOASVL(IYR,J,N)
                     Label = "4CONDR"
                     K = ICONTP(J)
!                    WRITE(6,2131) Label,CURIYR+UHBSYR,CURITR,EFPSYR-IYR+1,IYR,NRGN,N,ICALL,J,L,K,K,CONDR(L,K),EOASVL(IYR,J,N),CONDR(L,ICONTP(J)) * EOASVL(IYR,J,N)
 2131                FORMAT(1x,A5,6(":",I4),": ",A1,4(":",I4),4(":",F11.3))
!
                  ELSE IF (IYR .LT. EFPSYR - 1981 + 2) THEN
!
!                    USE ERTA, MODIFIED BY TEFRA,
!                    IF VINTAGE YEAR IS (1987-1981)
!
                     K = ESTXRC(J,N)
                     IF (L .LE. ESTXRP(K)) THEN
                        ERTDE1(N) = ERTDE1(N) + ESTXRS(K,L) * (0.95 * EOASVL(IYR,J,N))
                        Label = "4ERTA_"
                        ITXRP = ESTXRP(K)
!                       WRITE(6,2131) Label,CURIYR+UHBSYR,CURITR,EFPSYR-IYR+1,IYR,NRGN,N,ICALL,J,L,K,ITXRP,ESTXRS(K,L),EOASVL(IYR,J,N), &
!                          ESTXRS(K,L) * 0.95 * EOASVL(IYR,J,N)
                     END IF
                  ELSE
!
!                    USE SYD IF VINTAGE YEAR IS BEFORE 1981
!
                     TEMP1 = (EOTXLF(J,N) - (L - 1))
                     TEMP1 = MAX( TEMP1 , 0.0 )
                     TEMP2 = EOTXLF(J,N) * (EOTXLF(J,N) + 1) * 0.5
                     SYDTDR = TEMP1 / TEMP2
                     SYDTDR = AMAX1( SYDTDR , 0.0)
                     ERTDE1(N) = ERTDE1(N) + SYDTDR * EOASVL(IYR,J,N)
                     Label = "4SYD__"
                     K = EOTXLF(J,N)
!                    WRITE(6,2131) Label,CURIYR+UHBSYR,CURITR,EFPSYR-IYR+1,IYR,NRGN,N,ICALL,J,L,K,K,SYDTDR,EOASVL(IYR,J,N),SYDTDR * EOASVL(IYR,J,N)
                  ENDIF
               ENDIF
            END DO
         END DO
!
!        CALCULATE EXCESS TAX OVER BOOK DEPRECIATION FOR EXISTING ASSETS
!        CUMULATIVE DEFERRAL SHOULD BE ZERO AT THE END OF THE BOOK LIFE
!        A CORRECTION IS MADE IF THIS DOES NOT OCCUR
!
         IF (ERPDTB(N) .GT. 0.0) THEN
            ERATSD(N) = ESFTXR(N)*(ERTDE1(N) - ERTDW1(N))* (1 - ESFLPR(N))
            ERATSF(N) = ESFTXR(N)*(ERTDE1(N) - ERTDW1(N))* (ESFLPR(N))
            ERPDTB(N) = ERPDTB(N) + ERATSD(N)
            IF (ERPDTB(N) .LT. 0.0) ERATSD(N) = ERATSD(N) - ERPDTB(N)
            IF (ERPDTB(N) .LT. 0.0) THEN
               ERATSD(N) = ERATSD(N) - ERPDTB(N)
               ERPDTB(N) = 0.0
            END IF
         ENDIF
         Label = "4RPDTB"
!        WRITE(6,2132) Label,CURIYR+UHBSYR,CURITR,NRGN,N,ICALL,ERTDW1(N),ERTDE1(N),ESFTXR(N),ESFLPR(N),ERATSD(N),ERATSF(N),ERPDTB(N)
 2132    FORMAT(1x,A6,4(":",I4),": ",A1,7(":",F11.3))
!
!        CALCULATE TAX DEPRECIATION FOR NEW ASSETS   ! use CONDRN variable to reflect alternate bonus depreciation based on online year, from Dec 2017 tax law
!
         DO I = 1 , EBNUM
            IF (CURIYR .GE. EBSYR(I)) THEN
               J = EBPTYP(I)
               NRETIR = EBSYR(I) + ESBKLF(J,N)
               IF (J .EQ. EICAN) THEN
!
!                 CALCULATE TAX DEPRECIATION USING SYD
!
                  K = EBSYR(I) + ESTXLF(J,N) - 1
                  IF (CURIYR .LE. K) THEN
                     TEMP1 = ESTXLF(J,N) - (CURIYR - EBSYR(I))
                     TEMP2 = ESTXLF(J,N)*(ESTXLF(J,N) + 1) * 0.5
                     ERTDE2(N) = (TEMP1/TEMP2)*EBASVL(I,N) + ERTDE2(N)
                  ENDIF
               ELSE
!
!                 USE SENATE & HOUSE CONFEREES DEPRECIATION IF ASSET
!                 START YEAR IS IN RANGE
!
                  IF ((EBSYR(I) .GE. IRSYP .AND. J .LE. EIPROD) .OR. (EBSYR(I) .GE. IRSYTD .AND. J .GT. EIPROD)) THEN
!
!                    DETERMINE TAX DEPRECIATION METHOD.
!
                     ICURYR = CURIYR - EBSYR(I) + 1
                     IF (EBSYR(I) .GE. IRSYT2 .AND. J .EQ. EITRAN) THEN    ! switch dep sched to 15 year - option 2
                       ERTDE2(N) = ERTDE2(N) + CONDRN(ICURYR,2,EBSYR(I))* EBASVL(I,N)
                     ELSE
                       ERTDE2(N) = ERTDE2(N) + CONDRN(ICURYR,ICONTP(J),EBSYR(I))* EBASVL(I,N)
                     ENDIF
                  ELSE
!
!                    CALCULATE TAX DEPRECIATION USING ERTA, AS MODIFIED BY TEFRA.
!
                     L = ESTXRC(J,N)
                     K = CURIYR - EBSYR(I) + 1
                     IF (K .LE. ESTXRP(L)) ERTDE2(N) = ERTDE2(N) + ESTXRS(L,K) * EBASVL(I,N)
                  ENDIF
               ENDIF
!
!              CALCULATE STRAIGHT LINE TAX DEPRECIATION
!
               IF (NRETIR .GE. CURIYR) THEN
                  ERTDW2(N) = ERTDW2(N) + EBASVL(I,N) * ESDEPR(J,N)
!
!                 ADJUST FOR PART YEAR DEPRECIATION IF FIRST OR
!                 LAST YEAR OF LIFE
!
                  IF (EBSYR(I) .EQ. CURIYR) ERTDW2(N) = ERTDW2(N) - EBASVL(I,N) * ESDEPR(J,N) * (1.0 - EBSERP(I))
                  IF (NRETIR .EQ. CURIYR) ERTDW2(N) = ERTDW2(N) - EBASVL(I,N) * ESDEPR(J,N) * EBSERP(I)
               ENDIF
            ENDIF
         END DO
!
!        CALCULATE TOTAL TAX DEPRECIATION EXPENSE, DEFERRED AND FLOWED
!        THROUGH TAX SAVINGS, ACCUMULATED DEFERRED TAX SAVINGS AND
!        DEPRECIATION EXPENSE USED FOR REGULATORY PURPOSES
!
         ERTDWO(N) = ERTDWO(N) + ERTDW2(N)
         ERTDE(N) = ERTDE1(N) + ERTDE2(N)
         ERATSF(N) = ERATSF(N) + (ESFTXR(N) * (ERTDE2(N) - ERTDW2(N))) * ESFLPR(N)
         ERATSD(N) = ERATSD(N) + (ESFTXR(N) * (ERTDE2(N) - ERTDW2(N))) * (1 - ESFLPR(N))
         ERPRDF(N) = ERPDFL(N) + ERATSD(N)
         ERTDRG(N) = ERTDWO(N) * (1.0 - ESFLPR(N)) + ERTDE(N) * ESFLPR(N)
      END IF
!
      RETURN
      END

      SUBROUTINE ELITC
      IMPLICIT NONE
!******************************************************************
!     THIS SUBROUTINE CALCULATES THE ACCOUNTS RELATING TO THE INVESTMENT TAX CREDIT
!******************************************************************
! INPUT VARIABLES:
!     EBPTYP = PLANT TYPE FOR EACH BUILD
!     EBSYR  = YEAR WHICH PLANT COMES ON LINE
!     EBASVL  = TAX BASIS OF ASSETS
!     EBYCWP = ANNUAL FLOW INTO CWIP ACCOUNT
!     CURIYR  = CURRENT YEAR INDEX
!     EOBKLF = BOOK LIFE FOR OLD ASSETS BY PLANT TYPE
!     EOAITC = AMORITIZATION OF DEFERRED ITC FROM EXISTING ASSETS
!     ESBKLF = BOOK LIFE FOR NEW ASSETS
!     ESFLPR = PERCENT OF DEFERRED TAX FLOWED THROUGH
!     ESLCP  = LENGTH OF CONSTRUCTION PERIOD FOR EACH BUILD
!     ESRITC = INVESTMENT TAX CREDIT RATE
!     ESTSHR = PUBLIC/PRIVATE GENERATION SHARE
!     ESFTXR = EFFECTIVE FEDERAL TAX RATE
!     EBLCP = NUCLEAR LENGTH OF CONSTRUCTION PERIOD
!     EOVYRS = NUMBER OF VINTAGE YEARS
!     IYRITC=YEAR ITC PROPOSAL IS IMPLEMENTED
!     JYRITC=FIRST START YEAR OF EFFECTED PLANTS
! INTERNAL VARIABLES:
!     GITC = ?????
!     XITCDP = ?????
!     NRETIR = YEAR AFTER PLANT FINISHES DEPRECIATION (RETIRE)
!     NSTRT  = YEAR START BUILDING PLANT
!     XITCD  = ITC GENERATED WHICH IS DEFERRED FOR RATEMAKING
! MODIFIED VARIABLES:
!     ERDITC = ACCUMULATED DEFERRED ITC
! OUTPUT VARIABLES:
!     EBDITC = DEFERRED ITC FROM NEW ASSETS
!     ERAITC = AMORTIZATION/YEAR OF DEFERRED ITC
!     ERFITC = AMOUNT OF ITC FLOWED THROUGH FOR RATEMAKING
!     ERNITC = TOTAL DEFERRED ITC NET OF AMORITIZED DEFERRALS
!******************************************************************
      include 'parametr'
      include 'emmparm'
      include 'tax'
      include 'ncntrl'
      include 'efpgen'
      include 'efprc'
      include 'efpr'
      include 'efprcy'
      include 'eusprc'
      include 'efpint'
      include 'efpbld'
      include 'control'
      INTEGER N
      INTEGER I
      INTEGER J
      INTEGER IYR,JK
      INTEGER NRETIR
      INTEGER NSTRT
      REAL*4 XITCD
      REAL*4 GITC
      REAL*4 XITCDP

      JK = EFPSYR - UHBSYR
!
!     LOOP OVER PRIVATE AND PUBLIC UTILITIES
!
      DO N=1,2
         IF (ESTSHR(N) .NE. 0.0) THEN
!
!           INTIALIZE OUTPUT VARIABLES
!
            ERAITC(N) = 0.0
            ERFITC(N) = 0.0
            ERNITC(N) = 0.0
            IF (ESFTXR(N) .NE. 0.0) THEN
               XITCD = 0.0
!
!             ADD IN AMORITIZATION OF DEFFERED ITC FROM EXISTING ASSETS TO
!             TOTAL AMORITIZATION OF DEFERRED ITC
!             USE THE VALUE OF NRETIR THAT IS DETERMINED BY THE VINTAGE YEAR
!
              DO J = 1 , NPTYP
                  NSTRT = JK - CURIYR  + 1
                  IF (NSTRT .GE. 1 ) THEN
                     ERDITC(N) = ERDITC(N) + EOAITC(NSTRT,J,N) * EOBKLF(J,N)
                  ELSE
                     NSTRT = 1
                  END IF
                  DO IYR = NSTRT , EOVYRS
                     NRETIR = EOBKLF(J,N) - IYR + JK
                     IF (CURIYR .LE. NRETIR) THEN
                        ERAITC(N) = EOAITC(IYR,J,N) + ERAITC(N)
                     END IF
                  END DO
               END DO
!
!             CALCULATE AMOUNT OF ITC GENERATED AND SHARE BETWEEN FLOW-THROUGH
!             AND DEFERRED ACCOUNTS
!
               DO I=1,EBNUM
                  J = EBPTYP(I)
                  NSTRT = EBSYR(I) - ESLCP(J) + 1
                  NRETIR = EBSYR(I) + ESBKLF(J,N)
                  IF ((CURIYR .GE. NSTRT) .AND. (CURIYR .LT. NRETIR)) THEN
                     IF (CURIYR .LE. EBSYR(I)) THEN
                        GITC = ESRITC(J)*EBYCWP(I,N)
                        IF ((CURIYR .GE. IYRITC) .AND. (NSTRT .GE. JYRITC)) GITC = 0.0
                        ERFITC(N) = GITC*ESFLPR(N) + ERFITC(N)
                        XITCDP = GITC*(1.0 - ESFLPR(N))
                        EBDITC(I,N) = EBDITC(I,N) + XITCDP
                        XITCD = XITCD + XITCDP
!
!                      LOWER TAX BASIS BY 1/2 OF DITC IF ASSETS ARE POST-TEFRA
!
                        IF (CURIYR .EQ. EBSYR(I)) THEN
                           EBASVL(I,N) = EBASVL(I,N) - 0.5*EBDITC(I,N)
                           ERAITC(N) = EBDITC(I,N)/ESBKLF(J,N) + ERAITC(N)
                        ENDIF
                     ELSE
!
!                       AMORITIZE DEFERRED ITC WHEN PLANT COMES ON LINE
!                       ADD AMORTIZED DEFERRED ITC FROM NEW ASSETS TO TOTAL AMORITIZED
!                       DEFERRED ITC
!
                        ERAITC(N) = EBDITC(I,N)/ESBKLF(J,N) + ERAITC(N)
                     ENDIF
                  ENDIF
               END DO
!
!              ADJUST TOTAL DEFERRED ITC ACCOUNT BY ADDING NEW DEFERREMENT AND
!              SUBTRACTING AMORITIZATION WHEN PLANT COMES ON LINE
!
               ERNITC(N) = XITCD - ERAITC(N)
               ERDITC(N) = ERDITC(N) + ERNITC(N)
            ENDIF
         ENDIF
      END DO
      RETURN
      END
!
      SUBROUTINE ELTSAF
      IMPLICIT NONE
!*******************************************************************
!     THIS SUBROUTINE CALCULATES THE ACCOUNTS RELATING TO THE TAX SAVINGS FROM THE DEBT PORTION OF AFUDC
!          AFUDC = Allowance of funds used during construction
!*******************************************************************
! INPUT VARIABLES:
!     EOBKLF = BOOK LIFE FOR OLD ASSETS BY PLANT TYPE
!     CURIYR  = CURRENT YEAR INDEX
!     EBPTYP = PLANT TYPE FOR EACH BUILD
!     EBSYR  = YEAR WHICH PLANT COMES ON LINE
!     EOAFDC = AMORITIZATION OF AFUDC TAX SAVINGS FROM OLD ASSETS
!     ESBKLF = BOOK LIFE FOR NEW ASSETS
!     ESFTXR = FEDERAL TAX RATE
!     ESTSHR = PUBLIC/PRIVATE GENERATION SHARE
!     EBAFDC = AMORITIZATION OF AFUDC TAX SAVINGS FROM NEW ASSETS
!     ERXFDC = DEFERRED AFUDC TAX SAVINGS
!     EOVYRS = NUMBER OF VINTAGE YEARS
! INTERNAL VARIABLES:
!     NRETIR = YEAR AFTER PLANT FINISHES DEPRECIATION (RETIRE)
! OUTPUT VARIABLES:
!     ERAFDC = AMORTIZATION OF AFUDC TAX SAVINGSFOR ALL ASSETS
!     EOLAFC = AMORTIZATION OF AFUDC TAX SAVINGS FOR OLD ASSETS
!     ENUAFC = AMORTIZATION OF AFUDC TAX SAVINGSFOR NEW ASSETS
!     ERPRDF = TOTAL DEFERRED TAXES FROM EXCESS OVER BOOK
!*******************************************************************
      include 'parametr'
      include 'emmparm'
      include 'tax'
      include 'ncntrl'
      include 'efpgen'
      include 'efprc'
      include 'efpr'
      include 'efprcy'
      include 'eusprc'
      include 'efpint'
      include 'efpbld'
      include 'control'
      INTEGER N
      INTEGER J
      INTEGER IYR,JY
      INTEGER NRETIR
      INTEGER I
      REAL*4 EOLAFC(2)
      REAL*4 ENUAFC(2)

      JY = EFPSYR - UHBSYR + 1 - CURIYR + 1
      JY = MAX (JY , 1)

!     LOOP OVER PRIVATE AND PUBLIC UTILITIES

      DO N=1,2
         IF (ESTSHR(N) .NE. 0.0) THEN

!           INTIALIZE OUTPUT VARIBALES

            EOLAFC(N) = 0.0
            ENUAFC(N) = 0.0
            IF (ESFTXR(N) .NE. 0.0) THEN

!              AMORTIZE THE AFUDC DEFERRAL OVER THE LIFE OF EXISTING ASSETS
!              USE THE VALUE OF NRETIR THAT IS DETERMINED BY THE VINTAGE YEAR

               DO J=1,NPTYP
                  DO IYR=JY,EOVYRS
                     NRETIR = EOBKLF(J,N)-IYR+EFPSYR-UHBSYR
                     IF (CURIYR .LT. NRETIR) EOLAFC(N) = &
                                            EOAFDC(IYR,J,N) + &
                                            EOLAFC(N)
                  END DO
               END DO

!              AMORITIZE TAX SAVING OVER LIFE OF NEW ASSETS

               DO I=1,EBNUM
                  J = EBPTYP(I)
                  NRETIR = EBSYR(I) + ESBKLF(J,N)
                  IF ((CURIYR .GE. EBSYR(I)) .AND. &
                     (CURIYR .LT. NRETIR)) ENUAFC(N) = &
                                          EBAFDC(I,N) + ENUAFC(N)
               END DO

!              CALCULATE CUMULATIVE DEFERRED AFUDC TAX SAVINGS

               ERAFDC(N) = EOLAFC(N) + ENUAFC(N)
               ERPRDF(N) = ERPRDF(N) + ERXFDC(N) - ERAFDC(N)
            ENDIF
         ENDIF
      END DO
      RETURN
      END
!
      SUBROUTINE ELPHIN(NRGN)
      IMPLICIT NONE
!******************************************************************
! THIS SUBROUTINE CALCULATES THE IMPACT OF PHASE-IN/DISALLOWANCE PLANS OF NEW CAPACITY
!     SINCE THE REST OF THE MODEL HAS
!     CALCULATED COMPONENTS OF REVENUE REQUIREMENTS ASSUMING
!     TRADITIONAL REGULATORY TREATMENT OF THESE PLANTS, THIS ROUTINE
!     ONLY CALCLUATES THE CHANGE IN THE COMPONENTS CAUSED BY THE PHASE
!     IN PLAN, RELATIVE TO TRADITIONAL REGULATION.
!******************************************************************
! INPUT VARIABLES:
!     EIDIST = PLANT CODE FOR DISTRIBUTION CAPITAL
!     EBPTYP = PLANT TYPE FOR EACH BUILD
!     CURIYR  = CURRENT YEAR INDEX
!     EBBKVL  = BOOKED VALUE OF ASSETS
!     ESPRLT = PERCENT OF CAPITAL OBTAINED WITH LONG TERM DEBT
!     EBSYR  = YEAR WHICH PLANT COMES ON LINE
!     ESRTST = COST OF NEW SHORT TERM DEBT
!     ESPRST = PERCENT OF CAPITAL OBTAINED WITH SHORT TERM DEBT
!     ESEMDL = LAST YEARS EMBEDDED COST OF DEBT
!     ESPRPS = PERCENT OF CAPITALIZATION FROM PREFERED STOCK
!     ESRTCE = COST OF COMMON EQUITY
!     ESPRCE = PERCENT OF CAPITALIZATION FROM COMMON EQUITY
!     ESAFDC = AFUDC RATE
!     ESFTXR = FEDERAL STATUTORY TAX RATE
!     ESEMPL = EMBEDDED COST FOR THE PREF. STOCK FOR LAST YR
!     NPI = TOTAL NUMBER OF PHASE-IN PLANS
!     LPI = LENGTH OF PHASE-IN PLAN
!     IROPI(1,IPI) = REGION OF PHASE-IN PLAN
!     IROPI(2,IPI) = OWNERSHIP TYPE OF PHASE-IN PLAN
!     IBYRPI = 1ST YEAR OF PHASE-IN PERIOD
!     PIBKVL = BOOK VALUE OF PLANT TO BE PHASED-IN
!     PIDFS = CUMULATIVE FRACTION OF TOTAL COST PHASED-IN BY
!             YEAR
!     PIRCS = FRACTION OF REMAINING DEFERRED REVENUES TO BE
!             RECOVERED BY YEAR
!     PIDEF = TOTAL CUMULATIVE DEFERRED REVENUES
!     IRDPI = CAPITALIZE RETURN ON DEFERRED COST? (1=YES,2=NO)
!     PIBKLF = BOOK LIFE OF PHASED-IN PLANT
!     PITXBS = TAX BASIS AS A FRACTION OF BOOKED COST OF
!              PHASE-IN PLANT
!     DISPER = FRACTION OF PLANT TOTALLY DISALLOWED
! INTERNAL VARIABLES:
!     IYRPI = INDEX OF 1ST YEAR OF PHASE-IN RELATIVE TO CURRENT YEAR
!     YRPI = SAME AS IYRPI BUT IN A REAL VARIABLE
!     IBKLF = BOOK LIFE OF PLANT TO BE PHASED-IN
!     AVERB = ADJUSTMENT FOR COMPUTING REVENUE REQUIREMENT USING
!             YEARLY AVERAGE OR YEAR-END RATE BASE
!     RR = REVENUE REQUIREMENT
!     DEFCST = DEFERRED COST
!     PIRET = RETURN ON DEFERRED PORTION
! OUTPUT VALUES:
!     ERBDED = DEPRECIATION EXPENSE FOR DISALLOWED PLANT
!     EPIDEF = TOTAL CUMULATIVE DEFERRED PHASE-IN REVENUES
!     EPIND = NET-DEFERRED PHASE-IN REVENUES FOR YEAR
!     EPIRET = CAPITALIZED RETURN ON DEFERRED REVENUES FOR YEAR
!     EDISNT = NET DISALLOWED PLANT FOR YEAR
!     EDISYR = GROSS PLANT DISALLOWED IN THIS YEAR
!******************************************************************
      include 'phasin'
      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'efpgen'
      include 'efprc'
      include 'efpr'
      include 'efprcy'
      include 'eusprc'
      include 'efpint'
      include 'efpbld'
      include 'control'
      INTEGER NRGN
      INTEGER I
      INTEGER IPI
      INTEGER IYRPI
      INTEGER IBKLF
      INTEGER K
      INTEGER FULLYR
      REAL*4 YRPI
      REAL*4 AVERB
      REAL*4 RR
      REAL*4 DEFCST
      REAL*4 PIRET
      FULLYR = USYEAR(CURIYR)
      DO I=1,2
!
!        INITIALIZATION
!
         ERBDED(I) = 0.0
         EPIND(I) = 0.0
         EPIDEF(I) = 0.0
         EPIRET(I) = 0.0
         EDISNT(I) = 0.0
         EDISYR(I) = 0.0
!
!        DO COMPUTATIONS FOR ALL PHASE-IN PLANS
!
         IF (NPI .NE. 0) THEN
            DO IPI=1,NPI
!
!              COMPUTATION NOT APPLICABLE IF PHASE-IN PLAN IS NOT
!              FOR CURRENT REGION AND OWNERSHIP TYPE
!
               IF ((IROPI(1,IPI) .EQ. NRGN) .AND. (IROPI(2,IPI) .EQ. I)) THEN
!
!                 IF PHASE-IN STARTED BEFORE INITIAL YEAR (CURIYR+UHBSYR=UESTYR)
!                 THEN DO CALCULATIONS FOR PREVIOUS YEARS
!
                  IF ((FULLYR .EQ. UESTYR) .AND. (IBYRPI(IPI) .LE. UESTYR - UHBSYR - 1)) CALL ELPIPY(I,IPI)
                  IYRPI = CURIYR - IBYRPI(IPI) + 1
                  YRPI = IYRPI
!
!                 COMPUTATION NOT APPLICABLE FOR PHASE-IN PLANS THAT ARE
!                 NOT YET STARTED OR ALREADY RETIRED
!
                  IBKLF = PIBKLF
                  IF ((IYRPI .GE. 1) .AND. (IYRPI .LE. IBKLF)) THEN
!
!                    IF PHASE-IN IS COMPLETE, THEN ONLY CHECK FOR DISALLOWANCE
!
                     IF (IYRPI .LE. LPI(IPI)) THEN
!
!                       PHASE IN DOES NOT START UNTIL UTILITY BEGINS TO RECOVER COSTS
!                       UNTIL THEN, CAPITALIZED COSTS WILL BE RECOVERED OVER LIFE OF PLANT
!
                        IF (IYRPI .NE. 1) THEN
                           IF ((PIDFS(IYRPI,IPI) .NE. 0.0) .AND. (PIDFS(IYRPI-1,IPI) .EQ. 0.0)) THEN
                              EBBKVL(EBNUM,I) = EBBKVL(EBNUM,I) + PIDEF(IPI)
                              PIBKVL(IPI) = PIBKVL(IPI) + PIDEF(IPI)
                              PIDEF(IPI) = 0.0
                           ENDIF
                        ENDIF
!
!                       CALCULATE DEFERRED AMOUNT FOR THIS YEAR
!
                        AVERB = 0.0
                        IF (OPAVRB(I) .EQ. 1) AVERB = 0.5
                        RR = PIBKVL(IPI)*(1.0 - DISPER(IPI)) &
                             *(1.0 - ((YRPI - AVERB)/PIBKLF)) &
                             *(((ESPRLT(I)*ESEMDL(I)) + &
                             (ESPRST(I)*ESRTST(I))) &
                             + (((ESPRCE(I)*ESRTCE(I)) + &
                             (ESPRPS(I)*ESEMPL(I)))/ &
                             (1.0 - ESFTXR(I)))) + &
                             ((PIBKVL(IPI)/PIBKLF) &
                             *(1.0 + (((1.0 - PITXBS)*ESFTXR(I))/ &
                             (1.0-ESFTXR(I)))))
                        DEFCST = RR*(1.0 - PIDFS(IYRPI,IPI))* (1.0 - ESFTXR(I))
!
!                       CALCULATE RECOVERY OF PAST DEFERRED AMOUNTS
!
                        DEFCST = DEFCST - (PIRCS(IYRPI,IPI)* PIDEF(IPI))
!
!                       CALCULATE RETURN ON DEFERRED PORTION
!
                        PIRET = 0.0
                        IF (IRDPI(IPI) .NE. 2) PIRET = (PIDEF(IPI) + (0.5*DEFCST))* ESAFDC(I)
!
!                       UPDATE CUMULATIVE DEFERRED REVENUES FOR PLANT
!
                        PIDEF(IPI) = PIDEF(IPI) + DEFCST + PIRET
                        IF (IYRPI .EQ. LPI(IPI)) THEN
                           DEFCST = DEFCST - PIDEF(IPI)
                           PIDEF(IPI) = 0.0
                           PIRET = 0.0
                        ENDIF
!
!                       UPDATE REGIONAL TOTALS
!
                        EPIDEF(I) = EPIDEF(I) + PIDEF(IPI)
                        EPIND(I) = EPIND(I) + DEFCST
                        EPIRET(I) = EPIRET(I) + PIRET
                     ENDIF
!
!                    CALCULATE IMPACTS OF DISALLOWANCE
!
                     IF (DISPER(IPI) .NE. 0.0) THEN
                        EDISNT(I) = EDISNT(I) + ((PIBKVL(IPI)* (1.0 - (YRPI/PIBKLF)))* DISPER(IPI))
!
!                       COMPUTE DEPRECIATION EXPENSE FOR DISALLOWED PLANT
!
                        ERBDED(I) = ERBDED(I) - ((PIBKVL(IPI)* DISPER(IPI))/PIBKLF)
                        IF (IYRPI .EQ. 1) EDISYR(I) = EDISYR(I) + (PIBKVL(IPI)*DISPER(IPI))
                     ENDIF
                  ENDIF
               ENDIF
            END DO
         ENDIF
      END DO
      RETURN
      END

      SUBROUTINE ELPIPY(I,IPI)
      IMPLICIT NONE
!*******************************************************************
! THIS SUBROUTINE CALCULATES THE IMPACT OF PHASE-IN/DISALLOWANCE PLANS OF NEW CAPACITY FOR YEARS BEFORE THE MODEL STARTS
!    IT WORKS EXACTLY THE SAME WAY AS THE SUBROUTINE ELPHIN WHICH CALLS THIS SUBROUTINE
!*******************************************************************
! INPUT VARIABLES:
!     EIDIST = PLANT CODE FOR DISTRIBUTION CAPITAL
!     EBPTYP = PLANT TYPE FOR EACH BUILD
!     CURIYR  = CURRENT YEAR INDEX
!     EBBKVL  = BOOKED VALUE OF ASSETS
!     ESPRLT = PERCENT OF CAPITAL OBTAINED WITH LONG TERM DEBT
!     EBSYR  = YEAR WHICH PLANT COMES ON LINE
!     ESRTST = COST OF NEW SHORT TERM DEBT
!     ESPRST = PERCENT OF CAPITAL OBTAINED WITH SHORT TERM DEBT
!     ESEMDL = LAST YEARS EMBEDDED COST OF DEBT
!     ESPRPS = PERCENT OF CAPITALIZATION FROM PREFERED STOCK
!     ESRTCE = COST OF COMMON EQUITY
!     ESPRCE = PERCENT OF CAPITALIZATION FROM COMMON EQUITY
!     ESAFDC = AFUDC RATE
!     ESFTXR = FEDERAL STATUTORY TAX RATE
!     ESEMPL = EMBEDDED COST FOR THE PREF. STOCK FOR LAST YR
!     NPI = TOTAL NUMBER OF PHASE-IN PLANS
!     LPI = LENGTH OF PHASE-IN PLAN
!     IROPI(1,IPI) = REGION OF PHASE-IN PLAN
!     IROPI(2,IPI) = OWNERSHIP TYPE OF PHASE-IN PLAN
!     IBYRPI = 1ST YEAR OF PHASE-IN PERIOD
!     PIBKVL = BOOK VALUE OF PLANT TO BE PHASED-IN
!     PIDFS = CUMULATIVE FRACTION OF TOTAL COST PHASED-IN BY
!             YEAR
!     PIRCS = FRACTION OF REMAINING DEFERRED REVENUES TO BE
!             RECOVERED BY YEAR
!     PIDEF = TOTAL CUMULATIVE DEFERRED REVENUES
!     IRDPI = CAPITALIZE RETURN ON DEFERRED COST? (1=YES,2=NO)
!     PIBKLF = BOOK LIFE OF PHASED-IN PLANT
!     PITXBS = TAX BASIS AS A FRACTION OF BOOKED COST OF
!              PHASE-IN PLANT
!     DISPER = FRACTION OF PLANT TOTALLY DISALLOWED
! INTERNAL VARIABLES:
!     IYRPI = INDEX OF 1ST YEAR OF PHASE-IN RELATIVE TO CURRENT YEAR
!     YRPI = SAME AS IYRPI BUT IN A REAL VARIABLE
!     IBKLF = BOOK LIFE OF PLANT TO BE PHASED-IN
!     AVERB = ADJUSTMENT FOR COMPUTING REVENUE REQUIREMENT USING
!             YEARLY AVERAGE OR YEAR-END RATE BASE
!     RR = REVENUE REQUIREMENT
!     DEFCST = DEFERRED COST
!     PIRET = RETURN ON DEFERRED PORTION
! OUTPUT VALUES:
!     ERBDED = DEPRECIATION EXPENSE FOR DISALLOWED PLANT
!     EPIDEF = TOTAL CUMULATIVE DEFERRED PHASE-IN REVENUES
!     EPIND = NET-DEFERRED PHASE-IN REVENUES FOR YEAR
!     EPIRET = CAPITALIZED RETURN ON DEFERRED REVENUES FOR YEAR
!     EDISNT = NET DISALLOWED PLANT FOR YEAR
!     EDISYR = GROSS PLANT DISALLOWED IN THIS YEAR
!******************************************************************
      include 'phasin'
      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'efpgen'
      include 'efprc'
      include 'efpr'
      include 'efprcy'
      include 'eusprc'
      include 'efpint'
      include 'efpbld'
      include 'control'
      INTEGER I
      INTEGER IPI
      INTEGER IYRPI
      INTEGER IBKLF
      INTEGER K
      REAL*4 YRPI
      REAL*4 AVERB
      REAL*4 RR
      REAL*4 DEFCST
      REAL*4 PIRET
      DO IYRPI = 1 , UESTYR - UHBSYR - IBYRPI(IPI)
         YRPI = IYRPI
!
!        COMPUTATION NOT APPLICABLE FOR PHASE-IN PLANS THAT ARE
!        NOT YET STARTED OR ALREADY RETIRED
!
         IBKLF = PIBKLF
         IF ((IYRPI .GE. 1) .AND. (IYRPI .LE. IBKLF)) THEN
!
!           IF PHASE-IN IS COMPLETE, THEN ONLY CHECK FOR DISALLOWANCE
!
            IF (IYRPI .LE. LPI(IPI)) THEN
!
!              PHASE IN DOES NOT START UNTIL UTILITY BEGINS TO RECOVER COSTS
!              UNTIL THEN, CAPITALIZED COSTS WILL BE RECOVERED OVER LIFE OF PLANT
!              THESE COSTS ARE ALREADY IN HISTORICAL FORM 1 NUMBERS, SO DONT ADD AGAIN
!
               IF (IYRPI .NE. 1) THEN
                  IF ((PIDFS(IYRPI,IPI) .NE. 0.0) .AND. &
                     (PIDFS(IYRPI-1,IPI) .EQ. 0.0)) THEN
                     PIDEF(IPI) = 0.0
                  ENDIF
               ENDIF
!
!              CALCULATE DEFERRED AMOUNT FOR THIS YEAR
!
               AVERB = 0.0
               IF (OPAVRB(I) .EQ. 1) AVERB = 0.5
               RR = PIBKVL(IPI)*(1.0 - DISPER(IPI)) &
                    *(1.0 - ((YRPI - AVERB)/PIBKLF)) &
                    *(((ESPRLT(I)*ESEMDL(I)) + &
                    (ESPRST(I)*ESRTST(I))) &
                    + (((ESPRCE(I)*ESRTCE(I)) + &
                    (ESPRPS(I)*ESEMPL(I)))/ &
                    (1.0 - ESFTXR(I)))) + &
                    ((PIBKVL(IPI)/PIBKLF) &
                    *(1.0 + (((1.0 - PITXBS)*ESFTXR(I))/ &
                    (1.0-ESFTXR(I)))))
               DEFCST = RR*(1.0 - PIDFS(IYRPI,IPI))* &
                        (1.0 - ESFTXR(I))
!
!              CALCULATE RECOVERY OF PAST DEFERRED AMOUNTS
!
               DEFCST = DEFCST - (PIRCS(IYRPI,IPI)* &
                        PIDEF(IPI))
!
!              CALCULATE RETURN ON DEFERRED PORTION
!
               PIRET = 0.0
               IF (IRDPI(IPI) .NE. 2) &
                  PIRET = (PIDEF(IPI) + (0.5*DEFCST))* &
                          ESAFDC(I)
!
!              UPDATE CUMULATIVE DEFERRED REVENUES FOR PLANT
!
               PIDEF(IPI) = PIDEF(IPI) + DEFCST + PIRET
               IF (IYRPI .EQ. LPI(IPI)) THEN
                  DEFCST = DEFCST - PIDEF(IPI)
                  PIDEF(IPI) = 0.0
                  PIRET = 0.0
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      RETURN
      END
!
      SUBROUTINE ELSL(NRGN)
      IMPLICIT NONE
!******************************************************************
! THIS SUBROUTINE ADJUSTS THE RESULTS OF EFP FOR THE IMPACT OF SALES AND SUBSEQUENT LEASEBACK OF UTILITY PLANT
!   IN ORDER TO CONSERVE SPACE AND NOT PRODUCE ANY ADDITIONAL BUILDS,
!   THE MOST RECENT DISTRIBUTION BUILD IS ADJUSTED TO REFLECT IMPACT.
!   IT RELIES ON 2 SIMPLIFICATIONS:
!    (1) EVEN THOUGH THE ALGORITHM ASSUMES THAT PROCEEDS FROM SALE
!        ARE USED TO RETIRE HIGH COST DEBT AND EQUITY, NO SPECIAL
!        ADJUSTMENTS ARE MADE TO THE EMBEDDED INTEREST RATES,
!    (2) THE TAX ON THE GAIN IS NOT REFLECTED IN THE CURRENT INCOME
!        TAX CALCULATIONS, BUT RATHER THE ALGORITHM USES THE
!        AFTER TAX GAIN (THIS STILL PRODUCES CORRECT RESULTS).
! INPUT VARIABLES:
!        NSL = TOTAL NUMBER OF SALE/LEASEBACK TRANSACTIONS
!        IROSL(1,ISL) = REGION OF SALE/LEASEBACK
!        IROSL(2,ISL) = OWNERSHIP TYPE OF SALE/LEASEBACK
!        IBYRSL = 1ST YEAR OF SALE/LEASEBACK PERIOD
!        SLBKVL = BOOK VALUE OF SALE/LEASEBACK PLANT
!        SLPROC = GROSS SALE PROCEEDS
!        SLGAIN = NET OF TAX GAIN OVER BOOK VALUE FROM SALE
!        SLTERM = TERM OF THE LEASE (YEARS)
!        SLLP = ANNUAL LEASE PAYMENT
!        NRGN = CURRENT REGION
!        EIDIST = INDEX OF DISTRIBUTION BUILD
!        EBSYR = START YEAR OF BUILD
!        CURIYR = CURRENT YEAR
!        ESFTXR = FEDERAL TAX RATE
! INTERNAL VARIABLES:
!        ISL = INDEX OF SALE/LEASEBACK PLAN
!        IYRSL = YEAR OF SALE/LEASEBACK PLAN
!        YRSL = SAME AS IYRSL EXCEPT AS REAL VARIABLE
!        ASVL = ASSET VALUE OF SALE/LEASEBACK PLAN
!        AMOR = ?????
! MODIFIED VARIABLES:
!        EBBKVL = BOOK VALUE OF BUILD
!        ERDLRB = CHANGE IN RATE BASE DUE TO NEW BUILDS GOING INTO
!                 RATE BASE ON JANUARY 1.  (I.E., IF A PLANT GOES
!                 INTO SERVICE ON JAN 1, THE AVERAGE RATE BASE FOR
!                 THE YEAR IS NOT AVERAGE OF DEC 31, YEAR AND
!                 DEC 31, YEAR MINUS 1.)
!        EBASVL = ASSET VALUE OF BUILD
! OUTPUT VALUES:
!        ESLPRC = NET OF TAX SALES PROCEEDS
!        ESLLP = TOTAL REGIONAL LEASE PAYMENT
!        ESLAGN = TOTAL REGIONAL AMORTIZATION OF GAIN FROM
!                 SALE/LEASEBACK
!        ESLNDG = TOTAL REGIONAL NET DEFERRED GAIN
!******************************************************************
      include 'salelb'
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'efpbld'
      include 'efpgen'
      include 'eusprc'
      include 'efpint'
      include 'efprcy'
      INTEGER NRGN
      INTEGER I
      INTEGER ISL
      INTEGER IYRSL
      INTEGER J
      REAL*4 YRSL
      REAL*4 ASVL
      REAL*4 AMOR
      DO I=1,2
         ESLPRC(I) = 0.0
         ESLLP(I) = 0.0
         ESLAGN(I) = 0.0
         ESLNDG(I) = 0.0
! COMPUTE IMPACT FOR EACH SALE/LEASEBACK PLAN
         IF (NSL .NE. 0) THEN
            DO ISL=1,NSL
! CHECK REGION AND OWNERSHIP OF PLAN
               IF ((IROSL(1,ISL) .EQ. NRGN) .AND. &
                  (IROSL(2,ISL) .EQ. I)) THEN
                  IYRSL = CURIYR - IBYRSL(ISL) + 1
                  YRSL = IYRSL
! CHECK IF SALE/LEASEBACK PLAN HAS STARTED
                  IF ((IYRSL .GE. 1) .AND. &
                     (IYRSL .LE. SLTERM(ISL))) THEN
! IF SALE/LEASEBACK OCCURS IN THIS YEAR, THEN ADJUST
! DISTRIBUTION BUILD
                     IF (IYRSL .EQ. 1) THEN
! ADJUST FOR SALE/LEASEBACK, CALCULATE PROCEEDS, TAX, AND NET GAIN
                        EBBKVL(EBNUM,I) = EBBKVL(EBNUM,I) - &
                                           SLBKVL(ISL)
                        ERDLRB(I) = ERDLRB(I) - SLBKVL(ISL)
                        IF (ESFTXR(I) .NE. 0.0) THEN
                           ASVL = SLPROC(ISL) - SLTAXS(ISL)/ESFTXR(I)
                        ELSE
                           ASVL = SLBKVL(ISL)
                        ENDIF
                        EBASVL(EBNUM,I) = EBASVL(EBNUM,I) - ASVL
                        ESLPRC(I) = ESLPRC(I) + SLPROC(ISL) - &
                                    SLTAXS(ISL)
                     ENDIF
! DO CALCULATIONS FOR EACH YEAR OF LEASE
                     ESLLP(I) = ESLLP(I) + SLLP(ISL)
                     AMOR = SLGAIN(ISL)/SLTERM(ISL)
                     ESLAGN(I) = ESLAGN(I) + AMOR
                     ESLNDG(I) = ESLNDG(I) + SLGAIN(ISL) - AMOR*YRSL
                  ENDIF
               ENDIF
            END DO
         ENDIF
      END DO
      RETURN
      END
      FUNCTION ELCEXP(JYR,LCP)
      IMPLICIT NONE
!***********************************************************
!   THIS FUNCTION COMPUTES THE PERCENTAGE OF TOTAL CONSTRUCTION EXPENDITURES THAT WILL OCCUR DURING THE GIVEN YEAR IN THE CONSTRUCTION PERIOD
!***********************************************************
      INTEGER JYR
      INTEGER LCP
      INTEGER IYR
      INTEGER ILCP
      REAL*4 ELCEXP
      REAL*4 CLT
      REAL*4 YCUR
      REAL*4 CEXPC
      REAL*4 YPRE
      REAL*4 CEXPL
      IF (JYR .EQ. 1) THEN
         ELCEXP = 0.0
      ELSE
         IYR = LCP - JYR + 1
         ILCP = LCP - 1
         CLT = FLOAT(ILCP)
         YCUR = FLOAT(IYR)
         CEXPC = (1.0 - (COS(1.570796*YCUR/CLT))**4.08198)**3.24948
         YPRE = FLOAT(IYR-1)
         CEXPL = (1.0 - (COS(1.570796*YPRE/CLT))**4.08198)**3.24948
         ELCEXP = CEXPC - CEXPL
      ENDIF
      RETURN
      END
      SUBROUTINE ELXPNS(NRGN,ICALL)
      IMPLICIT NONE
!*******************************************************************
!    THIS SUBROUTINE CALCULATES NOMINAL FUEL AND O&M EXPENSES, AND WORKING CAPITAL
!*******************************************************************
! INPUT VARIABLES:
!     EFPFL  = TOTAL FUEL COSTS IN REAL DOLLARS
!     EFPNUC = TOTAL NUCLEAR FUEL COSTS IN REAL DOLLARS
!     EFPOM  = TOTAL GENERATION RELATED O&M EXPENSES REAL DOLLARS
!     MC_JPGDP = GENERAL INFLATION RATE FROM BASE YEAR TO CURRENT YEAR
!     ESTSHR = PUBLIC/PRIVATE GENERATION SHARE
! OUTPUT VARIABLES:
!     ERTFLN = TOTAL FUEL COSTS IN NOMINAL DOLLARS
!     ERNFSN = NUCLEAR FUEL STOCK IN NOMINAL DOLLARS
!     ERTOMN = TOTAL O&M IN NOMINAL DOLLARS
!     ERWC   = WORKING CAPITAL
!*******************************************************************
      include 'parametr'
      include 'emmparm'
      include 'efpgen'
      include 'eusprc'
      include 'efpint'
      include 'macout'
      include 'efprcy'
      include 'ncntrl'
      include 'efple'
      include 'efprp2'
      INTEGER I,NRGN
      CHARACTER*1 ICALL

!     SHARE GENERATION COSTS BETWEEN PRIVATE AND PUBLIC FIRMS ACCORDING
!     TO HISTORICAL SHARE OF GENERATION

      DO I=1,2
         ERTFLN(I)=0.0
         ERTOMN(I)=0.0
         IF (ESTSHR(I) .NE. 0.0) THEN

!           CALCULATE NOMINAL FUEL COSTS

            ERTFLN(I) = EFPFL + BLKSUM
            ERTFLN(I) = ERTFLN(I) * ESTSHR(I) * MC_JPGDP(CURIYR)

!           CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!           WRITE(*,10)ESTSHR(I),I,NRGN
!   10      FORMAT(' ELXPNS - ESTSHR(I):',F8.4,' I,NRGN:',I3,I3)
!           CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!                 IF (NRGN .EQ. 20)WRITE(6,*)'ERTFLN ',CURIYR,NRGN,I,EFPFL,BLKSUM,ERTFLN(I), ESTSHR(I),MC_JPGDP(CURIYR)

!           CALCULATE THE NUCLEAR FUEL STOCK AS 2 YEARS SUPPLY OF NUCLEAR FUEL

            ERNFSN(I) = 3.5*EFPNUC*ESTSHR(I)*MC_JPGDP(CURIYR)

!           CALCULATE NOMINAL O&M COSTS

            ERTOMN(I) = (EFPOM*ESTSHR(I))*MC_JPGDP(CURIYR)

!              CALCULATE WORKING CAPITAL AS 1/8 OF TOTAL FUEL AND O&M COSTS

               ERWC(I) = 0.125*(ERTOMN(I) + ERTFLN(I))
         ENDIF
      END DO
      BLKSUM = BLKSUM * MC_JPGDP(CURIYR)
      PPWRBLK(1) = PPWRBLK(1) * MC_JPGDP(CURIYR)
      PPWRBLK(2) = PPWRBLK(2) * MC_JPGDP(CURIYR)
      PPWRBLK(3) = PPWRBLK(3) * MC_JPGDP(CURIYR)
      PPWRBLK(4) = PPWRBLK(4) * MC_JPGDP(CURIYR)
!            IF (NRGN .EQ. 20)WRITE(*,*)'PPWRBLK  ',BLKSUM,PPWRBLK(1),PPWRBLK(2),PPWRBLK(3),PPWRBLK(4)
      RETURN
      END
      SUBROUTINE ELINEX(NRGN)
      IMPLICIT NONE
!*******************************************************************
!             THIS SUBROUTINE CALCULATES INTEREST EXPENSE
!*******************************************************************
! INPUT VARIABLES:
!     ESEMPB = BASE YEARS EMBEDDED COST OF PREFERRED STOCK
!     ESPRPS = PERCENT OF PREFERRED STOCK
!     ERWC   = WORKING CAPITAL
!     ERTUP  = TOTAL UTILITY PLANT IN SERVICE
!     ERBCWP = BOOKED CWIP
!     ERABDE = TOTAL ACCUMULATED BOOK DEPRECIATION
!     ERPRDF = TOTAL DEFERRED TAXES FROM EXCESS TAX OVER BOOK
!     ERDITC = ACCUMULATED DEFERRED ITC
!     ERNFSN = NUCLEAR FUEL STOCK
!     ESPRLT = PERCENT OF CAPITAL OBTAINED WITH LONG TERM DEBT
!     ERBDE  = BOOK DEPRECIATION EXPENSE
!     ESEMDL = LAST YEARS EMBEDDED COST OF DEBT
!     ESEMDB = BASE YEARS EMBEDDED COST OF DEBT
!     ESRTLT = COST OF NEW LONG TERM DEBT
!     ERPFLPS  = LAST YEARS OUTSTANDING PREFERRED STOCK
!     ERPRFL = LAST YEARS PREFERRED STOCK
!     ERAMDL = LAST YEARS VALUE OF ASSETS TO BE FINANCED
!     ESRTST = COST OF NEW SHORT TERM DEBT
!     ESPRST = PERCENT OF CAPITAL OBTAINED WITH SHORT TERM DEBT
!     ESEMPL = EMBEDDED COST OF PREFERRED STOCK FR LAST YR
!     ESTSHR = PUBLIC/PRIVATE GENERATION SHARE
!     EDISNT = NET DISALLOWED PLANT FOR YEAR
!     EPIDEF = TOTAL CUMULATIVE DEFERRED PHASE-IN REVENUES
!     ESLNDG = TOTAL REGIONAL NET DEFERRED GAIN
!     ESLPRC = NET OF TAX SALES PROCEEDS
!     ESRTPS = COST OF PREFERED STOCK
!     ELPRST = LAST YEAR'S PERCENT OF CAPITAL OBTAINED WITH
!              SHORT TERM DEBT
!     ESPRCE = PERCENT OF CAPITALIZATION FROM COMMON EQUITY
!     ELPRCE = LAST YEAR'S PERCENT OF CAPITALIZATION FROM
!              COMMON EQUITY
! INTERNAL VARIABLES:
!     RETIRE = ?????
! OUTPUT VARIABLES:
!     ERPREF = TOTAL NEW PREFERRED STOCK
!     ERPF   = TOTAL OUTSTANDING PREFERRED STOCK
!     ERAMD  = TOTAL VALUE OF ASSETS TO BE FINANCED
!     ERBOND = NEW LONG TERM DEBT
!     EROB   = OUTSTANDING BONDS
!     ESEMDT = EMBEDDED COST OF DEBT
!     ERLIEX = LONG TERM INTEREST EXPENSE
!     ERSIEX = SHORT TERM INTEREST EXPENSE
!     ERTIEX = TOTAL INTEREST EXPENSE
!     ESEMPS = EMBEDDED COST OF PREFERRED STOCK
!     EROBL  = LAST YEARS OUTSTANDING BONDS
!     ERBNDL = LAST YEARS LONG TERM DEBT
!     ERPSDV = TOTAL PREFERRED STOCK DIVIDENDS - CURRENT YEAR
!     ERAVCE = AVERAGE YEAR COMMON EQUITY BALANCE - CURRENT YEAR
!*******************************************************************
      include 'parametr'
      include 'emmparm'
      include 'efpgen'
      include 'eusprc'
      include 'efpint'
      include 'efpr'
      include 'efprc'
      include 'efprcy'
      include 'ncntrl'
      include 'control'
      REAL*4 RETIRE
      INTEGER I
      INTEGER NRGN,FULLYR
      FULLYR = USYEAR(CURIYR)

!     DO ALL CALCULATIONS FOR PUBLIC AND PRIVATE UTILITIES

      DO I=1,2
         IF (ESTSHR(I) .NE. 0.0) THEN

!           CALCULATE THE VALUE OF ASSETS THAT NEED TO BE FINANCED

            ERAMD(I) = ERTUP(I) + ERBCWP(I) + ERWC(I) - ERABDE(I) - &
                       ERPRDF(I) - ERDITC(I) + ERNFSN(I) - &
                       EDISNT(I) + EPIDEF(I) - ESLNDG(I)

!           CALCULATE THE EMBEDDED COST OF LONG TERM DEBT

            ERBOND(I) = 0.0
            IF (ERAMD(I) .NE. 0.0) THEN
               ERBOND(I) = ERAMD(I)*ESPRLT(I)

!              IN FIRST YEAR, CHECK FOR DATA CONSISTENCY PROBLEMS

               IF (FULLYR .EQ. UESTYR) THEN
                  IF (ERBOND(I) .LT. EROB(I)) THEN

!                    WRITE(69,10)I,ERBOND(I),EROB(I)
!  10                FORMAT(' WARNING:  IN REGION ',I2,' AND OWN ',
!    1                      I2,' BOND REQUIREMENTS WERE LOWER ',
!    2                      'THAN EXISTING BONDS:',2(F10.1,1X))

                     EROB(I) = ERBOND(I)
                     EROBL(I) = ERBOND(I)
                     ERBNDL(I) = ERBOND(I)
                  ENDIF
               ENDIF

!              CONTINUE WITH DEBT CALCULATIONS

               RETIRE = ESPRLT(I)*ERBDE(I)
               EROB(I) = EROBL(I)-RETIRE
               IF (EROB(I) .LE. 0.0) THEN
                  RETIRE = RETIRE + EROB(I)
                  EROB(I) = 0.0
               ENDIF
               IF (ERBOND(I) .EQ. 0.0) ESEMDT(I) = 0.0
               IF (ERBOND(I) .GT. 0.0) ESEMDT(I) = (ESEMDL(I)* &
                                                ERBNDL(I) - &
                                                ESEMDB(I)* &
                                                RETIRE + &
                                                ESRTLT(I)* &
                                                (ERBOND(I) - &
                                                ERBNDL(I) + &
                                                RETIRE))/ &
                                                ERBOND(I)
            ENDIF

!           CALCULATE THE EMBEDDED COST OF PREFERRED STOCK

            ERPREF(I) = 0.0
            IF (ERAMD(I) .NE. 0.0) THEN
               ERPREF(I) = ERAMD(I)*ESPRPS(I)
               RETIRE = ESPRPS(I)*ERBDE(I)
               ERPF(I) = ERPFLPS(I)-RETIRE
               IF (ERPF(I) .LE. 0.0) THEN
                  RETIRE = RETIRE + ERPF(I)
                  ERPF(I) = 0.0
               ENDIF
               IF (ERPREF(I) .EQ. 0.0) ESEMPS(I) = 0.0
               IF (ERPREF(I) .NE. 0.0) THEN
                  ESEMPS(I) = (ESEMPL(I)*ERPRFL(I) - ESEMPB(I)* &
                        RETIRE + ESRTPS(I)*(ERPREF(I) - &
                        ERPRFL(I) + RETIRE))/ERPREF(I)
!                write(6,10) CURIYR,CURITR,NRGN,I,ESEMPS(I),ESEMPL(I),ERPRFL(I),ESEMPB(I),RETIRE,ESRTPS(I),ERPREF(I)
10    FORMAT(1x,4I6,7F15.5)
               ENDIF
            ENDIF

!           CALCULATE LONG AND SHORT TERM INTEREST EXPENSE

            ERLIEX(I) = ESEMDT(I)*(ERAMD(I)*ESPRLT(I) + ERAMDL(I)* &
                  ELPRLT(I) - ESLPRC(I)*ESPRLT(I))/2.0
            ERSIEX(I) = ESRTST(I)*(ERAMD(I)*ESPRST(I) + ERAMDL(I)* &
                  ELPRST(I) - ESLPRC(I)*ESPRST(I))/2.0
            ERTIEX(I) = ERLIEX(I) + ERSIEX(I)

!           CALCULATE PREFERRED DIVIDENDS AND AVERAGE COMMON EQUITY

            ERPSDV(I) = ESEMPS(I)*(ERAMD(I)*ESPRPS(I) + ERAMDL(I)* &
                  ELPRPS(I) - ESLPRC(I)*ESPRPS(I))/2.0
            ERAVCE(I) = (ERAMD(I)*ESPRCE(I) + ERAMDL(I)*ELPRCE(I) &
                  -ESLPRC(I)*ESPRCE(I))/2.0
         ENDIF
      END DO
      RETURN
      END
      SUBROUTINE ELREVS(ICALL,NRGN)
      IMPLICIT NONE
!*******************************************************************
!    THIS SUBROUTINE SOLVES FOR THE REVENUE REQUIREMENTS AND DETERMINES ALLOWED REVENUES BASED ON THE REGULATORY LAG
!*******************************************************************
! INPUT VARIABLES:
!     ERWC   = WORKING CAPITAL
!     ERTUP  = TOTAL UTILITY PLANT IN SERVICE
!     ERABDE = TOTAL ACCUMULATED BOOK DEPRECIATION
!     ERPRDF = TOTAL DEFERRED TAXES FROM EXCESS TAX OVER BOOK
!     ERNFSN = NUCLEAR FUEL STOCK
!     ERTFLN = TOTAL FUEL COSTS IN NOMINAL DOLLARS
!     ERTOMN = TOTAL O&M IN NOMINAL DOLLARS
!     ESPRLT = PERCENT OF CAPITAL OBTAINED WITH LONG TERM DEBT
!     ERBDE  = BOOK DEPRECIATION EXPENSE
!     ESRTST = COST OF NEW SHORT TERM DEBT
!     ESPRST = PERCENT OF CAPITAL OBTAINED WITH SHORT TERM DEBT
!     ESEMDT = EMBEDDED COST OF DEBT
!     ERTIEX = TOTAL INTEREST EXPENSE
!     EGTXRT = GENERAL TAX RATE
!     ESSTXR = STATE TAX RATE
!     ERRCWP = AMOUNT OF CWIP IN RATE BASE
!     ESPRCE = PERCENT OF CAPITALIZATION FROM COMMON EQUITY
!     ESRTCE = COST OF COMMON EQUITY
!     ESPRPS = PERCENT OF CAPITALIZATION FROM PREFERED STOCK
!     ESEMPS = EMBEDDED COST OF PREFERED STOCK
!     ERAITC = AMORTIZATION/YEAR OF DEFERRED ITC
!     ERFITC = AMOUNT OF ITC FLOWED THROUGH FOR RATEMAKING
!     ERAFDC = AMORTIZATION OF AFUDC TAX SAVINGS
!     ERFFDC = AMOUNT OF AFUDC TAX SAVINGS FLOWED THROUGH
!     EROFFS = AFUDC OFFSET
!     ERPRTX = PROPERTY TAX ON VALUE OF ASSETS
!     ERSLTX = SALES TAX ON YEARLY CWIP EXPENDITURES
!     ESFTXR = FEDERAL STATUTORY TAX RATE
!     ERTDWO = TAX DEPRECIATION W/O ACCELERATION
!     ERTDRG = DEPRECIATION EXPENSE USED IN FEDERAL TAX CALCULATIONS
!              FOR REGULATORY PURPOSES
!     EQTLSL = TOTAL ELECTRICITY SALES
!     ESTSHR = PUBLIC/PRIVATE GENERATION SHARE
!     EILAG  = REGULATORY LAG (0 TO 1 YRS BY .25; VAL = 1 TO 5 BY 1)
!     ERPSDV = TOTAL PREFERRED STOCK DIVIDENDS - CURRENT YEAR
!     ERAVCE = AVERAGE YEAR COMMON EQUITY BALANCE - CURRENT YEAR
!     ELBDE = LAST YEAR'S BOOK DEPRECIATION
!     ELTOMN = LAST YEAR'S TOTAL O&M IN NOMINAL DOLLARS
!       ELTFLN = LAST YEAR'S TOTAL FUEL COSTS IN NOMINAL DOLLARS
!     ELRCWP = LAST YEAR'S AMOUNT OF CWIP IN RATE BASE
!     ERATSF = ACCELERATED DEPRECIATION TAX SAVINGS - FLOWED THROUGH
!     EDISNT = NET DISALLOWED PLANT FOR YEAR
!     ERDLRB = CHANGE IN RATE BASE DUE TO NEW BUILDS GOING INTO
!     ERNFSN = NUCLEAR FUEL STOCK IN NOMINAL DOLLARS
!     ERDITC = ACCUMULATED DEFERRED ITC
!     ERCNAD = ACCUMULATED AMORTIZATION FOR CANCELLED PLANT
!     ESLNDG = TOTAL REGIONAL NET DEFERRED GAIN
!     ELLAGN = LAST YEAR'S TOTAL REGIONAL AMORTIZATION OF GAIN FROM
!              SALE/LEASEBACK
!     ELPRTX = LAST YEAR'S PROPERTY TAX ON VALUE OF ASSETS
!     ELSLTX = LAST YEAR'S SALES TAX ON YEARLY CWIP EXPENDITURES
!     CURIYR  = CURRENT YEAR INDEX
!     ELTDWO = LAST YEAR'S TAX DEPRECIATION W/O ACCELERATION
!     ELTLSL = LAST YEAR'S TOTAL ELECTRICITY SALES
!     ERCNBV = BOOK VALUE OF CANCELLED PLANT
!     ELRBRR = RATE BASE FROM PREVIOUS YEAR
!     ELAITC = LAST YEAR'S AMORTIZATION/YEAR OF DEFERRED ITC
!     ELFITC = LAST YEAR'S AMOUNT OF ITC FLOWED THROUGH FOR
!              RATEMAKING
!     ELLLP = LAST YEAR'S TOTAL REGIONAL LEASE PAYMENT
!     EREDTF=AMOUNT OF EXCESS TO BE FLOWED THROUGH THIS YEAR
!     EPIND = NET-DEFERRED PHASE-IN REVENUES FOR YEAR
!     ESLLP = TOTAL REGIONAL LEASE PAYMENT
!     ESLAGN = TOTAL REGIONAL AMORTIZATION OF GAIN FROM
!     ELTIEX = LAST YEAR'S TOTAL INTEREST EXPENSE
!     ELCIDC = LAST YEAR'S CAPITALIZED INTEREST DURING CONSTRUCTION
!     ERCIDC = CAPITALIZED INTEREST DURING CONSTRUCTION FOR TAXES
!     ESERCE = EARNED RETURN ON COMMON EQUITY
!     EDISYR = GROSS PLANT DISALLOWED IN THIS YEAR
!     EPIRET = CAPITALIZED RETURN ON DEFERRED REVENUES FOR YEAR
!     ERFDC  = YEARLY AFUDC
!     ELAFDC = LAST YEAR'S AMORTIZATION OF AFUDC TAX SAVINGS
!     ELFFDC = LAST YEAR'S AMOUNT OF AFUDC TAX SAVINGS
!              FLOWED THROUGH
!     ELOFFS = LAST YEAR'S AFUDC OFFSET
! INTERNAL VARIABLES:
!     LAG    = VALUE OF REGULATORY LAG SCENARIO FROM EILAG
!     RBLAST = LAST YEAR'S RATE BASE
! OUTPUT VARIABLES:
!     ERRB   = RATE BASE
!     ESRR   = RATE OF RETURN (WEIGHTED AVERAGE COST OF CAPITAL)
!     ERRVLG = ACTUAL REVENUES DEPENDING ON LAG SCENARIO
!     ERRVRQ = REVENUE REQUIREMENTS FOR CURRENT YEAR
!     ESRTCE = COST OF COMMON EQUITY
!     ERRBRR = ACTUAL RATE BASE USED FOR SETTING REV REQUIREMENTS
!     ELRB = LAST YEAR'S RATE BASE
!     ELRVRQ = LAST YEAR'S REVENUE REQUIREMENTS
!*******************************************************************
      include 'parametr'
      include 'emmparm'
      include 'tax'
      include 'efpr'
      include 'efprc'
      include 'efprcy'
      include 'efpgen'
      include 'eusprc'
      include 'efpint'
      include 'ncntrl'
      include 'macout'
      include 'control'
      REAL*4 ELRB(2)
      REAL*4 ELRVRQ(2)
      INTEGER I,NRGN,FULLYR
      INTEGER LAG
      REAL*4 RBLAST,ERR1,ERR2,CLOSS
      CHARACTER*1 ICALL


      REAL SV_PRGEN_ESRR(MNUMYR,MNUMNR)
      COMMON /STORE_ROR/SV_PRGEN_ESRR
!
!     DO ALL CALCULATIONS FOR BOTH PUBLIC AND PRIVATE UTILITIES
!
      FULLYR = USYEAR(CURIYR)
      DO I=1,2
         ERRVLG(I)=0.0
         IF (ESTSHR(I) .NE. 0.0) THEN
!
!           CALCULATE THE YEAR END RATE BASE
!
            ERRB(I) = ERTUP(I) + ERRCWP(I) + ERWC(I) - ERABDE(I) - ERPRDF(I) + ERNFSN(I) - (ERCNBV(I) - ERCNAD(I)) - ESLNDG(I)
!
!           DEDUCT DEFERRED ITC, IF REQUESTED
!
            IF ((OPITCRB(I) .EQ. 2) .AND. (ESFTXR(I) .NE. 0.0)) ERRB(I) = ERRB(I) - ERDITC(I)
!
!           CALCULATE RATE BASE
!
            IF (OPAVRB(I) .EQ. 2 .OR. CURIYR + UHBSYR .LE. EFPSYR) THEN
               ERRBRR(I) = ERRB(I) - ERAFDL(I)
            ELSE
!
!              CALCULATE RATE BASE AS YEAR AVERAGE
!
               RBLAST = ERRBL(I)
               IF (FULLYR .EQ. UESTYR) RBLAST = RBLAST + ERWC(I) + ERNFSN(I)
               ERRBRR(I) = (RBLAST + ERDLRB(I) + ERRB(I))/2.0
            ENDIF
!
!           CALCULATE RATE BASE AS YEAR END SUBTRACT OUT OF RATE BASE THE NET DISALLOWED PLANT
!
            ERRBRR(I) = ERRBRR(I) - EDISNT(I)
!
!           CALCULATE THE RETURN ON RATE BASE
!
            ESRR(I) = ESPRLT(I) * ESEMDT(I) + ESPRST(I) * ESRTST(I) + ESPRCE(I) * ESRTCE(I) + ESPRPS(I) * ESEMPS(I)
 !           WRITE(6,2041) 'CHECK ESRR ',CURIYR,CURITR,NRGN,I,ESRR(I), ESPRLT(I) , ESEMDT(I), ESPRST(I), ESRTST(I), ESPRCE(I) , ESRTCE(I), ESPRPS(I), ESEMPS(I)
2041      FORMAT(A25,1x,4(I4,1x),9(F12.3,1x))
!
!           lower return for regulated part of industry -
!
            if (ICALL .eq. 'T' .or. ICALL .eq. 'D') then
               ESRR(I) = ESRR(I) - RORADJ
            endif

            IF (ICALL .eq. 'G' .AND. I .eq. 1) &   ! store esrr for CAPCOST routine
               SV_PRGEN_ESRR(CURIYR,NRGN) = ESRR(I)
!              WRITE(6,2433)'LODAD SV_PRGEN_ESHR ', CURIYR, CURITR, NRGN, ESRR(I)   ,SV_PRGEN_ESRR(CURIYR,NRGN)
2433   FORMAT(A30,1x,3(I4,1x),2(F12.3,1x))

!
!           CALCULATE THE REVENUE REQUIREMENT.
!           THE EQUATION SHOWN BELOW IS THE SOLUTION TO 4 SIMULTANEOUS EQUATIONS INVOLVING THE REVENUE REQUIREMENTS.
!
            ERRVRQ(I) = (ERRBRR(I) * ESRR(I) + ERTFLN(I) + ERTOMN(I) + &
                        ERNDFPMT(I) + &
                        ERBDE(I) - ERAITC(I) - ERFITC(I) - &
                        ERAFDC(I) - ERFFDC(I) - EROFFS(I) - &
                        EREDTF(I) - EPIND(I) + ESLLP(I) - &
                        ESLAGN(I) + (ERPRTX(I) + ERSLTX(I)) * &
                        (1.0 - ESFTXR(I)) + (ERTFLN(I) + ERTOMN(I) + &
                        ERTDWO(I) + ERTIEX(I) + ESLLP(I)+ERNDFPMT(I)) * &
                        ((-1.0 * ESSTXR(I)) + ESSTXR(I) * ESFTXR(I)) + &
                        (ERTFLN(I) + ERTOMN(I) + ERTDRG(I) +ERNDFPMT(I)+ &
                        (ERTIEX(I) - ERCIDC(I)) + ESLLP(I)) * &
                        ((-1.0 * ESFTXR(I)))) / (1.0 - EGTXRT(I) - &
                        ESSTXR(I) - ESFTXR(I) + EGTXRT(I) * &
                        ESFTXR(I) + ESSTXR(I) * ESFTXR(I))
!
!           CALCULATE ELECTRIC REVENUES AS A FUNCTION OF REVENUE REQUIREMENTS AND THE REGULATORY LAG SCENARIO.
!
!           NO LAG TO 1 YEAR LAG IN INCREMENTS OF 1/4 YEAR
!
            LAG = EILAG(I)
!
            IF (LAG .EQ. -1) THEN
!
!              EILAG IS -1 -- EARNED RETURN ON EQUITY IS INPUT
!
               ERRVLG(I) = (ERAVCE(I) * ESERCE(I) + ERPSDV(I) + &
                           EDISYR(I) - EPIND(I) - EPIRET(I) - &
                           ERFDC(I) + ERTIEX(I) + ERTFLN(I) + &
                           ERTOMN(I) + ERBDE(I) - ERAITC(I) + &
                           ERNDFPMT(I) - &
                           ERFITC(I) - ERAFDC(I) - ERFFDC(I) - &
                           ERATSF(I) - EREDTF(I) + ESLLP(I) - &
                           ESLAGN(I) + (ERPRTX(I) + ERSLTX(I)) * &
                           (1.0 - ESFTXR(I)) + (ERTFLN(I) + &
                           ERTOMN(I) + ERTDWO(I) + ERTIEX(I) + &
                           ERNDFPMT(I) + &
                           ESLLP(I)) * ((-1.0 * ESSTXR(I)) + &
                           ESSTXR(I) * ESFTXR(I)) + (ERTFLN(I) + &
                           ERTOMN(I) + ERTDWO(I) + (ERTIEX(I) - &
                           ERCIDC(I)) + ESLLP(I) + ERNDFPMT(I)) * &
                           ((-1.0 * ESFTXR(I)))) / (1.0 - EGTXRT(I) - &
                           ESSTXR(I) - ESFTXR(I) + EGTXRT(I) * &
                           ESFTXR(I) + ESSTXR(I) * ESFTXR(I))
            ELSE IF (LAG .GT. 1) THEN
!
!              CALCULATE REVENUE REQUIREMENT ASSUMING HISTORICAL YEAR COSTS.
!
               ELRB(I) = ELRBRR(I) + (ERRCWP(I) - ELRCWP(I))
               ELRVRQ(I) = ((ELRB(I) * ESRR(I) + ELTOMN(I) + &
                           + ELTFLN(I) + ERNDFPMT(I) + &
                           ELBDE(I) - ELAITC(I) - ELFITC(I) - &
                           ELAFDC(I) - ELFFDC(I) - ELOFFS(I) - &
                           EREDTF(I) - EPIND(I) + ELLLP(I) - &
                           ELLAGN(I) + (ELPRTX(I) + &
                           ELSLTX(I)) * (1.0 - ESFTXR(I)) + &
                           (ELTOMN(I)+ ELTFLN(I) + ELTDWO(I) + &
                           ELTIEX(I) + ELLLP(I) + ERNDFPMT(I)) * &
                           ((-1.0 * ESSTXR(I)) + ESSTXR(I) * &
                           ESFTXR(I)) + (ELTOMN(I) + ELTFLN(I)+ &
                           ERTDRG(I)+ERNDFPMT(I) + (ELTIEX(I) - &
                           ELCIDC(I)) + ELLLP(I)) * &
                           (-1.0 * ESFTXR(I))) * EQTLSL(I) / &
                           ELTLSL(I))/ &
                           (1.0 - EGTXRT(I) - ESSTXR(I) - &
                           ESFTXR(I) + EGTXRT(I) * ESFTXR(I) + &
                           ESSTXR(I) * ESFTXR(I))
               IF (LAG .EQ. 3) THEN
!
!                 EILAG IS 3 -- 1/2 YEAR REGULATORY LAG
!
                  ERRVLG(I) = (0.5 * ELRVRQ(I)) + (0.5 * ERRVRQ(I))
               ELSE IF (LAG .EQ. 4) THEN
!
!                 EILAG IS 4 -- 3/4 YEAR REGULATORY LAG
!
                  ERRVLG(I) = (0.75 * ELRVRQ(I)) + (0.25 * ERRVRQ(I))
               ELSE IF (LAG .EQ. 5) THEN
!
!                 EILAG IS 5 -- 1 YEAR REGULATORY LAG
!
                  ERRVLG(I) = ELRVRQ(I)
               ELSE
!
!                 EILAG IS 2 -- 1/4 YEAR REGULATORY LAG
!
                  ERRVLG(I) = (0.25 * ELRVRQ(I)) + (0.75 * ERRVRQ(I))
               ENDIF
            ELSE
!
!              EILAG IS 1 -- NO REGULATORY LAG
!
               ERRVLG(I) = ERRVRQ(I)
            ENDIF
         ENDIF
!
         IF (ICALL .EQ. 'T') THEN
            ERRVLG(I) = ERRVLG(I) * OMADJTR(CURIYR)
         ELSEIF (ICALL .EQ. 'D') THEN
            ERRVLG(I) = ERRVLG(I) * OMADJDS(CURIYR)
         ENDIF
      END DO

      RETURN
      END
!
      SUBROUTINE STMTS(NRGN,ICALL)
      IMPLICIT NONE
!*****************************************************************
! THIS SUBROUTINE PREPARES THE FINANCIAL STATEMENTS
!*****************************************************************
! INPUT VARIABLES:
!   ICALL = OPERATING COMPONENT INDICATOR
!   NRGN = CURRENT REGION
!   CURIYR = CURRENT YEAR
!   NOCAP = NUMBER OF PLANT TYPE REPORTS
!   ESTSHR = PUBLIC/PRIVATE GENERATION SHARE
!   IPOWN = PRINT FLAGS BY OWNERSHIP TYPE
!   ERTUP = TOTAL UTILITY PLANT IN SERVICE
!   ERCNBV = BOOK VALUE OF CANCELLED PLANT
!   EDISNT = NET DISALLOWED PLANT FOR YEAR
!   ERRCWP = AMOUNT OF CWIP IN RATE BASE
!   ERNFSN = NUCLEAR FUEL STOCK IN NOMINAL DOLLARS
!   ERWC = WORKING CAPITAL
!   ERABDE = TOTAL ACCUMULATED BOOK DEPRECIATION
!   ERCNAD = ACCUMULATED AMORTIZATION FOR CANCELLED PLANT
!   ERPRDF = TOTAL DEFERRED TAXES FROM EXCESS TAX OVER BOOK
!   ESLNDG = TOTAL REGIONAL NET DEFERRED GAIN
!   ERRBRR = ACTUAL RATE BASE USED FOR SETTING REV REQUIREMENTS
!   ESRR = RATE OF RETURN (WEIGHTED AVERAGE COST OF CAPITAL)
!   ERTOMN = TOTAL O&M IN NOMINAL DOLLARS
!   ERTFLN = TOTAL FUEL COSTS IN NOMINAL DOLLARS
!   ERBDE = BOOK DEPRECIATION EXPENSE
!   NUMCAP = NUMBER OF PLANT TYPES IN EACH PLANT TYPE REPORT
!   ERAVCE = AVERAGE YEAR COMMON EQUITY BALANCE - CURRENT YEAR
!   ESLLP = TOTAL REGIONAL LEASE PAYMENT
!   ESLAGN = TOTAL REGIONAL AMORTIZATION OF GAIN FROM
!   ERRVRQ = REVENUE REQUIREMNTS FOR CURRENT YEAR
!   EGTXRT = GENERAL TAX RATE
!   ERPRTX = PROPERTY TAX ON VALUE OF ASSETS
!   ERSLTX = SALES TAX ON YEARLY CWIP EXPENDITURES
!   ERTDWO = TAX DEPRECIATION W/O ACCELERATION
!   ESSTXR = STATE TAX RATE
!   ERTIEX = TOTAL INTEREST EXPENSE
!   ERTDRG = DEPRECIATION EXPENSE USED IN FEDERAL TAX CALCULATIONS
!   ERCIDC = CAPITALIZED INTEREST DURING CONSTRUCTION
!   ESFTXR = FEDERAL STATUTORY TAX RATE
!   ERLIEX = LONG TERM INTEREST EXPENSE
!   EREDTF = AMOUNT OF EXCESS TO BE FLOWED THROUGH THIS YEAR
!   ERAITC = AMORTIZATION/YEAR OF DEFERRED ITC
!   ERFFDC = AMOUNT OF AFUDC TAX SAVINGS FLOWED THROUGH
!   ERFITC = AMOUNT OF ITC FLOWED THROUGH FOR RATEMAKING
!   EROFFS = AFUDC OFFSET
!   EPIND = NET-DEFERRED PHASE-IN REVENUES FOR YEAR
!   ERAFDC = AMORTIZATION OF AFUDC TAX SAVINGS ALL ASSETS
!   ERRVLG = ACTUAL REVENUES DEPENDING ON LAG SCENARIO
!   ERATSF = ACCELERATED DEPRECIATION TAX SAVINGS - FLOWED THROUGH
!   ERBDE = BOOK DEPRECIATION EXPENSE
!   ERFDCE = EQUITY PRTION OF AFUDC
!   ERNITC = TOTAL DEFERRED ITC NET OF AMORITIZED DEFERRALS
!   ERATSD = ACCELERATED DEPRECIATION TAX SAVINGS - DEFERRED
!   ERXFDC = DEFERRED AFUDC TAX SAVINGS
!   ERSIEX = SHORT TERM INTEREST EXPENSE
!   ERFDCD = DEBT PORTION OF TOTAL AFUDC
!   ERPSDV = TOTAL PREFERRED STOCK DIVIDENDS - CURRENT YEAR
!   EPIRET = CAPITALIZED RETURN ON DEFERRED REVENUES FOR YEAR
!   EDISYR = GROSS PLANT DISALLOWED IN THIS YEAR
!   ERBL = BALANCE SHEET ENTRIES
!   ERBCWP = BOOKED CWIP
!   EPIDEF = TOTAL CUMULATIVE DEFERRED PHASE-IN REVENUES
!   ESPRPS = PERCENT OF CAPITALIZATION FROM PREFERED STOCK
!   ESPRLT = PERCENT OF CAPITAL OBTAINED WITH LONG TERM DEBT
!   ESPRST = PERCENT OF CAPITAL OBTAINED WITH SHORT TERM DEBT
!   ERAMD = TOTAL VALUE OF ASSETS TO BE FINANCED
!   EDIVRT = DIVIDEND PAYOUT RATIO
!   ESPRCE = PERCENT OF CAPITALIZATION FROM COMMON EQUITY
!   ERDITC = ACCUMULATED DEFERRED ITC
!   ERFDC = YEARLY AFUDC
!   ESLPRC = NET OF TAX SALES PROCEEDS
!   EQTLSL = TOTAL ELECTRICITY SALES
!   EPGW = SUM OF CAPACITY ASSOCIATED WITH BUILDS
!   EPCGW = SUM OF CAPACITY ASSOCIATED WITH BUILDS
!   EPYCWP = DIRECT CONSTRUCTION EXPENDITURES FOR YEAR (NO AFDC)
!   EPAFDC = BOOKED AFUDC - CURRENT YEAR
!   EPONLN = BOOK VALUE OF CAPACITY COMPLETED IN CURRENT YEAR
!   ESRTCE = COST OF COMMON EQUITY
!   ESEMPS = EMBEDDED COST OF PREFERRED STOCK
!   ESRTPS = COST OF PREFERED STOCK
!   ESEMDT = EMBEDDED COST OF DEBT
!   ESRTLT = COST OF NEW LONG TERM DEBT
!   ESRTST = COST OF NEW SHORT TERM DEBT
! INTERNAL VARIABLES:
!   ECOMDV = COMMON STOCK DIVIDENDS
!   ERGNTX(N) = GENERAL TAXES
!   STFTAX = STATUTORY FEDERAL INCOME TAXES
!   ADJBDE = ADJUSTMENT FOR UNALLOWABLE BOOK DEPRECIATION
!   ADJADC = ADJUSTMENT FOR AFUDC
!   STTAX  = STATE INCOME TAXES, NET OF FEDERAL TAX BENEFIT
!   BKITAX = BOOKED INCOME TAXES
!   CITAX  = CURRENT INCOME TAXES (I.E. ACTUALLY PAID)
!   INDOC = OPERATING COMPONENT INDEX
!   N = OWNERSHIP TYPE INDEX
!   DEDINT = ?????
!   NUM = NUMBER OF CAPACITY TYPES
! OUTPUT VARIABLES:
!   REVREQ = REVENUE REQUIREMENTS REPORT
!   TAXINC = TAX INCOME REPORT
!   BALSHT = BALANCE SHEET REPORT
!   FUNDS = SOURCES/USES OF FUNDS REPORT
!   XINCST = INCOME STATEMENT REPORT
!   CAPREQ = CAPITAL REQUIREMENTS REPORT
!   SALES = SALES REPORT
!   CSTCAP = COST OF CAPITAL REPORT
!   CANPLT = CANCELLED PLANT REPORT
!*****************************************************************
      include 'parametr'
      include 'emmparm'
      include 'efprep'
      include 'efpcntrl'
      include 'efpgen'
      include 'eusprc'
      include 'efpint'
      include 'ldsm'
      include 'control'
      include 'efprp2'
      include 'efprc'
      include 'efprcy'
      include 'ncntrl'
      include 'bildin'
      include 'efpname'
      include 'uefdout'
      CHARACTER*1 ICALL  !OPERATING COMPONENT
      INTEGER NRGN       !REGION
      INTEGER INDOC      !OPERATING COMPONENT INDEX
      INTEGER N          !OWNERSHIP TYPE INDEX
      INTEGER I
      INTEGER K
      INTEGER L
      INTEGER NUM
      INTEGER J
      INTEGER FULLYR
      REAL*4 ADJBDE      !ADJUSTMENT FOR UNALLOWABLE BOOK DEPR.
      REAL*4 BKITAX      !BOOKED INCOME TAXES
      REAL*4 DEDINT
      REAL*4 STFTAX      !STATUTORY FEDERAL INCOME TAXES
      REAL*4 ADJADC      !ADJUSTMENT FOR AFUDC
      REAL*4 STTAX       !STATE INCOME TAXES NET OF FED. TAX BENEFIT
      REAL*4 CITAX       !CURRENT INCOME TAXES (I.E. ACTUALLY PAID)
      REAL*4 ERGNTX(2)   !CURRENT GENERAL TAXES (I.E. ACTUALLY PAID)
      REAL*4 ECOMDV(2)   !COMMON STOCK DIVIDENDS
      REAL COSTSERV(4,MNUMNR,MNUMYR)
      common /costofservice/ COSTSERV
! DETERMINE OPERATING COMPONENT INDEX
!   1 - GENERATION
!   2 - TRANSMISSION
!   3 - DISTRIBUTION
!     IF ((ICALL .NE. 'G') .AND. (ICALL .NE. 'T') .AND.
!    1   (ICALL .NE. 'D')) THEN
!        WRITE(6,*)'PROGRAMMING EROR IN STMTS'
!        STOP
!     ENDIF
      IF (ICALL .EQ. 'G') INDOC = 1
      IF (ICALL .EQ. 'T') INDOC = 2
      IF (ICALL .EQ. 'D') INDOC = 3

      FULLYR = USYEAR(CURIYR)
!   initialize both national and regional total
      IF( ICALL .eq. 'G') COMPEX(NRGN,CURIYR) = 0.0
      IF( ICALL .eq. 'G') COMPCAP(NRGN,CURIYR) = 0.0
      DO N=1,2
         IF (ESTSHR(N) .EQ. 0.0) THEN
! ZERO OUT MATRICES IF OWNERSHIP CATEGORY HAS NO SHARE
            IF ((IPOWN(N) .NE. 0) .OR. (IPOWN(3) .NE. 0)) THEN
               DO J=1,20
                  XINCST(J) = 0.0
               END DO
               DO J=1,26
                  BALSHT(J) = 0.0
               END DO
               DO J=1,21
                  FUNDS(J) = 0.0
               END DO
               DO J=1,24
                  REVREQ(J) = 0.0
               END DO
               DO J=1,2
                  SALES(J) = 0.0
               END DO
               DO J=1,4
                  PPCOST(J) = 0.0
               END DO
               DO J=1,NOCAP
                  DO K=1,8
                     CAPREQ(J,K) = 0.0
                  END DO
               END DO
               DO J=1,7
                  CSTCAP(J) = 0.0
               END DO
               DO J=1,3
                  CANPLT(J) = 0.0
               END DO
            ENDIF
         ELSE
! PREPARATION OF REVENUE REQUIREMENTS BUILD UP STATEMENT
            REVREQ(1) = ERTUP(N) - ERCNBV(N) - EDISNT(N)
            REVREQ(2) = ERRCWP(N)
            REVREQ(3) = ERNFSN(N)
            REVREQ(4) = ERWC(N)
            REVREQ(5) = ERABDE(N) - ERCNAD(N)
            REVREQ(6) = ERPRDF(N) + ESLNDG(N)
            REVREQ(7) = REVREQ(1) + REVREQ(2) + REVREQ(3) + &
                        REVREQ(4) - REVREQ(5) - REVREQ(6)
            REVREQ(8) = ERRBRR(N)
            REVREQ(9) = ESRR(N)
            REVREQ(10) = REVREQ(8)*REVREQ(9)
            REVREQ(11) = ERTOMN(N)
            REVREQ(12) = ERTFLN(N)
            REVREQ(13) = ERBDE(N) + ERNDFPMT(N)
            REVREQ(14) = ESLLP(N) - ESLAGN(N)
            REVREQ(15) = ERRVRQ(N) * EGTXRT(N) + ERPRTX(N) + &
                         ERSLTX(N)
            REVREQ(16) = (ERRVRQ(N) - ERTFLN(N) - ERTOMN(N) - &
                         ERTDWO(N) - ERTIEX(N) - ESLLP(N))*ESSTXR(N)
            REVREQ(17) = (ERRVRQ(N) - ERTOMN(N) - ERTFLN(N) - &
                         REVREQ(15) - REVREQ(16) - ERTDRG(N) - &
                         (ERTIEX(N) - ERCIDC(N)) - ESLLP(N)) &
                         *ESFTXR(N)
            REVREQ(18) = ERFITC(N) + EREDTF(N)
            REVREQ(19) = ERAITC(N)
            REVREQ(20) = ERFFDC(N)
            REVREQ(21) = ERAFDC(N)
            REVREQ(22) = EROFFS(N) + EPIND(N)
            REVREQ(23) = REVREQ(11) + REVREQ(12) + REVREQ(13) + &
                         REVREQ(14) + REVREQ(15) + REVREQ(16) + &
                         REVREQ(17) - REVREQ(18) - REVREQ(19) - &
                         REVREQ(20) - REVREQ(21) - REVREQ(22)
            REVREQ(24) = REVREQ(10) + REVREQ(23)

!
!pw2 need other expenses
!
            COSTSERV(INDOC,  NRGN,CURIYR) = COSTSERV(INDOC,  NRGN,CURIYR) + REVREQ(24)
            COSTSERV(INDOC,MNUMNR,CURIYR) = COSTSERV(INDOC,MNUMNR,CURIYR) + REVREQ(24)
            COSTSERV(    4,  NRGN,CURIYR) = COSTSERV(    4,  NRGN,CURIYR) + REVREQ(24)
            COSTSERV(    4,MNUMNR,CURIYR) = COSTSERV(    4,MNUMNR,CURIYR) + REVREQ(24)

            IF (ICALL .eq. 'G') THEN
               COMPEX(  NRGN,CURIYR) = COMPEX(  NRGN,CURIYR) +REVREQ(23) - REVREQ(13)
               COMPEX(MNUMNR,CURIYR) = COMPEX(MNUMNR,CURIYR) +REVREQ(23) - REVREQ(13)
            ENDIF


! PREPARE TAX ACCOUNTS
            ERGNTX(N) = ERRVLG(N) * EGTXRT(N) + ERPRTX(N) + ERSLTX(N)
            BKITAX = 0.0
            IF (N .NE. 2) THEN
               DEDINT = ERTIEX(N) - ERCIDC(N)
               STFTAX = (ERRVLG(N) - ERTFLN(N) - ERTOMN(N)-ERNDFPMT(N) - &
                        ESLLP(N) - ERBDE(N) - ERGNTX(N) - DEDINT + &
                        ERFDCE(N))*ESFTXR(N)
               ADJBDE = (ERBDE(N) - ERTDWO(N))*ESFTXR(N)
               ADJADC = ERFDCE(N)*ESFTXR(N)
               STTAX = (ERRVLG(N) - ERTFLN(N) - ESLLP(N) - &
                       ERTOMN(N) - ERTDWO(N) - ERTIEX(N))*ESSTXR(N)* &
                       (1 - ESFTXR(N))
               BKITAX = STFTAX + ADJBDE - ADJADC &
                        - ERAFDC(N) - ERFFDC(N) - EREDTF(N) &
                        - ERAITC(N) - ERFITC(N) - ERATSF(N) + STTAX
               CITAX = BKITAX - ERNITC(N) + EREDTF(N) - &
                       ERATSD(N) - ERXFDC(N) + ERAFDC(N)
! NOW BUILD UP INCOME TAXES STATEMENTS
               TAXINC(1) = CITAX
               TAXINC(2) = ERATSD(N) - EREDTF(N)
               TAXINC(3) = ERXFDC(N)
               TAXINC(4) = ERAFDC(N)
               TAXINC(5) = TAXINC(2) + TAXINC(3) - TAXINC(4)
               TAXINC(6) = ERNITC(N) + ERAITC(N)
               TAXINC(7) = ERAITC(N)
               TAXINC(8) = ERNITC(N)
               TAXINC(9) = BKITAX
               TAXINC(10) = STFTAX
               TAXINC(11) = ADJBDE
               TAXINC(12) = ADJADC
               TAXINC(13) = ERFITC(N)
               TAXINC(14) = ERAITC(N)
               TAXINC(15) = ERFFDC(N)
               TAXINC(16) = ERAFDC(N)
               TAXINC(17) = ERATSF(N) + EREDTF(N)
               TAXINC(18) = STTAX
               TAXINC(19) = BKITAX
            ENDIF
! PREPARATION OF INCOME STATEMENT
            XINCST(1) = ERRVLG(N)
            XINCST(2) = ERTOMN(N)
            XINCST(3) = ERTFLN(N)
            XINCST(4) = ERBDE(N) + ERNDFPMT(N)
            XINCST(5) = ESLLP(N) - ESLAGN(N)
            XINCST(6) = ERGNTX(N)
            XINCST(7) = BKITAX
            XINCST(8) = XINCST(2) + XINCST(3) + XINCST(4) + &
                        XINCST(5) + XINCST(6) + XINCST(7)
            XINCST(9) = XINCST(1) - XINCST(8)
            XINCST(10) = ERFDCE(N)
            XINCST(11) = XINCST(9) + XINCST(10)
            XINCST(12) = ERLIEX(N)
            XINCST(13) = ERSIEX(N)
            XINCST(14) = ERFDCD(N)
            XINCST(15) = XINCST(12) + XINCST(13) - XINCST(14)
            XINCST(16) = XINCST(11) - XINCST(15)
            XINCST(17) = ERPSDV(N)
            XINCST(18) = EPIND(N) + EPIRET(N)
            XINCST(19) = EDISYR(N)
            XINCST(20) = XINCST(16) - XINCST(17) + XINCST(18) - XINCST(19)

! MAKE ASSIGNMENTS TO VARIABLES NEEDED IN COST OF CAPITAL CALC.
      IF (N .EQ. 1) THEN
        ERBTXIT(NRGN,INDOC) = (XINCST(16) + (XINCST(6)+XINCST(7)+ &
               XINCST(15)))
        XINTCH(NRGN,INDOC) = XINCST(15)
      ENDIF

! PREPARATION OF BALANCE SHEET
! FIRST THE ASSET SIDE IS BUILT UP.
            BALSHT(1) = ERBL(1,N)
            BALSHT(2) = ERBL(2,N)
            BALSHT(3) = ERBL(3,N)
            BALSHT(4) = ERBL(4,N)
            BALSHT(5) = ERBCWP(N)
            BALSHT(6) = EDISNT(N)
            BALSHT(7) = BALSHT(1) + BALSHT(2) + BALSHT(3) + &
                        BALSHT(4) + BALSHT(5) - BALSHT(6)
            BALSHT(8) = ERABDE(N)
            BALSHT(9) = BALSHT(7) - BALSHT(8)
            BALSHT(10) = ERNFSN(N)
            BALSHT(11) = BALSHT(9) + BALSHT(10)
            BALSHT(12) = ERWC(N)
            BALSHT(13) = EPIDEF(N)
            BALSHT(14) = BALSHT(11) + BALSHT(12) + BALSHT(13)
! NEXT THE LIABILITIES ARE BUILT UP.
            BALSHT(16) = (ESPRPS(N)*ERAMD(N))
            BALSHT(19) = (ESPRLT(N)*ERAMD(N))
            ECOMDV(N) = EDIVRT(N)*XINCST(20)
            BALSHT(17) = XINCST(20) - ECOMDV(N)
            BALSHT(15) = ESPRCE(N)*ERAMD(N) - BALSHT(17)
            BALSHT(18) = BALSHT(15) + BALSHT(16) + BALSHT(17)
            BALSHT(20) = (ESPRST(N)*ERAMD(N))
            BALSHT(21) = BALSHT(19) + BALSHT(20)
            BALSHT(22) = ERDITC(N)
            BALSHT(23) = ERPRDF(N)
            BALSHT(24) = ESLNDG(N)
            BALSHT(25) = BALSHT(22) + BALSHT(23) + BALSHT(24)
            BALSHT(26) = BALSHT(18) + BALSHT(21) + BALSHT(25)

! MAKE ASSIGNMENTS TO VARIABLES FOR COST OF CAPITAL CALC.

       IF (N.EQ. 1) THEN
          TRNPL(NRGN,INDOC) = BALSHT(2)
          DSTPL(NRGN,INDOC) = BALSHT(3)
          TUTPL(NRGN,INDOC) = BALSHT(7)
          CWIPINT(NRGN,INDOC) = BALSHT(5)
          NTUTPL(NRGN,INDOC) = BALSHT(11)
       ENDIF


! STORE THE TOTAL CAPITALIZATION FOR SUMATION
        CAPTAL(N,INDOC)=BALSHT(15)+BALSHT(16)+BALSHT(19)+BALSHT(20)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!        WRITE(*,20)CAPTAL(N,INDOC),N,INDOC,NRGN,CURIYR,CURITR
!20      FORMAT(' STMTS - CAPTAL:',F10.2,' N,INDOC,NRGN,CURIYR,
!     1          CURITR:',I3,I3,I3,I3,I3)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! SEND THE TOTAL CAPITALIZATION FOR IOU'S TO CAPACITY PLANNING
      IF ((N .EQ. 2) .AND. (INDOC .EQ. 3)) THEN
        CALL GETBLD(1,NRGN)
        EPRTBS=0.0
         DO J=1,2
          DO K=1,3
        EPRTBS=EPRTBS+CAPTAL(J,K)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!       WRITE(*,30)EPRTBS,J,K
!30      FORMAT(' STMTS - EPRTBS,J,K:',F10.2,I3,I3)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
          END DO
         END DO
        CALL STRBLD(1,NRGN)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!        WRITE(*,10)EPRTBS,NRGN,CURIYR,CURITR
!10      FORMAT(' STMTS - EPRTBS:',F10.2,
!     1         ' NRGN,CURIYR,CURITR:',I3,I3,I3)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      ENDIF
! PREPARATION OF SOURCES AND USES OF FUNDS
            FUNDS(1) = XINCST(16)
            FUNDS(2) = XINCST(4)
            FUNDS(3) = ERNITC(N)
            FUNDS(4) = BALSHT(23)
            FUNDS(5) = ESLAGN(N)
            FUNDS(6) = ERFDC(N)
            FUNDS(7) = FUNDS(1) + FUNDS(2) + FUNDS(3) + &
                       FUNDS(4) - FUNDS(5) - FUNDS(6)
            FUNDS(8) = BALSHT(20)
            FUNDS(9) = BALSHT(19)
            FUNDS(10) = BALSHT(15)
            FUNDS(11) = BALSHT(16)
            FUNDS(12) = ESLPRC(N)
            FUNDS(13) = FUNDS(8) + FUNDS(9) + FUNDS(10) + &
                        FUNDS(11) + FUNDS(12)
            FUNDS(14) = FUNDS(7) + FUNDS(13)
            FUNDS(15) = ERYCWP(N)
            FUNDS(16) = ECOMDV(N)
            FUNDS(17) = ERPSDV(N)
            FUNDS(18) = BALSHT(12)
            FUNDS(19) = BALSHT(10)
            IF (FULLYR .EQ. UESTYR) FUNDS(19) = 0.0
            FUNDS(20) = FUNDS(15) + FUNDS(16) + FUNDS(17) + &
                        FUNDS(18) + FUNDS(19)
            FUNDS(21) = 0.0
            IF (FUNDS(15) .NE. 0.0) FUNDS(21) = (FUNDS(7) - &
                                    (FUNDS(16) + FUNDS(17)))/ &
                                    (FUNDS(15) + FUNDS(19))*100.0
! PREPARATION OF SALES DATA FOR PRICE CALCULATIONS
            SALES(1) = EQTLSL(N)
            SALES(2) = BLKSUM * ESTSHR(N)
             PPCOST(1) = PPWRBLK(1) * ESTSHR(N)
             PPCOST(2) = PPWRBLK(2) * ESTSHR(N)
             PPCOST(3) = PPWRBLK(3) * ESTSHR(N)
             PPCOST(4) = PPWRBLK(4) * ESTSHR(N)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!      WRITE(*,10)ESTSHR(N),N,NRGN
!10    FORMAT(' STMTS - ESTSHR(N):',F8.4,' N,NRGN:',I3,I3)
!          IF(NRGN.EQ.1) THEN
!     WRITE(*,10)ESTSHR(N),N,NRGN,BLKSUM,SALES(2),PPWRBLK(1),PPWRBLK(2)
!    +   ,PPCOST(1),PPCOST(2)
!10    FORMAT(' STMTS - ESTSHR(N):',F8.4,' N,NRGN:',I3,I3,6(F8.3))
!          ENDIF
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! PREPARATION OF CAPITAL REQUIREMENTS REPORT DATA
            DO K=1,NOCAP
               DO L=1,8
                  CAPREQ(K,L) = 0.0
               END DO
               NUM = NUMCAP(K)
               DO J=1,NUM
                  CAPREQ(K,1) = CAPREQ(K,1) + EPGW(CAPTYP(K,J),N)
                  CAPREQ(K,2) = CAPREQ(K,2) + EPCGW(CAPTYP(K,J),N)
                  CAPREQ(K,3) = CAPREQ(K,3) + EPYCWP(CAPTYP(K,J),N,1)
                  CAPREQ(K,4) = CAPREQ(K,4) + EPAFDC(CAPTYP(K,J),N)
                  CAPREQ(K,5) = CAPREQ(K,5) + EPONLN(CAPTYP(K,J),N)
                  CAPREQ(K,6) = CAPREQ(K,6) + EPYCWP(CAPTYP(K,J),N,2)
                  CAPREQ(K,7) = CAPREQ(K,7) + EPISOC(CAPTYP(K,J),N)
                  CAPREQ(K,8) = CAPREQ(K,8) + EPADOC(CAPTYP(K,J),N)
!     IF (NRGN .EQ. 1) WRITE(6,*) ICALL,K,J,EPISOC(CAPTYP(K,J),N),
!    * EPADOC(CAPTYP(K,J),N),'HELLO'
               END DO
!     IF (NRGN .EQ. 1) WRITE(6,*) ICALL,K,CAPREQ(K,7),CAPREQ(K,8)
!    *  ,'HELLO1'
          IF ((ICALL .eq.'G').and. (CAPTIT(K).eq. 'TOTAL PROD.')) THEN
             COMPCAP(NRGN,CURIYR) = CAPREQ(K,3)+CAPREQ(K,6)+CAPREQ(K,4)
             IF(NRGN .LE. UNRGNS) &
             COMPCAP(MNUMNR,CURIYR) = COMPCAP(MNUMNR,CURIYR) + &
               CAPREQ(K,3)+CAPREQ(K,6)+CAPREQ(K,4)
          ENDIF
            END DO

! MAKE ASSIGNMENTS TO VARIABLES FOR USE IN COST OF CAPITAL
      IF ((INDOC .EQ. 1) .AND. (N .EQ. 1)) THEN
         NCAPEX(NRGN) = NCAPEX(NRGN) + CAPREQ(NC_IND,6)
         SCAPEX(NRGN) = SCAPEX(NRGN) + CAPREQ(CL_IND,6) &
                        + CAPREQ(OG_IND,6)
         N_PIS(NRGN) = CAPREQ(NC_IND,7)
         S_PIS(NRGN) = CAPREQ(CL_IND,7) + CAPREQ(OG_IND,7)
      ENDIF

! PREPARATION OF COST OF CAPITAL DATA
            DO J=1,7
               CSTCAP(J) = 0.0
            END DO
            CSTCAP(1) = ESRTCE(N)
            CSTCAP(2) = ESEMPS(N)
            CSTCAP(3) = ESRTPS(N)
            CSTCAP(4) = ESEMDT(N)
            CSTCAP(5) = ESRTLT(N)
            CSTCAP(6) = ESRTST(N)
            CSTCAP(7) = ESPRCE(N)
            CSTCAP(8) = ESPRPS(N)
            CSTCAP(9) = ESPRLT(N)
            CSTCAP(10) = ESPRST(N)
            CSTCAP(11) = ESRR(N)
! PREPARATION OF CANCELLED NUCS REPORT
            CANPLT(1) = ERCNBV(N)
            CANPLT(2) = ERCNAD(N)
            CANPLT(3) = ERAVCE(N)
         ENDIF
         CALL STRREP(N,NRGN,CURIYR,INDOC)
      END DO
! PREPARE DEMAND FACTOR REPORT
      IF (ICALL .EQ. 'G') THEN
         DO I=1,4
            DEMREP(1,I,1) = SALCLS(NRGN,I)
            DO J=1,9
               DEMREP(J,I,2) = DEMFAC(J,I)
            END DO
            DEMREP(2,I,1) = FUELMR(I)
            DEMREP(3,I,1) = FUELEM(I)
            DEMREP(4,I,1) = CUST(I)
            DEMREP(5,I,1) = CP(I)
            DEMREP(6,I,1) = PCP(I)
            DEMREP(7,I,1) = NCP(I)
            DEMREP(8,I,1) = SYSLF*100.0
            DEMREP(9,I,1) = 0.0
         END DO

! MAKE ASSIGNMENTS TO VARIABLES FOR COST OF CAPITAL CALCULATION
         RESPCT(NRGN) = DEMREP(1,1,2)
         COMPCT(NRGN) = DEMREP(1,2,2)
         INDPCT(NRGN) = DEMREP(1,3,2)

      ENDIF
      IF (ICALL .EQ. 'D') CALL STRRP2(NRGN,CURIYR)
      RETURN
      END
      SUBROUTINE ELRATE(ICALL,NRGN)
      IMPLICIT NONE
!******************************************************************
! THIS SUBROUTINE CONTROLS THE CALCULATION OF RATES BY SECTOR.
!   IT UTILIZES 3 ROUTINES:
!       FUNCTN:  FUNCTIONALIZES TOTAL COSTS INTO CATEGORIES
!       ALLOCT:  ALLOCATES CATEGORY COSTS TO CLASSES
!       RATES:   CALCULATES AVERAGE PRICE FOR EACH CLASS
!*******************************************************************
! INPUT VARIABLES:
!   ICALL = OPERATING COMPONENT INDICATION
! INTERNAL VARIABLES:
!   ISECT = OPERATING COMPONENT INDEX
!*******************************************************************
      include 'parametr'
      include 'emmparm'
      include 'efpcntrl'
      include 'control'
      include 'ncntrl'
      INTEGER ISECT
      INTEGER NRGN
      CHARACTER*1 ICALL
! DETERMINE OPERATING COMPONENT INDEX
!   1 - GENERATION
!   2 - TRANSMISSION
!   3 - DISTRIBUTION
!     IF ((ICALL .NE. 'G') .AND. (ICALL .NE. 'T') .AND.
!    1   (ICALL .NE. 'D')) THEN
!        WRITE(6,*)'PROGRAMMING EROR IN ELRATE'
!        STOP
!     ENDIF
      IF (ICALL .EQ. 'G') ISECT = 1
      IF (ICALL .EQ. 'T') ISECT = 2
      IF (ICALL .EQ. 'D') ISECT = 3
! FUNCTIONALIZE COSTS INTO CATEGORIES
      CALL FUNCTN(ISECT,NRGN)
! ALLOCATE COSTS
      CALL ALLOCT(ISECT,NRGN)
! CALUCULATE AVERAGE RATES BY SECTOR
      CALL RATES(ISECT,NRGN)


      RETURN
      END
      SUBROUTINE FUNCTN(ISECT,NRGN)
      IMPLICIT NONE
!*******************************************************************
! THIS SUBROUTINE FUNCTIONALIZES THE REVENUE REQUIREMENT INTO PRE-DEFINED COST CATEGORIES
!    THE CATEGORIES ARE:
!         COST(1) = FUEL COSTS
!         COST(2) = VARIABLE O&M
!         COST(3) = FIXED O&M
!         COST(4) = CAPITAL COSTS
!*******************************************************************
! INPUT VARIABLES:
!   ERTFLN = TOTAL FUEL COSTS IN NOMINAL DOLLARS
!   ERTOMN = TOTAL O&M COSTS IN NOMINAL DOLLARS
!   ERRVLG = ACTUAL REVENUES DEPENDING ON LAG SCENARIO
! OUTPUT VARIABLES:
!   COST = COMPONENTS OF COST
!*******************************************************************
      include 'parametr'
      include 'emmparm'
      include 'control'
      include 'efpgen'
      include 'eusprc'
      include 'efpint'
      include 'efprp2'
      INTEGER ISECT
      INTEGER NRGN,I

      COST(:,ISECT) = 0.0
      ECOST(:,NRGN) = 0.0

      COST(1,ISECT) = ERTFLN(1) + ERTFLN(2)
      COST(2,ISECT) = 0.5*(ERTOMN(1) + ERTOMN(2))
      COST(3,ISECT) = 0.5*(ERTOMN(1) + ERTOMN(2))
      COST(4,ISECT) = ERRVLG(1) + ERRVLG(2) - &
                      COST(1,ISECT) - COST(2,ISECT) - COST(3,ISECT)
!      IF (NRGN .EQ. 20)Write(6,*)'cost ',ISECT,NRGN, (COST(I,ISECT),I=1,4)

      IF (ISECT .EQ. 1) THEN
         ECOST(1,NRGN) = (COST(1,ISECT)-BLKSUM+PPWRBLK(1)) &
                         /(EQTLSL(1) + EQTLSL(2))
         ECOST(2,NRGN) = (COST(2,ISECT) + COST(3,ISECT) + PPWRBLK(2))/ &
                         (EQTLSL(1) + EQTLSL(2))
         ECOST(3,NRGN) = PPWRBLK(4)/(EQTLSL(1) + EQTLSL(2))
!      IF (NRGN .EQ. 20) WRITE(6,*)'HELLO1',ISECT,NRGN,ECOST(1,NRGN),ECOST(2,NRGN),ECOST(3,NRGN)
        ECOST(1,NRGN) = ECOST(1,NRGN) - ECOST(3,NRGN)
      ELSE
         ECOST(1,NRGN) = (COST(1,ISECT)/(EQTLSL(1) + EQTLSL(2))) + &
                         ECOST(1,NRGN)
         ECOST(2,NRGN) = ((COST(2,ISECT) + COST(3,ISECT))/ &
                         (EQTLSL(1) + EQTLSL(2))) + ECOST(2,NRGN)
      ENDIF
      RETURN
      END

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!      WRITE(*,10)EFPFL,ERTFL(1),ERTFL(2),NRGN,ICALL
!10    FORMAT(' GL - EFPFL,ERTFL(1),ERTFL(2),NRGN,ICALL:',
!     1        F10.2,F10.2,F10.2,I3,I3)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE ALLOCT(ISECT,NRGN)
      IMPLICIT NONE
!*******************************************************************
! THIS SUBROUTINE CALCULATES REVENUES ALLOCATED TO EACH CLASS
!*******************************************************************
! INPUT VARIABLES:
!   ISECT = OPERATING COMPONENT INDEX
!   NCLASS = NUMBER OF CUSTOMER CLASSES
!   NCOST = NUMBER OF COST CATEGORIES
!   NTECH = NUMBER OF TECHNIQUES
! OUTPUT VARIABLES:
!   REV = REVENUES BY RATE CLASS AND FUNCTION
!   COSTFC = FACTORS TO ALLOCATE COST POOLS TO RATE CLASSES
!*******************************************************************
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'efpgen'
      include 'eusprc'
      include 'efpint'
      include 'efprp2'
      include 'dsmdimen'
      include 'dsmunits'
      INTEGER ISECT
      INTEGER ICLS
      INTEGER ICOST
      INTEGER ITECH
      INTEGER NRGN

      DO ICLS=1,NCLASS
         REV(ICLS,ISECT) = 0.0
         DO ICOST=1,NCOST
            COSTFC(ICOST,ICLS,ISECT) = 0.0
            DO ITECH=1,NTECH
               COSTFC(ICOST,ICLS,ISECT) = (DEMFAC(ITECH,ICLS) &
                                    *(TECFAC(ITECH,ICOST,ISECT)/100.0)) &
                                    + COSTFC(ICOST,ICLS,ISECT)
            END DO
            REV(ICLS,ISECT) = REV(ICLS,ISECT) + COSTFC(ICOST,ICLS,ISECT) * COST(ICOST,ISECT)

            WRITE(IMSG,2111) CURIRUN, CURIYR+1989, CURITR, NRGN, ISECT, ICLS, ICOST, COSTFC(ICOST,ICLS,ISECT), COST(ICOST,ISECT), REV(ICLS,ISECT)
 2111       FORMAT(1X,"REV_DBG",7(":",I4),3(":",F15.6))

         END DO
      END DO
      RETURN
      END
      SUBROUTINE RATES(ISECT,NRGN)
      IMPLICIT NONE
!*******************************************************************
! THIS SUBROUTINE CALCULATES AVERAGE PRICE BY SECTOR
!    AVERAGE PRICE IS CALCULATED AS REVENUES ALLOCATED TO THE CLASS DIVIDED BY SALES TO THE CLASS
!*******************************************************************
! INPUT VARIABLES:
!   NCLASS = NUMBER OF CUSTOMER CLASSES
!   ELFACT = CONVERSION FACTORS FOR VARIOUS UNITS OF MEASURE
!   REV = REVENUES ALLOCATED TO CLASS
! OUTPUT VARIABLES:
!   SALCLS = SALES BY CLASS (RES., COM., IND., TRANS.)
!   EPRICE = PRICES PASSED TO NEMS REPORT WRITER
!*******************************************************************
      include 'parametr'
      include 'emmparm'
      include 'efpgen'
      include 'eusprc'
      include 'efpint'
      include 'ldsm'
      include 'control'
      include 'efprp2'
      include 'ncntrl'
      include 'macout'
      include 'elout'
      include 'efpout'
      include 'edbdef'
      include 'udatout'
      include 'dispin'
      include 'dispinyr'
      include 'dsmdimen'
      include 'dsmunits'
      include 'dsmtoefd'
      include 'efpcntrl'
      include 'uefdout'
      INTEGER NUMTABS
      PARAMETER (NUMTABS = 1)        ! total number of database tables
      INTEGER ICLS
      INTEGER ISECT
      INTEGER JSECT
      INTEGER NRGN
      INTEGER ISEA
      INTEGER ISLC
      INTEGER ISCT
      REAL REVH1(3,MNUMNR-3)
      REAL REVH2(3,MNUMNR-3)
      REAL REVHRCI
      REAL REVRCI
      REAL REVHPCT(3)
      REAL REVPCT(3)
      REAL SALH1(3,MNUMNR-3)
      REAL SALH2(3,MNUMNR-3)
      REAL BPRICE(5,4,MNUMNR-3)
      REAL FPRICE(5,4,MNUMNR-3)
      REAL SLSHRCI
      REAL SLSRCI
      REAL SLSHPCT(3)
      REAL SLSPCT(3)
      REAL DIFFHPCT(3)
      REAL DIFFPCT(3)
      INTEGER HISYR1,HISYR2
      REAL SALHRCI,INDRAT,NYINDRAT

!  Need only CA transp price
      REAL TRANPRICE
      DATA TRANPRICE/70.05/
      DATA NYINDRAT/.008/
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
!
      HISYR1 = UYR_HIST - UHBSYR - 1
      HISYR2 = UYR_HIST - UHBSYR

      DO ICLS=1,NCLASS
         IF (SALCLS(NRGN,ICLS) .NE. 0.0) THEN
            EPRICE(ICLS,ISECT,NRGN) = REV(ICLS,ISECT) / SALCLS(NRGN,ICLS)

            WRITE(IMSG,2111) CURIRUN, CURIYR+1989, CURITR, NRGN, ISECT, ICLS, EPRICE(ICLS,ISECT,NRGN), REV(ICLS,ISECT), SALCLS(NRGN,ICLS)
 2111       FORMAT(1X,"EPRICE_DBG",6(":",I4),3(":",F15.6))

            IF (REV(ICLS,ISECT) .NE. REV(ICLS,ISECT)) THEN
               write(6,*)'efp REV is NaNq', ICLS,NRGN
            ENDIF
         ELSE
!           SALCLS(NRGN,ICLS) = 50.0
!           REV(ICLS,ISECT) = 100.0
            EPRICE(ICLS,ISECT,NRGN) = 0.1
            write(6,*)'EPRICE .eq. 0 due to 0 sales',NRGN,ICLS,ISECT
         ENDIF
         BPRICE(ICLS,ISECT,NRGN) = EPRICE(ICLS,ISECT,NRGN)
      END DO
      EPRICE(NCLASS+1,ISECT,NRGN) = (REV(1,ISECT) + REV(2,ISECT) + &
                                    REV(3,ISECT) + REV(4,ISECT))/ &
                                    (SALCLS(NRGN,1) + &
                                    SALCLS(NRGN,2) + &
                                    SALCLS(NRGN,3) + &
                                    SALCLS(NRGN,4))
         BPRICE(NCLASS+1,ISECT,NRGN) = EPRICE(NCLASS+1,ISECT,NRGN)

!  fill in generation electricity price for endogenous retirements
      IF (ISECT .EQ. 1) THEN
      DO ISEA = 1, EFDns
        DO ISLC = 1, ELNVCT(ISEA)
           ELGENP(ISLC,ISEA,NRGN) = EPRICE(5,ISECT,NRGN)/ &
             MC_JPGDP(CURIYR)
        ENDDO
      ENDDO
      ENDIF


      IF (ISECT .EQ. 3) THEN
         DO ICLS=1,NCLASS+1
            EPRICE(ICLS,4,NRGN) = 0.0
            IF (ICLS .LE. 4) REV(ICLS,4)=0.0
            DO JSECT=1,3
               EPRICE(ICLS,4,NRGN) = EPRICE(ICLS,JSECT,NRGN) + &
                                     EPRICE(ICLS,4,NRGN)
               IF (ICLS .LE. 4) REV(ICLS,4)=REV(ICLS,4)+REV(ICLS,JSECT)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                IF (JSECT .EQ. 3) THEN
!                 WRITE(*,20)EPRICE(ICLS,4,NRGN),ICLS,NRGN,CURIYR
!20               FORMAT(/' RATES - EPRICE(1ST):',F10.4,
!     2                  ' ICLS,NRGN,CURIYR:',I3,I3,I3)
!                ENDIF
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

            END DO
            BPRICE(ICLS,4,NRGN) = EPRICE(ICLS,4,NRGN)
         END DO
     ENDIF
! DETERMINE HISTORIC INTER-CUSTOMER CLASS SUBSIDIES FOR RESIDENTIAL,
! COMMERCIAL, AND INDUSTRIAL CUSTOMERS

      IF (CURIYR .LE. HISYR2) THEN

! READ IN HISTORIC REGIONAL REVENUES AND SALES
! FOR RES, COM, AND IND CUSTOMERS
! H1 is year before most recent year of data, H2 is most recent year of data
!
       REVH1(1,NRGN) = HREVRSNR(NRGN,HISYR1)
       REVH1(2,NRGN) = HREVCMNR(NRGN,HISYR1)
       REVH1(3,NRGN) = HREVINNR(NRGN,HISYR1)
       SALH1(1,NRGN) = HELRSNR(NRGN,HISYR1) * 1000.0
       SALH1(2,NRGN) = HELCMNR(NRGN,HISYR1) * 1000.0
       SALH1(3,NRGN) = HELINNR(NRGN,HISYR1) * 1000.0
       REVH2(1,NRGN) = HREVRSNR(NRGN,HISYR2)
       REVH2(2,NRGN) = HREVCMNR(NRGN,HISYR2)
       REVH2(3,NRGN) = HREVINNR(NRGN,HISYR2)
       SALH2(1,NRGN) = HELRSNR(NRGN,HISYR2) * 1000.0
       SALH2(2,NRGN) = HELCMNR(NRGN,HISYR2) * 1000.0
       SALH2(3,NRGN) = HELINNR(NRGN,HISYR2) * 1000.0


 7293 FORMAT(1x,a,2I5,3(F12.3))
!
!     DETERMINE TOTAL REVENUES AND SALES TO RES, COM, AND IND
!     BASED ON HISTORIC DATA AND MODEL OUTPUT

      REVHRCI=0.0
      REVRCI=0.0
      SLSHRCI=0.0
      SLSRCI=0.0
      DO ICLS=1,3
         IF (CURIYR .LE. HISYR1) THEN
            REVHRCI=REVHRCI+REVH1(ICLS,NRGN)
            SLSHRCI=SLSHRCI+SALH1(ICLS,NRGN)
         ELSE
            REVHRCI=REVHRCI+REVH2(ICLS,NRGN)
            SLSHRCI=SLSHRCI+SALH2(ICLS,NRGN)
         ENDIF
         REVRCI=REVRCI+REV(ICLS,4)
         SLSRCI=SLSRCI+SALCLS(NRGN,ICLS)
      END DO

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!      WRITE(*,30)REVHRCI,REVRCI,SLSHRCI,SLSRCI,CURIYR,NRGN
!30     FORMAT(' RATES - REVHRCI:',F10.2,' REVRCI:',F10.2, &
!             ' SLSHRCI:',F10.2,' SLSRCI:',F10.2, &
!             ' CURIYR,NRGN:',I3,I3)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!     CALCULATE PROPORTION OF REVENUES AND SALES TO RES,COM,AND IND
!     BASED ON HISTORIC DATA AND MODEL OUTPUT
      DO ICLS=1,3
         REVPCT(ICLS)=REV(ICLS,4)/REVRCI
         IF (CURIYR .LE. HISYR1) THEN
            IF (REVHRCI .GT. 0.0) THEN
               REVHPCT(ICLS)=REVH1(ICLS,NRGN)/REVHRCI
            ELSE
               REVHPCT(ICLS)=0.0
            END IF
            IF (SLSHRCI .GT. 0.0) THEN
               SLSHPCT(ICLS)=SALH1(ICLS,NRGN)/SLSHRCI
            ELSE
               SLSHPCT(ICLS)=0.0
            END IF
         ELSE
            IF (REVHRCI .GT. 0.0) THEN
               REVHPCT(ICLS)=REVH2(ICLS,NRGN)/REVHRCI
            ELSE
               REVHPCT(ICLS)=0.0
            END IF
            IF (SLSHRCI .GT. 0.0) THEN
               SLSHPCT(ICLS)=SALH2(ICLS,NRGN)/SLSHRCI
            ELSE
               SLSHPCT(ICLS)=0.0
            END IF
         END IF
         SLSPCT(ICLS)=SALCLS(NRGN,ICLS)/SLSRCI

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!      WRITE(*,40)REVPCT(ICLS),REVHPCT(ICLS),SLSPCT(ICLS), &
!                 SLSHPCT(ICLS),CURIYR,NRGN,ICLS
!40     FORMAT(' RATES - REVPCT:',F8.4,' REVHPCT:',F8.4, &
!             ' SLSPCT:',F8.4,' SLSHPCT:',F8.4, &
!             ' CURIYR,NRGN,ICLS:',I3,I3,I3)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
          END DO

! CALCULATE THE DIFFERENCE BETWEEN THE PROPORTION OF REVENUES AND
! THE PROPORTIONS OF SALES ALLOCATED TO EACH CLASS BASED ON
! HISTORIC DATA AND MODEL OUTPUT
          DO ICLS=1,3
           DIFFPCT(ICLS)=REVPCT(ICLS)-SLSPCT(ICLS)
           DIFFHPCT(ICLS)=REVHPCT(ICLS)-SLSHPCT(ICLS)


! DETERMINE IMPLIED SUBSIDY OF HISTORIC DATA -
! THE SUBSIDY IS THE PERCENT OF RESIDENTIAL,COMMERCIAL, AND
! INDUSTRIAL REVENUES TO BE REALLOCATED - A NEGATIVE SUBSIDY
! MEANS THAT THAT PARTICULAR CUSTOMER IS SUBSIDIZING OTHERS

      SUBPCT(ICLS,NRGN,CURIYR)=DIFFHPCT(ICLS)-DIFFPCT(ICLS)

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!      WRITE(*,50)DIFFPCT(ICLS),DIFFHPCT(ICLS),CURIYR,NRGN,ICLS
!50    FORMAT(' RATES - DIFFPCT:',F6.4,' DIFFHPCT:',F6.4, &
!             ' CURIYR,NRGN,ICLS:',I3,I3,I3)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

          END DO
!  hardcode change to subsidies for New York, shift more away from industrial
    IF (UNRGNS.EQ.13) THEN
      IF ((CURIYR .EQ. HISYR2) .and. (NRGN .eq. 6)) THEN
       SUBPCT(1,NRGN,CURIYR) = SUBPCT(1,NRGN,CURIYR) + .5*NYINDRAT
       SUBPCT(2,NRGN,CURIYR) = SUBPCT(2,NRGN,CURIYR) + .5*NYINDRAT
       SUBPCT(3,NRGN,CURIYR) = SUBPCT(3,NRGN,CURIYR) - NYINDRAT
      ENDIF
    ELSEIF (UNRGNS.EQ.22) THEN

!DKG      IF ((CURIYR .EQ. HISYR2) .and. (NRGN .eq. 6)) THEN
!DKG       SUBPCT(1,NRGN,CURIYR) = SUBPCT(1,NRGN,CURIYR) + .5*NYINDRAT
!DKG       SUBPCT(2,NRGN,CURIYR) = SUBPCT(2,NRGN,CURIYR) + .5*NYINDRAT
!DKG       SUBPCT(3,NRGN,CURIYR) = SUBPCT(3,NRGN,CURIYR) - NYINDRAT
!DKG      ENDIF
     ENDIF

!    Since nationally the industrial share declines over
!    the forecast, this tends to exacerbate the subsidy
!    We reduced the subsidy by the fraction reduction
!    of the industrial share to total share
!    First subhist years this use data only
         INDRAT = 1.0

          SALHRCI = 0.0
          DO ICLS=1,3
!           SALHRCI =SALHRCI+SALH2(ICLS,NRGN)
            SALHRCI =SALHRCI+SALCLS(NRGN,ICLS)
          END DO
!        HISINDRAT(NRGN) =(SALH2(3,NRGN)/SALHRCI)
         IF (SLSHRCI .GT. 0.0) THEN
            HISINDRAT(NRGN) =(SALCLS(NRGN,3)/SALHRCI)
         ELSE
            HISINDRAT(NRGN) = 0.0
         END IF

! SET SUBPCT AND REVRCI FOR YEARS WHEN SUBSIDY IS NOT
! CALCULATED FROM HISTORIC DATA

      ELSE


!    Since nationally the industrial share declines over
!    the forecast, this tends to exacerbate the subsidy
!    We reduced the subsidy by the fraction reduction
!    of the industrial share to total share
          SLSRCI = 0.0
          SALHRCI = 0.0
          DO ICLS=1,3
            SLSRCI=SLSRCI+SALCLS(NRGN,ICLS)
          END DO
          IF (SLSHRCI .GT. 0.0 .AND. HISINDRAT(NRGN) .GT. 0.0) THEN
             INDRAT =(SALCLS(NRGN,3)/SLSRCI)/HISINDRAT(NRGN)
          ELSE
             INDRAT = 1.0
          END IF

!   write(6,*)' NERC INDRAT HISINDRAT CURIYR',NRGN,INDRAT,HISINDRAT(NRGN),CURIYR

           REVRCI=0.0
          DO ICLS=1,3
           SUBPCT(ICLS,NRGN,CURIYR)=SUBPCT(ICLS,NRGN,HISYR2) * INDRAT
!          SUBPCT(ICLS,NRGN,CURIYR)=SUBPCT(ICLS,NRGN,HISYR2)
           REVRCI=REVRCI+REV(ICLS,4)
          END DO
      ENDIF

! RECALCULATE THE CUSTOMER CLASS PRICES WITH SUBSIDIES
         DO ICLS=1,3
            IF (SALCLS(NRGN,ICLS) .GT. 0.0) THEN
               IF (UNRGNS.EQ.13) THEN
                  EPRICE(ICLS,4,NRGN)=(REV(ICLS,4)+ (REVRCI *SUBPCT(ICLS,NRGN,CURIYR)))/ SALCLS(NRGN,ICLS)
                  FPRICE(ICLS,4,NRGN)=(REV(ICLS,4)+ (REVRCI *SUBPCT(ICLS,NRGN,CURIYR)))/ SALCLS(NRGN,ICLS)
                  DO ISCT = 1 , 3
                     FPRICE(ICLS,ISCT,NRGN)= BPRICE(ICLS,ISCT,NRGN) * (EPRICE(ICLS,4,NRGN) /   BPRICE(ICLS,4,NRGN) )
                  ENDDO
                ELSEIF (UNRGNS.EQ.22) THEN
                   EPRICE(ICLS,4,NRGN)=(REV(ICLS,4)+ (REVRCI *SUBPCT(ICLS,NRGN,CURIYR)))/ SALCLS(NRGN,ICLS)
                   FPRICE(ICLS,4,NRGN)=(REV(ICLS,4)+ (REVRCI *SUBPCT(ICLS,NRGN,CURIYR)))/ SALCLS(NRGN,ICLS)
                   DO ISCT = 1 , 3
                      FPRICE(ICLS,ISCT,NRGN)= BPRICE(ICLS,ISCT,NRGN) * (EPRICE(ICLS,4,NRGN) / BPRICE(ICLS,4,NRGN) )
                   ENDDO
                ELSEIF (UNRGNS.EQ.25) THEN
                  EPRICE(ICLS,4,NRGN)=(REV(ICLS,4)+ (REVRCI *SUBPCT(ICLS,NRGN,CURIYR)))/ SALCLS(NRGN,ICLS)
                  FPRICE(ICLS,4,NRGN)=(REV(ICLS,4)+ (REVRCI *SUBPCT(ICLS,NRGN,CURIYR)))/ SALCLS(NRGN,ICLS)
                  DO ISCT = 1 , 3
                     FPRICE(ICLS,ISCT,NRGN)= BPRICE(ICLS,ISCT,NRGN) * (EPRICE(ICLS,4,NRGN) / BPRICE(ICLS,4,NRGN) )
                  ENDDO
               ENDIF
            ENDIF

!           WRITE(6,10)EPRICE(ICLS,4,NRGN),SUBPCT(ICLS,NRGN,CURIYR), REV(ICLS,4),REVRCI,ICLS,NRGN, CURIYR,CURITR
10          FORMAT(' RATES - EPRICE(NEW):',F10.4,' SUBPCT:',F10.4, ' REV(ICLS,4):',F8.2,' REVRCI:',F8.2, ' ICLS,NRGN,CURIYR:',I3,I3,I3,I3)

         END DO
!  fill in fprice for classes 4 and 5 , set equal to eprice.
            DO ISCT = 1 , 4
              FPRICE(4,ISCT,NRGN) = EPRICE(4,ISCT,NRGN)
              FPRICE(5,ISCT,NRGN) = EPRICE(5,ISCT,NRGN)
            ENDDO
!
!   pw2 we store the fraction of the class price relative to industrial
!    and use these for the competitive case
!
          DO ICLS=1,4
            SUBPRFRC(ICLS,NRGN) = EPRICE(ICLS,4,NRGN)/EPRICE(3,4,NRGN)
!           write(6,*)'subprfrc ICLS NRGN',SUBPRFRC(ICLS,NRGN),ICLS,NRGN
          ENDDO

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!         CALIFORNIA LONG TERM CONTRACT ADDERS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


!Region 13  *** removed this code *** need another method to adjust with dynamic region tool
!
         ! All CA customers must pay a Competition Transition Charge and a
         ! Fixed Transition Amount.  Customers of the 3 largest IOUs must also pay a
         ! an Energy Cost Recovery Amount and pay off a DWR Bond.  These are reflected
         ! on customer distribution portion of the bill below and can differ among
       ! customer classes.

!
!     Write EFP EPRICE Output Table by Demand Sector and Stage of Productions (EFP_EPRICE)
!
       IF ( (ORCLEFP .EQ. 1) .AND. (FNRUN .EQ. 1) .AND. (FCRL .EQ. 1) ) THEN
            TNUM = 1
            IF (LOOPING(TNUM) .EQ. 0) THEN
              NUMCOLS(TNUM) = 8
              DYNSTM(TNUM) = 'INSERT INTO EFP_EPRICE VALUES(?,?,?,?,?,?,?,?,?)'
              WRTSTM(TNUM) = 'EFP_EPRICE'
            ENDIF

              DO ICLS = 1 , 5
                LOOPING(TNUM) = LOOPING(TNUM) + 1
                COLV(TNUM,1,LOOPING(TNUM)) = CURIYR
                COLV(TNUM,2,LOOPING(TNUM)) = NRGN
                COLV(TNUM,3,LOOPING(TNUM)) = CURITR
                COLV(TNUM,4,LOOPING(TNUM)) = ICLS
                COLV(TNUM,5,LOOPING(TNUM)) = ISECT
                COLV(TNUM,6,LOOPING(TNUM)) = EPRICE(ICLS,ISECT,NRGN)/MC_JPGDP(CURIYR)
                COLV(TNUM,7,LOOPING(TNUM)) = BPRICE(ICLS,ISECT,NRGN)/MC_JPGDP(CURIYR)
                COLV(TNUM,8,LOOPING(TNUM)) = FPRICE(ICLS,ISECT,NRGN)/MC_JPGDP(CURIYR)
                IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
                  COLVALS(:,:) = COLV(TNUM,:,:)
!                 CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                  CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                  LOOPING(TNUM) = 0
                ENDIF
              ENDDO
              IF (ISECT .EQ. 3) THEN
                DO ICLS = 1 , 5
                  LOOPING(TNUM) = LOOPING(TNUM) + 1
                  COLV(TNUM,1,LOOPING(TNUM)) = CURIYR
                  COLV(TNUM,2,LOOPING(TNUM)) = NRGN
                  COLV(TNUM,3,LOOPING(TNUM)) = CURITR
                  COLV(TNUM,4,LOOPING(TNUM)) = ICLS
                  COLV(TNUM,5,LOOPING(TNUM)) = 4
                  COLV(TNUM,6,LOOPING(TNUM)) = EPRICE(ICLS,4,NRGN)/MC_JPGDP(CURIYR)
                  COLV(TNUM,7,LOOPING(TNUM)) = BPRICE(ICLS,4,NRGN)/MC_JPGDP(CURIYR)
                  COLV(TNUM,8,LOOPING(TNUM)) = FPRICE(ICLS,4,NRGN)/MC_JPGDP(CURIYR)
                  IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
                    COLVALS(:,:) = COLV(TNUM,:,:)
!                   CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                    CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                    LOOPING(TNUM) = 0
                  ENDIF
                ENDDO
              ENDIF
!
!      --- WRITE OUT ANY LEFT OVER RECORDS TO THE NEMS DATA BASE
!
       DO TNUM = 1 , NUMTABS
         IF (LOOPING(TNUM) .NE. 0) THEN
           COLVALS(:,:) = COLV(TNUM,:,:)
!          CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
           CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
           LOOPING(TNUM) = 0
         ENDIF
       ENDDO
!
     ENDIF


      RETURN
      END
      SUBROUTINE TRANSM(NRGN,ICALL)
      IMPLICIT NONE
!******************************************************************
! THIS SUBROUTINE INVOKES TRANSMISSION ACCOUNTING COMPUTATIONS
!*******************************************************************
! INPUT VARIABLES:
!     ICALL = OPERATING COMPONENT IDENTIFICATION
!     NRGN = CURRENT REGION
!*******************************************************************
      CHARACTER*1 ICALL
      INTEGER NRGN
      CALL AVRPRC(NRGN,ICALL)
      RETURN
      END
      SUBROUTINE DISTRI(NRGN,ICALL)
      IMPLICIT NONE
!******************************************************************
! THIS SUBROUTINE INVOKES DISTRIBUTION ACCOUNTING COMPUTATIONS
!*******************************************************************
! INPUT VARIABLES:
!     ICALL = OPERATING COMPONENT IDENTIFICATION
!     NRGN = CURRENT REGION
!*******************************************************************
      CHARACTER*1 ICALL
      INTEGER NRGN
      CALL AVRPRC(NRGN,ICALL)
      RETURN
      END
      SUBROUTINE GETEB(NRGN,ICALL)
      IMPLICIT NONE
!******************************************************************
! THIS SUBROUTINE RETRIEVES ARRAYS DESCRIBING EACH BUILD APPLICABLE TO THE CURRENT REGION AND OPERATING COMPONENT
!    THE OPERATING COMPONENTS ARE TRANSMISSION, DISTRIBUTION, AND GENERATION
!    THE ARRAYS ARE REFERRED TO AS THE EB ARRAYS
!******************************************************************
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'efpbld'
      include 'bldctl'
      INTEGER ILINK    !STEPS THROUGH THE LINKED LIST
      INTEGER NUM      !BUILD COUNTER
      INTEGER I
      CHARACTER*1 ICALL
      INTEGER NRGN
      NUM = 1
      IF (ICALL .EQ. 'G') THEN
         ILINK = BLDLNK(NRGN,1)
      ELSE
         IF (ICALL .EQ. 'T') THEN
            ILINK = BLDLNK(NRGN,3)
         ELSE
            ILINK = BLDLNK(NRGN,5)
         ENDIF
      ENDIF
      DO WHILE (ILINK .NE. 0)
         READ(IBD,REC=ILINK)BLINK,(EBDITC(NUM,I),I=1,2), &
                            (EBAFDC(NUM,I),I=1,2), &
                            (EBBKVL(NUM,I),I=1,2), &
                            (EBASVL(NUM,I),I=1,2), &
                            (EBBCWP(NUM,I),I=1,2), &
                            (EBRCWP(NUM,I),I=1,2), &
                            EBPCST(NUM), &
                            (EBPCAP(NUM,I),I=1,2),EBSERP(NUM), &
                            (EBABDE(NUM,I),I=1,2), &
                            EBPTYP(NUM),EBSYR(NUM)

         WRITE(18,1301) CURIYR+1989, CURITR, NRGN, ICALL, ILINK, NUM, BLINK, EBPTYP(NUM), EBSYR(NUM) + 1989, EBPCST(NUM), EBSERP(NUM),  &
            EBPCAP(NUM,1), EBBKVL(NUM,1), EBASVL(NUM,1), EBAFDC(NUM,1), EBBCWP(NUM,1), EBRCWP(NUM,1), EBABDE(NUM,1),  &
            EBPCAP(NUM,2), EBBKVL(NUM,2), EBASVL(NUM,2), EBAFDC(NUM,2), EBBCWP(NUM,2), EBRCWP(NUM,2), EBABDE(NUM,2)
 1301    FORMAT(1X,"GETEB",3(":",I4),":",A1,5(":",I5),16(":",F9.3))

         NUM = NUM + 1
         ILINK = BLINK

      END DO

      EBNUM = NUM - 1

      RETURN
      END

      SUBROUTINE STREB(NRGN,ICALL)
      IMPLICIT NONE
!******************************************************************
! THIS SUBROUTINE CREATES ARRAYS DESCRIBING EACH BUILD APPLICABLE TO THE CURRENT REGION AND OPERATING COMPONENT
!    THE OPERATING COMPONENTS ARE TRANSMISSION, DISTRIBUTION, AND GENERATION
!    THE ARRAYS ARE REFERRED TO AS THE EB ARRAYS
!******************************************************************
      INTEGER O
      PARAMETER (O=2)
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'efpbld'
      include 'bldctl'
      INTEGER ILINK    !STEPS THROUGH THE LINKED LIST
      INTEGER NUM      !BUILD COUNTER
      INTEGER I
      INTEGER J
      CHARACTER*1 ICALL
      INTEGER NRGN
      REAL*4 TDITC(O)  !TEMPORARY DEFERRED INVESTMENT TAX CREDIT
      REAL*4 TAFDC(O)  !TEMPORARY AFUDC
      REAL*4 TBKVL(O)  !TEMPORARY BOOK VALUE
      REAL*4 TASVL(O)  !TEMPORARY ASSET VALUE
      REAL*4 TBCWP(O)  !TEMPORARY BOOKED CWIP
      REAL*4 TRCWP(O)  !TEMPORARY CWIP IN RATE BASE
      INTEGER TPTYP    !TEMPORARY PLANT TYPE
      INTEGER TSYR     !TEMPORARY START YEAR
      REAL*4 TPCST     !TEMPORARY PLANT COST
      REAL*4 TPCAP(O)  !TEMPORARY NAME PLATE CAPACITY
      REAL*4 TSERP     !TEMPORARY % OF FIRST YEAR IN SERVICE
      REAL*4 TABDE(O)  !TEMPORARY ACCUMULATED BOOK DEPRECIATION
      IF (ICALL .EQ. 'G') THEN
         ILINK = BLDLNK(NRGN,1)
      ELSE
         IF (ICALL .EQ. 'T') THEN
            ILINK = BLDLNK(NRGN,3)
         ELSE
            ILINK = BLDLNK(NRGN,5)
         ENDIF
      ENDIF
      IF (EBNUM .GT. 0) THEN
         DO J=1,EBNUM
            READ(IBD,REC=ILINK)BLINK, &
                               (TDITC(I),I=1,2),(TAFDC(I),I=1,2), &
                               (TBKVL(I),I=1,2),(TASVL(I),I=1,2), &
                               (TBCWP(I),I=1,2),(TRCWP(I),I=1,2), &
                               TPCST,(TPCAP(I),I=1,2),TSERP, &
                               (TABDE(I),I=1,2),TPTYP,TSYR
            WRITE(IBD,REC=ILINK)BLINK,(EBDITC(J,I),I=1,2), &
                                (EBAFDC(J,I),I=1,2), &
                                (EBBKVL(J,I),I=1,2), &
                                (EBASVL(J,I),I=1,2), &
                                (EBBCWP(J,I),I=1,2), &
                                (EBRCWP(J,I),I=1,2), &
                                EBPCST(J), &
                                (EBPCAP(J,I),I=1,2),EBSERP(J), &
                                (EBABDE(J,I),I=1,2), &
                                EBPTYP(J),EBSYR(J)

            WRITE(18,1301) CURIYR+1989, CURITR, NRGN, ICALL, ILINK, j, BLINK, EBPTYP(j), EBSYR(j) + 1989, EBPCST(j), EBSERP(j),  &
               EBPCAP(j,1), EBBKVL(j,1), EBASVL(j,1), EBAFDC(j,1), EBBCWP(j,1), EBRCWP(j,1), EBABDE(j,1),  &
               EBPCAP(j,2), EBBKVL(j,2), EBASVL(j,2), EBAFDC(j,2), EBBCWP(j,2), EBRCWP(j,2), EBABDE(j,2)
 1301       FORMAT(1X,"STREB",3(":",I4),":",A1,5(":",I5),16(":",F9.3))

            ILINK = BLINK
         END DO
      ENDIF
      RETURN
      END

      SUBROUTINE ELSET
      IMPLICIT NONE
!*******************************************************************
!   THIS SUBROUTINE ASSIGNS ELECTRICITY PRICES BY SECTOR AND REGION USING A NERC TO CENSUS REGION MAP
!*******************************************************************
!   PARAMETERS:
!     MNUMCR--NUMBER OF CENSUS REGIONS 'INCLUDES TOTAL.'
!     MNUMNR--NUMBER OF NERC REGIONS 'INCLUDES TOTAL.'
      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'control'
      include 'dsmdimen'
      include 'pq'
      include 'elshrs'
!     include 'emmout'
      include 'uefpout'     ! UEFP output variables
      include 'uefdout'     ! EFD output variables
      include 'udatout'     ! UDAT output variables
      include 'uecpout'     ! UECP output variables
      include 'uldsmout'    ! LDSM output variables
      include 'uettout'     ! UETT output variables
      include 'efpout'
      include 'efpgen'
      include 'eusprc'
      include 'efpint'
      include 'efprp2'
      include 'dispout'
      include 'dsmsectr'
      include 'ldsm'
      include 'macout'
      include 'efpcntrl'
      include 'tranrep'
      include 'dsmunits'
      include 'elcntl'
      include 'elout'
      REAL*4 NUM0
      REAL*4 NUM1
      REAL*4 NUM2
      REAL*4 NUM3
      REAL*4 NUM4
      REAL*4 NUM5
      REAL*4 NUM6
      REAL*4 DEN1
      REAL*4 DEN2
      REAL*4 DEN3
      REAL*4 DEN4
      REAL*4 DEN5
      REAL*4 TOT
      REAL*4 DENOM
      REAL*4 BNCHFCTR(9)
      REAL*4 STEOPRC(8)
      INTEGER J
      INTEGER I,K,N,EUG
      INTEGER NERC
      INTEGER CENSUS
      INTEGER YEAR,BYR1,BYR2
      INTEGER YR,FULLYR
      INTEGER REGION
      REAL*4 PRS
      REAL*4 PCM
      REAL*4 PTR
      REAL*4 PIN
      REAL*4 PAS
      REAL*4 margprc
      REAL*4 demwt(MNEUGRP,MNUMCR),demwtnr(MNEUGRP,MNUMNR)
      REAL*4 totall(5,MNUMCR),mapcnavg(MNUMNR,MNUMCR)
      REAL*4 totallnr(5,MNUMNR),temp
      REAL*4 temp1,temp2,temp3,temp4,temp5
      INTEGER l,indx,bd,sv,EU,sectr,c,comp
      REAL*4 num(MNEUGRP),den(MNEUGRP)
      REAL*4 NUMER1(10),NUMER2(10),NUMER3(10),NUMER4(10),NUMER5(10)
      REAL*4 totrev,totrevt,totrevd,indnew,clprnew
      REAL*4 tranrat,transal ! transportation/industrial ratio
      REAL*4 SALTOTA(MNUMNR)

      REAL*8 COFF(4),P,P1,X, CALCSEC,PG(3),Q(4)
      REAL*8 lambda,beta(3),gam(3),TOTSEC,alpha(3)
      REAL*8 PSEC(4),sdval,nuval,newtd,bval
      REAL newtp(3,MNUMNR-3)
      REAL newdp(3,MNUMNR-3)
      REAL SUMCN22(MNUMCR-1)
      REAL RVMappCtoN(MNUMNR-3,MNUMCR-1,4)

      INTEGER tdbsyr,iyav
      INTEGER ICLS
      CHARACTER*10 NM

!     put in common block later!

      real PECRSFXN(MNUMNR,MNUMYR)
      real PECCMFXN(MNUMNR,MNUMYR)
      real PECINFXN(MNUMNR,MNUMYR)
      real PECTRRLN(MNUMNR,MNUMYR),PECTRMEN(MNUMNR,MNUMYR), &
           PECTRFXN(MNUMNR,MNUMYR),PECTRTXN(MNUMNR,MNUMYR), &
           PECTRTDN(MNUMNR,MNUMYR)

!     real dgapold

      real tempreg(MNEUGRP,MNUMNR),tempregold(MNEUGRP,MNUMNR)
      COMMON /efpprcsmooth/tempregold

      REAL ERDSCAR2(MNUMNR)
      COMMON /WAX_PRC/ ERDSCAR2

      FULLYR = USYEAR(CURIYR)

!     assign tempregold for smoothing - not on first year, first iteration

      do NERC = 1,UNRGNS
         do l = 1,MNEUGRP
            if ((FULLYR .EQ. UESTYR) .and. (CURITR .EQ. 1)) then
               tempregold(l,NERC) = 0.0      !filler, we won't use this anyway
            else
               if (CURITR .eq. 1) then
                  tempregold(l,NERC) = tempreg(l,NERC)* MC_JPGDP(CURIYR)/MC_JPGDP(CURIYR-1)
               else
                  tempregold(l,NERC) = tempreg(l,NERC)
               endif
            endif
         enddo
      enddo


!     READ IN SECTORAL PRICES BY NERC REGION

      YEAR = CURIYR

!     start new code - get demand weights for 23 end uses - value will be percent of total for sector

      l = 0
      do sectr = 1,4
         DO EU = 1,NEUSGRP(sectr)
            l = l+1
            DO NERC = 1,MNUMNR
               demwtnr(l,NERC) = 0.0
            ENDDO
            DO CENSUS = 1, MNUMCR
               demwt(l,CENSUS) = 0.0
            ENDDO
         ENDDO
      enddo

      do sectr = 1,5
         do NERC = 1,MNUMNR
            totallnr(sectr,NERC) = 0.0
         enddo
         do CENSUS = 1, MNUMCR
            totall(sectr,CENSUS) = 0.0
         enddo
      enddo

      do NERC = 1,UNRGNS
         l = 0
         do sectr = 1,4
            do eu = 1,NEUSGRP(sectr)
               l = l+1
               demwtnr(l,NERC) = TOTEULOAD(l,NERC)
               totallnr(sectr,NERC) = totallnr(sectr,NERC) + demwtnr(l,NERC)
            enddo
            totallnr(5,NERC) = totallnr(5,NERC) + totallnr(sectr,NERC)
         enddo
      enddo

      do NERC = 1, UNRGNS
         l = 0
         do sectr = 1,4
            do eu = 1,NEUSGRP(sectr)
               l = l+1
               IF (totallnr(sectr,NERC) .GT. 0.0) THEN
                  demwtnr(l,NERC) = demwtnr(l,NERC) / totallnr(sectr,NERC)
               ELSE
                  demwtnr(l,NERC) = 0.0
               END IF

!              IF (isnan(DEMWTNR(l,NERC))) &
               WRITE(IMSG,3311) CURIRUN, CURIYR+1989, CURITR, NERC, sectr, eu, l, demwtnr(l,NERC), totallnr(sectr,NERC), TOTEULOAD(l,NERC)
 3311          FORMAT(1X,"DEMWTNR_DBG",7(":",I4),3(":",F15.6))

            enddo
         enddo
      enddo

!     create revised mapping to exclude AK/HI because EMM doesn't project prices for them
      IF ((CURIYR+UHBSYR) .EQ. UESTYR .AND. CURITR .EQ. 1) THEN

      DO sectr = 1,4
       SUMCN22 = 0.0
       DO NERC = 1, UNRGNS
        DO CENSUS = 1, MNUMCR-1
          SUMCN22(CENSUS) = SUMCN22(CENSUS) + MAPPCtoN(NERC,CENSUS,sectr)
        ENDDO
       ENDDO

       DO NERC = 1, UNRGNS
        DO CENSUS = 1, MNUMCR-1
         IF (SUMCN22(CENSUS) .GT. 0.0) THEN
          RVMappCtoN(NERC,CENSUS,sectr) = MAPPCtoN(NERC,CENSUS,sectr) / SUMCN22(CENSUS)
         ELSE
          RVMappCtoN(NERC,CENSUS,sectr) = 0.0
         ENDIF
        ENDDO
       ENDDO
      DO NERC = 1, UNRGNS
 !       write(IMSG,3315) 'rvmapp',NERC,sectr,(RVMappCtoN(NERC,CENSUS,sectr),CENSUS=1,MNUMCR-1)
      enddo
      ENDDO

3315  format(1x,a15,2(i5,1x),10F13.5)

!     create average nerc to census map from the sector mappings (MappCtoN)

      DO NERC = 1,MNUMNR
         do CENSUS = 1,MNUMCR
            mapcnavg(NERC,CENSUS) = 0.0
         enddo
      ENDDO

      DO NERC = 1,UNRGNS
         DO CENSUS = 1, MNUMCR-2
            do sectr = 1, 4
               mapcnavg(NERC,CENSUS) = mapcnavg(NERC,CENSUS) + RVMappCtoN(NERC,CENSUS,sectr)*SALCLS(NERC,sectr)
            enddo
            mapcnavg(NERC,CENSUS) = mapcnavg(NERC,CENSUS)/(SALCLS(NERC,1) + SALCLS(NERC,2) + SALCLS(NERC,3) + SALCLS(NERC,4))
         ENDDO
      ENDDO

      DO NERC = 1, UNRGNS
         DO sectr = 1, 4
!              write(IMSG,3315) 'rvmapp2',NERC,sectr,(RVMappCtoN(NERC,CENSUS,sectr),CENSUS=1,MNUMCR-1)
         ENDDO
      enddo

      ENDIF    ! revised mappings

      SALTOTA=0.0
      DO NERC = 1, UNRGNS
        DO ICLS=1,NCLASS
           SALTOTA(NERC) = SALTOTA(NERC) + SALCLS(NERC,ICLS)
        END DO
      ENDDO

!     start new code - marginal cost pricing

      DENOM = MC_JPGDP(YEAR)
      DO NERC=1,MNUMNR
         IF (NERC .LE. UNRGNS) THEN

            WRITE(IMSG,7317) CURIRUN, CURIYR+1989, CURITR, YEAR+1989, NERC, USW_POL, NCLASS, COMPRM(YEAR,NERC), (neusgrp(I), I = 1,NCLASS), &
               FRMARG(YEAR,NERC), EPRICE(1,1,NERC), EPRICE(1,2,NERC), EPRICE(1,3,NERC), EPRICE(1,4,NERC)
 7317       FORMAT(1X,"NCLASS_DBG_11",8(":",I4),<NCLASS>(":",I2),20(":",F9.3))

           WRITE(IMSG,7327) CURIRUN, CURIYR+1989, CURITR, YEAR+1989, NERC, USW_POL, NCLASS, COMPRM(YEAR,NERC), (neusgrp(I), I = 1,NCLASS), &
                FRMARG(YEAR,NERC), EPRICE(2,1,NERC), EPRICE(2,2,NERC), EPRICE(2,3,NERC), EPRICE(2,4,NERC)
 7327       FORMAT(1X,"NCLASS_DBG_12",8(":",I4),<NCLASS>(":",I2),20(":",F9.3))

           WRITE(IMSG,7337) CURIRUN, CURIYR+1989, CURITR, YEAR+1989, NERC, USW_POL, NCLASS, COMPRM(YEAR,NERC), (neusgrp(I), I = 1,NCLASS), &
                FRMARG(YEAR,NERC), EPRICE(3,1,NERC), EPRICE(3,2,NERC), EPRICE(3,3,NERC), EPRICE(3,4,NERC)
 7337       FORMAT(1X,"NCLASS_DBG_13",8(":",I4),<NCLASS>(":",I2),20(":",F9.3))

           WRITE(IMSG,7347) CURIRUN, CURIYR+1989, CURITR, YEAR+1989, NERC, USW_POL, NCLASS, COMPRM(YEAR,NERC), (neusgrp(I), I = 1,NCLASS), &
                FRMARG(YEAR,NERC), EPRICE(4,1,NERC), EPRICE(4,2,NERC), EPRICE(4,3,NERC), EPRICE(4,4,NERC)
 7347       FORMAT(1X,"NCLASS_DBG_14",8(":",I4),<NCLASS>(":",I2),20(":",F9.3))

            PELRSNR(NERC,YEAR) = (EPRICE(1,4,NERC)/DENOM)
            PELCMNR(NERC,YEAR) = (EPRICE(2,4,NERC)/DENOM)
            PELTRNR(NERC,YEAR) = (EPRICE(4,4,NERC)/DENOM)
            PELINNR(NERC,YEAR) = (EPRICE(3,4,NERC)/DENOM)
            PELASNR(NERC,YEAR) = (EPRICE(5,4,NERC)/DENOM)
            PELFLNR(NERC,YEAR) = (ECOST(1,NERC)/DENOM)
            PELOMNR(NERC,YEAR) = (ECOST(2,NERC)/DENOM)
            PELTLNR(NERC,YEAR) = PELASNR(NERC,YEAR)
            PELWHNR(NERC,YEAR) = (ECOST(3,NERC)/DENOM)
            PELCPNR(NERC,YEAR) = PELTLNR(NERC,YEAR) - &
                                 PELFLNR(NERC,YEAR) - &
                                 PELOMNR(NERC,YEAR) - &
                                 PELWHNR(NERC,YEAR)
!
            ULEPRS(NERC) = (EPRICE(1,4,NERC)/DENOM)
            ULEPCM(NERC) = (EPRICE(2,4,NERC)/DENOM)
            ULEPIN(NERC) = (EPRICE(3,4,NERC)/DENOM)
            ULEPTR(NERC) = (EPRICE(4,4,NERC)/DENOM)
            ULEPAS(NERC) = (EPRICE(5,4,NERC)/DENOM)
            ULEPFL(NERC) = (ECOST(1,NERC)/DENOM)
            ULEPOM(NERC) = (ECOST(2,NERC)/DENOM)
            ULEPWP(NERC) = (ECOST(3,NERC)/DENOM)
            ULEPCP(NERC) = ULEPAS(NERC) - ULEPFL(NERC) - &
               ULEPOM(NERC) - ULEPWP(NERC)

!           fill competitive sector price with weighted avg of above
!           must do t&d after these calculations

            if (USW_POL .eq. 4) then
               l = 0; temp=0.0
               temp1= 0.0; temp2= 0.0; temp3= 0.0; temp4=0.0; temp5=0.0
               do eu = 1, NEUSGRP(1)
                  l = l+1
                  temp1 = temp1 + demwtnr(l,NERC)*COMPCOMP(1,l,NERC)
                  temp2 = temp2 + demwtnr(l,NERC)*COMPCOMP(2,l,NERC)
                  temp3 = temp3 + demwtnr(l,NERC)*COMPCOMP(3,l,NERC)
                  temp4 = temp4 + demwtnr(l,NERC)*COMPCOMP(4,l,NERC)
               enddo
               PECRSRLN(NERC,YEAR) = temp1/DENOM
               PECRSMEN(NERC,YEAR) = temp2/DENOM
               PECRSFXN(NERC,YEAR) = temp3/DENOM
               PECRSTXN(NERC,YEAR) = temp4/DENOM
               temp = 0.0
               temp1= 0.0; temp2= 0.0; temp3= 0.0; temp4=0.0; temp5=0.0
               do eu = 1, NEUSGRP(2)
                  l = l+1
                  temp1 = temp1 + demwtnr(l,NERC)*COMPCOMP(1,l,NERC)
                  temp2 = temp2 + demwtnr(l,NERC)*COMPCOMP(2,l,NERC)
                  temp3 = temp3 + demwtnr(l,NERC)*COMPCOMP(3,l,NERC)
                  temp4 = temp4 + demwtnr(l,NERC)*COMPCOMP(4,l,NERC)
               enddo
               PECCMRLN(NERC,YEAR) = temp1/DENOM
               PECCMMEN(NERC,YEAR) = temp2/DENOM
               PECCMFXN(NERC,YEAR) = temp3/DENOM
               PECCMTXN(NERC,YEAR) = temp4/DENOM
               temp = 0.0
               temp1= 0.0; temp2= 0.0; temp3= 0.0; temp4=0.0; temp5=0.0
               do eu = 1, NEUSGRP(3)
                  l = l+1
                  temp1 = temp1 + demwtnr(l,NERC)*COMPCOMP(1,l,NERC)
                  temp2 = temp2 + demwtnr(l,NERC)*COMPCOMP(2,l,NERC)
                  temp3 = temp3 + demwtnr(l,NERC)*COMPCOMP(3,l,NERC)
                  temp4 = temp4 + demwtnr(l,NERC)*COMPCOMP(4,l,NERC)
               enddo
               PECINRLN(NERC,YEAR) = temp1/DENOM
               PECINMEN(NERC,YEAR) = temp2/DENOM
               PECINFXN(NERC,YEAR) = temp3/DENOM
               PECINTXN(NERC,YEAR) = temp4/DENOM
               temp = 0.0
               temp1= 0.0; temp2= 0.0; temp3= 0.0; temp4=0.0; temp5=0.0
               do eu = 1, NEUSGRP(4)
                  l = l+1
                  temp1 = temp1 + demwtnr(l,NERC)*COMPCOMP(1,l,NERC)
                  temp2 = temp2 + demwtnr(l,NERC)*COMPCOMP(2,l,NERC)
                  temp3 = temp3 + demwtnr(l,NERC)*COMPCOMP(3,l,NERC)
                  temp4 = temp4 + demwtnr(l,NERC)*COMPCOMP(4,l,NERC)
               enddo
               PECTRRLN(NERC,YEAR) = temp1/DENOM
               PECTRMEN(NERC,YEAR) = temp2/DENOM
               PECTRFXN(NERC,YEAR) = temp3/DENOM
               PECTRTXN(NERC,YEAR) = temp4/DENOM

!  code commented out, cannot be used unless tfac/dfac are moved to new DB input
!              if (FRMARG(YEAR,NERC) .gt. 0.0) then   
!                 totrev = 0.0
!                 totrevt = 0.0
!                 totrevd = 0.0

!                 DO I=1,NCLASS - 1
!                    totrevt = totrevt +  salcls(NERC,I) * &
!                       EPRICE(I,2,NERC)/DENOM
!                    totrevd = totrevd +  salcls(NERC,I) * &
!                       EPRICE(I,3,NERC)/DENOM
!                    Q(I) = salcls(NERC,I)
!                 END DO ! I
!                 totrevd = totrevd + ERDSCAR2(NERC)

!                 CALCSEC = 0.0
!                 DO I=1,NCLASS - 1
!                    newtp(I,NERC)=totrevt * tfac(I,NERC)
!                    newdp(I,NERC)=totrevd * dfac(I,NERC)
!                    IF (Q(I) .GT. 0.0) THEN
!                       PSEC(I) = (newtp(I,NERC) + newdp(I,NERC)) / Q(I)
!                       CALCSEC = CALCSEC + PSEC(I) * Q(I)
!                    ELSE
!                        PSEC(I) = 0.0
!                    END IF

!                    IF (isnan(PSEC(I)) .OR. PSEC(I) .LT. 0.0 .OR. PSEC(I) .GT. 1000.0) THEN
!                       WRITE(6,1031) CURIRUN, CURIYR+1989, YEAR, CURITR, NERC, I, &
!                          PSEC(I), newtp(I,NERC), newdp(I,NERC), totrevt, totrevd, tfac(I,NERC), dfac(I,NERC), Q(I)
1031                    FORMAT(1X,"PSEC_DBG",6(":",I4),8(":",F12.3))
!                    END IF

!                 END DO

!                 fill in transportation with regulated version
!                 PSEC(NCLASS) = (EPRICE(NCLASS,2,NERC) + EPRICE(NCLASS,3,NERC))/DENOM
!                 CALCSEC = CALCSEC + PSEC(NCLASS) * Q(NCLASS)

!              endif  !frmarg
            endif    !USW_POL

            WRITE(IMSG,7318) CURIRUN, CURIYR+1989, CURITR, YEAR+1989, NERC, USW_POL, NCLASS, COMPRM(YEAR,NERC), (neusgrp(I), I = 1,NCLASS), FRMARG(YEAR,NERC)
 7318       FORMAT(1X,"NCLASS_DBG_2",8(":",I4),<NCLASS>(":",I2),":",F6.3)

            K = 1
            DO I = 1, NCLASS
               DO J = 1, neusgrp(I)
! COMPRM is no longer used and all sectoral T&D prices are based on same regulated allocation methods
!                 if (FRMARG(YEAR,NERC) .GT. 0.0 .AND. USW_POL .eq. 4 .AND.  COMPRM(YEAR,NERC) .EQ. 2 .AND. Q(I) .GT. 0.0) then

!                     margprc = EPRIC2(K,NERC) + PSEC(I)*DENOM - (ERDSCAR2(NERC) * DENOM / SALTOTA(NERC))
!                     tempreg(K,NERC) = FRMARG(YEAR,NERC) * margprc + (1.0 - FRMARG(YEAR,NERC)) * EPRICE(I,4,NERC)

!                     COMPCOMP(5,K,NERC) = PSEC(I)*DENOM - (ERDSCAR2(NERC) * DENOM / SALTOTA(NERC))

 2031                 FORMAT(1X,"TEMPREG_",A1,"_",A3,":",F12.3,10(":",I4),11(":",F12.3))
!                     IF (isnan(tempreg(K,NERC))) THEN
!                        WRITE(6,2031) "A","NaN",tempreg(K,NERC),     &    ! featured output
!                           CURIRUN, CURCALYR, YEAR, CURITR, NERC,    &    ! integers
!                           I, J, K, USW_POL, COMPRM(YEAR,NERC),      &    ! more integers
!                           DENOM, FRMARG(YEAR,NERC),                 &    ! the rest are real
!                           margprc, EPRIC2(K,NERC), PSEC(I), ERDSCAR2(NERC), SALTOTA(NERC), &
!                           EPRICE(I,2,NERC), EPRICE(I,3,NERC), EPRICE(I,4,NERC), EPRICE(I,1,NERC)
!                     ELSEIF (tempreg(K,NERC) .LE. 0.0) THEN
!                        WRITE(6,2031) "A","Neg",tempreg(K,NERC),     &    ! featured output
!                           CURIRUN, CURCALYR, YEAR, CURITR, NERC,    &    ! integers
!                           I, J, K, USW_POL, COMPRM(YEAR,NERC),      &    ! more integers
!                           DENOM, FRMARG(YEAR,NERC),                 &    ! the rest are real
!                           margprc, EPRIC2(K,NERC), PSEC(I), ERDSCAR2(NERC), SALTOTA(NERC), &
!                           EPRICE(I,2,NERC), EPRICE(I,3,NERC), EPRICE(I,4,NERC), EPRICE(I,1,NERC)
!                     ELSEIF (tempreg(K,NERC)/MC_JPGDP(YEAR) .GT. 350.0) THEN
!                        WRITE(IMSG,2031) "A","Big",tempreg(K,NERC),  &    ! featured output
!                           CURIRUN, CURCALYR, YEAR, CURITR, NERC,    &    ! integers
!                           I, J, K, USW_POL, COMPRM(YEAR,NERC),      &    ! more integers
!                           DENOM, FRMARG(YEAR,NERC),                 &    ! the rest are real
!                           margprc, EPRIC2(K,NERC), PSEC(I), ERDSCAR2(NERC), SALTOTA(NERC), &
!                           EPRICE(I,2,NERC), EPRICE(I,3,NERC), EPRICE(I,4,NERC), EPRICE(I,1,NERC)
!                     END IF

!                 elseif (FRMARG(YEAR,NERC) .GT. 0.0 .AND. USW_POL .eq. 4 .AND. (COMPRM(YEAR,NERC) .EQ. 1 .OR. Q(I) .EQ. 0.0)) then
                  if (FRMARG(YEAR,NERC) .GT. 0.0 .AND. USW_POL .eq. 4 ) then
                     margprc = EPRIC2(K,NERC)+ EPRICE(I,2,NERC) + EPRICE(I,3,NERC)
                     tempreg(K,NERC) = FRMARG(YEAR,NERC) * margprc + (1.0 - FRMARG(YEAR,NERC)) * EPRICE(I,4,NERC)
                     COMPCOMP(5,K,NERC) = EPRICE(I,2,NERC) + EPRICE(I,3,NERC)   !fill t&d in anyway

                      IF (isnan(tempreg(K,NERC))) THEN
                         WRITE(6,2031) "B","NaN",tempreg(K,NERC),     &    ! featured output
                            CURIRUN, CURCALYR, YEAR, CURITR, NERC,    &    ! integers
                            I, J, K, USW_POL, COMPRM(YEAR,NERC),      &    ! more integers
                            DENOM, FRMARG(YEAR,NERC),                 &    ! the rest are real
                            margprc, EPRIC2(K,NERC), PSEC(I), ERDSCAR2(NERC), SALTOTA(NERC), &
                            EPRICE(I,2,NERC), EPRICE(I,3,NERC), EPRICE(I,4,NERC), EPRICE(I,1,NERC)
                      ELSEIF (tempreg(K,NERC) .LE. 0.0) THEN
                         WRITE(6,2031) "B","Neg",tempreg(K,NERC),     &    ! featured output
                            CURIRUN, CURCALYR, YEAR, CURITR, NERC,    &    ! integers
                            I, J, K, USW_POL, COMPRM(YEAR,NERC),      &    ! more integers
                            DENOM, FRMARG(YEAR,NERC),                 &    ! the rest are real
                            margprc, EPRIC2(K,NERC), PSEC(I), ERDSCAR2(NERC), SALTOTA(NERC), &
                            EPRICE(I,2,NERC), EPRICE(I,3,NERC), EPRICE(I,4,NERC), EPRICE(I,1,NERC)
                      ELSEIF (tempreg(K,NERC)/MC_JPGDP(YEAR) .GT. 350.0) THEN
                         WRITE(IMSG,2031) "B","Big",tempreg(K,NERC),  &    ! featured output
                            CURIRUN, CURCALYR, YEAR, CURITR, NERC,    &    ! integers
                            I, J, K, USW_POL, COMPRM(YEAR,NERC),      &    ! more integers
                            DENOM, FRMARG(YEAR,NERC),                 &    ! the rest are real
                            margprc, EPRIC2(K,NERC), PSEC(I), ERDSCAR2(NERC), SALTOTA(NERC), &
                            EPRICE(I,2,NERC), EPRICE(I,3,NERC), EPRICE(I,4,NERC), EPRICE(I,1,NERC)
                      END IF

                  else
                     tempreg(K,NERC) = EPRICE(I,4,NERC)
                     COMPCOMP(5,K,NERC) = EPRICE(I,2,NERC) + EPRICE(I,3,NERC)   !fill t&d in anyway

                      IF (isnan(tempreg(K,NERC))) THEN
                         WRITE(6,2031) "C","NaN",tempreg(K,NERC),     &    ! featured output
                            CURIRUN, CURCALYR, YEAR, CURITR, NERC,    &    ! integers
                            I, J, K, USW_POL, COMPRM(YEAR,NERC),      &    ! more integers
                            DENOM, FRMARG(YEAR,NERC),                 &    ! the rest are real
                            margprc, EPRIC2(K,NERC), PSEC(I), ERDSCAR2(NERC), SALTOTA(NERC), &
                            EPRICE(I,2,NERC), EPRICE(I,3,NERC), EPRICE(I,4,NERC), EPRICE(I,1,NERC)
                      ELSEIF (tempreg(K,NERC) .LE. 0.0) THEN
                         WRITE(6,2031) "C","Neg",tempreg(K,NERC),     &    ! featured output
                            CURIRUN, CURCALYR, YEAR, CURITR, NERC,    &    ! integers
                            I, J, K, USW_POL, COMPRM(YEAR,NERC),      &    ! more integers
                            DENOM, FRMARG(YEAR,NERC),                 &    ! the rest are real
                            margprc, EPRIC2(K,NERC), PSEC(I), ERDSCAR2(NERC), SALTOTA(NERC), &
                            EPRICE(I,2,NERC), EPRICE(I,3,NERC), EPRICE(I,4,NERC), EPRICE(I,1,NERC)
                      ELSEIF (tempreg(K,NERC)/MC_JPGDP(YEAR) .GT. 350.0) THEN
                         WRITE(IMSG,2031) "C","Big",tempreg(K,NERC),  &    ! featured output
                            CURIRUN, CURCALYR, YEAR, CURITR, NERC,    &    ! integers
                            I, J, K, USW_POL, COMPRM(YEAR,NERC),      &    ! more integers
                            DENOM, FRMARG(YEAR,NERC),                 &    ! the rest are real
                            margprc, EPRIC2(K,NERC), PSEC(I), ERDSCAR2(NERC), SALTOTA(NERC), &
                            EPRICE(I,2,NERC), EPRICE(I,3,NERC), EPRICE(I,4,NERC), EPRICE(I,1,NERC)
                      END IF

                  endif  ! frmarg

                  if (isnan(tempreg(K,NERC)) .OR. tempreg(k,NERC)/MC_JPGDP(YEAR) .gt. 350.0 .OR. tempreg(k,NERC)/MC_JPGDP(YEAR) .LT. 1.0) tempreg(k,NERC) = 350.0*MC_JPGDP(YEAR)
                  K = K+1
               ENDDO ! J
            ENDDO ! I

            l = 0; temp=0.0
            temp1= 0.0; temp2= 0.0; temp3= 0.0; temp4=0.0; temp5=0.0
            do eu = 1, NEUSGRP(1)
               l = l+1
               temp = temp + demwtnr(l,NERC)*tempreg(l,NERC)
               temp5 = temp5 + demwtnr(l,NERC)*COMPCOMP(5,l,NERC)
            enddo
            PECRSTDN(NERC,YEAR) = temp5/DENOM
            PECRSNR(NERC,YEAR) = temp/DENOM

            temp = 0.0
            temp1= 0.0; temp2= 0.0; temp3= 0.0; temp4=0.0; temp5=0.0
            do eu = 1, NEUSGRP(2)
               l = l+1
               temp = temp + demwtnr(l,NERC)*tempreg(l,NERC)
               temp5 = temp5 + demwtnr(l,NERC)*COMPCOMP(5,l,NERC)
            enddo
            PECCMTDN(NERC,YEAR) = temp5/DENOM
            PECCMNR(NERC,YEAR) = temp/DENOM

            temp = 0.0
            temp1= 0.0; temp2= 0.0; temp3= 0.0; temp4=0.0; temp5=0.0
            do eu = 1, NEUSGRP(3)
               l = l+1
               temp = temp + demwtnr(l,NERC)*tempreg(l,NERC)
               temp5 = temp5 + demwtnr(l,NERC)*COMPCOMP(5,l,NERC)

!              IF (NERC .EQ. 7) THEN
!                 WRITE(6,7033) CURIRUN, CURIYR+1989, YEAR, CURITR, NERC, eu, l, PECINNR(NERC,YEAR), demwtnr(l,NERC), tempreg(l,NERC)
!7033             FORMAT(1X,"PECINNR_7",7(":",I4),25(":",F12.3))
!              END IF

            enddo
            PECINTDN(NERC,YEAR) = temp5/DENOM
            PECINNR(NERC,YEAR) = temp/DENOM

            temp = 0.0
            temp1= 0.0; temp2= 0.0; temp3= 0.0; temp4=0.0; temp5=0.0
            do eu = 1, NEUSGRP(4)
               l = l+1
               temp = temp + demwtnr(l,NERC)*tempreg(l,NERC)
               temp5 = temp5 + demwtnr(l,NERC)*COMPCOMP(5,l,NERC)
            enddo
            PECTRTDN(NERC,YEAR) = temp5/DENOM
            PECTRNR(NERC,YEAR) = temp/DENOM

            do i = 1, MNEUGRP
               PELOUTN(NERC,YEAR,i) = tempreg(i,NERC)/DENOM
            enddo

            write(imsg,102) CURIYR,CURITR,NERC,DENOM,(tempreg(i,NERC)/DENOM,i=1,MNEUGRP)
  102       format('nerc prc real$',3i4,f9.5,<MNEUGRP>f7.2)

            PECASNR(NERC,YEAR) = (PECRSNR(NERC,YEAR)*SALCLS(NERC,1) &
             + PECCMNR(NERC,YEAR)*SALCLS(NERC,2) &
             + PECINNR(NERC,YEAR)*SALCLS(NERC,3) &
             + PECTRNR(NERC,YEAR)*SALCLS(NERC,4))/ &
                 SALTOTA(NERC)
            PECASRLN(NERC,YEAR) = (PECRSRLN(NERC,YEAR)*SALCLS(NERC,1) &
             + PECCMRLN(NERC,YEAR)*SALCLS(NERC,2) &
             + PECINRLN(NERC,YEAR)*SALCLS(NERC,3) &
             + PECTRRLN(NERC,YEAR)*SALCLS(NERC,4))/ &
                 SALTOTA(NERC)
            PECASMEN(NERC,YEAR) = (PECRSMEN(NERC,YEAR)*SALCLS(NERC,1) &
             + PECCMMEN(NERC,YEAR)*SALCLS(NERC,2) &
             + PECINMEN(NERC,YEAR)*SALCLS(NERC,3) &
             + PECTRMEN(NERC,YEAR)*SALCLS(NERC,4))/ &
                 SALTOTA(NERC)
            PECASFXN(NERC,YEAR) = (PECRSFXN(NERC,YEAR)*SALCLS(NERC,1) &
             + PECCMFXN(NERC,YEAR)*SALCLS(NERC,2) &
             + PECINFXN(NERC,YEAR)*SALCLS(NERC,3) &
             + PECTRFXN(NERC,YEAR)*SALCLS(NERC,4))/ &
                 SALTOTA(NERC)
            PECASTXN(NERC,YEAR) = (PECRSTXN(NERC,YEAR)*SALCLS(NERC,1) &
             + PECCMTXN(NERC,YEAR)*SALCLS(NERC,2) &
             + PECINTXN(NERC,YEAR)*SALCLS(NERC,3) &
             + PECTRTXN(NERC,YEAR)*SALCLS(NERC,4))/ &
                 SALTOTA(NERC)
            PECASTDN(NERC,YEAR) = (PECRSTDN(NERC,YEAR)*SALCLS(NERC,1) &
             + PECCMTDN(NERC,YEAR)*SALCLS(NERC,2) &
             + PECINTDN(NERC,YEAR)*SALCLS(NERC,3) &
             + PECTRTDN(NERC,YEAR)*SALCLS(NERC,4))/ &
                 SALTOTA(NERC)
            PECTRNN(NERC,YEAR) = EPRICE(5,2,NERC)/DENOM
            PECDISN(NERC,YEAR) = EPRICE(5,3,NERC)/DENOM
            PECGENN(NERC,YEAR) = PECASNR(NERC,YEAR) - &
               PECTRNN(NERC,YEAR) - PECDISN(NERC,YEAR)
!cc overwrite ftab variables with competitive prices
       PELRSNR(NERC,YEAR) = PECRSNR(NERC,YEAR)
       PELCMNR(NERC,YEAR) = PECCMNR(NERC,YEAR)
       PELINNR(NERC,YEAR) = PECINNR(NERC,YEAR)
       PELTRNR(NERC,YEAR) = PECTRNR(NERC,YEAR)
       PELASNR(NERC,YEAR) = PECASNR(NERC,YEAR)
!
       ULEPRS(NERC) = PECRSNR(NERC,YEAR)
       ULEPCM(NERC) = PECCMNR(NERC,YEAR)
       ULEPIN(NERC) = PECINNR(NERC,YEAR)
       ULEPTR(NERC) = PECTRNR(NERC,YEAR)
       ULEPAS(NERC) = PECASNR(NERC,YEAR)
!

      if ((fcrl .eq. 1) .and. (FRMARG(YEAR,NERC).gt. 0.0)) then
      write(imsg,103) YEAR,NERC,'RS',PECRSNR(NERC,YEAR), &
         PECRSRLN(NERC,YEAR), &
         PECRSMEN(NERC,YEAR),PECRSFXN(NERC,YEAR),PECRSTXN(NERC,YEAR), &
         SCSPRC(YEAR,NERC,3)/DENOM, &
         PECRSTDN(NERC,YEAR)
      write(imsg,103) YEAR,NERC,'CM',PECCMNR(NERC,YEAR), &
         PECCMRLN(NERC,YEAR), &
         PECCMMEN(NERC,YEAR),PECCMFXN(NERC,YEAR),PECCMTXN(NERC,YEAR), &
         SCSPRC(YEAR,NERC,3)/DENOM, &
         PECCMTDN(NERC,YEAR)
      write(imsg,103) YEAR,NERC,'IN',PECINNR(NERC,YEAR), &
         PECINRLN(NERC,YEAR), &
         PECINMEN(NERC,YEAR),PECINFXN(NERC,YEAR),PECINTXN(NERC,YEAR), &
         SCSPRC(YEAR,NERC,3)/DENOM, &
         PECINTDN(NERC,YEAR)
      write(imsg,103) YEAR,NERC,'TR',PECTRNR(NERC,YEAR), &
         PECTRRLN(NERC,YEAR), &
         PECTRMEN(NERC,YEAR),PECTRFXN(NERC,YEAR),PECTRTXN(NERC,YEAR), &
         SCSPRC(YEAR,NERC,3)/DENOM, &
         PECTRTDN(NERC,YEAR)
      write(imsg,103) YEAR,NERC,'AS',PECASNR(NERC,YEAR), &
         PECASRLN(NERC,YEAR), &
         PECASMEN(NERC,YEAR),PECASFXN(NERC,YEAR),PECASTXN(NERC,YEAR), &
         SCSPRC(YEAR,NERC,3)/DENOM, &
         PECASTDN(NERC,YEAR)
      endif  ! fcrl

103    format ('nerc avg real$',2i4,a4,7f7.2)
!      if (fcrl .eq. 1) then
!            write(imsg,104) YEAR,NERC,PECGENN(NERC,YEAR),PECTRNN(NERC,YEAR),&
!                             PECDISN(NERC,YEAR), PECASNR(NERC,YEAR)
!      endif
104    format ('segment price ',2i4,4f7.2)

         ELSE      ! NERC GE UNRGN
            PELRSNR(NERC,YEAR) = 0.0
            PELCMNR(NERC,YEAR) = 0.0
            PELTRNR(NERC,YEAR) = 0.0
            PELINNR(NERC,YEAR) = 0.0
            PELASNR(NERC,YEAR) = 0.0
            PELCPNR(NERC,YEAR) = 0.0
            PELFLNR(NERC,YEAR) = 0.0
            PELOMNR(NERC,YEAR) = 0.0
            PELTLNR(NERC,YEAR) = 0.0
            PELWHNR(NERC,YEAR) = 0.0

            do i = 1,MNEUGRP
               PELOUTN(NERC,YEAR,i) = 0.0
            enddo

          IF (NERC .eq. MNUMNR) then     !fill national variables
            DEN1 = 0.0
            DEN2 = 0.0
            DEN3 = 0.0
            DEN4 = 0.0
            DEN5 = 0.0
           do comp = 1,10
            NUMER1(comp) = 0.0
            NUMER2(comp) = 0.0
            NUMER3(comp) = 0.0
            NUMER4(comp) = 0.0
            NUMER5(comp) = 0.0
           enddo
            DO I=1,UNRGNS
               TOT = 0.0
               DO J=1,4
                  TOT = TOT + SALCLS(I,J)
               END DO
               NUMER1(1) = NUMER1(1) + (SALCLS(I,1)*PECRSRLN(I,CURIYR))
               NUMER1(2) = NUMER1(2) + (SALCLS(I,1)*PECRSMEN(I,CURIYR))
               NUMER1(3) = NUMER1(3) + (SALCLS(I,1)*PECRSFXN(I,CURIYR))
               NUMER1(4) = NUMER1(4) + (SALCLS(I,1)*PECRSTXN(I,CURIYR))
               NUMER1(5) = NUMER1(5) + (SALCLS(I,1)*PECRSTDN(I,CURIYR))
               NUMER1(6) = NUMER1(6) + (SALCLS(I,1)*PECRSNR(I,CURIYR))
               DEN1 = DEN1 + SALCLS(I,1)

               NUMER2(1) = NUMER2(1) + (SALCLS(I,2)*PECCMRLN(I,CURIYR))
               NUMER2(2) = NUMER2(2) + (SALCLS(I,2)*PECCMMEN(I,CURIYR))
               NUMER2(3) = NUMER2(3) + (SALCLS(I,2)*PECCMFXN(I,CURIYR))
               NUMER2(4) = NUMER2(4) + (SALCLS(I,2)*PECCMTXN(I,CURIYR))
               NUMER2(5) = NUMER2(5) + (SALCLS(I,2)*PECCMTDN(I,CURIYR))
               NUMER2(6) = NUMER2(6) + (SALCLS(I,2)*PECCMNR(I,CURIYR))
               DEN2 = DEN2 + SALCLS(I,2)

               NUMER3(1) = NUMER3(1) + (SALCLS(I,3)*PECINRLN(I,CURIYR))
               NUMER3(2) = NUMER3(2) + (SALCLS(I,3)*PECINMEN(I,CURIYR))
               NUMER3(3) = NUMER3(3) + (SALCLS(I,3)*PECINFXN(I,CURIYR))
               NUMER3(4) = NUMER3(4) + (SALCLS(I,3)*PECINTXN(I,CURIYR))
               NUMER3(5) = NUMER3(5) + (SALCLS(I,3)*PECINTDN(I,CURIYR))
               NUMER3(6) = NUMER3(6) + (SALCLS(I,3)*PECINNR(I,CURIYR))
               DEN3 = DEN3 + SALCLS(I,3)

               NUMER4(1) = NUMER4(1) + (SALCLS(I,4)*PECTRRLN(I,CURIYR))
               NUMER4(2) = NUMER4(2) + (SALCLS(I,4)*PECTRMEN(I,CURIYR))
               NUMER4(3) = NUMER4(3) + (SALCLS(I,4)*PECTRFXN(I,CURIYR))
               NUMER4(4) = NUMER4(4) + (SALCLS(I,4)*PECTRTXN(I,CURIYR))
               NUMER4(5) = NUMER4(5) + (SALCLS(I,4)*PECTRTDN(I,CURIYR))
               NUMER4(6) = NUMER4(6) + (SALCLS(I,4)*PECTRNR(I,CURIYR))
               DEN4 = DEN4 + SALCLS(I,4)

               NUMER5(1) = NUMER5(1) + TOT*PECASRLN(I,CURIYR)
               NUMER5(2) = NUMER5(2) + TOT*PECASMEN(I,CURIYR)
               NUMER5(3) = NUMER5(3) + TOT*PECASFXN(I,CURIYR)
               NUMER5(4) = NUMER5(4) + TOT*PECASTXN(I,CURIYR)
               NUMER5(5) = NUMER5(5) + TOT*PECASTDN(I,CURIYR)
               NUMER5(6) = NUMER5(6) + TOT*PECASNR(I,CURIYR)
               NUMER5(7) = NUMER5(7) + TOT*SCSPRC(CURIYR,I,3)
               NUMER5(8) = NUMER5(8) + TOT*PECGENN(I,CURIYR)
               NUMER5(9) = NUMER5(9) + TOT*PECTRNN(I,CURIYR)
               NUMER5(10) = NUMER5(10) + TOT*PECDISN(I,CURIYR)
               DEN5 = DEN5 + TOT
            END DO

            SCSPRC(CURIYR,MNUMNR,3) = NUMER5(7) / DEN5

            IF (DEN1 .GT. 0.0) THEN
               PECRSRLN(MNUMNR,CURIYR) = NUMER1(1) / DEN1
               PECRSMEN(MNUMNR,CURIYR) = NUMER1(2) / DEN1
               PECRSFXN(MNUMNR,CURIYR) = NUMER1(3) / DEN1
               PECRSTXN(MNUMNR,CURIYR) = NUMER1(4) / DEN1
               PECRSTDN(MNUMNR,CURIYR) = NUMER1(5) / DEN1
               PECRSNR(MNUMNR,CURIYR) = NUMER1(6) / DEN1
            ELSE
               PECRSRLN(MNUMNR,CURIYR) = 0.0
               PECRSMEN(MNUMNR,CURIYR) = 0.0
               PECRSFXN(MNUMNR,CURIYR) = 0.0
               PECRSTXN(MNUMNR,CURIYR) = 0.0
               PECRSTDN(MNUMNR,CURIYR) = 0.0
               PECRSNR(MNUMNR,CURIYR) = 0.0
            END IF

            IF (DEN2 .GT. 0.0) THEN
               PECCMRLN(MNUMNR,CURIYR) = NUMER2(1) / DEN2
               PECCMMEN(MNUMNR,CURIYR) = NUMER2(2) / DEN2
               PECCMFXN(MNUMNR,CURIYR) = NUMER2(3) / DEN2
               PECCMTXN(MNUMNR,CURIYR) = NUMER2(4) / DEN2
               PECCMTDN(MNUMNR,CURIYR) = NUMER2(5) / DEN2
               PECCMNR(MNUMNR,CURIYR) = NUMER2(6) / DEN2
            ELSE
               PECCMRLN(MNUMNR,CURIYR) = 0.0
               PECCMMEN(MNUMNR,CURIYR) = 0.0
               PECCMFXN(MNUMNR,CURIYR) = 0.0
               PECCMTXN(MNUMNR,CURIYR) = 0.0
               PECCMTDN(MNUMNR,CURIYR) = 0.0
               PECCMNR(MNUMNR,CURIYR) = 0.0
            END IF

            IF (DEN3 .GT. 0.0) THEN
               PECINRLN(MNUMNR,CURIYR) = NUMER3(1) / DEN3
               PECINMEN(MNUMNR,CURIYR) = NUMER3(2) / DEN3
               PECINFXN(MNUMNR,CURIYR) = NUMER3(3) / DEN3
               PECINTXN(MNUMNR,CURIYR) = NUMER3(4) / DEN3
               PECINTDN(MNUMNR,CURIYR) = NUMER3(5) / DEN3
               PECINNR(MNUMNR,CURIYR) = NUMER3(6) / DEN3
            ELSE
               PECINRLN(MNUMNR,CURIYR) = 0.0
               PECINMEN(MNUMNR,CURIYR) = 0.0
               PECINFXN(MNUMNR,CURIYR) = 0.0
               PECINTXN(MNUMNR,CURIYR) = 0.0
               PECINTDN(MNUMNR,CURIYR) = 0.0
               PECINNR(MNUMNR,CURIYR) = 0.0
            END IF

            IF (DEN4 .GT. 0.0) THEN
               PECTRRLN(MNUMNR,CURIYR) = NUMER4(1) / DEN4
               PECTRMEN(MNUMNR,CURIYR) = NUMER4(2) / DEN4
               PECTRFXN(MNUMNR,CURIYR) = NUMER4(3) / DEN4
               PECTRTXN(MNUMNR,CURIYR) = NUMER4(4) / DEN4
               PECTRTDN(MNUMNR,CURIYR) = NUMER4(5) / DEN4
               PECTRNR(MNUMNR,CURIYR) = NUMER4(6) / DEN4
            ELSE
               PECTRRLN(MNUMNR,CURIYR) = 0.0
               PECTRMEN(MNUMNR,CURIYR) = 0.0
               PECTRFXN(MNUMNR,CURIYR) = 0.0
               PECTRTXN(MNUMNR,CURIYR) = 0.0
               PECTRTDN(MNUMNR,CURIYR) = 0.0
               PECTRNR(MNUMNR,CURIYR) = 0.0
            END IF

            PECASRLN(MNUMNR,CURIYR) = NUMER5(1) / DEN5
            PECASMEN(MNUMNR,CURIYR) = NUMER5(2) / DEN5
            PECASFXN(MNUMNR,CURIYR) = NUMER5(3) / DEN5
            PECASTXN(MNUMNR,CURIYR) = NUMER5(4) / DEN5
            PECASTDN(MNUMNR,CURIYR) = NUMER5(5) / DEN5
            PECASNR(MNUMNR,CURIYR) = NUMER5(6) / DEN5
            PECGENN(MNUMNR,CURIYR) = NUMER5(8) / DEN5
            PECTRNN(MNUMNR,CURIYR) = NUMER5(9) / DEN5
            PECDISN(MNUMNR,CURIYR) = NUMER5(10) / DEN5

      if ((fcrl .eq. 1).and.(USW_POL .eq. 4).and.(YEAR .gt. 8)) then
      write(imsg,103) YEAR,NERC,'RS',PECRSNR(NERC,YEAR), &
         PECRSRLN(NERC,YEAR), &
         PECRSMEN(NERC,YEAR),PECRSFXN(NERC,YEAR),PECRSTXN(NERC,YEAR), &
         SCSPRC(YEAR,NERC,3)/DENOM, &
         PECRSTDN(NERC,YEAR)
      write(imsg,103) YEAR,NERC,'CM',PECCMNR(NERC,YEAR), &
         PECCMRLN(NERC,YEAR), &
         PECCMMEN(NERC,YEAR),PECCMFXN(NERC,YEAR),PECCMTXN(NERC,YEAR), &
         SCSPRC(YEAR,NERC,3)/DENOM, &
         PECCMTDN(NERC,YEAR)
      write(imsg,103) YEAR,NERC,'IN',PECINNR(NERC,YEAR), &
         PECINRLN(NERC,YEAR), &
         PECINMEN(NERC,YEAR),PECINFXN(NERC,YEAR),PECINTXN(NERC,YEAR), &
         SCSPRC(YEAR,NERC,3)/DENOM, &
         PECINTDN(NERC,YEAR)
      write(imsg,103) YEAR,NERC,'TR',PECTRNR(NERC,YEAR), &
         PECTRRLN(NERC,YEAR), &
         PECTRMEN(NERC,YEAR),PECTRFXN(NERC,YEAR),PECTRTXN(NERC,YEAR), &
         SCSPRC(YEAR,NERC,3)/DENOM, &
         PECTRTDN(NERC,YEAR)
      write(imsg,103) YEAR,NERC,'AS',PECASNR(NERC,YEAR), &
         PECASRLN(NERC,YEAR), &
         PECASMEN(NERC,YEAR),PECASFXN(NERC,YEAR),PECASTXN(NERC,YEAR), &
         SCSPRC(YEAR,NERC,3)/DENOM, &
         PECASTDN(NERC,YEAR)
      endif  ! fcrl
!     if (fcrl .eq. 1) then
!       write(imsg,104) YEAR,NERC,PECGENN(NERC,YEAR),PECTRNN(NERC,YEAR),&
!                        PECDISN(NERC,YEAR), PECASNR(NERC,YEAR)
!     endif

        ENDIF   ! if mnumnr
       ENDIF ! NERC GE UNRGN
      END DO ! NERC ?

!  INITIALIZE CENSUS REGION PRICE VARIABLES
      DO CENSUS=1,MNUMCR
         PELBS(CENSUS,CURIYR) = 0.0
         PELWH(CENSUS,CURIYR) = 0.0
         PELAV(CENSUS,CURIYR) = 0.0
         PELRS(CENSUS,CURIYR) = 0.0
         PELCM(CENSUS,CURIYR) = 0.0
         PELTR(CENSUS,CURIYR) = 0.0
         PELIN(CENSUS,CURIYR) = 0.0
         PELAS(CENSUS,CURIYR) = 0.0
         PELCP(CENSUS,CURIYR) = 0.0
         PELFL(CENSUS,CURIYR) = 0.0
         PELOM(CENSUS,CURIYR) = 0.0
         PELTL(CENSUS,CURIYR) = 0.0

         PELSHRS(CENSUS,CURIYR) = 0.0;PELCLRS(CENSUS,CURIYR) = 0.0
         PELWHRS(CENSUS,CURIYR) = 0.0;PELCKRS(CENSUS,CURIYR) = 0.0
         PELCDRS(CENSUS,CURIYR) = 0.0;PELRFRS(CENSUS,CURIYR) = 0.0
         PELFZRS(CENSUS,CURIYR) = 0.0;PELLTRS(CENSUS,CURIYR) = 0.0
         PELH2RS(CENSUS,CURIYR) = 0.0;PELOTRS(CENSUS,CURIYR) = 0.0
         PELSHCM(CENSUS,CURIYR) = 0.0;PELSCCM(CENSUS,CURIYR) = 0.0
         PELWHCM(CENSUS,CURIYR) = 0.0;PELVTCM(CENSUS,CURIYR) = 0.0
         PELCKCM(CENSUS,CURIYR) = 0.0;PELLTCM(CENSUS,CURIYR) = 0.0
         PELRFCM(CENSUS,CURIYR) = 0.0;PELOPCM(CENSUS,CURIYR) = 0.0
         PELONCM(CENSUS,CURIYR) = 0.0;PELOTCM(CENSUS,CURIYR) = 0.0
         PELINP(CENSUS,CURIYR) = 0.0;PELINS(CENSUS,CURIYR) = 0.0
         PELINM(CENSUS,CURIYR) = 0.0
         PELLTTR(CENSUS,CURIYR) = 0.0;PELVHTR(CENSUS,CURIYR) = 0.0
         PECRS(CENSUS,CURIYR) = 0.0
         PECCM(CENSUS,CURIYR) = 0.0
         PECIN(CENSUS,CURIYR) = 0.0
         PECTR(CENSUS,CURIYR) = 0.0
         PECAS(CENSUS,CURIYR) = 0.0
         PELME(CENSUS,CURIYR) = 0.0
      END DO

!     COMPUTE CENSUS REGION PRICES FROM NERC REGION PRICES

      DO NERC=1,(MNUMNR-2)
         IF (NERC .LE. UNRGNS) THEN
            DO CENSUS=1,MNUMCR-1
               PELBS(CENSUS,CURIYR) = PELBS(CENSUS,CURIYR) + EWSPRCN(NERC,CURIYR) / 3.412 * mapcnavg(NERC,CENSUS)
               PELWH(CENSUS,CURIYR) = PELWH(CENSUS,CURIYR) + PELWHNR(NERC,CURIYR) / 3.412 * mapcnavg(NERC,CENSUS)
               PELAV(CENSUS,CURIYR) = PELAV(CENSUS,CURIYR) + PELASNR(NERC,CURIYR) / 3.412 * mapcnavg(NERC,CENSUS)
               PELRS(CENSUS,CURIYR) = PELRS(CENSUS,CURIYR) + PELRSNR(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(RES))
               PELCM(CENSUS,CURIYR) = PELCM(CENSUS,CURIYR) + PELCMNR(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(COM))
               PELTR(CENSUS,CURIYR) = PELTR(CENSUS,CURIYR) + PELTRNR(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(TRA))
               PELIN(CENSUS,CURIYR) = PELIN(CENSUS,CURIYR) + PELINNR(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(IND))
               PELAS(CENSUS,CURIYR) = PELAS(CENSUS,CURIYR) + PELASNR(NERC,CURIYR) / 3.412 * mapcnavg(NERC,CENSUS)
               PELCP(CENSUS,CURIYR) = PELCP(CENSUS,CURIYR) + PELCPNR(NERC,CURIYR) / 3.412 * mapcnavg(NERC,CENSUS)
               PELFL(CENSUS,CURIYR) = PELFL(CENSUS,CURIYR) + PELFLNR(NERC,CURIYR) / 3.412 * mapcnavg(NERC,CENSUS)
               PELOM(CENSUS,CURIYR) = PELOM(CENSUS,CURIYR) + PELOMNR(NERC,CURIYR) / 3.412 * mapcnavg(NERC,CENSUS)
               PELTL(CENSUS,CURIYR) = PELTL(CENSUS,CURIYR) + PELTLNR(NERC,CURIYR) / 3.412 * mapcnavg(NERC,CENSUS)

 !             write(IMSG,3335) 'rvmapp3',NERC,census,SEC(RES),SEC(COM),SEC(IND),SEC(TRA),RVMappCtoN(NERC,CENSUS,SEC(RES)),RVMappCtoN(NERC,CENSUS,SEC(COM)),RVMappCtoN(NERC,CENSUS,SEC(IND)),RVMappCtoN(NERC,CENSUS,SEC(TRA))


3335  format(1x,a15,6(i5,1x),10F13.5)

!              fill in all end use specific prices

!              residential:

               PELSHRS(CENSUS,CURIYR) = PELSHRS(CENSUS,CURIYR) + PELSHRSN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(RES))
               IF (PELSHRS(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELSHRS(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_SH_RS"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(RES), PELSHRS(CENSUS,CURIYR), PELSHRSN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(RES))
 3130             FORMAT(1X,"BAD_EL_PRICES",3(":",I4),":",A10,3(":",I2),3(":",F15.3))
                  PELSHRS(CENSUS,CURIYR) = 20.0
               END IF

               PELCLRS(CENSUS,CURIYR) = PELCLRS(CENSUS,CURIYR) + PELCLRSN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(RES))
               IF (PELCLRS(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELCLRS(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_CL_RS"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(RES), PELCLRS(CENSUS,CURIYR), PELCLRSN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(RES))
                  PELCLRS(CENSUS,CURIYR) = 20.0
               END IF

               PELWHRS(CENSUS,CURIYR) = PELWHRS(CENSUS,CURIYR) + PELWHRSN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(RES))
               IF (PELWHRS(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELWHRS(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_WH_RS"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(RES), PELWHRS(CENSUS,CURIYR), PELWHRSN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(RES))
                  PELWHRS(CENSUS,CURIYR) = 20.0
               END IF

               PELCKRS(CENSUS,CURIYR) = PELCKRS(CENSUS,CURIYR) + PELCKRSN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(RES))
               IF (PELCKRS(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELCKRS(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_CK_RS"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(RES), PELCKRS(CENSUS,CURIYR), PELCKRSN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(RES))
                  PELCKRS(CENSUS,CURIYR) = 20.0
               END IF

               PELCDRS(CENSUS,CURIYR) = PELCDRS(CENSUS,CURIYR) + PELCDRSN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(RES))
               IF (PELCDRS(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELCDRS(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_CD_RS"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(RES), PELCDRS(CENSUS,CURIYR), PELCDRSN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(RES))
                  PELCDRS(CENSUS,CURIYR) = 20.0
               END IF

               PELRFRS(CENSUS,CURIYR) = PELRFRS(CENSUS,CURIYR) + PELRFRSN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(RES))
               IF (PELRFRS(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELRFRS(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_RF_RS"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(RES), PELRFRS(CENSUS,CURIYR), PELRFRSN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(RES))
                  PELRFRS(CENSUS,CURIYR) = 20.0
               END IF

               PELFZRS(CENSUS,CURIYR) = PELFZRS(CENSUS,CURIYR) + PELFZRSN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(RES))
               IF (PELFZRS(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELFZRS(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_FZ_RS"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(RES), PELFZRS(CENSUS,CURIYR), PELFZRSN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(RES))
                  PELFZRS(CENSUS,CURIYR) = 20.0
               END IF

               PELLTRS(CENSUS,CURIYR) = PELLTRS(CENSUS,CURIYR) + PELLTRSN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(RES))
               IF (PELLTRS(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELLTRS(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_LT_RS"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(RES), PELLTRS(CENSUS,CURIYR), PELLTRSN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(RES))
                  PELLTRS(CENSUS,CURIYR) = 20.0
               END IF

               PELH2RS(CENSUS,CURIYR) = PELH2RS(CENSUS,CURIYR) + PELH2RSN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(RES))
               IF (PELH2RS(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELH2RS(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_H2_RS"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(RES), PELH2RS(CENSUS,CURIYR), PELH2RSN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(RES))
                  PELH2RS(CENSUS,CURIYR) = 20.0
               END IF

               PELOTRS(CENSUS,CURIYR) = PELOTRS(CENSUS,CURIYR) + PELOTRSN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(RES))
               IF (PELOTRS(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELOTRS(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_OT_RS"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(RES), PELOTRS(CENSUS,CURIYR), PELOTRSN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(RES))
                  PELOTRS(CENSUS,CURIYR) = 20.0
               END IF

!              commercial end uses

               PELSHCM(CENSUS,CURIYR) = PELSHCM(CENSUS,CURIYR) + PELSHCMN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(COM))
               IF (PELSHCM(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELSHCM(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_SH_CM"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(COM), PELSHCM(CENSUS,CURIYR), PELSHCMN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(COM))
                  PELSHCM(CENSUS,CURIYR) = 20.0
               END IF

               PELSCCM(CENSUS,CURIYR) = PELSCCM(CENSUS,CURIYR) + PELSCCMN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(COM))
               IF (PELSCCM(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELSCCM(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_SC_CM"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(COM), PELSCCM(CENSUS,CURIYR), PELSCCMN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(COM))
                  PELSCCM(CENSUS,CURIYR) = 20.0
               END IF

               PELWHCM(CENSUS,CURIYR) = PELWHCM(CENSUS,CURIYR) + PELWHCMN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(COM))
               IF (PELWHCM(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELWHCM(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_WH_CM"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(COM), PELWHCM(CENSUS,CURIYR), PELWHCMN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(COM))
                  PELWHCM(CENSUS,CURIYR) = 20.0
               END IF

               PELVTCM(CENSUS,CURIYR) = PELVTCM(CENSUS,CURIYR) + PELVTCMN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(COM))
               IF (PELVTCM(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELVTCM(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_VT_CM"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(COM), PELVTCM(CENSUS,CURIYR), PELVTCMN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(COM))
                  PELVTCM(CENSUS,CURIYR) = 20.0
               END IF

               PELCKCM(CENSUS,CURIYR) = PELCKCM(CENSUS,CURIYR) + PELCKCMN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(COM))
               IF (PELCKCM(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELCKCM(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_CK_CM"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(COM), PELCKCM(CENSUS,CURIYR), PELCKCMN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(COM))
                  PELCKCM(CENSUS,CURIYR) = 20.0
               END IF

               PELLTCM(CENSUS,CURIYR) = PELLTCM(CENSUS,CURIYR) + PELLTCMN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(COM))
               IF (PELLTCM(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELLTCM(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_LT_CM"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(COM), PELLTCM(CENSUS,CURIYR), PELLTCMN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(COM))
                  PELLTCM(CENSUS,CURIYR) = 20.0
               END IF

               PELRFCM(CENSUS,CURIYR) = PELRFCM(CENSUS,CURIYR) + PELRFCMN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(COM))
               IF (PELRFCM(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELRFCM(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_RF_CM"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(COM), PELRFCM(CENSUS,CURIYR), PELRFCMN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(COM))
                  PELRFCM(CENSUS,CURIYR) = 20.0
               END IF

               PELOPCM(CENSUS,CURIYR) = PELOPCM(CENSUS,CURIYR) + PELOPCMN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(COM))
               IF (PELOPCM(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELOPCM(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_OP_CM"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(COM), PELOPCM(CENSUS,CURIYR), PELOPCMN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(COM))
                  PELOPCM(CENSUS,CURIYR) = 20.0
               END IF

               PELONCM(CENSUS,CURIYR) = PELONCM(CENSUS,CURIYR) + PELONCMN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(COM))
               IF (PELONCM(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELONCM(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_ON_CM"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(COM), PELONCM(CENSUS,CURIYR), PELONCMN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(COM))
                  PELONCM(CENSUS,CURIYR) = 20.0
               END IF

               PELOTCM(CENSUS,CURIYR) = PELOTCM(CENSUS,CURIYR) + PELOTCMN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(COM))
               IF (PELOTCM(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELOTCM(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_OT_CM"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(COM), PELOTCM(CENSUS,CURIYR), PELOTCMN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(COM))
                  PELOTCM(CENSUS,CURIYR) = 20.0
               END IF

!              industrial

               PELINP(CENSUS,CURIYR) = PELINP(CENSUS,CURIYR) + PELINPN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(IND))
               IF (PELINP(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELINP(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_P__IN"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(IND), PELINP(CENSUS,CURIYR), PELINPN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(IND))
                  PELINP(CENSUS,CURIYR) = 20.0
               END IF

               PELINS(CENSUS,CURIYR) = PELINS(CENSUS,CURIYR) + PELINSN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(IND))
               IF (PELINS(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELINS(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_S__IN"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(IND), PELINS(CENSUS,CURIYR), PELINSN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(IND))
                  PELINS(CENSUS,CURIYR) = 20.0
               END IF

               PELINM(CENSUS,CURIYR) = PELINM(CENSUS,CURIYR) + PELINMN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(IND))
               IF (PELINM(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELINM(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_M__IN"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(IND), PELINM(CENSUS,CURIYR), PELINMN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(IND))
                  PELINM(CENSUS,CURIYR) = 20.0
               END IF

!              transportation

               PELLTTR(CENSUS,CURIYR) = PELLTTR(CENSUS,CURIYR) + PELLTTRN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(TRA))
               IF (PELLTTR(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELLTTR(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_LT_TR"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(TRA), PELLTTR(CENSUS,CURIYR), PELLTTRN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(TRA))
                  PELLTTR(CENSUS,CURIYR) = 20.0
               END IF

               PELVHTR(CENSUS,CURIYR) = PELVHTR(CENSUS,CURIYR) + PELVHTRN(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(TRA))
               IF (PELVHTR(CENSUS,CURIYR) .LT. 0.0 .OR. ISNAN(PELVHTR(CENSUS,CURIYR)) ) THEN   ! check for NaNQ this way
                  NM = "P_EL_VH_TR"
                  WRITE(6,3130) CURIRUN, CURIYR+1989, CURITR, NM, NERC, CENSUS, SEC(TRA), PELVHTR(CENSUS,CURIYR), PELVHTRN(NERC,CURIYR), &
                     RVMappCtoN(NERC,CENSUS,SEC(TRA))
                  PELVHTR(CENSUS,CURIYR) = 20.0
               END IF

!              sector average competitive prices

               PECRS(CENSUS,CURIYR) = PECRS(CENSUS,CURIYR) + PECRSNR(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(RES))
               PECCM(CENSUS,CURIYR) = PECCM(CENSUS,CURIYR) + PECCMNR(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(COM))
               PECIN(CENSUS,CURIYR) = PECIN(CENSUS,CURIYR) + PECINNR(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(IND))
               PECTR(CENSUS,CURIYR) = PECTR(CENSUS,CURIYR) + PECTRNR(NERC,CURIYR) / 3.412 * RVMappCtoN(NERC,CENSUS,SEC(TRA))
               PECAS(CENSUS,CURIYR) = PECAS(CENSUS,CURIYR) + PECASNR(NERC,CURIYR) / 3.412 * mapcnavg(NERC,CENSUS)
               PELME(CENSUS,CURIYR) = PELME(CENSUS,CURIYR) + PECASMEN(NERC,CURIYR) / 3.412 * mapcnavg(NERC,CENSUS)

!              overwrite purchased electricity prices (PEL__) with competitive prices

               PELRS(CENSUS,CURIYR) = PECRS(CENSUS,CURIYR)
               PELCM(CENSUS,CURIYR) = PECCM(CENSUS,CURIYR)
               PELIN(CENSUS,CURIYR) = PECIN(CENSUS,CURIYR)
               PELTR(CENSUS,CURIYR) = PECTR(CENSUS,CURIYR)
               PELAS(CENSUS,CURIYR) = PECAS(CENSUS,CURIYR)

            END DO
         ELSE                ! fill in US total values
            NUM1 = 0.0
            DEN1 = 0.0
            NUM2 = 0.0
            DEN2 = 0.0
            NUM3 = 0.0
            DEN3 = 0.0
            NUM4 = 0.0
            DEN4 = 0.0
            NUM5 = 0.0
            DEN5 = 0.0
            DO I=1,UNRGNS
               TOT = 0.0
               DO J=1,4
                  TOT = TOT + SALCLS(I,J)
               END DO
               NUM1 = NUM1 + (SALCLS(I,1) * PELRSNR(I,CURIYR))
               DEN1 = DEN1 + SALCLS(I,1)
               NUM2 = NUM2 + (SALCLS(I,2) * PELCMNR(I,CURIYR))
               DEN2 = DEN2 + SALCLS(I,2)
               NUM3 = NUM3 + (SALCLS(I,3) * PELINNR(I,CURIYR))
               DEN3 = DEN3 + SALCLS(I,3)
               NUM4 = NUM4 + (SALCLS(I,4) * PELTRNR(I,CURIYR))
               DEN4 = DEN4 + SALCLS(I,4)
               NUM5 = NUM5 + (TOT * PELASNR(I,CURIYR))
               DEN5 = DEN5 + TOT
            END DO
!
            ULEPRS(MNUMNR) = NUM1 / DEN1
            ULEPCM(MNUMNR) = NUM2 / DEN2
            ULEPIN(MNUMNR) = NUM3 / DEN3
            ULEPTR(MNUMNR) = NUM4 / DEN4
            ULEPAS(MNUMNR) = NUM5 / DEN5
!
            PELRS(MNUMCR,CURIYR) = NUM1/(3.412*DEN1)
            PELCM(MNUMCR,CURIYR) = NUM2/(3.412*DEN2)
            PELIN(MNUMCR,CURIYR) = NUM3/(3.412*DEN3)
            PELTR(MNUMCR,CURIYR) = NUM4/(3.412*DEN4)
            PELAS(MNUMCR,CURIYR) = NUM5/(3.412*DEN5)
            PELAV(MNUMCR,CURIYR) = PELAS(MNUMCR,CURIYR)
            NUM0 = 0.0
            NUM1 = 0.0
            DEN1 = 0.0
            NUM2 = 0.0
            NUM3 = 0.0
            NUM4 = 0.0
            NUM5 = 0.0
            NUM6 = 0.0
            DO I=1,UNRGNS
               TOT = 0.0
               DO J=1,4
                  TOT = TOT + SALCLS(I,J)
               END DO
               NUM0 = NUM0 + (TOT*EWSPRCN(I,CURIYR))
               NUM1 = NUM1 + (TOT*PELCPNR(I,CURIYR))
               DEN1 = DEN1 + TOT
               NUM2 = NUM2 + (TOT*PELFLNR(I,CURIYR))
               NUM3 = NUM3 + (TOT*PELOMNR(I,CURIYR))
               NUM5 = NUM5 + (TOT*PELWHNR(I,CURIYR))
               NUM4 = NUM4 + (TOT*PELTLNR(I,CURIYR))
               NUM6 = NUM6 + (TOT*PECASMEN(I,CURIYR))
            END DO
            PELBS(MNUMCR,CURIYR) = NUM0/(3.412*DEN1)
            PELCP(MNUMCR,CURIYR) = NUM1/(3.412*DEN1)
            PELFL(MNUMCR,CURIYR) = NUM2/(3.412*DEN1)
            PELOM(MNUMCR,CURIYR) = NUM3/(3.412*DEN1)
            PELWH(MNUMCR,CURIYR) = NUM5/(3.412*DEN1)
            PELTL(MNUMCR,CURIYR) = NUM4/(3.412*DEN1)
            PELME(MNUMCR,CURIYR) = NUM6/(3.412*DEN1)
!
            ULEPCP(MNUMNR) = NUM1 / DEN1
            ULEPFL(MNUMNR) = NUM2 / DEN1
            ULEPOM(MNUMNR) = NUM3 / DEN1
            ULEPWP(MNUMNR) = NUM5 / DEN1
!
            do l = 1,MNEUGRP
             num(l) = 0.0; den(l) = 0.0
            enddo

            DO I=1,UNRGNS
              l=0
               do sectr=1,4
               do eu = 1, NEUSGRP(sectr)
                 l = l+1
                 num(l) = num(l) + tempreg(l,I)* &
                     demwtnr(l,I)*totallnr(sectr,I)/DENOM
              den(l) = den(l) + demwtnr(l,I)*totallnr(sectr,I)
               enddo
               enddo
            ENDDO
! fill in end use variables
            PELSHRS(MNUMCR,CURIYR) = num(1)/(3.412*den(1))
            PELCLRS(MNUMCR,CURIYR) = num(2)/(3.412*den(2))
            PELWHRS(MNUMCR,CURIYR) = num(3)/(3.412*den(3))
            PELCKRS(MNUMCR,CURIYR) = num(4)/(3.412*den(4))
            PELCDRS(MNUMCR,CURIYR) = num(5)/(3.412*den(5))
            PELRFRS(MNUMCR,CURIYR) = num(6)/(3.412*den(6))
            PELFZRS(MNUMCR,CURIYR) = num(7)/(3.412*den(7))
            PELLTRS(MNUMCR,CURIYR) = num(8)/(3.412*den(8))
            PELOTRS(MNUMCR,CURIYR) = num(9)/(3.412*den(9))
            PELH2RS(MNUMCR,CURIYR) = num(10)/(3.412*den(10))
            PELSHCM(MNUMCR,CURIYR) = num(11)/(3.412*den(11))
            PELSCCM(MNUMCR,CURIYR) = num(12)/(3.412*den(12))
            PELWHCM(MNUMCR,CURIYR) = num(13)/(3.412*den(13))
            PELVTCM(MNUMCR,CURIYR) = num(14)/(3.412*den(14))
            PELCKCM(MNUMCR,CURIYR) = num(15)/(3.412*den(15))
            PELLTCM(MNUMCR,CURIYR) = num(16)/(3.412*den(16))
            PELRFCM(MNUMCR,CURIYR) = num(17)/(3.412*den(17))
            PELOPCM(MNUMCR,CURIYR) = num(18)/(3.412*den(18))
            PELONCM(MNUMCR,CURIYR) = num(19)/(3.412*den(19))
            PELOTCM(MNUMCR,CURIYR) = num(20)/(3.412*den(20))
            PELINP(MNUMCR,CURIYR) = num(21)/(3.412*den(21))
            PELINS(MNUMCR,CURIYR) = num(22)/(3.412*den(22))
            PELINM(MNUMCR,CURIYR) = num(23)/(3.412*den(23))
            PELVHTR(MNUMCR,CURIYR) = num(24)/(3.412*den(24))
            PELLTTR(MNUMCR,CURIYR) = num(25)/(3.412*den(25))

            NUM1 = 0.0;DEN1 = 0.0
            NUM2 = 0.0;DEN2 = 0.0
            NUM3 = 0.0;DEN3 = 0.0
            NUM4 = 0.0;DEN4 = 0.0
            NUM5 = 0.0;DEN5 = 0.0
            DO I=1,UNRGNS
               NUM1 = NUM1 + SALCLS(I,1) * PECRSNR(I,CURIYR)
               DEN1 = DEN1 + SALCLS(I,1)
               NUM2 = NUM2 + SALCLS(I,2) * PECCMNR(I,CURIYR)
               DEN2 = DEN2 + SALCLS(I,2)
               NUM3 = NUM3 + SALCLS(I,3) * PECINNR(I,CURIYR)
               DEN3 = DEN3 + SALCLS(I,3)
               NUM4 = NUM4 + SALCLS(I,4) * PECTRNR(I,CURIYR)
               DEN4 = DEN4 + SALCLS(I,4)
               NUM5 = NUM5 + SALTOTA(NERC) * PECASNR(I,CURIYR)
               DEN5 = DEN5 + SALTOTA(NERC)
            END DO
            PECRS(MNUMCR,CURIYR) = NUM1/(3.412*DEN1)
            PECCM(MNUMCR,CURIYR) = NUM2/(3.412*DEN2)
            PECIN(MNUMCR,CURIYR) = NUM3/(3.412*DEN3)
            PECTR(MNUMCR,CURIYR) = NUM4/(3.412*DEN4)
            PECAS(MNUMCR,CURIYR) = NUM5/(3.412*DEN5)

!  overwrite electricity prices with competitive prices
            PELRS(MNUMCR,CURIYR) = PECRS(MNUMCR,CURIYR)
            PELCM(MNUMCR,CURIYR) = PECCM(MNUMCR,CURIYR)
            PELTR(MNUMCR,CURIYR) = PECTR(MNUMCR,CURIYR)
            PELIN(MNUMCR,CURIYR) = PECIN(MNUMCR,CURIYR)
            PELAS(MNUMCR,CURIYR) = PECAS(MNUMCR,CURIYR)

         ENDIF
      END DO

      DO CENSUS = 1,11
        WRITE(imsg,18) 'CS RES',CURIYR,CURITR,CENSUS,PECRS(CENSUS,CURIYR),(PELRSOUT(CENSUS,CURIYR,EUG),EUG=1,10)
        WRITE(imsg,18) 'CS COM',CURIYR,CURITR,CENSUS,PECCM(CENSUS,CURIYR),(PELCMOUT(CENSUS,CURIYR,EUG),EUG=1,10)
      ENDDO
18    FORMAT(1x,a8,3I5,11F10.3)
!     write(6,17) 'cs res',(PECRS(CENSUS,CURIYR),CENSUS=1,11)
!     write(6,17) 'cs com',(PECCM(CENSUS,CURIYR),CENSUS=1,11)
!     write(6,17) 'cs ind',(PECIN(CENSUS,CURIYR),CENSUS=1,11)
!     write(6,17) 'cs tra',(PECTR(CENSUS,CURIYR),CENSUS=1,11)
!     write(6,17) 'cs tot',(PECAS(CENSUS,CURIYR),CENSUS=1,11)
17    format(a7,11f7.2)


!   get rid of steo benchmarking for now
        BYR1 = 1
        BYR2 = 1

      IF ((CURIYR .GT. 2) .AND. (CURIYR .LE. BYR1)) THEN
! SET YEAR COUNTER, I
        I=CURIYR-2
! ENTER RESIDENTIAL PRICES IN NOMINAL CENTS/KWH (RIGHT FROM STEO)
! FOR YEARS 1992 TO 1996 - FOR 1997, MAKE SOMETHING UP SO THE
! PRICE PATH WILL BE SMOOTHED - STEO IS 8.31 FOR 1997
        IF (CURIYR .LE. BYR2) THEN
          STEOPRC(1)=8.2
          STEOPRC(2)=8.3
          STEOPRC(3)=8.4
          STEOPRC(4)=8.41
          STEOPRC(5)=8.36
          STEOPRC(6)=8.5
! TRANSLATE STEO PRICES FROM NOMINAL CENTS/KWH TO 1987 DOLLARS PER
! MILLION BTU (CODE AT THIS POINT IS IN 1987 DOLLARS PER MILLION BTU)
          STEOPRC(I)=(STEOPRC(I)/MC_JPGDP(CURIYR))/.3412
! CALCULATE BENCHMARK FACTOR, BNCHFCTR, BASED ON NATIONAL PRICES -
          BNCHFCTR(I)=STEOPRC(I)/PELRS(MNUMCR,CURIYR)
        ENDIF
!  IF NOT A COMPETITION RUN THEN AFTER 1995 PHASE OUT BENCHMARKING
!  TO 2000 (YEAR 11). IF COMPETITION RUN BENCHMARKING ENDS IN 1994
!  WITH NO PHASE OUT.
!       IF ((USW_POL .EQ. 0) .AND. (CURIYR .GT. BYR2)) THEN
        IF  (CURIYR .GT. BYR2) THEN
         BNCHFCTR(I)=BNCHFCTR(6)-((BNCHFCTR(6)-1.0)*((CURIYR-8)/4.0))
        ENDIF
! ADJUST REGIONAL RESIDENTIAL PRICES BY NATIONAL BENCHMARK FACTOR
        DO J = 1,MNUMCR
          PELRS(J,CURIYR)=PELRS(J,CURIYR)*BNCHFCTR(I)
        END DO
! NOW GO BACK AND BENCHMARK THE NERC REGION RESIDENTIAL
! PRICES TO THE SAME FACTOR
        DO J = 1,MNUMNR
          PELRSNR(J,CURIYR)=PELRSNR(J,CURIYR)*BNCHFCTR(I)
        END DO
      ENDIF
      RETURN
      END

      SUBROUTINE REVGAP
      IMPLICIT NONE
!******************************************************************
!    THIS SUBROUTINE CALCULATES THE ANNUAL GAP BETWEEN COMPETITIVE REVENUES AND COSTS MINUS DEPRECIATION
!       The fraction of revenues to costs is saved for use in ecp
!*****************************************************************
      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'eusprc'
      include'efpint'
      include'efpout'
      include'efprp2'
      include'macout'
      include'dsmunits'
      include'uefdout'

      integer RGN,FULLYR
      real tsales,trevs,tcost,tgap

      FULLYR = USYEAR(CURIYR)

      DO RGN = 1, UNRGNS
      tsales = SALCLS(RGN,1) + SALCLS(RGN,2) + SALCLS(RGN,3) + &
               SALCLS(RGN,4)
      trevs = (PECASRLN(RGN,CURIYR) + PECASMEN(RGN,CURIYR) &
              +PECASFXN(RGN,CURIYR) + PECASTXN(RGN,CURIYR)) &
                  *MC_JPGDP(CURIYR)*tsales
      tcost = COMPCAP(RGN,CURIYR) + COMPEX(RGN,CURIYR)
      tgap = trevs/tcost
!
!
!
!     IF(FRMARG(CURIYR,RGN) .lt. 0.0001) tgap = 1.0
!
!    weighted average of 1 and tgap since early years
!    are screwed up
!
      tgap = (1.0 - FRMARG(CURIYR,RGN)) * 1.05 + &
        tgap *    FRMARG(CURIYR,RGN)

      IF (FULLYR .gt. UESTYR) THEN
!       DGAPOLD(RGN) = .667*DGAPOLD(RGN) + .333*tgap
        DGAPOLD(RGN) = .5*DGAPOLD(RGN) + .5*tgap
      ELSE
        DGAPOLD(RGN) = tgap
      ENDIF
      write(IMSG,*)' revgap: ',RGN,CURIYR,tsales,trevs,tcost &
        ,dgapold(RGN)
      ENDDO
      RETURN
      END
!
      SUBROUTINE EFP_REPORT(NRGN,INDOC)
      IMPLICIT NONE
!*****************************************************************
!      THIS SUBROUTINE CONTROLS WHICH OUTPUT REPORTS ARE PRINTED
!      A LARGE OR SMALL REPORT CAN BE PRINTED
!*****************************************************************
! INPUT VARIABLES:
!   NRGN = REGION TO BE PRINTED
!   NOPREG = NUMBER OF PRINT REGIONS
!   NAMES = REGION NAMES FOR REPORT TITLES
!   IPOWN = FLAGS INDICATING WHICH OWNERSHIP CLASSES TO PRINT
!   IPRPT = FLAGS INDICATING WHICH REPORTS (LARGE/SMALL) TO PRINT
!   EFPREALDOLLARS = FLAG INDICATING IF REPORTS ARE IN REAL OR NOMINAL DOLLARS
! INTERNAL VARIABLES:
!   INDOC = OPERATING COMPONENT INDEX
!   NATNAM = REPORT TITLE FOR NATIONAL RESULTS
!   NAME2A = REPORT TITLES INDICATING OWNERSHIP TYPE &
!            OPERATING COMPONENT
!   ITYPE = OWNERSHIP TYPE INDEX
! OUTPUT VARIABLES:
!   NAME1 = REGION TITLE FOR REPORT
!   NAME2 = OWNERSHIP CLASS & OPERATING COMPONENT FOR REPORT
!*****************************************************************
      include 'parametr'
      include 'emmparm'
      include 'control'
      include 'ncntrl'
      include 'eusprc'
      include 'efpcntrl'
      include 'efpname'
      include 'edbdef'
      INTEGER ITYPE
      INTEGER INDOC
      INTEGER NRGN
      CHARACTER*16 NATNAM
      CHARACTER*50 NAME2A(3,4)
      DATA NATNAM/'NATIONAL TOTALS '/
      DATA NAME2A/'PRIVATE SECTOR RESULTS - GENERATION ONLY', &
                  'PUBLIC SECTOR RESULTS - GENERATION ONLY', &
                  'PRIVATE & PUBLIC SECTORS - GENERATION ONLY', &
                  'PRIVATE SECTOR RESULTS - TRANSMISSION ONLY', &
                  'PUBLIC SECTOR RESULTS - TRANSMISSION ONLY', &
                  'PRIVATE & PUBLIC SECTORS - TRANSMISSION ONLY', &
                  'PRIVATE SECTOR RESULTS - DISTRIBUTION ONLY', &
                  'PUBLIC SECTOR RESULTS - DISTRIBUTION ONLY', &
                  'PRIVATE & PUBLIC SECTORS - DISTRIBUTION ONLY', &
                  'PRIVATE SECTOR RESULTS - TOTAL', &
                  'PUBLIC SECTOR RESULTS - TOTAL', &
                  'PRIVATE & PUBLIC SECTORS - TOTAL'/
      NAMER='                '
      IF (NRGN .EQ. (NOPREG+1)) THEN
         NAME1 = NATNAM
      ELSE
         NAME1 = NAMES(NRGN)
      ENDIF
      DO ITYPE=1,3
         IF ( (IPOWN(ITYPE) .NE. 0) .OR. ((ORCLEFP .EQ. 1) .AND. (FNRUN .EQ. 1)) ) THEN
            NAME2 = NAME2A(ITYPE,INDOC)
            IF (((IPRPT(1) .EQ. 1) .OR. (IPRPT(2) .EQ. 1)) .OR.    &
                    ((ORCLEFP .EQ. 1) .AND. (FNRUN .EQ. 1)) ) THEN
               CALL TMPSET(ITYPE,NRGN,INDOC)
               IF (IREAL .EQ. 1) CALL EFPREALDOLLARS(ITYPE)
            ENDIF
            IF (IPCOMP(INDOC) .NE. 0) THEN
              IF (IPRPT(1) .EQ. 1) CALL REPLAR(ITYPE)
              IF (IPRPT(2) .EQ. 1) CALL REPSML(ITYPE,NRGN,INDOC)
            ENDIF
!      Call routine to write EFPRPT variables to database
            IF ( (FCRL .EQ. 1) .AND. (CURIYR .EQ. LASTYR) .AND.          &
                (ORCLEFP .EQ. 1) .AND. (FNRUN .EQ. 1) ) CALL WREFPDB(ITYPE,NRGN,INDOC)
         ENDIF
      END DO
      RETURN
      END
!
      SUBROUTINE COMPREPORT(NRGN)
      IMPLICIT NONE
!*****************************************************************
!      THIS SUBROUTINE CONTROLS WHICH COMPETITIVE OUTPUT REPORTS ARE PRINTED
!      LARGE OR SMALL REPORT CAN BE PRINTED
!*****************************************************************
! INPUT VARIABLES:
!   NRGN = REGION TO BE PRINTED
!   NOPREG = NUMBER OF PRINT REGIONS
!   NAMES = REGION NAMES FOR REPORT TITLES
!   IPOWN = FLAGS INDICATING WHICH OWNERSHIP CLASSES TO PRINT
!   IPRPT = FLAGS INDICATING WHICH REPORTS (LARGE/SMALL) TO PRINT
!   EFPREALDOLLARS = FLAG INDICATING IF REPORTS ARE IN REAL OR NOMINAL DOLLARS
! INTERNAL VARIABLES:
!   INDOC = OPERATING COMPONENT INDEX
!   NATNAM = REPORT TITLE FOR NATIONAL RESULTS
!   NAME2A = REPORT TITLES INDICATING OWNERSHIP TYPE &
!            OPERATING COMPONENT
!   ITYPE = OWNERSHIP TYPE INDEX
! OUTPUT VARIABLES:
!   NAME1 = REGION TITLE FOR REPORT
!   NAME2 = OWNERSHIP CLASS & OPERATING COMPONENT FOR REPORT
!*****************************************************************
      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'efpcntrl'
      include 'control'
      include 'eusprc'
      include 'efpint'
      include 'efpout'
      include 'efpname'
      include 'efpgen'   ! unit number
      include 'macout'
      include 'efpwrt'
      INTEGER ITYPE
      INTEGER INDOC
      INTEGER NRGN
      INTEGER RGN
      INTEGER IYR
      INTEGER DOLYR
      REAL CMPX,CMPCP

      IF (NRGN .EQ. (NOPREG+1)) THEN
        RGN = MNUMNR
      ELSE
        RGN = NRGN   ! consistent with lc2
      ENDIF

      write(IOUT,5)

 5    FORMAT(5x,'YEAR',7x,'CAPITAL',8x,'EXPENSES',5x,'TOTAL COSTS', &
        4x,'COMP PRICE',8x,'t&d',8x,'REVENUE',5x,'SALES')
!
!    compex is in nominal dollars, compcap is in nominal dollars
!    and pecasnr is in 87 dollars
!
      DOLYR = IYRRL
      DO IYR = 1,UNYEAR

!    make compex now IYYR dollars
        CMPX = COMPEX(RGN,IYR) * MC_JPGDP(DOLYR) / MC_JPGDP(IYR)
        CMPCP = COMPCAP(RGN,IYR) * MC_JPGDP(DOLYR) / MC_JPGDP(IYR)
       write(IOUT,6)IYR,CMPCP,CMPX, &
       CMPX + CMPCP,PECASNR(RGN,IYR) &
       *MC_JPGDP(DOLYR),PECASTDN(RGN,IYR) * MC_JPGDP(DOLYR), &
        (PECASNR(RGN,IYR) - PECASTDN(RGN,IYR)) &
        * TSALES(IYR,1) *MC_JPGDP(DOLYR),TSALES(IYR,1)
       ENDDO
 6     FORMAT(I5,3f15.0,2f15.2,2f15.2)
!
!
!
      RETURN
      END
      SUBROUTINE TMPSET(IT,NRGN,IC)
      IMPLICIT NONE
!*****************************************************************
!      THIS SUBROUTINE SETS UP TEMPORARY ARRAYS OF REPORT DATA FOR THE GIVEN REGION AND TYPE
!*****************************************************************
! INPUT VARIABLES:
!   REVREQ = REVENUE REQUIREMENTS REPORT
!   TAXINC = TAX INCOME REPORT
!   BALSHT = BALANCE SHEET REPORT
!   FUNDS = SOURCES/USES OF FUNDS REPORT
!   XINCST = INCOME STATEMENT REPORT
!   CAPREQ = CAPITAL REQUIREMENTS REPORT
!   SALES = SALES REPORT
!   CSTCAP = COST OF CAPITAL REPORT
!   CANPLT = CANCELLED PLANT REPORT
!   IT = OWNERSHIP TYPE TO BE PRINTED
!   NRGN = REGION TO BE PRINTED
!   LASTYR = LAST YEAR OF REPORT
!   NOPREG = NUMBER OF PRINT REGIONS
!   FRACRG = FRACTIONS USED TO AGGREGATE/DISAGGREGATE INPUT
!            REGIONS TO PRINT REGIONS
! INTERNAL VARIABLES:
!   IC = OPERATING COMPONENT INDEX
! OUTPUT VARIABLES:
!   TREVRE = TEMPORARY REVENUE REQUIREMENTS REPORT
!   TTAXIN = TEMPORARY TAX INCOME REPORT
!   TBALSH = TEMPORARY BALANCE SHEET REPORT
!   TFUNDS = TEMPORARY SOURCES/USES OF FUNDS REPORT
!   TXINCS = TEMPORARY INCOME STATEMENT REPORT
!   TCAPRE = TEMPORARY CAPITAL REQUIREMENTS REPORT
!   TSALES = TEMPORARY SALES REPORT
!   TCSTCA = TEMPORARY COST OF CAPITAL REPORT
!   TCANPL = TEMPORARY CANCELLED PLANT REPORT
!*****************************************************************
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'efprep'
      include 'efprp2'
      include 'efpwrt'
      include 'efpcntrl'
      include 'control'
      include 'macout'
      include 'efpgen'
      include 'eusprc'
      include 'efpint'
      include 'uefdout'
      REAL*4 FRAC
      INTEGER IT    !OWNERSHIP-1 (PRIV.), 2 (PUB.), OR 3(BOTH)
      INTEGER NRGN
      INTEGER IC
      INTEGER I
      INTEGER J
      INTEGER K
      INTEGER KK
      INTEGER M
      INTEGER JJ
      INTEGER NN
      REAL*4 BAL17
      REAL*4 BAL23
      REAL*4 BAL20
      REAL*4 BAL19
      REAL*4 BAL15
      REAL*4 BAL16
      REAL*4 BAL12
      REAL*4 BAL10
      DO I=1,CURIYR
         IF (I .GT. 1) THEN
            BAL17 = BALSHT(17)
            BAL23 = BALSHT(23)
            BAL20 = BALSHT(20)
            BAL19 = BALSHT(19)
            BAL15 = BALSHT(15)
            BAL16 = BALSHT(16)
            BAL12 = BALSHT(12)
            BAL10 = BALSHT(10)
         ELSE
            BAL17 = 0.0
            BAL23 = 0.0
            BAL20 = 0.0
            BAL19 = 0.0
            BAL15 = 0.0
            BAL16 = 0.0
            BAL12 = 0.0
            BAL10 = 0.0
         ENDIF
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     WRITE(6,*) 'TMPSET - MC_JPGDP:', MC_JPGDP(IYRRL)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         GINFR(I) = MC_JPGDP(I)/MC_JPGDP(IYRRL)
         DO J=1,26    ! balance sheet rows
! FIRST - INITIALIZE SUMS TO ZERO
            IF (J .LE. 16) THEN
               DO K=1,8
                  TCAPRE(I,J,K) = 0.0
               END DO
            ENDIF
            IF (J .LE. 11) TCSTCA(I,J) = 0.0
            IF (J .LE. 3) TCANPL(I,J) = 0.0
            IF (J .LE. 24) TREVRE(I,J) = 0.0
            IF (J .LE. 2) TSALES(I,J) = 0.0
            IF (J .LE. 4) TPPWRBL(J,I)= 0.0
            IF (J .LE. 20) TXINCS(I,J) = 0.0
            IF (J .LE. 21) TFUNDS(I,J) = 0.0
            IF (J .LE. 19) TTAXIN(I,J) = 0.0
            IF (J .LE. 26) TBALSH(I,J) = 0.0
            IF ((IT .EQ. 3) .AND. (IC .EQ. 4)) THEN
             IF (J .LE. 5) THEN
               DO K=1,2
                  TEFPBL(J,K,I) = 0.0
               END DO
             ENDIF
              DO JJ=1,4
               IF (J .LE. 4) TCSTRE(I,J,JJ) = 0.0
              END DO
               DO K=1,4
                  IF (J .LE. 9) THEN
                     TDEMRE(I,J,K,1) = 0.0
                     TDEMRE(I,J,K,2) = 0.0
                  ENDIF
                  IF (J .LE. 4) THEN
                    DO JJ=1,4
                      TALLRE(I,J,K,JJ) = 0.0
                      TCS2RE(I,J,K,JJ) = 0.0
                      IF (K .LE. 3) THEN
                         TPRRRE(I,J,K,JJ) = 0.0

                      ENDIF
                    END DO
                  ENDIF
               END DO

            ENDIF
         END DO
! NEXT - SUM OVER THE FEDERAL REGIONS AND TAKE A WEIGHTED AVERAGE
! BY USING A PERCENTAGE OF EACH REGION'S RESULT
! FOR NATIONAL VALUES THE FRACTION FOR EACH REGION IS 1.0 (ALL OF IT)
            DO K=1,UNRGNS
! RETRIEVE THE APPROPRIATE RECORDS AND AGGREGATE IF NECESSARY
! (NATIONAL AGGREGATION IS TAKEN CARE OF BY USING 1.0 WEIGHTS ACROSS REGIONS
!  BUT PUBLIC/PRIVATE OWNERSHIP AND/OR OPERATING COMPONENT AGGREGATION
!  IS HANDLED SEPARATELY BY ADDUP ROUTINE)
               BALSHT(17) = BALSHT(17) + BAL17
               FUNDS(4) = FUNDS(4) - BAL23
               FUNDS(8) = FUNDS(8) - BAL20
               FUNDS(9) = FUNDS(9) - BAL19
               FUNDS(10) = FUNDS(10) - BAL15
               FUNDS(11) = FUNDS(11) - BAL16
               FUNDS(18) = FUNDS(18) - BAL12
               FUNDS(19) = FUNDS(19) - BAL10
               BALSHT(15) = BALSHT(15) - BAL17
               BALSHT(18) = BALSHT(18) + BAL17
               FUNDS(7) = FUNDS(7) - BAL23
               FUNDS(13) = FUNDS(13) -BAL20 - BAL19 - BAL15 - BAL16
               FUNDS(20) = FUNDS(20) - BAL12
               IF (I .EQ. 1) FUNDS(20) = FUNDS(20) - BAL10
               IF (NRGN .EQ. (NOPREG+1)) THEN
                  FRAC = 1.0
               ELSE
                  FRAC = FRACRG(K,NRGN)
               ENDIF
               IF (FRAC .NE. 0.0) THEN
                  CALL GETREP(IT,K,I,IC)
                  DO J=1,26
                  IF (J .LE. 14) THEN
                     DO M=1,8
                        TCAPRE(I,J,M) = TCAPRE(I,J,M) + FRAC &
                                        *CAPREQ(J,M)
                     END DO
!     IF ((I .EQ. 1).AND.(J.EQ.1)) WRITE(6,*) 'HELLO3',IT,IC,
!    * CAPREQ(1,8),TCAPRE(1,1,8),FRAC
                  ENDIF
!                 IF (J .LE. 11)TCSTCA(I,J) = TCSTCA(I,J) + FRAC*
!    1                                        CSTCAP(J)
                  IF (J .LE. 3)TCANPL(I,J) = TCANPL(I,J) + FRAC* &
                                             CANPLT(J)
                  IF (J .LE. 24)TREVRE(I,J) = TREVRE(I,J) + FRAC* &
                                              REVREQ(J)
                  IF (J .LE. 2)TSALES(I,J) = TSALES(I,J) + FRAC* &
                                             SALES(J)
                  IF (J .LE. 4) THEN
                             TPPWRBL(J,I) = TPPWRBL(J,I) + &
                                             (FRAC*PPCOST(J))
                  ENDIF
                  IF (J .LE. 20)TXINCS(I,J) = TXINCS(I,J) + FRAC* &
                                              XINCST(J)
                  IF (J .LE. 21)TFUNDS(I,J) = TFUNDS(I,J) + FRAC* &
                                              FUNDS(J)
                  IF (J .LE. 19)TTAXIN(I,J) = TTAXIN(I,J) + FRAC* &
                                              TAXINC(J)
                  IF (J .LE. 26)TBALSH(I,J) = TBALSH(I,J) + FRAC* &
                                              BALSHT(J)
                  IF (J .EQ. 26) THEN
                     TCSTCA(I,4) = FRAC*CSTCAP(4)* &
                                   BALSHT(19) + TCSTCA(I,4)
                     TCSTCA(I,5) = FRAC*CSTCAP(5)* &
                                   BALSHT(19) + TCSTCA(I,5)
                     TCSTCA(I,2) = FRAC*CSTCAP(2)* &
                                   BALSHT(16) + TCSTCA(I,2)
                     TCSTCA(I,3) = FRAC*CSTCAP(3)* &
                                   BALSHT(16) + TCSTCA(I,3)
                     TCSTCA(I,6) = FRAC*CSTCAP(6)* &
                                   BALSHT(20) + TCSTCA(I,6)
                     TCSTCA(I,1) = FRAC*CSTCAP(1)* &
                                   (BALSHT(15) + BALSHT(17)) + &
                                   TCSTCA(I,1)
                     DO JJ=7,10
                       TCSTCA(I,JJ) = FRAC*(CSTCAP(JJ)* &
                                      (BALSHT(18) + BALSHT(21))) + &
                                      TCSTCA(I,JJ)
                     END DO
                  ENDIF
                  END DO
! COMPUTE BULK POWER BY PRINT REGION
                  IF ((IT .EQ. 3) .AND. (IC .EQ. 4)) THEN
                  CALL GETRP2(K,I)
                  DO J=1,9
                     IF (J .LE. 5) THEN
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                       IF (NRGN .LT. NOPREG+1) THEN
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                        TEFPBL(J,1,I) = TEFPBL(J,1,I) + &
                                             (FRAC*EFPBLK(J,1))
                        TEFPBL(J,2,I) = TEFPBL(J,2,I) + &
                                             (FRAC*EFPBLK(J,2))
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                       ELSE
                        TEFPBL(J,1,I) = TEFPBL(J,1,I) + &
                                             (FRAC*ABS(EFPBLK(J,1)))
                        TEFPBL(J,2,I) = TEFPBL(J,2,I) + &
                                             (FRAC*ABS(EFPBLK(J,2)))
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!      WRITE(*,10)TEFPBL(J,2,I),EFPBLK(J,1),EFPBLK(J,2),FRAC,J,I,NRGN
!10    FORMAT(' TMPSET - TEFPBL(J,2,I):',F8.2,' EFPBLK(J,1):',F8.2,
!     1       ' EFPBLK(J,2):',F8.2,' FRAC,J,I,NRGN:',F4.2,I3,I3,I3)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                       ENDIF
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                     ENDIF
                        IF (J .LE. 4) THEN
                         DO JJ=1,4
                            IF (JJ .LE. 3) THEN
                           IF(J .LE. 4)TCSTRE(I,J,JJ)=TCSTRE(I,J,JJ) + &
                                                    (FRAC*COST(J,JJ))
                            ELSE
                               TCSTRE(I,J,JJ) = TCSTRE(I,J,1) + &
                                  TCSTRE(I,J,2) + TCSTRE(I,J,3)
                            ENDIF
                            TPRRRE(I,J,1,JJ) = TPRRRE(I,J,1,JJ) + &
                                               (FRAC*REV(J,JJ))
                            TPRRRE(I,J,2,JJ) = TPRRRE(I,J,2,JJ) + &
                                               (FRAC*SALCLS(K,J))


                         END DO
                        ENDIF
! FILL TOTAL OVER CLASSES SI TEMP ARRAY

                        TPRRRE(I,5,1,1) = TPRRRE(I,5,1,1) &
                                       + (FRAC*(REV(1,1) + REV(2,1) &
                                       + REV(3,1) + REV(4,1)))
                        TPRRRE(I,5,2,1) = TPRRRE(I,5,2,1) + &
                                       (FRAC*(SALCLS(K,1)+SALCLS(K,2) &
                                       + SALCLS(K,3)+SALCLS(K,4)))

                        DO KK=1,4
                              TDEMRE(I,J,KK,2) = TDEMRE(I,J,KK,2) + &
                                           (FRAC*DEMREP(J,KK,2))*100.0
                              IF (J .LE. 8) TDEMRE(I,J,KK,1) = &
                                            TDEMRE(I,J,KK,1) + &
                                            (FRAC*DEMREP(J,KK,1))
                           IF (J .LE. 4) THEN
                             DO JJ=1,4
            IF (JJ .LE. 3) THEN
                              TALLRE(I,J,KK,JJ) = TALLRE(I,J,KK,JJ) + &
                                          100.0*(FRAC*COSTFC(J,KK,JJ))
                              TCS2RE(I,J,KK,JJ) = TCS2RE(I,J,KK,JJ) + &
                                 (FRAC*(COSTFC(J,KK,JJ)*COST(J,JJ)))
            ELSE
               TALLRE(I,J,KK,JJ) = TALLRE(I,J,KK,1) + &
                                   TALLRE(I,J,KK,2) + TALLRE(I,J,KK,3)
               TCS2RE(I,J,KK,JJ) = TCS2RE(I,J,KK,1) + &
                                   TCS2RE(I,J,KK,2) + TCS2RE(I,J,KK,3)
            ENDIF
                             END DO
                           ENDIF
                        END DO
                  END DO
                  ENDIF
               ENDIF
            END DO
! CALCULATE COST OF CAPITAL RESULTS FOR REGIONAL TOTAL ONCE
! ALL OF THE REGION RESULTS HAVE BEEN ADDED IN.
               IF (TBALSH(I,19) .NE. 0.0) THEN
                  TCSTCA(I,4) = TCSTCA(I,4)/TBALSH(I,19)
                  TCSTCA(I,5) = TCSTCA(I,5)/TBALSH(I,19)
               ELSE
                  TCSTCA(I,4) = 0.0
                  TCSTCA(I,5) = 0.0
               ENDIF
               IF (TBALSH(I,16) .NE. 0.0) THEN
                  TCSTCA(I,2) = TCSTCA(I,2)/TBALSH(I,16)
                  TCSTCA(I,3) = TCSTCA(I,3)/TBALSH(I,16)
               ELSE
                  TCSTCA(I,2) = 0.0
                  TCSTCA(I,3) = 0.0
               ENDIF
               IF (TBALSH(I,20) .NE. 0.0) THEN
                  TCSTCA(I,6) = TCSTCA(I,6)/TBALSH(I,20)
               ELSE
                  TCSTCA(I,6) = 0.0
               ENDIF
               IF ((TBALSH(I,15)+TBALSH(I,17)).NE.0.0) THEN
                  TCSTCA(I,1) = TCSTCA(I,1)/ &
                                (TBALSH(I,15)+TBALSH(I,17))
               ELSE
                  TCSTCA(I,1) = 0.0
               ENDIF
               DO JJ=7,10
                  IF ((TBALSH(I,18)+TBALSH(I,21)) .NE. 0.0) THEN
                     TCSTCA(I,JJ) = TCSTCA(I,JJ)/ &
                                    (TBALSH(I,18) + TBALSH(I,21))
                  ELSE
                     TCSTCA(I,JJ) = 0.0
                  ENDIF
               END DO
         IF (TFUNDS(I,15) .NE. 0.0) THEN
            TFUNDS(I,21) = (TFUNDS(I,7) - &
                           (TFUNDS(I,16) + TFUNDS(I,17)))/ &
                           (TFUNDS(I,15) + TFUNDS(I,19))*100.0
         ENDIF
         IF (TREVRE(I,8) .NE. 0.0) THEN
            TREVRE(I,9) = TREVRE(I,10)/TREVRE(I,8)
            TCSTCA(I,11) = TREVRE(I,9)
         ENDIF
! CALCULATE REGIONAL PRICE BY SECTOR AND COMPONENT
         DO J=1,4
            DO JJ=1,4
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     WRITE(6,*) 'TMPSET - TPRRRE,I,J,JJ:',TPRRRE(I,J,2,JJ),I,J,JJ
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
              IF (TPRRRE(I,J,2,JJ) .NE. 0.0) THEN
               TPRRRE(I,J,3,JJ) = TPRRRE(I,J,1,JJ)/TPRRRE(I,J,2,JJ)
              ELSE
               TPRRRE(I,J,3,JJ) = 0.0
              ENDIF

            END DO

         END DO
         IF (TPRRRE(I,5,2,1) .NE. 0.0) THEN
            TPRRRE(I,5,3,1) = TPRRRE(I,5,1,1)/TPRRRE(I,5,2,1)
         ELSE
            TPRRRE(I,5,3,1) = 0.0
         ENDIF


      END DO
!     IF (NRGN .EQ. 1) WRITE(6,*) 'HELLO2',IT,IC,TCAPRE(1,1,8)
      RETURN
      END
      SUBROUTINE ADDUP(NRGN)
      IMPLICIT NONE
!*****************************************************************
!    THIS SUBROUTINE SUMS REGIONAL RESULTS TO YIELD TOTALS ACROSS OPERATING COMPONENT AND OWNERSHIP TYPES
!*****************************************************************
      include 'parametr'
      include 'emmparm'
      include 'efpcntrl'
      include 'efprep'
      include 'ncntrl'
      include 'control'
      DIMENSION TOTCAP(3)
      REAL*4 TOTCAP
      INTEGER IYR
      INTEGER JJ
      INTEGER JJJ
      INTEGER J
      INTEGER ITYPE          !OWNERSHIP TYPE
      INTEGER NRGN           !REGION
      INTEGER ICOMP          !OPERATING COMPONENT
      INTEGER IEND
      INTEGER K
      INTEGER IT
      INTEGER LL
      REAL*4 REVTMP(3,24)    !REVENUE REQUIREMENTS
      REAL*4 TAXTMP(3,19)    !TAX INCOME
      REAL*4 BALTMP(3,26)    !BALANCE SHEET
      REAL*4 FUNTMP(3,21)    !SOURCES/USES OF FUNDS
      REAL*4 XINTMP(3,20)    !INCOME STATEMENT
      REAL*4 CRQTMP(3,16,8)  !CAPITAL REQUIREMENTS
      REAL*4 SALTMP(3,2)     !SALES
      REAL*4 PPTMP(3,4)      !purchased power costs
      REAL*4 CSTTMP(3,11)    !COST OF CAPITAL
      REAL*4 CANTMP(3,3)     !CANCELLED PLANT
      IEND = 3
      IF (IPCOMP(4) .NE. 0) THEN
         IEND = 4
!     WRITE(*,*)'ADDUP BEGIN -IEND:',IEND
! ADD GENERATION, TRANSMISSION AND DISTRIBUTION COMPONENTS
         DO IYR=1,LASTYR
            DO ITYPE=1,2
               DO ICOMP=1,3
                  CALL GETREP(ITYPE,NRGN,IYR,ICOMP)
                  DO JJ=1,26
                     IF (JJ .LE. 24) REVTMP(ICOMP,JJ) = REVREQ(JJ)
                     IF ((JJ.LE.19).AND.(ITYPE.NE.2)) TAXTMP(ICOMP,JJ) = &
                                                      TAXINC(JJ)
                     BALTMP(ICOMP,JJ) = BALSHT(JJ)
                     IF (JJ .LE. 21) FUNTMP(ICOMP,JJ) = FUNDS(JJ)
                     IF (JJ .LE. 20) XINTMP(ICOMP,JJ) = XINCST(JJ)
                     IF (JJ .LE. 14) THEN
                        DO JJJ=1,8
                           CRQTMP(ICOMP,JJ,JJJ) = CAPREQ(JJ,JJJ)
                        END DO
                     ENDIF
                     IF (JJ .LE. 2) SALTMP(ICOMP,JJ) = SALES(JJ)
                     IF (JJ .LE. 4) PPTMP(ICOMP,JJ) = PPCOST(JJ)
                     IF (JJ .LE. 11) CSTTMP(ICOMP,JJ) = CSTCAP(JJ)
                     IF (JJ .LE. 3) CANTMP(ICOMP,JJ) = CANPLT(JJ)
                  END DO
               END DO
               DO J=1,20
                  XINCST(J) = XINTMP(1,J) + XINTMP(2,J) + XINTMP(3,J)
               END DO
               DO J=1,26
                  BALSHT(J) = BALTMP(1,J) + BALTMP(2,J) + BALTMP(3,J)
               END DO
               DO J=1,21
                  FUNDS(J) = FUNTMP(1,J) + FUNTMP(2,J) + FUNTMP(3,J)
               END DO
               IF (FUNDS(15) .NE. 0.0) &
                  FUNDS(21) = (FUNDS(7) - (FUNDS(16) + FUNDS(17)))/ &
                              (FUNDS(15) + FUNDS(19))*100.0
               DO J=1,24
                  REVREQ(J) = REVTMP(1,J) + REVTMP(2,J) + REVTMP(3,J)
               END DO
               IF (REVREQ(8) .NE. 0.0) &
                  REVREQ(9) = REVREQ(10)/REVREQ(8)
               DO J=1,2
                  SALES(J) = SALTMP(1,J)
               END DO
               DO J=1,4
                  PPCOST(J) = PPTMP(1,J)
               END DO
               DO J=1,NOCAP
                  DO K=1,8
                     CAPREQ(J,K) = CRQTMP(1,J,K) + CRQTMP(2,J,K) + &
                                   CRQTMP(3,J,K)
                  END DO
               END DO
               DO J=1,3
                  TOTCAP(J) = BALTMP(J,18) + BALTMP(J,21)
               END DO
! DO LONG TERM DEBT COSTS OF CAPITAL FOR TOTAL OPERATING COMPONENT
               IF (BALSHT(19) .EQ. 0.0) THEN
                  CSTCAP(4) = 0.0
                  CSTCAP(5) = 0.0
               ELSE
                  CSTCAP(4) = (CSTTMP(1,4)*BALTMP(1,19) + CSTTMP(2,4)* &
                              BALTMP(2,19) + CSTTMP(3,4)*BALTMP(3,19))/ &
                              BALSHT(19)
                  CSTCAP(5) = (CSTTMP(1,5)*BALTMP(1,19) + CSTTMP(2,5)* &
                              BALTMP(2,19) + CSTTMP(3,5)*BALTMP(3,19))/ &
                              BALSHT(19)
               ENDIF
! DO PREFERRED STOCK COSTS OF CAPITAL FOR TOTAL OPERATING COMPONENT
               IF (BALSHT(16) .EQ. 0.0) THEN
                  CSTCAP(2) = 0.0
                  CSTCAP(3) = 0.0
               ELSE
                  CSTCAP(2) = (CSTTMP(1,2)*BALTMP(1,16) + CSTTMP(2,2)* &
                              BALTMP(2,16) + CSTTMP(3,2)*BALTMP(3,16))/ &
                              BALSHT(16)
                  CSTCAP(3) = (CSTTMP(1,3)*BALTMP(1,16) + CSTTMP(2,3)* &
                              BALTMP(2,16) + CSTTMP(3,3)*BALTMP(3,16))/ &
                              BALSHT(16)
               ENDIF
! DO SHORT TERM DEBT COSTS OF CAPITAL FOR TOTAL OPERATING COMPONENT
               IF (BALSHT(20) .EQ. 0.0) THEN
                  CSTCAP(6) = 0.0
               ELSE
                  CSTCAP(6) = (CSTTMP(1,6)*BALTMP(1,20) + CSTTMP(2,6)* &
                              BALTMP(2,20) + CSTTMP(3,6)*BALTMP(3,20))/ &
                              BALSHT(20)
               ENDIF
! DO COMMON EQUITY COSTS OF CAPITAL FOR TOTAL OPERATING COMPONENT
               IF ((BALSHT(15)+BALSHT(17)) .EQ. 0.0) THEN
                  CSTCAP(1) = 0.0
               ELSE
                  CSTCAP(1) = (CSTTMP(1,1)*(BALTMP(1,15)+BALTMP(1,17)) + &
                              CSTTMP(2,1)*(BALTMP(2,15)+BALTMP(2,17)) + &
                              CSTTMP(3,1)*(BALTMP(3,15)+BALTMP(3,17)))/ &
                              (BALSHT(15) + BALSHT(17))
               ENDIF
               CSTCAP(11)=REVREQ(9)
! DO CAPITAL STRUCTURE WEIGHTS FOR TOTAL OPERATING COMPONENT
               DO J=7,10
                  IF ((TOTCAP(1)+TOTCAP(2)+TOTCAP(3)) .EQ. 0.0) THEN
                     CSTCAP(J) = 0.0
                  ELSE
                     CSTCAP(J) = ((CSTTMP(1,J)*TOTCAP(1)) + (CSTTMP(2,J) &
                                 *TOTCAP(2)) + (CSTTMP(3,J)*TOTCAP(3)))/ &
                                 (TOTCAP(1) + TOTCAP(2) + TOTCAP(3))
                  ENDIF
               END DO
               DO J=1,3
                  CANPLT(J) = CANTMP(1,J) + CANTMP(2,J) + CANTMP(3,J)
               END DO
               IF (ITYPE .NE. 2) THEN
                  DO J=1,19
                     TAXINC(J) = TAXTMP(1,J) + TAXTMP(2,J) + TAXTMP(3,J)
                  END DO
               ENDIF
               CALL STRREP(ITYPE,NRGN,IYR,IEND)
            END DO
         END DO
      ENDIF
! ADD PUBLIC AND PRIVATE TO GET REGIONAL TOTAL, IF NECESSARY.
      IF (IPOWN(3) .NE. 0) THEN
         DO IYR=1,LASTYR
            DO ICOMP=1,IEND
               DO ITYPE=1,2
                  CALL GETREP(ITYPE,NRGN,IYR,ICOMP)
                  DO JJ=1,26
                     IF (JJ .LE. 24) REVTMP(ITYPE,JJ) = REVREQ(JJ)
                     BALTMP(ITYPE,JJ) = BALSHT(JJ)
                     IF (JJ .LE. 21) FUNTMP(ITYPE,JJ) = FUNDS(JJ)
                     IF (JJ .LE. 20) XINTMP(ITYPE,JJ) = XINCST(JJ)
                     IF (JJ .LE. 14) THEN
                        DO JJJ=1,8
                           CRQTMP(ITYPE,JJ,JJJ) = CAPREQ(JJ,JJJ)
                        END DO
                     ENDIF
                     IF (JJ .LE. 2) SALTMP(ITYPE,JJ) = SALES(JJ)
                     IF (JJ .LE. 4) PPTMP(ITYPE,JJ) = PPCOST(JJ)
                     IF (JJ .LE. 11) CSTTMP(ITYPE,JJ) = CSTCAP(JJ)
                     IF (JJ .LE. 3) CANTMP(ITYPE,JJ) = CANPLT(JJ)
                  END DO
               END DO
               DO J=1,20
                  XINCST(J) = XINTMP(1,J) + XINTMP(2,J)
               END DO
               DO J=1,26
                  BALSHT(J) = BALTMP(1,J) + BALTMP(2,J)
               END DO
               DO J=1,21
                  FUNDS(J) = FUNTMP(1,J) + FUNTMP(2,J)
               END DO
               IF (FUNDS(15) .NE. 0.0) FUNDS(21) = (FUNDS(7) - &
                  (FUNDS(16) + FUNDS(17)))/(FUNDS(15) + FUNDS(19))*100.0
               DO J=1,24
                  REVREQ(J) = REVTMP(1,J) + REVTMP(2,J)
               END DO
               IF (REVREQ(8) .NE. 0.0) REVREQ(9) = REVREQ(10)/REVREQ(8)
               DO J=1,2
                  SALES(J) = SALTMP(1,J) + SALTMP(2,J)
               END DO
               DO J=1,4
                  PPCOST(J) = PPTMP(1,J) + PPTMP(2,J)
               END DO
               DO J=1,NOCAP
                  DO K=1,8
                     CAPREQ(J,K) = CRQTMP(1,J,K) + CRQTMP(2,J,K)
                  END DO
               END DO
               DO J=1,2
                  TOTCAP(J) = BALTMP(J,18) + BALTMP(J,21)
               END DO
! DO LONG TERM DEBT COSTS OF CAPITAL FOR TOTAL REGION
               IF (BALSHT(19) .EQ. 0.0) THEN
                  CSTCAP(4) = 0.0
                  CSTCAP(5) = 0.0
               ELSE
                  CSTCAP(4) = (CSTTMP(1,4)*BALTMP(1,19) + CSTTMP(2,4) &
                              *BALTMP(2,19))/BALSHT(19)
                  CSTCAP(5) = (CSTTMP(1,5)*BALTMP(1,19) + CSTTMP(2,5)* &
                              BALTMP(2,19))/BALSHT(19)
               ENDIF
! DO PREFERRED STOCK COSTS OF CAPITAL FOR TOTAL REGION
               IF (BALSHT(16) .EQ. 0.0) THEN
                  CSTCAP(2) = 0.0
                  CSTCAP(3) = 0.0
               ELSE
                  CSTCAP(2) = (CSTTMP(1,2)*BALTMP(1,16) + CSTTMP(2,2)* &
                              BALTMP(2,16))/BALSHT(16)
                  CSTCAP(3) = (CSTTMP(1,3)*BALTMP(1,16) + CSTTMP(2,3)* &
                               BALTMP(2,16))/BALSHT(16)
               ENDIF
! DO SHORT TERM DEBT COSTS OF CAPITAL FOR TOTAL REGION
               IF (BALSHT(20) .EQ. 0.0) THEN
                  CSTCAP(6) = 0.0
               ELSE
                  CSTCAP(6) = (CSTTMP(1,6)*BALTMP(1,20) + CSTTMP(2,6)* &
                               BALTMP(2,20))/BALSHT(20)
               ENDIF
! DO COMMON EQUITY COSTS OF CAPITAL FOR TOTAL REGION
               IF((BALSHT(15)+BALSHT(17)) .EQ. 0.0) THEN
                  CSTCAP(1) = 0.0
               ELSE
                  CSTCAP(1) = (CSTTMP(1,1)*(BALTMP(1,15)+BALTMP(1,17)) + &
                              CSTTMP(2,1)*(BALTMP(2,15)+BALTMP(2,17)))/ &
                              (BALSHT(15) + BALSHT(17))
               ENDIF
               CSTCAP(11) = REVREQ(9)
! DO CAPITAL STRUCTURE WEIGHTS FOR TOTAL REGION
               DO J=7,10
                  IF ((TOTCAP(1) + TOTCAP(2)) .EQ. 0.0) THEN
                     CSTCAP(J) = 0.0
                  ELSE
                     CSTCAP(J) = ((CSTTMP(1,J)*TOTCAP(1)) + (CSTTMP(2,J) &
                                 *TOTCAP(2)))/(TOTCAP(1) + TOTCAP(2))
                  ENDIF
               END DO
               DO J=1,3
                  CANPLT(J) = CANTMP(1,J) + CANTMP(2,J)
               END DO
               IT = 3
               CALL STRREP(IT,NRGN,IYR,ICOMP)
            END DO
         END DO
      ENDIF
      RETURN
      END
      SUBROUTINE EFPREALDOLLARS(ITYPE)
      IMPLICIT NONE
!*****************************************************************
!    THIS SUBROUTINE CONVERTS ALL DOLLARS FROM NOMINAL TO REAL DOLLARS ONLY WHEN IREAL=1
!*****************************************************************
! INPUT VARIABLES:
!   ITYPE = OWNERSHIP CLASS
!   GINFR = DEFLATION RATE
!   IYRRL = Year of real dollars reported
! MODIFIED VARIABLES:
!   TREVRE = TEMPORARY REVENUE REQUIREMENTS REPORT
!   TTAXIN = TEMPORARY TAX INCOME REPORT
!   TBALSH = TEMPORARY BALANCE SHEET REPORT
!   TFUNDS = TEMPORARY SOURCES/USES OF FUNDS REPORT
!   TXINCS = TEMPORARY INCOME STATEMENT REPORT
!   TCAPRE = TEMPORARY CAPITAL REQUIREMENTS REPORT
!   TSALES = TEMPORARY SALES REPORT
! OUTPUT VARIABLES:
!   NAME$ = REPORTING TITLE INDICATING REAL OR NOMINAL DOLLARS
!*****************************************************************
      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'efpwrt'
      include 'efpcntrl'
      include 'efpname'
      INTEGER J
      INTEGER K
      INTEGER I
      INTEGER ITYPE
      INTEGER ICLASS
      INTEGER II
      INTEGER ICOST
      CHARACTER*8 NAME$
      DATA NAME$/' REAL $S'/
      NAMER(7:14) = NAME$
      WRITE(NAMER(3:6),'(I4)') IYRS(IYRRL)
      DO I=1,LASTYR
         DO J=1,20
            TXINCS(I,J) = TXINCS(I,J)/GINFR(I)
         END DO
         DO J=1,26
            TBALSH(I,J) = TBALSH(I,J)/GINFR(I)
         END DO
         DO J=1,20
            TFUNDS(I,J) = TFUNDS(I,J)/GINFR(I)
         END DO
         DO J=1,8
            TREVRE(I,J) = TREVRE(I,J)/GINFR(I)
         END DO
         DO J=10,24
            TREVRE(I,J) = TREVRE(I,J)/GINFR(I)
         END DO
         TSALES(I,2) = TSALES(I,2)/GINFR(I)
         DO J=1,4
            TPPWRBL(J,I) = TPPWRBL(J,I)/GINFR(I)
         END DO
         DO J=1,5
            TEFPBL(J,1,I) = TEFPBL(J,1,I)/GINFR(I)
         END DO
         DO J=1,NOCAP
            DO K=3,8
               TCAPRE(I,J,K) = TCAPRE(I,J,K)/GINFR(I)
            END DO
         END DO
         IF (ITYPE .EQ. 1) THEN
            DO J=1,19
               TTAXIN(I,J) = TTAXIN(I,J)/GINFR(I)
            END DO
         ENDIF
         IF (ITYPE .EQ. 3) THEN
! II IS THE SECTORS (GEN,TRANS,DIST,TOTAL)
            DO II=1,4
! ICOST IS THE COST POOLS (FUEL, VAR OM, FIX OM, CAPITAL)
               DO ICOST=1,4
                  TCSTRE(I,ICOST,II)=TCSTRE(I,ICOST,II)/GINFR(I)
! ICLASS IS THE CUSTOMER CLASSES (RES, COMM, IND, TRANS)
                  DO ICLASS=1,4
                     TCS2RE(I,ICOST,ICLASS,II)= &
                        TCS2RE(I,ICOST,ICLASS,II)/GINFR(I)
                  END DO
               END DO
               DO ICLASS=1,4
! POSITION 1 OF TPRRRE IS THE REVENUES
                  TPRRRE(I,ICLASS,1,II)= &
                     TPRRRE(I,ICLASS,1,II)/GINFR(I)

! POSITION 3 OF TPRRRE IS THE PRICE
                  TPRRRE(I,ICLASS,3,II)= &
                     TPRRRE(I,ICLASS,3,II)/GINFR(I)

               END DO
            END DO

         ENDIF
      END DO
      RETURN
      END
      SUBROUTINE REPLAR(ITYPE)
      IMPLICIT NONE
!******************************************************************
!         THIS SUBROUTINE PRINTS THE LARGE REPORT
!******************************************************************
      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'efpwrt'
      include 'efprc'
      include 'efpgen'
      include 'efpname'
      include 'efpcntrl'
      REAL*4 D(9)
      INTEGER IYR1
      INTEGER IYR2
      INTEGER ID
      INTEGER ITYPE
      INTEGER I
      INTEGER K
      INTEGER L
      CHARACTER*20 NAME5
      CHARACTER*20 NAME4
      DATA NAME5/'ADJ  EOY RATE BASE  '/
      DATA NAME4/'AVERAGE RATE BASE   '/
      DATA D/9*0.0/
      IOUT = UF_EFPRPT
! *** PRINT INCOME STATEMENT
      IF (IPRPT(3) .EQ. 1) THEN
      WRITE(IOUT,10)
   10 FORMAT('1'//20X,'*****  PRO FORMA STATEMENT OF EARNINGS  **', &
             '***',5(/),75X,'YEAR ENDED DECEMBER 31'/45X,105('-')/)
      IYR1 = IYRRPT
      IYR2 = IYR1 + 9
   20 IF (IYR2 .GT. LASTYR) GO TO 100
      WRITE(IOUT,30)NAME1,NAMER,NAME2,NAME3,(IYRS(I),I=IYR1,IYR2), &
                    ((TXINCS(I,K),I=IYR1,IYR2),K=1,9)
!...+....1....+....2....+....3....+....4....+....5....+....6....+....7..
   30 FORMAT(/1X,A16,1X,A16/1X,A50/1X,A71/50X,10(I4,7X)/45X,10(9('-'), &
             2X)/75X,'(DOLLARS IN MILLIONS)'//1X,'ELECTRIC REVENUES' &
             ,26X,10('$',F9.0,1X)/45X,10(9('-'),2X)/1X, &
             'ELECTRIC EXPENSES'/3X,'OPERATION & MAINTENANCE', &
             19X,10(F9.0,2X)/3X,'FUEL',38X,10(F9.0,2X)/3X, &
             'DEPRECIATION',30X,10(F9.0,2X)/3X,'NET LEASE PAYMENTS', &
             24X,10(F9.0,2X)/3X,'OTHER TAXES',31X,10(F9.0,2X)/3X, &
             'INCOME TAXES',30X,10(F9.0,2X)/45X,10(9('-'),2X)/8X, &
             'TOTAL ELECTRIC EXPENSES',14X,10(F9.0,2X)/45X, &
             10(9('-'),2X)/10X,'ELECTRIC OPERATING INCOME',10X, &
             10(F9.0,2X)/45X,10(9('-'),2X)/)
      WRITE(IOUT,40)((TXINCS(I,K),I=IYR1,IYR2),K=10,20)
   40 FORMAT(1X,'OTHER INCOME'/3X,'AFUDC-EQUITY FUNDS',24X,10(F9.0, &
             2X)/45X,10(9('-'),2X)/10X,'INCOME BEFORE INTEREST', &
             ' DEDUCTIONS',2X,10(F9.0,2X)/45X,10(9('-'),2X)/1X, &
             'INTEREST DEDUCTIONS'/3X,'INTEREST ON LONG-TERM DEBT', &
             16X,10(F9.0,2X)/3X,'INTEREST ON SHORT-TERM DEBT',15X, &
             10(F9.0,2X)/3X,'AFUDC-BORROWED FUNDS',21X,10('(',F9.0, &
             ')')/45X,10(9('-'),2X)/8X,'TOTAL INTEREST DEDUCTIONS', &
             12X,10(F9.0,2X)/45X,10(9('-'),2X)/1X,'NET INCOME',34X, &
             10(F9.0,2X)/3X,'DIVIDENDS ON PREFERRED STOCKS',12X, &
             10('(',F9.0,')')/3X,'NET DEFERRED PHASE-IN REVENUES', &
             11X,10(1X,F9.0,1X)/3X,'WRITE OFF OF DISALLOWED PLANT', &
             12X,10('(',F9.0,')')/45X,/45X,10(9('-'),2X)/1X, &
             'EARNINGS FOR COMMON STOCK',18X,10('$',F9.0,1X)/45X, &
             10(9('-'),2X)/45X,10(9('-'),2X)/'1'/)
      IYR1 = IYR1 + 10
      IYR2 = IYR1 + 9
      GO TO 20
  100   IF (IYR1 .LE. LASTYR) THEN
         ID = IYR2 - LASTYR
         WRITE(IOUT,30)NAME1,NAMER,NAME2,NAME3,(IYRS(I),I=IYR1,IYR2), &
                       ((TXINCS(I,K),I=IYR1,LASTYR),(D(L),L=1,ID),K=1,9)
         WRITE(IOUT,40)((TXINCS(I,K),I=IYR1,LASTYR), &
                       (D(L),L=1,ID),K=10,20)
        ENDIF
      ENDIF
!     *****PRINT BALANCE SHEET*****
      IF (IPRPT(4) .EQ. 1) THEN
      WRITE(IOUT,210)
  210 FORMAT(20X,'*****  PRO FORMA BALANCE SHEET  *****',1(/),75X, &
             'DECEMBER 31'/45X,105('-'))
      IYR1 = IYRRPT
      IYR2 = IYR1 + 9
  220 IF (IYR2 .GT. LASTYR) GO TO 260
      WRITE(IOUT,230)NAME1,NAMER,NAME2,NAME3,(IYRS(I),I=IYR1,IYR2), &
                     ((TBALSH(I,K),I=IYR1,IYR2),K=1,9)
  230 FORMAT(/1X,A16,1X,A16/1X,A50/1X,A71/50X,10(I4,7X)/45X,10(9('-'), &
             2X)/75X,'(DOLLARS IN  MILLIONS)'/15X, &
             'ASSETS & OTHER DEBITS'/1X,'UTILITY PLANT'/3X, &
             'PRODUCTION',32X,10(F9.0,2X)/3X,'TRANSMISSION',30X, &
             10(F9.0,2X)/3X,'DISTRIBUTION',30X,10(F9.0,2X)/3X, &
             'OTHER',37X,10(F9.0,2X)/3X,'CONSTRUCTION WORK', &
             ' IN PROGRESS',13X,10(F9.0,2X)/3X, &
             'NET DISALLOWED PLANT',21X,10('(',F9.0,')')/45X, &
             10(9('-'),2X)/8X,'TOTAL UTILITY PLANT',18X,10(F9.0,2X)/ &
             45X,10(9('-'),2X)/3X,'ACCUMULATED DEPRECIATION',18X, &
             10(F9.0,2X)/45X,10(9('-'),2X)/8X,'NET UTILITY PLANT,', &
             ' LESS NUCLEAR FUEL',1X,10(F9.0,2X)/45X,10(9('-'),2X))
      WRITE(IOUT,240)((TBALSH(I,K),I=IYR1,IYR2),K=10,18)
  240 FORMAT(3X,'NET NUCLEAR FUEL',26X,10(F9.0,2X)/45X,10(9('-'),2X) &
             /8X,'NET UTILITY PLANT',20X,10(F9.0,2X)/45X,10(9('-'), &
             2X)/1X,'WORKING CAPITAL',29X,10(F9.0,2X)/1X, &
             'ACCUMULATED DEFERRED PHASE-IN REVENUES',6X,10(F9.0,2X) &
             /45X,10(9('-'),2X)/8X,'TOTAL ASSETS & OTHER DEBITS', &
             10X,10(F9.0,2X)/45X,10(9('-'),2X)/45X,10(9('-'),2X)/ &
             15X,'LIABILITIES & OTHER CREDITS'/1X, &
             'PROPRIETARY CAPITAL'/3X,'COMMON STOCK',30X,10(F9.0,2X) &
             /3X,'PREFERRED STOCK',27X,10(F9.0,2X)/3X, &
             'RETAINED EARNINGS',25X,10(F9.0,2X)/45X,10(9('-'),2X)/ &
             8X,'TOTAL PROPRIETARY CAPITAL',12X,10(F9.0,2X)/45X, &
             10(9('-'),2X))
      WRITE(IOUT,250)((TBALSH(I,K),I=IYR1,IYR2),K=19,26)
  250 FORMAT(1X,'LONG-TERM DEBT',30X,10(F9.0,2X)/1X, &
             'SHORT-TERM DEBT',29X,10(F9.0,2X)/45X,10(9('-'),2X)/8X, &
             'TOTAL DEBT',27X,10(F9.0,2X)/45X,10(9('-'),2X)/1X, &
             'DEFERRED CREDITS'/3X,'ACCUMULATED DEFERRED ITC',18X, &
             10(F9.0,2X)/3X,'ACCUMULATED DEFERRED INCOME TAXES', &
             9X,10(F9.0,2X)/3X,'NET GAINS ON SALE/LEASEBACK',15X, &
             10(F9.0,2X)/45X,10(9('-'),2X)/8X, &
             'TOTAL DEFERRED CREDITS',15X,10(F9.0,2X)/45X,10(9('-'), &
             2X)/8X,'TOTAL LIABILITIES &',' OTHER CREDITS',4X, &
             10(F9.0,2X)/45X,10(9('-'),2X)/45X,10(9('-'),2X)/'1'/)
      IYR1 = IYR1 + 10
      IYR2 = IYR1 + 9
      GO TO 220
  260   IF (IYR1 .LE. LASTYR) THEN
         ID = IYR2 - LASTYR
         WRITE(IOUT,230)NAME1,NAMER,NAME2,NAME3,(IYRS(I),I=IYR1,IYR2), &
                        ((TBALSH(I,K),I=IYR1,LASTYR), &
                        (D(L),L=1,ID),K=1,9)
         WRITE(IOUT,240)((TBALSH(I,K),I=IYR1,LASTYR), &
                        (D(L),L=1,ID),K=10,18)
         WRITE(IOUT,250)((TBALSH(I,K),I=IYR1,LASTYR), &
                        (D(L),L=1,ID),K=19,26)
        ENDIF
      ENDIF
!     *****PRINT NOTES TO FINANCIAL STATEMENTS*****
!           (ONLY PRINTED FOR PRIVATE SECTOR)
      IF (IPRPT(5) .EQ. 1) THEN
        IF (ITYPE .EQ. 1) THEN
         WRITE(IOUT,310)
  310    FORMAT(' ',20X,'*****  PRO FORMA NOTES TO FINANCIAL', &
                ' STATEMENTS  *****',5(/),1X,'INCOME TAX EXPENSE'// &
                5X,'INCOME TAX EXPENSE IS COMPRISED OF THE ', &
                'FOLLOWING COMPONENTS:'/75X,'YEAR ENDED DECEMBER 31' &
                /45X,105('-')/)
         IYR1=IYRRPT
         IYR2=IYR1+9
  320    IF (IYR2 .GT. LASTYR) GO TO 350
         WRITE(IOUT,330)NAME1,NAMER,NAME2,NAME3,(IYRS(I),I=IYR1,IYR2), &
                        ((TTAXIN(I,K),I=IYR1,IYR2),K=1,5)
  330    FORMAT(/1X,A16,1X,A16/1X,A50/1X,A71/50X,10(I4,7X)/45X, &
                10(9('-'),2X)/75X,'(DOLLARS  IN MILLIONS)'/3X, &
                'CURRENT INCOME TAXES',22X,10(F9.0,2X)/45X, &
                10(9('-'),2X)//3X,'DEFERRED  TAXES, NET'/5X, &
                'ACCELERATED DEPRECIATION',16X,10(F9.0,2X)/5X, &
                'TAX SAVINGS FROM AFUDC-DEBT PORTION'/7X,'DEFERRED', &
                30X,10(F9.0,2X)/7X,'AMORTIZATION (CREDIT)',16X, &
                10('(',F9.0,')')/45X,10(9('-'),2X)/45X,10(F9.0,2X))
         WRITE(IOUT,340)((TTAXIN(I,K),I=IYR1,IYR2),K=6,9)
  340    FORMAT(3X,'INVESTMENT TAX CREDIT, NET'/5X,'DEFERRED',32X, &
                10(F9.0,2X)/5X,'AMORTIZATION (CREDIT)',18X, &
                10('(',F9.0,')')/45X,10(9('-'),2X)/45X,10(F9.0, &
                2X)/45X,10(9('-'),2X)//11X,'TOTAL', &
                ' INCOME TAX EXPENSE',10X,10(F9.0,2X)/45X, &
                10(9('-'),2X)/45X,10(9('-'),2X)//)
         IYR1 = IYR1 + 10
         IYR2 = IYR1 + 9
         GO TO 320
  350    IF (IYR1 .LE. LASTYR) THEN
            ID = IYR2 - LASTYR
            WRITE(IOUT,330)NAME1,NAMER,NAME2,NAME3, &
                           (IYRS(I),I=IYR1,IYR2), &
                           ((TTAXIN(I,K),I=IYR1,LASTYR), &
                           (D(L),L=1,ID),K=1,5)
            WRITE(IOUT,340)((TTAXIN(I,K),I=IYR1,LASTYR), &
                           (D(L),L=1,ID),K=6,9)
         ENDIF
! PRINT FINANCIAL STATEMENTS
         WRITE(IOUT,370)
  370    FORMAT('1',20X,'***** PRO FORMA NOTES TO FINANCIAL STATE', &
                'MENTS, CONT  *****',5(/),3X,'INCOME TAXES DIFFER ', &
                'FROM AMOUNTS COMPUTED BY APPLYING THE STATUTORY' &
                /5X,'TAX RATE TO PRE-TAX INCOME AS FOLLOWS:'/75X, &
                'YEAR ENDED DECEMBER 31'/45X,105('-')/)
         IYR1 = IYRRPT
         IYR2 = IYR1 + 9
  400    IF (IYR2 .GT. LASTYR) GO TO 420
         WRITE(IOUT,410)NAME1,NAMER,NAME2,NAME3,(IYRS(I),I=IYR1,IYR2), &
                        ((TTAXIN(I,K),I=IYR1,IYR2),K=10,19)
  410    FORMAT(/1X,A16,1X,A16/1X,A50/1X,A71/50X,10(I4,7X)/45X, &
                10(9('-'),2X)/75X,'(DOLLARS IN MILLIONS)'//1X, &
                'INCOME TAXES ON PRE-TAX INCOME AT THE'/3X, &
                'STATUTORY FEDERAL RATE OF 46%',13X,10(F9.0,2X)//1X, &
                'ADJUSTMENTS TO ABOVE AT 46%'/5X,'UNALLOWABLE ', &
                'BOOK DEPRECIATION',11X,10(F9.0,2X)/5X, &
                'AFUDC-EQUITY FUNDS',21X,10('(',F9.0,')')// &
                ' ADJUSTMENTS DUE TO TAX SAVINGS'/5X, &
                'INVESTMENT TAX CREDIT'/7X,'NOT DEFERRED',25X, &
                10('(',F9.0,')')/7X,'AMORTIZATION',25X,10('(',F9.0, &
                ')')/5X,'TAX SAVINGS FROM AFUDC-DEBT PORTION'/7X, &
                'NOT DEFERRED',25X,10('(',F9.0,')')/7X, &
                'AMORTIZATION',25X,10('(',F9.0,')')/5X, &
                'EXCESS DEF TAXES FROM TAX   '/7X,'RATE CHANGE', &
                '-NOT DEFERRED',13X,10('(',F9.0,')')//1X, &
                'STATE INCOME TAXES, NET OF FEDERAL'/3X, &
                'INCOME TAX BENEFIT',24X,10(F9.0,2X)/45X,10(9('-'), &
                2X)/1X,'TOTAL',' INCOME TAX EXPENSE',20X,10(F9.0,2X) &
                /45X,10(9('-'),2X)/45X,10(9('-'),2X)//)
         IYR1 = IYR1 + 10
         IYR2 = IYR1 + 9
         GO TO 400
  420    IF (IYR1 .GT. LASTYR) GO TO 500
         ID = IYR2 - LASTYR
         WRITE(IOUT,410)NAME1,NAMER,NAME2,NAME3,(IYRS(I),I=IYR1,IYR2), &
                        ((TTAXIN(I,K),I=IYR1,LASTYR),(D(L),L=1,ID), &
                        K=10,19)
        ENDIF
      ENDIF
!     *****PRINT SOURCES AND USES STATEMENT*****
500   CONTINUE
      IF (IPRPT(6) .EQ. 1) THEN
      WRITE(IOUT,510)
  510 FORMAT('1'//20X,'***** PRO FORMA SOURCES AND USES OF FUNDS', &
             ' *****',3(/),75X,'DECEMBER 31'//45X,105('-')/)
      IYR1 = IYRRPT
      IYR2 = IYR1 + 9
  520 IF (IYR2 .GT. LASTYR) GO TO 550
      WRITE(IOUT,530)NAME1,NAMER,NAME2,NAME3,(IYRS(I),I=IYR1,IYR2), &
                     ((TFUNDS(I,K),I=IYR1,IYR2),K=1,14)
  530 FORMAT(/1X,A16,1X,A16/1X,A50/1X,A71/50X,10(I4,7X)/45X, &
             10(9('-'),2X)/75X,'(DOLLARS IN MILLIONS)'//1X, &
             'SOURCES OF FUNDS:'//3X,'NET INCOME',32X,10(F9.0,2X)/ &
             3X,'DEPRECIATION',30X,10(F9.0,2X)/3X, &
             'DEFERRED ITC-NET',26X,10(F9.0,2X)/3X, &
             'DEFERRED INCOME TAXES',21X,10(F9.0,2X)/3X, &
             'AMORT OF NET GAIN ON S/L',17X,10('(',F9.0,')')/3X, &
             'AFUDC',36X,10('(',F9.0,')')/45X,10(9('-'),2X)/5X, &
             'TOTAL INTERNAL FUNDS',20X,10(F9.0,2X)//3X, &
             'INCREASE IN SHORT-TERM DEBT',15X,10(F9.0,2X)/3X, &
             'INCREASE IN LONG-TERM DEBT',16X,10(F9.0,2X)/3X, &
             'ISSUANCE OF COMMON STOCK',18X,10(F9.0,2X)/3X, &
             'ISSUANCE OF PREFERRED STOCK',15X,10(F9.0,2X)/3X, &
             'NET PROCEEDS FROM SALE/LEASEBACK',10X,10(F9.0,2X)/45X, &
             10(9('-'),2X)/5X,'TOTAL EXTERNAL FUNDS',20X,10(F9.0,2X) &
             /45X,10(9('-'),2X)/8X,'TOTAL SOURCES',24X,10(F9.0,2X)/ &
             45X,10(9('-'),2X)/)
      WRITE(IOUT,540)((TFUNDS(I,K),I=IYR1,IYR2),K=15,21)
  540 FORMAT(45X,10(9('-'),2X)/1X,'APPLICATION OF FUNDS:'//3X, &
             'CONSTRUCTION EXPENDITURES'/5X,'(EXCLUDING AFUDC)',23X, &
             10(F9.0,2X)/3X,'COMMON STOCK DIVIDENDS',20X,10(F9.0,2X) &
             /3X,'PREFERRED STOCK DIVIDENDS',17X,10(F9.0,2X)/3X, &
             'CHANGE IN WORKING CAPITAL',17X,10(F9.0,2X)/3X, &
             'CHANGE IN NUCLEAR FUEL STOCK',14X,10(F9.0,2X)/45X, &
             10(9('-'),2X)/5X,'TOTAL APPLICATIONS',22X,10(F9.0,2X)/ &
             45X,10(9('-'),2X)/45X,10(9('-'),2X)///1X, &
             'INTERNAL CASH FLOW AS A PERCENT'/3X, &
             'OF CONSTRUCTION EXPENDITURES',14X,10(F9.2,2X)/'1'/)
      IYR1 = IYR1 + 10
      IYR2 = IYR1 + 9
      GO TO 520
  550   IF (IYR1 .LE. LASTYR) THEN
         ID = IYR2 - LASTYR
         WRITE(IOUT,530)NAME1,NAMER,NAME2,NAME3,(IYRS(I),I=IYR1,IYR2), &
                        ((TFUNDS(I,K),I=IYR1,LASTYR), &
                        (D(L),L=1,ID),K=1,14)
         WRITE(IOUT,540)((TFUNDS(I,K),I=IYR1,LASTYR), &
                        (D(L),L=1,ID),K=15,21)
        ENDIF
      ENDIF
!     *****PRINT REVENUE REQUIREMENT*****
      IF (IPRPT(7) .EQ. 1) THEN
        IF (ITYPE .NE. 3) THEN
         IF (OPAVRB(ITYPE) .NE. 1) THEN
            DO I=1,5
               NAME4 = NAME5
            END DO
         ENDIF
        ENDIF
      WRITE(IOUT,620)
  620 FORMAT(' ',20X,'*****  PRO FORMA REVENUE REQUIREMENT  *****', &
             5(/),75X,'YEAR ENDED DECEMBER 31'/45X,105('-')/)
      IYR1 = IYRRPT
      IYR2 = IYR1 + 9
  630 IF (IYR2 .GT. LASTYR) GO TO 660
      WRITE(IOUT,640)NAME1,NAMER,NAME2,NAME3,(IYRS(I),I=IYR1,IYR2), &
                     ((TREVRE(I,K),I=IYR1,IYR2),K=1,7)
  640 FORMAT(/1X,A16,1X,A16/1X,A50/1X,A71/50X,10(I4,7X)/45X,10(9('-'), &
             2X)/75X,'(DOLLARS IN MILLIONS)'//1X, &
             'UTILITY PLANT IN SERVICE',20X,10(F9.0,2X)/1X, &
             'ADDITIONS:'/5X,'CONSTRUCTION WORK IN PROGRESS',11X, &
             10(F9.0,2X)/5X,'NUCLEAR FUEL',28X,10(F9.0,2X)/5X, &
             'WORKING CAPITAL',25X,10(F9.0,2X)/1X,'DEDUCTIONS:'/5X, &
             'ACCUMULATED DEPRECIATION',15X,10('(',F9.0,')')/5X, &
             'DEFERRED TAXES & NET GAIN ON S/L',7X,10('(',F9.0,')')/ &
             45X,10(9('-'),2X)/8X,'YEAR END RATE BASE',19X, &
             10(F9.0,2X)/45X,10(9('-'),2X)/45X,10(9('-'),2X)/)
      WRITE(IOUT,650)NAME4,((TREVRE(I,K),I=IYR1,IYR2),K=8,24)
  650 FORMAT(1X,'REVENUE REQUIREMENT'//1X,A20,24X,10(F9.0,2X)/3X, &
             'RATE OF RETURN ON RATE BASE',15X,10(F9.3,2X)/45X, &
             10(9('-'),2X)/1X,'RETURN ON RATE BASE',25X,10(F9.0,2X)/ &
             45X,10(9('-'),2X)/1X,'EXPENSES:'/3X, &
             'OPERATION & MAINTENANCE',19X,10(F9.0,2X)/3X,'FUEL', &
             38X,10(F9.0,2X)/3X,'DEPRECIATION',30X,10(F9.0,2X)/3X, &
             'LEASE PAYMENTS (NET OF GAIN)',15X,10(F9.0,2X)/3X, &
             'GENERAL TAXES',29X,10(F9.0,2X)/3X,'STATE TAXES',31X, &
             10(F9.0,2X)/3X,'FEDERAL INCOME TAXES (NET OF ITC)',9X, &
             10(F9.0,2X)/3X,'EXCESS DEF TAXES FROM TAX RATE CHANGE', &
             4X,10('(',F9.0,')')/3X,'ITC-AMORTIZED',28X,10('(',F9.0, &
             ')')/3X,'TAX SAVINGS FROM AFUDCD-FLOWED THROUGH',3X,10( &
             '(',F9.0,')')/3X,'TAX SAVINGS FROM AFUDCD-AMORTIZED', &
             8X,10('(',F9.0,')')/3X,'DEFERRED PHASE-IN REVENUES', &
             15X,10('(',F9.0,')')/45X,10(9('-'),2X)/10X, &
             'TOTAL EXPENSES',21X,10(F9.0,2X)/45X,10(9('-'),2X)/1X, &
             'REVENUE REQUIREMENTS',24X,10(F9.0,2X)/45X,10(9('-'), &
             2X)/45X,10(9('-'),2X)/'1'/)
      IYR1 = IYR1 + 10
      IYR2 = IYR1 + 9
      GO TO 630
  660   IF (IYR1 .LE. LASTYR) THEN
         ID = IYR2 - LASTYR
         WRITE(IOUT,640)NAME1,NAMER,NAME2,NAME3,(IYRS(I),I=IYR1,IYR2), &
                        ((TREVRE(I,K),I=IYR1,LASTYR), &
                        (D(L),L=1,ID),K=1,7)
         WRITE(IOUT,650)NAME4,((TREVRE(I,K),I=IYR1,LASTYR), &
                        (D(L),L=1,ID),K=8,24)
        ENDIF
      ENDIF
      RETURN
      END
      SUBROUTINE REPSML(ITYPE,NRGN,INDOC)
      IMPLICIT NONE
!*****************************************************************
!         THIS SUBROUTINE PRINTS THE SMALL REPORT
!*****************************************************************
      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'efpwrt'
      include 'efpgen'
      include 'efpname'
      include 'efpcntrl'
      include 'eusprc'
      include 'efpint'
      include 'efpout'
      include 'macout'
      include 'pq'
      include 'control'
      include 'uefdout'
      INTEGER NRGN,ICL
      CHARACTER*14 TITLE
      CHARACTER*25 SECT
      CHARACTER*14 CNAME(5)
      INTEGER I
      INTEGER II
      INTEGER L
      INTEGER ILOOP
      INTEGER ISTRT
      INTEGER IEND
      INTEGER J
      INTEGER K
      INTEGER ITYPE
      INTEGER INDOC
      INTEGER IYR,IYR1,IYR2,ID
      INTEGER IMISC
      REAL*4 TEMP(5)
      REAL*4 PV(5)
      REAL*4 CPV(5)
      REAL*4 PPRICE(MNUMYR)
      REAL*4 COVRAT(MNUMYR)
      REAL*4 COVRT2
      REAL*4 AFCPER(MNUMYR)
      REAL*4 DIRECT
      REAL*4 AFDC
      REAL*4 TOTCMW
      REAL*4 TOTAL
      REAL*4 CUMDIR
      REAL*4 BV
      REAL*4 BVNET
      REAL*4 EARN
      REAL*4 ROEIMP
      REAL*4 REVRES
      REAL*4 REVULT
      REAL*4 RPRICE
      REAL*4 FUEL
      REAL*4 OM
      REAL*4 WHO, PPWS, TOTWS
      REAL*4 FUELTTL, OMTTL, CAPTTL
      REAL*4 TOTCAP2
      REAL*4 DEP
      REAL*4 TAXESI
      REAL*4 TAXESO
      REAL*4 RET
      REAL*4 COVRT3
      REAL*4 ROE
      REAL*4 CWIPNT
      REAL*4 EFTXRT
      REAL*4 SAFETY
      REAL*4 TOTANN
      REAL*4 COVRT1(MNUMYR)
      REAL*4 NATPRC(3)
      REAL*4 EPIS(MNUMYR),EPISPS(MNUMYR) ! ELECTRIC PLANT IN SERVICE
      REAL*4 EPAD(MNUMYR),EPADPS(MNUMYR)  ! ACCUMULATED DEPRECIATION
      REAL*4 BVNETO
      REAL*4 BVNETP
      REAL*4 EP
      REAL*4 AD
      REAL*4 ADR
      DATA CNAME/'RESIDENTIAL   ','COMMERCIAL    ', &
                 'INDUSTRIAL    ','TRANSPORTATION','TOTAL         '/
      IOUT = UF_EFPRPT
! PRINT CAPITAL REQUIREMENTS REPORT
      IF (IPRPT(8) .EQ. 1) THEN
        DO K=1,NOCAP
! DETERMINE IF THIS REPORT INCLUDES MISCELLANEOUS BUILDS
         IMISC=0
         DO I=1,NUMCAP(K)
            IF (CAPTYP(K,I) .EQ. EIMISC) IMISC=1
         ENDDO
         WRITE(IOUT,5)NAME1,NAMER,NAME2,NAME3,CAPTIT(K)
    5    FORMAT('1',/1X,A16,1X,A16/1X,A50/1X,A71,5(/),40X, &
                'CAPITAL REQUIREMENTS',' REPORT FOR'/,31X,A50/// &
                110X,'DIRECT CAPITAL EXPENDITURES'/10X, &
                'CWIP',3X,'MW COMPLETED',2X,'CUMULATIVE',7X, &
                'ANNUAL CAPITAL EXPEND.',7X,'CUM. CAPITAL EXPEND.', &
                12X,'NEW',11X,'EXISTING',3X,'IN-SERV. BOOK VALUE'/ &
                1X,'YEAR',5X,'(MW)',5X,'THIS YEAR',3X, &
                'MW COMPLETED',4X,'DIRECT',4X,'AFUDC',3X,'TOTAL', &
                5X,'DIRECT',4X,'AFUDC',4X,'TOTAL',10X,'BUILDS', &
                8X,'PLANTS',7X,'TOT. BV',5X,'$/KW')
         DIRECT = 0.0
         AFDC = 0.0
         TOTCMW = 0.0
         TOTAL = 0.0
         CUMDIR = 0.0
         DO I=IYRRPT,LASTYR
! CONVERT CAPACITY FROM GW TO MW
            TCAPRE(I,K,2) = TCAPRE(I,K,2)*1000.0
            TCAPRE(I,K,1) = TCAPRE(I,K,1)*1000.0
            DIRECT = TCAPRE(I,K,3) + TCAPRE(I,K,6)
            TOTANN = DIRECT + TCAPRE(I,K,4)
            TOTCMW = TOTCMW + TCAPRE(I,K,2)
            CUMDIR = CUMDIR + DIRECT
            AFDC = AFDC + TCAPRE(I,K,4)
            TOTAL = TOTAL + TOTANN
            IF (TCAPRE(I,K,2) .NE. 0.0) THEN
               BV = (TCAPRE(I,K,5)/TCAPRE(I,K,2))*1000.0
            ELSE
               BV = 0.0
            ENDIF
!
!  pw2 need capital expenditures  only done if combined public private and gen only
!co
!           IF (ITYPE .eq. 3 .and. INDOC .eq. 1 .and.
!    1  CAPTIT(K)  .eq. 'TOTAL PROD.' ) THEN
!             COMPCAP(NRGN,I) = TOTANN
!             IF(NRGN .LE. UNRGNS)
!    1        COMPCAP(MNUMNR,I) = COMPCAP(MNUMNR,I) + TOTANN
!           ENDIF
            WRITE(IOUT,10)IYRS(I),TCAPRE(I,K,1),TCAPRE(I,K,2), &
                          TOTCMW,DIRECT,TCAPRE(I,K,4),TOTANN,CUMDIR, &
                          AFDC,TOTAL,TCAPRE(I,K,3),TCAPRE(I,K,6), &
                          TCAPRE(I,K,5),BV
   10       FORMAT(1X,I4,2X,F9.0,4X,F8.0,4X,F9.0,1X, &
                   2(2X,F8.0,1X,F8.0,2X,F8.0),4X,F10.0,3X, &
                   F10.0,4X,F10.0,4X,F8.2)
         END DO
! PRINT ELECTRIC PLANT IN SERVICE REPORT
         WRITE(IOUT,6)NAME1,NAMER,NAME2,NAME3,CAPTIT(K)
    6    FORMAT('1',/1X,A16,1X,A16/1X,A50/1X,A71,5(/),30X, &
       'ELECTRIC PLANT IN SERVICE REPORT FOR'/,31X,A50/// &
       ,6X,'-----------ORIGINAL PLANT----------',1X, &
       '----POST SERVICE ADDITIONS---',1X, &
       '---------TOTAL PLANT---------'/, &
       7X,'PLANT IN',5X,'ACCUM',8X,'NET BOOK', &
       2X,'PLANT IN',2X,'ACCUM',5X,'NET BOOK', &
       2X,'PLANT IN',1X,'ACCUM',5X,'NET BOOK'/, &
       1X,'YEAR',2X,'SERVICE',6X,'DEPREC',6X,' VALUE', &
       5X,'SERVICE',3X,'DEPREC',3X,' VALUE', &
       5X,'SERVICE',2X,'DEPREC',3X,' VALUE')
! CALCULATE POST SERVICE RESULTS
!      WRITE(6,*) 'K = ',K,' INDOC = ',INDOC, ' ITYPE = ',ITYPE
         DO I=IYRRPT,LASTYR
            EPIS(I) = 0.0
            EPAD(I) = 0.0
         ENDDO
         DO I=IYRRPT,LASTYR
            DO J=1,ESBKLF(EIMISC,1)
               IYR=I+J-1
               IF (IYR .LE. LASTYR) THEN
                  ADR = FLOAT(J)/ESBKLF(EIMISC,1)
                  EPIS(IYR) = EPIS(IYR) + TCAPRE(I,K,6) * GINFR(I)
                  EPAD(IYR) = EPAD(IYR) + ADR * TCAPRE(I,K,6) * GINFR(I)
               ENDIF
            ENDDO
            IF (K .EQ. 1) THEN
               EPISPS(I) = EPIS(I) / GINFR(I)
               EPADPS(I) = EPAD(I) / GINFR(I)
            ENDIF
         ENDDO
! WRITE REPORT
         DO I=IYRRPT,LASTYR
            IF (IMISC .EQ. 1) THEN
               TCAPRE(I,K,7) = TCAPRE(I,K,7) - EPISPS(I)
               TCAPRE(I,K,8) = TCAPRE(I,K,8) - EPADPS(I)
            ENDIF
            BVNETO=TCAPRE(I,K,7)-TCAPRE(I,K,8)
            BVNETP = EPIS(I) - EPAD(I)
            EP = TCAPRE(I,K,7) + EPIS(I)
            AD = TCAPRE(I,K,8) + EPAD(I)
            BV = BVNETO + BVNETP
            WRITE(IOUT,10)IYRS(I),TCAPRE(I,K,7),TCAPRE(I,K,8), &
             BVNETO,EPIS(I),EPAD(I),BVNETP,EP,AD,BV
         END DO
        END DO
      END IF
! ***** PRINT COST OF CAPITAL REPORT
      IF (IPRPT(9) .EQ. 1) THEN
      WRITE(IOUT,40)NAME1,NAMER,NAME2,NAME3
   40 FORMAT('1',/1X,A16,1X,A16/1X,A50/1X,A71,/,1X, &
             T30,'COST OF CAPITAL (PERCENT)',T88, &
             'CAPITAL STRUCTURE (PERCENT)',//,T5,'YEARS',T10,'*', &
             T11,'ALLOWED RETURN ON',T31,'PREFERRED STOCK',T52, &
             'LONG TERM DEBT',T69,'SHORT TERM',T79,'*',T82,'COMMON', &
             T91,'PREFERRED',T101,'LONG TERM',T111,'SHORT TERM', &
             T122,'*',T123,'RATE OF',/,T10,'*',T13,'COMMON EQUITY', &
             T29,'EMBEDDED',2X,'MARGINAL',T50,'EMBEDDED',2X, &
             'MARGINAL',T72,'DEBT',T79,'*',T83,'STOCK',T94,'STOCK', &
             T104,'DEBT',T114,'DEBT',T122,'*',T123,'RETURN',/)
      DO I=IYRRPT,LASTYR
         DO J=1,11
            TCSTCA(I,J) = TCSTCA(I,J)*100.0
         END DO
         WRITE(IOUT,60)IYRS(I),(TCSTCA(I,J),J=1,11)
   60    FORMAT(T6,I4,T10,'*',T15,F8.2,T29,F8.2,2X,F8.2,T50,F8.2,2X, &
                F8.2,T70,F8.2,T79,'*',T80,F8.2,T92,F8.2,T102,F8.2, &
                T112,F8.2,T122,'*',T123,F8.2)
      END DO
      END IF
! PRINT CANCELLED NUCS REPORT
      IF (IPRPT(10) .EQ. 1) THEN
      WRITE(IOUT,70)NAME1,NAMER,NAME2,NAME3
   70 FORMAT('1',/1X,A16,1X,A16/1X,A50/1X,A71,//,42X, &
             'REPORT ON CANCELLED PLANTS',//,1X,5X,'YEAR',18X, &
             'GROSS BOOK',10X,'ACCUMULATED',11X,'NET BOOK',12X, &
             'EARNINGS *',11X,'ROE % *',/,31X,'VALUE',12X, &
             'DEPRECIATION',12X,'VALUE',14X,'IMPACT',13X,'IMPACT',/)
      DO I=IYRRPT,LASTYR
         BVNET = TCANPL(I,1) - TCANPL(I,2)
         EARN = -1.0*(BVNET * TREVRE(I,9))
         IF (TCANPL(I,3) .EQ. 0.0) THEN
            ROEIMP = 0.0
         ELSE
            ROEIMP = (EARN/TCANPL(I,3))*100.0
         ENDIF
         WRITE(IOUT,80)IYRS(I),(TCANPL(I,J),J=1,2),BVNET,EARN,ROEIMP
   80    FORMAT(1X,5X,I4,20X,5(F8.2,12X))
      END DO
      WRITE(IOUT,100)
  100 FORMAT(/,1X,5X,'----------------',/,1X,5X,'* ASSUMES END', &
             ' OF YEAR RATEBASE')
      END IF
! PRINT ELECTRICITY PRICES
      IF (IPRPT(11) .EQ. 1) THEN
      WRITE(IOUT,110)NAME1,NAMER,NAME2,NAME3
  110 FORMAT('1',/1X,A16,1X,A16/1X,A50/1X,A71,4(/),1X,34X, &
             'ELECTRIC REVENUES',14X,'TOTAL SALES',6X, &
             'AVERAGE PRICE OF ELECTRICITY (MILLS/KWH)',/1X,13X, &
             'TOTAL',9X,'FROM SALES',7X,'FROM SALES',11X, &
             'TO ULTIMATE',20X,'TO ULTIMATE'/1X,'YEAR',22X, &
             'TO  ULTIMATE',6X,'FOR RESALE',15X,'(GWH)',17X, &
             'NOMINAL',10X,'REAL')
      DO I=IYRRPT,LASTYR
         REVRES = 0.0
         REVULT = TXINCS(I,1) - REVRES
         PPRICE(I) = REVULT/TSALES(I,1)
         RPRICE = PPRICE(I)/GINFR(I)
         IF (IREAL .EQ. 1) RPRICE = PPRICE(I)
         WRITE(IOUT,120)IYRS(I),TXINCS(I,1),REVULT,REVRES, &
                        TSALES(I,1),PPRICE(I),RPRICE
  120    FORMAT(1X,I4,5X,3(F12.3,5X),F13.4,17X,F7.2,9X,F7.2)
      END DO
      ENDIF
! PRINT COMPONENTS OF PRICE (PERCENT)
      IF (IPRPT(12) .EQ. 1) THEN
      WRITE(IOUT,140)NAME1,NAMER,NAME2,NAME3
  140 FORMAT('1',/1X,A16,1X,A16/1X,A50/1X,A71,3(/),47X, &
             'COMPONENTS OF PRICE','(PERCENT)'//1X,82X, &
             'CAPITAL RELATED COMPONENTS'/1X,8X,'PRICE',26X, &
             '  WHOLE  ',5X,'CAPITAL',17X,'BOOK',6X,'RETURN TO',5X, &
             'INCOME',7X,'OTHER'/1X,'YEAR',1X,'(MILLS/KWH)',2X, &
             'FUEL',9X,'OM',7X,' SALE ',7X,'RELATED',13X, &
             'DEPRECIATION',2X,'INVESTORS',6X,'TAXES',7X,'TAXES')
      DO I=IYRRPT,LASTYR
         REVULT = TXINCS(I,1)/100.0
         FUEL = (TXINCS(I,3) - TSALES(I,2))/REVULT
         OM = TXINCS(I,2)/REVULT
         WHO = TSALES(I,2)/REVULT
         TOTCAP2 = 100.0 - FUEL - OM - WHO
         DEP = TXINCS(I,4)/REVULT
         TAXESI = TXINCS(I,7)/REVULT
         TAXESO = TXINCS(I,6)/REVULT
         RET = TOTCAP2 - DEP - TAXESI - TAXESO
         WRITE(IOUT,150)IYRS(I),PPRICE(I),FUEL,OM,WHO,TOTCAP2,DEP,RET, &
                        TAXESI,TAXESO
  150    FORMAT(1X,I4,3X,F6.2,2(4X,F6.2,2X),4X,F6.2,2X, &
                4X,F6.2,2X,12X,4(4X,F6.2,2X))
      END DO
!     ENDIF
! PRINT COMPONENTS OF PURCHASED POWER PRICE (PERCENT)
!     IF (IPRPT(12) .EQ. 1) THEN
      WRITE(IOUT,141)NAME1,NAMER,NAME2,NAME3
  141 FORMAT('1',/1X,A16,1X,A16/1X,A50/1X,A71,3(/),2X, &
             'COMPONENTS OF PURCHASED POWER PRICE','(PERCENT)',20X, &
             'COMPONENTS OF PRICE, UTIL + NONUTIL','(PERCENT)' &
             //1X,82X &
             /1X,8X,'PRICE',26X, &
             8X,'CAPITAL',2X,20X,'PRICE',26X,3X,'CAPITAL' &
             /1X,'YEAR',1X,'(MILLS/KWH)',2X, &
             'FUEL',9X,'OM',7X,3X,'RELATED',11X, &
             2X,'YEAR',1X,'(MILLS/KWH)',6X,'FUEL',8X,'OM', &
             7X,'RELATED' &
             )
      DO I=IYRRPT,LASTYR
         REVULT = TXINCS(I,1)/100.0
         FUEL = TPPWRBL(1,I)/REVULT
         OM = TPPWRBL(2,I)/REVULT
         WHO = TSALES(I,2)/REVULT
         FUELTTL = (TXINCS(I,3) - TSALES(I,2) + TPPWRBL(1,I)) / &
                    REVULT
         OMTTL = (TXINCS(I,2) + TPPWRBL(2,I)) / REVULT
         CAPTTL = (100.0*REVULT-(TXINCS(I,3)-TSALES(I,2))-TXINCS(I,2)- &
                   TSALES(I,2) + TPPWRBL(3,I)) / REVULT
         TOTCAP2 = TPPWRBL(3,I)/REVULT
         WRITE(IOUT,151) IYRS(I),WHO,FUEL,OM,TOTCAP2 &
                        ,IYRS(I),PPRICE(I),FUELTTL,OMTTL,CAPTTL
  151    FORMAT(1X,I4,3X,F6.2,2(4X,F6.2,2X), &
                2X,F6.2,2X,14X,I4,4(4X,F6.2,2X))
      END DO
      ENDIF
! ***** PRINT COMPONENTS OF PRICE (MILLS/KWH)
      IF (IPRPT(13) .EQ. 1) THEN
      WRITE(IOUT,170)NAME1,NAMER,NAME2,NAME3
  170 FORMAT('1',/1X,/1X,A16,1X,A16/1X,A50/1X,A71,3(/),47X, &
             'COMPONENTS OF PRICE','(MILLS/KWH)'//1X,82X, &
             'CAPITAL RELATED COMPONENTS'/1X,8X,'PRICE',26X, &
             '  WHOLE  ',5X,'CAPITAL',17X,'BOOK',6X,'RETURN TO',5X, &
             'INCOME',7X,'OTHER'/1X,'YEAR',1X,'(MILLS/KWH)',2X, &
             'FUEL',9X,'OM',7X,' SALE ',7X,'RELATED',13X, &
             'DEPRECIATION',2X,'INVESTORS',6X,'TAXES',7X,'TAXES')
      DO I=IYRRPT,LASTYR
         FUEL = (TXINCS(I,3) - TSALES(I,2))/TSALES(I,1)
         OM = TXINCS(I,2)/TSALES(I,1)
         WHO = TSALES(I,2)/TSALES(I,1)
         TOTCAP2 = PPRICE(I) - FUEL - OM - WHO
         DEP = TXINCS(I,4)/TSALES(I,1)
         TAXESI = TXINCS(I,7)/TSALES(I,1)
         TAXESO = TXINCS(I,6)/TSALES(I,1)
         RET = TOTCAP2 - DEP - TAXESI - TAXESO
         WRITE(IOUT,180)IYRS(I),PPRICE(I),FUEL,OM,WHO,TOTCAP2,DEP,RET, &
                        TAXESI,TAXESO
  180    FORMAT(1X,I4,3X,F6.2,2(4X,F6.2,2X),4X,F6.2, &
                2X,4X,F6.2,2X,12X,4(4X,F6.2,2X))



      END DO


!     ENDIF
! PRINT COMPONENTS OF PURCHASED POWER PRICE (MILLS/KWH)
!     IF (IPRPT(13) .EQ. 1) THEN
      WRITE(IOUT,142)NAME1,NAMER,NAME2,NAME3
  142 FORMAT('1',/1X,A16,1X,A16/1X,A50/1X,A71,3(/),2X, &
             'COMPONENTS OF PURCHASED POWER PRICE','(MILLS/KWH)' &
        ,18X,'COMPONENTS OF PRICE, UTIL + NONUTIL','(MILLS/KWH)' &
             //1X,82X &
             /1X,8X,'PRICE',23X,'PURCH', &
             3X,'CAPITAL',19X,'PRICE',27X,'PURCH',4X,'CAPITAL' &
             /1X,'YEAR',1X,'(MILLS/KWH)',2X, &
             'FUEL',5X,'OM',7X,'POWER',3X,'RELATED',11X &
             ,'YEAR',2X,'(MILLS/KWH)',2X,'FUEL',8X,'OM',7X, &
             'POWER',4X,'RELATED' &
             )
      DO I=IYRRPT,LASTYR
         FUEL = TPPWRBL(1,I)/TSALES(I,1)
         OM = TPPWRBL(2,I)/TSALES(I,1)
         WHO = TSALES(I,2)/TSALES(I,1)
         PPWS = TPPWRBL(4,I)/TSALES(I,1)
         FUELTTL = (TXINCS(I,3)-TSALES(I,2) + TPPWRBL(1,I))/ &
                   TSALES(I,1)
         OMTTL = (TXINCS(I,2) + TPPWRBL(2,I)) / TSALES(I,1)
         CAPTTL = PPRICE(I)-((TXINCS(I,3)-TSALES(I,2)) / &
                  TSALES(I,1)) &
                  - TXINCS(I,2)/TSALES(I,1) &
                  - TSALES(I,2)/TSALES(I,1) &
                  + TPPWRBL(3,I) / TSALES(I,1)
         TOTCAP2 = TPPWRBL(3,I)/TSALES(I,1)
         TOTWS = TPPWRBL(4,I)/TSALES(I,1)
         WRITE(IOUT,152)IYRS(I),WHO,FUEL,OM,PPWS,TOTCAP2 &
                       ,IYRS(I),PPRICE(I),FUELTTL,OMTTL,TOTWS,CAPTTL
  152    FORMAT(1X,I4,2X,F6.2,4(2X,F6.2,2X), &
                10X,I4,2X,5(2X,F6.2,2X))
      END DO
      ENDIF
! ***** PRINT FINANCIAL RATIOS 1
      IF (IPRPT(14) .EQ. 1) THEN
      WRITE(IOUT,200)NAME1,NAMER,NAME2,NAME3
  200 FORMAT('1',/1X,A16,1X,A16/1X,A50/1X,A71/,4(/),1X, &
             'AFUDC AS A % OF EARNINGS AVAILABLE FOR COMMON',7X, &
             'INTEREST COVERAGE CALCULATIONS',22X, &
             'COMMON STOCK INFORMATION',/45X,10X,'PRE-TAX',18X, &
             'POST-TAX',16X,'(PRIVATES ONLY)',/1X,'YEAR',4X, &
             'AFUDC',4X,'AVAILABLE EARNINGS',4X,'%',5X,3X, &
             2('LESS AFUDC',3X,'WITH AFUDC',3X),4X, &
             'AVERAGE BOOK VALUE',5X,'AVERAGE EARNED ROE(%)'/)
      DO I=IYRRPT,LASTYR
         COVRAT(I) = (TXINCS(I,9) + TXINCS(I,7))/(TXINCS(I,12) &
                     + TXINCS(I,13))
         COVRT1(I) = (TXINCS(I,9) + TXINCS(I,7) + TFUNDS(I,6)) &
                     /(TXINCS(I,12) + TXINCS(I,13))
         COVRT2 = TXINCS(I,9)/(TXINCS(I,12) + TXINCS(I,13))
         COVRT3 = (TXINCS(I,9) + TFUNDS(I,6))/(TXINCS(I,12) &
                  + TXINCS(I,13))
         AFCPER(I) = 0.0
         IF ((TFUNDS(I,6) .GE. 0.0) .AND. (TXINCS(I,20) .GT. 0.0)) &
            AFCPER(I)=TFUNDS(I,6)/TXINCS(I,20)*100.0
         IF (ITYPE .NE. 1) THEN
            WRITE(IOUT,210)IYRS(I),TFUNDS(I,6),TXINCS(I,20),AFCPER(I), &
                           COVRAT(I),COVRT1(I),COVRT2,COVRT3,'N/A', &
                           'N/A'
  210       FORMAT(1X,I4,2X,F8.0,7X,F10.0,5X,F6.2,4(7X,F6.4), &
                   14X,A9,16X,A9)
         ELSE
           ROE = (TXINCS(I,20)/TCANPL(I,3))*100.0
           WRITE(IOUT,220)IYRS(I),TFUNDS(I,6),TXINCS(I,20),AFCPER(I), &
                          COVRAT(I),COVRT1(I),COVRT2,COVRT3, &
                          TCANPL(I,3),ROE
  220      FORMAT(1X,I4,2X,F8.0,7X,F10.0,5X,F6.2,4(7X,F6.4), &
                  14X,F9.0,16X,F6.2)
         ENDIF
      END DO
      ENDIF
! **** PRINT FINANCIAL RATIOS 2 (OTHER MISCELLANEOUS RATIOS)
      IF (IPRPT(15) .EQ. 1) THEN
      WRITE(IOUT,240)NAME1,NAMER,NAME2,NAME3
  240 FORMAT('1',/1X,A16,1X,A16/1X,A50/1X,A71/,4(/),1X,'YEAR',5X, &
             'INTERNAL CASH AS A % OF', &
             ' CONSTRUCTION',5X,'CWIP AS A % OF NET PLANT',5X, &
             'EFFECTIVE INCOME TAX RATE',5X,'SAFETY MARGIN'/)
      DO I=IYRRPT,LASTYR
         CWIPNT = 0.0
         IF (TBALSH(I,11) .NE. 0.0) &
            CWIPNT = TBALSH(I,5)/TBALSH(I,11)*100.0
         EFTXRT = TXINCS(I,7)/(TXINCS(I,7) + TXINCS(I,9))*100.0
         SAFETY = (TXINCS(I,16) + TXINCS(I,7) - TFUNDS(I,6))/ &
                  TXINCS(I,1)*100.0
         WRITE(IOUT,250)IYRS(I),TFUNDS(I,21),CWIPNT,EFTXRT,SAFETY
  250    FORMAT(1X,I4,22X,F6.2,29X,F6.2,20X,F6.2,20X,F6.2)
      END DO
      ENDIF
! THE FOLLOWING REPORTS ARE WRITTEN OUT AFTER FINAL CUSTOMER AND
! STAGE OF PRODUCTION
! **** PRINT BULK POWER REPORT
      IF ((INDOC .EQ. 4) .AND. (ITYPE .EQ. 3)) THEN
        IF (IPRPT(16) .EQ. 1) THEN
         WRITE(IOUT,260)NAME1,NAME3
  260    FORMAT('1',/1X,A16,/,1X,A71/,4(/), &
                T50,'BULK POWER SALES',/,/, &
                T7,'   FIRM POWER SALES  ',T29,'    ECONOMY SALES    ', &
                T51,'CANADA/MEXICO IMPORTS',T73,'       EXPORTS       ', &
                T95,'         EWGS        ',/,T7, &
                '---------------------',T29,'---------------------', &
                T51,'---------------------',T73,'---------------------', &
                T95,'---------------------',/,T2,'YEAR',T7, &
                ' QUANTITY     PRICE  ',T29,' QUANTITY     PRICE  ',T51, &
                ' QUANTITY     PRICE  ',T73,' QUANTITY     PRICE  ',T95, &
                ' QUANTITY     PRICE  ',/,T7,'---------   ---------', &
                T29,'---------   ---------',T51,'---------   ---------', &
                T73,'---------   ---------',T95,'---------   ---------')
         DO I=IYRRPT,LASTYR
            DO J=1,5
               IF (TEFPBL(J,2,I) .EQ. 0.0) THEN
                  TEMP(J) = 0.0
               ELSE
                  TEMP(J) = TEFPBL(J,1,I)/TEFPBL(J,2,I)
               ENDIF
            END DO
            WRITE(IOUT,270)IYRS(I),(TEFPBL(J,2,I), &
                           TEMP(J),J=1,5)
  270       FORMAT(T2,I4,T7,5(F9.2,3X,F9.2,1X))
         END DO
       ENDIF
      ENDIF
! ***** PRINT DEMAND QUANTITIES
      IF ((INDOC .EQ. 4) .AND. (ITYPE .EQ. 3)) THEN
       IF (IPRPT(17) .EQ. 1) THEN
         ILOOP = (LASTYR - IYRRPT + 1)/6
         IF ((ILOOP*6) .NE. (LASTYR-IYRRPT+1)) ILOOP = ILOOP + 1
         DO I=1,ILOOP
            ISTRT = ((I - 1)*6) + IYRRPT
            IEND = ISTRT + 5
            IF (IEND .GT. LASTYR) IEND = LASTYR
            WRITE(IOUT,280)NAME1,NAME3,(IYRS(J),J=ISTRT,IEND)
  280       FORMAT('1',/1X,A16,/,1X,A71/,4(/), &
                   T50,'DEMAND QUANTITIES',/,/, &
                   T23,6('-------',I4,'------',1X),/, &
                   T23,6(' QUANTITY    %   ',1X),/, &
                   T23,6('-----------------',1X))
            DO J=1,NTECH+1
               IF (J .EQ. (NTECH+1)) THEN
                  WRITE(IOUT,285)
  285             FORMAT(' SYSTEM LOAD FACTOR')
               ELSE
                  WRITE(IOUT,290)TECNAM(J)
  290             FORMAT(T2,A25)
               ENDIF
               DO K=1,NCLASS
                  IF (K .EQ. 1) TITLE = 'RESIDENTIAL   '
                  IF (K .EQ. 2) TITLE = 'COMMERCIAL    '
                  IF (K .EQ. 3) TITLE = 'INDUSTRIAL    '
                  IF (K .EQ. 4) TITLE = 'TRANSPORTATION'
                  IF (J .LE. (NTECH-2)) THEN
                     WRITE(IOUT,300)TITLE,(TDEMRE(L,J,K,1), &
                                    TDEMRE(L,J,K,2),L=ISTRT,IEND)
  300                FORMAT(T2,A20,1X,6(F10.2,1X,F6.2,1X))
                  ELSE
                     IF (J .LE. NTECH) THEN
                        WRITE(IOUT,310)TITLE,(TDEMRE(L,J,K,2), &
                                       L=ISTRT,IEND)
  310                   FORMAT(T2,A20,1X,6(11X,F6.2,1X))
                     ELSE
                        WRITE(IOUT,320)TITLE,(TDEMRE(L,8,K,1), &
                                       L=ISTRT,IEND)
  320                   FORMAT(T2,A20,1X,6(F10.2,8X))
                     ENDIF
                  ENDIF
               END DO
            END DO
         END DO
       ENDIF
! ***** PRINT ALLOCATION FACTORS
       IF (IPRPT(18) .EQ. 1) THEN
         DO II=1,4
         IF (II .EQ. 1) SECT = 'GENERATION'
         IF (II .EQ. 2) SECT = 'TRANSMISSION'
         IF (II .EQ. 3) SECT = 'DISTRIBUTION'
         IF (II .EQ. 4) SECT = 'ALL COMPONENTS'
         ILOOP = NCOST/4
         IF ((ILOOP*4) .NE. NCOST) ILOOP = ILOOP + 1
         DO I=1,ILOOP
            ISTRT = ((I - 1)*4) + 1
            IEND = ISTRT + 3
            IF (IEND .GT. NCOST) IEND = NCOST
            WRITE(IOUT,340)NAME1,NAME3,SECT,(CCATNAM(J),J=ISTRT,IEND)
  340       FORMAT('1',/1X,A16,/,1X,A71/,1X,A25,4(/), &
                   T50,'ALLOCATION FACTORS (%)',/,/, &
                   T7,4('---',A25,'---',1X),/, &
                   T7,4(' RES.    COM.    IND.   TRANS. ', &
                   1X),/,T7, &
                   4('------- ------- ------- -------',1X))
            DO J=IYRRPT,LASTYR
               WRITE(IOUT,350)IYRS(J), &
                              ((TALLRE(J,L,K,II),K=1,4),L=ISTRT,IEND)
  350          FORMAT(T2,I4,1X,4(4(F7.2,1X)))
            END DO
         END DO
         END DO
      ENDIF
! ***** PRINT COST POOLS
      IF (IPRPT(19) .EQ. 1) THEN
         DO II=1,4
         IF (II .EQ. 1) SECT = 'GENERATION'
         IF (II .EQ. 2) SECT = 'TRANSMISSION'
         IF (II .EQ. 3) SECT = 'DISTRIBUTION'
         IF (II .EQ. 4) SECT = 'ALL COMPONENTS'
         ILOOP = NCOST/4
         IF ((ILOOP*4) .NE. NCOST) ILOOP = ILOOP + 1
         DO I=1,ILOOP
            ISTRT = ((I - 1)*4) + 1
            IEND = ISTRT + 3
            IF (IEND .GT. NCOST) IEND = NCOST
            WRITE(IOUT,355)NAME1,NAME3,SECT,(CCATNAM(J),J=ISTRT,IEND)
  355       FORMAT('1',/1X,A16,/,1X,A71/,1X,A25,4(/), &
                   T70,'COST POOLS',/,/, &
                   T7,4('---',A25,'---',1X))
            DO J=IYRRPT,LASTYR
               WRITE(IOUT,360)IYRS(J), &
                              (TCSTRE(J,K,II),K=ISTRT,IEND)
  360          FORMAT(T2,I4,1X,4(14X,F17.2,1X))
            END DO
         END DO
         END DO
      ENDIF
! ***** PRINT ALLOCATED COST POOLS
      IF (IPRPT(20) .EQ. 1) THEN
         DO II=1,4
         IF (II .EQ. 1) SECT = 'GENERATION'
         IF (II .EQ. 2) SECT = 'TRANSMISSION'
         IF (II .EQ. 3) SECT = 'DISTRIBUTION'
         IF (II .EQ. 4) SECT = 'ALL COMPONENTS'
         ILOOP = NCOST/4
         IF ((ILOOP*4) .NE. NCOST) ILOOP = ILOOP + 1
         DO I=1,ILOOP
            ISTRT = ((I - 1)*4) + 1
            IEND = ISTRT + 3
            IF (IEND .GT. NCOST) IEND = NCOST
            WRITE(IOUT,365)NAME1,NAME3,SECT,(CCATNAM(J),J=ISTRT,IEND)
  365       FORMAT('1',/1X,A16,/,1X,A71/,1X,A25,4(/), &
                   T50,'ALLOCATED COST POOLS (10^6 $)',/,/, &
                   T7,4('---',A25,'---',1X),/, &
                   T7,4(' RES.    COM.    IND.   TRANS. ', &
                   1X),/,T7, &
                   4('------- ------- ------- -------',1X))
            DO J=IYRRPT,LASTYR
               WRITE(IOUT,370)IYRS(J), &
                              ((TCS2RE(J,L,K,II),K=1,4),L=ISTRT,IEND)
  370          FORMAT(T2,I4,1X,4(4(F7.0,1X)))
            END DO
         END DO
         END DO
      ENDIF
! ***** PRINT CUSTOMER PRICES
      IF (IPRPT(21) .EQ. 1) THEN
         DO II=1,4
            IF (II .EQ. 1) SECT = 'GENERATION'
            IF (II .EQ. 2) SECT = 'TRANSMISSION'
            IF (II .EQ. 3) SECT = 'DISTRIBUTION'
            IF (II .EQ. 4) SECT = 'ALL COMPONENTS'
            WRITE(IOUT,385)NAME1,NAME3,SECT
  385       FORMAT('1',/1X,A16,/,1X,A71/,1X,A25,4(/), &
                   T75,'PRICES',/,/, &
                   T7,'---------RESIDENTIAL----------',1X, &
                   '----------COMMERCIAL----------',1X, &
                   '----------INDUSTRIAL----------',1X, &
                   '--------TRANSPORTATION--------',/, &
                   T7,4('-REVENUE-- -QUANTITY-- -PRICE-',1X),/, &
                   T7,4('---------- ----------- -------',1X))
            DO J=IYRRPT,LASTYR
               WRITE(IOUT,390)IYRS(J), &
                              ((TPRRRE(J,L,K,II),K=1,3),L=1,4)
  390          FORMAT(T2,I4,1X,4(3(F10.2,1X,F11.2,1X,F7.1,1X)))
            END DO





         END DO
      ENDIF
! WRITE OUT ADJUSTED FINAL PRICES IN REAL DOLLARS
      IF (IPRPT(22) .EQ. 1) THEN
            WRITE(IOUT,395)NAME1,NAME3,SECT
  395       FORMAT('1',/1X,A16,/,1X,A71/,1X,A25,4(/), &
                   T70,'BENCHMARKED PRICES',/,/, &
                   T7,'---------RESIDENTIAL----------',1X, &
                   '----------COMMERCIAL----------',1X, &
                   '----------INDUSTRIAL----------',1X, &
                   '--------TRANSPORTATION--------',1X, &
                   '--AVERAGE--',/, &
                   T7,4('---------- --SUBSIDY-- -PRICE-',1X), &
                   '---PRICE---',/, &
                   T7,4('---------- --PERCENT-- -------',1X), &
                   '-----------')
       IF (NRGN .LE. UNRGNS) THEN
           DO J=IYRRPT,LASTYR
              WRITE(IOUT,400)IYRS(J), &
             SUBPCT(1,NRGN,J),PELRSNR(NRGN,J)*MC_JPGDP(IYRRL), &
             SUBPCT(2,NRGN,J),PELCMNR(NRGN,J)*MC_JPGDP(IYRRL), &
             SUBPCT(3,NRGN,J),PELINNR(NRGN,J)*MC_JPGDP(IYRRL), &
                              PELTRNR(NRGN,J)*MC_JPGDP(IYRRL), &
                              PELASNR(NRGN,J)*MC_JPGDP(IYRRL)
  400          FORMAT(T2,I4,F23.4,F8.1,F23.4,F8.1,F23.4,F8.1, &
                      F31.1,F12.1)
           END DO
       ELSE

! NATIONAL SUBSIDIZED PRICE REPORT

           DO J=IYRRPT,LASTYR

         NATPRC(1)=PELRS(11,J)*3.412*MC_JPGDP(IYRRL)
         NATPRC(2)=PELCM(11,J)*3.412*MC_JPGDP(IYRRL)
         NATPRC(3)=PELIN(11,J)*3.412*MC_JPGDP(IYRRL)

             DO I=1,3
      SUBPCT(I,NRGN,J)=((NATPRC(I)*TPRRRE(J,I,2,4)) &
        -(TPRRRE(J,I,1,4))) &
        /(TPRRRE(J,1,1,4)+TPRRRE(J,2,1,4)+TPRRRE(J,3,1,4))
             END DO

            WRITE(IOUT,400)IYRS(J), &
            SUBPCT(1,NRGN,J),PELRS(11,J)*3.412*MC_JPGDP(IYRRL), &
            SUBPCT(2,NRGN,J),PELCM(11,J)*3.412*MC_JPGDP(IYRRL), &
            SUBPCT(3,NRGN,J),PELIN(11,J)*3.412*MC_JPGDP(IYRRL), &
                             PELTR(11,J)*3.412*MC_JPGDP(IYRRL), &
                             PELAS(11,J)*3.412*MC_JPGDP(IYRRL)
           END DO
      ENDIF
       END IF

      END IF
      RETURN
      END
!
      SUBROUTINE ELRDND(IN)
      IMPLICIT NONE
!**************************************************************************
! THIS SUBROUTINE READS IN THE NUCLEAR DECOMMISSIONING DATA
! It is called only in the first forecast year processing.
! totals are in nominal dollars.
      INTEGER IN           ! unit number assigned to the ND input file
!**************************************************************************
! get includes in order later in the process
      include 'parametr'   !
      include 'emmparm'
      include 'control'    !
      include 'ncntrl'     !
      include 'macout'     !
      include 'efpnd'
      include 'efpgen'
      include 'eusprc'
      include 'efpint'
! Locals
      REAL *4 RATE, RT, NUMINR, ESTP, NDAFCR
      REAL *4 RANN                     ! real annuity amount in NDCBALY
      INTEGER IY, IYB, IYC, N, I, K
      CHARACTER *256 LINE

      CALL NUCDECOMMSQL
! ====> Read global decommissioning data  (applicable to all units)
! ----> First data varing by year : tax rates, estimate escalation factors
!* ----> read nuclear plant data detail by unit
      RATE   = .0
      NUMINR = .0
      DO N=1,NUCNUM   !MXNDU
!        WRITE(6,*) NDEST(N), NDESTY(N)
        CALL ELNDAL(1,N)
!       WRITE(6,*) NDEST(N), NDESTY(N)
!       calculate national average rate of fund balance
        IF(NDCBAL(N) .NE. -1.) THEN
          CALL ELESTP(N,ESTP)
          RATE = RATE + &
                 ( NDCBAL(N) / &
                   (((40. - (NDRET(N) - NDCBALY(N) + 1.)) / 40.) * &
                   ESTP))
!         WRITE(6,*) 'bal,past life,estp,rate', NDCBAL(N),&
!                  ((40. - (NDRET(N) - NDCBALY(N) + 1.)) / 40.),&
!                  ESTP, RATE
          NUMINR = NUMINR + 1.
        ENDIF
      ENDDO
!      NUCNUM = N - 1
!      Write(6,*)'Nucnum ', Nucnum, ' Numinr ', Numinr, ' MXNDU ', MXNDU
!       calculate national average rate of fund balance
!     WRITE(6,*) RATE / NUMINR, ' =', RATE, ' /', NUMINR
      RATE = RATE / NUMINR
!     check if there is any unread unit data left on the input file
      IF(NUCNUM .EQ. MXNDU) THEN
       IF(NUCNUM .GT. MXNDU) THEN
        WRITE(*,*) NUCNUM, ' nuclear units listed on the data file.'
        WRITE(*,*) MXNDU, ' nuclear units allowed in the UEFP.'
        STOP 'Too few NUCs allowed - expand MXNDU parameter on EFPND'
       ENDIF
      ENDIF
! ----> Estimate fund balances for all units with missing balance
!       for the start of the forecast USYEAR(1) year
      DO N=1,MXNDU
        IF(NDCBAL(N) .EQ. -1.) THEN
          NDCBALY(N) = USYEAR(1) - 1
          CALL ELESTP(N,ESTP)
          NDCBAL(N) = RATE * &
                   (((40. - (NDRET(N) - USYEAR(1) + 2.)) / 40.) * &
                   ESTP)
!         WRITE(6,*) 'balance = rate * timesh * ESTP',
!                   NDCBAL(N), RATE,
!                  ((40. - (NDRET(N) - USYEAR(1) + 2.)) / 40.),
!                  ESTP
        ENDIF
      ENDDO
! ----> Estimate annual payment for units without known payment
!
!     loop over all units and add :
!                 - known actual payments, and
!                 - calculated annuities for the same units
      RATE = .0
      NUMINR = .0
      DO N=1,NUCNUM
        IF(NDCAPMT(N) .NE. -1.) THEN
          CALL NDRANN(N,NDCBALY(N),NDRET(N),NDCBALY(N),RANN)
!         WRITE(6,*) 'N,NDCBALY(N),NDRET(N),RANN',
!                     N,NDCBALY(N),NDRET(N),RANN
          IF(RANN .NE. .0) THEN
!           calculate unit's rate of payment
            RT = NDCAPMT(N) / RANN
!           Rate can not be over one, assume payment reflecting full funding
            IF(RT .GT. 1.) RT = 1.
            RATE = RATE + RT
!           WRITE(6,*) NDCAPMT(N),' / ',RANN,' = ',
!                              NDCAPMT(N)/RANN,RATE
            NUMINR = NUMINR + 1.
          ENDIF
        ENDIF
      ENDDO
!     and the average Actual Funds Collection Rate is :
      NDAFCR = RATE / NUMINR
      WRITE(6,*) 'National payment factor', NDAFCR
!     loop over units and calculate actual payment if necessary
      DO N=1,NUCNUM
        IF(NDCAPMT(N) .EQ. -1.) THEN
          CALL NDRANN(N,NDCBALY(N),NDRET(N),NDCBALY(N),RANN)
          NDCAPMT(N) = RANN * NDAFCR
!         WRITE(6,*) 'Estimated payment :', NDCAPMT(N), RANN, NDAFCR
        ENDIF
!       calculate ND fund balance, and payment to USYEAR(1)-1 year
!       this aligns data for annual ND fund calculation logic
        CALL ELNDAL(2,N)
!       WRITE(6,*) 'Fund Balance in year 0 :', NDCBAL(N), NDCBALY(N)
      ENDDO
      RETURN
      END
!
      SUBROUTINE NDRANN(N,STARTY,ENDY,CUREY,RANN)
      IMPLICIT NONE
!**************************************************************************
! THIS SUBROUTINE CALCULATES NUCLEAR DECOMMISSIONING REAL ANNUITY PAYMENTS
!    PAYMENTS ARE IN REAL DOLLARS
!
! INPUT   :
!         N - nuclear unit index (alows to access unit data)
!    STARTY - first year annuity would start
!      ENDY - last year annuity would be collected in
!     CUREY - year to which the estimated cost has to be escalated (from USYEAR(1))
!
! OUTPUT  :
!      RANN - real annuity required to build the ND fund to estimate level
!             = 0 if STARTY .GT. retirement year
!
      REAL *4 RANN         ! real annuity amount
      INTEGER N            ! nuclear unit calculations are for
      INTEGER STARTY       ! year to start the real amnnuity
      INTEGER ENDY         ! year annuity ends and all funds are collected
      INTEGER CUREY        ! year to escalate estimate to, or -1 for no escal.
!**************************************************************************
      include 'parametr'   !
      include 'ncntrl'     !
      include 'emmparm'
      include 'control'    !
      include 'macout'     !
      include 'efpnd'
      include 'efpgen'
      include 'eusprc'
      include 'efpint'
      REAL *4 UNIT         ! unit used to calculate rate of a real annuity
      REAL *4 RUNIT        ! nominal value of real annuity in current year
      REAL *4 CURB         ! start year balance and its earnings
      REAL *4 CURE         ! estimate, if needed it is escalated to a CUREY
      REAL *4 NETEARNR     ! net annual earning rate of the ND fund
      REAL *4 ANNBAL       ! annuity balance in current year
      REAL *4 INFLR        ! current year inflation rate
      INTEGER IY, I
      DATA UNIT /1000./
      ANNBAL = .0                    ! start annuity balance from $.0
      CURB = NDCBAL(N)             ! start current balance and its earnings
      IF(CURB .EQ. -1) CURB = .0
      RUNIT = UNIT                   ! initialize real annuity payment
!     loop over years and caclulate end balance of real annuity
      DO IY=STARTY,ENDY              ! loop from start end collection year
       I = IY - USYEAR(1) + 1                          ! year index
!      get current year inflation from the deflator array
       IF(I .LT. 2) THEN                        ! pre forecast period
         INFLR = (MC_JPGDP(2) / MC_JPGDP(1)) - 1
       ELSEIF(I .LT. UNYEAR) THEN               ! period within forecast
         INFLR = (MC_JPGDP(I) / MC_JPGDP(I-1)) - 1
       ELSE                                     ! post forecst years
         INFLR = (MC_JPGDP(UNYEAR) / MC_JPGDP(UNYEAR-1)) - 1
       ENDIF
       IF(I .LT. 1) THEN
        NETEARNR = (INFLR+NDEARNR-NDADMNR) * (1-NDTAXR(1)) ! net earning rate
       ELSE
        NETEARNR = (INFLR+NDEARNR-NDADMNR) * (1-NDTAXR(I)) ! net earning rate
       ENDIF
!      for years > STARTYR calculate nominal value of the annuity payment
       IF(IY .NE. STARTY) RUNIT = RUNIT * (1+INFLR)
!      calculate real annuity nominal value by the end of IY year.
       ANNBAL = ANNBAL * (1 + NETEARNR) + RUNIT        ! annuity balance
!      calculate start year fund balance and its earnings
       CURB = CURB * (1 + NETEARNR)
      END DO
      CURE = NDEST(N)
      IF(CUREY .NE. -1) THEN       ! is it nessecary to escalate estimate
        DO IY=USYEAR(1),CUREY        ! escalate the estimate
          I = IY - USYEAR(1) + 1                          ! year index
          CURE = CURE * NESTESC(I)
        ENDDO
      ENDIF
!     calculate real annuity payment amount for the STARTY year
      RANN = UNIT * (CURE - CURB) / ANNBAL
!     it will not happen, but just in case do not allow negative annuity
      IF(RANN .LT. 0.) RANN = .0
      RETURN
      END
!
      SUBROUTINE ELESTP(N,ESTP)
      IMPLICIT NONE
!**********************************************************************
!      THIS SUBROUTINE ALIGNS ESTIMATED COST OF NUCLEAR DECOMMISSIONING WITH THE FUND BALANCE DATE
!      for the purpose of estimating missing ND fund balances
!
      include 'parametr'   !
      include 'ncntrl'     !
      include 'macout'     !
      include 'emmparm'
      include 'control'
      include 'efpnd'
      include 'efpgen'
      include 'eusprc'
      include 'efpint'
      REAL *4 ESTP, INFL
      INTEGER IY, I, STARTY, ENDY, STEP, N
      ESTP = NDEST(N)
         IF(NDESTY(N) .LT. NDCBALY(N)) THEN
           STARTY = NDESTY(N) + 1
           ENDY = NDCBALY(N)
           STEP = 1
         ELSE
           STARTY = NDCBALY(N)
           ENDY = NDESTY(N) + 1
           STEP = -1
         ENDIF
         DO IY=STARTY,ENDY,STEP
            I = IY - USYEAR(1) + 1
            IF(I .LT. 2) THEN                        ! pre forecast period
              INFL = MC_JPGDP(2) / MC_JPGDP(1)
            ELSEIF(I .LT. UNYEAR) THEN               ! period within forecast
              INFL = MC_JPGDP(I) / MC_JPGDP(I-1)
            ELSE                                     ! post forecst years
              INFL = MC_JPGDP(UNYEAR) / MC_JPGDP(UNYEAR-1)
            ENDIF
            IF( STEP .EQ. 1 ) THEN
              ESTP = ESTP * INFL
            ELSE
              ESTP = ESTP / INFL
            ENDIF
!           WRITE(6,*) INFL, '  in', IY, ' step', STEP, ESTP
         ENDDO
      RETURN
      END
!
      SUBROUTINE ELNDAL(SCOPE,N)
      IMPLICIT NONE
!**************************************************************************
! THIS SUBROUTINE ALIGNS NUCLEAR DECOMMISSIONING FINANCIAL DATA
!
      INTEGER SCOPE        ! 1 - align ND cost estimate with retirement date,
!                          ! 2 - align ND funds and payment with BASEYR-1
      INTEGER N            ! current nuclear unit index
!**************************************************************************
      include 'parametr'   !
      include 'ncntrl'     !
      include 'macout'     !
      include 'emmparm'
      include 'control'
      include 'efpnd'
      include 'efpgen'
      include 'eusprc'
      include 'efpint'
      REAL *4 EARN, TAX, ADM, INFL
      INTEGER IY, I
      LOGICAL *1 CHANGEYR
      ALLIGN: SELECT CASE(SCOPE)
       CASE (1) ALLIGN
!      align estimated cost of ND to retirement year $, for use in
!      the current real annuity calculation
!      early retirement is NOT a concern since we assume payments to follow
!      normal retirement scedule unless otherwise specified - that would not
!      affect current years. (I.E. even already retired units do not collect
!      more then they would if they would be on line...
         IF(NDESTY(N) .LT. NDRET(N)) THEN
           DO IY=NDESTY(N)+1,NDRET(N)
            I = IY - USYEAR(1) + 1
            IF(I .LT. 2) THEN                        ! pre forecast period
              INFL = MC_JPGDP(2) / MC_JPGDP(1)
            ELSEIF(I .LT. UNYEAR) THEN               ! period within forecast
              INFL = MC_JPGDP(I) / MC_JPGDP(I-1)
            ELSE                                     ! post forecst years
              INFL = MC_JPGDP(UNYEAR) / MC_JPGDP(UNYEAR-1)
            ENDIF
            IF(I .LT. 1) THEN
              NDEST(N) = NDEST(N) * INFL * NESTESC(1)
!             WRITE(6,*) INFL, ' *', NESTESC(1), '  in', IY, NDEST(N)
            ELSE
!             cost escalation in and after USYEAR(1) is done in the main
!             model loop what allows to model price escalation and its
!             influence on annuity required.  No cost escalation here...
              NDEST(N) = NDEST(N) * INFL
!             WRITE(6,*) INFL, ' in', IY, NDEST(N)
            ENDIF
           ENDDO
!          back of cost escalation rate from NDESTY to USYEAR(1)-1
           DO IY=NDESTY(N),USYEAR(1),-1
            I = IY - USYEAR(1) + 1
            NDEST(N) = NDEST(N) / NESTESC(I)
!           WRITE(6,*) NESTESC(I), '  in', IY, NDEST(N)
           ENDDO
           NDESTY(N) = NDRET(N)
         ENDIF
       CASE (2) ALLIGN
!      align amount in ND funds and payment to BASEYR-1 year level
        IF(NDCBALY(N) .NE. BASEYR-1) THEN
         CHANGEYR = .TRUE.
!        WRITE(6,*) 'NDCBALY(N), BASEYR', NDCBALY(N), BASEYR
         IF(NDCBALY(N) .LT. BASEYR-1) THEN
          DO IY=NDCBALY(N)+1, BASEYR-1            ! loop over years
            I = IY - USYEAR(1) + 1                ! year index
            IF(I .LT. 3) THEN                     ! get inflation
              INFL = MC_JPGDP(2) / MC_JPGDP(1)
            ELSEIF(I .LT. UNYEAR) THEN               ! period within forecast
              INFL = MC_JPGDP(I) / MC_JPGDP(I-1)
            ELSE                                     ! post forecst years
              INFL = MC_JPGDP(UNYEAR) / MC_JPGDP(UNYEAR-1)
            ENDIF
            EARN = NDCBAL(N) * (NDEARNR + (INFL - 1))
            ADM  = NDCBAL(N) * NDADMNR
            IF(I .LT. 2) THEN
              TAX  = (EARN - ADM) * NDTAXR(1)
            ELSE
              TAX  = (EARN - ADM) * NDTAXR(I)
            ENDIF
            NDCAPMT(N) = NDCAPMT(N) * INFL
            NDCBAL(N) = NDCBAL(N) + EARN - TAX - ADM + NDCAPMT(N)
          ENDDO
         ELSE   !  IF(NDCBALY(N) .GT. BASEYR)
          DO IY=NDCBALY(N), BASEYR, -1
            I = IY - USYEAR(1) + 1
            IF(I .LT. 3) THEN                     ! get inflation
              INFL = MC_JPGDP(2) / MC_JPGDP(1)
            ELSEIF(I .LT. UNYEAR) THEN               ! period within forecast
              INFL = MC_JPGDP(I) / MC_JPGDP(I-1)
            ELSE                                     ! post forecst years
              INFL = MC_JPGDP(UNYEAR) / MC_JPGDP(UNYEAR-1)
            ENDIF
            IF(NDCBAL(N) .LT. NDCAPMT(N)) THEN
              CHANGEYR = .FALSE.
              NDCBALY(N) = IY
              EXIT
            ENDIF
!           WRITE(6,*) 'rec', IY, NDCBAL(N), NDCAPMT(N), NDTAXR(I)
            NDCBAL(N) = (NDCBAL(N) - NDCAPMT(N)) / (1 + &
                        ((NDEARNR+(INFL-1)-NDADMNR) * (1-NDTAXR(I))))
            NDCAPMT(N) = NDCAPMT(N) / INFL
!           WRITE(6,*) 'result', NDCBAL(N)
          ENDDO
         ENDIF
         IF(CHANGEYR) NDCBALY(N) = BASEYR-1
        ENDIF
      END SELECT ALLIGN
      RETURN
      END
!
      SUBROUTINE ELND(NRGN, IOUT, ICALL)
      IMPLICIT NONE
!**************************************************************************
! THIS SUBROUTINE CALCULATES ALL NUCLEAR DECOMMISSIONING AMOUNTS IN NOMINAL DOLLARS
!
! Assumptions:
!.1 Last known payment amount, in real $, is used until (catch up year-1).
!   If payment is not known then it is calculated based on indurtry
!   average payment expressed as fraction of 'full real annuity payment'.
!.2 Estimated ND cost is adjusted for inflation and cost escalation
!   up to the planned retirement year.  It is then used as base to
!   calculate real annuity payments.
!.3 ND payment on and after catch up date, or after early retirement date
!   are calculated annually as real annuity collecting full funds until
!   end of on line service, or until the end of post service monitoring
!   period (monitoring is supplied by unit).
!.4 In catch up period the portion of the payment in excess of current
!   payment adjusted for inflation is subject to payment shortfall split
!   among ratepayers, equity, and government according with catch up
!   period rates
!.5 Early retirement triggers monitoring period, and payment schedule
!   is adjusted accordingly to meet estimate requirements by the end of
!   monitoring period. Payment shortfall is split according to rates
!   defined for early retirement.
!.6 The tax exempt status for municipal and cooperative owned funds is
!   ignored.  Tax on earnings is always calculated.
!
! -? What do we do with the equity share of NDF payment
!
! -? How do we reflect liability on the balance sheet
!**************************************************************************
      INTEGER NRGN         ! current region number
      INTEGER IOUT         ! file unit for decommissioning report
      CHARACTER*1 ICALL    ! flag: G-generation, D-distrib, T-transm.
! get includes in order later in the process
      include 'parametr'       !
      include 'ncntrl'         !
      include 'macout'         !
      include 'emmparm'
      include 'control'
      include 'efpnd'
      include 'eusprc'
      include 'efpint'
      include 'efprcy'
!
! define local ND variables: at one point they were designed to be passed
! by COMMON /NUCDO/ on efpint file, and stored for reporting routines.
! Arrays present annual/regional totals for private and public sectors
      REAL *4 ERNDFBAL(2)    ! Nuclear Decommissioning Fund BALance
      REAL *4 ERNDFERN(2)    ! Nuclear Decommissioning Fund EaRNings
      REAL *4 ERNDFADM(2)    ! Nuclear Decommissioning Fund ADMinistrative cost
      REAL *4 ERNDETAX(2)    ! Nuclear Decommissioning Fund tax on earnings
      REAL *4 ERNDFEST(2)    ! Nuclear Decommissioning Fund ESTimated balance required for decommissioning
      REAL *4 ERNUNDF(2)     ! Number of Units comprizing the regional Nuclear Decommissioning Fund
!             NOTE:  The number can be a fraction if part of unit is privately and part publicly owned
!
!     COMMON /NUCDO/ ERNDFBAL, ERNDFADM, ERNDETAX, ERNDFEST, ERNUNDF
!
! define local varaibles
! earnings, tax, administrative cost, and payment are in nominal $
      REAL *4 EARN, TAX, ADM, PMT
      REAL *4 INFL, INFLR  ! inflation, and inflation rate [INFL=1+INFLR]
      REAL *4 PPR          ! pravite/public portion of unit ownership
      REAL *4 PMTSF        ! cutch up or retirement payment shartfall
      REAL *4 MONITOR      ! annual monitoring cost
      INTEGER RETS         ! retirement start
      INTEGER RETFCE       ! year when funds collection should end
      INTEGER KPP          ! loop index: private, public
      INTEGER ISPL         ! loop index cost split (ratepay., equity, gov.)
      INTEGER N            ! index in the loop over nuclear units
      INTEGER LMONP        ! length of monitoring period
      LOGICAL *1 TOEARLY   ! .TRUE. in years before ND funds are collected
      LOGICAL *1 PASS      ! DO NOT summ data for decomissioned unit.
! earnings, tax, administrative cost, and payment are in nominal $

!     *** INITIALIZE REGION TOTALS
      DO KPP=1,2
        ERNDFBAL(KPP) = .0
        ERNDFERN(KPP) = .0
        ERNDFADM(KPP) = .0
        ERNDFPMT(KPP) = .0
        ERNDFEST(KPP) = .0
        ERNDETAX(KPP) = .0
        ERNDMON(KPP)  = .0
        ERNUNDF(KPP)  = .0
        DO ISPL=1,3
          ERERTSPLIT(KPP,ISPL) = .0
          ERCUPSPLIT(KPP,ISPL) = .0
        ENDDO
      ENDDO
!     If run is not for generation then return
      IF (ICALL .EQ. 'G') THEN
        IF(CURIYR .LE. 2) THEN                   ! first forecast year
          INFL = MC_JPGDP(2) / MC_JPGDP(1)
        ELSEIF(CURIYR .LE. UNYEAR) THEN          ! period within forecast
          INFL = MC_JPGDP(CURIYR) / MC_JPGDP(CURIYR-1)
        ELSE                                     ! post forecst years
          INFL = MC_JPGDP(UNYEAR) / MC_JPGDP(UNYEAR-1)
        ENDIF
        INFLR = INFL - 1.
        DO N=1,NUCNUM
          IF (NUREG(N) .EQ. NRGN) THEN
            PASS = .FALSE.
!
!           initialize year unit retirement starts (RETS)
            RETS = NDERET(N)
            IF(RETS .EQ. -1) THEN
              RETS = NDRET(N)
            ENDIF
!
!           initialize year when ND funds must be collected (RETFCE)
            IF(NDPRMON(N) .NE. -1) THEN
              RETFCE = RETS + NDPRMON(N)
            ELSE
              RETFCE = RETS
            ENDIF
!
!           Current Year <= ND funds collection
            IF(USYEAR(CURIYR) .LE. RETFCE) THEN
             TOEARLY = .FALSE.        ! sel flag for facility with ND fund open
!            calculate ND cost escalation for the current year
             NDEST(N) = NDEST(N) * NESTESC(CURIYR)
!            for period before catch-up year calculate flat payment schedule
             IF(USYEAR(CURIYR) .LT. NDCATCH .AND. &
                USYEAR(CURIYR) .LT. RETS ) THEN
!             first payment may be lower if it falls at or after BASEYR
              IF(USYEAR(CURIYR) .EQ. NDCBALY(N)) THEN
               EARN = .0
               TAX  = .0
               ADM  = .0
               PMT  = NDCBAL(N)
!             full annual payments as specified by unit on the input file
              ELSEIF(USYEAR(CURIYR) .GT. NDCBALY(N)) THEN
               EARN = NDCBAL(N) * (NDEARNR + INFLR)
               ADM  = NDCBAL(N) * NDADMNR
               TAX  = (EARN - ADM) * NDTAXR(CURIYR)
!              grow payment with inflation
               NDCAPMT(N) = NDCAPMT(N) * INFL
               PMT  = NDCAPMT(N)
               NDCBAL(N) = NDCBAL(N) + EARN - TAX - ADM + PMT
!             funds colection did not started yet
              ELSE
               TOEARLY = .TRUE.
               EARN = .0
               TAX  = .0
               ADM  = .0
               PMT  = .0
              ENDIF
!            calculations for period after catch-up for early retirements
             ELSEIF(USYEAR(CURIYR) .LT. RETS .AND. &
                    NDERET(N) .EQ. RETS) THEN
!              calculate payment as grown with inflation (to obtain 'catch up'
!              part of payment which is subject to payment shortfall split
               NDCAPMT(N)  = NDCAPMT(N) * INFL
               EARN = NDCBAL(N) * (NDEARNR + INFLR)
               ADM  = NDCBAL(N) * NDADMNR
               TAX  = (EARN - ADM) * NDTAXR(CURIYR)
!              find real annuity PMT to meet estimted cost at 'retirement'
               CALL NDRANN(N,USYEAR(CURIYR),NDRET(N),-1,PMT)
               NDCBAL(N) = NDCBAL(N) + EARN - TAX - ADM + PMT
!            calculations for period of maintenance (accelerated payment)
!            after early retirement, and
!            normal plant operation in catch up and maintenance period
             ELSEIF(USYEAR(CURIYR) .LE. RETFCE ) THEN
!              find payment as grown with inflation (to get post retirement
!              part of payment which is subject to payment shortfall split
               NDCAPMT(N)  = NDCAPMT(N) * INFL
               EARN = NDCBAL(N) * (NDEARNR + INFLR)
               ADM  = NDCBAL(N) * NDADMNR
               TAX  = (EARN - ADM) * NDTAXR(CURIYR)
!              calculate real annuity payment to meet estimted cost at 'retirement'
               CALL NDRANN(N,USYEAR(CURIYR),RETFCE,-1,PMT)
               NDCBAL(N) = NDCBAL(N) + EARN - TAX - ADM + PMT
             ELSE
               PASS = .TRUE.
             ENDIF
!            calculate regional totals for private and public units
             IF(.NOT. PASS) THEN
              DO KPP=1,2
!              set private/public ownership portion
               OWNER: SELECT CASE(KPP)
                CASE (1) OWNER
                  PPR = NUPPR(N)            ! private
                CASE (2) OWNER
                  PPR = 1. - NUPPR(N)       ! public
               END SELECT OWNER
               IF(PPR .NE. .0) THEN
                IF(.NOT. TOEARLY) THEN
!                ND Fund BALance
                 ERNDFBAL(KPP) = ERNDFBAL(KPP) + NDCBAL(N) * PPR
!                ND Fund EaRNings
                 ERNDFERN(KPP) = ERNDFERN(KPP) + EARN * PPR
!                ND Fund ADMinistrative cost
                 ERNDFADM(KPP) = ERNDFADM(KPP) + ADM * PPR
!                ND Fund annual PayMenT (into the fund)
                 ERNDFPMT(KPP) = ERNDFPMT(KPP) + PMT * PPR
!                ND Fund tax on earnings
                 ERNDETAX(KPP) = ERNDETAX(KPP) + TAX * PPR
                ENDIF
!               ND ESTimated cost
                ERNDFEST(KPP) = ERNDFEST(KPP) + NDEST(N) * PPR
!               Number of Units comprizing the regional ND Fund
                ERNUNDF(KPP)  = ERNUNDF(KPP) + PPR
!                if current year falls after retirment year
                 IF(USYEAR(CURIYR) .GE. RETS) THEN
!                Early retirement payment and monitoring costs split among
!                ratepayers, equity, and government
                  PMTSF = PMT - NDCAPMT(N)
                  IF(PMTSF .LT. .0) PMTSF = .0
!                 monitoring cost is calculated at least for retirement year
                  MONITOR = NDTCMON * NUCAP(N)
                  DO ISPL=1,3
                   ERERTSPLIT(KPP,ISPL) = ERERTSPLIT(KPP,ISPL) + &
!     &                                   (PMTSF + MONITOR) *
!  don't add monitor expense to this!
                                         (PMTSF) * &
                                          NDPSER(ISPL) * PPR
                  ENDDO
!                 Colelct monitoring expense
                  ERNDMON(KPP) = ERNDMON(KPP) + MONITOR * PPR
!                Catch up pmt split among ratepayers, equity, and government
                 ELSEIF(USYEAR(CURIYR) .GE. NDCATCH) THEN
                  PMTSF = PMT - NDCAPMT(N)
                  IF(PMTSF .LT. .0) PMTSF = .0
                  DO ISPL=1,3
                   ERCUPSPLIT(KPP,ISPL) = ERCUPSPLIT(KPP,ISPL) + &
                                          PMTSF * NDPSCU(ISPL) * PPR
                  ENDDO
                 ENDIF
               ENDIF    ! if private|public has its share
              ENDDO     ! loop private/public - sum for the region
             ENDIF      ! if .not. PASS
            ENDIF       ! if ND funds are still collected
          ENDIF         ! is unit located in current region
        ENDDO           ! loop over NUC units

!  write out decom data in constant dollars (1987)
       WRITE(22,'(A,I4,A,I2,A,F12.4,A,F8.4,A,F8.4,A,F12.4,A,F12.4)') 'NUCDEC1 :',USYEAR(CURIYR),':',NRGN,':', &
        (ERNDFBAL(1)+ERNDFBAL(2))/MC_JPGDP(CURIYR),':', &
        (ERNDFERN(1)+ERNDFERN(2))/MC_JPGDP(CURIYR),':', &
        (ERNDFADM(1)+ERNDFADM(2))/MC_JPGDP(CURIYR),':', &
        (ERNDFPMT(1)+ERNDFPMT(2))/MC_JPGDP(CURIYR),':', &
        (ERNDFEST(1)+ERNDFEST(2))/MC_JPGDP(CURIYR)

!       Reduce Payment amount to the ratepayers share only to comply with this
!       array usage later in the model
        DO KPP=1,2
         DO ISPL=2,3
          ERNDFPMT(KPP) = ERNDFPMT(KPP) - &
                        (ERERTSPLIT(KPP,ISPL) + ERCUPSPLIT(KPP,ISPL))
         ENDDO
        ENDDO
       WRITE(22,'(A,I4,A,I2,A,F12.4)') 'NUCDEC2 :',USYEAR(CURIYR),':',NRGN,':', &
        (ERNDFPMT(1)+ERNDFPMT(2))/MC_JPGDP(CURIYR)
      ENDIF             ! generation
      RETURN
      END
!
      SUBROUTINE ANEWBLD(NRGN)
      IMPLICIT NONE
!*******************************************************************
! THIS SUBROUTINE CREATES A NEW BUILD TO RECORD THE COSTS OF CAPITAL ADDITIONS, PHASE-INS, SALE-LEASEBACKS, AND LIFE EXTENSIONS
!*******************************************************************
! INPUT VARIABLES:
!     NRGN   = CURRENT REGION
!     CURIYR = CURRENT YEAR INDEX
!     EIMISC = PLANT CODE FOR THIS BUILD (I.E., THE MISCELLANEOUS BUILD)
      include 'efpgen'
      include 'parametr'
      include 'ncntrl'
      INTEGER I
      INTEGER NRGN
      REAL*4 PCST
      REAL*4 SERP
      INTEGER PTYP
      INTEGER SYR
      REAL*4 PCAP(2)
      REAL*4 BCWP(2)
      REAL*4 ASVL(2)
      REAL*4 DITC(2)
      REAL*4 AFDC(2)
      REAL*4 RCWP(2)
      REAL*4 ABDE(2)
      REAL*4 BKVL(2)
! SET UP THE PARAMETERS TO INITIALIZE THE BUILD AT ZERO VALUE
         PCST = 0.0
         PTYP = EIMISC
         SYR = CURIYR
         SERP = 1.0
         DO I=1,2
            PCAP(I) = 0.0
            BCWP(I) = 0.0
            ASVL(I) = 0.0
            DITC(I) = 0.0
            AFDC(I) = 0.0
            RCWP(I) = 0.0
            ABDE(I) = 0.0
            BKVL(I) = 0.0
         END DO
          CALL UNEWBLD(NRGN,DITC,AFDC,BKVL,ASVL,BCWP,RCWP,PTYP, &
                     SYR,PCST,PCAP,SERP,ABDE)
      RETURN
      END
!
      SUBROUTINE WREFPDB(ITYPE,NRGN,INDOC)
      IMPLICIT NONE
!*******************************************************************
!
!     THIS SUBROUTINE WRITES THE EFP RESULTS TO THE EFP ACCESS DATABASE
!
!*******************************************************************
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'efpgen'
      include 'efprc'
      include 'efpr'
      include 'efprp2'
      include 'efprcy'
      include 'control'
      include 'eusprc'
      include 'efpint'
      include 'efpout'
      include 'efpwrt'
      include 'macout'
      include 'edbdef'
      include 'dispin'
      include 'efpcntrl'
      include 'dsmdimen'
      include 'dsmtoefd'
      INTEGER NUMTABS
      PARAMETER (NUMTABS = 7)        ! total number of database tables
      INTEGER NRGN,RGN,IOWN,ICAP,ICP,I,IS,IL,ITYPE,INDOC
      INTEGER M,ICL,J,Y,IY,ICOM,ISECT,IEU
      REAL*4 PPRICE(MNUMYR)
      REAL*4 REVRES
      REAL*4 REVULT
      REAL*4 FUEL
      REAL*4 OM
      REAL*4 WHO, PPWS, TOTWS
      REAL*4 FUELTTL, OMTTL, CAPTTL
      REAL*4 TOTCAP
      REAL*4 DEP
      REAL*4 TAXESI
      REAL*4 TAXESO
      REAL*4 RET
      CHARACTER*1 ICALL
!
      LOOPING = 0
      NUMCOLS = 0
      DYNSTM = ' '
      WRTSTM = ' '
      COLVALS = 0.0
      COLV = 0.0
      CHCOLVALS = ' '
      CHCOLV = ' '

      IF (NRGN .EQ. (NOPREG+1)) THEN
        RGN = MNUMNR
      ELSE
        RGN = NRGN
      ENDIF
!
!     Write regional and yearly output table to efp database
!
!
!     WRITE EFP_CSTOFCAP Table to EFP Database
!
        TNUM = 1
        IF (LOOPING(TNUM) .EQ. 0) THEN
          NUMCOLS(TNUM) = 16
          DYNSTM(TNUM) = 'INSERT INTO EFP_CSTOFCAP VALUES(?,?,?,?,?,?,?,' &
                  //'?,?,?,?,?,?,?,?,?,?)'
          WRTSTM(TNUM) = 'EFP_CSTOFCAP'
        ENDIF
        DO IY = UESTYR - UHBSYR , LASTYR
          LOOPING(TNUM) = LOOPING(TNUM) + 1
          COLV(TNUM,1,LOOPING(TNUM)) = IY
          COLV(TNUM,2,LOOPING(TNUM)) = RGN
          COLV(TNUM,3,LOOPING(TNUM)) = CURITR
          COLV(TNUM,4,LOOPING(TNUM)) = ITYPE
          COLV(TNUM,5,LOOPING(TNUM)) = INDOC
          COLV(TNUM,6,LOOPING(TNUM)) = TCSTCA(IY,1)
          COLV(TNUM,7,LOOPING(TNUM)) = TCSTCA(IY,2)
          COLV(TNUM,8,LOOPING(TNUM)) = TCSTCA(IY,3)
          COLV(TNUM,9,LOOPING(TNUM)) = TCSTCA(IY,4)
          COLV(TNUM,10,LOOPING(TNUM)) = TCSTCA(IY,5)
          COLV(TNUM,11,LOOPING(TNUM)) = TCSTCA(IY,6)
          COLV(TNUM,12,LOOPING(TNUM)) = TCSTCA(IY,7)
          COLV(TNUM,13,LOOPING(TNUM)) = TCSTCA(IY,8)
          COLV(TNUM,14,LOOPING(TNUM)) = TCSTCA(IY,9)
          COLV(TNUM,15,LOOPING(TNUM)) = TCSTCA(IY,10)
          COLV(TNUM,16,LOOPING(TNUM)) = TCSTCA(IY,11)
          IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
            COLVALS(:,:) = COLV(TNUM,:,:)
!           CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
            CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
            LOOPING(TNUM) = 0
          ENDIF
        ENDDO
!
!
!         WRITE EFP_FTREV table to database
!
!         TNUM = 2
!         IF (LOOPING(TNUM) .EQ. 0) THEN
!           NUMCOLS(TNUM) = 4
!           DYNSTM(TNUM) = 'INSERT INTO EFP_FTREV VALUES(?,?,?,?,?)'
!         ENDIF
!         LOOPING(TNUM) = LOOPING(TNUM) + 1
!         COLV(TNUM,1,LOOPING(TNUM)) = IY
!         COLV(TNUM,2,LOOPING(TNUM)) = NRGN
!         COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!         COLV(TNUM,4,LOOPING(TNUM)) = TOTREVNW(IY,NRGN)
!         IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!           COLVALS(:,:) = COLV(TNUM,:,:)
!           CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!           LOOPING(TNUM) = 0
!         ENDIF
!
!         WRITE EFP_FPRCYRO table to database
!
!         TNUM = 3
!         IF (LOOPING(TNUM) .EQ. 0) THEN
!           NUMCOLS(TNUM) = 7
!           DYNSTM(TNUM) = 'INSERT INTO EFP_FPRCYRO VALUES(?,?,?,?,?,?,?,?)'
!         ENDIF
!         DO IOWN = 1 , EFP_D_OWN + 1
!           LOOPING(TNUM) = LOOPING(TNUM) + 1
!           COLV(TNUM,1,LOOPING(TNUM)) = IY
!           COLV(TNUM,2,LOOPING(TNUM)) = NRGN
!           COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!           COLV(TNUM,4,LOOPING(TNUM)) = IOWN
!           COLV(TNUM,5,LOOPING(TNUM)) = ERITAXC(IY,NRGN,IOWN)/MC_JPGDP(IY)
!           COLV(TNUM,6,LOOPING(TNUM)) = EROTAXC(IY,NRGN,IOWN)/MC_JPGDP(IY)
!           COLV(TNUM,7,LOOPING(TNUM)) = SCSPRC(IY,NRGN,IOWN)/MC_JPGDP(IY)
!           IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!             COLVALS(:,:) = COLV(TNUM,:,:)
!             CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!             LOOPING(TNUM) = 0
!           ENDIF
!         ENDDO
!
!     Print Tables that are not dimensioned by stage of production for indoc = 4 only
!
      IF (INDOC .EQ. 4) THEN
!
!       WRITE EFP_FPRCLS table to database
!
        TNUM = 2
        IF (LOOPING(TNUM) .EQ. 0) THEN
            NUMCOLS(TNUM) = 12
            DYNSTM(TNUM) = 'INSERT INTO EFP_FPRCLS VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?)'
            WRTSTM(TNUM) = 'EFP_FPRCLS'
        ENDIF
        DO IY = UESTYR - UHBSYR , LASTYR
          DO IS = 1 , EFDns
            DO IL = 1, ELNVCT(IS)
              LOOPING(TNUM) = LOOPING(TNUM) + 1
              COLV(TNUM,1,LOOPING(TNUM)) = IY
              COLV(TNUM,2,LOOPING(TNUM)) = RGN
              COLV(TNUM,3,LOOPING(TNUM)) = CURITR
              COLV(TNUM,4,LOOPING(TNUM)) = IS
              COLV(TNUM,5,LOOPING(TNUM)) = IL
              COLV(TNUM,6,LOOPING(TNUM)) = ITYPE
              COLV(TNUM,7,LOOPING(TNUM)) = ERRELC(IY,RGN,IS,IL)/MC_JPGDP(IY)
              COLV(TNUM,8,LOOPING(TNUM)) = ERMECC(IY,RGN,IS,IL)/MC_JPGDP(IY)
              COLV(TNUM,9,LOOPING(TNUM)) = ERSLCPRC(IY,RGN,IS,IL,ITYPE)/MC_JPGDP(IY)
              COLV(TNUM,10,LOOPING(TNUM)) = ERITAXC(IY,RGN,ITYPE)/MC_JPGDP(IY)
              COLV(TNUM,11,LOOPING(TNUM)) = EROTAXC(IY,RGN,ITYPE)/MC_JPGDP(IY)
              COLV(TNUM,12,LOOPING(TNUM)) = SCSPRC(IY,RGN,ITYPE)/MC_JPGDP(IY)
              IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
                COLVALS(:,:) = COLV(TNUM,:,:)
!               CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                LOOPING(TNUM) = 0
              ENDIF
            ENDDO
          ENDDO
        ENDDO
!
!     if owner eq 3 do tables not dimensioned by owner
!
        IF (ITYPE .EQ. 3) THEN
!
!     Write EFP End_use Price Table (EFD_FEUPR) to database
!
          TNUM = 3
          IF (LOOPING(TNUM) .EQ. 0) THEN
            NUMCOLS(TNUM) = 5
            DYNSTM(TNUM) = 'INSERT INTO EFP_FEUPR VALUES(?,?,?,?,?,?)'
            WRTSTM(TNUM) = 'EFP_FEUPR'
          ENDIF
          DO IY = UESTYR - UHBSYR , LASTYR
            DO IEU = 1 , MNEUGRP
              LOOPING(TNUM) = LOOPING(TNUM) + 1
              COLV(TNUM,1,LOOPING(TNUM)) = IY
              COLV(TNUM,2,LOOPING(TNUM)) = RGN
              COLV(TNUM,3,LOOPING(TNUM)) = CURITR
              COLV(TNUM,4,LOOPING(TNUM)) = IEU
              COLV(TNUM,5,LOOPING(TNUM)) = PELOUTN(RGN,IY,IEU)
              IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
                COLVALS(:,:) = COLV(TNUM,:,:)
!               CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                LOOPING(TNUM) = 0
              ENDIF
            ENDDO
          ENDDO
!
!
          TNUM = 4
          IF (LOOPING(TNUM) .EQ. 0) THEN
            NUMCOLS(TNUM) = 13
            DYNSTM(TNUM) = 'INSERT INTO EFP_FPRICE VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
            WRTSTM(TNUM) = 'EFP_FPRICE'
          ENDIF
          DO IY = UESTYR - UHBSYR , LASTYR
            LOOPING(TNUM) = LOOPING(TNUM) + 1
            COLV(TNUM,1,LOOPING(TNUM)) = IY
            COLV(TNUM,2,LOOPING(TNUM)) = RGN
            COLV(TNUM,3,LOOPING(TNUM)) = CURITR
            COLV(TNUM,4,LOOPING(TNUM)) = PECGENN(RGN,IY)
            COLV(TNUM,5,LOOPING(TNUM)) = PECDISN(RGN,IY)
            COLV(TNUM,6,LOOPING(TNUM)) = PECTRNN(RGN,IY)
            COLV(TNUM,7,LOOPING(TNUM)) = PELASNR(RGN,IY)
            COLV(TNUM,8,LOOPING(TNUM)) = PELCPNR(RGN,IY)
            COLV(TNUM,9,LOOPING(TNUM)) = PELFLNR(RGN,IY)
            COLV(TNUM,10,LOOPING(TNUM)) = PELOMNR(RGN,IY)
            COLV(TNUM,11,LOOPING(TNUM)) = PELWHNR(RGN,IY)
            COLV(TNUM,12,LOOPING(TNUM)) = PELTLNR(RGN,IY)
            COLV(TNUM,13,LOOPING(TNUM)) = TOTREVNW(IY,NRGN)
            IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
              COLVALS(:,:) = COLV(TNUM,:,:)
!             CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
              CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
              LOOPING(TNUM) = 0
            ENDIF
          ENDDO
!
!     Write EFP Pricing Output Table by Demand Sector (EFP_FPRDS) table to database
!
            TNUM = 5
            IF (LOOPING(TNUM) .EQ. 0) THEN
              NUMCOLS(TNUM) = 9
              DYNSTM(TNUM) = 'INSERT INTO EFP_FPRDS VALUES(?,?,?,?,?,?,?,?,?,?)'
              WRTSTM(TNUM) = 'EFP_FPRDS'
            ENDIF
            DO IY = UESTYR - UHBSYR , LASTYR
            DO ISECT = 1 , 5
              LOOPING(TNUM) = LOOPING(TNUM) + 1
              COLV(TNUM,1,LOOPING(TNUM)) = IY
              COLV(TNUM,2,LOOPING(TNUM)) = RGN
              COLV(TNUM,3,LOOPING(TNUM)) = CURITR
              COLV(TNUM,4,LOOPING(TNUM)) = ISECT
              IF (ISECT .EQ. 1) THEN
                COLV(TNUM,5,LOOPING(TNUM)) = PECRSRLN(RGN,IY)
                COLV(TNUM,6,LOOPING(TNUM)) = PECRSMEN(RGN,IY)
                COLV(TNUM,7,LOOPING(TNUM)) = PECRSTXN(RGN,IY)
                COLV(TNUM,8,LOOPING(TNUM)) = PECRSTDN(RGN,IY)
                COLV(TNUM,9,LOOPING(TNUM)) = PECRSRLN(RGN,IY) + PECRSMEN(RGN,IY) + PECRSTXN(RGN,IY) + PECRSTDN(RGN,IY)
              ELSEIF (ISECT .EQ. 2) THEN
                COLV(TNUM,5,LOOPING(TNUM)) = PECCMRLN(RGN,IY)
                COLV(TNUM,6,LOOPING(TNUM)) = PECCMMEN(RGN,IY)
                COLV(TNUM,7,LOOPING(TNUM)) = PECCMTXN(RGN,IY)
                COLV(TNUM,8,LOOPING(TNUM)) = PECCMTDN(RGN,IY)
                COLV(TNUM,9,LOOPING(TNUM)) = PECCMRLN(RGN,IY) + PECCMMEN(RGN,IY) + PECCMTXN(RGN,IY) + PECCMTDN(RGN,IY)
              ELSEIF (ISECT .EQ. 3) THEN
                COLV(TNUM,5,LOOPING(TNUM)) = PECINRLN(RGN,IY)
                COLV(TNUM,6,LOOPING(TNUM)) = PECINMEN(RGN,IY)
                COLV(TNUM,7,LOOPING(TNUM)) = PECINTXN(RGN,IY)
                COLV(TNUM,8,LOOPING(TNUM)) = PECINTDN(RGN,IY)
                COLV(TNUM,9,LOOPING(TNUM)) = PECINRLN(RGN,IY) + PECINMEN(RGN,IY) + PECINTXN(RGN,IY) + PECINTDN(RGN,IY)
              ELSEIF (ISECT .EQ. 5) THEN
                COLV(TNUM,5,LOOPING(TNUM)) = PECASRLN(RGN,IY)
                COLV(TNUM,6,LOOPING(TNUM)) = PECASMEN(RGN,IY)
                COLV(TNUM,7,LOOPING(TNUM)) = PECASTXN(RGN,IY)
                COLV(TNUM,8,LOOPING(TNUM)) = PECASTDN(RGN,IY)
                COLV(TNUM,9,LOOPING(TNUM)) = PECASRLN(RGN,IY) + PECASMEN(RGN,IY) + PECASTXN(RGN,IY) + PECASTDN(RGN,IY)
              ENDIF
              IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
                COLVALS(:,:) = COLV(TNUM,:,:)
!               CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                LOOPING(TNUM) = 0
              ENDIF
            ENDDO
            ENDDO
!
        ENDIF                    ! end if itype (owner) = 3 (private + public)
      ENDIF                    ! end if indoc = 4
!
!       IF (ITYPE .EQ. 3) THEN
!     Write EFP EPRICE Output Table by Demand Sector and Stage of Productions (EFP_EPRICE)
!
!           TNUM = 6
!           IF (LOOPING(TNUM) .EQ. 0) THEN
!             NUMCOLS(TNUM) = 8
!             DYNSTM(TNUM) = 'INSERT INTO EFP_EPRICE VALUES(?,?,?,?,?,?,?,?,?)'
!           ENDIF
!           DO IY = 1 , LASTYR
!             CALL GETRP2(NRGN,IY)
!             DO ISECT = 1 , 5
!               LOOPING(TNUM) = LOOPING(TNUM) + 1
!               COLV(TNUM,1,LOOPING(TNUM)) = IY
!               COLV(TNUM,2,LOOPING(TNUM)) = NRGN
!               COLV(TNUM,3,LOOPING(TNUM)) = CURITR
!               COLV(TNUM,4,LOOPING(TNUM)) = ISECT
!               COLV(TNUM,5,LOOPING(TNUM)) = INDOC
!               COLV(TNUM,6,LOOPING(TNUM)) = EPRICE(ISECT,INDOC,NRGN)
!               COLV(TNUM,7,LOOPING(TNUM)) = BPRICE(ISECT,INDOC,NRGN)
!               COLV(TNUM,8,LOOPING(TNUM)) = FPRICE(ISECT,INDOC,NRGN)
!               IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
!                 COLVALS(:,:) = COLV(TNUM,:,:)
!                 CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
!                 LOOPING(TNUM) = 0
!               ENDIF
!             ENDDO
!           ENDDO
!       ENDIF
!
!
!     Write EFP Pricing Output Table by Demand Sector (EFP_CAPCOMP) table to database
!
          TNUM = 7
          IF (LOOPING(TNUM) .EQ. 0) THEN
            NUMCOLS(TNUM) = 14
            DYNSTM(TNUM) = 'INSERT INTO EFP_CAPCOMP VALUES(?,?,?,?,?,?,?,?,?,?,?,'     &
                        //'?,?,?,?)'
            WRTSTM(TNUM) = 'EFP_CAPCOMP'
          ENDIF
          DO IY = UESTYR - UHBSYR , LASTYR
!
!     Calculate Regulated Prices from temporary report arrays
!     Replicating pricing output table ( capital related components of price) from efprpt.txt
            REVRES = 0.0
            REVULT = TXINCS(IY,1) - REVRES
            PPRICE(IY) = REVULT/TSALES(IY,1)
            FUEL = (TXINCS(IY,3) - TSALES(IY,2))/TSALES(IY,1)
            OM = TXINCS(IY,2)/TSALES(IY,1)
            WHO = TSALES(IY,2)/TSALES(IY,1)
            TOTCAP = PPRICE(IY) - FUEL - OM - WHO
            DEP = TXINCS(IY,4)/TSALES(IY,1)
            TAXESI = TXINCS(IY,7)/TSALES(IY,1)
            TAXESO = TXINCS(IY,6)/TSALES(IY,1)
            RET = TOTCAP - DEP - TAXESI - TAXESO
!     Replicating pricing output table ( components of price, util & nonutil) from efprpt.txt
!     subroutine EFPREALDOLLARS should already have run, so the variables are already in reporting year dollars (real)
            FUELTTL = (TXINCS(IY,3)-TSALES(IY,2) + TPPWRBL(1,IY))/ &
                      TSALES(IY,1)
            OMTTL = (TXINCS(IY,2) + TPPWRBL(2,IY)) / TSALES(IY,1)
            CAPTTL = PPRICE(IY)-((TXINCS(IY,3)-TSALES(IY,2)) / &
                     TSALES(IY,1)) &
                     - TXINCS(IY,2)/TSALES(IY,1) &
                     - TSALES(IY,2)/TSALES(IY,1) &
                     + TPPWRBL(3,IY) / TSALES(IY,1)
            TOTWS = TPPWRBL(4,IY)/TSALES(IY,1)
            LOOPING(TNUM) = LOOPING(TNUM) + 1
            COLV(TNUM,1,LOOPING(TNUM)) = IY
            COLV(TNUM,2,LOOPING(TNUM)) = RGN
            COLV(TNUM,3,LOOPING(TNUM)) = CURITR
            COLV(TNUM,4,LOOPING(TNUM)) = ITYPE
            COLV(TNUM,5,LOOPING(TNUM)) = INDOC
            COLV(TNUM,6,LOOPING(TNUM)) = CAPTTL
            COLV(TNUM,7,LOOPING(TNUM)) = FUELTTL
            COLV(TNUM,8,LOOPING(TNUM)) = OMTTL
            COLV(TNUM,9,LOOPING(TNUM)) = TOTWS
            COLV(TNUM,10,LOOPING(TNUM)) = PPRICE(IY)
            COLV(TNUM,11,LOOPING(TNUM)) = DEP
            COLV(TNUM,12,LOOPING(TNUM)) = RET
            COLV(TNUM,13,LOOPING(TNUM)) = TAXESI
            COLV(TNUM,14,LOOPING(TNUM)) = TAXESO
            IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
              COLVALS(:,:) = COLV(TNUM,:,:)
!             CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
              CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
              LOOPING(TNUM) = 0
            ENDIF
          ENDDO
!
!      --- WRITE OUT ANY LEFT OVER RECORDS TO THE NEMS DATA BASE
!
       DO TNUM = 1 , NUMTABS
         IF (LOOPING(TNUM) .NE. 0) THEN
           COLVALS(:,:) = COLV(TNUM,:,:)
!          CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
           CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
           LOOPING(TNUM) = 0
         ENDIF
       ENDDO

      RETURN
      END
