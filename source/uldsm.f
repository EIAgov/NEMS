! $Header: m:/default/source/RCS/uldsm.f,v 1.79 2021/03/02 20:30:37 LC2 Exp $

  
!   uldsm.f contains the routines that make up the Load and Demand Side Management part of EMM
!   all routines written by Adam Kreczko, ICF Resources, Inc.
!       except DSMSHFT written by Pete Whitman
!              DSMECP1 by unknown
!          and sort routines from public domain

      SUBROUTINE ELLDSM(LDSMmode)
      
      USE EPHRTS_SWTICHES
      USE EPHRTS_FILE_UNIT_NUMBERS 

      IMPLICIT NONE

!  THIS SUBROUTINE IS THE MAIN LOAD AND DEMAND SIDE MANAGEMENT ROUTINE

      include 'parametr' !<< nems parameter declarations
      include 'dsmdimen' !<< all ldsm parameter declarations
      include 'emmparm'
      include 'ecpcntl'
      include 'control'
      include 'dsmunits' !<< include file with unit number for ldsm message file
      include 'dsmfmgrd' !<< file_mgr variables declarations
      include 'ncntrl'   !<< acces to nems global variables
      include 'dsmnercr' !<< nerc region data
      include 'dsmhelm'
      include 'dsmsectr'
      include 'dsmtfecp'
      include 'bildout'
      include 'bildin'
      include 'qblk'
      include 'efprp2'
      include 'dispout'  !<< dual solution of efd
      include 'dispinyr'
      include 'dsmrept'  !<< ldsm reports specification
      include 'dsmcaldr'
      include 'dsmtfefp'
      include 'dsmtoefd'
      include 'dsmnemsc' !<< results of ldsm to be passed to the rest of nems
      include 'wrenew'    ! mapping vars for ecp and efd hours to group number
      include 'eusprc'

      INTEGER*4 LDSMmode ! -1 - prepare data for ECP  every iteration
                         !  1 - prepare data for ECP only on first iteration
                         !  2 - process ECP solution
      LOGICAL WHOOPS ! ERROR FLAG
      INTEGER TMPRNB,I,J,K,L,M,b1,b2,SECT,d,h,CNB
      INTEGER CPUTIME1,CPUTIME2,CPUTSTRT,CPUTSTOP
      REAL Qdem,Histdem
      REAL CensusValues(MNUMCR),demout
      INTEGER C,IREG,IECPYR,FULLYR

      COMMON /REGSYSLD/LSRname_s,DistLo_s
      REAL*4 DistLo_s(MNUMNR,MAXHOUR)
      CHARACTER*8 LSRname_s(MNUMNR)    
      
!     other variables  needed for demand cumulant calculation

      real*8 pi,discr
      data pi/3.141592635/
      integer iprob
      LOGICAL E_DEBUG_EXIST
      IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN 
         INQUIRE(FILE="EPHRTS_DEBUG_FILE.TXT", EXIST=E_DEBUG_EXIST)
         IF (E_DEBUG_EXIST) THEN
            OPEN(unit_num_ephrts_debug_file, FILE="EPHRTS_DEBUG_FILE.TXT", STATUS="OLD", POSITION="APPEND", ACTION="WRITE")
         ELSE
            OPEN(unit_num_ephrts_debug_file, FILE="EPHRTS_DEBUG_FILE.TXT", STATUS="NEW", ACTION="WRITE")
         END IF
      END IF
      
!     Initializing Variables

      WHOOPS = .FALSE.
      FULLYR = USYEAR(CURIYR)

!     Body of the Program/Subprogram

      CALL MPTIM2(CPUTSTRT)

!     On the very first iteration of NEMS run:

      TotDemNERC = 0.0
      IF (FULLYR .EQ. UESTYR .AND. CURITR .EQ. 1 .AND. LDSMmode .EQ. 1) THEN
         IF (ECP_D_FPH+26 .GT. MNXYR) THEN
            WRITE(6,*)'<)) Message from LDSM module:'
            WRITE(6,*)'<))) Processing interrupted because of ERROR'
            WRITE(IMSG,*)'<))) ECP_D_FPH+26 .GT. MNXYR, STOP PROCESSING'
            WRITE(6,*)'<))) Data passed by LDSM may be CORRUPTED'
            WRITE(6,*)'<))) Control returned to UTIL'
            RETURN
         END IF
         DO IREG = 1, MNUMNR
            DO IECPYR = 1, UNYEAR + ECP_D_XPH
               EPKSHFT(IREG,IECPYR) = 0.0
            END DO          
         END DO
         
!        Get system load percentages from sqlitedatabase here 
         write(6,*)'Calling callsysload'
         CALL CALLSYSLOAD  
               
!
!        The Call to DSMRST has moved to UTIL so that the Load Shape Definitions are Available Before RDCNTRL is Called
!
!        set up probability calculation for demand
!        calculate normal probability points

         discr = 0.0
         do iprob = 1,nprob
            dprob(iprob) = (1.0 / ((2.0 * pi) ** 0.5)) * exp(-1.0d0 * dincr(iprob) * dincr(iprob) / 2.0) /2.0
            discr = discr + dprob(iprob)
         end do
         dprob(1) = dprob(1) + (1.0D0 - discr)/2.0D0
         dprob(nprob) = dprob(nprob) + (1.0D0 - discr)/2.0D0
      END IF

      WRITE(IMSG,*)'<))) ITERATION NUMBER,YEAR NUMBER:',CURITR,CURIYR
      WRITE(IMSG,*)'<))) LDSMmode:',LDSMmode
      IF (LDSMmode .LT. 2) THEN   ! LDSM is run to prepare data for EFP,ECP,EFD

         CALL DSMCADJ ! Calculate adjustment factors for COMMERCIAL LOAD

!        Initialize EFD Group and Segment Mapping Arrays

         ULGRP = 0
         ULSEG = 0
         
         DO RNB = 1,nNERCreg

!           Do for each NERC region to be processed.

!           Because RNB is a parameter of a global nature -- used by many levels of the routines within the loop, its value
!           is passed through a common block, rather then as an explicit subroutine parameter.

!           for historical years, calculate nerc region load adj factor from history
!           sum all sector demands, create share based on all sector historical

            if (((CURIYR+UHBSYR) .LE. UYR_HIST) .AND. ((CURIYR+UHBSYR) .GE. HSTYEAR)) then
               Qdem = 0.0
               DO C = 1,nCENSUSreg
                  CensusValues(C) = QELRS(C,CURIYR)
               END DO
               CALL DSMEMMV(CensusValues,RNB,SEC(RES),demout)
               Qdem = Qdem + demout

               DO C = 1,nCENSUSreg
                  CensusValues(C) = QELCM(C,CURIYR)
               END DO
               CALL DSMEMMV(CensusValues,RNB,SEC(COM),demout)
               Qdem = Qdem + demout

               DO C = 1,nCENSUSreg
                  CensusValues(C) = QELIN(C,CURIYR)
               END DO
               CALL DSMEMMV(CensusValues,RNB,SEC(IND),demout)
               Qdem = Qdem + demout

               DO C = 1,nCENSUSreg
                  CensusValues(C) = QELTR(C,CURIYR)
               END DO
               CALL DSMEMMV(CensusValues,RNB,SEC(TRA),demout)
               Qdem = Qdem + demout

!              convert to billion kwh

               Qdem = Qdem * UNCONFA/1000.0
               Histdem = HELASNR(RNB,CURIYR)
               Histshr = Histdem/Qdem

               write(IMSG,*) 'Hist dem :',curiyr,rnb,histdem,qdem,histshr
            END IF   ! UYRHIST

            K1 = CURIYR

!           Set base year

            UBASEYR = CURIYR

            CALL DSMFOR(WHOOPS)   ! Prepare load forecast for current year

            CALL DSMDLT
            IF (WHOOPS) THEN
               WRITE(6,*)'<)) Message from LDSM:'
               WRITE(6,*)'<))) Processing interrupted because of an ERROR'
               WRITE(6,*)'<))) Data passed by LDSM may be CORRUPTED'
               WRITE(6,*)'<))) Control returned to UTIL'
               RETURN
            END IF
            IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN 
               WRITE(unit_num_ephrts_debug_file,*) "CURIYR : ", CURIYR + 1989, " MAIN CALL FOR DSMHLM"
            END IF
            CALL DSMHLM   ! Run procedures which develop system load

! New prints to get 864 load shapes out

         I=0

!           IF (fcrl .EQ. 1 .and. CURIYR.ge. 19) WRITE(IMSG,10) RNB,CURIYR+1989

         Do m=1,12             ! months
            Do d=1,3              ! daytype

                  IF (fcrl .EQ. 1 .and. CURIYR .ge. 19) WRITE(IMSG,11) RNB,CURIYR+1989,m,d,(SYLOAD(h), h=I+1,I+24)          !writes to LDSMRPT

                  DO H = I+1 , I+24

                     IF (fcrl .EQ. 1 .and. CURIYR .ge. 19) WRITE(IMSG,1101) RNB, CURIYR+1989, m, d, h-I, IDAYTQ(d,m), HrToECPgrp(m,d,h-I), HrToECPseas(m,d,h-I), &
                        HrToEFDgrp(m,d,h-I), HrToEFDseas(m,d,h-I), SYLOAD(h)
 1101                FORMAT(1X,"SS4_SYSLOAD",10(":",I4),":",F21.6)

                     DUCK_SYSTEM_LOAD(m,d,h-I,RNB,CURIYR) = SYLOAD(h)
                  END DO
               I = I + 24
            Enddo           !daytype
         Enddo              !month

            IF (fcrl .EQ. 1 .and. CURIYR.ge. 19) THEN
         do SECT=1,NumSec         ! sector shapes (864)
           I=0
!           WRITE(IMSG,12) RNB,CURIYR+1989,SECT
           Do m=1,12             ! months
              Do d=1,3              ! daytype
                 WRITE(IMSG,12) RNB,CURIYR+1989,SECT,m,d,(SECTORLOAD(h,SECT), h=I+1,I+24)          !writes to LDSMRPT
                 I = I + 24
              Enddo           !daytype
           Enddo              !month
         Enddo                 !sector
            END IF


      IF (((RNB.eq. 9).or. (RNB .eq. 20)) .and. ((CURIYR.eq. 21).or. (CURIYR .eq. 46)) .and. (fcrl .EQ. 1)) THEN
!          WRITE(IMSG,*) RNB,CURIYR+1989,' Energy by EU'
          DO J = 1, MNEUGRP               ! neusgrp(icls)?  total number of end-uses
            DO L=1,EFD_D_MSP    ! seasons
                WRITE(IMSG,13) RNB,CURIYR+1989,J,L, (efdblkserv(b1,L,RNB,J),b1=1,3)
            ENDDO
          ENDDO
      ENDIF   ! region and year

10 FORMAT('SGRIDLD ',' region ',I2,', year ',i4)
11 FORMAT('TSYSLD: ',i2,':',i5,':',2x,2(':',i2),24(':',f8.3))  ! add dummy col so lines up with sector load tables
12 FORMAT('SECTLD: ',i2,':',i5,3(':',i2),24(':',f8.3))
!12 FORMAT('SGRIDLD:',' region ',I2,', year ',i4,' sector ',i4)
13 FORMAT('SDEMEU ',2i4,2i2,3f10.2)

            CALL DSMEFP   ! Prepare data required by EFP

            CALL DSMTOR   ! Prepare sectorial variables for reports

            CALL DSMEFD   ! Develop LDC's for EFD

            IF (FCRL .EQ. 1) THEN

               IF (NODSMRPT(1)) CALL DSMREP1 ! STORE ANNUAL LOAD RESULTS

            END IF

!           IF (FCRL .EQ. 1 .AND. NOREGRPT(RNB) .AND. NOYEARRPT(CURIYR)) THEN

            IF ((CURITR .EQ. 1.OR.LDSMmode .EQ. -1) .AND. (CURIYR .GE. UPSTYR-BASEYR+1)) THEN

               CALL DSMECP1(WHOOPS)  !Develop LDC's and DSM Program data for ECP

               IF (WHOOPS) THEN
                  WRITE(6,*)'<)) Message from LDSM module:'
                  WRITE(6,*)'<))) Processing interrupted because of ERROR'
                  WRITE(6,*)'<))) Data passed by LDSM may be CORRUPTED'
                  WRITE(6,*)'<))) Control returned to UTIL'
                  RETURN
               END IF
            END IF
         END DO
      END IF

      IF (CURIYR .EQ. LASTYR .AND. FCRL .EQ. 1 .AND. LDSMmode .NE. 2) THEN
         IF (NODSMRPT(1)) THEN
            DO RNB = 1,nNERCreg
               WRITE(IMSG,*)
               WRITE(IMSG,*)'TAB.1 ANNUAL LOAD RESULTS FOR REGION: ',NERCnam(RNB)
               WRITE(IMSG,1)
               WRITE(IMSG,2)
     1         FORMAT('YEAR',2X,'  PEAK  ',2X,' ENERGY ',1X,'LOAD FACTOR',1X,'-- TIME OF PEAK --',1X,'- SALES DISTRIBUTION (%) ---')
     2         FORMAT('    ',2X,'  (MW)  ',2X,' (GWH)  ',1X,'   (%)     ',1X,'MONTH DAYTYPE HOUR',1X,'RES   COMM  IN    TRAN  T&D ')
     3         FORMAT(I4,1(2X,F8.0),2X,f10.0,2X,F4.1,5X,A3,2X,A8,1X,I2,1X,5F6.1)

               DO L = UESTYR-UHBSYR,LASTYR
                  WRITE(IMSG,3)BASEYR+L-1,REP1SP(RNB,L),REP1TL(RNB,L), &
                     REP1LF(RNB,L),MONAME(REP1MO(RNB,L)),DTNAME(REP1JD(RNB,L)), &
                     REP1HO(RNB,L),(REP1SC(RNB,L,I),I = 1,NumSec),REP1TD(RNB,L)
               END DO
            END DO
         END IF
         CALL DSMSRP
      END IF
      WRITE(IMSG,*)'<)) Message from subroutine ELLDSM'
      WRITE(IMSG,*)'<))) Execution successfully completed'
      CALL MPTIM2(CPUTSTOP)
      WRITE(IMSG,*)'LDSM TOTAL CPUTIME = ',CPUTSTOP-CPUTSTRT
      IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN 
         CLOSE(unit_num_ephrts_debug_file)
      END IF
      RETURN
      END

      SUBROUTINE DSMREP1

!****************** Description of the Program/Subprogram **********************
! THIS SUBROUTINE STORES DATA FOR THE ANNUAL LOAD RESULT REPORT
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr' !<< nems parameter declarations
      include 'ncntrl' !<< global variables for nems
      include 'emmparm'
      include 'dispett'
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen' !<< all ldsm parameter declarations
      include 'dsmunits' !<< include file with unit number for ldsm message file
      include 'dsmhelm' !<< communication within helm algorithm
      include 'dsmcaldr' !<< calendar data
      include 'dsmnercr'
      include 'dsmsectr'
      include 'dsmtfefp'
      include 'dsmrept'
!********************** Declaring local variables *****************************
      INTEGER*2 I,J,K,L,M,h ! universal counters
      REAL*4 SECLOAD(MAXSEC)
!****************** Initializing Variables *************************************
      K=0
      L=0
      M=0
!****************** Body of the Program/Subprogram *****************************
      h=0
      DO K=1,NMONTH
        DO L=1,NODAYS(K)
          DO M=1,24
            h=h+1
            IF(SystemPeakHour(CURIYR).EQ.h) GOTO 100
          ENDDO
        ENDDO
      ENDDO
100   CONTINUE
      DO I=1,NUMSEC
        SECLOAD(I)=0.0
        DO J=EUINDEX(I,1),EUINDEX(I,2)
          SECLOAD(I)=SECLOAD(I)+LoadForec(J,1)
        ENDDO
      ENDDO
      REP1SP(RNB,CURIYR)=SystemPeak(CURIYR)*1000.
      REP1TL(RNB,CURIYR)=TotSystemLoad(CURIYR)
      REP1LF(RNB,CURIYR)=SystemLoadFactor(CURIYR)*100.
      REP1MO(RNB,CURIYR)=K
      REP1JD(RNB,CURIYR)=JDAYTP(L,K)
      REP1HO(RNB,CURIYR)=M
      DO I=1,NumSec
        REP1SC(RNB,CURIYR,I)=SECLOAD(I)/TotSystemLoad(CURIYR)*100.
      ENDDO
      REP1TD(RNB,CURIYR)=(NERCtdloss(RNB)*ULOSSADJ(CURIYR))*100.
!****************** Termination of the Program/Subprogram **********************
      RETURN
      END

      SUBROUTINE DSMECP1(WHOOPS)
      USE EPHRTS_SWTICHES
      USE EPHRTS_FILE_UNIT_NUMBERS 

      IMPLICIT NONE

!     THIS SUBROUTINE PREPARES DATA FOR THE FORESIGHT YEARS FOR ECP

!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr' !<< nems parameter declarations
      include 'ncntrl'   !<< access to nems global variables
      include 'emmparm'  !<< emm parameters
      include 'control'
      include 'ecpcntl'  !<< upstyr variable
      include 'comparm'  !<< defines parameters: cmnumbldg,cmnumserv
      include 'bildin'
      include 'qblk'
      include 'mxqblk' !<< foresight data
      include 'dispinyr'
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen' !<< all ldsm parameter declarations
      include 'dsmhelm'
      include 'dsmtoefd'
      include 'dsmnemsc'
      include 'dsmunits'
      include 'dsmsectr'
      include 'dsmcaldr'
      include 'dsmrept'  !<< ldsm reports specification
      include 'dsmnercr' !<< nerc region data
!********************** Declaring local variables *****************************
      LOGICAL WHOOPS ! error flag
      REAL Qdem,Histdem
      REAL CensusValues(MNUMCR),demout
      INTEGER C
      INTEGER*2 i,j,n ! counter
      LOGICAL E_DEBUG_EXIST

!****************** Initializing Variables *************************************
      ECPLastYearIndex=CURIYR+UNFPH-1 ! Calculate last year index for ECP run
!****************** Body of the Program/Subprogram *****************************

      IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN 
         INQUIRE(FILE="EPHRTS_DEBUG_FILE.TXT", EXIST=E_DEBUG_EXIST)
         IF (E_DEBUG_EXIST) THEN
            OPEN(unit_num_ephrts_debug_file, FILE="EPHRTS_DEBUG_FILE.TXT", STATUS="OLD", POSITION="APPEND", ACTION="WRITE")
         ELSE
            OPEN(unit_num_ephrts_debug_file, FILE="EPHRTS_DEBUG_FILE.TXT", STATUS="NEW", ACTION="WRITE")
         END IF
      END IF      
      
      DO K1 = CURIYR , ECPLastYearIndex ! do for all future years of ECP horizon
                                        ! Because K1 is used by many levels of routines within
                                        ! the loop, it is defined in a common block in dsmhelm
                                        ! rather then explicitly in this subroutine
                                        ! Same deal with UBaseYR

         if (((K1+UHBSYR) .LE. UYR_HIST) .AND. ((K1+UHBSYR) .GE. HSTYEAR)) then
            Qdem = 0.0
            DO C = 1,nCENSUSreg
               IF (K1 .GT. UNYEAR) THEN
                 CENSUSvalues(C)=XQELRS(C,K1)
               ELSE
                 CENSUSvalues(C)=QELRS(C,K1)
               END IF
            END DO
            CALL DSMEMMV(CensusValues,RNB,SEC(RES),demout)
            Qdem = Qdem + demout

            DO C = 1,nCENSUSreg
               IF (K1 .GT. UNYEAR) THEN
                 CENSUSvalues(C)=XQELCM(C,K1)
               ELSE
                 CENSUSvalues(C)=QELCM(C,K1)
               END IF
            END DO
            CALL DSMEMMV(CensusValues,RNB,SEC(COM),demout)
            Qdem = Qdem + demout

            DO C = 1,nCENSUSreg
               IF (K1 .GT. UNYEAR) THEN
                 CENSUSvalues(C)=XQELIN(C,K1)
               ELSE
                 CENSUSvalues(C)=QELIN(C,K1)
               END IF
            END DO
            CALL DSMEMMV(CensusValues,RNB,SEC(IND),demout)
            Qdem = Qdem + demout

            DO C = 1,nCENSUSreg
               IF (K1 .GT. UNYEAR) THEN
                 CENSUSvalues(C)=XQELTR(C,K1)
               ELSE
                 CENSUSvalues(C)=QELTR(C,K1)
               END IF
            END DO
            CALL DSMEMMV(CensusValues,RNB,SEC(TRA),demout)
            Qdem = Qdem + demout

!           convert to billion kwh

            Qdem = Qdem * UNCONFA/1000.0
            Histdem = HELASNR(RNB,K1)
            Histshr = Histdem/Qdem

            write(IMSG,*) 'Hist dem :',K1,rnb,histdem,qdem,histshr
         END IF   ! UYRHIST

         UBASEYR = MIN(K1 , UNYEAR)
         CALL DSMFOR(WHOOPS)   ! Prepare load forecast

         CALL DSMDLT
         IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN 
            WRITE(unit_num_ephrts_debug_file,*) "CURRENT YEAR", CURIYR, " FORESIGHT YEAR", K1, " CALL FOR DSMHLM"
         END IF
         CALL DSMHLM   ! Run procedures which develop system load
         IF (K1 .EQ. CURIYR)  CALL DSMTOR   ! Prepare sectorial variables for reports - adjust totsecload for DGPV
         CALL DSMLCP(1)   ! Develop LDC for ECP
      ENDDO

      IF (NODSMRPT(3)) THEN
         CALL DSMREP3 ! WRITE ECP LDC
      ENDIF

!     REPLACE PREVIOUS YEAR INFORMATION ABOUT THE ORDER OF THE BLOCKS IN ECP LDC WITH THE CURRENT YEAR INFORMATION.

      DO i = 1 , MAXECTB
         DO j = 1 , UNXPH-3
            BlockNum3(i,RNB,j,1)=BlockNum3(i,RNB,j,2)
         ENDDO
      ENDDO

!     Ensure ubaseyr reset to curiyr just in case

      UBASEYR = CURIYR
      IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN 
         CLOSE(unit_num_ephrts_debug_file)
      END IF
      RETURN
      END

      SUBROUTINE DSMHLC(SYLOAD1,WHENQSR)

!****************** Description of the Program/Subprogram **********************
! THIS SUBROUTINE WRITES CURRENT REGIONAL HOURLY LOAD CURVES FOR THE 36 TYPICAL DAY TYPES TO LDSMRPT.txt
!    LDSMRPT.txt is opened in util.f using IMSG to hold the unit number
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr' !<< nems parameter declarations
      include 'ncntrl'
      include 'emmparm'
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen' !<< all ldsm parameter declarations
      include 'dsmunits' !<< include file with unit number for ldsm message file
      include 'dsmhelm' !<< communication within helm algorithm
      include 'dsmtfecp' !<< communication with ecp
      include 'dsmtoefd'
      include 'dsmnemsc'
      include 'dsmcaldr'
      include 'dsmnercr'
!********************** Declaring local variables *****************************
      INTEGER*2 i,j,k,l,h ! universal counters
      REAL*4 SYLOAD1(MAXHOUR) ! current load for calendar hours
      CHARACTER*6 WHENQSR
!****************** Initializing Variables *************************************
!****************** Body of the Program/Subprogram *****************************
      WRITE(IMSG,'(A,I4,A,I4,A,I4,3A)') ' REGIONAL HOURLY LOAD [GWh] for EMM region=',RNB,  &
         '; NEMS year= ',CURCALYR,'; ECP TIME HORIZON year= ',K1+1989,'  (',WHENQSR,' DSMQSR)'
      h=0
      DO i=1,12
        DO j=1,3
          l=h+1
          h=h+24
! breaking hourly write into two two with half in each:
          WRITE(IMSG,'("   SYLOAD1",12(":",f5.1),":",A,":",A)')(SYLOAD1(k),k=l,l+11),MONAME(i),trim(DTNAME(j))
          WRITE(IMSG,'("   SYLOAD1",12(":",f5.1),":",A      )')(SYLOAD1(k),k=l+12,h),MONAME(i)
        ENDDO
      ENDDO
!****************** Termination of the Program/Subprogram **********************
      RETURN
      END

      SUBROUTINE DSMREP3

!****************** Description of the Program/Subprogram **********************
! THIS SUBROUTINE WRITES CURRENT ECP LOAD DURATION CURVES TO LDSMRPT.txt
!    LDSMRPT.txt is opened in util.f using IMSG to hold the unit number
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr' !<< nems parameter declarations
      include 'ncntrl'
      include 'emmparm'
      include 'ecpcntl'
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen' !<< all ldsm parameter declarations
      include 'dsmunits' !<< include file with unit number for ldsm message file
      include 'dsmhelm' !<< communication within helm algorithm
      include 'dsmtfecp' !<< communication with ecp
      include 'dsmtfefp'
      include 'dsmtoefd'
      include 'dsmnemsc'
      include 'dsmcaldr'
      include 'dsmnercr'
      include 'dsmsectr'
      include 'dsmrept'
!********************** Declaring local variables *****************************
      INTEGER*2 I,K,seg,sli,sea,slitot ! universal counters
      REAL*4 MW(ECP_D_XPH)     ! MWatts
      INTEGER*4 RANK(ECP_D_XPH)! rank
      CHARACTER*14 TEXT1 ! TEXT FOR HEADER
      CHARACTER*17 TEXT2 ! TEXT FOR HEADER
      CHARACTER*17 TEXT3 ! TEXT FOR HEADER
      INTEGER*4 y,year
!****************** Initializing Variables *************************************
      TEXT1='   MW     RANK'
      TEXT2='STRIP DESCRIPTION'
      TEXT3='                 '
!****************** Body of the Program/Subprogram *****************************
      WRITE(IMSG,*)
      WRITE(IMSG,*)'TAB.3 ECP LOAD DURATION CURVE FOR REGION: ', &
      NERCnam(RNB) &
      ,'  YEAR:',BASEYR+CURIYR-1
1     FORMAT(A17,12(2X,A14))
3     FORMAT(A17,12(2X,'-----',I4,'-----'))
      WRITE(IMSG,3)TEXT2,(CURIYR+I-2+BASEYR,I=1,UNXPH)
      WRITE(IMSG,1)TEXT3,(TEXT1,I=1,UNXPH)
      DO seg = 1 , ECPnumSg
        DO sli = 1 , ECPsgDnB(seg)
          DO y = 1 , UNXPH
            year = CURIYR + y - 1
            DO slitot = 1 , ECPnumBl
              IF (ECPLDCBS(year,RNB,slitot) .EQ. seg .AND.  ECPLDCSL(year,RNB,slitot) .EQ. sli) THEN
                MW(y)=ECPLDCBH(year,RNB,slitot)*1000.
                RANK(y)=slitot
                GOTO 200
              ENDIF
            ENDDO
200         CONTINUE
          ENDDO
          K=ECPsgDblock(seg,sli)
2         FORMAT(A12,1X,I3,'%',12(2X,F8.0,I5,1X))
          WRITE(IMSG,2)ECPSEGDSC(seg),K,(MW(y),RANK(y),y=1,UNXPH)
        ENDDO
      ENDDO
!****************** Termination of the Program/Subprogram **********************
      RETURN
      END

      SUBROUTINE DSMREP4

!****************** Description of the Program/Subprogram **********************
! THIS SUBROUTINE WRITES CURRENT EFD LOAD DURATION CURVES TO LDSMRPT.txt
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr' !<< nems parameter declarations
      include 'ncntrl'
      include 'emmparm'
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen' !<< all ldsm parameter declarations
      include 'dsmunits' !<< include file with unit number for ldsm message file
      include 'dsmhelm' !<< communication within helm algorithm
      include 'dsmtoefd' !<< communication with ecp
      include 'dsmtfefp'
      include 'dsmnemsc'
      include 'dsmcaldr'
      include 'dsmnercr'
      include 'dsmsectr'
      include 'dsmrept'
!********************** Declaring local variables *****************************
      INTEGER*2 I,K,seg,sli,sea,slitot,KK ! universal counters
      REAL*4 MW(MAXEFDS)     ! MWatts
      INTEGER*4 RANK(MAXEFDS)! rank
      CHARACTER*10 EFDSEGDSC1(MAXEFDSS) ! EFD SEG DESCRIPTIONS ALREADY PROCESSED
      CHARACTER*14 TEXT1 ! TEXT FOR HEADER
      CHARACTER*17 TEXT2 ! TEXT FOR HEADER
      CHARACTER*17 TEXT3 ! TEXT FOR HEADER
!****************** Initializing Variables *************************************
      TEXT1='   MW     RANK'
      TEXT2='STRIP DESCRIPTION'
      TEXT3='                 '
      KK=0
!****************** Body of the Program/Subprogram *****************************
      WRITE(IMSG,*)
      WRITE(IMSG,*)'TAB.4 EFD LOAD DURATION CURVE FOR REGION: ', &
      NERCnam(RNB) &
      ,'  YEAR:',BASEYR+CURIYR-1
1     FORMAT(A17,12(2X,A14))
      WRITE(IMSG,1)TEXT2,(SEASONEFD(I),I=1,EFDnS)
      WRITE(IMSG,1)TEXT3,(TEXT1,I=1,EFDnS)
      DO seg=1,EFDnumSg
        DO K=1,KK
          IF(EFDSEGDSC(seg).EQ.EFDSEGDSC1(K))  GOTO 300
        ENDDO
        KK=KK+1
        EFDSEGDSC1(KK)=EFDSEGDSC(seg)
        DO sli=1,EFDsgDnB(seg)
          DO sea=1,EFDnS
            DO slitot=1, ULNVCT(SEA)
              IF(EFDSEGDSC(ULGRP(slitot,sea,RNB)).EQ.EFDSEGDSC(seg).AND. &
              ULSEG(slitot,sea,RNB).EQ.sli) THEN
                MW(sea)=ULHGHT(slitot,sea,RNB)*1000.
                RANK(sea)=slitot
                GOTO 200
              ENDIF
            ENDDO
200         CONTINUE
          ENDDO
          K=EFDsgDblock(seg,sli)
2         FORMAT(A12,1X,I3,'%',12(2X,F8.0,I5,1X))
          WRITE(IMSG,2)EFDSEGDSC(seg),K,(MW(sea),RANK(sea),sea=1,EFDnS)
        ENDDO
300     CONTINUE
      ENDDO
!****************** Termination of the Program/Subprogram **********************
      RETURN
      END

      SUBROUTINE DSMACM(WHOOPS)

!****************** Description of the Program/Subprogram **********************
! THIS SUBROUTINE CREATES LOAD FORECASTS FOR EACH COMMERCIAL SECTOR END USE AND EMM REGION
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr' !<< nems parameter declarations
      include 'ncntrl' !<< global variables for nems
      include 'emmparm'
      include 'control'
      include 'comparm' !<< defines parameters: cmnumbldg,cmnumserv
      include 'qblk'
      include 'mxqblk' !<< foresight data
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen' !<< all ldsm parameter declarations
      include 'dsmunits' !<< include file with unit number for ldsm message file
      include 'dsmnercr' !<< nerc region data
      include 'dsmhelm' !<< helm algorithm variables
      include 'dsmsectr' !<< sector secific data and other associated variables
      include 'tranrep'
!********************** Declaring local variables *****************************
      REAL*4 load ! total load
      REAL*4 adjload
      INTEGER*2 l,i,j,k ! universal counters
      LOGICAL WHOOPS ! error flag
      REAL*4 CENSUSvalues(MAXCRG) ! temporary vector with CENSUS division values
      REAL*4 FORESIGHTadj ! FORESIGHT adjustment factor
      REAL*4 ComLK1 ! commercial demand for RNB year K1
      REAL*4 ComLUBASEYR ! commercial demand for RNB year CURIYR
      REAL*8 tot_census_b,tot_census_f

!****************** Initializing Variables *************************************
!****************** Body of the Program/Subprogram *****************************
      l=EUINDEX(SEC(COM),1)
!     write(IMSG,*)'ubase,k1',UBASEYR,K1
! Calculate current year FORESIGHT adjustment factor
      tot_census_b = 0.0
      tot_census_f = 0.0
      DO k=1,nCENSUSreg
        IF (K1 .GT. UNYEAR) THEN
        CENSUSvalues(k)=XQELCM(k,K1)
        ELSE
          CENSUSvalues(k)=QELCM(k,K1)
        END IF
        tot_census_f = tot_census_f + CENSUSvalues(k)
      ENDDO
!     WRITE(18,1001) CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,(CENSUSvalues(k),k=1,nCENSUSREG)
!1001 FORMAT(1X,"QELCM_K1",T20,3(":",I4),10(":",F12.3))
      CALL DSMEMMV(CENSUSvalues,RNB,SEC(COM),ComLK1)
      DO k=1,nCENSUSreg
        IF (UBASEYR .GT. UNYEAR) THEN
        CENSUSvalues(k)=XQELCM(k,UBASEYR)
        ELSE
          CENSUSvalues(k)=QELCM(k,UBASEYR)
        END IF
        tot_census_b = tot_census_b + CENSUSvalues(k)
      ENDDO
!     WRITE(18,1002) CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,(CENSUSvalues(k),k=1,nCENSUSREG)
!1002 FORMAT(1X,"QELCM_BS",T20,3(":",I4),10(":",F12.3))
      CALL DSMEMMV(CENSUSvalues,RNB,SEC(COM),ComLUBASEYR)
      IF(ComLUBASEYR.EQ.0) THEN
        FORESIGHTadj=0.0
      ELSE
        FORESIGHTadj=ComLK1/ComLUBASEYR * DEMCECM (k1 - CURIYR + 1)
      ENDIF
      DO i=1,CMnumBldg
        DO j=1,CMnumServ
! Aggregate CENSUS regions load forecast into NERC region forecasts by end-use
          IF(j.EQ.CMnumServ) THEN
            DO k=1,nCENSUSreg
                CENSUSvalues(k)=EndUseConsump(ELINDEX,j,i,k,UBASEYR)*(1.0+ComDemAdjFac(k,1,UBASEYR))
            ENDDO
!           WRITE(18,1003) i,j,CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,(CENSUSvalues(k),k=1,nCENSUSREG)
!1003       FORMAT(1X,"CM_EU1_",I3,"_"I3,T20,3(":",I4),10(":",F12.3))
          ELSE
            DO k=1,nCENSUSreg
                CENSUSvalues(k)=EndUseConsump(ELINDEX,j,i,k,UBASEYR)*(1.0+ComDemAdjFac(k,2,UBASEYR))
            ENDDO
!           WRITE(18,1004) i,j,CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,(CENSUSvalues(k),k=1,nCENSUSREG)
!1004       FORMAT(1X,"CM_EU2_",I3,"_"I3,T20,3(":",I4),10(":",F12.3))
          ENDIF
          LFinum=LFinum+1
          CALL DSMEMMV(CENSUSvalues,RNB,SEC(COM),load)
          LoadForec(LFinum,1)=load*UNCONFA*FORESIGHTadj
!       write(imsg,*)'DSMACM:loadforec,lfinum',
!    +    LoadForec(LFinum,1),LFinum
          l=l+1
        ENDDO
      ENDDO

          ! EV loops
          DO i=2,9 !number of ev types
              if (i .EQ. 7 .OR. i .EQ. 8) then !IB type = 6,7,8, will add cumulatively if > 
                  DO k=1,nCENSUSreg
                      CENSUSvalues(k)= CENSUSvalues(k) + TRQ_ELEC(i,k,UBASEYR)
                  enddo
              else
                  DO k=1,nCENSUSreg
                      CENSUSvalues(k)= TRQ_ELEC(i,k,UBASEYR)
                  enddo
              endif
                  
              if (i .LT. 6 .OR. i .GT. 7) then !if IB type, final=8
                  LFinum=LFinum+1
                  CALL DSMEMMV(CENSUSvalues,RNB,SEC(COM),load)
                  LoadForec(LFinum,1)=load*UNCONFA*FORESIGHTadj
                  l=l+1
              endif
          ENDDO
      
      WRITE(18,1005) CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,(ComDemAdjFac(k,1,UBASEYR),k=1,nCENSUSREG)
1005  FORMAT(1X,"CM_AF_1",T20,3(":",I4),10(":",F12.3))
      WRITE(18,1006) CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,(ComDemAdjFac(k,2,UBASEYR),k=1,nCENSUSREG)
1006  FORMAT(1X,"CM_AF_2",T20,3(":",I4),10(":",F12.3))
      l=l-1
      
      IF(l.NE.EUINDEX(SEC(COM),2)) GOTO 999
!****************** Termination of the Program/Subprogram **********************
1009  FORMAT(1x,A12,3I6,9F15.3)
1010  FORMAT(1x,A12,3I6,F15.3)
!     ENDIF
      RETURN
999   WRITE(IMSG,*)'<))) Message from subroutine DSMACM'
      WRITE(IMSG,*)'<))) Wrong number of end-uses specified on ', &
            'LDSMSTRU file for COMMERCIAL sector, region: ',NERCnam(RNB)
      WHOOPS=.TRUE.
      RETURN
      END

      SUBROUTINE DSMAIN(WHOOPS)

!****************** Description of the Program/Subprogram **********************
! THIS SUBROUTINE CREATES LOAD FORECASTS FOR EACH INDUSTRIAL END USE AND EMM REGION
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr' !<< nems parameter declarations
      include 'ncntrl' !<< global variables for nems
      include 'emmparm'
      include 'control'
      include 'qblk' !<< access to qelin matrix
      include 'mxqblk' !<< foresight data
      include 'indout' !<< broken down industrial data
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen' !<< all ldsm parameter declarations
      include 'dsmunits' !<< include file with unit number for ldsm message file
      include 'dsmsectr' !<< sector secific data and other associated variables
      include 'dsmnercr' !<< nerc region data
      include 'dsmhelm' !<< helm algorithm variables
      include 'dsmcaldr'
!********************** Declaring local variables *****************************
      REAL*4 load ! total load
      INTEGER*2 i,k,l ! universal counters
      LOGICAL WHOOPS ! error flag
      REAL*4 CENSUSvalues(MAXCRG) ! temporary vector with CENSUS division values
      REAL*4 FORESIGHTadj ! FORESIGHT adjustment factor
      REAL*4 IndLK1 ! industrial load in region RNB, year K1
      REAL*4 IndLUBASEYR ! industrial load in region RNB, yearCURIYR
      REAL*8 tot_census_b,tot_census_f
!****************** Initializing Variables *************************************
!****************** Body of the Program/Subprogram *****************************

      l = EUINDEX(SEC(IND),1)

!     Calculate current year FORESIGHT adjustment factor

      tot_census_b = 0.0
      tot_census_f = 0.0
      DO k = 1 , nCENSUSreg
         IF (K1 .GT. UNYEAR) THEN
            CENSUSvalues(k) = XQELIN(k,K1)
         ELSE
           CENSUSvalues(k) = QELIN(k,K1)
         END IF
         tot_census_f = tot_census_f + CENSUSvalues(k)
      ENDDO

      CALL DSMEMMV(CENSUSvalues,RNB,SEC(IND),IndLK1)

      WRITE(IMSG,1001) CURIRUN, CURIYR+UHBSYR, K1+UHBSYR, UBASEYR+UHBSYR, RNB, l, ind, sec(ind), IndLK1, &
        tot_census_f, (CENSUSvalues(k),k=1,nCENSUSREG)
 1001 FORMAT(1X,"QELIN_K1",8(":",I4),25(":",F12.3))

      DO k = 1, nCENSUSreg
         IF (UBASEYR .GT. UNYEAR) THEN
            CENSUSvalues(k)=XQELIN(k,UBASEYR)
         ELSE
           CENSUSvalues(k)=QELIN(k,UBASEYR)
         END IF
         tot_census_b = tot_census_b + CENSUSvalues(k)
      ENDDO

      CALL DSMEMMV(CENSUSvalues,RNB,SEC(IND),IndLUBASEYR)

      WRITE(IMSG,1002) CURIRUN, CURIYR+UHBSYR, K1+UHBSYR, UBASEYR+UHBSYR, RNB, l, ind, sec(ind), &
         IndLUBASEYR, tot_census_b, DEMCEIN(k1-curiyr+1), (CENSUSvalues(k),k=1,nCENSUSREG)
 1002 FORMAT(1X,"QELIN_BS",8(":",I4),25(":",F12.3))

      IF (IndLUBASEYR .GT. 0.0) THEN
         FORESIGHTadj = IndLK1 / IndLUBASEYR * DEMCEIN(k1-CURIYR+1)
      ELSE
         FORESIGHTadj = 1.0
      END IF

!     write(imsg,*)'DSMAIN:foresightadj',FORESIGHTadj

!     Aggregate CENSUS regions load forecast into NERC region forecasts by end-use

      DO i=1,NEUSES(SEC(IND))
        DO k=1,nCENSUSreg
!
           IF (i.eq.4) THEN  !This takes care of the new Hydrogen end use from EPHRTS (can be removed once HMM/IDM populates QELINH variable (part of QELINOUT equivalence), or uses different HMM NERC region variable)
               !load = annual_h2_mwh_consumed(RNB) * (3.41214 / 1000000.0) !converting mwh to tbtu (same as QELIN)
               load = annual_h2_mwh_consumed(RNB)/ (1000.0 * UNCONFA)! mwhrs to tbtu, where UNCONFA is 1 tbu equals 292 Gwhrs 
           ELSE
             CENSUSvalues(k)=QELINOUT(k,UBASEYR,i)
           ENDIF

           IF (RNB .EQ. 1) &   ! this gets called every EMM region, but these Census numbers don't change
           WRITE(IMSG,1003) CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,CURITR,i,k, &
              QELINOUT(k,UBASEYR,i), CENSUSvalues(k), QELIN(k,UBASEYR), QELHM(k,UBASEYR)
 1003      FORMAT(1X,"IN_EU_1",6(":",I4),10(":",F12.3))

        ENDDO
        LFinum=LFinum+1
        IF (i.ne.4) THEN !HMM populates QELHM_HR variable (and thus annual_h2_mwh_consumed) in NERC regions, so doesn't need this mapping. QELHM (part QELINOUT equivalence) not used becasue of iterational variation
          CALL DSMEMMV(CENSUSvalues,RNB,SEC(IND),load) 
        ENDIF

        WRITE(IMSG,2003) CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,CURITR, i, LFinum, ind, SEC(IND), RNB, load, UNCONFA, FORESIGHTadj
 2003   FORMAT(1X,"IN_EU_2",9(":",I4),10(":",F12.3))

!       IF( USW_XP .EQ.0) THEN              ! normal nems
          LoadForec(LFinum,1)=load*UNCONFA*FORESIGHTadj
!       ELSE
!         LoadForec(LFinum,1)=load          !in Canadian run
!       ENDIF
! For now we use a matrix with total load forecast for INDUSTRIAL sector
! When the INDUSTRIAL sector model is ready it should be replaced with
! a matrix with forecast for each end-use like: INDDEM(year,reg,e-u)
        l=l+1
      ENDDO
      l=l-1
      
      IF(l.NE.EUINDEX(SEC(IND),2) .AND. I .LT. 5) GOTO 999 ! I EXITS AS I + 1, SO IF THERE'S FOUR SECTORS, IT WILL BE SET TO FIVE
!****************** Termination of the Program/Subprogram **********************
      RETURN
999   WRITE(IMSG,*)'<))) Message from subroutine DSMAIN'
      WRITE(IMSG,*)'<))) Wrong number of end-uses specified on ', &
      'LDSMSTRU file for INDUSTRIAL sector, region: ',NERCnam(RNB)
      WHOOPS=.TRUE.
      RETURN
      END

      SUBROUTINE DSMARE(WHOOPS)

!****************** Description of the Program/Subprogram **********************
! THIS SUBROUTINE CREATES LOAD FORECASTS FOR EACH RESIDENTIAL END USE AND EMM REGION
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr' !<< nems parameter declarations
      include 'ncntrl' !<< global variables for nems
      include 'emmparm'
      include 'control'
      include 'qblk'
      include 'mxqblk' !<< foresight data
      include 'rscon'  ! residential end-use consumption
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen' !<< all ldsm parameter declarations
      include 'dsmunits' !<< include file with unit number for ldsm message file
      include 'dsmnercr' !<< nerc region data
      include 'dsmsectr' !<< sector secific data and other associated variables
      include 'dsmhelm' !<< helm algorithm variables
      include 'tranrep'
!********************** Declaring local variables *****************************
      REAL*4 load ! temporary value with EMM region load
      REAL*4 adjload
      INTEGER*2 l,k ! universal counters
      INTEGER*2 tgn ! technology group = end-use number
      LOGICAL WHOOPS ! error flag
      REAL*4 CENSUSvalues(MAXCRG) ! temporary vector with CENSUS division values
      REAL*4 FORESIGHTadj ! FORESIGHT adjustment factor
      REAL*4 ResLK1 ! residential demand for current NERC region year K1
      REAL*4 ResLUBASEYR ! resid. demand fro current NERC region year CURIYR
      REAL*8 tot_census_b,tot_census_f

!****************** Initializing Variables *************************************
!****************** Body of the Program/Subprogram *****************************
      l=EUINDEX(SEC(RES),1)
! Calculate current year FORESIGHT adjustment factor
      tot_census_b = 0.0
      tot_census_f = 0.0
      DO k=1,nCENSUSreg
        IF (K1 .GT. UNYEAR) THEN
        CENSUSvalues(k)=XQELRS(k,K1)
        ELSE
          CENSUSvalues(k)=QELRS(k,K1)
        END IF
        tot_census_f = tot_census_f + CENSUSvalues(k)
      ENDDO
!     WRITE(18,1001) CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,(CENSUSvalues(k),k=1,nCENSUSREG)
!1001 FORMAT(1X,"QELRS_K1",T20,3(":",I4),10(":",F12.3))
      CALL DSMEMMV(CENSUSvalues,RNB,SEC(RES),ResLK1)
      DO k=1,nCENSUSreg
        IF (UBASEYR .GT. UNYEAR) THEN
        CENSUSvalues(k)=XQELRS(k,UBASEYR)
        ELSE
          CENSUSvalues(k)=QELRS(k,UBASEYR)
        END IF
        tot_census_b = tot_census_b + CENSUSvalues(k)
      ENDDO
!     WRITE(18,1002) CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,(CENSUSvalues(k),k=1,nCENSUSREG)
!1002 FORMAT(1X,"QELRS_K1",T20,3(":",I4),10(":",F12.3))
      CALL DSMEMMV(CENSUSvalues,RNB,SEC(RES),ResLUBASEYR)
      IF(ResLUBASEYR.NE.0) THEN
         FORESIGHTadj=ResLK1/ResLUBASEYR * DEMCERS (k1 - CURIYR + 1)
      ELSE
         FORESIGHTadj=0.0
      ENDIF
! Aggregate CENSUS regions load forecast into NERC region forecasts by end-use
!     WRITE(IMSG,*)'FORESIGHTadj',FORESIGHTadj
! HEATING
      DO k=1,nCENSUSreg
        CENSUSvalues(k)=HTRCON(UBASEYR,ELHEATX,k)
      ENDDO
!     WRITE(18,1003) CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,(CENSUSvalues(k),k=1,nCENSUSREG)
!1003 FORMAT(1X,"HTRCON",T20,3(":",I4),10(":",E15.0))
!     if(RNB.eq.7.and.CURITR.eq.1)
!     write(imsg,*)'HEATING:',(CENSUSvalues(k),k=1,9)
      LFinum=LFinum+1
      CALL DSMEMMV(CENSUSvalues,RNB,SEC(RES),load)
      tgn=1
      LoadForec(LFinum,1)=load*UNCONFAR*FORESIGHTadj
      ResTGdem(tgn)=LoadForec(LFinum,1)
! COOLING   ! depending on DG PV dispatch treatment, use different demand array
      l=l+1
      DO k=1,nCENSUSreg
          CENSUSvalues(k)=COOLCN(UBASEYR,ELCOOLX,k)
      ENDDO
!     WRITE(18,1004) CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,(CENSUSvalues(k),k=1,nCENSUSREG)
!1004 FORMAT(1X,"COOLCN",T20,3(":",I4),10(":",E15.0))
!        if(RNB.eq.7.and.CURITR.eq.1)
!     write(imsg,*)'COOLING:',(CENSUSvalues(k),k=1,9)
      LFinum=LFinum+1
      CALL DSMEMMV(CENSUSvalues,RNB,SEC(RES),load)
      tgn=2
      LoadForec(LFinum,1)=load*UNCONFAR*FORESIGHTadj
      ResTGdem(tgn)=LoadForec(LFinum,1)
! WATER HEATING
      l=l+1
      DO k=1,nCENSUSreg
        CENSUSvalues(k)=H2OCON(UBASEYR,ELWHEAX,k)
      ENDDO
!     WRITE(18,1005) CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,(CENSUSvalues(k),k=1,nCENSUSREG)
!1005 FORMAT(1X,"H2OCON",T20,3(":",I4),10(":",E15.0))
!        if(RNB.eq.7.and.CURITR.eq.1)
!     write(imsg,*)'W.HEAT.:',(CENSUSvalues(k),k=1,9)
      LFinum=LFinum+1
      CALL DSMEMMV(CENSUSvalues,RNB,SEC(RES),load)
      tgn=3
      LoadForec(LFinum,1)=load*UNCONFAR*FORESIGHTadj
      ResTGdem(tgn)=LoadForec(LFinum,1)
! STOVES
      l=l+1
      DO k=1,nCENSUSreg
        CENSUSvalues(k)=CKCON(UBASEYR,ELSTOVX,k)
      ENDDO
!     WRITE(18,1006) CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,(CENSUSvalues(k),k=1,nCENSUSREG)
!1006 FORMAT(1X,"CKCON",T20,3(":",I4),10(":",E15.0))
!        if(RNB.eq.7.and.CURITR.eq.1)
!     write(imsg,*)'STOVES :',(CENSUSvalues(k),k=1,9)
      LFinum=LFinum+1
      CALL DSMEMMV(CENSUSvalues,RNB,SEC(RES),load)
      tgn=4
      LoadForec(LFinum,1)=load*UNCONFAR*FORESIGHTadj
      ResTGdem(tgn)=LoadForec(LFinum,1)
! DRYERS
      l=l+1
      DO k=1,nCENSUSreg
        CENSUSvalues(k)=DRYCON(UBASEYR,ELDRYEX,k)
      ENDDO
!     WRITE(18,1007) CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,(CENSUSvalues(k),k=1,nCENSUSREG)
!1007 FORMAT(1X,"DRYCON",T20,3(":",I4),10(":",E15.0))
!        if(RNB.eq.7.and.CURITR.eq.1)
!     write(imsg,*)'DRYERS :',(CENSUSvalues(k),k=1,9)
      LFinum=LFinum+1
      CALL DSMEMMV(CENSUSvalues,RNB,SEC(RES),load)
      tgn=5
      LoadForec(LFinum,1)=load*UNCONFAR*FORESIGHTadj
      ResTGdem(tgn)=LoadForec(LFinum,1)
! REFRIGERATORS
      l=l+1
      DO k=1,nCENSUSreg
        CENSUSvalues(k)=REFCON(UBASEYR,k)
      ENDDO
!     WRITE(18,1008) CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,(CENSUSvalues(k),k=1,nCENSUSREG)
!1008 FORMAT(1X,"REFCON",T20,3(":",I4),10(":",E15.0))
!        if(RNB.eq.7.and.CURITR.eq.1)
!     write(imsg,*)'REFRIG.:',(CENSUSvalues(k),k=1,9)
      LFinum=LFinum+1
      CALL DSMEMMV(CENSUSvalues,RNB,SEC(RES),load)
      tgn=6
      LoadForec(LFinum,1)=load*UNCONFAR*FORESIGHTadj
      ResTGdem(tgn)=LoadForec(LFinum,1)
! FREEZERS
      l=l+1
      DO k=1,nCENSUSreg
        CENSUSvalues(k)=FRZCON(UBASEYR,k)
      ENDDO
!     WRITE(18,1009) CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,(CENSUSvalues(k),k=1,nCENSUSREG)
!1009 FORMAT(1X,"FRZCON",T20,3(":",I4),10(":",E15.0))
!        if(RNB.eq.7.and.CURITR.eq.1)
!     write(imsg,*)'FREEZER:',(CENSUSvalues(k),k=1,9)
      LFinum=LFinum+1
      CALL DSMEMMV(CENSUSvalues,RNB,SEC(RES),load)
      tgn=7
      LoadForec(LFinum,1)=load*UNCONFAR*FORESIGHTadj
      ResTGdem(tgn)=LoadForec(LFinum,1)
! LIGHTING
      l=l+1
      DO k=1,nCENSUSreg
        CENSUSvalues(k)=LTCON(UBASEYR,k)
      ENDDO
!     WRITE(18,1010) CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,(CENSUSvalues(k),k=1,nCENSUSREG)
!1010 FORMAT(1X,"LTCON",T20,3(":",I4),10(":",E15.0))
!        if(RNB.eq.7.and.CURITR.eq.1)
!     write(imsg,*)'LIGHTI.:',(CENSUSvalues(k),k=1,9)
      LFinum=LFinum+1
      CALL DSMEMMV(CENSUSvalues,RNB,SEC(RES),load)
      tgn=8
      LoadForec(LFinum,1)=load*UNCONFAR*FORESIGHTadj
      ResTGdem(tgn)=LoadForec(LFinum,1)
! APPLIANCES    ! pw2 added miscellaneous
! Changed to add broken out services in June 1997
! use different demand array depending on DG PV dispatch treatment
      l=l+1
      DO k=1,nCENSUSreg
          CENSUSvalues(k)=APCON(UBASEYR,k) + CSWCON(UBASEYR,k) +  DSWCON(UBASEYR,k) + &
             PCRCON(UBASEYR,k) + TVRCON(UBASEYR,k) + FANCON(UBASEYR,k)
      ENDDO
!     WRITE(18,1011) CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,(CENSUSvalues(k),k=1,nCENSUSREG)
!1011 FORMAT(1X,"APCON",T20,3(":",I4),10(":",E15.0))
!        if(RNB.eq.7.and.CURITR.eq.1)
!     write(imsg,*)'APPLIEN:',(CENSUSvalues(k),k=1,9)
      LFinum=LFinum+1
      CALL DSMEMMV(CENSUSvalues,RNB,SEC(RES),load)
      tgn=9
      LoadForec(LFinum,1)=load*UNCONFAR*FORESIGHTadj
      ResTGdem(tgn)=LoadForec(LFinum,1)
! SECONDARY HEATING
      l=l+1
      DO k=1,nCENSUSreg
        CENSUSvalues(k)=SHTCON(UBASEYR,ELSECHX,k)
      ENDDO
!     WRITE(18,1012) CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,(CENSUSvalues(k),k=1,nCENSUSREG)
!1012 FORMAT(1X,"SHTCON",T20,3(":",I4),10(":",E15.0))
!        if(RNB.eq.7.and.CURITR.eq.1)
!     write(imsg,*)'SEC.HEA:',(CENSUSvalues(k),k=1,9)
      LFinum=LFinum+1
      CALL DSMEMMV(CENSUSvalues,RNB,SEC(RES),load)
      tgn=10
      LoadForec(LFinum,1)=load*UNCONFAR*FORESIGHTadj
      ResTGdem(tgn)=LoadForec(LFinum,1)
! RES EV
      l=l+1
      DO k=1,nCENSUSreg
        CENSUSvalues(k)=TRQ_ELEC(1,k,UBASEYR)
      ENDDO
!     WRITE(18,1012) CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,(CENSUSvalues(k),k=1,nCENSUSREG)
!1012 FORMAT(1X,"SHTCON",T20,3(":",I4),10(":",E15.0))
!        if(RNB.eq.7.and.CURITR.eq.1)
!     write(imsg,*)'SEC.HEA:',(CENSUSvalues(k),k=1,9)
      LFinum=LFinum+1
      CALL DSMEMMV(CENSUSvalues,RNB,SEC(RES),load)
      tgn=11
      LoadForec(LFinum,1)=load*UNCONFA*FORESIGHTadj       !use different conversion factor for tran demand, different units
      ResTGdem(tgn)=LoadForec(LFinum,1)
      IF(l.NE.EUINDEX(SEC(RES),2)) GOTO 999
!****************** Termination of the Program/Subprogram **********************

      RETURN
999   WRITE(IMSG,*)'<))) Message from subroutine DSMARE'
      WRITE(IMSG,*)'<))) Wrong number of end-uses specified on ', &
      'LDSMSTRU file for RESIDENTIAL sector, region: ',NERCnam(RNB)
      WHOOPS=.FALSE.
      RETURN
      END

      SUBROUTINE DSMATR(WHOOPS)

!****************** Description of the Program/Subprogram **********************
! THIS SUBROUTINE CREATES LOAD FORECASTS FOR EACH TRANSPORTATION END USE AND EMM REGION
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr' !<< nems parameter declarations
      include 'ncntrl' !<< global variables for nems
      include 'emmparm'
      include 'control'
      include 'qblk'
      include 'mxqblk' !<< foresight data
      include 'tranrep' !<< access to transportation sector demand
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen' !<< all ldsm parameter declarations
      include 'dsmunits' !<< include file with unit number for ldsm message file
      include 'dsmsectr' !<< sector secific data and other associated variables
      include 'dsmnercr' !<< nerc region data
      include 'dsmhelm' !<< helm algorithm variables
!********************** Declaring local variables *****************************
      REAL*4 load ! total load
      INTEGER*2 k,l ! universal counters
      LOGICAL WHOOPS ! error flag
      REAL*4 CENSUSvalues(MAXCRG) ! temporary vector with CENSUS division values
      REAL*4 FORESIGHTadj ! FORESIGHT adjustment factor
      REAL*4 TraLK1 ! current region transportation demand for year K1
      REAL*4 TraLUBASEYR !current region transportation demand for year CURIYR
      REAL*8 tot_census_b,tot_census_f
!****************** Initializing Variables *************************************
!****************** Body of the Program/Subprogram *****************************
      l=EUINDEX(SEC(TRA),1)
! Calculate current year FORESIGHT adjustment factor
      tot_census_b = 0.0
      tot_census_f = 0.0
      DO k=1,nCENSUSreg
        IF (K1 .GT. UNYEAR) THEN
        CENSUSvalues(k)=XQELTR(k,K1)
        ELSE
          CENSUSvalues(k)=QELTR(k,K1)
        END IF
        tot_census_f = tot_census_f + CENSUSvalues(k)
      ENDDO
!     WRITE(18,1001) CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,(CENSUSvalues(k),k=1,nCENSUSREG)
!1001 FORMAT(1X,"QELTR_K1",T20,3(":",I4),10(":",F12.3))
      CALL DSMEMMV(CENSUSvalues,RNB,SEC(TRA),TraLK1)
      DO k=1,nCENSUSreg
        IF (UBASEYR .GT. UNYEAR) THEN
        CENSUSvalues(k)=XQELTR(k,UBASEYR)
        ELSE
          CENSUSvalues(k)=QELTR(k,UBASEYR)
        END IF
        tot_census_b = tot_census_b + CENSUSvalues(k)
      ENDDO
!     WRITE(18,1002) CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,(CENSUSvalues(k),k=1,nCENSUSREG)
!1002 FORMAT(1X,"QELTR_BS",T20,3(":",I4),10(":",F12.3))
      CALL DSMEMMV(CENSUSvalues,RNB,SEC(TRA),TraLUBASEYR)
      IF(TraLUBASEYR.EQ.0) THEN
         FORESIGHTadj=0.0
      ELSE
         FORESIGHTadj=TraLK1/TraLUBASEYR * DEMCETR (k1 - CURIYR + 1)
      ENDIF
!! Aggregate CENSUS regions load forecast into NERC region forecasts by end-use
!! LIGHT DUTY ELECTRIC VEHICLES ! AEO2022 - new variable by fuel and vehicle type, add all vehicles
!!      - {1:LDV-home,2:LDV-public,3:LDV-fast,4:Bus school,5:Bus transit,6:Bus intercity,7:CLT,8:Freight depot,9:Freight nondepot,10:pass rail}
!      DO k=1,nCENSUSreg
!!        CENSUSvalues(k)=TRQLDV(ELLDVHX,k,UBASEYR)
!        CENSUSvalues(k)=TRQ_ELEC(1,k,UBASEYR) + TRQ_ELEC(2,k,UBASEYR) + TRQ_ELEC(3,k,UBASEYR) + TRQ_ELEC(4,k,UBASEYR) + &
!              TRQ_ELEC(5,k,UBASEYR) + TRQ_ELEC(6,k,UBASEYR) + TRQ_ELEC(7,k,UBASEYR) + TRQ_ELEC(8,k,UBASEYR) + TRQ_ELEC(9,k,UBASEYR)
!      ENDDO
!     WRITE(18,1003) CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,(CENSUSvalues(k),k=1,nCENSUSREG)
!1003 FORMAT(1X,"TRQLDV",T20,3(":",I4),10(":",F12.3))
!      LFinum=LFinum+1
!      CALL DSMEMMV(CENSUSvalues,RNB,SEC(TRA),load)
!      LoadForec(LFinum,1)=load*UNCONFA*FORESIGHTadj
! ELECTRIC TRAINS! AEO2022 - new variable by fuel and vehicle type, same value as old TRQRAILR
      l=l+1
      DO k=1,nCENSUSreg
        CENSUSvalues(k)=TRQ_ELEC(10,k,UBASEYR)
      ENDDO
!     WRITE(18,1004) CURIYR+UHBSYR,K1+UHBSYR,UBASEYR+UHBSYR,(CENSUSvalues(k),k=1,nCENSUSREG)
!1004 FORMAT(1X,"TRQRAILR",T20,3(":",I4),10(":",F12.3))
      LFinum=LFinum+1
      CALL DSMEMMV(CENSUSvalues,RNB,SEC(TRA),load)
      LoadForec(LFinum,1)=load*UNCONFA*FORESIGHTadj
      
      l=l-1
      IF(l.NE.EUINDEX(SEC(TRA),2)) GOTO 999
!****************** Termination of the Program/Subprogram **********************
      RETURN
999   WRITE(IMSG,*)'<))) Message from subroutine DSMATR'
      WRITE(IMSG,*)'<))) Wrong number of end-uses specified on ', &
      'LDSMSTRU file for TRANSPORT. sector, region: ',NERCnam(RNB)
      WHOOPS=.TRUE.
      RETURN
      END

      SUBROUTINE DSMCADJ

!****************** Description of the Program/Subprogram **********************
! THIS SUBROUTINE CALCULATES ADJUSTMENT FACTORS FOR COMMERCIAL SECTOR LOAD TO MATCH TOTALS WITH VALUES BY END USE
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr' !<< nems parameter declarations
      include 'ncntrl' !<< global variables for nems
      include 'emmparm'
      include 'control'
      include 'qblk'
      include 'cogen'
      include 'tranrep'
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen' !<< all ldsm parameter declarations
      include 'dsmunits' !<< include file with unit number for ldsm message file
      include 'comparm' !<< defines parameters: cmnumbldg,cmnumserv
      include 'dsmsectr' !<< sector secific data and other associated variables
      include 'dsmnercr' !<< nerc region data
!********************** Declaring local variables *****************************
      REAL*4 load ! toatal load at the end-use level
      REAL*4 loadoth ! toatal load of the other type of enduses
      INTEGER*2 l,i,j,k ! universal counters
      INTEGER*2 CMnumServ1 ! CMnumServ-1
      INTEGER KYR,IFL
      REAL*8 CENSUSvalues(MAXCRG) ! temporary vector with CENSUS division values
      REAL*8 QELCMwDG       ! temporary variable to calculate total comm sales with own use DG
      REAL*8 DGGEN          ! temporary variable to calculate total own use DG
      LOGICAL WHOOPS ! error flag
!****************** Initializing Variables *************************************
!****************** Body of the Program/Subprogram *****************************
      CMnumServ1=CMnumServ-1
      DO KYR = CURIYR , UNYEAR
         DO k=1,nCENSUSreg
             DGGEN = 0.0
             DO IFL = 1, MNUMCGF
               DGGEN = DGGEN + CGCOMMGEN(k,KYR,IFL,2) * 3412/1000000
             ENDDO
             QELCMwDG = QELCM(k,KYR) + DGGEN   ! create total variable with all DG 
           load=0.0
           DO j=CMnumServ,1,-1
             IF(j.EQ.CMnumServ1) loadoth=load
             DO i=1,CMnumBldg
                 load=load+EndUseConsump(ELINDEX,j,i,k,KYR)
             ENDDO
           ENDDO
           DO j=2,9 !commercial ev load
              load=load+ TRQ_ELEC(j,k,KYR)
           ENDDO
           IF(loadoth.GT.0.0) THEN
             ComDemAdjFac(k,1,KYR)=(QELCMwDG-load)/loadoth
             ComDemAdjFac(k,2,KYR)=0.0
             IF(ComDemAdjFac(k,1,KYR).lt.-1.0) THEN
                ComDemAdjFac(k,1,KYR)=-1.0
                ComDemAdjFac(k,2,KYR)=QELCMwDG/(load-loadoth)-1.0
             ENDIF
           ELSE IF(loadoth.EQ.0.0 .AND. load .EQ.0.0) THEN
                   ComDemAdjFac(k,1,KYR)=-1.0
                   ComDemAdjFac(k,2,KYR)=0.0
           ELSE IF(loadoth.EQ.0.0 .AND. load .NE. 0.0) THEN
                ComDemAdjFac(k,1,KYR)=-1.0
                ComDemAdjFac(k,2,KYR)=QELCMwDG/(load-loadoth)-1.0
           ELSE
             ComDemAdjFac(k,1,KYR)=0.0
             ComDemAdjFac(k,2,KYR)=0.0
             WRITE(6,*)'<)) Warning from LDSM, routine DSMCADJ'
             WRITE(6,*)'<))) Negative OTHER COMMERCIAL END-USE load'
             WRITE(6,*)'<))) TOTAL COMMERCIAL load is not adjusted'
             WRITE(IMSG,*)'<)) Warning from LDSM, routine DSMCADJ'
             WRITE(IMSG,*)'<)) Negative OTHER COMMERCIAL END-USE load'
             WRITE(IMSG,*)'<)) TOTAL COMMERCIAL load is not adjusted'
           ENDIF
          IF (KYR .EQ. CURIYR .AND. FCRL .EQ. 1) THEN
           write(22,111) k,KYR,QELCM(k,KYR),QELCMwDG,load,loadoth,ComDemAdjFac(k,1,KYR),ComDemAdjFac(k,2,KYR)
          ENDIF
111      FORMAT(1x,'ComAdj',2I4,4F10.2,2F10.4)
         ENDDO
      ENDDO
112      FORMAT(A25,22F8.3)

!****************** Termination of the Program/Subprogram **********************
      RETURN
      END

      SUBROUTINE DSMCENV (NERCvalues,CENSUSdiv,sector,CENSUSval)

!****************** Description of the Program/Subprogram **********************
! THIS SUBROUTINE DEVELOPS A CENSUS DIVISION VALUE FROM EMM REGION VALUES BASED ON A MAPPING MATRIX
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr'
      include 'emmparm'
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen' !<< include file with all parameter declarations
      include 'dsmsectr'
      include 'dsmnercr'
!********************** Declaring local variables *****************************
      REAL*4 NERCvalues(MAXNRG)   ! NERC region values
      REAL*4 CENSUSval            ! CENSUS division value
      INTEGER*2 CENSUSdiv         ! current NERC region
      INTEGER*2 sector            ! current sector
      INTEGER*2 k                 ! universal counter
!****************** Initializing Variables *************************************
!****************** Body of the Program/Subprogram *****************************
      CENSUSval=0.0
! MappCtoN shows what fraction of generation of CENSUSdiv maps to region k
      DO k=1,nNERCreg
        CENSUSval=CENSUSval+NERCvalues(k)*MappCtoN(k,CENSUSdiv,sector)
      ENDDO
!****************** Termination of the Program/Subprogram **********************
      RETURN
      END
      LOGICAL FUNCTION DSMCMP(STR1,STR2)
!****************** Description of the Program/Subprogram **********************
! THIS FUNCTION COMPARES TWO STRINGS LOOKING FOR COMMON SUBSTRINGS
!   If any of the two strings is included in in the other, the value is .TRUE., otherwise .FALSE.
!*******************************************************************************
      IMPLICIT NONE
!********************** Declaring local variables *****************************
      CHARACTER*12 STR1 ! string 1
      CHARACTER*12 STR2 ! string 2
      INTEGER*2 l1 ! length of string1
      INTEGER*2 l2 ! length of string2
      INTEGER*2 b1 ! first blank position in string1
      INTEGER*2 b2 ! first blank position in string2
      INTEGER*2 I  ! temporary variable
!**********Declaring LDSM variables and common blocks via NEMS include files********
!****************** Initializing Variables *************************************
      DSMCMP=.TRUE.
!****************** Body of the Program/Subprogram *****************************
      DSMCMP=.FALSE.
      l1=LEN(STR1)
      l2=LEN(STR2)
      b1=INDEX(STR1,' ')
      b2=INDEX(STR2,' ')
      IF(b1.GT.0) THEN
        IF(b2.GT.0) THEN
          IF(b1.LT.b2) THEN
            DO I=b1,l1
              STR1(I:I)=' '
            ENDDO
            DO I=b1,l2
              STR2(I:I)=' '
            ENDDO
          ELSE
            DO I=b2,l1
              STR1(I:I)=' '
            ENDDO
            DO I=b2,l2
              STR2(I:I)=' '
            ENDDO
          ENDIF
        ELSE
          DO I=b1,l1
            STR1(I:I)=' '
          ENDDO
          DO I=b1,l2
            STR2(I:I)=' '
          ENDDO
        ENDIF
      ELSE IF(b2.GT.0) THEN
        DO I=b2,l1
          STR1(I:I)=' '
        ENDDO
        DO I=b2,l2
          STR2(I:I)=' '
        ENDDO
      ENDIF
      IF(INDEX(STR1,STR2).EQ.1) THEN
        DSMCMP=.TRUE.
      ELSE IF(INDEX(STR2,STR1).EQ.1) THEN
        DSMCMP=.TRUE.
      ENDIF

!****************** Termination of the Program/Subprogram **********************
      RETURN
      END

      SUBROUTINE DSMDLT

!****************** Description of the Program/Subprogram **********************
! THIS SUBROUTINE CALCULATES LOAD FORECAST USING INCREMENTAL LOAD APPROACH
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen' !<< all ldsm parameter declarations
      include 'dsmsectr' !<< sector secific data and other associated variables
      include 'dsmhelm' !<< helm algorithm variables
      include 'dsmunits'
!********************** Declaring local variables *****************************
      INTEGER*2 I ! temporary variable
      REAL*4 slr ! system load ratio
!****************** Initializing Variables *************************************
!****************** Body of the Program/Subprogram *****************************
!********** Original delta approach **********************
      SystemLoad=0.0
      DO I=1,LFinum
        SystemLoad=SystemLoad+LoadForec(I,1)
      ENDDO
      slr=SystemLoad/BaseYrSysLd(RNB)
      DO I=1,LFinum
        LoadForec(I,2)=LoadForec(I,1)-slr*BaseYrLd(I,RNB)
      ENDDO
!*********************************************************
!* Approach where all load above the base year usage is modelled with end use LSR
!     WRITE(IMSG,*)'IN DSMDLT,K1=',K1
!     DO I=1,LFinum
!       LoadForec(I,2)=LoadForec(I,1)-BaseYrLd(I,RNB)
!     ENDDO
!     SystemLoad=BaseYrSysLd(RNB)
!****************** Termination of the Program/Subprogram **********************
      RETURN
      END

      SUBROUTINE DSMEFD

!****************** Description of the Program/Subprogram **********************
!*** THIS SUBROUTINE DEVELOPS LOAD DURATION CURVES FOR EFD
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr' !<< nems parameter declarations
      include 'ncntrl'
      include 'emmparm'
      include 'control' ! contains usw_pol
      include 'dispin'
      include 'eusprc'
      include 'efpint'
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen' !<< all ldsm parameter declarations
      include 'dsmsectr' !<< now contains various cumulant stuff
      include 'dsmunits' !<< include file with unit number for ldsm message file
      include 'dsmcaldr' !<< calendar data
      include 'dsmhelm'  !<< helm algorithm variables
      include 'dsmtoefd' !<< communication with efd
      include 'dsmnemsc' !<< results of ldsm to be passed to the rest of nems
      include 'dsmnercr' !<< nerc region data
      include 'dsmrept'  !<< dsm reports specification
!********************** Declaring local variables *****************************
      REAL*4 SYLOAD1(MAXHOUR) !copy of SYLOAD for one segment of hour # in year
      REAL*4 SYLOAD2(MAXHOUR) !same as SYLOAD1 but different order
      REAL*4 area ! area of the block
      INTEGER*2 i,j,k,l,h,h1,h2,w,w1,b1,b2,b,ii ! temporary variables
      INTEGER imom,iprob,ise,jj   ! various temps for demand cumulant
      REAL*8 dmult,dval,dmmnt,dmomh(nprob,nmom),dmom(MAXEFTB,nmom)
      REAL*8 dmompk(nprob,nmom)   ! more temps for cumulants
      INTEGER*4 SWITCH ! switch: SYSTEM LOAD or DSM program (is being processed)
      REAL*4 dh ! division point of an hour
      REAL*8 pl ! current peak load
      REAL*4 ENOVER ! OVERESTIMATION OF ENERGY IN THE PEAK BLOCKS
      INTEGER*2 sgn ! segment number
      INTEGER*2 blk ! number of a block in the entire LDC
      INTEGER*2 bls ! number of a block in the segment
      INTEGER*2 hn  ! current hour number
      INTEGER*2 PBL(MAXEFTB) ! PEAK TYPE BLOCKS
      INTEGER   nsea   ! season for block
      REAL*4 ovratio ! ratio for overestimation adjustment
      REAL*4 totarea ! total area of non-peak blocks
      REAL*4 BlockHeightEFD(MAXEFTB) ! Heights of blocks in EFD LDC
      INTEGER*2 BlockNumEFD(MAXEFTB) ! Numbers of blocks
      INTEGER*2 HOURSEGNUMEFD(nhour) ! Numbers of blocks
      INTEGER*2 icl,hh
      REAL*4 testel
      INTEGER*4 hmonthd,hdayd,hhourd
      INTEGER*4 TMPHRSLd(12,3,24)
!****************** Initializing Variables *************************************
      ENOVER=0.0
      DO I=1,MAXEFTB
        PBL(I)=0
        do imom=1,nmom
          dmom(i,imom)=0.0
        enddo
      ENDDO

!****************** Body of the Program/Subprogram *****************************
! For every segment of load of EFD LDC develop load duration curves
! Create a vector of hourly loads for all calendar hours
      DO hn=1,nhour
        SYLOAD1(hn)=SYLOAD(hn)
        HOURSEGNUMEFD(hn) = HourNumberEFD(hn)
      ENDDO
! Rewrite SYLOAD1 to SYLOAD2 in segment order
      DO i=1,nhour
        SYLOAD2(i)=SYLOAD1(HourNumberEFD(i))
      ENDDO
! Sort loads in each segment in descending order
      DO i=1,EFDnumSg
        CALL DSMQSR(SYLOAD2,HourNumberEFD,EFDsgFh(i),EFDsgLh(i),MAXHOUR)
      ENDDO
! SYLOAD2 now contains calendar hour loads sorted by segment(from last to first)
! then in each segment loads are sorted in descending order
! HourNumberEFD contains original positions of calendar hours
! Determin heights of the EFD LDC blocks
      blk=1 ! current block number within whole LDC
      bls=1 ! current block number within a segment
      area=0.0 ! current area under the curve from the end of the previous block
! Now go over all hours in the year and calculate blocks widths and heights
      DO sgn=EFDnumSg,1,-1
        l=0 ! x coordinate expressed in real hours
!  next two lines are wrong   ! segment not block
        h1=EFDsgFh(sgn) ! beginning of current block in calendar hours
        h2=EFDsgLh(sgn) ! end of current block in calendar hours
        pl=SYLOAD2(h1) !current local peak load

          do imom=1,nmom
          do iprob=1,nprob
              dmompk(iprob,imom) = 0.0
            enddo
        enddo

! Peak moment
         do iprob = 1,nprob
            dval  = (1. + dincr(iprob) * loadu) * (PL )
            dmult = dval
            dmmnt =  dprob(iprob)

!          write(6,*)'dval',dval,'dprob',dprob(iprob)
! Calculate all moments for this calendar hour
            do imom=1,4   ! all moments about 0
               dmompk(iprob,imom) = dmompk(iprob,imom)+(dmult )* dmmnt
               dmult = dmult * dval
            end do   ! imom
          enddo    ! iprob

!    calculate cumulants here
!    First figure out what season we are in
!    If the routine bombs here this means the calendar file (# seasons)
!    is inconsistent
           DO J=1,efdns
          if (blk .ge. EFDSEDEF(J,1)) then
            nsea =J
            goto 1201   ! break loop
          ENDIF
           END DO
 1201    continue
!       write(IMSG,*)'nsea, blk ',J,blk
!       write(IMSG,*)(EFDSEDEF(J,1),J=1,EFDnS)

        DO h=h1,h2 ! x coordinate expressed in calendar hours
          w1=HourlyWeights(HourNumberEFD(h))
!store original mapping to efd slice
            hmonthd = MON864(HourNumberEFD(h))
            hdayd = DAYT864(HourNumberEFD(h))
            hhourd = HR864(HourNumberEFD(h))
            TMPHRSLd(hmonthd,hdayd,hhourd) = blk

!         if(blk .lt. 10) then
!           write(IMSG,*)'SYLOAD h,w1,h1,h2',SYLOAD2(h),
!    +     h,w1,h1,h2 !,EEITAJ(nsea)
!         endif
!  Create cumulants for demand function by hour for each season
!  need value for each calendar hour, and sum appropriately
! equivalent to load part divided by total hrs at that probability level


           do imom=1,nmom
           do iprob=1,nprob
              dmomh(iprob,imom) = 0.0
             enddo
         enddo


         do iprob = 1,nprob
           dval  = (1. + dincr(iprob) * loadu) * (SYLOAD2(h) )
           dmult = dval
           dmmnt =  dprob(iprob)
!          write(6,*)'dval',dval,'dprob',dprob(iprob)

! Calculate all moments for this calendar hour
           do imom=1,4   ! all moments about 0
               dmomh(iprob,imom) = dmomh(iprob,imom)+(dmult )* dmmnt
               dmult = dmult * dval
           end do   ! imom
         enddo    ! iprob

          DO w=1,w1
            l=l+1 ! x coordinate in real hours
            area=area+SYLOAD2(h)

!  add moment about zero contribution
              do imom=1,nmom
                do iprob=1,nprob
                  IF (EFDsgDbltyp(sgn,bls).EQ.'p'.OR. &
                    EFDsgDbltyp(sgn,bls).EQ.'P') THEN !if a 'peak' type block
                    dmom(blk,imom)=dmom(blk,imom) + dmompk(iprob,imom)
                  ELSE
                    dmom(blk,imom)=dmom(blk,imom) + dmomh(iprob,imom)
                  ENDIF
                enddo
              endDo

            IF((bls.NE.EFDsgDnB(sgn)) .and.  & !if this is not last block in seg
              (l.GT.EFDblockx(sgn,bls))) THEN !if x coordinate beyond the block
!  Add fraction of hour going dripping over to next block
               do imom=1,nmom
                do iprob=1,nprob
                   dmom(blk+1,imom)=dmom(blk+1,imom) + dmomh(iprob,imom) * (l - EFDblockx(sgn,bls))

! subtract off the excess quantity from the current block
                  IF (EFDsgDbltyp(sgn,bls).EQ.'p'.OR. &
                    EFDsgDbltyp(sgn,bls).EQ.'P') THEN !if a 'peak' type block
                      dmom(blk,imom)=dmom(blk,imom) - dmompk(iprob,imom) * (l - EFDblockx(sgn,bls))
                  ELSE
                       dmom(blk,imom)=dmom(blk,imom) - dmomh(iprob,imom) * (l - EFDblockx(sgn,bls))
                  ENDIF
                enddo
              enddo

!           ELSE
!             do imom=1,nmom
!               do iprob=1,nprob
!                 dmom(blk,imom)=dmom(blk,imom) + dmomh(iprob,imom)
!               enddo
!              endDo
            ENDIF

            IF(bls.NE.EFDsgDnB(sgn)) THEN !if this is not last block in segment
              IF(l.GT.EFDblockx(sgn,bls)) THEN !if x coordinate beyond the block
                dh=l-EFDblockx(sgn,bls) ! calculate an x axcess
                IF (EFDsgDbltyp(sgn,bls).EQ.'p'.OR. &
                  EFDsgDbltyp(sgn,bls).EQ.'P') THEN !if a 'peak' type block
                  BlockHeightEFD(blk)=pl
                  ENOVER=ENOVER+pl*EFDblWidth(blk)-(area-dh*SYLOAD2(h))
                  PBL(blk)=1
                ELSE
                  BlockHeightEFD(blk)=(area-dh*SYLOAD2(h))/EFDblWidth(blk)
                ENDIF
                area=dh*SYLOAD2(h)
                bls=bls+1
                blk=blk+1
                pl=SYLOAD2(h)
              ENDIF
            ENDIF
          ENDDO
        ENDDO
        BlockHeightEFD(blk)=area/EFDblWidth(blk) ! for last block in a segment
        blk=blk+1
        bls=1
        area=0.0
      ENDDO
      EFDnumBl=blk-1
! Adjast the energy in the non-peak blocks for the overestimated energy in the
! peak blocks
      IF(ENOVER.NE.0.0) THEN
        totarea=0.0
        DO blk=1,EFDnumBl
          if(PBL(blk).EQ.0) totarea=totarea+BlockHeightEFD(blk)*EFDblWidth(blk)
        ENDDO
        IF(totarea.NE.0.0) THEN
          ovratio=1-ENOVER/totarea
          DO blk=1,EFDnumBl
            IF(PBL(blk).EQ.0) BlockHeightEFD(blk)=BlockHeightEFD(blk)*ovratio
          ENDDO
        ELSE
          WRITE(IMSG,*)'<)) All blocks in EFD LDCs are of peak type'
          WRITE(IMSG,*)'<)) Area under the LDCs overestimated'
        ENDIF
      ENDIF
      DO i=1,EFDnumBl
          BlockNumEFD(i)=i
      ENDDO

!  Divide  moments by width
        DO j=1,EFDnumBl
        do imom=1,nmom
             dmom(j,imom) = dmom(j,imom)/EFDblWidth(j)
        enddo
      enddo


!       DO J=1 ,1
!        write(IMSG,*)'BLOCK wdth cum',J,EFDblWidth(j)
!    +       ,(dcum(J,imom,RNB),imom=1,nmom)
!       ENDDO
!        stop
! Sort LDC blocks in their height order
      DO j=1,EFDnS
        b1=EFDSEDEF(j,1)
        b2=EFDSEDEF(j,2)
        CALL DSMQSR(BlockHeightEFD,BlockNumEFD,b1,b2,EFDnumBl)
        !Write the LDC into the communication common block arrays:
        DO i=b1,b2
          b=i-b1+1
          ULHGHT(b,j,RNB)=BlockHeightEFD(i)
          ULWDTH(b,j,RNB)=EFDblWidth(BlockNumEFD(i))
          ULGRP(b,j,RNB)=EFDblSeg(BlockNumEFD(i))
          ULSEG(b,j,RNB)=EFDblSlc(BlockNumEFD(i))

            IF (RNB .EQ. 1) THEN
               write(IMSG,9123) CURIRUN, CURIYR+1989, CURITR, RNB, j, b1, b2, i, b, ULGRP(b,j,RNB), ULSEG(b,j,RNB), ULHGHT(b,j,RNB), ULWDTH(b,j,RNB)
 9123          format(1X,"ULHGHT_ULDSM_0",11(":",I6),2(":",F21.6))
            END IF

         DO hmonthd= 1, 12
          DO HDAYd = 1,3
            DO hhourd = 1,24
               IF (TMPHRSLd(hmonthd,hdayd,hhourd) .EQ. BlockNumEFD(i)) THEN
                 HRTOEFDSL(RNB,hmonthd,hdayd,hhourd) = EFDblSlc(BlockNumEFD(i))

                 HRTOEFDGP(hmonthd,hdayd,hhourd) = EFDblSeg(BlockNumEFD(i))

                     IF (FCRL .EQ. 1) THEN
                        WRITE(IMSG,9127) CURIRUN, CURIYR+1989, RNB, b, j, hmonthd, hdayd, hhourd, HRTOEFDGP(hmonthd,hdayd,hhourd), HRTOEFDSL(RNB,hmonthd,hdayd,hhourd)
 9127                   FORMAT(1X,"Map_864_to_SP_GP_SG",10(":",I4))
                     END IF

               ENDIF
            ENDDO
          ENDDO
         ENDDO
         IF (FCRL .EQ. 1) THEN
            WRITE(IMSG,9128) CURIRUN, CURIYR+1989, RNB, b, j, ULHGHT(b,j,RNB), ULWDTH(b,j,RNB)
 9128                   FORMAT(1X,"Map_864_to_SP_GP_SG",5(":",I4),2(":",F18.6))
         ENDIF
        ENDDO
        ULPEAK(j,RNB)=SYLOAD2(EFDsgFh(ULGRP(1,j,RNB)))
      ENDDO

! Copy into global block number by region
      DO I=1,EFDNumBl
            GblockNumEfd(i,RNB)= BlockNumEFD(i)
       END DO

!        do ise=1,1
!         write(IMSG,*)'block dmom',ise,(dmom(ise,j),j=1,2)
!         write(IMSG,*)'blockwd',efdblwidth(ise)
!        end do

!       write(IMSG,*) 'entering dsmsecwt'
! only on first iteration do we re-do the end-use ldc ?? NO, need to run every iteration to get right demands for EFP
!         IF (CURITR .EQ. 1) THEN
                CALL DSMSECWT(HoursegnumEFD)
!         ENDIF
!     IF (FCRL .eq. 1 ) THEN
!       write(IMSG,118)(ULHGHT(b,6,RNB),
!    +  b=1,18)
!118    format('in dsmefd load before',6F10.2)
!     ENDIF

! In addition to here, We also call dmshft in the dsmecp1 routine on the first iteration
! but not the first year
! go ahead and call the first year - send a switch to mark where called from
 !     IF ((USW_POL .eq. 4) .and. (FRMARG(CURIYR,RNB) .gt. 0.0)) THEN
       IF (curiyr .gt. 11) THEN
! call only if elasticity for at least one sector is greater than 0
       testel=0.0
         do icl=1,MAXSEC
          do hh=1,neusgrp(icl)
            testel=testel+EUELAS(RNB,icl,hh)
           enddo
          enddo
         if (testel .gt. 0.0) CALL DSMSHFT(1)
       ENDIF

      DO jj=1,EFDnumBl
          dcumhr(jj,RNB) = BLOCKNUMEFD(jj)
      END DO ! jj

      CALL DSMEFPS(BlockNumEFD) ! DEVELOP SECTORIAL LDC THAT THE EFP needs
      IF (FCRL .EQ. 1) THEN
        IF(NODSMRPT(4)) CALL DSMREP4 ! WRITE EFD LDC
      ENDIF
!****************** Termination of the Program/Subprogram **********************
      RETURN
900   WRITE(*,*)'<))) ERROR IN DSMEFD ROUTINE OF LDSM'
      WRITE(*,*)'<)))) DSM PROGRAM DATA MAY BE CORRUPTED'
      WRITE(IMSG,*)'<))) ERROR IN DSMEFD ROUTINE OF LDSM'
      END

      SUBROUTINE DSMEFP

!****************** Description of the Program/Subprogram **********************
! THIS SUBROUTINE FINDS SECTORAL PEAK FOR EFP
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr'
      include 'emmparm'
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen' !<< all ldsm parameter declarations
      include 'dsmunits' !<< include file with unit number for ldsm message file
      include 'dsmhelm' !<< helm algorithm variables
      include 'dsmtfefp' !<< communitcation with efp
      include 'dsmcaldr' !<< calendar data
      include 'dsmsectr'
!********************** Declaring local variables *****************************
      INTEGER*2 I,J,K,L,M,N ! multi-purpose pointers
      REAL*4 peak     ! current maximum value of load
      REAL*4 average  ! temporary variable for averaging
      INTEGER*2 phour ! current hour of peak
      INTEGER*2 pday  ! current peak-day
!****************** Initializing Variables *************************************
!*** Find annual COINCIDENT/NON-COINCIDENT peaks for sectors
      DO I=1,NumSec
!       WRITE(IMSG,'(12H SectorLoad=,I2/8F10.3)')I,(SectorLoad(K,I),K=1,nhour)
!  Coincident peaks first
        SecAnnualPeak(RNB,I,1)=SectorLoad(SystemPeakHour(K1),I)
! CALCULATE AVERAGE OF NpeakH COINCIDENT SECTORIAL PEAK HOURS
        average=0.0
        DO K=1,NpeakH
          average=average+SectorLoad(SysPeakHour(K),I)
        ENDDO
        average=average/NpeakH
        SecAnnPeaAvPCP(RNB,I)=average
!       WRITE(IMSG,*)'SecAnnPeaAvPCP(RNB,I)',SecAnnPeaAVPCP(RNB,I)
!       WRITE(IMSG,*)'SecAnnualPeak(RNB,I,1)',SecAnnualPeak(RNB,I,1)
        peak=0.0
        DO K=1,nhour
          IF (SectorLoad(K,I) .GT. peak) THEN
            peak=SectorLoad(K,I)
          ENDIF
        ENDDO
        SecAnnualPeak(RNB,I,2)=peak
!      WRITE(IMSG,*)'SecAnnualPeak(RNB,I,2)',SecAnnualPeak(RNB,I,2)
      ENDDO
      SystemLF(RNB)=SystemLoadFactor(K1)
!****************** Termination of the Program/Subprogram **********************
      RETURN
      END

      SUBROUTINE DSMEMMV(CENSUSvalues,NERCreg,sector,NERCval)

!****************** Description of the Program/Subprogram **********************
! THIS ROUTINE CONVERTS EMM REGION VALUES TO CENSUS DIVISION VALUES USING MAPPING MATRICES FOR EACH DEMAND SECTOR
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr'
      include 'emmparm'
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen' !<< include file with all parameter declarations
      include 'dsmsectr'
      include 'dsmnercr'
      include 'dsmunits'
!********************** Declaring local variables *****************************
      REAL*4 CENSUSvalues(MAXCRG)      ! CENSUS division values
      INTEGER*2 NERCreg           ! current NERC region
      INTEGER*2 sector            ! current sector
      INTEGER*2 k                 ! universal counter
      REAL*4 NERCval              ! NERC value
!****************** Initializing Variables *************************************
!****************** Body of the Program/Subprogram *****************************
      NERCval=0.0
      DO k=1,nCENSUSreg
        NERCval=NERCval+CENSUSvalues(k)*MappCtoN(NERCreg,k,sector)
      ENDDO
!****************** Termination of the Program/Subprogram **********************
      RETURN
      END

      SUBROUTINE DSMFOR(WHOOPS)

!****************** Description of the Program/Subprogram **********************
! THIS SUBROUTINE PREPARES AN END USE LEVEL LOAD FORECAST FOR AN EMM REGION BASED ON CENSUS DIVISION LOAD FORECASTS FROM THE DEMAND MODULES
! Two versions of the load forecast by LSR are developed:
!    1.  the traditional approach of monthly coincident/noncoincident sectoral peak loads
!    2.  the incremental load approach, which technically is commented out now
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr' !<< nems parameter declarations
      include 'ncntrl' !<< acces to nems global variables like
      include 'emmparm'
      include 'control'
      include 'dispinyr'
      include 'uefdout'
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen' !<< all ldsm parameter declarations
      include 'dsmunits' !<< include file with unit number for ldsm message file
      include 'dsmsectr' !<< sector secific data and other associated variables
      include 'dsmhelm' !<< helm algorithm variables
!********************** Declaring local variables *****************************
      INTEGER*2 I ! current number of sector
      REAL*4    load ! temporary variable to calculate total system load
      LOGICAL*1 flag ! flag
      LOGICAL WHOOPS ! error flag
!****************** Initializing Variables *************************************
!****************** Body of the Program/Subprogram *****************************
! Bring in the load forecast data from the demand modules, converting from CENSUS regions to EMM regions
      LFinum=0  ! index on the Load Forecast list
      TotDemCensus=0.0
      DO I=1,NumSec
         IF (SEC(RES) .EQ. I) THEN
            CALL DSMARE(WHOOPS)
            IF(WHOOPS) RETURN
         ELSE IF (SEC(COM) .EQ. I) THEN
            CALL DSMACM(WHOOPS)
            IF(WHOOPS) RETURN
         ELSE IF (SEC(IND) .EQ. I) THEN
            CALL DSMAIN(WHOOPS)
            IF(WHOOPS) RETURN
         ELSE IF (SEC(TRA) .EQ. I) THEN
            CALL DSMATR(WHOOPS)
            IF(WHOOPS) RETURN
         ELSE
            GOTO 999
         ENDIF
      ENDDO
! if historical year, adjust load by historical regional factors
      IF ( CURIYR .eq. UBASEYR .and. (((CURIYR + UHBSYR) .le. UYR_HIST)  &
          .and. ((CURIYR+UHBSYR) .ge. HSTYEAR))  ) then
        do I=1,Neu
          if (UNRGNS.eq.13) then
             LoadForec(I,1) = LoadForec(I,1) * Histshr
          elseif (UNRGNS.eq.22) then
!DKG         LoadForec(I,1) = LoadForec(I,1) * Histshr
          endif
        enddo

       write(IMSG,*) 'changing loadforec ',CURIYR,RNB,histshr
      ENDIF

! IF HIGH DEMAND SCENARIO APPLY MULTIPLIERS
      if (USW_CEL .gt. 0) then
      do I=1, Neu
       if (EUGRP(I) .lt. 11 .or. EUGRP(I) .gt. 20) then !commercial already adjusted through ComDemAdjFac
        LoadForec(I,1) = LoadForec(I,1) * UQELFAC(CURIYR)
       endif
      enddo
      endif

! On first year and first iteration calculate base year system load
      IF(K1.LE. MSEDYR + 3) THEN
          DO I=1,Neu
            BaseYrLd(I,RNB)=LoadForec(I,1)
          ENDDO
        load=0.0
        DO I=1,Neu
          load=load+BaseYrLd(I,RNB)
        ENDDO
        BaseYrSysLd(RNB)=load
      ENDIF
! Apply DELTA approach to the load forecast
!      WRITE(IMSG,*)'CALL DSMDLT,K1=',K1
!      CALL DSMDLT
!      WRITE(IMSG,*)'BACK FROM DSMDLT,K1=',K1
!****************** Termination of the Program/Subprogram **********************
      RETURN
999   WRITE(IMSG,*)'<))) Message from subroutine DSMFOR'
      WRITE(IMSG,*)'<))) Access to load forecast for sector ',SLNAM(I),' is undefined'
      WRITE(IMSG,*)'<))) Execution of the program terminated'
      WHOOPS=.TRUE.
      RETURN
      END

      SUBROUTINE DSMHLM
      USE EPHRTS_SWTICHES
      USE EPHRTS_FILE_UNIT_NUMBERS 

!****************** Description of the Program/Subprogram **********************
! THIS SUBROUTINE RUNS THE HOURLY ELECTRICITY LOAD ALGORITHM
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr' !<< nems parameter declarations
      include 'ncntrl' !<< acces to nems global variables like
      include 'emmparm'
      include 'control'
      include 'dispett'
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen' !<< all ldsm parameter declarations
      include 'dsmunits' !<< include file with unit number for ldsm message file
      include 'dsmsectr' !<< sector secific data and other associated variables
      include 'dsmhelm' !<< helm algorithm variables
      include 'dsmtfecp' !<< communication with ecp
      include 'dsmtoefd' !<< communication with efd
      include 'dsmnemsc' !<< results of ldsm to be passed to the rest of nems
      include 'dsmtfefp' !<< communitcation with efp
      include 'dsmnercr' !<< nerc region data
      include 'dsmcaldr' !<< calendar data
!********************** Declaring local variables *****************************
      INTEGER*2 ZERO,ONE
      PARAMETER(ZERO=0,ONE=1)

      INTEGER*2 I,J,K,L,M,N ! multi-purpose pointers
      REAL*4 td ! t&dloss expantion factor
      REAL*4 SystLo,systlo2 !system load
      integer srec   !  rec num for DAF file for system loads
      real*8 osyload(maxhour),dsyload(maxhour)
      REAL*4 SYLOAD1(MAXHOUR) !matrix with system load sorted in descending ord.
      INTEGER*2 Hindex(MAXHOUR) ! indexes of hours
      CHARACTER*6 WHENQSR
      LOGICAL CHECK
      real sysload_debug, h2_elec_consumed_debug
      LOGICAL E_DEBUG_EXIST

      IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN 
         INQUIRE(FILE="EPHRTS_DEBUG_FILE.TXT", EXIST=E_DEBUG_EXIST)
         IF (E_DEBUG_EXIST) THEN
            OPEN(unit_num_ephrts_debug_file, FILE="EPHRTS_DEBUG_FILE.TXT", STATUS="OLD", POSITION="APPEND", ACTION="WRITE")
         ELSE
            OPEN(unit_num_ephrts_debug_file, FILE="EPHRTS_DEBUG_FILE.TXT", STATUS="NEW", ACTION="WRITE")
         END IF
      END IF
!****************** Initializing Variables *************************************
      IF (K1.EQ.CURIYR) THEN
        DO I=1,MAXHOUR
          DO K=1,MAXSEC
            SectorLoad(I,K)=0.0
            TotSecLoad(RNB,K)=0.0
          ENDDO
          SYSLDHR(CURIYR,RNB,I) = 0.0
        ENDDO
      ENDIF
      DO I=1,MAXHOUR
        SYLOAD(I)=0.0
      ENDDO
!****************** Body of the Program/Subprogram *****************************
!*** Calculate base system load
      IF (SystemLoad.GT.0.0) THEN
!        CALL DSMNWS(NERClsrN(RNB),ZERO,ZERO)
        CALL DSMNWS(ZERO,ZERO,ZERO)
      ELSE
        WRITE(IMSG,*)'<))) Warning from subroutine DSMHLM'
        WRITE(IMSG,*)' System load for region: ',NERCnam(RNB),' year:',K1,' <= 0.0'
      ENDIF
!
!    copy syload here (1990 864 curve with current year energy) for projection year
!      for comparison below after changes for end use consumption
!
      if (USW_POL .GE. 2) then
        do j=1,864
         osyload(j) = syload(j)
        enddo
      endif
!

!*** Now modify base system load according to changes in system structure
      K=0 ! pointer on  the list of end-uses
      SystLo=0.0
      DO I=1,NumSec ! do for each sector
        DO J=1,NEUSES(I) ! do for each end-use in a sector
          K=K+1
!         write(IMSG,*)'NumSec',I,'NEUSE',J,EUrecNUM(K,RNB)
          ! SKIP THE CALL TO DSMNWS FOR THE HYDROGEN END-USE (J=4) IF SECTOR IS INDUSTRIAL, THIS IS HANDLED DIFFERENTLY BY USING THE VARIABLES FROM HMM DIRECTLY INSTEAD.
		  CHECK = .TRUE.          
		  IF (I .EQ. SEC(IND) .AND. J .EQ. 4) THEN
              CHECK = .FALSE.
          END IF
          
          IF (CHECK .EQ. .TRUE.) THEN
            CALL DSMNWS(EUrecNUM(K,RNB),K,I)
          END IF
        ENDDO
        SystLo=SystLo+TotSecLoad(RNB,I)
      ENDDO
      
      SYSLOAD_DEBUG = 0.0
      H2_ELEC_CONSUMED_DEBUG = 0.0
      
      if (curiyr .eq. UPSTYR) then 
         IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN
            if ( annual_h2_mwh_consumed(rnb) .gt. 0.0) then
               write (unit_num_ephrts_debug_file,*) " warning - expected zero hydrogen consumption at start year for h2 submodule, received - ", annual_h2_mwh_consumed(rnb)
               write (unit_num_ephrts_debug_file,*) " warning - expected zero hydrogen consumption at start year for h2 submodule, received - ", annual_h2_mwh_consumed(rnb)
               write (unit_num_ephrts_debug_file,*) " warning - expected zero hydrogen consumption at start year for h2 submodule, received - ", annual_h2_mwh_consumed(rnb)
               write (unit_num_ephrts_debug_file,*) " warning - expected zero hydrogen consumption at start year for h2 submodule, received - ", annual_h2_mwh_consumed(rnb)
               write (unit_num_ephrts_debug_file,*) " warning - expected zero hydrogen consumption at start year for h2 submodule, received - ", annual_h2_mwh_consumed(rnb)
             end if
         END IF
      end if

      IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN 
         write(unit_num_ephrts_load_debug_file,*) "yr",curiyr+1989,"region",rnb
      END IF
      do i=1,nhour
        IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN 
            write(unit_num_ephrts_load_debug_file,*) "hour", i, "previous syload", syload(i)
        END IF
        !sysload_debug = sysload_debug + syload(i)
        ! mw to gw
        syload(i) = syload(i) + (h2_elec_consumed(rnb,i)/1000.0) 
        
        IF(K1.EQ.CURIYR) THEN
            SectorLoad(i,SEC(IND))= SectorLoad(i,SEC(IND)) + (h2_elec_consumed(rnb,i)/1000.0)       
            TotSecLoad(RNB,SEC(IND)) = TotSecLoad(RNB,SEC(IND)) + h2_mwh_consumed(rnb,i)/1000.0
			systlo = systlo + h2_mwh_consumed(rnb,i)/1000.0
        END IF
            
        !h2_elec_consumed_debug = h2_elec_consumed_debug + h2_elec_consumed(rnb,i)/1000.0 ! h2_elec_consumed+debug is the total cumulative load added from h2 for the year
        IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN 
            write(unit_num_ephrts_load_debug_file,*) "debug_ld",curiyr+1989,",", rnb,",", ",", i, ",", syload(i), ", ", h2_elec_consumed(rnb,i) / 1000.0,",",h2_mwh_consumed(rnb,i)/1000.0,",",SectorLoad(i,SEC(IND)),",",TotSecLoad(RNB,SEC(IND)),",",systlo,",",Annual_H2_MWH_Consumed(rnb)
        END IF
        sysload_debug = sysload_debug + (syload(I) * IDAYTQ(DAYT864(i),MON864(i))) 
        h2_elec_consumed_debug = h2_elec_consumed_debug + ( h2_elec_consumed(rnb,i) / 1000.0 ) * IDAYTQ(DAYT864(i),MON864(i)) 
      enddo
      
      IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN
         if (annual_h2_mwh_consumed(rnb) .gt. 0.0 ) then
            write(unit_num_ephrts_debug_file,*) "yr", curiyr+1989,"region",rnb
            write(unit_num_ephrts_debug_file,*) "systlo before h2, h2_elec_total (GWh): ", systlo, ", ", annual_h2_mwh_consumed(rnb) / 1000
            write(unit_num_ephrts_debug_file,*) "syload before h2, tot_h2_elec (GWh)  : ", sysload_debug, ", ", h2_elec_consumed_debug
         end if
      END IF
        
      if (curiyr .eq. 3 .and. RNB .eq. 1) then
       write(IMSG,*)' First 50 system load '
      WRITE(IMSG,'(8H SYLOAD=/8F10.3)')(SYLOAD(I),I=1,50)
      endif
! Expand load by t&d loss factor and find system peak, total system load, and system load factor

      td=1.0+NERCtdloss(RNB)*ULOSSADJ(CURIYR)

1077  FORMAT(1X,"LDSM_LOAD",5(":",I4),4(":",F12.3))
          WRITE(18,1077) CURIRUN, CURIYR+1989, K1+1989, CURITR, RNB, systlo, td, NERCtdloss(RNB), ULOSSADJ(CURIYR)

      systlo2=systlo
      SystLo=SystLo*td
!
!   calculate delta syload for 864 array
!    delta 864 = dsyload = syload updated with new end use - initial syload from above (osyload)
      if (USW_POL .GE. 2) then
       do j=1,864
        dsyload(j) = syload(j) - osyload(j)
       enddo
      endif
!
      DO I=1,nhour
        SYLOAD(I)=SYLOAD(I)*td
        SYLOAD1(I)=SYLOAD(I)
        Hindex(I)=I
        if (K1 .EQ. CURIYR) then
          SYSLDHR(CURIYR,RNB,I) = SYLOAD(I)
        endif
      ENDDO
      WHENQSR='before'
      CALL DSMHLC(SYLOAD1,WHENQSR)
      CALL DSMQSR(SYLOAD1,Hindex,ONE,nhour,MAXHOUR)
      SystemPeak(K1)=SYLOAD1(1)
!     write(imsg,*)'SYLOAD1(1...10)',(SYLOAD1(I),I=1,10)
!     write(imsg,*)'K1,SystemPeak(K1)',K1,SystemPeak(K1)
      SystemPeakHour(K1)=Hindex(1)
      TotSystemLoad(K1)=SystLo
      SystemLoadFactor(K1)=SystLo/nhouryr/SystemPeak(K1)
      J=0
      DO I=1,nhour
        DO K=1,HourlyWeights(Hindex(I))
          J=J+1
          IF(J.GT.NpeakH) GOTO 300
          SysPeakHour(J)=Hindex(I)
        ENDDO
      ENDDO
300   CONTINUE
      WHENQSR='after '
      CALL DSMHLC(SYLOAD1,WHENQSR)
!****************** Termination of the Program/Subprogram **********************
      IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN
         CLOSE(unit_num_ephrts_debug_file)
      END IF
      RETURN
      END

      SUBROUTINE DSMLCP(SWITCH)

!****************** Description of the Program/Subprogram **********************
!*** THIS SUBROUTINE DEVELOPS LOAD DURATION CURVES FOR THE ECP
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr' !<< nems parameter declarations
      include 'ncntrl'
      include 'emmparm'
      include 'eusprc'
      include 'efpint'
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen' !<< all ldsm parameter declarations
      include 'dsmunits' !<< include file with unit number for ldsm message file
      include 'dsmcaldr' !<< calendar data
      include 'dsmhelm' !<< helm algorithm variables
      include 'dsmtoefd'
      include 'dsmnemsc' !<< results of ldsm to be passed to the rest of nems
      include 'dsmtfecp' !<< communication with ecp
      include 'dsmnercr' !<< nerc region data
      include 'dsmrept'  !<< ldsm reports specification
      include 'dsmsectr'
!********************** Declaring local variables *****************************
      INTEGER*2 ONE
      PARAMETER(ONE=1)

      REAL*4 SYLOAD1(MAXHOUR) !copy of SYLOAD for one segment of hour # in year
      REAL*4 SYLOAD2(MAXHOUR) !same as SYLOAD1 but different order
      REAL*4 area ! area of the block
      INTEGER*2 i,j,k,l,h,h1,h2,w,w1,s,s1,s2 ! temporary variables
      INTEGER*4 SWITCH ! switch: SYSTEM LOAD or DSM program (is being processed)
      REAL*4 dh ! division point of an hour
      REAL*4 pl ! current peak load
      REAL*4 ENOVER ! OVERESTIMATION OF ENERGY IN THE PEAK BLOCKS
      INTEGER*2 sgn ! segment number
      INTEGER*2 blk ! number of a block in the entire LDC
      INTEGER*2 bls ! number of a block in the segment
      INTEGER*2 hn  ! current hour number
      INTEGER*2 PBL(MAXECTB) ! PEAK TYPE BLOCKS
      REAL*4 ovratio ! ratio for overestimation adjustment
      REAL*4 totarea ! total area of non-peak blocks
      REAL*4 u,load ! temporary variable for load
      INTEGER*2 icl,jj ! temporary variables
      REAL*4 testel
      INTEGER*4 hmonth,hday,hhour
      INTEGER*4 TMPHRSL(12,3,24)
!****************** Initializing Variables *************************************
      ENOVER=0.0
      DO I=1,MAXECTB
        PBL(I)=0
      ENDDO
!****************** Body of the Program/Subprogram *****************************

!     For every segment of load of ECP LDC develop load duration curves
!     Create a vector of hourly loads for all calendar hours

      DO hn=1,nhour
         SYLOAD1(hn)=SYLOAD(hn)
      ENDDO

!     Put SYLOAD1 in SYLOAD2 in segment order

      IF (SWITCH.LE.2) THEN
         DO i=1,nhour
            SYLOAD2(i)=SYLOAD1(HourNumber(i))
         ENDDO

!        Sort loads in each segment in descending order

         IF(SWITCH.EQ.1) THEN
            DO i=1,ECPnumSg
               CALL DSMQSR(SYLOAD2,HourNumber,ECPsgFh(i),ECPsgLh(i),MAXHOUR)
            ENDDO
         ENDIF
      ELSE
         DO i=1,nhour
            SYLOAD2(i)=SYLOAD1(HourNumber3(i,RNB))
         ENDDO
      ENDIF

!     SYLOAD2 now contains calendar hour loads sorted by segment(from last to first)
!     Within each segment loads are sorted in descending order
!     HourNumber contains original positions of calendar hours
!     Determine heights of the ECP LDC blocks

      blk=1 ! current block number within whole LDC
      bls=1 ! current block number within a segment
      area=0.0 ! current area under the curve from the end of the previous block

!     Now go over all hours in the year and calculate block widths and heights

      DO sgn=ECPnumSg,1,-1
         l=0 ! x coordinate expressed in real hours
         h1=ECPsgFh(sgn) ! beginning of current block in calendar hours
         h2=ECPsgLh(sgn) ! end of current block in calendar hours
         pl=SYLOAD2(h1) !current local peak load
!        IF (CURIYR .EQ. 22 .AND. K1 .EQ. CURIYR) THEN
!        write(6,110) CURIYR,RNB,sgn,h1,h2,pl
!        ENDIF
110     FORMAT(' ECPSG',5I6,F10.2)
         DO h=h1,h2 ! x coordinate expressed in calendar hours
            IF(SWITCH.LE.2) THEN
               w1=HourlyWeights(HourNumber(h))
            ELSE
               w1=HourlyWeights(HourNumber3(h,RNB))
            ENDIF
           IF (K1 .EQ. CURIYR) THEN   ! store mapping to ECP slice
            hmonth = MON864(HourNumber(h))
            hday = DAYT864(HourNumber(h))
            hhour = HR864(HourNumber(h))
            TMPHRSL(hmonth,hday,hhour) = blk
           ENDIF
!          IF( CURIYR .EQ. 22 .AND. K1 .EQ. CURIYR) THEN
!          write(6,112) CURIYR,RNB,sgn,h,w1,HourNumber(h)
!          ENDIF
112      FORMAT(' HRWT ',6I6)
            DO w=1,w1
               l=l+1 ! x coordinate in real hours
               area=area+SYLOAD2(h)
               IF(bls.NE.ECPsgDnB(sgn)) THEN !if this is not last block in segment
                  IF(l.GT.ECPblockx(sgn,bls)) THEN !if x coordinate beyond the block
                     dh=l-ECPblockx(sgn,bls) ! calculate an x axcess
                     IF (ECPsgDbltyp(sgn,bls).EQ.'p'.OR. ECPsgDbltyp(sgn,bls).EQ.'P') THEN !if a 'peak' type block
                        BlockHeight(blk)=pl
                        ENOVER=ENOVER+pl*ECPblWidth(blk)-(area-dh*SYLOAD2(h))
                        PBL(blk)=1
                     ELSE
                        BlockHeight(blk)=(area-dh*SYLOAD2(h))/ECPblWidth(blk)
                     ENDIF
!                    IF (CURIYR .EQ. 22 .and. K1 .EQ. CURIYR) THEN
!                      write(6,111) CURIYR,CURITR,RNB,SWITCH,blk,h,SYLOAD2(h),SYLOAD(h),BLOCKHEIGHT(BLK)
!                    ENDIF
111     FORMAT(' DSMLCP: ',6I6,3F10.4)
                     area=dh*SYLOAD2(h)
                     bls=bls+1
                     blk=blk+1
                     pl=SYLOAD2(h)
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
         BlockHeight(blk)=area/ECPblWidth(blk) ! for last block in a segment
         IF (CURIYR .EQ. 21 .and. CURITR .eq. 1) THEN
             write(6,111) CURIYR,CURITR,RNB,SWITCH,blk,h,SYLOAD2(h),SYLOAD(h),BLOCKHEIGHT(BLK)
         ENDIF
         blk=blk+1
         bls=1
         area=0.0
      ENDDO
      ECPnumBl=blk-1

!     Adjust the energy in the non-peak blocks for the overestimated energy in the peak blocks

      IF(ENOVER.NE.0.0) THEN
         totarea=0.0
         DO blk=1,ECPnumBl
            if(PBL(blk).EQ.0) totarea=totarea+ BlockHeight(blk)*ECPblWidth(blk)
         ENDDO
         IF(totarea.NE.0.0) THEN
            ovratio=1-ENOVER/totarea
            DO blk=1,ECPnumBl
               IF(PBL(blk).EQ.0) BlockHeight(blk)=BlockHeight(blk)*ovratio
            ENDDO
         ELSE
            WRITE(IMSG,*)'<)) All blocks in current LDC are of peak type'
            WRITE(IMSG,*)'<)) Area under the LDC overestimated'
         ENDIF
      ENDIF

      IF(SWITCH.EQ.1) THEN
         DO i=1,ECPnumBl
            BlockNum(i)=i
         ENDDO

!        Sort LDC blocks in their height order

         CALL DSMQSR(BlockHeight,BlockNum,ONE,ECPnumBl,ECPnumBl)

!        Write into the communication common block arrays:
!        ECPLDCBH(year,region,blocknumber) heights,
!        ECPLDCBW(year,region,blocknumber) widths,
!        ECPLDCBS(year,region,blocknumber) segment identifiacation, for all blocks.

         DO i=1,ECPnumBl
            ECPLDCBH(K1,RNB,i)=BlockHeight(i)
            ECPLDCBW(K1,RNB,i)=ECPblWidth(BlockNum(i))/SumSegWidth !to fr. of year
            ECPLDCBS(K1,RNB,i)=ECPblSeg(BlockNum(i))
            ECPLDCSL(K1,RNB,i)=ECPblSlc(BlockNum(i))
           IF(CURIYR.LT.27) WRITE(IMSG,9402)'results ',K1,RNB,i,Blocknum(i),ECPLDCBH(k1,RNB,i),ECPLDCBW(k1,RNB,i),ECPLDCBS(k1,RNB,i),ECPLDCSL(k1,RNB,i)
9402  FORMAT(A20,1x,4(I4,1x),2(F12.4,1x),I4,1x,F12.0,1x)
         ENDDO
         IF (K1 .EQ. CURIYR) THEN
         DO hmonth = 1, 12
          DO HDAY = 1,3
            DO hhour = 1,24
               DO i = 1, ECPnumBL
                 IF (TMPHRSL(hmonth,hday,hhour) .EQ. BlockNum(i)) THEN
                     HRTOECPSL(CURIYR,RNB,hmonth,hday,hhour) = i
            IF(CURIYR.LT.27) WRITE(IMSG,9120)'hrtoecpsl ',CURIYR,RNB,hmonth,hday,hhour,i,Blocknum(i),TMPHRSL(hmonth,hday,hhour),HRTOECPSL(CURIYR,RNB,hmonth,hday,hhour)
9120  FORMAT(A20,1x,9(I4,1x))
                 ENDIF
               ENDDO
            ENDDO
          ENDDO
         ENDDO
         ENDIF
         DO i=1,ECPnS
            s1=ECPSEDEF(i,1)
            s2=ECPSEDEF(i,2)
            u=0.0
            DO s=s1,s2
               load=SYLOAD2(ECPsgFh(s))
               IF(load.GT.u) u=load
            ENDDO
            UPPEAK(i,K1-CURIYR+1,RNB)=u
         ENDDO

         IF(K1.GE.CURIYR+2.AND.K1.LE.ECPlastYearIndex-1) THEN
            IF(K1.EQ.CURIYR+2) THEN
               DO i=1,MAXHOUR
                  HourNumber3(i,RNB)=HourNumber(i)
               ENDDO

!   BlockNum3(x,x,1,x) stores the order the blocks are sorted in the 3rd year of the ECP time horizon

               DO i=1,ECPnumBl
                  BlockNum3(i,RNB,K1-CURIYR-1,2)=BlockNum(i)
               ENDDO
            ELSE

!              Rest of BlockNum3 matrix contains a map that says which ordinal number
!              avoided cost should be used for the i-th block in the first year curve

               DO i=1,ECPnumBl
                  DO k=1,ECPnumBl
                     IF(BlockNum(i).EQ.BlockNum3(k,RNB,1,2)) GOTO 777
                  ENDDO

!                 WRITE(IMSG,*)'RNB,CURIYR,K1,i,k,BlockNum(i)','BlockNum3(k,RNB,1,2)'
!                 write(imsg,*)RNB,CURIYR,K1,i,k,BlockNum(i), BlockNum3(k,RNB,1,2)

                  GOTO 900
  777             BlockNum3(k,RNB,K1-CURIYR-1,2)=i
               ENDDO
            ENDIF
         ENDIF
      ENDIF

!     add ecp load shifting on first iteration

      IF (CURITR .eq. 1 .and. CURIYR .gt. 11 .and. SWITCH .eq. 1) THEN

!        call only if elasticity for at least one sector is greater than 0

         testel=0.0
         do icl=1,MAXSEC
            do jj=1,neusgrp(icl)
               testel=testel+EUELAS(RNB,icl,jj)
            enddo
         enddo
         if (testel .gt. 0.0) CALL DSMSHFT(0)
      ENDIF ! CURIYR

!****************** Termination of the Program/Subprogram **********************
      RETURN
900   WRITE(*,*)'<))) ERROR IN DSMLCP ROUTINE OF LDSM'
      WRITE(*,*)'<)))) DSM PROGRAM DATA MAY BE CORRUPTED'
      WRITE(IMSG,*)'<))) ERROR IN DSMLCP ROUTINE OF LDSM'
      END

      SUBROUTINE CALLSYSLOAD
!****************** Description of the Program/Subprogram **********************
! THIS SUBROUTINE CALL SYSLOAD from SQLite table
!*******************************************************************************
      
      USE SQLITE
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr' !<< nems parameter declarations
      include 'ncntrl' !<< acces to nems global variables like
      include 'emmparm'
      include 'dsmdimen' !<< calendar data
      
      type(sqlite_database)                      :: db
      type(sqlite_statement)                     :: stmt
      type(sqlite_column), dimension(:), pointer :: col
      character(len=40), pointer, dimension(:)   :: result
      character(len=80)                          :: errmsg
      logical                                    :: finished

      integer                                    :: p
      integer                                    :: q
      integer                                    :: Month_sq,Day_sq,Hour_sq
      integer                                    :: id 
      Real                                       :: DistLo_sq

      INTEGER*4 REGNUM

      COMMON /REGSYSLD/LSRname_s,DistLo_s
      REAL*4 DistLo_s(MNUMNR,MAXHOUR)
      CHARACTER*8 LSRname_s(MNUMNR)
      
      
      
      call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )          
    
             allocate ( col(7) )
               ! QUERY THE DATABASE TABLE
                 call sqlite3_column_query( col(1), 'ID', sqlite_int ) 
                 call sqlite3_column_query( col(2), 'EMM_REG', sqlite_int )
                 call sqlite3_column_query( col(3), 'RGN_NAM', sqlite_char )
                 call sqlite3_column_query( col(4), 'Month', sqlite_int )
                 call sqlite3_column_query( col(5), 'DAY', sqlite_int )
                 call sqlite3_column_query( col(6), 'HOUR', sqlite_int )
                 call sqlite3_column_query( col(7), 'PCT', sqlite_real )
                 
                 call sqlite3_prepare_select( db, 'V_SYS_LOAD_by_EMM_REGION', col, stmt )
    
               ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
                 q = 0
                 do
!                 write(6,*)'loading db with hourly data'
                   call sqlite3_next_row( stmt, col, finished )
                   if ( finished ) exit
    
                   call sqlite3_get_column( col(1), ID )
                   call sqlite3_get_column( col(2), REGNUM )
                   call sqlite3_get_column( col(3), LSRname_s(REGNUM) )
                   call sqlite3_get_column( col(4), Month_sq )
                   call sqlite3_get_column( col(5), Day_sq )
                   call sqlite3_get_column( col(6), Hour_sq )
                   call sqlite3_get_column( col(7), DistLo_sq)
                   
                   q = ((Month_sq-1)*3+(Day_sq-1))*24 + Hour_sq
                   
                   DistLo_s(REGNUM,q) = DistLo_sq
!                   write(6,*) 'sysyload', REGNUM, LSRname_s(REGNUM),q, DistLo_s(REGNUM,q)
                   
                    if (Month_sq .EQ. 12 .and. Day_sq .EQ. 3 .and. Hour_sq .EQ. 24) q = 0
                 end do
                 !write(6,*)'done writing hourly data to db'
                    
                 do p =1,25
 !                  write(6,135) LSRname_s(p) , (DistLo_s(p,Hour_sq), Hour_sq = 1,864)
 !                   135       FORMAT(1X,"DistLo-sqlite",A8,864(":",E15.8))
                 enddo         

                deallocate ( col ) 
                
       call sqlite3_close( db )         
!****************** Termination of the Program/Subprogram **********************
      RETURN      
      END

      SUBROUTINE DSMNWS(NUMREC,LFpointer,SECTOR)

!****************** Description of the Program/Subprogram **********************
! THIS SUBROUTINE ADDS UP BASE SYSTEM LOAD
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr' !<< nems parameter declarations
      include 'ncntrl' !<< acces to nems global variables like
      include 'emmparm'
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen' !<< all ldsm parameter declarations
      include 'dsmcaldr' !<< calendar data
      include 'dsmhelm' !<< helm algorithm variables
      include 'dsmsectr' !<< sector secific data and other associated variables
      include 'dsmunits' !<< imsg output unit
      include 'dsmtfefp' !<< communication with efp
!********************** Declaring local variables *****************************
      INTEGER*2 K  ! month pointer
      INTEGER*2 L  ! day pointer
      INTEGER*2 M  ! hour pointer
      INTEGER*2 SECTOR ! sector number or 0 if not to process sectoral loads
      INTEGER*2 LFpointer ! pointer to LoadForec array or 0 if SystemLoad
      INTEGER*2 NUMREC ! number of record ond DAF-LSR-DB with current lsr
      REAL*4 DistLo(MAXHOUR) ! distribution of annual load over hours
      REAL*4 DistLoTMP(MAXHOUR) ! distribution of annual load over hours
      REAL*8 load1 ! temporary variables for annual load forecast orig. appr.
      REAL*8 load2 ! temporary variables for annual load forecast delta appr.
      CHARACTER*8 LSRname ! Current lsr name
      LOGICAL EUorECM ! flag depicting if the item is an eu (T) or ecm (F)
!****************** Initializing Variables *************************************
!****************** Body of the Program/Subprogram *****************************
!      real sysload_debug, h2_elec_consumed_debug

      COMMON /REGSYSLD/LSRname_s,DistLo_s
      REAL*4 DistLo_s(MNUMNR,MAXHOUR)
      CHARACTER*8 LSRname_s(MNUMNR)
	  
	  CHARACTER*8 LSReuname_s(MAXEU)
	  REAL*4 DistLo_eu_s(MAXEU,MAXHOUR) 
	  INTEGER*4 DBnr  ! DB number of records
	  COMMON /EUSYSLD/LSReuname_s,DistLo_eu_s, DBnr
      
! If(curiyr+1989.le.2009) Write(6,*)'in DSMNWS ',RNB,Numrec,Lfpointer,Sector

! Read a record from the Sqlite DB
      IF (Numrec.eq.0) THEN 
         Do m=1,nhour
           DistLo(M) =  DistLo_s(RNB,m)
!            if (RNB.eq.2.and.(curiyr+1989.eq.2010.or.curiyr+1989.eq.2020 ).AND.CURITR.EQ.1) Write(6,*)'test distlo db ',LSRname_s(RNB),RNB,m,DistLo(M)
         Enddo
      Else
         do m=1,nhour
           DistLo(m)=DistLo_eu_s(NUMREC,m)
		 enddo
		

         Do m=1,nhour
!             if (RNB.eq.2.and.(curiyr+1989.eq.2010.or.curiyr+1989.eq.2020 ).AND.CURITR.EQ.1)  Write(6,*)'test distlo daf ',LSRname,RNB,m,DistLo(M)
         Enddo
      Endif

      IF(LFpointer.NE.0) THEN
        load1=LoadForec(LFpointer,1)
        load2=LoadForec(LFpointer,2)
      ELSE
        load1=0.0
        load2=SystemLoad              !if the item is a system load
      ENDIF
      if (ISNAN(load1)) THEN
         WRITE(6,2311) CURIYR+1989,CURITR,RNB,SECTOR,LFpointer,LSRname,LoadForec(LFpointer,1),LoadForec(LFpointer,2)
 2311    FORMAT(1X,"NaNQ_Electricity_Demands",5(":",I4),":",A8,2(":",E15.6))
         load1 = 0.000
      endif
      IF(load2.NE.0.0) THEN
         if (ISNAN(load1)) THEN
            WRITE(6,2311) CURIYR+1989,CURITR,RNB,SECTOR,LFpointer,LSRname,LoadForec(LFpointer,1),LoadForec(LFpointer,2)
            load2 = 0.000
      endif     
      DO M=1,nhour
        SYLOAD(M)=SYLOAD(M)+DistLo(M)*load2 !VLA Here total annual laod is disaggregated into hourly load where load2 is total system load
        
      ENDDO
!  if (RNB.eq.2.and.(curiyr+1989.eq.2010.or.curiyr+1989.eq.2020 ).AND.CURITR.EQ.1) THEN
!  write(IMSG,*)'LSRNAME   ',LSRname
!   WRITE(IMSG,*)'load2,DistLo(1..10),SYLOAD(1...10)' &
!   ,load2,(DistLo(M),M=1,10),(SYLOAD(M),M=1,10)
!   ENDIF
      ENDIF
      IF(SECTOR.NE.0) THEN
        IF(K1.EQ.CURIYR) THEN
        TotSecLoad(RNB,SECTOR)=TotSecLoad(RNB,SECTOR)+load1
          DO M=1,nhour
            SectorLoad(M,SECTOR)=SectorLoad(M,SECTOR) +DistLo(M)*load1
          ENDDO
!  if (RNB.eq.2.and.(curiyr+1989.eq.2010.or.curiyr+1989.eq.2020 ).AND.CURITR.EQ.1) THEN       
!  write(IMSG,*)'LSRNAME   ',LSRname
!    WRITE(IMSG,*)'load1,DistLo(1..10),SectorLoad(1...10)' &
!    ,load1,(DistLo(M),M=1,10),(SectorLoad(M,SECTOR),M=1,10),nhour
! Endif
        ENDIF
      ENDIF
!****** Termination of the Program/Subprogram **********************
      RETURN
      END

      SUBROUTINE DSMQSI(TOSORT,INDEX,JNUMB,MXSIZE)

      IMPLICIT NONE
!**********************************************************************
! THIS SUBROUTINE SORTS AN INTEGER ARRAY
!***  TOSORT IS THE INPUT ARRAY (INTEGER*2)
!***  INDEX IS THE ASSOCIATED ARRAY (INTEGER*2)
!***  JNUMB IS THE NUMBER OF ELEMENTS
!**********************************************************************
!***
      INTEGER*2 MXSIZE
      INTEGER*2 TOPS(24),BOTTOM(24)
      INTEGER*2 TOSORT(MXSIZE),PIVOT
      INTEGER*2 INDEX(MXSIZE),PIVOTI
      INTEGER*2 JNUMB
      INTEGER*2 NUMBER
      INTEGER*2 ITEMP
      INTEGER*2 IPART
      INTEGER*2 INIT
      INTEGER*2 LIMDEX
      INTEGER*2 MEDIAN
      INTEGER*2 ITOP
      INTEGER*2 IBOT
      INTEGER*2 TEMP2I
      REAL*4 TEMP2
      INTEGER*2 MIN
      INTEGER*2 I
!***
!**********************************************************************
      NUMBER = JNUMB
      IF (NUMBER.GT.2) GO TO 75
      IF (TOSORT(1).LE.TOSORT(2)) RETURN
      ITEMP=TOSORT(1)
      TOSORT(1)=TOSORT(2)
      TOSORT(2)=ITEMP
      ITEMP=INDEX(1)
      INDEX(1)=INDEX(2)
      INDEX(2)=ITEMP
      RETURN
   75 IPART=1
      INIT=1
      LIMDEX=1
      IF (NUMBER.LT.10) GO TO 450
      GO TO 400
  100 MEDIAN=(IPART+NUMBER)/2
      PIVOT=TOSORT(MEDIAN)
      PIVOTI=INDEX(MEDIAN)
      ITOP=IPART
      IBOT=NUMBER
      IF (TOSORT(IPART).GE.PIVOT) GO TO 150
      TOSORT(MEDIAN)=TOSORT(IPART)
      TOSORT(IPART)=PIVOT
      PIVOT=TOSORT(MEDIAN)
      INDEX(MEDIAN)=INDEX(IPART)
      INDEX(IPART)=PIVOTI
      PIVOTI=INDEX(MEDIAN)
  150 IF (TOSORT(NUMBER).LE.PIVOT) GO TO 200
      TOSORT(MEDIAN)=TOSORT(NUMBER)
      TOSORT(NUMBER)=PIVOT
      PIVOT=TOSORT(MEDIAN)
      INDEX(MEDIAN)=INDEX(NUMBER)
      INDEX(NUMBER)=PIVOTI
      PIVOTI=INDEX(MEDIAN)
      IF (TOSORT(IPART).GE.PIVOT) GO TO 200
      TOSORT(MEDIAN)=TOSORT(IPART)
      TOSORT(IPART)=PIVOT
      PIVOT=TOSORT(MEDIAN)
      INDEX(MEDIAN)=INDEX(IPART)
      INDEX(IPART)=PIVOTI
      PIVOTI=INDEX(MEDIAN)
  200 IBOT=IBOT-1
      IF (TOSORT(IBOT).LT.PIVOT) GO TO 200
      TEMP2=TOSORT(IBOT)
      TEMP2I=INDEX(IBOT)
  250 ITOP=ITOP+1
      IF (TOSORT(ITOP).GT.PIVOT) GO TO 250
      IF (ITOP.GT.IBOT) GO TO 300
      TOSORT(IBOT)=TOSORT(ITOP)
      TOSORT(ITOP)=TEMP2
      INDEX(IBOT)=INDEX(ITOP)
      INDEX(ITOP)=TEMP2I
      GO TO 200
  300 IF (IBOT-IPART.GE.NUMBER-ITOP) GO TO 350
      TOPS(LIMDEX)=IPART
      BOTTOM(LIMDEX)=IBOT
      IPART=ITOP
      GO TO 399
  350 TOPS(LIMDEX)=ITOP
      BOTTOM(LIMDEX)=NUMBER
      NUMBER=IBOT
  399 LIMDEX=LIMDEX+1
  400 IF (NUMBER-IPART.GT.10) GO TO 100
  450 MIN=IPART+1
      DO 425 I=MIN,NUMBER
      PIVOT=TOSORT(I)
      PIVOTI=INDEX(I)
      ITOP=I-1
      IF (TOSORT(ITOP).GE.PIVOT) GO TO 425
  475 TOSORT(ITOP+1)=TOSORT(ITOP)
      INDEX(ITOP+1)=INDEX(ITOP)
      ITOP=ITOP-1
      IF (ITOP.EQ.0) GO TO 485
      IF (TOSORT(ITOP).LT.PIVOT) GO TO 475
  485 TOSORT(ITOP+1)=PIVOT
      INDEX(ITOP+1)=PIVOTI
  425 CONTINUE
  500 LIMDEX=LIMDEX-1
      IF (LIMDEX.EQ.0) RETURN
      IPART=TOPS(LIMDEX)
      NUMBER=BOTTOM(LIMDEX)
      GO TO 400
      END

      SUBROUTINE DSMQSR(TOSORT,INDEX,FIRST,LAST,MXSIZE)

      IMPLICIT NONE
!**********************************************************************
! THIS SUBROUTINE SORTS A REAL ARRAY
!***  TOSORT IS THE INPUT ARRAY (REAL*4)
!***  INDEX IS THE ASSOCIATED ARRAY (INTEGER*2)
!***  FIRST IS THE FIRST ELEMENT OF THE RANGE TO BE SORTED
!***  LAST IS THE LAST ELEMENT OF THE RANGE TO BE SORTED
!**********************************************************************
!***
      INTEGER*2 TOPS(24),BOTTOM(24)
      INTEGER*2 MXSIZE
      REAL*4 TOSORT(MXSIZE),PIVOT
      INTEGER*2 INDEX(MXSIZE),PIVOTI
      INTEGER*2 JNUMB
      INTEGER*2 NUMBER
      INTEGER*2 ITEMP
      INTEGER*2 IPART
      INTEGER*2 LIMDEX
      INTEGER*2 MEDIAN
      INTEGER*2 ITOP
      INTEGER*2 IBOT
      INTEGER*2 TEMP2I
      REAL*4 TEMP2
      INTEGER*2 MIN
      INTEGER*2 I
      INTEGER*2 FIRST,LAST,SECOND,ZEROTH,ONE,TWO,ZERO,TEN
!***
!**********************************************************************
      ZERO=0
      ONE=1
      TWO=2
      TEN=10
      ZEROTH=FIRST-ONE
      SECOND=FIRST+ONE
      NUMBER=LAST
      JNUMB=LAST-FIRST+1
      IF (JNUMB.GT.TWO) GO TO 75
      IF (TOSORT(FIRST).LE.TOSORT(SECOND)) RETURN
      ITEMP=TOSORT(FIRST)
      TOSORT(FIRST)=TOSORT(SECOND)
      TOSORT(SECOND)=ITEMP
      ITEMP=INDEX(FIRST)
      INDEX(FIRST)=INDEX(SECOND)
      INDEX(SECOND)=ITEMP
      RETURN
   75 IPART=FIRST
      LIMDEX=ONE
      IF (JNUMB.LT.TEN) GO TO 450
      GO TO 400
  100 MEDIAN=(IPART+NUMBER)/TWO
      PIVOT=TOSORT(MEDIAN)
      PIVOTI=INDEX(MEDIAN)
      ITOP=IPART
      IBOT=NUMBER
      IF (TOSORT(IPART).GE.PIVOT) GO TO 150
      TOSORT(MEDIAN)=TOSORT(IPART)
      TOSORT(IPART)=PIVOT
      PIVOT=TOSORT(MEDIAN)
      INDEX(MEDIAN)=INDEX(IPART)
      INDEX(IPART)=PIVOTI
      PIVOTI=INDEX(MEDIAN)
  150 IF (TOSORT(NUMBER).LE.PIVOT) GO TO 200
      TOSORT(MEDIAN)=TOSORT(NUMBER)
      TOSORT(NUMBER)=PIVOT
      PIVOT=TOSORT(MEDIAN)
      INDEX(MEDIAN)=INDEX(NUMBER)
      INDEX(NUMBER)=PIVOTI
      PIVOTI=INDEX(MEDIAN)
      IF (TOSORT(IPART).GE.PIVOT) GO TO 200
      TOSORT(MEDIAN)=TOSORT(IPART)
      TOSORT(IPART)=PIVOT
      PIVOT=TOSORT(MEDIAN)
      INDEX(MEDIAN)=INDEX(IPART)
      INDEX(IPART)=PIVOTI
      PIVOTI=INDEX(MEDIAN)
  200 IBOT=IBOT-1
      IF (TOSORT(IBOT).LT.PIVOT) GO TO 200
      TEMP2=TOSORT(IBOT)
      TEMP2I=INDEX(IBOT)
  250 ITOP=ITOP+1
      IF (TOSORT(ITOP).GT.PIVOT) GO TO 250
      IF (ITOP.GT.IBOT) GO TO 300
      TOSORT(IBOT)=TOSORT(ITOP)
      TOSORT(ITOP)=TEMP2
      INDEX(IBOT)=INDEX(ITOP)
      INDEX(ITOP)=TEMP2I
      GO TO 200
  300 IF (IBOT-IPART.GE.NUMBER-ITOP) GO TO 350
      TOPS(LIMDEX)=IPART
      BOTTOM(LIMDEX)=IBOT
      IPART=ITOP
      GO TO 399
  350 TOPS(LIMDEX)=ITOP
      BOTTOM(LIMDEX)=NUMBER
      NUMBER=IBOT
  399 LIMDEX=LIMDEX+ONE
  400 IF (NUMBER-IPART.GT.TEN) GO TO 100
  450 MIN=IPART+ONE
      DO 425 I=MIN,NUMBER
      PIVOT=TOSORT(I)
      PIVOTI=INDEX(I)
      ITOP=I-ONE
      IF (TOSORT(ITOP).GE.PIVOT) GO TO 425
  475 TOSORT(ITOP+ONE)=TOSORT(ITOP)
      INDEX(ITOP+ONE)=INDEX(ITOP)
      ITOP=ITOP-ONE
      IF (ITOP.EQ.ZEROTH) GO TO 485
      IF (TOSORT(ITOP).LT.PIVOT) GO TO 475
  485 TOSORT(ITOP+ONE)=PIVOT
      INDEX(ITOP+ONE)=PIVOTI
  425 CONTINUE
  500 LIMDEX=LIMDEX-ONE
      IF (LIMDEX.EQ.ZERO) RETURN
      IPART=TOPS(LIMDEX)
      NUMBER=BOTTOM(LIMDEX)
      GO TO 400
      END

      FUNCTION DSMRMS(PAYBACK,SECTOR)
!****************** Description of the Program/Subprogram **********************
! THIS FUNCTION CALCULATES AFTER-DEMAND-SIDE-MANAGEMENT-OPTION-APPLICATION MARKET SHARE OF A TECHNOLOGY
!       the TO technology is expressed as a fraction of TO+FROM market share
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr' ! nems parameter declarations
      include 'emmparm'
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen' ! ldsm parameter declarations
      include 'dsmunits' !<< i/o units
      include 'dsmsectr' !<< sector specific data and other associated variables
!********************** Declaring local variables *****************************
      INTEGER*2 PAYBACK ! DSM option pay-back period
      INTEGER*2 SECTOR ! SECTOR IDENTIFIER
      REAL*4 DSMRMS ! RESULITING MARKET SHARE
      INTEGER*2 I,J,K ! UNIVERSAL POINTERS
      INTEGER*2 Nrow ! current number of data rows
!****************** Initializing Variables *************************************
      Nrow=0
!****************** Body of the Program/Subprogram *****************************
      IF(SECTOR.GT.PACnS) GOTO 999
!  count number of data points in PAC for the current sector
      DO I=1,PACnR
        IF(PACmp(I,SECTOR).GE.0.0) THEN
          Nrow=I
        ELSE
          Nrow=I-1
          GOTO 555
        ENDIF
      ENDDO
555   CONTINUE
! If only one value of market penetration was given on inpt return this value
      IF(Nrow.EQ.1) THEN
        DSMRMS=0.01*PACmp(1,SECTOR)
        RETURN
      ENDIF
      DO I=1,Nrow
        IF(PAYBACK.LT.PACpb(I)) THEN
          IF(I.EQ.1) THEN
!          extrapolate back if value lower than any value specified on input
            DSMRMS=0.01*((PACmp(2,SECTOR)-PACmp(1,SECTOR)) &
                /(PACpb(2)-PACpb(1)) &
                *(PAYBACK-PACpb(1)) &
                +PACmp(1,SECTOR))
          ELSE
!          otherwise interpolate
            DSMRMS=0.01*((PACmp(I,SECTOR)-PACmp(I-1,SECTOR)) &
                /(PACpb(I)-PACpb(I-1)) &
                *(PAYBACK-PACpb(I-1)) &
                +PACmp(I-1,SECTOR))
          ENDIF
          GOTO 998
        ENDIF
      ENDDO
!    extrapolate if value higher than any value specified on input
      DSMRMS=0.01*((PACmp(Nrow,SECTOR)-PACmp(Nrow-1,SECTOR)) &
          /(PACpb(Nrow)-PACpb(Nrow-1)) &
          *(PAYBACK-PACpb(Nrow)) &
          +PACmp(Nrow,SECTOR))
998   IF(DSMRMS.LT.0.0) DSMRMS=0.0
!****************** Termination of the Program/Subprogram **********************
      RETURN
999   WRITE(IMSG,*)'<)) Message from FUNCTION DSMRMS'
      WRITE(IMSG,*)'<))) PAC HAS NOT BEEN DEFINED FOR SECTOR #:',SECTOR
      WRITE(IMSG,*)'<))) DSMRMS ASSUMED  = 1.0'
      DSMRMS=1.0
      RETURN
      END

      SUBROUTINE DSMRST(WHOOPS)

!     ****************** Description of the Program/Subprogram **********************
! THIS SUBROUTINE READS A FILE DEFINING THE ANALYZED SYSTEM STRUCTURE AND SUPPLIES CONTROL DATA FOR THE RUN
!     *******************************************************************************
      USE SQLITE
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      INTEGER*2 ONE
      PARAMETER(ONE=1)
      include 'parametr' !<< nems parameter declarations
      include 'dsmdimen' !<< all ldsm parameter declarations
      include 'emmparm'
      include 'ecpcntl'
      include 'dispett'
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmunits' !<< include file with unit number for ldsm message file
      include 'dsmfmgrd' !<< file_mgr variables declarations
      include 'dsmsectr' !<< sector specific data and other associated variables
      include 'dsmtoefd' !<< commumication with efd
      include 'dsmnemsc' !<< results of ldsm to be passed to the rest of nems
      include 'dsmtfecp' !<< communication with ecp
      include 'dsmtfefp' !<< communication with efp
      include 'dsmcaldr' !<< calendar data
      include 'dsmnercr' !<< nerc region data
      include 'dsmoptdb' !<< dsm option data base
      include 'dsmhelm'
      include 'dsmrept'  !<< ldsm reports specificaton
      include 'control'
      include 'ncntrl'
      include 'wrenew'    ! mapping vars for ecp and efd hours to group number
      include 'eusprc'
      include 'edbdef'
      include 'uefdout'
!********************** Declaring local variables *****************************
      INTEGER*2 d,hr
      INTEGER*2 I,J,K,m,n,h,t,p,r,s,v,n1,n2,b1,nn,m1,m2,inrgn  ! temporary variables
      INTEGER*4 l,iyr,ireg,icls     ! pw2
      INTEGER*4 FILE   ! I/O unit for struct file
      INTEGER*4 DAFnr  ! DAF-LSR-DB number of records
	  
      INTEGER*4 RRFnr  ! RESIDENTIAL RESTART FILE NUMBER OF RECORDS
      INTEGER*4 CRFnr  ! COMMERCIAL RESTART FILE NUMBER OF RECORDS
      CHARACTER*8 dafil ! DAF-LSR-DB file name
      CHARACTER*8 LSRnam,LSRnames(MAXCBT)! current lsr name(s)
      CHARACTER*8 DbLSRname(MAXREC) ! names of all lsrs on DAF-LSR-DB
      CHARACTER*8 REG  !CURRENT NERC REGION NAME
      CHARACTER*8 LREG(MAXRLST) !CURRENT LIST OF REGION NAMES
      CHARACTER*40 DUMMYchar ! dummy character variable
      CHARACTER*5 SGTYPE
      INTEGER*2 num,lengthm,lengthh    ! number of items following the line
      LOGICAL*1 FlagEU ! becomes FALSE when firest e-u for a sector is detected
      LOGICAL*1 FlagECM ! becomes FALSE when firest ecm for a sector is detected
      CHARACTER*12 MonNam(MAXMON,864) ! month names used to define LDC's
      CHARACTER*12 DtpNam(MAXDTP) ! day-type names used to define ECP LDC's
      EXTERNAL DSMCMP ! subroutine for comapring character strings
      LOGICAL DSMCMP,CHECK ! as above
      INTEGER*2 ECPsgDmonth(MAXMON) ! months numbers in a ECP LDC segments
      INTEGER*2 EFDsgDmonth(MAXMON) ! months numbers in a EFD LDC segments
      INTEGER*2 ETTsgDmonth(MAXMON) ! months numbers in a ETT LDC segments
      INTEGER*2 ECIsgDmonth(MAXMON) ! months numbers in a ECI LDC segments
      INTEGER*2 DnD ! Number of day-types in a ECP/EFD LDC segment
      INTEGER*2 DnM(864) ! Number of months in a ECP LDC segment
      INTEGER*2 DnH! Number of hourly intervals in a ECP/EFD LDC segm.
      INTEGER*2 Dday(MAXDTP) ! day-types in a ECP/EFD LDC segments
      INTEGER*2 Dhour(MAXITV,2) ! lower, upper hour of intervals
      INTEGER*2 Sindex ! current segment index
      INTEGER*2 ECPsgWidth(MAXECPSG) ! widths of ECP LDC segments
      INTEGER*2 EFDsgWidth(MAXEFDSG) ! widths of EFD LDC segments
      INTEGER*2 ETTsgWidth(MAXETTSG) ! widths of ETT LDC segments
      INTEGER*2 ECIsgWidth(MAXECISG) ! widths of ECI LDC segments
      INTEGER*2 MONGRP(MAXMON)       ! map month to LDC segment (group) to be used to map to season
      integer*2 tmp(12,2)   ! temporary seasonal def - start and stop grp numbers for each season
      integer*2 ECP_GRP, EFD_GRP, ETT_GRP, ECI_GRP
      integer*2 ECP_SP, EFD_SP, ETT_SP, ECI_SP
      integer*2 ECP_SEG, EFD_SEG, ETT_SEG, ECI_SEG, ECI_SLC
      REAL*8 TMP_ECP(0:MAXECPB) ! TEMP widths of ECP LDC segments
      REAL*8 TMP_EFD(0:MAXEFDB) ! TEMP widths of EFD LDC segments
      REAL*8 TMP_ETT(0:MAXETTB) ! TEMP widths of ETT LDC segments
      REAL*8 TMP_ECI(0:MAXECIB) ! TEMP widths of ECI LDC segments
      REAL*8 TMP_HRS
      REAL*4 x ! temporary variable
      INTEGER*2 sgn ! current segment number
      CHARACTER*7 ProgNam ! current DSM Program name
      CHARACTER*1 RegLd ! Region list descriptor
      CHARACTER*1 DecTd ! Decision type descriptor
      CHARACTER*2 BuilTd ! Building type descriptor
      CHARACTER*7 FtechLSR(MAXFRT) ! FROM technologies LSR names
      CHARACTER*7 TtechLSR(MAXTOT) ! TO techanologies LSR names
      Character*8 LsrNamSys(MNUMNR)
      INTEGER*4 TECHIDENT(MAXRRST) ! IDENTIFIERS ON RESTART FILE
      INTEGER*4 IDENTIF ! CURRENT TECHNOLOGY IDENTIFIER
      INTEGER*2 FCTC(NUMCTCE,MAXNTPO) ! FROM Commercial technology codes
      INTEGER*2 TCTC(NUMCTCE,MAXNTPO) ! TO Commercial technology codes
      LOGICAL WHOOPS ! error flag
      CHARACTER*12 SLNM ! sector name used during reading mapping matrices
      CHARACTER*2 CI1,CI2
      external getindex,rtovalue
      integer getindex,rtovalue
      
            type(sqlite_database)                      :: db
            type(sqlite_statement)                     :: stmt,stmt3
            type(sqlite_column), dimension(:), pointer :: col,col3
            character(len=40), pointer, dimension(:)   :: result
            character(len=80)                          :: errmsg
            logical                                    :: finished
      Integer*4 NREPREG_sq
      Character*8 LSRnam_sq(MNUMNR,MAXSEC,MAXEU), LSRnam2, y_sq
      real x_sq(MNUMNR,MAXSEC,MAXEU)
      Integer*4 ID,IK,JK,MNUMNR_sq
	  
	 
      
      CHARACTER*8 LSReuname_s(MAXEU)
	  REAL*4 DistLo_eu_s(MAXEU,MAXHOUR) 
	  INTEGER*4 DBnr  ! DB number of records
	  COMMON /EUSYSLD/LSReuname_s,DistLo_eu_s, DBnr
      
!     ****************** Initializing Variables *************************************
!     Define character strings which identify sectors

      SecNam(RES)='RES'
      SecNam(COM)='COM'
      SecNam(IND)='IND'
      SecNam(TRA)='TRA'
      Neu=0

!     Actual number of regions to be processed


      nCENSUSreg=MNUMCR-2
      DO I=1,MAXSEC
         DO J=1,2
            EUINDEX(I,J)=0   !if a sector has no e-uses EUINDEX(I,J)=0
         ENDDO
      ENDDO
      SumSegWidth=0
      DO I=1,MNUMNR
         NOREGRPT(I)=.FALSE.
      ENDDO
      DO I=1,UNYEAR
         NOYEARRPT(I)=.FALSE.
      ENDDO

      CALL EMM_LDSM
      
          call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )

           allocate ( col3(8) )
                 ! QUERY THE DATABASE TABLE
                   call sqlite3_column_query( col3(1), 'ID', sqlite_int ) 
                   call sqlite3_column_query( col3(2), 'EMM_REG', sqlite_int )
                   call sqlite3_column_query( col3(3), 'EMM_REG_NM', sqlite_char )
                   call sqlite3_column_query( col3(4), 'LSRnam', sqlite_char )
                   call sqlite3_column_query( col3(5), 'UQTDLS', sqlite_real )
                   call sqlite3_column_query( col3(6), 'MEFAC', sqlite_real )
                   call sqlite3_column_query( col3(7), 'DEMINT', sqlite_real )
                   call sqlite3_column_query( col3(8), 'NREPREGI', sqlite_int )
                   
                   call sqlite3_prepare_select( db, 'V_EMM_LDSMSTR_RGN', col3, stmt3 )
                   
                   !write(6,*) 'can you read this1'
         
                 ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
                   do
                     !write(6,*) 'can you read this1a'
                     call sqlite3_next_row( stmt3, col3, finished )
                     !write(6,*) 'can you read this1b'
                     if ( finished ) exit
                     
                     !write(6,*) 'can you read this2'
                     call sqlite3_get_column( col3(1), ID )
                     call sqlite3_get_column( col3(2), MNUMNR_sq )
                     call sqlite3_get_column( col3(3), NERCnam(MNUMNR_sq) )
                     call sqlite3_get_column( col3(4), LSRnamSys(MNUMNR_sq) )
                     call sqlite3_get_column( col3(5), UQTDLS(MNUMNR_sq) )
                     call sqlite3_get_column( col3(6), MEFAC(MNUMNR_sq) )
                     call sqlite3_get_column( col3(7), DEMINT(MNUMNR_sq) )
                     call sqlite3_get_column( col3(8), NREPREG_sq )
                     

                     NREPREG  = NREPREG_sq
               
            !write(6,*) 'can you read this3'
               
             !write(6,*)'LSRNa ',LSRnamSys(ID), 'UQTDLS(ID)',UQTDLS(ID),'NERCnam(ID)',NERCnam(ID)
               
                  enddo
                          
           deallocate ( col3 )  
           finished = .FALSE.
          
          allocate ( col(7) )
            ! QUERY THE DATABASE TABLE
              call sqlite3_column_query( col(1), 'ID', sqlite_int ) 
              call sqlite3_column_query( col(2), 'EMM_REG', sqlite_int )
              call sqlite3_column_query( col(3), 'SECTOR', sqlite_int )
              call sqlite3_column_query( col(4), 'EUSE', sqlite_int )
              call sqlite3_column_query( col(5), 'LSRnam', sqlite_char )
              call sqlite3_column_query( col(6), 'BSYRLD', sqlite_real )
              call sqlite3_column_query( col(7), 'EMM_NM', sqlite_char )              
              call sqlite3_prepare_select( db, 'V_EMM_LDSMSTR_EU_SECT', col, stmt )
                      ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
              do
                call sqlite3_next_row( stmt, col, finished )
                if ( finished ) exit
                          call sqlite3_get_column( col(1), ID )
                call sqlite3_get_column( col(2), MNUMNR_sq )
                call sqlite3_get_column( col(3), IK )
                call sqlite3_get_column( col(4), JK)
                call sqlite3_get_column( col(5), LSRnam_sq(MNUMNR_sq,IK,JK) )
                call sqlite3_get_column( col(6), x_sq(MNUMNR_sq,IK,JK) )
                call sqlite3_get_column( col(7), y_sq )
              enddo	                                
          deallocate ( col )
          call sqlite3_close( db ) 
		  
          call CALLSYSLOAD_EU	!TD	  
      
!      write(6,*)'Can read everything in EMM_LDSM'
!     ****************** Body of the Program/Subprogram *****************************
!     * Read the DAF LSR database
!     * Number of records and calendar data from the first record

      NEW = .FALSE.
      fname = 'LDSMDAF'
      IODB = FILE_MGR('O',fname,NEW)  !Open DAF-LSR-DB

      READ(IODB,REC=1)DAFnr,NMONTH,(NODAYS(I),I=1,NMONTH), &
         ((WEIGHT(J,K),J=1,NODAYS(K)),K=1,NMONTH),NODAYT,NOSEA, &
         (MONTYP(I),I=1,NMONTH),((IDAYTQ(I,J),I=1,NODAYS(J)),J=1,NMONTH), &
         ((JDAYTP(I,J),i=1,NODAYS(J)),J=1,NMONTH),(SENAME(I),I=1,NOSEA), &
         (DTNAME(I),I=1,NODAYT),(MONAME(i),i=1,NMONTH)
		
 ! this reads DbLSRname from the DAF file. See LC2 note in the code just after format 2793. May not be needed but leaving it alone for now.- TD
 ! DAFnr=         544 (from current DAF file)
      DO l=2,DAFnr
         READ(IODB,REC=l)DbLSRname(l)		 
      ENDDO
! overwrite ldsmdaf inuts with calendar data from the database 
      CALL LOAD_LDSM_CAL


	 
!
!     Open the structure file

      NEW=.FALSE.
      fname='LDSMSTR'
      FILE=FILE_MGR('O',fname,NEW)
      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN

!  Read number of top coincident peak hours to be used to calculate average coincident peak load for EFP
!      (PCP purposes)

      READ(FILE,*)NpeakH

!     Read definition of LDCs that are to be produced by the module
!     Read defintion of LDC for ECP module
!     Read number of segments

      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN
      READ(FILE,*)ECPnumSg

!     Read definition of blocks. All hours are allocated to a number of
!     segments defined by calendar months, day-types and hourly intervals.
!     Within each of segment, blocks are defined by percentages of hours which
!     they cover. Each block can be either 'peak' or 'non-peak'. Peak blocks
!     are represented by the peak load in the block. Non-peak blocks are
!     represented by the average load for the block.
!        DnM stores number of months
!        DnD - number of day-types
!        DnH - number of hourly intervals
!        ECIsgDnB(MAXECPSG) - number of blocks in a segment
!        ECIsgDmonth(MAXMON) - month numbers
!        Dday(MAXDTP) - day-type numbers
!        Dhour(MAXINTV,1) and Dhour(MAXINTV,2) - lower and upper bounds of hour intervals
!        ECIsgDblock(MAXECPSG,MAXECPB) - widths of blocks defined as percent of total hours in a segment

      DO I=1,ECPnumSg  ! do for all segments
         CALL DSMSKP(FILE,WHOOPS)
         IF(WHOOPS) RETURN

!        Read segment definition

         READ(FILE,*)nn,ECPSEGDSC(nn),DnM(nn),(MonNam(J,nn),J=1,DnM(nn)) &
            ,DnD,(DtpNam(J),J=1,DnD) &
            ,DnH,((Dhour(J,K),K=1,2),J=1,DnH) &
            ,ECPsgDnB(I),(ECPsgDblock(I,J),ECPsgDbltyp(I,J) &
            ,J=1,ECPsgDnB(I))

         DO J=1,DnM(nn)

!           Translate names of months and day-types into their index numbers

            DO K=1,NMONTH

!              write(imsg,*)'J,K,Mon,MONAME',J,K,'"',MonNam(J,nn),'"',MONAME(K),'"'

               IF(DSMCMP(MonNam(J,nn),MONAME(K))) GOTO 1200
            ENDDO
            GOTO 996
 1200       ECPsgDmonth(J)=K
         ENDDO
         DO J=1,DnD
            DO K=1,NODAYT
               IF(DSMCMP(DtpNam(J),DTNAME(K))) GOTO 1300
            ENDDO
            GOTO 995
 1300       Dday(J)=K
         ENDDO
!
!        Fill 864hr-mapping variable (HrToECPgrp) with group number for each hour in this (nn) line
!
         Do J=1,DnM(nn)              ! do for months assigned in current line month number has been assigned above  ECPsgDmonth(J)
            Do d=1,DnD               ! do for each daytype in currrent line daytype number has been assigned above  Dday(d)
               Do h=1,DnH            ! do for each set of hours in current line
                  Do hr=Dhour(h,1), Dhour(h,2)    ! do for each hour in set
                     HrToECPgrp(ECPsgDmonth(J),Dday(d),hr) = nn
                  Enddo     !hr
               Enddo        !hour set
            Enddo           !daytype
         Enddo              !month
!
!        For each calendar hour that is assigned to the current segment,
!        input to the ECPsgNum(MAXHOUR) table an index of the current segment, I

         t=0 ! counts number of real hours in each of the segments (total widths)
         DO J=1,DnM(nn) ! do for months assigned to current segment
            l=0 ! hour in a calendar year
            DO K=1,ECPsgDmonth(J)-1 !calculate number of last calendar hour
               l=l+NODAYS(K)*24     !in preceeding month
            ENDDO
            DO K=1,NODAYS(ECPsgDmonth(J)) !do for each day in a current month
               DO p=1,DnD !check if its day-type fits to the current block
                  IF(JDAYTP(K,ECPsgDmonth(J)).EQ.Dday(p)) THEN
                     DO n=1,DnH
                        DO h=Dhour(n,1),Dhour(n,2)
                           ECPsgNum(l+h)=I
                           t=t+WEIGHT(K,ECPsgDmonth(J))
                        ENDDO
                     ENDDO
                  ENDIF
               ENDDO
               l=l+24
            ENDDO
         ENDDO
         ECPsgWidth(I)=t
         SumSegWidth=SumSegWidth+t
!
!        Set ECP load definitions for emm database tables
!
         ECPLGSD(nn) = ECPSEGDSC(nn)
         ECPLGMD(nn) = ' '
         ECPLGHD(nn) = 'Hours: '
         DO J = 1 , Dnm(nn)
           lengthm = len_trim(ecplgmd(nn))
           IF (J .EQ. 1) then
             ECPLGMD(nn) = MonNam(J,nn)(1:4) // ' , '
           ELSEIF (J .NE. Dnm(nn)) then
             ECPLGMD(nn) = ECPLGMD(nn)(1:lengthm) // MonNam(J,nn)(1:4) // ', '
           ELSE
             ECPLGMD(nn) = ECPLGMD(nn)(1:lengthm) // MonNam(J,nn)(1:4)
           ENDIF
         ENDDO
         DO J = 1 , Dnh
           Write(ci1,'(I2)') Dhour(J,1)
           Write(ci2,'(I2)') Dhour(J,2)
             lengthh = len_trim(ecplghd(nn))
           IF (J .EQ. 1) then
             ECPLGHD(nn) = ECPLGHD(nn)(1:lengthh) // CI1 // ' - ' // CI2
           ELSE
             ECPLGHD(nn) = ECPLGHD(nn)(1:lengthh) // ' , ' // CI1 // ' - ' // CI2
           ENDIF
         ENDDO



      ENDDO
      IF(SumSegWidth.NE.8760 .AND. SumSegWidth.NE.8784) GOTO 994

! Now ECPsgNum stores, for each calendar hour, an index of an ECP LDC segment to which this hour belongs
!     Develop a vector of ordinal numbers of hours

      nhour=0
      nhouryr=0
      DO I=1,NMONTH
         DO J=1,NODAYS(I)
            DO h=1,24
               nhouryr=nhouryr+WEIGHT(J,I)
               nhour=nhour+1
               HourNumber(nhour)=nhour
               HourNumberEFD(nhour)=nhour
               HourlyWeights(nhour)=WEIGHT(J,I)
               MON864(nhour) = I
               DAYT864(nhour) = J
               HR864(nhour) = h
           ENDDO
         ENDDO
      ENDDO

!     Sort calendar hours by segment

      CALL DSMQSI(ECPsgNum,HourNumber,nhour,MAXHOUR)
!
!     Calculate division points (x coordinates) that define the LDC blocks within each segment
!     (beginning of segment=0.0, in real hours)

      K=0 ! ordinal number for blocks in the whole LDC
      DO I=ECPnumSg,1,-1
         x=0.0
         DO J=1,ECPsgDnB(I)
            K=K+1
            ECPblWidth(K)=ECPsgWidth(I)*ECPsgDblock(I,J)/100.0
            ECPblSeg(K)=I
            ECPblSlc(K)=J
            x=x+ECPblWidth(K)
            ECPblockx(I,J)=x
         ENDDO
      ENDDO

!     Find for each segment first and last hour (in new sort order)

      sgn=ECPnumSg
      ECPsgFh(sgn)=1
      DO i=1,nhour
         IF(ECPsgNum(i).NE.sgn) THEN
            ECPsgLh(sgn)=i-1
            sgn=sgn-1
            ECPsgFh(sgn)=i
         ENDIF
      ENDDO
      ECPsgLh(sgn)=nhour
      do sgn = 1, maxecpsg
!       write(6,*)'load2dbg ',ecpnumsg,sgn,ecpsglh(sgn),ecpsgfh(sgn)
      enddo
      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN

!     Definition of seasons for ECP

      READ(FILE,*)ECPnS
      DO I=1,ECPnS
         CALL DSMSKP(FILE,WHOOPS)
         IF(WHOOPS) RETURN
         READ(FILE,*)n,n1,n2,UNGSSN_ECP(I)

         DO ECP_GRP = n1 , n2

            WRITE(IMSG,1713) CURIRUN, CURIYR+1989, CURITR, I, ECP_GRP, n, n1, n2, UNGSSN_ECP(I)
 1713       FORMAT(1X,"ECP_LDSMSTR",9(":",I6))

            MAP_ECP_SP(ECP_GRP) = I
         END DO

         IF(n.LE.MAXECPS) THEN
            ECPSEDEF(n,1)=n1
            ECPSEDEF(n,2)=n2
         ELSE
            WRITE(IMSG,*)'<))) Invalid ECP seasone number'
            WHOOPS=.TRUE.
            RETURN
         ENDIF
      ENDDO
!
!     Fill 864hr-mapping variable (HrToECPseas) with season number for each of 864 hrs
!
      Do m=1,12
         Do d=1,3
            Do h=1,24
               ECP_GRP = HrToECPgrp(m,d,h)
               do i=1,ECPnS
                  if (ECP_GRP.ge.ECPSEDEF(i,1) .and. ECP_GRP.le.ECPSEDEF(i,2)) then
                     HrToECPseas(m,d,h) = i
                  endif
               enddo
            enddo
         enddo
      enddo

!     Read defintion of LDC for EFD module
!     Read number of segments

      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN
      SumSegWidth=0.0
      READ(FILE,*)EFDnumSg

!     write(6,*)EFDnumSg

!     Read definition of blocks. All hours are allocated to a number of
!     segments defined by calendar months, day-types and hourly intervals.
!     Within each of segment, blocks are defined by percentages of hours which
!     they cover. Each block can be either 'peak' or 'non-peak'. Peak blocks
!     are represented by the peak load in the block. Non-peak blocks are
!     represented by the average load for the block.
!        DnM stores number of months
!        DnD - number of day-types
!        DnH - number of hourly intervals
!        ECIsgDnB(MAXECPSG) - number of blocks in a segment
!        ECIsgDmonth(MAXMON) - month numbers
!        Dday(MAXDTP) - day-type numbers
!        Dhour(MAXINTV,1) and Dhour(MAXINTV,2) - lower and upper bounds of hour intervals
!        ECIsgDblock(MAXECPSG,MAXECPB) - widths of blocks defined as percent of total hours in a segment

      DO I=1,EFDnumSg  ! do for all segments
         CALL DSMSKP(FILE,WHOOPS)
         IF(WHOOPS) RETURN

!        Read segment definition

         READ(FILE,*)nn,EFDSEGDSC(nn),DnM(nn),(MonNam(J,nn),J=1,DnM(nn)) &
            ,DnD,(DtpNam(J),J=1,DnD) &
            ,DnH,((Dhour(J,K),K=1,2),J=1,DnH) &
            ,EFDsgDnB(I),(EFDsgDblock(I,J),EFDsgDbltyp(I,J) &
            ,J=1,EFDsgDnB(I))

         DO J=1,DnM(nn)

!           Translate names of months and day-types into their index numbers
            DO K=1,NMONTH
               IF(DSMCMP(MonNam(J,nn),MONAME(K))) GOTO 2200
            ENDDO
            GOTO 996
 2200       EFDsgDmonth(J)=K
            MONGRP(K)=NN
         ENDDO
          
         DO J=1,DnD
            DO K=1,NODAYT
               IF(DSMCMP(DtpNam(J),DTNAME(K))) GOTO 2300
            ENDDO
            GOTO 995
 2300       Dday(J)=K
         ENDDO
!
!        Fill 864hr-mapping variable (HrToEFDgrp) with group number for each hour in this (nn) line
!
         Do J=1,DnM(nn)              ! do for months assigned in current line month number has been assigned above  EFDsgDmonth(J)
            Do d=1,DnD               ! do for each daytype in currrent line daytype number has been assigned above  Dday(d)
               Do h=1,DnH            ! do for each set of hours in current line
                  Do hr=Dhour(h,1), Dhour(h,2)    ! do for each hour in set
                     HrToEFDgrp(EFDsgDmonth(J),Dday(d),hr) = nn
                  Enddo     !hr
               Enddo        !hour set
            Enddo           !daytype
         Enddo              !month
!
!        For each calendar hour that is assigned to the current segment,
!        put in EFDsgNum(MAXHOUR) an index of the current segment (I)

         t=0 ! counts number of real hours in each of the segments (total widths)
         DO J=1,DnM(nn) ! do for months assigned to current segment
            l=0 ! hour in a calendar year
            DO K=1,EFDsgDmonth(J)-1 !calculate number of last calendar hour
               l=l+NODAYS(K)*24     !in preceeding month
            ENDDO
            DO K=1,NODAYS(EFDsgDmonth(J)) !do for each day in a current month
               DO p=1,DnD !check if its day-type fits to the current block
                  IF(JDAYTP(K,EFDsgDmonth(J)).EQ.Dday(p)) THEN
                     DO n=1,DnH
                        DO h=Dhour(n,1),Dhour(n,2)
                           EFDsgNum(l+h)=I
                           t=t+WEIGHT(K,EFDsgDmonth(J))
                        ENDDO
                     ENDDO
                  ENDIF
               ENDDO
               l=l+24
            ENDDO
         ENDDO
         EFDsgWidth(I)=t
         SumSegWidth=SumSegWidth+t
!
!        Set EFD load definitions for emm database tables
!
         EFDLGSI(nn) = nn
         EFDLGD(nn) = 'Hours: '
         DO J = 1 , Dnh
           Write(ci1,'(I2)') Dhour(J,1)
           Write(ci2,'(I2)') Dhour(J,2)
           lengthh = len_trim(EFDLGD(nn))
           IF (J .EQ. 1) then
             EFDLGD(nn) = EFDLGD(nn)(1:lengthh) // CI1 // ' - ' // CI2
           ELSE
             EFDLGD(nn) = EFDLGD(nn)(1:lengthh) // ', ' // CI1 // ' - ' // CI2
           ENDIF
         ENDDO
           DO J = 1 , EFDsgDnB(nn)
             Write(ci1,'(I2)') INT(EFDsgDBLOCK(NN,J))
               IF ( EFDsgDblTyp(NN,J) .EQ. 'P' ) SGTYPE = 'Peak'
               IF ( EFDsgDblTyp(NN,J) .EQ. 'N' ) SGTYPE = 'NPeak'
             IF (J .EQ. 1) THEN
              EFDLSGD(NN,J) = SGTYPE // '-Highest Seg in Sea/Grp-' // CI1 // '%'
             ELSE
              EFDLSGD(NN,J) = SGTYPE // '-Next-Highest Seg in Sea/Grp-' // CI1 // '%'
             ENDIF
         ENDDO

      ENDDO
      IF(SumSegWidth.NE.8760 .AND. SumSegWidth.NE.8784) GOTO 994

! Now EFDsgNum stores, for each calendar hour, an index of an EFD LDC segment to which this hour belongs
!     Sort calendar hours by segment

      CALL DSMQSI(EFDsgNum,HourNumberEFD,nhour,MAXHOUR)
!
!     Calculate division points (x coordinates) that define the LDC blocks within each segment
!     (beginning of segment=0.0, in real hours)

      K=0 ! ordinal number for blocks in the whole LDC
      DO I=EFDnumSg,1,-1
         x=0.0
         DO J=1,EFDsgDnB(I)
            K=K+1
            EFDblWidth(K)=EFDsgWidth(I)*EFDsgDblock(I,J)/100.0
            EFDblSeg(K)=I
            EFDblSlc(K)=J
            x=x+EFDblWidth(K)
            EFDblockx(I,J)=x
         ENDDO
      ENDDO

!     Find for each segment first and last hour (in new sort order)

      sgn=EFDnumSg
      EFDsgFh(sgn)=1
      DO i=1,nhour
         IF(EFDsgNum(i).NE.sgn) THEN
            EFDsgLh(sgn)=i-1
            sgn=sgn-1
            EFDsgFh(sgn)=i
         ENDIF
      ENDDO
      EFDsgLh(sgn)=nhour
      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN

!     Definition of seasons

      EFD_Slice_ID = 0

      READ(FILE,*)EFDnS
      WRITE(IMSG,*)'EFDNS in ULDSM ',EFDnS
      DO I=1,EFDnS
         CALL DSMSKP(FILE,WHOOPS)
         IF(WHOOPS) RETURN
         READ(FILE,*)n,n1,n2,UNGSSN(I)
         tmp(n,1)=n1          !first grp in season n
         tmp(n,2)=n2          !last grp in season n
!
!        EMMDB Season assignment
         EFDSEAN(I) = EFDSEGDSC(n1)

         ULNVCT(I) = 0
         J = 0
         DO EFD_GRP = n1 , n2

            WRITE(IMSG,1714) CURIRUN, CURIYR+1989, CURITR, I, EFD_GRP, n, n1, n2, UNGSSN(I)
 1714       FORMAT(1X,"EFD_LDSMSTR",9(":",I6))

            MAP_EFD_SP(EFD_GRP) = I
            ULNVCT(I) = ULNVCT(I) + EFDsgDnB(EFD_GRP)
            DO EFD_SEG = 1 , EFDsgDnB(EFD_GRP)
               J = J + 1
               EFD_Slice_ID(EFD_GRP,EFD_SEG) = J
            END DO
            WRITE(IMSG,9345) CURIRUN, CURIYR+1989, I, EFD_GRP, EFDsgDnB(EFD_GRP), ULNVCT(I)
 9345       FORMAT(1X,"ULNVCT",6(":",I4))

!
!           EMMDB Season assignment
            EFDLGSI(EFD_GRP) = I

         END DO

!        Develop season names

         SEASONEFD(I)='--------------'
         m2=(14-DnM(n1)*3)/2
         DO m=1,DnM(n1)
            m1=m2+1
            m2=m1+2
            SEASONEFD(I)(m1:m2)=MonNam(m,n1)(1:3)
         ENDDO
         IF(n.LE.MAXEFDS) THEN
            b1=0
            DO s=EFDnumSg,n2+1,-1
               b1=b1+EFDsgDnB(s)
            ENDDO
            EFDSEDEF(n,1)=b1+1
            b1=0
            DO s=EFDnumSg,n1,-1
               b1=b1+EFDsgDnB(s)
            ENDDO
            EFDSEDEF(n,2)=b1
         ELSE
            WRITE(IMSG,*)'<))) Invalid EFD seasone number'
            WHOOPS=.TRUE.
            RETURN
         ENDIF

! Do season assignments for emm databasep
         EFDSEAD(I) = SEASONEFD(I)

      ENDDO
!
!     Fill 864hr-mapping variable (HrToEFDseas) with season number for each of 864 hrs
!
      Do m=1,12
         MONTYP(m) = MAP_EFD_SP(MONGRP(m))
         Do d=1,3
            Do h=1,24
               EFD_GRP = HrToEFDgrp(m,d,h)
               do i=1,EFDnS
                  if (EFD_GRP.ge.tmp(i,1) .and. EFD_GRP.le.tmp(i,2)) then
                     HrToEFDseas(m,d,h) = i
                  endif
               enddo
            enddo
         enddo
      enddo

!
!     Read defintion of LDC for ETT module
!     Read number of segments

      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN

      SumSegWidth=0.0
      READ(FILE,*)ETTnumSg
      write(imsg,*)'ETTnumSg=',ETTnumSg

!     Read definition of blocks. All hours are allocated to a number of
!     segments defined by calendar months, day-types and hourly intervals.
!     Within each of segment, blocks are defined by percentages of hours which
!     they cover. Each block can be either 'peak' or 'non-peak'. Peak blocks
!     are represented by the peak load in the block. Non-peak blocks are
!     represented by the average load for the block.
!        DnM stores number of months
!        DnD - number of day-types
!        DnH - number of hourly intervals
!        ECIsgDnB(MAXECPSG) - number of blocks in a segment
!        ECIsgDmonth(MAXMON) - month numbers
!        Dday(MAXDTP) - day-type numbers
!        Dhour(MAXINTV,1) and Dhour(MAXINTV,2) - lower and upper bounds of hour intervals
!        ECIsgDblock(MAXECPSG,MAXECPB) - widths of blocks defined as percent of total hours in a segment

      DO I=1,ETTnumSg  ! do for all segments
         CALL DSMSKP(FILE,WHOOPS)
         IF(WHOOPS) RETURN

!        Read segment definition

         READ(FILE,*)nn,ETTSEGDSC(nn),DnM(nn),(MonNam(J,nn),J=1,DnM(nn)), &
            DnD,(DtpNam(J),J=1,DnD), &
            DnH,((Dhour(J,K),K=1,2),J=1,DnH), &
            ETTsgDnB(I),(ETTsgDblock(I,J),ETTsgDbltyp(I,J),J=1,ETTsgDnB(I))
         write(imsg,'(I2,":",A,":",I2,<DnM(nn)>(":",A),":",I2,<DnD>(":",A),I2,<DnH*2>(":",I4),":",I2,<ETTsgDnB(I)>(":",F6.0,":",A))') &
                     nn,ETTSEGDSC(nn),DnM(nn),(MonNam(J,nn),J=1,DnM(nn)),  &
            DnD,(DtpNam(J),J=1,DnD),  &
            DnH,((Dhour(J,K),K=1,2),J=1,DnH),  &
            ETTsgDnB(I),(ETTsgDblock(I,J),ETTsgDbltyp(I,J),J=1,ETTsgDnB(I))
         DO J=1,DnM(nn)

!           Translate names of months and daytypes into their index numbers

            DO K=1,NMONTH
               IF(DSMCMP(MonNam(J,nn),MONAME(K))) GOTO 72200
            ENDDO
            GOTO 996
72200       ETTsgDmonth(J)=K
         ENDDO
         DO J=1,DnD
            DO K=1,NODAYT
               IF(DSMCMP(DtpNam(J),DTNAME(K))) GOTO 72300
            ENDDO
            GOTO 995
72300       Dday(J)=K
         ENDDO
!
!        Fill 864hr-mapping variable (HrToETTgrp) with group number for each hour in this
!        (nn) line
!
         Do J=1,DnM(nn)              ! do for months assigned in current line month number has been assigned above  ETTsgDmonth(J)
            Do d=1,DnD               ! do for each daytype in currrent line daytype number has been assigned above  Dday(d)
               Do h=1,DnH            ! do for each set of hours in current line
                  Do hr=Dhour(h,1), Dhour(h,2)    ! do for each hour in set
                     HrToETTgrp(ETTsgDmonth(J),Dday(d),hr) = nn
                  Enddo     !hr
               Enddo        !hour set
            Enddo           !daytype
         Enddo              !month
!
!        For each calendar hour that is assigned to the current segment,
!        input to the ETTsgNum(MAXHOUR) table an index of the current segment, I

         t=0 ! counts number of real hours in each of the segments (total widths)
         DO J=1,DnM(nn) ! do for months assigned to current segment
            l=0 ! hour in a calendar year
            DO K=1,ETTsgDmonth(J)-1 !calculate number of last calendar hour
               l=l+NODAYS(K)*24     !in preceeding month
            ENDDO
            DO K=1,NODAYS(ETTsgDmonth(J)) !do for each day in a current month
               DO p=1,DnD !check if its day-type fits to the current block
                  IF(JDAYTP(K,ETTsgDmonth(J)).EQ.Dday(p)) THEN
                     DO n=1,DnH
                        DO h=Dhour(n,1),Dhour(n,2)
                           ETTsgNum(l+h)=I
                           t=t+WEIGHT(K,ETTsgDmonth(J))
                        ENDDO
                     ENDDO
                  ENDIF
               ENDDO
               l=l+24
            ENDDO
         ENDDO
         ETTsgWidth(I)=t
         SumSegWidth=SumSegWidth+t
      ENDDO
      IF(SumSegWidth.NE.8760 .AND. SumSegWidth.NE.8784) GOTO 994

! Now ETTsgNum stores, for each calendar hour, an index of an ETT LDC segment to which this hour belongs
! Sort calendar hours by segment

      CALL DSMQSI(ETTsgNum,HourNumberETT,nhour,MAXHOUR)
!
!     Calculate division points (x coordinates) that define the LDC blocks within each segment
!     (beginning of segment=0.0, in real hours)

      K=0 ! ordinal number for blocks in the whole LDC
      DO I=ETTnumSg,1,-1
         x=0.0
         DO J=1,ETTsgDnB(I)
            K=K+1
            ETTblWidth(K)=ETTsgWidth(I)*ETTsgDblock(I,J)/100.0
            ETTblSeg(K)=I
            ETTblSlc(K)=J
            x=x+ETTblWidth(K)
            ETTblockx(I,J)=x
         ENDDO
      ENDDO

!     Find for each segment first and last hour (in new sort order)

      sgn=ETTnumSg
      ETTsgFh(sgn)=1
      DO i=1,nhour
         IF(ETTsgNum(i).NE.sgn) THEN
            ETTsgLh(sgn)=i-1
            sgn=sgn-1
            ETTsgFh(sgn)=i
         ENDIF
      ENDDO
      ETTsgLh(sgn)=nhour
      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN

!     Definition of seasons for ETT

      READ(FILE,*)ETTnS
      DO I=1,ETTnS
         CALL DSMSKP(FILE,WHOOPS)
         IF(WHOOPS) RETURN
         READ(FILE,*)n,n1,n2,UNGSSN_ETT(I)
         tmp(n,1)=n1          !first grp in season n
         tmp(n,2)=n2          !last grp in season n

         DO ETT_GRP = n1 , n2
            MAP_ETT_SP(ETT_GRP) = I
         END DO

!        Develop season names

         SEASONETT(I)='--------------'
         m2=(38-DnM(n1)*3)/2
         DO m=1,DnM(n1)
            m1=m2+1
            m2=m1+2
            SEASONETT(I)(m1:m2)=MonNam(m,n1)(1:3)
         ENDDO
         IF(n.LE.MAXETTS) THEN
            b1=0
            DO s=ETTnumSg,n2+1,-1
               b1=b1+ETTsgDnB(s)
            ENDDO
            ETTSEDEF(n,1)=b1+1
            b1=0
            DO s=ETTnumSg,n1,-1
               b1=b1+ETTsgDnB(s)
            ENDDO
            ETTSEDEF(n,2)=b1
         ELSE
            WRITE(IMSG,*)'<))) Invalid ETT seasone number'
            WHOOPS=.TRUE.
            RETURN
         ENDIF
      ENDDO
!
      write(imsg,*)'ETT seasons: ',(ETTSEDEF(1,i),i=1,2),(ETTSEDEF(2,i),i=1,2)
!
!     Fill 864hr-mapping variable (HrToETTseas) with season number for each of 864 hrs
!
      Do m=1,12
         Do d=1,3
            Do h=1,24
               ETT_GRP = HrToETTgrp(m,d,h)
               do i=1,ETTnS
                  if (ETT_GRP.ge.tmp(i,1) .and. ETT_GRP.le.tmp(i,2)) then
                     HrToETTseas(m,d,h) = i
                  endif
               enddo
            enddo
         enddo
      enddo

!
!     Read defintion of LDC for ECI module
!     Read number of segments

      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN

      SumSegWidth=0.0
      READ(FILE,*)ECInumSg
      write(imsg,*)'ECInumSg=',ECInumSg

!     Read definition of blocks. All hours are allocated to a number of
!     segments defined by calendar months, day-types and hourly intervals.
!     Within each of segment, blocks are defined by percentages of hours which
!     they cover. Each block can be either 'peak' or 'non-peak'. Peak blocks
!     are represented by the peak load in the block. Non-peak blocks are
!     represented by the average load for the block.
!        DnM stores number of months
!        DnD - number of day-types
!        DnH - number of hourly intervals
!        ECIsgDnB(MAXECPSG) - number of blocks in a segment
!        ECIsgDmonth(MAXMON) - month numbers
!        Dday(MAXDTP) - day-type numbers
!        Dhour(MAXINTV,1) and Dhour(MAXINTV,2) - lower and upper bounds of hour intervals
!        ECIsgDblock(MAXECPSG,MAXECPB) - widths of blocks defined as percent of total hours in a segment

      DO I=1,ECInumSg  ! do for all segments
         CALL DSMSKP(FILE,WHOOPS)
         IF(WHOOPS) RETURN

!        Read segment definition
         READ(FILE,*)nn,ECISEGDSC(nn),DnM(nn),(MonNam(J,nn),J=1,DnM(nn)), &
            DnD,(DtpNam(J),J=1,DnD), &
            DnH,((Dhour(J,K),K=1,2),J=1,DnH), &
            ECIsgDnB(I),(ECIsgDblock(I,J),ECIsgDbltyp(I,J),J=1,ECIsgDnB(I))
         write(imsg,'(I2,":",A,":",I2,<DnM(nn)>(":",A),":",I2,<DnD>(":",A),   &
                  ":",I2,<DnH*2>(":",I4),":",I2,<ECIsgDnB(I)>(":",F6.0,":",A))') &
                     nn,ECISEGDSC(nn),DnM(nn),(MonNam(J,nn),J=1,DnM(nn)), &
            DnD,(DtpNam(J),J=1,DnD), &
            DnH,((Dhour(J,K),K=1,2),J=1,DnH), &
            ECIsgDnB(I),(ECIsgDblock(I,J),ECIsgDbltyp(I,J),J=1,ECIsgDnB(I))
         DO J=1,DnM(nn)

!           Translate names of months and daytypes into their index numbers

            DO K=1,NMONTH
               IF(DSMCMP(MonNam(J,nn),MONAME(K))) GOTO 22200
            ENDDO
            GOTO 996
22200       ECIsgDmonth(J)=K
         ENDDO
         DO J=1,DnD
            DO K=1,NODAYT
               IF(DSMCMP(DtpNam(J),DTNAME(K))) GOTO 22300
            ENDDO
            GOTO 995
22300       Dday(J)=K
         ENDDO
!
!        Fill 864hr-mapping variable (HrToECIgrp) with group number for each hour in this
!        (nn) line
!
         Do J=1,DnM(nn)              ! do for months assigned in current line month number has been assigned above  ECIsgDmonth(J)
            Do d=1,DnD               ! do for each daytype in currrent line daytype number has been assigned above  Dday(d)
               Do h=1,DnH            ! do for each set of hours in current line
                  Do hr=Dhour(h,1), Dhour(h,2)    ! do for each hour in set
                     HrToECIgrp(ECIsgDmonth(J),Dday(d),hr) = nn
                  Enddo     !hr
               Enddo        !hour set
            Enddo           !daytype
         Enddo              !month
!
!        For each calendar hour that is assigned to the current segment,
!        input to the ECIsgNum(MAXHOUR) table an index of the current segment, I

         t=0 ! counts number of real hours in each of the segments (total widths)
         DO J=1,DnM(nn) ! do for months assigned to current segment
            l=0 ! hour in a calendar year
            DO K=1,ECIsgDmonth(J)-1 !calculate number of last calendar hour
               l=l+NODAYS(K)*24     !in preceeding month
            ENDDO
            DO K=1,NODAYS(ECIsgDmonth(J)) !do for each day in a current month
               DO p=1,DnD !check if its day-type fits to the current block
                  IF(JDAYTP(K,ECIsgDmonth(J)).EQ.Dday(p)) THEN
                     DO n=1,DnH
                        DO h=Dhour(n,1),Dhour(n,2)
                           ECIsgNum(l+h)=I
                           t=t+WEIGHT(K,ECIsgDmonth(J))
                        ENDDO
                     ENDDO
                  ENDIF
               ENDDO
               l=l+24
            ENDDO
         ENDDO
         ECIsgWidth(I)=t
         SumSegWidth=SumSegWidth+t
      ENDDO
      IF(SumSegWidth.NE.8760 .AND. SumSegWidth.NE.8784) GOTO 994

!     Now, table ECIsgNum stores, for each calendar hour, an index of an ECI LDC
!     segment to which this hour belongs to.
!     Sort calendar hours by segment

      CALL DSMQSI(ECIsgNum,HourNumberECI,nhour,MAXHOUR)
!
!     Calculate division points (x coordinates) that define the LDC blocks within
!     each of segments (beginning of segment=0.0, in real hours)

      K=0 ! ordinal number for blocks in the whole LDC
      DO I=ECInumSg,1,-1
         x=0.0
         DO J=1,ECIsgDnB(I)
            K=K+1
            ECIblWidth(K)=ECIsgWidth(I)*ECIsgDblock(I,J)/100.0
            ECIblSeg(K)=I
            ECIblSlc(K)=J
            x=x+ECIblWidth(K)
            ECIblockx(I,J)=x
         ENDDO
      ENDDO

!     Find for each segment first and last hour (in new sort order)

      sgn=ECInumSg
      ECIsgFh(sgn)=1
      DO i=1,nhour
         IF(ECIsgNum(i).NE.sgn) THEN
            ECIsgLh(sgn)=i-1
            sgn=sgn-1
            ECIsgFh(sgn)=i
         ENDIF
      ENDDO
      ECIsgLh(sgn)=nhour
      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN

!     Definition of seasons

      READ(FILE,*)ECInS
      DO I=1,ECInS
         CALL DSMSKP(FILE,WHOOPS)
         IF(WHOOPS) RETURN
         READ(FILE,*)n,n1,n2,UNGSSN_ECI(I)
         tmp(n,1)=n1          !first grp in season n
         tmp(n,2)=n2          !last grp in season n

!        Create Mapping from Season and Slice to Group and Segment

         ECI_SLC = 0
         DO ECI_GRP = n1 , n2
            DO ECI_SEG = 1 , ECIsgDnB(ECI_GRP)
               ECI_SLC = ECI_SLC + 1
               MAP_ECI_GRP(ECI_SLC,I) = ECI_GRP
               MAP_ECI_SEG(ECI_SLC,I) = ECI_SEG
               WRITE(IMSG,3791) I,ECI_SLC,ECI_GRP,ECI_SEG
 3791          FORMAT(1X,"MAP_ECI",4(":",I4))
            END DO
         END DO

!        Develop season names

         SEASONECI(I)='--------------'
         m2=(38-DnM(n1)*3)/2
         DO m=1,DnM(n1)
            m1=m2+1
            m2=m1+2
            SEASONECI(I)(m1:m2)=MonNam(m,n1)(1:3)
         ENDDO
         IF(n.LE.MAXECIS) THEN
            b1=0
            DO s=ECInumSg,n2+1,-1
               b1=b1+ECIsgDnB(s)
            ENDDO
            ECISEDEF(n,1)=b1+1
            b1=0
            DO s=ECInumSg,n1,-1
               b1=b1+ECIsgDnB(s)
            ENDDO
            ECISEDEF(n,2)=b1
         ELSE
            WRITE(IMSG,*)'<))) Invalid ECI seasone number'
            WHOOPS=.TRUE.
            RETURN
         ENDIF
      ENDDO
!
      write(imsg,*)'ECI seasons: ',(ECISEDEF(1,i),i=1,2),(ECISEDEF(2,i),i=1,2)
!
!     Fill 864hr-mapping variable (HrToECIseas) with season number for each of 864 hrs
!
      Do m=1,12
         Do d=1,3
            Do h=1,24
               ECI_GRP = HrToECIgrp(m,d,h)
               do i=1,ECInS
                  if (ECI_GRP.ge.tmp(i,1) .and. ECI_GRP.le.tmp(i,2)) then
                     HrToECIseas(m,d,h) = i
                  endif
               enddo
            enddo
         enddo
      enddo

!
!     Fill 8760hr-mapping variables HrsInEttEcpGrps, HrsInEcpEfdGrps, HrsInEttEfdGrps with
!     number of hrs in both groups (crosstab)
!                        dimension 0 is for totals
      HrsInEcpEfdGrps = 0
      HrsInEttEcpGrps = 0
      HrsInEttEfdGrps = 0
      HrsInECIEcpGrps = 0
      HrsInECIEfdGrps = 0
      HrsInEttEcpSPs = 0
      HrsInEttEfdSPs = 0
      HrsInECIEcpSegs = 0
      HrsInECIEfdSegs = 0
      Do m=1,12
        Do d=1,3
          Do h=1,24
            ETT_GRP = HrToETTgrp(m,d,h)
            ECP_GRP = HrToECPgrp(m,d,h)
            EFD_GRP = HrToEFDgrp(m,d,h)
            ECI_GRP = HrToECIgrp(m,d,h)
            ETT_SP = MAP_ETT_SP(ETT_GRP)
            ECP_SP = MAP_ECP_SP(ECP_GRP)
            EFD_SP = MAP_EFD_SP(EFD_GRP)
!
            HrsInEcpEfdGrps(ECP_GRP,EFD_GRP) = HrsInEcpEfdGrps(ECP_GRP,EFD_GRP) + IDAYTQ(d,m)
            HrsInEcpEfdGrps(0,EFD_GRP) = HrsInEcpEfdGrps(0,EFD_GRP) + IDAYTQ(d,m)
            HrsInEcpEfdGrps(ECP_GRP,0) = HrsInEcpEfdGrps(ECP_GRP,0) + IDAYTQ(d,m)
            HrsInEcpEfdGrps(0,0) = HrsInEcpEfdGrps(0,0) + IDAYTQ(d,m)
!
            HrsInEttEcpGrps(ETT_GRP,ECP_GRP) = HrsInEttEcpGrps(ETT_GRP,ECP_GRP) + IDAYTQ(d,m)
            HrsInEttEcpGrps(ETT_GRP,0) = HrsInEttEcpGrps(ETT_GRP,0) + IDAYTQ(d,m)
            HrsInEttEcpGrps(0,ECP_GRP) = HrsInEttEcpGrps(0,ECP_GRP) + IDAYTQ(d,m)
            HrsInEttEcpGrps(0,0) = HrsInEttEcpGrps(0,0) + IDAYTQ(d,m)
!
            HrsInEttEcpSPs(ETT_SP,ECP_SP) = HrsInEttEcpSPs(ETT_SP,ECP_SP) + IDAYTQ(d,m)
            HrsInEttEcpSPs(ETT_SP,0) = HrsInEttEcpSPs(ETT_SP,0) + IDAYTQ(d,m)
            HrsInEttEcpSPs(0,ECP_SP) = HrsInEttEcpSPs(0,ECP_SP) + IDAYTQ(d,m)
            HrsInEttEcpSPs(0,0) = HrsInEttEcpSPs(0,0) + IDAYTQ(d,m)
!
            HrsInEttEfdGrps(ETT_GRP,EFD_GRP) = HrsInEttEfdGrps(ETT_GRP,EFD_GRP) + IDAYTQ(d,m)
            HrsInEttEfdGrps(ETT_GRP,0) = HrsInEttEfdGrps(ETT_GRP,0) + IDAYTQ(d,m)
            HrsInEttEfdGrps(0,EFD_GRP) = HrsInEttEfdGrps(0,EFD_GRP) + IDAYTQ(d,m)
            HrsInEttEfdGrps(0,0) = HrsInEttEfdGrps(0,0) + IDAYTQ(d,m)
!
            HrsInEttEfdSPs(ETT_SP,EFD_SP) = HrsInEttEfdSPs(ETT_SP,EFD_SP) + IDAYTQ(d,m)
            HrsInEttEfdSPs(ETT_SP,0) = HrsInEttEfdSPs(ETT_SP,0) + IDAYTQ(d,m)
            HrsInEttEfdSPs(0,EFD_SP) = HrsInEttEfdSPs(0,EFD_SP) + IDAYTQ(d,m)
            HrsInEttEfdSPs(0,0) = HrsInEttEfdSPs(0,0) + IDAYTQ(d,m)
!
            HrsInECIEcpGrps(ECI_GRP,ECP_GRP) = HrsInECIEcpGrps(ECI_GRP,ECP_GRP) + IDAYTQ(d,m)
            HrsInECIEcpGrps(ECI_GRP,0) = HrsInECIEcpGrps(ECI_GRP,0) + IDAYTQ(d,m)
            HrsInECIEcpGrps(0,ECP_GRP) = HrsInECIEcpGrps(0,ECP_GRP) + IDAYTQ(d,m)
            HrsInECIEcpGrps(0,0) = HrsInECIEcpGrps(0,0) + IDAYTQ(d,m)
!
            HrsInECIEfdGrps(ECI_GRP,EFD_GRP) = HrsInECIEfdGrps(ECI_GRP,EFD_GRP) + IDAYTQ(d,m)
            HrsInECIEfdGrps(ECI_GRP,0) = HrsInECIEfdGrps(ECI_GRP,0) + IDAYTQ(d,m)
            HrsInECIEfdGrps(0,EFD_GRP) = HrsInECIEfdGrps(0,EFD_GRP) + IDAYTQ(d,m)
            HrsInECIEfdGrps(0,0) = HrsInECIEfdGrps(0,0) + IDAYTQ(d,m)
!
            DO J = 1 , ECIsgDnB(ECI_GRP)
               HrsInECIEcpSegs(ECI_GRP,J,ECP_GRP,0) = HrsInECIEcpSegs(ECI_GRP,J,ECP_GRP,0) + IDAYTQ(d,m) * ECIsgDblock(ECI_GRP,J) / 100.0
               HrsInECIEcpSegs(ECI_GRP,0,ECP_GRP,0) = HrsInECIEcpSegs(ECI_GRP,0,ECP_GRP,0) + IDAYTQ(d,m) * ECIsgDblock(ECI_GRP,J) / 100.0
               HrsInECIEcpSegs(ECI_GRP,J,0,0) = HrsInECIEcpSegs(ECI_GRP,J,0,0) + IDAYTQ(d,m) * ECIsgDblock(ECI_GRP,J) / 100.0
!
               HrsInECIEfdSegs(ECI_GRP,J,EFD_GRP,0) = HrsInECIEfdSegs(ECI_GRP,J,EFD_GRP,0) + IDAYTQ(d,m) * ECIsgDblock(ECI_GRP,J) / 100.0
               HrsInECIEfdSegs(ECI_GRP,0,EFD_GRP,0) = HrsInECIEfdSegs(ECI_GRP,0,EFD_GRP,0) + IDAYTQ(d,m) * ECIsgDblock(ECI_GRP,J) / 100.0
               HrsInECIEfdSegs(ECI_GRP,J,0,0) = HrsInECIEfdSegs(ECI_GRP,J,0,0) + IDAYTQ(d,m) * ECIsgDblock(ECI_GRP,J) / 100.0
            END DO
!
            DO J = 1 , ECPsgDnB(ECP_GRP)
               HrsInECIEcpSegs(0,0,ECP_GRP,J) = HrsInECIEcpSegs(0,0,ECP_GRP,J) + IDAYTQ(d,m) * ECPsgDblock(ECP_GRP,J) / 100.0
            END DO
!
            DO J = 1 , EFDsgDnB(EFD_GRP)
               HrsInECIEfdSegs(0,0,EFD_GRP,J) = HrsInECIEfdSegs(0,0,EFD_GRP,J) + IDAYTQ(d,m) * EFDsgDblock(EFD_GRP,J) / 100.0
            END DO
!
          enddo
        enddo
      enddo
!
!     Allocate ECI segment hours to ECP segments
!
      DO ECP_GRP = 1 , ECPnumSg
         DO ECI_GRP = 1 , ECInumSg
            IF (HrsInECIEcpSegs(ECI_GRP,0,ECP_GRP,0) .GT. 0.0) THEN
               TMP_ECP = 0.0
               TMP_ECI = 0.0
               DO ECP_SEG = 1 , ECPsgDnB(ECP_GRP)
                  TMP_ECP(ECP_SEG) = HrsInECIEcpSegs(ECI_GRP,0,ECP_GRP,0) * ECPsgDblock(ECP_GRP,ECP_SEG) / 100.0
                  HrsInECIEcpSegs(ECI_GRP,0,ECP_GRP,ECP_SEG) = HrsInECIEcpSegs(ECI_GRP,0,ECP_GRP,0) * ECPsgDblock(ECP_GRP,ECP_SEG) / 100.0
               END DO
               DO ECI_SEG = 1 , ECIsgDnB(ECI_GRP)
                  TMP_ECI(ECI_SEG) = HrsInECIEcpSegs(ECI_GRP,ECI_SEG,ECP_GRP,0)
               END DO
               TMP_HRS = HrsInECIEcpSegs(ECI_GRP,0,ECP_GRP,0)
               ECI_SEG = 1
               ECP_SEG = 1
               DO WHILE ( (TMP_HRS .GT. 0.0001) .AND. ( ECI_SEG .LE. MAXECIB) )
                  IF (TMP_ECI(ECI_SEG) .LT. TMP_ECP(ECP_SEG)) THEN
                     HrsInECIEcpSegs(ECI_GRP,ECI_SEG,ECP_GRP,ECP_SEG) = HrsInECIEcpSegs(ECI_GRP,ECI_SEG,ECP_GRP,ECP_SEG) + TMP_ECI(ECI_SEG)
                     TMP_ECP(ECP_SEG) = TMP_ECP(ECP_SEG) - TMP_ECI(ECI_SEG)
                     TMP_HRS = TMP_HRS  - TMP_ECI(ECI_SEG)
                     TMP_ECI(ECI_SEG) = 0.0
                     ECI_SEG = ECI_SEG + 1
                  ELSEIF (TMP_ECI(ECI_SEG) .GT. TMP_ECP(ECP_SEG)) THEN
                     HrsInECIEcpSegs(ECI_GRP,ECI_SEG,ECP_GRP,ECP_SEG) = HrsInECIEcpSegs(ECI_GRP,ECI_SEG,ECP_GRP,ECP_SEG) + TMP_ECP(ECP_SEG)
                     TMP_ECI(ECI_SEG) = TMP_ECI(ECI_SEG) - TMP_ECP(ECP_SEG)
                     TMP_HRS = TMP_HRS  - TMP_ECP(ECP_SEG)
                     TMP_ECP(ECP_SEG) = 0.0
                     ECP_SEG = ECP_SEG + 1
                  ELSE
                     HrsInECIEcpSegs(ECI_GRP,ECI_SEG,ECP_GRP,ECP_SEG) = HrsInECIEcpSegs(ECI_GRP,ECI_SEG,ECP_GRP,ECP_SEG) + TMP_ECI(ECI_SEG)
                     TMP_HRS = TMP_HRS  - TMP_ECI(ECI_SEG)
                     TMP_ECP(ECP_SEG) = 0.0
                     TMP_ECI(ECI_SEG) = 0.0
                     ECI_SEG = ECI_SEG + 1
                     ECP_SEG = ECP_SEG + 1
                  END IF
               END DO
            END IF
         END DO
      END DO

      DO ECP_GRP = 1 , ECPnumSg
         DO ECP_SEG = 1 , ECPsgDnB(ECP_GRP)
            WRITE(IMSG,9351) ECP_GRP,ECP_SEG,0,0,HrsInECIEcpSegs(0,0,ECP_GRP,ECP_SEG)
         END DO
      END DO
      DO ECI_GRP = 1 , ECInumSg
         DO ECI_SEG = 1 , ECIsgDnB(ECI_GRP)
            WRITE(IMSG,9351) 0,0,ECI_GRP,ECI_SEG,HrsInECIEcpSegs(ECI_GRP,ECI_SEG,0,0)
         END DO
      END DO
      DO ECP_GRP = 1 , ECPnumSg
         DO ECI_GRP = 1 , ECInumSg
            IF (HrsInECIEcpSegs(ECI_GRP,0,ECP_GRP,0) .GT. 0.0) THEN
               DO ECP_SEG = 0 , ECPsgDnB(ECP_GRP)
                  DO ECI_SEG = 0 , ECIsgDnB(ECI_GRP)
                     WRITE(IMSG,9351) ECP_GRP,ECP_SEG,ECI_GRP,ECI_SEG,HrsInECIEcpSegs(ECI_GRP,ECI_SEG,ECP_GRP,ECP_SEG)
 9351                FORMAT(1X,"HrsInECIEcpSegs",4(":",I2),":",F12.6)
                  END DO
               END DO
            END IF
         END DO
      END DO
!
!     Allocate ECI segment hours to EFD segments
!
      DO EFD_GRP = 1 , EFDnumSg
         DO ECI_GRP = 1 , ECInumSg
            IF (HrsInECIEfdSegs(ECI_GRP,0,EFD_GRP,0) .GT. 0.0) THEN
               TMP_EFD = 0.0
               TMP_ECI = 0.0
               DO EFD_SEG = 1 , EFDsgDnB(EFD_GRP)
                  TMP_EFD(EFD_SEG) = HrsInECIEfdSegs(ECI_GRP,0,EFD_GRP,0) * EFDsgDblock(EFD_GRP,EFD_SEG) / 100.0
                  HrsInECIEfdSegs(ECI_GRP,0,EFD_GRP,EFD_SEG) = HrsInECIEfdSegs(ECI_GRP,0,EFD_GRP,0) * EFDsgDblock(EFD_GRP,EFD_SEG) / 100.0
               END DO
               DO ECI_SEG = 1 , ECIsgDnB(ECI_GRP)
                  TMP_ECI(ECI_SEG) = HrsInECIEfdSegs(ECI_GRP,ECI_SEG,EFD_GRP,0)
               END DO
               TMP_HRS = HrsInECIEfdSegs(ECI_GRP,0,EFD_GRP,0)
               ECI_SEG = 1
               EFD_SEG = 1
               DO WHILE ( (TMP_HRS .GT. 0.0001) .AND. ( ECI_SEG .LE. MAXECIB) )
                  IF (TMP_ECI(ECI_SEG) .LT. TMP_EFD(EFD_SEG)) THEN
                     HrsInECIEfdSegs(ECI_GRP,ECI_SEG,EFD_GRP,EFD_SEG) = HrsInECIEfdSegs(ECI_GRP,ECI_SEG,EFD_GRP,EFD_SEG) + TMP_ECI(ECI_SEG)
                     TMP_EFD(EFD_SEG) = TMP_EFD(EFD_SEG) - TMP_ECI(ECI_SEG)
                     TMP_HRS = TMP_HRS  - TMP_ECI(ECI_SEG)
                     TMP_ECI(ECI_SEG) = 0.0
                     ECI_SEG = ECI_SEG + 1
                  ELSEIF (TMP_ECI(ECI_SEG) .GT. TMP_EFD(EFD_SEG)) THEN
                     HrsInECIEfdSegs(ECI_GRP,ECI_SEG,EFD_GRP,EFD_SEG) = HrsInECIEfdSegs(ECI_GRP,ECI_SEG,EFD_GRP,EFD_SEG) + TMP_EFD(EFD_SEG)
                     TMP_ECI(ECI_SEG) = TMP_ECI(ECI_SEG) - TMP_EFD(EFD_SEG)
                     TMP_HRS = TMP_HRS  - TMP_EFD(EFD_SEG)
                     TMP_EFD(EFD_SEG) = 0.0
                     EFD_SEG = EFD_SEG + 1
                  ELSE
                     HrsInECIEfdSegs(ECI_GRP,ECI_SEG,EFD_GRP,EFD_SEG) = HrsInECIEfdSegs(ECI_GRP,ECI_SEG,EFD_GRP,EFD_SEG) + TMP_ECI(ECI_SEG)
                     TMP_HRS = TMP_HRS  - TMP_ECI(ECI_SEG)
                     TMP_EFD(EFD_SEG) = 0.0
                     TMP_ECI(ECI_SEG) = 0.0
                     ECI_SEG = ECI_SEG + 1
                     EFD_SEG = EFD_SEG + 1
                  END IF
               END DO
            END IF
         END DO
      END DO

      DO EFD_GRP = 1 , EFDnumSg
         DO EFD_SEG = 1 , EFDsgDnB(EFD_GRP)
            WRITE(IMSG,8351) EFD_GRP,EFD_SEG,0,0,HrsInECIEfdSegs(0,0,EFD_GRP,EFD_SEG)
         END DO
      END DO
      DO ECI_GRP = 1 , ECInumSg
         DO ECI_SEG = 1 , ECIsgDnB(ECI_GRP)
            WRITE(IMSG,8351) 0,0,ECI_GRP,ECI_SEG,HrsInECIEfdSegs(ECI_GRP,ECI_SEG,0,0)
         END DO
      END DO
      DO EFD_GRP = 1 , EFDnumSg
         DO ECI_GRP = 1 , ECInumSg
            IF (HrsInECIEfdSegs(ECI_GRP,0,EFD_GRP,0) .GT. 0.0) THEN
               DO EFD_SEG = 0 , EFDsgDnB(EFD_GRP)
                  DO ECI_SEG = 0 , ECIsgDnB(ECI_GRP)
                     WRITE(IMSG,8351) EFD_GRP,EFD_SEG,ECI_GRP,ECI_SEG,HrsInECIEfdSegs(ECI_GRP,ECI_SEG,EFD_GRP,EFD_SEG)
 8351                FORMAT(1X,"HrsInECIEfdSegs",4(":",I2),":",F12.6)
                  END DO
               END DO
            END IF
         END DO
      END DO
!
! Read NERC regions names, their lsr names and their t&d loss factors
! and find adequate for the LSR's record numbers on DAF-LSR-DB


      !CALL DSMSKP(FILE,WHOOPS)
      !IF(WHOOPS) RETURN

!     IF (IJUMPEMRGN.gt.0) THEN
!       nNERCreg = IJUMPEMRGN
!     ELSE
!       nNERCreg = MNUMNR-3
!     ENDIF
      UNRGNS = nNERCreg        ! assigned to value read from EMM_DB.zip, table V_EMM_LDSMSTR_DIM

            WRITE(IMSG,2391) nNERCreg,UNRGNS,IJUMPEMRGN
 2391       FORMAT(1X,"nNERCreg= ",I3,"UNRGNS=",I3,"IJUMPEMRGN=",I3)
!
      WRITE(IMSG,*)'nNERCreg to read ldsmstr ',nNERCreg

      DO I=1,nNERCreg
         write(IMSG,*) 'Prob1c', (I),NERCnam(I),LSRnamSys(I),UQTDLS(I)
         
            NERCtdloss(I)=UQTDLS(I)
      Enddo
 
! Read number of sectors to be analyzed by the model

!***************commne out*******************
! Read definitions of each sector in order they appear
      DO I=1,NumSec
        FlagEU=.TRUE.
        FlagECM=.TRUE.
! Read sector name and number of end-uses in a sector
         write(imsg,*)SLNAM(I),NEUSES(I),NEUSGRP(I)
        DO J=1,NumSec
          IF (SLNAM(I)(1:3).EQ.SecNam(J)) THEN
            SEC(J) = I
            GOTO 30
          ENDIF
        ENDDO
        GOTO 999 ! if unrecognized sector write error message and stop
30      CONTINUE
         DO J=1,NEUSES(I)
! For each end-use read its name
!           CALL DSMSKP(FILE,WHOOPS)
!           IF(WHOOPS) RETURN
        
         CHECK = .TRUE.
         
         ! ONLY WHEN THE INDUSTRIAL IS ON AND H2 IS ON, FLIP CHECK TO FALSE
         IF ( I .EQ. SEC(IND) .AND. J .EQ. 4) THEN ! I = 3 IS INDUSTRIAL AND J = 4 IS H2
            CHECK = .FALSE.
         END IF
         
         Neu=Neu+1
         
         IF (CHECK) THEN 
              IF(FlagEU) THEN
                EUINDEX(I,1)=Neu
                FlagEU=.FALSE.
              ENDIF
    !          READ(FILE,*)EUNAM(Neu),EUGRP(Neu)
                    

! Read LSR name associated with an end-use in each region and the base year load
!
          DO K=1,nNERCreg
              LSRnam2 = LSRnam_sq(K,I,J)  
              x = x_sq(K,I,J)
                 
            BaseYrLd(Neu,K)=x*UNCONFA ! convert units
!      write(imsg,*)'baseyrld,neu,rgn,lsr,x,unconfa',
!    +     BaseYrLd(Neu,K),Neu,K,LSRnam,x,UNCONFA
            IF (K.gt.nNERCreg) GOTO 110
		    DO l=1,DBnr 
               IF(LSRnam2.EQ.LSReuname_s(l)) THEN
                EUrecNUM(Neu,K)=l
                GOTO 110
               ENDIF
            ENDDO
			GOTO 997 ! no match for LSR write message and stop the program
110         CONTINUE
          ENDDO
        ENDIF
        ENDDO       

        IF(EUINDEX(I,1).GT.0) EUINDEX(I,2)=Neu
      ENDDO

!   sort the groups
      EUGRP(neu+1) = 0  ! marker for end of uses
        CALL DSMSORT(EUGRP,EUGRPNDX,MAXEU)
!   read efd load uncertainty
        CALL DSMSKP(FILE,WHOOPS)
        IF(WHOOPS) RETURN
        READ(FILE,*)loadu
        loadu=loadu/100.

!   read ecp load uncertainty
        CALL DSMSKP(FILE,WHOOPS)
        IF(WHOOPS) RETURN
        READ(FILE,*)loadecpu
        loadecpu=loadecpu/100.
 !   read net cone percentage and weights for blending duals with levelized costs of turbine
        CALL DSMSKP(FILE,WHOOPS)
        IF(WHOOPS) RETURN
        READ(FILE,*) NETCON,CPWT1,CPWT2,CPWT3,CPWT4
        WRITE(18,*) 'NETCON and WEIGHTS',NETCON,CPWT1,CPWT2,CPWT3,CPWT4

!   read market entry factor for ecp
        !CALL DSMSKP(FILE,WHOOPS)
        !IF(WHOOPS) RETURN
        !READ(FILE,*)(MEFAC(j),j=1,nNERCreg),MEFAC(MNUMNR-2),MEFAC(MNUMNR-1),MEFAC(MNUMNR)

!     write(IMSG,*)'group',(EUGRPNDX(J),J=1,neu)
!     stop
!      do  j=1,neu
!     write(IMSG,*)'group, index',EUGRP(J),EUGRPNDX(J)
!     enddo
!     stop

! Read matrices used for mapping between CENSUS divisions and NERC regions and
! vice versa.
      DO J=1,Numsec
        DO l=1,nNERCreg
          write(IMSG,1520) 'MappCtoN ', l,J,(MappCtoN(l,K,J),K=1,MNUMCR)
1520    Format(A10,1x,2(I4,1x),11(F12.3,1x))          
        ENDDO !for l
! express the mapping values as fractions of the total for a region/census div.
        DO l=1,nNERCreg
          DO K=1,nCENSUSreg
!           WRITE(IMSG,2792) CURIRUN, CURIYR+1989, CURITR, l, k, j, SEC(j), MappCtoN(l,K,J), MappCtoN(l,MNUMCR,J)
!2792       FORMAT(1X,"MappNtoC",7(":",I4),2(":",F21.6))
            IF (MappCtoN(l,MNUMCR,J) .GT. 0.0) THEN
               MappNtoC(l,K,J) = MappCtoN(l,K,J) / MappCtoN(l,MNUMCR,J)
 !              MappNtoCRestart(l,K,J) = MappCtoN(l,K,J) / MappCtoN(l,MNUMCR,J)
            ELSE
               MappNtoC(l,K,J) = 0.0
 !              MappNtoCRestart(l,K,J) = 0.0
            ENDIF
          ENDDO
        ENDDO
        DO l=1,nNERCreg
          DO K=1,nCENSUSreg
            IF(MappCtoN(MNUMNR,K,J).LE.0.0) GOTO 896
            MappCtoN(l,K,J) = MappCtoN(l,K,J) / MappCtoN(MNUMNR,K,J)
!           WRITEIMSG,2793) CURIRUN, CURIYR+1989, CURITR, l, k, j, SEC(j), MappCtoN(l,K,J)
2793       FORMAT(1X,"MappCtoN",7(":",I4),2(":",F21.6))
          ENDDO
        ENDDO
      ENDDO
 

!!LC2 - sections further down related to DSM still seem to be using the DAF. I don't think any of this is relevant in the current
!!  model, but I would just leave this alone for now - so don't reuse DAFnr or DbLSRname variables.  

! Read Payback-acceptance curves
      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN
! Read number of rows of input data and the number of sectoral curves
      READ(FILE,*)PACnR,PACnS
!     write(imsg,*)'from dsmrst PACnR,PACnS',PACnR,PACnS
! Read length of payback period and associated market penetrations
! Data are assumed to be in ascending order of the length of the payback period
      DO I=1,PACnR
        CALL DSMSKP(FILE,WHOOPS)
        IF(WHOOPS) RETURN
        READ(FILE,*)PACpb(I),(PACmp(I,J),J=1,PACnS) !((Years)   Residential    Commercial   Industrial)
!       write(imsg,*)PACpb(l),(PACmp(l,J),J=1,PACnS)
      ENDDO
! Read DSM option database
! Read Pay-back period
      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN
      READ(FILE,*)PAYBACK
      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN
! Read Residential DSM Programs Specification
      READ(FILE,*)NRPROG  !(Number of Residential DSM Programs)
      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN
      DO I=1,NRPROG
        CALL DSMSKP(FILE,WHOOPS)
        IF(WHOOPS) RETURN
        READ(FILE,*)v,RprogCode(I)   !(** # PROGRAM CODE   PROGRAM SPECIFICATION)
      ENDDO
      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN
! Read Commercial DSM Programs Specification
      READ(FILE,*)NCPROG
      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN
      DO I=1,NCPROG
        CALL DSMSKP(FILE,WHOOPS)
        IF(WHOOPS) RETURN
        READ(FILE,*)v,CprogCode(I) ! (** # PROGRAM CODE   PROGRAM SPECIFICATION)
      ENDDO
      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN
! READ RESIDENTIAL TECHNLOLOGIES MAP
      READ(FILE,*)RtechNumb
      DO I=1,RtechNumb
        CALL DSMSKP(FILE,WHOOPS)
        IF(WHOOPS) RETURN
        READ(FILE,*)v,RtechCode(I),RtechDMtn(I), &
          (RtechDMG(I,J),J=1,RtechDMtn(I)), &
          (RtechDMT(I,J),J=1,RtechDMtn(i)),LSRnam        ! (# Technology   Number of  Technology   Technology    LSR Code)
        DO l=2,DAFnr
          IF(LSRnam.EQ.DbLSRname(l)) THEN
            RtechLSR(I)=l
            GOTO 1110
          ENDIF
        ENDDO
        GOTO 997 ! no match for LSR write message and stop the program
1110    CONTINUE
      ENDDO
! READ REGION GROUP DEFINITION
      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN
      READ(FILE,*)RRlistN
      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN
      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN
! READ DECISION TYPE DESCRIPTORS MAP
      READ(FILE,*)RdecTYPn
      DO I=1,RdecTYPn
        CALL DSMSKP(FILE,WHOOPS)
        IF(WHOOPS) RETURN
        READ(FILE,*)RdecTYPid(I),RdecTYPix(I)
      ENDDO
      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN
! READ BUILDING TYPE DESCRIPTORS MAP
      READ(FILE,*)RbuildTn   !( Number of types)
      DO I=1,RbuildTn
        CALL DSMSKP(FILE,WHOOPS)
        IF(WHOOPS) RETURN
        READ(FILE,*)RbuildTid(I),RbuildTix(I)
      ENDDO
! READ OPTIONS DATA BASE
      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN
      READ(FILE,*)DSMROptionNumb
      DO I=1,DSMROptionNumb
        CALL DSMSKP(FILE,WHOOPS)
        IF(WHOOPS) RETURN
        READ(FILE,*)DSMROptionCode(I),ProgNam,RegLd,DUMMYchar, &
        DecTd,BuilTd,DSMROptionFromTnum(I), &
        (FtechLSR(J),J=1,DSMROptionFromTnum(I)),DSMROptionToTnum(I), &
        (TtechLSR(J),J=1,DSMROptionToTnum(I)),DSMROptionCost(I), &
        DSMROptionRamp(I),DSMROptionFyr(I)
        
        DO J=1,NRPROG
          IF(ProgNam.EQ.RprogCode(J)) THEN
            RPROG(I)=J
            GOTO 3333
          ENDIF
        ENDDO
        GOTO 992
3333    CONTINUE
        DO J=1,RRlistN
          IF(RegLd.EQ.RRlistID(J)) THEN
            DSMROptionRegion(I)=J
            GOTO 3334
          ENDIF
        ENDDO
        GOTO 991
3334    CONTINUE
        DO J=1,RdecTYPn
          IF(DecTd.EQ.RdecTYPid(J)) THEN
            DSMROptionDecType(I)=RdecTYPix(J)
            GOTO 3335
          ENDIF
        ENDDO
        GOTO 990
3335    CONTINUE
        DO J=1,RbuildTn
          IF(BuilTd.EQ.RbuildTid(J)) THEN
            DSMROptionBuildT(I)=RbuildTix(J)
            GOTO 3336
          ENDIF
        ENDDO
        GOTO 889
3336    CONTINUE
        DO J=1,DSMROptionFromTnum(I)
          DO l=1,RtechNumb
            IF(FtechLSR(J).EQ.RtechCode(l)) THEN
              DSMROptionFromTech(I,J)=l
              GOTO 3337
            ENDIF
          ENDDO
          GOTO 888
3337      CONTINUE
        ENDDO
        DO J=1,DSMROptionToTnum(I)
          DO l=1,RtechNumb
            IF(TtechLSR(J).EQ.RtechCode(l)) THEN
              DSMROptionToTech(I,J)=l
              GOTO 3338
            ENDIF
          ENDDO
          GOTO 777
3338      CONTINUE
        ENDDO
      ENDDO
      
!!!!DKG PATCH HERE _SKIP THIS PART TILL I CAN FIGURE OUT WHAT TO DO WITH NEW REGIONALITY
!
! DEVELOP MAP OF RECORDS NUMBERS ON RESTART  FILE FOR DIFFERENT TECHNOLOGIES
! IN DIFFERENT REGIONS AND BUILDING TYPES
!  Read from RESIDENTIAL RESTART FILE technology identifiers
!      READ(IORR,REC=1)RRFnr
!     write(imsg,*)'RRFnr',RRFnr
!      DO l=2,RRFnr
!        READ(IORR,REC=l)TECHIDENT(l)
!!       write(imsg,*)'TECHIDENT',TECHIDENT(l)
!      ENDDO
!      DO I=1,RtechNumb
!        DO J=1,RtechDMtn(I)
!          DO K=1,nNERCreg
!            DO L=1,RbuildTn
!              IDENTIF=10000000*K+100000*L+1000*RtechDMG(I,J)+10*RtechDMT(I,J)
!!             if(K.eq.7.and.I.eq.20) write(imsg,*)'IDENTIF',IDENTIF
!              DO m=2,RRFnr
!                IF(IDENTIF.EQ.TECHIDENT(m)) THEN
!                  RtechRrc(I,J,K,L)=m
!                  GOTO 3355
!                ENDIF
!              ENDDO
!              WRITE(IMSG,*)'<)) Message from subroutine DSMRST'
!              WRITE(IMSG,*)'<))) No record with identifier: ',IDENTIF,'on RESTART file'
!              RtechRrc(I,J,K,L)=0
!3355          CONTINUE
!            ENDDO
!          ENDDO
!        ENDDO
!      ENDDO

      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN
! Read COMMERCIAL DSM OPTITONS DATA BASE ***********************
! READ COMMERCIAL TECHNLOLOGIES LSR MAP
      READ(FILE,*)CtechNumb   !(** Number of technologies)
      DO I=1,CtechNumb
        CALL DSMSKP(FILE,WHOOPS)
        IF(WHOOPS) RETURN
        READ(FILE,*)v,(CtechCode(I,J),J=1,NUMCTCE),(LSRnames(J),J=1,MAXCBT)
        DO J=1,MAXCBT
          DO l=2,DAFnr
            IF(LSRnames(J).EQ.DbLSRname(l)) THEN
              CtechLSR(I,J)=l
              GOTO 2111
            ENDIF
          ENDDO
          GOTO 1997 ! no match for LSR write message and stop the program
2111      CONTINUE
        ENDDO
      ENDDO
! READ REGION GROUP DEFINITION
      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN
      READ(FILE,*)CRlistN
      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN
      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN
! READ DECISION TYPE DESCRIPTORS MAP
      READ(FILE,*)CdecTYPn       !(** Number of decision types)
      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN
      DO I=1,CdecTYPn
        CALL DSMSKP(FILE,WHOOPS)
        IF(WHOOPS) RETURN
        READ(FILE,*)CdecTYPid(I),CdecTYPix(I)  !(** Descriptor   Decision type )
      ENDDO
      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN
! READ BUILDING TYPE DESCRIPTORS MAP
      READ(FILE,*)CbuildTn      !( ** Define building type descriptors)
      DO I=1,CbuildTn
        CALL DSMSKP(FILE,WHOOPS)
        IF(WHOOPS) RETURN
        READ(FILE,*)CbuildTid(I),CbuildTix(I)
!       write(imsg,*)'I,CbuildTid(I),CbuildTix(I)'
!       write(imsg,*)I,' ',CbuildTid(I),CbuildTix(I)
      ENDDO
! READ OPTIONS DATA BASE
      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN
      READ(FILE,*)DSMCOptionNumb  !(* Number of COMMERCIAL DSM options)
      DO I=1,DSMCOptionNumb
        CALL DSMSKP(FILE,WHOOPS)
        IF(WHOOPS) RETURN
        READ(FILE,*)DSMCOptionCode(I),ProgNam,RegLd,DUMMYchar, &
        DecTd,BuilTd,DSMCOptionFromTnum(I), &
        ((FCTC(K,J),K=1,NUMCTCE),J=1,DSMCOptionFromTnum(I)), &
        DSMCOptionToTnum(I), &
        ((TCTC(K,J),K=1,NUMCTCE),J=1,DSMCOptionToTnum(I)), &
        DSMCOptionCost(I), &
        DSMCOptionRamp(I),DSMCOptionFyr(I)
        IF (FCTC(3,1) .EQ. 11 .OR. TCTC(3,1) .EQ. 12) GOTO 666 !for heat pumps
        IF (FCTC(3,1) .EQ. 3 .AND. FCTC(3,2) .NE. 11) GOTO 666 !heating function
        IF (FCTC(3,1) .EQ. 4 .AND. FCTC(3,2) .NE. 12) GOTO 666 !must go first, then
        IF (TCTC(3,1) .EQ. 3 .AND. TCTC(3,2) .NE. 11) GOTO 666 !it has to be followed
        IF (TCTC(3,1) .EQ. 4 .AND. TCTC(3,2) .NE. 12) GOTO 666 !by correct cooling function
        IF (FCTC(3,1) .EQ. 3  .OR. FCTC(3,1) .EQ. 4 .OR. &
            TCTC(3,1) .EQ. 3  .OR. TCTC(3,1) .EQ. 4) THEN      ! if heat pump option
          IF (DSMCOptionFromTnum(I).NE.DSMCOptionToTnum(I)) GOTO 666
          DO J=1,DSMCOptionFromTnum(I)
            IF (FCTC(1,J).NE.TCTC(1,J)) GOTO 666 ! TO and FROM technologies has to be
          ENDDO                                  ! listed in the same order of services
        ENDIF
        DO J=1,NCPROG
          IF(ProgNam.EQ.CprogCode(J)) THEN
            CPROG(I)=J
            GOTO 7332
          ENDIF
        ENDDO
        GOTO 992
7332    CONTINUE
        DO J=1,CRlistN
          IF(RegLd.EQ.CRlistID(J)) THEN
            DSMCOptionRegion(I)=J
            GOTO 7334
          ENDIF
        ENDDO
        GOTO 991
7334    CONTINUE
        DO J=1,CdecTYPn
          IF(DecTd.EQ.RdecTYPid(J)) THEN
            DSMCOptionDecType(I)=CdecTYPix(J)
            GOTO 7335
          ENDIF
        ENDDO
        GOTO 990
7335    CONTINUE
        DO J=1,CbuildTn
          IF(BuilTd.EQ.CbuildTid(J)) THEN
!         write(imsg,*)'I,J,BuilTd,CbuildTid(J),CbuildTix(J)'
!         write(imsg,*)I,J,BuilTd,' ',CbuildTid(J),CbuildTix(J)
            DSMCOptionBuildT(I)=CbuildTix(J)
            GOTO 7336
          ENDIF
        ENDDO
        GOTO 889
7336    CONTINUE
        DO J=1,DSMCOptionFromTnum(I)
          DO l=1,CtechNumb
            S=0
            DO K=1,NUMCTCE
              IF(FCTC(K,J).EQ.CtechCode(l,K)) S=S+1
            ENDDO
            IF (S.EQ.NUMCTCE) THEN
              DSMCOptionFromTech(I,J)=l
              GOTO 7337
            ENDIF
          ENDDO
          GOTO 899
7337      CONTINUE
        ENDDO
        DO J=1,DSMCOptionToTnum(I)
          DO l=1,CtechNumb
            S=0
            DO K=1,NUMCTCE
              IF(TCTC(K,J).EQ.CtechCode(l,K)) S=S+1
            ENDDO
            IF (S.EQ.NUMCTCE) THEN
              DSMCOptionToTech(I,J)=l
              GOTO 7338
            ENDIF
          ENDDO
          GOTO 1777
7338      CONTINUE
        ENDDO
      ENDDO
!
!!!!DKG PATCH SKIP THIS FOR NOW
!
! DEVELOP MAP OF RECORDS NUMBERS ON RESTART  FILE FOR DIFFERENT TECHNOLOGIES
! IN DIFFERENT REGIONS AND BUILDING TYPES
!  Read from COMMERCIAL RESTART FILE technology identifiers
!      READ(IOCR,REC=1)CRFnr
!!     WRITE(IMSG,*)'CRFnr',CRFnr
!      DO l=2,CRFnr
!        READ(IOCR,REC=l)TECHIDENT(l)
!!       write(imsg,*)'TECHIDENT(l)',TECHIDENT(l)
!      ENDDO
!      DO I=1,CtechNumb
!          DO K=1,nNERCreg
!            DO L=1,CbuildTn
!              IDENTIF=10000000*K+100000*L+1000*CtechCode(I,1)+10*CtechCode(I,3)+CtechCode(I,2)
!!             write(imsg,*)'IDENTIF',IDENTIF
!              DO m=2,CRFnr
!                IF(IDENTIF.EQ.TECHIDENT(m)) THEN
!                  CtechRrc(I,K,L)=m
!!                 write(imsg,*)'CtechRrc(I,K,L),I,K,L',CtechRrc(I,K,L),I,K,L
!                  GOTO 7355
!                ENDIF
!              ENDDO
!!             WRITE(IMSG,*)'<)) Message from subroutine DSMRST'
!!             WRITE(IMSG,*)'<))) No record with identifier: ',IDENTIF,'on RESTART file'
!              CtechRrc(I,K,L)=0
!7355          CONTINUE
!            ENDDO
!          ENDDO
!      ENDDO
      CALL DSMSKP(FILE,WHOOPS)
      IF(WHOOPS) RETURN
      READ(FILE,*)SURCHARGE
      ! DEFINE REPORTS TO BE GENERATED
      ! READ SPECIFICATION OF THE REPORTS TO BE GENERATED
      DO I=1,QDSMRPT
        CALL DSMSKP(FILE,WHOOPS)
        READ(FILE,*)DecTd      ! (*                  LOAD PORTION)
        IF(DecTd.EQ.'Y'.OR.DecTd.EQ.'y') THEN
          NODSMRPT(I)=.TRUE.
        ELSE
          NODSMRPT(I)=.FALSE.
        ENDIF
      ENDDO
      ! READ EMM REGIONS THAT REPORTS ARE TO BE GENERATED FOR
      CALL DSMSKP(FILE,WHOOPS)
      READ(FILE,*)NREPREG,(LREG(I),I=1,NREPREG)  !(*** # OF REGIONS REGIONS)
      DO I=1,NREPREG
        DO J=1,nNERCreg
          IF(LREG(I).EQ.NERCnam(J)) NOREGRPT(J)=.TRUE.
        ENDDO
      ENDDO
      ! READ YEARS THAT REPORTS ARE TO BE GENERATED FOR "VERTICAL YEARS"
      CALL DSMSKP(FILE,WHOOPS)
      READ(FILE,*)NREPYRV,(IREPYRV(I),I=1,NREPYRV)   !( *** # OF YEARS   YEARS...)
      DO I=1,NREPYRV
        IREPYRV(I)=IREPYRV(I)-BASEYR+1
        NOYEARRPT(IREPYRV(I))=.TRUE.
      ENDDO
      ! READ YEARS THAT REPORTS ARE TO BE GENERATED FOR "HORIZONTAL YEARS"
      CALL DSMSKP(FILE,WHOOPS)
      READ(FILE,*)NREPYRH,(IREPYRH(I),I=1,NREPYRH)
      FILE=FILE_MGR('C',fname,NEW)

      WRITE(6,*)'nNERCreg to run ULDSM ',nNERCreg, UNRGNS

!       write(imsg,*)'RESIDENTIAL DSM OPTIONS'
!       do i=1,DSMROptionNumb
!         write(imsg,*)i,' ',DSMROptionCode(i)
!         ,(' ',RtechCode(DSMROptionFromTech(i,j)),
!         j=1,DSMROptionFromTnum(i))
!         ,(' ',RtechCode(DSMROptionToTech(i,j)),
!         j=1,DSMROptionToTnum(i))
!       enddo
!****************** Termination of the Program/Subprogram **********************
      RETURN
999   WRITE(IMSG,*)'<)) Message from subroutine DSMRST'
      WRITE(IMSG,*)'<))) Illegal sector name ',SLNAM(I),' on LDSMSTR file'
      WHOOPS=.TRUE.
      RETURN
998   WRITE(IMSG,*)'<))  Message from subroutine DSMRST'
      WRITE(IMSG,*)'<))) Illegal sector name ',SLNM,' on LDSMSTR file'
      WHOOPS=.TRUE.
      RETURN
997   WRITE(IMSG,*)'<)) Message from subroutine DSMRST'
      WRITE(IMSG,*)'<))) Illegal lsr name ',LSRnam,' on LDSMSTRU file'
      WHOOPS=.TRUE.
      RETURN
1997  WRITE(IMSG,*)'<)) Message from subroutine DSMRST'
      WRITE(IMSG,*)'<))) Illegal lsr name ',LSRnames(J),' on LDSMSTRU'
      WHOOPS=.TRUE.
      RETURN
996   WRITE(IMSG,*)'<)) Message from subroutine DSMRST'
      WRITE(IMSG,*)'<))) Illegal month name ',MonNam(J,n),' on LDSMSTR file'
      WHOOPS=.TRUE.
      RETURN
995   WRITE(IMSG,*)'<)) Message from subroutine DSMRST'
      WRITE(IMSG,*)'<))) Illegal day-type name ',DtpNam(J),' on LDSMSTR file'
      WHOOPS=.TRUE.
      RETURN
994   WRITE(IMSG,*)'<)) Message from subroutine DSMRST'
      WRITE(IMSG,*)'<))) Illegal sum of widths of segments',SumSegWidth, &
      ' declared for ECP LDC on LDSMSTR file'
      WHOOPS=.TRUE.
      RETURN
993   WRITE(IMSG,*)'<)) Message from subroutine DSMRST'
      WRITE(IMSG,*)'<))) Illegal region name ',LREG(K), &
      ' on region lists used for DSM options defin. on LDSMSTR file'
      WHOOPS=.TRUE.
      RETURN
992   WRITE(IMSG,*)'<)) Message from subroutine DSMRST'
      WRITE(IMSG,*)'<))) Illegal DSM program  name ',ProgNam, &
      ' in DSM option definition on LDSMSTR file'
      WHOOPS=.TRUE.
      RETURN
991   WRITE(IMSG,*)'<)) Message from subroutine DSMRST'
      WRITE(IMSG,*)'<))) Illegal DSM region list descriptor ',RegLd, &
      ' in DSM option definition on LDSMSTR file'
      WHOOPS=.TRUE.
      RETURN
990   WRITE(IMSG,*)'<)) Message from subroutine DSMRST'
      WRITE(IMSG,*)'<))) Illegal DSM decision type descriptor ',DecTd, &
      ' in DSM option definition on LDSMSTR file'
      WHOOPS=.TRUE.
      RETURN
889   WRITE(IMSG,*)'<)) Message from subroutine DSMRST'
      WRITE(IMSG,*)'<))) Illegal building type descriptor ',BuilTd, &
      ' in DSM option definition on LDSMSTR file'
      WHOOPS=.TRUE.
      RETURN
888   WRITE(IMSG,*)'<)) Message from subroutine DSMRST'
      WRITE(IMSG,*)'<))) Illegal FROM technology descrip.',FtechLSR(J), &
      ' in DSM option definition on LDSMSTR file'
      WHOOPS=.TRUE.
      RETURN
899   WRITE(IMSG,*)'<)) Message from subroutine DSMRST'
      WRITE(IMSG,*)'<))) Illegal FROM technology descrip.', &
      (FCTC(K,J),K=1,NUMCTCE), &
      ' in DSM option definition on LDSMSTR file'
      WHOOPS=.TRUE.
      RETURN
777   WRITE(IMSG,*)'<)) Message from subroutine DSMRST'
      WRITE(IMSG,*)'<))) Illegal TO technology descriptor ',TtechLSR(J), &
      ' in DSM option definition on LDSMSTR file'
      WHOOPS=.TRUE.
      RETURN
1777  WRITE(IMSG,*)'<)) Message from subroutine DSMRST'
      WRITE(IMSG,*)'<))) Illegal TO technology descriptor ',(TCTC(K,J), &
      K=1,NUMCTCE),' in DSM option definition on LDSMSTR file'
      WHOOPS=.TRUE.
      RETURN
666   WRITE(IMSG,*)'<)) Message from subroutine DSMRST'
      WRITE(IMSG,*)'<))) In DSM option definition ', &
       DSMCOptionCode(I),' heat pump technology is wrongly defined'
      WHOOPS=.TRUE.
      RETURN
897   WRITE(IMSG,*)'<)) Message from subroutine DSMRST'
      WRITE(IMSG,*)'<))) 0.0 total load for NERC region #',l &
      ,' in mapping matrix for sector ',SecNam(SEC(J))
      WHOOPS=.TRUE.
      RETURN
896   WRITE(IMSG,*)'<)) Message from subroutine DSMRST'
      WRITE(IMSG,*)'<))) 0.0 total load for CENSUS region #',K &
      ,' in mapping matrix for sector ',SecNam(SEC(J))
      WHOOPS=.TRUE.
      RETURN
      END

       SUBROUTINE DSMSKP(file,WOOPS)

!****************** Description of the Program/Subprogram **********************
! THIS SUBROUTINE READS A DATA LINE AND SKIPS THE COMMENTS TO POSITION IT AT THE NEXT LINE OF REAL DATA
!*******************************************************************************
       IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
!**********Declaring LDSM variables and common blocks via NEMS include files********
       include 'dsmunits' !<< include file with unit number for ldsm message fil
!********************** Declaring local variables *****************************
       INTEGER*4 file   ! Name of the file
       CHARACTER*160 line ! Current character
       CHARACTER*1 STAR ! Character variable representing a comment symbol
       LOGICAL WOOPS ! error flag
!****************** Initializing Variables *************************************
       DATA STAR/'*'/
       LINE=STAR
!****************** Body of the Program/Subprogram *****************************
       DO WHILE(LINE(1:1).EQ.STAR)
         READ(file,'(A)',END=20)line
       ENDDO
       BACKSPACE file
!****************** Termination of the Program/Subprogram **********************
       RETURN
   20  WRITE(IMSG,*)'<))) Unexpected EOF - reading file: ',file
       WRITE(IMSG,*)'<))) Program terminated by subroutine DSMSKP'
       WOOPS=.TRUE.
       RETURN
       END

      SUBROUTINE DSMSRP

!****************** Description of the Program/Subprogram **********************
! THIS SUBROUTINE WRITES OUT THE SALES REPORT
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr'
      include 'ncntrl' !<< acces to nems global variables like
      include 'emmparm'
      include 'control'
      include 'udatout' !<< electricity sales variables
      include 'qblk'
      include 'dispett'
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen' !<< all ldsm parameter declarations
      include 'dsmunits'
      include 'dsmhelm' !<< helm algorithm variables
      include 'dsmsectr' !<< sector secific data and other associated variables
      include 'dsmtfefp' !<< communication with efp
      include 'dsmnercr'
      include 'dsmtoefd'
      include 'dsmnemsc'
      include 'dsmtfecp'
      include 'dsmcaldr'
!********************** Declaring local variables *****************************
      INTEGER*2 MAXSEC1
      PARAMETER(MAXSEC1=MAXSEC+2)

      INTEGER*2 SECTOR ! sector number or 0 if not to process sectorial loads
      REAL*4    SALES(MNUMYR,MAXSEC1)
      REAL*4    SAL(5)
      INTEGER*2 I,J,K,C
      REAL*4 ENERGY,ENERGY1,DELTA1
!****************** Initializing Variables *************************************
      DO I=1,UNYEAR
        DO J=1,MAXSEC1
          SALES(I,J)=0.0
        ENDDO
      ENDDO
!****************** Body of the Program/Subprogram *****************************
      WRITE(IMSG,*)'DATA FOR THE NEMS REPORT WRITER'
      WRITE(IMSG,*)'ELECTRICITY SALES (NO T&D LOSSES) IN GWH'
      DO I=1,nNERCreg
        WRITE(IMSG,*)'REGION: ',NERCnam(I)
        WRITE(IMSG,*)'YEAR RESIDENTIAL  COMMERCIAL  INDUSTRIAL  TRANSPORT. TOT. SYSTEM TOT.SY.+T&D %ECPLDC'
        DO K=UESTYR-UHBSYR,LASTYR
          ENERGY=0.0
          DO J=1,ECPnumBl
            ENERGY=ENERGY+ECPLDCBH(K,I,J)*ECPLDCBW(K,I,J)*nhouryr
          ENDDO
          ENERGY1=QELASN(I,K)*(1.0+NERCtdloss(I)*ULOSSADJ(K))
          DELTA1=(ENERGY-ENERGY1)/ENERGY1*100.0

10        FORMAT(I4,6F12.3,F8.3)
          WRITE(IMSG,10)K+BASEYR-1,QELRSN(I,K),QELCMN(I,K),QELINN(I,K), &
                                   QELTRN(I,K),QELASN(I,K),ENERGY1,DELTA1

1077      FORMAT(1X,"ELEC_LOAD",5(":",I4),6(":",F12.3),3(":",F8.3))
          WRITE(18,1077) CURIRUN, CURIYR+1989, CURITR, K+BASEYR-1, I, QELRSN(I,K), QELCMN(I,K), QELINN(I,K), &
          QELTRN(I,K), QELASN(I,K), ENERGY1, DELTA1, NERCtdloss(I), ULOSSADJ(K)


          SALES(K,1)=SALES(K,1)+QELRSN(I,K)
          SALES(K,2)=SALES(K,2)+QELCMN(I,K)
          SALES(K,3)=SALES(K,3)+QELINN(I,K)
          SALES(K,4)=SALES(K,4)+QELTRN(I,K)
          SALES(K,5)=SALES(K,5)+QELASN(I,K)
          SALES(K,6)=SALES(K,6)+QELASN(I,K)*(1.0+NERCtdloss(I)*ULOSSADJ(K))
        ENDDO
      ENDDO
      WRITE(IMSG,*)'13 REGIONS TOTAL'
      WRITE(IMSG,*)'YEAR RESIDENTIAL  COMMERCIAL  INDUSTRIAL  TRANSPORT. TOT. SYSTEM TOT.SY.+T&D'
      DO K=UESTYR-UHBSYR,LASTYR
        WRITE(IMSG,10)K+BASEYR-1,(SALES(K,I),I=1,6)
      ENDDO
      WRITE(IMSG,*)
      WRITE(IMSG,*)'13 REGIONS TOTAL IN TRILLIONS BTU COMPARED TO'
      WRITE(IMSG,*)'9 CESNSUS DIV. FROM QELRS,QELCM,QELIN,QELTR,QELAS'
      WRITE(IMSG,*)'YEAR RESIDENTIAL  COMMERCIAL  INDUSTRIAL  TRANSPORT. TOT. SYSTEM'
      WRITE(IMSG,*)'YEAR QELRS        QELCM       QELIN        QELTR     QELAS'
      WRITE(IMSG,*)'YEAR %DELTA       %DELTA      %DELTA       %DELTA    %DELTA'
      DO K=UESTYR-UHBSYR,LASTYR
        DO J=1,5
          SAL(J)=0.0
        ENDDO
        DO C=1,nCENSUSreg
          SAL(1)=SAL(1)+QELRS(C,K)
          SAL(2)=SAL(2)+QELCM(C,K)
          SAL(3)=SAL(3)+QELIN(C,K)
          SAL(4)=SAL(4)+QELTR(C,K)
          SAL(5)=SAL(5)+QELAS(C,K)
        ENDDO
        WRITE(IMSG,10)K+BASEYR-1,((SALES(K,I)/UNCONFA),I=1,5)
        WRITE(IMSG,10)K+BASEYR-1,(SAL(I),I=1,5)
        WRITE(IMSG,10)K+BASEYR-1,(((SALES(K,I)/UNCONFA-SAL(I))/SAL(I)*100.0),I=1,5)
      ENDDO
!****************** Termination of the Program/Subprogram **********************
      RETURN
      END

      SUBROUTINE DSMTOR
      USE EPHRTS_SWTICHES
      USE EPHRTS_FILE_UNIT_NUMBERS 

!****************** Description of the Program/Subprogram **********************
! THIS SUBROUTINE FILLS VARIAZBLES REPRESENTING SECTORAL ELECTRICITY DEMAND BY EMM REGION
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr'
      include 'ncntrl' !<< acces to nems global variables like
      include 'emmparm'
      include 'control'
      include 'udatout' !<< electricity sales variables
      include 'dispinyr'
      include 'elcntl'
      include 'elout'
      include 'qblk'
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen' !<< all ldsm parameter declarations
      include 'dsmhelm' !<< helm algorithm variables
      include 'dsmsectr' !<< sector secific data and other associated variables
      include 'dsmtfefp' !<< communication with efp
      include 'dsmunits' !   unit for ldsmrpt dbg
      include 'dsmnercr' !<< nerc region data
      include 'dsmcaldr'    
!********************** Declaring local variables *****************************
      INTEGER*2 SECTOR ! sector number or 0 if not to process sectorial loads
      INTEGER I
      REAL CENSUSVAL_RS(MNUMCR),CENSUSVAL_CM(MNUMCR)
      REAL load
      LOGICAL E_DEBUG_EXIST

      IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN
         INQUIRE(FILE="EPHRTS_DEBUG_FILE.TXT", EXIST=E_DEBUG_EXIST)
         IF (E_DEBUG_EXIST) THEN
            OPEN(unit_num_ephrts_debug_file, FILE="EPHRTS_DEBUG_FILE.TXT", STATUS="OLD", POSITION="APPEND", ACTION="WRITE")
         ELSE
            OPEN(unit_num_ephrts_debug_file, FILE="EPHRTS_DEBUG_FILE.TXT", STATUS="NEW", ACTION="WRITE")
         END IF
      END IF
!****************** Initializing Variables *************************************
!****************** Body of the Program/Subprogram *****************************

!! need to assign RES/COM by mapping QELRS, QELCM because TotSecLoad contains end use DG
        DO I = 1,nCENSUSreg
           CensusVal_RS(I) = QELRS(I,CURIYR)
           CensusVal_CM(I) = QELCM(I,CURIYR)
        END DO
        CALL DSMEMMV(CensusVal_RS,RNB,SEC(RES),load)
        TotSecLoad(RNB,SEC(RES))=load * UNCONFA
        CALL DSMEMMV(CensusVal_CM,RNB,SEC(COM),load)
        TotSecLoad(RNB,SEC(COM))=load * UNCONFA 

        QELRSN(RNB,CURIYR)=TotSecLoad(RNB,SEC(RES))
        QELCMN(RNB,CURIYR)=TotSecLoad(RNB,SEC(COM))
        QELINN(RNB,CURIYR)=TotSecLoad(RNB,SEC(IND))
        QELTRN(RNB,CURIYR)=TotSecLoad(RNB,SEC(TRA))
        QELASN(RNB,CURIYR)=QELRSN(RNB,CURIYR) + QELCMN(RNB,CURIYR) &
                         + QELINN(RNB,CURIYR) + QELTRN(RNB,CURIYR) ! Don't include hydrogen electrolyzer load (Annual_H2_MWH_Consumed(RNB)* 3.41214/1000000) when only power sector hydrogen is used
          
        ULELRS(RNB) = TotSecLoad(RNB,SEC(RES))
        ULELCM(RNB) = TotSecLoad(RNB,SEC(COM))
        ULELIN(RNB) = TotSecLoad(RNB,SEC(IND))
        ULELTR(RNB) = TotSecLoad(RNB,SEC(TRA))
        IF (TURN_ON_DEBUGGING .EQ. .TRUE.) THEN
            CLOSE(unit_num_ephrts_debug_file)
         END IF
!****************** Termination of the Program/Subprogram **********************
      RETURN
      END

      FUNCTION DSMPRVF(INVESCST,HRYRS,LIFETIM)
!****************** Description of the Program/Subprogram **********************
! THIS FUNCTION CALCULATES THE FRACTION OF THE TOTAL INVESTMENT COSTS TO BE USED IN CONSIDERATIONS OF THE TIME HORIZON
!      the time horizon is HRYRS years
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr'
      include 'emmparm'
      include 'bildin'
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen'
      include 'dsmunits'
!********************** Declaring local variables *****************************
      REAL*4 DSMPRVF ! funcion itself
      REAL*4 INVESCST !total investment cost
      INTEGER*2 HRYRS ! time horizon of consideration
      REAL*4 LIFETIM ! equipment technical lifetime
      REAL*4 R1 ! temporary variable
!****************** Initializing Variables *************************************
!****************** Body of the Program/Subprogram *****************************
      R1=EPDSCRT+1.0
      DSMPRVF=INVESCST * (1.0-1.0/R1**HRYRS)/(1.0-1.0/R1**LIFETIM)
!     IF(INVESCST.NE.0)WRITE(IMSG,*)'DSMPRVF/INVESCST=',DSMPRVF/INVESCST
!****************** Termination of the Program/Subprogram **********************
      RETURN
      END

      SUBROUTINE DSMEFPS(BlockNumEFD)

!****************** Description of the Program/Subprogram **********************
!*** THIS SUBROUTINE DEVELOPS SECTORAL LOAD DURATION CURVES FOR EFP
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr' !<< nems parameter declarations
      include 'ncntrl'
      include 'emmparm'
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen' !<< all ldsm parameter declarations
      include 'dsmunits' !<< include file with unit number for ldsm message file
      include 'dsmcaldr' !<< calendar data
      include 'dsmhelm' !<< helm algorithm variables
      include 'dsmtfefp' !<< communication with efp
      include 'dsmtoefd' !<< communication with efd
      include 'dsmnemsc' !<< results of ldsm to be passed to the rest of nems
      include 'dsmnercr' !<< nerc region data
      include 'dsmsectr' !<< number of sectors
!********************** Declaring local variables *****************************
      REAL*8 SYLOAD2(MAXHOUR) !temporary variable
      REAL*4 area ! area of the block
      INTEGER*2 i,j,k,l,h,h1,h2,w,w1,b1,b2,b ! temporary variables
      INTEGER*4 SWITCH ! switch: SYSTEM LOAD or DSM program (is being processed)
      REAL*4 dh ! division point of an hour
      REAL*4 pl ! current peak load
      INTEGER*2 sgn ! segment number
      INTEGER*2 blk ! number of a block in the entire LDC
      INTEGER*2 bls ! number of a block in the segment
      INTEGER*2 hn  ! current hour number
      REAL*4 ovratio ! ratio for overestimation adjustment
      REAL*4 totarea ! total area of non-peak blocks
      REAL*4 BlockEnergy(MAXEFTB) ! Heights of blocks in EFD LDC
      INTEGER*2 SECT ! current sector number
      INTEGER*2 BlockNumEFD(MAXEFTB) ! Numbers of blocks
!****************** Initializing Variables *************************************
!****************** Body of the Program/Subprogram *****************************
!     write(imsg,*)'total load, sectoral loads'
!     do i=1,nhour
!       write(imsg,*)i,SYLOAD(i),(SectorLoad(i,SECT),SECT=1,NumSec)
!     enddo
      DO SECT=1,NumSec
        DO i=1,nhour
          SYLOAD2(i)=SectorLoad(HourNumberEFD(i),SECT)
        ENDDO
! SYLOAD2 now contains calendar hour loads sorted in the same order as in the system LDC
! HourNumberEFD contains original positions of calendar hours
! Determine heights of the LDC blocks
        blk=1 ! current block number within whole LDC
        bls=1 ! current block number within a segment
        area=0.0 ! current area under the curve from the end of the previous bl
! Now go over all hours in the year and calculate block widths and heights
        DO sgn=EFDnumSg,1,-1
          l=0             ! x coordinate expressed in real hours
          h1=EFDsgFh(sgn) ! beginning of current block in calendar hours
          h2=EFDsgLh(sgn) ! end of current block in calendar hours
          DO h=h1,h2      ! x coordinate expressed in calendar hours
            w1=HourlyWeights(HourNumberEFD(h))
            DO w=1,w1
              l=l+1 ! x coordinate in real hours
              area=area+SYLOAD2(h)
              IF(bls.NE.EFDsgDnB(sgn)) THEN !if this is not last block in segmen
                IF(l.GT.EFDblockx(sgn,bls)) THEN !if x coordinate beyond the blo
                  dh=l-EFDblockx(sgn,bls) ! calculate an x axis
                  BlockEnergy(blk)=area-dh*SYLOAD2(h)
                  area=dh*SYLOAD2(h)
                  bls=bls+1
                  blk=blk+1
                ENDIF
              ENDIF
            ENDDO
          ENDDO
          BlockEnergy(blk)=area ! for last block
          blk=blk+1
          bls=1
          area=0.0
        ENDDO
        DO j=1,EFDnS
          b1=EFDSEDEF(j,1)
          b2=EFDSEDEF(j,2)
          !Write the LDC into the communication common block arrays:
          DO i=b1,b2
            b=i-b1+1
            SECTLDCBLEN(b,j,RNB,SECT)=BlockEnergy(BlockNumEFD(i))
          ENDDO
        ENDDO
      ENDDO
!****************** Termination of the Program/Subprogram **********************
      RETURN
900   WRITE(*,*)'<))) ERROR IN DSMEFPS ROUTINE OF LDSM'
      WRITE(*,*)'<)))) DSM PROGRAM DATA MAY BE CORRUPTED'
      WRITE(IMSG,*)'<))) ERROR IN DSMEFPS ROUTINE OF LDSM'
      END

      SUBROUTINE DSMSECWT(HourSegNumEFD)   !(BLOCKNUMEFD)

!****************** Description of the Program/Subprogram **********************
!*** THIS SUBROUTINE DEVELOPS SECTORAL LOAD DURATION CURVES FOR EACH END USE REQUIRED FOR END USE PRICING
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr' !<< nems parameter declarations
      include 'ncntrl'
      include 'emmparm'
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen' !<< all ldsm parameter declarations
      include 'dsmunits' !<< include file with unit number for ldsm message file
      include 'dsmcaldr' !<< calendar data
      include 'dsmhelm' !<< helm algorithm variables
      include 'dsmtfefp' !<< communication with efp
      include 'dsmtoefd' !<< communication with efd
      include 'dsmnemsc' !<< results of ldsm to be passed to the rest of nems
      include 'dsmnercr' !<< nerc region data
      include 'dsmsectr' !<< number of sectors
!********************** Declaring local variables *****************************
      REAL*8 SYLOAD2(MAXHOUR) !temporary variable
      REAL*4 area ! area of the block
	  INTEGER*4 NUMREC ! record number
      CHARACTER*8 LSRNAME ! LSR name
      INTEGER*4 EUGRPLAST ! previous end-use group number
      REAL*4 DistLo(MAXHOUR),CALEN(MAXHOUR)! distribution of load over the hours
      INTEGER*2 i,j,k,l,h,h1,h2,w,w1,b1,b2,b,m,ll ! temporary variables
      INTEGER*4 SWITCH ! switch: SYSTEM LOAD or DSM program (is being processed)
      REAL*4 dh ! division point of an hour
      REAL*4 pl ! current peak load
      REAL*4 TOTBLK ! total energy in end-use group
      INTEGER*2 sgn ! segment number
      INTEGER*2 blk ! number of a block in the entire LDC
      INTEGER*2 bls ! number of a block in the segment
      INTEGER*2 hn  ! current hour number
      REAL*4 ovratio ! ratio for overestimation adjustment
      REAL*4 totarea ! total area of non-peak blocks
      REAL*4 BlockEnergy(MAXEFTB) ! Heights of blocks in EFD LDC
      REAL*4 EULOAD  !  end-use load (from LOADFOREC)
      INTEGER*2 SECT ! current sector number
      INTEGER*2 BlockNumEFD(MAXEFTB) ! Numbers of blocks
      INTEGER*2 HourSegNumEFD(nhour) ! Numbers of blocks from DSMEFD
      REAL*4 SYLOAD3(nhour)       !  put in segment order
	  
	 
	  
	  CHARACTER*8 LSReuname_s(MAXEU)
	  REAL*4 DistLo_eu_s(MAXEU,MAXHOUR) 
	  INTEGER*4 DBnr  ! DB number of records
	  COMMON /EUSYSLD/LSReuname_s,DistLo_eu_s, DBnr
	  !******
!****************** Initializing Variables *************************************
!****************** Body of the Program/Subprogram *****************************
!     write(imsg,*)'total load, sectoral loads'
!     do i=1,nhour
!       write(imsg,*)i,SYLOAD(i),(SectorLoad(i,SECT),SECT=1,NumSec)
!     enddo
!   Before 1992 commercial model turned off


        EUGRPLAST=EUGRP(1)
      TOTBLK= 0.0

!     Initialize end-use consumption by service group
      DO I=1,MAXEUGRP
         TOTEULOAD(I,RNB) = 0.0
      ENDDO
      do i=1,nhour
        SYLOAD2(i) = 0.0
        CALEN(i) = 0.0
      enddo
      DO LL=1,neu   ! loop through all end-uses (126 TOTAL WHERE H2 IS NEUSES(1)+NEUSES(2)+NEUSES(3))
          EULOAD = LoadForec(EUGRPNDX(LL),1)     ! end-use load by service
!  originally 1
!         write(IMSG,*)'EULOAD',EULOAD
          K = EUGRP(LL)    ! ordered list of end-uses
          if (ll .ne. NEUSES(1)+NEUSES(2)+NEUSES(3)) then ! skip hydrogen end-use here (124), read in directly from hmm model
          NUMREC=EUrecNUM(EUGRPNDX(LL),RNB)
		   DO m=1,nhour
		     DistLo(m) = DistLo_eu_s(NUMREC,m)
		   enddo
		   end if
		  !****************************************************
		
!         write(IMSG,*)'neu, name, group,dst',K,EUGRPNDX(LL),DISTLO(1)
! sort to segment level
         DO m=1,nhour
             ! if not hydrogen (end-use NEUSES(1)+NEUSES(2)+NEUSES(3)) from hmm, then skip and assign distlo normally, else add from hmm directly.
             if (ll .ne. NEUSES(1)+NEUSES(2)+NEUSES(3)) then
                SYLOAD3(m) = Distlo(m)  
             else
                SYLOAD3(m) = h2_elec_consumed(rnb,m)/1000.0  
             end if
         enddo
        IF (K .ne. EUGRPLAST ) then

! Determine heights of the LDC blocks
        blk=1 ! current block number within the whole LDC
        bls=1 ! current block number within a segment
        area=0.0 ! current area under the curve from the end of the previous bl
! Now go over all hours in the year and calculate block energy
        ! go backwards over the segments
        ! the last segment eFDnumSg is the highest load segment in the last
        ! season's LDC
        DO sgn=EFDnumSg,1,-1
          l=0 ! time coordinate expressed in real hours
          h1=EFDsgFh(sgn) ! beginning of segment expressed in calendar hours
          h2=EFDsgLh(sgn) ! end of segment expressed in calendar hours
          DO h=h1,h2 ! time coordinate expressed in calendar hours
            ! HourlyWeights(i) tells how many real hours are represented by
            ! the calendar hour i
            w1=HourlyWeights(HourNumberEFD(h))
            DO w=1,w1
              l=l+1 ! time coordinate in real hours
              area=area+SYLOAD2(h)
			  
              IF(bls.NE.EFDsgDnB(sgn)) THEN !if this is not last block in segmen
                IF(l.GT.EFDblockx(sgn,bls)) THEN !if time coordinate beyond
                  dh=l-EFDblockx(sgn,bls)
                  BlockEnergy(blk)=area-dh*SYLOAD2(h)
				  
                  area=dh*SYLOAD2(h) ! this portion belongs to the next block
				 
                  bls=bls+1
!  sum total energy for all ldc in group
                    TOTBLK = TOTBLK + BLockEnergy(blk)
					
                  blk=blk+1
                ENDIF
              ENDIF
            ENDDO
          ENDDO
          BlockEnergy(blk)=area ! for last block
            TOTBLK = TOTBLK + BLockEnergy(blk)
          blk=blk+1
          bls=1
          area=0.0
        ENDDO

        DO j=1,EFDnS ! do for each season
          b1=EFDSEDEF(j,1) ! first block in a season
          b2=EFDSEDEF(j,2) ! last block in a season
          !Write the LDC into the communication common block arrays:
          DO i=b1,b2
            b=i-b1+1
            ! b - block # in a seasonal LDC
            ! j - season
            ! RNB - EMM region
            ! SECT -  sector
            ! BlockNumEFD - a map of ordinal block numbers to block numbers
            ! in LDCs
            If (TOTBLK .ne. 0) then
             efdblkserv(b,j,RNB,eugrplast)=BlockEnergy(GBlockNumEFD(i,RNB))
              IF( CURITR .eq. 1) THEN
                efdblkservo(b,j,RNB,eugrplast) = efdblkserv(b,j,RNB,eugrplast)
              ENDIF
!             efdblkserv(b,j,RNB,eugrplast)=BlockEnergy(i) / TOTBLK
            endif
          ENDDO
        ENDDO
!  reset curve for group of end uses
        DO M=1,nhour
           SYLOAD2(M)=0.0
        ENDDO
! reset normalization factor
          TOTBLK = 0.0
!     IF(RNB.EQ.1.AND.CURIYR.EQ.4.AND.CURITR.EQ.1) THEN
!        write(IMSG,*)'LSRNAME   ',LSRname
!     WRITE(IMSG,*)'load2,DistLo(1..10),SYLOAD(1...10)'
!     ,load2,(DistLo(M),M=1,10),(SYLOAD(M),M=1,10)
!     ENDIF


        ENDIF   ! new end-use or not
!  Add to SYLOAD   every time. If it's a new one SYLOAD is already zeroed.

          DO i=1,nhour    ! do for each hour of 12*3*24=864 LDSM calendar hours
           ! copy sectorial loads to a temporary working variable
            ! HourNumberEFD(i) = maps 864 chronological hours to EFD's LDC hours,
            ! the map provides positions of hours in all 6 seasonal LDCs, starting
            ! with the last season and the highest load hour,
            ! e.g. HourNumberEFD(1) tells which of 1..864 chronological hours is
            ! the first hour in the LDC (the peak hour,in the last season's LDC)
            ! Multiply by total load for this end-use so we weight end-uses in
            ! a group correctly
              
            ! note: if end-use is not hydrogen then do the syload calculation normally, 
            !       else if hydrogen (NEUSES(1)+NEUSES(2)+NEUSES(3)) then add it directly from hmm  
            if (ll .ne. NEUSES(1)+NEUSES(2)+NEUSES(3)) then 
                SYLOAD2(i)=SYLOAD2(i)+SYLOAD3(HourNumberEFD(i)) * EULOAD
            else
                SYLOAD2(i)=SYLOAD2(i)+ SYLOAD3(HourNumberEFD(i))  ! No need to use EULOAD since we're not using the fractional/convertor method Distlo * EULOAD, instead we directly add load here hourly     
            end if
			! write(991,*) 'syload2',SYLOAD3(HourNumberEFD(i)),SYLOAD3_tst(HourNumberEFD(i)),HourNumberEFD(i)
!       if (CURIYR .eq. 3 .and. RNB .eq. 1 .and. LL .eq. 1) THEN
!          write(IMSG,*)'SYLOAD2,HOURSEGNUM',SYLOAD2(i),HourNumberEFD(I)
!       endif
           if ( k .eq. 21)  CALEN(I) = CALEN(I) +  DISTLO(I) * EULOAD
           ENDDO
           TOTEULOAD(K,RNB) = TOTEULOAD(K,RNB) + EULOAD
! SYLOAD2 now contains calendar hour loads sorted in the same order as in the
! system LDC
! HourNumberEFD contains original positions of calendar hours

        EUGRPLAST =  K
      ENDDO   ! K

! do for last unit should be subroutine!!!
! Determine heights of the LDC blocks
        blk=1 ! current block number within the whole LDC
        bls=1 ! current block number within a segment
        area=0.0 ! current area under the curve from the end of the previous bl
! Now go over all hours in the year and calculate block energy
        ! go backwards over the segments
        ! the last segment eFDnumSg is the highest load segment in the last
        ! season's LDC
        DO sgn=EFDnumSg,1,-1
          l=0 ! time coordinate expressed in real hours
          h1=EFDsgFh(sgn) ! beginning of segment expressed in calendar hours
                        h2=EFDsgLh(sgn) ! end of segment expressed in calendar hours
                        DO h=h1,h2 ! time coordinate expressed in calendar hours
            ! HourlyWeights(i) tells how many real hours are represented by
            ! the calendar hour i
            w1=HourlyWeights(HourNumberEFD(h))
            DO w=1,w1
              l=l+1 ! time coordinate in real hours
              area=area+SYLOAD2(h)
			 
			
              IF(bls.NE.EFDsgDnB(sgn)) THEN !if this is not last block in segmen
                IF(l.GT.EFDblockx(sgn,bls)) THEN !if time coordinate beyond
                  dh=l-EFDblockx(sgn,bls)
                  BlockEnergy(blk)=area-dh*SYLOAD2(h)
                  area=dh*SYLOAD2(h) ! this portion belongs to the next block
                  bls=bls+1
!  sum total energy for all ldc in group
                    TOTBLK = TOTBLK + BLockEnergy(blk)
                  blk=blk+1
                ENDIF
              ENDIF
            ENDDO
          ENDDO
          BlockEnergy(blk)=area ! for last block
          TOTBLK = TOTBLK + BLockEnergy(blk)
          blk=blk+1
          bls=1
          area=0.0
        ENDDO

        DO j=1,EFDnS ! do for each season
          b1=EFDSEDEF(j,1) ! first block in a season
          b2=EFDSEDEF(j,2) ! last block in a season
          !Write the LDC into the communication common block arrays:
          DO i=b1,b2
            b=i-b1+1
            ! b - block # in a seasonal LDC
            ! j - season
            ! RNB - EMM region
            ! SECT -  sector
            ! BlockNumEFD - a map of ordinal block numbers to block numbers
            ! in LDCs
            If (TOTBLK .ne. 0) then
        efdblkserv(b,j,RNB,eugrplast)=BlockEnergy(GBlockNumEFD(i,RNB))
              IF( CURITR .eq. 1) THEN
                efdblkservo(b,j,RNB,eugrplast) = efdblkserv(b,j,RNB,eugrplast)
              ENDIF
!             efdblkserv(b,j,RNB,eugrplast)=BlockEnergy(i) / TOTBLK
            endif
          ENDDO
        ENDDO
        if (CURIYR .eq. 3 .and. RNB .eq. 1) THEN
        endif
!****************** Termination of the Program/Subprogram **********************
      RETURN
900   WRITE(*,*)'<))) ERROR IN DSMSECWPS ROUTINE OF LDSM'
      WRITE(*,*)'<)))) DSM PROGRAM DATA MAY BE CORRUPTED'
      WRITE(IMSG,*)'<))) ERROR IN DSMEFPS ROUTINE OF LDSM'
      RETURN
      END


      SUBROUTINE DSMSORT(DEMKEY,DEMDEX,maxsize)

      IMPLICIT NONE
!
! THIS SUBROUTINE PERFORMS A HEAP SORT

      include 'dsmunits'
      INTEGER*2  maxsize
      INTEGER DEMKEY(maxsize),DEMDEX(maxsize)  ! names for historical
      INTEGER RECNO
      INTEGER I,J,IEND,MID, INDX, INDX1

!   Create indices 1 per "job"
          RECNO = 1
          DO WHILE (DEMKEY(RECNO) .GT. 0)
                 DEMDEX(RECNO) = RECNO
                 RECNO = RECNO + 1
          END DO

      IEND = RECNO - 1
      MID = RECNO /2 +1   ! integer divide

!          write(IMSG,'(1x,2I10)')(DEMKEY(J),DEMDEX(J),j=1,maxsize)
!    +     TCOLMSK(J),J=1,1000)
      DO WHILE (1 .GT. 0)
           IF (MID .GT. 1) THEN
                 MID  = MID - 1
             INDX = DEMKEY(MID)
         INDX1 = DEMDEX(MID)
           ELSE
                 INDX = DEMKEY(IEND)
         INDX1 = DEMDEX(IEND)
                 DEMKEY(IEND) = DEMKEY(1)
             DEMDEX(IEND) = DEMDEX(1)  ! re-arrange data
                 IEND = IEND - 1
                 IF (IEND .EQ. 1) THEN
                   DEMKEY(1) = INDX
               DEMDEX(1) = INDX1
                   GOTO 1101                 ! Need a break statement
             ENDIF  !   IEND = 1
           ENDIF ! inf loop

       I = MID
           J = MID + MID
       DO WHILE (J .LE. IEND)

                 IF (J .LT. IEND) THEN
                   IF (DEMKEY(J) .LT. DEMKEY(J+1)) J = J + 1
         ENDIF
                 IF (INDX .LT. DEMKEY(J))THEN
                   DEMKEY(I) = DEMKEY(J)
           DEMDEX(I) = DEMDEX(J)
                   I = J
               J = J * 2
             ELSE
                   J = IEND +1
                 ENDIF   ! INDX < DEMKEY
       END DO   ! J <= IEND
           DEMKEY(I) = INDX
           DEMDEX(I) = INDX1
      END DO        ! inf loop
 1101   RETURN
      END


      SUBROUTINE DSMSHFT(callflag)

!****************** Description of the Program/Subprogram **********************
!*** THIS SUBROUTINE IMPLEMENTS LOAD SHIFTING REQUIRED FOR END USE PRICING
!*******************************************************************************
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'eusprc'
      include 'efpint'
      include 'efpout'
      include 'efprp2'
      include 'efprcy'
      include 'macout'
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmdimen'
      include 'dsmsectr'
      include 'dsmtoefd'
      include 'dsmunits' !<< include file with unit number for ldsm message file
      include 'dsmhelm'
      include 'dsmnemsc'
      include 'dsmtfecp' !<< communication with ecp
      include 'dsmrept'
!********************** Declaring local variables *****************************
      INTEGER callflag,ireg, EFDSS(MAXEFDS)
      INTEGER ise,k,sblk,b,j,jj,i,icls,iblk,s,imom
      INTEGER*2 b1,b2
      REAL TOTEUSLD(MAXSEA),TOTEUSLD1(MAXSEA)
      REAL TOTSYSLOAD1(MAXEFDSS,MAXSEA)
      REAL TOTLD(MAXSEA),TOTLD1(MAXSEA)
      REAL TOTSYSLOAD(MAXEFDSS,MAXSEA)

      REAL*4 BlockHeightEFD(MAXEFTB) ! Heights of blocks in EFD LDC
      INTEGER*2 BlockNumEFD(MAXEFTB) ! Numbers of blocks
      REAL frac
      REAL EFDblWdth(MAXEFTB)
      REAL EFDblSg(MAXEFTB)
      REAL EFDblSc(MAXEFTB)
      REAL dcumo(MAXEFTB)
      REAL EPSHFT(ECP_D_VLS,2)
      REAL EPSHFTW(ECP_D_VLS,2)
      INTEGER USHFT(MAXEFTB,2)
      REAL TOTECPLD,TOTECPLD1
      INTEGER YEAR,IVLS,IYR,ivls2,seg,slice
      real tmppk(MAXSEA,mnumnr)
!****************** Initializing Variables *************************************
      DATA USHFT/9,9,9,8,8,8,7,7,7, &
                 6,6,6,5,5,5,4,4,4, &
                 3,3,3,2,2,2,1,1,1, &
                 9*1, &
                 6*1,        1,2,2, &
                 3*1,  1,2,2,3*1/


      DO ise = 1,  EFDNS
        do ireg=1,mnumnr
         tmppk(ise,ireg) = 0.0
        enddo
        TOTLD(ise)  = 0.0
        TOTLD1(ise) = 0.0
        DO IBLK = 1, ULNVCT(ise)
          TOTSYSLOAD(IBLK,ise) = 0.0
          TOTSYSLOAD1(IBLK,ise) = 0.0
        END DO
      ENDDO

! first iteration we use previous year's prices for load shifting
      IF(CURITR .eq. 1) THEN
        IYR = CURIYR -1
      ELSE
        IYR = CURIYR
      ENDIF

      k = 0

!****************** Body of the Program/Subprogram *****************************

!     calculate demand shifted prices by region, time slice and end-use group
       DO icls = 1, MAXSEC
        DO j = 1, neusgrp(icls)
         k = k +1
         DO ise=1,EFDns    ! seasons
          TOTEUSLD(ise) = 0.0
          TOTEUSLD1(ise) = 0.0
          b1=EFDSEDEF(ise,1)  ! starting and ending block in season
          b2=EFDSEDEF(ise,2)
          DO iblk=b1,b2
            b=iblk-b1+1
            s =  b

            frac = ((ERSLCPRC(IYR,RNB,ISE,s,3) + EPRICE(icls,2,RNB) + EPRICE(icls,3,RNB))/ (MC_JPGDP(IYR)*PELOUTN(RNB,IYR,k)))**(-1.*euelas(RNB,icls,j))

!  sum total load for normalization by season
            TOTSYSLOAD1(s,ise) = TOTSYSLOAD1(s,ise) + efdblkservo(s,ise,RNB,k)

            TOTEUSLD1(ise) = TOTEUSLD1(ise) + efdblkservo(s,ise,RNB,k)

! mult by frac
            efdblkserv(s,ise,RNB,k) = efdblkservo(s,ise,RNB,k) * frac
            TOTEUSLD(ise) = TOTEUSLD(ise) + efdblkserv(s,ise,RNB,k)

           ENDDO  !iblk
          ENDDO ! ise

!           completes one end-use
! normalize single end-use

        DO jj=1,EFDnS ! do for each season
          b1=EFDSEDEF(jj,1) ! first block in a season
          b2=EFDSEDEF(jj,2) ! last block in a season

!              Write the LDC into the communication common block arrays:

          DO i=b1,b2
            b=i-b1+1

!                    b:  block # in a seasonal LDC
!                   jj:  season
!                  RNB:  EMM region
!                 SECT:  sector
!          BlockNumEFD:  map of ordinal block numbers to block numbers in LDCs

! normalize   old over new
            If (TOTEUSLD(jj) .ne. 0) then
                efdblkserv(b,jj,RNB,k)= efdblkserv(b,jj,RNB,k) * TOTEUSLD1(JJ)/TOTEUSLD(jj)

! sum new for slice and season normalization
                TOTSYSLOAD(b,jj) = TOTSYSLOAD(b,jj) + efdblkserv(b,jj,RNB,k)
            endif
          ENDDO ! i
        ENDDO ! jj

!           write(IMSG,*)'k toteusld,toteul',k,(TOTEUSLD(jj) ,jj=1,EFDnS) ,TOTEULOAD(k,RNB)

         ENDDO   ! jj
      ENDDO  ! icls

! modify system load - if called from dsmefd subroutine

      if (callflag .eq. 1) then
        DO jj=1,EFDnS ! do for each season
          b1=EFDSEDEF(jj,1) ! first block in a season
          b2=EFDSEDEF(jj,2) ! last block in a season

!           Write the LDC into the communication common block arrays:

          DO iblk=b1,b2
            b=iblk-b1+1

!                    b:  block # in a seasonal LDC
!                   jj:  season
!                  RNB:  EMM region
!                 SECT:  sector
!          BlockNumEFD:  map of ordinal block numbers to block numbers in LDCs

!              if ((fcrl .eq. 1) )!      .and.  (RNB .eq. 1)) write(imsg, *) ' dsmshft: j b totsys1/totsys - ',CURIYR,RNB,jj,b, TOTSYSLOAD1(b,jj)/TOTSYSLOAD(b,jj),ULHGHT(b,jj,RNB)

! store old load

          totld1(jj) = totld1(jj) + ULHGHT(b,jj,RNB)*ULWDTH(b,jj,RNB)

!              BlockHeightEFD(iblk) = ULHGHT(b,jj,RNB) * TOTSYSLOAD1(b,jj)/ TOTSYSLOAD(b,jj)

               BlockHeightEFD(iblk) = ULHGHT(b,jj,RNB) * TOTSYSLOAD(b,jj)/ TOTSYSLOAD1(b,jj)

! store new load

          totld(jj) = totld(jj)+ BlockHeightEFD(iblk)*ULWDTH(b,jj,RNB)
          BlockNumEFD(iblk) = iblk
          EFDblWdth(iblk) = ULWDTH(b,jj,RNB)
          EFDblSg(iblk)= ULGRP(b,jj,RNB)
          EFDblSc(iblk)= ULSEG(b,jj,RNB)

         END DO ! iblk

!         write(IMSG,*)'b1,b2',b1,b2,efdnumbl

         CALL DSMQSR(BlockHeightEFD,BlockNumEFD,b1,b2,EFDnumBl)

!         write(IMSG,*)'b1,b2',b1,b2,efdnumbl
!         write(IMSG,*)'BLKHE',(BlockHeightEFD(iblk),iblk=b1,b2)

!           Write the LDC into the communication common block arrays:

        DO iblk=b1,b2
          b=iblk-b1+1
               ULHGHT(b,jj,RNB)=BlockHeightEFD(iblk) *  totld1(jj)/totld(jj)                      ! renormalize

          ULWDTH(b,jj,RNB)=EFDblWdth(BlockNumEFD(iblk))
          ULGRP(b,jj,RNB)=EFDblSg(BlockNumEFD(iblk))
          ULSEG(b,jj,RNB)=EFDblSc(BlockNumEFD(iblk))

          IF (RNB .EQ. 1) THEN
              write(IMSG,9123) CURIRUN, CURIYR+1989, CURITR, RNB, jj, b1, b2, iblk, b, ULGRP(b,jj,RNB), ULSEG(b,jj,RNB), ULHGHT(b,jj,RNB), ULWDTH(b,jj,RNB), &
              BlockHeightEFD(iblk),  totld1(jj), totld(jj)
 9123         format(1X,"ULHGHT_ULDSM_1",11(":",I6),5(":",F21.6))
          END IF

        ENDDO
       ENDDO ! jj

      endif   ! callflag

!      Only need to to do ecp load shifting on first iteration

       IF ((CURITR .gt. 1) .or. (callflag .eq. 1))  RETURN


!    Adjust ecp load on first iteration only

      DO IVLS=1 , ECP_D_VLS
        do slice=1,2
          EPSHFT(IVLS,slice) = 0.0
          EPSHFTW(IVLS,slice) = 0.0
        enddo
      END DO

      IYR = CURIYR

!      write(IMSG,*)'before',( ECPLDCBH(IYR,RNB,IVLS),IVLS=1,ECP_D_VLS)

       DO jj=1,EFDnS ! do for each season
         b1=EFDSEDEF(jj,1) ! first block in a season
         b2=EFDSEDEF(jj,2) ! last block in a season
         DO iblk=b1,b2
           sblk=gblocknumefd(iblk,RNB)
           b=iblk-b1+1

!           EPSHFT(Ushft(sblk)) = EPSHFT(USHFT(sblk)) + totsysload1(b,jj) /totsysload(b,jj)* ULWDTH(b,jj,RNB)

            EPSHFT(Ushft(sblk,1),USHFT(sblk,2)) = EPSHFT(USHFT(sblk,1),USHFT(sblk,2)) + totsysload(b,jj) /totsysload1(b,jj)* ULWDTH(b,jj,RNB)
            EPSHFTW(Ushft(sblk,1),Ushft(sblk,2)) = EPSHFTW(USHFT(sblk,1),USHFT(sblk,2))+ ULWDTH(b,jj,RNB)

         if (CURIYR .eq. 21) then
               write(imsg,1668) CURIYR,RNB,jj,b,sblk,USHFT(sblk,1), USHFT(sblk,2),totsysload1(b,jj),totsysload(b,jj), ULWDTH(b,jj,RNB)
 1668          FORMAT ('ecpshft ',7I4,3f10.2)
          endif

         ENDDO !iblk
       ENDDO ! jj

!  this subroutine is called from within a year loop k1=curiyr, curiyr+UNXPH-1

        IYR = K1

        TOTECPLD = 0.0
        TOTECPLD1 = 0.0

!  we go backwards (lowest to highest) to ensure that the slices are still in order
!
!       IVLS = ECP_D_VLS

      do seg = 1 , MAXECPsg
         do slice = 1 , ECPsgDnB(seg)
           do ivls2 = 1,ECP_D_VLS
              if (ECPLDCBS(IYR,RNB,ivls2) .eq. seg .and. ECPLDCSL(IYR,RNB,ivls2) .eq. slice) then
                 TOTECPLD = TOTECPLD + ECPLDCBH(IYR,RNB,IVLS2) * ECPLDCBW(IYR,RNB,IVLS2)
                 ECPLDCBH(IYR,RNB,IVLS2) = ECPLDCBH(IYR,RNB,IVLS2) * EPSHFT(seg,slice)/EPSHFTW(seg,slice)
                 TOTECPLD1 = TOTECPLD1 + ECPLDCBH(IYR,RNB,IVLS2) * ECPLDCBW(IYR,RNB,IVLS2)
              endif
           enddo
         enddo
      enddo

!   NORMALIZE load

       DO IVLS = 1 ,ECP_D_VLS
         ECPLDCBH(IYR,RNB,IVLS) = ECPLDCBH(IYR,RNB,IVLS) * TOTECPLD / TOTECPLD1
       ENDDO


!  update peak load variable for reserve margin calculation

      EPKSHFT(RNB,IYR) = ECPLDCBH(IYR,RNB,1)

      write(6,*) 'ldsm epkshft ',IYR,RNB,EPKSHFT(RNB,IYR)

!     when in this part of code (ecp loop) resave efblkserv with old values

      k=0
      DO icls = 1, MAXSEC
       DO j = 1, neusgrp(icls)
         k = k +1
            DO ise=1,EFDns    ! seasons
               do b = 1, MAXEFDSS
             efdblkserv(b,ise,RNB,k) = efdblkservo(b,ise,RNB,k)
           enddo
         ENDDO
       ENDDO
      ENDDO

      RETURN
      END

!************************************ new stuff TD ****************
!****************************************************************************************************************************
SUBROUTINE CALLSYSLOAD_EU
!****************** Description of the Program/Subprogram **********************
! THIS SUBROUTINE gets the enduse sysload from SQLite table
!*******************************************************************************
      
      USE SQLITE
      IMPLICIT NONE
!**********Declaring variables and common blocks via NEMS include files********
      include 'parametr' !<< nems parameter declarations
      include 'ncntrl' !<< acces to nems global variables like
      include 'emmparm'
      include 'dsmdimen' !<< calendar data
      
      type(sqlite_database)                      :: db
      type(sqlite_statement)                     :: stmt
      type(sqlite_column), dimension(:), pointer :: col
      character(len=40), pointer, dimension(:)   :: result
      character(len=80)                          :: errmsg
      logical                                    :: finished

      integer                                    :: p
      integer                                    :: q
      integer                                    :: Month_sq,Day_sq,Hour_sq
      integer                                    :: id 
      Real                                       :: DistLo_sq

      INTEGER*4 EUNUM

    !  COMMON /REGSYSLD/LSReuname_s,DistLo_eu_s
	  CHARACTER*8 LSReuname_s(MAXEU)
	  REAL*4 DistLo_eu_s(MAXEU,MAXHOUR) 
	  INTEGER*4 DBnr  ! DB number of records
	  COMMON /EUSYSLD/LSReuname_s,DistLo_eu_s, DBnr
      
        
      call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )          
    
             allocate ( col(7) )
               ! QUERY THE DATABASE TABLE
			  
                 call sqlite3_column_query( col(1), 'ID', sqlite_int ) 
                 call sqlite3_column_query( col(2), 'LS_INDEX', sqlite_int )
                 call sqlite3_column_query( col(3), 'LS_Name', sqlite_char )
                 call sqlite3_column_query( col(4), 'Month', sqlite_int )
                 call sqlite3_column_query( col(5), 'DAY', sqlite_int )
                 call sqlite3_column_query( col(6), 'HOUR', sqlite_int )
                 call sqlite3_column_query( col(7), 'PCT', sqlite_real )
                 
                 call sqlite3_prepare_select( db, 'V_SYS_LOAD_by_EndUse', col, stmt )
    
               ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
                 q = 0
				 DBnr=0
                 do
                   call sqlite3_next_row( stmt, col, finished )
                   if ( finished ) exit
    
                   call sqlite3_get_column( col(1), ID )
                   call sqlite3_get_column( col(2), EUNUM )
                   call sqlite3_get_column( col(3), LSReuname_s(EUNUM) )
                   call sqlite3_get_column( col(4), Month_sq )
                   call sqlite3_get_column( col(5), Day_sq )
                   call sqlite3_get_column( col(6), Hour_sq )
                   call sqlite3_get_column( col(7), DistLo_sq)
                   
                   q = ((Month_sq-1)*3+(Day_sq-1))*24 + Hour_sq
                   
                   DistLo_eu_s(EUNUM,q) = DistLo_sq
				   if (EUNUM.gt.DBnr) DBnr=EUNUM
                   
                    if (Month_sq .EQ. 12 .and. Day_sq .EQ. 3 .and. Hour_sq .EQ. 24) q = 0
                 end do
                 !!write(6,*)'done writing hourly data to db'
                 
                       

                deallocate ( col ) 
                
       call sqlite3_close( db ) 

      RETURN      
      END
!****************************************************************************	  
	 SUBROUTINE LOAD_LDSM_CAL
!****************** Description of the Program/Subprogram **********************
! THIS SUBROUTINE gets the ldsm calendar data from the database and assigns it to nems variables
!*******************************************************************************
! replace this read from ldsmdaf:(did not replace SENAME or DAFnr)
!         READ(IODB,REC=1)DAFnr,NMONTH,(NODAYS(I),I=1,NMONTH), &
!         ((WEIGHT(J,K),J=1,NODAYS(K)),K=1,NMONTH),NODAYT,NOSEA, &
!         (MONTYP(I),I=1,NMONTH),((IDAYTQ(I,J),I=1,NODAYS(J)),J=1,NMONTH), &
!         ((JDAYTP(I,J),i=1,NODAYS(J)),J=1,NMONTH),(SENAME(I),I=1,NOSEA), &
!         (DTNAME(I),I=1,NODAYT),(MONAME(i),i=1,NMONTH)
    

	  USE SQLITE
      IMPLICIT NONE

      include 'parametr' !<< nems parameter declarations
      include 'ncntrl' !<< acces to nems global variables like
      include 'emmparm'
      include 'dsmdimen' !<< calendar data
	  include 'dsmcaldr' !<< calendar data
      
      type(sqlite_database)                      :: db
      type(sqlite_statement)                     :: stmt
      type(sqlite_column), dimension(:), pointer :: col
      character(len=40), pointer, dimension(:)   :: result
      character(len=80)                          :: errmsg
      logical                                    :: finished

      integer                                    :: Month,Day
      integer                                    :: id 
	  integer                                    :: num_mo
	  integer                                    :: lNODAYS(MAXMON),lMONTYP(MAXMON)
	  integer                                    :: MOINDEX,Day_type
	  integer                                    :: lWEIGHT(MAXDAY,MAXMON)
      
      
      call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )          
    
      allocate ( col(4) )
      ! QUERY THE DATABASE TABLE
     call sqlite3_column_query( col(1), 'ID', sqlite_int ) 
     call sqlite3_column_query( col(2), 'NMONTH', sqlite_int ) 
     call sqlite3_column_query( col(3), 'NODAYT', sqlite_int )
     call sqlite3_column_query( col(4), 'NOSEA', sqlite_int )
                              
     call sqlite3_prepare_select( db, 'V_SYS_LOAD_CalParam', col, stmt )
    
     ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
     do
        call sqlite3_next_row( stmt, col, finished )
        if ( finished ) exit
    
        call sqlite3_get_column( col(1), ID )
        call sqlite3_get_column( col(2), num_mo )
		NMONTH=num_mo
        call sqlite3_get_column( col(3), num_mo )
		NODAYT=num_mo
        call sqlite3_get_column( col(4), num_mo )
		NOSEA=num_mo
	 enddo

     deallocate ( col ) 
					
	 allocate ( col(5) )
     ! QUERY THE DATABASE TABLE  
			  
     call sqlite3_column_query( col(1), 'ID', sqlite_int ) 
	 call sqlite3_column_query( col(2), 'MOINDEX', sqlite_int ) 
     call sqlite3_column_query( col(3), 'MONAME', sqlite_char ) 
     call sqlite3_column_query( col(4), 'NODAYS', sqlite_int )
     call sqlite3_column_query( col(5), 'MONTYPE', sqlite_int )
                     
     call sqlite3_prepare_select( db, 'V_SYS_LOAD_CalMo', col, stmt )
    
     ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
     do
        call sqlite3_next_row( stmt, col, finished )
        if ( finished ) exit
    
        call sqlite3_get_column( col(1), ID )
	    call sqlite3_get_column( col(2), MOINDEX )
        call sqlite3_get_column( col(3), MONAME(MOINDEX) )
		call sqlite3_get_column( col(4), lNODAYS(MOINDEX) )
		NODAYS=lNODAYS
        call sqlite3_get_column( col(5), lMONTYP(MOINDEX) )
		MONTYP=lMONTYP
     enddo
     deallocate ( col ) 
             
	 allocate ( col(6) )
     ! QUERY THE DATABASE TABLE  
			
     call sqlite3_column_query( col(1), 'ID', sqlite_int ) 
	 call sqlite3_column_query( col(2), 'Month', sqlite_int ) 
     call sqlite3_column_query( col(3), 'Day', sqlite_int ) 
     call sqlite3_column_query( col(4), 'Weight', sqlite_int )
                                         
     call sqlite3_prepare_select( db, 'V_SYS_LOAD_WEIGHTS', col, stmt )
    
     ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
     do
        call sqlite3_next_row( stmt, col, finished )
        if ( finished ) exit
    
        call sqlite3_get_column( col(1), ID )
	    call sqlite3_get_column( col(2), Month )
        call sqlite3_get_column( col(3), Day )
        call sqlite3_get_column( col(4), lWeight(Day,Month) )
		WEIGHT=lWEIGHT
        IDAYTQ=lWeight
		JDAYTP(Day,Month)=Day
     enddo
     deallocate ( col ) 
     call sqlite3_close( db )         

     RETURN      
     END