! $Header: m:/default/source/RCS/ucape.f,v 1.115 2020/10/23 18:42:57 LC2 Exp $
!
!
! THE TECHNOLOGY SUBMODULE PERFORMS MULTIPLE OPERATIONS.
!    SUBROUTINE ELSTCAP COMPUTES THE MAXIMUM HISTORICAL ADDITIONS PRIOR TO THE
!          EXECUTION OF THE ECP, WHICH ARE USED TO DETERMINE THE SUPPLY STEPS
!    SUBROUTINE ELSTELA CALCULATES THE SHORT-TERM ELASTICITIES
!    SUBROUTINE ELOPTLC DETERMINES THE TECHNOLOGICAL OPTIMISM AND LEARNING FACTORS, IF ANY
!    SUBROUTINE ELHRTLC DETERMINES THE IMPROVEMENT IN HEATRATES, IF THAT OPTION IS SELECTED
!    SUBROUTINE EPO$MSHR EXECUTES A MARKET-SHARING ALGORITHM AFTER THE ECP SOLVES TO ADJUST
!          THE ECP BUILD DECISIONS AMONG COMPETITIVE TECHNOLOGIES
!    SUBROUTINE EP0$HBLD DETERMINES SUPPLEMENTAL HYDRO BUILDS, IF ALLOWED (SUCH AS IN A CARBON SCENARIO)


      SUBROUTINE ELSTCAP

      IMPLICIT NONE

!     THIS SUBROUTINE DETERMINES THE MAXIMUM ANNUAL CAPACITY ADDITIONS TO USE AS A REFERENCE FOR SHORT-TERM ELASTICITIES
!        BASED ON RECENT HISTORY YEARS

      include'parametr'
      include'emmparm'
      include'ncntrl'
      include'control'
      include'ecpcntl'
      include'entcntl'
      include'uecpout'

      INTEGER ICAP,YEAR,IYS,IYE
      REAL*4 ANNADD,ANNWT

!  INITIALIZE QUANTITY TO AT LEAST 1 UNIT

      DO ICAP = 1 , ECP_D_CAP
         ESTCPADD(ICAP) = ESTMNADD(ICAP) / (1.0 + ESTCPSTP(ICAP,1))
         IF (UPVTYP(ICAP) .GT. 0 .AND. (CURIYR + 1989) .GE. UPSTYR)THEN

!           IF (ICAP .LE. ECP_D_DSP)THEN

               IYS = CURIYR - 5
               IYE = CURIYR - 1

!           ELSE
!              IYS = CURIYR - 4
!              IYE = CURIYR
!           END IF


!  CHECK FOR MAX ADDITIONS (UTILITY/NONUTILITY, PLANNED/UNPLANNED) OVER PREVIOUS FEW YEARS

            DO YEAR = IYS , IYE
               IF ((YEAR + UHBSYR) .GT. UPLRSYR(ICAP))THEN
                  ANNADD = ESTYRADD(ICAP,YEAR)
                  ANNWT = REAL(YEAR - IYS + 1) / REAL(IYE - IYS + 1)
                  ANNWT = 1.0 * 0.5 + ANNWT * 0.5      !only take half the weight so that older capacity isn't devalued as much
                  IF (ICAP .EQ. WIWN)ANNADD = ANNADD + ESTYRADD(WIWL,YEAR)
                  IF (ICAP .EQ. WIWL)ANNADD = ANNADD + ESTYRADD(WIWN,YEAR)
                  IF (ICAP .EQ. WIPV)ANNADD = ANNADD + ESTYRADD(WIPT,YEAR)
                  IF (ICAP .EQ. WIPT)ANNADD = ANNADD + ESTYRADD(WIPV,YEAR)
                  IF (ICAP .EQ. WICT)ANNADD = ANNADD + ESTYRADD(WIET,YEAR)
                  IF (ICAP .EQ. WICC)ANNADD = ANNADD + ESTYRADD(WIEC,YEAR)
                  ANNADD = ANNADD * ANNWT
                  IF (ANNADD .GT. ESTCPADD(ICAP))ESTCPADD(ICAP) = ANNADD
               END IF
            END DO

!  EXCLUDE CAPACITY REVISIONS FOR "WHOLE" UNITS FROM CAPACITY USED FOR STEP BOUND
!           IF ((CURIYR + 1989) .GT. UPSTYR)ESTCPADD(ICAP) = ESTCPADD(ICAP) - ESTROUND(ICAP)

         END IF
         IF (ICAP .EQ. WIAC) ESTCPADD(ICAP) = MAX(ESTCPADD(ICAP),ESTCPADD(WICC))
         IF (ICAP .EQ. WIAT) ESTCPADD(ICAP) = MAX(ESTCPADD(ICAP),ESTCPADD(WICT))
      END DO

!
      RETURN
      END


      SUBROUTINE ELSTELA

      IMPLICIT NONE

!     THIS SUBROUTINE DETERMINES THE COSTS AND CAPACITIES ASSOCIATES WITH THE SHORT-TERM ELASTICITIES

      include'parametr'
      include'emmparm'
      include'ncntrl'
      include'control'
      include'ecpcntl'
      include'entcntl'
      include'uecpout'
!
      INTEGER ICAP,STEP
      REAL*4 CUMFRAC,CAPSTP
      REAL*4 ESTELAST
!
      DO ICAP = 1 , ECP_D_CAP
         IF (UPVTYP(ICAP) .GT. 0 .AND. ESTSWTCH(ICAP) .GT. 0)THEN
            CAPSTP = 0.0
            CUMFRAC = 1.0
            DO STEP = 1 , ECP_D_SSTP
               IF (STEP .EQ. 1)THEN
                  CUMFRAC = CUMFRAC + ESTCPSTP(ICAP,1)
                  ESTCPLIM(ICAP,STEP) = ESTCPADD(ICAP) * CUMFRAC
                  IF (ESTSWCSM .EQ. 1)THEN
                     ESTCPCST(ICAP,STEP) = 1.0
                  ELSE
                     ESTCPCST(ICAP,STEP) = 1.0 + ESTCPCSM(ICAP,STEP)
                  END IF
                  CAPSTP = CAPSTP + ESTCPLIM(ICAP,STEP)

                  write(18,2222) CURIRUN, curiyr+1989, uplntcd(icap), estcpadd(icap), step, cumfrac, capstp,  &
                     CAPSTP,  &
                     ESTCPSTP(ICAP,1), ESTCSINC(ICAP), ESTCPINC(ICAP),  &
                     estcpcst(icap,step), estcplim(icap,step)

               ELSE
                  CUMFRAC = CUMFRAC + ESTCPSTP(ICAP,STEP)
                  ESTCPLIM(ICAP,STEP) = ESTCPADD(ICAP) * CUMFRAC - CAPSTP
                  IF (ESTSWCSM .EQ. 1)THEN
                     ESTCPCST(ICAP,STEP) = ESTELAST(CAPSTP + ESTCPLIM(ICAP,STEP) / 2.0,  &
                                              ESTCPADD(ICAP),ESTCPSTP(ICAP,1),ESTCSINC(ICAP),ESTCPINC(ICAP))
                  ELSE
                     ESTCPCST(ICAP,STEP) = 1.0 + ESTCPCSM(ICAP,STEP)
                  END IF

                  write(18,2222) CURIRUN, curiyr+1989, uplntcd(icap), estcpadd(icap), step, cumfrac, capstp,  &
                     CAPSTP + ESTCPLIM(ICAP,STEP) / 2.0,  &
                     ESTCPSTP(ICAP,1), ESTCSINC(ICAP), ESTCPINC(ICAP),  &
                     estcpcst(icap,step), estcplim(icap,step), alog(1.0+estcsinc(icap)), alog(1.0+estcpinc(icap))
 2222             format(1h ,'!stelas',2(":",i4),":",a3,":",f21.6,":",i4,10(":",f21.6))

                  CAPSTP = CAPSTP + ESTCPLIM(ICAP,STEP)
               END IF

!              IF (UPLNTCD(ICAP) .EQ. 'AN')ESTCPCST(ICAP,STEP) = ESTCPCST(ICAP,STEP) + 0.10

            END DO

!           print *,'!stelas',curiyr+1989,icap,estcpcst(icap,1),estcpcst(icap,2),estcpcst(icap,3)

         ELSE
            ESTCPCST(ICAP,1) = 1.0

!           IF (UPLNTCD(ICAP) .EQ. 'AN')ESTCPCST(ICAP,1) = ESTCPCST(ICAP,1) + 0.10

         END IF
      END DO
      RETURN
      END


      FUNCTION ESTELAST(CAP2,CAP1,THRESH,CSINC,CPINC)

      IMPLICIT NONE

!     FUNCTION ESTELAST DETERMINES THE SHORT-TERM ELASTICITIES TO REPRESENT A CAPACITY SUPPLY CURVE

      include'parametr'
      include'emmparm'
      include'ncntrl'
      include'control'

      REAL*4 CAP2
      REAL*4 CAP1
      REAL*4 THRESH
      REAL*4 CSINC
      REAL*4 CPINC
      REAL*4 ESTELAST

      ESTELAST = ((CAP2 / CAP1) - THRESH) ** (ALOG(1.0 + CSINC) / ALOG(1.0 + CPINC))

      RETURN
      END


      SUBROUTINE ELOPTLC

      IMPLICIT NONE

!     THIS SUBROUTINE DETERMINES THE OVERNIGHT CAPITAL COST ADJUSTMENTS DUE TO TECHNOLOGICAL OPTIMISM AND LEARNING EFFECTS

      include'parametr'
      include'emmparm'
      include'ncntrl'
      include'control'
      include'ecpcntl'
      include'entcntl'
      include'bildin'
      include'cdsparms'
      include'uso2grp'
      include'enewtech'
      include'bldglrn'
      include'uefdout'

!DSB*******VARIABLE DECLARATIONS**********************************
      REAL*4 MN_LRN                 !MINIMUM ANNUAL DECREASE IN LEARNING
      REAL*4 ANNLRN                 !COMPUTED ANNUAL DECREASE IN LEARNING
      REAL*4 LRN_EXPECT             !MINIMUM TOTAL DECREASE IN LEARNING

      REAL*4 SLOPE_OPT              !Slope of optimism function

      REAL*4 INIT_CAP(ECP$COMP)     !Cap(0)  1999 capacity if
                                    !year >= 1999 else = Cap(0)
                                    !for current year
                                    !max(UPLRSIZ or 0)

      REAL*4 N(3)                   !Capacity at beginning of each step.

      REAL*4 A(3)                   !Function intercept for each step

      REAL*4 B(3)                   !Progress Ratio for each step.
                                    ! = -(LOG(1-learning rate)/LOG(2)
      REAL*4 OC(3)                  !Overnight costs at beginning of each step

      REAL*4 OC_CURRENT             !Overnight costs at current capacity


       REAL*4 NEW_CAP(ECP_D_CAP,MNUMYR)      !Total new capacity
       REAL*4 NEW_CAPC(ECP$COMP,MNUMYR)    !New capacity by component

       REAL*4 NEW_TCAP(ECP_D_CAP,MNUMYR)     !Sum of new capacity and international learning
       REAL*4 NEW_TCAPC(ECP$COMP,MNUMYR)   !New + Intern. by component

      REAL*4 LRN_RCI(ECP$COMP,MNUMYR) !GJA(8/22/11) Capacity from end-use modules (R/C/I)

      CHARACTER*15 VINTAGE(ECP$COMP)

      REAL*4 LRN_CAP(ECP$COMP,MNUMYR)   ! Amount of capacity that will be applied to learning
                                        ! for each technology (mW)

      REAL*4 OPT_CAP(ECP$COMP)      ! Amount of capacity that will be applied to optimism

      REAL*4 LRMAX

      INTEGER*4 NERC
      INTEGER*4  J,I,K

      INTEGER*4  DEBUGGING
      INTEGER*4  BASE_YEAR          !BASE YEAR DO LEARNING AND INITIAL CAPITAL COSTS
                                    ! EX.  = 1999 IN AEO2000
!     REAL*4   CAPCOMPWT(ECP$COMP,ECP_D_CAP)          ! capacity weight matrix -plt typ vs component
!     REAL*4   CSTCOMPWT(ECP$COMP,ECP_D_CAP)          ! cost weight per component

      

!DSB  ***************** BEGIN ELOPTLC******************************
      DEBUGGING = 0  ! 1=do write statements; 0=do not do write statements
!DSB***************************************************************
!     BASE_YEAR = YEARPR+1
      BASE_YEAR = ULRSYR
!DSB***************************************************************
!     IF SWITCH FOR OPTIMISM AND LEARNING IS OFF, THEN SET FACTORS
!     TO 1.0
      IF (ECP_D_LFCC .LE. 0) THEN  !Set Opt. and Learning Factors
         DO J = 1 , ECP$COMP
            UPLROPTC(J) = 1.0
            UPLRLCC(J) = 1.0
         END DO
         DO J = 1 , ECP_D_CAP
            UPLROPT(J) = 1.0
            UPLRLC(J) = 1.0
         END DO
      ELSE   ! Compute Opt. and Learning Factors
!     *** BEGIN COMPUTE OPTIMISM AND LEARNING FACTORS **************
!DSB***************************************************************
!DSB     ACCUMULATE TOTAL NEW CAPACITY FOR EACH TYPE

       DO J = 1, ECP_D_CAP
              NEW_CAP(J,CURIYR) = 0.0
              NEW_TCAP(J,CURIYR) = 0.0
      ENDDO

      DO K = 1, ECP$COMP
           NEW_CAPC(K,CURIYR) = 0.0
           NEW_TCAPC(K,CURIYR) = 0.0
           LRN_RCI(K,CURIYR) = 0.0  !Added by GJA 8/11/2011
      ENDDO

      DO NERC = 1, UNRGNS  !Loop over NERC regions

       CALL GETBLD(1, NERC)

       DO J = 1, ECP_D_CAP
         NEW_CAP(J,CURIYR) = NEW_CAP(J,CURIYR) + EPNCAP(J)
       ENDDO

      ENDDO  !Loop over NERC regions

!DSB     ADD INTERNATIONAL LEARNING, DOMESTIC CAPACITY  ** move international learning later so it can have different mapping to components
         DO J  =  1, ECP_D_CAP
           IF (CURIYR .GT. FIRSYR) THEN
            IF (NEW_CAP(J,CURIYR) .LT. NEW_CAP(J,CURIYR - 1)) THEN
                NEW_CAP(J,CURIYR) = NEW_CAP(J,CURIYR - 1)
            ENDIF
           ENDIF
!            NEW_TCAP(J, CURIYR)  = NEW_CAP (J,CURIYR) + &
 !                              UPLRLCI(J, CURIYR) * UPISHR(J)
         ENDDO
!    GJA 9/22/11 create variable to send to end-use modules for utility PV capacity
!         UPV_MW(CURIYR)= NEW_TCAP(WIPV, CURIYR)      move later and assign to PV module component capacity

!DSB     **** END - ADD INTERNATIONAL CAPACITY ********************

!        aggregate capacity by component
         DO I = 1, ECP$COMP
           DO J = 1, ECP_D_CAP
            NEW_TCAPC(I,CURIYR) = NEW_TCAPC(I,CURIYR) + (NEW_CAP(J,CURIYR)*UPLCAPWT(J,I) +   &  
                                           UPLRLCI(J,CURIYR) * UPISHR(J) * UPLCAPWT_INT(J,I) )     !add international to component capacity using different weights if needed (PV)
            NEW_CAPC(I,CURIYR) = NEW_CAPC(I,CURIYR) + NEW_CAP(J,CURIYR)*UPLCAPWT(J,I)
           ENDDO
         ENDDO

         UPV_MW(CURIYR) = NEW_TCAPC(WIPVM,CURIYR)        ! variable for end-use modules based on PVM capacity
         
         LRN_RCI(WIPVM, CURIYR)= CPV_MW(CURIYR) + RPV_MW(CURIYR)
         NEW_TCAPC(WIPVM,CURIYR) = NEW_TCAPC(WIPVM,CURIYR) + LRN_RCI(WIPVM,CURIYR)
!    GJA 9/8/11: LRN_RCI added for PV only.  This hard-wiring should be removed once result is verified.

!DSB     **********************************************************
         DO J  =  1, ECP$COMP   !LOOP OVER CAPACITY
!          IF (ECP_D_OVCC(J) .LE. 0) THEN  ! NO OVERWRITES FOR CAPITAL COSTS

!DSB        *********************************************************
!DSB        CALCULATE EQUATION PARAMETERS FOR FUNCTION BREAKPOINTS
!DSB        *********************************************************

!DSB        *** IDENTIFY VINTAGE ************************************
            IF (UPLRLCX(J)  .EQ.  0) THEN
              VINTAGE(J)  =  'REVOLUTIONARY'
            ELSEIF (UPLRLCX(J)  .EQ.  1) THEN
              VINTAGE(J)  =  'EVOLUTIONARY'
            ELSE
              VINTAGE(J)  =  'CONVENTIONAL'
            ENDIF
            LRMAX = 1.0 - UPLRMAX(UPLRLCX(J)+1)
!DSB        *** END - IDENTIFY VINTAGE *******************************

!DSB        *** CALCULATE INITIAL CAPACITY -- CAP(0) ****************
            IF ((CURIYR + 1989) .GE.  BASE_YEAR) THEN
              IF (NEW_TCAPC(J, BASE_YEAR - 1989 - 1) .GT. UPLRSIZ(J)) THEN
                INIT_CAP(J)  =  NEW_TCAPC(J, BASE_YEAR - 1989)
              ELSE
                INIT_CAP(J)  =  UPLRSIZ(J)
              ENDIF
            ELSE
              INIT_CAP(J)  =  UPLRSIZ(J)
            ENDIF
!DSB        *** END - CALCULATE INITIAL CAPACITY -- CAP(0) ****************

!DSB        *** CALCULATE BREAKPOINT CAPACITIES -- THE N's **********
            N(1)  =  INIT_CAP(J)

            IF (VINTAGE(J)  .EQ.  'REVOLUTIONARY') THEN
              N(2)  =  INIT_CAP(J) * 2.0 ** UPLRLCU(J, 1)
            ELSE
              N(2)  =  INIT_CAP(J)
            ENDIF

            IF (VINTAGE(J)  .EQ.  'REVOLUTIONARY') THEN
              N(3)  =  INIT_CAP(J) * 2.0 ** (UPLRLCU(J, 1) + UPLRLCU(J, 2))
            ELSEIF (VINTAGE(J)  .EQ.  'EVOLUTIONARY') THEN
              N(3)  =  INIT_CAP(J) * 2.0 ** UPLRLCU(J, 2)
            ELSE
              N(3)  =  INIT_CAP(J)
            ENDIF


!DSB        *** END-CALCULATE BREAKPOINT CAPACITIES -- THE N's **********

!DSB        *** CALCULATE PROGRESS RATIOS -- THE b's **********

              B(1)  =  -(Log(1.0 - UPLRLCR(J, 1)) / Log(2.0))
              B(2)  =  -(Log(1.0 - UPLRLCR(J, 2)) / Log(2.0))
              B(3)  =  -(Log(1.0 - UPLRLCR(J, 3)) / Log(2.0))

!DSB        *** END-CALCULATE PROGRESS RATIOS -- THE b's **********

!DSB        *** CALCULATE INTERCEPT -- THE A's **********
            IF (VINTAGE(J)  .EQ.  'REVOLUTIONARY') THEN
              A(1)  =  1.0 / (INIT_CAP(J) ** (-B(1)))
            ELSEIF (VINTAGE(J)  .EQ.  'EVOLUTIONARY') THEN
              A(1)  =  1.0 / (INIT_CAP(J) ** (-B(2)))
            ELSE
              A(1)  =  1.0 / (INIT_CAP(J) ** (-B(3)))
            ENDIF

            IF (VINTAGE(J)  .EQ.  'REVOLUTIONARY') THEN
              A(2)  =  (A(1) * N(2) ** (-B(1))) / (N(2) ** (-B(2)))
            ELSEIF (VINTAGE(J)  .EQ.  'EVOLUTIONARY') THEN
              A(2)  =  1.0 / (INIT_CAP(J) ** (-B(2)))
            ELSE
              A(2)  =  1.0 / (INIT_CAP(J) ** (-B(3)))
            ENDIF

            IF (VINTAGE(J)  .EQ.  'REVOLUTIONARY') THEN
              A(3)  =  (A(2) * N(3) ** (-B(2))) / (N(3) ** (-B(3)))
            ELSEIF (VINTAGE(J)  .EQ.  'EVOLUTIONARY') THEN
              A(3)  =  (A(2) * N(3) ** (-B(2))) / (N(3) ** (-B(3)))
            ELSE
              A(3)  =  1.0 / (INIT_CAP(J) ** (-B(3)))
            ENDIF

!DSB        *** END - CALCULATE INTERCEPT -- THE A's **********
!DSB        *********************************************************
!DSB        END - CALCULATE EQUATION PARAMETERS DO FUNCTION BREAKPOINTS
!DSB        *********************************************************

!DSB        CALCULATE OVERNITE COSTS AT EACH PERIOD START
                OC(3)  =  A(3) * (N(3) ** (-1.0 * B(3)))
                OC(2)  =  A(2) * (N(2) ** (-1.0 * B(2)))
                OC(1)  =  A(1) * (N(1) ** (-1.0 * B(1)))

!DSB               IF NEW CAPACITY REQUIRED
!DSB        EXCEEDS THE DOUBLING LIMIT THEN INCREASE LEARNING CAPACITY
!DSB        BY THE DOUBLING AMOUNT ONLY -- ELSE INCREASE LEARNING CAPACITY
!DSB        BY THE AMOUNT OF NEW CAPACITY

              IF ((NEW_TCAPC(J, CURIYR) .LT. UPLRSIZ(J)) .OR. &
                 (CURIYR .LT. (BASE_YEAR - 1989))) THEN
                LRN_CAP(J, CURIYR)  =  UPLRSIZ(J)
              ELSEIF (CURIYR  .EQ.  (BASE_YEAR - 1989)) THEN
               IF (NEW_TCAPC(J, CURIYR) .LT. UPLRSIZ(J)) THEN
                  LRN_CAP(J, CURIYR)  =  UPLRSIZ(J)
               ELSE
                  LRN_CAP(J, CURIYR)  =  NEW_TCAPC(J, CURIYR)
               ENDIF
              ELSE
!               IF (NEW_TCAPC(J, CURIYR - 1) .GT. 0) THEN
                 IF (NEW_TCAPC(J, CURIYR) .GT. &
                    (2.0 ** IFIX(UPLRDBL(J)) * &
                     LRN_CAP(J, CURIYR - 1) * &
                     (1.0 + UPLRDBL(J) - IFIX (UPLRDBL(J))))) &
                      THEN
                   LRN_CAP(J, CURIYR)  =  (2.0 ** IFIX (UPLRDBL(J)) * &
                     LRN_CAP(J, CURIYR - 1) * &
                     (1.0 + UPLRDBL(J) - IFIX (UPLRDBL(J))))
                 ELSE
                   IF (NEW_TCAPC(J, CURIYR - 1) .LT. &
                       LRN_CAP(J, CURIYR - 1)) THEN
                    LRN_CAP(J, CURIYR)  =  LRN_CAP(J, CURIYR - 1) &
                    + (NEW_TCAPC(J, CURIYR) - LRN_CAP(J, CURIYR - 1))
                   ELSE
                    LRN_CAP(J, CURIYR)  =  LRN_CAP(J, CURIYR - 1) &
                    + (NEW_TCAPC(J, CURIYR) - NEW_TCAPC(J, CURIYR - 1))
                   ENDIF
                  ENDIF
!                ELSE
!                   LRN_CAP(J, CURIYR)  =  &
!                    (2.0 ** IFIX (UPLRDBL(J)) * &
!                    LRN_CAP(J, CURIYR - 1) * &
!                    (1.0 + UPLRDBL(J) - IFIX (UPLRDBL(J))))
!                ENDIF
              ENDIF

              IF (LRN_CAP(J, CURIYR) .GE.  N(3)) THEN
                OC_CURRENT  =  A(3) * &
                            (LRN_CAP(J, CURIYR) ** (-1.0 * B(3)))
              ELSEIF (LRN_CAP(J, CURIYR) .GE.  N(2)) THEN
                OC_CURRENT  =  A(2) * &
                             (LRN_CAP(J, CURIYR) ** (-1.0 * B(2)))
              ELSE
                IF (VINTAGE(J) .NE.  'REVOLUTIONARY') THEN
                  OC_CURRENT  =  A(2) * &
                             (LRN_CAP(J, CURIYR) ** (-1.0 * B(2)))
                ELSE
                  OC_CURRENT  =  A(1) * &
                             (LRN_CAP(J, CURIYR) ** (-1.0 * B(1)))
                ENDIF
              ENDIF

!DSB        *************** CALCULATE LEARNING FACTOR **************

!           SET ANNUAL LEARNING MINIMUM TO INPUT VALUE
            MN_LRN  =  UPMNLRN(J)

            IF ((CURIYR + 1989) .GT. BASE_YEAR) THEN
               ANNLRN = UPLRLCC(J) - OC_CURRENT
               IF (ANNLRN .LT. MN_LRN) THEN
                 UPLRLCC(J) = UPLRLCC(J) - MN_LRN
               ELSE
                 UPLRLCC(J) = OC_CURRENT
               ENDIF
            ELSE
               UPLRLCC(J) = OC_CURRENT
            ENDIF
            IF (UPLRLCC(J) .LT. LRMAX) UPLRLCC(J) = LRMAX

!DSB        *************** CALCULATE OPTIMISM FACTOR **************
            OPT_CAP(J) = MAX(NEW_CAPC(J,CURIYR) , UPLRSIZ(J))
            SLOPE_OPT  =  (1.0 - UPLROP0(J)) / &
             (UPLROPE(J) - UPLROPS(J))

             IF ((OPT_CAP(J) / UPLRSIZ(J)) .LE. &
                  (UPLROPE(J) + 1.0)) THEN
               IF (CURIYR + 1989 .GT. BASE_YEAR) THEN
                UPLROPTC(J)  =  UPLROP0(J) + &
                SLOPE_OPT * (OPT_CAP(J) / &
                UPLRSIZ(J) - 1.0)
               ELSE
                UPLROPTC(J) = UPLROP0(J)
               END IF
             ELSE
                UPLROPTC(J)  =  1.0
             ENDIF
!DSB        ***********END -CALCULATE OPTIMISM FACTOR **************

       WRITE(UF_MSG,101) CURIYR+1989, J, &
                  UPLROP0(J),  &
                  UPLROPE(J),UPLROPS(J), &
                  UPLRSIZ(J), &
                  UPLRLCU(J,1),UPLRLCU(J,2),  &
                  UPLRLCU(J,3),UPLRLCR(J,1), &
                  UPLRLCR(J,2),UPLRLCR(J,3), &
                  UPLRDBL(J),UPMNLRN(J), &
                  MN_LRN, LRN_EXPECT

        WRITE(UF_MSG,103) CURIYR+1989, J,  &
                  NEW_TCAPC(J,CURIYR), NEW_CAPC(J,CURIYR), &
                  LRN_RCI(J,CURIYR), VINTAGE(J), &
                  LRN_CAP(J,CURIYR),OPT_CAP(J), &
                  UPLRLCC(J),UPLROPTC(J)

      ENDDO   !LOOP OVER CAPACITIES (J)

!    CALCULATE AVERAGE LEARNING AND OPTIMISM BY PLANT TYPE
      DO J = 1, ECP_D_CAP
       IF (ECP_D_OVCC(J) .LE. 0) THEN  ! NO OVERWRITES FOR CAPITAL COSTS
        UPLRLC(J) = 0.0
        UPLROPT(J) = 0.0
        DO I = 1, ECP$COMP
           UPLRLC(J) = UPLRLC(J) + UPLRLCC(I) * UPLCSTWT(J,I)
           UPLROPT(J) = UPLROPT(J) + UPLROPTC(I) * UPLCSTWT(J,I)
        ENDDO
       ENDIF
      WRITE(UF_MSG,106) CURIYR+1989,J,UPLNTCD(J),NEW_CAP(J,CURIYR),&
            UPLRLC(J),UPLROPT(J)
      ENDDO
    ENDIF   !Calculate Optimism and Learning Factors

! Calculate updated share of CCS component of total cost
     DO J = 1, ECP_D_CAP
         IF (UPLCSTWT(J,WISEQ) .GT. 0.0) THEN          !CCS plant - update OVR share for learning
             UPCCS_INVSH(J) = UPCCS_INVSH0(J) * UPLRLCC(WISEQ)/UPLRLC(J) !update based on relative learning
         ENDIF
      WRITE(UF_MSG,107) CURIYR+1989,J,UPLNTCD(J),UPCCS_VOMSH(J),UPCCS_FOMSH(J),UPCCS_INVSH(J)
     ENDDO
!     *** END COMPUTE OPTIMISM AND LEARNING FACTORS **************
101    FORMAT ('UCAPE_OUT_FACTORS1',':',I4,':',I2,':', &
              F4.2,':',I2,':',I2,':',F12.3,':',I3,':',I3,':',I3,':', &
              F4.3,':',F4.3,':',F4.3,':',F6.3,':',F6.3,':', &
              F10.7,':',F6.3)

103    FORMAT ('UCAPE_OUT_NEW1',':',I4,':',I2,':', &
              F12.4,':',F12.4,':',F12.4,':',A15,':', &
              F12.4,':',F12.4,':',F6.4,':',F6.4)

106    FORMAT ('UCAPE_OUT_NEW2',':',I4,':',I2,':',A2,':',F12.4,':', &
               F6.3,':',f6.3)
107    FORMAT ('UCAPE_OUT_CCS',':',I4,':',I2,':',A2,':',F12.4,':', F12.4)
       RETURN
       END


      SUBROUTINE ELHRTLC

      IMPLICIT NONE

!     THIS SUBROUTINE DETERMINES THE ADJUSTMENTS IN THE HEATRATES DUE TO TECHNOLOGICAL IMPROVEMENTS

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
!
      REAL*4 YRSPAN,YRDIFF
      INTEGER DSP,YRADJ

!     Calculate a default heatrate for all dispatchable capacity types to be used in crpgrp the following model year.

      IF (CURIYR .LT. UNYEAR) THEN

!        LOOP OVER DISPATCHABLE PLANT TYPES

         DO DSP = 1 , ECP_D_DSP

!           DETERMINE ADJUSTMENT TO LEADTIME FOR COMPRESSED PLANNING HORIZON

            YRADJ = UPPLYR(UCPDSPI(DSP))

!           IF SWITCH FOR HEATRATE IMPROVEMENTS IS OFF OR A PARTICULAR TECHNOLOGY IS ASSUMED TO HAVE NO IMPROVEMENT, USE CURRENT HEATRATE

            IF(ECP_D_LFHR .LE. 0 .OR. UPDHRSW(DSP) .LE. 1 .OR. UPVTYP(DSP) .EQ. 0 .OR. (CURIYR + 1 + UHBSYR + YRADJ) .LE. UPDHRY0(DSP))THEN
               IF (EPPHRT0(DSP) .GT. 6000.0) THEN
                  UPHTRT(DSP) = EPPHRT0(DSP)
               END IF
            ELSE

!              OTHERWISE, DETERMINE IMPROVEMENT IN HEATRATE

               IF((CURIYR + 1 + UHBSYR + YRADJ) .LE. UPDHRYN(DSP))THEN
                  YRDIFF = FLOAT((CURIYR + 1 + UHBSYR + YRADJ) - UPDHRY0(DSP))
                  YRSPAN = FLOAT(UPDHRYN(DSP) - UPDHRY0(DSP))
                  UPHTRT(DSP) = EPPHRT0(DSP) + (EPPHRTN(DSP) - EPPHRT0(DSP)) * YRDIFF / YRSPAN
               ELSE
                  UPHTRT(DSP) = EPPHRTN(DSP)
               END IF
            END IF
         END DO
      END IF

      RETURN
      END


      SUBROUTINE ELOVWRT

      IMPLICIT NONE

!     THIS SUBROUTINE OVERWRITES CAPITAL COSTS, HEAT RATES, AND O&M COSTS IF USING ALTERNATIVE ASSUMPTIONS

      include'parametr'
      include'emmparm'
      include'ncntrl'
      include'control'
      include'ecpcntl'
      include'entcntl'
      include'enewtech'
      include'bildin'
      include'uefdout'
      INTEGER DSP,DGN,INT,RNW,NERC,STO,OVYR
!
!   CAPITAL COSTS (ALSO OVERWRITE LEARNING AND TECHNOLOGICAL OPTIMISM)
!    this version backs out the current year ANNADJ factor, and allows ECP to use and see foresight on UPANNADJ to be more consistent with reference case foresight
!    when using the overwrite structure. it may still need improvements to reflect actual foresight of the overwrite values
!
!   LOOP OVER REGIONS AND GET REGIONAL DATA
      DO NERC = 1 , UNRGNS
        CALL GETBLD(1,NERC)
!   DISPATCHABLE
        DO DSP = 1 , ECP_D_DSP
           IF(ECP_D_OVCC(UCPDSPI(DSP)).GT.0)THEN
              UPOVR(UCPDSPI(DSP)) = UPOVCC(UCPDSPI(DSP),MIN(MNUMYR,CURIYR+UPPLYR(UCPDSPI(DSP)))) * (1.0 - UPCSB(UCPDSPI(DSP))) / UPANNADJ(UCPDSPI(DSP),MIN(MNUMYR,CURIYR+UPPLYR(UCPDSPI(DSP))))
              UPLROPT(UCPDSPI(DSP)) = 1.0
              UPLRLC(UCPDSPI(DSP)) = 1.0
!             DO OVYR = 1, MNUMYR
!                UPANNADJ(UCPDSPI(DSP),OVYR) = 1.0
!             ENDDO
           END IF
        END DO
!   INTERMITTENT
        DO INT = 1 , ECP_D_INT
           IF(ECP_D_OVCC(UCPINTI(INT)).GT.0)THEN
              EPIROVR(UIRINTI(INT)) = UPOVCC(UCPINTI(INT),MIN(MNUMYR,CURIYR+UPPLYR(UCPINTI(INT)))) * (1.0 - UPCSB(UCPINTI(INT))) / UPANNADJ(UCPINTI(INT),MIN(MNUMYR,CURIYR+UPPLYR(UCPINTI(INT))))
              UPLROPT(UCPINTI(INT)) = 1.0
              UPLRLC(UCPINTI(INT)) = 1.0
!             DO OVYR = 1, MNUMYR
!                UPANNADJ(UCPINTI(INT),OVYR) = 1.0
!             ENDDO
           END IF
        END DO
!   STORAGE
        DO STO = 1 , ECP_D_STO
           IF (UCPSTOI(STO) .GT. 0) THEN
              IF(ECP_D_OVCC(UCPSTOI(STO)).GT.0)THEN
                 UPOVR(UCPSTOI(STO)) = UPOVCC(UCPSTOI(STO),MIN(MNUMYR,CURIYR+UPPLYR(UCPSTOI(STO)))) * (1.0 - UPCSB(UCPSTOI(STO))) / UPANNADJ(UCPSTOI(STO),MIN(MNUMYR,CURIYR+UPPLYR(UCPSTOI(STO))))
                 UPLROPT(UCPSTOI(STO)) = 1.0
                 UPLRLC(UCPSTOI(STO)) = 1.0
!                DO OVYR = 1, MNUMYR
!                   UPANNADJ(UCPSTOI(STO),OVYR) = 1.0
!                ENDDO
              END IF
           END IF
        END DO
!   RENEWABLE
        DO RNW = 1 , ECP_D_RNW
           IF (UCPRNWI(RNW) .GT. 0) THEN
              IF(ECP_D_OVCC(UCPRNWI(RNW)).GT.0)THEN
                 EPIROVR(UIRRNWI(RNW)) = UPOVCC(UCPRNWI(RNW),MIN(MNUMYR,CURIYR+UPPLYR(UCPRNWI(RNW)))) * (1.0 - UPCSB(UCPRNWI(RNW))) / UPANNADJ(UCPRNWI(RNW),MIN(MNUMYR,CURIYR+UPPLYR(UCPRNWI(RNW))))
                 UPLROPT(UCPRNWI(RNW)) = 1.0
                 UPLRLC(UCPRNWI(RNW)) = 1.0
!                DO OVYR = 1, MNUMYR
!                   UPANNADJ(UCPRNWI(RNW),OVYR) = 1.0
!                ENDDO
              END IF
           END IF
        END DO
!   DISTRIBUTED GENERATION
        DO DGN = 1 , ECP_D_DGN
           IF(ECP_D_OVCC(UCPDGNI(DGN)).GT.0)THEN
              UPOVR(UCPDGNI(DGN)) = UPOVCC(UCPDGNI(DGN),MIN(MNUMYR,CURIYR+UPPLYR(UCPDGNI(DGN)))) * (1.0 - UPCSB(UCPDGNI(DGN))) / UPANNADJ(UCPDGNI(DGN),MIN(MNUMYR,CURIYR+UPPLYR(UCPDGNI(DGN))))
              UPLROPT(UCPDGNI(DGN)) = 1.0
              UPLRLC(UCPDGNI(DGN)) = 1.0
!             DO OVYR = 1, MNUMYR
!                UPANNADJ(UCPDGNI(DGN),OVYR) = 1.0
!             ENDDO
           END IF
        END DO
!
!   HEAT RATES
!   DISPATCHABLE
       DO DSP = 1 , ECP_D_DSP
         IF(ECP_D_OVHR(UCPDSPI(DSP)).GT.0)THEN
            UPHTRT(UCPDSPI(DSP)) = UPOVHR(UCPDSPI(DSP),MIN(MNUMYR,CURIYR+UPPLYR(UCPDSPI(DSP))))
         END IF
       END DO
!   INTERMITTENT
       DO INT = 1 , ECP_D_INT
         IF(ECP_D_OVHR(UCPINTI(INT)).GT.0)THEN
            UPHTRT(UCPINTI(INT)) = UPOVHR(UCPINTI(INT),MIN(MNUMYR,CURIYR+UPPLYR(UCPINTI(INT))))
         END IF
       END DO
!   RENEWABLE
       DO RNW = 1 , ECP_D_RNW
         IF (UCPRNWI(RNW) .GT. 0) THEN
            IF(ECP_D_OVHR(UCPRNWI(RNW)).GT.0)THEN
               UPHTRT(UCPRNWI(RNW)) = UPOVHR(UCPRNWI(RNW),MIN(MNUMYR,CURIYR+UPPLYR(UCPRNWI(RNW))))
            END IF
         END IF
       END DO
!   DISTRIBUTED GENERATION
       DO DGN = 1 , ECP_D_DGN
         IF(ECP_D_OVHR(UCPDGNI(DGN)).GT.0)THEN
            UPHTRT(UCPDGNI(DGN)) = UPOVHR(UCPDGNI(DGN),MIN(MNUMYR,CURIYR+UPPLYR(UCPDGNI(DGN))))
         END IF
       END DO
!
!   FIXED O&M
!   DISPATCHABLE
        DO DSP = 1 , ECP_D_DSP
           IF(ECP_D_OVFX(UCPDSPI(DSP)).GT.0)THEN
              EPFOM(UCPDSPI(DSP)) = UPOVFX(UCPDSPI(DSP),MIN(MNUMYR,CURIYR+UPPLYR(UCPDSPI(DSP))))
           END IF
        END DO
!   INTERMITTENT
        DO INT = 1 , ECP_D_INT
           IF(ECP_D_OVFX(UCPINTI(INT)).GT.0)THEN
              EPIRFOM(UIRINTI(INT)) = UPOVFX(UCPINTI(INT),MIN(MNUMYR,CURIYR+UPPLYR(UCPINTI(INT))))
           END IF
        END DO
!   STORAGE
        DO STO = 1 , ECP_D_STO
           IF (UCPSTOI(STO) .GT. 0) THEN
              IF(ECP_D_OVFX(UCPSTOI(STO)).GT.0)THEN
                 EPFOM(UCPSTOI(STO)) = UPOVFX(UCPSTOI(STO),MIN(MNUMYR,CURIYR+UPPLYR(UCPSTOI(STO))))
              END IF
           END IF
        END DO
!   RENEWABLE
        DO RNW = 1 , ECP_D_RNW
           IF (UCPRNWI(RNW) .GT. 0) THEN
              IF(ECP_D_OVFX(UCPRNWI(RNW)).GT.0)THEN
                 EPIRFOM(UIRRNWI(RNW)) = UPOVFX(UCPRNWI(RNW),MIN(MNUMYR,CURIYR+UPPLYR(UCPRNWI(RNW))))
              END IF
           END IF
        END DO
!   DISTRIBUTED GENERATION
        DO DGN = 1 , ECP_D_DGN
           IF(ECP_D_OVFX(UCPDGNI(DGN)).GT.0)THEN
              EPFOM(UCPDGNI(DGN)) = UPOVFX(UCPDGNI(DGN),MIN(MNUMYR,CURIYR+UPPLYR(UCPDGNI(DGN))))
           END IF
        END DO
!
!   VARIABLE O&M
!   DISPATCHABLE
        DO DSP = 1 , ECP_D_DSP
           IF(ECP_D_OVVR(UCPDSPI(DSP)).GT.0)THEN
              EPVOM(UCPDSPI(DSP)) = UPOVVR(UCPDSPI(DSP),MIN(MNUMYR,CURIYR+UPPLYR(UCPDSPI(DSP))))
           END IF
        END DO
!   INTERMITTENT
        DO INT = 1 , ECP_D_INT
           IF(ECP_D_OVVR(UCPINTI(INT)).GT.0)THEN
              EPIRVOM(UIRINTI(INT)) = UPOVVR(UCPINTI(INT),MIN(MNUMYR,CURIYR+UPPLYR(UCPINTI(INT))))
           END IF
        END DO
!   STORAGE
        DO STO = 1 , ECP_D_STO
           IF (UCPSTOI(STO) .GT. 0) THEN
              IF(ECP_D_OVVR(UCPSTOI(STO)).GT.0)THEN
                 EPVOM(UCPSTOI(STO)) = UPOVVR(UCPSTOI(STO),MIN(MNUMYR,CURIYR+UPPLYR(UCPSTOI(STO))))
              END IF
           END IF
        END DO
!   RENEWABLE
        DO RNW = 1 , ECP_D_RNW
           IF (UCPRNWI(RNW) .GT. 0) THEN
              IF(ECP_D_OVVR(UCPRNWI(RNW)).GT.0)THEN
                 EPIRVOM(UIRRNWI(RNW)) = UPOVVR(UCPRNWI(RNW),MIN(MNUMYR,CURIYR+UPPLYR(UCPRNWI(RNW))))
              END IF
           END IF
        END DO
!   DISTRIBUTED GENERATION
        DO DGN = 1 , ECP_D_DGN
           IF(ECP_D_OVVR(UCPDGNI(DGN)).GT.0)THEN
              EPVOM(UCPDGNI(DGN)) = UPOVVR(UCPDGNI(DGN),MIN(MNUMYR,CURIYR+UPPLYR(UCPDGNI(DGN))))
           END IF
        END DO
        CALL STRBLD(1,NERC)
      END DO

      RETURN
      END


      SUBROUTINE ELRSKPR

      IMPLICIT NONE

!     THIS SUBROUTINE ASSIGNS THE RISK PREMIUMS FOR TECHNOLOGIES

      include'parametr'
      include'emmparm'
      include'ncntrl'
      include'control'
      include'ecpcntl'
      include'entcntl'
      include'bildin'
      INTEGER RSKPTS
      PARAMETER(RSKPTS = 4)
      INTEGER PLT,PTS,PT1,CLT,RSKLT(RSKPTS)
      REAL RSKCLT(RSKPTS),RSKPRM(10)
!     INPUT RISK FACTORS FOR DISCRETE LEAD TIMES FROM J. HEWLETT, 7/95
      DATA RSKLT/0,3,4,7/
      DATA RSKCLT/0.000,0.0075,0.0120,0.0330/
      PRINT *,'ECP_D_RSK',ECP_D_RSK
!
!     INITIALIZE RISK PREMIUMS TO ZERO
!
!     LOOP OVER ALL PLANT TYPES
      DO PLT = 1 , ECP_D_CAP
         UPRSK(PLT) = 0.0
      END DO
!     RETROFITS
      UPSRSK = 0.0
!
!     IF RISK SWITCH IS ON, DETERMINE RISK PREMIUMS AS A FUNCTION OF LEAD TIME
!     INTERPOLATION IS USED FOR LEAD TIMES BETWEEN POINTS
      IF (ECP_D_RSK .GT. 0) THEN
         DO PTS = 2 , RSKPTS
            DO PT1 = RSKLT(PTS - 1) + 1 , RSKLT(PTS)
               RSKPRM(PT1) = RSKCLT(PTS - 1) + &
                (RSKCLT(PTS) - RSKCLT(PTS - 1)) * &
                FLOAT(PT1 - RSKLT(PTS - 1)) / &
                FLOAT(RSKLT(PTS) - RSKLT(PTS - 1))
            END DO
!           EXTRAPOLATE BEYOND LAST POINT USING THE LAST TWO DATA POINTS
            IF(PTS .EQ. RSKPTS)THEN
               DO PT1 = RSKLT(PTS) + 1 , 10
                  RSKPRM(PT1) = RSKCLT(PTS - 1) + &
                   (RSKCLT(PTS) - RSKCLT(PTS - 1)) * &
                   FLOAT(PT1 - RSKLT(PTS - 1)) / &
                   FLOAT(RSKLT(PTS) - RSKLT(PTS - 1))
               END DO
            END IF
         END DO
!        CALCULATE RISK PREMIUMS FOR DISPATCHABLE PLANT TYPES
         DO PLT = 1 , ECP_D_DSP
            CLT = UPCLYR(UCPDSPI(PLT)) - 1
            IF(CLT .LE. 0)THEN
               UPRSK(UCPDSPI(PLT)) = 0.0
!              EXCEPT FOR COAL AND NUCLEAR PLANTS, WHICH HAVE COMPRESSED LEADTIMES,
!              USE INPUT CONSTRUCTION LEAD TIME (LESS ONE YEAR FOR LICENSING)
            ELSE IF(UPLNTCD(UCPDSPI(PLT)).NE.'CN'.AND. &
             UPLNTCD(UCPDSPI(PLT)).NE.'AN')THEN
!           ELSE
               UPRSK(UCPDSPI(PLT)) = RSKPRM(CLT)
            ELSE
!              UPRSK(UCPDSPI(PLT)) = RSKPRM(UPCLYR(UCPDSPI(PLT)) + 1)
               UPRSK(UCPDSPI(PLT)) = RSKPRM(UPCLYR(UCPDSPI(PLT)))
            END IF
         END DO
!        CALCULATE RISK PREMIUMS FOR INTERMITTENT PLANT TYPES
         DO PLT = 1 , ECP_D_INT
            CLT = UPCLYR(UCPINTI(PLT)) - 1
            IF(CLT .LE. 0)THEN
               UPRSK(UCPINTI(PLT)) = 0.0
            ELSE
               UPRSK(UCPINTI(PLT)) = RSKPRM(CLT)
            END IF
         END DO
!        CALCULATE RISK PREMIUMS FOR RENEWABLE PLANT TYPES
         DO PLT = 1 , ECP_D_RNW
            IF (UCPRNWI(PLT) .GT. 0) THEN
               CLT = UPCLYR(UCPRNWI(PLT)) - 1
               IF(CLT .LE. 0)THEN
                  UPRSK(UCPRNWI(PLT)) = 0.0
               ELSE
                  UPRSK(UCPRNWI(PLT)) = RSKPRM(CLT)
               END IF
            END IF
         END DO
!        CALCULATE RISK PREMIUMS FOR DISTRIBUTED GENERATION
         DO PLT = 1 , ECP_D_DGN
            CLT = UPCLYR(UCPDGNI(PLT)) - 1
            IF(CLT .LE. 0)THEN
               UPRSK(UCPDGNI(PLT)) = 0.0
            ELSE
               UPRSK(UCPDGNI(PLT)) = RSKPRM(CLT)
            END IF
         END DO
!        ASSUME 2-YEAR LEAD TIME FOR RETROFITS
         UPSRSK = RSKPRM(2)
      END IF
      RETURN
      END


      SUBROUTINE EPO$TBLD
      use ecp_row_col   ! this is created when you compile ecp_row_col.f90.

      IMPLICIT NONE

!     THIS SUBROUTINE ALLOCATES NATIONAL LEVEL BOUNDS TO REGIONS
!     IT USES "RELATIVE" COMPETITIVENESS OF A TECHNOLOGY ACROSS THE REGIONS AND TOTAL BUILDS FOR A REGION

      include 'parametr'
      include 'ncntrl'
      include'emmparm'
      include'control'
      include'ecpcntl'
      include 'bildin'
      include 'bildout'
      include 'wrenew'
      include 'emshrin'
      include 'emshrout'
      include 'cdsparms'
      include 'uso2grp'
      include 'uefdout'
      include 'ecp_nuc'
      include'emm_aimms'
!
!      COMMON /VARCOST/ VARCOL,VAROTH,CFCPLT
!      REAL*4 VARCOL(MAXNFR,ECP_D_CAP)
!      REAL*4 VAROTH(MNUMNR,ECP_D_CAP)
!      REAL*4 CFCPLT(MNUMNR,ECP_D_CAP)
!
      INTEGER*4 NERC,YEAR,STYR,OLYR,OLYEAR,OWN,IRET,IS
      INTEGER*4 PLANT,IECP
      INTEGER*4 EXPORT(MNUMNR),NREG(MNUMNR),EXP,NRG,IRG,JRG,K
      INTEGER*4 CLT(ECP_D_CAP)
      INTEGER*4 STEP,NSTEPS(ECP_D_CAP),JSTEP
      INTEGER*4 CLRG,TRG,T_RG
      INTEGER*4 FLRG,FRG,NXT_FLRG(MNUMNR),N_FLRG,GSRG,CSRG
      INTEGER*4 OLDSHR                                  ! SWITCH TO SKIP OLD NAT. TO. REG SHARING
      REAL*8 TOTBLD(MNUMNR,ECP_D_XPH)
      REAL*8 CVALUE(5),RVALUE(5),ACT,OBJ,LL,UL,RC,AVLBLD
      REAL*8 CSTRATIO
      REAL*8 TOTRATIO
      REAL*4 VAROBJ
      CHARACTER*1 SSTEP(ECP_D_DGS)
      CHARACTER*16 BUILDNM,ROWNAME
      CHARACTER*2 PLNT_CD,LIM_TYP
      CHARACTER*2 STATUS
      CHARACTER*1 BLD_TYP
      CHARACTER*3 PLT_TYP, rowname_mask*30

      ecpsub='EPO$TBLD'
!
!     TURN OFF OLD NATIONAL TO REGIONAL SHARING METHOD
!
         OLDSHR = 0
!
      DO IS = 1 , ECP_D_DGS
         SSTEP(IS) = UPRGCD(IS)
      END DO
!
!     INITIALIZE LIMITS
!
      DO NERC = 1 , UNRGNS
         DO IRG = 1 , ECP_D_MXP
            DO PLANT = 1 , ECP_D_CAP
               DO STEP = 1 , ECP_D_DGS
                  DO OWN = 1 , ECP_D_OWN
                     DO YEAR = 1 , ECP_D_XPH
                        EPMSLIM(NERC,IRG,PLANT,STEP,OWN,YEAR) = 0.0

!                       INITIALIZE NATIONAL BOUND HIGH

                        IF(NERC .EQ. 1 .AND. IRG .EQ. 1 .AND. OWN .EQ. 1)EPMSLIM(MNUMNR,IRG,PLANT,STEP,OWN,YEAR) = 9999.9
                     END DO
                  END DO
               END DO
            END DO
         END DO
      END DO
!
!     RETRIEVE TOTAL BUILDS FROM FREE ROW
!
      DO YEAR = 1 , UNXPH
         DO NERC = 1 , UNRGNS
            ROWNAME = 'TOTBLD' // UPRGCD(NERC) // UPYRCD(YEAR); call makmsk(ROWNAME_mask,':TOTBLD:' , UPRGCD(NERC) , UPYRCD(YEAR))
            CALL CWFSROW(ROWNAME,'A       ',STATUS,RVALUE,ROWNAME_mask,IRET)
            TOTBLD(NERC,YEAR) = RVALUE(1)
         END DO
      END DO

      DO PLANT = 1 , ECP_D_CAP
         IF (UPVTYP(PLANT) .GT. 0) THEN
!
!           GET PLANT CODE AND CONSTRUCTION LEAD TIME FOR DISPATCHABLE TECHNOLOGIES
!
            IF (UCPDSPIS(PLANT) .GT. 0) THEN
               IECP = PLANT
               PLNT_CD = UPLNTCD(UCPDSPI(IECP))
               BLD_TYP = 'B'
               PLT_TYP = 'DSP'
               LIM_TYP = 'DS'
               CLT(PLANT) = UPPLYR(UCPDSPI(IECP))
               NSTEPS(PLANT) = MAX(1,ESTSWTCH(PLANT))

!              GET PLANT CODE AND CONSTRUCTION LEAD TIME FOR STORAGE TECHNOLOGIES

            ELSE IF (UCPSTOIS(PLANT) .GT. 0) THEN
!
               IECP = UCPSTOIS(PLANT)
               PLNT_CD = UPLNTCD(PLANT)
               BLD_TYP = 'I'
               PLT_TYP = 'STO'
               LIM_TYP = 'ST'
               CLT(PLANT) = UPPLYR(PLANT)
               NSTEPS(PLANT) = 1
!
!           GET PLANT CODE AND CONSTRUCTION LEAD TIME FOR INTERMITTENT TECHNOLOGIES
!
            ELSE IF (UCPINTIS(PLANT) .GT. 0) THEN
!
               IECP = UCPINTIS(PLANT)
               PLNT_CD = UPLNTCD(UCPINTI(IECP))
               BLD_TYP = 'I'
               PLT_TYP = 'INT'
               LIM_TYP = 'IN'
               CLT(PLANT) = UPPLYR(UCPINTI(IECP))
               NSTEPS(PLANT) = EPSTSUP(UIRINTI(IECP))
!
!           GET PLANT CODE AND CONSTRUCTION LEAD TIME FOR RENEWABLE TECHNOLOGIES
!
            ELSE IF (UCPRNWIS(PLANT) .GT. 0) THEN
!
               IECP = UCPRNWIS(PLANT)
               PLNT_CD = UPLNTCD(UCPRNWI(IECP))
               BLD_TYP = 'B'
               PLT_TYP = 'REN'
               LIM_TYP = 'RN'
               CLT(PLANT) = UPPLYR(UCPRNWI(IECP))
               NSTEPS(PLANT) = EPSTSUP(UIRRNWI(IECP))
!
!           GET PLANT CODE AND CONSTRUCTION LEAD TIME FOR DISTRIBUTED GENERATION TECHNOLOGIES
!
            ELSE IF (UCPDGNIS(PLANT) .GT. 0) THEN
!
               IECP = UCPDGNIS(PLANT)
               PLNT_CD = UPLNTCD(UCPDGNI(IECP))
               BLD_TYP = 'B'
               PLT_TYP = 'DGN'
               LIM_TYP = 'DG'
               CLT(PLANT) = UPPLYR(UCPDGNI(IECP))
               NSTEPS(PLANT) = ECP_D_DGS
            END IF
!
!           LOOP OVER YEARS
!
            DO STYR = 1 , UNXPH - 1
               OLYR = STYR + CLT(PLANT)
               OLYEAR = UHBSYR + CURIYR + OLYR - 1
               IF (OLYR .LT. UNXPH .AND. OLYEAR .GE. UPAVLYR(PLANT)) THEN
!
!                 CHECK IF NATIONAL LEVEL CONSTRAINT EXISTS FOR TECHNOLOGY
!
                  DO STEP = 1 , NSTEPS(PLANT)
                     JSTEP = STEP
                     IF (UPTTYP(PLANT) .LE. NW_COAL) JSTEP = 1
                     TOTRATIO = 0.0
                     ROWNAME = 'L' // 'U' // PLNT_CD // LIM_TYP // SSTEP(JSTEP) // UPYRCD(STYR); call makmsk(ROWNAME_mask,':LU:' , PLNT_CD ,LIM_TYP , SSTEP(JSTEP) , UPYRCD(STYR))
                     CALL CWFSROW(ROWNAME,'AU      ',STATUS,RVALUE,ROWNAME_mask,IRET)
!
!                    IF NATIONAL LIMIT THEN, DISTRIBUTE AMONG REGIONS
!                    OTHERWISE, SET TO UNLIMITED FOR EACH REGION/TYPE ETC
!
                     DO NERC = 1 , UNRGNS
                        DO IRG = 1 , ECP_D_MXP
                           DO OWN = 1 , ECP_D_OWN
                              IF (STEP .LE. ECP_D_DGS) EPMSLIM(NERC,IRG,PLANT,STEP,OWN,STYR) = 9999.9
                           END DO
                        END DO
                     END DO
!
                     IF (IRET .LE. 0) THEN
                        AVLBLD = RVALUE(2) - RVALUE(1)

!                       STORE AVAILABLE BUILDS FOR MARKET-SHARE BY PLANT AND STEP

                        EPMSLIM(MNUMNR,1,PLANT,STEP,1,STYR) = AVLBLD

!                       SKIP OVER ALLOCATION OF NATIONAL TO REGIONAL LIMITS

                        IF (OLDSHR .GT. 0) THEN
!
!                          LOOP OVER REGIONS
!
                           DO NERC = 1 , UNRGNS
!
!                             DETERMINE EXPORT REGIONS (1 -> EXPORT = IMPORT REGION)

                              IF ((PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) .OR. PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                                 N_FLRG = 0
                                 DO FRG = 1 , UNFRGN
                                    IF (FL_CNXT_CST(NERC,FRG) .GT. 0.0) THEN
                                       N_FLRG = N_FLRG + 1
                                       NXT_FLRG(N_FLRG) = FRG
                                    END IF
                                 END DO
                              ELSE
                                 N_FLRG = 1
                                 NXT_FLRG(1) = NERC
                              END IF

                              DO FRG = 1 , N_FLRG
                                 FLRG = NXT_FLRG(FRG)
                                 CLRG = EPCLMP(FRG)
                                 GSRG = EPGSMP(FRG)
                                 CSRG = EPCSMP(FRG)
                                 IF ((PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) .OR. PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                                    JRG = 1
                                 ELSE
                                    JRG = NRG
                                 END IF
                                 NRG = 1
                                 EXPORT(1) = NERC
                                 IF (PLANT .GT. ECP_D_DSP .OR. PLANT .EQ. WICN .OR. PLANT .EQ. WIAN .OR. PLANT .EQ. WISM) THEN
                                    DO IRG = 1 , UNRGNS
                                       CALL GETBLD(1,IRG)
                                       K = 1
                                       DO WHILE (EPTIRGN(IRG,K) .GT. 0)
                                          IF (EPTIRGN(IRG,K) .EQ. NERC .AND. EPTCST(IRG,K) .LT. 9999.0 .AND. EPTCST(IRG,K) .GT. 0.) THEN
                                             NRG = NRG + 1
                                             EXPORT(NRG) = IRG
                                          END IF
                                          K = K + 1
                                       END DO
                                    END DO
                                 END IF

                                 CALL GETBLD(1,NERC)
!
                                 NREG(NERC) = NRG
!
!                                LOOP OVER REGION/EXPORT
!
                                 DO IRG = 1 , NREG(NERC)
!
                                    IF ((PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) .OR. PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                                       TRG = FRG
                                       T_RG = FLRG
                                    ELSE
                                       TRG = IRG
                                       T_RG = EXP
                                    END IF
!
                                    EXP = EXPORT(IRG)
                                    DO OWN = 1 , ECP_D_OWN
                                     IF (UPBLDTYP(NERC) .EQ. OWN .OR. UPBLDTYP(NERC) .EQ. 3)THEN
                                       VAROBJ = 0.0
                                       IF (IRG .EQ. 1) THEN
                                          IF (PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) THEN
                                             BUILDNM = BLD_TYP // UPRGCD(NERC) // PLNT_CD // UPOWNCD(OWN) // EPFLCD(FLRG) // SSTEP(STEP) // UPYRCD(STYR); call makmsk(BUILDNM_mask,':'//BLD_TYP//':' , UPRGCD(NERC) , PLNT_CD , UPOWNCD(OWN) , EPFLCD(FLRG) , SSTEP(STEP) , UPYRCD(STYR))
                                             VAROBJ = VARCOL(FLRG,PLANT) * CFCPLT(NERC,PLANT) * 8.76
                                          ELSE IF (PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                                             BUILDNM = BLD_TYP // UPRGCD(NERC) // PLNT_CD // UPOWNCD(OWN) // EPFLCD(FLRG) // SSTEP(STEP) // UPYRCD(STYR); call makmsk(BUILDNM_mask,':'//BLD_TYP//':' , UPRGCD(NERC) , PLNT_CD , UPOWNCD(OWN) , EPFLCD(FLRG) , SSTEP(STEP) , UPYRCD(STYR))
                                             VAROBJ = VARCOL(FLRG,PLANT) * CFCPLT(NERC,PLANT) * 8.76
                                          ELSE
                                             BUILDNM = BLD_TYP // UPRGCD(NERC) // PLNT_CD // UPOWNCD(OWN) // 'X' // SSTEP(STEP) // UPYRCD(STYR); call makmsk(BUILDNM_mask,':'//BLD_TYP//':' , UPRGCD(NERC) , PLNT_CD , UPOWNCD(OWN) , ':X:' , SSTEP(STEP) , UPYRCD(STYR))
                                             VAROBJ = VAROTH(NERC,PLANT) * CFCPLT(NERC,PLANT) * 8.76
                                          END IF
                                       ELSE
                                          BUILDNM = BLD_TYP // UPRGCD(EXP) // PLNT_CD // UPOWNCD(OWN) // UPRGCD(NERC) // SSTEP(STEP) // UPYRCD(STYR); call makmsk(BUILDNM_mask,':'//BLD_TYP//':' , UPRGCD(EXP) , PLNT_CD , UPOWNCD(OWN) , UPRGCD(NERC) , SSTEP(STEP) , UPYRCD(STYR))
                                          VAROBJ = VAROTH(NERC,PLANT) * CFCPLT(NERC,PLANT) * 8.76
                                       END IF
!
                                       CALL CWFSCOL(BUILDNM,'ACLUD   ',STATUS, CVALUE,BUILDNM_mask,IRET)
!
!                                      RETRIEVE SOLUTION INFORMATION FOR BUILD VECTORS
!
                                       IF (IRET .EQ. 0) THEN
                                          ACT = CVALUE(1)
                                          OBJ = CVALUE(2)
                                          LL = CVALUE(3)
                                          UL = CVALUE(4)
                                          RC = CVALUE(5)
!
!                                         IDENTIFY IF NON-BASIC AND ELIGIBLE FOR MARKET-SHARING
!
                                          IF (RC .GT. 0) THEN
                                             CSTRATIO = (OBJ + DBLE(VAROBJ) - RC) / (OBJ + DBLE(VAROBJ))
!
                                             IF (CSTRATIO .LT. (1.0 - EPMSTOL)) THEN
                                                EPMSLIM(NERC,TRG,PLANT,JSTEP,OWN,STYR) = DBLE(0.0)
                                             ELSE
                                                CSTRATIO = CSTRATIO ** EPMSEXP
                                                EPMSLIM(NERC,TRG,PLANT,JSTEP,OWN,STYR) = CSTRATIO * TOTBLD(NERC,OLYR)
                                                TOTRATIO = TOTRATIO +  CSTRATIO * TOTBLD(NERC,OLYR)
                                             END IF
!
!                                            FOR BASIC VECTORS (OR NONBASIC AT BOUND), SET LIMIT TO - LP VALUE
!
                                          ELSE
                                             EPMSLIM(NERC,TRG,PLANT,STEP,OWN,STYR) = - ACT
                                          END IF
                                       END IF                         ! IRET (COL)
                                     END IF                           ! UPBLDTYP
                                    END DO                            ! OWN
                                 END DO                               ! IRG
                              END DO                                  ! FRG
                           END DO                                     ! NERC
!
!                          RELOOP OVER REGIONS AND DISTRIBUTE NATIONAL LIMIT BASED ON REDUCED COST AND TOTAL BUILDS BY REGION
!
                           DO NERC = 1 , UNRGNS

                              IF ((PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) .OR. PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                                 N_FLRG = 0
                                 DO FRG = 1 , UNFRGN
                                    IF (FL_CNXT_CST(NERC,FRG) .GT. 0.0) THEN
                                       N_FLRG = N_FLRG + 1
                                       NXT_FLRG(N_FLRG) = FRG
                                    END IF
                                 END DO
                              ELSE
                                 N_FLRG = 1
                                 NXT_FLRG(1) = NERC
                              END IF

                              DO FRG = 1 , N_FLRG
                                 FLRG = NXT_FLRG(FRG)
                                 CLRG = EPCLMP(FRG)
                                 GSRG = EPGSMP(FRG)
                                 CSRG = EPCSMP(FRG)
                                 DO IRG = 1 , NREG(NERC)
                                    IF ((PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) .OR. PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                                       TRG = FRG
                                       T_RG = FLRG
                                    ELSE
                                       TRG = IRG
                                       T_RG = EXP
                                    END IF
!
                                    DO OWN = 1 , ECP_D_OWN
                                       IF (EPMSLIM(NERC,TRG,PLANT,STEP,OWN,STYR) .LT. 9999.9) THEN
                                          IF (EPMSLIM(NERC,TRG,PLANT,STEP,OWN,STYR) .GE. 0.0) THEN
                                             IF (TOTRATIO .LE. 0.0) THEN
                                                EPMSLIM(NERC,TRG,PLANT,STEP,OWN,STYR) = 0.0
                                             ELSE
                                                EPMSLIM(NERC,TRG,PLANT,STEP,OWN,STYR) = AVLBLD * EPMSLIM(NERC,TRG,PLANT,STEP,OWN,STYR) / TOTRATIO
                                             END IF
                                          ELSE
                                             EPMSLIM(NERC,TRG,PLANT,STEP,OWN,STYR) = - EPMSLIM(NERC,TRG,PLANT,STEP,OWN,STYR)
                                          END IF
                                       END IF
                                    END DO
                                 END DO
                              END DO                      ! FRG
                           END DO
                        END IF                            ! SKIPPING ALLOCATION OF NATIONAL TO REGIONAL LIMITS (OLDSHR)
                     END IF                               ! IRET (ROW)
                  END DO                                  ! STEP
               END IF                                     ! OLYR
            END DO                                        ! STYR
         END IF                                           ! UPVTYP
      END DO                                              ! PLANT
      RETURN
      END


      SUBROUTINE EPO$MSHR(NERC, MSHR_PLT_GRP)
      use ecp_row_col   ! this is created when you compile ecp_row_col.f90.
!
      IMPLICIT NONE

!     THIS SUBROUTINE RETRIEVES SOLUTION INFORMATION FOR BUILD DECISIONS
!     IT REALLOCATES BUILDS AMONG COMPETITIVE TECHNOLOGIES USING REDUCED COSTS AND A MARKET SHARING ALGORITHM

      include 'parametr'
      include 'ncntrl'
      include'emmparm'
      include'control'
      include'ecpcntl'
      include 'bildin'
      include 'bildout'
      include 'wrenew'
      include 'entcntl'
      include 'emshrin'
      include 'emshrout'
      include 'eusprc'
      include 'edbdef'
      include 'cdsparms'
      include 'uso2grp'
      include 'uefdout'
      include 'ecp_nuc'
      include'emm_aimms'
      
!
!      COMMON /VARCOST/ VARCOL,VAROTH,CFCPLT
!      REAL*4 VARCOL(MAXNFR,ECP_D_CAP)
!      REAL*4 VAROTH(MNUMNR,ECP_D_CAP)
!      REAL*4 CFCPLT(MNUMNR,ECP_D_CAP)
!
      INTEGER NUMTABS
      INTEGER, INTENT(IN) :: MSHR_PLT_GRP
      PARAMETER (NUMTABS = 1)        ! total number of database tables in this subroutine
      REAL*8 CVALUE(5),RVALUE(5),ACT,OBJ,LL,UL,RC,TOTBAS,TOTNBS
      REAL*8 SOLDATA(ECP_D_MXP,ECP_D_CAP,ECP_D_DGS,ECP_D_OWN,ECP_D_XPH,5)
      REAL*8 CSTRATIO(ECP_D_MXP,ECP_D_CAP,ECP_D_DGS,ECP_D_OWN,ECP_D_XPH)
      REAL*8 TOTRATIO,MAX_RATIO(ECP_D_OWN)
      REAL*8 MINBLD,REGLMT,NATLMT1,NATLMT2,NATLMT(ECP_D_CAP,ECP_D_XPH)
      REAL*8 EPULPT(ECP_D_CAP,ECP_D_XPH),EPULSCS(ECP_D_CAP,ECP_D_DGS,ECP_D_XPH)
      REAL*8 EPULNU(ECP_D_XPH),MS_BLD
      REAL*8 ESTACT(ECP_D_CAP,ECP_D_DGS),ESTBND(ECP_D_CAP,ECP_D_DGS)
      REAL*4 VAROBJ
      INTEGER*4 NERC,YEAR,STYR,OLYR,FULLYR,OWN,IRET,IOWN
      INTEGER*4 PLANT,TYPE,IECP,ISTP
      INTEGER*4 NUMBAS
      INTEGER*4 EXPORT(MNUMNR),EXP,NRG,IRG,K
      INTEGER*4 CLT(ECP_D_CAP),IS
      INTEGER*4 STEP,NSTEPS(ECP_D_CAP)
      INTEGER*4 CLRG,CRG,IFL,TRG,T_RG
      INTEGER*4 FLRG,FRG,NXT_FLRG(MNUMNR),N_FLRG,GSRG,CSRG
      INTEGER*4 EMSSKIP(ECP_D_CAP,ECP_D_DGS)
      CHARACTER*2 COL
      CHARACTER*2 PLNT_CD,PLT_GRP
      CHARACTER*1 BLD_TYP
      CHARACTER*3 PLT_TYP
      CHARACTER*16 BUILDNM,ROWNAME,ROWNAMS,ROWSCNM,ROWNAMEB
      CHARACTER*2 STATUS
      CHARACTER*2 SOLSTAT(ECP_D_MXP,ECP_D_CAP,ECP_D_DGS,ECP_D_OWN,ECP_D_XPH)
      CHARACTER*1 SSTEP(ECP_D_DGS)
      character*30 ROWSCNM_mask,rowname_mask,ROWNAMEB_mask,ROWNAMS_mask
      INTEGER*4 EXPIND
      CHARACTER*1 SUB_CODE(ECP_D_DGS)
      !INTEGER RTOVALUE ! declared in ecp_row_col.mod, code in uecp.f
      !EXTERNAL RTOVALUE
      
! Overall Idea of this Routine:
!      This routine, uses a cost-competitiveness array by initializing it to negative -1. Then it looks at the basic (Reduced Cost = zero) and non-basic (Reduced Cost > 0) vectors in the solution file. 
!      Where the basic vectors were 'dominant' or 'selected' by the solution, and the non-basic were not selected. The algorithm attempts to take capacity from the basic/selected vectors and donate them to the
!      the non-basic/not-selected vectors. There's some book-keeping here, where it loops over the builds, looks at basic/selected technologies and assigned them a value of 1.0 in CSTRAIO. For the non-basic/non-selected
!      technologies it computes the CSTRAIO by taking the activity - reduce cost to figure out how close/compeitivive it was to being selected  where 1.0 would be selected. It then assigns those near-selected vectors 
!      values between 0 and 1, and then only picks the ones within a threshold (e.g. technologies greater than .8). Meanwhile, it also stores the solution into a temporary array in memory called soldata, where it fetches and tallies 
!      up the amount of capacity during the book-keeping process. Finally, it goes back over the non-basic/not-selected ones, gives capacity and adjusts the upper/lower bounds accordingly, and makes changes to reflect the lost capacity from 
!      the basic/selected vectors (and adjusts their upper and lower bounds too). After all that, it creates a SQL type query, finds where in the EMM database it's updating, and updates it w/ the new values. 
      SUB_CODE(1) = 'S'
      SUB_CODE(2) = 'T'
      SUB_CODE(3) = 'U'
      SUB_CODE(4) = 'V'
      SUB_CODE(5) = 'W'
      SUB_CODE(6) = 'X'
      
      ecpsub='EPO$MSHR'
!
      LOOPING = 0
      NUMCOLS = 0
      DYNSTM = ' '
      WRTSTM = ' '
      COLVALS = 0.0
      COLV = 0.0
      CHCOLVALS = ' '
      CHCOLV = ' '

      ORCLECP = RTOVALUE('ORCLECP ',0)

      DO IS = 1 , ECP_D_DGS
         SSTEP(IS) = UPRGCD(IS)
      END DO
      COL = ' :'
!
!     DETERMINE EXPORT REGIONS (1 -> EXPORT = IMPORT REGION)
!
      NRG = 1
      EXPORT(1) = NERC
      DO IRG = 1 , UNRGNS
         CALL GETBLD(1,IRG)
         K = 1
         DO WHILE (EPTIRGN(IRG,K) .GT. 0)
            IF (EPTIRGN(IRG,K) .EQ. NERC .AND. EPTCST(IRG,K) .LT. 9999.0 .AND. EPTCST(IRG,K) .GT. 0.) THEN
               NRG = NRG + 1
               EXPORT(NRG) = IRG
               !IF (CURIYR+1989 .EQ. 2020) write(*,*) 'EXPORT(NRG)',NERC,K,IRG,EXPORT(NRG)               
            END IF
            K = K + 1
         END DO
      END DO
      CALL GETBLD(1,NERC)
!
!     INITIALIZE TOTALS
!
      TOTRATIO = 0.0
      NUMBAS = 0
      TOTBAS = 0.0
      TOTNBS = 0.0
      DO IRG = 1 , ECP_D_MXP
         DO PLANT = 1, ECP_D_CAP
			IF (UPMSHSW(PLANT) .EQ. MSHR_PLT_GRP) THEN
				DO STEP = 1 , ECP_D_DGS
				   DO OWN = 1 , ECP_D_OWN
					  DO YEAR = 1 , ECP_D_XPH
						 CSTRATIO(IRG,PLANT,STEP,OWN,YEAR) = - 1.0
						 EPMSBLD(IRG,PLANT,STEP,OWN,YEAR) = 0.0
						 SOLSTAT(IRG,PLANT,STEP,OWN,YEAR) = '  '
						 EPULSCS(PLANT,STEP,YEAR) = 9999.9
						 DO TYPE = 1 , 5
							SOLDATA(IRG,PLANT,STEP,OWN,YEAR,TYPE) = 0.0
						 END DO
					  END DO
				   END DO
				END DO
			END IF
         END DO
      END DO
!
!     CHECK TO SEE IF NONUTILITY BUILD CONSTRAINT IS BINDING
!
      DO YEAR = 1 , UNXPH
         ROWNAME = 'P' // UPRGCD(NERC) // 'XXXXX' // UPYRCD(YEAR); call makmsk(ROWNAME_mask,':P:' , UPRGCD(NERC) , ':XXXXX:' , UPYRCD(YEAR))
         CALL CWFSROW(ROWNAME,'PA      ',STATUS,RVALUE,ROWNAME_mask,IRET)
         IF (IRET .EQ. 0) THEN
            EPULNU(YEAR) = RVALUE(1)
         ELSE
            EPULNU(YEAR) = - 100.0
         END IF
      END DO
!
!     LOOP OVER SUPPLY REGIONS ( 1 -> DEMAND = SUPPLY REGION)
!
      DO IRG = 1 , NRG
         EXP = EXPORT(IRG)
!
!        LOOP OVER PLANT TYPES
!
         DO PLANT = 1, ECP_D_CAP
!            IF (UCPINTIS(PLANT) .GT. 0) THEN
!               WRITE(6,4781) CURIRUN, CURIYR+1989, NERC, IRG, EXP, PLANT, UPVTYP(PLANT), UPLNTCD(PLANT), UCPDSPIS(PLANT), UCPSTOIS(PLANT), UCPINTIS(PLANT), UCPRNWIS(PLANT), UCPDGNIS(PLANT), UPAVLYR(PLANT), &
!                  99, UCPINTI(UCPINTIS(PLANT)), 99, 99, 99, UIRINTI(UCPINTIS(PLANT)), 99
!            ELSEIF (UCPSTOIS(PLANT) .GT. 0) THEN
!               WRITE(6,4781) CURIRUN, CURIYR+1989, NERC, IRG, EXP, PLANT, UPVTYP(PLANT), UPLNTCD(PLANT), UCPDSPIS(PLANT), UCPSTOIS(PLANT), UCPINTIS(PLANT), UCPRNWIS(PLANT), UCPDGNIS(PLANT), UPAVLYR(PLANT), &
!                  99, 99, 99,  99, UCPSTOI(UCPSTOIS(PLANT)), 99, 99
!            ELSEIF (UCPRNWIS(PLANT) .GT. 0) THEN
!               WRITE(6,4781) CURIRUN, CURIYR+1989, NERC, IRG, EXP, PLANT, UPVTYP(PLANT), UPLNTCD(PLANT), UCPDSPIS(PLANT), UCPSTOIS(PLANT), UCPINTIS(PLANT), UCPRNWIS(PLANT), UCPDGNIS(PLANT), UPAVLYR(PLANT), &
!                  99, 99, UCPRNWI(UCPRNWIS(PLANT)), 99, 99, 99, UIRRNWI(UCPRNWIS(PLANT))
!            ELSEIF (UCPDGNIS(PLANT) .GT. 0) THEN
!               WRITE(6,4781) CURIRUN, CURIYR+1989, NERC, IRG, EXP, PLANT, UPVTYP(PLANT), UPLNTCD(PLANT), UCPDSPIS(PLANT), UCPSTOIS(PLANT), UCPINTIS(PLANT), UCPRNWIS(PLANT), UCPDGNIS(PLANT), UPAVLYR(PLANT), &
!                  99, 99, 99,  UCPDGNI(UCPDGNIS(PLANT)), 99, 99, 99
!            ELSE
!               WRITE(6,4781) CURIRUN, CURIYR+1989, NERC, IRG, EXP, PLANT, UPVTYP(PLANT), UPLNTCD(PLANT), UCPDSPIS(PLANT), UCPSTOIS(PLANT), UCPINTIS(PLANT), UCPRNWIS(PLANT), UCPDGNIS(PLANT), UPAVLYR(PLANT), &
!                  UCPDSPI(UCPDSPIS(PLANT)), 99, 99, 99, 99, 99, 99
!            END IF
! 4781       FORMAT(1X,"ECP_TYPE_INDEXES",7(":",I6),":",A2,13(":",I6))
            IF (UPMSHSW(PLANT) .EQ. MSHR_PLT_GRP .AND. ((UPVTYP(PLANT) .GT. 0 .AND. PLANT .GT. ECP_D_DSP .AND. PLANT .NE. WIWD .AND. PLANT .NE. WIBI) .OR. (PLANT .EQ. WICN) .OR. (PLANT .EQ. WIAN) .OR. (PLANT .EQ. WISM)  .OR. (UPVTYP(PLANT) .GT. 0 .AND. IRG .EQ.1))) THEN
               IF ((PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) .OR. PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                  N_FLRG = 0
                  DO FRG = 1 , UNFRGN
                     IF (FL_CNXT_CST(NERC,FRG) .GT. 0.0) THEN
                        N_FLRG = N_FLRG + 1
                        NXT_FLRG(N_FLRG) = FRG
                     END IF
                  END DO
               ELSE
                  N_FLRG = 1
                  NXT_FLRG(1) = NERC
               END IF

               DO FRG = 1 , N_FLRG
                  FLRG = NXT_FLRG(FRG)
                  CLRG = EPCLMP(FRG)
                  GSRG = EPGSMP(FRG)
                  CSRG = EPCSMP(FRG)
!
!                 LOOP OVER YEARS
!
                  DO STYR = 1 , UNXPH - 1
!
!                    GET PLANT CODE AND CONSTRUCTION LEAD TIME FOR DISPATCHABLE TECHNOLOGIES
!
                     IF (UCPDSPIS(PLANT) .GT. 0) THEN
                        IECP = PLANT
                        PLNT_CD = UPLNTCD(UCPDSPI(IECP))
                        BLD_TYP = 'B'
                        PLT_TYP = 'DSP'
                        PLT_GRP = 'DS'
                        ROWNAME = 'L' // UPRGCD(NERC) // PLNT_CD // PLT_TYP // UPYRCD(STYR); call makmsk(ROWNAME_mask,':L:' , UPRGCD(NERC) , PLNT_CD , PLT_TYP , UPYRCD(STYR))
                        CLT(PLANT) = UPPLYR(UCPDSPI(IECP))
!
!                       TECHNOLOGY SUPPLY CURVE ROWS
!
                        NSTEPS(PLANT) = MAX(1,ESTSWTCH(PLANT))


!                       GET PLANT CODE AND CONSTRUCTION LEAD TIME FOR STORAGE TECHNOLOGIES

                     ELSE IF (UCPSTOIS(PLANT) .GT. 0) THEN
!
                        IECP = UCPSTOIS(PLANT)
                        PLNT_CD = UPLNTCD(PLANT)
                        BLD_TYP = 'I'
                        PLT_TYP = 'STO'
                        PLT_GRP = 'ST'
                        ROWNAME = 'L' // UPRGCD(NERC) // PLNT_CD // PLT_TYP // UPYRCD(STYR); call makmsk(ROWNAME_mask,':L:' , UPRGCD(NERC) , PLNT_CD , PLT_TYP , UPYRCD(STYR))
                        CLT(PLANT) = UPPLYR(PLANT)

!                       TECHNOLOGY SUPPLY CURVE ROWS

                        NSTEPS(PLANT) = 1


!                       GET PLANT CODE AND CONSTRUCTION LEAD TIME FOR INTERMITTENT TECHNOLOGIES

                     ELSE IF (UCPINTIS(PLANT) .GT. 0) THEN
!
                        IECP = UCPINTIS(PLANT)
                        PLNT_CD = UPLNTCD(UCPINTI(IECP))
                        BLD_TYP = 'I'
                        PLT_TYP = 'INT'
                        PLT_GRP = 'IN'
						IF (PLNT_CD .EQ. 'PT') THEN
							ROWNAME = 'L' // UPRGCD(NERC) // 'PV' // PLT_TYP // UPYRCD(STYR); call makmsk(ROWNAME_mask,':L:' , UPRGCD(NERC) , 'PV', ':'//PLT_TYP//':' , UPYRCD(STYR))
						ELSE
							ROWNAME = 'L' // UPRGCD(NERC) // PLNT_CD // PLT_TYP // UPYRCD(STYR); call makmsk(ROWNAME_mask,':L:' , UPRGCD(NERC) , PLNT_CD, ':'//PLT_TYP//':' , UPYRCD(STYR))
						END IF
                        CLT(PLANT) = UPPLYR(UCPINTI(IECP))
!
!                       TECHNOLOGY SUPPLY CURVE ROWS
!
                        NSTEPS(PLANT) = EPSTSUP(UIRINTI(IECP))

!
!                       GET PLANT CODE AND CONSTRUCTION LEAD TIME FOR RENEWABLE TECHNOLOGIES
!
                     ELSE IF (UCPRNWIS(PLANT) .GT. 0) THEN
!
                        IECP = UCPRNWIS(PLANT)
                        PLNT_CD = UPLNTCD(UCPRNWI(IECP))
                        BLD_TYP = 'B'
                        PLT_TYP = 'REN'
                        PLT_GRP = 'RN'
                        ROWNAME = 'L' // UPRGCD(NERC) // PLNT_CD // PLT_TYP // UPYRCD(STYR); call makmsk(ROWNAME_mask,':L:' , UPRGCD(NERC) , PLNT_CD , ':'//PLT_TYP//':' , UPYRCD(STYR))
                        CLT(PLANT) = UPPLYR(UCPRNWI(IECP))
!
!                       TECHNOLOGY SUPPLY CURVE ROWS
!
                        NSTEPS(PLANT) = EPSTSUP(UIRRNWI(IECP))
!
!                       GET SUPPLY CURVE BOUNDS BY SUPPLY STEP
!
                        IF (EPSWSUP(UIRRNWI(IECP)) .GT. NSTEPS(PLANT) ) THEN
                           DO ISTP = 1 , NSTEPS(PLANT)
                              ROWSCNM = 'L' // UPRGCD(NERC) // PLNT_CD // 'RN' // SSTEP(ISTP) // UPYRCD(STYR); call makmsk(ROWSCNM_mask,':L:' , UPRGCD(NERC) , PLNT_CD , ':RN:' , SSTEP(ISTP) , UPYRCD(STYR))
                              CALL CWFSROW(ROWSCNM,'AU      ',STATUS,RVALUE,ROWSCNM_mask,IRET)
                              IF (IRET .EQ. 0) THEN
                                 EPULSCS(PLANT,ISTP,STYR) = RVALUE(2) - RVALUE(1)
                              END IF
                           ENDDO
                        ENDIF

!
!                       GET PLANT CODE AND CONSTRUCTION LEAD TIME FOR DISTRIBUTED GENERATION TECHNOLOGIES
!
                     ELSE IF (UCPDGNIS(PLANT) .GT. 0) THEN
!
                        IECP = UCPDGNIS(PLANT)
                        PLNT_CD = UPLNTCD(UCPDGNI(IECP))
                        BLD_TYP = 'B'
                        PLT_TYP = 'DGN'
                        PLT_GRP = 'DG'
                        ROWNAME = 'L' // UPRGCD(NERC) // PLNT_CD // PLT_TYP // UPYRCD(STYR); call makmsk(ROWNAME_mask,':L:' , UPRGCD(NERC) , PLNT_CD , PLT_TYP , UPYRCD(STYR))
                        CLT(PLANT) = UPPLYR(UCPDGNI(IECP))
!
!                       SUPPLY CURVE ROWS
!
                        NSTEPS(PLANT) = ECP_D_DGS
 

                     END IF
!
!                    DETERMINE IF THERE IS A CONSTRAINT THAT LIMITS TOTAL BUILDS FOR A TECHNOLOGIES OVER FORECAST HORIZON
!
                     IF (IRG .EQ. 1) THEN
                        EPULPT(PLANT,STYR) = 9999.9
                        CALL CWFSROW(ROWNAME,'AU      ',STATUS,RVALUE,ROWNAME_mask,IRET)
                        IF (IRET .EQ. 0) THEN
                           EPULPT(PLANT,STYR) = RVALUE(2) - RVALUE(1)
                        END IF
                     END IF
!
!                    Determine if regional builds limits used and set epulpt to minimum value  (cga 4/07)
!
                     IF (IRG .EQ. 1) THEN
                       REGLMT = 9999.99
                       IF (UPBLDREG(PLANT,NERC)  .LT. 999.0) THEN
                         ROWNAMEB = 'L' // UPRGCD(NERC) // PLNT_CD // 'BLDX'                  ; call makmsk(ROWNAMEB_mask,':L:' , UPRGCD(NERC) , PLNT_CD , ':BLDX:')
                         CALL CWFSROW(ROWNAMEB,'AU      ',STATUS,RVALUE,ROWNAMEB_mask,IRET)
                         IF (IRET .EQ. 0) THEN
                           REGLMT = RVALUE(2) - RVALUE(1)
                         END IF
                       END IF
                       EPULPT(PLANT,STYR) = MIN(EPULPT(PLANT,STYR),REGLMT)
                     ENDIF
!
!                    Determine if national build limit by technoloy used  (cga 4/07)
!
                     IF (IRG .EQ. 1) THEN
                       NATLMT1 = 9999.99
                       IF (UPBLDREG(PLANT,MNUMNR)  .LT. 999.0) THEN
                          ROWNAMEB = 'L' // 'U' // PLNT_CD // 'BLDX'                ; call makmsk(ROWNAMEB_mask,':LU:' , PLNT_CD , ':BLDX:')
                          CALL CWFSROW(ROWNAMEB,'AU      ',STATUS,RVALUE,ROWNAMEB_mask,IRET)
                          IF (IRET .EQ. 0) THEN
                             NATLMT1 = RVALUE(2) - RVALUE(1)
                          END IF
                       END IF
                     ENDIF
!
!                    Determine if annual national build limit by technoloy used  (cga 4/07)
!
                     IF (IRG .EQ. 1) THEN
                       NATLMT2 = 9999.9
                       ROWNAMEB = 'L' // 'U' // PLNT_CD // 'ANN' // UPYRCD(STYR); call makmsk(ROWNAMEB_mask,':LU:' , PLNT_CD , ':ANN:' , UPYRCD(STYR))
                       CALL CWFSROW(ROWNAMEB,'AU      ',STATUS,RVALUE,ROWNAMEB_mask,IRET)
                       IF (IRET .EQ. 0) THEN
                         NATLMT2 = RVALUE(2) - RVALUE(1)
                       END IF
                     END IF
!
                     NATLMT(PLANT,STYR) = MIN(NATLMT1,NATLMT2)
!
!
!                    FOR EACH TECHNOLOGY; CHECK FOR BUILD VECTOR BEGINNING OPERATION IN YEAR
!
                     OLYR = STYR + CLT(PLANT)
                     FULLYR = USYEAR(CURIYR) + OLYR - 1
                     IF (OLYR .LT. UNXPH .AND. FULLYR .GE. UPAVLYR(PLANT)) THEN
!
!                       WRITE OUT SUPPLY CURVE ACTIVITIES AND BOUNDS
!
                        IF (NERC .EQ. 1 .AND. NSTEPS(PLANT) .GT. 1 .AND. STYR .EQ. 1) THEN
                           DO STEP = 1 , NSTEPS(PLANT)
                              ESTACT(PLANT,STEP) = 0.0
                              ESTBND(PLANT,STEP) = 0.0
                              EMSSKIP(PLANT,STEP) = 0.0
							  IF (PLNT_CD .EQ. 'PT') THEN
								ROWNAMS = 'LU' // 'PV' // PLT_GRP // SSTEP(STEP) // UPYRCD(1); call makmsk(ROWNAMS_mask,':LU:' , 'PV' , PLT_GRP , SSTEP(STEP) , UPYRCD(1))
							  ELSE
								ROWNAMS = 'LU' // PLNT_CD // PLT_GRP // SSTEP(STEP) // UPYRCD(1); call makmsk(ROWNAMS_mask,':LU:' , PLNT_CD , PLT_GRP , SSTEP(STEP) , UPYRCD(1))
							  END IF
                              CALL CWFSROW(ROWNAMS,'AU      ',STATUS,RVALUE,ROWNAMS_mask,IRET)
                              IF (IRET .EQ. 0) THEN
                                 ESTACT(PLANT,STEP) = RVALUE(1)
                                 ESTBND(PLANT,STEP) = RVALUE(2)
                                 IF (STATUS .EQ. 'UL')EMSSKIP(PLANT,STEP) = EMSSKIP(PLANT,STEP) + 1
                              END IF

!                             print *,'!elas',curiyr+uhbsyr,curiyr+uhbsyr+upplyr(PLANT),PLANT,rowname,iret,estbnd(PLANT,step)

                           END DO

                          !IF (ESTBND(PLANT,1) .GT. 0.0)THEN
!                             WRITE(18,1234) CURIYR + UHBSYR,CURIYR + UHBSYR + UPPLYR(PLANT),PLNT_CD,(ESTACT(PLANT,STEP),ESTBND(PLANT,STEP), EMSSKIP(PLANT,STEP),STEP = 1 , 3)
!1234                         FORMAT(1h ,'ELAS',':',I4,':',I4,':',A2,3(':',F8.3,':',F8.3,':',I4))
                          !END IF

                        END IF
!
!                       FOR EACH OWNERSHIP TYPE; IDENTIFY NAME OF BUILD VECTOR
!
!                       LOOP OVER TECHNOLOGY COST CURVE STEPS

 
                        DO STEP = 1 , NSTEPS(PLANT)
                           DO OWN = 1, ECP_D_OWN
                            IF ((UPBLDTYP(NERC) .EQ. OWN .OR. UPBLDTYP(NERC) .EQ. 3) .AND. (BUILD_AVL(PLANT,STYR,NERC,CURIYR) .EQ. 1 .OR. BUILD_AVL(PLANT,STYR,NERC,CURIYR) .EQ. 2))THEN
                            
                              IF (BUILD_AVL(PLANT,STYR,NERC,CURIYR) .EQ. 1)  THEN 
                              ! THESE ARE UNSUB AND DO NOT CHANGE
                                    IF (IRG .EQ. 1) THEN
                                       IF (PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) THEN
                                          BUILDNM = BLD_TYP // UPRGCD(NERC) // PLNT_CD // UPOWNCD(OWN) // EPFLCD(FLRG) // SSTEP(STEP) // UPYRCD(STYR); call makmsk(BUILDNM_mask,':'//BLD_TYP//':' , UPRGCD(NERC) , PLNT_CD , UPOWNCD(OWN) , EPFLCD(FLRG) , SSTEP(STEP) , UPYRCD(STYR))
                                       ELSE IF (PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                                          BUILDNM = BLD_TYP // UPRGCD(NERC) // PLNT_CD // UPOWNCD(OWN) // EPFLCD(FLRG) // SSTEP(STEP) // UPYRCD(STYR); call makmsk(BUILDNM_mask,':'//BLD_TYP//':' , UPRGCD(NERC) , PLNT_CD , UPOWNCD(OWN) , EPFLCD(FLRG) , SSTEP(STEP) , UPYRCD(STYR))
                                       ELSE
                                          BUILDNM = BLD_TYP // UPRGCD(NERC) // PLNT_CD // UPOWNCD(OWN) // 'X' // SSTEP(STEP) // UPYRCD(STYR); call makmsk(BUILDNM_mask,':'//BLD_TYP//':' , UPRGCD(NERC) , PLNT_CD , UPOWNCD(OWN) , ':X:' , SSTEP(STEP) , UPYRCD(STYR))
                                       END IF
                                    ELSE
                                       BUILDNM = BLD_TYP // UPRGCD(EXP) // PLNT_CD // UPOWNCD(OWN) // UPRGCD(NERC) // SSTEP(STEP) // UPYRCD(STYR); call makmsk(BUILDNM_mask,':'//BLD_TYP//':' , UPRGCD(EXP) , PLNT_CD , UPOWNCD(OWN) , UPRGCD(NERC) , SSTEP(STEP) , UPYRCD(STYR))
                                       !write(*,*) 'INTERREGDC',BUILDNM,EXPIND
                                    END IF
                              ELSE IF (BUILD_AVL(PLANT,STYR,NERC,CURIYR) .EQ. 2)  THEN 
                                    ! THESE ARE SUBSIDIES VECTORS TTHAT NEEED TO CHANGE  
                              
                                    IF (IRG .EQ. 1) THEN
                                       IF (PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) THEN
                                          BUILDNM = BLD_TYP // UPRGCD(NERC) // PLNT_CD // UPOWNCD(OWN) // EPFLCD(FLRG) //SUB_CODE(STEP) // UPYRCD(STYR); call makmsk(BUILDNM_mask,':'//BLD_TYP//':' , UPRGCD(NERC) , PLNT_CD , UPOWNCD(OWN) , EPFLCD(FLRG) , SUB_CODE(STEP) , UPYRCD(STYR))
                                       ELSE IF (PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                                          BUILDNM = BLD_TYP // UPRGCD(NERC) // PLNT_CD // UPOWNCD(OWN) // EPFLCD(FLRG) //SUB_CODE(STEP) // UPYRCD(STYR); call makmsk(BUILDNM_mask,':'//BLD_TYP//':' , UPRGCD(NERC) , PLNT_CD , UPOWNCD(OWN) , EPFLCD(FLRG) , SUB_CODE(STEP) , UPYRCD(STYR))
                                       ELSE
                                          BUILDNM = BLD_TYP // UPRGCD(NERC) // PLNT_CD // UPOWNCD(OWN) // 'S' // SSTEP(STEP) // UPYRCD(STYR); call makmsk(BUILDNM_mask,':'//BLD_TYP//':' , UPRGCD(NERC) , PLNT_CD , UPOWNCD(OWN) , ':S:' , SSTEP(STEP) , UPYRCD(STYR))
                                       END IF
                                    ELSE
                                       BUILDNM = BLD_TYP // UPRGCD(EXP) // PLNT_CD // UPOWNCD(OWN) // UPRGCD(NERC) //SUB_CODE(STEP) // UPYRCD(STYR); call makmsk(BUILDNM_mask,':'//BLD_TYP//':' , UPRGCD(EXP) , PLNT_CD , UPOWNCD(OWN) , UPRGCD(NERC) , SUB_CODE(STEP) , UPYRCD(STYR))
                                       !write(*,*) 'INTERREGDC',BUILDNM,EXPIND
                                    END IF
                        
                              END IF
                        
!
                              CALL CWFSCOL(BUILDNM,'ACLUD   ',STATUS, CVALUE,BUILDNM_mask,IRET)
!
!                             RETRIEVE SOLUTION INFORMATION FOR BUILD VECTORS
!
                              IF (IRET .EQ. 0) THEN
!
!                                REQUIRE MINIMUM SIZE FOR NUCS (MAKE 0 IF LESS THAN HALF)
!
                                 IF (PLANT .EQ. WICN .OR. PLANT .EQ. WIAN .OR. PLANT .EQ. WISM) THEN
                                    IF (CVALUE(1) .LT. UPLRMIN(PLANT) * UPMSSIZ(PLANT) * 0.001) THEN

                                       if (cvalue(1) .gt. 0.0) write(18,2323) curiyr+1989,nerc,buildnm,cvalue(1)
 2323                                  format(1h ,'!rnddn',i4,i3,a10,f10.3,'     0.000')

                                       CVALUE(1) = 0.0
                                    END IF
                                 END IF
!
!                                IGNORE ANY BUILDS < 1 MW
!
                                 IF (CVALUE(1) .LT. DBLE(0.001)) THEN
                                    CVALUE(1) = 0.0
                                 END IF

                                 ACT = CVALUE(1)
                                 OBJ = CVALUE(2)
                                 LL = CVALUE(3)
                                 UL = CVALUE(4)
                                 RC = CVALUE(5)

!                                WRITE(18,2310) CURIYR+UHBSYR,NERC,IRG,CRG,FLRG,PLANT,STEP,OWN,STYR,IRET,BUILDNM,PLNT_CD,UPLNTCD(PLANT),STATUS,(CVALUE(TYPE),TYPE = 1, 5)

                                 WRITE(18,2310) CURIYR+UHBSYR,NERC,IRG,CRG,FLRG,PLANT,STEP,OWN,STYR,IRET,BUILDNM,PLNT_CD,UPLNTCD(PLANT),STATUS,(CVALUE(TYPE),TYPE = 1, 5)
 2310                            FORMAT(1X,"SDATA",10(",",I5),",",A8,3(",",A2),5(",",E20.6))
 
                                 IF (IRG .NE. 1 .AND. ACT .GT. 0.) WRITE(18,2311) CURIYR+UHBSYR,NERC,EXP,CRG,FLRG,PLANT,STEP,OWN,STYR,IRET,BUILDNM,PLNT_CD,UPLNTCD(PLANT),STATUS,(CVALUE(TYPE),TYPE = 1, 5)
 2311                            FORMAT(1X,"SDATA_INTERREG",10(",",I5),",",A8,3(",",A2),5(",",E20.6))                                 

!                                SAVE SOLUTION DATA INTO PERMANENT ARRAY

                                 DO TYPE = 1 , 5
                                    IF ((PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) .OR. PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                                       SOLDATA(FRG,PLANT,STEP,OWN,STYR,TYPE) = CVALUE(TYPE)
                                    ELSE
                                       SOLDATA(IRG,PLANT,STEP,OWN,STYR,TYPE) = CVALUE(TYPE)
                                    END IF
                                 END DO
                                 IF ((PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) .OR. PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                                    SOLSTAT(FRG,PLANT,STEP,OWN,STYR) = STATUS
                                 ELSE
                                    SOLSTAT(IRG,PLANT,STEP,OWN,STYR) = STATUS
                                 END IF
!
!                                EXCLUDE MSW FROM MARKET-SHARING SINCE IT IS FORCED IN.  ALSO, IGNORE BASIC VECTORS AT LOWER OR UPPER BOUND &
!                                   (I.E., LEAVE AT CURRENT VALUE) ALSO EXCLUDE TECHNOLOGY IF PART OF RENEWABLE PORTFOLIO STANDARD
!
                                 IF ((PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) .OR. PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                                    EPMSBLD(FRG,PLANT,STEP,OWN,STYR) = ACT
                                 ELSE
                                    EPMSBLD(IRG,PLANT,STEP,OWN,STYR) = ACT
                                 END IF
!
                                 IF (IRG .GT. 1. .OR. UPMSHSW(PLANT) .EQ. 0 .OR. &
                                    (EPMSLIM(MNUMNR,1,PLANT,STEP,1,STYR) .LE. (UPLRMIN(PLANT) * UPMSSIZ(PLANT) * 0.001)) .OR. &
									(PLNT_CD .EQ. 'PT' .AND. EPMSLIM(MNUMNR,1,PLANT-1,STEP,1,STYR) .LE. (UPLRMIN(PLANT) * UPMSSIZ(PLANT) * 0.001)) .OR. &
                                    (UPRNWBND(CURIYR + OLYR - 1) .GT. 0.001 .AND. UPRNWSHR(PLANT) .GT. 0.0) .OR. &
                                    (NATLMT(PLANT,STYR) .LE. MIN(1.0,(12 * UPMSSIZ(PLANT) * 0.001))) .OR. &
                                    (UPCAPBND(CURIYR + OLYR - 1) .GT. 0.001 .AND. UPCAPSHR(PLANT) .GT. 0.0) .OR. &
                                    (CO2_STDSW .GT. 0 .AND. CO2_STDRS(1,CURIYR + OLYR - 1) .GT. 0.1 .AND. CO2_PLTSW(PLANT) .GT. 0.0) .OR. &
                                     EMSSKIP(PLANT,STEP) .GT. 0 .OR. &
                                    (RC .EQ. 0.0 .AND. ACT .EQ. LL) .OR. (ACT .GT. 0.0 .AND. ACT .EQ. LL) .OR. ACT .EQ. UL) THEN
                                 ELSE
!
!                                   FOR BASIC VECTORS (NOT AT BOUND), DETERMINE THE TOTAL NUMBER AND CAPACITY. &
!                                   THIS REPRESENTS THE CAPACITY THAT WILL BE REALLOCATED USING THE MARKET-SHARING ALGORITHM.
!
                                    IF (RC .EQ. 0.0 .AND. (ACT .GT. LL .AND. ACT .LT. UL)) THEN
!
!                                      INCLUDE FROM MARKET SHARING IF ADDITION IS AT LEAST ONE FULL UNIT
!
                                       IF ((PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) .OR. PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                                          MS_BLD = EPMSBLD(FRG,PLANT,STEP,OWN,STYR)
                                       ELSE
                                          MS_BLD = EPMSBLD(IRG,PLANT,STEP,OWN,STYR)
                                       END IF
                                       IF (MS_BLD .GE. DBLE(UPLRMIN(PLANT) * UPMSSIZ(PLANT) * 0.001)) THEN
                                          NUMBAS = NUMBAS + 1
                                          TOTBAS = TOTBAS + (ACT - MAX(LL , 0.5 * DBLE(UPLRMIN(PLANT) * UPMSSIZ(PLANT) * 0.001)))
                                          IF ((PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) .OR. PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                                             CSTRATIO(FRG,PLANT,STEP,OWN,STYR) = 1.0
                                          ELSE
                                             CSTRATIO(IRG,PLANT,STEP,OWN,STYR) = 1.0
                                          END IF
                                       END IF
                                    ELSE
!
!                                      NONBASIC VECTORS
!
                                       IF (RC .GT. 0.0) THEN
!                                        ONLY COMPETE STEP 1 SO MULTIPLE STEPS OF SAME TECHNOLOGY AREN'T SHARING
                                         IF (STEP .EQ. 1)THEN
                                          IF ((PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) .OR. PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                                             CSTRATIO(FRG,PLANT,STEP,OWN,STYR) = 0.0
                                          ELSE
                                             CSTRATIO(IRG,PLANT,STEP,OWN,STYR) = 0.0
                                          END IF
                                         END IF
                                       END IF
                                    END IF
                                 END IF
                              ELSE

                                 IF (UPVTYP(PLANT) .EQ. 1 .AND. OWN .EQ. 2 .AND. (IRG .EQ. 1 .OR. UPETTSW(PLANT) .EQ. 1) .AND. STEP .LE. NSTEPS(PLANT)) THEN
!                                   WRITE(18,2310) CURIYR+UHBSYR,NERC,IRG,CRG,FLRG,PLANT,STEP,OWN,STYR,IRET,BUILDNM,PLNT_CD,UPLNTCD(PLANT),STATUS,(CVALUE(TYPE),TYPE = 1, 5)
                                 END IF

                                 DO TYPE = 1 , 5
                                    CVALUE(TYPE) = 0.0
                                 END DO
                                 ACT = CVALUE(1)
                                 OBJ = CVALUE(2)
                                 LL = CVALUE(3)
                                 UL = CVALUE(4)
                                 RC = CVALUE(5)
                              END IF                            ! (IRET .EQ. 0)
                            END IF                              ! UPBLDTYP
                           END DO                               ! OWN
                        END DO                                  ! STEP
                     END IF                                     ! (OLYR .LT. UNXPH)
                  END DO                                        ! STYR
               END DO                                           ! FRG
            END IF                                              ! (UPVTYP(PLANT) .GT. 0)
         END DO                                                 ! PLANT
      END DO                                                    ! IRG
!
!     MARKET-SHARING WILL SHIFT CAPACITY FROM BASIC VECTORS TO NONBASIC VECTORS IF TOTAL BUILDS EXCEED 100 MW TOTAL
!
      IF (TOTBAS .GT. 0.100) THEN
         DO IRG = 1 , NRG
            EXP = EXPORT(IRG)
            DO PLANT = 1, ECP_D_CAP
               IF (UPMSHSW(PLANT) .EQ. MSHR_PLT_GRP .AND. ((UPVTYP(PLANT) .GT. 0 .AND. IRG .EQ. 1) .OR. (UPVTYP(PLANT) .GT. 0 .AND. (PLANT .EQ. WICN .OR. PLANT .EQ. WIAN .OR. PLANT .EQ. WISM .OR. (PLANT .GT. ECP_D_DSP .AND. PLANT .NE. WIWD .AND. PLANT .NE. WIBI))))) THEN
                  IF ((PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) .OR. PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                     N_FLRG = 0
                     DO FRG = 1 , UNFRGN
                        IF (FL_CNXT_CST(NERC,FRG) .GT. 0.0) THEN
                           N_FLRG = N_FLRG + 1
                           NXT_FLRG(N_FLRG) = FRG
                        END IF
                     END DO
                  ELSE
                     N_FLRG = 1
                     NXT_FLRG(1) = IRG
                  END IF

                  DO STYR = 1 , UNXPH - 1
                     OLYR = STYR + CLT(PLANT)
                     IF (OLYR .LT. UNXPH) THEN
!
                        IF (UCPDSPIS(PLANT) .GT. 0) THEN
                           IECP = PLANT
                           NSTEPS(PLANT) = MAX(1,ESTSWTCH(PLANT))
                        ELSE IF (UCPINTIS(PLANT) .GT. 0) THEN
                           IECP = UCPINTIS(PLANT)
                           NSTEPS(PLANT) = EPSTSUP(UIRINTI(IECP))
                        ELSE IF (UCPRNWIS(PLANT) .GT. 0) THEN
                           IECP = UCPRNWIS(PLANT)
                           NSTEPS(PLANT) = EPSTSUP(UIRRNWI(IECP))
                        ELSE IF (UCPDGNIS(PLANT) .GT. 0) THEN
                           IECP = UCPDGNIS(PLANT)
                           NSTEPS(PLANT) = ECP_D_DGS
                        END IF
                        DO OWN = 1, ECP_D_OWN
                           MAX_RATIO(OWN) = 0.0
                        END DO
                        DO STEP = 1 , NSTEPS(PLANT)
                           DO FRG = 1 , N_FLRG
                              FLRG = NXT_FLRG(FRG)
                              CLRG = EPCLMP(FRG)
                              GSRG = EPGSMP(FRG)
                              CSRG = EPCSMP(FRG)
                              VAROBJ = 0.0
                              IF ((PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) .OR. PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                                 TRG = FRG
                                 T_RG = FLRG
                                 VAROBJ = VARCOL(FLRG,PLANT) * CFCPLT(NERC,PLANT) * 8.76
                              ELSE
                                 TRG = IRG
                                 T_RG = EXP
                                 VAROBJ = VAROTH(NERC,PLANT) * CFCPLT(NERC,PLANT) * 8.76
                              END IF
!
                              DO OWN = 1, ECP_D_OWN
!
!                                ASSIGN BUILD LIMIT FROM NATIONAL CONSTRAINT TO TEMPORARY VARIABLE
!
!JJ                              EPULPTS(NERC,PLANT,STEP,STYR) = EPMSLIM(NERC,TRG,PLANT,STEP,OWN,STYR)
!
!                                CHECK FOR BASIC AND NONBASIC VECTORS TO BE included in market-sharing
!
                                 IF (CSTRATIO(TRG,PLANT,STEP,OWN,STYR) .GE. 0.0) THEN
                                    ACT = SOLDATA(TRG,PLANT,STEP,OWN,STYR,1)
                                    OBJ = SOLDATA(TRG,PLANT,STEP,OWN,STYR,2)
                                    LL = SOLDATA(TRG,PLANT,STEP,OWN,STYR,3)
                                    UL = SOLDATA(TRG,PLANT,STEP,OWN,STYR,4)
                                    RC = SOLDATA(TRG,PLANT,STEP,OWN,STYR,5)
!
!                                   FOR THE NONBASIC VECTORS, DETERMINE COST COMPETITIVENESS BY COMPUTING RATIO OF ACTUAL
!                                   COST (OBJ) TO THE COST NEEDED FOR PENETRATION (OBJ - RC)
!
                                    IF (CSTRATIO(TRG,PLANT,STEP,OWN,STYR) .EQ. 0.0)THEN
!
!                                      EXCLUDE NONUTILITIES IF PURCHASE CONSTRAINT
!                                      IS BINDING OR A PARTICULAR PLANT TYPE IF
!                                      UPPER BOUND IS BINDING.
!
                                       IF ((EPULPT(PLANT,STYR) .GT. 0.0) .AND. &
!JJ                                        (EPULPTS(NERC,PLANT,STEP,STYR) .GT. 0.0) .AND. &
                                           (EPULSCS(PLANT,STEP,STYR) .GT. 0.0) .AND. &
                                           (UPOWNCD(OWN) .EQ. 'U' .OR. EPULNU(OLYR) .EQ. 0.0)) THEN
!
                                          IF (OBJ .GT. 0.0) THEN
                                             CSTRATIO(TRG,PLANT,STEP,OWN,STYR) = (OBJ + DBLE(VAROBJ) - RC) / (OBJ + DBLE(VAROBJ))

!                                            if (step .eq. 1 .and. styr .eq. 1)write(18,6789) curiyr+1989,nerc,trg,PLANT,uplntcd(PLANT),cfcplt(nerc,PLANT),obj,varobj,rc,  &
!                                               (OBJ + DBLE(VAROBJ) - RC) , (OBJ + DBLE(VAROBJ)),  &
!                                               CSTRATIO(TRG,PLANT,STEP,OWN,STYR)
!6789                                        format(1h ,'!cstr',i4,i3,i3,i3,a3,8f10.3)

!                                            IF RATIO EXCEEDS ACCEPTABLE TOLERANCE LEVEL THEN EXCLUDE FROM MARKET SHARING
!
                                             IF (CSTRATIO(TRG,PLANT,STEP,OWN,STYR) .LT. (1.0 - EPMSTOL)) THEN
                                                CSTRATIO(TRG,PLANT,STEP,OWN,STYR) = 0.0
                                             ELSE
!
!                                               APPLY EXPONENT OF SHARING ALGORITHM
!
                                                CSTRATIO(TRG,PLANT,STEP,OWN,STYR) = CSTRATIO(TRG,PLANT,STEP,OWN,STYR) ** EPMSEXP
                                             END IF
                                          END IF
                                       END IF
                                    END IF
!
!                                   COMPUTE TOTAL OF RATIOS
!
                                    TOTRATIO = TOTRATIO + CSTRATIO(TRG,PLANT,STEP,OWN,STYR)
                                    MAX_RATIO(OWN) = MAX(MAX_RATIO(OWN) , CSTRATIO(TRG,PLANT,STEP,OWN,STYR))
                                 END IF
                              END DO       ! OWN
                           END DO          ! FRG
                           IF ((PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) .OR. PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                              DO OWN = 1, ECP_D_OWN
                                 IF (MAX_RATIO(OWN) .GT. 0) THEN
                                    DO FRG = 1 , N_FLRG
                                       FLRG = NXT_FLRG(FRG)
                                       TRG = FRG
                                       T_RG = FLRG
                                       IF (CSTRATIO(TRG,PLANT,STEP,OWN,STYR) .LT. MAX_RATIO(OWN))  CSTRATIO(TRG,PLANT,STEP,OWN,STYR) = 0.0
                                    END DO
                                 END IF
                              END DO
                           END IF
                        END DO                ! STEP
                     END IF                   ! (OLYR .LT. UNXPH)
                  END DO                      ! STYR
               END IF                         ! (UPVTYP(PLANT) .GT. 0)
            END DO                            ! PLANT
         END DO                               ! IRG
!
!        LOOP OVER BUILD VECTORS, IDENTIFY NONBASIC VECTORS AND ALLOCATE
!        A SHARE TO THE COMPETITIVE TECHNOLOGIES (SKIP IF LESS THAN 100 MW TOTAL)
!
         DO IRG = 1 , NRG
            EXP = EXPORT(IRG)
            DO PLANT = 1, ECP_D_CAP
               IF (UPMSHSW(PLANT) .EQ. MSHR_PLT_GRP .AND. ((UPVTYP(PLANT) .GT. 0 .AND. IRG .EQ. 1) .OR. (UPVTYP(PLANT) .GT. 0 .AND. (PLANT .EQ. WICN .OR. PLANT .EQ. WIAN .OR. PLANT .EQ. WISM .OR. (PLANT .GT. ECP_D_DSP .AND. PLANT .NE. WIWD .AND. PLANT .NE. WIBI))))) THEN
                  IF ((PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) .OR. PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                     N_FLRG = 0
                     DO FRG = 1 , UNFRGN
                        IF (FL_CNXT_CST(NERC,FRG) .GT. 0.0) THEN
                           N_FLRG = N_FLRG + 1
                           NXT_FLRG(N_FLRG) = FRG
                        END IF
                     END DO
                  ELSE
                     N_FLRG = 1
                     NXT_FLRG(1) = IRG
                  END IF

                  DO YEAR = 1 , UNXPH
                     IF (YEAR + CLT(PLANT) .LT. UNXPH) THEN
!
                        IF (UCPDSPIS(PLANT) .GT. 0) THEN
                           IECP = PLANT
                           IF (PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) THEN
                              NSTEPS(PLANT) = 1
                           ELSE
                              IF (UPETTSW(IECP) .EQ. 1 .AND. IRG .GT. 1) THEN
                                 NSTEPS(PLANT) = 0
                                 DO IFL = 1 , ECP_D_FPP
                                    IF (UPFLTP(PLANT,IFL) .GT. 0) NSTEPS(PLANT) = NSTEPS(PLANT) + 1
                                 END DO
                              ELSE
                                 NSTEPS(PLANT) = 1
                              END IF
                           END IF
                        ELSE IF (UCPINTIS(PLANT) .GT. 0) THEN
                           IECP = UCPINTIS(PLANT)
                           NSTEPS(PLANT) = EPSTSUP(UIRINTI(IECP))
                        ELSE IF (UCPRNWIS(PLANT) .GT. 0) THEN
                           IECP = UCPRNWIS(PLANT)
                           NSTEPS(PLANT) = EPSTSUP(UIRRNWI(IECP))
                        ELSE IF (UCPDGNIS(PLANT) .GT. 0) THEN
                           IECP = UCPDGNIS(PLANT)
                           NSTEPS(PLANT) = ECP_D_DGS
                        END IF
                        DO STEP = 1 , NSTEPS(PLANT)
                           DO FRG = 1 , N_FLRG
                              FLRG = NXT_FLRG(FRG)
                              CLRG = EPCLMP(FRG)
                              GSRG = EPGSMP(FRG)
                              CSRG = EPCSMP(FRG)
                              IF ((PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) .OR. PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                                 TRG = FRG
                                 T_RG = FLRG
                              ELSE
                                 TRG = IRG
                                 T_RG = EXP
                              END IF

                              DO OWN = 1 , ECP_D_OWN

!                                ASSIGN BUILD LIMIT FROM NATIONAL CONSTRAINT TO TEMPORARY VARIABLE

                                 IF (CSTRATIO(TRG,PLANT,STEP,OWN,YEAR) .GT. 0.0 .AND. CSTRATIO(TRG,PLANT,STEP,OWN,YEAR) .LT. 1.0) THEN
                                    EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) = SOLDATA(TRG,PLANT,STEP,OWN,YEAR,3) + TOTBAS * CSTRATIO(TRG,PLANT,STEP,OWN,YEAR) / TOTRATIO

!                                   MAKE SURE LOWER AND UPPER BOUNDS ARE NOT VIOLATED AND REVISE UPPER BOUND TO ACCOUNT FOR ADJUSTED BUILD DECISION

                                    EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) = MIN(EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) , &
                                                                           SOLDATA(TRG,PLANT,STEP,OWN,YEAR,4), &
                                                                           (SOLDATA(TRG,PLANT,STEP,OWN,YEAR,1) + EPULPT(PLANT,YEAR)))
                                    EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) = MIN(EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) , SOLDATA(TRG,PLANT,STEP,OWN,YEAR,4), &
                                                                           (SOLDATA(TRG,PLANT,STEP,OWN,YEAR,1) + EPULSCS(PLANT,STEP,YEAR)))
                                    EPULPT(PLANT,YEAR) = EPULPT(PLANT,YEAR) - (EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) - SOLDATA(TRG,PLANT,STEP,OWN,YEAR,1))
                                    EPULPT(PLANT,YEAR) = MAX(EPULPT(PLANT,YEAR),DBLE(0.0))
                                    EPULSCS(PLANT,STEP,YEAR) = EPULSCS(PLANT,STEP,YEAR) - (EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) - SOLDATA(TRG,PLANT,STEP,OWN,YEAR,1))
                                    EPULSCS(PLANT,STEP,YEAR) = MAX(EPULSCS(PLANT,STEP,YEAR),DBLE(0.0))

!                                   TOTAL REALLOCATED TO ADJUST CAPACITY OF BASIC VECTORS IGNORE IF AMOUNT IS LESS THAN HALF A UNIT

                                    IF (EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) .LT. DBLE(UPLRMIN(PLANT) * UPMSSIZ(PLANT) * 0.001))THEN
                                       EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) = SOLDATA(TRG,PLANT,STEP,OWN,YEAR,3)
                                    ELSE
                                       TOTNBS = TOTNBS + &
                                       EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) - SOLDATA(TRG,PLANT,STEP,OWN,YEAR,3)

!6794                                   format(1h ,'epmslim',7(":",i4),5(":",f21.9))

!                                      DECREMENT ALLOWABLE NATIONAL BUILDS BY REALLOCATED AMOUNT
									   IF  (PLANT .EQ. WIPT) THEN
!									   		write(18,6794) curiyr+1989,nerc,trg,PLANT,STEP,YEAR,OWN,(EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) - SOLDATA(TRG,PLANT,STEP,OWN,YEAR,3)), EPMSLIM(MNUMNR,1,PLANT-1,STEP,1,YEAR), &
!											EPMSLIM(MNUMNR,1,PLANT-1,STEP,1,YEAR) -(EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) - SOLDATA(TRG,PLANT,STEP,OWN,YEAR,3)), &
!											CSTRATIO(TRG,PLANT,STEP,OWN,YEAR), 0.0
											
										  EPMSLIM(MNUMNR,1,PLANT-1,STEP,1,YEAR) = EPMSLIM(MNUMNR,1,PLANT-1,STEP,1,YEAR) -  &
											(EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) - SOLDATA(TRG,PLANT,STEP,OWN,YEAR,3))
									   ELSE
!									   		 write(18,6794) curiyr+1989,nerc,trg,PLANT,STEP,YEAR,OWN,(EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) - SOLDATA(TRG,PLANT,STEP,OWN,YEAR,3)), EPMSLIM(MNUMNR,1,PLANT,STEP,1,YEAR), &
!											EPMSLIM(MNUMNR,1,PLANT,STEP,1,YEAR) -(EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) - SOLDATA(TRG,PLANT,STEP,OWN,YEAR,3)), &
!											CSTRATIO(TRG,PLANT,STEP,OWN,YEAR), 0.0
											
										  EPMSLIM(MNUMNR,1,PLANT,STEP,1,YEAR) = EPMSLIM(MNUMNR,1,PLANT,STEP,1,YEAR) -  &
											(EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) - SOLDATA(TRG,PLANT,STEP,OWN,YEAR,3))
									   END IF
                                    END IF
                                 END IF      ! OWN
                              END DO
                           END DO            ! FRG
                        END DO               ! STEP
                     END IF                  ! (YEAR + CLT(PLANT) .LT. UNXPH)
                  END DO                     ! YEAR
               END IF                        ! (UPVTYP(PLANT .GT. 0 )
            END DO                           ! PLANT
         END DO                              ! IRG
!
!        LOOP OVER BUILD VECTORS, IDENTIFY BASIC VECTORS AND ADJUST THEIR CAPACITIES TO ACCOUNT FOR MARKET SHARING
!
         DO IRG = 1 ,NRG
            EXP = EXPORT(IRG)
            DO PLANT = 1, ECP_D_CAP
               IF (UPMSHSW(PLANT) .EQ. MSHR_PLT_GRP .AND. ((UPVTYP(PLANT) .GT. 0 .AND. IRG .EQ. 1) .OR. (UPVTYP(PLANT) .GT. 0 .AND. (PLANT .EQ. WICN .OR. PLANT .EQ. WIAN .OR. PLANT .EQ. WISM .OR. (PLANT .GT. ECP_D_DSP .AND. PLANT .NE. WIWD .AND. PLANT .NE. WIBI))))) THEN
                  IF ((PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) .OR. PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                     N_FLRG = 0
                     DO FRG = 1 , UNFRGN
                        IF (FL_CNXT_CST(NERC,FRG) .GT. 0.0) THEN
                           N_FLRG = N_FLRG + 1
                           NXT_FLRG(N_FLRG) = FRG
                        END IF
                     END DO
                  ELSE
                     N_FLRG = 1
                     NXT_FLRG(1) = IRG
                  END IF

                  DO YEAR = 1 , UNXPH
                     IF (YEAR + CLT(PLANT) .LT. UNXPH) THEN
!
                        IF (UCPDSPIS(PLANT) .GT. 0) THEN
                           IECP = PLANT
                           IF (PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) THEN
                              NSTEPS(PLANT) = 1
                           ELSE
                              IF (UPETTSW(IECP) .EQ. 1 .AND. IRG .GT. 1) THEN
                                 NSTEPS(PLANT) = 0
                                 DO IFL = 1 , ECP_D_FPP
                                    IF (UPFLTP(PLANT,IFL) .GT. 0) NSTEPS(PLANT) = NSTEPS(PLANT) + 1
                                 END DO
                              ELSE
                                 NSTEPS(PLANT) = 1
                              END IF
                           END IF
                        ELSE IF (UCPINTIS(PLANT) .GT. 0) THEN
                           IECP = UCPINTIS(PLANT)
                           NSTEPS(PLANT) = EPSTSUP(UIRINTI(IECP))
                        ELSE IF (UCPRNWIS(PLANT) .GT. 0) THEN
                           IECP = UCPRNWIS(PLANT)
                           NSTEPS(PLANT) = EPSTSUP(UIRRNWI(IECP))
                        ELSE IF (UCPDGNIS(PLANT) .GT. 0) THEN
                           IECP = UCPDGNIS(PLANT)
                           NSTEPS(PLANT) = ECP_D_DGS
                        END IF
                        DO STEP = 1 , NSTEPS(PLANT)
                           DO FRG = 1 , N_FLRG
                              FLRG = NXT_FLRG(FRG)
                              CLRG = EPCLMP(FRG)
                              GSRG = EPGSMP(FRG)
                              CSRG = EPCSMP(FRG)
                              IF ((PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) .OR. PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                                 TRG = FRG
                                 T_RG = FLRG
                              ELSE
                                 TRG = IRG
                                 T_RG = EXP
                              END IF
!
                              DO OWN = 1 , ECP_D_OWN
                                 IF (CSTRATIO(TRG,PLANT,STEP,OWN,YEAR) .EQ. 1.0) THEN
                                    MINBLD = MAX(SOLDATA(TRG,PLANT,STEP,OWN,YEAR,3), DBLE(UPLRMIN(PLANT) * UPMSSIZ(PLANT) * 0.001))
									

!                                   INCREMENT ALLOWABLE NATIONAL BUILDS BY REALLOCATED AMOUNT
									   
									IF  (PLANT .EQ. WIPT) THEN
!										write(18,6794) curiyr+1989,nerc,trg,PLANT,STEP,YEAR,OWN,TOTNBS *(EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) - MINBLD) / TOTBAS, EPMSLIM(MNUMNR,1,PLANT-1,STEP,1,YEAR), &
!											EPMSLIM(MNUMNR,1,PLANT-1,STEP,1,YEAR) + TOTNBS *(EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) - MINBLD) / TOTBAS, &
!											CSTRATIO(TRG,PLANT,STEP,OWN,YEAR), 1.0
											
										EPMSLIM(MNUMNR,1,PLANT-1,STEP,1,YEAR) = EPMSLIM(MNUMNR,1,PLANT-1,STEP,1,YEAR) +  &
                                       TOTNBS * (EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) - MINBLD) / TOTBAS

									ELSE
!										write(18,6794) curiyr+1989,nerc,trg,PLANT,STEP,YEAR,OWN,TOTNBS *(EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) - MINBLD) / TOTBAS, EPMSLIM(MNUMNR,1,PLANT,STEP,1,YEAR), &
!											EPMSLIM(MNUMNR,1,PLANT,STEP,1,YEAR) + TOTNBS *(EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) - MINBLD) / TOTBAS, &
!											CSTRATIO(TRG,PLANT,STEP,OWN,YEAR), 1.0
											
										EPMSLIM(MNUMNR,1,PLANT,STEP,1,YEAR) = EPMSLIM(MNUMNR,1,PLANT,STEP,1,YEAR) +  &
										   TOTNBS * (EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) - MINBLD) / TOTBAS
									END IF
										   
									! this is the final result thats used in ECP
                                    EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) = EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) - TOTNBS * &
                                       (EPMSBLD(TRG,PLANT,STEP,OWN,YEAR) - MINBLD) / TOTBAS
                                 END IF
                              END DO                                  ! OWN
                           END DO                                     ! FRG
                        END DO                                        ! STEP
                     END IF
                  END DO                                              ! YEAR
               END IF
            END DO                                                    ! PLANT
         END DO                                                       ! IRG
      END IF
!
!     LOOP OVER BUILD VECTORS, AND PRINT ECP SOLUTION AND MARKET-SHARE SOLUTION
!
      IF (USYEAR(CURIYR) .EQ. UPSTYR .AND. NERC .EQ. 1) WRITE(18,2300)
 2300 FORMAT(1H ,'EMS',":",'CYR ',":",'RG',":",'PT',":",'YR', ":",'OW',":",'ST',":",'  OBJ   ',":",'   RC   ',":",'  ECP   ', &
         ":",'  MSHR  ',":",'OW',":",'ST',":",'  OBJ   ',":",'   RC   ', ":",'  ECP   ',":",'  MSHR  ',":",'XR', ":", 'STEP')

      DO IRG = 1 ,NRG
         EXP = EXPORT(IRG)
         DO PLANT = 1, ECP_D_CAP
            IF (UPMSHSW(PLANT) .EQ. MSHR_PLT_GRP .AND. ((UPVTYP(PLANT) .GT. 0 .AND. IRG .EQ. 1) .OR. (UPVTYP(PLANT) .GT. 0 .AND. (PLANT .EQ. WICN .OR. PLANT .EQ. WIAN .OR. PLANT .EQ. WISM .OR. (PLANT .GT. ECP_D_DSP .AND. PLANT .NE. WIWD .AND. PLANT .NE. WIBI))))) THEN
               IF ((PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) .OR. PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                  N_FLRG = 0
                  DO FRG = 1 , UNFRGN
                     IF (FL_CNXT_CST(NERC,FRG) .GT. 0.0) THEN
                        N_FLRG = N_FLRG + 1
                        NXT_FLRG(N_FLRG) = FRG
                     END IF
                  END DO
               ELSE
                  N_FLRG = 1
                  NXT_FLRG(1) = IRG
               END IF

               PLNT_CD = UPLNTCD(PLANT)
               DO YEAR = 1 , UNXPH - 1
                  IF (YEAR + CLT(PLANT) .LT. UNXPH) THEN
!
                     IF (UCPDSPIS(PLANT) .GT. 0) THEN
                        IECP = PLANT
                        IF (PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) THEN
                           NSTEPS(PLANT) = 1
                        ELSE
                           IF (UPETTSW(IECP) .EQ. 1 .AND. IRG .GT. 1) THEN
                              NSTEPS(PLANT) = 0
                              DO IFL = 1 , ECP_D_FPP
                                 IF (UPFLTP(PLANT,IFL) .GT. 0) NSTEPS(PLANT) = NSTEPS(PLANT) + 1
                              END DO
                           ELSE
                              NSTEPS(PLANT) = 1
                           END IF
                        END IF
                     ELSE IF (UCPINTIS(PLANT) .GT. 0) THEN
                        IECP = UCPINTIS(PLANT)
                        NSTEPS(PLANT) = EPSTSUP(UIRINTI(IECP))
                     ELSE IF (UCPRNWIS(PLANT) .GT. 0) THEN
                        IECP = UCPRNWIS(PLANT)
                        NSTEPS(PLANT) = EPSTSUP(UIRRNWI(IECP))
                     ELSE IF (UCPDGNIS(PLANT) .GT. 0) THEN
                        IECP = UCPDGNIS(PLANT)
                        NSTEPS(PLANT) = ECP_D_DGS
                     END IF
                     DO STEP = 1 , NSTEPS(PLANT)
                        DO FRG = 1 , N_FLRG
                           FLRG = NXT_FLRG(FRG)
                           CLRG = EPCLMP(FRG)
                           GSRG = EPGSMP(FRG)
                           CSRG = EPCSMP(FRG)
                           IF ((PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) .OR. PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                              TRG = FRG
                              T_RG = FLRG
                           ELSE
                              TRG = IRG
                              T_RG = EXP
                           END IF
!
                           IF (SOLDATA(TRG,PLANT,STEP,2,YEAR,2) .GT. 0.0 .AND. SOLDATA(TRG,PLANT,STEP,2,YEAR,2) .LT. 50000.0) THEN
                              WRITE(18,2400) CURIYR + UHBSYR,NERC,PLNT_CD,YEAR, (OWN,SOLSTAT(TRG,PLANT,STEP,OWN,YEAR), &
                                 SOLDATA(TRG,PLANT,STEP,OWN,YEAR,2), SOLDATA(TRG,PLANT,STEP,OWN,YEAR,5), &
                                 SOLDATA(TRG,PLANT,STEP,OWN,YEAR,1), EPMSBLD(TRG,PLANT,STEP,OWN,YEAR),OWN = 1,2),T_RG,STEP,TRG
 2400                         FORMAT(1H ,'EMS',":",I4,":",I2,":",A2,":",I2,":",I2,":",A2,":",F8.1,":",F8.1,":",F8.3, &
                                 ":",F8.3,":",I2,":",A2,":",F12.1,":",F12.1,":",F8.3,":",F8.3,":",I2,":",I2,":",I2)
                           END IF
                        END DO                          ! FRG
                     END DO                             ! STEP
                  END IF
               END DO
            END IF
         END DO
!
!        Write out EMMDB Capacity Planning Build Characteristics Table 1
!
         IF ((USW_DBS .GT. 0) .OR. ((ORCLECP .EQ. 1) .AND. (FNRUN .EQ. 1)) )  THEN
!
            DO PLANT = 1, ECP_D_CAP                                        !//EMMDB//
               IF (UPMSHSW(PLANT) .EQ. MSHR_PLT_GRP .AND. ((UPVTYP(PLANT) .GT. 0 .AND. IRG .EQ. 1) .OR. (UPVTYP(PLANT) .GT. 0 .AND. (PLANT .EQ. WICN .OR. PLANT .EQ. WIAN .OR. PLANT .EQ. WISM .OR. (PLANT .GT. ECP_D_DSP .AND. PLANT .NE. WIWD .AND. PLANT .NE. WIBI))))) THEN
                  IF ((PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) .OR. PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                     N_FLRG = 0
                     DO FRG = 1 , UNFRGN
                        IF (FL_CNXT_CST(NERC,FRG) .GT. 0.0) THEN
                           N_FLRG = N_FLRG + 1
                           NXT_FLRG(N_FLRG) = FRG
                        END IF
                     END DO
                  ELSE
                     N_FLRG = 1
                     NXT_FLRG(1) = IRG
                  END IF

                  IF (UCPDSPIS(PLANT) .GT. 0) THEN
                     IECP = PLANT
                     IF (PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) THEN
                        NSTEPS(PLANT) = 1
                     ELSE
                        IF (UPETTSW(IECP) .EQ. 1 .AND. IRG .GT. 1) THEN
                           NSTEPS(PLANT) = 0
                           DO IFL = 1 , ECP_D_FPP
                              IF (UPFLTP(PLANT,IFL) .GT. 0) NSTEPS(PLANT) = NSTEPS(PLANT) + 1
                           END DO
                        ELSE
                           NSTEPS(PLANT) = 1
                        END IF
                     END IF
                  ELSE IF (UCPINTIS(PLANT) .GT. 0) THEN
                     IECP = UCPINTIS(PLANT)
                     NSTEPS(PLANT) = EPSTSUP(UIRINTI(IECP))
                  ELSE IF (UCPRNWIS(PLANT) .GT. 0) THEN
                     IECP = UCPRNWIS(PLANT)
                     NSTEPS(PLANT) = EPSTSUP(UIRRNWI(IECP))
                  ELSE IF (UCPDGNIS(PLANT) .GT. 0) THEN
                     IECP = UCPDGNIS(PLANT)
                     NSTEPS(PLANT) = ECP_D_DGS
                  END IF
                  DO YEAR = 1 , UNXPH - 1                                    !//EMMDB//
                     IF (YEAR + CLT(PLANT) .LT. UNXPH) THEN                   !//EMMDB//
                        IF (EXP .EQ. NERC .OR. UPETTSW(PLANT) .EQ. 1) THEN     !//EMMDB//
                           DO IOWN = 1,2                                        !//EMMDB//
                              DO STEP = 1,NSTEPS(PLANT)                           !//EMMDB//
                                 DO FRG = 1 , N_FLRG
                                    FLRG = NXT_FLRG(FRG)
                                    CLRG = EPCLMP(FRG)
                                    GSRG = EPGSMP(FRG)
                                    CSRG = EPCSMP(FRG)
                                    IF ((PLANT .LE. ECP_D_DSP .AND. PLANT .NE. WICN .AND. PLANT .NE. WIAN .AND. PLANT .NE. WISM) .OR. PLANT .EQ. WIWD .OR. PLANT .EQ. WIBI) THEN
                                       TRG = FRG
                                       T_RG = FLRG
                                       T_RG = CLRG
                                    ELSE
                                       TRG = IRG
                                       T_RG = EXP
                                    END IF
!
                                    IF ( (EPMSBLD(TRG,PLANT,STEP,IOWN,YEAR) .NE. 0.0) .OR.     &
                                          (SOLDATA(TRG,PLANT,STEP,IOWN,YEAR,1) .NE. 0.0) ) THEN
!
                                    IF (USW_DBS .GT. 0) &
                                       WRITE(UF_DBS,5000) COL,PLANT,COL,IOWN,COL,          & !//EMMDB//
                                       NERC,COL,STEP,COL,                                  & !//EMMDB//
                                       CURIYR,COL,YEAR,COL,                                & !//EMMDB//
                                       SOLDATA(TRG,PLANT,STEP,IOWN,YEAR,1),COL,            & !//EMMDB//
                                       EPMSBLD(TRG,PLANT,STEP,IOWN,YEAR),COL,              & !//EMMDB//
                                       SOLSTAT(TRG,PLANT,STEP,IOWN,YEAR),COL,              & !//EMMDB//
                                       SOLDATA(TRG,PLANT,STEP,IOWN,YEAR,2),COL,            & !//EMMDB//
                                       SOLDATA(TRG,PLANT,STEP,IOWN,YEAR,5),COL,EXP,        & !//EMMDB//
                                       COL,TRIM(SCEN_DATE)                        !//EMMDB//
 5000                               FORMAT(1X ,'ECPCPB1',6(A2,I4),2(A2,F8.3),A2,A4,2(A2,F15.1),A2,I4,A2,A)                           !//EMMDB//
!
!                                   write ecp build table to emm database
!
                                    TNUM = 1
                                    IF ((ORCLECP .EQ. 1) .AND. (FNRUN .EQ. 1)) THEN
                                       IF (LOOPING(TNUM) .EQ. 0) THEN
                                          NUMCOLS(TNUM) = 12
                                          DYNSTM(TNUM) =  'INSERT INTO ECP_BLD_CHAR VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?)'
                                          WRTSTM(TNUM) =  'ECP_BLD_CHAR'
                                       ENDIF
                                       LOOPING(TNUM) = LOOPING(TNUM) + 1
                                       COLV(TNUM,1,LOOPING(TNUM)) = PLANT
                                       COLV(TNUM,2,LOOPING(TNUM)) = IOWN
                                       COLV(TNUM,3,LOOPING(TNUM)) = NERC
                                       COLV(TNUM,4,LOOPING(TNUM)) = CURIYR
                                       COLV(TNUM,5,LOOPING(TNUM)) = EXP
                                       COLV(TNUM,6,LOOPING(TNUM)) = YEAR
                                       COLV(TNUM,7,LOOPING(TNUM)) = STEP
                                       COLV(TNUM,8,LOOPING(TNUM)) = SOLDATA(TRG,PLANT,STEP,IOWN,YEAR,1)
                                       COLV(TNUM,9,LOOPING(TNUM)) = EPMSBLD(TRG,PLANT,STEP,IOWN,YEAR)
                                       CHCOLV(TNUM,10,LOOPING(TNUM)) = SOLSTAT(TRG,PLANT,STEP,IOWN,YEAR)
                                       COLV(TNUM,11,LOOPING(TNUM)) = MIN(SOLDATA(TRG,PLANT,STEP,IOWN,YEAR,2),999999.0)
                                       COLV(TNUM,12,LOOPING(TNUM)) = MIN(SOLDATA(TRG,PLANT,STEP,IOWN,YEAR,5),999999.0)
                                       IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
                                          COLVALS(:,:) = COLV(TNUM,:,:)
                                          CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!                                         CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                                          CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
                                          LOOPING(TNUM) = 0
                                       ENDIF
                                    ENDIF
                                   ENDIF
                                 END DO           ! FRG
                              END DO                                              !//EMMDB//
                           END DO                                               !//EMMDB//
                        END IF                                                 !//EMMDB//
                     END IF                                                   !//EMMDB//
                  END DO                                                     !//EMMDB//
               END IF                                                       !//EMMDB//
            END DO                                                          !//EMMDB//
         END IF
      END DO
!
!      --- WRITE OUT ANY LEFT OVER RECORDS TO THE NEMS DATA BASE
       DO TNUM = 1 , NUMTABS
          IF (LOOPING(TNUM) .NE. 0) THEN
             COLVALS(:,:) = COLV(TNUM,:,:)
             CHCOLVALS(:,:) = CHCOLV(TNUM,:,:)
!            CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
             CALL WRITE_DB_DATA(WRTSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,UF_DBS)
             LOOPING(TNUM) = 0
          ENDIF
       ENDDO
!
      RETURN
      END


      SUBROUTINE EPO$HBLD(NERC)

      IMPLICIT NONE

!     THIS SUBROUTINE DETERMINES SUPPLEMENTAL HYDRO BUILDS WHEN ALLOWED (SUCH AS IN A CARBON SCENARIO)

      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'control'
      include 'ecpcntl'
      include 'entcntl'
      include 'bildin'
      include 'bildout'
      include 'wrenew'
      include 'rencntl'
      include 'emshrout'
!
      REAL*4 LVCSTMIN,LVCSTPLT,LVCSTHYD,FCF,CFC,FRAC,OVR,FOM,VOM
      INTEGER NERC,RNW,IR,IS,PLT,OWN,OWN1,OWN2,PMIN,OMIN,IECP,IRNW
      INTEGER OLYR,NSTP
!
!   IDENTIFY HYDRO AND CHECK UPTOPR TO SEE IF NEW BUILDS ALLOWED
!
      IF ((CURIYR + UHBSYR) .EQ. UPSTYR .OR. IECP .EQ. 0) THEN
         DO RNW = 1 , ECP_D_RNW
            IF (UCPRNWI(RNW) .GT. 0) THEN
               IF (UPLNTCD(UCPRNWI(RNW)) .EQ. 'HY')THEN
                 IECP = UCPRNWI(RNW)
                 IRNW = UIRRNWI(RNW)
                 IR = RNW
                 FCF = 0.155
                 CFC = 0.45
              END IF
           END IF
         END DO
      END IF
      IF ( (UPTOPR(IECP) .EQ. 3) .AND. (UPVTYP(IECP) .EQ. 0) ) THEN
!   SEE IF NEW HYDRO IS AVAILABLE YET
          OLYR = UHBSYR + CURIYR + UPPLYR(IECP)
       IF (UPAVLYR(IECP) .LE. OLYR) THEN
!         DO NERC = 1 , UNRGNS
!   GET HYDRO COST DATA TO COMPUTE LEVELIZED COST
                CALL GETBLD(1,NERC)
!   ASSIGN FIXED AND VARIABLE O&M--CAPITAL DONE LATER BY SUPPLY STEP
                FOM = EPIRFOM(IRNW)
                VOM = EPIRVOM(IRNW)
!   CAP MAXIMUM HYDRO CAPACITY FACTOR FOR NEW PLANTS
                EPRCFC(IRNW) = MIN(EPRCFC(IR),CFC)
!   GET LEVELIZED COST DATA FROM ECP OUTPUT DAF
!   DETERMINE MINIMUM LEVELIZED COST OF ALTERNATIVE TECHNOLOGIES
!           CALL GETBOUT(CURIYR,NERC)
           LVCSTMIN = 999.99
!   DETERMINE MINIMUM LEVELIZED COST
           DO PLT = 1 , ECP_D_DSP
!   CHECK IF PLANT/OWNER TYPE CAN BE BUILT
             IF(UPVTYP(PLT) .EQ. 1)THEN
               OLYR = UHBSYR + CURIYR + UPPLYR(PLT)
              IF(UPAVLYR(PLT) .LE. OLYR)THEN
               IF(UPNUGSW(PLT) .LE. 0)THEN
                 OWN1 = 1
                 OWN2 = 1
               ELSE
                IF(UPNUGSW(PLT) .EQ. 1)THEN
                 OWN1 = 1
                 OWN2 = 2
                ELSE
                 OWN1 = 2
                 OWN2 = 2
                END IF
               END IF
!   DETERMINE LEVELIZED COST FOR PLANT/OWNER TYPE
               DO OWN = OWN1 , OWN2
                LVCSTPLT = (EPLVCAP(PLT,OWN) + EPLVFOM(PLT)) /  &
                           (UPMCF(PLT) * 8.760) + &
                           (EPLVFLC(PLT) + EPLVVOM(PLT) + EPLVEXT(PLT))
                IF(LVCSTPLT .LT. LVCSTMIN)THEN
                   LVCSTMIN = LVCSTPLT
                   PMIN = PLT
                   OMIN = OWN
                END IF
               END DO
              END IF                                                 ! UPAVLYR
             END IF                                                  ! UPVTYP
           END DO                                                    ! PLT
!   CHECK IF ANY CAPACITY AVAILABLE FOR EACH SUPPLY STEP
             NSTP = 1
!          DO IS = 1 , MLPTMX
           DO IS = 1 , MSPTMX
             IF(IS .LT. MLPTMX)THEN
                FRAC = UTRSFC(NERC,1,IS + 1) - UTRSFC(NERC,1,IS)
             ELSE
                FRAC = 1.0 - UTRSFC(NERC,1,IS)
             END IF
             IF(FRAC .GT. 0.0 .AND. NSTP .LE. ECP_D_FPP)THEN
!   NOW DETERMINE LEVELIZED HYDRO COST FOR EACH SUPPLY STEP
                OVR = EPIROVR(IRNW) * UPLRPC(IECP) * EPRGM(IECP) * &
                      EPACM(IECP) * UTCSFC(NERC,1,IS) * UPANNADJ(IECP,MIN(MNUMYR,CURIYR+UPPLYR(IECP)))
                LVCSTHYD = (OVR * FCF + FOM) / (EPRCFC(IR) * 8.760) + VOM
!   IF HYDRO HAS LOWER LEVELIZED COST, THEN ADD HYDRO AT THAT STEP
                IF(LVCSTHYD .LT. LVCSTMIN)THEN
!      WRITE OUT TO SAME ARRAY USED FOR MARKET-SHARING ADJUSTMENTS
                      EPMSBLD(1,IECP,NSTP,2,1) = FRAC * UTRSMX(NERC,1) * 0.001
                      EPCCSUP(IRNW,NSTP) = UTCSFC(NERC,1,IS)
                      NSTP = NSTP + 1
                END IF                                               ! LVCSTHYD
             END IF                                                  ! FRAC
           END DO                                                    ! IS
!   IF ANY HYDRO ADDED, THEN STORE COST MULTPLIER IN BILDIN
                IF(NSTP.GT.1)CALL STRBLD(1,NERC)
!         END DO                                                      ! NERC
       END IF                                                        ! UPAVLYR
      END IF                                                         ! UPTOPR
      RETURN
      END
