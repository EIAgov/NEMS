! $Header: m:/default/source/RCS/wellak.f,v 1.67 2020/09/04 15:28:03 APR Exp $

    SUBROUTINE OGFOR_AK
    ! *******************************************************************
    ! *******************************************************************
    !
    !
    !                           A L A S K A
    !       updated existing ANS field configuration & data on 9/8/03
    !   Left structure and algorithms in place, commented out unused code
    !       Revisions mostly w.r.t. data in wlalaska.txt
    !   Added code to make resource recovery factor a function of the
    !       world oil price 10/8/03
    !   Added code for high/low WOP resource cases in AEO2006 (9/21/05)
    !       only affects resources of new fields and not existing fields,
    !       embedded in subroutine "OGPFAC_AK"
    !   Added code which internally calculates AK NGL production based on
    !       total Alaska oil production, global variable "OGNGLAK(T)", 5/30/06
    !   Changed code to accomodate relocation of ANWR in reserve & production matrices
    !       rescaled oil field reserves and start deducting reserves as of end of 2006 (7-30-08
    !   Added code to accomodate the categorization of reserves and undiscovered resources for
    !       the AEO2010 into onshore, state offshore, and federal offshore regions (8-12-09)
    !   July 2010 Major Changes:
    !             - North Slope new field wildcat exploration wells (NFW) as a function of crude oil prices.
    !             - NFW function econometrically estimated based on Alaska Oil and Gas Conservation Commission well counts and success rates.
    !             - Recalibrated Alaska oil and gas well drilling and completion costs based on the 2007 API JAS Drilling Cost Data.
    !             - Eliminated unnecessary code and data.
    !             - Reconfigured onshore, State offshore, and Federal offshore production and reserves for existing fields
    !   Sept 2011 Changes:
    !             - Allows North Slope Production = 0 when TAPS falls below TAPS threshold throughput volume
    !                  AND falls below North Slope revenue requirement, both are set in wlalaska.txt
    !   Sept 2012 Changes:
    !             - Expanded ANWR oil field list in wlalaska.txt by adding 2 additional fields, which allows ANWR case to be run
    !                  until 2040 timeframe
    !
    ! *******************************************************************
    ! *******************************************************************
    !
    !   Fuel/Region matrix dimemsions 3 rows x 2 columns; K = fuel column, R = region column
    !       K = 1 --> Oil Column, K = 2 --> Gas Column
    !       R = 1 --> Offshore North Slope, R = 2 Onshore North Slope, R = 3 South Alaska (Cook Inlet)
    !
    IMPLICIT NONE
    INCLUDE 'parametr'        ! nems parameters
    INCLUDE 'ncntrl'          ! nems control variables
    INCLUDE 'ogsmparm'        ! ogsm parameters
    INCLUDE 'ogsmbfw'         ! ogsm system variables
    INCLUDE 'ogsmak'          ! aogss variables
    INCLUDE 'ogsmout'         ! global oil & gas variables, esp. world oil price IT_WOP(T,1)
    INCLUDE 'ngtdmrep'        ! ngtdm global variables
    INCLUDE 'intout'          ! international global variables
    INCLUDE 'pmmout'          ! lfmm global variables

    REAL COSTADJ_AK

    T = CURIYR                 ! values range from 1 to 36 (? MIGHT HAVE BEEN CHANGED TO RUN TO 2050 ?)


    IF (CURITR.EQ.1) THEN          ! Initialization of matrices should only occur onceAllows


        ! Initialize resource base in time T to be equal to the configuration in the prior period when. (8-24-09) EIA RESERVES DATA 1 YEAR LESS
        IF(T.GT.(AKHYR-1))THEN
            DO J=1,AKRESTYPE
                DO K=1,AKRESRGN
                    DO L=1,AKFUEL
                        AKRESOURCES(T,J,K,L) = AKRESOURCES(T-1,J,K,L)
                        ! Adjust gas reserves in 2008 to line up with Advanced Summary
                        !        IF (T.EQ.AKHYR.AND.J.EQ.1.AND.L.EQ.2) AKRESOURCES(T,J,K,L) = AKRESOURCES(T-1,J,K,L)*0.795
                        !              WRITE(bugout,*)'AKRESOURCES2  ',T,L,K,J,AKRESOURCES(T,J,K,L)
                    ENDDO
                ENDDO
            ENDDO
        ENDIF

        ! RESET TOTALS
        DO R=1,AKRGN
            OGPRCOAK(R,T) = 0.
            OGPRNGAK(R,T) = 0.

            DO K=1,AKFUEL
                DO I=1,AKWELL
                    WELLSAK(I,R,K) = 0
                ENDDO
            ENDDO
        ENDDO
        IF(t.le.akhyr) THEN    ! Use historical or STEO production
            DO l=1,akfld
                IF (t.le.akhyr) THEN
                    akcoprd(t,l) = histprdco(l,t) * 0.365   ! Convert oil prod to MMB/YR from MB/D
                ENDIF
                !  RECOVERABLE RESOURCES AS OF 12/31/2008, SO ONLY PRODUCTION FOR 2009 AND LATER
                !  IS SUBTRACTED FROM THE RECOVERABLE RESERVE NUMBERS (7-30-08)
                !     T=19 ==> YEAR=2008.
                IF (t.GE.19) THEN
                    RECRES(L) = (RECRES(L)-AKCOPRD(t,L))  ! *(1+adjadd(l)) code bit used in depletion study.
                ENDIF
            ENDDO
        ELSE                    ! Only examine new field potential after history is over
            ! COMPUTE PRODUCTION FROM CURRENTLY PRODUCING FIELDS
            CALL OGPRO_AK
            ! COMPUTE CURRENT YEAR COSTS
            CALL OGCOST_AK
            ! COMPUTE EXPENDITURES AND PRODUCTION FROM DEVELOPING PROJECTS
            CALL OGDEV_AK
            ! COMPUTE EXPENDITURES FROM DRILLING NEW FIELDS
            CALL OGNEW_AK
            ! Adjust oil resource base for new oil discoveries; resources are moved when the production year equals the current year (9-8-09)
            !    The resources are added to reserves after the field has been discovered and developed.
            DO F=1,AKFLD
                IF(T.GE.(AKHYR - 1)) THEN                         ! Historical oil reserve figures only through AKHYR - 1
                    IF(PROYR(F).EQ.(T + BASEYR - 1)) THEN          ! Base year = 1990, subtract 1 to start with BASEYR-1
                        DO J=1,AKRESRGN   ! Move new oil discoveries to reserves, and apportion by region
                            AKRESOURCES(T,1,J,1) = AKRESOURCES(T,1,J,1) + (RECRES(F) * AKREGPER(F,J))  ! Add to reserves
                            AKRESOURCES(T,3,J,1) = AKRESOURCES(T,3,J,1) - (RECRES(F) * AKREGPER(F,J))  ! Subtract from undiscovered resources
                        ENDDO
                    ENDIF
                ENDIF
            ENDDO
        ENDIF
    ENDIF


    ! COMPUTE TOTAL PRODUCTION BY REGION -- CALLED EVERY ITERATION
    DO R=1,AKRGN
        OGPRCOAK(R,T) = 0.
        OGPRNGAK(R,T) = 0.
    ENDDO

    TOTPRCOAK(T) = 0.
    TOTPRNGAK(T) = 0.

    DO l=1,akfld                                              ! Aggregate oil production from existing fields
        r = fldreg(l)
        OGPRCOAK(R,T) = akcoprd(t,l) + ogprcoak(r,t)
    ENDDO

    !-------------------------------------------------------------------------------------------------
    ! DETERMINE WHETHER ONSHORE & OFFSHORE NORTH SLOPE OIL PRODUCTION CONTINUES, CONTINGENT ON TAPS OIL PIPELINE OPERATING (OR NOT)
    !    TAPS Operational Status determined by conditions 1) whether TAPS throughput falls below TAPSMINIMUM throughput volumes
    !    and 2) Whether North Slope oil production revenues fall below TAPSMINOILREV (TAPS Minimum Oil Revenues)
    !    ADDED July & Aug 2011

    AKWOP(T) = IT_WOP(T,1) * ( 1.10992 / .64819 )                      ! Convert world oil price [IT_WOP(T,1)] from 1987$ into 2010$ for WOP

    !        Calculates North Slope wellhead oil production revenues; first for the offshore and then for the onshore regions
    !             oil production volumes are expressed in million barrels per year

    NSOILREVENUES(T) = (1000000. * OGPRCOAK(1,T) * (AKWOP(T) - TRANSAK(1,1))) + (1000000. * OGPRCOAK(2,T) * (AKWOP(T) - TRANSAK(2,1)))


    IF (PRTDBGL.EQ.1.AND.OGREPORT(29).EQ.1.AND.CURITR.EQ.1) THEN  ! PRINT INTERMEDIATE NS OIL PRODUCTION REVENUE VALUES
        IF (t.eq.akbyr) WRITE(bugout,1118)
        WRITE(BUGOUT,1117) T+BASEYR-1,NSOILREVENUES(T),AKWOP(T),OGPRCOAK(1,T),OGPRCOAK(2,T),TRANSAK(1,1),TRANSAK(2,1)
    ENDIF

1117 FORMAT('TAPS ',I4,F20.1,5F12.1)
1118 FORMAT('TAPS     ','       NSOILREVENUES','         WOP',' NS OFF PROD','  NS ON PROD','   OFF TRANS','    ON TRANS')


    IF (TAPSFLAG.EQ.1) THEN					! test whether AK North Slope oil pipeline is operating ( 1 = yes )

        ! Test whether North Slope oil production is equal to or less than TAPSMINIMUM .AND. NS oil production revenues less than $10 bill in 2010 $

        IF (((OGPRCOAK(1,T) + OGPRCOAK(2,T)).LE.TAPSMINIMUM).AND.(NSOILREVENUES(T).LE.TAPSMINOILREV)) THEN

            TAPSFLAG=0                                               ! Turn off TAPS oil pipeline in future years
            OGPRCOAK(1,T)=0.0                                       ! Set NS Offshore oil production to zero
            OGPRCOAK(2,T)=0.0                                       ! Set NS onshore oil production to zero
            DO L=1,AKFLD
                IF (FLDREG(L).EQ.1.OR.FLDREG(L).EQ.2) THEN            ! Set individual NS oil field production to zero.
                    AKCOPRD(T,L) = 0.0
                ELSE
                ENDIF
            ENDDO
        ELSE
        ENDIF
    ELSE                                                           ! If pipeline is not operating (TAPSFLAG = 0) then make sure NS production is zero.
        OGPRCOAK(1,T)=0.0
        OGPRCOAK(2,T)=0.0
        DO L=1,AKFLD
            IF (FLDREG(L).EQ.1.OR.FLDREG(L).EQ.2) THEN
                AKCOPRD(T,L) = 0.0
            ELSE
            ENDIF
        ENDDO
    ENDIF

    !-------------------------------------------------------------------------------------------------

    ! CONTINUE COMPUTATIONS

    DO r=1,akrgn                                              ! Aggregate gas production from existing fields
        OGPRNGAK(R,t) = ogprngak(r,t) + ogprdng(r+L48RGN+OFFNEMSRGN,t)
    ENDDO

    DO R=1,AKRGN                                             ! Add ANWR oil and gas production to North Slope prod
        IF(T.GT.AKHYR+3) THEN
            RFQDCRD(R+L48RGN+OFFNEMSRGN,T) = OGPRCOAK(R,T)/365.	                ! Convert oil prod to MB/Day
            RFQTDCRD(R+L48RGN+OFFNEMSRGN,T) = OGPRCOAK(R,T)/365.	        ! Convert oil prod to MB/Day
        ELSE
            OGPRCOAK(R,T) = RFQDCRD(R+L48RGN+OFFNEMSRGN,T) * 365.               ! USE HISTORICAL DATA FROM WLHIST.TXT
        ENDIF

        TOTPRCOAK(T) = TOTPRCOAK(T) + OGPRCOAK(R,T)
        TOTPRNGAK(T) = TOTPRNGAK(T) + OGPRNGAK(R,T)

        IF(T.GT.AKHYR) THEN
            OGNGLAK(T) = 0.1 * TOTPRCOAK(T) * 1000 / 365	! Aug 2016 - increased amount of NGL produced per bbl of oil; ratio based on historical
            ! 	     NGL and oil production
            !            OGNGLAK(T) = 0.05914 * TOTPRCOAK(T) * 1000 / 365   ! there are ~5.914 bbl of NGL produced per 100 bbl of oil;  translate into MB/Day
        ENDIF
    ENDDO

    ! DO OIL AND NATURAL GAS RESOURCE ACCOUNTING ONLY ON FIRST INTERATION OF EVERY CYCLE
    IF (CURITR.EQ.1) THEN

        ! COMPUTE TOTAL OIL PRODUCTION AND REMAINING RESERVES BY RESERVE REPORTING REGION; AKRESRGN: 1=ONSHORE, 2=STATE OFFSHORE, 3=FEDERAL OFFSHORE (8-24-09)
        ! Added 8-24-09
        !
        DO R=1,AKRESRGN
            TOTCOPRODREG(T,R)=0
        ENDDO

        ! Oil production accounting by resource region, convert from MB/year to MMB/year

        DO F=1,AKFLD
            DO R=1,AKRESRGN
                TOTCOPRODREG(T,R) = TOTCOPRODREG(T,R) + (AKCOPRD(T,F) * AKREGPER(F,R))
                WRITE(bugout,'(A,3I6,3F12.5)')'TOTCOPRODREGX   ',T,R,F,TOTCOPRODREG(T,R),akcoprd(T,F),AKREGPER(F,R) ! temporary printout
            ENDDO
        ENDDO

        ! TRANSFER TOTAL OIL PRODUCTION BY RESERVE REPORTING REGION TO FTAB REPORTING VARIABLE (OGPRDOFF)
        ! Added Sept 2014
        !
        OGPRDOFF(5,1,T) = TOTCOPRODREG(T,2)
        WRITE(bugout,*) 'ALASKA STATE OFFSHORE OIL   ',T,OGPRDOFF(5,1,T),TOTCOPRODREG(T,2)	! tey sanity check
        OGPRDOFF(4,1,T) = TOTCOPRODREG(T,3)
        WRITE(bugout,*) 'ALASKA FED OFFSHORE OIL   ',T,OGPRDOFF(4,1,T),TOTCOPRODREG(T,3)	! tey sanity check

        ! Oil reserve and resource accounting by resource region
        IF(T.GE.AKHYR)THEN
            DO R=1,AKRESRGN  ! Apportion oil production between reserves and inferred resources based on the relative size of each category
                ! Allocate oil production among reserves
                DO L=1,(AKRESTYPE-1)
                    WRITE(bugout,'(A,3I6,F14.5)')'AKRESOURCESOILX   ',T,L,R,AKRESOURCES(T,L,R,1) ! temporary printout
                    AKRESOURCES(T,L,R,1) = AKRESOURCES(T,L,R,1) - &
                        (TOTCOPRODREG(T,R) * (AKRESOURCES(T,L,R,1) / &
                        (AKRESOURCES(T,1,R,1) + AKRESOURCES(T,2,R,1)) ))
                    IF (AKRESOURCES(T,2,R,1).LE.0.0) THEN   ! If inferred resources <= 0, add these negative resources to reserves.
                        AKRESOURCES(T,1,R,1) = AKRESOURCES(T,1,R,1) + AKRESOURCES(T,2,R,1)
                        AKRESOURCES(T,2,R,1) = 0.0
                    ENDIF
                    WRITE(bugout,'(A,3I6,F14.5)')'AKRESOURCESOILX   ',T,L,R,AKRESOURCES(T,L,R,1) ! temporary printout
                ENDDO
            ENDDO
        ENDIF


        ! COMPUTE TOTAL NATURAL GAS PRODUCTION BY RESERVE REPORTING REGION; AKRESRGN: 1=ONSHORE, 2=STATE OFFSHORE, 3=FEDERAL OFFSHORE (8-24-09)
        ! Added 8-24-09

        DO R=1,AKRESRGN
            TOTNGPRODREG(T,R)=0
        ENDDO

        !   Apportion Cook Inlet gas production; OGPRDNG(L48RGN+OFFNEMSRGN+3,CURIYR)= Southern AK gas production/consumption
        !     AKREGPER(23,AKRESREG)  AKFLD = 23 is Cook Inlet
        DO R=1,AKRESRGN
            TOTNGPRODREG(T,R)= TOTNGPRODREG(T,R) + (OGPRDNG(L48RGN+OFFNEMSRGN+3,T) * AKREGPER(23,R))
            WRITE(bugout,*)'TOTNGPRODREGX   ',T,R,TOTNGPRODREG(T,R),OGPRDNG(L48RGN+OFFNEMSRGN+3,T),AKREGPER(23,R)  ! temporary printout
        ENDDO

        !   Apportion North Slope gas production;
        !        OGPRDNG(L48RGN+OFFNEMSRGN+2,CURIYR)= North Slope AK gas production/consumption
        !        GAK_ALB(curiyr) = Alaska pipeline gas coming into Alberta, in Bcf per year
        IF (GAK_ALB(T).le.0.0) THEN
            TOTNGPRODREG(T,1)= TOTNGPRODREG(T,1) + OGPRDNG(L48RGN+OFFNEMSRGN+2,T) ! Prior to the North Slope Pipeline all NS gas production assumed onshore
        ELSE
            TOTNGPRODREG(T,1)= TOTNGPRODREG(T,1) + (0.9 * OGPRDNG(L48RGN+OFFNEMSRGN+2,T)) !After gas PL, production is 90% onshore & 10% state offshore
            TOTNGPRODREG(T,2)= TOTNGPRODREG(T,2) + (0.1 * OGPRDNG(L48RGN+OFFNEMSRGN+2,T)) !Proportions based on current reserve levels
        ENDIF
        WRITE(bugout,*)'TOTNGPRODREGX   ',T,R,TOTNGPRODREG(T,1),TOTNGPRODREG(T,2),OGPRDNG(L48RGN+OFFNEMSRGN+2,T)  ! temporary printout


        !   When the AK gas pipeline comes on-line inferred resources are moved to reserves at 30% per year of the remaining inferred resources.
        IF (GAK_ALB(T).gt.0.0) THEN
            DO L=1,AKRESRGN
                AKRESOURCES(T,1,L,2) = AKRESOURCES(T,1,L,2) + ( 0.3 * AKRESOURCES(T,2,L,2))  ! Add to reserves
                AKRESOURCES(T,2,L,2) = AKRESOURCES(T,2,L,2) - ( 0.3 * AKRESOURCES(T,2,L,2))  ! Subtract from inferred resources
                WRITE(bugout,*)'AKRESOURCESGASX   ',T,L,AKRESOURCES(T,1,L,2)
            ENDDO
        ENDIF

        ! Natural gas reserve and resource accounting by resource region; deduct natural gas production from reserves
        IF(T.GE.AKHYR)THEN
            DO R=1,AKRESRGN
                AKRESOURCES(T,1,R,2) = AKRESOURCES(T,1,R,2) - TOTNGPRODREG(T,R)
                !                WRITE(bugout,*)'AKRESOURCESGAS1  ',T,L,R,AKRESOURCES(T,L,R,2),TOTNGPRODREG(T,R) ! temporary printout
            ENDDO
        ENDIF

        ! Load Beginning of Year Reserve Results (BOYRESCOAK/BOYRESNGAK) and Reserve Additions (RESADCOAK/RESADNGAK) from AKRESOURCES matrix.
        ! Note that all reserve information for Alaska (i.e., total Alaska) is loaded into the first column of each of the four variables,
        !   so the results for each AK region (i.e., North Slope Offshore, North Slope Onshore, South Alaska) are not provided.
        !   (7-1-10)

        IF (T.GE.AKHYR) THEN
            DO R=1,AKRESRGN
                BOYRESCOAK(1,T) = BOYRESCOAK(1,T) + AKRESOURCES(T,1,R,1)
                BOYRESNGAK(1,T) = BOYRESNGAK(1,T) + AKRESOURCES(T,1,R,2)

                ! Reserve additions equal the change in both inferred and undiscovered resources

                RESADCOAK(1,T) = RESADCOAK(1,T) + (AKRESOURCES(T-1,2,R,1) - AKRESOURCES(T,2,R,1)) &
                    + (AKRESOURCES(T-1,3,R,1) - AKRESOURCES(T,3,R,1))

                RESADNGAK(1,T) = RESADNGAK(1,T) + (AKRESOURCES(T-1,2,R,2) - AKRESOURCES(T,2,R,2)) &
                    + (AKRESOURCES(T-1,3,R,2) - AKRESOURCES(T,3,R,2))
            ENDDO
        ENDIF


        ! Load Global variables with oil and gas reserve and resource numbers, divide by 1,000 to translate into billion BBL and Tcf, year-end figures.
        !             Differentiate regions in resource global variables with 11 = Onshore, 12 = State Offshore, 13 = Federal Offshore

        ! Initialize global reserve and resource matrices
        DO R=1,AKRESRGN
            DO F=1,AKFUEL
                OGEOYRSV(R+L48RGN+OFFNEMSRGN,F,T) = 0.0
                OGEOYINF(R+L48RGN+OFFNEMSRGN,F,T) = 0.0
                OGEOYURR(R+L48RGN+OFFNEMSRGN,F,T) = 0.0
            ENDDO
        ENDDO

        ! Load oil and gas reserve
        DO R=1,AKRESRGN
            DO F=1,AKFUEL
                OGEOYRSV(R+L48RGN+OFFNEMSRGN,F,T) = OGEOYRSV(R+L48RGN+OFFNEMSRGN,F,T) + (AKRESOURCES(T,1,R,F) / 1000)
                WRITE(bugout,*)'OGEOYRSVX   ',R,F,T,OGEOYRSV(R+L48RGN+OFFNEMSRGN,F,T)
            ENDDO
        ENDDO

        ! Load inferred resources
        DO R=1,AKRESRGN
            DO F=1,AKFUEL
                OGEOYINF(R+L48RGN+OFFNEMSRGN,F,T) = OGEOYINF(R+L48RGN+OFFNEMSRGN,F,T) + (AKRESOURCES(T,2,R,F) / 1000)
                WRITE(bugout,*)'OGEOYINFX   ',R,F,T,OGEOYINF(R+L48RGN+OFFNEMSRGN,F,T)
            ENDDO
        ENDDO

        ! Load undiscovered resources
        DO R=1,AKRESRGN
            DO F=1,AKFUEL
                OGEOYURR(R+L48RGN+OFFNEMSRGN,F,T) = OGEOYURR(R+L48RGN+OFFNEMSRGN,F,T) + (AKRESOURCES(T,3,R,F) / 1000)
                WRITE(bugout,*)'OGEOYURRX   ',R,F,T,OGEOYURR(R+L48RGN+OFFNEMSRGN,F,T)
            ENDDO
        ENDDO

    ENDIF   ! End resource accounting that occurs only during the first iteration of every cycle.


    ! ASSIGN ANWR PRODUCTION TO GLOBAL VARIABLE FOR FTAB
    OGQCRREP(5,t) = 0.
    DO F=51,62                                              ! Changed 9-14-12; added 2 new ANWR, extension needed for 2040 timeframe.
        OGQCRREP(5,t) = OGQCRREP(5,t) + akcoPRD(t,F)        ! MMB
    ENDDO
    WRITE(bugout,*) 'anwr', t+BASEYR-1, ogqcrrep(5,t)/365.

    !-------------------------------------------------------------------------------------------------!

    ! Prints out Oil Production by field in MB/Day.  Also prints out cost adj factor. Revised 10/2007.

    IF (PRTDBGL.EQ.1.AND.OGREPORT(29).EQ.1.AND.CURITR.EQ.1) THEN
        IF (t.eq.akbyr) WRITE(bugout,350)
        WRITE(BUGOUT,352) T+BASEYR-1,(akcoPRD(t,F)/.365,F=1,20)
    ENDIF
    IF (PRTDBGL.EQ.1.AND.OGREPORT(29).EQ.1.AND.CURITR.EQ.1) THEN
        IF (t.eq.akbyr) WRITE(bugout,360)
        WRITE(BUGOUT,353) T+BASEYR-1,(akcoPRD(t,F)/.365,F=21,40)
    ENDIF
    IF (PRTDBGL.EQ.1.AND.OGREPORT(29).EQ.1.AND.CURITR.EQ.1) THEN
        IF (t.eq.akbyr) WRITE(bugout,370)
        WRITE(BUGOUT,354) T+BASEYR-1,(akcoPRD(t,F)/.365,F=41,62),COSTADJ_AK(T)
    ENDIF

    IF (PRTDBGL.EQ.1.AND.OGREPORT(29).EQ.1.AND.CURITR.EQ.1) THEN
        IF (t.eq.akbyr) WRITE(bugout,381)
        WRITE(BUGOUT,380) T+BASEYR-1,TOTPRCOAK(t),OGNGLAK(t)
    ENDIF


352 FORMAT('AKPR1',I4,20F9.2)
353 FORMAT('AKPR2',I4,20F9.2)
354 FORMAT('AKPR3',I4,23F9.2)
380 FORMAT('AKNGL',I4,2F9.2)

350 FORMAT('AKPR1    ','   BADAMI',' COLVILLE','  DUCK-IU','    Empty','  KUPARUK', &
        '   W. SAK','  TABASCO','     TARN','  KRU-SAT',' MILNE PT',' SCHRADER', &
        '  N. STAR','  PRUDHOE','  PBU-SAT',' LISBURNE','   NIAKUK',' McINTYRE', &
        '  THOMSON',' OOOGURUK',' NIKAITCH')

360 FORMAT('AKPR2    ','  LIBERTY','     NUMA','     COOK','    GMT-1','    GMT-2', &
        '    PIKKA','    UMIAT','   QUGRUK','SMITH BAY','   WILLOW', &
        'HORSESHOE','     PUTU','    PIKKA','  MUSTANG','   KOLOA2','   ALKAID', &
        'TYONEK DP','    SoAK1','    SoAK2','    SoAK3')

370 FORMAT('AKPR3    ',' OnNorth1',' OnNorth2',' OnNorth3',' OnNorth4',' OnNorth5', &
        ' OfNorth1',' OfNorth2',' OfNorth3',' OfNorth4',' OfNorth5','   ANWR-1', &
        '   ANWR-2','   ANWR-3','   ANWR-4','   ANWR-5', &
        '   ANWR-6','   ANWR-7','   ANWR-8','   ANWR-9','  ANWR-10','  ANWR-11','  ANWR-12',' Cost Adj')

381 FORMAT('AKNGL    ','TOTPRCOAK','AKNGLPROD')

    !------------------------------------------------------------------------------------------

    ! Prints out remaining Oil Reserves in existing Alaskan fields, in MMB/year.

    IF (PRTDBGL.EQ.1.AND.OGREPORT(29).EQ.1.AND.CURITR.EQ.1) THEN
        IF (t.eq.akbyr) WRITE(bugout,351)
        WRITE(BUGOUT,355) T+BASEYR-1,(RECRES(F),F=1,20)
    ENDIF
    IF (PRTDBGL.EQ.1.AND.OGREPORT(29).EQ.1.AND.CURITR.EQ.1) THEN
        IF (t.eq.akbyr) WRITE(bugout,361)
        WRITE(BUGOUT,356) T+BASEYR-1,(RECRES(F),F=21,40)
    ENDIF
    IF (PRTDBGL.EQ.1.AND.OGREPORT(29).EQ.1.AND.CURITR.EQ.1) THEN
        IF (t.eq.akbyr) WRITE(bugout,371)
        WRITE(BUGOUT,357) T+BASEYR-1,(RECRES(F),F=41,62)
    ENDIF

355 FORMAT('AKRS1',I4,20F9.2)
356 FORMAT('AKRS2',I4,20F9.2)
357 FORMAT('AKRS3',I4,22F9.2)

351 FORMAT('AKRS1    ','   BADAMI',' COLVILLE','  DUCK-IU','    Empty','  KUPARUK', &
        '   W. SAK','  TABASCO','     TARN','  KRU-SAT',' MILNE PT',' SCHRADER', &
        '  N. STAR','  PRUDHOE','  PBU-SAT',' LISBURNE','   NIAKUK',' McINTYRE', &
        '  THOMSON',' Oooguruk',' Nikaitch')

361 FORMAT('AKRS2    ','  Liberty','    Empty','     Cook',' OnNorth1',' OnNorth2', &
        ' OnNorth3',' OnNorth4',' OnNorth5',' OnNorth6',' OnNorth7', &
        ' OnNorth8',' OnNorth9',' OnNort10',' OnNort11','    SoAK1','    SoAK2', &
        '    SoAK3','    SoAK4',' OfNorth1',' OfNorth2')

371 FORMAT('AKRS3    ',' OfNorth3',' OfNorth4',' OfNorth5',' OfNorth6',' OfNorth7', &
        ' OfNorth8',' OfNorth9',' OfNort10',' OfNort11',' OfNort12','   ANWR-1', &
        '   ANWR-2','   ANWR-3','   ANWR-4','   ANWR-5', &
        '   ANWR-6','   ANWR-7','   ANWR-8','   ANWR-9','  ANWR-10','  ANWR-11','  ANWR-12')

    ! Prints out total oil production "TOTCOPRODREG(MNUNYR,AKRESRGN)" and total gas production "TOTNGPRODREG(MNUMYR,AKRESRGN)"  (8-27-09)
    !    by resource region 1=onshore, 2=state offshore, 3=federal offshore

    IF (PRTDBGL.EQ.1.AND.OGREPORT(29).EQ.1.AND.CURITR.EQ.1) THEN
        IF (t.eq.akbyr) WRITE(bugout,205)
        WRITE(BUGOUT,206) T+BASEYR-1,(TOTCOPRODREG(T,J),J=1,3),(TOTNGPRODREG(T,K),K=1,3)
    ENDIF

206 FORMAT('AKPRODX  ',I4,3F9.1,'   ***   ',3F9.1)

205 FORMAT('AKPRODX  YEAR','  OILONSH','  OILSTOF','  OILFDOF','   ***   ','  GASONSH','  GASSTOF','  GASFDOF')

    !---------------------------------------------------------------------------------------------------------------

    ! Prints out remaining Oil and Natural Gas Reserves, Inferred Resources, and Undiscovered Resources in MMB & Bcf.
    !   Years across columns, Rows across Resource types 1=reserves, 2=inferred resources, 3=undiscovered resources

    IF (PRTDBGL.EQ.1.AND.OGREPORT(29).EQ.1.AND.CURITR.EQ.1) THEN
        IF (t.eq.1) WRITE(bugout,201)
        WRITE(BUGOUT,202) T+BASEYR-1,(AKRESOURCES(T,J,1,1),J=1,3),(AKRESOURCES(T,K,2,1),K=1,3),(AKRESOURCES(T,L,3,1),L=1,3)
    ENDIF
    IF (PRTDBGL.EQ.1.AND.OGREPORT(29).EQ.1.AND.CURITR.EQ.1) THEN
        IF (t.eq.1) WRITE(bugout,203)
        WRITE(BUGOUT,204) T+BASEYR-1,(AKRESOURCES(T,J,1,2),J=1,3),(AKRESOURCES(T,K,2,2),K=1,3),(AKRESOURCES(T,L,3,2),L=1,3)
    ENDIF

202 FORMAT('OILRESX    ',I4,3F9.1,'   ***   ',3F9.1,'   ***   ',3F9.1)
204 FORMAT('GASRESX    ',I4,3F9.1,'   ***   ',3F9.1,'   ***   ',3F9.1)

    ! Oil Reserve/Resource Accounting
201 FORMAT('OILRESX    YEAR','  ONSHRES','  ONSHINF','  ONSHUND', &    ! ONSH = Onshore, STOF = State Offshore, FDOF = Federal Offshore
    '   ***   ','  STOFRES','  STOFINF','  STOFUND', &          ! RES = Reserves, INF = Inferred Resources, UND = Undiscovered Resources
    '   ***   ','  FDOFRES','  FDOFINF','  FDOFUND')

    ! Natural Gas Reserve/Resource Accounting
203 FORMAT('GASRESX    YEAR','  ONSHRES','  ONSHINF','  ONSHUND', &    ! ONSH = Onshore, STOF = State Offshore, FDOF = Federal Offshore
    '   ***   ','  STOFRES','  STOFINF','  STOFUND', &          ! RES = Reserves, INF = Inferred Resources, UND = Undiscovered Resources
    '   ***   ','  FDOFRES','  FDOFINF','  FDOFUND')

    !-------------------------------------------------------------------------------------------------------------------


    ! Prints out BOYRESCOAK, BOYRESNGAK, RESADCOAK, RESADNGAK - beginning of year reserves for oil and gas, reserve additions for oil and gas
    !   Years across columns, Rows across Resource types 1=reserves, 2=inferred resources, 3=undiscovered resources

    IF (PRTDBGL.EQ.1.AND.OGREPORT(29).EQ.1.AND.CURITR.EQ.1) THEN
        IF (t.eq.1) WRITE(bugout,3334)
        WRITE(BUGOUT,3333) T+BASEYR-1,(BOYRESCOAK(R,T),R=1,3),(BOYRESNGAK(R,T),R=1,3),(RESADCOAK(R,T),R=1,3),(RESADNGAK(R,T),R=1,3)
    ENDIF
    
3333 FORMAT('RESAKX   ',I4,3F10.1,' *** ',3F10.1,' *** ',3F10.1,' *** ',3F10.1)

    ! Oil Reserve/Resource Accounting
3334 FORMAT('RESAKX   YEAR',' BOYRESCO1',' BOYRESCO2',' BOYRESCO3',' *** ', &
        ' BOYRESNG1',' BOYRESNG2',' BOYRESNG3',' *** ', &
        '  RESADCO1','  RESADCO2','  RESADCO3',' *** ', &
        '  RESADNG1','  RESADNG2','  RESADNG3')

    END SUBROUTINE OGFOR_AK
    
    !******************************************************************!
    !
    !******************************************************************!
    SUBROUTINE OGCOST_AK
    !
    !         Adjusts oil and gas drilling, equipment and operating costs
    !         for high/low tech cases and accelerated depletion runs
    !
    IMPLICIT NONE
    INCLUDE 'parametr'        ! nems parameters
    INCLUDE 'ncntrl'          ! nems control variables
    INCLUDE 'ogsmparm'        ! ogsm parameters
    INCLUDE 'ogsmbfw'         ! ogsm system variables
    INCLUDE 'ogsmak'          ! aogss variables
    INCLUDE 'ogsml48'         !

    REAL DRLTECH(AKFUEL)       ! HOLDS TECH FACTORS
    REAL LEQTECH(AKFUEL)       ! HOLDS TECH FACTORS
    REAL OPRTECH(AKFUEL)       ! HOLDS TECH FACTORS
    REAL COSTADJ_AK            ! cost adjustment factor for Cost Case scenarios

    ! LOOP OVER ALASKA FUEL TYPE, REGION AND WELL CLASS
    DO K=1,AKFUEL
        DO R=1,AKRGN
            DO I=1,AKWELL
                ! SHIFT BASE COSTS ACCORDING TO HI/LO/BASE CASES
                ! dry hole costs set equal to successful well costs, 8/14/02
                ! COSTADJ is a global variable.
                IF (CURIYR.EQ.1) THEN
                    DRILLAK(I,R,K) = DRILLAK(I,R,K)*(1+COSTADJ)
                    DRYAK(I,R,K) = DRILLAK(I,R,K)*(1+COSTADJ)
                    DRLNFWAK(R,K) = DRLNFWAK(R,K)*(1+COSTADJ)
                ENDIF
                !
                ! COMPUTE TECHNOLOGY ADJUSTMENT FACTOR
                IF (T.GE.TECHYR) THEN
                    DRLTECH(K) = TECHAK(1) * TECHADJ_AK(K,2)
                    LEQTECH(K) = TECHAK(2) * TECHADJ_AK(K,2)
                    OPRTECH(K) = TECHAK(3) * TECHADJ_AK(K,2)
                ELSEIF (T.GT.1) THEN
                    DRLTECH(K) = TECHAK(1)
                    LEQTECH(K) = TECHAK(2)
                    OPRTECH(K) = TECHAK(3)
                ELSE
                    DRLTECH(K) = 0
                    LEQTECH(K) = 0
                    OPRTECH(K) = 0
                ENDIF
                ! COMPUTE AVERAGE COST OF DRILLING SUCCESSFUL WELLS
                DRILLAK(I,R,K) = DRILLAK(I,R,K)*(1-DRLTECH(K))
                ! COMPUTE AVERAGE COST OF DRILLING DRY HOLES
                ! dry hole costs set equal to successful well costs, 8/14/02
                DRYAK(I,R,K) = DRILLAK(I,R,K)
                ! COMPUTE LEASE EQUIPMENT COSTS
                !           EQUIPAK(I,R,K) = EQUIPAK(I,R,K)*(1-LEQTECH(K))   ! commented out because is zero in wlalaska.txt, 8/8/02
                IF (K.EQ.1) THEN
                    IF (OGRUNOP(2).eq.11.AND.curiyr.ge.techyr) DRILLAK(I,R,K) = (COSTADJ_AK(T)*DRILLAK(I,R,K)) + FEDRILL_OIL(1,7,curiyr-techyr+1)     ! FE TECHNOLOGY ADJUSTMENT
                    IF (OGRUNOP(2).eq.12.AND.curiyr.ge.techyr) DRILLAK(I,R,K) = (COSTADJ_AK(T)*DRILLAK(I,R,K)) + FEDRILL_OIL(2,7,curiyr-techyr+1)     ! FE TECHNOLOGY ADJUSTMENT
                    IF (OGRUNOP(2).eq.11.AND.curiyr.ge.techyr) DRYAK(I,R,K) = (COSTADJ_AK(T)*DRYAK(I,R,K)) + FEDRILL_OIL(1,7,curiyr-techyr+1)     ! FE TECHNOLOGY ADJUSTMENT
                    IF (OGRUNOP(2).eq.12.AND.curiyr.ge.techyr) DRYAK(I,R,K) = (COSTADJ_AK(T)*DRYAK(I,R,K)) + FEDRILL_OIL(2,7,curiyr-techyr+1)     ! FE TECHNOLOGY ADJUSTMENT
                ELSE
                    IF (OGRUNOP(2).eq.11.AND.curiyr.ge.techyr) DRILLAK(I,R,K) = (COSTADJ_AK(T)*DRILLAK(I,R,K)) + FEDRILL_GAS(1,7,curiyr-techyr+1)     ! FE TECHNOLOGY ADJUSTMENT
                    IF (OGRUNOP(2).eq.12.AND.curiyr.ge.techyr) DRILLAK(I,R,K) = (COSTADJ_AK(T)*DRILLAK(I,R,K)) + FEDRILL_GAS(2,7,curiyr-techyr+1)     ! FE TECHNOLOGY ADJUSTMENT
                    IF (OGRUNOP(2).eq.11.AND.curiyr.ge.techyr) DRYAK(I,R,K) = (COSTADJ_AK(T)*DRYAK(I,R,K)) + FEDRILL_GAS(1,7,curiyr-techyr+1)     ! FE TECHNOLOGY ADJUSTMENT
                    IF (OGRUNOP(2).eq.12.AND.curiyr.ge.techyr) DRYAK(I,R,K) = (COSTADJ_AK(T)*DRYAK(I,R,K)) + FEDRILL_GAS(2,7,curiyr-techyr+1)     ! FE TECHNOLOGY ADJUSTMENT
                ENDIF
            ENDDO
            ! COMPUTE AVERAGE COST OF DRILLING A NEW FIELD WILDCAT
            DRLNFWAK(R,K) = DRLNFWAK(R,K)*(1-DRLTECH(K))
            IF (K.EQ.1) THEN
                IF (OGRUNOP(2).eq.11.AND.curiyr.ge.techyr) DRLNFWAK(R,K) = (COSTADJ_AK(T)*DRLNFWAK(R,K)) + FEDRILL_OIL(1,7,curiyr-techyr+1)     ! FE TECHNOLOGY ADJUSTMENT
                IF (OGRUNOP(2).eq.12.AND.curiyr.ge.techyr) DRLNFWAK(R,K) = (COSTADJ_AK(T)*DRLNFWAK(R,K)) + FEDRILL_OIL(2,7,curiyr-techyr+1)     ! FE TECHNOLOGY ADJUSTMENT
            ELSE
                IF (OGRUNOP(2).eq.11.AND.curiyr.ge.techyr) DRLNFWAK(R,K) = (COSTADJ_AK(T)*DRLNFWAK(R,K)) + FEDRILL_GAS(1,7,curiyr-techyr+1)     ! FE TECHNOLOGY ADJUSTMENT
                IF (OGRUNOP(2).eq.12.AND.curiyr.ge.techyr) DRLNFWAK(R,K) = (COSTADJ_AK(T)*DRLNFWAK(R,K)) + FEDRILL_GAS(2,7,curiyr-techyr+1)     ! FE TECHNOLOGY ADJUSTMENT
            ENDIF
        ENDDO
    ENDDO
    ! COMPUTE TRANSPORTATION COSTS
    ! >>>>>>> TEMPORARILY ASSIGNED IN SUBROUTINE OGINIT_AK
    ! COMPUTE OPERATING COSTS
    DO L = 1,4
        DO M = 1,2
            OPERAK(L,M) = OPERAK(L,M)*(1-OPRTECH(1))
        ENDDO
        IF (OGRUNOP(2).eq.11.AND.curiyr.ge.techyr) OPERAK(L,1) = OPERAK(R,K) + FEOPR_OIL(1,7,curiyr-techyr+1)     ! FE TECHNOLOGY ADJUSTMENT
        IF (OGRUNOP(2).eq.12.AND.curiyr.ge.techyr) OPERAK(L,1) = OPERAK(R,K) + FEOPR_OIL(2,7,curiyr-techyr+1)     ! FE TECHNOLOGY ADJUSTMENT
    ENDDO
    
    END SUBROUTINE OGCOST_AK
    
    !******************************************************************!
    !
    !******************************************************************!
    SUBROUTINE OGNEW_AK
    !
    ! UPDATED 3/21/2001 - to remove the all fields at once effect.
    !
    IMPLICIT NONE
    INCLUDE 'parametr'        ! nems parameters
    INCLUDE 'ncntrl'          ! nems control variables
    INCLUDE 'ogsmparm'        ! ogsm parameters
    INCLUDE 'ogsmbfw'         ! ogsm system variables
    INCLUDE 'ogsmak'          ! aogss variables
    INCLUDE 'ogsmout'         ! global oil & gas variables, esp. world oil price IT_WOP(T,1)
    INCLUDE 'intout'          ! international global variables
    !
    REAL TEMP                        ! COUNTER
    REAL PROF_AK(AKFUEL,AKRGN)       ! PROFITABILITY MEASURE
    REAL DCF_AK(AKWELL,AKRGN,AKFUEL) ! DCF
    REAL COST_AK                     ! DRILLING COST
    REAL NAK_NFW(MNUMYR)             ! NORTH ALASKA EXPLORATION WELLS (both new field/delineation wells and total onshore/offshore)
    INTEGER NFW(AKFUEL,AKRGN)        ! NEW FIELD WILDCATS DRILLED EACH YEAR
    INTEGER MINNFW                   ! MINIMUM # OF NFW'S DRILLED
    INTEGER TOTNFW(AKFUEL,AKRGN)     ! TOTAL # OF NFW'S DRILLED
    LOGICAL DRLNFW(AKFUEL,AKRGN)     ! NFW DRILLING FLAG
    LOGICAL MOVED1                   ! FLAG FOR MOVING FIELDS TO DEVELOPMENT, Offshore North
    LOGICAL MOVED2                   ! FLAG FOR MOVING FIELDS TO DEVELOPMENT, Onshore North
    LOGICAL MOVED3                   ! FLAG FOR MOVING FIELDS TO DEVELOPMENT, South Alaska
    !
    DATA DRLNFW/6*.TRUE./
    DATA MOVED1/.false./
    DATA MOVED2/.false./
    DATA MOVED3/.false./
    DATA TOTNFW/6*0/
    DATA NFW/6*0/
    !
    SPENDNF = 0

    ! DETERMINE NUMBER OF NEW FIELD WILDCATS (NFW) DRILLED IN YEAR = T
    !    First bring in world oil price and determine North Slope exploration wells drilled
    !    Note that NFW for gas are set to zero in the data statement and remain unchanged throughout projections
    !
    WOP(T) = IT_WOP(T,1) * ( 1.22422 / .73196 )   ! Convert world oil price [IT_WOP(T,1)] from 1987$ into 2008$ for WOP

    NAK_NFW(T) = 0.77 + (.13856 * WOP(T))             ! Based on 1977 - 2008 Exploration wells drilled, both wildcat and delineation.

    IF (T.GT.AKHYR) THEN
        NFW(1,1) = NINT(NS_OFF_NFW(T-AKHYR) * NAK_NFW(T))
        NFW(1,2) = NINT((1 - NS_OFF_NFW(T-AKHYR)) * NAK_NFW(T))
        NFW(1,3) = 3
    ENDIF
    !
    ! DISCOVER NEW FIELD ONLY IF ALL NEW FIELD WILDCATS HAVE BEEN
    ! DRILLED FOR THE PREVIOUS DISCOVERY
    DO K = 1,AKFUEL
        DO R = 1,AKRGN
            IF (DRLNFW(K,R).AND.T.GT.AKHYR) THEN
                ! USING THE STREAMLINED DCF ROUTINE, COMPUTE DCF AND COSTS
                ! FOR THE LARGEST FIELD IN EACH REGION
                IF (K.EQ.1.AND.FSZCOAK(1,R).GT.0) THEN
                    ! COMPUTE EXPECTED PROFITABILITY - CRUDE OIL FIELDS
                    CALL OGDCF_AK(1,R,1,DCF_AK,COST_AK,FSZCOAK(1,R))
                    PROF_AK(1,R) = DCF_AK(1,R,1)/COST_AK
                ENDIF
                IF (K.EQ.2.AND.FSZNGAK(1,R).GT.0) THEN
                    ! COMPUTE EXPECTED PROFITABILITY - NATURAL GAS FIELDS
                    CALL OGDCF_AK(1,R,2,DCF_AK,COST_AK,FSZNGAK(1,R))
                    PROF_AK(2,R) = DCF_AK(1,R,2)/COST_AK
                ENDIF
            ENDIF
            ! IF PROFITABILITY IS POSITIVE, DISCOVER FIELD & DRILL REQUIRED NFW'S
            IF (PROF_AK(K,R).GE.0) THEN
                DRLNFW(K,R) = .FALSE.
                TOTNFW(K,R) = NFW(K,R) + TOTNFW(K,R)
                !
                ! IF ALL NFW'S HAVE BEEN DRILLED THEN TRANSFER SIZE TO DEVELOPMENT ARRAY
                !   NINT converts to nearest integer, uses success rates for exploratory wells, by region,
                !      by fuel, so an exploratory well success rate of .1666 translates into an integer of 6
                !
                MINNFW = NINT(1/SRAK(1,R,K))
                IF (TOTNFW(K,R).GE.MINNFW) THEN  ! when min NFW exceeded then add to
                    ! NDP (new development projects), algorithm only
                    ! allows 1 new development project per cycle
                    NDP(K,R) = NDP(K,R) + 1
                    IF (K.EQ.1) THEN			                                 ! Oil Only
                        DO F=1,AKFLD
                            IF (FLDREG(F) .EQ. R) THEN
                                IF (R.EQ.2) THEN                                         ! Onshore North
                                    IF (recres(f).lt.-99.0.and..not.MOVED2) THEN           ! MOVE# initialized false
                                        MOVED2 = .true.
                                        recres(f) = fszcoak(1,r)                             ! Fill in reserves for new field
                                        proyr(f) = baseyr + t + 2                            ! Two year delay between discovery & production
                                    ENDIF
                                ELSEIF (r.eq.1) THEN                                    ! Offshore North
                                    IF (recres(f).le.0.0.and..not.MOVED1) THEN
                                        IF ((baseyr + t + 3).lt.2017) THEN                   ! Added on 12-19-08 because OffNorth field went online in low case
                                            ! before doing so in the reference and high price cases
                                        ELSE                                                 ! Added on 12-19-08
                                            MOVED1 = .true.
                                            recres(f) = fszcoak(1,r)
                                            proyr(f) = baseyr + t + 3
                                        ENDIF                                                ! Added on 12-19-08
                                    ENDIF
                                ELSEIF (r.eq.3) THEN                                    ! South Alaska
                                    IF (recres(f).le.0.0.and..not.MOVED3) THEN
                                        MOVED3 = .true.
                                        recres(f) = fszcoak(1,r)
                                        proyr(f) = baseyr + t + 2
                                    ENDIF
                                ENDIF
                            ENDIF
                        ENDDO
                        MOVED1 = .false.
                        MOVED2 = .false.
                        MOVED3 = .false.
                        RESADCOAK(R,T) = FSZCOAK(1,R)
                        DO M = 1,AKSIZE-1
                            FSZCOAK(M,R) = FSZCOAK(M+1,R)
                        ENDDO
                        FSZCOAK(AKSIZE,R) = 0.0
                    ENDIF
                    DRLNFW(K,R) = .TRUE.
                    TOTNFW(K,R) = TOTNFW(K,R) - MINNFW

                ENDIF
            ENDIF

        ENDDO
    ENDDO

    ! Temporary Debugging Printouts for OGNEW_AK

    IF (PRTDBGL.EQ.1.AND.OGREPORT(29).EQ.1.AND.CURITR.EQ.1) THEN
        IF ((T+BASEYR-1).eq.akbyr) WRITE(bugout,2200)
        WRITE(BUGOUT,2201) T+BASEYR-1,(T-AKHYR),WOP(T),NAK_NFW(T),NS_OFF_NFW(T-AKHYR),(NFW(1,R),R=1,3),(FSZCOAK(1,R),R=1,3),(PROF_AK(1,R),R=1,3),(TOTNFW(1,R),R=1,3)
    ENDIF

2201 FORMAT('OGNEWX ',I4,I9,3F9.2,3I9,6F9.1,3I9)

2200 FORMAT('OGNEWX     ','  T-AKHYR','      WOP','  NAK_NFW',' NSOFFNFW',' NFW(1,1)',' NFW(1,2)',' NFW(1,3)', &
        ' FSZ(1,1)',' FSZ(1,2)',' FSZ(1,3)',' PRF(1,1)',' PRF(1,2)',' PRF(1,3)', &
        '  TOTNFW1','  TOTNFW2','  TOTNFW3')
        
    END SUBROUTINE OGNEW_AK
    
    !******************************************************************!
    !
    !******************************************************************!
    SUBROUTINE OGDEV_AK
    !
    IMPLICIT NONE
    INCLUDE 'parametr'        ! nems parameters
    INCLUDE 'ncntrl'          ! nems control variables
    INCLUDE 'ogsmparm'        ! ogsm parameters
    INCLUDE 'ogsmbfw'         ! ogsm system variables
    INCLUDE 'ogsmak'          ! aogss variables
    INCLUDE 'ogsml48'         ! variables
    INCLUDE 'ogsmout'         ! to obtain world oil price
    !
    REAL DCF_AK(AKWELL,AKRGN,AKFUEL) 	! DCF
    REAL COST_AK                     	! DRILLING COST
    REAL PROF_AK(AKFUEL,AKFLD)
    REAL PROD_AK(AKFUEL,AKfld)
    REAL WELLSSUC(AKFUEL,AKfld)
    REAL WELLSSUC1(AKFUEL,AKfld)
    REAL OPERCOST(AKFUEL,AKfld)
    REAL NUMEXP
    REAL NUMDEV
    REAL CSUCEXP                        ! COST ASSOCIATED WITH SUCCESSFUL EXP WELLS
    REAL CSUCDEV                        ! COST ASSOCIATED WITH SUCCESSFUL DEV WELLS
    REAL CDRYEXP                        ! COST ASSOCIATED WITH DRY EXP WELLS
    REAL CDRYDEV                        ! COST ASSOCIATED WITH DRY DEV WELLS
    REAL FACOST                         ! FACILITY COST
    REAL OGFAC_AK                       ! FACILITY COST SUBROUTINE
    REAL CUMPRD(AKFUEL,AKfld)           ! CUMULATIVE PRODUCTION, not used anywhere
    REAL TECH(AKFUEL)                   ! TECH FACTOR

    INTEGER FACYR(AKFUEL,AKfld)
    INTEGER DEVYR(AKFUEL,AKfld)
    LOGICAL ONCE


    DATA ONCE/.FALSE./
    !
    IF (.NOT.ONCE) THEN
        DO K=1,AKFUEL
            DO F=1,AKfld
                DEVYR(K,F) = 0
                WELLSSUC(K,F) = 0
                FACYR(K,F) = 0
                CUMPRD(K,F) = 0                ! cumulative production, isn't used anywhere
            ENDDO
        ENDDO
        ONCE = .TRUE.
    ENDIF
    SPENDEX = 0
    SPENDDV = 0
    !
    !  APPLY POLICY TECHNOLOGY ADJUSTMENT FACTOR
    IF (T.GE.TECHYR) THEN
        IF(OGRUNOP(2).eq.30) TECH(1) = RTECHAK(1) * (1.+OGRUNOP(11)/100.)	!OGRUNOP(11) is runtime option for tech rate improvements
        IF(OGRUNOP(2).eq.23) TECH(1) = RTECHAK(1) * (1.-OGRUNOP(11)/100.)
    ELSE
        TECH(1) = RTECHAK(1)
    ENDIF
    !
    DO F=1,AKFLD
        r = fldreg(f)

        IF (proyr(f).gt.(baseyr+akhyr+1)) THEN
            IF (recres(f).le.0) THEN
                prof_ak(1,f) = -0.000001
            ELSE
                ! COMPUTE EXPECTED PROFITABILITY
                CALL OGDCF_AK(2,R,1,DCF_AK,COST_AK,recres(F))  
                prof_ak(1,f) = dcf_ak(2,r,1)/cost_ak           ! next to prevent speculative fields
                ! FORCE DEVELOPMENT OF CURRENTLY KNOWN FIELDS ON OR AFTER START YEAR
                IF (f.le.50) prof_ak(1,f) = 0.1  ! ALLOW DCF CALCULATION FOR ANWR fields
!               IF (proyr(F)-baseyr+1.le.akhyr+10) prof_ak(1,f) = 0.1  ! ALLOW DCF CALC FOR ANWR fields and discoveries not undevelopment
                IF (t.lt.(proyr(F)-baseyr+1).or.(f.gt.51.and.devyr(1,f-1).le.2)) THEN
                    PROF_AK(1,F) = -0.000001
                ENDIF
            ENDIF
            IF (PROF_AK(1,F).GT.0.005) THEN                 ! Increased profitability requirement from zero to 0.005 (12-18-08)
                DEVYR(1,F) = DEVYR(1,F) + 1
                ! RECORD NUMBER OF SUCCESSFUL WELLS DRILLED IN YEAR T (EXP. & DEV.)
                NUMEXP = EXP_AK(R,DEVYR(1,F))
                NUMDEV = DEV_AK(R,DEVYR(1,F))
                ! COMPUTE TOTAL NUMBER OF SUCCESSFUL WELLS
                WELLSSUC(1,F) = NUMEXP + NUMDEV + WELLSSUC(1,F)
                ! COMPUTE FACILITY COST IN YEAR T
                IF (NUMDEV.GT.0.OR.FACYR(1,F).GT.0) THEN
                    FACYR(1,F) = FACYR(1,F) + 1                         ! FACYR initialized to zero
                    ! then incremented,
                    FACOST = OGFAC_AK(FACYR(1,F),recres(F),FACILAK)
                ELSE
                    FACOST = 0
                ENDIF
                ! ASSIGN FIELD SIZE CLASS
                CALL OGFSC_AK(recres(F),N)
                ! RECORD PRODUCTION IN YEAR T
                akcoprd(t,F) = recres(F) * PRODFAC(N,DEVYR(1,F)) * (1+tech(1))
                IF ((akcoprd(t,F).GT.maxpro(F)*0.365).and. &  ! if prod exceeds max, set to max
                (maxpro(f).gt.0.0)) &
                    akcoprd(t,F) = maxpro(F)*0.365
            ENDIF
        ENDIF
        CUMPRD(1,F) = CUMPRD(1,F) + akcoprd(t,F)
    ENDDO
    
    END SUBROUTINE OGDEV_AK
    
    !******************************************************************!
    !
    !******************************************************************!
    SUBROUTINE OGPRO_AK
    !
    IMPLICIT NONE
    INCLUDE 'parametr'        ! nems parameters
    INCLUDE 'ncntrl'          ! nems control variables
    INCLUDE 'ogsmparm'        ! ogsm parameters
    INCLUDE 'ogsmbfw'         ! ogsm system variables
    INCLUDE 'ogsmak'          ! aogss variables
    INCLUDE 'ogsml48'         ! variables
    INCLUDE 'ogsmout'
    !
    REAL CURMAX(AKFLD)  !
    REAL ADJADD(AKFLD)  !
    REAL POWER          !
    REAL RESADNGSA      ! BASE RESERVE ADDITIONS - SOUTH ALASKA
    REAL TECH(AKFUEL)   ! TECH FACTOR
    LOGICAL TEST        !
    !
    DATA TEST/.FALSE./
    DATA RESADNGSA/500/ ! Arbitrary initialization of gas reserve additions in South AK.
    !
    !  APPLY POLICY TECHNOLOGY ADJUSTMENT FACTOR
    !     IF (T.GE.TECHYR) THEN
    !       TECH(1) = RTECHAK(1) * TECHADJ_AK(1,3)
    !     ELSE
    !       TECH(1) = RTECHAK(1)
    !     ENDIF
    !  ADJUST DELCINE IN RESERVES FOR ACCELERATED DEPLETION
    !    IF (OGRUNOP(2).GE.3.AND.OGRUNOP(2).LE.10) THEN  ! OGRUNOP specifies tech cases w/ 1=base
    !                                                     ! 0=low, 2=high, >2=depletion
    !
    !       DO L=1,AKFLD
    !         DECLPRO(L) = DECLPRO(L)* (4/3)
    !       ENDDO
    !    ENDIF
    !
    IF (OGRUNOP(2).eq.11.AND.CURIYR.GE.TECHYR) THEN  ! FE TECHNOLOGY - W/O DOE R&D
        DO L=1,AKFLD
            DECLPRO(L) = DECLPRO(L) - FETECH_CONV(19,1)/100.
        ENDDO
    ENDIF
    
    IF (OGRUNOP(2).eq.12.AND.CURIYR.GE.TECHYR) THEN  ! FE TECHNOLOGY - W/ DOE R&D
        DO L=1,AKFLD
            DECLPRO(L) = DECLPRO(L) - FETECH_CONV(19,1)*FEADJ_CONV(1)/100.
        ENDDO
    ENDIF

    !*** CALCULATE CRUDE OIL PRODUCTION FROM CURRENT PRODUCING FIELDS ***
    DO L=1,AKFLD
        IF (PROYR(L).LE.(BASEYR+AKHYR+1)) THEN
            !  CONVERT MAX DAILY PRODUCTION (MB/D) TO ANNUAL VOLUME (MMB)
            CURMAX(L)=MAXPRO(L)*0.365
            !  CALCULATE PRODUCTION (MMB)
            IF (RECRES(L).GT.0) THEN
                AKCOPRD(t,L) = RECRES(L)*DECLPRO(l)        !  DETERMINE CRUDE OIL PRODUCTION
                !  MAKE SURE PRODUCTION IS LESS THAN OR EQUAL TO MAX
                IF ((CURMAX(L).LT.AKCOPRD(t,L)).and. &
                    (CURMAX(L).GT.0.0)) THEN
                AKCOPRD(t,L) = CURMAX(L)
                ENDIF

                !  SET MINIMUM FOR SOUTH ALASKA (field #23)
                !           IF (AKCOPRD(t,20).LT.5*0.365) THEN AKCOPRD(t,23)=5*0.365
            ELSE
                AKCOPRD(t,L) = 0
            ENDIF

            !  MAKE SURE PRODUCTION IS GREATER THAN OR EQUAL TO ZERO (0), NEEDED WHEN DECLPRO(L) < 0
            IF (AKCOPRD(t,L).LT.0) THEN
                AKCOPRD(t,L) = 0
                RECRES(L) = 0      ! if production is zero than preclude reserves from being negative
            ENDIF
            !  RECOVERABLE RESOURCES AS OF 12/31/2008, SO ONLY PRODUCTION FOR 2009 AND LATER
            !     IS SUBTRACTED FROM THE RECOVERABLE RESERVE NUMBERS (8-19-09)
            !     T=19 ==> YEAR=2008.
            IF (T.GE.(AKHYR-1)) THEN
                RECRES(L) = (RECRES(L)-AKCOPRD(T,L))  ! *(1+adjadd(l)) code bit used in depletion study.
            ENDIF
        ENDIF
    ENDDO
    
    END SUBROUTINE OGPRO_AK
    
    !******************************************************************!
    !
    !******************************************************************!
    SUBROUTINE OGSORT_AK(FLDSIZE)
    !
    IMPLICIT NONE
    INCLUDE 'parametr'        ! nems parameters
    INCLUDE 'ncntrl'          ! nems control variables
    INCLUDE 'ogsmparm'        ! ogsm parameters
    INCLUDE 'ogsmbfw'         ! ogsm system variables
    INCLUDE 'ogsmak'          ! aogss variables
    !
    REAL , INTENT(INOUT) :: FLDSIZE(AKSIZE,AKRGN)   ! USED IN SORT ROUTINE
    REAL TEMP                ! USED IN SORT ROUTINE
    !
    DO R = 1,AKRGN
        I = 1
        DO WHILE (I.LT.AKSIZE)
            L = I + 1
            DO WHILE (FLDSIZE(I,R).GE.FLDSIZE(L,R).AND.L.LT.AKSIZE)
                L = L+1
            ENDDO
            IF (FLDSIZE(I,R).GE.FLDSIZE(L,R).AND.L.EQ.AKSIZE) THEN
                I = I+1
            ELSE
                TEMP = FLDSIZE(L,R)
                DO M = L,I+1,-1
                    FLDSIZE(M,R) = FLDSIZE(M-1,R)
                ENDDO
                FLDSIZE(I,R) = TEMP
            ENDIF
        ENDDO
    ENDDO
    
    END SUBROUTINE OGSORT_AK
    
    !******************************************************************!
    !
    !******************************************************************!
    SUBROUTINE OGINIT_AK
    !
    IMPLICIT NONE
    INCLUDE 'parametr'        ! nems parameters
    INCLUDE 'ncntrl'          ! nems control variables
    INCLUDE 'ogsmparm'        ! ogsm parameters
    INCLUDE 'ogsmbfw'         ! ogsm system variables
    INCLUDE 'ogsmak'          ! aogss variables
    INCLUDE 'ogsmout'
    INCLUDE 'ngtdmrep'        ! ngtdm global variables

    CHARACTER*750 cline

    INTEGER*1  ANWRFLAG    ! ANWRFLAG (1 is on, 0 is off)
    INTEGER*4  IYR         ! YEAR INDEX (1,2,3...)
    INTEGER*4  IYEAR       ! YEAR ID (1990, 1991...)
    REAL*4     NGFAC       ! FACTOR TO SPLIT AK NG RES OVER REGION
    ! ------------

    !  INVOKE FILE_MGR TO OPEN INPUT FILE
    fname='WLALASKA'
    NEW=.FALSE.
    IFILE5 = FILE_MGR('O',FNAME,NEW)

    !  --------------  READ BASE YEAR DATA FILE  ----------------
    !  READ START YEAR FOR AK MODULE
    !  AKBYR MUST BE LESS THAN AKHYR
    CALL ognxtdat(ifile5,cline)
    READ (cline,*)    AKBYR

    !  CONVERT AKBYR FORMAT FROM 1990,1991, etc. to 1,2,etc.
    IF (AKBYR .GE. BASEYR) THEN
        AKBYR = AKBYR - BASEYR + 1
    ENDIF

    !  READ STATE TAX RATES
    CALL ognxtdat(ifile5,cline)
    READ (cline,*)    STTXAK

    !  READ SEVERANCE TAX RATES
    DO L = 1,2
        CALL ognxtdat(ifile5,cline)
        READ (cline,*)    SEVTXAK(L)
    ENDDO

    !  READ DRILL COSTS THAT ARE TANGIBLE & MUST BE DEPRECIATED
    CALL ognxtdat(ifile5,cline)
    READ (cline,*)    (KAPFRCAK(I),I=1,AKWELL)

    !  READ INTANGIBLE DRILL COSTS THAT MUST BE DEPRECIATED
    CALL ognxtdat(ifile5,cline)
    READ (cline,*)    XDCKAPAK

    !  READ OIL PROJECT LIFE (YEARS)
    CALL ognxtdat(ifile5,cline)
    READ (cline,*)    (PRJAK(K),K=1,AKFUEL)

    !  READ RECOVERY PERIOD OF INTANGIBLE & TANGIBLE DRILL COST
    CALL ognxtdat(ifile5,cline)
    READ (cline,*)    (RCPRDAK(M),M=1,2)

    !  READ TECH FACTORS (cost adjustment)
    CALL ognxtdat(ifile5,cline)
    READ (cline,*)    (TECHAK(L),L=1,COSTCAT)
    IF (OGRUNOP(2).EQ.23 .or. OGRUNOP(2).eq.0) THEN              ! Low resource case
        TECHAK(1) = TECHAK(1) *(1. - OGRUNOP(11)/100.)
        TECHAK(2) = TECHAK(2) *(1. - OGRUNOP(11)/100.)
        TECHAK(3) = TECHAK(3) *(1. - OGRUNOP(11)/100.)
    ENDIF
    IF (OGRUNOP(2).EQ.30 .or. OGRUNOP(2).eq.2) THEN              ! High resource case
        TECHAK(1) = TECHAK(1) *(1. + OGRUNOP(11)/100.)
        TECHAK(2) = TECHAK(2) *(1. + OGRUNOP(11)/100.)
        TECHAK(3) = TECHAK(3) *(1. + OGRUNOP(11)/100.)
    ENDIF

    !  READ OIL FIELD SIZE DISTRIBUTIONS
    DO L = 1,aksize
        CALL ognxtdat(ifile5,cline)
        READ (cline,*)    (FSZCOAK(L,R),R=1,AKRGN)
    ENDDO

    !  READ GAS FIELD SIZE DISTRIBUTIONS
    DO L = 1,aksize
        CALL ognxtdat(ifile5,cline)
        READ (cline,*)    (FSZNGAK(L,R),R=1,AKRGN)
    ENDDO

    !  READ SUCCESS RATES
    DO I = 1,AKWELL
        DO R = 1,AKRGN
            CALL ognxtdat(ifile5,cline)
            READ (cline,*)    (SRAK(I,R,K),K=1,AKFUEL)
        ENDDO
    ENDDO

    !  READ TRANSPORTATION COST >>>>>> TEMPORARY ASSIGNMENT
    DO R = 1,AKRGN
        CALL ognxtdat(ifile5,cline)
        READ (cline,*)    (TRANSAK(R,K),K=1,AKFUEL)
    ENDDO

    !  READ DRILLING COST
    !  new field wildcat drilling costs
    DO R = 1,AKRGN
        CALL ognxtdat(ifile5,cline)
        READ (cline,*)    (DRLNFWAK(R,K),K=1,AKFUEL)
    ENDDO

    !  exploratory/development drilling costs, wellak.f variable:DRILLAK
    DO I = 1,AKWELL
        DO R = 1,AKRGN
            CALL ognxtdat(ifile5,cline)
            READ (cline,*)    (DRILLAK(I,R,K),K=1,AKFUEL)
            DO K = 1,AKFUEL
                DRYAK(I,R,K) = DRILLAK(I,R,K)    ! dry well costs = successful well costs
            ENDDO
        ENDDO
    ENDDO

    !  READ LEASE EQUIPMENT COST
    DO I = 1,AKWELL
        DO R = 1,AKRGN
            DO K = 1,AKFUEL
                !             CALL ognxtdat(ifile5,cline)                   ! commented out because is zero
                !             READ (cline,*)    (EQUIPAK(I,R,K),K=1,AKFUEL) ! in database wlalaska.txt
                EQUIPAK(I,R,K)=0
            ENDDO
        ENDDO
    ENDDO

    !  READ FACILITY COST (OIL FIELD), by field size (ie., 64, 94, 128, etc.)
    DO M = 1,9
        CALL ognxtdat(ifile5,cline)
        READ (cline,*)    FACILAK(M)
    ENDDO

    !  READ OPERATING COST
    !  used by function OGOPER_AK, which itself is no longer used (8/12/02)
    DO L = 1,4
        CALL ognxtdat(ifile5,cline)
        READ (cline,*)    (OPERAK(L,M),M=1,2)
    ENDDO

    !  READ DRILLING SCHEDULE FOR OTHER EXPLORATORY WELLS
    DO R = 1,AKRGN
        CALL ognxtdat(ifile5,cline)
        READ (cline,*)    (EXP_AK(R,L),L=1,30)
    ENDDO

    !  READ DRILLING SCHEDULE FOR DEVELOPMENTAL WELLS
    DO R = 1,AKRGN
        CALL ognxtdat(ifile5,cline)
        READ (cline,*)    (DEV_AK(R,L),L=1,30)
    ENDDO

    !  READ CRUDE OIL DATA FOR KNOWN FIELDS (created 4/4/03; revised 8/12/09 with % for onshore, State offshore, and Fed Offshore)
    !  FLDREG(L) = Maps field to regions 1=Offshore North, 2=Onshore North, 3=South Alaska
    !  PROYR(L)  = Year field went into production aka "start year"
    !  RECRES(L) = Recoverable Resources (MMB)
    !  DECLPRO(L)= Production decline rates (Fraction of resources remaining)
    !  MAXPRO(L) = Maximum field production level (MB/D)
    !  AKREGPER(L,3) = the Onshore percentage of production, reserves, and resources by resource region 1=onshore, 2=state offshore, 3=fed offshore.
    !      WRITE(6,1000)  ! temporary (9-1-09) File "6" writes to nohup.out, must remove once debugged so thast the defaulted file doesn't write in nohup.out
    DO L = 1,AKFLD
        CALL ognxtdat(ifile5,cline)
        READ (cline,*)    FLDREG(L), PROYR(L), RECRES(L), DECLPRO(L), MAXPRO(L), (AKREGPER(L,M),M=1,AKRESRGN)
        !         WRITE(6,1001) L, FLDREG(L), PROYR(L), RECRES(L), DECLPRO(L), MAXPRO(L), (AKREGPER(L,M),M=1,AKRESRGN)   ! temporary (9-1-09)
    ENDDO
    !
    !      1001 FORMAT('Field    ',I4,I9,I9,6F9.2)
    !      1000 FORMAT('Field    ','No. ','   FLDREG','   PROYR','   RECRES','  DECLPRO','   MAXPRO','  REGPER1','  REGPER2','  REGPER3')

    !  READ HISTORIC CRUDE OIL PRODUCTION (MB/D)
    DO L = 1,AKFLD
        CALL ognxtdat(ifile5,cline)
        READ (cline,*)    (HISTPRDCO(L,M),M=1,AKHYR)
    ENDDO

    !  READ HISTORIC OIL RESERVES (MMB) (8-25-09) AKRESTYPE=1, AKFUEL=1
    DO T = 1,(AKHYR-1)   ! EIA RESERVE NUMBERS NOT AVAILABLE FOR LAST HISTORIC YEAR
        CALL ognxtdat(ifile5,cline)
        READ (cline,*)    (AKHISTRES(T,1,M,1),M=1,AKRESRGN)
    ENDDO

    !  READ HISTORIC NATURAL GAS RESERVES (BCF) (8-25-09) AKRESTYPE=1, AKFUEL=2
    DO T = 1,(AKHYR-1)   ! EIA RESERVE NUMBERS NOT AVAILABLE FOR LAST HISTORIC YEAR
        CALL ognxtdat(ifile5,cline)
        READ (cline,*)    (AKHISTRES(T,1,M,2),M=1,AKRESRGN)
    ENDDO

    !  READ HISTORIC INFERRED OIL RESOURCES (MMB) (8-25-09) AKRESTYPE=2, AKFUEL=1
    DO T = 1,(AKHYR-1)   ! EIA RESERVE NUMBERS NOT AVAILABLE FOR LAST HISTORIC YEAR
        CALL ognxtdat(ifile5,cline)
        READ (cline,*)    (AKHISTRES(T,2,M,1),M=1,AKRESRGN)
    ENDDO

    !  READ HISTORIC INFERRED NATURAL GAS RESOURCES (BCF) (8-25-09) AKRESTYPE=2, AKFUEL=2
    DO T = 1,(AKHYR-1)   ! EIA RESERVE NUMBERS NOT AVAILABLE FOR LAST HISTORIC YEAR
        CALL ognxtdat(ifile5,cline)
        READ (cline,*)    (AKHISTRES(T,2,M,2),M=1,AKRESRGN)
    ENDDO

    !  READ HISTORIC UNDISCOVERED OIL RESOURCES (MMB) (8-25-09) AKRESTYPE=3, AKFUEL=1
    DO T = 1,(AKHYR-1)   ! EIA RESERVE NUMBERS NOT AVAILABLE FOR LAST HISTORIC YEAR
        CALL ognxtdat(ifile5,cline)
        READ (cline,*)    (AKHISTRES(T,3,M,1),M=1,AKRESRGN)
    ENDDO

    !  READ HISTORIC UNDISCOVERED NATURAL GAS RESOURCES (BCF) (8-25-09) AKRESTYPE=3, AKFUEL=2
    DO T = 1,(AKHYR-1)   ! EIA RESERVE NUMBERS NOT AVAILABLE FOR LAST HISTORIC YEAR
        CALL ognxtdat(ifile5,cline)
        READ (cline,*)    (AKHISTRES(T,3,M,2),M=1,AKRESRGN)
    ENDDO

    DO T = 1,(AKHYR-1)
        DO J=1,AKRESTYPE
            DO K=1,AKRESRGN
                DO L=1,AKFUEL
                    WRITE(bugout,*) 'AKHISTRES1  ',T,J,K,L,AKHISTRES(T,J,K,L) ! temporary write (9-2-09)
                ENDDO
            ENDDO
        ENDDO
    ENDDO

    !  READ RESERVE TECH FACTORS
    CALL ognxtdat(ifile5,cline)
    READ (cline,*)    (RTECHAK(K),K=1,AKFUEL)
    IF (OGRUNOP(2).EQ.11) THEN
        RTECHAK(1) = RTECHAK(1) - FETECH_CONV(18,1)/100.  ! FE TECHNOLOGY - W/O DOE R&D
        RTECHAK(2) = RTECHAK(2) - FETECH_CONV(18,1)/100.  ! FE TECHNOLOGY - W/O DOE R&D
    ENDIF
    IF (OGRUNOP(2).EQ.12) THEN
        RTECHAK(1) = RTECHAK(1) - FETECH_CONV(18,1)*FEADJ_CONV(1)/100.  ! FE TECHNOLOGY - W/ DOE R&D
        RTECHAK(2) = RTECHAK(2) - FETECH_CONV(18,1)*FEADJ_CONV(1)/100.  ! FE TECHNOLOGY - W/ DOE R&D
    ENDIF

    !  READ RECOVERABLE RESOURCE ADJUSTMENT FACTOR
    !  currently zero in wlalaska.txt
    CALL ognxtdat(ifile5,cline)
    READ (cline,*)    RECADJAK

    !  READ STEO CO PRODUCTION
    DO L = 1,AKFLD
        CALL ognxtdat(ifile5,cline)
        READ (cline,*)    (STEO(L,M),M=1,6)
    ENDDO

    !  READ IN ANWR OPTION, 1=on, 0=off
    CALL ognxtdat(ifile5,cline)
    READ (cline,*)    ANWRFLAG

    !  READ IN AKOILPL STATUS 1=TAPS ON, 0=TAPS OFF  (PROGRAM SETS THIS TO O WHEN nORTH sLOPE PRODUCTION IS EQUAL TO OR LESS THAN MINIMUM THROUGHPUT)
    CALL ognxtdat(ifile5,cline)
    READ (cline,*)    TAPSFLAG

    !  READ IN AK OIL PIPELINE MINIMUM THROUGHPUT VOLUME, READ IN VOLUME EXPRESSED IN 1,000 BBLS/DAY, BELOW WHICH THE PIPELINE BECOMES INOPERATIVE
    CALL ognxtdat(ifile5,cline)
    READ (cline,*)    TAPSMINIMUM
    TAPSMINIMUM = TAPSMINIMUM * .365          ! Translates 1,000 bbl per day into million bbl per year.

    !  READ IN AK NORTH SLOPE OIL PRODUCTION REVENUE REQUIREMENTS TO KEEP TAPS PIPELINE OPERATING (EXPRESSED IN 2010 DOLLARS)
    CALL ognxtdat(ifile5,cline)
    READ (cline,*)    TAPSMINOILREV

    !  READ HISTORIC AK NGL PRODUCTION (MB/D)
    CALL ognxtdat(ifile5,cline)
    READ (cline,*)    (OGNGLAK(M),M=1,AKHYR)

    !  READ North Alaska split between onshore and offshore exploratory wells,
    !       assumes that more wells are drilled offshore in future years
    !       string represents Offshore Federal North Alaska drilling fraction.
    !       NS_OFF_EXP(t after AKHYR --> M = T - AKHYR)
    CALL ognxtdat(ifile5,cline)
    READ (cline,*)    (NS_OFF_NFW(M),M=1,41)

    !  INITIALIZE OGSM VARIABLES TO ZERO
    DO IYR=1,IJUMPYR
        TOTPRCOAK(IYR) = 0.0
        TOTPRNGAK(IYR) = 0.0
        DO R=1,AKRGN
            OGPRCOAK(R,IYR) = 0.0
            OGPRNGAK(R,IYR) = 0.0
            RESADCOAK(R,IYR) = 0.0
            RESADNGAK(R,IYR) = 0.0
            BOYRESCOAK(R,IYR) = 0.0
            BOYRESNGAK(R,IYR) = 0.0
        ENDDO
    ENDDO

    !  SET ANWR OPTION
    !  Modified in 2009 for resource accounting
    !  if not equal to 1, then set anwr field resources equal to zero and remove resources from undiscovered resource base.

    IF (ANWRFLAG.NE.1) THEN
        DO L=1, AKFLD
            IF ((L.GE.51).AND.(L.LE.62)) THEN    ! expanded ANWR field count in Sept 2012
                RECRES(L)=0.
            ENDIF
        ENDDO
        DO T = 1,(AKHYR-1)
            AKHISTRES(T,3,1,1) = AKHISTRES(T,3,1,1) - 10678.
            AKHISTRES(T,3,1,2) = AKHISTRES(T,3,1,2) -  8605.
        ENDDO
    ENDIF

    ! Read in historical year-end data for resource base (8-28-09); Added BOYRES and RESAD variables for oil and gas (June 2010)

    DO IYR=1,(AKHYR-1)                     ! Minus 1 because EIA reserves data one year less than other historical AK data.
        DO K=1,AKRESRGN
            DO J=1,AKRESTYPE
                DO L=1,AKFUEL
                    AKRESOURCES(IYR,J,K,L) = AKHISTRES(IYR,J,K,L)
                    !	WRITE(bugout,*)'AKRESOURCES1  ',IYR,J,K,L,AKRESOURCES(IYR,J,K,L),'   AKHISTRES2  ',AKHISTRES(IYR,J,K,L)
                ENDDO
            ENDDO

            BOYRESCOAK(1,IYR) = BOYRESCOAK(1,IYR) + AKHISTRES(IYR,1,K,1)
            BOYRESNGAK(1,IYR) = BOYRESNGAK(1,IYR) + AKHISTRES(IYR,1,K,2)

        ENDDO
    ENDDO


    !  SET 1990 RESERVES AND SORT UNDISCOVERED FIELDS
    CALL OGONCE_AK

    !  TRANSFER HISTORICAL INFO TO OGSM VARIABLES
    IF ((AKBYR .GT. 1) .AND. (AKBYR .LE. AKHYR)) THEN
        DO IYR=1,AKBYR-1
            IYEAR = IYR + BASEYR - 1
            DO L=1,AKFLD
                R = FLDREG(L)
                OGPRCOAK(R,IYR) = OGPRCOAK(R,IYR) + HISTPRDCO(L,IYR)*0.365
            ENDDO
            DO R=1,AKRGN
                OGPRNGAK(R,IYR) = OGPRDNG(R+L48RGN+OFFNEMSRGN,IYR)		! AK gas prod from NGTDM
                TOTPRCOAK(IYR) = TOTPRCOAK(IYR) + OGPRCOAK(R,IYR)
                TOTPRNGAK(IYR) = TOTPRNGAK(IYR) + OGPRNGAK(R,IYR)
            ENDDO
        ENDDO
    ENDIF

    !  CLOSE INPUT FILE IFILE1
    IFILE5 = FILE_MGR('C',FNAME,NEW)
    
    END SUBROUTINE OGINIT_AK
    
    !******************************************************************!
    !
    !******************************************************************!
    SUBROUTINE OGONCE_AK
    IMPLICIT NONE

    INCLUDE 'parametr'        ! nems parameters
    INCLUDE 'ncntrl'          ! nems control variables
    INCLUDE 'ogsmparm'        ! ogsm parameters
    INCLUDE 'ogsmbfw'         ! ogsm system variables
    INCLUDE 'ogsmak'          ! aogss variables
    INCLUDE 'ogsmout'
    !
    LOGICAL ONCE         ! LOGICAL CONTROLLING INITIALIZATION
    !
    DATA ONCE/.FALSE./
    !
    IF (.NOT.ONCE) THEN
        ! INITIALIZE KNOWN BUT NOT PRODUCING OIL FIELDS
        DO R=1,AKRGN
            KNOFLD(R) = 0
            DO K=1,AKFUEL
                NDP(K,R) = 0
                NPF(K,R) = 0
                DO F=1,AKSIZE
                    DEVFLD(K,F,R) = 0
                    DEVBGN(K,F,R) = 0
                ENDDO
            ENDDO
        ENDDO
        DO F=1,AKFLD
            ! assign region
            R = fldreg(f)
            ! ADJUST RECOVERABLE RESOURCES FOR HIGH/LOW RECOVERY CASES
            RECRES(F) = RECRES(F) * (1 + RECADJAK)   ! RECADJAK in wlalaska.txt, currently zero

            IF (PROYR(F).GT.baseyr+akhyr-1.AND.PROYR(F).LE.IJUMPCALYR) THEN  ! for hard-wired fields not yet in production
                NDP(1,R) = NDP(1,R) + 1
                DEVFLD(1,NDP(1,R),R) = RECRES(F)
                DEVBGN(1,NDP(1,R),R) = PROYR(F) - BASEYR + 1
                DEVMAX(1,NDP(1,R),R) = MAXPRO(F)
                RESADCOAK(R,DEVBGN(1,NDP(1,R),R)-1) = DEVFLD(1,NDP(1,R),R) &
                    + RESADCOAK(R,DEVBGN(1,NDP(1,R),R)-1)
            ENDIF
            KNOFLD(R) = NDP(1,R)		!!!! doesn't seem to be used anywhere
        ENDDO

        ! ADJUST RECOVERABLE RESOURCES FOR HIGH AND LOW RESOURCE CASES
        IF (OGRUNOP(2).eq.30) THEN  			! High resource case
            DO L=1,AKSIZE
                DO R=1,AKRGN
                    FSZCOAK(L,R) = FSZCOAK(L,R) * (1. + OGRUNOP(13)/100.)       ! Field sizes are bigger than specified amount (OGRUNOP(13))
                    FSZNGAK(L,R) = FSZNGAK(L,R) * (1. + OGRUNOP(13)/100.)
                ENDDO
            ENDDO
            DO F=1,AKFLD
                IF(PROYR(F).GT.AKHYR+BASEYR) RECRES(F) = RECRES(F) * (1. + OGRUNOP(13)/100.)       ! Field sizes are bigger than specified amount (OGRUNOP(13))
            ENDDO

        ENDIF

        IF (OGRUNOP(2).eq.23) THEN  			! Low resource case
            DO L=1,AKSIZE
                DO R=1,AKRGN
                    FSZCOAK(L,R) = FSZCOAK(L,R) * (1. - OGRUNOP(13)/100.)       ! Field sizes are smaller than specified amount (OGRUNOP(13))
                    FSZNGAK(L,R) = FSZNGAK(L,R) * (1. - OGRUNOP(13)/100.)
                ENDDO
            ENDDO
            DO F=1,AKFLD
                IF(PROYR(F).GT.AKHYR+BASEYR) RECRES(F) = RECRES(F) * (1. - OGRUNOP(13)/100.)       ! Field sizes are bigger than specified amount (OGRUNOP(13))
            ENDDO
        ENDIF

        !	Done years ago for an accelerated depletion case
        !        IF (OGRUNOP(2).GE.3.AND.OGRUNOP(2).LE.10) THEN  ! OGRUNOP specifies tech cases w/ 1=base
        !                                                        ! 0=low, 2=high, >2=depletion
        !          DO L=1,AKSIZE
        !            DO R=1,AKRGN
        !              FSZCOAK(L,R) = FSZCOAK(L,R) * (2/3)       ! Field sizes are 2/3 of what is expected
        !              FSZNGAK(L,R) = FSZNGAK(L,R) * (2/3)
        !            ENDDO
        !          ENDDO
        !        ENDIF

        ! GENERATE PRODUCTION PERCENTAGES
        CALL OGPFAC_AK
        ! INITIALIZATION IS COMPLETE
        ONCE = .TRUE.
    ENDIF
    
    END SUBROUTINE OGONCE_AK
    
    !******************************************************************!
    !
    !******************************************************************!
    REAL FUNCTION OGSEVR_AK(M,PROD,CUMWELL)
    !  COMPUTE SEVERANCE TAX RATE
    IMPLICIT NONE
    INCLUDE 'parametr'        ! nems parameters
    INCLUDE 'ogsmparm'        ! ogsm parameters
    INCLUDE 'ogsmak'          ! aogss variables

    INTEGER, INTENT(IN) :: M
    REAL, INTENT(IN) :: PROD
    REAL, INTENT(IN) ::  CUMWELL
    
    REAL FLOW
    REAL TEMP
    REAL ADJFAC

    IF (M.EQ.1) TEMP = 0
    FLOW = PROD/365

    IF (PROD.LE.0) THEN
        OGSEVR_AK = 0
    ELSE
        TEMP = TEMP + 1
        ADJFAC = 0.0
        IF (FLOW.NE.0) THEN
            ADJFAC = 1 - (300/(FLOW/CUMWELL))
            IF (ADJFAC.LE.0) ADJFAC = 0
            ADJFAC = ADJFAC**((150000/FLOW)**1.533)
        ENDIF
        IF (TEMP.LE.5) THEN
            OGSEVR_AK = SEVTXAK(1)*ADJFAC
        ELSE
            OGSEVR_AK = SEVTXAK(2)*ADJFAC
        ENDIF
    ENDIF
    
    END FUNCTION OGSEVR_AK
    
    !******************************************************************!
    !
    !******************************************************************!
    REAL FUNCTION OGOPER_AK(PROD)
    !     COMPUTE OPERATING COSTS   (not used directly in wellak.f module, but used in welldcf.f, 8/13/02)
    IMPLICIT NONE           ! (ARI cost study based on field size not production level)

    INCLUDE 'parametr'        ! nems parameters
    INCLUDE 'ogsmparm'        ! ogsm parameters
    INCLUDE 'ogsmak'          ! aogss variables

    REAL, INTENT(IN) :: PROD
    REAL PROD_MBD

    PROD_MBD = PROD/365000

    IF (PROD_MBD.LE.0) THEN
        OGOPER_AK = 0
    ELSEIF (PROD_MBD.LE.50) THEN
        OGOPER_AK = OPERAK(1,1) + PROD/1000000*OPERAK(1,2)
    ELSEIF (PROD_MBD.LE.160) THEN
        OGOPER_AK = OPERAK(2,1) + PROD/1000000*OPERAK(2,2)
    ELSEIF (PROD_MBD.LE.300) THEN
        OGOPER_AK = OPERAK(3,1) + PROD/1000000*OPERAK(3,2)
    ELSE
        OGOPER_AK = OPERAK(4,1) + PROD/1000000*OPERAK(4,2)
    ENDIF

    END FUNCTION OGOPER_AK
    
    !******************************************************************!
    !
    !******************************************************************!
    SUBROUTINE OGSUP_AK(P,R,K,RESOURCE,PRJLIFE, &
        EXP,DEV,NFW,AKPRD,AKCOP,FACOST)
    IMPLICIT NONE
    INCLUDE 'parametr'        ! nems parameters
    INCLUDE 'ncntrl'          ! nems control variables
    INCLUDE 'ogsmparm'        ! ogsm parameters
    INCLUDE 'ogsmbfw'         ! ogsm system variables
    INCLUDE 'ogsmak'          ! aogss variables
    INCLUDE 'ogsmout'
    
    INTEGER, INTENT(IN) :: P                !
    INTEGER, INTENT(IN) :: PRJLIFE          ! PROJECT LIFE
    REAL, INTENT(IN) :: RESOURCE            ! RESOURCE BASE
    REAL, INTENT(OUT) :: EXP(2,30)           ! ASSIGN NUMBER OF EXPLORATORY WELLS
    REAL, INTENT(OUT) :: DEV(2,30)           ! ASSIGN NUMBER OF DEVELOPMENTAL WELLS
    REAL, INTENT(OUT) :: NFW(2,30)           ! ASSIGN NUMBER OF NEW FIELD WILDCATS
    REAL, INTENT(INOUT) :: FACOST(30)          ! FACILITY COST
    REAL, INTENT(OUT) :: AKPRD(30)           ! PRIMARY PRODUCTION
    REAL, INTENT(OUT) :: AKCOP(30)           ! CO-PRODUCT PRODUCTION
    
    INTEGER FSCLASS            ! ASSIGN FIELD SIZE CLASS
    REAL TARIFF(30)          ! TARIFF
    INTEGER TEMP1,TEMP2      !
    INTEGER CUMWELL          !

    REAL OGFAC_AK
    REAL OGTAP_AK


    TEMP1 = 0
    TEMP2 = 0
    CUMWELL = 0

    !  ASSIGN FIELD SIZE CLASS
    CALL OGFSC_AK(RESOURCE,FSCLASS)

    DO M = 1,PRJLIFE
        !  ASSIGN NUMBER OF SUCCESSFUL WELLS DRILLED
        NFW(P,M) = 0              ! DO NOT INCLUDE NEW FIELD WILDCAT
        EXP(P,M) = EXP_AK(R,M)    ! DRILLING COST IN DCF CALCULATION
        DEV(P,M) = DEV_AK(R,M)
        !
        CUMWELL = CUMWELL + EXP(P,M) + DEV(P,M)
        !
        !  CALCULATE OIL PRODUCTION (BARRELS)
        !     OGRUNOP(13) = Resource Adjustment Factor For High/Low WOP Cases
        IF (DEV(P,M).GT.0.OR.TEMP2.GT.0) THEN
            TEMP2 = TEMP2 + 1
            AKPRD(M) = RESOURCE*PRODFAC(FSCLASS,TEMP2)*1000000.
            !         IF (OGRUNOP(2).eq.30) THEN
            !           AKPRD(M) = RESOURCE*PRODFAC(CLASS,TEMP2)*1000000*(1.0 + (OGRUNOP(13) / 100.0))
            !         ELSEIF (OGRUNOP(2).eq.23) THEN
            !           AKPRD(M) = RESOURCE*PRODFAC(CLASS,TEMP2)*1000000*(1.0 - (OGRUNOP(13) / 100.0))
            !         ENDIF

        ELSE
            AKPRD(M) = 0
        ENDIF
        AKCOP(M) = 0
        !
        !  CALCULATE FACILITY COST, where FACILAK is facility cost by field size
        IF (TEMP2.GT.0) FACOST(M) = OGFAC_AK(TEMP2,RESOURCE,FACILAK)
        !
    ENDDO
    
    END SUBROUTINE OGSUP_AK
    
    !******************************************************************!
    !
    !******************************************************************!
    REAL FUNCTION OGFAC_AK(M,RESOURCE,FACILAK)
    !  COMPUTE FACILITY COSTS - RELATED TO SIZE OF FIELD >>> i.e., FACILAK (cost/BBL, by field size class)
    !  INCLUDES LEASE EQUIPMENT AND OTHER CAPITAL COSTS
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: M
    REAL, INTENT(IN) :: RESOURCE
    REAL XFF
    INTEGER J
    REAL SIZE(9), FACILAK(9)
    REAL FACTOR(6)

    DATA SIZE/64,94,128,192,256,384,512,768,1024/         ! Field size in MMB
    DATA FACTOR/.25,.25,.25,.25,0,0/

    IF (M.LE.6) THEN                                      ! M=FACYR, which is incremented in OGDEV_AK
        IF (RESOURCE.LE.SIZE(1)) THEN                       ! use facility cost for smallest field size
            ! when field size is less than smallest
            OGFAC_AK = RESOURCE*FACILAK(1)*FACTOR(M)*1000000
        ELSEIF (RESOURCE.LE.SIZE(9)) THEN
            DO J=1,8
                IF (RESOURCE.GT.SIZE(J).AND.RESOURCE.LE.SIZE(J+1)) THEN  ! when field size falls betw. two classes
                    XFF = (RESOURCE-SIZE(J))/(SIZE(J+1)-SIZE(J))           ! then extrapolate facility cost
                    OGFAC_AK = RESOURCE*((1-XFF)*FACILAK(J) + &
                        XFF*FACILAK(J+1))*FACTOR(M)*1000000  ! BBLS x (Cost/BBl) x E6 = E6 $
                ENDIF
            ENDDO
        ELSE           ! when field size exceeds 1,024 MMB, then use cost/BBL for largest size
            OGFAC_AK = RESOURCE*FACILAK(9)*FACTOR(M)*1000000
        ENDIF
    ELSE             ! set facility cost to zero, when FACYR is greater than 6 years
        OGFAC_AK = 0
    ENDIF

    WRITE (6,*) OGFAC_AK

    END FUNCTION OGFAC_AK
    
    !******************************************************************!
    !
    !******************************************************************!
    SUBROUTINE OGDCF_AK(P,R,K,DCF_AK,COST_AK,FIELD)
    !
    IMPLICIT NONE
    INCLUDE 'parametr'                ! nems parameters
    INCLUDE 'ncntrl'                  ! nems control variables
    INCLUDE 'ogsmparm'                ! ogsm parameters
    INCLUDE 'ogsmbfw'                 ! ogsm system variables
    INCLUDE 'ogsmak'                  ! aogss variables
    INCLUDE 'ogsmout'
    !
    INTEGER, INTENT(INOUT) :: P                        ! deleted K2 from list on 8/8/02, couldn't find its use
    REAL, INTENT(OUT) :: DCF_AK(AKFUEL,AKRGN,AKFUEL) ! DCF
    REAL, INTENT(OUT) :: COST_AK                     ! DRILLING COST
    REAL, INTENT(IN) :: FIELD                       !
    
    REAL EXPWLS(2,30)                !
    REAL DEVWLS(2,30)                !
    REAL NFWWLS(2,30)                !
    REAL OG_DCF                      !
    REAL AKPRD(30)                   !
    REAL AKCOP(30)                   !
    REAL FACOST(30)                  !
    REAL COPRIC                      !
    REAL PRIMPRC(30)                 ! TEMP PRIMARY PRICE VARIABLE
    REAL COPRC(30)                   ! TEMP COPRODUCT PRICE VARIABLE
    REAL DUMSEV                      ! DUMMY SEV TAX RATE
    REAL DISCAK                      ! DISCOUNT RATE FOR ALASKA (12 vs 7)
    REAL TGDP

    DUMSEV = 0.0

    IF (K .EQ. 1) THEN               ! K=1 then crude oil is primary product & gas is co-product
        DO M=1,30
            OGPRCAK(1,1,m) = OGPRCAK(2,1,m)  ! SET OFFSHORE NORTH SLOPE PRICES EQUAL TO ONSHORE NORTH SLOPE
            OGPRCAK(1,2,m) = OGPRCAK(2,2,m)  !
            PRIMPRC(M) = OGPRCAK(R,1,M)  ! Alaskan crude oil price from PMM, passed from wellogs.f
            COPRC(M) = OGPRCAK(R,2,M)    ! Alaskan gas price from NGTDM, passed from wellogs.f
        ENDDO
    ELSE                             ! K=2 then gas is primary product & oil/NGL is co-product
        DO M=1,30
            PRIMPRC(M) = OGPRCAK(R,2,M)  ! Alaskan gas price from NGTDM, passed from wellogs.f
            COPRC(M) = OGPRCAK(R,1,M)    ! Alaskan crude oil price from PMM, passed from wellogs.f
        ENDDO
    ENDIF

    CALL OGSUP_AK(P,R,K,FIELD,PRJAK(K), &
        EXPWLS(1,1),DEVWLS(1,1),NFWWLS(1,1), &
        AKPRD(1),AKCOP(1),FACOST(1))
    DISCAK=DISC+.05
!   DISCAK=0.0		! Changed Sept 2016 to allow more projects to come online
    DCF_AK(P,R,K) = OG_DCF(2,P,R,K,PRJAK(K),RCPRDAK(1), &
        RCPRDAK(2),PRIMPRC(1),COPRC(1), &
        AKPRD(1),AKCOP(1),0.10,DISCAK,INFL,ROYRT,EXPWLS(1,1), &
        DEVWLS(1,1),NFWWLS(1,1),TRANSAK(R,K),0.0,SRAK(1,R,K), &
        SRAK(2,R,K),FACOST(1),DRILLAK(1,R,K), &
        DRILLAK(2,R,K),DRLNFWAK(R,K),DRYAK(1,R,K), &
        DRYAK(2,R,K),EQUIPAK(1,R,K),0.0, &
        STTXAK,FEDTXR,DUMSEV,0.0,0.0,KAPFRCAK(1),XDCKAPAK, &
        0.0,0.0,0.0)
    COST_AK = EXPWLS(P,1)*DRILLAK(1,R,K) + &          ! Determine total new field cost
    DEVWLS(P,1)*DRILLAK(2,R,K) + &
        EXPWLS(P,1)*(1/SRAK(1,R,K)-1)*DRYAK(1,R,K) + &
        DEVWLS(P,1)*(1/SRAK(2,R,K)-1)*DRYAK(2,R,K) + &
        FACOST(1)
        
    END SUBROUTINE OGDCF_AK
    
    !******************************************************************!
    !
    !******************************************************************!
    SUBROUTINE OGPFAC_AK
    !  COMPUTE PRODUCTION PERCENTAGES
    IMPLICIT NONE

    INCLUDE 'parametr'        ! nems parameters
    INCLUDE 'ncntrl'          ! nems variables
    INCLUDE 'ogsmparm'        ! ogsm parameters
    INCLUDE 'ogsmbfw'         ! ogsm variables
    INCLUDE 'ogsmout'        !

    REAL YHPEAK
    REAL YHBLD(2)
    REAL YHDECL
    REAL PEAK(5)
    REAL BLDUP(5,4)
    REAL DECLINE(5)
    REAL PRODFAC(6,MNUMYR)          ! PRODFAC dimemsioned for 6 field sizes, but the 300 MMB field size is not used
    !       in the projections
    !         OGRUNOP                    ! global O&G run time variable for adjusting relative resource levels w.r.t.
    !     high/low WOP cases, = 0 in reference case, = -number in high WOP
    !     = +number in Low WOP, in percent.

    INTEGER ATPK(5)
    INTEGER CNT

    COMMON/AOGSSC4/PRODFAC

    !     The following rates of production build up depends on the AK oil fields can be delineated into 5 field size categories:
    ! 50 to 300 MMB field size category
    ! 300 to 699 MMB field size category
    ! 700 to 1,350 MMB field size category
    ! 1,350 to 3,000 MMB field size category
    ! 3,000+ MMB field size category

    DATA PEAK/.10,.10,.10,.07,.06/         ! Peak production rates for 5 field size categories

    DATA BLDUP/.03,.03,.01,.01,.01,  &     !  Data loaded by column, 1st column year for the 5 field sizes
    .07,.07,.02,.03,.03,  &     !  2nd column year for the 5 field sizes
    .00,.00,.05,.05,.04,  &     !  3rd column year for the 5 field sizes
    .00,.00,.00,.00,.05/        !  4th column year for the 5 field sizes

    DATA DECLINE/.12,.15,.15,.12,.12/      ! Field rates of decline for 5 field size categories

    DATA ATPK/3,4,4,7,8/                   ! Number of years at peak oil production for 5 field size categories


    !  ALASKA OIL & GAS - ENERGY WEALTH OR VANISHING OPPORTUNITY, Page 3-42
    !      modified 8/8/02 to delete PRNEWADJ

    DO J = 1,5
        CNT = 0
        DO L = 1,IJUMPYR
            IF (L.LE.4) THEN
                IF (BLDUP(J,L).GT.0) THEN
                    PRODFAC(J+1,L) = BLDUP(J,L)
                    CNT = CNT + 1
                ELSE
                    IF (L.LE.(CNT+ATPK(J))) THEN
                        PRODFAC(J+1,L) = PEAK(J)
                    ELSE
                        PRODFAC(J+1,L) = PEAK(J)*(1-DECLINE(J))**(L-(CNT+ATPK(J)))
                    ENDIF
                ENDIF
            ELSEIF (L.LE.(CNT+ATPK(J))) THEN
                PRODFAC(J+1,L) = PEAK(J)
            ELSE
                PRODFAC(J+1,L) = PEAK(J)*(1-DECLINE(J))**(L-(CNT+ATPK(J)))
            ENDIF
        ENDDO
    ENDDO
    
    END SUBROUTINE OGPFAC_AK
    
    !******************************************************************!
    !
    !******************************************************************!
    SUBROUTINE OGFSC_AK(FIELD,FSCLASS)
    !  DETERMINE OIL FIELD SIZE CLASS
    IMPLICIT NONE

    REAL, INTENT(IN) :: FIELD
    INTEGER, INTENT(OUT) :: FSCLASS

    !  CLASS = 1 IS RESERVED FOR THE YOUNG AND HAUSER PROFILE

    IF (FIELD.LE.300) THEN
        FSCLASS = 2
    ELSEIF (FIELD.LE.699) THEN
        FSCLASS = 3
    ELSEIF (FIELD.LE.1350) THEN
        FSCLASS = 4
    ELSEIF (FIELD.LE.3000) THEN
        FSCLASS = 5
    ELSE
        FSCLASS = 6
    ENDIF

    END SUBROUTINE OGFSC_AK
    
    !******************************************************************!
    !
    !******************************************************************!
    SUBROUTINE OGREP_AK
    IMPLICIT NONE

    INCLUDE 'parametr'        ! nems parameters
    INCLUDE 'ncntrl'          ! nems control variables
    INCLUDE 'ogsmparm'        ! ogsm parameters
    INCLUDE 'ogsmbfw'         ! ogsm system variables
    INCLUDE 'ogsmak'          ! aogss variables
    INCLUDE 'ogsmout'

    REAL DCF_AK(AKWELL,AKRGN,AKFUEL)    ! DCF
    REAL PROF_AK(AKFUEL,AKSIZE,AKRGN)   ! PROFITABILITY MEASURE
    REAL COST_AK                        ! DRILLING COST
    REAL FACOST                         ! FACILITY COST
    REAL WELLSSUC(AKFUEL,AKSIZE,AKRGN)  ! TOTAL SUCC. WELLS
    REAL ansprod(MNUMYR)                ! North Slope Production (not including NGLs)
    INTEGER RGN(3)                      ! HOLDS REGION NUMBERS
    DATA RGN/1,2,3/
    REAL PRICE(MNUMYR)


    IF (PRTDBGL .EQ. 1 .AND. OGREPORT(29) .EQ. 2) THEN

        WRITE (BUGOUT,800)
        WRITE (BUGOUT,*)  '************************'
        WRITE (BUGOUT,*)  '********ALASKA**********'
        WRITE (BUGOUT,*)  '************************'
        WRITE (BUGOUT,800)
        WRITE (BUGOUT,*)  '*** EXPLORATORY DRILL COSTS ALASKA  ***'
        WRITE (BUGOUT,820)
        WRITE (BUGOUT,830) (RGN(R),(DRILLAK(1,R,K),K=1,AKFUEL), &
            R=1,AKRGN)

        WRITE (BUGOUT,800)
        WRITE (BUGOUT,*)  '*** DEVELOPMENTAL DRILL COSTS ALASKA  ***'
        WRITE (BUGOUT,820)
        WRITE (BUGOUT,830) (RGN(R),(DRILLAK(2,R,K),K=1,AKFUEL), &
            R=1,AKRGN)

        WRITE (BUGOUT,800)
        WRITE (BUGOUT,*) '*** NEW FIELD WILDCAT DRILL COSTS ALASKA  ***'
        WRITE (BUGOUT,820)
        WRITE (BUGOUT,830) (RGN(R),(DRLNFWAK(R,K),K=1,AKFUEL), &
            R=1,AKRGN)

        WRITE (BUGOUT,800)
        WRITE (BUGOUT,*)  '*** EXPLORATORY LEASE EQUIPMENT COSTS  ***'
        WRITE (BUGOUT,820)
        WRITE (BUGOUT,830) (RGN(R),(EQUIPAK(1,R,K),K=1,AKFUEL), &
            R=1,AKRGN)

        WRITE (BUGOUT,800)
        WRITE (BUGOUT,*)  '*** DEVELOPMENTAL LEASE EQUIPMENT COSTS  ***'
        WRITE (BUGOUT,820)
        WRITE (BUGOUT,830) (RGN(R),(EQUIPAK(2,R,K),K=1,AKFUEL), &
            R=1,AKRGN)

        WRITE (BUGOUT,800)
        WRITE (BUGOUT,*)  '*** OPERATING COSTS   ***'
        WRITE (BUGOUT,823)
        WRITE (BUGOUT,825)
        WRITE (BUGOUT,850) (L,(OPERAK(L,M),M=1,2),L=1,4)

        WRITE (BUGOUT,800)
        WRITE (BUGOUT,*)  '*** EXPLORATORY DCF ALASKA    ***'
        WRITE (BUGOUT,820)
        WRITE (BUGOUT,830) (RGN(R),(DCF_AK(1,R,K),K=1,AKFUEL), &
            R=1,AKRGN)

        WRITE (BUGOUT,800)
        WRITE (BUGOUT,*)  '*** DEVELPOMENTAL DCF ALASKA   ***'
        WRITE (BUGOUT,820)
        WRITE (BUGOUT,830) (RGN(R),(DCF_AK(2,R,K),K=1,AKFUEL), &
            R=1,AKRGN)

        !      WRITE (BUGOUT,800)
        !      WRITE (BUGOUT,*)  '*** SUCCESSFUL WELLS   ***'
        !      WRITE (BUGOUT,820)
        !      WRITE (BUGOUT,830) (((WELLSSUC(K,F,R),R=1,AKRGN),
        !    1                       F=1,AKSIZE),K=1,AKFUEL)

        WRITE (BUGOUT,800)
        WRITE (BUGOUT,*)  '*** PROFITABILITY(OIL) ALASKA -- NFW ***'
        WRITE (BUGOUT,821)
        WRITE (BUGOUT,845) (RGN(R),PROF_AK(1,1,R),R=1,AKRGN)

        !      WRITE (BUGOUT,800)
        !      WRITE (BUGOUT,*)  '*** PROFITABILITY(GAS) ALASKA -- NFW ***'
        !      WRITE (BUGOUT,821)
        !      WRITE (BUGOUT,845) (RGN(R),PROF_AK(2,1,R),R=1,AKRGN)

        !      WRITE (BUGOUT,800)
        !      WRITE (BUGOUT,*)'*** PROFITABILITY(OIL) ALASKA -- DEV. PROJECTS ***'
        !      WRITE (BUGOUT,820)
        !      WRITE (BUGOUT,830) (((PROF_AK(K,F,R),R=1,AKRGN),
        !    1                       F=1,NDP(K,R)),K=1,AKFUEL)

        WRITE (BUGOUT,800)
        WRITE (BUGOUT,*)  '*** EXPLORATORY COSTS   ***'
        WRITE (BUGOUT,*)
        WRITE (BUGOUT,840) COST_AK

        WRITE (BUGOUT,800)
        WRITE (BUGOUT,*)  '*** EXPLORATORY EXPENDITURES    ***'
        WRITE (BUGOUT,*)
        WRITE (BUGOUT,840) SPEND_AK(1)

        WRITE (BUGOUT,800)
        WRITE (BUGOUT,*)  '*** DEVELPOMENTAL EXPENDITURES   ***'
        WRITE (BUGOUT,*)
        WRITE (BUGOUT,840) SPEND_AK(2)
        WRITE (BUGOUT,800)

    ENDIF
    !
    IF (PRTDBGL .EQ. 1 .AND. OGREPORT(15) .GT. 0 .AND. CURIYR .EQ. LASTYR) THEN
        !  --------------  WRITE VARIABLES  ----------------
        ! COMPUTE AVERAGE PRICE FOR CRUDE OIL
        DO M=1,LASTYR
            PRICE(M) = 0
            ansprod(m) = (ogprcoak(1,m)+ogprcoak(2,m))/0.365
            DO R=1,AKRGN
                IF (TOTPRCOAK(M).NE.0) THEN
                    PRICE(M) = PRICE(M) + repprcak(r,1,M)*OGPRCOAK(R,M)/ &
                        TOTPRCOAK(M)
                ELSE
                    PRICE(M) = 0
                ENDIF
            ENDDO
        ENDDO
        !  CRUDE OIL PRODUCTION BY REGION
        !      935 FORMAT(31(/,2X,I4,F9.2,F9.0,F9.2,F9.0,F9.2,4F9.0))
        !      WRITE(SYSOUT,915)
        !      WRITE(SYSOUT,*) '1'
        !      WRITE(SYSOUT,*) ' ALASKA CRUDE OIL PRODUCTION BY REGION'
        !      WRITE(SYSOUT,*) ' (THOUSAND BARRELS PER DAY)'
        !      WRITE(SYSOUT,930)
        !      WRITE(SYSOUT,935) (M+BASEYR-1,(repprcak(r,1,M), &
        !                          OGPRCOAK(R,M)/0.365,R=1,AKRGN), &
        !                          TOTPRCOAK(M)/0.365,ognglak(m), &
        !                          ansprod(m),M=1,LASTYR)
        !  BEGINNING OF YEAR CRUDE OIL RESERVES BY REGION
        WRITE(SYSOUT,915)
        WRITE(SYSOUT,*) '1'
        WRITE(SYSOUT,*) ' ALASKA CRUDE OIL RESERVES BY REGION (BOY)'
        WRITE(SYSOUT,*) ' (MILLION BARRELS)'
        WRITE(SYSOUT,900)
        WRITE(SYSOUT,910) (M+BASEYR-1,PRICE(M), &
            (BOYRESCOAK(R,M),R=1,AKRGN), &
            BOYRESCOAK(1,M)+BOYRESCOAK(2,M)+BOYRESCOAK(3,M),M=1,LASTYR)
        !  CRUDE OIL RESERVE ADDITIONS BY REGION
        WRITE(SYSOUT,915)
        WRITE(SYSOUT,*) '1'
        WRITE(SYSOUT,*)' ALASKA CRUDE OIL RESERVE ADDITIONS BY REGION'
        WRITE(SYSOUT,*) ' (MILLION BARRELS)'
        WRITE(SYSOUT,900)
        WRITE(SYSOUT,910) (M+BASEYR-1,PRICE(M), &
            (RESADCOAK(R,M),R=1,AKRGN), &
            RESADCOAK(1,M)+RESADCOAK(2,M)+RESADCOAK(3,M),M=1,LASTYR)
        ! COMPUTE AVERAGE PRICE FOR NATURAL GAS
        DO M=1,LASTYR
            PRICE(M) = 0
            DO R=1,AKRGN
                IF (TOTPRNGAK(M).NE.0) THEN
                    PRICE(M) = PRICE(M) + repprcak(R,2,M)*OGPRNGAK(R,M)/ &
                        TOTPRNGAK(M)
                ELSE
                    PRICE(M) = 0
                ENDIF
            ENDDO
        ENDDO
        !  NATURAL GAS PRODUCTION BY REGION
        WRITE(SYSOUT,915)
        WRITE(SYSOUT,*) '1'
        WRITE(SYSOUT,*) ' ALASKA NATURAL GAS PRODUCTION BY REGION'
        WRITE(SYSOUT,*) ' (BILLION CUBIC FEET)'
        WRITE(SYSOUT,900)
        WRITE(SYSOUT,910) (M+BASEYR-1,PRICE(M), &
            (OGPRNGAK(R,M),R=1,AKRGN), &
            TOTPRNGAK(M),M=1,LASTYR)
        !  BEGINNING OF YEAR NATURAL GAS RESERVES BY REGION
        WRITE(SYSOUT,915)
        WRITE(SYSOUT,*) '1'
        WRITE(SYSOUT,*) ' ALASKA NATURAL GAS RESERVES BY REGION (BOY)'
        WRITE(SYSOUT,*) ' (BILLION CUBIC FEET)'
        WRITE(SYSOUT,900)
        WRITE(SYSOUT,910) (M+BASEYR-1,PRICE(M), &
            (BOYRESNGAK(R,M),R=1,AKRGN), &
            BOYRESNGAK(1,M)+BOYRESNGAK(2,M)+BOYRESNGAK(3,M),M=1,LASTYR)
        !  NATURAL GAS RESERVE ADDITIONS BY REGION
        WRITE(SYSOUT,915)
        WRITE(SYSOUT,*) '1'
        WRITE(SYSOUT,*)' ALASKA NATURAL GAS RESERVE ADDITIONS BY REGION'
        WRITE(SYSOUT,*) ' (BILLION CUBIC FEET)'
        WRITE(SYSOUT,900)
        WRITE(SYSOUT,910) (M+BASEYR-1,PRICE(M), &
            (RESADNGAK(R,M),R=1,AKRGN), &
            RESADNGAK(1,M)+RESADNGAK(2,M)+RESADNGAK(3,M),M=1,LASTYR)


    ENDIF
800 FORMAT(/)
820 FORMAT('REGION',16X,'OIL',26X,'GAS')
821 FORMAT('REGION')
823 FORMAT('FIELD FLOW')
825 FORMAT(' CATEGORY',13X,'FIXED',23X,'VARIABLE')
830 FORMAT(2X,I2,8X,F17.2,10X,F17.2)
840 FORMAT(12X,F17.2)
845 FORMAT(2X,I2,8X,F17.2)
850 FORMAT(3X,I2,8X,F17.2,10X,F17.2)
900 FORMAT(/,2X,'YEAR',4X,'PRICE',3X,'OFFSHORE',3X,'ONSHORE',5X, &
        'SOUTH',5X,'TOTAL')
910 FORMAT(31(/,2X,I4,5F10.2))
915 FORMAT(/)
920 FORMAT(/,2X,I4,4F10.2)
925 FORMAT(/,2X,'YEAR',2X,'OFFSHORE',3X,'ONSHORE',5X, &
        'SOUTH',5X,'TOTAL')
930 FORMAT(/,2X,'YEAR',4X,'PRICE',1X,'OFFSHORE',4X,'PRICE', &
        2X,'ONSHORE',3X,'PRICE',4X,'SOUTH',4X,'TOTAL', &
        5X,'NGLs',5X,'ANS')
        
    END SUBROUTINE OGREP_AK
    
    !******************************************************************!
    !
    !******************************************************************!
    REAL FUNCTION COSTADJ_AK(TT)
    !
    !       Computes the cost adjustment scalar for the High/Low Capital Cost Scenarios where:
    !           OGRUNOP(17) = 1 is the low cost case
    !           OGRUNOP(17) = 2 is the reference case
    !           OGRUNOP(17) = 3 is the high cost case

    IMPLICIT NONE
    INCLUDE 'parametr'                ! nems parameters
    INCLUDE 'ncntrl'                  ! nems control variables
    INCLUDE 'ogsmparm'                ! ogsm parameters
    INCLUDE 'ogsmak'
    INCLUDE 'ogsmbfw'
    INCLUDE 'ogsmout'

    INTEGER, INTENT(IN) :: TT
    INTEGER NUMYRS         ! Indicates the period of time over which costs rise or decline
    REAL LOWCOST           ! Indicates how much costs fall over the NUMYRS period, percentage change (i.e., 0.5 = 50% decrease)
    REAL HICOST            ! Indicates how much costs rise over the NUMYRS period, percentage change (i.e., 1.0 = 100% increase)
    REAL ADJCOEF           ! Working variable used to calculate COSTADJ_AK

    NUMYRS=4               ! Assumes cost changes occur over 4 year period
    LOWCOST=-0.5           ! Assumes costs decline by 50% over period
    HICOST=1.0             ! Assumes costs double over period
    ADJCOEF=0.0            ! Initializes scalar to 0
    
    !   Low cost case scenario
    IF (OGRUNOP(17).EQ.1) THEN
        IF (TT.lt.akhyr+2) THEN
            ADJCOEF=0.0                                           ! Cost changes occur in last STEO year
        ELSE
            IF (TT.ge.akhyr+2+NUMYRS) THEN
                ADJCOEF=LOWCOST                                 ! Set cost coefficient to stay constant for all future years
            ELSE
                ADJCOEF=LOWCOST*(((TT+1.)-(akhyr+2.))/NUMYRS)      ! Calibrate cost coefficient for interim years
            ENDIF
        ENDIF
    ENDIF

    !   High cost case scenario
    IF (OGRUNOP(17).EQ.3) THEN
        IF (TT.lt.akhyr+2) THEN
            ADJCOEF=0.0
        ELSE
            IF (TT.ge.akhyr+2+NUMYRS) THEN
                ADJCOEF=HICOST
            ELSE
                ADJCOEF=HICOST*(((TT+1.)-(akhyr+2.))/NUMYRS)
            ENDIF
        ENDIF
    ENDIF

    COSTADJ_AK=1.0+ADJCOEF                                             ! Final value equals 1 +- cost adjustment
    
    END FUNCTION COSTADJ_AK
    
