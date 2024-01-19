! $Header: m:/default/source/RCS/wellimp.f,v 1.81 2019/10/02 15:38:33 dh5 Exp $
!********************************************************************************
!********************************************************************************
!********************************************************************************
! Canadian Natural Gas Production
!********************************************************************************
!********************************************************************************
      SUBROUTINE OGCANADA
      IMPLICIT NONE
 
      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmcan'      ! canada natural gas supply variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables
      include'ogsmout'      ! ogsm global output variables
      include'intout'       ! international global output variables
      include'macout'       ! macro global output variables
      include'ngtdmrep'     ! ngtdm global output variables

      INTEGER IYR,STARTYR
      REAL*8 ADGPRD(CNREGION,MNUMYR)
      REAL*8 NAGPRD(CNREGION,MNUMYR)
      REAL*8 NAGPRD0(CNREGION,MNUMYR)
      REAL*8 APRD(CNWELCAT,MNUMYR)
      REAL*8 APRD0(CNWELCAT,MNUMYR)
      REAL*8 WLPRD(CNWELCAT,45)
      REAL*8 CNNGPRC(CNREGION,MNUMYR)
      INTEGER CNWELLS(CNWELCAT,MNUMYR)
!
!  INVOKE INPUT & INITIALIZE ROUTINES IF FIRST YEAR & FIRST ITERATION
      IF (CURIYR == FIRSYR .AND. CURITR == 1) THEN
        CALL OGINIT_CAN   ! READ CANADA NATURAL GAS SUPPLY VARIABLES
        ADGPRD = 0.
        NAGPRD = 0.
        NAGPRD0 = 0.
        APRD = 0.
        APRD0 = 0.
        CNWELLS = 0.
        !  CONVERT OIL & GAS PRICES FROM BASEYR DOLLARS TO 1987 DOLLARS
        DO M=CNHISTYR,ENDYR
          CNBASEWOP(M) = CNBASEWOP(M)*mc_jpgdp(-2)/mc_jpgdp(CNBASEYR-baseyr+1)
          CNBASEHH(M) = CNBASEHH(M)*mc_jpgdp(-2)/mc_jpgdp(CNBASEYR-baseyr+1)
        ENDDO
        !  TOTAL HISTORICAL PRODUCTION TO REGION 
        !  AND DETERMINE AD GAS PRODUCTION AND OTHER FIXED NA VOLUMES FOR PROJECTION YEARS
        DO M=CNHISTYR,ENDYR
          IYR = M-BASEYR+1
          DO R=1,CNPRDCAT
            IF(IYR > L48HYR .and. (R==1.or.R==6.or.R==10)) THEN    ! WCSB SOLUTION (AD) GAS
              IF(BRENT_PRICE(IYR) > CNBASEWOP(M)) THEN
                ADGPRD(CNRMAP(R),IYR) = ADGPRD(CNRMAP(R),IYR) + CNBASEPRD(R,M)*(BRENT_PRICE(IYR)/CNBASEWOP(M))**CNADGEL(1,CNAMAP(R),M)
              ELSE
                ADGPRD(CNRMAP(R),IYR) = ADGPRD(CNRMAP(R),IYR) + CNBASEPRD(R,M)*(BRENT_PRICE(IYR)/CNBASEWOP(M))**CNADGEL(2,CNAMAP(R),M)
              ENDIF
              IF(ADGPRD(CNRMAP(R),IYR) < 0.) ADGPRD(CNRMAP(R),IYR) = 0.
            ELSE
              IF(CNTMAP(R)==1) ADGPRD(CNRMAP(R),IYR) = ADGPRD(CNRMAP(R),IYR) + CNBASEPRD(R,M)
              IF(CNTMAP(R)==2) NAGPRD(CNRMAP(R),IYR) = NAGPRD(CNRMAP(R),IYR) + CNBASEPRD(R,M)
            ENDIF
          ENDDO
          APRD(1,IYR) = CNBASEPRD(2,M)
          APRD(2,IYR) = CNBASEPRD(3,M)
          APRD(3,IYR) = CNBASEPRD(4,M)
          APRD(4,IYR) = CNBASEPRD(5,M)
          APRD(5,IYR) = CNBASEPRD(7,M)
          APRD(6,IYR) = CNBASEPRD(8,M)
          APRD(7,IYR) = CNBASEPRD(9,M)
          APRD(9,IYR) = CNBASEPRD(11,M)
          APRD(10,IYR) = CNBASEPRD(12,M)
        ENDDO

        !  DETERMINE WELL PRODUCTION PROFILE
        DO M=1,45
          DO R=1,CNWELCAT
            IF (CNPRD_B(R) .NE. 0.0) THEN
               WLPRD(R,M) = CNPRD_Q0(R)*365. / (1+CNPRD_Di(R)*CNPRD_B(R)*M)**(1/CNPRD_B(R))
            ELSE
               WLPRD(R,M) = CNPRD_Q0(R)*365.
            ENDIF
            IF(M==1) WLPRD(R,M) = WLPRD(R,M)*0.5
          ENDDO
        ENDDO
      ENDIF

!
!  DETERMINE PRODUCTION FROM ADDITIONAL DRILLING IN THE WESTERN CANADIAN SEDIMENTARY BASIN
    !  CALIBRATE CANADIAN NG PRICE TO HENRY HUB
      CNNGPRC(1,CURIYR) = AMAX1(OGCNPPRD(1,CURIYR) + CNPRCDIFF(1)*mc_jpgdp(-2)/mc_jpgdp(CNBASEYR-baseyr+1),0.)
      CNNGPRC(2,CURIYR) = AMAX1(OGCNPPRD(2,CURIYR) + CNPRCDIFF(2)*mc_jpgdp(-2)/mc_jpgdp(CNBASEYR-baseyr+1),0.)
!     CNNGPRC(1,CURIYR) = AMAX1(OGHHPRNG(CURIYR),0.)
!     CNNGPRC(2,CURIYR) = AMAX1(OGHHPRNG(CURIYR),0.)

      STARTYR = CURIYR - L48HYR 
      IF(STARTYR > 1) THEN
      !  DETERMINE ADDITIONAL DRILLING
        M=STARTYR
        ! DETERMINE ADDITIONAL DRILLING
        !DO R=1,CNWELCAT
        !  IF(BRENT_PRICE(CURIYR) > CNBASEWOP(CURIYR+BASEYR-1)) THEN
        !    CNWELLS(R,curiyr) = NINT((CNDRL_A3(R)*M**3 + CNDRL_A2(R)*M**2 + CNDRL_A1(R)*M + CNDRL_C(R))   &
        !                      * CNNGPRC(2,CURIYR)   &
        !                      * (BRENT_PRICE(CURIYR)/CNBASEWOP(CURIYR+BASEYR-1))**CNDRL_OPRCH(R))
        !  ELSE
        !    CNWELLS(R,curiyr) = NINT((CNDRL_A3(R)*M**3 + CNDRL_A2(R)*M**2 + CNDRL_A1(R)*M + CNDRL_C(R))   &
        !                      * CNNGPRC(2,CURIYR)   &
        !                      * (BRENT_PRICE(CURIYR)/CNBASEWOP(CURIYR+BASEYR-1))**CNDRL_OPRCL(R))
        !  ENDIF
        !  IF (CNWELLS(R,CURIYR) < 0) CNWELLS(R,CURIYR) = 0
        !ENDDO


        DO R=1,CNWELCAT
          IF(BRENT_PRICE(CURIYR) > CNBASEWOP(CURIYR+BASEYR-1)) THEN
            CNWELLS(R,curiyr) = NINT((CNDRL_A3(R)*OGHHPRNG(M)**3 + CNDRL_A2(R)*OGHHPRNG(M)**2 + CNDRL_A1(R)*OGHHPRNG(M) + CNDRL_A0(R)) &
                              * CNNGPRC(2,CURIYR)  * (BRENT_PRICE(CURIYR)/CNBASEWOP(CURIYR+BASEYR-1))**CNDRL_OPRCH(R))
          ELSE
            CNWELLS(R,curiyr) = NINT((CNDRL_A3(R)*OGHHPRNG(M)**3 + CNDRL_A2(R)*OGHHPRNG(M)**2 + CNDRL_A1(R)*OGHHPRNG(M) + CNDRL_A0(R)) &
                              * CNNGPRC(2,CURIYR)  * (BRENT_PRICE(CURIYR)/CNBASEWOP(CURIYR+BASEYR-1))**CNDRL_OPRCL(R))
          ENDIF
          IF (CNWELLS(R,CURIYR) < 0) CNWELLS(R,CURIYR) = 0
        ENDDO

      !  DETERMINE PRODUCTION FROM NEW DRILLING
        IF(CURITR == 1) THEN
          NAGPRD0 = NAGPRD
          APRD0 = APRD
        ENDIF
        NAGPRD = NAGPRD0
        APRD = APRD0
        DO R=1,CNWELCAT
          DO M=1,45
            IYR = CURIYR+M-1
            IF(IYR > MNUMYR) EXIT
              APRD(R,IYR) = APRD(R,IYR) + DFLOAT(CNWELLS(R,CURIYR))*WLPRD(R,M)*(1.0+CNPRD_TEC(R))**(curiyr-l48hyr)
              NAGPRD(2,IYR) = NAGPRD(2,IYR) + DFLOAT(CNWELLS(R,CURIYR))*WLPRD(R,M)
!             IF(CURITR == 1.and.R==1) WRITE(6,400) IYR+BASEYR-1,R,CNWELLS(R,CURIYR),APRD0(R,IYR),APRD(R,IYR)
          ENDDO
        ENDDO

      ENDIF

!
!  OUTPUT PRODUCTION FOR EAST and WEST CANADA
      IF(CURIYR >= CNHISTYR-BASEYR+1) THEN
      !  FIX VOLUMES ACROSS CASES IN YEAR L48HYR+1
        if (curiyr == L48hyr+1 .and. OGSTEO == 1) then
          NAGPRD(:,curiyr) = sCNNAGPRD(:)
          ADGPRD(:,curiyr) = sCNADGPRD(:)
        endif
      !  ASSIGN PRODUCTION TO GLOBAL VARIABLES
        CNENAGPRD(:,curiyr) = NAGPRD(:,curiyr)
        CNADGPRD(:,curiyr) = ADGPRD(:,curiyr)
      !  REMOVE PRODUCTION USED FOR LNG EXPORTS
        CNENAGPRD(2,curiyr) = NAGPRD(2,curiyr) - CNBASELNG(curiyr+baseyr-1)*365.
        if(CNENAGPRD(2,curiyr) < 0.) CNENAGPRD(2,curiyr) = 0.
      !  ASSIGN NA PRODUCTION TO GLOBAL VARIABLE FOR HISTORY ONLY -- NGMM FILLS IN FOR PROJECTION PERIOD
        if (curiyr <= L48hyr) then
          CNRNAGPRD(:,curiyr) = CNENAGPRD(:,curiyr) 
        endif

      !  WRITE DEBUG OUTPUT TO OGSMOUT
        IF(CURIYR == CNHISTYR-BASEYR+1.and.CURITR==1) WRITE(bugout,301),'year','itr','AL-CNV','AL-TIGHT','AL-SHALE','AL-CBM',  &
                                                               'BC-CNV','BC-TIGHT','BC-SHALE','BC-CBM',  &
                                                               'SA-CNV','SA-TIGHT'
        WRITE(bugout,300) CURIYR+BASEYR-1,CURITR,(CNWELLS(R,CURIYR),R=1,CNWELCAT)
        IF(CURIYR == CNHISTYR-BASEYR+1.and.CURITR==1) WRITE(bugout,201),'year','itr','AL-CNV','AL-TIGHT','AL-SHALE','AL-CBM',  &
                                                               'BC-CNV','BC-TIGHT','BC-SHALE','BC-CBM',  &
                                                               'SA-CNV','SA-TIGHT'
        WRITE(bugout,200) CURIYR+BASEYR-1,CURITR,(APRD(R,CURIYR),R=1,CNWELCAT)
        IF(CURIYR == CNHISTYR-BASEYR+1.and.CURITR==1) WRITE(bugout,101),'year','itr','oprat','ngp_E','ngp_W','AD-East','AD-West','NA-East','NA-West'
        WRITE(bugout,100) CURIYR+BASEYR-1,CURITR,BRENT_PRICE(CURIYR)/CNBASEWOP(CURIYR+BASEYR-1),CNNGPRC(1,CURIYR),CNNGPRC(2,CURIYR),  &
                              ADGPRD(1,CURIYR),ADGPRD(2,CURIYR),NAGPRD(1,CURIYR),NAGPRD(2,CURIYR)
      ENDIF

 100  FORMAT(1X,'csm-regprd',2x,2I4,2x,F6.3,2F6.2,2x,4F12.3)
 101  FORMAT(1X,'csm-regprd',2x,2A4,2x,3A6,2x,4A12)
 200  FORMAT(1X,'csm-areaprd',2x,2I4,2x,<cnwelcat>F12.3)
 201  FORMAT(1X,'csm-areaprd',2x,2A4,2x,<cnwelcat>A12)
 300  FORMAT(1X,'csm-wells',2x,2I4,2x,<cnwelcat>I12)
 301  FORMAT(1X,'csm-wells',2x,2A4,2x,<cnwelcat>A12)
 400  FORMAT(1X,'dh5tst',2x,I4,2x,I2,I6,2F12.3)

      RETURN
      END
!********************************************************************************
      SUBROUTINE OGINIT_CAN
      IMPLICIT NONE

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmcan'      ! canada natural gas supply variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables

      INTEGER DUMYR
      character*750 cline

!  INVOKE FILE_MGR TO OPEN INPUT FILE
      FNAME='WLIMP'
      IFILE1 = FILE_MGR('O',FNAME,.FALSE.)

!  READ BASE YEAR
      call ognxtdat(ifile1,cline)
      READ (cline,*) CNBASEYR

!  READ MAPPING
      DO R=1,CNPRDCAT
        call ognxtdat(ifile1,cline)
        READ (cline,*) CNRMAP(R), CNAMAP(R), CNTMAP(R)
      ENDDO

!  READ REMAINING TECHNICALLY RECOVERABLE RESOURCES
      DO R=1,CNRESCAT
        call ognxtdat(ifile1,cline)
        READ (cline,*) CNTRR(R)
      ENDDO

!  READ NEB BASELINE PRICES
      DO M=CNHISTYR,ENDYR
        call ognxtdat(ifile1,cline)
        READ (cline,*) DUMYR, CNBASEWOP(M), CNBASEHH(M)
      ENDDO

!  READ PRICE DIFFERENTIAL
      call ognxtdat(ifile1,cline)
      READ (cline,*) (CNPRCDIFF(R),R=1,CNREGION)

!  READ NEB BENCHLINE NATURAL GAS PRODUCTION
      DO M=CNHISTYR,ENDYR
        call ognxtdat(ifile1,cline)
        READ (cline,*) DUMYR, (CNBASEPRD(R,M),R=1,CNPRDCAT)
      ENDDO

!  READ NATURAL GAS DRILLING EQUATION PARAMETERS
      DO R=1,CNWELCAT
        call ognxtdat(ifile1,cline)
        READ (cline,*) CNDRL_A3(R),CNDRL_A2(R),CNDRL_A1(R),CNDRL_A0(R),CNDRL_OPRCH(R),CNDRL_OPRCL(R)
      ENDDO

!  READ NATURAL GAS WELL PRODUCTION PROFILE PARAMETERS
      DO R=1,CNWELCAT
        call ognxtdat(ifile1,cline)
        READ (cline,*) CNPRD_Q0(R),CNPRD_DI(R),CNPRD_B(R),CNPRD_TEC(R)
      ENDDO

!  READ AD GAS OIL PRICE ELASTICITY
      DO M=2016,ENDYR
        call ognxtdat(ifile1,cline)
        READ (cline,*) DUMYR, (CNADGEL(R,1,M),R=1,2), (CNADGEL(R,2,M),R=1,2), (CNADGEL(R,3,M),R=1,2) 
      ENDDO

      RETURN
      END
!********************************************************************************
