! $Header: m:/default/source/RCS/wellrep.f,v 1.109 2020/10/29 19:17:47 DH5 Exp $

      SUBROUTINE OGREP_OGS
      IMPLICIT NONE

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables
      include'ogsml48'      ! lower 48 variables
      include'ogsmoff'      ! offshore variables
      include'ogsmout'
      include'ngtdmout'
      include'pmmout'

      REAL*8 TOTQOIL(L48RGN)            ! HOLDS TOTAL OIL QUANTITIES
      REAL*8 TOTQGAS(L48RGN)            ! HOLDS TOTAL GAS QUANTITIES
      REAL*8 TOTFUEL(L48FUEL)      ! HOLDS TOTAL FUEL QUANTITIES
      REAL*8 TOTFUEL1(OFFFUEL)     ! HOLDS TOTAL FUEL QUANTITIES
      REAL*8 TOTFUEL2(OFFFUEL)     ! HOLDS TOTAL FUEL QUANTITIES
      REAL*8 TOTFUEL5(OFFFUEL)     ! HOLDS TOTAL FUEL QUANTITIES
      REAL*8 TOTFUEL3              ! HOLDS TOTAL FUEL QUANTITIES
      REAL*8 TOTFUEL4              ! HOLDS TOTAL FUEL QUANTITIES
      REAL*8 TOTFUEL7              ! HOLDS TOTAL FUEL QUANTITIES
      REAL*8 TOTFUEL8              ! HOLDS TOTAL FUEL QUANTITIES
      REAL*8 TOTFUEL9              ! HOLDS TOTAL FUEL QUANTITIES
      REAL*8 TOTFUEL10             ! HOLDS TOTAL FUEL QUANTITIES
      REAL*8 TOTRGN1(OFFRGN)       ! HOLDS TOTAL RGN QUANTITIES
      REAL*8 TOTRGN2(OFFRGN)       ! HOLDS TOTAL RGN QUANTITIES
      REAL*8 TOTRGN3(OFFRGN)       ! HOLDS TOTAL RGN QUANTITIES
      REAL*8 TOTRGN4(OFFRGN)       ! HOLDS TOTAL RGN QUANTITIES
      REAL*8 TOT                   ! HOLDS TOTAL QUANTITIES
      REAL*8 TOTAL1(L48RGN,L48FUEL)! HOLDS TOTAL QUANTITIES
      REAL*8 TOT1(4)               ! HOLDS TOTAL QUANTITIES
      REAL*8 TOT2(4)               ! HOLDS TOTAL QUANTITIES
      REAL*8 PRR(L48RGN,L48FUEL)   ! PR RATIO
      INTEGER YEARVAL

      IF (PRTDBGL.EQ.1) THEN
         YEARVAL = CURIYR + 1989
         WRITE (SYSOUT,803)
         WRITE(SYSOUT,*) '============================'
         WRITE(SYSOUT,*) 'CURRENT YEAR  ',YEARVAL
         WRITE(SYSOUT,*) '============================'
      ENDIF


      IF (PRTDBGL.EQ.1 .AND. OGREPORT(8).GT.0) THEN
!  -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
!       ---->>>> LOWER 48 CONVENTIONAL & UNCONVENTIONAL <<<<----
!  -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
         WRITE (SYSOUT,*) '1'    ! NEW PAGE CHARACTER
         WRITE (SYSOUT,*) ' ****************************************'
         WRITE (SYSOUT,*) ' *****  LOWER 48 COST AND DCF REPORT ****'
         WRITE (SYSOUT,*) ' ****************************************'


         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  EXPLORATORY DRILL COST LOWER 48'
         WRITE (SYSOUT,810)
         WRITE (SYSOUT,800) (R,(DRILLL48(1,R,K),K=1,L48FUEL), &
                            R=1,L48RGN)

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  DEVELOPMENTAL DRILL COST LOWER 48'
         WRITE (SYSOUT,810)
         WRITE (SYSOUT,800) (R,(DRILLL48(2,R,K),K=1,L48FUEL), &
                            R=1,L48RGN)

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  EXPLORATORY DRY COST LOWER 48'
         WRITE (SYSOUT,810)
         WRITE (SYSOUT,800) (R,(DRYL48(1,R,K),K=1,L48FUEL), &
                            R=1,L48RGN)

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  DEVELOPMENTAL DRY COST LOWER 48'
         WRITE (SYSOUT,810)
         WRITE (SYSOUT,800) (R,(DRYL48(2,R,K),K=1,L48FUEL), &
                            R=1,L48RGN)

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  EXPLORATORY LEASE COST LOWER 48'
         WRITE (SYSOUT,810)
         WRITE (SYSOUT,800) (R,(LEASL48(1,R,K),K=1,L48FUEL), &
                            R=1,L48RGN)

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  DEVELOPMENTAL LEASE COST LOWER 48'
         WRITE (SYSOUT,810)
         WRITE (SYSOUT,800) (R,(LEASL48(2,R,K),K=1,L48FUEL), &
                            R=1,L48RGN)

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  EXPLORATORY OPERATE COST LOWER 48'
         WRITE (SYSOUT,810)
         WRITE (SYSOUT,800) (R,(OPERL48(1,R,K),K=1,L48FUEL), &
                            R=1,L48RGN)

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  DEVELOPMENTAL OPERATE COST LOWER 48'
         WRITE (SYSOUT,810)
         WRITE (SYSOUT,800) (R,(OPERL48(2,R,K),K=1,L48FUEL), &
                            R=1,L48RGN)

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  EXPLORATORY DCF LOWER 48'
         WRITE (SYSOUT,810)
         WRITE (SYSOUT,800) (R,(DCFL48(1,R,K),K=1,L48FUEL), &
                            R=1,L48RGN)

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  DEVELOPMENTAL DCF LOWER 48'
         WRITE (SYSOUT,810)
         WRITE (SYSOUT,800) (R,(DCFL48(2,R,K),K=1,L48FUEL), &
                            R=1,L48RGN)

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  EXPLORATORY COST LOWER 48'
         WRITE (SYSOUT,810)
         WRITE (SYSOUT,800) (R,(COSTL48(1,R,K),K=1,L48FUEL), &
                            R=1,L48RGN)

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  DEVELOPMENTAL COST LOWER 48'
         WRITE (SYSOUT,810)
         WRITE (SYSOUT,800) (R,(COSTL48(2,R,K),K=1,L48FUEL), &
                            R=1,L48RGN)

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  EXPLORATORY PROFITABILITY LOWER 48'
         WRITE (SYSOUT,810)
         WRITE (SYSOUT,802) (R,(PROFIRK_L48(1,R,K),K=1,L48FUEL), &
                            R=1,L48RGN)

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  DEVELOPMENTAL PROFITABILITY LOWER 48'
         WRITE (SYSOUT,810)
         WRITE (SYSOUT,802) (R,(PROFIRK_L48(2,R,K),K=1,L48FUEL), &
                            R=1,L48RGN)

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQGAS(R) = 0.0
            TOTQOIL(R) = 0.0
            DO K=1,L48FUEL
              IF (K.LE.2) THEN
               TOTQOIL(R) = TOTQOIL(R) + SPENDIRK_L48(1,R,K)
              ELSE
               TOTQGAS(R) = TOTQGAS(R) + SPENDIRK_L48(1,R,K)
              ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
               TOTFUEL(K) = TOTFUEL(K) + SPENDIRK_L48(1,R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  EXPLORATORY EXPENDITURES LOWER 48'
         WRITE (SYSOUT,815)
         WRITE (SYSOUT,808) (R,(SPENDIRK_L48(1,R,K),K=1,L48FUEL), &
                            TOTQGAS(R),R=1,L48RGN)
         WRITE (SYSOUT,807) (TOTFUEL(K),K=1,L48FUEL),TOT

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
              IF (K.LE.2) THEN
               TOTQOIL(R) = TOTQOIL(R) + SPENDIRK_L48(2,R,K)
              ELSE
               TOTQGAS(R) = TOTQGAS(R) + SPENDIRK_L48(2,R,K)
              ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
               TOTFUEL(K) = TOTFUEL(K) + SPENDIRK_L48(2,R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         WRITE (SYSOUT,*) ' ***  DEVELOPMENTAL EXPENDITURES LOWER 48'
         WRITE (SYSOUT,815)
         WRITE (SYSOUT,808) (R,(SPENDIRK_L48(2,R,K),K=1,L48FUEL), &
                            TOTQGAS(R),R=1,L48RGN)
         WRITE (SYSOUT,807) (TOTFUEL(K),K=1,L48FUEL),TOT

         WRITE (SYSOUT,*) ' ***  TOTAL EXPLORATORY EXPENDITURES ', &
                          '(LOWER48)',SPEND_L48(1)
         WRITE (SYSOUT,*) ' ***  TOTAL DEVELOPMENTAL EXPENDITURES ', &
                          '(LOWER48)',SPEND_L48(2)

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
              IF (K.LE.2) THEN
               TOTQOIL(R) = TOTQOIL(R) + WELLSL48(1,R,K)
              ELSE
               TOTQGAS(R) = TOTQGAS(R) + WELLSL48(1,R,K)
              ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
               TOTFUEL(K) = TOTFUEL(K) + WELLSL48(1,R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  EXPLORATORY WELLS LOWER 48', &
                          ' (SUCCESSFUL AND DRY)'
         WRITE (SYSOUT,721)
         WRITE (SYSOUT,801) (R,(WELLSL48(1,R,K),K=1,L48FUEL), &
                            TOTQGAS(R),R=1,L48RGN)
         WRITE (SYSOUT,805) (TOTFUEL(K),K=1,L48FUEL),TOT

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
              IF (K.LE.2) THEN
               TOTQOIL(R) = TOTQOIL(R) + WELLSL48(2,R,K)
              ELSE
               TOTQGAS(R) = TOTQGAS(R) + WELLSL48(2,R,K)
              ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
               TOTFUEL(K) = TOTFUEL(K) + WELLSL48(2,R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         WRITE (SYSOUT,*) ' ***  DEVELOPMENTAL WELLS LOWER 48', &
                          ' (SUCCESSFUL AND DRY)'
         WRITE (SYSOUT,721)
         WRITE (SYSOUT,801) (R,(WELLSL48(2,R,K),K=1,L48FUEL), &
                            TOTQGAS(R),R=1,L48RGN)
         WRITE (SYSOUT,805) (TOTFUEL(K),K=1,L48FUEL),TOT

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
              IF (K.LE.2) THEN
               TOTQOIL(R) = TOTQOIL(R)+WELLSL48(1,R,K)+ WELLSL48(2,R,K)
              ELSE
               TOTQGAS(R) = TOTQGAS(R)+WELLSL48(1,R,K)+ WELLSL48(2,R,K)
              ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
              TOTFUEL(K) = TOTFUEL(K)+WELLSL48(1,R,K)+WELLSL48(2,R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         DO R = 1,L48RGN
           DO K=1,L48FUEL
             TOTAL1(R,K) = 0.0
           ENDDO
         ENDDO
         DO R = 1,L48RGN
           DO K=1,L48FUEL
             TOTAL1(R,K) = TOTAL1(R,K) + WELLSL48(1,R,K) &
                         + WELLSL48(2,R,K)
           ENDDO
         ENDDO

         WRITE (SYSOUT,*)' ***  EXPLORATORY AND DEVELOPMENTAL WELLS', &
                          ' LOWER 48 (SUCCESSFUL AND DRY)'
         WRITE (SYSOUT,721)
         WRITE (SYSOUT,801) (R,(TOTAL1(R,K),K=1,L48FUEL), &
                            TOTQGAS(R),R=1,L48RGN)
         WRITE (SYSOUT,805) (TOTFUEL(K),K=1,L48FUEL),TOT
!  COMPUTE TOTAL COLUMNS
         TOT = 0.0
         DO I=1,L48WELL
          DO R=1,L48RGN
           DO K=1,L48FUEL
             TOT = TOT + WELLSL48(I,R,K)
           ENDDO
          ENDDO
         ENDDO

         WRITE (SYSOUT,*) ' ***  TOTAL EXPL. AND DEV. DRY WELLS ', &
                          'LOWER 48 (SUCCESSFUL AND DRY)',TOT

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
              IF (K.LE.2) THEN
               TOTQOIL(R) = TOTQOIL(R) + SUCWELLL48(1,R,K)
              ELSE
               TOTQGAS(R) = TOTQGAS(R) + SUCWELLL48(1,R,K)
              ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
               TOTFUEL(K) = TOTFUEL(K) + SUCWELLL48(1,R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  EXPLORATORY SUCCESSFUL WELLS LOWER', &
                          ' 48'
         WRITE (SYSOUT,721)
         WRITE (SYSOUT,801) (R,(SUCWELLL48(1,R,K),K=1,L48FUEL), &
                            TOTQGAS(R),R=1,L48RGN)
         WRITE (SYSOUT,805) (TOTFUEL(K),K=1,L48FUEL),TOT

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
              IF (K.LE.2) THEN
               TOTQOIL(R) = TOTQOIL(R) + SUCWELLL48(2,R,K)
              ELSE
               TOTQGAS(R) = TOTQGAS(R) + SUCWELLL48(2,R,K)
              ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
               TOTFUEL(K) = TOTFUEL(K) + SUCWELLL48(2,R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         WRITE (SYSOUT,*) ' ***  DEVELOPMENTAL SUCCESSFUL WELLS', &
                          ' LOWER 48'
         WRITE (SYSOUT,721)
         WRITE (SYSOUT,801) (R,(SUCWELLL48(2,R,K),K=1,L48FUEL), &
                            TOTQGAS(R),R=1,L48RGN)
         WRITE (SYSOUT,805) (TOTFUEL(K),K=1,L48FUEL),TOT

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
             IF (K.LE.2) THEN
              TOTQOIL(R)=TOTQOIL(R)+SUCWELLL48(1,R,K)+SUCWELLL48(2,R,K)
             ELSE
              TOTQGAS(R)=TOTQGAS(R)+SUCWELLL48(1,R,K)+SUCWELLL48(2,R,K)
             ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
              TOTFUEL(K)=TOTFUEL(K)+SUCWELLL48(1,R,K)+SUCWELLL48(2,R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         DO R = 1,L48RGN
           DO K=1,L48FUEL
             TOTAL1(R,K) = 0.0
             TOTAL1(R,K) = TOTAL1(R,K) + SUCWELLL48(1,R,K) &
                         + SUCWELLL48(2,R,K)
           ENDDO
         ENDDO

         WRITE (SYSOUT,*) ' ***  EXPLORATORY AND DEVELOPMENTAL', &
                          ' SUCCESSFUL WELLS LOWER 48'
         WRITE (SYSOUT,721)
         WRITE (SYSOUT,801) (R,(TOTAL1(R,K),K=1,L48FUEL), &
                            TOTQGAS(R),R=1,L48RGN)
         WRITE (SYSOUT,805) (TOTFUEL(K),K=1,L48FUEL),TOT

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
              IF (K.LE.2) THEN
               TOTQOIL(R) = TOTQOIL(R) + DRYWELLL48(1,R,K)
              ELSE
               TOTQGAS(R) = TOTQGAS(R) + DRYWELLL48(1,R,K)
              ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
               TOTFUEL(K) = TOTFUEL(K) + DRYWELLL48(1,R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         WRITE (SYSOUT,*) ' ***  EXPLORATORY DRY WELLS LOWER 48'
         WRITE (SYSOUT,721)
         WRITE (SYSOUT,801) (R,(DRYWELLL48(1,R,K),K=1,L48FUEL), &
                            TOTQGAS(R),R=1,L48RGN)
         WRITE (SYSOUT,805) (TOTFUEL(K),K=1,L48FUEL),TOT

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
              IF (K.LE.2) THEN
               TOTQOIL(R) = TOTQOIL(R) + DRYWELLL48(2,R,K)
              ELSE
               TOTQGAS(R) = TOTQGAS(R) + DRYWELLL48(2,R,K)
              ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
               TOTFUEL(K) = TOTFUEL(K) + DRYWELLL48(2,R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         WRITE (SYSOUT,*) ' ***  DEVELOPMENTAL DRY WELLS LOWER 48'
         WRITE (SYSOUT,721)
         WRITE (SYSOUT,801) (R,(DRYWELLL48(2,R,K),K=1,L48FUEL), &
                            TOTQGAS(R),R=1,L48RGN)
         WRITE (SYSOUT,805) (TOTFUEL(K),K=1,L48FUEL),TOT

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
             IF (K.LE.2) THEN
              TOTQOIL(R)=TOTQOIL(R)+DRYWELLL48(1,R,K)+DRYWELLL48(2,R,K)
             ELSE
              TOTQGAS(R)=TOTQGAS(R)+DRYWELLL48(1,R,K)+DRYWELLL48(2,R,K)
             ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
              TOTFUEL(K)=TOTFUEL(K)+DRYWELLL48(1,R,K)+DRYWELLL48(2,R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         DO R = 1,L48RGN
           DO K=1,L48FUEL
             TOTAL1(R,K) = 0.0
             TOTAL1(R,K) = TOTAL1(R,K) + DRYWELLL48(1,R,K) &
                         + DRYWELLL48(2,R,K)
           ENDDO
         ENDDO

         WRITE (SYSOUT,*) ' ***  EXPLORATORY AND DEVELOPMENTAL', &
                          ' DRY WELLS LOWER 48'
         WRITE (SYSOUT,721)
         WRITE (SYSOUT,801) (R,(TOTAL1(R,K),K=1,L48FUEL), &
                            TOTQGAS(R),R=1,L48RGN)
         WRITE (SYSOUT,805) (TOTFUEL(K),K=1,L48FUEL),TOT

      ENDIF


      IF (PRTDBGL.EQ.1 .AND. OGREPORT(13).GT.0) THEN
!  -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
!                      ---->>>> OFFSHORE <<<<----
!  -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
         WRITE (SYSOUT,*) '*** DRILL COST OFFSHORE'
         WRITE (SYSOUT,845)
         WRITE (SYSOUT,830)
         WRITE (SYSOUT,835)  (R,DRILLOFF(1,R,1),DRILLOFF(1,R,2), &
              DRILLOFF(2,R,1),DRILLOFF(2,R,2),R=1,OFFRGN)

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) '*** LEASE COST OFFSHORE'
         WRITE (SYSOUT,845)
         WRITE (SYSOUT,830)
         WRITE (SYSOUT,835)  (R,LEASOFF(1,R,1),LEASOFF(1,R,2), &
              LEASOFF(2,R,1),LEASOFF(2,R,2),R=1,OFFRGN)

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) '*** OPERATE COST OFFSHORE'
         WRITE (SYSOUT,845)
         WRITE (SYSOUT,830)
         WRITE (SYSOUT,835)  (R,OPEROFF(1,R,1),OPEROFF(1,R,2), &
              OPEROFF(2,R,1),OPEROFF(2,R,2),R=1,OFFRGN)

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) '*** DRY COST OFFSHORE'
         WRITE (SYSOUT,845)
         WRITE (SYSOUT,830)
         WRITE (SYSOUT,835)  (R,DRYOFF(1,R,1),DRYOFF(1,R,2), &
              DRYOFF(2,R,1),DRYOFF(2,R,2),R=1,OFFRGN)

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  DISCOUNTED CASH FLOW OFFSHORE'
         WRITE (SYSOUT,845)
         WRITE (SYSOUT,830)
         WRITE (SYSOUT,835) (R,DCFOFF(1,R,1),DCFOFF(1,R,2), &
              DCFOFF(2,R,1),DCFOFF(2,R,2),R=1,OFFRGN)

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  COST OFFSHORE'
         WRITE (SYSOUT,845)
         WRITE (SYSOUT,830)
         WRITE (SYSOUT,835) (R,COSTOFF(1,R,1),COSTOFF(1,R,2), &
              COSTOFF(2,R,1),COSTOFF(2,R,2),R=1,OFFRGN)

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  PROFITABILITY OFFSHORE'
         WRITE (SYSOUT,845)
         WRITE (SYSOUT,830)
         WRITE (SYSOUT,840) (R,PROFIRK_OFF(1,R,1), &
              PROFIRK_OFF(1,R,2),PROFIRK_OFF(2,R,2), &
              PROFIRK_OFF(2,R,2),R=1,OFFRGN)

!  COMPUTE TOTALS
      DO R=1,OFFRGN
        TOTRGN1(R)=0.0
        TOTRGN2(R)=0.0
        DO K=1,OFFFUEL
          TOTRGN1(R)=TOTRGN1(R) + SPENDIRK_OFF(1,R,K)
          TOTRGN2(R)=TOTRGN2(R) + SPENDIRK_OFF(2,R,K)
        ENDDO
      ENDDO

!  COMPUTE TOTAL COLUMNS
      DO K=1,OFFFUEL
        TOTFUEL1(K) = 0.0
        TOTFUEL2(K) = 0.0
        DO R=1,OFFRGN
          TOTFUEL1(K) = TOTFUEL1(K) + SPENDIRK_OFF(1,R,K)
          TOTFUEL2(K) = TOTFUEL2(K) + SPENDIRK_OFF(2,R,K)
        ENDDO
      ENDDO
      TOTFUEL3 = 0.0
      TOTFUEL4 = 0.0
      DO R=1,OFFRGN
        TOTFUEL3 = TOTFUEL3 + TOTRGN1(R)
        TOTFUEL4 = TOTFUEL4 + TOTRGN2(R)
      ENDDO

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  EXPENDITURES OFFSHORE'
         WRITE (SYSOUT,875)
         WRITE (SYSOUT,846)
         WRITE (SYSOUT,847)
         WRITE (SYSOUT,880) (R,SPENDIRK_OFF(1,R,1), &
              SPENDIRK_OFF(1,R,2),SPENDIRK_OFF(2,R,1), &
              SPENDIRK_OFF(2,R,2),TOTRGN1(R),TOTRGN2(R),R=1,OFFRGN)
         WRITE (SYSOUT,885)TOTFUEL1(1),TOTFUEL1(2),TOTFUEL2(1), &
              TOTFUEL2(2),TOTFUEL3,TOTFUEL4

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  TOTAL EXPLORATORY EXPENDITURES ', &
                          '(OFFSHORE)',SPEND_OFF(1)
         WRITE (SYSOUT,*) ' ***  TOTAL DEVELOPMENTAL EXPENDITURES ', &
                          '(OFFSHORE)',SPEND_OFF(2)
         WRITE (SYSOUT,*)

!  COMPUTE TOTALS
      DO R=1,OFFRGN
        TOTRGN1(R)=0.0
        TOTRGN2(R)=0.0
        TOTRGN1(R) = TOTRGN1(R)+SUCWELLOFF(1,R,1)+DRYWELLOFF(1,R,1)
        TOTRGN2(R) = TOTRGN2(R)+SUCWELLOFF(1,R,2)+DRYWELLOFF(1,R,2)
      ENDDO

!  COMPUTE TOTAL COLUMNS
      DO K=1,OFFFUEL
        TOTFUEL1(K) = 0.0
        TOTFUEL2(K) = 0.0
        DO R=1,OFFRGN
          TOTFUEL1(K) = TOTFUEL1(K) + SUCWELLOFF(1,R,K)
          TOTFUEL2(K) = TOTFUEL2(K) + DRYWELLOFF(1,R,K)
        ENDDO
      ENDDO
      TOTFUEL3 = 0.0
      TOTFUEL4 = 0.0
      DO R=1,OFFRGN
        TOTFUEL3 = TOTFUEL3 + TOTRGN1(R)
        TOTFUEL4 = TOTFUEL4 + TOTRGN2(R)
      ENDDO

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  EXPLORATORY SUCCESSFUL AND DRY', &
                          ' WELLS OFFSHORE'
         WRITE (SYSOUT,875)
         WRITE (SYSOUT,850)
         WRITE (SYSOUT,847)
         WRITE (SYSOUT,836) (R,SUCWELLOFF(1,R,1), &
              SUCWELLOFF(1,R,2),DRYWELLOFF(1,R,1), &
              DRYWELLOFF(1,R,2),TOTRGN1(R),TOTRGN2(R),R=1,OFFRGN)
         WRITE (SYSOUT,837) TOTFUEL1(1),TOTFUEL1(2),TOTFUEL2(1), &
              TOTFUEL2(2),TOTFUEL3,TOTFUEL4

!  COMPUTE TOTALS
      DO R=1,OFFRGN
        TOTRGN1(R)=0.0
        TOTRGN2(R)=0.0
        TOTRGN1(R) = TOTRGN1(R)+SUCWELLOFF(2,R,1)+DRYWELLOFF(2,R,1)
        TOTRGN2(R) = TOTRGN2(R)+SUCWELLOFF(2,R,2)+DRYWELLOFF(2,R,2)
      ENDDO

!  COMPUTE TOTAL COLUMNS
      DO K=1,OFFFUEL
        TOTFUEL1(K) = 0.0
        TOTFUEL2(K) = 0.0
        TOTFUEL5(K) = 0.0
        DO R=1,OFFRGN
          TOTFUEL1(K) = TOTFUEL1(K) + SUCWELLOFF(2,R,K)
          TOTFUEL2(K) = TOTFUEL2(K) + DRYWELLOFF(2,R,K)
        ENDDO
      ENDDO
      TOTFUEL3 = 0.0
      TOTFUEL4 = 0.0
      DO R=1,OFFRGN
        TOTFUEL3 = TOTFUEL3 + TOTRGN1(R)
        TOTFUEL4 = TOTFUEL4 + TOTRGN2(R)
      ENDDO

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  DEVELOPMENTAL SUCCESSFUL AND DRY', &
                          ' WELLS OFFSHORE'
         WRITE (SYSOUT,875)
         WRITE (SYSOUT,850)
         WRITE (SYSOUT,847)
         WRITE (SYSOUT,836) (R,SUCWELLOFF(2,R,1), &
              SUCWELLOFF(2,R,2),DRYWELLOFF(2,R,1), &
              DRYWELLOFF(2,R,2),TOTRGN1(R),TOTRGN2(R),R=1,OFFRGN)
         WRITE (SYSOUT,837)TOTFUEL1(1),TOTFUEL1(2),TOTFUEL2(1), &
              TOTFUEL2(2),TOTFUEL3,TOTFUEL4

!  COMPUTE TOTALS
      DO R=1,OFFRGN
        TOTRGN1(R)=0.0
        TOTRGN2(R)=0.0
        TOTRGN3(R)=0.0
        TOTRGN4(R)=0.0
        TOTRGN1(R) = TOTRGN1(R)+SUCWELLOFF(1,R,1)+SUCWELLOFF(2,R,1)
        TOTRGN2(R) = TOTRGN2(R)+SUCWELLOFF(1,R,2)+SUCWELLOFF(2,R,2)
        TOTRGN3(R) = TOTRGN3(R)+DRYWELLOFF(1,R,1)+DRYWELLOFF(2,R,1)
        TOTRGN4(R) = TOTRGN4(R)+DRYWELLOFF(1,R,2)+DRYWELLOFF(2,R,2)
      ENDDO

!  COMPUTE TOTAL COLUMNS
      TOTFUEL3 = 0.0
      TOTFUEL4 = 0.0
      TOTFUEL7 = 0.0
      TOTFUEL8 = 0.0
      DO R=1,OFFRGN
        TOTFUEL3 = TOTFUEL3 + TOTRGN1(R)
        TOTFUEL4 = TOTFUEL4 + TOTRGN2(R)
        TOTFUEL7 = TOTFUEL7 + TOTRGN3(R)
        TOTFUEL8 = TOTFUEL8 + TOTRGN4(R)
      ENDDO

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  EXPLORATORY AND DEVELOPMENTAL', &
                          ' SUCCESSFUL AND DRY WELLS OFFSHORE'
         WRITE (SYSOUT,850)
         WRITE (SYSOUT,848)
         WRITE (SYSOUT,839) (R,TOTRGN1(R),TOTRGN2(R),TOTRGN3(R), &
              TOTRGN4(R),R=1,OFFRGN)
         WRITE (SYSOUT,838)TOTFUEL3,TOTFUEL4,TOTFUEL7,TOTFUEL8


!  COMPUTE TOTAL COLUMNS
      TOTFUEL3 = 0.0
      TOTFUEL4 = 0.0
      TOTFUEL7 = 0.0
      TOTFUEL8 = 0.0
      TOTFUEL9 = 0.0
      TOTFUEL10= 0.0
      DO R = 1,L48RGN
        TOTFUEL3 = TOTFUEL3 + SUCWELLL48(1,R,1) + SUCWELLL48(1,R,2)
      ENDDO
      DO R = 1,L48RGN
        DO K=3,L48FUEL
          TOTFUEL4 = TOTFUEL4 + SUCWELLL48(1,R,K)
        ENDDO
      ENDDO
      DO R = 1,L48RGN
        TOTFUEL7 = TOTFUEL7 + SUCWELLL48(2,R,1) + SUCWELLL48(2,R,2)
      ENDDO
      DO R = 1,L48RGN
        DO K=3,L48FUEL
          TOTFUEL8 = TOTFUEL8 + SUCWELLL48(2,R,K)
        ENDDO
      ENDDO
      DO R = 1,OFFRGN
        TOTFUEL3 = TOTFUEL3 + SUCWELLOFF(1,R,1)
      ENDDO
      DO R = 1,OFFRGN
        DO K=2,OFFFUEL
          TOTFUEL4 = TOTFUEL4 + SUCWELLOFF(1,R,K)
        ENDDO
      ENDDO
      DO R = 1,OFFRGN
        TOTFUEL7 = TOTFUEL7 + SUCWELLOFF(2,R,1)
      ENDDO
      DO R = 1,OFFRGN
        DO K=2,OFFFUEL
          TOTFUEL8 = TOTFUEL8 + SUCWELLOFF(2,R,K)
        ENDDO
      ENDDO
      DO R = 1,L48RGN
        DO K = 1,L48FUEL
          TOTFUEL9 = TOTFUEL9 + DRYWELLL48(1,R,K)
          TOTFUEL10 = TOTFUEL10 + DRYWELLL48(2,R,K)
        ENDDO
      ENDDO
      DO R = 1,OFFRGN
        DO K = 1,OFFFUEL
          TOTFUEL9 = TOTFUEL9 + DRYWELLOFF(1,R,K)
          TOTFUEL10 = TOTFUEL10 + DRYWELLOFF(2,R,K)
        ENDDO
      ENDDO
      DO R=1,4
        TOT1(R)=0.0
        TOT2(R)=0.0
      ENDDO
      TOT1(1) = TOTFUEL3 + TOTFUEL4 + TOTFUEL9
      TOT1(2) = TOTFUEL7 + TOTFUEL8 + TOTFUEL10
      TOT2(1) = TOTFUEL3 + TOTFUEL7
      TOT2(2) = TOTFUEL4 + TOTFUEL8
      TOT2(3) = TOTFUEL9 + TOTFUEL10
      TOT = TOT2(1) + TOT2(2) + TOT2(3)

         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  TOTAL WELLS (ONSHORE AND', &
                          ' OFFSHORE)'
         WRITE (SYSOUT,*)
         WRITE (SYSOUT,870)
         WRITE (SYSOUT,855)TOTFUEL3,TOTFUEL4,TOTFUEL9,TOT1(1)
         WRITE (SYSOUT,860)TOTFUEL7,TOTFUEL8,TOTFUEL10,TOT1(2)
         WRITE (SYSOUT,865)TOT2(1),TOT2(2),TOT2(3),TOT
         ENDIF

!  -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
!                      ---->>>> PRICES <<<<----
!  -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      IF (PRTDBGL.EQ.1 .AND. OGREPORT(4).GT.0) THEN
         WRITE (SYSOUT,*) '1'
         WRITE (SYSOUT,*) '===================================='
         WRITE (SYSOUT,*) '======   OGSM  PRICE  VECTORS ======'
         WRITE (SYSOUT,*) '===================================='
         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  ALASKA PRICES  ***'
         WRITE (SYSOUT,766)
         WRITE (SYSOUT,762) (R,(OGPRCAK(R,K,1),K=1,AKFUEL),R=1,AKRGN)
         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  LOWER48 PRICES  ***'
         WRITE (SYSOUT,768)
         WRITE (SYSOUT,763) (R,(OGPRCL48(R,K,1),K=1,L48FUEL),R=1,L48RGN)
         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' ***  OFFSHORE PRICES  ***'
         WRITE (SYSOUT,766)
         WRITE (SYSOUT,764) (R,(OGPRCOFF(R,K,1),K=1,OFFFUEL),R=1,OFFRGN)
      ENDIF   ! END REPORTING PRICES


!  -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
!              ---->>>> RESERVES ACCOUNTING <<<<----
!  -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      IF (PRTDBGL.EQ.1 .AND. OGREPORT(6).GT.0) THEN
         WRITE(SYSOUT,*) '1'   ! NEW PAGE CHARACTER
         WRITE(SYSOUT,*) ' ***** LOWER 48 RESERVES ACCOUNTING ****'
         WRITE(SYSOUT,*) ' GAS UNITS = BCF;   OIL UNITS = MMB'
         WRITE(SYSOUT,*)
         WRITE(SYSOUT,*) ' ***  FINDING RATE PARAMETER (FR1L48)'
         WRITE(SYSOUT,720)
         WRITE(SYSOUT,702) (R,(FR1L48(R,K),K=1,L48FUEL), &
                           R=1,L48RGN)
         WRITE(SYSOUT,720)
         WRITE(SYSOUT,*) ' ***  FINDING RATE PARAMETER (FR2L48)'
         WRITE(SYSOUT,720)
         WRITE(SYSOUT,702) (R,(FR2L48(R,K),K=1,L48FUEL), &
                           R=1,L48RGN)
         WRITE(SYSOUT,720)
         WRITE(SYSOUT,*) ' ***  FINDING RATE PARAMETER (FR3L48)'
         WRITE(SYSOUT,720)
         WRITE(SYSOUT,702) (R,(FR3L48(R,K),K=1,L48FUEL), &
                           R=1,L48RGN)
         WRITE(SYSOUT,*) ' ***  DELTA PARAMETER (DELTA1L48)'
         WRITE(SYSOUT,720)
         WRITE(SYSOUT,700) (R,(DELTA1L48(R,K),K=1,L48FUEL), &
                           R=1,L48RGN)
         WRITE(SYSOUT,*) ' ***  DELTA PARAMETER (DELTA2L48)'
         WRITE(SYSOUT,720)
         WRITE(SYSOUT,700) (R,(DELTA2L48(R,K),K=1,L48FUEL), &
                           R=1,L48RGN)
         WRITE(SYSOUT,*) ' ***  DELTA PARAMETER (DELTA3L48)'
         WRITE(SYSOUT,720)
         WRITE(SYSOUT,700) (R,(DELTA3L48(R,K),K=1,L48FUEL), &
                           R=1,L48RGN)

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
              IF (K.LE.2) THEN
               TOTQOIL(R) = TOTQOIL(R) + NFWL48(R,K)
              ELSE
               TOTQGAS(R) = TOTQGAS(R) + NFWL48(R,K)
              ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
               TOTFUEL(K) = TOTFUEL(K) + NFWL48(R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         WRITE(SYSOUT,*) ' ***  NEW FIELD WILDCAT WELLS (NFWL48)'
         WRITE(SYSOUT,721)
!        WRITE(SYSOUT,705) (R,(NFWL48(R,K),K=1,L48FUEL),
!    1                     R=1,L48RGN)
         WRITE(SYSOUT,801) (R,(NFWL48(R,K),K=1,L48FUEL), &
                            TOTQGAS(R),R=1,L48RGN)
         WRITE(SYSOUT,805) (TOTFUEL(K),K=1,L48FUEL),TOT

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
              IF (K.LE.2) THEN
               TOTQOIL(R) = TOTQOIL(R) + DEVL48(R,K)
              ELSE
               TOTQGAS(R) = TOTQGAS(R) + DEVL48(R,K)
              ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
               TOTFUEL(K) = TOTFUEL(K) + DEVL48(R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         WRITE(SYSOUT,*) ' ***  DEVELOPMENTAL WELLS (DEVL48)'
         WRITE(SYSOUT,721)
!        WRITE(SYSOUT,705) (R,(DEVL48(R,K),K=1,L48FUEL),
!    1                     R=1,L48RGN)
         WRITE(SYSOUT,801) (R,(DEVL48(R,K),K=1,L48FUEL), &
                            TOTQGAS(R),R=1,L48RGN)
         WRITE(SYSOUT,805) (TOTFUEL(K),K=1,L48FUEL),TOT

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
              IF (K.LE.2) THEN
               TOTQOIL(R) = TOTQOIL(R) + OEXPL48(R,K)
              ELSE
               TOTQGAS(R) = TOTQGAS(R) + OEXPL48(R,K)
              ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
               TOTFUEL(K) = TOTFUEL(K) + OEXPL48(R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         WRITE(SYSOUT,*) ' ***  OTHER EXPLORATORY WELLS (OEXPL48)'
         WRITE(SYSOUT,721)
!        WRITE(SYSOUT,705) (R,(OEXPL48(R,K),K=1,L48FUEL),
!    1                     R=1,L48RGN)
         WRITE(SYSOUT,801) (R,(OEXPL48(R,K),K=1,L48FUEL), &
                            TOTQGAS(R),R=1,L48RGN)
         WRITE(SYSOUT,805) (TOTFUEL(K),K=1,L48FUEL),TOT

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
              IF (K.LE.2) THEN
               TOTQOIL(R) = TOTQOIL(R) + NRDL48(R,K)
              ELSE
               TOTQGAS(R) = TOTQGAS(R) + NRDL48(R,K)
              ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
               TOTFUEL(K) = TOTFUEL(K) + NRDL48(R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         WRITE(SYSOUT,*) ' ***  NEW RESERVE DISCOVERIES (NRDL48)'
         WRITE(SYSOUT,721)
!        WRITE(SYSOUT,704) (R,(NRDL48(R,K),K=1,L48FUEL),
!    1                     R=1,L48RGN)
         WRITE(SYSOUT,801) (R,(NRDL48(R,K),K=1,L48FUEL), &
                            TOTQGAS(R),R=1,L48RGN)
         WRITE(SYSOUT,805) (TOTFUEL(K),K=1,L48FUEL),TOT

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
              IF (K.LE.2) THEN
               TOTQOIL(R) = TOTQOIL(R) + NDIRL48(R,K)
              ELSE
               TOTQGAS(R) = TOTQGAS(R) + NDIRL48(R,K)
              ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
               TOTFUEL(K) = TOTFUEL(K) + NDIRL48(R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         WRITE(SYSOUT,*) ' ***  NEW DISCOVERIES INFRD RSVS (NDIRL48)'
         WRITE(SYSOUT,721)
!        WRITE(SYSOUT,704) (R,(NDIRL48(R,K),K=1,L48FUEL),
!    1                     R=1,L48RGN)
         WRITE(SYSOUT,801) (R,(NDIRL48(R,K),K=1,L48FUEL), &
                            TOTQGAS(R),R=1,L48RGN)
         WRITE(SYSOUT,805) (TOTFUEL(K),K=1,L48FUEL),TOT

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
              IF (K.LE.2) THEN
               TOTQOIL(R) = TOTQOIL(R) + REVL48(R,K)
              ELSE
               TOTQGAS(R) = TOTQGAS(R) + REVL48(R,K)
              ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
               TOTFUEL(K) = TOTFUEL(K) + REVL48(R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         WRITE(SYSOUT,*) ' ***  RESERVE REVISIONS (REVL48)'
         WRITE(SYSOUT,721)
!        WRITE(SYSOUT,704) (R,(REVL48(R,K),K=1,L48FUEL),
!    1                     R=1,L48RGN)
         WRITE(SYSOUT,801) (R,(REVL48(R,K),K=1,L48FUEL), &
                            TOTQGAS(R),R=1,L48RGN)
         WRITE(SYSOUT,805) (TOTFUEL(K),K=1,L48FUEL),TOT

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
              IF (K.LE.2) THEN
               TOTQOIL(R) = TOTQOIL(R) + EXTL48(R,K)
              ELSE
               TOTQGAS(R) = TOTQGAS(R) + EXTL48(R,K)
              ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
               TOTFUEL(K) = TOTFUEL(K) + EXTL48(R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         WRITE(SYSOUT,*) ' ***  RESERVE EXTENSIONS (EXTL48)'
         WRITE(SYSOUT,721)
!        WRITE(SYSOUT,704) (R,(EXTL48(R,K),K=1,L48FUEL),
!    1                     R=1,L48RGN)
         WRITE(SYSOUT,801) (R,(EXTL48(R,K),K=1,L48FUEL), &
                            TOTQGAS(R),R=1,L48RGN)
         WRITE(SYSOUT,805) (TOTFUEL(K),K=1,L48FUEL),TOT

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
              IF (K.LE.2) THEN
               TOTQOIL(R) = TOTQOIL(R) + RESADL48(R,K)
              ELSE
               TOTQGAS(R) = TOTQGAS(R) + RESADL48(R,K)
              ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
               TOTFUEL(K) = TOTFUEL(K) + RESADL48(R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         WRITE(SYSOUT,*) ' ***  RESERVE ADDITIONS (RESADL48)'
         WRITE(SYSOUT,721)
!        WRITE(SYSOUT,704) (R,(RESADL48(R,K),K=1,L48FUEL),
!    1                     R=1,L48RGN)
         WRITE(SYSOUT,801) (R,(RESADL48(R,K),K=1,L48FUEL), &
                            TOTQGAS(R),R=1,L48RGN)
         WRITE(SYSOUT,805) (TOTFUEL(K),K=1,L48FUEL),TOT

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
              IF (K.LE.2) THEN
               TOTQOIL(R) = TOTQOIL(R) + CUMR1L48(R,K)
              ELSE
               TOTQGAS(R) = TOTQGAS(R) + CUMR1L48(R,K)
              ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
               TOTFUEL(K) = TOTFUEL(K) + CUMR1L48(R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         WRITE(SYSOUT,*) ' ***  CUMULATIVE RESERVES (CUMR1L48)'
         WRITE(SYSOUT,721)
!        WRITE(SYSOUT,704) (R,(CUMR1L48(R,K),K=1,L48FUEL),
!    1                     R=1,L48RGN)
         WRITE(SYSOUT,801) (R,(CUMR1L48(R,K),K=1,L48FUEL), &
                            TOTQGAS(R),R=1,L48RGN)
         WRITE(SYSOUT,805) (TOTFUEL(K),K=1,L48FUEL),TOT

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
              IF (K.LE.2) THEN
               TOTQOIL(R) = TOTQOIL(R) + CUMR2L48(R,K)
              ELSE
               TOTQGAS(R) = TOTQGAS(R) + CUMR2L48(R,K)
              ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
               TOTFUEL(K) = TOTFUEL(K) + CUMR2L48(R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         WRITE(SYSOUT,*) ' ***  CUMULATIVE RESERVES (CUMR2L48)'
         WRITE(SYSOUT,721)
!        WRITE(SYSOUT,704) (R,(CUMR2L48(R,K),K=1,L48FUEL),
!    1                     R=1,L48RGN)
         WRITE(SYSOUT,801) (R,(CUMR2L48(R,K),K=1,L48FUEL), &
                            TOTQGAS(R),R=1,L48RGN)
         WRITE(SYSOUT,805) (TOTFUEL(K),K=1,L48FUEL),TOT

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
              IF (K.LE.2) THEN
               TOTQOIL(R) = TOTQOIL(R) + CUMR3L48(R,K)
              ELSE
               TOTQGAS(R) = TOTQGAS(R) + CUMR3L48(R,K)
              ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
               TOTFUEL(K) = TOTFUEL(K) + CUMR3L48(R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO


         WRITE(SYSOUT,*) ' ***  CUMULATIVE RESERVES (CUMR3L48)'
         WRITE(SYSOUT,721)
!        WRITE(SYSOUT,704) (R,(CUMR3L48(R,K),K=1,L48FUEL),
!    1                     R=1,L48RGN)
         WRITE(SYSOUT,801) (R,(CUMR3L48(R,K),K=1,L48FUEL), &
                            TOTQGAS(R),R=1,L48RGN)
         WRITE(SYSOUT,805) (TOTFUEL(K),K=1,L48FUEL),TOT

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
              IF (K.LE.2) THEN
               TOTQOIL(R) = TOTQOIL(R) + RESBOYL48(R,K)
              ELSE
               TOTQGAS(R) = TOTQGAS(R) + RESBOYL48(R,K)
              ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
               TOTFUEL(K) = TOTFUEL(K) + RESBOYL48(R,K)
               IF (CURRESL48(R,K).GT.0.0) THEN
                 PRR(R,K) = PRDL48(R,K)/CURRESL48(R,K)
               ELSE
                 PRR(R,K) = 0.0
               ENDIF
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         WRITE(SYSOUT,*) ' ***  BEGINING-OF-YEAR RESERVES(RESBOYL48)'
         WRITE(SYSOUT,721)
         WRITE(SYSOUT,710) (R,(RESBOYL48(R,K),K=1,L48FUEL), &
                           TOTQGAS(R),R=1,L48RGN)
         WRITE(SYSOUT,805) (TOTFUEL(K),K=1,L48FUEL),TOT
         WRITE(SYSOUT,*) ' ***  P/R RATIO (PRRATL48)'
         WRITE(SYSOUT,720)
         WRITE(SYSOUT,700) (R,(PRR(R,K),K=1,L48FUEL),R=1,L48RGN)
!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
              IF (K.LE.2) THEN
               TOTQOIL(R) = TOTQOIL(R) + URRL48(R,K)
              ELSE
               TOTQGAS(R) = TOTQGAS(R) + URRL48(R,K)
              ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
               TOTFUEL(K) = TOTFUEL(K) + URRL48(R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         WRITE(SYSOUT,*) ' ***  UNDISCOVERED REC. RESOURCES(URRL48)'
         WRITE(SYSOUT,721)
         WRITE(SYSOUT,710) (R,(URRL48(R,K),K=1,L48FUEL), &
                           TOTQGAS(R),R=1,L48RGN)
         WRITE(SYSOUT,805) (TOTFUEL(K),K=1,L48FUEL),TOT

!  COMPUTE TOTAL COLUMNS
         TOT = 0.0
         DO R = 1,L48RGN
           TOT = TOT + PRDOGSM(R)
         ENDDO

         WRITE(SYSOUT,*) ' ***  PRODUCTION AT OGSM LEVEL (PRDOGSM)'
         WRITE(SYSOUT,725)
         WRITE(SYSOUT,709) (R,PRDOGSM(R),R=1,L48RGN)
         WRITE(SYSOUT,806) TOT
      ENDIF

      IF (PRTDBGL.EQ.1 .AND. OGREPORT(5).GT.0) THEN

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
              IF (K.LE.2) THEN
               TOTQOIL(R) = TOTQOIL(R) + PRDL48(R,K)
              ELSE
               TOTQGAS(R) = TOTQGAS(R) + PRDL48(R,K)
              ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
               TOTFUEL(K) = TOTFUEL(K) + PRDL48(R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         WRITE(SYSOUT,*)' ***  PRODUCTION OGSM/FUEL LEVEL (PRDL48)'
         WRITE(SYSOUT,721)
         WRITE(SYSOUT,710) (R,(PRDL48(R,K),K=1,L48FUEL), &
                           TOTQGAS(R),R=1,L48RGN)
         WRITE(SYSOUT,805) (TOTFUEL(K),K=1,L48FUEL),TOT

!  COMPUTE TOTAL COLUMNS
         DO R = 1,L48RGN
            TOTQOIL(R) = 0.0
            TOTQGAS(R) = 0.0
            DO K=1,L48FUEL
              IF (K.LE.2) THEN
               TOTQOIL(R) = TOTQOIL(R) + EXPRDL48(R,K)
              ELSE
               TOTQGAS(R) = TOTQGAS(R) + EXPRDL48(R,K)
              ENDIF
            ENDDO
         ENDDO

         DO K=1,L48FUEL
            TOTFUEL(K) = 0.0
            DO R=1,L48RGN
               TOTFUEL(K) = TOTFUEL(K) + EXPRDL48(R,K)
            ENDDO
         ENDDO

         TOT = 0.0
         DO R=1,L48RGN
            TOT = TOT + TOTQGAS(R)
         ENDDO

         WRITE(SYSOUT,*)' ***  EXPECTED PRODUCTION  (EXPRDL48)'
         WRITE(SYSOUT,721)
         WRITE(SYSOUT,710) (R,(EXPRDL48(R,K),K=1,L48FUEL), &
                           TOTQGAS(R),R=1,L48RGN)
         WRITE(SYSOUT,805) (TOTFUEL(K),K=1,L48FUEL),TOT

         WRITE(SYSOUT,*)
         WRITE(SYSOUT,*) ' ***  PRODUCTION FROM PMM (RFQDCRD)'
         WRITE(SYSOUT,*) '    RGN1           RGN2          RGN3     ' &
                        ,'    RGN4           RGN5          RGN6     RGN7'
         WRITE(SYSOUT,*) (RFQDCRD(R,CURIYR),R=1,L48RGN)
         WRITE(SYSOUT,*)

      ENDIF

!  ------  RESERVES ACCOUNTING OUTPUT FORMATS  ------
  700 FORMAT(<L48RGN>(4X,I1,7E15.5,/))
  702 FORMAT(<L48RGN>(4X,I1,7F15.5,/))
  703 FORMAT(17(3X,I2,1X,7F15.2,/))
  704 FORMAT(<L48RGN>(4X,I1,1X,7F15.2,/))
  705 FORMAT(<L48RGN>(4X,I1,7F15.2,/))
  706 FORMAT(<L48RGN>(4X,I1,7F15.0,/))
  707 FORMAT(<L48RGN>(4X,I1,7F15.3,/))
  709 FORMAT(<L48RGN>(4X,I1,F15.5,/))
  711 FORMAT(17(3X,I2,1X,8F15.2,/))
  710 FORMAT(<L48RGN>(4X,I1,1X,8F15.2,/))
  712 FORMAT(1X,'TOTAL',7F15.2,/,/)
  715 FORMAT(17(3X,I2,7F15.5,/))
  720 FORMAT(12X,'-----------------  CONVENTIONAL  ------------------', &
           '  -------------  UNCONVENTIONAL  --------------',/, &
           ' REGION',5X,'SHALLOW OIL',4X,'DEEP OIL', &
           4X,'SHALLOW GAS',7X,'DEEP GAS', &
           3X,'TIGHT SANDS',2X,'DEVONIAN SHALE',2X,'COALBED METHANE')
  721 FORMAT(12X,'-----------------  CONVENTIONAL  ------------------', &
           '    -------------  UNCONVENTIONAL  --------------',/, &
           ' REGION',5X,'SHALLOW OIL',4X,'DEEP OIL', &
           4X,'SHALLOW GAS',7X,'DEEP GAS', &
           5X,'TIGHT SANDS',2X,'DEVONIAN SHALE',2X,'COALBED METHANE', &
           2X,'TOTAL GAS')
  725 FORMAT(' REGION  GAS PRODUCTION')

!  -- PRICE OUTPUT FORMATS
  762 FORMAT(<AKRGN>(6X,I1,8X,F7.2,2X,F7.2,/))
  763 FORMAT(<L48RGN>(6X,I1,7F13.2,/))
  764 FORMAT(<OFFRGN>(6X,I1,8X,F7.2,2X,F7.2,/))
  765 FORMAT(2F13.2,/)
  766 FORMAT(3X,'REGION',5X,'CRUDE OIL',5X,'GAS')
  768 FORMAT(3X,'REGION',2X,'SHALLOW OIL',2X,'DEEP OIL',2X, &
           'SHALLOW GAS',5X, &
           'DEEP GAS',4X,'TIGHT SANDS',2X,'DEVONIAN SHALE',2X, &
           'COALBED METHANE')
  769 FORMAT(10X,'GAS')
!em4  769 FORMAT(6X,'CRUDE OIL',7X,'GAS')

!  ----  LOWER 48 FORMATS  ----
  801 FORMAT(<L48RGN>(4X,I1,1X,8F15.2,/))
  808 FORMAT(<L48RGN>(4X,I1,1X,8F16.0,/))
  805 FORMAT(1X,'TOTAL',8F15.2,/,/)
  806 FORMAT(1X,'TOTAL',1X,F13.5,/,/)
  807 FORMAT(1X,'TOTAL',8F16.0,/,/)
  800 FORMAT(<L48RGN>(4X,I1,7F17.2,/))
  804 FORMAT(<L48RGN>(4X,I1,<CONFUEL>F17.3,/))
  802 FORMAT(<L48RGN>(4X,I1,7F17.6,/))
  803 FORMAT(/,/)
  810 FORMAT(12X,'----------------  CONVENTIONAL  -----------------', &
           '  ---------------  UNCONVENTIONAL  ----------------',/, &
           ' REGION',7X,'SHALLOW OIL',4X,'DEEP OIL', &
           7X,'SHALLOW GAS',8X,'DEEP GAS', &
           7X,'TIGHT SANDS',4X,'DEVONIAN SHALE',4X,'COALBED METHANE')
  815 FORMAT(12X,'-------------  CONVENTIONAL  -------------', &
           '     -------------  UNCONVENTIONAL  -------------',/, &
           ' REGION',5X,'SHALLOW OIL',5X,'DEEP OIL', &
           7X,'SHALLOW GAS',7X,'DEEP GAS', &
           5X,'TIGHT SANDS',2X,'DEVONIAN SHALE',2X,'COALBED METHANE', &
           5X,'TOTAL GAS')

!  ----  OFFSHORE FORMATS  ----
  822 FORMAT(4X,I1,4X,F17.6,12X,F17.6)
  825 FORMAT(1X,'REGION',16X,'OIL',26X,'GAS')
  830 FORMAT(1X,'REGION',14X,'OIL',20X,'GAS')
  835 FORMAT(4X,I1,4X,F15.2,8X,F15.2,8X,F15.2,8X,F15.2)
  836 FORMAT(4(4X,I1,2X,F15.2,2X,F15.2,4X,F15.2,2X,F15.2,1X, &
             2(2X,F15.2),/))
  837 FORMAT(1X,'TOTAL',1X,F15.2,2X,F15.2,4X,F15.2,2X,F15.2,1X, &
             2(2X,F15.2))
  838 FORMAT(1X,'TOTAL',1X,F15.2,2X,F15.2,4X,F15.2,2X,F15.2)
  839 FORMAT(4(4X,I1,2X,F15.2,2X,F15.2,4X,F15.2,2X,F15.2,/))
  840 FORMAT(4X,I1,4X,F15.6,8X,F15.6,8X,F15.6,8X,F15.6)
  844 FORMAT(20X,'---EXPLORATORY---',18X,'---DEVELOPMENTAL---')
  845 FORMAT(22X,'---EXPLORATORY---',28X,'---DEVELOPMENTAL---')
  846 FORMAT(20X,'---EXPLORATORY---',18X,'---DEVELOPMENTAL---', &
       15X,'---EXP.         DEV.---')
  847 FORMAT(1X,'REGION',12X,'OIL',14X,'GAS',16X,'OIL',14X,'GAS', &
        15X,'OIL',14X,'GAS')
  848 FORMAT(1X,'REGION',12X,'OIL',14X,'GAS',16X,'OIL',14X,'GAS')
  850 FORMAT(21X,'---SUCCESFUL---',24X,'---DRY---')
  855 FORMAT(1X,'EXPLORATORY',6X,F15.2,2X,F15.2,2X,F15.2,2X,F15.2)
  860 FORMAT(1X,'DEVELOPMENTAL',4X,F15.2,2X,F15.2,2X,F15.2,2X,F15.2)
  865 FORMAT(1X,'TOTAL',12X,F15.2,2X,F15.2,2X,F15.2,2X,F15.2)
  870 FORMAT(1X,'REGION',21X,'OIL',14X,'GAS',14X,'DRY',12X,'TOTAL')
  875 FORMAT(98X,'TOTAL')
  880 FORMAT(<OFFRGN>(4X,I1,2X,F15.0,2X,F15.0,4X,F15.0,2X,F15.0,1X, &
             2(2X,F15.0),/))
  885 FORMAT(1X,'TOTAL',1X,F15.0,2X,F15.0,4X,F15.0,2X,F15.0,1X, &
             2(2X,F15.0))
      RETURN
      END

!  ************************************************************

      SUBROUTINE OGREP_DEMO
      IMPLICIT NONE

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables
      include'ogsml48'      ! lower 48 variables
      include'ogsmoff'      ! offshore variables
      include'ogsmak'       ! alaska variables
      include'ogsmlng'      ! lng variables
      include'ogsmout'

!  REPORTING VARIABLE DECLARATIONS
      REAL CRUDPROD(5,MNUMYR)
      REAL CRUDRSVS(5,MNUMYR)
      REAL CRUDRADD(5,MNUMYR)
      REAL CRUDPRIC(5,MNUMYR)
      REAL GASPRD(8,MNUMYR)
      REAL GASTOT(8,MNUMYR)
      REAL GASRSVS(8,MNUMYR)
      REAL GASRADD(8,MNUMYR)
      REAL GASPRIC(8,MNUMYR)
      REAL LNGMAX(MNUMYR)
      REAL LNGPRIC(MNUMYR)
      REAL PNGIMP(4,MNUMYR)
      REAL AVGCO(MNUMYR)
      REAL AVGNG(MNUMYR)
      REAL TOTADNG(5,MNUMYR)

      REAL TOTCRUD   ! USED TO COMPUTE WEIGHTED AVERAGE PRICE
      REAL TOTQGAS(7) ! USED TO COMPUTE WEIGHTED AVERAGE PRICE

      INTEGER YEAR(MNUMYR)

      DATA YEAR/1990,1991,1992,1993,1994,1995,1996,1997,1998,1999, &
                2000,2001,2002,2003,2004,2005,2006,2007,2008,2009, &
                2010,2011,2012,2013,2014,2015,2016,2017,2018,2019, &
                2020,2021,2022,2023,2024,2025,2026,2027,2028,2029, &
                2030,2031,2032,2033,2034,2035,2036,2037,2038,2039, &
                2040,2041,2042,2043,2044,2045,2046,2047,2048,2049, &
                2050/

!  ****  IF FLAGS ARE NOT SET THEN SKIP TO END OF SUBROUTINE  ****
!  WRITE OUTPUT TO DEBUG OUTPUT FILE (SYSOUT) IF FLAGS ARE SET
      IF (PRTDBGL.EQ.1 .AND. OGREPORT(1).GT.0) THEN

!  INITIALIZE ALL REPORTING VARIABLES TO ZERO
      DO R=1,5
         DO M=1,IJUMPYR
            CRUDPROD(R,M) = 0.0
            CRUDRSVS(R,M) = 0.0
            CRUDRADD(R,M) = 0.0
            CRUDPRIC(R,M) = 0.0
            TOTADNG(R,M) = 0.0
         ENDDO
      ENDDO

      DO R=1,8
         DO M=1,IJUMPYR
            GASPRD(R,M) = 0.0
            GASRSVS(R,M) = 0.0
            GASRADD(R,M) = 0.0
            GASPRIC(R,M) = 0.0
         ENDDO
      ENDDO

      DO R=1,4
         DO M=1,IJUMPYR
            PNGIMP(R,M) = 0.0
         ENDDO
      ENDDO

!  LOOP OVER EACH YEAR AND ASSIGN REPORTING VARIABLES
      DO M=1,LASTYR    ! ONLY LOOP OVER NUMBER OF YEARS RUN

!  ASSIGN LOWER 48 REPORTING VARIABLES
         DO R=1,L48RGN
            CRUDPROD(1,M) = CRUDPROD(1,M) + REPPRDL48(R,1,M) + REPPRDL48(R,2,M)
            CRUDRSVS(1,M) = CRUDRSVS(1,M) + REPRSVL48(R,1,M) + REPRSVL48(R,2,M)
            CRUDRADD(1,M) = CRUDRADD(1,M) + REPRADL48(R,1,M) + REPRADL48(R,2,M)
            DO K=3,7
               GASPRD(K-2,M) = GASPRD(K-2,M) + REPPRDL48(R,K,M)
               GASRSVS(K-2,M) = GASRSVS(K-2,M) + REPRSVL48(R,K,M)
               GASRADD(K-2,M) = GASRADD(K-2,M) + REPRADL48(R,K,M)
            ENDDO

            CRUDPROD(4,M) = CRUDPROD(4,M) + (OGQEORPR(R,M)/1000.0)
!           CRUDRADD(4,M) = CRUDRADD(4,M) + (OGQEORRA(R,M)/1000.0)
!           CRUDRSVS(4,M) = CRUDRSVS(4,M) + (OGQEORRS(R,M)/1000.0)
         ENDDO

!  ASSIGN AD GAS PRODUCTION REPORTING VARIABLE
      DO R = 1,L48RGN+OFFNEMSRGN
         IF (R.LE.L48RGN) THEN
           TOTADNG(1,M) = TOTADNG(1,M)+OGPRDAD(R,M)
         ELSE
           TOTADNG(2,M) = TOTADNG(2,M) +OGPRDAD(R,M)
         ENDIF
         TOTADNG(5,M) = TOTADNG(5,M)+OGPRDAD(R,M)
      ENDDO

!  ASSIGN OFFSHORE REPORTING VARIABLES
         DO R=1,OFFRGN
            CRUDPROD(2,M) = CRUDPROD(2,M) + REPPRDOFF(R,1,M)
            CRUDRSVS(2,M) = CRUDRSVS(2,M) + REPRSVOFF(R,1,M)
            CRUDRADD(2,M) = CRUDRADD(2,M) + REPRADOFF(R,1,M)
            GASPRD(6,M) = GASPRD(6,M) + REPPRDOFF(R,2,M)
            GASRSVS(6,M) = GASRSVS(6,M) + REPRSVOFF(R,2,M)
            GASRADD(6,M) = GASRADD(6,M) + REPRADOFF(R,2,M)
         ENDDO

!  ASSIGN ALASKA REPORTING VARIABLES
         DO R=1,AKRGN
            CRUDPROD(3,M) = CRUDPROD(3,M) + OGPRCOAK(R,M)
            CRUDRSVS(3,M) = CRUDRSVS(3,M) + BOYRESCOAK(R,M)
            CRUDRADD(3,M) = CRUDRADD(3,M) + RESADCOAK(R,M)
            GASPRD(7,M) = GASPRD(7,M) + OGPRNGAK(R,M)
            GASRSVS(7,M) = GASRSVS(7,M) + BOYRESNGAK(R,M)
            GASRADD(7,M) = GASRADD(7,M) + RESADNGAK(R,M)
         ENDDO

!  COMPUTE WEIGHTED AVERAGE TO AGGREGATE PRICES FOR LOWER 48
        TOTCRUD = 0.0      ! INITIALIZE TO ZERO
        DO K=3,7
           TOTQGAS(K) = 0.0    ! INITIALIZE TO ZERO
        ENDDO
        DO R=1,L48RGN
           TOTCRUD = TOTCRUD + REPPRDL48(R,1,M) + REPPRDL48(R,2,M)
           DO K=3,7
              TOTQGAS(K) = TOTQGAS(K) + REPPRDL48(R,K,M)
           ENDDO
        ENDDO

        DO R=1,L48RGN
           DO K=1,2
           CRUDPRIC(1,M) = CRUDPRIC(1,M) + (REPPRCL48(R,K,M)* &
                           (REPPRDL48(R,K,M)/TOTCRUD))
           ENDDO
           DO K=3,7
             IF (TOTQGAS(K).GT.0.0) THEN
               GASPRIC(K-2,M) = GASPRIC(K-2,M) + (REPPRCL48(R,K,M)* &
                                (REPPRDL48(R,K,M)/TOTQGAS(K)))
             ELSE
               GASPRIC(K-2,M) = 0.0
             ENDIF
           ENDDO
        ENDDO

!  COMPUTE WEIGHTED AVERAGE TO AGGREGATE PRICES FOR EOR
!       TOTCRUD = 0.0      ! INITIALIZE TO ZERO

        DO R=1,L48RGN
           TOTCRUD = TOTCRUD + OGQEORPR(R,M)
        ENDDO

!       DO R=1,L48RGN
!          IF (TOTCRUD .GT. 0.0) THEN
!             CRUDPRIC(4,M) = CRUDPRIC(4,M) + (OGQEORPR(R,M)* &
!                             (OGQEORPRC(R,M)/TOTCRUD))
!          END IF
!       ENDDO


!  COMPUTE WEIGHTED AVERAGE TO AGGREGATE PRICES FOR OFFSHORE
        TOTCRUD = 0.0      ! SET TO ZERO
        TOTQGAS(1) = 0.0    ! SET TO ZERO (ONLY USE FIRST ELEMENT)
        DO R=1,OFFRGN
           TOTCRUD = TOTCRUD + REPPRDOFF(R,1,M)
           TOTQGAS(1) = TOTQGAS(1) + REPPRDOFF(R,2,M)
        ENDDO

        DO R=1,OFFRGN
           CRUDPRIC(2,M) = CRUDPRIC(2,M) + (REPPRCOFF(R,1,M)* &
                           (REPPRDOFF(R,1,M)/TOTCRUD))
           GASPRIC(6,M) = GASPRIC(6,M) + (REPPRCOFF(R,2,M)* &
                          (REPPRDOFF(R,2,M)/TOTQGAS(1)))
        ENDDO

!  COMPUTE WEIGHTED AVERAGE TO AGGREGATE PRICES FOR ALASKA
        TOTCRUD = 0.0      ! SET TO ZERO
        TOTQGAS(1) = 0.0    ! SET TO ZERO  (ONLY USE FIRST ELEMENT)
        DO R=1,AKRGN
           TOTCRUD = TOTCRUD + OGPRCOAK(R,M)
           TOTQGAS(1) = TOTQGAS(1) + OGPRNGAK(R,M)
        ENDDO

        DO R=1,AKRGN
           CRUDPRIC(3,M) = CRUDPRIC(3,M) + (REPPRCAK(R,1,M)* &
                           (OGPRCOAK(R,M)/TOTCRUD))
           GASPRIC(7,M) = GASPRIC(7,M) + (REPPRCAK(R,2,M)* &
                            (OGPRNGAK(R,M)/TOTQGAS(1)))
        ENDDO

!  COMPUTE YEARLY TOTALS
        DO R=1,4
           CRUDPROD(5,M) = CRUDPROD(5,M) + CRUDPROD(R,M)
           CRUDRSVS(5,M) = CRUDRSVS(5,M) + CRUDRSVS(R,M)
           CRUDRADD(5,M) = CRUDRADD(5,M) + CRUDRADD(R,M)
        ENDDO

!  -- GAS IS N.A. ONLY
        DO R=1,7
           GASPRD(8,M) = GASPRD(8,M) + GASPRD(R,M)
           GASRSVS(8,M) = GASRSVS(8,M) + GASRSVS(R,M)
           GASRADD(8,M) = GASRADD(8,M) + GASRADD(R,M)
        ENDDO

!  08/12/94 CREATE NATURAL GAS PRODUCTION TABLE WITH THE
!  SUM OF AD AND NA GAS
!
!  STORE AD FIGURES IN  SHALLOW (#1) AND OFFSHORE (#6)

           GASTOT(1,M) = GASTOT(1,M) + TOTADNG(1,M)
           GASTOT(6,M) = GASTOT(6,M) + TOTADNG(2,M)

        DO R=1,7

!  ADD NA TOTALS TO GASTOT ARRAY --
           GASTOT(R,M) = GASTOT(R,M) + GASPRD(R,M)

!  ACCUMULATE GAS TOTALS IN GASTOT ARRAY (#8)
           GASTOT(8,M) = GASTOT(8,M) + GASTOT(R,M)
        ENDDO


!  COMPUTE AVERAGE PRICES FOR LOWER48,OFFSHORE
        AVGCO(M) = (CRUDPROD(1,M) * CRUDPRIC(1,M) + &
                    CRUDPROD(2,M) * CRUDPRIC(2,M) + &
                    CRUDPROD(4,M) * CRUDPRIC(4,M))/ &
           (CRUDPROD(1,M)+CRUDPROD(2,M)+CRUDPROD(4,M))

        AVGNG(M) = (GASPRD(1,M)*GASPRIC(1,M) + &
                    GASPRD(2,M)*GASPRIC(2,M) + &
                    GASPRD(3,M)*GASPRIC(3,M) + &
                    GASPRD(4,M)*GASPRIC(4,M) + &
                    GASPRD(5,M)*GASPRIC(5,M) + &
                    GASPRD(6,M)*GASPRIC(6,M)) / &
                   (GASPRD(8,M)-GASPRD(7,M))

! ASSIGN DRILLING STATISTICS REPORT VARIABLES (JPD 7/5/94)

        DO I = 1,L48WELL
          DRILLTOTL48(I,8,M) = 0.0
          DO K = 3,L48FUEL
            DRILLTOTL48(I,8,M) = DRILLTOTL48(I,8,M) + &
                                 DRILLTOTL48(I,K,M)
          ENDDO
        ENDDO

        DO I = 1,L48WELL
            DRILLTOTL48(I,9,M) = DRILLTOTL48(I,1,M) + &
                                 DRILLTOTL48(I,2,M) + &
                                 DRILLTOTL48(I,8,M)
        ENDDO

        DO I = 1,OFFWELL
          DRILLTOTOFF(I,4,M) = 0.0
          DO K = 1,(OFFFUEL+1)
            DRILLTOTOFF(I,4,M) = DRILLTOTOFF(I,4,M) + &
                                 DRILLTOTOFF(I,K,M)
          ENDDO
        ENDDO

      ENDDO  ! THIS ENDDO ENDS LARGE CALCULATION LOOP OVER LASTYR

!  WRITE OUTPUT TO DEBUG OUTPUT FILE (SYSOUT) IF FLAGS ARE SET
      WRITE(SYSOUT,*) '  ===================================='
      WRITE(SYSOUT,*) '  ===    OGSM   SUMMARY  REPORT    ==='
      WRITE(SYSOUT,*) '  ===================================='
      WRITE(SYSOUT,620)
      WRITE(SYSOUT,660)
      WRITE(SYSOUT,680) (YEAR(M),(DRILLTOTL48(1,R,M),R=1,L48RGN), &
                        M=1,LASTYR)

      WRITE(SYSOUT,630)
      WRITE(SYSOUT,660)
      WRITE(SYSOUT,680) (YEAR(M),(DRILLTOTL48(2,R,M),R=1,L48RGN), &
                        M=1,LASTYR)

      WRITE(SYSOUT,640)
      WRITE(SYSOUT,670)
      WRITE(SYSOUT,690) (YEAR(M),(DRILLTOTOFF(1,R,M),R=1,MNL48F+1), &
                        M=1,LASTYR)

      WRITE(SYSOUT,650)
      WRITE(SYSOUT,670)
      WRITE(SYSOUT,690) (YEAR(M),(DRILLTOTOFF(2,R,M),R=1,MNL48F+1), &
                        M=1,LASTYR)

      WRITE(SYSOUT,700)
      WRITE(SYSOUT,760)
      WRITE(SYSOUT,780) (YEAR(M),(CRUDPROD(R,M),R=1,5),M=1,LASTYR)
      WRITE(SYSOUT,701)
      WRITE(SYSOUT,760)
      WRITE(SYSOUT,780) (YEAR(M),(CRUDPROD(R,M)/.365,R=1,5),M=1,LASTYR)

      WRITE(SYSOUT,705)
      WRITE(SYSOUT,760)
      WRITE(SYSOUT,780) (YEAR(M),(CRUDRSVS(R,M),R=1,5),M=1,LASTYR)

      WRITE(SYSOUT,710)
      WRITE(SYSOUT,760)
      WRITE(SYSOUT,780) (YEAR(M),(CRUDRADD(R,M),R=1,5),M=1,LASTYR)

      WRITE(SYSOUT,720)
!     WRITE(SYSOUT,765)
      WRITE(SYSOUT,760)
      WRITE(SYSOUT,785) (YEAR(M),(CRUDPRIC(R,M)*1.211,R=1,4), &
               AVGCO(M)*1.211,M=1,LASTYR)

      WRITE(SYSOUT,725)
      WRITE(SYSOUT,760)
      WRITE(SYSOUT,780) (YEAR(M),(TOTADNG(R,M),R=1,5),M=1,LASTYR)

      WRITE(SYSOUT,730)
      WRITE(SYSOUT,770)
      WRITE(SYSOUT,790) (YEAR(M),(GASPRD(R,M),R=1,8),M=1,LASTYR)

      WRITE(SYSOUT,732)
      WRITE(SYSOUT,770)
      WRITE(SYSOUT,790) (YEAR(M),(GASTOT(R,M),R=1,8),M=1,LASTYR)

      WRITE(SYSOUT,735)
      WRITE(SYSOUT,770)
      WRITE(SYSOUT,790) (YEAR(M),(GASRSVS(R,M),R=1,8),M=1,LASTYR)

      WRITE(SYSOUT,740)
      WRITE(SYSOUT,770)
      WRITE(SYSOUT,790) (YEAR(M),(GASRADD(R,M),R=1,8),M=1,LASTYR)

!
!-WT1  CONVERT GAS PRICES TO 1992 $
      WRITE(SYSOUT,750)
!     WRITE(SYSOUT,775)
      WRITE(SYSOUT,770)
      WRITE(SYSOUT,795) (YEAR(M),(GASPRIC(R,M)*1.211,R=1,7), &
                        AVGNG(M)*1.211,M=1,LASTYR)

      ENDIF   ! END IF DEBUG OUTPUT FLAGS SET

!  REPORT TITLES
  620 FORMAT(2X,66('-'),/,2X,'---',8X,'SUCCESSFUL ONSHORE ', &
             'EXPLORATORY WELLS DRILLED',8X,'---',/,2X,66('-'))
  630 FORMAT(2X,66('-'),/,2X,'---',8X,'SUCCESSFUL ONSHORE ', &
             'DEVELOPMENTAL WELLS DRILLED',6X,'---',/,2X,66('-'))
  640 FORMAT(2X,55('-'),/,2X,'---',8X,'OFFSHORE EXPLORATORY WELLS ', &
             'DRILLED',7X,'---',/,2X,55('-'))
  650 FORMAT(2X,55('-'),/,2X,'---',8X,'OFFSHORE DEVELOPMENTAL ', &
             'WELLS DRILLED',5X,'---',/,2X,55('-'))
  700 FORMAT(2X,50('-'),/,2X,'---',9X,'CRUDE OIL PRODUCTION - MMB', &
             9X,'---',/,2X,50('-'))
  701 FORMAT(2X,50('-'),/,2X,'---',9X,'CRUDE OIL PRODUCTION - MB/D', &
            8X,'---',/,2X,50('-'))
  705 FORMAT(2X,50('-'),/,2X,'---',13X,'CRUDE OIL RESERVES',13X, &
             '---',/,2X,50('-'))
  710 FORMAT(2X,50('-'),/,2X,'---',8X,'CRUDE OIL RESERVE ADDITIONS', &
             9X,'---',/,2X,50('-'))
  720 FORMAT(2X,50('-'),/,2X,'---',9X,'CRUDE OIL PRICES (1992 $)', &
             10X,'---',/,2X,50('-'))
  725 FORMAT(2X,50('-'),/,2X,'---',13X,'AD GAS PRODUCTION', &
             14X,'---',/,2X,50('-'))
  730 FORMAT(2X,50('-'),/,2X,'---',8X,'NA NATURAL GAS PRODUCTION', &
             11X,'---',/,2X,50('-'))
  732 FORMAT(2X,50('-'),/,2X,'---',5X, &
             'TOTAL NATURAL GAS PRODUCTION (AD+NA)',3X, &
             '---',/,2X,50('-'))
  735 FORMAT(2X,50('-'),/,2X,'---',12X,'NATURAL GAS RESERVES',12X, &
             '---',/,2X,50('-'))
  740 FORMAT(2X,50('-'),/,2X,'---',7X,'NATURAL GAS RESERVE ', &
             'ADDITIONS',8X,'---',/,2X,50('-'))
  750 FORMAT(2X,50('-'),/,2X,'---',9X,'NATURAL GAS PRICES (92$)',11X, &
             '---',/,2X,50('-'))

!  REPORT HEADINGS
  660 FORMAT(11X,'SHALLOW       DEEP    SHALLOW       DEEP      ', &
             'TIGHT   DEVONIAN    ', &
             'COALBED      TOTAL      TOTAL      ',/,3X, &
             'YEAR        OIL        OIL        GAS        GAS', &
             6X,'SANDS', &
             6X,'SHALE    METHANE       GAS  SUCCESSFUL')
  670 FORMAT(3X,'YEAR',8X,'OIL',8X,'GAS',8X, &
             'DRY',6X,'TOTAL')
  760 FORMAT(2X,'YEAR',4X,'LOWER 48',3X,'OFFSHORE',5X, &
             'ALASKA',6X,'EOR',4X,'TOTAL')
  765 FORMAT(2X,'YEAR',4X,'LOWER 48',3X,'OFFSHORE',5X, &
             'ALASKA',6X,'EOR')

  770 FORMAT(12X,'SHALLOW      DEEP      TIGHT    DEVONIAN    ', &
             'COALBED',/,2X,'YEAR        GAS        GAS',7X,'SANDS', &
             5X,'SHALE      METHANE   OFFSHORE    ALASKA',5X, &
             'TOTAL')
  775 FORMAT(12X,'SHALLOW      DEEP      TIGHT    DEVONIAN    ', &
             'COALBED',/,2X,'YEAR        GAS        GAS',7X,'SANDS', &
             5X,'SHALE      METHANE   OFFSHORE    ALASKA')
  850 FORMAT(9X,'PRODUCTIVE',33X,'P/R',17X,'RESERVE',18X,'WELLHEAD',/, &
           2X,'YEAR',5X,'TARGET',5X, &
           'PRODUCTION',2X,'CONSUMPTION',5X,'RATIO',4X,'RESERVES', &
           4X,'ADDITIONS',5X,'WELLS',8X,'PRICE')

!  REPORT DATA FORMAT
  680 FORMAT(31(3X,I4,9F11.2,/))  ! WELLS DRILLED ONSHORE FORMAT
  690 FORMAT(31(3X,I4,4F11.2,/))  ! WELLS DRILLED OFFSHORE FORMAT
  780 FORMAT(31(3X,I4,5F11.2,/))  ! CRUDE OIL FORMAT
  785 FORMAT(31(3X,I4,5F11.2,/))  ! CRUDE OIL FORMAT
  790 FORMAT(31(3X,I4,8F11.2,/))  ! GAS FORMAT
  795 FORMAT(31(3X,I4,8F11.2,/))  ! GAS FORMAT
  890 FORMAT(31(3X,I4,8F12.2,/))

      RETURN
      END

!  ********************************************************

      SUBROUTINE OGTAB_WRITE
      IMPLICIT NONE

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables
      include'ogsml48'      ! lower 48 variables
      include'ogsmoff'      ! offshore variables
      include'macout'

!  THIS SUBROUTINE WRITES OUT THE CURRENT YEAR RESULTS

!  ONLY OUTPUT RESULTS IF PRINT DEBUG FLAG IS SET
      IF (PRTDBGL.EQ.1) THEN

        write(unout1) ogprcl48
        write(unout1) dcfl48
        write(unout1) spendirk_l48
        write(unout1) nfwl48
        write(unout1) oexpl48
        write(unout1) devl48
        write(unout1) sucwelll48
        write(unout1) drywelll48
        write(unout1) drilll48
        write(unout1) dryl48
        write(unout1) operl48
        write(unout1) leasl48
        write(unout1) delta1l48
        write(unout1) delta2l48
        write(unout1) delta3l48
        write(unout1) fr1l48
        write(unout1) fr2l48
        write(unout1) fr3l48
        write(unout1) nrdl48
        write(unout1) revl48
        write(unout1) extl48
        write(unout1) ndirl48
        write(unout1) resadl48
        write(unout1) cumr1l48
        write(unout1) cumr2l48
        write(unout1) cumr3l48
        write(unout1) prratl48
        write(unout1) resboyl48
        write(unout1) prdl48
        write(unout1) exprdl48
        write(unout1) ogprcoff
        write(unout1) elastoff
        write(unout1) dcfoff
        write(unout1) spendirk_off
        write(unout1) drilloff
        write(unout1) nfwoff
        write(unout1) devoff
        write(unout1) oexpoff
        write(unout1) delta1off
        write(unout1) delta2off
        write(unout1) delta3off
        write(unout1) fr1off
        write(unout1) fr2off
        write(unout1) fr3off
        write(unout1) nrdoff
        write(unout1) revoff
        write(unout1) extoff
        write(unout1) ndiroff
        write(unout1) resadoff
        write(unout1) cumr1off
        write(unout1) cumr2off
        write(unout1) cumr3off
        write(unout1) prratoff
        write(unout1) resboyoff
        write(unout1) exprdoff
        write(unout1) ogprodoff
        write(unout1) dryoff
        write(unout1) operoff
        write(unout1) leasoff
        write(unout1) r_urrl48
        write(unout1) r_infrsvl48
        write(unout1) r_unresoff
        write(unout1) r_infrsvoff
        write(unout1) estwellsl48
        write(unout1) rigsl48
        write(unout1) curwellsoff
        write(unout1) rigsoff
        write(unout1) sucwelloff
        write(unout1) drywelloff
        write(unout1) mc_jpgdp

!  CALL FILE MANAGER ROUTINE TO CLOSE FILE IN LAST MODEL YEAR
         IF (CURIYR.EQ.LASTYR) THEN
            FNAME='WLUNOUT'
            UNOUT1 = FILE_MGR('C',FNAME,NEW)
         ENDIF

      ENDIF   ! END IF PRTDBGL = 1

      RETURN
      END

!  ********************************************************

      SUBROUTINE OGTAB_GEN
      IMPLICIT NONE

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables
      include'ogsml48'      ! lower 48 variables
      include'ogsmoff'      ! offshore variables

      CHARACTER*60 TITLE
      INTEGER MAXSUC
      INTEGER PNTFLG
      INTEGER HDRFLG
      PARAMETER (MAXSUC=3)

!  VARIBLES TO HOLD ALL YEARS OF THE OGSM RUN DATA BEING INPUT
      REAL XSUCWELLL48(MNUMYR,L48WELL,L48RGN,L48FUEL)
      REAL XDRYWELLL48(MNUMYR,L48WELL,L48RGN,L48FUEL)
      REAL XSUCWELLOFF(MNUMYR,OFFWELL,OFFRGN,OFFFUEL)
      REAL XDRYWELLOFF(MNUMYR,OFFWELL,OFFRGN,OFFFUEL)
      REAL XRESBOYL48(MNUMYR,L48WELL,L48RGN,L48FUEL)
      REAL XRESTMPL48(MNUMYR+1,L48WELL+1,L48RGN+1,L48FUEL+2)
      REAL XRESBOYOFF(MNUMYR,OFFWELL,OFFRGN,OFFFUEL)
      REAL XRESTMPOFF(MNUMYR+1,OFFWELL+1,OFFRGN+1,OFFFUEL+2)
      REAL XPRDL48(MNUMYR,L48WELL,L48RGN,L48FUEL)
      REAL XPRDTMPL48(MNUMYR+1,L48WELL+1,L48RGN+1,L48FUEL+2)
      REAL XPRDOFF(MNUMYR,OFFWELL,OFFRGN,OFFFUEL)
      REAL XPRDTMPOFF(MNUMYR+1,OFFWELL+1,OFFRGN+1,OFFFUEL+2)

!  TEMP VARIABLES USED TO READ THE DATA FROM THE FILE
      REAL TEMPL48(L48WELL,L48RGN,L48FUEL)
      REAL TEMPOFF(OFFWELL,OFFRGN,OFFFUEL)
      REAL TEMPRESL48(L48RGN,L48FUEL)
      REAL TEMPRESOFF(OFFRGN,OFFFUEL)
      REAL TEMPRDL48(L48RGN,L48FUEL)
      REAL TEMPRDOFF(OFFRGN,OFFFUEL)

!  INITIALIZE TEMPORARY ARRAYS TO ZERO
      DO T = 1,IJUMPYR+1
      DO I = 1,L48WELL+1
         DO R = 1,L48RGN+1
         DO K = 1,L48FUEL+2
            XRESTMPL48(T,I,R,K) = 0.0
            XPRDTMPL48(T,I,R,K) = 0.0
         ENDDO
         ENDDO
      ENDDO
      ENDDO

      DO T = 1,IJUMPYR+1
      DO I = 1,OFFWELL+1
         DO R = 1,OFFRGN+1
         DO K = 1,OFFFUEL+2
            XRESTMPOFF(T,I,R,K) = 0.0
            XPRDTMPOFF(T,I,R,K) = 0.0
         ENDDO
         ENDDO
      ENDDO
      ENDDO

!  ONLY OUTPUT RESULTS IF PRINT DEBUG FLAG IS SET
      IF (PRTDBGL.EQ.1) THEN
!  CALL FILE MANAGER ROUTINE TO OPEN OGSM DUMP FILE AS INPUT
         NEW=.FALSE.
         FNAME='WLTBOUT'
         OFILE1 = FILE_MGR('O',FNAME,NEW)

!  LOOP OVER THE NUMBER OF MODEL YEARS
      DO T=1,LASTYR
         READ(OFILE1) TEMPL48
         CALL OGCOPY_34(TEMPL48,XSUCWELLL48,T,L48WELL,L48RGN,L48FUEL, &
                        IJUMPYR)
         READ(OFILE1) TEMPL48
         CALL OGCOPY_34(TEMPL48,XDRYWELLL48,T,L48WELL,L48RGN,L48FUEL, &
                        IJUMPYR)
         READ(OFILE1) TEMPRESL48
         CALL OGCOPY_24(TEMPRESL48,XRESBOYL48,T,L48WELL,L48RGN, &
                        L48FUEL,IJUMPYR)
         READ(OFILE1) TEMPRDL48
         CALL OGCOPY_24(TEMPRDL48,XPRDL48,T,L48WELL,L48RGN, &
                        L48FUEL,IJUMPYR)
         READ(OFILE1) TEMPOFF
         CALL OGCOPY_34(TEMPOFF,XSUCWELLOFF,T,OFFWELL,OFFRGN,OFFFUEL, &
                        IJUMPYR)
         READ(OFILE1) TEMPOFF
         CALL OGCOPY_34(TEMPOFF,XDRYWELLOFF,T,OFFWELL,OFFRGN,OFFFUEL, &
                        IJUMPYR)
         READ(OFILE1) TEMPRESOFF
         CALL OGCOPY_24(TEMPRESOFF,XRESBOYOFF,T,OFFWELL,OFFRGN, &
                        OFFFUEL,IJUMPYR)
         READ(OFILE1) TEMPRDOFF
         CALL OGCOPY_24(TEMPRDOFF,XPRDOFF,T,OFFWELL,OFFRGN, &
                        OFFFUEL,IJUMPYR)
      ENDDO

!  CALL FILE MANAGER ROUTINE TO CLOSE OGSM DUMP FILE
      FNAME='WLTBOUT'
      OFILE1 = FILE_MGR('C',FNAME,NEW)

!  CALL FILE MANAGER ROUTINE TO OPEN OGSM TABLE OUTPUT FILE
      NEW=.TRUE.
      FNAME='WLTBOUT'
      OFILE1 = FILE_MGR('O',FNAME,NEW)

      TITLE = 'L O W E R   4 8   O N S H O R E   W E L L S'
!-WT1      CALL OGTAB_GENL48(XSUCWELLL48,XDRYWELLL48,TITLE,0,1)

      TITLE = 'L O W E R   4 8   O F F S H O R E  W E L L S'
!-WT1      CALL OGTAB_GENOFF(XSUCWELLOFF,XDRYWELLOFF,TITLE,0,1)

      TITLE = 'L O W E R   4 8   O N S H O R E   R E S E R V E S'
!-WT1      CALL OGTAB_GENL48(XRESBOYL48,XRESTMPL48,TITLE,1,2)

      TITLE = 'L O W E R   4 8   O F F S H O R E  R E S E R V E S'
!-WT1      CALL OGTAB_GENOFF(XRESBOYOFF,XRESTMPOFF,TITLE,1,2)

      TITLE = 'L O W E R   4 8   O N S H O R E   P R O D U C T , &
              I O N'
!-WT1      CALL OGTAB_GENL48(XPRDL48,XPRDTMPL48,TITLE,1,3)

      TITLE = 'L O W E R   4 8   O F F S H O R E   P R O D U C , &
              T I O N'
!-WT1      CALL OGTAB_GENOFF(XPRDOFF,XPRDTMPOFF,TITLE,1,3)


!  CALL FILE MANAGER ROUTINE TO CLOSE OGSM TABLE OUTPUT FILE
      FNAME='WLTBOUT'
      OFILE1 = FILE_MGR('C',FNAME,NEW)

      ENDIF   ! END IF PRTDBGL = 1


      RETURN
      END

!  *************************************************************

      SUBROUTINE OGTAB_GENL48(L48SUC,L48DRY,TITLE,PNTFLG,HDRFLG)
      IMPLICIT NONE

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables

      CHARACTER*60 TITLE
      INTEGER MAXSUC
      INTEGER PNTFLG
      INTEGER HDRFLG
      PARAMETER (MAXSUC=3)

!  VARIBLES TO HOLD ALL YEARS OF THE OGSM RUN DATA BEING INPUT
      REAL L48SUC(MNUMYR,L48WELL,L48RGN,L48FUEL)
      REAL L48DRY(MNUMYR+1,L48WELL+1,L48RGN+1,L48FUEL+2)

!  DECLARE THE REPORTING VARIABLES
      REAL REPL48(MNUMYR+1,MAXSUC,L48WELL+1,L48RGN+1,L48FUEL+2)

      CHARACTER SUCNAME(MAXSUC)*12,YRNAME(MNUMYR+1)*5, &
                RGNNAML48(L48RGN+1)*18,FUELNAML48(L48FUEL+2)*13, &
                WELLNAME(L48WELL+1)*14,HEADERS(3)*35

      DATA SUCNAME/'SUCCESSFUL','DRY','TOTAL'/
      DATA YRNAME/'1990','1991','1992','1993','1994','1995','1996', &
                  '1997','1998','1999','2000','2001','2002','2003', &
                  '2004','2005','2006','2007','2008','2009','2010', &
                  '2011','2012','2013','2014','2015','2016','2017', &
                  '2018','2019','2020','2021','2022','2023','2024', &
                  '2025','2026','2027','2028','2029','2030', &
                  '2031','2032','2033','2034','2035', &
                  '2036','2037','2038','2039','2040', &
                  '2041','2042','2043','2044','2045', &
                  '2046','2047','2048','2049','2050','TOTAL'/
      DATA RGNNAML48/'1. NORTHEAST','2. GULF COAST', &
                     '3. MIDCONTINENT','4. SOUTHWEST', &
                     '5. ROCKY MOUNTAIN', &
                     '6. WEST COAST', &
                     '7. NORTHERN GREAT PLAINS', &
                     'LOWER 48 ONSHORE'/
      DATA FUELNAML48/'SHALL OIL','DEEP OIL','SHALL GAS','DEEP GAS', &
                    'TIGHT SANDS','DEV SHALE','COALBED METH', &
                    'TOTAL GAS','TOTAL'/
      DATA WELLNAME/'EXPLORATORY','DEVELOPMENTAL','EXPLOR+DEVELOP'/
      DATA HEADERS/'LOWER 48 ONSHORE WELLS', &
                   'LOWER 48 ONSHORE RESERVES', &
                   'LOWER 48 ONSHORE PRODUCTION'/

!  INITIALIZE REPORTING ARRAYS TO ZERO
      DO T = 1,IJUMPYR+1
      DO M = 1,MAXSUC
      DO I = 1,L48WELL+1
         DO R = 1,L48RGN+1
         DO K = 1,L48FUEL+2
            REPL48(T,M,I,R,K) = 0.0
         ENDDO
         ENDDO
      ENDDO
      ENDDO
      ENDDO

!  --------------------------------------------  C
!  ----      COMPUTE REPORTING TOTALS      ----  C
!  --------------------------------------------  C


!  *** COMPUTE LOWER 48 ONSHORE VALUES
      DO R = 1,L48RGN+1
      DO I = 1,L48WELL+1
      DO M = 1,MAXSUC
      DO T = 1,LASTYR
      DO K = 1,L48FUEL

         IF (I.LE.L48WELL.AND.R.LE.L48RGN) THEN
!              WRITE(SYSOUT,*) 'L48SUC',L48SUC(T,I,R,K)
!              WRITE(SYSOUT,*) 'L48DRY',L48DRY(T,I,R,K)
            IF (M.EQ.1) THEN
               REPL48(T,M,I,R,K) = L48SUC(T,I,R,K)
            ELSE IF (M.EQ.2) THEN
               REPL48(T,M,I,R,K) = L48DRY(T,I,R,K)
            ELSE
               REPL48(T,M,I,R,K) = L48SUC(T,I,R,K) + &
                   L48DRY(T,I,R,K)
            ENDIF
         ELSE
            IF (R.GT.L48RGN) THEN
               REPL48(T,M,I,R,K) = REPL48(T,M,I,1,K) + &
                   REPL48(T,M,I,2,K) + REPL48(T,M,I,3,K) + &
                   REPL48(T,M,I,4,K) + REPL48(T,M,I,5,K) + &
                   REPL48(T,M,I,6,K) + REPL48(T,M,I,7,K)
            ELSE  ! NON TOTAL REGION - TOTAL EXPL + DEVL
               REPL48(T,M,I,R,K) = REPL48(T,M,1,R,K) + &
                   REPL48(T,M,2,R,K)
            ENDIF
         ENDIF

!  COMPUTE TOTAL FOR ALL FUEL TYPES FOR A GIVEN T,M,I,R
         REPL48(T,M,I,R,L48FUEL+2) = REPL48(T,M,I,R,L48FUEL+2) + &
                  REPL48(T,M,I,R,K)

!  COMPUTE TOTAL GAS FOR A GIVEN T,M,I,R
         IF (K.GE.3) THEN
            REPL48(T,M,I,R,L48FUEL+1) = REPL48(T,M,I,R,L48FUEL+1) + &
                  REPL48(T,M,I,R,K)
         ENDIF

!  COMPUTE TOTAL FOR ALL YEARS FOR GIVEN M,I,R,K
         REPL48(LASTYR+1,M,I,R,K) = REPL48(LASTYR+1,M,I,R,K) + &
                  REPL48(T,M,I,R,K)

      ENDDO   ! ******  END FUEL (K) LOOP  ******

!  COMPUTE OVERALL TOTALS FOR A GIVEN M,I,R
        REPL48(LASTYR+1,M,I,R,L48FUEL+1) = &
           REPL48(LASTYR+1,M,I,R,L48FUEL+1)+REPL48(T,M,I,R,L48FUEL+1)
        REPL48(LASTYR+1,M,I,R,L48FUEL+2) = &
           REPL48(LASTYR+1,M,I,R,L48FUEL+2)+REPL48(T,M,I,R,L48FUEL+2)

      ENDDO   ! ******  END YEAR (T) LOOP  ******
      ENDDO   ! ******  END SUC (M) LOOP  ******
      ENDDO   ! ******  END WELL (I) LOOP  ******
      ENDDO   ! ******  END REGION (R) LOOP  ******

!  --------------------------------------------  C
!  ----      WRITE THE OUTPUT TABLES       ----  C
!  --------------------------------------------  C

!  RESET YRNAME VARIABLE TO CORRESPOND TO LASTYR
      YRNAME(LASTYR+1) = 'TOTAL'

!  *** WRITE LOWER 48 ONSHORE TABLES
      WRITE(OFILE1,90) TITLE

      IF (PNTFLG.EQ.1) THEN
        I=1  !DON'T LOOP OVER EXP/DEV/TOTAL
        M=1  !DON'T LOOP OVER SUC/DRY/TOTAL
        DO R = 1,L48RGN+1
           WRITE(OFILE1,115) HEADERS(HDRFLG)
           WRITE(OFILE1,100) RGNNAML48(R)
           WRITE(OFILE1,120) (FUELNAML48(K),K=1,L48FUEL+2)
        DO T = 1,LASTYR+1
           WRITE(OFILE1,130) YRNAME(T),(REPL48(T,M,I,R,K),K=1,L48FUEL+2)
        ENDDO   ! ******  END YEAR (T) LOOP  ******
           WRITE(OFILE1,140)
        ENDDO   ! ******  END REGION (R) LOOP  ******
      ELSE
        DO R = 1,L48RGN+1
        DO I = 1,L48WELL+1
        DO M = 1,MAXSUC
           WRITE(OFILE1,115) HEADERS(HDRFLG)
           WRITE(OFILE1,100) RGNNAML48(R)
           WRITE(OFILE1,110) WELLNAME(I),SUCNAME(M)
           WRITE(OFILE1,120) (FUELNAML48(K),K=1,L48FUEL+2)
        DO T = 1,LASTYR+1
           WRITE(OFILE1,130) YRNAME(T),(REPL48(T,M,I,R,K),K=1,L48FUEL+2)
        ENDDO   ! ******  END YEAR (T) LOOP  ******
           WRITE(OFILE1,140)
        ENDDO   ! ******  END SUC (M) LOOP  ******
        ENDDO   ! ******  END WELL (I) LOOP  ******
        ENDDO   ! ******  END REGION (R) LOOP  ******
      ENDIF

90    FORMAT (1X,120('-'),/,1X,30X,A50,/,1X,120('-'))
100   FORMAT (1X,'REGION:  ',A18)
110   FORMAT (1X,2A14)
115   FORMAT (1X,120('-'),/,1X,A35)
120   FORMAT (1X,'YEAR',6X,8A14)
130   FORMAT (1X,A5,8F14.1)
140   FORMAT (1X)

      RETURN
      END

!  *************************************************************

      SUBROUTINE OGTAB_GENOFF(OFFSUC,OFFDRY,TITLE,PNTFLG,HDRFLG)
      IMPLICIT NONE

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables

      CHARACTER*60 TITLE
      INTEGER MAXSUC
      INTEGER PNTFLG
      INTEGER HDRFLG
      PARAMETER (MAXSUC=3)

!  VARIBLES TO HOLD ALL YEARS OF THE OGSM RUN DATA BEING INPUT
      REAL OFFSUC(MNUMYR,OFFWELL,OFFRGN,OFFFUEL)
      REAL OFFDRY(MNUMYR+1,OFFWELL+1,OFFRGN+1,OFFFUEL+1)

!  DECLARE THE REPORTING VARIABLES
      REAL REPOFF(MNUMYR+1,MAXSUC,OFFWELL+1,OFFRGN+1,OFFFUEL+1)

      CHARACTER SUCNAME(MAXSUC)*12,YRNAME(MNUMYR+1)*5, &
                RGNNAMOFF(OFFRGN+1)*23,FUELNAMOFF(OFFFUEL+1)*13, &
                WELLNAME(OFFWELL+1)*14,HEADERS(3)*35

      DATA SUCNAME/'SUCCESSFUL','DRY','TOTAL'/
      DATA YRNAME/'1990','1991','1992','1993','1994','1995','1996', &
                  '1997','1998','1999','2000','2001','2002','2003', &
                  '2004','2005','2006','2007','2008','2009','2010', &
                  '2011','2012','2013','2014','2015','2016','2017', &
                  '2018','2019','2020','2021','2022','2023','2024', &
                  '2025','2026','2027','2028','2029','2030', &
                  '2031','2032','2033','2034','2035', &
                  '2036','2037','2038','2039','2040', &
                  '2041','2042','2043','2044','2045', &
                  '2046','2047','2048','2049','2050','TOTAL'/
      DATA WELLNAME/'EXPLORATORY','DEVELOPMENTAL', &
                    'EXPLOR+DEVELOP'/
      DATA RGNNAMOFF/'1. ATLANTIC','2. PACIFIC', &
                     '3. SHALLOW GULF','4. DEEP GULF', '5. EASTERN GULF', &
                     'LOWER 48 OFFSHORE'/
      DATA FUELNAMOFF/'CRUDE OIL','NATURAL GAS','TOTAL'/
      DATA HEADERS/'LOWER 48 OFFSHORE WELLS', &
                   'LOWER 48 OFFSHORE RESERVES', &
                   'LOWER 48 OFFSHORE PRODUCTION'/

!  INITIALIZE REPORTING ARRAYS TO ZERO
      DO T = 1,IJUMPYR+1
      DO M = 1,MAXSUC
      DO I = 1,OFFWELL+1
         DO R = 1,OFFRGN+1
         DO K = 1,OFFFUEL+1
            REPOFF(T,M,I,R,K) = 0.0
         ENDDO
         ENDDO
      ENDDO
      ENDDO
      ENDDO

!  --------------------------------------------  C
!  ----      COMPUTE REPORTING TOTALS      ----  C
!  --------------------------------------------  C

!  *** COMPUTE LOWER 48 OFFSHORE VALUES
      DO R = 1,OFFRGN+1
      DO I = 1,OFFWELL+1
      DO M = 1,MAXSUC
      DO T = 1,LASTYR
      DO K = 1,OFFFUEL

         IF (I.LE.OFFWELL.AND.R.LE.OFFRGN) THEN
            IF (M.EQ.1) THEN
               REPOFF(T,M,I,R,K) = OFFSUC(T,I,R,K)
            ELSE IF (M.EQ.2) THEN
               REPOFF(T,M,I,R,K) = OFFDRY(T,I,R,K)
            ELSE
               REPOFF(T,M,I,R,K) = OFFSUC(T,I,R,K) + &
                   OFFSUC(T,I,R,K)
            ENDIF
         ELSE
            IF (R.GT.OFFRGN) THEN
               REPOFF(T,M,I,R,K) = REPOFF(T,M,I,1,K) + &
                   REPOFF(T,M,I,2,K) + REPOFF(T,M,I,3,K) + &
                   REPOFF(T,M,I,4,K)
            ELSE  ! NON TOTAL REGION - TOTAL EXPL + DEVL
               REPOFF(T,M,I,R,K) = REPOFF(T,M,1,R,K) + &
                   REPOFF(T,M,2,R,K)
            ENDIF
         ENDIF

!  COMPUTE TOTAL FOR ALL FUEL TYPES FOR A GIVEN T,M,I,R
         REPOFF(T,M,I,R,OFFFUEL+1) = REPOFF(T,M,I,R,OFFFUEL+1) + &
                  REPOFF(T,M,I,R,K)

!  COMPUTE TOTAL FOR ALL YEARS FOR GIVEN M,I,R,K
         REPOFF(LASTYR+1,M,I,R,K) = REPOFF(LASTYR+1,M,I,R,K) + &
                  REPOFF(T,M,I,R,K)

!     IF (K.EQ.1) THEN
!        WRITE(*,*) 'M,I,R,TOTAL',M,I,R,REPOFF(LASTYR+1,M,I,R,K)
!        WRITE(*,*) 'T,M,I,R,K,REPOFF',T,M,I,R,K,REPOFF(T,M,I,R,K)
!     ENDIF

      ENDDO   ! ******  END FUEL (K) LOOP  ******

!  COMPUTE OVERALL TOTALS FOR A GIVEN M,I,R
        REPOFF(LASTYR+1,M,I,R,OFFFUEL+1) = &
           REPOFF(LASTYR+1,M,I,R,OFFFUEL+1)+REPOFF(T,M,I,R,OFFFUEL+1)

      ENDDO   ! ******  END YEAR (T) LOOP  ******
      ENDDO   ! ******  END SUC (M) LOOP  ******
      ENDDO   ! ******  END WELL (I) LOOP  ******
      ENDDO   ! ******  END REGION (R) LOOP  ******

!  --------------------------------------------  C
!  ----      WRITE THE OUTPUT TABLES       ----  C
!  --------------------------------------------  C

!  RESET YRNAME VARIABLE TO CORRESPOND TO LASTYR
      YRNAME(LASTYR+1) = 'TOTAL'

!  *** WRITE LOWER 48 OFFSHORE TABLES
      WRITE(OFILE1,80) TITLE

      IF (PNTFLG.EQ.1) THEN
        I=1  !DON'T LOOP OVER EXP/DEV/TOTAL
        M=1  !DON'T LOOP OVER SUC/DRY/TOTAL
        DO R = 1,OFFRGN+1
           WRITE(OFILE1,114) HEADERS(HDRFLG)
           WRITE(OFILE1,102) RGNNAMOFF(R)
           WRITE(OFILE1,122) (FUELNAMOFF(K),K=1,OFFFUEL+1)
        DO T = 1,LASTYR+1
           WRITE(OFILE1,132) YRNAME(T),(REPOFF(T,M,I,R,K),K=1,OFFFUEL+1)
        ENDDO   ! ******  END YEAR (T) LOOP  ******
           WRITE(OFILE1,140)
        ENDDO   ! ******  END REGION (R) LOOP  ******
      ELSE
        DO R = 1,OFFRGN+1
        DO I = 1,OFFWELL+1
        DO M = 1,MAXSUC
           WRITE(OFILE1,114) HEADERS(HDRFLG)
           WRITE(OFILE1,102) RGNNAMOFF(R)
           WRITE(OFILE1,110) WELLNAME(I),SUCNAME(M)
           WRITE(OFILE1,122) (FUELNAMOFF(K),K=1,OFFFUEL+1)
        DO T = 1,LASTYR+1
           WRITE(OFILE1,132) YRNAME(T),(REPOFF(T,M,I,R,K),K=1,OFFFUEL+1)
        ENDDO   ! ******  END YEAR (T) LOOP  ******
           WRITE(OFILE1,140)
        ENDDO   ! ******  END SUC (M) LOOP  ******
        ENDDO   ! ******  END WELL (I) LOOP  ******
        ENDDO   ! ******  END REGION (R) LOOP  ******
      ENDIF

80    FORMAT (1X,120('-'),/,1X,30X,A50,/,1X,120('-'))
102   FORMAT (1X,'REGION:  ',A23)
110   FORMAT (1X,2A14)
114   FORMAT (1X,120('-'),/,1X,A35)
122   FORMAT (1X,'YEAR',6X,3A14)
132   FORMAT (1X,A5,3F14.1)
140   FORMAT (1X)

      RETURN
      END

!  ********************************************************

      SUBROUTINE OGCOPY_34(SOURCE,TARGET,YEAR,MAXI,MAXR,MAXK, &
                              MAXYR)

!  THIS SUBROUTINE TAKES A 3 DIMENSION ARRAY (SOURCE) AND COPIES
!  ITS VALUES INTO A 4 DIMENSION ARRAY (TARGET) FOR YEAR (YEAR).

      INTEGER MAXI,MAXR,MAXK,MAXYR,YEAR
      INTEGER L,M,N
      REAL*4 SOURCE(MAXI,MAXR,MAXK)
      REAL*4 TARGET(MAXYR,MAXI,MAXR,MAXK)

      DO L = 1,MAXI
         DO M = 1,MAXR
            DO N = 1,MAXK
               TARGET(YEAR,L,M,N) = SOURCE(L,M,N)
!     IF (N.EQ.1) THEN
!        WRITE(*,*) 'YEAR,L,M,TARGET',YEAR,L,M,TARGET(YEAR,L,M,N)
!        WRITE(*,*) 'SOURCE',SOURCE(L,M,N)
!     ENDIF
            ENDDO
         ENDDO
      ENDDO

      RETURN
      END

!  ********************************************************

      SUBROUTINE OGCOPY_23(SOURCE,TARGET,YEAR,MAXI,MAXR,MAXYR)

!  THIS SUBROUTINE TAKES A 2 DIMENSION ARRAY (SOURCE) AND COPIES
!  ITS VALUES INTO A 3 DIMENSION ARRAY (TARGET) FOR YEAR (YEAR).

      INTEGER MAXI,MAXR,MAXYR,YEAR
      INTEGER L,M
      REAL*4 SOURCE(MAXI,MAXR)
      REAL*4 TARGET(MAXYR,MAXI,MAXR)

      DO L = 1,MAXI
         DO M = 1,MAXR
            TARGET(YEAR,L,M) = SOURCE(L,M)
         ENDDO
      ENDDO

      RETURN
      END

!  ********************************************************

      SUBROUTINE OGCOPY_24(SOURCE,TARGET,YEAR,MAXI,MAXR,MAXK, &
                              MAXYR)

!  THIS SUBROUTINE TAKES A 2 DIMENSION ARRAY (SOURCE) AND COPIES
!  ITS VALUES INTO A 4 DIMENSION ARRAY (TARGET) FOR YEAR (YEAR).
!  "I" IS SET EQUAL TO 1.

      INTEGER MAXI,MAXR,MAXK,MAXYR,YEAR
      INTEGER L,M,N
      REAL*4 SOURCE(MAXR,MAXK)
      REAL*4 TARGET(MAXYR,MAXI,MAXR,MAXK)

      L = 1
      DO M = 1,MAXR
         DO N = 1,MAXK
            TARGET(YEAR,L,M,N) = SOURCE(M,N)
            TARGET(YEAR,2,M,N) = 0.0
         ENDDO
      ENDDO

      RETURN
      END

!******************************************************************
      SUBROUTINE OGREP_TABLES
      IMPLICIT NONE

!  THIS SUBROUTINE REPORTS THE DATA NEEDED TO FILL TABLE 28 OF THE
!  NEMS OUTPUT TABLES.  THIS TABLE CONTAINS THE OIL AND NATURAL
!  GAS PRODUCTION, RESERVES, AND PRICES.

      include'parametr'
      include'ncntrl'
      include'ogsmparm'
      include'ogsmbfw'
      include'ogsml48'
      include'ogsmugr'
      include'ogsmoff'
      include'ogsmak'
      include'ogsmlng'
      include'ogsmout'
      include'ngtdmrep'
      include'pmmout'
      include'intout'

      integer iy
      integer ic
      REAL TOTCRUD
      REAL TOTQGAS
      REAL TOTCAP   ! USED TO COMPUTE PRICE WEIGHTED AVERAGES
      REAL CALRES(MNUMOR-1)  ! RESERVE REPORT CALIBRATION FACTORS
      REAL DELDRL  !  DELTA BETWEEN 1992 HISTORICAL WELLS & ESTIMATE
      REAL CALDRL  !  CALIBRATION FACTOR FOR DRILLING
      REAL YRFADE,WELBENCH,WELBENCH2
      REAL ugrresshr, tempscal
      REAL hwells(L48HYR+1)
      REAL NATEST(l48rgn,gastypes),PRDRGN(l48rgn),ADTEST(l48rgn,oiltypes)
      DATA HWELLS/ 34135., 30503., 26298., 30085., 26655., 25645., 27971., 30363., 26392., 25347.,  &
                   32145., 37084., 32156., 34806., 38589., 43513., 51648., 51557., 57718., 35366.,  &
                   40196., 44248., 43060., 40877., 41973., 28697., 17660., 21716., 25944., 24831.,  &
                   14649., 15707., 20800./



!  REPORT NUMBER OF TIMED PROJECTS BY PROCESS TYPE
      iy = curiyr + baseyr -1 - l48b4yr + 1
      if (iy.eq.1) WRITE(ogbug1,901) curiyr+baseyr-1,(r-1,r=1,max_proc)
      if (iy.gt.0) WRITE(ogbug1,901) curiyr+baseyr-1,(timed_proc(r,iy),r=1,max_proc)
  901 FORMAT (1x,'timed projects',I6,24I6)

!  INITIALIZE REGIONAL PRODUCTION VARIABLES TO ZERO
      DO R = 1,L48RGN
      DO K = 1,L48FUEL
        OGREGPRD(R,K,curiyr) = 0.
      ENDDO
      ENDDO
!  INITIALIZE CRUDE TABLE VARIABLES TO ZERO
      OGPCRWHP(CURIYR) = 0.0
      OGQCRRSV(CURIYR) = 0.0
      DO R=1,MNOGCRO
         IF (R.NE.1.AND.R.NE.5) OGQCRREP(R,CURIYR) = 0.0  ! Position 1 has already been assigned to oil shale
                                                           ! and position 2 is ANWR
         OGCORSV(R,CURIYR) = 0.0
      ENDDO
      DO R = 1,L48RGN+OFFNEMSRGN+1
        OGCOWHP(R,CURIYR) = 0.0
        OGCOPRD(R,CURIYR) = 0.0
      ENDDO

!  INITIALIZE GAS TABLE VARIABLES TO ZERO
      OGPNGWHP(CURIYR) = 0.0
      OGQNGRSV(CURIYR) = 0.0
      OGNOWELL(CURIYR) = 0.0
      DO R=1,MNOGCAT
         OGQNGREP(R,CURIYR) = 0.0
         OGNGRSV(R,CURIYR) = 0.0
      ENDDO

      DO R=1,L48RGN+OFFNEMSRGN+1
         OGNGPRD(R,CURIYR) = 0.0
         OGNGWHP(R,CURIYR) = 0.0
      ENDDO

!  INITIALIZE EXPENDITURES & WELL COUNT VARIABLES TO 0
      DO R=1,4
         EXSPEND(R,CURIYR) = 0.0
         DVSPEND(R,CURIYR) = 0.0
         EXOILFT(R,CURIYR) = 0.0
         DVOILFT(R,CURIYR) = 0.0
         EXGASFT(R,CURIYR) = 0.0
         DVGASFT(R,CURIYR) = 0.0
         EXDRYFT(R,CURIYR) = 0.0
         DVDRYFT(R,CURIYR) = 0.0
      ENDDO

! INITIALIZE RESERVES CALIBRATION FACTORS AND TOTAL WELLS DELTA
      IF(CURIYR.EQ.1) THEN
      DO I = 1,MNUMOR-1
        CALRES(I) = 0.0
      ENDDO
        DELDRL = 0.0
        CALDRL = 0.0
      ENDIF

!  SET ALL VALUES IN TABLE TO ZERO BEYOND THE LAST YEAR RUN OF MODEL
!  THIS WILL PREVENT THE TABLES FROM REPORTING OLD VALUES THAT
!  MAY BE IN THE INPUT RESTART FILE.
      IF (CURIYR.EQ.LASTYR .AND. LASTYR.LT.IJUMPYR) THEN
         DO M=LASTYR+1,IJUMPYR
!  INITIALIZE CRUDE TABLE VARIABLES TO ZERO
            OGPCRWHP(M) = 0.0
            OGQCRRSV(M) = 0.0
            DO R=1,MNOGCRO
               OGQCRREP(R,M) = 0.0
               OGCORSV(R,M) = 0.0
            ENDDO
          DO R = 1,L48RGN+OFFNEMSRGN+1
            OGCOWHP(R,M) = 0.0
            OGCOPRD(R,M) = 0.0
          ENDDO


!  INITIALIZE GAS TABLE VARIABLES TO ZERO
            OGPNGWHP(M) = 0.0
            OGQNGRSV(M) = 0.0
            OGNOWELL(M) = 0.0
            DO R=1,MNOGCAT
               OGQNGREP(R,M) = 0.0
               OGNGRSV(R,M) = 0.0
            ENDDO

            DO R=1,L48RGN+OFFNEMSRGN+1
               OGNGPRD(R,M) = 0.0
               OGNGWHP(R,M) = 0.0
            ENDDO

!  SET EXPENDITURE & WELL COUNT REPORTING VARIABLES TO 0
            DO R=1,4
               EXSPEND(R,M) = 0.0
               DVSPEND(R,M) = 0.0
               EXOILFT(R,M) = 0.0
               DVOILFT(R,M) = 0.0
               EXGASFT(R,M) = 0.0
               DVGASFT(R,M) = 0.0
               EXDRYFT(R,M) = 0.0
               DVDRYFT(R,M) = 0.0
            ENDDO

         ENDDO    ! END OF M LOOP
      ENDIF   ! END IF CURRENT=LAST & LAST<IJUMPYR

      natest = 0.
      adtest = 0.
      DO I=1,66
        DO K=1,gastypes-1
          r=DISTMAP(I,3)
          if(r>=1 .and. r<=l48rgn) then
            NATEST(r,k) = NATEST(r,K) + OGRNAGPRD(i,k,curiyr)
            NATEST(r,gastypes) = NATEST(r,gastypes) + OGRNAGPRD(i,k,curiyr)
          endif
        ENDDO
        DO K=1,oiltypes-1
          r=DISTMAP(I,3)
          if(r>=1 .and. r<=l48rgn) then
            ADTEST(r,k) = ADTEST(r,K) + OGADGPRD(i,k,curiyr)
            ADTEST(r,oiltypes) = ADTEST(r,oiltypes) + OGADGPRD(i,k,curiyr)
          endif
        ENDDO
      ENDDO
      DO R=1,L48RGN
!       write(6,*) 'dh5out-na',curiyr+1989,r,sum(prdl48(r,3:7)),natest(r,5)
!       write(6,*) 'dh5out-ad',curiyr+1989,r,ogprdad(r,curiyr),adtest(r,5)
        DO K=1,gastypes-1
          PRDL48(r,k+3) = NATEST(r,k)
        ENDDO
        REPPRDAD(r,2,curiyr) = ADTEST(r,2)
        REPPRDAD(r,1,curiyr) = ADTEST(r,oiltypes) - ADTEST(R,2)
      ENDDO

!  ASSIGN AD GAS FROM TIGHT OIL PLAYS
!     tempscal = 0.
!     if (curiyr.le.l48hyr+3) tempscal = sum(ogqshlgas(1:15,curiyr))*1000. - (sum(prdl48(1:l48rgn,6))+sum(repprdad(1:l48rgn,2,curiyr)))
!     REPPRDAD(1,2,CURIYR) = REPPRDAD(1,2,CURIYR)+0.02*TEMPSCAL
!     REPPRDAD(2,2,CURIYR) = REPPRDAD(2,2,CURIYR)+0.19*TEMPSCAL
!     REPPRDAD(3,2,CURIYR) = REPPRDAD(3,2,CURIYR)+0.13*TEMPSCAL
!     REPPRDAD(4,2,CURIYR) = REPPRDAD(4,2,CURIYR)+0.41*TEMPSCAL
!     REPPRDAD(5,2,CURIYR) = REPPRDAD(5,2,CURIYR)+0.22*TEMPSCAL
!     REPPRDAD(6,2,CURIYR) = REPPRDAD(6,2,CURIYR)+0.02*TEMPSCAL
!     REPPRDAD(7,2,CURIYR) = REPPRDAD(7,2,CURIYR)+0.01*TEMPSCAL
      DO R=1,L48RGN
        OGPRDAD(r,curiyr) = REPPRDAD(r,1,curiyr) + REPPRDAD(r,2,curiyr)
      ENDDO

!  ADJUST SHARING OF PRODUCTION
!     IF(CURIYR+BASEYR-1.EQ.2017) THEN
!       PRDL48(5,5) = PRDL48(5,5) + 350.
!       REPPRDAD(5,2,CURIYR) = REPPRDAD(5,2,CURIYR) - 350.
!       OGPRDAD(5,CURIYR) = OGPRDAD(5,CURIYR) - 350.
!     ENDIF

!  ******* ASSIGN TABLE 28 PRODUCTION AND RESERVES VARIABLES *******
      DO R=1,L48RGN
!  CONVENTIONAL OIL PRODUCTION + CO2 EOR PRODUCTION
         OGQCRREP(2,CURIYR) = OGQCRREP(2,CURIYR) + PRDL48(R,1) + &
                              PRDL48(R,2)

!  OIL SHALE PRODUCTION

!        OGQCRREP(1,CURIYR) has been assigned to oil shale production in OGFOR_OS

!  SHALE PRODUCTION PLUS GAS PRODUCTION FROM TIGHT OIL PLAYS
         OGQNGREP(1,CURIYR) = OGQNGREP(1,CURIYR) + PRDL48(R,6) + REPPRDAD(R,2,CURIYR)

!  COALBED METHANE PRODUCTION
         OGQNGREP(2,CURIYR) = OGQNGREP(2,CURIYR) + PRDL48(R,7)

!  TIGHT SANDS PRODUCTION
         OGQNGREP(3,CURIYR) = OGQNGREP(3,CURIYR) + PRDL48(R,5)

!  CONVENTIONAL GAS PRODUCTION
         OGQNGREP(4,CURIYR) = OGQNGREP(4,CURIYR) + PRDL48(R,3) + &
                              PRDL48(R,4)

!  ONSHORE AD GAS PRODUCTION FROM NON TIGHT OIL PLAYS
         OGQNGREP(5,CURIYR) = OGQNGREP(5,CURIYR) + REPPRDAD(R,1,CURIYR)

!  ACCUMULATE TOTAL US OIL RESERVES (ADD L48 TO TOTAL)
!  COMPUTE EOR RESERVES USING EOR PROD * 12.0 (R/P RATIO) MB->MMB
         OGQCRRSV(CURIYR) = OGQCRRSV(CURIYR) + RESBOYL48(R,1)

!  ACCUMULATE TOTAL US GAS RESERVES (ADD L48 TO TOTAL)
         OGQNGRSV(CURIYR) = OGQNGRSV(CURIYR) + RESBOYL48(R,3) + &
                            RESBOYL48(R,4) + RESBOYL48(R,5) + &
                            RESBOYL48(R,6) + RESBOYL48(R,7)
      ENDDO
      ! Adjust 'other' to make sure total play-level section matches total on Table 14
      tempscal =  OGQNGREP(1,curiyr) - sum(ogqshlgas(1:15,curiyr))*1000.
      ogqshlgas(15,curiyr) = ogqshlgas(15,curiyr) + tempscal/1000.


      DO R=1,OFFRGN
!  TOTAL OFFSHORE OIL PRODUCTION
        OGQCRREP(3,CURIYR) = OGQCRREP(3,CURIYR) + OGPRODOFF(R,1)

!  TOTAL OFFSHORE GAS PRODUCTION
        OGQNGREP(6,CURIYR) = OGQNGREP(6,CURIYR) + OGPRODOFF(R,2)

!  TOTAL OFFSHORE AD
        IF (R.LE.3) &
        OGQNGREP(7,CURIYR) = OGQNGREP(7,CURIYR) + OGPRDAD(R+L48RGN,CURIYR)

!  ACCUMULATE TOTAL US OIL RESERVES (ADD OFFSHORE TO TOTAL)
        OGQCRRSV(CURIYR) = OGQCRRSV(CURIYR) + RESBOYOFF(R,1)

!  ACCUMULATE TOTAL US GAS RESERVES (ADD OFFSHORE TO TOTAL)
        OGQNGRSV(CURIYR) = OGQNGRSV(CURIYR) + RESBOYOFF(R,2)
      ENDDO


      DO R=1,AKRGN
!  ALASKA OIL PRODUCTION
        OGQCRREP(4,CURIYR) = OGQCRREP(4,CURIYR) + OGPRCOAK(R,CURIYR)
!       OGQCRREP(5,CURIYR) equals ANWR volumes

!  ALASKA GAS PRODUCTION
        OGQNGREP(8,CURIYR) = OGQNGREP(8,CURIYR) + OGPRNGAK(R,CURIYR)

!  ACCUMULATE TOTAL US OIL RESERVES (ADD ALASKA TO TOTAL)
!  do not add Alaska to reserve total
!       OGQCRRSV(CURIYR) = OGQCRRSV(CURIYR) + BOYRESCOAK(R,CURIYR)

!  ACCUMULATE TOTAL US GAS RESERVES (ADD ALASKA TO TOTAL)
!  do not add Alaska to reserve total
!       OGQNGRSV(CURIYR) = OGQNGRSV(CURIYR) + BOYRESNGAK(R,CURIYR)
      ENDDO

!  ADD IN AD GAS RESERVES

      DO R=1,L48RGN+OFFNEMSRGN
         OGQNGRSV(CURIYR) = OGQNGRSV(CURIYR) + EOYADRES(R,CURIYR)
         IF (R.LE.L48RGN) OGQNGRSV(CURIYR) = OGQNGRSV(CURIYR) 
      ENDDO

!  TOTAL US OIL PRODUCTION - SLOT USED FOR ANWR VOLUMES INSTEAD OF TOTAL
!     OGQCRREP(5,CURIYR) = OGQCRREP(1,CURIYR) + OGQCRREP(2,CURIYR) + &
!                          OGQCRREP(3,CURIYR) + OGQCRREP(4,CURIYR)

!  TOTAL US GAS PRODUCTION
      DO R=1,8
         OGQNGREP(9,CURIYR) = OGQNGREP(9,CURIYR) + OGQNGREP(R,CURIYR)
      ENDDO

!  THIS SECTION ASSIGNS SYN GAS FROM LIQUIDS, OTHER SUPPLEMENTAL
!  GAS AND SYN GAS FROM COAL USING A VARIABLE FROM NGTDM.
!  ***********  ACTIVATE THESE LINES ONCE VARIABLE OGPRSUP3 GETS
!  ***********  PUT IN THE RESTART FILE.
!  SYNTHETIC GAS FROM LIQUIDS
!  W/ITM -- NO LONGER DESIGNATE 3 SEPARATELY, ALL INCLUDED IN ONE
      OGQNGREP(10,CURIYR) = OGPRSUP(CURIYR) / 1000.0 ! (BCF->TCF)
!  SYNTHETIC GAS FROM COAL
      OGQNGREP(11,CURIYR) = 0.0
!  OTHER SUPPLEMENTAL GAS
      OGQNGREP(12,CURIYR) = 0.0

!  CALC AVG CRUDE OIL WELLHEAD PRICE INSTEAD OF THE FOLLOWING
!  AVERAGE CRUDE OIL WELL HEAD PRICE
!     OGPCRWHP(CURIYR) = DCRDWHP(MNUMOR,CURIYR)
!  ASSIGN AVERAGE NG WELL HEAD PRICE - NGTDM VARIABLE
      OGPNGWHP(CURIYR) = OGWPRNG(MNUMOR,CURIYR)

!  COMPUTE WEIGHTED AVERAGE PRICES FOR LOWER 48 ON & OFFSHORE
        TOTCRUD = 0.0   ! INITIALIZE TO ZERO
        TOTQGAS = 0.0    ! INITIALIZE TO ZERO
        DO R=1,L48RGN   ! SUM ONSHORE PRODUCTION
           TOTCRUD = TOTCRUD + REPPRDL48(R,1,CURIYR) + &
                     REPPRDL48(R,2,CURIYR)
           DO K=3,7
              TOTQGAS = TOTQGAS + REPPRDL48(R,K,CURIYR)
           ENDDO
        ENDDO
        DO R=1,OFFRGN   ! SUM OFFSHORE PRODUCTION
           TOTCRUD = TOTCRUD + REPPRDOFF(R,1,CURIYR)
           TOTQGAS = TOTQGAS + REPPRDOFF(R,2,CURIYR)
        ENDDO

        DO R=1,L48RGN
!          OGPCRWHP(CURIYR) = OGPCRWHP(CURIYR) +
!    1        (REPPRCL48(R,1,CURIYR)*(REPPRDL48(R,1,CURIYR)/TOTCRUD))
!          DO K=2,6
!             OGPNGWHP(CURIYR) = OGPNGWHP(CURIYR) +
!    1           (REPPRCL48(R,K,CURIYR) * (REPPRDL48(R,K,CURIYR)
!    2           / TOTQGAS))
!          ENDDO
        ENDDO

        DO R=1,OFFRGN
!          OGPCRWHP(CURIYR) = OGPCRWHP(CURIYR) +
!    1        (REPPRCOFF(R,1,CURIYR)*(REPPRDOFF(R,1,CURIYR)/TOTCRUD))
!          OGPNGWHP(CURIYR) = OGPNGWHP(CURIYR) +
!    1        (REPPRCOFF(R,2,CURIYR)*(REPPRDOFF(R,2,CURIYR)/TOTQGAS))
        ENDDO

!  CALCULATE WELL TOTALS
      DO I=1,L48WELL
         DO R=1,L48RGN
            DO K=1,L48FUEL
               OGNOWELL(CURIYR) = OGNOWELL(CURIYR)+WELLSL48(I,R,K)
            ENDDO
         ENDDO
      ENDDO
   ! ADD CO2 EOR WELLS
      DO I=1,7
         DO R=1,13
           OGNOWELL(CURIYR) = OGNOWELL(CURIYR)
         ENDDO
      ENDDO

      DO I=1,OFFWELL
         DO R=1,OFFRGN
            DO K=1,OFFFUEL
               OGNOWELL(CURIYR) = OGNOWELL(CURIYR)+WELLSOFF(I,R,K)
           ENDDO
         ENDDO
      ENDDO

!  OVERWRITE WITH HISTORICAL ESTIMATE FROM WELLCOM
      IF (CURIYR.LE.L48HYR+1) THEN
        if(CURIYR.eq.L48HYR+1) WELBENCH = HWELLS(CURIYR)/OGNOWELL(CURIYR)
        OGNOWELL(CURIYR) = HWELLS(CURIYR)
      ENDIF
      WELBENCH = 1.0
      IF (CURIYR.GT.L48HYR+1) THEN
        YRFADE = YRFADE + 1
        IF (YRFADE.GT.3) YRFADE = 3
        OGNOWELL(CURIYR) = OGNOWELL(CURIYR)*  &
           (WELBENCH+(1-WELBENCH)*(YRFADE/3))
      ENDIF
      WELBENCH2 = 1.0
!     IF (CURIYR+1989.EQ.2021) OGNOWELL(CURIYR) = OGNOWELL(CURIYR)
!     IF (CURIYR+1989.EQ.2022) OGNOWELL(CURIYR) = OGNOWELL(CURIYR)-5000.
      IF (CURIYR+1989.EQ.2023) OGNOWELL(CURIYR) = OGNOWELL(CURIYR)-5000.

!  ONLY OUTPUT THIS DEBUG STUFF AFTER LAST YEAR REPORTED
      IF ((PRTDBGL.EQ.1 .AND. OGREPORT(28).GT.0) .AND. &
          (CURIYR .EQ. LASTYR .AND. NCRL .EQ. 1)) THEN
         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*)
         WRITE (SYSOUT,*) ' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
         WRITE (SYSOUT,*) ' !!!!!!   DEBUG  TABLE  OUTPUT  !!!!!!'
         WRITE (SYSOUT,*) ' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
         WRITE (SYSOUT,*)

         WRITE (SYSOUT,*) ' ----- CRUDE VARIABLES -----'
         DO M=1,IJUMPYR
            WRITE(SYSOUT,990) OGPCRWHP(M),OGQCRRSV(M),OGQCRREP(1,M), &
                           OGQCRREP(2,M),OGQCRREP(3,M),OGQCRREP(4,M), &
                           OGQCRREP(5,M)
         ENDDO

         WRITE (SYSOUT,*) ' ----- GAS VARIABLES -----'
         DO M=1,IJUMPYR
            WRITE(SYSOUT,991) OGPNGWHP(M),OGQNGRSV(M),OGQNGREP(1,M), &
                         OGQNGREP(2,M),OGQNGREP(3,M),OGQNGREP(4,M), &
                         OGQNGREP(5,M),OGQNGREP(6,M),OGQNGREP(7,M), &
                         OGQNGREP(8,M),OGQNGREP(9,M),OGQNGREP(10,M), &
                         OGQNGREP(11,M),OGQNGREP(12,M),OGNOWELL(M)
         ENDDO

      ENDIF

  990 FORMAT (7F7.0)
  991 FORMAT (15F7.0)

!********************************************************************
!  ASSIGN VARIABLES TO FILL TABLE 64 OF THE NEMS OUTPUT TABLES.
!  THE TABLE CONTAINS...
!*********************  CRUDE OIL RESERVES  *************************

      DO R=1,L48RGN
!  TOTAL EOR RESERVES
         OGCORSV(1,CURIYR) = OGCORSV(1,CURIYR) + RESBOYL48(R,2)

!  TOTAL CONVENTIONAL CRUDE RESERVES
         OGCORSV(2,CURIYR) = OGCORSV(2,CURIYR) + RESBOYL48(R,1)

!   TOTAL DEVONIAN SHALE RESERVES
         OGNGRSV(1,CURIYR) = OGNGRSV(1,CURIYR) + RESBOYL48(R,6)

!   TOTAL COALBED METHANE RESERVES
         OGNGRSV(2,CURIYR) = OGNGRSV(2,CURIYR) + RESBOYL48(R,7)

!   TOTAL TIGHT SANDS RESERVES
         OGNGRSV(3,CURIYR) = OGNGRSV(3,CURIYR) + RESBOYL48(R,5)

!  TOTAL CONVENTIONAL RESERVES
         OGNGRSV(4,CURIYR) = OGNGRSV(4,CURIYR) + RESBOYL48(R,3) + &
                             RESBOYL48(R,4)

!  TOTAL ONSHORE AD
         OGNGRSV(5,CURIYR) = OGNGRSV(5,CURIYR) + EOYADRES(R,CURIYR) 
      ENDDO

      DO R=1,OFFRGN
!  TOTAL OFFSHORE CRUDE RESERVES
         OGCORSV(3,CURIYR) = OGCORSV(3,CURIYR) + RESBOYOFF(R,1)

!  TOTAL OFFSHORE GAS RESERVES
         OGNGRSV(6,CURIYR) = OGNGRSV(6,CURIYR) + RESBOYOFF(R,2)

      ENDDO
!  TOTAL OFFSHORE AD
      DO R=1,OFFNEMSRGN
         OGNGRSV(7,CURIYR) = OGNGRSV(7,CURIYR) + EOYADRES(L48RGN+R,CURIYR)
      ENDDO

      DO R=1,AKRGN
!  TOTAL ALASKA CRUDE RESERVES
         OGCORSV(4,CURIYR) = OGCORSV(4,CURIYR) + BOYRESCOAK(R,CURIYR)

!  TOTAL ALASKA GAS RESERVES
         OGNGRSV(8,CURIYR) = OGNGRSV(8,CURIYR) + BOYRESNGAK(R,CURIYR)
      ENDDO

!  Lower 48 CRUDE OIL RESERVES
      OGCORSV(5,CURIYR) = OGCORSV(1,CURIYR) + OGCORSV(2,CURIYR) + &
                          OGCORSV(3,CURIYR)
      OGQCRRSV(CURIYR) = OGCORSV(5,CURIYR)

!  Lower 48 GAS RESERVES
      OGNGRSV(9,CURIYR) = OGNGRSV(1,CURIYR) + OGNGRSV(2,CURIYR) + &
                          OGNGRSV(3,CURIYR) + OGNGRSV(4,CURIYR) + &
                          OGNGRSV(5,CURIYR) + OGNGRSV(6,CURIYR) + &
                          OGNGRSV(7,CURIYR)
      OGQNGRSV(CURIYR) = OGNGRSV(9,CURIYR)

!  Total Proved Oil Reserves (FTAB Table 65)
      OGEOYRSV(MNUMOR,1,CURIYR) = (OGQCRRSV(CURIYR) * 0.001 + OGEOYRSV(MNUMOR-3,1,CURIYR) + &
                                OGEOYRSV(MNUMOR-2,1,CURIYR) + OGEOYRSV(MNUMOR-1,1,CURIYR))

!  Total Proved Gas Reserves (FTAB Table 66)
      OGEOYRSV(MNUMOR,2,CURIYR) = (OGQNGRSV(CURIYR) * 0.001 + OGEOYRSV(MNUMOR-3,2,CURIYR) + &
                                OGEOYRSV(MNUMOR-2,2,CURIYR) + OGEOYRSV(MNUMOR-1,2,CURIYR))

!  COMPUTE BEGINING-OF-YEAR (CURIYR+1) RESERVES FOR EACH REGION/FUEL
      DO R = 1,L48RGN
         DO K = 1,L48FUEL
            IF(RESBOYL48(R,K).LT.0.0) RESBOYL48(R,K)=0.0
            IF(K.GT.2)THEN
              OGEOYRSV(R,2,CURIYR) = OGEOYRSV(R,2,CURIYR) + RESBOYL48(R,K)/1000.
              OGEOYRSVON(R,K-2,CURIYR) = RESBOYL48(R,K)/1000.
            ENDIF
         ENDDO
         OGEOYRSV(R,1,CURIYR) = RESBOYL48(R,1)/1000. + RESBOYL48(R,2)/1000.
      ENDDO


!********************************************************************
!  ASSIGN VARIABLES TO FILL TABLE 65 OF THE NEMS OUTPUT TABLES.
!  THE TABLE CONTAINS L48 CRUDE OIL PRODUCTION AND WELLHEAD PRICES
!  BY REGION
!************  CRUDE OIL PRODUCTION VARIABLES BY REGION  ************

!  TOTAL LOWER 48 ONSHORE REGION PRODUCTION AND PRICES
      DO R=1,L48RGN
        OGCOPRD(R,CURIYR) = RFQTDCRD(R,CURIYR)
        OGCOWHP(R,CURIYR) = DCRDWHP(R,CURIYR)
      ENDDO
!  SWITCH PACIFIC and NORTHERN GREAT PLAINS for FTAB
      OGCOPRD(6,CURIYR) = RFQTDCRD(7,CURIYR)
      OGCOWHP(6,CURIYR) = DCRDWHP(7,CURIYR)
      OGCOPRD(7,CURIYR) = RFQTDCRD(6,CURIYR)
      OGCOWHP(7,CURIYR) = DCRDWHP(6,CURIYR)

!  TOTAL OFFSHORE REGION GOM PRODUCTION AND PRICES
      OGCOPRD(L48RGN+1,CURIYR) = RFQTDCRD(L48RGN+2,CURIYR)
      OGCOWHP(L48RGN+1,CURIYR) = DCRDWHP(L48RGN+2,CURIYR)   ! PMM GOM RGN=9

!  TOTAL OFFSHORE REGION PACIFIC PRODUCTION AND PRICES
      OGCOPRD(L48RGN+2,CURIYR) = RFQTDCRD(L48RGN+3,CURIYR)
      OGCOWHP(L48RGN+2,CURIYR) = DCRDWHP(L48RGN+3,CURIYR)   ! PMM PACIFIC RGN=10

!  TOTAL OFFSHORE REGION ATLANTIC PRODUCTION AND PRICES
      OGCOPRD(L48RGN+3,CURIYR) = RFQTDCRD(L48RGN+1,CURIYR)
      OGCOWHP(L48RGN+3,CURIYR) = DCRDWHP(L48RGN+1,CURIYR)   ! PMM ATLANTIC RGN=8

!  TOTAL LOWER 48 PRODUCTION AND PRICES
      DO R=1,L48RGN+OFFNEMSRGN
         OGCOPRD(L48RGN+OFFNEMSRGN+1,CURIYR) = OGCOPRD(L48RGN+OFFNEMSRGN+1,CURIYR) + OGCOPRD(R,CURIYR)
         if (r.le.l48rgn-2) then
           OGREGPRD(R,1,CURIYR) = RFQDCRD(R,CURIYR)
           OGREGPRD(R,2,CURIYR) = (RFQTDCRD(R,CURIYR) - RFQDCRD(R,CURIYR))
           if(r.eq.5) OGREGPRD(R,1,CURIYR) = OGREGPRD(R,1,CURIYR) + OGQCRREP(1,CURIYR)/365./1000.
           if(r.eq.5) OGREGPRD(R,2,CURIYR) = OGREGPRD(R,2,CURIYR) - OGQCRREP(1,CURIYR)/365./1000.
         ENDIF
      ENDDO
!  ASSIGN NGP REGION TO POSITION 6 and WC REGION to POSITION 7 FOR REPORTING IN FTAB
      OGREGPRD(6,1,CURIYR) = RFQDCRD(7,CURIYR)
      OGREGPRD(6,2,CURIYR) = (RFQTDCRD(7,CURIYR) - RFQDCRD(7,CURIYR))
      OGREGPRD(7,1,CURIYR) = RFQDCRD(6,CURIYR)
      OGREGPRD(7,2,CURIYR) = (RFQTDCRD(6,CURIYR) - RFQDCRD(6,CURIYR))

! LOWER 48 AVERAGE PRICE
      DO R=1,L48RGN+OFFNEMSRGN
        OGCOWHP(L48RGN+OFFNEMSRGN+1,CURIYR) = OGCOWHP(L48RGN+OFFNEMSRGN+1,CURIYR) + &
          OGCOWHP(R,CURIYR)*(OGCOPRD(R,CURIYR)/OGCOPRD(L48RGN+OFFNEMSRGN+1,CURIYR))
      ENDDO
      OGPCRWHP(CURIYR) = OGCOWHP(L48RGN+OFFNEMSRGN+1,CURIYR)

!  ******************************************************************
!  ASSIGN VARIABLES TO FILL TABLE 66 OF THE NEMS OUTPUT TABLES.
!  THE TABLE CONTAINS L48 NG PRODUCTION AND WELLHEAD PRICES
!  BY REGION

!   **********  NATURAL GAS PRODUCTION VARIABLES BY REGION *********
      DO K=3,L48FUEL
      DO R=1,L48RGN
        OGNGPRD(R,CURIYR) = OGNGPRD(R,CURIYR) + PRDL48(R,K)
        OGREGPRD(R,K,CURIYR) = OGREGPRD(R,K,CURIYR) + PRDL48(R,K)/1000.
      ENDDO
      ENDDO

!  ADD AD GAS PRODUCTION TO THE PRODUCTION REPORTING VARIABLES
      DO R=1,L48RGN
        OGNGPRD(R,CURIYR) = OGNGPRD(R,CURIYR) + OGPRDAD(R,CURIYR)
        OGREGPRD(R,4,CURIYR) = OGREGPRD(R,4,CURIYR) + REPPRDAD(R,1,CURIYR)/1000.
        OGREGPRD(R,6,CURIYR) = OGREGPRD(R,6,CURIYR) + REPPRDAD(R,2,CURIYR)/1000.
        OGNGWHP(R,CURIYR) = OGWPRNG(R,CURIYR)
      ENDDO
      DO K=3,L48FUEL
      DO R=1,L48RGN
        IF(OGREGPRD(R,K,CURIYR).LT.0.) THEN
         write(ogbug1,*) 'WARNING: NEG PROD', CURIYR+1989, R, K, PRDL48(R,K)/1000.
         OGREGPRD(R,K,CURIYR) = 0.
        ENDIF
      ENDDO
      ENDDO

!  TOTAL OFFSHORE REGION GOM PRODUCTION
      DO R=3,OFFRGN
         OGNGPRD(L48RGN+1,CURIYR) = OGNGPRD(L48RGN+1,CURIYR) + OGPRODOFF(R,2)
      ENDDO
      OGNGPRD(L48RGN+1,CURIYR) = OGNGPRD(L48RGN+1,CURIYR) + OGPRDAD(L48RGN+2,CURIYR)

!  TOTAL OFFSHORE REGION PACIFIC PRODUCTION
      OGNGPRD(L48RGN+2,CURIYR) = OGPRODOFF(2,2) + OGPRDAD(L48RGN+3,CURIYR)

!  TOTAL OFFSHORE REGION ATLANTIC PRODUCTION
      OGNGPRD(L48RGN+3,CURIYR) = OGPRODOFF(1,2) + OGPRDAD(L48RGN+1,CURIYR)

!  TOTAL LOWER 48 PRODUCTION
      DO R=1,L48RGN+OFFNEMSRGN
         OGNGPRD(L48RGN+OFFNEMSRGN+1,CURIYR) = OGNGPRD(L48RGN+OFFNEMSRGN+1,CURIYR) + OGNGPRD(R,CURIYR)
      ENDDO

!  *************** NATURAL GAS WELLHEAD PRICES BY REGION ************
      OGNGWHP(L48RGN+1,CURIYR) = OGWPRNG(L48RGN+2,CURIYR)   ! NGTDM GOM RGN=8
      OGNGWHP(L48RGN+2,CURIYR) = OGWPRNG(L48RGN+3,CURIYR)   ! NGTDM PACIFIC RGN=9
      OGNGWHP(L48RGN+3,CURIYR) = OGWPRNG(L48RGN+1,CURIYR)   ! NGTDM ATLANTIC RGN=7
      OGNGWHP(L48RGN+OFFNEMSRGN+1,CURIYR) = OGPNGWHP(CURIYR) ! LOWER 48 AVERAGE


!  ******************************************************************
!  ASSIGN VARIABLES TO FILL TABLE 67 OF THE NEMS OUTPUT TABLES.
!  THE TABLE CONTAINS OIL AND GAS DRILLING FOOTAGE AND EXPENDITURES.

!  LOWER 48 EXPENDITURES
      DO R=1,L48RGN
         DO K=3,L48FUEL
!           EXSPEND(1,CURIYR) = EXSPEND(1,CURIYR)+SPENDIRK_L48(1,R,K)
            EXSPEND(1,CURIYR) = EXSPEND(1,CURIYR)+DRILLL48(2,R,K)/1000.*SUCWELLL48(2,R,K)  &
                               + DRILLL48(1,R,K)/1000.*SUCWELLL48(1,R,K)
            DVSPEND(1,CURIYR) = DVSPEND(1,CURIYR)+SPENDIRK_L48(2,R,K)
         ENDDO
      ENDDO

!  OFFSHORE EXPENDITURES
      DO R=1,OFFRGN
         DO K=1,OFFFUEL
            EXSPEND(2,CURIYR) = EXSPEND(2,CURIYR)+SPENDIRK_OFF(1,R,K)
            DVSPEND(2,CURIYR) = DVSPEND(2,CURIYR)+SPENDIRK_OFF(2,R,K)
         ENDDO
      ENDDO

!  ALASKA EXPENDITURES  (ONLY REPORT LOWER 48  9/22/93 PJ3)
!     EXSPEND(3,CURIYR) = 0.0   ! DO NOT REPORT AK VALUES
      DVSPEND(3,CURIYR) = 0.0   ! DO NOT REPORT AK VALUES

!  U.S. TOTAL EXPENDITURES
      EXSPEND(4,CURIYR) = EXSPEND(1,CURIYR) + EXSPEND(2,CURIYR) + &
                          EXSPEND(3,CURIYR)
      DVSPEND(4,CURIYR) = DVSPEND(1,CURIYR) + DVSPEND(2,CURIYR) + &
                          DVSPEND(3,CURIYR)

      DO R=1,L48RGN
!  EXP & DEV SUCCESSFUL OIL WELLS LOWER48
        EXOILFT(1,CURIYR) = EXOILFT(1,CURIYR) + sucwelll48(1,R,1) + &
                            sucwelll48(1,r,2)
        DVOILFT(1,CURIYR) = DVOILFT(1,CURIYR) + sucwelll48(2,R,1) + &
                            sucwelll48(2,r,2)


!  EXP & DEV SUCCESSFUL GAS WELLS LOWER48
        DO K=3,L48FUEL
           EXGASFT(1,CURIYR) = EXGASFT(1,CURIYR) + sucwelll48(1,R,K)
           DVGASFT(1,CURIYR) = DVGASFT(1,CURIYR) + sucwelll48(2,R,K)
        ENDDO

!  EXP & DEV DRY WELLS LOWER48
        DO K=1,L48FUEL
           EXDRYFT(1,CURIYR) = EXDRYFT(1,CURIYR) + DRYWELLL48(1,R,K)
           DVDRYFT(1,CURIYR) = DVDRYFT(1,CURIYR) + DRYWELLL48(2,R,K)
        ENDDO

      ENDDO     ! END LOOP OVER L48 REGIONS

      EXSPEND(2,curiyr) = EXSPEND(1,curiyr)
      if (EXGASFT(1,CURIYR)+DVGASFT(1,CURIYR).GT.0.0)  &
         EXSPEND(1,CURIYR) = EXSPEND(1,CURIYR)/(EXGASFT(1,CURIYR)+DVGASFT(1,CURIYR))
!        EXSPEND(1,CURIYR) = EXSPEND(1,CURIYR)/(sum(resadl48(1:6,3:7)))
!        if (curiyr+1989.eq.2009) exspend(1,curiyr) = 1000.
         EXSPEND(3,CURIYR) = n_cstpermcf(curiyr)
         if (curiyr+1989.le.2009) exspend(3,curiyr) = 2.10
      WRITE(bugout,*) 'exspend2', curiyr+1989, exspend(1,curiyr), it_wop(curiyr,1), exspend(3,curiyr)

      DO R=1,7   ! CO2 EOR WELLS
         DO K=1,13
           DVOILFT(1,CURIYR) = DVOILFT(1,CURIYR)
         ENDDO
      ENDDO



      DO R=1,OFFRGN
!  EXP & DEV SUCCESSFUL OIL WELLS OFFSHORE
        EXOILFT(2,CURIYR) = EXOILFT(2,CURIYR) + sucWELLOFF(1,R,1)
        DVOILFT(2,CURIYR) = DVOILFT(2,CURIYR) + sucWELLOFF(2,R,1)

!  EXP & DEV SUCCESSFUL GAS WELLS OFFSHORE
        EXGASFT(2,CURIYR) = EXGASFT(2,CURIYR) + sucWELLOFF(1,R,2)
        DVGASFT(2,CURIYR) = DVGASFT(2,CURIYR) + sucWELLOFF(2,R,2)

!  EXP & DEV DRY WELLS OFFSHORE
        DO K=1,OFFFUEL
           EXDRYFT(2,CURIYR) = EXDRYFT(2,CURIYR) + DRYWELLOFF(1,R,K)
           DVDRYFT(2,CURIYR) = DVDRYFT(2,CURIYR) + DRYWELLOFF(2,R,K)
        ENDDO

      ENDDO     ! END LOOP OVER OFFSHORE REGIONS

!  TEMPORARILY ADJUST 1996 and 1997 TOTAL WELLS
      IF (CURIYR+1989.EQ.2020) THEN
        DO R=1,2
          EXOILFT(R,CURIYR) = EXOILFT(R,CURIYR)*welbench2
          DVOILFT(R,CURIYR) = DVOILFT(R,CURIYR)*welbench2
          EXGASFT(R,CURIYR) = EXGASFT(R,CURIYR)*welbench2
          DVGASFT(R,CURIYR) = DVGASFT(R,CURIYR)*welbench2
          EXDRYFT(R,CURIYR) = EXDRYFT(R,CURIYR)*welbench2
          DVDRYFT(R,CURIYR) = DVDRYFT(R,CURIYR)*welbench2
        ENDDO
      ENDIF
!     IF (CURIYR+1989.GT.1997) THEN
!       DO R=1,2
!         EXOILFT(R,CURIYR) = EXOILFT(R,CURIYR)* &
!            (WELBENCH+(1-WELBENCH)*(YRFADE/5))
!         DVOILFT(R,CURIYR) = DVOILFT(R,CURIYR)* &
!            (WELBENCH+(1-WELBENCH)*(YRFADE/5))
!         EXGASFT(R,CURIYR) = EXGASFT(R,CURIYR)* &
!            (WELBENCH+(1-WELBENCH)*(YRFADE/5))
!         DVGASFT(R,CURIYR) = DVGASFT(R,CURIYR)* &
!            (WELBENCH+(1-WELBENCH)*(YRFADE/5))
!         EXDRYFT(R,CURIYR) = EXDRYFT(R,CURIYR)* &
!            (WELBENCH+(1-WELBENCH)*(YRFADE/5))
!         DVDRYFT(R,CURIYR) = DVDRYFT(R,CURIYR)* &
!            (WELBENCH+(1-WELBENCH)*(YRFADE/5))
!       ENDDO
!     ENDIF

!  SET ALL ALASKA WELL VARIABLES TO 0.0 (9/22/93 PJ3)
      EXOILFT(3,CURIYR) = 0.0
      DVOILFT(3,CURIYR) = 0.0
      EXGASFT(3,CURIYR) = 0.0
      DVGASFT(3,CURIYR) = 0.0
      EXDRYFT(3,CURIYR) = 0.0
      DVDRYFT(3,CURIYR) = 0.0

!  EXP & DEV SUCCESSFUL OIL WELLS U.S.
      EXOILFT(4,CURIYR) = EXOILFT(1,CURIYR) + EXOILFT(2,CURIYR) &
                          + EXOILFT(3,CURIYR)
      DVOILFT(4,CURIYR) = DVOILFT(1,CURIYR) + DVOILFT(2,CURIYR) &
                          + DVOILFT(3,CURIYR)

!  EXP & DEV SUCCESSFUL GAS WELLS U.S.
      EXGASFT(4,CURIYR) = EXGASFT(1,CURIYR) + EXGASFT(2,CURIYR) &
                          + EXGASFT(3,CURIYR)
      DVGASFT(4,CURIYR) = DVGASFT(1,CURIYR) + DVGASFT(2,CURIYR) &
                          + DVGASFT(3,CURIYR)

!  EXP & DEV DRY WELLS U.S.
      EXDRYFT(4,CURIYR) = EXDRYFT(1,CURIYR) + EXDRYFT(2,CURIYR) &
                          + EXDRYFT(3,CURIYR)
      DVDRYFT(4,CURIYR) = DVDRYFT(1,CURIYR) + DVDRYFT(2,CURIYR) &
                          + DVDRYFT(3,CURIYR)

!  TOTAL EXPLORATORY WELLS LOWER 48
      EXFTAGE(1,CURIYR) = EXOILFT(1,CURIYR) + EXGASFT(1,CURIYR) &
                          + EXDRYFT(1,CURIYR)

!  TOTAL EXPLORATORY WELLS OFFSHORE
      EXFTAGE(2,CURIYR) = EXOILFT(2,CURIYR) + EXGASFT(2,CURIYR) &
                          + EXDRYFT(2,CURIYR)

!  TOTAL EXPLORATORY WELLS ALASKA
      EXFTAGE(3,CURIYR) = 0.0

!  TOTAL EXPLORATORY WELLS U.S.
      EXFTAGE(4,CURIYR) = EXOILFT(4,CURIYR) + EXGASFT(4,CURIYR) &
                          + EXDRYFT(4,CURIYR)

!  TOTAL DEVELOPMENT WELLS LOWER48
      DVFTAGE(1,CURIYR) = DVOILFT(1,CURIYR) + DVGASFT(1,CURIYR) &
                          + DVDRYFT(1,CURIYR)

!  TOTAL DEVELOPMENT WELLS OFFSHORE
      DVFTAGE(2,CURIYR) = DVOILFT(2,CURIYR) + DVGASFT(2,CURIYR) &
                          + DVDRYFT(2,CURIYR)

!  TOTAL DEVELOPMENT WELLS ALASKA
      DVFTAGE(3,CURIYR) = 0.0

!  TOTAL DEVELOPMENT WELLS U.S.
      DVFTAGE(4,CURIYR) = DVOILFT(4,CURIYR) + DVGASFT(4,CURIYR) &
                          + DVDRYFT(4,CURIYR)

!  CALL ROUTINE TO CALCULATE NUMBER OF JOBS

      CALL OGREP_JOBS

      RETURN
      END

!*******************************************************************

      SUBROUTINE OGREP_JOBS
      IMPLICIT NONE

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmout'

      INTEGER*4 I,J         ! INDICES
      INTEGER*4 NJOBHIS     ! NUMBER OF HISTORICAL YEARS OF JOBS DATA (MUST BE > 2)
      PARAMETER (NJOBHIS=27)

      REAL*4 A1, A2, A3
      REAL*4 JOBS(NJOBHIS)  ! # OF JOBS IN BLS CATEGORIES NAICS (211+213)
      DATA JOBS/377.7,375.6,338.0,333.9,321.2,305.7,307.1,321.9,321.9,283.1, &
                295.4,313.7,301.6,300.0,317.9,349.3,399.5,440.1,483.5,435.4, &
                450.5,521.0,577.8,601.0,631.1,562.6,445.5/
!     DATA A1/0.003157/
!     DATA A2/-7.04E-05/
!     DATA A3/0.865767/
      DATA A1/152.2320/
      DATA A2/0.006355/
      DATA A3/0.086729/


!  THIS ROUTINE CALCULATES & RPTS # OF OIL AND GAS EXTRACTION JOBS
!     ONLY PERFORM CALCULATIONS FOR FORECAST YEARS

      IF (CURIYR.LE.NJOBHIS) THEN
        OGJOBS(CURIYR) = JOBS(CURIYR)
      ELSE
!       OGJOBS(CURIYR) = ((OGNOWELL(CURIYR)/1000.)**0.200516) * &
!          ((OGNOWELL(CURIYR-1)/1000.)**0.141466) * &
!          EXP(37.6745+(-0.016534*(FLOAT(BASEYR+CURIYR-1))))    ! COMBINE CONST. AND TREND TERM
!       OGJOBS(CURIYR) = A1*OGNOWELL(CURIYR) + A2*(CURIYR+BASEYR-1) + A3*OGJOBS(CURIYR-1)
        OGJOBS(CURIYR) = A1 + A2*OGNOWELL(CURIYR) + A3*(CURIYR+BASEYR-1)
      ENDIF

      IF (CURIYR.EQ.LASTYR) THEN
       WRITE(BUGOUT,*) ' NUMBER OF OIL & GAS EXTRACTION JOBS', &
                       ' (1000s OF JOBS) '
        DO I=1,LASTYR
          J = I + BASEYR - 1
          WRITE(BUGOUT,1000) J,OGJOBS(I), OGNOWELL(I)/1000.
 1000      FORMAT(1X,' JOBS,WELLS ',I4,F10.3,F10.3)
        ENDDO
      ENDIF

      RETURN
      END

!*******************************************************************

      SUBROUTINE OGREP_IER
      IMPLICIT NONE

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include'ogsmbfw'      ! ogsm system variables
      include'ogsml48'      ! lower 48 variables
      include'ogsmoff'      ! offshore variables
      include'ogsmak'       ! aogss variables
      include'ogsmout'


      IF (PRTDBGL.EQ.1 .AND. OGREPORT(2).GT.0) THEN

      WRITE(SYSOUT,*)
      WRITE(SYSOUT,*)

!  THIS REPORT IS FOR THE LOWER48 SUBMODULE.

      WRITE(SYSOUT,*) '*****************************************'
      WRITE(SYSOUT,*) '**** L O W E R   4 8   S T A T E S   ****'
      WRITE(SYSOUT,*) '*****************************************'
      WRITE(SYSOUT,*)'         ***', CURIYR+1989,'     ***'
      WRITE(SYSOUT,*)
!  PRODUCTION
      WRITE(SYSOUT,*)
      WRITE(SYSOUT,*)' ***  PRODUCTION *** '
      WRITE(SYSOUT,*)'            OIL UNITS = MMB;   GAS UNITS = BCF'
      WRITE(SYSOUT,725)
      WRITE(SYSOUT,705) (R,(PRDL48(R,K),K=1,L48FUEL),R=1,L48RGN)
!  RESERVES
      WRITE(SYSOUT,*)
      WRITE(SYSOUT,*) ' ***  BEGINNING-OF-YEAR RESERVES  ***'
      WRITE(SYSOUT,*)'            OIL UNITS = MMB;   GAS UNITS = BCF'
      WRITE(SYSOUT,725)
      WRITE(SYSOUT,705) (R,(RESBOYL48(R,K),K=1,L48FUEL),R=1,L48RGN)
!  PRICES
      WRITE(SYSOUT,*)
      WRITE(SYSOUT,*) ' ***  LOWER48 PRICE VECTOR  ($87) ***'
      WRITE(SYSOUT,715)
      WRITE(SYSOUT,710) (R,(OGPRCL48(R,K,1),K=1,L48FUEL),R=1,L48RGN)
!  WELLS
      WRITE(SYSOUT,*)
      WRITE(SYSOUT,*) ' ***  EXPLORATORY WELLS LOWER 48'
      WRITE(SYSOUT,725)
      WRITE(SYSOUT,720) (R,(WELLSL48(1,R,K),K=1,L48FUEL),R=1,L48RGN)

      WRITE(SYSOUT,*)
      WRITE(SYSOUT,*) ' ***  DEVELOPMENTAL WELLS LOWER 48'
      WRITE(SYSOUT,725)
      WRITE (SYSOUT,720) (R,(WELLSL48(2,R,K),K=1,L48FUEL),R=1,L48RGN)

!  THIS REPORT IS FOR THE OFFSHORE SUBMODULE.

      WRITE(SYSOUT,700)
      WRITE(SYSOUT,*) '*****************************'
      WRITE(SYSOUT,*) '****   O F F S H O R E   ****'
      WRITE(SYSOUT,*) '*****************************'
      WRITE(SYSOUT,*)'   ***', CURIYR+1989,'     *** '
      WRITE(SYSOUT,*)
!  PRODUCTION
      WRITE(SYSOUT,*)
      WRITE(SYSOUT,*)' ***  PRODUCTION  ***'
      WRITE(SYSOUT,*)'            OIL UNITS = MMB;   GAS UNITS = BCF'
      WRITE(SYSOUT,740)
      WRITE(SYSOUT,745)
      WRITE(SYSOUT,735) (R,(OGPRODOFF(R,K),K=1,OFFFUEL),R=1,OFFRGN)
!  RESERVES
      WRITE(SYSOUT,*)
      WRITE(SYSOUT,*) ' ***  BEGINNING-OF-YEAR RESERVES  ***'
      WRITE(SYSOUT,*)'            OIL UNITS = MMB;   GAS UNITS = BCF'
      WRITE(SYSOUT,740)
      WRITE(SYSOUT,745)
      WRITE(SYSOUT,735) (R,(RESBOYOFF(R,K),K=1,OFFFUEL),R=1,OFFRGN)
! PRICES
      WRITE(SYSOUT,*)
      WRITE(SYSOUT,*) ' ***  OFFSHORE PRICE VECTOR  ($87)  ***'
      WRITE(SYSOUT,740)
      WRITE(SYSOUT,745)
      WRITE(SYSOUT,733) (R,(OGPRCOFF(R,K,1),K=1,OFFFUEL),R=1,OFFRGN)
!  WELLS
      WRITE(SYSOUT,*)
      WRITE(SYSOUT,*) ' ***  EXPLORATORY WELLS LOWER 48  ***'
      WRITE(SYSOUT,740)
      WRITE(SYSOUT,745)
      WRITE(SYSOUT,735) (R,(WELLSOFF(1,R,K),K=1,OFFFUEL),R=1,OFFRGN)
      WRITE(SYSOUT,*)
      WRITE(SYSOUT,*) ' ***  DEVELOPMENTAL WELLS LOWER 48  ***'
      WRITE(SYSOUT,740)
      WRITE(SYSOUT,745)
      WRITE(SYSOUT,735) (R,(WELLSOFF(2,R,K),K=1,OFFFUEL),R=1,OFFRGN)

!  THIS REPORT IS FOR THE ALASKA SUBMODULE.

      WRITE(SYSOUT,700)
      WRITE(SYSOUT,*) '*************************'
      WRITE(SYSOUT,*) '****   A L A S K A   ****'
      WRITE(SYSOUT,*) '*************************'
      WRITE(SYSOUT,*)' ***', CURIYR+1989,'     ***'
      WRITE(SYSOUT,*)
!  CRUDE OIL PRODUCTION
      WRITE(SYSOUT,*)
      WRITE(SYSOUT,*) '***  CRUDE OIL PRODUCTION  ***'
      WRITE(SYSOUT,*)'            OIL UNITS = MMB'
      WRITE(SYSOUT,750)
      WRITE(SYSOUT,755)
      WRITE(SYSOUT,760) (OGPRCOAK(R,CURIYR),R=1,AKRGN)
!  CRUDE OIL RESERVES
      WRITE(SYSOUT,*)
      WRITE(SYSOUT,*) '***  CRUDE OIL RESERVES  ***'
      WRITE(SYSOUT,*)'            OIL UNITS = MMB'
      WRITE(SYSOUT,750)
      WRITE(SYSOUT,755)
      WRITE(SYSOUT,760) (BOYRESCOAK(R,CURIYR),R=1,AKRGN)
!  PRICES
      WRITE(SYSOUT,*)
      WRITE(SYSOUT,*) '***  ALASKA PRICE VECTOR  ($87)  ***'
      WRITE(SYSOUT,750)
      WRITE(SYSOUT,745)
      WRITE(SYSOUT,730) (R,(OGPRCAK(R,K,1),K=1,AKFUEL),R=1,AKRGN)

!  THIS REPORT IS FOR THE EOR SUBMODULE.

      WRITE(SYSOUT,700)
      WRITE(SYSOUT,*) '******************************************'
      WRITE(SYSOUT,*) '****   E O R  (ENHANCED OIL RECOVERY)  ***'
      WRITE(SYSOUT,*) '******************************************'
      WRITE(SYSOUT,*)'          ***', CURIYR+1989,'     ***'
      WRITE(SYSOUT,*)
      WRITE(SYSOUT,*)
!  PRODUCTION
      WRITE(SYSOUT,*)
      WRITE(SYSOUT,*) '***  CRUDE OIL PRODUCTION  *** '
      WRITE(SYSOUT,*)'            OIL UNITS = MMB;'
      WRITE(SYSOUT,790)
      WRITE(SYSOUT,785)
      WRITE(SYSOUT,795) (OGQEORPR(R,CURIYR),R=1,L48RGN)

      ENDIF   ! END IF REPORTING FLAGS ARE SET

!  -- OUTPUT FORMATS
  700 FORMAT(/)
  705 FORMAT(4X,I1,<L48RGN>F17.1,/,4X,I1,<L48RGN>F17.1,/,4X,I1,<L48RGN>F17.1,/,4X,I1, &
             <L48RGN>F17.1,/,4X,I1,<L48RGN>F17.1,/,4X,I1,<L48RGN>F17.1,/)
  710 FORMAT(I4,4X,<L48RGN>F10.2,/,I4,4X,<L48RGN>F10.2,/,I4,4X,<L48RGN>F10.2,/, &
                I4,4X,<L48RGN>F10.2,/,I4,4X,<L48RGN>F10.2,/,I4,4X,<L48RGN>F10.2,/)
  715 FORMAT(12X,'------  CONVENTIONAL  ------',2X, &
           '  ---- UNCONVENTIONAL ----',/, &
           'REGION',7X,'CRUDE',5X,'SHALLOW',4X,'DEEP',6X,'TIGHT', &
           3X,'DEVONIAN',3X,'COALBED',/,13X,'OIL',8X,'GAS',7X,'GAS', &
           7X,'SANDS',5X,'SHALE',4X,'METHANE')
  720 FORMAT(4X,I1,<L48RGN>F17.2,/,4X,I1,<L48RGN>F17.2,/,4X,I1,<L48RGN>F17.2,/, &
             4X,I1,<L48RGN>F17.2,/,4X,I1,<L48RGN>F17.2,/,4X,I1,<L48RGN>F17.2)
  725 FORMAT(12X,'----------------  CONVENTIONAL  -----------------', &
             '  ---------------  UNCONVENTIONAL  ----------------',/, &
             ' REGION',7X,'CRUDE OIL',7X,'SHALLOW GAS',8X,'DEEP GAS', &
             7X,'TIGHT SANDS',4X,'DEVONIAN SHALE',4X,'COALBED METHANE')

!  -- OFFSHORE FORMATS
  730 FORMAT(I4,7X,2F10.2,/,I4,7X,2F10.2,/,I4,7X,2F10.2,/)
  733 FORMAT(4X,I1,2F15.2,/,4X,I1,2F15.2,/,4X,I1,2F15.2,/, &
             4X,I1,2F15.2,/,4X,I1,2F15.2,/,4X,I1,2F15.2,/, &
             4X,I1,2F15.2,/,4X,I1,2F15.2)
  735 FORMAT(4X,I1,2F15.1,/,4X,I1,2F15.1,/,4X,I1,2F15.1,/,4X,I1, &
             2F15.1,/,4X,I1,2F15.1,/,4X,I1,2F15.1,/,4X,I1,2F15.1, &
             /,4X,I1,2F15.1,/)
  740 FORMAT(5X,'------  O F F S H O R E  ------')
  745 FORMAT('REGION',7X,'CRUDE OIL',10X,'GAS')
! ALASKA
  750 FORMAT(4X,'------------ A L A S K A -------------')
  755 FORMAT(/,4X,'OFFSHORE',3X,'ONSHORE',5X,'SOUTH')
  760 FORMAT(2X,3F10.2,/)

! -- ENHANCED OIL RECOVERY
  785 FORMAT(9X,'NORTHEAST',5X,'GULF COAST',5X,'MID-CONT.', &
            5X,'SOUTHWEST',5X,'ROCKY MTNS.',5X,'WEST COAST',5X,'N GREAT PLAINS')
  790 FORMAT(2X,'------   E N H A N C E D  O I L  R E C O V E R Y &
            -------')
  795 FORMAT(1X,<L48RGN>F15.0,/)


      RETURN
      END


!***************************************************************************************************
       SUBROUTINE RPT_CO2
       ! YA 9.17.10 restructures/splits/adds sources

!  write the annual co2 constraint file

       implicit none

       include'parametr'     ! nems dimension parameters
       include'ncntrl'       ! nems control variables
       include'ogsmparm'     ! ogsm parameter file
       include 'ogsmbfw'
       include 'ogsml48'
       include 'ogsmeor'
       include 'ctus'
       
       INTEGER IR,iyr,isrc,lyr,jyr
       REAL DUM_CO2_NAT(MAX_YR)
       REAL DUM_CO2_USE(MAX_YR)
       REAL DUM_CO2_REM(MAX_YR)
       REAL DUM_CO2_EOR(MAX_YR)
       REAL dum_regco2_rem(max_reg-1,MAX_src,max_yr)     ! 07.30.10 YA changed max_reg (8) to max_reg-1 (7)
       REAL regbse(max_reg-1,max_yr)                     ! 07.30.10 YA changed max_reg (8) to max_reg-1 (7)
       REAL reguse(max_reg-1,max_yr)                     ! 07.30.10 YA changed max_reg (8) to max_reg-1 (7)
       REAL regrem(max_reg-1,max_yr)                     ! 07.30.10 YA changed max_reg (8) to max_reg-1 (7)
       REAL tmp_availco2(max_reg-1,max_src,max_yr)       ! 07.30.10 YA changed max_reg (8) to max_reg-1 (7)
       REAL scr_avlco2(max_src,max_yr)
       REAL scr_useco2(max_src,max_yr)
       REAL scr_eorprd(max_src,max_yr)


       CHARACTER*30 region_label(max_reg-1)             ! 07.30.10 YA changed max_reg (8) to max_reg-1 (7)
       CHARACTER*40 source_label(max_src)

       region_label(1) = 'EAST COAST           '
       region_label(2) = 'GULF COAST           '
       region_label(3) = 'MIDCONTINENT         '
       region_label(4) = 'SOUTHWEST            '
       region_label(5) = 'ROCKY MOUNTAINS      '
       region_label(6) = 'WEST COAST           '
       region_label(7) = 'NORTHERN GREAT PLAINS'

       source_label(1) = 'HYDROGEN                             '
       source_label(2) = 'AMMONIA                              '
       source_label(3) = 'ETHANOL                              '
       source_label(4) = 'NATURAL                              '
       source_label(5) = 'CEMENT                               '
       source_label(6) = 'HYDROGEN REFINERIES                  '          ! 9.17.10 YA
       source_label(7) = 'PLANNED POWER PLANTS                 '          ! 9.17.10 YA
       source_label(8) = 'POWER PLANTS                         '          ! 9.17.10 YA
       source_label(9) = 'NATURAL GAS PROCESSING               '          ! 7.27.10 YA 
       source_label(10)= 'PLANNED XTL                          '          ! 10.01.10 YA 
       source_label(11)= 'XTL                                  '          ! 10.01.10 YA 
       source_label(12)= 'NEW SOURCE 1                         '          ! 7.29.10 YA 
       source_label(13)= 'NEW SOURCE 2                         '          ! 8.12.10 YA 

! determine index of lastyr of projection period
       lyr = lastyr - (l48b4yr-baseyr)

       dum_co2_nat = 0.0
       dum_co2_use = 0.0
       dum_co2_rem = 0.0
       dum_co2_eor = 0.0
       scr_avlco2 = 0.
       scr_useco2 = 0.
       scr_eorprd = 0.

!determine U.S. level CO2 availability, use, and remaining
       DO IR=1,MAX_REG-1                      
         do isrc = 1,max_src
          DO iyr=1,lyr
            jyr = iyr + l48b4yr - baseyr
            if (jyr.gt.lastyr) jyr = lastyr
              !if (ir.le.l48rgn) 
			  !!!!!!!!!!!!!!!! Matt
			  DUM_CO2_EOR(iyr) = DUM_CO2_EOR(iyr) + &
               EORProduction(IR,isrc,jyr)
			  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              ! YA 9.17.10 National available as a function of time (summed over regions, sources)
              DUM_CO2_NAT(iyr) = DUM_CO2_NAT(iyr) + &
               bse_availco2(IR,isrc,iyr)
              ! YA 9.17.10 National used as a function of time (summed over regions, sources)
              DUM_CO2_USE(iyr) = DUM_CO2_USE(iyr) + &
               use_availco2(IR,isrc,iyr)
              ! YA 9.17.10 National remaining = available - used as a function of time (summed over regions, sources)
              DUM_CO2_REM(iyr) = DUM_CO2_REM(iyr) + &
               bse_availco2(IR,isrc,iyr)-use_availco2(IR,isrc,iyr)
              ! YA 9.17.10 Regional remaining - function of region, source, time (NOT a sum)
              dum_regco2_rem(IR,isrc,iyr)= &
               bse_availco2(IR,isrc,iyr)-use_availco2(IR,isrc,iyr)
              ! YA 9.17.10 Used - function of source, time (summed over regions)
              scr_useco2(isrc,iyr) = scr_useco2(isrc,iyr) + use_availco2(IR,isrc,iyr)
              ! YA 9.17.10 Available - function of source, time (summed over regions)
              scr_avlco2(isrc,iyr) = scr_avlco2(isrc,iyr) + bse_availco2(IR,isrc,iyr)
              !if (ir.le.l48rgn) 
			  !!!!!!!!!!!!!matt; YA 9.17.10 EOR Production as a function of source, time (summed over regions)
			  scr_eorprd(isrc,iyr) = scr_eorprd(isrc,iyr) + EORProduction(IR,isrc,jyr)
			  !!!!!!!!!!!!!!!matt
          END DO
         end do
       END DO

       do isrc = 1,max_src
         do IR = 1,MAX_reg-1               ! YA 8.11.10
           do iyr = 1,lyr
              regbse(IR,iyr) = regbse(IR,iyr)+ &
               bse_availco2(IR,isrc,iyr)
              reguse(IR,iyr) = reguse(IR,iyr)+ &
               use_availco2(IR,isrc,iyr)
              regrem(IR,iyr) = regrem(IR,iyr)+ &
               bse_availco2(IR,isrc,iyr)-use_availco2(IR,isrc,iyr)
           end do
         end do
       end do

       WRITE (varout,*)
       WRITE (varout,'(1X,"NATIONAL CO2 CONSTRAINT (BCF)")')
       WRITE (varout,*)
       WRITE (varout,404) (iyr+(L48B4YR-1),iyr=1,lyr)
       WRITE (varout,405) ('==========',iyr=1,lyr)
       DO IR=1,MAX_REG-1
          WRITE (varout,400) IR,(regbse(IR,iyr),iyr=1,lyr)
          WRITE (varout,401) IR,(reguse(IR,iyr),iyr=1,lyr)
          WRITE (varout,402) IR,(regrem(IR,iyr),iyr=1,lyr)
          WRITE (varout,*)
       END DO

       WRITE (varout,*)
       WRITE (varout,453) (DUM_CO2_EOR(iyr),iyr=1,lyr)
       WRITE (varout,450) (DUM_CO2_NAT(iyr),iyr=1,lyr)
       WRITE (varout,451) (DUM_CO2_USE(iyr),iyr=1,lyr)
       WRITE (varout,452) (DUM_CO2_REM(iyr),iyr=1,lyr)
       WRITE (varout,*)
       WRITE (varout,*)

 1807  format (a40)
 1808  format (a40,<lyr>(2x,f12.2))

405    FORMAT (T40,2x,<lyr>(3X,A10,1X))
404    FORMAT (40X,<lyr>(2X,I12))
400    FORMAT (2X,I3,1X,'INITIAL',5X,22x,<lyr>(2X,F12.2))
401    FORMAT (2X,I3,1X,'USED   ',5X,22x,<lyr>(2X,F12.2))
402    FORMAT (2X,I3,1X,'REMAIN ',5X,22x,<lyr>(2X,F12.2))
450    FORMAT (1X,'NATIONAL(INITIAL)',22x,<lyr>(2X,F12.2))
451    FORMAT (1X,'NATIONAL(USED)   ',22x,<lyr>(2X,F12.2))
452    FORMAT (1X,'NATIONAL(REMAIN) ',22x,<lyr>(2X,F12.2))
453    FORMAT (1X,'NATIONAL EOR PROD',22x,<lyr>(2X,F12.2))

! Write the source specific CO2 volumes available
       write (varout,1807) 'NATIONAL SOURCE SPECIFIC VOLUMES'
         do isrc = 1,max_src
            write (varout,3807) source_label(isrc), &
              (iyr,iyr=l48b4yr,lastyr+baseyr-1)
            write (varout,2807) 'OIL PRODUCTION (MBBL)', &
              (scr_eorprd(isrc,iyr),iyr=1,lyr)
            WRITE (varout,2807) 'CO2 USED       (MMCF)', &
              (scr_useco2(isrc,iyr),iyr=1,lyr)
            WRITE (varout,2807) 'CO2 AVAILABLE  (MMCF)', &
              (scr_avlco2(isrc,iyr),iyr=1,lyr)
            write (varout,*)
         end do
         write (varout,*)
         write (varout,*)

! Write the regional source specific CO2 volumes available
       write (varout,1807) 'REGIONAL SOURCE SPECIFIC VOLUMES'
       do isrc = 1,max_src
          write (varout,*)
          write (varout,*)
          write (varout,1807) source_label(isrc)
          write (varout,1807) 'CO2 AVAILABLE (BCF/YR)'
          do IR = 1,max_reg-1
            write (varout,1808) region_label(IR), &
             (bse_availco2(IR,isrc,iyr),iyr=1,lyr)
          end do
       end do

       write (varout,1807) 'REGIONAL SOURCE SPECIFIC VOLUMES'
       do isrc = 1,max_src
          write (varout,*)
          write (varout,*)
          write (varout,1807) source_label(isrc)
          write (varout,1807) 'CO2 USED      (BCF/YR)'
          do IR = 1,max_reg-1
            write (varout,1808) region_label(IR), &
             (use_availco2(IR,isrc,iyr),iyr=1,lyr)
          end do
       end do

       write (varout,1807) 'REGIONAL SOURCE SPECIFIC VOLUMES'
       do isrc = 1,max_src
          write (varout,*)
          write (varout,*)
          write (varout,1807) source_label(isrc)
          write (varout,1807) 'CO2 REMAINING (BCF/YR)'
          do IR = 1,max_reg-1
            write (varout,1808) region_label(IR), &
             (nat_availco2(IR,isrc,iyr),iyr=1,lyr)
          end do
          write (varout,*)
       end do

       do isrc = 1,max_src
         DO iyr=1,lyr
           do IR = 1,max_reg-1
             tmp_availco2(IR,isrc,iyr) = bse_availco2(IR,isrc,iyr)
           end do
 !matt          tmp_availco2(5,isrc,iyr) = bse_availco2(5,isrc,iyr) + bse_availco2(7,isrc,iyr)
         end do
       end do

       do IR = 1,l48rgn 	          ! YA 9.21.10
         write (varout,1807) region_label(IR)
         do isrc = 1,max_src
            write (varout,3807) source_label(isrc), &
              (iyr,iyr=l48b4yr,lastyr+baseyr-1)
            write (varout,2807) 'OIL PRODUCTION (MBBL)', &
              (EORProduction(IR,isrc,iyr),iyr=l48b4yr-baseyr+1,lastyr)
            WRITE (varout,2807) 'GAS PRODUCTION (MMCF)', &
              (GasProduction(IR,isrc,iyr),iyr=l48b4yr-baseyr+1,lastyr)
            WRITE (varout,2807) 'CO2 INJECTED   (MMCF)', &
              (CO2Injected(IR,isrc,iyr),iyr=l48b4yr-baseyr+1,lastyr)
            WRITE (varout,2807) 'CO2 RECYCLED   (MMCF)', &
              (CO2Recycled(IR,isrc,iyr),iyr=l48b4yr-baseyr+1,lastyr)
            WRITE (varout,2807) 'CO2 PURCHASED  (MMCF)', &
              (CO2Purchased(IR,isrc,iyr),iyr=l48b4yr-baseyr+1,lastyr)
            WRITE (varout,2807) 'CO2 AVAILABLE  (MMCF)', &
              (tmp_availco2(IR,isrc,iyr+baseyr-l48b4yr)*1000.0,iyr=l48b4yr-baseyr+1,lastyr)
            write (varout,*)
         end do
         write (varout,*)
       end do

 2807  format (a21,20x,<lyr>(f12.3,3x))
 3807  format (a40,1x,<lyr>(8x,i4,3x))


       RETURN
       END SUBROUTINE
!***************************************************************************************************
!     Last change:  MC   22 Mar 2009    1:07 pm
        subroutine write_output(jres)

!  THIS SUBROUTINE CREATES THE RESERVOIR FILE USED BY THE AGGREGATION ROUTINES.  IT
!  IS ONLY APPLIED TO THE TIMED RESERVOIRS.

        implicit none

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
        include'ogsmbfw'      ! ogsm system variables
        include'ogsml48'      ! lower 48 variables
        include 'ogsmugr'

        INTEGER jres                                    !timed reservoir number
        INTEGER iy, ac

!  COUNT THE NUMBER OF TIMED RESERVOIRS
        njres = njres + 1
        ac = apcode(jres)

!  LINE ONE CONTAINS THE IDENTIFIERS FOR THE RESERVOIR
!  YEAR  RESID  API  DEPTH  SULFUR  CO2SOURCE  BASIN  PLAY  REGION  RESFLAG
        if (PRTDBGL.EQ.1) then
          IF ((AC.ge.3.and.AC.le.10).or.AC.ge.16)  &
             WRITE (ogbug1,300) itimeyr+l48b4yr-1,jres,aRESID(jres),  &
             aPLaY_cde(jres),aREGion(jres),aresflag(jres),project_npv(jres)
 300    format ('TIMED PROJECT: ',I4,I8,A12,I12,I4,I4,f12.2)

          if (ogreport(9).eq.1) then
             WRITE (dbout) itimeyr,jres,aRESID(jres),apcode(jres), aAPI(jres),  &
                aDEPTH(jres),aSULFOIL(jres),eco2code(jres),aprov(jres),  &
                aPLaY_cde(jres),aREGion(jres),aresflag(jres),  &
                aresacc(jres),arrc(jres), acrdtype(jres), acrdheat(jres)


             WRITE(dbout) (eSUMP(jres,iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eXPATN(jres,iy),iy=1,lastyr+baseyr-l48b4yr)

             WRITE(dbout) (ePRODoil(jres,iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eprodgas(jres,iy),iy=1,lastyr+baseyr-l48b4yr)

             WRITE(dbout) (eOILPRICE2(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eGASPRICE2(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eREMRES(jres,iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (egREMRES(jres,iy),iy=1,lastyr+baseyr-l48b4yr)

             WRITE(dbout) (eGROSSREV(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (adOILPRICE(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eGRAVadj(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eADJGROSS(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eROY(iy),iy=1,lastyr+baseyr-l48b4yr)

             WRITE(dbout) (eTOC(jres,iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eGAEXP(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eGACAP(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eINJcost(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (ePRODwat(jres,iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eWATINJ(jres,iy),iy=1,lastyr+baseyr-l48b4yr)

             WRITE(dbout) (eCO2COST(iy),iy=1,lastyr+baseyr-l48b4yr)

             WRITE(dbout) (esurfvol(jres,iy),iy=1,lastyr+baseyr-l48b4yr)

             WRITE(dbout) (eTORECY(jres,iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eTORECY_CST(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eFOAM(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eIGEN(iy),iy=1,lastyr+baseyr-l48b4yr)

             WRITE(dbout) (ePROC_OAM(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eOAM(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eAOAM(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eSTIM(iy),iy=1,lastyr+baseyr-l48b4yr)

             WRITE(dbout) (eEXIST_EOAM(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eNEW_EOAM(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eEXIST_ECAP(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eNEW_ECAP(iy),iy=1,lastyr+baseyr-l48b4yr)

             WRITE(dbout) (eII(jres,iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eIIDRL(jres,iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eICAP2(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eINTCAP(iy),iy=1,lastyr+baseyr-l48b4yr)

             WRITE(dbout) (eTI(jres,iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eTIDRL(jres,iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eOTC(iy),iy=1,lastyr+baseyr-l48b4yr)

             WRITE(dbout) (eCOMP(iy),iy=1,lastyr+baseyr-l48b4yr)

             WRITE(dbout) (eTCI(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eTCIADJ(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eCAP_BASE(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eDEPR(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eAMOR(iy),iy=1,lastyr+baseyr-l48b4yr)

             WRITE(dbout) (edepggla(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eLA(jres,iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eGG(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eDEP_CRD(iy),iy=1,lastyr+baseyr-l48b4yr)

             WRITE(dbout) (eADGGLA(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eDGGLA(iy),iy=1,lastyr+baseyr-l48b4yr)

             WRITE(dbout) (eEGGLA(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (expLA(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (expGG(iy),iy=1,lastyr+baseyr-l48b4yr)

             WRITE(dbout) (eNETREV(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eSEV(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eTOC(jres,iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eDEPLET(iy),iy=1,lastyr+baseyr-l48b4yr)

             WRITE(dbout) (etaxinc(iy),iy=1,lastyr+baseyr-l48b4yr)

             WRITE(dbout) (eEORTCA(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eINTADD(iy),iy=1,lastyr+baseyr-l48b4yr)

             WRITE(dbout) (eGGLAadd(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eNIBT(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eSTTAX(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eAMINT(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eFEDTAX(iy),iy=1,lastyr+baseyr-l48b4yr)

             WRITE(dbout) (eFEDCR(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eNIAT(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eDEPLET(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eADGGLA(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (etcadd(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eATCF(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eDATCF(iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (eCATCF(iy),iy=1,lastyr+baseyr-l48b4yr)

             WRITE(dbout) (EINJDR(jres,iy)   ,iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (EPRODDR(jres,iy)  ,iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (EPRODWELL(jres,iy),iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (EINJWELL(jres,iy) ,iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (ESHUTIN(jres,iy)  ,iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (EDRYHOLE(jres,iy) ,iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (PATDEV(jres,iy,1) ,iy=1,lastyr+baseyr-l48b4yr)
             WRITE(dbout) (PATDEV(jres,iy,2) ,iy=1,lastyr+baseyr-l48b4yr)
          end if
        end if

        RETURN
        END SUBROUTINE

!***************************************************************
       SUBROUTINE WRITE_RPROD       

       implicit none

       include'parametr'     ! nems dimension parameters
       include'ncntrl'       ! nems control variables
       include'ogsmparm'     ! ogsm parameter file
       include'ogsmbfw'      ! ogsm system variables
       include 'ogsmugr'
       INCLUDE 'ogsml48'
       INCLUDE 'ogsmeor'

       REAL*4  tempgas(max_yr-1), histgas, rdpth, tempoil(max_yr-1), histoil
       REAL*4  tempwel(max_yr-1)

       INTEGER ires                                        !reservoir number

       INTEGER iwell                                       ! well class
       INTEGER ist                                         ! state number
       INTEGER iproc                                       ! process number
       INTEGER IR,irgn                                     ! OGSM region number
       INTEGER ifuel                                       ! fuel type
       INTEGER iplay                                       ! play number
       INTEGER irrc                                        ! rrc number
       INTEGER iacc                                        ! resource access category
       INTEGER iflag                                       ! resource flag
       INTEGER isrc                                        ! CO2 source
       INTEGER icrd                                        ! crude type
       INTEGER DIST                                        ! DISTRICTS FOR PMM GAS PROD
       INTEGER imc,iy
       INTEGER yr(43),y
       INTEGER hold_oPADD
       INTEGER dtimedyr

       INTEGER iyr,mprov,jyr,kyr,jmc,jcde,umc,ucde,myr                                 !mc change ver 7


       CHARACTER*2 st1                                     ! state abbreviation
       CHARACTER*1 well
       character*9 jmres,imres

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

        IF (itimeyr.ge.1) then
 
        yr(1)=l48b4yr
        DO y=2,max_yr-1
         yr(y)=yr(y-1)+1
        ENDDO
  
        write (OFILE3,1812)'TIMEDYR','RESID','STATE','PLAY','FUEL','CRUDETYPE','REG','RRC','PROC',&
            (yr(y),y=1,max_yr-1),(yr(y),y=1,max_yr-1)
        write (OFILE2,1814)'TIMEDYR','RESID','STATE','PLAY','FUEL','CRUDETYPE','REG','RRC','PROC',&
            'TOTPAT','SUMWEL',(yr(y),y=1,max_yr-1)
         
        DO ires=1,nres
         if (dtimed(ires)) then 
          read (dresid(ires)(10:11),'(i2)')apcode(ires)
          read (dresid(ires)(10:11),'(i2)') iproc
          iproc = iproc + 1
          read (dresid(ires)(3:4),'(a2)')   st1
          read (dresid(ires)(1:1),'(a1)')   well

          IR = dregion(ires)

          iplay = dplay_cde(ires)
          iacc  = dresacc(ires)
          irrc  = INT(drrc(ires))
          iflag = dresflag(ires)
          icrd  = dcrdtype(ires)
          rdpth = ddepth(ires)
          dtimedyr = 1

          call dist_match(st1,irrc,IR,dist,hold_oPADD)     ! for PMM

          IF(iproc.LT.4.OR.iproc.eq.7.OR.(IPROC.GE.9.AND.iproc.LE.11) &
              .OR.iproc.EQ.18)THEN
            ifuel=1                               ! FUELTYPE 1: CONVENTIONAL OIL
          ELSEIF((iproc.GE.4.AND.IPROC.LE.6).OR.IPROC.EQ.8)THEN
            ifuel=2                               ! FUELTYPE 2: EOR/ASR
          ELSEIF(iproc.EQ.12.OR.iproc.EQ.13.OR.iproc.EQ.17)THEN
            ifuel=4                               ! FUELTYPE 4: CONVENTIONAL DEEP GAS
          ELSEIF(iproc.EQ.14.OR.iproc.EQ.19.or.iproc.eq.23)THEN
            ifuel=5                               ! FUELTYPE 5: TIGHT GAS
          ELSEIF((iproc.EQ.15.OR.iproc.EQ.16.OR.iproc &
                 .EQ.20.OR.iproc.EQ.21 &
                 .or.iproc.eq.22).AND.(iflag.eq.3.or.iflag.eq.4))THEN
            ifuel=6                               ! FUELTYPE 6: GAS SHALES
          ELSEIF((iproc.EQ.15.OR.iproc.EQ.16.OR.iproc &
                 .EQ.20.OR.iproc.EQ.21 &
                 .or.iproc.eq.24).AND.(iflag.eq.1.or.iflag.eq.2))THEN
            ifuel=7                               ! FUELTYPE 7: COALBED METHANE
          ENDIF

            do j = 1,max_yr-1
              tempoil(j) = dprodoil(ires,j)
              tempgas(j) = dprodgas(ires,j)
            ENDDO

           write (OFILE3,1811) dtimedyr,dresid(ires),st1,iplay,ifuel,icrd,ir,irrc,iproc-1,&
           (tempoil(j),j=1,max_yr-1),(tempgas(j),j=1,max_yr-1)

        ENDIF
       ENDDO

 1811    format (i7,3x,a11,3x,a2,3x,i8,3x,i2,5x,i3,3x,i6,3x,i6,3x,i6, &
         2(3x,<max_yr-1>(3x,f12.3)))

 1812    format (a7,6x,a5,5x,a5,5x,a4,2x,a4,4x,a9,4x,a3,6x,a4,6x,a4,2(<max_yr-1>(3x,i12)))

 1813    format (i7,3x,a11,3x,a2,3x,i8,3x,i2,5x,i3,3x,i6,3x,i6,3x,i4,3x,f8.0,2x,f8.0,<max_yr-1>(3x,f12.3))
 1814    format (a7,6x,a5,5x,a5,5x,a4,2x,a4,4x,a9,4x,a3,6x,a3,6x,a4,6x,a6,4x,a6,<max_yr-1>(3x,i12))

        DO ires=1,nres
         if (timed(ires)) then 
          read (aresid(ires)(10:11),'(i2)')apcode(ires)
          read (aresid(ires)(10:11),'(i2)') iproc
          iproc = iproc + 1
          read (aresid(ires)(3:4),'(a2)')   st1
          read (aresid(ires)(1:1),'(a1)')   well

          IR = aregion(ires)

          iplay = aplay_cde(ires)
          iacc  = aresacc(ires)
          irrc  = INT(arrc(ires))
          iflag = aresflag(ires)
          icrd = acrdtype(ires)
          rdpth = adepth(ires)

          call dist_match(st1,irrc,IR,dist,hold_oPADD)     ! for PMM

          IF(iproc.LT.4.OR.iproc.eq.7.OR.(IPROC.GE.9.AND.iproc.LE.11) &
              .OR.iproc.EQ.18)THEN
            ifuel=1                               ! FUELTYPE 1: CONVENTIONAL OIL
          ELSEIF((iproc.GE.4.AND.IPROC.LE.6).OR.IPROC.EQ.8)THEN
            ifuel=2                               ! FUELTYPE 2: EOR/ASR
          ELSEIF(iproc.EQ.12.OR.iproc.EQ.13.OR.iproc.EQ.17)THEN
            ifuel=4                               ! FUELTYPE 4: CONVENTIONAL DEEP GAS
          ELSEIF(iproc.EQ.14.OR.iproc.EQ.19.or.iproc.eq.23)THEN
            ifuel=5                               ! FUELTYPE 5: TIGHT GAS
          ELSEIF((iproc.EQ.15.OR.iproc.EQ.16.OR.iproc &
                 .EQ.20.OR.iproc.EQ.21 &
                 .or.iproc.eq.22).AND.(iflag.eq.3.or.iflag.eq.4))THEN
            ifuel=6                               ! FUELTYPE 6: GAS SHALES
          ELSEIF((iproc.EQ.15.OR.iproc.EQ.16.OR.iproc &
                 .EQ.20.OR.iproc.EQ.21 &
                 .or.iproc.eq.24).AND.(iflag.eq.1.or.iflag.eq.2))THEN
            ifuel=7                               ! FUELTYPE 7: COALBED METHANE
          ENDIF

            do j = 1,max_yr-1
              tempgas(j) = 0.0
              tempoil(j) = 0.0
              tempwel(j) = 0.0
              IF(j.ge.timedyr(ires).and.j.le.max_yr) then
                tempoil(j) = eprodoil(ires,j-timedyr(ires)+1)
                tempgas(j) = eprodgas(ires,j-timedyr(ires)+1)
                tempwel(j) = eproddr(ires,j-timedyr(ires)+1)
              ENDIF
            ENDDO

            histgas = 0.
            histoil = 0.

           write (OFILE3,1811) timedyr(ires),aresid(ires),st1,iplay,ifuel,icrd,ir,irrc,iproc-1,&
           (tempoil(j),j=1,max_yr-1),(tempgas(j),j=1,max_yr-1)
           write (OFILE2,1813) timedyr(ires),aresid(ires),st1,iplay,ifuel,icrd,ir,irrc,iproc-1,&
           atotpat(ires,1),sum(tempwel(:)),(tempwel(j),j=1,max_yr-1)

       
        ENDIF
       ENDDO
       ENDIF

        RETURN
        END SUBROUTINE
