!-*- f90 -*-
!module calc_weights
!    contains
!******************************************************************
!*    Subroutine DOCVFACTS
!*
!******************************************************************!*******************************************************************
! DOCVFACTS:  this subroutine calculates some weighted average conversion factors

      !SUBROUTINE DOCVFACTS(DBDUMP)
	  SUBROUTINE DOCVFACTS
      IMPLICIT NONE

      include 'parametr'
      include 'ncntrl'
      include 'qblk'
      include 'mpblk'
      include 'intout'
      include 'indout'     ! used for INQLGHP only.  when switch to QPRIN, remove this
      include 'pmmrpt'
      include 'pmmout'
      include 'lfmmout'
      include 'ogsmout'
      include 'convfact'
      include 'tranrep'

      INTEGER HISTYR, ICV, IV
      COMMON /CVHISTYR/ HISTYR           ! last history year for conversion factors
      REAL API_TO_BTU
      EXTERNAL API_TO_BTU

!WRITE(*,*)"start DOCVFACTS()"
!WRITE(*,*)"DOCVFACTS(): CURIYR=",CURIYR
!WRITE(*,*)"DOCVFACTS(): HISTYR=",HISTYR
!WRITE(*,*)"DOCVFACTS(): DBDUMP=",DBDUMP
      IF (CURIYR .LE. HISTYR) THEN
          !WRITE(*,*)"DOCVFACTS(): (CURIYR .LE. HISTYR)if DBDUMP=1 return, DBDUMP=",DBDUMP
          IF (DBDUMP .EQ. 1) WRITE(ICNVRTOUT,'(A)')  "   Historical year.  Not updating conversion factors!"
          RETURN
      ENDIF
! Calculate overall petroleum conversion factor
      IF (DBDUMP .EQ. 1 .AND. CURITR .EQ. 1) &
          WRITE(ICNVRTOUT,'(A,I4,A,F6.3)')  &
        " Petroleum consumption conversion factor from file in year ", &
          CURIYR+1989, " is ",CFTPQ(CURIYR)
!  unfinished oils
      IF ((RFIPQAR3(MNUMPR,CURIYR,2)+RFIPQGO3(MNUMPR,CURIYR,2)+RFIPQMN3(MNUMPR,CURIYR,2)) .NE. 0.0) THEN
          CFIMUO(CURIYR) =(RFIPQAR3(MNUMPR,CURIYR,2)*CFAR3(CURIYR)+   &
                           RFIPQGO3(MNUMPR,CURIYR,2)*CFGO3(CURIYR)+   &
                           RFIPQMN3(MNUMPR,CURIYR,2)*CFMN3(CURIYR))/  &
                     (RFIPQAR3(MNUMPR,CURIYR,2)+RFIPQGO3(MNUMPR,CURIYR,2)+RFIPQMN3(MNUMPR,CURIYR,2))
      ELSE
          CFIMUO(CURIYR) = CFDSQ
      ENDIF
!WRITE(*,*)"DOCVFACTS(): CFE85Q(CURIYR)=",CFE85Q(CURIYR)
      CFE85Q(CURIYR) = ETHNE85 * CFETQ(CURIYR) + TRGNE85 * CFRBOB(CURIYR)
!  uncomment the following if we switch to OGSM heat contents.  (will also need lfreport change)
!     CFCRDLTSWT(CURIYR)   = OGCRDHEAT( 1,CURIYR)
!     CFCRDLTSOUR(CURIYR)  = OGCRDHEAT( 2,CURIYR)
!     CFCRDMD2SOUR(CURIYR) = OGCRDHEAT( 3,CURIYR)
!     CFCRDMDSOUR(CURIYR)  = OGCRDHEAT( 4,CURIYR)
!     CFCRDHVSWT(CURIYR)   = OGCRDHEAT( 5,CURIYR)
!     CFCRDHVSOUR(CURIYR)  = OGCRDHEAT( 6,CURIYR)
!     CFCRDCA(CURIYR)      = OGCRDHEAT( 7,CURIYR)
!     CFCRDSYN(CURIYR)     = OGCRDHEAT( 8,CURIYR)
!     CFCRDDILBIT(CURIYR)  = OGCRDHEAT( 9,CURIYR)
!     CFCRDLT2SWT(CURIYR)  = OGCRDHEAT(10,CURIYR)
!     CFCRDLSCOND(CURIYR)  = OGCRDHEAT(11,CURIYR)
      IF (sum(OGCRDPRD(1:MNUMOR-1, 1:MNCRUD,CURIYR)) .GT. 0.0) THEN
         CFCRDDOM(CURIYR) =                                           &
              (sum(OGCRDPRD(1:MNUMOR-1, 1,CURIYR)) * CFCRDLTSWT(CURIYR)      + &
               sum(OGCRDPRD(1:MNUMOR-1, 2,CURIYR)) * CFCRDLTSOUR(CURIYR)     + &
               sum(OGCRDPRD(1:MNUMOR-1, 3,CURIYR)) * CFCRDMD2SOUR(CURIYR)    + &
               sum(OGCRDPRD(1:MNUMOR-1, 4,CURIYR)) * CFCRDMDSOUR(CURIYR)     + &
               sum(OGCRDPRD(1:MNUMOR-1, 5,CURIYR)) * CFCRDHVSWT(CURIYR)      + &
               sum(OGCRDPRD(1:MNUMOR-1, 6,CURIYR)) * CFCRDHVSOUR(CURIYR)     + &
               sum(OGCRDPRD(1:MNUMOR-1, 7,CURIYR)) * CFCRDCA(CURIYR)         + &
               sum(OGCRDPRD(1:MNUMOR-1, 8,CURIYR)) * CFCRDSYN(CURIYR)        + &
               sum(OGCRDPRD(1:MNUMOR-1, 9,CURIYR)) * CFCRDDILBIT(CURIYR)     + &
               sum(OGCRDPRD(1:MNUMOR-1,10,CURIYR)) * CFCRDLT2SWT(CURIYR)     + &
               sum(OGCRDPRD(1:MNUMOR-1,11,CURIYR)) * CFCRDLSCOND(CURIYR)) /    &
                  sum(OGCRDPRD(1:MNUMOR-1, 1:MNCRUD,CURIYR))
      ENDIF
      IF (sum(Q_CRUDE_IMPORTA(MNUMPR, 1:11,CURCALYR)) .GT. 0.0) THEN
         CFCRDIMP(CURIYR) =                                                    &
              (Q_CRUDE_IMPORTA(MNUMPR, 1,CURCALYR) * CFCRDLTSWT(CURIYR)      + &
               Q_CRUDE_IMPORTA(MNUMPR, 2,CURCALYR) * CFCRDLTSOUR(CURIYR)     + &
               Q_CRUDE_IMPORTA(MNUMPR, 3,CURCALYR) * CFCRDMD2SOUR(CURIYR)    + &
               Q_CRUDE_IMPORTA(MNUMPR, 4,CURCALYR) * CFCRDMDSOUR(CURIYR)     + &
               Q_CRUDE_IMPORTA(MNUMPR, 5,CURCALYR) * CFCRDHVSWT(CURIYR)      + &
               Q_CRUDE_IMPORTA(MNUMPR, 6,CURCALYR) * CFCRDHVSOUR(CURIYR)     + &
               Q_CRUDE_IMPORTA(MNUMPR, 7,CURCALYR) * CFCRDCA(CURIYR)         + &
               Q_CRUDE_IMPORTA(MNUMPR, 8,CURCALYR) * CFCRDSYN(CURIYR)        + &
               Q_CRUDE_IMPORTA(MNUMPR, 9,CURCALYR) * CFCRDDILBIT(CURIYR)     + &
               Q_CRUDE_IMPORTA(MNUMPR,10,CURCALYR) * CFCRDLT2SWT(CURIYR)     + &
               Q_CRUDE_IMPORTA(MNUMPR,11,CURCALYR) * CFCRDLSCOND(CURIYR)) /    &
                  sum(Q_CRUDE_IMPORTA(MNUMPR, 1:11,CURCALYR))
      ENDIF
      IT_WOP(CURIYR,2) = IT_WOP(CURIYR,1) / CFCRDIMP(CURIYR)
      IF (sum(Q_CRUDE_EXPORTS(MNUMPR, 1:11,CURCALYR)) .GT. 0.0) THEN
         CFCRDEXP(CURIYR) =                                                    &
              (Q_CRUDE_EXPORTS(MNUMPR, 1,CURCALYR) * CFCRDLTSWT(CURIYR)      + &
               Q_CRUDE_EXPORTS(MNUMPR, 2,CURCALYR) * CFCRDLTSOUR(CURIYR)     + &
               Q_CRUDE_EXPORTS(MNUMPR, 3,CURCALYR) * CFCRDMD2SOUR(CURIYR)    + &
               Q_CRUDE_EXPORTS(MNUMPR, 4,CURCALYR) * CFCRDMDSOUR(CURIYR)     + &
               Q_CRUDE_EXPORTS(MNUMPR, 5,CURCALYR) * CFCRDHVSWT(CURIYR)      + &
               Q_CRUDE_EXPORTS(MNUMPR, 6,CURCALYR) * CFCRDHVSOUR(CURIYR)     + &
               Q_CRUDE_EXPORTS(MNUMPR, 7,CURCALYR) * CFCRDCA(CURIYR)         + &
               Q_CRUDE_EXPORTS(MNUMPR, 8,CURCALYR) * CFCRDSYN(CURIYR)        + &
               Q_CRUDE_EXPORTS(MNUMPR, 9,CURCALYR) * CFCRDDILBIT(CURIYR)     + &
               Q_CRUDE_EXPORTS(MNUMPR,10,CURCALYR) * CFCRDLT2SWT(CURIYR)     + &
               Q_CRUDE_EXPORTS(MNUMPR,11,CURCALYR) * CFCRDLSCOND(CURIYR)) /    &
                  sum(Q_CRUDE_EXPORTS(MNUMPR, 1:11,CURCALYR))
      ENDIF
	  
!WRITE(*,*)"call API_TO_BTU()"

!  calculation option 12 in API_TO_BTU function converts API to Btu (array element 1 converted to array element 2)
!  calculation option 21 in API_TO_BTU function converts Btu to API (array element 2 converted to array element 1)
      APICAMG(1,CURIYR) = API_TO_BTU(APICAMG(2,CURIYR),21)
      APILTSW(2,CURIYR) = API_TO_BTU(APILTSW(1,CURIYR),12)
      APILTSO(2,CURIYR) = API_TO_BTU(APILTSO(1,CURIYR),12)
      APIMMSO(2,CURIYR) = API_TO_BTU(APIMMSO(1,CURIYR),12)
      APIMDSO(2,CURIYR) = API_TO_BTU(APIMDSO(1,CURIYR),12)
      APIHVSW(2,CURIYR) = API_TO_BTU(APIHVSW(1,CURIYR),12)
      APIHVSO(2,CURIYR) = API_TO_BTU(APIHVSO(1,CURIYR),12)
      APICA(2,CURIYR)   = API_TO_BTU(APICA(1,CURIYR),12)
      APISYN(2,CURIYR)  = API_TO_BTU(APISYN(1,CURIYR),12)
      APIDIL(2,CURIYR)  = API_TO_BTU(APIDIL(1,CURIYR),12)
      APILLSW(2,CURIYR) = API_TO_BTU(APILLSW(1,CURIYR),12)
      API50PL(2,CURIYR) = API_TO_BTU(API50PL(1,CURIYR),12)
      APICRDDOM(2,CURIYR) = API_TO_BTU(APICRDDOM(1,CURIYR),12)
      APICRDIMP(2,CURIYR) = API_TO_BTU(APICRDIMP(1,CURIYR),12)
      APICRDEXP(2,CURIYR) = API_TO_BTU(APICRDEXP(1,CURIYR),12)

!WRITE(*,*)"done with API_TO_BTU()"

!  but first somebody needs to calculate a total distillate conversion factor:
      CFDSQT(CURIYR) =(CFDSRS(CURIYR) * QDSRS(MNUMCR,CURIYR) + &
                        CFDSCM(CURIYR) * QDSCM(MNUMCR,CURIYR) + &
                        CFDSIN(CURIYR) * QDSIN(MNUMCR,CURIYR) + &
                        CFDSTR(CURIYR) * QDSTR(MNUMCR,CURIYR) + &
                        CFDSEL(CURIYR) * QDSEL(MNUMCR,CURIYR)) / &
                   (QDSRS(MNUMCR,CURIYR) + QDSCM(MNUMCR,CURIYR) + QDSIN(MNUMCR,CURIYR) + &
                    QDSTR(MNUMCR,CURIYR) + QDSEL(MNUMCR,CURIYR))

!  now it is all clear for total petroleum:
      CFTPQ(CURIYR) =(QMGAS(11,CURIYR) + QJFTR(11,CURIYR) + &
                      QDSAS(11,CURIYR) + QLGAS(11,CURIYR) + QPFIN(11,CURIYR) + &
                      QRLAS(11,CURIYR) + QRHAS(11,CURIYR) + &
! replacing other with the pieces - ind (lubricants, pentanes plus) and tran (av gas, lubricants)
                      QPPIN(MNUMCR,CURIYR) + QPPINPF(MNUMCR,CURIYR) + &
                      QLUIN(MNUMCR,CURIYR) + QOTIN(MNUMCR,CURIYR) + &
                      QAGTR(MNUMCR,CURIYR) + QLUTR(MNUMCR,CURIYR) + &
                                         QKSAS(11,CURIYR) + &
                      QASIN(11,CURIYR) + QPCAS(11,CURIYR) + QSGIN(11,CURIYR)) / &
                     (QMGAS(11,CURIYR)/CFMGQ(CURIYR) + &
                      QJFTR(11,CURIYR)/CFJFQ(CURIYR) + QDSAS(11,CURIYR)/CFDSQT(CURIYR) + &
                      QRLAS(11,CURIYR)/CFRSQ + QRHAS(11,CURIYR)/CFRSQ + &
!                       QOTAS(11,CURIYR)/CFOTQ(CURIYR) +
                     (QPPIN(MNUMCR,CURIYR)+QPPINPF(MNUMCR,CURIYR))/CFPPQ + &
                      QOTIN(MNUMCR,CURIYR)/CFOTQ(CURIYR) + &
                      QLUIN(MNUMCR,CURIYR)/CFLUQ + QLUTR(MNUMCR,CURIYR)/CFLUQ + &
                      QAGTR(MNUMCR,CURIYR)/CFAVQ + QKSAS(11,CURIYR)/CFKSQ + &
                      QLGAS(11,CURIYR)/CFLGQ(CURIYR) + QPCAS(11,CURIYR)/CFPCQ + &
                      QPFIN(11,CURIYR)/CFPFQ(CURIYR) + QASIN(11,CURIYR)/CFASQ + &
                      QSGIN(11,CURIYR)/CFSGQ)
      IF (DBDUMP .EQ. 1) &
          WRITE(ICNVRTOUT,'(A,I2,A,F6.3)')  &
        " Petroleum conversion factor after iteration ",CURITR," is ",CFTPQ(CURIYR)

         QPRDEX(30,CURIYR) = sum(QPRDEX(1:29,CURIYR))
! Fischer-Tropsch conversion factors
!  GTL:
      IF (sum(GTLFRAC(1:4,MNUMPR,CURIYR)) .NE. 0.0) THEN
         CFGTLLIQ(CURIYR) =(GTLFRAC(1,MNUMPR,CURIYR) * CFFTLIQ(1,CURIYR) + &
                            GTLFRAC(2,MNUMPR,CURIYR) * CFFTLIQ(2,CURIYR) + &
                            GTLFRAC(3,MNUMPR,CURIYR) * CFFTLIQ(3,CURIYR) + &
                            GTLFRAC(4,MNUMPR,CURIYR) * CFFTLIQ(4,CURIYR))/ &
                        sum(GTLFRAC(1:4,MNUMPR,CURIYR))
      ELSE
         CFGTLLIQ(CURIYR) = CFDSQ
      ENDIF
!  CTL:
      IF (sum(CTLFRAC(1:4,MNUMPR,CURIYR)) .NE. 0.0) THEN
         CFCTLLIQ(CURIYR) =(CTLFRAC(1,MNUMPR,CURIYR) * CFFTLIQ(1,CURIYR) + &
                            CTLFRAC(2,MNUMPR,CURIYR) * CFFTLIQ(2,CURIYR) + &
                            CTLFRAC(3,MNUMPR,CURIYR) * CFFTLIQ(3,CURIYR) + &
                            CTLFRAC(4,MNUMPR,CURIYR) * CFFTLIQ(4,CURIYR))/ &
                        sum(CTLFRAC(1:4,MNUMPR,CURIYR))
      ELSE
         CFCTLLIQ(CURIYR) = CFDSQ
      ENDIF
!  BTL:
      IF (sum(BTLFRAC(1:4,MNUMPR,CURIYR)) .NE. 0.0) THEN
         CFBTLLIQ(CURIYR) =(BTLFRAC(1,MNUMPR,CURIYR) * CFFTLIQ(1,CURIYR) + &
                            BTLFRAC(2,MNUMPR,CURIYR) * CFFTLIQ(2,CURIYR) + &
                            BTLFRAC(3,MNUMPR,CURIYR) * CFFTLIQ(3,CURIYR) + &
                            BTLFRAC(4,MNUMPR,CURIYR) * CFFTLIQ(4,CURIYR))/ &
                        sum(BTLFRAC(1:4,MNUMPR,CURIYR))
      ELSE
         CFBTLLIQ(CURIYR) = CFDSQ
      ENDIF
!  CBTL:
      IF (sum(CBTLFRAC(1,1:4,MNUMPR,CURIYR)) .NE. 0.0) THEN
         CFCBTLLIQ(1,CURIYR) =(CBTLFRAC(1,1,MNUMPR,CURIYR) * CFFTLIQ(1,CURIYR) + &
                               CBTLFRAC(1,2,MNUMPR,CURIYR) * CFFTLIQ(2,CURIYR) + &
                               CBTLFRAC(1,3,MNUMPR,CURIYR) * CFFTLIQ(3,CURIYR) + &
                               CBTLFRAC(1,4,MNUMPR,CURIYR) * CFFTLIQ(4,CURIYR))/ &
                           sum(CBTLFRAC(1,1:4,MNUMPR,CURIYR))
      ELSE
         CFCBTLLIQ(1,CURIYR) = CFDSQ
      ENDIF
      IF (sum(CBTLFRAC(2,1:4,MNUMPR,CURIYR)) .NE. 0.0) THEN
         CFCBTLLIQ(2,CURIYR) =(CBTLFRAC(2,1,MNUMPR,CURIYR) * CFFTLIQ(1,CURIYR) + &
                               CBTLFRAC(2,2,MNUMPR,CURIYR) * CFFTLIQ(2,CURIYR) + &
                               CBTLFRAC(2,3,MNUMPR,CURIYR) * CFFTLIQ(3,CURIYR) + &
                               CBTLFRAC(2,4,MNUMPR,CURIYR) * CFFTLIQ(4,CURIYR))/ &
                           sum(CBTLFRAC(2,1:4,MNUMPR,CURIYR))
      ELSE
         CFCBTLLIQ(2,CURIYR) = CFDSQ
      ENDIF
      IF ((sum(CBTLFRAC(1,:,MNUMPR,CURIYR)) + sum(CBTLFRAC(2,:,MNUMPR,CURIYR))) .NE. 0.0) THEN
          CFCBTLLIQ(3,CURIYR) =(sum(CBTLFRAC(1,:,MNUMPR,CURIYR)) * CFCBTLLIQ(1,CURIYR) + &
                                sum(CBTLFRAC(2,:,MNUMPR,CURIYR)) * CFCBTLLIQ(2,CURIYR)) / &
                               (sum(CBTLFRAC(1,:,MNUMPR,CURIYR)) + sum(CBTLFRAC(2,:,MNUMPR,CURIYR)))
      ELSE
         CFCBTLLIQ(3,CURIYR) = CFDSQ
      ENDIF
! end Fischer-Tropsch conversion factors
      IF ((RFIPQAS(MNUMPR,CURIYR,2) + RFIPQAG(MNUMPR,CURIYR,2) + RFIPQCD(MNUMPR,CURIYR,2) + &
           RFIPQMG(MNUMPR,CURIYR,2) + RFIPQRG(MNUMPR,CURIYR,2) + RFIPQDL(MNUMPR,CURIYR,2) + &
           RFIPQDU(MNUMPR,CURIYR,2) + RFIPQJF(MNUMPR,CURIYR,2) + RFIPQPF(MNUMPR,CURIYR,2) + &
           RFIPQPR(MNUMPR,CURIYR,2) + RFIPQPY(MNUMPR,CURIYR,2) + RFIPQPP(MNUMPR,CURIYR,2) + &
           RFIPQET(MNUMPR,CURIYR,2) + RFIPQBU(MNUMPR,CURIYR,2) + RFIPQIS(MNUMPR,CURIYR,2) + &
           RFIPQLU(MNUMPR,CURIYR,2) + RFIPQDS(MNUMPR,CURIYR,2) + RFIPQRL(MNUMPR,CURIYR,2) + &
           RFIPQRH(MNUMPR,CURIYR,2) + RFIPQPC(MNUMPR,CURIYR,2)) .NE. 0.0) THEN
               CFIMPRD(CURIYR) = ( &
                        RFIPQAS(MNUMPR,CURIYR,2) * CFASQ + &
                        RFIPQAG(MNUMPR,CURIYR,2) * CFAVQ + &
                        RFIPQCD(MNUMPR,CURIYR,2) * CFDSCQ(CURIYR) + &
                        RFIPQMG(MNUMPR,CURIYR,2) * CFTGQ(CURIYR) + &
                        RFIPQRG(MNUMPR,CURIYR,2) * CFRGQ(CURIYR) + &
                        RFIPQDL(MNUMPR,CURIYR,2) * CFDSLQ(CURIYR) + &
                        RFIPQDU(MNUMPR,CURIYR,2) * CFDSUQ(CURIYR) + &
                        RFIPQJF(MNUMPR,CURIYR,2) * CFJFQ(CURIYR) + &
                        RFIPQPF(MNUMPR,CURIYR,2) * CFPFQ(CURIYR) + &
                        RFIPQPR(MNUMPR,CURIYR,2) * CFPRQ + &
                        RFIPQPY(MNUMPR,CURIYR,2) * CFPRQ + &
                        RFIPQET(MNUMPR,CURIYR,2) * CFEEQ + &
                        RFIPQBU(MNUMPR,CURIYR,2) * CFBUQ + &
                        RFIPQIS(MNUMPR,CURIYR,2) * CFIBQ + &
                        RFIPQPP(MNUMPR,CURIYR,2) * CFPPQ + &
                        RFIPQLU(MNUMPR,CURIYR,2) * CFLUQ + &
                        RFIPQDS(MNUMPR,CURIYR,2) * CFDSQ + &
                        RFIPQRL(MNUMPR,CURIYR,2) * CFRSQ + &
                        RFIPQRH(MNUMPR,CURIYR,2) * CFRSQ + &
                        RFIPQPC(MNUMPR,CURIYR,2) * CFPCQ) / &
          (RFIPQAS(MNUMPR,CURIYR,2) + RFIPQAG(MNUMPR,CURIYR,2) + RFIPQCD(MNUMPR,CURIYR,2) + &
           RFIPQMG(MNUMPR,CURIYR,2) + RFIPQRG(MNUMPR,CURIYR,2) + RFIPQDL(MNUMPR,CURIYR,2) + &
           RFIPQDU(MNUMPR,CURIYR,2) + RFIPQJF(MNUMPR,CURIYR,2) + RFIPQPF(MNUMPR,CURIYR,2) + &
           RFIPQPR(MNUMPR,CURIYR,2) + RFIPQPY(MNUMPR,CURIYR,2) + RFIPQPP(MNUMPR,CURIYR,2) + &
           RFIPQET(MNUMPR,CURIYR,2) + RFIPQBU(MNUMPR,CURIYR,2) + RFIPQIS(MNUMPR,CURIYR,2) + &
           RFIPQLU(MNUMPR,CURIYR,2) + RFIPQDS(MNUMPR,CURIYR,2) + &
           RFIPQRL(MNUMPR,CURIYR,2) + RFIPQRH(MNUMPR,CURIYR,2) + RFIPQPC(MNUMPR,CURIYR,2))
      ELSE
               CFIMPRD(CURIYR) = 5.8
      ENDIF
      IF (QPRDEX(30,CURIYR) .NE. 0.0) THEN
         CFEXPRD(CURIYR) = ( &
               QPRDEX( 1,CURIYR) * CFPRQ + &            ! use straight propane factor
               QPRDEX( 2,CURIYR) * CFTGQ(CURIYR) + &
               QPRDEX( 3,CURIYR) * CFRGQ(CURIYR) + &
               QPRDEX( 4,CURIYR) * CFTGQ(CURIYR) + &
               QPRDEX( 5,CURIYR) * CFRGQ(CURIYR) + &
               QPRDEX( 6,CURIYR) * CFJFQ(CURIYR) + &
               QPRDEX( 7,CURIYR) * CFDSQ + &
               QPRDEX( 8,CURIYR) * CFRSQ + &
               QPRDEX( 9,CURIYR) * CFRSQ + &
               QPRDEX(10,CURIYR) * CFGO3(CURIYR) + &
               QPRDEX(11,CURIYR) * CFPFQ(CURIYR) + &
               QPRDEX(12,CURIYR) * CFASQ + &
               QPRDEX(13,CURIYR) * CFDSLQ(CURIYR) + &
               QPRDEX(14,CURIYR) * CFPRQ + &
               QPRDEX(15,CURIYR) * CFBUQ + &
               QPRDEX(16,CURIYR) * CFPCQ + &
               QPRDEX(17,CURIYR) * CFE85Q(CURIYR) + &
               QPRDEX(18,CURIYR) * CFAVQ + &
               QPRDEX(19,CURIYR) * CFLUQ + &
               QPRDEX(20,CURIYR) * CFAR3(CURIYR) + &
               QPRDEX(21,CURIYR) * CFMN3(CURIYR) + &
               QPRDEX(22,CURIYR) * CFMEQT + &
               QPRDEX(23,CURIYR) * CFGOP(CURIYR) + &
               QPRDEX(24,CURIYR) * CFDSUQ(CURIYR) + &
               QPRDEX(25,CURIYR) * CFDSCQ(CURIYR) + &   !  CarbDSUout; next (26) is CaRBOBout
               QPRDEX(26,CURIYR) * CFRGQ(CURIYR) + &
               QPRDEX(27,CURIYR) * CFEEQ + &
               QPRDEX(28,CURIYR) * CFIBQ + &
               QPRDEX(29,CURIYR) * CFPPQ) / QPRDEX(30,CURIYR)
      ELSE
         CFEXPRD(CURIYR) = 5.8
      ENDIF

!WRITE(*,*)"done with DOCVFACTS()"

      RETURN
      END SUBROUTINE DOCVFACTS

!end module calc_weights

!******************************************************************
!*    FUNCTION API_TO_BTU(API_IN,API_CALC)
!*
!******************************************************************
!  function to return a million Btu/barrel conversion factor when sent an API gravity, any API gravity.
!  note that the API_OUT formulas can be simplified, but here parallel the BTU_OUT formulas

      REAL FUNCTION API_TO_BTU(API_IN,API_CALC)
      IMPLICIT NONE

      include 'apiblk'

      REAL API_IN, API_OUT, BTU_OUT, BTU_IN

      INTEGER I, API_CALC

!WRITE(*,*)"inside API_TO_BTU()"

      IF (API_CALC .EQ. 21) THEN
         BTU_IN = API_IN / 42 * 1000000.
         IF (BTU_IN .GT. BTU_PER_GAL(1)) THEN
             BTU_OUT = (BTU_IN-BTU_PER_GAL(1))/(BTU_PER_GAL(1)-BTU_PER_GAL(2))*(BTU_PER_GAL(1)-BTU_PER_GAL(2))+BTU_PER_GAL(1)
             API_OUT = (BTU_IN-BTU_PER_GAL(1))/(BTU_PER_GAL(1)-BTU_PER_GAL(2))*(API_GRAV(1)-API_GRAV(2))+API_GRAV(1)
             API_TO_BTU = API_OUT
             RETURN
         ELSE IF (BTU_IN .LT. BTU_PER_GAL(API_COUNT)) THEN
             BTU_OUT = (BTU_IN-BTU_PER_GAL(API_COUNT))/(BTU_PER_GAL(API_COUNT-1)-BTU_PER_GAL(API_COUNT))*(BTU_PER_GAL(API_COUNT-1)-BTU_PER_GAL(API_COUNT))+BTU_PER_GAL(API_COUNT)
             API_OUT = (BTU_IN-BTU_PER_GAL(API_COUNT))/(BTU_PER_GAL(API_COUNT-1)-BTU_PER_GAL(API_COUNT))*(API_GRAV(API_COUNT-1)-API_GRAV(API_COUNT))+API_GRAV(API_COUNT)
             API_TO_BTU = API_OUT
             RETURN
         ENDIF
         DO I=1,API_COUNT
            IF (BTU_IN .LE. BTU_PER_GAL(I) .AND. BTU_IN .GT. BTU_PER_GAL(I+1)) THEN
                BTU_OUT = (BTU_IN-BTU_PER_GAL(I+1))/(BTU_PER_GAL(I)-BTU_PER_GAL(I+1))*(BTU_PER_GAL(I)-BTU_PER_GAL(I+1))+BTU_PER_GAL(I+1)
                API_OUT = (BTU_IN-BTU_PER_GAL(I+1))/(BTU_PER_GAL(I)-BTU_PER_GAL(I+1))*(API_GRAV(I)-API_GRAV(I+1))+API_GRAV(I+1)
                API_TO_BTU = API_OUT
                RETURN
            ENDIF
         ENDDO
      ENDIF
      IF (API_CALC .EQ. 12) THEN
         IF (API_IN .LT. API_GRAV(1)) THEN
!  off table, extrapolate off low end
             API_OUT = (API_GRAV(1)-API_IN)/(API_GRAV(2)-API_GRAV(1))*  &
                       (API_GRAV(1)-API_GRAV(2))+API_GRAV(1)
             BTU_OUT = (API_GRAV(1)-API_IN)/(API_GRAV(2)-API_GRAV(1))*  &
                       (BTU_PER_GAL(1)-BTU_PER_GAL(2))+BTU_PER_GAL(1)
             API_TO_BTU = BTU_OUT * 42 / 1000000
             RETURN
         ELSE IF (API_IN .GT. API_GRAV(API_COUNT)) THEN
!  off table, extrapolate off high end
             API_OUT = (API_IN-API_GRAV(API_COUNT))/(API_GRAV(API_COUNT)-API_GRAV(API_COUNT-1))*  &
                       (API_GRAV(API_COUNT)-API_GRAV(API_COUNT-1))+API_GRAV(API_COUNT)
             BTU_OUT = (API_IN-API_GRAV(API_COUNT))/(API_GRAV(API_COUNT)-API_GRAV(API_COUNT-1))*  &
                        (BTU_PER_GAL(API_COUNT)-BTU_PER_GAL(API_COUNT-1))+BTU_PER_GAL(API_COUNT)
             API_TO_BTU = BTU_OUT * 42 / 1000000
             RETURN
         ENDIF
         DO I=1,API_COUNT
            IF (API_IN .GE. API_GRAV(I) .AND. API_IN .LT. API_GRAV(I+1)) THEN
!  within bounds of table, find place and interpolate
                API_OUT = (API_GRAV(I+1)-API_IN)/(API_GRAV(I+1)-API_GRAV(I))*  &
                          (API_GRAV(I)-API_GRAV(I+1))+API_GRAV(I+1)
                BTU_OUT = (API_GRAV(I+1)-API_IN)/(API_GRAV(I+1)-API_GRAV(I))*  &
                          (BTU_PER_GAL(I)-BTU_PER_GAL(I+1))+BTU_PER_GAL(I+1)
                API_TO_BTU = BTU_OUT * 42 / 1000000
                RETURN
            ENDIF
         ENDDO
      ENDIF

      RETURN
      END