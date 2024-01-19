! $Header: M:/default/source/RCS/mac.f,v 1.203 2021/05/25 14:09:49 RT2 Exp $
!**************************************************************************
!**************************************************************************
!   Macroeconomic Module
!**************************************************************************


!**************************************************************************
       SUBROUTINE MAC
       USE DFLIB
!**************************************************************************
!  The MAC subroutine directs the execution of the Macroeconomic
!   Activity Module (MAM). It calls the following subroutines:
!
!   READMAC    - reads a single file, mcparms, which contains MAM
!                parameters.
!   DRTLINK    - executes the GI US quarterly model in EViews based upon
!                NEMS energy driver variables.
!   INDUSTSUB  - industrial forecast based mostly upon generated output
!                and to a lesser extent industrial output.
!   REGIONSUB  - executes EIA regional models of economic activity,
!                industrial output and employment.
!   EMPLOYMENT - employment forecast based on percent changes from base
!                of mostly generated output and to a lesser extent
!                industrial output.
!   COMFLR     - computes a forecast of commercial floorspace by type,
!                within the nine Census Division regions.
!   TRANC      - EIA transportation size class model for the US
!   MACOUTPUT  - writes to output directory the following files containing
!                at solution and base values of variables contained
!                in MAM: MC_COMMON.CVS, MC_REGIONAL.CSV, MC_INDUSTRIAL.CSV,
!                MC_EMPLOYMENT.CSV and MC_NATIONAL.CSV,
!
!   The MAC subroutine also writes the MC_ENERGY.CSV file containing the
!   bridged energy variables and their components.  It also writes to the
!   global data structure MAM solutions.
!
!**************************************************************************

       IMPLICIT NONE

!  Local variables.
       CHARACTER*1200 line,label
       INTEGER        i,j,k,r,iyr,fyrprc,charc
       REAL           rdays,scaleprice,seccon
       LOGICAL        lstatus

!  Include parameter files.
       INCLUDE'parametr'
       INCLUDE'apq'
       INCLUDE'macout'
       INCLUDE'pmmout'
       INCLUDE'lfmmout'
       INCLUDE'pmmftab'
       INCLUDE'pmmrpt'
       INCLUDE'ogsmout'
       INCLUDE'convfact'
       INCLUDE'ngtdmrep'
       INCLUDE'angtdm'
       INCLUDE'ncntrl'
       INCLUDE'emmparm'
       INCLUDE'emission'
       INCLUDE'ghgrep'
       INCLUDE'cdsparms'
       INCLUDE'coalout'
       INCLUDE'coalrep'
       INCLUDE'intout'
       INCLUDE'wrenew'
       INCLUDE'tranrep'
       INCLUDE'uefdout'
       INCLUDE'macparm.'
       INCLUDE'mcinter2.'
       INCLUDE'uecpout'

!  Declare function for NEMS parameters.
       EXTERNAL getindex,rtovalue
       INTEGER  getindex,rtovalue


!  START IF: Call subroutines readmac and drtlink in first year, first iteration.
       IF (curiyr .EQ. firsyr .AND. curitr .EQ. 1) THEN
!  Read external input files.
         WRITE(*,*) "MAC.F: Calling read of parameter file"
         CALL readmac
!  START IF (macfdbk .EQ. 1): Execute the GI and EIA models in EViews if macro
!     feedback is on.
         IF (giswitch .GE. 0) THEN
           write(*,*) "Calling GISUB Subroutine"
           CALL gisub
           GOTO 100

         END IF

!  Initialize matrix of solutions.
         epmac = 1.0
!  Initialize vehicle global data structure matrix
         mc_vehicles = -99999.
!  Initialize commercial floorspace matrix.
         mc_commflsp = 0.0

!  Set parameters used to compute energy variables.
         rdays = 365
         fyrprc = 2012
         scaleprice=mc_jpgdp(fyrprc-1989)
! Commented out to test 2050
         mamlastyr = RTOVALUE('AEOLSTYR',0)

!  START DO IYR: Loop across components of energy variables contained
!   in the restart file by year.
         DO iyr = 1,(mamlastyr-1990+1)
!  Prod_CrudeOil
           nlink(1,iyr)   = rfqtdcrd(mnumor+2,iyr)*rdays*cfcrddom(iyr)*.001
           nlink(2,iyr)   = sum(ogoilprd(:,5,iyr))
           nlink(3,iyr)   = cfcrddom(iyr)
!  Prod_NaturalGas
           nlink(4,iyr)   = rfpqngl(mnumpr,iyr,6,2)*rdays*cfngl(iyr)*.001
           nlink(5,iyr)   = sum(ogngplprd(:,iyr))
           nlink(6,iyr)   = cfngl(iyr)
!  Prod_DryNaturalGas
           nlink(7,iyr)   = cfngc(iyr)*(ogqngrep(1,iyr)+ogqngrep(2,iyr)+ogqngrep(3,iyr)+ogqngrep(4,iyr)+ogqngrep(5,iyr)+ogqngrep(6,iyr)+ogqngrep(7,iyr)+ogqngrep(8,iyr)+ogshaleng(iyr))*.001
           nlink(8,iyr)   = cfngc(iyr)
           nlink(9,iyr)   = ogprdng(mnumor,iyr)
           nlink(10,iyr)  = ogshaleng(iyr)
!  Prod_Coal
           nlink(11,iyr)  =(cqsbb(3,iyr)+sum(wc_prod_btu(11,1:mnumlr,iyr)))*.001
           nlink(12,iyr)  = cqsbb(3,iyr)
           nlink(13,iyr)  = sum(wc_prod_btu(11,1:mnumlr,iyr))
!  Prod_Nuclear
           nlink(14,iyr)  = qurel(11,iyr)*.001
           nlink(15,iyr)  = qurel(11,iyr)
!  Prod_Hydro
           nlink(16,iyr)  = qhoas(11,iyr)*.001
           nlink(17,iyr)  = qhoas(11,iyr)
!  Prod_Biomass 
           nlink(18,iyr) = (qbmas(11,iyr) - qbmrf(11,iyr))/1000.                                                         &
                         + corncd(3,11,iyr)*cfcorn/1000000000.                                                           &
                         + sum(bimqtycd(1:4,11,iyr))/1000000.*rdays*cfveggie(iyr)                                        &
                         + (sbo2gdtpd(mnumpr,iyr)+ygr2gdtpd(mnumpr,iyr)+wgr2gdtpd(mnumpr,iyr))*cfveggie(iyr)*rdays/1000. &
                         + qbmrfbtl(11,iyr)/1000.
           nlink(19,iyr)  = (qbmas(11,iyr) - qbmrf(11,iyr))/1000.
           nlink(20,iyr)  = corncd(3,11,iyr)*cfcorn/1000000000.
           nlink(21,iyr)  = sum(bimqtycd(1:4,11,iyr))/1000000.*rdays*cfveggie(iyr)
           nlink(22,iyr)  = (sbo2gdtpd(mnumpr,iyr)+ygr2gdtpd(mnumpr,iyr)+wgr2gdtpd(mnumpr,iyr))*cfveggie(iyr)*rdays/1000.
           nlink(23,iyr)  = qbmrfbtl(11,iyr)/1000.
           nlink(24,iyr)  = 0.9751*cllethcd(11,iyr)*rdays*cfbmq(iyr)*42./coneff(iyr)/1000000.
           nlink(25,iyr)  = wncmsel(iyr,11)
           nlink(26,iyr)  = grd2dsqty(mnumpr,iyr)
           nlink(27,iyr)  = grn2mgqty(mnumpr,iyr)
           nlink(28,iyr)  = sum(btlfrac(1:4,mnumpr,iyr))
!  Prod_OthRenew
           nlink(29,iyr)  =(qtras(11,iyr)-qgers(11,iyr)-qstrs(11,iyr)       &
                          - qstcm(11,iyr)-qpvcm(11,iyr)-qpvrs(11,iyr)       &
                          - qbmas(11,iyr)-qhoas(11,iyr)-qettr(11,iyr))*.001 &
                          - wncmsel(iyr,11)
           nlink(30,iyr)  = qtras(11,iyr)
           nlink(31,iyr)  = qgers(11,iyr)
           nlink(32,iyr)  = qstrs(11,iyr)
           nlink(33,iyr)  = qstcm(11,iyr)
           nlink(34,iyr)  = qpvcm(11,iyr)
           nlink(35,iyr)  = qpvrs(11,iyr)
           nlink(36,iyr)  = qbmas(11,iyr)
           nlink(37,iyr)  = qhoas(11,iyr)
           nlink(38,iyr)  = qettr(11,iyr)
           nlink(39,iyr)  = wncmsel(iyr,11)
!  Prod_Other
           nlink(40,iyr)  = wncmsel(iyr,11)                                &
                          + rfcrdoth(mnumpr,iyr)*cfcrddom(iyr)*rdays/1000. &
                          + rfhcxh2in(mnumpr,iyr)*cfrsq*rdays/1000.        &
                          + rfmetm85(mnumpr,iyr)*cfm85q(iyr)*rdays/1000.
           nlink(41,iyr)  = wncmsel(iyr,11)
           nlink(42,iyr)  = rfcrdoth(mnumpr,iyr)*cfcrddom(iyr)*rdays/1000.
           nlink(43,iyr)  = rfhcxh2in(mnumpr,iyr)*cfrsq*rdays/1000.
           nlink(44,iyr)  = rfmetm85(mnumpr,iyr)*cfm85q(iyr)*rdays/1000.
           nlink(45,iyr)  = ygr2gdtpd(mnumpr,iyr)
           nlink(46,iyr)  = wgr2gdtpd(mnumpr,iyr)
           nlink(47,iyr)  = cfveggie(iyr)
           nlink(48,iyr)  = rfhcxh2in(mnumpr,iyr)
           nlink(49,iyr)  = cfrsq
           nlink(50,iyr)  = rfmetm85(mnumpr,iyr)
           nlink(51,iyr)  = cfmeqt
!  Import_Petroleum
!T1(9,IY,IS)=(RFQICRD(MNUMPR,IY)+RFSPRIM(IY))*CFCRDIMP(IY)*RDAYS*.001
!T1(10,IY,IS)=(RFPQIPRDT(MNUMPR,IY,2)*CFIMPRD(IY)
!+ETHIMP(11,IY)/1000.*CFPET+BIODIMP(11,IY)/1000.*CFBIOD(IY)
!+RENEWDIMP(11,IY)/1000. * CFDSQT(IY)
!+RFIPQCBOB(MNUMPR,IY,2)*CFCBOB(IY)/1000.
!+RFIPQRBOB(MNUMPR,IY,2)*CFRBOB(IY)/1000.
!+RFMTBI(MNUMPR,IY)*4.24
!+RFPQUFC(MNUMPR,IY,2)*CFIMUO(IY))*RDAYS*.001


           nlink(52,iyr)  = ((rfqicrd(mnumpr,iyr)+rfsprim(iyr))*cfcrdimp(iyr) &
                          +   rfpqiprdt(mnumpr,iyr,2)*cfimprd(iyr)            &
                          +   rfipqcbob(mnumpr,iyr,2)*cfcbob(iyr)/1000.       &
                          +   rfipqrbob(mnumpr,iyr,2)*cfrbob(iyr)/1000.       &
                          +   rfmtbi(mnumpr,iyr)*4.24                         &
                          +   rfpqufc(mnumpr,iyr,2)*cfimuo(iyr))*rdays*.001
           nlink(53,iyr)  = rfqicrd(mnumpr,iyr)
           nlink(54,iyr)  = rfsprim(iyr)
           nlink(55,iyr)  = cfcrdimp(iyr)
           nlink(56,iyr)  = rfpqiprdt(mnumpr,iyr,2)
           nlink(57,iyr)  = cfimprd(iyr)
           nlink(58,iyr)  = rfipqcbob(mnumpr,iyr,2)
           nlink(59,iyr)  = cfcbob(iyr)
           nlink(60,iyr)  = rfipqrbob(mnumpr,iyr,2)
           nlink(61,iyr)  = cfrbob(iyr)
           nlink(62,iyr)  = rfmtbi(mnumpr,iyr)
           nlink(63,iyr)  = rfpqufc(mnumpr,iyr,2)
           nlink(64,iyr)  = cfimuo(iyr)
!  Import_NatGas
           nlink(65,iyr)  = ngimpvol(4,iyr)*cfngi(iyr)*.001
           nlink(66,iyr)  = ngimpvol(4,iyr)
           nlink(67,iyr)  = cfngi(iyr)
!  Import_Biofuels
           nlink(68,iyr)  = (ethimp(11,iyr)*cfpet                           &
                          +  biodimp(11,iyr)*cfbiod(iyr)                    &
                          +  biobuteimp(iyr)*cfbiobute(iyr))*rdays/1000000.
           nlink(69,iyr)  = ethimp(11,iyr)
           nlink(70,iyr)  = biodimp(11,iyr)
           nlink(71,iyr)  = biobuteimp(iyr)
           nlink(72,iyr)  = cfngi(iyr)
           nlink(73,iyr)  = cfbiod(iyr)
           nlink(74,iyr)  = cfbiobute(iyr)
!  Import_Coal
           nlink(75,iyr)  = cqdbfb(11,7,iyr)*.001
           nlink(76,iyr)  = cqdbfb(11,7,iyr)
!  Import_Other
           nlink(77,iyr)  =(qciin(11,iyr)+cqdbfb(11,7,iyr)+qeiel(11,iyr))*.001
           nlink(78,iyr)  = qciin(11,iyr)
           nlink(79,iyr)  = qeiel(11,iyr)
!  Export_Fuels
           nlink(80,iyr)  = ((rfqexcrd(mnumpr,iyr)*.001*cfcrdexp(iyr)  &
                          +   rfqexprdt(mnumpr,iyr)*cfexprd(iyr)       &
                          +   ethexp(11,iyr)/1000.*cfpet               &
                          +   biodexp(11,iyr)/1000.*cfbiod(iyr))*rdays &
                          +   ngexpvol(4,iyr)*cfnge(iyr)               &
                          +  cqdbfb(11,5,iyr))*.001
           nlink(81,iyr)  = rfqexcrd(mnumpr,iyr)
           nlink(82,iyr)  = cfcrdexp(iyr)
           nlink(83,iyr)  = rfqexprdt(mnumpr,iyr)
           nlink(84,iyr)  = cfexprd(iyr)
           nlink(85,iyr)  = ethexp(11,iyr)
           nlink(86,iyr)  = cfpet
           nlink(87,iyr)  = biodexp(11,iyr)
           nlink(88,iyr)  = cfbiod(iyr)
           nlink(89,iyr)  = ngexpvol(4,iyr)
           nlink(90,iyr)  = cfnge(iyr)
           nlink(91,iyr)  = cqdbfb(11,5,iyr)
!  Export_Petroleum
           nlink(92,iyr)  = (rfqexcrd(mnumpr,iyr)*cfcrdexp(iyr)/1000000.+rfqexprdt(mnumpr,iyr)*cfexprd(iyr)/1000.)*rdays
           nlink(93,iyr)  = rfqexcrd(mnumpr,iyr)
           nlink(94,iyr)  = cfcrdexp(iyr)
           nlink(95,iyr)  = rfqexprdt(mnumpr,iyr)
           nlink(96,iyr)  = cfexprd(iyr)
!  Export_NatGas
           nlink(97,iyr)  = ngexpvol(4,iyr)*cfnge(iyr)*.001
           nlink(98,iyr)  = ngexpvol(4,iyr)*cfnge(iyr)
           nlink(99,iyr)  = cfnge(iyr)
!  Export_Coal
           nlink(100,iyr) = cqdbfb(11,5,iyr)*.001
           nlink(101,iyr) = cqdbfb(11,5,iyr)
!  Export_Biofuels
           nlink(102,iyr) = (ethexp(11,iyr)*cfpet        &
                          +  biodexp(11,iyr)*cfbiod(iyr) &
                          +  biobuteexp(iyr)*cfbiobute(iyr))*rdays/1000000.
           nlink(103,iyr) = ethexp(11,iyr)
           nlink(104,iyr) = cfpet
           nlink(105,iyr) = biodexp(11,iyr)
           nlink(106,iyr) = cfbiod(iyr)
           nlink(107,iyr) = biobuteexp(iyr)
           nlink(108,iyr) = cfbiobute(iyr)
!  Export_Other
           nlink(109,iyr) = 0
!  Cons_Petroleum
!          T2(88,IR,IY,IS)=QDSAS(IR,IY)
!          T2(89,IR,IY,IS)=QKSAS(IR,IY)
!          T2(90,IR,IY,IS)=QJFTR(IR,IY)                       !  All sectors - don't subtract QJFBS
!          T2(91,IR,IY,IS)=QLGAS(IR,IY)
!          T2(92,IR,IY,IS)=QMGAS(IR,IY) + QETTR(IR,IY) + QMETR(IR,IY)
!          T2(93,IR,IY,IS)=QPFIN(IR,IY)
!          T2(94,IR,IY,IS)=QRSAS(IR,IY)
!          T2(95,IR,IY,IS)=QOTAS(IR,IY) + QSGIN(IR,IY) + QPCIN(IR,IY) + QASIN(IR,IY)
!          T2(96,IR,IY,IS)=FSUM(T2(88,IR,IY,IS),8)

           nlink(110,iyr) = (qdsas(11,iyr)                                &
                          + qksas(11,iyr)                                 &
                          + qjftr(11,iyr)                                 &
                          + qlgas(11,iyr)                                 &
                          + qmgas(11,iyr) + qettr(11,iyr) + qmetr(11,iyr) &
                          + qpfin(11,iyr)                                 &
                          + qrsas(11,iyr)                                 &
                          + qotas(11,iyr) + qsgin(11,iyr) + qpcin(11,iyr) + qasin(11,iyr))*.001




!           nlink(110,iyr) = (qtpas(11,iyr)+qettr(11,iyr)+qmetr(11,iyr))/1000
           nlink(111,iyr) = qtpas(11,iyr)
           nlink(112,iyr) = qettr(11,iyr)
           nlink(113,iyr) = qmetr(11,iyr)
!  Cons_NatGas
           nlink(114,iyr) =(qngas(11,iyr)+qgptr(11,iyr)+qlpin(11,iyr)+qnglq(11,iyr)-qhytr(11,iyr)/.7)*.001
           nlink(115,iyr) = qngas(11,iyr)
           nlink(116,iyr) = qgptr(11,iyr)
           nlink(117,iyr) = qlpin(11,iyr)+qnglq(11,iyr)
           nlink(118,iyr) = cfngn(iyr)
           nlink(119,iyr) = cfgtlliq(iyr)
           nlink(120,iyr) = qhytr(11,iyr)
!  Cons_Coal
!          T2(98,IR,IY,IS)=QMCIN(IR,IY)
!          T2(99,IR,IY,IS)=QCLAS(IR,IY)-QCTLRF(IR,IY) - OGSUPGAS(1,IR,IY) * CFNGC(IY) * .001
!          T2(100,IR,IY,IS)=QCIIN(IR,IY)
!          T2(119,IR,IY,IS)=QCTLRF(IR,IY)
!          T2(101,IR,IY,IS)=FSUM(T2(98,IR,IY,IS),3) + T2(119,IR,IY,IS)

           nlink(121,iyr) = (qmcin(11,iyr)                                                   &
                          + qclas(11,iyr) - qctlrf(11,iyr) - ogsupgas(1,11,iyr) * cfngc(iyr) &
                          + qciin(11,iyr)                                                    &
                          + qctlrf(11,iyr))*.001



!           nlink(121,iyr) =(qclas(11,iyr)+qmcin(11,iyr)+qciin(11,iyr))*.001 &
!                          - ogsupgas(1,11,iyr)*cfngc(iyr)*.001
           nlink(122,iyr) = qclas(11,iyr)
           nlink(123,iyr) = qmcin(11,iyr)
           nlink(124,iyr) = qciin(11,iyr)
           nlink(125,iyr) = ogsupgas(1,11,iyr)
           nlink(126,iyr) = cfngc(iyr)
!  Cons_Nuclear
           nlink(127,iyr) = qurel(11,iyr)*.001
           nlink(128,iyr) = qurel(11,iyr)
!  Cons_Hydro
           nlink(129,iyr) = qhoas(11,iyr)*.001
           nlink(130,iyr) = qhoas(11,iyr)
!  Cons_Biomass
           nlink(131,iyr) = (qbmas(11,iyr)-qbmrf(11,iyr))/1000.                                                  &
                          + corncd(3,11,iyr)*cfcorn/1000000000.                                                  &
                          - rfbiobutecd(mnumcr,iyr)*rdays*cfbiobute(iyr)/1000000.                                &
                          - 0.9751*(crnethcd(11,iyr)+cllethcd(11,iyr)+othethcd(11,iyr))/1000.*cfpet*rdays/1000.  &
                          + sum(bimqtycd(1:4,11,iyr))/1000000.*rdays*(cfveggie(iyr)-cfbiod(iyr))                 &
                          + qbmrfbtl(11,iyr)/1000.                                                               &
                          - rdays/1000000.*(sum(btlfrac(1:4,mnumpr,iyr))*cfbtlliq(iyr)+sum(cbtlfrac(2,1:4,mnumpr,iyr))*cfcbtlliq(2,iyr)+ubavol(mnumpr,iyr)*5.763)
           nlink(132,iyr) = (qbmas(11,iyr)-qbmrf(11,iyr))/1000.
           nlink(133,iyr) = corncd(3,11,iyr)*cfcorn/1000000000.
           nlink(134,iyr) = rfbiobutecd(mnumcr,iyr)*rdays*cfbiobute(iyr)/1000000.
           nlink(135,iyr) = 0.9751*(crnethcd(11,iyr)+cllethcd(11,iyr)+othethcd(11,iyr))/1000.*cfpet*rdays/1000.
           nlink(136,iyr) = sum(bimqtycd(1:4,11,iyr))/1000000.*rdays*(cfveggie(iyr)-cfbiod(iyr))
           nlink(137,iyr) = qbmrfbtl(11,iyr)/1000.
           nlink(138,iyr) = rdays/1000000.*(sum(btlfrac(1:4,mnumpr,iyr))*cfbtlliq(iyr)+sum(cbtlfrac(2,1:4,mnumpr,iyr))*cfcbtlliq(2,iyr)+ubavol(mnumpr,iyr)*5.763)
           nlink(139,iyr) = cllethcd(11,iyr)
           nlink(140,iyr) = othethcd(11,iyr)
           nlink(141,iyr) = cfpet
           nlink(142,iyr) = sum(bimqtycd(1:4,11,iyr))
           nlink(143,iyr) = cfveggie(iyr)
           nlink(144,iyr) = cfbiod(iyr)
           nlink(145,iyr) = qbmrfbtl(11,iyr)
           nlink(146,iyr) = sum(btlfrac(1:4,mnumpr,iyr))
           nlink(147,iyr) = cfbtlliq(iyr)
           nlink(148,iyr) = sum(cbtlfrac(2,1:4,mnumpr,iyr))
           nlink(149,iyr) = cfcbtlliq(2,iyr)
           nlink(150,iyr) = ubavol(mnumpr,iyr)
!  Cons_OthRenew
           nlink(151,iyr) =(qtras(11,iyr)-qgers(11,iyr)-qstrs(11,iyr)       &
                          - qstcm(11,iyr)-qpvcm(11,iyr)-qpvrs(11,iyr)       &
                          - qbmas(11,iyr)-qhoas(11,iyr)-qettr(11,iyr))*.001 &
                          - wncmsel(iyr,11)
           nlink(152,iyr) = qtras(11,iyr)
           nlink(153,iyr) = qgers(11,iyr)
           nlink(154,iyr) = qstrs(11,iyr)
           nlink(155,iyr) = qstcm(11,iyr)
           nlink(156,iyr) = qpvcm(11,iyr)
           nlink(157,iyr) = qpvrs(11,iyr)
           nlink(158,iyr) = qbmas(11,iyr)
           nlink(159,iyr) = qhoas(11,iyr)
           nlink(160,iyr) = qettr(11,iyr)
           nlink(161,iyr) = wncmsel(iyr,11)
!  Cons_Other
           nlink(162,iyr) = (qeiel(11,iyr)+qhytr(11,iyr))*.001+wncmsel(iyr,11)
           nlink(163,iyr) = qeiel(11,iyr)
           nlink(164,iyr) = qhytr(11,iyr)
           nlink(165,iyr) = wncmsel(iyr,11)
!  ElGen_Renewable
           nlink(166,iyr) =(qtrel(11,iyr) + qpcel(11,iyr)) * .001
           nlink(167,iyr) = qtrel(11,iyr)
           nlink(168,iyr) = qpcel(11,iyr)
!  DelEng_Coal
!          T2(68,IR,IY,IS)=QMCIN(IR,IY)
!          T2(69,IR,IY,IS)=QCLAS(IR,IY) - QCLEL(IR,IY) - QCTLRF(IR,IY) - OGSUPGAS(1,IR,IY) * CFNGC(IY) * .001
!          T2(70,IR,IY,IS)=QCIIN(IR,IY)
!          T2(119,IR,IY,IS)=QCTLRF(IR,IY)
!          T2(71,IR,IY,IS)=FSUM(T2(68,IR,IY,IS),3) + T2(119,IR,IY,IS)

           nlink(169,iyr) = (QMCIN(11,IYr)                                                                   &
                          + QCLAS(11,IYr) - QCLEL(11,IYr) - QCTLRF(11,IYr) - OGSUPGAS(1,11,IYr) * CFNGC(IYr) &
                          + QCIIN(11,IYr)                                                                    &
                          + QCTLRF(11,IYr))*.001





!           nlink(169,iyr) =(qmcin(11,iyr)                                                                 &
!                          + qclas(11,iyr)-qclel(11,iyr)-qctlrf(11,iyr)-ogsupgas(1,11,iyr)*cfngc(iyr)*.001 &
!                          + qctlrf(11,iyr)                                                                &
!                          + qciin(11,iyr)) * .001
           nlink(170,iyr) = qmcin(11,iyr)
           nlink(171,iyr) = qclas(11,iyr)
           nlink(172,iyr) = qclel(11,iyr)
           nlink(173,iyr) = qciin(11,iyr)
!  DelEng_Electricity
           nlink(174,iyr) = qelas(11,iyr) * .001
           nlink(175,iyr) = qelas(11,iyr)
!  DelEng_NaturalGas
!           T2(67,IR,IY,IS)=T2(117,IR,IY,IS)+T2(120,IR,IY,IS)+T2(128,IR,IY,IS)+T2(116,IR,IY,IS)+T2(49,IR,IY,IS)+T2(52,IR,IY,IS)
!           T2(117,IR,IY,IS)=QNGAS(IR,IY) - QNGEL(IR,IY) - QHYTR(IR,IY) / .7 - QGTLRF(IR,IY)
!           T2(120,IR,IY,IS)=QGTLRF(IR,IY)
!           T2(128,IR,IY,IS)=QNGLQ(IR,IY)
!           T2(116,IR,IY,IS)=QLPIN(IR,IY)
!           T2(49,IR,IY,IS)=QGPTR(IR,IY)
!           T2(52,IR,IY,IS)=0.0

!          QNGAS(IR,IY) - QNGEL(IR,IY) - QHYTR(IR,IY) / .7 +QNGLQ(IR,IY)+QLPIN(IR,IY)+QGPTR(IR,IY)
           
           
           
           
           
           
!          T2(97,IR,IY,IS)=T2(118,IR,IY,IS) + T2(120,IR,IY,IS) + T2(128,IR,IY,IS) +   &
!                          T2(116,IR,IY,IS) + T2( 52,IR,IY,IS) + T2( 49,IR,IY,IS)

!          T2(118,IR,IY,IS)=QNGAS(IR,IY) - QHYTR(IR,IY) / .7 - QGTLRF(IR,IY)
!          T2(120,IR,IY,IS)=QGTLRF(IR,IY)
!          T2(128,IR,IY,IS)=QNGLQ(IR,IY)
!          T2(116,IR,IY,IS)=QLPIN(IR,IY)
!          T2(52,IR,IY,IS)=0.0
!          T2(49,IR,IY,IS)=QGPTR(IR,IY)

           nlink(176,iyr) = (qngas(11,iyr) - qngel(11,iyr) - qhytr(11,iyr)/0.7 + qnglq(11,iyr) + qlpin(11,iyr) + qgptr(11,iyr))*.001




!           nlink(176,iyr) =(qngas(11,iyr) + qgptr(11,iyr) + qlpin(11,iyr) + qnglq(11,iyr) &
!                          - qngel(11,iyr)) * .001
           nlink(177,iyr) = qngas(11,iyr)
           nlink(178,iyr) = qgptr(11,iyr)
           nlink(179,iyr) = qlpin(11,iyr)+qnglq(11,iyr)
           nlink(180,iyr) = qngel(11,iyr)
!  DelEng_Petroleum
!          T2(58,IR,IY,IS)=QDSAS(IR,IY) - QDSEL(IR,IY)
!          T2(59,IR,IY,IS)=QKSAS(IR,IY)
!          T2(60,IR,IY,IS)=QJFTR(IR,IY)                       !  All sectors - don't subtract QJFBS
!          T2(61,IR,IY,IS)=QLGAS(IR,IY)
!          T2(62,IR,IY,IS)=QMGAS(IR,IY) + QETTR(IR,IY) + QMETR(IR,IY)
!          T2(63,IR,IY,IS)=QPFIN(IR,IY)
!          T2(64,IR,IY,IS)=QRSAS(IR,IY) - QRSEL(IR,IY)
!          T2(65,IR,IY,IS)=QOTAS(IR,IY) + QSGIN(IR,IY) + QPCIN(IR,IY) + QASIN(IR,IY)
!          T2(66,IR,IY,IS)=FSUM(T2(58,IR,IY,IS),8)






          nlink(181,iyr) =(qdsas(11,iyr) - qdsel(11,iyr)                  &
                         + qksas(11,iyr)                                  &
                         + qjftr(11,iyr)                                  &
                         + qlgas(11,iyr)                                  &
                         + qmgas(11,iyr) + qettr(11,iyr) + qmetr(11,iyr)  &
                         + qpfin(11,iyr)                                  &
                         + qrsas(11,iyr)  - qrsel(11,iyr)                 &
                         + qotas(11,iyr)  + qsgin(11,iyr) + qpcin(11,iyr) &
                         + qasin(11,iyr)) * .001
           nlink(182,iyr) = qdsas(11,iyr)
           nlink(183,iyr) = qdsel(11,iyr)
           nlink(184,iyr) = qksas(11,iyr)
           nlink(185,iyr) = qjftr(11,iyr)
           nlink(186,iyr) = qlgas(11,iyr)
           nlink(187,iyr) = qmgas(11,iyr)
           nlink(188,iyr) = qpfin(11,iyr)
           nlink(189,iyr) = qrsas(11,iyr)
           nlink(190,iyr) = qrsel(11,iyr)
           nlink(191,iyr) = qotas(11,iyr)
           nlink(192,iyr) = qsgin(11,iyr)
           nlink(193,iyr) = qpcin(11,iyr)
           nlink(194,iyr) = qasin(11,iyr)
!  Deleng_Bio
           nlink(195,iyr) = nlink(131,iyr) &
                          - qbmel(11,iyr)/1000.
           nlink(196,iyr) = qbmel(11,iyr)/1000
           nlink(197,iyr) = qmgbs(11,iyr)
           nlink(198,iyr) = corncd(1,11,iyr)
           nlink(199,iyr) = crnethcd(11,iyr)
           nlink(200,iyr) = othethcd(11,iyr)
           nlink(201,iyr) = qdstr(11,iyr)
!  TranCon_Distillate
           nlink(202,iyr) = qdstr(11,iyr)   * .001
           nlink(203,iyr) = qdstr(11,iyr)
!  ResCon_Petroleum
           nlink(204,iyr) = qtprs(11,iyr)   * .001
           nlink(205,iyr) = qtprs(11,iyr)
!  ResCon_NaturalGas
           nlink(206,iyr) = qngrs(11,iyr)   * .001
           nlink(207,iyr) = qngrs(11,iyr)
!  ResCon_Electricity
           nlink(208,iyr) = qelrs(11,iyr)   * .001
           nlink(209,iyr) = qelrs(11,iyr)
!  ResPrice_Petroleum
           nlink(210,iyr) = ptprs(11,iyr)   *  scaleprice
           nlink(211,iyr) = ptprs(11,iyr)
!  ResPrice_Electricity
           nlink(212,iyr) = pelrs(11,iyr)   *  scaleprice
           nlink(213,iyr) = pelrs(11,iyr)
!  ResPrice_NaturalGas
           nlink(214,iyr) = pngrs(11,iyr)   *  scaleprice
           nlink(215,iyr) = pngrs(11,iyr)
!  TranPrice_MotorGas
           nlink(216,iyr) = pmgtr(11,iyr)   *  scaleprice
           nlink(217,iyr) = pmgtr(11,iyr)
!  TranPrice_Distillate
           nlink(218,iyr) = pdstr(11,iyr)   *  scaleprice
           nlink(219,iyr) = pdstr(11,iyr)
!  IndPrice_SteamCoal
           nlink(220,iyr) = pclin(11,iyr)   *  scaleprice
           nlink(221,iyr) = pclin(11,iyr)
!  GasWellheadPrice
           nlink(222,iyr) = ogwprng(mnumor,iyr) *  scaleprice
           nlink(223,iyr) = ogwprng(mnumor,iyr)
!  ComPrice_Electricity
           nlink(224,iyr) = pelcm(11,iyr)   *  scaleprice
           nlink(225,iyr) = pelcm(11,iyr)
!  IndPrice_Electricity
           nlink(226,iyr) = pelin(11,iyr)   *  scaleprice
           nlink(227,iyr) = pelin(11,iyr)
!  TranPrice_Electricity
           nlink(228,iyr) = peltr(11,iyr)   *  scaleprice
           nlink(229,iyr) = peltr(11,iyr)
!  ComPrice_NaturalGas
           nlink(230,iyr) = pngcm(11,iyr)   *  scaleprice
           nlink(231,iyr) = pngcm(11,iyr)
!  IndPrice_NaturalGas
           nlink(232,iyr) = pngin(11,iyr)   *  scaleprice
           nlink(233,iyr) = pngin(11,iyr)
!  TranPrice_NaturalGas
           nlink(234,iyr) = pngtr(11,iyr)   *  scaleprice
           nlink(235,iyr) = pngtr(11,iyr)
!  ElGenPrice_NaturalGas
           nlink(236,iyr) = pngel(11,iyr)   *  scaleprice
           nlink(237,iyr) = pngel(11,iyr)
!  WorldOilPrice
           nlink(238,iyr) = it_wop(iyr,1)   *  scaleprice
           nlink(239,iyr) = it_wop(iyr,1)
!  ComPrice_Distillate
           nlink(240,iyr) = pdscm(11,iyr)   *  scaleprice
           nlink(241,iyr) = pdscm(11,iyr)
!  ComPrice_ResidualFuel
           nlink(242,iyr) = prscm(11,iyr)   *  scaleprice
           nlink(243,iyr) = prscm(11,iyr)
!  IndPrice_Distillate
           nlink(244,iyr) = pdsin(11,iyr)   *  scaleprice
           nlink(245,iyr) = pdsin(11,iyr)
!  IndPrice_ResidualFuel
           nlink(246,iyr) = prsin(11,iyr)   *  scaleprice
           nlink(247,iyr) = prsin(11,iyr)
!  TranPrice_JetFuel
           nlink(248,iyr) = pjftr(11,iyr)   *  scaleprice
           nlink(249,iyr) = pjftr(11,iyr)
!  TranPrice_ResidualFuel
           nlink(250,iyr) = prstr(11,iyr)   *  scaleprice
           nlink(251,iyr) = prstr(11,iyr)
!  CarbEm_ElGenTotal
           nlink(252,iyr) = emel(2,1,iyr) &
                          + emel(1,1,iyr) &
                          + emel(3,1,iyr)
           nlink(253,iyr) = emel(2,1,iyr)
           nlink(254,iyr) = emel(1,1,iyr)
           nlink(255,iyr) = emel(3,1,iyr)
!  CarbPen_Resulting
           nlink(256,iyr) = emetax(1,iyr) * 1000. * scaleprice
           nlink(257,iyr) = emetax(1,iyr)
!  TranPrice_AvgVehicle (Converting base year of price to 2012 for GI model from 1990 base year in TRAN.F)
           nlink(258,iyr) = avg_prc_veh(iyr) * 1.570444239414224

           nlink(259,iyr) = avg_prc_veh(iyr)
!  TranPrice_AvgCar (Converting base year of price to 2012 for GI model from 1990 base year in TRAN.F)
           nlink(260,iyr) = avg_prc_car(iyr) * 1.570444239414224
           nlink(261,iyr) = avg_prc_car(iyr)
!  TranPrice_AvgTruck (Converting base year of price to 2012 for GI model from 1990 base year in TRAN.F)
           nlink(262,iyr) = avg_prc_trk(iyr) * 1.570444239414224
           nlink(263,iyr) = avg_prc_trk(iyr)
!  Incremental tax on petroleum based highway fuels
           nlink(264,iyr) = hfueltax(iyr) * scaleprice
           nlink(265,iyr) = hfueltax(iyr)
!  NEMS Emission revenues by sector (1=residential, 2=commercial, 3=industrial, 4=transportation
!   and 5=electric power) in billions of nominal dollars
           nlink(266,iyr) = emrev(6,iyr) * scaleprice / mc_jpgdp(iyr)
           nlink(267,iyr) = emrev(1,iyr)
           nlink(268,iyr) = emrev(2,iyr)
           nlink(269,iyr) = emrev(3,iyr)
           nlink(270,iyr) = emrev(4,iyr)
           nlink(271,iyr) = emrev(5,iyr)
!  ElSec_InstCap, Total Resource Costs, Electric Sector, Annual Expense and Capital Commitment, Installed Capacity in Billion 2012 Dollars
           nlink(272,iyr) = g_inst_all(mnumnr,iyr)*scaleprice*0.001
           nlink(273,iyr) = g_inst_all(mnumnr,iyr)
!  ElSec_Tran, Total Resource Costs, Electric Sector, Annual Expense and Capital Commitment, Transmission in Billion 2012 Dollars
           nlink(274,iyr) = t_ovr(mnumnr,iyr)*scaleprice*0.001
           nlink(275,iyr) = t_ovr(mnumnr,iyr)
!  ElSec_Retro, Total Resource Costs, Electric Sector, Annual Expense and Capital Commitment, Retrofits in Billion 2012 Dollars
           nlink(276,iyr) = ret_inv(mnumnr,iyr)*scaleprice*0.001
!  NEMS Covered GHG emissions revenue (covered emissions*full carbon/ghg trading price) in billions of 1987 dollars.
           nlink(277,iyr) = ghg_rev(1,iyr) * scaleprice
           nlink(278,iyr) = ghg_rev(1,iyr)
!  NEMS International offset revenue (abatement*offset price) in billions of 1987 dollars.
           nlink(279,iyr) = emrev(11,iyr) * scaleprice / mc_jpgdp(iyr)
           nlink(280,iyr) = emrev(11,iyr)
!  NEMS Share of allowances that are auctioned.
           nlink(281,iyr) = em_auction_sh(iyr)
!  JPGDP (Converting base year of deflator to 2012 for GI model from 1987 base year in NEMS)
           nlink(282,iyr) = mc_jpgdp(iyr)/scaleprice
!  NEMS Industrial Output 25: Petroleum Refining (NAICS 32411), manufacturing
           nlink(283,iyr) =(rfqprdt(11,iyr)-rfpqiprdt(mnumpr,iyr,2))
           nlink(284,iyr) = rfqprdt(11,iyr)
           nlink(285,iyr) = rfpqiprdt(mnumpr,iyr,2)
!  NEMS Industrial Output 45: Coal Mining (NAICS 2121), non-manufacturing
           nlink(286,iyr) = cqsbb(3,iyr)
!  NEMS Industrial Output 46: Oil and Gas Extraction and Support Activities (NAICS 211, 213), non-manufacturing
           nlink(287,iyr) =((sum(ogoilprd(:,5,iyr))*rdays*cfcrddom(iyr))     &
                          + (sum(ogngplprd(:,iyr))*rdays*cfngl(iyr)) &
                          + ((ogprdng(mnumor,iyr)+ogprsup(iyr))*1.031))
           nlink(288,iyr) = sum(ogoilprd(:,5,iyr))
           nlink(289,iyr) = sum(ogngplprd(:,iyr))
           nlink(290,iyr) = ogprdng(mnumor,iyr)
           nlink(291,iyr) = ogprsup(iyr)
!  NEMS Industrial Output 51: Electric Power Generation and Distribution (NAICS 2211), non-industrial/services
           nlink(292,iyr) =(ugntlnr(1,mnumnr,iyr)+ugntlnr(2,mnumnr,iyr))
           nlink(293,iyr) = ugntlnr(1,mnumnr,iyr)
           nlink(294,iyr) = ugntlnr(2,mnumnr,iyr)
!  NEMS Industrial Output 52: Natural Gas Distribution (NAICS 2212), non-industrial/services
           nlink(295,iyr) = oghhprng(iyr)
!  NEMS Employment 36: Coal Mining (NAICS 2121),non-manufacturing
           nlink(296,iyr) = totminers(iyr)
!  NEMS Employment 37: Oil and Gas Extraction and Support Activities (NAICS 211, 213),non-manufacturing
           nlink(297,iyr) = ogjobs(iyr)
!  HenryHubPrice
           nlink(298,iyr) = oghhprng(iyr)  * scaleprice
           nlink(299,iyr) = oghhprng(iyr)
!  ComCon_Electricity
           nlink(300,iyr) = qelcm(11,iyr)  * .001
           nlink(301,iyr) = qelcm(11,iyr)
!  IndCon_Electricity
           nlink(302,iyr) = qelin(11,iyr)  * .001
           nlink(303,iyr) = qelin(11,iyr)
!  TranCon_Electricity
           nlink(304,iyr) = qeltr(11,iyr)  * .001
           nlink(305,iyr) = qeltr(11,iyr)
!  ComCon_NaturalGas
           nlink(306,iyr) = qngcm(11,iyr)  * .001
           nlink(307,iyr) = qngcm(11,iyr)
!  IndCon_NaturalGas
           nlink(308,iyr) = qngin(11,iyr)  * .001
           nlink(309,iyr) = qngin(11,iyr)
!  TranCon_NaturalGas
           nlink(310,iyr) = qngtr(11,iyr)  * .001
           nlink(311,iyr) = qngtr(11,iyr)
!  ElGenCon_NaturalGas
           nlink(312,iyr) = qngel(11,iyr)  * .001
           nlink(313,iyr) = qngel(11,iyr)
!  ElGenCon_SteamCoal
           nlink(314,iyr) = qclel(11,iyr)  * .001
           nlink(315,iyr) = qclel(11,iyr)
!  ElGenCon_FuelOil
           nlink(316,iyr) =(qdsel(11,iyr) + qrsel(11,iyr)) * .001
           nlink(317,iyr) = qdsel(11,iyr)
           nlink(318,iyr) = qrsel(11,iyr)
!  ElGenCon_ElecImports
           nlink(319,iyr) = qeiel(11,iyr)  * .001
           nlink(320,iyr) = qeiel(11,iyr)
!  ElGenCon_ElecPowerTot
           nlink(321,iyr) = qtsel(11,iyr)  * .001
           nlink(322,iyr) = qtsel(11,iyr)
!  ComCon_Distillate
           nlink(323,iyr) = qdscm(11,iyr)  * .001
           nlink(324,iyr) = qdscm(11,iyr)
!  ComCon_ResidualFuel
           nlink(325,iyr) = qrscm(11,iyr)  * .001
           nlink(326,iyr) = qrscm(11,iyr)
!  IndCon_Distillate
           nlink(327,iyr) = qdsin(11,iyr)  * .001
           nlink(328,iyr) = qdsin(11,iyr)
!  IndCon_ResidualFuel
           nlink(329,iyr) = qrsin(11,iyr)  * .001
           nlink(330,iyr) = qrsin(11,iyr)
!  TranCon_MotorGas
           nlink(331,iyr) = qmgtr(11,iyr)  * .001
           nlink(332,iyr) = qmgtr(11,iyr)
!  TranCon_ResidualFuel
           nlink(333,iyr) = qrstr(11,iyr)  * .001
           nlink(334,iyr) = qrstr(11,iyr)
!  Ethanol_Imports
           nlink(335,iyr) = ethimp(11,iyr) * rdays * 42. / 1000000.
           nlink(336,iyr) = ethimp(11,iyr)
!  Ethanol_WholesalePrice
           nlink(337,iyr) = pethm(11,iyr)  * scaleprice / 42.
           nlink(338,iyr) = pethm(11,iyr)
!  Corn_Price
           nlink(339,iyr) = crnprice(11,iyr) * scaleprice
           nlink(340,iyr) = crnprice(11,iyr) * scaleprice
!  TranCon_Ethanol
           nlink(341,iyr) = qettr(11,iyr)  * .001
           nlink(342,iyr) = qettr(11,iyr)
!  TranPrice_Ethanol
           nlink(343,iyr) = pettr(11,iyr)  * scaleprice
           nlink(344,iyr) = pettr(11,iyr)
!  WestTexasOilPrice
           nlink(345,iyr) = wti_price(iyr) * scaleprice
           nlink(346,iyr) = wti_price(iyr)
!  LPGFeedstockPrice
           nlink(347,iyr) = plginpf(11,iyr) * scaleprice
           nlink(348,iyr) = plginpf(11,iyr)
!  NaphthaPrice
           nlink(349,iyr) = ppfin(11,iyr) * scaleprice
           nlink(350,iyr) = ppfin(11,iyr)
!  EthanePrice
           nlink(351,iyr) = petin(11,iyr) * scaleprice
           nlink(352,iyr) = petin(11,iyr)
!  EthaneProduction
           nlink(353,iyr) = sum(ogngplet(:,iyr))
           nlink(354,iyr) = sum(ogngplet(:,iyr))
!  PropaneProduction
           nlink(355,iyr) = sum(ogngplpr(:,iyr))
           nlink(356,iyr) = sum(ogngplpr(:,iyr))
!  RINRevenue
           nlink(357,iyr) = rfs_rev(iyr) * scaleprice
           nlink(358,iyr) = rfs_rev(iyr)


!  Ethanol_IP
           nlink(359,iyr)   = 0.9751*(crnethcd(11,iyr)+cllethcd(11,iyr)+othethcd(11,iyr)+grnethcd(11,iyr))*rdays*42/1000.
           nlink(360,iyr)   = pethm(11,iyr)/42*scaleprice


!  Miscellaneous parameters
           nlink(361,iyr) = rdays
           nlink(362,iyr) = scaleprice

!  END DO IYR: Loop across components of energy variables contained
!   in the restart file by year.
         END DO


!  Open MC.ENERGY.CSV for write of nlink data.
         OPEN(3,ACTION='WRITE',FILE='MC_ENERGY.CSV')
!  Write header.
         line = '"'//TRIM(scen)//' '//TRIM(date)//' '//TRIM(comment)//'"'
         charc = LEN(TRIM(line))
         WRITE(3,'(A<charc>)') line
         WRITE(3,*) ' '
!  Write title.
         line = '"'//"NEMS ENERGY DRIVER VARIABLES AND COMPONENTS"//'"'
         charc = LEN(TRIM(line))
         WRITE(3,'(A<charc>)') line
!  Write dates across columns.
         line = '"'//"obs"//'"'
         DO i = 1990,mamlastyr
           WRITE(label,'(i4)') i
           line = TRIM(line)//',"'//TRIM(label)//'"'
         END DO
         charc = LEN(TRIM(line))
         WRITE(3,'(A<charc>)') line
!  Write label and data by time series.
         DO j = 1,nemsenergynum
           line = '"'//TRIM(nemsenergylabel(j))//'"'
           DO i = 1,(mamlastyr-1990+1)
             WRITE(label,'(F15.5)') nlink(j,i)
             line = TRIM(line)//',"'//TRIM(label)//'"'
           END DO
           charc = LEN(TRIM(line))
           WRITE(3,'(A<charc>)') line
         END DO
!  Close MC_ENERGY.CSV file.
         CLOSE(3)


!  START IF (macfdbk .EQ. 1): Execute the GI and EIA models in EViews if macro
!     feedback is on.
         IF (macfdbk .EQ. 1) THEN

           CALL drtlink
           
         END IF


!  END IF: Call subroutines readmac and drtlink in first year, first iteration.
       END IF

!  Skip to the end if this is a stand-alone for GI data.
   IF (giswitch .GE. 0) THEN
     GOTO 100

   ENDIF


!  START IF ((baseyr+curiyr-1) .GE. 1990 .AND. macfdbk .EQ. 1): Overwrite
!   epmac matrix beginning in 1990 if macro feedback is on with forecast
!   from EViews models.
       IF ((baseyr+curiyr-1) .GE. 1990 .AND. macfdbk .EQ. 1) THEN

!  START DO I: Overwrite epmac matrix beginning in 1990 if macro feedback
!   is on with forecast from EViews models. 'numepmac' is the number of variables
!   returned from EViews.
         DO i = 1,numepmac
           epmac(i,curiyr) = myinputs(i,curiyr)
!  END DO I: Overwrite epmac matrix beginning in 1990 if macro feedback
!   is on with forecast from EViews models. 'numepmac' is the number of variables
!   returned from EViews.
         END DO
!  END IF ((baseyr+curiyr-1) .GE. 1990 .AND. macfdbk .EQ. 1): Overwrite
!   epmac matrix beginning in 1990 if macro feedback is on with forecast
!   from EViews models.
       END IF

!  START DO I: Loop through number of national and regional macro variables.
       DO i = 1,mcnmmac+mcnmnatreg
!  START IF ((baseyr+curiyr-1) .GE. 1990): Compute a forecast beginning in
!   2023. 'baseyr' in NEMS is 1990. 'curiyr' is an index 1990=1.
         IF ((baseyr+curiyr-1) .GE. 1990) THEN
!  START IF (i .LE. mcnmmac): Copy EViews model solutions to esmac matrix from
!   epmac matrix. 'mcnmmac' is the number of national macro variables.
           IF (i .LE. mcnmmac) THEN
             esmac(i,curiyr) = epmac(i,curiyr)
!  START ELSE (i .LE. mcnmmac): Writing US forecast to esmacreg from mc_regmac.
           ELSE
             esmacreg(11,i-mcnmmac,curiyr) = mc_regmac(126+(i-mcnmmac),curiyr)
!  END IF (i .LE. mcnmmac): Copy EViews model solutions to esmac matrix from
!   epmac matrix. 'mcnmmac' is the number of national macro variables.
           END IF
!  START EASE ((baseyr+curiyr-1) .GE. 1990): Return historical values prior
!   to 2023. 'baseyr' in NEMS is 1990. 'curiyr' is an index 1990=1.
         ELSE

!  START IF ((i-mcnmmac) .LE. 10): Write US history to esmacreg from mc_regmac
           IF ((i-mcnmmac) .LE. 10) THEN
             esmacreg(11,i-mcnmmac,curiyr) = mc_regmac(248+(i-mcnmmac),curiyr)
!  START ELSE IF ((i-mcnmmac) .GE. 11 .AND. (i-mcnmmac) .LE. 12): Write US history
!   for populations np and np16a to esmacreg from mc_regmac.
           ELSE IF ((i-mcnmmac) .GE. 11 .AND. (i-mcnmmac) .LE. 12) THEN
             esmacreg(11,i-mcnmmac,curiyr) = mc_regmac(126+(i-mcnmmac),curiyr)
!  START ELSE: Write history for wages rwm and rwnm to esmacreg from mc_regmac
           ELSE
             esmacreg(11,i-mcnmmac,curiyr) = mc_regmac(126+(i-mcnmmac),curiyr)
!  END IF ((i-mcnmmac) .LE. 10): Write US history to esmacreg from mc_regmac
           END IF
!  END IF ((baseyr+curiyr-1) .GE. 1990): Compute a forecast beginning in
!   2023. 'baseyr' in NEMS is 1990. 'curiyr' is an index 1990=1.
         END IF
!  END DO I: Loop through number of national and regional macro variables.
       END DO

!  Call the Interindustry Submodule.
       WRITE(*,*) "MAC.F: Calling industsub"
       CALL industsub

!  Call the Regional Submodule.
       WRITE(*,*) "MAC.F: Calling regionsub"
       CALL regionsub

!  Call the Employment Submodule.
       WRITE(*,*) "MAC.F: Calling employment"
       CALL employment

!  Call the Commercial Floorspace Submodule
       WRITE(*,*) "MAC.F: Calling commercial floorspace"
       CALL comflr

!  Call the Transportation Size Class Submodule
       WRITE(*,*) "MAC.F: Calling transportation size class"
       CALL tranc


!  Start writes of macro variables to the common block.


!  Energy driver variables.


!  prod_crudeoil
       mc_engdompet(curiyr)    = nlink(1,curiyr)+nlink(4,curiyr)
       mamenergy(1,curiyr)     = mc_engdompet(curiyr)

!  prod_other = 
!  prod_nuclear + prod_hydro + prod_othrenew
       mc_engdomoo(curiyr)     = nlink(14,curiyr)+nlink(16,curiyr) &
                               + nlink(29,curiyr)
       mamenergy(2,curiyr)     = mc_engdomoo(curiyr)

!  prod_biomass
       mc_engdombio(curiyr)    = nlink(18,curiyr)
       mamenergy(3,curiyr)     = mc_engdombio(curiyr)

!  prod_coal
       mc_engdomcoal(curiyr)   = nlink(11,curiyr)
       mamenergy(4,curiyr)     = mc_engdomcoal(curiyr)

!  prod_naturalgas = 
!  prod_naturalgas + prod_drynaturalgas
       mc_engdomng(curiyr)     = nlink(4,curiyr)+nlink(7,curiyr)
       mamenergy(5,curiyr)     = mc_engdomng(curiyr)


!  imports of biofuels (quads)
       mc_engimpbio(curiyr)   = (ethimp(11,curiyr)/1000*cfpet+biodimp(11,curiyr)/1000*cfbiod(curiyr))*rdays*0.001
       mamenergy(6,curiyr)    = mc_engimpbio(curiyr)

!  imports of coal
       mc_engimpcoal(curiyr) = cqdbfb(11,7,curiyr)*.001
       mamenergy(7,curiyr)   = mc_engimpcoal(curiyr)

!  imports of natural gas
       mc_engimpng(curiyr) = nlink(65,curiyr)
       mamenergy(8,curiyr) = mc_engimpng(curiyr)

!  imports of other fuels
       mc_engimpoo(curiyr) = qciin(11,curiyr)+qeiel(11,curiyr)
       mamenergy(9,curiyr) = mc_engimpoo(curiyr)

!  imports of petroleum
       mc_engimppet(curiyr) = ((rfqicrd(mnumpr,curiyr)+rfsprim(curiyr))*cfcrdimp(curiyr) &
                            +   rfpqiprdt(mnumpr,curiyr,2)*cfimprd(curiyr)               &
                            +   rfipqcbob(mnumpr,curiyr,2)*cfcbob(curiyr)/1000.          &
                            +   rfipqrbob(mnumpr,curiyr,2)*cfrbob(curiyr)/1000.          &
                            +   rfmtbi(mnumpr,curiyr)*4.24                               &
                            +   rfpqufc(mnumpr,curiyr,2)*cfimuo(curiyr))*rdays*.001
       mamenergy(10,curiyr) = mc_engimppet(curiyr)

!  imports of energy
!  import_biofuels + import_coal + import_natgas + import_other + import_petroleum
       mc_engimp(curiyr)       = mc_engimpbio(curiyr) + mc_engimpcoal(curiyr) + mc_engimpng(curiyr) &
                               + mc_engimpoo(curiyr)  + mc_engimppet(curiyr)
       mamenergy(11,curiyr)    = mc_engimp(curiyr)


!  exports of fuels
       mc_engexp(curiyr)    = ((rfqexcrd(mnumpr,curiyr)*.001*cfcrdexp(curiyr)  &
                            +   rfqexprdt(mnumpr,curiyr)*cfexprd(curiyr)       &
                            +   ethexp(11,curiyr)/1000.*cfpet                  &
                            +   biodexp(11,curiyr)/1000.*cfbiod(curiyr))*rdays &
                            +   ngexpvol(4,curiyr)*cfnge(curiyr)               &
                            +  cqdbfb(11,5,curiyr))*.001
       mamenergy(12,curiyr) = mc_engexp(curiyr)

!  export of biofuels
       mc_engexpbio(curiyr) = (ethexp(11,curiyr)*cfpet           &
                            +  biodexp(11,curiyr)*cfbiod(curiyr) &
                            +  biobuteexp(curiyr)*cfbiobute(curiyr))*rdays/1000000.
       mamenergy(13,curiyr) = mc_engexpbio(curiyr)

!  export of coal
       mc_engexpcoal(curiyr) = cqdbfb(11,5,curiyr)*.001
       mamenergy(14,curiyr)  = mc_engexpcoal(curiyr)

!  export of natual gas
       mc_engexpng(curiyr)  = ngexpvol(4,curiyr)*cfnge(curiyr)*.001
       mamenergy(15,curiyr) = mc_engexpng(curiyr)

!  export of petroleum
       mc_engexppet(curiyr) = (rfqexcrd(mnumpr,curiyr)*cfcrdexp(curiyr)/1000000. + rfqexprdt(mnumpr,curiyr)*cfexprd(curiyr)/1000.)*rdays
       mamenergy(16,curiyr) = mc_engexppet(curiyr)

!  export of other fuels
       mc_engexpoo(curiyr)   = 0
       mamenergy(17,curiyr)  = mc_engexpoo(curiyr)


!  cons_petroleum + cons_natgas + cons_coal + cons_nuclear + cons_hydro + cons_biofuels + cons_othrenew + cons_other
       mc_dallfuels(curiyr)    = nlink(110,curiyr)  + nlink(114,curiyr) + nlink(121,curiyr) &
                               + nlink(127,curiyr) + nlink(129,curiyr)+ nlink(131,curiyr)   &
                               + nlink(151,curiyr) + nlink(162,curiyr)
       mamenergy(18,curiyr)    = mc_dallfuels(curiyr)

!  cons_biofuels
       mc_dallfuelsbio(curiyr) = nlink(131,curiyr)
       mamenergy(19,curiyr)    = mc_dallfuelsbio(curiyr)

!  cons_coal
       mc_dallfuelscoal(curiyr) = nlink(121,curiyr)
       mamenergy(20,curiyr)     = mc_dallfuelscoal(curiyr)

!  cons_natgas
       mc_dallfuelsng(curiyr) = nlink(114,curiyr)
       mamenergy(21,curiyr)   = mc_dallfuelsng(curiyr)

!  cons_nuclear + cons_hydro + cons_othrenew + cons_other
       mc_dallfuelsoo(curiyr) = nlink(127,curiyr) + nlink(129,curiyr)   &
                              + nlink(151,curiyr) + nlink(162,curiyr)
       mamenergy(22,curiyr)   = mc_dallfuelsoo(curiyr)

!  cons_petroleum
       mc_dallfuelspet(curiyr) = nlink(110,curiyr)
       mamenergy(23,curiyr)    = mc_dallfuelspet(curiyr)


!  deleng_coal
       mc_denducoal(curiyr)    = nlink(169,curiyr)
       mamenergy(24,curiyr)    = mc_denducoal(curiyr)

!  deleng_electricity
       mc_denduelc(curiyr)     = nlink(174,curiyr)
       mamenergy(25,curiyr)    = mc_denduelc(curiyr)

!  deleng_naturalgas
       mc_dendung(curiyr)      = nlink(176,curiyr)
       mamenergy(26,curiyr)    = mc_dendung(curiyr)

!  deleng_petroleum
       mc_dendupet(curiyr)     = nlink(181,curiyr)
       mamenergy(27,curiyr)    = mc_dendupet(curiyr)

!  deleng_bio
       mc_dendubio(curiyr)     = nlink(195,curiyr)
       mamenergy(28,curiyr)    = mc_dendubio(curiyr)


!  trancon_distillate + trancon_motorgas + trancon_ethanol
       mc_qgasasf(curiyr)      = nlink(202,curiyr)+nlink(331,curiyr)+nlink(341,curiyr)
       mamenergy(29,curiyr)    = mc_qgasasf(curiyr)

!  rescon_petroleum
       mc_cnefaor(curiyr)      = nlink(204,curiyr)
       mamenergy(30,curiyr)    = mc_cnefaor(curiyr)

!  rescon_naturalgas
       mc_csvugr(curiyr)       = nlink(206,curiyr)
       mamenergy(31,curiyr)    = mc_csvugr(curiyr)

!  rescon_electricity
       mc_csvuer(curiyr)       = nlink(208,curiyr)
       mamenergy(32,curiyr)    = mc_csvuer(curiyr)

!  trancon_distillate + trancon_motorgas + transcon_ethanol
       mc_cnegaor(curiyr)      = nlink(202,curiyr)*nlink(218,curiyr) &
                               + nlink(331,curiyr)*nlink(216,curiyr) &
                               + nlink(341,curiyr)*nlink(343,curiyr)
       mamenergy(33,curiyr)    = mc_cnegaor(curiyr)

!  resprice_petroleum * jpgdp
       mc_jpcnefao(curiyr)     = nlink(210,curiyr)*nlink(282,curiyr)
       mamenergy(34,curiyr)    = mc_jpcnefao(curiyr)

!  resprice_electricity * jpgdp
       mc_jpcsvue(curiyr)      = nlink(212,curiyr)*nlink(282,curiyr)
       mamenergy(35,curiyr)    = mc_jpcsvue(curiyr)

!  resprice_naturalgas * jpgdp
       mc_jpcsvug(curiyr)      = nlink(214,curiyr)*nlink(282,curiyr)
       mamenergy(36,curiyr)    = mc_jpcsvug(curiyr)

!  (((tranprice_motorgas*jpgdp)*trancon_motorgas)
!  + ((tranprice_distillate*jpgdp)*trancon_distillate))
!  + ((tranprice_ethanol*jpgdp)*trancon_ethanol))
!  / (trancon_distillate + trancon_motorgas+trancon_ethanol)
!
!  START IF ((nlink(331,curiyr)+nlink(202,curiyr)+nlink(341,curiyr)) .NE. 0.0): For some
!   reason, it is possible that trans diesel, motor gas and
!   ethanol consumption can be zero.
       IF ((nlink(331,curiyr)+nlink(202,curiyr)+nlink(341,curiyr)) .NE. 0.0) THEN
         mc_jpcnegao(curiyr)   = (((nlink(216,curiyr)*nlink(282,curiyr))*nlink(331,curiyr))  &
                               +  ((nlink(218,curiyr)*nlink(282,curiyr))*nlink(202,curiyr))  &
                               +  ((nlink(343,curiyr)*nlink(282,curiyr))*nlink(341,curiyr))) &
                               /   (nlink(331,curiyr)+nlink(202,curiyr)+nlink(341,curiyr))
!  START ELSE ((nlink(331,curiyr)+nlink(202,curiyr)+nlink(341,curiyr)) .NE. 0.0): For some
!   reason, it is possible that trans diesel and motorgas consumption
!   can be zero.
       ELSE
         mc_jpcnegao(curiyr)   = 0.0
!  END IF ((nlink(331,curiyr)+nlink(202,curiyr)+nlink(341,curiyr)) .NE. 0.0): For some
!   reason, it is possible that trans diesel, motor gas and ethanol consumption
!   can be zero.
       END IF
       mamenergy(37,curiyr)    = mc_jpcnegao(curiyr)

!  indprice_steamcoal * jpgdp
       mc_wpi051(curiyr)       = nlink(220,curiyr)*nlink(282,curiyr)
       mamenergy(38,curiyr)    = mc_wpi051(curiyr)

!  gaswellheadprice * jpgdp
       mc_wpi053(curiyr)       = nlink(222,curiyr)*nlink(282,curiyr)
       mamenergy(39,curiyr)    = mc_wpi053(curiyr)

!  ((resprice_electricity *(rescon_electricity /(rescon_electricity+comcon_electricity+indcon_electricity+trancon_electricity)))   &
!  +(comprice_electricity *(comcon_electricity /(rescon_electricity+comcon_electricity+indcon_electricity+trancon_electricity)))   &
!  +(indprice_electricity *(indcon_electricity /(rescon_electricity+comcon_electricity+indcon_electricity+trancon_electricity)))   &
!  +(tranprice_electricity*(trancon_electricity/(rescon_electricity+comcon_electricity+indcon_electricity+trancon_electricity))))  &
!  * jpgdp
       seccon = nlink(208,curiyr)+nlink(300,curiyr)+nlink(302,curiyr)+nlink(304,curiyr)
       mc_wpi054(curiyr)       = ((nlink(212,curiyr)*(nlink(208,curiyr)/seccon))  &
                               +  (nlink(224,curiyr)*(nlink(300,curiyr)/seccon))  &
                               +  (nlink(226,curiyr)*(nlink(302,curiyr)/seccon))  &
                               +  (nlink(228,curiyr)*(nlink(304,curiyr)/seccon))) &
                               * nlink(282,curiyr)
       mamenergy(40,curiyr)    = mc_wpi054(curiyr)

!  ((resprice_naturalgas  *(rescon_naturalgas  /(rescon_naturalgas+comcon_naturalgas+indcon_naturalgas+trancon_naturalgas+elgencon_naturalgas)))   &
!  +(comprice_naturalgas  *(comcon_naturalgas  /(rescon_naturalgas+comcon_naturalgas+indcon_naturalgas+trancon_naturalgas+elgencon_naturalgas)))   &
!  +(indprice_naturalgas  *(indcon_naturalgas  /(rescon_naturalgas+comcon_naturalgas+indcon_naturalgas+trancon_naturalgas+elgencon_naturalgas)))   &
!  +(tranprice_naturalgas *(trancon_naturalgas /(rescon_naturalgas+comcon_naturalgas+indcon_naturalgas+trancon_naturalgas+elgencon_naturalgas)))   &
!  +(elgenprice_naturalgas*(elgencon_naturalgas/(rescon_naturalgas+comcon_naturalgas+indcon_naturalgas+trancon_naturalgas+elgencon_naturalgas))))  &
!  * jpgdp
       seccon = nlink(206,curiyr)+nlink(306,curiyr)+nlink(308,curiyr)+nlink(310,curiyr)+nlink(312,curiyr)
       mc_wpi055(curiyr)       = ((nlink(214,curiyr)*(nlink(206,curiyr)/seccon))  &
                               +  (nlink(230,curiyr)*(nlink(306,curiyr)/seccon))  &
                               +  (nlink(232,curiyr)*(nlink(308,curiyr)/seccon))  &
                               +  (nlink(234,curiyr)*(nlink(310,curiyr)/seccon))  &
                               +  (nlink(236,curiyr)*(nlink(312,curiyr)/seccon))) &
                               * nlink(282,curiyr)
       mamenergy(41,curiyr)    = mc_wpi055(curiyr)

!  worldoilprice * jpgdp
       mc_wpi0561(curiyr)      = nlink(238,curiyr)*nlink(282,curiyr)
       mamenergy(42,curiyr)    = mc_wpi0561(curiyr)

!  ((resprice_petroleum    *(rescon_petroleum    /(rescon_petroleum+comcon_distillate+comcon_residualfuel+indcon_distillate+indcon_residualfuel+trancon_distillate+trancon_jetfuel+trancon_motorgas+trancon_residualfuel)))  &
!  +(comprice_distillate   *(comcon_distillate   /(rescon_petroleum+comcon_distillate+comcon_residualfuel+indcon_distillate+indcon_residualfuel+trancon_distillate+trancon_jetfuel+trancon_motorgas+trancon_residualfuel)))  &
!  +(comprice_residualfuel *(comcon_residualfuel /(rescon_petroleum+comcon_distillate+comcon_residualfuel+indcon_distillate+indcon_residualfuel+trancon_distillate+trancon_jetfuel+trancon_motorgas+trancon_residualfuel)))  &
!  +(indprice_distillate   *(indcon_distillate   /(rescon_petroleum+comcon_distillate+comcon_residualfuel+indcon_distillate+indcon_residualfuel+trancon_distillate+trancon_jetfuel+trancon_motorgas+trancon_residualfuel)))  &
!  +(indprice_residualfuel *(indcon_residualfuel /(rescon_petroleum+comcon_distillate+comcon_residualfuel+indcon_distillate+indcon_residualfuel+trancon_distillate+trancon_jetfuel+trancon_motorgas+trancon_residualfuel)))  &
!  +(tranprice_distillate  *(trancon_distillate  /(rescon_petroleum+comcon_distillate+comcon_residualfuel+indcon_distillate+indcon_residualfuel+trancon_distillate+trancon_jetfuel+trancon_motorgas+trancon_residualfuel)))  &
!  +(tranprice_jetfuel     *(trancon_jetfuel     /(rescon_petroleum+comcon_distillate+comcon_residualfuel+indcon_distillate+indcon_residualfuel+trancon_distillate+trancon_jetfuel+trancon_motorgas+trancon_residualfuel)))  &
!  +(tranprice_motorgas    *(trancon_motorgas    /(rescon_petroleum+comcon_distillate+comcon_residualfuel+indcon_distillate+indcon_residualfuel+trancon_distillate+trancon_jetfuel+trancon_motorgas+trancon_residualfuel)))  &
!  +(tranprice_residualfuel*(trancon_residualfuel/(rescon_petroleum+comcon_distillate+comcon_residualfuel+indcon_distillate+indcon_residualfuel+trancon_distillate+trancon_jetfuel+trancon_motorgas+trancon_residualfuel)))) &
!  * jpgdp
       seccon = nlink(204,curiyr)+nlink(323,curiyr)+nlink(325,curiyr)+nlink(327,curiyr)+nlink(329,curiyr) &
              + nlink(202,curiyr)+nlink(185,curiyr)+nlink(331,curiyr)+nlink(333,curiyr)
       mc_wpi057(curiyr)       = ((nlink(210,curiyr)*(nlink(204,curiyr)/seccon))  &
                               +  (nlink(240,curiyr)*(nlink(323,curiyr)/seccon))  &
                               +  (nlink(242,curiyr)*(nlink(325,curiyr)/seccon))  &
                               +  (nlink(244,curiyr)*(nlink(327,curiyr)/seccon))  &
                               +  (nlink(246,curiyr)*(nlink(329,curiyr)/seccon))  &
                               +  (nlink(218,curiyr)*(nlink(202,curiyr)/seccon))  &
                               +  (nlink(248,curiyr)*(nlink(185,curiyr)/seccon))  &
                               +  (nlink(216,curiyr)*(nlink(331,curiyr)/seccon))  &
                               +  (nlink(250,curiyr)*(nlink(333,curiyr)/seccon))) &
                               * nlink(282,curiyr)
       mamenergy(43,curiyr)    = mc_wpi057(curiyr)

!  ((comprice_residualfuel *(comcon_residualfuel /(comcon_residualfuel+indcon_residualfuel+trancon_residualfuel)))  &
!  +(indprice_residualfuel *(indcon_residualfuel /(comcon_residualfuel+indcon_residualfuel+trancon_residualfuel)))  &
!  +(tranprice_residualfuel*(trancon_residualfuel/(comcon_residualfuel+indcon_residualfuel+trancon_residualfuel)))) &
!  * jpgdp

       seccon = nlink(325,curiyr)+nlink(329,curiyr)+nlink(333,curiyr)
       mc_wpi0574(curiyr)      = ((nlink(242,curiyr)*(nlink(325,curiyr)/seccon))  &
                               +  (nlink(246,curiyr)*(nlink(329,curiyr)/seccon))  &
                               +  (nlink(250,curiyr)*(nlink(333,curiyr)/seccon))) &
                               * nlink(282,curiyr)
       mamenergy(44,curiyr)    = mc_wpi0574(curiyr)

!  worldoilprice * jpgdp
       mc_poilimp(curiyr)      = nlink(238,curiyr)*nlink(282,curiyr)
       mamenergy(45,curiyr)    = mc_poilimp(curiyr)

!  westtexasoilprice * jpgdp
       mc_poilwti(curiyr)      = nlink(345,curiyr)*nlink(282,curiyr)
       mamenergy(46,curiyr)    = mc_poilwti(curiyr)


!  Energy investment variables.
       mc_consr(curiyr)       = esmac(3,curiyr)
       maminvest(1,curiyr)    = esmac(3,curiyr)
       mc_cdr(curiyr)         = esmac(8,curiyr)
       maminvest(2,curiyr)    = esmac(8,curiyr)
       mc_cdmvr(curiyr)       = esmac(62,curiyr)
       maminvest(3,curiyr)    = esmac(62,curiyr)
       mc_cdfher(curiyr)      = esmac(63,curiyr)
       maminvest(4,curiyr)    = esmac(63,curiyr)
       mc_cdrecipr(curiyr)    = esmac(64,curiyr)
       maminvest(5,curiyr)    = esmac(64,curiyr)
       mc_cdrecor(curiyr)     = esmac(65,curiyr)
       maminvest(6,curiyr)    = esmac(65,curiyr)
       mc_cdoor(curiyr)       = esmac(66,curiyr)
       maminvest(7,curiyr)    = esmac(66,curiyr)
       mc_cdotaer(curiyr)     = esmac(67,curiyr)
       maminvest(8,curiyr)    = esmac(67,curiyr)
       mc_ir(curiyr)          = esmac(4,curiyr)
       maminvest(9,curiyr)    = esmac(4,curiyr)
       mc_ifxr(curiyr)        = esmac(15,curiyr)
       maminvest(10,curiyr)   = esmac(15,curiyr)
       mc_ifnrer(curiyr)      = esmac(16,curiyr)
       maminvest(11,curiyr)   = esmac(16,curiyr)
       mc_ifnreer(curiyr)     = esmac(13,curiyr)
       maminvest(12,curiyr)   = esmac(13,curiyr)
       mc_ifnreetlvr(curiyr)  = esmac(68,curiyr)
       maminvest(13,curiyr)   = esmac(68,curiyr)
       mc_ifnreeipctr(curiyr) = esmac(69,curiyr)
       maminvest(14,curiyr)   = esmac(69,curiyr)
       mc_ifnreeipccr(curiyr) = esmac(70,curiyr)
       maminvest(15,curiyr)   = esmac(70,curiyr)
       mc_ifnreeipcsr(curiyr) = esmac(71,curiyr)
       maminvest(16,curiyr)   = esmac(71,curiyr)
       mc_ifnreemiscr(curiyr) = esmac(72,curiyr)
       maminvest(17,curiyr)   = esmac(72,curiyr)
       mc_ifnresr(curiyr)     = esmac(11,curiyr)
       maminvest(18,curiyr)   = esmac(11,curiyr)
       mc_ifnresbaor(curiyr)  = esmac(73,curiyr)
       maminvest(19,curiyr)   = esmac(73,curiyr)
       mc_ifnresmir(curiyr)   = esmac(74,curiyr)
       maminvest(20,curiyr)   = esmac(74,curiyr)
       mc_ifnrespur(curiyr)   = esmac(75,curiyr)
       maminvest(21,curiyr)   = esmac(75,curiyr)
       mc_ifrer(curiyr)       = esmac(17,curiyr)
       maminvest(22,curiyr)   = esmac(17,curiyr)
       mc_ifresr(curiyr)      = esmac(12,curiyr)
       maminvest(23,curiyr)   = esmac(12,curiyr)
       mc_ifreer(curiyr)      = esmac(14,curiyr)
       maminvest(24,curiyr)   = esmac(14,curiyr)

!  National macroeconomic variables
       mc_gdpr(curiyr)       = esmac(1,curiyr)
       mc_gdpfer(curiyr)     = esmac(2,curiyr)
       mc_consr(curiyr)      = esmac(3,curiyr)
       mc_ir(curiyr)         = esmac(4,curiyr)
       mc_xr(curiyr)         = esmac(5,curiyr)
       mc_mr(curiyr)         = esmac(6,curiyr)
       mc_gr(curiyr)         = esmac(7,curiyr)
       mc_cdr(curiyr)        = esmac(8,curiyr)
       mc_cnr(curiyr)        = esmac(9,curiyr)
       mc_csvr(curiyr)       = esmac(10,curiyr)
       mc_ifnresr(curiyr)    = esmac(11,curiyr)
       mc_ifresr(curiyr)     = esmac(12,curiyr)
       mc_ifnreer(curiyr)    = esmac(13,curiyr)
       mc_ifreer(curiyr)     = esmac(14,curiyr)
       mc_ifxr(curiyr)       = esmac(15,curiyr)
       mc_ifnrer(curiyr)     = esmac(16,curiyr)
       mc_ifrer(curiyr)      = esmac(17,curiyr)
       mc_xgffbr(curiyr)     = esmac(18,curiyr)
       mc_xginr(curiyr)      = esmac(19,curiyr)
       mc_xgkr(curiyr)       = esmac(20,curiyr)
       mc_xgautor(curiyr)    = esmac(21,curiyr)
       mc_xgcr(curiyr)       = esmac(22,curiyr)
       mc_xgr(curiyr)        = esmac(23,curiyr)
       mc_xsvtotr(curiyr)    = esmac(24,curiyr)
       mc_mgffbr(curiyr)     = esmac(25,curiyr)
       mc_mginapetr(curiyr)  = esmac(26,curiyr)
       mc_mgkr(curiyr)       = esmac(27,curiyr)
       mc_mgautor(curiyr)    = esmac(28,curiyr)
       mc_mgcr(curiyr)       = esmac(29,curiyr)
       mc_msvtotr(curiyr)    = esmac(30,curiyr)
       mc_iir(curiyr)        = esmac(31,curiyr)
       mc_gfmlr(curiyr)      = esmac(32,curiyr)
       mc_gdp(curiyr)        = esmac(33,curiyr)
       mc_cons(curiyr)       = esmac(34,curiyr)
       mc_i(curiyr)          = esmac(35,curiyr)
       mc_gnpr(curiyr)       = esmac(36,curiyr)
!  Convert deflator to 2012=1.0 from 2012=100.
       esmac(37,curiyr)      = esmac(37,curiyr)/100.
!  Convert base year of deflator to 1987 for NEMS from 2012 base year in GI model.
       mc_jpgdp(curiyr)      = esmac(37,curiyr)/0.5704075
       mc_rmtb3m(curiyr)     = esmac(38,curiyr)
       mc_rmmtg30con(curiyr) = esmac(39,curiyr)
       mc_rmcorppuaa(curiyr) = esmac(40,curiyr)
       mc_rmgblusreal(curiyr)= esmac(41,curiyr)
       mc_jeciwsp(curiyr)    = esmac(42,curiyr)
       mc_suva(curiyr)       = esmac(43,curiyr)
       mc_suvlv(curiyr)      = esmac(44,curiyr)
       mc_suvtl(curiyr)      = esmac(45,curiyr)
       mc_suvtham(curiyr)    = esmac(46,curiyr)
       mc_ruc(curiyr)        = esmac(47,curiyr)
       mc_wpi(curiyr)        = esmac(48,curiyr)
       mc_wpi11(curiyr)      = esmac(49,curiyr)
       mc_wpi14(curiyr)      = esmac(50,curiyr)
       mc_nlfc(curiyr)       = esmac(51,curiyr)
       mc_rmff(curiyr)       = esmac(52,curiyr)
       mc_wpi05(curiyr)      = esmac(53,curiyr)
       mc_rmtcm10y(curiyr)   = esmac(54,curiyr)
       mc_rmcorpbaa(curiyr)  = esmac(55,curiyr)
       mc_cpie(curiyr)       = esmac(56,curiyr)
       mc_np65a(curiyr)      = esmac(57,curiyr)
       mc_jqpcmhnf(curiyr)   = esmac(58,curiyr)
       mc_wpisop3200(curiyr) = esmac(59,curiyr)
       mc_wpi10(curiyr)      = esmac(60,curiyr)
       mc_gslgisnhwyr(curiyr)= esmac(61,curiyr)

!  START IF (curiyr .GT. 3): Compute real utility bond interest rate starting
!   in 1992 using a three year moving average of gdp deflator.
       IF (curiyr .GT. 3) THEN
         mc_rlrmcorppuaa(curiyr) = esmac(40,curiyr)                           &
           -(((((esmac(37,curiyr)+esmac(37,curiyr-1)+esmac(37,curiyr-2))/3.0) &
           /((esmac(37,curiyr-1)+esmac(37,curiyr-2)+esmac(37,curiyr-3))/3.0)) &
           -1.0)*100.0)
!  END IF (curiyr .GT. 3): Compute real utility bond interest rate starting
!   in 1982 using a three year moving average of gdp deflator.
       END IF

!  Real value of RMPUAANS for 1990 through 1992 was computed using the
!   nominal value of RMPUAANS and a three year rolling average of JPGDP.
!   Data is from Global Insight US model. The resulting real interest rates
!   follow:

       mc_rlrmcorppuaa(1) = 6.376943022781729 ! 1990 real rmpuaans value
       mc_rlrmcorppuaa(2) = 5.391977755891341 ! 1991 real rmpuaans value
       mc_rlrmcorppuaa(3) = 4.908316368542533 ! 1992 real rmpuaans value

!  Data for extra macro tables
       mc_bopcrntac(curiyr)     = mcxtabs(1,curiyr)
       mc_cd(curiyr)            = mcxtabs(2,curiyr)
       mc_cdfhe(curiyr)         = mcxtabs(3,curiyr)
       mc_cdmv(curiyr)          = mcxtabs(4,curiyr)
       mc_cdo(curiyr)           = mcxtabs(5,curiyr)
       mc_cdor(curiyr)          = mcxtabs(6,curiyr)
       mc_cdrec(curiyr)         = mcxtabs(7,curiyr)
       mc_cdrecr(curiyr)        = mcxtabs(8,curiyr)
       mc_cgoods(curiyr)        = mcxtabs(9,curiyr)
       mc_cgoodsr(curiyr)       = mcxtabs(10,curiyr)
       mc_ckf(curiyr)           = mcxtabs(11,curiyr)
       mc_ckfadjcorp(curiyr)    = mcxtabs(12,curiyr)
       mc_ckfcorp(curiyr)       = mcxtabs(13,curiyr)
       mc_ckfcorpbk(curiyr)     = mcxtabs(14,curiyr)
       mc_cn(curiyr)            = mcxtabs(15,curiyr)
       mc_cncs(curiyr)          = mcxtabs(16,curiyr)
       mc_cncsr(curiyr)         = mcxtabs(17,curiyr)
       mc_cnefao(curiyr)        = mcxtabs(18,curiyr)
       mc_cnegao(curiyr)        = mcxtabs(19,curiyr)
       mc_cnf(curiyr)           = mcxtabs(20,curiyr)
       mc_cnfr(curiyr)          = mcxtabs(21,curiyr)
       mc_cnoo(curiyr)          = mcxtabs(22,curiyr)
       mc_cnoor(curiyr)         = mcxtabs(23,curiyr)
       mc_cnopmp(curiyr)        = mcxtabs(24,curiyr)
       mc_cnopmpr(curiyr)       = mcxtabs(25,curiyr)
       mc_cnotob(curiyr)        = mcxtabs(26,curiyr)
       mc_cnotobr(curiyr)       = mcxtabs(27,curiyr)
       mc_cpixfae(curiyr)       = mcxtabs(28,curiyr)
       mc_csv(curiyr)           = mcxtabs(29,curiyr)
       mc_csvac(curiyr)         = mcxtabs(30,curiyr)
       mc_csvacr(curiyr)        = mcxtabs(31,curiyr)
       mc_csvf(curiyr)          = mcxtabs(32,curiyr)
       mc_csvfin(curiyr)        = mcxtabs(33,curiyr)
       mc_csvfinr(curiyr)       = mcxtabs(34,curiyr)
       mc_csvfr(curiyr)         = mcxtabs(35,curiyr)
       mc_csvh(curiyr)          = mcxtabs(36,curiyr)
       mc_csvhc(curiyr)         = mcxtabs(37,curiyr)
       mc_csvhcr(curiyr)        = mcxtabs(38,curiyr)
       mc_csvhh(curiyr)         = mcxtabs(39,curiyr)
       mc_csvhhr(curiyr)        = mcxtabs(40,curiyr)
       mc_csvhr(curiyr)         = mcxtabs(41,curiyr)
       mc_csvins(curiyr)        = mcxtabs(42,curiyr)
       mc_csvinsr(curiyr)       = mcxtabs(43,curiyr)
       mc_csvnpish(curiyr)      = mcxtabs(44,curiyr)
       mc_csvnpishr(curiyr)     = mcxtabs(45,curiyr)
       mc_csvo(curiyr)          = mcxtabs(46,curiyr)
       mc_csvor(curiyr)         = mcxtabs(47,curiyr)
       mc_csvrec(curiyr)        = mcxtabs(48,curiyr)
       mc_csvrecr(curiyr)       = mcxtabs(49,curiyr)
       mc_csvts(curiyr)         = mcxtabs(50,curiyr)
       mc_csvtsr(curiyr)        = mcxtabs(51,curiyr)
       mc_csvu(curiyr)          = mcxtabs(52,curiyr)
       mc_csvur(curiyr)         = mcxtabs(53,curiyr)
       mc_eea(curiyr)           = mcxtabs(54,curiyr)
       mc_g(curiyr)             = mcxtabs(55,curiyr)
       mc_gf(curiyr)            = mcxtabs(56,curiyr)
       mc_gfml(curiyr)          = mcxtabs(57,curiyr)
       mc_gfo(curiyr)           = mcxtabs(58,curiyr)
       mc_gfor(curiyr)          = mcxtabs(59,curiyr)
       mc_gfr(curiyr)           = mcxtabs(60,curiyr)
       mc_gnp(curiyr)           = mcxtabs(61,curiyr)
       mc_gsl(curiyr)           = mcxtabs(62,curiyr)
       mc_gslr(curiyr)          = mcxtabs(63,curiyr)
       mc_huesold(curiyr)       = mcxtabs(64,curiyr)
       mc_husps(curiyr)         = mcxtabs(65,curiyr)
       mc_i(curiyr)             = mcxtabs(66,curiyr)
       mc_ifnre(curiyr)         = mcxtabs(67,curiyr)
       mc_ifnree(curiyr)        = mcxtabs(68,curiyr)
       mc_ifnreeind(curiyr)     = mcxtabs(69,curiyr)
       mc_ifnreeindr(curiyr)    = mcxtabs(70,curiyr)
       mc_ifnreeip(curiyr)      = mcxtabs(71,curiyr)
       mc_ifnreeipr(curiyr)     = mcxtabs(72,curiyr)
       mc_ifnreeo(curiyr)       = mcxtabs(73,curiyr)
       mc_ifnreeor(curiyr)      = mcxtabs(74,curiyr)
       mc_ifnreetac(curiyr)     = mcxtabs(75,curiyr)
       mc_ifnreetacr(curiyr)    = mcxtabs(76,curiyr)
       mc_ifnreetlv(curiyr)     = mcxtabs(77,curiyr)
       mc_ifnreeto(curiyr)      = mcxtabs(78,curiyr)
       mc_ifnreetor(curiyr)     = mcxtabs(79,curiyr)
       mc_ifnreetr(curiyr)      = mcxtabs(80,curiyr)
       mc_ifnres(curiyr)        = mcxtabs(81,curiyr)
       mc_ifnresc(curiyr)       = mcxtabs(82,curiyr)
       mc_ifnrescr(curiyr)      = mcxtabs(83,curiyr)
       mc_ifnresmfg(curiyr)     = mcxtabs(84,curiyr)
       mc_ifnresmfgr(curiyr)    = mcxtabs(85,curiyr)
       mc_ifnresmi(curiyr)      = mcxtabs(86,curiyr)
       mc_ifnreso(curiyr)       = mcxtabs(87,curiyr)
       mc_ifnresor(curiyr)      = mcxtabs(88,curiyr)
       mc_ifnresp(curiyr)       = mcxtabs(89,curiyr)
       mc_ifnrespc(curiyr)      = mcxtabs(90,curiyr)
       mc_ifnrespcr(curiyr)     = mcxtabs(91,curiyr)
       mc_ifnrespp(curiyr)      = mcxtabs(92,curiyr)
       mc_ifnresppr(curiyr)     = mcxtabs(93,curiyr)
       mc_ifnrespr(curiyr)      = mcxtabs(94,curiyr)
       mc_ifre(curiyr)          = mcxtabs(95,curiyr)
       mc_ifree(curiyr)         = mcxtabs(96,curiyr)
       mc_ifres(curiyr)         = mcxtabs(97,curiyr)
       mc_ii(curiyr)            = mcxtabs(98,curiyr)
       mc_iicmiu(curiyr)        = mcxtabs(99,curiyr)
       mc_iicmiur(curiyr)       = mcxtabs(100,curiyr)
       mc_iif(curiyr)           = mcxtabs(101,curiyr)
       mc_iifr(curiyr)          = mcxtabs(102,curiyr)
       mc_iim(curiyr)           = mcxtabs(103,curiyr)
       mc_iimr(curiyr)          = mcxtabs(104,curiyr)
       mc_iinf(curiyr)          = mcxtabs(105,curiyr)
       mc_iinfr(curiyr)         = mcxtabs(106,curiyr)
       mc_iirt(curiyr)          = mcxtabs(107,curiyr)
       mc_iirtr(curiyr)         = mcxtabs(108,curiyr)
       mc_iiw(curiyr)           = mcxtabs(109,curiyr)
       mc_iiwr(curiyr)          = mcxtabs(110,curiyr)
       mc_intnetbus(curiyr)     = mcxtabs(111,curiyr)
       mc_ipsb50001(curiyr)     = mcxtabs(112,curiyr)
       mc_ivacorp(curiyr)       = mcxtabs(113,curiyr)
       mc_jcsmich(curiyr)       = mcxtabs(114,curiyr)
       mc_jeciwssp(curiyr)      = mcxtabs(115,curiyr)
       mc_jexchmtp(curiyr)      = mcxtabs(116,curiyr)
       mc_m(curiyr)             = mcxtabs(117,curiyr)
       mc_mfy(curiyr)           = mcxtabs(118,curiyr)
       mc_mg(curiyr)            = mcxtabs(119,curiyr)
       mc_mgr(curiyr)           = mcxtabs(120,curiyr)
       mc_msvtot(curiyr)        = mcxtabs(121,curiyr)
       mc_netcaptrf(curiyr)     = mcxtabs(122,curiyr)
       mc_netcfiva(curiyr)      = mcxtabs(123,curiyr)
       mc_netsavgfunify(curiyr) = mcxtabs(124,curiyr)
       mc_nnp(curiyr)           = mcxtabs(125,curiyr)
       mc_savprate(curiyr)      = mcxtabs(126,curiyr)
       mc_sfdprodr(curiyr)      = mcxtabs(127,curiyr)
       mc_sp500(curiyr)         = mcxtabs(128,curiyr)
       mc_stat(curiyr)          = mcxtabs(129,curiyr)
       mc_sublsurpgf(curiyr)    = mcxtabs(130,curiyr)
       mc_sublsurpgsl(curiyr)   = mcxtabs(131,curiyr)
       mc_trfbus(curiyr)        = mcxtabs(132,curiyr)
       mc_txcorp(curiyr)        = mcxtabs(133,curiyr)
       mc_txcorpgf(curiyr)      = mcxtabs(134,curiyr)
       mc_txcorpgsl(curiyr)     = mcxtabs(135,curiyr)
       mc_txcorprw(curiyr)      = mcxtabs(136,curiyr)
       mc_txim(curiyr)          = mcxtabs(137,curiyr)
       mc_utlb00004(curiyr)     = mcxtabs(138,curiyr)
       mc_wpisop3000(curiyr)    = mcxtabs(139,curiyr)
       mc_x(curiyr)             = mcxtabs(140,curiyr)
       mc_xfy(curiyr)           = mcxtabs(141,curiyr)
       mc_xg(curiyr)            = mcxtabs(142,curiyr)
       mc_xsvtot(curiyr)        = mcxtabs(143,curiyr)
       mc_yn(curiyr)            = mcxtabs(144,curiyr)
       mc_ypcomp(curiyr)        = mcxtabs(145,curiyr)
       mc_ypd(curiyr)           = mcxtabs(146,curiyr)
       mc_yppropadjf(curiyr)    = mcxtabs(147,curiyr)
       mc_yppropadjnf(curiyr)   = mcxtabs(148,curiyr)
       mc_yprentadj(curiyr)     = mcxtabs(149,curiyr)
       mc_za(curiyr)            = mcxtabs(150,curiyr)
       mc_zadiv(curiyr)         = mcxtabs(151,curiyr)
       mc_zar(curiyr)           = mcxtabs(152,curiyr)
       mc_zare(curiyr)          = mcxtabs(153,curiyr)
       mc_zb(curiyr)            = mcxtabs(154,curiyr)
       mc_zbecon(curiyr)        = mcxtabs(155,curiyr)
       mc_zbivadfin521(curiyr)  = mcxtabs(156,curiyr)
       mc_zbivarw(curiyr)       = mcxtabs(157,curiyr)
       mc_wpiind05(curiyr)      = mcxtabs(158,curiyr)


!  START DO i: Write to common block macro variables that are regionalized.
       DO i = 1,mcnumregs
         mc_cpi(i,curiyr)       = esmacreg(i,1,curiyr)
         mc_ypdr(i,curiyr)      = esmacreg(i,2,curiyr)
         mc_ypcompwsd(i,curiyr) = esmacreg(i,3,curiyr)
         mc_yp(i,curiyr)        = esmacreg(i,4,curiyr)
         mc_husmfg(i,curiyr)    = esmacreg(i,5,curiyr)
         mc_husps1(i,curiyr)    = esmacreg(i,6,curiyr)
         mc_husps2a(i,curiyr)   = esmacreg(i,7,curiyr)
         mc_khumfg(i,curiyr)    = esmacreg(i,8,curiyr)
         mc_khups1(i,curiyr)    = esmacreg(i,9,curiyr)
         mc_khups2a(i,curiyr)   = esmacreg(i,10,curiyr)
         mc_np(i,curiyr)        = esmacreg(i,11,curiyr)
         mc_np16a(i,curiyr)     = esmacreg(i,12,curiyr)
         mc_mfgwgrt(i,curiyr)   = esmacreg(i,13,curiyr)
         mc_nmfgwgrt(i,curiyr)  = esmacreg(i,14,curiyr)

!  START DO j: Write to common block first 48 industrial variables that are
!    also regionalized. This spans manufacturing and non-manufacturing industries:
!    Food Products (NAICS 311) to Construction (NAICS 23) and includes
!    aggregations for chemicals and allied products; petroleum and coal products;
!    stone, clay, and glass products; and primary metals industries.
         DO j = 1,mcnmind+4
           mc_revind(i,j,curiyr) = esind(i,j,curiyr)
!  END DO j: Write to common block first 48 industrial variables that are
!    also regionalized. This spans manufacturing and non-manufacturing industries:
!    Food Products (NAICS 311) to Construction (NAICS 23) and includes
!    aggregations for chemicals and allied products; petroleum and coal products;
!    stone, clay, and glass products; and primary metals industries.
         END DO

!  START DO j: Write to common block 10 non-industrial/service industries and total. Service
!    industries are not regionalized.
         DO j = 1,mcnmserv+1
           mc_revser(11,j,curiyr) = esserv(j,curiyr)
!  END DO j: Write to common block 10 service industries and total. Service
!    industries are not regionalized.
         END DO

!  START DO j: Write to common block commercial floor space forecast by type
!    within region.
         DO j = 1,mcnmfltype
           mc_commflsp(i,j,curiyr) = esmacreg(i,14+j,curiyr)
!  END DO j: Write to common block commercial floor space forecast by type
!    within region.
         END DO

!  START DO j: Initialize common block employment array.
         DO j = 1,numempl
           mc_empna(i,j,curiyr) = 0.0
!  END DO j: Initialize common block employment array.
         END DO

!  END DO i: Write to common block macro variables that are regionalized.
       END DO

!  Regionalize the employment model output. There is no regional
!   share for agriculture, EAG, but is printed anyway as zeros. Region
!   11, representing the US, does have values for EAG.
!  START DO r: Loop through the nine Census Division regions.
       DO r = 1,mcnumregs-2

!  START DO j: Loop through employment categories; number set by mcparms.
         DO j = 1,numempl
           mc_empna(r,j,curiyr)  = mc_regemp((numempl*(r-1)+j),curiyr)
           mc_empna(11,j,curiyr) = mc_empna(11,j,curiyr)+mc_empna(r,j,curiyr)
!  END DO j: Loop through employment categories.
         END DO

!  END DO r: Loop through the nine Census Division regions.
       END DO

!  Coal Mining, E11,12 NAICS 2121 (Millions of Persons)
       mc_empna(11,22,curiyr) = esemp(11,22,curiyr)
!  Oil  Gas Mining, E13 NAICS 211, 213 (Millions of Persons)
       mc_empna(11,23,curiyr) = esemp(11,23,curiyr)
! Petroleum Refining, E291 NAICS 324 (Millions of Persons)
       mc_empna(11,10,curiyr) = esemp(11,10,curiyr)
! Blast Furnace  Basic Steel, E331 (Millions of Persons)
!       mc_empna(11,25,curiyr) = esemp(11,25,curiyr)
! Primary Aluminum, E3334 (Millions of Persons)
!       mc_empna(11,26,curiyr) = esemp(11,26,curiyr)


!  START IF (fcrl .EQ. 1 .AND. lastyr .EQ. curiyr .AND. giswitch .LT. 0): Call on macoutput subroutine.
       IF (fcrl .EQ. 1 .AND. lastyr .EQ. curiyr .AND. giswitch .LT. 0) THEN
         WRITE(*,*) "MAC.F: Calling macro output writes"
         CALL macoutput

!  END IF (fcrl .EQ. 1 .AND. lastyr .EQ. curiyr .AND. giswitch .LT. 0): Call on macoutput subroutine.
       END IF

!  End of subroutine MAC. Return to calling program.

100    RETURN
       END


!******************************************************************
       SUBROUTINE READMAC
!******************************************************************
!  The READMAC subroutine reads a single file, mcparms, which
!  contains MAM parameters. This file is read just once per sim.
!  It is called on the first iteration of the first year.
!
!******************************************************************

       IMPLICIT NONE

!  Include parameter files.
       INCLUDE'parametr.'
       INCLUDE'macparm.'
       INCLUDE'mcinter2.'
       INCLUDE'ncntrl.'

!  Local variable definitions.
       INTEGER      file_mgr,getindex,rtovalue
       EXTERNAL     file_mgr,getindex,rtovalue
       CHARACTER*18 name     ! File name for FILE_MGR
       LOGICAL      new      ! Logical variable for FILE_MGR
       INTEGER      inunit   ! Unit number for reads as returned from FILE_MGR
       INTEGER      i,j,k    ! Indexes for arrays


!  Open macroeconomic parameters file mcparms for read.
!   File mcparms is always read first.
       name='MCPARMS'
       new=.FALSE.
       INUNIT=FILE_MGR('O',name,new)

!  Read number of industrial output variables.
       READ(INUNIT,2001) mcnmind

!  Read number of non-industrial/service output variables.
       READ(INUNIT,2001) mcnmserv

!  Read number of non-regional macro variables.
       READ(INUNIT,2001) mcnmmac

!  Read number of regional macro variables from the national model.
       READ(INUNIT,2001) mcnmnatreg

!  Read number of regional macro variables.
       READ(INUNIT,2001) mcnmmacreg

!  Read number of manufacturing industrial outputs.
       READ(INUNIT,2001) mcnummnf

!  Read number of Census districts plus California
!   and National.
       READ(INUNIT,2001) mcnumregs

!  Read number of commercial floorspace types including total.
       READ(INUNIT,2001) mcnmfltype

!  Read number of industrial employment categories.
       READ(INUNIT,2001) numempl

!  Read number of exogenous variables (aggregates and components) read from NEMS.
       READ(INUNIT,2002) nemsenergynum

!  Read number of driver variables passed to EViews from MAM.
       READ(INUNIT,2002) scennum

!  Read number of solution variables returned to MAM from EViews.
       READ(INUNIT,2002) numepmac

!  Read number of solution variables returned for extra macro tables.
       READ(INUNIT,2002) numxtabs

!  Read number of solution variables returned for extra macro tables.
       READ(INUNIT,2001) giswitch

!  Read number of solution variables returned for extra macro tables.
       READ(INUNIT,2002) numgixtab

!  Read value of Commercial Floor Space add factor switch; 1=ON 0=OFF
       READ(INUNIT,2004) controltarget

!  Read value of Commercial Floor Space growth rate table switch; 1=ON 0=OFF
       READ(INUNIT,2004) cfdiagx

!  Read value of lever for turning on Commercial Floor Space model; 1=ON 0=OFF
       READ(INUNIT,2004) cflever

!  Read value of lever for running Linkage Model; 1=ON 0=OFF
       READ(INUNIT,2004) linkrun

!  Read value of max number of cycles to allow for Linkage Model
       READ(INUNIT,2001) linkcyclemax

!  Read value of lever for recording Linkage cycle results; 1=ON 0=OFF
       READ(INUNIT,2004) linkrec

!  Read formats for mcparms file.
2001   FORMAT(I2)
2002   FORMAT(I3)
2003   FORMAT(F4.2)
2004   FORMAT(I1)

!  Close mcparms file for read.
       INUNIT=FILE_MGR('C',name,new)


!  End of subroutine READMAC. Return to calling subroutine MAC.
       RETURN
       END


!***********************************************************************
       SUBROUTINE GISUB
       USE DFLIB
!***********************************************************************
!  This subroutine reads baseline, pessimistic, optimistic or cyclical
!  forecast from a Global Insight forecast. The switch setting is
!  in the mcparms.txt file, giswitch:
!  -1 = off
!   0 = baseline
!   1 = pessimistic
!   2 = optimistic
!   3 = cyclical
!
!September 2010
!***********************************************************************

       IMPLICIT NONE

!  Include parameter files.
       INTEGER      FILE_MGR
       EXTERNAL     FILE_MGR
       CHARACTER*30 name     ! File name for FILE_MGR
       LOGICAL      new      ! Logical variable for FILE_MGR
       INTEGER      inunit   ! Unit number for reads as returned from FILE_MGR
       LOGICAL      problem

!  Include parameter files.
       INCLUDE'parametr'
       INCLUDE'apq'
       INCLUDE'macout'
       INCLUDE'pmmout'
       INCLUDE'lfmmout'
       INCLUDE'pmmrpt'
       INCLUDE'pmmftab'
       INCLUDE'ogsmout'
       INCLUDE'convfact'
       INCLUDE'ngtdmrep'
       INCLUDE'angtdm'
       INCLUDE'ncntrl'
       INCLUDE'emmparm'
       INCLUDE'emission'
       INCLUDE'ghgrep'
       INCLUDE'cdsparms'
       INCLUDE'coalout'
       INCLUDE'coalrep'
       INCLUDE'intout'
       INCLUDE'wrenew'
       INCLUDE'tranrep'
       INCLUDE'uefdout'
       INCLUDE 'macparm.'
       INCLUDE 'mcinter2.'
       INCLUDE 'mcdetail'
       INCLUDE 'continew'

!  Declare functions for NEMS parameters.
       EXTERNAL getindex,rtovalue
       INTEGER  getindex,rtovalue

!  Local variable definitions.
       REAL           rdays,scaleprice
       INTEGER        i,j,charc,is
       INTEGER*4      iret,eviewsversion
       CHARACTER      eviewscommand*80,cmdline*500,myline*128
       CHARACTER*8    computername,eviewsenv*8/' '/
       CHARACTER*255  pwd,filen,fimcevwork,fimcevsubs
       CHARACTER*3000 line,label

! Declares for OSCall  to invoke EViews.
       CHARACTER*80 args
       INTEGER*4    iWaitMS/-1/ ! Wait for OSCall in milliseconds. if -1, infinite wait
       LOGICAL      lstatus

       iWaitMS = 1000 * 60 * 15  !  milliseconds converting seconds to # of minutes (last multiplicand)

!  Query last year
       mamlastyr = RTOVALUE('AEOLSTYR',0)


!  Query present working directory and drive.
       pwd  = FILE$CURDRIVE
       iret = GETDRIVEDIRQQ(pwd)
!  START IF (iret .GT. 0): Write present working directory and drive to local
!    variable if one exists.
       IF (iret .GT. 0) THEN
         filen = TRIM(pwd)
!  START ELSE: Write an error if present working directory and drive do not exist.
       ELSE
         WRITE(*,*) "MAC.F: WARNING! NO DRIVE DETECTED."
!  END IF (iret .GT. 0): Write present working directory and drive to local
!    variable if one exists.
       END IF

!  Call environmental parameter locating EViews executable.
       eviewsversion=rtovalue("EVVERS  ",5)
           IF (eviewsversion .LT. 10) THEN
          WRITE(eviewsenv,'("EVIEWS",I1)') eviewsversion
           ELSE
          WRITE(eviewsenv,'("EVIEWS",I2)') eviewsversion
           ENDIF
       CALL GETENVmac(eviewsenv,eviewscommand,computername)

       IF (eviewscommand .NE. " ") THEN
         WRITE(6,'("  Using EViews version ",I2,"  command ",A)') eviewsversion,TRIM(eviewscommand)
       ELSE
         WRITE(6,'("  EViews version (",I2,") specified is different than those available on this computer")') eviewsversion
         WRITE(6,'("  EViews will be launched using ",A," .prg association")') computername
       END IF

!  MCEVWORK.WF1 file for GI US model.
       fimcevwork='MCEVWORK'
       inunit=FILE_MGR('O',fimcevwork,new)
       INQUIRE(UNIT=inunit,name=fimcevwork)
       CLOSE(inunit,STATUS='keep')

!  MCEVSUBS.PRG file model related subroutines in EViews.
       fimcevsubs='MCEVSUBS'
       inunit=FILE_MGR('O',fimcevsubs,new)
       INQUIRE(UNIT=inunit,name=fimcevsubs)
       CLOSE(inunit,STATUS='keep')

!  Open and write EView commands to program file drivers.prg.
       OPEN(3,ACTION='WRITE',FILE='DRIVERS.PRG')

! change to current working directory
       myline = 'cd "'//TRIM(filen)//'"'
       charc  = LEN(TRIM(myline))
       WRITE(3,'(A<charc>)') myline

! include the subroutines from mcevsubs.prg
       myline = 'include '//TRIM(fimcevsubs)
       charc  = LEN(TRIM(myline))
       WRITE(3,'(A<charc>)') myline

! load the mcevwork.wf1 
       myline = 'load '//TRIM(fimcevwork)
       charc  = LEN(TRIM(myline))
       WRITE(3,'(A<charc>)') myline

! create scalar for last NEMS year
       myline = 'scalar mamlastyr = '
       charc  = LEN(TRIM(myline))
       WRITE(3,'(A<charc>,1X,I4)') myline,mamlastyr

! create scalar for gi scenario
       myline = 'scalar giswitch  = '
       charc  = LEN(TRIM(myline))
       WRITE(3,'(A<charc>,1X,I4)') myline,giswitch

! call the gixtabs subroutine
       myline = 'call gixtabs'
       charc  = LEN(TRIM(myline))
       WRITE(3,'(A<charc>)') myline

! write scenario data to mc_gixtab.csv
       myline = 'write(t=txt,d=c,t,id) '//TRIM(filen)//'\mc_gixtab.csv xtab_group'
       charc  = LEN(TRIM(myline))
       WRITE(3,'(A<charc>)') myline

! save mcevwork.wf1
       myline = 'save '//TRIM(filen)//'\mcevwork.wf1'
       charc  = LEN(TRIM(myline))
       WRITE(3,'(A<charc>)') myline

! exit EViews
       myline = 'exit'
       charc  = LEN(TRIM(myline))
       WRITE(3,'(A<charc>)') myline

! close writes to drivers.prg
       CLOSE(3)

!  Execute EViews with constructed program file of commands.
       IF (LEN_TRIM(eviewscommand).GT.0) THEN
         cmdline=TRIM(eviewscommand)
         args=TRIM(filen)//'\drivers.prg'
       ELSE
         cmdline='cmd'
         args='/c '//trim(filen)//'\drivers.prg'
       END IF

       lstatus=.false.

!  OSCall is in main.f.  It creates a child process with a minimized window. 
!  "cmdline" is the program name; "args" contains the program arguments--in this case, the "prg" file.
!  "iwaitMS" is set to -1 to indicate indefinite wait.  
       CALL OSCall(iWaitMS,cmdline,args,iRet)
       IF (iret.EQ.1) lstatus=.true.

!  START IF (.not. lstatus): Trap if call on EViews command fails to execute.
        MC_EV_SUCCESS = 0      ! old way;  0 is pass
        IF (.NOT. lstatus) THEN 
          MC_EV_SUCCESS = 1
          IF (CONTINM .EQ. 1) THEN
             CONTINM=0            !  set to FAIL
             REASONM="EViews time-out"
          ENDIF
          IF (iret.lt.0) THEN
            WRITE (*,*) "MAC.F: WARNING! EVIEWS COMMAND FAILED TO EXECUTE."
          ELSE
            WRITE (*,*) "MAC.F: WARNING! WAIT FOR EVIEWS FAILED TO OCCUR."
          END IF

!  END IF (.not. lstatus): Trap if call on EViews command fails to execute.
        END IF

!  Open MC_GIXTAB.CSV for reading model solutions.
        OPEN(UNIT=2,ACTION='READ',FILE=TRIM(filen)//'\MC_GIXTAB.CSV')
!  Read year column labels.
        READ(2,*) label
!  Read model solutions into mcgixtab matrix.
        DO i = 1,numgixtab
          READ(2,'(a)') line
          DO WHILE (index(line,',NA,').gt.0)
            is=index(line,',NA,')
            line=line(1:is)//line(is+3:)
          END DO
          READ(line,*) label,(mcgixtab(i,j),j=1,mamlastyr-1990+1)
        END DO
!  Close MC_GIXTAB.CSV for reading model solutions.
        CLOSE(2)

!  Write gi scenario values to global data structure
        DO i = 1, (mamlastyr-1990+1)
          mc_bopcrntac(i)     = mcgixtab(1,i)
          mc_cd(i)            = mcgixtab(2,i)
          mc_cdfhe(i)         = mcgixtab(3,i)
          mc_cdfher(i)        = mcgixtab(4,i)
          mc_cdmv(i)          = mcgixtab(5,i)
          mc_cdmvr(i)         = mcgixtab(6,i)
          mc_cdo(i)           = mcgixtab(7,i)
          mc_cdor(i)          = mcgixtab(8,i)
          mc_cdr(i)           = mcgixtab(9,i)
          mc_cdrec(i)         = mcgixtab(10,i)
          mc_cdrecr(i)        = mcgixtab(11,i)
          mc_cgoods(i)        = mcgixtab(12,i)
          mc_cgoodsr(i)       = mcgixtab(13,i)
          mc_ckf(i)           = mcgixtab(14,i)
          mc_ckfadjcorp(i)    = mcgixtab(15,i)
          mc_ckfcorp(i)       = mcgixtab(16,i)
          mc_ckfcorpbk(i)     = mcgixtab(17,i)
          mc_cn(i)            = mcgixtab(18,i)
          mc_cncs(i)          = mcgixtab(19,i)
          mc_cncsr(i)         = mcgixtab(20,i)
          mc_cnefao(i)        = mcgixtab(21,i)
          mc_cnefaor(i)       = mcgixtab(22,i)
          mc_cnegao(i)        = mcgixtab(23,i)
          mc_cnegaor(i)       = mcgixtab(24,i)
          mc_cnf(i)           = mcgixtab(25,i)
          mc_cnfr(i)          = mcgixtab(26,i)
          mc_cnoo(i)          = mcgixtab(27,i)
          mc_cnoor(i)         = mcgixtab(28,i)
          mc_cnopmp(i)        = mcgixtab(29,i)
          mc_cnopmpr(i)       = mcgixtab(30,i)
          mc_cnotob(i)        = mcgixtab(31,i)
          mc_cnotobr(i)       = mcgixtab(32,i)
          mc_cnr(i)           = mcgixtab(33,i)
          mc_cons(i)          = mcgixtab(34,i)
          mc_consr(i)         = mcgixtab(35,i)
          mc_cpi(11,i)        = mcgixtab(36,i)
          mc_cpie(i)          = mcgixtab(37,i)
          mc_cpixfae(i)       = mcgixtab(38,i)
          mc_csv(i)           = mcgixtab(39,i)
          mc_csvac(i)         = mcgixtab(40,i)
          mc_csvacr(i)        = mcgixtab(41,i)
          mc_csvf(i)          = mcgixtab(42,i)
          mc_csvfin(i)        = mcgixtab(43,i)
          mc_csvfinr(i)       = mcgixtab(44,i)
          mc_csvfr(i)         = mcgixtab(45,i)
          mc_csvh(i)          = mcgixtab(46,i)
          mc_csvhc(i)         = mcgixtab(47,i)
          mc_csvhcr(i)        = mcgixtab(48,i)
          mc_csvhh(i)         = mcgixtab(49,i)
          mc_csvhhr(i)        = mcgixtab(50,i)
          mc_csvhr(i)         = mcgixtab(51,i)
          mc_csvins(i)        = mcgixtab(52,i)
          mc_csvinsr(i)       = mcgixtab(53,i)
          mc_csvnpish(i)      = mcgixtab(54,i)
          mc_csvnpishr(i)     = mcgixtab(55,i)
          mc_csvo(i)          = mcgixtab(56,i)
          mc_csvor(i)         = mcgixtab(57,i)
          mc_csvr(i)          = mcgixtab(58,i)
          mc_csvrec(i)        = mcgixtab(59,i)
          mc_csvrecr(i)       = mcgixtab(60,i)
          mc_csvts(i)         = mcgixtab(61,i)
          mc_csvtsr(i)        = mcgixtab(62,i)
          mc_csvu(i)          = mcgixtab(63,i)
          mc_csvur(i)         = mcgixtab(64,i)
          mc_eea(i)           = mcgixtab(65,i)
!          mc_emf(i)           = mcgixtab(66,i)
          mc_g(i)             = mcgixtab(67,i)
          mc_gdp(i)           = mcgixtab(68,i)
          mc_gdpfer(i)        = mcgixtab(69,i)
          mc_gdpr(i)          = mcgixtab(70,i)
          mc_gf(i)            = mcgixtab(71,i)
          mc_gfml(i)          = mcgixtab(72,i)
          mc_gfmlr(i)         = mcgixtab(73,i)
          mc_gfo(i)           = mcgixtab(74,i)
          mc_gfor(i)          = mcgixtab(75,i)
          mc_gfr(i)           = mcgixtab(76,i)
          mc_gnp(i)           = mcgixtab(77,i)
          mc_gr(i)            = mcgixtab(78,i)
          mc_gsl(i)           = mcgixtab(79,i)
          mc_gslr(i)          = mcgixtab(80,i)
          mc_huesold(i)       = mcgixtab(81,i)
          mc_husps(i)         = mcgixtab(82,i)
          mc_i(i)             = mcgixtab(83,i)
          mc_ifnre(i)         = mcgixtab(84,i)
          mc_ifnree(i)        = mcgixtab(85,i)
          mc_ifnreeind(i)     = mcgixtab(86,i)
          mc_ifnreeindr(i)    = mcgixtab(87,i)
          mc_ifnreeip(i)      = mcgixtab(88,i)
          mc_ifnreeipccr(i)   = mcgixtab(89,i)
          mc_ifnreeipctr(i)   = mcgixtab(90,i)
          mc_ifnreeipr(i)     = mcgixtab(91,i)
          mc_ifnreeo(i)       = mcgixtab(92,i)
          mc_ifnreeor(i)      = mcgixtab(93,i)
          mc_ifnreer(i)       = mcgixtab(94,i)
          mc_ifnreetac(i)     = mcgixtab(95,i)
          mc_ifnreetacr(i)    = mcgixtab(96,i)
          mc_ifnreetlv(i)     = mcgixtab(97,i)
          mc_ifnreetlvr(i)    = mcgixtab(98,i)
          mc_ifnreeto(i)      = mcgixtab(99,i)
          mc_ifnreetor(i)     = mcgixtab(100,i)
          mc_ifnreetr(i)      = mcgixtab(101,i)
          mc_ifnrer(i)        = mcgixtab(102,i)
          mc_ifnres(i)        = mcgixtab(103,i)
          mc_ifnresc(i)       = mcgixtab(104,i)
          mc_ifnrescr(i)      = mcgixtab(105,i)
          mc_ifnresmfg(i)     = mcgixtab(106,i)
          mc_ifnresmfgr(i)    = mcgixtab(107,i)
          mc_ifnresmi(i)      = mcgixtab(108,i)
          mc_ifnresmir(i)     = mcgixtab(109,i)
          mc_ifnreso(i)       = mcgixtab(110,i)
          mc_ifnresor(i)      = mcgixtab(111,i)
          mc_ifnresp(i)       = mcgixtab(112,i)
          mc_ifnrespc(i)      = mcgixtab(113,i)
          mc_ifnrespcr(i)     = mcgixtab(114,i)
          mc_ifnrespp(i)      = mcgixtab(115,i)
          mc_ifnresppr(i)     = mcgixtab(116,i)
          mc_ifnrespr(i)      = mcgixtab(117,i)
          mc_ifnresr(i)       = mcgixtab(118,i)
          mc_ifre(i)          = mcgixtab(119,i)
          mc_ifree(i)         = mcgixtab(120,i)
          mc_ifreer(i)        = mcgixtab(121,i)
          mc_ifrer(i)         = mcgixtab(122,i)
          mc_ifres(i)         = mcgixtab(123,i)
          mc_ifresr(i)        = mcgixtab(124,i)
          mc_ii(i)            = mcgixtab(125,i)
          mc_iicmiu(i)        = mcgixtab(126,i)
          mc_iicmiur(i)       = mcgixtab(127,i)
          mc_iif(i)           = mcgixtab(128,i)
          mc_iifr(i)          = mcgixtab(129,i)
          mc_iim(i)           = mcgixtab(130,i)
          mc_iimr(i)          = mcgixtab(131,i)
          mc_iinf(i)          = mcgixtab(132,i)
          mc_iinfr(i)         = mcgixtab(133,i)
          mc_iir(i)           = mcgixtab(134,i)
          mc_iirt(i)          = mcgixtab(135,i)
          mc_iirtr(i)         = mcgixtab(136,i)
          mc_iiw(i)           = mcgixtab(137,i)
          mc_iiwr(i)          = mcgixtab(138,i)
          mc_intnetbus(i)     = mcgixtab(139,i)
          mc_ipsb50001(i)     = mcgixtab(140,i)
          mc_ir(i)            = mcgixtab(141,i)
          mc_ivacorp(i)       = mcgixtab(142,i)
          mc_jcsmich(i)       = mcgixtab(143,i)
          mc_jeciwssp(i)      = mcgixtab(144,i)
          mc_jexchmtp(i)      = mcgixtab(145,i)
          mc_jpgdp(i)         = mcgixtab(146,i)
          mc_jqpcmhnf(i)      = mcgixtab(147,i)
          mc_m(i)             = mcgixtab(148,i)
          mc_mfy(i)           = mcgixtab(149,i)
          mc_mg(i)            = mcgixtab(150,i)
          mc_mgr(i)           = mcgixtab(151,i)
          mc_mr(i)            = mcgixtab(152,i)
          mc_msvtot(i)        = mcgixtab(153,i)
          mc_msvtotr(i)       = mcgixtab(154,i)
          mc_netcfiva(i)      = mcgixtab(155,i)
          mc_netsavgfunify(i) = mcgixtab(156,i)
          mc_nlfc(i)          = mcgixtab(157,i)
          mc_nnp(i)           = mcgixtab(158,i)
          mc_poilwti(i)       = mcgixtab(159,i)
          mc_rmcorppuaa(i)    = mcgixtab(160,i)
          mc_rmff(i)          = mcgixtab(161,i)
          mc_rmmtg30con(i)    = mcgixtab(162,i)
          mc_rmtb3m(i)        = mcgixtab(163,i)
          mc_rmtcm10y(i)      = mcgixtab(164,i)
          mc_ruc(i)           = mcgixtab(165,i)
          mc_savprate(i)      = mcgixtab(166,i)
          mc_sfdprodr(i)      = mcgixtab(167,i)
          mc_sp500(i)         = mcgixtab(168,i)
          mc_sublsurpgf(i)    = mcgixtab(169,i)
          mc_sublsurpgsl(i)   = mcgixtab(170,i)
          mc_suva(i)          = mcgixtab(171,i)
          mc_suvtl(i)         = mcgixtab(172,i)
          mc_trfbus(i)        = mcgixtab(173,i)
          mc_txcorp(i)        = mcgixtab(174,i)
          mc_txcorpgf(i)      = mcgixtab(175,i)
          mc_txcorpgsl(i)     = mcgixtab(176,i)
          mc_txim(i)          = mcgixtab(177,i)
          mc_utlb00004(i)     = mcgixtab(178,i)
          mc_wpi(i)           = mcgixtab(179,i)
          mc_wpi05(i)         = mcgixtab(180,i)
          mc_wpi10(i)         = mcgixtab(181,i)
          mc_wpiind05(i)      = mcgixtab(182,i)
          mc_wpisop3000(i)    = mcgixtab(183,i)
          mc_x(i)             = mcgixtab(184,i)
          mc_xfy(i)           = mcgixtab(185,i)
          mc_xg(i)            = mcgixtab(186,i)
          mc_xgr(i)           = mcgixtab(187,i)
          mc_xr(i)            = mcgixtab(188,i)
          mc_xsvtot(i)        = mcgixtab(189,i)
          mc_xsvtotr(i)       = mcgixtab(190,i)
          mc_yn(i)            = mcgixtab(191,i)
          mc_yp(11,i)         = mcgixtab(192,i)
          mc_ypcomp(i)        = mcgixtab(193,i)
          mc_ypd(i)           = mcgixtab(194,i)
          mc_ypdr(11,i)       = mcgixtab(195,i)
          mc_yppropadjf(i)    = mcgixtab(196,i)
          mc_yppropadjnf(i)   = mcgixtab(197,i)
          mc_yprentadj(i)     = mcgixtab(198,i)
          mc_za(i)            = mcgixtab(199,i)
          mc_zadiv(i)         = mcgixtab(200,i)
          mc_zar(i)           = mcgixtab(201,i)
          mc_zare(i)          = mcgixtab(202,i)
          mc_zb(i)            = mcgixtab(203,i)
          mc_zbecon(i)        = mcgixtab(204,i)
          mc_zbivarw(i)       = mcgixtab(205,i)

        END DO

!  End of subroutine GISUB. Return to calling subroutine MAC.
       RETURN
       END


!***********************************************************************
       SUBROUTINE DRTLINK
       USE DFLIB
!***********************************************************************
!  The DRTLINK subroutine executes GI and EIA models within EViews. NEMS
!  energy driver variables are passed to the US model. These are the
!  shocks introduced exogenously to the model. The US model and other GI
!  and EIA downstream models are simulated. The resulting forecasts are
!  returned to the MAM. Results are written to the global data structure
!  and to output spreadsheets. This subroutine is executed for the first
!  iteration of the first year.
!
!  To add a driver variable enlarge the size of matrices dlink, nlink,
!  dlvars and the integers nemsenergylabel, nemsenergynum, and scennum.
!  dlink contains the data. nlink contains the detail. dlvars contains
!  the labels for dlink. nemsenergylabel is the number of labels.
!  nemsenergynum is the number of energy variables. scennum is the
!  number of drivers.  Changes are needed to mac.f, mcinter2 and
!  mcparms.txt.
!
!***********************************************************************

       IMPLICIT NONE

!  Include parameter files.
       INTEGER      FILE_MGR
       EXTERNAL     FILE_MGR
       CHARACTER*30 name     ! File name for FILE_MGR
       LOGICAL      new      ! Logical variable for FILE_MGR
       INTEGER      inunit   ! Unit number for reads as returned from FILE_MGR
       LOGICAL      problem

!  Include parameter files.
       INCLUDE'parametr'
       INCLUDE'apq'
       INCLUDE'macout'
       INCLUDE'pmmout'
       INCLUDE'lfmmout'
       INCLUDE'pmmrpt'
       INCLUDE'pmmftab'
       INCLUDE'ogsmout'
       INCLUDE'convfact'
       INCLUDE'ngtdmrep'
       INCLUDE'angtdm'
       INCLUDE'ncntrl'
       INCLUDE'emmparm'
       INCLUDE'emission'
       INCLUDE'ghgrep'
       INCLUDE'cdsparms'
       INCLUDE'coalout'
       INCLUDE'coalrep'
       INCLUDE'intout'
       INCLUDE'wrenew'
       INCLUDE'tranrep'
       INCLUDE'uefdout'
       INCLUDE 'macparm.'
       INCLUDE 'mcinter2.'
       INCLUDE 'mcdetail'
       INCLUDE 'continew'

!  Declare functions for NEMS parameters.
       EXTERNAL getindex,rtovalue
       INTEGER  getindex,rtovalue

!  Local variable definitions.
       REAL           rdays,scaleprice
       INTEGER        i,j,iyr,charc,fyrprc,indx,cd,is
       INTEGER*4      iret,eviewsversion
       CHARACTER      eviewscommand*80,cmdline*500,myline*128
       CHARACTER*8    computername,eviewsenv*8/' '/
       CHARACTER*255  pwd,filen,fimcevwork,fimceviomd,fimcevepmd,fimcevrgmd,fimcevsubs
       CHARACTER*3500 line,label


! Declares for OSCall  to invoke EViews.
        CHARACTER*80 args
        INTEGER*4    iWaitMS/-1/ ! Wait for OSCall in milliseconds. if -1, infinite wait
        LOGICAL      lstatus
        INTEGER*4    WALL_EV_START, WALL_EV_END, CPU_DUMMY_TIME
        REAL         ELAPSED_EV_TIME

       iWaitMS = 1000 * 60 * 15  !  milliseconds converting seconds to # of minutes (last multiplicand)
!  Initialize values.
       rdays = 365
       fyrprc = 2012
       scaleprice=mc_jpgdp(fyrprc-1989)

!  START DO iyr: Loop through years within scenario.
       DO iyr = 1,(mamlastyr-1990+1)

!  Prod_CrudeOil
         dlink(1,iyr)   = nlink(1,iyr)
!  Prod_NaturalGas
         dlink(2,iyr)   = nlink(4,iyr)
!  Prod_DryNaturalGas
         dlink(3,iyr)   = nlink(7,iyr)
!  Prod_Coal
         dlink(4,iyr)   = nlink(11,iyr)
!  Prod_Nuclear
         dlink(5,iyr)   = nlink(14,iyr)
!  Prod_Hydro
         dlink(6,iyr)   = nlink(16,iyr)
!  Prod_Biomass
         dlink(7,iyr)   = nlink(18,iyr)
!  Prod_OthRenew
         dlink(8,iyr)   = nlink(29,iyr)
!  Prod_Other
         dlink(9,iyr)   = nlink(40,iyr)
!  Import_Petroleum
         dlink(10,iyr)   = nlink(52,iyr)
!  Import_NatGas
         dlink(11,iyr)   = nlink(65,iyr)
!  Import_BioFuels
         dlink(12,iyr)   = nlink(68,iyr)
!  Import_Coal
         dlink(13,iyr)   = nlink(75,iyr)
!  Import_Other
         dlink(14,iyr)   = nlink(77,iyr)
!  Export_Fuels
         dlink(15,iyr)   = nlink(80,iyr)
!  Export_Petroleum
         dlink(16,iyr)   = nlink(92,iyr)
!  Export_NatGas
         dlink(17,iyr)   = nlink(97,iyr)
!  Export_Coal
         dlink(18,iyr)   = nlink(100,iyr)
!  Export_BioFuels
         dlink(19,iyr)   = nlink(102,iyr)
!  Export_Other
         dlink(20,iyr)   = nlink(109,iyr)
!  Cons_Petroleum
         dlink(21,iyr)   = nlink(110,iyr)
!  Cons_NatGas
         dlink(22,iyr)   = nlink(114,iyr)
!  Cons_Coal
         dlink(23,iyr)   = nlink(121,iyr)
!  Cons_Nuclear
         dlink(24,iyr)   = nlink(127,iyr)
!  Cons_Hydro
         dlink(25,iyr)   = nlink(129,iyr)
!  Cons_Biomass
         dlink(26,iyr)   = nlink(131,iyr)
!  Cons_OthRenew
         dlink(27,iyr)   = nlink(151,iyr)
!  Cons_Other
         dlink(28,iyr)   = nlink(162,iyr)
!  ElGen_Renewable
         dlink(29,iyr)   = nlink(166,iyr)
!  DelEng_Coal
         dlink(30,iyr)   = nlink(169,iyr)
!  DelEng_Electricity
         dlink(31,iyr)   = nlink(174,iyr)
!  DelEng_NaturalGas
         dlink(32,iyr)   = nlink(176,iyr)
!  DelEng_Petroleum
         dlink(33,iyr)   = nlink(181,iyr)
!  DelEng_Bio
         dlink(34,iyr)   = nlink(195,iyr)
!  TranCon_Distillate
         dlink(35,iyr)   = nlink(202,iyr)
!  ResCon_Petroleum
         dlink(36,iyr)   = nlink(204,iyr)
!  ResCon_NaturalGas
         dlink(37,iyr)   = nlink(206,iyr)
!  ResCon_Electricity
         dlink(38,iyr)   = nlink(208,iyr)
!  ResPrice_Petroleum
         dlink(39,iyr)   = nlink(210,iyr)
!  ResPrice_Electricity
         dlink(40,iyr)   = nlink(212,iyr)
!  ResPrice_NaturalGas
         dlink(41,iyr)   = nlink(214,iyr)
!  TranPrice_Motorgas
         dlink(42,iyr)   = nlink(216,iyr)
!  TranPrice_Distillate
         dlink(43,iyr)   = nlink(218,iyr)
!  IndPrice_SteamCoal
         dlink(44,iyr)   = nlink(220,iyr)
!  GasWellheadPrice
         dlink(45,iyr)   = nlink(222,iyr)
!  ComPrice_Electricity
         dlink(46,iyr)   = nlink(224,iyr)
!  IndPrice_Electricity
         dlink(47,iyr)   = nlink(226,iyr)
!  TranPrice_Electricity
         dlink(48,iyr)   = nlink(228,iyr)
!  ComPrice_NaturalGas
         dlink(49,iyr)   = nlink(230,iyr)
!  IndPrice_NaturalGas
         dlink(50,iyr)   = nlink(232,iyr)
!  TranPrice_NaturalGas
         dlink(51,iyr)   = nlink(234,iyr)
!  ElGenPrice_NaturalGas
         dlink(52,iyr)   = nlink(236,iyr)
!  WorldOilPrice
         dlink(53,iyr)   = nlink(238,iyr)
!  ComPrice_Distillate
         dlink(54,iyr)   = nlink(240,iyr)
!  ComPrice_ResidualFuel
         dlink(55,iyr)   = nlink(242,iyr)
!  IndPrice_Distillate
         dlink(56,iyr)   = nlink(244,iyr)
!  IndPrice_ResidualFuel
         dlink(57,iyr)   = nlink(246,iyr)
!  TranPrice_JetFuel
         dlink(58,iyr)   = nlink(248,iyr)
!  TranPrice_ResidualFuel
         dlink(59,iyr)   = nlink(250,iyr)
!  CarbEm_ElGenTotal
         dlink(60,iyr)   = nlink(252,iyr)
!  CarbPen_Resulting
         dlink(61,iyr)   = nlink(256,iyr)
!  TranPrice_AvgVehicle (NEMS Converting base year of price to 2012 for US model from 1990 base year in TRAN.F)
         dlink(62,iyr)   = nlink(258,iyr)
!  TranTax_HwyFuel - NEMS Incremental tax on petroleum based highway fuels.
         dlink(63,iyr)   = nlink(264,iyr)
!  EmisRev_Total - NEMS Emission revenues by sector (1=residential, 2=commercial, 3=industrial, 4=transportation
!   and 5=electric power) in billions of nominal dollars.
!        dlink(64,iyr)   = (emrev(1,iyr) + emrev(2,iyr) + emrev(3,iyr) + emrev(4,iyr) + emrev(5,iyr)) &
!                       * scaleprice / mc_jpgdp(iyr)
         dlink(64,iyr)   = nlink(266,iyr)
!  ElSec_InstCap, Total Resource Costs, Electric Sector, Annual Expense and Capital Commitment, Installed Capacity in Billion 2012 Dollars
         dlink(65,iyr)   = nlink(272,iyr)
!  ElSec_Tran, Total Resource Costs, Electric Sector, Annual Expense and Capital Commitment, Transmission in Billion 2012 Dollars
         dlink(66,iyr)   = nlink(274,iyr)
!  ElSec_Retro, Total Resource Costs, Electric Sector, Annual Expense and Capital Commitment, Retrofits in Billion 2012 Dollars
         dlink(67,iyr)   = nlink(276,iyr)
!  EmisRev_Cover - NEMS Covered GHG emissions revenue (covered emissions*full carbon/ghg trading price) in billions of 1987 dollars.
         dlink(68,iyr)   = nlink(277,iyr)
!  EmisRev_Intl - NEMS International offset revenue (abatement*offset price) in billions of 1987 dollars.
         dlink(69,iyr)   = nlink(279,iyr)
!  EmisAuct_Share - NEMS Share of allowances that are auctioned.
         dlink(70,iyr)   = nlink(281,iyr)
!  JPGDP (Converting base year of deflator to 2012 for GI model from 1987 base year in NEMS)
         dlink(71,iyr)   = nlink(282,iyr)

!  The following seven series are industrial outputs and employment whose forecast are 
!    determined by NEMS. This industrial forecast is used in EViews to determine the
!    employment forecast. This industrial forecast is always one iteration back since
!    the MAM solves before most other NEMS modules. In addition, the models in EViews
!    solve before the INDUSTSUB subroutine. That is the MAM subroutine in which the
!    forecast is done.
!  MacIO25_PetroRefine - NEMS Industrial Output 25: Petroleum Refining (NAICS 32411), manufacturing
         dlink(72,iyr)   = nlink(283,iyr)
!  MacIO45_CoalMine - NEMS Industrial Output 45: Coal Mining (NAICS 2121), non-manufacturing
         dlink(73,iyr)   = nlink(286,iyr)
!  MacIO46_OilGasXtract - NEMS Industrial Output 46: Oil and Gas Extraction and Support Activities (NAICS 211, 213), non-manufacturing
         dlink(74,iyr)   = nlink(287,iyr)
!  MacIO51_ElecUtil - NEMS Industrial Output 51: Electric Power Generation and Distribution (NAICS 2211), non-industrial/services
         dlink(75,iyr)   = nlink(292,iyr)
!  MacIO52_GasUtil - NEMS Industrial Output 52: Natural Gas Distribution (NAICS 2212), non-industrial/services
         dlink(76,iyr)   = nlink(295,iyr)
!  MacEP36_CoalMine - NEMS Employment 36: Coal Mining (NAICS 2121), non-manufacturing
         dlink(77,iyr)   = nlink(296,iyr)
!  MacEP37_OilGasXtract - NEMS Employment 37: Oil and Gas Extraction and Support Activities (NAICS 211, 213), non-manufacturing
         dlink(78,iyr)   = nlink(297,iyr)

!  HenryHubPrice - NEMS Henry Hub Natural Gas Price
         dlink(79,iyr)   = nlink(298,iyr)
!  ComCon_Electricity - NEMS Purchased Electricity in Commercial
         dlink(80,iyr)   = nlink(300,iyr)
!  IndCon_Electricity - NEMS Purchased Electricity in Industrial
         dlink(81,iyr)   = nlink(302,iyr)
!  TranCon_Electricity - NEMS Purchased Electricity in Transportation
         dlink(82,iyr)   = nlink(304,iyr)
!  ComCon_NaturalGas - NEMS Natural Gas in Commercial
         dlink(83,iyr)   = nlink(306,iyr)
!  IndCon_NaturalGas - NEMS Natural Gas in Industrial
         dlink(84,iyr)   = nlink(308,iyr)
!  TranCon_NaturalGas - NEMS Natural Gas in Transportation
         dlink(85,iyr)   = nlink(310,iyr)
!  ElGenCon_NaturalGas - NEMS Natural Gas in Electrial Generation
         dlink(86,iyr)   = nlink(312,iyr)
!  ElGenCon_SteamCoal - NEMS Steam Coal in Electrical Generation
         dlink(87,iyr)   = nlink(314,iyr)
!  ElGenCon_FuelOil - NEMS Distillate and Residual Fuel Oil in Electrical Generation
         dlink(88,iyr)   = nlink(316,iyr)
!  ElGenCon_ElecImports - NEMS Electricity Imports
         dlink(89,iyr)   = nlink(319,iyr)
!  ElGenCon_ElecPowerTot - NEMS Electric Power Total
         dlink(90,iyr)   = nlink(321,iyr)
!  ComCon_Distillate - NEMS Distillate in Commercial
         dlink(91,iyr)   = nlink(323,iyr)
!  ComCon_ResidualFuel - NEMS Residual Fuel in Commercial
         dlink(92,iyr)   = nlink(325,iyr)
!  IndCon_Distillate - NEMS Distillate in Industrial
         dlink(93,iyr)   = nlink(327,iyr)
!  IndCon_ResidualFuel - NEMS Residual Fuel in Industrial
         dlink(94,iyr)   = nlink(329,iyr)
!  TranCon_MotorGas - NEMS Motor Gas in Transportation
         dlink(95,iyr)   = nlink(331,iyr)
!  TranCon_ResidualFuel - NEMS Residual Fuel in Transportation
         dlink(96,iyr)   = nlink(333,iyr)
!  TranCon_JetFuel - NEMS Jet Fuel in Transportation
         dlink(97,iyr)   = qjftr(11,iyr) * .001
!  Ethanol_Imports - NEMS Ethanol Imports in Refinery
         dlink(98,iyr)   = nlink(335,iyr)
!  Ethanol_WholesalePrice - NEMS Ethanol Wholesale Price in Refinery
         dlink(99,iyr)   = nlink(337,iyr)
!  Corn_Price - NEMS Corn Price in Refinery
         dlink(100,iyr)   = nlink(339,iyr)
!  TranCon_Ethanol - NEMS Ethanol Quantity in Transportation
         dlink(101,iyr)   = nlink(341,iyr)
!  TranPrice_Ethanol - NEMS Ethanol Price in Transportation
         dlink(102,iyr)   = nlink(343,iyr)
!  WestTexasOilPrice - NEMS West Texas Oil Price
         dlink(103,iyr)   = nlink(345,iyr)
!  LPGFeedstockPrice - NEMS LPG Feedstock Price
         dlink(104,iyr)   = nlink(347,iyr)
!  NaphthaPrice - NEMS Naphtha Price
         dlink(105,iyr)   = nlink(349,iyr)
!  EthanePrice - NEMS Ethane Price
         dlink(106,iyr)   = nlink(351,iyr)
!  EthaneProduction - NEMS Ethane Production
         dlink(107,iyr)   = nlink(353,iyr)
!  PropaneProduction - NEMS Propane Production
         dlink(108,iyr)   = nlink(355,iyr)
!  RINRevenue - NEMS RIN Revenue
         dlink(109,iyr)   = nlink(357,iyr)


!  Ethanol_IP - NEMS Value of Ethanol Production
         dlink(110,iyr)     =  nlink(359,iyr)*nlink(360,iyr)/1000.


!  Producer price indexes at the Census division level where:
!   New England - 01, Middle Atlantic - 02, East North Central - 03,
!   West North Central - 04, South Atlantic - 05, East South Central - 06,
!   West South Central - 07, Mountain - 08, Pacific - 09,
!   California - 10 and United States - 11

         indx = 111
         DO cd=1,9
!  Producer price index - coal wpi051
           dlink(indx,iyr) = pclin(cd,iyr)   * scaleprice
           indx = indx + 1
!  Producer price index - gas fuels wpi053
           dlink(indx,iyr) = pngin(cd,iyr)   * scaleprice
           indx = indx + 1
!  Producer price index - electric power wpi054
           dlink(indx,iyr) = pelrs(cd,iyr)*scaleprice*qelrs(cd,iyr)/(qelrs(cd,iyr)+qelcm(cd,iyr)+qelin(cd,iyr)+qeltr(cd,iyr))
           dlink(indx,iyr) = dlink(indx,iyr)+(pelcm(cd,iyr)*scaleprice*qelcm(cd,iyr)/(qelrs(cd,iyr)+qelcm(cd,iyr)+qelin(cd,iyr)+qeltr(cd,iyr)))
           dlink(indx,iyr) = dlink(indx,iyr)+(pelin(cd,iyr)*scaleprice*qelin(cd,iyr)/(qelrs(cd,iyr)+qelcm(cd,iyr)+qelin(cd,iyr)+qeltr(cd,iyr)))
           dlink(indx,iyr) = dlink(indx,iyr)+(peltr(cd,iyr)*scaleprice*qeltr(cd,iyr)/(qelrs(cd,iyr)+qelcm(cd,iyr)+qelin(cd,iyr)+qeltr(cd,iyr)))
           indx = indx + 1
!  Producer price index - utility natural gas wpi055
           dlink(indx,iyr) = pngrs(cd,iyr)*scaleprice*qngrs(cd,iyr)/(qngrs(cd,iyr)+qngcm(cd,iyr)+qngin(cd,iyr)+qngtr(cd,iyr)+qngel(cd,iyr))
           dlink(indx,iyr) = dlink(indx,iyr)+(pngcm(cd,iyr)*scaleprice*qngcm(cd,iyr)/(qngrs(cd,iyr)+qngcm(cd,iyr)+qngin(cd,iyr)+qngtr(cd,iyr)+qngel(cd,iyr)))
           dlink(indx,iyr) = dlink(indx,iyr)+(pngin(cd,iyr)*scaleprice*qngin(cd,iyr)/(qngrs(cd,iyr)+qngcm(cd,iyr)+qngin(cd,iyr)+qngtr(cd,iyr)+qngel(cd,iyr)))
           dlink(indx,iyr) = dlink(indx,iyr)+(pngtr(cd,iyr)*scaleprice*qngtr(cd,iyr)/(qngrs(cd,iyr)+qngcm(cd,iyr)+qngin(cd,iyr)+qngtr(cd,iyr)+qngel(cd,iyr)))
           dlink(indx,iyr) = dlink(indx,iyr)+(pngel(cd,iyr)*scaleprice*qngel(cd,iyr)/(qngrs(cd,iyr)+qngcm(cd,iyr)+qngin(cd,iyr)+qngtr(cd,iyr)+qngel(cd,iyr)))
           indx = indx + 1
!  Producer price index - refined petroleum products wpi057
           dlink(indx,iyr) = ptprs(cd,iyr)*scaleprice*qtprs(cd,iyr)/(qtprs(cd,iyr)+qdscm(cd,iyr)+qrscm(cd,iyr)+qdsin(cd,iyr)+qrsin(cd,iyr)+qdstr(cd,iyr)+qjftr(cd,iyr)+qmgtr(cd,iyr)+qrstr(cd,iyr))
           dlink(indx,iyr) = dlink(indx,iyr)+(pdscm(cd,iyr)*scaleprice*qdscm(cd,iyr)/(qtprs(cd,iyr)+qdscm(cd,iyr)+qrscm(cd,iyr)+qdsin(cd,iyr)+qrsin(cd,iyr)+qdstr(cd,iyr)+qjftr(cd,iyr)+qmgtr(cd,iyr)+qrstr(cd,iyr)))
           dlink(indx,iyr) = dlink(indx,iyr)+(prscm(cd,iyr)*scaleprice*qrscm(cd,iyr)/(qtprs(cd,iyr)+qdscm(cd,iyr)+qrscm(cd,iyr)+qdsin(cd,iyr)+qrsin(cd,iyr)+qdstr(cd,iyr)+qjftr(cd,iyr)+qmgtr(cd,iyr)+qrstr(cd,iyr)))
           dlink(indx,iyr) = dlink(indx,iyr)+(pdsin(cd,iyr)*scaleprice*qdsin(cd,iyr)/(qtprs(cd,iyr)+qdscm(cd,iyr)+qrscm(cd,iyr)+qdsin(cd,iyr)+qrsin(cd,iyr)+qdstr(cd,iyr)+qjftr(cd,iyr)+qmgtr(cd,iyr)+qrstr(cd,iyr)))
           dlink(indx,iyr) = dlink(indx,iyr)+(prsin(cd,iyr)*scaleprice*qrsin(cd,iyr)/(qtprs(cd,iyr)+qdscm(cd,iyr)+qrscm(cd,iyr)+qdsin(cd,iyr)+qrsin(cd,iyr)+qdstr(cd,iyr)+qjftr(cd,iyr)+qmgtr(cd,iyr)+qrstr(cd,iyr)))
           dlink(indx,iyr) = dlink(indx,iyr)+(pdstr(cd,iyr)*scaleprice*qdstr(cd,iyr)/(qtprs(cd,iyr)+qdscm(cd,iyr)+qrscm(cd,iyr)+qdsin(cd,iyr)+qrsin(cd,iyr)+qdstr(cd,iyr)+qjftr(cd,iyr)+qmgtr(cd,iyr)+qrstr(cd,iyr)))
           dlink(indx,iyr) = dlink(indx,iyr)+(pjftr(cd,iyr)*scaleprice*qjftr(cd,iyr)/(qtprs(cd,iyr)+qdscm(cd,iyr)+qrscm(cd,iyr)+qdsin(cd,iyr)+qrsin(cd,iyr)+qdstr(cd,iyr)+qjftr(cd,iyr)+qmgtr(cd,iyr)+qrstr(cd,iyr)))
           dlink(indx,iyr) = dlink(indx,iyr)+(pmgtr(cd,iyr)*scaleprice*qmgtr(cd,iyr)/(qtprs(cd,iyr)+qdscm(cd,iyr)+qrscm(cd,iyr)+qdsin(cd,iyr)+qrsin(cd,iyr)+qdstr(cd,iyr)+qjftr(cd,iyr)+qmgtr(cd,iyr)+qrstr(cd,iyr)))
           dlink(indx,iyr) = dlink(indx,iyr)+(prstr(cd,iyr)*scaleprice*qrstr(cd,iyr)/(qtprs(cd,iyr)+qdscm(cd,iyr)+qrscm(cd,iyr)+qdsin(cd,iyr)+qrsin(cd,iyr)+qdstr(cd,iyr)+qjftr(cd,iyr)+qmgtr(cd,iyr)+qrstr(cd,iyr)))
           indx = indx + 1
!  Producer price index - residual petroleum fuels wpi0574
           dlink(indx,iyr) = prscm(cd,iyr)*scaleprice*qrscm(11,iyr)/(qrscm(11,iyr)+qrsin(11,iyr)+qrstr(11,iyr))
           dlink(indx,iyr) = dlink(indx,iyr)+(prsin(cd,iyr)*scaleprice*qrsin(11,iyr)/(qrscm(11,iyr)+qrsin(11,iyr)+qrstr(11,iyr)))
           dlink(indx,iyr) = dlink(indx,iyr)+(prstr(cd,iyr)*scaleprice*qrstr(11,iyr)/(qrscm(11,iyr)+qrsin(11,iyr)+qrstr(11,iyr)))
           indx = indx + 1
         END DO

!  END DO iyr: Loop through years within scenario.
       END DO

!  Open ALTDATA.CSV for write of driver variables
       OPEN(3,ACTION='WRITE',FILE='ALTDATA.CSV')
!  Write column headers.
       line = '" "'
       DO j = 1,scennum
         line = TRIM(line)//',"'//TRIM(dlvars(j))//'_A"'
         line = TRIM(line)
       END DO
       charc = LEN(TRIM(line))
       WRITE(3,'(A<charc>)') line
!  Write year and variable values across columns.
       DO i = 8,(mamlastyr-1990+1)
         WRITE(label,'(i4)') 1989+i
         line = TRIM(label)
         DO j = 1,scennum
           WRITE(label,'(F15.5)') dlink(j,i)
           line = TRIM(line)//',"'//TRIM(label)//'"'
           line = TRIM(line)
         END DO
         charc  = LEN(TRIM(line))
         WRITE(3,'(A<charc>)') line
       END DO
!  Close ALTDATA.CSV for write of driver variables.
       CLOSE(3)

!  Query present working directory and drive.
       pwd  = FILE$CURDRIVE
       iret = GETDRIVEDIRQQ(pwd)
!  START IF (iret .GT. 0): Write present working directory and drive to local
!    variable if one exists.
       IF (iret .GT. 0) THEN
         filen = TRIM(pwd)
!  START ELSE: Write an error if present working directory and drive do not exist.
       ELSE
         WRITE(*,*) "MAC.F: WARNING! NO DRIVE DETECTED."
!  END IF (iret .GT. 0): Write present working directory and drive to local
!    variable if one exists.
       END IF

!  Call environmental parameter locating EViews executable.
       eviewsversion=rtovalue("EVVERS  ",5)
       IF (eviewsversion .LT. 10) THEN
          WRITE(eviewsenv,'("EVIEWS",I1)') eviewsversion
           ELSE
              WRITE(eviewsenv,'("EVIEWS",I2)') eviewsversion
           ENDIF
       CALL GETENVmac(eviewsenv,eviewscommand,computername)

       IF (eviewscommand .NE. " ") THEN
         WRITE(6,'("  Using EViews version ",I2,"  command ",A)') eviewsversion,TRIM(eviewscommand)
       ELSE
         WRITE(6,'("  EViews version (",I2,") specified is different than those available on this computer")') eviewsversion
         WRITE(6,'("  EViews will be launched using ",A," .prg association")') computername
       END IF

!  MCEVWORK.WF1 file for GI US model.
       fimcevwork='MCEVWORK'
       inunit=FILE_MGR('O',fimcevwork,new)
       INQUIRE(UNIT=inunit,name=fimcevwork)
       CLOSE(inunit,STATUS='keep')

!  MCEVIOMD.WF1 file for GI IO model.
!       fimceviomd='MCEVIOMD'
!       inunit=FILE_MGR('O',fimceviomd,new)
!       INQUIRE(UNIT=inunit,name=fimceviomd)
!       CLOSE(inunit,STATUS='keep')

!  MCEVEPMD.WF1 file for GI EMPLOYMENT model.
!       fimcevepmd='MCEVEPMD'
!       inunit=FILE_MGR('O',fimcevepmd,new)
!       INQUIRE(UNIT=inunit,name=fimcevepmd)
!       CLOSE(inunit,STATUS='keep')

!  MCEVRGMD.WF1 file for EIA Regional model.
       fimcevrgmd='MCEVRGMD'
       inunit=FILE_MGR('O',fimcevrgmd,new)
       INQUIRE(UNIT=inunit,name=fimcevrgmd)
       CLOSE(inunit,STATUS='keep')

!  MCEVSUBS.PRG file model related subroutines in EViews.
       fimcevsubs='MCEVSUBS'
       inunit=FILE_MGR('O',fimcevsubs,new)
       INQUIRE(UNIT=inunit,name=fimcevsubs)
       CLOSE(inunit,STATUS='keep')

!  Open and read input file of EView commands, mcevcode.txt, and write to program
!   file drivers.prg.
        name='MCEVCODE'
        inunit=FILE_MGR('O',name,new)
        OPEN(3,ACTION='WRITE',FILE='DRIVERS.PRG')
!  START DO WHILE: Continue loop reading from input file until end of file marker
!    is returned.
        DO WHILE (.NOT. EOF(inunit))

          READ(inunit,'(A128)') myline
          charc = LEN(TRIM(myline))
!  START IF (TRIM(myline) .EQ. "INSERT_1"): Write replacement block to drivers.prg
!    if myline is "INSERT_1".
          IF (TRIM(myline) .EQ. "INSERT_1") THEN
            myline = 'optset "'//TRIM(filen)//'\"'
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
            myline = 'cd "'//TRIM(filen)//'"'
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
            myline = 'create '//TRIM(filen)//'\drvdata a 1990'
            charc  = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,1X,I4)') myline,mamlastyr
            myline = 'scalar mamlastyr = '
            charc  = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,1X,I4)') myline,mamlastyr
            myline = 'smpl 1997'
            charc  = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,1X,I4)') myline,mamlastyr
            myline = 'read "'//TRIM(filen)//'\altdata.csv"'
            charc  = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,1X,I4)') myline,scennum+1
            myline = 'read(s=residential,t) '//TRIM(filen)//'\comfloor.xls 27'
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
            taxmode = RTOVALUE('MACTAX  ',0)
            myline  = 'scalar taxmode='
            charc   = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,I2)') myline,taxmode
            mac111d = RTOVALUE('EPA111D ',0)
            myline  = 'scalar mac111d='
            charc   = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,I2)') myline,mac111d
            myline = 'sample s_fcst 2023'
            charc  = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,1X,I4)') myline,mamlastyr
            myline = 'scalar macmode='
            charc  = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,I1)') myline,mmac
!  START ELSE IF (TRIM(myline) .EQ. "INSERT_1A"): Write replacement block to drivers.prg
!    if myline is "INSERT_1A".
          ELSE IF (TRIM(myline) .EQ. "INSERT_1A") THEN
            myline = 'save '//TRIM(filen)//'\drvdata.wf1'
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
!  START ELSE IF (TRIM(myline) .EQ. "INSERT_2"): Write replacement block to drivers.prg
!    if myline is "INSERT_2".
          ELSE IF (TRIM(myline) .EQ. "INSERT_2") THEN
            myline = 'db '//TRIM(filen)//'\eviewsdb'
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
!  START ELSE IF (TRIM(myline) .EQ. "INSERT_3"): Write replacement block to drivers.prg
!    if myline is "INSERT_3".
          ELSE IF (TRIM(myline) .EQ. "INSERT_3") THEN
            myline = 'store '//TRIM(filen)//'\eviewsdb::ratio'
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
            myline = 'store '//TRIM(filen)//'\eviewsdb::jonshr'
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
            myline = 'load '//TRIM(fimcevwork)
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
!  NEMS MACTAX PARAMETER - Read presence and type of tax.
            taxmode = RTOVALUE('MACTAX  ',0)
            myline  = 'scalar taxmode='
            charc   = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,I2)') myline,taxmode
!  NEMS MAC111D PARAMETER - Read presence of 111d.
            mac111d = RTOVALUE('EPA111D ',0)
            myline  = 'scalar mac111d='
            charc   = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,I2)') myline,mac111d
!  NEMS CAFE PARAMETER - Read presence of CAFE.
            cafemode = RTOVALUE('MACCAFE ',0)
            myline   = 'scalar cafemode='
            charc    = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,I1)') myline,cafemode
!  NEMS TTECH PARAMETER - Read presence and level of technology.
            ttechmode = RTOVALUE('TTECH   ',0)
            myline    = 'scalar ttechmode='
            charc     = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,I1)') myline,ttechmode
!  NEMS MMAC PARAMETER - Read macro scenario.
            myline = 'scalar macmode='
            charc  = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,I1)') myline,mmac
!  NEMS WWOP PARAMETER - Read world oil price scenario.
            myline   = 'scalar wwopmode='
            charc     = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,I1)') myline,wwop
!  NEMS OGTECH PARAMETER - Demand Driver for Industrial production, GO367.
            ogtechmode = RTOVALUE('OGTECH  ',1)
            myline   = 'scalar ogtechmode='
            charc     = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,I2)') myline,ogtechmode
!  Import transportation size class data.
            myline = 'smpl 1967:1 2021:4'
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
            myline = 'read(s=TranC,b3) '//TRIM(filen)//'\mchighlo.xls 5'
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
!  START ELSE IF (TRIM(myline) .EQ. "INSERT_4"): Write replacement block to drivers.prg
!    if myline is "INSERT_4".
          ELSE IF (TRIM(myline) .EQ. "INSERT_4") THEN
            myline = 'smpl 1959:1'
            charc  = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,1X,I4,A2)') myline,mamlastyr,':4'
!  START ELSE IF (TRIM(myline) .EQ. "INSERT_5"): Write replacement block to drivers.prg
!    if myline is "INSERT_5".
          ELSE IF (TRIM(myline) .EQ. "INSERT_5") THEN
            myline = 'scalar mamlastyr = '
            charc  = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,1X,I4)') myline,mamlastyr
            myline = 'sample s_fcst 2023:1'
            charc  = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,1X,I4,A2)') myline,mamlastyr,':4'
!  START ELSE IF (TRIM(myline) .EQ. "INSERT_5A"): Write replacement block to drivers.prg
!    if myline is "INSERT_5A".
          ELSE IF (TRIM(myline) .EQ. "INSERT_5A") THEN
            myline = 'smpl s_fcst'
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
!  START ELSE IF (TRIM(myline) .EQ. "INSERT_6"): Write replacement block to drivers.prg
!    if myline is "INSERT_6".
          ELSE IF (TRIM(myline) .EQ. "INSERT_6") THEN
            myline = 'save '//TRIM(filen)//'\mcevwork.wf1'
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
!  START ELSE IF (TRIM(myline) .EQ. "INSERT_6A"): Write replacement block to drivers.prg
!    if myline is "INSERT_6A".
!  New Regional Model: Load workfile.
          ELSE IF (TRIM(myline) .EQ. "INSERT_6A") THEN
            myline = 'load '//TRIM(fimcevrgmd)
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
            myline = 'smpl 1970:1'
            charc  = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,1X,I4,A2)') myline,mamlastyr,':4'
            myline = 'scalar mamlastyr = '
            charc  = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,1X,I4)') myline,mamlastyr
            myline   = 'scalar ogtechmode='
            charc    = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,I2)') myline,ogtechmode
            myline   = 'scalar wwopmode='
            charc    = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,I1)') myline,wwop
!  START ELSE IF (TRIM(myline) .EQ. "INSERT_6B"): Write replacement block to drivers.prg
!    if myline is "INSERT_6B".
!  New Regional Model: Save workfile.
          ELSE IF (TRIM(myline) .EQ. "INSERT_6B") THEN
            myline = 'save '//TRIM(filen)//'\mcevrgmd.wf1'
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
!  START ELSE IF (TRIM(myline) .EQ. "INSERT_13C"): Write replacement block to drivers.prg
!    if myline is "INSERT_13C".
          ELSE IF (TRIM(myline) .EQ. "INSERT_13C") THEN
            myline = 'wfopen mcevrgmd'
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
            myline = 'smpl 1970'
            charc  = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,1X,I4)') myline,mamlastyr
            myline  = 'scalar macmode='
            charc   = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,I2)') myline,mmac
            myline  = 'scalar cflever='
            charc   = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,I2)') myline,cflever
!  START ELSE IF (TRIM(myline) .EQ. "INSERT_13D"): Write replacement block to drivers.prg
!    if myline is "INSERT_13D".
          ELSE IF (TRIM(myline) .EQ. "INSERT_13D") THEN
            myline = 'save '//TRIM(filen)//'\mcevrgmd.wf1'
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
!  START ELSE IF (TRIM(myline) .EQ. "INSERT_14"): Write replacement block to drivers.prg
!    if myline is "INSERT_14".
          ELSE IF (TRIM(myline) .EQ. "INSERT_14") THEN
            myline = 'include '//TRIM(fimcevsubs)
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
!  START ELSE IF (TRIM(myline) .EQ. "INSERT_15"): Write replacement block to drivers.prg
!    if myline is "INSERT_15".
          ELSE IF (TRIM(myline) .EQ. "INSERT_15") THEN
            myline = 'smpl 1990'
            charc  = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,1X,I4)') myline,mamlastyr
!  START ELSE IF (TRIM(myline) .EQ. "INSERT_16"): Write replacement block to drivers.prg
!    if myline is "INSERT_16".
          ELSE IF (TRIM(myline) .EQ. "INSERT_16") THEN
            myline = 'write(t=txt,d=c,t,id) '//TRIM(filen)//'\epmac.csv epmac rwm rwnm epio epep'
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
            myline = 'write(t=txt,d=c,t,id) '//TRIM(filen)//'\mc_xtabs.csv mc_xtabs'
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
            myline = 'save '//TRIM(filen)//'\drvdata.wf1'
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
!  START ELSE IF (TRIM(myline) .EQ. "INSERT_17"): Write replacement block to drivers.prg
!    if myline is "INSERT_17".
          ELSE IF (TRIM(myline) .EQ. "INSERT_17") THEN
            myline = 'create '//TRIM(filen)//'\mc_detail a 1990'
            charc  = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,1X,I4)') myline,mamlastyr
!  START ELSE IF (TRIM(myline) .EQ. "INSERT_18"): Write replacement block to drivers.prg
!    if myline is "INSERT_18".
          ELSE IF (TRIM(myline) .EQ. "INSERT_18") THEN
            myline = 'smpl 1990'
            charc  = LEN(TRIM(myline))
            WRITE(3,'(A<charc>,1X,I4)') myline,mamlastyr
            myline = 'write(t=txt,d=c,t,id) '//TRIM(filen)//'\mc_vehicles.csv mc_vehicles'
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
            myline = 'write(t=txt,d=c,t,id) '//TRIM(filen)//'\mc_detail.csv mc_detail gdpgap mc_detail2'
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
            myline = 'write(t=txt,d=c,t,id) '//TRIM(filen)//'\mc_commflr.csv gcommflr'
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
!  New Regional Model: Write spreadsheet with regional forecast to output directory.
            myline = 'write(t=txt,d=c,t,id) '//TRIM(filen)//'\mc_regmac.csv gregmac2'
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
            myline = 'write(t=txt,d=c,t,id) '//TRIM(filen)//'\mc_regio.csv gregio'
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
            myline = 'write(t=txt,d=c,t,id) '//TRIM(filen)//'\mc_regemp.csv gregemp'
            charc  = LEN(TRIM(myline))
            WRITE(3,1011) myline
!  START ELSE (TRIM(myline) .EQ. "INSERT_1"): Write input line from mcevcode.txt
!    to drivers.prg
          ELSE
            WRITE(3,1011) myline
!  START IF (TRIM(myline) .EQ. "INSERT_1"): Write replacement block to drivers.prg
!    if myline is "INSERT_1".
          END IF

!  END DO WHILE: Continue loop reading from input file until end of file marker
!    is returned.
        END DO

!  Close input file of EView commands, mcevcode.txt, and program file drivers.prg.
        inunit=FILE_MGR('C',name,new)
        CLOSE(3)

1011    FORMAT(A<charc>)

! before executing EViews, check if there are numbers that would cause a problem
        problem = .FALSE.
        DO j=yearpr-1989,lastyr
          DO i=1,scennum
            IF (ISNAN(dlink(i,j))) THEN
               problem = .TRUE.
               WRITE(6,'(" Encountered NaN in dlink,row=",I4," Year=",I4,5X,A25)') i,j+1989,dlvars(i)
            END IF
            IF (dlink(i,j) .EQ. 0.0 .AND. dlink(i,j-1) .GT. 1.0) THEN
               WRITE(6,'(" Encountered 0 in dlink,row=",I4," Year=",I4,5X,A25)') i,j+1989,dlvars(i)
            END IF
            IF (dlink(i,j) .LT. 0.0 .AND. dlink(i,j-1) .GT. 1.0) THEN
               WRITE(6,'(" Encountered negative in dlink,row=",I4," Year=",I4,5X,A25)') i,j+1989,dlvars(i)
            END IF
          END DO
          DO i=1,NEMSENERGYNUM
            IF (ISNAN(nlink(i,j))) THEN
               problem = .TRUE.
               WRITE(6,'(" Encountered NaN in nlink,row=",I4," Year=",I4,5X,A100)') i,j+1989,nemsenergylabel(i)
            END IF
            IF (nlink(i,j) .EQ. 0.0 .AND. nlink(i,j-1) .GT. 1.0) then
               WRITE(6,'(" Encountered 0 in nlink,row=",I4," Year=",I4,5X,A100)') i,j+1989,nemsenergylabel(i)
            END IF
            IF (nlink(i,j) .LT. 0.0 .AND. nlink(i,j-1) .GT. 1.0) then
               WRITE(6,'(" Encountered negative in nlink,row=",I4," Year=",I4,5X,A100)') i,j+1989,nemsenergylabel(i)
            END IF
          END DO
        END DO
        IF (problem) STOP 14

        CALL MPTIM3(CPU_DUMMY_TIME,WALL_EV_START)
!  Execute EViews with constructed program file of commands.
        IF (LEN_TRIM(eviewscommand).GT.0) THEN
          cmdline=TRIM(eviewscommand)
          args=TRIM(filen)//'\drivers.prg'
        ELSE
          cmdline='cmd'
          args='/c '//trim(filen)//'\drivers.prg'
        END IF

        lstatus=.false.
!  OSCall is in main.f.  It creates a child process with a minimized window. 
!  "cmdline" is the program name; "args" contains the program arguments--in this case, the "prg" file.
!  "iwaitMS" is set to -1 to indicate indefinite wait.  
        CALL OSCall(iWaitMS,cmdline,args,iRet)
        IF (iret.EQ.1) lstatus=.true.


!  START IF (.not. lstatus): Trap if call on EViews command fails to execute.
        MC_EV_SUCCESS = 0      ! old way;  0 is pass
        IF (.NOT. lstatus) THEN 
          MC_EV_SUCCESS = 1
          IF (CONTINM .EQ. 1) THEN
             CONTINM=0            !  set to FAIL
             REASONM="EViews time-out"
          ENDIF
          IF (iret.lt.0) THEN
            WRITE (*,*) "MAC.F: WARNING! EVIEWS COMMAND FAILED TO EXECUTE."
          ELSE
            WRITE (*,*) "MAC.F: WARNING! WAIT FOR EVIEWS FAILED TO OCCUR."
          END IF
!  END IF (.not. lstatus): Trap if call on EViews command fails to execute.
        END IF
        CALL MPTIM3(CPU_DUMMY_TIME,WALL_EV_END)
        ELAPSED_EV_TIME=(FLOAT(WALL_EV_END)-FLOAT(WALL_EV_START))/100.
!  check to see if EViews came back quicker than expected.  if so, possible license denial
!  check file for the day at
!  c:/program files (x86)/eviews license manager/log files/evlm_yyyymmdd.txt
        IF (ELAPSED_EV_TIME .LT. 10.0) THEN
           write(6,'(a,F6.1)') "Command failure?  Possible license rejection, EViews returned suspiciously quickly:  ", ELAPSED_EV_TIME
        ENDIF

!  Open EPMAC.CSV for reading model solutions.
        OPEN(UNIT=2,ACTION='READ',FILE=TRIM(filen)//'\EPMAC.CSV')
!  Read year column labels.
        READ(2,*) label
!  Read model solutions into myinputs matrix.
        DO i = 1,numepmac
          READ(2,'(a)') line
          DO WHILE (index(line,',NA,').gt.0)
            is=index(line,',NA,')
            line=line(1:is)//line(is+3:)
          END DO
          READ(line,*) label,(myinputs(i,j),j=1,mamlastyr-1990+1)
        END DO
!  Close EPMAC.CSV for reading model solutions.
        CLOSE(2)

!  Open MC_XTABS.CSV for reading model solutions.
        OPEN(UNIT=2,ACTION='READ',FILE=TRIM(filen)//'\MC_XTABS.CSV')
!  Read year column labels.
        READ(2,*) label
!  Read model solutions into mcxtabs matrix.
        DO i = 1,numxtabs
          READ(2,'(a)') line
          DO WHILE (index(line,',NA,').gt.0)
            is=index(line,',NA,')
            line=line(1:is)//line(is+3:)
          END DO
          READ(line,*) label,(mcxtabs(i,j),j=1,mamlastyr-1990+1)
        END DO
!  Close MC_XTABS.CSV for reading model solutions.
        CLOSE(2)

!  Open MC_DETAIL.CSV for read of energy detail.
        OPEN(UNIT=2,ACTION='READ',FILE=TRIM(filen)//'\MC_DETAIL.CSV')
!  Read year column labels.
        READ(2,*) label
!  Read model detail into mc_detail matrix.
        DO i = 1,100
          READ(2,'(a)') line
          DO WHILE (index(line,',NA,').gt.0)
            is=index(line,',NA,')
            line=line(1:is)//line(is+3:)
          END DO
          READ(line,*) label,(mc_detail(i,j),j=1,mamlastyr-1990+1)
        END DO
!  Close MC_DETAIL.CSV for reading energy detail.
        CLOSE(2)

!  Open MC_VEHICLES.CSV for read vehicle solutions.
        OPEN(UNIT=2,ACTION='READ',FILE=TRIM(filen)//'\MC_VEHICLES.CSV')
!  Read year column labels.
        READ(2,*) label
!  Read model solution into mc_vehicles matrix.
        DO i = 1,4
          READ(2,'(a)') line
          DO WHILE (index(line,',NA,').gt.0)
            is=index(line,',NA,')
            line=line(1:is)//line(is+3:)
          END DO
          READ(line,*) label,(mc_vehicles(i,j),j=1,mamlastyr-1990+1)
        END DO
!  Close MC_VEHICLES.CSV for reading vehicle solutions.
        CLOSE(2)

!  Open MC_COMMFLR.CSV for read commercial floorspace solution.
        OPEN(UNIT=2,ACTION='READ',FILE=TRIM(filen)//'\MC_COMMFLR.CSV')
!  Read year column labels.
        READ(2,*) label
!  Read model solution into mc_commflr matrix.
        DO i = 1,81
          READ(2,'(a)') line
          DO WHILE (index(line,',NA,').gt.0)
            is=index(line,',NA,')
            line=line(1:is)//line(is+3:)
          END DO
          READ(line,*) label,(mc_commflr(i,j),j=1,mamlastyr-1990+1)
        END DO
!  Close MC_COMMFLR.CSV for read commercial floorspace solution.
        CLOSE(2)

!  Open MC_REGMAC.CSV for read regional macro forecast.
        OPEN(UNIT=2,ACTION='READ',FILE=TRIM(filen)//'\MC_REGMAC.CSV')
!  Read year column labels.
        READ(2,*) label
!  Read model solution into mc_regmac matrix.
        DO i = 1,260
          READ(2,'(a)') line
          DO WHILE (index(line,',NA,').gt.0)
            is=index(line,',NA,')
            line=line(1:is)//line(is+3:)
          END DO
          READ(line,*) label,(mc_regmac(i,j),j=1,mamlastyr-1990+1)
        END DO
!  Close MC_REGMAC.CSV for read regional macro forecast.
        CLOSE(2)

!  Open MC_REGIO.CSV for read regional IO forecast.
        OPEN(UNIT=2,ACTION='READ',FILE=TRIM(filen)//'\MC_REGIO.CSV')
!  Read year column labels.
        READ(2,*) label
!  Read model solution into mc_regio matrix.
        DO i = 1,432
          READ(2,'(a)') line
          DO WHILE (index(line,',NA,').gt.0)
            is=index(line,',NA,')
            line=line(1:is)//line(is+3:)
          END DO
          READ(line,*) label,(mc_regio(i,j),j=1,mamlastyr-1990+1)
        END DO
!  Close MC_REGIO.CSV for read regional IO forecast.
        CLOSE(2)

!  Open MC_REGEMP.CSV for read regional employment forecast
        OPEN(UNIT=2,ACTION='READ',FILE=TRIM(filen)//'\MC_REGEMP.CSV')
!  Read year column labels.
        READ(2,*) label
!  Read model solution into mc_regemp matrix.
        DO i = 1,351
          READ(2,'(a)') line
          DO WHILE (index(line,',NA,').gt.0)
            is=index(line,',NA,')
            line=line(1:is)//line(is+3:)
          END DO
          READ(line,*) label,(mc_regemp(i,j),j=1,mamlastyr-1990+1)
        END DO
!  Close MC_REGEMP.CSV for read regional employment forecast.
        CLOSE(2)

!  End of subroutine DRTLINK. Return to calling subroutine MAC.
       RETURN
       END


!******************************************************************
       SUBROUTINE INDUSTSUB
!******************************************************************
!  The INDUSTSUB subroutine copies the industrial output and services
!   forecast to the esind and esserv matrices from the epmac matrix.
!   The forecast for five energy related industries is supplied by
!   NEMS. These are Petroleum Refineries (NAICS 32411), Coal Mining
!   (NAICS 2121), Oil and Gas Extraction and Support Activities
!   (NAICS 211, 213), Electric Power Generation and Distribution
!   (NAICS 2211) and Natural Gas Distribution (NAICS 2212).  The
!   subroutine then computes some aggregates. The forecast comes
!   from the GI's industrial model.
!******************************************************************

       IMPLICIT NONE

!  Include parameter files.
       INCLUDE 'parametr.'
       INCLUDE 'ncntrl.'
       INCLUDE 'macparm.'
       INCLUDE 'mcinter2.'

       INTEGER i

!  START DO i: Loop through the number of industrial outputs and initialize
!    arrays containing forecasts for non-industrial/services (ESSERV) and industrial (ESIND) outputs.
!    Seven is added to number of industrial output variables for aggregates.
       DO i = 1,mcnmind + 7
!  START IF (i .LE. mcnmserv+1): There are fewer categories of services as
!    compared to industrial output. This is used to initialize the ESSERV matrix.
         IF (i .LE. mcnmserv+1) THEN
           esserv(i,curiyr) = 0.0
!  END IF (i .LE. mcnmserv+1): There are fewer categories of services as
!    compared to industrial output. This is used to initialize the ESSERV matrix.
         END IF
         esind(11,i,curiyr) = 0.0
!  END DO i: Loop through the number of industrial outputs and initialize
!    arrays containing forecasts for service (ESSERV) and industrial (ESIND) outputs.
!    Seven is added to number of industrial output variables for aggregates.
       END DO

!  START DO i: Loop through number of industrial and service outputs copying forecast
!    to the esind and esserv matrices from the epmac matrix.

       DO i = 1,mcnmind+mcnmserv

!  START IF for five industries covered in NEMS: Petroleum Refineries (NAICS 32411),
!    Coal Mining (NAICS 2121), Oil and Gas Extraction and Support Activities (NAICS 211, 213),
!    Electric Power Generation and Distribution (NAICS 2211) and Natural Gas Distribution (NAICS 2212).
         IF ((i .EQ. 25 .OR. i .EQ. 45 .OR. i .EQ. 46) &
           .AND. ((baseyr+curiyr-1) .GE. 2021) .AND. (macfdbk .EQ. 1)) THEN
!  START IF for NEMS Industrial Output 25: Petroleum Refining (NAICS 32411), manufacturing.
           IF (i .EQ. 25)  THEN
             esind(11,i,curiyr) = epmac(i+89,curiyr)
!  START ELSE IF for NEMS Industrial Output 45: Coal Mining (NAICS 2121), non-manufacturing.
           ELSE IF (i .EQ. 45) THEN
             esind(11,i,curiyr) = epmac(i+89,curiyr)
!  START ELSE IF for NEMS Industrial Output 46: Oil and Gas Extraction and Support Activities (NAICS 211, 213), non-manufacturing.
           ELSE IF (i .EQ. 46) THEN
             esind(11,i,curiyr) = epmac(i+89,curiyr)
!  END IF for NEMS Industrial Output 25, 45 and 46.
           END IF
!  START ELSE IF for NEMS Industrial Output for Electric Power Generation and Distribution (NAICS 2211)
!    and Natural Gas Distribution (NAICS 2212).
         ELSE IF ((i .EQ. 51 .OR. i .EQ. 52) &
           .AND. ((baseyr+curiyr-1) .GE. 2021) .AND. (macfdbk .EQ. 1)) THEN
!  START IF for NEMS Industrial Output 51: Electric Power Generation and Distribution (NAICS 2211), non-industrial/services.
           IF (i .EQ. 51) THEN
             esserv(i-mcnmind,curiyr) = epmac(i+89,curiyr)
!  START ELSE IF for NEMS Industrial Output 52: Natural Gas Distribution (NAICS 2212), non-industrial/services.
           ELSE IF (i .EQ. 52) THEN
             esserv(i-mcnmind,curiyr) = epmac(i+89,curiyr)
!  END IF for NEMS Industrial Output 51: Electric Power Generation and Distribution (NAICS 2211), non-industrial/services.
           END IF
!  START ELSE for all other industries not covered in NEMS.
         ELSE

!  START IF (i .LE. mcnmind): Copy industrial output forecast for 48 industries.
           IF (i .LE. mcnmind) THEN
             esind(11,i,curiyr) = epmac(i+89,curiyr)

!  START ELSE: Copy service output forecast for 10 industries.
           ELSE
             esserv(i-mcnmind,curiyr)=epmac(i+89,curiyr)
!  END IF (i .LE. mcnmind): Copy industrial output forecast for 48 industries.
           END IF

!  END IF for five industries covered in NEMS: Petroleum Refineries (NAICS 32411),
!    Coal Mining (NAICS 2121), Oil and Gas Extraction and Support Activities (NAICS 211, 213),
!    Electric Power Generation and Distribution (NAICS 2211) and Natural Gas Distribution (NAICS 2212).
         END IF

!  START IF (i .LE. mcnummnf): Aggregate solution values for first
!    41 industrial outputs for total manufacturing output.
         IF (i .LE. mcnummnf) THEN
           esind(11,mcnmind+5,curiyr) = esind(11,mcnmind+5,curiyr) &
                                      + esind(11,i,curiyr)

!  END IF (i .LE. mcnummnf): Aggregate solution values for first
!    41 industrial outputs for total manufacturing output.
         END IF

!  START IF (i .GT. mcnmind .AND. i .LE. mcnmind+mcnmserv): Aggregate solution
!    values for ten non-industrial/service outputs for total services output.
         IF (i .GT. mcnmind .AND. i .LE. mcnmind+mcnmserv) THEN
           esserv(mcnmserv+1,curiyr) = esserv(mcnmserv+1,curiyr) &
                                     + esserv(i-mcnmind,curiyr)

!  END IF (i .GT. mcnmind .AND. i .LE. mcnmind+mcnmserv): Aggregate solution
!    values for ten service outputs for total services output.
         END IF

!  START IF (i .LE. mcnmind): Aggregate solution values for the 48 
!   industrial categories for total industrial output.
         IF (i .LE. mcnmind) THEN
           esind(11,mcnmind+6,curiyr) = esind(11,mcnmind+6,curiyr) &
                                      + esind(11,i,curiyr)

!  END IF (i .LE. mcnmind): Aggregate solution values for the 48 
!   industrial categories for total industrial output.
         END IF

!  END DO i: Loop through number of industrial and non-industrial/service outputs copying forecast
!    to the esind and esserv matrices from the epmac matrix.
       END DO

!  Adjustment to total manufacturing output computed above. Revind 2 through 5, 11 through 
!  13, 17, 21 through 24, and 29 are components of totals revind 1, 10, 18, 20, and 28 respectively.
       esind(11,mcnmind+5,curiyr) = esind(11,mcnmind+5,curiyr) &
                                  - esind(11,2,curiyr)  - esind(11,3,curiyr)  &
                                  - esind(11,4,curiyr)  - esind(11,5,curiyr)  &
                                  - esind(11,11,curiyr) - esind(11,12,curiyr) &
                                  - esind(11,13,curiyr) - esind(11,17,curiyr) &
                                  - esind(11,21,curiyr) - esind(11,22,curiyr) &
                                  - esind(11,23,curiyr) - esind(11,24,curiyr) &
                                  - esind(11,29,curiyr)


!  Adjustment to total industrial output computed above. Revind 2 through 5, 11 through 
!  13, 17, 21 through 24, and 29 are components of totals revind 1, 10, 18, 20, and 28 respectively.
       esind(11,mcnmind+6,curiyr) = esind(11,mcnmind+6,curiyr) &
                                  - esind(11,2,curiyr)  - esind(11,3,curiyr)  &
                                  - esind(11,4,curiyr)  - esind(11,5,curiyr)  &
                                  - esind(11,11,curiyr) - esind(11,12,curiyr) &
                                  - esind(11,13,curiyr) - esind(11,17,curiyr) &
                                  - esind(11,21,curiyr) - esind(11,22,curiyr) &
                                  - esind(11,23,curiyr) - esind(11,24,curiyr) &
                                  - esind(11,29,curiyr)

!  Sum total industrial output and total non-industrial/services output for solution
!   total gross output.
       esind(11,mcnmind+7,curiyr) = esind(11,mcnmind+6,curiyr) &
                                  + esserv(mcnmserv+1,curiyr)

!  START DO i: Sum industrial categories 15, 16, 18, and 19: Basic Organic Chemicals (NAICS 32511,32519),
!   Basic Inorganic Chemicals (NAICS 32512-32518), Plastic and Synthetic Rubber Materials (NAICS 3252),
!   Agricultural Chemicals (NAICS 3253) and Other Chemical Products (NAICS 3254-3259), skip Ethanol (325193),
!   for total chemicals solution, ESIND position immediately following 48 industries.

       DO i = 1,6
         IF (i .EQ. 3) THEN
           CYCLE
         END IF
         esind(11,mcnmind+1,curiyr) = esind(11,mcnmind+1,curiyr) &
                                    + esind(11,14+i,curiyr)

!  END DO i: Sum industrial categories 15, 16, 18, and 19: Basic Organic Chemicals (NAICS 32511,32519),
!   Basic Inorganic Chemicals (NAICS 32512-32518), Plastic and Synthetic Rubber Materials (NAICS 3252),
!   Agricultural Chemicals (NAICS 3253) and Other Chemical Products (NAICS 3254-3259), skip Ethanol (325193),
!   for total chemicals solution, ESIND position immediately following 48 industries.
       END DO

!  Sum industry categories 25 and 26: Petroleum Refineries (NAICS 32411) and Other Petroleum and 
!   Coal Products (NAICS 32412, 32419) for solution petroleum and coal solution, ESIND second position following 48 industries.
       esind(11,mcnmind+2,curiyr) = esind(11,25,curiyr) &
                                  + esind(11,26,curiyr)

!  Sum industry categories 28, 30, 31, and 32: Glass and Glass Products (NAICS 3272), Cement Manufacturing
!   (NAICS 32731), Lime (3274),  and Other Nonmetallic Mineral Products (NAICS 327 less 3272, 32731, & 3274), 
!   skip Flat Glass (327211), for stone, cement and glass solution, ESIND third position following 48 industries.
       esind(11,mcnmind+3,curiyr) = esind(11,28,curiyr) &
                                  + esind(11,30,curiyr) &
                                  + esind(11,31,curiyr) &
                                  + esind(11,32,curiyr)

!  Sum industry categories 33 through 35: Iron and Steel Mills, Ferroalloy and Steel Products (NAICS 3311,3312),
!   Alumina and Aluminum Products (NAICS 3313), and Other Primary Metals (NAICS 3314, 3315) for primary
!   metals solution, ESIND fourth position following 48 industries.
       esind(11,mcnmind+4,curiyr) = esind(11,33,curiyr) &
                                  + esind(11,34,curiyr) &
                                  + esind(11,35,curiyr)

!  End of subroutine INDUSTSUB. Return to calling subroutine MAC.
       RETURN
       END


!******************************************************************
      SUBROUTINE REGIONSUB
!******************************************************************
!  The REGIONSUB subroutine copies regional forecasts done using
!   EIA's regional models within EViews into local arrays.  These
!   include regional commercial floorspace, macroeconomic and
!   industrial.
!******************************************************************

       IMPLICIT NONE

!  Include parameter files.
       INCLUDE 'parametr.'
       INCLUDE 'ncntrl.'
       INCLUDE 'macparm.'
       INCLUDE 'macout.'
       INCLUDE 'mcinter2.'

!  Declare local variables.
       INTEGER i,j,r

!  START DO Loop r: Loop through number of Census regions less California.
       DO r = 1,mcnumregs-2

!  START DO LOOP j: Initialize portion of forecast matrix esind containing the following aggregates:
!    chemicals and allied products, first position following the industries; petroleum and coal products,
!    second position following the industries; stone, clay, and glass products, third position following the
!    industries, and primary metals industries, fourth position following the industries. In addition there
!    are two aggregates at the regional level: total manufacturing output and total industrial output.
         DO j = 1,6
           esind(r,mcnmind+j,curiyr) = 0.0
!  END DO LOOP j: Initialize portion of forecast matrix esind containing the following aggregates:
!    chemicals and allied products, first position following the industries; petroleum and coal products,
!    second position following the industries; stone, clay, and glass products, third position following the
!    industries, and primary metals industries, fourth position following the industries. In addition there
!    are two aggregates at the regional level: total manufacturing output and total industrial output.
         END DO

!  START DO LOOP i: Loop through number regional macro variables and number of industrial
!    outputs. Aggregations are also done.
         DO i = 1,mcnmmacreg+mcnmind

!  START IF (i .LE. mcnmmacreg): Compute regional macro forecast by
!    variable for each of the Census Division regions excluding California
!    and National.
           IF (i .LE. mcnmmacreg) THEN
             IF (i .LE. mcnmnatreg) THEN
               esmacreg(r,i,curiyr) = mc_regmac((mcnmnatreg*(r-1)+i),curiyr)
             END IF

!  START ELSE: Copy regional industrial output.
           ELSE
             j = i - mcnmmacreg
             esind(r,j,curiyr) = mc_regio((mcnmind*(r-1)+j),curiyr)

!  START IF (j .LE. mcnummnf): Aggregate industrial outputs representing
!    manufacturing as total manufacturing output.
             IF (j .LE. mcnummnf) THEN
               esind(r,mcnmind+5,curiyr) = esind(r,mcnmind+5,curiyr) &
                                         + esind(r,j,curiyr)
!  END IF (j .LE. mcnummnf): Aggregate industrial outputs representing
!    manufacturing as total manufacturing output.
             END IF

!  START IF (j .GE. 15 .AND. j .LE. 20): Aggregate industrial outputs 
!    Basic Organic Chemicals (32511,32519), Basic Inorganic Chemicals (32512-32518),
!    Plastic and Synthetic Rubber Materials (3252), Agricultural Chemicals (3253), and
!    Other Chemical Products (3254-3259), skip Ethanol (325193).
             IF (j .GE. 15 .AND. j .LE. 20 .AND. j .NE. 17) THEN
               esind(r,mcnmind+1,curiyr) = esind(r,mcnmind+1,curiyr) &
                                         + esind(r,j,curiyr)

!  START ELSE IF (j .GE. 25 .AND. j .LE. 26): Aggregate industrial outputs
!    Petroleum Refineries (32411) and Other Petroleum and Coal Products (32412, 32419).
             ELSE IF (j .GE. 25 .AND. j .LE. 26) THEN
               esind(r,mcnmind+2,curiyr) = esind(r,mcnmind+2,curiyr) &
                                         + esind(r,j,curiyr)

!  START ELSE IF (j .GE. 28 .AND. j .LE. 32): Aggregate industrial outputs
!    Glass and Glass Products (3272), Cement Manufacturing (32731), Lime (3274), and
!    Other Nonmetallic Mineral Products (327 less 3272, 32731, & 3274), skip Flat Glass (327211).
             ELSE IF (j .GE. 28 .AND. j .LE. 32 .AND. j .NE. 29) THEN
               esind(r,mcnmind+3,curiyr) = esind(r,mcnmind+3,curiyr) &
                                         + esind(r,j,curiyr)

!  START ELSE IF (j .GE. 33 .AND. j .LE. 35): Aggregate industrial outputs
!    Iron and Steel Mills, Ferroalloy and Steel Products (3311, 3312), Alumina and Aluminum
!    Products (3313), and Other Primary Metals (3314, 3315).
             ELSE IF (j .GE. 33 .AND. j .LE. 35) THEN
               esind(r,mcnmind+4,curiyr) = esind(r,mcnmind+4,curiyr) &
                                         + esind(r,j,curiyr)
!  END IF (j .GE. 15 .AND. j .LE. 20): Aggregate industrial outputs 
!    Basic Organic Chemicals (32511,32519), Basic Inorganic Chemicals (32512-32518),
!    Plastic and Synthetic Rubber Materials (3252), Agricultural Chemicals (3253), and
!    Other Chemical Products (3254-3259), skip Ethanol (325193).
             END IF

!  Aggregate all industrial outputs as total industrial output. Note that
!   total gross output is not computed at the regional level.
!   This is the sum of manufacturing, non-manufacturing, and services/non-industrial output
!   and is available only at the national level.
             esind(r,mcnmind+6,curiyr)   = esind(r,mcnmind+6,curiyr) &
                                         + esind(r,j,curiyr)

!  END IF (i .LE. mcnmmacreg): Compute regional forecast by variable for each
!    of the Census Division regions excluding California and National.
           END IF

!  END DO LOOP i: Loop through number regional macro variables and number of industrial
!    outputs. Aggregations are also done.
         END DO

!  Adjustment to total manufacturing output for region r computed above. Revind 2 through
!  5, 11 through 13, 17, 21 through 24, 29, and 31 are components of totals revind 1, 10,
!  18, 20, 28, and 30 respectively.

         esind(r,mcnmind+5,curiyr) = esind(r,mcnmind+5,curiyr)               &
                                   - esind(r,2, curiyr) - esind(r,3, curiyr) &
                                   - esind(r,4, curiyr) - esind(r,5, curiyr) &
                                   - esind(r,11,curiyr) - esind(r,12,curiyr) &
                                   - esind(r,13,curiyr) - esind(r,17,curiyr) &
                                   - esind(r,21,curiyr) - esind(r,22,curiyr) &
                                   - esind(r,23,curiyr) - esind(r,24,curiyr) &
                                   - esind(r,29,curiyr) - esind(r,31,curiyr)

!  Adjustment to total industrial output for region r computed above. Revind 2 through
!  5, 11 through 13, 17, 21 through 24, 29, and 31 are components of totals revind 1, 10,
!  18, 20, 28, and 30 respectively.

         esind(r,mcnmind+6,curiyr) = esind(r,mcnmind+6,curiyr)               &
                                   - esind(r,2, curiyr) - esind(r,3, curiyr) &
                                   - esind(r,4, curiyr) - esind(r,5, curiyr) &
                                   - esind(r,11,curiyr) - esind(r,12,curiyr) &
                                   - esind(r,13,curiyr) - esind(r,17,curiyr) &
                                   - esind(r,21,curiyr) - esind(r,22,curiyr) &
                                   - esind(r,23,curiyr) - esind(r,24,curiyr) &
                                   - esind(r,29,curiyr) - esind(r,31,curiyr)

!  END DO Loop r: Loop through number of Census regions less California.
       END DO

!  End of subroutine REGIONSUB. Return to calling subroutine MAC.
      RETURN
      END


!******************************************************************
      SUBROUTINE EMPLOYMENT
!******************************************************************
!  Employment subroutine copies the employment forecast to the ESEMP
!   matrix from the EPMAC matrix. This subroutine also computes some
!   aggregates.
!******************************************************************

      IMPLICIT NONE

!  Include parameter files.
      INCLUDE 'parametr'
      INCLUDE 'ncntrl'
      INCLUDE 'macparm'
      INCLUDE 'mcinter2'

!  Declare local variables.
       INTEGER i


!  This is the sector employment forecast. Five of the
!   energy-related industries are forecasted by NEMS.
! Food Products,E20 NAICS 311 (Millions of Persons)
      esemp(11,1,curiyr) = epmac(148,curiyr)
! Beverage and Tobacco Manufactures,E21 NAICS 312 (Millions of Persons)
      esemp(11,2,curiyr) = epmac(149,curiyr)
! Textiles, Apparel and Leather NAICS 313-316 (Millions of Persons)
      esemp(11,3,curiyr) = epmac(150,curiyr)
! Wood Products,E24 NAICS 321 (Millions of Persons)
      esemp(11,4,curiyr) = epmac(151,curiyr)
! Furniture  and Related,E25 NAICS 337 (Millions of Persons)
      esemp(11,5,curiyr) = epmac(152,curiyr)
! Paper  Products,E26 NAICS 322 (Millions of Persons)
      esemp(11,6,curiyr) = epmac(153,curiyr)
! Printing  Publishing,E27 NAICS 323 (Millions of Persons)
      esemp(11,7,curiyr) = epmac(154,curiyr)
! Bulk Chemicals, E281-7 NAICS 3251-3 (Millions of Persons)
      esemp(11,8,curiyr) = epmac(155,curiyr)
! Other Chemicals, E28 nec NAICS 3254-9 (Millions of Persons)
      esemp(11,9,curiyr) = epmac(156,curiyr)
! Petroleum and Coal Products, E29 NAIICS 324 (Millions of Persons)
      esemp(11,10,curiyr) = epmac(157,curiyr)
! Rubber  and Plastic Products, E30 NAICS 326 (Millions of Persons)
      esemp(11,11,curiyr) = epmac(158,curiyr)
! Nonmetallic Minerals, E32  NAICS 327 (Millions of Persons)
      esemp(11,12,curiyr) = epmac(159,curiyr)
! Primary Metals, E33 NAICS 331 (Millions of Persons)
      esemp(11,13,curiyr) = epmac(160,curiyr)
! Fabricated Metal Products, E34 NAICS 332 (Millions of Persons)
      esemp(11,14,curiyr) = epmac(161,curiyr)
! Machinery, E35 NAICS 333 (Millions of Persons)
      esemp(11,15,curiyr) = epmac(162,curiyr)
! Computers and Electronic Equipment, E36 NAICS 334 (Millions of Persons)
      esemp(11,16,curiyr) = epmac(163,curiyr)
! Transportation Equipment, E37 NAICS 336 (Millions of Persons)
      esemp(11,17,curiyr) = epmac(164,curiyr)
! Appliance and Electrical Equipment, E38 NAICS 335 (Millions of Persons)
      esemp(11,18,curiyr) = epmac(165,curiyr)
! Miscellaneous Manufacturing Industries, E39 NAICS 339 (Millions of Persons)
      esemp(11,19,curiyr) = epmac(166,curiyr)
! Agricultural Production, Crops; E01 NAICS 111 (Millions of Persons)
      esemp(11,20,curiyr) = epmac(167,curiyr)
! Other Agriculture, E07-09 NAICS 112-5 (Millions of Persons)
      esemp(11,21,curiyr) = epmac(168,curiyr)
! Coal Mining and Oil and Gas Mining employment from NEMS
! Coal Mining, E11,12 NAICS 2121 (Millions of Persons)
      esemp(11,22,curiyr) = epmac(169,curiyr)
! Oil  Gas Mining, E13 NAICS 211, 213 (Millions of Persons)
      esemp(11,23,curiyr) = epmac(170,curiyr)
! Metal  Other Non-metallic Mining, E10,14 NAICS 2122-3 (Millions of Persons)
      esemp(11,24,curiyr) = epmac(171,curiyr)
! Construction of Buildings, E15 NAICS 236 (Millions of Persons)
      esemp(11,25,curiyr) = epmac(172,curiyr)
! Heavy and Civ. Eng. Construction, E16 NAICS 237 (Millions of Persons)
      esemp(11,26,curiyr) = epmac(173,curiyr)
! Specialty Trade Contractors, E17 NAICS 238 (Millions of Persons)
      esemp(11,27,curiyr) = epmac(174,curiyr)
! Electric Utilities; E491,part 493 NAICS 2211 (Millions of Persons)
! empser1
      esemp(11,28,curiyr) = epmac(175,curiyr)
! Natural Gas Utilities; E492,part 493 NAICS 2212 (Millions of Persons)
! empser2
      esemp(11,29,curiyr) = epmac(176,curiyr)
! Water  Sewer Services; E494-497,part 493 NAICS 2213 (Millions of Persons)
! empser3
      esemp(11,30,curiyr) = epmac(177,curiyr)
! Wholesale Trade; E50,51 NAICS 42 (Millions of Persons)
! empser4
      esemp(11,31,curiyr) = epmac(178,curiyr)
! Retail Trade; E52-57,59,739 NAICS 44-5 (Millions of Persons)
! empser5
      esemp(11,32,curiyr) = epmac(179,curiyr)
! Transportation Services, E40-47 NAICS 48-9 (Millions of Persons)
! empser6
      esemp(11,33,curiyr) = epmac(180,curiyr)
! Publishing, E48x NAICS 511 (Millions of Persons)
! empser 7
      esemp(11,34,curiyr) = epmac(181,curiyr)
! Broadcasting, E48x NAICS 515 (Millions of Persons)
! empser 8
      esemp(11,35,curiyr) = epmac(182,curiyr)
! Telecommunications, E48x NAICS 517 (Millions of Persons)
! empser 9
      esemp(11,36,curiyr) = epmac(183,curiyr)
! Finance and Insurance; E60-63 NAICS 52 (Millions of Persons)
! empser 10
      esemp(11,37,curiyr) = epmac(184,curiyr)
! Real Estate and Retal/Leasing; E65-66,153 NAICS 53 (Millions of Persons)
! empser 11
      esemp(11,38,curiyr) = epmac(185,curiyr)
! All Other Services, Retail; E58,70,73,75-91 NAICS 54-81, 92 (Millions of Persons) and
! empser12
      esemp(11,39,curiyr) = epmac(186,curiyr)


!  START DO LOOP i: Initialize aggregates holding totals for solution and base.
      DO i = NUMEMPL+1,NUMEMPL+4
        esemp(11,i,curiyr) = 0
!  END DO LOOP i: Initialize aggregates holding totals for solution and base.
      END DO

!  START DO LOOP i: Loop through number of manufacturing industries for total 
!    of solution and base; total employment categories minus 12 serv & 7 non-manufacturing.
      DO i = 1,NUMEMPL-20
        esemp(11,NUMEMPL+1,curiyr)=esemp(11,NUMEMPL+1,curiyr)+esemp(11,i,curiyr)
!  END DO LOOP i: Loop through number of manufacturing industries for total 
!    of solution and base.
      END DO

!  START DO LOOP i: Loop through number of non-manufacturing industries for total
!    including agriculture of solution and base.
      DO i = NUMEMPL-19,NUMEMPL-12
        esemp(11,NUMEMPL+2,curiyr)=esemp(11,NUMEMPL+2,curiyr)+esemp(11,i,curiyr)
!  END DO LOOP i: Loop through number of non-manufacturing industries for total
!    including agriculture of solution and base.
      END DO

!  START DO LOOP i: Loop through number of service industries for total
!    including government of solution and base.
      DO i = NUMEMPL-11,numempl
        esemp(11,NUMEMPL+3,curiyr)=esemp(11,NUMEMPL+3,curiyr)+esemp(11,i,curiyr)
!  END DO LOOP i: Loop through number of service industries for total
!    including government of solution and base.
      END DO

!  START DO LOOP i: Loop through number of non-agriculture industries for total
!    of manufacturing, non-manufacturing and services of solution and base.
      DO i = 1,numempl
!  START IF (i .NE. 20 .AND. i .NE. 21): Exclude Agricultural Production, Crops; E01 NAICS 111
!    and Other Agriculture, E07-09 NAICS 112-5 from total.
        IF (i .NE. 20 .AND. i .NE. 21) THEN
          esemp(11,NUMEMPL+4,curiyr) = esemp(11,NUMEMPL+4,curiyr) + esemp(11,i,curiyr)
!  END IF (i .NE. 20 .AND. i .NE. 21): Exclude Agricultural Production, Crops; E01 NAICS 111
!    and Other Agriculture, E07-09 NAICS 112-5 from total.
        END IF

!  START DO LOOP i: Loop through number of non-agriculture industries for total
!    of manufacturing, non-manufacturing and services of solution and base.
      END DO


!  End of subroutine EMPLOYMENT. Return to calling subroutine MAC.
      RETURN
      END


!***********************************************************************
       SUBROUTINE COMFLR
       USE DFLIB
!***********************************************************************
!  A commercial floorspace model by floor type within Census Division
!   region. There is a separate, estimated equation for each.  Aggregation
!   is also done by floor type across Census Division regions and within
!   Census Division region.  The model is quarterly.
!***********************************************************************

       IMPLICIT NONE

!  Include parameter files.
       INCLUDE'parametr'
       INCLUDE'macout'
       INCLUDE'ncntrl'
       INCLUDE'macparm.'
       INCLUDE'mcinter2.'

!  Local variable definition.
       INTEGER       j,k


!  START DO LOOP j: Loop through Census Division Regions including Pacific but not National.
       DO j=1,9
       
!  START DO LOOP k: Loop through k floorspace types.
         DO k=15,23

           esmacreg(j,k,curiyr) = mc_commflr((j-1)*9 +(k-14),curiyr)

!  END DO LOOP k: Loop through k floorspace types.
         END DO

!  END DO LOOP j: Loop through Census Division Regions including Pacific but not National.
       END DO

!  End of subroutine COMFLR. Return to calling subroutine MAC.
       RETURN
       END


!***********************************************************************
       SUBROUTINE TRANC
       USE DFLIB
!***********************************************************************
!  A transportation size class model.
!***********************************************************************

       IMPLICIT NONE

!  Include parameter files.
       INCLUDE'parametr'
       INCLUDE'macout'
       INCLUDE'ncntrl'
       INCLUDE'macparm.'
       INCLUDE'mcinter2.'

!  Local variable definition.
       INTEGER  i
       REAL     temp


          IF (macfdbk.EQ. 1) THEN 
            DO i = 1,ijumpyr
              temp = mc_vehicles(4,i)
              mc_vehicles(11,i) = temp
              mc_vehicles(4,i) = 0.0000000E+00
            END DO
          END IF


!  End of subroutine TRANC. Return to calling subroutine MAC.
       RETURN
       END


!******************************************************************
       BLOCK DATA MACLABELS
!******************************************************************

       IMPLICIT NONE

!  Include parameter files.
       INCLUDE 'parametr.'
       INCLUDE 'ncntrl.'
       INCLUDE 'macparm.'
       INCLUDE 'mcinter2.'

!  Declare local variables.
       INTEGER i

       DATA (nemsenergylabel(i),i=1,362) /'PROD_CRUDEOIL','  ogoilprd','  cfcrddom',           &
       'PROD_NATURALGAS','  ogngplprd','  cfngl',                                              &
       'PROD_DRYNATURALGAS','  cfngc','  ogprdng','  ogshaleng',                               &
       'PROD_COAL','  cqsbb','  wc_prod_btu',                                                  &
       'PROD_NUCLEAR','  qurel',                                                               &
       'PROD_HYDRO','  qhoas',                                                                 &
       'PROD_BIOMASS','  qbmas','  crnethcd','  othethcd','  cllethcd','  qmsin','  qmsel',    &
                      '  wncmsel','  grd2dsqty','  grn2mgqty','  btlfrac',                     &
       'PROD_OTHRENEW','  qtras','  qgers','  qstrs','  qstcm','  qpvcm','  qpvrs','  qbmas',  &
                       '  qhoas','  qettr','  wncmsel',                                        &
       'PROD_OTHER','  wncmsel','  rfcrdoth','  cfcrddom','  sbo2gdtpd','  ygr2gdtpd',         &
                    '  wgr2gdtpd','  cfveggie','  rfhcxh2in','  cfrsq','  rfmetm85',           &
                    '  cfmeqt',                                                                &
       'IMPORT_PETROLEUM','  rfqicrd','  rfsprim','  cfcrdimp',                                &
                          '  rfpqiprdt','  cfimprd','  rfipqcbob','  cfcbob','  rfipqrbob',    &
                          '  cfrbob','  rfmtbi','  rfpqufc','  cfimuo',                        &
       'IMPORT_NATGAS','  ngimpvol','  cfngi',                                                 &
       'IMPORT_BIOFUELS','  ethimp','  biodimp','  biobuteimp','  cfngi','  cfbiod',           &
                         '  cfbiobute',                                                        &
       'IMPORT_COAL','  cqdbfb',                                                               &
       'IMPORT_OTHER','  qciin','  qeiel',                                                     &
       'EXPORT_FUELS','  rfqexcrd','  cfcrdexp','  rfqexprdt','  cfexprd','  ethexp',          &
                      '  cfpet','  biodexp','  cfbiod','  ngexpvol','  cfnge','  cqdbfb',      &
       'EXPORT_PETROLEUM','  rfqexcrd','   cfcrdexp','  rfqexprdt','  cfexprd',                &
       'EXPORT_NATGAS','  ngexpvol','  cfnge',                                                 &
       'EXPORT_COAL','  cqdbfb',                                                               &
       'EXPORT_BIOFUELS','  ethexp','  cfpet','  biodexp','  cfbiod','  biobuteexp',           &
                         '  cfbiobute',                                                        &
       'EXPORT_OTHER',                                                                         &
       'CONS_PETROLEUM','  qtpas','  ethanol','  biodiesel',                                   &
       'CONS_NATGAS','  qngas','  qgptr','  qlpin','  cfngn','  cfgtlliq','  qhytr',           &
       'CONS_COAL','  qclas','  qmcin','  qciin','  ogsupgas','  cfngc',                       &
       'CONS_NUCLEAR','  qurel',                                                               &
       'CONS_HYDRO','  qhoas',                                                                 &
       'CONS_BIOMASS','  qbmas','  qbmrf','  corncd','  cfcorn','  rfbiobutecd',               &
                      '   cfbiobute','  crnethcd','  cllethcd','  othethcd','  cfpet',         &
                      '  bimqtycd','  cfveggie','  cfbiod','  qbmrfbtl','  btlfrac',           &
                      '  cfbtlliq','  cbtlfrac','  cfcbtlliq','  ubavol',                      &
       'CONS_OTHRENEW','  qtras','  qgers','  qstrs','  qstcm','  qpvcm','  qpvrs',            &
                       '  qbmas','  qhoas','  qettr','  wncmsel',                              &
       'CONS_OTHER','  qeiel','  qhytr','  wncmsel',                                           &
       'ELGEN_RENEWABLE','  qtrel','  qpcel',                                                  &
       'DELENG_COAL','  qmcin','  qclas','  qclel','  qciin',                                  &
       'DELENG_ELECTRICITY','  qelas',                                                         &
       'DELENG_NATURALGAS','  qngas','  qgptr','  qlpin','  qngel',                            &
       'DELENG_PETROLEUM','  qdsas','  qdsel','  qksas','  qjftr','  qlgas','  qmgas',         &
                          '  qpfin','  qrsas','  qrsel','  qotas','  qsgin','  qpcin',         &
                          '  qasin',                                                           &
       'DELENG_BIO','  qmgtr','  qmgbs','  corncd','  crnethcd','  othethcd','  qdstr',        &
       'TRANCON_DISTILLATE','  qdstr',                                                         &
       'RESCON_PETROLEUM','  qtprs',                                                           &
       'RESCON_NATURALGAS','  qngrs',                                                          &
       'RESCON_ELECTRICITY','  qelrs',                                                         &
       'RESPRICE_PETROLEUM','  ptprs',                                                         &
       'RESPRICE_ELECTRICITY','  pelrs',                                                       &
       'RESPRICE_NATURALGAS','  pngrs',                                                        &
       'TRANPRICE_MOTORGAS','  pmgtr',                                                         &
       'TRANPRICE_DISTILLATE','  pdstr',                                                       &
       'INDPRICE_STEAMCOAL','  pclin',                                                         &
       'GASWELLHEADPRICE','  ogwprng',                                                         &
       'COMPRICE_ELECTRICITY','  pelcm',                                                       &
       'INDPRICE_ELECTRICITY','  pelin',                                                       &
       'TRANPRICE_ELECTRICITY','  peltr',                                                      &
       'COMPRICE_NATURALGAS','  pngcm',                                                        &
       'INDPRICE_NATURALGAS','  pngin',                                                        &
       'TRANPRICE_NATURALGAS','  pngtr',                                                       &
       'ELGENPRICE_NATURALGAS','  pngel',                                                      &
       'WORLDOILPRICE','  it_wop',                                                             &
       'COMPRICE_DISTILLATE','  pdscm',                                                        &
       'COMPRICE_RESIDUALFUEL','  prscm',                                                      &
       'INDPRICE_DISTILLATE','  pdsin',                                                        &
       'INDPRICE_RESIDUALFUEL','  prsin',                                                      &
       'TRANPRICE_JETFUEL','  pjftr',                                                          &
       'TRANPRICE_RESIDUALFUEL','  prstr',                                                     &
       'CARBEM_ELGENTOTAL','  emel(2)','  emel(1)','  emel(3)',                                &
       'CARBPEN_RESULTING','  emetax',                                                         &
       'TRANPRICE_AVGVEHICLE','  avg_prc_veh',                                                 &
       'TRANPRICE_AVGCAR','  avg_prc_car',                                                     &
       'TRANPRICE_AVGTRUCK','  avg_prc_trk',                                                   &
       'TRANTAX_HWYFUEL','  hfueltax',                                                         &
       'EMISREV_TOTAL','  emrev(1)','  emrev(2)','  emrev(3)','  emrev(4)','  emrev(5)',       &
       'ELSEC_INSTCAP','  g_inst_all',                                                         &
       'ELSEC_TRAN','  t_ovr',                                                                 &
       'ELSEC_RETRO',                                                                          &
       'EMISREV_COVER','  ghg_rev(1)',                                                         &
       'EMISREV_INTL','  emrev(11)',                                                           &
       'EMISAUCT_SHARE',                                                                       &
       'JPGDP',                                                                                &
       'MACIO25_PETROREFINE','  rfqprdt','  rfpqiprdt',                                        &
       'MACIO45_COALMINE',                                                                     &
       'MACIO46_OILGASXTRACT','  ogoilprd','  ogngplprd','  ogprdng','  ogprsup',              &
       'MACIO51_ELECUTIL','  ugntlnr(1)','  ugntlnr(2)',                                       &
       'MACIO52_GASUTIL',                                                                      &
       'MACEP36_COALMINE',                                                                     &
       'MACEP37_OILGASXTRACT',                                                                 &
       'HENRYHUBPRICE',' oghhprng',                                                            &
       'COMCON_ELECTRICITY','  qelcm',                                                         &
       'INDCON_ELECTRICITY','  qelin',                                                         &
       'TRANCON_ELECTRICITY','  qeltr',                                                        &
       'COMCON_NATURALGAS','  qngcm',                                                          &
       'INDCON_NATURALGAS','  qngin',                                                          &
       'TRANCON_NATURALGAS','  qngtr',                                                         &
       'ELGENCON_NATURALGAS','  qngel',                                                        &
       'ELGENCON_STEAMCOAL','  qclel',                                                         &
       'ELGENCON_FUELOIL','  qdsel','  qrsel',                                                 &
       'ELGENCON_ELECIMPORTS','  qeiel',                                                       &
       'ELGENCON_ELECPOWERTOT','  qtsel',                                                      &
       'COMCON_DISTILLATE','  qdscm',                                                          &
       'COMCON_RESIDUALFUEL','  qrscm',                                                        &
       'INDCON_DISTILLATE','  qdsin',                                                          &
       'INDCON_RESIDUALFUEL','  qrsin',                                                        &
       'TRANCON_MOTORGAS','  qmgtr',                                                           &
       'TRANCON_RESIDUALFUEL','  qrstr',                                                       &
       'ETHANOL_IMPORTS','  ethimp',                                                           &
       'ETHANOL_WHOLESALEPRICE','  pethm',                                                     &
       'CORN_PRICE','  crnprice',                                                              &
       'TRANCON_ETHANOL','  qettr',                                                            &
       'TRANPRICE_ETHANOL','  pettr',                                                          &
       'WESTTEXASOILPRICE','  wti_price',                                                      &
       'LPGFEEDSTOCKPRICE','  plginpf',                                                        &
       'NAPHTHAPRICE','  ppfin',                                                               &
       'ETHANEPRICE','  petin',                                                                &
       'ETHANEPRODUCTION','  rfqngpl_1',                                                       &
       'PROPANEPRODUCTION','  rfqngpl_2',                                                      &
       'RINREVENUE','  rfs_rev',                                                               &
       'ETHANOL_IP','ETHANOL_WHOLESALEPRICE',                                                  &
       'RDAYS','SCALEPRICE'/

       DATA (dlvars(i),i=1,164) /'Prod_CrudeOil','Prod_NaturalGas','Prod_DryNaturalGas',         &
       'Prod_Coal','Prod_Nuclear','Prod_Hydro','Prod_Biomass','Prod_OthRenew','Prod_Other',      &
       'Import_Petroleum','Import_NatGas','Import_BioFuels','Import_Coal','Import_Other',        &
       'Export_Fuels','Export_Petroleum','Export_NatGas','Export_Coal','Export_BioFuels',        &
       'Export_Other','Cons_Petroleum','Cons_NatGas','Cons_Coal','Cons_Nuclear','Cons_Hydro',    &
       'Cons_Biomass','Cons_OthRenew','Cons_Other','ElGen_Renewable','DelEng_Coal',              &
       'DelEng_Electricity','DelEng_NaturalGas','DelEng_Petroleum','DelEng_Bio',                 &
       'TranCon_Distillate','ResCon_Petroleum','ResCon_NaturalGas','ResCon_Electricity',         &
       'ResPrice_Petroleum','ResPrice_Electricity','ResPrice_NaturalGas','TranPrice_MotorGas',   &
       'TranPrice_Distillate','IndPrice_SteamCoal','GasWellheadPrice','ComPrice_Electricity',    &
       'IndPrice_Electricity','TranPrice_Electricity','ComPrice_NaturalGas',                     &
       'IndPrice_NaturalGas','TranPrice_NaturalGas','ElGenPrice_NaturalGas','WorldOilPrice',     &
       'ComPrice_Distillate','ComPrice_ResidualFuel','IndPrice_Distillate',                      &
       'IndPrice_ResidualFuel','TranPrice_JetFuel','TranPrice_ResidualFuel','CarbEm_ElGenTotal', &
       'CarbPen_Resulting','TranPrice_AvgVehicle','TranTax_HwyFuel','EmisRev_Total',             &
       'ElSec_InstCap','ElSec_Tran','ElSec_Retro','EmisRev_Cover','EmisRev_Intl',                &
       'EmisAuct_Share','JPGDP','MacIO25_PetroRefine','MacIO45_CoalMine','MacIO46_OilGasXtract', &
       'MacIO51_ElecUtil','MacIO52_GasUtil','MacEP36_CoalMine','MacEP37_OilGasXtract',           &
       'HenryHubPrice','ComCon_Electricity','IndCon_Electricity','TranCon_Electricity',          &
       'ComCon_NaturalGas','IndCon_NaturalGas','TranCon_NaturalGas','ElGenCon_NaturalGas',       &
       'ElGenCon_SteamCoal','ElGenCon_FuelOil','ElGenCon_ElecImports','ElGenCon_ElecPowerTot',   &
       'ComCon_Distillate','ComCon_ResidualFuel','IndCon_Distillate','IndCon_ResidualFuel',      &
       'TranCon_MotorGas','TranCon_ResidualFuel','TranCon_JetFuel','Ethanol_Imports',            &
       'Ethanol_WholesalePrice','Corn_Price','TranCon_Ethanol','TranPrice_Ethanol',              &
       'WestTexasOilPrice','LPGFeedstockPrice','NaphthaPrice','EthanePrice',                     &
       'EthaneProduction','PropaneProduction','RINRevenue','Ethanol_IP',                         &
       'WPI051_Neng_CD1','WPI053_Neng_CD1','WPI054_Neng_CD1','WPI055_Neng_CD1','WPI057_Neng_CD1','WPI0574_Neng_CD1', &
       'WPI051_Matl_CD2','WPI053_Matl_CD2','WPI054_Matl_CD2','WPI055_Matl_CD2','WPI057_Matl_CD2','WPI0574_Matl_CD2', &
       'WPI051_Enc_CD3', 'WPI053_Enc_CD3', 'WPI054_Enc_CD3', 'WPI055_Enc_CD3', 'WPI057_Enc_CD3', 'WPI0574_Enc_CD3',  &
       'WPI051_Wnc_CD4', 'WPI053_Wnc_CD4', 'WPI054_Wnc_CD4', 'WPI055_Wnc_CD4', 'WPI057_Wnc_CD4', 'WPI0574_Wnc_CD4',  &
       'WPI051_Satl_CD5','WPI053_Satl_CD5','WPI054_Satl_CD5','WPI055_Satl_CD5','WPI057_Satl_CD5','WPI0574_Satl_CD5', &
       'WPI051_Esc_CD6', 'WPI053_Esc_CD6', 'WPI054_Esc_CD6', 'WPI055_Esc_CD6', 'WPI057_Esc_CD6', 'WPI0574_Esc_CD6',  &
       'WPI051_Wsc_CD7', 'WPI053_Wsc_CD7', 'WPI054_Wsc_CD7', 'WPI055_Wsc_CD7', 'WPI057_Wsc_CD7', 'WPI0574_Wsc_CD7',  &
       'WPI051_Mtn_CD8', 'WPI053_Mtn_CD8', 'WPI054_Mtn_CD8', 'WPI055_Mtn_CD8', 'WPI057_Mtn_CD8', 'WPI0574_Mtn_CD8',  &
       'WPI051_Pac_CD9', 'WPI053_Pac_CD9', 'WPI054_Pac_CD9', 'WPI055_Pac_CD9', 'WPI057_Pac_CD9', 'WPI0574_Pac_CD9'/

       DATA (mamenergylabel(i),i=1,46) /'MC_ENGDOMPET (Quadrillions of BTUs)','MC_ENGDOMOO (Quadrillions of BTUs)',              &
       'MC_ENGDOMBIO (Quadrillions of BTUs)','MC_ENGDOMCOAL (Quadrillions of BTUs)','MC_ENGDOMNG (Quadrillions of BTUs)',        &
       'MC_ENGIMPBIO (Quadrillions of BTUs)','MC_ENGIMPCOAL (Quadrillions of BTUs)','MC_ENGIMPNG (Quadrillions of BTUs)',        &
       'MC_ENGIMPOO (Quadrillions of BTUs)','MC_ENGIMPPET (Quadrillions of BTUs)','MC_ENGIMP (Quadrillions of BTUs)',            &
       'MC_ENGEXP (Quadrillions of BTUs)','MC_ENGEXPBIO (Quadrillions of BTUs)','MC_ENGEXPCOAL (Quadrillions of BTUs)',          &
       'MC_ENGEXPNG (Quadrillions of BTUs)','MC_ENGEXPPET (Quadrillions of BTUs)','MC_ENGEXPOO (Quadrillions of BTUs)',          &
       'MC_DALLFUELS (Quadrillions of BTUs)','MC_DALLFUELSBIO (Quadrillions of BTUs)','MC_DALLFUELSCOAL (Quadrillions of BTUs)', &
       'MC_DALLFUELSNG (Quadrillions of BTUs)','MC_DALLFUELSOO (Quadrillions of BTUs)','MC_DALLFUELSPET (Quadrillions of BTUs)', &
       'MC_DENDUCOAL (Quadrillions of BTUs)','MC_DENDUELC (Quadrillions of BTUs)','MC_DENDUNG (Quadrillions of BTUs)',           &
       'MC_DENDUPET (Quadrillions of BTUs)','MC_DENDUBIO (Quadrillions of BTUs)','MC_QGASASF (Billions of Gallons)',             &
       'MC_CNEFAOR (Billions of Chained 2012 Dollars)','MC_CSVUGR (Billions of Chained 2012 Dollars)',                           &
       'MC_CSVUER (Billions of Chained 2012 Dollars)','MC_CNEGAOR (Billions of Chained 2012 Dollars)','MC_JPCNEFAO (1996=1.0)',  &
       'MC_JPCSVUE (1996=1.0)','MC_JPCSVUG (1996=1.0)','MC_JPCNEGAO (1996=1.0)','MC_WPI051 (1982=1.0)','MC_WPI053 (1982=1.0)',   &
       'MC_WPI054 (1982=1.0)','MC_WPI055 (1982=1.0)','MC_WPI0561 (1982=1.0)','MC_WPI057 (1982=1.0)','MC_WPI0574 (1982=1.0)',     &
       'MC_POILIMP (Dollars per Barrel)','MC_POILWTI (Dollars per Barrel)'/

       DATA (maminvestlabel(i),i=1,24) /'MC_CONSR (Billions of Chained 2012 Dollars)',                          &
       'MC_CDR (Billions of Chained 2012 Dollars)','MC_CDMVR (Billions of Chained 2012 Dollars)',               &
       'MC_CDFHER (Billions of Chained 2012 Dollars)','MC_CDRECIPR (Billions of Chained 2012 Dollars)',         &
       'MC_CDRECOR (Billions of Chained 2012 Dollars)','MC_CDOOR (Billions of Chained 2012 Dollars)',           &
       'MC_CDOTAER (Billions of Chained 2012 Dollars)','MC_IR (Billions of Chained 2012 Dollars)',              &
       'MC_IFXR (Billions of Chained 2012 Dollars)','MC_IFNRER (Billions of Chained 2012 Dollars)',             &
       'MC_IFNREER (Billions of Chained 2012 Dollars)','MC_IFNREETLVR (Billions of Chained 2012 Dollars)',      &
       'MC_IFNREEIPCTR (Billions of Chained 2012 Dollars)','MC_IFNREEIPCCR (Billions of Chained 2012 Dollars)', &
       'MC_IFNREIPSR (Billions of Chained 2012 Dollars)','MC_IFNREEMISCR (Billions of Chained 2012 Dollars)',   &
       'MC_IFNRESR (Billions of Chained 2012 Dollars)','MC_IFNRESBAOR (Billions of Chained 2012 Dollars)',      &
       'MC_IFNRESMIR (Billions of Chained 2012 Dollars)','MC_IFNRESPUR (Billions of Chained 2012 Dollars)',     &
       'MC_IFRER (Billions of Chained 2012 Dollars)','MC_IFRESR (Billions of Chained 2012 Dollars)',            &
       'MC_IFREER (Billions of Chained 2012 Dollars)'/

       DATA (mamnationlabel(i),i=1,62) /'MC_GDPR (Billions of Chained 2012 Dollars)',                                &
       'MC_GDPFER (Billions of Chained 2012 Dollars)','MC_CONSR (Billions of Chained 2012 Dollars)',                 &
       'MC_IR (Billions of Chained 2012 Dollars)','MC_XR (Billions of Chained 2012 Dollars)',                        &
       'MC_MR (Billions of Chained 2012 Dollars)','MC_GR (Billions of Chained 2012 Dollars)',                        &
       'MC_CDR (Billions of Chained 2012 Dollars)','MC_CNR (Billions of Chained 2012 Dollars)',                      &
       'MC_CSVR (Billions of Chained 2012 Dollars)','MC_IFNRESR (Billions of Chained 2012 Dollars)',                 &
       'MC_IFRESR (Billions of Chained 2012 Dollars)','MC_IFNREER (Billions of Chained 2012 Dollars)',               &
       'MC_IFREER (Billions of Chained 2012 Dollars)','MC_IFXR (Billions of Chained 2012 Dollars)',                  &
       'MC_IFNRER (Billions of Chained 2012 Dollars)','MC_IFRER (Billions of Chained 2012 Dollars)',                 &
       'MC_XGFFBR (Billions of Chained 2012 Dollars)','MC_XGINR (Billions of Chained 2012 Dollars)',                 &
       'MC_XGKR (Billions of Chained 2012 Dollars)','MC_XGAUTOR (Billions of Chained 2012 Dollars)',                 &
       'MC_XGCR (Billions of Chained 2012 Dollars)','MC_XGR (Billions of Chained 2012 Dollars)',                     &
       'MC_XSVTOTR (Billions of Chained 2012 Dollars)','MC_MGFFBR (Billions of Chained 2012 Dollars)',               &
       'MC_MGINAPETR (Billions of Chained 2012 Dollars)','MC_MGKR (Billions of Chained 2012 Dollars)',               &
       'MC_MGAUTOR (Billions of Chained 2012 Dollars)','MC_MGCR (Billions of Chained 2012 Dollars)',                 &
       'MC_MSVTOTR (Billions of Chained 2012 Dollars)','MC_IIR (Billions of Chained 2012 Dollars)',                  &
       'MC_GFMLR (Billions of Chained 2012 Dollars)','MC_GDP (Billions of Dollars)','MC_CONS (Billions of Dollars)', &
       'MC_I (Billions of Dollars)','MC_GNPR (Billions of Chained 2012 Dollars)','MC_JPGDP (2012=1.0)',              &
       'MC_RMTB3M (Percent per Annum)','MC_RMMTG30CON (Percent per Annum)','MC_RMCORPPUAA (Percent per Annum)',      &
       'MC_RMGBLUSREAL (Percent per Annum)','MC_JECIWSP (June 1989=1.0)','MC_SUVA (Millions of Units)',              &
       'MC_SUVLV (Millions of Units)','MC_SUVTL (Millions of Units)','MC_SUVTHAM (Millions of Units)',               &
       'MC_RUC (Percent)','MC_WPI (1982=1.0)','MC_WPI11 (1982=1.0)','MC_WPI14 (1982=1.0)',                           &
       'MC_NLFC (Millions of persons)','MC_RMFF (Percent per Annum)','MC_WPI05 (1982=1.0)',                          &
       'MC_RMTCM10Y (Percent per Annum)','MC_RMCORPBAA (Percent per Annum)','MC_CPIE (1982-84=1.0)',                 &
       'MC_NP65A (Millions of Persons)','MC_JQPCMHNF (2012=1.0)','MC_WPISOP3200 (1982=1.0)', 'MC_WPI10 (1982=1.0)',  &
       'MC_GSLGISNHWYR (Billions of Chained 2012 Dollars)','MC_RLRMCORPPUAA (Percent per Annum)'/

       DATA (mamregionlabel(i),i=1,117) /'MC_CPI (1982-84=1.0)','MC_YPDR (Billions of Chained 2012 Dollars)',      &
       'MC_YPCOMPWSD (Billions of Dollars)','MC_YP (Billions of Dollars)','MC_HUSMFG (Millions of Units)',         &
       'MC_HUSPS1 (Millions of Units)','MC_HUSPS2A (Millions of Units)','MC_KHUMFG (Millions of Units)',           &
       'MC_KHUPS1 (Millions of Units)','MC_KHUPS2A (Millions of Units)','MC_NP (Millions of Persons)',             &
       'MC_NP16A (Millions of Persons)','MC_RWM (Dollars per Hour)','MC_RWNM (Dollars per Hour)',                  &
       'AMUSE_REL (Rate of Growth)','EDUC (Rate of Growth)','HEALTH (Rate of Growth)',                             &
       'HOTEL_DORM (Rate of Growth)','OFFICE (Rate of Growth)','AUTO (Rate of Growth)',                            &
       'STORES (Rate of Growth)','WARE (Rate of Growth)','PUB_MISCNR (Rate of Growth)',                            &
       'EMPIND1 Food Products (Millions of Persons)',                                                              &
       'EMPIND2 Beverage and Tobacco Products (Millions of Persons)',                                              &
       'EMPIND3 Textiles, Apparel, and Leather (Millions of Persons)',                                             &
       'EMPIND4 Wood Products (Millions of Persons)',                                                              &
       'EMPIND5 Furniture and Related Products (Millions of Persons)',                                             &
       'EMPIND6 Paper Products (Millions of Persons)',                                                             &
       'EMPIND7 Printing (Millions of Persons)',                                                                   &
       'EMPIND8 Bulk Chemicals (Millions of Persons)',                                                             &
       'EMPIND9 Other Chemical Products (Millions of Persons)',                                                    &
       'EMPIND10 Petroleum and Coal Products (Millions of Persons)',                                               &
       'EMPIND11 Plastics and Rubber Products (Millions of Persons)',                                              &
       'EMPIND12 Nonmetallic Minerals (Millions of Persons)',                                                      &
       'EMPIND13 Primary Metals (Millions of Persons)',                                                            &
       'EMPIND14 Fabricated Metal Products (Millions of Persons)',                                                 &
       'EMPIND15 Machinery (Millions of Persons)',                                                                 &
       'EMPIND16 Computers and Electronic Products (Millions of Persons)',                                         &
       'EMPIND17 Transportation Equipment (Millions of Persons)',                                                  &
       'EMPIND18 Appliance and Electrical Equipment (Millions of Persons)',                                        &
       'EMPIND19 Miscellaneous Manufacturing (Millions of Persons)',                                               &
       'EMPIND20 Crop Production (Millions of Persons)',                                                           &
       'EMPIND21 Other Agriculture (Millions of Persons)',                                                         &
       'EMPIND22 Coal Mining (Millions of Persons)',                                                               &
       'EMPIND23 Oil and Gas Extraction and Support Activities (Millions of Persons)',                             &
       'EMPIND24 Other Mining and Quarrying (Millions of Persons)',                                                &
       'EMPIND25 Construction of Buildings (Millions of Persons)',                                                 &
       'EMPIND26 Heavy and Civ. Eng. Construction (Millions of Persons)',                                          &
       'EMPIND27 Specialty Trade Contractors (Millions of Persons)',                                               &
       'EMPSER1 Electric Power Generation and Distribution (Millions of Persons)',                                 &
       'EMPSER2 Natural Gas Distribution (Millions of Persons)',                                                   &
       'EMPSER3 Water, Sewage, and Related System (Millions of Persons)',                                          &
       'EMPSER4 Wholesale Trade (Millions of Persons)',                                                            &
       'EMPSER5 Retail Trade (Millions of Persons)',                                                               &
       'EMPSER6 Transportation and Warehousing (Millions of Persons)',                                             &
       'EMPSER7 Publishing Industries exp. Internet (Millions of Persons)',                                        &
       'EMPSER8 Broadcasting exp. Internet (Millions of Persons)',                                                 &
       'EMPSER9 Telecommunications (Millions of Persons)',                                                         &
       'EMPSER10 Finance and Insurance (Millions of Persons)',                                                     &
       'EMPSER11 Real Estate and Rental/Leasing (Millions of Persons)',                                            &
       'EMPSER12 Other Services (Millions of Persons)',                                                            &
       'REVIND1 Food Products (Billions of Fixed 2012 Dollars)',                                                   &
       'REVIND2 Grain and Oil Seed Milling (Billions of Fixed 2012 Dollars)',                                      &
       'REVIND3 Dairy Products (Billions of Fixed 2012 Dollars)',                                                  &
       'REVIND4 Animal Slaughter and Seafood Products (Billions of Fixed 2012 Dollars)',                           &
       'REVIND5 Other Food Products (Billions of Fixed 2012 Dollars)',                                             &
       'REVIND6 Beverage and Tobacco Products (Billions of Fixed 2012 Dollars)',                                   &
       'REVIND7 Textiles, Apparel, and Leather (Billions of Fixed 2012 Dollars)',                                  &
       'REVIND8 Wood Products (Billions of Fixed 2012 Dollars)',                                                   &
       'REVIND9 Furniture and Related Products (Billions of Fixed 2012 Dollars)',                                  &
       'REVIND10 Paper Products (Billions of Fixed 2012 Dollars)',                                                 &
       'REVIND11 Pulp and Paper Mills (Billions of Fixed 2012 Dollars)',                                           &
       'REVIND12 Paperboard Containers (Billions of Fixed 2012 Dollars)',                                          &
       'REVIND13 Other Paper Products (Billions of Fixed 2012 Dollars)',                                           &
       'REVIND14 Printing (Billions of Fixed 2012 Dollars)',                                                       &
       'REVIND15 Basic Inorganic Chemicals (Billions of Fixed 2012 Dollars)',                                      &
       'REVIND16 Basic Organic Chemicals (Billions of Fixed 2012 Dollars)',                                        &
       'REVIND17 Ethanol (Billions of Fixed 2012 Dollars)',                                                        &
       'REVIND18 Resins and Synthetics (Billions of Fixed 2012 Dollars)',                                          &
       'REVIND19 Agricultural Chemicals (Billions of Fixed 2012 Dollars)',                                         &
       'REVIND20 Other Chemical Products Subtotal (Billions of Fixed 2012 Dollars)',                               &
       'REVIND21 Pharma Products (Billions of Fixed 2012 Dollars)',                                                &
       'REVIND22 Paint Products (Billions of Fixed 2012 Dollars)',                                                 &
       'REVIND23 Soaps and Cleaning Products (Billions of Fixed 2012 Dollars)',                                    &
       'REVIND24 Other Chemical Products (Billions of Fixed 2012 Dollars)',                                        &
       'REVIND25 Petroleum Refining (Billions of Fixed 2012 Dollars)',                                             &
       'REVIND26 Other Petroleum and Coal Products (Billions of Fixed 2012 Dollars)',                              &
       'REVIND27 Plastics and Rubber Products (Billions of Fixed 2012 Dollars)',                                   &
       'REVIND28 Glass and Glass Products (Billions of Fixed 2012 Dollars)',                                       &
       'REVIND29 Flat Glass (Billions of Fixed 2012 Dollars)',                                                     &
       'REVIND30 Cement Manufacturing (Billions of Fixed 2012 Dollars)',                                           &
       'REVIND31 Lime Manufacturing (Billions of Fixed 2012 Dollars)',                                             &
       'REVIND32 Other Nonmetallic Mineral Products (Billions of Fixed 2012 Dollars)',                             &
       'REVIND33 Iron and Steel Products (Billions of Fixed 2012 Dollars)',                                        &
       'REVIND34 Alumina and Aluminum Products (Billions of Fixed 2012 Dollars)',                                  &
       'REVIND35 Other Primary Metals (Billions of Fixed 2012 Dollars)',                                           &
       'REVIND36 Fabricated Metal Products (Billions of Fixed 2012 Dollars)',                                      &
       'REVIND37 Machinery (Billions of Fixed 2012 Dollars)',                                                      &
       'REVIND38 Computers and Electronic Products (Billions of Fixed 2012 Dollars)',                              &
       'REVIND39 Transportation Equipment (Billions of Fixed 2012 Dollars)',                                       &
       'REVIND40 Appliance and Electrical Equipment (Billions of Fixed 2012 Dollars)',                             &
       'REVIND41 Miscellaneous Manufacturing (Billions of Fixed 2012 Dollars)',                                    &
       'REVIND42 Crop Production (Billions of Fixed 2012 Dollars)',                                                &
       'REVIND43 Animal Production (Billions of Fixed 2012 Dollars)',                                              &
       'REVIND44 Other Agriculture (Billions of Fixed 2012 Dollars)',                                              &
       'REVIND45 Coal Mining (Billions of Fixed 2012 Dollars)',                                                    &
       'REVIND46 Oil & Gas Extraction & Support Activities (Bil of Fixed 2012 Dollars)',                           &
       'REVIND47 Other Mining and Quarrying (Billions of Fixed 2012 Dollars)',                                     &
       'REVIND48 Construction (Billions of Fixed 2012 Dollars)',                                                   &
       'Sum of All Chemicals; NAICS 325 (Billions of Fixed 2012 Dollars)',                                         &
       'Sum of All Petroleum; NAICS 324 (Billions of Fixed 2012 Dollars)',                                         &
       'Sum of All Stone, Clay, Glass & Cement; NAICS 327 (Bil of Fixed 2012 Dollars)',                            &
       'Sum of All Primary Metals; NAICS 331 (Billions of Fixed 2012 Dollars)',                                    &
       'Total Manufacturing Output; NAICS 31-33 (Billions of Fixed 2012 Dollars)',                                 &
       'Total Industrial Output; NAICS 11, 21, 23, 31-33 (Billions of Fixed 2012 Dollars)',                        &
       'Total Gross Output; NAICS 11-92 (Billions of Fixed 2012 Dollars)'/

       DATA (mamnonmangolabel(i),i=1,11) /'REVSER1 Transportation and Warehousing (Billions of Fixed 2012 Dollars)', &
       'REVSER2 Broadcasting and Telecommunications (Billions of Fixed 2012 Dollars)',                               &
       'REVSER3 Electric Power Generation and Distribution (Billions of Fixed 2012 Dollars)',                        &
       'REVSER4 Natural Gas Distribution (Billions of Fixed 2012 Dollars)',                                          &
       'REVSER5 Water, Sewage, and Related System (Billions of Fixed 2012 Dollars)',                                 &
       'REVSER6 Wholesale Trade (Billions of Fixed 2012 Dollars)',                                                   &
       'REVSER7 Retail Trade (Billions of Fixed 2012 Dollars)',                                                      &
       'REVSER8 Finance, Insurance, and Real Estate (Billions of Fixed 2012 Dollars)',                               &
       'REVSER9 Other Services (Billions of Fixed 2012 Dollars)',                                                    &
       'REVSER10 Public Administration (Billions of Fixed 2012 Dollars)',                                            &
       'Total Non-Industrial/Service Gross Output; NAICS 22, 42-92 (Billions of Fixed 2012 Dollars)'/

       DATA (mamcensuslabel(i),i=1,11) /'New England','Middle Atlantic','East North Central', &
       'West North Central','South Atlantic','East South Central','West South Central',       &
       'Mountain','Pacific','California','United States'/

       DATA (mamemploylabel(i),i=1,43) /                                                    &
       'EMPIND1 Food Products (Millions of Persons)',                                       &
       'EMPIND2 Beverage and Tobacco Products (Millions of Persons)',                       &
       'EMPIND3 Textiles, Apparel, and Leather (Millions of Persons)',                      &
       'EMPIND4 Wood Products (Millions of Persons)',                                       &
       'EMPIND5 Furniture and Related Products (Millions of Persons)',                      &
       'EMPIND6 Paper Products (Millions of Persons)',                                      &
       'EMPIND7 Printing (Millions of Persons)',                                            &
       'EMPIND8 Bulk Chemicals (Millions of Persons)',                                      &
       'EMPIND9 Other Chemical Products (Millions of Persons)',                             &
       'EMPIND10 Petroleum and Coal Products (Millions of Persons)',                        &
       'EMPIND11 Plastics and Rubber Products (Millions of Persons)',                       &
       'EMPIND12 Nonmetallic Minerals (Millions of Persons)',                               &
       'EMPIND13 Primary Metals (Millions of Persons)',                                     &
       'EMPIND14 Fabricated Metal Products (Millions of Persons)',                          &
       'EMPIND15 Machinery (Millions of Persons)',                                          &
       'EMPIND16 Computers and Electronic Products (Millions of Persons)',                  &
       'EMPIND17 Transportation Equipment (Millions of Persons)',                           &
       'EMPIND18 Appliance and Electrical Equipment (Millions of Persons)',                 &
       'EMPIND19 Miscellaneous Manufacturing (Millions of Persons)',                        &
       'EMPIND20 Crop Production (Millions of Persons)',                                    &
       'EMPIND21 Other Agriculture (Millions of Persons)',                                  &
       'EMPIND22 Coal Mining (Millions of Persons)',                                        &
       'EMPIND23 Oil and Gas Extraction and Support Activities (Millions of Persons)',      &
       'EMPIND24 Other Mining and Quarrying (Millions of Persons)',                         &
       'EMPIND25 Construction of Buildings (Millions of Persons)',                          &
       'EMPIND26 Heavy and Civ. Eng. Construction (Millions of Persons)',                   &
       'EMPIND27 Specialty Trade Contractors (Millions of Persons)',                        &
       'EMPSER1 Electric Power Generation and Distribution (Millions of Persons)',          &
       'EMPSER2 Natural Gas Distribution (Millions of Persons)',                            &
       'EMPSER3 Water, Sewage, and Related System (Millions of Persons)',                   &
       'EMPSER4 Wholesale Trade (Millions of Persons)',                                     &
       'EMPSER5 Retail Trade (Millions of Persons)',                                        &
       'EMPSER6 Transportation and Warehousing (Millions of Persons)',                      &
       'EMPSER7 Publishing Industries exp. Internet (Millions of Persons)',                 &
       'EMPSER8 Broadcasting exp. Internet (Millions of Persons)',                          &
       'EMPSER9 Telecommunications (Millions of Persons)',                                  &
       'EMPSER10 Finance and Insurance (Millions of Persons)',                              &
       'EMPSER11 Real Estate and Rental/Leasing (Millions of Persons)',                     &
       'EMPSER12 Other Services (Millions of Persons)',                                     &
       'Total Manufacturing (Millions of Persons)',                                         &
       'Total Non-Manufacturing (Millions of Persons)',                                     &
       'Total Services (Millions of Persons)',                               &
       'Total Non-Agricultural (Millions of Persons)'/

       END BLOCK DATA MACLABELS


!******************************************************************
       SUBROUTINE MACOUTPUT
!******************************************************************
!  Writes to the output directory the following worksheets:
!
!  MC_COMMON.CSV - Common block variables containing:
!                  energy driver variables
!                  energy investment variables
!                  national variables
!                  regional variables (arranged by variable)
!
!  MC_REGIONAL.CSV - Regional solution, and base by Census 
!                    Division including National.
!
!  MC_INDUSTRIAL.CSV - Solution for manufacturing and non-manufacturing,
!                      services, output at the Census Division level
!                      including National. The base is at the national
!                      level only since shares mostly of gross output
!                      are used to produce the forecast.
!
!  MC_EMPLOYMENT.CSV - Solution and base for employment.
!
!  MC_NATIONAL.CSV   - Solution, base and change from base of 
!                      national macro variables.

!******************************************************************

       IMPLICIT NONE

!  Include parameter files.
       INCLUDE 'emmparm'
       INCLUDE 'parametr'
       INCLUDE 'ncntrl'
       INCLUDE 'macout'
       INCLUDE 'macparm'
       INCLUDE 'mcinter2'
       INCLUDE 'intout'
       INCLUDE 'cdsparms'
       INCLUDE 'coalout'
       INCLUDE 'coalrep'
       INCLUDE 'pmmout'
       INCLUDE 'lfmmout'
       INCLUDE 'pmmrpt'
       INCLUDE 'pmmftab'
       INCLUDE 'uefpout'     ! UEFP output variables
       INCLUDE 'uefdout'     ! EFD output variables
       INCLUDE 'udatout'     ! UDAT output variables
       INCLUDE 'uecpout'     ! UECP output variables
       INCLUDE 'uettout'     ! UETT output variables
       INCLUDE 'ngtdmrep'
       INCLUDE 'emission'
       INCLUDE 'ghgrep'

!  Declare local variables.
       INTEGER        i,j,k,l,charc
       CHARACTER*1200 label,line

!  Open MC_COMMON.CSV for writes on variables for global data structure.
       OPEN(3,ACTION='WRITE',FILE='MC_COMMON.CSV')
!  Write header.
       line = '"'//TRIM(scen)//' '//TRIM(date)//' '//TRIM(comment)//'"'
       charc = LEN(TRIM(line))
       WRITE(3,'(A<charc>)') line
       WRITE(3,*) ' '
!  Write section title.
       line = '"'//"Energy Driver Variables"//'"'
       charc = LEN(TRIM(line))
       WRITE(3,'(A<charc>)') line
!  Write year labels across columns.
       line = '"'//"obs"//'"'
       DO i = 1990,mamlastyr
         WRITE(label,'(i4)') i
         line = TRIM(line)//',"'//TRIM(label)//'"'
       END DO
       charc = LEN(TRIM(line))
       WRITE(3,'(A<charc>)') line
!  Write variable name and data.
       DO j = 1,46
         line = '"'//TRIM(mamenergylabel(j))//'"'
         DO i = 1,mamlastyr-1990+1
           WRITE(label,'(F15.5)') mamenergy(j,i)
           line = TRIM(line)//',"'//TRIM(label)//'"'
         END DO
         charc = LEN(TRIM(line))
         WRITE(3,'(A<charc>)') line
       END DO
       WRITE(3,*) ' '
!  Write section title.
       line = '"'//"Energy Investment Variables"//'"'
       charc = LEN(TRIM(line))
       WRITE(3,'(A<charc>)') line
!  Write year labels across columns.
       line = '"'//"obs"//'"'
       DO i = 1990,mamlastyr
         WRITE(label,'(i4)') i
         line = TRIM(line)//',"'//TRIM(label)//'"'
       END DO
       charc = LEN(TRIM(line))
       WRITE(3,'(A<charc>)') line
!  Write variable name and data.
       DO j = 1,24
         line = '"'//TRIM(maminvestlabel(j))//'"'
         DO i = 1,mamlastyr-1990+1
           WRITE(label,'(F15.5)') maminvest(j,i)
           line = TRIM(line)//',"'//TRIM(label)//'"'
         END DO
         charc = LEN(TRIM(line))
         WRITE(3,'(A<charc>)') line
       END DO
       WRITE(3,*) ' '
!  Write section title.
       line = '"'//"National Variables"//'"'
       charc = LEN(TRIM(line))
       WRITE(3,'(A<charc>)') line
!  Write year labels across columns.
       line = '"'//"obs"//'"'
       DO i = 1990,mamlastyr
         WRITE(label,'(i4)') i
         line = TRIM(line)//',"'//TRIM(label)//'"'
       END DO
       charc = LEN(TRIM(line))
       WRITE(3,'(A<charc>)') line
!  Write variable name and data.
       DO j = 1,62
         line = '"'//TRIM(mamnationlabel(j))//'"'
         DO i = 1,mamlastyr-1990+1
           IF (j .LT. 62) THEN
             WRITE(label,'(F15.5)') esmac(j,i)
           ELSE
             WRITE(label,'(F15.5)') mc_rlrmcorppuaa(i)
           END IF
           line = TRIM(line)//',"'//TRIM(label)//'"'
         END DO
         charc = LEN(TRIM(line))
         WRITE(3,'(A<charc>)') line
       END DO
       WRITE(3,*) ' '
!  Write section title.
       line = '"'//"Regional Variables"//'"'
       charc = LEN(TRIM(line))
       WRITE(3,'(A<charc>)') line
!  Write year labels across columns.
       line = '"'//"obs"//'"'
       DO i = 1990,mamlastyr
         WRITE(label,'(i4)') i
         line = TRIM(line)//',"'//TRIM(label)//'"'
       END DO
       charc = LEN(TRIM(line))
       WRITE(3,'(A<charc>)') line
!  Write variable name and data.
       DO j = 1,117
         DO k = 1,9
           line = '"'//TRIM(mamregionlabel(j))//' '//TRIM(mamcensuslabel(k))//'"'
           DO i = 1,mamlastyr-1990+1
             IF (j .le. 23) THEN
               WRITE(label,'(F15.5)') esmacreg(k,j,i)
             ELSE IF (j .GE. 24 .AND. j .LE. 62) THEN
               WRITE(label,'(F15.5)') mc_empna(k,j-23,i)
             ELSE
               WRITE(label,'(F15.5)') mc_revind(k,j-62,i)
             END IF
             line = TRIM(line)//',"'//TRIM(label)//'"'
           END DO
           charc = LEN(TRIM(line))
           WRITE(3,'(A<charc>)') line
         END DO
       END DO
!  Close MC_COMMON.CSV for writes on variables for global data structure.
       CLOSE(3)

!  Open MC_REGIONAL.CSV for writes of regional data.
       OPEN(3,ACTION='WRITE',FILE='MC_REGIONAL.CSV')
!  Write header.
       line = '"'//TRIM(scen)//' '//TRIM(date)//' '//TRIM(comment)//'"'
       charc = LEN(TRIM(line))
       WRITE(3,'(A<charc>)') line
       WRITE(3,*) ' '
!  Write Census Division title.
       DO j = 1,mcnumregs
         line = '"'//TRIM("Census Division")//' - '//TRIM(mamcensuslabel(j))//'"'
         charc = LEN(TRIM(line))
         WRITE(3,'(A<charc>)') line
!  Write year labels across columns.
         line = '"'//"obs"//'"'
         DO i = 1990,mamlastyr
           WRITE(label,'(i4)') i
           line = TRIM(line)//',"'//TRIM(label)//'"'
         END DO
         charc = LEN(TRIM(line))
         WRITE(3,'(A<charc>)') line
!  Write variable name and data.
         DO k = 1,117
           line = '"'//TRIM(mamregionlabel(k))//'"'
           DO i = 1,mamlastyr-1990+1
!  Regional macro.
             IF (k .le. 23) THEN
               WRITE(label,'(F15.5)') esmacreg(j,k,i)
!  Regional employment by industry.
             ELSE IF (k .GE. 24 .AND. k .LE. 62) THEN
               WRITE(label,'(F15.5)') mc_empna(j,k-23,i)
!  Regional industrial output.
             ELSE
               WRITE(label,'(F15.5)') mc_revind(j,k-62,i)
             END IF
             line =  TRIM(line)//',"'//TRIM(label)//'"'
           END DO
           charc = LEN(TRIM(line))
           WRITE(3,'(A<charc>)') line
         END DO
!  Regional services.
         IF (j .EQ. mcnumregs) THEN
           DO l = 1,11
             line = '"'//TRIM(mamnonmangolabel(l))//'"'
             DO i = 1,mamlastyr-1990+1
               WRITE(label,'(F15.5)') mc_revser(j,l,i)
               line = TRIM(line)//',"'//TRIM(label)//'"'
             END DO
             charc = LEN(TRIM(line))
             WRITE(3,'(A<charc>)') line
           END DO
         END IF
         WRITE(3,*) ' '
       END DO
!  Close MC_REGIONAL.CSV for writes of regional data.
       CLOSE(3)

!  Open MC_INDUSTRIAL.CSV for writes of regional industrial output and national services.
       OPEN(3,ACTION='WRITE',FILE='MC_INDUSTRIAL.CSV')
!  Write header.
       line = '"'//TRIM(scen)//' '//TRIM(date)//' '//TRIM(comment)//'"'
       charc = LEN(TRIM(line))
       WRITE(3,'(A<charc>)') line
       WRITE(3,*) ' '
!  Write Census Division title.
       DO j = 1,mcnumregs
         line = '"'//TRIM("Census Division")//' - '//TRIM(mamcensuslabel(j))//'"'
         charc = LEN(TRIM(line))
         WRITE(3,'(A<charc>)') line
!  Write year labels across columns.
         line = '"'//"obs"//'"'
         DO i = 1990,mamlastyr
           WRITE(label,'(i4)') i
           line = TRIM(line)//',"'//TRIM(label)//'"'
         END DO
         charc = LEN(TRIM(line))
         WRITE(3,'(A<charc>)') line
!  Write variable name and data, manufacturing.
         DO k = 1,54
           line = '"'//TRIM(mamregionlabel(k+62))//'"'
           DO i = 1,mamlastyr-1990+1
             WRITE(label,'(F15.5)') esind(j,k,i)
             line =  TRIM(line)//',"'//TRIM(label)//'"'
           END DO
           charc = LEN(TRIM(line))
           WRITE(3,'(A<charc>)') line
         END DO
         WRITE(3,*) ' '
       END DO
!  Write variable name and data non-industrial.
       DO l = 1,11
         line = '"'//TRIM(mamnonmangolabel(l))//'"'
         DO i = 1,mamlastyr-1990+1
           WRITE(label,'(F15.5)') esserv(l,i)
           line = TRIM(line)//',"'//TRIM(label)//'"'
         END DO
         charc = LEN(TRIM(line))
         WRITE(3,'(A<charc>)') line
       END DO
       WRITE(3,*) ' '
!  Write variable name and data for total gross output.
       line = '"'//TRIM(mamregionlabel(117))//'"'
       DO i = 1,mamlastyr-1990+1
         WRITE(label,'(F15.5)') esind(11,54+1,i)
         line = TRIM(line)//',"'//TRIM(label)//'"'
       END DO
       charc = LEN(TRIM(line))
       WRITE(3,'(A<charc>)') line
!  Close MC_INDUSTRIAL.CSV for writes of regional industrial output and national services.
       CLOSE(3)

!  Open MC_EMPLOYMENT.CSV for writes of national employment.
       OPEN(3,ACTION='WRITE',FILE='MC_EMPLOYMENT.CSV')
!  Write header.
       line = '"'//TRIM(scen)//' '//TRIM(date)//' '//TRIM(comment)//'"'
       charc = LEN(TRIM(line))
       WRITE(3,'(A<charc>)') line
       WRITE(3,*) ' '
!  Write title.
       line = '"'//TRIM("Employment Model - Solution")//'"'
       charc = LEN(TRIM(line))
       WRITE(3,'(A<charc>)') line
!  Write year labels across columns.
       line = '"'//"obs"//'"'
       DO i = 1990,mamlastyr
         WRITE(label,'(i4)') i
         line = TRIM(line)//',"'//TRIM(label)//'"'
       END DO
       charc = LEN(TRIM(line))
       WRITE(3,'(A<charc>)') line
!  Write variable name and data.
       j = 1
       DO k = 1,43
         line = '"'//TRIM(mamemploylabel(j))//'"'
         DO i = 1,mamlastyr-1990+1
           WRITE(label,'(F15.5)') esemp(11,k,i)
           line = TRIM(line)//',"'//TRIM(label)//'"'
         END DO
         charc = LEN(TRIM(line))
         WRITE(3,'(A<charc>)') line
         j = j+1
       END DO
!  Close MC_EMPLOYMENT.CSV for writes of national employment.
       CLOSE(3)

!  Open MC_NATIONAL.CSV for writes of national employment.
       OPEN(3,ACTION='WRITE',FILE='MC_NATIONAL.CSV')
!  Write header.
       line = '"'//TRIM(scen)//' '//TRIM(date)//' '//TRIM(comment)//'"'
       charc = LEN(TRIM(line))
       WRITE(3,'(A<charc>)') line
       WRITE(3,*) ' '
!  Write title.
       line = '"'//TRIM("National Model - Solution")//'"'
       charc = LEN(TRIM(line))
       WRITE(3,'(A<charc>)') line
!  Write year labels across columns.
       line = '"'//"obs"//'"'
       DO i = 1990,mamlastyr
         WRITE(label,'(i4)') i
         line = TRIM(line)//',"'//TRIM(label)//'"'
       END DO
       charc = LEN(TRIM(line))
       WRITE(3,'(A<charc>)') line
!  Write variable name and data.
       DO k = 1,62
         line = '"'//TRIM(mamnationlabel(k))//'"'
         DO i = 1,mamlastyr-1990+1
           IF (k .LT. 62) THEN
             WRITE(label,'(F15.5)') esmac(k,i)
           ELSE
             WRITE(label,'(F15.5)') mc_rlrmcorppuaa(i)
           END IF
           line = TRIM(line)//',"'//TRIM(label)//'"'
         END DO
         charc = LEN(TRIM(line))
         WRITE(3,'(A<charc>)') line
       END DO
!  Close MC_NATIONAL.CSV for writes of national employment.
       CLOSE(3)

!  End of subroutine MACOUTPUT. Return to calling subroutine MAC.
       RETURN
       END

subroutine GETENVmac(eviewsenv,eviewscommand,computername)
  use ifport
  character*(*) eviewsenv,eviewscommand
  character*(*) computername
  call getenv(eviewsenv,eviewscommand)
  call getenv('COMPUTERNAME',computername)
  return
  end
