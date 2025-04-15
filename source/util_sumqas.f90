!-*- f90 -*-
!******************************************************************
!*  Subroutine SUMQAS
!*
!*  Sums NEMS ACROSS-SECTOR (...AS) QUANTITY variables; sums TOTAL
!*  PETROLEUM, TOTAL RENEWABLE, and TOTAL SECTOR QUANTITY variables.
!******************************************************************
      SUBROUTINE SUMQAS
      IMPLICIT NONE

      include 'parametr'
      include 'ncntrl'
      include 'qblk'
      include 'ngtdmrep'
      include 'indrep'
      include 'indout'
      include 'convfact'
      include 'tranrep'
      include 'qsblk'
      include 'calshr'
      include 'qonroad'
      INTEGER IY,IC,IVAR,isedyr

!WRITE(*,*)"start SUMQAS()"
!WRITE(*,*)"SUMQAS(), MNUMQ=",MNUMQ

! +++ FIRST sum NEMS QUANTITIES Across Regions for the current year.
      DO IVAR=1,MNUMQ
         CALL SUMARY(MQTY(1,CURIYR,IVAR),MNUMCR)
      ENDDO
! share Pacific (Division 9) to California based on SEDS
!WRITE(*,*)"SUMQAS(), curiyr=",curiyr
!WRITE(*,*)"SUMQAS(), msedyr=",msedyr
      isedyr=min(curiyr,msedyr)
      mqty(10,curiyr,:)=0.  ! initialize all California
      if(curiyr.ge.msedyr) then
        QELRS(10,curiyr) = QELRS(9,curiyr) * ELRS_SHR(curiyr) !  1 Purchased Electricity - Residential
        QELCM(10,curiyr) = QELCM(9,curiyr) * ELCM_SHR(curiyr) !  2 Purchased Electricity - Commercial
        QELTR(10,curiyr) = QELTR(9,curiyr) * ELTR_SHR(curiyr) !  3 Purchased Electricity - Transportation
        QELIN(10,curiyr) = QELIN(9,curiyr) * ELIN_SHR(curiyr) !  4 Purchased Electricity - Industrial
        QELRF(10,curiyr) = QELRF(9,curiyr) * ELRF_SHR(curiyr) !  5 Purchased Electricity - Refinery
        QELHM(10,curiyr) = QELHM(9,curiyr) * ELHM_SHR(curiyr) !  6 Purchased Electricity - Hydrogen

        QNGRS(10,curiyr) = QNGRS(9,curiyr) * NGRS_SHR(curiyr) ! 24 Natural Gas - Residential
        QNGCM(10,curiyr) = QNGCM(9,curiyr) * NGCM_SHR(curiyr) ! 25 Natural Gas - Commercial
        QNGTR(10,curiyr) = QNGTR(9,curiyr) * NGTR_SHR(curiyr) ! 26 Natural Gas - Transportation
        QNGIN(10,curiyr) = QNGIN(9,curiyr) * NGIN_SHR(curiyr) ! 27 Natural Gas - Industrial
        QNGRF(10,curiyr) = QNGRF(9,curiyr) * NGRF_SHR(curiyr) ! 28 Natural Gas - Refinery
        QNGEL(10,curiyr) = QNGEL(9,curiyr) * NGEL_SHR(curiyr) ! 29 Natural Gas - Electricity
        QNGHM(10,curiyr) = QNGHM(9,curiyr) * NGHM_SHR(curiyr) ! 30 Natural Gas - Hydrogen
        QGPTR(10,curiyr) = QGPTR(9,curiyr) * GPTR_SHR(curiyr) ! 32 Natural Gas - Pipeline
        QLPIN(10,curiyr) = QLPIN(9,curiyr) * LPIN_SHR(curiyr) ! 33 Natural Gas - Lease and Plant Fuel
        QNGLQ(10,curiyr) = QNGLQ(9,curiyr) * LPIN_SHR(curiyr) !    Natural Gas - Liquefaction

        QCLRS(10,curiyr) = QCLRS(9,curiyr) * CLRS_SHR(curiyr) ! 34 Coal - Residential
        QCLCM(10,curiyr) = QCLCM(9,curiyr) * CLCM_SHR(curiyr) ! 35 Coal - Commercial
        QCLIN(10,curiyr) = QCLIN(9,curiyr) * CLIN_SHR(curiyr) ! 36 Coal - Industrial
        QCLRF(10,curiyr) = QCLRF(9,curiyr) * CLRF_SHR(curiyr) ! 37 Coal - Refinery
        QCLEL(10,curiyr) = QCLEL(9,curiyr) * CLEL_SHR(curiyr) ! 38 Coal - Electricity
        QCLSN(10,curiyr) = QCLSN(9,curiyr) * CLSN_SHR(curiyr) ! 39 Coal - Synthetics
        QCLHM(10,curiyr) = QCLHM(9,curiyr) * CLHM_SHR(curiyr) ! 40 Coal - Hydrogen

        QMCIN(10,curiyr) = QMCIN(9,curiyr) * MCIN_SHR(curiyr) ! 42 Metallurgical Coal - Industrial
        QMGCM(10,curiyr) = QMGCM(9,curiyr) * MGCM_SHR(curiyr) ! 43 Motor Gasoline - Commercial
        QMGTR(10,curiyr) = QMGTR(9,curiyr) * MGTR_SHR(curiyr) ! 44 Motor Gasoline - Transportation
        QMGIN(10,curiyr) = QMGIN(9,curiyr) * MGIN_SHR(curiyr) ! 45 Motor Gasoline - Industrial

        QJFTR(10,curiyr) = QJFTR(9,curiyr) * JFTR_SHR(curiyr) ! 47 Jet Fuel - Transportation
        QDSRS(10,curiyr) = QDSRS(9,curiyr) * DSRS_SHR(curiyr) ! 48 Distillate - Residential
        QDSCM(10,curiyr) = QDSCM(9,curiyr) * DSCM_SHR(curiyr) ! 49 Distillate - Commercial
        QDSTR(10,curiyr) = QDSTR(9,curiyr) * DSTR_SHR(curiyr) ! 50 Distillate - Transportation
        QDSIN(10,curiyr) = QDSIN(9,curiyr) * DSIN_SHR(curiyr) ! 51 Distillate - Industrial
        QDSRF(10,curiyr) = QDSRF(9,curiyr) * DSRF_SHR(curiyr) ! 52 Distillate - Refinery
        QDSEL(10,curiyr) = QDSEL(9,curiyr) * DSEL_SHR(curiyr) ! 53 Distillate - Electricity (+petroleum coke)

        QKSRS(10,curiyr) = QKSRS(9,curiyr) * KSRS_SHR(curiyr) ! 55 Kerosene - Residential
        QKSCM(10,curiyr) = QKSCM(9,curiyr) * KSCM_SHR(curiyr) ! 56 Kerosene - Commercial
        QKSIN(10,curiyr) = QKSIN(9,curiyr) * KSIN_SHR(curiyr) ! 57 Kerosene - Industrial

        QLGRS(10,curiyr) = QLGRS(9,curiyr) * LGRS_SHR(curiyr) ! 59 Liquid Petroleum Gases - Residential
        QLGCM(10,curiyr) = QLGCM(9,curiyr) * LGCM_SHR(curiyr) ! 60 Liquid Petroleum Gases - Commercial
        QLGTR(10,curiyr) = QLGTR(9,curiyr) * LGTR_SHR(curiyr) ! 61 Liquid Petroleum Gases - Transportation
        QLGIN(10,curiyr) = QLGIN(9,curiyr) * LGIN_SHR(curiyr) ! 62 Liquid Petroleum Gases - Industrial
        QLGRF(10,curiyr) = QLGRF(9,curiyr) * LGRF_SHR(curiyr) ! 63 Liquid Petroleum Gases - Refinery

        QRSCM(10,curiyr) = QRSCM(9,curiyr) * RSCM_SHR(curiyr) ! 74 Residual Fuel - Commercial
        QRSTR(10,curiyr) = QRSTR(9,curiyr) * RSTR_SHR(curiyr) ! 75 Residual Fuel - Transportation
        QRSIN(10,curiyr) = QRSIN(9,curiyr) * RSIN_SHR(curiyr) ! 76 Residual Fuel - Industrial
        QRSRF(10,curiyr) = QRSRF(9,curiyr) * RSRF_SHR(curiyr) ! 77 Residual Fuel - Refinery
        QRSEL(10,curiyr) = QRSEL(9,curiyr) * RSEL_SHR(curiyr) ! 78 Residual Fuel - Electricity

        QPFIN(10,curiyr) = QPFIN(9,curiyr) * PFIN_SHR(curiyr) ! 80 Petrochemical Feedstocks - Industrial
        QSGIN(10,curiyr) = QSGIN(9,curiyr) * SGIN_SHR(curiyr) ! 81 Still Gas - Industrial
        QSGRF(10,curiyr) = QSGRF(9,curiyr) * SGRF_SHR(curiyr) ! 82 Still Gas - Refinery
        QPCIN(10,curiyr) = QPCIN(9,curiyr) * PCIN_SHR(curiyr) ! 83 Petroleum Coke - Industrial
        QPCRF(10,curiyr) = QPCRF(9,curiyr) * PCRF_SHR(curiyr) ! 84 Petroleum Coke - Refinery
        QPCEL(10,curiyr) = QPCEL(9,curiyr) * PCEL_SHR(curiyr) ! 85 Petroleum Coke - Electricity
        QPCAS(10,curiyr) = QPCAS(9,curiyr) * PCAS_SHR(curiyr) ! 86 Petroleum Coke - All Sectors
        QASIN(10,curiyr) = QASIN(9,curiyr) * ASIN_SHR(curiyr) ! 87 Asphalt and Road Oil - Industrial
        QOTTR(10,curiyr) = QOTTR(9,curiyr) * OTTR_SHR(curiyr) ! 88 Other Petroleum - Transportation
        QOTIN(10,curiyr) = QOTIN(9,curiyr) * OTIN_SHR(curiyr) ! 89 Other Petroleum - Industrial
        QOTRF(10,curiyr) = QOTRF(9,curiyr) * OTRF_SHR(curiyr) ! 90 Other Petroleum - Refinery

        QMETR(10,curiyr) = QMETR(9,curiyr) * METR_SHR(curiyr) ! 99 Methanol - Transporation
        QETTR(10,curiyr) = QETTR(9,curiyr) * ETTR_SHR(curiyr) !100 Ethanol - Transporation
        QETHM(10,curiyr) = QETHM(9,curiyr) * ETHM_SHR(curiyr) !101 Ethanol - Hydrogen
        QH2TR(10,curiyr) = QH2TR(9,curiyr) * HYTR_SHR(curiyr) !102 Liquid Hydrogen - Transportation
        QUREL(10,curiyr) = QUREL(9,curiyr) * UREL_SHR(curiyr) !103 Uranium - Electricity
        QURHM(10,curiyr) = QURHM(9,curiyr) * URHM_SHR(curiyr) !104 Uranium - Hydrogen
        QHOIN(10,curiyr) = QHOIN(9,curiyr) * HOIN_SHR(curiyr) !105 Hydropower - Industrial
        QHOEL(10,curiyr) = QHOEL(9,curiyr) * HOEL_SHR(curiyr) !106 Hydropower - Electricity

        QGERS(10,curiyr) = QGERS(9,curiyr) * GERS_SHR(curiyr) !108 Geothermal - Residential
        QGEIN(10,curiyr) = QGEIN(9,curiyr) * GEIN_SHR(curiyr) !109 Geothermal - Industrial
        QGEEL(10,curiyr) = QGEEL(9,curiyr) * GEEL_SHR(curiyr) !110 Geothermal - Electricity

        QBMRS(10,curiyr) = QBMRS(9,curiyr) * BMRS_SHR(curiyr) !112 Biomass - Residential
        QBMCM(10,curiyr) = QBMCM(9,curiyr) * BMCM_SHR(curiyr) !113 Biomass - Commercial
        QBMIN(10,curiyr) = QBMIN(9,curiyr) * BMIN_SHR(curiyr) !114 Biomass - Industrial
        QBMRF(10,curiyr) = QBMRF(9,curiyr) * BMRF_SHR(curiyr) !115 Biomass - Refinery
        QBMEL(10,curiyr) = QBMEL(9,curiyr) * BMEL_SHR(curiyr) !116 Biomass - Electricity
        QBMSN(10,curiyr) = QBMSN(9,curiyr) * BMSN_SHR(curiyr) !117 Biomass - Synthetics
        QBMHM(10,curiyr) = QBMHM(9,curiyr) * BMHM_SHR(curiyr) !118 Biomass - Hydrogen

        QMSIN(10,curiyr) = QMSIN(9,curiyr) * MSIN_SHR(curiyr) !120 Municipal Solid Waste - Industrial
        QMSEL(10,curiyr) = QMSEL(9,curiyr) * MSEL_SHR(curiyr) !121 Municipal Solid Waste - Electricity

        QSTRS(10,curiyr) = QSTRS(9,curiyr) * STRS_SHR(curiyr) !123 Solar Thermal - Residential
        QSTCM(10,curiyr) = QSTCM(9,curiyr) * STCM_SHR(curiyr) !124 Solar Thermal - Commercial
        QSTIN(10,curiyr) = QSTIN(9,curiyr) * STIN_SHR(curiyr) !125 Solar Thermal - Industrial
        QSTEL(10,curiyr) = QSTEL(9,curiyr) * STEL_SHR(curiyr) !126 Solar Thermal - Electricity

        QPVRS(10,curiyr) = QPVRS(9,curiyr) * PVRS_SHR(curiyr) !128 Photovoltaic - Residential
        QPVCM(10,curiyr) = QPVCM(9,curiyr) * PVCM_SHR(curiyr) !129 Photovoltaic - Commercial
        QPVIN(10,curiyr) = QPVIN(9,curiyr) * PVIN_SHR(curiyr) !130 Photovoltaic - Industrial
        QPVEL(10,curiyr) = QPVEL(9,curiyr) * PVEL_SHR(curiyr) !131 Photovoltaic - Electricity

        QWIIN(10,curiyr) = QWIIN(9,curiyr) * WIIN_SHR(curiyr) !133 Wind - Industrial
        QWIEL(10,curiyr) = QWIEL(9,curiyr) * WIEL_SHR(curiyr) !134 Wind - Electricity

        QEIEL(10,curiyr) = QEIEL(9,curiyr) * EIEL_SHR(curiyr) !144 Net Electricity Imports - Electricity
        QCIIN(10,curiyr) = QCIIN(9,curiyr) * CIIN_SHR(curiyr) !145 Net Coal Coke Imports - Industrial

      endif
      DO 20 IY=1,MNUMYR
         QPRRS(MNUMCR,IY) = 0.0
         QPRCM(MNUMCR,IY) = 0.0
         QPRTR(MNUMCR,IY) = 0.0
         QETIN(MNUMCR,IY) = 0.0
         QBUIN(MNUMCR,IY) = 0.0
         QPRIN(MNUMCR,IY) = 0.0
         QISIN(MNUMCR,IY) = 0.0
         QETINPF(MNUMCR,IY) = 0.0
         QBUINPF(MNUMCR,IY) = 0.0
         QPRINPF(MNUMCR,IY) = 0.0
         QISINPF(MNUMCR,IY) = 0.0
         INQLGHP(MNUMCR,IY) = 0.0
         INQLGPF(MNUMCR,IY) = 0.0
         QPROLENERF(MNUMCR,IY) = 0.0
         QAGTR(MNUMCR,IY) = 0.0
         QLUTR(MNUMCR,IY) = 0.0
         QPPIN(MNUMCR,IY) = 0.0
         QPPINPF(MNUMCR,IY) = 0.0
         QLUIN(MNUMCR,IY) = 0.0
         QLGIN(MNUMCR,IY) = 0.0
         QMGBS(MNUMCR,IY) = 0.0
         QJFBS(MNUMCR,IY) = 0.0
         QDSBS(MNUMCR,IY) = 0.0
         QDSTRHWY(MNUMCR,IY) = 0.0
         DO IC=1,MNUMCR-2
            QPRRS(MNUMCR,IY) = QPRRS(MNUMCR,IY) + QPRRS(IC,IY)
            QPRCM(MNUMCR,IY) = QPRCM(MNUMCR,IY) + QPRCM(IC,IY)
            QPRTR(MNUMCR,IY) = QPRTR(MNUMCR,IY) + QPRTR(IC,IY)
            QETIN(MNUMCR,IY) = QETIN(MNUMCR,IY) + QETIN(IC,IY)
            QBUIN(MNUMCR,IY) = QBUIN(MNUMCR,IY) + QBUIN(IC,IY)
            QPRIN(MNUMCR,IY) = QPRIN(MNUMCR,IY) + QPRIN(IC,IY)
            QISIN(MNUMCR,IY) = QISIN(MNUMCR,IY) + QISIN(IC,IY)
            QETINPF(MNUMCR,IY) = QETINPF(MNUMCR,IY) + QETINPF(IC,IY)
            QBUINPF(MNUMCR,IY) = QBUINPF(MNUMCR,IY) + QBUINPF(IC,IY)
            QPRINPF(MNUMCR,IY) = QPRINPF(MNUMCR,IY) + QPRINPF(IC,IY)
            QISINPF(MNUMCR,IY) = QISINPF(MNUMCR,IY) + QISINPF(IC,IY)
            INQLGHP(MNUMCR,IY) = INQLGHP(MNUMCR,IY) + INQLGHP(IC,IY)
            INQLGPF(MNUMCR,IY) = INQLGPF(MNUMCR,IY) + INQLGPF(IC,IY)
            QPROLENERF(MNUMCR,IY) = QPROLENERF(MNUMCR,IY) + QPROLENERF(IC,IY)
            QLGIN(MNUMCR,IY) = QLGIN(MNUMCR,IY) + QLGIN(IC,IY)
            QAGTR(MNUMCR,IY) = QAGTR(MNUMCR,IY) + QAGTR(IC,IY)
            QLUTR(MNUMCR,IY) = QLUTR(MNUMCR,IY) + QLUTR(IC,IY)
            QPPIN(MNUMCR,IY) = QPPIN(MNUMCR,IY) + QPPIN(IC,IY)
            QPPINPF(MNUMCR,IY) = QPPINPF(MNUMCR,IY) + QPPINPF(IC,IY)
            QLUIN(MNUMCR,IY) = QLUIN(MNUMCR,IY) + QLUIN(IC,IY)
            QMGBS(MNUMCR,IY) = QMGBS(MNUMCR,IY) + QMGBS(IC,IY)
            QJFBS(MNUMCR,IY) = QJFBS(MNUMCR,IY) + QJFBS(IC,IY)
            QDSBS(MNUMCR,IY) = QDSBS(MNUMCR,IY) + QDSBS(IC,IY)
            QDSTRHWY(MNUMCR,IY) = QDSTRHWY(MNUMCR,IY) + QDSTRHWY(IC,IY)
         ENDDO
         DO 10 IC=1,MNUMCR
            QELAS(IC,IY)=QELRS(IC,IY)+QELCM(IC,IY)+QELTR(IC,IY)+QELIN(IC,IY)
            QGFAS(IC,IY)=QGFRS(IC,IY)+QGFCM(IC,IY)+QGFTR(IC,IY)+QGFIN(IC,IY)+QGFEL(IC,IY)
            QGIAS(IC,IY)=QGIRS(IC,IY)+QGICM(IC,IY)+QGITR(IC,IY)+QGIIN(IC,IY)+QGIEL(IC,IY)
            QNGAS(IC,IY)=QNGRS(IC,IY)+QNGCM(IC,IY)+QNGTR(IC,IY)+QNGIN(IC,IY)+QNGEL(IC,IY)
            QCLAS(IC,IY)=QCLRS(IC,IY)+QCLCM(IC,IY)+QCLIN(IC,IY)+QCLEL(IC,IY)
            QMGAS(IC,IY)=QMGCM(IC,IY)+QMGTR(IC,IY)+QMGIN(IC,IY)
            QDSAS(IC,IY)=QDSRS(IC,IY)+QDSCM(IC,IY)+QDSTR(IC,IY)+QDSIN(IC,IY)+QDSEL(IC,IY)
            QKSAS(IC,IY)=QKSRS(IC,IY)+QKSCM(IC,IY)+QKSIN(IC,IY)
            QLGAS(IC,IY)=QLGRS(IC,IY)+QLGCM(IC,IY)+QLGTR(IC,IY)+QLGIN(IC,IY)
            QRLAS(IC,IY)=QRLCM(IC,IY)+QRLTR(IC,IY)+QRLIN(IC,IY)+QRLEL(IC,IY)
            QRHAS(IC,IY)=QRHTR(IC,IY)+QRHEL(IC,IY)
!            QRSCM(IC,IY)=QRLCM(IC,IY)
!            QRSIN(IC,IY)=QRLIN(IC,IY)
            QRSTR(IC,IY)=QRLTR(IC,IY)+QRHTR(IC,IY)
            QRSEL(IC,IY)=QRLEL(IC,IY)+QRHEL(IC,IY)
            QRSAS(IC,IY)=QRSCM(IC,IY)+QRSTR(IC,IY)+QRSIN(IC,IY)+QRSEL(IC,IY)
            QPCAS(IC,IY)=QPCIN(IC,IY)+QPCEL(IC,IY)
            QOTAS(IC,IY)=QOTTR(IC,IY)+QOTIN(IC,IY)
            QHOAS(IC,IY)=QHOIN(IC,IY)+QHOEL(IC,IY)
            QGEAS(IC,IY)=QGEIN(IC,IY)+QGEEL(IC,IY)
            QBMAS(IC,IY)=QBMRS(IC,IY)+QBMCM(IC,IY)+QBMIN(IC,IY)+QBMEL(IC,IY)+QBMSN(IC,IY)
            QMSAS(IC,IY)=QMSIN(IC,IY)+QMSEL(IC,IY)
            QSTAS(IC,IY)=QSTRS(IC,IY)+QSTCM(IC,IY)+QSTIN(IC,IY)+QSTEL(IC,IY)
            QPVAS(IC,IY)=QPVRS(IC,IY)+QPVCM(IC,IY)+QPVIN(IC,IY)+QPVEL(IC,IY)
            QWIAS(IC,IY)=QWIIN(IC,IY)+QWIEL(IC,IY)

! +++ Total Residential:
            QTPRS(IC,IY)=QDSRS(IC,IY)+QKSRS(IC,IY)+QLGRS(IC,IY)
            QTRRS(IC,IY)=QBMRS(IC,IY)+QSTRS(IC,IY)+QPVRS(IC,IY)+QGERS(IC,IY)
            QTSRS(IC,IY)=QELRS(IC,IY)+QNGRS(IC,IY)+QCLRS(IC,IY)+QTPRS(IC,IY)+QTRRS(IC,IY)

! +++ Total Commercial:
            QTPCM(IC,IY)=QMGCM(IC,IY)+QDSCM(IC,IY)+QKSCM(IC,IY)+QLGCM(IC,IY)+QRSCM(IC,IY)
            QTRCM(IC,IY)=QBMCM(IC,IY)+QSTCM(IC,IY)+QPVCM(IC,IY)
            QTSCM(IC,IY)=QELCM(IC,IY)+QNGCM(IC,IY)+QCLCM(IC,IY)+QTPCM(IC,IY)+QTRCM(IC,IY)

! +++ Total Transportation:
            QTPTR(IC,IY)=QMGTR(IC,IY)+QJFTR(IC,IY)+QDSTR(IC,IY)+QLGTR(IC,IY)+QRSTR(IC,IY)+QOTTR(IC,IY)
            QTRTR(IC,IY)=QETTR(IC,IY)
            QTSTR(IC,IY)=QELTR(IC,IY)+QNGTR(IC,IY)+QGPTR(IC,IY)+QTPTR(IC,IY)+ &
                         QMETR(IC,IY)+QETTR(IC,IY)+QH2TR(IC,IY)+QNGLQ(IC,IY)

! +++ Total Refinery:
            QTPRF(IC,IY)=QDSRF(IC,IY)+QLGRF(IC,IY)+QRSRF(IC,IY)+QSGRF(IC,IY)+QPCRF(IC,IY)+QCCRF(IC,IY)+QOTRF(IC,IY)
            QTSRF(IC,IY)=QELRF(IC,IY)+QNGRF(IC,IY)+QCLRF(IC,IY)+QTPRF(IC,IY)+QBMRF(IC,IY)

! +++ Total Industrial:
            QTPIN(IC,IY)=QMGIN(IC,IY)+QDSIN(IC,IY)+QKSIN(IC,IY)+QLGIN(IC,IY)+QRSIN(IC,IY)+ &
                         QPFIN(IC,IY)+QSGIN(IC,IY)+QPCIN(IC,IY)+QASIN(IC,IY)+QOTIN(IC,IY)
            QTRIN(IC,IY)=QHOIN(IC,IY)+QGEIN(IC,IY)+QBMIN(IC,IY)+QMSIN(IC,IY)+QSTIN(IC,IY)+ &
                         QPVIN(IC,IY)+QWIIN(IC,IY)
            QTSIN(IC,IY)=QELIN(IC,IY)+QNGIN(IC,IY)+QLPIN(IC,IY)+  &
                         QCLIN(IC,IY)+QMCIN(IC,IY)+QCIIN(IC,IY)+&
                         QTPIN(IC,IY)+QTRIN(IC,IY)

! +++ Total Electric Utility:
            QTPEL(IC,IY)=QDSEL(IC,IY)+QRSEL(IC,IY)+QPCEL(IC,IY)
            QTREL(IC,IY)=QHOEL(IC,IY)+QGEEL(IC,IY)+QBMEL(IC,IY)+QMSEL(IC,IY)+QSTEL(IC,IY)+ &
                         QPVEL(IC,IY)+QWIEL(IC,IY)
            QTSEL(IC,IY)=QNGEL(IC,IY)+QCLEL(IC,IY)+QTPEL(IC,IY)+QUREL(IC,IY)+QTREL(IC,IY)+QEIEL(IC,IY)+ &
                         QH2EL(IC,IY)

! +++ Total Hydrogen Conversion:
            !QTRHM(IC,IY)=QETHM(IC,IY)+QBMHM(IC,IY)
            !QTSHM(IC,IY)=QELHM(IC,IY)+QNGHM(IC,IY)+QCLHM(IC,IY)+QETHM(IC,IY)+QURHM(IC,IY)+QBMHM(IC,IY)

! +++ Total Synthetics:
            QTRSN(IC,IY)=QBMSN(IC,IY)
            QTSSN(IC,IY)=QTRSN(IC,IY)

! +++ Sum ALL-SECTOR variables for TOTAL PETROLEUM, TOTAL RENEWABLE, and TOTAL SECTOR
            QTPAS(IC,IY)=QTPRS(IC,IY)+QTPCM(IC,IY)+QTPTR(IC,IY)+QTPIN(IC,IY)+QTPEL(IC,IY)
            QTRAS(IC,IY)=QTRRS(IC,IY)+QTRCM(IC,IY)+QTRTR(IC,IY)+QTRIN(IC,IY)+QTREL(IC,IY)+QTRSN(IC,IY)
            QTSAS(IC,IY)=QTSRS(IC,IY)+QTSCM(IC,IY)+QTSTR(IC,IY)+QTSIN(IC,IY)+QTSEL(IC,IY)+ &
                         QTSSN(IC,IY)-QELAS(IC,IY)
10       CONTINUE
20    CONTINUE

! +++ Sum NEMS QUANTITIES Across Regions for the current year.
      DO IVAR=1,MNUMQ
         CALL SUMARY(MQTY(1,CURIYR,IVAR),MNUMCR)
      ENDDO

      RETURN
      END

!******************************************************************
!*  Subroutine SUMARY(ARRAY,N)
!*
!*  Sums the N-1 elements of an array into a total stored as the nth
!*  element.  The length of the array is set by the calling program.
!******************************************************************
      SUBROUTINE SUMARY(ARRAY,N)
      IMPLICIT NONE

      INTEGER N,I
      REAL ARRAY(N)

!WRITE(*,*)"now in SUMQAS.summary()"
      ARRAY(N)=0.
      DO 10 I=1,N-2
         ARRAY(N)=ARRAY(N)+ARRAY(I)
10    CONTINUE

      RETURN
      END
