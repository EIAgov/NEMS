SUBROUTINE NEXEC_CALL_REFINE
    IMPLICIT NONE
    
    include 'parametr'
    include 'emmparm'
    include 'ncntrl'
    include 'macout'
    include 'qblk'
    include 'wrenew'
    include 'indrep'
    include 'bifurc'
    include 'pmmout'
    include 'convfact'
    include 'lfmmout'

    INTEGER IR
    INTEGER IVAR
    REAL TREFCON(20,11)
    real sELeth(9),sNGeth(9),sCLeth(9)  ! backup regional shares for missing regional ethanol quantities

    integer map_refreg_to_cenreg(refreg)     !  final refreg is Caribbean/Canada, but just in case...
    data map_refreg_to_cenreg / 2, 7, 3, 7, 7, 8, 9, 9, 9/
    
    INTEGER DIVS(5,2)/1,3,5,8,11, &  ! Census Division start for each Region and US
                      2,4,7,9,11/    ! Census Division end   for each Region and US


    do ir=1,11
        TREFCON(1,ir)= QELRF(ir,curiyr) + qELeth(curiyr,ir) ! electricity
        TREFCON(2,ir)= QNGRF(ir,curiyr) + qNGeth(curiyr,ir)!+qGTLRF(ir,curiyr) ! nat gas h&p&GTL losses
        TREFCON(3,ir)= QCLRF(ir,curiyr) + qCLeth(curiyr,ir) ! +qCTLRF(ir,curiyr) ! steam coal and CTL losses
        TREFCON(6,ir)= QRLRF(ir,curiyr) ! residual
        TREFCON(7,ir)= QDSRF(ir,curiyr) ! distillate
        TREFCON(8,ir)= QLGRF(ir,curiyr) ! lpg h&p
        TREFCON(10,ir)=QSGRF(ir,curiyr) ! still gas
        TREFCON(11,ir)=QPCRF(ir,curiyr) + qCCRF(ir,curiyr) ! petroleum coke
        TREFCON(15,ir)=QOTRF(ir,curiyr) ! other petroleum
        TREFCON(18,ir)=QBMRF(ir,curiyr) ! biomass
		TREFCON(19,ir)=QH2RF(ir,curiyr) ! H2 feedstock
		TREFCON(20,ir)=0.0				! H2 heat and power placeholder
    enddo

    CALL REFINE

    sELeth(1:9)=(/0.,0.,0.31,0.69,0.,0.,0.,0.,0./)  !  backup regional shares
    sNGeth(1:9)=(/0.,0.,0.30,0.70,0.,0.,0.,0.,0./)
    sCLeth(1:9)=(/0.,0.,0.28,0.72,0.,0.,0.,0.,0./)

    if(sum(qNGeth(curiyr,1:9)).eq.0.0) qNGeth(curiyr,1:9)=qNGeth(curiyr,11)*sNGeth(1:9)
    if(sum(qELeth(curiyr,1:9)).eq.0.0) qELeth(curiyr,1:9)=qELeth(curiyr,11)*sELeth(1:9)
    if(sum(qCLeth(curiyr,1:9)).eq.0.0) qCLeth(curiyr,1:9)=qCLeth(curiyr,11)*sCLeth(1:9)

! assign new national totals
    DO IVAR=1,MNUMQ
        CALL SUMARY(MQTY(1,CURIYR,IVAR),MNUMCR)
    ENDDO

! 1) Subtract what was previously added to Q*IN from Last iteration
! 2) Add back in current values for Q*RF
! 3) Reset reporting variables
    DO IR=1,11
        IF (IR .EQ. 10) CYCLE
    !     1)
        QELIN(IR,curiyr)=QELIN(IR,curiyr)-TREFCON(1,IR)
        QNGIN(IR,curiyr)=QNGIN(IR,curiyr)-TREFCON(2,IR)
        QCLIN(IR,curiyr)=QCLIN(IR,curiyr)-TREFCON(3,IR)
        QRLIN(IR,curiyr)=QRLIN(IR,curiyr)-TREFCON(6,IR)  ! low sulfur reside
        QRSIN(IR,curiyr)=QRSIN(IR,curiyr)-TREFCON(6,IR)  ! total resid
        QDSIN(IR,curiyr)=QDSIN(IR,curiyr)-TREFCON(7,IR)
        QLGIN(IR,curiyr)=QLGIN(IR,curiyr)-TREFCON(8,IR)
        QSGIN(IR,curiyr)=QSGIN(IR,curiyr)-TREFCON(10,IR)
        QPCIN(IR,curiyr)=QPCIN(IR,curiyr)-TREFCON(11,IR)
        QOTIN(IR,curiyr)=QOTIN(IR,curiyr)-TREFCON(15,IR)
        QBMIN(IR,curiyr)=QBMIN(IR,curiyr)-TREFCON(18,IR)
		QH2INPF(IR,curiyr)=QH2INPF(IR,curiyr)-TREFCON(19,IR)
		QH2INHP(IR,curiyr)=QH2INHP(IR,curiyr)-TREFCON(20,IR)	! 0, but adding in case refining ever adds H2 heat and power
		QH2IN(IR,curiyr)=QH2INPF(IR,curiyr)+QH2INHP(IR,curiyr)
		
    !     2)
        QELIN(IR,curiyr)=QELIN(IR,curiyr)+QELRF(IR,curiyr)+qELeth(curiyr,ir)
        QNGIN(IR,curiyr)=QNGIN(IR,curiyr)+QNGRF(IR,curiyr)+qNGeth(curiyr,ir)!+QGTLRF(IR,curiyr)
        QGIIN(ir,curiyr)=QNGIN(IR,curiyr)-QGFIN(ir,curiyr)                    ! so they add up
        QCLIN(IR,curiyr)=QCLIN(IR,curiyr)+QCLRF(IR,curiyr)+qCLeth(curiyr,ir) ! +QCTLRF(IR,curiyr)
        QRLIN(IR,curiyr)=QRLIN(IR,curiyr)+QRLRF(IR,curiyr)
        QRSIN(IR,curiyr)=QRSIN(IR,curiyr)+QRSRF(IR,curiyr)
        QDSIN(IR,curiyr)=QDSIN(IR,curiyr)+QDSRF(IR,curiyr)
        QLGIN(IR,curiyr)=QLGIN(IR,curiyr)+QLGRF(IR,curiyr)
        QSGIN(IR,curiyr)=QSGIN(IR,curiyr)+QSGRF(IR,curiyr)
        QPCIN(IR,curiyr)=QPCIN(IR,curiyr)+QPCRF(IR,curiyr)  +qCCRF(IR,curiyr)
        QOTIN(IR,curiyr)=QOTIN(IR,curiyr)+QOTRF(IR,curiyr)
        QBMIN(IR,curiyr)=QBMIN(IR,curiyr)+QBMRF(IR,curiyr)
		QH2INPF(IR,curiyr)=QH2INPF(IR,curiyr)+QH2RF(IR,curiyr)
		QH2IN(IR,curiyr)=QH2INPF(IR,curiyr)+QH2INHP(IR,curiyr)
    ENDDO

    Qe2NGIN(11,curiyr,1)=Qe2NGIN(11,curiyr,1)-TREFCON( 2,11)
	Qe2H2IN(11,curiyr,1)=Qe2H2IN(11,curiyr,1)-TREFCON(19,11)
    Qe2CLIN(11,curiyr,1)=Qe2CLIN(11,curiyr,1)-TREFCON( 3,11)
    Qe2RSIN(11,curiyr,1)=Qe2RSIN(11,curiyr,1)-TREFCON( 6,11)
    Qe2DSIN(11,curiyr,1)=Qe2DSIN(11,curiyr,1)-TREFCON( 7,11)
    Qe2LGIN(11,curiyr,1)=Qe2LGIN(11,curiyr,1)-TREFCON( 8,11)
    Qe2SGIN(11,curiyr,1)=Qe2SGIN(11,curiyr,1)-TREFCON(10,11)
    Qe2PCIN(11,curiyr,1)=Qe2PCIN(11,curiyr,1)-TREFCON(11,11)
    Qe2OTIN(11,curiyr,1)=Qe2OTIN(11,curiyr,1)-TREFCON(15,11)
	

    Qe2NGIN(11,curiyr,1)=Qe2NGIN(11,curiyr,1)+QNGRF(11,curiyr)+qNGeth(curiyr,11)
	Qe2H2IN(11,curiyr,1)=Qe2H2IN(11,curiyr,1)+QH2RF(11,curiyr)
    Qe2CLIN(11,curiyr,1)=Qe2CLIN(11,curiyr,1)+QCLRF(11,curiyr)+qCLeth(curiyr,11)
    Qe2RSIN(11,curiyr,1)=Qe2RSIN(11,curiyr,1)+QRSRF(11,curiyr)
    Qe2DSIN(11,curiyr,1)=Qe2DSIN(11,curiyr,1)+QDSRF(11,curiyr)
    Qe2LGIN(11,curiyr,1)=Qe2LGIN(11,curiyr,1)+QLGRF(11,curiyr)
    Qe2SGIN(11,curiyr,1)=Qe2SGIN(11,curiyr,1)+QSGRF(11,curiyr)
    Qe2PCIN(11,curiyr,1)=Qe2PCIN(11,curiyr,1)+QPCRF(11,curiyr)+qCCRF(11,curiyr)
    Qe2OTIN(11,curiyr,1)=Qe2OTIN(11,curiyr,1)+QOTRF(11,curiyr)

    !    3) update Table 35 values
    DO IR=1,5            ! 4 census regions and 5:US
        REFCON(1,ir,curiyr)= SUM(QELRF(DIVS(ir,1):DIVS(ir,2),curiyr))+SUM(QELeth(curiyr,DIVS(ir,1):DIVS(ir,2))) ! electricity
        REFCON(2,ir,curiyr)= SUM(QNGRF(DIVS(ir,1):DIVS(ir,2),curiyr))+SUM(QNGeth(curiyr,DIVS(ir,1):DIVS(ir,2)))+SUM(QGTLRF(DIVS(ir,1):DIVS(ir,2),curiyr)) ! nat gas h&p
        REFCON(3,ir,curiyr)= SUM(QCLRF(DIVS(ir,1):DIVS(ir,2),curiyr))+SUM(QCLeth(curiyr,DIVS(ir,1):DIVS(ir,2))) ! steam coal
        REFCON(6,ir,curiyr)= SUM(QRLRF(DIVS(ir,1):DIVS(ir,2),curiyr))      ! residual
        REFCON(7,ir,curiyr)= SUM(QDSRF(DIVS(ir,1):DIVS(ir,2),curiyr))      ! distillate
        REFCON(8,ir,curiyr)= SUM(QLGRF(DIVS(ir,1):DIVS(ir,2),curiyr))      ! lpg h&p
        REFCON(10,ir,curiyr)=SUM(QSGRF(DIVS(ir,1):DIVS(ir,2),curiyr))      ! still gas
        REFCON(11,ir,curiyr)=SUM(QPCRF(DIVS(ir,1):DIVS(ir,2),curiyr)) + &  ! petroleum coke, Marketable
                             SUM(QCCRF(DIVS(ir,1):DIVS(ir,2),curiyr))      ! petroleum coke, cat coke
        REFCON(15,ir,curiyr)=SUM(QOTRF(DIVS(ir,1):DIVS(ir,2),curiyr))      ! other petroleum
        REFCON(18,ir,curiyr)=SUM(QBMRF(DIVS(ir,1):DIVS(ir,2),curiyr))      ! biomass
		REFCON(19,ir,curiyr)=SUM(QH2RF(DIVS(ir,1):DIVS(ir,2),curiyr))	   ! H2 feedstock
    ENDDO

    RETURN
END SUBROUTINE NEXEC_CALL_REFINE