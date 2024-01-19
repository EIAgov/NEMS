module ctus_mod
! Module that packages CTUS data and routines in a single place

implicit none
private
public read_co2, set_base_co2_volumes_available, co2cost_calc

logical :: is_ctus_logging = .true.

contains


subroutine write_log(msg)
! Write a log message to the log
!
! This subroutine writes a given string message ('msg') to the log,
! with a datetime prefix. These log messages can be turned on/off in bulk by
! defining the 'is_ctus_logging' boolean.
!
! Usage:
!   character(len=1000) :: log_msg
!   log_msg = 'this is a test'
!   call write_log(trim(log_msg))

implicit none

character(len = *) :: msg
character(len=8) :: d
character(len=10) :: t

if (is_ctus_logging) then
    call date_and_time(date=d, time=t)
    write (6,*) '['//d//':'//t//']'//' :: '//'|'//msg//'|'
end if

end subroutine write_log


SUBROUTINE READ_CO2
! Read contents of "wlco2.txt" file and process CO2 price/supply curve

implicit none

include 'parametr'     ! nems dimension parameters
include 'ncntrl'       ! nems control variables
include 'ogsmparm'     ! ogsm parameter file
include 'ogsmbfw'      ! ogsm system variables
include 'ogsml48'
include 'emmparm'
include 'ecpcntl'
include 'uecpout'
include 'macout'
include 'ogsmout'
include 'tcs45q'

REAL*8      T_Price 
REAL*8      T_Quantity 
REAL*8      FRACTION
REAL*8      HOLDIT(MAX_IND_CO2) 
REAL*8      CCS_EOR_87_EFD2(MNUMYR)
INTEGER*4   USETAX(max_reg,max_ind_co2,MNUMYR),EYR
INTEGER     SINDEX(MAX_IND_CO2)
INTEGER*4   T_INDX, J_INDX, I_INDX 
INTEGER*4   T_REG 
INTEGER*4   T_ID 
INTEGER*4   T_SRC_INDX 
INTEGER*4   T_BIN
INTEGER*4   F_WLCO2
CHARACTER*8 T_SOURCE 

! open constraints file

INTEGER   RGN                              ! REGION VARIABLE
INTEGER   YR                               ! YEAR INDEX VARIABLE
INTEGER   SRC                              ! CO2 SOURCE INDEX VARIABLE
INTEGER   RUN45Q                           ! RUN with 45Q, 0=>No
INTEGER   RTOVALUE

call write_log('[CTUS][READ_CO2] Start.')

FNAME = 'WLCO2'
NEW = .FALSE.
F_WLCO2 = FILE_MGR('O',FNAME,NEW)
USETAX = 1

RUN45Q = RTOVALUE('RUN45Q  ',0)

! section 4:  co2 constraints

CALL OGSM_NEXTDATA(F_WLCO2)
READ(F_WLCO2,*) NAT_SRCC02               ! USE NATURAL SOURCE OF CO2?
READ(F_WLCO2,*) IND_SRCC02               ! USE INDUSTRIAL SOURCE OF CO2?

! base co2 volumes for natural sources only (bcf/yr)

CALL OGSM_NEXTDATA(F_WLCO2)
src = 4
READ(F_WLCO2,*)(CO2BASE(RGN,SRC),rgn=1,max_reg-1)

CALL OGSM_NEXTDATA(F_WLCO2)

! read in bin end points

READ(F_WLCO2,*) (BIN_DOLAMT(T_INDX),T_INDX=1,8)

CALL OGSM_NEXTDATA(F_WLCO2)

NS_ID = 0
NS_PRICE = 0.0
NS_QUANT = 0.0
NS_SRC = 0
NS_SYR = 0
NS_BIN = 0
N_NS_RECS = 0
NS_PRC_NDX = 0.0

DO I_INDX = 1 , MAX_IND_CO2 * max_reg
    READ(F_WLCO2,*) T_INDX, T_REG, T_ID, T_SRC_INDX, T_SOURCE, T_Price, T_Quantity, T_BIN
    IF (T_INDX .GT. 0) THEN
        N_NS_RECS(T_REG) = N_NS_RECS(T_REG) + 1
        J_INDX = N_NS_RECS(T_REG)
        NS_ID(T_REG,J_INDX) = T_ID
        NS_PRICE(T_REG,J_INDX,1) = T_PRICE
        NS_QUANT(T_REG,J_INDX) = T_QUANTITY
        NS_BIN(T_REG,J_INDX,1) = T_BIN
        NS_SRC(T_REG,J_INDX) = T_SRC_INDX
        DO YR = 1,MNUMYR
            NS_BIN(T_REG,J_INDX,YR) = NS_BIN(T_REG,J_INDX,1)
            NS_PRICE(T_REG,J_INDX,YR) = NS_PRICE(T_REG,J_INDX,1) 
        END DO
    ELSE
        EXIT
    END IF
END DO

IF (RUN45Q .EQ. 0) THEN
    USETAX = 0
ELSE
    DO T_REG=1,MAX_REG
        DO J_INDX = 1 , N_NS_RECS(T_REG)
            DO YR = 1 , I_45Q_SYR - 1990
                USETAX(T_REG,J_INDX,YR) =  0
            end do
            DO YR = I_45Q_LYR_RET - 1988, LASTYR
                USETAX(T_REG,J_INDX,YR) =  0
            end do
        end do
    end do
END IF

IF (RUN45Q .GT. 0) THEN
    DO T_REG=1,MAX_REG
        DO J_INDX = 1 , N_NS_RECS(T_REG)
            IF (NS_START(T_REG,J_INDX) .NE. 0 .AND. NS_START(T_REG,J_INDX) + 1989 .LE. I_45Q_LYR_RET) THEN
                EYR = MIN(LASTYR, NS_START(T_REG,J_INDX) + I_45Q_Duration -1)
                DO YR = I_45Q_SYR - 1989, EYR
                    USETAX(T_REG,J_INDX,YR) =  1
                end do
                DO YR = EYR + 1, LASTYR
                    USETAX(T_REG,J_INDX,YR) =  0
                end do
            END IF
        end do
    end do
END IF

DO YR = 1, MNUMYR
    IF (YR + 1989 .LE. I_45Q_LYR_RET + I_45Q_Duration - 1 .AND. RUN45Q .GT. 0) THEN
        CCS_EOR_87_EFD2(YR) = CCS_EOR_45Q(YR)  / (1.0 - FEDTXR) * MC_JPGDP(2008-1989) 
    ELSE
        CCS_EOR_87_EFD2(YR) = 0.0
    END IF
end do

DO T_REG=1,MAX_REG
    do YR = 1, MNUMYR
        FRACTION = 0.0
        IF (YR + 1989 .GE. I_45Q_SYR .AND. YR + 1989 .LE. I_45Q_LYR_RET) THEN
            FRACTION = 1.0
        ELSE 
            FRACTION = MAX(DBLE(0.0) , (DBLE(I_45Q_Duration - (YR + 1989 - I_45Q_LYR_RET))) / DBLE(I_45Q_Duration))
        END IF

        IF (N_NS_RECS(T_REG).gt.0) THEN
            HOLDIT = 0.0
            DO J_INDX = 1 , N_NS_RECS(T_REG)
                IF (USETAX(T_REG,J_INDX,YR).EQ.1) THEN 
                    NS_PRICE(T_REG,J_INDX,yr) = NS_PRICE(T_REG,J_INDX,yr) - CCS_EOR_87_EFD2(YR)
                ENDIF
                HOLDIT(J_INDX) = NS_PRICE(T_REG,J_INDX,yr)
                SINDEX(J_INDX) = J_INDX
            end do
              
            CALL WELLHEAPSORT(N_NS_RECS(T_REG),HOLDIT,SINDEX)
   
            DO J_INDX = 1 , N_NS_RECS(T_REG)
                NS_PRC_NDX(T_REG,J_INDX,YR) = SINDEX(J_INDX)
            end do
              
            DO J_INDX = 1 , N_NS_RECS(T_REG)
                NS_BIN(T_REG,J_INDX,YR) = 8
                DO T_INDX = 1, 7
                    IF (NS_PRICE(T_REG,J_INDX,YR) .LT. BIN_DOLAMT(T_INDX) - FRACTION * CCS_EOR_87_EFD2(YR) ) THEN
                        NS_BIN(T_REG,J_INDX,YR)= T_INDX
                        EXIT
                    end if
                end do
            end do
        END IF
    end do
end do

CALL OGSM_NEXTDATA(F_WLCO2)
! co2 avail multiplier
READ(F_WLCO2,*)CO2MUL

! development levers to be applied to existing co2 sources

CALL OGSM_NEXTDATA(F_WLCO2)
DO SRC = 1,max_src
    ! yrs to develop technology
    READ(F_WLCO2,*)YRDT(SRC)
end do

CALL OGSM_NEXTDATA(F_WLCO2)
DO SRC = 1,max_src
    ! yrs to develop infrastructure
    READ(F_WLCO2,*)YRDI(SRC)
end do

CALL OGSM_NEXTDATA(F_WLCO2)
DO SRC = 1,max_src
    ! yrs to reach full capacity
    READ(F_WLCO2,*)YRMA(SRC)
end do

CALL OGSM_NEXTDATA(F_WLCO2)
DO SRC = 1,max_src
    READ(F_WLCO2,*)(AARP(YR,SRC),YR=1,10)
end do

CALL OGSM_NEXTDATA(F_WLCO2)
DO SRC = 1,max_src
    ! ultimate market acceptance
    READ(F_WLCO2,*)UMPCO2(SRC)
end do

F_WLCO2 = FILE_MGR('C',FNAME,.FALSE.)

NS_START = 0

call write_log('[CTUS][READ_CO2] End.')
RETURN
END SUBROUTINE READ_CO2


subroutine set_base_co2_volumes_available
! Set base CO2 volumes available

! The following steps are taken for each source and each region:
!            if the year is less than the RNDyr, CO2 = 0.0
!            if the year is less than the RNDyr+INFyr CO2 = 0.0
!              if the year is between RNDyr+INFyr and RNDyr+INFyr+MARyr, 
!                 CO2 = (ultimate volume)*(anmar/100.0)*(ump/100.0) for the source and region
!              if the year is greater than RNDyr+INFyr+MARyr, 
!                 CO2 = (ultimate volume)*(ump/100.0)

implicit none

include 'parametr'     ! nems dimension parameters
include 'ogsmparm'
include 'ogsml48'
include 'ogsmbfw'
include 'ctus'

integer ireg, isrc, iyr, irec, jrec, stop1, stop2, stop3, nems_yr, jbin, ibin
real*8 target_co2

integer cotype2(max_src)
real camar(max_yr)
real co2avl(max_reg,max_src,max_yr)

character*40 source_label(max_src), source_label2(max_src)

source_label(1) = 'HYDROGEN                             '   
source_label(2) = 'AMMONIA                              '  
source_label(3) = 'ETHANOL                              ' 
source_label(4) = 'NATURAL                              '
source_label(5) = 'CEMENT                               '
source_label(6) = 'HYDROGEN REFINERIES                  '
source_label(7) = 'PLANNED POWER PLANTS                 '
source_label(8) = 'POWER PLANTS                         '   
source_label(9)=  'NATURAL GAS PROCESSING               '  
source_label(10)= 'PLANNED XTL                          '  
source_label(11)= 'XTL                                  '
source_label(12)= 'NEW SOURCE 1                         ' 
source_label(13)= 'NEW SOURCE 2                         '

data cotype2(1:13)  /0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/

call write_log('[CTUS][set_base_co2_volumes_available] Start.')

! start existing CO2 source loop
! Establish co2base with NETL data

DO IREG = 1 , MAX_REG - 1
    DO ISRC = 1, max_src
        IF (ISRC .NE. 4) THEN
            ! Not Natural, Industrial source from NETL, Power from EMM, XTL from LFMM
            CO2BASE(IREG,ISRC) = 0.0
        END IF
        DO iyr = 1, max_yr
            nat_availco2(IREG,ISRC,iyr) = 0.0
            src_availco2(IREG,ISRC,iyr) = 0.0
        END DO
    END DO
END DO

DO IREG = 1 , MAX_REG - 1
    DO IREC = 1 , N_NS_RECS(IREG)
        JREC = NS_PRC_NDX(IREG,IREC,iyr)
        ISRC = NS_SRC(IREG,jREC)
        IF (ISRC .NE. 4) THEN
            ! Not Natural, Industrial source from NETL, Power from EMM, XTL from LFMM
            co2base(IREG,ISRC) = co2base(IREG,ISRC) + NS_QUANT(IREG,jREC) * 18.0
        END IF
    END DO
END DO

do ISRC = 1, max_src

    stop1 = 0
    stop2 = 0
    stop3 = 0

    stop1 = yrdt(ISRC)                                  !R&D completed
    stop2 = stop1 + yrdi(ISRC)                          !Infrastructure completed
    stop3 = stop2 + yrma(ISRC)                          !MarIREGet Developed

    ! calculate cumulative annual market development
    do i = 1, 10
        camar(i) = 0.0
        if (i == 1) then
            camar(i) = camar(i) + (aarp(i,ISRC) / 100.0)
        else
            camar(i) = camar(i-1) + (aarp(i,ISRC) / 100.0)
        end if
    end do

    ! IREG   - region
    ! ISRC   - CO2 source
    ! iyr - year
    do IREG = 1 , max_reg-1 !start regional loop  

        ! only assign values for industrial sources
        IF (ISRC /= 4 .AND. ISRC /= 7 .AND. ISRC /= 8 .AND. ISRC /= 10 .AND. ISRC /= 11) THEN
            do iyr = 1, max_yr
                NEMS_YR = MIN(iyr + L48B4YR - BASEYR, MNUMYR)

                ! nat_availco2 and src_availco2 have already been initilized to zero

                if (iyr .gt. stop2 .and. iyr .le. stop3) then
                    ! during the marIREGet penetration phase
                    target_co2 = camar(iyr-stop2) * (umpco2(ISRC)/100.0) * co2base(IREG,ISRC)
                    DO IREC = 1 , N_NS_RECS(IREG)
                        JREC = NS_PRC_NDX(IREG,IREC,NEMS_YR)                     
                        IF (ISRC .EQ. NS_SRC(IREG,jREC) .AND. target_co2 .GE. 0.0 .AND. NS_QUANT(IREG,jREC) .GT. 0.0) THEN
                            JBIN = NS_BIN(IREG,jREC,NEMS_YR)
                            IBIN = MAP_BINS_TO_SRC_INDEX(JBIN)
                            nat_availco2(IREG,IBIN,iyr) = nat_availco2(IREG,IBIN,iyr) + NS_QUANT(IREG,jREC) * 18.0
                            src_availco2(IREG,ISRC,iyr) = src_availco2(IREG,ISRC,iyr) + NS_QUANT(IREG,jREC) * 18.0
                            IF (NS_SYR(IREG,jREC) .EQ. 0) NS_SYR(IREG,jREC) = iyr
                            target_co2 = target_co2 - NS_QUANT(IREG,jREC) * 18.0
                            cregpryr(IREG,IBIN,iyr) = MAX(cregpryr(IREG,IBIN,iyr) , NS_PRICE(IREG,jREC,NEMS_YR) / 18.0)
                            cregpryrS(IREG,ISRC,iyr) = MIN(cregpryrS(IREG,ISRC,iyr) , NS_PRICE(IREG,jREC,NEMS_YR) / 18.0)
                        END IF
                    END DO
                end if

                if (iyr.gt.stop3) then
                    ! after marIREGet saturation has been reached
                    target_co2 = (umpco2(ISRC)/100.0) * co2base(IREG,ISRC)
                    DO jREC = 1 , N_NS_RECS(IREG)
                        IF (ISRC .EQ. NS_SRC(IREG,jREC) .AND. target_co2 .GE. 0.0 .AND. NS_QUANT(IREG,jREC) .GT. 0.0) THEN
                            JBIN = NS_BIN(IREG,jREC,NEMS_YR)
                            IBIN = MAP_BINS_TO_SRC_INDEX(JBIN)
                            nat_availco2(IREG,IBIN,iyr) = nat_availco2(IREG,IBIN,iyr) + NS_QUANT(IREG,jREC) * 18.0
                            src_availco2(IREG,ISRC,iyr) = src_availco2(IREG,ISRC,iyr) + NS_QUANT(IREG,jREC) * 18.0
                            IF (NS_SYR(IREG,jREC) .EQ. 0) NS_SYR(IREG,jREC) = iyr
                            target_co2 = target_co2 - NS_QUANT(IREG,jREC) * 18.0
                            cregpryr(IREG,IBIN,iyr) = MAX(cregpryr(IREG,IBIN,iyr) , NS_PRICE(IREG,jREC,NEMS_YR) / 18.0)
                            cregpryrS(IREG,ISRC,iyr) = MIN(cregpryrS(IREG,ISRC,iyr) , NS_PRICE(IREG,jREC,NEMS_YR) / 18.0)
                        END IF
                    END DO
                end if

                IF (src_availco2(IREG,ISRC,iyr) .EQ. 0.0) THEN
                    cregpryrS(IREG,ISRC,iyr) = 0.0
                END IF
            end do

        ELSEIF (ISRC == 4) THEN
            do iyr = 1, max_yr
                if (iyr <= stop1) then
                    ! before the technology has been developed
                    nat_availco2(IREG,ISRC,iyr) = 0.0
                end if

                if (iyr >= stop1.and.iyr <= stop2) then
                    ! after technology is developed,before infrastructure
                    nat_Availco2(IREG,ISRC,iyr) = 0.0
                end if

                if (iyr > stop2.and.iyr <= stop3) then
                    ! during the market penetration phase
                    nat_availco2(IREG,ISRC,iyr) = camar(iyr-stop2) * &
                    (umpco2(ISRC)/100.0) * co2base(IREG,ISRC)
                end if

                if (iyr > stop3) then
                    ! after market saturation has been reached
                    nat_availco2(IREG,ISRC,iyr) = (umpco2(ISRC) / 100.0) * co2base(IREG,ISRC)
                end if

                cregpryr(IREG,ISRC,iyr) = cregpr(IREG,ISRC)
                cregpryrS(IREG,ISRC,iyr) = cregpr(IREG,ISRC)
                src_availco2(IREG,ISRC,iyr) = nat_availco2(IREG,ISRC,iyr)
            end do
        END IF
    end do !end regional loop
end do !end CO2 source loop

! call get_co2 to populate quantities of capturable co2 from XTL and keep track of CCS activity in EMM and PMM
call Get_CO2()

! assign temporary variable
do i = 1, max_src
    source_label2(i) = source_label(i)
    do j = 1, max_reg-1       
        do k = 1, max_yr
            co2avl(j,i,k) = nat_availco2(j,i,k)
        end do
    end do
end do

do i = 1, max_src
    l = i
    source_label(i) = source_label2(l)
    cotype(i) = cotype2(l)

    do j = 1, max_reg-1        
        do k = 1, max_yr
            bse_availco2(j,i,k) = nat_availco2(j,i,k)
            co2avail(j,k) = co2avail(j,k)+nat_availco2(j,i,k)

            nat_availco2(j,i,k) = co2avl(j,l,k)
            bse_availco2(j,i,k) = nat_availco2(j,i,k)
            co2avail(j,k) = co2avail(j,k)+nat_availco2(j,i,k)
        end do
    end do
end do

do j = 1, max_reg-1
    do iyr = 1,max_yr
        basavail(j,iyr) = co2avail(j,iyr)
    end do
end do

call write_log('[CTUS][set_base_co2_volumes_available] End.')
end subroutine set_base_co2_volumes_available


SUBROUTINE CO2COST_CALC(rr)
! Determine CO2 costs and set price output variables

implicit none

include 'parametr'     ! nems dimension parameters
include 'ncntrl'       ! nems control variables
include 'ogsmparm'     ! ogsm parameter file
include 'ogsmbfw'
include 'ogsmugr'
include 'ogsml48'
include 'ogsmout'
include 'pmmout'
include 'ctus'

integer ires
integer pseg
real ptar, cost, oil_price
real*4 og_avgoprc
integer ir,is,rr, t_yr, t_cyr, jr

REAL PIPETAR
DATA PIPETAR/0.40/

INTEGER PIPESEG(max_reg-1,max_reg-1)
DATA PIPEseg/1,2,2,4,4,6,3,  &
            2,1,2,2,4,5,4,  &
            2,2,1,2,2,4,2,  &
            4,2,2,1,2,4,4,  &
            4,4,2,2,1,2,2,  &
            6,5,4,4,2,1,3,  &
            3,4,2,4,2,3,1/

call write_log('[CTUS][CO2COST_CALC] Start.')

! determine matrix of co2 cost
DO t_yr = itimeyr , max_yr
    t_cyr = min(lastyr , t_yr + l48hyr-1)
    DO ir=1,max_reg-1
        DO is = 1,max_src
            IF (is.eq.4) THEN
                ! natural CO2
                jr = ir
                oil_price = min(dcrdwhp(jr,t_cyr) , OG_AVGOPRC(t_cyr,-5,jr)) * dladj
                COST = CO2K + CO2B * oil_price

                ! caps cost at 85% of cement cost
                COST = min(COST , 0.85 * cregpr(ir,5))
                OGCO2PRCs(ir,is,t_cyr) = COST

                IF (rr .eq. 4) THEN
                    write(ogbug1,3712) curirun, curiyr+1989, t_cyr+1989, rr, ir, jr, is, oilpriceo(1), &
                    oil_price, dcrdwhp(jr,t_cyr), OG_AVGOPRC(t_cyr,-5,jr), dladj, &
                    COST, CO2K, CO2B, cregpr(ir,5)
                    3712  FORMAT(1X,"WELLCOST_OIL_PRICE",7(":",I4),9(":",F20.6))
                END IF
            ELSE
                COST = cregpryr(ir,is,t_yr) - PIPETAR
                OGCO2PRCs(ir,is,t_cyr) = cregpryrS(ir,is,t_yr) - PIPETAR
            end if

            reg_COSTCO2(rr,ir,is,t_yr) = COST + PIPETAR * PIPESEG(rr,IR)
            OGCO2TAR(rr,ir) =  PIPETAR * PIPESEG(rr,IR)
            OGCO2PRC(ir,is,t_cyr) = COST

            if (is.eq.7) then
                if (rr .eq. ir) then
                    reg_COSTCO2(rr,ir,is,t_yr) = OGCO2PEM(ir,t_cyr)
                    COST = OGCO2PEM(ir,t_cyr)
                else
                    reg_COSTCO2(rr,ir,is,t_yr) = 999.99
                    COST = 999.99
                end if
                OGCO2PRC(ir,is,t_cyr) = OGCO2PEM(ir,t_cyr)
                OGCO2PRCs(ir,is,t_cyr) = OGCO2PEM(ir,t_cyr)
            end if

            if (is.eq.10) then
                if (rr .eq. ir) then
                    reg_COSTCO2(rr,ir,is,t_yr) = OGCO2PLF(ir,t_cyr)
                    COST = OGCO2PLF(ir,t_cyr)
                else
                    reg_COSTCO2(rr,ir,is,t_yr) = 999.99
                    COST = 999.99
                end if
                OGCO2PRC(ir,is,t_cyr) = OGCO2PLF(ir,t_cyr)
                OGCO2PRCs(ir,is,t_cyr) = OGCO2PLF(ir,t_cyr)
            end if

            IF (nat_availco2(ir,is,t_yr) .GT. 0.0 .OR. is .eq. 4 .or. is .eq. 7 .or. is .eq. 10) THEN
                write(ogbug1,2043) CURIRUN, curiyr+1989, itimeyr+2010, t_cyr+1989, t_yr+2010, curitr, rr, ir, is, &
                    COST, REAL(PIPETAR * PIPESEG(rr,IR)), reg_COSTCO2(rr,ir,is,t_yr), &
                    OGCO2PRC(ir,is,t_cyr), nat_availco2(ir,is,t_yr), CO2K, CO2B, OILPRICEo(1)
                2043  FORMAT(1X,"DBGCOST",9(":",I4),8(":",F15.6))
            END IF

            IF (nat_availco2(ir,is,t_yr) .le. 0.0) reg_COSTCO2(rr,ir,is,t_yr) = 999.99

            ! time to construct pipeline
            IF (t_cyr .le. 25 .and. ir .ne. rr) reg_COSTCO2(rr,ir,is,t_yr) = 999.99
        end do

        IF (rr .EQ. ir) THEN
            is = 7
            WRITE(ogbug1,2743) CURIRUN, CURIYR+1989, itimeyr+2010, t_cyr+1989, t_yr+2010, CURITR, rr, ir, is, OGCO2PEM(ir,t_cyr), &
                reg_COSTCO2(rr,ir,is,t_yr), nat_availco2(ir,is,t_yr), cregpr(ir,is), OGCO2PRC(ir,is,t_cyr), OGCO2AVL(ir,is,t_cyr)
            is = 10
            WRITE(ogbug1,2743) CURIRUN, CURIYR+1989, itimeyr+2010, t_cyr+1989, t_yr+2010, CURITR, rr, ir, is, OGCO2PLF(ir,t_cyr), &
                reg_COSTCO2(rr,ir,is,t_yr), nat_availco2(ir,is,t_yr), cregpr(ir,is), OGCO2PRC(ir,is,t_cyr), OGCO2AVL(ir,is,t_cyr)
            2743  FORMAT(1X,"reg_COSTCO2",9(":",I4),6(":",F15.6))
        END IF
    end do
end do

call write_log('[CTUS][CO2COST_CALC] End.')

RETURN
END SUBROUTINE CO2COST_CALC

end module ctus_mod
