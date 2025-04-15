! $Header: m:/default/source/RCS/unugs.f,v 1.57 2020/10/14 21:15:42 LC2 Exp $
!
      SUBROUTINE ELNUGS(IRG,IYR)
      IMPLICIT NONE

!     THIS SUBROUTINE IS FOR PROCESSING NON UTILITY (END USE) GENERATION

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'ecpcntl'
      include 'dispin'
      include 'bildin'
      include 'cogen'
      include 'postpr'
      include 'wrenew'
      include 'dispinyr'
      include 'dispuse'
      include 'uefdout'
!
      INTEGER*4 I,J,IECP,IFL,JFL,ISEC,ICN
      INTEGER*4 IRG,CRG,IYR

!     TC_FUELS has been moved to emmparm
!     Moved F_CL - F_WN to ecpcntl and moved the assignment to RDCNTRL in udat.f

!     INTEGER*4 F_CL                 !  1 - Coal
!     INTEGER*4 F_OL                 !  2 - Oil
!     INTEGER*4 F_OG                 !  3 - Other Gaesous Fuels
!     INTEGER*4 F_NG                 !  4 - Natural Gas
!     INTEGER*4 F_HY                 !  5 - Hydro
!     INTEGER*4 F_GT                 !  6 - Geothermal
!     INTEGER*4 F_MS                 !  7 - MSW
!     INTEGER*4 F_WD                 !  8 - Biomass
!     INTEGER*4 F_SO                 !  9 - Solar Thermal
!     INTEGER*4 F_PV                 ! 10 - Solar PV
!     INTEGER*4 F_OT                 ! 11 - Other
!     INTEGER*4 F_WN                 ! 12 - Wind

!     TC_SECTORS has been moved to emmparm

      INTEGER*4 S_IND                !  1 - Industrial
      INTEGER*4 S_COM                !  2 - Commercial
      INTEGER*4 S_RES                !  3 - Residential
      INTEGER*4 S_OG                 !  4 - Enhanced Oil and Gas Recovery
      INTEGER*4 S_REF                !  5 - Refinery
      INTEGER*4 MAP_EL_SCTR(TC_SECTORS,TC_FUELS) ! Map Electric Fuel Codes to Sector Fuels Codes
      INTEGER*4 MAP_ECP_NDX(TC_FUELS)             ! For Renewables Map to ECP Indexs
      INTEGER*4 MAP_SECT(TC_SECTORS)     ! Map sectors to LDSM sectors
!
      REAL*8 RFRGSHR                         ! Use for Refineries (CTL uses different share)
      REAL*8 GN_to_Grid(TC_SECTORS,TC_FUELS) ! Generation to Grid
      REAL*8 GN_Own_Use(TC_SECTORS,TC_FUELS) ! Generation for Own Use
      REAL*8 CP_Total(TC_SECTORS,TC_FUELS)   ! Capacity Total
      REAL*8 Con_Total(TC_SECTORS,TC_FUELS)  ! Consumption Total
!
      REAL*8 Total
      REAL*8 IAMT_,CAMT_,SAMT_,GAMT_,RAMT_
!     REAL*8 TRADRG,TRADTG,TRADRO,TRADTO
!
!     MOVED THE COMMON CGGEN to ecpcntl

!     REAL*8 CG_GEN_I(MNUMNR,TC_FUELS),CG_GEN_C(MNUMNR,TC_FUELS),CG_GEN_R(MNUMNR,TC_FUELS), &
!            CG_GEN_O(MNUMNR,TC_FUELS),CG_GEN_F(MNUMNR,TC_FUELS)
!     COMMON /CGGEN/ CG_GEN_I,CG_GEN_C,CG_GEN_R,CG_GEN_O,CG_GEN_F
!
      REAL*8 CGPVGEN,CGPVCRD,CGWNGEN,CGWNCRD

!
!     INITIALIZE ARRAYS FOR BONUS END-USE CREDITS FOR PV AND WIND (IF ANY)
!
      UCSPVSHR(IRG) = 0.0
      UCWNDSHR(IRG) = 0.0
      CGPVGEN = 0.0
      CGPVCRD = 0.0
      CGWNGEN = 0.0
      CGWNCRD = 0.0

!     F_CL  =  1   ! Coal
!     F_OL  =  2   ! Oil
!     F_OG  =  3   ! Other Gaesous Fuels
!     F_NG  =  4   ! Natural Gas
!     F_HY  =  5   ! Hydro
!     F_GT  =  6   ! Geothermal
!     F_MS  =  7   ! MSW
!     F_WD  =  8   ! Biomass
!     F_SO  =  9   ! Solar Thermal
!     F_PV  = 10   ! Solar PV
!     F_OT  = 11   ! Other
!     F_WN  = 12   ! Wind
!
      S_IND  =  1   ! Industrial
      S_COM  =  2   ! Commercial
      S_RES  =  3   ! Commercial
      S_OG   =  4   ! Enhanced Oil and Gas Recovery
      S_REF  =  5   ! Refinery

      MAP_SECT(S_IND) = 3
      MAP_SECT(S_COM) = 2
      MAP_SECT(S_RES) = 1
      MAP_SECT(S_OG)  = 3
      MAP_SECT(S_REF) = 3
!
!
!     DETERMINE CTL SHARES
!
      IF (IRG .EQ. 1)THEN
        CTLGEN(MNUMNR,IYR) = 0.0
        CTLCAP(MNUMNR,IYR) = 0.0
      END IF
      IF ((CGREFGEN(MNUMCR,IYR,1,1) + CGREFGEN(MNUMCR,IYR,1,2)) .LE. 0.0)THEN
        CTLGEN(IRG,IYR) = 1.0 / FLOAT(UNRGNS)
      ELSE
          CTLGEN(IRG,IYR) = 0.0
        DO I = 1 , MNUMCR - 2
          CTLGEN(IRG,IYR) = CTLGEN(IRG,IYR) + CTL_CD_NR(I,IRG) *  &
                            (CGREFGEN(I,IYR,1,1) + CGREFGEN(I,IYR,1,2))
        END DO
          CTLGEN(IRG,IYR) = CTLGEN(IRG,IYR) / (CGREFGEN(MNUMCR,IYR,1,1) + CGREFGEN(MNUMCR,IYR,1,2))
      END IF
          CTLCAP(IRG,IYR) = CTLGEN(IRG,IYR)
!
          CTLGEN(MNUMNR,IYR) = CTLGEN(MNUMNR,IYR) + CTLGEN(IRG,IYR)
          CTLCAP(MNUMNR,IYR) = CTLCAP(MNUMNR,IYR) + CTLCAP(IRG,IYR)
!
      DO IFL = 1 , TC_FUELS
         DO ISEC = 1 , TC_SECTORS
            MAP_EL_SCTR(ISEC,IFL) = 0
            GN_to_Grid(ISEC,IFL) = 0.0
            GN_Own_Use(ISEC,IFL) = 0.0
            CP_Total(ISEC,IFL) = 0.0
            Con_Total(ISEC,IFL) = 0.0
         END DO
         CGTOTQNR(IRG,CURIYR,IFL) = 0.0
         CGTOTCAPNR(IRG,CURIYR,IFL) = 0.0
         CGTOTGENNR(IRG,CURIYR,IFL,1) = 0.0
         CGTOTGENNR(IRG,CURIYR,IFL,2) = 0.0
         MAP_ECP_NDX(IFL) = 0
      END DO
!
      MAP_EL_SCTR(S_IND,F_CL) = 1
      MAP_EL_SCTR(S_IND,F_OL) = 2
      MAP_EL_SCTR(S_IND,F_NG) = 3
      MAP_EL_SCTR(S_IND,F_HY) = 4
      MAP_EL_SCTR(S_IND,F_GT) = 5
      MAP_EL_SCTR(S_IND,F_MS) = 6
      MAP_EL_SCTR(S_IND,F_WD) = 7
      MAP_EL_SCTR(S_IND,F_PV) = 8
      MAP_EL_SCTR(S_IND,F_OG) = 9
      MAP_EL_SCTR(S_IND,F_OT) = 10
      MAP_EL_SCTR(S_IND,F_WN) = 11
!
      MAP_EL_SCTR(S_COM,F_CL) = 1
      MAP_EL_SCTR(S_COM,F_OL) = 2
      MAP_EL_SCTR(S_COM,F_NG) = 3
      MAP_EL_SCTR(S_COM,F_HY) = 4
      MAP_EL_SCTR(S_COM,F_GT) = 5
      MAP_EL_SCTR(S_COM,F_MS) = 6
      MAP_EL_SCTR(S_COM,F_WD) = 7
      MAP_EL_SCTR(S_COM,F_PV) = 8
      MAP_EL_SCTR(S_COM,F_OG) = 9
      MAP_EL_SCTR(S_COM,F_OT) = 10
      MAP_EL_SCTR(S_COM,F_WN) = 11
!
      MAP_EL_SCTR(S_RES,F_CL) = 1
      MAP_EL_SCTR(S_RES,F_OL) = 2
      MAP_EL_SCTR(S_RES,F_NG) = 3
      MAP_EL_SCTR(S_RES,F_HY) = 4
      MAP_EL_SCTR(S_RES,F_GT) = 5
      MAP_EL_SCTR(S_RES,F_MS) = 6
      MAP_EL_SCTR(S_RES,F_WD) = 7
      MAP_EL_SCTR(S_RES,F_PV) = 8
      MAP_EL_SCTR(S_RES,F_OG) = 9
      MAP_EL_SCTR(S_RES,F_OT) = 10
      MAP_EL_SCTR(S_RES,F_WN) = 11
!
      MAP_EL_SCTR(S_OG,F_CL) = 1
      MAP_EL_SCTR(S_OG,F_OL) = 2
      MAP_EL_SCTR(S_OG,F_NG) = 3
      MAP_EL_SCTR(S_OG,F_OT) = 4
!
      MAP_EL_SCTR(S_REF,F_OG) = 9
      MAP_EL_SCTR(S_REF,F_OL) = 2
      MAP_EL_SCTR(S_REF,F_NG) = 3
      MAP_EL_SCTR(S_REF,F_WD) = 7
      MAP_EL_SCTR(S_REF,F_CL) = 1                !CTL
!
      MAP_ECP_NDX(F_HY) = WIHY
      MAP_ECP_NDX(F_GT) = WIGT
      MAP_ECP_NDX(F_MS) = WIMS
      MAP_ECP_NDX(F_WD) = WIWD
      MAP_ECP_NDX(F_SO) = WISO
      MAP_ECP_NDX(F_PV) = WIPV
      MAP_ECP_NDX(F_WN) = WIWN
!
      CALL GETBLD(1,IRG)
!
!     THIS CALL IS FOR CANADA ONLY               !//CANADA ONLY
!
      IF (USW_XP .GT. 0) THEN
         IF (IRG .EQ. 1) CALL RDCOGEN            !//CANADA ONLY
      END IF
!
!     LOAD Traditional Cogen Generation and Capacity Vectors
!
!      WRITE(6,*)'made it inside ELNUGS 1',IRG,IYR

      DO IFL = 1 , TC_FUELS
         JFL = MAP_EL_SCTR(S_IND,IFL)
         IECP = MAP_ECP_NDX(IFL)
         IF (JFL .GT. 0) THEN
           DO ICN = 1, MNUMCR - 2

                 GN_to_Grid(S_IND,IFL) = GN_to_Grid(S_IND,IFL) + CGINDLGEN(ICN,IYR,JFL,1) * &
                    MPCGCtoN(IRG,ICN,IFL)
                 GN_Own_Use(S_IND,IFL) = GN_Own_Use(S_IND,IFL) + CGINDLGEN(ICN,IYR,JFL,2) * &
                    MPCGCtoN(IRG,ICN,IFL)
                 CP_Total(S_IND,IFL) = CP_Total(S_IND,IFL) + MPCGCtoN(IRG,ICN,IFL) * CGINDLCAP(ICN,IYR,JFL)
                 Con_Total(S_IND,IFL) = Con_Total(S_IND,IFL) + MPCGCtoN(IRG,ICN,IFL) * CGINDLQ(ICN,IYR,JFL)

           ENDDO
           CGTOTGENNR(IRG,CURIYR,IFL,1) = CGTOTGENNR(IRG,CURIYR,IFL,1) + GN_to_Grid(S_IND,IFL)
           CGTOTGENNR(IRG,CURIYR,IFL,2) = CGTOTGENNR(IRG,CURIYR,IFL,2) + GN_Own_Use(S_IND,IFL)
           CGTOTCAPNR(IRG,CURIYR,IFL) = CGTOTCAPNR(IRG,CURIYR,IFL) + CP_Total(S_IND,IFL)
           CGTOTQNR(IRG,CURIYR,IFL) = CGTOTQNR(IRG,CURIYR,IFL) + Con_Total(S_IND,IFL)
         END IF
         JFL = MAP_EL_SCTR(S_COM,IFL)
         IF (JFL .GT. 0) THEN
           DO ICN = 1, MNUMCR - 2
            GN_to_Grid(S_COM,IFL) = GN_to_Grid(S_COM,IFL) + CGCOMMGEN(ICN,IYR,JFL,1) * &
               MPCGCtoN(IRG,ICN,IFL)
 !                IF (IRG.EQ.1.and.IYR.EQ.20) WRITE(6,*)'CGCOMMGEN, mpcgton ', CGCOMMGEN(ICN,IYR,JFL,1), MPCGCtoN(IRG,ICN,IFL)
            GN_Own_Use(S_COM,IFL) = GN_Own_Use(S_COM,IFL) + CGCOMMGEN(ICN,IYR,JFL,2) * &
               MPCGCtoN(IRG,ICN,IFL)
            CP_Total(S_COM,IFL) = CP_Total(S_COM,IFL) + MPCGCtoN(IRG,ICN,IFL) * CGCOMMCAP(ICN,IYR,JFL)
            Con_Total(S_COM,IFL) = Con_Total(S_COM,IFL) + CGCOMMQ(ICN,IYR,JFL) * MPCGCtoN(IRG,ICN,IFL)
            IF (IECP .EQ. WIPV)THEN
               CGPVGEN = CGPVGEN + (CGCOMMGEN(ICN,IYR,JFL,1) + CGCOMMGEN(ICN,IYR,JFL,2)) * &
                         MPCGCtoN(IRG,ICN,IFL)
               CGPVCRD = CGPVCRD + (CGCOMMGEN(ICN,IYR,JFL,1) + CGCOMMGEN(ICN,IYR,JFL,2)) * &
                         MPCGCtoN(IRG,ICN,IFL) * CGCPVCOM(ICN,CURIYR)
            END IF
            IF (IECP .EQ. WIWN)THEN
               CGWNGEN = CGWNGEN + (CGCOMMGEN(ICN,IYR,JFL,1) + CGCOMMGEN(ICN,IYR,JFL,2)) * &
                         MPCGCtoN(IRG,ICN,IFL)
               CGWNCRD = CGWNCRD + (CGCOMMGEN(ICN,IYR,JFL,1) + CGCOMMGEN(ICN,IYR,JFL,2)) * &
                         MPCGCtoN(IRG,ICN,IFL) * CGCWNCOM(ICN,CURIYR)
            END IF
           ENDDO
           CGTOTGENNR(IRG,CURIYR,IFL,1) = CGTOTGENNR(IRG,CURIYR,IFL,1) + GN_to_Grid(S_COM,IFL)
           CGTOTGENNR(IRG,CURIYR,IFL,2) = CGTOTGENNR(IRG,CURIYR,IFL,2) + GN_Own_Use(S_COM,IFL)
           CGTOTCAPNR(IRG,CURIYR,IFL) = CGTOTCAPNR(IRG,CURIYR,IFL) + CP_Total(S_COM,IFL)
           CGTOTQNR(IRG,CURIYR,IFL) = CGTOTQNR(IRG,CURIYR,IFL) + Con_Total(S_COM,IFL)
         END IF
!
         JFL = MAP_EL_SCTR(S_RES,IFL)
         IF (JFL .GT. 0) THEN
           DO ICN = 1, MNUMCR - 2
            GN_to_Grid(S_RES,IFL) = GN_to_Grid(S_RES,IFL) + CGRESGEN(ICN,IYR,JFL,1) * &
               MPCGCtoN(IRG,ICN,IFL)
 !                IF (IRG.EQ.1.and.IYR.EQ.20) WRITE(6,*)'CGRESGEN, mpcgton ', CGRESGEN(ICN,IYR,JFL,1), MPCGCtoN(IRG,ICN,IFL)
            GN_Own_Use(S_RES,IFL) = GN_Own_Use(S_RES,IFL) + CGRESGEN(ICN,IYR,JFL,2) * &
               MPCGCtoN(IRG,ICN,IFL)
            CP_Total(S_RES,IFL) = CP_Total(S_RES,IFL) + MPCGCtoN(IRG,ICN,IFL) * &
               CGRESCAP(ICN,IYR,JFL)
            Con_Total(S_RES,IFL) = Con_Total(S_RES,IFL) + CGRESQ(ICN,IYR,JFL) * &
               MPCGCtoN(IRG,ICN,IFL)
            IF (IECP .EQ. WIPV)THEN
               CGPVGEN = CGPVGEN + (CGRESGEN(ICN,IYR,JFL,1) + CGRESGEN(ICN,IYR,JFL,2)) * MPCGCtoN(IRG,ICN,IFL)
               CGPVCRD = CGPVCRD + (CGRESGEN(ICN,IYR,JFL,1) + CGRESGEN(ICN,IYR,JFL,2)) * MPCGCtoN(IRG,ICN,IFL) *  &
                                    CGCPVRES(ICN,CURIYR)
            END IF
            IF (IECP .EQ. WIWN)THEN
               CGWNGEN = CGWNGEN + (CGRESGEN(ICN,IYR,JFL,1) + CGRESGEN(ICN,IYR,JFL,2)) * MPCGCtoN(IRG,ICN,IFL)
               CGWNCRD = CGWNCRD + (CGRESGEN(ICN,IYR,JFL,1) + CGRESGEN(ICN,IYR,JFL,2)) * MPCGCtoN(IRG,ICN,IFL) *  &
                                    CGCWNRES(ICN,CURIYR)
            END IF
           ENDDO
           CGTOTGENNR(IRG,CURIYR,IFL,1) = CGTOTGENNR(IRG,CURIYR,IFL,1) + GN_to_Grid(S_RES,IFL)
           CGTOTGENNR(IRG,CURIYR,IFL,2) = CGTOTGENNR(IRG,CURIYR,IFL,2) + GN_Own_Use(S_RES,IFL)
           CGTOTCAPNR(IRG,CURIYR,IFL) = CGTOTCAPNR(IRG,CURIYR,IFL) + CP_Total(S_RES,IFL)
           CGTOTQNR(IRG,CURIYR,IFL) = CGTOTQNR(IRG,CURIYR,IFL) + Con_Total(S_RES,IFL)
         END IF
!
         JFL = MAP_EL_SCTR(S_OG,IFL)
         IF (JFL .GT. 0) THEN
           DO ICN = 1, MNUMCR - 2
            GN_to_Grid(S_OG,IFL) = GN_to_Grid(S_OG,IFL) + CGOGSGEN(ICN,IYR,JFL,1) * &
               MPCGCtoN(IRG,ICN,IFL)
 !                IF (IRG.EQ.1.and.IYR.EQ.20) WRITE(6,*)'CGOGSGEN, mpcgton ', CGOGSGEN(ICN,IYR,JFL,1) , MPCGCtoN(IRG,ICN,IFL)
            GN_Own_Use(S_OG,IFL) = GN_Own_Use(S_OG,IFL) + CGOGSGEN(ICN,IYR,JFL,2) * &
               MPCGCtoN(IRG,ICN,IFL)
            CP_Total(S_OG,IFL) = CP_Total(S_OG,IFL) + MPCGCtoN(IRG,ICN,IFL) * CGOGSCAP(ICN,IYR,JFL)
            Con_Total(S_OG,IFL) = Con_Total(S_OG,IFL) + MPCGCtoN(IRG,ICN,IFL) * CGOGSQ(ICN,IYR,JFL)
           ENDDO
           CGTOTGENNR(IRG,CURIYR,IFL,1) = CGTOTGENNR(IRG,CURIYR,IFL,1) + GN_to_Grid(S_OG,IFL)
           CGTOTGENNR(IRG,CURIYR,IFL,2) = CGTOTGENNR(IRG,CURIYR,IFL,2) + GN_Own_Use(S_OG,IFL)
           CGTOTCAPNR(IRG,CURIYR,IFL) = CGTOTCAPNR(IRG,CURIYR,IFL) + CP_Total(S_OG,IFL)
           CGTOTQNR(IRG,CURIYR,IFL) = CGTOTQNR(IRG,CURIYR,IFL) + Con_Total(S_OG,IFL)
         END IF
         JFL = MAP_EL_SCTR(S_REF,IFL)
         IF (JFL .GT. 0) THEN
           DO ICN = 1, MNUMCR - 2
              IF (IFL .EQ. F_CL)THEN
                RFRGSHR = CTL_CD_NR(ICN,IRG)
              ELSE
                RFRGSHR = MPCGCtoN(IRG,ICN,IFL)
              END IF
            GN_to_Grid(S_REF,IFL) = GN_to_Grid(S_REF,IFL) + CGREFGEN(ICN,IYR,JFL,1) * RFRGSHR
            GN_Own_Use(S_REF,IFL) = GN_Own_Use(S_REF,IFL) + CGREFGEN(ICN,IYR,JFL,2) * RFRGSHR
            CP_Total(S_REF,IFL) = CP_Total(S_REF,IFL) + RFRGSHR * CGREFCAP(ICN,IYR,JFL)
            Con_Total(S_REF,IFL) = Con_Total(S_REF,IFL) + CGREFQ(ICN,IYR,JFL) * RFRGSHR
           ENDDO
           CGTOTGENNR(IRG,CURIYR,IFL,1) = CGTOTGENNR(IRG,CURIYR,IFL,1) + GN_to_Grid(S_REF,IFL)
           CGTOTGENNR(IRG,CURIYR,IFL,2) = CGTOTGENNR(IRG,CURIYR,IFL,2) + GN_Own_Use(S_REF,IFL)
           CGTOTCAPNR(IRG,CURIYR,IFL) = CGTOTCAPNR(IRG,CURIYR,IFL) + CP_Total(S_REF,IFL)
           CGTOTQNR(IRG,CURIYR,IFL) = CGTOTQNR(IRG,CURIYR,IFL) + Con_Total(S_REF,IFL)
         END IF
      END DO
!
!     Write Traditional Cogen Estimates to Debug File
!
      IF (UF_DBG .GT. 0) THEN
         DO ISEC = 1 , TC_SECTORS
            DO IFL = 1 , TC_FUELS
               JFL = MAP_EL_SCTR(ISEC,IFL)
               IECP = MAP_ECP_NDX(IFL)
               IF (JFL .GT. 0 ) THEN

!                 WRITE(UF_DBG,2314) CURIRUN, CURIYR+UHBSYR, CURITR, IRG, ISEC, IFL, JFL, IECP, GN_to_Grid(ISEC,IFL), GN_Own_Use(ISEC,IFL), CP_Total(ISEC,IFL), CGTOTCAPNR(IRG,CURIYR,IFL)

                  WRITE(UF_DBG,2314) CURIRUN, CURIYR+UHBSYR, CURITR, IRG, ISEC, IFL, JFL, IECP, GN_to_Grid(ISEC,IFL), GN_Own_Use(ISEC,IFL), CP_Total(ISEC,IFL), CGTOTCAPNR(IRG,CURIYR,IFL)
 2314             FORMAT(1X,"TCOGEN",8(",",I4),4(",",F21.6))

               END IF
            END DO
         END DO
      END IF
!
!     INITIALIZE TRADITIONAL GRID AND OWN USE FOR RPS
!
!     TRADRG = 0.0
!     TRADTG = 0.0
!     TRADRO = 0.0
!     TRADTO = 0.0
      EDRPSRG = 0.0
      EDRPSTG = 0.0
      EDRPSRO = 0.0
      EDRPSTO = 0.0
      DO IECP = 1 , ECP_D_CAP
        ECRPSRG(IECP) = 0.0
        ECRPSRO(IECP) = 0.0
      END DO
      IF (IRG .EQ. 1)THEN
        URPSCGO(MNUMNR,CURIYR) = 0.0
        URPSCGG(MNUMNR,CURIYR) = 0.0
      END IF
!
!     ACCUMULATE TRADITIONAL COGEN FOR RPS ACCOUNTING
!
      DO IFL = 1 , TC_FUELS
         IECP = MAP_ECP_NDX(IFL)
         IF (IECP .EQ. 0) THEN
            DO ISEC = 1 , TC_SECTORS
!              TRADTG = TRADTG + GN_to_Grid(ISEC,IFL)
!              TRADTO = TRADTO + GN_Own_Use(ISEC,IFL)
               EDRPSTG = EDRPSTG + GN_to_Grid(ISEC,IFL) * 0.001
               EDRPSTO = EDRPSTO + GN_Own_Use(ISEC,IFL) * 0.001
            END DO
         ELSE IF (UPRNWSHR(IECP) .GE. 0.0) THEN
            DO ISEC = 1 , TC_SECTORS
!              TRADRG = TRADRG + GN_to_Grid(ISEC,IFL) * UPRNWSHR(IECP)
!              TRADTG = TRADTG + GN_to_Grid(ISEC,IFL)
!              TRADRO = TRADRO + GN_Own_Use(ISEC,IFL) * UPRNWSHR(IECP)
!              TRADTO = TRADTO + GN_Own_Use(ISEC,IFL)
               EDRPSRG = EDRPSRG + GN_to_Grid(ISEC,IFL) * UPRNWSHR(IECP) * 0.001
               EDRPSTG = EDRPSTG + GN_to_Grid(ISEC,IFL) * 0.001
               EDRPSRO = EDRPSRO + GN_Own_Use(ISEC,IFL) * UPRNWSHR(IECP) * 0.001
               EDRPSTO = EDRPSTO + GN_Own_Use(ISEC,IFL) * 0.001
               ECRPSRG(IECP) = ECRPSRG(IECP) + GN_to_Grid(ISEC,IFL) * 0.001
               ECRPSRO(IECP) = ECRPSRO(IECP) + GN_Own_Use(ISEC,IFL) * 0.001
            END DO
         END IF
      END DO
      IF (CGPVGEN .GT. 0.0)THEN
         UCSPVSHR(IRG) = CGPVCRD / CGPVGEN
      ELSE
         UCSPVSHR(IRG) = UPRNWSHR(WIPV)
      END IF
      IF (CGWNGEN .GT. 0.0)THEN
         UCWNDSHR(IRG) = CGWNCRD / CGWNGEN
      ELSE
         UCWNDSHR(IRG) = UPRNWSHR(WIWN)
      END IF

!     CONVERT TO BILLION-KILOWATTHOURS
!
!     EDRPSRO = EDRPSRO + TRADRO * 0.001
!     EDRPSTO = EDRPSTO + TRADTO * 0.001
!     EDRPSRG = EDRPSRG + TRADRG * 0.001
!     EDRPSTG = EDRPSTG + TRADTG * 0.001
!
!     STORE CURRENT YEAR INFO FOR ECP
!
!     EPRPSRO(CURIYR) = EDRPSRO
!     EPRPSTO(CURIYR) = EDRPSTO
!     EPRPSRG(CURIYR) = EDRPSRG
!     EPRPSTG(CURIYR) = EDRPSTG
      URPSCGO(IRG,CURIYR) = EDRPSRO
      URPSCGG(IRG,CURIYR) = EDRPSRG
      IF (UCSPVSHR(IRG) .GT. 0.0)THEN
         URPSCGO(MNUMNR,CURIYR) = URPSCGO(MNUMNR,CURIYR) + ECRPSRO(WIPV) * (UCSPVSHR(IRG) - UPRNWSHR(WIPV))
         URPSCGG(MNUMNR,CURIYR) = URPSCGG(MNUMNR,CURIYR) + ECRPSRG(WIPV) * (UCSPVSHR(IRG) - UPRNWSHR(WIPV))
      END IF
      IF (UCWNDSHR(IRG) .GT. 0.0)THEN
         URPSCGO(MNUMNR,CURIYR) = URPSCGO(MNUMNR,CURIYR) + ECRPSRO(WIWN) * (UCWNDSHR(IRG) - UPRNWSHR(WIWN))
         URPSCGG(MNUMNR,CURIYR) = URPSCGG(MNUMNR,CURIYR) + ECRPSRG(WIWN) * (UCWNDSHR(IRG) - UPRNWSHR(WIWN))
      END IF
!
      BFIRM(IRG) = 0.0
      BTCOGEN(IRG) = 0.0
      IAMT_ = 0.0
      CAMT_ = 0.0
      SAMT_ = 0.0
      GAMT_ = 0.0
      RAMT_ = 0.0
      DO IFL = 1, TC_FUELS
        CG_GEN_I(IRG,IFL) = 0.0
        CG_GEN_C(IRG,IFL) = 0.0
        CG_GEN_R(IRG,IFL) = 0.0
        CG_GEN_O(IRG,IFL) = 0.0
        CG_GEN_F(IRG,IFL) = 0.0
      ENDDO
!

      DO IFL = 1 , TC_FUELS
         IAMT_ = IAMT_ + GN_to_Grid(S_IND,IFL) / 8760.0
         CAMT_ = CAMT_ + GN_to_Grid(S_COM,IFL) / 8760.0
         SAMT_ = SAMT_ + GN_to_Grid(S_RES,IFL) / 8760.0
         GAMT_ = GAMT_ + GN_to_Grid(S_OG,IFL) / 8760.0
         RAMT_ = RAMT_ + GN_to_Grid(S_REF,IFL) / 8760.0
         CG_GEN_I(IRG,IFL) =  CG_GEN_I(IRG,IFL) + GN_to_Grid(S_IND,IFL)
         CG_GEN_C(IRG,IFL) = CG_GEN_C(IRG,IFL) + GN_to_Grid(S_COM,IFL)
         CG_GEN_R(IRG,IFL) = CG_GEN_R(IRG,IFL) + GN_to_Grid(S_RES,IFL)
         CG_GEN_O(IRG,IFL) = CG_GEN_O(IRG,IFL) + GN_to_Grid(S_OG,IFL)
         CG_GEN_F(IRG,IFL) = CG_GEN_F(IRG,IFL) + GN_to_Grid(S_REF,IFL)
         DO ISEC = 1 , TC_SECTORS
            IF (ISEC .EQ. S_RES .OR. ISEC .EQ. S_COM) THEN     ! include own use for res/comm, except PV if DPVDISPATCH is on
               IF (DPVDISPATCH) THEN 
                   IF( IFL .NE. F_PV) THEN  ! don't include PV in BTCOGEN - it will be dispatched 
                     BTCOGEN(IRG) = BTCOGEN(IRG) + GN_to_Grid(ISEC,IFL) * 0.001
                     BTCOGEN(IRG) = BTCOGEN(IRG) + GN_own_use(ISEC,IFL) * 0.001
                   ENDIF
               ELSE                                    ! include all fuels in BTCOGEN
                  BTCOGEN(IRG) = BTCOGEN(IRG) + GN_to_Grid(ISEC,IFL) * 0.001
                  BTCOGEN(IRG) = BTCOGEN(IRG) + GN_own_use(ISEC,IFL) * 0.001
               ENDIF
            ELSE
               BTCOGEN(IRG) = BTCOGEN(IRG) + GN_to_Grid(ISEC,IFL) * 0.001   !other sectors just include to_grid
            ENDIF 
         END DO
      END DO
!
      DO I = 1 , EENSP
         IF (UF_DBG .GT. 0) THEN
            WRITE(UF_DBG,394) 'EEITAJ,PRENUG',CURIYR+UHBSYR,CURITR,IRG,I,EEITAJ(I)
  394       FORMAT(A12,1X,4(":",I4),":",F8.4,I4)
         ENDIF
         BFIRM(IRG) = BFIRM(IRG) + EEITAJ(I) * EETIME(I) * 0.001

!  do not adjust EEITAJ for cogen, we account for it in the EFD LP!
!
!        EEITAJ(I) = EEITAJ(I) - (IAMT_ + CAMT_ + SAMT_ + RAMT_ + GAMT_)
!        WRITE(6,394)'EEITAJ,SEA',EEITAJ(I),I

         IF (UF_DBG .GT. 0) THEN
            WRITE(UF_DBG,393) 'EEITAJ,POSTNG',CURIYR+UHBSYR,CURITR, &
             IRG,I,EEITAJ(I),IAMT_,CAMT_,SAMT_,RAMT_,GAMT_
  393       FORMAT(A12,1X,4(":",I4),7(":",F12.4))
         END IF
      END DO
!
      CALL STRBLD(1,IRG)
!
      RETURN
      END
!
!
      SUBROUTINE RDNUGS
                
      USE SQLITE

      IMPLICIT NONE

!     THIS SUBROUTINE READS THE NUGPIPE AND ETTIN INPUT FILES

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'dispin'
      include 'cogen'
      include 'postpr'
      include 'dispett'
      include 'ecpcntl'
      include 'dsmdimen'
      include 'dsmtoefd'
      include 'dsmtfecp'
      include 'wrenew'
      include 'macout'
      include 'uefdout'
!
      CHARACTER*18 FILENM
      LOGICAL NEW
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR

      INTEGER TYR,TGRP,HIGHSTEP,FOUND_SW,MIN_YR,MAX_YR
      INTEGER UF_NUG,CANRGN(EFD_D_PROV),PROV,IGRP,JGRP,ISEA,JSEG,JSLC,NUM_PROV,NUM_RGNS,IRGN,JRGN
      INTEGER NEMS,YEAR,I,J,F,BYEAR,EYEAR,IYR,JYR,IPROV,JPROV,JSTEP,CYR
      INTEGER  PROVN(EFD_D_PROV),IJ,NUM_YRS,YEARS(MNUMYR),RGNS(MNUMNR),PROVRG(EFD_D_PROV)
      INTEGER ECI_GRP,ECI_SEG,ECP_GRP,ECP_SEG,EFD_GRP,EFD_SEG,ECP_SP,EFD_SP,ETT_SP
      INTEGER ISP,YR,IRG
      INTEGER NUMTABS
      REAL*8 TMP_ECP_GW,TMP_ECP_WGHT,SEG_WGHTS(EFD_D_SSZ)
      REAL*4 CID,FID,SIC,ZIP,CENSUS,HRATE,CAP,SMTH,SYR,RMTH,RYR
      REAL*4 GENGRD,GENOWN
      REAL*4 TEMPS(MNUMNR+EFD_D_PROV,2*MNUMYR)
      REAL*8 TGW(ECI_D_MSP),TPRC,CAN_DEFLAT,DOLLAR_YEAR,MAX_CC,TMP1(MNUMYR),TMP2(MNUMYR)
      REAL*8 TEMPCNS(ETT_D_MSP),TEMPCNSPF(ETT_D_MSP),TST_VAL
      CHARACTER*80 DUMMY
      CHARACTER*4 GID
      CHARACTER*3 PFL,SFL,TPTYPE
      CHARACTER*2 STATE,STATUS,PM,TPROV,HLDPROV,PROVA(EFD_D_PROV)

      type(sqlite_database)                      :: db
      type(sqlite_statement)                     :: stmt
      type(sqlite_column), dimension(:), pointer :: col
      character(len=40), pointer, dimension(:)   :: result
      character(len=80)                          :: errmsg

       integer                                    :: t_id
      integer                                    :: crg
       integer                                    :: t_expcicol
      real                                       :: bkgain
      logical                                    :: finished

      REAL*8 T_EXPCI, T_LINELOSS, T_CGCOMPF, T_CGINDPF, T_IGENGN, T_ISHARE, T_CSHARE, t_mapexpci

      call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )

      DATA PROVA/"BC","MB","ON","QB","NB","00","00","00"/
      DATA PROVN/  1,   2,   3,   4,   5,   0,   0,   0/
!
!     INITIALIZE
!
!     CGOTGEN = 0
      MAPEXPCI = 0.0
      EXPCI = 0.0
!
      ECANSQZ = 0.0
!
      CAN_CST = 0.0
      CAN_CST_SCMULT = 0.0
      CAN_QTY_SCMULT = 0.0
!
!     OPEN NUGS HISTORY AND PIPELINE FILE (USING FILE MANAGER)
!
      NEW = .FALSE.
      FILENM = 'NUGPIPE'
      UF_NUG = FILE_MGR('O',FILENM,NEW)
!
!     Read over header line
!
      READ(UF_NUG, *) DUMMY
!
      READ(UF_NUG, * ) DUMMY,LINELOSS,CGCOMPF,CGINDPF
!
!     QUERY THE DATABASE TABLE V_EMM_NUGPIPE_PARM

      allocate ( col(3) )

      call sqlite3_column_query( col(001), 'LINELOSS', sqlite_real )
      call sqlite3_column_query( col(002), 'CGCOMPF', sqlite_real )
      call sqlite3_column_query( col(003), 'CGINDPF', sqlite_real )

      call sqlite3_prepare_select( db, 'V_EMM_NUGPIPE_PARM', col, stmt)

!     LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS

      i = 0

      do
         i = i + 1
         call sqlite3_next_row( stmt, col, finished )
         if ( finished ) exit

         call sqlite3_get_column( col(001), T_LINELOSS)
         call sqlite3_get_column( col(002), T_CGCOMPF)
         call sqlite3_get_column( col(003), T_CGINDPF)

         iyr = year - UHBSYR

         WRITE(18,2005) CURIRUN, CURCALYR, LINELOSS, T_LINELOSS, CGCOMPF, T_CGCOMPF, CGINDPF, T_CGINDPF
 2005    FORMAT(1X,"V_EMM_NUGPIPE_PARM",2(",",I5),6(",",F21.6))

         LINELOSS = T_LINELOSS
         CGCOMPF = T_CGCOMPF
         CGINDPF = T_CGINDPF
      end do

      deallocate(col)

!     READ CANADIAN SQUEEZE PCTS - RGN by YR
!
      READ(UF_NUG, *) DUMMY
      READ(UF_NUG, * ) DUMMY,NUM_YRS,NUM_PROV
      READ(UF_NUG, * ) DUMMY,(PROVA(IPROV),IPROV=1,NUM_PROV)
      READ(UF_NUG, *) DUMMY,(CANRGN(IPROV),IPROV=1,NUM_PROV)
      DO IYR = 1 , NUM_YRS
         READ(UF_NUG, *) YEARS(IYR),(temps(IPROV,IYR),IPROV=1,NUM_PROV)
      END DO
!
      IF (YEARS(1) .GT. (UHBSYR + 1)) THEN
         DO YEAR = UHBSYR + 1 , YEARS(1) - 1
            IYR = YEAR - UHBSYR
            DO JPROV = 1 , NUM_PROV
               IPROV = CANRGN(JPROV)
               ECANSQZ(IPROV,IYR) = temps(JPROV,1)
            END DO
         END DO
      END IF
!
      IF (YEARS(NUM_YRS) .LT. MNUMYR+UHBSYR) THEN
         DO YEAR = YEARS(NUM_YRS) + 1 , UHBSYR + MNUMYR
            IYR = YEAR - UHBSYR
            DO JPROV = 1 , NUM_PROV
               IPROV = CANRGN(JPROV)
               ECANSQZ(IPROV,IYR) = temps(JPROV,NUM_YRS)
            END DO
         END DO
      END IF

      DO IYR = 1, NUM_YRS
        CYR = YEARS(IYR) - UHBSYR
        DO JPROV = 1, NUM_PROV
          IPROV = CANRGN(JPROV)
          ECANSQZ(IPROV,CYR) = temps(JPROV,IYR)
        ENDDO
      ENDDO
!
!     READ CANADIAN COSTS AND QUANTITY SCENARIO MULTIPLIERS by YR
!
      READ(UF_NUG, *) DUMMY
      READ(UF_NUG, * ) DUMMY,NUM_YRS
      READ(UF_NUG, * ) DUMMY
      READ(UF_NUG, *) DUMMY
      READ(UF_NUG, *) DUMMY
      DO IYR = 1 , NUM_YRS
         READ(UF_NUG, *) YEARS(IYR),TMP1(IYR),TMP2(IYR)
      END DO
!
      IF (YEARS(1) .GT. (UHBSYR + 1)) THEN
         DO YEAR = UHBSYR + 1 , YEARS(1) - 1
            IYR = YEAR - UHBSYR
            CAN_CST_SCMULT(IYR) = TMP1(1)
            CAN_QTY_SCMULT(IYR) = TMP2(1)
         END DO
      END IF
!
      IF (YEARS(NUM_YRS) .LT. MNUMYR+UHBSYR) THEN
         DO YEAR = YEARS(NUM_YRS) + 1 , UHBSYR + MNUMYR
            IYR = YEAR - UHBSYR
            CAN_CST_SCMULT(IYR) = TMP1(NUM_YRS)
            CAN_QTY_SCMULT(IYR) = TMP2(NUM_YRS)
         END DO
      END IF

      DO IYR = 1, NUM_YRS
        CYR = YEARS(IYR) - UHBSYR
        CAN_CST_SCMULT(CYR) = TMP1(IYR)
        CAN_QTY_SCMULT(CYR) = TMP2(IYR)
      ENDDO

!     QUERY THE DATABASE TABLE V_EMM_NUGPIPE

      allocate ( col(5) )

      call sqlite3_column_query( col(001), 'ID', sqlite_int )
      call sqlite3_column_query( col(002), 'YEAR', sqlite_int )
      call sqlite3_column_query( col(003), 'EMM_REG', sqlite_int )
      call sqlite3_column_query( col(004), 'EXPCI', sqlite_real )
      call sqlite3_column_query( col(005), 'EXPCICOL', sqlite_int )

      call sqlite3_prepare_select( db, 'V_EMM_NUGPIPE', col, stmt)

!     LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS

      i = 0

      do
         i = i + 1
         call sqlite3_next_row( stmt, col, finished )
         if ( finished ) exit

         call sqlite3_get_column( col(001), t_id)
         call sqlite3_get_column( col(002), year)
         call sqlite3_get_column( col(003), irg)
         call sqlite3_get_column( col(004), T_EXPCI)
         call sqlite3_get_column( col(005), t_expcicol)

         iyr = year - UHBSYR
         EXPCI(iyr,irg) = T_EXPCI
         
         WRITE(18,2002) CURIRUN, CURCALYR, year, iyr, t_id, t_expcicol, T_EXPCI, EXPCI(iyr,irg)
 2002    FORMAT(1X,"V_EMM_NUGPIPE_EXPCI_NEW",6(",",I5),2(",",F21.6))

      end do

      deallocate(col)

!     QUERY THE DATABASE TABLE V_NUGPIPE_MAP

      allocate ( col(5) )

      call sqlite3_column_query( col(001), 'ID', sqlite_int )
      call sqlite3_column_query( col(002), 'EMM_REG', sqlite_int )
      call sqlite3_column_query( col(003), 'PROVRG', sqlite_int )
      call sqlite3_column_query( col(004), 'MAPEXPCI', sqlite_real )
      call sqlite3_column_query( col(005), 'PROVINCE', sqlite_real )

      call sqlite3_prepare_select( db, 'V_NUGPIPE_MAP', col, stmt)

!     LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS

      i = 0

      do
         i = i + 1
         call sqlite3_next_row( stmt, col, finished )
         if ( finished ) exit

         call sqlite3_get_column( col(001), t_id)
         call sqlite3_get_column( col(002), irg)
         call sqlite3_get_column( col(003), crg)
         call sqlite3_get_column( col(004), t_mapexpci)
         call sqlite3_get_column( col(005), T_EXPCI)

         WRITE(18,2004) CURIRUN, CURCALYR, t_id, irg, crg, crg-MNUMNR, t_mapexpci, T_EXPCI, MAPEXPCI(irg,crg-MNUMNR)
 2004    FORMAT(1X,"V_NUGPIPE_MAPEXPCI",6(",",I5),3(",",F21.6))

         MAPEXPCI(irg,crg-MNUMNR) = t_mapexpci

      end do

      deallocate(col)

!     QUERY THE DATABASE TABLE V_EMM_NUGPIPE

      allocate ( col(2) )

      call sqlite3_column_query( col(001), 'EMM_REG', sqlite_int )
      call sqlite3_column_query( col(002), 'IGENGN', sqlite_real )

      call sqlite3_prepare_select( db, 'V_EMM_NUGPIPE_IGENGN', col, stmt)

!     LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS

      i = 0

      do
         i = i + 1
         call sqlite3_next_row( stmt, col, finished )
         if ( finished ) exit

         call sqlite3_get_column( col(001), irg)
         call sqlite3_get_column( col(002), T_IGENGN)

         iyr = year - UHBSYR

         WRITE(18,2008) CURIRUN, CURCALYR, irg, IGENGN(irg), T_IGENGN
 2008    FORMAT(1X,"V_EMM_NUGPIPE_IGENGN",3(",",I5),2(",",F21.6))

         IGENGN(irg) = T_IGENGN

      end do

      deallocate(col)

!     QUERY THE DATABASE TABLE V_EMM_NUGPIPE_SECT

      allocate ( col(4) )

      call sqlite3_column_query( col(001), 'EMM_REG', sqlite_int )
      call sqlite3_column_query( col(002), 'CEN_RGN', sqlite_int )
      call sqlite3_column_query( col(003), 'ISHARE', sqlite_real )
      call sqlite3_column_query( col(004), 'CSHARE', sqlite_real )

      call sqlite3_prepare_select( db, 'V_EMM_NUGPIPE_SECT', col, stmt)

!     LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS

      i = 0

      do
         i = i + 1
         call sqlite3_next_row( stmt, col, finished )
         if ( finished ) exit

         call sqlite3_get_column( col(001), irg)
         call sqlite3_get_column( col(002), crg)
         call sqlite3_get_column( col(003), T_ISHARE)
         call sqlite3_get_column( col(004), T_CSHARE)

         iyr = year - UHBSYR

         WRITE(18,2009) CURIRUN, CURCALYR, irg, crg, ISHARE(irg,crg), T_ISHARE, CSHARE(irg,crg), T_CSHARE
 2009    FORMAT(1X,"V_EMM_NUGPIPE_SECT",4(",",I5),4(",",F21.6))

         ISHARE(irg,crg) = T_ISHARE 
         CSHARE(irg,crg) = T_CSHARE

      end do

      deallocate(col)

!     READ Canadian Import Supply Opportunity Set
!
      READ(UF_NUG, *) DUMMY
      READ(UF_NUG, * ) DUMMY,DOLLAR_YEAR
      READ(UF_NUG, *) DUMMY
!
!     new Canadian supply curves
!
      MIN_YR = UNYEAR
      MAX_YR = 0
!
  25  READ(UF_NUG,*,END=26)TYR,TPROV,TPRC,TGRP,(TGW(ISEA),ISEA=1,ECIns)
!
      MIN_YR = min(MIN_YR,TYR-UHBSYR)
      MAX_YR = max(MAX_YR,TYR-UHBSYR)

      JYR = TYR - UHBSYR
      JSLC = TGRP
!
!     get province index
!
      JPROV = 0
      DO IJ = 1, EFD_D_PROV   !get prov number
         IF (TPROV .EQ. PROVA(IJ) ) THEN
             JPROV = PROVN(IJ)
         END IF
      END DO
      IF (JPROV .EQ. 0)write(*,*)'ERROR - Canadian Supply - BAD PROVINCE'
!
!     get step index
!
      JSTEP = 0
      HIGHSTEP = 0
      FOUND_SW = 0
      MAX_CC = 0.0
      DO IJ = 1 , EFD_D_CSS
         IF (CAN_CST(IJ,JPROV,JYR) .gt. 0.0) HIGHSTEP = HIGHSTEP + 1
         MAX_CC = MAX(MAX_CC, CAN_CST(IJ,JPROV,JYR))
         IF (TPRC / MC_JPGDP(DOLLAR_YEAR - BASEYR + 1 ) .eq. CAN_CST(IJ,JPROV,JYR)) THEN
            JSTEP = IJ
            FOUND_SW = 1
         END IF
      END DO
      IF (MAX_CC .le. 0.01) JSTEP = 1
      IF (FOUND_SW .eq. 0) JSTEP = HIGHSTEP + 1
      IF (JSTEP .eq. 0) write(*,*)'ERROR - Canadian Supply - PRICE/STEP ERROR'

!    write(*,266)'CANSUPPLY,yr,prov,prc,grp ',TYR,TPROV,TPRC,TGRP,JYR,JPROV,JSTEP,JGRP
!266  format(1x,a27, 1x,i4,1x,a2,1x,f5.1,1x,i4,4(1x,i3))

      CAN_CST(JSTEP,JPROV,JYR) = TPRC  / MC_JPGDP(DOLLAR_YEAR - BASEYR + 1 )

!     write(*,267)'deflat cancst, tprc,cancst',TPRC,DOLLAR_YEAR,CAN_CST(JSTEP,JPROV,JYR)
!267  format(1x,a28,1x,3F10.2)

!     convert season and slice to group and segment
!
      DO ISEA= 1 , ECIns
         JSEG = MAP_ECI_SEG(JSLC,ISEA)
         JGRP = MAP_ECI_GRP(JSLC,ISEA)
         EXGW(JSEG,JGRP,JSTEP,JPROV,JYR) = TGW(ISEA)
         WRITE(18,9337) CURIYR+UHBSYR,JYR+UHBSYR,JPROV,JSTEP,ISEA,JGRP,JSEG,EXGW(JSEG,JGRP,JSTEP,JPROV,JYR)
 9337    FORMAT(1X,"EXGW",7(":",I4), ":",E13.6)
      ENDDO
!
      GO TO 25
26    CONTINUE
!
!     Create Mapping of Canadian Supplies at ECI Slices to ECP Slices and EFD Slices
!
      ECP_GW = 0.0
      EFD_GW = 0.0
!
      DO JYR = MIN_YR , MAX_YR
         DO JPROV = 1 , EFD_D_PROV
            DO JSTEP = 1 , EFD_D_CSS
               DO ECP_GRP = 1 , ECPnumSg
                  DO ECP_SEG = 1 , ECPsgDnB(ECP_GRP)
                     DO ECI_GRP = 1 , ECInumSg
                        DO ECI_SEG = 1 , ECIsgDnB(ECI_GRP)
                           ECP_GW(ECP_SEG,ECP_GRP,JSTEP,JPROV,JYR) = ECP_GW(ECP_SEG,ECP_GRP,JSTEP,JPROV,JYR) + EXGW(ECI_SEG,ECI_GRP,JSTEP,JPROV,JYR) * HrsInECIEcpSegs(ECI_GRP,ECI_SEG,ECP_GRP,ECP_SEG)

                         IF ( PRTDBGE .GT. 0 ) THEN
                           WRITE(18,8331) CURIYR+UHBSYR,JYR+UHBSYR,JPROV,JSTEP,ECP_GRP,ECP_SEG,ECI_GRP,ECI_SEG,ECP_GW(ECP_SEG,ECP_GRP,JSTEP,JPROV,JYR),EXGW(ECI_SEG,ECI_GRP,JSTEP,JPROV,JYR), &
                              HrsInECIEcpSegs(ECI_GRP,ECI_SEG,ECP_GRP,ECP_SEG)
                         ENDIF

 8331                      FORMAT(1X,"HrsInECIEcpSegs",8(":",I4),3(":",F15.6))
                        END DO
                     END DO
                  END DO
               END DO
               DO ECP_GRP = 1 , ECPnumSg
                  DO ECP_SEG = 1 , ECPsgDnB(ECP_GRP)
                     ECP_GW(ECP_SEG,ECP_GRP,JSTEP,JPROV,JYR) = ECP_GW(ECP_SEG,ECP_GRP,JSTEP,JPROV,JYR) / HrsInECIEcpSegs(0,0,ECP_GRP,ECP_SEG)

                     ECP_SP = MAP_ECP_SP(ECP_GRP)
                     WRITE(18,9331) CURIYR+UHBSYR,JYR+UHBSYR,JPROV,JSTEP,ECP_SP,ECP_GRP,ECP_SEG,ECP_GW(ECP_SEG,ECP_GRP,JSTEP,JPROV,JYR)
 9331                FORMAT(1X,"ECP_GW",7(":",I4), ":",E13.6)

                  END DO
               END DO

               DO EFD_GRP = 1 , EFDnumSg
                  DO EFD_SEG = 1 , EFDsgDnB(EFD_GRP)
                     DO ECI_GRP = 1 , ECInumSg
                        DO ECI_SEG = 1 , ECIsgDnB(ECI_GRP)
                           EFD_GW(EFD_SEG,EFD_GRP,JSTEP,JPROV,JYR) = EFD_GW(EFD_SEG,EFD_GRP,JSTEP,JPROV,JYR) + EXGW(ECI_SEG,ECI_GRP,JSTEP,JPROV,JYR) * HrsInECIEfdSegs(ECI_GRP,ECI_SEG,EFD_GRP,EFD_SEG)

                         IF ( PRTDBGE .GT. 0 ) THEN
                           WRITE(18,8332) CURIYR+UHBSYR,JYR+UHBSYR,JPROV,JSTEP,EFD_GRP,EFD_SEG,ECI_GRP,ECI_SEG,EFD_GW(EFD_SEG,EFD_GRP,JSTEP,JPROV,JYR),EXGW(ECI_SEG,ECI_GRP,JSTEP,JPROV,JYR), &
                              HrsInECIEfdSegs(ECI_GRP,ECI_SEG,EFD_GRP,EFD_SEG)
                         ENDIF

 8332                      FORMAT(1X,"HrsInECIEfdSegs",8(":",I4),3(":",F15.6))
                        END DO
                     END DO
                  END DO
               END DO
               DO EFD_GRP = 1 , EFDnumSg
                  DO EFD_SEG = 1 , EFDsgDnB(EFD_GRP)
                     EFD_GW(EFD_SEG,EFD_GRP,JSTEP,JPROV,JYR) = EFD_GW(EFD_SEG,EFD_GRP,JSTEP,JPROV,JYR) / HrsInECIEfdSegs(0,0,EFD_GRP,EFD_SEG)

                     EFD_SP = MAP_EFD_SP(EFD_GRP)
                     WRITE(18,9332) CURIYR+UHBSYR,JYR+UHBSYR,JPROV,JSTEP,EFD_SP,EFD_GRP,EFD_SEG,EFD_GW(EFD_SEG,EFD_GRP,JSTEP,JPROV,JYR)
 9332                FORMAT(1X,"EFD_GW",7(":",I4), ":",E13.6)

                  END DO
               END DO
            END DO
         END DO
      END DO
!
!     EXTEND CAN_CST,EFD_GW,ECP_GW
!
      IF ((MAX_YR .GT. 0) .and. (MAX_YR .LT. MNUMYR)) THEN
         DO JYR = MAX_YR + 1 , MNUMYR
            DO JPROV = 1 , EFD_D_PROV
               DO JSTEP = 1 , EFD_D_CSS
                  CAN_CST(JSTEP,JPROV,JYR) = CAN_CST(JSTEP,JPROV,MAX_YR)
                  DO JGRP = 1 , EFDnumSg
                     DO JSEG = 1 , EFDsgDnB(JGRP)
                        EFD_GW(JSEG,JGRP,JSTEP,JPROV,JYR) = EFD_GW(JSEG,JGRP,JSTEP,JPROV,MAX_YR)
                     END DO
                  END DO
                  DO ECP_GRP = 1 , ECPnumSg
                     DO ECP_SEG = 1 , ECPsgDnB(ECP_GRP)
                        ECP_GW(ECP_SEG,ECP_GRP,JSTEP,JPROV,JYR) = ECP_GW(ECP_SEG,ECP_GRP,JSTEP,JPROV,MAX_YR)
                     END DO
                  END DO
               END DO
            END DO
         END DO
      END IF
!
!     Read in Constraints from ETTIN File
!
!
      CNSTRNTS = 0.0
      CNSTRNTS_EFD = 0.0
      CNSTRNTS_PREFIRM_EFD = 0.0
      CNSTRNTS_ECP = 0.0
!
!     OPEN AND READ THE CONSTRAINTS FILE
!
      NEW = .FALSE.
      FILENM = 'ETTIN'
      UF_ETTIN = FILE_MGR('O',FILENM,NEW)
!
!     READ IN CONSTRAINTS DATA
!
      DO WHILE (.TRUE.)           ! loop to read
         READ (UF_ETTIN, * ,END = 98) IRGEX, IRGIM, YR, PTHRESH1(YR,IRGEX,IRGIM), PTHRESH2(YR,IRGEX,IRGIM), (TEMPCNS(I),I = 1 , ETTns),  &
                                     (TEMPCNSPF(I),I=1,ETTns)
!
         DO ISP = 1 , ETTns
            CNSTRNTS(ISP,YR,IRGIM,IRGEX) = CNSTRNTS(ISP,YR,IRGIM,IRGEX) +  TEMPCNS(ISP)
            CNSTRNTS_PREFIRM(ISP,YR,IRGIM,IRGEX) = CNSTRNTS_PREFIRM(ISP,YR,IRGIM,IRGEX) +  TEMPCNSPF(ISP)
         END DO ! isp
      END DO !  READ
!
 98   CONTINUE

      DO IRGEX = 1 ,  MNUMNR + EFD_D_PROV
         DO IRGIM = 1 , MNUMNR + EFD_D_PROV
            DO YR = 1 , MNUMYR

!              Map to EFD Seasons

               DO EFD_SP = 1 , EFDns
                  IF (HrsInETTEfdSPs(0,EFD_SP) .GT. 0.0) THEN
                     DO ETT_SP = 1 , ETTns
                        CNSTRNTS_EFD(EFD_SP,YR,IRGIM,IRGEX) = CNSTRNTS_EFD(EFD_SP,YR,IRGIM,IRGEX) + CNSTRNTS(ETT_SP,YR,IRGIM,IRGEX) * HrsInETTEfdSPs(ETT_SP,EFD_SP)
                        CNSTRNTS_PREFIRM_EFD(EFD_SP,YR,IRGIM,IRGEX) = CNSTRNTS_PREFIRM_EFD(EFD_SP,YR,IRGIM,IRGEX) + &
                                CNSTRNTS_PREFIRM(ETT_SP,YR,IRGIM,IRGEX) * HrsInETTEfdSPs(ETT_SP,EFD_SP)
                     END DO
                  END IF
               END DO

               TST_VAL = 0.0
               DO EFD_SP = 1 , EFDns
                  IF (HrsInETTEfdSPs(0,EFD_SP) .GT. 0.0) THEN
                     CNSTRNTS_EFD(EFD_SP,YR,IRGIM,IRGEX) = CNSTRNTS_EFD(EFD_SP,YR,IRGIM,IRGEX) / HrsInETTEfdSPs(0,EFD_SP)
                     CNSTRNTS_PREFIRM_EFD(EFD_SP,YR,IRGIM,IRGEX) = CNSTRNTS_PREFIRM_EFD(EFD_SP,YR,IRGIM,IRGEX) / HrsInETTEfdSPs(0,EFD_SP)
                     TST_VAL = TST_VAL + CNSTRNTS_EFD(EFD_SP,YR,IRGIM,IRGEX)
                  END IF
               END DO

               IF (TST_VAL .GT. 0.0) THEN
                  WRITE(18,6371) CURIYR+UHBSYR,CURITR,YR+1989,IRGEX,IRGIM,(CNSTRNTS_EFD(EFD_SP,YR,IRGIM,IRGEX),EFD_SP=1,EFDns)
 6371             FORMAT(1X,"CNSTRNTS_EFD",5(":",I4),6(":",F12.6))
               END IF

!              Map to ECP Seasons

               DO ECP_SP = 1 , ECPns
                  IF (HrsInETTEcpSPs(0,ECP_SP) .GT. 0.0) THEN
                     DO ETT_SP = 1 , ETTns
                        CNSTRNTS_ECP(ECP_SP,YR,IRGIM,IRGEX) = CNSTRNTS_ECP(ECP_SP,YR,IRGIM,IRGEX) + CNSTRNTS(ETT_SP,YR,IRGIM,IRGEX) * HrsInETTEcpSPs(ETT_SP,ECP_SP)
                     END DO
                  END IF
               END DO

               TST_VAL = 0.0
               DO ECP_SP = 1 , ECPns
                  IF (HrsInETTEcpSPs(0,ECP_SP) .GT. 0.0) THEN
                     CNSTRNTS_ECP(ECP_SP,YR,IRGIM,IRGEX) = CNSTRNTS_ECP(ECP_SP,YR,IRGIM,IRGEX) / HrsInETTEcpSPs(0,ECP_SP)
                     TST_VAL = TST_VAL + CNSTRNTS_ECP(ECP_SP,YR,IRGIM,IRGEX)
                  END IF
               END DO

               IF (TST_VAL .GT. 0.0) THEN
                  WRITE(18,6372) CURIYR+UHBSYR,CURITR,YR+1989,IRGEX,IRGIM,(CNSTRNTS_ECP(ECP_SP,YR,IRGIM,IRGEX),ECP_SP=1,ECPns)
 6372             FORMAT(1X,"CNSTRNTS_ECP",5(":",I4),6(":",F12.6))
               END IF
            END DO
         END DO
      END DO

      call sqlite3_close( db )

      RETURN
      END
!
!
      SUBROUTINE LOADNG(IRG,IYR)
      IMPLICIT NONE

!     THIS SUBROUTINE ADDS END USE GENERATION, REVENUES, AND COSTS TO EMM VARIABLES

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'dispuse'
      include 'dispin'
      include 'dispout'
      include 'dispett'
      include 'cogen'
      include 'postpr'
      include 'elcntl'
      include 'elout'
      include 'efpout'
      include 'ecpcntl'
      include 'uecpout'
!
!
      INTEGER IRG,IYR,I,J,CRG,K,L,IFL,IDSP,IVIN
      REAL EWGAVP,EWGAVP2,TOTGENC,TOTGENRS,TOTGENE,TOTGENI,TOTGENR, &
       TOTGEN,TOTREV,HOMALL,INTNP,EWGP,COMP,INDPI,RNWP, &
       INTNGEN,INTNCOST
      REAL TOTCAPC,TOTCAPRS,TOTCAPE,TOTCAPI,TOTCAPR,RNWGEN
      REAL LOCEWG,LOCRIC,LOCRCC,LOCRRC,LOCRNW

!     TC_FUELS has been moved to emmparm
!     Moved F_CL - F_WN to ecpcntl and moved the assignment to RDCNTRL in udat.f

!     INTEGER*4 F_CL                 !  1 - Coal
!     INTEGER*4 F_OL                 !  2 - Oil
!     INTEGER*4 F_OG                 !  3 - Other Gaesous Fuels
!     INTEGER*4 F_NG                 !  4 - Natural Gas
!     INTEGER*4 F_HY                 !  5 - Hydro
!     INTEGER*4 F_GT                 !  6 - Geothermal
!     INTEGER*4 F_MS                 !  7 - MSW
!     INTEGER*4 F_WD                 !  8 - Biomass
!     INTEGER*4 F_SO                 !  9 - Solar Thermal
!     INTEGER*4 F_PV                 ! 10 - Solar PV
!     INTEGER*4 F_OT                 ! 11 - Other
!     INTEGER*4 F_WN                 ! 12 - Wind

!     TC_SECTORS has been moved to emmparm

      INTEGER*4 S_IND                !  1 - Industrial
      INTEGER*4 S_COM                !  2 - Commercial
      INTEGER*4 S_RES                !  3 - Residential
      INTEGER*4 S_OG                 !  4 - Enhanced Oil and Gas Recovery
      INTEGER*4 S_REF                !  5 - Refinery
      INTEGER*4 IECP
      REAL  RPSPRCR, RPSPRCC, RPSPRCI
      INTEGER*4 MAP_ECP_NDX(TC_FUELS)             ! For Renewables Map to ECP Indexs

!     MOVED THE COMMON CGGEN to ecpcntl

!     REAL*8 CG_GEN_I(MNUMNR,TC_FUELS),CG_GEN_C(MNUMNR,TC_FUELS),CG_GEN_R(MNUMNR,TC_FUELS), &
!            CG_GEN_O(MNUMNR,TC_FUELS),CG_GEN_F(MNUMNR,TC_FUELS)
!     COMMON /CGGEN/ CG_GEN_I,CG_GEN_C,CG_GEN_R,CG_GEN_O,CG_GEN_F
!
!     FUEL DEFINITIONS

!     F_CL  =  1   ! Coal
!     F_OL  =  2   ! Oil
!     F_OG  =  3   ! Other Gaesous Fuels
!     F_NG  =  4   ! Natural Gas
!     F_HY  =  5   ! Hydro
!     F_GT  =  6   ! Geothermal
!     F_MS  =  7   ! MSW
!     F_WD  =  8   ! Biomass
!     F_SO  =  9   ! Solar Thermal
!     F_PV  = 10   ! Solar PV
!     F_OT  = 11   ! Other
!     F_WN  = 12   ! Wind

      MAP_ECP_NDX = 0

      MAP_ECP_NDX(F_HY) = WIHY
      MAP_ECP_NDX(F_GT) = WIGT
      MAP_ECP_NDX(F_MS) = WIMS
      MAP_ECP_NDX(F_WD) = WIWD
      MAP_ECP_NDX(F_SO) = WISO
      MAP_ECP_NDX(F_PV) = WIPV
      MAP_ECP_NDX(F_WN) = WIWN

!     DELETE THESE NEXT 4 INITIALIZATIONS WHEN FIXED COSTS
!     START COMING THROUGH FROM ECP.
!
      EWGRNW = 0
      EWGRIC = 0
      EWGRCC = 0
      EWGRRC = 0
      EWGREV = 0
      LOCEWG = 0
      LOCRIC = 0
      LOCRCC = 0
      LOCRRC = 0
      LOCRNW = 0

      EWGAVP = 0.0
      TOTGENC = 0.0
      TOTGENRS = 0.0
      TOTGENE = 0.0
      TOTGENI = 0.0
      TOTGENR = 0.0
      TOTGEN = 0.0
      TOTREV = 0.0
      RNWGEN = 0.0
      RPSPRCC = 0.0
      RPSPRCI = 0.0
      RPSPRCR = 0.0

      TOTCAPC = 0.0
      TOTCAPRS = 0.0
      TOTCAPE = 0.0
      TOTCAPI = 0.0
      TOTCAPR = 0.0

      HOMALL = 0
      INTNP = 0
      EWGP = 0
      COMP = 0
      INDPI = 0
!     INDPO=0
      RNWP = 0
      INTNGEN = 0
      INTNCOST = 0


!     CALCULATE RENEWABLES NUGS REVENUES
      DO 90 I = 1,EFD_D_RNW
         DO 92 J = 1,USW_OWN
            HOMALL = HOMALL + ERHOM(I,J)
92       CONTINUE
         LOCRNW = LOCRNW + ERHOM(I,3)
90    CONTINUE

      EWGRNW = EWGRNW + LOCRNW

      DO 170 I = 1,EFD_D_RNW
         TOTGENR = TOTGENR + EQHGN(I,3)
         INTNGEN = INTNGEN + EQHGN(I,1) + EQHGN(I,2)
         DO 160 J = 1, USW_OWN
           RNWGEN = RNWGEN + EQHGN(I,J)
160      ENDDO
170   CONTINUE

      DO 100 I = 1,EFD_D_NFL
         TOTGENE = TOTGENE + EQFGN(I,3)
         INTNGEN = INTNGEN + EQFGN(I,1) + EQFGN(I,2)
         DO 102 J = 1,USW_OWN
            TOTGEN = TOTGEN + EQFGN(I,J)
102      CONTINUE
100   CONTINUE

      DO 104 I = 1,USW_OWN
         TOTREV = TOTREV + (ERTFL(I) + ERTOM(I) )
104   CONTINUE
      INTNCOST = INTNCOST + &
       (ERTFL(1) + ERTFL(2) + ERTOM(1) + ERTOM(2)) + (HOMALL - EWGRNW)

!     CALCULATE AVERAGE PRICE
      IF(TOTGEN .NE. 0) THEN
         EWGAVP = (TOTREV - HOMALL)/TOTGEN
         EWGAVP2 = TOTREV/(TOTGEN + RNWGEN)
      ELSE
         EWGAVP = 0.0
         EWGAVP2 = 0.0
      ENDIF
       write(UF_DBG,326)  CURIYR,CURITR,IRG,TOTGEN,RNWGEN,TOTREV,HOMALL,EWGAVP,EWGAVP2,EWSPRCN(IRG,IYR)*0.001
326    format(1x,'EWGAVP ',3I6,4F16.2,3F10.6)
!     replace average price with average wholesale price
      IF (CURITR .GT. 1)   &
        EWGAVP = EWSPRCN(IRG,IYR)*0.001

!     CALCULATE EWG REVENUES
      LOCEWG = LOCEWG + ERTFL(3) + ERTOM(3)


      EWGREV = EWGFIX + LOCEWG

!     WRITE(UF_DBG,1403)'IYR',IYR,'IRG',IRG,'EWGFIX',EWGFIX,'LOCEWG',
!     +   LOCEWG,'EWGREV',EWGREV,'GEN',TOTGENE
!1403      FORMAT(2(A4,1X,I4,1X),4(A8,1X,F8.2,1X))
!     WRITE(UF_DBG,1403)'IYR',IYR,'IRG',IRG,'ERTFL',ERTFL(3),'ERTOM',ERTOM(3)

!    CALCULATE TOTAL COGEN GENERATION AND RPS COSTS
     DO IFL = 1, TC_FUELS
        IECP = MAP_ECP_NDX(IFL)
        IF (IECP .EQ. 0) THEN
           TOTGENRS = TOTGENRS + CG_GEN_R(IRG,IFL)
           TOTGENC = TOTGENC + CG_GEN_C(IRG,IFL)
           TOTGENI = TOTGENI + CG_GEN_I(IRG,IFL) + CG_GEN_O(IRG,IFL) + CG_GEN_F(IRG,IFL)

          IF (UPRNWREG .LE. 1) THEN
           RPSPRCR = RPSPRCR + CG_GEN_R(IRG,IFL) * EPRPSPR(CURIYR) * UPRNWBND(CURIYR)
           RPSPRCC = RPSPRCC + CG_GEN_C(IRG,IFL) * EPRPSPR(CURIYR) * UPRNWBND(CURIYR)
           RPSPRCI = RPSPRCI + CG_GEN_I(IRG,IFL) * EPRPSPR(CURIYR) * UPRNWBND(CURIYR)  &
                             + CG_GEN_O(IRG,IFL) * EPRPSPR(CURIYR) * UPRNWBND(CURIYR)  &
                             + CG_GEN_F(IRG,IFL) * EPRPSPR(CURIYR) * UPRNWBND(CURIYR)
          ELSE
           RPSPRCR = RPSPRCR + CG_GEN_R(IRG,IFL) * EPRPSPRR(IRG,CURIYR) * UPRNWBNDR(CURIYR,IRG)
           RPSPRCC = RPSPRCC + CG_GEN_C(IRG,IFL) * EPRPSPRR(IRG,CURIYR) * UPRNWBNDR(CURIYR,IRG)
           RPSPRCI = RPSPRCI + CG_GEN_I(IRG,IFL) * EPRPSPRR(IRG,CURIYR) * UPRNWBNDR(CURIYR,IRG)  &
                             + CG_GEN_O(IRG,IFL) * EPRPSPRR(IRG,CURIYR) * UPRNWBNDR(CURIYR,IRG)  &
                             + CG_GEN_F(IRG,IFL) * EPRPSPRR(IRG,CURIYR) * UPRNWBNDR(CURIYR,IRG)
          ENDIF

        ELSE
           TOTGENRS = TOTGENRS + CG_GEN_R(IRG,IFL)
           TOTGENC = TOTGENC + CG_GEN_C(IRG,IFL)
           TOTGENI = TOTGENI + CG_GEN_I(IRG,IFL) + CG_GEN_O(IRG,IFL) + CG_GEN_F(IRG,IFL)

          IF (UPRNWREG .LE. 1) THEN
           RPSPRCR = RPSPRCR + CG_GEN_R(IRG,IFL) * EPRPSPR(CURIYR) * (UPRNWBND(CURIYR) - UPRNWSHR(IECP))
           RPSPRCC = RPSPRCC + CG_GEN_C(IRG,IFL) * EPRPSPR(CURIYR) * (UPRNWBND(CURIYR) - UPRNWSHR(IECP))
           RPSPRCI = RPSPRCI + CG_GEN_I(IRG,IFL) * EPRPSPR(CURIYR) * (UPRNWBND(CURIYR) - UPRNWSHR(IECP))  &
                             + CG_GEN_O(IRG,IFL) * EPRPSPR(CURIYR) * (UPRNWBND(CURIYR) - UPRNWSHR(IECP))  &
                             + CG_GEN_F(IRG,IFL) * EPRPSPR(CURIYR) * (UPRNWBND(CURIYR) - UPRNWSHR(IECP))
          ELSE
           RPSPRCR = RPSPRCR + CG_GEN_R(IRG,IFL) * EPRPSPRR(IRG,CURIYR) * (UPRNWBNDR(CURIYR,IRG) - UPRNWSHRR(IECP,IRG))
           RPSPRCC = RPSPRCC + CG_GEN_C(IRG,IFL) * EPRPSPRR(IRG,CURIYR) * (UPRNWBNDR(CURIYR,IRG) - UPRNWSHRR(IECP,IRG))
           RPSPRCI = RPSPRCI + CG_GEN_I(IRG,IFL) * EPRPSPRR(IRG,CURIYR) * (UPRNWBNDR(CURIYR,IRG) - UPRNWSHRR(IECP,IRG))  &
                             + CG_GEN_O(IRG,IFL) * EPRPSPRR(IRG,CURIYR) * (UPRNWBNDR(CURIYR,IRG) - UPRNWSHRR(IECP,IRG))  &
                             + CG_GEN_F(IRG,IFL) * EPRPSPRR(IRG,CURIYR) * (UPRNWBNDR(CURIYR,IRG) - UPRNWSHRR(IECP,IRG))
          ENDIF

        ENDIF
      ENDDO

!  NT Cogen added to EFP IPP costs
!     DO IFL = 1,EFD_D_NFL
!        TOTGENI = TOTGENI + UQFGENN(IFL,IRG,4)
!       IF (UPRNWREG .LE. 1) THEN
!        RPSPRCI = RPSPRCI + UQFGENN(IFL,IRG,4) * EPRPSPR(CURIYR) * UPRNWBND(CURIYR)
!       ELSE
!        RPSPRCI = RPSPRCI + UQFGENN(IFL,IRG,4) * EPRPSPRR(IRG,CURIYR) * UPRNWBNDR(CURIYR,IRG)
!       ENDIF
!     END DO

      IF (TOTGENRS .GT. 0.0) RPSPRCR = RPSPRCR / TOTGENRS * 0.001
      IF (TOTGENC  .GT. 0.0) RPSPRCC = RPSPRCC / TOTGENC * 0.001
      IF (TOTGENI  .GT. 0.0) RPSPRCI = RPSPRCI / TOTGENI * 0.001

      WRITE(UF_DBG,1212) CURIYR,CURITR,IRG,RPSPRCR,RPSPRCC,RPSPRCI
1212  FORMAT(1X,'RPSPRC ',3I5,3F12.6)


!     CALCULATE RESIDENTIAL NUGS REVENUES (USE COM SHARES--NO RES)

!     DO I = 1,9
!       DO J = 1,10
!         TOTGENRS = TOTGENRS + CSHARE(IRG,I) * CGRESGEN(I,IYR,J,1)
!         if ( &
!           (CGRESGEN(I,IYR,J,1) + CGRESGEN(I,IYR,J,2)) * CGRESCAP(I,IYR,J) &
!           .NE. 0.0) &
!         TOTCAPRS = TOTCAPRS + CSHARE(IRG,I) * (CGRESGEN(I,IYR,J,1) / &
!           (CGRESGEN(I,IYR,J,1) + CGRESGEN(I,IYR,J,2))) * (CGRESCAP(I,IYR,J)/1000)
!       END DO
!     END DO

!     TOTGENRS = CG_GEN_R(IRG)

      LOCRRC = LOCRRC + ((EWGAVP + RPSPRCR) * TOTGENRS)

      EWGRRC = EWGRRC + LOCRRC

!     CALCULATE COMMERCIAL NUGS REVENUES

!     DO 110 I = 1,9
!        DO 112 J = 1,10
!           TOTGENC = TOTGENC + &
!            ( CSHARE(IRG,I) * (GRIDSHR(I,IYR) * (CGCOMGEN(I,IYR,J))) )

!           DO 114 K = 1,2
!              TOTCAPC = TOTCAPC + &
!               (CSHARE(IRG,I) * (GRIDSHR(I,IYR) * &
!                (CGCOMCAP(I,IYR,J,K)/1000)) )
!114         CONTINUE
!112      CONTINUE
!110   CONTINUE

!     TOTGENC = CG_GEN_C(IRG)

      LOCRCC = LOCRCC + ((EWGAVP + RPSPRCC) * TOTGENC)

      EWGRCC = EWGRCC + LOCRCC

!     CALCULATE INDUSTRIAL NUGS REVENUES
!     AS SUM OF INDUSTRIAL COGEN PLUS COGEN - OTHER AND NON-TRAD COGEN

      DO 3420 I = 1,10
         DO 3410 J = 1,2
!           WRITE(6,2504)'A-CGINDLGEN',(CGINDLGEN(CRG,IYR,I,J),CRG=1,9)
2504        FORMAT(A9,1X,9(F9.2,1X))
3410     CONTINUE
3420  CONTINUE

!     TOTGENI = TOTGENI + &
!      ( IGENGN(IRG) * (CGINDLGEN(11,IYR,1,1) + &
!      CGINDLGEN(11,IYR,2,1) + &
!      CGINDLGEN(11,IYR,3,1) + &
!      CGINDLGEN(11,IYR,4,1) + &
!      CGINDLGEN(11,IYR,5,1) + &
!      CGINDLGEN(11,IYR,6,1) + &
!      CGINDLGEN(11,IYR,7,1) + &
!      CGINDLGEN(11,IYR,8,1) + &
!      CGINDLGEN(11,IYR,9,1) + &
!      CGINDLGEN(11,IYR,10,1) + &
!      CGREGEN(11,IYR,1,1) + &
!      CGREGEN(11,IYR,2,1) + &
!      CGREGEN(11,IYR,3,1) + &
!      CGREGEN(11,IYR,4,1) + &
!      CGREGEN(11,IYR,5,1) + &
!      CGOGSGEN(11,IYR,1,1) + &
!      CGOGSGEN(11,IYR,2,1) + &
!      CGOGSGEN(11,IYR,3,1) + &
!      CGOGSGEN(11,IYR,4,1)))

!     TOTGENI = CG_GEN_I(IRG) + CG_GEN_O(IRG) + CG_GEN_F(IRG)
!     DO IFL = 1,EFD_D_NFL
!        TOTGENI = TOTGENI + UQFGENN(IFL,IRG,4)
!     END DO

!     DO 140 I = 1,10
!           TOTCAPI = TOTCAPI + (ISHARE(IRG,K) * CGINDLCAP(K,IYR,I,1)/1000)
!140   CONTINUE

!     ADD NON-TRADITIONAL COGEN TO CAPACITY CHARGES
!
!     DO 555 IDSP = 1,EFD_D_DSP
!        DO 565 IVIN = 1,EFD_D_VIN
!           TOTCAPI = TOTCAPI + (ECSCAP(IDSP,IVIN,4)/1000)
!565      CONTINUE
!555   CONTINUE

135   CONTINUE

      LOCRIC = LOCRIC + ((EWGAVP + RPSPRCI) * TOTGENI)

      EWGRIC = EWGRIC + LOCRIC


      IF(FCRL .EQ. 1) THEN
         WRITE(UF_ETT,366)IYR,IRG,'9',TOTCAPI,TOTCAPC
366      FORMAT(I4,1X,I4,1X,A5,4(F10.4,1X))

         WRITE(UF_ETT,355)IYR,IRG,'1',INTNGEN,TOTGENE,TOTGENC, &
          TOTGENI,TOTGENR,TOTGENRS
350      FORMAT(I4,1X,I4,1X,A5,6(F10.2,1X))


         WRITE(UF_ETT,355)IYR,IRG,'2',INTNCOST,EWGREV,EWGRCC, &
          EWGRIC,EWGRNW,EWGRRC
355      FORMAT(I4,1X,I4,1X,A5,4(F10.2,1X),11X,2F10.2,1X)

      ENDIF

      IF(INTNGEN .NE. 0) THEN
         INTNP = INTNCOST/INTNGEN
      ELSE
         INTNP = 0
      ENDIF
      IF(TOTGENE .NE. 0) THEN
         EWGP = EWGREV/TOTGENE
      ELSE
         EWGP = 0
      ENDIF
      COMP = EWGAVP
      INDPI = EWGAVP
!     INDPO=CGOTPV
      IF(TOTGENR .NE. 0) THEN
         RNWP = EWGRNW/TOTGENR
      ELSE
         RNWP = 0
      ENDIF
      IF(FCRL .EQ. 1) THEN
         WRITE(UF_ETT,355)IYR,IRG,'3',INTNP,EWGP,COMP,INDPI,RNWP
      ENDIF


!     LOAD CANADIAN INTERRUPTIBLE GLOBAL DATA INTO DISPOUT VARIABLES

!  these are filled in by the EFD
!     ETIMPD = (EWGAVP2 * ETIMPE)
!     ETEXPD = (EWGAVP2 * ETEXPE)
      ULEIXR(IRG) = ULEIXE(IRG) * EWGAVP

      RETURN
      END


      SUBROUTINE RDCOGEN
      IMPLICIT NONE

!     THIS SUBROUTINE IS NONUTILITY (END USE) GENERATION FOR CANADA ONLY

      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'dispin'
      include 'cogen'
      include 'postpr'
      include 'wrenew'
      include 'dispinyr'
!
!
      LOGICAL NEW
      INTEGER I,J,K,CRG
      CHARACTER*18 FILENM
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
!
!     RDCOGEN.CMN READ COMMERCIAL AND INDUSTRIAL FOR PC AND CANADA ONLY RUNS
!
!     OPEN THE COGEN FILE
!

      FILENM = 'COGENMF'
      NEW = .FALSE.
      UF_TMP=FILE_MGR('O',FILENM,NEW)
      WRITE(6,'("  Reading ",A," file")') TRIM(FILENM)

!     READ IN COGEN COMMERCIAL & INDUSTRIAL

      DO 9430 J = 1,10
         DO 9432 I = 1,MNUMYR
            READ(UF_TMP, * ) (CGCOMMGEN(CRG,I,J,2),CRG = 1,MNUMCR)
9432     CONTINUE
9430  CONTINUE

      DO 9434 K = 1,2
         DO 9436 J = 1,10
            DO 9438 I = 1,MNUMYR
               READ(UF_TMP, * ) (CGINDLGEN(CRG,I,J,K),CRG = 1,MNUMCR)
9438        CONTINUE
9436     CONTINUE
9434  CONTINUE

      DO 9334 K = 1,2
         DO 9336 J = 1,5
            DO 9338 I = 1,MNUMYR
               READ(UF_TMP, * ) (CGREFGEN(CRG,I,J,K),CRG = 1,MNUMCR)
9338        CONTINUE
9336     CONTINUE
9334  CONTINUE

      DO 9234 K = 1,2
         DO 9236 J = 1,4
            DO 9238 I = 1,MNUMYR
               READ(UF_TMP, * ) (CGOGSGEN(CRG,I,J,K),CRG = 1,MNUMCR)
9238        CONTINUE
9236     CONTINUE
9234  CONTINUE

!      DO 9230 I = 1,MNUMYR
!         READ(UF_TMP, * ) (GRIDSHR(CRG,I),CRG = 1,MNUMCR)
!9230  CONTINUE


      RETURN
      END
