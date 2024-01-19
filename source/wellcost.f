! $Header: m:/default/source/RCS/wellcost.f,v 1.67 2020/07/23 19:55:39 DH5 Exp $
!***************************************************************
!from ECON_COST.FOR
!     Last change:  MC    5 May 2009    9:18 am
        subroutine cost_oil(ires,itech,co2code,ctype,ir)

        implicit none

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
        include 'ogsmbfw'
        include 'ogsmugr'
        include 'ogsml48'
        include 'ogsmout'
        include 'macout'

        LOGICAL ONCE
        INTEGER ires,depthcat,iyr,patlife,nyr,ik,ib,ih,ii,ig, ir
        INTEGER IYR2,igen(max_yr),lyr,kyr,jyr,ix,iyr1,myr
        integer itech,ishift
        INTEGER co2code                                               !code for evaluation of CO2 sources
                                                                      !1: natural only
                                                                      !0: natural and industrial
        INTEGER ctype                                                 !CO2 source number

!--- (NEW) ---------------------------------------------------------

	REAL MINDEPTHoil, MAXDEPTHoil	!depth range variables for output write

! ------------------------------------------------------------------
!
        REAL term,rmaxw,rmaxp,rmax,rmaxi,test,rmaxo,apat,ooip,totpatn               !mc change ver 11
        real tcumresv,tcumresv2                                               !mc change ver 7
        REAL OPRODO(MAX_YR)
        REAL GPRODO(MAX_YR)
        REAL NPRODO(MAX_YR)
        REAL WPRODO(MAX_YR)
        REAL OINJO(MAX_YR)
        REAL WINJO(MAX_YR)
        REAL ORECYO(MAX_YR)
        REAL totgasprod(max_yr)
        REAL temporesv(max_yr)
        REAL tempgresv(max_yr)
        real tempiresv(max_yr)                                                !mc change ver 7
        real tempiresvg(max_yr)                                               !mc change ver 7
        real cstp_drl_fac0(max_tech)                     !PROJECT LEVEL cost to apply drilling technology
        real cstp_stm_fac0(max_tech)                     !PROJECT LEVEL cost to apply stimulation technology
        real cstp_comp_fac0(max_tech)                    !PROJECT LEVEL cost to apply completion technology
        real cstp_fac_fac0(max_tech)                     !PROJECT LEVEL cost to apply facilities technology (
        real cstp_secconv_fac0(max_tech)                 !PROJECT LEVEL cost to apply secondary conversion te
        real cstp_injconv_fac0(max_tech)                 !PROJECT LEVEL cost to apply injector conversion tec
        real cstp_facupg_fac0(max_tech)                  !PROJECT LEVEL cost to apply facilities upgrade tech
        real cstp_wrk_fac0(max_tech)                     !PROJECT LEVEL cost to apply workover technology
        real cstp_foam_fac0(max_tech)                    !PROJECT LEVEL cost to apply fixed annual O & M tech
        real cstp_voam_fac0(max_tech)                    !PROJECT LEVEL cost to apply variable annual O & M t
        real cstp_gna_fac0(max_tech)                     !PROJECT LEVEL cost to apply G & A technology
        real cstp_annsec_fac0(max_tech)                  !PROJECT LEVEL cost to apply secondary producer tech
        real cstp_lft_fac0(max_tech)                     !PROJECT LEVEL cost to apply lifting technology
        real cstp_secwrk_fac0(max_tech)                  !PROJECT LEVEL cost to apply secondary workover tech
        real cstp_injc_fac0(max_tech)                    !PROJECT LEVEL cost to apply injection technology
        real cstp_cmp_fac0(max_tech)                     !PROJECT LEVEL cost to apply compression technology
        real prodtech, prodadj, prodadj2
        real drctech, drcadj
        real leotech, leoadj
        real spacing, wlspacing, wlsdrilled, dfactor


        REAL oil_oam1,gas_oam1,wat_oam1,inj_oam1
        COMMON/NEWcost/oil_oam1,gas_oam1,wat_oam1,inj_oam1

        REAL patdev2(max_yr,max_tech)

        REAL stma,stmp
        INTEGER ic, irmax

!  temporary variables
        INTEGER ist,ll3

        REAL ngen

       DATA ONCE/.FALSE./

!  set temporary data
!!!        itimeyr = 1!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!temporary read statement

!  end setting of temporary data


!  initialization
        nyr = max_yr
        DO IYR =1,nyr
               OPRODO(IYR) = OPROD(IYR)
               GPRODO(IYR) = GPROD(IYR)
               NPRODO(IYR) = NPROD(IYR)
               WPRODO(IYR) = 0.0
               OINJO(IYR)  = 0.0
               WINJO(IYR)  = 0.0
               ORECYO(IYR) = 0.0
               temporesv(iyr) = 0.0
               tempgresv(iyr) = 0.0
               tempiresv(iyr) = 0.0                                            !mc change ver 7
               tempiresvg(iyr) = 0.0                                           !mc change ver 7
          IF(itech.eq.1) igen(IYR) = 0.0
        ENDDO
        ist = stnum
        tcumresv = 0.0                                                         !mc change ver 7
        tcumresv2 = 0.0                                                        !mc change ver 7

        DWC_W    = 0.0
        DRY_W    = 0.0

! ---- (new) for testing only -----------------------------------------------------------------
!
        DNC_COST     = 0.0	   	!initialize New Oil Well D&C Cost Estimate variable
	DNC_COST_DRY = 0.0 		!initialize New Dry Well D&C Cost Estimate variable
!
	MINDEPTHoil =  100			!minimum drilling depth for D&C cost Output file (9934)

	MAXDEPTHoil = 100000			!maximum drilling depth for D&C cost Output file (9934)

! ---------------------------------------------------------------------------------------------

        NPR_W    = 0.0
        WRK_W    = 0.0
        PSW_W    = 0.0
        PSI_W    = 0.0
        FAC_W    = 0.0
        PWP_F    = 0.0
        CHM_F    = 0.0
        PLY_F    = 0.0
        CO2_F    = 0.0
        OMO_W    = 0.0
        OPSEC_W  = 0.0
        OML_W    = 0.0
        SWK_W    = 0.0
        OPINJ_W  = 0.0
        frac_w   = 0.0
        POLYCOST = poly
        CO2COST  = 0.0
        stim_w   = 0.0
        if (ir.eq.0) ir = aregion(ires)



! RESET PRODUCTION TO LOCAL ARRAY
!

!  determine the indices for the cost equations

        depthcat = 0
!        if (depth.lt.2000.0) THEN                                      !mc change ver3
        if (depth.lt.5000.0) THEN                                       !mc change ver3
          depthcat = 1
!        ELSEIF(depth.ge.2000.0.and.depth.lt.4000.0) then               !mc change ver3
        ELSEIF(depth.ge.5000.0.and.depth.lt.10000.0) then               !mc change ver3
          depthcat = 2
!        ELSEIF(depth.ge.4000.0.and.depth.lt.8000.0) then               !mc change ver3
        ELSEIF(depth.ge.10000.0.and.depth.lt.15000.0) then              !mc change ver3
          depthcat = 3
!        ELSEIF(depth.ge.8000.0) then                                   !mc change ver3
        ELSEIF(depth.ge.15000.0) then                                   !mc change ver3
          depthcat = 4
        end if

        if (iregion.eq.1) then
          IF(state.eq.'IL'.or.state.eq.'IN'.or.state.eq.'KY'.or. &
             state.eq.'MI') then
             regnum = 2
          else
             regnum = 1
          END if
        else
          regnum = iregion+1
        end if

        call maxrate(winj,max_yr-1,rmaxw)
        call maxrate(oinj,max_yr-1,rmaxp)

!  this subroutine calculates the capital and operating costs for oil drilling

!   Step 1: Determine cost multipliers based on current and base oil prices
           IF (OILPRICEC(1).GE.cutoil) THEN
              TERM = (OILPRICEC(1) - baseoil)/baseoil
           ELSE
              TERM          = (cutoil - baseoil)/baseoil
           ENDIF

           INTANG_M = ( 1.0 + omult_int*TERM )**0.5
           TANG_M   = ( 1.0 + omult_tang*TERM )**0.5
           OAM_M    = ( 1.0 + omult_oam*TERM )**0.5

            FPLY = 1. + (0.3913 * TERM)
            FCO2 = (0.5+ 0.013*baseoil*(1.0+TERM))/(0.5 + 0.013*baseoil)
!           PRINT*, aresid(ires),iyr,oilpricec(iyr),oam_m(iyr)

  1     format (a20,<max_yr-1>(3x,f12.2))
  2     format (a20,<max_yr-1>(3x,f12.7))
  3     format (a20,2(3x,f12.2))
  4     format (a20,2(3x,f12.7))
  5     format (a20,3x,f12.2)
  6     format (a20,3x,i2)
  7     format (a20,3x,a2)

!   Step 2: calculate unit costs.
!      A. Drilling and completion.  
!         Use OLOGSS region and project depth to calculate drilling and completion cost for vertical well (K$)
!      B. If reservoir is horizontal, use nlat,latlen in equation

! FOR HORIZONTAL WELLS USE OLD EQN USING NLAT AND LATLEN
        IF (PROCDE.eq.8.or.procde.eq.9) THEN
           DWC_W =OIL_DWCK(9,depthcat)                                    & !horizontal well drilling for
                  + OIL_DWCA(9,depthcat)*DEPTH**2 &                         !process codes 8 & 9
                  + OIL_DWCB(9,depthcat)*(DEPTH**2)*NLAT &
                  + OIL_DWCC(9,depthcat)*(DEPTH**2)*(NLAT*LATLEN)
        ELSE
           if(procde.ne.17) nlat = 0.
           DWC_W =OIL_DWCK(regnum,depthcat)                               & !all other drilling
                  + OIL_DWCA(regnum,depthcat)*DEPTH &
                  + OIL_DWCB(regnum,depthcat)*DEPTH**2 &
                  + OIL_DWCC(regnum,depthcat)*DEPTH**3
           DWC_W = DWC_W/1000.0

           tang_m=OIL_DWCK_F(regnum,depthcat)  &
                  + OIL_DWCA_F(regnum,depthcat)*OILPRICEC(1) &
                  + OIL_DWCB_F(regnum,depthcat)*OILPRICEC(1)**2 &
                  + OIL_DWCC_F(regnum,depthcat)*OILPRICEC(1)**3

! --  (NEW) ----------------------------------------------------------------------------------------------

! After testing, use old variables (DWC_W and DRY_W) Compute Average D&C Cost for Oil Wells in region (regnum)

! Set COEF for each regnum value (accounting for 1a region and northern rockies region (7))

            IF (regnum .gt. 1) COEF = regnum - 1	! standard relationship for region 1a, 2, 3, 4, 5, 6, and 7
            IF (regnum .eq. 1) COEF = 1	! no index shift for region 1
 
! Calculate new D&C cost using new function and coefficients          

	   DNC_COST = DNCC_COEF(1,COEF,1) * exp(-DNCC_COEF(1,COEF,2) * DEPTH) 			!first cost term
	   DNC_COST = DNC_COST + DNCC_COEF(1,COEF,3) * (DEPTH+nlat*Latlen) +  &
                      DNCC_COEF(1,COEF,4)*(DEPTH+nlat*latlen)**2				!second cost term
	   DNC_COST = DNC_COST + DNCC_COEF(1,COEF,5) * exp(DNCC_COEF(1,COEF,6) * DEPTH)		!third cost term
           DNC_COST = DNC_COST/1000                                              		!D&C cost estimate

    if(ogreport(26).eq.1.and.((depth.ge.MINDEPTHoil) .and. (depth.le.MAXDEPTHoil))) then           
           SELECT CASE (regnum)
             CASE (1)
                write(9901,*) 'O ',regnum,':',DEPTH,':', DWC_W,':',DNC_COST,':',aresid(ires)
             CASE (2)
                write(9902,*) 'O ',regnum,':',DEPTH,':', DWC_W,':',DNC_COST,':',aresid(ires)
             CASE (3)
                write(9903,*) 'O ',regnum,':',DEPTH,':', DWC_W,':',DNC_COST,':',aresid(ires)
             CASE (4)
                write(9904,*) 'O ',regnum,':',DEPTH,':', DWC_W,':',DNC_COST,':',aresid(ires)
             CASE (5)
                write(9905,*) 'O ',regnum,':',DEPTH,':', DWC_W,':',DNC_COST,':',aresid(ires)
             CASE (6)
                write(9906,*) 'O ',regnum,':',DEPTH,':', DWC_W,':',DNC_COST,':',aresid(ires)
             CASE (7)
                write(9907,*) 'O ',regnum,':',DEPTH,':', DWC_W,':',DNC_COST,':',aresid(ires)
             CASE DEFAULT
            END SELECT
     end if   
           DWC_W = DNC_COST ! New equation for DWC_W (July 2013)
        END IF
!==========================> HSM Code Start <==========================
    ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, curitr, 'cost_oil',ires , aresid(ires), DNC_COST, DEPTH
!===========================> HSM Code End <===========================
! -------------------------------------------------------------------------------------------------------

 8      format (a20,5(3x,f12.3),3x,f12.3,3x,f12.3,3x,f20.3)

!      C. Dry Hole Well Cost (K$)
        IF (PROCDE.eq.8.OR.procde.eq.9) THEN
           DRY_W =DRY_DWCK(9,depthcat)                                    & !horizontal well drilling for
                  + DRY_DWCA(9,depthcat)*DEPTH**2 &                         !process codes 8 & 9
                  + DRY_DWCB(9,depthcat)*(DEPTH**2)*NLAT &
                  + DRY_DWCC(9,depthcat)*(DEPTH**2)*(NLAT*LATLEN)
        ELSE
           DRY_W =DRY_DWCK(regnum,depthcat)                               & !all other drilling
                  + DRY_DWCA(regnum,depthcat)*DEPTH &
                  + DRY_DWCB(regnum,depthcat)*DEPTH**2 &
                  + DRY_DWCC(regnum,depthcat)*DEPTH**3
           DRY_W = dry_w/1000.0
           dry_m=DRY_DWCK_F(regnum,depthcat)  &
                  + DRY_DWCA_F(regnum,depthcat)*OILPRICEC(1) &
                  + DRY_DWCB_F(regnum,depthcat)*OILPRICEC(1)**2 &
                  + DRY_DWCC_F(regnum,depthcat)*OILPRICEC(1)**3

! -------------- (NEW) ---------------------------------------------------------------------------------
!
!       Equations for estimating the dry well drilling costs using new function 
!	related to fixed, variable drilling, and trouble costs 
!	A*exp(-b*Depth)+ C*Depth + D*Depth*Depth + E*exp(f*Depth) based on fits to JAS 2007 
!	survey data and annual cost multipliers (added by msc - July 2013)
!
	   DNC_COST_DRY = DNCC_COEF(3,COEF,1)*exp(-DNCC_COEF(3,COEF,2)*DEPTH) 
	   DNC_COST_DRY = DNC_COST_DRY + DNCC_COEF(3,COEF,3)*DEPTH + DNCC_COEF(3,COEF,4)*DEPTH*DEPTH
	   DNC_COST_DRY = DNC_COST_DRY + DNCC_COEF(3,COEF,5)*exp(DNCC_COEF(3,COEF,6)*DEPTH)
           DNC_COST_DRY = DNC_COST_DRY/1000

   if(ogreport(26).eq.1.and.((depth.ge.MINDEPTHoil) .and. (depth.le.MAXDEPTHoil))) then
        SELECT CASE (regnum)
             CASE (1)
                write(9911,*) 'DO ',regnum,':',DEPTH,':', DRY_W,':',DNC_COST_DRY,':',aresid(ires)
             CASE (2)
                write(9912,*) 'DO ',regnum,':',DEPTH,':', DRY_W,':',DNC_COST_DRY,':',aresid(ires)
             CASE (3)
                write(9913,*) 'DO ',regnum,':',DEPTH,':', DRY_W,':',DNC_COST_DRY,':',aresid(ires)
             CASE (4)
                write(9914,*) 'DO ',regnum,':',DEPTH,':', DRY_W,':',DNC_COST_DRY,':',aresid(ires)
             CASE (5)
                write(9915,*) 'DO ',regnum,':',DEPTH,':', DRY_W,':',DNC_COST_DRY,':',aresid(ires)
             CASE (6)
                write(9916,*) 'DO ',regnum,':',DEPTH,':', DRY_W,':',DNC_COST_DRY,':',aresid(ires)
             CASE (7)
                write(9917,*) 'DO ',regnum,':',DEPTH,':', DRY_W,':',DNC_COST_DRY,':',aresid(ires)
             CASE DEFAULT
            END SELECT   
   end if
           DRY_W = DNC_COST_DRY ! New equation for DWC_W (July 2013)
        END IF
!==========================> HSM Code Start <==========================
    ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, curitr, 'cost_oil_dry',ires , aresid(ires), DNC_COST_DRY, DEPTH
!===========================> HSM Code End <===========================
!
! -----------------------------------------------------------------------------------------------------



!      D. Cost to Equip new Producer (K$)
        NPR_W =NPRK(regnum,depthcat) &
              + NPRA(regnum,depthcat)*DEPTH &
              + NPRB(regnum,depthcat)*DEPTH**2 &
              + NPRC(regnum,depthcat)*DEPTH**3
        NPR_W = NPR_W/1000.0
        NPR_m =NPRK_F(regnum,depthcat) &
              + NPRA_F(regnum,depthcat)*OILPRICEC(1) &
              + NPRB_F(regnum,depthcat)*OILPRICEC(1)**2 &
              + NPRC_F(regnum,depthcat)*OILPRICEC(1)**3

!      E. Workover Costs (K$)
        wrk_W =wrkK(regnum,depthcat) &
              + wrkA(regnum,depthcat)*DEPTH &
              + wrkB(regnum,depthcat)*DEPTH**2 &
              + wrkC(regnum,depthcat)*DEPTH**3
        wrk_W = wrk_W/1000.0
        wrk_m =wrkK_F(regnum,depthcat) &
              + wrkA_F(regnum,depthcat)*OILPRICEC(1) &
              + wrkB_F(regnum,depthcat)*OILPRICEC(1)**2 &
              + wrkC_F(regnum,depthcat)*OILPRICEC(1)**3

!      F. cost to convert primary to secondary well (K$)
        PSW_W =PSWK(regnum,depthcat) &
               + PSWA(regnum,depthcat)*DEPTH &
               + PSWB(regnum,depthcat)*DEPTH**2 &
               + PSWC(regnum,depthcat)*DEPTH**3
        PSW_W = PSW_W/1000.0
        PSW_m =PSWK_F(regnum,depthcat) &
               + PSWA_F(regnum,depthcat)*OILPRICEC(1) &
               + PSWB_F(regnum,depthcat)*OILPRICEC(1)**2 &
               + PSWC_F(regnum,depthcat)*OILPRICEC(1)**3

!      G. Cost to convert producer to injector (K$)
        PSI_W =PSIK(regnum,depthcat) &
               + PSIA(regnum,depthcat)*DEPTH &
               + PSIB(regnum,depthcat)*DEPTH**2 &
               + PSIC(regnum,depthcat)*DEPTH**3
        PSI_W = PSI_W/1000.0
        PSI_m =PSIK_F(regnum,depthcat) &
               + PSIA_F(regnum,depthcat)*OILPRICEC(1) &
               + PSIB_F(regnum,depthcat)*OILPRICEC(1)**2 &
               + PSIC_F(regnum,depthcat)*OILPRICEC(1)**3

!      H. Facilites Upgrade Cost (K$)
        fac_W =facupK(regnum,depthcat) &
               + facupA(regnum,depthcat)*DEPTH &
               + facupB(regnum,depthcat)*DEPTH**2 &
               + facupC(regnum,depthcat)*DEPTH**3
        fac_W = fac_W/1000.0
        fac_m =facupK_F(regnum,depthcat) &
               + facupA_F(regnum,depthcat)*OILPRICEC(1) &
               + facupB_F(regnum,depthcat)*OILPRICEC(1)**2 &
               + facupC_F(regnum,depthcat)*OILPRICEC(1)**3

!      H. Cost of produced water handling plant (K$)
         pwp_f = pwhp*(rmaxw/365.0)
         pwp_f = pwp_f

!      I. Cost of chemical handling plant (non-polymer)
         chm_f = chmk*chma*((rmaxp/365.0)**chmb)
         chm_f = chm_f

!      J. Cost of polymer handling plant
         ply_f = plypk*plypa*((rmaxp/365)**0.6)
         ply_f = ply_f

 9     format (a20,5(3x,f12.3))

!      K. Cost of CO2 recycling plant
!        co2_f = (co2rk*(RMAXp/365.0)**co2rb)
!        co2_f = co2_f*1000.0

!      L. Cost of Steam Manifolds/Pipelines
         stmm_f = totpat*patsze*stmma

!CCC THIS NEEDS TO BE ON AN ANNUAL BASIS! CCCC
!      M. Cost of Steam Generators

!   Step 3: Calculate annual operating costs
!      A. Calculate fixed operating costs
        omo_W =omoK(regnum,depthcat) &
                 + omoA(regnum,depthcat)*DEPTH &
                 + omoB(regnum,depthcat)*DEPTH**2 &
                 + omoC(regnum,depthcat)*DEPTH**3
        omo_W = omo_w/1000.0
        oam_m =omoK_F(regnum,depthcat) &
                 + omoA_F(regnum,depthcat)*OILPRICEC(1) &
                 + omoB_F(regnum,depthcat)*OILPRICEC(1)**2 &
                 + omoC_F(regnum,depthcat)*OILPRICEC(1)**3

!      B. Calculate annual costs for secondary producers
        OPSEC_W =OPSECK(regnum,depthcat) &
                 + OPSECA(regnum,depthcat)*DEPTH &
                 + OPSECB(regnum,depthcat)*DEPTH**2 &
                 + OPSECC(regnum,depthcat)*DEPTH**3
        OPSEC_W = opsec_w/1000.0
        OPSEC_m =OPSECK(regnum,depthcat) &
                 + OPSECA(regnum,depthcat)*OILPRICEC(1) &
                 + OPSECB(regnum,depthcat)*OILPRICEC(1)**2 &
                 + OPSECC(regnum,depthcat)*OILPRICEC(1)**3

!      C. Calculate lifting costs
        oml_W =omlK(regnum,depthcat) &
                 + omlA(regnum,depthcat)*DEPTH &
                 + omlB(regnum,depthcat)*DEPTH**2 &
                 + omlC(regnum,depthcat)*DEPTH**3
        oml_W = oml_w/1000.0
        oml_m =omlK_F(regnum,depthcat) &
                 + omlA_F(regnum,depthcat)*OILPRICEC(1) &
                 + omlB_F(regnum,depthcat)*OILPRICEC(1)**2 &
                 + omlC_F(regnum,depthcat)*OILPRICEC(1)**3

!      C. Calculate secondary workover
        swk_w =omswrK(regnum,depthcat) &
                 + omswrA(regnum,depthcat)*DEPTH &
                 + omswrB(regnum,depthcat)*DEPTH**2 &
                 + omswrC(regnum,depthcat)*DEPTH**3
        swk_w = swk_w/1000.0
        swk_m =omswrK_F(regnum,depthcat) &
                 + omswrA_F(regnum,depthcat)*OILPRICEC(1) &
                 + omswrB_F(regnum,depthcat)*OILPRICEC(1)**2 &
                 + omswrC_F(regnum,depthcat)*OILPRICEC(1)**3

!      D. Calculate injection costs
        opinj_W =opinjK(regnum,depthcat) &
                 + opinjA(regnum,depthcat)*DEPTH &
                 + opinjB(regnum,depthcat)*DEPTH**2 &
                 + opinjC(regnum,depthcat)*DEPTH**3
        opinj_W = opinj_w/1000.0

!      E. Calculate injectant costs
        polycost = polycost * fply                     !polymer - adjust for oil price

!      F. Hydraulic fracturing costs
        if(depth.ge.min_fd(1).and.depth.le.max_fd(1)) then
          frac_w = (k_fd(1)*depth**3)+(a_fd(1)*depth**2)+(b_fd(1)*depth)+c_fd(1)
        elseif(depth.ge.min_fd(2).and.depth.le.max_fd(2)) then
          frac_w = (k_fd(2)*depth**3)+(a_fd(2)*depth**2)+(b_fd(2)*depth)+c_fd(2)
        end if
        aflen(ires) = aflen(ires)*chg_fraclen_fac(itech)

!       frac_w = frac_w * aflen(ires) * 2.0
        frac_w = frac_w * aflen(ires)

!      Calculate CO2 cost
         if (co2code.eq.1) then
           CO2COST = reg_COSTCO2(iregion,ir,4,itimeyr)
           eco2code(ires) = 4
         elseif (co2code.eq.2) then    !prescreening only
           CO2COST = 0.0
           eco2code(ires) = 4
         else
           CO2COST = reg_COSTCO2(iregion,ir,ctype,itimeyr)
           eco2code(ires) = ctype
         end if

         eco2reg(ires) = ir

!      Stimulation
           STIM_W = (STIM_A + STIM_B * DEPTH)/1000.0

 12      format (a20,3x,f12.2)
 13      format (a20,3x,i12)

!  Apply the technology levers to the costs.  Two types of levers are applied:
!     1) The change in cost from new technology       (unit: fraction of cost)
!     2) The cost to apply the technology             (unit: K$              )

         if (procde.eq.10.or.procde.eq.16) drctech = togtechon(1,1)*techadj_on(1,1)
         if (procde.eq.17) drctech = togtechon(1,2)*techadj_on(1,1)
         if (procde.ge.3.and.procde.le.9) drctech = togtechon(1,3)*techadj_on(1,1)
         if (procde.eq.18.or.procde.eq.22) drctech = togtechon(1,4)*techadj_on(1,1)
         if (procde.eq.20.or.procde.eq.21) drctech = togtechon(1,5)*techadj_on(1,1)
         if (procde.eq.19.or.procde.eq.23) drctech = togtechon(1,6)*techadj_on(1,1)
         drcadj = 1.0
         if (curiyr.ge.techyr) drcadj = (1.0+drctech)**(curiyr-techyr+1)
         DWC_W = DWC_W*drcadj
!==========================> HSM Code Start <==========================
    ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, curitr, 'cost_oil_drcadj',ires , aresid(ires), DWC_W, drcadj
!===========================> HSM Code End <===========================
         DRY_W = DRY_W*drcadj

         if (procde.eq.10.or.procde.eq.16) leotech = togtechon(2,1)*techadj_on(1,1)
         if (procde.eq.17) leotech = togtechon(2,2)*techadj_on(1,1)
         if (procde.ge.3.and.procde.le.9) leotech = togtechon(2,3)*techadj_on(1,1)
         if (procde.eq.18.or.procde.eq.22) leotech = togtechon(2,4)*techadj_on(1,1)
         if (procde.eq.20.or.procde.eq.21) leotech = togtechon(2,5)*techadj_on(1,1)
         if (procde.eq.19.or.procde.eq.23) leotech = togtechon(2,6)*techadj_on(1,1)
         leoadj = 1.0
         if (curiyr.ge.techyr) leoadj = (1.0+leotech)**(curiyr-techyr+1)
         OMO_W = OMO_W*leoadj
         

         DWC_W = (DWC_W + cst_drl_fac(itech)) * chg_drl_fac(itech)
         DWC_W = (DWC_W + cst_comp_fac(itech)) * chg_comp_fac(itech) 
!==========================> HSM Code Start <==========================
    ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, curitr, 'cost_oil_drl_comp',ires , aresid(ires), DWC_W, cst_drl_fac(itech), chg_drl_fac(itech), cst_comp_fac(itech), chg_comp_fac(itech)
!===========================> HSM Code End <===========================
!        if (aplay_cde(ires).ge.3110.and.aplay_cde(ires).le.3115.and.itech.eq.1)  &
!        if ((procde.eq.17.or.procde.eq.21.or.procde.eq.22).and.itech.eq.1)  &
!        if (procde.eq.3)  &
!          write(6,*) 'dh5csto', curiyr+1989, aresid(ires), aplay_cde(ires), dwc_w

         dry_W = (dry_W + cst_drl_fac(itech)) * chg_drl_fac(itech)
         dry_W = (dry_W + cst_comp_fac(itech)) * chg_comp_fac(itech) 

         frac_w = frac_w * chg_frac_fac(itech) + cst_frac_fac(itech) + cst_fraclen_fac(itech)

         NPR_W = NPR_W * chg_fac_fac(itech) + cst_fac_fac(itech)
         wrk_W = wrk_W * chg_wrk_fac(itech) + cst_wrk_fac(itech)
         PSW_W = PSW_W * chg_secconv_fac(itech) + cst_secconv_fac(itech)
         PSI_W = PSI_W * chg_injconv_fac(itech) + cst_injconv_fac(itech)
         fac_W = fac_W * chg_facupg_fac(itech) + cst_facupg_fac(itech)
         pwp_f = pwp_f * chg_prdwat_fac(itech)
         chm_f = chm_f * chg_chmpnt_fac(itech)
         ply_f = ply_f * chg_plypnt_fac(itech)
         co2_f = co2_f * chg_co2pnt_fac(itech)
         omo_w = omo_w * chg_foam_fac(itech) + cst_foam_fac(itech)
         opsec_w = opsec_w * chg_annsec_fac(itech)+cst_annsec_fac(itech)
         oml_w = oml_w * chg_lft_fac(itech) + cst_lft_fac(itech)
         swk_w = swk_w * chg_secwrk_fac(itech) + cst_secwrk_fac(itech)
         stmm_f = stmm_f * chg_stmgen_fac(itech)

         STIM_W = STIM_W * chg_stm_fac(itech) + cst_stm_fac(itech)

         polycost = polycost * chg_injt_fac(itech)
         co2cost = co2cost * chg_injt_fac(itech)

!  apply the CPI indices to the costs

         dwc_w = dwc_w * cpi_2005 
!==========================> HSM Code Start <==========================
    ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, curitr, 'cost_oil_cpi',ires , aresid(ires), DWC_W, cpi_2005
!===========================> HSM Code End <===========================
         dry_w = dry_w * cpi_2005

         npr_w = npr_w * cpi_2005
         wrk_w = wrk_w * cpi_2005
         psw_w = psw_w * cpi_2005
         psi_w = psi_w * cpi_2005
         fac_w = fac_w * cpi_2005
         pwp_f = pwp_f * cpi_2003
         chm_f = chm_f * cpi_2003
         ply_f = ply_f * cpi_2003
         co2_f = co2_f * cpi_2003
         omo_w = omo_w * cpi_2005
         opsec_w = opsec_w * cpi_2005
         oml_w = oml_w * cpi_2005
         swk_w = swk_w * cpi_2005
         opinj_w = opinj_w * cpi_2005
         stmm_f = stmm_f * cpi_2003
         stim_w = stim_w * cpi_2003

         oil_oam1 = oil_oam(procde+1) * cpi_2005
         gas_oam1 = gas_oam(procde+1) * cpi_2005
         wat_oam1 = wat_oam(procde+1) * cpi_2005
         inj_oam1 = inj_oam(procde+1) * cpi_2005

         oil_oam1 = oil_oam1 * chg_voam_fac(itech)
         gas_oam1 = gas_oam1 * chg_voam_fac(itech)
         wat_oam1 = wat_oam1 * chg_voam_fac(itech)
         inj_oam1 = inj_oam1 * chg_injc_fac(itech)

         oil_oam1 = oil_oam1 + chg_ooil_fac(itech)   !mc change ver 11
         gas_oam1 = gas_oam1 + chg_ogas_fac(itech)   !mc change ver 11
         wat_oam1 = wat_oam1 + chg_owat_fac(itech)   !mc change ver 11
         inj_oam1 = inj_oam1 + chg_oinj_fac(itech)   !mc change ver 11

          if (opt_dbg2) then
           write (OGBUG1,*) 'RESERVOIR ID                       :',RESID
           WRITE (OGBUG1,*) 'TECHNOLOGY CASE                    :',ITECH
           WRITE (OGBUG1,*) 'DEPTH                              :',DEPTH
           WRITE (OGBUG1,*) 'NUMBER OF LATERALS                 :',NLAT
           WRITE (OGBUG1,*) 'LENGTH OF LATERALS                 :',LATLEN
           WRITE (OGBUG1,*) 'OLOGSS REGION:                     :',IREGION
           WRITE (OGBUG1,*) 'TIMING YEAR:                       :',ITIMEYR
           WRITE (OGBUG1,*) 'REGION KNOWN DEV. DRY HOLE RATE    :',                     & !4.16.09
            REGDRYKD(IREGION)
           WRITE (OGBUG1,*) 'REGION UNDISC EXP. DRY HOLE RATE   :',                     & !4.16.09
            REGDRYUE(IREGION)
           WRITE (OGBUG1,*) 'REGION UNDISC DEV. DRY HOLE RATE   :',                     & !4.16.09
            REGDRYUD(IREGION)

           WRITE (OGBUG1,*) 'CPI 2005,CPI 2003                  :',CPI_2005 &
            ,CPI_2003
           WRITE (OGBUG1,*) 'DRILLING & COMPL.                  :',DWC_W
           WRITE (OGBUG1,*) 'DRY HOLE                           :',DRY_W

           WRITE (OGBUG1,*) 'EQUIP NEW PRODUCER                 :',NPR_W
           WRITE (OGBUG1,*) 'WORKOVER                           :',WRK_W
           WRITE (OGBUG1,*) 'PRIMARY TO SECONDARY               :',PSW_W
           WRITE (OGBUG1,*) 'PRIMARY TO INJECTOR                :',PSI_W
           WRITE (OGBUG1,*) 'FACILITIES UPGRADE                 :',FAC_W
           WRITE (OGBUG1,*) 'PROD. WAT HANDLING PLANT           :',PWP_F
           WRITE (OGBUG1,*) 'CHEMICAL HANDLING PLANT            :',CHM_F
           WRITE (OGBUG1,*) 'POLYMER HANDLING PLANT             :',PLY_F
           WRITE (OGBUG1,*) 'CO2 RECYCLING PLANT                :',CO2_F
           WRITE (OGBUG1,*) 'FIXED ANNUAL OPERATING COST        :',OMO_W
           WRITE (OGBUG1,*) 'FIXED ANNUAL O&M FOR SEC. PRODUCERS:',OPSEC_W
           WRITE (OGBUG1,*) 'LIFTING COSTS                      :',OML_W
           WRITE (OGBUG1,*) 'SECONDARY WORKOVER                 :',SWK_W
           WRITE (OGBUG1,*) 'INJECTION                          :',OPINJ_W
           WRITE (OGBUG1,*) 'INJECTANT                          :',POLYCOST
           WRITE (OGBUG1,*) 'CO2                                :',CO2COST
           WRITE (OGBUG1,*) 'STIMULATION COST                   :',STIM_W
           WRITE (OGBUG1,*) 'FRACTURE COST                      :',FRAC_W
           WRITE (OGBUG1,*) 'O&M FOR OIL PRODUCTION             :',OIL_OAM1
           WRITE (OGBUG1,*) 'O&M FOR GAS PRODUCTION             :',GAS_OAM1
           WRITE (OGBUG1,*) 'O&M FOR WATER PRODUCTION           :',WAT_OAM1
           WRITE (OGBUG1,*) 'O&M FOR INJECTION                  :',INJ_OAM1
           WRITE (OGBUG1,*)
           WRITE (OGBUG1,*)
          end if

!   Step 4: Determine well development profile
         ishift = 0
       IF(apcode(ires).eq.10.or.apcode(ires).eq.16) ishift = disclag
         call patt_development(ires,1+ishift,301)

!           determine the economic life of one pattern
         econ_life = max_yr-1
         call patcost_oil(itech)
         call patcash(patlife,301)
         econ_life = patlife
       call initpre

       do i = 1,max_yr-1
          patn1(i) = PATDEV(IReS,I,ITeCH)
!          PRINT*, PATN1(I),'A'
       end do
!       PAUSE

       totpatn = 0.0                                              !mc change ver 11
       do i = 1,max_yr-1                                          !mc change ver 11
         do J = 1,2                                               !mc change ver 11
            totpatn = totpatn + patdev(ires,i,J)                  !mc change ver 11
         end do                                                   !mc change ver 11
       end do                                                     !mc change ver 11



!       IF (STARTPR.GT.1) THEN
! SHIFT THE PRICE, AND DEVELOPMENT SCHEDULE, WRITE ZEROES AT THE END
! OF THE PATTERN DEVELOPMENT SCHEDULE

           DO I=1,MAX_YR-STARTPR
             PATTERNS(I) = PATN1(I+STARTPR-1)
!             PRINT*, I,PATTERNS(I),'B'
           END DO
!           PAUSE

       IF(procde.eq.0) then                                                       !mc change 5.5.09
         do i=1,max_yr                                                            !mc change 5.5.09
           IF(i.eq.1) then                                                        !mc change 5.5.09
             patterns(i) = 0.0!totpat                                             !mc change 5.5.09
           else                                                                   !mc change 5.5.09
             patterns(i) = 0.0                                                    !mc change 5.5.09
           END if                                                                 !mc change 5.5.09
         end do                                                                   !mc change 5.5.09
       END if                                                                     !mc change 5.5.09
!         do i = 1,startpr-1
!           patterns(i) = 0.0
!         end do
!         do i = startpr,max_yr
!           patterns(i) = patn1(i-startpr+1)
!         end do
!       ELSE
!       ENDIF !END OF PROJECT SHIFT IF STATEMENT
!
! MC CHANGE 4.27.09 START - INCORPORATION OF THE RE-DRILLING
       IF(PROCDE.EQ.0)THEN      ! DECLINE CURVE MODELS & Unconventional Exploration - already laid out       !removed 17 - 9/2/08
       ishift = 0
            DO IYR=1,max_yr-1
!               PATTERNS(IYR) = 0.0
               IF(IYR.EQ.1+ishift.AND.ITECH.EQ.1) PATTERNS(IYR) = TOTPAT
!               PATTERNS(IYR) = PATTERNS(IYR) + PATN1(IYR)                           !MC CHANGE 4.27.09
!               patdev(ires,iyr,1) = 0.0                                            !MC CHANGE 4.27.09
!               PRINT*, IYR,PATTERNS(IYR),PATN1(IYR),'C'
            END DO
!            PAUSE

!            PATTERNS(1+ishift) = TOTPAT                                            !MC CHANGE 4.27.09
!            patdev(ires,1+ishift,1) = patterns(1+ishift)                           !MC CHANGE 4.27.09
!            patdev(ires,1,2+ishift) = 0.0                                          !MC CHANGE 4.27.09
!            IF (ITECH.EQ.2) THEN                                                   !MC CHANGE 4.27.09
!               DO IYR=1,max_yr-1                                                   !MC CHANGE 4.27.09
!                  PATTERNS(IYR) = 0.0                                              !MC CHANGE 4.27.09
!                  patdev(ires,iyr,2) = 0.0                                         !MC CHANGE 4.27.09
!               END DO                                                              !MC CHANGE 4.27.09
!            END IF                                                                 !MC CHANGE 4.27.09
       ENDIF
!   Step 5: Determine annual production profile
       ilife = patlife
        DO I=1,nyr
           PATN(I)=PATTERNS(I)
        END DO

!        DO I=1,NYR
!          PRINT*, ARESID(IRES,1),I,PATN(I),PATTERNS(I),PATN1(I),ITECH
!        END DO
!        PAUSE

!
! SETTING ACTIVE PATTERNS BY YEAR
!
       XPATN(1) = PATN(1)
       DO IK =2,nyr
          XPATN(IK) = XPATN(IK-1) + PATN(IK)
!
          IF ( IK.GT.(ILIFE+1)) THEN
             XPATN(IK) = XPATN(IK) - PATN(IK-ILIFE-1)
          ENDIF
!
          IF ( IK.GT.(ILIFE*2+1)) THEN
             XPATN(IK) = XPATN(IK) - PATN(IK-ILIFE-1)
          ENDIF
!
          IF (XPATN(IK).LE.0.0) XPATN(IK) = 0.0
       ENDDO

        LYR = 0
        IYR = 0
        KYR = 0
        JYR = 0
        DO IX = nyr,1,-1
           IF (PATN(IX).NE.0.0)THEN
              JYR = IX
              GO TO 15
           ENDIF
        ENDDO
15      CONTINUE
!
! CALCULATE THE PRODUCTION PROFILE FOR THE PROJECT
!

! initialize the production arrays
       do IYR = 1,max_yr
         oilprod(iyr) = 0.0
         gasprod(iyr) = 0.0
         nglprod(iyr) = 0.0
         watprod(iyr) = 0.0
         totinj(iyr) = 0.0
         watinj(iyr) = 0.0
         torecy(iyr) = 0.0
         sump(iyr) = 0.0
       end do
       wlspacing = alatlen(ires,1)*870./43560.

        IF (procde.ne.0) THEN
           do iyr = 1,max_yr
             aresvoil(ires,iyr,itech) = 0.0
             aresvgas(ires,iyr,itech) = 0.0
           end do
!      
           prodadj2 = 1.0
           DO IYR = 1,JYR
              KYR = 0
              LYR = IYR + nyr - 1
              IF(LYR.GE.max_yr)LYR=max_yr
              if (procde.eq.10.or.procde.eq.16) prodtech = togtechon(3,1)*techadj_on(1,1)
              if (procde.eq.17) prodtech = togtechon(3,2)*techadj_on(1,1)
              if (procde.ge.3.and.procde.le.9) prodtech = togtechon(3,3)*techadj_on(1,1)
              if (procde.eq.18.or.procde.eq.22) prodtech = togtechon(3,4)*techadj_on(1,1)
              if (procde.eq.20.or.procde.eq.21) prodtech = togtechon(3,5)*techadj_on(1,1)
              if (procde.eq.19.or.procde.eq.23) prodtech = togtechon(3,6)*techadj_on(1,1)
              prodadj = 1.0
              if (curiyr.lt.techyr.and.curiyr+iyr-1.ge.techyr) prodadj = (1.0+prodtech)**(curiyr+iyr-techyr)
              if (curiyr.ge.techyr) prodadj = (1.0+prodtech)**(iyr-1)
              if (procde.eq.17.and.anwellinj(ires).le.0.and.(curiyr+iyr.gt.techyr+1.and.iyr.le.5)) prodadj2 = (1.0+2.*prodtech)**(iyr)
! DIMINISHING_RETURNS
              DO IYR1 = IYR,LYR
                 KYR = KYR+1
                 wlsdrilled = atotacres(ires)/apatsiz(ires)-atotpat(ires,1)+sum(patn(1:IYR))
                 if(wlsdrilled.gt.0) then
                   spacing = atotacres(ires)/wlsdrilled
                   if(spacing.gt.0..and.spacing.lt.wlspacing.and.procde.eq.17) then
                      dfactor = (1.0-1./(1.+exp(-1.*((wlspacing/spacing-1)-1.))))
                      oprod(kyr) = oprodo(kyr)*dfactor
                      gprod(kyr) = gprodo(kyr)*dfactor
                      nprod(kyr) = nprodo(kyr)*dfactor
                      prodadj = 1.
                      prodadj2 = 1.
                      if(kyr.eq.1) write(ogbug1,*) 'dim returns', aresid(ires), aplay_cde(ires), curiyr+iyr+1989, dfactor
                   endif
                 endif

                 IF(procde.eq.6) then
                OILPROD(IYR1) = OILPROD(IYR1) + OPROD(KYR)*PATN(IYR)*2.0*prodadj    ! OIL PRODUCTION
                GASPROD(IYR1) = GASPROD(IYR1) + GPROD(KYR)*PATN(IYR)*2.0*prodadj    ! GAS PRODUCTION
                 else
                OILPROD(IYR1) = OILPROD(IYR1) + OPROD(KYR)*PATN(IYR)*prodadj*prodadj2    ! OIL PRODUCTION
                GASPROD(IYR1) = GASPROD(IYR1) + GPROD(KYR)*PATN(IYR)*prodadj*prodadj2    ! GAS PRODUCTION
                 END if
                 NGLPROD(IYR1) = NGLPROD(IYR1) + NPROD(KYR)*PATN(IYR)*prodadj*prodadj2    ! NGL PRODUCTION
                 WATPROD(IYR1) = WATPROD(IYR1) + WPROD(KYR)*PATN(IYR)            ! WATER PRODUCTION
                 TOTINJ(IYR1)  = TOTINJ(IYR1)  + OINJ(KYR)*PATN(IYR)             ! INJECTANT
                 WATINJ(IYR1)  = WATINJ(IYR1)  + WINJ(KYR)*PATN(IYR)             ! WATER INJECTED
                 TORECY(IYR1)  = TORECY(IYR1)  + ORECY(KYR)*PATN(IYR)            ! RECYCLED VOLUME
                 SUMP(IYR1)    = SUMP(IYR1)    + PATN(IYR)
              ENDDO


                 do myr = 1,40!25!max_yr                                                              !mc change ver 7
                 aresvoil(ires,iyr,itech) = aresvoil(ires,iyr,itech) + &
                   oprod(myr)*patn(iyr)
                 aresvgas(ires,iyr,itech) = aresvgas(ires,iyr,itech) + &
                   gprod(myr)*patn(iyr)
                end do
           ENDDO
           do iyr = 1,40!24!max_yr-1                                                                  !mc change ver 7
              IF(iyr.eq.1) then
                 temporesv(iyr) = aresvoil(ires,iyr,itech)
                 tempgresv(iyr) = aresvgas(ires,iyr,itech)
              else
                 temporesv(iyr) = aresvoil(ires,iyr,itech) &
                  +temporesv(iyr-1)
                 tempgresv(iyr) = aresvgas(ires,iyr,itech) &
                  +tempgresv(iyr-1)
              END if
              tcumresv = temporesv(iyr)              !total booked oil reserves for life of project   !mc change ver 7
              tcumresv2= tempgresv(iyr)              !total booked gas reserves for life of project   !mc change ver 7
           end do

           do iyr = 1,40                                                                              !mc change ver 7
             tempiresv(iyr) = tcumresv - temporesv(iyr)  !calculate the inferred oil reserves         !mc change ver 7
             tempiresvg(iyr) = tcumresv2 - tempgresv(iyr)  !calculate the inferred gas reserves       !mc change ver 7
!             print*, iyr,tcumresv,tempiresv(iyr), '1st step'
           end do                                                                                     !mc change ver 7
!           pause

           do iyr = 1,40!24!max_yr-1                                                                  !mc change ver 7
            do myr = 1,iyr
              temporesv(iyr) = temporesv(iyr) - oilprod(myr)
              tempgresv(iyr) = tempgresv(iyr) - gasprod(myr)
            end do
           end do

           do iyr = 1,40!24!max_yr-1                                                                  !mc change ver 7
            aresvoil(ires,iyr,itech) = temporesv(iyr)
            airsvoil(ires,iyr,itech) = tempiresv(iyr)                                                 !mc change ver 7
            aresvgas(ires,iyr,itech) = tempgresv(iyr)
            airsvgas(ires,iyr,itech) = tempiresvg(iyr)                                                !mc change ver 7
            IF(aresvoil(ires,iyr,itech).le.0.0) &
               aresvoil(ires,iyr,itech)=0.0
            IF(aresvgas(ires,iyr,itech).le.0.0) &
               aresvgas(ires,iyr,itech)=0.0
            if(airsvoil(ires,iyr,itech).le.0.0) airsvoil(ires,iyr,itech)=0.0                          !mc change ver 7
            if(airsvgas(ires,iyr,itech).le.0.0) airsvgas(ires,iyr,itech)=0.0                          !mc change ver 7
           end do

        ELSEIF (procde.eq.0) THEN
         if (itech.eq.1) then
           DO IYR = 1,nyr-ishift
!              KYR = 0
!              LYR = IYR + nyr - 1
!              IF(LYR.GE.40)LYR=40
!              DO IYR1 = IYR,LYR
!                 KYR = KYR+1
!               IF(procde.eq.0) then                                       ! MC CHANGE 6.23.09
!                do myr = iyr+1,nyr-ishift
!                 aresvoil(ires,iyr,itech) = aresvoil(ires,iyr,itech) + &
!                   oprod(myr)
!                 aresvgas(ires,iyr,itech) = aresvgas(ires,iyr,itech) + &
!                   gprod(myr)
!                end do
!               END if

                 OILPROD(IYR+ishift) = OPROD(IYR)    ! OIL PRODUCTION
                 GASPROD(IYR+ishift) = GPROD(IYR)    ! GAS PRODUCTION
                 NGLPROD(IYR+ishift) = NPROD(IYR)    ! GAS PRODUCTION
                 WATPROD(IYR+ishift) = WPROD(IYR)    ! WATER PRODUCTION
                 TOTINJ(IYR+ishift)  = OINJ(IYR)     ! INJECTANT
                 WATINJ(IYR+ishift)  = WINJ(IYR)     ! WATER INJECTED
                 TORECY(IYR+ishift)  = ORECY(IYR)    ! RECYCLED VOLUME
! SPECIAL CASE FOR UNCONVENTIONAL
                 IF (IYR.LE.1) THEN
                    SUMP(IYR+ishift) = PATN(IYR)
                 ELSE
                    SUMP(IYR) = SUMP(IYR-1) + PATN(IYR)
                 END IF
           ENDDO
         end if
        END IF

!
! CALCULATE TOTAL PRODUCTION AND CUMULATIVE PRODUCTION
!
        TOTPROD = 0.0
        DO IB=1,nyr
           TOTPROD = TOTPROD +  OILPROD(IB) + GASPROD(IB)/5.6
           IF(IB.GT.1)THEN
             XCUMPROD(IB)=XCUMPROD(IB-1) + OILPROD(IB) + GASPROD(IB)/5.6
           ELSE
             XCUMPROD(IB) = OILPROD(IB) + GASPROD(IB)/5.6
           ENDIF
        ENDDO
!
! FIND THE REMAINING RESERVES
!
        DO IH=1,nyr
           REMRES(IH) = aresvoil(ires,ih,itech)
           gremres(ih) = aresvgas(ires,ih,itech)
           iremres(ih) = airsvoil(ires,ih,itech)                                      !mc change ver 7
           igremres(ih) = airsvgas(ires,ih,itech)                                     !mc change ver 7
        ENDDO

! convert project level costs to apply to well level costs to apply and integrate with the unit costs !mc change ver 11
        if (.not.once) then
          cstp_drl_fac0(itech)     =    cstp_drl_fac(itech)
          cstp_stm_fac0(itech)     =    cstp_stm_fac(itech)
          cstp_comp_fac0(itech)    =    cstp_comp_fac(itech)
          cstp_fac_fac0(itech)     =    cstp_fac_fac(itech)
          cstp_secconv_fac0(itech) =    cstp_secconv_fac(itech)
          cstp_injconv_fac0(itech) =    cstp_injconv_fac(itech)
          cstp_facupg_fac0(itech)  =    cstp_facupg_fac(itech)
          cstp_wrk_fac0(itech)     =    cstp_wrk_fac(itech)
          cstp_foam_fac0(itech)    =    cstp_foam_fac(itech)
          cstp_voam_fac0(itech)    =    cstp_voam_fac(itech)
          cstp_gna_fac0(itech)     =    cstp_gna_fac(itech)
          cstp_annsec_fac0(itech)  =    cstp_annsec_fac(itech)
          cstp_lft_fac0(itech)     =    cstp_lft_fac(itech)
          cstp_secwrk_fac0(itech)  =    cstp_secwrk_fac(itech)
          cstp_injc_fac0(itech)    =    cstp_injc_fac(itech)
          cstp_cmp_fac0(itech)     =    cstp_cmp_fac(itech)
          once = .true.
        endif

        if(totpatn.gt.0.0) cstp_drl_fac(itech)     =    cstp_drl_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_stm_fac(itech)     =    cstp_stm_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_comp_fac(itech)    =    cstp_comp_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_fac_fac(itech)     =    cstp_fac_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_secconv_fac(itech) =    cstp_secconv_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_injconv_fac(itech) =    cstp_injconv_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_facupg_fac(itech)  =    cstp_facupg_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_wrk_fac(itech)     =    cstp_wrk_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_foam_fac(itech)    =    cstp_foam_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_voam_fac(itech)    =    cstp_voam_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_gna_fac(itech)     =    cstp_gna_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_annsec_fac(itech)  =    cstp_annsec_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_lft_fac(itech)     =    cstp_lft_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_secwrk_fac(itech)  =    cstp_secwrk_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_injc_fac(itech)    =    cstp_injc_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_cmp_fac(itech)     =    cstp_cmp_fac0(itech)/totpatn
   !mc change ver 11

        res_chr_fac(itech) = res_chr_chg(itech)*totprod/1000.      !mc change ver 11
                                                                   !reservoir characterization cost
                                                                   !this cost will be applied in the cashflow.

        DWC_W   = DWC_W   + cstp_drl_fac(itech)                    !mc change ver 11
        DWC_W   = DWC_W   + cstp_comp_fac(itech)                   !mc change ver 11
!==========================> HSM Code Start <==========================
    ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, curitr, 'cost_oil_drl_comp_p',ires , aresid(ires), DWC_W, cstp_drl_fac(itech), cstp_comp_fac(itech)
!===========================> HSM Code End <===========================
!
!        dry_W   = dry_W   + cstp_drl_fac(itech)                    !mc change ver11
!        dry_W   = dry_W   + cstp_comp_fac(itech)                   !mc change ver11
        NPR_W   = NPR_W   + cstp_fac_fac(itech)                    !mc change ver 11
        wrk_W   = wrk_W   + cstp_wrk_fac(itech)                    !mc change ver 11
        PSW_W   = PSW_W   + cstp_secconv_fac(itech)                !mc change ver 11
        PSI_W   = PSI_W   + cstp_injconv_fac(itech)                !mc change ver 11
        fac_W   = fac_W   + cstp_facupg_fac(itech)                 !mc change ver 11
        omo_w   = omo_w   + cstp_foam_fac(itech)                   !mc change ver 11
        opsec_w = opsec_w + cstp_annsec_fac(itech)                 !mc change ver 11
        oml_w   = oml_w   + cstp_lft_fac(itech)                    !mc change ver 11
        swk_w   = swk_w   + cstp_secwrk_fac(itech)                 !mc change ver 11
        STIM_W = STIM_W   + cstp_stm_fac(itech)                    !mc change ver 11

!
! DEFINE NEW PRODUCERS, NEW INJECTORS, CONVERSIONS PRODUCER TO INJECTOR,
! INJECTOR TO PRODUCER
!
          IF(PROCDE.EQ.8)THEN     !horizontal continuity!!!!!!!
           WPP1= 2.0 +2.0*REGDRYKD(iregion)!*DRY_FAC(ITECH)                            !4.16.09 REGDRY -> REDGRYKD
           WPP2= 0.0
           WPP3= 2.0
           WPP4= 0.0

          ELSEIF(PROCDE.EQ.0.OR.PROCDE.EQ.10.or.procde.eq.17)THEN  !TEMPORARY - ADD TO INPUT FILE
           WPP1= 1.0
           WPP2= 0.0
           WPP3= 0.0
           WPP4= 0.0
          ELSEIF(procde.eq.2) THEN              !waterflood - convert all producers to injectors.
                                                ! one producer to one injector
           WPP1 = 0.0
           WPP2 = 1.0
           WPP3 = 0.0
           WPP4 = 0.0
          ELSEIF(procde.eq.3) then               !drawn from the CO2 model
           WPP3 = 0.7
           WPP4 = 0.4
           IF(vdp.gt.0.7) WPP3 = wpp3*0.5
           IF(vdp.gt.0.7) WPP4 = WPP4 * 0.75
          ENDIF

          XPP1 = WPP1
          XPP2 = WPP2
          XPP3 = WPP3
          XPP4 = WPP4

!   Step 6: Determine annual capital costs
!      A. Drilling - both developmental and dry wells (Step 2: A-C)
!      B. Determine annual facilities costs for EOR/ASR - (Step 2: D-L)

!        print*, aresid(ires)
        DO IYR = 1,NYR
            IF(PROCDE.EQ.10.or.procde.eq.17) THEN                            !4.16.09  Use the exploration dryhole
              IF(iyr.eq.1) then                                              !4.16.09  for the first well drilled.
               IF(patn(iyr).gt.0) then
               drl_cst2(iyr) = drl_cst2(iyr) + (dwc_w+dry_w*                  & !4.16.09
                 regdryUe(iregion))*1.0 *xpp1                                    !4.16.09

               drl_cst2(iyr) = drl_cst2(iyr) + (dwc_w+dry_w*                  & !4.16.09  USE THE DEVELOPMENT DRYHOLE
                 regdryUD(iregion))*(patn(iyr)-1)*xpp1                          !4.16.09  for the rest of the wells.
               END if
              else                                                           !4.16.09
               drl_cst2(iyr) = drl_cst2(iyr) + (dwc_w+dry_w*                  & !4.16.09
                 regdryUD(iregion))*patn(iyr)*xpp1                              !4.16.09
              END if                                                         !4.16.09
            ELSEIF(PROCDE.EQ.0) then                                         !MC CHANGE 4.27.09
             DRL_CST2(IYR) = DRL_CST2(IYR) + (DWC_W +dry_w *                  & !MC CHANGE 4.27.09
             regdryKD(iregion)) * patdev(ires,iyr,itech) * XPP1                 !MC CHANGE 4.27.09
            else                                                             !4.16.09
             DRL_CST2(IYR) = DRL_CST2(IYR) + (DWC_W +dry_w *                            & !for non-exploration
                             regdryKD(iregion)) * PATN(IYR) * XPP1              !4.16.09  REGDRY -> REGDRYKD
            END if        

          IF(PROCDE.EQ.1.OR.PROCDE.EQ.11.or.procde.eq.17)THEN            !MC added 17 7/14/08
            FACCOST(IYR) = FACCOST(IYR) &
                       +    NPR_W*PATN(IYR) * XPP1  !&
!                      +    PSW_W*PATN(IYR) * XPP4
!                      +    PSI_W*PATN(IYR) * XPP3
!                      +    SFU_W*PATN(IYR) * (XPP1+XPP2)
          ELSEIF(PROCDE.EQ.0) THEN                                           !MC CHANGE 4.27.09
            FACCOST(IYR) = FACCOST(IYR)                            &         !MC CHANGE 4.27.09
!                      +    NPR_W*(PATDEV(IRES,IYR,ITECH)) * XPP1            !MC CHANGE 4.27.09
                       +    PSW_W*(PATDEV(IRES,IYR,ITECH)) * XPP4  &         !MC CHANGE 4.27.09
                       +    PSI_W*(PATDEV(IRES,IYR,ITECH)) * XPP3  &         !MC CHANGE 4.27.09
                       +    fac_w*(PATDEV(IRES,IYR,ITECH)) * (xpp1+xpp2)     !MC CHANGE 4.27.09
!                      +    SFU_W*(PATDEV(IRES,IYR,ITECH)) * (XPP1+XPP2)     !MC CHANGE 4.27.09
          ELSE
            FACCOST(IYR) = FACCOST(IYR) &
!                      +    NPR_W*PATN(IYR) * XPP1
                       +    PSW_W*PATN(IYR) * XPP4 &
                       +    PSI_W*PATN(IYR) * XPP3 &
                       +    fac_w*patn(iyr) * (xpp1+xpp2)
!                      +    SFU_W*PATN(IYR) * (XPP1+XPP2)
          ENDIF
!
! INJECTANT COST ADDED TO OAM IN CASHFLOW
!
            INJ(IYR) = INJ(IYR) + INJ_OAM1*WATINJ(IYR)

            faccost(iyr) = faccost(iyr) + cst_voam_fac(itech) + &
             cst_injc_fac(itech) + cst_gna_fac(itech)

! fracturing cost added to OAM in cashflow - for the advanced case only
          if((procde.eq.10.or.procde.eq.17).and.itech.eq.max_Tech) then
            fraccst(iyr) = fraccst(iyr) + frac_W*patn(iyr)*xpp1
          end if

          if(procde.eq.0.and.itech.eq.max_tech) then
            fraccst(iyr) = fraccst(iyr) + frac_w*patdev(ires,iyr,1)*xpp1
          end if
           

          IF(PROCDE.EQ.3)THEN
             AOAM(IYR) = AOAM(IYR) + (Omo_W * SUMP(IYR) &
              +  OPSEC_W*SUMP(IYR))
          ELSE
             IF (PROCDE.EQ.1.or.procde.eq.10.OR.PROCDE.EQ.11) THEN
                OPSEC_W = 0.0
             END IF
           AOAM(IYR) = AOAM(IYR) + (Omo_W * XPATN(IYR) &
            +  OPSEC_W*XPATN(IYR))
          ENDIF
!
!         VARIABLE OPERATING COSTS
!
           OAM(IYR) = OAM(IYR) &
            + OILPROD(IYR)* OIL_OAM1*OAM_M(IYR) &
            + GASPROD(IYR)* GAS_OAM1*OAM_M(IYR) &
            + WATPROD(IYR)* WAT_OAM1*OAM_M(IYR)
!            STIMULATION COST APPLY 20% PER YEAR (OF ACTIVE PATTERNS)
             STIM(IYR) = STIM(IYR) + 0.2 * STIM_W * XPATN(IYR) * XPP1
!
!      FOR INFILL
!
          IF(PROCDE.EQ.6)THEN
             OAM(IYR) = OAM(IYR) + INJ(IYR)
             INJ(IYR) = 0.0
          ENDIF
        ENDDO    ! COST  CALCULATION

!==========================> HSM Code Start <==========================
    ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, curitr, 'cost_oil_drl_cst2',ires , aresid(ires), (DRL_CST2(IYR),IYR=1,NYR)
!===========================> HSM Code End <===========================

       IF(procde.eq.4) then
          do IYR = 1,max_yr
             torecy(iyr) = 0.0
          end do
       END if
   
!   Step 7: Determine annual operating costs

       IF(PROCDE.EQ.3) THEN
        ! WRITE(hsm_out,'(*(G0.16,:,","))') 'TORECY', curcalyr, aresid(ires), TORECY
        ! WRITE(hsm_out,'(*(G0.16,:,","))') 'ORECY', curcalyr, aresid(ires), ORECY
        ! WRITE(hsm_out,'(*(G0.16,:,","))') 'TOTINJ', curcalyr, aresid(ires), TOTINJ
        ! WRITE(hsm_out,'(*(G0.16,:,","))') 'OINJ', curcalyr, aresid(ires), OINJ
        WRITE(hsm_out,'(*(G0.16,:,","))') 'patn', curcalyr, aresid(ires), patn

!     LETS CALCULATE THE MAXIMUM CO2 RECYCLING RATE WHICH WILL
!     CALCULATE THE RECYCLE PLANT CAPACITY
!     CALCULATE MAXIMUM PRODUCTION/INJECTION RATE
          RMAX = 0.0
          CALL  MAXRATE(TORECY,max_yr-1,RMAX)
!     RECYCLE PLANT COST
         if(RMAX/365.0 <= 30.0) then
           co2_f = 1200.0*(rmax/365.0)
         else
           co2_f = 36000.0 + 750.0*(rmax/365.0-30.0)
         endif
         co2_f = co2_f*mc_jpgdp(nom_year-baseyr+1)/mc_jpgdp(2016-baseyr+1)  ! convert from $2016 to cost year dollars
         FACCOST(1) = FACCOST(1) + co2_f
!      if(RMAX/365.0.ge.60.0) write(6,*) 'dh5outCO', itimeyr, aresid(ires), rmax/365.0, co2_f, (co2rk*(RMAX/365.0*.5)**co2rb)*cpi_2003*1000.
!      if(RMAX/365.0.lt.60.0) write(6,*) 'dh5outCO', itimeyr, aresid(ires), rmax/365.0, co2_f, (co2rk*(RMAX/365.0)**co2rb)*cpi_2003*1000.

!C IN CO2 PURCHASE PRICE
          CO2OAM=0.40
          IF(RMAX/(365.0).LT.20.0) CO2OAM = 0.45
          CO2OAM2 = CO2OAM
          DO IYR= 1, NYR
            IF(TOTINJ(IYR)-TORECY(IYR).GT.0)  &
               INJ(IYR) = INJ(IYR) +  &
                             (TOTINJ(IYR)-TORECY(IYR))*CO2COST
                  OAM(IYR) = OAM(IYR) + OAM_M(IYR)*TOTINJ(IYR)*CO2OAM  &
                                                     + PSW_W * 0.25
               FOAM(IYR) = FOAM(IYR) + TOTINJ(IYR)*0.40*FCO2
          ENDDO
        ! WRITE(hsm_out,'(*(G0.16,:,","))') 'other', curcalyr, aresid(ires), 'RMAX', RMAX, 'co2_f', co2_f, 'CO2COST', &
        !         CO2COST, 'OAM_M', OAM_M(1), 'CO2OAM', CO2OAM, 'PSW_W', PSW_W, 'FCO2', FCO2
        ENDIF   ! CO2 MISCIBLE COSTS


!//////////////////////////////////////////////////////////////////////
!
!     FOR PROFILE MODEL
!
       IF(PROCDE.EQ.7) THEN
!
!     LETS CALCULATE THE MAXIMUM POLYMER INJECTION RATE AND
!     ALSO THE MAXIMUM WATER INJECTION RATE FOR INJECTION PLANT CAPACITY
!     FOLLOWING COST ADDED FOR POLYMER OR INFILL POLYMER MODEL
!     COST OF INJECTANT ( $1.50/LB)
!     CURRENTLY IN MODEL POUNDS OF POLYMER IN FUTURE IT WILL BE 1000LBS.
!
          DO IYR= 1, NYR
           INJ(IYR) = INJ(IYR) +OAM_M(IYR)*TOTINJ(IYR)*POLYCOST/1000.0
           OAM(IYR) = OAM(IYR) + XPATN(IYR)*0.25*PSI_W
          ENDDO
!C
!     COST OF WATER HANDLING  PLANT
!          CALL  MAXRATE(WATINJ,25,RMAX)
!          FACCOST(1) = FACCOST(1) + 40.0*(RMAX/365.0)
       ENDIF   ! INFILL PROFILE/PROFILE MODEL COSTS


!////////////////////////////////////////////////////////////////////
!
! FOR POLYER MODEL
!
       IF(PROCDE.EQ.5) THEN
!
!     LETS CALCULATE THE MAXIMUM POLYMER INJECTION RATE AND
!     ALSO THE MAXIMUM WATER INJECTION RATE FOR INJECTION PLANT CAPACITY
!     FOLLOWING COST ADDED FOR POLYMER OR INFILL POLYMER MODEL
!     COST OF INJECTANT ( $1.50/LB)
!     CURRENTLY IN MODEL POUNDS OF POLYMER IN FUTURE IT WILL BE 1000LBS.
!
          DO IYR= 1, NYR
           INJ(IYR) = INJ(IYR) + TOTINJ(IYR)*POLYCOST/1000.0          !FPLY removed - already included in cost
           OAM(IYR) = OAM(IYR) + XPATN(IYR)*0.25*PSI_W
          ENDDO
!
!     COST OF CHEMICAL PLANT
!          CALL  MAXRATE(TOTINJ,25,RMAX)
!          RMAX=RMAX/1000.0
!          FACCOST(1) = FACCOST(1) + 0.1*1000.0*(RMAX/365)**0.6
!
!     COST OF WATER HANDLING  PLANT
!          CALL  MAXRATE(WATINJ,25,RMAX)
!          FACCOST(1) = FACCOST(1) + 40.0*(RMAX/365)
        ENDIF   ! INFILL POLYMER/POLYMER MODEL COSTS

!///////////////////////////////////////////////////////////////////////
!
!     FOR WATER FLOOD MODEL
!
       IF(PROCDE.EQ.2) THEN
          DO IYR= 1, NYR
           OAM(IYR) = OAM(IYR) + XPATN(IYR)*0.25*PSI_W
          ENDDO
        ENDIF   ! WATERFLOOD MODEL COSTS

!////////////////////////////////////////////////////////////////////
!
!     FOR DECLINE CURVE MODEL
!
        IF(PROCDE.EQ.0) THEN
          DO IYR = 1,NYR
              DRL_CST2(IYR) = 0.0                            !MC CHANGE 4.27.09 - DRILLING COST FOR REDRILLED WELLS
                                                             !mc change 5.4.09 - included in stimulation cost - zeroed out again
              AOAM(IYR)     = 0.0
              OAM(IYR)      = 0.0
              FACCOST(IYR)  = 0.0                            !MC CHANGE 4.27.09 - FACILITIES COSTS FOR REDRILLED WELLS
                                                             !mc change 5.4.09 - included in stimulation cost - zeroed out again
              INJ(IYR)      = 0.0
              if(frac(ires)) stim(iyr) = 0.0                 !mc change B - fracturing instead of stimulation

          END DO
          STMA = 0.0
          STMP = 0.0
          DO IYR = 1,NYR
             OAM(IYR) = OAM(IYR) &
                        + ((oil_oam1 * OILPROD(IYR)) &
                        + (GAS_OAM1 * gasprod(iyr)) &
                        + (WAT_OAM1 * watprod(iyr)))*oam_m(iyr)

             AOAM(IYR) = AOAM(IYR) + (OPSEC_W*OAM_M(IYR)*SUMP(IYR)/5)
!     &                   nwelloil)
          END DO
!        if (itech.eq.1) then
!          write (OGBUG1,1833) aresid(ires),sump(1),
!     &     (oilprod(iyr),iyr=1,10),(gasprod(iyr),iyr=1,10),
!     &     (watprod(iyr),iyr=1,10),(aoam(iyr),iyr=1,10),
!     &     (oam(iyr),iyr=1,10)
!        end if

! 1833   format (a11,3x,f10.2,3x,50(f10.2,3x))

        ENDIF ! DECLINE CURVE MODEL

!      D. Calculate environmental costs
   
!      E. Calculate overhead costs


        IF(PROCDE.eq.6)THEN
          GNA_EXP3 = 0.10
          GNA_CAP3 = 0.10
        ELSE
          GNA_EXP3 = GNA_EXP2
          GNA_CAP3 = GNA_CAP2
        ENDIF

        GNA_EXP(ITECH) = GNA_EXP3
        GNA_CAP(ITECH) = GNA_CAP3
        GG_FAC = GG_FAC2

        gna_exp(itech) = gna_exp(itech) * chg_gna_fac(itech)
        gna_cap(itech) = gna_cap(itech) * chg_gna_fac(itech) 

        IF (opt_Dbg2) THEN
           WRITE (OGBUG1,*) 'NUMBER OF PRODUCERS PER PATTERN    :',WPP1
           WRITE (OGBUG1,*) 'NUMBER OF INJECTORS PER PATTERN    :',WPP2
           WRITE (OGBUG1,*) 'NUMBER OF CONVERSIONS PER PATTERN  :',WPP3
           WRITE (OGBUG1,*) 'NUMBER OF PRIMARY TO SECONDARY     :',WPP4
           WRITE (OGBUG1,*)
           WRITE (OGBUG1,*)
        END IF

        end subroutine

!***********************************************************************
!***********************************************************************
!***********************************************************************
!
        SUBROUTINE MAXRATE(X,Y,Z)
!
!     READS ARRAY X FOR ELEMENTS Y AND RETURNS Z AS THE MAXRATE
!
        implicit none

        INCLUDE 'OGSMPARM'
        REAL X(MAX_YR)
        REAL Z,MAXV,MINV
        INTEGER Y,iyr
        MAXV = 0.0
        Z    = 0.0
        DO IYR = 1,Y
          IF(IYR.EQ.1)THEN
            MAXV = X(1)
            MINV = X(1)
            GOTO 100
          ENDIF
          IF(X(IYR).GT.MAXV)MAXV=X(IYR)
          IF(X(IYR).LT.MINV)MINV=X(IYR)
100     CONTINUE
        END DO
        Z = MAXV
        RETURN
        END SUBROUTINE

!***********************************************************************
!***********************************************************************
!***********************************************************************

        SUBROUTINE INITPRE
!-------------------------------------------------------------------------
! THIS SUBROUTINE INITIALIZES THE VARIABLES USED IN THE PRECOST SUBROUTINE
!-------------------------------------------------------------------------

       include'parametr'     ! nems dimension parameters
       include'ncntrl'       ! nems control variables
       include'ogsmparm'     ! ogsm parameter file
       INCLUDE 'ogsml48'

!       LOCAL VARIABLES

        INTEGER IYR

        XPP1 = 0.0
        XPP2 = 0.0
        XPP3 = 0.0
        XPP4 = 0.0

        DO IYR = 1,MAX_YR
              OILPROD(IYR)  = 0.0
              GASPROD(IYR)  = 0.0
              NGLPROD(IYR)  = 0.0
              WATPROD(IYR)  = 0.0
              WATINJ(IYR)   = 0.0
              TOTINJ(IYR)   = 0.0
              TORECY(IYR)   = 0.0
              XCUMPROD(IYR) = 0.0
              DRL_CST2(IYR) = 0.0
              DRL_CST(IYR)  = 0.0
              FACCOST(IYR)  = 0.0
              INJ(IYR)      = 0.0
              AOAM(IYR)     = 0.0
              OAM(IYR)      = 0.0
              SUMP(IYR)     = 0.0
              PATN(IYR)     = 0.0
              XPATN(IYR)    = 0.0
              REMRES(IYR)   = 0.0
              gremres(iyr)  = 0.0                                              !mc change ver 7
              iremres(iyr)  = 0.0                                              !mc change ver 7
              igremres(iyr) = 0.0                                              !mc change ver 7
              FOAM(IYR)     = 0.0
              COMP(IYR)     = 0.0
              STIM(IYR)     = 0.0
              OAM_COMP(IYR) = 0.0
              fraccst(iyr)  = 0.0
! NEW VARIABLES FOR ENVIRONMENTAL COSTS
              EXIST_EOAM(IYR) = 0.0
              NEW_EOAM(IYR)   = 0.0
              EXIST_ECAP(IYR) = 0.0
              NEW_ECAP(IYR)   = 0.0
              PROC_OAM(IYR)   = 0.0

        ENDDO
        RETURN
        end

!***************************************************************
!from ECON_COSTG.FOR
!     Last change:  MC    5 May 2009    9:16 am
        subroutine cost_gas(ires,itech)
        
!  this subroutine calculates the capital and operating costs for gas drilling
        implicit none

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
        include 'ogsmbfw'
        include 'ogsmugr'
        include 'ogsml48'
        include 'ogsmout'

        INTEGER ires,depthcat,iyr,patlife,nyr,ik,ib,ih,ii,myr
        INTEGER IYR2,igen(max_yr),lyr,kyr,jyr,ix,iyr1,itech,ishift

!--- (NEW) ---------------------------------------------------------------------
!
	REAL MINDEPTHgas, MAXDEPTHgas	!depth range variables for output write
!
! -----------------------------------------------------------------------------

        REAL term,rmaxw,rmaxp,rmax,prssin
        real tcumresv,tcumresv2                                               !mc change ver 7
        REAL OPRODO(MAX_YR)
        REAL GPRODO(MAX_YR)
        REAL NPRODO(MAX_YR)
        REAL WPRODO(MAX_YR)
        REAL OINJO(MAX_YR)
        REAL WINJO(MAX_YR)
        REAL ORECYO(MAX_YR)
        REAL totgasprod(max_yr)
        REAL temporesv(max_yr)
        REAL tempgresv(max_yr)
        real tempiresv(max_yr)                                                !mc change ver 7
        real tempiresvg(max_yr)                                               !mc change ver 7

        REAL PEAKRATE
        REAL PEAKDAILY_RATE
        REAL daily_gas,daily_wat,daily_oil

        REAL oil_oam1,gas_oam1,wat_oam1,inj_oam1,totpatn                      !mc change ver 11

!  temporary variables
        INTEGER ist,co2code,ll3

        REAL ngen

        real cstp_drl_fac0(max_tech)                     !PROJECT LEVEL cost to apply drilling technology
        real cstp_stm_fac0(max_tech)                     !PROJECT LEVEL cost to apply stimulation technology
        real cstp_comp_fac0(max_tech)                    !PROJECT LEVEL cost to apply completion technology
        real cstp_fac_fac0(max_tech)                     !PROJECT LEVEL cost to apply facilities technology (
        real cstp_secconv_fac0(max_tech)                 !PROJECT LEVEL cost to apply secondary conversion te
        real cstp_injconv_fac0(max_tech)                 !PROJECT LEVEL cost to apply injector conversion tec
        real cstp_facupg_fac0(max_tech)                  !PROJECT LEVEL cost to apply facilities upgrade tech
        real cstp_wrk_fac0(max_tech)                     !PROJECT LEVEL cost to apply workover technology
        real cstp_foam_fac0(max_tech)                    !PROJECT LEVEL cost to apply fixed annual O & M tech
        real cstp_voam_fac0(max_tech)                    !PROJECT LEVEL cost to apply variable annual O & M t
        real cstp_gna_fac0(max_tech)                     !PROJECT LEVEL cost to apply G & A technology
        real cstp_annsec_fac0(max_tech)                  !PROJECT LEVEL cost to apply secondary producer tech
        real cstp_lft_fac0(max_tech)                     !PROJECT LEVEL cost to apply lifting technology
        real cstp_secwrk_fac0(max_tech)                  !PROJECT LEVEL cost to apply secondary workover tech
        real cstp_injc_fac0(max_tech)                    !PROJECT LEVEL cost to apply injection technology
        real cstp_cmp_fac0(max_tech)                     !PROJECT LEVEL cost to apply compression technology
        real prodtech, prodadj, prodadj2
        real drctech, drcadj
        real leotech, leoadj
        real spacing, wlspacing, wlsdrilled, dfactor

        logical once
        data once/.false./

!  set temporary data

        FRAC_CO2 = 0.02
        FRAC_N2  = 0.02
        FRAC_H2S = 0.02
!       FRAC_NGL = 0.02

!!!        itimeyr = 1!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
!  initialization
        nyr = max_yr-1
        DO IYR =1,nyr
               OPRODO(IYR) = OPROD(IYR)
               GPRODO(IYR) = GPROD(IYR)
               NPRODO(IYR) = NPROD(IYR)
               WPRODO(IYR) = 0.0
               OINJO(IYR)  = 0.0
               WINJO(IYR)  = 0.0
               ORECYO(IYR) = 0.0
               IGEN(IYR)   = 0
               temporesv(iyr) = 0.0
               tempgresv(iyr) = 0.0
               tempiresv(iyr) = 0.0                                            !mc change ver 7
               tempiresvg(iyr) = 0.0                                           !mc change ver 7
        ENDDO
        tcumresv = 0.0                                                         !mc change ver 7
        tcumresv2 = 0.0                                                        !mc change ver 7

        PEAKRATE = 0.0
        PEAKDAILY_RATE = 0.0
        DWC_W    = 0.0
        DRY_W    = 0.0

! -- (NEW) ----- for testing only -------------------------------------------------------------
!              
	DNC_COST = 0.0	 		!initialize New Gas Well D&C Cost variable
	DNC_COST_DRY = 0.0 		!initialize New Dry Well D&C Cost variable
!
	MINDEPTHgas = 100		!minimum drilling depth for D&C cost Output file (9934)
	MAXDEPTHgas = 100000		!maximum drilling depth for D&C cost Output file (9934)
!
! ---------------------------------------------------------------------------------------------

        fwc_w    = 0.0
        FOAMG_1  = 0.0
        FOAMG_2  = 0.0
        STIM_W   = 0.0
        COMP_W   = 0.0

        CALL MAXRATE(GPROD,max_yr-1,PEAKRATE)
        PEAKDAILY_RATE = PEAKRATE * 1000.0 / 365.0

        if (procde.ge.18.and.procde.le.23) THEN            !recalculate the maximum daily rate for developing/undiscovered unconventional resources
           PEAKRATE = 0.0
           IF(ainjinj(ires,1,itech).gt.0.0) PEAKRATE = &
              APRODGAS(IRES,1,itech)/ainjinj(ires,1,itech)
        PEAKDAILY_RATE = PEAKRATE * 1000.0 / 365.0
        end if

        if (procde.ge.11.and.procde.le.15) THEN            !recalculate the maximum daily rate for decline curve resources
           PEAKRATE = 0.0
           IF(totpat.gt.0.0) PEAKRATE = &
              APRODGAS(IRES,1,itech)/totpat
        PEAKDAILY_RATE = PEAKRATE * 1000.0 / 365.0
        end if


!  rationale: the gas production coming into the model for 21 - 23 is
!  already at the reservoir level.  The peakdaily_rate is at a WELL level.
!  Adjustment is required to get to the proper unit.

!  determine the indices for the cost equations

        depthcat = 0
!        if (depth.lt.2000.0) THEN                                      !mc change ver3
        if (depth.lt.5000.0) THEN                                       !mc change ver3
          depthcat = 1
!       ELSEIF(depth.ge.2000.0.and.depth.lt.4000.0) then               !mc change ver3
        ELSEIF(depth.ge.5000.0.and.depth.lt.10000.0) then               !mc change ver3
          depthcat = 2
!        ELSEIF(depth.ge.4000.0.and.depth.lt.8000.0) then               !mc change ver3
        ELSEIF(depth.ge.10000.0.and.depth.lt.15000.0) then              !mc change ver3
          depthcat = 3
!        ELSEIF(depth.ge.8000.0) then                                   !mc change ver3
        ELSEIF(depth.ge.15000.0) then                                   !mc change ver3
          depthcat = 4
        end if
        if (iregion.eq.1) then
          IF(state.eq.'IL'.or.state.eq.'IN'.or.state.eq.'KY'.or. &
             state.eq.'MI') then
             regnum = 2
          else
             regnum = 1
          END if
        else
          regnum = iregion+1
        end if

!   Step 1: Determine cost multipliers based on current and base gas prices
           TERM = (OILPRICEC(1) - baseoil)/baseoil
!          TERM = 0.0

           TANG_M  = (1.0 + gmult_tang * TERM)**0.5
           INTANG_M= (1.0 + gmult_int * TERM)**0.5
           OAM_M   = (1.0 + gmult_oam * TERM)**0.5

  1     format (a20,<max_yr-1>(3x,f12.2))
  2     format (a20,<max_yr-1>(3x,f12.7))
  3     format (a20,2(3x,f12.2))
  4     format (a20,2(3x,f12.7))
  5     format (a20,3x,f12.2)
  6     format (a20,3x,i2)
  7     format (a20,3x,a2)
  18    format (a20,a11)

!***********************************************************************
!
!     GET THE GAS PROCESSING COST IN $/MCF (PROC_CST)
!
!***********************************************************************
!
        CALL PROCESS(FRAC_N2,FRAC_CO2,FRAC_H2S,FRAC_NGL,PROC_CST,RESID, &
         iregion)

!
!   Step 2: calculate unit costs.
!      A. Drilling and completion.  
!         Use OLOGSS region and project depth to calculate drilling and completion cost for vertical well

        IF (PROCDE.eq.8.or.procde.eq.9) THEN
           DWC_W =GAS_DWCK(9,depthcat)                                         & !horizontal drilling for
                  + GAS_DWCA(9,depthcat)*DEPTH**2 &                              !process codes 8 & 9
                  + GAS_DWCB(9,depthcat)*(DEPTH**2)*NLAT &
                  + GAS_DWCC(9,depthcat)*(DEPTH**2)*(NLAT*LATLEN)
         else
           DWC_W =GAS_DWCK(regnum,depthcat)                                    & !all other drilling
                  + GAS_DWCA(regnum,depthcat)*DEPTH &
                  + GAS_DWCB(regnum,depthcat)*DEPTH**2 &
                  + GAS_DWCC(regnum,depthcat)*DEPTH**3
           DWC_W = DWC_W /1000.0

           if (.not.(procde.eq.13.or.(aresflag(ires).ge.3.and.(procde.eq.14.or.procde.eq.15)).or.  &
               procde.eq.18.or.procde.eq.20.or.procde.eq.21.or.procde.eq.22)) then   ! add lateral  
             nlat = 0.
           endif
!  --------------- (NEW) ------------------------------------------------------------------------------

!          Equations for estimating the D&C gas well costs using new function related to fixed, variable 
!	   drilling, and trouble costs A*exp(-b*Depth)+ C*Depth + D*Depth*Depth + E*exp(f*Depth) based on fits to JAS 2007 
!	   survey data and annual cost multipliers (added by msc - May 2013)

!    Set COEF for each regnum value (accounting for 1a region and northern rockies region (7))
           IF (regnum .gt. 1) COEF = regnum - 1		! standard relationship for region 1a, 2, 3, 4, 5, 6, and 7
           IF (regnum .eq. 1) COEF = 1	! no index shift for region 1
          

	   DNC_COST = DNCC_COEF(2,COEF,1) * exp(-DNCC_COEF(2,COEF,2) * DEPTH) 				!first cost term
	   DNC_COST = DNC_COST + DNCC_COEF(2,COEF,3) * (DEPTH+nlat*latlen) + &
                      DNCC_COEF(2,COEF,4) * (DEPTH+Nlat*latlen)**2                              	!second cost term
           DNC_COST = DNC_COST + DNCC_COEF(2,COEF,5) * exp(DNCC_COEF(2,COEF,6) * DEPTH)			!third cost term
	   DNC_COST = DNC_COST/1000								!D&C cost estimate

           if(ogreport(26).eq.1.and.((depth.ge.MINDEPTHgas) .and. (depth.le.MAXDEPTHgas))) then
     write(9948,*) depth, 'DNCC Coefficients (A,B,C,D,E,F)  : ',DNCC_COEF(2,COEF,1),DNCC_COEF(2,COEF,2),DNCC_COEF(2,COEF,3),DNCC_COEF(2,COEF,4),DNCC_COEF(2,COEF,5), DNCC_COEF(2,COEF,6)
            SELECT CASE (regnum)
             CASE (1)
                write(9921,*) 'G ',regnum,':',DEPTH,':', DWC_W,':',DNC_COST,':',aresid(ires)
             CASE (2)
                write(9922,*) 'G ',regnum,':',DEPTH,':', DWC_W,':',DNC_COST,':',aresid(ires)
             CASE (3)
                write(9923,*) 'G ',regnum,':',DEPTH,':', DWC_W,':',DNC_COST,':',aresid(ires)
             CASE (4)
                write(9924,*) 'G ',regnum,':',DEPTH,':', DWC_W,':',DNC_COST,':',aresid(ires)
             CASE (5)
                write(9925,*) 'G ',regnum,':',DEPTH,':', DWC_W,':',DNC_COST,':',aresid(ires)
             CASE (6)
                write(9926,*) 'G ',regnum,':',DEPTH,':', DWC_W,':',DNC_COST,':',aresid(ires)
             CASE (7)
                write(9927,*) 'G ',regnum,':',DEPTH,':', DWC_W,':',DNC_COST,':',aresid(ires)
             CASE DEFAULT
            END SELECT
 	   end if
           DWC_W = DNC_COST  ! New equation for DWC_W (July 2013)
        END if
!==========================> HSM Code Start <==========================
    ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, curitr, 'cost_gas', ires, aresid(ires), DNC_COST, DEPTH
!===========================> HSM Code End <===========================
! ------------------------------------------------------------------------------------------------------------

!      B. Dry Hole Well Cost
        IF (PROCDE.eq.8.or.procde.eq.9) THEN
           DRY_W =dry_DWCK(9,depthcat)                                         & !horizontal drilling for
                  + dry_DWCA(9,depthcat)*DEPTH**2 &                              !process codes 8 & 9
                  + dry_DWCB(9,depthcat)*(DEPTH**2)*NLAT &
                  + dry_DWCC(9,depthcat)*(DEPTH**2)*(NLAT*LATLEN)
         else
           DRY_W =DRY_DWCK(regnum,depthcat) &                                    !all other drilling
                  + DRY_DWCA(regnum,depthcat)*DEPTH &
                  + DRY_DWCB(regnum,depthcat)*DEPTH**2 &
                  + DRY_DWCC(regnum,depthcat)*DEPTH**3
           DRY_W = DRY_W /1000.0

! ------------- (NEW) ---------------------------------------------------------------------------------
!
!       Equations for estimating the dry well drilling costs using new function 
!	related to fixed, variable drilling, and trouble costs 
!	D*(A*exp(-b*Depth)+ C*Depth + D*Depth*Depth + E*exp(F*Depth)) based on fits to JAS 2007 
!	survey data and annual cost multipliers (added by msc - July 2013)
!
	   DNC_COST_DRY = DNCC_COEF(3,COEF,1)*exp(-DNCC_COEF(3,COEF,2)*DEPTH) 
	   DNC_COST_DRY = DNC_COST_DRY + DNCC_COEF(3,COEF,3)*DEPTH + DNCC_COEF(3,COEF,4)*DEPTH*DEPTH
	   DNC_COST_DRY = DNC_COST_DRY + DNCC_COEF(3,COEF,5)*exp(DNCC_COEF(3,COEF,6)*DEPTH)
	   DNC_COST_DRY = DNC_COST_DRY/1000

    if(ogreport(26).eq.1.and.((depth.ge.MINDEPTHgas) .and. (depth.le.MAXDEPTHgas))) then
           SELECT CASE (regnum)
             CASE (1)
                write(9931,*) 'DG ',regnum,':',DEPTH,':', DRY_W,':',DNC_COST_DRY,':',aresid(ires)
             CASE (2)
                write(9932,*) 'DG ',regnum,':',DEPTH,':', DRY_W,':',DNC_COST_DRY,':',aresid(ires)
             CASE (3)
                write(9933,*) 'DG ',regnum,':',DEPTH,':', DRY_W,':',DNC_COST_DRY,':',aresid(ires)
             CASE (4)
                write(9934,*) 'DG ',regnum,':',DEPTH,':', DRY_W,':',DNC_COST_DRY,':',aresid(ires)
             CASE (5)
                write(9935,*) 'DG ',regnum,':',DEPTH,':', DRY_W,':',DNC_COST_DRY,':',aresid(ires)
             CASE (6)
                write(9936,*) 'DG ',regnum,':',DEPTH,':', DRY_W,':',DNC_COST_DRY,':',aresid(ires)
             CASE (7)
                write(9937,*) 'DG ',regnum,':',DEPTH,':', DRY_W,':',DNC_COST_DRY,':',aresid(ires)
             CASE DEFAULT
           END SELECT
    end if
           DRY_W = DNC_COST_DRY ! New equation for DWC_W (July 2013)
        END if
!==========================> HSM Code Start <==========================
    ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, curitr, 'cost_gas_dry',ires , aresid(ires), DNC_COST_DRY, DEPTH
!===========================> HSM Code End <===========================
!  ---------------------------------------------------------------------------


        dwc_w = dwc_w * (ognowell(curiyr-1)/30000.)**0.25
        dry_w = dry_w * (ognowell(curiyr-1)/30000.)**0.25
!
!      C. Gas Facilities Cost
           fwc_w = facgk(regnum,depthcat) &
                  + facga(regnum,depthcat)*depth &
                  + facgb(regnum,depthcat)*peakdaily_rate &
                  + facgc(regnum,depthcat)*(depth*peakdaily_rate)
           fwc_w = fwc_w /1000.0

!***********************************************************************

!   Step 3: Calculate annual operating costs
!      A. Calculate fixed operating costs
       foamg_w = omgk(regnum,depthcat) &
               + omga(regnum,depthcat)*depth &
               + omgb(regnum,depthcat)*peakdaily_rate &
               + omgc(regnum,depthcat)*(depth*peakdaily_rate)
       foamg_w = foamg_w / 1000.0

!      B. Gas Stimulation Costs
           STIM_W = (STIM_A + STIM_B * DEPTH)/1000.0

        PRSSIN=200.0
        COMP_W   = (22.0*1000.0/ PRSSIN*PEAKDAILY_RATE /1000.0)/1000.0
        COMP_W   = COMP_W * COST_BHP
        COMP_OAM = COMP_VC
   
!      C. Calculate overhead costs
        IF(PROCDE.eq.6)THEN
          GNA_EXP3 = 0.10
          GNA_CAP3 = 0.10
        ELSE
          GNA_EXP3 = GNA_EXP2
          GNA_CAP3 = GNA_CAP2
        ENDIF

        GNA_EXP(ITECH) = GNA_EXP3
        GNA_CAP(ITECH) = GNA_CAP3
        GG_FAC = GG_FAC2

        gna_exp(itech) = gna_exp(itech) * chg_gna_fac(itech)
        gna_cap(itech) = gna_cap(itech) * chg_gna_fac(itech)
!      D. Calculate environmental costs
   

!  Apply the technology levers to the costs.  Two types of levers are applied:
!     1) The change in cost from new technology       (unit: fraction of cost)
!     2) The cost to apply the technology             (unit: K$              )   

         DWC_W = DWC_W * chg_drl_fac(itech) + cst_drl_fac(itech)
         DWC_W = DWC_W * chg_comp_fac(itech) + cst_comp_fac(itech)

         fwc_w = fwc_w * chg_fac_fac(itech) + cst_fac_fac(itech)

         dry_W = dry_W * chg_drl_fac(itech) + cst_drl_fac(itech)
         dry_W = dry_W * chg_comp_fac(itech) + cst_comp_fac(itech)      

         STIM_W = STIM_W * chg_stm_fac(itech) + cst_stm_fac(itech)
         COMP_W = COMP_W * chg_cmp_fac(itech) + cst_cmp_fac(itech)

         FOAMG_1 = FOAMG_1 * chg_foam_fac(itech)
         FOAMG_2 = FOAMG_2 * chg_foam_fac(itech)

!  apply CPI indices to costs
       dwc_w = dwc_w * cpi_2005
       dry_w = dry_w * cpi_2005

      foamg_w = foamg_w * cpi_2005
       stim_w = stim_w * cpi_2003
       comp_w = comp_w * cpi_2003
       fwc_w = fwc_w * cpi_2005

         oil_oam1 = oil_oam(procde+1) * cpi_2005
         gas_oam1 = gas_oam(procde+1) * cpi_2005
         wat_oam1 = wat_oam(procde+1) * cpi_2005
         inj_oam1 = inj_oam(procde+1) * cpi_2005

         oil_oam1 = oil_oam1 * chg_voam_fac(itech)
         gas_oam1 = gas_oam1 * chg_voam_fac(itech)
         wat_oam1 = wat_oam1 * chg_voam_fac(itech)
         inj_oam1 = inj_oam1 * chg_injc_fac(itech)

         oil_oam1 = oil_oam1 + chg_ooil_fac(itech)   !mc change ver 11
         gas_oam1 = gas_oam1 + chg_ogas_fac(itech)   !mc change ver 11
         wat_oam1 = wat_oam1 + chg_owat_fac(itech)   !mc change ver 11
         inj_oam1 = inj_oam1 + chg_oinj_fac(itech)   !mc change ver 11

         if (opt_Dbg2) then
           write (OGBUG1,*) 'RESERVOIR ID:                      :',RESID
           WRITE (OGBUG1,*) 'TECHNOLOGY CASE                    :',ITECH
           WRITE (OGBUG1,*) 'DEPTH       :                      :',DEPTH
           WRITE (OGBUG1,*) 'OLOGSS REGION:                     :',IREGION
           WRITE (OGBUG1,*) 'TIMING YEAR:                       :',ITIMEYR
           WRITE (OGBUG1,*) 'REGION KNOWN DEV. DRY HOLE RATE    :',                     & !4.16.09
            REGDRYKD(IREGION)
           WRITE (OGBUG1,*) 'REGION UNDISC EXP. DRY HOLE RATE   :',                     & !4.16.09
            REGDRYUE(IREGION)
           WRITE (OGBUG1,*) 'REGION UNDISC DEV. DRY HOLE RATE   :',                     & !4.16.09
            REGDRYUD(IREGION)
           WRITE (OGBUG1,*) 'CPI 2005,CPI 2003                  :',CPI_2005 &
            ,CPI_2003
           WRITE (OGBUG1,*) 'DRILLING & COMPL.                  :',DWC_W
           WRITE (OGBUG1,*) 'DRY HOLE                           :',DRY_W

           WRITE (OGBUG1,*) 'FACILITIES COSTS                   :',FWC_W
           WRITE (OGBUG1,*) 'FIXED ANNUAL O&M COST              :',FOAMG_w
           WRITE (OGBUG1,*) 'STIMULATION COST                   :',STIM_W
           WRITE (OGBUG1,*) 'COMPRESSION COST                   :',COMP_W
           WRITE (OGBUG1,*) 'O&M FOR OIL PRODUCTION             :',OIL_OAM1
           WRITE (OGBUG1,*) 'O&M FOR GAS PRODUCTION             :',GAS_OAM1
           WRITE (OGBUG1,*) 'O&M FOR WATER PRODUCTION           :',WAT_OAM1
           WRITE (OGBUG1,*) 'O&M FOR INJECTION                  :',INJ_OAM1
           WRITE (OGBUG1,*)
           WRITE (OGBUG1,*)
         end if

!   Step 4: Determine well development profile
       ishift = 0
       IF(apcode(ires).eq.10.or.apcode(ires).eq.16) ishift = disclag
         call patt_development(ires,1+ishift,302)

!           determine the economic life of one pattern
         econ_life = max_yr-1
         call patcost_gas(itech)
         call patcash(patlife,302)
         econ_life = patlife

 12      format (a20,3x,f12.2)
 13      format (a20,3x,i12)

!   Step 5: Determine annual production profile
       call initpre

       do i = 1,max_yr-1
          patn1(i) = PATDEV(IReS,I,ITeCH)
       end do

       totpatn = 0.0                                              !mc change ver 11
       do i = 1,max_yr-1                                          !mc change ver 11
         do J = 1,2                                               !mc change ver 11
            totpatn = totpatn + patdev(ires,i,J)                  !mc change ver 11
         end do                                                   !mc change ver 11
       end do                                                     !mc change ver 11

       IF (STARTPR.GT.1) THEN
! SHIFT THE PRICE, AND DEVELOPMENT SCHEDULE, WRITE ZEROES AT THE END
! OF THE PATTERN DEVELOPMENT SCHEDULE
         do i = 1,startpr-1
           patterns(i) = 0.0
         end do
         do i = startpr,max_yr
           patterns(i) = patn1(i-startpr+1)
         end do
!           DO I=1,MAX_YR-STARTPR
!             PATTERNS(I) = PATN1(I+STARTPR-1)
!           END DO
       ELSE
           DO I=1,MAX_YR
             PATTERNS(I) = 0.0
             PATTERNS(I) = PATN1(I)
           END DO
       ENDIF !END OF PROJECT SHIFT IF STATEMENT

       IF(procde.ge.11.and.procde.le.15) then                                     !mc change 5.5.09
         do i=1,max_yr                                                            !mc change 5.5.09
           IF(i.eq.1) then                                                        !mc change 5.5.09
             patterns(i) = 0.0!totpat                                             !mc change 5.5.09
           else                                                                   !mc change 5.5.09
             patterns(i) = 0.0                                                    !mc change 5.5.09
           END if                                                                 !mc change 5.5.09
         end do                                                                   !mc change 5.5.09
       END if                                                                     !mc change 5.5.09

!
       IF(PROCDE.EQ.0.OR.PROCDE.GE.11.AND.PROCDE.LE.15) THEN
          ishift = 0
          IF(PROCDE.GE.11.AND.PROCDE.LE.15) THEN                                  !MC CHANGE 4.27.09
            DO IYR=1,MAX_YR-1                                                     !MC CHANGE 4.27.09
              IF(IYR.EQ.ISHIFT+1.AND.ITECH.EQ.1) PATTERNS(IYR) = TOTPAT           !MC CHANGE 4.27.09
            END DO                                                                !MC CHANGE 4.27.09
          ELSE                                                                    !MC CHANGE 4.27.09
            DO IYR=1,max_yr-1
               PATTERNS(IYR) = 0.0
               patdev(ires,iyr,1) =0.0
            END DO

            IF(procde.eq.0) then
              patterns(1+ishift) = 1
            else
              PATTERNS(1+ishift) = TOTPAT
            END if

            patdev(ires,1+ishift,1) = patterns(1+ishift)
            patdev(ires,1+ishift,2) = 0.0
            IF (ITECH.EQ.2) THEN
               DO IYR=1,max_yr-1
                  PATTERNS(IYR) = 0.0
                  patdev(ires,iyr,2) = 0.0
               END DO
            END IF
         END IF                                                                   !MC CHANGE 4.27.09
       ENDIF

!   Step 5: Determine annual production profile
       ilife = patlife
        DO I=1,nyr
           PATN(I)=PATTERNS(I)
        END DO
!
! SETTING ACTIVE PATTERNS BY YEAR
!
       XPATN(1) = PATN(1)
       DO IK =2,nyr
          XPATN(IK) = XPATN(IK-1) + PATN(IK)
!
          IF ( IK.GT.(ILIFE+1)) THEN
             XPATN(IK) = XPATN(IK) - PATN(IK-ILIFE-1)
          ENDIF
!
          IF ( IK.GT.(ILIFE*2+1)) THEN
             XPATN(IK) = XPATN(IK) - PATN(IK-ILIFE-1)
          ENDIF
!
          IF (XPATN(IK).LE.0.0) XPATN(IK) = 0.0
       ENDDO

        LYR = 0
        IYR = 0
        KYR = 0
        JYR = 0
        DO IX = nyr,1,-1
           IF (PATN(IX).NE.0.0)THEN
              JYR = IX
              GO TO 15
           ENDIF
        ENDDO
15      CONTINUE
!
! CALCULATE THE PRODUCTION PROFILE FOR THE PROJECT
!

! initialize the production arrays
       do IYR = 1,max_yr
         oilprod(iyr) = 0.0
         gasprod(iyr) = 0.0
         nglprod(iyr) = 0.0
         watprod(iyr) = 0.0
         totinj(iyr) = 0.0
         watinj(iyr) = 0.0
         torecy(iyr) = 0.0
         sump(iyr) = 0.0
       end do
       wlspacing = alatlen(ires,1)*870./43560.

        IF(PROCDE.GE.16.AND.PROCDE.LE.23) THEN
           do iyr = 1,max_yr
             aresvoil(ires,iyr,itech) = 0.0
             aresvgas(ires,iyr,itech) = 0.0
           end do


           prodadj2 = 1.0
           DO IYR = 1,JYR
              KYR = 0
              LYR = IYR + nyr - 1
              IF(LYR.GE.max_yr)LYR=max_yr 
              if (procde.eq.10.or.procde.eq.16) prodtech = togtechon(3,1)*techadj_on(1,1)
              if (procde.eq.17) prodtech = togtechon(3,2)*techadj_on(1,1)
              if (procde.ge.3.and.procde.le.9) prodtech = togtechon(3,3)*techadj_on(1,1)
              if (procde.eq.18.or.procde.eq.22) prodtech = togtechon(3,4)*techadj_on(1,1)
              if (procde.eq.20.or.procde.eq.21) prodtech = togtechon(3,5)*techadj_on(1,1)
              if (procde.eq.19.or.procde.eq.23) prodtech = togtechon(3,6)*techadj_on(1,1)
              prodadj = 1.0
              if (curiyr.lt.techyr.and.curiyr+iyr-1.ge.techyr) prodadj = (1.0+prodtech)**(curiyr+iyr-techyr)
              if (curiyr.ge.techyr) prodadj = (1.0+prodtech)**(iyr-1)
              if ((procde.ge.20.and.procde.le.22).and.anwellinj(ires).le.0.and.(curiyr+iyr.gt.techyr+1.and.iyr.le.5))  &
                    prodadj2 = (1.0+2.*prodtech)**(iyr)

! DIMINISHING_RETURNS
              DO IYR1 = IYR,LYR
                 KYR = KYR+1
                 wlsdrilled = atotacres(ires)/apatsiz(ires)-atotpat(ires,1)+sum(patn(1:IYR))
                 if(wlsdrilled.gt.0.) then
                   spacing = atotacres(ires)/wlsdrilled
                   if(spacing.gt.0..and.spacing.lt.wlspacing.and.(procde.eq.21.or.procde.eq.22)) then
                      dfactor = (1.0-1./(1.+exp(-1.*((wlspacing/spacing-1)-1.))))
                      oprod(kyr) = oprodo(kyr)*dfactor
                      gprod(kyr) = gprodo(kyr)*dfactor
                      nprod(kyr) = nprodo(kyr)*dfactor
                      prodadj = 1.
                      prodadj2 = 1.
                   endif
                 endif
                 OILPROD(IYR1) = OILPROD(IYR1) + OPROD(KYR)*PATN(IYR)*prodadj*prodadj2    ! OIL PRODUCTION
                 GASPROD(IYR1) = GASPROD(IYR1) + GPROD(KYR)*PATN(IYR)*prodadj*prodadj2    ! GAS PRODUCTION
                 NGLPROD(IYR1) = NGLPROD(IYR1) + NPROD(KYR)*PATN(IYR)*prodadj*prodadj2    ! NGL PRODUCTION
                 WATPROD(IYR1) = WATPROD(IYR1) + WPROD(KYR)*PATN(IYR)    ! WATER PRODUCTION
                 TOTINJ(IYR1)  = TOTINJ(IYR1)  + OINJ(KYR)*PATN(IYR)     ! INJECTANT
                 WATINJ(IYR1)  = WATINJ(IYR1)  + WINJ(KYR)*PATN(IYR)     ! WATER INJECTED
                 TORECY(IYR1)  = TORECY(IYR1)  + ORECY(KYR)*PATN(IYR)    ! RECYCLED VOLUME
                 SUMP(IYR1)    = SUMP(IYR1)    + PATN(IYR)
              ENDDO
                 do myr = 1,40!25!max_yr                                                              !mc change ver 7
                 aresvoil(ires,iyr,itech) = aresvoil(ires,iyr,itech) + &
                   oprod(myr)*patn(iyr)
                 aresvgas(ires,iyr,itech) = aresvgas(ires,iyr,itech) + &
                   gprod(myr)*patn(iyr)
                end do
           ENDDO
           do iyr = 1,40!24!max_yr-1                                                                  !mc change ver 7
              IF(iyr.eq.1) then
                 temporesv(iyr) = aresvoil(ires,iyr,itech)
                 tempgresv(iyr) = aresvgas(ires,iyr,itech)
              else
                 temporesv(iyr) = aresvoil(ires,iyr,itech) &
                  +temporesv(iyr-1)
                 tempgresv(iyr) = aresvgas(ires,iyr,itech) &
                  +tempgresv(iyr-1)
              END if
              tcumresv = temporesv(iyr)              !total booked oil reserves for life of project   !mc change ver 7
              tcumresv2= tempgresv(iyr)              !total booked gas reserves for life of project   !mc change ver 7
           end do

           do iyr = 1,40                                                                              !mc change ver 7
             tempiresv(iyr) = tcumresv - temporesv(iyr)  !calculate the inferred oil reserves         !mc change ver 7
             tempiresvg(iyr) = tcumresv2 - tempgresv(iyr)  !calculate the inferred gas reserves       !mc change ver 7
!             print*, iyr,tcumresv2,tempiresvg(iyr), '1st step'
           end do                                                                                     !mc change ver 7
!           pause

           do iyr = 1,40!24!max_yr-1                                                                  !mc change ver 7
            do myr = 1,iyr
              temporesv(iyr) = temporesv(iyr) - oilprod(myr)
              tempgresv(iyr) = tempgresv(iyr) - gasprod(myr)
            end do

           end do

           do iyr = 1,40!24!max_yr-1                                                                  !mc change ver 7
            aresvoil(ires,iyr,itech) = temporesv(iyr)
            airsvoil(ires,iyr,itech) = tempiresv(iyr)                                                 !mc change ver 7
            aresvgas(ires,iyr,itech) = tempgresv(iyr)
            airsvgas(ires,iyr,itech) = tempiresvg(iyr)                                                !mc change ver 7
            IF(aresvoil(ires,iyr,itech).le.0.0) &
               aresvoil(ires,iyr,itech)=0.0
            IF(aresvgas(ires,iyr,itech).le.0.0) &
               aresvgas(ires,iyr,itech)=0.0
            if(airsvoil(ires,iyr,itech).le.0.0) airsvoil(ires,iyr,itech)=0.0                          !mc change ver 7
            if(airsvgas(ires,iyr,itech).le.0.0) airsvgas(ires,iyr,itech)=0.0                          !mc change ver 7
           end do

        ELSEIF(itech.eq.1) then
           DO IYR = 1,nyr-ishift
!              KYR = 0
!              LYR = IYR + nyr - 1
!              IF(LYR.GE.40)LYR=40
!              DO IYR1 = IYR,LYR
!                 KYR = KYR+1
!              IF(procde.ge.11.and.procde.le.15) then                         ! MC CHANGE 6.23.09
!                do myr = iyr+1,nyr-ishift
!                 aresvoil(ires,iyr,itech) = aresvoil(ires,iyr,itech) + &
!                   oprod(myr)
!                 aresvgas(ires,iyr,itech) = aresvgas(ires,iyr,itech) + &
!                   gprod(myr)
!                end do
!              END if

                 OILPROD(IYR+ishift) = OPROD(IYR)                    ! OIL PRODUCTION
                 GASPROD(IYR+ishift) = GPROD(IYR)                    ! GAS PRODUCTION
                 NGLPROD(IYR+ishift) = NPROD(IYR)                    ! GAS PRODUCTION
                 WATPROD(IYR+ishift) = WPROD(IYR)                    ! WATER PRODUCTION
                IF(procde.le.16) TOTINJ(IYR+ishift)  = OINJ(IYR)     ! INJECTANT
                 WATINJ(IYR+ishift)  = WINJ(IYR)                     ! WATER INJECTED
                IF(procde.le.16) TORECY(IYR+ishift)  = ORECY(IYR)    ! RECYCLED VOLUME
! SPECIAL CASE FOR UNCONVENTIONAL
                 IF (IYR.LE.1) THEN
                    SUMP(IYR+ishift) = PATN(IYR)
                 ELSE
                    SUMP(IYR) = SUMP(IYR-1) + PATN(IYR)
                 END IF

!              ENDDO
           ENDDO
        END IF
!
! CALCULATE TOTAL PRODUCTION AND CUMULATIVE PRODUCTION
!
        TOTPROD = 0.0
        DO IB=1,nyr
           TOTPROD = TOTPROD +  OILPROD(IB) + GASPROD(IB)/5.6
           IF(IB.GT.1)THEN
             XCUMPROD(IB)=XCUMPROD(IB-1) + OILPROD(IB) + GASPROD(IB)/5.6
           ELSE
             XCUMPROD(IB) = OILPROD(IB) + GASPROD(IB)/5.6
           ENDIF
        ENDDO

! convert project level costs to apply to well level costs to apply and integrate with the unit costs !mc change ver 11
        if (.not.once) then
          cstp_drl_fac0(itech)     =    cstp_drl_fac(itech)
          cstp_stm_fac0(itech)     =    cstp_stm_fac(itech)
          cstp_comp_fac0(itech)    =    cstp_comp_fac(itech)
          cstp_fac_fac0(itech)     =    cstp_fac_fac(itech)
          cstp_secconv_fac0(itech) =    cstp_secconv_fac(itech)
          cstp_injconv_fac0(itech) =    cstp_injconv_fac(itech)
          cstp_facupg_fac0(itech)  =    cstp_facupg_fac(itech)
          cstp_wrk_fac0(itech)     =    cstp_wrk_fac(itech)
          cstp_foam_fac0(itech)    =    cstp_foam_fac(itech)
          cstp_voam_fac0(itech)    =    cstp_voam_fac(itech)
          cstp_gna_fac0(itech)     =    cstp_gna_fac(itech)
          cstp_annsec_fac0(itech)  =    cstp_annsec_fac(itech)
          cstp_lft_fac0(itech)     =    cstp_lft_fac(itech)
          cstp_secwrk_fac0(itech)  =    cstp_secwrk_fac(itech)
          cstp_injc_fac0(itech)    =    cstp_injc_fac(itech)
          cstp_cmp_fac0(itech)     =    cstp_cmp_fac(itech)
          once = .true.
        endif

        if(totpatn.gt.0.0) cstp_drl_fac(itech)     =    cstp_drl_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_stm_fac(itech)     =    cstp_stm_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_comp_fac(itech)    =    cstp_comp_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_fac_fac(itech)     =    cstp_fac_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_secconv_fac(itech) =    cstp_secconv_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_injconv_fac(itech) =    cstp_injconv_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_facupg_fac(itech)  =    cstp_facupg_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_wrk_fac(itech)     =    cstp_wrk_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_foam_fac(itech)    =    cstp_foam_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_voam_fac(itech)    =    cstp_voam_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_gna_fac(itech)     =    cstp_gna_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_annsec_fac(itech)  =    cstp_annsec_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_lft_fac(itech)     =    cstp_lft_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_secwrk_fac(itech)  =    cstp_secwrk_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_injc_fac(itech)    =    cstp_injc_fac0(itech)/totpatn
   !mc change ver 11
        if(totpatn.gt.0.0) cstp_cmp_fac(itech)     =    cstp_cmp_fac0(itech)/totpatn
   !mc change ver 11

        res_chr_fac(itech) = res_chr_chg(itech)*totprod/1000.     !mc change ver 11
                                                            !reservoir characterization cost
                                                            !this cost will be applied in the cashflow.

!  Apply the technology levers to the costs.  Two types of levers are applied:    !mc change ver 11
!     2) The cost - project level prorated to the well - to apply the technology  !mc change ver 11

         if (procde.eq.10.or.procde.eq.16) drctech = togtechon(1,1)*techadj_on(1,1)
         if (procde.eq.17) drctech = togtechon(1,2)*techadj_on(1,1)
         if (procde.ge.3.and.procde.le.9) drctech = togtechon(1,3)*techadj_on(1,1)
         if (procde.eq.18.or.procde.eq.22) drctech = togtechon(1,4)*techadj_on(1,1)
         if (procde.eq.20.or.procde.eq.21) drctech = togtechon(1,5)*techadj_on(1,1)
         if (procde.eq.19.or.procde.eq.23) drctech = togtechon(1,6)*techadj_on(1,1)
         drcadj = 1.0
         if (curiyr.ge.techyr) drcadj = (1.0+drctech)**(curiyr-techyr+1)
         DWC_W = DWC_W*drcadj
         DRY_W = DRY_W*drcadj

         if (procde.eq.10.or.procde.eq.16) leotech = togtechon(2,1)*techadj_on(1,1)
         if (procde.eq.17) leotech = togtechon(2,2)*techadj_on(1,1)
         if (procde.ge.3.and.procde.le.9) leotech = togtechon(2,3)*techadj_on(1,1)
         if (procde.eq.18.or.procde.eq.22) leotech = togtechon(2,4)*techadj_on(1,1)
         if (procde.eq.20.or.procde.eq.21) leotech = togtechon(2,5)*techadj_on(1,1)
         if (procde.eq.19.or.procde.eq.23) leotech = togtechon(2,6)*techadj_on(1,1)
         leoadj = 1.0
         if (curiyr.ge.techyr) leoadj = (1.0+drctech)**(curiyr-techyr+1)
         FOAMG_W = FOAMG_W*leoadj
         

         DWC_W = DWC_W + cstp_drl_fac(itech)                                      !mc change ver 11
         DWC_W = DWC_W + cstp_comp_fac(itech)                                     !mc change ver 11
!        if ((procde.eq.17.or.procde.eq.21.or.procde.eq.22).and.itech.eq.1)  &
!          write(6,*) 'dh5cstg', curiyr+1989, aresid(ires), aplay_cde(ires), dwc_w

         fwc_w = fwc_w + cstp_fac_fac(itech)                                      !mc change ver 11

!         dry_W = dry_W + cstp_drl_fac(itech)                                     !mc change ver 11
!         dry_W = dry_W + cstp_comp_fac(itech)                                    !mc change ver 11

         STIM_W = STIM_W + cstp_stm_fac(itech)                                    !mc change ver 11
         COMP_W = COMP_W + cstp_cmp_fac(itech)                                    !mc change ver 11

!
! FIND THE REMAINING RESERVES
!
        DO IH=1,nyr
           REMRES(IH) = aresvoil(ires,ih,itech)
           gremres(ih) = aresvgas(ires,ih,itech)
           iremres(ih) = airsvoil(ires,ih,itech)                                      !mc change ver 7
           igremres(ih) = airsvgas(ires,ih,itech)                                     !mc change ver 7
!           print*, ih,igremres(ih),'econ_gcst2'
        ENDDO
!        pause

!   Step 6: Determine annual capital costs
!      A. Drilling - both developmental and dry wells (Step 2: A-C)
   

!   Step 7: Determine annual capital costs
!      A. Fixed operating costs (Step 3: A-B)
   
!      B. Variable operating costs (Step 3: C-D)

!
! DEFINE NEW PRODUCERS, NEW INJECTORS, CONVERSIONS PRODUCER TO INJECTOR,
! INJECTOR TO PRODUCER
!
          IF(PROCDE.EQ.8)THEN
           WPP1= 2.0 +2.0*REGDRYKD(iregion)!*DRY_FAC(ITECH)                             !4.16.09 REGDRY -> REGDRYKD
           WPP2= 0.0
           WPP3= 2.0
           WPP4= 0.0
          ELSEIF(PROCDE.EQ.0.OR.PROCDE.EQ.10.or.procde.eq.17)THEN  !TEMPORARY - ADD TO INPUT FILE
           WPP1= 1.0
           WPP2= 0.0
           WPP3= 0.0
           WPP4= 0.0
          ELSEIF(procde.eq.3) then               !drawn from the CO2 model
           WPP3 = 0.7
           WPP4 = 0.4
           IF(vdp.gt.0.7) WPP3 = wpp3*0.5
           IF(vdp.gt.0.7) WPP4 = WPP4 * 0.75
          ENDIF

          XPP1 = WPP1
          XPP2 = WPP2
          XPP3 = WPP3
          XPP4 = WPP4

!
!***********************************************************************
!
!     FOR GAS EVALUATE ALL GAS PROCESSES IN THIS ONE BLOCK
!
!***********************************************************************
!
          DO IYR=1,NYR
!            DRILING COST
            IF(PROCDE.EQ.16.or.procde.eq.18.or.procde.eq.19.or.               & !4.15.09
             procde.eq.20) then                                              !4.15.09  Use the exploration dryhole
              IF(iyr.eq.1) then                                              !4.15.09  for the first well drilled.
               IF(patn(iyr).gt.0) then
               drl_cst2(iyr) = drl_cst2(iyr) + (dwc_w+dry_w*                  & !4.15.09
                 regdryUe(iregion))*1.0 *xpp1                                   !4.15.09  Use the development dryhole
                                                                             !4.16.09  REGDRYE - REGDRYUE
!          PRINT*, aresid(ires),dwc_w+dry_w*regdryUe(iregion),drl_cst2(iyr)
!     &              ,patn(iyr),iregion
               drl_cst2(iyr) = drl_cst2(iyr) + (dwc_w+dry_w*                  & !4.15.09  for the rest of the wells.
                 regdryUD(iregion))*(patn(iyr)-1)*xpp1                          !4.15.09
                                                                             !4.16.09  REGDRYG -> REGDRYUD
!          PRINT*, aresid(ires),dwc_w+dry_w*regdryUD(iregion),drl_cst2(iyr)
!     &              ,patn(iyr),iregion
!          pause
               END if
              else                                                           !4.15.09
               drl_cst2(iyr) = drl_cst2(iyr) + (dwc_w+dry_w*                  & !4.15.09
                 regdryUD(iregion))*patn(iyr)*xpp1                              !4.15.09
                                                                             !4.16.09  REGDRYG - REGDRYUD
              END if                                                         !4.15.09
            ELSEIF(procde.eq.21.or.procde.eq.22.or.procde.eq.23) then        !4.15.09
               drl_cst2(iyr) = drl_cst2(iyr) + (dwc_w+dry_w*                  & !4.15.09
                 regdryUD(iregion))*patn(iyr)*xpp1                              !4.15.09
            else
             DRL_CST2(IYR) = DRL_CST2(IYR) + (DWC_W +dry_w *                            & !for non-exploration
                             regdryKD(iregion)) * (PATDEV(IRES,IYR,ITECH)) &
                             * XPP1              !4.16.09  REGDRYG - REGDRYKD  !4.27.09 MC CHANGE PATN->PATDEV
            END if                                                           !4.15.09

            jyr = curiyr+iyr-1
            if (jyr.gt.lastyr) jyr = lastyr

!          PRINT*, aresid(ires),dwc_w+dry_w*regdrYKD(iregion),drl_cst2(iyr)
!     &              ,patn(iyr),iregion,iyr

!            FACILITIES COST
           IF(PROCDE.GE.11.AND.PROCDE.LE.15) THEN                            !MC CHANGE 4.27.09
             FACCOST(IYR) = FACCOST(IYR) + FWC_W * &
              PATDEV(IRES,IYR,ITECH) * XPP1
           ELSE                                                              !MC CHANGE 4.27.09
             FACCOST(IYR) = FACCOST(IYR) + FWC_W * PATN(IYR) * XPP1
           END IF                                                            !MC CHANGE 4.27.09
             faccost(iyr) = faccost(iyr) + cst_foam_fac(itech) + &
                cst_gna_fac(itech)

!            VARIABLE OPERATING COST
             OAM(IYR) = OAM(IYR) &
              + OILPROD(IYR)* OIL_OAM1 * OAM_M(IYR) &
              + GASPROD(IYR)* GAS_OAM1 * OAM_M(IYR) &
              + WATPROD(IYR)* WAT_OAM1 * OAM_M(IYR)

!            STIMULATION COST APPLY 20% PER YEAR (OF ACTIVE PATTERNS)
             STIM(IYR) = STIM(IYR) + 0.2 * STIM_W * XPATN(IYR) * XPP1
!             STIM(IYR) = STIM(IYR) + STIM_W * PATN(IYR)

!            FIXED ANNUAL OPERATING COST ( ADD DAILY RATE COMPONENT )
             FOAMG_W = FOAMG_W + cst_foam_fac(itech)

             IF(PROCDE.EQ.0.OR.PROCDE.EQ.11.OR.PROCDE.EQ.12.OR. &
                PROCDE.EQ.13.OR.PROCDE.EQ.14.OR.PROCDE.EQ.15) THEN
             AOAM(IYR) = AOAM(IYR) + XPATN(IYR)*FOAMG_W*OAM_M(IYR)
             ELSE
             AOAM(IYR) = AOAM(IYR) + XPATN(IYR)*FOAMG_W*OAM_M(IYR)* XPP1
             ENDIF

!            COMPRESSION COST INSTALL WITH NEW PATTERNS
             COMP(IYR) = COMP(IYR) + COMP_W * PATN(IYR) * XPP1

             OAM_COMP(IYR) = OAM_COMP(IYR) &
                 + GASPROD(IYR) * COMP_OAM * OAM_M(IYR)

!            COMPRESSION O & M COST
             OAM(IYR) = OAM(IYR) + OAM_COMP(IYR)

!            GAS PROCESSING AND TREATING COST
             PROC_OAM(IYR) = PROC_OAM(IYR) + PROC_CST * GASPROD(IYR)

!             print*, iyr,oam(iyr),oam_comp(iyr),oam_m(iyr),
!     &        gasprod(iyr),gas_oam1
!             pause
             
!            EXISTING GAS PROJECTS NO NEW DRILLING, COMPRESSION, FACILITIES
             IF(PROCDE.EQ.0.OR.PROCDE.EQ.11.OR.PROCDE.EQ.13.OR. &
                PROCDE.EQ.12.OR. &
                PROCDE.EQ.14.or.procde.eq.15) THEN
                DRL_CST2(IYR) = 0.0                         !MC CHANGE 4.27.09  !mc change 5.4.09 - included in stimulation cost
                COMP(IYR)     = 0.0
                FACCOST(IYR)  = 0.0                         !MC CHANGE 4.27.09  !mc change 5.4.09 - included in stimulation cost
             ENDIF
          END DO
!          pause
        IF (opt_Dbg2) THEN
           WRITE (OGBUG1,*) 'NUMBER OF PRODUCERS PER PATTERN    :',WPP1
           WRITE (OGBUG1,*) 'NUMBER OF INJECTORS PER PATTERN    :',WPP2
           WRITE (OGBUG1,*) 'NUMBER OF CONVERSIONS PER PATTERN  :',WPP3
           WRITE (OGBUG1,*) 'NUMBER OF PRIMARY TO SECONDARY     :',WPP4
           WRITE (OGBUG1,*)
           WRITE (OGBUG1,*)
        END IF

        end subroutine

!********************************************************************************************
!********************************************************************************************
!********************************************************************************************
!********************************************************************************************

        SUBROUTINE MAXRATE_MC(X,W,Y,Z)
!
!     READS ARRAY X / W FOR ELEMENTS Y AND RETURNS Z AS THE MAXRATE
!     USED TO FIND THE PEAK DAILY RATE FOR A SERIES OF PATTERNS
!
        INCLUDE 'OGSMPARM'
        REAL X(MAX_YR)   ! production
        REAL W(MAX_YR)   ! patterns
        REAL Z,MAXV,MINV
        REAL CW
        INTEGER Y
        CW = 0.0
        DO IYR = 1,Y
          CW = CW + W(IYR)
          IF(IYR.EQ.1)THEN
            if (w(1).lt.1.0) then
               MAXV = 0.0
               MINV = 0.0
            else
               MAXV = X(1)/CW
               MINV = X(1)/CW
            end if
            GOTO 100
          ENDIF
          IF (CW.GE.1.0) THEN
            IF((X(IYR)/CW).GT.MAXV)MAXV=X(IYR)/CW
            IF((X(IYR)/CW).LT.MINV)MINV=X(IYR)/CW
          END IF
100     CONTINUE
        END DO
        Z = MAXV
        RETURN
        END SUBROUTINE


!***************************************************************
!from PATCOST_O.FOR
!     Last change:  MC   16 Apr 2009    1:25 pm

        SUBROUTINE PATCOST_Oil(ITECH)
!-------------------------------------------------------------------------
! THIS SUBROUTINE CALCULATES THE PROJECT COSTS AND ANUALIZES THEM FOR USE
! IN CASHFLOW. IT ALSO CALCULATES THE ANNUAL PRODUCTION STREAM BASED
! ON PATTERN DEVELOPMENT SCHEDULE
!-------------------------------------------------------------------------

!********************************************************************
!
!     THIS ROUTINE IS BASICALLY A DUPLICATION OF THE PRECOST SUBROUTINE
!     IT GIVES US THE PATTERN LIFE FOR USE IN PRECOST CALCULATIONS OF
!     PRODUCTION AND OPERATING COSTS.
!
!********************************************************************

        IMPLICIT NONE

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
        INCLUDE 'ogsml48'
        include 'macout'


!       LOCAL VARIABLES
        INTEGER IYR, LYR, KYR, IYR1, JYR, IRMAX,nyr
        REAL RMAX, STMA, STMP
        REAL DUM(MAX_YR),TOTGASPROD(MAX_YR)
        INTEGER IDIM
        INTEGER ITECH, IX, IB, IH, IC, IG, I

        REAL RMAXWAT, RMAXOIL
        REAL oil_oam1,gas_oam1,wat_oam1,inj_oam1
!
        INTEGER IGEN(MAX_YR)              ! NO OF GENERATORS REQUIRED FOR STEA
        COMMON/NEWCASH/IGEN

!  temp variables
        NYR = max_yr-1
        RMAXWAT = 0.0
        RMAXOIL = 0.0
        TOTPROD = 0.0

         CALL  INITPRE
!
        oil_oam1 = oil_oam(procde+1)
        gas_oam1 = gas_oam(procde+1)
        wat_oam1 = wat_oam(procde+1)
        inj_oam1 = inj_oam(procde+1)

! CALCULATE PRODUCTION SCHEDULE FOR THE PROJECT BASED
! ON PATTERN INITIALIZATION
!
! SET ALL PATTERNS FOR ALL YEARS TO ZERO INITIALIZE 1 PATTERN----
! IN YEAR ONE TO DO PATTERN ECONOMICS, AND PATTERN LIFE CALCULATIONS
!
        DO IYR=1,max_yr-1
         PATN(IYR) = 0.0
        END DO
        PATN(1) = 1.0

        LYR = 0
        IYR = 0
        KYR = 0
          DO IX = 1,max_yr-1
            IF(PATN(IX).EQ.0.0)THEN
              JYR = IX
              GO TO 15
            ENDIF
          ENDDO
15      CONTINUE
!
! GET PATTERN PRODUCTION FOR THE PROJECT
!
        DO IYR = 1,JYR
          KYR = 0
          LYR = IYR + max_yr-1 - 1
          IF(LYR.GE.MAX_YR)LYR=MAX_YR
          DO IYR1 = IYR,LYR
            KYR = KYR+1
            OILPROD(IYR1)=OILPROD(IYR1) + OPROD(KYR)*PATN(IYR)    ! OIL PRODUCTION
            GASPROD(IYR1)=GASPROD(IYR1) + GPROD(KYR)*PATN(IYR)    ! GAS PRODUCTION
            WATPROD(IYR1)=WATPROD(IYR1) + WPROD(KYR)*PATN(IYR)    ! WATER PRODUCTION
            TOTINJ(IYR1) =TOTINJ(IYR1)  + OINJ(KYR)*PATN(IYR)     ! INJECTANT
            WATINJ(IYR1) =WATINJ(IYR1)  + WINJ(KYR)*PATN(IYR)     ! WATER INJECTED
            TORECY(IYR1) =TORECY(IYR1)  + ORECY(KYR)*PATN(IYR)    ! RECYCLED VOLUME
            SUMP(IYR1)   =SUMP(IYR1)    + PATN(IYR)

          ENDDO
        ENDDO

! TOTAL PRODUCTION, OIL PRODUCTION + MBOE(OIL EQUIVALENT OF GAS)
        TOTPROD = 0.0
        DO IB=1,max_yr-1
           TOTPROD = TOTPROD +  OILPROD(IB) + GASPROD(IB)/5.6
          IF(IB.GT.1)THEN
           XCUMPROD(IB)=XCUMPROD(IB-1) + OILPROD(IB) + GASPROD(IB)/5.6
          ELSE
           XCUMPROD(IB) = OILPROD(IB) + GASPROD(IB)/5.6
          ENDIF
        ENDDO

        DO IH=1,max_yr-1
           REMRES(IH) = TOTPROD - XCUMPROD(IH)
        ENDDO
!
! DEFINE NEW PRODUCERS, NEW INJECTORS, CONVERSIONS PRODUCER TO INJECTOR,
! INJECTOR TO PRODUCER
!
          IF(PROCDE.EQ.8)THEN
           WPP1= 2.0 +2.0*REGDRYKD(iregion)!*DRY_FAC(ITECH)                       !4.16.09 REGDRY -> REGDRYKD
           WPP2= 0.0
           WPP3= 2.0
           WPP4= 0.0

          ELSEIF(PROCDE.EQ.0.OR.PROCDE.EQ.10.or.procde.eq.17)THEN  !TEMPORARY - ADD TO INPUT FILE
           WPP1= 1.0
           WPP2= 0.0
           WPP3= 0.0
           WPP4= 0.0
          ELSEIF(procde.eq.3) then              !drawn from the CO2 model     !4.16.09
           WPP3 = 0.7
           WPP4 = 0.4
           IF(vdp.gt.0.7) WPP3 = wpp3*0.5
           IF(vdp.gt.0.7) WPP4 = WPP4 * 0.75
          ENDIF

          XPP1 = WPP1
          XPP2 = WPP2
          XPP3 = WPP3
          XPP4 = WPP4
!
! SIMILARLY TO CALCULATE COST SCHEDULE FOR THE PROJECT
!
        DO IYR = 1,NYR
        IF(PROCDE.EQ.10.OR.PROCDE.EQ.17) THEN                                  !4.16.09
          IF(IYR.EQ.1) THEN                                                    !4.16.09
        DRL_CST2(IYR) = DRL_CST2(IYR) + (DWC_W + dry_w*regdryUE(iregion))*         & !4.16.09
                        PATN(IYR)*(XPP1 + XPP2)                                !4.16.09
        DRY_CST(IYR) = DRY_CST(IYR) + DRY_W * PATN(IYR)*REGDRYUE(iregion)
          ELSE                                                                 !4.16.09
        DRL_CST2(IYR) = DRL_CST2(IYR) + (DWC_W + dry_w*regdryUD(iregion))*         & !4.16.09
                        PATN(IYR)*(XPP1 + XPP2)                                !4.16.09
        DRY_CST(IYR) = DRY_CST(IYR) + DRY_W * PATN(IYR)*REGDRYUD(iregion)
          END IF                                                               !4.16.09
        ELSE                                                                   !4.16.09
        DRL_CST2(IYR) = DRL_CST2(IYR) + (DWC_W + dry_w*regdryKD(iregion))*         & !4.16.09 REGDRY -> REGDRYKD
                        PATN(IYR)*(XPP1 + XPP2)
        DRY_CST(IYR) = DRY_CST(IYR) + DRY_W * PATN(IYR)*REGDRYKD(iregion)
        END IF                                                                 !4.16.09

         IF(PROCDE.EQ.0.or.procde.eq.10. &
          .OR.PROCDE.EQ.17)THEN
          FACCOST(IYR) = FACCOST(IYR) &
                       +    NPR_W*PATN(IYR)* XPP1 &
!                      +    PSW_W*PATN(IYR)* XPP4
                       +    PSI_W*PATN(IYR)* XPP3
!                      +    SFU_W*PATN(IYR)*(XPP1+XPP2)
         ELSE
          FACCOST(IYR) = FACCOST(IYR) &
!                      +    NPR_W*PATN(IYR)* XPP1
                       +    PSW_W*PATN(IYR)* XPP4 &
                       +    PSI_W*PATN(IYR)* XPP3 &
                       +    fac_w*patn(iyr)*(xpp3+xpp4)
!                      +    SFU_W*PATN(IYR)*(XPP1+XPP2)
         ENDIF


! INJECTANT COST ADDED TO OAM IN CASHFLOW
            INJ(IYR) = INJ(IYR) + INJ_oam1*WATINJ(IYR)


!
! ANNUAL OAM COST(PLEASE CHECK THIS VARIABLE WITH DON)
! VERY CRITICAL HARDWIRES IN TORIS MODELS
         IF(PROCDE.EQ.3)THEN
           AOAM(IYR) = AOAM(IYR) + (Omo_W * SUMP(IYR) &
            +  OPSEC_W*SUMP(IYR))
         ELSE
           AOAM(IYR) = AOAM(IYR) + (Omo_W * SUMP(IYR) &
            +  OPSEC_W*SUMP(IYR))
         ENDIF


! OPERATING COSTS
           OAM(IYR) = OAM(IYR) &
            + OILPROD(IYR)* OIL_OAM1*OAM_M(IYR) &
            + GASPROD(IYR)* GAS_OAM1*OAM_M(IYR) &
            + WATPROD(IYR)* WAT_OAM1*OAM_M(IYR)
        ENDDO    ! COST  CALCULATION

! FOR MISCIBLE CO2 MODEL  --->>> PROCESS CODE 17 (ONLY CO2 ADVANCED)
       IF(PROCDE.EQ.3) THEN
!      DO IYR = 1,max_yr-1
!          OAM(IYR) = 0.0
!          OAM(IYR) = OAM(IYR) &
!           + WATPROD(IYR)* WAT_OAM1*OAM_M(IYR)
!      END DO
!
! LETS CALCULATE THE MAXIMUM CO2 RECYCLING RATE WHICH WILL
! CALCULATE THE RECYCLE PLANT CAPACITY
!
! CALCULATE MAXIMUM PRODUCTION/INJECTION RATE
          RMAX = 0.0
          CALL MAXRATE(TORECY,max_yr-1,RMAX)
! RECYCLE PLANT COST
         IF(RMAX/365.0 <= 30.0) THEN
           co2_f = 1200.0*(RMAX/365.0)
         ELSE
           co2_f = 36000.0 + 750.0*(RMAX/365.0-30.0)
         ENDIF
         co2_f = co2_f*mc_jpgdp(nom_year-baseyr+1)/mc_jpgdp(2016-baseyr+1)  ! convert from $2016 to cost year dollars
         FACCOST(1) = FACCOST(1) + co2_f
!      if(RMAX/365.0.ge.60.0) write(6,*) 'dh5outPO', itimeyr, procde, rmax/365.0, co2_f, (co2rk*(RMAX/365.0*.5)**co2rb)*cpi_2003*1000.
!      if(RMAX/365.0.lt.60.0) write(6,*) 'dh5outPO', itimeyr, procde, rmax/365.0, co2_f, (co2rk*(RMAX/365.0)**co2rb)*cpi_2003*1000.

! NO OAM_M MULTIPLIER TO INJECTED COST AS OILPRICE ADJUSTMENT IS ALREADY
! IN CO2 PURCHASE PRICE
          CO2OAM=co2om20
          IF(RMAX/(365.0).LT.20.0)CO2OAM = co2om_20
          CO2OAM2 = CO2OAM
          DO IYR= 1, NYR
          IF(TOTINJ(IYR)-TORECY(IYR).GT.0) &
              INJ(IYR) = INJ(IYR) + &
                             (TOTINJ(IYR)-TORECY(IYR))*CO2COST
!          PRINT*,iyr,procde, co2cost,totinj(iyr),torecy(iyr),inj(iyr)
!          pause

           OAM(IYR) = OAM(IYR) + OAM_M(IYR)*TOTINJ(IYR)*CO2OAM &
                       + PSW_W * 0.25
           FOAM(IYR) = FOAM(IYR) + TOTINJ(IYR)*0.40*FCO2
          ENDDO

        ENDIF   ! CO2 MISCIBLE COSTS

! FOR PROFILE MODEL
       IF(PROCDE.EQ.7) THEN
!
! LETS CALCULATE THE MAXIMUM POLYMER INJECTION RATE AND
! ALSO THE MAXIMUM WATER INJECTION RATE FOR INJECTION PLANT CAPACITY
! FOLLOWING COST ADDED FOR POLYMER OR INFILL POLYMER MODEL
! COST OF INJECTANT ( $1.50/LB)
! CURRENTLY IN MODEL POUNDS OF POLYMER IN FUTURE IT WILL BE 1000LBS.
!
          DO IYR= 1, NYR
           INJ(IYR) = INJ(IYR) +OAM_M(IYR)*TOTINJ(IYR)*POLYCOST/1000.0
          ENDDO
!
! COST OF WATER HANDLING  PLANT
          CALL  MAXRATE(WATINJ,max_yr-1,RMAX)
          FACCOST(1) = FACCOST(1) + 40.0*(RMAX/365.0)

        ENDIF   ! INFILL PROFILE/PROFILE MODEL COSTS

! FOR POLYER MODEL
       IF(PROCDE.EQ.5) THEN
!
! LETS CALCULATE THE MAXIMUM POLYMER INJECTION RATE AND
! ALSO THE MAXIMUM WATER INJECTION RATE FOR INJECTION PLANT CAPACITY
! FOLLOWING COST ADDED FOR POLYMER OR INFILL POLYMER MODEL
! COST OF INJECTANT ( $1.50/LB)
! CURRENTLY IN MODEL POUNDS OF POLYMER IN FUTURE IT WILL BE 1000LBS.
!
          DO IYR= 1, NYR
           INJ(IYR) = INJ(IYR) + TOTINJ(IYR)*POLYCOST/1000.0
          ENDDO
!
! COST OF CHEMICAL PLANT
          CALL  MAXRATE(TOTINJ,MAX_yr-1,RMAX)
          RMAX=RMAX/1000.0
          FACCOST(1) = FACCOST(1) + chm_f
!
! COST OF WATER HANDLING  PLANT
          CALL  MAXRATE(WATINJ,max_yr-1,RMAX)
          FACCOST(1) = FACCOST(1) + 40.0*(RMAX/365)

        ENDIF   ! INFILL POLYMER/POLYMER MODEL COSTS

!-----ADDED 9/04/01 ASH----------------------------------------------
! FOR WATER FLOOD MODELS 22-24
       IF(PROCDE.EQ.2) THEN
!
! LETS CALCULATE THE MAXIMUM POLYMER INJECTION RATE AND
! ALSO THE MAXIMUM WATER INJECTION RATE FOR INJECTION PLANT CAPACITY
!
          DO IYR= 1, NYR
!           INJ(IYR) = INJ(IYR) + FPLY*TOTINJ(IYR)*POLYCOST/1000.0
           OAM(IYR) = OAM(IYR) + XPATN(IYR)*0.25*PSI_W
          ENDDO
!
! COST OF CHEMICAL PLANT
!          CALL  MAXRATE(TOTINJ,40,RMAX)
!          RMAX=RMAX/1000.0
!          FACCOST(STYR) = FACCOST(STYR) + 0.1*1000.0*(RMAX/365)**0.6
!
! COST OF WATER HANDLING  PLANT
          CALL  MAXRATE(WATINJ,max_yr-1,RMAX)
!          FACCOST(1) = FACCOST(1) + 40.0*(RMAX/365)

        ENDIF   ! WATERFLOOD MODEL COSTS
!--------------------------------------------------------------------

! FOR DECLINE COURVE MODEL

        IF(PROCDE.EQ.0) THEN
            DO IYR = 1,NYR
              DRL_CST2(IYR) = 0.0
              AOAM(IYR) = 0.0
              OAM(IYR) = 0.0
              FACCOST(IYR) = 0.0
              INJ(IYR) = 0.0
            END DO
          STMA = 0.0
          STMP = 0.0
          IC = 0
! READ RESID
!          CALL ILOOK(STMRES,ISTM,RESID,IC)
!            IF(IC.GT.0)THEN
!              STMA = STMFRACA(IC)
!              STMP = STMFRACP(IC)
!            ENDIF

          DO IYR = 1,NYR
!         IF(OILPROD(IYR)/TOTPAT/0.365.GT.15.0)THEN

            OAM(IYR) = OAM(IYR) + ((STMP*3.475*OILPROD(IYR)) &
                       + ((1.-STMA)*0.5*OILPROD(IYR)) &
                       + (STMP*3.7*OILPROD(IYR)))*OAM_M(IYR)

            AOAM(IYR) = AOAM(IYR) + (19.8000 * STMA &
                        + OPSEC_W* (1-STMA))*OAM_M(IYR)*SUMP(IYR)
!         ELSE
!            WATCUT = WATPROD(IYR)/(WATPROD(IYR)+OILPROD(IYR))
!            OPY = OILPROD(IYR)*1000.0/TOTPAT
!            CALL STRIPCST(WATCUT,DEPTH,OPY,SCOST)
!            OAM(IYR) = OAM(IYR)+ SCOST/1000.0* TOTPAT
!            AOAM(IYR) = 0.0
!         ENDIF

          END DO

!           PRINT* , OAM(1),AOAM(1)
!           PAUSE
        ENDIF ! DECLINE CURVE MODEL

!/////////////////////////////////////////////////////////////////////
!STEAM MODEL....
! TOTINJ = STEAM INJECTED
! WATINJ = FEUL REQUIRED TO RUN GENERATORS...
!
        IF (PROCDE.EQ.4) THEN
          DO IYR = 1,NYR
               FACCOST(IYR) = 0.0
               INJ(IYR) = 0.0
               OAM(IYR) = 0.0
          END DO
!
!LETS CALCULATE THE GENARATORS REQUIRED IN THE GIVEN STEAM PROJECT
!
          DO IYR = 1,NYR
                IGEN(IYR) = 0.0
                IGEN(IYR) = NINT(TOTINJ(IYR)/365.0/3.00)
          ENDDO
! CAPITAL EXPENDITURES
!
          RMAX = 0.0
          CALL  MAXRATE(WATPROD,MAX_yr-1,RMAX)
          RMAXWAT=RMAX/365.0
          RMAX = 0.0
          CALL  MAXRATE(OILPROD,max_yr-1,RMAX)
          RMAXOIL=RMAX/365.0


!          READ(RESID(9:12),112)IRESG1
!112        FORMAT(I4)
!           DO I=1,NRESHM
!             IF(IRESG1.EQ.ID2(I))THEN
!              OOIP = XOOIP(I)/1000.0
!               GO TO 12
!             ENDIF
!           END DO
!12        CONTINUE
!           FACCOST(1)=FACCOST(1)+(OOIP*0.1*2.0* APAT/TOTPAT)
!     &                      + pwp_f     ! PRODUCED WATER RECYLING COST
!     &                      + stmm_f    ! STEAM MANIFOLDS/PIPELINES ETC


          DO IYR=1,NYR
             IF(IYR.EQ.1)IG=0
             IF(IYR.GT.1)IG=IGEN(IYR-1)
             FACCOST(IYR) = FACCOST(IYR) &
                            + (IGEN(IYR)-IG)*(650.000)    ! GENERATOR COST
!
!
             OAM(IYR) = OAM(IYR) + ((0.025*WATPROD(IYR)) &
                             + (3.140*OILPROD(IYR)))*OAM_M(IYR)

!     &                       + ((2.0+0.15)*WATINJ(IYR)))*OAM_M(IYR)

           FOAM(IYR) = FOAM(IYR) + TOTINJ(IYR)*0.50
          ENDDO
!
        ENDIF  ! END OF STEAM COST CALCULATION
!////////////////////////////////////////////////////////////////////

!        WRITE (OGBUG1,*)'Patcost ROUTINE - one pattern'
!        WRITE (OGBUG1,*)'PATTERN INITIATION AND OIL PRODUCTION'
!        WRITE (OGBUG1,*)'FOR TECHNOLOGY ',ITECH
!        WRITE (OGBUG1,599)
!599     FORMAT (3X,'YR',6x,'PATTERNS',6x,'ACTIVEPT',6x,' OILPROD',6x,
!     &          ' GASPROD',6x,' WATPROD',6x,' TOTINJ ',6x,' WATINJ ',
!     &          6x,' TORECY ',6x,'SUM_PAT')
!        DO I=1,40
!           WRITE (OGBUG1,600)I,PATN(I),XPATN(I),OILPROD(I),GASPROD(I),
!     &                    WATPROD(I),TOTINJ(I),WATINJ(I),TORECY(I),
!     &                    SUMP(I)


!600        FORMAT (3X,I2,9(6x,F8.3))
!        END DO

!        WRITE (OGBUG1,*)'COST SCHEDULE FOR TECHNOLOGY',ITECH
!        WRITE (OGBUG1,602)
!602     FORMAT (3X,'YR',6x,'    VAR OAM',6x,'    ANN OAM',6x,
!     &    '    FACCOST',6x,      '   INJECANT',6x,'    DRL_CST')
!        DO I=1,40
!           WRITE (OGBUG1,601)I,OAM(I),AOAM(I),FACCOST(I),INJ(I),DRL_CST2(I)
!601        FORMAT (3X,I2,5(6x,F11.2))
!        END DO


        END


!***************************************************************
!from ECON_PATCOSTG.FOR
!     Last change:  MC   17 Apr 2009    9:28 am
       subroutine patcost_gas(itech)

       implicit none

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
        INCLUDE 'ogsml48'

!       LOCAL VARIABLES
!        REAL NEWPROD
!        INTEGER RSTY
        INTEGER IYR, LYR, KYR, IYR1, JYR, IRMAX,nyr
        REAL RMAX, STMA, STMP
        REAL DUM(MAX_YR),TOTGASPROD(MAX_YR)
        INTEGER IDIM
        INTEGER ITECH, IX, IB, IH, IC, IG, I

        REAL RMAXWAT, RMAXOIL
        REAL oil_oam1,gas_oam1,wat_oam1,inj_oam1
!
        INTEGER IGEN(MAX_YR)              ! NO OF GENERATORS REQUIRED FOR STEA
        COMMON/NEWCASH/IGEN

!  temporary variables
        NYR = max_yr-1
        RMAXWAT = 0.0
        RMAXOIL = 0.0
        TOTPROD = 0.0

        CALL  INITPRE

        oil_oam1 = oil_oam(procde+1)
        gas_oam1 = gas_oam(procde+1)
        wat_oam1 = wat_oam(procde+1)
        inj_oam1 = inj_oam(procde+1)
!
! CALCULATE PRODUCTION SCHEDULE FOR THE PROJECT BASED
! ON PATTERN INITIALIZATION
!
! SET ALL PATTERNS FOR ALL YEARS TO ZERO INITIALIZE 1 PATTERN----
! IN YEAR ONE TO DO PATTERN ECONOMICS, AND PATTERN LIFE CALCULATIONS
!
        DO IYR=1,max_yr-1
         PATN(IYR) = 0.0
        END DO
        PATN(1) = 1.0

        LYR = 0
        IYR = 0
        KYR = 0
          DO IX = 1,max_yr-1
            IF(PATN(IX).EQ.0.0)THEN
              JYR = IX
              GO TO 15
            ENDIF
          ENDDO
15      CONTINUE
!
! GET PATTERN PRODUCTION FOR THE PROJECT
!
! initialize the production arrays
       do IYR = 1,max_yr
         oilprod(iyr) = 0.0
         gasprod(iyr) = 0.0
         nglprod(iyr) = 0.0
         watprod(iyr) = 0.0
         totinj(iyr) = 0.0
         watinj(iyr) = 0.0
         torecy(iyr) = 0.0
         sump(iyr) = 0.0
       end do

        DO IYR = 1,JYR
          KYR = 0
          LYR = IYR + max_yr-1 - 1
          IF(LYR.GE.MAX_YR)LYR=MAX_YR
          DO IYR1 = IYR,LYR
            KYR = KYR+1
            OILPROD(IYR1)=OILPROD(IYR1) + OPROD(KYR)*PATN(IYR)    ! OIL PRODUCTION
            GASPROD(IYR1)=GASPROD(IYR1) + GPROD(KYR)*PATN(IYR)    ! GAS PRODUCTION
            WATPROD(IYR1)=WATPROD(IYR1) + WPROD(KYR)*PATN(IYR)    ! WATER PRODUCTION
            TOTINJ(IYR1) =TOTINJ(IYR1)  + OINJ(KYR)*PATN(IYR)     ! INJECTANT
            WATINJ(IYR1) =WATINJ(IYR1)  + WINJ(KYR)*PATN(IYR)     ! WATER INJECTED
            TORECY(IYR1) =TORECY(IYR1)  + ORECY(KYR)*PATN(IYR)    ! RECYCLED VOLUME
            SUMP(IYR1)   =SUMP(IYR1)    + PATN(IYR)
         ENDDO
        ENDDO


!             REMRES(IYR1)  = REMRES(IYR1)
!     &       +(NEWPROD-OPROD(IRES,KYR) - GPROD(IRES,KYR)/5.642)*PATN(IYR)
!              NEWPROD = NEWPROD - (OPROD(IRES,KYR) + GPROD(IRES,KYR)/5.642)


! TOTAL PRODUCTION, OIL PRODUCTION + MBOE(OIL EQUIVALENT OF GAS)
        TOTPROD = 0.0
        DO IB=1,max_yr-1
           TOTPROD = TOTPROD +  OILPROD(IB) + GASPROD(IB)/5.6
          IF(IB.GT.1)THEN
           XCUMPROD(IB)=XCUMPROD(IB-1) + OILPROD(IB) + GASPROD(IB)/5.6
          ELSE
           XCUMPROD(IB) = OILPROD(IB) + GASPROD(IB)/5.6
          ENDIF
        ENDDO

        DO IH=1,max_yr-1
           REMRES(IH) = TOTPROD - XCUMPROD(IH)
        ENDDO
!
! DEFINE NEW PRODUCERS, NEW INJECTORS, CONVERSIONS PRODUCER TO INJECTOR,
! INJECTOR TO PRODUCER
!

          IF(PROCDE.EQ.8)THEN
           WPP1= 2.0 +2.0*REGDRYKD(iregion)!*DRY_FAC(ITECH)                          !4.16.09  REGDRY -> REGDRYKD
           WPP2= 0.0
           WPP3= 2.0
           WPP4= 0.0
          ELSEIF(PROCDE.EQ.0.OR.PROCDE.EQ.10.or.procde.eq.17)THEN  !TEMPORARY - ADD TO INPUT FILE
           WPP1= 1.0
           WPP2= 0.0
           WPP3= 0.0
           WPP4= 0.0
          ELSEIF(procde.eq.3) then              !drawn from the CO2 model
           WPP1 = 2.0
           WPP2 = 1.0
           WPP3 = 2.0
           WPP4 = 1.0
          ENDIF

          XPP1 = WPP1
          XPP2 = WPP2
          XPP3 = WPP3
          XPP4 = WPP4



!
!***********************************************************************
!
!     TEMPORARY-- FOR GAS EVALUATE ALL GAS PROCESSES IN THIS ONE BLOCK
!
!***********************************************************************
!
          DO IYR=1,NYR
!            DRILING COST
           IF(PROCDE.EQ.16.OR.PROCDE.EQ.18.OR.PROCDE.EQ.19.OR.        & !4.16.09
             PROCDE.EQ.20) THEN                                      !4.16.09
             IF(IYR.EQ.1) THEN
             DRL_CST2(IYR) = DRL_CST2(IYR) + (DWC_W +dry_w *          & !4.16.09
                             regdryUE(iregion)) * PATN(IYR) * XPP1      !4.16.09
             DRY_CST(IYR) = DRY_CST(IYR) + DRY_W * PATN(IYR)*REGDRYUE(iregion)
             ELSE
             DRL_CST2(IYR) = DRL_CST2(IYR) + (DWC_W +dry_w *          & !4.16.09
                             regdryUD(iregion)) * PATN(IYR) * XPP1      !4.16.09
             DRY_CST(IYR) = DRY_CST(IYR) + DRY_W * PATN(IYR)*REGDRYUD(iregion)
             END IF
            ELSEIF(procde.eq.21.or.procde.eq.22.or.procde.eq.23) then        !4.15.09
               drl_cst2(iyr) = drl_cst2(iyr) + (dwc_w+dry_w*                  & !4.15.09
                 regdryUD(iregion))*patn(iyr)*xpp1                              !4.15.09
               DRY_CST(IYR) = DRY_CST(IYR) + DRY_W * PATN(IYR)*REGDRYUD(iregion)
           ELSE                                                      !4.16.09
             DRL_CST2(IYR) = DRL_CST2(IYR) + (DWC_W +dry_w * &
                             regdryKD(iregion)) * PATN(IYR) * XPP1      !4.16.09 REGDRY -> REGDRYKD
             DRY_CST(IYR) = DRY_CST(IYR) + DRY_W * PATN(IYR)*REGDRYKD(iregion)
           END IF                                                    !4.16.09


!            FACILITIES COST
             FACCOST(IYR) = FACCOST(IYR) + FWC_W * PATN(IYR) * XPP1

!            VARIABLE OPERATING COST
             OAM(IYR) = OAM(IYR) &
              + GASPROD(IYR)* COMP_OAM * OAM_M(IYR) &
              + GASPROD(IYR)* GAS_OAM1 * OAM_M(IYR) &
              + WATPROD(IYR)* WAT_OAM1 * OAM_M(IYR)

!            STIMULATION COST APPLY 20% PER YEAR (OF ACTIVE PATTERNS)
             STIM(IYR) = STIM(IYR) + 0.2 * STIM_W * PATN(IYR) * XPP1

!            FIXED ANNUAL OPERATING COST ( ADD DAILY RATE COMPONENT )
!cccc             IF (XPATN(IYR).GT.0.0) THEN
!cccc                FOAMG_W = FOAMG_1 +
!cccc     &             FOAMG_2 * (1000.0*(GASPROD(IYR)/PATN(IYR)) / 365.0)
!cccc             ELSE
!cccc                FOAMG_W = FOAMG_1
!cccc             END IF
!cccc             FOAMG_W = FOAMG_W / 1000.0

             IF(PROCDE.EQ.0.OR.PROCDE.EQ.11.OR.PROCDE.EQ.13.OR. &
                PROCDE.EQ.12.OR.procde.eq.14.or.PROCDE.EQ.15) THEN
             AOAM(IYR) = AOAM(IYR) + XPATN(IYR)*FOAMG_W*OAM_M(IYR)
             ELSE
             AOAM(IYR) = AOAM(IYR) + XPATN(IYR)*FOAMG_W*OAM_M(IYR)* XPP1
             ENDIF

!            COMPRESSION COST
             COMP(IYR) = COMP(IYR) + COMP_W * PATN(IYR) * XPP1

!            GAS PROCESSING AND TREATING COST
             PROC_OAM(IYR) = PROC_OAM(IYR) + PROC_CST * GASPROD(IYR)

!            EXISTING GAS PROJECTS NO NEW DRILLING, COMPRESSION, FACILITIES
             IF(PROCDE.EQ.0.OR.PROCDE.EQ.11.OR.PROCDE.EQ.13.OR. &
                PROCDE.EQ.12.OR.PROCDE.eq.14.or.PROCDE.EQ.15) THEN
                DRL_CST2(IYR) = 0.0
                COMP(IYR)     = 0.0
                FACCOST(IYR)  = 0.0
                DRY_CST(IYR)  = 0.0
             ENDIF
          END DO


       end subroutine
!***************************************************************
!from ECON_CAPCOST.FOR
!     Last change:  MC   16 Jan 2009    9:19 am
       SUBROUTINE CAPITAL_COST(IRES)
!
!***********************************************************************
!                                                                      *
!     THE FOLLOWING SECTION OF CODE IS USED TO CALCULATE THE PROJECT   *
!     LEVEL COSTS, FACILITIES, CAPITAL ETC.  THESE COULD NOT BE        *
!     DONE FROM WITHIN THE PRECOST ROUTINE.  THESE ARE PROJECT LEVEL   *
!     CALCULATIONS.                                                    *
!                                                                      *
!***********************************************************************
!
!-----THESE INCLUDES ARE FOR THE TIMING/EXPLORATION MODELS
       implicit none

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
       INCLUDE 'ogsmugr'
       INCLUDE 'ogsml48'
        include 'macout'
       
! LOCAL VARIABLES
       REAL rmax,test,rmaxwat,rmaxoil,apat,ooip
       REAL TOTGASPROD(MAX_YR)  !TOTAL GAS PRODUCTION
       INTEGER IRES,iyr,i,irmax,ist,ig
       INTEGER IGEN(MAX_YR)              ! NO OF GENERATORS REQUIRED FOR STEAM
       COMMON/NEWCASH/IGEN

       REAL oil_oam1,gas_oam1,wat_oam1,inj_oam1
       COMMON/NEWcost/oil_oam1,gas_oam1,wat_oam1,inj_oam1


       REAL FEDROYALTY(MAX_YR)
       COMMON/AGG/FEDROYALTY

       IF(procde.ne.4) then
         do iyr = 1,max_yr
           igen(iyr) = 0
         end do
       END if
!
!       CO2COST = 0.0  - co2 cost calculated in unit cost
!
! PROFILE MODEL - -
!
       IF(PROCDE.EQ.7) THEN
!
!      LETS CALCULATE THE MAXIMUM POLYMER INJECTION RATE AND
!      ALSO THE MAXIMUM WATER INJECTION RATE FOR INJECTION PLANT CAPACITY
!      FOLLOWING COST ADDED FOR POLYMER OR INFILL POLYMER MODEL
!C      COST OF INJECTANT ( $1.50/LB)
!      CURRENTLY IN MODEL POUNDS OF POLYMER IN FUTURE IT WILL BE 1000LBS.
!
!      COST OF WATER HANDLING  PLANT
          CALL  MAXRATE(WATINJ,max_yr-1,RMAX)
          FACCOST(1) = FACCOST(1) + pwhp*(RMAX/365.0)
       ENDIF   ! INFILL PROFILE/PROFILE MODEL COSTS
!
!
! POLYER MODEL - -
!
       IF(PROCDE.EQ.5) THEN
         do iyr = 1,max_yr
           igen(iyr) = 0
         end do

!
!        LETS CALCULATE THE MAXIMUM POLYMER INJECTION RATE AND
!        ALSO THE MAXIMUM WATER INJECTION RATE FOR INJECTION PLANT CAPACITY
!        FOLLOWING COST ADDED FOR POLYMER OR INFILL POLYMER MODEL
!        COST OF INJECTANT
!        CURRENTLY IN MODEL POUNDS OF POLYMER IN FUTURE IT WILL BE 1000LBS.
!
!        COST OF CHEMICAL PLANT
          FACCOST(1) = FACCOST(1) + chm_f!0.1*(RMAX/365)**0.6
!
!        COST OF WATER HANDLING  PLANT
          FACCOST(1) = FACCOST(1) + pwp_f!*(RMAX/365)
       ENDIF   ! INFILL POLYMER/POLYMER MODEL COSTS

! MISCIBLE CO2 MODEL - CO2 ADVANCED
!
       IF(PROCDE.EQ.3) THEN
       do iyr = 1,max_yr
          igen(iyr) = 0
       end do
!
!        LETS CALCULATE THE MAXIMUM CO2 RECYCLING RATE WHICH WILL
!        CALCULATE THE RECYCLE PLANT CAPACITY
!        CALCULATE MAXIMUM PRODUCTION/INJECTION RATE
         RMAX = 0.0
         CALL  MAXRATE(TORECY,max_yr-1,RMAX)
!        RECYCLE PLANT COST
         IF(RMAX/365.0 <= 30.0) THEN
           co2_f = 1200.0*(rmax/365.0)
         ELSE
           co2_f = 36000.0 + 750.0*(rmax/365.0-30.0)
         ENDIF
         co2_f = co2_f*mc_jpgdp(nom_year-baseyr+1)/mc_jpgdp(2016-baseyr+1)  ! convert from $2016 to cost year dollars
         FACCOST(1) = FACCOST(1) + co2_f
!      if(RMAX/365.0.ge.60.0) write(6,*) 'dh5outCC', itimeyr, aresid(ires), rmax/365.0, co2_f, (co2rk*(RMAX/365.0*.5)**co2rb)*cpi_2003*1000.
!      if(RMAX/365.0.lt.60.0) write(6,*) 'dh5outCC', itimeyr, aresid(ires), rmax/365.0, co2_f, (co2rk*(RMAX/365.0)**co2rb)*cpi_2003*1000.

!        AMOUNT OF CO2 PURCHASED IS EQUAL TO AMOUNT INJECTED - RECYCLED
         CO2OAM=co2om20
         IF(RMAX/(365.0).LT.20.0) CO2OAM = co2om_20
         CO2OAM2 = CO2OAM
         DO IYR= 1, max_yr-1
            IF(TOTINJ(IYR)-TORECY(IYR).GT.0) &
               INJ(IYR) = INJ(IYR) + &
                              (TOTINJ(IYR)-TORECY(IYR))*CO2COST
               OAM(IYR) = OAM(IYR) + OAM_M(IYR)*TOTINJ(IYR)*CO2OAM &
                                                     + PSW_W * 0.25
               FOAM(IYR) = FOAM(IYR) + TOTINJ(IYR)*0.40*FCO2                    !0.40 ->inj_oam1
               TORECY_CST(IYR) = TORECY_CST(IYR) + &
                                       TORECY(IYR)*CO2OAM2*OAM_M(IYR)
         ENDDO
       ENDIF   ! CO2 MISCIBLE COSTS
!
!
! STEAM MODEL
!
!     TOTINJ = STEAM INJECTED
!     WATINJ = FEUL REQUIRED TO RUN GENERATORS...
!
        IF (PROCDE.EQ.4) THEN
           DO IYR = 1,max_yr
               FACCOST(IYR) = 0.0
               INJ(IYR)     = 0.0
               OAM(IYR)     = 0.0
           END DO
!
!          LETS CALCULATE THE GENERATORS REQUIRED IN THE GIVEN STEAM PROJECT
!
           DO IYR =1,max_yr-1
               IGEN(IYR) = 0.0
               TEST = TOTINJ(IYR)/365.0/3.0
               IF(IYR.EQ.1) IGEN(IYR) = NINT(TEST)
               IF(IYR.GT.1) IGEN(IYR) = INT(TEST)
               IF(IYR.GT.1)THEN
                  IF(IGEN(IYR).LE.IGEN(IYR-1))IGEN(IYR)=IGEN(IYR-1)
               ENDIF
           ENDDO
!
!         CAPITAL EXPENDITURES
!
          RMAX = 0.0
          CALL  MAXRATE(WATPROD,max_yr-1,RMAX)
          RMAXWAT=RMAX/365.0
          RMAX = 0.0
          CALL  MAXRATE(OILPROD,max_yr-1,RMAX)
          RMAXOIL=RMAX/365.0
!     FOR ADVANCED STEAM
          APAT = 0.0
          DO IYR=1,ILIFE   !ADVANCED
             APAT = APAT + PATDEV(IRES,IYR,2)
          END DO
          DO IYR=1,ILIFEX2 !BASE
             APAT = APAT + PATDEV(IRES,IYR,1)
          END DO

          OOIP = XOOIP/1000.0
          IF (PATN(1).GT.0.0) THEN
                FACCOST(1)=FACCOST(1) &
                            + (OOIP*0.1*2.0* APAT/TOTPAT)    & ! KING FACTOR
                            + (recy_wat*RMAXWAT + recy_oil*RMAXOIL)      & ! PRODUCED WATER RECYLING COST
                            + (stmma * TOTPAT*PATSZE)   ! STEAM MANIFOLDS/PIPELINES ETC
          END IF

          DO IYR=1,max_yr-1
             IF(IYR.EQ.1)IG=0
             IF(IYR.GT.1)IG=IGEN(IYR-1)
             FACCOST(IYR) = FACCOST(IYR) &
                            + (IGEN(IYR)-IG)*(stmga)    ! GENERATOR COST
!             PRINT*, iyr,oam(iyr)
             OAM(IYR) = OAM(IYR) + (wat_oam1*WATPROD(IYR)*oam_m(iyr)) &
               + (oil_oam1*OILPROD(IYR)*OAM_M(IYR)) &
               + (inj_oam1*WATINJ(IYR)*OAM_M(IYR))
          ENDDO
        ENDIF  ! END OF STEAM COST CALCULATION
!
       RETURN
       END SUBROUTINE
!
!*******************************************************************************************

      subroutine co2cost_sort(OGSM_REG)

      implicit none

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include 'ogsmbfw'
      include 'ogsmugr'
      include 'ogsml48'
      include 'ctus'

      logical once
      integer OGSM_REG
      integer ir,is,ii,iii,iiii,t_yr

      DATA ONCE/.FALSE./

!     INITIALIZE VARIABLES

      IF (.not.ONCE) THEN
         s_cost = 999.99
         s_reg = 0
         s_src = 0
         once=.true.
      ENDIF

      DO t_yr = itimeyr , max_yr

!        SORT COSTS -- LOWEST TO HIGHEST

         DO ir = 1, max_reg-1
            DO is = 1 , max_src
               IF (reg_costco2(OGSM_REG,ir,is,t_yr) .LT. 998.0) THEN
  
                  WRITE(ogbug1,3171) CURIRUN, CURIYR+1989, itimeyr+2010, t_yr+2010, CURITR, OGSM_REG, ir, is, &
                     reg_costco2(OGSM_REG,ir,is,t_yr), nat_availco2(ir,is,t_yr)
 3171             FORMAT(1X,"co2cost_sort_input",8(":",I6),2(":",F21.3))
               END IF
            END DO
         END DO

         i = 0
         DO ir=1,max_reg-1
            DO is=1,max_src
               IF (reg_costco2(OGSM_REG,ir,is,t_yr) .lt. 999.0) THEN
                  if (i.eq.0) then
                     s_cost(OGSM_REG,1,t_yr) = reg_costco2(OGSM_REG,ir,is,t_yr)
                     s_reg(OGSM_REG,1,t_yr) = ir
                     s_src(OGSM_REG,1,t_yr) = is
                  else
                     DO ii=1,i
                        if (reg_costco2(OGSM_REG,ir,is,t_yr).lt.s_cost(OGSM_REG,ii,t_yr)) then
                           DO iii=i,ii,-1
                              s_cost(OGSM_REG,iii+1,t_yr) = s_cost(OGSM_REG,iii,t_yr)
                              s_reg(OGSM_REG,iii+1,t_yr) = s_reg(OGSM_REG,iii,t_yr)
                              s_src(OGSM_REG,iii+1,t_yr) = s_src(OGSM_REG,iii,t_yr)
                           ENDDO
                           s_cost(OGSM_REG,ii,t_yr) = reg_costco2(OGSM_REG,ir,is,t_yr)
                           s_reg(OGSM_REG,ii,t_yr) = ir
                           s_src(OGSM_REG,ii,t_yr) = is
                           EXIT
                        endif
                     ENDDO
                     IF (ii.eq.i+1) THEN
                        s_cost(OGSM_REG,ii,t_yr) = reg_costco2(OGSM_REG,ir,is,t_yr)
                        s_reg(OGSM_REG,ii,t_yr) = ir
                        s_src(OGSM_REG,ii,t_yr) = is
                     ENDIF
                  endif
                  i=min(i+1,30)
               ENDIF
            ENDDO
         ENDDO
         s_num(OGSM_REG,t_yr) = i
      END DO ! t_yr

      RETURN
      END SUBROUTINE
