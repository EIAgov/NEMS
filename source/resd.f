! ------------------------------------------------------------------
! NEMS Residential Demand Module (RDM)                             *
!                                                                  *
! A component of the U.S. Energy Information Administration of the *
!  Department of Energy's National Energy Modeling System (NEMS)   *
!                                                                  *
! LANGUAGE:      FORTRAN                                           *
! CALLED BY:     PROGRAM NEMS (Integrating Module)                 *
!                                                                  *
! ANALYSIS:      AEO2022                                           *
! CASE:          Reference                                         *
! DATE:          September 26, 2022                                 *
!                                                                  *
!*******************************************************************
! AEO2023 CHANGES                                                  *
! -Close RSSTK input file after reading values (!close_RSSTK)      *
!*******************************************************************
! AEO2022 CHANGES                                                  *
! -Endogenize housing demolition/decay rate (!HDRendog)            *
! -Add new MELs from 2021 contractor report: smart speakers (SPK), *
!   smartphones (PHN), tablets (TAB), and small kitchen appliances *
!   (KIT); separate pool pumps and heaters (PHP -> PLP & PLH); replace*
!   DVD MELs with over-the-top (OTT) streaming devices (!MELs21)   *
! -Add switch for MELs income effect into RSMELS.txt (!IncEff)     *
!*******************************************************************
! AEO2021 CHANGES                                                  *
! -Remove distributed generation from end-use consumption and      *
!   report separately for EMM use (!DGreport)                      *
! -NOTE: there are many instances where mNumCR 10 is used to denote*
!   national total; per PARAMETR includes file, 10 is California   *
!   and 11 should be national; no fix yet implemented              *
! -Update solar PV contagion effect factor to ACTUALLY be 0.090    *
! -Remove read-in of natural gas and electricity from RSSTEO.txt;  *
!   this comes directly from common steoblock (!STEOgasElecBench)  *
!*******************************************************************
! AEO2020 CHANGES                                                  *
! -Switch distillate fuel oil + kerosene back to an additive       *
!   benchmarking factor from multiplicative (undo !DSmultiBench)   *
! -Expand WTHRZTN array to accommodate inputs by housing type for  *
!   low income and non-low income weatherization (!LIupdate)       *
! -Add ability to use weighted marginal (wholesale) and retail     *
!   electricity rate blend instead of retail space cooling         *
!   electricity rate for solar PV penetration (!DGrate)            *
! -Revise MaxNiche value to accommodate more distributed generation*
!   niches when updating to 2015 RECS microdata and ZIP code-level *
!   solar PV insolation rates (!DGniches)                          *
! -Update solar PV contagion effect factor to 0.090 to align with  *
!   Stanford DeepSolar data incorporation (!DeepSolar)             *
! -Remove unused WATERLOAD (not used since AEO2007)                *
! -Update WASHNEW and NEWDRYSAT average annual penetration rate    *
!   into new homes based on 2015 RECS data                         *
! -Move STEO electricity (and natural gas) benchmarking tweaking   *
!   factor from code to RSSTEO.txt (!STEObenchX)                   *
! -Move consumer discount rate (ResDiscountRate; formerly DISRT) and*
!   average time spent in home (Tenure; formerly HORIZON) to RSMISC*
!*******************************************************************
! AEO2019 CHANGES                                                  *
! -Modify distillate-kerosene benchmarking to use multiplicative   *
!   factors rather than additive (!DSmultiBench)                   *
! -Some general code cleanup throughout                            *
! -Moved CPP subsidies from federal to non-federal investment      *
!   reporting to RESDEQP.txt output file; CPP may be a federal     *
!   program/ policy, but the EE subsidy component would be         *
!   implemented by utilities (!CPPinvest)                          *
! -Updated space cooling equipment types (xlRTEQTYPE in RSMEQP)    *
!   used to differentiate between cooling classes (!CoolTypes)     *
! -Updated water heater equipment types (xlRTEQTYPE in RSMEQP)     *
!   referenced for 2015 water heater standard (!WHStandard)        *
! -Added parameters for hard-coded end-use class and technology    *
!   count numbers, with values calculated in the different tabs    *
!   of RSMESS.xlsx (!EqpParam)                                     *
! -Adjusted cost for ASHP compared with central AC (!ACcost)       *
! -Increased HORIZON, the average time/ tenure spent in a home,    *
!   to 9 years per National Association of Realtors 2015 data      *
! -Updated average natural gas water heating UEC (XWATERHTGMMBTU)  *
! -Updated penetration rate of diswashers into new construction    *
!   (DISHNEWpen)                                                   *
! -Updated share of homes with propane grills (LPGGRILL)           *
! -Updated shares of natural gas water-heated homes that also      *
!   have natural gas cooking ranges by housing type (NGNGFACT)     *
! -Combine kerosene with distillate fuel oil inputs and            *
!   benchmarking (!KeroBench)                                      *	!kj - currently keeping placeholders in RSSTK, RSUEC, RSCLASS, RSMEQP, RSMSHL, RSSTEO
! -Standardize use of CACPR, DWPR, and ELDRYPR penetration rate    *
!   inputs from RSMISC.txt and prevent greater than 90%            *
!   penetration of central ACs, dishwashers, and electric clothes  *
!   dryers into existing homes from the RECSYear (!RSMISCpen)      *
! -Add shares of clothes washers by type (!CWshr)                  *
! -Reorganize read-in of RSMISC.txt to group similar equipment     *
! -Adjust electricity bench factor to 1.1 after iLastSTEOYr        *
!   for initial STEO benchmarking (!STEObenchEL)                   *
! -Add variable(iExogHistYr) read in from RSGENTK.txt input file   *
!   to note last year of historical capacity data for each DG      *
!   tech; triggers start of model builds(!ExogHist)                *
! -Move shares of refrigerators and freezers by type/ orientation  *
!   from code to RSMISC.txt input file (!RefFreezShr)              *
! -Organize read-in of NEWDRYSAT from RSMISC.txt by fuel then CD   *
!   (rather than CD THEN fuel)                                     *
! -Streamline RSMEQP so that lines with census division set equal  *
!   to 11 automatically use the same values for CD 1-9 rather      *
!   than RSMEQP having redundant data rows (!EqpDivCopy)           *
! -Combine RSCLASS, RSMEQP, and RSMSHL input files into a single   *
!   multi-tab RSMESS xlsx workbook using named ranges (!ExcelRead) *
! -Clean up read-in of RSGENTK.txt and remove xIntRate read-in     *
!   (variable was being overwritten by MC_RMMTG30CON already)      *
!*******************************************************************
! AEO2018 CHANGES                                                  *
! -Adjust electricity bench factor by 1.3 in 2018 onward for       *
!   October STEO benchmarking (!STEObenchEL)                       *
! -Fixed solar PV investment calculation in ZIP code model to      *
!   incorporate ExogPVMistie capacity (!PVinvest)                  *
! -Repurposed RSESTAR file to read in HVBETA1 & HVBETA2 from       *
!   RSMSHL.txt for ENERGY STAR historical single-family housing    *
!   start share benchmarking and LEARNFACT from RSUECSHL.txt       *
!   for projected ENERGY STAR shell increases; former HVEQWTN      *
!   values from RSESTAR.txt have been removed because they've      *
!   been calculated endogenously since AEO2014 (!RSESTARbetas)     *
! -Split RSMEQP.txt subsidies between federal (RTEQSUB, RTRESUB)   *
!   and non-federal (RTEQSUBN, RTRESUBN) by Census division        *
!   (!Utility_invest)                                              *
! -Disaggregate lighting betas by individual technology vintage    *
!   (!lgtbetas)                                                    *
! -Calculate ZIP code PV model cost multiplier adjustment for      *
!   historical PV costs from RSGENTK (!PVmultiplier)               *
! -Add output of own-use generation to RDGENOUT (!OwnUseOut)       *
! -Update PV ZIP code model to use dynamic coefficients for        *
!   contagion effect (!PVcontagion)                                *
! -Split RSMLGT.txt lighting subsidies between CPP (SubX) and      *
!   historical EE(EE_SubX) by Census division; remove !lgtsubhist  *
!   code now that subsidies are split out (!EElightsub)            *
! -Allow PV ZIP code model capacity calibration to be turned       *
!   off in RGENTK.txt (!PVzipcalib)                                *
! -Rename UseNewModel variable as UseZipModel and clean up format  *
!   and read-in of RGENTK.txt                                      *
! -Add year variable read-in from RSHTSHR.txt  (!HtShrYr)          *
! -Replace various hard-coded year indices with variables (!yr)    *
! -Use STEO/ MER average fossil fuel heat rate by year and CD      *
!   for converting renewable energy to electricity  (!STEOhr)      *
! -Remove unused NEWFRIDGEUEC from RSUECSHL.txt                    *
! -Remove unused RTMAJORF from RSCLASS.txt                         *
!*******************************************************************
! AEO2017 CHANGES                                                  *
! -Keep lighting subsidies available regardless of CPP switch in   *
!   order to accommodate historical energy efficiency rebates      *
!   (!lgtsubhist)                                                  *
! -Updated call to calculate EE Costs to zero out costs in cost    *
!   calc rather than subroutine call so can call subroutine        *
!   in all cases, also read bldbase in all cases (!EEcosts)        *
! -Add MEL modeling of wine coolers, including input from RSMELS,  *
!   RSSTK, and RSUEC files (!winecool)                             *
! -Initialize variables read in from RGENTK for PV ZIP code model  *
! -Move initialization of DG accumulation variables above test     *
!   to use PV ZIP code model                                       *
! -Adjust electricity bench factor by 1.1 in 2017 onward for       *
!   September STEO benchmarking (!STEObenchEL)                     *
! -Adjust shares of PV generation deducted from space cooling      *
!   and other appliances (!PVshare)                                *
! -Modify PV ZIP-code model to correct sales to grid rather than   *
!   assume that all generation is for own use (!PVownuse)          *
! -Calibrate ZIP-code PV penetration model to historical           *
!   exogenous PV capacity from RSGENTK (!PVzipcalib)               *
! -Changed year for RSHTRSHR Census data read-in (!SOC update)     *
! -Average bnchfct for post-STEO years based on last 5 historical  *
!   data years (or less depending on RECS year) (!STEOread-avg)    *
! -Change HSHELL and CSHELL WRITE-out placeholders for CDIV and    *
!   BLDG from 0 and 0 to 11 and 1 (respectively) for RESDBOUT      *
! -Correct various spelling errors (xDegrad) and in comments       *
! -Remove 1.45 STEO benchmarking factor for 2016 on (!STEObenchNG) *
!*******************************************************************
! AEO2016 CHANGES                                                  *
! -For February 2016 STEO benchmarking, NG back to MER factor      *
!   times 1.45 for 2016 on (!STEObenchNG)                          *
! -Read STEO values from new steoblock include (!STEOread-avg)     *
! -Changed year for RSHTRSHR Census data read-in (!SOC update)     *
! -Add variables for end-use consumption including PV              *
!   self-gen for uldsm to build load shapes (!PVdispatch)          *
! -Add initialization for iteration control and subsidy capability *
!   for renewable DG (!111dren)                                    *
! -Generalize lighting menu dollar costs instead of hard-code      *
!   requires dollar year at beginning of lighting menu allows all  *
!   tech menus to have different dollar year (!lightmenu)          *
! -Add read of dollar year for technology file (!rtek$)            *
! -Set 80% limit on share of households that can have PV in each   *
!   ZIP code                                                       *
! -WRITE HSHELL and CSHELL factors to RESDBOUT.txt (not RESOUT)    *
! -Remove AEO2015 natural gas 1.1 BNCHFCT tweak                    *
! -Change SEDS benchmark code temporarily until defaulted;         *
!   commented out for STEO benchmarking 1/19/2016 (!bnchSEDS)      *
! -Updated calculation of solar PV generation based on PVWatts 5   *
!   (!PVgen)                                                       *
! -Removed RECS average solar PV generation constraint             *
! -Updated output of end uses to four characters (!eu)             *
! -Added econometric PV penetration model option, coefficients,    *
!   ZIP code data and control switch are in rgentk.txt (!PVPen)    *
! -Dynamically ALLOCATE several large arrays to cut common block   *
!   NEMS overhead (!DYN)                                           *
!*******************************************************************
! AEO2015 CHANGES                                                  *
! -111(d) adjust lighting capital cost calc                        *
! -111(d) test version, search 111(d)                              *
!   -Changed indirect cost multiplier to 1.5                       *
! -Finalized September STEO Benchmarking                           *
!   -NG back to MER factor *1.1 for 2015 on                        *
!   -EL 2014 STEO factor throughout                                *
! -Finalized August STEO Benchmarking                              *
!   -NG back to MER bench in 2015; EL STEO throughout              *
! -Major end-use equipment tech update:                            *
!   -Refrigerator and freezer Shares                               *
!   -Tech dollar Year = 2013$ per Navigant                         *
! -Adjust new fan efficiency to implement furnace fan standard     *
!   effective 2019                                                 *
! -Adjust furnace fan consumption calculations to use new versus   *
!   average UEC for fans replaced in the current model year        *
! -Moved furnace fan weather adjustment from UEC calculation to    *
!   consumption calculation                                        *
! -Adjusted furnace fan equipment count calculation to include     *
!   EQCREP(matches heating equipment count now)                    *
! -Added light bulb type to database output file                   *
! -Moved diagnostic print for PV technical potential inside of     *
!   census division loop (no effect on any NEMS variables          *
! -Changed lighting initialization to maintain foresight ability   *
! -Added residential housing starts (back) to the residential      *
!   database output file (no effect on any NEMS variables          *
! -Eliminated several arrays that are no longer used               *
!*******************************************************************
! AEO2014 CHANGES                                                  *
! -Added equipment and shell subsidy values to input files         *
! -TECHG revised                                                   *
!   -KDEGDAY read-in                                               *
!   -ELOTPEN dimensions                                            *
!   -PCPEN, other RMELS dimensions                                 *
! -HVEQWTN read-in from RSESTAR.txt avoided due to outdated data   *
! -BNCHFCT changed                                                 *
! -RSSQFT updated, read-in by BT THEN DIV                          *
! -Merged to capture SW2's 1.506 lighting changes                  *
! -Reorganized the read-in of RSSTK and RSUEC files                *
! -Removed saturation of TVs, PCs, etc in RSMELS                   *
! -Changed read-in of RSSTEO housing starts                        *
! -Now uses MAM starts throughout                                  *
! -Reorganized the read-in of RSMELS                               *
! -Added network equipment, pool heaters and pumps                 *
! -Removed external power supplies                                 *
! -Changed year for RSHTRSHR read-in (C25 update)                  *
! -Finalized September STEO Benchmarking                           *
! -Replaced old fossil fuel heat rate for renewable consumption    *
!   with parameter in resdrep STEO section                         *
! -Increased the CRI-related penalty in lighting                   *
! -Revised MELs variables to three-letter codes                    *
! -Removed subroutines for TV and PC consumption                   *
! -Moved TV and PC-related end uses to APCNS                       *
! -Cleanup of residential database-related concepts                *
! -Removed unusual benchmarking treatment                          *
! -Revised stock of 2009 Electric Other Appliances                 *
! -Increased weather elasticity factors                            *
! -Removed coal from consumption totals and BNCHFCT                *
! -Revised benchmarking of kerosene                                *
! -Added income effect to more MELs                                *
! -Adjusted benchmarking for Table 31 only                         *
! -Fixed benchmarking treatment for electric other                 *
! -Removed last vestiges of coal consumption                       *
! -Added subsidy for shell and equipment for residential database  *
! -Added benchmarking bb factors to database heating               *
!*******************************************************************
! AEO2000 CHANGES                                                  *
! -Compute and vintage replacements of replacement equipment       *
!*******************************************************************
      MODULE R_

      INCLUDE 'parametr'
      INCLUDE 'ncntrl'
      INCLUDE 'apq'
      INCLUDE 'resdrep'
      INCLUDE 'rtek'
      INCLUDE 'bldglrn'
      INCLUDE 'emmparm'
      INCLUDE 'emission'
      INCLUDE 'eusprc'
      INCLUDE 'emablk'
      INCLUDE 'macout'
      INCLUDE 'rscon'
      INCLUDE 'rseff'
      INCLUDE 'qsblk'
      INCLUDE 'cogen'
      INCLUDE 'uefpout' !electricity price for grid sales
      INCLUDE 'uecpout' !contains RPS credit price in 1987 mills/kWh - EPRPSPR(CurIYr)
      INCLUDE 'e111d'
      INCLUDE 'steoblock' !common STEO inputs !STEOread-avg

! Array location for aggregate of census division data
      INTEGER NationalPtr
      PARAMETER (NationalPtr=11)

! Parameters for RSCLASS, RSMEQP, and RSMSHL input file RSMESS.xlsx
      INTEGER nHeatClasses,nCoolClasses,nClWashClasses,nDishClasses,nWatHtClasses,nCookClasses,nClDryClasses,nRefrClasses,nFrezClasses  !Max number of end-use classes in RSCLASS  !EqpParam
      INTEGER nHeatTypes,nCoolTypes,nClWashTypes,nDishTypes,nWatHtTypes,nCookTypes,nClDryTypes,nRefrTypes,nFrezTypes  !Max number of end-use equipment types in RSMEQP  !EqpParam
      INTEGER nShellTypes  !Max number of shell types in RSMSHL

      PARAMETER (nHeatClasses=11)  !EqpParam
      PARAMETER (nCoolClasses=5)  !EqpParam
      PARAMETER (nClWashClasses=1)  !EqpParam
      PARAMETER (nDishClasses=1)  !EqpParam
      PARAMETER (nWatHtClasses=5)  !EqpParam
      PARAMETER (nCookClasses=3)  !EqpParam
      PARAMETER (nClDryClasses=2)  !EqpParam
      PARAMETER (nRefrClasses=1)  !EqpParam
      PARAMETER (nFrezClasses=1)  !EqpParam

      PARAMETER (nHeatTypes=36)  !EqpParam
      PARAMETER (nCoolTypes=17)  !EqpParam
      PARAMETER (nClWashTypes=8)  !EqpParam
      PARAMETER (nDishTypes=4)  !EqpParam
      PARAMETER (nWatHtTypes=18)  !EqpParam
      PARAMETER (nCookTypes=5)  !EqpParam
      PARAMETER (nClDryTypes=8)  !EqpParam
      PARAMETER (nRefrTypes=12)  !EqpParam
      PARAMETER (nFrezTypes=8)  !EqpParam

      PARAMETER (nShellTypes=5)

! Parameters for RSMLGT lighting menu and arrays
      INTEGER NLRec,MaxApps,MaxTypes,MaxBins
      PARAMETER (NLRec=100)  !Number of lighting records in the technology database
      PARAMETER (MaxApps=4)  !Maximum number of applications
      PARAMETER (MaxTypes=4) !Maximum number of bulb types within an application
      PARAMETER (MaxBins=6)  !Maximum number of hours per day usage bins per applications

! Parameters for Price-Induced Technical Change
!   These parameters allow first years of availability to be advanced when energy price increases are large.
!   The idea is that the menu years are based on business as usual and would not account for R&D in the event of large energy price increases.
!   Setting IFMAX = 0 turns this feature off.
      INTEGER   IFMAX         !MAXIMUM FORWARD EFFECT
      PARAMETER (IFMAX=0)

      COMMON/BASE111D/BASELINEBKWH(mNumCR,mNumYr) !111(d)
      COMMON/EFFDriver/Driver(mNumYr,8,mNumCR-2,mNumBldg),Driver2(mNumYr,mNumCR-2,mNumBldg)
      COMMON/EQCES/EQCESE(RECSYear:EndYr+1,mNumRTCl,mNumBldg,mNumCR)
      COMMON/EQCRP/EQCRP90(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR)
      COMMON/EQCSR/EQCSR90(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR)
      COMMON/EQADD/EQCADD(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR)
      COMMON/EQREP/EQCREP(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR)
      COMMON/EQSUR/EQCSUR(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR)
      COMMON/RFCON/NHTRFL,NCLFL,NWHFL,NSTVFL,NDRYFL,NREFFL,NFRZFL, &
       FHTRCON(10),FCLCON(10),FWHCON(10),FSTVCON(10), &	!kj - what does the 10 signify? Max number of fuels for each consumption type?
       FDRYCON(10),FREFCON(10),FFRZCON(10), &
       FCSWCON(10),FDSWCON(10),NCSWFL,NDSWFL,NSHTRFL
      COMMON/LFE/  HDRfy,HDRly,HDR(mNumBldg),HDQ((BaseYr-BaseYr+2):(EndYr-BaseYr+1),mNumCR,mNumBldg),HDi((BaseYr-BaseYr+2):(EndYr-BaseYr+1),mNumCR,mNumBldg), &  !HDRendog
                    HDiAve(mNumCR,mNumBldg),HCDshr((BaseYr-BaseYr+2):(EndYr-BaseYr+1),mNumCR,mNumBldg),HCDshrAve(mNumCR,mNumBldg),ResDiscountRate,Tenure  !HDRendog
      COMMON/EXHS/ EH(RECSYear:EndYr,mNumBldg,mNumCR-2)
      COMMON/CWSHR/TCW_SHR,FCW_SHR  !CWshr
      COMMON/FRZSHR/TMF_SHR,SMF_SHR,BMF_SHR,CH_SHR,UP_SHR  !RefFreezShr
      COMMON/SAT/  RACSAT(mNumBldg,mNumCR), &
                   RACUnits(mNumBldg,mNumCR), &
                   CACSAT(mNumBldg,mNumCR), &
                   CACPR(mNumCR), &
                   FRZSAT(mNumBldg,mNumCR), &
                   ELDRYPR(mNumBldg,mNumCR), &
                   REFSAT(mNumBldg,mNumCR)
      COMMON/RENSH/RENSHR(mNumCR)
      COMMON/SHTR/ SHTSHR(mNumBldg,mNumCR,8),NSHTSHR(mNumBldg,mNumCR,8)
      COMMON/SHELL/ &
       EHSHELL(RECSYear:EndYr+1,MNUMFUEL,mNumCR,mNumBldg), &
       ECSHELL(RECSYear:EndYr+1,mNumCR,mNumBldg), &
       NHSHELL(RECSYear:EndYr+1,MNUMFUEL,mNumCR,mNumBldg), &
       NCSHELL(RECSYear:EndYr+1,mNumCR,mNumBldg), &
       AHSHELL(RECSYear:EndYr+1,MNUMFUEL,mNumCR,mNumBldg), &
       ACSHELL(RECSYear:EndYr+1,mNumCR,mNumBldg), &
       TECHG(RECSYear:EndYr+1,mNumCR-2,mNumBldg),LIMIT
      COMMON/WTHRADJ/HDDYEAR(BaseYr:NUMHDDYR),CDDYEAR(BaseYr:NUMCDDYR)
      COMMON/APPLSAT/NEWDRYSAT(RECSYear+1:EndYr,2,mNumBldg,mNumCR-2), &
       DISHNEW(RECSYear+1:EndYr,mNumBldg,mNumCR-2),WASHNEW(RECSYear+1:EndYr,mNumBldg,mNumCR-2)  !MELs21 - these should not have been paired with MELs
      COMMON/ALLHOUSE/OLDHSES(RECSYear:EndYr),NEWHSES(RECSYear:EndYr)
      COMMON/INSCOST/RPINSCOST(mNumRTCl,mNumRTCl)
      COMMON/NEWMISC/MELsIncomeEffect(30),&  !IncEff - 30 is an arbitrary value meant to exceed the current number of end uses
       TVSPEN(RECSYear:EndYr),TVSEFF(RECSYear:EndYr),STBPEN(RECSYear:EndYr),STBEFF(RECSYear:EndYr),&
       HTSPEN(RECSYear:EndYr),HTSEFF(RECSYear:EndYr),OTTPEN(RECSYear:EndYr),OTTEFF(RECSYear:EndYr),&  !MELs21
       VGCPEN(RECSYear:EndYr),VGCEFF(RECSYear:EndYr),&
       DPCPEN(RECSYear:EndYr),DPCEFF(RECSYear:EndYr),LPCPEN(RECSYear:EndYr),LPCEFF(RECSYear:EndYr),&
       MONPEN(RECSYear:EndYr),MONEFF(RECSYear:EndYr),NETPEN(RECSYear:EndYr),NETEFF(RECSYear:EndYr),&
       BATPEN(RECSYear:EndYr),BATEFF(RECSYear:EndYr),CFNPEN(RECSYear:EndYr),CFNEFF(RECSYear:EndYr),&
       COFPEN(RECSYear:EndYr),COFEFF(RECSYear:EndYr),DEHPEN(RECSYear:EndYr),DEHEFF(RECSYear:EndYr),&
       MCOPEN(RECSYear:EndYr),MCOEFF(RECSYear:EndYr),PLPPEN(RECSYear:EndYr),PLPEFF(RECSYear:EndYr),&
       PLHPEN(RECSYear:EndYr),PLHEFF(RECSYear:EndYr),&  !MELs21
       SECPEN(RECSYear:EndYr),SECEFF(RECSYear:EndYr),SPAPEN(RECSYear:EndYr),SPAEFF(RECSYear:EndYr),&
       WCLPEN(RECSYear:EndYr),WCLEFF(RECSYear:EndYr),&   !winecool
       SPKPEN(RECSYear:EndYr),SPKEFF(RECSYear:EndYr),PHNPEN(RECSYear:EndYr),PHNEFF(RECSYear:EndYr),&  !MELs21
       TABPEN(RECSYear:EndYr),TABEFF(RECSYear:EndYr),KITPEN(RECSYear:EndYr),KITEFF(RECSYear:EndYr)  !MELs21
      COMMON/APLSHARES/NEWHEATUEC(nHeatClasses,mNumBldg,mNumCR-2),NEWCOOLUEC(mNumBldg,mNumCR-2),BASELOAD(16)
      COMMON/RETIRE/EQCRET(RECSYear:EndYr,mNumRTCl)
      COMMON/SQRFOOT/SQNEW(RECSYear:EndYr,mNumBldg,mNumCR-2),EXSQFTADJ(RECSYear:EndYr,mNumBldg,mNumCR-2,5)
      COMMON/SQRFLTS/ELASTIC(5,mNumCR-2)
      COMMON/SQFTDATA/SQRFOOT(RECSYear:EndYr,mNumBldg,mNumCR-2),EXSQRFOOT(RECSYear:EndYr,mNumBldg,mNumCR-2),STOCKSQRFOOT(RECSYear:EndYr,mNumBldg,mNumCR-2)
      COMMON/PRI/PRICES(MNUMFUEL,mNumCR,BaseYr:EndYr)
      COMMON/DRYER/DRYSHR(8,mNumBldg,mNumCR)  !NG_DRY1, NG_DRY2, NG_DRY3, NG_DRY4, ELEC_DRY1, ELEC_DRY2, ELEC_DRY3, ELEC_DRY4	!kj - Was 4; should this be 8 now? Is it even used? If so, find a way to make this value dynamic?
      COMMON/DRYSA/DRYSAT(mNumBldg,mNumCR)
      COMMON/HOTWATER/HOTWATQ(RECSYear:EndYr,mNumBldg,mNumCR-2), &
        CWLOAD(RECSYear), &
        NCWLOAD(RECSYear:EndYr,mNumCR-2,mNumBldg),ECWLOAD(RECSYear:EndYr,mNumCR-2,mNumBldg), &
        DWPR(mNumBldg,mNumCR)
      COMMON/EFFIC/EQCEFF(RECSYear:EndYr,mNumRTCl)
      COMMON/STEFFIC/STKEFF(RECSYear:EndYr,mNumRTCl)
      COMMON/EUECS/EQCUEC(mNumCR,mNumRTCl,mNumBldg) &
        ,FANUEC(mNumCR,mNumBldg) &
        ,TVSUEC(mNumCR,mNumBldg),STBUEC(mNumCR,mNumBldg),HTSUEC(mNumCR,mNumBldg) &
        ,OTTUEC(mNumCR,mNumBldg),VGCUEC(mNumCR,mNumBldg),DPCUEC(mNumCR,mNumBldg) &  !MELs21
        ,LPCUEC(mNumCR,mNumBldg),MONUEC(mNumCR,mNumBldg),NETUEC(mNumCR,mNumBldg) &
        ,BATUEC(mNumCR,mNumBldg),CFNUEC(mNumCR,mNumBldg),COFUEC(mNumCR,mNumBldg) &
        ,DEHUEC(mNumCR,mNumBldg),MCOUEC(mNumCR,mNumBldg),PLPUEC(mNumCR,mNumBldg),PLHUEC(mNumCR,mNumBldg) &  !MELs21
        ,SECUEC(mNumCR,mNumBldg),SPAUEC(mNumCR,mNumBldg),WCLUEC(mNumCR,mNumBldg) &    !winecool
        ,SPKUEC(mNumCR,mNumBldg),PHNUEC(mNumCR,mNumBldg),TABUEC(mNumCR,mNumBldg),KITUEC(mNumCR,mNumBldg) &  !MELs21
        ,EAUEC(mNumCR,mNumBldg),FANIUEC(mNumCR,mNumBldg),SHTUEC(mNumCR,7,mNumBldg),APPUEC(mNumCR,3,mNumBldg)
      COMMON/EQCUEC/EQCNUEC(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR) &
        ,EQCAUEC(RECSYear:EndYr+1,mNumRTCl,mNumBldg,mNumCR) &
        ,EQCAHVUEC(RECSYear:EndYr+1,mNumRTCl,mNumBldg,mNumCR) &
        ,EQCRUEC(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR) &
        ,EQCSUEC(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR) &
        ,EQCHVUEC(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR) &
        ,EQCNIUEC(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR) &
        ,EQCRIUEC(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR) &
        ,EQCSIUEC(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR) &
        ,EQCHVIUEC(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR)
      COMMON/SLC/SLCON(RECSYear-BaseYr+1:mNumYr+1,mNumCR),SLUEC(mNumCR)
      COMMON/APLC/APLCON(RECSYear-BaseYr+1:mNumYr,3,mNumCR-2) !3 Fuels
      COMMON/RSFC/RSFLCN(RECSYear-BaseYr+1:mNumYr,8,mNumCR-1)
      COMMON/NHOUSES/NH(RECSYear:EndYr,mNumBldg,mNumCR-2) &
        ,HSEADD(RECSYear:EndYr,mNumBldg,mNumCR-2) &
        ,HHSTOCKBYDIV(RECSYear:EndYr,mNumCR-2)
      COMMON/ALLNEW/ALLNEW(RECSYear:EndYr,mNumCR-2)
      COMMON/LIFE/LFCY(MNUMRTTY,mNumBldg,mNumCR,3)
      COMMON/EQCRP90/EQCRP90RP(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR)
      COMMON/OEQCRP/OEQCRP90(RECSYear:EndYr,mNumRTCl,1,mNumCR)
      COMMON/OEQCRPR/OEQCRP90R(RECSYear:EndYr,mNumRTCl,1,mNumCR)
      COMMON/OEQREP/OEQCREP(RECSYear:EndYr,mNumRTCl,1,mNumCR)
      COMMON/SWITCH/EQCSW90(RECSYear:EndYr,mNumRTCl,mNumRTCl,1,mNumCR) &
        ,EQCSW90R(RECSYear:EndYr,mNumRTCl,mNumRTCl,1,mNumCR)
      COMMON/SW/SWITCHES(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR) &
        ,SWITCHESR(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR) &
        ,SWITCHTO(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR) &
        ,SWITCHTOR(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR)&
        ,SWTOTAL(RECSYear:EndYr,mNumRTCl,mNumCR-2) &
        ,SWFTOTAL(RECSYear:EndYr,mNumRTCl,mNumCR-2)
      COMMON/NWHTR/HSYSSHR(RECSYear:EndYr+1,nHeatClasses,mNumBldg,mNumCR)
      COMMON/HTSHRYR/HTSHRYR  !HtShrYr
      COMMON/ESTARHISTYR/ESTARHISTYR  !RSESTARbetas
     !  Dynamically ALLOCATE these large arrays:                          !DYN
     ! COMMON/EQTSH/NEQTSHR(RECSYear:EndYr+1,MNUMRTTY,mNumBldg,mNumCR) &  !DYN
     !   ,REQTSHR(RECSYear:EndYr+1,MNUMRTTY,mNumBldg,mNumCR)              !DYN
      REAL*4,ALLOCATABLE::NEQTSHR(:,:,:,:)                                !DYN
      REAL*4,ALLOCATABLE::REQTSHR(:,:,:,:)                                !DYN
      COMMON/WEQCEF/ WTEQCEFFN(RECSYear:EndYr+1,mNumRTCl,mNumBldg,mNumCR) &
        ,WTEQCEFFR(RECSYear:EndYr+1,mNumRTCl,mNumBldg,mNumCR) &
        ,WTEQCEFFA(RECSYear:EndYr+1,mNumRTCl,mNumBldg,mNumCR) &
        ,WTEQCEFFHV(RECSYear:EndYr+1,mNumRTCl,mNumBldg,mNumCR) &
        ,WTEQCSQFHV(RECSYear:EndYr+1,mNumRTCl,mNumBldg,mNumCR)
      COMMON/HURDLE/HRDRATE,ELIGBLE,ALPHA1,HRDADJ
      COMMON/DISCRATE/BETA1DR(MNUMRTTY)
      COMMON/EQCND/EQCND90(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR)       !LOCAL
      COMMON/HEATOT/HEATOT(RECSYear:EndYr+1,nHeatClasses,mNumBldg,mNumCR)
!  Dynamically ALLOCATE these large arrays:                                         !DYN
!      COMMON/EQPFUT/ &                                                             !DYN
!        EQR90FUT  (RECSYear:EndYr,RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR-2) &     !DYN
!       ,EQREPFUT  (RECSYear:EndYr,RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR-2) &     !DYN
!       ,EQADDFUT  (RECSYear:EndYr,RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR-2) &     !DYN
!       ,EQR90RPFUT(RECSYear:EndYr,RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR-2) &     !DYN
!       ,EQCESEFUT (RECSYear:EndYr,RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR-2)       !DYN
      REAL*4,ALLOCATABLE::EQR90FUT  (:,:,:,:,:)                                     !DYN
      REAL*4,ALLOCATABLE::EQREPFUT  (:,:,:,:,:)                                     !DYN
      REAL*4,ALLOCATABLE::EQADDFUT  (:,:,:,:,:)                                     !DYN
      REAL*4,ALLOCATABLE::EQR90RPFUT(:,:,:,:,:)                                     !DYN
      REAL*4,ALLOCATABLE::EQCESEFUT (:,:,:,:,:)                                     !DYN
      COMMON/EQCEQ/EQCEQCN(RECSYear-BaseYr:mNumYr,mNumRTCl,mNumBldg,mNumCR)
      COMMON/GOEQ/GEEQCN(RECSYear-BaseYr:mNumYr,4,mNumBldg,mNumCR),SLEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR) !4 refers to max number of end-use classes; space heating=1; space cooling=2	!kj
      COMMON/FANEQ/FANEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2)
      COMMON/NH2O/NH2OSH(RECSYear:EndYr+1,5,mNumBldg,mNumCR) !5 water heating fuel types
      COMMON/NWCK/NCKSH(RECSYear:EndYr+1,3,mNumBldg,mNumCR)
      COMMON/LTEQ/LTEQCN(RECSYear-BaseYr:mNumYr,4,mNumBldg,mNumCR-2)
      COMMON/EAEQ/EAEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2)
      COMMON/OTUSES/TVSEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2)&
        ,STBEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2),HTSEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2) &
        ,OTTEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2),VGCEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2) &  !MELs21
        ,DPCEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2),LPCEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2) &
        ,MONEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2),NETEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2) &
        ,BATEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2),CFNEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2) &
        ,COFEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2),DEHEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2) &
        ,MCOEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2),PLPEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2) &  !MELs21
        ,SECEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2),SPAEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2) &
        ,WCLEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2),PLHEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2) &  !winecool  !MELs21
        ,SPKEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2),PHNEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2) &  !MELs21
        ,TABEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2),KITEQCN(RECSYear-BaseYr:mNumYr,1,mNumBldg,mNumCR-2)  !MELs21
      COMMON/SHEQ/SHEQCN(RECSYear-BaseYr:mNumYr,7,mNumBldg,mNumCR-2)
      COMMON/APEQ/APEQCN(RECSYear-BaseYr:mNumYr,3,mNumBldg,mNumCR-2),APLEQP(RECSYear:EndYr,mNumBldg,mNumCR-2,3)
      COMMON/HSESHRS/HSESHR(RECSYear:EndYr,mNumBldg,mNumCR-2),HSETOT(RECSYear:EndYr)
      COMMON/RBENCH/BNCHFCT(RECSYear-BaseYr+1:mNumYr,6,mNumCR-2), BNCHFCTAVG(6,mNumCR-2)    !STEOread-avg
      COMMON/RSDgOut/Units(mNumYr,mNumCR,nTek), Cap(mNumYr,mNumCR,nTek), Trills(mNumYr,mNumCR,nTek) &
        ,TrillsOwnUse(mNumYr,mNumCR,nTek), GasUsage(mNumYr,mNumCR,nTek) &
        ,HwBtu(mNumYr,mNumCR,nTek), Invest(mNumYr,mNumCR,nTek)   &
        ,x111dRenSub(mNumYr,mNumCR,nTek),iGenCapCostYr   !111dren
      COMMON/COOLVAC/ACICOST(MNUMRTTY,RECSYear:EndYr,mNumCR-2) &
        ,ACEFF(MNUMRTTY,RECSYear:EndYr,mNumCR-2) &
        ,HTRCOST(RECSYear:EndYr,mNumCR-2)
      !  Dynamically ALLOCATE these large arrays:                                   !DYN
          ! COMMON/DBEFFOUT/RSNEFDB1(mNumYr,MNUMRTTY,mNumBldg,mNumCR-2),            !DYN
          !                 RSEEFDB1(mNumYr,MNUMRTTY,mNumBldg,mNumCR-2)             !DYN
        REAL*4,ALLOCATABLE::RSNEFDB1(:,:,:,:)                                       !DYN
        REAL*4,ALLOCATABLE::RSEEFDB1(:,:,:,:)                                       !DYN
      !  Dynamically ALLOCATE these large arrays:                                   !DYN
      !COMMON/SHELLEFF/ &                                                           !DYN
      !   HTSHELLEFFWT(RECSYear:EndYr,nHeatTypes,nShellTypes,mNumBldg,mNumCR-2) &     !DYN
      !  ,HTSHELLWT(RECSYear:EndYr,nHeatTypes,nShellTypes,mNumBldg,mNumCR-2) &        !DYN
      !  ,HSHELL(RECSYear:EndYr,nHeatClasses,mNumBldg,mNumCR-2) &                     !DYN
      !  ,CSHELL(RECSYear:EndYr,nHeatClasses,mNumBldg,mNumCR-2) &                     !DYN
      !  ,SHELLBUILDS(RECSYear:EndYr,nHeatTypes,nShellTypes,mNumBldg,mNumCR-2) &      !DYN
      !  ,SHELLINVEST (RECSYear:EndYr,nHeatClasses,nShellTypes,mNumBldg,mNumCR) &     !DYN
      !  ,SHELLSUBSIDY(RECSYear:EndYr,nHeatClasses,nShellTypes,mNumBldg,mNumCR) &     !DYN
      !  ,SHELLSUBSIDY111D(RECSYear:EndYr,nHeatClasses,nShellTypes,mNumBldg,mNumCR) & !DYN
      !  ,CLSHELLWT(RECSYear:EndYr,nCoolTypes,mNumBldg,mNumCR-2) &                    !DYN
      !  ,SHLEVELH(RECSYear:EndYr,nHeatClasses,nShellTypes,mNumBldg,mNumCR-2)         !DYN

        REAL*4,ALLOCATABLE::HTSHELLEFFWT(:,:,:,:,:)     !DYN
        REAL*4,ALLOCATABLE::HTSHELLWT(:,:,:,:,:)        !DYN
        REAL*4,ALLOCATABLE::HSHELL(:,:,:,:)             !DYN
        REAL*4,ALLOCATABLE::CSHELL(:,:,:,:)             !DYN
        REAL*4,ALLOCATABLE::SHELLBUILDS(:,:,:,:,:)      !DYN
        REAL*4,ALLOCATABLE::SHELLINVEST (:,:,:,:,:)     !DYN
        REAL*4,ALLOCATABLE::SHELLSUBSIDY(:,:,:,:,:)     !DYN
        REAL*4,ALLOCATABLE::SHELLSUBSIDY111D(:,:,:,:,:) !DYN
        REAL*4,ALLOCATABLE::CLSHELLWT(:,:,:,:)
        REAL*4,ALLOCATABLE::SHLEVELH(:,:,:,:,:)

      COMMON/OTHEREQP/FANEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),&
        EAEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),APPEQP(RECSYear:EndYr,mNumBldg,mNumCR-2,3),&
        SHTEQP(RECSYear:EndYr,mNumBldg,mNumCR-2,7),&
        TVSEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),STBEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),&
        HTSEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),OTTEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),&  !MELs21
        VGCEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),&
        DPCEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),LPCEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),&
        MONEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),NETEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),&
        BATEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),CFNEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),&
        COFEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),DEHEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),&
        MCOEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),PLPEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),&  !MELs21
        SECEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),SPAEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),&
        WCLEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),PLHEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),&  !winecool  !MELs21
        SPKEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),PHNEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),&  !MELs21
        TABEQP(RECSYear:EndYr,mNumBldg,mNumCR-2),KITEQP(RECSYear:EndYr,mNumBldg,mNumCR-2)  !MELs21
      COMMON/DISPINC/INCOME(mNumCR-2,RECSYear:EndYr,30) !DISPOSABLE INCOME VARIABLE  !IncEff - 30 is an arbitrary value meant to exceed the current number of MELs end uses (aligns with MELsIncomeEffect)
      !  Dynamically ALLOCATE these large arrays:                                               !DYN
     ! COMMON/HVACEQPSHARE/HVEQSHR(RECSYear:EndYr,nHeatTypes,mNumBldg,mNumCR-2),                !DYN
         ! HEATINGTYPEPURCH(RECSYear:EndYr,MNUMRTTY,mNumBldg,mNumCR-2,2),&                      !DYN
         ! NEQTSHRC(RECSYear:EndYr,nCoolTypes,mNumBldg,mNumCR),LEARNFACT(mNumBldg,mNumCR-2)     !DYN
        REAL*4,ALLOCATABLE::HVEQSHR(:,:,:,:)             !DYN
        REAL*4,ALLOCATABLE::HEATINGTYPEPURCH(:,:,:,:,:)  !DYN
        REAL*4,ALLOCATABLE::NEQTSHRC(:,:,:,:)            !DYN
        REAL*4,ALLOCATABLE::LEARNFACT(:,:)               !DYN

      COMMON/LTUEC/LTUEC(MaxApps,mNumCR-2,mNumBldg),LTEQP(MaxApps,RECSYear:EndYr,mNumBldg,mNumCR-2), &
        LTNUEC(MaxApps,RECSYear:EndYr,mNumCR-2,mNumBldg), LTNUECly(MaxApps,RECSYear:EndYr,mNumCR-2,mNumBldg), &
        LTCONWTly(mNumYr,mNumCR-2,mNumBldg),LTCONINly(mNumYr,mNumCR-2,mNumBldg), &  !these use year indexes
        LTCONly(RECSYear:EndYr,mNumCR-2)

      COMMON/STIMULUS/WTHRZTN(RECSYear:EndYr,2,mNumCR-2,mNumBldg)  !LIupdate - 2 = space heating and space cooling end uses

     !Common block for lighting variables !NLRec is the maximum number of lighting records in the rsmlgt.txt cost
     ! and performance section (see above for parameter setting)     !111(D) adding subsidy with division dimension and division to capital cost
      COMMON/NewLightingVars/BulbCost(NLRec),BulbEESub(NLRec,mNumCR-2),BulbSub(NLRec,mNumCR-2),LPW(NLRec),BulbWatts(NLRec),LifeHours(NLRec),BulbCRI(NLRec), &  !EElightsub
         BaseWattsBulbs(MaxApps,MaxTypes),BaseWattBins(MaxApps,MaxBins),AnnualBulbCost(MaxApps,MaxTypes,MaxBins), &
         WattsCY(MaxTypes),Beta1,Beta2,AppBinHours(MaxApps,MaxBins), BulbBinLife(MaxApps,MaxBins), &
         BulbsPerHH(MaxApps,mNumBldg),BulbBinShares(MaxApps,MaxTypes,MaxBins),BinShares(MaxApps,MaxBins), &
         BulbBinEnergy(MaxApps,MaxTypes,MaxBins),CRIBulb(MaxApps),LTlbeta1(NLRec), LTlbeta2(NLRec), watts(MaxTypes), &  !lgtbetas
         LTlCap(MaxTypes,mNumCR-2,MaxBins),LTLsub(MaxTypes,mNumCR-2),LTlcapInvest(MaxTypes),LTLIFE(MaxTypes,MaxBins),LTBinShare(MaxApps,MaxBins), &
         NumApps,NumTypes(MaxApps),AppIndex(MaxApps),NumAppBins(MaxApps),FirstYear(NLRec),LastYear(NLRec),BulbDiv(nlrec),&
         AppID(MaxApps),LightingApp(NLRec),BulbType(NLRec),RLGTDOLLARYR

      COMMON/LTDATABASE/  LTInvest(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,2),  &
                          LTsubsidy(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,2), &  !111(d) add subsidy for reporting
                          LTREPbyAPP(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2),&
                          LTNEEDEDbyAPP(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2), &
                          WTLEFFbyAPP(MaxApps,RECSYear:EndYr,mNumBldg,mNumCR-2), &
                          appbulbname(MaxApps,MaxTypes),LTSTOCK(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins)
      REAL*4 LTinvest,LTsubsidy,LTREPbyAPP,LTNEEDEDbyAPP,WTLEFFbyAPP,LTSTOCK
      CHARACTER*3 appbulbname

      REAL*4 BulbCost, BulbEESub, BulbSub, LPW, BulbWatts, LifeHours, BulbCRI,BaseWattsBulbs,BaseWattBins,AnnualBulbCost, &  !EElightsub
           WattsCY,Beta1,Beta2,AppBinHours,BulbBinLife,BulbsPerHH,BulbBinShares,BinShares,BulbBinEnergy,CRIBulb, &
           LTlbeta1, LTlbeta2, watts, LTlcap, LTLsub, LTlife, LTBinShare
      INTEGER NumApps, NumTypes, appindex, NumAppBins, FirstYear, LastYear, BulbDiv
      INTEGER RLGTDOLLARYR         !lightmenu
      CHARACTER*3 LightingApp, BulbType, AppID
      REAL*4 LTUEC, LTNUEC, LTEQP, LTCONly, LTCONWTly, LTCONINly, LTNUECly
      REAL*4 BASELINEBKWH
      REAL*4 Driver,Driver2
      REAL*4 WTHRZTN
!      REAL*4 HVEQSHR,HEATINGTYPEPURCH,NEQTSHRC,LEARNFACT !DYN
      REAL*4 INCOME
      REAL*4 FANEQP,EAEQP,APPEQP,SHTEQP
      REAL*4 TVSEQP,STBEQP,HTSEQP,OTTEQP,VGCEQP  !MELs21
      REAL*4 DPCEQP,LPCEQP,MONEQP,NETEQP
      REAL*4 BATEQP,CFNEQP,COFEQP,DEHEQP,MCOEQP,PLPEQP,PLHEQP,SECEQP,SPAEQP,WCLEQP    !winecool  !MELs21
      REAL*4 SPKEQP,PHNEQP,TABEQP,KITEQP  !MELs21
      REAL*4 FANPEN,EAPEN,APPPEN,SHTPEN
      INTEGER NumMELs,MELsIncomeEffect  !IncEff
      REAL*4 TVSPEN,STBPEN,HTSPEN,OTTPEN,VGCPEN  !MELs21
      REAL*4 DPCPEN,LPCPEN,MONPEN,NETPEN
      REAL*4 BATPEN,CFNPEN,COFPEN,DEHPEN,MCOPEN,PLPPEN,PLHPEN,SECPEN,SPAPEN,WCLPEN  !winecool  !MELs21
      REAL*4 SPKPEN,PHNPEN,TABPEN,KITPEN  !MELs21
      REAL*4 ACICOST,ACEFF,HTRCOST
      REAL*4 Units, Cap, Trills, TrillsOwnUse, GasUsage, HwBtu, Invest
      REAL*4 x111dRenSub             !111dren
      INTEGER iGenCapCostYr          !111dren
      REAL*4 BNCHFCT, BNCHFCTAVG    !STEOread-avg
      REAL*4 HSESHR,HSETOT
      REAL*4 APEQCN,APLEQP
      REAL*4 SHEQCN
      REAL*4 TVSEQCN,STBEQCN,HTSEQCN,OTTEQCN,VGCEQCN  !MELs21
      REAL*4 DPCEQCN,LPCEQCN,MONEQCN,NETEQCN
      REAL*4 BATEQCN,CFNEQCN,COFEQCN,DEHEQCN,MCOEQCN,PLPEQCN,PLHEQCN,SECEQCN,SPAEQCN,WCLEQCN    !winecool  !MELs21
      REAL*4 SPKEQCN,PHNEQCN,TABEQCN,KITEQCN  !MELs21
      REAL*4 LTEQCN
      REAL*4 NCKSH
      REAL*4 NH2OSH
      REAL*4 EQCEQCN
      REAL*4 GEEQCN,SLEQCN
      REAL*4 FANEQCN
!      REAL*4 EQR90FUT,EQREPFUT,EQADDFUT,EQR90RPFUT,EQCESEFUT  !DYN
      REAL*4 EQCRP90RP,EQCRET
      REAL*4 BETA1DR
      REAL*4 EQCND90
      REAL*4 HEATOT
      REAL*4 HRDRATE,ELIGBLE,ALPHA1,HRDADJ
      INTEGER HTSHRYR  !HtShrYr
      INTEGER ESTARHISTYR  !RSESTARbetas
      REAL*4,ALLOCATABLE::HVBETA1(:,:,:,:),HVBETA2(:,:,:,:) ! LOGIT PARAMETER 1 (INSTALLED COST) and 2 (OPERATING COST)  !RSESTARbetas
      REAL*4 HSYSSHR  !,REQTSHR,NEQTSHR  !DYN
      REAL*4 WTEQCEFFN,WTEQCEFFR,WTEQCEFFA,WTEQCEFFHV,WTEQCSQFHV
      REAL*4 SWITCHES,SWITCHESR,SWITCHTO,SWITCHTOR,SWTOTAL,SWFTOTAL
      REAL*4 EQCSW90,EQCSW90R
      REAL*4 LFCY,OEQCRP90,OEQCREP,OEQCRP90R
      REAL*4 ALLNEW
      REAL*4 NH,HSEADD,HHSTOCKBYDIV
      REAL*4 SLCON, SLUEC,SHTSHR,NSHTSHR,APLCON,RSFLCN
      REAL*4 EQCUEC,EAUEC,FANUEC,FANIUEC,SHTUEC,APPUEC
      REAL*4 TVSUEC,STBUEC,HTSUEC,OTTUEC,VGCUEC  !MELs21
      REAL*4 DPCUEC,LPCUEC,MONUEC,NETUEC
      REAL*4 BATUEC,CFNUEC,COFUEC,DEHUEC,MCOUEC,PLPUEC,PLHUEC,SECUEC,SPAUEC,WCLUEC    !winecool  !MELs21
      REAL*4 SPKUEC,PHNUEC,TABUEC,KITUEC  !MELs21
      REAL*4 EQCNUEC,EQCAUEC,EQCRUEC,EQCSUEC,EQCHVUEC,EQCAHVUEC
      REAL*4 EQCNIUEC,EQCRIUEC,EQCSIUEC,EQCHVIUEC
      REAL*4 STKEFF
      REAL*4 EQCEFF
      REAL*4 HOTWATQ,CWLOAD,NCWLOAD,ECWLOAD,DWPR
      REAL*4 DRYSAT,DRYSHR
      REAL*4 PRICES ! 1=Distillate Fuel Oil 2=Propane/LPG 3=Natural Gas 4=Electricity 5=Kerosene 6=Wood 7=Coal
      REAL*4 SQFTADJ,SQRFOOT,SQNEW,STOCKSQRFOOT,EXSQRFOOT,EXSQFTADJ,ELASTIC
      REAL*4 NEWHEATUEC,NEWCOOLUEC,BASELOAD
!      REAL*4 HTSHELLEFFWT,HTSHELLWT,HSHELL,CSHELL,CLSHELLWT,SHLEVELH,SHELLBUILDS,shellinvest,shellsubsidy,SHELLSUBSIDY111D  !DYN
      REAL*4 TVSEFF,STBEFF,HTSEFF,OTTEFF,VGCEFF  !MELs21
      REAL*4 DPCEFF,LPCEFF,MONEFF,NETEFF
      REAL*4 BATEFF,CFNEFF,COFEFF,DEHEFF,MCOEFF,PLPEFF,PLHEFF,SECEFF,SPAEFF,WCLEFF    !winecool  !MELs21
      REAL*4 SPKEFF,PHNEFF,TABEFF,KITEFF  !MELs21
      REAL*4 DISHNEW,WASHNEW
      REAL*4 OLDHSES,NEWHSES
      INTEGER HDDYEAR,CDDYEAR,RPINSCOST
      REAL*4 HDR,HDQ,HDi,HDiAve,HCDshr,HCDshrAve,ResDiscountRate,Tenure,LEAPYR  !HDRendog
      INTEGER HDRfy,HDRly  !HDRendog
      REAL*4 EH
      REAL*4 TCW_SHR,FCW_SHR !CWshr
      REAL*4 TMF_SHR,SMF_SHR,BMF_SHR,CH_SHR,UP_SHR  !RefFreezShr
      REAL*4 RACSAT,RACUNITS,CACSAT,CACPR,FRZSAT,ELDRYPR,REFSAT
      REAL*4 RENSHR
      REAL*4 NEWDRYSAT
      REAL*4 EHSHELL,ECSHELL,NHSHELL,NCSHELL,AHSHELL,ACSHELL,TECHG,LIMIT,SHELLCH,CUMSHWTNUM
      INTEGER NHTRFL,NCLFL,NWHFL,NSTVFL,NDRYFL,NREFFL,NFRZFL, &
       FHTRCON,FCLCON,FWHCON,FSTVCON,FDRYCON,FREFCON,FFRZCON, &
       FCSWCON,FDSWCON,NCSWFL,NDSWFL,NSHTRFL
      REAL*4 EQCESE,EQCRP90,EQCSR90,EQCADD,EQCREP,EQCSUR
      CHARACTER*30 TITLE
      INTEGER RSYR,PREVYR,EU,RTOVALUE,STEOBM,NRGBILL,NRGBILL07,STIMULUS,EPA111D
      EXTERNAL RTOVALUE
      END MODULE R_

!*******************************************************************
!     RESD SUBROUTINE
!*******************************************************************
      SUBROUTINE RESD
      USE R_
      IMPLICIT NONE

      !This whole block is added to dynamically assign array dimensions once in a NEMS run !DYN
       IF(CurCalYr.NE.RECSYear.OR.CURITR.NE.1) GOTO 2 !First time through ALLOCATE arrays

       ALLOCATE(NEQTSHR(RECSYear:EndYr+1,MNUMRTTY,mNumBldg,mNumCR))
       ALLOCATE(REQTSHR(RECSYear:EndYr+1,MNUMRTTY,mNumBldg,mNumCR))
       ALLOCATE(EQR90FUT  (RECSYear:EndYr,RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR-2))
       ALLOCATE(EQREPFUT  (RECSYear:EndYr,RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR-2))
       ALLOCATE(EQADDFUT  (RECSYear:EndYr,RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR-2))
       ALLOCATE(EQR90RPFUT(RECSYear:EndYr,RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR-2))
       ALLOCATE(EQCESEFUT (RECSYear:EndYr,RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR-2))
       ALLOCATE(RSNEFDB1(mNumYr,MNUMRTTY,mNumBldg,mNumCR-2))
       ALLOCATE(RSEEFDB1(mNumYr,MNUMRTTY,mNumBldg,mNumCR-2))                        !DYN
       ALLOCATE(HTSHELLEFFWT(RECSYear:EndYr,nHeatTypes,nShellTypes,mNumBldg,mNumCR-2))
       ALLOCATE(HTSHELLWT(RECSYear:EndYr,nHeatTypes,nShellTypes,mNumBldg,mNumCR-2))
       ALLOCATE(HSHELL(RECSYear:EndYr,nHeatClasses,mNumBldg,mNumCR-2))
       ALLOCATE(CSHELL(RECSYear:EndYr,nHeatClasses,mNumBldg,mNumCR-2))
       ALLOCATE(SHELLBUILDS(RECSYear:EndYr,nHeatTypes,nShellTypes,mNumBldg,mNumCR-2))
       ALLOCATE(SHELLINVEST (RECSYear:EndYr,nHeatClasses,nShellTypes,mNumBldg,mNumCR))
       ALLOCATE(SHELLSUBSIDY(RECSYear:EndYr,nHeatClasses,nShellTypes,mNumBldg,mNumCR))
       ALLOCATE(SHELLSUBSIDY111D(RECSYear:EndYr,nHeatClasses,nShellTypes,mNumBldg,mNumCR))
       ALLOCATE(CLSHELLWT(RECSYear:EndYr,nCoolTypes,mNumBldg,mNumCR-2))
       ALLOCATE(SHLEVELH(RECSYear:EndYr,nHeatClasses,nShellTypes,mNumBldg,mNumCR-2))
       ALLOCATE(HVEQSHR(RECSYear:EndYr,nHeatTypes,mNumBldg,mNumCR-2))
       ALLOCATE(HEATINGTYPEPURCH(RECSYear:EndYr,MNUMRTTY,mNumBldg,mNumCR-2,2))
       ALLOCATE(NEQTSHRC(RECSYear:EndYr,nCoolTypes,mNumBldg,mNumCR))
       ALLOCATE(LEARNFACT(mNumBldg,mNumCR-2))

 ! initialize ALLOCATED arrays to zero; otherwise, you get what is in the assigned memory !DYN
       NEQTSHR=0.0
       REQTSHR=0.0
       EQR90FUT=0.0
       EQREPFUT=0.0
       EQADDFUT=0.0
       EQR90RPFUT=0.0
       EQCESEFUT=0.0
       RSNEFDB1=0.0
       RSEEFDB1=0.0
       HTSHELLEFFWT=0.0
       HTSHELLWT=0.0
       HSHELL=0.0
       CSHELL=0.0
       SHELLBUILDS=0.0
       SHELLINVEST=0.0
       SHELLSUBSIDY=0.0
       SHELLSUBSIDY111D=0.0
       CLSHELLWT=0.0
       SHLEVELH=0.0
       HVEQSHR=0.0
       HEATINGTYPEPURCH=0.0
       NEQTSHRC=0.0
       LEARNFACT=0.0

2     CONTINUE
      ! End of dynamic array dimension assignments  !DYN

      RSYR=CurIYr+(BaseYr-1)
      PREVYR=CurIYr-1
      STEOBM=RTOVALUE("STEOBM  ",0)
      NRGBILL=RTOVALUE("NRGBILL  ",1)
      NRGBILL07=RTOVALUE("NRG2007  ",1)
      STIMULUS=RTOVALUE("STIMULUS  ",1)
      EPA111D=RTOVALUE("EPA111D ",0)

!*******************************************************************
!     READ DATA THE FIRST YEAR AND FIRST ITERATION
!*******************************************************************
      IF(CurCalYr.LT.RECSYear) RETURN
      IF(CurCalYr.EQ.RECSYear.AND.CURITR.EQ.1) THEN
        CALL RTEKREAD
      IF(IFMAX.NE.0) CALL PITCINIT
        CALL RDSQFOOT
        CALL DEGDAYREAD
!        IF(EPA111D.EQ.1) CALL BLDBASEREAD - read in all cases to allow for incremental difference calculations
        CALL BLDBASEREAD
        CALL RSUECSHLREAD
        CALL RSMELSREAD
        CALL RSSWITCHREAD
        CALL RSMISCREAD
        CALL RSMLGTREAD
        CALL RDRET
        CALL INTEQT
        CALL RDHTREQC
        CALL RDEFF
        CALL RDSTEFF
        CALL RDESTARHOMES
        CALL RDUECS
        CALL RCONSFL
        CALL RDISTGEN
      ENDIF

!*******************************************************************
!     STORE PRICES FOR EACH YEAR
!*******************************************************************
      IF (CurCalYr.GE.RECSYear) CALL RDPR

!*******************************************************************
!     BEGIN CALLING OTHER SUBROUTINES
!*******************************************************************
      IF (CurCalYr.EQ.RECSYear) THEN
         CALL EXCONS
         CALL RSBENCH
         CALL NEMSCN
         CALL RESDRP
      ELSE
        IF (MOD(CurCalYr,4).EQ.0.AND.CurCalYr.LE.LastSTEOYr) THEN  !MOD(A,P) computes the remainder of the division of A by P
           LEAPYR= 366.0/365.0
         ELSE
           LEAPYR= 365.0/365.0
        ENDIF

      IF(IFMAX.NE.0) CALL RSPITC(IFMAX, LastSTEOYr)

      CALL NEWHSE

!*******************************************************************
!     HEATING EQUIPMENT SUBROUTINES
!*******************************************************************
         CALL EPACTWD          ! EPACT window labeling
         CALL SQFTCALC         ! Average floorspace of housing and sqft adjustments for EUs
         CALL RSHVAC
         CALL RHTRTEC
         CALL RHTRADD

!*******************************************************************
!    DISTRIBUTED GENERATION SUBROUTINE
!*******************************************************************
         CALL RDISTGEN

!*******************************************************************
!     COOLING EQUIPMENT SUBROUTINES
!*******************************************************************
         CALL RCLTEC
         CALL RCLADD

!*******************************************************************
!    CLOTHES WASHER EQUIPMENT SUBROUTINES
!*******************************************************************
         CALL RCWTEC
         CALL RCWADD

!*******************************************************************
!     DISHWASHER EQUIPMENT SUBROUTINES
!*******************************************************************
         CALL RDWTEC
         CALL RDWADD

!*******************************************************************
!     WATER HEATING EQUIPMENT SUBROUTINES
!*******************************************************************
         EU = 5        ! EU = 5 is water heaters; this is used by REUADD subroutine
         CALL RWHTEC
         CALL REUADD

!*******************************************************************
!     COOKING EQUIPMENT SUBROUTINES
!*******************************************************************
         EU = 6        ! EU = 6 is cooking; this is used by REUADD subroutine
         CALL RSTVTEC
         CALL REUADD

!*******************************************************************
!     DRYING EQUIPMENT SUBROUTINES
!*******************************************************************
         CALL RDRYTEC
         CALL RDRYADD

!*******************************************************************
!     REFRIGERATING EQUIPMENT SUBROUTINES
!*******************************************************************
         CALL RREFTEC
         CALL RREFADD

!*******************************************************************
!     FREEZER EQUIPMENT SUBROUTINES
!*******************************************************************
         CALL RFRZTEC
         CALL RFRZADD

!*******************************************************************
!     CONSUMPTION SUBROUTINES
!*******************************************************************
         CALL RHTRCON
         CALL RCLCON
         CALL RCWCON
         CALL RDWCON
         CALL RWHCON
         CALL RSTOVCON
         CALL RDRYCON
         CALL RREFCON
         CALL RFRZCON

!*******************************************************************
!     LIGHTING, SECONDARY HEATING, & SMALL APPLIANCE SUBROUTINES
!*******************************************************************
         CALL LTCNS
         CALL APCNS
         CALL SHTCNS
         CALL APPCNS

!*******************************************************************
!     NEMS CONSUMPTION SUBROUTINE
!*******************************************************************
         CALL FUELCN
         CALL RSBENCH
         CALL NEMSCN
         CALL RESDRP
      !IF(EPA111D.EQ.1) CALL CALC111D      !111(D)
         CALL CALC111D      !EEcosts
       ENDIF  !check CurCalYr >= RECSYear

!*******************************************************************
!     REPORTING SUBROUTINES
!*******************************************************************
      IF ((CurCalYr-(BaseYr-1).EQ.LastYr).AND.(FCRL.EQ.1)) THEN
        CALL NHTSHR
        CALL RESDRP2
        CALL RESDBOUT
      ENDIF
      CONTAINS

!***********************************************************  !ExcelRead - replaces old code to read in text files
!  RTEKREAD READS THE RESIDENTIAL MODULE TECHNOLOGY MENUS  *
!    RSCLASS                                               *
!    RSMEQP                                                *
!    RSMSHL                                                *
!***********************************************************
SUBROUTINE RTEKREAD
  IMPLICIT NONE

  ! These are for printing out the read-in named range data for debugging purposes
  COMMON/NEMSWK1/XMLOUT

  INTEGER FILE_MGR          ! File manager
  INTEGER*4 INFILE,       & ! File handle
    I,xlI,NewDiv,NewRowI,J,K,Type,  & ! General indices  !EqpDivCopy
    Y,D,R,T,B, &
    LASTEU,LASTCLAS,LASTTYPE

  INTEGER XMLOUT

  INTEGER*4 ClsRecords,EqpRecords,NewEqpRecords,ShlRecords !number/count of records in the RSCLASS, RSMEQP, and RSMSHL tabs of input file, calculated and read-in from input file

  INTEGER*2, ALLOCATABLE :: xlRTCLENDU(:),xlRTCLEQCL(:),xlRTCLTYPT(:),xlRTCLPNTR(:),xlRTCLREPL(:),xlRTFUEL(:),xlRTFFAN(:),xlRTMINLIF(:),xlRTMAXLIF(:), & !RSCLASS
                            xlRTTYENDU(:),xlRTTYEQCL(:),xlRTEQTYPE(:),xlRTINITYR(:),xlRTLASTYR(:),xlRTCENDIV(:),xlHVACPNTR(:),xlRTTYPNTR(:),           & !RSMEQP
                            xlRSCENDIV(:),xlRSBTYPE(:),xlHVHTEQCL(:),xlHVHTEQTY(:),xlHVCLEQCL(:),xlHVCLEQTY(:),xlHVFYEAR(:),xlHVLYEAR(:),xlHVPACKG(:)    !RSMSHL

  CHARACTER*10, ALLOCATABLE :: xlRTCLNAME(:), & !RSCLASS
                               xlRTMATURE(:), & !RSMEQP
                               xlRTTYNAME(:)    !RSMEQP

  CHARACTER*14, ALLOCATABLE :: xlHVPGNAME(:)    !RSMSHL

  REAL*4, ALLOCATABLE :: xlRTALPHA(:),xlRTBASEFF(:),xlRTK(:),xlRTLAMBDA(:),xlRTFCBETA(:),xlRTSWFACT(:),xlRTSWBETA(:),xlRTSWBIAS(:),                            & !RSCLASS
                         xlCWMEF(:),xlLOADADJ(:),xlRTEQEFF(:),xlRTEQCOST(:),xlRTRECOST(:),xlRTEQSUB(:),xlRTRESUB(:),xlRTEQSUBN(:),xlRTRESUBN(:),               & !RSMEQP
                          xlRTEQSUB111D(:),xlRTRESUB111D(:),xlRTCOSTP1(:),xlRTCOSTP2(:),xlRTCOSTP3(:),xlRTECBTA1(:),xlRTECBTA2(:),xlRTECBTA3(:),xlRTECBIAS(:), & !RSMEQP
                         xlHVHEATFACTOR(:),xlHVCOOLFACTOR(:),xlHTSHEFF(:),xlCLSHEFF(:),xlHTSHBASE(:),xlCLSHBASE(:),xlSHELCOST(:),xlSHELSUB(:),xlSHELSUB111D(:)   !RSMSHL

  ! ALLOCATE dynamic arrays
  ALLOCATE (xlRTCLENDU(mNumRTCl),xlRTCLEQCL(mNumRTCl),xlRTCLTYPT(mNumRTCl),xlRTCLPNTR(mNumRTCl),xlRTCLREPL(mNumRTCl),xlRTFUEL(mNumRTCl),xlRTFFAN(mNumRTCl),          & !RSCLASS
             xlRTMINLIF(mNumRTCl),xlRTMAXLIF(mNumRTCl),xlRTCLNAME(mNumRTCl),xlRTALPHA(mNumRTCl),xlRTBASEFF(mNumRTCl),xlRTK(mNumRTCl),xlRTLAMBDA(mNumRTCl),xlRTFCBETA(mNumRTCl), & !RSCLASS
             xlRTSWFACT(mNumRTCl),xlRTSWBETA(mNumRTCl),xlRTSWBIAS(mNumRTCl),                                                                                         & !RSCLASS
            xlRTTYENDU(MNUMRTTY),xlRTTYEQCL(MNUMRTTY),xlRTEQTYPE(MNUMRTTY),xlRTINITYR(MNUMRTTY),xlRTLASTYR(MNUMRTTY),xlRTCENDIV(MNUMRTTY),xlHVACPNTR(MNUMRTTY)       & !RSMEQP
             ,xlRTTYPNTR(MNUMRTTY),xlRTTYNAME(MNUMRTTY),xlRTMATURE(MNUMRTTY),xlCWMEF(MNUMRTTY),xlLOADADJ(MNUMRTTY),xlRTEQEFF(MNUMRTTY),xlRTEQCOST(MNUMRTTY),         & !RSMEQP
             xlRTRECOST(MNUMRTTY),xlRTEQSUB(MNUMRTTY),xlRTRESUB(MNUMRTTY),xlRTEQSUBN(MNUMRTTY),xlRTRESUBN(MNUMRTTY),xlRTEQSUB111D(MNUMRTTY),xlRTRESUB111D(MNUMRTTY), & !RSMEQP
             xlRTCOSTP1(MNUMRTTY),xlRTCOSTP2(MNUMRTTY),xlRTCOSTP3(MNUMRTTY),xlRTECBTA1(MNUMRTTY),xlRTECBTA2(MNUMRTTY),xlRTECBTA3(MNUMRTTY),xlRTECBIAS(MNUMRTTY),     & !RSMEQP
            xlRSCENDIV(MNUMHVAC),xlRSBTYPE(MNUMHVAC),xlHVHTEQCL(MNUMHVAC),xlHVHTEQTY(MNUMHVAC),xlHVCLEQCL(MNUMHVAC),xlHVCLEQTY(MNUMHVAC),xlHVFYEAR(MNUMHVAC),        & !RSMSHL
             xlHVLYEAR(MNUMHVAC),xlHVPACKG(MNUMHVAC),xlHVPGNAME(MNUMHVAC),xlHVHEATFACTOR(MNUMHVAC),xlHVCOOLFACTOR(MNUMHVAC),xlHTSHEFF(MNUMHVAC),xlCLSHEFF(MNUMHVAC), & !RSMSHL
             xlHTSHBASE(MNUMHVAC),xlCLSHBASE(MNUMHVAC),xlSHELCOST(MNUMHVAC),xlSHELSUB(MNUMHVAC),xlSHELSUB111D(MNUMHVAC))                                               !RSMSHL

  ! Initialize ALLOCATED arrays to zero; otherwise, you get what is in the assigned memory
  xlRTCLENDU=0; xlRTCLEQCL=0; xlRTCLTYPT=0; xlRTCLPNTR=0; xlRTCLREPL=0; xlRTFUEL=0; xlRTFFAN=0; xlRTMINLIF=0; xlRTMAXLIF=0 !RSCLASS
   xlRTCLNAME=""; xlRTALPHA=0; xlRTBASEFF=0; xlRTK=0; xlRTLAMBDA=0; xlRTFCBETA=0; xlRTSWFACT=0; xlRTSWBETA=0; xlRTSWBIAS=0 !RSCLASS
  xlRTTYENDU=0; xlRTTYEQCL=0; xlRTEQTYPE=0; xlRTINITYR=0; xlRTLASTYR=0; xlRTCENDIV=0; xlHVACPNTR=0; xlRTTYPNTR=0; xlRTTYNAME=""; xlRTMATURE=""      !RSMEQP
   xlCWMEF=0; xlLOADADJ=0; xlRTEQEFF=0; xlRTEQCOST=0; xlRTRECOST=0; xlRTEQSUB=0; xlRTRESUB=0; xlRTEQSUBN=0; xlRTRESUBN=0              !RSMEQP
   xlRTEQSUB111D=0; xlRTRESUB111D=0; xlRTCOSTP1=0; xlRTCOSTP2=0; xlRTCOSTP3=0; xlRTECBTA1=0; xlRTECBTA2=0; xlRTECBTA3=0; xlRTECBIAS=0 !RSMEQP
  xlRSCENDIV=0; xlRSBTYPE=0; xlHVHTEQCL=0; xlHVHTEQTY=0; xlHVCLEQCL=0; xlHVCLEQTY=0; xlHVFYEAR=0; xlHVLYEAR=0; xlHVPACKG=0; xlHVPGNAME="" !RSMSHL
   xlHVHEATFACTOR=0; xlHVCOOLFACTOR=0; xlHTSHEFF=0; xlCLSHEFF=0; xlHTSHBASE=0; xlCLSHBASE=0; xlSHELCOST=0; xlSHELSUB=0; xlSHELSUB111D=0   !RSMSHL

  DO I=1,MNUMENDU
    RTCLEUPT(I) = 0 ! Last record # in RSCLASS for each end use
    RTTYEUPT(I) = 0 ! Last record # in RSMEQP for each end use
    RTTYPECT(I) = 0 ! Last type # in type arrays for each end use
  ENDDO

  LASTCLAS=-1
  LASTEU  =-1
  LASTTYPE=-1

  RTCLCNT = 0
  RTEUCNT = 0
  RTTYCNT = 0

  NewDiv = 0  !Used to increment expanded RSMEQP CD from 11 to 1-9  !EqpDivCopy
  NewRowI = 0  !Counts number of "new rows" endogenously inserted into  !EqpDivCopy

  ! Read technology input data for RSCLASS, RSMEQP, and RSMSHL from RSMESS.xlsx Excel workbook

  ! Open input workbook using file manager
  INFILE= FILE_MGR ('O','RSMESS',.FALSE.)

  ! Turn on debugging WRITE-out of read-in named ranges to RSXLSDBG.txt output file if scedes value PRTDBGR=1
  IF (PRTDBGR.EQ.1) THEN
    XMLOUT = FILE_MGR('O','RSXLSDBG',.TRUE.)
  ENDIF

  ! Call subroutine to read all defined ranges from worksheets in workbook
  !  This stores the ranges in a temporary data area that can get overwritten by the next model if they use it.
  !  All ranges have to be extracted from the temporary area immediately.
  CALL ReadRngXLSX(infile,'RSCLASS') !read worksheet named 'RSCLASS' in Excel workbook RSMESS.xlsx

  !*****************************************************************************************************
  ! Copy each range from worksheet data area to variables
  !  Example: ('Variable_Description',VariableName,#rows_of_data,#columns_of_data,1)
  !
  !  GETRNGI: Copies an INTEGER*2 variable from the worksheet data area into the variable.
  !           -The variable dimensions are passed as the 3rd, 4th, and 5th arguments, (e.g., ... 1,1,1).
  !           -A variable with dimesions of 1,1,1 is a scalar (e.g., ClsRecords).
  !           -A variable with dimensions of n,1,1 is a one-dimensional array with n elements
  !  GETRNGR: Copies a REAL variable from the worksheet data area into the variable.
  !  GETRNGC: Copies a CHARACTER variable from the worksheet data area into the variable.
  !           Max string length is set in wk1block includes file (i.e., MaxStrings=30000).
  !*****************************************************************************************************

  ! Read in record count from spreadsheet calculation
  CALL GETRNGI('ClsRecords        ',ClsRecords,1,1,1)          !RSCLASS

  ! Read the values of RSCLASS, later to be parsed into NEMS residential variables
  !  Dimensioned by technology, vintage, etc.
  CALL GETRNGI('xlRTCLENDU        ',xlRTCLENDU,ClsRecords,1,1) !RSCLASS
  CALL GETRNGI('xlRTCLEQCL        ',xlRTCLEQCL,ClsRecords,1,1) !RSCLASS
  CALL GETRNGI('xlRTCLTYPT        ',xlRTCLTYPT,ClsRecords,1,1) !RSCLASS
  CALL GETRNGI('xlRTCLPNTR        ',xlRTCLPNTR,ClsRecords,1,1) !RSCLASS
  CALL GETRNGI('xlRTCLREPL        ',xlRTCLREPL,ClsRecords,1,1) !RSCLASS
  CALL GETRNGI('xlRTFUEL          ',xlRTFUEL  ,ClsRecords,1,1) !RSCLASS
  CALL GETRNGI('xlRTFFAN          ',xlRTFFAN  ,ClsRecords,1,1) !RSCLASS
  CALL GETRNGR('xlRTBASEFF        ',xlRTBASEFF,ClsRecords,1,1) !RSCLASS
  CALL GETRNGR('xlRTALPHA         ',xlRTALPHA ,ClsRecords,1,1) !RSCLASS
  CALL GETRNGI('xlRTMINLIF        ',xlRTMINLIF,ClsRecords,1,1) !RSCLASS
  CALL GETRNGI('xlRTMAXLIF        ',xlRTMAXLIF,ClsRecords,1,1) !RSCLASS
  CALL GETRNGR('xlRTK             ',xlRTK     ,ClsRecords,1,1) !RSCLASS
  CALL GETRNGR('xlRTLAMBDA        ',xlRTLAMBDA,ClsRecords,1,1) !RSCLASS
  CALL GETRNGR('xlRTFCBETA        ',xlRTFCBETA,ClsRecords,1,1) !RSCLASS
  CALL GETRNGR('xlRTSWFACT        ',xlRTSWFACT,ClsRecords,1,1) !RSCLASS
  CALL GETRNGR('xlRTSWBETA        ',xlRTSWBETA,ClsRecords,1,1) !RSCLASS
  CALL GETRNGR('xlRTSWBIAS        ',xlRTSWBIAS,ClsRecords,1,1) !RSCLASS
  CALL GETRNGC('xlRTCLNAME        ',xlRTCLNAME,ClsRecords,1,1) !RSCLASS

  ! Convert Excel named ranges to NEMS variables

  DO I = 1,ClsRecords           !RSCLASS
    RTCLENDU(I) = xlRTCLENDU(I) !RSCLASS
    RTCLEQCL(I) = xlRTCLEQCL(I) !RSCLASS
    RTCLTYPT(I) = xlRTCLTYPT(I) !RSCLASS
    RTCLPNTR(I) = xlRTCLPNTR(I) !RSCLASS
    RTCLREPL(I) = xlRTCLREPL(I) !RSCLASS
    RTFUEL(I) = xlRTFUEL(I)     !RSCLASS
    RTFFAN(I) = xlRTFFAN(I)     !RSCLASS
    RTBASEFF(RECSYear,I) = xlRTBASEFF(I) !RSCLASS
    RTALPHA(I) = xlRTALPHA(I)   !RSCLASS
    RTMINLIF(I) = xlRTMINLIF(I) !RSCLASS
    RTMAXLIF(I) = xlRTMAXLIF(I) !RSCLASS
    RTK(I) = xlRTK(I)           !RSCLASS
    RTLAMBDA(I) = xlRTLAMBDA(I) !RSCLASS
    RTFCBETA(I) = xlRTFCBETA(I) !RSCLASS
    RTSWFACT(I) = xlRTSWFACT(I) !RSCLASS
    RTSWBETA(I) = xlRTSWBETA(I) !RSCLASS
    RTSWBIAS(I) = xlRTSWBIAS(I) !RSCLASS
    RTCLNAME(I) = xlRTCLNAME(I) !RSCLASS

    ! Processing RSCLASS data after being read in

    ! Un-commenting this code turns off switching for a quick test of how much actually goes on.
    !  RTSWFACT(I) = 0.

    RTCLCNT=RTCLCNT+1
    J=RTCLENDU(I)
    IF(J.NE.LASTEU) RTEUCNT=RTEUCNT+1

    ! Collect the raw data to compute the RSCLASS end-use pointers
      RTCLEUPT(J+1)=RTCLEUPT(J+1)+1 ! RTCLEUPT(I) is the last record # in RSCLASS for each end use
      LASTEU=J

  ENDDO !ClsRecords

  ! Use raw data to assign pointers
  DO J=1,RTEUCNT
    RTCLEUPT(J+1)=RTCLEUPT(J+1)+RTCLEUPT(J)
  ENDDO

  ! If RSCLASS read was successful, print summary inFORMATion to unit 6 (nohup.out)
  WRITE(6,*) 'RESDMSG SUB_RTEKREAD RSCLASS EOF REACHED OK; COUNT = ',ClsRecords

  CALL ReadRngXLSX(infile,'RSMEQP')  !read worksheet named 'RSMEQP' in Excel workbook RSMESS.xlsx

  LASTEU=-1 ! Resets counter for value of last end use

  ! Read in record count from spreadsheet calculation
  CALL GETRNGI('EqpRecords        ',EqpRecords,1,1,1)          !RSMEQP

  ! Read the values of RSMEQP, later to be parsed into NEMS residential variables
  !  Dimensioned by technology, vintage, etc.
  CALL GETRNGI('RTEKDOLLARYR      ',RTEKDOLLARYR,1,1,1)        !RSMEQP
  CALL GETRNGI('xlRTTYENDU        ',xlRTTYENDU,EqpRecords,1,1) !RSMEQP
  CALL GETRNGI('xlRTTYEQCL        ',xlRTTYEQCL,EqpRecords,1,1) !RSMEQP
  CALL GETRNGI('xlRTEQTYPE        ',xlRTEQTYPE,EqpRecords,1,1) !RSMEQP
  CALL GETRNGI('xlRTINITYR        ',xlRTINITYR,EqpRecords,1,1) !RSMEQP
  CALL GETRNGI('xlRTLASTYR        ',xlRTLASTYR,EqpRecords,1,1) !RSMEQP
  CALL GETRNGI('xlRTCENDIV        ',xlRTCENDIV,EqpRecords,1,1) !RSMEQP
  CALL GETRNGI('xlHVACPNTR        ',xlHVACPNTR,EqpRecords,1,1) !RSMEQP
  CALL GETRNGI('xlRTTYPNTR        ',xlRTTYPNTR,EqpRecords,1,1) !RSMEQP
  CALL GETRNGR('xlCWMEF           ',xlCWMEF   ,EqpRecords,1,1) !RSMEQP
  CALL GETRNGR('xlLOADADJ         ',xlLOADADJ ,EqpRecords,1,1) !RSMEQP
  CALL GETRNGR('xlRTEQEFF         ',xlRTEQEFF ,EqpRecords,1,1) !RSMEQP
  CALL GETRNGR('xlRTEQCOST        ',xlRTEQCOST,EqpRecords,1,1) !RSMEQP
  CALL GETRNGR('xlRTRECOST        ',xlRTRECOST,EqpRecords,1,1) !RSMEQP
  CALL GETRNGR('xlRTEQSUB         ',xlRTEQSUB ,EqpRecords,1,1) !RSMEQP
  CALL GETRNGR('xlRTRESUB         ',xlRTRESUB ,EqpRecords,1,1) !RSMEQP
  CALL GETRNGR('xlRTEQSUBN        ',xlRTEQSUBN,EqpRecords,1,1) !RSMEQP
  CALL GETRNGR('xlRTRESUBN        ',xlRTRESUBN,EqpRecords,1,1) !RSMEQP
  CALL GETRNGR('xlRTEQSUB111D     ',xlRTEQSUB111D,EqpRecords,1,1) !RSMEQP
  CALL GETRNGR('xlRTRESUB111D     ',xlRTRESUB111D,EqpRecords,1,1) !RSMEQP
  CALL GETRNGC('xlRTMATURE        ',xlRTMATURE,EqpRecords,1,1) !RSMEQP
  CALL GETRNGR('xlRTCOSTP1        ',xlRTCOSTP1,EqpRecords,1,1) !RSMEQP
  CALL GETRNGR('xlRTCOSTP2        ',xlRTCOSTP2,EqpRecords,1,1) !RSMEQP
  CALL GETRNGR('xlRTCOSTP3        ',xlRTCOSTP3,EqpRecords,1,1) !RSMEQP
  CALL GETRNGR('xlRTECBTA1        ',xlRTECBTA1,EqpRecords,1,1) !RSMEQP
  CALL GETRNGR('xlRTECBTA2        ',xlRTECBTA2,EqpRecords,1,1) !RSMEQP
  CALL GETRNGR('xlRTECBTA3        ',xlRTECBTA3,EqpRecords,1,1) !RSMEQP
  CALL GETRNGR('xlRTECBIAS        ',xlRTECBIAS,EqpRecords,1,1) !RSMEQP
  CALL GETRNGC('xlRTTYNAME        ',xlRTTYNAME,EqpRecords,1,1) !RSMEQP

  ! Convert Excel named ranges to NEMS variables and expand all instances where a single RSMEQP input row represents CD 1-9 (i.e., CD=11)
  ! NOTE: The xl input arrays only go up to EqpRecords but are ALLOCATED up to MNUMRTTY from RTEK (so MNUMRTTY could be 9*EqpRecords from RSMEQP to generously ensure proper array size)

    NewEqpRecords = EqpRecords  !Used to increment expanded RSMEQP CD from 11 to 1-9  !EqpDivCopy

    DO xlI = 1,EqpRecords  !RSMEQP  !EqpDivCopy
      I=xlI+NewRowI        !RSMEQP  !Used to skip over the newly expanded CD data when pulling next data from Excel named range  !EqpDivCopy
      IF (xlRTCENDIV(xlI) .EQ. 11) THEN !RSMEQP  !If CD=11, the row will be "copied" eight more times and each of the census divisions will be renumbered 1-9  !EqpDivCopy
        DO I = I,I+8       !RSMEQP  !Adding 8 represents the 8 new "rows" that are being endogenously added to expand from one CD row to nine CD rows in RSMEQP  !EqpDivCopy
          NewDiv=NewDiv+1  !RSMEQP  !Used to increment expanded CD from 11 to 1-9  !EqpDivCopy
          RTTYENDU(I) = xlRTTYENDU(xlI) !RSMEQP  !EqpDivCopy
          RTTYEQCL(I) = xlRTTYEQCL(xlI) !RSMEQP  !EqpDivCopy
          RTEQTYPE(I) = xlRTEQTYPE(xlI) !RSMEQP  !EqpDivCopy
          RTINITYR(I) = xlRTINITYR(xlI) !RSMEQP  !EqpDivCopy
          RTLASTYR(I) = xlRTLASTYR(xlI) !RSMEQP  !EqpDivCopy
          RTCENDIV(I) = NewDiv          !RSMEQP  !Used to increment expanded CD from 11 to 1-9  !EqpDivCopy
          HVACPNTR(I) = xlHVACPNTR(xlI) !RSMEQP  !EqpDivCopy
          RTTYPNTR(I) = xlRTTYPNTR(xlI) !RSMEQP  !EqpDivCopy
          CWMEF(I) = xlCWMEF(xlI)       !RSMEQP  !EqpDivCopy
          LOADADJ(I) = xlLOADADJ(xlI)   !RSMEQP  !EqpDivCopy
          RTEQEFF(I) = xlRTEQEFF(xlI)   !RSMEQP  !EqpDivCopy
          RTEQCOST(I) = xlRTEQCOST(xlI) !RSMEQP  !EqpDivCopy
          RTRECOST(I) = xlRTRECOST(xlI) !RSMEQP  !EqpDivCopy
          RTEQSUB(I) = xlRTEQSUB(xlI)   !RSMEQP  !EqpDivCopy
          RTRESUB(I) = xlRTRESUB(xlI)   !RSMEQP  !EqpDivCopy
          RTEQSUBN(I) = xlRTEQSUBN(xlI) !RSMEQP  !EqpDivCopy
          RTRESUBN(I) = xlRTRESUBN(xlI) !RSMEQP  !EqpDivCopy
          RTEQSUB111D(I) = xlRTEQSUB111D(xlI) !RSMEQP  !EqpDivCopy
          RTRESUB111D(I) = xlRTRESUB111D(xlI) !RSMEQP  !EqpDivCopy
          RTMATURE(I) = xlRTMATURE(xlI) !RSMEQP  !EqpDivCopy
          RTCOSTP1(I) = xlRTCOSTP1(xlI) !RSMEQP  !EqpDivCopy
          RTCOSTP2(I) = xlRTCOSTP2(xlI) !RSMEQP  !EqpDivCopy
          RTCOSTP3(I) = xlRTCOSTP3(xlI) !RSMEQP  !EqpDivCopy
          RTECBTA1(I) = xlRTECBTA1(xlI) !RSMEQP  !EqpDivCopy
          RTECBTA2(I) = xlRTECBTA2(xlI) !RSMEQP  !EqpDivCopy
          RTECBTA3(I) = xlRTECBTA3(xlI) !RSMEQP  !EqpDivCopy
          RTECBIAS(I) = xlRTECBIAS(xlI) !RSMEQP  !EqpDivCopy
          RTTYNAME(I) = xlRTTYNAME(xlI) !RSMEQP  !EqpDivCopy
        ENDDO !I  !RSMEQP  !Adding 8 represents the 8 new "rows" that are being endogenously "inserted" to expand from 1 CD row to 9 CD rows in RSMEQP  !EqpDivCopy
        NewDiv=0  !RSMEQP  !Used to reset increment for expanding CD from 11 to 1-9  !EqpDivCopy
        NewRowI=NewRowI+8  !RSMEQP  !Used to skip over the newly expanded CD data when pulling next data from Excel named range  !EqpDivCopy
        NewEqpRecords=NewEqpRecords+8  !Adding 8 represents the 8 new "rows" that are being endogenously "inserted" each time a "row" is endogenously "expanded" from 1 CD row to 9 CD rows in RSMEQP  !EqpDivCopy
      ELSEIF (xlRTCENDIV(xlI) .NE. 11) THEN  !If not a national row, THEN use the row from RSMEQP as entered into RSMESS.xlsx  !EqpDivCopy
        RTTYENDU(I) = xlRTTYENDU(xlI) !RSMEQP  !EqpDivCopy
        RTTYEQCL(I) = xlRTTYEQCL(xlI) !RSMEQP  !EqpDivCopy
        RTEQTYPE(I) = xlRTEQTYPE(xlI) !RSMEQP  !EqpDivCopy
        RTINITYR(I) = xlRTINITYR(xlI) !RSMEQP  !EqpDivCopy
        RTLASTYR(I) = xlRTLASTYR(xlI) !RSMEQP  !EqpDivCopy
        RTCENDIV(I) = xlRTCENDIV(xlI) !RSMEQP  !EqpDivCopy
        HVACPNTR(I) = xlHVACPNTR(xlI) !RSMEQP  !EqpDivCopy
        RTTYPNTR(I) = xlRTTYPNTR(xlI) !RSMEQP  !EqpDivCopy
        CWMEF(I) = xlCWMEF(xlI)       !RSMEQP  !EqpDivCopy
        LOADADJ(I) = xlLOADADJ(xlI)   !RSMEQP  !EqpDivCopy
        RTEQEFF(I) = xlRTEQEFF(xlI)   !RSMEQP  !EqpDivCopy
        RTEQCOST(I) = xlRTEQCOST(xlI) !RSMEQP  !EqpDivCopy
        RTRECOST(I) = xlRTRECOST(xlI) !RSMEQP  !EqpDivCopy
        RTEQSUB(I) = xlRTEQSUB(xlI)   !RSMEQP  !EqpDivCopy
        RTRESUB(I) = xlRTRESUB(xlI)   !RSMEQP  !EqpDivCopy
        RTEQSUBN(I) = xlRTEQSUBN(xlI) !RSMEQP  !EqpDivCopy
        RTRESUBN(I) = xlRTRESUBN(xlI) !RSMEQP  !EqpDivCopy
        RTEQSUB111D(I) = xlRTEQSUB111D(xlI) !RSMEQP  !EqpDivCopy
        RTRESUB111D(I) = xlRTRESUB111D(xlI) !RSMEQP  !EqpDivCopy
        RTMATURE(I) = xlRTMATURE(xlI) !RSMEQP  !EqpDivCopy
        RTCOSTP1(I) = xlRTCOSTP1(xlI) !RSMEQP  !EqpDivCopy
        RTCOSTP2(I) = xlRTCOSTP2(xlI) !RSMEQP  !EqpDivCopy
        RTCOSTP3(I) = xlRTCOSTP3(xlI) !RSMEQP  !EqpDivCopy
        RTECBTA1(I) = xlRTECBTA1(xlI) !RSMEQP  !EqpDivCopy
        RTECBTA2(I) = xlRTECBTA2(xlI) !RSMEQP  !EqpDivCopy
        RTECBTA3(I) = xlRTECBTA3(xlI) !RSMEQP  !EqpDivCopy
        RTECBIAS(I) = xlRTECBIAS(xlI) !RSMEQP  !EqpDivCopy
        RTTYNAME(I) = xlRTTYNAME(xlI) !RSMEQP  !EqpDivCopy
      ENDIF  !xlRTCENDIV  !EqpDivCopy
    ENDDO  !xlI  !EqpDivCopy

    ! Write out expanded RSMEQP inputs to same debug file (RSXLSDBG.txt) that Excel named ranges are written into.  !EqpDivCopy
    IF (PRTDBGR.EQ.1) THEN
      WRITE(XMLOUT,9998) 'I ', 'RTTYENDU(I) ', 'RTTYEQCL(I) ', 'RTEQTYPE(I) ', 'RTINITYR(I) ', 'RTLASTYR(I) ', 'RTCENDIV(I) ', 'HVACPNTR(I) ', 'RTTYPNTR(I) ', 'CWMEF(I) ', 'LOADADJ(I) ', 'RTEQEFF(I) ', 'RTEQCOST(I) ', &
       'RTRECOST(I) ', 'RTEQSUB(I) ', 'RTRESUB(I) ', 'RTEQSUBN(I) ', 'RTRESUBN(I) ', 'RTEQSUB111D(I) ', 'RTRESUB111D(I) ', 'RTMATURE(I) ', 'RTCOSTP1(I) ', 'RTCOSTP2(I) ', 'RTCOSTP3(I) ', 'RTECBTA1(I) ', 'RTECBTA2(I) ', &
       'RTECBTA3(I) ', 'RTECBIAS(I) ', 'RTTYNAME(I) '
      DO I = 1,NewEqpRecords
        WRITE(XMLOUT,9999) I, RTTYENDU(I), RTTYEQCL(I), RTEQTYPE(I), RTINITYR(I), RTLASTYR(I), RTCENDIV(I), HVACPNTR(I), RTTYPNTR(I), CWMEF(I), LOADADJ(I), RTEQEFF(I), RTEQCOST(I), &
         RTRECOST(I), RTEQSUB(I), RTRESUB(I), RTEQSUBN(I), RTRESUBN(I), RTEQSUB111D(I), RTRESUB111D(I), RTMATURE(I), RTCOSTP1(I), RTCOSTP2(I), RTCOSTP3(I), RTECBTA1(I), RTECBTA2(I), &
         RTECBTA3(I), RTECBIAS(I), RTTYNAME(I)
      ENDDO  !I
    9998 FORMAT(29A)
    9999 FORMAT(9(I4,' '),11(F8.3,' '),A,' ',7(F8.3,' '),A)
    ENDIF

  DO I = 1,NewEqpRecords  !EqpDivCopy
    ! Immediately subtract subsidy value from equipment costs
    RTEQCOST(I)=RTEQCOST(I)-RTEQSUB(I)-RTEQSUBN(I)-FLOAT(EPA111D)*RTEQSUB111D(I) !Utility_invest
    RTRECOST(I)=RTRECOST(I)-RTRESUB(I)-RTRESUBN(I)-FLOAT(EPA111D)*RTRESUB111D(I) !Utility_invest

    ! Create variables for HVAC subroutine
    DO Y=RECSYear,EndYr
    ! HVACPNTR counts equipment types across heating and cooling.
    ! Note that the first cooling type is numbered nHeatTypes+1 because heating technologies are listed before cooling
    !  for heating HVACPNTR = RTEQTYPE
    !  for cooling HVACPNTR = RTEQTYPE + nHeatTypes (current number of heating system types)
      IF (HVACPNTR(I).GT.nHeatTypes) THEN
        Type=HVACPNTR(I)-nHeatTypes
        D=RTCENDIV(I)
        IF (Y.GE.RTINITYR(I).AND.Y.LE.RTLASTYR(I)) THEN
          ACEFF(Type,Y,D)=RTEQEFF(I)
          ACICOST(Type,Y,D)=RTEQCOST(I)
        ENDIF !Y
      ENDIF !HVACPNTR
    ENDDO !Y

    RTTYCNT=RTTYCNT+1
    J=RTTYENDU(I)

    ! Collect the raw data to compute the RSMEQP end-use pointers
    RTTYEUPT(J+1)=RTTYEUPT(J+1)+1 ! RTTYEUPT(I) is the last record # in RSMEQP for each end use
    K=RTEQTYPE(I)
    IF(J.NE.LASTEU.OR.K.NE.LASTTYPE)RTTYPECT(J+1)=RTTYPECT(J+1)+1 ! RTTYPECT(I) is the last type # in type arrays for each end use
    LASTEU=J
    LASTTYPE=K

  ENDDO !NewEqpRecords !EqpDivCopy

  !Use raw data to assign pointers
  DO J=1,RTEUCNT
    RTTYEUPT(J+1)=RTTYEUPT(J+1)+RTTYEUPT(J)
    RTTYPECT(J+1)=RTTYPECT(J+1)+RTTYPECT(J)
  ENDDO

  ! If RSMEQP read was successful, print summary inFORMATion to unit 6 (nohup.out)
  WRITE(6,*) 'RESDMSG SUB_RTEKREAD RSMEQP EOF REACHED OK; COUNT = ',EqpRecords  !reports number of records read-in from RSMEQP rather than counting the expanded CD rows created endogenously

  CALL ReadRngXLSX(infile,'RSMSHL')  !read worksheet named 'RSMSHL' in Excel workbook RSMESS.xlsx

  ! Read in record count from spreadsheet calculation
  CALL GETRNGI('ShlRecords      ',ShlRecords,1,1,1)             !RSMSHL

  ! Read the values of RSMSHL, later to be parsed into NEMS residential variables
  !  Dimensioned by technology, vintage, etc.
  CALL GETRNGI('RSHLdollarYR      ',RSHLdollarYR,1,1,1)         !RSMSHL
  CALL GETRNGI('xlRSCENDIV        ',xlRSCENDIV,ShlRecords,1,1)  !RSMSHL
  CALL GETRNGI('xlRSBTYPE         ',xlRSBTYPE ,ShlRecords,1,1)  !RSMSHL
  CALL GETRNGI('xlHVHTEQCL        ',xlHVHTEQCL,ShlRecords,1,1)  !RSMSHL
  CALL GETRNGI('xlHVHTEQTY        ',xlHVHTEQTY,ShlRecords,1,1)  !RSMSHL
  CALL GETRNGI('xlHVCLEQCL        ',xlHVCLEQCL,ShlRecords,1,1)  !RSMSHL
  CALL GETRNGI('xlHVCLEQTY        ',xlHVCLEQTY,ShlRecords,1,1)  !RSMSHL
  CALL GETRNGI('xlHVFYEAR         ',xlHVFYEAR ,ShlRecords,1,1)  !RSMSHL
  CALL GETRNGI('xlHVLYEAR         ',xlHVLYEAR ,ShlRecords,1,1)  !RSMSHL
  CALL GETRNGR('xlHVHEATFACTOR    ',xlHVHEATFACTOR,ShlRecords,1,1) !RSMSHL
  CALL GETRNGR('xlHVCOOLFACTOR    ',xlHVCOOLFACTOR,ShlRecords,1,1) !RSMSHL
  CALL GETRNGR('xlHTSHEFF         ',xlHTSHEFF ,ShlRecords,1,1)  !RSMSHL
  CALL GETRNGR('xlCLSHEFF         ',xlCLSHEFF ,ShlRecords,1,1)  !RSMSHL
  CALL GETRNGR('xlHTSHBASE        ',xlHTSHBASE ,ShlRecords,1,1) !RSMSHL
  CALL GETRNGR('xlCLSHBASE        ',xlCLSHBASE ,ShlRecords,1,1) !RSMSHL
  CALL GETRNGR('xlSHELCOST        ',xlSHELCOST ,ShlRecords,1,1) !RSMSHL
  CALL GETRNGR('xlSHELSUB         ',xlSHELSUB, ShlRecords,1,1)  !RSMSHL
  CALL GETRNGR('xlSHELSUB111D     ',xlSHELSUB111D,ShlRecords,1,1) !RSMSHL
  CALL GETRNGI('xlHVPACKG         ',xlHVPACKG ,ShlRecords,1,1)  !RSMSHL
  CALL GETRNGC('xlHVPGNAME        ',xlHVPGNAME,ShlRecords,1,1)  !RSMSHL

  ! Convert Excel named ranges to NEMS variables
  DO I = 1,ShlRecords           !RSMSHL
    RSCENDIV(I) = xlRSCENDIV(I) !RSMSHL
    RSBTYPE(I) = xlRSBTYPE(I)   !RSMSHL
    HVHTEQCL(I) = xlHVHTEQCL(I) !RSMSHL
    HVHTEQTY(I) = xlHVHTEQTY(I) !RSMSHL
    HVCLEQCL(I) = xlHVCLEQCL(I) !RSMSHL
    HVCLEQTY(I) = xlHVCLEQTY(I) !RSMSHL
    HVFYEAR(I) = xlHVFYEAR(I)   !RSMSHL
    HVLYEAR(I) = xlHVLYEAR(I)   !RSMSHL
    HVHEATFACTOR(I) = xlHVHEATFACTOR(I) !RSMSHL
    HVCOOLFACTOR(I) = xlHVCOOLFACTOR(I) !RSMSHL
    HTSHEFF(I) = xlHTSHEFF(I)   !RSMSHL
    CLSHEFF(I) = xlCLSHEFF(I)   !RSMSHL
    HTSHBASE(I) = xlHTSHBASE(I) !RSMSHL
    CLSHBASE(I) = xlCLSHBASE(I) !RSMSHL
    SHELCOST(I) = xlSHELCOST(I) !RSMSHL
    SHELSUB(I) = xlSHELSUB(I)   !RSMSHL
    SHELSUB111D(I) = xlSHELSUB111D(I) !RSMSHL
    HVPACKG(I) = xlHVPACKG(I)   !RSMSHL
    HVPGNAME(I) = xlHVPGNAME(I) !RSMSHL

    ! Processing RSMSHL data after being read in
    ! Convert shell costs to RSMEQP dollar-year costs (if different)
    SHELCOST(I)    = SHELCOST(I)    *MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(rshldollaryr-BaseYr+1) !rtek$
    SHELSUB(I)     = SHELSUB(I)     *MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(rshldollaryr-BaseYr+1) !rtek$
    SHELSUB111D(I) = SHELSUB111D(I) *MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(rshldollaryr-BaseYr+1) !rtek$

    ! Immediately subtract subsidies from shell cost
    SHELCOST(I) = (SHELCOST(I)-SHELSUB(I)-SHELSUB111D(I)*FLOAT(EPA111D))
  ENDDO !ShlRecords

  ! If RSMSHL read was successful, print summary information to unit 6 (nohup.out)
  WRITE(6,*) 'RESDMSG SUB_RTEKREAD1 RSMSHL EOF REACHED OK; COUNT = ',ShlRecords

  ! Close input workbook
  INFILE= FILE_MGR ('C','RSMESS',.FALSE.)

  ! Turn off debugging write-out of read-in named ranges to RSXLSDBG.txt output file
  IF (PRTDBGR.EQ.1) THEN
    XMLOUT = FILE_MGR('C','RSXLSDBG',.TRUE.) ! Comment out this line of code to turn off
    XMLOUT = 0
  ENDIF

  ! DEALLOCATE dynamic arrays
    DEALLOCATE (xlRTCLENDU,xlRTCLEQCL,xlRTCLTYPT,xlRTCLPNTR,xlRTCLREPL,xlRTFUEL,xlRTFFAN,xlRTMINLIF,xlRTMAXLIF, & !RSCLASS
                 xlRTCLNAME,xlRTALPHA,xlRTBASEFF,xlRTK,xlRTLAMBDA,xlRTFCBETA,xlRTSWFACT,xlRTSWBETA,xlRTSWBIAS,  & !RSCLASS
                xlRTTYENDU,xlRTTYEQCL,xlRTEQTYPE,xlRTINITYR,xlRTLASTYR,xlRTCENDIV,xlHVACPNTR,xlRTTYPNTR,      & !RSMEQP
                 xlRTTYNAME,xlRTMATURE,xlCWMEF,xlLOADADJ,xlRTEQEFF,xlRTEQCOST,xlRTRECOST,xlRTEQSUB,xlRTRESUB, & !RSMEQP
                 xlRTEQSUBN,xlRTRESUBN,xlRTEQSUB111D,xlRTRESUB111D,xlRTCOSTP1,xlRTCOSTP2,                     & !RSMEQP
                 xlRTCOSTP3,xlRTECBTA1,xlRTECBTA2,xlRTECBTA3,xlRTECBIAS,                                      & !RSMEQP
                xlRSCENDIV,xlRSBTYPE,xlHVHTEQCL,xlHVHTEQTY,xlHVCLEQCL,xlHVCLEQTY,xlHVFYEAR,xlHVLYEAR, & !RSMSHL
                 xlHVPACKG,xlHVPGNAME,xlHVHEATFACTOR,xlHVCOOLFACTOR,xlHTSHEFF,xlCLSHEFF,xlHTSHBASE,   & !RSMSHL
                 xlCLSHBASE,xlSHELCOST,xlSHELSUB,xlSHELSUB111D)                                         !RSMSHL

END SUBROUTINE RTEKREAD


!********************************************************************
!   READ DEGREE DAY DATA
!     KDEGDAY
!********************************************************************
      SUBROUTINE DEGDAYREAD
      IMPLICIT NONE

      INTEGER FILE_MGR              ! FILE MANAGER
      INTEGER*4 INFILE,           & ! FILE HANDLE
          IOS,                    & ! READ ERROR NUMBER
          YEAR,DIV,D,Y              ! GENERAL INDICES

!  Initialize HDD and CDD variables as zero	!kj
      DO YEAR=BaseYr,IJUMPCALYR
        DO D=1,mNumCR
          HDDADJ(YEAR,D) = 0.0
          CDDADJ(YEAR,D) = 0.0
        ENDDO
      ENDDO

!   OPEN AND READ THE DATA FILE
      INFILE=FILE_MGR('O','KDEGDAY',.FALSE.)

      READ(INFILE,'(99(/))')                ! SKIP 100 LINE HEADER PER CDM CONVENTION

      DO YEAR=BaseYr,IJUMPCALYR
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) HDDYEAR(YEAR), (HDDADJ(YEAR,D),D=1,mNumCR-2)
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) CDDYEAR(YEAR), (CDDADJ(YEAR,D),D=1,mNumCR-2)

        !Create national population-weighted degree days
        DO D=1,mNumCR-2
          HDDADJ(YEAR,NationalPtr) = HDDADJ(YEAR,NationalPtr) + HDDADJ(YEAR,D) * MC_NP(D,YEAR-(BaseYr-1))/MC_NP(NationalPtr,YEAR-(BaseYr-1))
          CDDADJ(YEAR,NationalPtr) = CDDADJ(YEAR,NationalPtr) + CDDADJ(YEAR,D) * MC_NP(D,YEAR-(BaseYr-1))/MC_NP(NationalPtr,YEAR-(BaseYr-1))
        ENDDO
      ENDDO  ! YEAR

     ! If KDEGDAY read was successful, print summary information to unit 6 (nohup.out)
      WRITE(6,*) 'RESDMSG SUB_DEGDAYREAD: KDEGDAY data set read successfully'
      INFILE=FILE_MGR('C','KDEGDAY',.FALSE.)
      RETURN !successful

!   READ ERROR OCCURRED
!   CLOSE THE FILE, PRINT READ ERROR MESSAGE TO UNIT 6 (NOHUP.OUT), AND RETURN
10    CONTINUE
      INFILE=FILE_MGR('C','KDEGDAY',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_DEGDAYREAD read error number ',IOS
      RETURN

!   END OF FILE REACHED BEFORE DATA COMPLETELY READ IN
!   CLOSE THE FILE, PRINT ERROR MESSAGE TO UNIT 6 (NOHUP.OUT), AND RETURN
 95   CONTINUE
      INFILE=FILE_MGR('C','KDEGDAY',.FALSE.)
      WRITE(6,*)'RESDMSG SUB_DEGDAYREAD: EOF reached before all data read in.'
      WRITE(6,*)'RESDMSG SUB_DEGDAYREAD: Correct KDEGDAY and resubmit job.'
      RETURN
      END SUBROUTINE DEGDAYREAD


!********************************************************************
!   READ BASELINEKWH FOR EPA111D ANALYSIS
!********************************************************************
      SUBROUTINE BLDBASEREAD
      IMPLICIT NONE
!
      INTEGER FILE_MGR              ! FILE MANAGER
      INTEGER*4 INFILE,           & ! FILE HANDLE
          IOS,                    & ! READ ERROR NUMBER
          YEAR,DIV,D,Y              ! GENERAL INDICES
      REAL TEMP                     !PLACEHOLDER TO PULL IN QELCM AND DISCARD


!   OPEN AND READ THE DATA FILE
      INFILE=FILE_MGR('O','BLDBASE',.FALSE.) ! OPEN THE DEGDAY DATA SET

      READ(INFILE,'(99(/))')                 ! SKIP 100 LINE HEADER PER CDM CONVENTION + SKIP YEARS BEFORE RECS YEAR

      !Write baseline electricity consumption to unit 9 (RESOUT.txt)
      WRITE (9,*) 'division, divcheck, year, yearcheck, baseline Trills converted to bkWh'

          DO D=1,mNumCR-2
           DO y=1,mNumYr
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) Div, Year, baselinebkwh(d,y), TEMP ! upon read, these data are in Trills
            BASELINEBKWH(d,y)=(BASELINEBKWH(d,y)/3412.)*10**3  !convert quads (trills?) to bkWh  !DGreport
            WRITE(9,5) D,Div,Y,Year,BASELINEBKWH(D,Y)
           ENDDO
          ENDDO
 5    FORMAT(' BASELINE CHECK',4I5,F12.5)

!   FILE SUCCESSFULLY READ, CLOSE FILE AND RETURN
      WRITE(6,*) 'RESDMSG SUB_BLDBASEREAD: BASELINEKWH data set read successfully'
      INFILE=FILE_MGR('C','BLDBASE',.FALSE.)
      RETURN

!   READ ERROR OCCURRED
!   CLOSE THE FILE, PRINT READ ERR MESSAGE, AND RETURN
10    CONTINUE
      INFILE=FILE_MGR('C','BLDBASE',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_BLDBASEREAD read error number ',IOS
      RETURN

!   EOF REACHED BEFORE DATA COMPLETELY READ IN
!   CLOSE THE FILE, PRINT ERR MESSAGE, AND RETURN
 95   CONTINUE
      INFILE=FILE_MGR('C','BLDBASE',.FALSE.)
      WRITE(6,*)'RESDMSG SUB_BLDBASEREAD: EOF reached before all data read in.'
      WRITE(6,*)'RESDMSG SUB_BLDBASEREAD: Correct BLDBASE and resubmit job.'
      RETURN
      END SUBROUTINE BLDBASEREAD

!********************************************************************
!   READ FUEL SWITCHING DATA
!     RSSWITCH
!********************************************************************
      SUBROUTINE RSSWITCHREAD
      IMPLICIT NONE

      INTEGER FILE_MGR              ! FILE MANAGER
      INTEGER*4 INFILE,           & ! FILE HANDLE
          IOS,                    & ! READ ERROR NUMBER
          B,D,F,E,                & ! GENERAL INDICES
          NUMCL,RECCL,RECCLSW

!   OPEN AND READ THE DATA FILE

      INFILE=FILE_MGR('O','RSSWITCH',.FALSE.) ! OPEN THE RSSWITCH DATA SET

      READ(INFILE,'(19(/))')                ! SKIP 20 LINE HEADER

      DO B=1,mNumBldg
        DO E=1,nHeatClasses
         READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(RTFCBIAS(E,B,D),D=1,mNumCR-2)
        ENDDO
      ENDDO

!   READ IN MATRIX OF COSTS INVOLVED WITH SWITCHING TECHNOLOGIES ON REPLACEMENT
!     READ RTCLCNT-2 RECORDS BECAUSE NO SWITCHING FOR FOOD REFRIGERTION OR FREEZING

      READ(INFILE,'(3(/))')                ! SKIP 4 LINE HEADER

      DO RECCL = 1,RTCLCNT-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) &
       (RPINSCOST(RECCLSW,RECCL),RECCLSW=1,RTCLCNT-2)
      ENDDO

      INFILE=FILE_MGR('C','RSSWITCH',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_RSSWITCHREAD: RSSWITCH data set read successfully'
      RETURN

!   READ ERROR OCCURRED
!   CLOSE THE FILE, PRINT READ ERROR MESSAGE TO UNIT 6 (NOHUP.OUT), AND RETURN
 10   CONTINUE
      INFILE=FILE_MGR('C','RSSWITCH',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_RSSWITCHREAD: RSSWITCH read error number ',IOS
      RETURN

!   EOF REACHED BEFORE DATA COMPLETELY READ IN
!   CLOSE THE FILE, PRINT ERR MESSAGE, AND RETURN
 95   CONTINUE
      INFILE=FILE_MGR('C','RSSWITCH',.FALSE.)
      WRITE(6,*)'RESDMSG SUB_RSSWITCHREAD: EOF reached before all data read in.'
      WRITE(6,*)'RESDMSG SUB_RSSWITCHREAD: Correct RSSWITCH and resubmit job.'
      RETURN

      END SUBROUTINE RSSWITCHREAD


!********************************************************************
!   READ NEW AND EXISTING HOME SHELL UEC DATA
!     RSUECSHL
!********************************************************************
      SUBROUTINE RSUECSHLREAD
      IMPLICIT NONE


      INTEGER FILE_MGR              ! FILE MANAGER
      INTEGER*4 INFILE,           & ! FILE HANDLE
          IOS,                    & ! READ ERROR NUMBER
          D,E,B,F                   ! GENERAL INDICES

!   OPEN AND READ THE DATA FILE

      INFILE=FILE_MGR('O','RSUECSHL',.FALSE.) ! OPEN THE RSUECSHL DATA SET

      READ(INFILE,'(19(/))')                ! SKIP 20 LINE HEADER


      DO D=1,mNumCR-2
        DO E=1,nHeatClasses
         READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(NEWHEATUEC(E,B,D),B=1,mNumBldg)
        ENDDO
      ENDDO

      DO D=1,mNumCR-2
         READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(NEWCOOLUEC(B,D),B=1,mNumBldg)
      ENDDO

      READ(INFILE,'(1(/))')                ! SKIP 2 LINE HEADER

      DO E=1,16
         READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) BASELOAD(E)
      ENDDO

      DO B=1,mNumBldg
       DO D=1,mNumCR-2
        DO F=1,MNUMFUEL-2
         EHSHELL(RECSYear,F,D,B)=1.0
        ENDDO
       ENDDO
      ENDDO

      DO D=1,mNumCR-2
       DO B=1,mNumBldg
        ECSHELL(RECSYear,D,B)=1.0
       ENDDO
      ENDDO

      INFILE=FILE_MGR('C','RSUECSHL',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_RSUECSHLREAD: RSUECSHL data set read successfully'
      RETURN

!   READ ERROR OCCURRED
!   CLOSE THE FILE, PRINT READ ERROR MESSAGE TO UNIT 6 (NOHUP.OUT), AND RETURN
 10   CONTINUE
      INFILE=FILE_MGR('C','RSUECSHL',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_RSUECSHLREAD: RSUECSHL read error number ',IOS
      RETURN

!   EOF REACHED BEFORE DATA COMPLETELY READ IN
!   CLOSE THE FILE, PRINT ERROR MESSAGE TO UNIT 6 (NOHUP.OUT), AND RETURN
 95   CONTINUE
      INFILE=FILE_MGR('C','RSUECSHL',.FALSE.)
      WRITE(6,*)'RESDMSG SUB_RSUECSHLREAD: EOF reached before all data read in.'
      WRITE(6,*)'RESDMSG SUB_RSUECSHLREAD: Correct RSUECSHL and resubmit job.'
      RETURN

      END SUBROUTINE RSUECSHLREAD


!********************************************************************
!   READ MISCELLANEOUS ELECTRIC LOAD (MELs) UEC AND STOCK INDICES
!     RSMELS
!********************************************************************
      SUBROUTINE RSMELSREAD
      IMPLICIT NONE

      INTEGER FILE_MGR              ! FILE MANAGER
      INTEGER*4 INFILE,           & ! FILE HANDLE
          IOS,                    & ! READ ERROR NUMBER
          B,D,F,Y,YEAR,EQC,E,V,L,I  ! GENERAL INDICES  !IncEff

!   OPEN AND READ THE DATA FILE

      INFILE=FILE_MGR('O','RSMELS',.FALSE.) ! OPEN THE RMELS DATA SET

      READ(INFILE,'(19(/))')                ! SKIP 20 LINE HEADER

!   Read in counter for number of MELs end uses in RSMELS.txt  !IncEff	!kj - will be used more once MELs read-in/calculations are streamlined
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) NumMELs  !IncEff

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER  !IncEff

!   Read in "Televisions and related equipment"
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(TVSPEN(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(STBPEN(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(HTSPEN(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(OTTPEN(Y), Y=RECSYear+1,ijumpcalyr)  !MELs21
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(VGCPEN(Y), Y=RECSYear+1,ijumpcalyr)
!   Read in "Computers and related equipment"
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(DPCPEN(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(LPCPEN(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(MONPEN(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(NETPEN(Y), Y=RECSYear+1,ijumpcalyr)
!   Read in other specified miscellaneous electric loads
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(BATPEN(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(CFNPEN(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(COFPEN(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(DEHPEN(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(MCOPEN(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(PLPPEN(Y), Y=RECSYear+1,ijumpcalyr)  !MELs21
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(PLHPEN(Y), Y=RECSYear+1,ijumpcalyr)  !MELs21
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(SECPEN(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(SPAPEN(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(WCLPEN(Y), Y=RECSYear+1,ijumpcalyr)    !winecool
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(SPKPEN(Y), Y=RECSYear+1,ijumpcalyr)  !MELs21
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(PHNPEN(Y), Y=RECSYear+1,ijumpcalyr)  !MELs21
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(TABPEN(Y), Y=RECSYear+1,ijumpcalyr)  !MELs21
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(KITPEN(Y), Y=RECSYear+1,ijumpcalyr)  !MELs21

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

!   Read in "Televisions and related equipment"
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(TVSEFF(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(STBEFF(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(HTSEFF(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(OTTEFF(Y), Y=RECSYear+1,ijumpcalyr)  !MELs21
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(VGCEFF(Y), Y=RECSYear+1,ijumpcalyr)
!   Read in "Computers and related equipment"
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(DPCEFF(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(LPCEFF(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(MONEFF(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(NETEFF(Y), Y=RECSYear+1,ijumpcalyr)
!   Read in other specified miscellaneous electric loads
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(BATEFF(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(CFNEFF(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(COFEFF(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(DEHEFF(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(MCOEFF(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(PLPEFF(Y), Y=RECSYear+1,ijumpcalyr)  !MELs21
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(PLHEFF(Y), Y=RECSYear+1,ijumpcalyr)  !MELs21
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(SECEFF(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(SPAEFF(Y), Y=RECSYear+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(WCLEFF(Y), Y=RECSYear+1,ijumpcalyr)  !winecool
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(SPKEFF(Y), Y=RECSYear+1,ijumpcalyr)  !MELs21
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(PHNEFF(Y), Y=RECSYear+1,ijumpcalyr)  !MELs21
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(TABEFF(Y), Y=RECSYear+1,ijumpcalyr)  !MELs21
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(KITEFF(Y), Y=RECSYear+1,ijumpcalyr)  !MELs21

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER  !IncEff

!   Read in switch to apply/not apply income effect to each of the specific MELs (1=yes, 0=no)  !IncEff
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(MELsIncomeEffect(I), I=1,NumMELs)  !IncEff

      INFILE=FILE_MGR('C','RSMELS',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_RSMELSREAD: RSMELS data set read successfully'
      RETURN

!   READ Error OCCURRED
!   CLOSE THE FILE, PRINT READ ERR MESSAGE, AND RETURN
 10   CONTINUE
      INFILE=FILE_MGR('C','RSMELS',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_RSMELSREAD: RSMELS read error number ',IOS
      RETURN

!   EOF REACHED BEFORE DATA COMPLETELY READ IN
!   CLOSE THE FILE, PRINT ERR MESSAGE, AND RETURN
 95   CONTINUE
      INFILE=FILE_MGR('C','RSMELS',.FALSE.)
      WRITE(6,*)'RESDMSG SUB_RSMELSREAD: EOF reached before all data read in.'
      WRITE(6,*)'RESDMSG SUB_RSMELSREAD: Correct RSMELS and resubmit job.'
      RETURN

      END SUBROUTINE RSMELSREAD


!********************************************************************
!   READ MISCELLANEOUS RESIDENTIAL INPUTS
!     RSMISC
!********************************************************************
      SUBROUTINE RSMISCREAD
      IMPLICIT NONE

!   DECLARE LOCAL VARIABLES
      INTEGER FILE_MGR              ! FILE MANAGER
      INTEGER*4 INFILE,           & ! FILE HANDLE
          IOS,                    & ! READ ERROR NUMBER
          B,D,F,Y,YEAR,EQC,E,V,L, & ! GENERAL INDICES
          DummyNum,NUMCL,EU,RECCL,RECCLSW,BIN,Y1
      CHARACTER*3 DummyTxt
      REAL*4 ELfactor, NGfactor, DSfactor, LGfactor       ! elastruns
      INTEGER*2 modyear, endmodyear, s                    ! elastruns

!   OPEN AND READ THE DATA FILE
      INFILE=FILE_MGR('O','RSMISC',.FALSE.) ! OPEN THE RSMISC DATA SET

      READ(INFILE,'(19(/))')                ! SKIP 20 LINE HEADER

      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(HDR(B),B=1,mNumBldg)  !HDRendog

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER  !HDRendog

      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)HDRfy,HDRly  !HDRendog
	  
	  IF (HDRfy.LE.BaseYr) THEN  !HDRendog
        HDRfy=BaseYr+1 !Because HDR calculations require a previous year (i.e., Y-1), the first year must be later than the base year  !HDRendog
        WRITE(9,'("Warning: HDRfy modified from RSMISC.txt value")')  !HDRendog
      ENDIF  !HDRendog
      IF (HDRly.GT.EndYr) THEN  !HDRendog
        HDRly=EndYr !Prevents last year from being outside of range of available housing stock/start data from MAM  !HDRendog
        WRITE(9,'("Warning: HDRly modified from RSMISC.txt value")')  !HDRendog
      ENDIF  !HDRendog

      !Write input HDR years to unit 9 (RESOUT.txt) to compare with input years  !HDRendog
      WRITE(9,'(a)') 'Housing demolition/decay rate (HDR) years to compare with RSMISC.txt'  !HDRendog
      WRITE(9,'("HDRfy ",i4)') HDRfy  !HDRendog
      WRITE(9,'("HDRly ",i4)') HDRly  !HDRendog

      !Write input HDRs to unit 9 (RESOUT.txt) to compare with calculated HDRs  !HDRendog
      WRITE(9,'(a)') 'Input housing demolition/decay rates (HDR) from RSMISC.txt'  !HDRendog
      WRITE(9,'("HDR_input_SF ",f6.4)') HDR(1)  !HDRendog
      WRITE(9,'("HDR_input_MF ",f6.4)') HDR(2)  !HDRendog
      WRITE(9,'("HDR_input_MH ",f6.4)') HDR(3)  !HDRendog

      !For standalone RDM runs, use HDR values as input into RSMISC.txt (above); otherwise, overwrite with calculated values based on integrated NEMS Macroeconomic Activity Module (MAM) outputs for housing starts/stocks
      !!!If input HDR values differ from integrated run-calculated values (below), RDM outputs will differ between standalone and integrated test runs!!!  !HDRendog
      IF (EXM.EQ.1) THEN !calculate HDR endogenously  !HDRendog
        !MAM VARIABLES (FOR REFERENCE):
        !MC_HUSPS1(D,CurIYr) !single-family (B=1) housing starts (millions of housing units)  !HDRendog
        !MC_HUSPS2A(D,CurIYr) !multifamily (B=2) housing starts (millions of housing units)  !HDRendog
        !MC_HUSMFG(D,CurIYr) !mobile home (B=3) shipments (millions of housing units)  !HDRendog

        !MC_KHUPS1(D,CurIYr) !single-family (B=1) housing stocks (millions of housing units)  !HDRendog
        !MC_KHUPS2A(D,CurIYr) !multifamily (B=2) housing stocks (millions of housing units)  !HDRendog
        !MC_KHUMFG(D,CurIYr) !mobile home (B=3) stocks (millions of housing units)  !HDRendog

        !Zero-out variables (especially important for aggregated values used for averages  !HDRendog
        HDR(1:mNumBldg)=0.0  !HDRendog
        HDQ((BaseYr-BaseYr+2):(EndYr-BaseYr+1),1:mNumCR,1:mNumBldg)=0.0  !HDRendog
        HDi((BaseYr-BaseYr+2):(EndYr-BaseYr+1),1:mNumCR,1:mNumBldg)=0.0  !HDRendog
        HDiAve(1:mNumCR,1:mNumBldg)=0.0  !HDRendog
        HCDshr((BaseYr-BaseYr+2):(EndYr-BaseYr+1),1:mNumCR,1:mNumBldg)=0.0  !HDRendog
        HCDshrAve(1:mNumCR,1:mNumBldg)=0.0  !HDRendog

        DO Y=(HDRfy-BaseYr+1),(HDRly-BaseYr+1) !Convert calendar years to index years  !HDRendog
          DO D=1,mNumCR-2  !HDRendog
            !Number of households demolished in a given year (millions of housing units)  !HDRendog
            HDQ(Y,D,1)=MC_KHUPS1(D,Y-1)-MC_KHUPS1(D,Y)+MC_HUSPS1(D,Y-1)  !HDRendog
            HDQ(Y,D,2)=MC_KHUPS2A(D,Y-1)-MC_KHUPS2A(D,Y)+MC_HUSPS2A(D,Y-1)  !HDRendog
            HDQ(Y,D,3)=MC_KHUMFG(D,Y-1)-MC_KHUMFG(D,Y)+MC_HUSMFG(D,Y-1)  !HDRendog
            IF (PRTDBGR.EQ.1) THEN  !HDRendog
              !Write MAM housing stocks/starts to unit 9 (RESOUT.txt) to verify  !HDRendog
              WRITE(9,*) 'SF_stock,',Y+BaseYr-1,',',D,',',MC_KHUPS1(D,Y),',',Y+BaseYr-2,',',MC_KHUPS1(D,Y-1)  !HDRendog
              WRITE(9,*) 'SF_starts,',Y+BaseYr-1,',',D,',',MC_HUSPS1(D,Y)  !HDRendog
              WRITE(9,*) 'MF_stock,',Y+BaseYr-1,',',D,',',MC_KHUPS2A(D,Y),',',Y+BaseYr-2,',',MC_KHUPS2A(D,Y-1)  !HDRendog
              WRITE(9,*) 'MF_starts,',Y+BaseYr-1,',',D,',',MC_HUSPS2A(D,Y)  !HDRendog
              WRITE(9,*) 'MH_stock,',Y+BaseYr-1,',',D,',',MC_KHUMFG(D,Y),',',Y+BaseYr-2,',',MC_KHUMFG(D,Y-1)  !HDRendog
              WRITE(9,*) 'MH_ships,',Y+BaseYr-1,',',D,',',MC_HUSMFG(D,Y)  !HDRendog
              WRITE(9,*) 'HDQ,',Y+BaseYr-1,',',D,',',HDQ(Y,D,1),',',HDQ(Y,D,2),',',HDQ(Y,D,3)  !HDRendog
            ENDIF  !HDRendog

            !Annual index of households demoslished  !HDRendog
            HDi(Y,D,1)=1-(HDQ(Y,D,1)/MC_KHUPS1(D,Y))  !HDRendog
            HDi(Y,D,2)=1-(HDQ(Y,D,2)/MC_KHUPS2A(D,Y))  !HDRendog
            HDi(Y,D,3)=1-(HDQ(Y,D,3)/MC_KHUMFG(D,Y))  !HDRendog
            IF (PRTDBGR.EQ.1) THEN  !HDRendog
              !Write to unit 9 (RESOUT.txt) to verify  !HDRendog
              WRITE(9,*) 'HDi,',Y+BaseYr-1,',',D,',',HDi(Y,D,1),',',HDi(Y,D,2),',',HDi(Y,D,3)  !HDRendog
            ENDIF  !HDRendog

            !Aggregate annual index of households demoslished to create an average value  !HDRendog
            HDiAve(D,1)=HDiAve(D,1)+HDi(Y,D,1)  !HDRendog
            HDiAve(D,2)=HDiAve(D,2)+HDi(Y,D,2)  !HDRendog
            HDiAve(D,3)=HDiAve(D,3)+HDi(Y,D,3)  !HDRendog
            IF (PRTDBGR.EQ.1) THEN  !HDRendog
              !Write to unit 9 (RESOUT.txt) to verify  !HDRendog
              IF (Y.EQ.HDRly-BaseYr+1) THEN  !HDRendog
                WRITE(9,*) 'HDiAveTot,',Y+BaseYr-1,',',D,',',HDiAve(D,1),',',HDiAve(D,2),',',HDiAve(D,3)  !HDRendog
              ENDIF  !HDRendog
			ENDIF  !HDRendog

            IF (PRTDBGR.EQ.1) THEN  !HDRendog
              !Write to unit 9 (RESOUT.txt) to verify  !HDRendog
              IF (D.EQ.mNumCR-2) THEN  !HDRendog
                WRITE(9,*) 'Total_Housing_Units,',Y+BaseYr-1,',',D,',',MC_KHUPS1(11,Y),',',MC_KHUPS2A(11,Y),',',MC_KHUMFG(11,Y)  !HDRendog
              ENDIF  !HDRendog
            ENDIF  !HDRendog

            !Census division share of U.S. total households by housing type  !HDRendog
            HCDshr(Y,D,1)=MC_KHUPS1(D,Y)/MC_KHUPS1(11,Y)  !HDRendog
            HCDshr(Y,D,2)=MC_KHUPS2A(D,Y)/MC_KHUPS2A(11,Y)  !HDRendog
            HCDshr(Y,D,3)=MC_KHUMFG(D,Y)/MC_KHUMFG(11,Y)  !HDRendog
            IF (PRTDBGR.EQ.1) THEN  !HDRendog
              !Write to unit 9 (RESOUT.txt) to verify  !HDRendog
              WRITE(9,*) 'HCDshr,',Y+BaseYr-1,',',D,',',HCDshr(Y,D,1),',',HCDshr(Y,D,2),',',HCDshr(Y,D,3)  !HDRendog
            ENDIF  !HDRendog

            !Aggregate census division share of U.S. total households by housing type to create an average value  !HDRendog
            HCDshrAve(D,1)=HCDshrAve(D,1)+HCDshr(Y,D,1)  !HDRendog
            HCDshrAve(D,2)=HCDshrAve(D,2)+HCDshr(Y,D,2)  !HDRendog
            HCDshrAve(D,3)=HCDshrAve(D,3)+HCDshr(Y,D,3)  !HDRendog
            IF (PRTDBGR.EQ.1) THEN  !HDRendog
              !Write to unit 9 (RESOUT.txt) to verify  !HDRendog
              IF (Y.EQ.HDRly-BaseYr+1) THEN  !HDRendog
                WRITE(9,*) 'HCDshrAveTot,',Y+BaseYr-1,',',D,',',HCDshrAve(D,1),',',HCDshrAve(D,2),',',HCDshrAve(D,3)  !HDRendog
              ENDIF  !HDRendog
            ENDIF  !HDRendog
          ENDDO !D  !HDRendog
        ENDDO !Y  !HDRendog

        DO D=1,mNumCR-2  !HDRendog
          !Average index of households demolished over specified period
          HDiAve(D,1)=HDiAve(D,1)/(HDRly-HDRfy+1)  !HDRendog
          HDiAve(D,2)=HDiAve(D,2)/(HDRly-HDRfy+1)  !HDRendog
          HDiAve(D,3)=HDiAve(D,3)/(HDRly-HDRfy+1)  !HDRendog
          IF (PRTDBGR.EQ.1) THEN  !HDRendog
            !Write to unit 9 (RESOUT.txt) to verify  !HDRendog
            WRITE(9,*) 'HDiAve,',D,',',HDiAve(D,1),',',HDiAve(D,2),',',HDiAve(D,3)  !HDRendog
          ENDIF  !HDRendog

          !Average census division shares of U.S. total households by housing type over specified period
          HCDshrAve(D,1)=HCDshrAve(D,1)/(HDRly-HDRfy+1)  !HDRendog
          HCDshrAve(D,2)=HCDshrAve(D,2)/(HDRly-HDRfy+1)  !HDRendog
          HCDshrAve(D,3)=HCDshrAve(D,3)/(HDRly-HDRfy+1)  !HDRendog
          IF (PRTDBGR.EQ.1) THEN  !HDRendog
            !Write to unit 9 (RESOUT.txt) to verify  !HDRendog
            WRITE(9,*) 'HCDshrAve,',D,',',HCDshrAve(D,1),',',HCDshrAve(D,2),',',HCDshrAve(D,3)  !HDRendog
          ENDIF  !HDRendog
        ENDDO !D  !HDRendog

        DO B=1,mNumBldg  !HDRendog
          DO D=1,mNumCR-2  !HDRendog
            HDR(B)=HDR(B)+(HDiAve(D,B)*HCDshrAve(D,B))  !HDRendog
          ENDDO !D  !HDRendog
        ENDDO !B  !HDRendog

        !Write calculated HDRs to unit 9 (RESOUT.txt) to compare with input HDRs  !HDRendog
        !!!If input HDR values in RSMISC.txt differ from integrated run-calculated values, RDM outputs will differ between standalone and integrated test runs!!!  !HDRendog
        !!!Can use these values written to RESOUT.txt as inputs to RSMISC.txt for standalone testing, though values may change in side cases (e.g., economic growth cases or oil/fuel prices affecting mobile home shipments)!!!!  !HDRendog
        WRITE(9,'(a)') 'Calculated housing demolition/decay rates (HDR) for integrated runs'  !HDRendog
        WRITE(9,'("HDR_calculated_SF ",f6.4)') HDR(1)  !HDRendog
        WRITE(9,'("HDR_calculated_MF ",f6.4)') HDR(2)  !HDRendog
        WRITE(9,'("HDR_calculated_MH ",f6.4)') HDR(3)  !HDRendog

        DO B=1,mNumBldg  !HDRendog
          IF (HDR(B).GT.1.0) THEN  !HDRendog
		    HDR(B)=1.0  !HDRendog	!kj - should 1.0 max be reconsidered? Set to value less than 1.0 (0.9999?) to ensure some removal of housing stock?
            !Write warning to both NOHUP.OUT and RESOUT.txt that HDR has been overwritten
            !HDR=1.0 means that housing stock does not decrease (no demolitions, or conversions into residential housing units exceed demolitions)  !HDRendog
            WRITE(6,'("Warning: calculated residential HDR value greater than 1.0000; overwritten to 1.0000 for housing type ",i2)') B  !HDRendog
            WRITE(9,'("Warning: calculated residential HDR value greater than 1.0000; overwritten to 1.0000 for housing type ",i2)') B  !HDRendog
          ENDIF  !HDRendog
        ENDDO !B  !HDRendog
      ENDIF !calculate HDR endogenously  !HDRendog

      READ(INFILE,'(1(/))')                ! SKIP 2 LINE HEADER

      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(ResDiscountRate)

      READ(INFILE,'(1(/))')                ! SKIP 2 LINE HEADER

      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(Tenure)

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO D=1,mNumCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,(EH(RECSYear,B,D),B=1,mNumBldg)
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO D=1,mNumCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,(RACSAT(B,D),B=1,mNumBldg)
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO D=1,mNumCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,(RACUnits(B,D),B=1,mNumBldg)
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO D=1,mNumCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,(CACSAT(B,D),B=1,mNumBldg)
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 2 LINE HEADER

      DO D=1,mNumCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,CACPR(D)  !RSMISCpen - expanded to CD
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 2 LINE HEADER

      DO D=1,mNumCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,(DWPR(B,D),B=1,mNumBldg)  !RSMISCpen - expanded to CD and housing type
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO D=1,mNumCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,(ELDRYPR(B,D),B=1,mNumBldg)
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER  !CWshr

      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)TCW_SHR,FCW_SHR  !CWshr

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER  !RefFreezShr

      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)TMF_SHR,SMF_SHR,BMF_SHR  !RefFreezShr

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER  !RefFreezShr

      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)CH_SHR,UP_SHR  !RefFreezShr

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO D=1,mNumCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,(FRZSAT(B,D),B=1,mNumBldg)
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO D=1,mNumCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,(REFSAT(B,D),B=1,mNumBldg)
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO D=1,mNumCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,(DISHNEW(RECSYear+1,B,D), B=1,mNumBldg)
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO D=1,mNumCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,(WASHNEW(RECSYear+1,B,D), B=1,mNumBldg)
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO E=1,2
       DO D=1,mNumCR-2
         READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,DummyTxt,(NEWDRYSAT(RECSYear+1,E,B,D),B=1,mNumBldg)
       ENDDO
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO B=1,mNumBldg
       DO D=1,mNumCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,DummyTxt,(SHTSHR(B,D,F),F=1,7)
       ENDDO
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO B=1,mNumBldg
       DO D=1,mNumCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,DummyTxt,(NSHTSHR(B,D,F),F=1,7)
       ENDDO
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO F=1,2  !Heating = 1; Cooling = 2
        DO B=1,mNumBldg !LIupdate
          DO D=1,mNumCR-2
            READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyTxt,DummyTxt,DummyNum,(WTHRZTN(Y,F,D,B),Y=RECSyear,EndYr)  !yr  !LIupdate
          ENDDO
        ENDDO !LIupdate
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO D=1,mNumCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DummyNum,(ELASTIC(S,D),S=1,5) !HEATING FOSSIL, HEATING ELEC, CAC, HP AC, FURN FAN
      ENDDO

!    Overwrite prices in the common block for elasticity runs. ! elastruns
      READ(INFILE,'(2(/))')       ! SKIP 3 LINE HEADER         ! elastruns

      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)                & ! elastruns
          MODYEAR, EndModYear, ELFACTOR, NGFACTOR, DSFACTOR, & ! elastruns
            LGFACTOR                                           ! elastruns

      DO y=ModYear,EndModYear                                  ! elastruns
        DO d= 1, mNumCR-2                                      ! elastruns
          PELRS (d,y) =  PELRS (d,y) * ELfactor                ! elastruns
           DO s=1,10                                           ! elastruns	!kj - what are S=1-10? see above for S=1,5
             pelrsout(d,y,s)=pelrsout(d,y,s)*Elfactor          ! elastruns
           ENDDO ! s                                           ! elastruns
            PNGRS (d,y) =  PNGRS (d,y) * NGfactor              ! elastruns
            PDSRS (d,y) =  PDSRS (d,y) * DSfactor              ! elastruns
            PLGRS (d,y) =  PLGRS (d,y) * LGfactor              ! elastruns
        ENDDO  !divisions                                      ! elastruns
       ENDDO  !years                                           ! elastruns

!   READ SUCESSFUL.  CLOSE THE FILE.
      INFILE=FILE_MGR('C','RSMISC',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_RSMISCREAD: RSMISC data set read successfully'

!   ZERO OUT AND INITIALIZE VARIABLES AS APPROPRIATE AND RETURN
      DO Y=RECSYear+1,EndYr
        DO B=1,mNumBldg
          DO D=1,mNumCR-2
            EH(Y,B,D)=((EH(Y-1,B,D)*HDR(B)))    ! COMMON/EXHS/
          ENDDO  !D
        ENDDO  !B
      ENDDO  !Y

      OLDHSES(RECSYear)=0.0
      DO D=1,mNumCR-2	!kj - Was DO-CONTINUE loop
        DO B=1,mNumBldg
          OLDHSES(RECSYear)=OLDHSES(RECSYear)+EH(RECSYear,B,D)  ! COMMON/ALLHOUSES/
        ENDDO  !B
      ENDDO  !D

       LIMIT=0.30  !Maximum shell efficiency index of 0.3 (i.e., maximum shell efficiency is limited to a 70% improvement on the base-year value); applies to existing, new, heating, cooling	!kj

      DO Y1=RECSYear+1,LastYr+BaseYr-1  !represents the annual increase in existing shell integrity due to technology improvements	!kj - Identify source; subtracted from existing shell index, so is this possibly tech degradation?
        DO B=1,mNumBldg
         TECHG(Y1,1,B)=TECHG(Y1-1,1,B)+0.005
         TECHG(Y1,2,B)=TECHG(Y1-1,2,B)+0.003
         TECHG(Y1,3,B)=TECHG(Y1-1,3,B)+0.006
         TECHG(Y1,4,B)=TECHG(Y1-1,4,B)+0.003
         TECHG(Y1,5,B)=TECHG(Y1-1,5,B)+0.006
         TECHG(Y1,6,B)=TECHG(Y1-1,6,B)+0.016
         TECHG(Y1,7,B)=TECHG(Y1-1,7,B)+0.003
         TECHG(Y1,8,B)=TECHG(Y1-1,8,B)+0.003
         TECHG(Y1,9,B)=TECHG(Y1-1,9,B)+0.011
        ENDDO
      ENDDO

      RETURN  !Return if successful

!   READ ERROR OCCURRED
!   CLOSE THE FILE, PRINT READ ERROR MESSAGE TO UNIT 6 (NOHUP.OUT), AND RETURN
10    CONTINUE
      INFILE=FILE_MGR('C','RSMISC',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_RSMISCREAD file RSMISC read error number ',IOS
      RETURN

!   EOF REACHED BEFORE DATA COMPLETELY READ IN
!   CLOSE THE FILE, PRINT ERROR MESSAGE TO UNIT 6 (NOHUP.OUT), AND RETURN
95    CONTINUE
      INFILE=FILE_MGR('C','RSMISC',.FALSE.)
      WRITE(6,*)'RESDMSG SUB_RSMISCREAD: EOF reached before all data read in.'
      WRITE(6,*)'RESDMSG SUB_RSMISCREAD: Correct RSMISC and resubmit job.'
      RETURN
      END SUBROUTINE RSMISCREAD


!********************************************************************
!   READ RESIDENTIAL Lighting DATA
!     RSMLGT.TXT
!********************************************************************
      SUBROUTINE RSMLGTREAD
      IMPLICIT NONE

!DECLARE LOCAL VARIABLES
      INTEGER FILE_MGR              ! FILE MANAGER
      INTEGER*4 INFILE,           & ! FILE HANDLE
          IOS,                    & ! READ ERROR NUMBER
          D,B,E,BIN,Y1,I,app
      INTEGER LightDiag

     !Reverse the order of these statements to switch diagnostics on or off; used to test read-in of RSMLGT	!kj - move this to RSMLGT.txt input file as first variable to read in
     LightDiag=0 !1=Print diagnostics; 0=don't print diagnostics

     !READ RSMLGT.TXT -- residential lighting technology and usage data file
      INFILE=FILE_MGR('O','RSMLGT',.FALSE.) ! OPEN THE RSMLGT DATA SET

      READ(INFILE,'(19(/))')                ! SKIP 20 LINE HEADER

! Control Variables
!   dollar year for lighting technology costs
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) RLGTDOLLARYR    !lightmenu
      IF(LightDiag .NE. 0) WRITE(9,*) 'Read of Lighting data and Test Print'
      IF(LightDiag .NE. 0) WRITE(9,*) RLGTDOLLARYR

!   Number of lighting application types (currently 4 types: general service,
!     reflectors, linear fluorescent and exterior)
      READ(INFILE,'((/))')                ! SKIP 2 LINE HEADER
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) NumApps
      IF(LightDiag .NE. 0) WRITE(9,*) 'Number of Lighting Apps'
      IF(LightDiag .NE. 0) WRITE(9,*) numapps

! Application IDs - 3-character codes and order of index which map to the lighting technology data later
      READ(INFILE,'((/))')                ! SKIP 2 LINE HEADER
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (AppID(i),i=1,NumApps)
      IF(LightDiag .NE. 0) WRITE(9,'(5a5)') (appid(i),i=1,NumApps)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (AppIndex(i),i=1,NumApps)
      IF(LightDiag .NE. 0) WRITE(9,'(5i5)') (appindex(i),i=1,NumApps)

! Number of bulbs per application types
      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (NumTypes(i),i=1,NumApps)
      IF(LightDiag .NE. 0) WRITE(9,'(5i5)') (numtypes(i),i=1,NumApps)

! Number of bins (i.e., hours used) by application
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (NumAppBins(i),i=1,NumApps)
      IF(LightDiag .NE. 0) WRITE(9,'(5i5)') (numappbins(i),i=1,NumApps)

      READ(INFILE,'(3(/))')                ! SKIP 4 LINE HEADER
! Technology Data - read until a first year of 9999 is found
      DO I=1,NLRec
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) FirstYear(i),LastYear(i),BulbCost(i),(BulbEESub(i,d),d=1,mNumCR-2),(BulbSub(i,d),d=1,mNumCR-2),LPW(i), &  !EElightsub
           BulbWatts(i),LifeHours(i),BulbCRI(i),LightingApp(i),BulbType(i),LTLBeta1(i),LTLBeta2(i)   !lgtbetas
      IF(LightDiag .NE. 0) WRITE(9,3)FirstYear(i),LastYear(i),BulbCost(i),(BulbEESub(i,d),d=1,mNumCR-2),(BulbSub(i,d),d=1,mNumCR-2),LPW(i), &  !EElightsub
           BulbWatts(i),LifeHours(i),BulbCRI(i),LightingApp(i),BulbType(i),LTLBeta1(i),LTLBeta2(i)   !lgtbetas
          ! Convert Lighting Equipment Costs to RTEKDOLLARYR (consistent with prices)
            BulbCost(i)=BulbCost(i)*MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(RLGTDOLLARYR-BaseYr+1)  !lightmenu
      IF(FirstYear(i).EQ.9999) GOTO 5
      ENDDO
 3    FORMAT(2i6,19f8.2,4f7.1,2a5,2f7.1)  !lgtbetas EElightsub
 5    CONTINUE

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER
      DO app=1,NumApps !maximum 5 lighting applications, currently 4
       READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (BulbsPerHH(app,B),B=1,3)
       IF(LightDiag .NE. 0) WRITE(9,'("bulbs per hh",i5,3f8.2)') app,(BulbsPerHH(app,B),B=1,3)
       READ(INFILE,'(1x)')
      ENDDO

      DO app=1,NumApps !maximum 5 lighting applications, currently 4

         IF(LightDiag .NE. 0) WRITE(9,*) 'APPBINHOURS'
         READ(INFILE,'((/))')                ! SKIP 2 MORE LINES
         READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (AppBinHours(app,BIN),BIN=1,NumAppBins(app))
         IF(LightDiag .NE. 0) WRITE(9,'(6f10.0)') (AppBinHours(app,BIN),BIN=1,NumAppBins(app))

         IF(LightDiag .NE. 0) WRITE(9,*) 'LTBINSHARES'
! General service bin shares each of 3 lighting types by 6 bins
         READ(INFILE,'(1x)')
         READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (BinShares(app,BIN),BIN=1,NumAppBins(app))
         IF(LightDiag .NE. 0) WRITE(9,'(i4,6f10.2)') app,(BinShares(app,BIN),BIN=1,NumAppBins(app))
         READ(INFILE,'(1x)')
         IF(LightDiag .NE. 0) WRITE(9,*) 'app, type, BULBBINSHARES (by type)'
         DO e=1,NumTypes(app)
            READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (BulbBinShares(app,e,BIN),BIN=1,NumAppBins(app))
            IF(LightDiag .NE. 0) WRITE(9,'(2I4,6f10.1)') app,e,(BulbBinSHAREs(app,e,BIN),BIN=1,NumAppBins(app))
         ENDDO !e
         READ(INFILE,'(1x)')
         READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (BaseWattsBulbs(app,e),e=1,numtypes(app))
         IF(LightDiag .NE. 0) WRITE(9,'(i4,6f10.1)') app,(BaseWattsBulbs(app,e),e=1,numtypes(app))
         IF(app .NE. NumApps) READ(INFILE,'(1x)')   !lgtbetas

      ENDDO


!Compute RECS-year base watts per bulb weight averaged across bulb types
      DO app=1,numapps
       DO bin=1,numappbins(app)
        basewattbins(app,bin)=0.
       ENDDO
      ENDDO
      DO app=1,numapps
       DO bin=1,numappbins(app)
        DO e=1,numtypes(app)
         basewattbins(app,bin)=basewattbins(app,bin) + bulbbinshares(app,e,bin)*basewattsbulbs(app,e)
        ENDDO
       ENDDO
      ENDDO
      DO app=1,numapps
       IF(LightDiag .NE. 0) WRITE(9,9) AppID(app),(basewattbins(app,bin),BIN=1,numappbins(app))
      ENDDO
 9    FORMAT(' weighted base watts by bin for app: ',a5, 6f10.2)

      WRITE(6,*) 'RESDMSG SUB_RSMLGT: RSMLGT data set read successfully'

      INFILE=FILE_MGR('C','RSMLGT',.FALSE.)
      RETURN !successful

!   READ ERROR OCCURRED
!   CLOSE THE FILE, PRINT READ ERROR MESSAGE TO UNIT 6 (NOHUP.OUT), AND RETURN
10    CONTINUE
      INFILE=FILE_MGR('C','RSMLGT',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_RSMLGTREAD file RSMLGT read error number ',IOS
      IF(LightDiag .NE. 0) WRITE(9,*) 'RESDMSG SUB_RSMLGTREAD file RSMLGT read error number ',IOS
      RETURN

!   EOF REACHED BEFORE DATA COMPLETELY READ IN
!   CLOSE THE FILE, PRINT READ ERROR MESSAGE TO UNIT 6 (NOHUP.OUT), AND RETURN
95    CONTINUE
      INFILE=FILE_MGR('C','RSMLGT',.FALSE.)
      WRITE(6,*)'RESDMSG SUB_RSMLGTREAD: EOF reached before all data read in.'
      WRITE(6,*)'RESDMSG SUB_RSMLGTREAD: Correct RSMLGT and resubmit job.'
      IF(LightDiag .NE. 0) WRITE(9,*)'RESDMSG SUB_RSMLGTREAD: EOF reached before all data read in.'
      IF(LightDiag .NE. 0) WRITE(9,*)'RESDMSG SUB_RSMLGTREAD: Correct RSMLGT and resubmit job.'
      RETURN

      END SUBROUTINE Rsmlgtread


!*******************************************************************
!     READ IN RETIRING PERCENTAGES FOR DECAY OF RECS-YEAR EQUIPMENT
!*******************************************************************
      SUBROUTINE RDRET
      INTEGER  Y, RECCL
      LOGICAL NEW
      CHARACTER*18 FNAME
      INTEGER FILE_MGR
      INTEGER*4 IOS
      EXTERNAL FILE_MGR
      INTEGER  IUNIT1

      FNAME='RSRET01'
      NEW=.FALSE.
      IUNIT1=FILE_MGR('O',FNAME,NEW)

      READ(IUNIT1,'(19(/))')

      DO 50 RECCL=1,RTCLCNT
        READ(IUNIT1,*,ERR=10,END=95,IOSTAT=IOS)(EQCRET(Y,RECCL), Y=RECSYear+1,ijumpcalyr)
 50   CONTINUE

      IUNIT1=FILE_MGR('C',FNAME,NEW)
      WRITE(6,*) 'RESDMSG SUB_RDRET: RSRET01 data set read successfully'
      RETURN

!   READ ERROR OCCURRED
!   CLOSE THE FILE, PRINT READ ERROR MESSAGE TO UNIT 6 (NOHUP.OUT), AND RETURN
10    CONTINUE
      IUNIT1=FILE_MGR('C','FNAME',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_RDRET file RSRET01 read error number ',IOS
      RETURN

!   END OF FILE REACHED BEFORE DATA COMPLETELY READ IN
!   CLOSE THE FILE, PRINT ERROR MESSAGE TO UNIT 6 (NOHUP.OUT), AND RETURN
95    CONTINUE
      IUNIT1=FILE_MGR('C','FNAME',.FALSE.)
      WRITE(6,*)'RESDMSG SUB_RDRET: EOF reached before all data read in.'
      WRITE(6,*)'RESDMSG SUB_RDRET: Correct RDRET01 and resubmit job.'
      RETURN
      END SUBROUTINE RDRET


!********************************************************************
!     FUEL NUMBERING SYSTEMS MAPPING SUBROUTINE
!********************************************************************
      SUBROUTINE RCONSFL
      IMPLICIT NONE

! NOTE FOR ELECTRICITY, PRICES ARE BY END USE AS FOLLOWS
!    PELRSOUT(...,EU)      RTEK EU #
!    1 = Space Heating     1
!    2 = Space Cooling     2
!    3 = Water Heating     5
!    4 = Cooking           6
!    5 = Clothes Drying    7 (used for clothes washers also)
!    6 = Refrigeration     8
!    7 = Freezing          9
!    8 = Lighting          not in RTEK
!    9 = Appliances/Other  4 Dishwashers, 14 Other electric appliances
!    10= Secondary Space Heating not in RTEK


!    MAP RTEK FUEL NUMBERS INTO HTRCON FUEL NUMBERS
!                   HTRCON    RTEK
!                   FUEL #   FUEL #
!    FUEL NAME       FCON       F
!    NATURAL GAS       1        3
!    ELECTRICITY       2        4
!    DFO+KEROSENE      3        1 (DFO=Distillate Fuel Oil)
!    PROPANE           4        2
!    KEROSENE          5        5 (Combined with distillate fuel oil in AEO2019)	!kj
!    WOOD              6        1 (Priced to distillate fuel oil)
!    GEOTHERMAL        7        4 (Priced to electricity)
!    SOLAR             8        4

      NHTRFL=7 !number of space heating fuels
      FHTRCON(1)=3 !natural gas
      FHTRCON(2)=4 !electricity
      FHTRCON(3)=1 !distillate fuel oil
      FHTRCON(4)=2 !propane
      FHTRCON(5)=5 !kerosene (Combined with distillate fuel oil in AEO2019)	!kj
      FHTRCON(6)=6 !wood
      FHTRCON(7)=7 !geothermal

!    MAP RTEK FUEL NUMBERS INTO CLCON FUEL NUMBERS
!                   CLCON     RTEK
!                   FUEL #    FUEL #
!    FUEL NAME       FCON       F
!    ELECTRICITY       1        4
!    GEOTHERMAL        2        4 (Priced to electricity; use 7)
!    NATURAL GAS       3        3
      NCLFL=3 !number of space cooling (air conditioning) fuels
      FCLCON(3)=3 !natural gas
      FCLCON(4)=1 !electricity
      FCLCON(7)=2 !geothermal

! Water Heaters
      NWHFL=5 !number of water heating fuels
      FWHCON(1)=3 !natural gas
      FWHCON(2)=4 !electricity
      FWHCON(3)=1
      FWHCON(4)=2
      FWHCON(8)=5 !solar

! Cooking
      NSTVFL=3 !number of cooking fuels
      FSTVCON(2)=2
      FSTVCON(3)=1
      FSTVCON(4)=3

! Clothes Dryers
      NDRYFL=2 !number of clothes dryer fuels
      FDRYCON(3)=1 !natural gas
      FDRYCON(4)=2 !electricity

! Refrigerators
      NREFFL=1 !number of refrigeration fuels (i.e., electricity)	!kj - not used?

! Standalone Freezers
      NFRZFL=1 !number of standalone freezer fuels (i.e., electricity)	!kj - not used?

! Clothes Washers
      NCSWFL=1 !number of clothes washer fuels (i.e., electricity)	!kj - not used?

! Dishwashers
      NDSWFL=1 !number of dishwasher fuels (i.e., electricity)	!kj - not used?

! Secondary Space HEATING
      NSHTRFL=4 !number of secondary space heating fuels (natural gas, electricity, distillate fuel oil/kerosene, propane)	!kj - not used?

  RETURN
END SUBROUTINE RCONSFL


!*******************************************************************
!     READ SQUARE FEET IN NEW AND EXISTING HOUSES
!*******************************************************************
      SUBROUTINE RDSQFOOT
      IMPLICIT NONE
      INTEGER D, Y, B,S
      LOGICAL NEW
      CHARACTER*18 FNAME
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
      INTEGER  IUNIT1
!********************************************************************
      FNAME='RSSQFT'
      NEW=.FALSE.
      IUNIT1=FILE_MGR('O',FNAME,NEW)
      READ(IUNIT1,'(19(/))')
      DO B=1,mNumBldg
         DO D=1,mNumCR-2
           READ(IUNIT1,*) (SQRFOOT(Y,B,D),Y=RECSYear,ijumpcalyr)
         ENDDO
      ENDDO
      IUNIT1=FILE_MGR('C',FNAME,NEW)
      FNAME='RESOUT.TXT'
      OPEN(9,FILE=FNAME,FORM='FORMATTED')
! 100  FORMAT(1X,3(F6.4))
      END SUBROUTINE RDSQFOOT


!*******************************************************************
!     INITIALIZE PRICES FROM NEMS AND INFLATE
!*******************************************************************
      SUBROUTINE RDPR
      IMPLICIT NONE
      INTEGER D, Y, Y1

      IF(CurCalYr.EQ.RECSYear) THEN
        !MAP ALL PRICES FOR YEARS PRIOR TO RECSYear AND ALL FUTURE YEARS FROM RESTART FILE
        !  NOT IMPLEMENTED, BUT COULD BE USED FOR EXPECTATIONS
        DO 5 D=1,mNumCR-2
          DO 5 Y=1,mNumYr
            Y1=Y+(BaseYr-1)
            PRICES(1,D,Y1)=PDSRS(D,Y)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
            PRICES(2,D,Y1)=PLGRS(D,Y)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
            PRICES(3,D,Y1)=PNGRS(D,Y)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
            PRICES(4,D,Y1)=PELRS(D,Y)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
            PRICES(5,D,Y1)=PKSRS(D,Y)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
            PRICES(6,D,Y1)=PDSRS(D,Y)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
            PRICES(7,D,Y1)=PCLRS(D,Y)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
 5      CONTINUE
      ELSE
        !OVERWRITE PREVIOUS PRICES WITH CURRENT PRICES
        DO 10 D=1,mNumCR-2
          PRICES(1,D,CurCalYr)=PDSRS(D,CurIYr)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
          PRICES(2,D,CurCalYr)=PLGRS(D,CurIYr)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
          PRICES(3,D,CurCalYr)=PNGRS(D,CurIYr)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
          PRICES(4,D,CurCalYr)=PELRS(D,CurIYr)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
          PRICES(5,D,CurCalYr)=PKSRS(D,CurIYr)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
          ! WOOD PRICE IS LINKED TO GAS PRICE LESS ANY CARBON TAX

!bookmark  !!!NOTE IN REVIEWING RESOUT.TXT, THE WOOD PRICE IS THE DISTILLATE PRICE IN RECS YEAR BUT NATURAL GAS PRICE LESS CARBON TAX IN OTHER YEARS!
          PRICES(6,D,CurCalYr)=(PNGRS(D,CurIYr)-JNGRS(CurIYr))*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
          PRICES(7,D,CurCalYr)=PCLRS(D,CurIYr)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))

!  Check for zero prices and report to unit 6 (nohup.out)
         IF (PRICES(1,D,CurCalYr) .LE. 0.0) THEN
           PRICES(1,D,CurCalYr)=PRICES(1,D,CurCalYr-1)
           WRITE(6,*) 'RESDMSG Non-pos PRICE FOR RESD DIST REG=',D
         ENDIF
         IF (PRICES(2,D,CurCalYr).LE. 0.0) THEN
           PRICES(2,D,CurCalYr)=PRICES(2,D,CurCalYr-1)
           WRITE(6,*) 'RESDMSG Non-pos PRICE FOR RESD PROP REG=',D
         ENDIF
         IF (PRICES(3,D,CurCalYr) .LE. 0.0) THEN
           PRICES(3,D,CurCalYr)=PRICES(3,D,CurCalYr-1)
           WRITE(6,*) 'RESDMSG Non-pos PRICE FOR RESD NGAS REG=',D
         ENDIF
         IF (PRICES(4,D,CurCalYr) .LE. 0.0) THEN
           PRICES(4,D,CurCalYr)=PRICES(4,D,CurCalYr-1)
           WRITE(6,*) 'RESDMSG Non-pos PRICE FOR RESD ELEC REGION=',D
         ENDIF
         IF (PRICES(5,D,CurCalYr) .LE. 0.0) THEN
           PRICES(5,D,CurCalYr)=PRICES(5,D,CurCalYr-1)
           WRITE(6,*) 'RESDMSG Non-pos PRICE FOR RESD KERO REGION=',D
         ENDIF
         IF (PRICES(6,D,CurCalYr) .LE. 0.0) THEN
           PRICES(6,D,CurCalYr)=PRICES(6,D,CurCalYr-1)
           WRITE(6,*) 'RESDMSG Non-pos PRICE FOR RESD WOOD REGION=',D
         ENDIF

 10     CONTINUE
      ENDIF
      END SUBROUTINE RDPR


!**********************************************************************************************
! READ RECS-YEAR VINTAGE EQUIPMENT SUBROUTINE
!  Refer to RSCLASS tab of RSMESS.xlsx to see the numbers that correspond to equipment classes
!**********************************************************************************************
      SUBROUTINE RDHTREQC
      IMPLICIT NONE
      LOGICAL NEW
      CHARACTER*18 FNAME
      CHARACTER*18 FN
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
      INTEGER  B, D, Y, EU, RECCL, IUNIT1,NEQC,EQC,RECCLHP,E

!   READ IN All EQUIPMENT FOR RECS YEAR
      FNAME='RSSTK'
      NEW=.FALSE.
      IUNIT1=FILE_MGR('O',FNAME,NEW)
      READ(IUNIT1,'(19(/))')

        EU = 1             ! SPACE HEATING SECTION OF THE DATA

        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
         DO D=1,mNumCR-2
          READ(IUNIT1,*) (EQCESE(RECSYear,RECCL,B,D),B=1,mNumBldg)
         ENDDO
        ENDDO

        EU = 2             ! SPACE COOLING SECTION OF THE DATA

        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
         DO D=1,mNumCR-2
          READ(IUNIT1,*) (EQCESE(RECSYear,RECCL,B,D), B=1,mNumBldg)
         ENDDO
        ENDDO

        EU = 3             ! CLOTHES WASHER SECTION OF THE DATA

        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQC=RTCLEQCL(RECCL)
         DO D=1,mNumCR-2
          READ(IUNIT1,*) (EQCESE(RECSYear,RECCL,B,D), B=1,mNumBldg)
         ENDDO
        ENDDO

        EU = 4             ! DISHWASHER SECTION OF THE DATA

        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQC=RTCLEQCL(RECCL)
         DO D=1,mNumCR-2
          READ(IUNIT1,*) (EQCESE(RECSYear,RECCL,B,D), B=1,mNumBldg)
         ENDDO
        ENDDO

        EU = 5             ! WATER HEATING SECTION OF THE DATA

        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQC=RTCLEQCL(RECCL)
         DO D=1,mNumCR-2
          READ(IUNIT1,*) (EQCESE(RECSYear,RECCL,B,D), B=1,mNumBldg)
         ENDDO
        ENDDO

        EU = 6             ! COOKING SECTION OF THE DATA

        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQC=RTCLEQCL(RECCL)
         DO D=1,mNumCR-2
          READ(IUNIT1,*) (EQCESE(RECSYear,RECCL,B,D), B=1,mNumBldg)
         ENDDO
        ENDDO

        EU = 7             ! CLOTHES DRYER SECTION OF THE DATA

        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQC=RTCLEQCL(RECCL)
         DO D=1,mNumCR-2
          READ(IUNIT1,*) (EQCESE(RECSYear,RECCL,B,D), B=1,mNumBldg)
         ENDDO
        ENDDO

        EU = 8             ! FOOD REFRIGERATION SECTION OF THE DATA

        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQC=RTCLEQCL(RECCL)
         DO D=1,mNumCR-2
          READ(IUNIT1,*)   (EQCESE(RECSYear,RECCL,B,D),B=1,mNumBldg)
         ENDDO
        ENDDO

        EU = 9             ! FOOD FREEZING SECTION OF THE DATA
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQC=RTCLEQCL(RECCL)
         DO D=1,mNumCR-2
          READ(IUNIT1,*) (EQCESE(RECSYear,RECCL,B,D),B=1,mNumBldg)
         ENDDO
        ENDDO

  ! MELs SECTION OF THE DATA (NO END-USE NUMBER)

         DO D=1,mNumCR-2  !furnace fans
          READ(IUNIT1,*) (FANEQP(RECSYear,B,D),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !televisions
          READ(IUNIT1,*) (TVSEQP(RECSYear,B,D),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !set-top boxes
          READ(IUNIT1,*) (STBEQP(RECSYear,B,D),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !home theater systems
          READ(IUNIT1,*) (HTSEQP(RECSYear,B,D),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !Over-the-top (OTT) streaming devices
          READ(IUNIT1,*) (OTTEQP(RECSYear,B,D),B=1,mNumBldg)  !MELs21
         ENDDO

         DO D=1,mNumCR-2  !video game consoles
          READ(IUNIT1,*) (VGCEQP(RECSYear,B,D),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !desktop personal computers
          READ(IUNIT1,*) (DPCEQP(RECSYear,B,D),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !laptop personal computers
          READ(IUNIT1,*) (LPCEQP(RECSYear,B,D),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !computer monitors
          READ(IUNIT1,*) (MONEQP(RECSYear,B,D),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !computer networking equipment
          READ(IUNIT1,*) (NETEQP(RECSYear,B,D),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !rechargeable devices
          READ(IUNIT1,*) (BATEQP(RECSYear,B,D),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !ceiling fans
          READ(IUNIT1,*) (CFNEQP(RECSYear,B,D),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !coffee makers
          READ(IUNIT1,*) (COFEQP(RECSYear,B,D),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !dehumidifiers
          READ(IUNIT1,*) (DEHEQP(RECSYear,B,D),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !microwave ovens
          READ(IUNIT1,*) (MCOEQP(RECSYear,B,D),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !pool pumps  !MELs21
          READ(IUNIT1,*) (PLPEQP(RECSYear,B,D),B=1,mNumBldg)  !MELs21
         ENDDO

         DO D=1,mNumCR-2  !pool heaters  !MELs21
          READ(IUNIT1,*) (PLHEQP(RECSYear,B,D),B=1,mNumBldg)  !MELs21
         ENDDO  !MELs21

         DO D=1,mNumCR-2  !home security systems
          READ(IUNIT1,*) (SECEQP(RECSYear,B,D),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !portable electric spas
          READ(IUNIT1,*) (SPAEQP(RECSYear,B,D),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  ! wine coolers
          READ(IUNIT1,*) (WCLEQP(RECSYear,B,D),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !MELs21
          READ(IUNIT1,*) (SPKEQP(RECSYear,B,D),B=1,mNumBldg)  !MELs21
         ENDDO  !MELs21

         DO D=1,mNumCR-2  !MELs21
          READ(IUNIT1,*) (PHNEQP(RECSYear,B,D),B=1,mNumBldg)  !MELs21
         ENDDO  !MELs21

         DO D=1,mNumCR-2  !MELs21
          READ(IUNIT1,*) (TABEQP(RECSYear,B,D),B=1,mNumBldg)  !MELs21
         ENDDO  !MELs21

         DO D=1,mNumCR-2  !MELs21
          READ(IUNIT1,*) (KITEQP(RECSYear,B,D),B=1,mNumBldg)  !MELs21
         ENDDO  !MELs21

         DO D=1,mNumCR-2  !electric other equipment
          READ(IUNIT1,*) (EAEQP(RECSYear,B,D),B=1,mNumBldg)
         ENDDO

         DO E=1,7  !secondary heating equipment (natural gas, electric, distillate fuel oil + kerosene, propane, blank, blank, wood)	!kj - kerosene and coal values are placeholders; delete those columns	!kj - replace 7 with variable?
          DO D=1,mNumCR-2
           READ(IUNIT1,*) (SHTEQP(RECSYear,B,D,E), B=1,mNumBldg)
          ENDDO
         ENDDO

         DO E=1,3  !other appliances (natural gas, propane, distillate fuel oil + kerosene)	!kj - replace 3 with variable?
          DO D=1,mNumCR-2
           READ(IUNIT1,*) (APPEQP(RECSYear,B,D,E), B=1,mNumBldg)
          ENDDO
         ENDDO


!********************************************************************
!  CALCULATE TOTAL WATER HEATERS TO SHARE OUT CLOTHES AND DISWASHERS
!********************************************************************
      EU=5
      DO 51 D=1,mNumCR-2
       DO 51 B=1,mNumBldg
        HOTWATQ(RECSYear,B,D)=0.0
         DO 51 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          HOTWATQ(RECSYear,B,D)=HOTWATQ(RECSYear,B,D)+EQCESE(RECSYear,RECCL,B,D)
 51   CONTINUE

!***************************************************************************************
! PROJECT EXISTING HEATING EQUIPMENT BY APPLYING EQUIPMENT RETIREMENT/REPLACEMENT RATE
!***************************************************************************************
      EU=1                     ! SPACE HEATING
      DO 55 D=1,mNumCR-2
        DO 55 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          DO 55 B=1,mNumBldg
            DO 55 Y=RECSYear+1,EndYr
              EQCESE(Y,RECCL,B,D)=((EQCESE(RECSYear,RECCL,B,D)* &
                (HDR(B)**(Y-RECSYear))*(1.0-EQCRET(Y,RECCL))))
 55   CONTINUE

      EU = 2             ! SPACE COOLING SECTION OF THE DATA

!********************************************************************
!  PROJECT EXISTING COOLING EQUIPMENT SUBROUTINE
!  SET HEAT PUMP EQUAL TO PREVIOUS CALCULATED
!  APPLY RETIREMENT/REPLACEMENT RATE TO EQUIPMENT STOCK
!********************************************************************
      DO 65 D=1,mNumCR-2
       DO 65 B=1,mNumBldg
        DO 65 Y=RECSYear+1,EndYr
          DO 65 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            IF(RTCLPNTR(RECCL).EQ.0) THEN
              EQCESE(Y,RECCL,B,D)= (EQCESE(RECSYear,RECCL,B,D)* &
                (HDR(B)**(Y-RECSYear))*(1.0-EQCRET(Y,RECCL)))
            ELSE
              RECCLHP=RTCLPNTR(RECCL)
              EQCESE(Y,RECCL,B,D)=EQCESE(Y,RECCLHP,B,D)
            ENDIF
 65   CONTINUE

      EU = 3                 ! clothes washers
!********************************************************************
!     PROJECT EXISTING CLOTHES WASHER EQUIPMENT
!********************************************************************
      DO 68 D=1,mNumCR-2
        DO 68 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          DO 68 B=1,mNumBldg
             DO 68 Y=RECSYear+1,EndYr
              EQCESE(Y,RECCL,B,D)= (EQCESE(RECSYear,RECCL,B,D)* &
                (HDR(B)**(Y-RECSYear))*(1.0-EQCRET(y,RECCL)))
 68   CONTINUE

       EU = 4                ! DISHWASHERS
!********************************************************************
!     PROJECT EXISTING DISHWASHER EQUIPMENT
!********************************************************************
      DO 70 D=1,mNumCR-2
        DO 70 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          DO 70 B=1,mNumBldg
             DO 70 Y=RECSYear+1,EndYr
              EQCESE(Y,RECCL,B,D)= (EQCESE(RECSYear,RECCL,B,D)* &
                (HDR(B)**(Y-RECSYear))*(1.0-EQCRET(y,RECCL)))
 70   CONTINUE

      EU = 5

!   READ IN WATER HEATING EQUIPMENT SHARE FOR CURRENT RECS YEAR
!********************************************************************
!     PROJECT EXISTING WATER HEATING EQUIPMENT
!********************************************************************
      DO 77 D=1,mNumCR-2
        DO 77 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          DO 77 B=1,mNumBldg
            DO 77 Y=RECSYear+1,EndYr
              EQCESE(Y,RECCL,B,D)= (EQCESE(RECSYear,RECCL,B,D)* &
                (HDR(B)**(Y-RECSYear))*(1.0-EQCRET(y,RECCL)))
 77   CONTINUE

      EU = 6

!   READ IN COOKING SHARES FOR CURRENT RECS YEAR
!********************************************************************
!     PROJECT EXISTING COOKING EQUIPMENT
!********************************************************************
      DO 80 D=1,mNumCR-2
        DO 80 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          DO 80 B=1,mNumBldg
             DO 80 Y=RECSYear+1,EndYr
              EQCESE(Y,RECCL,B,D)= (EQCESE(RECSYear,RECCL,B,D)* &
                (HDR(B)**(Y-RECSYear))*(1.0-EQCRET(y,RECCL)))
 80   CONTINUE

      EU   = 7
      NEQC = RTCLEUPT(EU+1)-RTCLEUPT(EU)

!********************************************************************
!     PROJECT EXISTING DRYER EQUIPMENT
!********************************************************************
      DO 92 D=1,mNumCR-2
        DO 92 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          DO 92 B=1,mNumBldg
              DO 92 Y=RECSYear+1,EndYr
               EQCESE(Y,RECCL,B,D)= ((EQCESE(RECSYear,RECCL,B,D)* &
                (HDR(B)**(Y-RECSYear))*(1.0-EQCRET(y,RECCL))))
 92   CONTINUE

      EU = 8                 ! REFRIGERATORS
!********************************************************************
!     PROJECT EXISTING REFRIGERATION EQUIPMENT
!********************************************************************
      DO 94 D=1,mNumCR-2
        DO 94 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          DO 94 B=1,mNumBldg
             DO 94 Y=RECSYear+1,EndYr
              EQCESE(Y,RECCL,B,D)= (EQCESE(RECSYear,RECCL,B,D)* &
                (HDR(B)**(Y-RECSYear))*(1.0-EQCRET(y,RECCL)))
 94   CONTINUE

       EU = 9                 ! FREEZERS
!********************************************************************
!     PROJECT EXISTING STANDALONE FREEZER EQUIPMENT
!********************************************************************
      DO 96 D=1,mNumCR-2
        DO 96 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          DO 96 B=1,mNumBldg
             DO 96 Y=RECSYear+1,EndYr
              EQCESE(Y,RECCL,B,D)= (EQCESE(RECSYear,RECCL,B,D)* &
                (HDR(B)**(Y-RECSYear))*(1.0-EQCRET(y,RECCL)))
 96   CONTINUE
      IUNIT1=FILE_MGR('C',FNAME,NEW)  !close_RSSTK
      END SUBROUTINE RDHTREQC

!*******************************************************************
!     READ IN RETIRING EFFICIENCIES
!*******************************************************************
      SUBROUTINE RDEFF
      INTEGER  Y, RECCL
      LOGICAL NEW
      CHARACTER*18 FNAME
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
      INTEGER  IUNIT1
!********************************************************************
      FNAME='RSEFF01'
      NEW=.FALSE.
      IUNIT1=FILE_MGR('O',FNAME,NEW)
      READ(IUNIT1,'(19(/))')
      DO 50 RECCL=1,RTCLCNT
      READ(IUNIT1,*) &
        (EQCEFF(Y,RECCL), Y=RECSYear+1,ijumpcalyr)
 50   CONTINUE

! Freeze retiring efficiency at the level 8 years out to approximate
!      DO Y=RECSYear+1,ijumpcalyr
!        DO RECCL=1,rtclcnt
!         eqceff(y,RECCL)=eqceff(RECSYear+8,RECCL)
!        ENNDO
!      ENNDO

      IUNIT1=FILE_MGR('C',FNAME,NEW)
      END SUBROUTINE RDEFF


!*******************************************************************
!     READ IN RECS-YEAR STOCK EFFICIENCIES
!*******************************************************************
      SUBROUTINE RDSTEFF
      INTEGER  Y, RECCL
      LOGICAL NEW
      CHARACTER*18 FNAME
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
      INTEGER  IUNIT1
!********************************************************************
      FNAME='RSSTKEFF'
      NEW=.FALSE.
      IUNIT1=FILE_MGR('O',FNAME,NEW)
      READ(IUNIT1,'(19(/))')
      DO 50 RECCL=1,RTCLCNT
      READ(IUNIT1,*) &
        (STKEFF(Y,RECCL), Y=RECSYear,ijumpcalyr)
 50   CONTINUE

      IUNIT1=FILE_MGR('C',FNAME,NEW)
      END SUBROUTINE RDSTEFF


!*******************************************************************
!     READ IN ENERGY STAR LEARNING FACTORS AS WELL AS ALL SHELL BETAS TO BENCHMARK ENERGY STAR HOME SHARES  !RSESTARbetas
!*******************************************************************
      SUBROUTINE RDESTARHOMES
      IMPLICIT NONE
      INTEGER  B, D, E, S, Y
      !Input variables declared in RTEK includes file
      LOGICAL NEW
      CHARACTER*18 FNAME
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
      INTEGER  IUNIT1

      FNAME='RSESTAR'
      NEW=.FALSE.
      IUNIT1=FILE_MGR('O',FNAME,NEW)
      READ(IUNIT1,'(19(/))')               ! SKIP 20-LINE HEADER
      READ(IUNIT1,*) ESTARHISTYR  !RSESTARbetas
!       WRITE(9,*)  'ESTARHISTYR', ESTARHISTYR  !RSESTARbetas

      ALLOCATE(HVBETA1(RECSYear+1:ESTARHISTYR,mNumBldg,nShellTypes,mNumCR)) !RSESTARbetas
      ALLOCATE(HVBETA2(RECSYear+1:ESTARHISTYR,mNumBldg,nShellTypes,mNumCR)) !RSESTARbetas
       HVBETA1=0.0 !RSESTARbetas
       HVBETA2=0.0 !RSESTARbetas

      READ(IUNIT1,'(//)')                ! SKIP 3-LINE HEADER  !RSESTARbetas
      DO B=1,mNumBldg  !RSESTARbetas
        DO S=1,nShellTypes  !RSESTARbetas
          DO D=1,mNumCR-2  !RSESTARbetas
            READ(IUNIT1,*) (HVBETA1(Y,B,S,D), Y=RECSYear+1,ESTARHISTYR)  !RSESTARbetas
          ENDDO !D loop for Census division  !RSESTARbetas
        ENDDO !S loop for building shell levels  !RSESTARbetas
      ENDDO !B loop for building type  !RSESTARbetas

      READ(IUNIT1,'(//)')                ! SKIP 3-LINE HEADER  !RSESTARbetas
      DO B=1,mNumBldg  !RSESTARbetas
        DO S=1,nShellTypes  !RSESTARbetas
          DO D=1,mNumCR-2  !RSESTARbetas
            READ(IUNIT1,*) (HVBETA2(Y,B,S,D), Y=RECSYear+1,ESTARHISTYR)  !RSESTARbetas
          ENDDO !D loop for Census division  !RSESTARbetas
        ENDDO !S loop for building shell levels  !RSESTARbetas
      ENDDO !B loop for building type  !RSESTARbetas

      READ(IUNIT1,'(//)')                ! SKIP 3-LINE HEADER  !RSESTARbetas
      DO D=1,mNumCR-2  !RSESTARbetas
         READ(IUNIT1,*)(LEARNFACT(B,D),B=1,mNumBldg)  !RSESTARbetas
      ENDDO  !RSESTARbetas

      IUNIT1=FILE_MGR('C',FNAME,NEW)
      END SUBROUTINE RDESTARHOMES

!*******************************************************************
!     READ IN INITIAL UECS
!*******************************************************************
      SUBROUTINE RDUECS
      IMPLICIT NONE
! SECONDARY HEATING FUEL=7=G,E,D,L,K,C,W
! APPLIANCE FUEL=2=Natural Gas, Propane
      INTEGER  B, D, E, F, EU, EQC, RECCL, RECCL1
      LOGICAL NEW
      CHARACTER*18 FNAME
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
      INTEGER  IUNIT1

      FNAME='RSUEC'
      NEW=.FALSE.
      IUNIT1=FILE_MGR('O',FNAME,NEW)
      READ(IUNIT1,'(19(/))')


        EU = 1             ! SPACE HEATING SECTION OF THE DATA

        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
         DO D=1,mNumCR-2
          READ(IUNIT1,*) (EQCUEC(D,RECCL,B), B=1,mNumBldg)
         ENDDO
        ENDDO

        EU = 2             ! SPACE COOLING SECTION OF THE DATA

        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
         DO D=1,mNumCR-2
          READ(IUNIT1,*) (EQCUEC(D,RECCL,B), B=1,mNumBldg)
         ENDDO
        ENDDO

        EU = 3             ! CLOTHES WASHER SECTION OF THE DATA

        RECCL1=RTCLEUPT(EU)+1
        DO D=1,mNumCR-2
         READ(IUNIT1,*)(EQCUEC(D,RECCL1,B),B=1,mNumBldg)
        ENDDO
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
         DO D=1,mNumCR-2
          DO B=1,mNumBldg
            EQCUEC(D,RECCL,B)=EQCUEC(D,RECCL1,B)
          ENDDO
         ENDDO
        ENDDO

        EU = 4             ! DISHWASHER SECTION OF THE DATA

        RECCL1=RTCLEUPT(EU)+1
        DO D=1,mNumCR-2
         READ(IUNIT1,*)(EQCUEC(D,RECCL1,B),B=1,mNumBldg)
        ENDDO
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
         DO D=1,mNumCR-2
          DO B=1,mNumBldg
            EQCUEC(D,RECCL,B)=EQCUEC(D,RECCL1,B)
          ENDDO
         ENDDO
        ENDDO

        EU = 5             ! WATER HEATING SECTION OF THE DATA

        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
         DO D=1,mNumCR-2
          READ(IUNIT1,*) (EQCUEC(D,RECCL,B), B=1,mNumBldg)
         ENDDO
        ENDDO

        EU = 6             ! COOKING SECTION OF THE DATA

        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
         DO D=1,mNumCR-2
          READ(IUNIT1,*) (EQCUEC(D,RECCL,B), B=1,mNumBldg)
         ENDDO
        ENDDO

        EU = 7             ! CLOTHES DRYER SECTION OF THE DATA

        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
         DO D=1,mNumCR-2
          READ(IUNIT1,*) (EQCUEC(D,RECCL,B), B=1,mNumBldg)
         ENDDO
        ENDDO

        EU = 8             ! FOOD REFRIGERATION SECTION OF THE DATA

        RECCL1=RTCLEUPT(EU)+1
        DO D=1,mNumCR-2
         READ(IUNIT1,*)(EQCUEC(D,RECCL1,B),B=1,mNumBldg)
        ENDDO
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
         DO D=1,mNumCR-2
          DO B=1,mNumBldg
            EQCUEC(D,RECCL,B)=EQCUEC(D,RECCL1,B)
          ENDDO
         ENDDO
        ENDDO

        EU = 9             ! FOOD FREEZING SECTION OF THE DATA

        RECCL1=RTCLEUPT(EU)+1
        DO D=1,mNumCR-2
         READ(IUNIT1,*)(EQCUEC(D,RECCL1,B),B=1,mNumBldg)
        ENDDO
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
         DO D=1,mNumCR-2
          DO B=1,mNumBldg
            EQCUEC(D,RECCL,B)=EQCUEC(D,RECCL1,B)
          ENDDO
         ENDDO
        ENDDO

  ! MELs SECTION OF THE DATA (NO END-USE NUMBER)

         DO D=1,mNumCR-2  !furnace fans
          READ(IUNIT1,*) (FANUEC(D,B),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !televisions
          READ(IUNIT1,*) (TVSUEC(D,B),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !set-top boxes
          READ(IUNIT1,*) (STBUEC(D,B),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !home theater systems
          READ(IUNIT1,*) (HTSUEC(D,B),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !Over-the-top (OTT) streaming devices
          READ(IUNIT1,*) (OTTUEC(D,B),B=1,mNumBldg)  !MELs21
         ENDDO

         DO D=1,mNumCR-2  !video game consoles
          READ(IUNIT1,*) (VGCUEC(D,B),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !desktop personal computers
          READ(IUNIT1,*) (DPCUEC(D,B),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !laptop personal computers
          READ(IUNIT1,*) (LPCUEC(D,B),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !computer monitors
          READ(IUNIT1,*) (MONUEC(D,B),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !computer networking equipment
          READ(IUNIT1,*) (NETUEC(D,B),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !rechargeable devices
          READ(IUNIT1,*) (BATUEC(D,B),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !ceiling fans
          READ(IUNIT1,*) (CFNUEC(D,B),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !coffee makers
          READ(IUNIT1,*) (COFUEC(D,B),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !dehumidifiers
          READ(IUNIT1,*) (DEHUEC(D,B),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !microwave ovens
          READ(IUNIT1,*) (MCOUEC(D,B),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !pool pumps  !MELs21
          READ(IUNIT1,*) (PLPUEC(D,B),B=1,mNumBldg)  !MELs21
         ENDDO

         DO D=1,mNumCR-2  !pool heaters  !MELs21
          READ(IUNIT1,*) (PLHUEC(D,B),B=1,mNumBldg)  !MELs21
         ENDDO  !MELs21

         DO D=1,mNumCR-2  !home security systems
          READ(IUNIT1,*) (SECUEC(D,B),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !portable electric spas
          READ(IUNIT1,*) (SPAUEC(D,B),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  ! wine coolers
          READ(IUNIT1,*) (WCLUEC(D,B),B=1,mNumBldg)
         ENDDO

         DO D=1,mNumCR-2  !MELs21
          READ(IUNIT1,*) (SPKUEC(D,B),B=1,mNumBldg)  !MELs21
         ENDDO  !MELs21

         DO D=1,mNumCR-2  !MELs21
          READ(IUNIT1,*) (PHNUEC(D,B),B=1,mNumBldg)  !MELs21
         ENDDO  !MELs21

         DO D=1,mNumCR-2  !MELs21
          READ(IUNIT1,*) (TABUEC(D,B),B=1,mNumBldg)  !MELs21
         ENDDO  !MELs21

         DO D=1,mNumCR-2  !MELs21
          READ(IUNIT1,*) (KITUEC(D,B),B=1,mNumBldg)  !MELs21
         ENDDO  !MELs21

         DO D=1,mNumCR-2  !electric other equipment
          READ(IUNIT1,*) (EAUEC(D,B),B=1,mNumBldg)
         ENDDO

         DO E=1,7  !secondary heating equipment (natural gas, electric, distillate fuel oil, propane, kerosene, coal, wood)	!kj - kerosene (and coal) values are placeholders; combined with distillate fuel oil
          DO D=1,mNumCR-2
           READ(IUNIT1,*) (SHTUEC(D,E,B), B=1,mNumBldg)
          ENDDO
         ENDDO

         DO E=1,3  !other appliances (natural gas, propane, distillate fuel oil)
          DO D=1,mNumCR-2
           READ(IUNIT1,*) (APPUEC(D,E,B), B=1,mNumBldg)
          ENDDO
         ENDDO

      IUNIT1=FILE_MGR('C',FNAME,NEW)
100   FORMAT(2X,3(F9.4,3X))
      END SUBROUTINE RDUECS


!********************************************************************
!     CALCULATE CONSUMPTION IN RECS YEAR
!       CALLED ONLY FOR CurIYr = RECSYear
!*******************************************************************
      SUBROUTINE EXCONS
      IMPLICIT NONE
      REAL*4  TVSCON(mNumYr,mNumCR-2),STBCON(mNumYr,mNumCR-2),HTSCON(mNumYr,mNumCR-2),OTTCON(mNumYr,mNumCR-2),&  !MELs21
              VGCCON(mNumYr,mNumCR-2)
      REAL*4  DPCCON(mNumYr,mNumCR-2),LPCCON(mNumYr,mNumCR-2),MONCON(mNumYr,mNumCR-2),NETCON(mNumYr,mNumCR-2)
      REAL*4  BATCON(mNumYr,mNumCR-2),CFNCON(mNumYr,mNumCR-2),COFCON(mNumYr,mNumCR-2),DEHCON(mNumYr,mNumCR-2),&
              MCOCON(mNumYr,mNumCR-2),PLPCON(mNumYr,mNumCR-2),SPACON(mNumYr,mNumCR-2),WCLCON(mNumYr,mNumCR-2),&    !winecool  !MELs21
              SPKCON(mNumYr,mNumCR-2),PHNCON(mNumYr,mNumCR-2),TABCON(mNumYr,mNumCR-2),KITCON(mNumYr,mNumCR-2),&  !MELs21
              PLHCON(mNumYr,mNumCR-2),SECCON(mNumYr,mNumCR-2),EACON(mNumYr,mNumCR-2)  !MELs21

      REAL*4  TVSCONUS(mNumYr),STBCONUS(mNumYr),HTSCONUS(mNumYr),OTTCONUS(mNumYr),VGCCONUS(mNumYr)  !MELs21
      REAL*4  DPCCONUS(mNumYr),LPCCONUS(mNumYr),MONCONUS(mNumYr),NETCONUS(mNumYr)
      REAL*4  BATCONUS(mNumYr),CFNCONUS(mNumYr),COFCONUS(mNumYr),DEHCONUS(mNumYr),&
              MCOCONUS(mNumYr),PLPCONUS(mNumYr),SPACONUS(mNumYr),WCLCONUS(mNumYr),&    !winecool  !MELs21
              SPKCONUS(mNumYr),PHNCONUS(mNumYr),TABCONUS(mNumYr),KITCONUS(mNumYr),&  !MELs21
              PLHCONUS(mNumYr),SECCONUS(mNumYr),EACONUS(mNumYr)  !MELs21

      INTEGER D,E,F,B,FCON,EU,EQC,RECCL,EQCGHP,EQCEHP,EQCEWH,EQCSWH,y
      INTEGER RECCLGHP,RECCLEHP,RECCLEWH,RECCLSWH

          ! 111(d) - initialize sales based on input restart file, against which to track savings (written to RESOUT.txt)
          WRITE(9,*) 'RESTART FILE baseline electricity data (Trills) d, y, QELRS(d,y), QELCM(d,y)'
          DO D=1,mNumCR-2
           DO y=1,mNumYr
           ! QELRS,QELCM in Trills	!kj - QELRS and QELCM are normally in quads, but these aren't multiplied by 1000 here. But this is legacy EPA 111D code, so not really used.
           WRITE(9,5) D,Y,QELRS(d,y),QELCM(d,y)
           ENDDO
          ENDDO
 5    FORMAT(2i5,2F12.5)

!  SET HOT WATER LOAD ADDITIONS FOR CLOTHESWASHERS AND DISHWASHERS	!kj -  Load adjustment of clothes washers with respect to water heating load in RECSyear
      CWLOAD(RECSYear)=0.2047

!CALCULATE HEATING CONSUMPTION RECSYear,F,D,B
      DO D=1,mNumCR-2
        DO F=1,NHTRFL
          HTRCON(CurIYr,F,D)=0.0
          SHTCON(CurIYr,F,D)=0.0
        ENDDO
      ENDDO

      ! SET EU = 1 FOR SPACE HEATING IN RSCLASS
      EU = 1
      ! CALCULATE HEATING CONSUMPTION FOR THE IDENTIFIED FUELS IN RSCLASS
      ! AT THE SAME TIME IDENTIFY ELECTRIC AIR-SOURCE HEAT PUMPS AND GEOTHERMAL HEAT PUMPS FOR LATER USE
      ! LOOP OVER ALL HEATING EQUIPMNENT TYPES
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1) !ALL RECORDS IN RSCLASS
        EQC=RTCLEQCL(RECCL)
        F  =RTFUEL(RECCL)
        ! MAP RSCLASS FUEL NUMBERS INTO HTRCON FUEL NUMBERS
        FCON=FHTRCON(F)
            ! ALSO FIND INDICES FOR THE ELECTRIC AND GEOTHERMAL HEAT PUMPS.
            ! THESE ARE USED TO COMPUTE CONSUMPTION FOR GEOTHERMAL AND IDENTIFY ELECTRIC VS GEOTHERMAL HEAT PUMP EQUIPMENT.
            ! GEOTHERMAL USE (UNDER FUEL 7) IS IN ADDITION TO ELECTRICITY, ITS DESIGNATED FUEL UNDER RSCLASS.
            ! BECAUSE FUEL 7 IS NOT ENCOUNTERED IN THIS LOOP THROUGH RSCLASS, CALCULATE BELOW AFTER THIS LOOP.
            IF(RTCLNAME(RECCL).EQ.'ELEC_HP')THEN
              EQCEHP=RTCLEQCL(RECCL)
              RECCLEHP=EQCEHP
             ELSEIF(RTCLNAME(RECCL).EQ.'GEO_HP')THEN
              EQCGHP=RTCLEQCL(RECCL)
              RECCLGHP=EQCGHP
            ENDIF
        ! NOW THAT FUEL IS MAPPED, AGGREGATE BY D AND B
        DO D=1,mNumCR-2
          DO B=1,mNumBldg
            HTRCON(CurIYr,FCON,D)=HTRCON(CurIYr,FCON,D)+ &
              (EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B))
          ENDDO !BUILDING TYPES, B
        ENDDO !CENSUS DIVISIONS, D
      ENDDO !HEATING EQUIPMENT TYPES, RECCL

      ! FINALLY, AGGREGATE GEOTHERMAL NOW THAT RSCLASS HAS BEEN PROCESSED
      FCON = FHTRCON(7)  !7 FOR HEATING
      DO D=1,mNumCR-2
        DO B=1,mNumBldg
         ! NOW THAT ELEC_HP AND GEO_HP ARE IDENTIFIED, CALCULATE CONSUMPTION.
         ! FOR GEOTHERMAL (FUEL 7) AS THE DIFFERENCE BETWEEN THE ELEC_HP AND GEO_HP UEC
         ! WHRFOSS/3412 USED TO CONVERT ELECTRICITY INTO PRIMARY GEOTHERMAL USE BASED ON AVERAGE FOSSIL FUEL HEAT RATE.  !STEOhr
         HTRCON(CurIYr,FCON,D)=HTRCON(CurIYr,FCON,D)+ &
          EQCESE(RECSYear,RECCLGHP,B,D)*(EQCUEC(D,RECCLEHP,B)-EQCUEC(D,RECCLGHP,B))*WHRFOSS(D,CurIYr)/3412.  !STEOhr
        ENDDO !BUILDING TYPES, B
      ENDDO !CENSUS DIVISIONS, D

!CALCULATE COOLING CONSUMPTION
      DO D=1,mNumCR-2
        DO F=1,NCLFL
          COOLCN(CurIYr,F,D)=0.0
        ENDDO
      ENDDO

      ! SET EU = 2 FOR SPACE COOLING IN RSCLASS
      EU = 2
      ! CALCULATE COOLING CONSUMPTION FOR THE IDENTIFIED FUELS IN RSCLASS
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1) !ALL RECORDS IN RSCLASS
        EQC=RTCLEQCL(RECCL)
        F  =RTFUEL(RECCL)
        ! MAP RSCLASS FUEL NUMBERS INTO CLCON FUEL NUMBERS
        FCON=FCLCON(F)
        ! ALSO FIND INDICES FOR THE ELECTRIC AND GEOTHERMAL HEAT PUMPS AS FOR HEATING
        IF(RTCLNAME(RECCL).EQ.'ELEC_HP')THEN
          EQCEHP=RTCLEQCL(RECCL)
          ! NOTE START NUMBERING AFTER HEATERS -- NEED TO INCREMENT EQCEHP BY HEATERS
          RECCLEHP=EQCEHP+RTCLEUPT(EU)
        ELSEIF(RTCLNAME(RECCL).EQ.'GEO_HP')THEN
          EQCGHP=RTCLEQCL(RECCL)
          ! NOTE START NUMBERING AFTER HEATERS -- NEED TO INCREMENT EQCGHP BY HEATERS
          RECCLGHP=EQCGHP+RTCLEUPT(EU)
        ENDIF

        ! NOW THAT FUEL IS MAPPED, AGGREGATE BY D AND B
        DO D=1,mNumCR-2
          DO B=1,mNumBldg
            COOLCN(CurIYr,FCON,D)=COOLCN(CurIYr,FCON,D)+ &
              (EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B))
          ENDDO !BUILDING TYPES, B
        ENDDO !CENSUS DIVISIONS, D
      ENDDO !COOLING EQUIPMENT TYPES, RECCL

      ! FINALLY, AGGREGATE GEOTHERMAL NOW THAT RSCLASS HAS BEEN PROCESSED
      FCON = FCLCON(7) !=2 FOR GEOTHERMAL COOLING
      DO D=1,mNumCR-2
        DO B=1,mNumBldg
        COOLCN(CurIYr,FCON,D)=COOLCN(CurIYr,FCON,D)+ &
         EQCESE(RECSYear,RECCLGHP,B,D)*(EQCUEC(D,RECCLEHP,B)-EQCUEC(D,RECCLGHP,B))*WHRFOSS(D,CurIYr)/3412.  !STEOhr
        ENDDO !BUILDING TYPES, B
      ENDDO !CENSUS DIVISIONS, D

!CALCULATE CLOTHES WASHER CONSUMPTION
!   SET EU = 3 TO SEARCH THE CLOTHES WASHER SECTION OF THE DATA
      EU = 3
      DO 160 D=1,mNumCR-2
        DO 160 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          CSWCON(CurIYr,D)=0.0
          DO 160 B=1,mNumBldg
            CSWCON(CurIYr,D)=CSWCON(CurIYr,D)+ &
             (EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B))
 160  CONTINUE

!CALCULATE DISHWASHER CONSUMPTION
!   SET EU = 4 TO SEARCH THE DISHWASHER SECTION OF THE DATA
      EU = 4
      DO 170 D=1,mNumCR-2
        DO 170 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          DSWCON(CurIYr,D)=0.0
          DO 170 B=1,mNumBldg
            DSWCON(CurIYr,D)=DSWCON(CurIYr,D)+ &
             (EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B))
 170  CONTINUE

!CALCULATE WATER HEATING CONSUMPTION
!ALSO CALCULATE SOLAR CONSUMPTION - USES EL UEC (55 PERCENT)	!kj
!   SET EU = 5 TO SEARCH THE WATER HEATING SECTION OF THE DATA
      EU = 5

      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)

!*******************************************************************
!  FIND INDICES FOR THE ELECTRIC AND SOLAR WATER HEATERS
!    USED TO COMPUTE H2OCON FOR SOLAR FUEL (FCON=5)
!*******************************************************************
        IF(RTCLNAME(RECCL).EQ.'ELEC_WH')THEN
          EQCEWH=RTCLEQCL(RECCL)
          !AS FOR COOLING, INCREMENT THE EQUIPMENT CLASS BY THE SUM OF CLASSES BEFORE IT
          RECCLEWH=EQCEWH+RTCLEUPT(EU)
        ELSEIF(RTCLNAME(RECCL).EQ.'SOLAR_WH')THEN
          EQCSWH=RTCLEQCL(RECCL)
          !AS FOR COOLING, INCREMENT THE EQUIPMENT CLASS BY THE SUM OF CLASSES BEFORE IT
          RECCLSWH=EQCSWH+RTCLEUPT(EU)
        ENDIF
      ENDDO

      DO D=1,mNumCR-2
       SLCON(CurIYr,D)=0.0
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC  = RTCLEQCL(RECCL)
          F  =RTFUEL(RECCL)
          FCON = FWHCON(F)
           H2OCON(CurIYr,FCON,D)=0.0
        ENDDO
      ENDDO
       DO D=1,mNumCR-2
        DO B=1,mNumBldg
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC  = RTCLEQCL(RECCL)
          F  =RTFUEL(RECCL)
          FCON = FWHCON(F)
          H2OCON(CurIYr,FCON,D)=H2OCON(CurIYr,FCON,D)+(EQCUEC(D,RECCL,B)*EQCESE(RECSYear,RECCL,B,D))
        ENDDO

!    SOLAR IS COMPUTED DIFFERENTLY
!        FCON = FWHCON(8)

        H2OCON(CurIYr,5,D)=H2OCON(CurIYr,5,D)+ &
         (EQCESE(RECSYear,RECCLSWH,B,D)* &
         (EQCUEC(D,RECCLEWH,B)-EQCUEC(D,RECCLSWH,B))*WHRFOSS(D,CurIYr)/3412.)  !STEOhr
        SLCON(CurIYr,D)=SLCON(CurIYr,D)+ &
         (EQCESE(RECSYear,RECCLSWH,B,D)* &
         (EQCUEC(D,RECCLEWH,B)-EQCUEC(D,RECCLSWH,B))*WHRFOSS(D,CurIYr)/3412.)  !STEOhr
        SLEQCN(CurIYr,1,B,D)=(EQCESE(RECSYear,RECCLSWH,B,D)* &
         (EQCUEC(D,RECCLEWH,B)-EQCUEC(D,RECCLSWH,B))*WHRFOSS(D,CurIYr)/3412.)  !STEOhr

        ENDDO
       ENDDO


!*******************************************************************
!  CALCULATE COOKING CONSUMPTION
!   SET EU = 6 TO SEARCH THE COOKING SECTION OF THE DATA
!*******************************************************************
      EU = 6

!*******************************************************************
      DO 250 D=1,mNumCR-2
        DO 250 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          CKCON(CurIYr,EQC,D)=0.0
          DO 250 B=1,mNumBldg
            CKCON(CurIYr,EQC,D)=CKCON(CurIYr,EQC,D)+ &
              (EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B))
 250  CONTINUE

!*******************************************************************
!  CALCULATE CLOTHES DRYER CONSUMPTION
!   SET EU = 7 TO SEARCH THE CLOTHES DRYER SECTION OF THE DATA
!*******************************************************************
      EU = 7

!*******************************************************************
      DO 270 D=1,mNumCR-2
        DO 270 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          DRYCON(CurIYr,EQC,D)=0.0
          DO 270 B=1,mNumBldg
            DRYCON(CurIYr,EQC,D)=DRYCON(CurIYr,EQC,D)+ &
              (EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B))
 270  CONTINUE
!*******************************************************************
!  CALCULATE REFRIGERATOR CONSUMPTION
!   SET EU = 8 TO SEARCH THE FOOD REFRIGERATION SECTION OF THE DATA
!*******************************************************************
      EU = 8

!*******************************************************************
      DO 300 D=1,mNumCR-2
        DO 300 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          REFCON(CurIYr,D)=0.0
          DO 300 B=1,mNumBldg
            REFCON(CurIYr,D)=REFCON(CurIYr,D)+ &
             (EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B))
 300  CONTINUE
!*******************************************************************
!  CALCULATE FREEZER CONSUMPTION
!   SET EU = 9 TO SEARCH THE FOOD FREEZING SECTION OF THE DATA
!*******************************************************************
      EU = 9

      DO 400 D=1,mNumCR-2
        DO 400 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          FRZCON(CurIYr,D)=0.0
          DO 400 B=1,mNumBldg
            FRZCON(CurIYr,D)=FRZCON(CurIYr,D)+ &
             (EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B))
  400  CONTINUE

!*******************************************************************
! moved to Lighting Subroutine  CALCULATE LIGHTING CONSUMPTION
!********************************************************************
!     DO 500 D=1,mNumCR-2
!        LTCON(CurIYr,D)=0.0
!        DO 500 B=1,mNumBldg
!         DO 500 E=1,4
!          LTCON(CurIYr,D)=LTCON(CurIYr,D)+(LTEQP(RECSYear,E,B,D)*LTUEC(E,D,B))
!  500   CONTINUE
!
!*******************************************************************
!  CALCULATE PERSONAL COMPUTER CONSUMPTION
!********************************************************************
!     DO 510 D=1,mNumCR-2
!        PCCON(CurIYr,D)=0.0
!        DO 510 B=1,mNumBldg
!          PCCON(CurIYr,D)=PCCON(CurIYr,D)+(PCEQP(RECSYear,B,D)*PCUEC(D,B))
! 510   CONTINUE
!
!*******************************************************************
!  CALCULATE COLOR TV CONSUMPTION
!********************************************************************
!      DO 520 D=1,mNumCR-2
!        TVCON(CurIYr,D)=0.0
!        DO 520 B=1,mNumBldg
!          TVCON(CurIYr,D)=TVCON(CurIYr,D)+(TVEQP(RECSYear,B,D)*TVUEC(D,B))+ &
!          STBCON(CurIYr,D)+(STBEQP(RECSYear,B,D)*STBUEC(D,B))+ &
!          VGCON(CurIYr,D)+(VGEQP(RECSYear,B,D)*VGUEC(D,B))
! 520   CONTINUE
!
!*******************************************************************
!  CALCULATE FURNACE FAN
!********************************************************************
      DO 521 D=1,mNumCR-2
         FANCON(CurIYr,D)=0.0
         DO 521 B=1,mNumBldg
         FANEQCN(CurIYr,1,b,d)=(FANEQP(RECSYear,B,D)*FANUEC(D,B))
         FANCON(CurIYr,D)=FANCON(CurIYr,D)+(FANEQP(RECSYear,B,D)*FANUEC(D,B))
521   CONTINUE

!*******************************************************************
!  CALCULATE CONSUMPTION FOR MISCELLANEOUS ELECTRIC LOADS (MELs)
!   TELEVISIONS, SET-TOP BOXES, HOME THEATER SYSTEMS, OVER-THE-TOP (OTT) STREAMING DEVICES, VIDEO GAME CONSOLES,
!   DESKTOP PCs, LAPTOP PCs, MONITORS, NETWORKING EQUIPMENT, NON-PC RECHARGEABLES,CEILING FANS, COFFEE MAKERS, DEHUMIDIFIERS,
!   MICROWAVES, POOL PUMPS, POOL HEATERS, SECURITY SYSTEMS, PORTABLE ELECTRIC SPAS, PORTABLE REFRIGERATION EQUIPMENT (WINE COOLERS),
!   SMART SPEAKERS, SMARTPHONES, TABLETS, SMALL KITCHEN APPLIANCES, OTHER ELECTRIC APPLIANCES
!********************************************************************
      DO 523 D=1,mNumCR-2
        TVSCON(CurIYr,D)=0.0
        STBCON(CurIYr,D)=0.0
        HTSCON(CurIYr,D)=0.0
        OTTCON(CurIYr,D)=0.0  !MELs21
        VGCCON(CurIYr,D)=0.0
        DPCCON(CurIYr,D)=0.0
        LPCCON(CurIYr,D)=0.0
        MONCON(CurIYr,D)=0.0
        NETCON(CurIYr,D)=0.0
        BATCON(CurIYr,D)=0.0
        CFNCON(CurIYr,D)=0.0
        COFCON(CurIYr,D)=0.0
        DEHCON(CurIYr,D)=0.0
        MCOCON(CurIYr,D)=0.0
        PLPCON(CurIYr,D)=0.0  !MELs21
        PLHCON(CurIYr,D)=0.0  !MELs21
        SECCON(CurIYr,D)=0.0
        SPACON(CurIYr,D)=0.0
        WCLCON(CurIYr,D)=0.0    !winecool
        SPKCON(CurIYr,D)=0.0  !MELs21
        PHNCON(CurIYr,D)=0.0  !MELs21
        TABCON(CurIYr,D)=0.0  !MELs21
        KITCON(CurIYr,D)=0.0  !MELs21
        EACON(CurIYr,D)=0.0
        DO 523 B=1,mNumBldg
          TVSCON(CurIYr,D)=TVSCON(CurIYr,D)+(TVSEQP(RECSYear,B,D)*TVSUEC(D,B))
          STBCON(CurIYr,D)=STBCON(CurIYr,D)+(STBEQP(RECSYear,B,D)*STBUEC(D,B))
          HTSCON(CurIYr,D)=HTSCON(CurIYr,D)+(HTSEQP(RECSYear,B,D)*HTSUEC(D,B))
          OTTCON(CurIYr,D)=OTTCON(CurIYr,D)+(OTTEQP(RECSYear,B,D)*OTTUEC(D,B))  !MELs21
          VGCCON(CurIYr,D)=VGCCON(CurIYr,D)+(VGCEQP(RECSYear,B,D)*VGCUEC(D,B))
          DPCCON(CurIYr,D)=DPCCON(CurIYr,D)+(DPCEQP(RECSYear,B,D)*DPCUEC(D,B))
          LPCCON(CurIYr,D)=LPCCON(CurIYr,D)+(LPCEQP(RECSYear,B,D)*LPCUEC(D,B))
          MONCON(CurIYr,D)=MONCON(CurIYr,D)+(MONEQP(RECSYear,B,D)*MONUEC(D,B))
          NETCON(CurIYr,D)=NETCON(CurIYr,D)+(NETEQP(RECSYear,B,D)*NETUEC(D,B))
          BATCON(CurIYr,D)=BATCON(CurIYr,D)+(BATEQP(RECSYear,B,D)*BATUEC(D,B))
          CFNCON(CurIYr,D)=CFNCON(CurIYr,D)+(CFNEQP(RECSYear,B,D)*CFNUEC(D,B))
          COFCON(CurIYr,D)=COFCON(CurIYr,D)+(COFEQP(RECSYear,B,D)*COFUEC(D,B))
          DEHCON(CurIYr,D)=DEHCON(CurIYr,D)+(DEHEQP(RECSYear,B,D)*DEHUEC(D,B))
          MCOCON(CurIYr,D)=MCOCON(CurIYr,D)+(MCOEQP(RECSYear,B,D)*MCOUEC(D,B))
          PLPCON(CurIYr,D)=PLPCON(CurIYr,D)+(PLPEQP(RECSYear,B,D)*PLPUEC(D,B))  !MELs21
          PLHCON(CurIYr,D)=PLHCON(CurIYr,D)+(PLHEQP(RECSYear,B,D)*PLHUEC(D,B))  !MELs21
          SECCON(CurIYr,D)=SECCON(CurIYr,D)+(SECEQP(RECSYear,B,D)*SECUEC(D,B))
          SPACON(CurIYr,D)=SPACON(CurIYr,D)+(SPAEQP(RECSYear,B,D)*SPAUEC(D,B))
          WCLCON(CurIYr,D)=WCLCON(CurIYr,D)+(WCLEQP(RECSYear,B,D)*WCLUEC(D,B))    !winecool
          SPKCON(CurIYr,D)=SPKCON(CurIYr,D)+(SPKEQP(RECSYear,B,D)*SPKUEC(D,B))  !MELs21
          PHNCON(CurIYr,D)=PHNCON(CurIYr,D)+(PHNEQP(RECSYear,B,D)*PHNUEC(D,B))  !MELs21
          TABCON(CurIYr,D)=TABCON(CurIYr,D)+(TABEQP(RECSYear,B,D)*TABUEC(D,B))  !MELs21
          KITCON(CurIYr,D)=KITCON(CurIYr,D)+(KITEQP(RECSYear,B,D)*KITUEC(D,B))  !MELs21
          EACON(CurIYr,D)=EACON(CurIYr,D)+(EH(RECSYear,B,D)*EAUEC(D,B))
          EAEQCN(CurIYr,1,B,D)=EH(RECSYear,B,D)*EAUEC(D,B)
 523   CONTINUE
        TVSCONUS(CurIYr)=0.0
        STBCONUS(CurIYr)=0.0
        HTSCONUS(CurIYr)=0.0
        OTTCONUS(CurIYr)=0.0  !MELs21
        VGCCONUS(CurIYr)=0.0
        DPCCONUS(CurIYr)=0.0
        LPCCONUS(CurIYr)=0.0
        MONCONUS(CurIYr)=0.0
        NETCONUS(CurIYr)=0.0
        BATCONUS(CurIYr)=0.0
        CFNCONUS(CurIYr)=0.0
        COFCONUS(CurIYr)=0.0
        DEHCONUS(CurIYr)=0.0
        MCOCONUS(CurIYr)=0.0
        PLPCONUS(CurIYr)=0.0  !MELs21
        PLHCONUS(CurIYr)=0.0  !MELs21
        SPACONUS(CurIYr)=0.0
        WCLCONUS(CurIYr)=0.0    !winecool
        SPKCONUS(CurIYr)=0.0  !MELs21
        PHNCONUS(CurIYr)=0.0  !MELs21
        TABCONUS(CurIYr)=0.0  !MELs21
        KITCONUS(CurIYr)=0.0  !MELs21
        SECCONUS(CurIYr)=0.0
        EACONUS(CurIYr)=0.0
      DO D=1,mNumCR-2
        TVSCONUS(CurIYr)=TVSCONUS(CurIYr)+TVSCON(CurIYr,D)
        STBCONUS(CurIYr)=STBCONUS(CurIYr)+STBCON(CurIYr,D)
        HTSCONUS(CurIYr)=HTSCONUS(CurIYr)+HTSCON(CurIYr,D)
        OTTCONUS(CurIYr)=OTTCONUS(CurIYr)+OTTCON(CurIYr,D)  !MELs21
        VGCCONUS(CurIYr)=VGCCONUS(CurIYr)+VGCCON(CurIYr,D)
        DPCCONUS(CurIYr)=DPCCONUS(CurIYr)+DPCCON(CurIYr,D)
        LPCCONUS(CurIYr)=LPCCONUS(CurIYr)+LPCCON(CurIYr,D)
        MONCONUS(CurIYr)=MONCONUS(CurIYr)+MONCON(CurIYr,D)
        NETCONUS(CurIYr)=NETCONUS(CurIYr)+NETCON(CurIYr,D)
        BATCONUS(CurIYr)=BATCONUS(CurIYr)+BATCON(CurIYr,D)
        CFNCONUS(CurIYr)=CFNCONUS(CurIYr)+CFNCON(CurIYr,D)
        COFCONUS(CurIYr)=COFCONUS(CurIYr)+COFCON(CurIYr,D)
        DEHCONUS(CurIYr)=DEHCONUS(CurIYr)+DEHCON(CurIYr,D)
        MCOCONUS(CurIYr)=MCOCONUS(CurIYr)+MCOCON(CurIYr,D)
        PLPCONUS(CurIYr)=PLPCONUS(CurIYr)+PLPCON(CurIYr,D)  !MELs21
        PLHCONUS(CurIYr)=PLHCONUS(CurIYr)+PLHCON(CurIYr,D)  !MELs21
        SECCONUS(CurIYr)=SECCONUS(CurIYr)+SECCON(CurIYr,D)
        SPACONUS(CurIYr)=SPACONUS(CurIYr)+SPACON(CurIYr,D)
        WCLCONUS(CurIYr)=WCLCONUS(CurIYr)+WCLCON(CurIYr,D)    !winecool
        SPKCONUS(CurIYr)=SPKCONUS(CurIYr)+SPKCON(CurIYr,D)  !MELs21
        PHNCONUS(CurIYr)=PHNCONUS(CurIYr)+PHNCON(CurIYr,D)  !MELs21
        TABCONUS(CurIYr)=TABCONUS(CurIYr)+TABCON(CurIYr,D)  !MELs21
        KITCONUS(CurIYr)=KITCONUS(CurIYr)+KITCON(CurIYr,D)  !MELs21
        EACONUS(CurIYr)=EACONUS(CurIYr)+EACON(CurIYr,D)
      ENDDO

!*******************************************************************
!  CALCULATE TELEVISIONS AND RELATED EQUIPMENT TOTAL
!********************************************************************
      DO 530 D=1,mNumCR-2
        TVRCON(CurIYr,D)=0.0
        DO 530 B=1,mNumBldg
          TVRCON(CurIYr,D)=TVSCON(CurIYr,D)+STBCON(CurIYr,D)+HTSCON(CurIYr,D)+OTTCON(CurIYr,D)+VGCCON(CurIYr,D)  !MELs21
 530   CONTINUE

!*******************************************************************
!  CALCULATE PERSONAL COMPUTER AND RELATED EQUIPMENT TOTAL
!********************************************************************
      DO 531 D=1,mNumCR-2
        PCRCON(CurIYr,D)=0.0
        DO 531 B=1,mNumBldg
          PCRCON(CurIYr,D)=DPCCON(CurIYr,D)+LPCCON(CurIYr,D)+MONCON(CurIYr,D)+NETCON(CurIYr,D)
 531   CONTINUE

!********************************************************************
! ELECTRIC APPLIANCES
!********************************************************************
      DO 550 D=1,mNumCR-2
        APCON(CurIYr,D)=0.0
        DO 550 B=1,mNumBldg
          APCON(CurIYr,D)=BATCON(CurIYr,D)+CFNCON(CurIYr,D)+&
            COFCON(CurIYr,D)+DEHCON(CurIYr,D)+MCOCON(CurIYr,D)+&
            PLPCON(CurIYr,D)+SECCON(CurIYr,D)+SPACON(CurIYr,D)+&  !MELs21
            WCLCON(CurIYr,D)+PLHCON(CurIYr,D)+EACON(CurIYr,D)+&  !winecool  !MELs21
            SPKCON(CurIYr,D)+PHNCON(CurIYr,D)+TABCON(CurIYr,D)+KITCON(CurIYr,D)  !MELs21
 550   CONTINUE

!*******************************************************************
!  NON-ELECTRIC APPLIANCES CONSUMPTION  (Natural Gas, Propane, and Distillate Fuel Oil/Kerosene)
!********************************************************************
      DO 700 D=1,mNumCR-2
        DO 700 F=1,3
          APLCON(CurIYr,F,D)=0.0
          DO 700 B=1,mNumBldg
            APLCON(CurIYr,F,D)=APLCON(CurIYr,F,D)+(APPUEC(D,F,B)*APPEQP(RECSYear,B,D,F))
 700   CONTINUE

!*******************************************************************
!  CALCULATE SECONDARY HEATING CONSUMPTION
!********************************************************************
      DO 800 D=1,mNumCR-2
        DO 800 F=1,7
          SHTCON(CurIYr,F,D)=0.0
         DO 800 B=1,mNumBldg
          SHEQCN(CurIYr,F,B,D)=SHTEQP(RECSYear,B,D,F)*SHTUEC(D,F,B)
          SHTCON(CurIYr,F,D)=SHTCON(CurIYr,F,D)+(SHTEQP(RECSYear,B,D,F)*SHTUEC(D,F,B))
 800  CONTINUE

!********************************************************************
!     INITIALIZE RECS-YEAR NEMS DATA
!********************************************************************
         QBMRS(NATIONALPTR,CurIYr)=0.0
         QGERS(NATIONALPTR,CurIYr)=0.0
         QSTRS(NATIONALPTR,CurIYr)=0.0
         QPVRS(NATIONALPTR,CurIYr)=0.0

!********************************************************************
!     CALCULATE DIVISIONAL FUEL CONSUMPTION
!********************************************************************
      DO 750 D=1,mNumCR-2
! SOLAR ENERGY - ADDED SOLAR WATER HEATERS (SLCON)
         SLCON(CurIYr,10)=SLCON(CurIYr,10)+H2OCON(CurIYr,5,D)
         QSTRS(D,CurIYr)=H2OCON(CurIYr,5,D)/1000000.
         QSTRS(NATIONALPTR,CurIYr)=QSTRS(NATIONALPTR,CurIYr)+H2OCON(CurIYr,5,D)/1000000.
         QPVRS(D,CurIYr)=QPVRS(NATIONALPTR,CurIYr)*RENSHR(D)
! NATURAL GAS
         RSFLCN(CurIYr,1,D)= &
         (HTRCON(CurIYr,1,D)+H2OCON(CurIYr,1,D)+APLCON(CurIYr,1,D)+COOLCN(CurIYr,3,D)+ &
         CKCON(CurIYr,1,D)+DRYCON(CurIYr,1,D)+SHTCON(CurIYr,1,D))/1000000.
         QNGRS(D,CurIYr)=RSFLCN(CurIYr,1,D)
         QGFRS(D,CurIYr)=RSFLCN(CurIYr,1,D)*1.0
         QGIRS(D,CurIYr)=RSFLCN(CurIYr,1,D)*0.0
! ELECTRICITY
         RSFLCN(CurIYr,2,D)= &
         (HTRCON(CurIYr,2,D)+COOLCN(CurIYr,1,D)+H2OCON(CurIYr,2,D)+REFCON(CurIYr,D)+ &
          FRZCON(CurIYr,D)+LTCON(CurIYr,D)+APCON(CurIYr,D)+CKCON(CurIYr,3,D)+ &
          DRYCON(CurIYr,2,D)+SHTCON(CurIYr,2,D)+PCRCON(CurIYr,D)+TVRCON(CurIYr,D)+ &
          CSWCON(CurIYr,D)+DSWCON(CurIYr,D)+FANCON(CurIYr,D))/1000000.
         QELRS(D,CurIYr)=RSFLCN(CurIYr,2,D)  !DGreport - No change because EXCONS subroutine is only called in RECSyear (so TrillsOwnUse not calculated yet)
! DISTILLATE FUEL OIL
         RSFLCN(CurIYr,3,D)= &
         (HTRCON(CurIYr,3,D)+H2OCON(CurIYr,3,D)+APLCON(CurIYr,3,D)+SHTCON(CurIYr,3,D)) &
         /1000000.
         QDSRS(D,CurIYr)=RSFLCN(CurIYr,3,D)
! PROPANE
         RSFLCN(CurIYr,4,D)= &
         (HTRCON(CurIYr,4,D)+H2OCON(CurIYr,4,D)+APLCON(CurIYr,2,D)+ &
         CKCON(CurIYr,2,D)+SHTCON(CurIYr,4,D))/1000000.
         QLGRS(D,CurIYr)=RSFLCN(CurIYr,4,D)
         QPRRS(D,CurIYr)=QLGRS(D,CurIYr)
! KEROSENE	!kj - kerosene combined with distillate fuel oil but still in published Table 4
         QKSRS(D,CurIYr)=0.0	!kj - kerosene combined with distillate fuel oil but still in published Table 4
! BIOMASS (WOOD)
         RSFLCN(CurIYr,7,D)=(HTRCON(CurIYr,6,D)+SHTCON(CurIYr,7,D))/1000000.
         QBMRS(D,CurIYr)=RSFLCN(CurIYr,7,D)
! GEOTHERMAL
         RSFLCN(CurIYr,8,D)=(HTRCON(CurIYr,7,D)+COOLCN(CurIYr,2,D))/1000000.
         QGERS(D,CurIYr)=RSFLCN(CurIYr,8,D)
 750   CONTINUE

!********************************************************************
!     CALCULATE US (DIVISION 10) FUEL CONSUMPTION IN QUADRILLION BTU	!kj - D=10 is technically reserved for CA per PARAMETR include file; US/National should otherwise be 11
!********************************************************************
      DO 775 F=1,8
        RSFLCN(CurIYr,F,10)=0.0
        DO 775 D=1,mNumCR-2
          RSFLCN(CurIYr,F,10)=RSFLCN(CurIYr,F,10)+RSFLCN(CurIYr,F,D)
 775  CONTINUE

      QELRS(NATIONALPTR,CurIYr)=RSFLCN(CurIYr,2,10)  !DGreport - No change because EXCONS subroutine is only called in RECSyear (so TrillsOwnUse not calculated yet)
      QNGRS(NATIONALPTR,CurIYr)=RSFLCN(CurIYr,1,10)
      QGFRS(NATIONALPTR,CurIYr)=RSFLCN(CurIYr,1,10)*1.0  !FIRM GAS	!kj - still used?
      QGIRS(NATIONALPTR,CurIYr)=RSFLCN(CurIYr,1,10)*0.0  !INTERRUPTIBLE GAS	!kj - still used?
      QDSRS(NATIONALPTR,CurIYr)=RSFLCN(CurIYr,3,10)
      QLGRS(NATIONALPTR,CurIYr)=RSFLCN(CurIYr,4,10)
      QPRRS(NATIONALPTR,CurIYr)=QLGRS(NATIONALPTR,CurIYr)
      QKSRS(NATIONALPTR,CurIYr)=0.0  !KeroBench
      QBMRS(NATIONALPTR,CurIYr)=RSFLCN(CurIYr,7,10)
      QGERS(NATIONALPTR,CurIYr)=RSFLCN(CurIYr,8,10)
      END SUBROUTINE EXCONS


!******************************************************************
!     CALCULATE EPACT WINDOW LABELING IMPACT	!kj - Is this still relevant?
!*******************************************************************
      SUBROUTINE EPACTWD
      IMPLICIT NONE
      REAL*4 OLDSHR
      INTEGER Y,D,B

!*******************************************************************
!   COMPUTE EXISTING HOUSING SHARE
!*******************************************************************
      OLDHSES(CurCalYr)=0.0
      NEWHSES(CurCalYr)=0.0
      DO 5 B=1,mNumBldg
        DO 5 D=1,mNumCR-2
          OLDHSES(CurCalYr)=OLDHSES(CurCalYr)+EH(CurCalYr,B,D)
          NEWHSES(CurCalYr)=NEWHSES(CurCalYr)+NH(CurCalYr,B,D)
 5    CONTINUE
      OLDSHR=OLDHSES(CurCalYr)/(OLDHSES(CurCalYr)+NEWHSES(CurCalYr))

      END SUBROUTINE EPACTWD


!*******************************************************************
!     CALCULATE NEW HOUSING FOR RSYR = RECSYear+1:LastSTEOYrAvail
!*******************************************************************
      SUBROUTINE NEWHSE
      IMPLICIT NONE
      INTEGER  Y, B, D, E, F, IUNIT1,Y1

      Y=CurCalYr
      Y1=CurIYr

      DO 10 D=1,mNumCR-2
         IF (Y.EQ.RECSYear+1) NH(RECSYear,1,D)=0.0
         IF (Y.EQ.RECSYear+1) NH(RECSYear,2,D)=0.0
         IF (Y.EQ.RECSYear+1) NH(RECSYear,3,D)=0.0

          HSEADD(Y,1,D)=1000000.0*MC_HUSPS1(D,Y1)
          NH(Y,1,D) =   1000000.0*MC_HUSPS1(D,Y1) + (NH(Y-1,1,D)*HDR(1))
          HSEADD(Y,2,D)=1000000.0*MC_HUSPS2A(D,Y1)
          NH(Y,2,D) =   1000000.0*MC_HUSPS2A(D,Y1) + (NH(Y-1,2,D)*HDR(2))
          HSEADD(Y,3,D)=1000000.0*MC_HUSMFG(D,Y1)
          NH(Y,3,D) =   1000000.0*MC_HUSMFG(D,Y1) + (NH(Y-1,3,D)*HDR(3))

 10   CONTINUE

! DEVELOP SUBTOTALS FOR REPORTING
       DO D=1,mNumCR-2
         ALLNEW(Y,D)=0.0
         DO B=1,mNumBldg
           ALLNEW(Y,D)=NH(Y,B,D)
         ENDDO
       ENDDO

       DO D=1,mNumCR-2
         HHSTOCKBYDIV(CurCalYr,D)=0.0
         DO B=1,mNumBldg
           HHSTOCKBYDIV(CurCalYr,D)=(HHSTOCKBYDIV(CurCalYr,D)+NH(CurCalYr,B,D)+EH(CurCalYr,B,D))/1000000.
         ENDDO
       ENDDO

      END SUBROUTINE NEWHSE


!****************************************************************************
!     CALCULATE AVERAGE SQUARE FOOT OF HOUSING FOR RSYR = RECSYear+1:EndYr
!      Note, not called in the RECS year, so if this is the first year after
!       RECS, some initial RECS year calculations are also done.
!****************************************************************************
      SUBROUTINE SQFTCALC
      IMPLICIT NONE
      REAL*4 SQFTTOT(RECSYear:EndYr),RENOVATE
      INTEGER  Y, B, D, E, F, IUNIT1,T,Y1,Y2,V,S

!     STOCSQRFOOT is output to the database only, not used in other calculations

      Y=CurCalYr  !calendar year
      Y1=CurIYr   !NEMS index for calendar year
      RENOVATE=7.18 !THIS IS DERIVED FROM THE % OF HOMES ADDING A ROOM X THE SIZE ADDED (1.2% X 1/3 FLOOR AREA).	!kj


! Initialize RECS Year Values
!     Process RECS year calculations if this is the first call to this routine
      IF (Y.EQ.RECSYear+1) THEN
        SQFTTOT(RECSYear)=0.0
        DO D=1,mNumCR-2
          DO B=1,mNumBldg
           ! calculate total square footage in the RECS stock
           SQFTTOT(RECSYear)=SQFTTOT(RECSYear)+ SQRFOOT(RECSYear,B,D)*EH(RECSYear,B,D)
           EXSQRFOOT(RECSYear,B,D)=SQRFOOT(RECSYear,B,D)
          ENDDO !B
        ENDDO !D
        SQFTAVG(Y1-1)=SQFTTOT(RECSYear)/OLDHSES(RECSYear)
      ENDIF !End of RECS year calculations

       ! Calculate SQFTADJ for 5 SQFT sensitive end uses "S" loop
       !  s=1 fossil heating, s=2 electric heating, s=3 CAC, s=4 HP AC, s=5 furnace fans
       DO D=1,mNumCR-2
        DO B=1,mNumBldg
         !For projected renovation activity, allow the square footage of existing (RECS year) houses
         ! to increase over time
         EXSQRFOOT(Y,B,D)=SQRFOOT(RECSYear,B,D)+(RENOVATE*(Y-RECSYear))
! Eliminate this code because exsqftadj replaces sqftadj for all but new furnace fans...
!         DO S=1,5
!          ! Elastic(s,d) is the responsiveness of end uses to changes in squarefootage
!          !  See RMISC "New and Existing Elasticities for additions to floor space for heating and cooling"
!          SQFTADJ(Y,B,D,S)=(ELASTIC(S,D)*((SQRFOOT(Y,B,D)-SQRFOOT(RECSYear,B,D))/ &
!            SQRFOOT(RECSYear,B,D)))+1
!         ENDDO !S - SQFT sensitive end uses
        ENDDO ! B
       ENDDO ! D

!*******************************************************************
!     CALCULATE AVERAGE SQUARE FOOT IN EACH YEAR
!*******************************************************************
     DO B=1,mNumBldg
      DO D=1,mNumCR-2
        IF (Y.EQ.RECSYear+1) THEN
         SQNEW(RECSYear,B,D)=0.
        ELSE
         SQNEW(Y-1,B,D)=0.0
         DO T=RECSYear+1,Y-1
          SQNEW(Y-1,B,D)= SQNEW(Y-1,B,D) + &
           ((HSEADD(T,B,D)*HDR(B)**(Y-1-T))*SQRFOOT(T,B,D))/NH(Y-1,B,D)
         ENDDO
        ENDIF
      ENDDO
     ENDDO

!*******************************************************************
!     CALCULATE STOCK AVERAGE SQUARE FOOTAGE FOR DATABASE
!*******************************************************************
! STOCKSQRFOOT for database only
     DO B=1,mNumBldg
      DO D=1,mNumCR-2
        IF (Y.EQ.RECSYear+1) THEN
         STOCKSQRFOOT(CurCalYr-1,B,D)=EXSQRFOOT(CurCalYr-1,B,D)
         STOCKSQRFOOT(CurCalYr,B,D)=(EH(Y,B,D)*EXSQRFOOT(CurCalYr,B,D)+HSEADD(Y,B,D)*SQRFOOT(Y,B,D)) &
                                    /(EH(Y,B,D)+HSEADD(Y,B,D))
        ELSE
         STOCKSQRFOOT(CurCalYr,B,D)=(EH(Y,B,D)*EXSQRFOOT(CurCalYr,B,D)+HSEADD(Y,B,D)*SQRFOOT(Y,B,D)+ &
                                    SQNEW(Y-1,B,D)*NH(Y-1,B,D))/(EH(Y,B,D)+NH(Y,B,D))
        ENDIF
      ENDDO
     ENDDO

     SQFTTOT(Y)=0.0
     DO D=1,mNumCR-2
       DO B=1,mNumBldg
        SQFTTOT(Y)=SQFTTOT(Y)+ ( SQRFOOT(Y,B,D)*HSEADD(Y,B,D)  + &
          SQNEW(Y-1,B,D)*NH(Y-1,B,D)+ EXSQRFOOT(CurCalYr,B,D)*EH(Y,B,D) )
       ENDDO
     ENDDO
     SQFTAVG(Y1)= SQFTTOT(Y) / ( OLDHSES(Y)+  NEWHSES(Y) )

     END SUBROUTINE SQFTCALC


!******************************************
!     CALCULATE REPLACEMENT EQUIPMENT TYPE
!******************************************
      SUBROUTINE REPLACE(EU,R,B,RECCL,FLAG)
      IMPLICIT NONE
      REAL*4 EQCOST,CAPITAL,RETAIL,RPSHARE(mNumRTCl)
      REAL*4 TOTSH, RETIRED,RETIREDR,EQC
      INTEGER EU,EQCSW,RECCL,RECCLSW,EQTSW,RECTYSW,B,R
      INTEGER I,Y,FLAG

      TOTSH = 0.0
      IF(FLAG.EQ.1) THEN
        !  OEQCREP is old value of EQCREP with no technology switching
        RETIRED = OEQCREP(CurCalYr,RECCL,1,R)
       ELSE
        !  OEQCRP90 is old value of EQCRP90 with only switching to NG_FA
        RETIRED = OEQCRP90(CurCalYr,RECCL,1,R)
        RETIREDR= OEQCRP90R(CurCalYr,RECCL,1,R)
      ENDIF

      DO RECCLSW=RTCLEUPT(EU)+1,RTCLEUPT(EU+1) !process all classes this enduse
        EQCSW = RTCLEQCL(RECCLSW)
        EQTSW = RTCLTYPT(RECCLSW)
        RECTYSW = 0

        DO I = RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
          IF(RTEQTYPE(I).EQ.EQTSW.AND. &
            (CurCalYr.GE.RTINITYR(I).AND.CurCalYr.LE.RTLASTYR(I))) THEN
            RECTYSW = I
            GO TO 10 ! Once RECTYSW found, get out of loop
          ENDIF
        ENDDO !I
        IF(RECTYSW.EQ.0) THEN
          !Report error to unit 6 (nohup.out)
          WRITE(6,*) 'RESDMSG SUB_REPLACE: No representative equipment type ', &
           'in RSCLASS for end use = ',EU,' eq class = ',EQCSW &
          ,' eq type = ',EQTSW,' CurCalYr = ',CurCalYr
          RETURN
        ENDIF

 10   CONTINUE  ! RECTYSW found, CONTINUE

        !  If COSTTRSW = 1, use function EQCOST to compute capital and retail
        !     cost of new equipment.
        !  If COSTTRSW = 0, use constant value from RSMEQP file for capital
        !     and retail cost of new equipment.
        IF (COSTTRSW.EQ.1) THEN
          CAPITAL = EQCOST(RECTYSW,CurCalYr,"CAP")
          RETAIL = EQCOST(RECTYSW,CurCalYr,"RET")
        ELSE
          CAPITAL = RTEQCOST(RECTYSW)
          RETAIL = RTRECOST(RECTYSW)
        ENDIF

        ! Compute shares for this equipment class
        RPSHARE(EQCSW) = EXP(RTSWBIAS(RECCLSW)+RTSWBETA(RECCLSW) * &
          (LFCY(EQTSW,B,R,1)+RPINSCOST(RECCL,RECCLSW)))

        ! TOTSH = TOTAL SHARES FOR ALL EQCSW's FOR THIS EQUIPMENT CLASS
        TOTSH = TOTSH+ RPSHARE(EQCSW)
      ENDDO !RECCLSW

      ! NORMALIZE SHARES FOR THOSE WHO SWITCH TECHNOLOGIES
      DO RECCLSW=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQCSW = RTCLEQCL(RECCLSW)
        IF(TOTSH.GT.0.0)THEN
          RPSHARE(EQCSW)=RPSHARE(EQCSW)/TOTSH
        ELSE
          RPSHARE(EQCSW) = 0.0
        ENDIF

        ! SHARE OUT REPLACEMENTS FOR THOSE WHO SWITCH TECHNOLOGIES
        !  RTSWFACT(RECCL)=SWITCHING FACTOR
        IF(FLAG.EQ.1) THEN
        !  Flag = 1 calculate replacements for post-RECSyear homes
            EQCREP(CurCalYr,RECCLSW,B,R) = (EQCREP(CurCalYr,RECCLSW,B,R) &
               + (RETIRED * RPSHARE(EQCSW) * RTSWFACT(RECCL)))
          ELSE
          ! ELSE calculate replacements pre-RECSyear+1 homes
            EQCSW90(CurCalYr,RECCL,RECCLSW,B,R) =  &
             (RETIRED * RPSHARE(EQCSW) * RTSWFACT(RECCL))
            EQCSW90R(CurCalYr,RECCL,RECCLSW,B,R) =  &
             (RETIREDR * RPSHARE(EQCSW) * RTSWFACT(RECCL))
        ENDIF
      ENDDO

      !   SUM OVER ALL TYPES FOR TOTAL SWITCHES FROM EACH TECHNOLOGY
      IF (FLAG.NE.1) THEN
          SWITCHES(CurCalYr,RECCL,B,R)=0.0
          SWITCHESR(CurCalYr,RECCL,B,R)=0.0
        DO RECCLSW=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
         IF (RECCLSW.NE.RECCL) THEN
          SWITCHES(CurCalYr,RECCL,B,R)=SWITCHES(CurCalYr,RECCL,B,R)+ &
             EQCSW90(CurCalYr,RECCL,RECCLSW,B,R)
          SWITCHESR(CurCalYr,RECCL,B,R)=SWITCHESR(CurCalYr,RECCL,B,R)+ &
             EQCSW90R(CurCalYr,RECCL,RECCLSW,B,R)
         ENDIF
        ENDDO
      ENDIF

      ! REPLACEMENTS FOR THOSE WHO DON'T SWITCH TECHNOLOGIES
      IF(FLAG.EQ.1) THEN
      ! Flag = 1 calculate replacements for post-RECS-year homes
      EQCREP(CurCalYr,RECCL,B,R) = (EQCREP(CurCalYr,RECCL,B,R)+ &
         (RETIRED*(1-RTSWFACT(RECCL))) )
       ENDIF

!      RETURN
      END SUBROUTINE REPLACE


!*******************************************************************
!     HVAC CHOICE SUBROUTINE
!*******************************************************************
      SUBROUTINE RSHVAC
      IMPLICIT NONE
!bookmark      COMMON/SHINV/SHELLInvest(RECSYear:EndYr,nHeatClasses,nShellTypes,mNumBldg,mNumCR), &
!bookmark                   SHELLSUBSIDY(RECSYear:EndYr,nHeatClasses,nShellTypes,mNumBldg,mNumCR)
      COMMON/ESTARHOMES/HVEQWTN(RECSYear:EndYr,nHeatTypes,nShellTypes,mNumBldg,mNumCR)
      COMMON/SQFTSHELL/HTSQRFOOTFAC(RECSYear:EndYr,nHeatTypes,mNumCR-2,mNumBldg),CLSQRFOOTFAC(RECSYear:EndYr,nHeatTypes,mNumCR-2,mNumBldg)
      ! LOCAL VARIABLES
      REAL*4 HTSQRFOOTFAC,CLSQRFOOTFAC
!bookmark      REAL*4 SHELLINVEST,SHELLSUBSIDY
      REAL*4 ACRECOST,rlearncost
      REAL*4 EQCOST,CAPITAL,RETAIL,CAPITALX
      REAL*4 HDDFACT(mNumCR),CDDFACT(mNumCR),HVOPCOST,HVLFCY
      REAL*4 RTEFFAC(3),DECAY,ECTEMP,DENOM,SUM,SUM1,E
      REAL*4 HTSHELLFAC(RECSYear:EndYr,MNUMCENDIV,mNumBldg),&
             CLSHELLFAC(RECSYear:EndYr,MNUMCENDIV,mNumBldg),SQFTWEIGHTC(RECSYear:EndYr,nCoolTypes,mNumBldg,mNumCR),&
             EFFWEIGHTC(RECSYear:EndYr,nCoolTypes,mNumBldg,mNumCR),EQWTNCA(RECSYear:EndYr,nCoolTypes,mNumBldg,mNumCR)
      REAL*4 COOLSHWT(nCoolTypes,mNumBldg,mNumCR)
      REAL*4 TOTEWTN(nHeatClasses,mNumBldg,mNumCR),HVEQWTN,HVBETA2A(MNUMHVAC)
      REAL*4 WTDEFF(nHeatClasses),EFFWEIGHT(RECSYear:EndYr,nHeatTypes,mNumBldg,mNumCR),SQFTWEIGHT(RECSYear:EndYr,nHeatTypes,mNumBldg,mNumCR)	!kj - WTDEFF not really used (set to 0 later but that's it)
      REAL*4 TOTEWTNC(RECSYear:EndYr,nCoolClasses,mNumBldg,mNumCR)  !EqpParam	!kj - why is TOTEWTNC annualized but TOTEWTN isn't?
      REAL*4 EQFSHRNC(nCoolTypes),SHLLEARN(RECSYear:EndYr,mNumBldg,mNumCR-2)
      REAL*4 EQFSHRN(nHeatTypes),EFFWT(RECSYear:EndYr,nHeatTypes,mNumBldg,mNumCR),EQPEFF(nHeatTypes)  !EqpParam	!kj - EFFWT not used?
      REAL*4 WeightTot(RECSYear:EndYr,nHeatClasses,mNumBldg,mNumCR),TOTEFFWT(RECSYear:EndYr,nHeatClasses,mNumBldg,mNumCR)	!kj - WeightTot and TOTEFFWT not used?
      REAL*4 EPRICE(mNumCR-2,RECSYear:EndYr),ESHR(RECSYear:EndYr)
      !These variables are involved in the efficiency choice calculation.	!kj - bookmark to clarify comment
      ! RECAR and EQTAR are dimensioned for the number of choices
      ! across efficiency types in any single year.
      INTEGER EU,EUPR,RECTY,RECCL,R,B,F,T,EQT,EQC,TYPE,COUNT,L,HCNT,Y,Y1,HC,EV,RECCL1
      INTEGER RECAR(nHeatTypes),EQTAR(nHeatTypes),S,HVRCTY,HVC,HVT,FS,HVCC,HVCT,HVTYCNT,HE,HS,CS  !EqpParam

!*******************************************************************
!   THE GENERAL FORM OF THIS SUBROUTINE APPLIES TO ALL END USES
!     SET EU = 1 TO SEARCH THE SPACE HEATING SECTION OF THE DATA
!*******************************************************************
     EU     = 1
     EUPR=1
     ALPHA1 = -0.50

     DO R=1,mNumCR-2
      DO B=1,mNumBldg
       IF (CurCalYr.GT.RECSYear+1) THEN  !RSESTARbetas	!kj - (RECSYear+1) should be changed to ESTARHISTYR once betas in RSESTAR.txt are readjusted for historical ENERGY STAR housing start shares
        SHLLEARN(CurCalYr,B,R)=SHLLEARN(CurCalYr-1,B,R)*(LEARNFACT(B,R)**(CurCalYr-(RECSYear+1)))  !RSESTARbetas	!kj - (RECSYear+1) should be changed to ESTARHISTYR once betas in RSESTAR.txt are readjusted for historical ENERGY STAR housing start shares
       ELSE
        SHLLEARN(CurCalYr,B,R)=1.0
       ENDIF
      ENDDO
     ENDDO

     !   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
     DO R=1,mNumCR-2
        !Heating price (EU=1,EUPR=1)
        PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
        !AC Price (EU=2,EUPR=2)
        EPRICE(R,CurCalYr)=PELRSOUT(R,CurIYr,2)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
     ENDDO

      ! COMPUTE WEATHER ADJUSTMENT FACTORS
      DO R=1,mNumCR-2
          HDDFACT(R)=(HDDADJ(CurCalYr,R)/HDDADJ(RECSYear,R))**2.00 !A 10% increase in HDD would increase space heating consumption by 21% (e.g., 1.10^2.00=1.21)
          CDDFACT(R)=(CDDADJ(CurCalYr,R)/CDDADJ(RECSYear,R))**1.50 !A 10% increase in CDD would increase space cooling consumption by 15% (e.g., 1.10^1.50=1.15)
      ENDDO

!     COMPUTE DECAY RATE USED TO COMPUTE LIFE CYCLE COST (FIRST ITERATION ONLY)
      IF(CURITR.EQ.1) THEN
        DECAY = (1-((1+ResDiscountRate)**(-Tenure)))/ResDiscountRate
      ENDIF

      DO 90 R=1,mNumCR-2   !Major loops to end of share calculations
        DO 90 B=1,mNumBldg
         EU=1

          ! INITIALIZE ARRAYS
          DO HVC=1,nHeatClasses
           TOTEWTN(HVC,B,R)=0.0
           WTDEFF(HVC)=0.0
           HSHELL(CurCalYr,HVC,B,R)=0.0
          ENDDO

          DO HVCC=1,nCoolClasses  !EqpParam
           TOTEWTNC(CurCalYr,HVCC,B,R)=0.0
           CSHELL(CurCalYr,HVCC,B,R)=0.0
          ENDDO

          DO HVCT=1,nCoolTypes
           EQWTNCA(CurCalYr,HVCT,B,R)=0.0
           COOLSHWT(HVCT,B,R)=0.0
           EQFSHRNC(HVCT)=0.0
          ENDDO

          ! RSMEQP and RSCLASS Variables
          ! RTTYEUPT(EU)   = 0 FOR EU=1 (BEFORE BEGINNING OF FILE)
          ! RTTYEUPT(EU+1) = LAST RECORD # IN SPACE HEATING (EU=1)
          ! RECTY          = RECORD # FROM RSMEQP FILE
          ! EQT            = EQUIPMENT TYPE NUMBER
          ! EQC            = EQUIPMENT CLASS NUMBER
          ! RECCL          = RECORD # FROM RSCLASS FILE
          ! F              = FUEL #
          DO HVRCTY=1,MNUMHVAC   ! for all records in the shell file
          IF (RSCENDIV(HVRCTY).EQ.R .AND. RSBTYPE(HVRCTY).EQ.B) THEN
              ! Heating and cooling types and classes assigned by the building envelope/ shell input file RSMSHL
              HVT=HVHTEQTY(HVRCTY)   ! Number of heating types calculated in RSMESS-- defines efficiency of equipment
              HVC=HVHTEQCL(HVRCTY)   ! Number of heating classes calculated in RSMESS
              HVCT=HVCLEQTY(HVRCTY)  ! Number of cooling types calculated in RSMESS -- defines efficiency of equipment
              HVCC=HVCLEQCL(HVRCTY)  ! Number of cooling classes calculated in RSMESS
              S= HVPACKG(HVRCTY)     ! S = 1 to 5, 1=NoCode, 2=IECC, 3=ENERGY STAR, 4=IECC+40%, 5=PATH; ENERGY STAR-qualified = 3 + 4 + 5
              HS=HTSHEFF(HVRCTY)     ! Heating shell efficiency
              CS=CLSHEFF(HVRCTY)     ! Cooling shell efficiency
          ! Filter Shell File for Calendar Year Availability
          IF (CurCalYr.GE.HVFYEAR(HVRCTY).AND.CurCalYr.LE.HVLYEAR(HVRCTY)) THEN
           DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            ! Filter RSMEQP for equipment availability
            IF(CurCalYr.GE.RTINITYR(RECTY).AND. &
                CurCalYr.LE.RTLASTYR(RECTY).AND.RTCENDIV(RECTY).EQ.R) THEN
              EQT=RTEQTYPE(RECTY) !nHeatTypes = # of specific heating types
              EQC=RTTYEQCL(RECTY) !nHeatClasses = # of heating classes
              RECCL=RTCLEUPT(EU)+EQC  !points to record number of heating equipment class in RSCLASS
              F    =RTFUEL(RECCL) !fuel for equipment class
              FS   =RTFUEL(HVC)   !fuel for shell class

              ! RECCL1 maps the heating equipment class to the appropriate cooling class
              !  e.g., RECCL=2,(HP) maps to RECCL1=14(HP in cooling section
              !  adding number of heating classes(nHeatClasses) to the cooling class for HPs(3), etc.
              !  RECCL1 points to the appropriate cooling class record number in RSCLASS
               IF (RECCL.EQ.2) THEN
                 RECCL1=14   !nHeatClasses + RTCLEQCL(ELEC_HP) = 11 + 3 = 14
                 ELSEIF (RECCL.EQ.10) THEN
                   RECCL1=15   !nHeatClasses + RTCLEQCL(GEO_HP) = 11 + 4 = 15
                 ELSEIF (RECCL.EQ.11) THEN
                   RECCL1=16   !nHeatClasses + RTCLEQCL(NG_HP) = 11 + 5 = 16
                 ELSE
                   RECCL1=13   !central AC (RECCL1=13) for all other; room air conditioner (RECCL1=12) not considered here
               ENDIF  !RECCL

             IF (EQT.EQ.HVT) THEN
              !  COMPUTE EFFICIENCY FACTORS USED IN COMPUTING OPERATING COST
              !   RTEFFAC(2) is used for the heating component of the shell package
              !   RTEFFAC(3) is used for the cooling component of the shell package
              SHLEVELH(CurCalYr,HVC,S,B,R)=0.0
              EQPEFF(HVT)=RTEQEFF(RECTY)
              IF(RTEQEFF(RECTY) .NE. 0.0) THEN
                RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)/RTEQEFF(RECTY)
               ELSE
                RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)
              ENDIF
              IF (ACEFF(HVCT,CurCalYr,R) .NE. 0.0) THEN
                RTEFFAC(3)=RTBASEFF(RECSYear,HVCC)/ACEFF(HVCT,CurCalYr,R)
               ELSE
                RTEFFAC(3)=RTBASEFF(RECSYear,HVCC)
              ENDIF

              ! COMPUTE EFFICIENCY SHELL EFFICIENCY FACTOR USED IN COMPUTING OPERATING COST
              HTSHELLFAC(CurCalYr,R,B)=HTSHEFF(HVRCTY)/HTSHBASE(HVRCTY)
              CLSHELLFAC(CurCalYr,R,B)=CLSHEFF(HVRCTY)/CLSHBASE(HVRCTY)

              ! COMPUTE SQUARE FOOTAGE EFFECT FOR NEW CONSTRUCTION (CHANGES FROM INITIAL NEW CONTRUCTION VALUE)	!kj
              ! These factors are estimated from building simulations for 10% increases in floor area.
              !  The inputs are total effects and thus must be divided by 1.10 to produce a percentage
              !  change in HVAC use per percentage change in floor area.
              HTSQRFOOTFAC(CurCalYr,HVT,R,B)=1.+(((SQRFOOT(CurCalYr,B,R)/SQRFOOT(RECSYear+1,B,R))-1.) &
                *(HVHEATFACTOR(HVRCTY)/1.10))
              CLSQRFOOTFAC(CurCalYr,HVCT,R,B)=1.+(((SQRFOOT(CurCalYr,B,R)/SQRFOOT(RECSYear+1,B,R))-1.)&
                *(HVCOOLFACTOR(HVRCTY)/1.10))

             !COST TREND CALCULATIONS
             ! If COSTTRSW = 1, use function EQCOST to compute capital cost of new equipment.
             ! If COSTTRSW = 0, use constant value from RSMEQP file for capital cost of new equipment.
             IF (COSTTRSW.EQ.1) THEN
               IF (EQT.EQ.HVT) THEN
                CAPITAL=(EQCOST(RECTY,CurCalYr,"CAP")+SHELCOST(HVRCTY)+ACICOST(HVCT,CurCalYr,R))
                SHELLSUBSIDY(CurCalYr,RECCL,S,B,R)=SHELSUB(HVRCTY)+SHELSUB111D(HVRCTY)*FLOAT(EPA111D)
                SHELLSUBSIDY111D(CurCalYr,RECCL,S,B,R)=SHELSUB111D(HVRCTY)*FLOAT(EPA111D)
               ENDIF
              ELSE
               IF (EQT.EQ.HVT) THEN
                CAPITAL=(RTEQCOST(RECTY)+SHELCOST(HVRCTY)+ACICOST(HVCT,CurCalYr,R))
                SHELLSUBSIDY(CurCalYr,RECCL,S,B,R)=SHELSUB(HVRCTY)+SHELSUB111D(HVRCTY)*FLOAT(EPA111D)
                SHELLSUBSIDY111D(CurCalYr,RECCL,S,B,R)=SHELSUB111D(HVRCTY)*FLOAT(EPA111D)
               ENDIF
             ENDIF !Cost Trend Calculations

              ! COMPUTE THE PART OF THE EQUIMENT CHOICE CALC NOT DEPENDENT ON REGION OR BUILDING TYPE
              SHELLInvest(CurCalYr,RECCL,S,B,R)=CAPITAL-RTEQCOST(RECTY)-ACICOST(HVCT,CurCalYr,R)

              ! CALCULATE OPERATING COST
              HVOPCOST=PRICES(F,R,CurCalYr)*NEWHEATUEC(RECCL,B,R)/BASELOAD(RECCL)* &
                RTEFFAC(2)*HDDFACT(R)*HTSQRFOOTFAC(CurCalYr,HVT,R,B)*HTSHELLFAC(CurCalYr,R,B)+  &
                EPRICE(R,CurCalYr)*NEWCOOLUEC(B,R)/BASELOAD(RECCL1)*              &
                RTEFFAC(3)*CDDFACT(R)*CLSQRFOOTFAC(CurCalYr,HVT,R,B)*CLSHELLFAC(CurCalYr,R,B)

              ! CALCULATE LIFE CYCLE COSTS
              HVLFCY=CAPITAL+HVOPCOST*DECAY

              ! Shell learning for ENERGY STAR-qualified shells (s>2)
              IF (S.GT.2) THEN
                ! ENERGY STAR-qualified (EStar, Forty, and PATH)
                ! Allow further shell improvements
                HVBETA2A(HVRCTY)=HVBETA2(MIN(CurCalYr,ESTARHISTYR),B,S,R)*SHLLEARN(CurCalYr,B,R)  !RSESTARbetas
               ELSE
                ! Code homes, non-ENERGY STAR
                ! Code homes DO not get further learned shell improvements
                HVBETA2A(HVRCTY)=HVBETA2(MIN(CurCalYr,ESTARHISTYR),B,S,R)  !RSESTARbetas
              ENDIF  !S>2

              ! Benchmark ENERGY STAR shares for each of the nHeatTypes (HVT)
              IF (B.NE.1) THEN
                 ! For multifamily and mobile homes, compute shares here
                 HVEQWTN(CurCalYr,HVT,S,B,R)=EXP(HVBETA2A(HVRCTY)+(HVBETA1(MIN(CurCalYr,ESTARHISTYR),B,S,R)*HVLFCY))  !RSESTARbetas
               ELSEIF ((B.EQ.1).AND.(CurCalYr.GT.RECSYear)) THEN            ! HVAC Historical  !RSESTARbetas	!kj - RECSYear should be ESTARHISTYR?
                 ! If beyond the historical ENERGY STAR housing share benchmarking period also compute shares here
                 HVEQWTN(CurCalYr,HVT,S,B,R)=EXP(HVBETA2A(HVRCTY)+(HVBETA1(MIN(CurCalYr,ESTARHISTYR),B,S,R)*HVLFCY))  !RSESTARbetas
              ENDIF !B<>1

              TOTEWTN(HVC,B,R)=TOTEWTN(HVC,B,R)+HVEQWTN(CurCalYr,HVT,S,B,R)
              EQWTNCA(CurCalYr,HVCT,B,R)=EQWTNCA(CurCalYr,HVCT,B,R)+HVEQWTN(CurCalYr,HVT,S,B,R)
              COOLSHWT(HVCT,B,R)=COOLSHWT(HVCT,B,R)+HVEQWTN(CurCalYr,HVT,S,B,R)*CLSHEFF(HVRCTY)
              TOTEWTNC(CurCalYr,HVCC,B,R)=TOTEWTNC(CurCalYr,HVCC,B,R)+HVEQWTN(CurCalYr,HVT,S,B,R)

             ENDIF  !Calculations for EQT = HVT

            ENDIF  !Filter all RSMEQP records (RECTY) for Current Year and Division

           ENDDO   !DO all RSMEQP records

           ENDIF  !Filter shells for year
           ENDIF  !Filter shells for census division and building type

          ENDDO  !For all shell file records

          ! Now that the first pass through the data has been made, raw accumulations
          !  of logit exponents are available for share calculations
          DO HE=1,nHeatTypes      !For all specific heating equipment types
            EQFSHRN(HE)=0.0
            HVEQSHR(CurCalYr,HE,B,R)=0.0

           DO HVRCTY=1,MNUMHVAC  !All shell file records (max 5000)

            IF (RSCENDIV(HVRCTY).EQ.R .AND. RSBTYPE(HVRCTY).EQ.B) THEN
                HVC=HVHTEQCL(HVRCTY)  !nHeatClasses = # of heating classes
                HVCC=HVCLEQCL(HVRCTY) !nCoolClasses = # of cooling classes
                HVT=HVHTEQTY(HVRCTY)  !nHeatTypes = # of specific heating types
                HVCT=HVCLEQTY(HVRCTY) !nCoolTypes = # of specific cooling types
                S= HVPACKG(HVRCTY)    !nShellTypes = # of building envelope/ shell options
               ! Filter for year availability
               IF (CurCalYr.GE.HVFYEAR(HVRCTY).AND. &
                 CurCalYr.LE.HVLYEAR(HVRCTY) ) THEN

               !Filter for equipment match
               IF (HE.EQ.HVT) THEN  !Ignore records for other specific equipment types
               ! SET EQUIPMENT FUEL SHARE (AND NEQTSHR FOR WATER HEATING)
               IF (TOTEWTN(HVC,B,R).GT.0.0) THEN
                 HTSHELLWT(CurCalYr,HVT,S,B,R)=HVEQWTN(CurCalYr,HVT,S,B,R)/TOTEWTN(HVC,B,R)
                 !Weighted heating shell efficiency for this specific equipment type
                 HTSHELLEFFWT(CurCalYr,HVT,S,B,R)= HTSHELLWT(CurCalYr,HVT,S,B,R)*HTSHEFF(HVRCTY)
                ELSE
                 HTSHELLEFFWT(CurCalYr,HVT,S,B,R)=0.0
                 HTSHELLWT(CurCalYr,HVT,S,B,R)=0.0
               ENDIF

               ! Calculate shell efficiency by heating class and accumulate shares
               HSHELL(CurCalYr,HVC,B,R)=HSHELL(CurCalYr,HVC,B,R)+HTSHELLEFFWT(CurCalYr,HVT,S,B,R)
               SHLEVELH(CurCalYr,HVC,S,B,R)=SHLEVELH(CurCalYr,HVC,S,B,R)+HVEQWTN(CurCalYr,HVT,S,B,R)/TOTEWTN(HVC,B,R)

               !For equipment classes with non-zero shares accumulate shares for specific equipment used by shell
                IF (TOTEWTN(HVC,B,R).GT.0.0) THEN
                  EQFSHRN(HE)=EQFSHRN(HE)+HVEQWTN(CurCalYr,HVT,S,B,R)/TOTEWTN(HVC,B,R)
                     ELSE
                  EQFSHRN(HE)=0.0
                ENDIF

                HVEQSHR(CurCalYr,HE,B,R)=EQFSHRN(HE) !Map specific equipment share into the HV array
               ENDIF !Filter for specific equipment match (HVT = HE)


              ENDIF !Filter for current year validity
             ENDIF  !Filter for Census Division and Building Type
          ENDDO   !All shell file records
         ENDDO    !HE, All nHeatTypes for specific heating equipment types


         ! CALCULATE WEIGHTED EFFICIENCY AND WEIGHTED SQUARE FOOTAGE FACTOR FOR EACH HEATING EQUIPMENT CLASS FOR USE BELOW
         DO HVT=1,nHeatTypes
           IF(EQPEFF(HVT).GT.0.) EFFWEIGHT(CurCalYr,HVT,B,R)=HVEQSHR(CurCalYr,HVT,B,R)/EQPEFF(HVT)
!           EFFWEIGHT(CurCalYr,HVT,B,R)=HVEQSHR(CurCalYr,HVT,B,R)*EQPEFF(HVT)
           SQFTWEIGHT(CurCalYr,HVT,B,R)=HVEQSHR(CurCalYr,HVT,B,R)*HTSQRFOOTFAC(CurCalYr,HVT,R,B)
!           WRITE(9,'("efficiency calc",4i5,2f12.4)') CurCalYr,hvt, b, r, hveqshr(CurCalYr,HVT,B,R), EQPEFF(HVT)
         ENDDO

 90   CONTINUE  ! End census division and building type loop for share calculations

      ! CALCULATE WEIGHTED EFFICIENCY FOR NEW AND REPLACEMENT HEATING EQUIP
      DO 91 R=1,mNumCR-2
       DO 91 B=1,mNumBldg
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC   =RTCLEQCL(RECCL)
            DENOM =0
            COUNT =0
            ! TYPE = INDEX FOR ARRAYS NEQTSHR AND REQTSHR
            !  INITIALIZE TO LAST ARRAY POSTION IN PREVIOUS END USE
            !  AND THEN COUNT VALID TYPES IN CURRENT END USE
            !Loop through all equipment records for this end use
                DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
              IF(CurCalYr.GE.RTINITYR(RECTY) &
                .AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
                IF(RTTYEQCL(RECTY).EQ.EQC) THEN
                  !Count valid efficiency levels for this type
                  COUNT=COUNT+1
                  EQT=RTEQTYPE(RECTY)  !nHeatTypes = # of specific heating types
                  RECAR(COUNT)=RECTY   !RSMEQP record number
                  EQTAR(COUNT)=EQT     !specific type
                  DENOM=DENOM+HVEQSHR(CurCalYr,EQT,B,R)
                ENDIF
              ENDIF
             ENDIF
            ENDDO

            ! COMPLETE CALCULATION FOR NEW EQUIPMENT
            IF(DENOM.LE.0.0) THEN
              WTEQCEFFHV(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
              WTEQCSQFHV(CurCalYr,RECCL,B,R)=1.0
            ELSE
              SUM=0.0
              SUM1=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                SUM=SUM+EFFWEIGHT(CurCalYr,TYPE,B,R)
                SUM1=SUM1+SQFTWEIGHT(CurCalYr,TYPE,B,R)
              ENDDO
              !SYSTEM-WEIGHTED SQUARE FOOTAGE UEC ADJUSTMENT FACTORS
              !  (DERIVED FROM RSMSHL HEATING ADJUSTMENT FACTORS)
              WTEQCSQFHV(CurCalYr,RECCL,B,R)=SUM1/DENOM
              !SYSTEM-WEIGHTED CLASS EFFICIENCY
! 5-21 Don't invert here it is used as an inverse  WTEQCEFFHV(CurCalYr,RECCL,B,R)=(1/SUM)/DENOM
              WTEQCEFFHV(CurCalYr,RECCL,B,R)=SUM/DENOM
            ENDIF

!              WRITE(9,'("final efficiency calc",4i5,f12.4)') CurCalYr,RECCL, b, r, WTEQCEFFHV(CurCalYr,RECCL,B,R)

          ENDDO
 91     CONTINUE

      DO 190 B=1,mNumBldg
        DO 190 R=1,mNumCR-2
          DO HC=5,nCoolTypes !skips over room air conditioners; HC numbers may need to change if xlRTEQTYPE changes in RSMEQP tab of RSMESS.xlsx  !CoolTypes
            EQFSHRNC(HC)=0.0
            NEQTSHRC(CurCalYr,HC,B,R)=0.0
            IF (HC.LT.9)               HVCC=2 !CENT_AIR  !CoolTypes
            IF (HC.GT.8.AND.HC.LT.13)  HVCC=3 !ELEC_HP  !CoolTypes
            IF (HC.GT.12.AND.HC.LT.17) HVCC=4 !GEO_HP  !CoolTypes
            IF (HC.EQ.17)              HVCC=5 !NG_HP  !CoolTypes
            IF (TOTEWTNC(CurCalYr,HVCC,B,R).GT.0.0) THEN
              CLSHELLWT(CurCalYr,HC,B,R)=COOLSHWT(HC,B,R)/TOTEWTNC(CurCalYr,HVCC,B,R)
            ELSE
              CLSHELLWT(CurCalYr,HC,B,R)=0.0
            ENDIF
            CSHELL(CurCalYr,HVCC,B,R)=CSHELL(CurCalYr,HVCC,B,R)+CLSHELLWT(CurCalYr,HC,B,R)
            IF (TOTEWTNC(CurCalYr,HVCC,B,R).GT.0.0) THEN
              EQFSHRNC(HC)=EQWTNCA(CurCalYr,HC,B,R)/TOTEWTNC(CurCalYr,HVCC,B,R)
            ELSE
              EQFSHRNC(HC)=0.0
            ENDIF
            NEQTSHRC(CurCalYr,HC,B,R)=EQFSHRNC(HC)
         ENDDO !HC

         DO HVCT=5,nCoolTypes !skips over room air conditioners; HC numbers may need to change if xlRTEQTYPE changes in RSMEQP tab of RSMESS.xlsx  !CoolTypes
           IF(ACEFF(HVCT,CurCalYr,R).GT.0.) EFFWEIGHTC(CurCalYr,HVCT,B,R)=NEQTSHRC(CurCalYr,HVCT,B,R)/ACEFF(HVCT,CurCalYr,R)
!             EFFWEIGHTC(CurCalYr,HVCT,B,R)=NEQTSHRC(CurCalYr,HVCT,B,R)*ACEFF(HVCT,CurCalYr,R)
             SQFTWEIGHTC(CurCalYr,HVCT,B,R)=NEQTSHRC(CurCalYr,HVCT,B,R)*CLSQRFOOTFAC(CurCalYr,HVCT,R,B)
         ENDDO

        ! CALCULATE WEIGHTED EFFICIENCY FOR NEW AND REPLACEMENT COOLING EQUIPMENT
        EV=2
        DO RECCL=RTCLEUPT(EV)+2,RTCLEUPT(EV+1) !process cooling classes, skipping room AC
            EQC   =RTCLEQCL(RECCL)
            DENOM =0
            COUNT =0

            !     TYPE = INDEX FOR TYPE ARRAYS NEQTSHR AND REQTSHR
            !            INITIALIZE TO LAST ARRAY POSTION IN PREVIOUS END USE
            !              AND THEN COUNT VALID TYPES IN CURRENT END USE
            TYPE = RTTYPECT(EV)
            ! All records for cooling in RSMEQP, skipping the first 63 for room air
            DO RECTY=RTTYEUPT(EV),RTTYEUPT(EV+1) !all records in RSMEQP
              IF(CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
                IF (RTCENDIV(RECTY).EQ.R) THEN
                  IF(RTTYEQCL(RECTY).EQ.EQC) THEN
                    COUNT=COUNT+1
                    EQT=RTEQTYPE(RECTY)
                    RECAR(COUNT)=RECTY
                    EQTAR(COUNT)=EQT
                    DENOM=DENOM+NEQTSHRC(CurCalYr,EQT,B,R)
                  ENDIF !equipment is a member of this class
                ENDIF !census division filter
              ENDIF !Year availability filter
            ENDDO !All records from RSMEQP for this end use

            ! COMPLETE CALCULATION FOR NEW EQUIPMENT
            IF(DENOM.LE.0.0) THEN
              WTEQCEFFHV(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
              WTEQCSQFHV(CurCalYr,RECCL,B,R)=1.0
            ELSE
              SUM=0.0
              SUM1=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                SUM=SUM+EFFWEIGHTC(CurCalYr,TYPE,B,R)
                SUM1=SUM1+SQFTWEIGHTC(CurCalYr,TYPE,B,R)
              ENDDO
              WTEQCSQFHV(CurCalYr,RECCL,B,R)=SUM1/DENOM
              ! 5-21 since used as an inverse, don't invert numerator
              WTEQCEFFHV(CurCalYr,RECCL,B,R)=SUM/DENOM
            ENDIF

            IF (WTEQCEFFHV(CurCalYr,RECCL,B,R).EQ.0.0) THEN
              ! SHOULDN'T BE HERE!
              WRITE(9,'("NOTE: should not see this msg!",3i5)') CurCalYr,RECCL,eqt
              WTEQCEFFHV(CurCalYr,RECCL,B,R)= 1/RTBASEFF(RECSYear,RECCL)
            ENDIF
        ENDDO !all classes of equipment for this end use

 190  CONTINUE

END SUBROUTINE RSHVAC


!*******************************************************************
!     HEATING CHOICE SUBROUTINE
!       CALLED FOR CurIYr = 5,6,7,...
!*******************************************************************
      SUBROUTINE RHTRTEC
      IMPLICIT NONE
      COMMON/TESTHT/HTYSSHR(RECSYear:EndYr,nHeatClasses,mNumBldg,mNumCR)
      ! LOCAL VARIABLES
      REAL*4 TOTEWTN(nHeatClasses,mNumBldg,mNumCR),TOTEWTR(nHeatClasses,mNumBldg,mNumCR)
      REAL*4 EQWTN(nHeatTypes,mNumBldg,mNumCR),EQWTR(nHeatTypes,mNumBldg,mNumCR)
      REAL*4 HEATSYS(RECSYear:EndYr,nHeatTypes,mNumBldg,mNumCR-2),SYSTOT
      REAL*4 EQCOST,CAPITAL,RETAIL,CAPITAL1
      REAL*4 HDDFACT(mNumCR)
      REAL*4 EQFSHRR,EQFSHRN,OPCOST(3),BLDRWT !OPCOST(3) represents 1)replacement equipment for housing unit existing in RECS year, 2)new equipment in post-RECS-built housing unit, and 3)replacement equipment in post-RECS-built housing unit	!kj - should EQFSHRR and EQFSHRN be declared with(nHeatTypes), or is that no longer necessary because they've already been populated once?
      REAL*4 RTEFFAC(2),DECAY,ECTEMP,DENOM,SUM,DENOM2,e
      REAL*4 HTYSSHR,OTSHRT,HSYSTOT,LAGFACTOR,tmplogit
      !These variables are involved in the efficiency choice calculation.	!kj - bookmark to clarify comment
      ! RECAR and EQTAR are dimensioned for the number of choices across efficiency types in any single year.
     INTEGER EU,EUPR,RECTY,RECCL,R,B,F,EQT,EQC,TYPE,COUNT,L
     INTEGER RECAR(nHeatTypes),EQTAR(nHeatTypes)  !EqpParam

!*******************************************************************
!   THE GENERAL FORM OF THIS SUBROUTINE APPLIES TO ALL END USES
!     SET EU = 1 TO SEARCH THE SPACE HEATING SECTION OF THE DATA
!*******************************************************************
      EU=1
      EUPR=1
      ALPHA1=-0.50
      BLDRWT=6.0 ! FACTOR TO DISCOUNT FUEL PRICE IMPACT IN BUILDERS' FUEL CHOICE DECISION	!kj

      ! MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,mNumCR-2
        PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
      ENDDO

      ! COMPUTE HDDFACT
      DO R=1,mNumCR-2
          HDDFACT(R)=(HDDADJ(CurCalYr,R)/HDDADJ(RECSYear,R))**2.00 !A 10% increase in HDD would increase space heating consumption by 21% (e.g., 1.10^2.00=1.21)
      ENDDO

      ! COMPUTE DECAY RATE USED TO COMPUTE LIFE CYCLE COST (FIRST ITERATION ONLY)
      IF(CURITR.EQ.1) THEN
        DECAY = (1-((1+ResDiscountRate)**(-Tenure)))/ResDiscountRate
      ENDIF

      !  OUTTER LOOPS ARE CENSUS DIVISION AND BUILDING TYPE
      DO 90 R=1,mNumCR-2
        DO 90 B=1,mNumBldg

          ! INITIALIZE ARRAYS
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            TOTEWTR(RECCL,B,R)=0.0
            TOTEWTN(RECCL,B,R)=0.0
          ENDDO
          ! VARIABLES USED THIS SECTION:
          ! RSMEQP and RSCLASS Variables
          ! RTTYEUPT(EU)   = 0 FOR EU=1 (BEFORE BEGINNING OF FILE)
          ! RTTYEUPT(EU+1) = LAST RECORD # IN END USE 1 (SPACE HEATING)
          ! RECTY          = RECORD # FROM RSMEQP.TXT FILE
          ! EQT            = EQUIPMENT TYPE NUMBER FROM RSMEQP.TXT FILE
          ! EQC            = EQUIPMENT CLASS NUMBER FROM RSMEQP.TXT FILE
          ! RECCL          = RECORD # FROM RSCLASS.TXT FILE
          ! F              = FUEL # FROM RSCLASS.TXT FILE
          ! RTEQEFF(RECTY) = SPECIFIC EQUIPMENT EFFICIENCY FROM RSMEQP.TXT FILE
          ! EQCEFF(Y,RECCL)= FORECAST RETIRING EFFICIENCY FROM RSEFF01.TXT (computed in vintaging workbook)
          ! RTBASEFF(RECSYear,RECCL) = AVERAGE STOCK EFFICIENCY FROM RSCLASS.TXT FILE
          ! BASELOAD (RECCL) = STANDARD LEVEL EFFICIENCY FOR HVAC (THROUGH RECCL=nHeatClasses+nCoolClasses=16) FROM RSUECSHL.TXT

          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1) !for all RSMEQP records this end use
            ! Filter for year availability
            IF(CurCalYr.GE.RTINITYR(RECTY).AND. &
               CurCalYr.LE.RTLASTYR(RECTY)) THEN
             ! Filter for census division
             IF (RTCENDIV(RECTY).EQ.R) THEN
              EQT=RTEQTYPE(RECTY)
              EQC=RTTYEQCL(RECTY)
              RECCL=RTCLEUPT(EU)+EQC
              F    =RTFUEL(RECCL)

              ! COMPUTE EFFICIENCY FACTOR USED IN COMPUTING OPERATING COST
              IF(RTEQEFF(RECTY).NE.0.0) THEN
                ! RTEFFAC(1) is used to adjust UECs for replacements from the original stock of equipment from RECSYear
                RTEFFAC(1)=EQCEFF(CurCalYr,RECCL)/RTEQEFF(RECTY)    !eqceff is retiring stock efficiency
                ! RTEFFACt(2) is used to adjust RECSYear UECs for new construction decisions
                RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)/RTEQEFF(RECTY)  !rtbaseff is stock efficiency at RECSYear
               ELSE
                WRITE(9,'("shouldnt see",3i5,3e15.4)') CurCalYr,RECCL,eqt,eqceff(CurCalYr,RECCL),rteqeff(recty),rtbaseff(RECSYear,RECCL)
                RTEFFAC(1)=RTBASEFF(RECSYear,RECCL)
                RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)
              ENDIF

              ! SET CAPITAL COSTS
              !  If COSTTRSW = 1, use function EQCOST to compute capital
              !     cost of new equipment.
              !  If COSTTRSW = 0, use constant value from RSMEQP file for capital
              !     cost of new equipment.
              IF (COSTTRSW.EQ.1) THEN
                CAPITAL =  EQCOST(RECTY,CurCalYr,"CAP")
                IF ((EQC.NE.2).AND.(EQC.NE.10).AND.(EQC.NE.11)) THEN
                   ! If not a HP technology, then add typical retail cost for central air conditioning
                   CAPITAL1= RTRECOST(RECTY)+2200.  !updated using April 2018 tech report (using 2015 installed base retail equipment cost in RTEKDOLLARYR dollars)  !ACcost
                  ELSE
                   CAPITAL1= RTRECOST(RECTY)
                ENDIF
               ELSE
                CAPITAL =  RTEQCOST(RECTY)
                IF ((EQC.NE.2).AND.(EQC.NE.10).AND.(EQC.NE.11)) THEN
                  ! If not a HP technology, then add typical retail cost for central air conditioning
                  CAPITAL1= RTRECOST(RECTY)+2200.  !updated using April 2018 tech report (using 2015 installed base retail equipment cost in RTEKDOLLARYR dollars)  !ACcost
                 ELSE
                  CAPITAL1= RTRECOST(RECTY)
                ENDIF
              ENDIF

              ! CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOR CHANGES
              !  i.e., reduce implicit discount rates as real prices increase
              IF ((CurCalYr.GT.2008).AND. &	!kj - 2008 marks last year before American Clean Energy and Security Act of 2009 (Waxman-Markey bill)?
                (PRICES(F,R,CurCalYr).GT.PRICES(F,R,RECSYear))) THEN
                HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
                ELIGBLE=HRDRATE - 0.07	!kj
                IF (ELIGBLE.GT.0.0) THEN
                  HRDADJ= ELIGBLE * &
                    ((PRICES(F,R,CurCalYr)/PRICES(F,R,RECSYear))**ALPHA1 )
                  BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
                 ELSE
                  BETA1DR(RECTY)=RTECBTA1(RECTY)
                ENDIF
               ELSE
               BETA1DR(RECTY)=RTECBTA1(RECTY)
              ENDIF

              ! COMPUTE THE PART OF THE EQUIMENT CHOICE WEIGHT NOT DEPENDENT ON REGION AND BUILDING TYPE
              ECTEMP = RTECBIAS(RECTY) + (BETA1DR(RECTY)*CAPITAL)	!kj - reincorporate into EQWTN and EQWTR equations below (similar to other end uses?)

             ! CALCULATE OPERATING COST FOR 3 DECISION TYPES
             ! UECS: EQCUEC = RECSYear UEC FROM RSUEC.TXT
             ! NEWHEATUEC = NEW UECS READ IN FROM RSHLUEC.TXT NOT YEAR DEPENDENT, SO ADJUST BELOW
             ! EQCAHVUEC =

             IF (CurCalYr.EQ.RECSYear+1) THEN
                !prices x original RECS UEC x efficiency adjustment x hddadj
                OPCOST(1)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B)*RTEFFAC(1)*HDDFACT(R)

                ! FOR BUILDER CHOICE IN NEW CONSTRUCTION: DILUTE OPCOST USING BLDRWT
                !prices x new construction UEC from RSUECSHL.txt / standard efficiency x basestock efficiency / specific equipment efficiency x hddadj / adj for builder
               OPCOST(2)=PRICES(F,R,CurCalYr)*(NEWHEATUEC(RECCL,B,R)/BASELOAD(RECCL))*RTEFFAC(2)*HDDFACT(R)/BLDRWT
               OPCOST(3)=PRICES(F,R,CurCalYr)*NEWHEATUEC(RECCL,B,R)/BASELOAD(RECCL)*RTEFFAC(2)*HDDFACT(R)
               ELSE
               OPCOST(1)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B)*RTEFFAC(1)*HDDFACT(R)*(EHSHELL(CurCalYr-1,F,R,B)/EHSHELL(RECSYear,F,R,B))
               OPCOST(2)=PRICES(F,R,CurCalYr)*(NEWHEATUEC(RECCL,B,R)/BASELOAD(RECCL))*RTEFFAC(2)*HDDFACT(R)/BLDRWT
               OPCOST(3)=PRICES(F,R,CurCalYr)*EQCAHVUEC(CurCalYr-1,RECCL,b,r)*(AHSHELL(CurCalYr-1,F,R,B)/EHSHELL(RECSYear,F,R,B))*RTEFFAC(2)*HDDFACT(R)
             ENDIF

             ! CALCULATE LIFE CYCLE COSTS
             LFCY(EQT,B,R,1)=CAPITAL +  (OPCOST(1)*DECAY)  !Replacement choice (EQWTR) for homeowner of house existing in RECS year (equipment costs + installation costs)
             LFCY(EQT,B,R,2)=CAPITAL1 + (OPCOST(2)*DECAY)  !New construction choice for builder, counting only a fraction of operating costs in the decision,
                                                           ! thus favoring a choice toward equipment with lower first costs (also builder does incur different
                                                           ! installation costs than the homeowner does)
             LFCY(EQT,B,R,3)=CAPITAL1 + (OPCOST(3)*DECAY)  !Replacement choice (EQWTN) for homeowner in post-RECS added house

             ! COMPUTE WEIGHTS FOR REPLACEMENT EQUIPMENT TYPES
             EQWTR(EQT,B,R)= EXP(ECTEMP+(RTECBTA2(RECTY)*OPCOST(1)) + (RTECBTA3(RECTY)*LFCY(EQT,B,R,1)))
             TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)

             ! COMPUTE WEIGHTS FOR POST-RECS YEAR REPLACEMENT(?) EQUIPMENT TYPES
             ! OPCOST(3) represents 1)replacement equipment for housing unit existing in RECS year, 2)new equipment in post-RECS-built housing unit, and 3)replacement equipment in post-RECS-built housing unit
             EQWTN(EQT,B,R)= EXP(ECTEMP+(RTECBTA2(RECTY)*OPCOST(3)) + (RTECBTA3(RECTY)*LFCY(EQT,B,R,3)))	!kj - Other major end uses (except refrigeration, freezing, and lighting) use OPCOST(2) rather than OPCOST(3)
             TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)

             ENDIF !filter census division
            ENDIF  !filter year availability
          ENDDO    !for all RSMEQP records this enduse

!*******************************************************************
!     CALCULATE NEW AND REPLACEMENT MARKET SHARES
!*******************************************************************
          TYPE = RTTYPECT(EU)
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            ! Filter for year availability
            IF(CurCalYr.GE.RTINITYR(RECTY).AND. &
                CurCalYr.LE.RTLASTYR(RECTY)) THEN
             ! Filter for census division
             IF (RTCENDIV(RECTY).EQ.R) THEN
              TYPE=TYPE+1
              ! SET EQUIPMENT CLASS (EQC) & EQUIPMENT TYPE (EQT)
              EQC=RTTYEQCL(RECTY)
              EQT=RTEQTYPE(RECTY)
              ! SET NEW EQUIPMENT FUEL SHARES (AND NEQTSHR FOR WATER HEATING)
              IF (TOTEWTN(EQC,B,R).GT.0.0) THEN
                  EQFSHRN=EQWTN(EQT,B,R)/TOTEWTN(EQC,B,R)
                ELSE
                  EQFSHRN=0.0
              ENDIF
              NEQTSHR(CurCalYr,TYPE,B,R)=EQFSHRN

              ! SET REPLACEMENT EQUIPMENT FUEL SHARES (AND NEQTSHR FOR WATER HEATING)
              IF (TOTEWTR(EQC,B,R).GT.0.0) THEN
                  EQFSHRR=EQWTR(EQT,B,R)/TOTEWTR(EQC,B,R)
                ELSE
                  EQFSHRR=0.0
              ENDIF

              REQTSHR(CurCalYr,TYPE,B,R)=EQFSHRR

            ENDIF
           ENDIF
          ENDDO

          ! CALCULATE WEIGHTED EFFICIENCY FOR NEW AND REPLACEMENT HEATING EQUIPMENT
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC   =RTCLEQCL(RECCL)
            DENOM=0
            DENOM2=0
            COUNT =0
            TYPE = RTTYPECT(EU) !initialize to last equipment record, previous end use
            DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
              IF(CurCalYr.GE.RTINITYR(RECTY) &
                .AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
              IF (RTCENDIV(RECTY).EQ.R) THEN
               TYPE=TYPE+1      !counting valid types
                IF(RTTYEQCL(RECTY).EQ.EQC) THEN
                  COUNT=COUNT+1
                  EQT=RTEQTYPE(RECTY)
                  RECAR(COUNT)=RECTY
                  EQTAR(COUNT)=TYPE
                  DENOM=DENOM+NEQTSHR(CurCalYr,TYPE,B,R)
                  DENOM2=DENOM2+REQTSHR(CurCalYr,TYPE,B,R)
                ENDIF
              ENDIF
             ENDIF
            ENDDO

           ! COMPLETE CALCULATION FOR NEW EQUIPMENT
            IF(DENOM.LE.0.0) THEN
              WTEQCEFFN(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                SUM=SUM+(NEQTSHR(CurCalYr,TYPE,B,R)/RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFN(CurCalYr,RECCL,B,R)=SUM/DENOM
            ENDIF
            !based on if statement above should never be here!
            IF (WTEQCEFFN(CurCalYr,RECCL,B,R).EQ.0.0) THEN
                WTEQCEFFN(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
            ENDIF

            ! COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT
            IF(DENOM2.LE.0.0) THEN
              WTEQCEFFR(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                SUM=SUM+(REQTSHR(CurCalYr,TYPE,B,R)/RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFR(CurCalYr,RECCL,B,R)=SUM/DENOM2
            ENDIF
            IF (WTEQCEFFR(CurCalYr,RECCL,B,R).EQ.0.0) THEN
                WTEQCEFFR(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
            ENDIF

          ENDDO

          ! COMPUTE LOGIT VALUES FOR EACH HEATING SYSTEM AND SUM OVER TYPE
          OTSHRT=0.0
          SYSTOT=0.0
          LAGFACTOR=0.9	!kj - does this need to be updated?
          ! COMPUTE PERCENT ELIGIBLE FOR FUEL CHOICE SIMULATION
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)
              EQT=RTCLTYPT(RECCL)
              TYPE = RTTYPECT(EU) + EQT  ! not used...
              tmplogit=EXP( RTFCBIAS(RECCL,B,R)+ RTFCBETA(RECCL)*LFCY(EQT,B,R,2) )
              HEATSYS(CurCalYr,EQT,B,R)=LAGFACTOR*HEATSYS(CurCalYr-1,EQT,B,R) &
                          +(1-LAGFACTOR)*EXP( RTFCBIAS(RECCL,B,R)+ RTFCBETA(RECCL)*LFCY(EQT,B,R,2) )
              SYSTOT=SYSTOT+HEATSYS(CurCalYr,EQT,B,R)

          !Diagnostics only:
          IF ((CurCalYr.GT.RECSYear) .AND. (B.EQ.1) .AND. (R.EQ.5)) THEN	!kj - what is R=5? CD 5?
!             WRITE(9,'("parm checks",3i5,3e15.4)') CurCalYr,RECCL,EQT,HEATSYS(CurCalYr-1,EQT,B,R),tmplogit,HEATSYS(CurCalYr,EQT,B,R)
          ENDIF

          ENDDO

          ! COMPUTE NORMALIZED SHARES FOR EACH FUEL SYSTEM CHOICE
          HSYSTOT=0.0
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            EQT=RTCLTYPT(RECCL)
            IF(SYSTOT.NE.0.0) THEN
              HTYSSHR(CurCalYr,EQC,B,R)=HEATSYS(CurCalYr,EQT,B,R)/SYSTOT
            ELSE
              HTYSSHR(CurCalYr,EQC,B,R)=0.0
            ENDIF

          !Use heating equipment shares in historical years as shown in RSHTSHR.txt input file; otherwise, use calculated value
          IF (CurCalYr.GT.HTSHRYR) HSYSSHR(CurCalYr,EQC,B,R)=HTYSSHR(CurCalYr,EQC,B,R)  !HtShrYr

          !Diagnostics only:
!          IF ((CurCalYr.GT.HTSHRYR) .AND. (b.EQ.1) .AND. (r.EQ.5)) THEN  !HtShrYr
!              WRITE(19,'("system shares",2i5,3e15.4)') CurCalYr,eqc,htysshr(CurCalYr,EQC,B,R), &
!                   htysshr(CurCalYr-1,EQC,B,R), htysshr(CurCalYr-2,EQC,B,R)
!          ENDIF

          ENDDO

90    CONTINUE

      END SUBROUTINE RHTRTEC


!*******************************************************************
!     HEATING ADDED/REPLACED SUBROUTINE
!*******************************************************************
      SUBROUTINE RHTRADD
      IMPLICIT NONE
!bookmark      COMMON/SHINV/SHELLInvest(RECSYear:EndYr,nHeatClasses,nShellTypes,mNumBldg,mNumCR), &
!bookmark                   SHELLSUBSIDY(RECSYear:EndYr,nHeatClasses,nShellTypes,mNumBldg,mNumCR)
      REAL*4 SHARESN(nHeatTypes,mNumBldg,mNumCR) !bookmark ,SHARESR(nHeatTypes,mNumBldg,mNumCR) !bookmark,SHELLINVEST,SHELLSUBSIDY
      REAL*4 SWT(RECSYear:EndYr),SWF(RECSYear:EndYr),SA, HSR, ESR, SVRTE
      REAL*4 NEWSHELLWT(RECSYear:EndYr,6,mNumBldg,mNumCR),NEWADDWT(RECSYear:EndYr,6,mNumBldg,mNumCR)
             !newshellwt & newaddwt are dimensioned by the number of fuels reflected in RSCLASS
! bookmark      REAL*4 SHWTNUM(RECSYear:EndYr,nShellTypes)
! bookmark      REAL*4 SHLEVELWT(RECSYear:EndYr,nHeatClasses,nShellTypes,mNumBldg,mNumCR-2)
! bookmark      REAL*4 SHWTNUMB(RECSYear:EndYr,nShellTypes,mNumBldg)  !bookmark ,SHELLTOTALInvest(RECSYear:EndYr)
      INTEGER EQT,RECTY,TYPE,COUNT,L,EQCAR(10),RECCLSW,V
      INTEGER EU,EQC,RECCL,Y,R,B,TEMP,F,FSW,NUMEQT,T,S,Y1,E,D

      EU = 1 !Space Heating End Use Number
      DO 5 R=1,mNumCR-2
        DO 5 B=1,mNumBldg
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            SHARESN(EQC,B,R)=0.0
!bookmark            SHARESR(EQC,B,R)=0.0
            EQCRP90RP(CurCalYr,RECCL,B,R)=0.0
            EQCSR90(CurCalYr,RECCL,B,R)=0.0
            EQCSUR(CurCalYr,RECCL,B,R)=0.0
           IF(B.EQ.1) EQCREP(CurCalYr,RECCL,B,R) = 0.0
          ENDDO

          !Aggregate heating equipment shares by the nHeatClasses # of general heating classes in RSCLASS
          TYPE = RTTYPECT(EU)
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) &
              THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              TYPE=TYPE+1
              EQC=RTTYEQCL(RECTY)
              EQT=RTEQTYPE(RECTY)
              SHARESN(EQC,B,R)=HSYSSHR(CurCalYr,EQC,B,R)
!bookmark              SHARESR(EQC,B,R)=SHARESR(EQC,B,R)+REQTSHR(CurCalYr,TYPE,B,R)
            ENDIF
           ENDIF
          ENDDO

          ! CALCULATE HEATERS ADDED IN CurIYr-1
          !  CUMULATE SURVIVING EQUIPMENT REPLACED FOR RECS YEAR VINTAGE PRIOR TO PREVYR
          !  CUMULATE SURVIVING NEW HEATERS ADDED PRIOR TO PREVYR TO ESTIMATE NH
          !  SA REPRESENTS NH at PREVYR-1
          !  CUMULATE SURVIVING NEW HEATERS ADDED & REPLACED PRIOR TO PREVYR
          !  REPLACEMENT EQUIPMENT = SURV.HOUSES(SA) -  SURV.EQUIP(EQCSUR)
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1) !process all classes this enduse
            EQC=RTCLEQCL(RECCL)
            EQCADD(CurCalYr,RECCL,B,R)=(HSEADD(CurCalYr,B,R)*SHARESN(EQC,B,R))
            SA=0.0

            ! Calculate replacement equipment from original RECS-year stock
            IF (CurCalYr.EQ.RECSYear+1) THEN
              EQCRP90(CurCalYr,RECCL,B,R)=(EQCRET(CurCalYr,RECCL) &
               *EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-RECSYear)))
             ELSE
              EQCRP90(CurCalYr,RECCL,B,R)=((EQCRET(CurCalYr,RECCL)- &
              EQCRET(CurCalYr-1,RECCL))*EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-RECSYear)))
            ENDIF

            ! COMPUTE AND VINTAGE REPLACEMENTS OF REPLACEMENTS
            IF(CurCalYr.GT.RECSYear+1) THEN
             DO Y=RECSYear+1,CurCalYr-1
              TEMP=CurCalYr-Y
              ESR=SVRTE(RTALPHA(RECCL),TEMP-1,RTK(RECCL),RTLAMBDA(RECCL)) &
                   -SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
              EQCRP90RP(CurCalYr,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R) + &
                ( EQCRP90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
                  EQCRP90RP(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
             ENDDO
            ENDIF


           ! CALCULATE SURVIVING REPLACEMENTS
           IF(CurCalYr.GT.RECSYear+1) THEN
              DO Y=RECSYear+1,CurCalYr-1 !loop previous years
                TEMP=CurCalYr-Y
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
                EQCSR90(CurCalYr,RECCL,B,R)=(EQCSR90(CurCalYr,RECCL,B,R) + &
                 (EQCRP90(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
                 EQCRP90RP(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))))
                HSR=HDR(B)**(TEMP)
                SA = (SA + EQCADD(Y,RECCL,B,R)*HSR)
                EQCSUR(CurCalYr,RECCL,B,R) = (EQCSUR(CurCalYr,RECCL,B,R) + &
                 (((EQCADD(Y,RECCL,B,R)+EQCREP(Y,RECCL,B,R))*(HSR*ESR))))
              ENDDO !loop previous years
            ENDIF

            !************************************************************
            ! CALCULATE REPLACEMENT HEATERS FOR NEW VINTAGE IN CurIYr-1
            !  NOTE: REPLACES WITH LIKE IF NOT SINGLE-FAMILY HOMES
            !  NOTE: FOR NEW HOUSES (NH) - CurIYr-1 IS THE LAGGED VALUE
            !************************************************************
            !  SUBROUTINE 'REPLACE' DISTRIBUTES REPLACEMENTS IN POST-RECS-YEAR
            !   SINGLE-FAMILY HOMES WHEN LAST ARGUEMENT = 1
            IF(B.EQ.1) THEN
               ! First, store what replacements would have been if no switching allowed.
                 OEQCREP(CurCalYr,RECCL,1,R)=SA-EQCSUR(CurCalYr,RECCL,B,R)
               ! Call REPLACE to distribute replacements.
                 CALL REPLACE(EU,R,B,RECCL,1)
              ELSE
               !  No switching allowed in multifamily or mobile homes.
                 EQCREP(CurCalYr,RECCL,B,R)=SA-EQCSUR(CurCalYr,RECCL,B,R)
            ENDIF

         ENDDO !Process all classes for this end use

5     CONTINUE ! Census Divisions and Building Types

      ! The following call to REPLACE with final arguement = 2 distributes
      !   replacements in existing single-family homes.
      B = 1
      DO  R=1,mNumCR-2
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        OEQCRP90(CurCalYr,RECCL,B,R) = EQCRP90(CurCalYr,RECCL,1,R)
        OEQCRP90R(CurCalYr,RECCL,B,R) = EQCRP90RP(CurCalYr,RECCL,1,R)

          CALL REPLACE(EU,R,B,RECCL,2)
        ENDDO
      ENDDO

      DO  R=1,mNumCR-2
       DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
         SWITCHTO(CurCalYr,RECCL,B,R)=0.0
         SWITCHTOR(CurCalYr,RECCL,B,R)=0.0
          DO RECCLSW=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           IF (RECCLSW.NE.RECCL) THEN
             SWITCHTO(CurCalYr,RECCL,B,R)=SWITCHTO(CurCalYr,RECCL,B,R)+ &
                                        EQCSW90(CurCalYr,RECCLSW,RECCL,B,R)
             SWITCHTOR(CurCalYr,RECCL,B,R)=SWITCHTOR(CurCalYr,RECCL,B,R)+ &
                                         EQCSW90R(CurCalYr,RECCLSW,RECCL,B,R)
           ENDIF
          ENDDO
       ENDDO
      ENDDO

      DO  R=1,mNumCR-2
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)

          EQCRP90(CurCalYr,RECCL,B,R)= EQCRP90(CurCalYr,RECCL,B,R)- &
                                     SWITCHES(CurCalYr,RECCL,B,R)
          EQCRP90RP(CurCalYr,RECCL,B,R)= EQCRP90RP(CurCalYr,RECCL,B,R)- &
                                       SWITCHESR(CurCalYr,RECCL,B,R)+ SWITCHTOR(CurCalYr,RECCL,B,R) &
                                     + SWITCHTO(CurCalYr,RECCL,B,R)
        ENDDO
      ENDDO

      SWF(CurCalYr)=0.0
      SWT(CurCalYr)=0.0

      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
       DO  R=1,mNumCR-2
          SWT(CurCalYr)=SWT(CurCalYr)+SWITCHTO(CurCalYr,RECCL,B,R)+ &
                      SWITCHTOR(CurCalYr,RECCL,B,R)
          SWF(CurCalYr)=SWF(CurCalYr)+SWITCHES(CurCalYr,RECCL,B,R)+ &
                      SWITCHESR(CurCalYr,RECCL,B,R)
          SWTOTAL(CurCalYr,RECCL,R)= SWITCHTO(CurCalYr,RECCL,B,R)+SWITCHTOR(CurCalYr,RECCL,B,R)
          SWFTOTAL(CurCalYr,RECCL,R)=SWITCHES(CurCalYr,RECCL,B,R)+SWITCHESR(CurCalYr,RECCL,B,R)
       ENDDO
      ENDDO

       DO  R=1,mNumCR-2
         DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1) ! Sums equipment in existing RECS-year households
           EQCND90(CurCalYr,RECCL,B,R) = EQCRP90(CurCalYr,RECCL,B,R) + EQCESE(CurCalYr,RECCL,B,R) + &
                                         EQCSR90(CurCalYr,RECCL,B,R)+ EQCRP90RP(CurCalYr,RECCL,B,R) 
         ENDDO
       ENDDO


       !   ADD TOTAL HEATERS AND COMPUTE NEW SHELL EFFICIENCY
       DO R=1,mNumCR-2
           DO B=1,mNumBldg
                 DO F=1,6  !Distillate fuel oil, propane, natural gas, electricity, kerosene, wood	!kj - kerosene being combined with distillate fuel oil for AEO2019
              NHSHELL(CurCalYr,F,R,B)=0.0
              NEWSHELLWT(CurCalYr,F,B,R)=0.0
              NEWADDWT(CurCalYr,F,B,R)=0.0
             ENDDO
         ENDDO
       ENDDO

      DO RECCL=1,nHeatClasses
         DO S=1,nShellTypes
          DO B=1,mNumBldg
           DO R=1,mNumCR-2
! bookmark              SHLEVELWT(CurCalYr,EQC,S,B,R)=0.0
             ENDDO
            ENDDO
           ENDDO
      ENDDO

      ! Compute Shell Investment
! bookmark      SHELLTOTALInvest(CurCalYr)=0.0
      DO 32 R=1,mNumCR-2
        DO 32 B=1,mNumBldg
          DO 32 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            IF (RECCL.LE.2.OR.RECCL.EQ.10) F=4 !ELEC_RAD, ELEC_HP, and GEO_HP
                IF (RECCL.EQ.3.OR.RECCL.EQ.4.OR.RECCL.EQ.11) F=3 !NG_FA, NG_RAD, and NG_HP
                IF (RECCL.EQ.5) F=1 !KERO_FA	!kj - switched tech menu kerosene (RTFUEL=5) equipment/shells to distillate fuel oil (RTFUEL=1) for now
                IF (RECCL.EQ.6) F=2 !LPG_FA
            IF (RECCL.EQ.7.OR.RECCL.EQ.8) F=1 !DIST_FA and DIST_RAD
            IF (RECCL.EQ.9) F=6 !WOOD_HT
            EQC=RTCLEQCL(RECCL)
            HEATOT(CurCalYr,EQC,B,R)=  EQCESE(CurCalYr,RECCL,B,R)+ &
              EQCRP90(CurCalYr,RECCL,B,R)+EQCSR90(CurCalYr,RECCL,B,R)+ &
              EQCADD(CurCalYr,RECCL,B,R) +EQCREP(CurCalYr,RECCL,B,R) + &
              EQCSUR(CurCalYr,RECCL,B,R)+EQCRP90RP(CurCalYr,RECCL,B,R)

            NEWSHELLWT(CurCalYr,F,B,R)=NEWSHELLWT(CurCalYr,F,B,R)+ &
              EQCADD(CurCalYr,RECCL,B,R)*HSHELL(CurCalYr,RECCL,B,R)
              NEWADDWT(CurCalYr,F,B,R)=NEWADDWT(CurCalYr,F,B,R)+EQCADD(CurCalYr,RECCL,B,R)
              DO 32 S=1,nShellTypes
! bookmark               SHLEVELWT(CurCalYr,EQC,S,B,R)= &
! bookmark                 EQCADD(CurCalYr,EQC,B,R)*SHLEVELH(CurCalYr,EQC,S,B,R)
! bookmark               SHELLTOTALInvest(CurCalYr)=SHELLTOTALInvest(CurCalYr) + &
! bookmark                 SHLEVELWT(CurCalYr,EQC,S,B,R)*SHELLInvest(CurCalYr,EQC,S,B,R)
 32   CONTINUE ! End loop compute shell investment


!      DO S=1,nShellTypes
! bookmark        SHWTNUM(CurCalYr,S)=0.0
!          DO B=1,mNumBldg
! bookmark           SHWTNUMB(CurCalYr,S,B)=0.0
!             DO R=1,mNumCR-2
!            DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
! bookmark             SHWTNUM(CurCalYr,S)=SHWTNUM(CurCalYr,S)+SHLEVELWT(CurCalYr,RECCL,S,B,R)
! bookmark             SHWTNUMB(CurCalYr,S,B)=SHWTNUMB(CurCalYr,S,B)+SHLEVELWT(CurCalYr,RECCL,S,B,R)
!            ENDDO !RECCL
!           ENDDO !r
!            ENDDO !b
!        ENDDO !s

      ! Initialize Energy Star Shells (by assumption for higher efficiencies)	!kj
! bookmark      CUMSHWTNUM(RECSYear,3)=57515.
! bookmark      CUMSHWTNUM(RECSYear,4)=5752.
! bookmark      CUMSHWTNUM(RECSYear,5)=575.
! bookmark      SHWTNUM(RECSYear,3)=27359.
! bookmark      SHWTNUM(RECSYear,4)=2736.
! bookmark      SHWTNUM(RECSYear,5)=274.

!      DO S=3,5
!bookmark       CUMSHWTNUM(CurCalYr,S)=CUMSHWTNUM(CurCalYr-1,S)+SHWTNUM(CurCalYr,S)
!      ENDDO

      ! COMPUTE SHELL AVERAGE FOR EACH FUEL TYPE
      DO R=1,mNumCR-2
        DO B=1,mNumBldg
          DO F=1,6  !Distillate fuel oil, propane, natural gas, electricity, kerosene, wood	!kj - kerosene being combined with distillate fuel oil for AEO2019
            IF (NEWADDWT(CurCalYr,F,B,R).GT.0.0) THEN
              NHSHELL(CurCalYr,F,R,B)=NEWSHELLWT(CurCalYr,F,B,R)/NEWADDWT(CurCalYr,F,B,R)
            ELSE
              NHSHELL(CurCalYr,F,R,B)=1.0
            ENDIF
            NHSHELL(RECSYear,F,R,B)=NHSHELL(RECSYear+1,F,R,B)
          ENDDO
        ENDDO
      ENDDO

      DO D=1,mNumCR-2
           DO B=1,mNumBldg
          ! Adjustments changes to house size (cooling adjustments in positions 3 & 4 done in RCLADD)
          !  fossil fuel heating
          EXSQFTADJ(CurCalYr,B,D,1)=(ELASTIC(1,D)*((EXSQRFOOT(CurCalYr,B,D)-EXSQRFOOT(RECSYear,B,D))/ &
                        EXSQRFOOT(RECSYear,B,D))*NHSHELL(CurCalYr,3,D,B))+1
          !  electric heating
          EXSQFTADJ(CurCalYr,B,D,2)=(ELASTIC(2,D)*((EXSQRFOOT(CurCalYr,B,D)-EXSQRFOOT(RECSYear,B,D))/ &
                        EXSQRFOOT(RECSYear,B,D))*NHSHELL(CurCalYr,4,D,B))+1
          !  furnace fans
          EXSQFTADJ(CurCalYr,B,D,5)=(ELASTIC(5,D)*((EXSQRFOOT(CurCalYr,B,D)-EXSQRFOOT(RECSYear,B,D))/ &
                        EXSQRFOOT(RECSYear,B,D))*NHSHELL(CurCalYr,3,D,B))+1
         ENDDO
      ENDDO

        DO B=1,mNumBldg
        DO R=1,mNumCR-2
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
              DO Y=CurCalYr,EndYr       ! VINTAGE EQUIPMENT FOR AVERAGE STOCK ACCOUNTING
                TEMP=Y-CurCalYr
                HSR=HDR(B)**(TEMP)
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
                EQR90FUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90(CurCalYr,RECCL,B,R)*ESR*HSR)
                EQR90RPFUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R)*ESR*HSR)
                EQREPFUT(CurCalYr,Y,RECCL,B,R)=(EQCREP(CurCalYr,RECCL,B,R)*ESR*HSR)
                EQADDFUT(CurCalYr,Y,RECCL,B,R)=(EQCADD(CurCalYr,RECCL,B,R)*ESR*HSR)
              ENDDO
          ENDDO
        ENDDO
      ENDDO

      ! AGGREGATE HEATING SYSTEMS FOR INVESTMENT ANALYSIS
      T=CurCalYr
      Y=CurIYr
      NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)

        DO B=1,mNumBldg
         DO r=1,mNumCR-2
          TYPE = RTTYPECT(EU)
           DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)

           ! CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR
           IF(CurCalYr.GE.RTINITYR(RECTY).AND. &
             CurCalYr.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN

            TYPE=TYPE+1    ! INDEX to count the 'TYPE' records in RSMEQP
            EQT=RTEQTYPE(RECTY)
            EQC=RTTYEQCL(RECTY)
            RECCL=RTCLEUPT(EU)+EQC
                HEATINGTYPEPURCH(T,EQT,B,R,1)=(HVEQSHR(T,EQT,B,r)*EQCADD(T,RECCL,B,r))
                HEATINGTYPEPURCH(T,EQT,B,R,2)=(NEQTSHR(T,EQT,B,r)*(EQCREP(T,RECCL,B,r) + &
                   EQCRP90RP(T,RECCL,B,r)) + REQTSHR(T,EQT,B,r)*EQCRP90(T,RECCL,B,r) )
               DO S=1,nShellTypes
                SHELLBUILDS(T,EQT,S,B,R)=HTSHELLWT(T,EQT,S,B,R)*EQCADD(T,RECCL,B,r)
!bookmark                shellinvdb(T,EQT,S,B,R)=shellInvest(t,RECCL,s,b,r)*ifix(shellbuilds(T,EQT,S,B,R))
!bookmark                shellsubdb(T,EQT,S,B,R)=shellsubsidy(t,RECCL,s,b,r)*ifix(shellbuilds(T,EQT,S,B,R))
               ENDDO
           ENDIF
           ENDIF
           ENDDO
         ENDDO
        ENDDO

         ! RSGASCUST tracks the number of natural gas customers by looking across end uses.
         !  Note: it is not a constraint on hookups...
         DO R=1,mNumCR-2
           RSGASCUST(CurCalYr,R)=0.0
         ENDDO

        ! Natural gas space heating equipment defined in RSCLASS  (e.g., furnaces, boilers, heat pumps) are proxy for number of natural gas customers
        IF ((CurCalYr.EQ.RECSYear+1).AND.(CURITR.EQ.1)) THEN
         DO R=1,mNumCR-2
          RSGASCUST(RECSYear,R)=0.0
          DO B=1,mNumBldg
           RSGASCUST(RECSYear,R)=RSGASCUST(RECSYear,R)+EQCESE(RECSYear,3,B,R)+EQCESE(RECSYear,4,B,R)+EQCESE(RECSYear,11,B,R)
          ENDDO
         ENDDO
        ELSE
         DO R=1,mNumCR-2
          DO B=1,mNumBldg
           RSGASCUST(CurCalYr,R)=RSGASCUST(CurCalYr,R)+HEATOT(CurCalYr,3,B,R)+HEATOT(CurCalYr,4,B,R)+HEATOT(CurCalYr,11,B,R)
          ENDDO
         ENDDO
        ENDIF

       END SUBROUTINE RHTRADD


!*******************************************************************
!     HEATING CONSUMPTION SUBROUTINE
!*******************************************************************
      SUBROUTINE RHTRCON
      IMPLICIT NONE
      REAL*4 NFANUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),EFANUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
      REAL*4 ENUMER(mNumCR,mNumBldg), EDENOM(mNumCR,mNumBldg), &
       NDENOM(mNumCR,mNumBldg),ANUM(MNUMFUEL,mNumCR,mNumBldg), &
       ADEN(MNUMFUEL,mNumCR,mNumBldg), NNUMER(mNumCR,mNumBldg)
      REAL*4 HDDFACT(mNumCR),TEMP,TEMP1,TEMP2, NFANIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
      REAL*4 FANEFF(RECSYear:EndYr),NFANEFF(RECSYear:EndYr),ALPHA,ef1,ef2,ef3,alpha2,rbn,rbr,rba,AFANUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
      INTEGER F, F2, FCON, Q, V, YEAR,FAN,E,T,y1
      INTEGER EU,EUPR,EQC,RECCL,Y,R,B,EQCEHP,EQCGHP,D,S
      INTEGER RECCLEHP, RECCLGHP
! PRICES 1=Distillate Fuel Oil 2=Propane 3=Natural Gas 4=Electricity 5=Kerosene 6=Coal
! SHELL 1=Distillate Fuel Oil/Wood 2=Propane 3=Natural Gas 4=Electricity/Geothermal 5=Kerosene
!*******************************************************************
!  F    = FUEL NUMBER FROM RSCLASS FILE
!  FCON = FUEL NUMBER FOR CONSUMPTION (AS FOLLOWS)
!         1=Natural Gas 2=Electricity 3=Distillate Fuel Oil + Kerosene 4=Propane 5=N/A 6=Wood 7=Geothermal
!*******************************************************************

!  SET EU = 1 TO SEARCH THE SPACE HEATING SECTION OF THE DATA
      EU    =  1
      EUPR=1
      ALPHA=-.15; ef1=.5;ef2=.35;ef3=.15 !own-price elasticity and distribution
      ALPHA2=-.05  !heating shell adjustment

!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,mNumCR-2
        PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EU)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
      ENDDO

      DO R=1,mNumCR-2
        IF(CurCalYr.LE.IJUMPCALYR)THEN
          HDDFACT(R)=(HDDADJ(CurCalYr,R)/HDDADJ(RECSYear,R))**2.00 !A 10% increase in HDD would increase space heating consumption by 21% (e.g., 1.10^2.00=1.21)
        ENDIF
      ENDDO

      IF (CurCalYr.EQ.RECSYear+1) THEN
       DO 1 Y=RECSyear,EndYr
        IF (Y.LE.2018) THEN  !Furnace fan standard effective 2019	!kj - update when new standard available
         NFANEFF(Y)=1.0
         FANEFF(Y)=1.0
        ELSE
         NFANEFF(Y)=0.75 !Furance fans expected to be 25% more efficient due to 2019 standard?
         FANEFF(Y)=1.0
        ENDIF
 1    CONTINUE
      ENDIF

       IF (CURITR.EQ.1) THEN
       DO 2 R=1,mNumCR-2
         DO 2 B=1,mNumBldg
          EFANUEC(CurCalYr,R,B)=FANUEC(R,B)*FANEFF(CurCalYr)*EXSQFTADJ(CurCalYr,B,R,5)
            FANIUEC(R,B)=FANUEC(R,B)*FANEFF(CurCalYr)
 2     CONTINUE
      ENDIF

       DO 3 R=1,mNumCR-2
         DO 3 B=1,mNumBldg
          NFANUEC(CurCalYr,R,B)=FANUEC(R,B)*NFANEFF(CurCalYr)*exSQFTADJ(CurCalYr,B,R,5)
          NFANIUEC(CurCalYr,R,B)=FANUEC(R,B)*NFANEFF(CurCalYr)  ! INTENSITY UEC FOR INDEX
 3     CONTINUE
!*******************************************************************
! COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR RECS-YEAR EQUIPMENT
!*******************************************************************
      IF (CurCalYr.GE.RECSYear+1) THEN
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RTBASEFF(CurCalYr,RECCL)= STKEFF(CurCalYr,RECCL)
        ENDDO
      ENDIF

!*****************************************************************************
!  CALCULATE EXISTING HEATING & COOLING SHELL INDICES - before weatherization
!*****************************************************************************
        DO 5 R=1,mNumCR-2
          DO 5 B=1,mNumBldg
            DO F=1,MNUMFUEL-2
              EHSHELL(CurCalYr,F,R,B)=0.0
              IF (F.NE.6) THEN  !No price response for wood
                EHSHELL(CurCalYr,F,R,B)=EHSHELL(RECSYear,F,R,B)*RSELAST(F,R,ALPHA2,EF1,EF2,EF3,RECSYear,EUPR)&
                 -TECHG(CurCalYr,R,B)
               ELSE  !Wood-heated
                EHSHELL(CurCalYr,F,R,B)=EHSHELL(RECSYear,F,R,B)-TECHG(CurCalYr,R,B)
              ENDIF
            ENDDO

            ! Compute composite cooling shell by R & B based on 3 fuels	!kj - verify source of shares
            ECSHELL(CurCalYr,R,B)=ECSHELL(RECSYear,R,B)- &
            ((EHSHELL(RECSYear,1,R,B)-EHSHELL(CurCalYr,1,R,B))*0.1 + &  !Natural Gas?
             (EHSHELL(RECSYear,3,R,B)-EHSHELL(CurCalYr,3,R,B))*0.6 + &  !Distillate Fuel Oil?
             (EHSHELL(RECSYear,4,R,B)-EHSHELL(CurCalYr,4,R,B))*0.3 )*0.38  !Propane?

 5    CONTINUE

!*******************************************************************
!  Add weatherization (read in from RSMISC.txt)
!*******************************************************************
        DO 10 R=1,mNumCR-2
          DO 10 B=1,mNumBldg
            DO 9 F=1,MNUMFUEL-2
              !ZERO OUT ARRAYS
               ANUM(F,R,B)=0.0
               ADEN(F,R,B)=0.0
               EHSHELL(CurCalYr,F,R,B)=EHSHELL(CurCalYr,F,R,B)+(WTHRZTN(CurCalYr,1,R,B))  !LIupdate
               IF (EHSHELL(CurCalYr,F,R,B) .GT. EHSHELL(CurCalYr-1,F,R,B)) &
                EHSHELL(CurCalYr,F,R,B)=EHSHELL(CurCalYr-1,F,R,B)
               IF(EHSHELL(CurCalYr,F,R,B).LT.LIMIT) EHSHELL(CurCalYr,F,R,B)=LIMIT
  9   CONTINUE
         ECSHELL(CurCalYr,R,B)=ECSHELL(CurCalYr,R,B)+(WTHRZTN(CurCalYr,2,R,B))  !LIupdate
          IF (ECSHELL(CurCalYr,R,B) .GT. ECSHELL(CurCalYr-1,R,B)) &
            ECSHELL(CurCalYr,R,B)=ECSHELL(CurCalYr-1,R,B)
          IF(ECSHELL(CurCalYr,R,B).LT.LIMIT) ECSHELL(CurCalYr,R,B)=LIMIT
 10   CONTINUE

!*******************************************************************
!  CALCULATE AVERAGE NEW HEATING SHELL INDEX
!*******************************************************************
        DO 50 R=1,mNumCR-2
         DO 50 B=1,mNumBldg
          DO 50 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            F  =RTFUEL(RECCL)
            IF (CurCalYr.GT.RECSYear+1) THEN
            DO Y=RECSYear+1,CurCalYr-1
            ANUM(F,R,B)=ANUM(F,R,B)+(NHSHELL(Y,F,R,B)* &
              EQCADD(Y,RECCL,B,R))
            ADEN(F,R,B)=ADEN(F,R,B)+(EQCADD(Y,RECCL,B,R))
             ENDDO
            ENDIF
 50   CONTINUE
!*******************************************************************
        DO 55 R=1,mNumCR-2
         DO 55 B=1,mNumBldg
          DO 55 F=1,MNUMFUEL-2
          IF (ADEN(F,R,B).LE.0) THEN
             AHSHELL(CurCalYr,F,R,B)=AHSHELL(CurCalYr-1,F,R,B)
          ELSE
             AHSHELL(CurCalYr,F,R,B)=ANUM(F,R,B)/ADEN(F,R,B)
          ENDIF
!         IF(AHSHELL(CurCalYr,F,R,B).GT.AHSHELL(CurCalYr-1,F,R,B)) &
!             AHSHELL(CurCalYr,F,R,B)=AHSHELL(CurCalYr-1,F,R,B)
         IF(AHSHELL(CurCalYr,F,R,B).LT.LIMIT) AHSHELL(CurCalYr,F,R,B)=LIMIT
         IF(CurCalYr.LE.(RECSYear+1)) THEN
          AHSHELL(CurCalYr,F,R,B)=NHSHELL(CurCalYr,F,R,B)
         ENDIF
 55   CONTINUE
!*******************************************************************
!  CALCULATE NEW, REPLACEMENT AND AVERAGE UECS
!*******************************************************************
      DO 80 R=1,mNumCR-2
        DO 80 B=1,mNumBldg
          DO 80 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            FAN=RTFFAN(RECCL)
            F  =RTFUEL(RECCL)
            !S IS USED IN THE EXSQFTADJ CALCULATION AND DIFFERS DEPENDING ON
            !  WHETHER THE FUEL FOR HEATING IS ELECTRIC (2) OR FOSSIL (1)
            !  SEE RSMISC.TXT FOR "ELASTIC" INPUTS THAT MODIFY THE SQFT ADJUSTMENTS
            !  BY TYPE OF FUEL (THERE ARE ALSO SEPARATE ADJUSTMENTS FOR
            !  CENTRAL AC (3), HP COOLING (4) AND FURNACE FANS (5)
            IF (F.EQ.4) S=2
            IF (F.NE.4) S=1
            ! RTBASEFF(CurCalYr,RECCL) IS PROJECTED STOCK EFFICIENCY OF THE SURVIVING RECSYear STOCK
            ! READ FROM RSSTKEFF.TXT WHICH WAS DEVELOPED BY VINTAGING PROGRAM

            !  THERE ARE 2 SETS OF UECS DEVELOPED BELOW:
            !   THE "I" IN MIDDLE OF THE UECS NAMES DENOTE INTENSITIES FOR THE RESIDENTIAL EFFICIENCY INDEX IN FTAB,
            !   LEAVING OFF SQUARE FOOTAGE ADJUSTMENT (INCREASE) TO RECOGNIZE THAT GREATER USAGE FOR EXPANDED
            !   LIVING SPACE IS NOT AN EFFICIENCY LOSS, BUT INSTEAD SERVES INCREASED SERVICE DEMAND

            ! EQCSUEC APPLIES TO THE SURVIVING RECSYear STOCK
            EQCSUEC(CurCalYr,RECCL,B,R) =EQCUEC(R,RECCL,B)*RTBASEFF(RECSYear,RECCL)/RTBASEFF(CurCalYr,RECCL) &
              *EXSQFTADJ(CurCalYr,B,R,S)
            EQCSIUEC(CurCalYr,RECCL,B,R)=EQCUEC(R,RECCL,B)*RTBASEFF(RECSYear,RECCL)/RTBASEFF(CurCalYr,RECCL)

            ! EQCNUEC APPLIES TO NEW REPLACEMENT EQUIPMENT IN NEW CONSTRUCTION (I.E. ADDED AFTER THE RECSYear)
            IF (WTEQCEFFN(CurCalYr,RECCL,B,R).GT.0.0) THEN
             EQCNUEC(CurCalYr,RECCL,B,R) =EQCUEC(R,RECCL,B)*WTEQCEFFN(CurCalYr,RECCL,B,R)*RTBASEFF(RECSYear,RECCL)
             EQCNIUEC(CurCalYr,RECCL,B,R)=EQCUEC(R,RECCL,B)*WTEQCEFFN(CurCalYr,RECCL,B,R)*RTBASEFF(RECSYear,RECCL)
             ELSE
              EQCNUEC(CurCalYr,RECCL,B,R) =EQCUEC(R,RECCL,B)
              EQCNIUEC(CurCalYr,RECCL,B,R)=EQCUEC(R,RECCL,B)
            ENDIF

            ! EQCHVUEC APPLIES TO NEW CONSTRUCTION THIS YEAR
            IF (WTEQCEFFHV(CurCalYr,RECCL,B,R).GT.0.0) THEN
              EQCHVUEC(CurCalYr,RECCL,B,R) =EQCUEC(R,RECCL,B)*WTEQCEFFHV(CurCalYr,RECCL,B,R)*RTBASEFF(RECSYear,RECCL)*WTEQCSQFHV(CurCalYr,RECCL,B,R)
              EQCHVIUEC(CurCalYr,RECCL,B,R)=EQCUEC(R,RECCL,B)*WTEQCEFFHV(CurCalYr,RECCL,B,R)*RTBASEFF(RECSYear,RECCL)
             ELSE
              EQCHVUEC(CurCalYr,RECCL,B,R)=EQCUEC(R,RECCL,B)*WTEQCSQFHV(CurCalYr,RECCL,B,R)
              EQCHVIUEC(CurCalYr,RECCL,B,R)=EQCUEC(R,RECCL,B)    !INTENSITY UEC FOR INDEX
            ENDIF

            ! EQCRUEC APPLIES TO REPLACEMENTS OF EQUIPMENT THIS YEAR FROM HOUSING EXISTING IN RECSYear
            IF (WTEQCEFFR(CurCalYr,RECCL,B,R) .GT. 0.0) THEN
              EQCRUEC(CurCalYr,RECCL,B,R) =EQCUEC(R,RECCL,B)*WTEQCEFFR(CurCalYr,RECCL,B,R)*RTBASEFF(RECSYear,RECCL)*EXSQFTADJ(CurCalYr,B,R,S)
              EQCRIUEC(CurCalYr,RECCL,B,R)=EQCUEC(R,RECCL,B)*WTEQCEFFR(CurCalYr,RECCL,B,R)*RTBASEFF(RECSYear,RECCL)
             ELSE
              EQCRUEC(CurCalYr,RECCL,B,R) =EQCUEC(R,RECCL,B)*EXSQFTADJ(CurCalYr,B,R,S)
              EQCRIUEC(CurCalYr,RECCL,B,R)=EQCUEC(R,RECCL,B)
            ENDIF

            ! EQCAUEC IS THE AVERAGE UEC FOR EQUIPMENT IN RECSYear HOUSING STOCK THAT HAS BEEN REPLACED (ONCE)
            !   AND THAT BOTH THE EQUIPMENT AND HOUSE HAVE SURVIVED TO THIS YEAR.
            ! EQCAHVUEC IS THE AVERAGE UEC FOR EQUIPMENT IN POST-RECSYear HOUSINGS STOCK.
            IF (CurCalYr-1.EQ.RECSYear)THEN
                 EQCAUEC(CurCalYr,RECCL,B,R)=EQCNUEC(CurCalYr,RECCL,B,R)
                 EQCAHVUEC(CurCalYr,RECCL,B,R)=EQCHVUEC(CurCalYr,RECCL,B,R)
                 AFANUEC(RECSYear+1,R,B)=NFANUEC(RECSYear+1,R,B)
                ELSE
                 ! SUM ALL OF THE SURVIVING / VINTAGED EQUIPMENT FROM RECSYear TO YEAR PRIOR TO THIS YEAR
                 !  (EQUIPMENT STOCK NAMES WITH "FUT" APPENDED FOR "SURVIVING IN A FUTURE YEAR")
                 TEMP=0.0
                 TEMP1=0.0
                 TEMP2=0.0
                 DO Y=RECSYear,CurCalYr-1
                  TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,R)+ &
                   EQR90RPFUT(Y,CurCalYr,RECCL,B,R)
                  TEMP1=TEMP1+(EQR90FUT(Y,CurCalYr,RECCL,B,R)+ &
                   EQADDFUT(Y,CurCalYr,RECCL,B,R)+EQREPFUT(Y,CurCalYr,RECCL,B,R)+ &
                   EQR90RPFUT(Y,CurCalYr,RECCL,B,R))*FAN
                  TEMP2=TEMP2+ &
                   EQADDFUT(Y,CurCalYr,RECCL,B,R)+EQREPFUT(Y,CurCalYr,RECCL,B,R)
                 ENDDO

                IF(TEMP.LE.0.0) THEN
                  EQCAUEC(CurCalYr,RECCL,B,R)=EQCNUEC(CurCalYr,RECCL,B,R)
                 ELSE
                  EQCAUEC(CurCalYr,RECCL,B,R)=0.0
                  DO Y=RECSYear,CurCalYr-1
                   EQCAUEC(CurCalYr,RECCL,B,R)=EQCAUEC(CurCalYr,RECCL,B,R)+( &
                    EQR90FUT(Y,CurCalYr,RECCL,B,R)*EQCRUEC(Y,RECCL,B,R)+ &
                    EQR90RPFUT(Y,CurCalYr,RECCL,B,R)*EQCNIUEC(Y,RECCL,B,R)) &
                    /TEMP
                  ENDDO
                ENDIF

             IF(TEMP1.LE.0.0) THEN
               AFANUEC(CurCalYr,R,B)=NFANUEC(CurCalYr,R,B)
              ELSE
               AFANUEC(CurCalYr,R,B)=0.0
               DO Y=RECSYear,CurCalYr-1
                AFANUEC(CurCalYr,R,B)=AFANUEC(CurCalYr,R,B)+( &
                 EQR90FUT(Y,CurCalYr,RECCL,B,R)*EFANUEC(Y,R,B)*FAN+ &
                 EQADDFUT(Y,CurCalYr,RECCL,B,R)*NFANUEC(Y,R,B)*FAN+&
                 EQREPFUT(Y,CurCalYr,RECCL,B,R)*NFANUEC(Y,R,B)*FAN+ &
                 EQR90RPFUT(Y,CurCalYr,RECCL,B,R)*NFANUEC(Y,R,B)*FAN) &   !Change from EUEC to NUEC in fan calculation
                 /TEMP1
               ENDDO
              ENDIF !TEMP1 <=0.

              IF(TEMP2.LE.0.0) THEN
                EQCAHVUEC(CurCalYr,RECCL,B,R)=EQCHVUEC(CurCalYr,RECCL,B,R)
               ELSE
               EQCAHVUEC(CurCalYr,RECCL,B,R)=0.0
               DO Y=RECSYear,CurCalYr-1
                 EQCAHVUEC(CurCalYr,RECCL,B,R)=EQCAHVUEC(CurCalYr,RECCL,B,R)+( &
                  EQADDFUT(Y,CurCalYr,RECCL,B,R)*EQCHVUEC(Y,RECCL,B,R)+ &
                  EQREPFUT(Y,CurCalYr,RECCL,B,R)*EQCNUEC(Y,RECCL,B,R)) &
                  /TEMP2
               ENDDO
              ENDIF !TEMP2 <=0.
            ENDIF  !CurCalYr = RECSYear

 80     CONTINUE

!*******************************************************************
!  CALCULATE AVERAGE EFFICIENCY
!*******************************************************************
      DO 85 R=1,mNumCR-2
        DO 85 B=1,mNumBldg
          DO 85 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            FAN=RTFFAN(RECCL)
            IF (CurCalYr.EQ.RECSYear+1) THEN
              WTEQCEFFA(RECSYear+1,RECCL,B,R)=WTEQCEFFN(RECSYear+1,RECCL,B,R)
            ELSE
              TEMP=0.0
              DO Y=RECSYear,CurCalYr-1
                TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,R)+ &
                EQADDFUT(Y,CurCalYr,RECCL,B,R)+EQREPFUT(Y,CurCalYr,RECCL,B,R)+ &
                EQR90RPFUT(Y,CurCalYr,RECCL,B,R)
              ENDDO
              IF(TEMP.GT. 0.0) THEN
                WTEQCEFFA(CurCalYr,RECCL,B,R)=0.0
              DO Y=RECSYear,CurCalYr-1
                WTEQCEFFA(CurCalYr,RECCL,B,R)=WTEQCEFFA(CurCalYr,RECCL,B,R)+ &
                 ((EQR90FUT(Y,CurCalYr,RECCL,B,R)*WTEQCEFFR(Y,RECCL,B,R))+ &
                 ((EQADDFUT(Y,CurCalYr,RECCL,B,R)*WTEQCEFFHV(Y,RECCL,B,R))+ &
                 (EQREPFUT(Y,CurCalYr,RECCL,B,R)+ &
                 EQR90RPFUT(Y,CurCalYr,RECCL,B,R))*WTEQCEFFN(Y,RECCL,B,R)))/TEMP
              ENDDO
              ELSE
                WTEQCEFFA(CurCalYr,RECCL,B,R)=WTEQCEFFN(CurCalYr,RECCL,B,R)
              ENDIF
            ENDIF
 85   CONTINUE

!*******************************************************************
!  INITIALIZE HEATING CONSUMPTION
!*******************************************************************
      DO 90 R=1,mNumCR-2
        DO 90 FCON=1,NHTRFL
          !FOR NEMS REPORTING
          HTRCON(CurIYr,FCON,R)=0.0
          FANCON(CurIYr,R)=0.0
         DO 90 B=1,mNumBldg
          !FOR FTAB EFFICIENCY CALCULATION
          HTRCONWT(CurIYr,FCON,R,B)=0.
          HTRCONIN(CurIYr,FCON,R,B)=0.
          Driver(CurIYr,FCON,R,B)=0.
          FANCONWT(CurIYr,R,B)=0.
          FANCONIN(CurIYr,R,B)=0.
          Driver2(CurIYr,R,B)=0.
 90   CONTINUE

!*******************************************************************
!  CALCULATE HEATING CONSUMPTION
!*******************************************************************
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)

      ! FIND INDICES FOR THE ELECTRIC AND GEOTHERMAL HEAT PUMPS
        IF(RTCLNAME(RECCL).EQ.'ELEC_HP')THEN
          EQCEHP=RTCLEQCL(RECCL)
          RECCLEHP=EQCEHP
        ELSEIF(RTCLNAME(RECCL).EQ.'GEO_HP')THEN
          EQCGHP=RTCLEQCL(RECCL)
          RECCLGHP=EQCGHP
        ENDIF
      ENDDO

      DO 100 R=1,mNumCR-2
        DO 100 B=1,mNumBldg
!bookmark MELS
!  this statement may be causing a problem as it zeros out RECS year faneqcn!
!         IF (CurCalYr.EQ.(RECSYear+1)) FANEQCN(CurIYr-1,1,B,R)=0.0
         FANEQCN(CurIYr,1,B,R)=0.0
         FANEQP(CurCalYr,B,R)=0.0
         !   F = FUEL NUMBER FROM RSCLASS FILE, F=1,2,3,4,5,6(WOOD),7(GEO)
         !   FHTRCON(F) = HTRCON FUEL NUMBER, EXCEPT FOR GEOTHERMAL WHICH IS 4 AND 7
         !   MAP RSCLASS FUEL NUMBERS INTO FHTRCON FUEL NUMBERS
         !                  FHTRCON   RSCLASS
         !    FUEL            FCON       F
         !    NATURAL GAS       1        3
         !    ELECTRICITY       2        4
         !    DFO+KEROSENE      3        1 (DFO=Distillate Fuel Oil)
         !    PROPANE           4        2
         !    KEROSENE          5        5 (Combined with distillate fuel oil in AEO2019)	!kj
         !    WOOD              6        1 (Priced to distillate fuel oil)
         !    GEOTHERMAL        7        4 (Priced to electricity)

          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            F  =RTFUEL(RECCL)
            FAN=RTFFAN(RECCL)
            IF (F.EQ.4) S=2
            IF (F.NE.4) S=1
            !MAP RTEK FUEL NUMBERS INTO NEMS FUEL NUMBERS
            FCON=FHTRCON(F)

            !Special code for ARRA stimulus bill
            IF ((CurCalYr.GT.2010).AND.(STIMULUS.EQ.1).AND.(F.EQ.4)) THEN
               ALPHA=-0.30
             ELSE
               ALPHA=-0.15
            ENDIF

            !Code to streamline wood treatment
            F2=F
            IF(F.EQ.6) ALPHA=0.50
            IF(F.EQ.6) F2=1

            !Efficiency Rebound Effects
            IF(CurCalYr.GT.RECSYear+1) THEN
               RBA=(RTBASEFF(RECSYear,RECCL)*WTEQCEFFA(CurCalYr,RECCL,B,R))**ALPHA
               RBR=(RTBASEFF(RECSYear,RECCL)*WTEQCEFFR(CurCalYr,RECCL,B,R))**ALPHA
               RBN=(RTBASEFF(RECSYear,RECCL)*WTEQCEFFN(CurCalYr,RECCL,B,R))**ALPHA
              ELSE
               RBA=1.0
               RBR=1.0
               RBN=1.0
             ENDIF

             !CONSUMPTION FOR NEMS OUTPUT TABLES
             HTRCON(CurIYr,FCON,R)=HTRCON(CurIYr,FCON,R)+HDDFACT(R)*LEAPYR*(( &
               (EQCESE(CurCalYr,RECCL,B,R)*EQCSUEC(CurCalYr,RECCL,B,R)* &
                 (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2)) + &
               (EQCADD(CurCalYr,RECCL,B,R)*EQCHVUEC(CurCalYr,RECCL,B,R) * &
                 (NHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBN + &
               (EQCRP90(CurCalYr,RECCL,B,R)*EQCRUEC(CurCalYr,RECCL,B,R)* &
                 (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBR + &
               (EQCRP90RP(CurCalYr,RECCL,B,R)*EQCNIUEC(CurCalYr,RECCL,B,R)*EXSQFTADJ(CurCalYr,B,R,S)* &
                 (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBN + &
               (EQCSR90(CurCalYr,RECCL,B,R)*EQCAUEC(CurCalYr,RECCL,B,R)* &
                 (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBA + &
               (EQCREP(CurCalYr,RECCL,B,R) *EQCNUEC(CurCalYr,RECCL,B,R)* &
                 (AHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBN + &
               (EQCSUR(CurCalYr,RECCL,B,R) *EQCAHVUEC(CurCalYr,RECCL,B,R) * &
                 (AHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBA)*( &
                RSELAST(F2,R,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)))

             !WEIGHT, INTENSITY AND "DRIVER" VARIABLES FOR THE FTAB EFFICIENCY CALCULATION
             HTRCONWT(CurIYr,FCON,R,B)=HTRCONWT(CurIYr,FCON,R,B)+HDDFACT(R)*LEAPYR*(( &
               (EQCESE(CurCalYr,RECCL,B,R)*EQCSUEC(CurCalYr,RECCL,B,R)* &
                 (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2)) + &
               (EQCADD(CurCalYr,RECCL,B,R)*EQCHVUEC(CurCalYr,RECCL,B,R)* &
                 (NHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBN + &
               (EQCRP90(CurCalYr,RECCL,B,R)*EQCRUEC(CurCalYr,RECCL,B,R)* &
                 (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBR + &
               (EQCRP90RP(CurCalYr,RECCL,B,R)*EQCNIUEC(CurCalYr,RECCL,B,R)*EXSQFTADJ(CurCalYr,B,R,S)* &
                 (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBN + &
               (EQCSR90(CurCalYr,RECCL,B,R)*EQCAUEC(CurCalYr,RECCL,B,R)* &
                 (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBA + &
               (EQCREP(CurCalYr,RECCL,B,R) *EQCNUEC(CurCalYr,RECCL,B,R)* &
                 (AHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBN + &
               (EQCSUR(CurCalYr,RECCL,B,R) *EQCAHVUEC(CurCalYr,RECCL,B,R)* &
                 (AHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBA)*( &
                RSELAST(F2,R,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)))

             HTRCONIN(CurIYr,FCON,R,B)=HTRCONIN(CurIYr,FCON,R,B)+((( &
               (EQCESE(CurCalYr,RECCL,B,R)*EQCSIUEC(CurCalYr,RECCL,B,R)* &
                 (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))) + &
               (EQCADD(CurCalYr,RECCL,B,R)*EQCHVIUEC(CurCalYr,RECCL,B,R)* &
                 (NHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))) + &
               (EQCRP90(CurCalYr,RECCL,B,R)*EQCRIUEC(CurCalYr,RECCL,B,R)* &
                 (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))) + &
               (EQCRP90RP(CurCalYr,RECCL,B,R)*EQCNIUEC(CurCalYr,RECCL,B,R)* &
                 (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))) + &
               (EQCSR90(CurCalYr,RECCL,B,R)*EQCAUEC(CurCalYr,RECCL,B,R)* &
                 (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))) + &
               (EQCREP(CurCalYr,RECCL,B,R) *EQCNIUEC(CurCalYr,RECCL,B,R)* &
                 (AHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))) + &
               (EQCSUR(CurCalYr,RECCL,B,R) *EQCAHVUEC(CurCalYr,RECCL,B,R)* &
                 (AHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B)))))           )

               Driver(CurIYr,FCON,R,B)=Driver(CurIYr,FCON,R,B)+ &
                  (EQCESE(CurCalYr,RECCL,B,R)+EQCADD(CurCalYr,RECCL,B,R)+ &
                   EQCRP90RP(CurCalYr,RECCL,B,R)+EQCRP90(CurCalYr,RECCL,B,R)+ &
                   EQCSR90(CurCalYr,RECCL,B,R)+EQCREP(CurCalYr,RECCL,B,R)+ &
                   EQCSUR(CurCalYr,RECCL,B,R))

            !CALCULATION FOR EQUIPMENT-SPECIFIC ENERGY CONSUMPTION DATABASE
             EQCEQCN(CurIYr,RECCL,B,R)= HDDFACT(R)*LEAPYR*(( &
               (EQCESE(CurCalYr,RECCL,B,R)*EQCSUEC(CurCalYr,RECCL,B,R)* &
               (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))    + &
               (EQCADD(CurCalYr,RECCL,B,R)*EQCHVUEC(CurCalYr,RECCL,B,R)* &
                 (NHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBN + &
               (EQCRP90(CurCalYr,RECCL,B,R)*EQCRUEC(CurCalYr,RECCL,B,R)* &
                 (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBR + &
               (EQCRP90RP(CurCalYr,RECCL,B,R)*EQCNIUEC(CurCalYr,RECCL,B,R)*EXSQFTADJ(CurCalYr,B,R,S)* &
                 (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBN + &
               (EQCSR90(CurCalYr,RECCL,B,R)*EQCAUEC(CurCalYr,RECCL,B,R)* &
                 (EHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBA + &
               (EQCREP(CurCalYr,RECCL,B,R)*EQCNUEC(CurCalYr,RECCL,B,R)* &
                 (AHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBN + &
               (EQCSUR(CurCalYr,RECCL,B,R)*EQCAHVUEC(CurCalYr,RECCL,B,R)* &
                 (AHSHELL(CurCalYr,F,R,B)/EHSHELL(RECSYear,F,R,B))**(1.0+ALPHA2))*RBA)* &
                  RSELAST(F2,R,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

             IF(F.NE.6) THEN   !ASSUMING NO FANS OR FAN CONSUMPTION FOR WOOD...  Moved HDDFACT to here instead of UEC calc
               FANEQCN(CurIYr,1,B,R)= FANEQCN(CurIYr,1,B,R)+HDDFACT(R)*LEAPYR*( &
                 EQCESE(CurCalYr,RECCL,B,R)*FAN*EFANUEC(CurCalYr,R,B)+ &
                (EQCRP90(CurCalYr,RECCL,B,R)+ EQCRP90RP(CurCalYr,RECCL,B,R))* &
                   FAN*NFANUEC(CurCalYr,R,B)+ &                             !Changed EQCRP, EQCRP90RP to calculation with new fan UEC
                (EQCSR90(CurCalYr,RECCL,B,R))*FAN*AFANUEC(CurCalYr,R,B)+ &
                (EQCADD(CurCalYr,RECCL,B,R)+EQCREP(CurCalYr,RECCL,B,R))* &  !Moved EQCREP to calculation with new fan UEC
                   FAN*NFANUEC(CurCalYr,R,B) + &
                 EQCSUR(CurCalYr,RECCL,B,R)*FAN*AFANUEC(CurCalYr,R,B))* &
                 RSELAST(F,R,ALPHA,EF1,EF2,EF3,RECSYear,EUPR) ! CHANGED FROM F=3

               FANCON(CurIYr,R)=FANCON(CurIYr,R)+ HDDFACT(R)*LEAPYR* &
                (EQCESE(CurCalYr,RECCL,B,R)*EFANUEC(CurCalYr,R,B)*FAN+ &
                (EQCRP90(CurCalYr,RECCL,B,R)+EQCRP90RP(CurCalYr,RECCL,B,R))* &
                   FAN*NFANUEC(CurCalYr,R,B)+ &                             !Changed EQCRP, EQCRP90RP to calc with new fan UEC
                (EQCSR90(CurCalYr,RECCL,B,R))*FAN*AFANUEC(CurCalYr,R,B)+ &
                (EQCADD(CurCalYr,RECCL,B,R)+EQCREP(CurCalYr,RECCL,B,R))* &  !Moved EQCREP to calc with new fan UEC
                   FAN*NFANUEC(CurCalYr,R,B) + &
                 EQCSUR(CurCalYr,RECCL,B,R)*FAN*AFANUEC(CurCalYr,R,B))* &
                 RSELAST(F,R,ALPHA,EF1,EF2,EF3,RECSYear,EUPR) ! CHANGED FROM F=3

               FANCONWT(CurIYr,R,B)= FANCONWT(CurIYr,R,B)+HDDFACT(R)*LEAPYR*( &
                 EQCESE(CurCalYr,RECCL,B,R)*EFANUEC(CurCalYr,R,B)*FAN+ &
                (EQCRP90(CurCalYr,RECCL,B,R)+EQCRP90RP(CurCalYr,RECCL,B,R))* &
                     FAN*NFANUEC(CurCalYr,R,B)+ &                           !Changed EQCRP, EQCRP90RP to calc with new fan UEC
                (EQCSR90(CurCalYr,RECCL,B,R))*FAN*AFANUEC(CurCalYr,R,B)+ &
                (EQCADD(CurCalYr,RECCL,B,R)+EQCREP(CurCalYr,RECCL,B,R))* &  !Moved EQCREP to calc with new fan UEC
                     FAN*NFANUEC(CurCalYr,R,B) + &
                (+EQCSUR(CurCalYr,RECCL,B,R))*FAN*AFANUEC(CurCalYr,R,B))* &
                 RSELAST(F,R,ALPHA,EF1,EF2,EF3,RECSYear,EUPR) ! CHANGED FROM F=3

               FANEQP(CurCalYr,B,R)=FANEQP(CurCalYr,B,R)+ &
                (EQCESE(CurCalYr,RECCL,B,R)+EQCRP90(CurCalYr,RECCL,B,R)+ &
                 EQCSR90(CurCalYr,RECCL,B,R)+EQCSUR(CurCalYr,RECCL,B,R)+ &
                 EQCRP90RP(CurCalYr,RECCL,B,R)+EQCREP(CurCalYr,RECCL,B,R)+& !Added EQCREP
                  EQCADD(CurCalYr,RECCL,B,R))*FAN

               FANCONIN(CurIYr,R,B)=FANCONIN(CurIYr,R,B)+((&
                (EQCESE(CurCalYr,RECCL,B,R))*FAN*FANIUEC(R,B)+ &
                (EQCADD(CurCalYr,RECCL,B,R)+EQCREP(CurCalYr,RECCL,B,R)+ &
                  EQCRP90(CurCalYr,RECCL,B,R)+EQCRP90RP(CurCalYr,RECCL,B,R))* &
                  FAN*NFANIUEC(CurCalYr,R,B) + &
                (EQCSUR(CurCalYr,RECCL,B,R)+EQCSR90(CurCalYr,RECCL,B,R))* &
                 FAN*AFANUEC(CurCalYr,R,B)) )

               Driver2(CurIYr,R,B)=Driver2(CurIYr,R,B)+ &
                EQCESE(CurCalYr,RECCL,B,R)+EQCADD(CurCalYr,RECCL,B,R)+ &
                EQCRP90RP(CurCalYr,RECCL,B,R)+EQCRP90(CurCalYr,RECCL,B,R)+ &
                EQCSR90(CurCalYr,RECCL,B,R)+EQCREP(CurCalYr,RECCL,B,R)+ &
                EQCSUR(CurCalYr,RECCL,B,R)
             ENDIF ! F.NE.6 NO FAN CONSUMPTION FOR WOOD

          ENDDO

!GEOTHERMAL IS CALCULATED DIFFERENTLY FROM OTHER FUELS
! Set FCON=7 for geothermal and calculate outside of the RSCLASS loop because it is not encountered in RSCLASS.
! UEC of ground-source heat pump (RECCL=15) is subtracted from air-source heat pump (RECCL=14) to differentiate amount of electricity used versus geothermal energy	!kj
          FCON = FHTRCON(7)  ! FCON = 7
          HTRCON(CurIYr,FCON,R)=HTRCON(CurIYr,FCON,R)+HDDFACT(R)*LEAPYR* &
           (EQCESE(CurCalYr,RECCLGHP,B,R)* &
           (EQCUEC(R,RECCLEHP,B)-EQCUEC(R,RECCLGHP,B))*WHRFOSS(R,CurIYr)/3412.+ &  !STEOhr
            EQCRP90(CurCalYr,RECCLGHP,B,R)* &
           (EQCRUEC(CurCalYr,RECCLEHP,B,R)-EQCRUEC(CurCalYr,RECCLGHP,B,R))*WHRFOSS(R,CurIYr)/3412.+ &  !STEOhr
            EQCADD(CurCalYr,RECCLGHP,B,R)*&
           (EQCHVUEC(CurCalYr,RECCLEHP,B,R)-EQCHVUEC(CurCalYr,RECCLGHP,B,R))*WHRFOSS(R,CurIYr)/3412.+ &  !STEOhr
            EQCSUR(CurCalYr,RECCLGHP,B,R)*&
           (EQCAHVUEC(CurCalYr,RECCLEHP,B,R)-EQCAHVUEC(CurCalYr,RECCLGHP,B,R))*WHRFOSS(R,CurIYr)/3412.+ &  !STEOhr
           (EQCREP(CurCalYr,RECCLGHP,B,R)+EQCRP90RP(CurCalYr,RECCLGHP,B,R))* &
           (EQCNUEC(CurCalYr,RECCLEHP,B,R)-EQCNUEC(CurCalYr,RECCLGHP,B,R))*WHRFOSS(R,CurIYr)/3412.+ &  !STEOhr
           (EQCSUR(CurCalYr,RECCLGHP,B,R))* &
           (EQCAUEC(CurCalYr,RECCLEHP,B,R)-EQCAUEC(CurCalYr,RECCLGHP,B,R))*WHRFOSS(R,CurIYr)/3412.)  !STEOhr

          GEEQCN(CurIYr,1,B,R)= HDDFACT(R)*LEAPYR* &
           (EQCESE(CurCalYr,RECCLGHP,B,R)* &
           (EQCUEC(R,RECCLEHP,B)-EQCUEC(R,RECCLGHP,B))*WHRFOSS(R,CurIYr)/3412.+ &  !STEOhr
           EQCRP90(CurCalYr,RECCLGHP,B,R)* &
           (EQCRUEC(CurCalYr,RECCLEHP,B,R)-EQCRUEC(CurCalYr,RECCLGHP,B,R))*WHRFOSS(R,CurIYr)/3412.+ &  !STEOhr
           EQCADD(CurCalYr,RECCLGHP,B,R)*&
           (EQCHVUEC(CurCalYr,RECCLEHP,B,R)-EQCHVUEC(CurCalYr,RECCLGHP,B,R))*WHRFOSS(R,CurIYr)/3412.+ &  !STEOhr
           EQCSUR(CurCalYr,RECCLGHP,B,R)*&
           (EQCAHVUEC(CurCalYr,RECCLEHP,B,R)-EQCAHVUEC(CurCalYr,RECCLGHP,B,R))*WHRFOSS(R,CurIYr)/3412.+ &  !STEOhr
           (EQCREP(CurCalYr,RECCLGHP,B,R)+EQCRP90RP(CurCalYr,RECCLGHP,B,R))* &
           (EQCNUEC(CurCalYr,RECCLEHP,B,R)-EQCNUEC(CurCalYr,RECCLGHP,B,R))*WHRFOSS(R,CurIYr)/3412.+ &  !STEOhr
           EQCSR90(CurCalYr,RECCLGHP,B,R)* &
           (EQCAUEC(CurCalYr,RECCLEHP,B,R)-EQCAUEC(CurCalYr,RECCLGHP,B,R))*WHRFOSS(R,CurIYr)/3412.)  !STEOhr
 100  CONTINUE

      ! CALCULATION OF INTENSITY VARIABLE FOR FTAB, ADJUSTING FOR DRIVER IN DENOMINATOR
      DO R=1,mNumCR-2
        DO FCON=1,NHTRFL
         DO B=1,mNumBldg
          IF (Driver(CurIYr,FCON,R,B).GT.0) &
           HTRCONIN(CurIYr,FCON,R,B)= &
           HTRCONIN(CurIYr,FCON,R,B)/Driver(CurIYr,FCON,R,B)
          IF (Driver2(CurIYr,R,B).GT.0) &
           FANCONIN(CurIYr,R,B)= &
           FANCONIN(CurIYr,R,B)/Driver2(CurIYr,R,B)
         ENDDO
        ENDDO
      ENDDO

END SUBROUTINE RHTRCON


!******************************************************************
!     ROOM AC, CENTRAL AC, AND HEAT PUMP COOLING CHOICE SUBROUTINE
!*******************************************************************
      SUBROUTINE RCLTEC
      IMPLICIT NONE
      REAL*4 TOTEWTN(nCoolClasses,mNumBldg,mNumCR-2),TOTEWTR(nCoolClasses,mNumBldg,mNumCR-2)  !EqpParam
      REAL*4 OPCOST(2),CDDFACT(mNumCR-2)
      REAL*4 EQWTN(nCoolTypes,mNumBldg,mNumCR),EQWTR(nCoolTypes,mNumBldg,mNumCR)
      REAL*4 RTEFFAC(2),DECAY,ECTEMP,DENOM,SUM,DENOM2
      REAL*4 EQCOST,CAPITAL,RETAIL
      INTEGER EU,EUPR,EUHT,RECTY,RECTYHT,RECCL,R,B,F,EQT,EQC,TYPE, &
        TYPEHT,RECCLHHP,COUNT,CNT,L,IND
!      INTEGER RECAR(nCoolTypes),EQTAR(nCoolTypes)  !EqpParam	!kj - revert back to this?
      INTEGER RECAR(nHeatTypes),EQTAR(nHeatTypes)  !EqpParam	!kj - doing this to use same values as heating, which seems to work correctly

!*******************************************************************
!   THE GENERAL FORM OF THIS SUBROUTINE APPLIES TO ALL END USES
!     SET EU = 2 TO SEARCH THE SPACE COOLING SECTION OF THE DATA
!*******************************************************************
      EU=2
      EUPR=2

!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,mNumCR-2
        PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
      ENDDO

!   SET CDDFACT
      DO R=1,mNumCR-2
          CDDFACT(R)=(CDDADJ(CurCalYr,R)/CDDADJ(RECSYear,R))**1.50 !A 10% increase in CDD would increase space cooling consumption by 15% (e.g., 1.10^1.50=1.15)
      ENDDO

      ALPHA1=-0.50

!     COMPUTE DECAY RATE USED TO COMPUTE LIFE CYCLE COST (FIRST ITERATION ONLY)
      IF(CURITR.EQ.1) THEN
        DECAY = (1-((1+ResDiscountRate)**(-Tenure)))/ResDiscountRate
      ENDIF

!   OUTTER LOOPS ARE CENSUS DIVISION AND BUILDING TYPE
      DO 100 R=1,mNumCR-2
        DO 100 B=1,mNumBldg
!*******************************************************************
!    RECCL          = RECORD NUMBER IN RSCLASS
!    RTCLEUPT(EU)   = LAST RECORD # IN SPACE HEATING (EU=1)
!    RTCLEUPT(EU+1) = LAST RECORD # IN SPACE COOLING (EU=2)
!    EQC            = EQUIPMENT CLASS # FOR COOLING
!*******************************************************************

!    ZERO OUT ARRAYS
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC   =RTCLEQCL(RECCL)
            TOTEWTN(EQC,B,R)=0.0
            TOTEWTR(EQC,B,R)=0.0
          ENDDO

!*******************************************************************
!    RECTY          = RECORD NUMBER IN RSMEQP
!    RTTYEUPT(EU)   = LAST RECORD # IN SPACE HEATING (EU=1)
!    RTTYEUPT(EU+1) = LAST RECORD # IN SPACE COOLING (EU=2)
!*******************************************************************
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)

!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
            IF(CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN

!      FIND VALID INDICES FOR EQUIPMENT CLASS (EQC), EQUIPMENT
!        TYPE (EQT), REC # FOR RECTY FILE (RECCL), AND FUEL TYPE (F)
              EQC=RTTYEQCL(RECTY)
              EQT=RTEQTYPE(RECTY)
              RECCL=RTCLEUPT(EU)+EQC

!    ONLY CONTINUE CALCULATIONS IF THIS IS NOT A HEAT PUMP (ground-source or air-source)
!      (HEAT PUMPS USE THE SAME DATA CALCULATED FOR HEATING)
!      (If RTTYPNTR is not zero, then it points back to the heating equipment type)
              IF(RTTYPNTR(RECTY).LE.0)THEN

!    F = FUEL NUMBER FOR THE CURRENT EQUIPMENT CLASS
                F = RTFUEL(RECCL)

!    COMPUTE EFFICIENCY FACTOR USED IN COMPUTING OPERATING COST
                IF(RTEQEFF(RECTY).NE.0.0) THEN
                  RTEFFAC(1)=EQCEFF(CurCalYr,RECCL)/RTEQEFF(RECTY)
                  RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)/RTEQEFF(RECTY)
                ELSE
                  RTEFFAC(1)=RTBASEFF(RECSYear,RECCL)
                  RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)
                ENDIF

!  If COSTTRSW = 1, use function EQCOST to compute capital
!     cost of new equipment.
!  If COSTTRSW = 0, use constant value from RSMEQP file for capital
!     cost of new equipment.
        IF (COSTTRSW.EQ.1) THEN
          CAPITAL = EQCOST(RECTY,CurCalYr,"CAP")
        ELSE
          CAPITAL = RTEQCOST(RECTY)
        ENDIF

!     CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOR CHANGES
      IF ((CurCalYr.GT.2008).AND. &	!kj - 2008 marks first year before American Clean Energy and Security Act of 2009 (Waxman-Markey bill)?
               (PRICES(F,R,CurCalYr).GT.PRICES(F,R,RECSYear))) THEN
        HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
        ELIGBLE=HRDRATE - 0.07
        IF (ELIGBLE.GT.0.0) THEN
           HRDADJ= ELIGBLE * &
            ((PRICES(F,R,CurCalYr)/PRICES(F,R,RECSYear))**ALPHA1 )
           BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
         ELSE
           BETA1DR(RECTY)=RTECBTA1(RECTY)
        ENDIF
       ELSE
        BETA1DR(RECTY)=RTECBTA1(RECTY)
      ENDIF

!     COMPUTE THE PART OF THE EQUIMENT CHOICE WEIGHT NOT DEPENDENT
!       ON REGION AND BUILDING TYPE (SAVES COMPUTATIONAL TIME)
                ECTEMP = RTECBIAS(RECTY)+ &
                   (BETA1DR(RECTY)*CAPITAL)

!      CALCULATE OPERATING COST
       IF (CurCalYr.EQ.RECSYear+1) THEN
            OPCOST(1)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B) &
               *RTEFFAC(1)*CDDFACT(R)
            OPCOST(2)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B) &
               *RTEFFAC(2)*CDDFACT(R)
       ELSE
            OPCOST(1)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B) &
               *RTEFFAC(1)*CDDFACT(R) &
                    *(ECSHELL(CurCalYr-1,R,B)/ECSHELL(RECSYear,R,B))
            OPCOST(2)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B) &
               *RTEFFAC(2)*CDDFACT(R) &
                    *(NCSHELL(CurCalYr-1,R,B)/ECSHELL(RECSYear,R,B))
       ENDIF

!      CALCULATE LIFE CYCLE COSTS
                LFCY(EQT,B,R,1)=CAPITAL + (OPCOST(1) *DECAY)
                LFCY(EQT,B,R,2)=CAPITAL + (OPCOST(2) *DECAY)

!    COMPUTE WEIGHTS FOR NEW AND REPLACEMENT EQUIPMENT TYPES
                EQWTN(EQT,B,R)= EXP(ECTEMP+(RTECBTA2(RECTY)*OPCOST(2)) + &
                     ( RTECBTA3(RECTY)*LFCY(EQT,B,R,2) ) )
                TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)
                EQWTR(EQT,B,R)= EXP(ECTEMP+(RTECBTA2(RECTY)*OPCOST(1)) + &
                     ( RTECBTA3(RECTY)*LFCY(EQT,B,R,1) ) )
                TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)

              ENDIF
            ENDIF
           ENDIF
          ENDDO

!*******************************************************************
          TYPE = RTTYPECT(EU)
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)

!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
            IF(CurCalYr.GE.RTINITYR(RECTY).AND. &
               CurCalYr.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              TYPE=TYPE+1

!   FIND VALID INDICES FOR EQUIPMENT CLASS (EQC), EQUIPMENT
!      TYPE (EQT), REC # IN THE RSCLASS FILE (RECCL)
              EQC=RTTYEQCL(RECTY)
              EQT=RTEQTYPE(RECTY)
              RECCL=RTCLEUPT(EU)+EQC

!  IF THIS IS A HEAT PUMP, ASSIGN CORRESPONDING HEATER SHARES TO
!    HEAT PUMP AIR CONDITIONERS
              IF(RTTYPNTR(RECTY).GT.0) THEN
                TYPEHT=RTTYPNTR(RECTY)
                NEQTSHR(CurCalYr,TYPE,B,R)=NEQTSHR(CurCalYr,TYPEHT,B,R)
                REQTSHR(CurCalYr,TYPE,B,R)=REQTSHR(CurCalYr,TYPEHT,B,R)

!   IF NOT A HEAT PUMP, COMPUTE SHARES
              ELSE
                IF (TOTEWTN(EQC,B,R).GT.0.0) THEN
                  NEQTSHR(CurCalYr,TYPE,B,R)=EQWTN(EQT,B,R)/ &
                                          TOTEWTN(EQC,B,R)
                ELSE
                  NEQTSHR(CurCalYr,TYPE,B,R)=0.0
                ENDIF

                IF (TOTEWTR(EQC,B,R).GT.0.0) THEN
                  REQTSHR(CurCalYr,TYPE,B,R)=EQWTR(EQT,B,R)/ &
                                          TOTEWTR(EQC,B,R)
                ELSE
                  REQTSHR(CurCalYr,TYPE,B,R)=0.0
                ENDIF
              ENDIF
            ENDIF
           ENDIF
          ENDDO

!*******************************************************************
!     CALCULATE WEIGHTED EFFICIENCY FOR NEW EQUIPMENT AND
!     CALCULATE WEIGHTED EFFICIENCY FOR REPLACEMENT EQUIPMENT
!*******************************************************************
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC   =RTCLEQCL(RECCL)
            DENOM =0
            DENOM2=0
            COUNT =0

            TYPE = RTTYPECT(EU)
            DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
              IF(CurCalYr.GE.RTINITYR(RECTY).AND. &
                 CurCalYr.LE.RTLASTYR(RECTY)) THEN
               IF (RTCENDIV(RECTY).EQ.R) THEN
                TYPE=TYPE+1
                IF(RTTYEQCL(RECTY).EQ.EQC) THEN
                  COUNT=COUNT+1
                  RECAR(COUNT)=RECTY
                  EQTAR(COUNT)=TYPE
                  DENOM=DENOM+NEQTSHR(CurCalYr,TYPE,B,R)
                  DENOM2=DENOM2+REQTSHR(CurCalYr,TYPE,B,R)
                ENDIF
              ENDIF
             ENDIF
            ENDDO

!    COMPLETE CALCULATION FOR NEW EQUIPMENT
            IF(DENOM.LE.0.0) THEN
              WTEQCEFFN(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                SUM=SUM+(NEQTSHR(CurCalYr,TYPE,B,R)/RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFN(CurCalYr,RECCL,B,R)=SUM/DENOM
            ENDIF
            IF (WTEQCEFFN(CurCalYr,RECCL,B,R).EQ.0.0) THEN
                WTEQCEFFN(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
            ENDIF

!     COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT
            IF(DENOM2.LE.0.0) THEN
              WTEQCEFFR(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                SUM=SUM+(REQTSHR(CurCalYr,TYPE,B,R)/RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFR(CurCalYr,RECCL,B,R)=SUM/DENOM2
            ENDIF
            IF (WTEQCEFFR(CurCalYr,RECCL,B,R).EQ.0.0) THEN
                WTEQCEFFR(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
            ENDIF
          ENDDO
100   CONTINUE
      END SUBROUTINE RCLTEC


!*******************************************************************
!     COOLING ADDED SUBROUTINE
!*******************************************************************
      SUBROUTINE RCLADD
      IMPLICIT NONE
      REAL*4 EQCPN90(RECSYear:EndYr,mNumRTCl,mNumBldg,mNumCR)
      REAL*4 SA(8,mNumBldg,mNumCR-2), HSR, ESR, SVRTE
      REAL*4 SUMHP,CACFACT,x
      REAL*4 RPTOT(mNumYr),EXTOT(mNumYr),ADDTOT(mNumYr),RPRPTOT(mNumYr),ANUMC(mNumCR-2,mNumBldg),ADENC(mNumCR-2,mNumBldg)
      REAL*4 NEWSHELLWTC(RECSYear:EndYr,mNumBldg,mNumCR),NEWADDWTC(RECSYear:EndYr,mNumBldg,mNumCR)
      INTEGER EQC,RECCL,RECCLHHP,RECCLCAC,RECCLEHP,EU
      INTEGER Y, R, B ,EV,TYPE,NUMEQT,EQT,RECTY,T,TEMP,V,E,D

!  SET EU = 2 TO SEARCH THE SPACE COOLING SECTION OF THE DATA
      EU      =  2
      EV      =  2
      CACFACT = .1

!   ZERO OUT ARRAYS
      DO 1 R=1,mNumCR-2
        DO 1 B=1,mNumBldg
         DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQC=RTCLEQCL(RECCL)
           EQCSR90(CurCalYr,RECCL,B,R)=0.0
           EQCSUR(CurCalYr,RECCL,B,R)=0.0
           SA(EQC,B,R)=0.0
           EQCRP90RP(CurCalYr,RECCL,B,R)=0.0
         ENDDO
1     CONTINUE

!*******************************************************************
!  Calculate CACs added in PREVYR (CurIYr-1)
!*******************************************************************
      DO 10 R=1,mNumCR-2
       DO 10 B=1,mNumBldg
         SUMHP=0.0
         DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQC=RTCLEQCL(RECCL)

!   RECCLCAC = EQUIPMENT CLASS NUMBER FOR CENTRAL AC
!   RECCLEHP = EQUIPMENT CLASS NUMBER FOR ELECTRIC HP
           IF(RTCLNAME(RECCL).EQ.'ROOM_AIR') THEN
             EQCADD(CurCalYr,RECCL,B,R)=(HSEADD(CurCalYr,B,R)*RACSAT(B,R)*RACUnits(B,R))
           ELSEIF(RTCLNAME(RECCL).EQ.'CENT_AIR') THEN
             RECCLCAC=EQC+RTCLEUPT(EU)
             EQCADD(CurCalYr,RECCL,B,R)=(HSEADD(CurCalYr,B,R)*CACSAT(B,R))
           ELSE

!   IF NOT ROOM_AIR OR CENT_AIR, MUST BE HEAT PUMP.
!   ADJUST FOR HEAT PUMPS CALCULATED IN HEATING SUBROUTINE
!   RECCLHHP = HEAT PUMP RECORD NUMBER FROM THE HEATING DATA
!   RECCLEHP = ELEC HEAT PUMP RECORD NUMBER FROM COOLING

             IF(RTCLNAME(RECCL).EQ.'ELEC_HP') THEN
               RECCLEHP=EQC+RTCLEUPT(EU)
             ENDIF
             RECCLHHP=RTCLPNTR(RECCL)
             EQCADD(CurCalYr,RECCL,B,R)=(EQCADD(CurCalYr,RECCLHHP,B,R))
             SUMHP=(SUMHP+EQCADD(CurCalYr,RECCL,B,R))
           ENDIF
         ENDDO

!  UPDATE CENTRAL AC ADDITIONS BASED ON TOTAL HP ADDITIONS

           IF(EQCADD(CurCalYr,RECCLCAC,B,R).LE.SUMHP)THEN
             EQCADD(CurCalYr,RECCLCAC,B,R)=(EQCADD(CurCalYr,RECCLEHP,B,R)*CACFACT)
          ELSE
             EQCADD(CurCalYr,RECCLCAC,B,R)=(EQCADD(CurCalYr,RECCLCAC,B,R)-SUMHP)
          ENDIF
 10   CONTINUE

!*******************************************************************
! CUMULATE SURVIVING EQUIPMENT REPLACED FOR RECS-YEAR VINTAGE PRIOR TO
!   PREVYR
!*******************************************************************
      DO 15 R=1,mNumCR-2
        DO 15 B=1,mNumBldg
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            IF(RTCLPNTR(RECCL).GT.0) THEN

!  HEAT PUMP SPACE COOLING USES HEAT PUMP SPACE HEATING DATA
              RECCLHHP=RTCLPNTR(RECCL)
              EQCSR90(CurCalYr,RECCL,B,R)=(EQCSR90(CurCalYr,RECCLHHP,B,R))
              EQCSUR(CurCalYr,RECCL,B,R)=(EQCSUR(CurCalYr,RECCLHHP,B,R))
            ELSE

!   CALCULATE DATA FOR NON-HEAT PUMPS
              IF (CurCalYr.EQ.RECSYear+1) THEN
                EQCND90(CurCalYr,RECCL,B,R)=(EQCESE(RECSYear,RECCL,B,R)*HDR(B))
              ELSE
                EQCND90(CurCalYr,RECCL,B,R)= &
                 ( EQCND90(CurCalYr-1,RECCL,B,R)*HDR(B))
              ENDIF

              IF ((B.EQ.1).AND.(RTCLNAME(RECCL).EQ.'CENT_AIR')) THEN
                IF ((EQCPN90(CurCalYr,RECCL,B,R)/EH(CurCalYr,B,R)).GE.0.90) THEN  !Max 90% central AC penetration into remaining RECSYear housing stock  !RSMISCpen
                  EQCPN90(CurCalYr,RECCL,B,R)=(EQCND90(CurCalYr,RECCL,B,R)- &  !RSMISCpen
                                               EQCND90(CurCalYr,RECCL,B,R))  !RSMISCpen
                ELSE  !RSMISCpen
                  EQCPN90(CurCalYr,RECCL,B,R)=(EQCND90(CurCalYr,RECCL,B,R)*(1.+CACPR(R))- &
                                               EQCND90(CurCalYr,RECCL,B,R))
                ENDIF !90% central AC penetration into remaining RECSYear housing stock  !RSMISCpen
              ELSE
                EQCPN90(CurCalYr,RECCL,B,R)=0.0
              ENDIF

      IF (CurCalYr.EQ.RECSYear+1) THEN
        EQCRP90(CurCalYr,RECCL,B,R)=(EQCRET(CurCalYr,RECCL) &
         *EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
      ELSE
        EQCRP90(CurCalYr,RECCL,B,R)=((EQCRET(CurCalYr,RECCL)- &
         EQCRET(CurCalYr-1,RECCL))*EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
      ENDIF

!   COMPUTE AND VINTAGE REPLACEMENTS OF REPLACEMENTS
           IF(CurCalYr.GT.RECSYear+1) THEN
!         EQCRP90RP(CurCalYr,RECCL,B,R)=0.0
              DO Y=RECSYear+1,(CurCalYr-1)
                TEMP=CurCalYr-Y
              ESR=SVRTE(RTALPHA(RECCL),TEMP-1,RTK(RECCL),RTLAMBDA(RECCL)) &
                   -SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
         EQCRP90RP(CurCalYr,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R) + &
          ( EQCRP90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
            EQCRP90RP(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP)) &
        +   EQCPN90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
               ENDDO
           ELSE
          EQCRP90RP(CurCalYr,RECCL,B,R)=EQCPN90(CurCalYr,RECCL,B,R)
           ENDIF
           IF(CurCalYr.GT.RECSYear+1) THEN
          EQCRP90RP(CurCalYr,RECCL,B,R)=EQCRP90RP(CurCalYr,RECCL,B,R)+ &
                                      EQCPN90(CurCalYr,RECCL,B,R)
           ENDIF

           IF(CurCalYr.GT.RECSYear+1) THEN
               DO Y=RECSYear+1,(CurCalYr-1)
                HSR=HDR(B)**(CurCalYr-Y)
                ESR=SVRTE(RTALPHA(RECCL),CurCalYr-Y,RTK(RECCL), &
                          RTLAMBDA(RECCL))
                EQCSR90(CurCalYr,RECCL,B,R)= (&
                  EQCSR90(CurCalYr,RECCL,B,R)+EQCRP90(Y,RECCL,B,R)* &
                  ESR*HSR+EQCRP90RP(Y,RECCL,B,R)*ESR*HSR &
                         + EQCPN90(Y,RECCL,B,R)*ESR*HSR)

!*******************************************************************
! CUMULATE SURVIVING NEW CACS ADDED PRIOR TO PREVYR TO ESTIMATE NH
! SA REPRESENTS NH AT PREVYR-1
! CUMULATE SURVIVING NEW CACS ADDED & REPLACED PRIOR TO PREVYR
! REPLACEMENT EQUIPMENT = SURV.HOUSES(SA) -  SURV.EQUIP(CACSUR)
!*******************************************************************
                SA(EQC,B,R) = (SA(EQC,B,R) + EQCADD(Y,RECCL,B,R)*HSR)
                EQCSUR(CurCalYr,RECCL,B,R) = ( EQCSUR(CurCalYr,RECCL,B,R) + &
                  ((EQCADD(Y,RECCL,B,R)+EQCREP(Y,RECCL,B,R))*(HSR*ESR)))
              ENDDO
           ENDIF
            ENDIF
          ENDDO
 15   CONTINUE

!*******************************************************************
! CALCULATE REPLACEMENTS FOR RECS-YEAR VINTAGE IN PREVYR
!*******************************************************************
! CALCULATE REPLACEMENT HEATERS FOR NEW VINTAGE IN PREVYR
! NOTE: REPLACES WITH LIKE
! NOTE: FOR NEW HOUSES (NH) - PREVYR REPRESENTS THE LAGGED VALUE
!*******************************************************************
      DO 20 R=1,mNumCR-2
        DO 20 B=1,mNumBldg
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            IF(RTCLPNTR(RECCL).GT.0) THEN

!  HEAT PUMP SPACE COOLING USES HEAT PUMP SPACE HEATING DATA
              RECCLHHP=RTCLPNTR(RECCL)
              EQCRP90(CurCalYr,RECCL,B,R)=(EQCRP90(CurCalYr,RECCLHHP,B,R))
              EQCRP90RP(CurCalYr,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCLHHP,B,R))
            IF(B.EQ.1) &
              OEQCRP90(CurCalYr,RECCL,B,R)=(EQCRP90(CurCalYr,RECCLHHP,B,R))
            IF(B.EQ.1) &
             OEQCRP90R(CurCalYr,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCLHHP,B,R))
             EQCREP(CurCalYr,RECCL,B,R)=(EQCREP(CurCalYr,RECCLHHP,B,R))
            IF(B.EQ.1) &
             OEQCREP(CurCalYr,RECCL,B,R)=(EQCREP(CurCalYr,RECCLHHP,B,R))
            ELSE

!   CALCULATE DATA FOR NON-HEAT PUMPS

!******************************************************************
!  Calculate replacement equipment from original base year stock
!******************************************************************
             IF(B.EQ.1) &
              OEQCRP90(CurCalYr,RECCL,B,R)=EQCRP90(CurCalYr,RECCL,B,R)
             IF(B.EQ.1) &
              OEQCRP90R(CurCalYr,RECCL,B,R)=EQCRP90RP(CurCalYr,RECCL,B,R)
              EQCREP(CurCalYr,RECCL,B,R)=(SA(EQC,B,R) &
                                   -EQCSUR(CurCalYr,RECCL,B,R))
             IF(B.EQ.1) &
                OEQCREP(CurCalYr,RECCL,B,R)=(SA(EQC,B,R) &
                                   -EQCSUR(CurCalYr,RECCL,B,R))
            ENDIF
          ENDDO
20    CONTINUE

         DO B=1,mNumBldg
           DO R=1,mNumCR-2
             DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
               EQC=RTCLEQCL(RECCL)
               DO Y=CurCalYr,EndYr       ! VINTAGE EQUIPMENT FOR AVERAGE STOCK ACCOUNTING
                 TEMP=Y-CurCalYr
                 HSR=HDR(B)**(TEMP)
                 ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
                 EQR90FUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90(CurCalYr,RECCL,B,R)*ESR*HSR)
                 EQR90RPFUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R)*ESR*HSR)
                 EQREPFUT(CurCalYr,Y,RECCL,B,R)=(EQCREP(CurCalYr,RECCL,B,R)*ESR*HSR)
                 EQADDFUT(CurCalYr,Y,RECCL,B,R)=(EQCADD(CurCalYr,RECCL,B,R)*ESR*HSR)
               ENDDO
             ENDDO
           ENDDO
         ENDDO

!   ADD TOTAL ACS AND COMPUTE NEW SHELL EFFICIENCY

      DO R=1,mNumCR-2
           DO B=1,mNumBldg
            NCSHELL(CurCalYr,R,B)=0.0
            NEWSHELLWTC(CurCalYr,B,R)=0.0
            NEWADDWTC(CurCalYr,B,R)=0.0
            ANUMC(R,B)=0.0
            ADENC(R,B)=0.0
       ENDDO
          ENDDO
      DO 32 R=1,mNumCR-2
        DO 32 B=1,mNumBldg
          DO 32 RECCL=RTCLEUPT(EU)+2,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            NEWSHELLWTC(CurCalYr,B,R)=NEWSHELLWTC(CurCalYr,B,R)+ &
             EQCADD(CurCalYr,RECCL,B,R)*CSHELL(CurCalYr,EQC,B,R)
            NEWADDWTC(CurCalYr,B,R)=NEWADDWTC(CurCalYr,B,R)+EQCADD(CurCalYr,RECCL,B,R)
 32   CONTINUE

!   COMPUTE SHELL AVERAGE FOR EACH FUEL TYPE

      DO R=1,mNumCR-2
        DO B=1,mNumBldg
          IF (NEWADDWTC(CurCalYr,B,R).GT.0.0) THEN
            NCSHELL(CurCalYr,R,B)=NEWSHELLWTC(CurCalYr,B,R)/NEWADDWTC(CurCalYr,B,R)
          ELSE
            NCSHELL(CurCalYr,R,B)=1.0
          ENDIF
        ENDDO
      ENDDO

      DO D=1,mNumCR-2
        DO B=1,mNumBldg
          ! cooling non HP
          EXSQFTADJ(CurCalYr,B,D,3)=(ELASTIC(3,D)*((EXSQRFOOT(CurCalYr,B,D)-EXSQRFOOT(RECSYear,B,D))/ &
           EXSQRFOOT(RECSYear,B,D))*NCSHELL(CurCalYr,D,B))+1
          ! cooling HP
          EXSQFTADJ(CurCalYr,B,D,4)=(ELASTIC(4,D)*((EXSQRFOOT(CurCalYr,B,D)-EXSQRFOOT(RECSYear,B,D))/ &
           EXSQRFOOT(RECSYear,B,D))*NCSHELL(CurCalYr,D,B))+1
        ENDDO
      ENDDO

!*******************************************************************
!  CALCULATE AVERAGE NEW COOLING SHELL INDEX
!*******************************************************************
        DO 50 R=1,mNumCR-2
          DO 50 B=1,mNumBldg
            DO 50 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)
              IF (CurCalYr.GT.RECSYear+1) THEN
                DO Y=RECSYear+1,CurCalYr-1
                  ANUMC(R,B)=ANUMC(R,B)+(NCSHELL(Y,R,B)* &
                  EQCADD(Y,RECCL,B,R))
                  ADENC(R,B)=ADENC(R,B)+(EQCADD(Y,RECCL,B,R))
                ENDDO
              ENDIF
        50  CONTINUE

!*******************************************************************
        DO 55 R=1,mNumCR-2
          DO 55 B=1,mNumBldg
            IF (ADENC(R,B).LE.0) THEN
              ACSHELL(CurCalYr,R,B)=ACSHELL(CurCalYr-1,R,B)
            ELSE
              ACSHELL(CurCalYr,R,B)=ANUMC(R,B)/ADENC(R,B)
            ENDIF
           ! IF(ACSHELL(CurCalYr,R,B).GT.ACSHELL(CurCalYr-1,R,B)) ACSHELL(CurCalYr,R,B)=ACSHELL(CurCalYr-1,R,B)
            IF(ACSHELL(CurCalYr,R,B).LT.LIMIT) ACSHELL(CurCalYr,R,B)=LIMIT
            IF(CurCalYr.LE.RECSYear+1) THEN
              ACSHELL(CurCalYr,R,B)=NCSHELL(CurCalYr,R,B)
            ENDIF
        55  CONTINUE

!*******************************************************************
!     AGGREGATE COOLING SYSTEMS FOR INVESTMENT ANALYSIS
!*******************************************************************
       Y=CurCalYr
       NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)
       DO B=1,mNumBldg
         DO r=1,mNumCR-2
           TYPE=RTTYPECT(EU)
           DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)

!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
             IF(CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
               IF (RTCENDIV(RECTY).EQ.R) THEN
                 TYPE=TYPE+1                  ! INDEX FOR 'TYPE' VARIABLES
                 EQT=RTEQTYPE(RECTY)
                 EQC=RTTYEQCL(RECTY)
                 RECCL=RTCLEUPT(EU)+EQC
                 X=1.0
                 IF (RECCL.EQ.12) THEN !nHeatClasses + RTCLEQCL(ROOM_AIR) = 11 + 1 = 12
                   X=1.0	!kj - Is this necessary?
                   HEATINGTYPEPURCH(Y,TYPE,B,R,1)=(NEQTSHR(Y,TYPE,B,r)*EQCADD(Y,RECCL,B,r))
                   HEATINGTYPEPURCH(Y,TYPE,B,R,2)=(NEQTSHR(Y,TYPE,B,r)*(EQCREP(Y,RECCL,B,r) + EQCRP90RP(Y,RECCL,B,r)) + &
                    REQTSHR(Y,TYPE,B,r)*EQCRP90(Y,RECCL,B,r))
                 ELSE
                   HEATINGTYPEPURCH(Y,TYPE,B,R,1)=(NEQTSHRC(Y,EQT,B,r)*EQCADD(Y,RECCL,B,r))
                   HEATINGTYPEPURCH(Y,TYPE,B,R,2)=(NEQTSHR(Y,TYPE,B,r)*(EQCREP(Y,RECCL,B,r) + EQCRP90RP(Y,RECCL,B,r)) + &
                    REQTSHR(Y,TYPE,B,r)*EQCRP90(Y,RECCL,B,r))
                 ENDIF
               ENDIF
             ENDIF
           ENDDO
         ENDDO
       ENDDO

END SUBROUTINE RCLADD


!*******************************************************************
!     COOLING CONSUMPTION SUBROUTINE
!*******************************************************************
      SUBROUTINE RCLCON
      IMPLICIT NONE
      REAL*4 ALPHA,ef1,ef2,ef3,TEMP,CDDFACT(mNumCR),TEMP1,TEMP2
      REAL*4 alpha2,rba,rbr,rbn
      REAL*4 AVNEWEQP(RECSYear+1:EndYr),AVNEWUEC(RECSYear+1:EndYr),UECPRINT(RECSYear+1:EndYr)  !yr
      INTEGER B, D, F, Y, YEAR,CYEAR,S,R
      INTEGER EU, EUPR, EQC, RECCL, FCON, FDIM, EQCGHP, EQCEHP, V
      INTEGER RECCLGHP, RECCLEHP

!*******************************************************************
!  EQUIPMENT 1=ROOM_AIR 2=CENT_AIR 3=ELEC_HP 4=GEO_HP 5=NG_HP - Y,E,B,D
!  CONSUMPTION FUEL 1=ELECTRICITY 2=GEOTHERMAL 3=NATURAL GAS
!*******************************************************************

!  SET EU = 2 TO SEARCH THE SPACE COOLING SECTION OF THE DATA
      EU=2
      EUPR=2
      ALPHA=-.15; ef1=.5;ef2=.35;ef3=.15
      ALPHA2=-.15  !cooling shell adjustment?

!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,mNumCR-2
        PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
      ENDDO

!   SET CDDFACT

      DO D=1,mNumCR-2
          CDDFACT(D)=(CDDADJ(CurCalYr,D)/CDDADJ(RECSYear,D))**1.50 !A 10% increase in CDD would increase space cooling consumption by 15% (e.g., 1.10^1.50=1.15)
      ENDDO

!*******************************************************************
! COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR RECS-YEAR EQUIPMENT
!*******************************************************************
      IF (CurCalYr.GE.RECSYear+1) THEN
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RTBASEFF(CurCalYr,RECCL)= STKEFF(CurCalYr,RECCL)
        ENDDO
      ENDIF

!*******************************************************************
!  CALCULATE NEW AND AVERAGE UECS
!*******************************************************************
      DO 20 D=1,mNumCR-2
        DO 20 B=1,mNumBldg

!   SEARCH THE SPACE COOLING SECTION OF THE DATA (EU=2)

          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            IF (EQC.LT.3) THEN
              S=3
            ELSE
              S=4
            ENDIF
         EQCSUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)*EXSQFTADJ(CurCalYr,B,D,S)* &
           ( RTBASEFF(RECSYear,RECCL) / RTBASEFF(CurCalYr,RECCL) )
         EQCSIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
           ( RTBASEFF(RECSYear,RECCL) / RTBASEFF(CurCalYr,RECCL) )
       IF (WTEQCEFFN(CurCalYr,RECCL,B,D) .GT. 0.0) THEN
         EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
           WTEQCEFFN(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL) ! &
         EQCNIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B) * &
           WTEQCEFFN(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)
       ELSE
         EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
         EQCNIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
           ENDIF
       IF (WTEQCEFFHV(CurCalYr,RECCL,B,D) .GT. 0.0) THEN
         EQCHVUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
           WTEQCEFFHV(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL) &
                                      *WTEQCSQFHV(CurCalYr,RECCL,B,D)
         EQCHVIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
           WTEQCEFFHV(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)
       ELSE
         EQCHVUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)*WTEQCSQFHV(CurCalYr,RECCL,B,D)
         EQCHVIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
       ENDIF
        IF (WTEQCEFFR(CurCalYr,RECCL,B,D) .GT. 0.0) THEN
          EQCRUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
          WTEQCEFFR(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)*EXSQFTADJ(CurCalYr,B,D,S)
          EQCRIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
          WTEQCEFFR(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)
        ELSE
          EQCRUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
          EQCRIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
        ENDIF
         IF (CurCalYr .EQ. RECSYear+1) THEN
           EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
           EQCAHVUEC(CurCalYr,RECCL,B,D)=EQCHVUEC(CurCalYr,RECCL,B,D)
         ELSE
         TEMP=0.0
         TEMP1=0.0
         TEMP2=0.0
           DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
         EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
          TEMP1=TEMP1+ &
         EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)
          TEMP2=TEMP2+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
         EQR90RPFUT(Y,CurCalYr,RECCL,B,D) + EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)
         ENDDO
          IF (EQC.EQ.1) THEN
            IF(TEMP2.LE.0.0) THEN
              EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
            ELSE
              EQCAUEC(CurCalYr,RECCL,B,D)=0.0
            DO Y=RECSYear,CurCalYr-1
              EQCAUEC(CurCalYr,RECCL,B,D)=EQCAUEC(CurCalYr,RECCL,B,D)+ &
               ((EQR90FUT(Y,CurCalYr,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D))+ &
               (EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D))*EQCNUEC(Y,RECCL,B,D)+ &
               EQR90RPFUT(Y,CurCalYr,RECCL,B,D)*EQCNIUEC(Y,RECCL,B,D))/TEMP2
            ENDDO
          ENDIF
          ELSE
          IF(TEMP.LE.0.0) THEN
            EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
          ELSE
            EQCAUEC(CurCalYr,RECCL,B,D)=0.0
            DO Y=RECSYear,CurCalYr-1
              EQCAUEC(CurCalYr,RECCL,B,D)=EQCAUEC(CurCalYr,RECCL,B,D)+ &
               (EQR90FUT(Y,CurCalYr,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D)+ &
               EQR90RPFUT(Y,CurCalYr,RECCL,B,D)*EQCNIUEC(Y,RECCL,B,D))/TEMP
            ENDDO
          ENDIF
        ENDIF
         IF (EQC.GT.1) THEN
           IF(TEMP1.LE.0.0) THEN
             EQCAHVUEC(CurCalYr,RECCL,B,D)=EQCHVUEC(CurCalYr,RECCL,B,D)
           ELSE
             EQCAHVUEC(CurCalYr,RECCL,B,D)=0.0
             DO Y=RECSYear,CurCalYr-1
               EQCAHVUEC(CurCalYr,RECCL,B,D)=EQCAHVUEC(CurCalYr,RECCL,B,D)+ &
                (EQADDFUT(Y,CurCalYr,RECCL,B,D)*EQCHVUEC(Y,RECCL,B,D)+ &
                EQREPFUT(Y,CurCalYr,RECCL,B,D)*EQCNUEC(Y,RECCL,B,D))/TEMP1
             ENDDO
           ENDIF
         ENDIF
       ENDIF
     ENDDO
 20   CONTINUE
         AVNEWUEC(CurCalYr)=0.0
         AVNEWEQP(CurCalYr)=0.0
      DO 21 D=1,mNumCR-2
        DO 21 B=1,mNumBldg
          DO 21 RECCL=13,14 !nHeatClasses+RTCLEQCL(cooling); CENT_AIR=11+2 and ELEC_HP=11+3
            AVNEWUEC(CurCalYr)=AVNEWUEC(CurCalYr)+( &
             EQCRUEC(CurCalYr,RECCL,B,D)*EQCRP90(CurCalYr,RECCL,B,D) + &
             EQCAUEC(CurCalYr,RECCL,B,d)*EQCREP(CurCalYr,RECCL,B,D) + &
             EQCADD(CurCalYr,RECCL,B,d)*EQCHVUEC(CurCalYr,RECCL,B,D)+ &
             EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNIUEC(CurCalYr,RECCL,B,D))
            AVNEWEQP(CurCalYr)=AVNEWEQP(CurCalYr)+&
             EQCRP90(CurCalYr,RECCL,B,D) + EQCREP(CurCalYr,RECCL,B,D) + &
             EQCADD(CurCalYr,RECCL,B,d)+EQCRP90RP(CurCalYr,RECCL,B,D)
      21  CONTINUE

      UECPRINT(CurCalYr)=(AVNEWUEC(CurCalYr)/AVNEWEQP(CurCalYr))*293.

!*******************************************************************
!  CALCULATE AVERAGE EFFICIENCY
!*******************************************************************
      DO 30 B=1,mNumBldg
        DO 30 D=1,mNumCR-2
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            IF (CurCalYr.EQ.RECSYear+1) THEN
              WTEQCEFFA(RECSYear+1,RECCL,B,D)=WTEQCEFFN(RECSYear+1,RECCL,B,D)
            ELSE
              TEMP=0.0
              DO Y=RECSYear,CurCalYr-1
                TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
                 EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
                 EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
              ENDDO
              IF (TEMP .GT. 0.0) THEN
                WTEQCEFFA(CurCalYr,RECCL,B,D)=0.0
                DO Y=RECSYear,CurCalYr-1
                  IF (EQC.EQ.1) THEN
                    WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFA(CurCalYr,RECCL,B,D)+( &
                     (EQR90FUT(Y,CurCalYr,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))+ &
                     ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
                     EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D)))/TEMP
                  ELSE
                    WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFA(CurCalYr,RECCL,B,D)+( &
                     EQR90FUT(Y,CurCalYr,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D)+ &
                     EQADDFUT(Y,CurCalYr,RECCL,B,D)*WTEQCEFFHV(Y,RECCL,B,D)+ &
                     (EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
                     EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D))/TEMP
                  ENDIF
                ENDDO
              ELSE
                WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFN(CurCalYr,RECCL,B,D)
              ENDIF
            ENDIF
          ENDDO
        30  CONTINUE

!*******************************************************************
!  INITIALIZE COOLING CONSUMPTION
!*******************************************************************
      DO 90 D=1,mNumCR-2
        DO 90 FCON=1,NCLFL
          COOLCN(CurIYr,FCON,D)=0.0
          DO 90 B=1,mNumBldg
            Driver(CurIYr,fcon,d,b)=0.
            COOLCNWT(CurIYr,FCON,D,B)=0.
            COOLCNIN(CurIYr,FCON,D,B)=0.
      90  CONTINUE

!  FIND INDICES FOR THE ELECTRIC AND GEOTHERMAL HEAT PUMPS
!    USED TO COMPUTE COOLCN FOR GEOTHERMAL FUEL (FCON=2)
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        IF(RTCLNAME(RECCL).EQ.'ELEC_HP')THEN
          EQCEHP=RTCLEQCL(RECCL)
          RECCLEHP=EQCEHP+RTCLEUPT(EU)
        ELSEIF(RTCLNAME(RECCL).EQ.'GEO_HP')THEN
          EQCGHP=RTCLEQCL(RECCL)
          RECCLGHP=EQCGHP+RTCLEUPT(EU)
        ENDIF
      ENDDO

!*******************************************************************
!  CALCULATE COOLING CONSUMPTION
!*******************************************************************
      DO 100 D=1,mNumCR-2
        DO 100 B=1,mNumBldg
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC = RTCLEQCL(RECCL)
            F=RTFUEL(RECCL) !F = RTEK FUEL NUMBER
            FCON=FCLCON(F) !FCON = CONSUMPTION FUEL NUMBER FOR COOLING
            EQCEQCN(CurIYr,RECCL,B,D)=0.0
            IF (CurCalYr.EQ.RECSYear+1) THEN
              IF (EQC.EQ.1) THEN !ROOM_AIR
                COOLCN(CurIYr,FCON,D)=COOLCN(CurIYr,FCON,D)+ CDDFACT(D)*( &
                 ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D)* &
                 (ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+(EQCADD(CurCalYr,RECCL,B,D)* &
                 EQCNUEC(CurCalYr,RECCL,B,D)*(NCSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
                 (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNIUEC(CurCalYr,RECCL,B,D)*EXSQFTADJ(CurCalYr,B,D,S) &
                 *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
                 (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D) &
                 *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B))))* &
                 RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

               COOLCNWT(CurIYr,FCON,D,B)=COOLCNWT(CurIYr,FCON,D,B)+CDDFACT(D)*( &
               ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D)* &
                (ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+(EQCADD(CurCalYr,RECCL,B,D)* &
                 EQCNUEC(CurCalYr,RECCL,B,D)*(NCSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
                (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNIUEC(CurCalYr,RECCL,B,D)*EXSQFTADJ(CurCalYr,B,D,S) &
               *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
                (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D) &
               *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B))))* &
                 RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

              COOLCNIN(CurIYr,FCON,D,B)=COOLCNIN(CurIYr,FCON,D,B)+( &
               ((EQCESE(CurCalYr,RECCL,B,D)*EQCSIUEC(CurCalYr,RECCL,B,D)* &
               (ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+(EQCADD(CurCalYr,RECCL,B,D)* &
                EQCNUEC(CurCalYr,RECCL,B,D)*(NCSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
               (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNIUEC(CurCalYr,RECCL,B,D) &
              *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
               (EQCRP90(CurCalYr,RECCL,B,D)*EQCRIUEC(CurCalYr,RECCL,B,D) &
              *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))))

              Driver(CurIYr,fcon,d,b)=Driver(CurIYr,fcon,d,b)+                   &
               (EQCESE(CurCalYr,RECCL,B,d)+EQCADD(CurCalYr,RECCL,B,d)+ &
                EQCRP90RP(CurCalYr,RECCL,B,d)+EQCRP90(CurCalYr,RECCL,B,d))

              EQCEQCN(CurIYr,RECCL,B,D)=CDDFACT(D)*(((EQCESE(CurCalYr,RECCL,B,D)* &
                EQCSUEC(CurCalYr,RECCL,B,D)* &
               (ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B))) + &
               (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNIUEC(CurCalYr,RECCL,B,D)*EXSQFTADJ(CurCalYr,B,D,S) &
              *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
               (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)* &
               (NCSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+(EQCRP90(CurCalYr,RECCL,B,D)* &
                EQCRUEC(CurCalYr,RECCL,B,D)*(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B))))* &
                RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

             ELSE ! EQC <> 1
              COOLCN(CurIYr,FCON,D)=COOLCN(CurIYr,FCON,D)+ CDDFACT(D)*( &
               ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D)* &
                (ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+(EQCADD(CurCalYr,RECCL,B,D)* &
                 EQCHVUEC(CurCalYr,RECCL,B,D)*(NCSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
                (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNIUEC(CurCalYr,RECCL,B,D)*EXSQFTADJ(CurCalYr,B,D,S) &
               *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
                (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D) &
               *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B))))* &
                 RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

              COOLCNWT(CurIYr,FCON,D,B)=COOLCNWT(CurIYr,FCON,D,B)+CDDFACT(D)*( &
               ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D)* &
                (ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+(EQCADD(CurCalYr,RECCL,B,D)* &
                 EQCHVUEC(CurCalYr,RECCL,B,D)*(NCSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
                (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNIUEC(CurCalYr,RECCL,B,D)*EXSQFTADJ(CurCalYr,B,D,S) &
               *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
                (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D) &
               *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B))))* &
                 RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

               COOLCNIN(CurIYr,FCON,D,B)=COOLCNIN(CurIYr,FCON,D,B)+( &
               ((EQCESE(CurCalYr,RECCL,B,D)*EQCSIUEC(CurCalYr,RECCL,B,D)* &
                (ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+(EQCADD(CurCalYr,RECCL,B,D)* &
                 EQCHVIUEC(CurCalYr,RECCL,B,D)*(NCSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
                (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNIUEC(CurCalYr,RECCL,B,D) &
               *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
                (EQCRP90(CurCalYr,RECCL,B,D)*EQCRIUEC(CurCalYr,RECCL,B,D) &
               *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))))

               Driver(CurIYr,fcon,d,b)=Driver(CurIYr,fcon,d,b)+ &
                (EQCESE(CurCalYr,RECCL,B,d)+EQCADD(CurCalYr,RECCL,B,d)+ &
                 EQCRP90RP(CurCalYr,RECCL,B,d)+EQCRP90(CurCalYr,RECCL,B,d))

               EQCEQCN(CurIYr,RECCL,B,D)=CDDFACT(D)*(((EQCESE(CurCalYr,RECCL,B,D)* &
                 EQCSUEC(CurCalYr,RECCL,B,D)* &
                (ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B))) + &
                (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNIUEC(CurCalYr,RECCL,B,D)*EXSQFTADJ(CurCalYr,B,D,S) &
               *(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+ &
                (EQCADD(CurCalYr,RECCL,B,D)*EQCHVUEC(CurCalYr,RECCL,B,D)* &
                (NCSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B)))+(EQCRP90(CurCalYr,RECCL,B,D)* &
                 EQCRUEC(CurCalYr,RECCL,B,D)*(ECSHELL(CurCalYr,D,B)/ECSHELL(RECSYear,D,B))))* &
                 RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

             ENDIF ! EQC=1

            ELSE ! CurCalYr <> RECSYear+1

! Special treatment of stimulus program
               IF ((CurCalYr.GT.2010).AND.(STIMULUS.EQ.1).AND.(F.EQ.4)) THEN
                 ALPHA=-0.30
                ELSE
                 ALPHA=-0.15
               ENDIF

      rba=(rtbaseff(RECSYear,RECCL)*wteqceffa(CurCalYr,RECCL,b,d))**alpha2
      rbr=(rtbaseff(RECSYear,RECCL)*wteqceffr(CurCalYr,RECCL,b,d))**alpha2
      rbn=(rtbaseff(RECSYear,RECCL)*wteqceffn(CurCalYr,RECCL,b,d))**alpha2

         IF (EQC.EQ.1) THEN
               coolcn(CurIYr,fcon,d)=coolcn(CurIYr,fcon,d)+CDDFACT(D)* (( &
               (EQCESE(CurCalYr,RECCL,b,d)*eqcsuec(CurCalYr,RECCL,b,d)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2)) + &
               (eqcadd(CurCalYr,RECCL,b,d)*eqcnuec(CurCalYr,RECCL,b,d)* &
       (ncshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn + &
               (eqcrp90(CurCalYr,RECCL,b,d)*eqcruec(CurCalYr,RECCL,b,d)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbr + &
             (eqcrp90rp(CurCalYr,RECCL,b,d)*eqcniuec(CurCalYr,RECCL,b,d)*EXSQFTADJ(CurCalYr,B,D,S)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn + &
               (EQCSR90(CurCalYr,RECCL,b,d)*eqcauec(CurCalYr,RECCL,b,d) &
       *(ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rba + &
               (EQCREP(CurCalYr,RECCL,b,d) *eqcnuec(CurCalYr,RECCL,b,d)* &
       (acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn + &
               (EQCSUR(CurCalYr,RECCL,b,d) *eqcauec(CurCalYr,RECCL,b,d) &
       *(acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rba)*( &
               RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)))

               coolcnwt(CurIYr,fcon,d,b)=coolcnwt(CurIYr,fcon,d,b)+CDDFACT(D)*(( &
               (EQCESE(CurCalYr,RECCL,b,d)*eqcsiuec(CurCalYr,RECCL,b,d)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2)) + &
               (eqcadd(CurCalYr,RECCL,b,d)*eqcnuec(CurCalYr,RECCL,b,d)* &
       (ncshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn + &
               (eqcrp90(CurCalYr,RECCL,b,d)*eqcruec(CurCalYr,RECCL,b,d)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbr + &
             (eqcrp90rp(CurCalYr,RECCL,b,d)*eqcniuec(CurCalYr,RECCL,b,d)*EXSQFTADJ(CurCalYr,B,D,S)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn + &
               (EQCSR90(CurCalYr,RECCL,b,d)*eqcauec(CurCalYr,RECCL,b,d) &
       *(ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rba + &
               (EQCREP(CurCalYr,RECCL,b,d) *eqcnuec(CurCalYr,RECCL,b,d)* &
       (acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn + &
               (EQCSUR(CurCalYr,RECCL,b,d) *eqcauec(CurCalYr,RECCL,b,d) &
       *(acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rba)*( &
               RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)))

               coolcnin(CurIYr,fcon,d,b)=coolcnin(CurIYr,fcon,d,b)+(( &
               (EQCESE(CurCalYr,RECCL,b,d)*eqcsiuec(CurCalYr,RECCL,b,d)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))) + &
               (eqcadd(CurCalYr,RECCL,b,d)*eqcniuec(CurCalYr,RECCL,b,d)* &
       (ncshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))) + &
               (eqcrp90(CurCalYr,RECCL,b,d)*eqcriuec(CurCalYr,RECCL,b,d)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))) + &
             (eqcrp90rp(CurCalYr,RECCL,b,d)*eqcniuec(CurCalYr,RECCL,b,d)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))) + &
               (EQCSR90(CurCalYr,RECCL,b,d)*eqcauec(CurCalYr,RECCL,b,d) &
       *(ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B)))+ &
               (EQCREP(CurCalYr,RECCL,b,d) *eqcniuec(CurCalYr,RECCL,b,d)* &
       (acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))) + &
               (EQCSUR(CurCalYr,RECCL,b,d) *eqcauec(CurCalYr,RECCL,b,d) &
       *(acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B)))))

       Driver(CurIYr,fcon,d,b)=Driver(CurIYr,fcon,d,b)+                   &
                (EQCESE(CurCalYr,RECCL,B,d)+EQCADD(CurCalYr,RECCL,B,d)+     &
                 EQCRP90RP(CurCalYr,RECCL,B,d)+EQCRP90(CurCalYr,RECCL,B,d)+ &
                 EQCSR90(CurCalYr,RECCL,b,d)+EQCREP(CurCalYr,RECCL,b,d)+    &
                 EQCSUR(CurCalYr,RECCL,b,d))

               eqceqcn(CurIYr,RECCL,b,d)=CDDFACT(D)*( &
               ((EQCESE(CurCalYr,RECCL,b,d)*eqcsuec(CurCalYr,RECCL,b,d)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))+ &
                (eqcadd(CurCalYr,RECCL,b,d)*eqcnuec(CurCalYr,RECCL,b,d)* &
       (ncshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn  + &
                (eqcrp90(CurCalYr,RECCL,b,d)*eqcruec(CurCalYr,RECCL,b,d)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbr+ &
              (eqcrp90rp(CurCalYr,RECCL,b,d)*eqcniuec(CurCalYr,RECCL,b,d)*EXSQFTADJ(CurCalYr,B,D,S)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn+ &
                (EQCSR90(CurCalYr,RECCL,b,d)*eqcauec(CurCalYr,RECCL,b,d)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rba  + &
                (EQCREP(CurCalYr,RECCL,b,d)*eqcnuec(CurCalYr,RECCL,b,d)* &
       (acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn+ &
                (EQCSUR(CurCalYr,RECCL,b,d)*eqcauec(CurCalYr,RECCL,b,d)* &
       (acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rba)*( &
                RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)))

         ELSE
               coolcn(CurIYr,fcon,d)=coolcn(CurIYr,fcon,d)+CDDFACT(D)* (( &
               (EQCESE(CurCalYr,RECCL,b,d)*eqcsuec(CurCalYr,RECCL,b,d)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2)) + &
               (eqcadd(CurCalYr,RECCL,b,d)*eqchvuec(CurCalYr,RECCL,b,d)* &
       (ncshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn + &
               (eqcrp90(CurCalYr,RECCL,b,d)*eqcruec(CurCalYr,RECCL,b,d)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbr + &
             (eqcrp90rp(CurCalYr,RECCL,b,d)*eqcniuec(CurCalYr,RECCL,b,d)*EXSQFTADJ(CurCalYr,B,D,S)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn + &
               (EQCSR90(CurCalYr,RECCL,b,d)*eqcauec(CurCalYr,RECCL,b,d) &
       *(ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rba + &
               (EQCREP(CurCalYr,RECCL,b,d) *eqcnuec(CurCalYr,RECCL,b,d)* &
       (acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn + &
               (EQCSUR(CurCalYr,RECCL,b,d) *eqcahvuec(CurCalYr,RECCL,b,d) &
       *(acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rba)*( &
               RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)))

               coolcnwt(CurIYr,fcon,d,b)=coolcnwt(CurIYr,fcon,d,b)+CDDFACT(D)*(( &
               (EQCESE(CurCalYr,RECCL,b,d)*eqcsuec(CurCalYr,RECCL,b,d)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2)) + &
               (eqcadd(CurCalYr,RECCL,b,d)*eqchvuec(CurCalYr,RECCL,b,d)* &
       (ncshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn + &
               (eqcrp90(CurCalYr,RECCL,b,d)*eqcruec(CurCalYr,RECCL,b,d)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbr + &
             (eqcrp90rp(CurCalYr,RECCL,b,d)*eqcniuec(CurCalYr,RECCL,b,d)*EXSQFTADJ(CurCalYr,B,D,S)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn + &
               (EQCSR90(CurCalYr,RECCL,b,d)*eqcauec(CurCalYr,RECCL,b,d) &
       *(ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rba + &
               (EQCREP(CurCalYr,RECCL,b,d) *eqcnuec(CurCalYr,RECCL,b,d)* &
       (acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn + &
               (EQCSUR(CurCalYr,RECCL,b,d) *eqcahvuec(CurCalYr,RECCL,b,d) &
       *(acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rba)*( &
               RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)))

               coolcnin(CurIYr,fcon,d,b)=coolcnin(CurIYr,fcon,d,b)+(( &
               (EQCESE(CurCalYr,RECCL,b,d)*eqcsiuec(CurCalYr,RECCL,b,d)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))) + &
               (eqcadd(CurCalYr,RECCL,b,d)*eqchviuec(CurCalYr,RECCL,b,d)* &
       (ncshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))) + &
               (eqcrp90(CurCalYr,RECCL,b,d)*eqcriuec(CurCalYr,RECCL,b,d)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))) + &
             (eqcrp90rp(CurCalYr,RECCL,b,d)*eqcniuec(CurCalYr,RECCL,b,d)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))) + &
               (EQCSR90(CurCalYr,RECCL,b,d)*eqcauec(CurCalYr,RECCL,b,d) &
       *(ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B)))+ &
               (EQCREP(CurCalYr,RECCL,b,d) *eqcniuec(CurCalYr,RECCL,b,d)* &
       (acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))) + &
               (EQCSUR(CurCalYr,RECCL,b,d) *eqcahvuec(CurCalYr,RECCL,b,d) &
       *(acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B)))))

       Driver(CurIYr,fcon,d,b)=Driver(CurIYr,fcon,d,b)+                   &
                (EQCESE(CurCalYr,RECCL,B,d)+EQCADD(CurCalYr,RECCL,B,d)+     &
                 EQCRP90RP(CurCalYr,RECCL,B,d)+EQCRP90(CurCalYr,RECCL,B,d)+ &
                 EQCSR90(CurCalYr,RECCL,b,d)+EQCREP(CurCalYr,RECCL,b,d)+    &
                 EQCSUR(CurCalYr,RECCL,b,d))

               eqceqcn(CurIYr,RECCL,b,d)=CDDFACT(D)*( &
               ((EQCESE(CurCalYr,RECCL,b,d)*eqcsuec(CurCalYr,RECCL,b,d)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))+ &
                (eqcadd(CurCalYr,RECCL,b,d)*eqchvuec(CurCalYr,RECCL,b,d)* &
       (ncshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn  + &
                (eqcrp90(CurCalYr,RECCL,b,d)*eqcruec(CurCalYr,RECCL,b,d)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbr+ &
              (eqcrp90rp(CurCalYr,RECCL,b,d)*eqcniuec(CurCalYr,RECCL,b,d)*EXSQFTADJ(CurCalYr,B,D,S)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn+ &
                (EQCSR90(CurCalYr,RECCL,b,d)*eqcauec(CurCalYr,RECCL,b,d)* &
       (ecshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rba  + &
                (EQCREP(CurCalYr,RECCL,b,d)*eqcnuec(CurCalYr,RECCL,b,d)* &
       (acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rbn+ &
                (EQCSUR(CurCalYr,RECCL,b,d)*eqcahvuec(CurCalYr,RECCL,b,d)* &
       (acshell(CurCalYr,d,b)/ECSHELL(RECSYear,D,B))**(1.0+alpha2))*rba     )*( &
                RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)))

           ENDIF
          ENDIF
        ENDDO

!GEOTHERMAL IS CALCULATED DIFFERENTLY FROM OTHER FUELS
! Set FCON=7 for geothermal and calculate outside of the RSCLASS loop because it is not encountered in RSCLASS.
! UEC of ground-source heat pump (RECCL=15) is subtracted from air-source heat pump (RECCL=14) to differentiate amount of electricity used versus geothermal energy	!kj
            FCON = FCLCON(7)   ! FCON = 2	!kj - Why =2 in comment?
            COOLCN(CurIYr,FCON,D)=COOLCN(CurIYr,FCON,D)+CDDFACT(D)* &
             (EQCESE(CurCalYr,RECCLGHP,B,D)* &
            ((EQCUEC(D,RECCLEHP,B)-EQCUEC(D,RECCLGHP,B))*WHRFOSS(D,CurIYr)/3412.)+ &  !STEOhr
              EQCRP90(CurCalYr,RECCLGHP,B,D)* &
            ((EQCRUEC(CurCalYr,RECCLEHP,B,D)-EQCRUEC(CurCalYr,RECCLGHP,B,D))*WHRFOSS(D,CurIYr)/3412.) &  !STEOhr
            + EQCADD(CurCalYr,RECCLGHP,B,D)* &
                   ((EQCHVUEC(CurCalYr,RECCLEHP,B,D)-EQCHVUEC(CurCalYr,RECCLGHP,B,D))*WHRFOSS(D,CurIYr)/3412.) &  !STEOhr
            + EQCSUR(CurCalYr,RECCLGHP,B,D)* &
                   ((EQCAHVUEC(CurCalYr,RECCLEHP,B,D)-EQCAHVUEC(CurCalYr,RECCLGHP,B,D))*WHRFOSS(D,CurIYr)/3412.) + &  !STEOhr
          (EQCREP(CurCalYr,RECCLGHP,B,D)+EQCRP90RP(CurCalYr,RECCLGHP,B,D))* &
            ((EQCNUEC(CurCalYr,RECCLEHP,B,D)-EQCNUEC(CurCalYr,RECCLGHP,B,D))*WHRFOSS(D,CurIYr)/3412.) &  !STEOhr
            +(EQCSR90(CurCalYr,RECCLGHP,B,D))*&
            ((EQCAUEC(CurCalYr,RECCLEHP,B,D)-EQCAUEC(CurCalYr,RECCLGHP,B,D))*WHRFOSS(D,CurIYr)/3412.))  !STEOhr

            GEEQCN(CurIYr,2,B,D)=CDDFACT(D)*&
             (EQCESE(CurCalYr,RECCLGHP,B,D)* &
            ((EQCUEC(D,RECCLEHP,B)-EQCUEC(D,RECCLGHP,B))*WHRFOSS(D,CurIYr)/3412.)+ &  !STEOhr
              EQCRP90(CurCalYr,RECCLGHP,B,D)* &
            ((EQCRUEC(CurCalYr,RECCLEHP,B,D)-EQCRUEC(CurCalYr,RECCLGHP,B,D))*WHRFOSS(D,CurIYr)/3412.) &  !STEOhr
            + EQCADD(CurCalYr,RECCLGHP,B,D)* &
                   ((EQCHVUEC(CurCalYr,RECCLEHP,B,D)-EQCHVUEC(CurCalYr,RECCLGHP,B,D))*WHRFOSS(D,CurIYr)/3412.) &  !STEOhr   !kj - this calculation is negative!
            + EQCSUR(CurCalYr,RECCLGHP,B,D)* &
                   ((EQCAHVUEC(CurCalYr,RECCLEHP,B,D)-EQCAHVUEC(CurCalYr,RECCLGHP,B,D))*WHRFOSS(D,CurIYr)/3412.) + &  !STEOhr   !kj - this calculation is negative!
          (EQCREP(CurCalYr,RECCLGHP,B,D)+EQCRP90RP(CurCalYr,RECCLGHP,B,D))* &
            ((EQCNUEC(CurCalYr,RECCLEHP,B,D)-EQCNUEC(CurCalYr,RECCLGHP,B,D))*WHRFOSS(D,CurIYr)/3412.) &  !STEOhr
            +(EQCSR90(CurCalYr,RECCLGHP,B,D))* &
            ((EQCAUEC(CurCalYr,RECCLEHP,B,D)-EQCAUEC(CurCalYr,RECCLGHP,B,D))*WHRFOSS(D,CurIYr)/3412.))  !STEOhr
 100  CONTINUE

      DO R=1,mNumCR-2
        DO FCON=1,NCLFL
         DO B=1,mNumBldg
          IF (Driver(CurIYr,FCON,R,B).GT.0)   &
           COOLCNIN(CurIYr,FCON,R,B)=                &
           COOLCNIN(CurIYr,FCON,R,B)/Driver(CurIYr,FCON,R,B)
         ENDDO
        ENDDO
      ENDDO

      END SUBROUTINE RCLCON


!*******************************************************************
!     CLOTHES WASHER CHOICE SUBROUTINE
!*******************************************************************
      SUBROUTINE RCWTEC
      IMPLICIT NONE
      REAL*4 TCWSHR(mNumBldg,mNumCR-2)   !share of top-load clothes washers  !CWshr
      REAL*4 FCWSHR(mNumBldg,mNumCR-2)   !share of front-load clothes washers  !CWshr
      REAL*4 RTEFFAC(2)
      REAL*4 DECAY,OPCOST(2)
      REAL*4 EQWTN(nClWashTypes,mNumBldg,mNumCR),EQWTR(nClWashTypes,mNumBldg,mNumCR), &  !EqpParam
       TOTEWTN(nClWashClasses,mNumBldg,mNumCR),TOTEWTR(nClWashClasses,mNumBldg,mNumCR)  !EqpParam
      REAL*4 DENOM, DENOM2, SUM,SUM1
      REAL*4 EQCOST,CAPITAL,RETAIL,BASEUSE,BASEMEF
      INTEGER R,F,B,EU,EUPR,RECTY,EQT,TYPE,RECCL,EQC,COUNT,L
      INTEGER RECAR(nClWashTypes),EQTAR(nClWashTypes)  !EqpParam

!*******************************************************************
!   SET EU = 3 TO SEARCH THE CLOTHES WASHER SECTION OF THE DATA
!*******************************************************************
      EU=3
      EUPR=5

!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,mNumCR-2
        PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
      ENDDO

      ALPHA1=-0.50
      BASEMEF=1.14 !RECS base-year stock-average efficiency (clothes washer Modified Energy Factor, or MEF)	!kj - should be 1.14 for top-loading (59% in 2015)or 2.16 for front-loading (41% in 2015)->1.56 weighted average; RSEFF01.txt and RSSTKEFF.txt use top-loading clothes washer machine energy, so using TCW installed base efficiency here
      BASEUSE=4.0125 !Annual energy use (MMBtu) of clothes washers (+ clothes dryers?) in RECS base year, or just average UEC of a single clothes washer?	!kj; used since AEO2008

!     COMPUTE DECAY RATE USED TO COMPUTE LIFE CYCLE COST (FIRST ITERATION ONLY)
      IF(CURITR.EQ.1) THEN
        DECAY = (1-((1+ResDiscountRate)**(-Tenure)))/ResDiscountRate
      ENDIF

!   ZERO OUT ARRAYS
      DO 5 R=1,mNumCR-2
        DO 5 B=1,mNumBldg
          NCWLOAD(CurCalYr,R,B)=0.0
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            TOTEWTN(EQC,B,R)=0.0
            TOTEWTR(EQC,B,R)=0.0
            WTEQCEFFN(CurCalYr,RECCL,B,R)=0.0
            WTEQCEFFR(CurCalYr,RECCL,B,R)=0.0
          ENDDO
 5    CONTINUE

!*******************************************************************
!     CALCULATE OPERATING COSTS
!     CALCULATE LIFE CYCLE COSTS
!     CALCULATE EQUIPMENT WEIGHT & TOTAL EQUIPMENT WEIGHT
!*******************************************************************
      DO 50 R=1,mNumCR-2
        DO 50 B=1,mNumBldg
          DO 50 RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) &
              THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              EQT  =RTEQTYPE(RECTY)
              EQC  =RTTYEQCL(RECTY)
              RECCL=RTCLEUPT(EU)+EQC
              F    =RTFUEL(RECCL)

!     COMPUTE EFFICIENCY FACTOR USED IN COMPUTING OPERATING COST
              IF(RTEQEFF(RECTY).NE.0.0) THEN
                RTEFFAC(1)=RTEQEFF(RECTY)/EQCEFF(CurCalYr,RECCL)
                RTEFFAC(2)=RTEQEFF(RECTY)/RTBASEFF(RECSYear,RECCL)
              ELSE
                RTEFFAC(1)=RTBASEFF(RECSYear,RECCL)
                RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)
              ENDIF

!  If COSTTRSW = 1, use function EQCOST to compute capital
!     cost of new equipment.
!  If COSTTRSW = 0, use constant value from RSMEQP file for capital
!     cost of new equipment.

        IF (COSTTRSW.EQ.1) THEN
          CAPITAL = EQCOST(RECTY,CurCalYr,"CAP")
        ELSE
          CAPITAL = RTEQCOST(RECTY)
        ENDIF

        OPCOST(1)=PRICES(F,R,CurCalYr)*(BASEUSE*(BASEMEF/CWMEF(RECTY))) !new
        OPCOST(2)=PRICES(F,R,CurCalYr)*(BASEUSE*(BASEMEF/CWMEF(RECTY))) !existing

!     CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOR CHANGES
      IF ((CurCalYr.GT.2008).AND. &	!kj - 2008 marks first year before American Clean Energy and Security Act of 2009 (Waxman-Markey bill)?
       (PRICES(4,R,CurCalYr).GT.PRICES(4,R,RECSYear))) THEN
        HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
        ELIGBLE=HRDRATE - 0.07
        IF (ELIGBLE.GT.0.0) THEN
          HRDADJ= ELIGBLE * ((PRICES(4,R,CurCalYr)/PRICES(4,R,RECSYear))**ALPHA1 )
          BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
        ELSE
          BETA1DR(RECTY)=RTECBTA1(RECTY)
        ENDIF
      ELSE
        BETA1DR(RECTY)=RTECBTA1(RECTY)
      ENDIF

   ! CALCULATE LIFE CYCLE COSTS
      LFCY(EQT,B,R,1)=CAPITAL + (OPCOST(1) *DECAY)
      LFCY(EQT,B,R,2)=CAPITAL + (OPCOST(2) *DECAY)

             ! COMPUTE WEIGHTS FOR NEW EQUIPMENT TYPES
                EQWTN(EQT,B,R)=EXP(RTECBIAS(RECTY)+(BETA1DR(RECTY)*CAPITAL)+ &
                 (RTECBTA2(RECTY)*OPCOST(2))+(RTECBTA3(RECTY)*LFCY(EQT,B,R,2)))
                TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)

             ! COMPUTE WEIGHTS FOR REPLACEMENT EQUIPMENT TYPES
                EQWTR(EQT,B,R)=EXP(RTECBIAS(RECTY)+(BETA1DR(RECTY)*CAPITAL)+ &
                 (RTECBTA2(RECTY)*OPCOST(1))+(RTECBTA3(RECTY)*LFCY(EQT,B,R,1)))
                TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)
            ENDIF
          ENDIF
 50   CONTINUE

!*******************************************************************
!     CALCULATE NEW AND REPLACEMENT MARKET SHARES  !CWshr
!*******************************************************************
      DO R=1,mNumCR-2
        DO B=1,mNumBldg
          TCWSHR(B,R)=0.
          FCWSHR(B,R)=0.
          TYPE = RTTYPECT(EU)

!*******************************************************************
!     RECTY          = RECORD NUMBER IN RSMEQP FILE
!     RTTYEUPT(EU)   = LAST RECORD IN SPACE COOLING (EU=2)
!     RTTYEUPT(EU+1) = LAST RECORD IN CLOTHES WASHING (EU=3)
!*******************************************************************
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
              IF (RTCENDIV(RECTY).EQ.R) THEN
                TYPE=TYPE+1
                EQT=RTEQTYPE(RECTY)
                EQC=RTTYEQCL(RECTY)
                RECCL=RTCLEUPT(EU)+EQC
                IF(EQT.LE.4) THEN  !refers to instances of CL_WASH_T in RSMEQP
                  TCWSHR(B,R) = TCWSHR(B,R) + EQWTN(EQT,B,R)
                ELSE  !refers to instances of CL_WASH_F in RSMEQP
                  FCWSHR(B,R) = FCWSHR(B,R) + EQWTN(EQT,B,R)
                ENDIF
              ENDIF !RTCENDIV
            ENDIF !CurCalYr
          ENDDO !RECTY
        ENDDO !B
      ENDDO !R

!*******************************************************************
!     CALCULATE NEW AND REPLACEMENT MARKET SHARES  !CWshr
!*******************************************************************
      DO 70 R=1,mNumCR-2
        DO 70 B=1,mNumBldg
          TYPE = RTTYPECT(EU)

!*******************************************************************
!     RECTY          = RECORD NUMBER IN RSMEQP FILE
!     RTTYEUPT(EU)   = LAST RECORD IN SPACE COOLING (EU=2)
!     RTTYEUPT(EU+1) = LAST RECORD IN CLOTHES WASHING (EU=3)
!*******************************************************************
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
              IF (RTCENDIV(RECTY).EQ.R) THEN
                TYPE=TYPE+1
                EQT=RTEQTYPE(RECTY)
                EQC=RTTYEQCL(RECTY)
                RECCL=RTCLEUPT(EU)+EQC
                IF(EQT.LE.4) THEN  !refers to instances of CL_WASH_T in RSMEQP
                  NEQTSHR(CurCalYr,TYPE,B,R)=(EQWTN(EQT,B,R)/TCWSHR(B,R))*TCW_SHR
                  REQTSHR(CurCalYr,TYPE,B,R)= NEQTSHR(CurCalYr,TYPE,B,R) ! choices the same for CL_WASH
                ELSE  !refers to instances of CL_WASH_F in RSMEQP
                  NEQTSHR(CurCalYr,TYPE,B,R)=(EQWTN(EQT,B,R)/FCWSHR(B,R))*FCW_SHR
                  REQTSHR(CurCalYr,TYPE,B,R)= NEQTSHR(CurCalYr,TYPE,B,R) ! choices the same for CL_WASH
                ENDIF !EQT
              ENDIF !RTCENDIV
            ENDIF !CurCalYr
          ENDDO !RECTY

!*************************************************************************************
!     CALCULATE WEIGHTED EFFICIENCY FOR NEW AND REPLACEMENT CLOTHES WASHING EQUIPMENT
!*************************************************************************************
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC   =RTCLEQCL(RECCL)
            DENOM =0
            DENOM2=0
            COUNT =0

!     TYPE = INDEX FOR TYPE ARRAYS NEQTSHR AND REQTSHR
!     INITIALIZE TO LAST ARRAY POSTION IN PREVIOUS END USE AND THEN COUNT VALID TYPES IN CURRENT END USE
            TYPE = RTTYPECT(EU)

!*******************************************************************
!     RECTY          = RECORD NUMBER IN RSMEQP FILE
!     RTTYEUPT(EU)   = LAST RECORD IN SPACE COOLING (EU=2)
!     RTTYEUPT(EU+1) = LAST RECORD IN CLOTHES WASHING (EU=3)
!*******************************************************************
            DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
              IF(CurCalYr.GE.RTINITYR(RECTY) &
                .AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
                TYPE=TYPE+1
                IF(RTTYEQCL(RECTY).EQ.EQC) THEN
                  COUNT=COUNT+1
                  EQT=RTEQTYPE(RECTY)
                  RECAR(COUNT)=RECTY
                  EQTAR(COUNT)=TYPE
                  DENOM=DENOM+NEQTSHR(CurCalYr,TYPE,B,R)
                  DENOM2=DENOM2+REQTSHR(CurCalYr,TYPE,B,R)
                ENDIF
              ENDIF
             ENDIF
            ENDDO

!    COMPLETE CALCULATION FOR NEW EQUIPMENT

            IF(DENOM.LE.0.0) THEN
              WTEQCEFFN(CurCalYr,RECCL,B,R)=RTBASEFF(RECSYear,RECCL)
            ELSE
              SUM=0.0
              SUM1=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                 SUM =SUM+(NEQTSHR(CurCalYr,TYPE,B,R)*RTEQEFF(RECTY))
                 SUM1=SUM1+(NEQTSHR(CurCalYr,TYPE,B,R)*LOADADJ(RECTY))
              ENDDO
              WTEQCEFFN(CurCalYr,RECCL,B,R)=SUM/DENOM
              NCWLOAD(CurCalYr,R,B)=SUM1/DENOM
            ENDIF

!     COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT

            IF(DENOM2.LE.0.0) THEN
              WTEQCEFFR(CurCalYr,RECCL,B,R)=RTBASEFF(RECSYear,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                SUM=SUM+(REQTSHR(CurCalYr,TYPE,B,R)*RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFR(CurCalYr,RECCL,B,R)=SUM/DENOM2
            ENDIF

          ENDDO

 70   CONTINUE
      END SUBROUTINE RCWTEC


!*******************************************************************
!     CLOTHES WASHERS ADDED SUBROUTINE
!*******************************************************************
      SUBROUTINE RCWADD
      IMPLICIT NONE
      REAL*4 LOADTOT(RECSYear:EndYr,mNumRTCl,mNumCR,mNumBldg)
      REAL*4 SA, HSR, ESR, SVRTE
      INTEGER EU,EQC,RECCL,Y,R,B,TEMP,EV,EQT,NUMEQT,TYPE, &
              RECTY,V
!*******************************************************************
!   EU = 3 IS CLOTHES WASHERS
!*******************************************************************
      EV       = 3
      EU       = 3
!*******************************************************************
!  CALCULATE CLOTHES WASHERS ADDED IN CurCalYr (CurCalYr-1)
!  CUMULATE SURVIVING EQUIPMENT REPLACED FOR RECS-YEAR VINTAGE PRIOR TO CurCalYr
!*******************************************************************
! CUMULATE SURVIVING NEW CLOTHES WASHERS ADDED PRIOR TO CurCalYr TO ESTIMATE NH
! SA REPRESENTS NH at CurCalYr-1
! CUMULATE SURVIVING NEW WASHERS ADDED & REPLACED PRIOR TO CurCalYr
! REPLACEMENT EQUIPMENT = SURV.HOUSES(SA) - SURV.EQUIP(EQCSUR-CLOTHES WASHERS)
!*******************************************************************
      DO 5 R=1,mNumCR-2
        DO 5 B=1,mNumBldg
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            EQCSR90(CurCalYr,RECCL,B,R)=0.0
            EQCSUR(CurCalYr,RECCL,B,R)=0.0
            EQCRP90RP(CurCalYr,RECCL,B,R)=0.0
            ECWLOAD(CurCalYr,R,B) = 0.0
          ENDDO

       IF (CurCalYr.GT.RECSYear+1) THEN
          WASHNEW(CurCalYr,B,R)=WASHNEW(CurCalYr-1,B,R)*1.0000 !Average annual penetration rate of clothes washers into new homes (based on newest homes in RECS); penetration rate not increasing based on latest RECS, so set to 1.0000
       ENDIF
       IF (WASHNEW(CurCalYr,B,R).GT.1.0000) THEN
          WASHNEW(CurCalYr,B,R)=1.0000  !Prevents penetration of clothes washers into new homes since RECS year from exceeding 100%
       ENDIF
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQCADD(CurCalYr,RECCL,B,R)=(HSEADD(CurCalYr,B,R)*WASHNEW(CurCalYr,B,R))
      SA=0.0
!******************************************************************
!  Calculate replacement equipment from original base-year stock
!******************************************************************
      IF (CurCalYr.EQ.RECSYear+1) THEN
      EQCRP90(CurCalYr,RECCL,B,R)=(EQCRET(CurCalYr,RECCL)*EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
      ELSE
      EQCRP90(CurCalYr,RECCL,B,R)=((EQCRET(CurCalYr,RECCL)- &
      EQCRET(CurCalYr-1,RECCL))*EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
      ENDIF

!   COMPUTE AND VINTAGE REPLACEMENTS OF REPLACEMENTS

           IF(CurCalYr.GT.RECSYear+1) THEN
              DO Y=RECSYear+1,CurCalYr-1
                TEMP=CurCalYr-Y
              ESR=SVRTE(RTALPHA(RECCL),TEMP-1,RTK(RECCL),RTLAMBDA(RECCL)) &
                   -SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
         EQCRP90RP(CurCalYr,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R) + &
          ( EQCRP90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
            EQCRP90RP(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
              ENDDO
           ENDIF

           IF(CurCalYr.GT.RECSYear+1) THEN
              DO Y=RECSYear+1,CurCalYr-1
                TEMP=CurCalYr-Y
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
                EQCSR90(CurCalYr,RECCL,B,R)=(EQCSR90(CurCalYr,RECCL,B,R) + &
                 (EQCRP90(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
                 EQCRP90RP(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))))
                HSR=HDR(B)**(TEMP)
                SA=(SA+EQCADD(Y,RECCL,B,R)*HSR)
                EQCSUR(CurCalYr,RECCL,B,R)=(EQCSUR(CurCalYr,RECCL,B,R) + &
                 (((EQCADD(Y,RECCL,B,R)+EQCREP(Y,RECCL,B,R))*(HSR*ESR))))
              ENDDO
            ENDIF

!*******************************************************************
! CALCULATE REPLACEMENT CLOTHES WASHERS FOR NEW VINTAGE IN CurCalYr-1
! NOTE: REPLACES WITH LIKE IF NOT SINGLE-FAMILY HOMES
! NOTE: FOR NEW HOUSES (NH) - CurCalYr-1 IS THE LAGGED VALUE
!*******************************************************************
            EQCREP(CurCalYr,RECCL,B,R)=SA-EQCSUR(CurCalYr,RECCL,B,R)

         ENDDO
5     CONTINUE

         DO B=1,mNumBldg
           DO R=1,mNumCR-2
             DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
               EQC=RTCLEQCL(RECCL)
               DO Y=CurCalYr,EndYr  !VINTAGE EQUIPMENT FOR AVERAGE STOCK ACCOUNTING
                 TEMP=Y-CurCalYr
                 HSR=HDR(B)**(TEMP)
                 ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
                 EQR90FUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90(CurCalYr,RECCL,B,R)*ESR*HSR)
                 EQR90RPFUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R)*ESR*HSR)
                 EQREPFUT(CurCalYr,Y,RECCL,B,R)=(EQCREP(CurCalYr,RECCL,B,R)*ESR*HSR)
                 EQADDFUT(CurCalYr,Y,RECCL,B,R)=(EQCADD(CurCalYr,RECCL,B,R)*ESR*HSR)
                 EQCESEFUT(CurCalYr,Y,RECCL,B,R)=(EQCESE(CurCalYr,RECCL,B,R)*ESR*HSR)
               ENDDO
             ENDDO
           ENDDO
         ENDDO

!*******************************************************************
!     AGGREGATE CLOTHES WASHERS FOR INVESTMENT ANALYSIS
!*******************************************************************
      Y=CurCalYr
      NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)

      DO B=1,mNumBldg
       DO r=1,mNumCR-2
        TYPE=RTTYPECT(EU)

!*******************************************************************
!     RECTY          = RECORD NUMBER IN RSMEQP FILE
!     RTTYEUPT(EU)   = LAST RECORD IN SPACE COOLING (EU=2)
!     RTTYEUPT(EU+1) = LAST RECORD IN CLOTHES WASHING (EU=3)
!*******************************************************************
        DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)

!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr

           IF(CurCalYr.GE.RTINITYR(RECTY).AND. &
             CurCalYr.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
               TYPE=TYPE+1                  ! INDEX FOR 'TYPE' VARIABLES
               EQT=RTEQTYPE(RECTY)
               EQC=RTTYEQCL(RECTY)
               RECCL=RTCLEUPT(EU)+EQC
               HEATINGTYPEPURCH(Y,TYPE,B,R,1)=(NEQTSHR(Y,TYPE,B,r)*EQCADD(Y,RECCL,B,r))
               HEATINGTYPEPURCH(Y,TYPE,B,R,2)=(NEQTSHR(Y,TYPE,B,r)*(EQCREP(Y,RECCL,B,r) + EQCRP90RP(Y,RECCL,B,r)) + &
                REQTSHR(Y,TYPE,B,r)*EQCRP90(Y,RECCL,B,r) )
             ENDIF
           ENDIF
          ENDDO
         ENDDO
        ENDDO

       END SUBROUTINE RCWADD


!*******************************************************************
!  CLOTHES WASHER CONSUMPTION
!*******************************************************************
      SUBROUTINE RCWCON
      IMPLICIT NONE
      REAL*4 ALPHA,ef1,ef2,ef3,TEMP,TEMP1
      INTEGER B,E,D,EU,EUPR,RECCL,EQC,F,Y,R

!*******************************************************************
!   SET EU = 3 TO SEARCH THE CLOTHES WASHER SECTION OF THE DATA
!*******************************************************************
      EU = 3
      EUPR=5  !no end use price for washers; map to clothes dryer price (assume usage pattern is similar)
      alpha=0.0;ef1=.5;ef2=.35;ef3=.15

!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,mNumCR-2
        PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
      ENDDO

!*******************************************************************
! COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR RECS-YEAR EQUIPMENT
!*******************************************************************
      IF (CurCalYr.GE.RECSYear+1) THEN
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RTBASEFF(CurCalYr,RECCL)= STKEFF(CurCalYr,RECCL)
        ENDDO
      ENDIF

!*******************************************************************
!  Calculate New, Replacement, and Average UECs
!*******************************************************************
      DO 10 D=1,mNumCR-2
        DO 10 B=1,mNumBldg
          DO 10 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            EQCSUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
             (RTBASEFF(CurCalYr,RECCL)/RTBASEFF(RECSYear,RECCL))
            EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
             (WTEQCEFFN(CurCalYr,RECCL,B,D)/RTBASEFF(RECSYear,RECCL))
            EQCRUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
             (WTEQCEFFR(CurCalYr,RECCL,B,D)/RTBASEFF(RECSYear,RECCL))
            IF (CurCalYr.EQ.RECSYear+1) THEN
              EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
              ECWLOAD(CurCalYr,D,B)=(EQCESE(CurCalYr,RECCL,B,D)+ &
               ((EQCADD(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D))* &
               NCWLOAD(CurCalYr,D,B)))/(EQCESE(CurCalYr,RECCL,B,D)+ &
               EQCADD(CurCalYr,RECCL,B,D)+ EQCRP90(CurCalYr,RECCL,B,D))
            ELSE
              TEMP=0.0
              TEMP1=0.0
              DO Y=RECSYear,CurCalYr-1
                TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
                 EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
                 EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
                TEMP1=TEMP1+ EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
                 EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
                 EQR90RPFUT(Y,CurCalYr,RECCL,B,D)+EQCESEFUT(Y,CurCalYr,RECCL,B,D)
              ENDDO
              IF(TEMP.LE.0.0) THEN
                EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
              ELSE
                EQCAUEC(CurCalYr,RECCL,B,D)=0.0
                DO Y=RECSYear,CurCalYr-1
                  EQCAUEC(CurCalYr,RECCL,B,D)=EQCAUEC(CurCalYr,RECCL,B,D)+ &
                   ((EQR90FUT(Y,CurCalYr,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D))+ &
                   ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
                   EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*EQCNUEC(Y,RECCL,B,D)))/TEMP
                  ECWLOAD(CurCalYr,D,B)=ECWLOAD(CurCalYr,D,B)+ &
                   (EQCESEFUT(Y,CurCalYr,RECCL,B,D)+((EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
                   EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
                   EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*NCWLOAD(Y,D,B)))/TEMP1
                ENDDO
              ENDIF
            ENDIF
      10  CONTINUE

!*******************************************************************
!  CALCULATE AVERAGE EFFICIENCY
!*******************************************************************
      DO 30 B=1,mNumBldg
        DO 30 D=1,mNumCR-2
          DO 30 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            IF (CurCalYr.EQ.RECSYear+1) THEN
              WTEQCEFFA(RECSYear+1,RECCL,B,D)=WTEQCEFFN(RECSYear+1,RECCL,B,D)
            ELSE
              TEMP=0.0
              DO Y=RECSYear,CurCalYr-1
                TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
                EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
                EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
              ENDDO
              IF (TEMP .GT. 0.0) THEN
                WTEQCEFFA(CurCalYr,RECCL,B,D)=0.0
                DO Y=RECSYear,CurCalYr-1
                  WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFA(CurCalYr,RECCL,B,D)+ &
                   ((EQR90FUT(Y,CurCalYr,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))+ &
                   ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
                   EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D)))/TEMP
                ENDDO
              ELSE
                WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFN(CurCalYr,RECCL,B,D)
              ENDIF
            ENDIF
      30  CONTINUE

!*******************************************************************
!  Calculate Clothes Washer Consumption
!*******************************************************************
      DO 40 D=1,mNumCR-2
        CSWCON(CurIYr,D)=0.0
       DO 40 B=1,mNumBldg
        CSWCONIN(CurIYr,D,B)=0.
        Driver2(CurIYr,d,B)=0.
        CSWCONWT(CurIYr,D,B)=0.
40    CONTINUE

      DO 50 D=1,mNumCR-2
        DO 50 B=1,mNumBldg
          DO 50 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            F  =RTFUEL(RECCL)
            IF (CurCalYr.EQ.RECSYear+1) THEN
              CSWCON(CurIYr,D)=CSWCON(CurIYr,D)+LEAPYR* &
               (((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))* &
               RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

              CSWCONWT(CurIYr,D,B)=CSWCONWT(CurIYr,D,B)+LEAPYR* &
               (((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
               (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))* &
               RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

              IF(EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
               EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D).GT.0.) THEN
                CSWCONIN(CurIYr,D,B)=CSWCONIN(CurIYr,D,B)+ &
                 ((((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
                 (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
                 (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
                 (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))))

                Driver2(CurIYr,d,B)=Driver2(CurIYr,d,B)+ &
                 (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
                 EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D))
              ENDIF

              EQCEQCN(CurIYr,RECCL,B,D)= LEAPYR* &
               (((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
               (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))* &
               RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

            ELSE
              CSWCON(CurIYr,D)=CSWCON(CurIYr,D)+ LEAPYR* &
               (((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
               (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
               (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)))* &
               RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

              CSWCONWT(CurIYr,D,B)=CSWCONWT(CurIYr,D,B)+LEAPYR* &
               (((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
               (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
               (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)))* &
               RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

               IF(EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
                EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
                EQCSR90(CurCalYr,RECCL,B,D)+EQCREP(CurCalYr,RECCL,b,D)+ &
                EQCSUR(CurCalYr,RECCL,B,D).GT.0.) THEN
                 CSWCONIN(CurIYr,D,B)=CSWCONIN(CurIYr,D,B)+ &
                  ((((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
                  (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
                  (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
                  (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
                  (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
                  (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
                  (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)))))

                 Driver2(CurIYr,D,B)=Driver2(CurIYr,d,B)+ &
                  (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
                  EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
                  EQCSR90(CurCalYr,RECCL,B,D)+EQCREP(CurCalYr,RECCL,b,D)+ &
                  EQCSUR(CurCalYr,RECCL,B,D))
               ENDIF
 
               EQCEQCN(CurIYr,RECCL,B,D)= LEAPYR* &
                (((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
                (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
                (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
                (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
                (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
                (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
                (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)))* &
                RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))
            ENDIF

      50  CONTINUE

      DO R=1,mNumCR-2
        DO B=1,mNumBldg
          IF (Driver2(CurIYr,R,B).GT.0) &
            CSWCONIN(CurIYr,R,B)= &
             CSWCONIN(CurIYr,R,B)/Driver2(CurIYr,R,B)
        ENDDO
      ENDDO

END SUBROUTINE RCWCON


!*******************************************************************
!     DISHWASHER CHOICE SUBROUTINE
!*******************************************************************
      SUBROUTINE RDWTEC
      IMPLICIT NONE
      REAL*4 RTEFFAC(2)
      REAL*4 DECAY,OPCOST(2)
      REAL*4 EQWTN(nDishTypes,mNumBldg,mNumCR),EQWTR(nDishTypes,mNumBldg,mNumCR), &  !EqpParam
       TOTEWTN(nDishClasses,mNumBldg,mNumCR),TOTEWTR(nDishClasses,mNumBldg,mNumCR)  !EqpParam
      REAL*4 DENOM, DENOM2, SUM,SUM1
      REAL*4 EQCOST,CAPITAL,RETAIL
      INTEGER R,F,B,EU,EUPR,RECTY,EQT,TYPE,RECCL,EQC,COUNT,L
      INTEGER RECAR(nDishTypes),EQTAR(nDishTypes)  !EqpParam

!*******************************************************************
!   SET EU = 4 TO SEARCH THE DISHWASHER SECTION OF THE DATA
!*******************************************************************
      EU=4
      EUPR=9

!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,mNumCR-2
        PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
      ENDDO

      ALPHA1=-0.50

      IF(CURITR.EQ.1) THEN
        DECAY = (1-((1+ResDiscountRate)**(-Tenure)))/ResDiscountRate
      ENDIF

!   ZERO OUT ARRAYS
      DO 5 R=1,mNumCR-2
        DO 5 B=1,mNumBldg
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            TOTEWTN(EQC,B,R)=0.0
            TOTEWTR(EQC,B,R)=0.0
            WTEQCEFFN(CurCalYr,RECCL,B,R)=0.0
            WTEQCEFFR(CurCalYr,RECCL,B,R)=0.0
          ENDDO
 5    CONTINUE

!*******************************************************************
!     CALCULATE OPERATING COSTS
!     CALCULATE LIFE CYCLE COSTS
!     CALCULATE EQUIPMENT WEIGHT & TOTAL EQUIPMENT WEIGHT
!*******************************************************************
      DO 50 R=1,mNumCR-2
        DO 50 B=1,mNumBldg
          DO 50 RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) &
              THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              EQT  =RTEQTYPE(RECTY)
              EQC  =RTTYEQCL(RECTY)
              RECCL=RTCLEUPT(EU)+EQC
              F    =RTFUEL(RECCL)

!     COMPUTE EFFICIENCY FACTOR USED IN COMPUTING OPERATING COST
              IF(RTEQEFF(RECTY).NE.0.0) THEN
                RTEFFAC(1)=EQCEFF(CurCalYr,RECCL)/RTEQEFF(RECTY)
                RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)/RTEQEFF(RECTY)
              ELSE
                RTEFFAC(1)=RTBASEFF(RECSYear,RECCL)
                RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)
              ENDIF

!  If COSTTRSW = 1, use function EQCOST to compute capital
!     cost of new equipment.
!  If COSTTRSW = 0, use constant value from RSMEQP file for capital
!     cost of new equipment.

        IF (COSTTRSW.EQ.1) THEN
          CAPITAL = EQCOST(RECTY,CurCalYr,"CAP")
        ELSE
          CAPITAL = RTEQCOST(RECTY)
        ENDIF

        OPCOST(1)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B)*RTEFFAC(1)
        OPCOST(2)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B)*RTEFFAC(2)

!     CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOR CHANGES

      IF ((CurCalYr.GT.2008).AND. &	!kj - 2008 marks first year before American Clean Energy and Security Act of 2009 (Waxman-Markey bill)?
               (PRICES(4,R,CurCalYr).GT.PRICES(4,R,RECSYear))) THEN
       HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
       ELIGBLE=HRDRATE - 0.07
        IF (ELIGBLE.GT.0.0) THEN
         HRDADJ= ELIGBLE * &
            ((PRICES(4,R,CurCalYr)/PRICES(4,R,RECSYear))**ALPHA1 )

         BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
        ELSE
         BETA1DR(RECTY)=RTECBTA1(RECTY)
        ENDIF
        ELSE
         BETA1DR(RECTY)=RTECBTA1(RECTY)
      ENDIF

!      CALCULATE LIFE CYCLE COSTS

              LFCY(EQT,B,R,1)=CAPITAL + (OPCOST(1) *DECAY)
              LFCY(EQT,B,R,2)=CAPITAL + (OPCOST(2) *DECAY)

                EQWTN(EQT,B,R)=EXP (RTECBIAS(RECTY)+ &
                 (BETA1DR(RECTY)*CAPITAL)+ &
                 (RTECBTA2(RECTY)*OPCOST(2))+ &
                 (RTECBTA3(RECTY)*LFCY(EQT,B,R,2)))
                TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)
                EQWTR(EQT,B,R)=EXP (RTECBIAS(RECTY)+ &
                 (BETA1DR(RECTY)*CAPITAL)+ &
                 (RTECBTA2(RECTY)*OPCOST(1))+ &
                 (RTECBTA3(RECTY)*LFCY(EQT,B,R,1)))
                TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)
            ENDIF
           ENDIF
 50   CONTINUE
!*******************************************************************
!     CALCULATE NEW AND REPLACEMENT MARKET SHARES
!*******************************************************************
      DO 70 R=1,mNumCR-2
        DO 70 B=1,mNumBldg
          TYPE = RTTYPECT(EU)

!*******************************************************************
!     RECTY          = RECORD NUMBER IN RSMEQP FILE
!     RTTYEUPT(EU)   = LAST RECORD IN CLOTHES WASHING (EU=3)
!     RTTYEUPT(EU+1) = LAST RECORD IN DISHWASHING (EU=4)
!*******************************************************************
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) &
              THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              TYPE=TYPE+1
              EQT=RTEQTYPE(RECTY)
              EQC=RTTYEQCL(RECTY)
              RECCL=RTCLEUPT(EU)+EQC

                  NEQTSHR(CurCalYr,TYPE,B,R)=(EQWTN(EQT,B,R)/ &
                  TOTEWTN(EQC,B,R))
                  REQTSHR(CurCalYr,TYPE,B,R)=(EQWTR(EQT,B,R)/ &
                  TOTEWTR(EQC,B,R))
             ENDIF
            ENDIF
          ENDDO

!*******************************************************************
!     CALCULATE WEIGHTED EFFICIENCY FOR NEW EQUIPMENT AND
!     CALCULATE WEIGHTED EFFICIENCY FOR REPLACEMENT EQUIPMENT
!*******************************************************************
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC   =RTCLEQCL(RECCL)
            DENOM =0
            DENOM2=0
            COUNT =0

!     TYPE = INDEX FOR TYPE ARRAYS NEQTSHR AND REQTSHR
!            INITIALIZE TO LAST ARRAY POSTION IN PREVIOUS END USE
!              AND THEN COUNT VALID TYPES IN CURRENT END USE

            TYPE = RTTYPECT(EU)

!*******************************************************************
!     RECTY          = RECORD NUMBER IN RSMEQP FILE
!     RTTYEUPT(EU)   = LAST RECORD IN CLOTHES WASHING (EU=3)
!     RTTYEUPT(EU+1) = LAST RECORD IN DISHWASHING (EU=4)
!*******************************************************************
            DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
              IF(CurCalYr.GE.RTINITYR(RECTY) &
                .AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
                TYPE=TYPE+1
                IF(RTTYEQCL(RECTY).EQ.EQC) THEN
                  COUNT=COUNT+1
                  EQT=RTEQTYPE(RECTY)
                  RECAR(COUNT)=RECTY
                  EQTAR(COUNT)=TYPE
                  DENOM=DENOM+NEQTSHR(CurCalYr,TYPE,B,R)
                  DENOM2=DENOM2+REQTSHR(CurCalYr,TYPE,B,R)
                ENDIF
              ENDIF
             ENDIF
            ENDDO

!    COMPLETE CALCULATION FOR NEW EQUIPMENT

            IF(DENOM.LE.0.0) THEN
              WTEQCEFFN(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
            ELSE
              SUM=0.0
              SUM1=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                 SUM=SUM+(NEQTSHR(CurCalYr,TYPE,B,R)/RTEQEFF(RECTY))
                SUM1=SUM1+(NEQTSHR(CurCalYr,TYPE,B,R)*LOADADJ(RECTY))
              ENDDO
              WTEQCEFFN(CurCalYr,RECCL,B,R)=SUM/DENOM
            ENDIF

!     COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT

            IF(DENOM2.LE.0.0) THEN
              WTEQCEFFR(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                SUM=SUM+(REQTSHR(CurCalYr,TYPE,B,R)/RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFR(CurCalYr,RECCL,B,R)=SUM/DENOM2
            ENDIF

          ENDDO

 70   CONTINUE
      END SUBROUTINE RDWTEC


!*******************************************************************
!     DISHWASHERS ADDED SUBROUTINE
!*******************************************************************
      SUBROUTINE RDWADD
      IMPLICIT NONE
      REAL*4 SA, HSR, ESR, SVRTE
      INTEGER EU,EQC,RECCL,Y,R,B,TEMP,EV,EQT,NUMEQT,TYPE, &
              RECTY,V
!*******************************************************************
!   EU       = 4 IS DISHWASHERS
!*******************************************************************
      EV       = 4
      EU       = 4
!*******************************************************************
!  CALCULATE DISHWASHERS ADDED IN CurCalYr (CurCalYr-1)
!  CUMULATE SURVIVING EQUIPMENT REPLACED FOR RECS-YEAR VINTAGE PRIOR TO
!   CurCalYr
!*******************************************************************
! CUMULATE SURVIVING NEW DISHWASHERS ADDED PRIOR TO CurCalYr
!   TO ESTIMATE NH
! SA REPRESENTS NH at CurCalYr-1
! CUMULATE SURVIVING NEW DISHWASHERS ADDED & REPLACED PRIOR TO CurCalYr
! REPLACEMENT EQUIPMENT = SURV.HOUSES(SA) - SURV.EQUIP(EQCSUR-DISHWASHERS)
!*******************************************************************
      DO 5 R=1,mNumCR-2
        DO 5 B=1,mNumBldg
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            EQCSR90(CurCalYr,RECCL,B,R)=0.0
            EQCRP90RP(CurCalYr,RECCL,B,R)=0.0
            EQCSUR(CurCalYr,RECCL,B,R)=0.0
          ENDDO

          IF (CurCalYr.GT.RECSYear+1) THEN
            DISHNEW(CurCalYr,B,R)=DISHNEW(CurCalYr-1,B,R)*1.0055 !Average annual penetration rate of dishwashers into new homes (based on newest homes in RECS)  !DISHNEWpen
          ENDIF

          IF (DISHNEW(CurCalYr,B,R).GT.1.0000) THEN
            DISHNEW(CurCalYr,B,R)=1.0000  !Prevents penetration of dishwashers into new homes since RECS year from exceeding 100%
          ENDIF

          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQCADD(CurCalYr,RECCL,B,R)=(HSEADD(CurCalYr,B,R)*DISHNEW(CurCalYr,B,R))
            SA=0.0
            IF ((EQCND90(CurCalYr,RECCL,B,R)/EH(CurCalYr,B,R)).GE.0.90) THEN  !Max 90% dishwasher penetration into remaining RECSYear housing stock  !RSMISCpen
              EQCND90(CurCalYr,RECCL,B,R)=(EQCESE(RECSYear,RECCL,B,R)*HDR(B)**(CurCalYr-(RECSYear))- &  !RSMISCpen
                                           EQCESE(RECSYear,RECCL,B,R)*HDR(B)**(CurCalYr-(RECSYear)))  !RSMISCpen
            ELSE  !RSMISCpen
              EQCND90(CurCalYr,RECCL,B,R)=(EQCESE(RECSYear,RECCL,B,R)*HDR(B)**(CurCalYr-(RECSYear))*(1.+DWPR(B,R))- &
                                           EQCESE(RECSYear,RECCL,B,R)*HDR(B)**(CurCalYr-(RECSYear)))
            ENDIF !90% dishwasher penetration into remaining RECSYear housing stock  !RSMISCpen

!******************************************************************
!  Calculate replacement equipment from original base-year stock
!******************************************************************
      IF (CurCalYr.EQ.RECSYear+1) THEN
        EQCRP90(CurCalYr,RECCL,B,R)=(EQCRET(CurCalYr,RECCL)*EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
      ELSE
        EQCRP90(CurCalYr,RECCL,B,R)=((EQCRET(CurCalYr,RECCL)- &
        EQCRET(CurCalYr-1,RECCL))*EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
      ENDIF

!   COMPUTE AND VINTAGE REPLACEMENTS OF REPLACEMENTS
           IF(CurCalYr.GT.RECSYear+1) THEN
              DO Y=RECSYear+1,CurCalYr-1
                TEMP=CurCalYr-Y
              ESR=SVRTE(RTALPHA(RECCL),TEMP-1,RTK(RECCL),RTLAMBDA(RECCL)) &
                   -SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
         EQCRP90RP(CurCalYr,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R) + &
          ( EQCRP90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
            EQCRP90RP(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+&
            EQCND90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
              ENDDO
           ENDIF
         EQCRP90RP(CurCalYr,RECCL,B,R)=EQCRP90RP(CurCalYr,RECCL,B,R) + &
                                     EQCND90(CurCalYr,RECCL,B,R)

           IF(CurCalYr.GT.RECSYear+1) THEN
              DO Y=RECSYear+1,CurCalYr-1
                TEMP=CurCalYr-Y
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL), &
                          RTLAMBDA(RECCL))
                EQCSR90(CurCalYr,RECCL,B,R)=(EQCSR90(CurCalYr,RECCL,B,R) + &
             ( EQCRP90(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
               EQCRP90RP(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP)) +&
               EQCND90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
                HSR=HDR(B)**(TEMP)
                SA = (SA + EQCADD(Y,RECCL,B,R)*HSR)
                EQCSUR(CurCalYr,RECCL,B,R) = (EQCSUR(CurCalYr,RECCL,B,R) + &
             ( ((EQCADD(Y,RECCL,B,R)+EQCREP(Y,RECCL,B,R))* &
                 (HSR*ESR)) ))
              ENDDO
            ENDIF

!*******************************************************************
! CALCULATE REPLACEMENT DISHWASHERS FOR NEW VINTAGE IN CurCalYr-1
! NOTE: REPLACES WITH LIKE IF NOT SINGLE-FAMILY HOMES
! NOTE: FOR NEW HOUSES (NH) - CurCalYr-1 IS THE LAGGED VALUE
!*******************************************************************
            EQCREP(CurCalYr,RECCL,B,R)=SA-EQCSUR(CurCalYr,RECCL,B,R)

         ENDDO
5     CONTINUE

         DO B=1,mNumBldg
           DO R=1,mNumCR-2
             DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
                EQC=RTCLEQCL(RECCL)
               DO Y=CurCalYr,EndYr       ! VINTAGE EQUIPMENT FOR AVERAGE STOCK ACCOUNTING
                TEMP=Y-CurCalYr
                HSR=HDR(B)**(TEMP)
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL), &
                          RTLAMBDA(RECCL))
                EQR90FUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90(CurCalYr,RECCL,B,R) &
                 *ESR*HSR)
            EQR90RPFUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R) &
                 *ESR*HSR)
                EQREPFUT(CurCalYr,Y,RECCL,B,R)=(EQCREP(CurCalYr,RECCL,B,R) &
                 *ESR*HSR)
                EQADDFUT(CurCalYr,Y,RECCL,B,R)=(EQCADD(CurCalYr,RECCL,B,R) &
                 *ESR*HSR)
               ENDDO
             ENDDO
           ENDDO
         ENDDO
!*******************************************************************
!     AGGREGATE DISHWASHERS FOR INVESTMENT ANALYSIS
!*******************************************************************
      Y=CurCalYr
      NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)

      DO B=1,mNumBldg
       DO r=1,mNumCR-2
        TYPE=RTTYPECT(EU)

!*******************************************************************
!     RECTY          = RECORD NUMBER IN RSMEQP FILE
!     RTTYEUPT(EU)   = LAST RECORD IN CLOTHES WASHING (EU=3)
!     RTTYEUPT(EU+1) = LAST RECORD IN DISHWASHING (EU=4)
!*******************************************************************
        DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)

!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
           IF(CurCalYr.GE.RTINITYR(RECTY).AND. &
             CurCalYr.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN

            TYPE=TYPE+1                  ! INDEX FOR 'TYPE' VARIABLES
            EQT=RTEQTYPE(RECTY)
            EQC=RTTYEQCL(RECTY)
            RECCL=RTCLEUPT(EU)+EQC
                HEATINGTYPEPURCH(Y,TYPE,B,R,1)=(NEQTSHR(Y,TYPE,B,r)*EQCADD(Y,RECCL,B,r))
                HEATINGTYPEPURCH(Y,TYPE,B,R,2)=(NEQTSHR(Y,TYPE,B,r)*(EQCREP(Y,RECCL,B,r) + EQCRP90RP(Y,RECCL,B,r)) + &
                                               REQTSHR(Y,TYPE,B,r)*EQCRP90(Y,RECCL,B,r) )
             ENDIF
           ENDIF
         ENDDO
         ENDDO
        ENDDO

       END SUBROUTINE RDWADD


!*******************************************************************
!  DISHWASHER CONSUMPTION
!*******************************************************************
      SUBROUTINE RDWCON
      IMPLICIT NONE
      REAL*4 ALPHA,ef1,ef2,ef3,TEMP
      INTEGER B, E, D,EU,EUPR,RECCL,EQC,F,Y,R
!*******************************************************************
!   SET EU = 4 TO SEARCH THE DISHWASHER SECTION OF THE DATA
      EU = 4
      EUPR=9
      alpha=0.0;ef1=.5;ef2=.35;ef3=.15

!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,mNumCR-2
        PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
      ENDDO

!*******************************************************************
! COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR RECS-YEAR EQUIPMENT
!*******************************************************************
      IF (CurCalYr.GE.RECSYear+1) THEN
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RTBASEFF(CurCalYr,RECCL)= STKEFF(CurCalYr,RECCL)
        ENDDO
      ENDIF

!*******************************************************************
!  Calculate New, REPLACEMENT, AND Average UECS
!*******************************************************************
      DO 10 D=1,mNumCR-2
        DO 10 B=1,mNumBldg
          DO 10 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            EQCSUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
                   ( RTBASEFF(RECSYear,RECCL) / RTBASEFF(CurCalYr,RECCL) )
            EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
             ( WTEQCEFFN(CurCalYr,RECCL,B,D)* RTBASEFF(RECSYear,RECCL))
            EQCRUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
             ( WTEQCEFFR(CurCalYr,RECCL,B,D)* RTBASEFF(RECSYear,RECCL))
            IF (CurCalYr.EQ.RECSYear+1) THEN
              EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
         ELSE
         TEMP=0.0
           DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
         EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
         EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
           ENDDO
          IF(TEMP.LE.0.0) THEN
             EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
          ELSE
          EQCAUEC(CurCalYr,RECCL,B,D)=0.0
        DO Y=RECSYear,CurCalYr-1
             EQCAUEC(CurCalYr,RECCL,B,D)=EQCAUEC(CurCalYr,RECCL,B,D)+( &
       (EQR90FUT(Y,CurCalYr,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
         EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*EQCNUEC(Y,RECCL,B,D)))/TEMP
        ENDDO
           ENDIF
          ENDIF
 10   CONTINUE

!*******************************************************************
!  CALCULATE AVERAGE EFFICIENCY
!*******************************************************************
      DO 30 B=1,mNumBldg
        DO 30 D=1,mNumCR-2
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)

            IF (CurCalYr.EQ.RECSYear+1) THEN
              WTEQCEFFA(RECSYear+1,RECCL,B,D)=WTEQCEFFN(RECSYear+1,RECCL,B,D)

            ELSE
             TEMP=0.0
            DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
         EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
         EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
           ENDDO
              IF (TEMP .GT. 0.0) THEN
                WTEQCEFFA(CurCalYr,RECCL,B,D)=0.0
       DO Y=RECSYear,CurCalYr-1
      WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFA(CurCalYr,RECCL,B,D)+( &
       (EQR90FUT(Y,CurCalYr,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
         EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D))) &
                                           /TEMP
       ENDDO
              ELSE
                WTEQCEFFA(CurCalYr,RECCL,B,D)= &
                  WTEQCEFFN(CurCalYr,RECCL,B,D)
              ENDIF
            ENDIF
          ENDDO
 30    CONTINUE
!*******************************************************************
!  Calculate DISHWASHER Consumption
!*******************************************************************
      DO 40 D=1,mNumCR-2
        DSWCON(CurIYr,D)=0.0
       DO 40 B=1,mNumBldg
        DSWCONIN(CurIYr,D,B)=0.
        Driver2(CurIYr,d,B)=0.
        DSWCONWT(CurIYr,D,B)=0.
40    CONTINUE

      DO 50 D=1,mNumCR-2
        DO 50 B=1,mNumBldg
          DO 50 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            F  =RTFUEL(RECCL)

            IF (CurCalYr.EQ.RECSYear+1) THEN

              DSWCON(CurIYr,D)=DSWCON(CurIYr,D)+LEAPYR* ( &
             ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
             (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) &
            +(EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) &
             +(EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))) &
            *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

              DSWCONWT(CurIYr,D,B)=DSWCONWT(CurIYr,D,B)+LEAPYR*( &
             ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
             (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) &
            +(EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) &
             +(EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))) &
            *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

           IF(EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+     &
                 EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D).GT.0.) THEN
               DSWCONIN(CurIYr,D,B)=DSWCONIN(CurIYr,D,B)+(  (&
             ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
             (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) &
            +(EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) &
             +(EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))) )

              Driver2(CurIYr,d,B)=Driver2(CurIYr,d,B)+            &
                (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+     &
                 EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D))
         ENDIF

              EQCEQCN(CurIYr,RECCL,B,D)= LEAPYR*( &
             ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
             (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) &
            +(EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) &
             +(EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))) &
            *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

            ELSE

              DSWCON(CurIYr,D)=DSWCON(CurIYr,D)+ LEAPYR*( &
             ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
              (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
              (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
            (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
              (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
              (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) + &
              (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)))* &
             RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

              DSWCONWT(CurIYr,D,B)=DSWCONWT(CurIYr,D,B)+LEAPYR*(           &
             ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
              (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
              (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
            (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
              (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
              (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) + &
              (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)))* &
             RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

       IF(EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+     &
                 EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
                 EQCSR90(CurCalYr,RECCL,b,D)+EQCREP(CurCalYr,RECCL,b,D)+    &
                 EQCSUR(CurCalYr,RECCL,b,D).GT.0.) THEN
              DSWCONIN(CurIYr,D,B)=DSWCONIN(CurIYr,D,B)+(  (&
             ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
              (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
              (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
              (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
              (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
              (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) + &
              (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))))   )

              Driver2(CurIYr,d,B)=Driver2(CurIYr,d,B)+            &
                (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+     &
                 EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
                 EQCSR90(CurCalYr,RECCL,b,D)+EQCREP(CurCalYr,RECCL,b,D)+    &
                 EQCSUR(CurCalYr,RECCL,b,D))
        ENDIF

              EQCEQCN(CurIYr,RECCL,B,D)=LEAPYR* ( &
             ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
              (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
              (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
            (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
              (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)) &
             +(EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) + &
              (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))) &
            *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)  )
          ENDIF
 50   CONTINUE

      DO R=1,mNumCR-2
        DO B=1,mNumBldg
        IF (Driver2(CurIYr,R,B).GT.0)   &
           DSWCONIN(CurIYr,R,B)=                &
           DSWCONIN(CurIYr,R,B)/Driver2(CurIYr,R,B)
        ENDDO
      ENDDO

      END SUBROUTINE RDWCON


!*******************************************************************
!     WATER HEATER CHOICE SUBROUTINE
!*******************************************************************
      SUBROUTINE RWHTEC
      IMPLICIT NONE
      REAL*4 TOTN(mNumBldg,mNumCR)
      REAL*4 EQFSHRN(nWatHtTypes,mNumBldg,mNumCR),EQFSHRR(nWatHtTypes,mNumBldg,mNumCR)  !EqpParam
      REAL*4 EQWTN(nWatHtTypes,mNumBldg,mNumCR),EQWTR(nWatHtTypes,mNumCR,mNumCR)  !EqpParam
      REAL*4 Temp
      REAL*4 OPCOST(2)
      REAL*4 TOTEWTN(nWatHtClasses,mNumBldg,mNumCR),TOTEWTR(nWatHtClasses,mNumBldg,mNumCR)
      REAL*4 RTEFFAC(2),DECAY,ECTEMP,DENOM,SUM,DENOM2
      REAL*4 EQCOST,CAPITAL,RETAIL
      INTEGER EU,EUPR,EUHT,RECTY,RECCL,RECCLHT,R,B,F,EQT,EQC,EQCHT,TYPE,COUNT,L
      INTEGER RECAR(nWatHtTypes),EQTAR(nWatHtTypes)  !EqpParam

      ALPHA1=-0.50

!     COMPUTE DECAY RATE USED TO COMPUTE LIFE CYCLE COST (FIRST ITERATION ONLY)
      IF(CURITR.EQ.1) THEN
        DECAY = (1-((1+ResDiscountRate)**(-Tenure)))/ResDiscountRate
      ENDIF

!*******************************************************************
!   THE SAME GENERAL FORM OF THIS SUBROUTINE WORKS FOR ALL END USES
!     SET EU   = 5 FOR WATER HEATERS
!     SET EUPR = 3 FOR WATER HEATERS END-USE PRICE
!     SET EUHT = 1 FOR SECTIONS USING SPACE HEATING DATA
!*******************************************************************
      EU  = 5
      EUPR= 3
      EUHT= 1

!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,mNumCR-2
        PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
      ENDDO

!   ZERO-OUT ARRAYS
      DO 5 B=1,mNumBldg
        DO 5 R=1,mNumCR-2
          TOTN(B,R)=0.0
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            TOTEWTN(EQC,B,R)=0.0
            TOTEWTR(EQC,B,R)=0.0
            NH2OSH(CurCalYr,EQC,B,R)=0.0
          ENDDO

!    SUM THE HEATER SHARES OVER ALL HEATER CLASSES

          DO RECCL=RTCLEUPT(EUHT)+1,RTCLEUPT(EUHT+1)
            EQCHT=RTCLEQCL(RECCL)
            TOTN(B,R)=TOTN(B,R)+HSYSSHR(CurCalYr,EQCHT,B,R)
          ENDDO
 5    CONTINUE

      DO 100 R=1,mNumCR-2
        DO 100 B=1,mNumBldg

!   CALCULATE WATER HEATER SHARES BY FUEL FOR NEW DISTRIBUTION

          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            DO RECCLHT=RTCLEUPT(EUHT)+1,RTCLEUPT(EUHT+1)

!   ASSUME SAME SHARES FOR WATER HEATERS AS SPACE HEATERS WHOSE
!     RTCLPNTR POINTER POINTS FROM THE SPACE HEATER CLASS TO THE
!     WATER HEATER CLASS

              IF(RTCLPNTR(RECCLHT).EQ.EQC) THEN
                EQCHT=RTCLEQCL(RECCLHT)
              IF (TOTN(B,R).GT.0.0) THEN
                NH2OSH(CurCalYr,EQC,B,R)=NH2OSH(CurCalYr,EQC,B,R) &
                  +(HSYSSHR(CurCalYr,EQCHT,B,R)/TOTN(B,R))
              ELSE
                NH2OSH(CurCalYr,EQC,B,R)=0.0
              ENDIF
              ENDIF
            ENDDO
          ENDDO

!*******************************************************************
!     RECTY          = RECORD NUMBER IN RSMEQP FILE
!     RTTYEUPT(EU)   = LAST RECORD IN DISHWASHING (EU=4)
!     RTTYEUPT(EU+1) = LAST RECORD IN WATER HEATING (EU=5)
!*******************************************************************
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)

!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr

            IF(CurCalYr.GE.RTINITYR(RECTY).AND. &
               CurCalYr.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN

!      FIND VALID INDICES FOR EQUIPMENT CLASS (EQC), EQUIPMENT
!        TYPE (EQT), REC # FOR RECCL FILE (RECCL), AND FUEL TYPE (F)

              EQC   = RTTYEQCL(RECTY)
              EQT   = RTEQTYPE(RECTY)
              RECCL = RTCLEUPT(EU)+EQC
              F     = RTFUEL(RECCL)

!     COMPUTE EFFICIENCY FACTOR USED IN COMPUTING OPERATING COST

              IF(RTEQEFF(RECTY).NE.0.0) THEN
                RTEFFAC(1)=EQCEFF(CurCalYr,RECCL)/RTEQEFF(RECTY)
                RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)/RTEQEFF(RECTY)
              ELSE
                RTEFFAC(1)=RTBASEFF(RECSYear,RECCL)
                RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)
              ENDIF

!  If COSTTRSW = 1, use function EQCOST to compute capital
!     cost of new equipment.
!  If COSTTRSW = 0, use constant value from RSMEQP file for capital
!     cost of new equipment.

        IF (COSTTRSW.EQ.1) THEN
          CAPITAL = EQCOST(RECTY,CurCalYr,"CAP")
        ELSE
          CAPITAL = RTEQCOST(RECTY)
        ENDIF

!      CALCULATE OPERATING COST

              OPCOST(1)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B) &
                    *RTEFFAC(1)
              OPCOST(2)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B) &
                    *RTEFFAC(2)

!      CALCULATE LIFE CYCLE COSTS

              LFCY(EQT,B,R,1)=CAPITAL + (OPCOST(1) *DECAY)
              LFCY(EQT,B,R,2)=CAPITAL + (OPCOST(2) *DECAY)

!     CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOR CHANGES

      IF ((CurCalYr.GT.2008).AND. &
               (PRICES(F,R,CurCalYr).GT.PRICES(F,R,RECSYear))) THEN
       HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
       ELIGBLE=HRDRATE - 0.07
        IF ((ELIGBLE.GT.0.0).AND. &
         (PRICES(F,R,CurCalYr).GT.PRICES(F,R,RECSYear)) )THEN
         HRDADJ= ELIGBLE * &
            ((PRICES(F,R,CurCalYr)/PRICES(F,R,RECSYear))**ALPHA1 )
         BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
        ELSE
         BETA1DR(RECTY)=RTECBTA1(RECTY)
        ENDIF
        ELSE
         BETA1DR(RECTY)=RTECBTA1(RECTY)
       ENDIF

!    COMPUTE WEIGHTS FOR NEW AND REPLACEMENT EQUIPMENT TYPES

              EQWTN(EQT,B,R)= EXP(RTECBIAS(RECTY)+(BETA1DR(RECTY)* &
                        CAPITAL)+(RTECBTA2(RECTY)*OPCOST(2)) + &
                        ( RTECBTA3(RECTY)*LFCY(EQT,B,R,2) ) )
              TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)
              EQWTR(EQT,B,R)= EXP(RTECBIAS(RECTY)+(BETA1DR(RECTY)* &
                        CAPITAL)+(RTECBTA2(RECTY)*OPCOST(1)) + &
                        ( RTECBTA3(RECTY)*LFCY(EQT,B,R,1) ) )
              TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)

            ENDIF
           ENDIF
          ENDDO

!*******************************************************************

          TYPE = RTTYPECT(EU)
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)

!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr

            IF(CurCalYr.GE.RTINITYR(RECTY).AND. &
               CurCalYr.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              TYPE=TYPE+1

!      FIND VALID INDICES FOR EQUIPMENT CLASS (EQC) & EQUIPMENT
!        TYPE (EQT)

              EQC=RTTYEQCL(RECTY)
              EQT=RTEQTYPE(RECTY)

!   SET EQUIPMENT FUEL SHARE (AND NEQTSHR FOR WATER HEATING)

              IF (TOTEWTN(EQC,B,R).GT.0.0) THEN
                EQFSHRN(EQT,B,R)=EQWTN(EQT,B,R)/TOTEWTN(EQC,B,R)
              ELSE
                EQFSHRN(EQT,B,R)=0.0
              ENDIF

              IF (TOTEWTR(EQC,B,R).GT.0.0) THEN
                EQFSHRR(EQT,B,R)=EQWTR(EQT,B,R)/TOTEWTR(EQC,B,R)
              ELSE
                EQFSHRR(EQT,B,R)=0.0
              ENDIF

              NEQTSHR(CurCalYr,TYPE,B,R)=EQFSHRN(EQT,B,R)
              REQTSHR(CurCalYr,TYPE,B,R)=EQFSHRR(EQT,B,R)

          !Diagnostics only:
!          IF ((CurCalYr.GE.2015) .AND. (B.EQ.1) .AND. (B.EQ.1)) THEN	!kj - updated standard due in 2018
!           WRITE(9,'("Water heater new and replacement shares, before revision for 2015 water heater standard",4i5,2e15.4)') CurCalYr, EQC, EQT, TYPE, EQFSHRN(EQT,B,R),EQFSHRR(EQT,B,R)
!          ENDIF

            ENDIF
           ENDIF
          ENDDO

         !Revise weights for 2015 water heater standard  !WHStandard

            IF(CurCalYr.GE.2015) THEN	!kj - updated standard due in 2018
              ! Adjustments for Natural Gas Water Heating (EQC=1); xlRTCLEQCL in RSCLASS
              ! The standard requires a condensing natural gas water heater for capacities greater than 56 gallons, which comprise
              !  approximately 4% of the existing market; if purchased share is less than 4%, revise shares.	!kj - source of 4% share? TSD?
              EQC=1

              IF(EQFSHRN(4,B,R) .LT. .04) THEN  !Values 1-4 represent the current NG_WH xlRTEQTYPE values in RSMEQP, with 4 being the highest efficiency available  !WHStandard
                Temp=(1+(EQFSHRN(4,B,R)-.04)/(1.-EQFSHRN(4,B,R)))
                EQFSHRN(1,B,R)=EQFSHRN(1,B,R)*temp
                EQFSHRN(2,B,R)=EQFSHRN(2,B,R)*temp
                EQFSHRN(3,B,R)=EQFSHRN(3,B,R)*temp
                EQFSHRN(4,B,R)=.04
              ENDIF

              IF(EQFSHRR(4,B,R) .LT. .04) THEN  !Values 1-4 represent the current NG_WH xlRTEQTYPE values in RSMEQP, with 4 being the highest efficiency available  !WHStandard
                Temp=(1+(EQFSHRR(4,B,R)-.04)/(1.-EQFSHRR(4,B,R)))
                EQFSHRR(1,B,R)=EQFSHRR(1,B,R)*temp
                EQFSHRR(2,B,R)=EQFSHRR(2,B,R)*temp
                EQFSHRR(3,B,R)=EQFSHRR(3,B,R)*temp
                EQFSHRR(4,B,R)=.04
              ENDIF

              ! Adjustments for Electric Water Heating (EQC=2); xlRTCLEQCL in RSCLASS	!kj - updated standard due in 2018
              ! The standard requires a heat pump water heater for capacities greater than 56 gallons which comprise
              !  approximately 9% of the existing market; if purchased share is less than 9%, revise shares.	!kj - source of 9% share? TSD?
              EQC=2

              IF(EQFSHRN(8,B,R)+EQFSHRN(9,B,R)+EQFSHRN(10,B,R) .LT. .09) THEN  !Values 5-10 represent the current ELEC_WH and HP_WH xlRTEQTYPE values in RSMEQP, with 8-10 being the heat pump water heaters  !WHStandard
                Temp=(1+(EQFSHRN(8,B,R)+EQFSHRN(9,B,R)+EQFSHRN(10,B,R)-.09)/(1.-EQFSHRN(8,B,R)-EQFSHRN(9,B,R)-EQFSHRN(10,B,R)))
                EQFSHRN(5,B,R)=EQFSHRN(5,B,R)*Temp
                EQFSHRN(6,B,R)=EQFSHRN(6,B,R)*Temp
                EQFSHRN(7,B,R)=EQFSHRN(7,B,R)*Temp
                Temp=.09/(EQFSHRN(8,B,R)+EQFSHRN(9,B,R)+EQFSHRN(10,B,R))
                EQFSHRN(8,B,R)=EQFSHRN(8,B,R)*Temp
                EQFSHRN(9,B,R)=EQFSHRN(9,B,R)*Temp
                EQFSHRN(10,B,R)=EQFSHRN(10,B,R)*Temp
              ENDIF

              IF(EQFSHRR(8,B,R)+EQFSHRR(9,B,R)+EQFSHRR(10,B,R) .LT. .09) THEN  !Values 5-10 represent the current ELEC_WH and HP_WH xlRTEQTYPE values in RSMEQP, with 8-10 being the heat pump water heaters
                Temp=(1+(EQFSHRR(8,B,R)+EQFSHRR(9,B,R)+EQFSHRR(10,B,R)-.09)/(1.-EQFSHRR(8,B,R)-EQFSHRR(9,B,R)-EQFSHRR(10,B,R)))
                EQFSHRR(5,B,R)=EQFSHRR(5,B,R)*Temp
                EQFSHRR(6,B,R)=EQFSHRR(6,B,R)*Temp
                EQFSHRR(7,B,R)=EQFSHRR(7,B,R)*Temp
                Temp=.09/(EQFSHRR(8,B,R)+EQFSHRR(9,B,R)+EQFSHRR(10,B,R))
                EQFSHRR(8,B,R)=EQFSHRR(8,B,R)*Temp
                EQFSHRR(9,B,R)=EQFSHRR(9,B,R)*Temp
                EQFSHRR(10,B,R)=EQFSHRR(10,B,R)*Temp
              ENDIF

              ! Reset Shares
              TYPE = RTTYPECT(EU)
              DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
                IF(CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
                  IF (RTCENDIV(RECTY).EQ.R) THEN
                    TYPE=TYPE+1
                    EQC=RTTYEQCL(RECTY)
                    EQT=RTEQTYPE(RECTY)
                    NEQTSHR(CurCalYr,TYPE,B,R)=EQFSHRN(EQT,B,R)
                    REQTSHR(CurCalYr,TYPE,B,R)=EQFSHRR(EQT,B,R)
                    !Diagnostics only:
!                    IF ((CurCalYr.GE.2015) .AND. (B.EQ.1) .AND. (R.EQ.1)) THEN	!kj - updated standard due in 2018
!                      WRITE(9,'("Water heater new and replacement shares, revised for 2015 water heater standard",4i5,2e15.4)') CurCalYr, EQC, EQT, TYPE, EQFSHRN(EQT,B,R),EQFSHRR(EQT,B,R)
!                    ENDIF
                  ENDIF
                ENDIF
              ENDDO

            ENDIF !Revised WH for 2015 standard

!*******************************************************************
!     CALCULATE WEIGHTED EFFICIENCY FOR NEW EQUIPMENT AND
!     CALCULATE WEIGHTED EFFICIENCY FOR REPLACEMENT EQUIPMENT
!*******************************************************************
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC   =RTCLEQCL(RECCL)
            DENOM =0
            DENOM2=0
            COUNT =0
            DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
              IF(RTTYEQCL(RECTY).EQ.EQC.AND.CurCalYr.GE.RTINITYR(RECTY) &
                .AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
                COUNT=COUNT+1
                EQT=RTEQTYPE(RECTY)
                RECAR(COUNT)=RECTY
                EQTAR(COUNT)=EQT
                DENOM =DENOM +EQFSHRN(EQT,B,R)
                DENOM2=DENOM2+EQFSHRR(EQT,B,R)
              ENDIF
             ENDIF
            ENDDO

!    COMPLETE CALCULATION FOR NEW EQUIPMENT

            IF(DENOM.LE.0.0) THEN
              WTEQCEFFN(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                EQT=EQTAR(L)
                SUM=SUM+(EQFSHRN(EQT,B,R)/RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFN(CurCalYr,RECCL,B,R)=SUM/DENOM
            IF (WTEQCEFFN(CurCalYr,RECCL,B,R).EQ.0.0) THEN
                WTEQCEFFN(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
            ENDIF
            ENDIF

!     COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT

            IF(DENOM2.LE.0.0) THEN
              WTEQCEFFR(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                EQT=EQTAR(L)
                SUM=SUM+(EQFSHRR(EQT,B,R)/RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFR(CurCalYr,RECCL,B,R)=SUM/DENOM2
            ENDIF
            IF (WTEQCEFFR(CurCalYr,RECCL,B,R).EQ.0.0) THEN
                WTEQCEFFR(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
            ENDIF
          ENDDO
100   CONTINUE
      END SUBROUTINE RWHTEC


!*******************************************************************
!     WATER HEATERS ADDED SUBROUTINE
!*******************************************************************
      SUBROUTINE REUADD
      IMPLICIT NONE
      REAL*4 SWT(RECSYear:EndYr),SWF(RECSYear:EndYr),WATERTOT(RECSYear:EndYr,mNumCR-2),COOKTOT(RECSYear:EndYr,mNumCR-2)
      REAL*4 SA, HSR, ESR, SVRTE, SHARE  ,SWFT
      REAL*4 EQSRT(RECSYear:EndYr,nHeatTypes,mNumBldg,mNumCR-2),EQSR90T(RECSYear:EndYr,nHeatTypes,mNumBldg,mNumCR-2)
      INTEGER EQC,RECCL,TEMP,RECCLSW
      INTEGER Y,Y1,R,D, B,EV,TYPE,RECTY,NUMEQT,EQT,V

!   WHEN EU = 5, SEARCH THE WATER HEATING SECTION OF THE DATA
!   WHEN EU = 6, SEARCH THE COOKING SECTION OF THE DATA
      IF (EU.EQ.5) EV = 5
      IF (EU.EQ.6) EV = 6

!*******************************************************************

!  ZERO OUT ARRAYS

      DO 1 R=1,mNumCR-2
        DO 1 B=1,mNumBldg
          DO 1 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            EQCSR90(CurCalYr,RECCL,B,R)=0.0
            EQCRP90RP(CurCalYr,RECCL,B,R)=0.0
            EQCSUR(CurCalYr,RECCL,B,R)=0.0
            IF(B.EQ.1) EQCREP(CurCalYr,RECCL,B,R) = 0.0
 1    CONTINUE

!*******************************************************************
!  Calculate Conventional Equipment Added in CurCalYr
!*******************************************************************
! CUMULATE SURVIVING NEW EQUIPMENT ADDED PRIOR TO CurCalYr
!   TO ESTIMATE NH
! SA REPRESENTS NH at CurCalYr-1
! CUMULATE SURVIVING NEW EQUIPMENT ADDED & REPLACED PRIOR TO CurCalYr
! REPLACEMENT EQUIPMENT = SURV.HOUSES(SA) -  SURV.EQUIP(EQCSUR)
!*******************************************************************

!*******************************************************************
!            INITIALIZE TO LAST ARRAY POSTION IN PREVIOUS END USE
!              AND THEN COUNT VALID TYPES IN CURRENT END USE
!*******************************************************************
        DO 15 R=1,mNumCR-2
         DO 15 B=1,mNumBldg
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQC=RTCLEQCL(RECCL)
           SHARE = 1.0
           IF(EU.EQ.5) SHARE = NH2OSH(CurCalYr,EQC,B,R)
           IF(EU.EQ.6) SHARE = NCKSH(CurCalYr,EQC,B,R)
           EQCADD(CurCalYr,RECCL,B,R)=(HSEADD(CurCalYr,B,R)*SHARE)
           SA = 0.0

!******************************************************************
!  Calculate replacement equipment from original base year stock
!******************************************************************
      IF (CurCalYr.EQ.RECSYear+1) THEN
      EQCRP90(CurCalYr,RECCL,B,R)=(EQCRET(CurCalYr,RECCL) &
        *EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
      ELSE
      EQCRP90(CurCalYr,RECCL,B,R)=((EQCRET(CurCalYr,RECCL)- &
      EQCRET(CurCalYr-1,RECCL))*EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
      ENDIF

!   COMPUTE AND VINTAGE REPLACEMENTS OF REPLACEMENTS
           IF(CurCalYr.GT.RECSYear+1) THEN
              DO Y=RECSYear+1,CurCalYr
                TEMP=CurCalYr-Y
              ESR=SVRTE(RTALPHA(RECCL),TEMP-1,RTK(RECCL),RTLAMBDA(RECCL)) &
                   -SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
         EQCRP90RP(CurCalYr,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R) + &
          ( EQCRP90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
            EQCRP90RP(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
              ENDDO
           ENDIF

           IF(CurCalYr.GT.RECSYear+1) THEN
              DO Y=RECSYear+1,CurCalYr-1
                TEMP=CurCalYr-Y
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL), &
                          RTLAMBDA(RECCL))
                EQCSR90(CurCalYr,RECCL,B,R)=(EQCSR90(CurCalYr,RECCL,B,R) + &
             ( EQCRP90(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
               EQCRP90RP(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))))
                HSR=HDR(B)**(TEMP)
                SA = (SA + EQCADD(Y,RECCL,B,R)*HSR)
                EQCSUR(CurCalYr,RECCL,B,R) = (EQCSUR(CurCalYr,RECCL,B,R) + &
             ( ((EQCADD(Y,RECCL,B,R)+EQCREP(Y,RECCL,B,R))* &
                 (HSR*ESR)) ))
              ENDDO
            ENDIF

!*******************************************************************
! CALCULATE REPLACEMENT WATER HEATERS FOR NEW VINTAGE IN CurCalYr-1
! NOTE: REPLACES WITH LIKE IF NOT SINGLE-FAMILY HOMES
! NOTE: FOR NEW HOUSES (NH) - CurCalYr-1 IS THE LAGGED VALUE
!*******************************************************************
!  SUBROUTINE 'REPLACE' DISTRIBUTES REPLACEMENTS IN POST-RECS-YEAR
!    SINGLE-FAMILY HOMES WHEN LAST ARGUMENT = 1

            IF(B.EQ.1) THEN
!  First, store what replacements would have been if no switching allowed.
               OEQCREP(CurCalYr,RECCL,1,R)=SA-EQCSUR(CurCalYr,RECCL,B,R)
!  Call REPLACE to distribute replacements.
               CALL REPLACE(EU,R,B,RECCL,1)
            ELSE
!  No switching allowed in multifamily or mobile homes.
                EQCREP(CurCalYr,RECCL,B,R)=SA-EQCSUR(CurCalYr,RECCL,B,R)
            ENDIF

         ENDDO
 15     CONTINUE

!  The following call to REPLACE with final argument = 2  distributes
!    replacements in Existing Single-Family Homes.
      B = 1
      DO  R=1,mNumCR-2
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          OEQCRP90(CurCalYr,RECCL,B,R) = EQCRP90(CurCalYr,RECCL,1,R)
          OEQCRP90R(CurCalYr,RECCL,B,R) = EQCRP90RP(CurCalYr,RECCL,1,R)

          CALL REPLACE(EU,R,B,RECCL,2)

        ENDDO
      ENDDO

      B = 1
      DO  R=1,mNumCR-2
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
         SWITCHTO(CurCalYr,RECCL,B,R)=0.0
         SWITCHTOR(CurCalYr,RECCL,B,R)=0.0
          DO RECCLSW=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          IF (RECCLSW.NE.RECCL) THEN
      SWITCHTO(CurCalYr,RECCL,B,R)=SWITCHTO(CurCalYr,RECCL,B,R)+ &
                              EQCSW90(CurCalYr,RECCLSW,RECCL,B,R)
      SWITCHTOR(CurCalYr,RECCL,B,R)=SWITCHTOR(CurCalYr,RECCL,B,R)+ &
                              EQCSW90R(CurCalYr,RECCLSW,RECCL,B,R)
          ENDIF
         ENDDO
       ENDDO
      ENDDO

      B = 1
      DO  R=1,mNumCR-2
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)

      EQCRP90(CurCalYr,RECCL,B,R)= EQCRP90(CurCalYr,RECCL,B,R)- &
        SWITCHES(CurCalYr,RECCL,B,R)
       EQCRP90RP(CurCalYr,RECCL,B,R)= EQCRP90RP(CurCalYr,RECCL,B,R)- &
        SWITCHESR(CurCalYr,RECCL,B,R)+ SWITCHTOR(CurCalYr,RECCL,B,R) &
       + SWITCHTO(CurCalYr,RECCL,B,R)
        ENDDO
      ENDDO
       B=1
         SWF(CurCalYr)=0.0
         SWT(CurCalYr)=0.0
      DO  R=1,mNumCR-2
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      SWT(CurCalYr)=SWT(CurCalYr)+SWITCHTO(CurCalYr,RECCL,B,R)+ &
                             SWITCHTOR(CurCalYr,RECCL,B,R)
      SWF(CurCalYr)=SWF(CurCalYr)+SWITCHES(CurCalYr,RECCL,B,R)+ &
                             SWITCHESR(CurCalYr,RECCL,B,R)
       ENDDO
      ENDDO
      IF (CurCalYr.EQ.EndYr) THEN
          SWFT=0.0
        DO Y=RECSYear+1,EndYr
          SWFT=SWFT+SWF(Y)
        ENDDO
      ENDIF
         DO B=1,mNumBldg
           DO R=1,mNumCR-2
             DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
                EQC=RTCLEQCL(RECCL)
               DO Y=CurCalYr,EndYr       ! VINTAGE EQUIPMENT FOR AVERAGE STOCK ACCOUNTING
                TEMP=Y-CurCalYr
                HSR=HDR(B)**(TEMP)
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL), &
                          RTLAMBDA(RECCL))
                EQR90FUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90(CurCalYr,RECCL,B,R) &
                 *ESR*HSR)
            EQR90RPFUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R) &
                 *ESR*HSR)
                EQREPFUT(CurCalYr,Y,RECCL,B,R)=(EQCREP(CurCalYr,RECCL,B,R) &
                 *ESR*HSR)
                EQADDFUT(CurCalYr,Y,RECCL,B,R)=(EQCADD(CurCalYr,RECCL,B,R) &
                 *ESR*HSR)
               ENDDO
             ENDDO
           ENDDO
         ENDDO

!*******************************************************************
!     AGGREGATE WATER HEATING SYSTEMS FOR INVESTMENT ANALYSIS
!*******************************************************************
      Y=CurCalYr
      NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)

            DO B=1,mNumBldg
             DO r=1,mNumCR-2
              TYPE=RTTYPECT(EU)
              DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
              !     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
              IF(CurCalYr.GE.RTINITYR(RECTY).AND. &
              CurCalYr.LE.RTLASTYR(RECTY)) THEN
              IF (RTCENDIV(RECTY).EQ.R) THEN
              TYPE=TYPE+1      ! INDEX FOR 'TYPE' VARIABLES
            EQT=RTEQTYPE(RECTY)
            EQC=RTTYEQCL(RECTY)
            RECCL=RTCLEUPT(EU)+EQC
                HEATINGTYPEPURCH(Y,TYPE,B,R,1)=(NEQTSHR(Y,TYPE,B,r)*EQCADD(Y,RECCL,B,r))
                HEATINGTYPEPURCH(Y,TYPE,B,R,2)=(NEQTSHR(Y,TYPE,B,r)*(EQCREP(Y,RECCL,B,r) + EQCRP90RP(Y,RECCL,B,r)) + &
                                               REQTSHR(Y,TYPE,B,r)*EQCRP90(Y,RECCL,B,r) )
              ENDIF
              ENDIF
            ENDDO
           ENDDO
        ENDDO

       IF (EU.EQ.5) THEN
         IF ((CurCalYr.EQ.RECSYear+1).AND.(CURITR.EQ.1)) THEN
           DO R=1,mNumCR-2
            WATERTOT(RECSYear,R)=0.0
             DO B=1,mNumBldg
              WATERTOT(RECSYear,R)=WATERTOT(RECSYear,R)+EQCESE(RECSYear,19,B,R)
             ENDDO
           ENDDO
         ELSE
           DO R=1,mNumCR-2
            WATERTOT(CurCalYr,R)=0.0
             DO B=1,mNumBldg
              WATERTOT(CurCalYr,R)=WATERTOT(CurCalYr,R)+EQCESE(CurCalYr,19,B,R)+EQCADD(CurCalYr,19,B,R)+&
                EQCRP90(CurCalYr,19,B,R)+EQCRP90RP(CurCalYr,19,B,R)+EQCSUR(CurCalYr,19,B,R)+EQCREP(CurCalYr,19,B,R)+&
                EQCSR90(CurCalYr,19,B,R)
             ENDDO
           ENDDO
         ENDIF
        ELSE
         IF ((CurCalYr.EQ.RECSYear+1).AND.(CURITR.EQ.1)) THEN
           DO R=1,mNumCR-2
            COOKTOT(RECSYear,R)=0.0
             DO B=1,mNumBldg
              COOKTOT(RECSYear,R)=COOKTOT(RECSYear,R)+EQCESE(RECSYear,24,B,R)
             ENDDO
           ENDDO
         ELSE
           DO R=1,mNumCR-2
            COOKTOT(CurCalYr,R)=0.0
             DO B=1,mNumBldg
              COOKTOT(CurCalYr,R)=COOKTOT(CurCalYr,R)+EQCESE(CurCalYr,24,B,R)+EQCADD(CurCalYr,24,B,R)+&
                EQCRP90(CurCalYr,24,B,R)+EQCRP90RP(CurCalYr,24,B,R)+EQCSUR(CurCalYr,24,B,R)+EQCREP(CurCalYr,24,B,R)+&
                EQCSR90(CurCalYr,24,B,R)
             ENDDO
           ENDDO
         ENDIF
       ENDIF

! Proxy for gas customers is gas water heating for CDs 1,2,7, and 9
        DO R=1,mNumCR-2
         IF ((CurCalYr.EQ.RECSYear+1).AND.(CURITR.EQ.1)) THEN
          IF (EU.EQ.5) THEN
            IF (WATERTOT(RECSYear,R).GT.RSGASCUST(RECSYear,R)) THEN
              RSGASCUST(RECSYear,R)=WATERTOT(RECSYear,R)
            ENDIF
          ELSE
            IF (COOKTOT(RECSYear,R).GT.RSGASCUST(RECSYear,R)) THEN
              RSGASCUST(RECSYear,R)=COOKTOT(RECSYear,R)
            ENDIF
          ENDIF
        ELSE
          IF (EU.EQ.5) THEN
            IF (WATERTOT(CurCalYr,R).GT.RSGASCUST(CurCalYr,R)) THEN
              RSGASCUST(CurCalYr,R)=WATERTOT(CurCalYr,R)
            ENDIF
          ELSE
            IF (COOKTOT(CurCalYr,R).GT.RSGASCUST(CurCalYr,R)) THEN
              RSGASCUST(CurCalYr,R)=COOKTOT(CurCalYr,R)
            ENDIF
          ENDIF
        ENDIF
       ENDDO
      END SUBROUTINE REUADD


!*******************************************************************
!  WATER HEATING CONSUMPTION
!*******************************************************************
      SUBROUTINE RWHCON
      IMPLICIT NONE
      REAL*4 HOUSES(RECSYear:EndYr,mNumCR)
      REAL*4 ALPHA,ef1,ef2,ef3,TEMP,HHSELAS,HHSIZE(RECSYear:EndYr,mNumCR-2)
      REAL*4 CHECK(RECSYear:EndYr,4),DCHECK(RECSYear:EndYr,4)
      INTEGER B, D, EU, EUPR, EQC, RECCL, V, F, FCON,Y,FD,R
      INTEGER RECCLSWH, RECCLEWH,EQCSWH, EQCEWH,EQCCW

!**************************************************************************************
!  WATER HEATING - 1=Natural Gas 2=Electricity 3=Distillate Fuel Oil 4=Propane 5=Solar
!**************************************************************************************

!   SET EU = 5 TO SEARCH THE WATER HEATING SECTON OF THE DATA
      EU     = 5
      EUPR=3
      ALPHA=-.15;ef1=.5;ef2=.35;ef3=.15
      HHSELAS=.315  ! People per house elasticity for hot water use (lbl)	!kj

!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,mNumCR-2
        PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
      ENDDO

!*******************************************************************
! COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR RECS-YEAR EQUIPMENT
!*******************************************************************
      IF (CurCalYr.GE.RECSYear+1) THEN
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RTBASEFF(CurCalYr,RECCL)= STKEFF(CurCalYr,RECCL)
        ENDDO
      ENDIF

!*******************************************************************
!  Calculate New, REPLACEMENT, and Average UEC
!*******************************************************************
      IF(CurCalYr.EQ.RECSYear+1) THEN
        DO 3 D=1,mNumCR-2
          HOUSES(RECSYear,D)=0.0
          DO 3 B=1,mNumBldg
            HOUSES(RECSYear,D)=HOUSES(RECSYear,D)+EH(RECSYear,B,D)
 3      CONTINUE
      ENDIF

      DO 5 D=1,mNumCR-2
        HOUSES(CurCalYr,D)=0.0
        DO 5 B=1,mNumBldg
          HOUSES(CurCalYr,D)=HOUSES(CurCalYr,D)+EH(CurCalYr,B,D)+ &
            NH(CurCalYr,B,D)
 5    CONTINUE

!*******************************************************************
!  Calculate people per housing unit
!*******************************************************************
      IF (CurCalYr.EQ.RECSYear+1) THEN
      DO 6 D=1,mNumCR-2
          HHSIZE(RECSYear,D)=MC_NP16A(D,RECSYear-BaseYr+1)/HOUSES(RECSYear,D)
 6    CONTINUE
      ENDIF

        DO 7 D=1,mNumCR-2
          HHSIZE(CurCalYr,D)=MC_NP16A(D,CurIYr)/HOUSES(CurCalYr,D)
 7      CONTINUE

!*******************************************************************
!  Calculate New, Replacement, and Average UEC
!*******************************************************************
      DO 10 D=1,mNumCR-2
        DO 10 B=1,mNumBldg
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
       EQCSUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B) * &
       ((HHSIZE(CurCalYr,D)/HHSIZE(RECSYear,D))**HHSELAS)* &
        ( RTBASEFF(RECSYear,RECCL) / RTBASEFF(CurCalYr,RECCL) )

       EQCSIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B) * &
        ( RTBASEFF(RECSYear,RECCL) / RTBASEFF(CurCalYr,RECCL) )

      IF (CurCalYr.EQ.RECSYear+1) THEN
         IF (WTEQCEFFN(CurCalYr,RECCL,B,D).GT.0.0) THEN
       EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
       ((HHSIZE(CurCalYr,D)/HHSIZE(RECSYear,D))**HHSELAS) * &
        WTEQCEFFN(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)
       EQCNIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
        WTEQCEFFN(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)
         ELSE
       EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B) * &
          ((HHSIZE(CurCalYr,D)/HHSIZE(RECSYear,D))**HHSELAS)
       EQCNIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
         ENDIF
      ELSE
              IF (WTEQCEFFN(CurCalYr,RECCL,B,D).GT.0.0) THEN
           EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
                 ((HHSIZE(CurCalYr,D)/HHSIZE(RECSYear,D))** HHSELAS)* &
              WTEQCEFFN(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)
           EQCNIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
              WTEQCEFFN(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)
              ELSE
                EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
              ((HHSIZE(CurCalYr,D)/HHSIZE(RECSYear,D))**HHSELAS)
                EQCNIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
              ENDIF
      ENDIF

            IF (WTEQCEFFR(CurCalYr,RECCL,B,D).GT.0.0) THEN
              EQCRUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
       ((HHSIZE(CurCalYr,D)/HHSIZE(RECSYear,D))**HHSELAS)* &
        WTEQCEFFR(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)
              EQCRIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
        WTEQCEFFR(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)
            ELSE
              EQCRUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B) * &
              ((HHSIZE(CurCalYr,D)/HHSIZE(RECSYear,D))**HHSELAS)
              EQCRIUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
            ENDIF

            IF (CurCalYr .EQ. RECSYear+1) THEN
              EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
            ELSE
         TEMP=0.0
           DO Y=RECSYear+1,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
         EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
         EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
           ENDDO
          IF(TEMP.LE.0.0) THEN
             EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
          ELSE
          EQCAUEC(CurCalYr,RECCL,B,D)=0.0
        DO Y=RECSYear+1,CurCalYr-1
             EQCAUEC(CurCalYr,RECCL,B,D)=EQCAUEC(CurCalYr,RECCL,B,D)+( &
       (EQR90FUT(Y,CurCalYr,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
         EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*EQCNUEC(Y,RECCL,B,D)))/TEMP
        ENDDO
           ENDIF
          ENDIF
          ENDDO
 10   CONTINUE

!*******************************************************************
!  CALCULATE AVERAGE EFFICIENCY
!*******************************************************************
      DO 30 B=1,mNumBldg
        DO 30 D=1,mNumCR-2
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)

            IF (CurCalYr.EQ.RECSYear+1) THEN
              WTEQCEFFA(RECSYear+1,RECCL,B,D)=WTEQCEFFN(RECSYear+1,RECCL,B,D)

            ELSE
             TEMP=0.0
            DO Y=RECSYear+1,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
         EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
         EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
           ENDDO
              IF (TEMP .GT. 0.0) THEN
                WTEQCEFFA(CurCalYr,RECCL,B,D)=0.0
       DO Y=RECSYear+1,CurCalYr-1
      WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFA(CurCalYr,RECCL,B,D)+( &
       (EQR90FUT(Y,CurCalYr,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
         EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D)))/TEMP
       ENDDO
              ELSE
                WTEQCEFFA(CurCalYr,RECCL,B,D)= &
                  WTEQCEFFN(CurCalYr,RECCL,B,D)
              ENDIF
            ENDIF
          ENDDO
30    CONTINUE

!*******************************************************************
!  Calculate Water Heating Shares of Dishwashers and Clothes Washers
!*******************************************************************
      DO D=1,mNumCR-2
         DO B=1,mNumBldg
            HOTWATQ(CurCalYr,B,D)=0.0
           DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC  = RTCLEQCL(RECCL)
             HOTWATQ(CurCalYr,B,D)=HOTWATQ(CurCalYr,B,D)+ &
              EQCESE(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D) +&
              EQCRP90RP(CurCalYr,RECCL,B,D)+EQCREP(CurCalYr,RECCL,B,D) +&
              EQCSUR(CurCalYr,RECCL,B,D)+ EQCSR90(CurCalYr,RECCL,B,D)
         ENDDO
        ENDDO
      ENDDO

!*******************************************************************
!  Calculate Water Heating Consumption
!*******************************************************************
      DO 20 D=1,mNumCR-2
         DO 19 B=1,mNumBldg
         DO 19 FCON=1,5  !Natural gas, electricity, distillate fuel oil, propane, solar
          H2OCONWT(CurIYr,FCON,D,B)=0.
          Driver(CurIYr,fcon,d,b)=0.
          H2OCONIN(CurIYr,FCON,D,B)=0.
 19   CONTINUE
        DO 20 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          H2OCON(CurIYr,EQC,D)=0.0
 20   CONTINUE

!*******************************************************************
!  FIND INDICES FOR THE ELECTRIC AND SOLAR WATER HEATERS
!    USED TO COMPUTE H2OCON FOR SOLAR FUEL (FCON=5)
!*******************************************************************
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        IF(RTCLNAME(RECCL).EQ.'ELEC_WH')THEN
          EQCEWH=RTCLEQCL(RECCL)
          RECCLEWH=EQCEWH+RTCLEUPT(EU)
        ELSEIF(RTCLNAME(RECCL).EQ.'SOLAR_WH')THEN
          EQCSWH=RTCLEQCL(RECCL)
          RECCLSWH=EQCSWH+RTCLEUPT(EU)
        ENDIF
      ENDDO

      DO 100 D=1,mNumCR-2
        DO 100 B=1,mNumBldg
          DO 100 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC  = RTCLEQCL(RECCL)
            F    = RTFUEL(RECCL)
            FCON = FWHCON(F)
            IF (CurCalYr.EQ.RECSYear+1) THEN
              H2OCON(CurIYr,FCON,D)=H2OCON(CurIYr,FCON,D)+ LEAPYR* &
               (((1.-CWLOAD(RECSYear))*EQCSUEC(CurCalYr,RECCL,B,D)+ &
               (EQCSUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
               ECWLOAD(CurCalYr,D,B)))*EQCESE(CurCalYr,RECCL,B,D) + &
               ((1.-CWLOAD(RECSYear))*EQCNUEC(CurCalYr,RECCL,B,D)+ &
               (EQCNUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)* &
               NCWLOAD(CurCalYr,D,B)))*EQCADD(CurCalYr,RECCL,B,D) + &
               ((1.-CWLOAD(RECSYear))*EQCRUEC(CurCalYr,RECCL,B,D)+ &
               (EQCRUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)* &
               NCWLOAD(CurCalYr,D,B)))*EQCRP90(CurCalYr,RECCL,B,D))* &
               RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

               H2OCONWT(CurIYr,FCON,D,B)=H2OCONWT(CurIYr,FCON,D,B)+LEAPYR* &
                (((1.-CWLOAD(RECSYear))*EQCSUEC(CurCalYr,RECCL,B,D)+ &
                (EQCSUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
                ECWLOAD(CurCalYr,D,B)))*EQCESE(CurCalYr,RECCL,B,D)+ &
                ((1.-CWLOAD(RECSYear))*EQCNUEC(CurCalYr,RECCL,B,D)+ &
                (EQCNUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)* &
                NCWLOAD(CurCalYr,D,B)))*EQCADD(CurCalYr,RECCL,B,D)+ &
                ((1.-CWLOAD(RECSYear))*EQCRUEC(CurCalYr,RECCL,B,D)+ &
                (EQCRUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)* &
                NCWLOAD(CurCalYr,D,B)))*EQCRP90(CurCalYr,RECCL,B,D))* &
                RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

       IF(EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+     &
                 EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D).GT.0.) THEN
       H2OCONIN(CurIYr,FCON,D,B)=H2OCONIN(CurIYr,FCON,D,B)+(          &
       ((1.-CWLOAD(RECSYear))*EQCSIUEC(CurCalYr,RECCL,B,D)+&
        (EQCSIUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         ECWLOAD(CurCalYr,D,B)))*EQCESE(CurCalYr,RECCL,B,D) + &
       ((1.-CWLOAD(RECSYear))*EQCNIUEC(CurCalYr,RECCL,B,D)+&
        (EQCNIUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         NCWLOAD(CurCalYr,D,B)))*EQCADD(CurCalYr,RECCL,B,D) + &
       ((1.-CWLOAD(RECSYear))*EQCRIUEC(CurCalYr,RECCL,B,D)+&
        (EQCRIUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         NCWLOAD(CurCalYr,D,B)))*EQCRP90(CurCalYr,RECCL,B,D))

       Driver(CurIYr,fcon,d,b)=Driver(CurIYr,fcon,d,b)+                   &
                (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+     &
                 EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D))
       ENDIF

              EQCEQCN(CurIYr,RECCL,B,D)= LEAPYR*( &
       ((1.-CWLOAD(RECSYear))*EQCSUEC(CurCalYr,RECCL,B,D)+&
        (EQCSUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         ECWLOAD(CurCalYr,D,B)))*EQCESE(CurCalYr,RECCL,B,D) + &
       ((1.-CWLOAD(RECSYear))*EQCNUEC(CurCalYr,RECCL,B,D)+&
        (EQCNUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         NCWLOAD(CurCalYr,D,B)))*EQCADD(CurCalYr,RECCL,B,D) + &
       ((1.-CWLOAD(RECSYear))*EQCRUEC(CurCalYr,RECCL,B,D)+&
        (EQCRUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         NCWLOAD(CurCalYr,D,B)))*EQCRP90(CurCalYr,RECCL,B,D))* &
              RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

            ELSE
           IF ((CurCalYr.GT.2010).AND.(STIMULUS.EQ.1).AND.(F.EQ.4)) THEN
             ALPHA=-0.30
           ELSE
             ALPHA=-0.15
           ENDIF

               H2OCON(CurIYr,FCON,D)=H2OCON(CurIYr,FCON,D)+ LEAPYR*( &
       ((1.-CWLOAD(RECSYear))*EQCSUEC(CurCalYr,RECCL,B,D)+&
        (EQCSUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         ECWLOAD(CurCalYr,D,B)))*EQCESE(CurCalYr,RECCL,B,D) + &
       ((1.-CWLOAD(RECSYear))*EQCNUEC(CurCalYr,RECCL,B,D)+&
        (EQCNUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         NCWLOAD(CurCalYr,D,B)))*(EQCADD(CurCalYr,RECCL,B,D)+EQCRP90RP(CurCalYr,RECCL,B,D)) + &
       ((1.-CWLOAD(RECSYear))*EQCRUEC(CurCalYr,RECCL,B,D)+&
        (EQCRUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         NCWLOAD(CurCalYr,D,B)))*EQCRP90(CurCalYr,RECCL,B,D)+ &
       ((1.-CWLOAD(RECSYear))*EQCAUEC(CurCalYr,RECCL,B,D)+&
        (EQCAUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         ECWLOAD(CurCalYr,D,B)))*(EQCREP(CurCalYr,RECCL,B,D)+EQCSUR(CurCalYr,RECCL,B,D)+EQCSR90(CurCalYr,RECCL,B,D)))* &
              RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

              H2OCONWT(CurIYr,FCON,D,B)=H2OCONWT(CurIYr,FCON,D,B)+LEAPYR*( &
       ((1.-CWLOAD(RECSYear))*EQCSUEC(CurCalYr,RECCL,B,D)+&
        (EQCSUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         ECWLOAD(CurCalYr,D,B)))*EQCESE(CurCalYr,RECCL,B,D) + &
       ((1.-CWLOAD(RECSYear))*EQCNUEC(CurCalYr,RECCL,B,D)+&
        (EQCNUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         NCWLOAD(CurCalYr,D,B)))*(EQCADD(CurCalYr,RECCL,B,D)+EQCRP90RP(CurCalYr,RECCL,B,D)) + &
       ((1.-CWLOAD(RECSYear))*EQCRUEC(CurCalYr,RECCL,B,D)+&
        (EQCRUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         NCWLOAD(CurCalYr,D,B)))*EQCRP90(CurCalYr,RECCL,B,D)+ &
       ((1.-CWLOAD(RECSYear))*EQCAUEC(CurCalYr,RECCL,B,D)+&
        (EQCAUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         ECWLOAD(CurCalYr,D,B)))*(EQCREP(CurCalYr,RECCL,B,D)+EQCSUR(CurCalYr,RECCL,B,D)+EQCSR90(CurCalYr,RECCL,B,D)))* &
              RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

       IF(EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+     &
                 EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
                 EQCSR90(CurCalYr,RECCL,b,D)+EQCREP(CurCalYr,RECCL,b,D)+    &
                 EQCSUR(CurCalYr,RECCL,b,D).GT.0.) THEN
               H2OCONIN(CurIYr,FCON,D,B)=H2OCONIN(CurIYr,FCON,D,B)+(  &
       ((1.-CWLOAD(RECSYear))*EQCSIUEC(CurCalYr,RECCL,B,D)+&
        (EQCSIUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         ECWLOAD(CurCalYr,D,B)))*EQCESE(CurCalYr,RECCL,B,D) + &
       ((1.-CWLOAD(RECSYear))*EQCNIUEC(CurCalYr,RECCL,B,D)+&
        (EQCNIUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         NCWLOAD(CurCalYr,D,B)))*(EQCADD(CurCalYr,RECCL,B,D)+EQCRP90RP(CurCalYr,RECCL,B,D)) + &
       ((1.-CWLOAD(RECSYear))*EQCRIUEC(CurCalYr,RECCL,B,D)+&
        (EQCRIUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         NCWLOAD(CurCalYr,D,B)))*EQCRP90(CurCalYr,RECCL,B,D)+ &
       ((1.-CWLOAD(RECSYear))*EQCAUEC(CurCalYr,RECCL,B,D)+&
        (EQCAUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         ECWLOAD(CurCalYr,D,B)))*(EQCREP(CurCalYr,RECCL,B,D)+EQCSUR(CurCalYr,RECCL,B,D)+EQCSR90(CurCalYr,RECCL,B,D)))

         Driver(CurIYr,fcon,d,b)=Driver(CurIYr,fcon,d,b)+                   &
                (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+     &
                 EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
                 EQCSR90(CurCalYr,RECCL,b,D)+EQCREP(CurCalYr,RECCL,b,D)+    &
                 EQCSUR(CurCalYr,RECCL,b,D))
       ENDIF

              EQCEQCN(CurIYr,RECCL,B,D)=   LEAPYR* ( &
       ((1.-CWLOAD(RECSYear))*EQCSUEC(CurCalYr,RECCL,B,D)+&
        (EQCSUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         ECWLOAD(CurCalYr,D,B)))*EQCESE(CurCalYr,RECCL,B,D) + &
       ((1.-CWLOAD(RECSYear))*EQCNUEC(CurCalYr,RECCL,B,D)+&
        (EQCNUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         NCWLOAD(CurCalYr,D,B)))*(EQCADD(CurCalYr,RECCL,B,D)+EQCRP90RP(CurCalYr,RECCL,B,D)) + &
       ((1.-CWLOAD(RECSYear))*EQCRUEC(CurCalYr,RECCL,B,D)+&
        (EQCRUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         NCWLOAD(CurCalYr,D,B)))*EQCRP90(CurCalYr,RECCL,B,D)+ &
       ((1.-CWLOAD(RECSYear))*EQCAUEC(CurCalYr,RECCL,B,D)+&
        (EQCAUEC(CurCalYr,RECCL,B,D)*CWLOAD(RECSYear)*&
         ECWLOAD(CurCalYr,D,B)))*(EQCREP(CurCalYr,RECCL,B,D)+EQCSUR(CurCalYr,RECCL,B,D)+EQCSR90(CurCalYr,RECCL,B,D)))* &
              RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
            ENDIF

!    SOLAR IS CALCULATED DIFFERENTLY

            FCON = FWHCON(8)   ! = 5

            H2OCON(CurIYr,5,D)=H2OCON(CurIYr,5,D)+LEAPYR* &
             (EQCESE(CurCalYr,RECCLSWH,B,D)* &
            ((EQCUEC(D,RECCLEWH,B)-EQCUEC(D,RECCLSWH,B))*WHRFOSS(D,CurIYr)/3412. )+ &  !STEOhr
              EQCRP90(CurCalYr,RECCLSWH,B,D)* &
            ((EQCUEC(D,RECCLEWH,B)-EQCRUEC(CurCalYr,RECCLSWH,B,D))*WHRFOSS(D,CurIYr)/3412.) &  !STEOhr
            + EQCADD(CurCalYr,RECCLSWH,B,D)* &
                   ((EQCUEC(D,RECCLEWH,B)-EQCHVUEC(CurCalYr,RECCLSWH,B,D))*WHRFOSS(D,CurIYr)/3412.) + &  !STEOhr
          (EQCREP(CurCalYr,RECCLSWH,B,D)+EQCRP90RP(CurCalYr,RECCLSWH,B,D))* &
            ((EQCUEC(D,RECCLEWH,B)-EQCNUEC(CurCalYr,RECCLSWH,B,D))*WHRFOSS(D,CurIYr)/3412.) &  !STEOhr
            +(EQCSR90(CurCalYr,RECCLSWH,B,D)+ &
              EQCSUR(CurCalYr,RECCLSWH,B,D))* &
            ((EQCUEC(D,RECCLEWH,B)-EQCAUEC(CurCalYr,RECCLSWH,B,D))*WHRFOSS(D,CurIYr)/3412.))  !STEOhr

            SLEQCN(CurIYr,1,B,D)=LEAPYR*&
             (EQCESE(CurCalYr,RECCLSWH,B,D)* &
            ((EQCUEC(D,RECCLEWH,B)-EQCUEC(D,RECCLSWH,B))*WHRFOSS(D,CurIYr)/3412. )+ &  !STEOhr
              EQCRP90(CurCalYr,RECCLSWH,B,D)* &
            ((EQCUEC(D,RECCLEWH,B)-EQCRUEC(CurCalYr,RECCLSWH,B,D))*WHRFOSS(D,CurIYr)/3412.) &  !STEOhr
            + EQCADD(CurCalYr,RECCLSWH,B,D)* &
                   ((EQCUEC(D,RECCLEWH,B)-EQCHVUEC(CurCalYr,RECCLSWH,B,D))*WHRFOSS(D,CurIYr)/3412.) + &  !STEOhr
          (EQCREP(CurCalYr,RECCLSWH,B,D)+EQCRP90RP(CurCalYr,RECCLSWH,B,D))* &
            ((EQCUEC(D,RECCLEWH,B)-EQCNUEC(CurCalYr,RECCLSWH,B,D))*WHRFOSS(D,CurIYr)/3412.) &  !STEOhr
            +(EQCSR90(CurCalYr,RECCLSWH,B,D)+ &
              EQCSUR(CurCalYr,RECCLSWH,B,D))* &
            ((EQCUEC(D,RECCLEWH,B)-EQCAUEC(CurCalYr,RECCLSWH,B,D))*WHRFOSS(D,CurIYr)/3412.))  !STEOhr
 100  CONTINUE

!*******************************************************************
!  Calculate Solar Water Heating Consumption
!*******************************************************************
      DO 110 D=1,mNumCR-2
        SLCON(CurIYr,D)=0.0
       DO 110 B=1,mNumBldg
        SLCON(CurIYr,D)=SLCON(CurIYr,D)+LEAPYR*&
             (EQCESE(CurCalYr,RECCLSWH,B,D)* &
            ((EQCUEC(D,RECCLEWH,B)-EQCUEC(D,RECCLSWH,B))*WHRFOSS(D,CurIYr)/3412. )+ &  !STEOhr
              EQCRP90(CurCalYr,RECCLSWH,B,D)* &
            ((EQCUEC(D,RECCLEWH,B)-EQCRUEC(CurCalYr,RECCLSWH,B,D))*WHRFOSS(D,CurIYr)/3412.) &  !STEOhr
            + EQCADD(CurCalYr,RECCLSWH,B,D)* &
                   ((EQCUEC(D,RECCLEWH,B)-EQCHVUEC(CurCalYr,RECCLSWH,B,D))*WHRFOSS(D,CurIYr)/3412.) + &  !STEOhr
          (EQCREP(CurCalYr,RECCLSWH,B,D)+EQCRP90RP(CurCalYr,RECCLSWH,B,D))* &
            ((EQCUEC(D,RECCLEWH,B)-EQCNUEC(CurCalYr,RECCLSWH,B,D))*WHRFOSS(D,CurIYr)/3412.) &  !STEOhr
            +(EQCSR90(CurCalYr,RECCLSWH,B,D)+ &
              EQCSUR(CurCalYr,RECCLSWH,B,D))* &
            ((EQCUEC(D,RECCLEWH,B)-EQCAUEC(CurCalYr,RECCLSWH,B,D))*WHRFOSS(D,CurIYr)/3412.))  !STEOhr
 110  CONTINUE

      DO R=1,mNumCR-2
        DO FCON=1,5  !Natural gas, electricity, distillate fuel oil, propane, solar
         DO B=1,mNumBldg
          IF (Driver(CurIYr,FCON,R,B).GT.0)   &
           H2OCONIN(CurIYr,FCON,R,B)=         &
           H2OCONIN(CurIYr,FCON,R,B)/Driver(CurIYr,FCON,R,B)
         ENDDO
        ENDDO
      ENDDO

      END SUBROUTINE RWHCON


!*******************************************************************
!     COOKING CHOICE SUBROUTINE
!*******************************************************************
      SUBROUTINE RSTVTEC
      IMPLICIT NONE
      REAL*4 NEQTSHRD(RECSYear:EndYr+1,MNUMRTTY,mNumBldg,mNumCR), &
             REQTSHRD(RECSYear:EndYr+1,MNUMRTTY,mNumBldg,mNumCR)
      REAL*4 OPCOST(2)
      REAL*4 NGNGFACT(mNumBldg)
      REAL*4 RTEFFAC(2),DECAY,DENOM,SUM,DENOM2,SUM2
      REAL*4 EQFSHRN(nCookTypes,mNumBldg,mNumCR),EQFSHRR(nCookTypes,mNumBldg,mNumCR)  !EqpParam
      REAL*4 EQWTN(nCookTypes,mNumBldg,mNumCR),EQWTR(nCookTypes,mNumCR,mNumCR)  !EqpParam
      REAL*4 TOTEWTN(nCookClasses,mNumBldg,mNumCR),TOTEWTR(nCookClasses,mNumBldg,mNumCR)  !EqpParam
      REAL*4 EQCOST,CAPITAL,RETAIL
      INTEGER EU,EUPR,EUHW,RECTY,RECCL,RECCLHW,R,B,F,EQT,EQC,EQCHW, &
              TYPE,COUNT,RECAR(nCookTypes),EQTAR(nCookTypes),L  !EqpParam

      ALPHA1=-0.50

!     COMPUTE DECAY RATE USED TO COMPUTE LIFE CYCLE COST (FIRST ITERATION ONLY)
      IF(CURITR.EQ.1) THEN
        DECAY = (1-((1+ResDiscountRate)**(-Tenure)))/ResDiscountRate
      ENDIF

! Set share of homes with natural gas water heaters that also have natural gas cooking ranges (based on RECS)	!kj
      NGNGFACT(1)= 0.50
      NGNGFACT(2)= 0.48
      NGNGFACT(3)= 0.75

!*******************************************************************
!   THE SAME GENERAL FORM OF THIS SUBROUTINE WORKS FOR ALL END USES
!     SET EU   = 6 FOR COOKING.
!     SET EUHW = 5 FOR WATER HEATING
!*******************************************************************
      EU=6
      EUPR=4
      !EUHW links cooking choice to water heating fuel
      EUHW=5

!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,mNumCR-2
        PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
      ENDDO

!  ZERO OUT ARRAYS
      DO 10 R=1,mNumCR-2
        DO 10 B=1,mNumBldg
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            NCKSH(CurCalYr,EQC,B,R) =0.0
            TOTEWTN(EQC,B,R)=0.0
            TOTEWTR(EQC,B,R)=0.0
          ENDDO
10    CONTINUE

!*******************************************************************
!     CALCULATE NCKSH (G,L,E)FOR NEW DISTRIBUTION
!*******************************************************************
      DO 100 R=1,mNumCR-2
        DO 100 B=1,mNumBldg
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            DO RECCLHW=RTCLEUPT(EUHW)+1,RTCLEUPT(EUHW+1)
              IF(RTCLPNTR(RECCLHW).EQ.EQC.OR.RTCLREPL(RECCLHW).EQ.EQC) THEN
                EQCHW=RTCLEQCL(RECCLHW)
                IF(RTCLNAME(RECCLHW).EQ.'NG_WH'.AND.RTCLNAME(RECCL).EQ.'NG_STV') THEN
                  NCKSH(CurCalYr,EQC,B,R)=NCKSH(CurCalYr,EQC,B,R)+NH2OSH(CurCalYr,EQCHW,B,R)*NGNGFACT(B)
                ELSEIF(RTCLNAME(RECCLHW).EQ.'NG_WH'.AND.RTCLNAME(RECCL).EQ.'ELEC_STV') THEN
                  NCKSH(CurCalYr,EQC,B,R)=NCKSH(CurCalYr,EQC,B,R)+NH2OSH(CurCalYr,EQCHW,B,R)*(1-NGNGFACT(B))
                ELSE
                  NCKSH(CurCalYr,EQC,B,R)=NCKSH(CurCalYr,EQC,B,R)+NH2OSH(CurCalYr,EQCHW,B,R)
                ENDIF
              ENDIF
            ENDDO
          ENDDO

!*******************************************************************
!     RECTY          = RECORD NUMBER IN RSMEQP FILE
!     RTTYEUPT(EU)   = LAST RECORD IN WATER HEATING (EU=5)
!     RTTYEUPT(EU+1) = LAST RECORD IN COOKING       (EU=6)
!*******************************************************************
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)

!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
            IF(CurCalYr.GE.RTINITYR(RECTY).AND. &
               CurCalYr.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              EQC  = RTTYEQCL(RECTY)
              EQT  = RTEQTYPE(RECTY)
              RECCL= RTCLEUPT(EU)+EQC
              F    = RTFUEL(RECCL)

!     COMPUTE EFFICIENCY FACTOR USED IN COMPUTING OPERATING COST
              IF(RTEQEFF(RECTY).NE.0.0) THEN
                RTEFFAC(1)=EQCEFF(CurCalYr,RECCL)/RTEQEFF(RECTY)
                RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)/RTEQEFF(RECTY)
              ELSE
                RTEFFAC(1)=RTBASEFF(RECSYear,RECCL)
                RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)
              ENDIF

!  If COSTTRSW = 1, use function EQCOST to compute capital
!     cost of new equipment.
!  If COSTTRSW = 0, use constant value from RSMEQP file for capital
!     cost of new equipment.
        IF (COSTTRSW.EQ.1) THEN
          CAPITAL = EQCOST(RECTY,CurCalYr,"CAP")
        ELSE
          CAPITAL = RTEQCOST(RECTY)
        ENDIF

!     CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOR CHANGES
      IF ((CurCalYr.GT.2008).AND. &	!kj - 2008 marks first year before American Clean Energy and Security Act of 2009 (Waxman-Markey bill)?
               (PRICES(F,R,CurCalYr).GT.PRICES(F,R,RECSYear))) THEN
       HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
       ELIGBLE=HRDRATE - 0.07
        IF (ELIGBLE.GT.0.0) THEN
         HRDADJ= ELIGBLE * &
            ((PRICES(F,R,CurCalYr)/PRICES(F,R,RECSYear))**ALPHA1 )

         BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
        ELSE
         BETA1DR(RECTY)=RTECBTA1(RECTY)
        ENDIF
        ELSE
         BETA1DR(RECTY)=RTECBTA1(RECTY)
      ENDIF

!      CALCULATE OPERATING COST
              OPCOST(1)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B) &
                    *RTEFFAC(1)
              OPCOST(2)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B) &
                    *RTEFFAC(2)

!      CALCULATE LIFE CYCLE COSTS
              LFCY(EQT,B,R,1)=CAPITAL + (OPCOST(1) *DECAY)
              LFCY(EQT,B,R,2)=CAPITAL + (OPCOST(2) *DECAY)
!*******************************************************************
!    COMPUTE WEIGHTS FOR NEW AND REPLACEMENT EQUIPMENT TYPES
!*******************************************************************
              EQWTN(EQT,B,R)= EXP(RTECBIAS(RECTY)+(BETA1DR(RECTY)* &
                        CAPITAL)+(RTECBTA2(RECTY)*OPCOST(2)) + &
                        ( RTECBTA3(RECTY)*LFCY(EQT,B,R,2) ) )
              TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)
              EQWTR(EQT,B,R)= EXP(RTECBIAS(RECTY)+(BETA1DR(RECTY)* &
                        CAPITAL)+(RTECBTA2(RECTY)*OPCOST(1)) + &
                        ( RTECBTA3(RECTY)*LFCY(EQT,B,R,1) ) )
              TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)

            ENDIF
           ENDIF
          ENDDO

!*******************************************************************

          TYPE = RTTYPECT(EU)
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)

!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
            IF(CurCalYr.GE.RTINITYR(RECTY).AND. &
               CurCalYr.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              TYPE=TYPE+1

!      FIND VALID INDICES FOR EQUIPMENT CLASS (EQC) & EQUIPMENT
!        TYPE (EQT)
              EQC=RTTYEQCL(RECTY)
              EQT=RTEQTYPE(RECTY)

!   SET EQUIPMENT FUEL SHARE (AND NEQTSHR FOR WATER HEATING)
              IF (TOTEWTN(EQC,B,R).GT.0.0) THEN
                EQFSHRN(EQT,B,R)=EQWTN(EQT,B,R) /TOTEWTN(EQC,B,R)
              ELSE
                EQFSHRN(EQT,B,R)=0.0
              ENDIF

              IF (TOTEWTR(EQC,B,R).GT.0.0) THEN
                EQFSHRR(EQT,B,R)=EQWTR(EQT,B,R)/TOTEWTR(EQC,B,R)
              ELSE
                EQFSHRR(EQT,B,R)=0.0
              ENDIF

              NEQTSHR(CurCalYr,TYPE,B,R)=EQFSHRN(EQT,B,R)
              REQTSHR(CurCalYr,TYPE,B,R)=EQFSHRR(EQT,B,R)
            ENDIF
           ENDIF
          ENDDO

!*******************************************************************
!     CALCULATE WEIGHTED EFFICIENCY FOR NEW EQUIPMENT AND
!     CALCULATE WEIGHTED EFFICIENCY FOR REPLACEMENT EQUIPMENT
!*******************************************************************
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC   =RTCLEQCL(RECCL)
            DENOM =0
            DENOM2=0
            COUNT =0
            TYPE = RTTYPECT(EU)
            DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
              IF(RTTYEQCL(RECTY).EQ.EQC.AND.CurCalYr.GE.RTINITYR(RECTY) &
                .AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
                COUNT=COUNT+1
                EQT=RTEQTYPE(RECTY)
                RECAR(COUNT)=RECTY
                EQTAR(COUNT)=EQT
                DENOM =DENOM +EQFSHRN(EQT,B,R)
                DENOM2=DENOM2+EQFSHRR(EQT,B,R)
              ENDIF
             ENDIF
            ENDDO

!    COMPLETE CALCULATION FOR NEW EQUIPMENT
            IF(DENOM.LE.0.0) THEN
              WTEQCEFFN(CurCalYr,RECCL,B,R)=RTBASEFF(RECSYear,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                EQT=EQTAR(L)
                SUM=SUM+(EQFSHRN(EQT,B,R)*RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFN(CurCalYr,RECCL,B,R)=SUM/DENOM
            ENDIF

!     COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT
            IF(DENOM2.LE.0.0) THEN
              WTEQCEFFR(CurCalYr,RECCL,B,R)=RTBASEFF(RECSYear,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                EQT=EQTAR(L)
                SUM=SUM+(EQFSHRR(EQT,B,R)*RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFR(CurCalYr,RECCL,B,R)=SUM/DENOM2
            ENDIF
          ENDDO
100   CONTINUE
      END SUBROUTINE RSTVTEC


!******************************************************************
!     COOKING ADDED SUBROUTINE HANDLED BY SUB REUADD(EU)
!******************************************************************

!*******************************************************************
!  COOKING CONSUMPTION
!*******************************************************************
      SUBROUTINE RSTOVCON
      IMPLICIT NONE
      REAL*4 ALPHA,ef1,ef2,ef3,TEMP
      INTEGER B, E, D, FCON, RECCL, EU, EUPR, V, F,EV,TYPE,&
             RECTY,EQT,NUMEQT,EQC,R,Y

!*******************************************************************
!   COOKING SECTION OF THE DATA
!*******************************************************************
      EV=4
      EU=6
      EUPR=4
      alpha=0.0;ef1=.5;ef2=.35;ef3=.15

!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,mNumCR-2
        PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
      ENDDO

!*******************************************************************
! COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR RECS-YEAR EQUIPMENT
!*******************************************************************
      IF (CurCalYr.GE.RECSYear+1) THEN
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RTBASEFF(CurCalYr,RECCL)= STKEFF(CurCalYr,RECCL)
        ENDDO
      ENDIF

!*******************************************************************
!  Calculate New and Average UEC
!*******************************************************************
      DO 10 D=1,mNumCR-2
        DO 10 B=1,mNumBldg
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
         EQCSUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
        ( RTBASEFF(RECSYear,RECCL) / RTBASEFF(CurCalYr,RECCL) )
            IF (CurCalYr.EQ.RECSYear+1) THEN
              IF (WTEQCEFFN(CurCalYr,RECCL,B,D).GT.0.0) THEN
              EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
                ( RTBASEFF(RECSYear,RECCL)/WTEQCEFFN(CurCalYr,RECCL,B,D))
              ELSE
                EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
              ENDIF
            ELSE
              IF (WTEQCEFFN(CurCalYr,RECCL,B,D).GT.0.0) THEN
                EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
                 ( RTBASEFF(RECSYear,RECCL)/WTEQCEFFN(CurCalYr,RECCL,B,D))
              ELSE
                EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
              ENDIF
            ENDIF

            IF (WTEQCEFFR(CurCalYr,RECCL,B,D).GT.0.0) THEN
              EQCRUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B) * &
               (RTBASEFF(RECSYear,RECCL)/WTEQCEFFR(CurCalYr,RECCL,B,D))
            ELSE
              EQCRUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
            ENDIF

            IF (CurCalYr .EQ. RECSYear+1) THEN
              EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
         ELSE
         TEMP=0.0
           DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
         EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
         EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
           ENDDO
          IF(TEMP.LE.0.0) THEN
             EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
          ELSE
          EQCAUEC(CurCalYr,RECCL,B,D)=0.0
        DO Y=RECSYear,CurCalYr-1
             EQCAUEC(CurCalYr,RECCL,B,D)=EQCAUEC(CurCalYr,RECCL,B,D)+( &
       (EQR90FUT(Y,CurCalYr,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
         EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*EQCNUEC(Y,RECCL,B,D))) &
                                           /TEMP
        ENDDO
           ENDIF
          ENDIF
          ENDDO
 10   CONTINUE

!*******************************************************************
!  CALCULATE AVERAGE EFFICIENCY
!*******************************************************************
      DO 30 B=1,mNumBldg
        DO 30 D=1,mNumCR-2
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)

            IF (CurCalYr.EQ.RECSYear+1) THEN
              WTEQCEFFA(RECSYear+1,RECCL,B,D)=WTEQCEFFN(RECSYear+1,RECCL,B,D)

            ELSE
             TEMP=0.0
            DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
         EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
         EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
           ENDDO
              IF (TEMP .GT. 0.0) THEN
                WTEQCEFFA(CurCalYr,RECCL,B,D)=0.0
       DO Y=RECSYear,CurCalYr-1
      WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFA(CurCalYr,RECCL,B,D)+( &
       (EQR90FUT(Y,CurCalYr,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
         EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D))) &
                                           /TEMP
       ENDDO
              ELSE
                WTEQCEFFA(CurCalYr,RECCL,B,D)= &
                  WTEQCEFFN(CurCalYr,RECCL,B,D)
              ENDIF
            ENDIF
          ENDDO
30    CONTINUE

!*******************************************************************
!  Calculate Cooking Consumption
!*******************************************************************
      DO 20 D=1,mNumCR-2
        DO 20 FCON=1,NSTVFL
          CKCON(CurIYr,FCON,D)=0.0
            DO 20 B=1,mNumBldg
             CKCONWT(CurIYr,FCON,D,B)=0.
             Driver(CurIYr,fcon,d,b)=0.
             CKCONIN(CurIYr,FCON,D,B)=0.
 20   CONTINUE

!*******************************************************************
      DO 100 D=1,mNumCR-2
        DO 100 B=1,mNumBldg
          DO 100 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            E=RTCLEQCL(RECCL)
            F=RTFUEL(RECCL)
            FCON=FSTVCON(F)
            IF (CurCalYr.EQ.RECSYear+1) THEN
              CKCON(CurIYr,FCON,D)=CKCON(CurIYr,FCON,D)+LEAPYR* ( &
              ( (EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
                (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
                (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
                (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))) &
                 * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

              CKCONWT(CurIYr,FCON,D,B)=CKCONWT(CurIYr,FCON,D,B)+ LEAPYR*   ( &
              ( (EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
                (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
                (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
                (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))) &
                 * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

       IF(EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+     &
                 EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D).GT.0.) THEN
              CKCONIN(CurIYr,FCON,D,B)=CKCONIN(CurIYr,FCON,D,B)+(       (&
              ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
                (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
                (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
                (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))) )

         Driver(CurIYr,fcon,d,b)=Driver(CurIYr,fcon,d,b)+                   &
                (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+     &
                 EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D) )
        ENDIF

              EQCEQCN(CurIYr,RECCL,B,D)= LEAPYR*&
              ( EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D)+ &
               (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
                EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
                EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)) &
              * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

            ELSE
              CKCON(CurIYr,FCON,D)=CKCON(CurIYr,FCON,D)+LEAPYR* &
             (  EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D)+ &
                EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
                EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)+ &
                EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
                EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)+ &
                EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
                EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)) &
              * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

              CKCONWT(CurIYr,FCON,D,B)= CKCONWT(CurIYr,FCON,D,B) + LEAPYR*&
             (  EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D)+ &
                EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
                EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)+ &
                EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
                EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)+ &
                EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
                EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)) &
              * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

       IF(EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+     &
                 EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
                 EQCSR90(CurCalYr,RECCL,b,D)+EQCREP(CurCalYr,RECCL,b,D)+    &
                 EQCSUR(CurCalYr,RECCL,b,D).GT.0.) THEN
              CKCONIN(CurIYr,FCON,D,B)= CKCONIN(CurIYr,FCON,D,B) +      (&
             (  EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D)+ &
                EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
                EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)+ &
                EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
                EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)+ &
                EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
                EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))   )

            Driver(CurIYr,fcon,d,b)=Driver(CurIYr,fcon,d,b)+                   &
                (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+     &
                 EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
                 EQCSR90(CurCalYr,RECCL,b,D)+EQCREP(CurCalYr,RECCL,b,D)+    &
                 EQCSUR(CurCalYr,RECCL,b,D))
              ENDIF

              EQCEQCN(CurIYr,RECCL,B,D)= LEAPYR*&
              ( EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D)+ &
                EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
                EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)+ &
              EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
               EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)+ &
                EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)+ &
                EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D) ) &
            * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
            ENDIF
 100  CONTINUE

      DO R=1,mNumCR-2
        DO FCON=1,NSTVFL
         DO B=1,mNumBldg
          IF (Driver(CurIYr,FCON,R,B).GT.0)   &
           CKCONIN(CurIYr,FCON,R,B)=                &
           CKCONIN(CurIYr,FCON,R,B)/Driver(CurIYr,FCON,R,B)
         ENDDO
        ENDDO
      ENDDO

END SUBROUTINE RSTOVCON


!*******************************************************************
!     CLOTHES DRYER CHOICE SUBROUTINE
!*******************************************************************
      SUBROUTINE RDRYTEC
      IMPLICIT NONE
      REAL*4 EQWTN(nClDryTypes,mNumBldg,mNumCR),EQWTR(nClDryTypes,mNumBldg,mNumCR)  !EqpParam
      REAL*4 TOTEWTN(nClDryClasses,mNumBldg,mNumCR),TOTEWTR(nClDryClasses,mNumBldg,mNumCR)  !EqpParam
      REAL*4 OPCOST(2)
      REAL*4 CONVFACT,RTEFFAC(2),DECAY,ECTEMP,DENOM,SUM,DENOM2,SUM2
      REAL*4 EQCOST,CAPITAL,RETAIL
      INTEGER EU,RECTY,RECCL,R,B,F,EQT,EQC,REQT,TYPE, &
              COUNT,L
      INTEGER RECAR(nClDryTypes),EQTAR(nClDryTypes)  !EqpParam

!*******************************************************************
!     SET CONVERSION FACTOR FOR COMPUTING NEQTSHR FOR CLOTHES DRYERS
!*******************************************************************
      CONVFACT=100.0	!kj - not used?
      ALPHA1=-0.50

!     COMPUTE DECAY RATE USED TO COMPUTE LIFE CYCLE COST (FIRST ITERATION ONLY)
      IF(CURITR.EQ.1) THEN
        DECAY = (1-((1+ResDiscountRate)**(-Tenure)))/ResDiscountRate
      ENDIF

!*******************************************************************
!   THE SAME GENERAL FORM OF THIS SUBROUTINE WORKS FOR ALL END USES.
!     SET EU = 7 TO SEARCH THE CLOTHES DRYING SECTION OF THE DATA
!*******************************************************************
      EU=7

!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,mNumCR-2
        PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,5)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
      ENDDO

!   ZERO-OUT ARRAYS
      DO 5 B=1,mNumBldg
        DO 5 R=1,mNumCR-2
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            TOTEWTN(EQC,B,R)=0.0
            TOTEWTR(EQC,B,R)=0.0
          ENDDO
5     CONTINUE

!*******************************************************************
!     RTTYEUPT(EU)   = LAST RECORD IN COOKING        (EU=6)
!     RTTYEUPT(EU+1) = LAST RECORD IN CLOTHES DRYING (EU=7)
!     RECTY          = RECORD NUMBER IN RSMEQP FILE
!*******************************************************************
    DO 40 R=1,mNumCR-2
     DO 40 B=1,mNumBldg
      DO 40 RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)

!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
        IF(CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
          IF (RTCENDIV(RECTY).EQ.R) THEN

!     FIND VALID INDICES FOR EQUIPMENT CLASS (EQC), EQUIPMENT TYPE (EQT),
!     RECORD # FOR TECH MENU, AND FUEL TYPE (F)
        EQC=RTTYEQCL(RECTY)
        EQT=RTEQTYPE(RECTY)

!     RECCL = RECORD NUMBER IN THE RSCLASS FILE
!     F     = FUEL NUMBER FOR THE CURRENT EQUIPMENT CLASS
        RECCL=RTCLEUPT(EU)+EQC
        F    =RTFUEL(RECCL)

!     COMPUTE EFFICIENCY FACTOR USED IN COMPUTING OPERATING COST
        IF(RTEQEFF(RECTY).NE.0.0) THEN
          RTEFFAC(1)=EQCEFF(CurCalYr,RECCL)/RTEQEFF(RECTY)
          RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)/RTEQEFF(RECTY)
        ELSE
          RTEFFAC(1)=RTBASEFF(RECSYear,RECCL)
          RTEFFAC(2)=RTBASEFF(RECSYear,RECCL)
        ENDIF

!  If COSTTRSW = 1, use function EQCOST to compute capital
!     cost of new equipment.
!  If COSTTRSW = 0, use constant value from RSMEQP file for capital
!     cost of new equipment.
        IF (COSTTRSW.EQ.1) THEN
          CAPITAL = EQCOST(RECTY,CurCalYr,"CAP")
        ELSE
          CAPITAL = RTEQCOST(RECTY)
        ENDIF

!     COMPUTE THE PART OF THE EQUIMENT CHOICE WEIGHT NOT DEPENDENT
!       ON REGION AND BUILDING TYPE
!       NOTE: ALL BIASES CURRENTLY SET TO 0.0 FOR CLOTHES DRYING

!     CALCULATE OPERATING COST
!       CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOR CHANGES
      IF ((CurCalYr.GT.2008).AND. &	!kj - 2008 marks first year before American Clean Energy and Security Act of 2009 (Waxman-Markey bill)?
               (PRICES(F,R,CurCalYr).GT.PRICES(F,R,RECSYear))) THEN
       HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
       ELIGBLE=HRDRATE - 0.07
        IF (ELIGBLE.GT.0.0) THEN
         HRDADJ= ELIGBLE * &
        ((PRICES(F,R,CurCalYr)/PRICES(F,R,RECSYear))**ALPHA1 )

         BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
        ELSE
         BETA1DR(RECTY)=RTECBTA1(RECTY)
        ENDIF
        ELSE
         BETA1DR(RECTY)=RTECBTA1(RECTY)
      ENDIF

        OPCOST(1)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B)*RTEFFAC(1)
        OPCOST(2)=PRICES(F,R,CurCalYr)*EQCUEC(R,RECCL,B)*RTEFFAC(2)

        ECTEMP = RTECBIAS(RECTY) + BETA1DR(RECTY)*CAPITAL

!      CALCULATE LIFE CYCLE COSTS
        LFCY(EQT,B,R,1)=RTEQCOST(RECTY) + (OPCOST(1) *DECAY)
        LFCY(EQT,B,R,2)=RTEQCOST(RECTY) + (OPCOST(2) *DECAY)

!    COMPUTE WEIGHTS FOR NEW AND REPLACEMENT EQUIPMENT TYPES
        EQWTN(EQT,B,R)= EXP(ECTEMP+(RTECBTA2(RECTY)*OPCOST(2)) + ( RTECBTA3(RECTY)*LFCY(EQT,B,R,2) ) )
        TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)
        EQWTR(EQT,B,R)= EXP(ECTEMP+(RTECBTA2(RECTY)*OPCOST(1)) + ( RTECBTA3(RECTY)*LFCY(EQT,B,R,1) ) )
        TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)

       ENDIF
      ENDIF
 40   CONTINUE

!*******************************************************************
!     RTTYEUPT(EU)   = LAST RECORD IN COOKING        (EU=6)
!     RTTYEUPT(EU+1) = LAST RECORD IN CLOTHES DRYING (EU=7)
!     RECTY          = RECORD NUMBER IN RSMEQP FILE
!*******************************************************************

        DO 45 R=1,mNumCR-2
         DO 45 B=1,mNumBldg
          TYPE=RTTYPECT(EU)
          DO 45 RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)

!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
        IF(CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
        TYPE=TYPE+1

!      FIND VALID INDICES FOR EQUIPMENT CLASS (EQC) & EQUIPMENT TYPE (EQT)
        EQC=RTTYEQCL(RECTY)
        EQT=RTEQTYPE(RECTY)

!   SET NEW EQUIPMENT SHARE
          IF (TOTEWTN(EQC,B,R).GT.0.0) THEN
            RECCL=RTCLEUPT(EU)+EQC
            NEQTSHR(CurCalYr,TYPE,B,R)=(EQWTN(EQT,B,R)/TOTEWTN(EQC,B,R))
!             *(DRYSHR(EQC,B,R)/CONVFACT)
            REQTSHR(CurCalYr,TYPE,B,R)=(EQWTR(EQT,B,R)/TOTEWTR(EQC,B,R))
          ELSE
            NEQTSHR(CurCalYr,TYPE,B,R)=0.0
            REQTSHR(CurCalYr,TYPE,B,R)=0.0
          ENDIF
      ENDIF
      ENDIF
45    CONTINUE

!*******************************************************************
!     CALCULATE WEIGHTED EFFICIENCY
!*******************************************************************
!*******************************************************************
!     RTTYEUPT(EU)   = LAST RECORD IN COOKING        (EU=6)
!     RTTYEUPT(EU+1) = LAST RECORD IN CLOTHES DRYING (EU=7)
!     RECTY          = RECORD NUMBER IN RSMEQP FILE
!*******************************************************************
      DO 90 R=1,mNumCR-2
        DO 90 B=1,mNumBldg
          DO 90 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC   =RTCLEQCL(RECCL)
            DENOM =0
            DENOM2=0
            COUNT =0
            DO 80 RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
              IF(RTTYEQCL(RECTY).EQ.EQC.AND.CurCalYr.GE.RTINITYR(RECTY) &
                .AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
                COUNT=COUNT+1
                EQT=RTEQTYPE(RECTY)
                RECAR(COUNT)=RECTY
                EQTAR(COUNT)=EQT
                DENOM =DENOM+EQWTN(EQT,B,R)
                DENOM2=DENOM2+EQWTR(EQT,B,R)
              ENDIF
              ENDIF
80          CONTINUE

!    COMPLETE CALCULATION FOR NEW EQUIPMENT
            IF(DENOM.LE.0.0) THEN
              WTEQCEFFN(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                EQT=EQTAR(L)
                SUM=SUM+(EQWTN(EQT,B,R)/RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFN(CurCalYr,RECCL,B,R)=SUM/DENOM
            ENDIF

!    COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT
            IF(DENOM2.LE.0.0) THEN
              WTEQCEFFR(CurCalYr,RECCL,B,R)=1/RTBASEFF(RECSYear,RECCL)
            ELSE
              SUM2=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                EQT=EQTAR(L)
                SUM2=SUM2+(EQWTR(EQT,B,R)/RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFR(CurCalYr,RECCL,B,R)=SUM2/DENOM2
            ENDIF
 90    CONTINUE
      END SUBROUTINE RDRYTEC


!*******************************************************************
!     DRYERS ADDED SUBROUTINE
!*******************************************************************
      SUBROUTINE RDRYADD
      IMPLICIT NONE
      REAL*4 SWT(RECSYear:EndYr),SWF(RECSYear:EndYr),DRYERTOT(RECSYear:EndYr,mNumCR-2)
      REAL*4 SA, HSR, ESR, SVRTE, P
      INTEGER EV,NUMEQT,RECCLSW
      INTEGER EU,EQC,EQT,RECCL,RECTY,TYPE,Y,R,B,TEMP,V,Y1

!*******************************************************************
!   SET EU = 7 TO SEARCH THE CLOTHES DRYING SECTION OF THE DATA
!*******************************************************************
      EV = 7  !check use of EV below
      EU = 7

!****************************************************************************************************************
!  DRYER Shares - 1=NG_DRY1, 2=NG_DRY2, 3=NG_DRY3, 4=NG_DRY4, 5=ELEC_DRY1, 6=ELEC_DRY2, 7=ELEC_DRY3, 8=ELEC_DRY4
!  DRYERS - 1=NG_DRY 2=ELEC_DRY
!****************************************************************************************************************
!  ZERO OUT ARRAYS
      DO 1 R=1,mNumCR-2
        DO 1 B=1,mNumBldg
          DO 1 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            EQCADD(CurCalYr,RECCL,B,R)=0.0
            EQCSR90(CurCalYr,RECCL,B,R)=0.0
            EQCRP90RP(CurCalYr,RECCL,B,R)=0.0
            EQCSUR(CurCalYr,RECCL,B,R)=0.0
            EQCREP(CurCalYr,RECCL,B,R) = 0.0
 1    CONTINUE

!*******************************************************************
!  Calculate Conventional Equipment Added in CurCalYr
!*******************************************************************
! CUMULATE SURVIVING NEW EQUIPMENT ADDED PRIOR TO CurCalYr TO ESTIMATE NH
! SA REPRESENTS NH at CurCalYr-1
! CUMULATE SURVIVING NEW EQUIPMENT ADDED & REPLACED PRIOR TO CurCalYr
! REPLACEMENT EQUIPMENT = SURV.HOUSES(SA) -  SURV.EQUIP(EQCSUR)
!*******************************************************************
!*******************************************************************
!     Calculate clothes dryers added in CurCalYr (CurCalYr-1)
!*******************************************************************
      DO 7 R=1,mNumCR-2
        DO 7 B=1,mNumBldg
          TYPE=RTTYPECT(EU)

!*******************************************************************
!     RTTYEUPT(EU)   = LAST RECORD IN COOKING        (EU=6)
!     RTTYEUPT(EU+1) = LAST RECORD IN CLOTHES DRYING (EU=7)
!     RECTY          = RECORD NUMBER IN RSMEQP FILE
!*******************************************************************
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              TYPE=TYPE+1
              EQC=RTTYEQCL(RECTY)
              EQT=RTEQTYPE(RECTY)
              RECCL=RTCLEUPT(EU)+EQC
       IF (CurCalYr.GT.RECSYear+1) THEN
          NEWDRYSAT(CurCalYr,2,B,R)=NEWDRYSAT(CurCalYr-1,2,B,R)*1.0069  !Average annual penetration rate of electric clothes dryers (E=2) into new homes
          NEWDRYSAT(CurCalYr,1,B,R)=NEWDRYSAT(CurCalYr-1,1,B,R)  !Average annual penetration rate of natural gas clothes dryers (E=1) into new homes; natural gas dryer penetration not assumed to increase over time like electric
       ENDIF
       IF ((NEWDRYSAT(CurCalYr,1,B,R)+NEWDRYSAT(CurCalYr,2,B,R)).GT.1.0000) THEN
          NEWDRYSAT(CurCalYr,2,B,R)=1.0000  !Prevents penetration of clothes dryers into new homes since RECS year from exceeding 100%
       ENDIF
              EQCADD(CurCalYr,RECCL,B,R)=(EQCADD(CurCalYr,RECCL,B,R) + (NEQTSHR(CurCalYr,TYPE,B,R) &
               *HSEADD(CurCalYr,B,R)*NEWDRYSAT(CurCalYr,EQC,B,R)))
            ENDIF
           ENDIF
          ENDDO
 7    CONTINUE

!*******************************************************************
!            INITIALIZE TO LAST ARRAY POSTION IN PREVIOUS END USE
!              AND THEN COUNT VALID TYPES IN CURRENT END USE
!*******************************************************************
        DO 15 R=1,mNumCR-2
         DO 15 B=1,mNumBldg
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           SA = 0.0
             IF (RTCLNAME(RECCL).EQ.'ELEC_DRY') THEN
               IF ((EQCND90(CurCalYr,RECCL,B,R)/EH(CurCalYr,B,R)).GE.0.90) THEN  !Max 90% electric dryer penetration into remaining RECSYear housing stock  !RSMISCpen
                 EQCND90(CurCalYr,RECCL,B,R)=EQCESE(RECSYear,RECCL,B,R)*HDR(B)**(CurCalYr-(RECSYear))- &  !RSMISCpen
                                             EQCESE(RECSYear,RECCL,B,R)*HDR(B)**(CurCalYr-(RECSYear))  !RSMISCpen
               ELSE  !RSMISCpen
                 EQCND90(CurCalYr,RECCL,B,R)=EQCESE(RECSYear,RECCL,B,R)*HDR(B)**(CurCalYr-(RECSYear))*(1.+ELDRYPR(B,R))- &
                                             EQCESE(RECSYear,RECCL,B,R)*HDR(B)**(CurCalYr-(RECSYear))
               ENDIF !90% electric dryer penetration into remaining RECSYear housing stock  !RSMISCpen
             ELSE
               EQCND90(CurCalYr,RECCL,B,R)=0.0
             ENDIF ! RTCLNAME='ELEC_DRY'

!******************************************************************
!  Calculate replacement equipment from original base year stock
!******************************************************************
      IF (CurCalYr.EQ.RECSYear+1) THEN
      EQCRP90(CurCalYr,RECCL,B,R)=(EQCRET(CurCalYr,RECCL) &
       *EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
      ELSE
      EQCRP90(CurCalYr,RECCL,B,R)=((EQCRET(CurCalYr,RECCL)- &
       EQCRET(CurCalYr-1,RECCL))*EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
      ENDIF

!   COMPUTE AND VINTAGE REPLACEMENTS OF REPLACEMENTS
           IF(CurCalYr.GT.RECSYear+1) THEN
              DO Y=RECSYear+1,CurCalYr-1
                TEMP=CurCalYr-Y
                ESR=SVRTE(RTALPHA(RECCL),TEMP-1,RTK(RECCL),RTLAMBDA(RECCL)) &
                 -SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
                EQCRP90RP(CurCalYr,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R) + &
                 (EQCRP90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
                 EQCRP90RP(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
                 EQCND90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
              ENDDO
          ENDIF

         EQCRP90RP(CurCalYr,RECCL,B,R)=EQCRP90RP(CurCalYr,RECCL,B,R) + EQCND90(CurCalYr,RECCL,B,R)
             IF(CurCalYr.GT.RECSYear+1) THEN
              DO Y=RECSYear+1,CurCalYr-1
                TEMP=CurCalYr-Y
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
                EQCSR90(CurCalYr,RECCL,B,R)=(EQCSR90(CurCalYr,RECCL,B,R) + &
                 (EQCRP90(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
                 EQCRP90RP(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
                 EQCND90(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP)) ))
                HSR=HDR(B)**(TEMP)
                SA = (SA + EQCADD(Y,RECCL,B,R)*HSR)
                EQCSUR(CurCalYr,RECCL,B,R) = (EQCSUR(CurCalYr,RECCL,B,R) + &
                 (((EQCADD(Y,RECCL,B,R)+EQCREP(Y,RECCL,B,R))*(HSR*ESR)) ))
              ENDDO
            ENDIF

!*******************************************************************
! CALCULATE REPLACEMENT WATER HEATERS FOR NEW VINTAGE IN CurCalYr-1
! NOTE: REPLACES WITH LIKE IF NOT SINGLE-FAMILY HOMES
! NOTE: FOR NEW HOUSES (NH) - CurCalYr-1 IS THE LAGGED VALUE
!*******************************************************************
!  SUBROUTINE REPLACE DISTRIBUTES REPLACEMENTS IN POST-RECS-YEAR
!    SINGLE-FAMILY HOMES WHEN LAST ARGUEMENT = 1

            IF(B.EQ.1) THEN
              !First, store what replacements would have been if no switching allowed.
               OEQCREP(CurCalYr,RECCL,1,R)=SA-EQCSUR(CurCalYr,RECCL,B,R)
              !Call REPLACE to distribute replacements.
               CALL REPLACE(EU,R,B,RECCL,1)
            ELSE
              !No switching allowed in multifamily or mobile homes.
               EQCREP(CurCalYr,RECCL,B,R)=SA-EQCSUR(CurCalYr,RECCL,B,R)
            ENDIF

         ENDDO
 15     CONTINUE

!  The following call to REPLACE with final argument = 2  distributes
!    replacements in Existing Single-Family Homes.

      B = 1
      DO  R=1,mNumCR-2
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          OEQCRP90(CurCalYr,RECCL,B,R) = EQCRP90(CurCalYr,RECCL,1,R)
          OEQCRP90R(CurCalYr,RECCL,B,R) = EQCRP90RP(CurCalYr,RECCL,1,R)
          CALL REPLACE(EU,R,B,RECCL,2)
        ENDDO
      ENDDO

      B = 1
      DO  R=1,mNumCR-2
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          SWITCHTO(CurCalYr,RECCL,B,R)=0.0
          SWITCHTOR(CurCalYr,RECCL,B,R)=0.0
          DO RECCLSW=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            IF (RECCLSW.NE.RECCL) THEN
              SWITCHTO(CurCalYr,RECCL,B,R)=SWITCHTO(CurCalYr,RECCL,B,R)+EQCSW90(CurCalYr,RECCLSW,RECCL,B,R)
              SWITCHTOR(CurCalYr,RECCL,B,R)=SWITCHTOR(CurCalYr,RECCL,B,R)+EQCSW90R(CurCalYr,RECCLSW,RECCL,B,R)
            ENDIF
          ENDDO
        ENDDO
      ENDDO

      B = 1
      DO  R=1,mNumCR-2
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQCRP90(CurCalYr,RECCL,B,R)= EQCRP90(CurCalYr,RECCL,B,R)-SWITCHES(CurCalYr,RECCL,B,R)
          EQCRP90RP(CurCalYr,RECCL,B,R)= EQCRP90RP(CurCalYr,RECCL,B,R)-SWITCHESR(CurCalYr,RECCL,B,R)+ &
		   SWITCHTOR(CurCalYr,RECCL,B,R)+SWITCHTO(CurCalYr,RECCL,B,R)
        ENDDO
      ENDDO

       B=1
       SWF(CurCalYr)=0.0
       SWT(CurCalYr)=0.0
       DO  R=1,mNumCR-2
         DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           SWT(CurCalYr)=SWT(CurCalYr)+SWITCHTO(CurCalYr,RECCL,B,R)+SWITCHTOR(CurCalYr,RECCL,B,R)
           SWF(CurCalYr)=SWF(CurCalYr)+SWITCHES(CurCalYr,RECCL,B,R)+SWITCHESR(CurCalYr,RECCL,B,R)
         ENDDO
       ENDDO

         DO B=1,mNumBldg
           DO R=1,mNumCR-2
             DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
               EQC=RTCLEQCL(RECCL)
               DO Y=CurCalYr,EndYr  ! VINTAGE EQUIPMENT FOR AVERAGE STOCK ACCOUNTING
                 TEMP=Y-CurCalYr
                 HSR=HDR(B)**(TEMP)
                 ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
                 EQR90FUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90(CurCalYr,RECCL,B,R)*ESR*HSR)
                 EQR90RPFUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R)*ESR*HSR)
                 EQREPFUT(CurCalYr,Y,RECCL,B,R)=(EQCREP(CurCalYr,RECCL,B,R)*ESR*HSR)
                 EQADDFUT(CurCalYr,Y,RECCL,B,R)=(EQCADD(CurCalYr,RECCL,B,R)*ESR*HSR)
               ENDDO
             ENDDO
           ENDDO
         ENDDO

!*******************************************************************
!     AGGREGATE CLOTHES DRYERS FOR INVESTMENT ANALYSIS
!*******************************************************************
      Y=CurCalYr
      NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)

            DO B=1,mNumBldg
              DO r=1,mNumCR-2
                TYPE=RTTYPECT(EU)
                DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
                  !CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
                  IF(CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
                    IF (RTCENDIV(RECTY).EQ.R) THEN
                      TYPE=TYPE+1  ! INDEX FOR 'TYPE' VARIABLES
                      EQT=RTEQTYPE(RECTY)
                      EQC=RTTYEQCL(RECTY)
                      RECCL=RTCLEUPT(EU)+EQC
                      HEATINGTYPEPURCH(Y,TYPE,B,R,1)=(NEQTSHR(Y,TYPE,B,r)*EQCADD(Y,RECCL,B,r))
                      HEATINGTYPEPURCH(Y,TYPE,B,R,2)=(NEQTSHR(Y,TYPE,B,r)*(EQCREP(Y,RECCL,B,r) + EQCRP90RP(Y,RECCL,B,r)) + &
                       REQTSHR(Y,TYPE,B,r)*EQCRP90(Y,RECCL,B,r)  )
                    ENDIF
                  ENDIF
                ENDDO
              ENDDO
            ENDDO

        IF ((CurCalYr.EQ.RECSYear+1).AND.(CURITR.EQ.1)) THEN
           DO R=1,mNumCR-2
            DRYERTOT(RECSYear,R)=0.0
             DO B=1,mNumBldg
              DRYERTOT(RECSYear,R)=DRYERTOT(RECSYear,R)+EQCESE(RECSYear,27,B,R)
             ENDDO
           ENDDO
         ELSE
           DO R=1,mNumCR-2
            DRYERTOT(CurCalYr,R)=0.0
             DO B=1,mNumBldg
              DRYERTOT(CurCalYr,R)=DRYERTOT(CurCalYr,R)+EQCESE(CurCalYr,27,B,R)+EQCADD(CurCalYr,27,B,R)+&
                EQCRP90(CurCalYr,27,B,R)+EQCRP90RP(CurCalYr,27,B,R)+EQCSUR(CurCalYr,27,B,R)+EQCREP(CurCalYr,27,B,R)+&
                EQCSR90(CurCalYr,27,B,R)
             ENDDO
           ENDDO
        ENDIF

! Proxy for gas customers is gas water heating for CDs 1,2,7 and 9
        DO R=1,mNumCR-2
         IF ((CurCalYr.EQ.RECSYear+1).AND.(CURITR.EQ.1)) THEN
            IF (DRYERTOT(RECSYear,R).GT.RSGASCUST(RECSYear,R)) THEN
              RSGASCUST(RECSYear,R)=DRYERTOT(RECSYear,R)
            ENDIF
         ELSE
            IF (DRYERTOT(CurCalYr,R).GT.RSGASCUST(CurCalYr,R)) THEN
              RSGASCUST(CurCalYr,R)=DRYERTOT(CurCalYr,R)
            ENDIF
         ENDIF
        ENDDO
        END SUBROUTINE RDRYADD


!*******************************************************************
!  CLOTHES DRYER CONSUMPTION
!*******************************************************************
      SUBROUTINE RDRYCON
      IMPLICIT NONE
      REAL*4 TEMP,ALPHA,ef1,ef2,ef3
      INTEGER B, E, D, F, EQC, RECCL, EU, EUPR,FCON,Y,R

!*******************************************************************
! CLOTHES DRYER SECTION OF THE DATA
!*******************************************************************
     EU = 7             
     EUPR=5
     ALPHA=-.15;ef1=.5;ef2=.35;ef3=.15

!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,mNumCR-2
        PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
      ENDDO

!*******************************************************************
! COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR RECS-YEAR EQUIPMENT
!*******************************************************************
      IF (CurCalYr.GE.RECSYear+1) THEN
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RTBASEFF(CurCalYr,RECCL)= STKEFF(CurCalYr,RECCL)
        ENDDO
      ENDIF

!*******************************************************************
!  Calculate New and Average UEC
!*******************************************************************
      DO 10 D=1,mNumCR-2
        DO 10 B=1,mNumBldg
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
         EQCSUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
        ( RTBASEFF(RECSYear,RECCL) / RTBASEFF(CurCalYr,RECCL) )
            IF (CurCalYr.EQ.RECSYear+1) THEN
              IF (WTEQCEFFN(CurCalYr,RECCL,B,D).GT.0.0) THEN
              EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
                  WTEQCEFFN(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)
              ELSE
                EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
              ENDIF
            ELSE
              IF (WTEQCEFFN(CurCalYr,RECCL,B,D).GT.0.0) THEN
                EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
                  WTEQCEFFN(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)
              ELSE
                EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
              ENDIF
            ENDIF

            IF (WTEQCEFFR(CurCalYr,RECCL,B,D).GT.0.0) THEN
              EQCRUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B) * &
                WTEQCEFFR(CurCalYr,RECCL,B,D)*RTBASEFF(RECSYear,RECCL)
            ELSE
              EQCRUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)
            ENDIF

            IF (CurCalYr .EQ. RECSYear+1) THEN
              EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
         ELSE
         TEMP=0.0
           DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
         EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
         EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
           ENDDO
          IF(TEMP.LE.0.0) THEN
             EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
          ELSE
          EQCAUEC(CurCalYr,RECCL,B,D)=0.0
        DO Y=RECSYear,CurCalYr-1
             EQCAUEC(CurCalYr,RECCL,B,D)=EQCAUEC(CurCalYr,RECCL,B,D)+( &
       (EQR90FUT(Y,CurCalYr,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
         EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*EQCNUEC(Y,RECCL,B,D)))/TEMP
        ENDDO
           ENDIF
          ENDIF
          ENDDO
 10   CONTINUE

!*******************************************************************
!  CALCULATE AVERAGE EFFICIENCY
!*******************************************************************
      DO 30 B=1,mNumBldg
        DO 30 D=1,mNumCR-2
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)

            IF (CurCalYr.EQ.RECSYear+1) THEN
              WTEQCEFFA(RECSYear+1,RECCL,B,D)=WTEQCEFFN(RECSYear+1,RECCL,B,D)

            ELSE
             TEMP=0.0
            DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
         EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
         EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
           ENDDO
              IF (TEMP .GT. 0.0) THEN
                WTEQCEFFA(CurCalYr,RECCL,B,D)=0.0
       DO Y=RECSYear,CurCalYr-1
      WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFA(CurCalYr,RECCL,B,D)+( &
       (EQR90FUT(Y,CurCalYr,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
         EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D)))/TEMP
       ENDDO
              ELSE
                WTEQCEFFA(CurCalYr,RECCL,B,D)= &
                WTEQCEFFN(CurCalYr,RECCL,B,D)
              ENDIF
            ENDIF
          ENDDO
30    CONTINUE

!*******************************************************************
!  Calculate Dryer Consumption
!*******************************************************************
      DO 20 D=1,mNumCR-2
        DO 20 FCON=1,NDRYFL
          DRYCON(CurIYr,FCON,D)=0.0
         DO 20 B=1,mNumBldg
          DRYCONIN(CurIYr,FCON,D,B)=0.
          Driver(CurIYr,FCON,D,B)=0.
          DRYCONWT(CurIYr,FCON,D,B)=0.
 20   CONTINUE
!*******************************************************************
      DO 100 D=1,mNumCR-2
        DO 100 B=1,mNumBldg
           DO 100 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            E=RTCLEQCL(RECCL)
            F=RTFUEL(RECCL)
            FCON=FDRYCON(F)
            IF (CurCalYr.EQ.RECSYear+1) THEN
              DRYCON(CurIYr,FCON,D)=DRYCON(CurIYr,FCON,D)+LEAPYR* ( &
               (EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D)) + &
               (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))) &
             * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

              DRYCONWT(CurIYr,FCON,D,B)=DRYCONWT(CurIYr,FCON,D,B)+ LEAPYR* ( &
               (EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D)) + &
               (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))) &
             * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

       IF(EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+     &
                 EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D).GT.0.) THEN
              DRYCONIN(CurIYr,FCON,D,B)=DRYCONIN(CurIYr,FCON,D,B)+(      (&
               (EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D)) + &
               (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))) )

         Driver(CurIYr,fcon,d,b)=Driver(CurIYr,fcon,d,b)+                   &
                (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+     &
                 EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D) )
       ENDIF

              EQCEQCN(CurIYr,RECCL,B,D)=LEAPYR* ( &
               (EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
               (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
             (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))) &
            * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
            ELSE

           IF ((CurCalYr.GT.2010).AND.(STIMULUS.EQ.1).AND.(F.EQ.4)) THEN
             ALPHA=-0.30
           ELSE
             ALPHA=-0.15
           ENDIF

              DRYCON(CurIYr,FCON,D)=DRYCON(CurIYr,FCON,D)+LEAPYR*( &
               (EQCESE(CurCalYr,RECCL,B,D) *EQCSUEC(CurCalYr,RECCL,B,D))+ &
               (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
               (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)) ) &
              * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

              DRYCONWT(CurIYr,FCON,D,B)=DRYCONWT(CurIYr,FCON,D,B)+ LEAPYR*( &
               (EQCESE(CurCalYr,RECCL,B,D) *EQCSUEC(CurCalYr,RECCL,B,D))+ &
               (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
               (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)) ) &
              * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

       IF(EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+     &
                 EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
                 EQCSR90(CurCalYr,RECCL,b,D)+EQCREP(CurCalYr,RECCL,b,D)+    &
                 EQCSUR(CurCalYr,RECCL,b,D).GT.0.) THEN
              DRYCONIN(CurIYr,FCON,D,B)=DRYCONIN(CurIYr,FCON,D,B)+ (     ( &
               (EQCESE(CurCalYr,RECCL,B,D) *EQCSUEC(CurCalYr,RECCL,B,D))+ &
               (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
               (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)) ) )

         Driver(CurIYr,fcon,d,b)=Driver(CurIYr,fcon,d,b)+                   &
                (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+     &
                 EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
                 EQCSR90(CurCalYr,RECCL,b,D)+EQCREP(CurCalYr,RECCL,b,D)+    &
                 EQCSUR(CurCalYr,RECCL,b,D))
         ENDIF

              EQCEQCN(CurIYr,RECCL,B,D)=LEAPYR* ( &
               (EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
               (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
             (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
               (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)) ) &
            * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
            ENDIF
 100  CONTINUE

      DO R=1,mNumCR-2
        DO FCON=1,NDRYFL
         DO B=1,mNumBldg
          IF (Driver(CurIYr,FCON,R,B).GT.0)   &
           DRYCONIN(CurIYr,FCON,R,B)=                &
           DRYCONIN(CurIYr,FCON,R,B)/Driver(CurIYr,FCON,R,B)
         ENDDO
        ENDDO
      ENDDO

      END SUBROUTINE RDRYCON


!*******************************************************************
!     REFRIGERATOR CHOICE SUBROUTINE
!*******************************************************************
      SUBROUTINE RREFTEC
      IMPLICIT NONE
      REAL*4 TMFSHR(mNumBldg,mNumCR-2)   !share for top-mounted freezers
      REAL*4 SMFSHR(mNumBldg,mNumCR-2)   !share for side-mounted freezers
      REAL*4 BMFSHR(mNumBldg,mNumCR-2)   !share for bottom-mounted freezers
      REAL*4 DECAY,OPCOST1,LFCYCLE,FACTOR,UEC(MNUMRTTY)
      REAL*4 EQWTN(nRefrTypes,mNumBldg,mNumCR),EQWTR(nRefrTypes,mNumBldg,mNumCR), &  !EqpParam
       TOTEWTN(nRefrClasses,mNumBldg,mNumCR),TOTEWTR(nRefrClasses,mNumBldg,mNumCR)  !EqpParam
      REAL*4 DENOM, DENOM2, SUM
      REAL*4 EQCOST,CAPITAL,RETAIL
      INTEGER R,F,B,EU,EUPR,RECTY,EQT,TYPE,RECCL,EQC,COUNT,L
      INTEGER RECAR(nRefrTypes),EQTAR(nRefrTypes)  !EqpParam
!*******************************************************************
!   SET EU = 8 TO SEARCH THE FOOD REFIGERATION SECTION OF THE DATA
!*******************************************************************
      EU=8
      EUPR=6
      ALPHA1=-0.50

!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,mNumCR-2
        PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
      ENDDO

!    ASSIGN FACTOR TO CONVERT FROM REFRIGERATOR EFFICIENCY TO UEC
      FACTOR=.003412

!     COMPUTE DECAY RATE USED TO COMPUTE LIFE CYCLE COST (FIRST ITERATION ONLY)

      IF(CURITR.EQ.1) THEN
        DECAY = (1-((1+ResDiscountRate)**(-Tenure)))/ResDiscountRate
      ENDIF

!   ZERO OUT ARRAYS
      DO 5 R=1,mNumCR-2
        DO 5 B=1,mNumBldg
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            TOTEWTN(EQC,B,R)=0.0
            TOTEWTR(EQC,B,R)=0.0
            WTEQCEFFN(CurCalYr,RECCL,B,R)=0.0
            WTEQCEFFR(CurCalYr,RECCL,B,R)=0.0
          ENDDO
 5    CONTINUE

!*******************************************************************
!     CALCULATE OPERATING COSTS
!     CALCULATE LIFE CYCLE COSTS
!     CALCULATE EQUIPMENT WEIGHT & TOTAL EQUIPMENT WEIGHT
!*******************************************************************
       DO 50 R=1,mNumCR-2
        DO 50 B=1,mNumBldg
          DO 50 RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              EQT  =RTEQTYPE(RECTY)
              EQC  =RTTYEQCL(RECTY)
              RECCL=RTCLEUPT(EU)+EQC
              F    =RTFUEL(RECCL)
              UEC(RECTY)=RTEQEFF(RECTY)*FACTOR

                !  If COSTTRSW = 1, use function EQCOST to compute capital
                !     cost of new equipment.
                !  If COSTTRSW = 0, use constant value from RSMEQP file for capital
                !     cost of new equipment.

                     IF (COSTTRSW.EQ.1) THEN
                       CAPITAL = EQCOST(RECTY,CurCalYr,"CAP")
                     ELSE
                       CAPITAL = RTEQCOST(RECTY)
                     ENDIF

                     !     CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOR CHANGES

                     IF ((CurCalYr.GT.2008).AND. &	!kj - 2008 marks first year before American Clean Energy and Security Act of 2009 (Waxman-Markey bill)?
                              (PRICES(F,R,CurCalYr).GT.PRICES(F,R,RECSYear))) THEN
                      HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
                      ELIGBLE=HRDRATE - 0.07
                          IF (ELIGBLE.GT.0.0) THEN
                            HRDADJ= ELIGBLE * &
                              ((PRICES(4,R,CurCalYr)/PRICES(4,R,RECSYear))**ALPHA1 )

                            BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
                           ELSE
                            BETA1DR(RECTY)=RTECBTA1(RECTY)
                          ENDIF  !eligible .GT.0
                       ELSE
                        BETA1DR(RECTY)=RTECBTA1(RECTY)
                     ENDIF    !CurCalYr .GT. 2008


                        OPCOST1=PRICES(F,R,CurCalYr)*UEC(RECTY)

                        LFCYCLE= CAPITAL+(OPCOST1*DECAY)
                        EQWTN(EQT,B,R)=EXP (RTECBIAS(RECTY)+ &
                         (BETA1DR(RECTY)*CAPITAL)+ &
                         (RTECBTA2(RECTY)*OPCOST1)+ &
                         (RTECBTA3(RECTY)*LFCYCLE))
                        TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)
                        EQWTR(EQT,B,R)=EXP (RTECBIAS(RECTY)+ &
                         (BETA1DR(RECTY)*CAPITAL)+ &
                         (RTECBTA2(RECTY)*OPCOST1)+ &
                         (RTECBTA3(RECTY)*LFCYCLE))
                        TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)
!              ENDIF  !.NE.'REF_TTD'
            ENDIF   !census division check
          ENDIF   !year validity check
 50   CONTINUE


!*******************************************************************
!     CALCULATE NEW AND REPLACEMENT MARKET SHARES
!*******************************************************************
      DO R=1,mNumCR-2
        DO B=1,mNumBldg
          TMFSHR(B,R) = 0.
          BMFSHR(B,R) = 0.
          SMFSHR(B,R) = 0.
          TYPE = RTTYPECT(EU)

!*******************************************************************
!     RTTYEUPT(EU)   = LAST RECORD IN CLOTHES DRYING (EU=7)
!     RTTYEUPT(EU+1) = LAST RECORD IN REFRIGERATION (EU=8)
!     RECTY          = RECORD NUMBER IN RSMEQP FILE
!*******************************************************************
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
              IF (RTCENDIV(RECTY).EQ.R) THEN
                TYPE=TYPE+1
                EQT=RTEQTYPE(RECTY)
                EQC=RTTYEQCL(RECTY)
                RECCL=RTCLEUPT(EU)+EQC
                IF(EQT.LE.4) THEN  !refers to xlRTEQTYPE instances of REFR_TF in RSMEQP
                  TMFSHR(B,R) = TMFSHR(B,R) + EQWTN(EQT,B,R)
                ELSEIF (EQT.GE.5 .AND. EQT.LE.8) THEN  !refers to xlRTEQTYPE instances of REFR_SF in RSMEQP
                  SMFSHR(B,R) = SMFSHR(B,R) + EQWTN(EQT,B,R)
                ELSE  !refers to xlRTEQTYPE instances of REFR_BF in RSMEQP
                  BMFSHR(B,R) = BMFSHR(B,R) + EQWTN(EQT,B,R)
                ENDIF !EQT
              ENDIF !RTCENDIV
            ENDIF !CurCalYr
          ENDDO !RECTY
        ENDDO !B
      ENDDO !R

!*******************************************************************
!     CALCULATE NEW AND REPLACEMENT MARKET SHARES
!*******************************************************************
!      WRITE(9,*) 'refrigerator efficiencies, new and replacement'
      DO 70 R=1,mNumCR-2
        DO 70 B=1,mNumBldg
          TYPE = RTTYPECT(EU)

!*******************************************************************
!     RTTYEUPT(EU)   = LAST RECORD IN CLOTHES DRYING (EU=7)
!     RTTYEUPT(EU+1) = LAST RECORD IN REFRIGERATION (EU=8)
!     RECTY          = RECORD NUMBER IN RSMEQP FILE
!*******************************************************************
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) &
              THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              TYPE=TYPE+1
              EQT=RTEQTYPE(RECTY)
              EQC=RTTYEQCL(RECTY)
              RECCL=RTCLEUPT(EU)+EQC
               IF(EQT.LE.4) THEN  !refers to xlRTEQTYPE instances of REFR_TF in RSMEQP
                 NEQTSHR(CurCalYr,TYPE,B,R)=(EQWTN(EQT,B,R)/TMFSHR(B,R))*TMF_SHR
                 REQTSHR(CurCalYr,TYPE,B,R)= NEQTSHR(CurCalYr,TYPE,B,R) !choices the same for refrigerators
                ELSEIF (EQT.GE.5 .AND. EQT.LE.8) THEN  !refers to xlRTEQTYPE instances of REFR_SF in RSMEQP
                 NEQTSHR(CurCalYr,TYPE,B,R)=(EQWTN(EQT,B,R)/SMFSHR(B,R))*SMF_SHR
                 REQTSHR(CurCalYr,TYPE,B,R)= NEQTSHR(CurCalYr,TYPE,B,R) !choices the same for refrigerators
                ELSE  !refers to instances xlRTEQTYPE of REFR_BF in RSMEQP
                 NEQTSHR(CurCalYr,TYPE,B,R)=(EQWTN(EQT,B,R)/BMFSHR(B,R))*BMF_SHR
                 REQTSHR(CurCalYr,TYPE,B,R)= NEQTSHR(CurCalYr,TYPE,B,R) !choices the same for refrigerators
               ENDIF ! EQT filters for different types of refrigerators
            ENDIF
           ENDIF
          ENDDO

!*******************************************************************
!     CALCULATE WEIGHTED EFFICIENCY FOR NEW AND REPLACEMENT EQUIPMENT
!*******************************************************************
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC   =RTCLEQCL(RECCL)
            DENOM =0
            DENOM2=0
            COUNT =0

!     TYPE = INDEX FOR TYPE ARRAYS NEQTSHR AND REQTSHR
!            INITIALIZE TO LAST ARRAY POSTION IN PREVIOUS END USE AND THEN COUNT VALID TYPES IN CURRENT END USE

            TYPE = RTTYPECT(EU)
            DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
              IF(CurCalYr.GE.RTINITYR(RECTY) &
                .AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
                TYPE=TYPE+1
                IF(RTTYEQCL(RECTY).EQ.EQC) THEN
                  COUNT=COUNT+1
                  EQT=RTEQTYPE(RECTY)
                  RECAR(COUNT)=RECTY
                  EQTAR(COUNT)=TYPE
                  DENOM=DENOM+NEQTSHR(CurCalYr,TYPE,B,R)
                  DENOM2=DENOM2+REQTSHR(CurCalYr,TYPE,B,R)
                ENDIF  ! rttyeqcl=eqc filter refrigerators
               ENDIF   ! rtcendiv=r   filter regions
              ENDIF    ! filter years
            ENDDO      ! process RSMEQP records

!    COMPLETE CALCULATION FOR NEW EQUIPMENT

            IF(DENOM.LE.0.0) THEN
              WTEQCEFFN(CurCalYr,RECCL,B,R)=RTBASEFF(RECSYear,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                SUM=SUM+(NEQTSHR(CurCalYr,TYPE,B,R)*RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFN(CurCalYr,RECCL,B,R)=SUM/DENOM
            ENDIF
!          WRITE(9,5006) r,b,type,CurCalYr,wteqceffn(CurCalYr,RECCL,b,r),recty,RECCL
! 5006     FORMAT("new  r= ",i3," b= ",i3," type= ",2i5,f10.4,2i6)

!     COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT
            IF(DENOM2.LE.0.0) THEN
              WTEQCEFFR(CurCalYr,RECCL,B,R)=RTBASEFF(RECSYear,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                SUM=SUM+(REQTSHR(CurCalYr,TYPE,B,R)*RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFR(CurCalYr,RECCL,B,R)=SUM/DENOM2
            ENDIF
!          WRITE(9,5007) r,b,type,CurCalYr,wteqceffr(CurCalYr,RECCL,b,r),recty,RECCL
! 5007     FORMAT("rep  r= ",i3," b= ",i3," type= ",2i5,f10.4,2i6)

          ENDDO  !eqc = refrigerators

 70   CONTINUE  !end loop r, end loop b

      END SUBROUTINE RREFTEC


!*******************************************************************
!     REFRIGERATORS ADDED SUBROUTINE
!*******************************************************************
      SUBROUTINE RREFADD
      IMPLICIT NONE
      REAL*4 SA, HSR, ESR, SVRTE
      INTEGER EU,EQC,RECCL,Y,R,B,TEMP,EV,EQT,NUMEQT,TYPE, &
              RECTY,V
!*******************************************************************
!   EU       = 8 IS FOOD REFRIGERATION

      EV       = 8
      EU       = 8
!*******************************************************************
!  CALCULATE REFRIGERATORS ADDED IN CurCalYr (CurCalYr-1)
!  CUMULATE SURVIVING EQUIPMENT REPLACED FOR RECS-YEAR VINTAGE PRIOR TO
!   CurCalYr
!*******************************************************************
! CUMULATE SURVIVING NEW REFRIGERATORS ADDED PRIOR TO CurCalYr
!   TO ESTIMATE NH
! SA REPRESENTS NH at CurCalYr-1
! CUMULATE SURVIVING NEW REFRIGS ADDED & REPLACED PRIOR TO CurCalYr
! REPLACEMENT EQUIPMENT = SURV.HOUSES(SA) - SURV.EQUIP(EQCSUR-REFRIGERATORS)
!*******************************************************************
      DO 15 R=1,mNumCR-2
        DO 15 B=1,mNumBldg
          DO 15 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQC=RTCLEQCL(RECCL)
             EQCADD(CurCalYr,RECCL,B,R)=(HSEADD(CurCalYr,B,R)*REFSAT(B,R))

!   ZERO OUT ARRAYS AND VARIABLE
           EQCSR90(CurCalYr,RECCL,B,R)=0.0
           EQCSUR(CurCalYr,RECCL,B,R)=0.0
           EQCRP90RP(CurCalYr,RECCL,B,R)=0.0
           SA=0.0
!******************************************************************
!  Calculate replacement equipment from original base year stock
!******************************************************************
      IF (CurCalYr.EQ.RECSYear+1) THEN
      EQCRP90(CurCalYr,RECCL,B,R)=(EQCRET(CurCalYr,RECCL) &
        *EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
      ELSE
      EQCRP90(CurCalYr,RECCL,B,R)=((EQCRET(CurCalYr,RECCL)- &
      EQCRET(CurCalYr-1,RECCL))*EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear))))
      ENDIF

!   COMPUTE AND VINTAGE REPLACEMENTS OF REPLACEMENTS
           IF(CurCalYr.GT.RECSYear+1) THEN
              DO Y=RECSYear+1,CurCalYr-1
                TEMP=CurCalYr-Y
              ESR=SVRTE(RTALPHA(RECCL),TEMP-1,RTK(RECCL),RTLAMBDA(RECCL)) &
                   -SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
         EQCRP90RP(CurCalYr,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R) + &
          ( EQCRP90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
            EQCRP90RP(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
              ENDDO
           ENDIF

           IF(CurCalYr.GT.RECSYear+1) THEN
              DO Y=RECSYear+1,CurCalYr-1
                TEMP=CurCalYr-Y
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
                EQCSR90(CurCalYr,RECCL,B,R)=(EQCSR90(CurCalYr,RECCL,B,R) + &
             ( EQCRP90(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
               EQCRP90RP(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))))
                HSR=HDR(B)**(TEMP)
                SA = (SA + EQCADD(Y,RECCL,B,R)*HSR)
                EQCSUR(CurCalYr,RECCL,B,R) = (EQCSUR(CurCalYr,RECCL,B,R) + &
             ( ((EQCADD(Y,RECCL,B,R)+EQCREP(Y,RECCL,B,R))*(HSR*ESR)) ))
              ENDDO
            ENDIF

!*******************************************************************
! CALCULATE REPLACEMENT REFRIGERATORS FOR NEW VINTAGE IN CurCalYr-1
! NOTE: REPLACES WITH LIKE IF NOT SINGLE-FAMILY HOMES
! NOTE: FOR NEW HOUSES (NH) - CurCalYr-1 IS THE LAGGED VALUE
!*******************************************************************
        EQCREP(CurCalYr,RECCL,B,R)=SA-EQCSUR(CurCalYr,RECCL,B,R)

!         ENDDO
 15     CONTINUE

         DO B=1,mNumBldg
           DO R=1,mNumCR-2
             DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
               EQC=RTCLEQCL(RECCL)
               DO Y=CurCalYr,EndYr       ! VINTAGE EQUIPMENT FOR AVERAGE STOCK ACCOUNTING
                 TEMP=Y-CurCalYr
                 HSR=HDR(B)**(TEMP)
                 ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
                 EQR90FUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90(CurCalYr,RECCL,B,R)*ESR*HSR)
                 EQR90RPFUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R)*ESR*HSR)
                 EQREPFUT(CurCalYr,Y,RECCL,B,R)=(EQCREP(CurCalYr,RECCL,B,R)*ESR*HSR)
                 EQADDFUT(CurCalYr,Y,RECCL,B,R)=(EQCADD(CurCalYr,RECCL,B,R)*ESR*HSR)
               ENDDO !Y
             ENDDO !RECCL
           ENDDO !R
         ENDDO !B

!*******************************************************************
!     AGGREGATE REFRIGERATORS FOR INVESTMENT ANALYSIS
!*******************************************************************
      Y=CurCalYr
      NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)

         DO B=1,mNumBldg
           DO R=1,mNumCR-2
             TYPE=RTTYPECT(EU)
             DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
               !CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
               IF(CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
                 IF (RTCENDIV(RECTY).EQ.R) THEN
                   TYPE=TYPE+1  ! INDEX FOR 'TYPE' VARIABLES
                   EQT=RTEQTYPE(RECTY)
                   EQC=RTTYEQCL(RECTY)
                   RECCL=RTCLEUPT(EU)+EQC
                   HEATINGTYPEPURCH(Y,TYPE,B,R,1)=(NEQTSHR(Y,TYPE,B,r)*EQCADD(Y,RECCL,B,r))
                   HEATINGTYPEPURCH(Y,TYPE,B,R,2)=(NEQTSHR(Y,TYPE,B,r)*(EQCREP(Y,RECCL,B,r) + EQCRP90RP(Y,RECCL,B,r)) + &
                                                   REQTSHR(Y,TYPE,B,r)*EQCRP90(Y,RECCL,B,r) )
                 ENDIF !RTCENDIV
               ENDIF !CurCalYr
             ENDDO !RECTY
           ENDDO !R
         ENDDO !B

       END SUBROUTINE RREFADD


!*******************************************************************
!  REFRIGERATION CONSUMPTION
!*******************************************************************
      SUBROUTINE RREFCON
      IMPLICIT NONE
      REAL*4 ALPHA,ef1,ef2,ef3,TEMP
      INTEGER B, E, EUPR, D,EU,RECCL,EQC,F,Y,R

!*******************************************************************
!   SET EU = 8 TO SEARCH THE FOOD REFIGERATION SECTION OF THE DATA
!*******************************************************************
      EU = 8
      EUPR=6
      alpha=0.0;ef1=.5;ef2=.35;ef3=.15

!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,mNumCR-2
        PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
      ENDDO

!*******************************************************************
! COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR RECS-YEAR EQUIPMENT
!*******************************************************************
      IF (CurCalYr.GE.RECSYear+1) THEN
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RTBASEFF(CurCalYr,RECCL)= STKEFF(CurCalYr,RECCL)
        ENDDO
      ENDIF

!*******************************************************************
!  Calculate New, REPLACEMENT, AND Average UECS
!*******************************************************************
      DO 10 D=1,mNumCR-2
        DO 10 B=1,mNumBldg
          DO 10 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            EQCSUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
                   ( RTBASEFF(CurCalYr,RECCL) / RTBASEFF(RECSYear,RECCL) )
            EQCNUEC(CurCalYr,RECCL,B,D)=(EQCUEC(D,RECCL,B)* &
             (WTEQCEFFN(CurCalYr,RECCL,B,D)/RTBASEFF(RECSYear,RECCL)))*(1.0/REFSAT(B,D))+ &
               EQCUEC(D,RECCL,B)*(1.0-(1.0/REFSAT(B,D)))
            EQCRUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
             (WTEQCEFFR(CurCalYr,RECCL,B,D)/RTBASEFF(RECSYear,RECCL))
            IF (CurCalYr.EQ.RECSYear+1) THEN
              EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
         ELSE
         TEMP=0.0
           DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
         EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
         EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
           ENDDO
          IF(TEMP.LE.0.0) THEN
             EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
          ELSE
          EQCAUEC(CurCalYr,RECCL,B,D)=0.0
        DO Y=RECSYear,CurCalYr-1
             EQCAUEC(CurCalYr,RECCL,B,D)=EQCAUEC(CurCalYr,RECCL,B,D)+( &
       (EQR90FUT(Y,CurCalYr,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
         EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*EQCNUEC(Y,RECCL,B,D))) &
                                           /TEMP
        ENDDO
           ENDIF
          ENDIF
 10   CONTINUE

!*******************************************************************
!  CALCULATE AVERAGE EFFICIENCY
!*******************************************************************
      DO 30 B=1,mNumBldg
        DO 30 D=1,mNumCR-2
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)

            IF (CurCalYr.EQ.RECSYear+1) THEN
              WTEQCEFFA(RECSYear+1,RECCL,B,D)=WTEQCEFFN(RECSYear+1,RECCL,B,D)

            ELSE
             TEMP=0.0
            DO Y=RECSYear,CurCalYr-1
          TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
         EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
         EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
           ENDDO
              IF (TEMP .GT. 0.0) THEN
                WTEQCEFFA(CurCalYr,RECCL,B,D)=0.0
       DO Y=RECSYear,CurCalYr-1
      WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFA(CurCalYr,RECCL,B,D)+( &
       (EQR90FUT(Y,CurCalYr,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
         EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D))) &
                                           /TEMP
       ENDDO
              ELSE
                WTEQCEFFA(CurCalYr,RECCL,B,D)= &
                  WTEQCEFFN(CurCalYr,RECCL,B,D)
              ENDIF
            ENDIF
          ENDDO
30    CONTINUE

!*******************************************************************
!  Calculate Refrigerator Consumption
!*******************************************************************
      DO 40 D=1,mNumCR-2
            REFCON(CurIYr,D)=0.0
       DO 40 B=1,mNumBldg
            REFCONWT(CurIYr,D,B)=0.
            Driver2(CurIYr,d,B)=0.
            REFCONIN(CurIYr,D,B)=0.
40    CONTINUE

      DO 50 D=1,mNumCR-2
        DO 50 B=1,mNumBldg
          DO 50 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            F  =RTFUEL(RECCL)

            IF (CurCalYr.EQ.RECSYear+1) THEN

              REFCON(CurIYr,D)=REFCON(CurIYr,D)+ LEAPYR*( &
             ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
             (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
             (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) &
             +(EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))) &
            *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

              REFCONWT(CurIYr,D,B)=REFCONWT(CurIYr,D,B)+LEAPYR*( &
             ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
             (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
             (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) &
             +(EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))) &
             *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

       IF(EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+     &
                 EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D).GT.0.) THEN
              REFCONIN(CurIYr,D,B)=REFCONIN(CurIYr,D,B)+(  (&
             ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
             (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
             (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) &
             +(EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))))  )

              Driver2(CurIYr,d,B)=Driver2(CurIYr,d,B)+            &
                (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+     &
                 EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D) )
         ENDIF

              EQCEQCN(CurIYr,RECCL,B,D)= LEAPYR*( &
             ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
             (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
             (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) &
             +(EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))) &
            *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

            ELSE

              REFCON(CurIYr,D)=REFCON(CurIYr,D)+ LEAPYR*( &
             ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
              (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
              (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
            (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
              (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)) &
             +(EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) + &
              (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))) &
            *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR) )

              REFCONWT(CurIYr,D,B)=REFCONWT(CurIYr,D,B)+ LEAPYR* ( &
             ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
              (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
              (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
              (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
              (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)) &
             +(EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) + &
              (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))) &
              *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR) )

       IF(EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+     &
                 EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
                 EQCSR90(CurCalYr,RECCL,b,D)+EQCREP(CurCalYr,RECCL,b,D)+    &
                 EQCSUR(CurCalYr,RECCL,b,D).GT.0.) THEN
              REFCONIN(CurIYr,D,B)= REFCONIN(CurIYr,D,B)+( (&
             ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
              (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
              (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
              (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
              (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)) &
             +(EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) + &
              (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))))   )

              Driver2(CurIYr,d,B)=Driver2(CurIYr,d,B)+            &
                (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+     &
                 EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
                 EQCSR90(CurCalYr,RECCL,b,D)+EQCREP(CurCalYr,RECCL,b,D)+    &
                 EQCSUR(CurCalYr,RECCL,b,D))
           ENDIF

              EQCEQCN(CurIYr,RECCL,B,D)= LEAPYR*( &
             ((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
              (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
              (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
            (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
              (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)) &
             +(EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D)) + &
              (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))) &
            *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))
          ENDIF
 50   CONTINUE

      DO R=1,mNumCR-2
        DO B=1,mNumBldg
        IF (Driver2(CurIYr,R,B).GT.0)   &
           REFCONIN(CurIYr,R,B)=        &
           REFCONIN(CurIYr,R,B)/Driver2(CurIYr,R,B)
        ENDDO
      ENDDO

      END SUBROUTINE RREFCON


!*******************************************************************
!     STANDALONE FREEZER CHOICE SUBROUTINE
!*******************************************************************
      SUBROUTINE RFRZTEC
      IMPLICIT NONE
      REAL*4 CHSHR(mNumBldg,mNumCR-2)  ! share for chest freezers
      REAL*4 UPSHR(mNumBldg,mNumCR-2)  ! share for upright freezers
      REAL*4 DECAY,OPCOST2,LFCYCLE1,FACTOR,UEC(MNUMRTTY)
      REAL*4 EQWTN(nFrezTypes,mNumBldg,mNumCR),EQWTR(nFrezTypes,mNumBldg,mNumCR), &  !EqpParam
       TOTEWTN(nFrezClasses,mNumBldg,mNumCR),TOTEWTR(nFrezClasses,mNumBldg,mNumCR)  !EqpParam
      REAL*4 DENOM,DENOM2,SUM
      REAL*4 EQCOST,CAPITAL,RETAIL
      INTEGER R,F,B,EU,EUPR,RECTY,EQT,TYPE,RECCL,EQC,COUNT,L
      INTEGER RECAR(nFrezTypes),EQTAR(nFrezTypes)  !EqpParam

!*******************************************************************
!   SET EU = 9 TO SEARCH THE STANDALONE FREEZER SECTION OF THE DATA
!*******************************************************************
      EU=9
      EUPR=7

!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,mNumCR-2
        PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
      ENDDO

      ALPHA1=-0.50

!    ASSIGN FACTOR TO CONVERT FROM FREEZER EFFICIENCY TO UEC
      FACTOR=.003412

!     COMPUTE DECAY RATE USED TO COMPUTE LIFE CYCLE COST (FIRST ITERATION ONLY)
      IF(CURITR.EQ.1) THEN
        DECAY = (1-((1+ResDiscountRate)**(-Tenure)))/ResDiscountRate
      ENDIF

!   ZERO OUT ARRAYS
      DO 5 R=1,mNumCR-2
        DO 5 B=1,mNumBldg
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            TOTEWTN(EQC,B,R)=0.0
            TOTEWTR(EQC,B,R)=0.0
            WTEQCEFFN(CurCalYr,RECCL,B,R)=0.0
            WTEQCEFFR(CurCalYr,RECCL,B,R)=0.0
          ENDDO
 5    CONTINUE

!*******************************************************************
!     CALCULATE OPERATING COSTS
!     CALCULATE LIFE CYCLE COSTS
!     CALCULATE EQUIPMENT WEIGHT & TOTAL EQUIPMENT WEIGHT
!*******************************************************************
       DO 50 R=1,mNumCR-2
        DO 50 B=1,mNumBldg
          DO 50 RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              EQT  =RTEQTYPE(RECTY)
              EQC  =RTTYEQCL(RECTY)
              RECCL=RTCLEUPT(EU)+EQC
              F    =RTFUEL(RECCL)
              UEC(RECTY)=RTEQEFF(RECTY)*FACTOR

!  If COSTTRSW = 1, use function EQCOST to compute capital
!     cost of new equipment.
!  If COSTTRSW = 0, use constant value from RSMEQP file for capital
!     cost of new equipment.

              IF (COSTTRSW.EQ.1) THEN
                CAPITAL = EQCOST(RECTY,CurCalYr,"CAP")
              ELSE
                CAPITAL = RTEQCOST(RECTY)
              ENDIF

              OPCOST2=PRICES(F,R,CurCalYr)*UEC(RECTY)

!     CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOR CHANGES
      IF ((CurCalYr.GT.2008).AND. &
       (PRICES(4,R,CurCalYr).GT.PRICES(4,R,RECSYear))) THEN
        HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
        ELIGBLE=HRDRATE - 0.07	!kj - source for 0.07?
        IF (ELIGBLE.GT.0.0) THEN
          HRDADJ= ELIGBLE *((PRICES(4,R,CurCalYr)/PRICES(4,R,RECSYear))**ALPHA1 )
          BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)	!kj - source for 0.07?
        ELSE
          BETA1DR(RECTY)=RTECBTA1(RECTY)
        ENDIF
      ELSE
        BETA1DR(RECTY)=RTECBTA1(RECTY)
      ENDIF

!    CALCULATE LIFE CYCLE COSTS
     LFCYCLE1= CAPITAL+(OPCOST2*DECAY)

                EQWTN(EQT,B,R)=EXP (RTECBIAS(RECTY)+ &
                 (BETA1DR(RECTY)*CAPITAL)+ &
                 (RTECBTA2(RECTY)*OPCOST2)+ &
                 (RTECBTA3(RECTY)*LFCYCLE1))
                TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)
                EQWTR(EQT,B,R)=EXP (RTECBIAS(RECTY)+ &
                 (BETA1DR(RECTY)*CAPITAL)+ &
                 (RTECBTA2(RECTY)*OPCOST2)+ &
                 (RTECBTA3(RECTY)*LFCYCLE1))
                TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)
              ENDIF
           ENDIF
       50  CONTINUE

!*******************************************************************
!     CALCULATE NEW AND REPLACEMENT MARKET SHARES
!*******************************************************************
      DO R=1,mNumCR-2
        DO B=1,mNumBldg
          CHSHR(B,R)=0.
          UPSHR(B,R)=0.
          TYPE = RTTYPECT(EU)

!*******************************************************************
!     RECTY          = RECORD NUMBER IN RSMEQP FILE
!     RTTYEUPT(EU)   = LAST RECORD IN REFRIGERATION (EU=8)
!     RTTYEUPT(EU+1) = LAST RECORD IN STANDALONE FREEZERS (EU=9)
!*******************************************************************
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
              IF (RTCENDIV(RECTY).EQ.R) THEN
                TYPE=TYPE+1
                EQT=RTEQTYPE(RECTY)
                EQC=RTTYEQCL(RECTY)
                RECCL=RTCLEUPT(EU)+EQC
                IF(EQT.LE.4) THEN  !refers to xlRTEQTYPE instances of FREZ_C in RSMEQP
                  CHSHR(B,R) = CHSHR(B,R) + EQWTN(EQT,B,R)
                ELSE  !refers to xlRTEQTYPE instances of FREZ_U in RSMEQP
                  UPSHR(B,R) = UPSHR(B,R) + EQWTN(EQT,B,R)
                ENDIF
              ENDIF !RTCENDIV
            ENDIF !CurCalYr
          ENDDO !RECTY
        ENDDO !B
      ENDDO !R

!*******************************************************************
!     CALCULATE NEW AND REPLACEMENT MARKET SHARES
!*******************************************************************
      DO 70 R=1,mNumCR-2
        DO 70 B=1,mNumBldg
          TYPE = RTTYPECT(EU)

!*******************************************************************
!     RECTY          = RECORD NUMBER IN RSMEQP FILE
!     RTTYEUPT(EU)   = LAST RECORD IN REFRIGERATION (EU=8)
!     RTTYEUPT(EU+1) = LAST RECORD IN STANDALONE FREEZERS (EU=9)
!*******************************************************************
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CurCalYr.GE.RTINITYR(RECTY).AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
              IF (RTCENDIV(RECTY).EQ.R) THEN
                TYPE=TYPE+1
                EQT=RTEQTYPE(RECTY)
                EQC=RTTYEQCL(RECTY)
                RECCL=RTCLEUPT(EU)+EQC
                IF(EQT.LE.4) THEN  !refers to xlRTEQTYPE instances of FREZ_C in RSMEQP
                  NEQTSHR(CurCalYr,TYPE,B,R)=(EQWTN(EQT,B,R)/CHSHR(B,R))*CH_SHR
                  REQTSHR(CurCalYr,TYPE,B,R)= NEQTSHR(CurCalYr,TYPE,B,R) !choices the same for freezers
                ELSE  !refers to xlRTEQTYPE instances of FREZ_U in RSMEQP
                  NEQTSHR(CurCalYr,TYPE,B,R)=(EQWTN(EQT,B,R)/UPSHR(B,R))*UP_SHR
                  REQTSHR(CurCalYr,TYPE,B,R)= NEQTSHR(CurCalYr,TYPE,B,R) !choices the same for freezers
                ENDIF !EQT
              ENDIF !RTCENDIV
            ENDIF !CurCalYr
          ENDDO !RECTY

!*******************************************************************
!     CALCULATE WEIGHTED EFFICIENCY FOR NEW EQUIPMENT AND
!     CALCULATE WEIGHTED EFFICIENCY FOR REPLACEMENT EQUIPMENT
!*******************************************************************
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC   =RTCLEQCL(RECCL)
            DENOM =0
            DENOM2=0
            COUNT =0

!     TYPE = INDEX FOR TYPE ARRAYS NEQTSHR AND REQTSHR
!            INITIALIZE TO LAST ARRAY POSTION IN PREVIOUS END USE AND THEN COUNT VALID TYPES IN CURRENT END USE
            TYPE = RTTYPECT(EU)
	
!*******************************************************************
!     RECTY          = RECORD NUMBER IN RSMEQP FILE
!     RTTYEUPT(EU)   = LAST RECORD IN REFRIGERATION (EU=8)
!     RTTYEUPT(EU+1) = LAST RECORD IN STANDALONE FREEZERS (EU=9)
!*******************************************************************
            DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
              IF(CurCalYr.GE.RTINITYR(RECTY) &
                .AND.CurCalYr.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
                TYPE=TYPE+1
                IF(RTTYEQCL(RECTY).EQ.EQC) THEN
                  COUNT=COUNT+1
                  EQT=RTEQTYPE(RECTY)
                  RECAR(COUNT)=RECTY
                  EQTAR(COUNT)=TYPE
                  DENOM=DENOM+NEQTSHR(CurCalYr,TYPE,B,R)
                  DENOM2=DENOM2+REQTSHR(CurCalYr,TYPE,B,R)
                ENDIF
              ENDIF
             ENDIF
            ENDDO

!    COMPLETE CALCULATION FOR NEW EQUIPMENT

            IF(DENOM.LE.0.0) THEN
              WTEQCEFFN(CurCalYr,RECCL,B,R)=RTBASEFF(RECSYear,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                SUM=SUM+(NEQTSHR(CurCalYr,TYPE,B,R)*RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFN(CurCalYr,RECCL,B,R)=SUM/DENOM
            ENDIF

!     COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT

            IF(DENOM2.LE.0.0) THEN
              WTEQCEFFR(CurCalYr,RECCL,B,R)=RTBASEFF(RECSYear,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                SUM=SUM+(REQTSHR(CurCalYr,TYPE,B,R)*RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFR(CurCalYr,RECCL,B,R)=SUM/DENOM2
            ENDIF

          ENDDO

 70   CONTINUE
      END SUBROUTINE RFRZTEC


!*******************************************************************
!     FREEZERS ADDED SUBROUTINE
!*******************************************************************
      SUBROUTINE RFRZADD
      IMPLICIT NONE
      REAL*4 SA, HSR, ESR, SVRTE, FZRPFAC
      INTEGER EU,EQC,RECCL,Y,R,B,TEMP,EQT,NUMEQT,TYPE, &
              EV,RECTY,V
!*******************************************************************
!     EU      = 9 IS FOOD FREEZING
!     FZRPFAC = FACTOR AFFECTING FREEZER REPLACEMENTS
!*******************************************************************
      EV      = 9
      EU      = 9
      FZRPFAC = 1.0

!*******************************************************************
!  CALCULATE FREEZERS ADDED IN CurCalYr (CurCalYr-1)
!  CUMULATE SURVIVING EQUIPMENT REPLACED FOR RECS-YEAR VINTAGE PRIOR TO
!   CurCalYr
!*******************************************************************
! CUMULATE SURVIVING NEW FREEZERS ADDED PRIOR TO CurCalYr
!   TO ESTIMATE NH
! SA REPRESENTS NH at CurCalYr-1
! CUMULATE SURVIVING NEW FREEZERS ADDED & REPLACED PRIOR TO CurCalYr
! REPLACEMENT EQUIPMENT = SURV.HOUSES(SA) - SURV.EQUIP(EQCSUR-REFRIGERATORS)
!*******************************************************************
      DO 15 R=1,mNumCR-2
        DO 15 B=1,mNumBldg
          DO 15 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQC=RTCLEQCL(RECCL)
           EQCADD(CurCalYr,RECCL,B,R)=(HSEADD(CurCalYr,B,R)*FRZSAT(B,R))

!   ZERO OUT ARRAYS AND VARIABLE
           EQCSR90(CurCalYr,RECCL,B,R)=0.0
           EQCRP90RP(CurCalYr,RECCL,B,R)=0.0
           EQCSUR(CurCalYr,RECCL,B,R)=0.0
           SA=0.0

!******************************************************************
!  Calculate replacement equipment from original base year stock
!******************************************************************
      IF (CurCalYr.EQ.RECSYear+1) THEN
        EQCRP90(CurCalYr,RECCL,B,R)=(EQCRET(CurCalYr,RECCL) &
         *EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear)))*FZRPFAC)
      ELSE
        EQCRP90(CurCalYr,RECCL,B,R)=((EQCRET(CurCalYr,RECCL)- &
         EQCRET(CurCalYr-1,RECCL))*EQCESE(RECSYear,RECCL,B,R)*(HDR(B)**(CurCalYr-(RECSYear)))*FZRPFAC)
      ENDIF

!   COMPUTE AND VINTAGE REPLACEMENTS OF REPLACEMENTS

           IF(CurCalYr.GT.RECSYear+1) THEN
              DO Y=RECSYear+1,CurCalYr-1
                TEMP=CurCalYr-Y
              ESR=SVRTE(RTALPHA(RECCL),TEMP-1,RTK(RECCL),RTLAMBDA(RECCL)) &
                   -SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
         EQCRP90RP(CurCalYr,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R) + &
          ( EQCRP90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
            EQCRP90RP(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
              ENDDO
           ENDIF

           IF(CurCalYr.GT.RECSYear+1) THEN
              DO Y=RECSYear+1,CurCalYr-1
                TEMP=CurCalYr-Y
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL), &
                          RTLAMBDA(RECCL))
                EQCSR90(CurCalYr,RECCL,B,R)=(EQCSR90(CurCalYr,RECCL,B,R) + &
             ( EQCRP90(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
               EQCRP90RP(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))))
                HSR=HDR(B)**(TEMP)
                SA = (SA + EQCADD(Y,RECCL,B,R)*HSR)
                EQCSUR(CurCalYr,RECCL,B,R) = (EQCSUR(CurCalYr,RECCL,B,R) + &
             ( ((EQCADD(Y,RECCL,B,R)+EQCREP(Y,RECCL,B,R))* &
                 (HSR*ESR)) ))
              ENDDO
            ENDIF

!*******************************************************************
! CALCULATE REPLACEMENT STANDALONE FREEZERS FOR NEW VINTAGE IN CurCalYr-1
! NOTE: REPLACES WITH LIKE IF NOT SINGLE-FAMILY HOMES
! NOTE: FOR NEW HOUSES (NH) - CurCalYr-1 IS THE LAGGED VALUE
!*******************************************************************
         EQCREP(CurCalYr,RECCL,B,R)=SA-EQCSUR(CurCalYr,RECCL,B,R)

 15     CONTINUE

         DO B=1,mNumBldg
           DO R=1,mNumCR-2
             DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
               EQC=RTCLEQCL(RECCL)
               DO Y=CurCalYr,EndYr       ! VINTAGE EQUIPMENT FOR AVERAGE STOCK ACCOUNTING
                 TEMP=Y-CurCalYr
                 HSR=HDR(B)**(TEMP)
                 ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
                 EQR90FUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90(CurCalYr,RECCL,B,R)*ESR*HSR)
                 EQR90RPFUT(CurCalYr,Y,RECCL,B,R)=(EQCRP90RP(CurCalYr,RECCL,B,R)*ESR*HSR)
                 EQREPFUT(CurCalYr,Y,RECCL,B,R)=(EQCREP(CurCalYr,RECCL,B,R)*ESR*HSR)
                 EQADDFUT(CurCalYr,Y,RECCL,B,R)=(EQCADD(CurCalYr,RECCL,B,R)*ESR*HSR)
               ENDDO
             ENDDO
           ENDDO
         ENDDO

!*******************************************************************
!     AGGREGATE STANDALONE FREEZERS FOR INVESTMENT ANALYSIS
!*******************************************************************
      Y=CurCalYr
      NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)

         DO B=1,mNumBldg
           DO r=1,mNumCR-2
             TYPE=RTTYPECT(EU)

!*******************************************************************
!     RECTY          = RECORD NUMBER IN RSMEQP FILE
!     RTTYEUPT(EU)   = LAST RECORD IN REFRIGERATION (EU=8)
!     RTTYEUPT(EU+1) = LAST RECORD IN STANDALONE FREEZERS (EU=9)
!*******************************************************************
             DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)

!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr

               IF(CurCalYr.GE.RTINITYR(RECTY).AND. &
                 CurCalYr.LE.RTLASTYR(RECTY)) THEN
                 IF (RTCENDIV(RECTY).EQ.R) THEN
                   TYPE=TYPE+1                  ! INDEX FOR 'TYPE' VARIABLES
                   EQT=RTEQTYPE(RECTY)
                   EQC=RTTYEQCL(RECTY)
                   RECCL=RTCLEUPT(EU)+EQC
                   HEATINGTYPEPURCH(Y,TYPE,B,R,1)=(NEQTSHR(Y,TYPE,B,r)*EQCADD(Y,RECCL,B,r))
                   HEATINGTYPEPURCH(Y,TYPE,B,R,2)=(NEQTSHR(Y,TYPE,B,r)*(EQCREP(Y,RECCL,B,r) + EQCRP90RP(Y,RECCL,B,r)) + &
                    REQTSHR(Y,TYPE,B,r)*EQCRP90(Y,RECCL,B,r)  )
                 ENDIF
               ENDIF
             ENDDO
           ENDDO
         ENDDO

END SUBROUTINE RFRZADD


!*******************************************************************
!  FREEZER CONSUMPTION
!*******************************************************************
      SUBROUTINE RFRZCON
      IMPLICIT NONE
      REAL*4 ALPHA,ef1,ef2,ef3,TEMP
      INTEGER B,E,EUPR,D,EU,RECCL,EQC,F,Y,R

!*******************************************************************
!   SET EU = 9 TO SEARCH THE STANDALONE FREEZING SECTION OF THE DATA
!*******************************************************************
      EU=9
      EUPR=7
      alpha=0.0;ef1=.5;ef2=.35;ef3=.15

!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,mNumCR-2
        PRICES(4,R,CurCalYr)=PELRSOUT(R,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
      ENDDO

!*******************************************************************
! COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR RECS-YEAR EQUIPMENT
!*******************************************************************
      IF (CurCalYr.GE.RECSYear+1) THEN
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RTBASEFF(CurCalYr,RECCL)= STKEFF(CurCalYr,RECCL)
        ENDDO
      ENDIF

!*******************************************************************
!  Calculate New, Replacement, and Average UECs
!*******************************************************************
      DO 10 D=1,mNumCR-2
        DO 10 B=1,mNumBldg
          DO 10 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            EQCSUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
             (RTBASEFF(CurCalYr,RECCL)/RTBASEFF(RECSYear,RECCL))
            EQCNUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
             (WTEQCEFFN(CurCalYr,RECCL,B,D)/RTBASEFF(RECSYear,RECCL))
            EQCRUEC(CurCalYr,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
             (WTEQCEFFR(CurCalYr,RECCL,B,D)/RTBASEFF(RECSYear,RECCL))
            IF (CurCalYr.EQ.RECSYear+1) THEN
              EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
            ELSE
              TEMP=0.0
              DO Y=RECSYear,CurCalYr-1
                TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
                EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
                 EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
              ENDDO
              IF(TEMP.LE.0.0) THEN
                EQCAUEC(CurCalYr,RECCL,B,D)=EQCNUEC(CurCalYr,RECCL,B,D)
              ELSE
                EQCAUEC(CurCalYr,RECCL,B,D)=0.0
                DO Y=RECSYear,CurCalYr-1
                  EQCAUEC(CurCalYr,RECCL,B,D)=EQCAUEC(CurCalYr,RECCL,B,D)+ &
                   ((EQR90FUT(Y,CurCalYr,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D))+ &
                   ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
                   EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*EQCNUEC(Y,RECCL,B,D)))/TEMP
                ENDDO
              ENDIF
            ENDIF
      10  CONTINUE

!*******************************************************************
!  CALCULATE AVERAGE EFFICIENCY
!*******************************************************************
      DO 30 B=1,mNumBldg
        DO 30 D=1,mNumCR-2
          DO 30 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            IF (CurCalYr.EQ.RECSYear+1) THEN
              WTEQCEFFA(RECSYear+1,RECCL,B,D)=WTEQCEFFN(RECSYear+1,RECCL,B,D)
            ELSE
              TEMP=0.0
              DO Y=RECSYear,CurCalYr-1
                TEMP=TEMP+EQR90FUT(Y,CurCalYr,RECCL,B,D)+ &
                 EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
                 EQR90RPFUT(Y,CurCalYr,RECCL,B,D)
              ENDDO
              IF (TEMP .GT. 0.0) THEN
                WTEQCEFFA(CurCalYr,RECCL,B,D)=0.0
                DO Y=RECSYear,CurCalYr-1
                  WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFA(CurCalYr,RECCL,B,D)+ &
                   ((EQR90FUT(Y,CurCalYr,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))+ &
                   ((EQADDFUT(Y,CurCalYr,RECCL,B,D)+EQREPFUT(Y,CurCalYr,RECCL,B,D)+ &
                   EQR90RPFUT(Y,CurCalYr,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D)))/TEMP
                ENDDO
              ELSE
                WTEQCEFFA(CurCalYr,RECCL,B,D)=WTEQCEFFN(CurCalYr,RECCL,B,D)
              ENDIF
            ENDIF
      30  CONTINUE

!*******************************************************************
!  Calculate Standalone Freezer Consumption
!*******************************************************************
      DO 40 D=1,mNumCR-2
        FRZCON(CurIYr,D)=0.0
       DO 40 B=1,mNumBldg
         FRZCONIN(CurIYr,D,B)=0.
         Driver2(CurIYr,d,B)=0.
         FRZCONWT(CurIYr,D,B)=0.
 40   CONTINUE

      DO 50 D=1,mNumCR-2
        DO 50 B=1,mNumBldg
          DO 50 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            F  =RTFUEL(RECCL)
            IF (CurCalYr.EQ.RECSYear+1) THEN
              FRZCON(CurIYr,D)=FRZCON(CurIYr,D)+LEAPYR* &
               (((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))* &
               RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

              FRZCONWT(CurIYr,D,B)=FRZCONWT(CurIYr,D,B)+LEAPYR* &
               (((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
               (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))* &
               RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

              IF(EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
               EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D).GT.0.) THEN
                FRZCONIN(CurIYr,D,B)=FRZCONIN(CurIYr,D,B)+ &
                ((((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
                (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
                (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
                (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))))

                Driver2(CurIYr,d,B)=Driver2(CurIYr,d,B)+ &
                 (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
                 EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D))
              ENDIF

              EQCEQCN(CurIYr,RECCL,B,D)= LEAPYR* &
               (((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
               (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D)))* &
               RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

            ELSE
              FRZCON(CurIYr,D)=FRZCON(CurIYr,D)+ LEAPYR* &
               (((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
               (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
               (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)))* &
               RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

              FRZCONWT(CurIYr,D,B)=FRZCONWT(CurIYr,D,B)+LEAPYR* &
               (((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
               (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
               (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
               (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
               (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)))* &
               RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))

               IF(EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
                EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
                EQCSR90(CurCalYr,RECCL,B,D)+EQCREP(CurCalYr,RECCL,b,D)+ &
                EQCSUR(CurCalYr,RECCL,B,D).GT.0.) THEN
                 FRZCONIN(CurIYr,D,B)=FRZCONIN(CurIYr,D,B)+ &
                  ((((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
                  (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
                  (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
                  (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
                  (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
                  (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
                  (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)))))

                 Driver2(CurIYr,D,B)=Driver2(CurIYr,d,B)+ &
                  (EQCESE(CurCalYr,RECCL,B,D)+EQCADD(CurCalYr,RECCL,B,D)+ &
                  EQCRP90RP(CurCalYr,RECCL,B,D)+EQCRP90(CurCalYr,RECCL,B,D)+ &
                  EQCSR90(CurCalYr,RECCL,B,D)+EQCREP(CurCalYr,RECCL,b,D)+ &
                  EQCSUR(CurCalYr,RECCL,B,D))
               ENDIF

               EQCEQCN(CurIYr,RECCL,B,D)= LEAPYR* &
                (((EQCESE(CurCalYr,RECCL,B,D)*EQCSUEC(CurCalYr,RECCL,B,D))+ &
                (EQCADD(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
                (EQCRP90(CurCalYr,RECCL,B,D)*EQCRUEC(CurCalYr,RECCL,B,D))+ &
                (EQCRP90RP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
                (EQCSR90(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D))+ &
                (EQCREP(CurCalYr,RECCL,B,D)*EQCNUEC(CurCalYr,RECCL,B,D))+ &
                (EQCSUR(CurCalYr,RECCL,B,D)*EQCAUEC(CurCalYr,RECCL,B,D)))* &
                RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))
            ENDIF

      50  CONTINUE

      DO R=1,mNumCR-2
        DO B=1,mNumBldg
          IF (Driver2(CurIYr,R,B).GT.0) &
            FRZCONIN(CurIYr,R,B)= &
             FRZCONIN(CurIYr,R,B)/Driver2(CurIYr,R,B)
        ENDDO
      ENDDO

      END SUBROUTINE RFRZCON


!********************************************************************
!     LIGHTING CHOICE, STOCK AND CONSUMPTION
!********************************************************************
     SUBROUTINE LTCNS
     IMPLICIT NONE

     ! Set parameters for array dimensions - These are set near top of code
!     INTEGER MaxApps, MaxTypes, MaxBins, NLRec
!     PARAMETER (NLRec=100)  !Number of lighting records in the technology database
!     PARAMETER (MaxApps=4)  !Maximum number of applications
!     PARAMETER (MaxTypes=4) !Maximum number of bulb types within an application
!     PARAMETER (MaxBins=6)  !Maximum number of hours per day usage bins per applications

     REAL*4  LTMSHR(RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins),opcost(MaxTypes), &
             WTLEFF(MaxApps,RECSYear:EndYr,mNumBldg,mNumCR-2,MaxBins), &
             TOTEWTN(mNumBldg,mNumCR-2,MaxBins), &
             EQWTN(MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
             LTREPTOT(MaxApps,RECSYear:EndYr,mNumBldg,mNumCR-2,MaxBins), &
!             LTSTOCK(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &  ! MOVED TO COMMON BLOCK FOR DATABASE WRITING
             LTREP(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
             LTTOTSTOCK(MaxApps,RECSYear:EndYr,mNumBldg,mNumCR-2,MaxBins), &
             LTSTOCKEX(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
             LTNEEDED(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
             LTNEEDEDFUTly(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
             LTREPFUTly(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
             LTrepconsly(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
             LTrepstkly(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
             LTREPFUT(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
             LTNEEDEDFUT(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
             LTREPstk(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
             LTREPcons(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
             LTstockexcons(MaxApps,RECSYear:EndYr,MaxTypes,mNumBldg,mNumCR-2,MaxBins), &
             LightBeta1(MaxApps,RECSYear:EndYr,MaxTypes), &      !lgtbetas
             LightBeta2(MaxApps,RECSYear:EndYr,MaxTypes)         !lgtbetas

     REAL*4 temp, rep, cumrep, maxrep, annrep, &
              ef1,ef2,ef3,FACTOR,LTLBeta1DR,ALPHA,alpha1, &
            LHRATE,ELIGL,LHRDADJ,LGTBeta1,LGTBeta2,LGTBETA1DR

     INTEGER Y,B,D,EUPR,Y1,R,E,T,BIN,y2,diagnostics,LightDiag,i,indx,app,ilife,iLastYr

      ! reverse these statements to switch diagnostics on or off
     LightDiag=1 !print diagnostics
     LightDiag=0 !DOn't print diagnostics

     !-----------------------------------------------------
     !  Lighting Consumption Initialization, All Iterations
     !-----------------------------------------------------
         y=CurCalYr
       DO d=1,mNumCR-2
         LTCON(Y-(BaseYr-1),D)=0.0
!             LTCONly(Y,D)=0.0
         DO B=1,mNumBldg
          LTCONwt(Y-(BaseYr-1),D,B)=0.0
          LTCONin(Y-(BaseYr-1),D,B)=0.0
!          LTCONwtly(Y-(BaseYr-1),D,B)=0.0
!          LTCONinly(Y-(BaseYr-1),D,B)=0.0
         ENDDO
       ENDDO

     !-----------------------------------------------------
     !  Lighting Initializations & Consumption Calculation
     !  First year of call to this routine is RECSYear+1
     !-----------------------------------------------------
     IF (CurCalYr.EQ.RECSYear+1.AND.CURITR.EQ.1) THEN
       y=RECSYear
       DO d=1,mNumCR-2
         LTCON(Y-(BaseYr-1),D)=0.0
         DO B=1,mNumBldg
          LTCONwt(Y-(BaseYr-1),D,B)=0.0
          LTCONin(Y-(BaseYr-1),D,B)=0.0
         ENDDO
       ENDDO

     DO 999 app=1,NumApps

      DO y=RECSYear,LastYr+BaseYr-1
       DO d=1,mNumCR-2
         DO B=1,mNumBldg
          LTNUEC(app,y,d,b)=0.0
         ENDDO
       ENDDO
      ENDDO

      !--------------------------------------------------
      ! Filter Lighting Database for Current Year Values
      !--------------------------------------------------
      e=1 !initialize number of bulb types and add to it below
      DO i=1,NLRec
       IF(lightingapp(i).NE.AppID(app)) cycle
       ! Application found, check years
         IF(CurCalYr-1.GE.firstyear(i).AND.CurCalYr-1.LE.lastyear(i)) THEN
           watts(e)=bulbwatts(i)
           DO bin=1,NumAppBins(app)
            bulbbinlife(e,bin)=lifehours(i)/(365.*appbinhours(app,bin))
           ENDDO
           LightBeta1(app,CurCalYr,e)=LTLBeta1(i)    !lgtbetas
           LightBeta2(app,CurCalYr,e)=LTLBeta2(i)    !lgtbetas
           IF(LightDiag .NE. 0) WRITE(9,*) 'Betas (i,app,CurCalYr,e,beta1,beta2): ',i,app,CurCalYr,e,&
               LightBeta1(app,CurCalYr,e),LightBeta2(app,CurCalYr,e)    !lgtbetas
           e=e+1
         ENDIF
      ENDDO !i
      IF (e-1 .NE. numtypes(app)) &
         WRITE(9,*) 'RESDMSG SUB_LTCNS: Lamps not lining up for application ', app

      DO bin=1,NumAppBins(app)
       LTBinShare(app,bin)=0.
      ENDDO

      !Compute equipment stocks, consumption per HH by application (LTUEC), and bin shares of energy use (LTBinShare)
      DO d=1,mNumCR-2
       DO B=1,mNumBldg
        LTEQP(app,RECSYear,B,d)=bulbsperhh(app,b)*EH(RECSYear,B,d)
        LTuec(app,d,b)=0.
        DO bin=1,NumAppBins(app)
         DO e=1,numtypes(app)
          LTUEC(app,d,B)=ltuec(app,d,b)+appbinhours(app,bin)*365.*binshares(app,bin)*bulbbinshares(app,e,bin) &
                   *basewattsbulbs(app,e)*3.412/10**6
          LTBinShare(app,bin)=LTBinShare(app,bin)  +appbinhours(app,bin)*365.*binshares(app,bin)*bulbbinshares(app,e,bin) &
                   *basewattsbulbs(app,e)*3.412/10**6
         ENDDO !e
        ENDDO !bin
       ENDDO
      ENDDO

      !LTBinShare contains total energy, now convert LTBinShare into shares of energy use
      temp=0.
      DO bin=1,NumAppBins(app)
       temp=temp+LTBinShare(app,bin)
      ENDDO
      DO bin=1,NumAppBins(app)
       LTBinShare(app,bin)=LTBinShare(app,bin)/temp
      ENDDO

      !---------------------------------
      !  Calculate Lighting Consumption
      !---------------------------------
      DO d=1,mNumCR-2
       DO B=1,mNumBldg
         ! Added next line to feed RECS year consumption into output database file
         lteqcn(RECSYear-(BaseYr-1),app,b,d)=LTEQP(app,RECSYear,B,d)*LTUEC(app,d,B)
         LTCON(RECSYear-(BaseYr-1),d)=LTCON(RECSYear-(BaseYr-1),d)+(LTEQP(app,RECSYear,B,d)*LTUEC(app,d,B))
       ENDDO
      ENDDO

      DO d=1,mNumCR-2
       DO B=1,mNumBldg
        DO BIN=1,NumAppBins(APP)
          LTTOTSTOCK(app,RECSYear,B,d,BIN)=0.0
         DO E=1,numtypes(APP)
          LTSTOCK  (app,RECSYear,E,B,d,BIN)=(LTEQP(app,RECSYear,B,d)*binshares(app,bin)*bulbbinshares(app,e,BIN))
          LTSTOCKEX(app,RECSYear,E,B,d,BIN)=(LTEQP(app,RECSYear,B,d)*binshares(app,bin)*bulbbinshares(app,e,BIN))
          LTstockexcons(app,RECSYear,E,B,d,BIN)=LTstockex(app,RECSYear,E,B,d,BIN) &
            *appbinhours(app,bin)*365.*basewattsbulbs(app,e)*3.412/10**6
          LTNEEDED(app,RECSYear,E,B,d,BIN)=0.0
          LTrepfut(app,RECSYear,E,B,d,BIN)=0.0
          LTREP(app,RECSYear,E,B,d,BIN)=0.0
          LTTOTSTOCK(app,RECSYear,B,d,BIN)=LTTOTSTOCK(app,RECSYear,B,d,BIN)+LTSTOCK(app,RECSYear,E,B,d,BIN)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      !      This is the remaining RECSYear stock by bin projected into the future.
      !              Will become zero at some point for all bulb types depending on
      !              bin hours and bulb lives (see calculation of bulbbinlife(e, bin))
      DO d=1,mNumCR-2
       DO B=1,mNumBldg
        DO BIN=1,NumAppBins(app)
         DO Y1=RECSYear+1,LastYr+BaseYr-1
          DO E=1,numtypes(app)

           LTStockEx(app,Y1,E,B,d,BIN)= &
              max(0.,LTStockEx(app,Y1-1,E,B,d,BIN)-LTStockEx(app,RECSYear,E,B,d,BIN)*HDR(B)**(y1-RECSYear)/bulbbinlife(E,BIN))
           LTstockexcons(app,Y1,E,B,d,BIN)= LTStockEx(app,Y1,E,B,d,BIN) &
                                              *appbinhours(app,bin)*365.*basewattsbulbs(app,e)*3.412/10**6
          ENDDO !e
         ENDDO !y1
        ENDDO  !bin
       ENDDO   !b
      ENDDO    !r

999   CONTINUE  !DO for each lighting application
      ENDIF !RECSYear processing on first iteration

      !*****************************
      !   PROCESS FORECAST YEARS
      !*****************************

      !   Map Electricity Price into "Menu dollar Year"
      !   Prices in constant dollars, $/MMBtu
      EUPR=8   !End use lighting
      DO d=1,mNumCR-2
        PRICES(4,d,CurCalYr)=PELRSOUT(d,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
      ENDDO

     !Price elasticities and distribution and behavioral elasticity adjustment factor(alpah1)
     ALPHA=-.15;ef1=.50;ef2=.35;ef3=.15;ALPHA1=-0.01
     ! CONVERSION FACTOR FROM WattHours TO MMBTU
     FACTOR=3.412/10**6   !Btu/Watt to millions of Btu

     ! Loop through all applications
     DO 1000 app=1,NumApps
     !-----------------------------------------------------
     !     Filter Lighting Database for Current Year Values
     !-----------------------------------------------------
      e=1
      DO i=1,NLRec
       IF(lightingapp(i).NE.AppID(app)) cycle
       ! Application found in data, now check years
         IF(CurCalYr.GE.firstyear(i).AND.CurCalYr.LE.lastyear(i)) THEN
           watts(e)=bulbwatts(i)
           DO bin=1,NumAppBins(app)
            bulbbinlife(e,bin)=lifehours(i)/(365.*appbinhours(app,bin))
           ENDDO
           LightBeta1(app,CurCalYr,e)=LTLBeta1(i)    !lgtbetas
           LightBeta2(app,CurCalYr,e)=LTLBeta2(i)    !lgtbetas
         DO d =1,mNumCR-2
          LTLCap(e,d,1)=bulbcost(i)-bulbEEsub(i,d)-bulbsub(i,d)*FLOAT(EPA111D) !put bulb cost in bin 1 for now  !EElightsub
          ltlsub(e,d)=bulbEEsub(i,d)+bulbsub(i,d)*FLOAT(EPA111D)  !EElightsub
          ltlcapInvest(e)=bulbcost(i)
         ENDDO

          cribulb(e)=bulbcri(i)
          appbulbname(app,e)=bulbtype(i)
          e=e+1
         ENDIF
       ENDDO !i

      IF (e-1 .NE. numtypes(app)) &
         WRITE(6,*)  'RESDMSG SUB_LTCNS:', AppID(app),' lamps not lining up'
      IF (e-1 .NE. numtypes(app) .AND. LightDiag.EQ.1) &
         WRITE(9,*) 'RESDMSG SUB_LTCNS:', AppID(app),' lamps not lining up'

     !Further processing of LTLCAP - adjust for CRI and multiple replacements per year
       DO d=1,mNumCR-2
     DO e=1,NUMTYPES(APP)
      temp=LTLCap(e,d,1) !bulb cost DOesn't vary by bin until bin hours are accounted for below
      DO bin=1,NumAppBins(app)
       LTLCap(e,d,bin)= temp/(cribulb(e)/100.)**2
       ! if bulb lasts less than a year in this bin, THEN increase capital costs based on number of
       !   replacements per year
       IF(bulbbinlife(e,bin) .LT. 1.) LTLCap(e,d,bin)=(temp/bulbbinlife(e,bin))/(cribulb(e)/100.)**2
      ENDDO
     ENDDO !e
       ENDDO !division

     !  Operating Cost and Logit Shares
     DO d=1,mNumCR-2
       DO B=1,mNumBldg
             !  lighting diagnostics	!kj - check significance of years below; otherwise, comment out?
             diagnostics=0
               IF(d==1 .AND. b==1 .AND. CurCalYr==2006) diagnostics=1
               IF(d==1 .AND. b==1 .AND. CurCalYr==2010) diagnostics=1
               IF(d==1 .AND. b==1 .AND. CurCalYr==2011) diagnostics=1
               IF(d==1 .AND. b==1 .AND. CurCalYr==2012) diagnostics=1
               IF(d==1 .AND. b==1 .AND. CurCalYr==2013) diagnostics=1
               IF(d==1 .AND. b==1 .AND. CurCalYr==2019) diagnostics=1
               IF(d==1 .AND. b==1 .AND. CurCalYr==2020) diagnostics=1
               IF(d==1 .AND. b==1 .AND. CurCalYr==2021) diagnostics=1
               IF(diagnostics==1 .AND. LightDiag==1) &
               WRITE(9,*) 'year, app, bin, e, LTLBeta1dr, LTLCAP, bulbbinlife(e,bin), LightBeta2(app,CurCalYr,e), opcost(e), prices, watts, &
                 factor, LTHOURS(e), logit calc(e) '
             diagnostics=0

         DO BIN=1,NumAppBins(app)
            TOTEWTN(B,d,BIN)=0.0
           DO E=1,numtypes(app)
            !      CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOR CHANGES	!kj
            !         Note: to temporarily disable discount rate adjustment -- uncomment the following line and comment the one below
            !IF ((CurCalYr.GT.EndYr).AND. &  !yr
            IF ((CurCalYr.GT.2008).AND. &
                   (PRICES(4,d,CurCalYr).GT.PRICES(4,d,RECSYear))) THEN
             LHRATE=LightBeta1(app,CurCalYr,e)/LightBeta2(app,CurCalYr,e)    !lgtbetas (also note that this whole calculation is moved to within this DO loop).
             ELIGL=LHRATE - 0.07	!kj - 0.07 reference?
             IF (ELIGL.GT.0.0) THEN
                  LHRDADJ= ELIGL * &
                  ((PRICES(4,d,CurCalYr)/PRICES(4,d,RECSYear))**ALPHA1 )
                  LTLBeta1DR = (LHRDADJ+0.07) * LightBeta2(app,CurCalYr,e)    !lgtbetas	!kj - 0.07 reference?
             ELSE
                LTLBeta1DR=LightBeta1(app,CurCalYr,e)    !lgtbetas
             ENDIF
            ELSE
                LTLBeta1DR=LightBeta1(app,CurCalYr,e)    !lgtbetas
            ENDIF

             OPCOST(E)=PRICES(4,d,CurCalYr)*WATTS(E)*FACTOR*appbinHOURS(app,BIN)*365.
             EQWTN(E,B,d,BIN)=exp(LTLBeta1dr*LTLCap(e,d,bin)+LightBeta2(app,CurCalYr,e)*opcost(e))    !lgtbetas
             TOTEWTN(B,d,BIN)=TOTEWTN(B,d,BIN)+EQWTN(E,B,d,BIN)

             !  lighting diagnostics	!kj - check significance of years below; otherwise, comment out?
             diagnostics=0
               IF(d==1 .AND. b==1 .AND. CurCalYr==2006) diagnostics=1
               IF(d==1 .AND. b==1 .AND. CurCalYr==2010) diagnostics=1
               IF(d==1 .AND. b==1 .AND. CurCalYr==2011) diagnostics=1
               IF(d==1 .AND. b==1 .AND. CurCalYr==2012) diagnostics=1
               IF(d==1 .AND. b==1 .AND. CurCalYr==2013) diagnostics=1
               IF(d==1 .AND. b==1 .AND. CurCalYr==2019) diagnostics=1
               IF(d==1 .AND. b==1 .AND. CurCalYr==2020) diagnostics=1
               IF(d==1 .AND. b==1 .AND. CurCalYr==2021) diagnostics=1
               IF(diagnostics==1 .AND. LightDiag==1) &
                  WRITE(9,443) CurCalYr, app, bin, e, LTLBeta1dr, LTLCap(e,d,bin), bulbbinlife(e,bin),  &
                   LightBeta2(app,CurCalYr,e), opcost(e),PRICES(4,d,CurCalYr),watts(e),factor,appbinhours(app,bin),eqwtn(e,b,d,bin)    !lgtbetas
             diagnostics=0
             ENDDO  !e
           ENDDO    !bin
         ENDDO      !b
       ENDDO        !d

       DO d=1,mNumCR-2
         DO B=1,mNumBldg
           DO E=1,NumTypes(app)
             DO BIN=1,6  !6 usage bins for lighting technologies (based on hours of use)	!kj
               IF(TOTEWTN(B,d,BIN).NE.0.0) THEN
                  LTMSHR(CurCalYr,E,B,d,BIN)=(EQWTN(E,B,d,BIN)/ &
                    TOTEWTN(B,d,BIN))
               ELSE
                  LTMSHR(CurCalYr,E,B,d,BIN)=0.0
               ENDIF
               !LTMSHR(CurCalYr,E,B,d,BIN)= bulbbinshares(app,e,bin)
             ENDDO
           ENDDO
         ENDDO
       ENDDO

            !Diagnostics
            IF (CurCalYr.GT.RECSYear .OR. CurCalYr.LE.2025) THEN	!kj - why 2025? AEO2005 was the last one to project through 2025, though even AEO2006 resd.f didn't include this code
             DO BIN=1,NumAppBins(app)
              IF(LightDiag==1)WRITE (18,442) 'SHR',CurCalYr,app,bin,(LTMSHR(CurCalYr,E,1,1,BIN),E=1,numtypes(app))
             ENDDO
            ENDIF

       ! Initialize future replacements for iteration control
       DO BIN=1,NumAppBins(app)
        DO d=1,mNumCR-2
         DO B=1,mNumBldg
          DO E=1,NumTypes(app)
          IF(curitr.EQ.1) THEN
           DO Y1=CurCalYr,LastYr+BaseYr-1
            LTNEEDEDFUTly(app,y1,E,B,d,BIN)=LTNEEDEDFUT(app,y1,E,B,d,BIN)
            LTREPFUTly(app,y1,E,B,d,BIN)=LTREPFUT(app,y1,E,B,d,BIN)
            LTrepconsly(app,y1,e,B,d,BIN)=LTrepcons(app,y1,e,B,d,BIN)
            LTrepstkly(app,y1,e,B,d,BIN)=LTrepstk(app,y1,e,B,d,BIN)
!            LTCONly(Y1,d)=LTCON(Y1-(BaseYr-1),d)
!            LTCONINly(Y1-(BaseYr-1),d,b)=LTCONIN(Y1-(BaseYr-1),d,b)
!            LTCONWTly(Y1-(BaseYr-1),d,b)=LTCONWT(Y1-(BaseYr-1),d,b)
            LTNUECly(app,Y1,d,b)=LTNUEC(app,Y1,d,b)
           ENDDO
          ELSE
           DO Y1=CurCalYr,LastYr+BaseYr-1
            LTNEEDEDFUT(app,y1,E,B,d,BIN)=LTNEEDEDFUTly(app,y1,E,B,d,BIN)
            LTREPFUT(app,y1,E,B,d,BIN)=LTREPFUTly(app,y1,E,B,d,BIN)
            LTrepcons(app,y1,e,B,d,BIN)=LTrepconsly(app,y1,e,B,d,BIN)
            LTrepstk(app,y1,e,B,d,BIN)=LTrepstkly(app,y1,e,B,d,BIN)
!            LTCON(Y1-(BaseYr-1),d)=LTCONly(Y1,d)
!            LTCONIN(Y1-(BaseYr-1),d,b)=LTCONINly(Y1-(BaseYr-1),d,b)
!            LTCONWT(Y1-(BaseYr-1),d,b)=LTCONWTly(Y1-(BaseYr-1),d,b)
            LTNUEC(app,Y1,d,b)=LTNUECly(app,Y1,d,b)
           ENDDO
          ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       ! Additional stock bulbs needed for this year's new construction and for newly added floorspace this year
       !  in existing homes that remain from the original RECSYear stock of homes.
       !  This is ALLOCATED by purchases and represents current year requirements only.
       DO BIN=1,NumAppBins(app)
        DO d=1,mNumCR-2
         DO B=1,mNumBldg
          DO E=1,NumTypes(app)

            LTNEEDED(app,CurCalYr,E,B,d,BIN)=(HSEADD(CurCalYr,B,d)*(SQRFOOT(CurCalYr,B,d)/SQRFOOT(RECSYear,B,d)) &
                               *bulbsperhh(app,b)*LTMSHR(CurCalYr,E,B,d,BIN)*binshares(app,BIN) &
                               +EH(CurCalYr,B,d)*((EXSQRFOOT(CurCalYr,B,d)/EXSQRFOOT(CurCalYr-1,B,d))-1.0) &
                               *bulbsperhh(app,b)*LTMSHR(CurCalYr,E,B,d,BIN)*binshares(app,BIN))
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       ! Bulbs needed this year for all reasons
       DO d=1,mNumCR-2
        DO B=1,mNumBldg
         DO BIN=1,NumAppBins(app)
           LTREPTOT(app,CurCalYr,B,d,BIN)=0.
          DO e=1,NumTypes(app)
           ! This year's purchases =
           !   replacements for original RECS stock + replacements from past purchases needed this year
           !   + new bulbs added due to new construction & floorspace additions in existing homes
           ! NOTE that LTrepfut(app,CurCalYr) is finalized in CurCalYr-1, we update the future stream for this
           !   years purchases below

           LTREPTOT(app,CurCalYr,B,d,BIN)=LTREPTOT(app,CurCalYr,B,d,BIN) &
                                           +LTSTOCKEX(app,CurCalYr-1,E,B,d,BIN)*hdr(b)-LTSTOCKEX(app,CurCalYr,E,B,d,BIN) &
                                           +LTREPFUT(app,CurCalYr,E,B,d,BIN) &
                                           +LTneeded(app,CurCalYr,e,b,d,bin)

          ENDDO
         ENDDO
        ENDDO
       ENDDO


       ! Distribute purchases to bulb types based on purchase shares (LTMSHR), accumulate stocks and consumption
       DO d=1,mNumCR-2
        DO B=1,mNumBldg
         DO BIN=1,NumAppBins(app)
          DO e=1,NumTypes(app)
           LTREP(app,CurCalYr,e,B,d,BIN)=LTREPTOT(app,CurCalYr,B,d,BIN)*LTMSHR(CurCalYr,E,B,d,BIN)
           LTREPstk(app,CurCalYr,E,B,d,BIN)=LTREPstk(app,CurCalYr,E,B,d,BIN)+ LTREP(app,CurCalYr,e,B,d,BIN)
           LTrepcons(app,CurCalYr,e,B,d,BIN)=LTrepcons(app,CurCalYr,e,B,d,BIN) &
                       +LTrep(app,CurCalYr,e,B,d,BIN)*365.*appbinhours(app,bin)*watts(e)*3.412/10**6
          ENDDO
         ENDDO
        ENDDO
       ENDDO


       ! Extend this year's bulb purchases (LTREP) into the future replacement purchase requirements
       ! Also extend this years purchased bulbs into future purchased-bulb remaining stocks
       ! And compute the energy consumed for this years purchases and the energy requirements
       !  for future remaining stocks from this years purchases
       DO d=1,mNumCR-2
        DO B=1,mNumBldg
         DO BIN=1,NumAppBins(app)
          DO E=1,NumTypes(app)
           cumrep=0.  !cumulative replacements
           rep=LTREP(app,CurCalYr,E,B,d,BIN)/max(1.,bulbbinlife(E,BIN)) !bulbs per year decaying from LTREP

           ! restricting the looping will save some amount of execution time
           ilife=ifix(bulbbinlife(E,BIN)+.5)+1 !round up bulblife as an INTEGER and add a year for looping	!kj - what is .5?
           iLastYr=min(LastYr+BaseYr-1,CurCalYr+ilife)
           DO Y1=CurCalYr+1,iLastYr

            maxrep=LTREP(app,CurCalYr,E,B,d,BIN)
            maxrep=maxrep*hdr(b)
            annrep=max(0.,min(maxrep-cumrep,rep))

            IF (annrep .GT. 0.) THEN
              LTREPFUT(app,Y1,E,B,d,BIN)=LTREPFUT(app,Y1,E,B,d,BIN) + annrep*HDR(B)**(y1-CurCalYr)
              cumrep=cumrep+annrep
              LTREPstk(app,y1,E,B,d,BIN)=LTREPstk(app,y1,E,B,d,BIN)+ maxrep-cumrep
              LTrepcons(app,y1,E,B,d,BIN)=LTrepcons(app,y1,E,B,d,BIN)+(maxrep-cumrep)*365.*appbinhours(app,bin) &
                  *watts(e)*3.412/10**6
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       ! LTrepstk is prior replacements remaining stock, so total stock = RECS stock remaining
       !     + this year's purchases + prior year replacements still left in stock
       DO d=1,mNumCR-2
        DO B=1,mNumBldg
         DO BIN=1,NumAppBins(app)
          DO E=1,NumTypes(app)

           LTSTOCK(app,CurCalYr,E,B,d,BIN)=LTSTOCKEX(app,CurCalYr,E,B,d,BIN)+LTrepstk(app,CurCalYr,E,B,d,BIN)

          ENDDO  !e
         ENDDO   !bin
        ENDDO    !bldg
       ENDDO     !reg

       DO d=1,mNumCR-2
         DO B=1,mNumBldg
            LTEQP(app,CurCalYr,B,d)=0.0
          DO BIN =1,NumAppBins(app)
            LTTOTSTOCK(app,CurCalYr,B,d,BIN)=0.0
           DO E=1,NumTypes(app)
            LTTOTSTOCK(app,CurCalYr,B,d,BIN)=LTTOTSTOCK(app,CurCalYr,B,d,BIN)+LTSTOCK(app,CurCalYr,E,B,d,BIN)
            LTEQP(app,CurCalYr,B,d)=LTEQP(app,CurCalYr,B,d)+LTSTOCK(app,CurCalYr,E,B,d,BIN)
           ENDDO
         ENDDO
        ENDDO
       ENDDO

       DO d=1,mNumCR-2
         DO B=1,mNumBldg
          DO E=1,NumTypes(app)
            LTInvest(app,CurCalYr,E,B,d,1)=0.0
            LTInvest(app,CurCalYr,E,B,d,2)=0.0
            LTsubsidy(app,CurCalYr,E,B,d,1)=0.0
            LTsubsidy(app,CurCalYr,E,B,d,2)=0.0
            LTNEEDEDbyAPP(app,CurCalYr,E,B,d)=0.0
            LTREPbyAPP(app,CurCalYr,E,B,d)=0.0
           DO BIN=1,NumAppBins(app)
            LTNEEDEDbyAPP(app,CurCalYr,E,B,d)=LTNEEDEDbyAPP(app,CurCalYr,E,B,d)+LTNEEDED(app,CurCalYr,E,B,d,BIN)
            LTREPbyAPP(app,CurCalYr,E,B,d)=LTREPbyAPP(app,CurCalYr,E,B,d)+LTREP(app,CurCalYr,E,B,d,BIN)
            !adjust investment spending for bulbs lasting < 1 year by dividing by life in years
                  temp=LTLCAPInvest(E)-LTLSUB(e,d)
            IF(bulbbinlife(e,bin) .LT. 1.) temp=temp/bulbbinlife(e,bin)
            LTInvest(app,CurCalYr,E,B,d,1)=LTInvest(app,CurCalYr,E,B,d,1)+LTNEEDED(app,CurCalYr,E,B,d,BIN)*temp
            LTInvest(app,CurCalYr,E,B,d,2)=LTInvest(app,CurCalYr,E,B,d,2)+LTREP(app,CurCalYr,E,B,d,BIN)*temp
                  temp=LTLSUB(E,d)           !111(D) -- not likely to subsidize short lived bulbs, but just in case
                  IF(bulbbinlife(e,bin) .LT. 1.) temp=temp/bulbbinlife(e,bin)
            LTsubsidy(app,CurCalYr,E,B,d,1)=LTsubsidy(app,CurCalYr,E,B,d,1)+LTNEEDED(app,CurCalYr,E,B,d,BIN)*temp
            LTsubsidy(app,CurCalYr,E,B,d,2)=LTsubsidy(app,CurCalYr,E,B,d,2)+LTREP(app,CurCalYr,E,B,d,BIN)*temp
           ENDDO
              ENDDO
         ENDDO
       ENDDO

       !-----------------------------------
       !     CALCULATE WEIGHTED EFFICIENCY
       !-----------------------------------
       DO d=1,mNumCR-2
        DO B=1,mNumBldg
         DO BIN=1,NumAppBins(app)
            WTLEFF(app,CurCalYr,B,d,BIN)=0.0
            DO E=1,NumTypes(app)
              ! first calculate total watts for this years purchases and surviving purchases from past years
              !  THEN calculate watts per bulb in the next loop
               WTLEFF(app,CurCalYr,B,d,BIN)=WTLEFF(app,CurCalYr,B,d,BIN) &
                   +LTrepcons(app,CurCalYr,E,B,d,BIN)/(365.*appbinhours(app,bin)*3.412/10**6)&
                   +LTstockexcons(app,CurCalYr,e,B,d,BIN)/(365.*appbinhours(app,bin)*3.412/10**6)
            ENDDO
          ENDDO
         ENDDO
        ENDDO

       DO d=1,mNumCR-2
        DO B=1,mNumBldg
         DO BIN=1,NumAppBins(app)
          ! now divide by the total stock to get average watts per bulb by bin
          WTLEFF(app,CurCalYr,B,d,BIN)=WTLEFF(app,CurCalYr,B,d,BIN)/LTTOTSTOCK(app,CurCalYr,B,d,BIN)
         ENDDO
        ENDDO
       ENDDO

       DO d=1,mNumCR-2
        DO B=1,mNumBldg
          WTLEFFbyAPP(app,CurCalYr,B,d)=0.0
         DO BIN=1,NumAppBins(app)
          WTLEFFbyAPP(app,CurCalYr,B,d)=WTLEFFbyAPP(app,CurCalYr,B,d)+((WTLEFF(app,CurCalYr,B,d,BIN)*LTTOTSTOCK(app,CurCalYr,B,d,BIN)) &
                                                                   /LTEQP(app,CurCalYr,B,d))
         ENDDO
        ENDDO
       ENDDO
      !-----------------
      !   LIGHTING UEC
      !-----------------
       DO d=1,mNumCR-2
        DO B=1,mNumBldg
          DO BIN=1,NumAppBins(app)
           ! This calculation adjusts UECs for efficiency and rebound effect, price elasticity in next loop
           LTNUEC(app,CurCalYr,d,b)= LTNUEC(app,CurCalYr,d,b) &
              + LTBinShare(app,BIN)*LTUEC(app,d,b)*(WTLEFF(app,CurCalYr,b,d,BIN)/basewattbins(app,bin))**(1+alpha)

! this might be preferred        + LTBinShare(app,BIN)*LTUEC(app,d,b)*(WTLEFF(app,CurCalYr,b,d,BIN)/WTLEFF(app,RECSYear+1,b,d,BIN))**(1+alpha)
! 0613 switch from basewatts     + LTBinShare(app,BIN)*LTUEC(app,d,b)*(WTLEFF(app,CurCalYr,b,d,BIN)/basewattbins(app,bin))**(1+alpha)
!      this caused an unexpected jump in 2006 consumption -- NEED TO INVESTIGATE FURTHER	!kj

          ENDDO !Bins
        ENDDO !Building Types
       ENDDO !Regions

      !-------------------------------
      ! LIGHTING UEC AND CONSUMPTION
      !-------------------------------
      y=CurIYr
      ! Economic Stimulus Package Affects Price Elasticity (but not rebound)
      ! This is a permanent shift based on the smart grid concept
      DO d=1,mNumCR-2
       DO B=1,mNumBldg
       IF ((CurCalYr.GT.2010).AND.(STIMULUS.EQ.1)) THEN
         ALPHA=-0.30
        ELSE
         ALPHA=-0.15
       ENDIF

       lteqcn(y,app,b,d)=leapyr*lteqp(app,CurCalYr,b,d)*ltnuec(app,CurCalYr,d,b)*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

       ! OUTPUTS TO NEMS TABLE 4 (ltcon)
       ltcon(y,d)= ltcon(y,d) + lteqcn(y,app,b,d)
       ltconwt(y,d,b)= ltconwt(y,d,b)+lteqcn(y,app,b,d)

       ! OUTPUTS TO NEMS TABLE 31  (ltconwt, ltconin)
       ltconin(y,d,b)= ltconin(y,d,b)+(ltconwt(y,d,b)/RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR))/&
          (EH(CurCalYr,B,D)*EXSQRFOOT(CurCalYr,B,D)+HSEADD(CurCalYr,B,D)*SQRFOOT(CurCalYr,B,D)+ &
           SQNEW(CurCalYr-1,B,D)*NH(CurCalYr-1,B,D))

       ENDDO !Building Types
      ENDDO !Regions

             !  Lighting diagnostics
             IF(LightDiag==0) GOTO 446
             ! WRITE(9,*) 'year, app, bin, e, LTLBeta1dr, LTLCAP, bulbbinlife(e,bin), LightBeta2(app,CurCalYr,e), opcost(e) prices, watts, &
             !              factor, LTHOURS(e), logit calc(e) '   !lgtbetas

             IF (CurCalYr == EndYr) THEN  !yr
               b=1
               d=1

               WRITE(9,*) ' Printing Lighting Diagnostics for Division ',d,' Building Type ',b
               WRITE(9,*) 'EH EXSQRFOOT'
               WRITE(9,445)   bin, e,(EH(y1,1,1)/10.**6,Y1=RECSyear,EndYr)  !yr
               WRITE(9,445)   bin, e,(EXSQRFOOT(y1,1,1)/10.**3,Y1=RECSyear,EndYr)  !yr

               WRITE(9,*) 'HSEADD SQRFOOT'
               WRITE(9,445)   bin, e,(HSEADD(y1,1,1)/10.**6,Y1=RECSyear,EndYr)  !yr
               WRITE(9,445)   bin, e,(SQRFOOT(y1,1,1)/10.**3,Y1=RECSyear,EndYr)  !yr

                 WRITE(9,*) ' '
                 WRITE(9,*) 'LTrep Bins&Type '
               DO BIN=1,NumAppBins(app)
                DO E=1,NumTypes(app)
                   ! Divide by 10^6 for most detail
                   WRITE(9,445)   bin, e,(LTrep(app,y1,E,B,d,BIN)/10.**6,Y1=RECSyear,EndYr)  !yr
                ENDDO !e
               ENDDO !bin

                 WRITE(9,*) ' '
                 WRITE(9,*) 'LTRepTot Bins '
               DO BIN=1,NumAppBins(app)
                   ! Divide by 10^6 for most detail
                   WRITE(9,445) bin, e,(LTreptot(app,Y1,B,d,BIN)/10.**6,Y1=RECSyear,EndYr)  !yr
               ENDDO !bin

                 WRITE(9,*) ' '
                 WRITE(9,*) 'LTRepFut Bins&Type '
               DO BIN=1,NumAppBins(app)
                DO E=1,NumTypes(app)
                   ! Divide by 10^6 for most detail
                   WRITE(9,445) bin, e,(LTrepFUT(app,Y1,E,B,d,BIN)/10.**6,Y1=RECSyear,EndYr)  !yr
                ENDDO !e
               ENDDO !bin

                 WRITE(9,*) ' '
                 WRITE(9,*) 'LTStockEx Bins&Type '
               DO BIN=1,NumAppBins(app)
                DO E=1,NumTypes(app)
                   ! Divide by 10^6 for most detail
                   WRITE(9,445) bin, e,(LTSTOCKEX(app,Y1,E,B,d,BIN)/10.**6,Y1=RECSyear,EndYr)  !yr
                ENDDO !e
               ENDDO !bin

                 WRITE(9,*) ' '
                 WRITE(9,*) 'LTStockExCons Bins&Type '
               DO BIN=1,NumAppBins(app)
                DO E=1,NumTypes(app)
                   ! Divide by 10^6 for most detail
                   WRITE(9,445) bin, e,(LTstockexcons(app,Y1,E,B,d,BIN)/10.**6,Y1=RECSyear,EndYr)  !yr
                ENDDO !e
               ENDDO !bin

               WRITE(9,*) ' '
               WRITE(9,*) 'LTrepstk Bins&Type '
               DO BIN=1,NumAppBins(app)
                DO E=1,NumTypes(app)
                   ! Divide by 10^6 for most detail
                   WRITE(9,445) bin, e,(LTrepstk(app,Y1,E,B,d,BIN)/10.**6,Y1=RECSyear,EndYr)  !yr
                ENDDO !e
               ENDDO !bin

                 WRITE(9,*) ' '
                 WRITE(9,*) 'LTrepCons Bins&Type '
               DO BIN=1,NumAppBins(app)
                DO E=1,NumTypes(app)
                   ! Divide by 10^6 for most detail
                   WRITE(9,445)   bin, e,(LTrepCons(app,y1,E,B,d,BIN)/10.**6,Y1=RECSyear,EndYr)  !yr
                ENDDO !e
               ENDDO !bin

                 WRITE(9,*) ' '
                 WRITE(9,*) 'LTneeded Bins&Type '
               DO BIN=1,NumAppBins(app)
                DO E=1,NumTypes(app)
                   ! Divide by 10^6 for most detail
                   WRITE(9,445)  bin, e,(LTneeded(app,y1,E,B,d,BIN)/10.**6,Y1=RECSyear,EndYr)  !yr
                ENDDO !e
               ENDDO !bin

               WRITE(9,*) ' '
               WRITE(9,*) 'LTStock Bins&Type '
               DO BIN=1,NumAppBins(app)
                DO E=1,NumTypes(app)
                 WRITE(9,445) bin,e,(LTSTOCK(app,Y1,E,B,d,BIN)/10.**6,Y1=RECSyear,EndYr)  !yr
                ENDDO !e
               ENDDO !bin

               WRITE(9,*) ' '
               WRITE(9,*) 'WTLEFF Bins '
               e=0
               DO BIN=1,NumAppBins(app)
                 WRITE(9,445) bin,e,(WTLEFF(app,y1,B,d,BIN),Y1=RECSyear,EndYr)  !yr
               ENDDO !bin

               WRITE(9,*) ' '
               WRITE(9,*) 'ltcon/10**9 '
               WRITE(9,444) (ltcon(y1,1)/10**9,Y1=RECSYear-BaseYr+1,LastYr)  !yr

               WRITE(9,*) 'ltnuec '
               WRITE(9,444) (LTNUEC(app,y1,1,1),Y1=RECSyear,EndYr)  !yr

               WRITE(9,*) 'ltnuec*lteqp/10**9'
               WRITE(9,444) (LTNUEC(app,y1,1,1)*LTEQP(app,y1,1,1)/10**9,Y1=RECSyear,EndYr)  !yr
               !End Lighting Diagnostics

       ENDIF  !current year is EndYr
  446  CONTINUE  !jump here to skip diagnostic output

 1000  CONTINUE  !looping for lighting applications

442    FORMAT(a4,i5,2i2,4e11.3)
443    FORMAT(i5,3i2,10e11.3)
444    FORMAT(36(1x,f7.4))
445    FORMAT(2i3,36(1x,f7.3))

      END SUBROUTINE LTCNS


!********************************************************************
!     MISCELLANEOUS ELECTRIC LOADS (MELs) AND OTHER ELECTRIC APPLIANCE CONSUMPTION
!********************************************************************
      SUBROUTINE APCNS
      IMPLICIT NONE
      REAL*4 TVSNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),TVSNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
      REAL*4 STBNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),STBNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
      REAL*4 HTSNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),HTSNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
      REAL*4 OTTNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),OTTNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)  !MELs21
      REAL*4 VGCNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),VGCNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
      REAL*4 DPCNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),DPCNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
      REAL*4 LPCNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),LPCNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
      REAL*4 MONNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),MONNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
      REAL*4 NETNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),NETNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
      REAL*4 BATNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),BATNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
      REAL*4 CFNNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),CFNNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
      REAL*4 COFNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),COFNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
      REAL*4 DEHNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),DEHNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
      REAL*4 MCONIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),MCONUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
      REAL*4 PLPNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),PLPNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)  !MELs21
      REAL*4 PLHNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),PLHNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)  !MELs21
      REAL*4 SECNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),SECNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
      REAL*4 SPANIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),SPANUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
      REAL*4 WCLNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),WCLNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)    !winecool
      REAL*4 SPKNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),SPKNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)  !MELs21
      REAL*4 PHNNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),PHNNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)  !MELs21
      REAL*4 TABNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),TABNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)  !MELs21
      REAL*4 KITNIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),KITNUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)  !MELs21
      REAL*4 EANIUEC(RECSYear:EndYr,mNumCR-2,mNumBldg),EANUEC(RECSYear:EndYr,mNumCR-2,mNumBldg)
      REAL*4 TVSCONWT(mNumYr,mNumCR-2,mNumBldg),TVSCONIN(mNumYr,mNumCR-2,mNumBldg)
      REAL*4 STBCONWT(mNumYr,mNumCR-2,mNumBldg),STBCONIN(mNumYr,mNumCR-2,mNumBldg)
      REAL*4 HTSCONWT(mNumYr,mNumCR-2,mNumBldg),HTSCONIN(mNumYr,mNumCR-2,mNumBldg)
      REAL*4 OTTCONWT(mNumYr,mNumCR-2,mNumBldg),OTTCONIN(mNumYr,mNumCR-2,mNumBldg)  !MELs21
      REAL*4 VGCCONWT(mNumYr,mNumCR-2,mNumBldg),VGCCONIN(mNumYr,mNumCR-2,mNumBldg)
      REAL*4 DPCCONWT(mNumYr,mNumCR-2,mNumBldg),DPCCONIN(mNumYr,mNumCR-2,mNumBldg)
      REAL*4 LPCCONWT(mNumYr,mNumCR-2,mNumBldg),LPCCONIN(mNumYr,mNumCR-2,mNumBldg)
      REAL*4 MONCONWT(mNumYr,mNumCR-2,mNumBldg),MONCONIN(mNumYr,mNumCR-2,mNumBldg)
      REAL*4 NETCONWT(mNumYr,mNumCR-2,mNumBldg),NETCONIN(mNumYr,mNumCR-2,mNumBldg)
      REAL*4 BATCONWT(mNumYr,mNumCR-2,mNumBldg),BATCONIN(mNumYr,mNumCR-2,mNumBldg)
      REAL*4 CFNCONWT(mNumYr,mNumCR-2,mNumBldg),CFNCONIN(mNumYr,mNumCR-2,mNumBldg)
      REAL*4 COFCONWT(mNumYr,mNumCR-2,mNumBldg),COFCONIN(mNumYr,mNumCR-2,mNumBldg)
      REAL*4 DEHCONWT(mNumYr,mNumCR-2,mNumBldg),DEHCONIN(mNumYr,mNumCR-2,mNumBldg)
      REAL*4 MCOCONWT(mNumYr,mNumCR-2,mNumBldg),MCOCONIN(mNumYr,mNumCR-2,mNumBldg)
      REAL*4 PLPCONWT(mNumYr,mNumCR-2,mNumBldg),PLPCONIN(mNumYr,mNumCR-2,mNumBldg)  !MELs21
      REAL*4 PLHCONWT(mNumYr,mNumCR-2,mNumBldg),PLHCONIN(mNumYr,mNumCR-2,mNumBldg)  !MELs21
      REAL*4 SECCONWT(mNumYr,mNumCR-2,mNumBldg),SECCONIN(mNumYr,mNumCR-2,mNumBldg)
      REAL*4 SPACONWT(mNumYr,mNumCR-2,mNumBldg),SPACONIN(mNumYr,mNumCR-2,mNumBldg)
      REAL*4 WCLCONWT(mNumYr,mNumCR-2,mNumBldg),WCLCONIN(mNumYr,mNumCR-2,mNumBldg)    !winecool
      REAL*4 SPKCONWT(mNumYr,mNumCR-2,mNumBldg),SPKCONIN(mNumYr,mNumCR-2,mNumBldg)  !MELs21
      REAL*4 PHNCONWT(mNumYr,mNumCR-2,mNumBldg),PHNCONIN(mNumYr,mNumCR-2,mNumBldg)  !MELs21
      REAL*4 TABCONWT(mNumYr,mNumCR-2,mNumBldg),TABCONIN(mNumYr,mNumCR-2,mNumBldg)  !MELs21
      REAL*4 KITCONWT(mNumYr,mNumCR-2,mNumBldg),KITCONIN(mNumYr,mNumCR-2,mNumBldg)  !MELs21
      REAL*4 EACONWT(mNumYr,mNumCR-2,mNumBldg),EACONIN(mNumYr,mNumCR-2,mNumBldg)
      REAL*4 ALPHA,ef1,ef2,ef3,ELOTPEN(RECSYear:EndYr,mNumCR-2),sqftadj
      INTEGER Y,B,D,EUPR,F,Y1,Y3,I  !IncEff
      Y=CurCalYr-(BaseYr-1)
      ALPHA=-.15;ef1=.5;ef2=.35;ef3=.15
      EUPR=9

!********************************************************************
!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
!********************************************************************
      DO D=1,mNumCR-2
        PRICES(4,D,CurCalYr)=PELRSOUT(D,CurIYr,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BaseYr+1)/MC_JPGDP(-2))
      ENDDO

!********************************************************************
!   CALCULATE INCOME EFFECT FOR MISCELLANEOUS ELECTRIC LOADS (MELs); Assumes that higher disposable income means more annual usage of MELs
!********************************************************************
      DO I=1,NumMELs  !IncEff
        DO D=1,mNumCR-2
          IF (MELsIncomeEffect(I).LT.1) THEN  !IncEff
            INCOME(D,CurCalYr,I)=1.  !IncEff
		  ELSE  !IncEff
            INCOME(D,CurCalYr,I)=(MC_YPDR(D,CurIYr)/MC_YPDR(D,RECSYear-BaseYr+1))**.05  !IncEff
          ENDIF  !IncEff
		ENDDO
      ENDDO  !IncEff

!********************************************************************
! SET PENETRATION RATES FOR OTHER ELECTRIC/ UNSPECIFIED MISCELLANEOUS ELECTRIC LOADS (MELs)
!*******************************************************************
   !Penetration of electric other changes at the rate of disposable income per person over age 16
      DO D=1,mNumCR-2
        ELOTPEN(RECSYear,D)=1.
        ELOTPEN(CurCalYr,D)=ELOTPEN(RECSYear,D)*((MC_YPDR(D,Y)/MC_NP16A(D,Y))/(MC_YPDR(D,RECSYear-(BaseYr-1))/MC_NP16A(D,RECSYear-(BaseYr-1))))
      ENDDO

!********************************************************************
! SET UECs
!********************************************************************
      DO 10 D=1,mNumCR-2
        DO 10 B=1,mNumBldg
          !note added adjustment to electric other for average square footage
          ! sqftadj= ( EH(CurCalYr,B,D)*EXSQRFOOT(CurCalYr,B,d)+NH(CurCalYr,B,D)*SQRFOOT(CurCalYr,b,d) ) / &
          !    ( EH(RECSYear,B,D)*EXSQRFOOT(RECSYear,B,d)+NH(RECSYear,B,D)*SQRFOOT(RECSYear,b,d) )
          sqftadj=1.
          TVSNUEC(CurCalYr,D,B)=  TVSUEC(D,B)*TVSEFF(CurCalYr)*INCOME(D,CurCalYr,1)  !IncEff
          TVSNIUEC(CurCalYr,D,B)= TVSUEC(D,B)*TVSEFF(CurCalYr)
          STBNUEC(CurCalYr,D,B)=  STBUEC(D,B)*STBEFF(CurCalYr)*INCOME(D,CurCalYr,2)  !IncEff
          STBNIUEC(CurCalYr,D,B)= STBUEC(D,B)*STBEFF(CurCalYr)
          HTSNUEC(CurCalYr,D,B)=  HTSUEC(D,B)*HTSEFF(CurCalYr)*INCOME(D,CurCalYr,3)  !IncEff
          HTSNIUEC(CurCalYr,D,B)= HTSUEC(D,B)*HTSEFF(CurCalYr)
          OTTNUEC(CurCalYr,D,B)=  OTTUEC(D,B)*OTTEFF(CurCalYr)*INCOME(D,CurCalYr,4)  !IncEff  !MELs21
          OTTNIUEC(CurCalYr,D,B)= OTTUEC(D,B)*OTTEFF(CurCalYr)  !MELs21
          VGCNUEC(CurCalYr,D,B)=  VGCUEC(D,B)*VGCEFF(CurCalYr)*INCOME(D,CurCalYr,5)  !IncEff
          VGCNIUEC(CurCalYr,D,B)= VGCUEC(D,B)*VGCEFF(CurCalYr)
          DPCNUEC(CurCalYr,D,B)=  DPCUEC(D,B)*DPCEFF(CurCalYr)*INCOME(D,CurCalYr,6)  !IncEff
          DPCNIUEC(CurCalYr,D,B)= DPCUEC(D,B)*DPCEFF(CurCalYr)
          LPCNUEC(CurCalYr,D,B)=  LPCUEC(D,B)*LPCEFF(CurCalYr)*INCOME(D,CurCalYr,7)  !IncEff
          LPCNIUEC(CurCalYr,D,B)= LPCUEC(D,B)*LPCEFF(CurCalYr)
          MONNUEC(CurCalYr,D,B)=  MONUEC(D,B)*MONEFF(CurCalYr)*INCOME(D,CurCalYr,8)  !IncEff
          MONNIUEC(CurCalYr,D,B)= MONUEC(D,B)*MONEFF(CurCalYr)
          NETNUEC(CurCalYr,D,B)=  NETUEC(D,B)*NETEFF(CurCalYr)*INCOME(D,CurCalYr,9)  !IncEff
          NETNIUEC(CurCalYr,D,B)= NETUEC(D,B)*NETEFF(CurCalYr)
          BATNUEC(CurCalYr,D,B)=  BATUEC(D,B)*BATEFF(CurCalYr)*INCOME(D,CurCalYr,10)  !IncEff
          BATNIUEC(CurCalYr,D,B)= BATUEC(D,B)*BATEFF(CurCalYr)
          CFNNUEC(CurCalYr,D,B)=  CFNUEC(D,B)*CFNEFF(CurCalYr)*(CDDADJ(CurCalYr,D)/CDDADJ(RECSYear,D))**(2.0)*INCOME(D,CurCalYr,11)  !IncEff	!kj - only including for consistency; need to differentiate ceiling fans from other income-affected MELs
          CFNNIUEC(CurCalYr,D,B)= CFNUEC(D,B)*CFNEFF(CurCalYr)
          COFNUEC(CurCalYr,D,B)=  COFUEC(D,B)*COFEFF(CurCalYr)*INCOME(D,CurCalYr,12)  !IncEff
          COFNIUEC(CurCalYr,D,B)= COFUEC(D,B)*COFEFF(CurCalYr)
          DEHNUEC(CurCalYr,D,B)=  DEHUEC(D,B)*DEHEFF(CurCalYr)*INCOME(D,CurCalYr,13)  !IncEff
          DEHNIUEC(CurCalYr,D,B)= DEHUEC(D,B)*DEHEFF(CurCalYr)
          MCONUEC(CurCalYr,D,B)=  MCOUEC(D,B)*MCOEFF(CurCalYr)*INCOME(D,CurCalYr,14)  !IncEff
          MCONIUEC(CurCalYr,D,B)= MCOUEC(D,B)*MCOEFF(CurCalYr)
          PLPNUEC(CurCalYr,D,B)=  PLPUEC(D,B)*PLPEFF(CurCalYr)*INCOME(D,CurCalYr,15)  !MELs21  !IncEff
          PLPNIUEC(CurCalYr,D,B)= PLPUEC(D,B)*PLPEFF(CurCalYr)  !MELs21
          PLHNUEC(CurCalYr,D,B)=  PLHUEC(D,B)*PLHEFF(CurCalYr)*INCOME(D,CurCalYr,16)  !MELs21  !IncEff
          PLHNIUEC(CurCalYr,D,B)= PLHUEC(D,B)*PLHEFF(CurCalYr)  !MELs21
          SECNUEC(CurCalYr,D,B)=  SECUEC(D,B)*SECEFF(CurCalYr)*INCOME(D,CurCalYr,17)  !IncEff
          SECNIUEC(CurCalYr,D,B)= SECUEC(D,B)*SECEFF(CurCalYr)
          SPANUEC(CurCalYr,D,B)=  SPAUEC(D,B)*SPAEFF(CurCalYr)*INCOME(D,CurCalYr,18)  !IncEff
          SPANIUEC(CurCalYr,D,B)= SPAUEC(D,B)*SPAEFF(CurCalYr)
          WCLNUEC(CurCalYr,D,B)=  WCLUEC(D,B)*WCLEFF(CurCalYr)*INCOME(D,CurCalYr,19)  !winecool  !IncEff
          WCLNIUEC(CurCalYr,D,B)= WCLUEC(D,B)*WCLEFF(CurCalYr)                       !winecool
          SPKNUEC(CurCalYr,D,B)=  SPKUEC(D,B)*SPKEFF(CurCalYr)*INCOME(D,CurCalYr,20)  !MELs21  !IncEff
          SPKNIUEC(CurCalYr,D,B)= SPKUEC(D,B)*SPKEFF(CurCalYr)  !MELs21
          PHNNUEC(CurCalYr,D,B)=  PHNUEC(D,B)*PHNEFF(CurCalYr)*INCOME(D,CurCalYr,21)  !MELs21  !IncEff
          PHNNIUEC(CurCalYr,D,B)= PHNUEC(D,B)*PHNEFF(CurCalYr)  !MELs21
          TABNUEC(CurCalYr,D,B)=  TABUEC(D,B)*TABEFF(CurCalYr)*INCOME(D,CurCalYr,22)  !MELs21  !IncEff
          TABNIUEC(CurCalYr,D,B)= TABUEC(D,B)*TABEFF(CurCalYr)  !MELs21
          KITNUEC(CurCalYr,D,B)=  KITUEC(D,B)*KITEFF(CurCalYr)*INCOME(D,CurCalYr,23)  !MELs21  !IncEff
          KITNIUEC(CurCalYr,D,B)= KITUEC(D,B)*KITEFF(CurCalYr)  !MELs21

          EANUEC(CurCalYr,D,B)=  EAUEC(D,B)* ELOTPEN(CurCalYr,D)* sqftadj
          EANIUEC(CurCalYr,D,B)= EAUEC(D,B)* ELOTPEN(CurCalYr,D)* sqftadj
 10   CONTINUE

!********************************************************************
! Number of MELs
!********************************************************************
      DO 15 D=1,mNumCR-2
        DO 15 B=1,mNumBldg
          TVSEQP(CurCalYr,B,D)=((TVSEQP(RECSYear,B,D)/EH(RECSYear,B,D))*TVSPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
          STBEQP(CurCalYr,B,D)=((STBEQP(RECSYear,B,D)/EH(RECSYear,B,D))*STBPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
          HTSEQP(CurCalYr,B,D)=((HTSEQP(RECSYear,B,D)/EH(RECSYear,B,D))*HTSPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
          OTTEQP(CurCalYr,B,D)=((OTTEQP(RECSYear,B,D)/EH(RECSYear,B,D))*OTTPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))  !MELs21
          VGCEQP(CurCalYr,B,D)=((VGCEQP(RECSYear,B,D)/EH(RECSYear,B,D))*VGCPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
          DPCEQP(CurCalYr,B,D)=((DPCEQP(RECSYear,B,D)/EH(RECSYear,B,D))*DPCPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
          LPCEQP(CurCalYr,B,D)=((LPCEQP(RECSYear,B,D)/EH(RECSYear,B,D))*LPCPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
          MONEQP(CurCalYr,B,D)=((MONEQP(RECSYear,B,D)/EH(RECSYear,B,D))*MONPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
          NETEQP(CurCalYr,B,D)=((NETEQP(RECSYear,B,D)/EH(RECSYear,B,D))*NETPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
          BATEQP(CurCalYr,B,D)=((BATEQP(RECSYear,B,D)/EH(RECSYear,B,D))*BATPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
          CFNEQP(CurCalYr,B,D)=((CFNEQP(RECSYear,B,D)/EH(RECSYear,B,D))*CFNPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
          COFEQP(CurCalYr,B,D)=((COFEQP(RECSYear,B,D)/EH(RECSYear,B,D))*COFPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
          DEHEQP(CurCalYr,B,D)=((DEHEQP(RECSYear,B,D)/EH(RECSYear,B,D))*DEHPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
          MCOEQP(CurCalYr,B,D)=((MCOEQP(RECSYear,B,D)/EH(RECSYear,B,D))*MCOPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
          PLPEQP(CurCalYr,B,D)=((PLPEQP(RECSYear,B,D)/EH(RECSYear,B,D))*PLPPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))  !MELs21
          PLHEQP(CurCalYr,B,D)=((PLHEQP(RECSYear,B,D)/EH(RECSYear,B,D))*PLHPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))  !MELs21
          SECEQP(CurCalYr,B,D)=((SECEQP(RECSYear,B,D)/EH(RECSYear,B,D))*SECPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
          SPAEQP(CurCalYr,B,D)=((SPAEQP(RECSYear,B,D)/EH(RECSYear,B,D))*SPAPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))
          WCLEQP(CurCalYr,B,D)=((WCLEQP(RECSYear,B,D)/EH(RECSYear,B,D))*WCLPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))    !winecool
          SPKEQP(CurCalYr,B,D)=((SPKEQP(RECSYear,B,D)/EH(RECSYear,B,D))*SPKPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))  !MELs21
          PHNEQP(CurCalYr,B,D)=((PHNEQP(RECSYear,B,D)/EH(RECSYear,B,D))*PHNPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))  !MELs21
          TABEQP(CurCalYr,B,D)=((TABEQP(RECSYear,B,D)/EH(RECSYear,B,D))*TABPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))  !MELs21
          KITEQP(CurCalYr,B,D)=((KITEQP(RECSYear,B,D)/EH(RECSYear,B,D))*KITPEN(CurCalYr)*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D)))  !MELs21
          EAEQP(CurCalYr,B,D)= EAEQP(RECSYear,B,D)+NH(CurCalYr,B,D)
 15   CONTINUE

!********************************************************************
! CALCULATE CONSUMPTION FOR MISCELLANEOUS ELECTRIC LOADS (MELs) AND OTHER ELECTRIC APPLIANCES
!********************************************************************
      DO 20 D=1,mNumCR-2
       DO 20 B=1,mNumBldg
           IF ((CurCalYr.GT.2010).AND.(STIMULUS.EQ.1)) THEN
             ALPHA=-0.30
           ELSE
             ALPHA=-0.15
           ENDIF
         !TELEVISIONS (TVS)
         TVSCONWT(Y,D,B)=LEAPYR*(TVSEQP(CurCalYr,B,D)*TVSNUEC(CurCalYr,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
         TVSCONIN(Y,D,B)= 0.
         IF(TVSEQP(CurCalYr,B,D).GT.0.)TVSCONIN(Y,D,B)=(TVSEQP(CurCalYr,B,D)*TVSNIUEC(CurCalYr,D,B)) &
                                                        / TVSEQP(CurCalYr,B,D)
         TVSEQCN(Y,1,B,D)=LEAPYR*(TVSEQP(CurCalYr,B,D) &
                          *TVSNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

         !SET-TOP BOXES (STB)
         STBCONWT(Y,D,B)=LEAPYR*(STBEQP(CurCalYr,B,D)*STBNUEC(CurCalYr,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
         STBCONIN(Y,D,B)= 0.
         IF(STBEQP(CurCalYr,B,D).GT.0.)STBCONIN(Y,D,B)=(STBEQP(CurCalYr,B,D)*STBNIUEC(CurCalYr,D,B)) &
                                                        / STBEQP(CurCalYr,B,D)
         STBEQCN(Y,1,B,D)=LEAPYR*(STBEQP(CurCalYr,B,D) &
                          *STBNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

         !HOME THEATER SYSTEMS (HTS)
         HTSCONWT(Y,D,B)=LEAPYR*(HTSEQP(CurCalYr,B,D)*HTSNUEC(CurCalYr,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
         HTSCONIN(Y,D,B)= 0.
         IF(HTSEQP(CurCalYr,B,D).GT.0.)HTSCONIN(Y,D,B)=(HTSEQP(CurCalYr,B,D)*HTSNIUEC(CurCalYr,D,B)) &
                                                        / HTSEQP(CurCalYr,B,D)
         HTSEQCN(Y,1,B,D)=LEAPYR*(HTSEQP(CurCalYr,B,D) &
                          *HTSNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

         !OVER-THE-TOP STREAMING DEVICES (OTT)
         OTTCONWT(Y,D,B)=LEAPYR*(OTTEQP(CurCalYr,B,D)*OTTNUEC(CurCalYr,D,B)) &  !MELs21
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
         OTTCONIN(Y,D,B)= 0.  !MELs21
         IF(OTTEQP(CurCalYr,B,D).GT.0.)OTTCONIN(Y,D,B)=(OTTEQP(CurCalYr,B,D)*OTTNIUEC(CurCalYr,D,B)) &  !MELs21
                                                        / OTTEQP(CurCalYr,B,D)  !MELs21
         OTTEQCN(Y,1,B,D)=LEAPYR*(OTTEQP(CurCalYr,B,D) &  !MELs21
                          *OTTNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)  !MELs21

         !VIDEO GAME CONSOLES (VGC)
         VGCCONWT(Y,D,B)=LEAPYR*(VGCEQP(CurCalYr,B,D)*VGCNUEC(CurCalYr,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
         VGCCONIN(Y,D,B)= 0.
         IF(VGCEQP(CurCalYr,B,D).GT.0.)VGCCONIN(Y,D,B)=(VGCEQP(CurCalYr,B,D)*VGCNIUEC(CurCalYr,D,B)) &
                                                        / VGCEQP(CurCalYr,B,D)
         VGCEQCN(Y,1,B,D)=LEAPYR*(VGCEQP(CurCalYr,B,D) &
                          *VGCNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

         !DESKTOP PCS (DPC)
         DPCCONWT(Y,D,B)=LEAPYR*(DPCEQP(CurCalYr,B,D)*DPCNUEC(CurCalYr,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
         DPCCONIN(Y,D,B)= 0.
         IF(DPCEQP(CurCalYr,B,D).GT.0.)DPCCONIN(Y,D,B)=(DPCEQP(CurCalYr,B,D)*DPCNIUEC(CurCalYr,D,B)) &
                                                        / DPCEQP(CurCalYr,B,D)
         DPCEQCN(Y,1,B,D)=LEAPYR*(DPCEQP(CurCalYr,B,D) &
                          *DPCNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

         !LAPTOP PCS (LPC)
         LPCCONWT(Y,D,B)=LEAPYR*(LPCEQP(CurCalYr,B,D)*LPCNUEC(CurCalYr,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
         LPCCONIN(Y,D,B)= 0.
         IF(LPCEQP(CurCalYr,B,D).GT.0.)LPCCONIN(Y,D,B)=(LPCEQP(CurCalYr,B,D)*LPCNIUEC(CurCalYr,D,B)) &
                                                        / LPCEQP(CurCalYr,B,D)
         LPCEQCN(Y,1,B,D)=LEAPYR*(LPCEQP(CurCalYr,B,D) &
                          *LPCNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

         !MONITORS (MON)
         MONCONWT(Y,D,B)=LEAPYR*(MONEQP(CurCalYr,B,D)*MONNUEC(CurCalYr,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
         MONCONIN(Y,D,B)= 0.
         IF(MONEQP(CurCalYr,B,D).GT.0.)MONCONIN(Y,D,B)=(MONEQP(CurCalYr,B,D)*MONNIUEC(CurCalYr,D,B)) &
                                                        / MONEQP(CurCalYr,B,D)
         MONEQCN(Y,1,B,D)=LEAPYR*(MONEQP(CurCalYr,B,D) &
                          *MONNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

         !NETWORKING EQUIPMENT (NET)
         NETCONWT(Y,D,B)=LEAPYR*(NETEQP(CurCalYr,B,D)*NETNUEC(CurCalYr,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
         NETCONIN(Y,D,B)= 0.
         IF(NETEQP(CurCalYr,B,D).GT.0.)NETCONIN(Y,D,B)=(NETEQP(CurCalYr,B,D)*NETNIUEC(CurCalYr,D,B)) &
                                                        / NETEQP(CurCalYr,B,D)
         NETEQCN(Y,1,B,D)=LEAPYR*(NETEQP(CurCalYr,B,D) &
                          *NETNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

         !NON-PC RECHARGEABLES (BAT)
         BATCONWT(Y,D,B)=LEAPYR*(BATEQP(CurCalYr,B,D)*BATNUEC(CurCalYr,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
         BATCONIN(Y,D,B)= 0.
         IF(BATEQP(CurCalYr,B,D).GT.0.)BATCONIN(Y,D,B)=(BATEQP(CurCalYr,B,D)*BATNIUEC(CurCalYr,D,B)) &
                                                        / BATEQP(CurCalYr,B,D)
         BATEQCN(Y,1,B,D)=LEAPYR*(BATEQP(CurCalYr,B,D) &
                          *BATNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

         !CEILING FANS (CFN)
         CFNCONWT(Y,D,B)=LEAPYR*(CFNEQP(CurCalYr,B,D)*CFNNUEC(CurCalYr,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
         CFNCONIN(Y,D,B)= 0.
         IF(CFNEQP(CurCalYr,B,D).GT.0.)CFNCONIN(Y,D,B)=(CFNEQP(CurCalYr,B,D)*CFNNIUEC(CurCalYr,D,B)) &
                                                        / CFNEQP(CurCalYr,B,D)
         CFNEQCN(Y,1,B,D)=LEAPYR*(CFNEQP(CurCalYr,B,D) &
                          *CFNNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

         !COFFEE MACHINES (COF)
         COFCONWT(Y,D,B)=LEAPYR*(COFEQP(CurCalYr,B,D)*COFNUEC(CurCalYr,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
         COFCONIN(Y,D,B)= 0.
         IF(COFEQP(CurCalYr,B,D).GT.0.)COFCONIN(Y,D,B)=(COFEQP(CurCalYr,B,D)*COFNIUEC(CurCalYr,D,B)) &
                                                        / COFEQP(CurCalYr,B,D)
         COFEQCN(Y,1,B,D)=LEAPYR*(COFEQP(CurCalYr,B,D) &
                          *COFNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

         !DEHUMIDIFIERS (DEH)
         DEHCONWT(Y,D,B)=LEAPYR*(DEHEQP(CurCalYr,B,D)*DEHNUEC(CurCalYr,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
         DEHCONIN(Y,D,B)= 0.
         IF(DEHEQP(CurCalYr,B,D).GT.0.)DEHCONIN(Y,D,B)=(DEHEQP(CurCalYr,B,D)*DEHNIUEC(CurCalYr,D,B)) &
                                                        / DEHEQP(CurCalYr,B,D)
         DEHEQCN(Y,1,B,D)=LEAPYR*(DEHEQP(CurCalYr,B,D) &
                          *DEHNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

         !MICROWAVE OVENS (MCO)
         MCOCONWT(Y,D,B)=LEAPYR*(MCOEQP(CurCalYr,B,D)*MCONUEC(CurCalYr,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
         MCOCONIN(Y,D,B)= 0.
         IF(MCOEQP(CurCalYr,B,D).GT.0.)MCOCONIN(Y,D,B)=(MCOEQP(CurCalYr,B,D)*MCONIUEC(CurCalYr,D,B)) &
                                                        / MCOEQP(CurCalYr,B,D)
         MCOEQCN(Y,1,B,D)=LEAPYR*(MCOEQP(CurCalYr,B,D) &
                          *MCONUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

         !POOL PUMPS (PLP)  !MELs21
         PLPCONWT(Y,D,B)=LEAPYR*(PLPEQP(CurCalYr,B,D)*PLPNUEC(CurCalYr,D,B)) &  !MELs21
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)  !MELs21
         PLPCONIN(Y,D,B)= 0.  !MELs21
         IF(PLPEQP(CurCalYr,B,D).GT.0.)PLPCONIN(Y,D,B)=(PLPEQP(CurCalYr,B,D)*PLPNIUEC(CurCalYr,D,B)) &  !MELs21
                                                        / PLPEQP(CurCalYr,B,D)  !MELs21
         PLPEQCN(Y,1,B,D)=LEAPYR*(PLPEQP(CurCalYr,B,D) &  !MELs21
                          *PLPNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)  !MELs21

         !POOL HEATERS (PLH)  !MELs21
         PLHCONWT(Y,D,B)=LEAPYR*(PLHEQP(CurCalYr,B,D)*PLHNUEC(CurCalYr,D,B)) &  !MELs21
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)  !MELs21
         PLHCONIN(Y,D,B)= 0.  !MELs21
         IF(PLHEQP(CurCalYr,B,D).GT.0.)PLHCONIN(Y,D,B)=(PLHEQP(CurCalYr,B,D)*PLHNIUEC(CurCalYr,D,B)) &  !MELs21
                                                        / PLHEQP(CurCalYr,B,D)  !MELs21
         PLHEQCN(Y,1,B,D)=LEAPYR*(PLHEQP(CurCalYr,B,D) &  !MELs21
                          *PLHNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)  !MELs21

         !SECURITY SYSTEMS (SEC)
         SECCONWT(Y,D,B)=LEAPYR*(SECEQP(CurCalYr,B,D)*SECNUEC(CurCalYr,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
         SECCONIN(Y,D,B)= 0.
         IF(SECEQP(CurCalYr,B,D).GT.0.)SECCONIN(Y,D,B)=(SECEQP(CurCalYr,B,D)*SECNIUEC(CurCalYr,D,B)) &
                                                        / SECEQP(CurCalYr,B,D)
         SECEQCN(Y,1,B,D)=LEAPYR*(SECEQP(CurCalYr,B,D) &
                          *SECNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

         !SPAS (SPA)
         SPACONWT(Y,D,B)=LEAPYR*(SPAEQP(CurCalYr,B,D)*SPANUEC(CurCalYr,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
         SPACONIN(Y,D,B)= 0.
         IF(SPAEQP(CurCalYr,B,D).GT.0.)SPACONIN(Y,D,B)=(SPAEQP(CurCalYr,B,D)*SPANIUEC(CurCalYr,D,B)) &
                                                        / SPAEQP(CurCalYr,B,D)
         SPAEQCN(Y,1,B,D)=LEAPYR*(SPAEQP(CurCalYr,B,D) &
                          *SPANUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

         !WINE COOLERS (WCL)                                                                              !winecool
         WCLCONWT(Y,D,B)=LEAPYR*(WCLEQP(CurCalYr,B,D)*WCLNUEC(CurCalYr,D,B)) &                            !winecool
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)                                    !winecool
         WCLCONIN(Y,D,B)= 0.                                                                              !winecool
         IF(WCLEQP(CurCalYr,B,D).GT.0.)WCLCONIN(Y,D,B)=(WCLEQP(CurCalYr,B,D)*WCLNIUEC(CurCalYr,D,B)) &    !winecool
                                                        / WCLEQP(CurCalYr,B,D)                            !winecool
         WCLEQCN(Y,1,B,D)=LEAPYR*(WCLEQP(CurCalYr,B,D) &                                                  !winecool
                          *WCLNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)            !winecool

         !SMART SPEAKERS (SPK)  !MELs21
         SPKCONWT(Y,D,B)=LEAPYR*(SPKEQP(CurCalYr,B,D)*SPKNUEC(CurCalYr,D,B)) &  !MELs21
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)  !MELs21
         SPKCONIN(Y,D,B)= 0.  !MELs21
         IF(SPKEQP(CurCalYr,B,D).GT.0.)SPKCONIN(Y,D,B)=(SPKEQP(CurCalYr,B,D)*SPKNIUEC(CurCalYr,D,B)) &  !MELs21
                                                        / SPKEQP(CurCalYr,B,D)  !MELs21
         SPKEQCN(Y,1,B,D)=LEAPYR*(SPKEQP(CurCalYr,B,D) &  !MELs21
                          *SPKNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)  !MELs21

         !SMARTPHONES (PHN)  !MELs21
         PHNCONWT(Y,D,B)=LEAPYR*(PHNEQP(CurCalYr,B,D)*PHNNUEC(CurCalYr,D,B)) &  !MELs21
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)  !MELs21
         PHNCONIN(Y,D,B)= 0.  !MELs21
         IF(PHNEQP(CurCalYr,B,D).GT.0.)PHNCONIN(Y,D,B)=(PHNEQP(CurCalYr,B,D)*PHNNIUEC(CurCalYr,D,B)) &  !MELs21
                                                        / PHNEQP(CurCalYr,B,D)  !MELs21
         PHNEQCN(Y,1,B,D)=LEAPYR*(PHNEQP(CurCalYr,B,D) &  !MELs21
                          *PHNNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)  !MELs21

         !TABLETS (TAB)  !MELs21
         TABCONWT(Y,D,B)=LEAPYR*(TABEQP(CurCalYr,B,D)*TABNUEC(CurCalYr,D,B)) &  !MELs21
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)  !MELs21
         TABCONIN(Y,D,B)= 0.  !MELs21
         IF(TABEQP(CurCalYr,B,D).GT.0.)TABCONIN(Y,D,B)=(TABEQP(CurCalYr,B,D)*TABNIUEC(CurCalYr,D,B)) &  !MELs21
                                                        / TABEQP(CurCalYr,B,D)  !MELs21
         TABEQCN(Y,1,B,D)=LEAPYR*(TABEQP(CurCalYr,B,D) &  !MELs21
                          *TABNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)  !MELs21

         !SMALL KITCHEN APPLIANCES (KIT)  !MELs21
         KITCONWT(Y,D,B)=LEAPYR*(KITEQP(CurCalYr,B,D)*KITNUEC(CurCalYr,D,B)) &  !MELs21
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)  !MELs21
         KITCONIN(Y,D,B)= 0.  !MELs21
         IF(KITEQP(CurCalYr,B,D).GT.0.)KITCONIN(Y,D,B)=(KITEQP(CurCalYr,B,D)*KITNIUEC(CurCalYr,D,B)) &  !MELs21
                                                        / KITEQP(CurCalYr,B,D)  !MELs21
         KITEQCN(Y,1,B,D)=LEAPYR*(KITEQP(CurCalYr,B,D) &  !MELs21
                          *KITNUEC(CurCalYr,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)  !MELs21

         !ELECTRIC OTHER APPLIANCES (EOA)
         EACONWT(y,d,b)=LEAPYR*(EAEQP(CurCalYr,B,D)*EANUEC(CurCalYr,d,b)) &
             *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
         EACONIN(y,d,b)=0.
         IF(eaeqp(CurCalYr,B,D).GT.0.)EACONIN(y,d,b)=(EAEQP(CurCalYr,B,D)*EANIUEC(CurCalYr,d,b)) &
                  / EAEQP(CurCalYr,B,D)
         EAEQCN(y,1,b,d)=LEAPYR*(EAEQP(CurCalYr,b,d)*EAnuec(CurCalYr,d,b)) &
             *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

   20   CONTINUE

!********************************************************************
!  Calculate NEMS consumption for TV-related, PC-related, and all other appliances/MELs
!********************************************************************
      DO 50 D=1,mNumCR-2
          TVRCON(Y,D)=0.0
          PCRCON(Y,D)=0.0
          APCON(Y,D)=0.0
      DO 50 B=1,mNumBldg
        TVRCON(Y,D)=TVRCON(Y,D)+TVSEQCN(Y,1,B,D)+STBEQCN(Y,1,B,D)+HTSEQCN(Y,1,B,D)+OTTEQCN(Y,1,B,D)+VGCEQCN(Y,1,B,D)  !MELs21
        TVRCONWT(Y,D,b)=TVSCONWT(Y,D,B)+STBCONWT(Y,D,B)+HTSCONWT(Y,D,B)+OTTCONWT(Y,D,B)+VGCCONWT(Y,D,B)  !MELs21
        PCRCON(Y,D)=PCRCON(Y,D)+ DPCEQCN(Y,1,B,D)+LPCEQCN(Y,1,B,D)+MONEQCN(Y,1,B,D)+NETEQCN(Y,1,B,D)
        PCRCONWT(Y,D,b)=DPCCONWT(Y,D,B)+LPCCONWT(Y,D,B)+MONCONWT(Y,D,B)+NETCONWT(Y,D,B)
        APCON(Y,D)=APCON(Y,D)+ BATEQCN(Y,1,B,D)+CFNEQCN(Y,1,B,D)+COFEQCN(Y,1,B,D)+DEHEQCN(Y,1,B,D)+&
                   MCOEQCN(Y,1,B,D)+PLPEQCN(Y,1,B,D)+SECEQCN(Y,1,B,D)+SPAEQCN(Y,1,B,D)+&  !MELs21
                   WCLEQCN(Y,1,B,D)+PLHEQCN(Y,1,B,D)+EAEQCN(Y,1,B,D)+&  !winecool  !MELs21
                   SPKEQCN(Y,1,B,D)+PHNEQCN(Y,1,B,D)+TABEQCN(Y,1,B,D)+KITEQCN(Y,1,B,D)  !MELs21
        APCONWT(Y,D,b)=BATCONWT(Y,D,B)+CFNCONWT(Y,D,B)+COFCONWT(Y,D,B)+DEHCONWT(Y,D,B)+&
                       MCOCONWT(Y,D,B)+PLPCONWT(Y,D,B)+SECCONWT(Y,D,B)+SPACONWT(Y,D,B)+&  !MELs21
                       WCLCONWT(Y,D,B)+PLHCONWT(Y,D,B)+EACONWT(Y,D,B)+&  !winecool  !MELs21
                       SPKCONWT(Y,D,B)+PHNCONWT(Y,D,B)+TABCONWT(Y,D,B)+KITCONWT(Y,D,B)  !MELs21

  50   CONTINUE

!     Compute other electric appliance efficiency based on weighted average equipment intensities
      DO 51 D=1,mNumCR-2
      DO 51 B=1,mNumBldg
        TVRCONIN(Y,D,b)=(TVSCONIN(Y,D,B)*tvsconwt(y,d,b)+STBCONIN(Y,D,B)*stbconwt(y,d,b)+HTSCONIN(Y,D,B)*htsconwt(y,d,b) +&
                         OTTCONIN(Y,D,B)*OTTconwt(y,d,b)+VGCCONIN(Y,D,B)*vgcconwt(y,d,b)) / tvrconwt(y,d,b)  !MELs21
        PCRCONIN(Y,D,b)=(DPCCONIN(Y,D,B)*dpcconwt(y,d,b)+LPCCONIN(Y,D,B)*lpcconwt(y,d,b)+MONCONIN(Y,D,B)*monconwt(y,d,b) +&
                         NETCONIN(Y,D,B)*netconwt(y,d,b)) / pcrconwt(y,d,b)
        APCONIN(Y,D,b)= (BATCONIN(Y,D,B)*batconwt(y,d,b)+CFNCONIN(Y,D,B)*cfnconwt(y,d,b)+COFCONIN(Y,D,B)*cofconwt(y,d,b) +&
                         DEHCONIN(Y,D,B)*dehconwt(y,d,b)+MCOCONIN(Y,D,B)*mcoconwt(y,d,b)+PLPCONIN(Y,D,B)*PLPconwt(y,d,b) +&  !MELs21
                         SECCONIN(Y,D,B)*secconwt(y,d,b)+SPACONIN(Y,D,B)*spaconwt(y,d,b)+WCLCONIN(Y,D,B)*wclconwt(y,d,b) +&    !winecool
                         SPKCONIN(Y,D,B)*SPKconwt(y,d,b)+PHNCONIN(Y,D,B)*PHNconwt(y,d,b)+TABCONIN(Y,D,B)*TABconwt(y,d,b)+KITCONIN(Y,D,B)*KITconwt(y,d,b) +&  !MELs21
                         EACONIN(Y,D,B)*eaconwt(y,d,b)+PLHCONIN(Y,D,B)*PLHconwt(y,d,b)) / apconwt(y,d,b)  !MELs21
 51   CONTINUE

      END SUBROUTINE APCNS


!********************************************************************
!     SECONDARY HEATING CONSUMPTION
!********************************************************************
! SEC HEAT= G,E,D,L,K,C,W APPL=G,L

      SUBROUTINE SHTCNS
      IMPLICIT NONE
      REAL*4 ALPHA,ef1,ef2,ef3
      REAL*4 HDDFACT(mNumCR), SHTEFF, NHtoUse
      INTEGER Y, B, D, F, F2, FShell, EUPR
      Y=CurCalYr-(BaseYr-1)
      ALPHA=-.15;ef1=.5;ef2=.35;ef3=.15
      EUPR=10

!     INITIALIZE
      DO D=1,mNumCR-2
       DO F=1,7
        SHTCON(Y,F,D)=0.0
        DO B=1,mNumBldg
          SHEQCN(Y,F,B,D)=0.0
          SHTCONIN(Y,F,D,B)=0.0
          SHTCONWT(Y,F,D,B)=0.0
        ENDDO
       ENDDO
      ENDDO

!   COMPUTE HDDFACT
      DO D=1,mNumCR-2
          HDDFACT(D)=(HDDADJ(CurCalYr,D)/HDDADJ(RECSYear,D))
      ENDDO

! Begin Looping for Secondary Heating Calculations
      DO F=1,7
  !   Fuels are matched with prices below for the RSELAST function calls
  !   F is the secondary heating numbering scheme
  !     "F" 1=Natural Gas, 2=Electricity, 3=Distillate Fuel Oil, 4=Propane, 5=Kerosene, 6=Wood, 7=Coal
  !   F2 is the fuel for the elasticity calculation:
  !     Prices "F2" 1=Distillate Fuel Oil 2=Propane 3=Natural Gas 4=Electricity 5=Kerosene 6=Wood
  !     Since no price for coal, turn off elasticity below by setting F2=0

            ! Add ARRA effect for enhanced price sensitivity for stimulus runs
            IF ((CurCalYr.GT.2010).AND.(STIMULUS.EQ.1).AND.(F.EQ.4)) THEN
               ALPHA=-0.30
             ELSE
               ALPHA=-0.15
            ENDIF

            FShell=F  !FShell is the heating shell to use, none for wood, so set wood below
            IF (F.EQ.1) F2=3 !natural gas
            IF (F.EQ.2) F2=4 !electricity
            IF (F.EQ.3) F2=1 !distillate fuel oil
            IF (F.EQ.4) F2=2 !propane
            IF (F.EQ.5) F2=0 !kerosene combined with distillate fuel oil, so setting F2 to zero to turn off elasticity	!kj - was 5
            IF (F.EQ.6) F2=0 !coal !setting F2 to zero turns off price elasticity	!kj - no longer reporting coal
            IF (F.EQ.7) THEN !wood; priced to distillate fuel oil
             F2=1
             FShell=3 !There is no shell for wood, so use distillate
             ALPHA=0.50 !No ARRA effect for wood, has positive elasticity with respect to distillate fuel oil
            ENDIF

         DO D=1,mNumCR-2
            DO B=1,mNumBldg

                SHTEQP(CurCalYr,B,D,F)=(SHTSHR(B,D,F)*EH(CurCalYr,B,D)+NSHTSHR(B,D,F)*NH(CurCalYr,B,D))

                ! Special Treatment for Natural Gas and propane due to secondary heating standard after 2012
                ! In October 2016, DOE determined that updated standards for direct heating equipment were not currently economically justified (Federal Register # 2016-24866)
                NHtoUse=NH(CurCalYr,B,D) !replace NH(...) with "discounted" value for standard
!                IF (F.EQ.1 .OR. F.EQ.4) THEN	!kj - commented out because 2012 is prior to 2015 RECS base year
!                  IF(CurCalYr .GT. 2012) THEN	!kj - commented out because 2012 is prior to 2015 RECS base year
                    ! Houses added to the stock after 2012 have increased efficiency
!                    SHTEFF=.5003 !calculated effect of efficiency on UEC for houses post-2012	!kj - commented out because 2012 is prior to 2015 RECS base year
                    ! Do weighted calculation for NH, "discounting" post-2012 to account for
                    !  increased efficiency requirements
!                    NHtoUse=(NH(CurCalYr,B,D)-NH(2012,B,D))*SHTEFF + NH(2012,B,D)	!kj - commented out because 2012 is prior to 2015 RECS base year
!                  ENDIF !CurCalYr > 2012	!kj - commented out because 2012 is prior to 2015 RECS base year
!                ENDIF !F=1 or F=4 and CurCalYr>2012	!kj - commented out because 2012 is prior to 2015 RECS base year

                SHTCON(Y,F,D)=SHTCON(Y,F,D)+LEAPYR*(SHTSHR(B,D,F)* &
                 EH(CurCalYr,B,D)*SHTUEC(D,F,B)* &
                 HDDFACT(D)*(EHSHELL(CurCalYr,FShell,D,B)/EHSHELL(RECSYear,FShell,D,B)) + &
                 NSHTSHR(B,D,F)*NHtoUse*SHTUEC(D,F,B)*HDDFACT(D)* &
                 (AHSHELL(CurCalYr,FShell,D,B)/EHSHELL(RECSYear,FShell,D,B)))* &
                 RSELAST(F2,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

                SHTCONWT(Y,F,D,B)=SHTCONWT(Y,F,D,B)+LEAPYR*(SHTSHR(B,D,F)* &
                 EH(CurCalYr,B,D)*SHTUEC(D,F,B)* &
                 HDDFACT(D)*(EHSHELL(CurCalYr,FShell,D,B)/EHSHELL(RECSYear,FShell,D,B)) + &
                 NSHTSHR(B,D,F)*NHtoUse*SHTUEC(D,F,B)*HDDFACT(D)* &
                 (AHSHELL(CurCalYr,FShell,D,B)/EHSHELL(RECSYear,FShell,D,B)))* &
                 RSELAST(F2,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

                IF ((EH(CurCalYr,B,D)+NH(CurCalYr,B,D)).GT.0. .AND. ehshell(RECSYear,fshell,d,b).GT.0. .AND. &
                     SHTSHR(B,D,F).GT.0. .AND. ahshell(CurCalYr,fshell,d,b).GT.0. .AND. NSHTSHR(B,D,F).GT.0. ) THEN
                  SHTCONIN(Y,F,D,B)=SHTCONIN(Y,F,D,B)+( (SHTSHR(B,D,F)* &
                   EH(CurCalYr,B,D)*SHTUEC(D,F,B)* &
                   (EHSHELL(CurCalYr,FShell,D,B)/EHSHELL(RECSYear,FShell,D,B)) + &
                   NSHTSHR(B,D,F)*NHtoUse*SHTUEC(D,F,B)*HDDFACT(D)* &
                   (AHSHELL(CurCalYr,FShell,D,B)/EHSHELL(RECSYear,FShell,D,B))) )&
                   / (EH(CurCalYr,B,D)+NH(CurCalYr,B,D))
                ENDIF

                SHEQCN(Y,F,B,D)=LEAPYR*(SHTSHR(B,D,F)* &
                 EH(CurCalYr,B,D)*SHTUEC(D,F,B)* &
                 HDDFACT(D)*(EHSHELL(CurCalYr,FShell,D,B)/EHSHELL(RECSYear,FShell,D,B)) + &
                 NSHTSHR(B,D,F)*NHtoUse*SHTUEC(D,F,B)*HDDFACT(D)* &
                 (AHSHELL(CurCalYr,FShell,D,B)/EHSHELL(RECSYear,FShell,D,B)))* &
                 RSELAST(F2,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)

           ENDDO ! B=1,mNumBldg
          ENDDO ! D=1,mNumCR-2
         ENDDO ! F=1,7

      END SUBROUTINE SHTCNS


!********************************************************************
!     APPLIANCE CONSUMPTION
!********************************************************************
      SUBROUTINE APPCNS
      IMPLICIT NONE
      REAL*4 ALPHA,ef1,ef2,ef3,LPGGRILL(RECSYear:EndYr)
      INTEGER Y, B, D,F,F1,Y1,EUPR
      Y=CurCalYr-(BaseYr-1)
      ALPHA=-.15;ef1=.5;ef2=.35;ef3=.15
      EUPR=10
!********************************************************************
! APPL= G,L,D
!********************************************************************
      LPGGRILL(RECSYear)=0.36  !share of homes with propane grills in the RECS year
      LPGGRILL(ijumpcalyr)=0.60  !share of homes with propane grills by end of projection (source unknown)	!kj
      IF (CurCalYr.EQ.(RECSYear+1)) THEN
      DO 30 D=1,mNumCR-2
       DO 30 B=1,mNumBldg
        DO 30 F=1,3
           APLEQP(RECSYear,B,D,F)=APPEQP(RECSYear,B,D,F)
 30 CONTINUE
       ENDIF
     IF ((CurCalYr.EQ.RECSYear+1).AND.(CURITR.EQ.1)) THEN
       DO Y1=RECSYear+1,ijumpcalyr
         LPGGRILL(Y1)=LPGGRILL(Y1-1)+((LPGGRILL(ijumpcalyr)-LPGGRILL(RECSYear))/(ijumpcalyr-RECSYear))  !calculates average annual penetration rate of propane grills into residential households
       ENDDO
     ENDIF
      DO 40 D=1,mNumCR-2
       DO 40 B=1,mNumBldg
        DO 40 F=1,3
          IF (F.EQ.2) THEN
           APLEQP(CurCalYr,B,D,F)=((APPEQP(RECSYear,B,D,F)/EH(RECSYear,B,D))*(EH(CurCalYr,B,D)+NH(CurCalYr,B,D))&
                                      *(LPGGRILL(CurCalYr)/LPGGRILL(RECSYear)) )
          ELSE
           APLEQP(CurCalYr,B,D,F)=((APPEQP(RECSYear,B,D,F)/EH(RECSYear,B,D))*EH(CurCalYr,B,D))
          ENDIF
 40 CONTINUE

      DO 50 D=1,mNumCR-2
        DO 50 F=1,3
           IF (F.EQ.1) F1=3
           IF (F.EQ.2) F1=2
           IF (F.EQ.3) F1=1
          APLCON(Y,F,D)=0.0
          DO 50 B=1,mNumBldg
           APLCON(Y,F,D)= APLCON(Y,F,D)+ LEAPYR*APLEQP(CurCalYr,B,D,F)*APPUEC(D,F,B) &
             *RSELAST(F1,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
           APLCONWT(Y,F,D,B)= LEAPYR*APLEQP(CurCalYr,B,D,F)*APPUEC(D,F,B) &
             *RSELAST(F1,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
        IF (EH(CurCalYr,B,D)+NH(CurCalYr,B,D).GT.0.) THEN
           APLCONIN(Y,F,D,B)= (APLEQP(CurCalYr,B,D,F)*APPUEC(D,F,B)) &
                   / (EH(CurCalYr,B,D)+NH(CurCalYr,B,D))
        ENDIF
            APEQCN(Y,F,B,D)=LEAPYR*APLEQP(CurCalYr,B,D,F)*APPUEC(D,F,B) &
             *RSELAST(F1,D,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
 50   CONTINUE
      END SUBROUTINE APPCNS
!********************************************************************
!     CALCULATE FUEL CONSUMPTION
!********************************************************************
      SUBROUTINE FUELCN
      IMPLICIT NONE
      INTEGER Y,D,F
      Y=CurCalYr-(BaseYr-1)
!********************************************************************
!     CALCULATE DIVISIONAL FUEL CONSUMPTION
!********************************************************************
      DO 10 D=1,mNumCR-2
! NATURAL GAS
         RSFLCN(Y,1,D)= &
           (HTRCON(Y,1,D)+H2OCON(Y,1,D)+CKCON(Y,1,D)+DRYCON(Y,1,D)+COOLCN(Y,3,D)+SHTCON(Y,1,D)+APLCON(Y,1,D))/1000000.
! ELECTRICITY
         RSFLCN(Y,2,D)= &
         (HTRCON(Y,2,D)+COOLCN(Y,1,D)+H2OCON(Y,2,D)+REFCON(Y,D)+CKCON(Y,3,D)+DRYCON(Y,2,D)+SHTCON(Y,2,D)+TVRCON(Y,D)+ &
          FRZCON(Y,D)+LTCON(Y,D)+APCON(Y,D)+PCRCON(Y,D)+FANCON(Y,D)+CSWCON(Y,D)+DSWCON(Y,D))/1000000.
! DISTILLATE FUEL OIL
         RSFLCN(Y,3,D)= (APLCON(Y,3,D)+HTRCON(Y,3,D)+H2OCON(Y,3,D)+SHTCON(Y,3,D))/1000000.
! PROPANE
         RSFLCN(Y,4,D)= (SHTCON(Y,4,D)+APLCON(Y,2,D)+HTRCON(Y,4,D)+H2OCON(Y,4,D)+CKCON(Y,2,D))/1000000.
! WOOD
         RSFLCN(Y,7,D)=(HTRCON(Y,6,D)+SHTCON(Y,7,D))/1000000.
! GEOTHERMAL
         RSFLCN(Y,8,D)=(HTRCON(Y,7,D)+COOLCN(Y,2,D))/1000000.
 10      CONTINUE
!********************************************************************
!     CALCULATE US (DIVISION 10) FUEL CONSUMPTION
!********************************************************************
      DO 20 F=1,8
         RSFLCN(Y,F,10)=0.0
         DO 20 D=1,mNumCR-2
            RSFLCN(Y,F,10)=RSFLCN(Y,F,10)+RSFLCN(Y,F,D)
 20   CONTINUE
      END SUBROUTINE FUELCN
!********************************************************************
!     OUTPUT NEMS CONSUMPTION
!********************************************************************
      SUBROUTINE NEMSCN
      IMPLICIT NONE
      INTEGER Y,D,F

      Y=CurCalYr-(BaseYr-1)
      SLCON(Y,10)=0.0
      QPVRS(NationalPtr,y)=0.0

!********************************************************************
!     CALCULATE DIVISIONAL FUEL CONSUMPTION
!********************************************************************
      DO 10 D=1,mNumCR-2
! RENEWABLES
!      SOLAR CHANGED FOR DISTRIBUTED GENERATION BY PV
         IF(CurIYr.LT.RECSYear-BaseYr+1) THEN
            QPVRS(D,Y)=QPVRS(D,Y-1)*WHRFOSS(D,Y)/3412.  !STEOhr
!      SOLAR KWH GENERATION BY PV IN QUADS
          ELSE
            QPVRS(D,Y)=Trills(Y,D,1)*WHRFOSS(D,Y)/3412.  !STEOhr
         ENDIF
            QPVRS(NationalPtr,y)=QPVRS(NationalPtr,y)+QPVRS(d,y)

         SLCON(Y,10)=SLCON(Y,10)+SLCON(Y,D)
         QSTRS(D,Y)=SLCON(Y,D)/1000000.
! NATURAL GAS
         QNGRS(D,Y)=RSFLCN(Y,1,D)
         QGFRS(D,Y)=QNGRS(D,Y)*1.0
         QGIRS(D,Y)=QNGRS(D,Y)*0.0	!kj - is this still necessary if multiplied by zero?
! ELECTRICITY
         QELRS(D,Y)=RSFLCN(Y,2,D)-(TrillsOwnUse(Y,D,1)+TrillsOwnUse(Y,D,2)+TrillsOwnUse(Y,D,3))/1000.  !Subtract all onsite own-use generation; QELRS = quads purchased electricity from grid  !DGreport
! DISTILLATE FUEL OIL + KEROSENE
         IF (RSFLCN(Y,3,D).LT.0.) THEN
           QDSRS(D,Y)=0.
         ELSE
           QDSRS(D,Y)=RSFLCN(Y,3,D)
         ENDIF
! PROPANE
         QLGRS(D,Y)=RSFLCN(Y,4,D)
         QPRRS(D,Y)=QLGRS(D,Y)
! KEROSENE
         QKSRS(D,Y)=0.0  !KeroBench
! WOOD
         QBMRS(D,Y)=RSFLCN(Y,7,D)
! GEOTHERMAL
         QGERS(D,Y)=RSFLCN(Y,8,D)
 10   CONTINUE
      END SUBROUTINE NEMSCN


!********************************************************************
!     OUTPUT RESDREP
!********************************************************************
      SUBROUTINE RESDRP
      IMPLICIT NONE
      INTEGER Y, B, D, F, F2,Y1

!*******************************************************************
!  AGGREGATE EXISTING HOUSES, NEW HOUSES & HOUSING STARTS
!*******************************************************************
      Y=CurIYr
        DO 10 B=1,mNumBldg
          RSEH(Y,B)=0.0
          RSNH(Y,B)=0.0
          RSHSEADD(Y,B)=0.0
          DO 10 D=1,mNumCR-2
            RSEH(Y,B)=RSEH(Y,B)+EH(CurCalYr,B,D)
            RSNH(Y,B)=RSNH(Y,B)+NH(CurCalYr,B,D)
            RSHSEADD(Y,B)=RSHSEADD(Y,B)+HSEADD(CurCalYr,B,D)
 10   CONTINUE

!*******************************************************************
!  AGGREGATE EXISTING HOUSES, NEW HOUSES & HOUSING STARTS
!*******************************************************************
          HSETOT(CurCalYr)=0.0
        DO 15 D=1,mNumCR-2
          RSHOUSES(Y,D)=0.0
          DO 15 B=1,mNumBldg
            RSHOUSES(Y,D)=RSHOUSES(Y,D)+(EH(CurCalYr,B,D)+NH(CurCalYr,B,D))
            TotalHouses(y,b,d)=(EH(CurCalYr,B,D)+NH(CurCalYr,B,D))
            HSETOT(CurCalYr)=HSETOT(CurCalYr)+RSHOUSES(Y,D)
 15   CONTINUE

!*******************************************************************
!  COMPUTE HOUSING SHARES FOR "FROZEN HOUSING STARTS" ANALYSIS	!kj - is this used by MAM or elsewhere in NEMS, or can this be removed?
!*******************************************************************
        DO 17 D=1,mNumCR-2
          DO 17 B=1,mNumBldg
            HSESHR(CurCalYr,B,D)=(EH(CurCalYr,B,D)+NH(CurCalYr,B,D))/HSETOT(CurCalYr)
 17   CONTINUE

!*******************************************************************
!  AGGREGATE HEATING CONSUMPTION
!*******************************************************************
        DO 20 F=1,8	!kj - should this be tired to NHTRFL, though that current value is 7 because coal (8) was removed
          RSHTRCON(Y,F)=0.0
          DO 20 D=1,mNumCR-2
            IF (F.EQ.7) THEN
              RSHTRCON(Y,7)=RSHTRCON(Y,7)+HTRCON(Y,7,D)
            ELSEIF (F.EQ.6) THEN
              RSHTRCON(Y,6)=RSHTRCON(Y,6)+HTRCON(Y,6,D)+SHTCON(Y,7,D)
            ELSEIF (F.EQ.8) THEN
!              RSHTRCON(Y,8)=RSHTRCON(Y,8)+SHTCON(Y,6,D)  !removing coal from consumption totals	!kj - do same for kerosene?
              RSHTRCON(Y,8)=0.0
            ELSE
              RSHTRCON(Y,F)=RSHTRCON(Y,F)+HTRCON(Y,F,D)+SHTCON(Y,F,D)
            ENDIF
 20   CONTINUE

!*******************************************************************
!  AGGREGATE COOLING CONSUMPTION
!*******************************************************************
           RSCOOLCN(Y,1:NCLFL)=0.0 !DGreport - general cleanup

         DO D=1,mNumCR-2 !DGreport
		   DO F=1,NCLFL !DGreport
             RSCOOLCN(Y,F)=RSCOOLCN(Y,F)+COOLCN(Y,F,D) !DGreport
           ENDDO !DGreport
         ENDDO !DGreport

!*******************************************************************
!  AGGREGATE WATER HEATING CONSUMPTION
!*******************************************************************
          RSH2OCON(Y,5)=SLCON(Y,10)
        DO 30 F=1,NWHFL-1  !excludes solar water heating consumption from previous line  !DG report - general cleanup
          RSH2OCON(Y,F)=0.0
         DO 30 D=1,mNumCR-2
          RSH2OCON(Y,F)=RSH2OCON(Y,F)+H2OCON(Y,F,D)
 30   CONTINUE

!*******************************************************************
!  AGGREGATE COOKING CONSUMPTION
!*******************************************************************
          RSCKCON(Y,1:NSTVFL)=0.0 !DGreport - general cleanup

        DO 35 D=1,mNumCR-2
          RSCKCON(Y,1)=RSCKCON(Y,1)+CKCON(Y,1,D)
          RSCKCON(Y,2)=RSCKCON(Y,2)+CKCON(Y,2,D)
          RSCKCON(Y,3)=RSCKCON(Y,3)+CKCON(Y,3,D)
 35   CONTINUE

!*******************************************************************
!  AGGREGATE DRYERS CONSUMPTION
!*******************************************************************
        DO 37 F=1,NDRYFL  !DGreport - general cleanup
            RSDRYCON(Y,F)=0.0
          DO 37 D=1,mNumCR-2
            RSDRYCON(Y,F)=RSDRYCON(Y,F)+DRYCON(Y,F,D)
 37   CONTINUE

!*******************************************************************
!  AGGREGATE APPLIANCE CONSUMPTION
!*******************************************************************
            RSAPCON(Y,1:4)=0.0 !DGreport - general cleanup	!kj - find variable to replace 4

       DO 40 D=1,mNumCR-2
            RSAPCON(Y,1)=RSAPCON(Y,1)+APLCON(Y,1,D) !Natural Gas
            RSAPCON(Y,2)=RSAPCON(Y,2)+APCON(Y,D)    !Electricity
            RSAPCON(Y,3)=RSAPCON(Y,3)+APLCON(Y,3,D) !Distillate Fuel Oil/Kerosene
            RSAPCON(Y,4)=RSAPCON(Y,4)+APLCON(Y,2,D) !Propane
 40   CONTINUE

!*******************************************************************
!  AGGREGATE THE REST OF CONSUMPTION
!*******************************************************************
            RSREFCON(Y)=0.0
            RSFRZCON(Y)=0.0
            RSLTCON(Y) =0.0
            RSLTCON(RECSYear-(BaseYr-1)) =0.0
            RSCSWCON(Y)=0.0
            RSDSWCON(Y)=0.0
            RSTVRCON(Y)=0.0
            RSPCRCON(Y)=0.0
            RSFANCON(Y)=0.0
       DO 50 D=1,mNumCR-2
            RSREFCON(Y)=RSREFCON(Y)+REFCON(Y,D)
            RSFRZCON(Y)=RSFRZCON(Y)+FRZCON(Y,D)
            RSLTCON(RECSYear-(BaseYr-1))= RSLTCON(RECSYear-(BaseYr-1))+LTCON(RECSYear-(BaseYr-1),D)
            RSLTCON(Y)= RSLTCON(Y)+LTCON(Y,D)
            RSCSWCON(Y)=RSCSWCON(Y)+CSWCON(Y,D)
            RSDSWCON(Y)=RSDSWCON(Y)+DSWCON(Y,D)
            RSTVRCON(Y)=RSTVRCON(Y)+TVRCON(Y,D)
            RSFANCON(Y)=RSFANCON(Y)+FANCON(Y,D)
            RSPCRCON(Y)=RSPCRCON(Y)+PCRCON(Y,D)
 50   CONTINUE
      END SUBROUTINE RESDRP


!********************************************************************
!     SUPPLEMENTAL NEMS REPORT
!*******************************************************************
      SUBROUTINE RESDRP2
      IMPLICIT NONE
      ! COMMON/DBEFFOUT/RSNEFDB1(mNumYr,MNUMRTTY,mNumBldg,mNumCR-2),RSEEFDB1(mNumYr,MNUMRTTY,mNumBldg,mNumCR-2)  !DYN
      ! REAL*4 RSNEFDB1,RSEEFDB1  !DYN
      REAL*4 EHANDNH(RECSYear:EndYr,mNumBldg,mNumCR-2)
!      REAL*4 TBENCH(RECSYear:EndYr,6),ABENCH(RECSYear:EndYr,6),TRSCON(RECSYear:EndYr,6)
      REAL*4 RACUnits(mNumBldg,mNumCR-2), X, TEMP
      REAL*4 NUME(RECSYear:EndYr+1,15), DEN(RECSYear:EndYr+1,15),NUME1(RECSYear:EndYr+1,15,mNumBldg,mNumCR-2), DEN1(RECSYear:EndYr+1,15,mNumBldg,mNumCR-2)
      REAL*4 RSCLUSR(RECSYear:EndYr+1)
      REAL*4 RSCLUSC(RECSYear:EndYr+1)
      REAL*4 HSHINDE(RECSYear:EndYr,6,mNumCR-2,mNumBldg),HSHINDA(RECSYear:EndYr,6,mNumCR-2,mNumBldg),HSHINDN(RECSYear:EndYr,6,mNumCR-2,mNumBldg), &
             HSHELLE(RECSYear:EndYr),HEATOTE(RECSYear:EndYr), &
             HSHELLN(RECSYear:EndYr),HEATOTN(RECSYear:EndYr), &
             HSHELLA(RECSYear:EndYr),HEATOTA(RECSYear:EndYr)
      REAL*4 CSHINDE(RECSYear:EndYr,mNumCR-2,mNumBldg),CSHINDA(RECSYear:EndYr,mNumCR-2,mNumBldg),CSHINDN(RECSYear:EndYr,mNumCR-2,mNumBldg), &
             CSHELLE(RECSYear:EndYr),COLTOTE(RECSYear:EndYr), &
             CSHELLN(RECSYear:EndYr),COLTOTN(RECSYear:EndYr), &
             CSHELLA(RECSYear:EndYr),COLTOTA(RECSYear:EndYr)
      INTEGER Y, D, B, E, E2, E3, F, T, EV,V,Y1
      INTEGER EU,RECCL,EQC,NUMEQC,RECTY,TYPE,EQT,NUMEQT,OTYPE
      INTEGER EUHT,RECCLHHP,RECTYHT,TYPEHT
      CHARACTER*15  EUPRNAMES(10) !electricity end-use price categories
      DATA EUPRNAMES/'Space Heating','Space Cooling','Water Heating','Cooking', &
                  'Clothes Drying','Refrigeration','Freezing','Lighting', &
                  'Appliances','Secondary Heat'/
      CHARACTER*40 FN
      CHARACTER*18 HEN(9),CEN(5),WEN(5), CKEN(3), DRYEN(2)	!kj - not used?
      DATA HEN/'Electric HP','Other Electric','Gas HP','Gas Other', &	!kj - why does this exclude natural gas and distillate furnaces? not used?
      'Distillate','Propane','Kerosene','Wood Stoves','Geothermal HP'/	!kj - why does this exclude natural gas and distillate furnaces? not used?
      DATA CEN/'Room AC','Central Air','Heat Pump','GSHP','GHP'/	!kj - these are mapped in a different order as RSCOOLERS; not used?
      DATA WEN/'Natural Gas','Electric','Distillate','Propane','Solar'/	!kj - not used?
      DATA CKEN/'Natural Gas','Propane','Electric'/	!kj - not used?
      DATA DRYEN/'Natural Gas','Electric'/	!kj - not used?

!*******************************************************************
!   AGGREGATE HEATING SYSTEMS
!   SET EU = 1 TO SEARCH THE SPACE HEATING SECTION OF THE DATA
!*******************************************************************
      EU = 1

      DO 2 Y=RECSYear,LastYr+BaseYr-1
         Y1=Y-BaseYr+1
       DO 2 E2=1,9	!kj - is there a variable to refer to the 9 heating tech equipments listed below? Why aren't all 11 heating classes included?
          RSHTRS(Y1,E2)=0.0
 2    CONTINUE
      DO 4 Y=RECSYear,LastYr+BaseYr-1
        Y1=Y-BaseYr+1
        DO 4 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)	!kj - where is EQC 3 (NG_FA) and 7 (DIST_FA)?
          IF (EQC.EQ.1)  THEN  !ELEC_RAD
             E2=2
          ELSEIF (EQC.EQ.2) THEN  !ELEC_HP
             E2=1
          ELSEIF (EQC.LE.4) THEN  !NG_RAD
             E2=4
          ELSEIF (EQC.EQ.5) THEN  !KERO_FA	!kj - kerosene is being combined with distillate (DIST_FA)
             E2=7
          ELSEIF (EQC.EQ.6) THEN  !LPG_FA
             E2=6
          ELSEIF (EQC.LE.8) THEN  !DIST_RAD
             E2=5
          ELSEIF (EQC.EQ.9) THEN  !WOOD_HT
             E2=8
          ELSEIF (EQC.EQ.10) THEN  !GEO_HP
             E2=9
          ELSEIF (EQC.EQ.11) THEN  !NG_HP
             E2=3
          ENDIF
          DO 4 B=1,mNumBldg
            DO 4 D=1,mNumCR-2
              IF (Y.EQ.RECSYear) THEN
                RSHTRS(Y1,E2)=RSHTRS(Y1,E2)+EQCESE(Y,RECCL,B,D)
              ELSE
                RSHTRS(Y1,E2)=RSHTRS(Y1,E2)+  EQCESE(Y,RECCL,B,D)+ &
                EQCRP90(Y,RECCL,B,D)+  EQCSR90(Y,RECCL,B,D)+ &
                EQCADD(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D) &
               +EQCSUR(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
              ENDIF
 4    CONTINUE

!*******************************************************************
!     AGGREGATE HEATING SYSTEMS FOR COMPUTING AGGREGATE SHELL
!*******************************************************************
      DO 5 Y=RECSYear,LastYr+BaseYr-1
       DO 5 F=1,MNUMFUEL-2
        DO 5 D=1,mNumCR-2
         DO 5 B=1,mNumBldg
          HSHINDE(Y,F,D,B)=0.0
          HSHINDA(Y,F,D,B)=0.0
          HSHINDN(Y,F,D,B)=0.0
 5    CONTINUE
      DO 6 Y=RECSYear,LastYr+BaseYr-1
       DO 6 D=1,mNumCR-2
        DO 6 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          E=RTCLEQCL(RECCL)
          F=RTFUEL(RECCL)
          DO 6 B=1,mNumBldg
              IF (Y.EQ.RECSYear) THEN
                HSHINDE(Y,F,D,B)=HSHINDE(Y,F,D,B)+EQCESE(Y,RECCL,B,D)
                HSHINDN(Y,F,D,B)=0.0
                HSHINDA(Y,F,D,B)=0.0
              ELSE
                HSHINDE(Y,F,D,B)=HSHINDE(Y,F,D,B)+EQCESE(Y,RECCL,B,D) &
              +   EQCRP90(Y,RECCL,B,D) + EQCSR90(Y,RECCL,B,D) &
              +EQCRP90RP(Y,RECCL,B,D)
                HSHINDN(Y,F,D,B)=HSHINDN(Y,F,D,B)+EQCADD(Y,RECCL,B,D)
                HSHINDA(Y,F,D,B)=HSHINDA(Y,F,D,B) &
              +   EQCREP(Y,RECCL,B,D) + EQCSUR(Y,RECCL,B,D)
              ENDIF
 6    CONTINUE

!*******************************************************************
!     CALCULATE HEATING SHELL INDICES FOR REPORT
!*******************************************************************
      DO 7 Y=RECSYear,LastYr+BaseYr-1
        HSHELLE(Y)= 0.0
        HSHELLN(Y)= 0.0
        HSHELLA(Y)= 0.0
        DO 7 F=1,MNUMFUEL-2
          DO 7 D=1,mNumCR-2
           DO 7 B=1,mNumBldg
            IF (Y.EQ.RECSYear) THEN
            HSHELLE(Y)=HSHELLE(Y)+(HSHINDE(Y,F,D,B)*EHSHELL(Y,F,D,B))
            HSHELLN(Y)=HSHELLN(Y)+(HSHINDE(Y,F,D,B)*EHSHELL(Y,F,D,B))
            HSHELLA(Y)=HSHELLA(Y)+(HSHINDE(Y,F,D,B)*EHSHELL(Y,F,D,B))
            ELSE
            HSHELLE(Y)=HSHELLE(Y) + (HSHINDE(Y,F,D,B)*EHSHELL(Y,F,D,B))
            HSHELLN(Y)=HSHELLN(Y) + (HSHINDN(Y,F,D,B)*NHSHELL(Y,F,D,B))
            HSHELLA(Y)=HSHELLA(Y) + (HSHINDA(Y,F,D,B)*AHSHELL(Y,F,D,B) &
                                  +  HSHINDN(Y,F,D,B)*NHSHELL(Y,F,D,B) &
                                  +  HSHINDE(Y,F,D,B)*EHSHELL(Y,F,D,B))
            ENDIF
 7    CONTINUE
          DO 8 Y=RECSYear,LastYr+BaseYr-1
              HEATOTE(Y)=0.0
              HEATOTN(Y)=0.0
              HEATOTA(Y)=0.0
            DO 8 F=1,MNUMFUEL-2
              DO 8 D=1,mNumCR-2
               DO 8 B=1,mNumBldg
                HEATOTE(Y)=HEATOTE(Y)+HSHINDE(Y,F,D,B)
                HEATOTN(Y)=HEATOTN(Y)+HSHINDN(Y,F,D,B)
                HEATOTA(Y)=HEATOTA(Y)+HSHINDN(Y,F,D,B)+HSHINDE(Y,F,D,B) &
            +   HSHINDA(Y,F,D,B)
 8    CONTINUE

!*******************************************************************
          DO 10 Y=RECSYear,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
           IF (HEATOTE(Y).GT.0.0) THEN
               HSHELL1(Y1)=HSHELLE(Y)/HEATOTE(Y)
           ELSE
               HSHELL1(Y1)=1.0
           ENDIF
           IF (HEATOTN(Y).GT.0.0) THEN
               HSHELL2(Y1)=HSHELLN(Y)/HEATOTN(Y)
           ELSE
               HSHELL2(Y1)=1.0
           ENDIF
           IF (HEATOTA(Y).GT.0.0) THEN
               HSHELL3(Y1)=HSHELLA(Y)/HEATOTA(Y)
           ELSE
               HSHELL3(Y1)=1.0
           ENDIF
 10   CONTINUE

!*******************************************************************
!   WRITE OUT VARIABLES OF SPECIAL INTEREST TO UNIT 9 (RESOUT.TXT)
!*******************************************************************
        WRITE(9,*) 'fuel prices'
        DO D=1,mNumCR-2
          WRITE(9,*) 'D=', D
          WRITE(9,*) 'Dist   Prop    NatGas    Elec    Kero   Wood'
          DO Y=RECSYear,LastYr+BaseYr-1
            WRITE(9,223) Y,(PRICES(F,D,Y),F=1,6)
          ENDDO
        ENDDO
 223    FORMAT(3X,I5,6(3X,F5.2))

        WRITE(9,*) 'ALL HOUSING STARTS'
        DO D=1,mNumCR-2
          WRITE(9,*) 'D=', D
          DO Y=RECSYear,LastYr+BaseYr-1
            WRITE(9,224) Y,(HSEADD(Y,B,D),B=1,mNumBldg)
          ENDDO
        ENDDO
224    FORMAT(3X,I5,3(3X,F9.2))

        WRITE(9,*) 'average squarefootage all housing types and divisions'
        DO Y=RECSYear-BaseYr+1,LastYr
         WRITE(9,*) Y+BaseYr-1, SQFTAVG(Y)
        ENDDO

        WRITE(9,*) 'single-family replacement uecs - heating'
        DO d=1,mNumCR-2
          WRITE(9,*) 'D=',D
          WRITE(9,1140)(RTCLNAME(E),E=1,nHeatClasses)
          DO Y=RECSYear,LastYr+BaseYr-1
            WRITE(9,1141) Y,(EQCRUEC(Y,E,1,D),E=1,nHeatClasses)
          ENDDO
        ENDDO
 1140   FORMAT(6X,11(1X,a9))
 1141   FORMAT(I6,11(1X,F9.4))

        WRITE(9,*) 'single-family average uecs - heating'
        DO d=1,mNumCR-2
          WRITE(9,*) 'D=',D
          WRITE(9,1140)(RTCLNAME(E),E=1,nHeatClasses)
          DO Y=RECSYear,LastYr+BaseYr-1
            WRITE(9,1141) Y,(EQCAUEC(Y,E,1,D),E=1,nHeatClasses)
          ENDDO
        ENDDO

        WRITE(9,*) 'single-family new uecs - heating'
        DO d=1,mNumCR-2
          WRITE(9,*) 'D=',D
          WRITE(9,1140)(RTCLNAME(E),E=1,nHeatClasses)
          DO Y=RECSYear,LastYr+BaseYr-1
            WRITE(9,1141) Y,(EQCNUEC(Y,E,1,D),E=1,nHeatClasses)
          ENDDO
        ENDDO

      WRITE(9,*) 'BNCHFCT(trills?)'	!kj - is this actually in quads? output values in RESOUT.txt seem much too high to be quads, but the variables used to calculate BNCHFCT appear to be in quads
      DO Y=(RECSYear-BaseYr+1),(LASTSTEOYR-BaseYr+3)  !BNCHFCT adjustment for RECS through first two years after hard-bench STEO year  !STEOread-avg
                                                      !(last two years of factors should be same and carried through projection)       !STEOread-avg
        WRITE(9,*) Y+BaseYr-1
        DO D=1,mNumCR-2
          WRITE(9,114) D,(BNCHFCT(Y,F,D),F=1,4)
        ENDDO
      ENDDO
 114  FORMAT(2X,I1,4(1X,F9.4))

!*******************************************************************
!   AGGREGATE COOLING SYSTEMS
!   SET EU = 2 TO SEARCH THE SPACE COOLING SECTION OF THE DATA
!*******************************************************************
      EU = 2

      DO 20 Y=RECSYear,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        DO 20 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          E=RTCLEQCL(RECCL)
          IF (E.EQ.1) THEN  !ROOM_AIR
             E2=5
          ELSEIF (E.EQ.2) THEN  !CENT_AIR
             E2=4
          ELSEIF (E.EQ.3) THEN  !ELEC_HP
             E2=1
          ELSEIF (E.EQ.4) THEN  !GEO_HP
             E2=3
          ELSEIF (E.EQ.5) THEN  !NG_HP
             E2=2
          ENDIF
          RSCOOLERS(Y1,E2)=0.0
          DO 20 B=1,mNumBldg
            DO 20 D=1,mNumCR-2
              X=1.0
              IF (RTCLNAME(RECCL).EQ.'ROOM_AIR') X=1.0 ! RACUnits(B,D)

              IF (Y.EQ.RECSYear) THEN
                RSCOOLERS(Y1,E2)=RSCOOLERS(Y1,E2)+EQCESE(Y,RECCL,B,D)*X
              ELSE
                RSCOOLERS(Y1,E2)=RSCOOLERS(Y1,E2)+ (  EQCESE(Y,RECCL,B,D) + &
                EQCRP90(Y,RECCL,B,D)+ &
                EQCSR90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D) + &
                EQCADD(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D) + &
                EQCSUR(Y,RECCL,B,D))*X
              ENDIF
 20   CONTINUE

!*******************************************************************
!     AGGREGATE COOLING SYSTEMS FOR COMPUTING AGGREGATE SHELL
!*******************************************************************
      DO 105 Y=RECSYear,LastYr+BaseYr-1
        DO 105 D=1,mNumCR-2
         DO 105 B=1,mNumBldg
          CSHINDE(Y,D,B)=0.0
          CSHINDA(Y,D,B)=0.0
          CSHINDN(Y,D,B)=0.0
 105    CONTINUE
      DO 106 Y=RECSYear,LastYr+BaseYr-1
       DO 106 D=1,mNumCR-2
        DO 106 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          E=RTCLEQCL(RECCL)
          DO 106 B=1,mNumBldg
              IF (Y.EQ.RECSYear) THEN
                CSHINDE(Y,D,B)=CSHINDE(Y,D,B)+EQCESE(Y,RECCL,B,D)
              ELSE
                CSHINDE(Y,D,B)=CSHINDE(Y,D,B)+EQCESE(Y,RECCL,B,D)+ &
                 EQCRP90(Y,RECCL,B,D) + EQCSR90(Y,RECCL,B,D)+ &
                 EQCRP90RP(Y,RECCL,B,D)
                CSHINDN(Y,D,B)=CSHINDN(Y,D,B)+EQCADD(Y,RECCL,B,D)
                CSHINDA(Y,D,B)=CSHINDA(Y,D,B)+ &
                 EQCREP(Y,RECCL,B,D) + EQCSUR(Y,RECCL,B,D)
              ENDIF
 106    CONTINUE

!*******************************************************************
!     CALCULATE COOLING SHELL INDICES FOR REPORT
!*******************************************************************
      DO 107 Y=RECSYear,LastYr+BaseYr-1
        CSHELLE(Y)= 0.0
        CSHELLN(Y)= 0.0
        CSHELLA(Y)= 0.0
        DO 107 D=1,mNumCR-2
         DO 107 B=1,mNumBldg
          IF (Y.EQ.RECSYear) THEN
            CSHELLE(Y)=CSHELLE(Y) + (CSHINDE(Y,D,B)*ECSHELL(Y,D,B))
            CSHELLN(Y)=CSHELLN(Y) + (CSHINDE(Y,D,B)*ECSHELL(Y,D,B))
            CSHELLA(Y)=CSHELLA(Y) + (CSHINDE(Y,D,B)*ECSHELL(Y,D,B))
          ELSE
            CSHELLE(Y)=CSHELLE(Y) + (CSHINDE(Y,D,B)*ECSHELL(Y,D,B))
            CSHELLN(Y)=CSHELLN(Y) + (CSHINDN(Y,D,B)*NCSHELL(Y,D,B))
            CSHELLA(Y)=CSHELLA(Y) + (CSHINDE(Y,D,B)*ECSHELL(Y,D,B) &
                                  +  CSHINDN(Y,D,B)*NCSHELL(Y,D,B) &
                                  +  CSHINDA(Y,D,B)*ACSHELL(Y,D,B))
          ENDIF
 107  CONTINUE

      DO 108 Y=RECSYear,LastYr+BaseYr-1
        COLTOTE(Y)=0.0
        COLTOTN(Y)=0.0
        COLTOTA(Y)=0.0
        DO 108 D=1,mNumCR-2
         DO 108 B=1,mNumBldg
          COLTOTE(Y)=COLTOTE(Y)+ CSHINDE(Y,D,B)
          COLTOTN(Y)=COLTOTN(Y)+ CSHINDN(Y,D,B)
          COLTOTA(Y)=COLTOTA(Y)+ CSHINDE(Y,D,B) + CSHINDN(Y,D,B) + CSHINDA(Y,D,B)
 108  CONTINUE

!*******************************************************************
      DO 109 Y=RECSYear,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        IF (COLTOTE(Y).GT.0.0) THEN
          CSHELL1(Y1)=CSHELLE(Y)/COLTOTE(Y)
        ELSE
          CSHELL1(Y1)=1.0
        ENDIF
        IF (COLTOTN(Y).GT.0.0) THEN
          CSHELL2(Y1)=CSHELLN(Y)/COLTOTN(Y)
        ELSE
          CSHELL2(Y1)=1.0
        ENDIF
        IF (COLTOTA(Y).GT.0.0) THEN
          CSHELL3(Y1)=CSHELLA(Y)/COLTOTA(Y)
        ELSE
          CSHELL3(Y1)=1.0
        ENDIF
 109  CONTINUE

!*******************************************************************
!     AGGREGATE WATER HEATING SYSTEMS
!*******************************************************************
      DO 25 Y=RECSYear,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        DO 25 E=1,5  !Natural gas, electricity, distillate fuel oil, propane, solar
          RSWATER(Y1,E)=0.0
 25   CONTINUE

!*******************************************************************
!   SET EU = 5 TO SEARCH THE WATER HEATING SECTION OF THE DATA
!*******************************************************************
      EU = 5

      DO 30 Y=RECSYear,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        DO 30 D=1,mNumCR-2
          DO 30 B=1,mNumBldg
            DO 30 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)
              IF (Y.EQ.RECSYear) THEN
                RSWATER(Y1,EQC)=RSWATER(Y1,EQC)+EQCESE(Y,RECCL,B,D)
              ELSE
                RSWATER(Y1,EQC)=RSWATER(Y1,EQC)  +EQCESE(Y,RECCL,B,D) &
                 +EQCRP90(Y,RECCL,B,D)+EQCADD(Y,RECCL,B,D) + &
                  EQCSR90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D) + &
                 EQCREP(Y,RECCL,B,D)  +EQCSUR(Y,RECCL,B,D)
              ENDIF
 30   CONTINUE

!*******************************************************************
!   WRITE OUT WATER HEATING MARKET SHARES	!kj - is this still needed? not shares; quantity of equipment
!*******************************************************************
      WRITE(9,*) 'rswater shares?'
      DO Y=RECSYear,LastYr+BaseYr-1
         Y1=Y-BaseYr+1
        WRITE(9,3773) Y,(RSWATER(Y1,E3),E3=1,5)  !Natural gas, electricity, distillate fuel oil, propane, solar
      ENDDO
 3773 FORMAT(4X,I4,5(1X,F14.3))

!*******************************************************************
!   AGGREGATE COOKING SYSTEMS
!   SET EU = 6 TO SEARCH THE COOKING SECTION OF THE DATA
!*******************************************************************
      EU = 6

      DO 40 Y=RECSYear,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        DO 40 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          RSCOOK(Y1,EQC)=0.0
          DO 40 B=1,mNumBldg
             DO 40 D=1,mNumCR-2
              IF (Y.EQ.RECSYear) THEN
                RSCOOK(Y1,EQC)=RSCOOK(Y1,EQC)+EQCESE(Y,RECCL,B,D)
              ELSE
                RSCOOK(Y1,EQC)=RSCOOK(Y1,EQC)+  EQCESE(Y,RECCL,B,D)+ &
                  EQCRP90(Y,RECCL,B,D)+EQCADD(Y,RECCL,B,D)+ &
                  EQCSR90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)+ &
                  EQCREP(Y,RECCL,B,D)  +EQCSUR(Y,RECCL,B,D)
              ENDIF
 40   CONTINUE

!*******************************************************************
!     AGGREGATE COOKING SYSTEMS FOR MARKET SHARE ANALYSIS
!*******************************************************************
      NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)

!*******************************************************************
!   WRITE OUT COOKING MARKET SHARES	!kj - is this still needed? not shares; quantity of equipment
!*******************************************************************
      WRITE(9,*) ' '
      WRITE(9,*) 'rscook'
      NUMEQC=RTCLEUPT(EU+1)-RTCLEUPT(EU)
      DO Y=RECSYear,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        WRITE(9,3117) Y,(RSCOOK(Y1,EQC),EQC=1,NUMEQC)
      ENDDO
 3117 FORMAT(4X,I7,3(1X,F12.1))

!*******************************************************************
!   AGGREGATE DRYING SYSTEMS
!   SET EU = 7 TO SEARCH THE CLOTHES DRYER SECTION OF THE DATA
!*******************************************************************
      EU = 7

      DO 50 Y=RECSYear,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        DO 50 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          RSDRY(Y1,EQC)=0.0
          DO 50 B=1,mNumBldg
             DO 50 D=1,mNumCR-2
               IF (Y.EQ.RECSYear) THEN
                 RSDRY(Y1,EQC)=RSDRY(Y1,EQC)+EQCESE(Y,RECCL,B,D)
               ELSE
                 RSDRY(Y1,EQC)=RSDRY(Y1,EQC)+  EQCESE(Y,RECCL,B,D) + &
                   EQCRP90(Y,RECCL,B,D)+EQCADD(Y,RECCL,B,D) + &
                   EQCSR90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)+ &
                   EQCREP(Y,RECCL,B,D) +EQCSUR(Y,RECCL,B,D)
               ENDIF
 50   CONTINUE

!*******************************************************************
!   WRITE OUT DRYER MARKET SHARES	!kj - is this still needed? not shares; quantity of equipment
!*******************************************************************
      NUMEQC=RTCLEUPT(EU+1)-RTCLEUPT(EU)
      WRITE(9,*) 'rsdry'
      DO Y=RECSYear,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        WRITE(9,4117) Y,(RSDRY(Y1,EQC),EQC=1,NUMEQC)
      ENDDO
 4117 FORMAT(4X,I7,2(1X,F12.2))

!*******************************************************************
!   AGGREGATE FOOD REFRIGERATION SYSTEMS
!   SET EU = 8 TO SEARCH THE FOOD REFRIG SECTION OF THE DATA
      EU = 8
!*******************************************************************
      DO 60 Y=RECSYear,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        DO 60 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          RSREF(Y1)=0.0
          DO 60 B=1,mNumBldg
            DO 60 D=1,mNumCR-2
              IF (Y.EQ.RECSYear) THEN
                RSREF(Y1)=RSREF(Y1)+EQCESE(Y,RECCL,B,D)
              ELSE
                RSREF(Y1)=RSREF(Y1)  + EQCESE(Y,RECCL,B,D)    + &
                EQCRP90(Y,RECCL,B,D) + EQCSR90(Y,RECCL,B,D)   + &
                EQCADD(Y,RECCL,B,D)  + EQCRP90RP(Y,RECCL,B,D) + &
                EQCREP(Y,RECCL,B,D)  + EQCSUR(Y,RECCL,B,D)
             ENDIF
60    CONTINUE

!*******************************************************************
!   WRITE OUT REFRIGERATOR MARKET SHARES	!kj - is this still needed? not shares; quantity of equipment
!*******************************************************************
      WRITE(9,*) 'Refrigerator Equipment Stock'
      DO y=RECSYear-BaseYr+1,LastYr
       WRITE(9,*) RSREF(Y)
      ENDDO

      WRITE(9,*) 'Refrigerator Equipment Shares'	!kj - is this still needed? shares not actually written; this is followed by weighted efficiencies

!*******************************************************************
!   AGGREGATE STANDALONE FREEZING SYSTEMS
!   SET EU = 9 TO SEARCH THE STANDALONE FREEZING SECTION OF THE DATA
      EU = 9
!*******************************************************************
      DO 65 Y=RECSYear,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        DO 65 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          RSFRZ(Y1)=0.0
          DO 65 B=1,mNumBldg
            DO 65 D=1,mNumCR-2
              IF (Y.EQ.RECSYear) THEN
                RSFRZ(Y1)=RSFRZ(Y1)+EQCESE(Y,RECCL,B,D)
              ELSE
                RSFRZ(Y1)=RSFRZ(Y1)+  EQCESE(Y,RECCL,B,D)+ &
                  EQCRP90(Y,RECCL,B,D) +  EQCSR90(Y,RECCL,B,D)+ &
                  EQCADD(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D) &
                +  EQCSUR(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
              ENDIF
 65   CONTINUE

!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED NEW HEATER EFFICIENCIES
!   SET EU = 1 TO SEARCH THE SPACE HEATING SECTION OF THE DATA
!*******************************************************************
      EU = 1

      DO 70 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
       DO 70 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        NUME(Y,EQC)=0.0
        DEN(Y,EQC)=0.0
        DO 70 D=1,mNumCR-2
         DO 70 B=1,mNumBldg
           IF( (WTEQCEFFN(Y,RECCL,B,D).GT.0.0).AND. &
               (WTEQCEFFR(Y,RECCL,B,D).GT.0.0).AND. &
                 (WTEQCEFFHV(Y,RECCL,B,D).GT.0.0)) THEN
             NUME(Y,EQC)=NUME(Y,EQC) +EQCADD(Y,RECCL,B,D)  &
              * (1/WTEQCEFFHV(Y,RECCL,B,D)) &
              + (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
              * (1/WTEQCEFFN(Y,RECCL,B,D)) &
              + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))
             NUME1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)  &
              * (1/WTEQCEFFHV(Y,RECCL,B,D)) &
              + (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
              * (1/WTEQCEFFN(Y,RECCL,B,D)) &
              + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))
             DEN(Y,EQC)=DEN(Y,EQC)+ EQCADD(Y,RECCL,B,D)  + &
              EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
             DEN1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)  + &
              EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
           ENDIF
 70   CONTINUE

!*******************************************************************
      DO 75 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
       DO 75 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        IF(RTCLNAME(RECCL).EQ.'ELEC_HP'.AND.DEN(Y,EQC).GT.0.0) THEN
           RSNEFHT(Y1,1)=NUME(Y,EQC)/DEN(Y,EQC)   ! ELEC_HP
        ELSEIF(RTCLNAME(RECCL).EQ.'NG_HP'.AND.DEN(Y,EQC).GT.0.0) &
          THEN
           RSNEFHT(Y1,2)=NUME(Y,EQC)/DEN(Y,EQC)   ! NG_HP
        ELSEIF(RTCLNAME(RECCL).EQ.'GEO_HP'.AND.DEN(Y,EQC).GT.0.0) &
          THEN
           RSNEFHT(Y1,3)=NUME(Y,EQC)/DEN(Y,EQC)   ! GEO_HP
        ELSEIF(RTCLNAME(RECCL).EQ.'NG_FA'.AND.DEN(Y,EQC).GT.0.0) &
          THEN
           RSNEFHT(Y1,4)=NUME(Y,EQC)/DEN(Y,EQC)   ! NG_FA
        ELSEIF(RTCLNAME(RECCL).EQ.'DIST_FA'.AND.DEN(Y,EQC).GT.0.0) &
          THEN
           RSNEFHT(Y1,5)=NUME(Y,EQC)/DEN(Y,EQC)   ! DIST_FA
        ENDIF
 75   CONTINUE
     DO 76 D=1,mNumCR-2
      DO 76 B=1,mNumBldg
       DO 76 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
       DO 76 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
          IF (DEN1(Y,EQC,B,D).GT.0.0) THEN
            IF (EQC.NE.2) THEN
           RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
            ELSE
           RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)*3.412
            ENDIF
          ENDIF
 76   CONTINUE

!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED NEW COOLER EFFICIENCIES
!   SET EU = 2 TO SEARCH THE SPACE COOLING SECTION OF THE DATA
!*******************************************************************
      EU = 2

      DO 80 Y=RECSYear+1,LastYr+BaseYr-1
        DO 80 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          E=RTCLEQCL(RECCL)
          NUME(Y,E)=0.0
          DEN(Y,E)=0.0
          DO 80 D=1,mNumCR-2
            DO 80 B=1,mNumBldg
            IF (E.NE.1) THEN
            IF((WTEQCEFFN(Y,RECCL,B,D).GT.0.0).AND. &
              (WTEQCEFFR(Y,RECCL,B,D).GT.0.0).AND. &
              (WTEQCEFFHV(Y,RECCL,B,D).GT.0.0)) THEN
            NUME(Y,E)=NUME(Y,E) + EQCADD(Y,RECCL,B,D)  * &
          (1/WTEQCEFFHV(Y,RECCL,B,D))  &
       + (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * (1/WTEQCEFFN(Y,RECCL,B,D)) &
       + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))
             DEN(Y,E)=DEN(Y,E)+ EQCADD(Y,RECCL,B,D)  + &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
             NUME1(Y,E,B,D)= EQCADD(Y,RECCL,B,D)  * &
          (1/WTEQCEFFHV(Y,RECCL,B,D))  &
       + (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * (1/WTEQCEFFN(Y,RECCL,B,D)) &
       + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))
             DEN1(Y,E,B,D)=EQCADD(Y,RECCL,B,D)  + &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
            ENDIF
            ELSE
             IF((WTEQCEFFN(Y,RECCL,B,D).GT.0.0).AND. &
              (WTEQCEFFR(Y,RECCL,B,D).GT.0.0)) THEN
            NUME(Y,E)=NUME(Y,E) +EQCADD(Y,RECCL,B,D)* &
          (1/WTEQCEFFN(Y,RECCL,B,D)) &
       + (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * (1/WTEQCEFFN(Y,RECCL,B,D)) &
       + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))
             DEN(Y,E)=DEN(Y,E)+ EQCADD(Y,RECCL,B,D)+ &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
             NUME1(Y,E,B,D)=(EQCADD(Y,RECCL,B,D)+&
       +  EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * (1/WTEQCEFFN(Y,RECCL,B,D)) &
       + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))
             DEN1(Y,E,B,D)=EQCADD(Y,RECCL,B,D)+ &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
            ENDIF
           ENDIF
 80   CONTINUE

!*******************************************************************
      DO 85 Y=RECSYear+1,LastYr+BaseYr-1
        Y1=Y-BaseYr+1
        DO 85 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          IF(RTCLNAME(RECCL).EQ.'ELEC_HP'.AND.DEN(Y,EQC).GT.0.0) THEN
            RSNEFCL(Y1,1)=NUME(Y,EQC)/DEN(Y,EQC)   ! ELEC_HP
          ELSEIF(RTCLNAME(RECCL).EQ.'NG_HP'.AND.DEN(Y,EQC).GT.0.0) THEN
            RSNEFCL(Y1,2)=NUME(Y,EQC)/DEN(Y,EQC)   ! NG_HP
          ELSEIF(RTCLNAME(RECCL).EQ.'GEO_HP'.AND.DEN(Y,EQC).GT.0.0) THEN
            RSNEFCL(Y1,3)=(NUME(Y,EQC)/DEN(Y,EQC))/3.412   ! GEO_HP
          ELSEIF(RTCLNAME(RECCL).EQ.'CENT_AIR'.AND.DEN(Y,EQC).GT.0.0) THEN
            RSNEFCL(Y1,4)=NUME(Y,EQC)/DEN(Y,EQC)   ! CENT_AIR
          ELSEIF(RTCLNAME(RECCL).EQ.'ROOM_AIR'.AND.DEN(Y,EQC).GT.0.0) THEN
            RSNEFCL(Y1,5)=NUME(Y,EQC)/DEN(Y,EQC)   ! ROOM_AIR
          ENDIF
      85  CONTINUE

      DO 86 D=1,mNumCR-2
        DO 86 B=1,mNumBldg
          DO 86 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
            DO 86 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)
              IF(DEN1(Y,EQC,B,D).GT.0.0) THEN
                IF (EQC.LE.3) THEN
                  RSNEFDB1(Y1,RECCL,B,D)=(NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D))*3.412
                ELSE
                  RSNEFDB1(Y1,RECCL,B,D)=(NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D))
                ENDIF
              ENDIF
      86  CONTINUE

!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED NEW CLOTHESWASHER EFFICIENCIES
!   SET EU = 3 TO SEARCH THE CLOTHES WASHER SECTION OF THE DATA
!*******************************************************************
      EU = 3

      DO 87 Y=RECSYear+1,LastYr+BaseYr-1
        DO 87 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          DO 87 D=1,mNumCR-2
            DO 87 B=1,mNumBldg
             NUME1(Y,EQC,B,D)=((EQCADD(Y,RECCL,B,D) &
       + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * (WTEQCEFFN(Y,RECCL,B,D)) &
       + EQCRP90(Y,RECCL,B,D)*(WTEQCEFFR(Y,RECCL,B,D)))
             DEN1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)+ &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
 87   CONTINUE

!*******************************************************************
      DO 88 D=1,mNumCR-2
       DO 88 B=1,mNumBldg
        DO 88 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
       DO 88 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
           IF(DEN1(Y,EQC,B,D).GT.0.0) RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
 88   CONTINUE

!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED NEW CLOTHESWASHER EFFICIENCIES
!   SET EU = 4 TO SEARCH THE DISHWASHER SECTION OF THE DATA
!*******************************************************************
      EU = 4

      DO 89 Y=RECSYear+1,LastYr+BaseYr-1
        DO 89 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          DO 89 D=1,mNumCR-2
            DO 89 B=1,mNumBldg
           IF((WTEQCEFFN(Y,RECCL,B,D).GT.0.0).AND. &
              (WTEQCEFFR(Y,RECCL,B,D).GT.0.0)) THEN
             NUME1(Y,EQC,B,D)=((EQCADD(Y,RECCL,B,D) &
       + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * (1/WTEQCEFFN(Y,RECCL,B,D)) &
       + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D)))
             DEN1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)+ &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
              ENDIF
 89   CONTINUE

!*******************************************************************
      DO 90 D=1,mNumCR-2
       DO 90 B=1,mNumBldg
        DO 90 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
       DO 90 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        IF(DEN1(Y,EQC,B,D).GT.0.0) RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
 90   CONTINUE

!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED NEW H2O EFFICIENCIES
!   SET EU = 5 TO SEARCH THE WATER HEATING SECTION OF THE DATA
!*******************************************************************
      EU = 5

      DO 91 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        DO 91 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          NUME(Y,EQC)=0.0
          DEN(Y,EQC)=0.0
          DO 91 D=1,mNumCR-2
            DO 91 B=1,mNumBldg
           IF((WTEQCEFFN(Y,RECCL,B,D).GT.0.0).AND. &
              (WTEQCEFFR(Y,RECCL,B,D).GT.0.0)) THEN
             NUME(Y,EQC)=NUME(Y,EQC)+((EQCADD(Y,RECCL,B,D) &
       + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * (1/WTEQCEFFN(Y,RECCL,B,D)) &
       + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D)))
             DEN(Y,EQC)=DEN(Y,EQC)+EQCADD(Y,RECCL,B,D)+ &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
             NUME1(Y,EQC,B,D)=((EQCADD(Y,RECCL,B,D) &
       + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * (1/WTEQCEFFN(Y,RECCL,B,D)) &
       + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D)))
             DEN1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)+ &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
              ENDIF
 91   CONTINUE

!*******************************************************************
      DO 92 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
       DO 92 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        IF(RTCLNAME(RECCL).EQ.'ELEC_WH'.AND.DEN(Y,EQC).GT.0.0) THEN
           RSNEFHW(Y1,1)=NUME(Y,EQC)/DEN(Y,EQC)   ! ELEC_WH
        ELSEIF(RTCLNAME(RECCL).EQ.'NG_WH'.AND.DEN(Y,EQC).GT.0.0) &
          THEN
           RSNEFHW(Y1,2)=NUME(Y,EQC)/DEN(Y,EQC)   ! NG_WH
        ELSEIF(RTCLNAME(RECCL).EQ.'DIST_WH'.AND.DEN(Y,EQC).GT.0.0) &
          THEN
           RSNEFHW(Y1,3)=NUME(Y,EQC)/DEN(Y,EQC)   ! DIST_WH
        ELSEIF(RTCLNAME(RECCL).EQ.'LPG_WH'.AND.DEN(Y,EQC).GT.0.0) &
          THEN
           RSNEFHW(Y1,4)=NUME(Y,EQC)/DEN(Y,EQC)   ! LPG_WH
        ENDIF
 92   CONTINUE
     DO 93 D=1,mNumCR-2
      DO 93 B=1,mNumBldg
       DO 93 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        DO 93 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
         EQC=RTCLEQCL(RECCL)
          IF(DEN1(Y,EQC,B,D).GT.0.0) RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
 93   CONTINUE

!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED NEW COOKING EFFICIENCIES
!   SET EU = 6 TO SEARCH THE COOKING SECTION OF THE DATA
!*******************************************************************
      EU = 6

      DO 94 Y=RECSYear+1,LastYr+BaseYr-1
        DO 94 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          DO 94 D=1,mNumCR-2
            DO 94 B=1,mNumBldg
           IF((WTEQCEFFN(Y,RECCL,B,D).GT.0.0).AND. &
              (WTEQCEFFR(Y,RECCL,B,D).GT.0.0)) THEN
             NUME1(Y,EQC,B,D)=((EQCADD(Y,RECCL,B,D) &
       + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * (WTEQCEFFN(Y,RECCL,B,D)) &
       + EQCRP90(Y,RECCL,B,D)*(WTEQCEFFR(Y,RECCL,B,D)))
             DEN1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)+ &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
              ENDIF
 94   CONTINUE

!*******************************************************************
     DO 95 D=1,mNumCR-2
      DO 95 B=1,mNumBldg
       DO 95 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        DO 95 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
         EQC=RTCLEQCL(RECCL)
          IF(DEN1(Y,EQC,B,D).GT.0.0)  RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
 95   CONTINUE

!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED NEW CLOTHES DRYER EFFICIENCIES
!   SET EU = 7 TO SEARCH THE CLOTHES DRYER SECTION OF THE DATA
!*******************************************************************
      EU = 7

      DO 196 Y=RECSYear+1,LastYr+BaseYr-1
        DO 196 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          DO 196 D=1,mNumCR-2
            DO 196 B=1,mNumBldg
           IF((WTEQCEFFN(Y,RECCL,B,D).GT.0.0).AND. &
              (WTEQCEFFR(Y,RECCL,B,D).GT.0.0)) THEN
             NUME1(Y,EQC,B,D)=((EQCADD(Y,RECCL,B,D) &
       + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * (1/WTEQCEFFN(Y,RECCL,B,D)) &
       + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D)))
             DEN1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)+ &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
              ENDIF
 196   CONTINUE

!*******************************************************************
     DO 97 D=1,mNumCR-2
      DO 97 B=1,mNumBldg
       DO 97 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        DO 97 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
         EQC=RTCLEQCL(RECCL)
           IF(DEN1(Y,EQC,B,D).GT.0.0) RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
 97   CONTINUE

!********************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED NEW REFRIGERATOR EFFICIENCIES
!   SET EU = 8 TO SEARCH THE FOOD REFRIGERATION SECTION OF THE DATA
!********************************************************************
      EU = 8

      DO 99 Y=RECSYear+1,LastYr+BaseYr-1
        DO 99 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          NUME(Y,EQC)=0.0
          DEN(Y,EQC)=0.0
          DO 99 D=1,mNumCR-2
            DO 99 B=1,mNumBldg
             NUME(Y,EQC)=NUME(Y,EQC)+((EQCADD(Y,RECCL,B,D) &
       + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * WTEQCEFFN(Y,RECCL,B,D) &
       + EQCRP90(Y,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))
             DEN(Y,EQC)=DEN(Y,EQC)+EQCADD(Y,RECCL,B,D)+ &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
             NUME1(Y,EQC,B,D)=((EQCADD(Y,RECCL,B,D) &
       + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * WTEQCEFFN(Y,RECCL,B,D) &
       + EQCRP90(Y,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))
             DEN1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)+ &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
 99  CONTINUE

!*******************************************************************
      DO 100 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        IF (DEN(Y,1).GT.0.0) RSNEFRF(Y1)=NUME(Y,1)/DEN(Y,1)   !REF
 100   CONTINUE
          DO 101 D=1,mNumCR-2
            DO 101 B=1,mNumBldg
             DO 101 Y=RECSYear+1,LastYr+BaseYr-1
              DO 101 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)
             Y1=Y-BaseYr+1
        IF (DEN1(Y,1,B,D).GT.0.0) RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,1,B,D)/DEN1(Y,1,B,D)   !REF
 101   CONTINUE

!********************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED NEW STANDALONE FREEZER EFFICIENCIES
!   SET EU = 9 TO SEARCH THE STANDALONE FREEZER SECTION OF THE DATA
!********************************************************************
      EU = 9
      DO 102 Y=RECSYear+1,LastYr+BaseYr-1
        DO 102 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          NUME(Y,EQC)=0.0
          DEN(Y,EQC)=0.0
          DO 102 D=1,mNumCR-2
            DO 102 B=1,mNumBldg
             NUME(Y,EQC)=NUME(Y,EQC)+((EQCADD(Y,RECCL,B,D) &
       + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * WTEQCEFFN(Y,RECCL,B,D) &
       + EQCRP90(Y,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))
             DEN(Y,EQC)=DEN(Y,EQC)+EQCADD(Y,RECCL,B,D)+ &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
             NUME1(Y,EQC,B,D)=((EQCADD(Y,RECCL,B,D) &
       + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * WTEQCEFFN(Y,RECCL,B,D) &
       + EQCRP90(Y,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))
             DEN1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)+ &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
 102  CONTINUE

!*******************************************************************
      DO Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        IF (DEN(Y,1).GT.0.0) RSNEFFZ(Y1)=NUME(Y,1)/DEN(Y,1)   !FRZ
      ENDDO
          DO 103 D=1,mNumCR-2
            DO 103 B=1,mNumBldg
             DO 103 Y=RECSYear+1,LastYr+BaseYr-1
              DO 103 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)
              Y1=Y-BaseYr+1
        IF (DEN1(Y,1,B,D).GT.0.0) RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,1,B,D)/DEN1(Y,1,B,D)   !FRZ
103      CONTINUE
!*******************************************************************
!   !   WRITE OUT NEW WEIGHTED EFFICIENCIES	!kj - is this still needed? not shares; quantity of equipment
!*******************************************************************
       WRITE(9,*) 'rsnefht'
       DO 6666 Y=RECSYear+1,LastYr+BaseYr-1
        Y1=Y-BaseYr+1
        WRITE(9,6667) Y1+(BaseYr-1),(RSNEFHT(Y1,E3),E3=1,5)
 6667   FORMAT(4X,I4,5(1X,F6.2))
 6666   CONTINUE
       WRITE(9,*) 'rsnefcl'
       DO 6766 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
      WRITE(9,6667) Y1+(BaseYr-1),(RSNEFCL(Y1,E3)*3.412,E3=1,5)
 6766   CONTINUE
       WRITE(9,*) 'rsnefhw'
       DO 6866 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
      WRITE(9,6668) Y1+(BaseYr-1),(RSNEFHW(Y1,E3),E3=1,4)
 6668   FORMAT(4X,I4,4(1X,F7.4))
 6866   CONTINUE
       WRITE(9,*) 'rsnefrf'
       DO 6966 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
      WRITE(9,6669) Y1+(BaseYr-1), RSNEFRF(Y1)
 6669   FORMAT(4X,I4,1X,F7.2)
 6966   CONTINUE
       WRITE(9,*) 'rsneffz'
       DO 6777 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
      WRITE(9,6669) Y1+(BaseYr-1), RSNEFFZ(Y1)
 6777   CONTINUE

!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED EXISTING HEATER EFFICIENCIES
!   SET EU = 1 TO SEARCH THE SPACE HEATING SECTION OF THE DATA
!*******************************************************************
      EU = 1

      DO 110 Y=RECSYear+1,LastYr+BaseYr-1
       DO 110 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
          NUME(Y,EQC)=0.0
          DEN(Y,EQC)=0.0
          DO 110 D=1,mNumCR-2
            DO 110 B=1,mNumBldg
               NUME(Y,EQC)=NUME(Y,EQC)+ &
              (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL) + &
              (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D))* &
               (1/WTEQCEFFN(Y,RECCL,B,D)) + &
               EQCADD(Y,RECCL,B,D)*(1/WTEQCEFFHV(Y,RECCL,B,D))+ &
              EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D)) + &
             (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
              (1/WTEQCEFFA(Y,RECCL,B,D)) )

              DEN(Y,EQC)=DEN(Y,EQC)+ ( EQCESE(Y,RECCL,B,D)+ &
              EQCRP90RP(Y,RECCL,B,D)+ EQCRP90(Y,RECCL,B,D) + &
               EQCREP(Y,RECCL,B,D) + EQCADD(Y,RECCL,B,D)+&
               EQCSUR(Y,RECCL,B,D)+EQCSR90(Y,RECCL,B,D) )
               NUME1(Y,EQC,B,D)= &
              (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL) + &
              (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D))* &
               (1/WTEQCEFFN(Y,RECCL,B,D)) + &
               EQCADD(Y,RECCL,B,D)*(1/WTEQCEFFHV(Y,RECCL,B,D))+ &
              EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D)) + &
             (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
              (1/WTEQCEFFA(Y,RECCL,B,D)) )

              DEN1(Y,EQC,B,D)= ( EQCESE(Y,RECCL,B,D)+ &
              EQCRP90RP(Y,RECCL,B,D)+ EQCRP90(Y,RECCL,B,D) + &
               EQCREP(Y,RECCL,B,D) + EQCADD(Y,RECCL,B,D)+&
               EQCSUR(Y,RECCL,B,D)+EQCSR90(Y,RECCL,B,D) )
 110  CONTINUE

!*******************************************************************
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        IF(RTCLNAME(RECCL).EQ.'ELEC_HP') THEN
          RSEEFHT(RECSYear-BaseYr+1,1)=RTBASEFF(RECSYear,RECCL)  !ELEC_HP
        ELSEIF(RTCLNAME(RECCL).EQ.'NG_HP') THEN
          RSEEFHT(RECSYear-BaseYr+1,2)=RTBASEFF(RECSYear,RECCL)  !NG_HP
        ELSEIF(RTCLNAME(RECCL).EQ.'GEO_HP') THEN
          RSEEFHT(RECSYear-BaseYr+1,3)=RTBASEFF(RECSYear,RECCL)  !GEO_HP
        ELSEIF(RTCLNAME(RECCL).EQ.'NG_FA') THEN
          RSEEFHT(RECSYear-BaseYr+1,4)=RTBASEFF(RECSYear,RECCL)  !NG_FA
        ELSEIF(RTCLNAME(RECCL).EQ.'DIST_FA') THEN
          RSEEFHT(RECSYear-BaseYr+1,5)=RTBASEFF(RECSYear,RECCL)  !DIST_FA
        ENDIF
       DO D=1,mNumCR-2
        DO B=1,mNumBldg
         RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)=RTBASEFF(RECSYear,RECCL)
        ENDDO
       ENDDO
      ENDDO

      DO 120 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        DO 120 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          IF(RTCLNAME(RECCL).EQ.'ELEC_HP') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFHT(Y1,1)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFHT(Y1,1)=RSEEFHT(RECSYear-BaseYr+1,1)
            ENDIF
          ELSEIF(RTCLNAME(RECCL).EQ.'NG_HP') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFHT(Y1,2)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFHT(Y1,2)=RSEEFHT(RECSYear-BaseYr+1,2)
            ENDIF
          ELSEIF(RTCLNAME(RECCL).EQ.'GEO_HP') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFHT(Y1,3)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFHT(Y1,3)=RSEEFHT(RECSYear-BaseYr+1,3)
            ENDIF
          ELSEIF(RTCLNAME(RECCL).EQ.'NG_FA') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFHT(Y1,4)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFHT(Y1,4)=RSEEFHT(RECSYear-BaseYr+1,4)
            ENDIF
          ELSEIF(RTCLNAME(RECCL).EQ.'DIST_FA') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFHT(Y1,5)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFHT(Y1,5)=RSEEFHT(RECSYear-BaseYr+1,5)
            ENDIF
          ENDIF
 120  CONTINUE
    DO 121 D=1,mNumCR-2
     DO 121 B=1,mNumBldg
      DO 121 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        DO 121 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
            IF (DEN1(Y,EQC,B,D).GT.0.0) THEN
              IF (EQC.NE.2) THEN
               RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
              ELSE
               RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)*3.412  !Multiply by 3.412 to convert ELEC_HP from COP to HSPF
              ENDIF
            ELSE
              IF (EQC.NE.2) THEN
               RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)
              ELSE
               RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)*3.412  !Multiply by 3.412 to convert ELEC_HP from COP to HSPF
              ENDIF
            ENDIF
 121  CONTINUE

!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED EXISTING SPACE COOLING EFFICIENCIES
!   SET EU = 2 TO SEARCH THE SPACE COOLING SECTION OF THE DATA
!*******************************************************************
      EU = 2

      DO 130 Y=RECSYear+1,LastYr+BaseYr-1
        DO 130 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          E=RTCLEQCL(RECCL)
          NUME(Y,E)=0.0
          DEN(Y,E)=0.0
          DO 130 D=1,mNumCR-2
            DO 130 B=1,mNumBldg
             IF (E.EQ.1) THEN
               NUME(Y,E)=NUME(Y,E)+  &
              (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL) + &
               EQCADD(Y,RECCL,B,D)*(1/WTEQCEFFN(Y,RECCL,B,D))+ &
              (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D))* &
               (1/WTEQCEFFN(Y,RECCL,B,D))+ &
               EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))+ &
              (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
              (1/WTEQCEFFA(Y,RECCL,B,D)))
               NUME1(Y,E,B,D)=  &
              (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL) + &
               EQCADD(Y,RECCL,B,D)*(1/WTEQCEFFN(Y,RECCL,B,D))+ &
              (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D))* &
               (1/WTEQCEFFN(Y,RECCL,B,D))+ &
               EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))+ &
              (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
              (1/WTEQCEFFA(Y,RECCL,B,D)))
             ELSE
               NUME(Y,E)=NUME(Y,E)+  &
              (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL) + &
               EQCADD(Y,RECCL,B,D)*(1/WTEQCEFFHV(Y,RECCL,B,D)) + &
              (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D))* &
               (1/WTEQCEFFN(Y,RECCL,B,D))+ &
               EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D)) + &
              (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
              (1/WTEQCEFFA(Y,RECCL,B,D)))
               NUME1(Y,E,B,D)=  &
              (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL) + &
               EQCADD(Y,RECCL,B,D)*(1/WTEQCEFFHV(Y,RECCL,B,D)) + &
              (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D))* &
               (1/WTEQCEFFN(Y,RECCL,B,D))+ &
               EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D)) + &
              (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
              (1/WTEQCEFFA(Y,RECCL,B,D)))
             ENDIF

              DEN(Y,E)=DEN(Y,E)+  EQCESE(Y,RECCL,B,D)  + &
                EQCRP90(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D) + &
                EQCADD(Y,RECCL,B,D) +EQCSR90(Y,RECCL,B,D)+ &
                EQCRP90RP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D)
              DEN1(Y,E,B,D)=  EQCESE(Y,RECCL,B,D)  + &
                EQCRP90(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D) + &
                EQCADD(Y,RECCL,B,D) +EQCSR90(Y,RECCL,B,D)+ &
                EQCRP90RP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D)
 130  CONTINUE

!*******************************************************************
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        IF(RTCLNAME(RECCL).EQ.'ELEC_HP') THEN
          RSEEFCL(RECSYear-BaseYr+1,1)=RTBASEFF(RECSYear,RECCL)  !ELEC_HP
        ELSEIF(RTCLNAME(RECCL).EQ.'NG_HP') THEN
          RSEEFCL(RECSYear-BaseYr+1,2)=RTBASEFF(RECSYear,RECCL)  !NG_HP
        ELSEIF(RTCLNAME(RECCL).EQ.'GEO_HP') THEN
          RSEEFCL(RECSYear-BaseYr+1,3)=RTBASEFF(RECSYear,RECCL)  !GEO_HP
        ELSEIF(RTCLNAME(RECCL).EQ.'CENT_AIR') THEN
          RSEEFCL(RECSYear-BaseYr+1,4)=RTBASEFF(RECSYear,RECCL)  !CENT_AIR
        ELSEIF(RTCLNAME(RECCL).EQ.'ROOM_AIR') THEN
          RSEEFCL(RECSYear-BaseYr+1,5)=RTBASEFF(RECSYear,RECCL)  !ROOM_AIR
        ENDIF
       DO D=1,mNumCR-2
        DO B=1,mNumBldg
         RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)=RTBASEFF(RECSYear,RECCL)
        ENDDO
       ENDDO
     ENDDO

      DO 140 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        DO 140 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          IF(RTCLNAME(RECCL).EQ.'ELEC_HP') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFCL(Y1,1)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFCL(Y1,1)=RSEEFCL(RECSYear-BaseYr+1,1)
            ENDIF
          ELSEIF(RTCLNAME(RECCL).EQ.'NG_HP') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFCL(Y1,2)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFCL(Y1,2)=RSEEFCL(RECSYear-BaseYr+1,2)
            ENDIF
          ELSEIF(RTCLNAME(RECCL).EQ.'GEO_HP') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFCL(Y1,3)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFCL(Y1,3)=RSEEFCL(RECSYear-BaseYr+1,3)
            ENDIF
          ELSEIF(RTCLNAME(RECCL).EQ.'CENT_AIR') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFCL(Y1,4)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFCL(Y1,4)=RSEEFCL(RECSYear-BaseYr+1,4)
            ENDIF
          ELSEIF(RTCLNAME(RECCL).EQ.'ROOM_AIR') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFCL(Y1,5)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFCL(Y1,5)=RSEEFCL(RECSYear-BaseYr+1,5)
            ENDIF
          ENDIF
 140  CONTINUE
      DO 141 D=1,mNumCR-2
      DO 141 B=1,mNumBldg
      DO 141 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        DO 141 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
            IF (DEN1(Y,EQC,B,D).GT.0.0) THEN
               IF (EQC.LE.3) THEN
                RSEEFDB1(Y1,RECCL,B,D)=(NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D))*3.412  !Multiply by 3.412 to convert ROOM_AIR from COP to EER/ CENT_AIR and ELEC_HP from COP to SEER
               ELSE
                RSEEFDB1(Y1,RECCL,B,D)=(NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D))
               ENDIF
            ELSE
               IF (EQC.LE.3) THEN
                RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)*3.412  !Multiply by 3.412 to convert ROOM_AIR from COP to EER/ CENT_AIR and ELEC_HP from COP to SEER
               ELSE
                RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)
               ENDIF
            ENDIF
 141  CONTINUE

!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED EXISTING CLOTHES WASHER
!   SET EU = 3 TO SEARCH THE CLOTHES WASHER SECTION OF THE DATA
!*******************************************************************
      EU = 3

      DO 142 D=1,mNumCR-2
      DO 142 B=1,mNumBldg
      DO 142 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)=RTBASEFF(RECSYear,RECCL)
 142     CONTINUE

       DO 143 Y=RECSYear+1,LastYr+BaseYr-1
             Y1=Y-BaseYr+1
       DO 143 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          DO 143 D=1,mNumCR-2
            DO 143 B=1,mNumBldg
              NUME1(Y,EQC,B,D)= &
               (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL)+ &
               (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D))*(WTEQCEFFN(Y,RECCL,B,D)) + &
                EQCRP90(Y,RECCL,B,D)*(WTEQCEFFR(Y,RECCL,B,D))+ &
               (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
               (WTEQCEFFA(Y,RECCL,B,D)))

              DEN1(Y,EQC,B,D)=(EQCRP90RP(Y,RECCL,B,D)+ &
                EQCESE(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+ &
                EQCSR90(Y,RECCL,B,D)+EQCADD(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))
 143  CONTINUE

!*******************************************************************
       DO 144 D=1,mNumCR-2
       DO 144 B=1,mNumBldg
       DO 144 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
       DO 144 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
        IF (DEN1(Y,1,B,D).GT.0.0) THEN
           RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,1,B,D)/DEN1(Y,1,B,D)
        ELSE
           RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)
        ENDIF
 144  CONTINUE

!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED EXISTING DISHWASHER
!   SET EU = 4 TO SEARCH THE DISHWASHER SECTION OF THE DATA
!*******************************************************************
      EU = 4

      DO 145 D=1,mNumCR-2
      DO 145 B=1,mNumBldg
      DO 145 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)=RTBASEFF(RECSYear,RECCL)
 145     CONTINUE

       DO 146 Y=RECSYear+1,LastYr+BaseYr-1
             Y1=Y-BaseYr+1
       DO 146 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          DO 146 D=1,mNumCR-2
            DO 146 B=1,mNumBldg
              NUME1(Y,EQC,B,D)= &
               (EQCESE(Y,RECCL,B,D)*(RTBASEFF(Y,RECCL))+ &
               (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D))*(1/WTEQCEFFN(Y,RECCL,B,D)) + &
                EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))+ &
               (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
               (1/WTEQCEFFA(Y,RECCL,B,D)))

              DEN1(Y,EQC,B,D)=(EQCRP90RP(Y,RECCL,B,D)+ &
                EQCESE(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+ &
                EQCSR90(Y,RECCL,B,D)+EQCADD(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))
 146  CONTINUE

!*******************************************************************
       DO 147 D=1,mNumCR-2
       DO 147 B=1,mNumBldg
       DO 147 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
       DO 147 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
        IF (DEN1(Y,1,B,D).GT.0.0) THEN
           RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,1,B,D)/DEN1(Y,1,B,D)
        ELSE
           RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)
        ENDIF
 147  CONTINUE

!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED EXISTING WATER HEATER EFFIC
!   SET EU = 5 TO SEARCH THE WATER HEATING SECTION OF THE DATA
!*******************************************************************
      EU = 5

      DO 150 Y=RECSYear+1,LastYr+BaseYr-1
        DO 150 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          E=RTCLEQCL(RECCL)
          NUME(Y,E)=0.0
          DEN(Y,E)=0.0
          DO 150 D=1,mNumCR-2
            DO 150 B=1,mNumBldg
               NUME(Y,E)=NUME(Y,E)+ &
               (EQCESE(Y,RECCL,B,D)*(RTBASEFF(Y,RECCL)) + &
               (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D))*(1/WTEQCEFFN(Y,RECCL,B,D)) + &
                EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))+ &
               (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
               (1/WTEQCEFFA(Y,RECCL,B,D)))

              DEN(Y,E)=DEN(Y,E)+(EQCESE(Y,RECCL,B,D)+ &
                EQCRP90(Y,RECCL,B,D)+EQCSR90(Y,RECCL,B,D)+ &
                EQCADD(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)+ &
                EQCRP90RP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))

               NUME1(Y,E,B,D)= &
               (EQCESE(Y,RECCL,B,D)*(RTBASEFF(Y,RECCL)) + &
               (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D))*(1/WTEQCEFFN(Y,RECCL,B,D)) + &
                EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))+ &
               (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
               (1/WTEQCEFFA(Y,RECCL,B,D)))

              DEN1(Y,E,B,D)=(EQCESE(Y,RECCL,B,D)+ &
                EQCRP90(Y,RECCL,B,D)+EQCSR90(Y,RECCL,B,D)+ &
                EQCADD(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)+ &
                EQCRP90RP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))
 150  CONTINUE

!*******************************************************************
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        IF(RTCLNAME(RECCL).EQ.'ELEC_WH') THEN
          RSEEFHW(RECSYear-BaseYr+1,1)=RTBASEFF(RECSYear,RECCL)  !ELEC_WH
        ELSEIF(RTCLNAME(RECCL).EQ.'NG_WH') THEN
          RSEEFHW(RECSYear-BaseYr+1,2)=RTBASEFF(RECSYear,RECCL)  !NG_WH
        ELSEIF(RTCLNAME(RECCL).EQ.'DIST_WH') THEN
          RSEEFHW(RECSYear-BaseYr+1,3)=RTBASEFF(RECSYear,RECCL)  !DIST_WH
        ELSEIF(RTCLNAME(RECCL).EQ.'LPG_WH') THEN
          RSEEFHW(RECSYear-BaseYr+1,4)=RTBASEFF(RECSYear,RECCL)  !LPG_WH
        ENDIF
       DO D=1,mNumCR-2
        DO B=1,mNumBldg
          RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)=RTBASEFF(RECSYear,RECCL)
        ENDDO
       ENDDO
      ENDDO

      DO 160 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        DO 160 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          IF(RTCLNAME(RECCL).EQ.'ELEC_WH') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFHW(Y1,1)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFHW(Y1,1)=RSEEFHW(RECSYear-BaseYr+1,1)          !ELEC_WH
            ENDIF
          ELSEIF(RTCLNAME(RECCL).EQ.'NG_WH') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFHW(Y1,2)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFHW(Y1,2)=RSEEFHW(RECSYear-BaseYr+1,2)          !NG_WH
            ENDIF
          ELSEIF(RTCLNAME(RECCL).EQ.'DIST_WH') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFHW(Y1,3)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFHW(Y1,3)=RSEEFHW(RECSYear-BaseYr+1,3)          !DIST_WH
            ENDIF
          ELSEIF(RTCLNAME(RECCL).EQ.'LPG_WH') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFHW(Y1,4)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFHW(Y1,4)=RSEEFHW(RECSYear-BaseYr+1,4)          !LPG_WH
            ENDIF
          ENDIF
 160  CONTINUE

      DO 161 D=1,mNumCR-2
       DO 161 B=1,mNumBldg
       DO 161 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        DO 161 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
            IF (DEN1(Y,EQC,B,D).GT.0.0) THEN
              RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
            ELSE
              RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)
            ENDIF
 161  CONTINUE

!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED EXISTING COOKING EFFIC
!   SET EU = 6 TO SEARCH THE COOKING SECTION OF THE DATA
!*******************************************************************
      EU = 6

      DO 162 Y=RECSYear+1,LastYr+BaseYr-1
        DO 162 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          E=RTCLEQCL(RECCL)
          DO 162 D=1,mNumCR-2
            DO 162 B=1,mNumBldg
               NUME1(Y,E,B,D)= &
               (EQCESE(Y,RECCL,B,D)*(RTBASEFF(Y,RECCL)) + &
               (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D))*(WTEQCEFFN(Y,RECCL,B,D)) + &
                EQCRP90(Y,RECCL,B,D)*(WTEQCEFFR(Y,RECCL,B,D))+ &
               (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
               (WTEQCEFFA(Y,RECCL,B,D)))

              DEN1(Y,E,B,D)=(EQCESE(Y,RECCL,B,D)+ &
                EQCRP90(Y,RECCL,B,D)+EQCSR90(Y,RECCL,B,D)+ &
                EQCADD(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)+ &
                EQCRP90RP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))
 162  CONTINUE

!*******************************************************************
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
       DO D=1,mNumCR-2
        DO B=1,mNumBldg
          RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)=RTBASEFF(RECSYear,RECCL)
        ENDDO
       ENDDO
      ENDDO

      DO 163 D=1,mNumCR-2
       DO 163 B=1,mNumBldg
       DO 163 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        DO 163 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
            IF (DEN1(Y,EQC,B,D).GT.0.0) THEN
              RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
            ELSE
              RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)
            ENDIF
 163  CONTINUE

!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED EXISTING CLOTHES DRYERS
!   SET EU = 7 TO SEARCH THE CLOTHES DRYER SECTION OF THE DATA
!*******************************************************************
      EU = 7

      DO 164 Y=RECSYear+1,LastYr+BaseYr-1
        DO 164 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          E=RTCLEQCL(RECCL)
          DO 164 D=1,mNumCR-2
            DO 164 B=1,mNumBldg
               NUME1(Y,E,B,D)= &
               (EQCESE(Y,RECCL,B,D)*(RTBASEFF(Y,RECCL)) + &
               (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D))*(1/WTEQCEFFN(Y,RECCL,B,D)) + &
                EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))+ &
               (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
               (1/WTEQCEFFA(Y,RECCL,B,D)))

              DEN1(Y,E,B,D)=(EQCESE(Y,RECCL,B,D)+ &
                EQCRP90(Y,RECCL,B,D)+EQCSR90(Y,RECCL,B,D)+ &
                EQCADD(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)+ &
                EQCRP90RP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))
 164  CONTINUE

!*******************************************************************
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
       DO D=1,mNumCR-2
        DO B=1,mNumBldg
          RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)=RTBASEFF(RECSYear,RECCL)
        ENDDO
       ENDDO
      ENDDO

      DO 165 D=1,mNumCR-2
       DO 165 B=1,mNumBldg
       DO 165 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        DO 165 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
            IF (DEN1(Y,EQC,B,D).GT.0.0) THEN
              RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
            ELSE
              RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)
            ENDIF
 165  CONTINUE

!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED EXISTING REFRIGERATORS
!   SET EU = 8 TO SEARCH THE FOOD REFRIGERATION SECTION OF THE DATA
!*******************************************************************
      EU = 8

      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RSEEFRF(RECSYear-BaseYr+1)=RTBASEFF(RECSYear,RECCL)
       DO D=1,mNumCR-2
        DO B=1,mNumBldg
          RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)=RTBASEFF(RECSYear,RECCL)
        ENDDO
       ENDDO
      ENDDO

      DO 170 Y=RECSYear+1,LastYr+BaseYr-1
             Y1=Y-BaseYr+1
       DO 170 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
         NUME(Y,EQC)=0.0
          DEN(Y,EQC)=0.0
          DO 170 D=1,mNumCR-2
            DO 170 B=1,mNumBldg
              NUME(Y,EQC)=NUME(Y,EQC)+ &
               (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL)+ &
               (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D))*(WTEQCEFFN(Y,RECCL,B,D)) + &
                EQCRP90(Y,RECCL,B,D)*(WTEQCEFFR(Y,RECCL,B,D))+ &
               (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
               (WTEQCEFFA(Y,RECCL,B,D)))

              DEN(Y,EQC)=DEN(Y,EQC)+(EQCRP90RP(Y,RECCL,B,D)+ &
                EQCESE(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+ &
                EQCSR90(Y,RECCL,B,D)+EQCADD(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))

              NUME1(Y,EQC,B,D)=&
               (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL)+ &
               (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D))*(WTEQCEFFN(Y,RECCL,B,D)) + &
                EQCRP90(Y,RECCL,B,D)*(WTEQCEFFR(Y,RECCL,B,D))+ &
               (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
               (WTEQCEFFA(Y,RECCL,B,D)))

              DEN1(Y,EQC,B,D)=(EQCRP90RP(Y,RECCL,B,D)+ &
                EQCESE(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+ &
                EQCSR90(Y,RECCL,B,D)+EQCADD(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))
 170  CONTINUE

!*******************************************************************
      DO 175 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        IF (DEN(Y,1).GT.0.0) RSEEFRF(Y1)=NUME(Y,1)/DEN(Y,1)
 175  CONTINUE
     DO 176 D=1,mNumCR-2
      DO 176 B=1,mNumBldg
       DO 176 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
       DO 176 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
        IF (DEN1(Y,1,B,D).GT.0.0) THEN
           RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,1,B,D)/DEN1(Y,1,B,D)
        ELSE
           RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)
        ENDIF
    176  CONTINUE

!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED EXISTING STANDALONE FREEZERS
!   SET EU = 9 TO SEARCH THE STANDALONE FREEZING SECTION OF THE DATA
!*******************************************************************
      EU = 9

      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RSEEFFZ(RECSYear-BaseYr+1)=RTBASEFF(RECSYear,RECCL)
       DO D=1,mNumCR-2
        DO B=1,mNumBldg
          RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)=RTBASEFF(RECSYear,RECCL)
        ENDDO
       ENDDO
      ENDDO

      DO 180 Y=RECSYear+1,LastYr+BaseYr-1
        DO 180 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          NUME(Y,EQC)=0.0
          DEN(Y,EQC)=0.0
          DO 180 D=1,mNumCR-2
            DO 180 B=1,mNumBldg
            NUME(Y,EQC)=NUME(Y,EQC)+ &
             (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL) + &
             (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
              EQCREP(Y,RECCL,B,D))* &
             (WTEQCEFFN(Y,RECCL,B,D)) + &
              EQCRP90(Y,RECCL,B,D)*(WTEQCEFFR(Y,RECCL,B,D))+ &
             (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
             (WTEQCEFFA(Y,RECCL,B,D)))

            DEN(Y,EQC)=DEN(Y,EQC)+(EQCRP90RP(Y,RECCL,B,D)+ &
              EQCESE(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+ &
              EQCSR90(Y,RECCL,B,D)+EQCADD(Y,RECCL,B,D)+ &
              EQCREP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))

              NUME1(Y,EQC,B,D)=&
               (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL)+ &
               (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D))*(WTEQCEFFN(Y,RECCL,B,D)) + &
                EQCRP90(Y,RECCL,B,D)*(WTEQCEFFR(Y,RECCL,B,D))+ &
               (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
               (WTEQCEFFA(Y,RECCL,B,D)))

              DEN1(Y,EQC,B,D)=(EQCRP90RP(Y,RECCL,B,D)+ &
                EQCESE(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+ &
                EQCSR90(Y,RECCL,B,D)+EQCADD(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))
 180  CONTINUE

!*******************************************************************
      DO 185 Y=RECSYear,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
        IF (DEN(Y,1).GT.0.0) RSEEFFZ(Y1)=NUME(Y,1)/DEN(Y,1)
 185  CONTINUE

     DO 186 D=1,mNumCR-2
      DO 186 B=1,mNumBldg
       DO 186 Y=RECSYear+1,LastYr+BaseYr-1
            Y1=Y-BaseYr+1
       DO 186 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
        IF (DEN1(Y,1,B,D).GT.0.0) THEN
          RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,1,B,D)/DEN1(Y,1,B,D)
        ELSE
          RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYear-BaseYr+1,RECCL,B,D)
        ENDIF
 186  CONTINUE

      WRITE(9,*) 'END-USE ELECTRICITY PRICES BY CD'	!kj - still needed? add switch to turn on?
      DO 190 EU=1,10
       WRITE(9,*) 'EU= ', eu,' ', EUPRNAMES(eu)
       DO 190 Y=RECSYear-BaseYr+1,LastYr
        WRITE(9,191) Y+(BaseYr-1),(PELRSOUT(D,Y,EU),D=1,mNumCR-2)
 190  CONTINUE
 191   FORMAT (I4,9(1X,F5.2))

       WRITE(9,*) 'AVERAGE ELECTRICITY PRICES'	!kj - still needed? add switch to turn on?
       DO 193 Y=RECSYear-BaseYr+1,LastYr
        WRITE(9,191) Y+(BaseYr-1),(PELRS(d,y),D=1,mNumCR-2)
 193  CONTINUE

      END SUBROUTINE RESDRP2


!*******************************************************************
!     INITIALIZE MARKET SHARES FOR HEATING EQUIPMENT from RECSYear to last year of Census SOC (formerly C25) data in RSHTSHR.txt
!*******************************************************************
      SUBROUTINE INTEQT
      IMPLICIT NONE
      LOGICAL NEW
      CHARACTER*18 FNAME
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
      INTEGER  IUNIT1
      INTEGER B, E ,D, Y, EU, RECCL
!********************************************************************

          EU = 1             ! SPACE HEATING SECTION OF THE DATA

      FNAME='RSHTSHR'
      NEW=.FALSE.
      IUNIT1=FILE_MGR('O',FNAME,NEW)
      READ(IUNIT1,'(19(/))')
      READ(IUNIT1,*) HTSHRYR      !HtShrYr
      READ(IUNIT1,'(3(/))')       !HtShrYr
     !HVAC historical data from Census SOC (formerly C25)
      DO 20 Y=RECSYear,HTSHRYR  !HtShrYr
       DO 20 D=1,mNumCR-2
        DO 20 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          READ(IUNIT1,*) (HSYSSHR(Y,RECCL,B,D),B=1,mNumBldg)
 20   CONTINUE
! 25   FORMAT(4X,3(F8.6,1X))
      IUNIT1=FILE_MGR('C',FNAME,NEW)
      END SUBROUTINE INTEQT

!*******************************************************************
!     NEW HOME HEATING SYSTEM REPORT
!*******************************************************************
      SUBROUTINE NHTSHR
      IMPLICIT NONE
      COMMON/TESTHT/HTYSSHR(RECSYear:EndYr,nHeatClasses,mNumBldg,mNumCR)
      INTEGER Y, D, B, E, E2, EU, RECCL, EQC, NUMEQC
      REAL*4  NWEQHTSH(RECSYear:EndYr,nHeatClasses),HTYSSHR &
             ,HEATCAL(RECSYear:EndYr,nHeatClasses),TOTALSUM(RECSYear:EndYr)
      CHARACTER*40 FN
!*******************************************************************
!     PRINT SUPLEMENTAL REPORT
!*******************************************************************

      EU = 1                      ! SPACE HEATING SECTION OF THE DATA
      NUMEQC=RTCLEUPT(EU+1)-RTCLEUPT(EU)

      DO 10 Y=RECSYear+1,LastYr+BaseYr-1
        DO 10 E=1,NUMEQC
               HEATCAL(Y,E)=0.0
          DO 10 B=1,mNumBldg
            DO 10 D=1,mNumCR-2
               HEATCAL(Y,E)= HEATCAL(Y,E) + &
                       (HSYSSHR(Y,E,B,D)*HSEADD(Y,B,D) )
 10   CONTINUE

      DO 15 Y=RECSYear+1,LastYr+BaseYr-1
               TOTALSUM(Y)=0.0
          DO 15 B=1,mNumBldg
            DO 15 D=1,mNumCR-2
               TOTALSUM(Y)=TOTALSUM(Y)+HSEADD(Y,B,D)
15    CONTINUE
      DO 20 Y=RECSYear+1,LastYr+BaseYr-1
        DO 20 E=1,NUMEQC
          IF (TOTALSUM(Y).GT.0.0) THEN
               NWEQHTSH(Y,E)=HEATCAL(Y,E) / TOTALSUM(Y)
          ELSE
               NWEQHTSH(Y,E)=0.0
          ENDIF
 20   CONTINUE

      WRITE(9,*) ' '
      WRITE(9,*) ' '
      WRITE(9,*)' Residential New Heating Report '
      WRITE(9,*)' New Home Heating System Shares (percent)'
      WRITE(9,*) ' '
      WRITE(9,6)(RTCLNAME(E),E=1,NUMEQC)
  6   FORMAT(6x,11(1X,A7))
      DO 200 Y=RECSYear+1,LastYr+BaseYr-1
        WRITE(9,2) Y,(NWEQHTSH(Y,E)*100.,E=1,NUMEQC)
 200  CONTINUE
 2    FORMAT(1X,I4,11(1X,F7.2))
      END SUBROUTINE NHTSHR


!*******************************************************************
!     BENCHMARK CONSUMPTION TO SEDS / STEO
!*******************************************************************
      SUBROUTINE RSBENCH
      IMPLICIT NONE
      INTEGER iLastSTEOYr ! allows the last STEO year parameter to be reset when turning off STEO benchmarking
      COMMON/STEO/STEOCN(RECSYear+1:LastSTEOYrAvail,9,mNumCR-2),STEObenchNG,STEObenchEL  !STEObenchX
      REAL*4 STEOCN,STEObenchNG,STEObenchEL  !STEObenchX
      LOGICAL NEW
      CHARACTER*18 FNAME
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
      INTEGER IUNIT1
      INTEGER Y,Y1,y4,D,F,B                    !STEOread-avg
      INCLUDE'steoblock' ! common STEO inputs  !STEOread-avg

      Y=CurIYr ! For brevity below

!   First year processing to read RSSTEO input file
      IF (CurCalYr.EQ.RECSYear) THEN
        FNAME='RSSTEO'
        NEW=.FALSE.
        IUNIT1=FILE_MGR('O',FNAME,NEW)
        READ(IUNIT1,'(19(/))')
        READ(IUNIT1,*) STEObenchNG  !STEObenchX
        READ(IUNIT1,'(1(/))')       !STEObenchX
        READ(IUNIT1,*) STEObenchEL  !STEObenchX
        READ(IUNIT1,'(1(/))')       !STEObenchX
        DO 50 F=3,4	 !STEOgasElecBench !Natural gas and electricity (F=1,2) are pulled directly from common steoblock below
          READ(IUNIT1,*)  !skip header
          DO 50 D=1,mNumCR-2
            READ(IUNIT1,*) (STEOCN(Y1,F,D),Y1=RECSYear+1,LastSTEOYrAvail)
!            WRITE(9,*) (STEOCN(Y1,F,D),Y1=RECSYear+1,LastSTEOYrAvail)  !Write RSSTEO.txt inputs to RESOUT.txt
        50  CONTINUE
        IUNIT1=FILE_MGR('C',FNAME,NEW)
!        WRITE(9,'("rsbench,steobm,iLastSTEOYr,LastSTEOYr,LastSEDSyr,msedyr,GLOBALBENCHON,BENCHALLYRS")')
      ENDIF

!     If STEOBM == 0, then turn off STEO benchmarking by setting
!       the last STEO year to the MER year.
      iLastSTEOYr=LastSTEOYr
      IF (STEOBM==0) iLastSTEOYr=LastSEDSyr+1    !LastSEDSyr=BaseYr-1+MSEDYR (declared in resdrep include file)

!      WRITE(9,'("rsbench",6i5,e15.4)') steobm,iLastSTEOYr,LastSTEOYr,LastSEDSyr,msedyr,GLOBALBENCHON,BENCHALLYRS

!STEOread-avg
!  Replace RSSTEO inputs with values for STEO variables from common steoblock - consistent
!    with MER through latest historical year and convert to trillion Btu. STEO does not
!    have sector-level liquids variables/coal. MER values for these fuels still need to be input -
!    currently in rsteo.txt. Regional natural gas values in STEO may also be problematic.

        DO Y1= RECSYear+1, LastSTEOYrAvail  ! Get MER/STEO data from common block !STEOread-avg

      !Natural gas
        ! Use regional natural gas shares applied to national STEO total to calculate CD consumption
         STEOCN(Y1,1,1)= NGRCP_NEC(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.
         STEOCN(Y1,1,2)= NGRCP_MAC(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.
         STEOCN(Y1,1,3)= NGRCP_ENC(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.
         STEOCN(Y1,1,4)= NGRCP_WNC(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.
         STEOCN(Y1,1,5)= NGRCP_SAC(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.
         STEOCN(Y1,1,6)= NGRCP_ESC(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.
         STEOCN(Y1,1,7)= NGRCP_WSC(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.
         STEOCN(Y1,1,8)= NGRCP_MTN(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.
         STEOCN(Y1,1,9)= NGRCP_PAC(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.

      !Electricity - convert from bkWh to Trills
        ! Get regional electricity values from STEO
         STEOCN(Y1,2,1)= EXRCP_NEC(y) * 3.412
         STEOCN(Y1,2,2)= EXRCP_MAC(y) * 3.412
         STEOCN(Y1,2,3)= EXRCP_ENC(y) * 3.412
         STEOCN(Y1,2,4)= EXRCP_WNC(y) * 3.412
         STEOCN(Y1,2,5)= EXRCP_SAC(y) * 3.412
         STEOCN(Y1,2,6)= EXRCP_ESC(y) * 3.412
         STEOCN(Y1,2,7)= EXRCP_WSC(y) * 3.412
         STEOCN(Y1,2,8)= EXRCP_MTN(y) * 3.412
         STEOCN(Y1,2,9)= (EXRCP_PAC(y)+EXRCP_HAK(y)) * 3.412

       ENDDO ! y !STEOread-avg

!  BEFORE BENCHMARKING, MAKE "HIGH-LEVEL" ADJUSTMENTS TO TOTALS FOR NATURAL GAS AND ELECTRICITY
       DO D=1,mNumCR-2
        !  DISTRIBUTED GENERATION
        !  ADD NATURAL GAS USAGE FOR FUEL CELLS AND DEDUCT SELF-GENERATED ELECTRICITY
        !  GasUsage(Y,D,2) IS FOR FUEL CELLS (DG TECHNOLOGY NUMBER 2)
         RSFLCN(Y,1,D)=RSFLCN(Y,1,D)+GasUsage(Y,D,2)-HWBTU(Y,D,2)
         ! DISTRIBUTED GENERATION, ELECTRICITY GENERATION OFFSETS FROM GRID PURCHASES  !DGreport
         RSFLCN(Y,2,D)=RSFLCN(Y,2,D)-TrillsOwnUse(Y,D,1)-TrillsOwnUse(Y,D,2)-TrillsOwnUse(Y,D,3)  !DGreport
       ENDDO

! TEST YEAR TO DETERMINE BENCHMARKING TREATMENTS
      IF (Y.LE.MSEDYR) THEN
        !SEDS Historical Benchmarking Years [Data from NEMS Global (QSBLK) in trillion Btu]
        DO D=1,mNumCR-2
          BNCHFCT(Y,1,D)=  QSNGRS(D,Y)-RSFLCN(Y,1,D)
          BNCHFCT(Y,2,D)=  QSELRS(D,Y)-RSFLCN(Y,2,D)
          BNCHFCT(Y,3,D)=  (QSDSRS(D,Y)+QSKSRS(D,Y))-RSFLCN(Y,3,D)
          BNCHFCT(Y,4,D)=  QSLGRS(D,Y)-RSFLCN(Y,4,D)
          BNCHFCT(Y,5,D)=  0.0  !KeroBench
        ENDDO

! Use Data from RSSTEO file instead of NEMS Global Data Structure (GDS) for testing before NEMS is loaded
! FOR HISTORICAL DATA TESTING ONLY, PRIOR TO UPDATE OF NEMS GDS TO MSEDYR = 2011
!          IF(CurCalYr.GT.RECSYear) THEN
!          DO F=1,4
!           DO D=1,mNumCR-2
!              BNCHFCT(Y,F,D)= STEOCN(CurCalYr,F,D)-RSFLCN(Y,F,D)
!           ENDDO
!          ENDDO
!          ENDIF

        !STEO Benchmarking Years (includes MSEDYR+1 MER Historical Data)
!        ELSEIF ((Y.GT.MSEDYR).AND.(CurCalYr.LE.iLastSTEOYr)) THEN  !KeroBench - old approach prior to combining kerosene with distillate fuel oil
!          DO F=1,4
!           DO D=1,mNumCR-2
!             BNCHFCT(Y,F,D)= STEOCN(CurCalYr,F,D)-RSFLCN(Y,F,D)
!           ENDDO
!          ENDDO

        !STEO Benchmarking Years (includes MSEDYR+1 MER Historical Data)  !KeroBench
        ELSEIF ((Y.GT.MSEDYR).AND.(CurCalYr.LE.iLastSTEOYr)) THEN  !KeroBench
          DO F=1,4  !KeroBench - Calculating through propane (4) instead of kerosene (5) now that kerosene combined with distillate fuel oil (3)
            DO D=1,mNumCR-2  !KeroBench
              BNCHFCT(Y,F,D)= STEOCN(CURCALYR,F,D)-RSFLCN(Y,F,D)  !KeroBench
            ENDDO  !KeroBench
          ENDDO  !KeroBench

         !Post-STEO Period Factors
        ELSEIF (CurCalYr.GT.iLastSTEOYr) THEN
          DO F=1,4	!kj - Reading in through propane (4) instead of kerosene (5) now that kerosene combined with distillate fuel oil (3)
           DO D=1,mNumCR-2
            IF (F.LE.2) THEN
             !Post-STEO period benchmarking option for gas and electricity
             ! Setting BENCHALLYRS=0. turns off (and assumes UECs were adjusted for SEDS/MER)
             ! Setting BENCHALLYRS=1. turns on and keeps benchmarking factor for the remainder of the projection years
              BNCHFCT(Y,F,D)= BNCHFCTAVG(F,D)*BENCHALLYRS                                !STEOread-avg
              ! Temporarily freeze benchmarking to MER year, with ability to tweak final totals from RSSTEO.txt for final benchmarking purposes   !STEObenchX
              IF (F.EQ.1 .AND. STEObenchNG.NE.1.0) BNCHFCT(Y,F,D) = BNCHFCT(LastSEDSyr+1-(BaseYr-1),F,D)*STEObenchNG*BENCHALLYRS    !STEObenchX
              IF (F.EQ.2 .AND. STEObenchEL.NE.1.0) BNCHFCT(Y,F,D) = BNCHFCT(LastSEDSyr+1-(BaseYr-1),F,D)*STEObenchEL*BENCHALLYRS    !STEObenchX
            ELSE
             !Maintain STEO bench factors for other fuels
              BNCHFCT(Y,F,D)= BNCHFCTAVG(F,D)                                            !STEOread-avg
            ENDIF !F.LE.2
           ENDDO !D
          ENDDO !F=1,4
      ENDIF !CurCalYr (and Y)

!  TAKE AVERAGE OF LAST 5 YEARS OF BNCHFCT FOR FIRST YEAR AFTER HISTORICAL DATA          !STEOread-avg
!   (OR THE NUMBER OF YEARS BETWEEN RECSYear AND MER YEAR)                               !STEOread-avg
        IF (Y .EQ. MSEDYR+2) THEN  !MSED+2 is first year after MER                       !STEOread-avg
          DO D=1,mNumCR-2                                                                !STEOread-avg
            BNCHFCTAVG(1:4,D)= 0.0                                                       !STEOread-avg  !DGreport - general cleanup

            DO y4= 1, MIN(MSEDYR+1-(RECSYear-BaseYr+1),4)                                !STEOread-avg
              BNCHFCTAVG(1,D)=  BNCHFCTAVG(1,D) + BNCHFCT(Y-y4,1,D)                      !STEOread-avg
              BNCHFCTAVG(2,D)=  BNCHFCTAVG(2,D) + BNCHFCT(Y-y4,2,D)                      !STEOread-avg
              BNCHFCTAVG(3,D)=  BNCHFCTAVG(3,D) + BNCHFCT(Y-y4,3,D)                      !STEOread-avg
              BNCHFCTAVG(4,D)=  BNCHFCTAVG(4,D) + BNCHFCT(Y-y4,4,D)                      !STEOread-avg
            ENDDO !y4                                                                    !STEOread-avg

            BNCHFCTAVG(1,D)=  BNCHFCTAVG(1,D) / MIN((MSEDYR+1)-(RECSYear-BaseYr+1),5)    !STEOread-avg
            BNCHFCTAVG(2,D)=  BNCHFCTAVG(2,D) / MIN((MSEDYR+1)-(RECSYear-BaseYr+1),5)    !STEOread-avg
            BNCHFCTAVG(3,D)=  BNCHFCTAVG(3,D) / MIN((MSEDYR+1)-(RECSYear-BaseYr+1),5)    !STEOread-avg
            BNCHFCTAVG(4,D)=  BNCHFCTAVG(4,D) / MIN((MSEDYR+1)-(RECSYear-BaseYr+1),5)    !STEOread-avg
          ENDDO !D                                                                       !STEOread-avg
        ENDIF  !Y                                                                        !STEOread-avg

! END TEST YEAR TO DETERMINE BENCHMARKING TREATMENTS ^^^^

!   Turn all post-RECS benchmarking off for testing purposes only
!     This option maintains only the RECS benchmarking factors by
!     setting all post-RECS years to those of the RECS year
      IF (GLOBALBENCHON==0 .AND. CurCalYr>RECSYear) THEN
          DO D=1,mNumCR-2
            DO F=1,4  !KeroBench
              BNCHFCT(Y,F,D)= BNCHFCT(Y-1,F,D)
!              WRITE(9,'("rsNObench Y F D ",3i5,f7.1)') y,f,d,bnchfct(y,f,d)	!kj - add switch to print these?
            ENDDO
          ENDDO
       ENDIF

!   Print benchmarking for testing purposes
      IF (GLOBALBENCHON==1) THEN
          DO D=1,mNumCR-2
            DO F=1,4  !KeroBench
!              WRITE(9,'("rsbenchfactors Y F D ",3i5,f12.2)') y,f,d,bnchfct(y,f,d)	!kj - add switch to print these?
            ENDDO
          ENDDO
       ENDIF

!     CALCULATE RSFLCN
       DO D=1,mNumCR-2
         DO F=1,4  !KeroBench - Reading in through propane (4) instead of kerosene (5) now that kerosene combined with distillate fuel oil (3)
           ! Additive benchmarking for Natural Gas, Electricity, Distillate Fuel Oil + Kerosene, and Propane
           RSFLCN(Y,F,D)=RSFLCN(Y,F,D)+BNCHFCT(Y,F,D)
         ENDDO !F
       ENDDO !D

!BENCHMARK NATURAL GAS (F=1), DISTILLATE FUEL OIL + KEROSENE (F=3), AND PROPANE (F=4) USING SINGLE-FAMILY SPACE HEATING
  !Natural gas
       F=1
       B=1
       DO D=1,mNumCR-2
         HTRCON(Y,F,D)=HTRCON(Y,F,D)+BNCHFCT(Y,F,D)*1000000.
!         HTRCONIN(Y,F,D,B)=(BNCHFCT(Y,F,d)*1000000.+HTRCONWT(Y,F,d,B))/HTRCONWT(y,1,d,B)*HTRCONIN(Y,1,d,B)
         HTRCONWT(Y,F,D,B)=HTRCONWT(Y,F,D,B)+BNCHFCT(Y,F,D)*1000000.
       ENDDO

  !Distillate fuel oil + kerosene and propane
       B=1 !Single-family
       DO D=1,9
         DO F=3,4
           HTRCON(Y,F,D)=HTRCON(Y,F,D)+BNCHFCT(Y,F,D)*1000000.
           ! Don't allow weights to become negative (consumption could have also been capped, but is checked manually)
           IF(HTRCONWT(Y,F,D,B)+BNCHFCT(Y,F,D)*1000000. .GT. 0. .AND. HTRCONWT(Y,F,D,B) .GT. 0.) THEN
             HTRCONIN(Y,F,D,B)=(BNCHFCT(Y,F,D)*1000000.+HTRCONWT(Y,F,D,B))/HTRCONWT(Y,F,D,B)*HTRCONIN(Y,F,D,B)
             HTRCONWT(Y,F,D,B)=HTRCONWT(Y,F,D,B)+BNCHFCT(Y,F,D)*1000000.
           ELSE
             HTRCONIN(Y,F,D,B)= 0.
             HTRCONWT(Y,F,D,B)= 0.
           ENDIF
         ENDDO !F=3,4
       ENDDO !D=1,9
 115   CONTINUE

!BENCHMARK ALL DIVISIONS USING OTHER APPLIANCES  !DGreport - No longer removing own-use generation from space cooling or other electric appliance/MELs consumption
      DO D=1,mNumCR-2
        B=1
        APCON(Y,D)=APCON(Y,D)+BNCHFCT(Y,2,D)*1000000.  !DGreport
        APCONWT(Y,D,B)=APCONWT(Y,D,B)+BNCHFCT(Y,2,D)*1000000.  !DGreport
        EAEQCN(Y,1,B,D)=EAEQCN(Y,1,B,D)+BNCHFCT(Y,2,D)*1000000.  !DGreport
      ENDDO !census divisions

!  ADJUST NATURAL GAS USAGE FOR DISTRIBUTED GENERATION (FUEL CELLS)
!   (ASSUMING FUEL CELLS PENETRATE WHERE GAS IS AVAILABLE USING WATER HEATING)
       DO D=1,mNumCR-2
         H2OCON(Y,1,D)=H2OCON(Y,1,D)-HWBTU(Y,D,2)*1000000.+(GasUsage(Y,D,2)+GasUsage(Y,D,3))*1000000.0
         DO B=1,mNumBldg
          IF(B.EQ.1) THEN
            H2OCONWT(Y,1,D,B)=H2OCONWT(Y,1,D,B)-(HWBTU(Y,D,2)+GasUsage(Y,D,2))*1000000.0
            EQCEQCN(Y,19,B,D)=EQCEQCN(Y,19,B,D)-(HWBTU(Y,D,2)+GasUsage(Y,D,2))*1000000.0
          ENDIF
         ENDDO  ! Building Types
       ENDDO ! Census Divisions

      END SUBROUTINE RSBENCH


!*******************************************************************
!     Residential Database File
!*******************************************************************
      SUBROUTINE RESDBOUT
      IMPLICIT NONE
!      INTEGER MaxApps, MaxTypes, MaxBins! These are used in looping of report variables - These are set near top of code
!      PARAMETER (MaxApps=4)  !Maximum number of applications
!      PARAMETER (MaxTypes=4) !Maximum number of bulb types within an application
!      PARAMETER (MaxBins=6)  !Maximum number of bulb bins within an application
      INTEGER app, bin
      REAL*4 temp

      !COMMON/DBEFFOUT/RSNEFDB1(mNumYr,MNUMRTTY,mNumBldg,mNumCR-2),RSEEFDB1(mNumYr,MNUMRTTY,mNumBldg,mNumCR-2)  !DYN
      !REAL*4 RSNEFDB1,RSEEFDB1
      REAL*4 bb
      REAL*4 HEATERS(RECSYear:EndYr,nHeatClasses,mNumBldg,mNumCR-2)
      REAL*4 COOLERS(RECSYear:EndYr,5,mNumBldg,mNumCR-2)
      REAL*4 WATERS(RECSYear:EndYr,5,mNumBldg,mNumCR-2)
      REAL*4 COOKS(RECSYear:EndYr,3,mNumBldg,mNumCR-2)
      REAL*4 DRYERS(RECSYear:EndYr,2,mNumBldg,mNumCR-2)
      REAL*4 FRIGS(RECSYear:EndYr,mNumBldg,mNumCR-2)
      REAL*4 FREEZE(RECSYear:EndYr,mNumBldg,mNumCR-2)
      REAL*4 CLOTHE(RECSYear:EndYr,mNumBldg,mNumCR-2)
      REAL*4 DISHW(RECSYear:EndYr,mNumBldg,mNumCR-2)

      INTEGER W, Y, B, E, D, F, P, E2, Z, R,Y1
      INTEGER ff(mNumFuel-2)  !pointers to benchmarking fuels from rtfuel (mNumFuel=8)  !EqpParam
      INTEGER EU,RECCL,EQC,EQTYPE,TYPE,RECTY,S
      INTEGER EQCGHP,EQCEHP,RECCLGHP,RECCLEHP,EQCSWH,EQCEWH,RECCLSWH,RECCLEWH
      INTEGER FILE_MGR       ! FILE MANAGER
      INTEGER*4  OUTFILE     ! FILE HANDLE
      CHARACTER*18 FN,CNAME(mNumRTCl),TNAME(MNUMRTTY),LTNAME(4)	!kj - replace values with variables?
      CHARACTER*18 REGION(mNumCR+1)  !EqpParam
      CHARACTER*3 FL,HQ(nHeatClasses+1),CQ(nCoolClasses+1),WQ(nWatHtClasses),KQ(nCookClasses),DQ(nClDryClasses)  !EqpParam
      CHARACTER*4 HQT(nHeatTypes),SR    !eu
      CHARACTER*8 EUNAME(9),SN(nShellTypes)  !EqpParam	!kj - replace values with variables?
      CHARACTER*13 HTYPES(mNumBldg+1)  !Number of building types plus Total  !EqpParam
!*******************************************************************
      INTEGER DUM, zero
!      REAL*4 TH(RECSYear:EndYr,3)
!      REAL*4 THD(RECSYear:EndYr,mNumBldg,mNumCR-2)
      DATA ff/3,4,1,2,5,6/   ! pointers to benchmarking fuels from rtfuel	!kj
      DATA EUNAME/'HEAT','COOL','CWASH','DWASH','HOTWATER','COOK','DRYERS','FRIDG','FREEZE'/	!kj - for some reason, these all print out to RESDEQP as 4 spaces and 4 characters despite being CHARACTER*8 and formatted as A8 in the write statement
      DATA HQ/'EFN','EHP','GFN','GOT','KER','LPG','DFN','DOT','WST','GE1','GHP','GE2'/  !correspond to nHeatClasses+1 (for geothermal energy GE2)	!kj
      DATA CQ/'RAC','CAC','EHP','GE1','GHP','GE2'/  !correspond to nCoolClasses+1 (for geothermal energy GE2)	!kj
      DATA WQ/'GAS','ELE','DIS','LPG','SOL'/  !water heater fuels
      DATA HQT/'EFN2','EHP1','EHP2','EHP3','EHP4','GFN1','GFN2','GFN3','GFN4','GOT1','GOT2','GOT3','GOT4','KER1','KER2','KER3','KER4',&	!kj - corresponde to nHeatTypes; not used?
       'LPG1','LPG2','LPG3','LPG4','DFN1','DFN2','DFN3','DFN4','DOT1','DOT2','DOT3','DOT4','WST2','WST4','GEO1','GEO2','GEO3','GEO4','GHP2'/	!kj - corresponde to nHeatTypes; not used?
      DATA LTNAME/'LT-GSL','LT-REFL','LT-FLUOR','LT-EXT'/	!kj - based on current RSMLGT.txt order; not used?
      DATA KQ/'GAS','LPG','ELE'/	!kj - not used?
      DATA DQ/'GAS','ELE'/	!kj - not used?
      DATA SN/'NoCode','IECC','ESTAR','IECC+40','PATH'/	!kj - not used?
      DATA HTYPES/'Single-Family',' Multifamily',' Mobile Homes','        Total'/	!kj - not used?
      DATA REGION/'New England    ','Mid Atlantic    ','E North Central','W North Central','South Atlantic  ','E South Central', &	!kj - not used?
       'W South Central','Mountain        ','Pacific        ',' United States ','Less CA         ','United States  '/	!kj - not used?

!                  DO 649 f=1,6
!                 DO 649 d=1,mNumCR-2
! 649       bnchfct(4,f,d)=1.0

      zero=0

 !      DO 650 Y=RECSyear,EndYr
 !       DO 650 B=1,mNumBldg
 !         TH(Y,B)=0.0
 !         DO 650 D=1,mNumCR-2
 !           TH(Y,B)=TH(Y,B)+ (EH(Y,B,D)+NH(Y,B,D))
 !           THD(Y,B,D)=EH(Y,B,D)+NH(Y,B,D)
 !650   CONTINUE

!*******************************************************************
!  AGGREGATE EQUIPMENT	!kj - Perhaps convert this section of code into CASE statement?
!*******************************************************************
      DO 800 Y=RECSyear,EndYr
        Z=Y
        DO 800 D=1,mNumCR-2
          DO 800 B=1,mNumBldg
          EU = 1             ! SPACE HEATING SECTION OF THE DATA

            DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)
              IF (Z.EQ.RECSYear) THEN
                HEATERS(Y,EQC,B,D)=EQCESE(RECSYear,RECCL,B,D)
              ELSE
                HEATERS(Y,EQC,B,D)= &
                 EQCESE(Y,RECCL,B,D)+EQCRP90(Z,RECCL,B,D)+EQCRP90RP(Z,RECCL,B,D)+EQCSR90(Z,RECCL,B,D)+ & !equipment in existing RECS-year households
                 EQCADD(Z,RECCL,B,D)+EQCREP(Z,RECCL,B,D)+EQCSUR(Z,RECCL,B,D) !equipment in post-RECS-year (new) households
              ENDIF
            ENDDO

          EU = 2             ! SPACE COOLING SECTION OF THE DATA

            DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)
              IF (Z.EQ.RECSYear) THEN
                 COOLERS(Y,EQC,B,D)=EQCESE(RECSYear,RECCL,B,D)
              ELSE
                COOLERS(Y,EQC,B,D)= &
                 EQCESE(Y,RECCL,B,D)+EQCRP90(Z,RECCL,B,D)+EQCRP90RP(Z,RECCL,B,D)+EQCSR90(Z,RECCL,B,D)+ & !equipment in existing RECS-year households
                 EQCADD(Z,RECCL,B,D)+EQCREP(Z,RECCL,B,D)+EQCSUR(Z,RECCL,B,D) !equipment in post-RECS-year (new) households
              ENDIF
            ENDDO

          EU = 5             ! WATER HEATING SECTION OF THE DATA

            DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)
              IF (Z.EQ.RECSYear) THEN
                WATERS(Y,EQC,B,D)=EQCESE(RECSYear,RECCL,B,D)
              ELSE
                WATERS(Y,EQC,B,D)= &
                 EQCESE(Y,RECCL,B,D)+EQCRP90(Z,RECCL,B,D)+EQCRP90RP(Z,RECCL,B,D)+EQCSR90(Z,RECCL,B,D)+ & !equipment in existing RECS-year households
                 EQCADD(Z,RECCL,B,D)+EQCREP(Z,RECCL,B,D)+EQCSUR(Z,RECCL,B,D) !equipment in post-RECS-year (new) households
              ENDIF
            ENDDO

          EU = 6             ! COOKING SECTION OF THE DATA

            DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)
              IF (Z.EQ.RECSYear) THEN
                COOKS(Y,EQC,B,D)=EQCESE(RECSYear,RECCL,B,D)
              ELSE
                COOKS(Y,EQC,B,D)= &
                 EQCESE(Y,RECCL,B,D)+EQCRP90(Z,RECCL,B,D)+EQCRP90RP(Z,RECCL,B,D)+EQCSR90(Z,RECCL,B,D)+ & !equipment in existing RECS-year households
                 EQCADD(Z,RECCL,B,D)+EQCREP(Z,RECCL,B,D)+EQCSUR(Z,RECCL,B,D) !equipment in post-RECS-year (new) households
              ENDIF
            ENDDO

          EU = 7             ! CLOTHES DRYING SECTION OF THE DATA

            DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)
              IF (Z.EQ.RECSYear) THEN
                DRYERS(Y,EQC,B,D)=EQCESE(RECSYear,RECCL,B,D)
              ELSE
                DRYERS(Y,EQC,B,D)= &
                 EQCESE(Y,RECCL,B,D)+EQCRP90(Z,RECCL,B,D)+EQCRP90RP(Z,RECCL,B,D)+EQCSR90(Z,RECCL,B,D)+ & !equipment in existing RECS-year households
                 EQCADD(Z,RECCL,B,D)+EQCREP(Z,RECCL,B,D)+EQCSUR(Z,RECCL,B,D) !equipment in post-RECS-year (new) households
             ENDIF
            ENDDO

          EU = 8             ! FOOD REFRIGERATION SECTION OF THE DATA

            DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)

            IF (Z.EQ.RECSYear) THEN
               FRIGS(Y,B,D)=EQCESE(RECSYear,RECCL,B,D)

            ELSE
              FRIGS(Y,B,D)= &
               EQCESE(Y,RECCL,B,D)+EQCRP90(Z,RECCL,B,D)+EQCRP90RP(Z,RECCL,B,D)+EQCSR90(Z,RECCL,B,D)+ & !equipment in existing RECS-year households
               EQCADD(Z,RECCL,B,D)+EQCREP(Z,RECCL,B,D)+EQCSUR(Z,RECCL,B,D) !equipment in post-RECS-year (new) households
              ENDIF
            ENDDO

          EU = 9             !FOOD FREEZING SECTION OF THE DATA

            DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)

              IF (Z.EQ.RECSYear) THEN
                FREEZE(Y,B,D)=EQCESE(RECSYear,RECCL,B,D)
              ELSE
                FREEZE(Y,B,D)= &
                 EQCESE(Y,RECCL,B,D)+EQCRP90(Z,RECCL,B,D)+EQCRP90RP(Z,RECCL,B,D)+EQCSR90(Z,RECCL,B,D)+ & !equipment in existing RECS-year households
                 EQCADD(Z,RECCL,B,D)+EQCREP(Z,RECCL,B,D)+EQCSUR(Z,RECCL,B,D) !equipment in post-RECS-year (new) households
              ENDIF
            ENDDO

         EU = 3             !CLOTHES WASHER SECTION OF THE DATA

            DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)

              IF (Z.EQ.RECSYear) THEN
                CLOTHE(Y,B,D)=EQCESE(RECSYear,RECCL,B,D)
              ELSE
                CLOTHE(Y,B,D)= &
                 EQCESE(Y,RECCL,B,D)+EQCRP90(Z,RECCL,B,D)+EQCRP90RP(Z,RECCL,B,D)+EQCSR90(Z,RECCL,B,D)+ & !equipment in existing RECS-year households
                 EQCADD(Z,RECCL,B,D)+EQCREP(Z,RECCL,B,D)+EQCSUR(Z,RECCL,B,D) !equipment in post-RECS-year (new) households
              ENDIF
            ENDDO

         EU = 4             !DISHWASHER SECTION OF THE DATA

            DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)

              IF (Z.EQ.RECSYear) THEN
                DISHW(Y,B,D)=EQCESE(RECSYear,RECCL,B,D)
              ELSE
                DISHW(Y,B,D)= &
                 EQCESE(Y,RECCL,B,D)+EQCSR90(Z,RECCL,B,D)+EQCRP90(Z,RECCL,B,D)+EQCRP90RP(Z,RECCL,B,D)+ & !equipment in existing RECS-year households
                 EQCSUR(Z,RECCL,B,D)+EQCADD(Z,RECCL,B,D)+EQCREP(Z,RECCL,B,D) !equipment in post-RECS-year (new) households
              ENDIF
            ENDDO

 800  CONTINUE

!*******************************************************************
!  CALCULATE EQUIPMENT CONSUMPTION FOR RECSYear	!kj - Seems like these could probably be combined into a better loop (EU=1,9) or CASE statement
!*******************************************************************
      DO 100 D=1,mNumCR-2
        DO 100 B=1,mNumBldg

          EU = 1             ! SPACE HEATING SECTION OF THE DATA

          DO 10 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            EQCEQCN(RECSYear-BaseYr+1,RECCL,B,D)=EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B)

!   FIND THE RECORD NUMBERS FOR THE GROUND-SOURCE AND ELECTRIC AIR-SOURCE HEAT PUMPS FOR HEATING
            IF(RTCLNAME(RECCL).EQ.'GEO_HP') RECCLGHP=RECCL
            IF(RTCLNAME(RECCL).EQ.'ELEC_HP') RECCLEHP=RECCL
 10       CONTINUE

! GEOTHERMAL HP
          GEEQCN(RECSYear-BaseYr+1,1,B,D)=(EQCESE(RECSYear,RECCLGHP,B,D)*EQCUEC(D,RECCLGHP,B)*.2)*WHRFOSS(D,RECSYear-BaseYr+1)/3412.  !STEOhr	!kj - Why *0.2?

          EU = 2             ! SPACE COOLING SECTION OF THE DATA

          DO 20 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            EQCEQCN(RECSYear-BaseYr+1,RECCL,B,D)=EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B)

!   FIND THE EQUIPMENT CLASS NUMBERS FOR GEOTHERMAL AND ELECTRIC AIR-SOURCE HEAT PUMPS FOR COOLING
            IF(RTCLNAME(RECCL).EQ.'GEO_HP') THEN
              EQCGHP=RECCL
!              RECCLGHP=EQCGHP+RTCLEUPT(EU)
               RECCLGHP=RECCL
            ENDIF
            IF(RTCLNAME(RECCL).EQ.'ELEC_HP') THEN
               EQCEHP=RECCL
!              RECCLEHP=EQCEHP+RTCLEUPT(EU)
               RECCLEHP=RECCL
            ENDIF
 20       CONTINUE

         !GEOTHERMAL HEAT PUMP
          GEEQCN(RECSYear-BaseYr+1,2,B,D)= (EQCESE(RECSYear,RECCLGHP,B,D)*EQCUEC(D,RECCLGHP,B)*.2)*WHRFOSS(D,RECSYear-BaseYr+1)/3412.  !STEOhr	!kj - why *0.2?

         EU = 3             ! CLOTHES WASHER SECTION OF THE DATA
         DO 21 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
!            EQC=RTCLEQCL(RECCL)
 21         EQCEQCN(RECSYear-BaseYr+1,RECCL,B,D)=EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B)

         EU = 4             ! DISHWASHER SECTION OF THE DATA
         DO 22 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
!            EQC=RTCLEQCL(RECCL)
 22         EQCEQCN(RECSYear-BaseYr+1,RECCL,B,D)=EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B)

          EU = 5             ! WATER HEATING SECTION OF THE DATA
          DO 30 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
!            EQC=RTCLEQCL(RECCL)
 30         EQCEQCN(RECSYear-BaseYr+1,RECCL,B,D)=EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B)

          EU = 6             ! COOKING SECTION OF THE DATA
          DO 40 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
!            EQC=RTCLEQCL(RECCL)
            EQCEQCN(RECSYear-BaseYr+1,RECCL,B,D)=EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B)
!            WRITE(23,*) 'equipment=',eqcese(RECSYear,RECCL,b,d),
!     1                  '   uec=   ',eqcuec(d,RECCL,b)
 40       CONTINUE

!          WRITE(23,*) 'total =',eqceqcn(RECSYear,eqc,b,d)

          EU = 7             ! CLOTHES DRYING SECTION OF THE DATA
          DO 50 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
!            EQC=RTCLEQCL(RECCL)
 50         EQCEQCN(RECSYear-BaseYr+1,RECCL,B,D)=EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B)

         EU = 8             ! FOOD REFRIGERATION SECTION OF THE DATA
         DO 52 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
!            EQC=RTCLEQCL(RECCL)
 52         EQCEQCN(RECSYear-BaseYr+1,RECCL,B,D)=EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B)

         EU = 9             ! FOOD FREEZING SECTION OF THE DATA
         DO 55 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
!            EQC=RTCLEQCL(RECCL)
 55         EQCEQCN(RECSYear-BaseYr+1,RECCL,B,D)=EQCESE(RECSYear,RECCL,B,D)*EQCUEC(D,RECCL,B)

         DO 70 E=1,3
 70        APEQCN(RECSYear-BaseYr+1,E,B,D)=APPEQP(RECSYear,B,D,E)*APPUEC(D,E,B)
           TVSEQCN(RECSYear-BaseYr+1,1,B,D)=TVSEQP(RECSYear,B,D)*TVSUEC(D,B)
           STBEQCN(RECSYear-BaseYr+1,1,B,D)=STBEQP(RECSYear,B,D)*STBUEC(D,B)
           HTSEQCN(RECSYear-BaseYr+1,1,B,D)=HTSEQP(RECSYear,B,D)*HTSUEC(D,B)
           OTTEQCN(RECSYear-BaseYr+1,1,B,D)=OTTEQP(RECSYear,B,D)*OTTUEC(D,B)  !MELs21
           VGCEQCN(RECSYear-BaseYr+1,1,B,D)=VGCEQP(RECSYear,B,D)*VGCUEC(D,B)
           DPCEQCN(RECSYear-BaseYr+1,1,B,D)=DPCEQP(RECSYear,B,D)*DPCUEC(D,B)
           LPCEQCN(RECSYear-BaseYr+1,1,B,D)=LPCEQP(RECSYear,B,D)*LPCUEC(D,B)
           MONEQCN(RECSYear-BaseYr+1,1,B,D)=MONEQP(RECSYear,B,D)*MONUEC(D,B)
           NETEQCN(RECSYear-BaseYr+1,1,B,D)=NETEQP(RECSYear,B,D)*NETUEC(D,B)
           BATEQCN(RECSYear-BaseYr+1,1,B,D)=BATEQP(RECSYear,B,D)*BATUEC(D,B)
           CFNEQCN(RECSYear-BaseYr+1,1,B,D)=CFNEQP(RECSYear,B,D)*CFNUEC(D,B)
           COFEQCN(RECSYear-BaseYr+1,1,B,D)=COFEQP(RECSYear,B,D)*COFUEC(D,B)
           DEHEQCN(RECSYear-BaseYr+1,1,B,D)=DEHEQP(RECSYear,B,D)*DEHUEC(D,B)
           MCOEQCN(RECSYear-BaseYr+1,1,B,D)=MCOEQP(RECSYear,B,D)*MCOUEC(D,B)
           PLPEQCN(RECSYear-BaseYr+1,1,B,D)=PLPEQP(RECSYear,B,D)*PLPUEC(D,B)  !MELs21
           PLHEQCN(RECSYear-BaseYr+1,1,B,D)=PLHEQP(RECSYear,B,D)*PLHUEC(D,B)  !MELs21
           SECEQCN(RECSYear-BaseYr+1,1,B,D)=SECEQP(RECSYear,B,D)*SECUEC(D,B)
           SPAEQCN(RECSYear-BaseYr+1,1,B,D)=SPAEQP(RECSYear,B,D)*SPAUEC(D,B)
           WCLEQCN(RECSYear-BaseYr+1,1,B,D)=WCLEQP(RECSYear,B,D)*WCLUEC(D,B)    !winecool
           SPKEQCN(RECSYear-BaseYr+1,1,B,D)=SPKEQP(RECSYear,B,D)*SPKUEC(D,B)  !MELs21
           PHNEQCN(RECSYear-BaseYr+1,1,B,D)=PHNEQP(RECSYear,B,D)*PHNUEC(D,B)  !MELs21
           TABEQCN(RECSYear-BaseYr+1,1,B,D)=TABEQP(RECSYear,B,D)*TABUEC(D,B)  !MELs21
           KITEQCN(RECSYear-BaseYr+1,1,B,D)=KITEQP(RECSYear,B,D)*KITUEC(D,B)  !MELs21

 100  CONTINUE
 984  FORMAT(A4,',',I2,',',I2,',',A2,',',A8,',',I4,',',F12.0,',',I13,',',I3,',',A5)    !eu
 985  FORMAT(A7,A5,A5,A5,A9,A5,A8,A12,A11,A8)
 986  FORMAT(A7,A5,A5,A5,A9,A8,A5,A7,A6,A12,A12,6A10,A12,A13,A11,A13,A9,10a13)    !Utility_invest
 987  FORMAT(A8,',',I4,',',I4,',',A2,',',A8,',',A10,',',I4,2(',',F9.4),23(',',I11))    !Utility_invest
 988  FORMAT(1X,A8,1X,I4,1X,I4,1X,A2,1X,A8,1X,A8,1X,I4,2(1X,I11))
 989  FORMAT(A4,',',I2,',',I2,',',A2,',',A8,',',I4,',',F12.0,',',I13,',',A5)    !eu
 990  FORMAT(A8,',',I4,',',I4,',',A2,',',A8,',',A8,',',I4,2(',',F9.4),11(',',f11.0))    !Utility_invest
 991  FORMAT(A4,',',I2,',',I2,',',A2,',',A8,',',I4,',',F12.0,',',I13,',',I13)    !eu
 992  FORMAT(A4, ',', 4(I2,','), I4, ',', F12.8, ',', I2, ',', I2)
 999  FORMAT(1X,A2,1X,I4,1X,A2,1X,A3,1X,I4,6(1X,F10.3))
 998  FORMAT(A4,',',I2,',',A2,',',A2,',',A8,',',I4,',',F12.2,',',I13)    !eu

!--------------------------------------------------------------
!   OPEN FILE AND WRITE DATA
      OUTFILE=FILE_MGR('O','RESDEQP',.TRUE.) !OPEN THE output file
      WRITE(OUTFILE,986) 'ENDUSE,','CDIV,','BLDG,','FUEL,','EQPCLASS,','EQPTYPE,', &
      'YEAR,','NEWEFF,','EXEFF,','NEWPURCHASE,','REPPURCHASE,',&
      'NEWINVEST,','REPINVEST,','NEWFEDSUB,','REPFEDSUB,','NEWNFDSUB,','REPNFDSUB,', &    !Utility_invest
      'NON_IECC09,','IECC2009,','ESTAR,','IECC+40%,','BEST,', &
      'I_NON_IECC09,','I_IECC2009,','I_ESTAR,','I_IECC+40%,','I_BEST,', &
      'S_NON_IECC09,','S_IECC2009,','S_ESTAR,','S_IECC+40%,','S_BEST'

      DO 150 D=1,mNumCR-2
       DO 150 B=1,mNumBldg
!       DO 150 Y=RECSYear+1,EndYr
        DO 150 Y=RECSYear+1,LastYr+BaseYr-1
          DO 140 EU = 1,9	!kj - replace 9 with variable for last end use number
          SR=EUNAME(EU)
          TYPE = RTTYPECT(EU)
          DO 140 RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)

!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
              IF((Y.GE.RTINITYR(RECTY).AND. Y.LE.RTLASTYR(RECTY)).AND.(RTCENDIV(RECTY).EQ.D)) THEN
                TYPE = TYPE + 1
                EQTYPE=RTEQTYPE(RECTY)
                EQC=RTTYEQCL(RECTY)
                TNAME=RTTYNAME(RECTY)
                RECCL=RTCLEUPT(EU)+EQC
                CNAME=RTCLNAME(RECCL)
                F    =RTFUEL(RECCL)
                E=RTCLEQCL(RECCL)
                Y1=Y-BaseYr+1

    !   FIND THE RECORD NUMBER FOR THE GROUND-SOURCE HEAT PUMP FOR HEATING
                IF(RTCLNAME(RECCL).EQ.'GEO_HP') RECCLGHP=RECCL

    !   DEFINE THE NAMES CORRESPONDING TO FUEL NUMBERS
                IF (RTCLNAME(RECCL).EQ.'WOOD_HT') THEN
                  FL='WD'
                ELSEIF (RTFUEL(RECCL).EQ.4) THEN
                  FL='EL'
                ELSEIF (RTFUEL(RECCL).EQ.3) THEN
                  FL='GS'
                ELSEIF (RTFUEL(RECCL).EQ.5) THEN
                  FL='KS'
                ELSEIF (RTFUEL(RECCL).EQ.2) THEN
                  FL='LG'
                ELSEIF (RTFUEL(RECCL).EQ.1) THEN
                  FL='DS'
                ELSE
                  FL='SO'
                ENDIF

                IF(EU.EQ.1) THEN !Calculate shell investment and subsidies with heating system records only
                  WRITE(OUTFILE,987) SR,D,B,FL,RTCLNAME(RECCL),RTTYNAME(RECTY),Y , RSNEFDB1(Y1,RECCL,B,D), RSEEFDB1(Y1,RECCL,B,D),&
                  INT(HEATINGTYPEPURCH(Y,TYPE,B,D,1)),INT(HEATINGTYPEPURCH(Y,TYPE,B,D,2)),&
                  INT(HEATINGTYPEPURCH(Y,TYPE,B,D,1)*(rtrecost(recty)+rtresub(recty)+rtresubn(recty)+FLOAT(EPA111D)*RTRESUB111D(recty))),   & !Utility_invest  Investment excludes installation costs, so use rtresub for investment only
                  INT(HEATINGTYPEPURCH(Y,TYPE,B,D,2)*(rtrecost(recty)+rtresub(recty)+rtresubn(recty)+FLOAT(EPA111D)*RTRESUB111D(recty))),   & !Utility_invest  Investment excludes installation costs for retrofit equipment because of way heat pump costs are split between heating & cooling (and replacement versus new)
                  INT(HEATINGTYPEPURCH(Y,TYPE,B,D,1)*rtresub(recty)), &  ! New federal subsidy  !CPPinvest
                  INT(HEATINGTYPEPURCH(Y,TYPE,B,D,2)*rteqsub(recty)), &  ! Replacement federal subsidy  !CPPinvest
                  INT(HEATINGTYPEPURCH(Y,TYPE,B,D,1)*(rtresubn(recty)+FLOAT(EPA111D)*RTRESUB111D(recty))), & !Utility_invest  New non-federal subsidy  !CPPinvest
                  INT(HEATINGTYPEPURCH(Y,TYPE,B,D,2)*(rteqsubn(recty)+FLOAT(EPA111D)*RTEQSUB111D(recty))), & !Utility_invest  Replacement non-federal subsidy  !CPPinvest
                  (INT(SHELLBUILDS(Y,EQTYPE,S,B,D)),S=1,nShellTypes), &
                  (INT((shellinvest (y,RECCL,s,b,d)+shellsubsidy (y,RECCL,s,b,d))*ifix(shellbuilds(Y,EQTYPE,S,B,D))),S=1,nShellTypes), &  !add the subsidy back in for investment includes 111D subsidies
                  (INT(shellsubsidy(y,RECCL,s,b,d)*ifix(shellbuilds(Y,EQTYPE,S,B,D))),S=1,nShellTypes)
                 ELSE  !other equipment
                  WRITE(OUTFILE,987) SR,D,B,FL,RTCLNAME(RECCL),RTTYNAME(RECTY),Y , RSNEFDB1(Y1,RECCL,B,D), RSEEFDB1(Y1,RECCL,B,D),&
                  INT(HEATINGTYPEPURCH(Y,TYPE,B,D,1)),INT(HEATINGTYPEPURCH(Y,TYPE,B,D,2)),&
                  INT(HEATINGTYPEPURCH(Y,TYPE,B,D,1)*(rtrecost(recty)+rtresub(recty)+rtresubn(recty)+FLOAT(EPA111D)*RTRESUB111D(recty))),   & !Utility_invest   Add back in the subsidy, which was taken out earlier ((..1) is new construction (..2) replacements)
                  INT(HEATINGTYPEPURCH(Y,TYPE,B,D,2)*(rtrecost(recty)+rtresub(recty)+rtresubn(recty)+FLOAT(EPA111D)*RTRESUB111D(recty))),   & !Utility_invest   Investment excludes installation costs for retrofit equipment because of way heat pump costs are split between heating & cooling (and replacement versus new)
                  INT(HEATINGTYPEPURCH(Y,TYPE,B,D,1)*rtresub(recty)),&                    !New federal subsidy. For subsidies, account for all components - here new construction, just capital subsidies  !CPPinvest
                  INT(HEATINGTYPEPURCH(Y,TYPE,B,D,2)*rteqsub(recty)),&                    !Replacement federal subsidy. For replacement purchases, include rteqsub which includes subsidies for total installed costs (including labor)  !CPPinvest
                  INT(HEATINGTYPEPURCH(Y,TYPE,B,D,1)*(rtresubn(recty)+FLOAT(EPA111D)*RTRESUB111D(recty))),&                   !Utility_invest  New non-federal subsidy  !CPPinvest
                  INT(HEATINGTYPEPURCH(Y,TYPE,B,D,2)*(rteqsubn(recty)+FLOAT(EPA111D)*RTEQSUB111D(recty))),&                   !Utility_invest  Replacement non-federal subsidy  !CPPinvest
                  0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0
                ENDIF
              ENDIF !Year and census division filter
 140     CONTINUE
 150  CONTINUE

       DO 1151 app=1,NumApps
       DO 1151 D=1,mNumCR-2
       DO 1151 B=1,mNumBldg
       DO 1151 E=1,numtypes(app)
       DO 1151 Y=RECSYear+1,LastYr+(BaseYr-1)
         WRITE(OUTFILE,990) 'LIGHTING',D,B,'EL',AppID(app),appbulbname(app,e),Y,WTLEFFbyAPP(app,Y,B,D),WTLEFFbyAPP(app,Y,B,D),(LTNEEDEDbyAPP(app,Y,E,B,D)), &
          (LTREPbyAPP(app,Y,E,B,D)),LTInvest(app,Y,E,B,D,1),LTInvest(app,Y,E,B,D,2),0,0,LTsubsidy(app,Y,E,B,D,1),LTsubsidy(app,Y,E,B,D,2),0,0,0    !Utility_invest
 1151  CONTINUE

      OUTFILE=FILE_MGR('C','RESDEQP',.FALSE.) !Close the output file

!*******************************************************************
      FN='RESDBOUT.TXT'
      OPEN(23,FILE=FN,FORM='FORMATTED')
!*******************************************************************
!*******************************************************************
!  HEATING EQUIPMENT
!*******************************************************************
      EU = 1             ! SPACE HEATING SECTION OF THE DATA
      SR='HT'
      WRITE(23,985) 'ENDUSE,','CDIV,','BLDG,','FUEL,','EQPCLASS,','YEAR,','EQSTOCK,','CONSUMPTION,','HOUSEHOLDS,','BULBTYPE'
      DO 151 D=1,mNumCR-2
       DO 151 B=1,mNumBldg
         DO 141 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            E=RTCLEQCL(RECCL)

!   FIND THE RECORD NUMBER FOR THE GEOTHERMAL/ GROUND-SOURCE HEAT PUMP FOR HEATING
            IF(RTCLNAME(RECCL).EQ.'GEO_HP') RECCLGHP=RECCL

!   DEFINE THE NAMES CORRESPONDING TO FUEL NUMBERS
            IF (RTCLNAME(RECCL).EQ.'WOOD_HT') THEN
              FL='WD'
            ELSEIF (RTFUEL(RECCL).EQ.4) THEN
              FL='EL'
            ELSEIF (RTFUEL(RECCL).EQ.3) THEN
              FL='GS'
            ELSEIF (RTFUEL(RECCL).EQ.5) THEN
              FL='KS'
            ELSEIF (RTFUEL(RECCL).EQ.2) THEN
              FL='LG'
            ELSEIF (RTFUEL(RECCL).EQ.1) THEN
              FL='DS'
            ELSE
            ENDIF

            DO 141 Y=RECSYear,LastYr+(BaseYr-1)  ! Write DB to LastYr only
             Y1=Y-BaseYr+1
             bb=0.
             IF (RECCL.EQ.1) THEN  !Writes number of households when processing electric space heating (ELEC_RAD)
              WRITE(23,991) SR,D,B,FL,RTCLNAME(RECCL),Y,HEATERS(Y,E,B,D),INT(EQCEQCN(Y1,RECCL,B,D)),INT(EH(Y,B,D)+NH(Y,B,D))
             ELSE
              IF ((RECCL.EQ.3).AND.(B.EQ.1)) bb=BNCHFCT(Y1,1,D)*1000000.  !NG_FA
              IF ((RECCL.EQ.6).AND.(B.EQ.1)) bb=BNCHFCT(Y1,4,D)*1000000.  !LPG_FA
              IF ((RECCL.EQ.7).AND.(B.EQ.1)) bb=BNCHFCT(Y1,3,D)*1000000.  !DIST_FA
                IF (bb+EQCEQCN(Y1,RECCL,B,D).LE.0.) THEN !Set negative consumption equal to zero
			      bb=0.
			      EQCEQCN(Y1,RECCL,B,D)=0.
			    ENDIF
                WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y, &
                  HEATERS(Y,E,B,D),INT(bb+EQCEQCN(Y1,RECCL,B,D))
              IF ((RECCL.EQ.5).AND.(B.EQ.1)) THEN  !KERO_FA
               bb=BNCHFCT(Y1,3,D)  !KeroBench	!kj - Should this be changed? F=3 for distillate + kerosene; F=5 for kerosene (no longer used)
               WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y, &
                  HEATERS(Y,E,B,D),INT(bb*EQCEQCN(Y1,RECCL,B,D))
              ENDIF
             ENDIF
 141     CONTINUE

         FL='GE'
         DO Y=RECSYear,LastYr+(BaseYr-1)
            Y1=Y-BaseYr+1
            WRITE(23,989) SR,D,B,FL,HQ(nHeatClasses+1),Y,HEATERS(Y,RECCLGHP,B,D),INT(GEEQCN(Y1,1,B,D))
         ENDDO
 151  CONTINUE

!*******************************************************************
!  COOLING EQUIPMENT
!*******************************************************************
      EU = 2             ! SPACE COOLING SECTION OF THE DATA
      SR='CL'
      DO 180 D=1,mNumCR-2
       DO 180 B=1,mNumBldg
        DO 170 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            E=RTCLEQCL(RECCL)

!   FIND THE EQ CLASS NUMBER FOR THE GROUND-SOURCE HEAT PUMP FOR COOLING
            IF(RTCLNAME(RECCL).EQ.'GEO_HP') THEN
              EQCGHP=RTCLEQCL(RECCL)
              RECCLGHP=EQCGHP+RTCLEUPT(EU)
            ENDIF

!   DEFINE THE NAMES CORRESPONDING TO FUEL NUMBERS
            IF (RTFUEL(RECCL).EQ.4) THEN
              FL='EL'
            ELSEIF (RTFUEL(RECCL).EQ.3) THEN
              FL='GS'
            ELSEIF (RTFUEL(RECCL).EQ.5) THEN
              FL='KS'
            ELSEIF (RTFUEL(RECCL).EQ.2) THEN
              FL='LG'
            ELSEIF (RTFUEL(RECCL).EQ.1) THEN
              FL='DS'
            ENDIF

!     DO 170 Y=RECSyear,EndYr
      DO 170 Y=RECSYear,LastYr+(BaseYr-1)  !Write DB to LastYr only
        Y1=Y-BaseYr+1
        IF ((D.GT.1).AND.(B.EQ.1).AND.(E.EQ.2)) THEN
!          bb=bnchfct(y1,2,d)*1000000.
        ELSE
          bb=0.0
        ENDIF

        WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y,COOLERS(Y,E,B,D),INT(EQCEQCN(Y1,RECCL,B,D)+bb)
 170    CONTINUE

         FL='GE'
         DO Y=RECSYear,LastYr+(BaseYr-1)
           Y1=Y-BaseYr+1
           WRITE(23,989) SR,D,B,FL,CQ(nCoolClasses+1),Y,COOLERS(Y,EQCGHP,B,D),INT(GEEQCN(Y1,2,B,D))
         ENDDO
 180  CONTINUE

!*******************************************************************
!  CLOTHES WASHERS
!*******************************************************************
      EU = 3             ! CLOTHES WASHER SECTION OF THE DATA
      SR='CW'
      FL='EL'
      DO 171 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      DO 171 D=1,mNumCR-2
       DO 171 B=1,mNumBldg
         DO 171 Y=RECSYear,LastYr+(BaseYr-1)
            Y1=Y-BaseYr+1
               WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y,CLOTHE(Y,B,D),INT(EQCEQCN(Y1,RECCL,B,D))
171  CONTINUE

!*******************************************************************
!  DISHWASHERS
!*******************************************************************
      EU = 4             ! DISHWASHER SECTION OF THE DATA
      SR='DW'
      FL='EL'
      DO 172 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      DO 172 D=1,mNumCR-2
       DO 172 B=1,mNumBldg
         DO 172 Y=RECSYear,LastYr+(BaseYr-1)	!kj - LastYr+(BaseYr-1) had been EndYr
           Y1=Y-BaseYr+1
           WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y,DISHW(Y,B,D),INT(EQCEQCN(Y1,RECCL,B,D))
 172  CONTINUE

!*******************************************************************
!  WATER HEATING EQUIPMENT
!*******************************************************************
      EU = 5             ! WATER HEATING SECTION OF THE DATA
      SR='HW'
      DO 200 D=1,mNumCR-2
       DO 200 B=1,mNumBldg
       DO 190 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            E=RTCLEQCL(RECCL)
            IF (RTFUEL(RECCL).EQ.4) THEN
              FL='EL'
            ELSEIF (RTFUEL(RECCL).EQ.3) THEN
              FL='GS'
            ELSEIF (RTFUEL(RECCL).EQ.5) THEN
              FL='SL'
            ELSEIF (RTFUEL(RECCL).EQ.2) THEN
              FL='LG'
            ELSEIF (RTFUEL(RECCL).EQ.1) THEN
              FL='DS'
            ENDIF
            DO 190 Y=RECSYear,LastYr+(BaseYr-1)
              Y1=Y-BaseYr+1
              WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y,WATERS(Y,E,B,D),INT(EQCEQCN(Y1,RECCL,B,D))
 190     CONTINUE

         FL='SL'
         DUM=0.0
         DO Y=RECSYear,LastYr+(BaseYr-1)
           Y1=Y-BaseYr+1
           WRITE(23,989) SR,D,B,FL,WQ(5),Y,WATERS(Y,5,B,D),INT(SLEQCN(Y1,1,B,D))
         ENDDO
 200     CONTINUE

!*******************************************************************
!  COOKING EQUIPMENT
!*******************************************************************
      EU = 6             ! COOKING SECTION OF THE DATA
      SR='CK'
      DO 220 D=1,mNumCR-2
       DO 220 B=1,mNumBldg
        DO 220 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            E=RTCLEQCL(RECCL)
            IF (RTFUEL(RECCL).EQ.4) THEN
              FL='EL'
            ELSEIF (RTFUEL(RECCL).EQ.3) THEN
              FL='GS'
            ELSEIF (RTFUEL(RECCL).EQ.5) THEN
              FL='KS'
            ELSEIF (RTFUEL(RECCL).EQ.2) THEN
              FL='LG'
            ELSEIF (RTFUEL(RECCL).EQ.1) THEN
              FL='DS'
            ENDIF
          DO 220 Y=RECSYear,LastYr+(BaseYr-1)
            Y1=Y-BaseYr+1
            WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y,COOKS(Y,E,B,D),int(EQCEQCN(Y1,RECCL,B,D))
 220  CONTINUE

!*******************************************************************
!  CLOTHES DRYERS
!*******************************************************************
      EU = 7             ! CLOTHES DRYER SECTION OF THE DATA
      SR='DR'
      DO 240 D=1,mNumCR-2
       DO 240 B=1,mNumBldg
        DO 240 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            E=RTCLEQCL(RECCL)
            IF (RTFUEL(RECCL).EQ.4) THEN
              FL='EL'
            ELSEIF (RTFUEL(RECCL).EQ.3) THEN
              FL='GS'
            ELSEIF (RTFUEL(RECCL).EQ.5) THEN
              FL='KS'
            ELSEIF (RTFUEL(RECCL).EQ.2) THEN
              FL='LG'
            ELSEIF (RTFUEL(RECCL).EQ.1) THEN
              FL='DS'
            ENDIF
          DO 240 Y=RECSYear,LastYr+(BaseYr-1)
            Y1=Y-BaseYr+1
            WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y,DRYERS(Y,E,B,D),int(EQCEQCN(Y1,RECCL,B,D))
 240  CONTINUE

!*******************************************************************
!  REFRIGERATORS
!*******************************************************************
      EU = 8             ! FOOD REFRIGERATION SECTION OF THE DATA
      SR='RF'
      FL='EL'
      DO 260 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      DO 260 D=1,mNumCR-2
       DO 260 B=1,mNumBldg
        DO 260 Y=RECSYear,LastYr+(BaseYr-1)
            Y1=Y-BaseYr+1
              WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y, &
           FRIGS(Y,B,D),int(EQCEQCN(Y1,RECCL,B,D))
 260  CONTINUE

!*******************************************************************
!  STANDALONE FREEZERS
!*******************************************************************
      EU = 9             ! FOOD FREEZING SECTION OF THE DATA
      SR='FZ'
      FL='EL'
      DO 280 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      DO 280 D=1,mNumCR-2
       DO 280 B=1,mNumBldg
         DO 280 Y=RECSYear,LastYr+(BaseYr-1)
            Y1=Y-BaseYr+1
              WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y, &
           FREEZE(Y,B,D),int(EQCEQCN(Y1,RECCL,B,D))
 280  CONTINUE

!*******************************************************************
!  LIGHTING - All Applications
!*******************************************************************
      SR='LT'
      FL='EL'

      DO 301 app =1,NumApps
        !adding this loop across types splits lighting applications into bulb types for stocks
        ! also need to accumulate into temp below to split by types
        DO 301 type =1,numtypes(app)
       DO 301 D=1,mNumCR-2
        DO 301 B=1,mNumBldg
         DO 301 Y=RECSYear,LastYr+(BaseYr-1)
            Y1=Y-BaseYr+1
                  TEMP=0.
                  DO BIN=1,MAXBINS
                   TEMP=TEMP+ltstock(app,y,type,B,D,BIN)
                  ENDDO
              IF(type.EQ.1) THEN
                WRITE(23,984) SR,D,B,FL,appid(app),Y,TEMP,int(LTEQCN(Y1,app,B,D)),0,appbulbname(app,type)
              ELSE
                WRITE(23,984) SR,D,B,FL,appid(app),Y,TEMP,0,0,appbulbname(app,type)
              ENDIF
 301  CONTINUE

!*******************************************************************
!  FURNACE FANS & BOILER PUMPS
!*******************************************************************
      SR='FF'
      FL='EL'
       DO 302 D=1,mNumCR-2
        DO 302 B=1,mNumBldg
         DO 302 Y=RECSYear,LastYr+(BaseYr-1)
            Y1=Y-BaseYr+1
              WRITE(23,989) SR,D,B,FL,SR,Y,FANEQP(Y,B,D),INT(FANEQCN(Y1,1,B,D))
 302  CONTINUE

!*******************************************************************
!  TELEVISIONS
!*******************************************************************
      SR='TVS'
      FL='EL'
       DO 305 D=1,mNumCR-2
        DO 305 B=1,mNumBldg
         DO 305 Y=RECSYear,LastYr+(BaseYr-1)
            Y1=Y-BaseYr+1
            WRITE(23,989) SR,D,B,FL,'TV&R',Y,TVSEQP(Y,B,D),INT(TVSEQCN(Y1,1,B,D))
 305  CONTINUE

!*******************************************************************
!  SET-TOP BOXES
!*******************************************************************
      SR='STB'
      FL='EL'
       DO 306 D=1,mNumCR-2
        DO 306 B=1,mNumBldg
         DO 306 Y=RECSYear,LastYr+(BaseYr-1)
           Y1=Y-BaseYr+1
           WRITE(23,989) SR,D,B,FL,'TV&R',Y,STBEQP(Y,B,D),INT(STBEQCN(Y1,1,B,D))
 306  CONTINUE

!*******************************************************************
!  HOME THEATER SYSTEMS
!*******************************************************************
      SR='HTS'
      FL='EL'
       DO 307 D=1,mNumCR-2
        DO 307 B=1,mNumBldg
         DO 307 Y=RECSYear,LastYr+(BaseYr-1)
           Y1=Y-BaseYr+1
           WRITE(23,989) SR,D,B,FL,'TV&R',Y,HTSEQP(Y,B,D),INT(HTSEQCN(Y1,1,B,D))
 307  CONTINUE

!*******************************************************************
!  OVER-THE-TOP STREAMING DEVICES
!*******************************************************************
      SR='OTT'  !MELs21
      FL='EL'
       DO 308 D=1,mNumCR-2
        DO 308 B=1,mNumBldg
         DO 308 Y=RECSYear,LastYr+(BaseYr-1)
           Y1=Y-BaseYr+1
           WRITE(23,989) SR,D,B,FL,'TV&R',Y,OTTEQP(Y,B,D),INT(OTTEQCN(Y1,1,B,D))  !MELs21
 308  CONTINUE

!*******************************************************************
!  VIDEO GAME CONSOLES
!*******************************************************************
      SR='VGC'
      FL='EL'
       DO 309 D=1,mNumCR-2
        DO 309 B=1,mNumBldg
         DO 309 Y=RECSYear,LastYr+(BaseYr-1)
           Y1=Y-BaseYr+1
           WRITE(23,989) SR,D,B,FL,'TV&R',Y,VGCEQP(Y,B,D),INT(VGCEQCN(Y1,1,B,D))
 309  CONTINUE

!*******************************************************************
!  DESKTOP PCs
!*******************************************************************
      SR='DPC'
      FL='EL'
       DO 312 D=1,mNumCR-2
        DO 312 B=1,mNumBldg
         DO 312 Y=RECSYear,LastYr+(BaseYr-1)
           Y1=Y-BaseYr+1
           WRITE(23,989) SR,D,B,FL,'PC&R',Y,DPCEQP(Y,B,D),INT(DPCEQCN(Y1,1,B,D))
 312  CONTINUE

!*******************************************************************
!  LAPTOP PCs
!*******************************************************************
      SR='LPC'
      FL='EL'
       DO 313 D=1,mNumCR-2
        DO 313 B=1,mNumBldg
         DO 313 Y=RECSYear,LastYr+(BaseYr-1)
           Y1=Y-BaseYr+1
           WRITE(23,989) SR,D,B,FL,'PC&R',Y,LPCEQP(Y,B,D),INT(LPCEQCN(Y1,1,B,D))
 313  CONTINUE

!*******************************************************************
!  COMPUTER MONITORS
!*******************************************************************
      SR='MON'
      FL='EL'
       DO 314 D=1,mNumCR-2
        DO 314 B=1,mNumBldg
         DO 314 Y=RECSYear,LastYr+(BaseYr-1)
           Y1=Y-BaseYr+1
           WRITE(23,989) SR,D,B,FL,'PC&R',Y,MONEQP(Y,B,D),INT(MONEQCN(Y1,1,B,D))
 314  CONTINUE

!*******************************************************************
!  NETWORKING EQUIPMENT
!*******************************************************************
      SR='NET'
      FL='EL'
      DO 315 D=1,mNumCR-2
       DO 315 B=1,mNumBldg
         DO 315 Y=RECSYear,LastYr+(BaseYr-1)
           Y1=Y-BaseYr+1
           WRITE(23,989) SR,D,B,FL,'PC&R',Y,NETEQP(Y,B,D),INT(NETEQCN(Y1,1,B,D))
 315  CONTINUE

!*******************************************************************
!  NON-PC RECHARGEABLES
!*******************************************************************
      SR='BAT'
      FL='EL'
      DO 320 D=1,mNumCR-2
       DO 320 B=1,mNumBldg
        DO 320 Y=RECSYear,LastYr+(BaseYr-1)
          Y1=Y-BaseYr+1
          WRITE(23,989) SR,D,B,FL,'MEL',Y, BATEQP(Y,B,D),INT(BATEQCN(Y1,1,B,D))
 320  CONTINUE

!*******************************************************************
!  CEILING FANS
!*******************************************************************
      SR='CFN'
      FL='EL'
      DO 321 D=1,mNumCR-2
       DO 321 B=1,mNumBldg
        DO 321 Y=RECSYear,LastYr+(BaseYr-1)
          Y1=Y-BaseYr+1
          WRITE(23,989) SR,D,B,FL,'MEL',Y, CFNEQP(Y,B,D),INT(CFNEQCN(Y1,1,B,D))
 321  CONTINUE

!*******************************************************************
!  COFFEE MAKERS
!*******************************************************************
      SR='COF'
      FL='EL'
      DO 322 D=1,mNumCR-2
       DO 322 B=1,mNumBldg
         DO 322 Y=RECSYear,LastYr+(BaseYr-1)
           Y1=Y-BaseYr+1
           WRITE(23,989) SR,D,B,FL,'MEL',Y, COFEQP(Y,B,D),INT(COFEQCN(Y1,1,B,D))
 322  CONTINUE

!*******************************************************************
!  DEHUMIDIFIERS
!*******************************************************************
      SR='DEH'
      FL='EL'
      DO 323 D=1,mNumCR-2
       DO 323 B=1,mNumBldg
         DO 323 Y=RECSYear,LastYr+(BaseYr-1)
           Y1=Y-BaseYr+1
           WRITE(23,989) SR,D,B,FL,'MEL',Y, DEHEQP(Y,B,D),INT(DEHEQCN(Y1,1,B,D))
 323  CONTINUE

!*******************************************************************
!  MICROWAVES
!*******************************************************************
      SR='MCO'
      FL='EL'
      DO 324 D=1,mNumCR-2
       DO 324 B=1,mNumBldg
         DO 324 Y=RECSYear,LastYr+(BaseYr-1)
           Y1=Y-BaseYr+1
           WRITE(23,989) SR,D,B,FL,'MEL',Y, MCOEQP(Y,B,D),INT(MCOEQCN(Y1,1,B,D))
 324  CONTINUE

!*******************************************************************
!  POOL PUMPS  !MELs21
!*******************************************************************
      SR='PLP'  !MELs21
      FL='EL'
      DO 325 D=1,mNumCR-2
       DO 325 B=1,mNumBldg
         DO 325 Y=RECSYear,LastYr+(BaseYr-1)
           Y1=Y-BaseYr+1
           WRITE(23,989) SR,D,B,FL,'MEL',Y, PLPEQP(Y,B,D),INT(PLPEQCN(Y1,1,B,D))  !MELs21
 325  CONTINUE

!*******************************************************************  !MELs21
!  POOL HEATERS  !MELs21
!*******************************************************************  !MELs21
      SR='PLH'  !MELs21
      FL='EL'  !MELs21
      DO 326 D=1,mNumCR-2  !MELs21
       DO 326 B=1,mNumBldg  !MELs21
         DO 326 Y=RECSYear,LastYr+(BaseYr-1)  !MELs21
           Y1=Y-BaseYr+1  !MELs21
           WRITE(23,989) SR,D,B,FL,'MEL',Y, PLHEQP(Y,B,D),INT(PLHEQCN(Y1,1,B,D))  !MELs21
 326  CONTINUE  !MELs21

!*******************************************************************
!  SECURITY SYSTEMS
!*******************************************************************
      SR='SEC'
      FL='EL'
      DO 327 D=1,mNumCR-2
       DO 327 B=1,mNumBldg
         DO 327 Y=RECSYear,LastYr+(BaseYr-1)
           Y1=Y-BaseYr+1
           WRITE(23,989) SR,D,B,FL,'MEL',Y, SECEQP(Y,B,D),INT(SECEQCN(Y1,1,B,D))
 327  CONTINUE

!*******************************************************************
!  PORTABLE ELECTRIC SPAS
!*******************************************************************
      SR='SPA'
      FL='EL'
      DO 328 D=1,mNumCR-2
       DO 328 B=1,mNumBldg
         DO 328 Y=RECSYear,LastYr+(BaseYr-1)
           Y1=Y-BaseYr+1
           WRITE(23,989) SR,D,B,FL,'MEL',Y, SPAEQP(Y,B,D),INT(SPAEQCN(Y1,1,B,D))
 328  CONTINUE

!*******************************************************************
!  WINE COOLERS
!*******************************************************************
      SR='WCL'
      FL='EL'
      DO 329 D=1,mNumCR-2
       DO 329 B=1,mNumBldg
         DO 329 Y=RECSYear,LastYr+(BaseYr-1)
           Y1=Y-BaseYr+1
           WRITE(23,989) SR,D,B,FL,'MEL',Y, WCLEQP(Y,B,D),INT(WCLEQCN(Y1,1,B,D))
 329  CONTINUE

!*******************************************************************  !MELs21
!  SMART SPEAKERS  !MELs21
!*******************************************************************  !MELs21
      SR='SPK'  !MELs21
      FL='EL'  !MELs21
      DO 330 D=1,mNumCR-2  !MELs21
       DO 330 B=1,mNumBldg  !MELs21
         DO 330 Y=RECSYear,LastYr+(BaseYr-1)  !MELs21
           Y1=Y-BaseYr+1  !MELs21
           WRITE(23,989) SR,D,B,FL,'MEL',Y, SPKEQP(Y,B,D),INT(SPKEQCN(Y1,1,B,D))  !MELs21
 330  CONTINUE  !MELs21

!*******************************************************************  !MELs21
!  SMARTPHONES  !MELs21
!*******************************************************************  !MELs21
      SR='PHN'  !MELs21
      FL='EL'  !MELs21
      DO 331 D=1,mNumCR-2  !MELs21
       DO 331 B=1,mNumBldg  !MELs21
         DO 331 Y=RECSYear,LastYr+(BaseYr-1)  !MELs21
           Y1=Y-BaseYr+1  !MELs21
           WRITE(23,989) SR,D,B,FL,'MEL',Y, PHNEQP(Y,B,D),INT(PHNEQCN(Y1,1,B,D))  !MELs21
 331  CONTINUE  !MELs21

!*******************************************************************  !MELs21
!  TABLETS  !MELs21
!*******************************************************************  !MELs21
      SR='TAB'  !MELs21
      FL='EL'  !MELs21
      DO 332 D=1,mNumCR-2  !MELs21
       DO 332 B=1,mNumBldg  !MELs21
         DO 332 Y=RECSYear,LastYr+(BaseYr-1)  !MELs21
           Y1=Y-BaseYr+1  !MELs21
           WRITE(23,989) SR,D,B,FL,'MEL',Y, TABEQP(Y,B,D),INT(TABEQCN(Y1,1,B,D))  !MELs21
 332  CONTINUE  !MELs21

!*******************************************************************  !MELs21
!  SMALL KITCHEN APPLIANCES  !MELs21
!*******************************************************************  !MELs21
      SR='KIT'  !MELs21
      FL='EL'  !MELs21
      DO 333 D=1,mNumCR-2  !MELs21
       DO 333 B=1,mNumBldg  !MELs21
         DO 333 Y=RECSYear,LastYr+(BaseYr-1)  !MELs21
           Y1=Y-BaseYr+1  !MELs21
           WRITE(23,989) SR,D,B,FL,'MEL',Y, KITEQP(Y,B,D),INT(KITEQCN(Y1,1,B,D))  !MELs21
 333  CONTINUE  !MELs21

!*******************************************************************
!  OTHER ELECTRIC APPLIANCES NOT EXPLICITLY MODELED
!*******************************************************************
      SR='EO'
      FL='EL'
      DO 334 D=1,mNumCR-2
       DO 334 B=1,mNumBldg
         DO 334 Y=RECSYear,LastYr+(BaseYr-1)
            Y1=Y-BaseYr+1
            bb=0.0
            WRITE(23,989) SR,D,B,FL,'MEL',Y,EAEQP(Y,B,D),INT(EAEQCN(Y1,1,B,D))  !DGreport - EAEQCN no longer uses 65% TrillsOwnUse reduction
 334  CONTINUE

!*******************************************************************
!  OTHER APPLIANCES IN FUELS OTHER THAN ELECTRICITY
!*******************************************************************
      SR='OA'
      DO 340 D=1,mNumCR-2
        DO 340 B=1,mNumBldg
          DO 340 E=1,3
            DO 340 Y=RECSYear,LastYr+(BaseYr-1)
              Y1=Y-BaseYr+1
              IF (E .EQ. 1) THEN
                FL='GS'
                f=1
                bb=0.0
              ELSEIF (E .EQ. 2) THEN
                FL='LG'
                f=4
                bb=0.0
              ELSEIF (E .EQ. 3) THEN
                FL='DS'
                f=3
                bb=0.0
              ENDIF
              WRITE(23,989) SR,D,B,FL,FL,Y,APLEQP(Y,B,D,E),INT(bb+APEQCN(Y1,E,B,D))
      340  CONTINUE

!*******************************************************************
!  WRITE OUT SECONDARY HEATING
!*******************************************************************
      SR='SH'
      DO 360 D=1,mNumCR-2
        DO 360 B=1,mNumBldg
          DO 360 E=1,3
            DO 360 Y=RECSYear,LastYr+(BaseYr-1)
              Y1=Y-BaseYr+1
              IF (E .EQ. 1) THEN
                FL='GS'
                bb=0.0 ! bnchfct(y1,e,d)
              ELSEIF (E .EQ. 2) THEN
                FL='EL'
                bb=0.0 ! bnchfct(y1,e,d)
              ELSEIF (E .EQ. 3) THEN
                FL='DS'
                bb=0.0
              ENDIF
              WRITE(23,989) SR,D,B,FL,FL,Y,SHTEQP(Y,B,D,E),INT(bb+sheQCN(Y1,E,B,D))
      360  CONTINUE

      DO 361 D=1,mNumCR-2
        DO 361 B=1,mNumBldg
          DO 361 E=4,7	!kj - remove 5 and 6 from write statement because kerosene and coal no longer used; may require removing two placeholder columns from RSMISC.txt secondary heating inputs
            DO 361 Y=RECSYear,LastYr+(BaseYr-1)
              Y1=Y-BaseYr+1
              IF (E .EQ. 4) THEN
                FL='LG'
                bb=1.0 ! bnchfct(y1,e,d)
              ELSEIF (E .EQ. 5) THEN	!kj - remove from write statement (but maintain correct fuel numbering)
                FL='KS'
                bb=bnchfct(y1,e,d)
              ELSEIF (E .EQ. 6) THEN	!kj - remove from write statement (but maintain correct fuel numbering)
                FL='CL'
                bb=bnchfct(y1,e,d)
              ELSEIF (E .EQ. 7) THEN
                FL='WD'
                bb=1.0
              ENDIF
              WRITE(23,989) SR,D,B,FL,FL,Y,SHTEQP(Y,B,D,E),INT(bb*sheQCN(Y1,E,B,D))
      361  CONTINUE

!**********************************************************************
!      WRITE OUT HOUSING STARTS
!**********************************************************************
      SR='HS'
      DO 370 D=1,mNumCR-2
        DO 370 B=1,mNumBldg
          DO 370 Y=RECSYear,LastYr+(BaseYr-1)
            WRITE(23,989) SR,D,B,'0','0',Y,HSEADD(Y,B,D),zero
      370  CONTINUE

!**********************************************************************
!      WRITE OUT SQUARE FOOTAGE
!**********************************************************************
! adjust database output
        SR='SQ'
        DO 375 D=1,mNumCR-2
         DO 375 B=1,mNumBldg
          DO 375 Y=RECSYear,LastYr+(BaseYr-1)
          WRITE(23,991) SR,D,B,'0','0',Y,STOCKSQRFOOT(Y,B,D),INT(SQRFOOT(Y,B,D)), &
            INT(((EH(Y,B,D)+NH(Y,B,D))*STOCKSQRFOOT(Y,B,D))/10**6)
 375   CONTINUE

!**********************************************************************
!      WRITE OUT FUEL SWITCHING TO TECHNOLOGIES
!**********************************************************************
        EU = 1
        SR='ST'
        DO 380 D=1,mNumCR-2
         DO 380 B=1,1
          DO 380 Y=RECSYear,LastYr+(BaseYr-1)
           DO 380 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            E=RTCLEQCL(RECCL)
!   DEFINE THE NAMES CORRESPONDING TO FUEL NUMBERS
            IF (RTCLNAME(RECCL).EQ.'WOOD_HT') THEN
              FL='WD'
            ELSEIF (RTFUEL(RECCL).EQ.4) THEN
              FL='EL'
            ELSEIF (RTFUEL(RECCL).EQ.3) THEN
              FL='GS'
            ELSEIF (RTFUEL(RECCL).EQ.5) THEN
              FL='KS'
            ELSEIF (RTFUEL(RECCL).EQ.2) THEN
              FL='LG'
            ELSEIF (RTFUEL(RECCL).EQ.1) THEN
              FL='DS'
            ELSE
            ENDIF

          WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y,SWTOTAL(Y,E,D),zero
 380   CONTINUE

!**********************************************************************
!      WRITE OUT FUEL SWITCHING FROM TECHNOLOGIES
!**********************************************************************
        EU = 1
        SR='SF'
        DO 390 D=1,mNumCR-2
         DO 390 B=1,1
          DO 390 Y=RECSYear,LastYr+(BaseYr-1)
           DO 390 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            E=RTCLEQCL(RECCL)
!   DEFINE THE NAMES CORRESPONDING TO FUEL NUMBERS
            IF (RTCLNAME(RECCL).EQ.'WOOD_HT') THEN
              FL='WD'
            ELSEIF (RTFUEL(RECCL).EQ.4) THEN
              FL='EL'
            ELSEIF (RTFUEL(RECCL).EQ.3) THEN
              FL='GS'
            ELSEIF (RTFUEL(RECCL).EQ.5) THEN
              FL='KS'
            ELSEIF (RTFUEL(RECCL).EQ.2) THEN
              FL='LG'
            ELSEIF (RTFUEL(RECCL).EQ.1) THEN
              FL='DS'
            ELSE
            ENDIF

          WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y,SWFTOTAL(Y,E,D),zero
 390   CONTINUE

!*******************************************************************
!  WRITE OUT FUEL PRICES
!*******************************************************************
! adjust database output
!      WRITE NEMS PRICES IN INTERNAL NEMS DOLLAR YEAR (1987$/MMBTU)
      SR='FP'
      DO 400 D=1,mNumCR
!      DO 400 Y=1,mNumYr
      DO 400 Y=1,LastYr  !Only WRITE results to LastYr
          WRITE(23,998) SR,D,'1','DS','0',Y+(BaseYr-1),PDSRS(D,Y),zero
          WRITE(23,998) SR,D,'1','LG','0',Y+(BaseYr-1),PLGRS(D,Y),zero
          WRITE(23,998) SR,D,'1','NG','0',Y+(BaseYr-1),PNGRS(D,Y),zero
          WRITE(23,998) SR,D,'1','EL','0',Y+(BaseYr-1),PELRS(D,Y),zero
          WRITE(23,998) SR,D,'1','KS','0',Y+(BaseYr-1),PKSRS(D,Y),zero
          WRITE(23,998) SR,D,'1','CL','0',Y+(BaseYr-1),PCLRS(D,Y),zero
 400  CONTINUE

!*******************************************************************
!  WRITE OUT RESIDENTIAL SHELL INDICES
!*******************************************************************
!      WRITE shell heating indices by year
        DO 410 Y=RECSYear-(BaseYr-1),LastYr
         WRITE(23,992) 'HSHE',11,1,0,0,Y+(BaseYr-1),HSHELL1(Y),0,0    !existing
         WRITE(23,992) 'HSHN',11,1,0,0,Y+(BaseYr-1),HSHELL2(Y),0,0    !new construction
         WRITE(23,992) 'HSHA',11,1,0,0,Y+(BaseYr-1),HSHELL3(Y),0,0    !average
 410  CONTINUE

!      WRITE shell cooling indices by year
        DO 420 Y=RECSYear-(BaseYr-1),LastYr
         WRITE(23,992) 'CSHE',11,1,0,0,Y+(BaseYr-1),CSHELL1(Y),0,0    !existing
         WRITE(23,992) 'CSHN',11,1,0,0,Y+(BaseYr-1),CSHELL2(Y),0,0    !new construction
         WRITE(23,992) 'CSHA',11,1,0,0,Y+(BaseYr-1),CSHELL3(Y),0,0    !average
 420  CONTINUE

      Close(23)

      END SUBROUTINE RESDBOUT


!*******************************************************************
!  CALCULATION OF SUBSIDIES FOR EPA 111D ENERGY EFFICIENCY REBATES
!*******************************************************************
      SUBROUTINE CALC111D

!     EPA111D=1 is scedes switch to turn national utility energy efficiency subsidies and modeling on (primarily for modeling EPA's Clean Power Plan)
!     AB32SW=1 is scedes switch to turn CA energy efficiency modeling on

      IMPLICIT NONE
!      INTEGER MaxApps, MaxTypes, MaxBins! These are used in looping of report variables - These are set near top of code
!      PARAMETER (MaxApps=4)  !Maximum number of applications
!      PARAMETER (MaxTypes=4) !Maximum number of bulb types within an application
      INTEGER app
      INTEGER Y, B, E, D, F, Y1, NT
      INTEGER EU,RECCL,EQC,EQTYPE,TYPE,RECTY,S

!      QELRS in trillion Btu (rather than quadrillion Btu), BASELINEBKWH in billions kWh
!      BASELINEBKWH(d,y)=(QELRS(D,Y)/3412.)*10**3

         Y=CurCalYr
         !Y1=Y-BaseYr+1
         Y1=CurIYr

         IF(CurCalYr .LT. 2017) THEN  !comment this block out out so savings can be calculated in years prior to 2017 for EE runs?	!kj - should the 2017 be changed?
          DO D=1,mNumCR-2
           SAVE111RES(D,Y1)=0. !Residential sector savings in billions of kilowatthours
           COST111RES(D,Y1)=0. !Residential costs billions of 1987$
          ENDDO
         RETURN  ! DON'T RETURN DURING TESTING MODE
         ENDIF

!        Calculate savings and initialize cost to zero
         DO D=1,mNumCR-2
           SAVE111RES(D,Y1)=BASELINEBKWH(D,Y1)-((RSFLCN(Y1,2,D)-(TrillsOwnUse(Y1,D,1)+TrillsOwnUse(Y1,D,2)+TrillsOwnUse(Y1,D,3))/1000.)/3412.)*10**3  !Subtract all onsite own-use generation; BASELINEBKWH converted from QELRS (quads purchased electricity from grid)  !DGreport
           COST111RES(D,Y1)=0.
         ENDDO

       DO 150 D=1,mNumCR-2
         DO 150 B=1,mNumBldg
           DO 140 EU = 1,9	!kj - replace 9 with variable for last end use number
             TYPE = RTTYPECT(EU)
             DO 140 RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
               !CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CurCalYr
               IF((Y.GE.RTINITYR(RECTY).AND. &
                Y.LE.RTLASTYR(RECTY)).AND.(RTCENDIV(RECTY).EQ.D)) THEN
                 TYPE = TYPE + 1
                 EQTYPE=RTEQTYPE(RECTY)
                 EQC=RTTYEQCL(RECTY)
                 RECCL=RTCLEUPT(EU)+EQC
                 IF(RTFUEL(RECCL).EQ.4) THEN  !electricity subsidies only
                   IF(EU.EQ.1) THEN !Calculate shell investment and subsidies with heating system records only
                     COST111RES(D,Y1)=COST111RES(D,Y1) + FLOAT(EPA111D)*&          !EEcosts - costs only accumulate in CPP runs
                      ((HEATINGTYPEPURCH(Y,TYPE,B,D,1)*rtresub111D(recty))  &       !new construction subsidies (..1) are from equipment only
                      +(HEATINGTYPEPURCH(Y,TYPE,B,D,2)*rteqsub111D(recty)))         !replacement purchases (..2) include subsidy for installation costs
                     DO S=1,nShellTypes
                       COST111RES(D,Y1)=COST111RES(D,Y1) + FLOAT(EPA111D)*&   !EEcosts - costs only accumulate in CPP runs
                        shellsubsidy111D(y,RECCL,s,b,d)*(shellbuilds(Y,TYPE,S,B,D))
                     ENDDO
                   ELSE  !other equipment
                     COST111RES(D,Y1)=COST111RES(D,Y1) + FLOAT(EPA111D)*&          !EEcosts - costs only accumulate in CPP runs
                      ((HEATINGTYPEPURCH(Y,TYPE,B,D,1)*rtresub111D(recty))+  &
                      (HEATINGTYPEPURCH(Y,TYPE,B,D,2)*rteqsub111D(recty)))
                   ENDIF
                 ENDIF  ! fuel is electricity
               ENDIF !Year and Census division filter
           140  CONTINUE
       150  CONTINUE

       !Add Lighting Subsidies (note, lighting bulb costs are converted into RTEKDOLLARYR based on RLGTDOLLARYR)
       DO 151 app=1,NumApps
         DO 151 D=1,mNumCR-2
           DO 151 B=1,mNumBldg
             DO 151 E=1,numtypes(app)
               COST111RES(D,Y1)=COST111RES(D,Y1)+ FLOAT(EPA111D)*(LTsubsidy(app,Y,E,B,d,1)+LTsubsidy(app,Y,E,B,d,2))  !EEcosts - costs only accumulate in CPP runs
       151  CONTINUE

         ! Before exit convert to billions of 1987$ & multiply incentive payments for approximate direct and indirect costs (slightly higher costs than EIA-861 for 2012)
         DO D=1,mNumCR-2
           COST111RES(D,Y1)= 1.5*(COST111RES(D,Y1)*MC_JPGDP(-2)/MC_JPGDP(RTEKDOLLARYR-BaseYr+1) )/10**9  ! scale the incentive payment costs here to account for program administration	!kj
         ENDDO

 !    Add subsidies for distributed generation and deflate from iGenCapCostYr (generally not the same as rsmeqp) as well as convert from $mill to $bill
      DO 152 D=1,mNumCR-2
        DO 152 NT=1, nTek
          COST111RES(D,Y1)=COST111RES(D,Y1)+ FLOAT(EPA111D)*x111dRenSub(y1,d,nt)*( MC_JPGDP(-2)/MC_JPGDP(iGenCapCostYr-BaseYr+1) )/1000.   !111dren  !EEcosts - costs only accumulate in CPP runs
      152  CONTINUE

      WRITE(9,*)'111d year, division, cost (bill 87$), savings (bkWh)'
      !Print exactly what gets passed on for integrated debugging only
      DO D=1,mNumCR-2
        WRITE(9,160) Y, D, COST111RES(D,Y1), SAVE111RES(D,Y1)
      ENDDO
      160  FORMAT(2I5,2f12.5)

END SUBROUTINE CALC111D


!======================================================================
! Project distributed generation penetration
!
!  INCLUDES USER SPECIFIED NUMBER OF GENERAL TECHNOLOGIES:
!  -- CAN MODEL NON-OVERLAPPING VINTAGE RANGES OR INDIVIDUAL YEARS
!  -- CAN INCLUDE A USER-SPECIFIED NUMBER OF TECHNOLOGIES
!======================================================================
SUBROUTINE RDISTGEN

  IMPLICIT NONE

  !*******************************************
  !   NEMS VARIABLES FROM RESD AND LDSM/EMM
  !*******************************************
  INTEGER MaxNiche
  PARAMETER (MaxNiche=16) !Maximum number of distributed generation niches per census division (i.e., maximum iNiche) from RSGENTK.txt !DGniches
  REAL*4 xExistPen

  !----LOCAL VARIABLES AND PARAMETERS INTERNAL TO RDISTGEN

   !DGrate variables for switching between retail space cooling end-use electricity rates and weighted marginal/retail electricity rates for solar PV calculations
    LOGICAL DGrateBlend  !DGrate - switch to turn on blending of retail and wholesale electricity rate
    INTEGER DGrateYr  !DGrate - first year of electricity rate change
    REAL*4 DGmargWt(MNUMCR)  !DGrate - CD-level weight for marginal/ wholesale electricity rate
    REAL*4 DGretWt(MNUMCR)  !DGrate - CD-level weight for retail electricity rate

   !DATA FOR PAYBACK COMPUTATION
    INTEGER iPayback(30),iSimplePayback	!kj - iSimplePayback variable not used?
    REAL*4  xMaxPen,xSimplePayback,xPen,xTemp,xTempHH,xTest

    !DATA FOR INTERCONNECTION LIMITATION
    REAL*4  XINX(mNumCR-2),xInxDecay(mNumCR-2,mNumYr)
    INTEGER xInxFY, xInxLY

    !DATA FOR 30-YEAR CASH-FLOW MODEL CALCULATION
    REAL*4 XOUTLAY(30), XINTAMT(30), XPRIN(30)
    REAL*4 XLOANBAL(30), XDEPR(30)
    REAL*4 XFUELCOST(30), XTAXDEDUCT(30)
    REAL*4 XMAINTCOST(30), XTOTALCOST(30)
    REAL*4 XVALESAVE(30),xKWH(30)
    REAL*4 XNETCASHFLOW(30), XPAYMENT, XDOWNPAY
    REAL*4 XCUMNETFLOW(35) !add positions for "look ahead"
    REAL*4 XINTRATE, XTERM, XINFLATION, XLIFE, XDOWNPAYPCT
    REAL*4 XTAXCREDIT(30), XTAXRATE
    REAL*4 xEqCost, xTaxCreditPct, XTAXCREDITMAxKW, XTAXCREDITMAX, XBaseYrFUELCOST
    REAL*4 XMAINTCOSTBASE, XVALESAVEBASE, xDegradation
    REAL*4 xAnnualKWh,XGASINPUT,XWATERHTGMMBTU
    REAL*4 XBTUWASTEHEAT,XEXCESSKWH,xElecAvgUEC
    REAL*4 xSalestoGridPR, xRetailElecPR, xSizefromTaxOptim
    REAL*4 xUnits, xTrills, xCapacity, xTrillsOwnUse, xfuelusage, xhwbtu, xInvest

!  FOR RPS MODELING
       REAL*4 xRetailElecPRnoRPS, xSalestoGridPrnoRPS, xRPS(nTek,mNumYr)
       REAL*4 xRetailElecPRadjRPS, xSalestoGridPradjRPS !for Markey 2009 bill credit adjustment features
       INTEGER iRPSStartYear, iRPSPhaseOutYear, iRPSGrandFatherYear, iCALYR
       INTEGER iNumRPSCreditYrs(nTek,mNumYr), iNumYrsatRPSBaseRate(nTek,mNumYr), iTemp1, iTemp2
       ! For calculating credit factors to pass to Electric Generation Model
       REAL*4 xPVGenAdded(mNumCR,mNumYr),xWindGenAdded(mNumCR,mNumYr)

       REAL*4 xCompCredit, xCompGen, xCredit, xBonus
       REAL*4 XVALESAVEBASEnoRPS, XVALESAVEBASEadjRPS

!  TECHNOLOGY-SPECIFIC DATA FROM INPUT FILE
       REAL*4 xDegrad(nTek,mNumYr),xElEff(nTek,mNumYr)
       REAL*4 xEqLife(nTek,mNumYr),xWhRecovery(nTek,mNumYr)
       REAL*4 xInstCost(nTek,mNumYr),xCapCost(nTek,mNumYr)
       REAL*4 xMaintCst(nTek,mNumYr),xAvail(nTek,mNumYr)
       REAL*4 xTxCrPct(nTek,mNumYr),xTXCrMaxPerKW(nTek,mNumYr),xTXCrMaxPerSys(nTek,mNumYr)

       REAL*4 xTxCrPct_Div(mNumYr,mNumCR,nTek)                                            !111dren
       REAL*4 xKW(nTek,mNumYr),xOperHours(nTek),xLossFac(nTek,mNumYr)
       REAL*4 xIntervalCst(nTek,mNumYr)
       REAL*4 xAlpha,xPenParm,xExogPen(mNumYr,mNumCR,nTek),ExogPVMistie(mNumYr,mNumCR)    !PVzipcalib
       INTEGER iFirstYr(nTek,mNumYr),iLastYr(nTek,mNumYr), iIntervalYrs(nTek,mNumYr)
       INTEGER iFuelType(nTek),NumTechs,NumYears,NumDiv, iIntervalYrstoUse
       INTEGER iExogHistYr(nTek)                                                          !ExogHist Last year of historical exogenous capacity data
       CHARACTER*10 aEquipName(nTek,mNumYr)

! Learning Related Variables
       REAL *4 xAdjCost,rlearncost,cumship,xbeta(nTek),xc0(nTek)
       LOGICAL GlobalLearn
       INTEGER NVINT

!Internal Niche Variables
    REAL*4 QRSDGSG(mNumYr,mNumCR) !Grid electricity sales in trillion Btu
    INTEGER iNiche, iRateLevel
    INTEGER NumPVNiche(mNumCR)

!Solar PV Niche Variables
    REAL*4 xSolarInsolation(mNumCR,MaxNiche,3)     !Dimensions: Census Division, MaxNiche, RateLevel (i.e., high, mid/average, and low)
    REAL*4 xHHShare(mNumCR,MaxNiche,3)             !Dimensions: Census Division, MaxNiche, RateLevel (i.e., high, mid/average, and low)
    REAL*4 xRateScalar(mNumCR,MaxNiche,3)          !Dimensions: Census Division, MaxNiche, RateLevel (i.e., high, mid/average, and low)
    REAL*4 xAvgKWH(mNumCR,MaxNiche,3)              !Dimensions: Census Division, MaxNiche, RateLevel (i.e., high, mid/average, and low)
    REAL*4 xRoofAreaPerHH(mNumCR,MaxNiche,3)       !Dimensions: Census Division, MaxNiche, RateLevel (i.e., high, mid/average, and low)
    REAL*4 xRuralPctHH(mNumCR,MaxNiche,3)          !Dimensions: Census Division, MaxNiche, RateLevel (i.e., high, mid/average, and low)
    REAL*4 xSizefromRoofArea,xSizefromAnnualKWH,xSizeMax,xSizeMin,xCalcKW,xCalcEqCost,xSolarIns    !PVgen
    REAL*4 SolarPVTechPotentialMW(mNumYr,mNumCR)
    REAL*4 SolarPVAvailRoofArea(mNumYr,mNumCR)
    REAL*4 SolarPVInstalledMW(mNumYr,mNumCR)
    REAL*4 SolarPVUsedRoofArea(mNumYr,mNumCR)
    REAL*4 xSqftPerKW
    REAL*4 xpctPVSuitable             !Percentage of households with a suitable south-facing roof (technical potential)
    REAL*4 xpctWindSuitable           !Assumed percentage of HH for which wind could be appropriate	!kj - not used

!Distributed Wind Niche Variables
    REAL*4 xWindSpeed(mNumCR,MaxNiche,3)           !Dimensions: Census Division, MaxNiche, RateLevel (i.e., high, mid/average, and low)
    REAL*4 WindAvailHH(mNumYr,mNumCR)
    REAL*4 WindTechPotentialMW(mNumYr,mNumCR)
    REAL*4 WindInstalledMW(mNumYr,mNumCR)
    REAL*4 xMpS                                    !meters per second temp variable

!OTHER LOCAL VARIABLES
    INTEGER IYR,iCurIYr,NV,NT,iDiv,ilife,r,i,F
    LOGICAL LPRINT ,LPRINT2  ! LPRINT, AND LPRINT2 CONTROL OUTPUT DETAIL
    INTEGER FILE_MGR         ! FILE MANAGER
    INTEGER*4 INFILE         ! FILE HANDLE FOR INPUT
    INTEGER*4 DGDAT          ! FILE HANDLE FOR OUTPUT
    REAL XValue

!NEW PV MODEL VARIABLES (use dynamic allocations due to ZIP code dimension for arrays, ~30,000 ZIPs)    !PVPen
    !Static and Dynamic Variables for Econometric PV Penetration Model
    INTEGER NumZIPs                                 ! Read from input file, number of ZIP code records to read
    Logical UseZipModel                             ! Read from input file
    Logical PVzipcalib                              ! Read from input file  !PVzipcalib
    INTEGER EstYear                                 ! Estimation year for the econometric model, also first year used in projections
    INTEGER*4, ALLOCATABLE:: ZipCode(:)             ! ZIP code for diagnostics only
    CHARACTER*2, ALLOCATABLE:: State(:)             ! State code
    INTEGER, ALLOCATABLE:: CenDiv(:)                ! Census division of ZIP Code
    REAL*4, ALLOCATABLE:: Income(:)                 ! Income per capita in ZIP
    REAL*4, ALLOCATABLE:: Households(:)             ! Households in ZIP
    REAL*4, ALLOCATABLE:: PopDensity (:)            ! PopDensity in ZIP
    REAL*4, ALLOCATABLE:: ElecRate(:)               ! Electric Rate in ZIP
    REAL*4, ALLOCATABLE:: Income_L(:)               ! Initial Value for Iteration Control
    REAL*4, ALLOCATABLE:: Households_L(:)           ! Initial Value for Iteration Control
    REAL*4, ALLOCATABLE:: PopDensity_L(:)           ! Initial Value for Iteration Control
    REAL*4, ALLOCATABLE:: ElecRate_L(:)             ! Initial Value for Iteration Control
    REAL*4, ALLOCATABLE:: Insol(:)                  !
    REAL*4, ALLOCATABLE:: LagCDD(:)                 !
    REAL*4 IntRate                                  ! National level variable
    REAL*4 PVPrice                                  ! National level variable
    REAL*4 InputPVPrice                             ! National level variable; used to store unmodified PV price as input from RGENTK.txt  !PVmultiplier
    REAL*4 MonthlyPayment                           ! National level variable
    REAL*4, ALLOCATABLE:: Lag1Installs(:)           ! Initial Lag1 Installs from Input File
    REAL*4, ALLOCATABLE:: Lag2Installs(:)           ! Initial Lag2 Installs from Input File
    REAL*4, ALLOCATABLE:: ProjectedInstalls(:)      ! Contains model projections, used to set lagged installs for subsequent projection years
    INTEGER, ALLOCATABLE:: PureHurdle(:)            ! PureHurdle in combination with RuralZip determine model coefficient values
    INTEGER, ALLOCATABLE:: RuralZip(:)              ! Density less than 10 HH per square mile
    REAL*4, ALLOCATABLE:: ModelInstalls(:)          ! For verification that results = R Code values in First Year, for Last Year's Projections Subsequently
    REAL*4, ALLOCATABLE:: CumUnits(:)               ! For constraining penetration
    REAL*4 CINT(2,3),CHH(2,3),CPD(2,3),CINC(2,3),CINS(2,3), &
            CER(2,3),CCDD(2,3),CPMT(2,3),CIR(2,3),CLAG1(2,3),CLAG2(2,3),CPVP(2,3)  !Sets of model coefficients
    REAL*16 xLogit, xNegBinom                      ! Temporary variables
    INTEGER j                                      ! Index variable for model selection
    REAL*8 factor0, factor, factor1, factor2, factor3, factor4, factor5, factor6  !PVcontagion
    INTEGER*2 NumYearsPV                                                 !PVcontagion
    !End of Additional Variable Assignments for Econometric PV Penetration Model    !PVPen


!------------------------------------------------------------
!   TEST FOR TRIGGER TO READ FILE AND BEGIN CALCULATIONS
!------------------------------------------------------------
     DGDAT=23  !Unit 23 is RDGENOUT.txt
     !Unit 23 is also used for the RESDBOUT file and is closed when RDGENOUT is written	!kj - are RDGENOUT and RESDBOUT both referred to as unit 23?

!  NO CALCULATIONS PRIOR TO RECSYear+1
      IF(CurCalYr.LT.RECSYear) RETURN
      IF(RSYR.EQ.RECSYear) OPEN(DGDAT,FILE='RDGENOUT.txt',FORM='FORMATTED')
      iCurIYr=RSYR-(BaseYr-1)
      IF(CurCalYr.NE.RECSYear.OR.CURITR.NE.1) GOTO 95

       ! Read dataset for ZIP code econometric penetration model  !PVPen
       ! Data from EstYear onward where projections are based on an econometric logit/ hurdle model formulation
       INFILE=FILE_MGR('O','RGENTK',.FALSE.) !Open the PV ZIP code penetration model input file
        READ(INFILE,'(19(/))')
        READ(INFILE,*)UseZipModel !if true the ZIP code model will be used for EstYear and beyond
         !IF (LPRINT) WRITE(DGDAT,*) 'UseZipModel= ', UseZipModel
        READ(INFILE,*)PVzipcalib !if true the ZIP code model will calibrate to exogenous PV capacity in EstYear and earlier  !PVzipcalib
         !IF (LPRINT) WRITE(DGDAT,*) 'PVzipcalib= ', PVzipcalib                                                                         !PVzipcalib
        READ(INFILE,*)EstYear    !this is the first year that the ZIP code model will project
         !IF (LPRINT) WRITE(DGDAT,*) 'Estimation Year= ', EstYear
        READ(INFILE,'(2(/))')
        DO i=1,3
         READ(INFILE,*)CINT(1,i),CINT(2,i)
         READ(INFILE,*)CHH(1,i),CHH(2,i)
         READ(INFILE,*)CPD(1,i),CPD(2,i)
         READ(INFILE,*)CINC(1,i),CINC(2,i)
         READ(INFILE,*)CINS(1,i),CINS(2,i)
         READ(INFILE,*)CER(1,i),CER(2,i)
         READ(INFILE,*)CCDD(1,i),CCDD(2,i)
         READ(INFILE,*)CPMT(1,i),CPMT(2,i)
         READ(INFILE,*)CIR(1,i),CIR(2,i)
         READ(INFILE,*)CLAG1(1,i),CLAG1(2,i)
         READ(INFILE,*)CLAG2(1,i),CLAG2(2,i)
         READ(INFILE,*)CPVP(1,i),CPVP(2,i)
         READ(INFILE,*) !skip model title
        ENDDO
         !IF (LPRINT) WRITE(DGDAT,*) CLAG1(1,1),CLAG2(1,1)
        READ(INFILE,*)NumZIPs
         !IF (LPRINT) WRITE(DGDAT,*) 'NumZIPs= ', NumZIPs
        READ(INFILE,'((/))')

      !Beginning of Dynamic Array Allocations for new Econometric Model
        IF(ALLOCATED(ZipCode)) DEALLOCATE(ZipCode); ALLOCATE(ZipCode(NumZIPs))
        IF(ALLOCATED(State)) DEALLOCATE(State); ALLOCATE(State(NumZIPs))
        IF(ALLOCATED(CenDiv)) DEALLOCATE(CenDiv); ALLOCATE(CenDiv(NumZIPs))
        IF(ALLOCATED(Income)) DEALLOCATE(Income); ALLOCATE(Income(NumZIPs))
        IF(ALLOCATED(Households)) DEALLOCATE(HouseHolds); ALLOCATE(Households(NumZIPs))
        IF(ALLOCATED(PopDensity)) DEALLOCATE(PopDensity); ALLOCATE(PopDensity(NumZIPs))
        IF(ALLOCATED(ElecRate)) DEALLOCATE(ElecRate); ALLOCATE(ElecRate(NumZIPs))
        IF(ALLOCATED(Income_L)) DEALLOCATE(Income_L); ALLOCATE(Income_L(NumZIPs))
        IF(ALLOCATED(Households_L)) DEALLOCATE(HouseHolds_L); ALLOCATE(Households_L(NumZIPs))
        IF(ALLOCATED(PopDensity_L)) DEALLOCATE(PopDensity_L); ALLOCATE(PopDensity_L(NumZIPs))
        IF(ALLOCATED(ElecRate_L)) DEALLOCATE(ElecRate_L); ALLOCATE(ElecRate_L(NumZIPs))
        IF(ALLOCATED(LagCDD)) DEALLOCATE(LagCDD); ALLOCATE(LagCDD(NumZIPs))
        IF(ALLOCATED(Insol)) DEALLOCATE(Insol); ALLOCATE(Insol(NumZIPs))
        IF(ALLOCATED(Lag1Installs)) DEALLOCATE(Lag1Installs); ALLOCATE(Lag1Installs(NumZIPs))
        IF(ALLOCATED(Lag2Installs)) DEALLOCATE(Lag2Installs); ALLOCATE(Lag2Installs(NumZIPs))
        IF(ALLOCATED(ProjectedInstalls)) DEALLOCATE(ProjectedInstalls); ALLOCATE(ProjectedInstalls(NumZIPs))
        IF(ALLOCATED(PureHurdle)) DEALLOCATE(PureHurdle); ALLOCATE(PureHurdle(NumZIPs))
        IF(ALLOCATED(RuralZip)) DEALLOCATE(RuralZip); ALLOCATE(RuralZip(NumZIPs))
        IF(ALLOCATED(ModelInstalls)) DEALLOCATE(ModelInstalls); ALLOCATE(ModelInstalls(NumZIPs))
        IF(ALLOCATED(CumUnits)) DEALLOCATE(CumUnits); ALLOCATE(CumUnits(NumZIPs))

        ZipCode(:)=0
        State(:)=" "
        CenDiv(:)=0
        Income(:)=0
        Households(:)=0
        PopDensity(:)=0
        ElecRate(:)=0
        Income_L(:)=0
        Households_L(:)=0
        PopDensity_L(:)=0
        ElecRate_L(:)=0
        LagCDD(:)=0
        Insol(:)=0
        Lag1Installs(:)=0
        Lag2Installs(:)=0
        ProjectedInstalls(:)=0
        PureHurdle(:)=0
        RuralZip(:)=0
        ModelInstalls(:)=0
        CumUnits(:)=0
        IntRate=0
        PVPrice=0
        MonthlyPayment=0

      DO i=1,NumZIPs
        READ(INFILE,*) ZipCode(i),State(i),CenDiv(i),Income(i),Households(i),PopDensity(i),  &
             Insol(i),ElecRate(i),LagCDD(i),IntRate,PVPrice,MonthlyPayment,Lag1Installs(i),  &
             Lag2Installs(i),PureHurdle(i),RuralZip(i),ModelInstalls(i)
        Income_L(i)=Income(i)
        Households_L(i)=Households(i)
        PopDensity_L(i)=PopDensity(i)
        ElecRate_L(i)=ElecRate(i)
        InputPVPrice=PVPrice  !preserves original PVPrice as input  !PVmultiplier
!        IF (UseZipModel) WRITE(DGDAT,29) 'ZipCode,State,CenDiv,ElecRate(2018$),PELME,PELRSOUT(cooling) ',ZipCode(i),State(i),CenDiv(i),ElecRate(i),PELME(CenDiv(i),EstYear-BaseYr+1),PELRSOUT(CenDiv(i),EstYear-BaseYr+1,2)  !DGrate - Compare ZIP code-level retail electricity rate with census division-level retail and wholesale rates
!          29 FORMAT(A,I5,A4,I2,1X,F5.2,1X,F5.2,1X,F5.2)  !DGrate
      ENDDO
     !IF (UseZipModel) WRITE(DGDAT,*) 'Completed Read of ZIP Code Data'
     !IF (UseZipModel) WRITE(DGDAT,*) 'First Value', ZipCode(1)
     !IF (UseZipModel) WRITE(DGDAT,*) 'Last Value', ModelInstalls(NumZIPs)
     INFILE=FILE_MGR('C','RGENTK',.FALSE.)

!End of processing of RGENTK for new econometric PV penetration model        !PVPen

!--------------------------------------------------------------
!   OPEN FILE AND READ/WRITE DATA
      INFILE=FILE_MGR('O','RSGENTK',.FALSE.) !OPEN THE DISTRIBUTED GENERATION TECHNOLOGY MENU
!      Unit 23 is also used for the ResDBOut file and is closed when ResDBOut is written	!kj - unit 23 is used for both RESDBOUT and RDGENOUT? Doesn't seem to be the case.

 ! SKIP 20-LINE HEADER AND READ GENERAL CONTROL PARAMETERS AND INPUTS
      READ(INFILE,'(19(/))')
      READ(INFILE,*)LPRINT, LPRINT2
      IF(LPRINT) WRITE(DGDAT,*) 'MODEL YEAR ',RSYR, 'ITERATION ',CURITR

!  -- NUMBER OF TECHNOLOGIES (3), NUMBER OF MODEL YEARS (mNumYr), AND NUMBER OF MODELED CENSUS DIVISIONS (9)
      READ(INFILE, '(/)')
      READ(INFILE,*) NumTechs,NumYears,NumDiv	!kj - NumYears and NumDiv could/should be same as other NEMS variables and not read in here
      IF(LPRINT)WRITE(DGDAT,*) NumTechs,NumYears,NumDiv

      READ(INFILE,'(/)')
      READ(INFILE,*) iGenCapCostYr
      IF(LPRINT)WRITE(DGDAT,*) iGenCapCostYr

!  -- LPRINT TURNS ON TRACING OF EXECUTION; LPRINT2 PROVIDES DETAILS OF THE CASH-FLOW CALCULATIONS FOR "VINTAGE" YEARS
      READ(INFILE, '(/)')
      READ(INFILE,*) xAlpha, xPenParm
      IF(LPRINT)WRITE(DGDAT,*) xAlpha,xPenParm
!  -- xAlpha AND xPenParm CONTROL THE MAGNITUDE AND SHAPE OF THE
!       PENETRATION OF DISTRIBUTED GENERATION TECHNOLOGIES

      READ(INFILE, '(/)')
! Vary DG capacity by year -- eliminate read here
!      READ(INFILE,*) (xKW(NT), NT=1,NumTechs)
!      IF(LPRINT)WRITE(DGDAT,'(7F9.2)') (xKW(NT), NT=1,NumTechs)
      READ(INFILE,*) (xOperHours(NT), NT=1,NumTechs)
      IF(LPRINT)WRITE(DGDAT,'(7F9.2)') (xOperHours(NT), NT=1,NumTechs)
!  -- FOR EACH OF THE TECHNOLOGIES, SYSTEM SIZE IN KW, AND ANNUAL OPERATING
!        HOURS (RELEVENT ONLY FOR FUEL USING GENERATION TECHNOLOGIES)

      READ(INFILE, '(/)')
      READ(INFILE,*) GLOBALLEARN
      IF(LPRINT)WRITE(DGDAT,*) "Global Learning = ",GLOBALLEARN

      READ(INFILE, '(/)')
      READ(INFILE,*) (XBETA(NT), NT=1,NumTechs)
      IF(LPRINT)WRITE(DGDAT,'(3F9.2)') (XBETA(NT), NT=1,NumTechs)
      READ(INFILE,*) (XC0(NT), NT=1,NumTechs)
      IF(LPRINT)WRITE(DGDAT,'(3F9.2)') (XC0(NT), NT=1,NumTechs)
!  -- FOR EACH OF THE TECHNOLOGIES, Learning Betas (DOubling parameter, 0=no learning) and c0's (initial costs)
!
      READ(INFILE, '(/)')
      READ(INFILE,*) iRPSStartYear, iRPSPhaseOutYear, iRPSGrandFatherYear
      IF(LPRINT)WRITE(DGDAT,'(3I8)') iRPSStartYear, iRPSPhaseOutYear, iRPSGrandFatherYear
!  -- The above are scalars for multiplying the RPS credit price

      READ(INFILE, '(/)')
      READ(INFILE,*) (XINX(i),i=1,NumDiv)
      IF(LPRINT)WRITE(DGDAT,'(9F7.3)') (XINX(i),i=1,NumDiv)
!  -- The above are scalars for limiting the penetration based on interconnection index
      READ(INFILE,*) xInxFY, xInxLY
      IF(LPRINT)WRITE(DGDAT,'(3I8)') xInxFY, xInxLY
!  -- The above are the first and last years of the interconnection limits

      READ(INFILE, '(/)')
      READ(INFILE,*)XTAXRATE,XDOWNPAYPCT,XTERM,XINFLATION
      IF(LPRINT)WRITE(DGDAT,'(4F7.2)') XTAXRATE, XDOWNPAYPCT, XTERM, XINFLATION
!  NOTES:
!  -- XTAXRATE IS THE COMBINED FEDERAL AND STATE INCOME TAX RATE WHICH IS USED
!       IN THE CASH-FLOW CALCULATIONS FOR INTEREST PAID ON EQUIPMENT LOANS
!       (ASSUMED TO BE "ROLLED IN" WITH THE MORTGAGE)
!  -- XDOWNPAYPCT IS THE MORTGAGE DOWN PAYMENT PERCENTAGE
!  -- XINTRATE IS THE MORTGAGE ANNUAL LOAN RATE
!  -- XTERM IS THE NUMBER OF YEARS FOR THE LOAN (ASSUMING 20 YEARS SIMPLIFIES
!       EQUIPMENT ACCCOUNTING (REPLACEMENT OF ORIGINAL EQUIPMENT IS NOT
!       PART OF THE CASH-FLOW BECAUSE IT DIES AFTER 20 YEARS)
!  -- XINFLATION IS THE INFLATION FOR THE CASH-FLOW CALCULATIONS, WHICH ARE
!       IN REAL DOLLARS, TO DISCOUNT LOAN PAYMENTS (IN NOMINAL DOLLARS)

      READ(INFILE, '(//)')
      READ(INFILE,*) NVINT       ! read number of technology records in file
      IF(LPRINT)WRITE(DGDAT,*) NVINT
   ! 0 out technology variable to be read in next
      aEquipName = "          "
      iFuelType = 0.0
      iFirstYr = 0.0
      iLastYr = 0.0
      xKW = 0.0
      xElEff = 0.0
      xLossFac = 0.0
      xDegrad = 0.0
      xEqLife = 0.0
      xWhRecovery = 0.0
      xInstCost = 0.0
      xCapCost = 0.0
      xMaintCst = 0.0
      xIntervalCst = 0.0
      iIntervalYrs = 0.0
      xAvail = 0.0
      xTxCrPct = 0.0
      xTXCrMaxPerKW = 0.0
      xTXCrMaxPerSys = 0.0

!  POPULATE TECHNOLOGY ARRAY
          DO NT=1,NumTechs
          DO NV=1,NVINT

!   THE ORIGINAL TECHNOLOGY FILE ASSUMES:
!      NT=1 IS SOLAR PHOTOVOLTAIC (PV) TECHNOLOGY
!      NT=2 IS NATURAL GAS FUEL CELL TECHNOLOGY
!      NT=3 IS SMALL WIND TURBINE TECHNOLOGY
!   THE VINTAGES APPLY TO DIFFERENT NON-OVERLAPPING TIME PERIODS AND MAY EMBODY TECH PROGRESS

! Vary DG capacity by year -- add read of xKW capacity by vintage here
          READ(INFILE,*,END=99) aEquipName(NT,NV), iFuelType(NT), &
                    iFirstYr(NT,NV),   iLastYr(NT,NV), &
                    xKW(NT,NV),   xElEff(NT,NV), &
                    xLossFac(NT,NV),  xDegrad(NT,NV), &
                    xEqLife(NT,NV),  xWhRecovery(NT,NV), &
                    xInstCost(NT,NV),  xCapCost(NT,NV), &
                    xMaintCst(NT,NV),  xIntervalCst(NT,NV), &
                    iIntervalYrs(NT,NV),  xAvail(NT,NV), &
                    xTxCrPct(NT,NV),  xTXCrMaxPerKW(NT,NV), xTXCrMaxPerSys(NT,NV), &
                    (xTxCrPct_Div(nv,iDiv,nt),iDiv=1,mNumCR-2), &                             !111dren
                    xTemp, iTemp1, iTemp2

!  NOTES:
!  --  aEquipName IS NAME FOR REPORTING PURPOSES
!  --  iFuelType IS THE FUEL USED BY THE TECHNOLOGY (0 FOR SOLAR OR WIND)
!         THIS FUEL TYPE MUST COINCIDE WITH THE MAIN MODEL DEFINITION OF
!         FUELS
!  --  iFirstYr IS THE FIRST YEAR A TECHNOLOGY CAN BE PURCHASED
!  --  iLastYr IS THE LAST YEAR A TECHNOLOGY CAN BE PURCHASED (DON'T
!         ALLOW TECHNOLOGIES TO "OVERLAP" OR "GAP" (E.G., VINTAGE 1 2015-2019;
!         VINTAGE 2 2020-2029; VINTAGE 3 2030-2039; VINTAGE 4 2040-2050)
!  --  xElEff IS THE ELECTRICAL CONVERSION EFFICIENCY OF THE TECHNOLOGY
!  --  xLossFac IS THE LOSS FACTOR FROM GENERATION TO END USE 'INCLUDES LINE LOSS,
!         INVERTER LOSSES, ETC....
!  --  xDegrad IS THE ANNUAL DEGRADATION IN THE EFFICIENCY OF SOLAR PV
!  --  xEqLife IS THE EQUIPMENT LIFE IN YEARS
!  --  xWhRecovery IS THE PERCENTAGE OF WASTE HEAT (FOR FUEL USING TECHNOLOGIES)
!         THAT CAN BE RECOVERED FOR WATER HEATING (ANYTHING IN EXCESS OF AVERAGE
!         WATER HEATING REQUIREMENTS IS ASSUMED WASTED)
!  --  xInstCost INSTALLATION COST PER KW IN iGenCapCostYr-DOLLARS
!  --  xCapCost CAPITAL COST PER KW IN iGenCapCostYr-DOLLARS
!  --  xMaintCst ANNUAL MAINTENANCE COST IN iGenCapCostYr-DOLLARS
!  --  xAvail = 1.0-FORCED OUTAGE RATE
!  --  xTxCrPct IS THE TAX CREDIT PERCENTAGE FOR THIS TECHNOLOGY
!  --  xTXCrMaxPerKW IS THE MAXIMUM DOLLAR AMOUNT (PER KW) OF THE TAX CREDIT (if zero, no cap)
!  --  xTXCrMaxPerSys IS THE MAXIMUM DOLLAR AMOUNT (PER System) OF THE TAX CREDIT (if zero, no cap)

      IF(LPRINT)WRITE(DGDAT,30) aEquipName(NT,NV), iFuelType(NT), iFirstYr(NT,NV), iLastYr(NT,NV), &
                    xKW(NT,NV), xElEff(NT,NV), xLossFac(NT,NV), xDegrad(NT,NV), xEqLife(NT,NV), &
                    xWhRecovery(NT,NV), xInstCost(NT,NV), xCapCost(NT,NV), xMaintCst(NT,NV), xIntervalCst(NT,NV), &
                    iIntervalYrs(NT,NV), xAvail(NT,NV), xTxCrPct(NT,NV), xTXCrMaxPerKW(NT,NV), xTXCrMaxPerSys(NT,NV), &
                    (xTxCrPct_Div(nv,iDiv,nt),iDiv=1,mNumCR-2), xTemp, iTemp1, iTemp2  !111dren

             ! Immediately populate the RPS variable mapping vintages to iCurIYr	!kj - why use the temp variable names at all when annualized RPS values are read-in from file?
             DO iyr=iFirstYr(nt,nv),iLastYr(nt,nv)
              xRPS(nt,iyr-(BaseYr-1))=xTemp
              iNumYrsatRPSBaseRate(nt,iyr-(BaseYr-1))=iTemp1
              iNumRPSCreditYrs(nt,iyr-(BaseYr-1))=iTemp2
             ENDDO

          ENDDO
          ENDDO
 30    FORMAT(1X,A10,3I6,F9.1,3F9.2,2F9.1,F9.2,3F9.0,I6,2F9.2,2F9.1,9F9.2,F5.1,2I5)

!Debugging to see what we ended up with:
!             DO iyr=1,mNumYr
!               WRITE(DGDAT,*)xrps(3,iyr),iNumYrsatRPSBaseRate(3,iyr),iNumRPSCreditYrs(3,iyr)
!             ENDDO

!      PV NICHES:
!      Add an arbitrary number of insolation niches customized to each census division (based on bins of solar insolation levels rounded to nearest 0.25 kWh/m^2/day)
!      Add 3 rate levels: High, Mid, and Low average electricity rates

!      READ SOLAR INSOLATION, SQFT SHARES, AVERAGE ELECTRICITY RATES RELATIVE TO CENSUS DIVISION, ROOF TO SQFT RATIOS, WIND SPEED FOR NICHES.
!      VALUES ARE ESTIMATED FROM RECS MICRODATA (BOTH PUBLIC AND UN-PUBLISHED)USING ZIP CODE-LEVEL PV SOLAR RADIATION/INSOLATION (LATITUDED TILT)
!       AND NREL 30M ANNUAL AVERAGE WIND SPEEDS BY STATE.

      READ(INFILE,'(///)')  !DGniches
      DO I=1,NumDiv
       READ(INFILE,*) iDiv, iNiche
       IF(LPRINT)WRITE(dgdat,*) "iDiv, iNiche", iDiv, iNiche
       NumPVNiche(iDiv)= iNiche
        DO iNiche=1,NumPVNiche(iDiv)
          DO iRateLevel=1,3 !i.e., high, mid/average, and low average electricity rate levels
            READ(INFILE,*) xSolarInsolation(iDiv,iNiche,iRateLevel),xHHShare(iDiv,iNiche,iRateLevel), &
             xRateScalar(iDiv,iNiche,iRateLevel), xAvgKWH(iDiv,iNiche,iRateLevel), &
             xRoofAreaPerHH(iDiv,iNiche,iRateLevel), xWindSpeed(iDiv,iNiche,iRateLevel),xRuralPctHH(iDiv,iNiche,iRateLevel)
            IF(LPRINT)WRITE(dgdat,87) iDiv,xSolarInsolation(iDiv,iNiche,iRateLevel),xHHShare(iDiv,iNiche,iRateLevel), &
             xRateScalar(iDiv,iNiche,iRateLevel), xAvgKWH(iDiv,iNiche,iRateLevel), &
             xRoofAreaPerHH(iDiv,iNiche,iRateLevel), xWindSpeed(iDiv,iNiche,iRateLevel),xRuralPctHH(iDiv,iNiche,iRateLevel)
          ENDDO !iRateLevel
        ENDDO !iNiche
      ENDDO !NumDiv

 87   FORMAT(1x,"Div= ",I3,3F10.4,2F12.4,2F10.4)

      !Last year of historical exogenous capacity data; model builds begin after this  !ExogHist
      READ(INFILE, '(/)')                                    !ExogHist
      READ(INFILE,*,END=99) (iExogHistYr(NT),NT=1,NumTechs)  !ExogHist

! Read exogenous distributed generation capacity
      READ(INFILE, '(/)')
      DO NT=1,NumTechs
        DO iDiv=1,NumDiv
          READ(INFILE,*,END=99) (xExogPen(IYR,iDiv,NT),IYR=RECSYear-BaseYr+1,NumYears)

          IF(LPRINT) WRITE(DGDAT,*) 'EXOGENOUS DG CAPACITY (KW-DC) FROM RSGENTK (RECSYR-ONWARD)'
          IF(LPRINT) WRITE(DGDAT,*) 'TECHNOLOGY ',NT,'  DIVISION ',iDiv
          IF(LPRINT) WRITE(DGDAT,97) (xExogPen(IYR,iDiv,NT),IYR=RECSYear-BaseYr+1,NumYears)
 97       FORMAT(5F10.0)
        ENDDO !NumDiv
      ENDDO !NumTechs

! Read switch to combine marginal (wholesale) electricity rates and retail electricity rates for PV calculations  !DGrate
      READ(INFILE,'(/)')  !DGrate
      READ(INFILE,*) DGrateBlend  !DGrate - switch to turn on blending of retail and wholesale electricity rate
      IF(LPRINT) WRITE(DGDAT,*) 'DGrateBlend ',DGrateBlend  !DGrate

! Read first year chosen to combine marginal (wholesale) electricity rates and retail electricity rates for PV calculations  !DGrate
      READ(INFILE,'(/)')  !DGrate
      READ(INFILE,*) DGrateYr  !DGrate
      IF(LPRINT) WRITE(DGDAT,*) 'DGrateYr ',DGrateYr  !DGrate

! Read weight values to combine marginal (wholesale) electricity rates and retail electricity rates for PV calculations  !DGrate
      READ(INFILE, '(//)')  !DGrate
      DO iDiv=1,NumDiv  !DGrate
        READ(INFILE,*,END=99) DGmargWt(iDiv),DGretWt(iDiv)  !DGrate
      ENDDO !NumDiv  !DGrate
	  IF(LPRINT) THEN
        WRITE(DGDAT,*) 'DGmargWt ','DGretWt ','CD'  !DGrate
        DO iDiv=1,NumDiv  !DGrate
          WRITE(DGDAT,*) DGmargWt(iDiv),' ',DGretWt(iDiv),' ',iDiv  !DGrate
        ENDDO !NumDiv  !DGrate
      ENDIF  !DGrate

      READ(INFILE, * ,END=96)  XTEST
       WRITE(DGDAT,*) &
          'INPUT ERROR ON RESIDENTIAL DISTRIBUTED GENERATION TECHNOLOGY DATA FILE'
       WRITE(DGDAT,*) &
          'EXTRA DATA ENCOUNTERED -- MOST LIKELY A SEVERE PROBLEM'

      GOTO 96

 99    WRITE(DGDAT,*) &
          'INPUT ERROR ON RESIDENTIAL DISTRIBUTED GENERATION TECHNOLOGY DATA FILE'
       WRITE(DGDAT,*) &
          'TOO FEW DATA ENCOUNTERED -- A SEVERE PROBLEM'

 96    INFILE=FILE_MGR('C','RSGENTK',.FALSE.)

        DO iDiv=1,NumDiv
         CGCPVRES(iDiv,iCurIYr)=1.
         CGCWNRES(iDiv,iCurIYr)=1.
         DO NT=1,NumTechs
          DO IYR=1,NumYears
!           xExogPen(IYR,iDiv,NT)=0.0 !used for testing / removing exogenous DG installations
           Units(IYR,iDiv,NT)=0.
           Cap(IYR,iDiv,NT)=0.
           Trills(IYR,iDiv,NT)=0.
           HWBTU(IYR,iDiv,NT)=0.
           GasUsage(IYR,iDiv,NT)=0.
           TrillsOwnUse(IYR,iDiv,NT)=0.
          ENDDO
         ENDDO
        ENDDO

        !Assumptions for developing technical potential for PV	!kj
        ! Based on orientation alone, approximately 50% of HH would have a suitable southwest- to southeast-facing roof surface.
        ! Next, assume that, of the suitably oriented single-family households, only 50% of the roof area is facing south.
        ! Also assume that 40% of this area is unavailable due to shading and other issues like roof impediments.
        xpctPVSuitable= .5*.5*(1.-.4)

!--------END OF READ AND INITIALIZE

!-------------------------------------------------------------
!  BEGIN ECONOMIC PENETRATION MODELING FOR PROJECTED DG BUILDS
!-------------------------------------------------------------
 95    CONTINUE

       !Initialize accumulators to allow for multiple NEMS iterations
       DO iDiv=1,NumDiv
         QRSDGSG(iCurIYr,iDiv)=0.
         SolarPVTechPotentialMW(iCurIYr,iDiv)=0.
         SolarPVInstalledMW(iCurIYr,iDiv)=0.
         SolarPVAvailRoofArea(iCurIYr,iDiv)=0.
         SolarPVUsedRoofArea(iCurIYr,iDiv)=0.
         WindAvailHH(iCurIYr,iDiv)=0.
         WindTechPotentialMW(iCurIYr,iDiv)=0.
         WindInstalledMW(iCurIYr,iDiv)=0.
         DO nt=1,NumTechs
            x111dRenSub(iCurIYr,iDiv,nt)=0.  !111dren
         ENDDO
         !Also set interconnection limit variable used for all technologies, by Census Division
         !INXLIMIT (moved from below, and now set up here to accommodate new PV penetration approach)
         xInxDecay(iDiv,iCurIYr)=1.
         IF(CurCalYr.EQ.xInxFY) THEN
           xInxDecay(iDiv,iCurIYr)=XINX(iDiv)
         ELSE
           xInxDecay(iDiv,iCurIYr)=MIN(1.,(XINX(iDiv)+((1.-XINX(iDiv))*(FLOAT(CurCalYr-xInxFY)/FLOAT(xInxLY-xInxFY)))))
         ENDIF
!        IF (LPRINT) WRITE(DGDAT,*) 'Interconnection Limit ',CurCalYr, iDiv, xInxDecay(iDiv,iCurIYr)
        ENDDO

          DO NT=1,NumTechs

            !     NT=1 IS SOLAR PHOTOVOLTAIC TECHNOLOGY
            !     NT=2 IS FUEL CELL TECHNOLOGY
            !     NT=3 IS DISTRIBUTED WIND TURBINE TECHNOLOGY
            !
            !     FILTER FOR "VINTAGE" APPROPRIATE FOR THIS MODEL YEAR

            DO NV=1,NVINT
            IF(iFirstYr(NT,NV).GT.CurCalYr) GOTO 66  !SKIP OUT-OF-SCOPE VINTAGES
            IF(iLastYr (NT,NV).LT.CurCalYr) GOTO 66  !SKIP OUT-OF-SCOPE VINTAGES
            IF(LPRINT) THEN
             IF(iFuelType(NT).EQ.0) THEN
                WRITE(DGDAT,*)'BEGINNING CASH-FLOW CALCULATIONS FOR SOLAR PV OR WIND SYSTEMS'
               ELSE
                WRITE(DGDAT,*) &
                  'BEGINNING CASH-FLOW CALCULATIONS FOR FUEL CELL / OTHER SYSTEMS'
             ENDIF
            ENDIF

            !INITIALIZE VALUES FOR OPERATING COST CALCULATIONS
            ! Update "Learned" Costs for PV, Fuel Cells, Wind and Micro Turbines

        IF(aEquipName(nt,nv) .EQ. "Fuel_Cell" )THEN
              cumship=CFuelCell_MW(iCurIYr-1)+ RFuelCell_MW(iCurIYr-1)! buildings shipments only
              ! Not Enabled ! IF (globallearn.EQ.1)cumship=CFuelCell_MW(iCurIYr-1)+ RFuelCell_MW(iCurIYr-1) +UFuelcell_MW(iCurIYr-1)+ IntnlFuelCell_MW(iCurIYr-1)+ IFuelCell_MW(iCurIYr-1)
              xAdjCost = rLearnCost (xCapCost(nt,nv), xbeta(nt), xc0(nt), cumship, 23)
                    IF(LPRINT)WRITE(DGDAT,*)'current year ',iCurIYr+(BaseYr-1)
                    IF(LPRINT)WRITE(DGDAT,*)'learning beta ',xbeta(nt)
                    IF(LPRINT)WRITE(DGDAT,*)'initial cost ',xc0(nt)
                    IF(LPRINT)WRITE(DGDAT,*)'cumulative shipments ',CumShip
                    IF(LPRINT)WRITE(DGDAT,*)'RSGENTK default cost ',xCapCost(nt,nv)
                    IF(LPRINT)WRITE(DGDAT,*)'learning-adjusted cost ',xAdjCost

         ELSEIF(aEquipName(nt,nv) .EQ. "Solar_PV" )THEN
              IF (globallearn)THEN
                 ! globallearn=True indicates to include electric generator PV installs in the learning calculations for buildings
                 cumship=CPV_MW(iCurIYr-1) +RPV_MW(iCurIYr-1)+ UPV_MW(iCurIYr-1)
                ELSE
                 cumship=CPV_MW(iCurIYr-1) +RPV_MW(iCurIYr-1)
              ENDIF
              ! Not Enabled ! IF(globallearn.EQ.1) cumship=CPV_MW(iCurIYr-1) +RPV_MW(iCurIYr-1)+ UPV_MW(iCurIYr-1) + IntnlPV_MW(iCurIYr-1)+IPV_MW(iCurIYr-1)
              xAdjCost = rLearnCost (xCapCost(nt,nv), xbeta(nt), xc0(nt), cumship, 23)
                    IF(LPRINT)WRITE(DGDAT,*)'current year ',iCurIYr+(BaseYr-1)
                    IF(LPRINT)WRITE(DGDAT,*)'learning beta ',xbeta(nt)
                    IF(LPRINT)WRITE(DGDAT,*)'initial cost ',xc0(nt)
                    IF(LPRINT)WRITE(DGDAT,*)'cumulative shipments ',CumShip
                    IF(LPRINT)WRITE(DGDAT,*)'RSGENTK default cost ',xCapCost(nt,nv)
                    IF(LPRINT)WRITE(DGDAT,*)'learning-adjusted cost ',xAdjCost

         ELSEIF(aEquipName(nt,nv) .EQ. "Wind" )THEN
              cumship=CWind_MW(iCurIYr-1) +RWind_MW(iCurIYr-1)
              ! Not Enabled ! IF(globallearn.EQ.1) cumship=CWind_MW(iCurIYr-1) +RWind_MW(iCurIYr-1)+ UWind_MW(iCurIYr-1) + IntnlWind_MW(iCurIYr-1) + IWind_MW(iCurIYr-1)
              xAdjCost = rLearnCost (xCapCost(nt,nv), xbeta(nt), xc0(nt), cumship, 23)
                    IF(LPRINT)WRITE(DGDAT,*)'current year ',iCurIYr+(BaseYr-1)
                    IF(LPRINT)WRITE(DGDAT,*)'learning beta ',xbeta(nt)
                    IF(LPRINT)WRITE(DGDAT,*)'initial cost ',xc0(nt)
                    IF(LPRINT)WRITE(DGDAT,*)'cumulative shipments ',CumShip
                    IF(LPRINT)WRITE(DGDAT,*)'RSGENTK default cost ',xCapCost(nt,nv)
                    IF(LPRINT)WRITE(DGDAT,*)'learning-adjusted cost ',xAdjCost

         ELSE
              xAdjCost=xCapCost(nt,nv)
        ENDIF

          xTaxCreditPct=xTxCrPct(NT,NV)
          XTAXCREDITMAxKW=xTXCrMaxPerKW(NT,NV)
          XTAXCREDITMAX=xTXCrMaxPerSys(NT,NV)
          XLIFE=xEqLife(NT,NV)
          xEqCost=(xAdjCost+xInstCost(NT,NV)) * xKW(NT,nv) !Vary DG capacity by year
          xDegradation=xDegrad(NT,NV)
            !      IF(LPRINT)
            !     1  WRITE (DGDAT,'(4F12.2)') xInstCost(NT,NV),xCapCost(NT,NV), xEqCost,
            !     2           xDegradation

            DO iDiv=1,NumDiv

            ! Initialization to output accumulating variables
            xUnits=0.
            xTrills=0.
            xCapacity=0.
            xTrillsOwnUse=0.
            xfuelusage=0.
            xhwbtu=0.
            xInvest=0.
!            CGCPVRES(iDiv,iCurIYr)=1. !RPS Variable for Electricity Module
!            CGCWNRES(iDiv,iCurIYr)=1. !RPS Variable for Electricity Module

            IF(NT.EQ.1 .AND. UseZipModel .AND. CurCalYr.GE.EstYear)GOTO 26  !FOR PV FOR THE ESTIMATION YEAR AND BEYOND USE CD MODEL

            !      WRITE (DGDAT,*) 'MODEL YEAR = ',CurCalYr, 'CENSUS DIVISION ', iDiv
            !      IF(LPRINT) WRITE(DGDAT,*) 'VINTAGE YEAR',CurCalYr
            !          WRITE(DGDAT,*) '   COMPUTING OPERATING COSTS AND VALUE OF ENERGY SAVINGS',iDiv

            IF(LPRINT.AND.LPRINT2) &
             WRITE(DGDAT,*) 'FUEL TYPE ',iFuelType(NT),'TECHNOLOGY ',NT

            ! Calculate Grid Sales Price
            ! Units are in $/kWh in year dollars of capital costs
            ! Assumed not to be "niche" related
             xSalestoGridPR=PELME(iDiv,iCurIYr)*.003412*MC_JPGDP(iGenCapCostYr-BaseYr+1)/MC_JPGDP(-2)	!kj - need to update PELME? !DGrate

            DO iNiche=1,NumPVNiche(iDiv)          !Add Insolation and Wind Niches
            DO iRateLevel=1,3                     !Add Niches for High=1, Mid=2 and Low=3 Average Rates
            IF(xHHShare(iDiv,iNiche,iRateLevel)==0.) GOTO 25        !next niche -- skip those that have 0 HH share
            ! Set average consumption
            xElecAvgUEC=xAvgKWH(iDiv,iNiche,iRateLevel)

 !-----------------------------------
 ! -Setup for Solar Calculations
 !-----------------------------------
            IF(NT.EQ.3)GOTO 100  ! Wind
            IF(NT.EQ.2)GOTO 150  ! Fuel Cells

              ! CALCULATION OF KWH SUPPLIED FOR Solar
              !  The quantity "77.*(.14/xElEff)*xKW" represents the estimated module square footage.
              !  Thus the kWh supplied is:
              !      annualkwh=eff*insolation*sqftperkw*systemkw*lossadj
              !  where lossadj represents average non-optimality factor (orientation effects, etc.)
              !  Notes: Future efficiency gains will result in a smaller collector footprint for a given kW capacity.
              !         Solar insolation is in kWh/m^2/day convert to annual per square foot (365.25/10.8)
              xSqftperKW=77.*.14/xElEff(NT,NV)	!kj

              ! Optimize capacity: set maximums based on 80% of the optimally oriented roof surface area is available (e.g., non-shaded)         !PVgen
              !  Assume only 40% of roof area is suitable for PV to allow for non-optimal orientation and/or complex roof angles.                !PVgen
              !  Also, assume most homeowners cover only 75% of the potential max area and assume a global maximum and minimum of 10 and 1 kW.   !PVgen
              IF (CurCalYr>=RECSYear) THEN                                                                                                       !PVgen
                 xSolarIns=xSolarInsolation(iDiv,iNiche,IRateLevel)                                                                              !PVgen
                 ! transform solar insolation to account for unknown orientations                                                                !PVgen
                 xSolarIns= -1.0533 + 1.4325*xSolarIns - 0.0652*xSolarIns**2                                                                     !PVgen	!kj
                 xSizefromRoofArea= (xRoofAreaPerHH(iDiv,iNiche,iRateLevel)*0.8*0.4*0.75/xSqftperKW)                                             !PVgen	!kj
                 xSizefromAnnualKWH=xElecAvgUEC/(xElEff(nt,nv)*xSolarIns*365.25/10.8*xSqftperKW*xLossFac(NT,NV))                                 !PVgen	!kj

                 ! also optimize to maximize the after tax cost per kW based on credits
                 xSizefromTaxOptim=xSizeMax !set to max size if no cap on tax credit
                 ! ELSE compute largest size that fully utilizes the credit
                 ! first reset the tax credit percentage to potentially something less if the max credit per kW is capped
                 IF(xTaxCreditPct>0. .AND. xtaxcreditmaxKW>0.) THEN
                    xTaxCreditPct=min(xTaxCreditPct,xtaxcreditmaxKW/(xAdjCost+xInstCost(NT,NV)))
                 ENDIF
                 IF(xTaxCreditPct>0. .AND. xtaxcreditmax>0.) xSizefromTaxOptim = (xtaxcreditmax/xTaxCreditPct)/(xAdjCost+xInstCost(NT,NV))
                 xSizeMax=10.      !set absolute maximum size
                 xSizeMin=1.       !set absolute minimum size
                 xCalcKW=FLOAT(ifix(max(min(xSizefromRoofArea,xSizefromTaxOptim,xSizeMax),xSizeMin)))    !removed RECS average generation constraint
              ELSE
                 xCalcKW=xKW(nt,nv)  !set size to menu capacity
              ENDIF

              xAnnualKWh=xElEff(NT,NV)*xSolarIns*365.25/10.8*xSqftperKW*xCalcKW*xLossFac(NT,NV)    !PVgen	!kj

              !   The internal NEMS energy prices are converted to "current year" dollars (the
              !    dollar year for the DG capacity costs) from the internal NEMS year of 1987 dollars.
              !   Note that MC_JPGDP(-2) is the deflator for 1987.
              !   For PV, use the air conditioning price as the implicit value for own-use generation,
              !    due to the high "coincidence factor" between PV output and AC loads.
              !   This usage of the air conditioning price is to reflect average summer prices, when PV output
              !    is at its highest.

      !THIS CODE IS USED TO SWITCH FROM RETAIL COOLING ELECTRICITY RATES (PELRSOUT(iDiv,iCurIYr,2)) TO WEIGHTED MARGINAL/WHOLESALE (PELME(iDiv,ICURIYR)) AND RETAIL RATE BLEND  !DGrate
      IF (DGrateBlend) THEN  !DGrate - If set to TRUE in RSGENTK.txt, then use blended electricity rate starting in DGrateYr
        IF (CURCALYR.LT.DGrateYr) THEN  !DGrate
          xRetailElecPR=( PELRSOUT(iDiv,iCurIYr,2)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &  !DGrate
           +xRPS(nt,iCurIYr)*EPRPSPR(iCurIYr)/1000. )*MC_JPGDP(iGenCapCostYr-(BaseYr-1))/MC_JPGDP(-2)     !own-use including scaled RPS credit  !DGrate
          xRetailElecPRadjRPS=( PELRSOUT(iDiv,iCurIYr,2)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &  !DGrate
           +1.0*EPRPSPR(iCurIYr)/1000. )*MC_JPGDP(iGenCapCostYr-(BaseYr-1))/MC_JPGDP(-2)     !own-use including scaled RPS credit  !DGrate
          xRetailElecPRnoRPS= PELRSOUT(iDiv,iCurIYr,2)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &  !DGrate
           *MC_JPGDP(iGenCapCostYr-(BaseYr-1))/MC_JPGDP(-2)                                            !own-use no RPS credit  !DGrate
        ELSEIF (CURCALYR.GE.DGrateYr) THEN  !DGrate
          xRetailElecPR=( ((PELME(iDiv,ICURIYR)*DGmargWt(iDiv)) + (PELRSOUT(iDiv,ICURIYR,2)*DGretWt(iDiv)))*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &  !DGrate
           +xRPS(nt,iCurIYr)*EPRPSPR(iCurIYr)/1000. )*MC_JPGDP(iGenCapCostYr-(BaseYr-1))/MC_JPGDP(-2)     !own-use including scaled RPS credit  !DGrate
          xRetailElecPRadjRPS=( ((PELME(iDiv,ICURIYR)*DGmargWt(iDiv)) + (PELRSOUT(iDiv,ICURIYR,2)*DGretWt(iDiv)))*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &  !DGrate
           +1.0*EPRPSPR(iCurIYr)/1000. )*MC_JPGDP(iGenCapCostYr-(BaseYr-1))/MC_JPGDP(-2)     !own-use including scaled RPS credit  !DGrate
          xRetailElecPRnoRPS= ((PELME(iDiv,ICURIYR)*DGmargWt(iDiv)) + (PELRSOUT(iDiv,ICURIYR,2)*DGretWt(iDiv)))*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &  !DGrate
           *MC_JPGDP(iGenCapCostYr-(BaseYr-1))/MC_JPGDP(-2)                                            !own-use no RPS credit  !DGrate
        ENDIF  !DGrate
      ELSE  !DGrate
        xRetailElecPR=( PELRSOUT(iDiv,iCurIYr,2)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &  !DGrate
         +xRPS(nt,iCurIYr)*EPRPSPR(iCurIYr)/1000. )*MC_JPGDP(iGenCapCostYr-(BaseYr-1))/MC_JPGDP(-2)     !own-use including scaled RPS credit  !DGrate
        xRetailElecPRadjRPS=( PELRSOUT(iDiv,iCurIYr,2)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &  !DGrate
         +1.0*EPRPSPR(iCurIYr)/1000. )*MC_JPGDP(iGenCapCostYr-(BaseYr-1))/MC_JPGDP(-2)     !own-use including scaled RPS credit  !DGrate
        xRetailElecPRnoRPS= PELRSOUT(iDiv,iCurIYr,2)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &  !DGrate
         *MC_JPGDP(iGenCapCostYr-(BaseYr-1))/MC_JPGDP(-2)                                            !own-use no RPS credit  !DGrate
      ENDIF  !DGrate

              ! Compare annual PV generation to building use, value own-use at the retail price
              !  and grid sales at the grid price
              XEXCESSKWH=xAnnualKWh-xElecAvgUEC
              IF(XEXCESSKWH.LT.0.) THEN
                XVALESAVEBASE=xAnnualKWh*xRetailElecPr  ! own-use
               ELSE
                XVALESAVEBASE= &
                XEXCESSKWH*( xSalestoGridPR+xRPS(nt,iCurIYr)*EPRPSPR(iCurIYr)/1000.*MC_JPGDP(iGenCapCostYr-(BaseYr-1))/MC_JPGDP(-2) ) & !adds scaled RPS credit
                 +xElecAvgUEC*xRetailElecPR             !plus own-use
              ENDIF
              ! Recompute value of energy savings with RPS credit of 1 for years where credit can switch (a la Markey 2009 Bill Provisions)
              IF(XEXCESSKWH.LT.0.) THEN
                XVALESAVEBASEadjRPS=xAnnualKWh*xRetailElecPradjRPS
              ELSE
                XVALESAVEBASE= xElecAvgUEC*xRetailElecPR   &                                                                   !own-use
                 +XEXCESSKWH*( xSalestoGridPR+1.0*EPRPSPR(iCurIYr)/1000.*MC_JPGDP(iGenCapCostYr-(BaseYr-1))/MC_JPGDP(-2) ) !grid sales with scaled RPS credit
              ENDIF
              ! Recompute value of energy savings without RPS credit for potential phase outs
              IF(XEXCESSKWH.LT.0.) THEN
                XVALESAVEBASEnoRPS=xAnnualKWh*xRetailElecPrnoRPS  ! own-use only
               ELSE
                XVALESAVEBASEnoRPS= XEXCESSKWH*xSalestoGridPR +xElecAvgUEC*xRetailElecPRnoRPS
              ENDIF

              ! Just in Case RPS Credits come through in years before they should be credited to residential:
              IF (CurCalYr .LT. iRPSStartYear) THEN
                XVALESAVEBASE=XVALESAVEBASEnoRPS
              ENDIF

              ! Zero out unused variables
              XBaseYrFUELCOST=0.
              XGASINPUT=0.
              XWATERHTGMMBTU=0.
              XBTUWASTEHEAT=0.
              XMAINTCOSTBASE=xMaintCst(NT,NV) * xCalcKW
              IF(LPRINT)WRITE (DGDAT,*) &
               "AC Price R&C", PELRSOUT(iDiv,iCurIYr,2), PELCMOUT(iDiv,iCurIYr,2), &
               "GridSalesPrice ", PELME(iDiv,iCurIYr), &
               "Deflators ", MC_JPGDP(iGenCapCostYr-(BaseYr-1)),MC_JPGDP(-2), &
               "RPS Credit (mills)", EPRPSPR(iCurIYr)
               !IF (LPRINT) WRITE(DGDAT,*) 'using old niche model', CurCalYr, iDiv, iNiche

              GOTO 200

 !-----------------------------
 ! -Setup for Wind Calculations
 !-----------------------------

 100          CONTINUE
              ! Wind generation is valued at the average residential electricity price.
              ! Convert to same year dollars as generation capital costs and THEN use
              !  inflation to maintain nominal dollars. $/kWh in year dollars of capital cost data
              xRetailElecPR=( PELRS(iDiv,iCurIYr)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
                 +xRPS(nt,iCurIYr)*EPRPSPR(iCurIYr)/1000. )*MC_JPGDP(iGenCapCostYr-(BaseYr-1))/MC_JPGDP(-2)     !own-use including scaled RPS credit
              xRetailElecPRadjRPS=( PELRS(iDiv,iCurIYr)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
                 +1.0*EPRPSPR(iCurIYr)/1000. )*MC_JPGDP(iGenCapCostYr-(BaseYr-1))/MC_JPGDP(-2)     !own-use including scaled RPS credit
              xRetailElecPRnoRPS= PELRS(iDiv,iCurIYr)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
                 *MC_JPGDP(iGenCapCostYr-(BaseYr-1))/MC_JPGDP(-2)     !own-use no RPS credit

              ! Account for total households with potential for wind penetration
              ! Assume xx% of households have lots of 0.5 acre or above and are appropriate
              WindAvailHH(iCurIYr,iDiv)= WindAvailHH(iCurIYr,iDiv) + &
               (HSEADD(CurCalYr,1,iDiv)+EH(CurCalYr,1,iDiv)) * xHHShare(iDiv,iNiche,iRateLevel) &
               *xRuralPctHH(iDiv,iNiche,iRateLevel)                                                    !Assumes rural households are suitable for wind

              ! CALCULATION OF KWH SUPPLIED FOR WIND
              !  Wind speed is in m/s
              !  xElEff represents relative efficiency of future technologies relative to today's models
              !  xMpS is the wind speed in meters per second and capacity factor is a cubic function of
              !  wind speed (.0645 -0.0670*xMpS +.0210*xMpS**2 -.0011*xMpS**3).
              xMpS=xWindSpeed(iDiv,iNiche,iRateLevel)
              xAnnualKWh=xElEff(NT,NV)/xElEff(nt,1)* &
               (.0645 -0.0670*xMpS +.0210*xMpS**2 -.0011*xMpS**3)*xKW(NT,NV)*8760.*xLossFac(NT,NV)	!kj

              ! Compare annual Wind generation to building use, value own-use at the retail price
              !  and grid sales at the grid price
              XEXCESSKWH=xAnnualKWh-xElecAvgUEC
              IF(XEXCESSKWH.LT.0.) THEN
                XVALESAVEBASE=xAnnualKWh*xRetailElecPr     ! own-use only
               ELSE
                XVALESAVEBASE= xElecAvgUEC*xRetailElecPR   &                                                                   !own-use
                 +XEXCESSKWH*( xSalestoGridPR+xRPS(nt,iCurIYr)*EPRPSPR(iCurIYr)/1000.*MC_JPGDP(iGenCapCostYr-(BaseYr-1))/MC_JPGDP(-2) ) !grid sales w/ scaled RPS credit
              ENDIF
              ! Recompute value of energy savings with RPS credit of 1 for years where credit can switch (a la Markey 2009 Bill Provisions)
              IF(XEXCESSKWH.LT.0.) THEN
                XVALESAVEBASEadjRPS=xAnnualKWh*xRetailElecPradjRPS
              ELSE
                XVALESAVEBASE= xElecAvgUEC*xRetailElecPR   &                                                                   !own-use
                 +XEXCESSKWH*( xSalestoGridPR+1.0*EPRPSPR(iCurIYr)/1000.*MC_JPGDP(iGenCapCostYr-(BaseYr-1))/MC_JPGDP(-2) ) !grid sales w/ scaled RPS credit
              ENDIF
              ! Recompute value of energy savings without RPS credit for potential phase outs
              IF(XEXCESSKWH.LT.0.) THEN
                XVALESAVEBASEnoRPS=xAnnualKWh*xRetailElecPrnoRPS  ! own-use only
               ELSE
                XVALESAVEBASEnoRPS= XEXCESSKWH*xSalestoGridPR +xElecAvgUEC*xRetailElecPRnoRPS
              ENDIF

              !Just in Case RPS Credits come through in years before they should be credited to residential:
              IF (CurCalYr .LT. iRPSStartYear) THEN
                XVALESAVEBASE=XVALESAVEBASEnoRPS
              ENDIF

             ! Zero out variables not relevant to Wind
             XBaseYrFUELCOST=0.
             XGASINPUT=0.
             XWATERHTGMMBTU=0.
             XBTUWASTEHEAT=0.
             xCalcKW=xKW(NT,NV)
             XMAINTCOSTBASE=xMaintCst(NT,NV) * xCalcKW
             ! End Setup for Wind Calculations
             GOTO 200 ! Jump to Cash Flow Model

 !-------------------------------------
 ! -Setup for Fuel Cell Calculations
 !-------------------------------------

 150          CONTINUE
              ! Fuel Cell generation is valued at the average residential electricity price.
              ! Convert to same year dollars as generation capital costs and THEN use
              !  inflation to maintain nominal dollars. $/kWh in year dollars of capital cost data
              xRetailElecPR=PELRS(iDiv,iCurIYr)*xRateScalar(iDiv,iNiche,iRateLevel) &
                *.003412*MC_JPGDP(iGenCapCostYr-BaseYr+1)/MC_JPGDP(-2)

              ! COMPUTE ANNUAL KWH GENERATION
              xAnnualKWh=xOperHours(NT) * xAvail(NT,NV) * xKW(NT,NV) * xLossFac(NT,NV)
              ! Average UEC for natural gas water heating (main & secondary) from RECS (Annual MMBtu per household)
              XWATERHTGMMBTU=18.1

              ! COMPUTE FUEL INPUT IN MMBTU
              XGASINPUT=.003412 * xKW(NT,NV)/xElEff(NT,NV) * xOperHours(NT) * xAvail(NT,NV)

              ! COMPUTE HEAT AVAILABLE FOR WATER HEATING IN MMBTU
              XBTUWASTEHEAT= (XGASINPUT-.003412 * xAnnualKWh)* xWhRecovery(NT,NV)
              IF(XBTUWASTEHEAT .LT. XWATERHTGMMBTU) XWATERHTGMMBTU=XBTUWASTEHEAT

              ! COMPUTE ANNUAL FUEL COST FOR FUEL CELL -- NET OF IMPUTED WATERHEATING COSTS
              XBaseYrFUELCOST = (XGASINPUT-XWATERHTGMMBTU) &
                *PNGRS(iDiv,iCurIYr)*MC_JPGDP(iGenCapCostYr-(BaseYr-1))/MC_JPGDP(-2)

              ! Compare annual Fuel Cell generation to building use, value own-use at the retail price
              !  and grid sales at the grid price
              XEXCESSKWH=xAnnualKWh-xElecAvgUEC
              IF(XEXCESSKWH.LT.0.) THEN
                XVALESAVEBASE=xAnnualKWh*xRetailElecPr !own-use
               ELSE
                XVALESAVEBASE= XEXCESSKWH*xSalestoGridPR & !grid sales
                 +xElecAvgUEC*xRetailElecPR !own-use
              ENDIF
              xCalcKW=xKW(NT,NV)
              XMAINTCOSTBASE=xMaintCst(NT,NV) * xCalcKW

200           CONTINUE

!----------------------------------------------------
!  CALCULATE PAYBACKS BY AVAILABLE TECHNOLOGY TYPE
!----------------------------------------------------

              !IF(LPRINT)WRITE(DGDAT,*) ' CALCULATING SIMPLE PAYBACK'
              xCalcEqCost = xEqCost*xCalcKW/xKW(NT,NV)
              !CALCULATE ANNUAL LEVELIZED PAYMENT
              XINTRATE=MC_RMMTG30CON(iCurIYr)/100.
              XDOWNPAY=XDOWNPAYPCT*xCalcEqCost
              XPAYMENT=XINTRATE/(1.-(1.+XINTRATE)**(-1.*XTERM))*(xCalcEqCost-XDOWNPAY)
              IF(Lprint) WRITE(DGDAT,*) ' PAYMENT',XPAYMENT,' Int Rate',XINTRATE

              !INITIALIZE CASH FLOW STARTING VALUES
              XCUMNETFLOW(1:35)=0.
              XOUTLAY(1)=XDOWNPAYPCT*xCalcEqCost
              XFUELCOST(1)=0.
              XFUELCOST(2)=XBaseYrFUELCOST
              XMAINTCOST(1)=0.
              XMAINTCOST(2)=XMAINTCOSTBASE
              XLOANBAL(1)=xCalcEqCost*(1.-XDOWNPAYPCT)
              XTAXCREDIT(1)=0.
              XTAXCREDIT(2)=0.
              XTAXCREDIT(3)=xCalcEqCost*xTaxCreditPct
              !Apply cap if there is one
              IF(xtaxcreditmaxKW .GT. 0.) XTAXCREDIT(3)=min(XTAXCREDIT(3),XTAXCREDITMAxKW*xCalcKW)
              IF(xtaxcreditmax .GT. 0.) XTAXCREDIT(3)=min(XTAXCREDIT(3),XTAXCREDITMAX)
              !Add census division subsidy (if any) for renewable technologies
              XTAXCREDIT(3)=XTAXCREDIT(3)+xCalcEqCost*xTxCrPct_Div(NV,iDiv,NT) ! 111dren
              XNETCASHFLOW(1)=-XOUTLAY(1)
              XCUMNETFLOW(1)=-XOUTLAY(1)
              iIntervalYrstoUse=iIntervalYrs(NT,NV)

              DO IYR=2,30
                XOUTLAY(IYR)=0.0
                XVALESAVE(IYR)=0.
                xKWH(IYR)=0.
                IF(FLOAT(IYR).LE.XTERM+1.)XOUTLAY(IYR)=XPAYMENT
                XINTAMT(IYR)=XLOANBAL(IYR-1)*XINTRATE
                XPRIN(IYR)=0.0
                IF(FLOAT(IYR).LE.XTERM+1.)XPRIN(IYR)=XPAYMENT-XINTAMT(IYR)
                XLOANBAL(IYR)=XLOANBAL(IYR-1)-XPRIN(IYR)
                !CURRENTLY NO DEPRECIATION ALLOWANCE FOR RESIDENTIAL TAXES KEEP FOR GENERALITY
                !XDEPR(IYR)=xCalcEqCost/XLIFE  ! STRAIGHT LINE DEPRECIATION
                XDEPR(IYR)=0.
                XTAXDEDUCT(IYR)=XTAXRATE*(XINTAMT(IYR-1)+XDEPR(IYR-1))+XTAXCREDIT(IYR)
                IF(IYR.GT.2) XFUELCOST(IYR)=0.
                IF(FLOAT(IYR).LE.(XLIFE+1.)) &
                  XFUELCOST(IYR)=XFUELCOST(2)*(1.+XINFLATION)**(IYR-2)
                XMAINTCOST(IYR)=0.

              !Inverters:
              ! Calculate both annual and discrete maintenance costs.  Initially designed to accommodate
              !   discrete solar PV inverter replacements; this is also used for wind.
              IF(iyr.GT.2 .AND. IMod(Iyr-2,iIntervalYrstoUse).EQ.0 .AND. Iyr.NE.29 .AND. Iyr.NE.30) THEN
                IF(FLOAT(IYR).LE.(XLIFE+1.)) &
                  XMAINTCOST(IYR)=(XMAINTCOSTBASE+xIntervalCst(NT,NV)* xCalcKW)*(1.+XINFLATION)**(IYR-2)
                  !Adjust the interval years for subsequent (if needed) discrete replacement
                  !The use of IYR reflects "average" progress in extending inverter lives for subsequent replacements
                  iIntervalYrstoUse=2*iIntervalYrstoUse+IYR
                  !diagnostic only -- IF (lprint3) WRITE(dgdat,*) 'interval adjustment***  orig ', iyr, 'new ',iIntervalYrstoUse
                ELSE
                  IF(FLOAT(IYR).LE.(XLIFE+1.)) &
                    XMAINTCOST(IYR)=XMAINTCOSTBASE*(1.+XINFLATION)**(IYR-2)
                  ENDIF

              IF(FLOAT(IYR).LE.(XLIFE+1.)) THEN
               XVALESAVE(IYR)=XVALESAVEBASE * (1.+XINFLATION)**(IYR-2) * (1.-xDegradation)**(IYR-2)
               ! Sunset the RPS if applicable
               IF(NT.EQ.1 .OR. NT.EQ.3) THEN
               IF(CurCalYr+iyr-1 .GT. iRPSPhaseOutYear) &
               XVALESAVE(IYR)=XVALESAVEBASEnoRPS * (1.+XINFLATION)**(IYR-2) * (1.-xDegradation)**(IYR-2)
               !Adjust
               IF(CurCalYr .GE. iRPSStartYear .AND. iyr .GT. iNumYrsatRPSBaseRate(nt,iCurIYr)) &
                 XVALESAVE(IYR)=XVALESAVEBASEadjRPS * (1.+XINFLATION)**(IYR-2) * (1.-xDegradation)**(IYR-2)
               !Remove Credits if number of years < 30
               IF(CurCalYr .GE. iRPSStartYear .AND. iyr .GT. iNumRPSCreditYrs(nt,iCurIYr)) &
                 XVALESAVE(IYR)=XVALESAVEBASEnoRPS * (1.+XINFLATION)**(IYR-2) * (1.-xDegradation)**(IYR-2)
               !Remove Credits if CurCalYr is before start of RPS credits
               IF(CurCalYr .LT. iRPSStartYear) &
                 XVALESAVE(IYR)=XVALESAVEBASEnoRPS * (1.+XINFLATION)**(IYR-2) * (1.-xDegradation)**(IYR-2)
              ENDIF
              ENDIF

              IF(FLOAT(IYR).LE.(XLIFE+1.)) xKWH(IYR)=xAnnualKWh * &
                (1.-xDegradation)**(IYR-2)
              XNETCASHFLOW(IYR)=-XOUTLAY(IYR)-XFUELCOST(IYR)-XMAINTCOST(IYR) &
                       +XTAXDEDUCT(IYR)+XVALESAVE(IYR)
              XCUMNETFLOW(IYR)=XCUMNETFLOW(IYR-1)+XNETCASHFLOW(IYR)
              ENDDO  !cash flow year loop (iyr)

             ! Print Switch for Detailed Cash Flow Model Results
             IF(LPRINT.AND.LPRINT2.AND.iDiv.EQ.1.AND.iNiche.EQ.1.AND.iratelevel.EQ.1) &
              WRITE(DGDAT,*) 'YEAR    OUTLAY TAXDEDUCT  FUELCOST MAINTCOST    ESAVE  NETCASHFL   CUM   Annual kWh', aEquipName(NT,NV)

             iPayback(1:30)=1

            ! SEARCH FOR POSITIVE CASH FLOW, PERSISTING FOR SEVERAL YEARS
            !  USE "0" TO INDICATE POSITIVE CASH FLOW MEETING PERSISTENCE CRITERIA
            DO IYR=1,20 !Stop at 20 because of look-ahead; assume that greater than 20-year paybacks are too long
            IF(XCUMNETFLOW(iyr  ).GE.0. .AND. XCUMNETFLOW(iyr+1).GE.0. .AND. &
             xcumnetflow(iyr+2).GE.0. .AND. xcumnetflow(iyr+3).GE.0. .AND. &
             xcumnetflow(iyr+4).GE.0. .AND. xcumnetflow(iyr+5).GE.0. .AND. &
             xcumnetflow(iyr+6).GE.0. .AND. xcumnetflow(iyr+7).GE.0. .AND. &
             xcumnetflow(iyr+8).GE.0.) iPayback(IYR)=0
            ! SWITCHABLE DIAGNOSTICS:
             IF(LPRINT.AND.LPRINT2.AND.iDiv.EQ.1.AND.iNiche.EQ.1.AND.iratelevel.EQ.1) &
              WRITE(DGDAT,10)IYR-1,XOUTLAY(IYR),XTAXDEDUCT(IYR),XFUELCOST(IYR), &
              XMAINTCOST(IYR), XVALESAVE(IYR), XNETCASHFLOW(IYR), &
              XCUMNETFLOW(IYR),xKWH(IYR),iPayback(iyr),xAnnualKWh
            ENDDO

 10   FORMAT(1X,I4,8F10.2,i4,f10.2)

            xSimplePayback=29.
            ilife=ifix(xlife)
            DO IYR=1,30
              IF(iPayback(iyr).EQ.0)THEN
                xSimplePayback=FLOAT(IYR-1)  !Allow 1-year and less simple paybacks
             !  Get the first year of a positive cumulative cash flow and compute how long at that year's net cash flow it would take to build the cumulative balance in the first positive year.
             !  The interpolated years to positive cash flow are then equal to the simple payback years minus ending cumulative cash flow balance / net cash flow in "iYr".
                IF(xSimplePayback.LT.1.) xSimplePayback=1.
                IF (iyr.LT.15) xSimplePayback= xSimplePayback - XCUMNETFLOW(IYR)/XNETCASHFLOW(IYR) !relax distributed generation cap in new
             !    Diagnostic warning
                  IF (xSimplePayback .LT. 0.) THEN
!                   WRITE(DGDAT,*) '**Negative Payback** CurCalYr,NT,NV,iDiv',CurCalYr,NT,NV,iDiv,'EQ CLASS ', & !DGrate - turned off for DGrate testing
!                       aEquipName(NT,NV), '1ST YEAR ', iFirstYr(NT,NV), 'PAYBACK=', xSimplePayback, 'xPen= ',xPen !DGrate - turned off for DGrate testing
                  ENDIF
                GOTO 11  ! CONTINUE once payback is identified
              ENDIF
             ! If here, investment never achieves positive cash flow; payback is set to 29 years
            ENDDO

!-------------END OF CALC PAYBACK

!--------------------------------------------------
!  CALCULATE PENETRATION BASED PAYBACK PERIOD
!--------------------------------------------------
11          CONTINUE
            xMaxPen=xPenParm/xSimplePayback
            ! Maximum penetration into new construction capped at 75%.
            ! The cap would affect projects with paybacks of less than approximately 5 months.
            XValue=FLOAT(CurCalYr-(RECSYear+1))
            IF(XValue.GT.25.0) XValue=25.0  ! currently limit penetration beyond 2030	!kj - hasn't been updated since 2005 was base year; 25 needs to be changed, or this line needs to be removed
            xPen=min(0.75,xMaxPen-xMaxPen/(1.+xMaxPen*EXP(xAlpha*(XValue-xSimplePayback))))	!kj

            IF(LPRINT) WRITE(DGDAT,*) 'CurCalYr,NT,NV,iDiv',CurCalYr,NT,NV,iDiv,'EQ CLASS ', & !DGrate - turned off for DGrate testing
              aEquipName(NT,NV),'1ST YEAR ',iFirstYr(NT,NV),'PAYBACK=',xSimplePayback,'xPen= ',xPen !DGrate - turned off for DGrate testing

            !  CODE TO PRINT 20 YEARS OF PENETRATION DATA FOR TESTING
            !      DO IYR=CurCalYr,CurCalYr+20
            !          xPen2=xMaxPen-xMaxPen/(1.+xMaxPen*EXP(xAlpha*((iyr-(RECSYear+1))-xSimplePayback)))
            !          IF(LPRINT)WRITE(DGDAT,'(1X,4I8,4e16.3)') CurCalYr,RECSYear,IYR,NV,xMaxPen,xAlpha,xSimplePayback,xPen2
            !          IF(LPRINT)WRITE(DGDAT,*) 'xPen2',CurCalYr,RECSYear,IYR,NV,xAlpha,xMaxPen,xSimplePayback,xPen2
            !      ENDDO

            ! Turn off endogenous builds to avoid double-counting of historical data
            ! (Increasing LastSTEOYear in RESDREP include file for STEO benchmarking prior to updating
            !  exogenous capacity will cause PV to nearly flatten out between SEDS year and MER year)

            !Solved issue by creating variable set in RSGENTK.txt input file with last year of historical data (historical capacity is input as exogenous capacity)  !ExogHist
            IF(CurCalYr.LE.iExogHistYr(NT)) xPen=0.0  !ExogHist No endogenous model builds for any DG technology in years with historical data

            !  Account for penetration into existing housing units
            !  Penetration into existing is based on penetration into new construction with an assumed upper bound
            xExistPen=min(xPen/40.0,0.005)  !Penetration cap for existing	!kj

            xTemp=xPen*xInxDecay(iDiv,iCurIYr)*HSEADD(CurCalYr,1,iDiv) &             !Penetration into New Construction !INXLIMIT
               +(xExogPen(iCurIYr,iDiv,NT)-xExogPen(iCurIYr-1,iDiv,NT))/xCalcKW  &   !Add Current Year Exogenous Units  !convert to kW
               +xExistPen*xInxDecay(iDiv,iCurIYr)*(EH(CurCalYr,1,iDiv)-Units(iCurIYr-1,iDiv,NT))  !Existing Construction
            xTemp=xTemp*xHHShare(iDiv,iNiche,iRateLevel)                 !Scale down to suitable HH for niche share of HH
            xTemp = FLOAT(ifix(xTemp*100.+.5))/100.                      !Eliminate fractional units < 0.01
            xTempHH=(HSEADD(CurCalYr,1,iDiv)+EH(CurCalYr,1,iDiv))*xHHShare(iDiv,iNiche,iRateLevel)  !HH in niche
            !--END OF PENETRATION

            ! Accumulators for Technical Potential and Other Summary Statistics
            ! Note xSqftperKW varies by year and is calculated above
            IF (nt==1) THEN
              SolarPVTechPotentialMW(iCurIYr,iDiv)=SolarPVTechPotentialMW(iCurIYr,iDiv)+ &
               (xTempHH*xpctPVSuitable*xRoofAreaperHH(iDiv,iNiche,iRateLevel)/(xSqftperKW*1000.))                     !PVgen
              SolarPVInstalledMW(iCurIYr,iDiv)=SolarPVInstalledMW(iCurIYr,iDiv)+xTemp*xCalcKW/1000.
              SolarPVAvailRoofArea(iCurIYr,iDiv)= SolarPVAvailRoofArea(iCurIYr,iDiv) + &
               (xTempHH*xpctPVSuitable*xRoofAreaperHH(iDiv,iNiche,iRateLevel)/10.**6)                                 !PVgen
              SolarPVUsedRoofArea(iCurIYr,iDiv)=SolarPVUsedRoofArea(iCurIYr,iDiv)+(xTemp*xCalcKW*xSqftperKW/10**6)    !PVgen
            ENDIF
            IF (nt==3) THEN
             WindTechPotentialMW(iCurIYr,iDiv)=WindTechPotentialMW(iCurIYr,iDiv)+xTempHH*xCalcKW/1000.*xRuralPctHH(iDiv,iNiche,iRateLevel)
             WindInstalledMW(iCurIYr,iDiv)=WindInstalledMW(iCurIYr,iDiv)+xTemp*xCalcKW/1000.
             WindAvailHH(iCurIYr,iDiv)=WindAvailHH(iCurIYr,iDiv)+xTempHH*xRuralPctHH(iDiv,iNiche,iRateLevel)
            ENDIF
            !--End Summary Calculations

            ! Now that new units are determined, calculate associated estimates of generation in Trills, capacity,
            !   own use generation, fuel usage, offsets to energy consumption for hot water & space heating
            !   and investment.
            xUnits = xUnits + xTemp
            xTrills = xTrills + xTemp*xAnnualKWh*3412./10.**12 !Trills
            xCapacity = xCapacity + xTemp*xCalcKW           !in KW for now
            IF(xAnnualKWh.GT.xElecAvgUEC) THEN
              xTrillsOwnUse = xTrillsOwnUse + xTemp*xElecAvgUEC*3412./10.**12
             ELSE
              ! BUILDING CONSUMES ALL OF ITS OWN GENERATION
              xTrillsOwnUse = xTrillsOwnUse + xTemp*xAnnualKWh*3412./10.**12
            ENDIF
           xfuelusage = xfuelusage + xTemp*xgasinput/10.**6   !Trills
           xhwbtu = xhwbtu +xTemp*XWATERHTGMMBTU/10.**6       !Trills
           xInvest = xInvest + xTemp*xCalcEqCost/10.**6       !$Millions

           ! Label 25 for skipping unpopulated niches
 25        CONTINUE
         ENDDO  !Rate Levels (iRateLevel)
       ENDDO  !Climate Zone Niches (iNiche)

       GOTO 81  !SKIP OVER NEW PV CODE IF HERE

 26       CONTINUE

!--------------------------------------------
!  NEW ZIP CODE-BASED PV MODEL        !PVPen
!--------------------------------------------

          ! THIS IS JUST OUTSIDE OF THE RATE LEVEL, CLIMATE ZONE NICHE LOOPS
          !  ALL RESULTS HERE ARE FOR CENSUS DIVISIONS
          ! USEZIPMODEL IF HERE
          xSqftperKW=77.*.14/xElEff(NT,NV)

          !National and Division-level input variables
          IF(CurCalYr.GT.EstYear) THEN
            INTRATE=(MC_RMMTG30CON(iCurIYr)/100.)            !Convert to a decimal fraction

      !111dren add the divisional tax credit into the calculation of PVPRICE in the line of code below
             !PVPrice equals RSGENTK installed equipment cost, net of any tax credit, times an adjustment factor to scale to the econometric ZIP code model's cost level [PVPrice in RGENTK]
             PVPrice=(xadjcost+xInstCost(NT,NV))*(1.-xtaxcreditpct-xTxCrPct_Div(nv,iDiv,nt) )*InputPVPrice/(xCapCost(1,EstYear-RECSYear+1)/1000*(1-xTxCrPct(1,EstYear-RECSYear+1)))  !PVmultiplier
               !IF (LPRINT) WRITE(DGDAT,*) 'PVmultiplier_calc', InputPVPrice/(xCapCost(1,EstYear-RECSYear+1)/1000*(1-xTxCrPct(1,EstYear-RECSYear+1))), 'EstYear-RECSYear+1', EstYear-RECSYear+1, &    !PVmultiplier
               !'xCapCost(1,EstYear-RECSYear+1)', xCapCost(1,EstYear-RECSYear+1), 'xTxCrPct(1,EstYear-RECSYear+1)', xTxCrPct(1,EstYear-RECSYear+1)                                    !PVmultiplier

!             PVPrice=(xAdjCost+xInstCost(NT,NV))*(1.-xTaxCreditPct-xTxCrPct_Div(nv,iDiv,nt) )*InputPVPrice/((xCapCost(1,EstYear-RECSYear+1)+xInstCost(1,EstYear-RECSYear+1))/1000*(1-xTxCrPct(1,EstYear-RECSYear+1)))  !PVmultiplier	!kj - The line above should use total installed cost, not just capital cost. Test this code further, but verify costs to sum)
               !IF (LPRINT) WRITE(DGDAT,*) 'PVmultiplier_calc', InputPVPrice/((xCapCost(1,EstYear-RECSYear+1)+xInstCost(1,EstYear-RECSYear+1))/1000*(1-xTxCrPct(1,EstYear-RECSYear+1))), 'EstYear-RECSYear+1', EstYear-RECSYear+1, &    !PVmultiplier	!kj - The line above should use total installed cost, not just capital cost. Test this code further, but verify costs to sum)
               !'xCapCost(1,EstYear-RECSYear+1)', xCapCost(1,EstYear-RECSYear+1), 'xInstCost(1,EstYear-RECSYear+1)', xInstCost(1,EstYear-RECSYear+1), 'xTxCrPct(1,EstYear-RECSYear+1)', xTxCrPct(1,EstYear-RECSYear+1)  !PVmultiplier	!kj - The line above should use total installed cost, not just capital cost. Test this code further, but verify costs to sum)

			   MonthlyPayment= ( PVPRICE * (INTRATE/12.) / (1.-(1.+INTRATE/12.)**(-360.)) ) !assume a 360 month mortgage
            !IF (LPRINT) WRITE(DGDAT,*) 'USING NEW MODEL National Level Variables ', IntRate, PVPrice, MonthlyPayment
          ENDIF

          !Process ZIP codes
           DO i=1,NumZIPs
             IF(CenDiv(i) .NE. iDiv) CYCLE
             CumUnits(i) = 0.0
             xSolarIns=INSOL(i)                                                 !PVgen
             !Transform solar insolation to account for unknown orientations    !PVgen
             xSolarIns= -1.0533 + 1.4325*xSolarIns - 0.0652*xSolarIns**2        !PVgen	!kj - update values using PVwatts?

             xCalcKW=xKW(nt,nv)  !set size to menu capacity

             xAnnualKWh=xElEff(NT,NV)*xSolarIns*365.25/10.8*xSqftperKW*xCalcKW*xLossFac(NT,NV)

             ! j is the model switch based on the 3 sets of coefficients
             !   if PureHurdle is zero, THEN j = 1 using the first subscript position for coefficients
             !   if PureHurdle is 1, THEN j = 2 using the 2nd subscript position for coefficients (urban model)
             !   if PureHurdle is 0, and ruralzip is 1, THEN j = 3 using the 3rd subscript position for coefficients (rural model)
             j = PureHurdle(i)+1
             IF (ruralzip(i) .EQ. 1) j=3
             ! The first array element of the coefficient is for the logit model, the 2nd for the negative binomial.
             ! Not all variables for specific model variants are non-zero, but this is programmed to allow flexible
             !   model evolution.
             ! Set Inputs to Equations:
                IF(curitr.EQ.1 .AND. CurCalYr.GT.EstYear) THEN  !Subsequent to the estimation year update lag variables on first iteration
                  Income_L(i)=Income(i)
                  Households_L(i)=Households(i)
                  ElecRate_L(i)=ElecRate(i)
                  PopDensity_L(i)=PopDensity(i)
                  Lag2Installs(i)=Lag1Installs(i)
                  Lag1Installs(i)=ProjectedInstalls(i)
                ENDIF !curitr=1 for years after econometric model estimation year

                IF(CurCalYr.GT.EstYear) THEN  !Subsequent to the estimation year update ZIP code level variables
                  Income(i)=Income_L(i)*(MC_YPDR(CenDiv(i),iCurIYr)/MC_YPDR(CenDiv(i),iCurIYr-1)) / &
                   ( ( EH(CurCalYr,1,iDiv)+NH(CurCalYr,1,iDiv)+EH(CurCalYr,2,iDiv)+NH(CurCalYr,2,iDiv)+EH(CurCalYr,3,iDiv)+NH(CurCalYr,3,iDiv) ) &
                   / ( EH(CurCalYr-1,1,iDiv)+NH(CurCalYr-1,1,iDiv)+EH(CurCalYr-1,2,iDiv)+NH(CurCalYr-1,2,iDiv)+EH(CurCalYr-1,3,iDiv)+NH(CurCalYr-1,3,iDiv) ) )
                  Households(i)=Households_L(i)*(EH(CurCalYr,1,iDiv)+NH(CurCalYr,1,iDiv)+EH(CurCalYr,2,iDiv)+NH(CurCalYr,2,iDiv)+EH(CurCalYr,3,iDiv)+NH(CurCalYr,3,iDiv)) &
                   /(EH(CurCalYr-1,1,iDiv)+NH(CurCalYr-1,1,iDiv)+EH(CurCalYr-1,2,iDiv)+NH(CurCalYr-1,2,iDiv)+EH(CurCalYr-1,3,iDiv)+NH(CurCalYr-1,3,iDiv))
				  PopDensity(i)=PopDensity_L(i)*MC_NP65A(iCurIYr)/MC_NP65A(iCurIYr-1)
                  !THIS CODE IS USED TO SWITCH FROM RETAIL COOLING ELECTRICITY RATES (PELRSOUT(iDiv,iCurIYr,2)) TO WEIGHTED MARGINAL/WHOLESALE (PELME(iDiv,ICURIYR)) AND RETAIL RATE BLEND  !DGrate
                  IF (DGrateBlend) THEN  !DGrate - If set to TRUE in RSGENTK.txt, then use blended electricity rate starting in DGrateYr
                    IF (CURCALYR.LT.DGrateYr) THEN  !DGrate
                      ElecRate(i)=ElecRate_L(i)*( (PELRSOUT(CenDiv(iDiv),iCurIYr,2)*.003412+xRPS(nt,iCurIYr)*EPRPSPR(iCurIYr)/1000. ) ) / &    !own-use including scaled RPS credit  !DGrate
                       ( (PELRSOUT(CenDiv(iDiv),iCurIYr-1,2)*.003412+xRPS(nt,iCurIYr-1)*EPRPSPR(iCurIYr-1)/1000. ) )  !DGrate
					ELSEIF (CURCALYR.EQ.DGrateYr) THEN  !DGrate - This is a transition where where the prior year uses 100% retail space cooling electricity rate [PELRSOUT(D,Y,2)] and the current year uses weighted retail/marginal electricity rate
                      ElecRate(i)=ElecRate_L(i)*( (((PELME(iDiv,iCurIYr)*DGmargWt(iDiv)) + (PELRSOUT(iDiv,iCurIYr,2)*DGretWt(iDiv)))*.003412+xRPS(nt,iCurIYr)*EPRPSPR(iCurIYr)/1000. ) ) / &    !own-use including scaled RPS credit  !DGrate
                       ( (PELRSOUT(CenDiv(iDiv),iCurIYr-1,2)*.003412+xRPS(nt,iCurIYr-1)*EPRPSPR(iCurIYr-1)/1000. ) )  !DGrate
					ELSEIF (CURCALYR.GT.DGrateYr) THEN  !DGrate
                      ElecRate(i)=ElecRate_L(i)*( (((PELME(iDiv,iCurIYr)*DGmargWt(iDiv)) + (PELRSOUT(iDiv,iCurIYr,2)*DGretWt(iDiv)))*.003412+xRPS(nt,iCurIYr)*EPRPSPR(iCurIYr)/1000. ) ) / &    !own-use including scaled RPS credit  !DGrate
                       ( (((PELME(iDiv,iCurIYr-1)*DGmargWt(iDiv)) + (PELRSOUT(iDiv,iCurIYr-1,2)*DGretWt(iDiv)))*.003412+xRPS(nt,iCurIYr-1)*EPRPSPR(iCurIYr-1)/1000. ) )  !DGrate
                    ENDIF  !DGrate
                  ELSE  !DGrate
                    ElecRate(i)=ElecRate_L(i)*( (PELRSOUT(CenDiv(iDiv),iCurIYr,2)*.003412+xRPS(nt,iCurIYr)*EPRPSPR(iCurIYr)/1000. ) ) / &    !own-use including scaled RPS credit  !DGrate
                     ( (PELRSOUT(CenDiv(iDiv),iCurIYr-1,2)*.003412+xRPS(nt,iCurIYr-1)*EPRPSPR(iCurIYr-1)/1000. ) )  !DGrate
                  ENDIF  !DGrate
                ENDIF

             !PVcontagion
             ! Here we calculate adjustment factors for the lag coefficients clag1 and clag2.
             ! The factors will be decreasing functions of the number of years between the projection year 
             ! (CurCalYr) and the estimation year for the econometric model (EstYear).  
             ! The factors will represent the decay in the social or “contagion effect” of solar PV.

             ! Calculate factors  !PVcontagion
             ! We declare the factors as REAL*8 above
             IF(CurCalYr.GT.EstYear) THEN  !to avoid dividing by zero  !PVcontagion
             NumYearsPV = CurCalYr-EstYear  !PVcontagion
             factor0 = 1                      !PVcontagion - used to turn off contagion effect decay
             factor1 = 1/(NumYearsPV**0.050)  !PVcontagion
             factor2 = 1/(NumYearsPV**0.055)  !PVcontagion
             factor3 = 1/(NumYearsPV**0.060)  !PVcontagion
             factor4 = 1/(NumYearsPV**0.090)  !PVcontagion !DeepSolar
             !factor5 = exp(-NumYearsPV)  !compiler doesn't appear to like negative exponential argument  !PVcontagion
             !factor6 = exp(2*(-NumYearsPV))  !compiler doesn't appear to like negative exponential argument  !PVcontagion
             ENDIF

             ! For each individual test, we set factor equal to one of the test values  !PVcontagion

             factor = factor4  !PVcontagion !DeepSolar

             !Calculate numerator and denominator components for projections (these are REAL*8 to prevent overflows)
               xLogit= cint (1,j) + chh (1,j)*HouseHolds(i) +cpd  (1,j)*PopDensity(i)   +factor*cinc (1,j)*Income(i) + &  !PVcontagion
                       factor*cer (1,j)*ElecRate(i)   +ccdd (1,j)*LagCDD(i)       +cpmt (1,j)*MonthlyPayment  + &  !PVcontagion
                       cir (1,j)*INTRATE       +factor*clag1(1,j)*Lag1Installs(i) +factor*clag2(1,j)*lag2Installs(i) + &  !PVcontagion
                       cpvp(1,j)*PVPRICE       +cins (1,j)*Insol(i)
               xNegBinom= cint (2,j) + chh (2,j)*HouseHolds(i) +cpd  (2,j)*PopDensity(i)   +factor*cinc (2,j)*Income(i) + &  !PVcontagion
                       factor*cer (2,j)*ElecRate(i)   +ccdd (2,j)*LagCDD(i)       +cpmt (2,j)*MonthlyPayment  + &  !PVcontagion
                       cir (2,j)*INTRATE       +factor*clag1(2,j)*Lag1Installs(i) +factor*clag2(2,j)*lag2Installs(i) + &  !PVcontagion
                       cpvp(2,j)*PVPRICE       +cins (2,j)*Insol(i)

             !Compute hurdle model additions to PV households with a test for overflows
               IF(IsNaN(exp(xLogit+xNegBinom) / ( 1. + exp(xLogit) )) ) THEN
                 xTemp=0.
                 !IF (LPRINT) WRITE(DGDAT,*) 'USING NEW MODEL :: NAN encountered at ZIP ', state(i),zipcode(i),xLogit,xNegBinom,xTemp
                 !IF (LPRINT) WRITE(DGDAT,*) '    Model, HH, PopDens, Insol, ElecRate ',   j,households(i),popdensity(i),insol(i),elecrate(i)
                 !IF (LPRINT) WRITE(DGDAT,*) '    Coefficients logit',  chh(1,j), cpd(1,j),cins(1,j),cer(1,j)
                 !IF (LPRINT) WRITE(DGDAT,*) '    Coefficients binom',  chh(2,j), cpd(2,j),cins(2,j),cer(2,j)
               ELSE
                 xTemp= REAL(  exp(xLogit+xNegBinom) / ( 1. + exp(xLogit) )  )
                 IF(CurCalYr .GT. EstYear) xTemp= xTemp*xInxDecay(iDiv,iCurIYr)
                 ENDIF
                 IF(IsNaN(xTemp)) THEN
                   !print warning message somewhere, for now:
                   !IF (LPRINT) WRITE(DGDAT,*) ':: NAN encountered at ZIP ',zipcode(i)
                   xTemp=0.   !reset xTemp before additional use
                 ENDIF
                 ProjectedInstalls(i)=xTemp  !save this year's projected installs for setting lags in next projection year
                 xUnits=xUnits + xTemp       !accumulate ZIP code results to the census division level
                 CumUnits(i)=CumUnits(i)+xTemp !also accumulate units within a ZIP code for testing penetration level
              !If penetration exceeds a particular value back out additional units
                IF(CumUnits(i)/Households(i) .GT. 0.80) THEN !80% is limit on share of households that can have PV in each ZIP code
                  xUnits=xUnits-xTemp
                  CumUnits(i)=CumUnits(i)-xTemp
                  xTemp=0.
                ENDIF

             !Temporary Test for Consistency with R Coded Model
			 IF(LPRINT .AND. CurCalYr .GE. EstYear .AND. CurCalYr .LE. DGrateYr) THEN  !DGrate
               IF (xTemp-ModelInstalls(i) .GT. .001) THEN
!                 WRITE(DGDAT,*) ' Model disagreement for ZIP ',state(i),zipcode(i), xTemp, modelinstalls(i)
!                 WRITE(DGDAT,949) 'CenDiv(i), zipcode(i), CurCalYr, xLogit, xNegBinom, xTemp, xUnits ', CenDiv(i),zipcode(i),CurCalYr,xLogit,xNegBinom,xTemp,xUnits  !DGrate
!                 949  FORMAT(A,1X,I2,1X,I8,1X,I8,1X,4F12.4)  !DGrate				   
!                 WRITE(DGDAT,*) ' Model, HH, PopDens, Insol, ElecRate ',   j,households(i),popdensity(i),insol(i),elecrate(i)
!                 WRITE(DGDAT,*) ' Coefficients logit',  chh(1,j), cpd(1,j),cins(1,j),cer(1,j)
!                 WRITE(DGDAT,*) ' Coefficients binom',  chh(2,j), cpd(2,j),cins(2,j),cer(2,j)
               ENDIF
             ENDIF

             !Add tax credit for penetration calculation of payment (?)
              xTrills= xTrills + xTemp*xAnnualKWh*3412./10.**12  !beware of mixed mode: bad results if coded as "/10**12" !!!

            xElecAvgUEC= 0.
            xElecAvgUEC= ((RSFLCN(iCurIYr,2,iDiv) * 10.**12) / 3412.) / (EH(CurCalYr,1,iDiv) + NH(CurCalYr,1,iDiv) + EH(CurCalYr,2,iDiv) +  &    !PVownuse  !DGreport - consumption includes onsite own-use generation
                          NH(CurCalYr,2,iDiv) + EH(CurCalYr,3,iDiv) + NH(CurCalYr,3,iDiv))  !Average electricity consumption (kWh) per household  !PVownuse

            IF(xAnnualKWh.GT.xElecAvgUEC) THEN  !If PV generation exceeds average consumption per household...    !PVownuse
              xTrillsOwnUse = xTrillsOwnUse + xTemp*xElecAvgUEC*3412./10.**12    !PVownuse
             ELSE                                                                !PVownuse
              ! BUILDING CONSUMES ALL OF ITS OWN GENERATION                      !PVownuse
              xTrillsOwnUse = xTrillsOwnUse + xTemp*xAnnualKWh*3412./10.**12     !PVownuse
            ENDIF                                                                !PVownuse

            IF (xTrillsOwnUse .GT. xTrills) THEN                                 !PVownuse
             !Prevents negative sales to grid                                    !PVownuse
             xTrillsOwnUse= xTrills                                              !PVownuse
            ENDIF                                                                !PVownuse

            IF (xTrillsOwnUse .LT. 0.) THEN                                      !PVownuse
             !Prevents negative generation for own use                           !PVownuse
             xTrillsOwnUse= 0.                                                   !PVownuse
            ENDIF                                                                !PVownuse

           ENDDO !Process ZIP codes

        !Print DGrate variables to RDGENOUT.txt
          IF (LPRINT .AND. CURITR.EQ.1 .AND. IDIV.EQ.1) THEN  !DGrate - Write header only during first iteration and before first census division
            WRITE(DGDAT,*) 'CD ICURIYR Cal_Year Blended_ElecRate PELRSOUT PELME'  !DGrate
          ENDIF  !DGrate
          IF (LPRINT .AND. CURITR.EQ.1) THEN  !DGrate - Write header only during first iteration
            WRITE(DGDAT,909) IDIV,ICURIYR,CurCalYr,((PELME(IDIV,ICURIYR)*DGmargWt(IDIV))+(PELRSOUT(IDIV,ICURIYR,2)*DGretWt(IDIV))),PELRSOUT(IDIV,ICURIYR,2),PELME(IDIV,icuriyr)  !DGrate
            909   FORMAT(I2,1X,I2,1X,I4,4F8.4)  !DGrate
          ENDIF !DGrate

          !IF (LPRINT) WRITE(DGDAT,*) 'Using New Model', CurCalYr, iDiv, xUnits, xTrills
          ! CREATE CENSUS DIVISION RESULTS FOR PV (NT==1) HERE
          Units(iCurIYr,iDiv,NT)=Units(iCurIYr-1,iDiv,NT)+ xUnits
          xCapacity=xUnits*xCalcKW  !in kW
          Cap(iCurIYr,iDiv,NT)=Cap(iCurIYr-1,iDiv,NT)+ xCapacity

          !xTrills= xUnits*xAnnualKWh*3412./10.**12  !insert NREL equation this is a per kW calculation so multiply by above and convert to Trills
          Trills(iCurIYr,iDiv,NT)=Trills(iCurIYr-1,iDiv,NT)+ xTrills
!          xTrillsOwnUse=xTrills   !placeholder; base this on RECS by census division  !This placeholder was never filled.  Calculations added above  !PVownuse

          TrillsOwnUse(iCurIYr,iDiv,NT)= TrillsOwnUse(iCurIYr-1,iDiv,NT) +xTrillsOwnUse
          xInvest=xUnits*xEqCost/10.**6   !PVinvest
          x111dRenSub(iCurIYr,iDiv,nt)=xInvest*xTxCrPct_Div(nv,iDiv,nt)  !111dren

          Invest(iCurIYr,iDiv,NT)=xInvest  !($mill)

          IF (iCurIYr .LE. EstYear-BaseYr+2 .AND. PVzipcalib .AND. NT .EQ. 1) THEN    !Used to calibrate to historical exogenous PV capacity in EstYear and the following year    !PVzipcalib	!kj - should the +2 be set to <PVzipcalibyear, or ExogPVlastDataYear/ExogPVhistYear>-ESTYEAR?
           ExogPVMistie(iCurIYr,iDiv)= 0.                                                                                                                        !PVzipcalib
           !xExogPen is exogenous PV capacity (NT=1) in kW from RSGENTK in index year (EstYear-BaseYr+1; 2015=26)                                                !PVzipcalib
           ExogPVMistie(iCurIYr,iDiv)= xExogPen(iCurIYr,iDiv,1) - Cap(iCurIYr,iDiv,1)                                                                            !PVzipcalib
!           WRITE(DGDAT,*)  'ExogPVMistie_Test_1', iCurIYr, iDiv, xExogPen(iCurIYr,iDiv,1), Cap(iCurIYr,iDiv,1), ExogPVMistie(iCurIYr,iDiv), xUnits, &            !PVzipcalib
!            xCalcKW, xAnnualKWh, xElecAvgUEC, ( EH(CurCalYr,1,iDiv) + NH(CurCalYr,1,iDiv) + EH(CurCalYr,2,iDiv) + NH(CurCalYr,2,iDiv) + EH(CurCalYr,3,iDiv) + &  !PVzipcalib
!            NH(CurCalYr,3,iDiv)), QELRS(iDiv,iCurIYr), Trills(iCurIYr,iDiv,1), TrillsOwnUse(iCurIYr,iDiv,1)                                                      !PVzipcalib
           Cap(iCurIYr,iDiv,1)= Cap(iCurIYr,iDiv,1) + ExogPVMistie(iCurIYr,iDiv)                                                                                 !PVzipcalib
           Units(iCurIYr,iDiv,1)= Units(iCurIYr,iDiv,1) + (ExogPVMistie(iCurIYr,iDiv) / xCalcKW)                                                                 !PVzipcalib
           xUnits=Units(iCurIYr,iDiv,1)-Units(iCurIYr-1,iDiv,1)  !PVinvest
           xCapacity=xUnits*xCalcKW                                                                                                                              !PVzipcalib
           xInvest=xUnits*xEqCost/10.**6                         !PVinvest
           x111dRenSub(iCurIYr,iDiv,1)=xInvest*xTxCrPct_Div(nv,iDiv,1)                                                                                           !PVzipcalib
           Trills(iCurIYr,iDiv,1)= Trills(iCurIYr,iDiv,1) + (ExogPVMistie(iCurIYr,iDiv) / xCalcKW) *xAnnualKWh*3412./10.**12                                     !PVzipcalib

           !Test for negative generation after exogenous PV calibration                                                                                          !PVzipcalib
           IF (Trills(iCurIYr,iDiv,1) .LT. 0.) THEN                                                                                                              !PVzipcalib
            Trills(iCurIYr,iDiv,1)=0.                                                                                                                            !PVzipcalib
           ENDIF                                                                                                                                                 !PVzipcalib

           !Share out generation of ExogPVMistie to own-use generation rather than putting it all into both Trills and TrillsOwnUse                              !PVzipcalib
           TrillsOwnUse(iCurIYr,iDiv,1)= TrillsOwnUse(iCurIYr,iDiv,1) + (TrillsOwnUse(iCurIYr,iDiv,1)/Trills(iCurIYr,iDiv,1)) * &                                !PVzipcalib
            (ExogPVMistie(iCurIYr,iDiv) / xCalcKW) *xAnnualKWh*3412./10.**12                                                                                     !PVzipcalib

           !Test for negative own-use generation after exogenous PV calibration                                                                                  !PVzipcalib
           IF (TrillsOwnUse(iCurIYr,iDiv,1) .LT. 0.) THEN                                                                                                        !PVzipcalib
            TrillsOwnUse(iCurIYr,iDiv,1)=0.                                                                                                                      !PVzipcalib
           ENDIF                                                                                                                                                 !PVzipcalib

           Invest(iCurIYr,iDiv,NT)=xInvest  !($mill)                                                                                                             !PVzipcalib
          ENDIF                                                                                                                                                  !PVzipcalib

          IF (iCurIYr .GT. EstYear-BaseYr+2 .AND. NT .EQ. 1) THEN    !Apply historical exogenous PV capacity mistie to remainder of projection years             !PVzipcalib
           Cap(iCurIYr,iDiv,1)= Cap(iCurIYr,iDiv,1) + ExogPVMistie(EstYear-BaseYr+2,iDiv)                                                                        !PVzipcalib
           Units(iCurIYr,iDiv,1)= Units(iCurIYr,iDiv,1) + (ExogPVMistie(EstYear-BaseYr+2,iDiv) / xCalcKW)                                                        !PVzipcalib
           xUnits=Units(iCurIYr,iDiv,1)-Units(iCurIYr-1,iDiv,1)                                                                                                  !PVinvest
           xCapacity=xUnits*xCalcKW                                                                                                                              !PVzipcalib
           xInvest=xUnits*xEqCost/10.**6                                                                                                                         !PVinvest
           x111dRenSub(iCurIYr,iDiv,1)=xInvest*xTxCrPct_Div(NV,iDiv,1)                                                                                        !PVzipcalib
           Trills(iCurIYr,iDiv,1)= Trills(iCurIYr,iDiv,1) + (ExogPVMistie(EstYear-BaseYr+2,iDiv) / xCalcKW) *xAnnualKWh*3412./10.**12                            !PVzipcalib
           !Share out generation of ExogPVMistie to self-use generation rather than putting it all into both Trills and TrillsOwnUse                             !PVzipcalib
           TrillsOwnUse(iCurIYr,iDiv,1)= TrillsOwnUse(iCurIYr,iDiv,1) + (TrillsOwnUse(iCurIYr,iDiv,1)/Trills(iCurIYr,iDiv,1)) * &                                !PVzipcalib
            (ExogPVMistie(EstYear-BaseYr+2,iDiv) / xCalcKW) *xAnnualKWh*3412./10.**12                                                                            !PVzipcalib
           Invest(iCurIYr,iDiv,NT)=xInvest  !($mill)                                                                                                             !PVzipcalib
          ENDIF                                                                                                                                                  !PVzipcalib

      GOTO 82  !DON'T REDO DIVISION LEVEL CALCULATIONS FOR PV IF USING ECONOMETRIC MODEL

!--------------------------------------------------
!  END OF NEW ZIP CODE BASED PV MODEL        !PVPen
!--------------------------------------------------

 81    CONTINUE  !END OF NEW PV CODE
       !  DIVISION-LEVEL CALCS IF USING THE NICHE MODEL INSTEAD OF THE ECONOMETRIC MODEL
       !      IF(LPRINT)WRITE(DGDAT,*) 'KWH ',xAnnualKWh
       !      IF(LPRINT)WRITE(DGDAT,*) 'xTemp ', xTemp, 'xPen ',xPen
       !      IF(LPRINT)WRITE(DGDAT,*) 'HSEADD ',HSEADD(CurCalYr,1,iDiv)
       !      IF(LPRINT)WRITE(DGDAT,*) 'EXOG PEN ' , xExogPen(iCurIYr,iDiv,NT)

          Units(iCurIYr,iDiv,NT)=Units(iCurIYr-1,iDiv,NT)+ xUnits
          Cap(iCurIYr,iDiv,NT)=Cap(iCurIYr-1,iDiv,NT)+ xCapacity
          Trills(iCurIYr,iDiv,NT)=Trills(iCurIYr-1,iDiv,NT)+ xTrills
          TrillsOwnUse(iCurIYr,iDiv,NT)= TrillsOwnUse(iCurIYr-1,iDiv,NT) +xTrillsOwnUse
          GasUsage(iCurIYr,iDiv,NT)=GasUsage(iCurIYr-1,iDiv,NT)+ xfuelusage
          HWBTU(iCurIYr,iDiv,NT)=HWBTU(iCurIYr-1,iDiv,NT)+ xhwbtu
          Invest(iCurIYr,iDiv,NT)=xInvest  !($mill)
          x111dRenSub(iCurIYr,iDiv,nt)=xInvest*xTxCrPct_Div(nv,iDiv,nt)  !111dren
 82   CONTINUE
       !-------------END OF PENETRATION CALCULATIONS

!----------------------------------------------------------------------------------------
! More RPS Calculations to Transfer the Composite Bonus Credits to the Electricity Market Module
!----------------------------------------------------------------------------------------
        IF(NT.EQ.1) THEN
         xPVGenAdded(iDiv,iCurIYr)= xTrills
         xCompCredit=0.
         xCompGen=0.
         ! Accumulate Credits
         xCredit=1.0 !Minimum credit multiplier is 1.
         DO iyr=RECSYear-(BaseYr-1),iCurIYr
          iCalYR=iyr+(BaseYr-1)
          xBonus=0.
          xCompGen=xCompGen+xPVGenAdded(iDiv,iyr)
          IF(CurCalYr .LT. iRPSStartYear .AND. iCalYR .GE. iRPSGrandFatherYear) THEN  !no bonus for equipment in service before grandfather year
                xbonus=xRPS(NT,iCurIYr)-1.0  !give the current year credit to renewable DG placed in service before the legislation
          ELSE
            IF(iCurIYr .LT. iyr+iNumYrsatRPSBaseRate(NT,iyr) .AND.  CurCalYr .LT. iRPSPhaseOutYear) THEN
             xBonus=xRPS(NT,iyr)-1.0  !give the base year credit for capacity added in iyr for iNumYrsatRPSBaseRate of years
!            WRITE (DGDAT,*) 'test iCurIYr iyr inumyrs at rpsbase', iCurIYr, iyr, inumyrsatrpsbaserate(nt,iyr)
            ELSE
             IF(iCurIYr .LT. iyr+iNumRPSCreditYrs(NT,iyr) .AND.  CurCalYr .LT. iRPSPhaseOutYear) &
               xBonus=xRPS(NT,iCurIYr)-1.0 !Give the current year credit if beyond the number of base credit years
            ENDIF
          ENDIF
          xCompCredit=xCompCredit+xPVGenAdded(iDiv,iyr)*(xCredit+xBonus)
!         Debugging Use IF (LPrint .AND. (iDiv < 2) WRITE (DGDAT,*) 'RPS Calcs PV', iyr, xPVGenAdded(iDiv,iyr), xcredit+xbonus
         ENDDO
         IF(xCompGen .GT. 0.) CGCPVRES(iDiv,iCurIYr)=xCompCredit/xCompGen
         IF (LPrint .AND. iDiv < 10) WRITE (DGDAT,'("RPS Calcs PV",3i6,2F12.5)') iCurIYr, CurCalYr, iDiv, CGCPVRes(iDiv,iCurIYr), EPRPSPR(iCurIYr)
        ENDIF !nt=1

         IF(NT.EQ.3) THEN
         xWindGenAdded(iDiv,iCurIYr)= xTrills
         xCompCredit=0.
         xCompGen=0.
         ! Accumulate Credits
         xCredit=1.0 !Minimum credit multiplier is 1.
         DO iyr=RECSYear-(BaseYr-1),iCurIYr
           iCalYR=iyr+(BaseYr-1)
           xBonus=0.
            xCompGen=xCompGen+xWindGenAdded(iDiv,iyr)
            IF(CurCalYr .LT. iRPSStartYear .AND. iCALYR .GE. iRPSGrandFatherYear) THEN  !no bonus for equipment in service before grandfather year
                xbonus=xRPS(NT,iCurIYr)-1.0  !give the current year credit to renewable DG placed in service before the legislation
            ELSE
              IF(iCurIYr .LT. iyr+iNumYrsatRPSBaseRate(NT,iyr) .AND.  CurCalYr .LT. iRPSPhaseOutYear) THEN
                   xBonus=xRPS(NT,iyr)-1.0  !give the base year credit for capacity added in iyr for iNumYrsatRPSBaseRate of years
!                  WRITE (dgdat,*) 'test iCurIYr iyr inumyrs at rpsbase', iCurIYr, iyr, inumyrsatrpsbaserate(nt,iyr)
               ELSE
                   IF(iCurIYr .LT. iyr+iNumRPSCreditYrs(NT,iyr) .AND.  CurCalYr .LT. iRPSPhaseOutYear) &
                     xBonus=xRPS(NT,iCurIYr)-1.0 !Give the current year credit if beyond the number of base credit years
               ENDIF
            ENDIF
         xCompCredit=xCompCredit+xWindGenAdded(iDiv,iyr)*(xCredit+xBonus)
!        Debugging Use IF (LPrint .AND. iDiv < 2) WRITE (dgdat,*) 'RPS Calcs Wind', iyr, xWindGenAdded(iDiv,iyr), xcredit+xbonus
         ENDDO
         IF(xCompGen .GT. 0.) CGCWNRES(iDiv,iCurIYr)=xCompCredit/xCompGen
         IF (LPrint .AND. iDiv < 10) WRITE (dgdat,'("RPS Calcs WN",3i6,2F12.5)') iCurIYr, CurCalYr, iDiv, CGCWNRes(iDiv,iCurIYr), EPRPSPR(iCurIYr)
        ENDIF !nt=3

!--------------------------------------------------
!  CHECK TECHNICAL POTENTIAL
!--------------------------------------------------
        ! Accumulators for Technical Potential and Other Summary Statistics
        ! Note xSqftperKW varies by year and is calculated above
        IF (nt==1) THEN
          SolarPVInstalledMW(iCurIYr,iDiv)=SolarPVInstalledMW(iCurIYr,iDiv)+SolarPVInstalledMW(iCurIYr-1,iDiv)
          SolarPVUsedRoofArea(iCurIYr,iDiv)=SolarPVUsedRoofArea(iCurIYr,iDiv)+SolarPVUsedRoofArea(iCurIYr-1,iDiv)
        ENDIF

        IF (nt==3) THEN
          WindInstalledMW(iCurIYr,iDiv)=WindInstalledMW(iCurIYr,iDiv)+WindInstalledMW(iCurIYr-1,iDiv)
        ENDIF

      ENDDO      ! END CENSUS DIVISION LOOP iDiv

 66     CONTINUE    ! CYCLE THROUGH VINTAGES THAT DON'T APPLY

       ENDDO      ! END TECHNOLOGY VINTAGE LOOP NV
      ENDDO      ! END TECHNOLOGY TYPE LOOP NT

        !  MORE DIAGNOSTIC PRINTING
          DO iDiv=1,NumDiv
            DO NT=1,NumTechs
              QRSDGSG(iCurIYr,iDiv)=QRSDGSG(iCurIYr,iDiv)+Trills(iCurIYr,iDiv,NT)-TrillsOwnUse(iCurIYr,iDiv,NT) !Grid electricity sales in trillion Btu
               ! IF(LPRINT.AND.LPRINT2) WRITE(DGDAT,*)QRSDGSG(iCurIYr,iDiv),Trills(iCurIYr,iDiv,NT),TrillsOwnUse(iCurIYr,iDiv,NT)
            ENDDO  ! TECHNOLOGIES
              IF(LPRINT.AND.LPRINT2) THEN
                WRITE(DGDAT,*) ' DIV       GRID SALES'
                WRITE(DGDAT,67) iDiv,QRSDGSG(iCurIYr,iDiv)
              ENDIF
          ENDDO  ! DIVISIONS

!---------------------------------------------
!  CALCULATE OUTPUTS TO NEMS AND RESD
!---------------------------------------------

       !LOAD Arrays for Passing Data to the Electric Utility Module
       !Calculate Base Year Cogeneration Capacity by Census division,
       ! Building Type and Fuel;
       !Also Populate Common Block Variables for DG Learning
       !Initialize learning variables
       rFuelCell_MW(iCurIYr)=0.
       rPV_MW(iCurIYr)=0.
       rMicroTur_MW(iCurIYr)=0.
       rWind_MW(iCurIYr)=0.

       DO r=1,NumDiv  !Census Division Loop for Populating Arrays for Utility Module

       !Initialize Output Arrays for Linking to the Utility Module
       ! The index f(1:10) is the cogeneration fuel numbering scheme for the Utility Module Link Array:
       !            f=3, natural gas corresponds to NT=2, fuel cells
       !            f=8, solar corresponds to NT=1
       !            f=11, wind corresponds to NT=3
       !         Currently unused, but present in the Utility Module Link Array of the cogen common block:
       !            f=1, coal
       !            f=2, distillate/ residual fuel oil + kerosene
       !            f=4, hydro
       !            f=5, geothermal
       !            f=6, MSW
       !            f=7, biomass
       !            f=9, other gaseous
       !            f=10, other
       !            f=12, solar thermal

       ! Cogeneration Fuel Consumption in Trills
       CGRESQ (r,iCurIYr,1:12)= 0.0
       ! Generation in GWh by Capacity Type and Grid Sales(1) versus Own Use(2)
       CGRESGEN (r,iCurIYr,1:12,1:2)= 0.0
       ! Capacity in MW
       CGRESCAP(r,iCurIYr,1:12)=0.

       ! Cogen Capacity MW
       CGRESCAP (r,iCurIYr,3)= CGRESCAP (r,iCurIYr,3) + Cap(iCurIYr,r,2)/1000. !natural gas-fired fuel cells
       CGRESCAP (r,iCurIYr,8)= CGRESCAP (r,iCurIYr,8) + Cap(iCurIYr,r,1)/1000. !solar PV
       CGRESCAP (r,iCurIYr,11)= CGRESCAP (r,iCurIYr,11) + Cap(iCurIYr,r,3)/1000. !wind

       ! Populate learning common block variables in MW
       rPV_MW(iCurIYr)=      rPV_MW(iCurIYr)       + Cap(iCurIYr,r,1)/1000.
       rFuelCell_MW(iCurIYr)=rFuelCell_MW(iCurIYr) + Cap(iCurIYr,r,2)/1000.
       rWind_MW(iCurIYr)=    rWind_MW(iCurIYr)     + Cap(iCurIYr,r,3)/1000.

       ! Cogen Electricity Generated in GWh (1=grid sales, 2=own-use) -- UNUGS.f uses both CGRESGEN(ICN,IYR,JFL,1) and CGRESGEN(ICN,IYR,JFL,2)  !DGreport

       !  Grid Sales:
       CGRESGEN (r,iCurIYr,3,1)= CGRESGEN (r,iCurIYr,3,1)     &     ! Natural Gas Fuel Cell
              + (Trills(iCurIYr,r,2) - TrillsOwnUse(iCurIYr,r,2)) & ! Grid Sales = total generation - own use
              * (1000./3.412)                                       ! GWh/trill conversion

       CGRESGEN (r,iCurIYr,8,1)= CGRESGEN (r,iCurIYr,8,1)     &     ! Solar PV
              + (Trills(iCurIYr,r,1) - TrillsOwnUse(iCurIYr,r,1)) & ! Grid Sales = total generation - own use
              * (1000./3.412)                                       ! GWh/trill conversion

       CGRESGEN (r,iCurIYr,11,1)= CGRESGEN (r,iCurIYr,11,1)     &   ! Wind
              + (Trills(iCurIYr,r,3) - TrillsOwnUse(iCurIYr,r,3)) & ! Grid Sales = total generation - own use
              * (1000./3.412)                                       ! GWh/trill conversion

       !  Own Use:
       CGRESGEN (r,iCurIYr,3,2)= CGRESGEN (r,iCurIYr,3,2)      &    ! Natural Gas Fuel Cell
              + TrillsOwnUse(iCurIYr,r,2) &
              * (1000./3.412)                                       ! GWh/trill conversion

       CGRESGEN (r,iCurIYr,8,2)= CGRESGEN (r,iCurIYr,8,2)      &    ! Solar PV
              + TrillsOwnUse(iCurIYr,r,1) &
              * (1000./3.412)                                       ! GWh/trill conversion

       CGRESGEN (r,iCurIYr,11,2)= CGRESGEN (r,iCurIYr,11,2)    &    ! Wind
              + TrillsOwnUse(iCurIYr,r,3) &
              * (1000./3.412)                                       ! GWh/trill conversion

       ! Fuel consumption for cogeneration (trillion Btu)
           CGRESQ (r,iCurIYr,3)= CGRESQ(r,iCurIYr,3)              & ! Natural Gas Fuel Cell
             + GasUsage(iCurIYr,r,2)

           CGRESQ (r,iCurIYr,8)= CGRESQ(r,iCurIYr,8)              & ! Solar PV
             + Trills(iCurIYr,r,1)*WHRFOSS(R,iCurIYr)/3412.                              ! report "fuel usage" as generation in Trills  !STEOhr

           CGRESQ (r,iCurIYr,11)= CGRESQ(r,iCurIYr,11)            & ! Wind
             + Trills(iCurIYr,r,3)*WHRFOSS(R,iCurIYr)/3412.                              ! report "fuel usage" as generation in Trills  !STEOhr

       ENDDO !Census Division Loop for Populating Arrays for Utility Module

       !  Aggregation of national cogen results for utility model
       DO F=1,12
         CGRESGEN(11,iCurIYr,F,1)=0.0   !GRID SALES (GWh)
         CGRESGEN(11,iCurIYr,F,2)=0.0   !OWN-USE (GWh)
         CGRESQ(11,iCurIYr,F)=0.0       !COGEN FUEL CONS (Trills)
         CGRESCap(11,iCurIYr,F)=0.0     !CAPACITY (MW)
       ENDDO

       DO R=1,NumDiv
         DO F=1,12
           CGRESGEN(11,iCurIYr,F,1)=CGRESGEN(11,iCurIYr,F,1)+CGRESGEN(R,iCurIYr,F,1)
           CGRESGEN(11,iCurIYr,F,2)=CGRESGEN(11,iCurIYr,F,2)+CGRESGEN(R,iCurIYr,F,2)
           CGRESQ(11,iCurIYr,F)=CGRESQ(11,iCurIYr,F)+CGRESQ(R,iCurIYr,F)
           CGRESCap(11,iCurIYr,F)=CGRESCap(11,iCurIYr,F)+CGRESCap(R,iCurIYr,F)
         ENDDO
       ENDDO

       ! Aggreggate TrillsOwnUse to National Level  !DGreport
       DO R=1,mNumCR-2  !DGreport
         DO NT=1,nTek  !DGreport
           TrillsOwnUse(iCurIYr,11,NT)=TrillsOwnUse(iCurIYr,11,NT)+TrillsOwnUse(iCurIYr,R,NT)  !DGreport
         ENDDO  !DGreport
       ENDDO  !DGreport


!----------End LOAD Arrays for the Electric Utility Module


!  SUMMARY PRINTING TO THE OUTPUT DATABASE FILE FOR THE YEAR
      IF(LPRINT) THEN
         DO NT=1,NumTechs
          WRITE(DGDAT,*) 'TECHNOLOGY CLASS:  ',aEquipName(NT,1)
          WRITE(DGDAT,*) ' DIV   UNITS       INVESTMENT'
          WRITE(DGDAT,68) (iDiv,Units(iCurIYr,iDiv,NT), &
                             Invest(iCurIYr,iDiv,NT),iDiv=1,NumDiv)

          WRITE(DGDAT,*) ' DIV       Trills       TrillsOwnUse'
          WRITE(DGDAT,68) (iDiv,Trills(iCurIYr,iDiv,NT), &
           TrillsOwnUse(iCurIYr,iDiv,NT),iDiv=1,NumDiv)

          WRITE(DGDAT,*) ' DIV       GasUsage       HWSAVINGS'
          WRITE(DGDAT,68) (iDiv,GasUsage(iCurIYr,iDiv,NT), &
           HWBTU(iCurIYr,iDiv,NT),iDiv=1,NumDiv)
         ENDDO

          WRITE(DGDAT,*)' Technical Potentials and Penetration ', CurCalYr
          WRITE(DGDAT,*)' Div  PV_Potential_MW           PV_Installed_MW'
          WRITE(DGDAT,73) (iDiv, SolarPVTechPotentialMW(iCurIYr,iDiv), SolarPVInstalledMW(iCurIYr,iDiv),iDiv=1,NumDiv)
          WRITE(DGDAT,*)' Div  PV_Available_Roof_Area    PV_Used_Roof_Area '
          WRITE(DGDAT,73) (iDiv, SolarPVAvailRoofArea(iCurIYr,iDiv), SolarPVUsedRoofArea(iCurIYr,iDiv), iDiv=1,NumDiv)
          WRITE(DGDAT,*)' Div  Wind_Potential_MW         Wind_Installed_MW'
          WRITE(DGDAT,73) (iDiv, WindTechPotentialMW(iCurIYr,iDiv),WindInstalledMW(iCurIYr,iDiv),iDiv=1,NumDiv)

      ENDIF
 67   FORMAT(1X,I4,F15.0)
 68   FORMAT(1X,I4,2F15.1)
 73   FORMAT(1X,I4,F15.1,10X,F15.1)

!---------------------------------------------------------
!  PRINT THE DISTRIBUTED GENERATION DATABASE (RDGENOUT)
!---------------------------------------------------------
      IF (iCurIYr.EQ.LastYr .AND. FCRL.EQ.1) THEN
        WRITE(DGDAT,69)
        DO NT=1,NumTechs
          DO IYR=RECSYear-(BaseYr-1),LastYr  !yr
          DO iDiv=1,NumDiv
           xTrills=Trills(IYR,iDiv,NT)-TrillsOwnUse(IYR,iDiv,NT)  !Grid Sales = Trills - TrillsOwnUse
           xUnits=Units(IYR,iDiv,NT)-Units(IYR-1,iDiv,NT) !just in case units DOn't change from one year to the next
           IF(xUnits>0. .AND. nt==1) THEN
             xCalcKW=(Cap(IYR,iDiv,NT)-Cap(IYR-1,iDiv,NT))/xUnits
            ELSE
             xCalcKW=xKW(nt,iyr-10)  !iyr started in 1990, data in 2000 (no longer relevant?)	!kj
           ENDIF
           WRITE(DGDAT,70) aEquipName(nt,1),IYR+(BaseYr-1),iDiv, & !BldgType added in FORMAT below
            Units(IYR,iDiv,NT), xCalcKW,       &
            xCalcKW*xUnits,                    &
            Trills(IYR,iDiv,NT), xTrills,      &
            TrillsOwnUse(IYR,iDiv,NT),         &  !OwnUseOut
            HWBTU(IYR,iDiv,NT),GasUsage(IYR,iDiv,NT), &
            Invest(IYR,iDiv,NT)
          ENDDO
          ENDDO
        ENDDO
 69     FORMAT(1X,'Tech,Year,Division,BldgType,#Units,AvgKWCap,TotKWAdded,GEN(tBtu),GridSales,OwnUse,HWOut,SHOut,FuelInp,Invest($mill)')  !OwnUseOut
 70     FORMAT(1X,A14,',',I5,',',I5,',','SF',3(',',F12.3),4(',',F12.5),', 0.',2(',',F12.5))  !OwnUseOut
      ENDIF !Check for final convergence

         IF (CurCalYr .EQ. ESTYEAR+2) THEN  !Test-write ExogPVMistie data to RESOUT.txt to verify                   !PVzipcalib  !DGrate
           WRITE(9,*)  'ExogPVMistie_Test_2'                                                                        !PVzipcalib
           WRITE(9,*)  'IYR  ', 'CalYr  ', 'CD  ', 'xExogPen  ', 'Cap  ', 'ExogPVMistie  ', 'Units  ', 'Trills  ', 'TrillsOwnUse  '  !PVzipcalib  !DGrate
           DO IYR= (RECSYear-BaseYr+1),(DGrateYr-BASEYR+2)                                                         !PVzipcalib  !DGrate - Prints through year after DGrateYr (hence +2); ExogPVMistie should be 0.0 in years before EstYear
            DO iDiv= 1,mNumCR-2                                                                                     !PVzipcalib
             WRITE(9,919)  IYR, IYR+BaseYr-1, iDiv, xExogPen(IYR,iDiv,1), Cap(IYR,iDiv,1), ExogPVMistie(IYR,iDiv), Units(IYR,iDiv,1), Trills(IYR,iDiv,1), TrillsOwnUse(IYR,iDiv,1)  !PVzipcalib  !DGrate
             919   FORMAT(I4,1X,I4,1X,I2,1X,F12.0,1X,F12.4,1X,F12.4,1X,F12.4,1X,F12.4,1X,F12.4)  !DGrate
            ENDDO                                                                                                   !PVzipcalib
           ENDDO                                                                                                    !PVzipcalib
         ENDIF                                                                                                      !PVzipcalib

        RETURN     ! SEND CONTROL BACK TO RESD

       END SUBROUTINE RDISTGEN


!============================================================================
!       PITCINIT INITIALIZES THE PRICE-DRIVEN TECHNOLOGY ADVANCEMENT
!           VARIABLES AND ARCHIVES THE RTINITYR FROM RSMEQP
!============================================================================
      SUBROUTINE PITCINIT
      IMPLICIT NONE
      INTEGER  I , F
      COMMON /PITCVARS/XTINITYR(MNUMRTTY), IFORWARD(4), IFWDPREVYR(4)
      INTEGER   XTINITYR      ! STORAGE FOR INITIAL YEARS FROM RSMEQP
      INTEGER   IFORWARD      !   CALCULATION OF PRICE EFFECTS ON TECH MENU
      INTEGER   IFWDPREVYR    !   STORAGE OF PREVIOUS YEAR'S VALUES
      INTEGER   IFMAX         !   MAXIMUM FORWARD EFFECT
      INTEGER   ILastSTEOYr   !   CALENDAR YEAR FOR LAST STEO BENCH YEAR
      INTEGER   IFWD          !   TEMP VARIABLE

      DO F=1,4
       IFORWARD(F)=0.0
       IFWDPREVYR(F)=0.0
      ENDDO

      DO I=1,RTTYCNT
        XTINITYR(I)=RTINITYR(I)  ! ARCHIVE INITIAL START YEAR FOR REPORTING
      !        WRITE(DGDAT,*) "READ DATA " , CurCalYr, RTTYNAME(I),XTINITYR(I), RTINITYR(I)
      ENDDO

      RETURN
      END SUBROUTINE PITCINIT


!=====================================================================
!  RSPITC -- COMPUTES AND STORES TECH MENU ADVANCEMENTS ANNUALLY
!            VARIABLES AND ARCHIVES THE RTINITYR FROM RSMEQP
!=====================================================================
      SUBROUTINE RSPITC (IFMAX, ILastSTEOYr)
      IMPLICIT NONE
      INTEGER  I, Y, F, RECNO, EU, EQCLNO, EQC
      COMMON /PITCVARS/XTINITYR(MNUMRTTY), IFORWARD(4), IFWDPREVYR(4)
      INTEGER   XTINITYR      ! STORAGE FOR INITIAL YEARS FROM RSMEQP
      INTEGER   IFORWARD      !   CALCULATION OF PRICE EFFECTS ON TECH MENU
      INTEGER   IFWDPREVYR    !   STORAGE OF PREVIOUS YEAR'S VALUES
      INTEGER   IFMAX         !   MAXIMUM FORWARD EFFECT
      INTEGER   iLastSTEOYr   !   CALENDAR YEAR FOR LAST STEO BENCH YEAR
      INTEGER   IFWD          !   TEMP VARIABLE
      REAL*4    PRICEDELTA(4) !   PRICE CHANGE - 3YR AVERAGE

        iLastSTEOYr=LastSTEOYr
        IF(CurCalYr.LE.iLastSTEOYr)RETURN
        ! IFMAX=-10 ! READ FROM RMISC EVENTUALLY SET TO ZERO TO TURN OFF RSPITC

!  MAP IFORWARD TO RTEK FUEL NUMBERING SYSTEM

!   ON FIRST ITERATION, STORE PREVIOUS YEAR'S ADVANCEMENT INTO IFWDPREVYR
         IF(CURITR.EQ.1) THEN
          DO F=1,4
          IFWDPREVYR(F)=IFORWARD(F)!SET TO LAST YEAR'S  ! RSPITC
          ENDDO
         ENDIF

!  NEXT COMPUTE THREE-YEAR AVERAGE PRICE INDEX RELATIVE TO BASE YEAR
         Y=CurIYr
         PRICEDELTA(1)=.33333*(PDSRS(11,Y)+ PDSRS(11,Y-1)+ PDSRS(11,Y-2)) &
          /PDSRS(11,RECSYear-BaseYr+1)
         PRICEDELTA(2)=.33333*(PLGRS(11,Y)+ PLGRS(11,Y-1)+ PLGRS(11,Y-2)) &
          /PLGRS(11,RECSYear-BaseYr+1)
         PRICEDELTA(3)=.33333*(PNGRS(11,Y)+ PNGRS(11,Y-1)+ PNGRS(11,Y-2)) &
          /PNGRS(11,RECSYear-BaseYr+1)
         PRICEDELTA(4)=.33333*(PELRS(11,Y)+ PELRS(11,Y-1)+ PELRS(11,Y-2)) &
          /PELRS(11,RECSYear-BaseYr+1)

!     SET ADVANCMENT YEARS BY FUEL FOR PRICE-INDUCED TECHNICAL CHANGE

        DO F= 1, 4

!       SET TO LAST YEAR'S ADVANCEMENT (IE ONCE SHIFTED FORWARD, THEY REMAIN ADVANCED
          IFWD=IFWDPREVYR(F)

!       SET MINIMUM SHIFT TO WHAT IT WAS LAST YEAR OR TO A GREATER SHIFT
          IFORWARD(F) = MIN(IFWD,-IFIX(((PRICEDELTA(F)-1.0)/.10)))

!       SET MAXIMUM SHIFT TO IFMAX FROM RSMISC? FILE
          IFORWARD(F) = MAX(IFMAX,IFORWARD(F))

        ENDDO

!  APPLY SHIFTS TO INDIVIDUAL TECHNOLOGIES BASED ON NEARNESS TO LAST BENCHMARKING YEAR

! THIS DO LOOP INDEX AND LIMITS ARE LIKELY WRONG!!!!!!	!kj - noting this old comment, should probably be updated
       DO 10 RECNO=1,RTTYCNT  ! DO FOR ALL RTEK RECORDS
         EQC= RTTYEQCL(RECNO) ! EQUIPMENT CLASS NUMBER FROM RSMEQP
         EU = RTTYENDU(RECNO) ! END USE NUMBER FROM RSMEQP

         DO EQCLNO=1,RTCLCNT  !NEXT MATCH RSMEQP RECORD TO RSCLASS RECORD TO FIND FUEL TYPE
           IF (RTCLENDU(EQCLNO).EQ.EU) THEN ! END USE MATCHED, NOW CHECK FOR EQUIPMENT CLASS
               IF (RTCLEQCL(EQCLNO).EQ.EQC) THEN
                  F=RTFUEL(EQCLNO)  ! MATCH FOUND, SET FUEL POINTER AND PROCEED
                  IF(F.GT.4)F=0     ! SET TO ZERO IF NOT ONE OF THE 4 MAJOR FUELS
                  GOTO 5
               ELSE
                  CONTINUE
               ENDIF
           ELSE
              CONTINUE
           ENDIF
         ENDDO

!       SET ADVANCEMENTS:

5         IF(F.EQ.0)GOTO 10  ! SKIP IF NOT ONE OF THE 4 MAJOR FUELS
          IFWD=IFORWARD(F)   ! FIRST SET MAXIMUM ADVANCEMENT BASED ON FUEL PRICES

!       NOW CHECK FOR "CLOSE-IN" TECHNOLOGIES AND TRIM ADVANCMENT YEARS
!        PERFORM CHECKING ON UN-ADVANCED INITIAL YEARS

          IF(XTINITYR(RECNO).LE.iLastSTEOYr+50) IFWD=IFORWARD(F)
          IF(XTINITYR(RECNO).LE.iLastSTEOYr+10) IFWD=MAX(-5,IFORWARD(F))
          IF(XTINITYR(RECNO).LE.iLastSTEOYr+ 5) IFWD=MAX(-3,IFORWARD(F))
          IF(XTINITYR(RECNO).LE.iLastSTEOYr   ) IFWD=0

          RTINITYR(RECNO)=XTINITYR(RECNO)+IFWD  ! SHIFT INITIAL YEAR AND STORE IN WORKING ARRAY

!        WRITE(DGDAT,*) "RSPITC " , CurCalYr, RTCLNAME(EQCLNO), RTTYNAME(RECNO), &
!          XTINITYR(RECNO), "SHIFTED TO (i), RTINITYR(RECNO), "FUEL " , &
!          F, "ENDUSE (i), EU, "CLASS (i), EQC

 10    CONTINUE                                 ! NEXT RSMEQP RECORD

       RETURN                                   ! ALL RECORDS PROCESSED
       END SUBROUTINE RSPITC


!===================================================================
!     DISTRIBUTED SHORT-RUN ELASTICITY CALCULATION FUNCTION
!===================================================================
      REAL FUNCTION RSELAST (F,R,ALPHA,EF1,EF2,EF3,RECSYear,EUPR)
      IMPLICIT NONE
      REAL*4 EF1,EF2,EF3
      REAL*4 ALPHA
      INTEGER F,R,RECSYear,EUPR
      REAL*4 FAC1,FAC2,FAC3

      ! Set no elasticity adjustment if no fuel is specified, then return
      !  Allows more general use of this function and streamlines code
      IF (F .EQ. 0.) THEN
        RSELAST=1.
        RETURN
      ENDIF

      ! NOTE EF1+EF2+EF3 SHOULD SUM TO 1.0 -- THEY ARE DISTRIBUTIONAL SHARES FOR THE SHORT-RUN ELASTICITY EFFECTS
      FAC1=1.  ;  FAC2=1.  ;  FAC3=1.   !INITIALIZE

      IF (F.EQ.4) THEN
      !END USE PRICING FOR ELECTRICITY (no need to deflate to a particular year because it would appear in numerator and denominator
        IF (CurCalYr>=RECSYear+1)FAC1=(PELRSOUT(R,CurIYr,  EUPR)/PELRSOUT(R,RECSYear-(BaseYr-1),EUPR))**(ALPHA*EF1)
        IF (CurCalYr>=RECSYear+2)FAC2=(PELRSOUT(R,CurIYr-1,EUPR)/PELRSOUT(R,RECSYear-(BaseYr-1),EUPR))**(ALPHA*EF2)
        IF (CurCalYr>=RECSYear+3)FAC3=(PELRSOUT(R,CurIYr-2,EUPR)/PELRSOUT(R,RECSYear-(BaseYr-1),EUPR))**(ALPHA*EF3)
       ELSE
        IF (CurCalYr>=RECSYear+1)FAC1=(PRICES(F,R,CurCalYr  )/PRICES(F,R,RECSYear))**(ALPHA*EF1)
        IF (CurCalYr>=RECSYear+2)FAC2=(PRICES(F,R,CurCalYr-1)/PRICES(F,R,RECSYear))**(ALPHA*EF2)
        IF (CurCalYr>=RECSYear+3)FAC3=(PRICES(F,R,CurCalYr-2)/PRICES(F,R,RECSYear))**(ALPHA*EF3)
      ENDIF

      RSELAST=FAC1*FAC2*FAC3

      ! WRITE(DGDAT,*) "rselast=(i),rselast,CurCalYr,PRICES(F,R,CurCalYr),RECSYear,prices(f,r,RECSYear)!produces copious output in rdgenout

      RETURN
      END FUNCTION RSELAST

         END SUBROUTINE RESD   ! closes the contains structure

!*******************************************************************
!     SURVIVING FRACTION OF EQUIPMENT STOCK IN YEAR Y
!*******************************************************************
      REAL FUNCTION SVRTE(ALPHA,Y,K,LAMBDA)
      REAL*4 ALPHA
      REAL*4 K
      REAL*4 LAMBDA
      REAL*4 KLAMBDA1
      REAL*4 KLAMBDA2
!      INTEGER MIN, MAX, Y
      INTEGER Y
      IF (Y.LT.0) THEN  ! calling y less than zero is to the left of Weibull curve
         y=0            ! y=0 means current year, too new to decay;  survival rate will be 1.0
      ENDIF
!      IF (Y.LE.MIN) THEN
         KLAMBDA1=FLOAT(Y)/LAMBDA
         KLAMBDA2=KLAMBDA1**K
         SVRTE=EXP(-KLAMBDA2)
!      ELSEIF(Y.LT.MAX) THEN
!         SVRTE=1.0-FLOAT(Y-MIN)/FLOAT(MAX-MIN)
!      ELSE
!         SVRTE=0.0
!      ENDIF
      RETURN
      END FUNCTION SVRTE

!==============================================================================
!     Cost Trend Function
!==============================================================================
      REAL FUNCTION EQCOST*4 (RECTY,CurCalYr,CTYPE)
     !  This function returns the projected cost of equipment identified
     !  in the RSMEQP file by technology RECTY, for the calendar year CurCalYr,
     !  where CTYPE indicates whether the requested cost type
     !  is the total installed cost (equip. + installation) or the only the
     !  equipment cost.  Several required parameters, such as the
     !  trend type (MATURE, ADOLESCENT, INFANT), logistic shape
     !  parameters, years of availability, etc., are obtained from the
     !  RTEK common block rather than passed as arguments.

      IMPLICIT NONE
      INCLUDE'parametr'
      INCLUDE'rtek'

      INTEGER*4 RECTY     ! Technology index
      INTEGER*4 CurCalYr      ! Price forecast calendar year
      CHARACTER*3 CTYPE   ! Cost type requested ('CAP' or 'RET')

      REAL*4 y0        ! Year of inflection on logistic cost curve
      REAL*4 y1        ! Starting year of logistic cost curve
      REAL*4 d            ! Proportional decline in equipment cost
      REAL*4 gamma        ! Logistic cost curve shape parameter
      REAL*4 RSYR2
!  In case of any error that might occur below, the cost returned
!  will be huge:
       EQCOST= 10.0**9
       RSYR2=FLOAT(CurCalYr)
!  Project the equipment cost based on the type of cost trend
!  appropriate for the maturity level of this technology:

       ! Mature technology:
       IF ( RTMATURE(RECTY).EQ. "MATURE" ) THEN
          ! Current implementation calls for costs to CONTINUE
          ! unchanged from the initial costs specified in RSMEQP.
        IF ( CTYPE .EQ. "CAP" ) THEN
          ! Total installed cost of equipment
          EQCOST= RTEQCOST(RECTY)
          RETURN
        ELSE
         IF (CTYPE .EQ. "RET") THEN
          ! Equipment only cost (Total installed cost less installation costs)
          EQCOST= RTRECOST(RECTY)
          RETURN
         ELSE
          RETURN
         ENDIF ! Retail cost test
        ENDIF  ! Capital cost test
       ENDIF   ! MATURE technology test

       ! Adolescent technology
       IF ( RTMATURE(RECTY).EQ. "ADOLESCENT" ) THEN
          ! Current implementation calls for a logistic functional
          ! form, with the base year coinciding with the
          ! inflection point (the code actually uses the first year
          ! of availability as specified in RSMEQP). The remaining
          ! proportional cost decline is specified (RTCOSTP3), as
          ! is a 'representative' year of introduction (RTCOSTP1),
          ! and shape parameter (RTCOSTP2), in the RSMEQP input file:

        y1= RTCOSTP1(RECTY) ! representative year cost decline began
        y0= FLOAT(RTINITYR(RECTY)) ! year of inflection of cost trend
        d= RTCOSTP3(RECTY) ! total possible proportional decline in equipment cost from y0 onward
        gamma= RTCOSTP2(RECTY) ! logistic curve shape parameter

        IF ( CTYPE .EQ. "CAP" ) THEN
          EQCOST= RTEQCOST(RECTY) * 2.0 * d &
                / ( 1.0 + ((RSYR2 - y1)/(y0 - y1))**gamma ) &
                + ( 1.0 - d ) * RTEQCOST(RECTY)
          RETURN

        ELSE
         IF (CTYPE .EQ. "RET") THEN
          EQCOST= RTRECOST(RECTY) * 2.0 * d &
                / ( 1.0 + ((RSYR2 - y1)/(y0 - y1))**gamma ) &
                + ( 1.0 - d ) * RTRECOST(RECTY)
          RETURN
         ELSE
          RETURN
         ENDIF ! Retail cost test
        ENDIF  ! CAPITAL cost test
       ENDIF   ! ADOLSECENT technology test

       ! Infant technology
       IF ( RTMATURE(RECTY).EQ. "INFANT" ) THEN
          ! Current implementation calls for a logistic functional
          ! form for the cost trend:

        y1= FLOAT(RTINITYR(RECTY)) ! year cost decline begins
        y0= RTCOSTP1(RECTY) ! year of inflection of cost trend
         d= RTCOSTP3(RECTY) ! total possible proportional decline in
                            ! equipment cost from y1 onward
        gamma= RTCOSTP2(RECTY) ! logistic curve shape parameter

        IF ( CTYPE .EQ. "CAP" ) THEN
          EQCOST= RTEQCOST(RECTY) * d &
                / ( 1.0 + ((RSYR2 - y1)/(y0 - y1))**gamma ) &
                + ( 1.0 - d ) * RTEQCOST(RECTY)
          RETURN
        ELSE
         IF (CTYPE .EQ. "RET") THEN
          EQCOST= RTRECOST(RECTY) * d &
                / ( 1.0 + ((RSYR2 - y1)/(y0 - y1))**gamma ) &
                + ( 1.0 - d ) * RTRECOST(RECTY)
          RETURN
         ELSE
          RETURN
         ENDIF ! Retail cost test
        ENDIF  ! Capital cost test
       ENDIF   ! INFANT technology test

       RETURN
       END FUNCTION EQCOST

!====================================================================
!     Learning Cost Function
!====================================================================
      REAL FUNCTION rLearnCost*4 (MaxCost,Beta,c0,CumShip,report)
      ! This function returns the projected cost of equipment based on cumulative
      !  shipment estimates (from the prior year)
      IMPLICIT NONE

      REAL*4 MaxCost     ! maximum cost set equal to default projections
      REAL*4 Beta        ! the learning cost function shape parameter
      REAL*4 c0          ! first unit cost
      REAL*4 CumShip     ! cumulative shipments through the previous year

      INTEGER*4 report   ! link to krpt output file handle

      IF (cumship .LE. 1.) rLearnCost=MaxCost
      IF (cumship .LE. 1.) RETURN
      IF (c0 .EQ. 0.) rLearnCost=MaxCost
      IF (c0 .EQ. 0.) RETURN

      rLearnCost=min( MaxCost, exp( log(c0) - Beta*log(CumShip) ) )

      RETURN
      END FUNCTION rLearnCost

!======You've reached the end of the code!=======