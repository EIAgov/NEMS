! $Header: m:/default/source/RCS/efd_row_col.f90,v 1.26 2021/05/17 15:06:51 LC2 Exp $
! the module "efd_row_col" contains declarations and data shared throughout the subroutines in uefd.f and uaimms.f.  The
! purpose is store LP coefficients in dynamically-declared arrays for the AIMMS EFD interface.
!
module efd_row_col

! This module declares shared storage variables to hold row and col name index parsing and 
! coefficient storage

  character*15 efdsub ! subroutine invoking LP routines. Used to link coefficient parameters to subroutines for aimms conversion.

  integer,parameter :: max_set=5  ! maximum number of sets represented on an oml row/col name. 5 in uefd; 6 in uecp
! structure type to hold column name types, indexing attributes, and solution values
  type column_name
    character*30 colnam_mask     ! oml column name mask
    character*30 colnam_alt_mask     ! oml column name mask
    character*24 colnam_aimms    ! AIMMS variable (column) name 
    character*30 setnam(max_set) ! indexing set names
    integer*2    indstt(max_set) !  position in oml colnam where each indexing field starts
    integer*2    indend(max_set) ! position in oml colnam where each indexing field ends
    integer*2    needsol         ! if >0, need solution data.
  end type
 character(len=30), allocatable :: colmask_hash(:)  ! oml column mask hash based on mask, allocate with max_col_aimms
 character(len=31), allocatable :: rowmask_hash(:)  ! oml row    mask hash based on mask and row_type, allocate with max_row_aimms
 character(len=24), allocatable :: colname_hash(:)  ! oml column name hash based on aimms identifier, allocate with max_col_aimms
 character(len=24), allocatable :: rowname_hash(:)  ! oml row    name hash based on aimms identifier, allocate with max_row_aimms
 integer(2),        allocatable :: col_aimms_ptr(:) ! holds aimms_col_ID_num given a hash value for the AIMMS column name
 integer(2),        allocatable :: row_aimms_ptr(:) ! holds aimms_row_ID_num given a hash value for the AIMMS row    name
    
! structure type to hold row name types and indexing attributes
  type row_name
    character*30 rownam_mask       ! oml row name mask
    character*24 rownam_aimms      ! AIMMS constraint (row) name 
    character*30 setnam(max_set)   ! indexing set names
    integer*2    indstt(max_set)   ! position in omlcol where each indexing field starts
    integer*2    indend(max_set)   ! position in omlcol where each indexing field ends
    character*1  row_type          ! E/L/G/N  E: = (default), N: nonconstraining/free, L: <=, G: >=
    integer*2    needsol           ! if >0, need solution data.
  end type

  parameter (max_aimms_param=1000) ! count of all the AIMMS indentifiers used to organize the LP coefficients
  character*38 aimms_param(max_aimms_param)
  
  parameter (max_domain_sets=8) ! most sets in the domain of any AIMMS parameter
! structure type to hold aimms parameter names, set domains, pointers to their coefficients in a sorted coefficient array
  type aimms_parameter
    character*38 name                       ! name of AIMMS parameter
    character*30 setnames(max_domain_sets)  ! names of the sets
    integer*2    numsets                    ! number of sets
    integer*4    idxFirst                   ! position in efd_sortidx with first coefficient
    integer*4    idxLast                    ! position in efd_sortidx with last coefficient
  end type
  type (aimms_parameter) :: aimparm(max_aimms_param)
  
  integer max_col_aimms, max_row_aimms  ! dimensioning parameters
  parameter (max_col_aimms=887)
  parameter (max_row_aimms=887)
  
  type (column_name), allocatable :: col_aimms(:)  ! allocate with max_col_aimms  ! holds array of aimms column identifiers
  type (row_name), allocatable    :: row_aimms(:)  ! allocate with max_row_aimms  ! holds array of aimms row identifiers
  
  integer*2    aimms_col_ID_num      ! unique identifier for this column aimms
  character*30 colnam_mask           ! oml column name mask
  character*30 colnam_alt_mask           ! oml column name mask
  character*24 colnam_aimms          ! AIMMS variable name 
  character*30 csetnam(max_set)      ! indexing set names
  integer*2    cindstt(max_set)      ! position in oml colnam where each indexing field starts
  integer*2    cindend(max_set)      ! position in oml colnam where each indexing field ends
  
  integer*2    aimms_row_ID_num      ! unique identifier for this row aimms
  character*30 rownam_mask           ! oml row name mask
  character*24 rownam_aimms          ! AIMMS constraint name 
  character*30 rsetnam(max_set)      ! indexing set names
  integer*2    rindstt(max_set)      ! position in omlrow where each indexing field starts
  integer*2    rindend(max_set)      ! position in omlrow where each indexing field ends
  character*1  row_type              ! E/L/G/N  E: = (default), N: nonconstraining/free, L: <=, G: >=

  integer max_efd_coeff, max_efd_row, max_efd_row_hash, max_efd_col,max_efd_col_hash
  integer cnt_efd_coeff, cnt_efd_row, cnt_efd_col
  integer*4 c_count,r_count,p_count                   ! number of row items for columns, rows, and parameters
  
  parameter (max_efd_coeff=4000000)
  parameter (max_efd_row=25000, max_efd_row_hash=99991)   ! for hashing, max_efd_row_hash should be several times bigger than number of rows to reduce "collisions"
  parameter (max_efd_col=150000, max_efd_col_hash=224990)  ! for hashing, max_efd_col_hash should be several times bigger than number of rows to reduce "collisions"

  integer num_aimms_param
  integer aimms_param_numcoeff(max_aimms_param)

  type coeff_int          ! holds coefficient at intersection of a given col, row, with AIMMS row/col matches
    character*8 efdcolnam
    character*8 efdrownam     
    real(8)     coeff8                          ! coefficient.  8-byte double precision.
    integer*2   aimms_col_ID_num                ! aimms column/variable ID number (each indexed by various sets)
    integer*2   aimms_row_ID_num                ! aimms row/constraint ID number (each indexed by various sets)
    integer*2   iset_elements(max_domain_sets)  ! pointers to setElement positions holding the index elements for each coefficient; for converting aliases
    character*5 cset_elements(max_domain_sets)  ! set elements; used directly when writing coefficients out.
    character*15 efdsub                         ! name of subroutine setting the coefficient
  end type
  
  
  integer,allocatable       :: efd_sortidx(:)       ! holds the order of the coefficients sorted by column, row for mps output.
  character(64),allocatable :: efd_sortkey(:)       ! holds the sort keys: column&row for mps output.

  
  character*8 efdcolnam  ! oml column name
  character*8 efdrownam  ! oml row name
  real(4) efdcoeff
  real(8) efdcoeff8
  logical set_lookup ! if true, selects option to look up individual set element in efd_get_set_element. otherwise write out set to efdsetdata.dat
  character*1 firstchar ! for writing sets, this is the first character of lines in efdsetdata, normally a blank. for  sets with "_ALT" in name, it is "!" (comment)to AIMMS ignores it.

  character*30 efd_setname  
  
  type (coeff_int),   allocatable :: efd_coeff(:)  ! allocate with max_efd_coeff 
 
 !  
  integer spot ! location in hashing table of a given entry
  logical initial_store  ! argument for hashing to indicate when new entry is stored
  character(len=8), allocatable :: efd_row_name(:)                ! oml rownam(8) used as hashing key. allocate with max_efd_row_hash for hash insertion
  character(len=8), allocatable :: efd_col_name(:)                ! allocate with max_efd_col_hash for hash insertion
  character(len=16),allocatable :: efd_rowkey(:)                  ! holds the sorted row keys
  integer,allocatable           :: efd_rowidx(:)                  ! holds the storage location the sorted rows for mps output.
  character(len=16),allocatable :: efd_colkey(:)                  ! holds the sorted column keys
  integer,allocatable           :: efd_colidx(:)                  ! holds the storage location the sorted columns for get_masked_col.
  integer ncsort,nrsort ! count on nonblank cols, rows in sort

  type col_sol  ! holds solution values and column indexing information from AIMMMS
    integer*2   aimms_col_ID_num
    real(8)     solval(5)   ! up to 5 solution values:  activity,objcost,lower, upper,reducedcost
    integer*2   nsets
    integer*2   iset_elements(max_set)
    character*5 cset_elements(max_set)
    character*8 efdcolnam
    character*2 status ! oml compatibility: BS, UL, LL, or EQ
    
  end type
  type row_sol  ! holds solution values and row indexing information from AIMMMS
    integer*2   aimms_row_ID_num
    real(8)     solval(5)   ! up to 5 solution values: activity,slack activity,lower, upper,shadowprice
    integer*2   nsets
    integer*2   iset_elements(max_set)
    character*5 cset_elements(max_set)
    character*8 efdrownam
    character*1 row_type
    character*2 status ! oml compatibility: BS, UL, LL, EQ, or FR for free row
  end type

  character(len=8),allocatable :: row_sol_names(:) ! holds sequence of oml row names as read in and translated from AIMMS solution transfer file. allocated with max_efd_row
  character(len=1),allocatable :: row_sol_types(:) ! holds sequence of oml row types as read in and translated from AIMMS solution transfer file. allocated with max_efd_row
  character(len=8),allocatable :: col_sol_names(:) ! holds sequence of oml col names as read in and translated from AIMMS solution transfer file. allocated with max_efd_col
  integer num_efd_col_sol,num_efd_row_sol
  
  type (col_sol), allocatable ::  efd_col_sol(:) ! allocate with Max_efd_col_sol: allocate efd_col_sol(max_efd_col_sol)
  type (row_sol), allocatable ::  efd_row_sol(:) ! allocate with Max_efd_row_sol: allocate efd_row_sol(max_efd_row_sol)

! Set names and elements for use in writing composite data arrays.
   type aimset
     character*32 setname
     integer*4 istart
     integer*4 iend
   end type
   integer, parameter :: max_aimsets=125  ! 96 7/13/2017
   type (aimset) :: aimsets(max_aimsets)
   character*5 setElements(45000) ! need 18000 for PlantGroup, 18000 for PlantGroupOrd, plus another few thousand. 
   integer numAimSets
   integer nSetElements
  
  integer io ! unit number for writing efdcoeff_yyyy_itr.txt
  integer write_efd_mps/0/          ! if 1, call routine to write out each efd mps file as captured for aimms
  logical make_efd_aimms /.false./  ! option to generate the AIMMS LP structural data for arraycode_efd.f, such as the list of coefficient arrays, row/column lists, row types.
  integer AIMEFDBG                  ! set via runtime option from scedes file. if 0, and make_efd_aimms and false, omit some output in efdcoeff debug file to save time; omit aimms validation files in aimms
  logical USE_AIMEFD_SLNADJ /.true./             ! set during runtime based on collective values of CODEUSAGE of EFD LHS transfer variables found in aimefd.xlsx. it is set to .TRUE. if all are either 'RHS' or 'LHS_done',
                                    ! it is set to .FALSE. if any codeusage value is found to be either 'LHS' and 'LHS_coded'.   if .TRUE., skip FORTRAN EFD post-solution adjustments on EFD output variables.
                                    ! if .FALSE., execute FORTRAN EFD post-solution adjustments on EFD output variables. 
  integer AIMMKEFD                  ! set via runtime option from scedes file. if 1, set make_efd_aimms to .true.
  logical SKIP_EFDOML /.false./     ! flag to bypass passing fortran caculated EFD coefficients to AIMMS EFD and OML based on all 0 status found in efdarrays_all.txt
  
  external rtovalue ! function to get run-time option value AIMMSEFD
  integer rtovalue
  integer AIMMSEFD  ! run-time option to invoke AIMMS EFD LP 
  character*25 filen_efdcoeff
  integer iOutTxt
  integer colunit ! I-O unit for column solution retrieval debug file
  integer rowunit ! I-O unit for row    solution retrieval debug file

  character(LEN=30), allocatable :: xefdSetLook(:,:)     ! xlxs in Worksheet EFDSetMatch: set lookup table in Range EFDLOOK
  integer*2 EFDSetLookCount
  
  integer TransferCount  ! size of arrays to allocate
  character(LEN=50) EFDtransferarray(2000)
  integer           EFDtransfer_usage(2000)  ! indicator for how to treat a EFD transfer variable, 0: RHS usage only so only transfered to AIMMS, 1: LHS usage, to be passed back to NEMS, 2: LHS usage, omit calculated version declaration because aimms code already has declaration
  integer CODEUSAGE_AIMEFD_read/0/   !0/1 switch to execute READAIMEFDOPTIONS only once

  integer AIM_Phase ! 1: if in LP set up phase, 2: if in LP solution retrieval phase.  Call getbout only in phase 2, AIMMS validation phase, after oml sol retrieval
! variables to hold mask strings
      character*30 BTUROW_mask
      character*30 CBROW_mask
      character*30 CFGEN_mask
      character*30 CFROW_mask
      character*30 CLROW_mask
      character*30 COL_mask
      character*30 COL_ALT_mask
      character*30 SAF_mask
      character*30 COL_MAX_mask
      character*30 COL_MAX_SR_mask
      character*30 COL_MAX_SR_ALT_mask
      character*30 COL_MIN_mask
      character*30 COL_MIN_SR_mask
      character*30 COL_MIN_SR_ALT_mask
      character*30 COL_MP_mask
      character*30 COL_OTHER_mask
      character*30 COL_OTHR_mask
      character*30 COL_SR_mask
      character*30 COL_TRANS_mask
      character*30 COL_TRANS_S_mask
      character*30 COLLIM_mask
      character*30 COLNAM_MAX_mask
      character*30 COLNAM_MAX_ALT_mask
      character*30 COLNAM_MIN_mask
      character*30 COLNAM_MIN_ALT_mask
      character*30 COLUMN_mask
      character*30 LD_ROW_mask
      character*30 LOAD_mask
      character*30 MASKOP_mask
      character*30 MASKOP_ALT_mask
      character*30 MASKPL_mask
      character*30 COLNAMP_mask
      character*30 NGROW_mask
      character*30 OLROW_mask
      character*30 P2ROW_mask
      character*30 RELCOL_mask
      character*30 RELROW_mask
      character*30 ROW_AC_mask,ROW_AO_mask,ROW_AR_mask,ROWAB32_mask
      character*30 ROW_C_mask
      character*30 ROW_CAR_mask
      character*30 ROW_DL_mask
      character*30 ROW_DS_mask
      character*30 ROW_EOR_CO2_mask
      character*30 ROW_ERC_mask
      character*30 ROW_FLRG_mask
      character*30 ROW_FLRS_mask
      character*30 ROW_FR_mask
      character*30 ROW_G_mask
      character*30 ROW_GPS_mask
      character*30 ROW_GPSN_mask
      character*30 ROW_H_mask
      character*30 ROW_HG_mask
      character*30 ROW_HG_PT_mask
      character*30 ROW_HGO_mask
      character*30 ROW_K_mask
      character*30 ROW_LD_mask
      character*30 ROW_mask
      character*30 ROW_ALT_mask
      character*30 ROW_MP_mask
      character*30 ROW_MR_mask
      character*30 ROW_MV_mask
      character*30 ROW_NOX_mask
      character*30 ROW_O_mask
      character*30 ROW_OGSM_mask
      character*30 ROW_OTHR_FR_mask
      character*30 ROW_OTHR_mask
      character*30 ROW_S_ALL_mask
      character*30 ROW_S_mask
      character*30 ROW_SO2_mask
      character*30 ROW_SO2_PT_mask
      character*30 ROW_SR_mask
      character*30 ROW_T_mask
      character*30 ROW_TL_mask
      character*30 ROW_TO_mask
      character*30 ROW_TR_mask
      character*30 ROWBAL_mask
      character*30 ROWBMS_mask
      character*30 ROWCAR_mask
      character*30 ROWCARC_mask
      character*30 ROWCARR_mask
      character*30 ROW_LOAD_mask
      character*30 ROWEX_mask
      character*30 ROWEX_SR_mask
      character*30 ROWIM_mask
      character*30 ROWIM_SR_mask
      character*30 ROWM_mask
      character*30 ROWNRG_mask
      character*30 ROWRPS_mask
      character*30 ROWSEQ_mask
      character*30 ROWSTOR_mask
      character*30 ROWUS_mask
      character*30 ROWX_mask
      character*30 WDCOL_mask
      character*30 WDROW_mask

! set cardinalities
integer,parameter :: BiomassProductionStep = 50 ! NNWDSUPP in /wrenew/
integer,parameter :: BiomassSector = 6          ! FSTPMX number of sectors using Biomass
integer,parameter :: BiomassType = 4
integer,parameter :: BoilerType = 3 ! EFD_D_BTP
integer,parameter :: CanadianSupplyRegion = 8
integer,parameter :: CanadianSupplyStep = 5
integer,parameter :: CarbonEmissionType = 10  ! 10 (bnk, esc, fue, ind, lim, off, oth, ref, rsv, utl)
integer,parameter :: CarbonRegion = 3
integer,parameter :: CensusRegion = 11 ! nine census divisons, 10:california breakout, 11:US
integer,parameter :: CoalDemandRegion_ALT1 = 16
integer,parameter :: CoalDemandRegion_ALT2 = 16
integer,parameter :: CoalDemandRegion_ALTto = 16
integer,parameter :: CoalDemandRegion_ALTfrom = 16
integer,parameter :: CoalDemandRegion = 16
integer,parameter :: CoalDiversityType = 2 ! {L Lignite and S subbituminous}
integer,parameter :: CoalProductionStep = 12
integer,parameter :: CoalSupplyCurve = 53
integer,parameter :: CoalSupplyCurve_Dom = 41
integer,parameter :: CoalSupplyCurve_Int = 12
integer,parameter :: CoalType = 6
integer,parameter :: DistillateProductionStep = 1
integer,parameter :: SupplyRegion_ALT1 =28
integer,parameter :: SupplyRegion_ALT2 = 28
integer,parameter :: SupplyRegion_ALTfrom = 28
integer,parameter :: SupplyRegion_ALTto = 28
integer,parameter :: SupplyRegion_SUB=25 ! UNRGNS 22 lower 48 states.
integer,parameter :: SupplyRegion = 28
integer,parameter :: FuelReg_ALT2 = 24
integer,parameter :: FuelRegion = 24
integer,parameter :: FuelRegion_ALT1 = 24
integer,parameter :: FuelShareOption = 3
integer,parameter :: GasProductionStep = 41
integer,parameter :: GasRegion = 17 ! =NNGEM
integer,parameter :: GasSeason = 3
integer,parameter :: GenerationSeason = 4
integer,parameter :: INOXP = 5
integer,parameter :: JustOne =1
integer,parameter :: nHGCODE = 16 
integer,parameter :: nHGCODEFrom = 16
integer,parameter :: nHGCODETo = 16
integer,parameter :: nIFGD = 2
integer,parameter :: nRCF=1
integer,parameter :: numACI=8  ! 1..8
integer,parameter :: ACIOption=8  ! 0..7
integer,parameter :: OGSMRegion = 7           ! for CO2 EOR, not same as OGSMReg=13 (MNUMOR)
integer,parameter :: OGSMRegion_ALTFrom = 7
integer,parameter :: OGSMRegion_ALTTo = 7
integer,parameter :: OGSMSector = 13          ! CO2 Supply Sectors for OGSM EOR CO2 uses 
integer,parameter :: OilRegion = 9
integer,parameter :: OperatingMode = 5        ! normally 3: peak, base, intermediate. up to 5 for 5*3 15 slices
integer,parameter :: PlantGroup = 20000       ! emmparm:      PARAMETER(EMM_D_GRP = 20000) and M_EFD_GRPS=EMM_D_GRP
integer,parameter :: PlantGroupOrd = 20000    ! size based on EMM_D_GRP which matches M_EFD_GRPS, but in uefd indexes up_grp with the ordinal of 5-digit plant IDs so is actually a lot smaller
integer,Parameter :: PlantType_ECP = 80       ! 55(ECP_D_DSP) + 9(ECP_D_INT) + 14(ECP_D_RNW)+ 2(ECP_D_DGN)
integer,Parameter :: PlantType_ECPp2 = 82     ! 55(ECP_D_DSP) + 9(ECP_D_INT) + 14(ECP_D_RNW)+ 2(ECP_D_DGN) +2
integer,Parameter :: PlantType_EFD = 54
integer,parameter :: PlantType = 93           ! 54 efd + 38 ecp coal + STX as retrofit ecp coal to oil/gas 
integer,parameter :: RelyStep = 5
integer,parameter :: ResidProductionStep = 1
integer,parameter :: Season = 3
integer,parameter :: Season_SUP = 4  ! plus 0 index for total
integer,parameter :: SeasonCCS = 3
integer,parameter :: nSlice = 5  ! per season
integer,parameter :: nSliceSorted = 5  ! per season
integer,parameter :: SO2Region = 2
integer,parameter :: SO2Region_ALTFrom = 2
integer,parameter :: SO2Region_ALTTo = 2
integer,parameter :: SupplyStates = 49;    ! = UNSTAS for character*2 USTNME(1:49), lower 48+dc
! The following were added to transfer EMM variables and so are tied to EMM dimensioning parameters but must have different names
integer,Parameter :: PlantType_EFDp2 = 56  ! 54+2=56 Integer set for ECP plant types used in EFD
integer,parameter :: BiomassOption=6       ! ECP_D_CFS Biomass cofiring utilization options
integer,parameter :: BiomassRetrofit=5     ! ECP_D_RCF ECP BiomassCofiring retrofit categories
integer,parameter :: CHPFuel=12            ! MNUMCGF: combined heat and power fuels
integer,parameter :: CO2CapGroup=5         ! CO2_D_GRP MAXIMUM NUMBER OF CO2 CAP GROUPS
integer,parameter :: CPPRegion=9           ! EPAREG, Regions for EPA rule 111d: 6 + Alaska + Hawaii + national
integer,parameter :: DispPlantGroup=1200   ! EFD_D_MPG EFD Dispatchable plant group, do index is typically ECNTP
integer,parameter :: RenewPlantGroup=1300   ! EFD_D_MHG EFD Renewable plant group, do index is typically EHNTP
integer,parameter :: GroupSet=8          ! NUMBER OF periods per day
integer,parameter :: DemandSectors=4       ! MAXSEC=4: RES=1,COM=2,IND=3,TRA=4
integer,parameter :: DispatchableECP=55    ! ECP_D_DSP
integer,parameter :: DispatchableEFD=34    ! EFD_D_DSP Dispatchable cpacity types, subset of PlantType_ECP
integer,parameter :: DistGenPlantGroup=600     ! EFD_D_MDG Distributed generation EFD plant Groups
integer,parameter :: EMMStates=50          ! lower 48, dc, and US
integer,parameter :: Eleven=11
integer,parameter :: EmissionRank = 3
integer,parameter :: EmissionType=8        ! MNPOLLUT: Air Pollutants (C,CO,CO2,SOx,NOx,VOC,CH4,PART)
integer,parameter :: Fifteen=15
integer,parameter :: Five=5
integer,parameter :: Four=4
integer,parameter :: FuelRegion26=26
integer,parameter :: FuelType=60           ! EFD_D_NFL  corresponds with first 60 ECP plant types 
integer,parameter :: FuelsPerPlant=6       ! ECP_D_FPP Fuels per Plant, ECP
integer,parameter :: HoursADay=24
integer,parameter :: ImportStep = 5        ! EFD_D_CSS=5    maximum number of Canadian Supply steps
integer,parameter :: int_fuel_region=25    ! EFD_D_MFRG max number of fuel region for oil, gas, or coal regions
integer,parameter :: int_fuel_region_ALT1=25    ! alias for int_fuel_region
integer,parameter :: int_fuel_region1=26   ! EFDE$MFRG+1
integer,parameter :: int_fuel_region1_ALT1=26   ! alias for int_fuel_region1 
integer,parameter :: Knots=11              ! MAX_KNOTS max of these: FLRG_HR_KNOTS(FUEL_RGN,ECPt), heat-rate-related
integer,parameter :: LoadGroup = 12
integer,parameter :: MercuryClass=3        ! EPM_HG_CLS mercury class
integer,parameter :: OGSMReg=14            ! MNUMOR number of ogsm supply regions. happens to match 13 OGSM CO2 sectors
integer,parameter :: Months=12
!integer,parameter :: NOXRegion=6           ! NOX_GRP, NOX_D_GRP=5
!integer,parameter :: NScrubCode = 2        ! Maximum number of scrub codes by coal sector  (2 = unscrubbed; 1 = scrubbed)
integer,parameter :: GridResilienceSource=6  ! Maximum number of Grid Resilience sources   MX_GRDSRC =6
integer,parameter :: NOXStates=50          ! NOX_D_MST MAXIMUM NUMBER OF STATES WITH NOX CAP
integer,parameter :: OGSMRegion_ALT1=7
integer,parameter :: Two=2
integer,parameter :: OwnerShipType=5       ! EFD_D_OWN ownership types
integer,parameter :: PlanningHorizon=30    ! ECP_D_FPH (LENGTH OF FULL PLANNING HORIZON)
integer,parameter :: RegionsPerFuel=3      ! EFD_D_FRG supply/reporting Regions Per Fuel
integer,parameter :: SCALARSet=1           ! dummy Set used for scalars
!integer,parameter :: SO2ComplyGroup=2      ! EFD_D_SO2  SO2 compliance groups
integer,parameter :: SO2_Transport=3       ! mx_so2_tran, csapr,
integer,parameter :: Segment=4       ! NUMBER OF STEPS PER GROUP 
integer,parameter :: SupplyRegionAll=36    ! MNUMNR(28)+EFD_D_PROV(8) Superset of SupplREgion and Candian Supply REgion
integer,parameter :: Ten=10
integer,parameter :: Thirteen=13
integer,parameter :: Three=3
integer,parameter :: UtilitySector=41 ! nutsec (38) + 3 
integer,parameter :: VLoadSegment = 24 ! )/'X','W','V','U','T','S','R','Q','P','O','N','M','L','K','J', 'I','H','G','F','E','D','C','B','A'/ ! from UPLDCD(ECP_D_VLS) LOAD SEGMENT CODE.  or possible confusion with operatingmode UPMDCD(ECP_D_VLS)
integer,parameter :: WindClass=4           ! MNUMCL: wind classes, include 'wrenew'
integer,parameter :: FuelRegion_SUP = 25
integer,parameter :: OGSMRegion_SUP = 10
integer,parameter :: BiomassType_SUP = 5
integer,parameter :: CoalDemandRegion_SUP = 17
integer,parameter :: Two0 = 3
integer,parameter :: OGSMRegionEX = 8           ! for CO2 EOR, not same as OGSMReg=13 (MNUMOR)
integer,parameter :: OGSMRegionEX_ALTTo = 8

!=================================================================================================================
  interface
! this allows a number of optional arguments so it can build the mask for a variable number of components  
    subroutine makmsk(mask,a,b,c,d,e,f,g,h,i)
      character(LEN=*),intent(out) :: mask  ! 
      character(LEN=*) :: a
      character(LEN=*), optional :: b,c,d,e,f,g,h,i
    end subroutine makmsk

! this allows 4th, 5th, and 6th arguments to be optional
    SUBROUTINE DVAL(COL,RW,VAL,colmask,rowmask,called_from)
      character*16 col,rw
      real(8) val
      character(len=*),optional :: colmask,rowmask,called_from
    end subroutine dval

! this allows 4th and 5th arguments to be optional
    SUBROUTINE DRHS(RHS,RW,VAL,rowmask,called_from)
      character*16 rhs,rw
      real(8) val
      character(len=*),optional :: rowmask,called_from
    end subroutine drhs
    
    SUBROUTINE DBND(BND,COL,LVAL,UVAL,colmask,called_from)
      character(len=8) BND
      character(len=16) COL
      real(8) LVAL,UVAL
      character(len=*),optional :: colmask,called_from
    end subroutine dbnd

! this allows 3rd argument to be optional
    SUBROUTINE DROWTYPE(RW,RTYPE,rowmask)
      character*16 rw
      character*(*) RTYPE
      character(len=*),optional :: rowmask
    end subroutine DROWTYPE


end interface
!=================================================================================================================

end module efd_row_col
