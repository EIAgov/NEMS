! $Header: m:/default/source/RCS/ecp_row_col.f90,v 1.24 2021/05/25 15:50:36 LC2 Exp $
! the module "ecp_row_col" contains declarations and data shared throughout the subroutines in this file.  The
! purpose is store LP coefficients in dynamically-declared arrays for the AIMMS ECP interface.

module ecp_row_col

! This module declares shared storage variables to hold row and col name index parsing and 
! coefficient storage
  
  character*25 ecpsub ! subroutine invoking LP routines. Used to link coefficient parameters to subroutines for aimms conversion.

  integer,parameter :: max_set=6  ! maximum number of sets represented on an oml row/col name. 5 in uefd; 6 in uecp

! structure type to hold column name types and indexing attributes
  type column_name
    character*30 colnam_mask  ! oml column name mask
    character*24 colnam_aimms ! AIMMS variable (column) name 
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
    character*30 rownam_mask    ! oml row name mask
    character*24 rownam_aimms   ! AIMMS constraint (row) name 
    character*30 setnam(max_set)   ! indexing set names
    integer*2    indstt(max_set)   ! position in omlcol where each indexing field starts
    integer*2    indend(max_set)   ! position in omlcol where each indexing field ends
    character*1  row_type       ! E/L/G/N  E: = (default), N: nonconstraining/free, L: <=, G: >=
    integer*2    needsol           ! if >0, need solution data.
  end type
  parameter (max_aimms_param=2000) ! count of all the AIMMS indentifiers used to organize the LP coefficients
  character*38 aimms_param(max_aimms_param)
  
  parameter (max_domain_sets=11) ! most sets in the domain of any AIMMS parameter  = number of PSETNAM columns on worksheet "parameter" in aimecp.xlsx
! structure type to hold aimms parameter names, set domains, pointers to their coefficients in a sorted coefficient array
  type aimms_parameter
    character*38 name                       ! name of AIMMS parameter
    character*30 setnames(max_domain_sets)  ! names of the sets
    integer*2    numsets                    ! number of sets
    integer*4    idxFirst                   ! position in ecp_sortidx with first coefficient
    integer*4    idxLast                    ! position in ecp_sortidx with last coefficient
  end type
  type (aimms_parameter) :: aimparm(max_aimms_param)
  
  integer max_col_aimms, max_row_aimms  ! dimensioning parameters
  parameter (max_col_aimms=887)  !  set as prime number 2-3 times as big as number aimms columns
  parameter (max_row_aimms=887)  !  set as prime number 2-3 times as big as number aimms rows
  
  type (column_name), allocatable :: col_aimms(:)  ! allocate with max_col_aimms  ! holds array of aimms column identifiers
  type (row_name), allocatable    :: row_aimms(:)  ! allocate with max_row_aimms  ! holds array of aimms row identifiers
  
  integer*2    aimms_col_ID_num      ! unique identifier for this column aimms
  character*30 colnam_mask     ! oml column name mask
  character*24 colnam_aimms    ! AIMMS variable name 
  character*30 csetnam(max_set)      ! indexing set names
  integer*2    cindstt(max_set)      ! position in oml colnam where each indexing field starts
  integer*2    cindend(max_set)      ! position in oml colnam where each indexing field ends
  
  integer*2    aimms_row_ID_num      ! unique identifier for this row aimms
  character*30 rownam_mask      ! oml row name mask
  character*24 rownam_aimms    ! AIMMS constraint name 
  character*30 rsetnam(max_set)      ! indexing set names
  integer*2    rindstt(max_set)      ! position in omlrow where each indexing field starts
  integer*2    rindend(max_set)      ! position in omlrow where each indexing field ends
  character*1  row_type        ! E/L/G/N  E: = (default), N: nonconstraining/free, L: <=, G: >=
 
  integer max_ecp_coeff, max_ecp_row, max_ecp_row_hash, max_ecp_col,max_ecp_col_hash
  integer cnt_ecp_coeff, cnt_ecp_row, cnt_ecp_col
  integer*4 c_count,r_count,p_count                   ! number of row items for columns, rows, and parameters
  
  parameter (max_ecp_coeff=5000000)
  parameter (max_ecp_row=175000, max_ecp_row_hash=225023)
  parameter (max_ecp_col=500000, max_ecp_col_hash=500009)  ! for hashing, max_ecp_col_hash should be several times bigger than number of rows to reduce "collisions"

  integer num_aimms_param
  integer aimms_param_numcoeff(max_aimms_param)

  type coeff_int          ! holds coefficient at intersection of a given col, row, with AIMMS row/col matches
    character*8 ecpcolnam
    character*8 ecprownam     
    real(8)     coeff8                          ! coefficient.  8-byte double precision.
    integer*2   aimms_col_ID_num                ! aimms column/variable ID number (each indexed by various sets)
    integer*2   aimms_row_ID_num                ! aimms row/constraint ID number (each indexed by various sets)
    integer*2   iset_elements(max_domain_sets)  ! pointers to setElement positions holding the index elements for each coefficient; for converting aliases
    character*5 cset_elements(max_domain_sets)  ! set elements; used directly when writing coefficients out.
    character*25 ecpsub                         ! name of subroutine setting the coefficient
  end type

  
  integer,allocatable       :: ecp_sortidx(:)       ! holds the order of the coefficients sorted by AIMMS Parameter Name, oml column, oml row for mps output.
  character(64),allocatable :: ecp_sortkey(:)       ! holds the sort keys: Aimms Parameter Name, OML column & row, and fill order for mps output. 

 
  character*8 ecpcolnam  ! oml column name
  character*8 ecprownam  ! oml row name
  real(4) ecpcoeff
  real(8) ecpcoeff8
  logical set_lookup ! if true, selects option to look up individual set element in ecp_get_set_element. otherwise write out set to ecpsetdata.dat
  character*1 firstchar ! for writing sets, this is the first character of lines in ecpsetdata, normally a blank. for  sets with "_ALT" in name, it is "!" (comment)to AIMMS ignores it.
  
  character*30 ecp_setname  

  type (coeff_int), allocatable :: ecp_coeff(:)  ! allocate with max_ecp_coeff 

 !  
  integer spot ! location in hashing table of a given entry
  logical initial_store  ! argument for hashing to indicate when new entry is stored
  character(len=8), allocatable :: ecp_row_name(:)                ! oml rownam(8) used as hashing key. allocate with max_ecp_row_hash for hash insertion
  integer,allocatable           :: ecp_rowidx(:)                  ! holds the storage location the sorted rows for mps output.
  character(len=16),allocatable :: ecp_rowkey(:)                  ! holds the sorted rows keys
  character(len=8), allocatable :: ecp_col_name(:)                ! allocate with max_ecp_col_hash for hash insertion

  type col_sol  ! holds solution values and column indexing information from AIMMMS
    integer*2   aimms_col_ID_num
    real(8)     solval(5)   ! up to 5 solution values:  activity,objcost,lower, upper,reducedcost
    integer*2   nsets
    integer*2   iset_elements(max_set)
    character*5 cset_elements(max_set)
    character*8 ecpcolnam
    character*2 status ! oml compatibility: BS, UL, LL, or EQ
    
  end type
  type row_sol  ! holds solution values and row indexing information from AIMMMS
    integer*2   aimms_row_ID_num
    real(8)     solval(5)   ! up to 5 solution values: activity,slack activity,lower, upper,shadowprice
    integer*2   nsets
    integer*2   iset_elements(max_set)
    character*5 cset_elements(max_set)
    character*8 ecprownam
    character*1 row_type
    character*2 status ! oml compatibility: BS, UL, LL, EQ, or FR for free row
 end type
  
  character(len=8),allocatable :: row_sol_names(:) ! holds sequence of oml row names as read in and translated from AIMMS solution transfer file. allocated with max_ecp_row
  character(len=1),allocatable :: row_sol_types(:) ! holds sequence of oml row types as read in and translated from AIMMS solution transfer file. allocated with max_ecp_row
  character(len=8),allocatable :: col_sol_names(:) ! holds sequence of oml col names as read in and translated from AIMMS solution transfer file. allocated with max_ecp_col
  integer num_ecp_col_sol,num_ecp_row_sol
  
  type (col_sol), allocatable ::  ecp_col_sol(:) ! allocate with Max_ecp_col_sol: allocate ecp_col_sol(max_ecp_col_sol)
  type (row_sol), allocatable ::  ecp_row_sol(:) ! allocate with Max_ecp_row_sol: allocate ecp_row_sol(max_ecp_row_sol)


! Set names and elements for use in writing composite data arrays.
   type aimset
     character*32 setname
     integer*4 istart
     integer*4 iend
   end type
   integer, parameter :: max_aimsets=200
   type (aimset) :: aimsets(max_aimsets)
   character*5 setElements(100000) ! 
   integer numAimSets
   integer nSetElements

  integer io ! unit number for writing ecpcoeff_yyyy.txt

  integer write_ecp_mps/0/ ! if 1, call ecp_mps to write out each ecp mps file based on AIMMS coefficient collection.
  logical make_ecp_aimms /.false./   ! option to generate the AIMMS LP structural data for arraycode_ecp.f, such as the list of coefficient arrays, row/column lists, row types.
  integer AIMECPBG                  ! set via runtime option from scedes file. if 0, and make_ecp_aimms and false, omit some output in ecpcoeff debug file to save time; omit aimms validation files in aimms
  logical USE_AIMECP_SLNADJ /.true./             ! set during runtime based on collective values of CODEUSAGE of EFD LHS transfer variables found in aimefd.xlsx. it is set to .TRUE. if all are either 'RHS' or 'LHS_done',
                                    ! it is set to .FALSE. if any codeusage value is found to be either 'LHS' and 'LHS_coded'.   if .TRUE., skip FORTRAN EFD post-solution adjustments on EFD output variables.
                                    ! if .FALSE., execute FORTRAN EFD post-solution adjustments on EFD output variables.
  integer AIMMKECP                  ! set via runtime option from scedes file. if 1, set make_ecp_aimms to .true. 
  logical SKIP_ECPOML /.false./     ! flag to bypass passing fortran caculated ECP coefficients to AIMMS ECP and OML based on all 0 status found in ecparrays_all.txt
  
  external rtovalue ! function to get run-time option value AIMMSECP
  integer rtovalue
  integer AIMMSECP  ! run-time option to invoke AIMMS ECP LP if = 1
  character*25 filen_ecpcoeff
  integer iOutTxt
  integer colunit ! I-O unit for column solution retrieval debug file
  integer rowunit ! I-O unit for row    solution retrieval debug file

  character(LEN=30), allocatable :: xecpSetLook(:,:)     ! xlxs in Worksheet ECPSetMatch: set lookup table in Range ECPLOOK
  integer*2 ECPSetLookCount
  
  integer TransferCount  ! size of arrays to allocate
  character(LEN=50) ECPtransferarray(2000)
  integer           ECPtransfer_usage(2000)  ! indicator for how to treat a ECP transfer variable, 0: RHS usage only so only transfered to AIMMS, 1: LHS usage, to be passed back to NEMS, 2: LHS usage, omit calculated version declaration because aimms code already has declaration
  integer  CODEUSAGE_AIMECP_read/0/   !0/1 swtich to execute READAIMECPOPTIONS only once 

  integer AIM_Phase ! 1: if in LP set up phase, 2: if in LP solution retrieval phase.  Call getbout only in phase 2, AIMMS validation phase, after oml sol retrieval
! variables to hold mask strings
      character*30 BUILDNM_FREE_mask
      character*30 BUILDNM_HTRT_mask
      character*30 BUILDNM_mask
      character*30 CFG_mask
      character*30 COLUMN_BLD_mask
      character*30 COLUMN_CREATE_BNK_mask
      character*30 COLUMN_FREE_mask
      character*30 COLUMN_HTRT_mask
      character*30 COLUMN_L_mask
      character*30 COLUMN_MAX_SR_HTRT_mask
      character*30 COLUMN_MAX_SR_MR_mask
      character*30 COLUMN_MAX_SR_mask
      character*30 COLUMN_MIN_SR_HTRT_mask
      character*30 COLUMN_MIN_SR_L_mask
      character*30 COLUMN_MIN_SR_MR_mask
      character*30 COLUMN_MIN_SR_mask
      character*30 COLUMN_MR_HTRT_mask
      character*30 COLUMN_MR_mask
      character*30 COLUMN_USE_BNK_mask
      character*30 COLUMN_mask
      character*30 COL_CL_NG_mask
      character*30 COL_CLtNG_mask
      character*30 COL_CREATE_BNK_mask
      character*30 COL_HTRT_mask
      character*30 COL_OTHER_mask
      character*30 COL_OTHR_mask
      character*30 COL_RET_mask
      character*30 COL_TRANS_mask
      character*30 COL_USE_BNK_mask
      character*30 COL_mask
      character*30 ROW0_mask
      character*30 ROW1_mask
      character*30 ROW2_mask
      character*30 ROWCAR_mask
      character*30 ROWQ_mask
      character*30 ROWR_mask
      character*30 ROWU_mask
      character*30 ROWX_mask
      character*30 ROW_1st_mask
      character*30 ROW_ACI_mask
      character*30 ROW_ALL_mask
      character*30 ROW_BA_mask
      character*30 ROW_BK_mask
      character*30 ROW_CARR_mask
      character*30 ROW_CARS_mask
      character*30 ROW_CAR_mask
      character*30 ROW_CCS_mask
      character*30 ROW_CF_mask
      character*30 ROW_LOAD_mask
      character*30 ROW_CO2_mask
      character*30 ROW_COF_mask
      character*30 ROW_ERC_mask
      character*30 ROW_FLDV_mask
      character*30 ROW_FLRG_mask
      character*30 ROW_FR_mask
      character*30 ROW_F_mask
      character*30 ROW_FUEL_mask
      character*30 ROW_G_mask
      character*30 ROW_HG_mask
      character*30 ROW_HTRT_mask
      character*30 ROW_HY_MR_mask
      character*30 ROW_HY_mask
      character*30 ROW_MAX_GEN_mask
      character*30 ROW_MR_mask
      character*30 ROW_MV_mask
      character*30 ROW_NG_mask
      character*30 ROW_NOX_mask
      character*30 ROW_NSR_mask
      character*30 ROW_OGSM_mask
      character*30 ROW_OL_mask
      character*30 ROW_OTHR_FR_mask
      character*30 ROW_OTHR_mask
      character*30 ROW_RET_mask
      character*30 ROW_RNW_mask
      character*30 ROW_RS_mask
      character*30 ROW_SC_mask
      character*30 ROW_SO2_mask
      character*30 ROW_SR_mask
      character*30 ROW_TO_mask
      character*30 ROW_T_mask
      character*30 ROW_WD_mask
      character*30 ROW_XC_mask
      character*30 ROW_mask

! set cardinalities
integer,parameter :: SupplyRegion = 28 ! like NERC Region mnumnr. First in list because it is used to define other parameters below
integer,parameter :: BioCOFCF = 30 ! 5 Biomass Retrofit (ecp$RCF) x 6 utilization levels (ECP_D_CFS), collapsed into one set
integer,parameter :: BiomassCOF = 5 ! Biomass Retrofit Categories =  ECP_D_RCF
integer,parameter :: BiomassOtherUsage = 6  
integer,parameter :: BiomassStep = 49           !  NM_BM_SUP_STP+1, NM_BM_STP=48 from wodsupp.txt
integer,parameter :: BiomassType = 4
integer,parameter :: BuildType = 2  
integer,parameter :: CanadaProject = 99 
integer,parameter :: CanadianSupplyImportRegion = 8  !ECP$CS3 Canadian Supply import region counter
integer,parameter :: CanadianSupplyRegion = 8    
integer,parameter :: CanadianSupplyStep = 5
integer,parameter :: CanProvince = 3
integer,parameter :: CarbonEmissionType = 10  ! 10 (bnk, esc, fue, ind, lim, off, oth, ref, rsv, utl)
integer,parameter :: CarbonRegion = 3
integer,parameter :: CensusRegion = 11
integer,parameter :: CoalDemandRegion = 16
integer,parameter :: CoalDemandRegion_ALT1 = 16
integer,parameter :: CoalDemandRegion_ALT2 = 16
integer,parameter :: CoalDemandRegion_ALTfrom = 16
integer,parameter :: CoalDemandRegion_ALTto = 16
integer,parameter :: CoalDiversityType = 2     ! {L Lignite and S subbituminous}
integer,parameter :: CoalGroup = 1800 
integer,parameter :: NaturalGasGroup = 1800
integer,parameter :: CoalProductionStep = 11
integer,parameter :: CoalSupplyCurve = 53
integer,parameter :: CoalSupplyCurve_Dom = 41
integer,parameter :: CoalSupplyCurve_Int = 12
integer,parameter :: CoalSupplyStep = 24       ! I01, I02, ..., I11, OTH, DN1, DN2..UP1, UP2, ..UP5,ZR0
integer,parameter :: CoalType = 6
integer,parameter :: CommitYear = 3 ! should equal PlanYear...same as UNXPH read from  emmcntl.txt
integer,parameter :: NGCommitYear = 5 !INTEGER 1..5 created to handle column cE
integer,parameter :: DispatchType = 5 ! DS, IN, RN, DG, ST  short for dispatchable, intermittent, renewable, distributed gen, and storage
integer,parameter :: DistillateProductionStep = 1
integer,parameter :: FuelRegion = 24
integer,parameter :: FuelRegion_ALT1 = 24
integer,parameter :: FuelShareOption = 4
integer,parameter :: FuelSupplyStep = 18 ! 9 upper priced steps, A-I from center, 9 lower priced steps 1..9 from center
integer,parameter :: FuelType = 60 ! ECP_D_NFL=60.  but 13 is maximum integer found in "UPFLTP(IP,FPP), IP=1,ECP_D_DSP, FPP=1,ECP_D_FPP)
integer,parameter :: ECPFuelType = 15 !ECP_D_NFL=15  ECP NUMBER OF FUEL TYPES
integer,parameter :: GasProductionStep = 41
integer,parameter :: GasRegion = 17 ! =NNGEM
integer,parameter :: GasSeason = 2
integer,parameter :: GasSupplyStep = 41 
integer,parameter :: GenerationSeason = 3
integer,parameter :: HTRTYear = 3   ! should equal PlanYear...same as UNXPH read from  emmcntl.txt
integer,parameter :: ImportStep = 5 ! INTEGER, Parameter EFD_D_CSS=5    maximum number of Canadian Supply steps
integer,parameter :: JustOne =1
integer,parameter :: LoadGroup = 9  !changed from 3 based on the decision made by Laura, Jeff, and Augustine
integer,parameter :: LoadGroup2 = 9
integer,parameter :: LoadSegment = 3
integer,parameter :: NERCImport = SupplyRegion
integer,parameter :: nHGCODE = 1  ! mercury (Hg)
integer,parameter :: nHGCODEFrom = 1
integer,parameter :: nHGCODETo = 1
integer,parameter :: nIFGD = 2
integer,parameter :: NOXRegion = 6  !  emmparm: PARAMETER(NOX_D_GRP = 5).  region IDs differ under TRANRULE, so upping to 6
integer,parameter :: nRCF=1
integer,parameter :: Nuclear=450 ! max_nuc, parameter in includes/ecp_nuc
integer,parameter :: numACI=8  ! 0,1..7   ! activated carbon types
integer,parameter :: numACSS=1  ! num_ACSS number of aci supply steps.  1 
integer,parameter :: OGSMRegion = 7
integer,parameter :: OGSMRegion_ALTFrom = 7
integer,parameter :: OGSMRegion_ALTTo = 7
integer,parameter :: OGSMSector = 13
integer,parameter :: OilRegion = 9
integer,parameter :: OilSupplyStep = 1 
integer,parameter :: OperatingMode = 12  ! one for each vertical load segment, ECP_D_VLS=12
integer,parameter :: PlantGroup = 20000       ! based on EMM_D_GRP=20000. 
integer,parameter :: PlantType=80  
integer,parameter :: PlantType_ALT2=80     ! used for retrofit from one plant in row to another in column
integer,parameter :: PlanttypeW=PlantType    ! same as plantype, used for combining on/offshore wind into one wind category
integer,parameter :: PlanYear=3              ! should be same as UNXPH read from  emmcntl.txt
integer,parameter :: PlanYear_ALTB=3         ! should be same as UNXPH read from  emmcntl.txt
integer,parameter :: CommitYearSR = 3  ! should equal PlanYear...same as UNXPH read from  emmcntl.txt
integer,parameter :: RelyStep = 5
integer,parameter :: ResidProductionStep = 1
integer,parameter :: RetireGroup = 4       ! for IGRP=1,ECP_D_RET
integer,parameter :: RetrofitCFG = 107     ! NOT SURE ABOUT NUMBER of elements.  "COL" and then "001", "002",..."106" 
integer,parameter :: RPSRegion = SupplyRegion
integer,parameter :: RPSRegion_ALT1 = SupplyRegion
integer,parameter :: RPSRegion_ALT2 = SupplyRegion
integer,parameter :: RPSRegion_ALTI = SupplyRegion
integer,parameter :: RPSRegion_ALTX = SupplyRegion
integer,parameter :: RPSTranche=99  ! nm_st_rps=29, mx_st_rps=100, oml field is character*2. Make it an integer (I2.2) set
integer,parameter :: RPSTrancheID=9  ! st_rps_id is character*1 but dimensioned with PARAMETER(MX_ST_RPS=100). So far all states have 1 tranche ID code: '1'.  so I'll make the set {'1','2',...'9'}
integer,parameter :: Scrubber = 3          ! S, U, or X for Scrubbed, Unscrubbed and X:Total?
integer,parameter :: Season  = 4           !  =ECP_D_MSP  as a set, based on ISP (season-index) filled from local SCODE(ISP) filled from UPRGCD: '1','2','3','4'
integer,parameter :: Seasonmd = 8          !  =2*ECP_D_MSP as a combo of two sets based on ISP=1-to-ECP_D_MSP via SCODE(ISP) from uprgcd or SCODE_HTRT(ISP) from upmdcd
integer,parameter :: Seasonx = 4           !  =ECP_D_MSP  as a set, based on ISP (season-index) filled by X,W,V,U via UPLDCD(ISP)
integer,parameter :: Slice = 3
integer,parameter :: SO2Region = 2
integer,parameter :: SO2Region_ALTFrom = 2
integer,parameter :: SO2Region_ALTTo = 2
integer,parameter :: SStep_NucSubsidy = 25    ! Capacity Supply Elasticity steps. for nuclear ptc 25 steps
integer,parameter :: SStep_Subsidy = 10    ! Capacity Supply Elasticity steps. from  DO STEPS = 1 , MAX(1,ESTSWTCH(IP)), ESTSWTCH = 3 for most IP, but 10 for 2 renewabless
integer,parameter :: SupplyRegion_ALT1 = SupplyRegion
integer,parameter :: SupplyRegion_ALT2 = SupplyRegion
integer,parameter :: SupplyRegion_ALTfrom = SupplyRegion
integer,parameter :: SupplyRegion_ALTto = SupplyRegion
integer,parameter :: SupplyRegion_ALTtwo = SupplyRegion
integer,parameter :: SupplyRegion_ALTX = SupplyRegion
integer,parameter :: SupplyRegion_ALTY = SupplyRegion
integer,parameter :: SupplyRegion_ALTZ = SupplyRegion
integer,parameter :: SupplyState = 49; ! = UNSTAS , for character*2 USTNME(49)
integer,parameter :: SupplyStep = 6 ! from 'emmparm', INTEGER, parameter :: ECP_D_DGS=6 ! ECP NUMBER OF DIST GEN AVOIDED TRANS. STEPS


integer,parameter :: BiomassOption=6       ! ECP_D_CFS Biomass cofiring utilization options
integer,parameter :: BiomassRetrofit=5     ! ECP_D_RCF ECP BiomassCofiring retrofit categories
integer,parameter :: CHPFuel=12            ! MNUMCGF: combined heat and power fuels
integer,parameter :: CO2CapGroup=5         ! CO2_D_GRP MAXIMUM NUMBER OF CO2 CAP GROUPS
integer,parameter :: CPPRegion=9           ! EPAREG, Regions for EPA rule 111d: 6 + Alaska + Hawaii + national
integer,parameter :: DayTypeSet=9          ! NUMBER OF periods per day   --- changed from 4
integer,parameter :: DemandSectors=4       ! MAXSEC=4: RES=1,COM=2,IND=3,TRA=4
integer,parameter :: DispatchableECP=55    ! ECP_D_DSP
integer,parameter :: DispatchableECPplus1=56   ! ECP_D_DSP+1
integer,parameter :: DispatchableEFD=34    ! EFD_D_DSP Dispatchable cpacity types, subset of PlantType_ECP
integer,parameter :: DSMLoadGroup=8        ! MAXECPB MAXIMUM NUMBER OF BLOCKS IN ONE SEASON LDC FOR ECP
integer,parameter :: DSMSupplyRegion=25    ! MAXNRG in dsmdimen UNRGNS
integer,parameter :: EmissionRank = 3
integer,parameter :: EmissionType=8        ! MNPOLLUT: Air Pollutants (C,CO,CO2,SOx,NOx,VOC,CH4,PART)
integer,parameter :: EMMStates=50          ! lower 48, dc, and US
integer,parameter :: Fifteen=15
integer,parameter :: DepreciationYears = 26 ! 
integer,parameter :: DepreciationOptions = 6 !
integer,parameter :: Four=4
integer,parameter :: FuelRegion26=26
integer,parameter :: FuelsPerPlant=6       ! ECP_D_FPP Fuels per Plant, ECP
integer,parameter :: HoursADay=24
integer,parameter :: Knots=11              ! MAX_KNOTS max of these: FLRG_HR_KNOTS(FUEL_RGN,ECPt), heat-rate-related
integer,parameter :: MercuryClass=3        ! EPM_HG_CLS mercury class
integer,parameter :: Months=12
integer,parameter :: OGSMReg=14            ! MNUMOR number of ogsm supply regions. happens to match 14 OGSM CO2 supply sectors
integer,parameter :: NOXStates=50          ! NOX_D_MST MAXIMUM NUMBER OF STATES WITH NOX CAP
integer,parameter :: OGSMRegion_ALT1=7
integer,parameter :: OwnerType=2
integer,parameter :: PlanningHorizon=30    ! ECP_D_FPH (LENGTH OF FULL PLANNING HORIZON)
integer,Parameter :: PlantType_EFDp2 = 56  ! 54+2=56 Integer set for ECP plant types used in EFD
integer,parameter :: RegionsPerFuel=3      ! EFD_D_FRG supply/reporting Regions Per Fuel
integer,parameter :: SCALARSet=1           ! dummy Set used for scalars
integer,parameter :: SO2_Transport=3       ! mx_so2_tran, csapr,
integer,parameter :: SO2ComplyGroup=2      ! EFD_D_SO2  SO2 compliance groups
integer,parameter :: StepsPerGroup=4       ! NUMBER OF STEPS PER GROUP 
integer,parameter :: SupplyRegionAll=36    ! MNUMNR(28)+EFD_D_PROV(8) Superset of SupplREgion and Candian Supply REgion
integer,parameter :: Thirteen=13
integer,parameter :: Three=3
integer,parameter :: UtilitySector=41 ! nutsec (38) + 3 
integer,parameter :: VLoadSegment = 24 ! )/'X','W','V','U','T','S','R','Q','P','O','N','M','L','K','J', 'I','H','G','F','E','D','C','B','A'/ ! from UPLDCD(ECP_D_VLS) LOAD SEGMENT CODE.  or possible confusion with operatingmode UPMDCD(ECP_D_VLS)
integer,parameter :: WindClass=4           ! MNUMCL: wind classes, include 'wrenew'
integer,parameter :: CanadianSupplyImport=100    ! ECP$CS2
integer,parameter :: CoalConfiguration=144       ! MX_CNFG  Maximum Number of Coal Configuration
integer,parameter :: ConstructionPeriod=10       ! ECP_D_LCP      LONGEST CONSTRUCTION PROFILE
integer,parameter :: DayTimePeriod=3             ! MAXDTP   
integer,parameter :: DBColumns=90                ! MAXCOLS Maximum Columns written in a DB record
integer,parameter :: DBRecords=90                ! MAXRECS 
integer,parameter :: DemSideProg=12              ! MAXDSMP
integer,parameter :: DistGenAvoidStep=6          ! ECP_D_DGS
integer,parameter :: DistributedGenType=2        ! ECP_D_DGN
integer,parameter :: DiversityProfile=500        ! MX_PRF
integer,parameter :: ECPLoadGroup=12             ! ECP_D_STP
integer,parameter :: ECPStorageType=3            ! ECP_D_STO
integer,parameter :: EFDFuelType=60              ! EFD_D_NFL
integer,parameter :: EFDPlantType=3              ! EFD_D_CAP
integer,parameter :: ElasticityStep=3            ! ECP_D_SSTP
integer,parameter :: ElasticityStep_ALT1=3       ! ECP_D_SSTP
integer,parameter :: ExpectationsYrIndex=92      ! MNXYR
integer,parameter :: ExplicitPlanningHorizon=10  ! ECP_D_XPH
integer,parameter :: FortyNine=49                ! 
integer,parameter :: Fourteen=14                 ! 
integer,parameter :: FullPlanningHorizon=30      ! ECP_D_FPH 
integer,parameter :: ImportExportReg=8           ! ECP_D_MXP
integer,Parameter :: Intermittent=9              ! ECP_D_INT      
integer,parameter :: IntermittenRenStor=23       ! ECP_D_I_R  =  ECP_D_INT(9) + ECP_D_RNW(11) + ECP_D_STO (3)
integer,parameter :: LDCBlocks=192               ! MAXECTB=MAXECPB (8) * MAXECPSG (24)
integer,parameter :: NewPrice=38                 ! NCLPR2
integer,parameter :: NGBaseLoad=1800             ! MX_NGBS
integer,parameter :: NuclearUnit=900             ! MAX_NUC
integer,parameter :: OtherRenewable=11           ! ECP_D_RNW
integer,parameter :: PlantGrpReg=3100            ! WPLT_D_GRP
!integer,parameter :: PlantRec=27000              ! WPLT_D_REC
integer,parameter :: PlanReg=28                  ! WPLT_D_RGN
integer,parameter :: RetrofitCombinations=47     ! MX_ROPT
integer,parameter :: RetrofitComponent=6         ! MX_RCMB
integer,parameter :: StateCodes=54               ! MX_ST_CODES 0:mx_st_codes
integer,parameter :: SupplyCurves=53             ! MX_SUPPLY_CURVES (41) + MX_INTL_CURVES (12)
integer,parameter :: Thousand=1000               
integer,parameter :: TradCogenFuelType=12        ! TC_FUELS
integer,parameter :: UtilityType=11              ! MX_TYPE  0:mx_type
 
 
! need for new set element decoding: 
 integer, parameter :: BiomassProductionStep=50 ! nwdsupp=50 in wrenew
 integer, parameter :: BiomassSector=6          ! fstpmx
 integer, parameter :: BoilerType=3             ! ECP_D_BTP boiler types per plant, up to 3
!integer, parameter :: CanadianSupplyImport=100 ! ECP$CS2  Import project counter
!integer, parameter :: CoalConfiguration=144    ! MX_CNFG
!integer, parameter :: ConstructionPeriod=10    ! ECP_D_LCP 
!integer, parameter :: DayTimePeriod=3          ! MAXDTP 
!integer, parameter :: DBColumns=90             ! MAXCOLS 
!integer, parameter :: DBRecords=100            ! MAXRECS 
!integer, parameter :: DemSideProg=12           ! MAXDSMP 
 integer, parameter :: DispPlantGroup=1200      ! EFD_D_MPG 
!integer, parameter :: DistGenAvoidStep=6       ! ECP_D_DGS  
 integer, parameter :: DistGenPlantGroup=600    ! EFD_D_MDG 
!integer, parameter :: DistributedGenType=2     ! ECP_D_DGN 
!integer, parameter :: DiversityProfile=500     ! MX_PRF 
 integer, parameter :: DSMBlock=8               ! MAXEFDB 
 integer, parameter :: DSMSeason=4              ! MAXEFDS 
 integer, parameter :: DSMSegment=24            ! MAXEFDSG 
 integer, parameter :: DSMSegmentSeason=4       ! MAXEFDSS 
!integer, parameter :: ECPLoadGroup=12          ! ECP_D_STP 
!integer, parameter :: ECPStorageType=3         ! ECP_D_STO  
!integer, parameter :: EFDFuelType=60           ! EFD_D_NFL 
 integer, parameter :: EFDOwnerShipType=5       ! EFD_D_OWN 
!integer, parameter :: EFDPlantType=54          ! EFD_D_CAP =EFD_D_DSP (34) + EFD_D_RNW (18) + EFD_D_DGN (2)
!integer, parameter :: ElasticityStep=3         ! ECP_D_SSTP 
 integer, parameter :: Eleven=11                ! 
 integer, parameter :: ExpectationYrIndex=92    ! MNXYR+1 (0:mnxyr)
!integer, parameter :: ExplicitPlanningHorizon=10 ! ECP_D_XPH 
!integer, parameter :: FortyNine=49             !  
!integer, parameter :: Fourteen=14              !  
!integer, parameter :: FullPlanningHorizon=30   ! ECP_D_FPH 
!integer, parameter :: ImportExportReg= 8       ! ECP_D_MXP 
 integer, parameter :: IntermittentRenStor=23   ! ECP_D_I_R = ECP_D_INT (9) + ECP_D_RNW (11) + ECP_D_STO (3)=(23)
!integer, parameter :: Intermittent=9           ! ECP_D_INT 
!integer, parameter :: LDCBlocks=192            ! MAXECTB =MAXECPB (8 *MAXECPSG (24)
!integer, parameter :: NewPrice=38              ! NCLPR2 
!integer, parameter :: NGBaseLoad=1800          ! MX_NGBS 
!integer, parameter :: NuclearUnit=900          ! MAX_NUC 
!integer, parameter :: OtherRenewable=11        ! ECP_D_RNW 
 integer, parameter :: OwnerShipType=2          ! ECP_D_OWN 
 integer, parameter :: PlantGroupOrd=20000      ! M_EFD_GRPS = EMM_D_GRP = 20000
!integer, parameter :: PlantGrpReg=3000         ! WPLT_D_GRP 
!integer, parameter :: PlantRec=27000           ! WPLT_D_REC 
 integer, parameter :: PlantReg=28              ! WPLT_D_RGN 
 integer, parameter :: PlantType_ECP=83         ! 55(ECP_D_DSP) + 9(ECP_D_INT) + 14(ECP_D_RNW)+ 3 (ECP_D_STO) + 2(ECP_D_DGN)
!integer, parameter :: RetrofitCombinations=47  ! MX_ROPT 
!integer, parameter :: RetrofitComponent=8      ! MX_RCMB+2
 integer, parameter :: SliceGroup=24            ! MAXECPSG
!integer, parameter :: StateCodes=54            ! MX_ST_CODES+1 (0:mx_st_codes) 
!integer, parameter :: SupplyCurves=53          ! MX_SUPPLY_CURVES+MX_INTL_CURVES
!integer, parameter :: Thousand=1000            !  
!integer, parameter :: TradCogenFuelType=12     ! TC_FUELS
!integer, parameter :: UtilityType=11           ! MX_TYPE+1 (0:MX_TYPE) 
 integer, parameter :: MNUMYRX = 64             !  MNUMYR + ECP_D_XPH 
 integer, parameter :: MNUMYRF = 91             !  MNUMYR + ECP_D_FPH 

 !SUPer Sets   
 integer,parameter :: BiomassType_SUP = 5
 integer,parameter :: CoalDemandRegion_SUP = 17
 integer,parameter :: ExplicitPlanningHorizon_SUP=11  ! 0:ECP_D_XPH
 integer,parameter :: FuelRegion_SUP = 25
 integer,parameter :: MNUMYR_SUP = 64             ! -2:MNUMYR
 integer,parameter :: OGSMRegionEX = 8           ! for CO2 EOR, not same as OGSMReg=13 (MNUMOR)
 integer,parameter :: OGSMRegionEX_ALTTo = 8
 integer,parameter :: Season_SUP = 5  ! plus 0 index for total
!=================================================================================================================
  interface                                      
! this allows a number of optional arguments so it can build the mask for a variable number of components.
! the subroutine makmsk is in uefd.f
    subroutine makmsk(mask,a,b,c,d,e,f,g,h,i)
      character(LEN=*),intent(out) :: mask  ! 
      character(LEN=*) :: a
      character(LEN=*), optional :: b,c,d,e,f,g,h,i
    end subroutine makmsk

! this allows 4th, 5th, and 6th arguments to be optional
    SUBROUTINE CVAL(COL,RW,VAL,colmask,rowmask,called_from)
      character*16 col,rw
      real(8) val
      character(len=*),optional :: colmask,rowmask,called_from
    end subroutine cval

! this allows 4th argument to be optional
    SUBROUTINE CRHS(RHS,RW,VAL,rowmask,called_from)
      character*16 rhs,rw
      real(8) val
      character(len=*),optional :: rowmask,called_from
    end subroutine crhs
    
    SUBROUTINE CBND(BND,COL,LVAL,UVAL,colmask,called_from)
      character(len=8) BND
      character(len=16) COL
      real(8) LVAL,UVAL
      character(len=*),optional :: colmask,called_from
    end subroutine cbnd

! this allows 3rd argument to be optional
    SUBROUTINE CROWTYPE(RW,RTYPE,rowmask)
      character*16 rw
      character*(*) RTYPE
      character(len=*),optional :: rowmask
    end subroutine CROWTYPE

end interface

end module ecp_row_col
