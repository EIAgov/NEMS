! $Header: m:/default/source/RCS/prepplt.f,v 1.59 2020/09/11 12:37:41 LC2 Exp $
!
!     READ PLANT DATA
!
      IMPLICIT NONE
      INCLUDE 'parametr'
      INCLUDE 'emmparm'
      INCLUDE 'control'
      INCLUDE 'ncntrl'
      INCLUDE 'bildin'
      include 'uefdout'
      include 'dsmunits' !<< include file with unit number for ldsm message file
!
      INTEGER I_CNTL
!
!     VARIABLES TO CALL FILE_MGR FOR INITIALIZATION
!
      CHARACTER*18 FM_NAME/'FILELIST'/
      CHARACTER*80 FILENM
      INTEGER ISTATUS,IRG
      LOGICAL NEW/.FALSE./
      INTEGER FILE_MGR,IFILE
      LOGICAL WHOOPS ! error flag
      EXTERNAL FILE_MGR
!
!     CALL FILE_MGR FIRST TIME TO OPEN UP AND READ THE LIST OF FILES
!
      ISTATUS=FILE_MGR('I',FM_NAME,NEW)
!
!     OPEN THE ECP INPUT DIRECT ACCESS FILE

      NEW = .TRUE.
      FILENM='ECPIDAF'
      UF_BLD=FILE_MGR('O',FILENM,NEW)
!
!     SET INTERNAL YEAR AND ITERATION VARIABLES USING GLOBAL VARIABLES
!
      I_CNTL = 80

      OPEN(UNIT=22,FILE='EMMDBUG')
      OPEN(UNIT=13,FILE='EMMREPT')
      OPEN(UNIT=18,FILE='EMMPRNT')

!
      NEW=.TRUE.
      FILENM='LDSMRPT'
      IMSG=FILE_MGR('O',FILENM,NEW)  !Open LDSM REPORT FILE
      NEW=.FALSE.
      FILENM='LDSMDAF'
      IODB=FILE_MGR('O',FILENM,NEW)  !Open DAF-LSR-DB
      NEW=.FALSE.
      FILENM='LDSMCRS'
      IOCR=FILE_MGR('O',FILENM,NEW)  !Open COMMERCIAL RESTART FILE
      FILENM='LDSMRRS'
      IORR=FILE_MGR('O',FILENM,NEW)  !Open RESIDENTIAL RESTART FILE
!
      CALL DSMRST(WHOOPS) ! Read structure file and DSM option database * need this for UNRGNS
      IF(WHOOPS) THEN
         WRITE(6,*)'<))) Data passed by LDSM may be CORRUPTED'
      ENDIF
!
!     READ THE CONTROL FILE
!
      CALL RDCNTRL(I_CNTL)
      CURIYR = UESTYR - UHBSYR + 1
!
!     READ ECPDAT FOR REGIONAL MULTIPLIERS
!
      DO IRG = 1, UNRGNS
         CALL RDBD_UECP(IRG)
         CALL STRBLD(1,IRG)
      ENDDO
!
!     READ EFP INPUT DATA (FORMERLY GURFMAPS)
!
      CALL RDMAPS
!
!     READ IN PLANT DATA
!
      CALL RDPLNT
!
!     STORE PLANT CONTROL DATA IN BEGINNING OF THE PLANT DAF
!
      CALL STRPCNTL
!
      IF (UF_DBG.GT.0) CLOSE(UF_DBG)
      IF (UF_RPT.GT.0) CLOSE(UF_RPT)
      IF (UF_CRV.GT.0) CLOSE(UF_CRV)
      CLOSE(UF_PLT)
!
      END

!
!
!     RDPLNT
!
      SUBROUTINE RDPLNT
      USE SQLITE
!
      IMPLICIT NONE
      include'parametr'
      include'emmparm'
      include'control'
      include'ecpcntl'
      include'dispin'
      include'plntin'
      include'plntctl'
      include'elcntl'
      include'elout'
      include'ncntrl'
      include'uefdout'
!
      INTEGER*4 N_PART,N_SCBT
      PARAMETER(N_PART=6,N_SCBT=6)
      REAL*8 TC_NP,TC_SUM,TC_WIN,THRATE,TSCBEF,TSCBCST,TSEQEF,T_CF,TMP_TOT
      REAL*8 SC_NP,SC_SUM,SC_WIN,SHRATE,SSCBEF,SSCBCST,SSEQEF,S_CF
      REAL*8 M_CF(12),T_FSHR(EFD_D_FPP),T_GRID,S_GRID
      REAL*8 T_VOM,T_FOM,T_GA,T_CAPAD,S_VOM,S_FOM,S_GA,S_CAPAD,TCOUNT,SCOUNT
      REAL*8 GA_PLT(EFD_D_DSP)
      REAL*8 Z_CF(12),S_FSHR(EFD_D_FPP),TOTFSH
      REAL*8 TMP_CAP(MNUMYR),TMP_RET(MNUMYR)
      REAL*8 FO
!     REAL*8 T_NRET1,T_NRET2,T_NRET3
      REAL*8 T_CCSROV,T_CCSF,T_CCSV,T_CCSHR,T_CCSCAPA
      REAL*8 T_DSIOV,T_DSIR,T_DSIV,T_DSIF,T_FFOV,T_FFF,T_FFV,T_ESPU
      REAL*8 T_CFBU,T_CFBUV,T_CFBUF,T_NG_COST,T_NG_TRAN
      REAL*8 SDSIOV,SDSIR,SDSIV,SDSIF,SFFOV,SFFF,SFFV,SESPU
      REAL*8 SCFBU,SCFBUV,SCFBUF,S_NG_COST,S_NG_TRAN
      REAL*8 T_LONG,T_LAT
      REAL*8 SCCSROV, SCCSF, SCCSV, SCCSHR, SCCSCAPA
      REAL*8 SCOMB_O,TCOMB_O
      REAL*8 SCOMB_F,TCOMB_F
      REAL*8 SCOMB_V,TCOMB_V
      REAL*8 SCOMB_R,TCOMB_R
      REAL*8 SSNCR_O,TSNCR_O
      REAL*8 SSNCR_F,TSNCR_F
      REAL*8 SSNCR_V,TSNCR_V
      REAL*8 SSNCR_R,TSNCR_R
      REAL*8 SSCR_O,TSCR_O
      REAL*8 SSCR_F,TSCR_F
      REAL*8 SSCR_V,TSCR_V
      REAL*8 SSCR_R,TSCR_R
      REAL*8 SNOX_R,TNOX_R
      REAL*8 THR_O,THR_F,THR_V,THR_IMP
      REAL*8 SHR_O,SHR_F,SHR_V,SHR_IMP
      REAL*8 GDPADJ2012
      REAL*8 SNUCA_O,TNUCA_O
      REAL*4 CP_BY_CL_UNIT_N_YR(MNUMYR,MX_CL_UNIT)
      REAL*4 AC1, AC2, AC3, AC4
      INTEGER*4 T_COMB
      INTEGER*4 T_POST
      INTEGER*4 THR_QT
      INTEGER T_IGRP,T_GRP,T_GRP2,LGRP,IEFD,FL_NUM,RG_NUM,IMO,IFL,JFL,I_CL,I_TST,TYR,N_COUNT
      INTEGER ITEST,IRG,IGRP,ICNT,JCNT,IFP,IFP2,UF_FLG,IYR,ILAST,ICAR,IST
      INTEGER IEND,ISTRT,MONTH,IOW2,IOWN,JTEST
      INTEGER IYRMO,SYRMO,RYRMO,IVIN3,IECP,OWRGN,PHRGN,IROW
      INTEGER ITAB,IY,IS,IDIM1,JDIM1,NDIM1,IDIM2,JDIM2,NDIM2,JROW
      INTEGER DBREC,IFTAB,IREC,IPARM1,IPARM2,DBCMB
      INTEGER NROWS,IROWS,NFTAB,IDTYP
      INTEGER*4 T_RMO           ! Retire Month (12 If Not Specified)
      INTEGER*4 T_RYR           ! Retire Year (9999 If Not Specified)
      INTEGER*4 T_SMO           ! On-Line Month (12 If Not Specified)
      INTEGER*4 T_SYR           ! On-Line Year (9999 If Not Specified)
      INTEGER*4 TSCBGRP         ! Scrubber Group
      INTEGER*4 TSCBYR          ! Scrubber On-Line Year
      INTEGER*4 TVIN            ! Plant Vintage 0 = Cancelled or Retired <1990
      INTEGER*4 TRFURB          ! Refurbishment Year
      INTEGER*4 T_NRET          ! Nuclear Ret/LE Switch
      INTEGER*4 T_CLRG          ! Coal Region Number
      INTEGER*4 T_CR            ! Census Region Number
      INTEGER*4 T_GR            ! Natural Region Number
      INTEGER*4 T_CAR           ! Carbon Region Number
      INTEGER*4 TECPT           ! Plant Type Number for ECP
      CHARACTER*2 ECPcd         ! Plant Type Code for ECP
      INTEGER*4 JECPT           ! Plant Type Number for ECP
      INTEGER*4 TST_FOR_CL_ECP  ! Highest ECP in Plant File for Coal Units
      INTEGER*4 TEFPT           ! Financial Type Number for EFP
      INTEGER*4 TEFDT           ! EFD Plant Type Number
      CHARACTER*3 EFDcd         ! Three Digit EFD Plant Type Code
      INTEGER*4 TNOPER          ! NEMS Region Code for Location of Plant
      INTEGER*4 TNOWN           ! NEMS Region Code for Unit Owner
      INTEGER*4 TFL(EFD_D_FPP)    ! NEMS FUEL IDEXES
      CHARACTER*2 CFL(EFD_D_FPP)    ! NEMS FUEL CODES
      INTEGER*4 TFOWN           ! Ownership Type 1=Private, 2=Public, 3=NUG
      INTEGER*4 T_MRUN           ! Must Run Code 1 = Yes, 0 = No
      INTEGER*4 T_CCSLOC,T_COOL     
      CHARACTER*512 LABELS
      CHARACTER*80 FILENM
      CHARACTER*20 T_PNM
      CHARACTER*10 CL_UNIT_TEXT
      CHARACTER*5 T_CID,T_PID,T_UID
      CHARACTER*4 RECTYP
      CHARACTER*5 T_PCA
      CHARACTER*5 W_PCA,W_PID
      CHARACTER*2 TSTATE        ! 2-Digit State Abbreviation for Plant Location
      CHARACTER*2 IFLAG
      CHARACTER*2 TSCBT2(N_SCBT),TPART2(N_PART),TSCBT,TPART
      CHARACTER*1 TSCBT1(N_SCBT),TPART1(N_PART),JSCBT,JPART
      CHARACTER*1 JFLAG
      LOGICAL NEW
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
      
      type(sqlite_database)                      :: db
      type(sqlite_statement)                     :: stmt
      type(sqlite_column), dimension(:), pointer :: col
      character(len=40), pointer, dimension(:)   :: result
      character(len=80)                          :: errmsg
      
      integer                                    :: i
      integer                                    :: j
      integer                                    :: id
      real                                       :: bkgain
      logical                                    :: finished
      CHARACTER*3                                :: T_EFDcd
      CHARACTER*2                                :: T_ECPcd


      DATA TSCBT2/"D "," D","W "," W","N "," N"/
      DATA TSCBT1/"D", "D", "W", "W", "N", "N" /
!
      DATA TPART2/"B "," B","E "," E","N "," N"/
      DATA TPART1/"B", "B", "E", "E", "N", "N" /
    
      DATA GDPADJ2012/1.017037/
      
      DATA AC1/118.3/      !lbs/mmbtu for NG
      DATA AC2/0.90/       !capture percent
      DATA AC4/0.192501/  !kwh/lb co2
!
!     INITIALIZE COAL UNIT ARRAYs
!
      NUM_CL_UNIT = 0
      COAL_UNIT_ID = "          "
      MAP_TO_WIGRP_FRST = 0
      MAP_TO_WIGRP_NEXT = 0
      SCAP_BY_COAL_UNIT = 0.0
      CP_BY_CL_UNIT_N_YR = 0.0
      N_COUNT = 0
      COAL_REGION = 0
      INITIAL_ECP_TYPE = 0
!
!     SET G&A FACTORS (FUNCTION OF FIXED O&M)
!
      DO IEFD = 1 , EFD_D_DSP
       IF (IEFD .EQ. UICNU .OR. IEFD .EQ. UIANC .OR. IEFD .EQ. UISMR)THEN
        GA_PLT(IEFD) = 0.20
       ELSE
        GA_PLT(IEFD) = 0.15
       END IF
      END DO
!
!     READ AND STORE THE PLANT DATA
!
!     OPEN THE PLANT DIRECT ACCESS FILE
!
      NEW=.TRUE.
      FILENM = 'PLNTDAF'
      UF_PLT=FILE_MGR('O',FILENM,NEW)
!
!     INITIALIZE PLANT CONTROL ARRAYS
!
      CALL RDPGRP
!
!     READ IN GENERIC PLANT INFORMATION FILE
!
      CALL RDPNEMS
!
!     OPEN THE PLANT FILE
!
      DO IGRP = 1 , EMM_D_GRP
         WNXT_SGRP(IGRP) = 0
      END DO
!
      WREC_INT = 0
      WGRP_INT = 0
!
      ISTRT = 0
      IEND = 0
      ICNT = 0
      JCNT = 0
      T_IGRP = 0
      T_GRP = 0
      T_GRP2 = 0
      SCOUNT = DBLE(0.0)
      SC_NP = DBLE(0.0)
      SC_SUM = DBLE(0.0)
      SC_WIN = DBLE(0.0)
      DO IFP = 1 , EFD_D_FPP
         S_FSHR(IFP) = DBLE(0.0)
      END DO
      DO MONTH = 1 , 12
         Z_CF(MONTH) = DBLE(0.0)
      END DO
      LGRP = 0
      TST_FOR_CL_ECP = WIIS ! Highest ECP in Plant File for Coal Units
      
!              WRITE(6,1100) "SQLITE_OPEN"
         1100 FORMAT(1X,A)
        
              call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )
              allocate ( col(103) )
        
        !     QUERY THE DATABASE TABLE
        
              call sqlite3_column_query( col(001), 'T_IGRP', sqlite_int )
              call sqlite3_column_query( col(002), 'T_GRP', sqlite_int )
              call sqlite3_column_query( col(003), 'T_GRP2', sqlite_int )
              call sqlite3_column_query( col(004), 'T_CID', sqlite_char )
              call sqlite3_column_query( col(005), 'T_PCA', sqlite_char )
              call sqlite3_column_query( col(006), 'T_PID', sqlite_char )
              call sqlite3_column_query( col(007), 'T_UID', sqlite_char )
              call sqlite3_column_query( col(008), 'T_PNM', sqlite_char )
              call sqlite3_column_query( col(009), 'TVIN', sqlite_int )
              call sqlite3_column_query( col(010), 'EFDcd', sqlite_char )
              call sqlite3_column_query( col(011), 'ECPcd', sqlite_char )
              call sqlite3_column_query( col(012), 'TFOWN', sqlite_int )
              call sqlite3_column_query( col(013), 'T_MRUN', sqlite_int )
              call sqlite3_column_query( col(014), 'TEFPT', sqlite_int )
              call sqlite3_column_query( col(015), 'TNOPER', sqlite_int )
              call sqlite3_column_query( col(016), 'TNOWN', sqlite_int )
              call sqlite3_column_query( col(017), 'T_CLRG', sqlite_int )
              call sqlite3_column_query( col(018), 'TSTATE', sqlite_char )
              call sqlite3_column_query( col(019), 'T_CR', sqlite_int )
              call sqlite3_column_query( col(020), 'T_GR', sqlite_int )
              call sqlite3_column_query( col(021), 'TCOUNT', sqlite_real )
              call sqlite3_column_query( col(022), 'TC_NP', sqlite_real )
              call sqlite3_column_query( col(023), 'TC_SUM', sqlite_real )
              call sqlite3_column_query( col(024), 'TC_WIN', sqlite_real )
              call sqlite3_column_query( col(025), 'THRATE', sqlite_real )
              call sqlite3_column_query( col(026), 'T_CCSROV', sqlite_real )
              call sqlite3_column_query( col(027), 'T_CCSF', sqlite_real )
              call sqlite3_column_query( col(028), 'T_CCSV', sqlite_real )
              call sqlite3_column_query( col(029), 'T_CCSHR', sqlite_real )
              call sqlite3_column_query( col(030), 'TRFURB', sqlite_int )
              call sqlite3_column_query( col(031), 'T_SYR', sqlite_int )
              call sqlite3_column_query( col(032), 'T_SMO', sqlite_int )
              call sqlite3_column_query( col(033), 'T_RYR', sqlite_int )
              call sqlite3_column_query( col(034), 'T_RMO', sqlite_int )
              call sqlite3_column_query( col(035), 'T_NRET', sqlite_int )
              call sqlite3_column_query( col(036), 'T_CCSCAPA', sqlite_real )
              call sqlite3_column_query( col(037), 'T_CCSLOC', sqlite_int )
              call sqlite3_column_query( col(038), 'T_COOL', sqlite_int )
              call sqlite3_column_query( col(039), 'TSCBEF', sqlite_real )
              call sqlite3_column_query( col(040), 'TSCBYR', sqlite_int )
              call sqlite3_column_query( col(041), 'TSCBCST', sqlite_real )
              call sqlite3_column_query( col(042), 'TSCBGRP', sqlite_int )
              call sqlite3_column_query( col(043), 'TSEQEF', sqlite_real )
              call sqlite3_column_query( col(044), 'T_CF', sqlite_real )
              call sqlite3_column_query( col(045), 'M_CF_JAN', sqlite_real )
              call sqlite3_column_query( col(046), 'M_CF_FEB', sqlite_real )
              call sqlite3_column_query( col(047), 'M_CF_MAR', sqlite_real )
              call sqlite3_column_query( col(048), 'M_CF_APR', sqlite_real )
              call sqlite3_column_query( col(049), 'M_CF_MAY', sqlite_real )
              call sqlite3_column_query( col(050), 'M_CF_JUN', sqlite_real )
              call sqlite3_column_query( col(051), 'M_CF_JUL', sqlite_real )
              call sqlite3_column_query( col(052), 'M_CF_AUG', sqlite_real )
              call sqlite3_column_query( col(053), 'M_CF_SEP', sqlite_real )
              call sqlite3_column_query( col(054), 'M_CF_OCT', sqlite_real )
              call sqlite3_column_query( col(055), 'M_CF_NOV', sqlite_real )
              call sqlite3_column_query( col(056), 'M_CF_DEC', sqlite_real )
              call sqlite3_column_query( col(057), 'CFL_1', sqlite_char )
              call sqlite3_column_query( col(058), 'CFL_2', sqlite_char )
              call sqlite3_column_query( col(059), 'CFL_3', sqlite_char )
              call sqlite3_column_query( col(060), 'T_FSHR_1', sqlite_real )
              call sqlite3_column_query( col(061), 'T_FSHR_2', sqlite_real )
              call sqlite3_column_query( col(062), 'T_FSHR_3', sqlite_real )
              call sqlite3_column_query( col(063), 'T_VOM', sqlite_real )
              call sqlite3_column_query( col(064), 'T_FOM', sqlite_real )
              call sqlite3_column_query( col(065), 'T_CAPAD', sqlite_real )
              call sqlite3_column_query( col(066), 'T_GRID', sqlite_real )
              call sqlite3_column_query( col(067), 'TNOX_R', sqlite_real )
              call sqlite3_column_query( col(068), 'T_COMB', sqlite_int )
              call sqlite3_column_query( col(069), 'T_POST', sqlite_int )
              call sqlite3_column_query( col(070), 'TCOMB_O', sqlite_real )
              call sqlite3_column_query( col(071), 'TCOMB_F', sqlite_real )
              call sqlite3_column_query( col(072), 'TCOMB_V', sqlite_real )
              call sqlite3_column_query( col(073), 'TCOMB_R', sqlite_real )
              call sqlite3_column_query( col(074), 'TSNCR_O', sqlite_real )
              call sqlite3_column_query( col(075), 'TSNCR_F', sqlite_real )
              call sqlite3_column_query( col(076), 'TSNCR_V', sqlite_real )
              call sqlite3_column_query( col(077), 'TSNCR_R', sqlite_real )
              call sqlite3_column_query( col(078), 'TSCR_O', sqlite_real )
              call sqlite3_column_query( col(079), 'TSCR_F', sqlite_real )
              call sqlite3_column_query( col(080), 'TSCR_V', sqlite_real )
              call sqlite3_column_query( col(081), 'TSCR_R', sqlite_real )
              call sqlite3_column_query( col(082), 'TSCBT', sqlite_char )
              call sqlite3_column_query( col(083), 'TPART', sqlite_char )
              call sqlite3_column_query( col(084), 'T_LONG', sqlite_real )
              call sqlite3_column_query( col(085), 'T_LAT', sqlite_real )
              call sqlite3_column_query( col(086), 'T_DSIR', sqlite_real )
              call sqlite3_column_query( col(087), 'T_DSIOV', sqlite_real )
              call sqlite3_column_query( col(088), 'T_DSIF', sqlite_real )
              call sqlite3_column_query( col(089), 'T_DSIV', sqlite_real )
              call sqlite3_column_query( col(090), 'T_FFOV', sqlite_real )
              call sqlite3_column_query( col(091), 'T_FFF', sqlite_real )
              call sqlite3_column_query( col(092), 'T_FFV', sqlite_real )
              call sqlite3_column_query( col(093), 'T_ESPU', sqlite_real )
              call sqlite3_column_query( col(094), 'T_CFBU', sqlite_real )
              call sqlite3_column_query( col(095), 'T_CFBUV', sqlite_real )
              call sqlite3_column_query( col(096), 'T_CFBUF', sqlite_real )
              call sqlite3_column_query( col(097), 'T_NG_COST', sqlite_real )
              call sqlite3_column_query( col(098), 'T_NG_TRAN', sqlite_real )
              call sqlite3_column_query( col(099), 'THR_QT', sqlite_int )
              call sqlite3_column_query( col(100), 'THR_O', sqlite_real )
              call sqlite3_column_query( col(101), 'THR_F', sqlite_real )
              call sqlite3_column_query( col(102), 'THR_V', sqlite_real )
              call sqlite3_column_query( col(103), 'THR_IMP', sqlite_real )
        
!              WRITE(6,1100) "SQLITE_PREPARE_SELECT"
        
              call sqlite3_prepare_select( db, 'V_EMM_PLTF860', col, stmt)      
      
!
   10 CONTINUE
!   Write (6,*)'IEND,ISTRT at 10 continue', IEND,ISTRT
!
      W_IGRP = T_IGRP
      W_GRP = T_GRP
      WGRP_INT = MAX( WGRP_INT , W_GRP )
      W_GRP2 = T_GRP2
      W_PID = T_PID
      W_PCA = T_PCA
      IF (W_GRP .GT. 0) THEN
         IF (WNXT_SGRP(W_GRP) .LT. W_GRP2) WNXT_SGRP(W_GRP) = W_GRP2
      END IF
      W_RMO = T_RMO
      W_RYR = T_RYR
      W_SMO = T_SMO
      W_SYR = T_SYR
      WSCBGRP = TSCBGRP
      WSCBYR = TSCBYR
      WVIN = TVIN
      W_CLRG = T_CLRG
      W_CR = T_CR
      W_GR = T_GR
      W_CAR = T_CAR
      WECPT = TECPT
      WEFPT = TEFPT
      WEFDT = TEFDT
      WNOPER = TNOPER
      WNOWN = TNOWN
      WSTATE = TSTATE
      W_LONG = T_LONG
      W_LAT = T_LAT
      IF (W_GRP .GT. 0) THEN
         WSCBT = "N"
         DO IFL = 1 , N_SCBT
            IF (TSCBT .EQ. TSCBT2(IFL)) THEN
               WSCBT = TSCBT1(IFL)
            END IF
         END DO
         WPART = "N"
         DO IFL = 1 , N_PART
            IF (TPART .EQ. TPART2(IFL)) THEN
               WPART = TPART1(IFL)
            END IF
         END DO
      END IF
      WFOWN = TFOWN
      W_MRUN = T_MRUN
      IF (WFOWN .EQ. 4) W_MRUN = 9999

      
      CCS_LOC = T_CCSLOC
      W_COOL = T_COOL

      W_COMB = T_COMB
      W_POST = T_POST
      W_ACI = "N"

      WHR_QT = THR_QT
!
      DO IFL = 1 , UIFPLT
         WFL(IFL) = TFL(IFL)
      END DO
!
      WRFURB = TRFURB
!

!
      SCOUNT = TCOUNT
      SC_NP = TC_NP
      SC_SUM = TC_SUM
      SC_WIN = TC_WIN
      SHRATE = THRATE * TC_NP
      SSCBEF = TSCBEF * TC_NP
      SSCBCST = TSCBCST * TC_NP
      SSEQEF = TSEQEF * TC_NP
      S_CF = T_CF * TC_NP
      S_GRID = T_GRID * TC_NP
      S_VOM = T_VOM * TC_NP
      S_FOM = T_FOM * TC_NP
      S_GA  = T_GA  * TC_NP
      S_CAPAD = T_CAPAD * TC_NP
      SCOMB_O = TCOMB_O * TC_NP
      SCOMB_F = TCOMB_F * TC_NP
      SCOMB_V = TCOMB_V * TC_NP
      SCOMB_R = TCOMB_R * TC_NP
      SCCSROV = T_CCSROV * TC_NP
      SCCSF = T_CCSF * TC_NP
      SCCSV = T_CCSV * TC_NP
      SCCSHR = T_CCSHR * TC_NP
      SCCSCAPA = T_CCSCAPA * TC_NP
      SSNCR_O = TSNCR_O * TC_NP
      SSNCR_F = TSNCR_F * TC_NP
      SSNCR_V = TSNCR_V * TC_NP
      SSNCR_R = TSNCR_R * TC_NP
      SSCR_O = TSCR_O * TC_NP
      SSCR_F = TSCR_F * TC_NP
      SSCR_V = TSCR_V * TC_NP
      SSCR_R = TSCR_R * TC_NP
      SNOX_R = TNOX_R * TC_NP
      SDSIR = T_DSIR * TC_NP
      SDSIOV = T_DSIOV * TC_NP
      SDSIV = T_DSIV * TC_NP
      SDSIF = T_DSIF * TC_NP
      SFFOV = T_FFOV * TC_NP
      SFFF = T_FFF * TC_NP
      SFFV = T_FFV * TC_NP
      SESPU = T_ESPU * TC_NP
      SCFBU = T_CFBU * TC_NP
      SCFBUV = T_CFBUV * TC_NP
      SCFBUF = T_CFBUF * TC_NP
      S_NG_COST = T_NG_COST * TC_NP
      S_NG_TRAN = T_NG_TRAN * TC_NP
      SHR_O = THR_O * TC_NP
      SHR_F = THR_F * TC_NP
      SHR_V = THR_V * TC_NP
      SHR_IMP = THR_IMP * TC_NP
      SNUCA_O = TNUCA_O * TC_NP
      DO IFP = 1 , EFD_D_FPP
         S_FSHR(IFP) = T_FSHR(IFP) * TC_NP
      END DO
      DO MONTH = 1 , 12
         Z_CF(MONTH) = M_CF(MONTH) * TC_NP
      END DO
!

        !     LOAD SQLITE DB DATA INTO FORTRAN VARIABLES FOR NEMS
        
              I = 0
              
      DO WHILE (W_GRP .EQ. T_GRP .AND. W_GRP2 .EQ. T_GRP2 .AND.  WVIN .EQ. TVIN)       
              !do
                 I = I + 1
                 call sqlite3_next_row( stmt, col, finished )
                 if ( finished ) go to 2000 !  exit
        
                 call sqlite3_get_column( col(001), T_IGRP)
                 call sqlite3_get_column( col(002), T_GRP)
                 call sqlite3_get_column( col(003), T_GRP2)
                 call sqlite3_get_column( col(004), T_CID)
                 call sqlite3_get_column( col(005), T_PCA)
                 call sqlite3_get_column( col(006), T_PID)
                 call sqlite3_get_column( col(007), T_UID)
                 call sqlite3_get_column( col(008), T_PNM)
                 call sqlite3_get_column( col(009), TVIN)
                 call sqlite3_get_column( col(010), EFDcd)
                 call sqlite3_get_column( col(011), ECPcd)
                 call sqlite3_get_column( col(012), TFOWN)
                 call sqlite3_get_column( col(013), T_MRUN)
                 call sqlite3_get_column( col(014), TEFPt)
                 call sqlite3_get_column( col(015), TNOPER)
                 call sqlite3_get_column( col(016), TNOWN)
                 call sqlite3_get_column( col(017), T_CLRG)
                 call sqlite3_get_column( col(018), TSTATE)
                 call sqlite3_get_column( col(019), T_CR)
                 call sqlite3_get_column( col(020), T_GR)
                 call sqlite3_get_column( col(021), TCOUNT)
                 call sqlite3_get_column( col(022), TC_NP)
                 call sqlite3_get_column( col(023), TC_SUM)
                 call sqlite3_get_column( col(024), TC_WIN)
                 call sqlite3_get_column( col(025), THRATE)
                 call sqlite3_get_column( col(026), T_CCSROV)
                 call sqlite3_get_column( col(027), T_CCSF)
                 call sqlite3_get_column( col(028), T_CCSV)
                 call sqlite3_get_column( col(029), T_CCSHR)
                 call sqlite3_get_column( col(030), TRFURB)
                 call sqlite3_get_column( col(031), T_SYR)
                 call sqlite3_get_column( col(032), T_SMO)
                 call sqlite3_get_column( col(033), T_RYR)
                 call sqlite3_get_column( col(034), T_RMO)
                 call sqlite3_get_column( col(035), T_NRET)
                 call sqlite3_get_column( col(036), T_CCSCAPA)
                 call sqlite3_get_column( col(037), T_CCSLOC)
                 call sqlite3_get_column( col(038), T_COOL)
                 call sqlite3_get_column( col(039), TSCBEF)
                 call sqlite3_get_column( col(040), TSCBYR)
                 call sqlite3_get_column( col(041), TSCBCST)
                 call sqlite3_get_column( col(042), TSCBGRP)
                 call sqlite3_get_column( col(043), TSEQEF)
                 call sqlite3_get_column( col(044), T_CF)
                 call sqlite3_get_column( col(045), M_CF(1))
                 call sqlite3_get_column( col(046), M_CF(2))
                 call sqlite3_get_column( col(047), M_CF(3))
                 call sqlite3_get_column( col(048), M_CF(4))
                 call sqlite3_get_column( col(049), M_CF(5))
                 call sqlite3_get_column( col(050), M_CF(6))
                 call sqlite3_get_column( col(051), M_CF(7))
                 call sqlite3_get_column( col(052), M_CF(8))
                 call sqlite3_get_column( col(053), M_CF(9))
                 call sqlite3_get_column( col(054), M_CF(10))
                 call sqlite3_get_column( col(055), M_CF(11))
                 call sqlite3_get_column( col(056), M_CF(12))
                 call sqlite3_get_column( col(057), CFL(1))
                 call sqlite3_get_column( col(058), CFL(2))
                 call sqlite3_get_column( col(059), CFL(3))
                 call sqlite3_get_column( col(060), T_FSHR(1))
                 call sqlite3_get_column( col(061), T_FSHR(2))
                 call sqlite3_get_column( col(062), T_FSHR(3))
                 call sqlite3_get_column( col(063), T_VOM)
                 call sqlite3_get_column( col(064), T_FOM)
                 call sqlite3_get_column( col(065), T_CAPAD)
                 call sqlite3_get_column( col(066), T_GRID)
                 call sqlite3_get_column( col(067), TNOX_R)
                 call sqlite3_get_column( col(068), T_COMB)
                 call sqlite3_get_column( col(069), T_POST)
                 call sqlite3_get_column( col(070), TCOMB_O)
                 call sqlite3_get_column( col(071), TCOMB_F)
                 call sqlite3_get_column( col(072), TCOMB_V)
                 call sqlite3_get_column( col(073), TCOMB_R)
                 call sqlite3_get_column( col(074), TSNCR_O)
                 call sqlite3_get_column( col(075), TSNCR_F)
                 call sqlite3_get_column( col(076), TSNCR_V)
                 call sqlite3_get_column( col(077), TSNCR_R)
                 call sqlite3_get_column( col(078), TSCR_O)
                 call sqlite3_get_column( col(079), TSCR_F)
                 call sqlite3_get_column( col(080), TSCR_V)
                 call sqlite3_get_column( col(081), TSCR_R)
                 call sqlite3_get_column( col(082), TSCBT)
                 call sqlite3_get_column( col(083), TPART)
                 call sqlite3_get_column( col(084), T_LONG)
                 call sqlite3_get_column( col(085), T_LAT)
                 call sqlite3_get_column( col(086), T_DSIR)
                 call sqlite3_get_column( col(087), T_DSIOV)
                 call sqlite3_get_column( col(088), T_DSIF)
                 call sqlite3_get_column( col(089), T_DSIV)
                 call sqlite3_get_column( col(090), T_FFOV)
                 call sqlite3_get_column( col(091), T_FFF)
                 call sqlite3_get_column( col(092), T_FFV)
                 call sqlite3_get_column( col(093), T_ESPU)
                 call sqlite3_get_column( col(094), T_CFBU)
                 call sqlite3_get_column( col(095), T_CFBUV)
                 call sqlite3_get_column( col(096), T_CFBUF)
                 call sqlite3_get_column( col(097), T_NG_COST)
                 call sqlite3_get_column( col(098), T_NG_TRAN)
                 call sqlite3_get_column( col(099), THR_QT)
                 call sqlite3_get_column( col(100), THR_O)
                 call sqlite3_get_column( col(101), THR_F)
                 call sqlite3_get_column( col(102), THR_V)
                 call sqlite3_get_column( col(103), THR_IMP)
        

         WRITE (6,1000) T_IGRP,T_GRP,T_GRP2,T_CID, T_PCA, T_PID, T_UID, &
            T_PNM, TVIN, EFDcd, ECPcd, TFOWN, T_MRUN, TEFPT, TNOPER, &
            TNOWN, T_CLRG, TSTATE, T_CR, T_GR, TCOUNT, TC_NP, &
            TC_SUM, TC_WIN, THRATE, T_CCSROV, T_CCSF, T_CCSV, T_CCSHR, &
            TRFURB, T_SYR, T_SMO, T_RYR, T_RMO, T_NRET, T_CCSCAPA, &
            T_CCSLOC, T_COOL, TSCBEF, TSCBYR, TSCBCST, TSCBGRP, TSEQEF, &
            T_CF, (M_CF(IMO),IMO=1,12), (CFL(IFL),IFL=1,EFD_D_FPP), &
            (T_FSHR(JFL),JFL=1,EFD_D_FPP), T_VOM, T_FOM, T_CAPAD, &
            T_GRID, TNOX_R, T_COMB, T_POST, TCOMB_O, &
            TCOMB_F, TCOMB_V, TCOMB_R, TSNCR_O, TSNCR_F, TSNCR_V, &
            TSNCR_R, TSCR_O, TSCR_F, TSCR_V, TSCR_R, TSCBT, TPART, &
            T_LONG, T_LAT, T_DSIR, T_DSIOV, T_DSIF, T_DSIV, T_FFOV, &
            T_FFF, T_FFV, T_ESPU, T_CFBU, T_CFBUV, T_CFBUF, T_NG_COST, T_NG_TRAN, &
            THR_QT,THR_O,THR_F,THR_V,THR_IMP
 1000    FORMAT(I5, 1X,I5, 1X,I5, 1X,A5, 1X,A5, 1X,A5, 1X,A5, &
            1X,A20, 1X,I2, 1X,A3, 1X,A2, 1X,I1, 1X,I4, 1X,I2, 1X,I3, &
            1X,I3, 1X,I2, 1X,A2, 1X,I2, 1X,I2, 1X,F6.3, 1X,F9.3, &
            1X,F9.3, 1X,F9.3, 1X,F8.1, 1X,F7.1, 1X,F7.3, 1X,F7.3, 1X,F8.1, &
            1X,I4, 1X,I4, 1X,I2, 1X,I4, 1X,I2, 1X,I1, 1X,F7.4, &
            1X,I1, 1X,I1, 1X,F6.1, 1X,I4, 1X,F7.1, 1X,I2, 1X,F7.1, &
            1X,F6.3, 12(1X,F6.3), 3(1X,A2), &
            3(1X,F6.3), 1X,F7.3, 1X,F7.3, 1X,F7.3, &
            1X,F6.3, 1X,F6.4, 1X,I1, 1X,I1, 1X,F6.1, &
            1X,F6.2, 1X,F6.2, 1X,F6.4, 1X,F6.1, 1X,F6.2, 1X,F6.2, &
            1X,F6.4, 1X,F6.1, 1X,F6.2, 1X,F6.2, 1X,F6.4, 1X,A2, 1X,A2, &
            1X,F8.3, 1X,F8.3, 1X,F6.4, 1X,F7.1, 1X,F7.3, 1X,F7.3, 1X,F7.1, &
            1X,F7.3, 1X,F7.3, 1X,F6.1, 1X,F6.2, 1X,F6.2, 1X,F6.1, 1X,F8.3, 1X,F8.3, &
            1X,I4, 1X,F8.3, 1X,F8.1, 1X,F8.1, 1X,F8.1)                        
!
!        Adjust cost variables for new historical inflation
!
         CCSROV = CCSROV * GDPADJ2012
         CCSF   = CCSF   * GDPADJ2012
         CCSV   = CCSV   * GDPADJ2012
         TSCBCST= TSCBCST* GDPADJ2012
         T_VOM  = T_VOM  * GDPADJ2012
         T_FOM  = T_FOM  * GDPADJ2012
         T_CAPAD= T_CAPAD* GDPADJ2012
         TCOMB_O= TCOMB_O* GDPADJ2012
         TCOMB_F= TCOMB_F* GDPADJ2012
         TCOMB_V= TCOMB_V* GDPADJ2012
         TSNCR_O= TSNCR_O* GDPADJ2012
         TSNCR_F= TSNCR_F* GDPADJ2012
         TSNCR_V= TSNCR_V* GDPADJ2012
         TSCR_O = TSCR_O* GDPADJ2012
         TSCR_F = TSCR_F* GDPADJ2012
         TSCR_V = TSCR_V* GDPADJ2012
         T_DSIOV= T_DSIOV * GDPADJ2012
         T_DSIF = T_DSIF  * GDPADJ2012
         T_DSIV = T_DSIV  * GDPADJ2012
         T_FFOV = T_FFOV  * GDPADJ2012
         T_FFF  = T_FFF   * GDPADJ2012
         T_FFV  = T_FFV   * GDPADJ2012
         T_ESPU = T_ESPU  * GDPADJ2012
         T_CFBU = T_CFBU  * GDPADJ2012
         T_CFBUV= T_CFBUV * GDPADJ2012
         T_CFBUF= T_CFBUF * GDPADJ2012
!        TNUCA_O= TNUCA_O* GDPADJ2012
!        T_NG_COST= T_NG_COST * GDPADJ2012
!        T_NG_TRAN= T_NG_TRAN * GDPADJ2012
!

! test lower DSI costs
         IF (T_DSIOV .LT. 12.0) THEN
            T_DSIOV = 0.10
         ENDIF
 
!        Translate State Codes into Carbon Region
!
         DO ICAR = 1 , CO2_GRP
            DO IST = 1 , CO2_NST(ICAR)
               IF (TSTATE .EQ. CO2_ST(IST,ICAR))THEN
                  T_CAR = ICAR
                  EXIT
               END IF
            END DO
         END DO
!
!        Translate EFD and ECP codes into indexes
!
         TECPT = 0
         DO IECP = 1 , ECP_D_CAP
            IF (ECPcd .EQ. UPLNTCD(IECP)) THEN
               TECPT = IECP
               EXIT
            END IF
         END DO
         TEFDT = 0
         DO IEFD = 1 , EFD_D_CAP
            IF (EFDcd .EQ. EPPLCD(IEFD)) THEN
               TEFDT = IEFD
               EXIT
            END IF
         END DO
         IF (TECPT .EQ. 0 .OR. TEFDT .EQ. 0) THEN
            WRITE(6,2321) T_IGRP,T_GRP,T_GRP2,T_PID,T_UID,T_PNM,ECPcd,TECPT,EFDcd,TEFDT
 2321       FORMAT(1X,"ERROR_BAD_ECP_OR_EFD_Code",3(":",I5),2(":",A5),":",A20,":",A2,":",I2,":",A3,":",I2)
            TECPT = 2
            TEFDT = 31
         END IF
!
!      For CPP purposes, make CC plants online in 2014 or later "CC" not "EC" (not an existing source)
!        IF (TRFURB .GT. 2013 .AND. TECPT .EQ. WIEC) THEN
!          TECPT = WICC
!        ENDIF

!    Make planned units new technologies rather than existing so they pick up the correct build cost
         IF (TVIN .EQ. 2 .OR. TVIN .EQ. 3) THEN
            IF (TECPT .EQ. WIEC) TECPT = WICC
            IF (TECPT .EQ. WIET) TECPT = WICT
         ENDIF

!compute T_CCSHR from THRATE for all EC plants
         IF (TECPT .EQ. WIEC) THEN
           IF (T_CCSHR .GT. 0) THEN
             IF (THRATE.GT.0.0) THEN
                T_CCSHR = 1/(1-AC2*AC4*AC1*THRATE/1000000)*THRATE
             ENDIF
                WRITE(6,7392)'T_CCSHR ', T_IGRP,T_GRP,T_GRP2,T_PID,T_UID,T_PNM,ECPcd,TECPT,T_CCSHR
7392       FORMAT(1X,A25,3(":",I5),2(":",A5),":",A20,":",A2,":",I4,":",F14.4)    

            ENDIF
         ENDIF

! ensure all CC plants have CCS retrofit costs
         IF (TECPT .EQ. WIEC .OR. TECPT .EQ. WICC) THEN
            IF (T_CCSROV .EQ. 0) THEN
               T_CCSROV = 9999
               T_CCSHR = THRATE * 1.14
            ENDIF
         ENDIF
           
!
!        Translate Fuel Codes into indexes
!
         DO IFL = 1 , UIFPLT
            IF (CFL(IFL) .EQ. 'CL') THEN
               CFL(IFL) = ECPcd
            ELSE IF (CFL(IFL) .EQ. 'RS') THEN
               CFL(IFL) = 'RL'
            ELSE IF (CFL(IFL) .EQ. 'NG') THEN
               IF (EFDcd .EQ. 'CTO' .OR. EFDcd .EQ. 'CTG' .OR. EFDcd .EQ. 'CTX') THEN
                  CFL(IFL) = 'GI'
               ELSE
                  CFL(IFL) = 'GF'
               END IF
            END IF
            IF (CFL(IFL) .EQ. '00' .OR. CFL(IFL) .EQ. '0' .OR. CFL(IFL) .EQ. ' 0' .OR. CFL(IFL) .EQ. '0 ') THEN
               TFL(IFL) = 0
            ELSE
               DO JFL = 1 , EFD_D_NFL
                  IF (UFLCODE(JFL) .EQ. CFL(IFL)) THEN
                     TFL(IFL) = JFL
                     EXIT
                  END IF
               END DO
            END IF
         END DO
!
!        Scale seasonal capacity factors for all reversible hydro units
!
         IF (ECPcd .EQ. 'PS') THEN
            TMP_TOT = 0.0
            DO IMO = 1 , 12
               TMP_TOT = TMP_TOT + M_CF(IMO) / 12.0
            END DO
            IF (TMP_TOT .GT. 0.0) THEN
               DO IMO = 1 , 12
                  M_CF(IMO) = min(0.8 , M_CF(IMO) * T_CF / TMP_TOT)
               END DO
            ELSE
               DO IMO = 1 , 12
                  M_CF(IMO) = T_CF
               END DO
            END IF
         END IF
!
!        If Retire Year is beyond 2025 then Set it well beyond 2025
!
         IF (T_RYR .GT. MNUMYR + 1989) T_RYR = 9999
!
!           Identify Coal Units and Create mapping to wigrp
!
         IF (TECPT .LE. TST_FOR_CL_ECP .AND. TNOPER .LE.UNRGNS .AND. TFOWN .LE. 4) THEN
            IF (TNOX_R .LE. 0.0001) THEN
               T_COMB = 1
               T_POST = 2
            END IF
            N_COUNT = N_COUNT + 1
            CL_UNIT_TEXT(1:6) = T_PID
            CL_UNIT_TEXT(7:10) = T_UID
            I_TST = 0
            DO I_CL = 1 , NUM_CL_UNIT
               IF (CL_UNIT_TEXT .EQ. COAL_UNIT_ID(I_CL)) THEN
                  I_TST = MAP_TO_WIGRP_FRST(I_CL)
                  MAP_TO_WIGRP_NEXT(I_TST) = MAP_TO_WIGRP_FRST(I_CL)
                  MAP_TO_WIGRP_FRST(I_CL) = T_IGRP
                  I_TST = I_CL
               END IF
            END DO
            IF (I_TST .EQ. 0) THEN
               I_TST = NUM_CL_UNIT + 1
               NUM_CL_UNIT = NUM_CL_UNIT + 1
               MAP_TO_WIGRP_FRST(I_TST) = T_IGRP
               COAL_UNIT_ID(I_TST) = CL_UNIT_TEXT
            END IF
            SCAP_BY_COAL_UNIT(I_TST) = 0.0
            MAP_TO_COAL_ID(T_IGRP) = I_TST
            write(6,*) 'map to coal id ',T_IGRP,I_TST
            COAL_REGION(I_TST) = T_CLRG
!
            JSCBT = "N"
            DO IFL = 1 , N_SCBT
               IF (TSCBT .EQ. TSCBT2(IFL)) THEN
                  JSCBT = TSCBT1(IFL)
               END IF
            END DO
            JPART = "N"
            DO IFL = 1 , N_PART
               IF (TPART .EQ. TPART2(IFL)) THEN
                  JPART = TPART1(IFL)
               END IF
            END DO
            JECPT = TECPT
!
            IF (INITIAL_ECP_TYPE(I_TST) .EQ. 0) THEN
               INITIAL_ECP_TYPE(I_TST) = JECPT
            ELSE
               INITIAL_ECP_TYPE(I_TST) = MIN(INITIAL_ECP_TYPE(I_TST) , JECPT)
            END IF
!
            DO TYR = 1 , MNUMYR
               IF (T_SYR .LE. TYR + UHBSYR .AND. T_RYR .GT. TYR + UHBSYR) THEN
                  CP_BY_CL_UNIT_N_YR(TYR,I_TST) = CP_BY_CL_UNIT_N_YR(TYR,I_TST) + TC_SUM
               END IF
               SCAP_BY_COAL_UNIT(I_TST) = MAX(SCAP_BY_COAL_UNIT(I_TST) , CP_BY_CL_UNIT_N_YR(TYR,I_TST))
            END DO
            WRITE(6,3417) I_TST,N_COUNT,T_PID,T_UID,CL_UNIT_TEXT,T_IGRP,T_GRP,T_PNM,T_SYR,T_RYR,TFOWN,TC_SUM,SCAP_BY_COAL_UNIT(I_TST),JECPT,INITIAL_ECP_TYPE(I_TST)
 3417       FORMAT(1X,"COAL_UNITS",2(":",I5),2(":",A),":",A,2(":",I5),":",A20,3(":",I4),2(":",F9.3),2(":",I2))
         END IF
!
!        WRITE OUT PLANTS.NEWEST FOR VALCAP
!
         IEFD = TEFDT
         FO = WFOR(IEFD)
         WRITE(UF_MSG,5893) TNOWN,T_PNM,TEFDT,TC_SUM,T_SYR,T_RYR,FO, &
            T_GRID
 5893    FORMAT(1X,"%ART%",6X,I3,1X,A25,6X,I2,F9.3,2X,I4, &
            1X,I4,1X,F5.3,1X,F5.3)
!
         IF (TCOUNT .LE. DBLE(0.0001)) TCOUNT = 0.0
         TC_SUM = MAX(DBLE(0.001) , TC_SUM)
         TC_WIN = MAX(DBLE(0.001) , TC_WIN)
         IF (TC_NP .LT. DBLE(0.001)) TC_NP = TC_SUM
         IEFD = TEFDT
 1113    FORMAT(I2)
         JCNT = JCNT + 1
         IF (T_SYR .LT. 1000) THEN
            T_SYR = T_SYR + 1900
         END IF
         IF (T_RYR .EQ. 0) THEN
            T_RYR = 9999
         ELSE
            IF (T_RYR .LT. 1000) THEN
               T_RYR = T_RYR + 1900
            END IF
         END IF
!
         IF (T_RMO .LE. 0 .OR. T_RMO .GT. 12 ) T_RMO = 6
         IF (T_SMO .LE. 0 .OR. T_SMO .GT. 12 ) T_SMO = 6
!
         IF (TSCBEF .GT. 0.0 .AND. TSCBEF .LT. 20.0) THEN
            TSCBEF = 95.0
         END IF
!
         IF (IEFD .LE. EFD_D_DSP) THEN
            T_GA = T_FOM * GA_PLT(IEFD)
         ELSE
            T_GA = 0.0
         ENDIF
!
         IF (W_GRP .EQ. T_GRP .AND. W_GRP2 .EQ. T_GRP2 .AND. &
         WVIN .EQ. TVIN) THEN
            SCOUNT = SCOUNT + TCOUNT
            SC_NP = SC_NP + TC_NP
            SC_SUM = SC_SUM + TC_SUM
            SC_WIN = SC_WIN + TC_WIN
            SHRATE = SHRATE + THRATE * TC_NP
            SSCBEF = SSCBEF + TSCBEF * TC_NP
            SSCBCST = SSCBCST + TSCBCST * TC_NP
            SSEQEF = SSEQEF + TSEQEF * TC_NP
            S_CF = S_CF + T_CF * TC_NP
            S_GRID = S_GRID + T_GRID * TC_NP
            S_VOM = S_VOM + T_VOM * TC_NP
            S_FOM = S_FOM + T_FOM * TC_NP
            S_GA  = S_GA  + T_GA  * TC_NP
            S_CAPAD = S_CAPAD + T_CAPAD * TC_NP
            SCCSROV = SCCSROV + T_CCSROV * TC_NP
            SCCSF = SCCSF + T_CCSF * TC_NP
            SCCSV = SCCSV + T_CCSV * TC_NP
            SCCSHR = SCCSHR + T_CCSHR * TC_NP
            SCCSCAPA = SCCSCAPA + T_CCSCAPA * TC_NP
            SDSIR = SDSIR + T_DSIR * TC_NP
            SDSIOV = SDSIOV + T_DSIOV * TC_NP
            SDSIV = SDSIV + T_DSIV * TC_NP
            SDSIF = SDSIF + T_DSIF * TC_NP
            SFFOV = SFFOV + T_FFOV * TC_NP
            SFFF = SFFF + T_FFF * TC_NP
            SFFV = SFFV + T_FFV * TC_NP
            SESPU = SESPU + T_ESPU * TC_NP
            SCFBU = SCFBU + T_CFBU * TC_NP
            SCFBUV = SCFBUV + T_CFBUV * TC_NP
            SCFBUF = SCFBUF + T_CFBUF * TC_NP
            S_NG_COST = S_NG_COST + T_NG_COST * TC_NP
            S_NG_TRAN = S_NG_TRAN + T_NG_TRAN * TC_NP
            SCOMB_O = SCOMB_O + TCOMB_O * TC_NP
            SCOMB_F = SCOMB_F + TCOMB_F * TC_NP
            SCOMB_V = SCOMB_V + TCOMB_V * TC_NP
            SCOMB_R = SCOMB_R + TCOMB_R * TC_NP
            SSNCR_O = SSNCR_O + TSNCR_O * TC_NP
            SSNCR_F = SSNCR_F + TSNCR_F * TC_NP
            SSNCR_V = SSNCR_V + TSNCR_V * TC_NP
            SSNCR_R = SSNCR_R + TSNCR_R * TC_NP
            SSCR_O = SSCR_O + TSCR_O * TC_NP
            SSCR_F = SSCR_F + TSCR_F * TC_NP
            SSCR_V = SSCR_V + TSCR_V * TC_NP
            SSCR_R = SSCR_R + TSCR_R * TC_NP
            SNOX_R = SNOX_R + TNOX_R * TC_NP
            SHR_O = SHR_O + THR_O * TC_NP
            SHR_F = SHR_F + THR_F * TC_NP
            SHR_V = SHR_V + THR_V * TC_NP
            SHR_IMP = SHR_IMP + THR_IMP * TC_NP
            SNUCA_O = SNUCA_O + TNUCA_O * TC_NP
            DO IFP = 1 , EFD_D_FPP
               S_FSHR(IFP) = S_FSHR(IFP) + T_FSHR(IFP) * TC_NP
            END DO
            DO MONTH = 1 , 12
               Z_CF(MONTH) = Z_CF(MONTH) + M_CF(MONTH) * TC_NP
            END DO
         END IF
         IF ((TSCBEF .GT. 0.0) .AND. (TSCBYR .LT. 200)) THEN
            TSCBYR = TSCBYR + 1900
         END IF
!      END DO
       !enddo   
       END DO
!
 7777 CONTINUE
!   Write (6,*)'IEND,ISTRT at 7777 continue', IEND,ISTRT

      IF (ISTRT .EQ. 1) THEN
         IF (SCOUNT .GE. 0.0001) THEN
            WCOUNT = SCOUNT
         ELSE
!           WCOUNT = 0.0001
            WCOUNT = 1.000
         END IF
         WC_NP = SC_NP
         WC_SUM = SC_SUM
         WC_WIN = SC_WIN
         WHRATE = SHRATE / SC_NP
         WSCBEF = SSCBEF / SC_NP
         WSCBCST = SSCBCST / SC_NP
         WSEQEF = SSEQEF / SC_NP
         W_CF = S_CF / SC_NP
         W_GRID = S_GRID / SC_NP
         W_FOM = S_FOM / SC_NP
         W_GA  = S_GA  / SC_NP
         W_VOM = S_VOM / SC_NP
         W_CAPAD = S_CAPAD / SC_NP
         WCOMB_O = SCOMB_O / SC_NP
         WCOMB_F = SCOMB_F / SC_NP
         WCOMB_V = SCOMB_V / SC_NP
         WCOMB_R = SCOMB_R / SC_NP
         WSNCR_O = SSNCR_O / SC_NP
         WSNCR_F = SSNCR_F / SC_NP
         WSNCR_V = SSNCR_V / SC_NP
         WSNCR_R = SSNCR_R / SC_NP
         WSCR_O = SSCR_O / SC_NP
         WSCR_F = SSCR_F / SC_NP
         WSCR_V = SSCR_V / SC_NP
         WSCR_R = SSCR_R / SC_NP
         CCSROV = SCCSROV / SC_NP
         CCSF = SCCSF / SC_NP
         CCSV = SCCSV / SC_NP
         CCSHR = SCCSHR / SC_NP
         CCSCAPA = SCCSCAPA / SC_NP
         W_DSIOV = SDSIOV / SC_NP
         W_DSIR = SDSIR / SC_NP
         W_DSIV = SDSIV / SC_NP
         W_DSIF = SDSIF / SC_NP
         W_FFOV = SFFOV / SC_NP
         W_FFF = SFFF / SC_NP
         W_FFV = SFFV / SC_NP
         W_ESPU = SESPU / SC_NP
         W_CFBU = SCFBU / SC_NP
         W_CFBUV = SCFBUV / SC_NP
         W_CFBUF = SCFBUF / SC_NP
         NG_COST = S_NG_COST / SC_NP
         NG_TRAN = S_NG_TRAN / SC_NP
         WHR_O = SHR_O / SC_NP
         WHR_F = SHR_F / SC_NP
         WHR_V = SHR_V / SC_NP
         WHR_IMP = SHR_IMP / SC_NP
         WNUCA_O = SNUCA_O / SC_NP
          IF (WECPT .LE. WIIS .AND. WSCR_R .LT. 0.85) WSCR_R = 0.85
         WNOX_R = SNOX_R / SC_NP
         WNOX_B4 = WNOX_R
         IF (W_POST .EQ. 2 .OR. W_POST .EQ. 4) THEN
            WNOX_R = WNOX_R * ( 1.0 - WSCR_R )
         ELSE IF (W_POST .EQ. 1 .OR. W_POST .EQ. 3) THEN
            WNOX_R = WNOX_R * ( 1.0 - WSNCR_R )
         END IF
         TOTFSH = 0.0
         DO IFP = 1 , UIFPLT
            W_FSHR(IFP) = S_FSHR(IFP) / SC_NP
            IF (WFL(IFP) .GT. 0)  THEN
               TOTFSH = TOTFSH + W_FSHR(IFP)
            ENDIF
         END DO
         IF (TOTFSH .GT. 0.0 .AND. TOTFSH .LT. 1.0) THEN   !normalize to equal 1.0
          DO IFP = 1, UIFPLT
            W_FSHR(IFP) = W_FSHR(IFP) / TOTFSH
          ENDDO
         ENDIF

! make add'l mrun for STEO - oil steam units
         IF (WEFDT .EQ. UISTO .AND. WNOWN .NE. 3)  THEN   ! all oil steam
           IF (W_CF .GE. 0.01 .AND. W_CF .LE. 0.75) THEN
             IF (W_MRUN .EQ. 0) W_MRUN = 2020
           ENDIF
         ENDIF

! make large biomass must-run
         IF (WEFDT .EQ. UIBMS .AND. WC_SUM .GE. 20.0) THEN
            IF (W_MRUN .EQ. 0) W_MRUN = 9999
         ENDIF

! make sure coal isn't must run forever
         IF (WEFDT .LE. UICTN .AND. WFOWN .LT. 4) THEN
            IF (W_MRUN .EQ. 9999) W_MRUN = 0
         ENDIF

! coal CF min 50%
!        IF (WEFDT .LE. UICTN .AND. W_CF .LT. 0.5) W_CF = 0.5

! Make Ohio Valley Electric Coal Plants Must-run Through 2030 for Subsidy Program
         IF (WEFDT .LE. UICTN .AND. (W_PCA .EQ. 'OVEC ' .OR. W_PCA .EQ. 'OVEC ')) THEN
            W_MRUN = 2030
         END IF

         DO MONTH = 1 , 12
            WCF_M(MONTH) = Z_CF(MONTH) / SC_NP
         END DO
!
!        INITIALIZE EFP VARIABLES FOR PIPELINE BUILDS
!
         IF (USW_XP .EQ. 0) then
            CALL INITEFP
         ELSE
            WPCST = 0.0
            WASVL = 0.0
            WBCWP = 0.0
            WRCWP = 0.0
         END IF
!
!        GET REGION AND GROUP NUMBER IF UNIT PASSES THE PRESCREEN
!
         WREC_INT = WREC_INT + 1
!
!        UPDATE END OF YEAR CAPACITY
!
         RYRMO = W_RYR * 100 + W_RMO
         SYRMO = W_SYR * 100 + W_SMO
!
!        DETERMINE REPORT VINTAGE
!
         IVIN3 = WVIN
         IF (WVIN .GT. 3) IVIN3 = 1
         IF (WVIN .GT. 9) IVIN3 = 2
         ULVINT(W_GRP) = IVIN3
!
!        DETERMINE EFD PLANT TYPE
!
         IEFD = WEFDT
         ULEFDT(W_GRP) = IEFD
!
!        DETERMINE ECP PLANT TYPE
!
         IECP = WECPT
         ULECPT(W_GRP) = IECP
!
!        DETERMINE OWNERSHIP TYPE
!
         IOWN = WFOWN
         ULOWNT(W_GRP) = IOWN
!
         IRG = WNOWN
         ULORGN(W_GRP) = IRG
         OWRGN = IRG
!
         PHRGN = WNOPER
         ULOPER(W_GRP) = PHRGN
!
         IF (ULORGN(W_GRP) .EQ. ULOPER(W_GRP)) THEN
            ULRGNT(W_GRP) = 1
         ELSE
            ULRGNT(W_GRP) = 2
         END IF
!
         ULCENS(W_GRP) = W_CR
!
         ULEFPT(W_GRP) = WEFPT
!
         DO IFP = 1, UIFPLT
            FL_NUM = WFL(IFP)
            ULFUEL(IFP,W_GRP) = FL_NUM
            IF (FL_NUM .GT. 0) THEN
               IF (UCDFLRG(FL_NUM,1) .EQ. 'CR') THEN
                  RG_NUM = W_CR
               ELSE IF (UCDFLRG(FL_NUM,1) .EQ. 'GR') THEN
                  RG_NUM = W_GR
               ELSE IF (UCDFLRG(FL_NUM,1) .EQ. 'NR') THEN
                  RG_NUM = WNOWN
               ELSE IF (UCDFLRG(FL_NUM,1) .EQ. 'N2') THEN
                  RG_NUM = WNOPER
               ELSE IF (UCDFLRG(FL_NUM,1) .EQ. 'CL') THEN
                  RG_NUM = W_CLRG
               END IF
            END IF
            ULFLRG(IFP,W_GRP) = RG_NUM
         END DO
!
         DO IYR = 1 , MNUMYR
            IYRMO = 198900 + IYR * 100 + 12
!
            ULCAPC(W_GRP) = 0.0
            IF (((WVIN .EQ. 1) .AND. (RYRMO .GE. IYRMO)) .OR. &
                ((WVIN .GT. 1) .AND. (SYRMO .LE. IYRMO) .AND. &
                (RYRMO .GE. IYRMO))) THEN
               ULCAPC(W_GRP) = WC_SUM
            END IF
!
!           UPDATE CUMULATIVE RETIREMENTS
!
            ULRETC(W_GRP) = 0.0
            JTEST = 1
            IF ((WVIN .EQ. 0) .OR. &
                (WVIN .EQ. 4) .OR. &
                (WVIN .EQ. 6) .OR. &
                (WVIN .EQ. 8)) JTEST = 0
            IF ((JTEST .EQ. 1) .AND. (RYRMO .LT. IYRMO)) THEN
               ULRETC(W_GRP) = WC_SUM
            END IF
!
        END DO
 
!        WSCBCST = WSCBCST * 1.15
!        WCOMB_O = WCOMB_O * 1.15
!        WSNCR_O = WSNCR_O * 1.15
!        WSCR_O  = WSCR_O * 1.15
!
         CALL STRPLT(WREC_INT)
         WRITE(6,*)'calling STRPLT1 ',WREC_INT
!
         ICNT = ICNT + 1
         CALL EREGN(IRG)
!
         CALL EGRP(IRG,IGRP,LGRP)
!
         WRITE(*,3111) W_GRP, W_GRP2, WREC_INT, IRG, IGRP, W_CLRG, W_CR, W_GR, WVIN, WFOWN, WEFDT, WECPT, WNOWN, WSTATE, WCOUNT,  &
            WC_NP, WC_SUM, WHRATE, WRFURB, W_SYR, W_SMO, W_RMO, W_RYR, CCSROV, CCSF, CCSV, W_COMB, W_POST, WNOX_R,  &
            WSCBEF, WSCBYR, WSCBCST, WSCBGRP, WSEQEF, W_CF,  &
            (WFL(IFP), W_FSHR(IFP), IFP = 1, UIFPLT), WSCBT, WPART, W_MRUN,  &
            W_DSIR, W_DSIOV, W_DSIF, W_DSIV, W_FFOV, W_FFF, W_FFV, W_ESPU, W_CFBU, W_CFBUV, W_CFBUF, WPCST, NG_COST, NG_TRAN, GDPADJ2012, &
            WHR_QT,WHR_O,WHR_F,WHR_V,WHR_IMP
 
 3111    FORMAT("Records",":",I5,":",I5,":",I5,":",I3,":",I5,3(":",I2),":",I2,":",I2,":",I2,":",I2,":",I3,":",A2,":",F8.4, &
         ":",F9.3,":",F9.3,":",F8.1,":",I4,":",I4,":",I2,":",I2,":",I4,":",F7.1,":",F7.3,":",F7.3,":",I1,":",I1,":",F6.4, &
         ":",F5.1,":",I4,":",F6.1,":",I3,":",F5.1,":",F5.3, &
         3(":",I2,":",F8.4),2(":",A1),":",I4, &
         ":",F6.4,":",F7.1,":",F7.3,":",F7.3,":",F7.1,":",F7.3,":",F7.3,":",F6.1,":",F6.2,":",F6.2,":",F6.1,":",F8.3,":",F9.3,":",F9.3,":",F9.6,&
         ":",I2,":",F8.3,":",F8.1,":",F8.1,":",F8.1)
!
!        CREATE CHAIN BY REGION AND GROUP IF REGION AND GROUP ARE IN RANGE
!
         IF ((IRG .LT. WPLT_D_RGN) .AND. (IGRP .LT. WPLT_D_GRP)) THEN
            W_NXT(WREC_INT) = W_INT(IRG,IGRP)
            W_INT(IRG,IGRP) = WREC_INT
         ELSE
            IF (UF_DBG.GT.0) THEN
               WRITE(UF_DBG,1011) IRG,IGRP,W_GRP,W_GRP2
 1011          FORMAT(1X,4I6)
            END IF
         END IF
      ELSE
         ISTRT = 1
      END IF
!   Write (6,*)'IEND,ISTRT before goto10', IEND,ISTRT      
      IF (IEND .EQ. 0) GO TO 10
 2000 CONTINUE
!    Write (6,*)'IEND,ISTRT at after 2000 continue', IEND,ISTRT
      IF (IEND .EQ. 0) THEN
         IEND = 1
         GO TO 7777
      END IF

      WGRP_NXT = WGRP_INT
      CALL STRPLT(0)
      WRITE(6,*)'calling STRPLT0 ',WGRP_NXT 
!        
              deallocate(col)        
              call sqlite3_close( db )


      NEW=.FALSE.                                         !//PC//
      FILENM = 'MPLTM'                                 !//PC//
!      UF_FLG=FILE_MGR('C',FILENM,NEW)                    !//PC//
      RETURN
      END

!
!
!     RDPGRP
!
      SUBROUTINE RDPGRP
!
      IMPLICIT NONE
!
      INCLUDE 'parametr'
      INCLUDE 'emmparm'
      INCLUDE 'control'
      INCLUDE 'plntctl'
!
      INTEGER IRG,IGRP,IUNIT
!
!     INITIALIZE PLANT CONTROL ARRAYS
!
      DO IRG = 1 , WPLT_D_RGN
         DO IGRP = 1 , WPLT_D_GRP
            W_INT(IRG,IGRP) = 0
         END DO
         WNGRPS(IRG) = 0
         WNPIPE(IRG) = 0
      END DO
!
      DO IUNIT = 1 , WPLT_D_REC
         W_NXT(IUNIT) = 0
         WO_RYR(IUNIT) = 0
         WO_RMO(IUNIT) = 0
      END DO
!
      RETURN
      END
!
!
! RDPNEMS
!
      SUBROUTINE RDPNEMS
!
      IMPLICIT NONE
!
      INCLUDE 'parametr'
      INCLUDE 'emmparm'
      INCLUDE 'control'
      INCLUDE 'plntctl'
!
      REAL*8 TPMR,TFOR,TMXCP,TLOWER,TUPPER
      INTEGER IEFD,IFLP,I,L,IFLTP,IYR,NERC,YEAR,IREG,IPLT,FUEL
      INTEGER*4 TCFLTP(12)
      CHARACTER*132 COMMENT
      CHARACTER*80 FILENM
      CHARACTER*30 PLANT
      CHARACTER*24 TNAME
      CHARACTER*4 FUEL1,FUEL2,FUEL3
      CHARACTER*3 TCODE
      CHARACTER*2 FL_CD(12)
      LOGICAL NEW
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
!
!     OPEN THE P_NEMS FILE
!
      NEW=.FALSE.
      FILENM = 'PLTDATA'
      UF_TMP = FILE_MGR('O',FILENM,NEW)
!
!     READ IN PLANT GROUP FILE
!
      IF (UF_DBG.GT.0) THEN
         WRITE(UF_DBG, '(A)') ' GENERIC PLANT INFORMATION'
      END IF
!
      READ (UF_TMP,*) COMMENT
      WRITE(UF_DBG,*) COMMENT
!
      IFLP = UIFPLT
      DO I = 1 , UIFPLT
         TCFLTP(I) = 0
      END DO
!
      DO IPLT = 1 , EFD_D_CAP
!
         READ (UF_TMP,*) IEFD,TCODE,TNAME, &
            TPMR,TFOR,TMXCP, &
            (FL_CD(I),I=1,IFLP),(WCMFSH(IPLT,L),L=1,IFLP), &
            TLOWER,TUPPER

         DO I = 1, IFLP
           IF (FL_CD(I) .EQ. 'NA') THEN
              TCFLTP(I) = 0
           ELSE 
              DO L = 1 , EFD_D_NFL
                 IF (FL_CD(I) .EQ. UFLCODE(L)) THEN
                    TCFLTP(I) = L
                    EXIT
                 END IF
              END DO
              IF (L .GT. EFD_D_NFL) THEN
                 TCFLTP(I) = 0

                 WRITE(6,3001) IEFD, TCODE, TNAME, I, L, FL_CD(I)
 3001            FORMAT(1X,"PLTDATA_FUEL_OOPS",":",I4,":",A3,":",A24,2(":",I4),":",A2)

              END IF
           END IF
           WFLTP(IEFD,I) = TCFLTP(I)

           WRITE(6,3002) IEFD, TCODE, TNAME, I, L, FL_CD(I), WFLTP(IEFD,I)
 3002      FORMAT(1X,"PLTDATA_FUEL     ",":",I4,":",A3,":",A24,2(":",I4),":",A2,":",I4)

         END DO
!
         IF (UF_DBG.GT.0) THEN
            WRITE (UF_DBG,3010) IEFD,TCODE,TNAME, &
            TPMR,TFOR,TMXCP, &
            (TCFLTP(I),I=1,IFLP),(WCMFSH(IPLT,L),L=1,IFLP), &
            TLOWER,TUPPER
 3010       FORMAT(1X,I2,1X,A3,1X,A24,3F6.3,3(1X,I4),5F6.3)
         END IF
!
         WNAME(IEFD) = TNAME
         WPMR(IEFD) = TPMR
         WFOR(IEFD) = TFOR
         WMXCP(IEFD) = TMXCP
         WLOWER(IEFD) = TLOWER
         WUPPER(IEFD) = TUPPER
!
      END DO
!
      FILENM = 'PLTDATA'
      UF_TMP = FILE_MGR('C',FILENM,NEW)
!
      RETURN
      END
!
!
! EREGN
!
      SUBROUTINE EREGN(IRG)
!
      IMPLICIT NONE
!
      INCLUDE 'parametr'
      INCLUDE 'emmparm'
      INCLUDE 'control'
      INCLUDE 'plntctl'
      INCLUDE 'plntin'
!
      INTEGER IRG
!
!     SET REGION TO NEMS REGION BY LOCATION OF UNIT OPERATOR
!
      IRG = WNOWN
   10 FORMAT(I2)
!
!     IF IRG OUT OF RANGE SET AT MAXIMUM
!
      IF ((IRG .LE. 0) .OR. (IRG .GT. WPLT_D_RGN)) IRG = WPLT_D_RGN
!
      RETURN
      END
!
!
! EGRP
!
      SUBROUTINE EGRP(IRG,IGRP,LGRP)
!
      IMPLICIT NONE
!
      INCLUDE 'parametr'
      INCLUDE 'emmparm'
      INCLUDE 'control'
      INCLUDE 'plntctl'
      INCLUDE 'plntin'
!
      INTEGER IRG,IGRP,IEFD,LGRP
!
!     SET GROUP TO NEXT AVAILABLE IF W_GRP HAS CHANGED
!
      IF (LGRP .NE. W_GRP) THEN
         WNGRPS(IRG) = WNGRPS(IRG) + 1
         WNPIPE(IRG) = WNPIPE(IRG) + 1
         LGRP = W_GRP
         WTYPE(W_GRP) = WNGRPS(IRG)
      END IF
      IGRP = WNGRPS(IRG)
!
      IF ((IGRP .LE. 0) .OR. (IGRP .GT. WPLT_D_GRP)) IGRP = WPLT_D_GRP
!
!     CAPTURE NEMS PLANT TYPE
!                                       '
      IEFD = WEFDT
 1000 FORMAT(I2)
!
      RETURN
      END

!     THIS SUBROUTINE READS THE DATA USED TO ESTABLISH NEW BUILD COSTS
!     INCURRED PRIOR TO THE START YEAR FOR NEW BUILDS IN THE PIPELINE.
!     (THIS DATA FORMERLY CAME FROM THE BLOCK DATA IN GURFMAPS)
!
      SUBROUTINE RDMAPS
      IMPLICIT NONE
!
      INCLUDE 'parametr'
      INCLUDE 'emmparm'
      INCLUDE 'control'

      INTEGER NPLT                 ! NUMBER OF PLANT TYPES
      INTEGER NREG                 ! NUMBER OF REGIONS
      INTEGER NYR                  ! NUMBER OF YEARS
      INTEGER NOWN                 ! NUMBER OF OWNERSHIP TYPES
      INTEGER NST                  ! NUMBER OF STATES
      PARAMETER(NPLT=12,NREG=10,NYR=15,NOWN=2,NST=51)
!
      COMMON/EFPMAP/CWPPER,AFUDCR,CNSPRF,GNPD,CAPCST,XAFUDC, &
         DEFCST,LENCP,FEDREG,POSTCD
      REAL*4 CWPPER(NYR,NREG,NOWN) ! CWIP %AGES
      REAL*4 AFUDCR(NREG,NYR)      ! AFUDC RATES
      INTEGER LENCP(NREG,NPLT)     ! LENGTH OF CONSTRUCTION PERIOD
      REAL*4 CNSPRF(NPLT,NYR)      ! CONSTRUCTION PROFILES (% COSTS)
      CHARACTER*2 POSTCD(NST)      ! STATE POSTAL ABBREVIATIONS
      INTEGER FEDREG(NST)          ! FEDERAL REGIONS OF STATE
      REAL*4 GNPD(NYR)             ! GNP DEFLATOR
      REAL*4 CAPCST(NREG,NPLT)     ! CAPITAL COST PER UNIT CAPACITY
      REAL*4 XAFUDC(NPLT)          ! AFUDC IN CAPITAL COST (%)
      REAL*4 DEFCST(NPLT)          ! DEFLATION FACTORS
      CHARACTER*7 DDNAME           ! DDN NAME FOR FILE TO BE READ
      INTEGER I
      INTEGER J
      INTEGER K
      INTEGER IN                   ! DEVICE NUMBER FOR INPUT DATA READS
      LOGICAL NEW
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
!
      DDNAME = 'EFPDATA'
      NEW=.FALSE.
      IN = FILE_MGR('O',DDNAME,NEW)
!
!      READ CWIP PERCENTAGES/SHARES IN RATE BASE
!
      CALL COMENT(IN)
      DO I = 1 , NOWN                         ! LOOP ON OWNERSHIP TYPE
         DO J = 1 , NREG                      ! LOOP ON NUMBER OF REGIONS
           READ(IN,*) (CWPPER(K,J,I),K=1,NYR) ! READ FOR ALL YEARS
         END DO
      END DO
!
!     READ AFUDC RATES
!
      CALL COMENT(IN)
      DO I = 1 , NREG                         ! LOOP ON REGIONS
         READ(IN,*) (AFUDCR(I,J),J=1,NYR)     ! READ FOR ALL YEARS
      END DO
!
!     READ LENGTH OF CONSTRUCTION PERIOD
!
      CALL COMENT(IN)
      DO I = 1 , NPLT                         ! LOOP OVER PLANT TYPES
         READ(IN,*) (LENCP(J,I),J=1,NREG)     ! READ FOR ALL REGIONS
      END DO
!
!     READ CONSTRUCTION COST PERCENTAGES/SHARES IN EACH CONSTRUCTION YEAR
!     (CONSTRUCTION PROFILES)
!
      CALL COMENT(IN)
      DO I = 1 , NPLT                         ! LOOP OVER PLANT TYPES
         READ(IN,*) (CNSPRF(I,J),J=1,NYR)     ! READ FOR ALL YEARS
      END DO
!
!     READ POSTAL CODE ABBREVIATIONS BY STATE
!
      CALL COMENT(IN)
      READ(IN,*) (POSTCD(I),I=1,NST)
!
!     READ MAPPING OF POSTAL CODES TO FEDERAL REGIONS (BY NUMBER)
!
      CALL COMENT(IN)
      READ(IN,*) (FEDREG(I),I=1,NST)
!
! READ GNP DEFLATION RATE
!
      CALL COMENT(IN)
      READ(IN,*)(GNPD(I),I=1,NYR)
!
!     READ CAPITAL COSTS
!
      CALL COMENT(IN)
      DO I = 1 , NPLT                         ! LOOP OVER PLANTS
         READ(IN,*) (CAPCST(J,I),J=1,NREG)    ! READ FOR ALL REGIONS
      END DO
!
!     READ XAFUDC
!
      CALL COMENT(IN)
      READ(IN,*) (XAFUDC(I),I=1,NPLT)         ! READ FOR ALL PLANT TYPES
!
!     READ CAPITAL COST DEFLATORS
!
      CALL COMENT(IN)
      READ(IN,*) (DEFCST(I),I=1,NPLT)         ! READ FOR ALL PLANT TYPES
!
      IN=FILE_MGR('C',DDNAME,NEW)
      RETURN
      END

!     THIS SUBROUTINE COMPUTES THE ASSET VALUE, BOOKED CWIP, PLANT COST
!     AND CWIP IN RATE BASE FIELDS OF THE UNIT DATABASE
!
      SUBROUTINE INITEFP
      IMPLICIT NONE
      include'parametr'
      include'emmparm'
      include'plntin'
      include'control'
      include'bildin'
      include'ncntrl'
      include'uefdout'
      INTEGER NPLT                  ! NUMBER OF PLANTS
      INTEGER NREG                  ! NUMBER OF REGIONS
      INTEGER NYR                   ! NUMBER OF YEARS
      INTEGER NOWN                  ! NUMBER OF OWNERSHIP TYPES
      INTEGER NST                   ! NUMBER OF STATES
!      INTEGER FIRSYR                ! FIRST FORECAST YEAR
      INTEGER JREG                  ! NEMS REGION
      PARAMETER(NPLT=12,NREG=10,NYR=15,NOWN=2,NST=51)               !,FIRSYR=1990)
      COMMON/EFPMAP/CWPPER,AFUDCR,CNSPRF,GNPD,CAPCST,XAFUDC, &
                    DEFCST,LENCP,FEDREG,POSTCD
      REAL*4 CWPPER(NYR,NREG,NOWN)  ! CWIP %AGES
      REAL*4 AFUDCR(NREG,NYR)       ! AFUDC RATES
      INTEGER LENCP(NREG,NPLT)      ! LENGTH OF CONSTRUCTION PERIOD
      REAL*4 CNSPRF(NPLT,NYR)       ! CONSTRUCTION PROFILES (% COSTS)
      CHARACTER*2 POSTCD(NST)       ! STATE POSTAL ABBREVIATIONS
      INTEGER FEDREG(NST)           ! FEDERAL REGIONS OF STATE
      REAL*4 GNPD(NYR)              ! GNP DEFLATOR
      REAL*4 CAPCST(NREG,NPLT)      ! CAPITAL COST PER UNIT CAPACITY
      REAL*4 XAFUDC(NPLT)           ! AFUDC IN CAPITAL COST (%)
      REAL*4 DEFCST(NPLT)           ! DEFLATION FACTORS
!     REAL*8 ANNUITY(EFD_D_CAP)
      REAL*8 ANNUITY(ECP_D_CAP)
      REAL*8 RGN_FAC(MNUMNR),RGFAC
      REAL*8 ANNGRW,ANNFAC(80),VINADJ
      INTEGER ANNBSYR,ANNFINYR,ISYR,ANNYR
      INTEGER I,IEFD,IECP
      INTEGER IREG                  ! REGION NUMBER
      INTEGER IPLT                  ! PLANT TYPE INDEX
      INTEGER IOWN                  ! OWNERSHIP TYPE INDEX
      INTEGER LNCP                  ! NUMBER OF YEARS BEFORE START YEAR
      INTEGER L
      INTEGER L1
      INTEGER IY,YR,LYR
      REAL*4 COST                   ! PLANT COST IN $
      REAL*4 YCWP                   ! CURRENT YEAR CWIP
!
      ANNBSYR = 2015
      ANNFINYR = 1960
      ANNGRW = 0.01
      ANNFAC = 1.0

      IY = 0
      DO ANNYR = ANNBSYR, ANNFINYR, -1
        YR = (ANNYR - ANNFINYR + 1) 
        ANNFAC(YR) = (1.0 - ANNGRW) ** (IY)
        IY = IY + 1
      ENDDO

      ANNUITY(1)  = 400.00
      ANNUITY(2)  = 400.00
      ANNUITY(3)  = 400.00
      ANNUITY(4)  = 400.00
      ANNUITY(5)  = 400.00
      ANNUITY(6)  = 400.00
      ANNUITY(7)  = 400.00
      ANNUITY(8)  = 400.00
      ANNUITY(9)  = 400.00
      ANNUITY(10) = 400.00
      ANNUITY(11) = 400.00
      ANNUITY(12) = 400.00
      ANNUITY(13) = 400.00
      ANNUITY(14) = 400.00
      ANNUITY(15) = 400.00
      ANNUITY(16) = 400.00
      ANNUITY(17) = 400.00
      ANNUITY(18) = 400.00
      ANNUITY(19) = 400.00
      ANNUITY(20) = 400.00
      ANNUITY(21) = 400.00
      ANNUITY(22) = 400.00
      ANNUITY(23) = 400.00
      ANNUITY(24) = 400.00
      ANNUITY(25) = 400.00
      ANNUITY(26) = 400.00
      ANNUITY(27) = 400.00
      ANNUITY(28) = 400.00
      ANNUITY(29) = 400.00
      ANNUITY(30) = 400.00
      ANNUITY(31) = 400.00
      ANNUITY(32) = 400.00
      ANNUITY(33) = 615.00
      ANNUITY(34) = 615.00
      ANNUITY(35) = 750.00
      ANNUITY(36) = 750.00
      ANNUITY(37) = 800.00
      ANNUITY(38) = 800.00
      ANNUITY(39) = 100.00
      ANNUITY(40) = 100.00
      ANNUITY(41) = 100.00
      ANNUITY(42) = 100.00
      ANNUITY(43) = 100.00
      ANNUITY(44) = 100.00
      ANNUITY(45) = 100.00
      ANNUITY(46) =  85.00
      ANNUITY(47) = 125.00
      ANNUITY(48) = 125.00
      ANNUITY(49) = 125.00
      ANNUITY(50) = 150.00
      ANNUITY(51) = 300.00
      ANNUITY(52) = 650.00
      ANNUITY(53) = 600.00
      ANNUITY(54) = 600.00
      ANNUITY(55) = 600.00
      ANNUITY(56) = 800.00
      ANNUITY(57) = 800.00
      ANNUITY(58) = 300.00
      ANNUITY(59) = 300.00
      ANNUITY(60) = 500.00
      ANNUITY(61) = 500.00
      ANNUITY(62) = 500.00
      ANNUITY(63) = 500.00
      ANNUITY(64) = 500.00
      ANNUITY(65) = 300.00
      ANNUITY(66) = 300.00
      ANNUITY(67) = 300.00
      ANNUITY(68) = 300.00
      ANNUITY(69) = 300.00
      ANNUITY(70) =  50.00
      ANNUITY(71) =  50.00
      ANNUITY(72) = 750.00
      ANNUITY(73) = 350.00
      ANNUITY(74) = 350.00
      ANNUITY(75) = 350.00
      ANNUITY(76) = 300.00
      ANNUITY(77) = 300.00
      ANNUITY(78) = 300.00
      ANNUITY(79) = 200.00
      ANNUITY(80) = 225.00
!
     WRITE(6,*)'UNRGNS ',UNRGNS
    !IF (UNRGNS.EQ.22) THEN
    !  RGN_FAC(1)  = 1.000
    !  RGN_FAC(2)  = 1.000
    !  RGN_FAC(3)  = 0.200
    !  RGN_FAC(4)  = 1.000
    !  RGN_FAC(5)  = 1.000
    !  RGN_FAC(6)  = 1.000
    !  RGN_FAC(7)  = 1.000
    !  RGN_FAC(8)  = 1.000
    !  RGN_FAC(9)  = 1.000
    !  RGN_FAC(10) = 1.000
    !  RGN_FAC(11) = 1.000
    !  RGN_FAC(12) = 1.000
    !  RGN_FAC(13) = 0.800
    !  RGN_FAC(14) = 0.800
    !  RGN_FAC(15) = 1.000
    !  RGN_FAC(16) = 1.000
    !  RGN_FAC(17)  = 1.000          
    !  RGN_FAC(18)  = 1.000                
    !  RGN_FAC(19)  = 1.000  
    !  RGN_FAC(20)  = 1.000        
    !  RGN_FAC(21)  = 0.500        
    !  RGN_FAC(22)  = 1.000    
    !  RGN_FAC(23)  = 1.000                
    !  RGN_FAC(24)  = 1.000      
    !  RGN_FAC(25)  = 1.000  
    !ELSEIF (UNRGNS.EQ.25) THEN
    !  RGN_FAC(1)  = 1.000
    !  RGN_FAC(2)  = 1.000
    !  RGN_FAC(3)  = 0.860
    !  RGN_FAC(4)  = 0.900
    !  RGN_FAC(5)  = 1.000
    !  RGN_FAC(6)  = 0.380
    !  RGN_FAC(7)  = 1.000
    !  RGN_FAC(8)  = 1.000
    !  RGN_FAC(9)  = 1.000
    !  RGN_FAC(10) = 1.000
    !  RGN_FAC(11) = 1.000
    !  RGN_FAC(12) = 0.700
    !  RGN_FAC(13) = 1.000
    !  RGN_FAC(14) = 1.000
    !  RGN_FAC(15) = 0.780
    !  RGN_FAC(16) = 1.000
    !  RGN_FAC(17)  = 1.000          
    !  RGN_FAC(18)  = 1.000                
    !  RGN_FAC(19)  = 1.000  
    !  RGN_FAC(20)  = 1.000        
    !  RGN_FAC(21)  = 1.000        
    !  RGN_FAC(22)  = 1.000    
    !  RGN_FAC(23)  = 0.500                
    !  RGN_FAC(24)  = 0.980      
    !  RGN_FAC(25)  = 0.640    
    !  RGN_FAC(26)  = 1.000   !DGR  
    !  RGN_FAC(27)  = 1.000   !DGR  
    !  RGN_FAC(28)  = 1.000   !DGR          
    !ENDIF 
    
    RGN_FAC = 1.0    !overwrite all with 1.0 - regional factors have not been reviewd recently (and need to move to SQL DB if using)
    
      DO I=1,51
         IF (WSTATE .EQ. POSTCD(I)) THEN
            IREG = FEDREG(I)             ! CONVERT STATE CODE TO FEDERAL REGION
            GO TO 10
         END IF
      END DO
!
   10 CONTINUE
      JREG = WNOWN
      IPLT = WEFPT                       ! INTEGER PLANT TYPE INDEX
      IOWN = WFOWN                       ! INTEGER OWNERSHIP INDEX
      IEFD = WEFDT
      IECP = WECPT
      ISYR = WRFURB
      WASVL = 0.0                        ! INITIALIZE ASSET VALUE
      WBCWP = 0.0                        ! INITIALIZE BOOKED CWIP
      WRCWP = 0.0                        ! INITIALIZE CWIP IN RATE BASE

      CALL GETBLD(1,JREG)                ! GET REGIONAL FACTORS FROM ECPDAT
      RGFAC = EPRGM(IECP) * EPACM(IECP)
      IF (IECP .EQ. 45 .AND. JREG .EQ. 3) RGFAC = 0.0    !remove cost for Kewaunee

      IF (WRFURB .LT. ANNFINYR) THEN
        VINADJ = ANNFAC(1)
      ELSEIF (WRFURB .LT. ANNBSYR) THEN
        VINADJ = ANNFAC(WRFURB - ANNFINYR + 1)
      ELSE
        VINADJ = 1.0
      ENDIF

      IF ((W_SYR .LT. EFPSYR) .OR. (IOWN .GE. 3) &
         .OR. (WVIN .NE. 2)) THEN
!
!        ALL VALUES ARE ZERO -- PLANT IS EXISTING
!
         IF (IOWN .LT. 3) THEN
            WPCST = 0.0
         ELSEIF (WVIN .NE. 2) THEN
!
!           EWG'S AND NTC Annuity Cost - for existing, use regional benchmark factor
!
            WPCST = ANNUITY(IECP) * RGFAC * RGN_FAC(JREG) * VINADJ 
         ELSE
!
!           EWG's and NTC planned additions - just use annuity
!      ** this is now recalculated in udat using ecpdat costs, so this assignment is not relevant **
!
            WPCST = ANNUITY(IECP) * RGFAC * RGN_FAC(JREG) * VINADJ
         END IF
      ELSE
!
!        COMPUTE PLANT COST BASED UPON CAPITAL COST W/O AFUDC (DEFLATED)
!
         WPCST = CAPCST(IREG,IPLT) * (1.0 - XAFUDC(IPLT)) * DEFCST(IPLT)
!
!        DIFFERENCE BETWEEN FIRST FORECAST YEAR AND YEAR CONSTRUCTION
!        BEGAN.  INDICATES # OF YEARS FOR WHICH CONSTRUCTION DATA IS NOT
!        ACCOUNTED FOR AS EXISTING PLANT (E.G. START YEAR BEFORE FIRST
!        FORECAST YEAR INDICATES COSTS ARE INCLUDED IN EFP DATA SOURCE.',
!        OR WILL NOT BE COMPUTED DURING MODEL EXECUTION (E.G. COST IS
!        INCURRED DURING FORECAST HORIZON AND IS COMPUTED AS IT IS
!        INCURRED).  NOTE THAT COSTS DATA FOR EXISTING PLANT BEFORE THE
!        FIRST FORECAST YEAR (ASSUMED TO BE 1990 + HARDWIRED HERE) IS
!        PROVIDED IN A DATA FILE READ BY EFP (SEE ROUTINE ELREAD).
!
         LNCP = FIRSYR - (W_SYR - LENCP(IREG,IPLT) + 1)
         IF (LNCP .GE. 1) THEN
!
!           PLANT IS UNDER CONSTRUCTION AT START YEAR
!
            COST = WPCST*WC_NP   !CONVERT TO $ (FROM $/UNIT CAPACITY)
            DO L = 1 , LNCP
               L1 = LENCP(IREG,IPLT) - L + 1  ! CONSTR. YEAR BACKWARDS
               LYR = LNCP - L + 1             ! INDEX # OF YEARS BEFORE FIRSYR
!
!              COMPUTE THIS CONTRUCTION YEAR'S CWIP AS %
!                 OF CAPITAL COST DEFLATED
!
               YCWP = COST * CNSPRF(IPLT,L1) * GNPD(LYR)
               WASVL = WASVL + YCWP           ! ACCUMULATE CWIP IN ASSET VALUE
!
!              COMPUTE PORTION OF CWIP IN RATEBASE AS %AGE OF TOTAL WHERE
!                 THE TOTAL IS THIS CONSTRUCTION YEAR'S CWIP PLUS AMOUNT
!                 ALREADY BOOKED
!
               WRCWP = (WBCWP + YCWP)*CWPPER(LYR,IREG,IOWN)
!
!              ACCUMULATE BOOKED CWIP W/AFUDC WHERE AFUDC IS TAKEN ON THE
!                 AVERAGE OF THIS CONSTRUCTION YEAR'S CWIP (YCWP/2.0) PLUS
!                 PREVIOUSLY BOOKED CWIP (BCWP).  SUBTRACT THAT AMOUNT THAT
!                 IS NOW IN THE RATEBASE (1.0-CWPPER).
!
               WBCWP = (WBCWP + YCWP) + ((WBCWP + (YCWP / 2.0)) * &
                  AFUDCR(IREG,LYR) * (1.0 - CWPPER(LYR,IREG,IOWN)))
            END DO
         END IF
      END IF
      RETURN
      END
!
!     THIS SUBROUTINE SKIPS OVER COMMENT LINE IN DATA INPUT FILES.
!     A COMMENT BEGINS WITH AN ASTERISK (*) IN COLUMN 1
!
      SUBROUTINE COMENT(NN)
      IMPLICIT NONE
!
!     INPUT VARIABLES:
!       NN = FILE NUMBER TO BE STRIPED
!
      INTEGER NN
      CHARACTER*1 LINE,STAR
      DATA STAR/'*'/
   10 CONTINUE
      READ(NN,20) LINE
   20 FORMAT(A1)
      IF (LINE .EQ. STAR) GO TO 10
      BACKSPACE NN
      RETURN
      END
!
!     THIS FUNCTION SEARCHES THE MOREOPT UNIT IN THE JCL FOR THE
!     RUN TIME OPTION (RTO) NAME SENT IN THE FUNCTION INVOCATION,
!     AND RETURNS THE VALUE SET FOR THE RUN.  IF NOT FOUND, THE
!     FUNCTION RETURNS 9999.  THE UNIT IS REWOUND AT THE END.
!
      FUNCTION RTOVALUE(RTONAME,RTODEFVAL)
!
      IMPLICIT NONE
      INTEGER UNIT,VVALUE,RTOVALUE,FILE_MGR,RTODEFVAL
      CHARACTER FILENM*18,RTONAME*8,VNAME*8
      LOGICAL NEW,FINDRTO
      EXTERNAL FILE_MGR
      RTOVALUE = 0
      RETURN
      END

subroutine unitunopened(start,end,iunit)
implicit none
logical :: lopen
integer :: start,end,iunit
integer i
iunit=-1
do i= start,end
  iunit=i
  inquire(unit=iunit,opened=lopen)
  if(.not. lopen) then
     return
  endif
enddo
return
end
      subroutine callsys(iret,cmd)
      use dfport
      character*(*) cmd
      integer iret
      write(6,*) ' Calling system to do this:  ',cmd
      iret=system(cmd)
      if(iret.ne.0) then
        write(6,'(a,I2,a,a)') '   Command failed with return code of ',iret,':  ',cmd
      endif
      return
      end
