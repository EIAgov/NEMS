! $Header: m:/default/source/RCS/welloff.f,v 1.179 2020/10/02 16:26:36 APR Exp $

!**********************************************************************
!
!     *****************************************************************
!     *    EIA - Offshore Model                                       *
!     *****************************************************************
!     *                                                               *
!     *    A Field-level Micro-Economic Model for characterizing      *
!     *    the oil and gas resource potential in                      *
!     *    the United States Outer Continental Shelf                  *
!     *                                                               *
!     *    The model currently handles the following regions:         *
!     *    1. WESTERN & CENTRAL GULF OF MEXICO                        *
!     *    2. EASTERN   GULF OF MEXICO                                *
!     *    3. ATLANTIC OCS                                            *
!     *    4. PACIFIC OCS                                             *
!     *                                                               *
!     *****************************************************************
!
!**********************************************************************
! Debug File OGSMDEEP:
! DBG_Flag_Royalty_Relief
! DBG_Flag_Prod_By_Field
! DBG_Flag_NFW_Breakdown
! DBG_Flag_CumDis_NRR
! DBG_Flag_RecRes_by_Reg
! DBG_Flag_Associated_Gas
! DBG_Flag_Check_Prd_Hist
! DBG_Flag_Rnk_Exp_Proj_By_Year
! DBG_Flag_Recov_By_Reg
! DBG_Flag_teyCA
! DBG_Flag_nmaxfld
! DBG_Flag_Poss_Projs
! DBG_Flag_Ent_Ext_Sub
! DBG_Flag_Ent_UNRR_DNRR
! DBG_Flag_print_Results

!************************************************************************************!
!
!************************************************************************************!
SUBROUTINE OGINIT_OFF
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ncntrl'       ! nems control variables
INCLUDE 'ogsmparm'
INCLUDE 'ogsmbfw'
INCLUDE 'OGSMOFF'
INCLUDE 'DWPPARM'
INCLUDE 'ogsmout'

INTEGER*4 VALUE
INTEGER iPA, iEU, iFSC
INTEGER nextInt
INTEGER DELAYYR
REAL PRCRATIO, EXPONENT
CHARACTER TEXT3*3, TEXT8*8, TEXT12*12, TEXT14*14, TEXT20*20, TEXT25*25, TEXT30*30
LOGICAL OK, FOUND

! OPEN A FILE FOR WRITING OUTPUT DATA FOR EACH YEAR
! apr 01/30/2020 moved to beginning of init for debugging purposes
IF (PRTDBGL.EQ.1) THEN
    NEW=.TRUE.
    FNAME='OGSMDEEP'
    OFFOUT = FILE_MGR('O',FNAME,NEW)
    ! Generic header for using sort/filter in Excel
    WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))') 'a','b','c','d','e','f','g','h','i','j','k','l',&
        'm','n','o','p','q','r','s','t','u','v','w','x','y','z'
ENDIF

! OPEN INPUT FILE
NEW=.FALSE.
FNAME='WLOFF'
WLOFF = FILE_MGR('O',FNAME,NEW)

! READ EVALUATION UNIT CROSS-WALK

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
nEU=0
DO I=1,ntEU
    READ(WLOFF,*)  VALUE,  REGname(I),  PAname(I),  EUname(I),  REGID(I), &
        PAID(I),  EUID(I),  OGSMID(I)
    nEU( PAID(I)) =  nEU( PAID(I)) + 1
    PAREG(PAID(I)) = REGID(I)
    EUPA(I) = PAID(I)
    OGSMREG(PAID(I),EUID(I)) = OGSMID(I)
    nmREG(REGID(I)) = REGname(I)
    nmPA(PAID(I)) = PAname(I)
    nmEU(PAID(I),EUID(I)) = EUname(I)
ENDDO

! READ INITIAL SPECIFICATIONS

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
READ(WLOFF,*)  YRBEGFORECAST ! BEGIN PROJECTION YEAR
READ(WLOFF,*)  YRENDFORECAST ! END PROJECTION YEAR
READ(WLOFF,*)  YRBEGPRDHIS   ! BEGIN YEAR DATA OF HISTORICAL PRODUCTION
READ(WLOFF,*)  YRENDPRDHIS   ! END YEAR DATA OF HISTORICAL PRODUCTION
READ(WLOFF,*)  BOEtoMCF       ! BOE TO MCF CONVERSION (MCF/BOE)

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO IPA=1,nPA
    DO IEU=1,nEU(iPA)
        READ(WLOFF,*) TEXT12, ( FLDEXPRATE(IPA,IEU,N),N=minFSC,minFSC+numFSC-1)
    ENDDO
ENDDO

! READ INFERRED RESERVES
CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO R = 1,OFFRGN
    READ (wloff,*) (INFRSVOFF(R,K),K=1,OFFFUEL)
enddo

! ASSIGN OFFSHORE MODEL START YEAR
OFFBYR =  YRBEGFORECAST - 1989
nREPORTTIME =  yrENDFORECAST -  yrBEGFORECAST + 1
nREPORTTIME =  LASTYR - OFFBYR + 1
nTIME = nREPORTTIME + 10
DO t=1,ntime
    YEAROF(t) = YRBEGFORECAST + t - 1
ENDDO
nTime0 = yrEndPrdHis - yrBegPrdHis + 1
DO t0 = 1,nTime0
    yearOf0(t0) = yrBegPrdHis + t0 - 1
ENDDO

! READ ECONOMIC DATA

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
READ(WLOFF,*)  CSTCAP
READ(WLOFF,*)  FEDTAXRATE
READ(WLOFF,*)  OILSEVTAXRATE
READ(WLOFF,*)  GASSEVTAXRATE
READ(WLOFF,*)  OILSEVTAXPRD
READ(WLOFF,*)  GASSEVTAXPRD
READ(WLOFF,*)  EXPTANGFRC
READ(WLOFF,*)  PFTANGFRC
READ(WLOFF,*)  DEVTANGFRC
READ(WLOFF,*)  CHGOPCSTOIL
READ(WLOFF,*)  CHGDRLCSTOIL
READ(WLOFF,*)  CHGPFCSTOIL
READ(WLOFF,*)  OILPRCCSTTBL
READ(WLOFF,*)  YRCSTTBL

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO IPA=1,nPA
    DO IEU=1,nEU(iPA)
        READ(WLOFF,*) TEXT12,  STTAXRATE(iPA,iEU)
    ENDDO
ENDDO

! READ DEPRECIATION SCHEDULE

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
READ(WLOFF,*) TEXT30, ( DEPRSCH(I),I=1,8)

! READ ROYALTY RATE SCHEDULE

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO R=1,OFFRGN
    READ(WLOFF,*) TEXT12, ROYENDYR(R)
ENDDO
CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO IPA=1,nPA
    DO IEU=1,nEU(iPA)
        READ(WLOFF,*) TEXT12, ( ROYRATEU(iPA,iEU,N),N=minFSC,minFSC+numFSC-1)
        DO N=minFSC,minFSC+numFSC-1
            IF (ROYRATEU(iPA,iEU,N).LE.(0.16667).AND.OGRUNOP(21).GE.1) ROYRATEU(iPA,iEU,N) = 0.16667
        ENDDO
    ENDDO
ENDDO


CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO IPA=1,nPA
    DO IEU=1,nEU(iPA)
        READ(WLOFF,*) TEXT12, ( ROYRATED(iPA,iEU,N),N=minFSC,minFSC+numFSC-1)
        DO N=minFSC,minFSC+numFSC-1
            IF (ROYRATED(iPA,iEU,N).LE.(0.16667).AND.OGRUNOP(21).GE.1) ROYRATED(iPA,iEU,N) = 0.16667
        ENDDO
    ENDDO
ENDDO


! READ TECHNOLOGY LEVERS

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
READ(WLOFF,*) TEXT30,( LEVEXPSUCRATE(I),I=1,2)
READ(WLOFF,*) TEXT30,( LEVDELWLS(I),I=1,2)
READ(WLOFF,*) TEXT30,( LEVEXPDLY(I),I=1,2)
READ(WLOFF,*) TEXT30,( LEVDRLCST(I),I=1,2)
READ(WLOFF,*) TEXT30,( LEVOPRCST(I),I=1,2)
READ(WLOFF,*) TEXT30,( LEVPFDLY(I),I=1,2)
READ(WLOFF,*) TEXT30,( LEVPFCST(I),I=1,2)
READ(WLOFF,*) TEXT30,( LEVPRDPERF1(I),I=1,2)
READ(WLOFF,*) TEXT30,( LEVPRDPERF2(I),I=1,2)

! READ FIELD SIZE CLASS DEFINITION
CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
READ(WLOFF,*) TEXT8, ( VMIN(N), N=minFSC,minFSC+numFSC-1)
READ(WLOFF,*) TEXT8, ( VMAX(N), N=minFSC,minFSC+numFSC-1)
READ(WLOFF,*) TEXT8, ( VMEAN(N), N=minFSC,minFSC+numFSC-1)

! READ NUMBER OF UNDISCOVERED FIELDS

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
READ(WLOFF,*) ASSESS_YR
CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO IPA=1,nPA
    DO IEU=1,nEU(iPA)
        READ(WLOFF,*) TEXT12, ( UNRR(iPA,iEU,N), N=minFSC,minFSC+numFSC-1)
    ENDDO
ENDDO
! apr initialize available fields to 0
uNRRavl = 0

! READ NUMBER OF DISCOVERED/UNDEVELOPED FIELDS (ANNOUNCED DISCOVERIES)

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO I=1,nANN
    READ(WLOFF,*)  ANN_FN(I), ANN_NN(I), ANN_EU(I), ANN_OG(I), ANN_WD(I), ANN_FSC(I), &
        ANN_IPRDOIL(I), ANN_IPRDGAS(I), ANN_PKPRDOIL(I), ANN_PKPRDGAS(I), ANN_DECOIL(I), &
        ANN_DECGAS(I), ANN_FRCOIL(I), ANN_FRCGAS(I), ANN_FAC(I), ANN_YRDISC(I), ANN_PRDSTYR(I)
    !prcratio = (oit_WOP(offbyr+2,1)/oit_WOP(offbyr-1,1))**(0.5)
    !exponent = max(2,2+(14-ann_fsc(i)))
    !IF (prcratio.lt.0.90) THEN
    !    DELAYYR = offbyr+baseyr-2 + INT((ann_prdstyr(i)-(offbyr+baseyr-2))*(2-prcratio)**exponent)
    !    WRITE(6,*)  'dh5off1', ANN_NN(I), ANN_FSC(I), ANN_PRDSTYR(I), DELAYYR, prcratio
    !    ANN_PRDSTYR(I) = DELAYYR
    !ENDIF
    IF (ogrunop(17).eq.3.and.ANN_FSC(I).lt.12.and.ANN_PRDSTYR(I).GT.2009) ANN_PRDSTYR(I) = 2100
    IF (ogrunop(20).gt.0.and.ANN_FSC(I).eq.12.and.ANN_PRDSTYR(I).GT.2011) ANN_PRDSTYR(I) = ANN_PRDSTYR(I) + 1
    IF (ogrunop(20).gt.0.and.ANN_FSC(I).le.11.and.ANN_PRDSTYR(I).GT.2011) ANN_PRDSTYR(I) = 2100
    !WRITE(6,*)  'dh5out1', ANN_FN(I), ANN_NN(I), ANN_EU(I), I
ENDDO

! READ PARAMETERS ASSOCIATED WITH PRODUCTION PROFILE OF DISCOVERED/UNDEVELOPED FIELDS (ANNOUNCED DISCOVERIES)
CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
READ(WLOFF,*)  ANN_RAMPUP  ! LENGTH OF RAMP UP PERIOD (IN YEARS)
READ(WLOFF,*)  ANN_HYP	 ! HYPERBOLIC DECLINE COEFF

! READ ANNUAL HISTORICAL OIL (MB) AND GAS (MMCF) PRODUCTION

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO I=1,nPRD
    READ(WLOFF,*)  PRD_ID(2,I),  PRD_EU(2,I),  PRD_DEPTH(2,I), PRD_OG(2,I), &
        PRD_FSC(2,I),  PRD_YRDISC(2,I),  &
        PRD_FLAG(2,I), TEXT3, ( PROD(2,I,M),M= 1, nTime0), PRD_FN(2,I)

    IF (PRD_DEPTH(2,I).LE.0) PRD_DEPTH(2,I) = 1
    READ(WLOFF,*)  PRD_ID(1,I),  PRD_EU(1,I),  PRD_DEPTH(1,I), PRD_OG(1,I), &
        PRD_FSC(1,I),  PRD_YRDISC(1,I),  &
        PRD_FLAG(1,I), TEXT3, ( PROD(1,I,M),M= 1, nTime0), PRD_FN(1,I)
    IF (PRD_DEPTH(1,I).LE.0) PRD_DEPTH(1,I) = 1
    !WRITE(6,*)  'dh5out', PRD_ID(2,I),  PRD_EU(2,I),  PRD_FSC(2,I), sum(prod(2,I,1:nTime0))
ENDDO

! Count the number of discovered since assessment year amd delete from undiscovered field distribution
dunrr = 0
post1990NRR = 0
! Announced Fields
DO I = 1,nANN
    IF (ANN_YRDISC(I) .GE. ASSESS_YR .AND. ANN_YRDISC(I).LT.YRBEGFORECAST) THEN
        CALL EUnm2ID( ANN_EU(I),iEU,iPA,FOUND)
        IF (.NOT.FOUND) WRITE(OFFOUT,*) 'WARNING: Invalid Evaluation Unit Name For Discovered/undeveloped Field', I,  ANN_EU(I)
        dunrr(iPA,iEU,ANN_FSC(I)) = dunrr(iPA,iEU,ANN_FSC(I)) + 1
    ENDIF
    ! Count Discovered fields after (and including) 1990
    ! Hopefully all announced Fields are discovered after 1990, but just to make sure, apr 02/12/2020
    IF ( ANN_YRDISC(I).GE.1990) THEN
        CALL EUnm2ID( ANN_EU(I),iEU,iPA,FOUND)
        IF (.NOT.FOUND) WRITE(OFFOUT,*) 'WARNING: Invalid Evaluation Unit Name For post-1990 Producing Field', I,  ANN_EU(I)
        post1990NRR(iPA,iEU,ANN_FSC(I)) = post1990NRR(iPA,iEU,ANN_FSC(I)) + 1
    ENDIF
ENDDO
! Producing Fields
DO I = 1,nPRD
    IF (PRD_YRDISC(1,I) .GE. ASSESS_YR .AND. PRD_YRDISC(1,I).LT.YRBEGFORECAST) THEN
        CALL EUnm2ID( PRD_EU(1,I),iEU,iPA,FOUND)
        IF (.NOT.FOUND) WRITE(OFFOUT,*) 'WARNING: Invalid Evaluation Unit Name For Discovered/undeveloped Field', I,  PRD_EU(1,I)
        dunrr(iPA,iEU,PRD_FSC(1,I)) = dunrr(iPA,iEU,PRD_FSC(1,I)) + 1
    ENDIF
    ! Count Discovered fields after (and including) 1990
    IF ( PRD_YRDISC(1,I).GE.1990) THEN
        CALL EUnm2ID( PRD_EU(1,I),iEU,iPA,FOUND)
        IF (.NOT.FOUND) WRITE(OFFOUT,*) 'WARNING: Invalid Evaluation Unit Name For post-1990 Producing Field', I,  PRD_EU(1,I)
        post1990NRR(iPA,iEU,PRD_FSC(1,I)) = post1990NRR(iPA,iEU,PRD_FSC(1,I)) + 1
    ENDIF
ENDDO
! Subtraction from Undiscovered Fields
DO IPA=1,nPA
    DO IEU=1,nEU(iPA)
        DO IFSC=minFSC,minFSC+numFSC-1
            UNRR(iPA,iEU,iFSC) = UNRR(iPA,iEU,iFSC) - dUNRR(iPA,iEU,iFSC)
            IF (UNRR(iPA,iEU,iFSC).LT.0) THEN
                UNRR(iPA,iEU,iFSC) = 0
                WRITE(OFFOUT,*) 'WARNING:  TOO MANY DISCOVERED FIELDS IN', iPA, iEU, iFSC
            ENDIF
        ENDDO
        !WRITE(offout,'(*(G0.6,:,"'//achar(9)//'"))') 'UNRR', nmEU(iPA,iEU),(UNRR(iPA,iEU,N), N=minFSC,minFSC+numFSC-1) ! DBG_Flag_Ent_UNRR_DNRR (1/2)
        !WRITE(offout,'(*(G0.6,:,"'//achar(9)//'"))') 'DUNRR', nmEU(iPA,iEU),(DUNRR(iPA,iEU,N), N=minFSC,minFSC+numFSC-1) ! DBG_Flag_Ent_UNRR_DNRR (2/2)
        
    ENDDO
ENDDO

! READ CUMULATIVE GROWTH FACTOR, CUMULATIVE PRODUCTION AND PROVED RESERVES
! AS OF END-OF-YEAR 2002 (OIL: MMB, GAS: BCF)
CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO I=0,50
    READ(WLOFF,*) VALUE,  oRGCGF(I),  gRGCGF(I)
ENDDO

! READ GAS TO OIL PROPORTION, CONDENSATE YIELD
! GASFRAC: fraction
! CONDENSATE: bbl/mmcf
! GOR: Scf/bbl

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO IPA=1,nPA
    DO IEU=1,nEU(iPA)
        READ(WLOFF,*) TEXT25,  GOprop(IPA,IEU),  cndYld(IPA,IEU),  GOR(IPA,IEU)
    ENDDO
ENDDO

! READ WATER DEPTH (FEET)

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO IPA=1,nPA
    DO IEU=1,nEU(iPA)
        READ(WLOFF,*) TEXT12, ( WDPTH(IPA,IEU,N),N=minFSC,minFSC+numFSC-1)
    ENDDO
ENDDO

! READ DRILLING DEPTH (FEET)

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO IPA=1,nPA
    DO IEU=1,nEU(iPA)
        READ(WLOFF,*) TEXT12, ( DDPTH(IPA,IEU,N),N=minFSC,minFSC+numFSC-1)
    ENDDO
ENDDO

! READ DRILLING RIG AVAILABILITY CONSTRAINT

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
READ(WLOFF,*) RIGTYPWDMIN(1), RIGTYPWDMAX(1), NRIGS(1), NRIGS(3), EXPDRLDAYS(1), &
    EXPDRLDAYS(3), RIGINCRMIN(1), RIGINCRMIN(3)
READ(WLOFF,*) RIGTYPWDMIN(2), RIGTYPWDMAX(2), NRIGS(2), EXPDRLDAYS(2), RIGINCRMIN(2)
READ(WLOFF,*) RIGTYPWDMIN(4), RIGTYPWDMAX(4), NRIGS(4), EXPDRLDAYS(4), RIGINCRMIN(4)
READ(WLOFF,*) RIGTYPWDMIN(5), RIGTYPWDMAX(5), NRIGS(5), NRIGS(7), EXPDRLDAYS(5), &
    EXPDRLDAYS(7), RIGINCRMIN(5), RIGINCRMIN(7)
READ(WLOFF,*) RIGTYPWDMIN(6), RIGTYPWDMAX(6), NRIGS(6), NRIGS(8), EXPDRLDAYS(6), &
    EXPDRLDAYS(8), RIGINCRMIN(6), RIGINCRMIN(8)
CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
READ(WLOFF,*) RIGTYPWDMIN(1), RIGTYPWDMAX(1), RIGBLDRATMAX(1), RIGBLDRATMAX(3), RIGUTILTARGET(1), &
    RIGUTILTARGET(3), DRILLCAP(1), DRILLCAP(3)
READ(WLOFF,*) RIGTYPWDMIN(2), RIGTYPWDMAX(2), RIGBLDRATMAX(2), RIGUTILTARGET(2), DRILLCAP(2)
READ(WLOFF,*) RIGTYPWDMIN(4), RIGTYPWDMAX(4), RIGBLDRATMAX(4), RIGUTILTARGET(4), DRILLCAP(4)
READ(WLOFF,*) RIGTYPWDMIN(5), RIGTYPWDMAX(5), RIGBLDRATMAX(5), RIGBLDRATMAX(7), RIGUTILTARGET(5), &
    RIGUTILTARGET(7), DRILLCAP(5), DRILLCAP(7)
READ(WLOFF,*) RIGTYPWDMIN(6), RIGTYPWDMAX(6), RIGBLDRATMAX(6), RIGBLDRATMAX(8), RIGUTILTARGET(6), &
    RIGUTILTARGET(8), DRILLCAP(6), DRILLCAP(8)
RIGTYPWDMIN(3) = RIGTYPWDMIN(1)
RIGTYPWDMAX(3) = RIGTYPWDMAX(1)
RIGTYPWDMIN(7) = RIGTYPWDMIN(5)
RIGTYPWDMAX(7) = RIGTYPWDMAX(5)
RIGTYPWDMIN(8) = RIGTYPWDMIN(6)
RIGTYPWDMAX(8) = RIGTYPWDMAX(6)

! READ EXPLORATION DELAYS

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO IPA=1,nPA
    DO IEU=1,nEU(iPA)
        READ(WLOFF,*) TEXT12,  YRAVL(IPA,IEU),  EXP1STDLY(IPA,IEU),  EXP2NDDLY(IPA,IEU), BUFFERVOL(IPA,IEU)
        DO N=minFSC,minFSC+numFSC-1
            ! ADJUST RESOURCES IN THE ATLANTIC TO ACCOUNT FOR A BUFFER ZONE
            unrr(ipa,ieu,n) = nint(unrr(ipa,ieu,n)*(1.-buffervol(ipa,ieu)/100.))
            ! ADJUST RESOURCES IN THE ATLANTIC, EGOM, & PACIFIC FOR SIDE CASE
            IF (iPA.ge.3.and.iEU.ne.7.and.YRAVL(iPA,iEU).LE.IJUMPYR+1989.and.ogrunop(19).gt.0) THEN
                unrr(ipa,ieu,n) = unrr(ipa,ieu,n)*(1.*ogrunop(19))
                IF (iPA.eq.3) sc10(3,iEU) = sc10(3,iEU)/(1.*ogrunop(19))
            ENDIF
        ENDDO
    ENDDO
ENDDO
EXP1STDLYORG =  EXP1STDLY
EXP2NDDLYORG =  EXP2NDDLY

! READ EXPLORATION SUCCESS RATE

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO IPA=1,nPA
    DO IEU=1,nEU(iPA)
        READ(WLOFF,*) TEXT12, ( EXPSUCRATE(IPA,IEU,N),N=minFSC,minFSC+numFSC-1)
    ENDDO
ENDDO

! READ NUMBER OF DELINEATION WELLS TO JUSTIFY DEVELOPMENT

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO IPA=1,nPA
    DO IEU=1,nEU(iPA)
        READ(WLOFF,*) TEXT12, ( NDELWLS(IPA,IEU,N),N=minFSC,minFSC+numFSC-1)
    ENDDO
ENDDO
NDELWLSORG =  NDELWLS

! READ MAXIMUM NUMBER OF DEVELOPMENT WELLS

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO IEU=1,nmaxEU
    READ(WLOFF,*) TEXT12, ALPHA_DW(iEU), BETA_DW(iEU), DEVMIN(iEU)
ENDDO

DO IPA=1,nPA
    DO IEU=1,nEU(iPA)
        DO IFSC=minFSC,minFSC+numFSC-1
            NDEVWLS(iPA,iEU,iFSC) = MAX(devmin(ieu),nextINT(ALPHA_DW(iEU)/iFSC*VMEAN(iFSC)**BETA_DW(iEU)))
        ENDDO
        !WRITE(6,50) 'ndevwls', nmEU(iPA,iEU),(NDEVWLS(IPA,IEU,N),N=minFSC,minFSC+numFSC-1)
        !50 FORMAT(A8,2x,A15,<numFSC>I4)
    ENDDO
ENDDO

! READ PRODUCTION FACILITY TYPE

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
READ(WLOFF,*) TEXT3,TEXT3, ( nmPF(J),J=1,nPFTYP)
DO I=1,nPFWDR
    READ(WLOFF,*)  PFTYPWDPTHMIN(I),  PFTYPWDPTHMAX(I), ( PFTYPFLG(I,N),N=1,nPFTYP), ( PFOPTFLG(I,N),N=minFSC,minFSC+numFSC-1)
ENDDO
READ(WLOFF,*) TEXT20, ( PLATFORM(N),N=1,nPFTYP)

! READ NUMBER OF FIELDS IN A PROJECT/PROSPECT

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
READ(WLOFF,*)  PFMINNFLD(2),  PFMAXNFLD(2)
READ(WLOFF,*)  PFMINNFLD(3),  PFMAXNFLD(3)

! READ MAXIMUM NUMBER OF WELLS SHARING A FLOWLINE

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
READ(WLOFF,*)  PFMAXNWLS(2)
READ(WLOFF,*)  PFMAXNWLS(3)

! READ PLATFORM STURCTURE

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO I=1,5
    READ(WLOFF,*)  PFWDMIN(I), PFWDMAX(I), (PFTYPE(I,iFSC),iFSC=minFSC,minFSC+numFSC-1)
ENDDO

! READ NUMBER OF SLOTS

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO I=1,5
    READ(WLOFF,*)  PFWDMIN(I), PFWDMAX(I), (PFNSLOT(I,iFSC),iFSC=minFSC,minFSC+numFSC-1)
ENDDO

! READ PRODUCTION FACILITY DESIGN, FABRICATION, AND INSTALLATION PERIOD MATRIX (YEARS)

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
READ(WLOFF,*) TEXT12, ( PFBLDDLYWDPTH(N),N=1,NPFWD)
DO I=1,NSLTIDX
    READ(WLOFF,*)  NSLT(I), ( PFBLDDLY1(N,1,I),N=1,NPFWD)
    DO J=2,nPFTYP
        DO K=1,nPFWD
            IF ( PLATFORM(J).EQ.1)  PFBLDDLY1(K,J,I) =  PFBLDDLY1(K,1,I)
        ENDDO
    ENDDO
ENDDO
READ(WLOFF,*)
READ(WLOFF,*) TEXT12, ( PFBLDDLY1(N,7,0),N=1,NPFWD)
READ(WLOFF,*) TEXT12, ( PFBLDDLY1(N,4,0),N=1,NPFWD)
READ(WLOFF,*) TEXT12, ( PFBLDDLY1(N,6,0),N=1,NPFWD)

! READ DELAY BETWEEN PRODUCTION FACILITY CONSTRUCTION (YEARS)

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
READ(WLOFF,*) ( PFBLDDLY2(N),N=1,NPFWD)

! READ DRILLING SCHEDULE

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO I=1,NPLTDD
    IF (I.LE.NOPFWD) THEN
        READ(WLOFF,*)  DEVDRLDLY24DDPTH(I), DEVDRLDLY24(I), DEVDrlDLYOTHWDPTH(I), &
            DEVDRLDLYOTH(7,I),  DEVDRLDLYOTH(4,I),  DEVDRLDLYOTH(6,I)
    ELSE
        READ(WLOFF,*)  DEVDRLDLY24DDPTH(I), DEVDRLDLY24(I)
    ENDIF
ENDDO

! READ PRODUCTION FACILITY COST FRACTIONS

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO I=1,10
    READ(WLOFF,*) VALUE, ( PFCSTFRC(I,J),J=1,I)
    !       WRITE(6,*) 'tey1', (PFCSTFRC(I,J),J=1,I)
ENDDO

! READ PRODUCTION PERFORMANCE DATA - HYPERBOLIC DECLINE

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO IPA=1,nPA
    DO IEU=1,nEU(iPA)
        READ(WLOFF,*) TEXT12,  PRDOILFRC(IPA,IEU),  PRDOILRATEI(IPA,IEU),  &
            PRDOILDECRATEI(IPA,IEU),  PRDOILHYP(IPA,IEU), &
            PRDGASFRC(IPA,IEU),  PRDGASRATEI(IPA,IEU), &
            PRDGASDECRATEI(IPA,IEU),  PRDGASHYP(IPA,IEU)
    ENDDO
ENDDO

! READ PRODUCTION PERFORMANCE DATA FOR OIL PRODUCING FIELDS - HYPERBOLIC DECLINE

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO IPA=1,nPA
    DO IEU=1,nEU(iPA)
        READ(WLOFF,*) TEXT12,  PRDDYRRAMPUPOIL(IPA,IEU,1),  PRDDYRPEAKOIL(IPA,IEU,1), &
            PRDDOILDECRATEI(IPA,IEU,1),  PRDDOILHYP(IPA,IEU,1), &
            PRDDYRRAMPUPOIL(IPA,IEU,2),  PRDDYRPEAKOIL(IPA,IEU,2), &
            PRDDOILDECRATEI(IPA,IEU,2),  PRDDOILHYP(IPA,IEU,2)
    ENDDO
ENDDO

! READ PRODUCTION PERFORMANCE DATA FOR GAS PRODUCING FIELDS - HYPERBOLIC DECLINE

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO IPA=1,nPA
    DO IEU=1,nEU(iPA)
        READ(WLOFF,*) TEXT12,  PRDDYRRAMPUPGAS(IPA,IEU,1),  PRDDYRPEAKGAS(IPA,IEU,1), &
            PRDDGASDECRATEI(IPA,IEU,1),  PRDDGASHYP(IPA,IEU,1), &
            PRDDYRRAMPUPGAS(IPA,IEU,2),  PRDDYRPEAKGAS(IPA,IEU,2), &
            PRDDGASDECRATEI(IPA,IEU,2),  PRDDGASHYP(IPA,IEU,2)
    ENDDO
ENDDO

! READ TRANSPORTATION PROPERTIES

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO IPA=1,nPA
    DO IEU=1,nEU(iPA)
        READ(WLOFF,*) TEXT12,  TRNFLOWLINELEN(IPA,IEU),  TRNPPDIAM(IPA,IEU), &
            TRNTRFOIL(IPA,IEU),  TRNTRFGAS(IPA,IEU)
    ENDDO
ENDDO

! READ CUMULATIVE NEW FIELD WILDCATS

CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO IPA=1,nPA
    DO IEU=1,nEU(iPA)
        READ(WLOFF,*) TEXT12,  (CUMNFW(iPA,iEU,offbyr))
    ENDDO
ENDDO

! READ NUMBER OF DISCOVERIES THROUGH 2001
! 02/12/2020 apr changing to pre1990NRR, discoveries before 1990
CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO IPA=1,nPA
    DO IEU=1,nEU(iPA)
        READ(WLOFF,*) TEXT12,  (pre1990NRR(iPA,iEU,iFSC),iFSC=minFSC,minFSC+numFSC-1)
    ENDDO
ENDDO
! add cumdisc and prod fields(if >= 1990) and ann
! CALCULATE TOTAL FIELDS (DISCOVERED PLUS UNDISCOVERED)
DO iPA = 1,nPA
    DO iEU = 1,nEU(iPA)
        DO iFSC = minFSC,minFSC+numFSC-1
            NRR(iPA,iEU,iFSC) = pre1990NRR(iPA,iEU,iFSC) + post1990NRR(iPA,iEU,iFSC) + unrr(iPA,iEU,iFSC)
            cumdisc(iPA,iEU,iFSC) = pre1990NRR(iPA,iEU,iFSC) + post1990NRR(iPA,iEU,iFSC)
        ENDDO
        WRITE(offout,'(*(G0.6,:,"'//achar(9)//'"))') 'CumDisc',nmEU(iPA,iEU),(cumdisc(iPA,iEU,iFSC),iFSC=minFSC,minFSC+numFSC-1) ! DBG_Flag_Tot_NRR (1/2)
        WRITE(offout,'(*(G0.6,:,"'//achar(9)//'"))') 'NRR',nmEU(iPA,iEU),(NRR(iPA,iEU,iFSC),iFSC=minFSC,minFSC+numFSC-1) ! DBG_Flag_Tot_NRR (2/2)
        
!==========================> HSM Code Start <==========================
    if (hsm_offshore_bool) then
        WRITE(hsm_offshore_disc,'(*(G0.16,:,","))') 'pre1990NRR', nmEU(iPA,iEU), 'setup', (pre1990NRR(iPA,iEU,iFSC),iFSC=minFSC,minFSC+numFSC-1) 
        WRITE(hsm_offshore_disc,'(*(G0.16,:,","))') 'post1990NRR', nmEU(iPA,iEU), 'setup', (post1990NRR(iPA,iEU,iFSC),iFSC=minFSC,minFSC+numFSC-1) 
        WRITE(hsm_offshore_disc,'(*(G0.16,:,","))') 'CumDisc', nmEU(iPA,iEU), 'setup', (cumdisc(iPA,iEU,iFSC),iFSC=minFSC,minFSC+numFSC-1) 
        WRITE(hsm_offshore_disc,'(*(G0.16,:,","))') 'unrr', nmEU(iPA,iEU), 'setup', (unrr(iPA,iEU,iFSC),iFSC=minFSC,minFSC+numFSC-1) 
        WRITE(hsm_offshore_disc,'(*(G0.16,:,","))') 'total_fields', nmEU(iPA,iEU), 'setup', (NRR(iPA,iEU,iFSC),iFSC=minFSC,minFSC+numFSC-1)
    endif
!===========================> HSM Code End <===========================
        
    ENDDO
ENDDO
! READ SEARCH COEFFICIENT PARAMETERS
CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
READ(WLOFF,*) TEXT12,  ALPHA_SC(1), ALPHA_SC(2), ALPHA_SC(3)
READ(WLOFF,*) TEXT12,  BETA1_SC(1), BETA1_SC(2), BETA1_SC(3)
READ(WLOFF,*) TEXT12,  BETA2_SC(1), BETA2_SC(2), BETA2_SC(3)

! READ SEARCH COEFFICIENT FOR FSC 10
CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO IEU=1,nMaxEU
    READ(WLOFF,*) TEXT12,  (SC10(m,iEU), m=1,3)
ENDDO

! READ NFW COEFFICIENTS
CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO IEU=1,nMaxEU
    READ(WLOFF,*) TEXT12,  ((alpha1(m,iEU),alpha2(m,iEU),beta(m,iEU),nlagyrs(m,iEU)), m=1,3)
ENDDO

! READ EXPLORATORY DRILLING COST COEFFICIENTS
CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO m=1,3
    READ(WLOFF,*) TEXT20, alpha_EDC(m), beta1_EDC(m), beta2_EDC(m), beta3_EDC(m), power_EDC(m)
ENDDO

! READ DEVELOPMENT DRILLING COST COEFFICIENTS
CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO m=1,2
    READ(WLOFF,*) TEXT20, alpha_DDC(m), beta1_DDC(m), beta2_DDC(m), GAMMA1_DDC(m), GAMMA2_DDC(m)
ENDDO

! READ COMPLETION COST COEFFICIENTS
CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO m=1,2
    READ(WLOFF,*) TEXT20, (COMPLCOST(m,i),i=1,3)
ENDDO

! READ OPERATING COST COEFFICIENTS
CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
READ(WLOFF,*) TEXT20, ALPHA_OC, BETA1_OC, BETA2_OC

! READ ABANDONMENT FACTOR
CALL OG_NEXTDATA(WLOFF)    ! READ UNTIL @ IN 1ST COLUMN
DO m=1,nPFTyp
    READ(WLOFF,*) TEXT20, ABANDONFAC(m)
ENDDO

! CLOSE INPUT FILE
WLOFF = FILE_MGR('C',FNAME,NEW)

! INITIALIZE DATABASE ELEMENTS WITH DEFAULT VALUES
CALL DATAINITIALIZATIONANDDEFAULTS


END SUBROUTINE OGINIT_OFF

!************************************************************************************!
! LOOPS THRU DATA FILE UNTIL NEXT RECORD TO BE READ IS FOUND
!************************************************************************************!
SUBROUTINE OG_NEXTDATA(UNITNUM)
IMPLICIT NONE

INTEGER*4, INTENT(IN) :: UNITNUM ! UNIT NUMBER
CHARACTER*1 CH ! BEGINNING OF DATA FLAG

! READ UNTIL @ IN 1ST COLUMN

CH = 'A'
DO WHILE (CH .NE. '@')
    READ(UNITNUM,10) CH
ENDDO
10 FORMAT(A1)

END SUBROUTINE OG_NEXTDATA

!************************************************************************************!
!
!************************************************************************************!
SUBROUTINE INITHISTORY
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ncntrl'       ! nems control variables
INCLUDE 'ogsmparm'
INCLUDE 'ogsmbfw'
INCLUDE 'OGSMOFF'
INCLUDE 'DWPPARM'

REAL*4 OILPRD(nTime0)
REAL*4 GASPRD(nTime0)
REAL*4 ADGPRD(nTime0)

OILPRD = 0.0
GASPRD = 0.0
ADGPRD = 0.0
DO N= 1, nTIME0
    DO I=1, nPRD
        OILPRD(N) = OILPRD(N) +  PROD(1,I,N)/365000.0
        IF ( PRD_OG(2,I).EQ."O") THEN
            ADGPRD(N) = ADGPRD(N) +  PROD(2,I,N)/1000000.0
        ELSE
            GASPRD(N) = GASPRD(N) +  PROD(2,I,N)/1000000.0
        ENDIF
    ENDDO
    !WRITE(OFFOUT,*) 'CHECKPRD', n+YRBEGPRDHIS-1,oilprd(n),adgprd(n),gasprd(n) ! DBG_Flag_Check_Prd_Hist
ENDDO

END SUBROUTINE INITHISTORY

!************************************************************************************!
! MAIN PROGRAM:
!
! Offshore Oil and Gas Supply Submodule
! National Energy Modeling System (NEMS)
! Office of Integrated Analysis and Forecasting (OIAF)
! Energy Information Administration (EIA)
!
! Developed by ICF Consulting, June 2003
!************************************************************************************!
SUBROUTINE OGMAIN_OFF
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ncntrl'       ! nems control variables
INCLUDE 'ogsmparm'
INCLUDE 'ogsmbfw'
INCLUDE 'ogsmout'
INCLUDE 'macout'
INCLUDE 'OGSMOFF'
INCLUDE 'DWPPARM'
INCLUDE 'pmmout' ! apr 02/20/2020 for dcrdwhp
INCLUDE 'ngtdmrep' ! apr 02/20/2020 for ogwprng
! apr 02/20/2020 mc_jpgdp used from macout

INTEGER DEVDRILLPERYEAR
INTEGER PFDELAY1, PFDELAY2
INTEGER iFSC, iEU, iPA, iFLD, UNRR_CUR, UNRR_LAG
INTEGER uNRR0(nPA,nMaxEU,minFSC:minFSC+numFSC-1)

!  Year loop
toff = curiyr - offbyr + 1

!  SAVE ORIGINAL FIELD SIZE DISTRIBUTION
uNRR0 = uNRR   ! dimensioned by pa, eu, fsc

!  CONVERT OIL AND GAS PRICES TO THE COSTS YEAR DOLLARS
DO I = 1,OFFRGN
    OILPRICE(I,toff) = OGPRCOFF(I,1,30)*MC_JPGDP( yrCstTbl-BASEYR+1)/MC_JPGDP(-2)
    GASPRICE(I,toff) = OGPRCOFF(I,2,30)*MC_JPGDP( yrCstTbl-BASEYR+1)/MC_JPGDP(-2)
ENDDO

!     DETERMINE IF ROYALTY RELIEF APPLIES
royrate = royrateU
DO j = 1,ntEU
    I = OGSMID(J)
    IF (I.eq.3.OR.I.eq.4) THEN
        IF (curcalyr.gt.ROYENDYR(i).or.oilprice(i,toff).GT.32.*mc_jpgdp(yrCstTbl-BASEYR+1)/mc_jpgdp(2002-BASEYR+1).or.  &
            gasprice(i,toff).GT.4.0*mc_jpgdp(yrCstTbl-BASEYR+1)/mc_jpgdp(2002-BASEYR+1)) THEN
        DO iFSC = minFSC,minFSC+numFSC-1
            royrate(PAID(J),EUID(j),iFSC) = royRateD(PAID(J),EUID(j),iFSC)
        ENDDO
        WRITE(offout,*) 'Royalty relief not applied in', yearof(toff), 'in EU', j !DBG_Flag_Royalty_Relief
        ENDIF
    ENDIF
ENDDO


IF (toff.eq.1) THEN

    IF (ogrunop(2).eq.11) THEN   ! FE TECHNOLOGY - W/O DOE R&D
        LEVEXPSUCRATE(1) = LEVEXPSUCRATE(1) - FETECH_CONV(7,1)*LEVEXPSUCRATE(2)
        LEVEXPDLY(1) = LEVEXPDLY(1) - FETECH_CONV(8,1)*LEVEXPDLY(2)
        LEVDRLCST(1) = LEVDRLCST(1) - FETECH_CONV(9,1)*LEVDRLCST(2)
        LEVOPRCST(1) = LEVOPRCST(1) - FETECH_CONV(10,1)*LEVOPRCST(2)
        LEVPFDLY(1) = LEVPFDLY(1) - FETECH_CONV(11,1)*LEVPFDLY(2)
        LEVPFCST(1) = LEVPFCST(1) - FETECH_CONV(12,1)*LEVPFCST(2)
        LEVPRDPERF1(1) = LEVPRDPERF1(1) - FETECH_CONV(13,1)*LEVPRDPERF1(2)
        LEVPRDPERF2(1) = LEVPRDPERF2(1) - FETECH_CONV(14,1)*LEVPRDPERF2(2)
    ENDIF
    IF (ogrunop(2).eq.12) THEN   ! FE TECHNOLOGY - W/ DOE R&D
        LEVEXPSUCRATE(1) = LEVEXPSUCRATE(1) - FETECH_CONV(7,1)*LEVEXPSUCRATE(2)*FEADJ_CONV(2)
        LEVEXPDLY(1) = LEVEXPDLY(1) - FETECH_CONV(8,1)*LEVEXPDLY(2)*FEADJ_CONV(2)
        LEVDRLCST(1) = LEVDRLCST(1) - FETECH_CONV(9,1)*LEVDRLCST(2)*FEADJ_CONV(2)
        LEVOPRCST(1) = LEVOPRCST(1) - FETECH_CONV(10,1)*LEVOPRCST(2)*FEADJ_CONV(2)
        LEVPFDLY(1) = LEVPFDLY(1) - FETECH_CONV(11,1)*LEVPFDLY(2)*FEADJ_CONV(2)
        LEVPFCST(1) = LEVPFCST(1) - FETECH_CONV(12,1)*LEVPFCST(2)*FEADJ_CONV(2)
        LEVPRDPERF1(1) = LEVPRDPERF1(1) - FETECH_CONV(13,1)*LEVPRDPERF1(2)*FEADJ_CONV(2)
        LEVPRDPERF2(1) = LEVPRDPERF2(1) - FETECH_CONV(14,1)*LEVPRDPERF2(2)*FEADJ_CONV(2)
    ENDIF
    ! ADJUST TECHNOLOGY LEVERS FOR ALTERNATE TECHNOLOGY CASES
    LEVEXPSUCRATE(1) = LEVEXPSUCRATE(1) * TECHADJ_OFF(1,1)
    LEVDELWLS(1) = LEVDELWLS(1) * TECHADJ_OFF(1,1)
    LEVEXPDLY(1) = LEVEXPDLY(1) * TECHADJ_OFF(1,1)
    LEVDRLCST(1) = LEVDRLCST(1) * TECHADJ_OFF(1,2)
    LEVOPRCST(1) = LEVOPRCST(1) * TECHADJ_OFF(1,2)
    LEVPFDLY(1) = LEVPFDLY(1) * TECHADJ_OFF(1,2)
    LEVPFCST(1) = LEVPFCST(1) * TECHADJ_OFF(1,2)
    LEVPRDPERF1(1) = LEVPRDPERF1(1) * TECHADJ_OFF(1,3)
    LEVPRDPERF2(1) = LEVPRDPERF2(1) * TECHADJ_OFF(1,3)

    ! DETERMINE IF ANNOUNCED DISCOVERIES WHOSE START YEAR IS AFTER THE LAST HISTORICAL YEAR ARE ECONOMIC
    ! AND DEVELOP/PRODUCE FROM THOSE THAT ARE ECONOMIC
    CALL AnnouncedDiscoveryEcon

    !  GENERATE PRODUCTION PROFILES FOR DISCOVERED-PRODUCING FIELDS
    CALL DiscoveredProducingFieldProductionProfile

    !  INITIALIZE HISTORICAL ARRAYS
    CALL INITHISTORY
ELSE

    !  Increase undiscovered resources in the EGOM, Pacific, and Atlantic over time
    !IFLD = NTFLD
    !DO iPA = 1,nPA
    !    DO iEU = 1,nEU(iPA)
    !        IF (iPA.ge.3.and.iEU.ne.7.and.YRAVL(iPA,iEU).LE.IJUMPYR+1989) THEN
    !            if (iPA.eq.3) sc10(3,iEU) = sc10(3,iEU)/(1.+ogrunop(19)/100.)
    !            DO iFSC = minFSC,minFSC+numFSC-1
    !                UNRR_CUR = nint(UNRR0(iPA,iEU,iFSC)*(1.+ogrunop(19)/100.)**(toff-1))
    !                UNRR_LAG = nint(UNRR0(iPA,iEU,iFSC)*(1.+ogrunop(19)/100.)**(toff-2))
    !                IF ( UNRR_CUR-UNRR_LAG.GT.0) THEN
    !                    UNRR(iPA,iEU,iFSC) = UNRR(iPA,iEU,iFSC)+(UNRR_CUR-UNRR_LAG)
    !                    DO M = 1, UNRR_CUR-UNRR_LAG
    !                        IFLD = IFLD + 1
    !                        FLD(IFLD)%PA = iPA
    !                        FLD(IFLD)%EU = iEU
    !                        FLD(IFLD)%nmEU = nmEU(iPA, iEU)
    !                        FLD(IFLD)%FSC = iFSC
    !                        FLD(IFLD)%INITTYP = 0
    !                        FLD(IFLD)%CURTYP = 0
    !                        FLD(IFLD)%oilKR = vmean(iFSC) * 1000000. * (1.-GOProp(iPA,iEU))
    !                        FLD(IFLD)%gasKR = vmean(iFSC) * 1000000. * GOProp(iPA,iEU) * BOEtoMcf
    !                        FLD(IFLD)%REG =  PAREG(FLD(IFLD)%PA)
    !                        FLD(IFLD)%OGSM =  OGSMREG(FLD(IFLD)%PA,FLD(IFLD)%EU)
    !                        FLD(IFLD)%nmEU = nmEU(FLD(IFLD)%PA,FLD(IFLD)%EU)
    !                        IF (FLD(IFLD)%WDPTH.LT.0.1) FLD(I)%WDPTH =  WDPTH(FLD(IFLD)%PA,FLD(IFLD)%EU,FLD(IFLD)%FSC)
    !                        IF (FLD(IFLD)%DDPTH.LT.0.1) FLD(I)%DDPTH =  DDPTH(FLD(IFLD)%PA,FLD(IFLD)%EU,FLD(IFLD)%FSC)
    !                        IF (FLD(IFLD)%WDPTH.GT.90000.) FLD(I)%WDPTH =  WDPTH(FLD(IFLD)%PA,FLD(IFLD)%EU,FLD(IFLD)%FSC)
    !                        IF (FLD(IFLD)%DDPTH.GT.90000.) FLD(I)%DDPTH =  DDPTH(FLD(IFLD)%PA,FLD(IFLD)%EU,FLD(IFLD)%FSC)
    !
    !                        IF (FLD(IFLD)%NDEVWLS.LE.0) FLD(I)%NDEVWLS =  NDEVWLS(FLD(IFLD)%PA,FLD(IFLD)%EU,FLD(IFLD)%FSC)
    !
    !                        r_unresoff(ogsmreg(iPA,iEU),1) = r_unresoff(ogsmreg(iPA,iEU),1) + fld(ifld)%oilKR/1000000.
    !                        r_unresoff(ogsmreg(iPA,iEU),2) = r_unresoff(ogsmreg(iPA,iEU),2) + fld(ifld)%gasKR/1000000.
    !                    ENDDO
    !                ENDIF
    !            ENDDO
    !        ENDIF
    !    ENDDO
    !ENDDO
    !NTFLD = IFLD

    ! Initialize exploration drilling capacity for the year
    CALL BOYExplorationDrillingRigsCapacity

    ! Create possible exploration projects and perform exploration
    ! and development economic calculations
    CALL DetermineEconomicResources
    CALL DeterminePossibleExplorationProjects

    IF (nPsbExpPrj .GT. 0) THEN

        ! Rank possible exploration projects by project type and by exploration NPV
        CALL RankExplorationProjects

        ! Perform exploration/delineation and move economic exploration
        ! project to development project subject to rig constraint
        CALL PerformExplorationAndDelineation

    ENDIF

    ! Build new exploration drilling rigs if necessary =.GT. for reporting
    CALL EOYExplorationDrillingRigsCapacity

    ! Re-assign field ID array after movements of fields from different category
    CALL AssignCurrentFieldID
ENDIF

! Assign OGSM variables
CALL OGreporttoogsm

! Print out results for all years
CALL printResults ! DBG_Flag_print_Results
!WRITE(offout,'(*(G0.6,:,"'//achar(9)//'"))') 'owhps',(dcrdwhp(mnumor,i),i=1,mnumyr)
!WRITE(offout,'(*(G0.6,:,"'//achar(9)//'"))') 'gwhps',(ogwprng(mnumor,i),i=1,mnumyr)
!WRITE(offout,'(*(G0.6,:,"'//achar(9)//'"))') 'mc_jpgdp',(mc_jpgdp(i),i=-2,mnumyr)


IF (CURIYR.EQ.LASTYR .AND. NCRL .EQ. 1) THEN
    FNAME='OGSMDEEP'
    OFFOUT = FILE_MGR('C',FNAME,NEW)
ENDIF
    
!==========================> HSM Code Start <==========================
IF (CURIYR.EQ.LASTYR) then
    DO IPA=1,nPA
        DO IEU=1,nEU(iPA)
            if (hsm_offshore_bool) WRITE(hsm_offshore_cumnfw,'(*(G0.16,:,","))') nmEU(iPA,iEU), (CUMNFW(iPA,iEU,i), i=1,MNUMYR)
        ENDDO
    ENDDO
endif
!===========================> HSM Code End <===========================

END SUBROUTINE OGMAIN_OFF

!************************************************************************************!
!
!************************************************************************************!
SUBROUTINE OGHIST_OFF(IYR)
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ncntrl'       ! nems control variables
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmbfw'      ! ogsm system variables
INCLUDE 'ogsmoff'      ! offshore variables
INCLUDE 'dwpparm'
INCLUDE 'ogsmout'

! subroutine variables
INTEGER, INTENT(IN) ::  iyr
INTEGER ireg

! IF (curiyr.le.offnhyr) then
DO ireg = 1, offrgn
    WELLSOFF(1,ireg,1)=HISTWELOFF(1,ireg,1,iyr)
    WELLSOFF(2,ireg,1)=HISTWELOFF(2,ireg,1,iyr)
    WELLSOFF(1,ireg,2)=HISTWELOFF(1,ireg,2,iyr)
    WELLSOFF(2,ireg,2)=HISTWELOFF(2,ireg,2,iyr)
    SUCWELLOFF(1,ireg,1)=ANINT(HISTSROFF(1,ireg,1,iyr)*HISTWELOFF(1,ireg,1,iyr))
    SUCWELLOFF(2,ireg,1)=ANINT(HISTSROFF(2,ireg,1,iyr)*HISTWELOFF(2,ireg,1,iyr))
    SUCWELLOFF(1,ireg,2)=ANINT(HISTSROFF(1,ireg,2,iyr)*HISTWELOFF(1,ireg,2,iyr))
    SUCWELLOFF(2,ireg,2)=ANINT(HISTSROFF(2,ireg,2,iyr)*HISTWELOFF(2,ireg,2,iyr))
    DRYWELLOFF(1,ireg,1)=HISTWELOFF(1,ireg,1,iyr) - SUCWELLOFF(1,ireg,1)
    DRYWELLOFF(2,ireg,1)=HISTWELOFF(2,ireg,1,iyr) - SUCWELLOFF(2,ireg,1)
    DRYWELLOFF(1,ireg,2)=HISTWELOFF(1,ireg,2,iyr) - SUCWELLOFF(1,ireg,2)
    DRYWELLOFF(2,ireg,2)=HISTWELOFF(2,ireg,2,iyr) - SUCWELLOFF(2,ireg,2)
    NFWOFF(ireg,1) = histnfwoff(ireg,1,iyr)
    NFWOFF(ireg,2) = histnfwoff(ireg,2,iyr)
    OEXPOFF(ireg,1) = SUCWELLOFF(1,ireg,1) - NFWOFF(ireg,1)
    OEXPOFF(ireg,2) = SUCWELLOFF(1,ireg,2) - NFWOFF(ireg,2)
    DEVOFF(ireg,1) = SUCWELLOFF(2,ireg,1)
    DEVOFF(ireg,2) = SUCWELLOFF(2,ireg,2)
    CURRESOFF(ireg,1)=HISTRESOFF(ireg,1,iyr)
    CURRESOFF(ireg,2)=HISTRESOFF(ireg,2,iyr)
    PRRATOFF(ireg,1)=HISTPRROFF(ireg,1,iyr)
    PRRATOFF(ireg,2)=HISTPRROFF(ireg,2,iyr)
    NRDOFF(ireg,1) = histnrdoff(ireg,1,iyr)
    NRDOFF(ireg,2) = histnrdoff(ireg,2,iyr)
    NDIROFF(ireg,1) = histnrdoff(ireg,1,iyr)*(oRGCGF(50)-1)
    NDIROFF(ireg,2) = histnrdoff(ireg,2,iyr)*(gRGCGF(50)-1)
    EXTOFF(ireg,1) = histextoff(ireg,1,iyr)
    EXTOFF(ireg,2) = histextoff(ireg,2,iyr)
    REVOFF(ireg,1) = histREVoff(ireg,1,iyr)
    REVOFF(ireg,2) = histREVoff(ireg,2,iyr)
    IF (iyr.eq.1998-1989) THEN      ! INITIAL VOLUMES OF INFERRED ARE AS OF 1/1/99
        r_infrsvoff(ireg,1) = infrsvoff(ireg,1)
        r_infrsvoff(ireg,2) = infrsvoff(ireg,2)
    ELSEIF (iyr.gt.1998-1989) THEN
        r_infrsvoff(ireg,1) = r_infrsvoff(ireg,1) + NDIROFF(ireg,1) - EXTOFF(ireg,1) - REVOFF(ireg,1)
        r_infrsvoff(ireg,2) = r_infrsvoff(ireg,2) + NDIROFF(ireg,2) - EXTOFF(ireg,2) - REVOFF(ireg,2)
    ENDIF
    IF (iyr.gt.2001-1989) THEN
        r_unresoff(ireg,1) = r_unresoff(ireg,1) - (NRDOFF(ireg,1)+NDIROFF(ireg,1))
        r_unresoff(ireg,2) = r_unresoff(ireg,2) - (NRDOFF(ireg,2)+NDIROFF(ireg,2))
    ENDIF
ENDDO
OGEOYURR(L48RGN+1,1,IYR) = r_unresoff(1,1) * 0.001
OGEOYURR(L48RGN+2,1,IYR) = (r_unresoff(3,1) + r_unresoff(4,1) + r_unresoff(5,1)) * 0.001
OGEOYURR(L48RGN+3,1,IYR) = r_unresoff(2,1) * 0.001
OGEOYURR(L48RGN+1,2,IYR) = r_unresoff(1,2) * 0.001
OGEOYURR(L48RGN+2,2,IYR) = (r_unresoff(3,2) + r_unresoff(4,2) + r_unresoff(5,2)) * 0.001
OGEOYURR(L48RGN+3,2,IYR) = r_unresoff(2,2) * 0.001
OGEOYINF(L48RGN+1,1,IYR) = r_infrsvoff(1,1) * 0.001
OGEOYINF(L48RGN+2,1,IYR) = (r_infrsvoff(3,1) + r_infrsvoff(4,1) + r_infrsvoff(5,1)) * 0.001
OGEOYINF(L48RGN+3,1,IYR) = r_infrsvoff(2,1) * 0.001
OGEOYINF(L48RGN+1,2,IYR) = r_infrsvoff(1,2) * 0.001
OGEOYINF(L48RGN+2,2,IYR) = (r_infrsvoff(3,2) + r_infrsvoff(4,2) + r_infrsvoff(5,2)) * 0.001
OGEOYINF(L48RGN+3,2,IYR) = r_infrsvoff(2,2) * 0.001
! ENDIF

END SUBROUTINE OGHIST_OFF

!************************************************************************************!
!
!************************************************************************************!
SUBROUTINE calc_ags(iyr)
IMPLICIT NONE
!***********************************************************************

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ncntrl'       ! nems control variables
INCLUDE 'ogsmparm'
INCLUDE 'ogsmbfw'
INCLUDE 'ogsmoff'
INCLUDE 'ogsml48'
INCLUDE 'ogsmout'
INCLUDE 'dwpparm'

! subroutine variables
INTEGER iyr
INTEGER ireg, ff

toff = curiyr - offbyr + 1
DO r = 1, offrgn
    prod_asg(r,iyr) = 0.
ENDDO

! use the discovered developed reserves to begin for year 1
! and for the next years use the end year reserves as the
! beginning of year reserves
IF (iyr.le.offnhyr) THEN
    DO ireg = 1, offrgn
        beg_rsva(ireg, iyr) =  Histadoff(ireg,iyr)
        IF(ireg.ge.3) prod_asg(ireg,iyr) = HISTADPRD(L48RGN+2,iyr)*beg_rsva(ireg,iyr) &
            / (histadoff(3,iyr)+histadoff(4,iyr)+histadoff(5,iyr))
        IF(ireg.eq.2) prod_asg(ireg,iyr) = histadprd(L48RGN+3,iyr)
        IF(ireg.eq.1) prod_asg(ireg,iyr) = histadprd(L48RGN+1,iyr)
    ENDDO
ELSEIF (iyr.eq.offnhyr+1) THEN
    DO ireg = 1, offrgn
        prod_asg(ireg,iyr) = prod_asg(ireg,iyr-1) * 1.15
        IF (prratoff(ireg,1) .ne. 0.0) &
            beg_rsva(ireg,iyr) = prod_asg(ireg,iyr)/prratoff(ireg,1)
    ENDDO
ELSE
    DO ff = 1, ntFld
        iREG = fld(ff)%OGSM
        prod_asg(ireg,iyr) = prod_asg(ireg,iyr) + fld(ff)%asgprd(toff)/1000000.  &
            + fld(ff)%asgRGprd(toff)/1000000.
        IF (prratoff(ireg,1) .ne. 0.0) &
            beg_rsva(ireg,iyr) = (fld(ff)%asgprd(toff)+fld(ff)%asgRGprd(toff))/1000000./prratoff(ireg,1)
    ENDDO
ENDIF

OGPRDAD(L48RGN+1,iyr) = prod_asg(1,iyr)
OGPRDAD(L48RGN+3,iyr) = prod_asg(2,iyr)
OGPRDAD(L48RGN+2,iyr) = prod_asg(3,iyr)+prod_asg(4,iyr)+prod_asg(5,iyr)

!write(offout,*) 'offadgas',iyr+1989,OGPRDAD(L48RGN+1,iyr),OGPRDAD(L48RGN+2,iyr),OGPRDAD(L48RGN+3,iyr) ! DBG_Flag_Associated_Gas

END SUBROUTINE calc_ags

!************************************************************************************!
!
!************************************************************************************!
SUBROUTINE EUnm2ID(EUnm,iEU,iPA,OK)
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmoff'      ! offshore variables
INCLUDE 'dwpparm'

CHARACTER*15, INTENT(IN) :: EUnm
INTEGER, INTENT(OUT) :: iPA
INTEGER, INTENT(OUT) :: iEU
LOGICAL, INTENT(OUT) :: OK
INTEGER i


OK = .FALSE.
DO i = 1,ntEU
    IF (EUnm.EQ.EUname(i)) THEN
        iPA = PAID(I)
        iEU = EUID(I)
        OK = .TRUE.
        EXIT
    ENDIF
ENDDO

END SUBROUTINE EUnm2ID

!************************************************************************************!
!
!************************************************************************************!
SUBROUTINE PFnm2ID(PFnm,iPF,OK)
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmoff'      ! offshore variables
INCLUDE 'dwpparm'

CHARACTER(15), INTENT(IN) :: PFnm
INTEGER J
INTEGER iPF
LOGICAL, INTENT(OUT) :: OK

OK = .FALSE.
DO J = 1,nPFTYP
    IF (PFnm.EQ. nmPF(J)) THEN
        OK = .TRUE.
        EXIT
    ENDIF
ENDDO

END SUBROUTINE PFnm2ID

!************************************************************************************!
!
!************************************************************************************!
SUBROUTINE REGnm2ID(REGnm,iREG,OK)
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmoff'      ! offshore variables
INCLUDE 'dwpparm'

CHARACTER(15), INTENT(IN) :: REGnm
INTEGER J
INTEGER iREG
LOGICAL, INTENT(OUT) :: OK

OK = .FALSE.
DO J = 1,nREG
    IF (REGnm.EQ. nmREG(J)) THEN
        OK = .TRUE.
        EXIT
    ENDIF
ENDDO

END SUBROUTINE REGnm2ID

!************************************************************************************!
!
!************************************************************************************!
SUBROUTINE DATAINITIALIZATIONANDDEFAULTS
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ncntrl'       ! nems control variables
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmbfw'      ! ogsm system variables
INCLUDE 'ogsml48'      ! misc variables
INCLUDE 'ogsmoff'      ! offshore variables
INCLUDE 'dwpparm'

INTEGER IPA, IEU, iFSC
INTEGER IFLD, NFLD
INTEGER*4 CALC_FSC
REAL*4 SIZE, TOTPRD
REAL*4 UNRRTOT(nPA,2)
LOGICAL FOUND, MATCH

! INITIALIZE ARRAYS
prod_asg = 0.
beg_rsva = 0.
adresad = 0.
DO iFLD = 1, TOTFLD
    FLD(iFLD)%nmEU = ""
    FLD(iFLD)%nickName = ""
    FLD(iFLD)%nmID = ""
    FLD(iFLD)%ogTyp = ""
    FLD(iFLD)%pfOptFlg = ""
    FLD(iFLD)%prjID=0
    FLD(iFLD)%Reg=0
    FLD(iFLD)%PA=0
    FLD(iFLD)%EU=0
    FLD(iFLD)%OGSM=0
    FLD(iFLD)%FSC=0
    FLD(iFLD)%initTyp=0
    FLD(iFLD)%curTyp=0
    FLD(iFLD)%decTyp=0
    FLD(iFLD)%rigTyp=0
    FLD(iFLD)%nPf=0
    FLD(iFLD)%pfTypFlg=0
    FLD(iFLD)%nDevWls=0
    FLD(iFLD)%yrDisc=0
    FLD(iFLD)%yrStPrd=0
    FLD(iFLD)%yrExpStart=0
    FLD(iFLD)%yrDevStart=0
    FLD(iFLD)%wDpth=0.0
    FLD(iFLD)%dDpth=0.0
    FLD(iFLD)%oilPrdHis=0.0
    FLD(iFLD)%gasPrdHis=0.0
    FLD(iFLD)%oilKR=0.0
    FLD(iFLD)%gasKR=0.0
    FLD(iFLD)%nOprWls=0
    FLD(iFLD)%nOprWlsRG=0
    FLD(iFLD)%oilPrd=0.0
    FLD(iFLD)%gasPrd=0.0
    FLD(iFLD)%cndPrd=0.0
    FLD(iFLD)%asgPrd=0.0
    FLD(iFLD)%oilRGPrd=0.0
    FLD(iFLD)%gasRGPrd=0.0
    FLD(iFLD)%cndRGPrd=0.0
    FLD(iFLD)%asgRGPrd=0.0
    FLD(IFLD)%FrcprdOil = 0.0
    FLD(IFLD)%FrcprdGas = 0.0
    FLD(IFLD)%prdODec = 0.0
    FLD(IFLD)%prdGDec = 0.0
    FLD(IFLD)%iprdOrate = 0
    FLD(IFLD)%iprdGrate = 0
    FLD(IFLD)%mprdOrate = 0
    FLD(IFLD)%mprdGrate = 0
ENDDO

! Adjust resources for high/low resource cases
IF (OGRUNOP(2).eq.28) then
    DO N= minFSC,minFSC+numFSC-1
        VMEAN(N) = VMEAN(N) * (1.+OGRUNOP(11)/100.)
    ENDDO
ENDIF
IF (OGRUNOP(2).eq.23) then
    DO N= minFSC,minFSC+numFSC-1
        VMEAN(N) = VMEAN(N) * (1.-ogrunop(13)/100.)
    ENDDO

    DO r = 1,offrgn
        DO k = 1,offfuel
            infrsvoff(r,k) = infrsvoff(r,k) * (1.-ogrunop(13)/100.)
        enddo
    enddo
ENDIF
IF (OGRUNOP(2).eq.29.or.OGRUNOP(2).eq.30) then
    DO N= minFSC,minFSC+numFSC-1
        VMEAN(N) = VMEAN(N) * (1.+ ogrunop(13)/100.)
    ENDDO

    DO r = 1,offrgn
        DO k = 1,offfuel
            infrsvoff(r,k) = infrsvoff(r,k) * (1.+ ogrunop(13)/100.)
        enddo
    enddo
ENDIF

! ASSIGN UNDISCOVERED FIELDS
IFLD = 0
r_unresoff = 0.
UNRRTOT = 0.
DO iPA = 1,nPA
    DO iEU = 1,nEU(iPA)
        DO iFSC = minFSC,minFSC+numFSC-1
            IF ( UNRR(iPA,iEU,iFSC).GT.0) THEN
                DO M = 1, UNRR(iPA,iEU,iFSC)
                    IFLD = IFLD + 1
                    FLD(IFLD)%PA = iPA
                    FLD(IFLD)%EU = iEU
                    FLD(IFLD)%nmEU = nmEU(iPA, iEU)
                    FLD(IFLD)%FSC = iFSC
                    FLD(IFLD)%INITTYP = 0
                    FLD(IFLD)%CURTYP = 0
                    FLD(IFLD)%oilKR = vmean(iFSC) * 1000000. * (1.-GOProp(iPA,iEU))
                    FLD(IFLD)%gasKR = vmean(iFSC) * 1000000. * GOProp(iPA,iEU) * BOEtoMcf
                    IF (YRAVL(iPA,iEU).LE.IJUMPYR+1989) THEN
                        r_unresoff(ogsmreg(iPA,iEU),1) = r_unresoff(ogsmreg(iPA,iEU),1) + fld(ifld)%oilKR/1000000.
                        r_unresoff(ogsmreg(iPA,iEU),2) = r_unresoff(ogsmreg(iPA,iEU),2) + fld(ifld)%gasKR/1000000.
                        UNRRTOT(iPA,1) = UNRRTOT(iPA,1) + fld(ifld)%oilKR/1000000.
                        UNRRTOT(iPA,2) = UNRRTOT(iPA,2) + fld(ifld)%gasKR/1000000.
                    ENDIF
                ENDDO
            ENDIF
        ENDDO
    ENDDO
ENDDO

!DO I=1,10
!    WRITE(OFFOUT,*) 'FLD1',FLD(I)%nmEU,FLD(I)%FSC,FLD(I)%oilKR,FLD(I)%gasKR
!ENDDO
DO I=1,OFFRGN
    WRITE(OFFOUT,*) 'urroff',i,r_unresoff(I,1),r_unresoff(I,2),r_unresoff(I,1)+r_unresoff(I,2)/BOEtoMcf ! DBG_Flag_RecRes_by_Reg (1/2)
ENDDO
DO I=1,nPA
    WRITE(OFFOUT,*) 'urroff',i,unrrtot(I,1),unrrtot(I,2),unrrtot(I,1)+unrrtot(I,2)/BOEtoMcf ! DBG_Flag_RecRes_by_Reg (2/2)
ENDDO

! ASSIGN DISCOVERED/UNDEVELOPED FIELDS
DO I = 1,nANN
    IFLD = IFLD + 1
    CALL EUnm2ID( ANN_EU(I),FLD(IFLD)%EU,FLD(IFLD)%PA,FOUND)
    IF (.NOT.FOUND) WRITE(OFFOUT,*) 'WARNING: Invalid Evaluation Unit Name For Discovered/undeveloped Field', IFLD,  ANN_EU(I)
    FLD(IFLD)%FSC =  ANN_FSC(I)
    FLD(IFLD)%INITTYP = 1
    FLD(IFLD)%CURTYP = 1
    FLD(IFLD)%OGTYP =  ANN_OG(I)
    FLD(IFLD)%NMID =  ANN_FN(I)
    FLD(IFLD)%NICKNAME = ANN_NN(I)
    FLD(IFLD)%WDPTH =  ANN_WD(I)
    !FLD(IFLD)%PFOPTFLG =  ANN_FAC(I)
    !IF (ANN_FAC(I).NE."SS") FLD(IFLD)%PFOPTFLG = ""
    !FLD(IFLD)%NDEVWLS = ANN_WL(I)
    ! Assigning pfTypFlg as defined in dwpparm
    IF (ANN_FAC(I).EQ. "FP") THEN
        FLD(IFLD)%pfTypFlg(1) = 1
    ELSE IF (ANN_FAC(I).EQ. "CT") THEN
        FLD(IFLD)%pfTypFlg(2) = 1
    ELSE IF (ANN_FAC(I).EQ. "TLP") THEN
        FLD(IFLD)%pfTypFlg(3) = 1
    ELSE IF (ANN_FAC(I).EQ. "FPS") THEN
        FLD(IFLD)%pfTypFlg(4) = 1
    ELSE IF (ANN_FAC(I).EQ. "SPAR") THEN
        FLD(IFLD)%pfTypFlg(5) = 1
    ELSE IF (ANN_FAC(I).EQ. "FPSO") THEN
        FLD(IFLD)%pfTypFlg(6) = 1
    ELSE IF (ANN_FAC(I).EQ. "SS") THEN
        FLD(IFLD)%pfTypFlg(7) = 1
    ENDIF
    FLD(IFLD)%NDEVWLS =  nDevWls(FLD(IFLD)%PA,FLD(IFLD)%EU,FLD(IFLD)%FSC)
    FLD(IFLD)%oilKR = vmean(FLD(IFLD)%FSC) * 1000000. * (1.-GOProp(FLD(IFLD)%PA,FLD(IFLD)%EU))
    FLD(IFLD)%gasKR = vmean(FLD(IFLD)%FSC) * 1000. * (GOProp(FLD(IFLD)%PA,FLD(IFLD)%EU)) * BOEtoMcf
    FLD(IFLD)%YRDISC =  ANN_YRDISC(I)
    FLD(IFLD)%YRSTPRD =  ANN_PRDSTYR(I)
    FLD(IFLD)%FrcprdOil = ANN_FRCOIL(I)
    FLD(IFLD)%FrcprdGas = ANN_FRCGAS(I)
    FLD(IFLD)%prdODec = ANN_DECOIL(I)
    FLD(IFLD)%prdGDec = ANN_DECGAS(I)
    FLD(IFLD)%iprdOrate = ANN_IPRDOIL(I)
    FLD(IFLD)%iprdGrate = ANN_IPRDGAS(I)
    FLD(IFLD)%mprdOrate = ANN_PKPRDOIL(I)
    FLD(IFLD)%mprdGrate = ANN_PKPRDGAS(I)
ENDDO

!  ASSIGN PRODUCING FIELDS
DO I = 1,nPRD
    IF (PRD_FLAG(1,I).NE.3) THEN
        IFLD = IFLD + 1
        CALL EUnm2ID( PRD_EU(1,I),FLD(IFLD)%EU,FLD(IFLD)%PA,FOUND)
        IF (.NOT.FOUND) WRITE(OFFOUT,*) 'WARNING: Invalid Evaluation Unit Name For Producing Oil Field', IFLD,  PRD_EU(1,I)
        FLD(IFLD)%FSC =  PRD_FSC(1,I)
        FLD(IFLD)%INITTYP = 2
        FLD(IFLD)%CURTYP = 2
        FLD(IFLD)%OGTYP =  PRD_OG(1,I)
        FLD(IFLD)%PFOPTFLG =  ""
        FLD(IFLD)%NMID =  PRD_ID(1,I)
        FLD(IFLD)%NICKNAME =  PRD_FN(1,I)
        FLD(IFLD)%DDPTH =  PRD_DEPTH(1,I)
        FLD(IFLD)%oilKR = vmean(FLD(IFLD)%FSC) * 1000000. * (1.-GOProp(FLD(IFLD)%PA,FLD(IFLD)%EU))
        FLD(IFLD)%gasKR = vmean(FLD(IFLD)%FSC) * 1000. * (GOProp(FLD(IFLD)%PA,FLD(IFLD)%EU)) * BOEtoMcf
        FLD(IFLD)%YRDISC =  PRD_YRDISC(1,I)
        FLD(IFLD)%YRSTPRD = YRBEGFORECAST
        FLD(IFLD)%DECTYP =  PRD_FLAG(1,I)
        DO M= 1, nTime0
            FLD(IFLD)%OILPRDHIS(M) =  PROD(1,I,M) * 1000.
        ENDDO
    ENDIF
ENDDO
NFLD = IFLD
DO I = 1,nPRD
    IF (PRD_FLAG(2,I).NE.3) THEN
        MATCH = .FALSE.
        DO J=1,NFLD
            IF ( PRD_ID(2,I).EQ.FLD(J)%NMID) THEN
                DO M= 1, nTime0
                    FLD(J)%GASPRDHIS(M) =  PROD(2,I,M) * 1000.
                ENDDO
                IF (FLD(J)%OGTYP.EQ.'G'.OR.FLD(J)%OGTYP.EQ.'g') THEN
                    FLD(J)%DECTYP = PRD_FLAG(2,I)
                ENDIF
                MATCH = .TRUE.
                EXIT
            ENDIF
        ENDDO
        IF (.NOT.MATCH) THEN
            IFLD = IFLD + 1
            FOUND = .FALSE.
            CALL EUnm2ID( PRD_EU(2,I),iEU,iPA,FOUND)
            IF (FOUND) FLD(IFLD)%PA = iPA
            IF (FOUND) FLD(IFLD)%EU = iEU
            IF (.NOT.FOUND) WRITE(OFFOUT,*) 'WARNING: Invalid Evaluation Unit Name For Producing Gas Field', IFLD,  PRD_EU(2,I)
            FLD(IFLD)%FSC =  PRD_FSC(2,I)
            FLD(IFLD)%INITTYP = 2
            FLD(IFLD)%CURTYP = 2
            FLD(IFLD)%OGTYP =  PRD_OG(2,I)
            FLD(IFLD)%PFOPTFLG =  ""
            FLD(IFLD)%NMID =  PRD_ID(2,I)
            FLD(IFLD)%DDPTH =  PRD_DEPTH(2,I)
            FLD(IFLD)%gasKR = vmean(FLD(IFLD)%FSC) * 1000000. * (GOProp(FLD(IFLD)%PA,FLD(IFLD)%EU)) * BOEtoMcf
            FLD(IFLD)%YRDISC =  PRD_YRDISC(2,I)
            FLD(IFLD)%YRSTPRD = YRBEGFORECAST
            FLD(IFLD)%DECTYP =  PRD_FLAG(2,I)
            DO M= 1,nTime0
                FLD(IFLD)%GASPRDHIS(M) =  PROD(2,I,M) * 1000.
                !IF (m.eq.nTime0) FLD(IFLD)%GASPRDHIS(M) =  PROD(2,I,M) * 1000. * 0.75  ! Adjust to match STEO
            ENDDO
        ENDIF
    ENDIF
ENDDO

! ASSIGN TOTAL NUMBER OF FIELDS
NTFLD = IFLD

! ASSIGN OTHER INITIAL FIELD SPECS

DO I=1,NTFLD
    FLD(I)%REG =  PAREG(FLD(I)%PA)
    FLD(I)%OGSM =  OGSMREG(FLD(I)%PA,FLD(I)%EU)
    FLD(I)%nmEU = nmEU(FLD(I)%PA,FLD(I)%EU)
    FLD(I)%FSC = MAX(FLD(I)%FSC,minFSC)
    FLD(I)%FSC = MIN(FLD(I)%FSC,minFSC+numFSC-1)
    IF (FLD(I)%WDPTH.LT.0.1) FLD(I)%WDPTH =  WDPTH(FLD(I)%PA,FLD(I)%EU,FLD(I)%FSC)
    IF (FLD(I)%DDPTH.LT.0.1) FLD(I)%DDPTH =  DDPTH(FLD(I)%PA,FLD(I)%EU,FLD(I)%FSC)
    IF (FLD(I)%WDPTH.GT.90000.) FLD(I)%WDPTH =  WDPTH(FLD(I)%PA,FLD(I)%EU,FLD(I)%FSC)
    IF (FLD(I)%DDPTH.GT.90000.) FLD(I)%DDPTH =  DDPTH(FLD(I)%PA,FLD(I)%EU,FLD(I)%FSC)

    IF (FLD(I)%INITTYP.EQ.2) THEN
        IF (FLD(I)%dectyp.EQ.0) THEN
            FLD(I)%yrDevStart = 1     ! New Field
        ELSEIF (FLD(I)%dectyp.NE.3) THEN
            DO M = 1, ntime0
                IF (FLD(I)%OILPRDHIS(M).GT.0. .OR. FLD(I)%GASPRDHIS(M).GT.0.) EXIT
            ENDDO
            FLD(I)%YRDEVSTART = yearof0(M) - yearof(1) + 1
            IF (FLD(I)%YRDISC.EQ.9999) FLD(I)%YRDISC = M+YrBegPrdHis-1 - 3
            IF (M.EQ.1 .OR. M.GT.nTime0) THEN
                FLD(I)%YRDEVSTART = FLD(I)%yrDisc - yrBegPrdHis + 1
            ENDIF
        ENDIF
    ENDIF

    IF (FLD(I)%NDEVWLS.LE.0) FLD(I)%NDEVWLS =  NDEVWLS(FLD(I)%PA,FLD(I)%EU,FLD(I)%FSC)

    J=1
    DO WHILE (FLD(I)%WDPTH.GT. PFWDMIN(J))
        J = J+1
        IF (J.GT.5) EXIT
    ENDDO
    FLD(I)%PFOPTFLG =  PFTYPE(J-1,FLD(I)%FSC)
    IF (FLD(I)%PFOPTFLG.EQ."") FLD(I)%PFOPTFLG =  PFTYPE(J-1,FLD(I)%FSC)

ENDDO

! CHECK IF FSC LISTED FOR PRODUCING FIELDS MATCHES CURRENT RESERVES PLUS CUM PRODUCTION
! NOT USING THIS FSC INFORMATION YET
dnrr = 0
dunrr = 0
DO I=1,NTFLD
    IF (FLD(I)%INITTYP.EQ.2) THEN
        ! Count the number of producing fields
        dnrr(FLD(I)%PA,FLD(I)%EU,FLD(I)%FSC) = dnrr(FLD(I)%PA,FLD(I)%EU,FLD(I)%FSC) + 1
        SIZE = (FLD(I)%GASKR)/BOEtoMCF + FLD(I)%OILKR
        J=minFSC
        IF (SIZE.LT. VMIN(J)) THEN
            IF (SIZE.GT.0.0) THEN
                TOTPRD = 0.
                DO M= 1, nTime0
                    TOTPRD = TOTPRD + (FLD(I)%GASPRDHIS(M))/BOEtoMCF + FLD(I)%OILPRDHIS(M)
                ENDDO
                IF (TOTPRD.GT.0.) WRITE(OFFOUT,*) 'WARNING: PRODUCING FIELD SIZE IS LESS THAN MINIMUM FSC', &
                    FLD(I)%PA, FLD(I)%EU, FLD(I)%NMID, TOTPRD
            ENDIF
            CALC_FSC = FLD(I)%FSC
        ELSE
            DO WHILE (SIZE.GT. VMIN(J))
                J=J+1
                IF (J.GT.minFSC+numFSC-1) EXIT
            ENDDO
            CALC_FSC = J - 1
        ENDIF
        !WRITE(OFFOUT,*) 'FSC', I, FLD(I)%FSC, CALC_FSC, SIZE
    ENDIF
    IF (FLD(I)%INITTYP.EQ.1) THEN
        ! Count the number of discovered/undeveloped fields
        dunrr(FLD(I)%PA,FLD(I)%EU,FLD(I)%FSC) = dunrr(FLD(I)%PA,FLD(I)%EU,FLD(I)%FSC) + 1
    ENDIF
ENDDO

! ASSIGN FIELD ID BASED ON CURRENT STATUS OF THE FIELD (CATEGORY)
CALL ASSIGNCURRENTFIELDID

! MAXIMUM NUMBER OF FIELDS FOR A PROJECT/PROSPECT - SET AS PARAMETER IN DWPPARM
!MAXNFIELDS =  PFMAXNFLD(2)
!IF ( PFMAXNFLD(3).GT. PFMAXNFLD(2)) MAXNFIELDS =  PFMAXNFLD(3)

! DEVELOPMENT PROJECTS
NDEVPRJ = 0 ! NUMBER DEVELOPMENT PROJECTS

!WRITE(OFFOUT,*) 'Total Number of Fields [ntfld, maxnfields]', NTFLD, MAXNFIELDS
!DO I=1,NTFLD
    !WRITE(OFFOUT,*) 'FLDID',I,FLD(I)%nmEU,FLD(I)%NMID,FLD(I)%FSC,FLD(I)%CURTYP,FLD(I)%oilKR,FLD(I)%gasKR 
!ENDDO

END SUBROUTINE DATAINITIALIZATIONANDDEFAULTS

!************************************************************************************!
!
!************************************************************************************!
SUBROUTINE ASSIGNCURRENTFIELDID
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ncntrl'       ! nems control variables
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmbfw'      ! ogsm system variables
INCLUDE 'ogsmoff'      ! offshore variables
INCLUDE 'dwpparm'

INTEGER iPA, iEU, iFSC, iFLD, iFLDTYP
INTEGER nFLD(0:2,nPA,nmaxEU,minFSC:minFSC+numFSC-1)
INTEGER TEMP(minFSC:minFSC+numFSC-1)

! DETERMINE MAXIMUM NUMBER OF FIELDS IN A FSC
NFLD = 0
NMAXFLD = 0
DO IFLD = 1,NTFLD
    IPA = FLD(IFLD)%PA
    IEU = FLD(IFLD)%EU
    IFSC = FLD(IFLD)%FSC
    IFLDTYP = FLD(IFLD)%CURTYP
    ! Count numver of Fields
    NFLD(IFLDTYP,IPA,IEU,iFSC) = NFLD(iFLDTYP,iPA,iEU,iFSC) + 1
    ! Store maximum number of fields in an FSC
    nMAXFLD = MAX(nMAXFLD,NFLD(iFLDTYP,IPA,IEU,iFSC))
ENDDO
!WRITE(OFFOUT,*) 'Maximum number of fields in a FSC (nmaxfld) =', nmaxfld ! DBG_Flag_nmaxfld

! ASSIGN FIELD ID
NFLD = 0
DO IFLD = 1,NTFLD
    IPA = FLD(IFLD)%PA
    IEU = FLD(IFLD)%EU
    IFSC = FLD(IFLD)%FSC
    IFLDTYP = FLD(IFLD)%CURTYP
    ! Field counter
    N = NFLD(iFLDTYP,iPA,iEU,iFSC) + 1
    NFLD(IFLDTYP,iPA,IEU,iFSC) = N
    ! Store field ID
    !FLDID(iFLDTYP,iPA,iEU,iFSC,N) = iFLD
    FLDID1(IFLD) = iFLD
    FLDID2(IFLD) = iFLDTYP
    FLDID3(IFLD) = iPA
    FLDID4(IFLD) = iEU
    FLDID5(IFLD) = iFSC
    FLDID6(IFLD) = N
ENDDO

END SUBROUTINE ASSIGNCURRENTFIELDID

!************************************************************************************!
!
!************************************************************************************!
REAL*4 FUNCTION LINEAR2(X,X1,X2,V1,V2)
IMPLICIT NONE

REAL*4, INTENT(IN) :: X
REAL*4, INTENT(IN) :: X1
REAL*4, INTENT(IN) :: X2
REAL*4, INTENT(IN) :: V1
REAL*4, INTENT(IN) :: V2

LINEAR2 = (V2-V1) * (X-X1)/(X2-X1) + V1

RETURN
END FUNCTION LINEAR2

!************************************************************************************!
!
!************************************************************************************!
REAL*4 FUNCTION LINEAR4(X,X1,X2,Y,Y1,Y2,V11,V12,V21,V22)
IMPLICIT NONE

REAL*4, INTENT(IN) :: X
REAL*4, INTENT(IN) :: X1
REAL*4, INTENT(IN) :: X2
REAL*4, INTENT(IN) :: Y
REAL*4, INTENT(IN) :: Y1
REAL*4, INTENT(IN) :: Y2
REAL*4, INTENT(IN) :: V11
REAL*4, INTENT(IN) :: V12
REAL*4, INTENT(IN) :: V21
REAL*4, INTENT(IN) :: V22

REAL*4 LINEAR2
REAL*4 VY1
REAL*4 VY2

VY1 = LINEAR2(X,X1,X2,V11,V12)
VY2 = LINEAR2(X,X1,X2,V21,V22)
LINEAR4 = LINEAR2(Y,Y1,Y2,VY1,VY2)

RETURN
END FUNCTION LINEAR4

!************************************************************************************!
!
!************************************************************************************!
INTEGER FUNCTION DEVDRILLPERYEAR(ipftyp,ns,wd,dd)
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'dwpparm'

INTEGER, INTENT(IN) :: ipftyp
INTEGER, INTENT(IN) :: ns
REAL*4, INTENT(IN) :: wd
REAL*4, INTENT(IN) :: dd

INTEGER i, i1,i2
INTEGER nextInt
REAL*4 LINEAR2
REAL*4 X, X1, X2, V1, V2, Y

IF ( PLATFORM(ipftyp).EQ.1) THEN   ! PLATFORM
    ! GET DRILLING DEPTH INDEX
    DO I = 1,NPLTDD
        IF ( DEVDRLDLY24DDPTH(I).GE.DD) EXIT
        IF (I.EQ.NPLTDD) WRITE(OFFOUT,*) 'WARNING: DRILLING DEPTH OUT OF RANGE IN DEVDRILLPERYEAR', dd
    ENDDO
    IF (I.EQ.1) THEN
        I1 = I; I2 = I+1
    ELSE
        I1 = I-1; I2 = I
    ENDIF

    ! LINEAR INTERPOLATION
    X = DD
    X1 =  DEVDRLDLY24DDPTH(I1)
    X2 =  DEVDRLDLY24DDPTH(I2)
    V1 =  DEVDRLDLY24(I1)
    V2 =  DEVDRLDLY24(I2)

    ! TABLE BASED ON 24 SLOT PLATFORM - ADJUST FOR OTHER SIZES
    DEVDRILLPERYEAR = INT(LINEAR2(X,X1,X2,V1,V2) * NS / 24)

ELSE ! OTHER PRODUCTION FACILITY (SS, FPS, FPSO)
    ! GET WATER DEPTH INDEX
    DO I = 1,NOPFWD
        IF ( DEVDRLDLYOTHWDPTH(I).GE.WD) EXIT
        IF (I.EQ.NOPFWD) WRITE(OFFOUT,*) 'WARNING: WATER DEPTH OUT OF RANGE IN DEVDRILLPERYEAR', wd
    ENDDO
    IF (I.EQ.1) THEN
        I1 = I; I2 = I+1
    ELSE
        I1 = I-1; I2 = I
    ENDIF

    ! LINEAR INTERPOLATION
    X = WD
    X1 =  DEVDRLDLYOTHWDPTH(I1)
    X2 =  DEVDRLDLYOTHWDPTH(I2)
    V1 =  DEVDRLDLYOTH(ipfTyp,i1)
    V2 =  DEVDRLDLYOTH(ipfTyp,i2)
    DEVDRILLPERYEAR = INT(LINEAR2(X,X1,X2,V1,V2))
ENDIF

!WRITE(OFFOUT,*) 'devdrillperyear', iPFTyp, NS, WD, DD, DEVDRILLPERYEAR

RETURN
END FUNCTION DEVDRILLPERYEAR

!************************************************************************************!
!
!************************************************************************************!
INTEGER FUNCTION PFDELAY1(ipftyp,wd,nwls,t1)
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmoff'
INCLUDE 'dwpparm'

INTEGER ipftyp
INTEGER nwls
INTEGER t1
REAL*4 wd

INTEGER i,i1,i2,j,j1,j2,tt,delaypf
INTEGER nextInt
REAL*4 LINEAR2, LINEAR4
REAL*4 X, X1, X2, Y, Y1, Y2
REAL*4 V11, V12, V21, V22, IRATE

! ALLOW A MAXIMUM OF 60 WELLS
IF (nwls.gt.60) WRITE(offout,*) 'WARNING: NUMBER OF WELLS > 60 IN PFDELAY1', nwls

! GET WATER DEPTH INDEX
DO I = 1,NPFWD
    IF ( pfBldDlyWDPTH(I).GE.WD) EXIT
    IF (I.EQ.NPFWD) WRITE(OFFOUT,*) 'WARNING: WATER DEPTH OUT OF RANGE IN PFDELAY1', wd
ENDDO
IF (I.EQ.1) THEN
    I1 = I; I2 = I+1
ELSE
    I1 = I-1; I2 = I
ENDIF
X = WD
X1 =  pfBldDlyWDpth(I1)
X2 =  pfBldDlyWDpth(I2)

! PRODUCTION FACILITY TYPE
IF ( PLATFORM(ipftyp).EQ.1) THEN   ! PLATFORM
    !  GET NUMBERS OF SLOTS INDEX
    DO J = 1,NSLTIDX
        IF ( NSLT(J).GE.NWLS) EXIT
        IF (J.EQ.NSLTIDX) WRITE(OFFOUT,*) 'WARNING: NUMBER OF WELLS OUT OF RANGE IN PFDELAY1', nwls
    ENDDO
    IF (J.EQ.1) THEN
        J1 = J; J2 = J+1
    ELSE
        J1 = J-1; J2 = J
    ENDIF

    ! LINEAR INTERPOLATION
    Y = nWLS
    Y1 =  nSLT(J1)
    Y2 =  nSLT(J2)
    V11 =  PFBLDDLY1(i1,ipfTyp,J1)
    V12 =  PFBLDDLY1(i1,ipfTyp,J2)
    V21 =  PFBLDDLY1(i2,ipfTyp,J1)
    V22 =  PFBLDDLY1(i2,ipfTyp,J2)

    DELAYPF = NextINT(LINEAR4(X,X1,X2,Y,Y1,Y2,V11,V12,V21,V22))

ELSE ! Other production facilities
    ! LINEAR INTERPOLATION
    V11 =  PFBLDDLY1(i1,ipfTyp,0)
    V21 =  PFBLDDLY1(i2,ipfTyp,0)
    DELAYPF = NextINT(LINEAR2(X,X1,X2,V11,V21))

ENDIF

! PRODUCTION FACILITY DESIGN, FABRICATION, AND INSTALLATION TECHNOLOGY
! => REDUCES TIME TO CONSTRUCT A PRODUCTION FACILITY
TT = INT( LEVPFDLY(2))
IF (T1.LT.TT) TT = T1
! TOTAL IMPROVEMENT FOR THE YEAR (FRACTION)
IRATE = TT * ( LEVPFDLY(1)/ LEVPFDLY(2))/100.
DELAYPF = NextINT(DELAYPF*(1.-IRATE))
IF (DELAYPF.LT.0) DELAYPF = 0

PFDELAY1 = DELAYPF
IF (PFDELAY1.EQ.0) WRITE(offout,*) 'WARNING:  PRODUCTION FACILITY TYPE NOT ALLOWED IN SPECIFIED WATER DEPTH', IPFTyp, WD

!WRITE(OFFOUT,*) 'pfdelay1 [iPFTyp, nWls, WD, T1, PFDELAY1]', iPFTyp, NWLS, WD, T1, PFDELAY1

RETURN
END FUNCTION PFDELAY1

!************************************************************************************!
!
!************************************************************************************!
INTEGER FUNCTION PFDELAY2(wd)
IMPLICIT NONE

INCLUDE 'parametr' ! nems dimension parameters
INCLUDE 'ogsmparm' ! ogsm parameter file
INCLUDE 'dwpparm'

REAL*4, INTENT(IN) :: WD
INTEGER I,I1,I2
INTEGER NextInt
REAL*4 X,X1,X2,V1,V2
REAL*4 LINEAR2

! GET WATER DEPTH INDEX
DO I = 1,NPFWD
    IF (pfBldDLYWDPTH(I).GE.WD) EXIT
    IF (I.EQ.NPFWD) WRITE(OFFOUT,*) 'WARNING: WATER DEPTH OUT OF RANGE IN PFDELAY2', wd
ENDDO
IF (I.EQ.1) THEN
    I1 = I; I2 = I+1
ELSE
    I1 = I-1; I2 = I
ENDIF

! LINEAR INTERPOLATION
X = WD
X1 =  PFBldDLYWDpth(I1)
X2 =  PFBldDLYWDpth(I2)
V1 =  PFBLDDLY2(I1)
V2 =  PFBLDDLY2(I2)
PFDELAY2 = NextINT(LINEAR2(X,X1,X2,V1,V2))

!WRITE(OFFOUT,*) 'pfdelay2', WD, PFDELAY2

RETURN
END FUNCTION PFDELAY2

!************************************************************************************!
!
!************************************************************************************!
REAL*4 FUNCTION FLOWLINECOST(iPA,iEU,WD,T1)
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ncntrl'       ! nems control variables
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'dwpparm'

INTEGER iPA
INTEGER iEU
INTEGER T1 
REAL*4 WD

INTEGER IREG, IOGSM
REAL*4 FLLEN, PDIAM
REAL*4 COSTFL

! GET REGION INDEX
iREG =  PAREG(iPA)
iOGSM =  OGSMREG(iPA,iEU)

! GET FLOWLINE LENGTH AND DIAMETER
FLLEN =  TRNFLOWLINELEN(iPA,iEU)
PDIAM =  TRNPPDIAM(iPA,iEU)

! CALCULATE COST
COSTFL = 0.042 * PDIAM * (WD/600)**0.1

! CHANGE FLOWLINE COST AS A FUNCTION OF OIL PRICE
FLOWLINECOST = COSTFL * (1. + (OILPRICE(iOGSM,TOFF)/ OILPRCCSTTBL - 1.)* CHGPFCSTOIL)

!WRITE(OFFOUT,*) 'flowlinecost', iPA, iEU, WD, T1, COSTFL, FLOWLINECOST

RETURN
END FUNCTION FLOWLINECOST

!************************************************************************************!
!
!************************************************************************************!
REAL*4 FUNCTION PFCOST(iPA,iEU,iFSC,WD,iPFTyp,NWLS,T1)
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ncntrl'       ! nems control variables
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmbfw'
INCLUDE 'ogsmoff'
INCLUDE 'dwpparm'
INCLUDE 'macout'

INTEGER iPA
INTEGER iEU
INTEGER iFSC
INTEGER iPFTyp
INTEGER NWLS
INTEGER T1
REAL*4 WD

INTEGER SLT, nPF
REAL*4 WDm
INTEGER iREG, TT
INTEGER nextint
REAL*4 COSTPF, iRATE

! GET REGION INDEX
iREG =  PAREG(iPA)

! CONVERT WATER DEPTH FROM FEET TO METERS
WDm = WD * 0.3048

! GET WATER DEPTH CATEGORY
J=1
DO WHILE (WDm.GT.PFWDMIN(J))
    J=J+1
    IF (J.GT.5) EXIT
ENDDO

! GET NUMBERS OF SLOTS INDEX
SLT = PFNSLOT(J-1,iFSC)

! PRODUCTION FACILITY COST
! Costs updated perfunctorily December 2015 based on IHS study - needs a more detailed look
!  Basically, looked at the costs IHS cited for various production facilities of various capacities in
!  in various water depths and applied multiplicative factors to these equations to get our costs to line up with the report
! Need to revisit how we do costs for subsea systems - not only for subsea systems requiring installation of a FPS, but also
!  and *in particular*, for subsea tiebacks to existing facilities; subsea systems should be a function of water depth and tieback distance
!  While doing this, should probably separate out costs of subsea tiebacks to existing facilities versus
!  subsea systems requiring installation of a production facility

IF (ipftyp.eq.1) THEN ! FIXED PLATFORM
    COSTPF = (2000000. + 9000.*SLT + 1500.*WD*SLT+ 40.*WD**2)/1000000.
    IF (iPA.EQ.2) COSTPF = COSTPF * 1.06   ! ADJUST TO ACCOUNT FOR CHANGE IN REGULATIONS
ELSEIF (ipftyp.eq.2) THEN ! COMPLIANT TOWER
    COSTPF = ((SLT+30.) * (1500000. + 2000.*(WD-1000.)))/1000000.
    IF (iPA.EQ.2) COSTPF = COSTPF * 1.06   ! ADJUST TO ACCOUNT FOR CHANGE IN REGULATIONS
ELSEIF (ipftyp.eq.3) THEN ! TENSION LEG PLATFORM
    !        COSTPF = ((SLT+30.) * (3000000. + 750.*(WD-1000.)))/1000000.
    COSTPF = 2 * ((SLT+30.) * (3000000. + 750.*(WD-1000.)))/1000000. 	! Based on IHS 2015 costs report for EIA; figure on TLP costs by capacity and WD
    IF (iPA.EQ.2) COSTPF = COSTPF * 1.06   ! ADJUST TO ACCOUNT FOR CHANGE IN REGULATIONS
ELSEIF (ipftyp.eq.4) THEN ! FLOATING PRODUCTION SYSTEM (including SEMI-SUBMERSIBLE here)
    COSTPF = ((SLT+20.) * (7500000. + 250.*(WD-1000.)))/1000000.
    IF (iPA.EQ.2) COSTPF = COSTPF * 1.02   ! ADJUST TO ACCOUNT FOR CHANGE IN REGULATIONS
ELSEIF (ipftyp.eq.5) THEN ! SPAR
    ! COSTPF = ((SLT+20.) * (3000000. + 500.*(WD-1000.)))/1000000.
    COSTPF = 100 * ((SLT+20.) * (3000000. + 500.*(WD-1000.)))/1000000.
    IF (iPA.EQ.2) COSTPF = COSTPF * 1.02   ! ADJUST TO ACCOUNT FOR CHANGE IN REGULATIONS

    ! Using FPS cost as placeholder for costs related to FPSO - need to figure out what costs are
ELSEIF (ipftyp.eq.6) THEN ! FLOATING PRODUCTION STORAGE AND OFFLOADING
    COSTPF = 4 * ((SLT+20.) * (7500000. + 250.*(WD-1000.)))/1000000.
    IF (iPA.EQ.2) COSTPF = COSTPF * 1.02   ! ADJUST TO ACCOUNT FOR CHANGE IN REGULATIONS

    ! To reiterate:
    ! Leaving as is right now for time's sake since costs as they are for SS are roughly in the ballpark as IHS study costs
    ! Need to revisit how we do costs for subsea systems - not only for subsea systems requiring installation of a FPS,
    !  but also and in particular, for subsea tiebacks to existing facilities;
    ! Subsea systems **should be** a function of water depth and tieback distance
    ! Also, should probably separate out costs of subsea tiebacks to existing facilities versus
    !  subsea systems requiring installation of a production facility
ELSEIF (ipftyp.eq.7) THEN                                ! FLOATING PRODUCTION SYSTEM W/ SUBSEA SYSTEM
    COSTPF = ((SLT+20.) * (1500000. + 250.*(WD-1000.)) + 2500000.)/1000000.
    IF (iPA.EQ.2) COSTPF = COSTPF * 1.02   ! ADJUST TO ACCOUNT FOR CHANGE IN REGULATIONS
ELSE
    WRITE(offout,*) 'WARNING: INVALID PRODUCTION FACILITY TYPE IN PFCOST', nmEU(iPA,iEU), WD, ipftyp
ENDIF
!COSTPF = 0.000001
! NUMBER OF FACILITIES
!nPF = nextINT(1.*NWLS/(SLT*4.))
nPF = 1

! PRODUCTION FACILITY COST TECHNOLOGY
! => REDUCES PRODUCTION FACILITY CONSTRUCTION COST
TT = INT( LEVPFCST(2))
IF (T1.LT.TT) TT = T1
! TOTAL IMPROVEMENT FOR THE YEAR (FRACTION)
IRATE = TT * ( LEVPFCST(1)/ LEVPFCST(2))/100.
COSTPF = COSTPF*(1.-IRATE)
IF (COSTPF.LT.0) COSTPF = 0.

PFCOST = COSTPF * nPF * (1.+ogrunop(20)/100.)
IF (PFCOST.EQ.0) WRITE(offout,*) 'WARNING:  PRODUCTION FACILITY TYPE NOT ALLOWED IN SPECIFIED WATER DEPTH', IPFTyp, WD

!WRITE(OFFOUT,100) 'pfcost [EU,WD,FSC,iPFTyp,SLT,nPF,nWLS,T1,PFCost]', nmEU(iPA,iEU), WD, iFSC, iPFTyp, SLT, nPF, NWLS, T1, PFCOST*MC_JPGDP( 2015-BASEYR+1)/MC_JPGDP(yrCstTbl-BASEYR+1)
!100 FORMAT(A50,2X,A15,F8.2,6I5,F8.2)
RETURN
END FUNCTION PFCOST

!************************************************************************************!
!
!************************************************************************************!
REAL*4 FUNCTION OPRCOST(iPA,iEU,iFSC,WD,DD,T1)
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ncntrl'       ! nems control variables
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmbfw'
INCLUDE 'ogsmoff'
INCLUDE 'macout'
INCLUDE 'dwpparm'

INTEGER iPA
INTEGER iEU
INTEGER iFSC
INTEGER T1
REAL*4 WD
REAL*4 DD

INTEGER IREG, IOGSM
INTEGER II, JJ, TT, SLT, RR
REAL*4 iRATE
REAL*4 COSTOPR

! GET REGION INDEX
iREG =  PAREG(iPA)
iOGSM =  OGSMREG(iPA,iEU)

! GET WATER DEPTH CATEGORY
JJ=1
DO WHILE (WD*0.3048.GT.PFWDMIN(JJ))
    JJ=JJ+1
    IF (JJ.GT.5) EXIT
ENDDO

! GET NUMBERS OF SLOTS INDEX
SLT = PFNSLOT(JJ-1,iFSC)

! Costs reviewed perfunctorily January 2016 based on IHS study - needs a more detailed look
!  Operating costs should be a function of production facility and WD
IF (WD.LE.1500.) THEN
    !COSTOPR = (ALPHA_OC + BETA1_OC*SLT + BETA2_OC*SLT*WD**2) / 1000000.
    COSTOPR = 3 * (ALPHA_OC + BETA1_OC*SLT + BETA2_OC*SLT*WD**2) / 1000000.
ELSE
    !COSTOPR = (ALPHA_OC + BETA1_OC*SLT + BETA2_OC*SLT*1500.**2) / 1000000.
    COSTOPR = 3 * (ALPHA_OC + BETA1_OC*SLT + BETA2_OC*SLT*1500.**2) / 1000000.
ENDIF
COSTOPR = COSTOPR / (SLT*1.)

RR = 8
IF (iOGSM.EQ.2) RR = 9
IF (OGRUNOP(2).eq.11.AND.curiyr.ge.techyr) COSTOPR = COSTOPR + FEOPR_GAS(1,RR,curiyr-techyr+1)/1000000. &
    * mc_jpgdp(yrCstTbl-baseyr+1)/mc_jpgdp(-2)     ! FE TECHNOLOGY ADJUSTMENT
IF (OGRUNOP(2).eq.12.AND.curiyr.ge.techyr) COSTOPR = COSTOPR + FEOPR_GAS(2,RR,curiyr-techyr+1)/1000000. &
    * mc_jpgdp(yrCstTbl-baseyr+1)/mc_jpgdp(-2)     ! FE TECHNOLOGY ADJUSTMENT

! OPERATING COST TECHNOLOGY
! => REDUCES OPERATING COSTS
TT = INT( LEVOPRCST(2))
IF (T1.LT.TT) TT = T1
! TOTAL IMPROVEMENT FOR THE YEAR (FRACTION)
IRATE = TT * ( LEVOPRCST(1)/ LEVOPRCST(2))/100.
COSTOPR = COSTOPR*(1.-IRATE)
IF (COSTOPR.LT.0) COSTOPR = 0.

! CHANGE OPERATING COST AS A FUNCTION OF OIL PRICE
OPRCOST = COSTOPR * (1. + (OILPRICE(iOGSM,TOFF)/ OILPRCCSTTbl - 1.)* CHGOPCSTOIL)*(1.+ogrunop(20)/100.)

!WRITE(offout,100) 'oprcost [iPA,iEU,DD,T1,CostOpr,OprCost]', nmEU(iPA,iEU), WD, T1, COSTOPR, OPRCOST*MC_JPGDP( 2015-BASEYR+1)/MC_JPGDP(yrCstTbl-BASEYR+1)
!100 FORMAT(A44,2X,A15,F8.2,I5,2F8.2)

RETURN
END FUNCTION OPRCOST

!************************************************************************************!
!
!************************************************************************************!
REAL*4 FUNCTION DEVLCOST(iPA,iEU,WD,DD,T1)
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ncntrl'       ! nems control variables
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmbfw'
INCLUDE 'ogsmoff'
INCLUDE 'macout'
INCLUDE 'dwpparm'

INTEGER iPA
INTEGER iEU
INTEGER T1
REAL*4 WD
REAL*4 DD

INTEGER T2
INTEGER iREG, iOGSM
INTEGER II, JJ, TT, RR
REAL*4 COSTDEVL, COSTCOMPL, iRATE
REAL*4 PHASEYR, cstescalator

DATA PHASEYR/4./

! GET REGION INDEX
iREG =  PAREG(iPA)
iOGSM =  OGSMREG(iPA,iEU)

! DETERMINE DEVELOPMENT DRILLING COSTS
! Costs updated perfunctorily December 2015 based on IHS study - needs a more detailed look
!  Basically, looked at the costs IHS cited for GOM drilling & completion costs by water depth and well depth
!  and applied multiplicative factors to these equations to get our costs to line up with the report

IF (WD*0.3048.LT.900.) THEN
    II = 1
ELSE
    II = 2
ENDIF

!COSTDEVL = (ALPHA_DDC(II) + (BETA1_DDC(II) + BETA2_DDC(II)*DD)*WD + (GAMMA1_DDC(II)*DD + GAMMA2_DDC(II))*DD) / 1000000.		!old
COSTDEVL = 5 * (ALPHA_DDC(II) + (BETA1_DDC(II) + BETA2_DDC(II)*DD)*WD + (GAMMA1_DDC(II)*DD + GAMMA2_DDC(II))*DD) / 1000000.	!based on IHS study

! DETERMINE COMPLETION COSTS
IF (DD.LT.10000.) THEN
    JJ = 1
ELSEIF (DD.LT.20000.) THEN
    JJ = 2
ELSE
    JJ = 3
ENDIF

!COSTCOMPL = COMPLCOST(II,JJ) / 1000000. ! old
COSTCOMPL = 5 * COMPLCOST(II,JJ) / 1000000. ! based on IHS study
!WRITE(OFFOUT,100) 'devlcost [iPA,iEU,WD,DD,T1,CostDevl,CostCompl]', nmEU(iPA,iEU), WD, DD, T1, COSTDEVL, COSTCOMPL

! TOTAL DEVELOPMENT AND COMPLETION COSTS
COSTDEVL = COSTDEVL + COSTCOMPL

RR = 8
IF (iOGSM.EQ.2) RR = 9
IF (OGRUNOP(2).eq.11.AND.curiyr.ge.techyr) COSTDEVL = COSTDEVL + FEDRILL_GAS(1,RR,curiyr-techyr+1)/1000000.  &
    * mc_jpgdp(yrCstTbl-baseyr+1)/mc_jpgdp(-2)     ! FE TECHNOLOGY ADJUSTMENT
IF (OGRUNOP(2).eq.12.AND.curiyr.ge.techyr) COSTDEVL = COSTDEVL + FEDRILL_GAS(2,RR,curiyr-techyr+1)/1000000.  &
    * mc_jpgdp(yrCstTbl-baseyr+1)/mc_jpgdp(-2)     ! FE TECHNOLOGY ADJUSTMENT

! DRILLING COST TECHNOLOGY
! => REDUCES DRILLING COSTS
TT = INT( LEVDRLCST(2))
IF (T1.LT.TT) TT = T1
! TOTAL IMPROVEMENT FOR THE YEAR (FRACTION)
IRATE = TT * ( LEVDRLCST(1)/ LEVDRLCST(2))/100.
COSTDEVL = COSTDEVL*(1.-IRATE)
IF (COSTDEVL.LT.0) COSTDEVL = 0.

! CHANGE DRILLING COST AS A FUNCTION OF OIL PRICE
IF (OILPRICE(iOGSM,TOFF)/ OILPRCCSTTbl.le.4.5) THEN
    DEVLCOST = COSTDEVL * (1. + (OILPRICE(iOGSM,TOFF)/ OILPRCCSTTbl - 1.)* CHGDRLCSTOIL)
ELSE
    DEVLCOST = COSTDEVL * (1. + (OILPRICE(iOGSM,TOFF)/ OILPRCCSTTbl - 1.)* CHGDRLCSTOIL*2.)
ENDIF

cstescalator = 0.30
IF (curiyr.ge.l48hyr+2.and.phaseyr.gt.0) THEN  ! adjust capital cost escalator
    t2 = curiyr-(l48hyr+2)+1
    IF (t2.gt.phaseyr) t2 = phaseyr
    IF (ogrunop(17).eq.1) cstescalator = cstescalator*(1.-(t2/phaseyr))  ! phase out escalator
    IF (ogrunop(17).eq.3) cstescalator = cstescalator*(1.+(t2/phaseyr))  ! double cost escalator over phase years
ENDIF
DEVLCOST = DEVLCOST * (1. + cstescalator) * (1.+ogrunop(20)/100.)

!WRITE(OFFOUT,100) 'devlcost [iPA,iEU,WD,DD,T1,CostDevl,DevlCost]', nmEU(iPA,iEU), WD, DD, T1, COSTDEVL*MC_JPGDP( 2015-BASEYR+1)/MC_JPGDP(yrCstTbl-BASEYR+1), DEVLCOST*MC_JPGDP( 2015-BASEYR+1)/MC_JPGDP(yrCstTbl-BASEYR+1)
!100 FORMAT(A44,2X,A15,2F8.0,I4,2F8.2)

RETURN
END FUNCTION DEVLCOST

!************************************************************************************!
!
!************************************************************************************!
REAL*4 FUNCTION EXPLCOST(iPA,iEU,WD,DD,T1)
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ncntrl'       ! nems control variables
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmbfw'
INCLUDE 'ogsmoff'
INCLUDE 'macout'
INCLUDE 'dwpparm'

INTEGER iPA
INTEGER iEU
INTEGER T1
REAL*4 WD
REAL*4 DD

INTEGER T2
INTEGER iREG, iOGSM
INTEGER TT, II, RR
REAL*4 COSTEXPL, iRATE
REAL*4 PHASEYR, cstescalator

DATA PHASEYR/4./


! GET REGION INDEX
iREG =  PAREG(iPA)
iOGSM =  OGSMREG(iPA,iEU)

! EXPLORATORY DRILLING COST

IF (WD*0.3048.LT.200.) THEN                  ! JACK-UP RIG
    II = 1
ELSEIF (WD*0.3048.LT.900.) THEN              ! SEMI-SUBMERSIBLE RIG
    II = 2
ELSE                                        ! DRILL SHIP
    II = 3
ENDIF
COSTEXPL = (ALPHA_EDC(II) + BETA1_EDC(II)*WD + BETA2_EDC(II)*(WD+DD) + BETA3_EDC(II)*WD*DD**POWER_EDC(II)) / 1000000.

RR = 8
IF (iOGSM.EQ.2) RR = 9
IF (OGRUNOP(2).eq.11.AND.curiyr.ge.techyr) COSTEXPL = COSTEXPL + FEDRILL_GAS(1,RR,curiyr-techyr+1)/1000000.  &
    * mc_jpgdp(yrCstTbl-baseyr+1)/mc_jpgdp(-2)     ! FE TECHNOLOGY ADJUSTMENT
IF (OGRUNOP(2).eq.12.AND.curiyr.ge.techyr) COSTEXPL = COSTEXPL + FEDRILL_GAS(2,RR,curiyr-techyr+1)/1000000.  &
    * mc_jpgdp(yrCstTbl-baseyr+1)/mc_jpgdp(-2)     ! FE TECHNOLOGY ADJUSTMENT

! DRILLING COST TECHNOLOGY
! => REDUCES DRILLING COSTS
TT = INT( LEVDRLCST(2))
IF (T1.LT.TT) TT = T1
! TOTAL IMPROVEMENT FOR THE YEAR (FRACTION)
IRATE = TT * ( LEVDRLCST(1)/ LEVDRLCST(2))/100.
COSTEXPL = COSTEXPL*(1.-IRATE)
IF (COSTEXPL.LT.0) COSTEXPL = 0.

! CHANGE DRILLING COST AS A FUNCTION OF OIL PRICE
IF (OILPRICE(iOGSM,TOFF)/ OILPRCCSTTbl.le.4.5) THEN
    EXPLCOST = COSTEXPL * (1. + (OILPRICE(iOGSM,TOFF)/ OILPRCCSTTbl - 1.)* CHGDRLCSTOIL)
ELSE
    EXPLCOST = COSTEXPL * (1. + (OILPRICE(iOGSM,TOFF)/ OILPRCCSTTbl - 1.)* CHGDRLCSTOIL*2.)
ENDIF

cstescalator = 0.15
if (curiyr.ge.l48hyr+2.and.phaseyr.gt.0) then  ! adjust capital cost escalator
    t2 = curiyr-(l48hyr+2)+1
    if (t2.gt.phaseyr) t2 = phaseyr
    if (ogrunop(17).eq.1) cstescalator = cstescalator*(1.-(t2/phaseyr))  ! phase out escalator
    if (ogrunop(17).eq.3) cstescalator = cstescalator*(1.+(t2/phaseyr))  ! double cost escalator over phase years
endif
EXPLCOST = EXPLCOST * (1. + cstescalator) * (1.+ogrunop(20)/100.)

!WRITE(OFFOUT,100) 'explcost [EU,WD,DD,II,T1,CostExpl,ExplCost]', nmEU(iPA,iEU), WD, DD, II, T1, COSTEXPL, EXPLCOST
!100 FORMAT(A44,2X,A15,2F8.0,2I4,2F8.2)

RETURN
END FUNCTION EXPLCOST

!************************************************************************************!
!
!************************************************************************************!
SUBROUTINE CREATEEXPLORATIONPROJECT(PRJ, exp_run_bool)
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ncntrl'       ! nems control variables
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmbfw'
INCLUDE 'ogsmoff'
INCLUDE 'dwpparm'

TYPE (PROJECTOBJ), INTENT(INOUT) :: PRJ
logical          , INTENT(INOUT) :: exp_run_bool ! is this the actual exploratory calculation (then true)


INTEGER PFDELAY1, PFDELAY2, DEVDRILLPERYEAR
INTEGER iPA, iEU, iFSC, t1, tbeg, tend,tt,nwls,ndrl,n0
INTEGER nf, nfpf, iECON, FF, ttt, nfl, ireg, iogsm
INTEGER tbld(maxNFields), pfnwlsarr(maxNFields), pfnfld(maxNFields)
INTEGER fnwls(maxNFields), tdecOil(maxNFields), tdecGas(maxNFields)
INTEGER nextint
REAL*4 wD, dD, costPF, costFL, fOilRes, fGasRes, qo, qg, qad, abandoncst
REAL*4 qcnd, v1, v2, v3, netrevenue, statetax, fedtax, econlimit
REAL*4 irate1, irate2, adjfac, tcstcap
REAL*4 foilcum(maxNFields), fgascum(maxNFields)
REAL*4 EXPLCOST, DEVLCOST, OPRCOST, PFCOST, FLOWLINECOST

REAL*4 prj_pfCst(MNUMYR)              ! Production facility cost (MM$) schedule by time index
REAL*4 prj_GRev(MNUMYR)               ! Gross revenue (MM$) by time incex
REAL*4 prj_ROY(MNUMYR)                ! Royalties (MM$) by time index
REAL*4 prj_NRev(MNUMYR)               ! Net revenue (MM$) by time index
REAL*4 prj_STax(MNUMYR)               ! Severance tax (MM$) by time index
REAL*4 prj_OCst(MNUMYR)               ! Operating cost (MM$) by time index
REAL*4 prj_TCst(MNUMYR)               ! Transportation cost (MM$) by time index
REAL*4 prj_Inv(MNUMYR)                ! Investment (MM$) by time index
REAL*4 prj_TInv(MNUMYR)               ! Tangible investment (MM$) by time index
REAL*4 prj_IInv(MNUMYR)               ! Intangible investment (MM$) by time index
REAL*4 prj_BTCF(MNUMYR)               ! Before tax cashflow (MM$) by time index
REAL*4 prj_DBas(MNUMYR)               ! Depreciation base (MM$) by time  index
REAL*4 prj_DExp(MNUMYR)               ! Depreciation expense (MM$) by time index
REAL*4 prj_EInv(MNUMYR)               ! Expensed investments (MM$) by time index
REAL*4 prj_NIBT(MNUMYR)               ! Net income before taxes (MM$) by time index
REAL*4 prj_SITax(MNUMYR)              ! State income taxes (MM$) by time index
REAL*4 prj_FTInc(MNUMYR)              ! Federal taxable income (MM$) by time index
REAL*4 prj_FITax(MNUMYR)              ! Federal income taxes (MM$) by time index
REAL*4 prj_NIAT(MNUMYR)               ! Net income after taxes (MM$) by time index
REAL*4 prj_ATCF(MNUMYR)               ! After tax cashflow (MM$) by time index
REAL*4 prj_ACST(MNUMYR)               ! Abandonment Costs (MM$) by time index
REAL*4 prj_prdCum(minFSC:minFSC+numFSC-1,MNUMYR)        ! Cumulative production (BOE) by FSC, time index
REAL*4 prj_prdRev(minFSC:minFSC+numFSC-1,MNUMYR)        ! Revenue (MM$) schedule from production by FSC,
                                ! time index
REAL*4 prj_trnCst(minFSC:minFSC+numFSC-1,MNUMYR)        ! Transportation cost (MM$) schedule by FSC, time index
REAL*4 prj_devDrlCst(minFSC:minFSC+numFSC-1,MNUMYR)          ! Development drilling cost (MM$) schedule by FSC,
                                ! time index
REAL*4 prj_oilPrd(minFSC:minFSC+numFSC-1,MNUMYR)             ! Oil production (Bbl) by FSC, time index
REAL*4 prj_gasPrd(minFSC:minFSC+numFSC-1,MNUMYR)             ! Non-associated Gas production (Mcf) by FSC,
                                ! time index
REAL*4 prj_cndPrd(minFSC:minFSC+numFSC-1,MNUMYR)             ! Condensate production (Bbl) by FSC, time index
REAL*4 prj_asgPrd(minFSC:minFSC+numFSC-1,MNUMYR)             ! Associated gas production (Mcf) by FSC, time index

INTEGER prj_fNwls(minFSC:minFSC+numFSC-1,MaxNFields,MNUMYR)  ! Number of wells by FSC, field counter, time index
INTEGER prj_nOprWls(minFSC:minFSC+numFSC-1,MNUMYR)         ! Number of operating wells by FSC, time index
INTEGER prj_devDrlSch(minFSC:minFSC+numFSC-1,MNUMYR)       ! Development drilling schedule (number of wells/yr)
! by FSC, time index

!==========================> HSM Code Start <==========================
real hsm_npv
integer hsm_fsc
real hsm_amor(MNUMYR)
real hsm_depr(MNUMYR)
hsm_npv         = 0.0
!===========================> HSM Code End <===========================

!WRITE(OFFOUT,*) 'ENTERING SUBROUTINE CreateExplorationProject, YEAR =', yearof(toff)

!prj%f = 0 apr 01/30/2020, this should already have been init in INITPROJECT
prj%fNwls = 0
prj_fNwls = 0

iPA = prj%PA
iReg = PAREG(iPA)
iEU = prj%EU
iOGSM = OGSMREG(iPA,iEU)


! COMMON WATER DEPTH AND DRILLING DEPTH
wD =  wDpth(iPA, iEU, prj%biggestFSC)
dD =  dDpth(iPA, iEU, prj%biggestFSC)
prj%wDpth = wD
prj%dDpth = dD


prj%pfNWls = 0  ! Total number of wells/slots for the projects
DO iFSC = minFSC,minFSC+numFSC-1
    IF (prj%nFld(iFSC).GT.0) THEN

        ! Number of exploration wells = (1/success rate + delineation wells) * number of fields
        prj%expDrlNWls(iFSC) = (nextInt(1. /  expSucRate(iPA, iEU, iFSC)) + &
            nDelWls(iPA, iEU, iFSC)) * prj%nFld(iFSC)

        ! Number of dry exploration wells (only one exploratory well is successful, others are dry)
        prj%expDryNWls(iFSC) = (nextInt(1. /  expSucRate(iPA, iEU, iFSC)) - 1) * prj%nFld(iFSC)

        ! Number of delineation wells
        prj%delDrlNWls(iFSC) =  nDelWls(iPA, iEU, iFSC) * prj%nFld(iFSC)

        ! Exploration drilling cost (MM$ at time=t) = Cost per well * number of wells
        prj%expDrlCst(iFSC) = ExplCost(iPA, iEU,  wDpth(iPA, iEU, iFSC),  dDpth(iPA, iEU, iFSC), toff) * &
            prj%expDrlNWls(iFSC)


        ! Delineation drilling cost (MM$ at time=t) = Cost per well * number of wells
        prj%delDrlCst(iFSC) = ExplCost(iPA, iEU,  wDpth(iPA, iEU, iFSC),  dDpth(iPA, iEU, iFSC), toff) * &
            prj%delDrlNWls(iFSC)

        ! Total number of wells/slots
        prj%pfNWls = prj%pfNWls +  nDevWls(iPA, iEU, iFSC) * prj%nFld(iFSC)

        ! Total number of development wells to be drilled by FSC
        prj%devWlsDrl(iFSC) = ( nDevWls(iPA, iEU, iFSC) -  nDelWls(iPA, iEU, iFSC)) * &
            prj%nFld(iFSC)
        IF (prj%devWlsDrl(iFSC).LT.0) prj%devWlsDrl(iFSC) = 0

    ENDIF
ENDDO

! Number of production facilities (max. 60 wells/PF)
prj%nPf = nextInt(prj%pfNWls / 60.)

! Number of wells for each production facility
n = 0
DO i = 1,prj%nPf
    IF ((n + 60).LE.prj%pfNWls) THEN
        prj%pfNWlsArr(i) = 60
        n = n + 60
    ELSE
        prj%pfNWlsArr(i) = prj%pfNWls - n
    ENDIF
    ! pfNWlsArr(i) will be used for number of wells to be drilled from production facility
    pfNWlsArr(i) = prj%pfNWlsArr(i)
ENDDO

! Number of fields for each production facility
i = 1
n = 0
nWls = 0
nf = 0
nfpf = 0
DO iFSC = minFSC,minFSC+numFSC-1
    IF (prj%nFld(iFSC).GT.0) THEN
        nf = nf + prj%nFld(iFSC)  ! Count total . of fields
        DO f = 1,prj%nFld(iFSC)
            nWls = nWls +  nDevWls(iPA, iEU, iFSC) ! Count # of wells
            n = n + 1  ! Count # of fields
            IF (nWls.GE.prj%pfNWlsArr(i)) THEN
                pfNFld(i) = n  ! Store number of fields in this production facility
                nfpf = nfpf + n ! Count total # of fields assigned to production facility
                i = i + 1
                n = 0
                nWls = 0
            ENDIF
        ENDDO
    ENDIF
ENDDO

! For the last production facility
IF (n.GT.0.And.i.EQ.prj%nPf) THEN
    pfNFld(i) = n  ! Store number of fields in this production facility
    nfpf = nfpf + n ! Count total # of fields assigned to production facility
ENDIF

! Check balance (dump to first production facility)
IF (nfpf.GT.nf) THEN
    pfNFld(1) = pfNFld(1) - (nfpf - nf)
    IF (pfNFld(1).LT.1) pfNFld(1) = 1
ELSEIF (nfpf.LT.nf) THEN
    pfNFld(1) = pfNFld(1) + (nf - nfpf)
ENDIF

! Production facility type array
IF (prj%nPf.GT.1) THEN
    ! A production facility
    IF (prj%pfTyp(1).NE.7) THEN
        DO i = 2,prj%nPf
            ! Use subsea system if remaining number of wells <= 18
            IF (prj%pfNWlsArr(i).LE.18) THEN
                prj%pfTyp(i) = 7
            ELSE
                prj%pfTyp(i) = prj%pfTyp(1)
            ENDIF
        ENDDO
        ! A subsea system
    ELSE
        DO i = 2,prj%nPf
            prj%pfTyp(i) = 7
        ENDDO
    ENDIF
ENDIF

! Start year of exploration
prj%yrExpStart = toff

! Start year of development:
n = toff
prj%yrDevStart = toff

! Year index when a production facility is available
DO i = 1,prj%nPf
    IF (i.GT.1) THEN
        ! pfDelay2 is the time to wait before construction of another production facility
        ! (use common water depth)
        n = n + pfDelay2(wD)
    ENDIF
    ! pfDelay1 is the time to construct a production facility (use common water depth)
    tbld(i) = pfDelay1(prj%pfTyp(i), wD, prj%pfNWlsArr(i), n)
    n = n + tbld(i)
    prj%pfTimeAvl(i) = n + 1
ENDDO

! Check if the project will produce within the projection period
n = prj%nPf
! DO i = 1,prj%nPf
!   IF (prj%pfTimeAvl(i) .GT. nReportTime) n = n - 1
! ENDDO
IF (n .EQ. 0) THEN
    prj%expNPV = -1.
    prj%devNPV = -1.
    RETURN
ENDIF
prj%nPf = n

! Production facility cost schedule (MM$/Year)
! ReDim prj%pfCst(1 To nTime) As Double
prj_pfCst = 0.

DO i = 1,prj%nPf
    ! Calculate production facility cost
    tt = prj%pfTimeAvl(i) - tbld(i)
    costPF = pfCost(iPA, iEU, prj%biggestFSC, wD, prj%pfTyp(i), prj%pfNWlsArr(i), tt)
    abandoncst = costPF * ABANDONFAC(prj%pfTyp(i))

    ! Add flowline cost (if any):
    ! - Project types: SS-PF and SS
    nFL = 0
    IF (prj%Typ .GT. 1) THEN
        nWls =  pfMaxNWls(prj%Typ) ! Maximum number of wells sharing a flowline
        nFL = nextInt(pfNWlsArr(i)*1.0 / nWls)  ! Number of flowlines
        IF (nFL .LT. pfNFld(i)) nFL = pfNFld(i)  ! it should be at least number of fields for the production facility
        ! - Project type PF
    ELSE
        IF (prj%pfTyp(i) .EQ. 7) THEN
            nWls =  pfMaxNWls(2) ! Maximum number of wells sharing a flowline (use SS-PF number)
            nFL = nextInt(pfNWlsArr(i)*1.0 / nWls)  ! Number of flowlines
            IF (nFL .LT. pfNFld(i)) nFL = pfNFld(i)  ! it should be at least number of fields for the production facility
        ENDIF
    ENDIF
    prj%nFlowline(i) = nFL  ! Store number of flowlines

    ! Calculate flowline cost
    ! IF (nFL .GT. 0) THEN
        costFL = FlowlineCost(iPA, iEU, wD, tt) * nFL
        ! Add flowline cost to the production facility cost
        costPF = costPF + costFL
    ! ENDIF

    ! Spread cost based on pfCstFrc
    tbeg = prj%pfTimeAvl(i) - tbld(i)
    tend = prj%pfTimeAvl(i) - 1
    IF (tend .GT. nTime)  tend = nTime
    tt = 0
    !   DO t1 = tbeg,tend
    !     tt = tt + 1
    !     prj_pfCst(t1) = prj_pfCst(t1) + costPF *  pfCstFrc(tbld(i), tt)
    !   ENDDO
    DO t1 = 1,tbld(i)
        prj_pfCst(t1) = prj_pfCst(t1) + costPF *  pfCstFrc(tbld(i), t1)
    ENDDO
ENDDO

! Development drilling and cost schedules (MM$/Year)
! ReDim prj%devDrlSch(5 To 20, 1 To nTime) As Integer
! ReDim prj%devDrlCst(5 To 20, 1 To nTime) As Double
prj%devDrlSch = 0
prj_devDrlSch = 0
prj_devDrlCst = 0.

! Convert delineation wells to production wells

DO iFSC = minFSC+numFSC-1,minFSC,-1
    IF (prj%nFld(iFSC) .GT. 0) THEN
        ! Number of delineation wells to be converted in the FSC
        IF (prj%devWlsDrl(iFSC) .GT. 0) THEN
            nWls =  nDelWls(iPA, iEU, iFSC) * prj%nFld(iFSC)
        ELSE
            nWls =  nDevWls(iPA, iEU, iFSC) * prj%nFld(iFSC)
        ENDIF
        IF (nWls .EQ. 0) EXIT
        DO i = 1,prj%nPf
            IF (pfNWlsArr(i) .GT. 0.And.nWls .GT. 0) THEN
                n = pfNWlsArr(i)
                pfNWlsArr(i) = pfNWlsArr(i) - nWls
                !         t1 = prj%pfTimeAvl(i)  - toff + 1
                t1 = tbld(i)+1
                IF (pfNWlsArr(i) .GE. 0) THEN
                    ! Make them to come online at the end of production facility construction
                    prj_devDrlSch(iFSC, t1) = prj_devDrlSch(iFSC, t1) + nWls
                    nWls = 0
                ELSE
                    prj_devDrlSch(iFSC, t1) = prj_devDrlSch(iFSC, t1) + n
                    nWls = -pfNWlsArr(i)
                    pfNWlsArr(i) = 0
                ENDIF
            ENDIF
        ENDDO
        IF (nWls .GT. 0) WRITE(offout,*) 'WARNING:  SUBROUTINE CreateExplorationProject: Problem converting delineation wells to production wells'
    ENDIF
ENDDO

! Drill bigger FSC first
DO iFSC = minFSC+numFSC-1,minFSC,-1
    IF (prj%nFld(iFSC) .GT. 0) THEN

        ! No drilling required (could be due to delineation wells - converted to production wells)
        IF (prj%devWlsDrl(iFSC).EQ.0) EXIT

        nWls = 0  ! Number of development wells drilled in the FSC
        DO i = 1,prj%nPf

            ! Maximum number of development wells could be drilled in a year
            n = devDrillPerYear(prj%pfTyp(i), prj%pfNWlsArr(i),  wDpth(iPA, iEU, iFSC), &
                dDpth(iPA, iEU, iFSC))
            ! For SS, FPS, or FPSO, multiply by number of fields in the FSC
            IF (.Not.  platform(prj%pfTyp(i))) n = n * prj%nFld(iFSC)

            ! Create development drilling schedule
            ndrl = 0
            !       DO t1 = prj%pfTimeAvl(i)-toff+1,IJUMPYR
            DO t1 = tbld(i)+1,IJUMPYR

                ! No more drilling capacity for this production facility
                IF (pfNWlsArr(i).EQ.0) EXIT

                ! Determine number of wells to be drilled at time t1
                ndrl = ndrl + n  ! Drilling rate/year
                IF (ndrl .GT. pfNWlsArr(i)) ndrl = pfNWlsArr(i)  ! Constraint: PF drilling capacity
                ! Constraint: # of wells that could be drilled in FSC
                IF ((nWls + ndrl) .GT. prj%devWlsDrl(iFSC)) ndrl = prj%devWlsDrl(iFSC) - nWls

                ! Drill the development wells (drilling schedule)
                prj_devDrlSch(iFSC, t1) = prj_devDrlSch(iFSC, t1) + ndrl

                ! Development drilling cost schedule
                prj_devDrlCst(iFSC, t1) = DevlCost(iPA, iEU,  wDpth(iPA, iEU, iFSC), &
                    dDpth(iPA, iEU, iFSC), t1+toff-1) * ndrl

                ! Remaining drilling capacity
                pfNWlsArr(i) = pfNWlsArr(i) - ndrl

                ! Total number of development wells drilled currently in the FSC
                nWls = nWls + ndrl

                ! All wells in the FSC have been drilled
                IF (nWls.EQ.prj%devWlsDrl(iFSC)) EXIT

            ENDDO

        ENDDO

    ENDIF
ENDDO

! Production profile
! ReDim prj%nOprWls(5 To 20, 1 To nTime) As Integer
! ReDim prj%oilPrd(5 To 20, 1 To nTime) As Double
! ReDim prj%gasPrd(5 To 20, 1 To nTime) As Double
! ReDim prj%cndPrd(5 To 20, 1 To nTime) As Double
! ReDim prj%asgPrd(5 To 20, 1 To nTime) As Double
prj_nOprWls = 0
prj_oilPrd = 0.
prj_gasPrd = 0.
prj_cndPrd = 0.
prj_asgPrd = 0.
prj%nOprWls = 0
prj%oilPrd = 0.
prj%gasPrd = 0.
prj%cndPrd = 0.
prj%asgPrd = 0.

! Start from bigger FSC first (does not matter)
DO iFSC = minFSC+numFSC-1,minFSC,-1

    IF (prj%nFld(iFSC) .GT. 0) THEN

        ! Reset field number of wells
        fNwls = 0
        ! Reset start decline time index
        tdecOil = 0
        tdecGas = 0

        DO f = 1,prj%nFld(iFSC)
            tdecOil(f) = 1
            tdecGas(f) = 1
        ENDDO

        ! Reset field cumulative oil and gas produced
        fOilCum = 0.
        fGasCum = 0.

        ! Field initial oil (Bbl) and gas (Mcf) resources
        fOilRes =  vMean(iFSC) * 1000000. * (1. -  GOprop(iPA, iEU))
        IF (fOilRes .LT. 0.001) fOilRes = 0.001
        fGasRes = ( vMean(iFSC) * 1000000. *  GOprop(iPA, iEU)) *  BOEtoMcf
        IF (fGasRes .LT. 0.001) fGasRes = 0.001

        ! Wells coming online loop
        f = 0
        !     DO t1 = toff,nTime
        DO t1 = 1, IJUMPYR

            nWls = prj_devDrlSch(iFSC, t1)  ! Number of wells come online at this time

            ! Store number of operating wells
            n = 0
            IF (t1 .GT. 1) n = prj_nOprWls(iFSC, t1 - 1)
            prj_nOprWls(iFSC, t1) = n + nWls
            IF (t1 .GT. 1) THEN
                DO FF = 1,prj%nFld(iFSC)
                    prj_fNwls(iFSC, FF, t1) = prj_fNwls(iFSC, FF, t1 - 1)
                ENDDO
            ENDIF

            IF (nWls .GT. 0) THEN

                ! Distribute wells to the fields
                n = 0
                DO WHILE (.True.)
                    ! Field index counter (remember the last field for next t1 loop)
                    f = f + 1
                    IF (f .GT. prj%nFld(iFSC)) f = 1
                    fNwls(f) = fNwls(f) + 1  ! Add one well to a field
                    ! Store number of operating wells by field
                    prj_fNwls(iFSC, f, t1) = prj_fNwls(iFSC, f, t1) + 1
                    n = n + 1  ! Number of wells that have beed assigned to the fields
                    IF (n .EQ. nWls)  EXIT
                ENDDO
            ENDIF
        ENDDO

        ! Production profile loop
        !     DO tt = toff,nTime
        DO tt = 1, IJUMPYR

            ! Produce the field
            DO f = 1,prj%nFld(iFSC)

                IF (prj_fNwls(iFSC,f,tt) .GT. 0) THEN
                    fNwls(f) = prj_fNwls(iFSC, f, tt)

                    ! Completion technology 1 => increases initial production rate
                    ttt = Int( levPrdPerf1(2))
                    IF (tt + toff -1.LT. ttt)  ttt = tt + toff -1
                    ! Total improvement for the year (Fraction)
                    iRate1 = ttt * ( levPrdPerf1(1) /  levPrdPerf1(2)) / 100.

                    ! Completion technology 2 => reduces decline rate
                    ttt = Int( levPrdPerf2(2))
                    IF (tt + toff -1.LT. ttt)  ttt = tt + toff -1
                    ! Total improvement for the year (Fraction)
                    iRate2 = ttt * ( levPrdPerf2(1) /  levPrdPerf2(2)) / 100.

                    ! Oil production rate (Bbl/Field/Year)
                    !           v1 =  prdOilRatei(iPA, iEU) * (1 + iRate1) * (iFSC/(minFSC+10.))**2
                    !           v1 =  prdOilRatei(iPA, iEU) * (1 + iRate1) * ((iFSC)/(15.))**2
                    !           v1 =  prdOilRatei(iPA, iEU) * (1 + iRate1) * ((iFSC)/(17.))**1
                    v1 =  prdOilRatei(iPA, iEU) * (1 + iRate1)
                    v2 =  prdOilDecRatei(iPA, iEU) * (1 - iRate2)
                    IF (v2 .LT. 0.) v2 = 0.
                    IF (fOilCum(f) / fOilRes .LT.  prdOilFrc(iPA, iEU)) THEN
                        qo = v1 * 365.
                        tdecOil(f) = tt
                    ELSE
                        qo = v1 * 365. * (1. + v2 *  prdOilHyp(iPA, iEU) * (tt - tdecOil(f))) &
                            ** (-1.0 / prdOilHyp(iPA, iEU))
                    ENDIF
                    !           qo = qo * fNwls(f) * (1.-GOProp(iPA,iEU))
                    qo = qo * fNwls(f)

                    ! Cumulative oil produced
                    IF ((fOilCum(f) + qo) .GT. fOilRes) qo = fOilRes - fOilCum(f)
                    fOilCum(f) = fOilCum(f) + qo

                    ! Non-associated gas production rate (Mcf/Field/Year)
                    !           v1 =  prdGasRatei(iPA, iEU) * (1 + iRate1) * (iFSC/(minFSC+10.))**2
                    !           v1 =  prdGasRatei(iPA, iEU) * (1 + iRate1) * ((iFSC)/(15.))**2
                    !           v1 =  prdGasRatei(iPA, iEU) * (1 + iRate1) * ((iFSC)/(17.))**1
                    v1 =  prdGasRatei(iPA, iEU) * (1 + iRate1)
                    v2 =  prdGasDecRatei(iPA, iEU) * (1 - iRate2)
                    IF (v2 .LT. 0.) v2 = 0.
                    IF (fGasCum(f) / fGasRes .LT.  prdGasFrc(iPA, iEU)) THEN
                        qg = v1 * 365.
                        tdecGas(f) = tt
                    ELSE
                        qg = v1 * 365. * (1. + v2 *  prdGasHyp(iPA, iEU) * (tt - tdecGas(f))) &
                            ** (-1.0 / prdGasHyp(iPA, iEU))
                    ENDIF
                    !           qg = qg * fNwls(f) * GOProp(iPA,iEU)
                    qg = qg * fNwls(f)

                    ! Cumulative gas production
                    IF ((fGasCum(f) + qg) .GT. fGasRes) qg = fGasRes - fGasCum(f)
                    fGasCum(f) = fGasCum(f) + qg

                    ! Associated gas portion from oil (Mcf/Field/Year) (GOR=Scf/Bbl)
                    qad = qo *  GOR(iPA, iEU) / 1000.
                    v1 = qad /  BOEtoMcf
                    ! This is a gas field
                    IF (v1 .GT. qo) qad = qo *  BOEtoMcf

                    ! Condensate portion from gas (Bbl/Field/Year)
                    ! (Condensate Yield=Bbl/MMcf)
                    adjfac = 1.0
                    !           IF (oit_wop(IJUMPYR,1).gt.25.) adjfac = 2.
                    !           IF (oit_wop(IJUMPYR,1).lt.18.) adjfac = 0.5
                    qcnd = (qg / 1000.) *  cndYld(iPA, iEU) * adjfac
                    v1 = qcnd *  BOEtoMcf

                    ! This is an oil field
                    IF (v1 .GT. qg) qcnd = qg /  BOEtoMcf

                    ! Remove associated gas from oil (Bbl/Field/Year)
                    qo = qo - qad /  BOEtoMcf
                    IF (qo .LT. 0.) qo = 0.

                    ! Remove condensate from non-associated gas (Mcf/Field/Year)
                    qg = qg - qcnd *  BOEtoMcf
                    IF (qg .LT. 0.) qg = 0.

                    ! Store oil field production
                    prj_oilPrd(iFSC, tt) = prj_oilPrd(iFSC, tt) + qo

                    ! Store non-associated gas field production
                    prj_gasPrd(iFSC, tt) = prj_gasPrd(iFSC, tt) + qg

                    ! Store associated gas field production
                    prj_asgPrd(iFSC, tt) = prj_asgPrd(iFSC, tt) + qad

                    ! Store condensate field production
                    prj_cndPrd(iFSC, tt) = prj_cndPrd(iFSC, tt) + qcnd

                    !           WRITE(OFFOUT,50) 'FLDPRD',nmEU(iPA,iEU),iFSC,yearof(toff),tt,fnwls(f),qo,qg,qad,qcnd,foilcum(f),foilres,fgascum(f),fgasres
50                  FORMAT(A8,2X,A15,I3,3I5,8F12.0)

                ENDIF
            ENDDO
        ENDDO

    ENDIF
ENDDO

! Calculate transportation cost and set cost schedule (MM$/Year)
! ReDim prj%trnCst(5 To 20, 1 To nTime) As Double
prj_trnCst = 0.
DO iFSC = minFSC,minFSC+numFSC-1
    IF (prj%nFld(iFSC) .GT. 0) THEN
        !         DO t1 = toff,nTime
        DO t1 = 1,IJUMPYR
            prj_trnCst(iFSC, t1) = ( &
                (prj_oilPrd(iFSC, t1) + prj_cndPrd(iFSC, t1)) *  trnTrfOil(iPA, iEU) + &
                (prj_gasPrd(iFSC, t1) + prj_asgPrd(iFSC, t1)) *  trnTrfGas(iPA, iEU)) &
                / 1000000.
        ENDDO
    ENDIF
ENDDO
! Calculate revenue from production and set revenue schedule (MM$/Year)
!     ReDim prj%prdRev(5 To 20, 1 To nTime) As Double
prj_prdRev = 0.
DO iFSC = minFSC,minFSC+numFSC-1
    IF (prj%nFld(iFSC) .GT. 0) THEN
        !         DO t1 = toff,nTime
        DO t1 = 1, IJUMPYR
            prj_prdRev(iFSC, t1) = (prj_oilPrd(iFSC, t1) *  oilPrice(iOGSM,toff) + &
                (prj_gasPrd(iFSC, t1) + prj_asgPrd(iFSC, t1)) *  gasPrice(iOGSM,toff) + &
                prj_cndPrd(iFSC, t1) *  oilPrice(iOGSM,toff)) / 1000000.
            IF (t1.gt.1) THEN
                prj_prdCum(iFSC,t1) = prj_prdCum(iFSC,t1-1) + prj_oilPrd(iFSC,t1) + prj_cndPrd(iFSC,t1)  &
                    + (prj_gasPrd(iFSC,t1) + prj_asgPrd(iFSC,t1))/BOEtoMCF
            ELSE
                prj_prdCum(iFSC,t1) = prj_oilPrd(iFSC,t1) + prj_cndPrd(iFSC,t1)  &
                    + (prj_gasPrd(iFSC,t1) + prj_asgPrd(iFSC,t1))/BOEtoMCF
            ENDIF
        ENDDO
    ENDIF
ENDDO

! Perform both exploration and development project economic
prj%expNPV = 0.
prj%devNPV = 0.

! Start with economic calculation for development project
DO iEcon = 1,2

    ! Exploration economic starts here
    !ExplorationEconomic:

    ! Allocate memory/reset economic variables
    ! ReDim prj%GRev(1 To nTime) As Double
    ! ReDim prj%ROY(1 To nTime) As Double
    ! ReDim prj%NRev(1 To nTime) As Double
    ! ReDim prj%STax(1 To nTime) As Double
    ! ReDim prj%OCst(1 To nTime) As Double
    ! ReDim prj%TCst(1 To nTime) As Double
    ! ReDim prj%Inv(1 To nTime) As Double
    ! ReDim prj%TInv(1 To nTime) As Double
    ! ReDim prj%IInv(1 To nTime) As Double
    ! ReDim prj%BTCF(1 To nTime) As Double
    ! ReDim prj%DBas(1 To nTime) As Double
    ! ReDim prj%DExp(1 To nTime) As Double
    ! ReDim prj%EInv(1 To nTime) As Double
    ! ReDim prj%NIBT(1 To nTime) As Double
    ! ReDim prj%SITax(1 To nTime) As Double
    ! ReDim prj%FTInc(1 To nTime) As Double
    ! ReDim prj%FITax(1 To nTime) As Double
    ! ReDim prj%NIAT(1 To nTime) As Double
    ! ReDim prj%ATCF(1 To nTime) As Double
    prj_GRev = 0.
    prj_ROY = 0.
    prj_NRev = 0.
    prj_STax = 0.
    prj_OCst = 0.
    prj_TCst = 0.
    prj_Inv = 0.
    prj_TInv = 0.
    prj_IInv = 0.
    prj_BTCF = 0.
    prj_DBas = 0.

!==========================> HSM Code Start <==========================
    hsm_amor = 0.0
    hsm_depr = 0.0
!===========================> HSM Code End <===========================

    prj_DExp = 0.
    prj_EInv = 0.
    prj_NIBT = 0.
    prj_SITax = 0.
    prj_FTInc = 0.
    prj_FITax = 0.
    prj_NIAT = 0.
    prj_ATCF = 0.
    prj_acst = 0.

    IF (iEcon .NE. 1) THEN

        ! Investment, Tangible Investment, Intangible Investment:
        ! => Exploration drilling cost (MM$/Year) (at the beginning of the project)
        DO iFSC = minFSC,minFSC+numFSC-1
            IF (prj%nFld(iFSC) .GT. 0) THEN
                prj_Inv(1) = prj_Inv(1) + prj%expDrlCst(iFSC)
                prj_TInv(1) = prj_TInv(1) + prj%expDrlCst(iFSC) *  expTangFrc
                prj_IInv(1) = prj_IInv(1) + prj%expDrlCst(iFSC) * (1. -  expTangFrc)
            ENDIF
        ENDDO

    ENDIF

    ! Development economic starts here (without exploration cost)

    !   DO t1 = toff,ntime
    DO t1 = 1, IJUMPYR

        !Investment, Tangible Investment, Intangible Investment:
        ! => Production facility cost (MM$/Year)
        prj_Inv(t1) = prj_Inv(t1) + prj_pfCst(t1)
        prj_TInv(t1) = prj_TInv(t1) + prj_pfCst(t1) *  pfTangFrc
        prj_IInv(t1) = prj_IInv(t1) + prj_pfCst(t1) * (1. -  pfTangFrc)

        DO iFSC = minFSC,minFSC+numFSC-1
            IF (prj%nFld(iFSC) .GT. 0) THEN

!==========================> HSM Code Start <==========================
                hsm_fsc = iFSC
!===========================> HSM Code End <===========================

                IF (iEcon .NE. 2) THEN          ! Do Economic Limit Check

                    ! *** FIELD ECONOMIC LIMIT CALCULATION *************************
                    ! Economic Limit = Revenue - Costs - Royalties - Taxes
                    ! => Stop field production if "Economic Limit" <= 0

                    IF (prj_prdRev(iFSC, t1) .GT. 0) THEN

                        ! Revenue from production (MM$/Year)
                        netRevenue = prj_prdRev(iFSC, t1)

                        ! Royalties (MM$/Year)
                        v1 = prj_prdRev(iFSC, t1) *  royRate(iPA, iEU, iFSC)
                        netRevenue = netRevenue - v1

                        ! Operating cost (MM$/Year)
                        v1 = prj_nOprWls(iFSC, t1) * &
                            OprCost(iPA, iEU, iFSC,  wDpth(iPA, iEU, iFSC),  dDpth(iPA, iEU, iFSC), t1+toff-1)
                        netRevenue = netRevenue - v1

                        ! Transportation cost (MM$/Year)
                        netRevenue = netRevenue - prj_trnCst(iFSC, t1)

                        ! Severance taxes (MM$/Year)
                        v2 = (prj_oilPrd(iFSC, t1) *  oilPrice(iogsm,toff) + prj_cndPrd(iFSC, t1) *  oilPrice(iogsm,toff)) / 1000000.
                        netRevenue = netRevenue - v1 * v2 * ( oilSevTaxRate / 100.)

                        v2 = prj_oilPrd(iFSC, t1) + prj_cndPrd(iFSC, t1)  ! Liquid production
                        netRevenue = netRevenue - v2 *  oilSevTaxPrd / 1000000.

                        v2 = (prj_gasPrd(iFSC, t1) + prj_asgPrd(iFSC, t1)) *  gasPrice(iogsm,toff) / 1000000.
                        netRevenue = netRevenue - v1 * v2 * ( gasSevTaxRate / 100.)

                        v2 = prj_gasPrd(iFSC, t1) + prj_asgPrd(iFSC, t1)  ! Gas production
                        netRevenue = netRevenue - v2 *  gasSevTaxPrd / 1000000.

                        ! State and federal taxes (these could be negative)
                        stateTax = netRevenue * ( stTaxRate(iPA, iEU) / 100.)
                        fedTax = (netRevenue - stateTax) * ( fedTaxRate / 100.)

                        ! Field reaches economic limit => STOP FIELD PRODUCTION
                        econLimit = netRevenue - stateTax - fedTax
                        IF (econLimit .LE. 0.) THEN
                            prj_aCst(t1) = abandoncst
                            DO tt = t1,IJUMPYR
                                prj_oilPrd(iFSC, tt) = 0.
                                prj_gasPrd(iFSC, tt) = 0.
                                prj_asgPrd(iFSC, tt) = 0.
                                prj_cndPrd(iFSC, tt) = 0.
                                prj_prdRev(iFSC, tt) = 0.
                                prj_trnCst(iFSC, tt) = 0.
                                prj_nOprWls(iFSC, tt) = 0
                                DO FF = 1,prj%nFld(iFSC)
                                    prj_fNwls(iFSC, FF, tt) = 0
                                ENDDO
                            ENDDO
                        ENDIF

                    ENDIF
                    !*** END ECONOMIC LIMIT CALCULTION ****************************

                ENDIF ! Skip Economic Limit Check

                ! Investment, Tangible Investment, Intangible Investment:
                ! => Development drilling cost (MM$/Year)
                prj_Inv(t1) = prj_Inv(t1) + prj_devDrlCst(iFSC, t1)
                prj_TInv(t1) = prj_TInv(t1) + prj_devDrlCst(iFSC, t1) *  devTangFrc
                prj_IInv(t1) = prj_IInv(t1) + prj_devDrlCst(iFSC, t1) * (1. -  devTangFrc)

                ! Operating cost (MM$/Year)
                v1 = prj_nOprWls(iFSC, t1) * &
                    OprCost(iPA, iEU, iFSC, wDpth(iPA, iEU, iFSC),  dDpth(iPA, iEU, iFSC), t1)
                prj_OCst(t1) = prj_OCst(t1) + v1

                ! Transportation cost (MM$/Year)
                prj_TCst(t1) = prj_TCst(t1) + prj_trnCst(iFSC, t1)

                ! Gross revenue (MM$/Year)
                prj_GRev(t1) = prj_GRev(t1) + prj_prdRev(iFSC, t1)

                ! Royalties (MM$/Year)
                v1 = prj_prdRev(iFSC, t1) *  royRate(iPA, iEU, iFSC)
                prj_ROY(t1) = prj_ROY(t1) + v1

                ! Net revenue (MM$/Year)
                v1 = 1. -  royRate(iPA, iEU, iFSC)
                prj_NRev(t1) = prj_NRev(t1) + prj_prdRev(iFSC, t1) * v1

                ! Severance taxes (MM$/Year)
                v2 = (prj_oilPrd(iFSC, t1) *  oilPrice(iogsm,toff) + prj_cndPrd(iFSC, t1) *  OilPrice(iOGSM,toff)) / 1000000.
                prj_STax(t1) = prj_STax(t1) + v1 * v2 * ( oilSevTaxRate / 100.)

                v2 = prj_oilPrd(iFSC, t1) + prj_cndPrd(iFSC, t1)
                prj_STax(t1) = prj_STax(t1) + v2 *  oilSevTaxPrd

                v2 = (prj_gasPrd(iFSC, t1) + prj_asgPrd(iFSC, t1)) *  gasPrice(iogsm,toff) / 1000000.
                prj_STax(t1) = prj_STax(t1) + v1 * v2 * ( gasSevTaxRate / 100.)

                v2 = prj_gasPrd(iFSC, t1) + prj_asgPrd(iFSC, t1)
                prj_STax(t1) = prj_STax(t1) + v2 *  gasSevTaxPrd

            ENDIF

        ENDDO

        ! Before tax cashflow (MM$/Year)
        prj_BTCF(t1) = prj_NRev(t1) - prj_STax(t1) - prj_OCst(t1) - prj_TCst(t1) - prj_Inv(t1)

        ! Depreciation base (MM$/Year)
        prj_DBas(t1) = prj_TInv(t1) + 0.3 * prj_IInv(t1)

        ! Depreciation expense (MM$/Year)
        i = 0
        tend = t1 + 8 - 1
        !If (tend .GT. nTime) tend = nTime
        IF (tend .GT. IJUMPYR) tend = IJUMPYR
        DO tt = t1,tend
            i = i + 1
            prj_DExp(tt) = prj_DExp(tt) + prj_DBas(t1) *  deprSch(i)

!==========================> HSM Code Start <==========================
            hsm_amor(tt) = hsm_amor(tt) + prj_IInv(t1) *  deprSch(i) * 0.3
            hsm_depr(tt) = hsm_depr(tt) + prj_TInv(t1) *  deprSch(i)
!===========================> HSM Code End <===========================

        ENDDO

        ! Expensed investments (MM$/Year)
        prj_EInv(t1) = 0.7 * prj_IInv(t1)

        ! Net income before taxes (MM$/Year)
        prj_NIBT(t1) = prj_NRev(t1) - prj_STax(t1) - prj_OCst(t1) - prj_TCst(t1) - prj_DExp(t1) - prj_EInv(t1)

        ! State income taxes (MM$/Year)
        prj_SITax(t1) = prj_NIBT(t1) * ( stTaxRate(iPA, iEU) / 100.)

        ! Federal taxable income (MM$/Year)
        prj_FTInc(t1) = prj_NIBT(t1) - prj_SITax(t1)

        ! Federal income taxes (MM$/Year)
        prj_FITax(t1) = prj_FTInc(t1) * ( fedTaxRate / 100.)

        ! Net income after taxes (MM$/Year)
        prj_NIAT(t1) = prj_NIBT(t1) - prj_SITax(t1) - prj_FITax(t1)

        ! After tax cashflow (MM$/Year)
        prj_ATCF(t1) = prj_BTCF(t1) - prj_SITax(t1) - prj_FITax(t1)

        ! Net present value (MM$ at time=t)
        tcstCap = cstCAP
        IF (iPA.ge.3.and.iEU.ne.7) tcstCap = cstCAP*1.25
        IF (iEcon .EQ. 1) THEN
            prj%devNPV = prj%devNPV + prj_ATCF(t1) / (1. +  tcstCap / 100.) ** (t1 - 1)

!==========================> HSM Code Start <==========================
            hsm_npv = prj%devNPV
!===========================> HSM Code End <===========================

        ELSE
            prj%expNPV = prj%expNPV + prj_ATCF(t1) / (1. +  tcstCap / 100.) ** (t1 - 1)

!==========================> HSM Code Start <==========================
            hsm_npv = prj%expNPV
!===========================> HSM Code End <===========================

        ENDIF
        
        !IF (T1.EQ.1) WRITE(OFFOUT,*) 'Prospect [toff, ipa, ieu]', toff, ipa, ieu
        !IF (T1.EQ.1) WRITE(OFFOUT,100)
        !WRITE(OFFOUT,200) t1, prj_GRev(t1), prj_ROY(t1), prj_NRev(t1), prj_STax(t1), &
        !    prj_OCst(t1), prj_TCst(t1), prj_Inv(t1), prj_TInv(t1), &
        !    prj_IInv(t1), prj_BTCF(t1), prj_DBas(t1), prj_DExp(t1), &
        !    prj_EInv(t1), prj_NIBT(t1), prj_SITax(t1), prj_FTInc(t1), &
        !    prj_FITax(t1), prj_NIAT(t1), prj_ATCF(t1)
        !IF (T1.EQ.IJUMPYR) THEN
        !    IF (iEcon .EQ. 1) THEN
        !        WRITE(OFFOUT,*) 'Development NPV', yearof(toff), ipa, ieu, prj%devNPV
        !    ELSE
        !        WRITE(OFFOUT,*) 'Exploration NPV', yearof(toff), ipa, ieu, prj%expNPV
        !    ENDIF
        !ENDIF

    ENDDO


ENDDO

!==========================> HSM Code Start <==========================
if (hsm_dcf_bool .and. exp_run_bool) then
    ! 'function', 'year', 'crude_price', 'natgas_price', 'crude_trans_price', &
    ! 'natgas_trans_price', 'crude_tariff_price', 'natgas_tariff_price', 'royalty_rate', 'fed_tax_rate', &
    ! 'net_present_value', 'abandon_rate', 'state_abbreviation', 'exploratory_success_rate', 'development_success_rate', &
    ! 'exploratory_cost', 'development_cost', 'exploratory_dry_cost', 'development_dry_cost', 'exp_tang_frac', &
    ! 'dev_tang_frac', 'kap_tang_frac', 'intang_amor_frac'
    ! hsm inputs
    write(hsm_dcf_opts , '(*(G0.16,:,","))') 'CEP', curcalyr, oilPrice(iogsm,toff), gasPrice(iogsm,toff), &
                                             trnTrfOil(iPA, iEU), trnTrfGas(iPA, iEU), 0.0, 0.0, royRate(iPA, iEU, hsm_fsc), &
                                             fedTaxRate / 100., hsm_npv*1000000.0, abandoncst*1000000.0, 'OCS', 1.0, 1.0, &
                                             ExplCost(iPA, iEU,  wDpth(iPA, iEU, hsm_fsc), dDpth(iPA, iEU, hsm_fsc), toff), &
                                             DevlCost(iPA, iEU,  wDpth(iPA, iEU, hsm_fsc), dDpth(iPA, iEU, hsm_fsc), toff), &
                                             0.0, 0.0, expTangFrc, devTangFrc, pfTangFrc, 0.3, 'macrs_7', 'macrs_7', tcstCap, &
                                             nmEU(iPA,iEU), hsm_fsc, fOilRes, prdOilFrc(iPA, iEU), fGasRes, prdGasFrc(iPA, iEU), &
                                             costPF*1000000.0, costFL*1000000.0, nFL, wD, prj%pfTyp(1), prj%pfNWlsArr(1), & 
                                             devDrillPerYear(prj%pfTyp(1), prj%pfNWlsArr(1),  wDpth(iPA, iEU, hsm_fsc), dDpth(iPA, iEU, hsm_fsc)), &
                                             prj%devWlsDrl(hsm_fsc), prj%expDrlNWls(hsm_fsc)
    ! hsm inputs
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'CEP', iEcon, 'crude_production'     , SUM(prj_oilPrd + prj_cndPrd, DIM=1)
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'CEP', iEcon, 'natgas_production'    , SUM(prj_gasPrd + prj_asgPrd, DIM=1)
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'CEP', iEcon, 'non_condensate_production'     , SUM(prj_oilPrd, DIM=1)
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'CEP', iEcon, 'condensate_production'         , SUM(prj_cndPrd, DIM=1)
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'CEP', iEcon, 'non_associated_gas_production' , SUM(prj_gasPrd, DIM=1)
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'CEP', iEcon, 'associated_gas_production'     , SUM(prj_asgPrd, DIM=1)
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'CEP', iEcon, 'exp_drill_cost'       , prj%expDrlCst(hsm_fsc)*1000000.0, (/1:60/)*0.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'CEP', iEcon, 'dev_drill_cost'       , ((prj_devDrlCst(hsm_fsc,M)*1000000.0), M=1,IJUMPYR)
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'CEP', iEcon, 'kap_cost'             , prj_pfCst*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'CEP', iEcon, 'operating_cost'       , prj_OCst*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'CEP', iEcon, 'tax_base_state'       , prj_NIBT*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'CEP', iEcon, 'tax_base_fed'         , prj_FTInc*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'CEP', iEcon, 'expn_intang_cost'     , prj_EInv*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'CEP', iEcon, 'amor_intang_cost'     , hsm_amor*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'CEP', iEcon, 'deprec_tang_cost'     , hsm_depr*1000000.0
    ! hsm checks
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'CEP', iEcon, 'revenue'              , SUM(prj_prdRev, DIM=1)*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'CEP', iEcon, 'transportation'       , prj_TCst*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'CEP', iEcon, 'royalty'              , prj_ROY*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'CEP', iEcon, 'severance_tax'        , prj_STax*1000000.0
    hsm_depr = 0.0
    hsm_depr(1) = prj%expDrlCst(hsm_fsc)
    hsm_depr = hsm_depr + prj_devDrlCst(hsm_fsc,1:IJUMPYR)
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'CEP', iEcon, 'drill_cost'           , hsm_depr*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'CEP', iEcon, 'state_tax'            , prj_SITax*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'CEP', iEcon, 'fed_tax'              , prj_FITax*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'CEP', iEcon, 'cash_flow'            , prj_ATCF*1000000.0
endif
!===========================> HSM Code End <===========================

100 FORMAT(' Year ', ' Gross Rev', '   Royalty', '   Net Rev', '   Sev Tax', &
    ' Oper Cost', ' Tran Cost', '  Drl Cost', '  Tang Cst', &
    ' Intan Cst', 'Pre-Tax CF', ' Depr Base', '  Depr Exp', &
    ' Exp Cost ', 'Net Inc BT', '    ST Tax', '   Tax Inc', &
    '  FED Tax ', 'Net Inc AT', 'Aft Tax CF')
200 FORMAT(I6,19F10.0)

! Save total project production and well counts
! DO t1 = toff,ntime
DO t1 = 1,IJUMPYR-toff+1
    DO iFSC = minFSC,minFSC+numFSC-1
        DO FF = 1,prj%nFld(iFSC)
            prj%fnwls(t1+toff-1) = prj%fnwls(t1+toff-1) + prj_fnwls(iFSC, ff, t1)
        ENDDO
        prj%devdrlsch(t1+toff-1) = prj%devdrlsch(t1+toff-1) + prj_devdrlsch(iFSC, t1)
        prj%noprwls(t1+toff-1) = prj%noprwls(t1+toff-1) + prj_nOprWls(iFSC, t1)
        prj%oilPrd(t1+toff-1) = prj%oilPrd(t1+toff-1) + prj_oilPrd(iFSC, t1)
        prj%gasPrd(t1+toff-1) = prj%gasPrd(t1+toff-1) + prj_gasPrd(iFSC, t1)
        prj%asgPrd(t1+toff-1) = prj%asgPrd(t1+toff-1) + prj_asgPrd(iFSC, t1)
        prj%cndPrd(t1+toff-1) = prj%cndPrd(t1+toff-1) + prj_cndPrd(iFSC, t1)
    ENDDO
ENDDO
!WRITE(OFFOUT,*) 'EXITING SUBROUTINE CreateExplorationProject, YEAR =', yearof(toff), prj%expNPV, prj%devNPV

END SUBROUTINE CREATEEXPLORATIONPROJECT

!************************************************************************************!
! Subroutine that interrelates information pertaining to announced discoveries 
! stored in FieldObj type to ProjectObj type such that costs can be calculated
!
! Calls subroutine DevelopAnnouncedDiscovery that actually calculates the economics
! of each announced discovery and determines if announced discovery is economic
!
! Calls subroutine AnnouncedDiscoveryProductionProfile
!
! Added December 2015 to represent price response of undeveloped offshore projects
!************************************************************************************!
SUBROUTINE AnnouncedDiscoveryEcon
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ncntrl'       ! nems control variables
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmbfw'
INCLUDE 'ogsmoff'
INCLUDE 'dwpparm'
INCLUDE 'ogsmout'
INCLUDE 'macout'

TYPE (PROJECTOBJ) PRJ(2)
INTEGER PFDELAY1, PFDELAY2, DEVDRILLPERYEAR
INTEGER iPA, iEU, iFSC, t1, tbeg, tend,tt,nwls,ndrl,n0, ifld, itmp
INTEGER nf, nfpf, iECON, FF, ttt, nfl, ireg, iogsm
INTEGER tbld(maxNFields), pfnwlsarr(maxNFields), pfnfld(maxNFields)
INTEGER fnwls(maxNFields), tdecOil(maxNFields), tdecGas(maxNFields)
INTEGER nextint
REAL*4 wD, dD, costPF, costFL, fOilRes, fGasRes, qo, qg, qad, abandoncst
REAL*4 qcnd, vo, vg, v1, v2, v3, netrevenue, statetax, fedtax, econlimit
REAL*4 irate1, irate2, adjfac, tcstcap
REAL*4 foilcum(maxNFields), fgascum(maxNFields)
REAL*4 EXPLCOST, DEVLCOST, OPRCOST, PFCOST, FLOWLINECOST
REAL*4 prj_pfCst(MNUMYR)              ! Production facility cost (MM$) schedule by time index
REAL*4 prj_GRev(MNUMYR)               ! Gross revenue (MM$) by time incex
REAL*4 prj_ROY(MNUMYR)                ! Royalties (MM$) by time index
REAL*4 prj_NRev(MNUMYR)               ! Net revenue (MM$) by time index
REAL*4 prj_STax(MNUMYR)               ! Severance tax (MM$) by time index
REAL*4 prj_OCst(MNUMYR)               ! Operating cost (MM$) by time index
REAL*4 prj_TCst(MNUMYR)               ! Transportation cost (MM$) by time index
REAL*4 prj_Inv(MNUMYR)                ! Investment (MM$) by time index
REAL*4 prj_TInv(MNUMYR)               ! Tangible investment (MM$) by time index
REAL*4 prj_IInv(MNUMYR)               ! Intangible investment (MM$) by time index
REAL*4 prj_BTCF(MNUMYR)               ! Before tax cashflow (MM$) by time index
REAL*4 prj_DBas(MNUMYR)               ! Depreciation base (MM$) by time  index
REAL*4 prj_DExp(MNUMYR)               ! Depreciation expense (MM$) by time index
REAL*4 prj_EInv(MNUMYR)               ! Expensed investments (MM$) by time index
REAL*4 prj_NIBT(MNUMYR)               ! Net income before taxes (MM$) by time index
REAL*4 prj_SITax(MNUMYR)              ! State income taxes (MM$) by time index
REAL*4 prj_FTInc(MNUMYR)              ! Federal taxable income (MM$) by time index
REAL*4 prj_FITax(MNUMYR)              ! Federal income taxes (MM$) by time index
REAL*4 prj_NIAT(MNUMYR)               ! Net income after taxes (MM$) by time index
REAL*4 prj_ATCF(MNUMYR)               ! After tax cashflow (MM$) by time index
REAL*4 prj_ACST(MNUMYR)               ! Abandonment Costs (MM$) by time index
REAL*4 prj_prdCum(minFSC:minFSC+numFSC-1,MNUMYR)        ! Cumulative production (BOE) by FSC, time index
REAL*4 prj_prdRev(minFSC:minFSC+numFSC-1,MNUMYR)        ! Revenue (MM$) schedule from production by FSC,
! time index
REAL*4 prj_trnCst(minFSC:minFSC+numFSC-1,MNUMYR)        ! Transportation cost (MM$) schedule by FSC, time index
REAL*4 prj_devDrlCst(minFSC:minFSC+numFSC-1,MNUMYR)          ! Development drilling cost (MM$) schedule by FSC,
! time index
REAL*4 prj_oilPrd(minFSC:minFSC+numFSC-1,MNUMYR)             ! Oil production (Bbl) by FSC, time index
REAL*4 prj_gasPrd(minFSC:minFSC+numFSC-1,MNUMYR)             ! Non-associated Gas production (Mcf) by FSC,
! time index
REAL*4 prj_cndPrd(minFSC:minFSC+numFSC-1,MNUMYR)             ! Condensate production (Bbl) by FSC, time index
REAL*4 prj_asgPrd(minFSC:minFSC+numFSC-1,MNUMYR)             ! Associated gas production (Mcf) by FSC, time index

INTEGER prj_fNwls(minFSC:minFSC+numFSC-1,MaxNFields,MNUMYR)  ! Number of wells by FSC, field counter, time index
INTEGER prj_nOprWls(minFSC:minFSC+numFSC-1,MNUMYR)         ! Number of operating wells by FSC, time index
INTEGER prj_devDrlSch(minFSC:minFSC+numFSC-1,MNUMYR)       ! Development drilling schedule (number of wells/yr)
! by FSC, time index

!WRITE(OFFOUT,*) 'ENTERING SUBROUTINE AnnouncedDiscoveryEcon, YEAR =', yearof(toff) ! DBG_Flag_Ent_Ext_Sub

DO iPA = 1, nPA
    DO iEU = 1, nEU(iPA)
        DO iFSC = minFSC,minFSC+numFSC-1

            IF ( duNRR(iPA, iEU, iFSC) .GT. 0) THEN

                DO iFld = 1, duNRR(iPA, iEU, iFSC)

                    ! Field ID of discovered/undeveloped field (type 1)
                    DO itmp = 1,ntfld
                        IF (fldID2(itmp).eq.1.and. &
                            fldID3(itmp).eq.iPA.and. &
                            fldID4(itmp).eq.iEU.and. &
                            fldID5(itmp).eq.iFSC.and. &
                            fldID6(itmp).eq.ifld) THEN
                        f = fldID1(itmp)
                        EXIT
                        ENDIF
                    ENDDO
                    If (Fld(f)%initTyp .NE. 1) WRITE(OFFOUT,*) 'WARNING: Logic error, expecting discovered/undeveloped field flag'

                    ! Store information related to announced discoveries in type FieldObj to type ProjectObj
                    CALL INITPROJECT(prj(1))

                    IF (Fld(f)%pfTypFlg(7) .eq. 1 .and. Fld(f)%FSC .le. 12) THEN
                        prj(1)%Typ = 3
                    ELSE IF (Fld(f)%pfTypFlg(7) .eq. 1 .and. Fld(f)%FSC .gt. 12) THEN
                        prj(1)%Typ = 2
                    ELSE
                        prj(1)%Typ = 1
                    ENDIF
                    prj(1)%Reg = Fld(f)%Reg
                    prj(1)%PA = Fld(f)%PA
                    prj(1)%EU = Fld(f)%EU
                    prj(1)%OGTyp = Fld(f)%ogtyp
                    prj(1)%FrcprdOil = Fld(f)%FrcprdOil
                    prj(1)%FrcprdGas = Fld(f)%FrcprdGas
                    prj(1)%iprdOrate = Fld(f)%iprdOrate
                    prj(1)%iprdGrate = Fld(f)%iprdGrate
                    prj(1)%mprdOrate = Fld(f)%mprdOrate
                    prj(1)%mprdGrate = Fld(f)%mprdGrate
                    prj(1)%prdODec = Fld(f)%prdODec
                    prj(1)%prdGDec = Fld(f)%prdGDec
                    prj(1)%biggestFSC = Fld(f)%FSC
                    prj(1)%nFld(iFSC) = 1
                    prj(1)%yrPrdStart = Fld(f)%yrStPrd - 1989		!year index (1990 = 1)
                    DO I = 1, nPFTYP
                        IF (Fld(f)%pfTypFlg(I) .eq. 1) prj(1)%pfTyp(1) = I
                    ENDDO

                    ! Run DCF/NPV calc on projects (unless it has already started, i.e. in the case of Lucius, Heidelberg, and Amethyst)
                    ! Not the prettiest/most efficient way to insert this caveat, but so it goes
                    IF (Fld(f)%nickname .ne. "LUCIUS" .and. Fld(f)%nickname .ne. "HEIDELBERG" .and. FLD(F)%nickname .ne. "AMETHYST") THEN
                        CALL DevelopAnnouncedDiscovery(prj, f)
                    ENDIF

                    ! If project isn't economic, delay production start year until it is economic
                    IF (prj(1)%devNPV .LT. 0) THEN
                        DO WHILE (prj(1)%devNPV .LT. 0 .AND. prj(1)%yrPrdStart .LT. MNUMYR)		! Loops from original start prod yr + 1 to end of projection period (2050 in this case)
                            prj(1)%yrPrdStart = prj(1)%yrPrdStart + 1
                            CALL DevelopAnnouncedDiscovery(prj, f)
                        ENDDO
                        IF (prj(1)%devNPV .LT. 0 .AND. prj(1)%yrPrdStart .EQ. MNUMYR) prj(1)%yrPrdStart = prj(1)%yrPrdStart + 1
                        Fld(f)%yrStPrd = prj(1)%yrPrdStart + 1989
                        WRITE(OFFOUT,*) 'teydelay ', Fld(f)%nickname, 'start production ', Fld(f)%yrStPrd, ' devNPV ', prj%devNPV
                    ENDIF
                ENDDO
            ENDIF
        ENDDO
    ENDDO
ENDDO

IF(prj(1)%yrPrdStart.LE.MNUMYR) CALL AnnouncedDiscoveryProductionProfile

!WRITE(OFFOUT,*) 'EXITING SUBROUTINE AnnouncedDiscoveryEcon, YEAR =', yearof(toff) ! DBG_Flag_Ent_Ext_Sub

END SUBROUTINE AnnouncedDiscoveryEcon

!************************************************************************************!
! Subroutine that calculates the economics of an announced discovery
!   Generates net present value of the development of an announced discovery
!   Adapted from CreateExplorationProject subroutine, added December 2015
!************************************************************************************!
SUBROUTINE DevelopAnnouncedDiscovery(prj, f_num)
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ncntrl'       ! nems control variables
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmbfw'
INCLUDE 'ogsmoff'
INCLUDE 'ngtdmrep'
INCLUDE 'macout'
INCLUDE 'lfmmout'
INCLUDE 'pmmout'	 ! has the dcrdwhp (oil wellhead prices) variable
INCLUDE 'dwpparm'

TYPE (PROJECTOBJ), INTENT(INOUT) ::  PRJ
INTEGER*4, INTENT(IN) ::  f_num

INTEGER PFDELAY1, PFDELAY2, DEVDRILLPERYEAR
INTEGER iPA, iEU, iFSC, t1, tbeg, tend,tt,nwls,ndrl,n0
INTEGER nf, nfpf, iECON, FF, ttt, nfl, ireg, iogsm
INTEGER tbld(maxNFields), pfnwlsarr(maxNFields), pfnfld(maxNFields)
INTEGER fnwls(maxNFields), tdecOil(maxNFields), tdecGas(maxNFields)
INTEGER nextint
REAL*4 wD, dD, costPF, costFL, fOilRes, fGasRes, qo, qg, qad, abandoncst
REAL*4 qcnd, vo, vg, v1, v2, v3, netrevenue, statetax, fedtax, econlimit
REAL*4 irate1, irate2, adjfac, tcstcap
REAL*4 foilcum(maxNFields), fgascum(maxNFields)
REAL*4 EXPLCOST, DEVLCOST, OPRCOST, PFCOST, FLOWLINECOST
REAL*4 prj_pfCst(MNUMYR)              ! Production facility cost (MM$) schedule by time index
REAL*4 prj_GRev(MNUMYR)               ! Gross revenue (MM$) by time incex
REAL*4 prj_ROY(MNUMYR)                ! Royalties (MM$) by time index
REAL*4 prj_NRev(MNUMYR)               ! Net revenue (MM$) by time index
REAL*4 prj_STax(MNUMYR)               ! Severance tax (MM$) by time index
REAL*4 prj_OCst(MNUMYR)               ! Operating cost (MM$) by time index
REAL*4 prj_TCst(MNUMYR)               ! Transportation cost (MM$) by time index
REAL*4 prj_Inv(MNUMYR)                ! Investment (MM$) by time index
REAL*4 prj_TInv(MNUMYR)               ! Tangible investment (MM$) by time index
REAL*4 prj_IInv(MNUMYR)               ! Intangible investment (MM$) by time index
REAL*4 prj_BTCF(MNUMYR)               ! Before tax cashflow (MM$) by time index
REAL*4 prj_DBas(MNUMYR)               ! Depreciation base (MM$) by time  index
REAL*4 prj_DExp(MNUMYR)               ! Depreciation expense (MM$) by time index
REAL*4 prj_EInv(MNUMYR)               ! Expensed investments (MM$) by time index
REAL*4 prj_NIBT(MNUMYR)               ! Net income before taxes (MM$) by time index
REAL*4 prj_SITax(MNUMYR)              ! State income taxes (MM$) by time index
REAL*4 prj_FTInc(MNUMYR)              ! Federal taxable income (MM$) by time index
REAL*4 prj_FITax(MNUMYR)              ! Federal income taxes (MM$) by time index
REAL*4 prj_NIAT(MNUMYR)               ! Net income after taxes (MM$) by time index
REAL*4 prj_ATCF(MNUMYR)               ! After tax cashflow (MM$) by time index
REAL*4 prj_ACST(MNUMYR)               ! Abandonment Costs (MM$) by time index
REAL*4 prj_prdCum(minFSC:minFSC+numFSC-1,MNUMYR)        ! Cumulative production (BOE) by FSC, time index
REAL*4 prj_prdRev(minFSC:minFSC+numFSC-1,MNUMYR)        ! Revenue (MM$) schedule from production by FSC,
! time index
REAL*4 prj_trnCst(minFSC:minFSC+numFSC-1,MNUMYR)        ! Transportation cost (MM$) schedule by FSC, time index
REAL*4 prj_devDrlCst(minFSC:minFSC+numFSC-1,MNUMYR)          ! Development drilling cost (MM$) schedule by FSC,
! time index
REAL*4 prj_oilPrd(minFSC:minFSC+numFSC-1,MNUMYR)             ! Oil production (Bbl) by FSC, time index
REAL*4 prj_gasPrd(minFSC:minFSC+numFSC-1,MNUMYR)             ! Non-associated Gas production (Mcf) by FSC,
! time index
REAL*4 prj_cndPrd(minFSC:minFSC+numFSC-1,MNUMYR)             ! Condensate production (Bbl) by FSC, time index
REAL*4 prj_asgPrd(minFSC:minFSC+numFSC-1,MNUMYR)             ! Associated gas production (Mcf) by FSC, time index

INTEGER prj_fNwls(minFSC:minFSC+numFSC-1,MaxNFields,MNUMYR)  ! Number of wells by FSC, field counter, time index
INTEGER prj_nOprWls(minFSC:minFSC+numFSC-1,MNUMYR)         ! Number of operating wells by FSC, time index
INTEGER prj_devDrlSch(minFSC:minFSC+numFSC-1,MNUMYR)       ! Development drilling schedule (number of wells/yr)
! by FSC, time index

!==========================> HSM Code Start <==========================
real hsm_npv
real hsm_amor(MNUMYR)
real hsm_depr(MNUMYR)
hsm_npv         = 0.0
!===========================> HSM Code End <===========================

!WRITE(OFFOUT,*) 'ENTERING SUBROUTINE DevelopAnnouncedDiscovery, YEAR =', yearof(toff)

prj%f = 0
prj%fNwls = 0
prj_fNwls = 0

iPA = prj%PA
iReg = prj%REG
iEU = prj%EU
iOGSM = OGSMREG(iPA,iEU)
iFSC = prj%biggestFSC


! COMMON WATER DEPTH AND DRILLING DEPTH
wD =  wDpth(iPA, iEU, prj%biggestFSC)
dD =  dDpth(iPA, iEU, prj%biggestFSC)
prj%wDpth = wD
prj%dDpth = dD


prj%pfNWls = 0 ! Total number of wells/slots for the projects
IF (prj%nFld(iFSC).GT.0) THEN

    ! Number of delineation wells
    prj%delDrlNWls(iFSC) =  nDelWls(iPA, iEU, iFSC) * prj%nFld(iFSC)

    ! Delineation drilling cost (MM$ at time=t) = Cost per well * number of wells
    prj%delDrlCst(iFSC) = ExplCost(iPA, iEU,  wDpth(iPA, iEU, iFSC),  dDpth(iPA, iEU, iFSC), toff) * &
        prj%delDrlNWls(iFSC)

    ! Total number of wells/slots
    prj%pfNWls = prj%pfNWls +  nDevWls(iPA, iEU, iFSC) * prj%nFld(iFSC)

    ! Total number of development wells to be drilled by FSC
    prj%devWlsDrl(iFSC) = ( nDevWls(iPA, iEU, iFSC) -  nDelWls(iPA, iEU, iFSC)) * &
        prj%nFld(iFSC)
    IF (prj%devWlsDrl(iFSC).LT.0) prj%devWlsDrl(iFSC) = 0
    !WRITE(OFFOUT,*) 'teybug nDevWls ', nDevWls(iPA, iEU, iFSC), ' prj%devWlsDrl(iFSC) ', prj%devWlsDrl(iFSC), ' prj%pfNWls ', prj%pfNWls

ENDIF

! Number of production facilities (max. 60 wells/PF)
!prj%nPf = nextInt(prj%pfNWls / 60.)
prj%nPf = 1

! Number of wells for each production facility
n = 0
i = 1
!DO i = 1,prj%nPf
IF ((n + 60).LE.prj%pfNWls) THEN
    prj%pfNWlsArr(i) = 60
    n = n + 60
ELSE
    prj%pfNWlsArr(i) = prj%pfNWls - n
ENDIF
! pfNWlsArr(i) will be used for number of wells to be drilled from production facility
pfNWlsArr(i) = prj%pfNWlsArr(1)
!ENDDO

! Number of fields for each production facility
i = 1
n = 0
nWls = 0
nf = 0
nfpf = 0

IF (prj%nFld(iFSC).GT.0) THEN
    nf = nf + prj%nFld(iFSC)  ! Count total . of fields
    !DO f = 1,prj%nFld(iFSC)
    nWls = nWls +  nDevWls(iPA, iEU, iFSC) ! Count # of wells
    n = n + 1  ! Count # of fields
    IF (nWls.GE.prj%pfNWlsArr(i)) THEN
        pfNFld(i) = n  ! Store number of fields in this production facility
        nfpf = nfpf + n ! Count total # of fields assigned to production facility
        i = i + 1
        n = 0
        nWls = 0
    ENDIF
    !ENDDO
ENDIF

! For the last production facility
!IF (n.GT.0.And.i.EQ.prj%nPf) THEN
!    pfNFld(i) = n  ! Store number of fields in this production facility
!    nfpf = nfpf + n ! Count total # of fields assigned to production facility
!ENDIF

! Check balance (dump to first production facility)
IF (nfpf.GT.nf) THEN
    pfNFld(1) = pfNFld(1) - (nfpf - nf)
    IF (pfNFld(1).LT.1) pfNFld(1) = 1
ELSEIF (nfpf.LT.nf) THEN
    pfNFld(1) = pfNFld(1) + (nf - nfpf)
ENDIF

! Start year of development:
n = toff
prj%yrDevStart = toff

! Year index when a production facility is available
i = 1
!DO i = 1,prj%nPf
! pfDelay1 is the time to construct a production facility (use common water depth)
tbld(i) = pfDelay1(prj%pfTyp(i), wD, prj%pfNWlsArr(i), n)
n = n + tbld(i)
prj%pfTimeAvl(i) = n + 1
!ENDDO

! Check if the project will produce within the projection period
n = prj%nPf
!DO i = 1,prj%nPf
IF (prj%pfTimeAvl(i) .GT. nReportTime) n = n - 1
!ENDDO
IF (n .EQ. 0) THEN
    prj%expNPV = -1.
    prj%devNPV = -1.
    RETURN
ENDIF
prj%nPf = n

! Production facility cost schedule (MM$/Year)
! ReDim prj%pfCst(1 To nTime) As Double
prj_pfCst = 0.

DO i = 1,prj%nPf
    ! Calculate production facility cost
    ! If project type SS-PF or SS, production facility cost is zero (i.e., announced discovery is tied back to existing platform)
    !   **NOTE: This will have to be changed when have more time -
    !           For SS-PF, should take into account cost
    ! If project type PF, then production facility cost will be nonzero
    tt = prj%pfTimeAvl(i) - tbld(i)
    IF (prj%Typ .eq. 1) THEN
        costPF = pfCost(iPA, iEU, prj%biggestFSC, wD, prj%pfTyp(i), prj%pfNWlsArr(i), tt)
    ELSE
        costPF = 0
    ENDIF
    abandoncst = costPF * ABANDONFAC(prj%pfTyp(i))

    ! Add flowline cost (if any):
    ! - Project types: SS-PF and SS
    nFL = 0
    IF (prj%Typ .GT. 1) THEN
        nWls =  pfMaxNWls(prj%Typ) ! Maximum number of wells sharing a flowline
        nFL = nextInt(pfNWlsArr(i)*1.0 / nWls)  ! Number of flowlines
        IF (nFL .LT. pfNFld(i)) nFL = pfNFld(i)  ! it should be at least number of fields for the production facility
        ! - Project type PF
    ELSE
        IF (prj%pfTyp(i) .EQ. 7) THEN
            nWls =  pfMaxNWls(2) ! Maximum number of wells sharing a flowline (use SS-PF number)
            nFL = nextInt(pfNWlsArr(i)*1.0 / nWls) ! Number of flowlines
            IF (nFL .LT. pfNFld(i)) nFL = pfNFld(i) ! it should be at least number of fields for the production facility
        ENDIF
    ENDIF
    prj%nFlowline(i) = nFL  ! Store number of flowlines

    ! Calculate flowline cost
    ! IF (nFL .GT. 0) THEN
        costFL = FlowlineCost(iPA, iEU, wD, tt) * nFL
        ! Add flowline cost to the production facility cost
        costPF = costPF + costFL
    ! ENDIF

    ! Spread cost based on pfCstFrc
    tbeg = prj%pfTimeAvl(i) - tbld(i)
    tend = prj%pfTimeAvl(i) - 1
    IF (tend .GT. nTime)  tend = nTime
    tt = 0
    DO t1 = 1,tbld(i)
        prj_pfCst(t1) = prj_pfCst(t1) + costPF *  pfCstFrc(tbld(i), t1)
    ENDDO
ENDDO

! Development drilling and cost schedules (MM$/Year)
! ReDim prj%devDrlSch(5 To 20, 1 To nTime) As Integer
! ReDim prj%devDrlCst(5 To 20, 1 To nTime) As Double
prj%devDrlSch = 0
prj_devDrlSch = 0
prj_devDrlCst = 0.

! Convert delineation wells to production wells

IF (prj%nFld(iFSC) .GT. 0) THEN
    ! Number of delineation wells to be converted in the FSC
    IF (prj%devWlsDrl(iFSC) .GT. 0) THEN
        nWls =  nDelWls(iPA, iEU, iFSC) * prj%nFld(iFSC)
    ELSE
        nWls =  nDevWls(iPA, iEU, iFSC) * prj%nFld(iFSC)
    ENDIF
    !IF (nWls .EQ. 0) EXIT
    DO i = 1,prj%nPf
        IF (pfNWlsArr(i) .GT. 0.And.nWls .GT. 0) THEN
            n = pfNWlsArr(i)
            pfNWlsArr(i) = pfNWlsArr(i) - nWls
            !t1 = prj%pfTimeAvl(i)  - toff + 1
            t1 = tbld(i)+1
            IF (pfNWlsArr(i) .GE. 0) THEN
                ! Make them to come online at the end of production facility construction
                prj_devDrlSch(iFSC, t1) = prj_devDrlSch(iFSC, t1) + nWls
                nWls = 0
            ELSE
                prj_devDrlSch(iFSC, t1) = prj_devDrlSch(iFSC, t1) + n
                nWls = -pfNWlsArr(i)
                pfNWlsArr(i) = 0
            ENDIF
        ENDIF
    ENDDO
    IF (nWls .GT. 0) WRITE(offout,*) 'WARNING:  SUBROUTINE DevelopAnnouncedDiscovery: Problem converting delineation wells to production wells'
ENDIF

! Drill
IF (prj%nFld(iFSC) .GT. 0) THEN
    ! No drilling required (could be due to delineation wells - converted to production wells)
    !IF (prj%devWlsDrl(iFSC).EQ.0) EXIT
    nWls = 0  ! Number of development wells drilled in the FSC
    DO i = 1,prj%nPf

        ! Maximum number of development wells could be drilled in a year
        n = devDrillPerYear(prj%pfTyp(i), prj%pfNWlsArr(i),  wDpth(iPA, iEU, iFSC), &
            dDpth(iPA, iEU, iFSC))
        ! For SS, FPS, or FPSO, multiply by number of fields in the FSC
        IF (.Not.  platform(prj%pfTyp(i))) n = n * prj%nFld(iFSC)

        ! Create development drilling schedule
        ndrl = 0
        !       DO t1 = prj%pfTimeAvl(i)-toff+1,IJUMPYR
        DO t1 = tbld(i)+1,IJUMPYR

            ! No more drilling capacity for this production facility
            IF (pfNWlsArr(i).EQ.0) EXIT

            ! Determine number of wells to be drilled at time t1
            ndrl = ndrl + n  ! Drilling rate/year
            IF (ndrl .GT. pfNWlsArr(i)) ndrl = pfNWlsArr(i)  ! Constraint: PF drilling capacity
            ! Constraint: # of wells that could be drilled in FSC
            IF ((nWls + ndrl) .GT. prj%devWlsDrl(iFSC)) ndrl = prj%devWlsDrl(iFSC) - nWls

            ! Drill the development wells (drilling schedule)
            prj_devDrlSch(iFSC, t1) = prj_devDrlSch(iFSC, t1) + ndrl

            ! Development drilling cost schedule
            prj_devDrlCst(iFSC, t1) = DevlCost(iPA, iEU,  wDpth(iPA, iEU, iFSC), &
                dDpth(iPA, iEU, iFSC), t1+toff-1) * ndrl

            ! Remaining drilling capacity
            pfNWlsArr(i) = pfNWlsArr(i) - ndrl

            ! Total number of development wells drilled currently in the FSC
            nWls = nWls + ndrl

            ! All wells in the FSC have been drilled
            IF (nWls.EQ.prj%devWlsDrl(iFSC)) EXIT

        ENDDO

    ENDDO

ENDIF

! Production profile
!ReDim prj%nOprWls(5 To 20, 1 To nTime) As Integer
!ReDim prj%oilPrd(5 To 20, 1 To nTime) As Double
!ReDim prj%gasPrd(5 To 20, 1 To nTime) As Double
!ReDim prj%cndPrd(5 To 20, 1 To nTime) As Double
!ReDim prj%asgPrd(5 To 20, 1 To nTime) As Double
prj_nOprWls = 0
prj_oilPrd = 0.
prj_gasPrd = 0.
prj_cndPrd = 0.
prj_asgPrd = 0.
prj%nOprWls = 0
prj%oilPrd = 0.
prj%gasPrd = 0.
prj%cndPrd = 0.
prj%asgPrd = 0.


IF (prj%nFld(iFSC) .GT. 0) THEN

    ! Reset field number of wells
    fNwls = 0

    ! Reset start decline time index
    DO f = 1,prj%nFld(iFSC)
        tdecOil(f) = 0
        tdecGas(f) = 0
    ENDDO

    ! Reset field cumulative oil and gas produced
    fOilCum = 0.
    fGasCum = 0.

    ! Field initial oil (Bbl) and gas (Mcf) resources
    IF (prj%ogtyp.eq.'G'.or.prj%ogtyp.eq.'g') THEN
        fgasres = vmean(ifsc) *1000000. * BOEtoMcf
        foilres = 0.
    ELSEIF (prj%ogtyp.eq.'O'.or.prj%ogtyp.eq.'o') THEN
        foilres = vmean(ifsc) *1000000.
        fgasres = 0.
    ELSE
        fOilRes =  vMean(iFSC) * 1000000. * (1. -  GOprop(iPA, iEU))
        IF (fOilRes .LT. 0.001) fOilRes = 0.001
        fGasRes = ( vMean(iFSC) * 1000000. *  GOprop(iPA, iEU)) *  BOEtoMcf
        IF (fGasRes .LT. 0.001) fGasRes = 0.001
    ENDIF

    ! Wells coming online loop
    f = 0
    !     DO t1 = toff,nTime
    DO t1 = 1, IJUMPYR

        nWls = prj_devDrlSch(iFSC, t1)  ! Number of wells come online at this time

        ! Store number of operating wells
        n = 0
        IF (t1 .GT. 1) n = prj_nOprWls(iFSC, t1 - 1)
        prj_nOprWls(iFSC, t1) = n + nWls
        IF (t1 .GT. 1) THEN
            DO FF = 1,prj%nFld(iFSC)
                prj_fNwls(iFSC, FF, t1) = prj_fNwls(iFSC, FF, t1 - 1)
            ENDDO
        ENDIF

        IF (nWls .GT. 0) THEN

            ! Distribute wells to the fields
            n = 0
            DO WHILE (.True.)
                ! Field index counter (remember the last field for next t1 loop)
                f = f + 1
                IF (f .GT. prj%nFld(iFSC)) f = 1
                fNwls(f) = fNwls(f) + 1  ! Add one well to a field
                ! Store number of operating wells by field
                prj_fNwls(iFSC, f, t1) = prj_fNwls(iFSC, f, t1) + 1
                n = n + 1  ! Number of wells that have beed assigned to the fields
                IF (n .EQ. nWls)  EXIT
            ENDDO
        ENDIF
    ENDDO

    ! Production profile loop - adapted from AnnouncedDiscoveryProductionProfile subroutine
    !DO tt = toff,nTime
    DO tt = 1, IJUMPYR

        ! Produce the field
        DO f = 1,prj%nFld(iFSC)

            !IF (prj_fNwls(iFSC,f,tt) .GT. 0) THEN
            !fNwls(f) = prj_fNwls(iFSC, f, tt)

            ! Oil production rate (Bbl/Field/Year)
            IF (tt .le. ANN_RAMPUP) THEN
                IF (foilres .gt. 0. .and. fOilCum(f) / fOilRes .gt.  prj%FrcprdOil) THEN
                    qo = (prj%iprdOrate + (((prj%mprdOrate - prj%iprdOrate)/ANN_RAMPUP)*tt))* 365.*&
                        ((1. + (prj%prdODec * (tt - tdecOil(f)))) ** (-1.0 / ANN_HYP))
                ELSE
                    vo = prj%iprdOrate + (((prj%mprdOrate - prj%iprdOrate)/ANN_RAMPUP)*(tt))	!Bbl/d
                    qo = vo * 365 ! Bbl/yr
                    tdecOil(f) = tt
                ENDIF
            ELSE
                IF (foilRes.gt.0.) THEN
                    IF (fOilCum(f) / fOilRes .LE.  prj%FrcprdOil) THEN
                        qo = prj%mprdOrate * 365.
                        tdecOil(f) = tt
                    Else
                        qo = prj%mprdOrate * 365. * ((1. + (prj%prdODec * (tt - tdecOil(f)))) ** (-1.0 / ANN_HYP))
                    ENDIF
                ELSE
                    qo = 0.
                ENDIF
            ENDIF

            !qo = qo * fNwls(f) * (1.-GOProp(iPA,iEU))
            !qo = qo * fNwls(f)

            ! Cumulative oil produced
            fOilCum(f) = fOilCum(f) + qo
            IF (fOilCum(f) .GE. fOilRes) THEN
                fOilCum(f) = fOilCum(f) - qo
                qo = fOilRes - fOilCum(f)
                fOilCum(f) = fOilRes
            ENDIF

            ! Non-associated gas production rate (Mcf/Field/Year)
            IF (tt .lt. 1+ANN_RAMPUP .and. fGasRes .ne. 0.0) THEN
                IF (foilres .gt. 0. .and. fGasCum(f) / fGasRes .gt.  prj%FrcprdGas) THEN
                    qg = (prj%iprdGrate + (((prj%mprdGrate - prj%iprdGrate)/ANN_RAMPUP)*(tt)))* 365.*&
                        ((1. + (prj%prdGDec * (tt - tdecGas(f)))) ** (-1.0 / ANN_HYP))
                ELSE
                    vg = prj%iprdGrate + (((prj%mprdGrate - prj%iprdGrate)/ANN_RAMPUP)*(tt))	!Mcf/d
                    qg = vg * 365		!Mcf/yr
                    tdecGas(f) = tt
                ENDIF

            ELSE
                IF (fGasRes .GT. 0.) THEN
                    IF (fGasCum(f) / fGasRes .LE.  prj%FrcprdGas) THEN
                        qg = prj%mprdGrate * 365.
                        tdecGas(f) = tt
                    ELSE
                        qg = prj%mprdGrate * 365. * ((1. + (prj%prdGDec * (tt - tdecGas(f)))) ** (-1.0 / ANN_HYP))
                    ENDIF
                ELSE
                    qg = 0.
                ENDIF
            ENDIF

            !qg = qg * fNwls(f) * GOProp(iPA,iEU)
            !qg = qg * fNwls(f)

            ! Cumulative gas production
            fGasCum(f) = fGasCum(f) + qg
            IF (fGasCum(f) .GT. fGasRes) THEN
                fGasCum(f) = fGasCum(f) - qg
                qg = fGasRes - fGasCum(f)
                fGasCum(f) = fGasRes
            ENDIF

            ! Associated gas portion from oil (Mcf/Field/Year) (GOR=Scf/Bbl)
            qad = qo *  GOR(iPA, iEU) / 1000.
            v1 = qad /  BOEtoMcf
            ! This is a gas field
            IF (v1 .GT. qo) qad = qo *  BOEtoMcf

            ! Condensate portion from gas (Bbl/Field/Year)
            ! (Condensate Yield=Bbl/MMcf)
            adjfac = 1.0
            !IF (oit_wop(IJUMPYR,1).gt.25.) adjfac = 2.
            !IF (oit_wop(IJUMPYR,1).lt.18.) adjfac = 0.5
            qcnd = (qg / 1000.) *  cndYld(iPA, iEU) * adjfac
            v1 = qcnd *  BOEtoMcf

            ! This is an oil field
            IF (v1 .GT. qg) qcnd = qg /  BOEtoMcf

            ! Remove associated gas from oil (Bbl/Field/Year)
            qo = qo - qad /  BOEtoMcf
            IF (qo .LT. 0.) qo = 0.

            ! Remove condensate from non-associated gas (Mcf/Field/Year)
            qg = qg - qcnd *  BOEtoMcf
            IF (qg .LT. 0.) qg = 0.

            ! Store oil field production
            prj_oilPrd(iFSC, tt) = prj_oilPrd(iFSC, tt) + qo

            ! Store non-associated gas field production
            prj_gasPrd(iFSC, tt) = prj_gasPrd(iFSC, tt) + qg

            ! Store associated gas field production
            prj_asgPrd(iFSC, tt) = prj_asgPrd(iFSC, tt) + qad

            ! Store condensate field production
            prj_cndPrd(iFSC, tt) = prj_cndPrd(iFSC, tt) + qcnd

!            WRITE(OFFOUT,50) 'FLDPRD',nmEU(iPA,iEU),iFSC,yearof(toff),tt,fnwls(f),qo,qg,qad,qcnd,foilcum(f),foilres,fgascum(f),fgasres
!50          FORMAT(A8,2X,A15,I3,3I5,8F12.0)
!
!            WRITE(offout,*) 'FLDPRD', tt, 'oil prd ', prj_oilPrd(iFSC, tt), ' gas prd ', prj_gasPrd(iFSC, tt), ' ad gas ', prj_asgPrd(iFSC, tt),&
!                ' condensate ', prj_cndPrd(iFSC, tt)
!
!            WRITE(offout,*) 'FLDPRD', tt, 'oil res ', foilres, ' gas res ', fgasres, ' oil prd ', qo, ' gas prd ', qg

            !ENDIF
        ENDDO
    ENDDO

ENDIF

! Calculate transportation cost and set cost schedule (MM$/Year)
! ReDim prj%trnCst(5 To 20, 1 To nTime) As Double
prj_trnCst = 0.
IF (prj%nFld(iFSC) .GT. 0) THEN
    !DO t1 = toff,nTime
    DO t1 = 1,IJUMPYR
        prj_trnCst(iFSC, t1) = ( &
            (prj_oilPrd(iFSC, t1) + prj_cndPrd(iFSC, t1)) *  trnTrfOil(iPA, iEU) + &
            (prj_gasPrd(iFSC, t1) + prj_asgPrd(iFSC, t1)) *  trnTrfGas(iPA, iEU)) &
            / 1000000.
    ENDDO
ENDIF

! Calculate revenue from production and set revenue schedule (MM$/Year)
!     ReDim prj%prdRev(5 To 20, 1 To nTime) As Double
prj_prdRev = 0.
IF (prj%nFld(iFSC) .GT. 0) THEN
    !         DO t1 = toff,nTime
    DO t1 = 1, IJUMPYR
        prj_prdRev(iFSC, t1) = (prj_oilPrd(iFSC, t1) *  dcrdwhp(L48RGN+2,prj%yrPrdStart)*MC_JPGDP(yrCstTbl-BASEYR+1)/MC_JPGDP(-2) + &
            !           prj_prdRev(iFSC, t1) = (prj_oilPrd(iFSC, t1) *  rfcrudewhp(4,3,prj%yrPrdStart)*MC_JPGDP(yrCstTbl-BASEYR+1)/MC_JPGDP(-2) + &
            (prj_gasPrd(iFSC, t1) + prj_asgPrd(iFSC, t1)) *  ogwprng(L48RGN+2,prj%yrPrdStart)*MC_JPGDP( yrCstTbl-BASEYR+1)/MC_JPGDP(-2) + &
            !             (prj_gasPrd(iFSC, t1) + prj_asgPrd(iFSC, t1)) *  oghhprng(prj%yrPrdStart)*MC_JPGDP( yrCstTbl-BASEYR+1)/MC_JPGDP(-2) + &
            prj_cndPrd(iFSC, t1) *  dcrdwhp(L48RGN+2,prj%yrPrdStart)*MC_JPGDP(yrCstTbl-BASEYR+1)/MC_JPGDP(-2)) / 1000000.
        !             prj_cndPrd(iFSC, t1) *  rfcrudewhp(4,3,prj%yrPrdStart)*MC_JPGDP(yrCstTbl-BASEYR+1)/MC_JPGDP(-2)) / 1000000.
        IF (t1.gt.1) THEN
            prj_prdCum(iFSC,t1) = prj_prdCum(iFSC,t1-1) + prj_oilPrd(iFSC,t1) + prj_cndPrd(iFSC,t1)  &
                + (prj_gasPrd(iFSC,t1) + prj_asgPrd(iFSC,t1))/BOEtoMCF
        ELSE
            prj_prdCum(iFSC,t1) = prj_oilPrd(iFSC,t1) + prj_cndPrd(iFSC,t1)  &
                + (prj_gasPrd(iFSC,t1) + prj_asgPrd(iFSC,t1))/BOEtoMCF
        ENDIF
    ENDDO
ENDIF

! Perform both exploration and development project economic
prj%expNPV = 0.
prj%devNPV = 0.

! Start with economic calculation for development project
DO iEcon = 1,2

    ! Exploration economic starts here
    ! ExplorationEconomic:

    ! Allocate memory/reset economic variables
    !ReDim prj%GRev(1 To nTime) As Double
    !ReDim prj%ROY(1 To nTime) As Double
    !ReDim prj%NRev(1 To nTime) As Double
    !ReDim prj%STax(1 To nTime) As Double
    !ReDim prj%OCst(1 To nTime) As Double
    !ReDim prj%TCst(1 To nTime) As Double
    !ReDim prj%Inv(1 To nTime) As Double
    !ReDim prj%TInv(1 To nTime) As Double
    !ReDim prj%IInv(1 To nTime) As Double
    !ReDim prj%BTCF(1 To nTime) As Double
    !ReDim prj%DBas(1 To nTime) As Double
    !ReDim prj%DExp(1 To nTime) As Double
    !ReDim prj%EInv(1 To nTime) As Double
    !ReDim prj%NIBT(1 To nTime) As Double
    !ReDim prj%SITax(1 To nTime) As Double
    !ReDim prj%FTInc(1 To nTime) As Double
    !ReDim prj%FITax(1 To nTime) As Double
    !ReDim prj%NIAT(1 To nTime) As Double
    !ReDim prj%ATCF(1 To nTime) As Double
    prj_GRev = 0.
    prj_ROY = 0.
    prj_NRev = 0.
    prj_STax = 0.
    prj_OCst = 0.
    prj_TCst = 0.
    prj_Inv = 0.
    prj_TInv = 0.
    prj_IInv = 0.
    prj_BTCF = 0.
    prj_DBas = 0.
    prj_DExp = 0.

!==========================> HSM Code Start <==========================
    hsm_amor = 0.0
    hsm_depr = 0.0
!===========================> HSM Code End <===========================

    prj_EInv = 0.
    prj_NIBT = 0.
    prj_SITax = 0.
    prj_FTInc = 0.
    prj_FITax = 0.
    prj_NIAT = 0.
    prj_ATCF = 0.
    prj_acst = 0.

    IF (iEcon .NE. 1) THEN

        ! Investment, Tangible Investment, Intangible Investment:
        ! => Exploration drilling cost (MM$/Year) (at the beginning of the project)
        IF (prj%nFld(iFSC) .GT. 0) THEN
            prj_Inv(1) = prj_Inv(1) + prj%expDrlCst(iFSC)
            prj_TInv(1) = prj_TInv(1) + prj%expDrlCst(iFSC) *  expTangFrc
            prj_IInv(1) = prj_IInv(1) + prj%expDrlCst(iFSC) * (1. -  expTangFrc)
        ENDIF

    ENDIF

    ! Development economic starts here (without exploration cost)

    !   DO t1 = toff,ntime
    DO t1 = 1, IJUMPYR

        !Investment, Tangible Investment, Intangible Investment:
        ! => Production facility cost (MM$/Year)
        prj_Inv(t1) = prj_Inv(t1) + prj_pfCst(t1)
        prj_TInv(t1) = prj_TInv(t1) + prj_pfCst(t1) *  pfTangFrc
        prj_IInv(t1) = prj_IInv(t1) + prj_pfCst(t1) * (1. -  pfTangFrc)


        IF (prj%nFld(iFSC) .GT. 0) THEN

            IF (iEcon .NE. 2) THEN          ! Do Economic Limit Check

                ! *** FIELD ECONOMIC LIMIT CALCULATION *************************
                ! Economic Limit = Revenue - Costs - Royalties - Taxes
                ! => Stop field production if "Economic Limit" <= 0

                IF (prj_prdRev(iFSC, t1) .GT. 0) THEN

                    ! Revenue from production (MM$/Year)
                    netRevenue = prj_prdRev(iFSC, t1)

                    ! Royalties (MM$/Year)
                    v1 = prj_prdRev(iFSC, t1) *  royRate(iPA, iEU, iFSC)
                    netRevenue = netRevenue - v1

                    ! Operating cost (MM$/Year)
                    v1 = prj_nOprWls(iFSC, t1) * &
                        OprCost(iPA, iEU, iFSC,  wDpth(iPA, iEU, iFSC),  dDpth(iPA, iEU, iFSC), t1+toff-1)
                    netRevenue = netRevenue - v1

                    ! Transportation cost (MM$/Year)
                    netRevenue = netRevenue - prj_trnCst(iFSC, t1)

                    ! Severance taxes (MM$/Year)
                    v2 = ((prj_oilPrd(iFSC, t1) + prj_cndPrd(iFSC, t1)) *  dcrdwhp(L48RGN+2,prj%yrPrdStart)*MC_JPGDP(yrCstTbl-BASEYR+1)/MC_JPGDP(-2)) / 1000000.
                    !             v2 = ((prj_oilPrd(iFSC, t1) + prj_cndPrd(iFSC, t1)) *  rfcrudewhp(4,3,prj%yrPrdStart)*MC_JPGDP(yrCstTbl-BASEYR+1)/MC_JPGDP(-2)) / 1000000.
                    netRevenue = netRevenue - v1 * v2 * ( oilSevTaxRate / 100.)

                    v2 = prj_oilPrd(iFSC, t1) + prj_cndPrd(iFSC, t1)  ! Liquid production
                    netRevenue = netRevenue - v2 *  oilSevTaxPrd / 1000000.

                    v2 = (prj_gasPrd(iFSC, t1) + prj_asgPrd(iFSC, t1)) * ogwprng(L48RGN+2,prj%yrPrdStart)*MC_JPGDP(yrCstTbl-BASEYR+1)/MC_JPGDP(-2) / 1000000.
                    !             v2 = (prj_gasPrd(iFSC, t1) + prj_asgPrd(iFSC, t1)) * oghhprng(prj%yrPrdStart)*MC_JPGDP(yrCstTbl-BASEYR+1)/MC_JPGDP(-2) / 1000000.
                    netRevenue = netRevenue - v1 * v2 * ( gasSevTaxRate / 100.)

                    v2 = prj_gasPrd(iFSC, t1) + prj_asgPrd(iFSC, t1)  ! Gas production
                    netRevenue = netRevenue - v2 *  gasSevTaxPrd / 1000000.

                    ! State and federal taxes (these could be negative)
                    stateTax = netRevenue * ( stTaxRate(iPA, iEU) / 100.)
                    fedTax = (netRevenue - stateTax) * ( fedTaxRate / 100.)

                    ! Field reaches economic limit => STOP FIELD PRODUCTION
                    econLimit = netRevenue - stateTax - fedTax
                    IF (econLimit .LE. 0.) THEN
                        prj_aCst(t1) = abandoncst
                        DO tt = t1,IJUMPYR
                            prj_oilPrd(iFSC, tt) = 0.
                            prj_gasPrd(iFSC, tt) = 0.
                            prj_asgPrd(iFSC, tt) = 0.
                            prj_cndPrd(iFSC, tt) = 0.
                            prj_prdRev(iFSC, tt) = 0.
                            prj_trnCst(iFSC, tt) = 0.
                            prj_nOprWls(iFSC, tt) = 0
                            DO FF = 1,prj%nFld(iFSC)
                                prj_fNwls(iFSC, FF, tt) = 0
                            ENDDO
                        ENDDO
                    ENDIF

                ENDIF
                !*** END ECONOMIC LIMIT CALCULTION ****************************

            ENDIF                           ! Skip Economic Limit Check

            ! Investment, Tangible Investment, Intangible Investment:
            ! => Development drilling cost (MM$/Year)
            prj_Inv(t1) = prj_Inv(t1) + prj_devDrlCst(iFSC, t1)
            prj_TInv(t1) = prj_TInv(t1) + prj_devDrlCst(iFSC, t1) *  devTangFrc
            prj_IInv(t1) = prj_IInv(t1) + prj_devDrlCst(iFSC, t1) * (1. -  devTangFrc)

            ! Operating cost (MM$/Year)
            v1 = prj_nOprWls(iFSC, t1) * &
                OprCost(iPA, iEU, iFSC, wDpth(iPA, iEU, iFSC),  dDpth(iPA, iEU, iFSC), t1)
            prj_OCst(t1) = prj_OCst(t1) + v1

            ! Transportation cost (MM$/Year)
            prj_TCst(t1) = prj_TCst(t1) + prj_trnCst(iFSC, t1)

            ! Gross revenue (MM$/Year)
            prj_GRev(t1) = prj_GRev(t1) + prj_prdRev(iFSC, t1)

            ! Royalties (MM$/Year)
            v1 = prj_prdRev(iFSC, t1) *  royRate(iPA, iEU, iFSC)
            prj_ROY(t1) = prj_ROY(t1) + v1

            ! Net revenue (MM$/Year)
            v1 = 1. -  royRate(iPA, iEU, iFSC)
            prj_NRev(t1) = prj_NRev(t1) + prj_prdRev(iFSC, t1) * v1

            ! Severance taxes (MM$/Year)
            v2 = ((prj_oilPrd(iFSC, t1) + prj_cndPrd(iFSC, t1)) *  dcrdwhp(L48RGN+2,prj%yrPrdStart)*MC_JPGDP(yrCstTbl-BASEYR+1)/MC_JPGDP(-2)) / 1000000.
            !         v2 = ((prj_oilPrd(iFSC, t1) + prj_cndPrd(iFSC, t1)) *  rfcrudewhp(4,3,prj%yrPrdStart)*MC_JPGDP(yrCstTbl-BASEYR+1)/MC_JPGDP(-2)) / 1000000.
            prj_STax(t1) = prj_STax(t1) + v1 * v2 * ( oilSevTaxRate / 100.)

            v2 = prj_oilPrd(iFSC, t1) + prj_cndPrd(iFSC, t1)
            prj_STax(t1) = prj_STax(t1) + v2 *  oilSevTaxPrd

            v2 = (prj_gasPrd(iFSC, t1) + prj_asgPrd(iFSC, t1)) *  ogwprng(L48RGN+2,prj%yrPrdStart)*MC_JPGDP(yrCstTbl-BASEYR+1)/MC_JPGDP(-2) / 1000000.
            !         v2 = (prj_gasPrd(iFSC, t1) + prj_asgPrd(iFSC, t1)) *  oghhprng(prj%yrPrdStart)*MC_JPGDP(yrCstTbl-BASEYR+1)/MC_JPGDP(-2) / 1000000.
            prj_STax(t1) = prj_STax(t1) + v1 * v2 * ( gasSevTaxRate / 100.)

            v2 = prj_gasPrd(iFSC, t1) + prj_asgPrd(iFSC, t1)
            prj_STax(t1) = prj_STax(t1) + v2 *  gasSevTaxPrd

        ENDIF


        ! Before tax cashflow (MM$/Year)
        prj_BTCF(t1) = prj_NRev(t1) - prj_STax(t1) - prj_OCst(t1) - prj_TCst(t1) - prj_Inv(t1)

        ! Depreciation base (MM$/Year)
        prj_DBas(t1) = prj_TInv(t1) + 0.3 * prj_IInv(t1)

        ! Depreciation expense (MM$/Year)
        i = 0
        tend = t1 + 8 - 1
        !     IF (tend .GT. nTime) tend = nTime
        IF (tend .GT. IJUMPYR) tend = IJUMPYR
        DO tt = t1,tend
            i = i + 1
            prj_DExp(tt) = prj_DExp(tt) + prj_DBas(t1) *  deprSch(i)

!==========================> HSM Code Start <==========================
            hsm_amor(tt) = hsm_amor(tt) + prj_IInv(t1) *  deprSch(i) * 0.3
            hsm_depr(tt) = hsm_depr(tt) + prj_TInv(t1) *  deprSch(i)
!===========================> HSM Code End <===========================

        ENDDO

        ! Expensed investments (MM$/Year)
        prj_EInv(t1) = 0.7 * prj_IInv(t1)

        ! Net income before taxes (MM$/Year)
        prj_NIBT(t1) = prj_NRev(t1) - prj_STax(t1) - prj_OCst(t1) - prj_TCst(t1) - prj_DExp(t1) - prj_EInv(t1)

        ! State income taxes (MM$/Year)
        prj_SITax(t1) = prj_NIBT(t1) * ( stTaxRate(iPA, iEU) / 100.)

        ! Federal taxable income (MM$/Year)
        prj_FTInc(t1) = prj_NIBT(t1) - prj_SITax(t1)

        ! Federal income taxes (MM$/Year)
        prj_FITax(t1) = prj_FTInc(t1) * ( fedTaxRate / 100.)

        ! Net income after taxes (MM$/Year)
        prj_NIAT(t1) = prj_NIBT(t1) - prj_SITax(t1) - prj_FITax(t1)

        ! After tax cashflow (MM$/Year)
        prj_ATCF(t1) = prj_BTCF(t1) - prj_SITax(t1) - prj_FITax(t1)

        ! Net present value (MM$ at time=t)
        tcstCap = cstCAP
        IF (iPA.ge.3.and.iEU.ne.7) tcstCap = cstCAP*1.25
        IF (iEcon .EQ. 1) THEN
            prj%devNPV = prj%devNPV + prj_ATCF(t1) / (1. +  tcstCap / 100.) ** (t1 - 1)

!==========================> HSM Code Start <==========================
            hsm_npv = prj%devNPV
!===========================> HSM Code End <===========================

        ELSE
            prj%expNPV = prj%expNPV + prj_ATCF(t1) / (1. +  tcstCap / 100.) ** (t1 - 1)

!==========================> HSM Code Start <==========================
            hsm_npv = prj%expNPV
!===========================> HSM Code End <===========================

        ENDIF

        !IF (T1.EQ.1) WRITE(OFFOUT,*) 'Prospect [toff, ipa, ieu]', toff, ipa, ieu
        !IF (T1.EQ.1) WRITE(OFFOUT,100)
        !WRITE(OFFOUT,*) t1, prj_GRev(t1), prj_ROY(t1), prj_NRev(t1), prj_STax(t1), &
        !    prj_OCst(t1), prj_TCst(t1), prj_Inv(t1), prj_TInv(t1), &
        !    prj_IInv(t1), prj_BTCF(t1), prj_DBas(t1), prj_DExp(t1), &
        !    prj_EInv(t1), prj_NIBT(t1), prj_SITax(t1), prj_FTInc(t1), &
        !    prj_FITax(t1), prj_NIAT(t1), prj_ATCF(t1)
        !IF (T1.EQ.IJUMPYR) THEN
        !    IF (iEcon .EQ. 1) THEN
        !        WRITE(OFFOUT,*) 'Development NPV', yearof(toff), ipa, ieu, prj%devNPV
        !    ELSE
        !        WRITE(OFFOUT,*) 'Exploration NPV', yearof(toff), ipa, ieu, prj%expNPV
        !    ENDIF
        !ENDIF

    ENDDO


!==========================> HSM Code Start <==========================
if (hsm_dcf_bool) then
    ! 'function', 'year', 'crude_price', 'natgas_price', 'crude_trans_price', &
    ! 'natgas_trans_price', 'crude_tariff_price', 'natgas_tariff_price', 'royalty_rate', 'fed_tax_rate', &
    ! 'net_present_value', 'abandon_rate', 'state_abbreviation', 'exploratory_success_rate', 'development_success_rate', &
    ! 'exploratory_cost', 'development_cost', 'exploratory_dry_cost', 'development_dry_cost', 'exp_tang_frac', &
    ! 'dev_tang_frac', 'kap_tang_frac', 'intang_amor_frac'
    ! hsm inputs
    write(hsm_dcf_opts , '(*(G0.16,:,","))') 'DAD', curcalyr, (dcrdwhp(L48RGN+2,prj%yrPrdStart)*MC_JPGDP(yrCstTbl-BASEYR+1)/MC_JPGDP(-2)), &
                                             (ogwprng(L48RGN+2,prj%yrPrdStart)*MC_JPGDP(yrCstTbl-BASEYR+1)/MC_JPGDP(-2)), &
                                             trnTrfOil(iPA, iEU), trnTrfGas(iPA, iEU), 0.0, 0.0, royRate(iPA, iEU, iFSC), &
                                             fedTaxRate / 100., hsm_npv*1000000.0, abandoncst*1000000.0, 'OCS', 1.0, 1.0, &
                                             ExplCost(iPA, iEU,  wDpth(iPA, iEU, iFSC), dDpth(iPA, iEU, iFSC), toff), &
                                             DevlCost(iPA, iEU,  wDpth(iPA, iEU, iFSC), dDpth(iPA, iEU, iFSC), toff), &
                                             0.0, 0.0, expTangFrc, devTangFrc, pfTangFrc, 0.3, 'macrs_7', 'macrs_7', tcstCap, &
                                             nmEU(iPA,iEU), iFSC
    ! hsm inputs
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'DAD', iEcon, 'crude_production'     , SUM(prj_oilPrd + prj_cndPrd, DIM=1)
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'DAD', iEcon, 'natgas_production'    , SUM(prj_gasPrd + prj_asgPrd, DIM=1)
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'DAD', iEcon, 'exp_drill_cost'       , prj%expDrlCst(iFSC)*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'DAD', iEcon, 'dev_drill_cost'       , ((prj_devDrlCst(iFSC,M)*1000000.0), M=1,IJUMPYR)
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'DAD', iEcon, 'kap_cost'             , prj_pfCst*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'DAD', iEcon, 'operating_cost'       , prj_OCst*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'DAD', iEcon, 'tax_base_state'       , prj_NIBT*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'DAD', iEcon, 'tax_base_fed'         , prj_FTInc*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'DAD', iEcon, 'expn_intang_cost'     , prj_EInv*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'DAD', iEcon, 'amor_intang_cost'     , hsm_amor*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'DAD', iEcon, 'deprec_tang_cost'     , hsm_depr*1000000.0
    ! hsm checks
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'DAD', iEcon, 'revenue'              , SUM(prj_prdRev, DIM=1)*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'DAD', iEcon, 'transportation'       , prj_TCst*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'DAD', iEcon, 'royalty'              , prj_ROY*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'DAD', iEcon, 'severance_tax'        , prj_STax*1000000.0
    hsm_depr = 0.0
    hsm_depr(1) = prj%expDrlCst(iFSC)
    hsm_depr = hsm_depr + prj_devDrlCst(iFSC,1:IJUMPYR)
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'DAD', iEcon, 'drill_cost'           , hsm_depr*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'DAD', iEcon, 'state_tax'            , prj_SITax*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'DAD', iEcon, 'fed_tax'              , prj_FITax*1000000.0
    write(hsm_dcf_off  , '(*(G0.16,:,","))') 'DAD', iEcon, 'cash_flow'            , prj_ATCF*1000000.0
endif
!===========================> HSM Code End <===========================

ENDDO

! 100 FORMAT(' Year ', ' Gross Rev', '   Royalty', '   Net Rev', '   Sev Tax', &
!                      ' Oper Cost', ' Tran Cost', '  Drl Cost', '  Tang Cst', &
!                      ' Intan Cst', 'Pre-Tax CF', ' Depr Base', '  Depr Exp', &
!                      ' Exp Cost ', 'Net Inc BT', '    ST Tax', '   Tax Inc', &
!                      '  FED Tax ', 'Net Inc AT', 'Aft Tax CF')
! 200 FORMAT(I6,19F10.0)

! Save total project production and well counts
! DO t1 = toff,ntime
DO t1 = 1,IJUMPYR-toff+1
    !DO iFSC = minFSC,minFSC+numFSC-1
    !    DO FF = 1,prj%nFld(iFSC)
    !        prj%fnwls(t1+toff-1) = prj%fnwls(t1+toff-1) + prj_fnwls(iFSC, ff, t1)
    !    ENDDO
    prj%devdrlsch(t1+toff-1) = prj%devdrlsch(t1+toff-1) + prj_devdrlsch(iFSC, t1)
    prj%noprwls(t1+toff-1) = prj%noprwls(t1+toff-1) + prj_nOprWls(iFSC, t1)
    prj%oilPrd(t1+toff-1) = prj%oilPrd(t1+toff-1) + prj_oilPrd(iFSC, t1)
    prj%gasPrd(t1+toff-1) = prj%gasPrd(t1+toff-1) + prj_gasPrd(iFSC, t1)
    prj%asgPrd(t1+toff-1) = prj%asgPrd(t1+toff-1) + prj_asgPrd(iFSC, t1)
    prj%cndPrd(t1+toff-1) = prj%cndPrd(t1+toff-1) + prj_cndPrd(iFSC, t1)
    !ENDDO
ENDDO
                    
!==========================> HSM Code Start <==========================
    ! if (hsm_offshore_bool) then
    !     WRITE(hsm_out,'(*(G0.16,:,","))') Fld(f_num)%nmEU, Fld(f_num)%FSC, Fld(f_num)%nmid, f_num, 'oilPrd', 'dad', (prj%oilPrd(tt),tt=1,MNUMYR) 
    !     WRITE(hsm_out,'(*(G0.16,:,","))') Fld(f_num)%nmEU, Fld(f_num)%FSC, Fld(f_num)%nmid, f_num, 'gasPrd', 'dad', (prj%gasPrd(tt),tt=1,MNUMYR) 
    !     WRITE(hsm_out,'(*(G0.16,:,","))') Fld(f_num)%nmEU, Fld(f_num)%FSC, Fld(f_num)%nmid, f_num, 'asgPrd', 'dad', (prj%asgPrd(tt),tt=1,MNUMYR) 
    !     WRITE(hsm_out,'(*(G0.16,:,","))') Fld(f_num)%nmEU, Fld(f_num)%FSC, Fld(f_num)%nmid, f_num, 'cndPrd', 'dad', (prj%cndPrd(tt),tt=1,MNUMYR) 
    ! endif
!===========================> HSM Code End <===========================

!WRITE(OFFOUT,*) 'EXITING SUBROUTINE DevelopAnnouncedDiscovery, YEAR =', yearof(toff), ' devNPV ', prj%devNPV, &
!  ' oil price ', dcrdwhp(L48RGN+2, prj%yrPrdStart)

END SUBROUTINE DevelopAnnouncedDiscovery

!************************************************************************************!
! Subroutine to determine possible exploration projects for each EU
! =.GT. ONLY 1 PROJECT FOR EACH TYPE
!************************************************************************************!
SUBROUTINE DeterminePossibleExplorationProjects
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ncntrl'       ! nems control variables
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmoff'      ! offshore variables
INCLUDE 'macout'
INCLUDE 'pmmout'
INCLUDE 'ngtdmrep'
INCLUDE 'dwpparm'

INTEGER i, f, iReg, iPA, iEU, &
    iFSC, iFSC1, iFSC2, iFld, iPF, &
    tAvl, nWls, lastFSC, nOpt, &
    iOpt, nFld, nFld1, nFld2, &
    n1, n2, f1, f2, iNPF, j, t1, n3
LOGICAL newProject
INTEGER iFSCOpt(1:16)
INTEGER nextInt
TYPE (Projectobj) tempPrj
INTEGER lagcumdisc(nPA,nMaxEU,minFSC:minFSC+numFSC-1)
INTEGER uNRRold(nPA,nMaxEU,minFSC:minFSC+numFSC-1)
INTEGER DELAYYR
REAL*4 SC
REAL*4 DELAYTMP(nPA,nMaxEU)
INTEGER itmp
INTEGER EffPa
Integer tempavl ! apr 03/02/2020 added as place holder for available field for do loop (uNRRavl(iPA, iEU, iFSC))

! WRITE(OFFOUT,*) 'ENTERING SUBROUTINE DeterminePossibleExplorationProjects, YEAR =', yearof(toff) ! DBG_Flag_Ent_Ext_Sub

! Reset exploration projects
! ReDim psbExpPrj(0)
DO I = 1,MAXEXPPRJ
    CALL INITPROJECT(psbExpPrj(I))
ENDDO

! Assign field ID based on current status of the field (category)
CALL AssignCurrentFieldID

! Number of possible projects
nPsbExpPrj = 0
!uNRRavl = 0. ! apr 01/20/2020 moved to OGINIT_OFF so accumulated year to year

! Determine number of new field wildcats drilled in Central and Western GOM
DO iPA = 1, 3
    DO iEU = 1, nEU(iPA)
        IF (YRAVL(iPA,iEU).LE.YEAROF(toff)) THEN
            TOTNFW(iPA,iEU) = INT((ALPHA1(iPA,iEU) + ALPHA2(iPA,iEU) &
                + BETA(iPA,iEU)*(dcrdwhp(mnumor,curiyr-nlagyrs(iPA,iEU))*ogwprng(mnumor,curiyr-nlagyrs(iPA,iEU)))))* (1.+toff*(LEVEXPSUCRATE(1)/LEVEXPSUCRATE(2))/100.)
            IF (YEAROF(toff).ge.ASSESS_YR) CUMNFW(iPA,iEU,toff+offbyr-1) = cumnfw(iPA,iEU,toff+offbyr-2) + TOTNFW(iPA,iEU)
            !WRITE(offout,'(*(G0.6,:,"'//achar(9)//'"))') 'NFW', CURIYR+1989, ' ', nmEU(iPA,iEU), TOTNFW(iPA,iEU), CUMNFW(iPA,iEU,toff+offbyr-1) ! DBG_Flag_NFW_Breakdown (1/2)
            
!==========================> HSM Code Start <==========================
            ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, ALPHA1(iPA,iEU) , ALPHA2(iPA,iEU), BETA(iPA,iEU), dcrdwhp(mnumor,curiyr-nlagyrs(iPA,iEU)), &
            !     ogwprng(mnumor,curiyr-nlagyrs(iPA,iEU)), TOTNFW(iPA,iEU), CUMNFW(iPA,iEU,toff+offbyr-1)
!===========================> HSM Code End <===========================
            
        ENDIF
    ENDDO
ENDDO
DO iPA = 4, nPA
    DO iEU = 1, nEU(iPA)
        IF (YRAVL(iPA,iEU).LE.YEAROF(toff)) THEN
            TOTNFW(iPA,iEU) = INT((ALPHA1(3,iEU) + ALPHA2(3,iEU) &
                + BETA(3,iEU)*(dcrdwhp(mnumor,curiyr-nlagyrs(3,iEU))*ogwprng(mnumor,curiyr-nlagyrs(3,iEU)))))* (1.+toff*(LEVEXPSUCRATE(1)/LEVEXPSUCRATE(2))/100.)
            IF (iPA.ge.4.and.iPA.le.8.and.dcrdwhp(mnumor,curiyr).lt.50.) totnfw(iPA,iEU) = 0.
            IF (YEAROF(toff).ge.ASSESS_YR) CUMNFW(iPA,iEU,toff+offbyr-1) = cumnfw(iPA,iEU,toff+offbyr-2) + TOTNFW(iPA,iEU)
            !WRITE(offout,'(*(G0.6,:,"'//achar(9)//'"))') 'NFW', CURIYR+1989, ' ', nmEU(iPA,iEU), TOTNFW(iPA,iEU), CUMNFW(iPA,iEU,toff+offbyr-1), dcrdwhp(mnumor,curiyr) ! DBG_Flag_NFW_Breakdown (2/2)
            
!==========================> HSM Code Start <==========================
            ! WRITE(hsm_out,'(*(G0.16,:,","))') curcalyr, ALPHA1(3,iEU) , ALPHA2(3,iEU), BETA(3,iEU), dcrdwhp(mnumor,curiyr-nlagyrs(3,iEU)), &
            !     ogwprng(mnumor,curiyr-nlagyrs(3,iEU)), TOTNFW(iPA,iEU), CUMNFW(iPA,iEU,toff+offbyr-1)
!===========================> HSM Code End <===========================
            
        ENDIF
    ENDDO
ENDDO


! Store number of explorable undiscovered fields
!WRITE(offout,*) 'discflds', yearof(toff) ! DBG_Flag_CumDis_NRR (2/4)
DO iPA = 1, nPA
    DO iEU = 1, nEU(iPA)
        DO iFSC = minFSC,minFSC+numFSC-1

            !uNRRavl(iPA, iEU, iFSC) = max(nextInt( uNRR(iPA, iEU, iFSC) *  fldExpRate(iPA, iEU, iFSC) / 100.),1) ! apr 01/31/2020 old discovery approach
            !IF (uNRRavl(iPA, iEU, iFSC) .GT.  uNRR(iPA, iEU, iFSC)) uNRRavl(iPA, iEU, iFSC) =  uNRR(iPA, iEU, iFSC) ! apr 01/31/2020 old discovery approach
            !uNRRold(iPA, iEU, iFSC) = uNRRavl(iPA, iEU, iFSC)  ! apr 01/31/2020 old discovery approach

            ! Use Arps Roberts Model to determine number of discovered fields
            IF (YRAVL(iPA,iEU).LE.YEAROF(toff)) THEN

                ! apr 02/12/2020, simplifying non-gulf discovery equations to reduce redundancy
                IF (iPA.le.3) THEN
                    EffPa = iPA
                ELSE
                    EffPa = 3
                ENDIF
                ! apr 02/12/2020, calculate SC, to be revisited
                !SC = SC10(iPA,iEU)*((beta1_sc(iPA)*iFSC**2 + beta2_sc(iPA)*iFSC + alpha_sc(iPA)) * (1.+toff*(LEVEXPSUCRATE(1)/LEVEXPSUCRATE(2))/100.))
                !SC = (beta1_sc(EffPa)*iFSC**2 + beta2_sc(EffPa)*iFSC + alpha_sc(EffPa)) * SC10(EffPa,iEU) * (1.+toff*(LEVEXPSUCRATE(1)/LEVEXPSUCRATE(2))/100.)

                lagcumdisc(iPA,iEU,iFSC) = cumdisc(iPA,iEU,iFSC)

                !IF (toff.eq.1) lagcumdisc(iPA,iEU,iFSC) = INT(NRR(iPA,iEU,iFSC)*(1-exp(-SC*CUMNFW(iPA,iEU,offbyr))))
                !cumdisc(iPA,iEU,iFSC) = INT(dble(dble(NRR(iPA,iEU,iFSC))*dble(1-dble(exp(-SC*dble(CUMNFW(iPA,iEU,toff+offbyr-1)))))))

                ! Rounding error means you can never find the final field in a FSC, if statement prevents negative fields when all of the fields in a class have been discovered. Needs to be revisited apr 02/14/2020
                IF (NRR(iPA,iEU,iFSC).NE.cumdisc(iPA,iEU,iFSC)) THEN
                    cumdisc(iPA,iEU,iFSC) = NRR(iPA,iEU,iFSC)*(1.0-exp((-FLDEXPRATE(iPA,iEU,iFSC)/NRR(iPA,iEU,iFSC))*CUMNFW(iPA,iEU,toff+offbyr-1)))
                ENDIF

                !IF (iPA.eq.3) cumdisc(iPA,iEU,iFSC) = ANINT(dble(dble(NRR(iPA,iEU,iFSC))*dble(1-dble(exp(-SC*dble(CUMNFW(iPA,iEU,toff+offbyr-1)))))))
                !uNRRavl(iPA,iEU,iFSC) = cumdisc(iPA,iEU,iFSC) - lagcumdisc(iPA,iEU,iFSC)
                uNRRavl(iPA,iEU,iFSC) = uNRRavl(iPA,iEU,iFSC) + cumdisc(iPA,iEU,iFSC) - lagcumdisc(iPA,iEU,iFSC)
                IF (uNRRavl(iPA,iEU,iFSC).le.-1) WRITE(offout,'(*(G0.6,:,"'//achar(9)//'"))') 'uNRR-avl error', iFSC, uNRRavl(iPA,iEU,iFSC), lagcumdisc(iPA,iEU,iFSC),cumdisc(iPA,iEU,iFSC),NRR(iPA,iEU,iFSC),FLDEXPRATE(iPA,iEU,iFSC),CUMNFW(iPA,iEU,toff+offbyr-1) ! DBG_Flag_CumDis_NRR (4/4)
                !    IF (uNRRavl(iPA,iEU,iFSC).GT.uNRR(iPA,iEU,iFSC)) uNRRavl(iPA,iEU,iFSC) = uNRR(iPA,iEU,iFSC)
                !    apr 02/12/2020,
                !ELSE
                !    !SC = SC10(3,iEU)*((beta1_sc(3)*iFSC**2 + beta2_sc(3)*iFSC + alpha_sc(3)) * (1.+toff*(LEVEXPSUCRATE(1)/LEVEXPSUCRATE(2))/100.))
                !    SC = (beta1_sc(3)*iFSC**2 + beta2_sc(3)*iFSC + alpha_sc(3)) * SC10(3,iEU) * (1.+toff*(LEVEXPSUCRATE(1)/LEVEXPSUCRATE(2))/100.)
                !    lagcumdisc(iPA,iEU,iFSC) = cumdisc(iPA,iEU,iFSC)
                !    IF (toff.eq.1) lagcumdisc(iPA,iEU,iFSC) = INT(NRR(iPA,iEU,iFSC)*(1-exp(-SC*CUMNFW(iPA,iEU,offbyr))))
                !    cumdisc(iPA,iEU,iFSC) = ANINT(dble(dble(NRR(iPA,iEU,iFSC))*dble(1-dble(exp(-SC*dble(CUMNFW(iPA,iEU,toff+offbyr-1)))))))
                !    uNRRavl(iPA,iEU,iFSC) = uNRRavl(iPA,iEU,iFSC) + cumdisc(iPA,iEU,iFSC) - lagcumdisc(iPA,iEU,iFSC)
                !    IF (uNRRavl(iPA,iEU,iFSC).GT.uNRR(iPA,iEU,iFSC)) uNRRavl(iPA,iEU,iFSC) = uNRR(iPA,iEU,iFSC)
                !ENDIF
                IF (yearof(toff).le.yrendprdhis) THEN
                    uNRRavl(iPA,iEU,iFSC) = 0     ! discoveries have already been announced
                ENDIF
            ENDIF
        ENDDO
        IF (YRAVL(iPA,iEU).LE.YEAROF(toff)) THEN
            !IF (iPA.eq.3) WRITE(offout,100) 'cumdiscs-und',nmEU(iPA,iEU),yearof(toff),(cumdisc(iPA,iEU,iFSC),iFSC=minFSC,minFSC+numFSC-1),CUMNFW(iPA,iEU,toff+offbyr-1) ! DBG_Flag_CumDis_NRR (2/4)
            WRITE(offout,'(*(G0.6,:,"'//achar(9)//'"))') 'discflds-cumdisc',nmEU(iPA,iEU),yearof(toff),(cumdisc(iPA,iEU,iFSC),iFSC=minFSC,minFSC+numFSC-1),CUMNFW(iPA,iEU,toff+offbyr-1) ! DBG_Flag_CumDis_NRR (3/4)
            WRITE(offout,'(*(G0.6,:,"'//achar(9)//'"))') 'discflds-NRR',nmEU(iPA,iEU),yearof(toff),(NRR(iPA,iEU,iFSC),iFSC=minFSC,minFSC+numFSC-1),CUMNFW(iPA,iEU,toff+offbyr-1) ! DBG_Flag_CumDis_NRR (3/4)
            WRITE(offout,'(*(G0.6,:,"'//achar(9)//'"))') 'discflds-und',nmEU(iPA,iEU),yearof(toff),(uNRR(iPA,iEU,iFSC),iFSC=minFSC,minFSC+numFSC-1),CUMNFW(iPA,iEU,toff+offbyr-1) ! DBG_Flag_CumDis_NRR (3/4)
            WRITE(offout,'(*(G0.6,:,"'//achar(9)//'"))') 'discflds-avl',nmEU(iPA,iEU),yearof(toff),(uNRRavl(iPA,iEU,iFSC),iFSC=minFSC,minFSC+numFSC-1),CUMNFW(iPA,iEU,toff+offbyr-1) ! DBG_Flag_CumDis_NRR (4/4)
            
!==========================> HSM Code Start <==========================
if (hsm_offshore_bool) WRITE(hsm_offshore_disc,'(*(G0.16,:,","))') 'CumDisc', nmEU(iPA,iEU), curcalyr, (cumdisc(iPA,iEU,iFSC),iFSC=minFSC,minFSC+numFSC-1)
!===========================> HSM Code End <===========================
            
        ENDIF
    ENDDO
ENDDO
100 FORMAT(A15,2X,A15,I5,<numFSC>I6,I6)
110 FORMAT(A15,2X,A15,I5,<numFSC>I6)

! Determine development of already announced discoveries
!DELAYYR = INT(210./oit_WOP(mnumyr,1))
!write(offout,*) 'delayyr', yearof(toff), delayyr
!IF (yearof(toff).GT.yrBegForecast+DELAYYR) THEN
!    DO iPA = 1, nPA
!        DO iEU = 1, nEU(iPA)
!            n3 = 2
!            DO iFSC = minFSC+numFSC-1,minFSC,-1
!
!                IF ( duNRR(iPA, iEU, iFSC) .GT. 0 ) THEN
!
!                    DO iFld = 1, duNRR(iPA, iEU, iFSC)
!
!                        ! Field ID of discovered/undeveloped field (type 1)
!                        f = fldID(1, iPA, iEU, iFSC, iFld)
!
!                        IF (FLD(f)%YRSTPRD.GE.2030 .AND. FLD(f)%initTyp.EQ.1.AND.n3.GT.0) THEN
!                            Fld(f)%yrStPrd = toff + yrBegForecast - 1
!                            t1 = toff
!                            Fld(f)%yrDevStart = t1
!                            ! New/existing field flag
!                            CALL FieldProductionProfile(Fld(f), 0, t1, 0., 0., 0., 0., 0., 0.)
!                            n3 = n3 -1
!                            WRITE(offout,*) 'anndevel', yearof(toff), fld(f)%nmID, fld(f)%nickname
!                        ENDIF
!
!                    ENDDO
!                ENDIF
!
!            ENDDO
!        ENDDO
!    ENDDO
!ENDIF

DO iPA = 1, nPA
    DO iEU = 1, nEU(iPA)

        ! Check if fields in this EU can be explored and appraised at current year
        !IF (curiyr+1989.eq.(YRBEGFORECAST+1)) delaytmp(iPA,iEU) = exp2ndDly(iPA,iEU)
        !delaytmp(iPA,iEU) = delaytmp(iPA,iEU)/(1.+0.25*(royrated(iPA,iEU,11)-royrate(ipa,ieu,11)))
        !exp2ndDly(iPA,iEU) = Int(delaytmp(iPA,iEU))
        !write(offout,*) '2nddelay', curiyr+1989, nmEU(ipa,ieu), exp2ndDly(iPA,iEU)
        tAvl =  yrAvl(iPA, iEU) -  yrBegForecast + 1
        tAvl = tAvl +  exp1stDly(iPA, iEU) +  exp2ndDly(iPA, iEU)

        IF (tAvl .LE. toff) THEN     ! ELSE GOTO SKIPEU

            ! Get region ID
            iReg =  PAREG(iPA)

            ! Start from the biggest FSC
            DO iFSC = minFSC+numFSC-1, minFSC, -1

                ! apr 03/02/2020 old statement for determining if field is available, only allows one new field per year
                ! If field exists in this FSC
                !IF (uNRRavl(iPA, iEU, iFSC) .GT. 0) THEN
                ! apr 03/02/2020 do while to switch through all fields of the same location, depth and size
                tempavl = uNRRavl(iPA, iEU, iFSC)
                DO WHILE (tempavl.GT.0)
                    ! Check if this FSC is suitable for Option 1
                    !           f = fldID(0, iPA, iEU, iFSC, 1)
                    DO itmp = 1,ntfld
                        IF (fldID2(itmp).eq.0.and. &
                            fldID3(itmp).eq.iPA.and. &
                            fldID4(itmp).eq.iEU.and. &
                            fldID5(itmp).eq.iFSC.and. &
                            fldID6(itmp).eq.1) THEN
                        f = fldID1(itmp)
                        EXIT
                        ENDIF
                    ENDDO

                    ! Get platform and non-platform PF type index
                    iPF = 0
                    I=1
                    DO WHILE (wdpth(iPA,iEU,iFSC)*0.3048.GT.PFWDMIN(I))
                        I=I+1
                        IF (I.GT.5) EXIT
                    ENDDO
                    IF (pftype(i-1,iFSC).eq.'PF') iPF = 1
                    IF (pftype(i-1,iFSC).eq.'CT') iPF = 2
                    IF (pftype(i-1,iFSC).eq.'TLP') iPF = 3
                    IF (pftype(i-1,iFSC).eq.'FPS') iPF = 4
                    IF (pftype(i-1,iFSC).eq.'SPAR') iPF = 5
                    IF (pftype(i-1,iFSC).eq.'FPS-SS') iPF = 7

                    IF (iPF .EQ. 0) WRITE(OFFOUT,*) &
                        'WARNING: Could not assign type of production facility for [PA, EU, FSC]:', iPA,iEU,iFSC

                    ! Pick the cheapest (expNPV .GT. 0) project
                    i = 0

                    !         ReDim prj(1 To 2) As ProjectObj
                    CALL INITPROJECT(tempPrj)

                    IF (iPF .GT. 0) THEN
                        tempPrj%Typ = 1
                        tempPrj%Reg = iReg
                        tempPrj%PA = iPA
                        tempPrj%EU = iEU
                        tempPrj%biggestFSC = iFSC
                        tempPrj%nFld(iFSC) = 1
                        tempPrj%pfTyp(1) = iPF
                        tempPrj%f(ifsc,1) = f ! apr 01/30/2020, added fieldID number to project (note if multiple fields in same FSC ids will be overwritten)
                        CALL CreateExplorationProject(tempPrj, .true.)
                        IF (tempPrj%expNPV .GT. 0) i = 1
                        !WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))') 'evald project', yearof(toff), nmEU(ipa,ieu), ifsc, tempPrj%expNPV ! DBG_Flag_Poss_Projs
                        !WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))') 'evald project', yearof(toff), nmEU(ipa,ieu), ifsc, tempPrj%expNPV, iPF, f, tempPrj%pfNWls, tempPrj%npf, tempPrj%expDrlCst(iFSC) ! DBG_Flag_Poss_Projs
                        CALL printprj(tempPrj, iPa, iEu, iFSC)

                    ENDIF

                    ! If (project is profitable THEN create one type possible project
                    IF (i .GT. 0) THEN
                        nPsbExpPrj = nPsbExpPrj + 1
                        IF (nPsbExpPrj.gt.MAXEXPPRJ) THEN
                            nPsbExpPrj = MAXEXPPRJ
                            WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))') 'WARNING: Too many profitable fields in year ' , yearof(toff)
                        ENDIF

                        ! Allocate memory
                        IF (nPsbExpPrj .EQ. 1) THEN
                            DO J = 1,MAXEXPPRJ
                                CALL InitProject(psbExpPrj(J))
                            ENDDO
                        ENDIF

                        ! Store the project
                        psbExpPrj(nPsbExpPrj) = tempPrj
                        WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))') 'possible project ', yearof(toff), nmEU(ipa,ieu), ifsc, tempPrj%expNPV ! DBG_Flag_Poss_Projs
                    ENDIF
                    tempavl = tempavl - 1
                ENDDO
            ENDDO

            IF (nPsbExpPrj .GT. MAXEXPPRJ) WRITE(OFFOUT,*) 'WARNING:  Too many possible projects (.GT.40).  ', &
                'Reduce variation of Number of Fields in a Project/Prospect in Sheet DEV-PFDef'

        ENDIF    ! SkipEU:
    ENDDO
ENDDO

!WRITE(OFFOUT,*) 'EXITING SUBROUTINE DeterminePossibleExplorationProjects, YEAR =', yearof(toff) ! DBG_Flag_Ent_Ext_Sub

END SUBROUTINE DeterminePossibleExplorationProjects

!************************************************************************************!
! Subroutine to rank possible exploration projects by project type with the following
! order:
!   1=Single big field with its own production facility
!   2=Multiple medium size fields sharing a production facility
!   3=Multiple small fields utilizing nearby production facility
! and then by exploration NPV descending order using "bubble sort" method
!************************************************************************************!
SUBROUTINE RankExplorationProjects
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmoff'      ! offshore variables
INCLUDE 'dwpparm'

INTEGER i, tRnk, tTyp
REAL*4 tNPV
INTEGER Typ(nPsbExpPrj)
REAL*4 NPV(nPsbExpPrj)
LOGICAL Ranked
INTEGER iFSC

!WRITE(OFFOUT,*) 'ENTERING SUBROUTINE RankExplorationProjects, YEAR =', yearof(toff) ! DBG_Flag_Ent_Ext_Sub

! Allocate memory
!ReDim iR(1 To nPsbExpPrj) As Integer
!ReDim Typ(1 To nPsbExpPrj) As Integer
!ReDim NPV(1 To nPsbExpPrj) As Double
iRnk = 0
Typ = 0
NPV = 0.

! Initialize arrays with original order
DO i = 1, nPsbExpPrj
    iRnk(i) = i
    Typ(i) = psbExpPrj(i)%Typ
    NPV(i) = psbExpPrj(i)%expNPV
ENDDO

! Perform bubble sorting on project type ascending order
Ranked = .False.
DO WHILE (.Not. Ranked)
    Ranked = .True.
    DO i = 1, (nPsbExpPrj - 1)
        ! Swap projects i and i+1 if not ascending order
        IF (Typ(i) .GT. Typ(i + 1)) THEN
            tRnk = iRnk(i)
            iRnk(i) = iRnk(i + 1)
            iRnk(i + 1) = tRnk
            tTyp = Typ(i)
            Typ(i) = Typ(i + 1)
            Typ(i + 1) = tTyp
            tNPV = NPV(i)
            NPV(i) = NPV(i + 1)
            NPV(i + 1) = tNPV
            Ranked = .False.
        ENDIF
    ENDDO
ENDDO

! For projects with the same type, perform bubble sorting on NPV descending order
Ranked = .False.
DO WHILE (.Not.Ranked)
    Ranked = .True.
    DO i = 1, (nPsbExpPrj - 1)
        ! Swap projects i and i+1 if not descending order
        IF (NPV(i) .LT. NPV(i + 1)) THEN
            tRnk = iRnk(i)
            iRnk(i) = iRnk(i + 1)
            iRnk(i + 1) = tRnk
            tNPV = NPV(i)
            NPV(i) = NPV(i + 1)
            NPV(i + 1) = tNPV
            Ranked = .False.
        ENDIF
    ENDDO
ENDDO

WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))') 'Ranked prospects', yearof(toff), ' nPsbExpPrj: ', nPsbExpPrj ! DBG_Flag_Rnk_Exp_Proj_By_Year (1/2)
DO i = 1, nPsbExpPrj
    WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))') 'Ranked prospects', i, nmEU(psbexpprj(iRnk(i))%PA,psbexpprj(iRnk(i))%EU), psbexpprj(iRnk(i))%biggestFSC, iRnk(i) , (psbexpprj(iRnk(i))%f(iFSC,1),iFSC=minFSC,minFSC+numFSC-1) ! DBG_Flag_Rnk_Exp_Proj_By_Year (2/2)
ENDDO

!WRITE(OFFOUT,*) 'EXITING SUBROUTINE RankExplorationProjects, YEAR =', yearof(toff)

END SUBROUTINE RankExplorationProjects

!************************************************************************************!
! Subroutine to set BOY exploration drilling rigs capacity
!************************************************************************************!
SUBROUTINE BOYExplorationDrillingRigsCapacity
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmoff'      ! offshore variables
INCLUDE 'dwpparm'

INTEGER i

! WRITE(OFFOUT,*) 'ENTERING SUBROUTINE BOYExplprationDrillingRigsCapacity, YEAR =', yearof(toff)

DO i = 1, 8

    ! Number of well drilling capacity (Wells/Rig) based on:
    !   [target utilization] + [max. build-up rate]
    nRigWlsCap(i) = Int( nRigs(i) * (( rigUtilTarget(i) +  rigBldRatMax(i)) / 100.) * 365. /  expDrlDays(i))

    ! Number of wells drilled (Wells/Rig)
    nRigWlsUtl(i) = Int(nRIGS(i)*0.25)
    IF (i.GT.5) nRigWlsUtl(i) = 0

    ! Rig utilization (%)
    rigUtil(i) = 0.

ENDDO

!WRITE(OFFOUT,*) 'EXITING SUBROUTINE BOYExplprationDrillingRigsCapacity, YEAR =', yearof(toff)

END SUBROUTINE BOYExplorationDrillingRigsCapacity

!************************************************************************************!
! Subroutine to increase number of drilling rigs if needed
!************************************************************************************!
SUBROUTINE EOYExplorationDrillingRigsCapacity
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmoff'      ! offshore variables
INCLUDE 'dwpparm'

INTEGER i, n, nRigInc, nWlsCapInc, &
    nUtilWells
INTEGER nextint

!WRITE(OFFOUT,*) 'ENTERING SUBROUTINE EOYExplorationDrillingRigsCapacity, YEAR =', yearof(toff)

! Loop through all rig type
DO i = 1, 8

    ! Number of wells already drilled with this rig type
    nUtilWells = nRigWlsUtl(i)

    ! Actual well drilling capacity (Wells/Rig)
    nRigWlsCap(i) = Int( nRigs(i) * 365. /  expDrlDays(i))

    ! Calculate rig utilization based on target utilization (%)
    rigUtil(i) = nUtilWells / nRigWlsCap(i) * 100.

    ! Rig utilization (%) is too high =.GT. increase number of rigs
    IF (rigUtil(i) .GT.  rigUtilTarget(i)) THEN

        ! Number of additional wells capacity needed to reach utilization target
        nWlsCapInc = nextInt(nUtilWells / ( rigUtilTarget(i) / 100.)) - nRigWlsCap(i)

        ! Number of additional rigs needed to reach utilization target
        nRigInc = nextInt(nWlsCapInc *  expDrlDays(i) / 365.)

        ! Maximum rig increase
        n = nextInt(( rigBldRatMax(i) / 100.) *  nRigs(i))
        IF (n .LT.  rigIncrMin(i)) n =  rigIncrMin(i)

        ! Allowable rig increase
        IF (nRigInc .GT. n) nRigInc = n

        ! Build rigs
        nRigs(i) =  nRigs(i) + nRigInc

    ENDIF

    ! Calculate new rig capacity (wells/rig) =.GT. for reporting
    nRigWlsCap(i) = Int( nRigs(i) * 365. /  expDrlDays(i))

    ! Calculate new rig utilization (%) =.GT. for reporting
    rigUtil(i) = nUtilWells / nRigWlsCap(i) * 100.
ENDDO
!WRITE(OFFOUT,*) 'EXITING SUBROUTINE EOYExplorationDrillingRigsCapacity, YEAR =', yearof(toff)

END SUBROUTINE EOYExplorationDrillingRigsCapacity

!************************************************************************************!
! Subroutines to perform exploration/delineation drilling subject to drilling rig
! availability
!************************************************************************************!
SUBROUTINE doExplorationDrilling(wD, dD, nWls, EnoughDrillingCapacity)
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmoff'      ! offshore variables
INCLUDE 'dwpparm'

REAL*4, INTENT(IN) ::  wD
REAL*4, INTENT(IN) ::  dD
INTEGER, INTENT(IN) ::  nWls
LOGICAL, INTENT(OUT) ::  EnoughDrillingCapacity

INTEGER i, nRT, n, nWlsAvlTot, nWlsLft, nn
INTEGER nWlsAvl(8)
INTEGER rigTyp(4)

!WRITE(OFFOUT,*) 'ENTERING SUBROUTINE DoExplorationDrilling, YEAR =', yearof(toff), &
!       wD, DD, NWLS

! Number of wells could be drilled by rig type
DO i = 1, 8
    nWlsAvl(i) = nRigWlsCap(i) - nRigWlsUtl(i)
    IF (nWlsAvl(i) .LT. 0) nWlsAvl(i) = 0
ENDDO

! Reset rig type index array
! ReDim rigTyp(1 To 4) As Integer
rigTyp = 0
EnoughDrillingCapacity = .False.

! Get drilling rig type
DO i = 8, 1, -1
    IF ( rigTypWDMin(i) .LE. wD.And.wD .LE. rigTypWDMax(i)) EXIT
    IF (i .EQ. 1) WRITE(OFFOUT,*) 'WARNING:  Water depth out of range for Drilling Rig ', &
        'Availability Constraint Table,', wD
ENDDO
! Water depth range 7500-10000 Feet
IF (i .EQ. 8) THEN
    nRT = 2
    rigTyp(1) = 6    ! Semi-submersible 7500-10000 Feet
    rigTyp(2) = 8    ! Drillship 7500-10000 Feet
    ! Water depth range 5000-7500 Feet
ELSEIF (i .EQ. 7) THEN
    nRT = 4
    rigTyp(1) = 5    ! Semi-submersible 5000-7500 Feet
    rigTyp(2) = -6   ! Semi-submersible 7500-10000 Feet (move down one depth range)
    rigTyp(3) = 7    ! Drillship 5000-7500 Feet
    rigTyp(4) = -8   ! Drillship 7500-10000 Feet (move down one depth range)
    ! Water depth range 1500-5000 Feet
ELSEIF (i .EQ. 4) THEN
    nRT = 3
    rigTyp(1) = 4    ! Semi-submersible 1500-5000 Feet
    rigTyp(2) = -5   ! Semi-submersible 5000-7500 Feet (move down one depth range)
    rigTyp(3) = -7   ! Drillship 5000-7500 Feet (move down one depth range)
    ! Water depth range 0-1500 Feet
ELSEIF (i .EQ. 3) THEN
    IF (dD .GT. 15000.) THEN
        nRT = 1
        rigTyp(1) = 2  ! Jack-up for deep drilling
    ELSE
        nRT = 3
        rigTyp(1) = 1  ! Jack-up for shallow drilling depth
        rigTyp(2) = -2 ! Jack-up for deep drilling depth (move to shallow drilling depth)
        rigTyp(3) = 3  ! Semi-submersible
    ENDIF
ENDIF

nWlsAvlTot = 0
DO n = 1, nRT
    i = Abs(rigTyp(n))
    nWlsAvlTot = nWlsAvlTot + nWlsAvl(i)
ENDDO

! Enough drilling capacity =.GT. Drill the wells
IF (nWls .LE. nWlsAvlTot) THEN
    EnoughDrillingCapacity = .True.
    nWlsLft = nWls
    ! First use designated drilling rigs
    DO n = 1, nRT
        IF (rigTyp(n) .GT. 0) THEN
            i = Abs(rigTyp(n))
            nn = nWlsAvl(i) - nWlsLft
            IF (nn .GE. 0) THEN
                nRigWlsUtl(i) = nRigWlsUtl(i) + nWlsLft
                nWlsLft = 0
                EXIT
            ELSE
                nRigWlsUtl(i) = nRigWlsUtl(i) + nWlsAvl(i)
                nWlsLft = nWlsLft - nWlsAvl(i)
            ENDIF
        ENDIF
    ENDDO
    ! Need more capacity => move rigs from other locations
    IF (nWlsLft .GT. 0) THEN
        DO n = 1, nRT
            IF (rigTyp(n) .LT. 0) THEN
                i = Abs(rigTyp(n))
                nn = nWlsAvl(i) - nWlsLft
                IF (nn .GE. 0) THEN
                    nRigWlsUtl(i) = nRigWlsUtl(i) + nWlsLft
                    nWlsLft = 0
                    EXIT
                ELSE
                    nRigWlsUtl(i) = nRigWlsUtl(i) + nWlsAvl(i)
                    nWlsLft = nWlsLft - nWlsAvl(i)
                ENDIF
            ENDIF
        ENDDO
    ENDIF
    IF (nWlsLft .GT. 0) WRITE(OFFOUT,*) 'WARNING: Expecting enough drilling capacity'

    ! Recalculate rig utilization (%)
    DO i = 1, nRIGTYP
        rigUtil(i) = nRigWlsUtl(i) / nRigWlsCap(i) * 100.
    ENDDO
ENDIF

! WRITE(OFFOUT,*) 'EXITING SUBROUTINE DoExplorationDrilling, YEAR =', yearof(toff), &
!        wD, DD, NWLS, EnoughDrillingCapacity

END SUBROUTINE doExplorationDrilling

!************************************************************************************!
! Subroutine to select exploration projects and perform exploration subject to field
! availability and drilling constraint
!************************************************************************************!
SUBROUTINE PerformExplorationAndDelineation
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmoff'      ! offshore variables
INCLUDE 'dwpparm'

INTEGER iNotRanked, iPA, iEU, iFSC, &
    nWls, f, n, nf, t1, &
    nFld, iprj, ifld, I
INTEGER nextInt
!INTEGER lagcumdisc(nPA,nMaxEU,minFSC:minFSC+numFSC-1) ! removed since unused, apr 02/12/2020
INTEGER itmp, test
LOGICAL fieldsAvailable, EnoughDrillingCapacity
TYPE (ProjectObj) expPrj

!WRITE(OFFOUT,*) 'ENTERING SUBROUTINE PerformExplorationAndDelineation, YEAR =', yearof(toff)


DO iNotRanked = 1, nPsbExpPrj ! Project loop
    ! Get project from the ranked ID list
    expPrj = psbExpPrj(iRnk(iNotRanked))

    iPA = expPrj%PA
    iEU = expPrj%EU

    ! Select several of this type of project subject to:
    DO WHILE (.True.)

        ! (1) Fields availability

        fieldsAvailable = .True.
        DO iFSC = minFSC+numFSC-1, minFSC, -1
            IF (expPrj.nFld(iFSC) .GT. 0) THEN
                ! Number of explorable fields in the FSC is less than what is needed for the project
                IF ((uNRRavl(iPA, iEU, iFSC) - expPrj.nFld(iFSC)) .LT. 0) THEN
                    fieldsAvailable = .False.
                    WRITE (offout,*) 'WARNING: FIELD NOT AVAILABLE', yearof(toff), nmEU(iPA,iEU)
                    EXIT
                ENDIF
            ENDIF
        ENDDO
        ! Fields are not available, go to next possible exploration project
        IF (.Not.fieldsAvailable) EXIT

        ! Drilling rig availability
        nWls = 0
        DO iFSC = minFSC+numFSC-1, minFSC, -1
            If (expPrj.nFld(iFSC) .GT. 0) THEN
                ! Number of exploration/delineation wells
                nWls = nWls + expPrj.expDrlNWls(iFSC)
            ENDIF
        ENDDO

        CALL doExplorationDrilling(expPrj.wDpth, expPrj.dDpth, nWls, EnoughDrillingCapacity)
        ! Not enough exploration drilling capacity for this project, go to next possible exploration project
        IF (.NOT. EnoughDrillingCapacity) WRITE (offout,*) 'WARNING: NOT ENOUGH DRILLING CAPACITY', yearof(toff), nmEU(iPA,iEU)
        If (.Not. EnoughDrillingCapacity) EXIT

        ! ***************************************************************************
        ! Move exploration project to development project, and
        ! Move fields from undiscovered bin to discovered/undeveloped bin
        ! ***************************************************************************

        ! Allocate memory for new project
        nDevPrj = nDevPrj + 1

        If (nDevPrj .EQ. 1) Then
            ! ReDim devPrj(1 To nDevPrj) As ProjectObj
            DO I = 1,nMaxPrj
                CALL InitProject(DevPrj(I))
            ENDDO
        !ELSE
        !    ReDim Preserve devPrj(1 To nDevPrj) As ProjectObj
        ENDIF
        iPrj = nDevPrj

        ! Create one development project from the exploration project
        devPrj(iPrj) = expPrj

        ! Move undiscovered fields to discovered/undeveloped category
        DO iFSC = minFSC+numFSC-1, minFSC, -1
            IF (devPrj(iPrj)%nFld(iFSC) .GT. 0) THEN

                DO f = 1, devPrj(iPrj)%nFld(iFSC)

                    ! Identfy undiscovered fields to be moved to disc/undeveloped category
                    nf =  uNRR(iPA, iEU, iFSC)  ! Index of the last field in the FSC
                    !iFld = fldID(0, iPA, iEU, iFSC, nf)  ! Field ID of this undiscovered field (type 0)
                    DO itmp = 1,ntfld
                        IF (fldID2(itmp).eq.0.and. &
                            fldID3(itmp).eq.iPA.and. &
                            fldID4(itmp).eq.iEU.and. &
                            fldID5(itmp).eq.iFSC.and. &
                            fldID6(itmp).eq.nf) THEN ! apr 01/30/2020 changed to first field instead of last apr 01/31/2020 changed to base to last field should be correct with current fieldid cleanup
                        ifld= fldID1(itmp)
                        EXIT
                        ENDIF
                    ENDDO
                    If (Fld(iFld)%curTyp .NE. 0) WRITE(OFFOUT,*) 'WARNING:  Logic error, expecting an undiscovered field.'
                    devPrj(iPrj)%f(iFSC, f) = iFld  ! Field ID
                    Fld(iFld)%prjID = iPrj  ! Project ID

                    ! Move this field from 0=undiscovered category to 1=Discovered/Undeveloped
                    Fld(iFld)%curTyp = 1
                    Fld(iFld)%yrDisc = yearOf(devPrj(iPrj)%yrExpStart)  ! Year of discovery
                    Fld(iFld)%yrExpStart = devPrj(iPrj)%yrExpStart  ! Year index of discovery
                    Fld(iFld)%yrDevStart = devPrj(iPrj)%yrDevStart  ! Year index of development

                    ! Operating wells and production profiles
                    !ReDim Fld(iFld)%nOprWls(1 To nTime) As Integer
                    !ReDim Fld(iFld)%oilPrd(1 To nTime) As Double
                    !ReDim Fld(iFld)%gasPrd(1 To nTime) As Double
                    !ReDim Fld(iFld)%cndPrd(1 To nTime) As Double
                    !ReDim Fld(iFld)%asgPrd(1 To nTime) As Double
                    Fld(iFld)%devdrlsch = 0
                    Fld(iFld)%nOprWls = 0
                    Fld(iFld)%oilPrd = 0.
                    Fld(iFld)%gasPrd = 0.
                    Fld(iFld)%cndPrd = 0.
                    Fld(iFld)%asgPrd = 0.
                    !DO t1 = 1, nTime
                    !    Fld(iFld)%nOprWls(t1) = devPrj(iPrj)%fNwls(iFSC, f, t1)
                    !    Fld(iFld)%oilPrd(t1) = devPrj(iPrj)%oilPrdF(iFSC, f, t1)
                    !    Fld(iFld)%gasPrd(t1) = devPrj(iPrj)%gasPrdF(iFSC, f, t1)
                    !    Fld(iFld)%cndPrd(t1) = devPrj(iPrj)%cndPrdF(iFSC, f, t1)
                    !    Fld(iFld)%asgPrd(t1) = devPrj(iPrj)%asgPrdF(iFSC, f, t1)
                    !ENDDO

                    ! Move undiscovered field to discovered/undeveloped category
                    uNRR(iPA, iEU, iFSC) =  uNRR(iPA, iEU, iFSC) - 1
                    uNRRavl(iPA, iEU, iFSC) =  uNRRavl(iPA, iEU, iFSC) - 1 ! apr 01/30/2020
                    duNRR(iPA, iEU, iFSC) =  duNRR(iPA, iEU, iFSC) + 1

                    ! Take one field from available explorable fields bin
                    !uNRRavl(iPA, iEU, iFSC) = uNRRavl(iPA, iEU, iFSC) - 1

                ENDDO
                DO t1 = 1, nTime
                    Fld(iFld)%devdrlsch(t1) = devPrj(iPrj)%devdrlsch(t1)
                    Fld(iFld)%nOprWls(t1) = devPrj(iPrj)%fNwls(t1)
                    Fld(iFld)%oilPrd(t1) = devPrj(iPrj)%oilPrd(t1)
                    Fld(iFld)%gasPrd(t1) = devPrj(iPrj)%gasPrd(t1)
                    Fld(iFld)%cndPrd(t1) = devPrj(iPrj)%cndPrd(t1)
                    Fld(iFld)%asgPrd(t1) = devPrj(iPrj)%asgPrd(t1)
                    IF (Fld(iFld)%oilprd(t1).le.0.) FLD(iFld)%yrStPrd = yearof(t1+1)
                ENDDO

            ENDIF
        ENDDO

        EXIT

    ENDDO

ENDDO

!WRITE(OFFOUT,*) 'EXITING SUBROUTINE PerformExplorationAndDelineation, YEAR =', yearof(toff)

END SUBROUTINE PerformExplorationAndDelineation

!************************************************************************************!
! Subroutine to generate production profile for discovered-producing fields
! from existing fields as well as those due to reserve growth
! (NRG-MMS data)
!************************************************************************************!
SUBROUTINE DiscoveredProducingFieldProductionProfile
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmoff'      ! offshore variables
INCLUDE 'dwpparm'

INTEGER iPA, iEU, iFSC, iFSC1, &
    ireg, iFld, f, nWls, t1, &
    tdecOil, tdecGas, i, &
    iFlag, n, tt
InTEGER itmp, test
REAL*4 prdORatei, prdGRatei, &
    prdODecRatei, prdGDecRatei, &
    prdOHyp, prdGHyp, &
    v, v1, v2, n1, n2
REAL*4 LINEAR2

!WRITE(OFFOUT,*) 'ENTERING SUBROUTINE DiscoveredProducingFieldProductionProfile, YEAR =', yearof(toff)

DO iPA = 1, nPA
    DO iEU = 1, nEU(iPA)
        DO iFSC = minFSC,minFSC+numFSC-1
            ireg = OGSMREG(iPA,iEU)
            IF ( dNRR(iPA, iEU, iFSC) .GT. 0) THEN
                DO iFld = 1, dNRR(iPA, iEU, iFSC)
                    ! Field ID of discovered-producing field (type 2)
                    !f = fldID(2, iPA, iEU, iFSC, iFld)
                    DO itmp = 1,ntfld
                        IF (fldID2(itmp).eq.2.and. &
                            fldID3(itmp).eq.iPA.and. &
                            fldID4(itmp).eq.iEU.and. &
                            fldID5(itmp).eq.iFSC.and. &
                            fldID6(itmp).eq.ifld) THEN
                        f = fldID1(itmp)
                        EXIT
                        ENDIF
                    ENDDO
                    IF (Fld(f)%initTyp .NE. 2) WRITE(OFFOUT,*) 'WARNING: Logic error, expecting discovered-producing field flag'

                    ! Allocate memory
                    !ReDim Fld(f)%nOprWls(1 To nTime) As Integer
                    !ReDim Fld(f)%oilPrd(1 To nTime) As Double
                    !ReDim Fld(f)%gasPrd(1 To nTime) As Double
                    !ReDim Fld(f)%cndPrd(1 To nTime) As Double
                    !ReDim Fld(f)%asgPrd(1 To nTime) As Double
                    !ReDim Fld(f)%nOprWlsRG(1 To nTime) As Integer
                    !ReDim Fld(f)%oilRGPrd(1 To nTime) As Double
                    !ReDim Fld(f)%gasRGPrd(1 To nTime) As Double
                    !ReDim Fld(f)%cndRGPrd(1 To nTime) As Double
                    !ReDim Fld(f)%asgRGPrd(1 To nTime) As Double
                    Fld(f)%nOprWls = 0
                    Fld(f)%oilPrd = 0.
                    Fld(f)%gasPrd = 0.
                    Fld(f)%cndPrd = 0.
                    Fld(f)%asgPrd = 0.
                    Fld(f)%nOprWlsRG = 0
                    Fld(f)%oilRGPrd = 0.
                    Fld(f)%gasRGPrd = 0.
                    Fld(f)%cndRGPrd = 0.
                    Fld(f)%asgRGPrd = 0.

                    ! Decline flag 0: New field, apply discovered undeveloped production forecast
                    IF (Fld(f)%decTyp .EQ. 0) THEN

                        ! Assume number of wells the same with those for undiscovered fields
                        Fld(f)%nDevWls =  nDevWls(iPA, iEU, iFSC)

                        !*** Generate field production profile with reserve growth
                        ! First decline year
                        t1 = 1
                        ! New/existing field flag
                        iFlag = 0  ! New field
                        CALL FieldProductionProfile(Fld(f), iFlag, t1, 0.,0.,0.,0.,0.,0.)

                        ! Decline flag 1 or 2
                    ELSEIF (fld(f)%decTyp.NE.3) THEN

                        ! Estimate number of wells
                        v = Fld(f)%oilKR / 1000. + Fld(f)%gasKR /  BOEtoMcf / 1000.
                        DO iFSC1 = minFSC,minFSC+numFSC-1
                            IF (v .GT.  vMean(iFSC1)) EXIT
                        ENDDO
                        IF (v .LT.  vMean(minFSC)) iFSC1 = minFSC
                        IF (v .GT.  vMean(minFSC+numFSC-1)) iFSC1 = minFSC+numFSC-2  ! Use lower FSC for interpolation purposes
                        v1 =  vMean(iFSC1)
                        v2 =  vMean(iFSC1 + 1)
                        n1 =  nDevWls(iPA, iEU, iFSC1)
                        n2 =  nDevWls(iPA, iEU, iFSC1 + 1)
                        nWls = Int(Linear2(v, v1, v2, n1, n2))
                        nWLs = nDevWls(fld(f)%PA,fld(f)%EU,fld(f)%FSC)
                        Fld(f)%nDevWls = nWls

                        ! Get initial production rates/well/day
                        prdORatei = 0.
                        DO t0 = nTime0, 1, -1
                            IF (Fld(f)%oilPrdHis(t0) .GT. 0) THEN
                                prdORatei = Fld(f)%oilPrdHis(t0) / nWls / 365.
                                EXIT
                            ENDIF
                        ENDDO
                        prdGRatei = 0.
                        DO t0 = nTime0, 1, -1
                            IF (Fld(f)%gasPrdHis(t0) .GT. 0) THEN
                                prdGRatei = Fld(f)%gasPrdHis(t0) / nWls / 365.
                                EXIT
                            ENDIF
                        ENDDO

                        ! Get initial decline rates and hyperbolic decline coefficients
                        IF (iFSC .LE. minFSC+8) THEN
                            i = 1
                        ELSE
                            i = 2
                        ENDIF

                        prdODecRatei =  prdDOilDecRatei(iPA, iEU, i)
                        prdGDecRatei =  prdDGasDecRatei(iPA, iEU, i)
                        prdOHyp =  prdDOilHyp(iPA, iEU, i)
                        prdGHyp =  prdDGasHyp(iPA, iEU, i)

                        ! Decline flag 1: Field declining; apply field decline profile
                        IF (Fld(f)%decTyp .EQ. 1) THEN

                            ! First decline year
                            t1 = 1
                            Fld(f)%oilPrd(1) = prdORatei * nWls * 365.
                            Fld(f)%gasPrd(1) = prdGRatei * nWls * 365.
                            Fld(f)%nOprWls(1) = nWls
                            IF (FLD(f)%ogtyp.eq.'O'.or.FLD(f)%ogtyp.eq.'o') THEN
                                Fld(f)%asgPrd(1) = Fld(f)%gasPrd(1)
                                Fld(f)%gasPrd(1) =0.
                            ENDIF

                            ! Decline flag 2: Field production increasing. Hold production constant for
                            ! the following year then start declining. Use latest production data if it
                            ! is available.
                        ELSEIF (Fld(f)%decTyp .EQ. 2) THEN

                            ! First decline year
                            t1 =  prdDYrRampUpOil(iPA, iEU, i) +  prdDYrPeakOil(iPA, iEU, i)
                            tt =  prdDYrRampUpGas(iPA, iEU, i) +  prdDYrPeakGas(iPA, iEU, i)
                            IF (tt .GT. t1) t1 = tt
                            DO tt = 1, t1
                                Fld(f)%oilPrd(tt) = prdORatei * nWls * 365.
                                Fld(f)%gasPrd(tt) = prdGRatei * nWls * 365.
                                Fld(f)%nOprWls(tt) = nWls
                                IF (FLD(f)%ogtyp.eq.'O'.or.FLD(f)%ogtyp.eq.'o') THEN
                                    Fld(f)%asgPrd(tt) = Fld(f)%gasPrd(tt)
                                    Fld(f)%gasPrd(tt) =0.
                                ENDIF
                            ENDDO
                            !Fld(f)%yrDevStart = t1
                        ENDIF

                        ! Generate field production profile
                        iFlag = 1
                        CALL FieldProductionProfile(Fld(f), iFlag, t1, &
                            prdORatei, prdGRatei, prdODecRatei, prdGDecRatei, prdOHyp, prdGHyp)
!==========================> HSM Code Start <==========================
    if (hsm_offshore_bool) then
        ! WRITE(hsm_out,'(*(G0.16,:,","))') Fld(f)%nmEU, Fld(f)%FSC, Fld(f)%nmid, f, nWls, prdORatei * nWls * 365., prdGRatei * nWls * 365., &
        !     prdODecRatei, prdGDecRatei, prdOHyp, prdGHyp
    endif
!===========================> HSM Code End <===========================
                    ENDIF
                ENDDO
            ENDIF
        ENDDO
    ENDDO
ENDDO

! WRITE(OFFOUT,*) 'EXITING SUBROUTINE DiscoveredProducingFieldProductionProfile, YEAR =', yearof(toff)

END SUBROUTINE DiscoveredProducingFieldProductionProfile

!************************************************************************************!
! Subroutine to generate production profile forecast for announced discovery fields
!	Based on FieldProductionProfile subroutine
!	Added for AEO2016 to facilitate alignment/benchmarking to STEO GOM results
!************************************************************************************!
SUBROUTINE AnnouncedDiscoveryProductionProfile
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ncntrl'       ! nems control variables
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmoff'      ! offshore variables
INCLUDE 'dwpparm'
INCLUDE 'ngtdmrep'
INCLUDE 'macout'

INTEGER iPA, iEU, iFSC, &
    iFld, f, nWls, t1, &
    tdecOil, tdecGas, iFlag, itmp, test

INTEGER tt, ttt

REAL*4 fOilCum, fGasCum, fOilRes, fGasRes, &
    fOilResInit, fGasResInit, &
    qo, qg, qad, qcnd, vo, vg, v1, &
    v2, netRevenue, stateTax, fedTax, &
    econLimit, trnCst, prdRev, iRate1, &
    iRate2, v, opc, OPRCOST
LOGICAL decflagO, decflagG ! apr 06/22/2020, true=decline has started, false-still at peak/before

!WRITE(OFFOUT,*) 'ENTERING SUBROUTINE AnnouncedDiscoveryProductionProfile, YEAR =', yearof(toff)

DO iPA = 1, nPA
    DO iEU = 1, nEU(iPA)
        DO iFSC = minFSC,minFSC+numFSC-1

            IF ( duNRR(iPA, iEU, iFSC) .GT. 0) THEN

                DO iFld = 1, duNRR(iPA, iEU, iFSC)

                    ! Field ID of discovered/undeveloped field (type 1)
                    DO itmp = 1,ntfld
                        IF (fldID2(itmp).eq.1.and. &
                            fldID3(itmp).eq.iPA.and. &
                            fldID4(itmp).eq.iEU.and. &
                            fldID5(itmp).eq.iFSC.and. &
                            fldID6(itmp).eq.ifld) THEN
                        f = fldID1(itmp)
                        EXIT
                        ENDIF
                    ENDDO
                    IF (Fld(f)%initTyp .NE. 1) WRITE(OFFOUT,*) 'WARNING: Logic error, expecting discovered/undeveloped field flag'

                    ! Allocate memory
                    Fld(f)%nOprWls = 0
                    Fld(f)%oilPrd = 0.
                    Fld(f)%gasPrd = 0.
                    Fld(f)%cndPrd = 0.
                    Fld(f)%asgPrd = 0.
                    Fld(f)%nOprWlsRG = 0
                    Fld(f)%oilRGPrd = 0.
                    Fld(f)%gasRGPrd = 0.
                    Fld(f)%cndRGPrd = 0.
                    Fld(f)%asgRGPrd = 0.

                    !*** Generate field production profile with reserve growth

                    ! First decline year
                    t1 = Fld(f)%yrStPrd -  yrBegForecast + 1
                    Fld(f)%yrDevStart = t1
                    !IF (Fld(f)%ogtyp.eq.'G') Fld(f)%nDevWls = Fld(f)%nDevWls*  &
                    !    ((ogwprng(13,curiyr)*mc_jpgdp(19)/mc_jpgdp(-2))/6.)**(1.0)

                    ! New/existing field flag
                    iFlag = 0  ! New field

                    ! only-once decline flag
                    decflagO = .False. ! apr 06/22/2020, true=decline has started, false-still at peak/before
                    decflagG = .False. ! apr 06/22/2020, true=decline has started, false-still at peak/before

                    ! Cumulative production
                    fOilCum = 0
                    fGasCum = 0

                    ! Field initial oil (Bbl) and gas (Mcf) resources for new fields
                    IF (Fld(f)%ogtyp.eq.'G'.or.Fld(f)%ogtyp.eq.'g') THEN
                        fgasresinit = vmean(ifsc) *1000000. * BOEtoMcf
                        foilresinit = 0.
                        ! Added tey Nov 2015
                        fOilRes = foilresinit
                        fGasRes = fgasresinit
                    elseif (Fld(f)%ogtyp.eq.'O'.or.Fld(f)%ogtyp.eq.'o') THEN
                        foilresinit = vmean(ifsc) *1000000.
                        fgasresinit = 0.
                        ! Added tey Nov 2015
                        fOilRes = foilresinit
                        fGasRes = fgasresinit
                    ELSE
                        fOilResInit =  vmean(iFSC) * 1000000. * (1. -  GOprop(iPA, iEU))
                        IF (fOilResInit .LT. 0.001) fOilResInit = 0.001
                        fGasResInit = ( vmean(iFSC) * 1000000. *  GOprop(iPA, iEU)) *  BOEtoMcf
                        IF (fGasResInit .LT. 0.001) fGasResInit = 0.001
                        ! Added tey Nov 2015
                        fOilRes = foilresinit
                        fGasRes = fgasresinit
                    endif

                    ! Production profile loop
                    DO tt = t1, nTime
                        ! Reserve growth: Grow the reserve with cumulative growth factor (CGF)
                        ! For discovered fields
                        !ttt = tt - (Fld(f)%yrDevStart+5)
                        !IF (ttt .LT. 0) ttt = 0
                        !IF (ttt .GT. 50) ttt = 50
                        !fOilRes =  oRGCGF(ttt)
                        !fGasRes =  gRGCGF(ttt)
                        !ttt = t1 - (Fld(f)%yrDevStart+5)
                        !IF (ttt .LT. 0) ttt = 0
                        !IF (ttt .GT. 50) ttt = 50
                        !fOilRes = fOilResInit + (fOilRes -  oRGCGF(ttt)) * fOilResInit
                        !fGasRes = fGasResInit + (fGasRes -  gRGCGF(ttt)) * fGasResInit
                        !fOilRes = fOilResInit + (fOilRes) * fOilResInit
                        !fGasRes = fGasResInit + (fGasRes) * fGasResInit

                        ! Number of wells from reserve growth component
                        v1 = (fOilRes - fOilResInit) + (fGasRes - fGasResInit) /  BOEtoMcf
                        v2 = fOilResInit + fGasResInit /  BOEtoMcf
                        !IF (v2.gt.0) nWlsRG = Int(v1 / v2 * nWls*.10)

                        ! Completion technology 1 => increases initial production rate
                        ttt = Int( levPrdPerf1(2))
                        IF (tt .LT. ttt) ttt = tt
                        ! Total improvement for the year (Fraction)
                        iRate1 = ttt * ( levPrdPerf1(1) /  levPrdPerf1(2)) / 100.

                        ! Completion technology 2 => reduces decline rate
                        ttt = Int( levPrdPerf2(2))
                        IF (tt .LT. ttt) ttt = tt
                        ! Total improvement for the year (Fraction)
                        iRate2 = ttt * ( levPrdPerf2(1) /  levPrdPerf2(2)) / 100.

                        ! Initial oil production rate (Bbl/Well/Year)
                        !v1 =  prdOilRatei(iPA, iEU) * (1 + iRate1) * (iFSC/(minFSC+10.))**2
                        !v1 =  prdOilRatei(iPA, iEU) * (1 + iRate1) * ((iFSC)/(15.))**2
                        !v1 =  prdOilRatei(iPA, iEU) * (1 + iRate1) * ((iFSC)/(17.))**1
                        v1 =  Fld(f)%mprdOrate * (1 + iRate1)
                        v2 =  Fld(f)%prdODec * (1 - iRate2)
                        !fOilRes = fOilResInit * (1 + iRate1)
                        !fGasRes = fGasResInit * (1 + iRate1)

                        IF (v2 .LT. 0.) v2 = 0.

                        !     WRITE(OFFOUT,*) 'teybug ', Fld(f)%nmid, ' initial oil prod rate ', Fld(f)%iprdOrate, ' max oil prod rate ', &
                        !         Fld(f)%mprdOrate, ' frac oil prod ', Fld(f)%FrcprdOil

                        ! Crude oil production ramp-up: linearly over length of ramp-up period as specified by variable ANN_RAMPUP
                        IF (tt .lt. t1+ANN_RAMPUP) THEN
                            ! First check if cumulative production during ramp up surpasses designated fraction of resource
                            ! IF it does, will have to decline
                            IF (foilres .gt. 0. .and. fOilCum / fOilRes .gt.  Fld(f)%FrcprdOil) THEN
                                qo = (Fld(f)%iprdOrate + (((Fld(f)%mprdOrate - Fld(f)%iprdOrate)/ANN_RAMPUP)*(tt-t1)))* 365.*&
                                    ((1. + (Fld(f)%prdODec * (tt - tdecOil))) ** (-1.0 / ANN_HYP))
                            ELSE
                                vo = Fld(f)%iprdOrate + (((Fld(f)%mprdOrate - Fld(f)%iprdOrate)/ANN_RAMPUP)*(tt-t1))	!Bbl/d
                                qo = vo * 365 !Bbl/yr
                                tdecOil = tt
                            ENDIF

                        ELSE
                            ! Plateau oil production rate (Bbl/Year) until specified fraction of resource produced
                            ! Then decline
                            IF (foilRes.gt.0.) THEN
                                IF (fOilCum / fOilRes .LE.  Fld(f)%FrcprdOil .and. .not. decflagO) THEN
                                    ! Flat rate period
                                    qo = v1 * 365.
                                    tdecOil = tt
                                ELSE
                                    ! Decline period
                                    !qo = Fld(f)%mprdOrate * 365. * ((1. - Fld(f)%prdODec)**(tt - tdecOil))
                                    qo = v1 * 365. * ((1. + (v2 * (tt - tdecOil))) ** (-1.0 / ANN_HYP))
                                    decflagO = .True.
                                ENDIF
                            ELSE
                                qo = 0.
                            ENDIF
                        ENDIF

                        ! Total field production (include reserve growth)
                        !qo = qo * (nWls + nWlsRG) * (1.-GOProp(iPA,iEU))
                        !qo = qo * (nWls + nWlsRG)

                        ! Cumulative oil produced
                        fOilCum = fOilCum + qo
                        IF (fOilCum .GE. fOilRes) THEN
                            fOilCum = fOilCum - qo
                            qo = fOilRes - fOilCum
                            fOilCum = fOilRes
                        ENDIF


                        ! Non-associated gas production rate (Mcf/Well/Year)
                        !v1 =  prdGasRatei(iPA, iEU) * (1 + iRate1) * (iFSC/(minFSC+10.))**2
                        !v1 =  prdGasRatei(iPA, iEU) * (1 + iRate1) * ((iFSC)/(15.))**2
                        !v1 =  prdGasRatei(iPA, iEU) * (1 + iRate1) * ((iFSC)/(17.))**1
                        v1 =  Fld(f)%mprdGrate * (1 + iRate1)
                        v2 =  Fld(f)%prdGDec * (1 - iRate2)
                        IF (v2 .LT. 0.) v2 = 0.

                        ! Nat gas production ramp-up: linearly over length of ramp-up period as specified by variable ANN_RAMPUP
                        IF (tt .lt. t1+ANN_RAMPUP .and. fGasRes .ne. 0.0) THEN
                            ! First check if cumulative production during ramp up surpasses designated fraction of resource
                            !	If it does, will have to decline
                            IF (foilres .gt. 0. .and. fGasCum / fGasRes .gt.  Fld(f)%FrcprdGas) THEN
                                qg = (Fld(f)%iprdGrate + (((Fld(f)%mprdGrate - Fld(f)%iprdGrate)/ANN_RAMPUP)*(tt-t1)))* 365.*&
                                    ((1. + (Fld(f)%prdGDec * (tt - tdecGas))) ** (-1.0 / ANN_HYP))
                            ELSE
                                vg = Fld(f)%iprdGrate + (((Fld(f)%mprdGrate - Fld(f)%iprdGrate)/ANN_RAMPUP)*(tt-t1))	!Mcf/d
                                qg = vg * 365		!Mcf/yr
                                tdecGas = tt
                            ENDIF

                        ELSE
                            ! Plateau nat gas production rate until specified fraction of resource produced
                            ! Then decline
                            IF (fGasRes .GT. 0.) THEN
                                IF (fGasCum / fGasRes .LE.  Fld(f)%FrcprdGas .and. .not. decflagG) THEN
                                    ! Flat rate period
                                    qg = v1 * 365.
                                    tdecGas = tt
                                ELSE
                                    ! Decline period
                                    !qg = Fld(f)%mprdGrate * 365. * ((1. - Fld(f)%prdGDec)**(tt - tdecGas))
                                    qg = v1 * 365. * ((1. + (v2 * (tt - tdecGas))) ** (-1.0 / ANN_HYP))
                                    decflagG = .True.
                                ENDIF
                            ELSE
                                qg = 0.
                            ENDIF
                        ENDIF

                        ! Total field production (include reserve growth)
                        !qg = qg * (nWls + nWlsRG) * GOProp(iPA,iEU)
                        !qg = qg * (nWls + nWlsRG)

                        ! Cumulative gas production
                        fGasCum = fGasCum + qg
                        IF (fGasCum .GT. fGasRes) THEN
                            fGasCum = fGasCum - qg
                            qg = fGasRes - fGasCum
                            fGasCum = fGasRes
                        ENDIF


                        ! Associated gas portion from oil (Mcf/Field/Year) (GOR=Scf/Bbl)
                        qad = qo *  GOR(iPA, iEU) / 1000.
                        v1 = qad /  BOEtoMcf
                        ! This is a gas field
                        IF (v1 .GT. qo) qad = qo *  BOEtoMcf

                        ! Condensate portion from gas (Bbl/Field/Year)
                        ! (Condensate Yield=Bbl/MMcf)
                        qcnd = (qg / 1000.) *  cndYld(iPA, iEU)
                        v1 = qcnd *  BOEtoMcf
                        ! This is an oil field
                        IF (v1 .GT. qg) qcnd = qg /  BOEtoMcf

                        ! Remove associated gas from oil (Bbl/Field/Year)
                        qo = qo - qad /  BOEtoMcf
                        IF (qo .LT. 0.) qo = 0.

                        ! Remove condensate from non-associated gas (Mcf/Field/Year)
                        qg = qg - qcnd *  BOEtoMcf
                        IF (qg .LT. 0.) qg = 0.


                        !WRITE(offout,100) FLD(F)%NMID, yearof(tt), foilres, foilcum, qo, qcnd, fgasres, fgascum, qg, qad
100                     FORMAT('known',A15,I4,8F16.1)

                        ! *** FIELD ECONOMIC LIMIT CALCULATION *************************
                        ! Economic Limit = Revenue - Costs - Royalties - Taxes
                        ! => Stop field production if "Economic Limit" .LT.= 0

                        ! Transportation cost (MM$/Year)
                        !trnCst = ((qo + qcnd) *  trnTrfOil(iPA, iEU) + &
                        !    (qg + qad) *  trnTrfGas(iPA, iEU)) / 1000000.

                        ! Revenue from production (MM$/Year)
                        !prdRev = (qo *  oilPrice(iogsm,toff) + (qg + qad) *  gasPrice(iogsm,toff) + &
                        !  qcnd *  oilPrice(iogsm,toff)) / 1000000.

                        ! Deduct royalties (MM$/Year)
                        !netRevenue = prdRev - (1. -  royRate(iPA, iEU, iFSC))

                        ! Deduct operating cost (MM$/Year)
                        !netRevenue = netRevenue - (nWls + nWlsRG) * OprCost(iPA, iEU, iFSC, FF%wDpth, FF%dDpth, tt)

                        !opc = (nWls+nWlsRG) * oprcost(ipa,ieu,iFSC, ff%wdpth,ff%ddpth,tt)

                        ! Deduct transportation cost (MM$/Year)
                        !netRevenue = netRevenue - trnCst

                        ! Deduct severance taxes (MM$/Year)
                        !v1 = 1. -  royRate(iPA, iEU, iFSC)
                        !v2 = ((qo + qcnd) *  oilPrice(iogsm,toff)) / 1000000.
                        !netRevenue = netRevenue - v1 * v2 * ( oilSevTaxRate / 100.)

                        !v2 = qo + qcnd
                        !netRevenue = netRevenue - v2 *  oilSevTaxPrd / 1000000.

                        !v2 = (qg + qad) *  gasPrice(iogsm,toff) / 1000000.
                        !netRevenue = netRevenue - v1 * v2 * ( gasSevTaxRate / 100.)

                        !v2 = qg + qad
                        !netRevenue = netRevenue - v2 *  gasSevTaxPrd / 1000000.

                        ! Deduct state and federal tax
                        !stateTax = netRevenue * ( stTaxRate(iPA, iEU) / 100.)
                        !fedTax = (netRevenue - stateTax) * ( fedTaxRate / 100.)

                        ! Economic limit indicator
                        !econLimit = netRevenue - stateTax - fedTax

                        ! Field reaches economic limit => STOP FIELD PRODUCTION
                        !If (econLimit .LE. 0.) EXIT
                        !IF (econLimit .LE. 0..AND.FF%nmid.NE."STGOM".AND.FF%nmid.NE."STCA") EXIT

                        ! Store number of operating wells
                        !FF%nOprWls(tt) = nWls
                        !FF%nOprWlsRG(tt) = nWlsRG

                        ! Ratio number of wells for existing fields
                        !v = 1.*nWls / (nWls + nWlsRG)

                        ! Store oil field production
                        !FF%oilPrd(tt) = v * qo
                        !FF%oilRGPrd(tt) = qo - FF%oilPrd(tt)
                        ! Added tey Nov 2015
                        Fld(f)%oilPrd(tt) = qo

                        ! Store non-associated gas field production
                        !FF%gasPrd(tt) = v * qg
                        !FF%gasRGPrd(tt) = qg - FF%gasPrd(tt)
                        ! Added tey Nov 2015
                        Fld(f)%gasPrd(tt) = qg

                        ! Store associated gas field production
                        !	      FF%asgPrd(tt) = v * qad
                        !	      FF%asgRGPrd(tt) = qad - FF%asgPrd(tt)
                        ! Added tey Nov 2015
                        Fld(f)%asgPrd(tt) = qad

                        ! Store condensate field production
                        !FF%cndPrd(tt) = v * qcnd
                        !FF%cndRGPrd(tt) = qcnd - FF%cndPrd(tt)
                        ! Added tey Nov 2015
                        Fld(f)%cndPrd(tt) = qcnd

                    ENDDO
                    
!==========================> HSM Code Start <==========================
    ! if (hsm_offshore_bool) then
    !     WRITE(hsm_out,'(*(G0.16,:,","))') Fld(f)%nmEU, Fld(f)%FSC, Fld(f)%nmid, f, 'oilPrd', 'adpp', (Fld(f)%oilPrd(tt),tt=1,MNUMYR) 
    !     WRITE(hsm_out,'(*(G0.16,:,","))') Fld(f)%nmEU, Fld(f)%FSC, Fld(f)%nmid, f, 'gasPrd', 'adpp', (Fld(f)%gasPrd(tt),tt=1,MNUMYR) 
    !     WRITE(hsm_out,'(*(G0.16,:,","))') Fld(f)%nmEU, Fld(f)%FSC, Fld(f)%nmid, f, 'asgPrd', 'adpp', (Fld(f)%asgPrd(tt),tt=1,MNUMYR) 
    !     WRITE(hsm_out,'(*(G0.16,:,","))') Fld(f)%nmEU, Fld(f)%FSC, Fld(f)%nmid, f, 'cndPrd', 'adpp', (Fld(f)%cndPrd(tt),tt=1,MNUMYR) 
    ! endif
!===========================> HSM Code End <===========================

                ENDDO
            ENDIF
        ENDDO
    ENDDO
ENDDO

! WRITE(OFFOUT,*) 'EXITING SUBROUTINE AnnouncedDiscoveryProductionProfile, YEAR =', yearof(toff)

END SUBROUTINE AnnouncedDiscoveryProductionProfile

!************************************************************************************!
! Subroutine to generate production profile for a field using "Hyperbolic Decline"
! equation.  iFlag: 0=Use default parameters (with flat period)
!                   1=Use passed parameters (no flat period)
!               t1: First decline year index
!************************************************************************************!
SUBROUTINE FieldProductionProfile(FF, iFlag, t1, &
    prdORatei, prdGRatei, &
    prdODecRatei, prdGDecRatei, &
    prdOHyp, prdGHyp)
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmoff'      ! offshore variables
INCLUDE 'dwpparm'

TYPE (FieldObj) FF
INTEGER iFlag
INTEGER t1
REAL*4 prdORatei
REAL*4 prdGRatei
REAL*4 prdODecRatei
REAL*4 prdGDecRatei
REAL*4 prdOHyp
REAL*4 prdGHyp

INTEGER iPA, iEU, iFSC, nWls, &
    tdecOil, tdecGas, tt, ttt, &
    n1, n2, nWlsRG, iOGSM
REAL*4 fOilCum, fGasCum, fOilRes, fGasRes, &
    fOilResInit, fGasResInit, &
    qo, qg, qad, qcnd, v1, &
    v2, netRevenue, stateTax, fedTax, &
    econLimit, trnCst, prdRev, iRate1, &
    iRate2, v, opc
REAL*4 OPRCOST
LOGICAL decflagO, decflagG ! apr 06/22/2020, true=decline has started, false-still at peak/before
decflagO = .False.
decflagG = .False.

!WRITE(OFFOUT,*) 'ENTERING SUBROUTINE FieldProductionProfile, YEAR =', yearof(toff), iFlag

IF (FF%nDevWls * FF%wDpth * FF%dDpth .LT. 0.01) &
    WRITE(OFFOUT,*) 'WARNING: Logic error, expecting non-zero variable', ff%nmid, ff%ndevwls, ff%yrdevstart, &
    ff%wdpth, ff%ddpth

iPA = FF%PA
iEU = FF%EU
iFSC = FF%FSC
iOGSM = OGSMREG(iPA,iEU)

! Number of operating wells in the field
nWls = FF%nDevWls
! ADJUST ULTRADEEP
!if (ieu.eq.6) nwls = nwls/3

!*** First decline year index
! For new fields => it will be determined based on fraction of produced before decline
IF (iFlag .EQ. 0) THEN
    tdecOil = t1
    tdecGas = t1

    ! For existing fields => decline begins immediately
ELSE
    tdecOil = t1 - 1
    tdecGas = t1 - 1
ENDIF

!*** Cumulative production
! For new fields
IF (iFlag .EQ. 0) THEN
    fOilCum = 0
    fGasCum = 0

    ! For existing fields
    ! => use big number to make sure it declines right away
ELSE
    fOilCum =  vMax(minFSC+numFSC-1) * 100000000.
    fGasCum =  vMax(minFSC+numFSC-1) * 100000000.
ENDIF

!*** Field initial oil (Bbl) and gas (Mcf) resources
! For new fields
IF (iFlag .EQ. 0) THEN
    IF (ff%ogtyp.eq.'G'.or.ff%ogtyp.eq.'g') THEN
        fgasresinit = vmean(ifsc) *1000000. * BOEtoMcf
        foilresinit = 0.
    ELSEIF (ff%ogtyp.eq.'O'.or.ff%ogtyp.eq.'o') THEN
        foilresinit = vmean(ifsc) *1000000.
        fgasresinit = 0.
    ELSE
        fOilResInit =  vMean(iFSC) * 1000000. * (1. -  GOprop(iPA, iEU))
        IF (fOilResInit .LT. 0.001) fOilResInit = 0.001
        fGasResInit = ( vMean(iFSC) * 1000000. *  GOprop(iPA, iEU)) *  BOEtoMcf
        IF (fGasResInit .LT. 0.001) fGasResInit = 0.001
    ENDIF

    ! For existitng fields (convert to Bbl and Mcf)
ELSE
    fOilResInit = FF%oilKR * 1000.
    IF (fOilResInit .LT. 0.001) fOilResInit = 0.001
    fGasResInit = FF%gasKR * 1000.
    IF (fGasResInit .LT. 0.001) fGasResInit = 0.001
ENDIF

! Production profile loop
DO tt = t1, nTime

    !*** Reserve growth: Grow the reserve with cumulative growth factor (CGF)
    ! For discovered fields
    IF (FF%inittyp.EQ.1.or.FF%inittyp.eq.2) THEN
        ttt = tt - (FF%yrDevStart+5)
        IF (ttt .LT. 0) ttt = 0
        IF (ttt .GT. 50) ttt = 50
        fOilRes =  oRGCGF(ttt)
        fGasRes =  gRGCGF(ttt)
        ttt = t1 - (FF%yrDevStart+5)
        IF (ttt .LT. 0) ttt = 0
        IF (ttt .GT. 50) ttt = 50
        fOilRes = fOilResInit + (fOilRes -  oRGCGF(ttt)) * fOilResInit
        fGasRes = fGasResInit + (fGasRes -  gRGCGF(ttt)) * fGasResInit
        !fOilRes = fOilResInit + (fOilRes) * fOilResInit
        !fGasRes = fGasResInit + (fGasRes) * fGasResInit
    ENDIF


    ! Number of wells from reserve growth component
    v1 = (fOilRes - fOilResInit) + (fGasRes - fGasResInit) /  BOEtoMcf
    v2 = fOilResInit + fGasResInit /  BOEtoMcf
    IF (v2.gt.0) nWlsRG = Int(v1 / v2 * nWls*.10)

    ! Completion technology 1 => increases initial production rate
    ttt = Int( levPrdPerf1(2))
    IF (tt .LT. ttt) ttt = tt
    ! Total improvement for the year (Fraction)
    iRate1 = ttt * ( levPrdPerf1(1) /  levPrdPerf1(2)) / 100.

    ! Completion technology 2 => reduces decline rate
    ttt = Int( levPrdPerf2(2))
    IF (tt .LT. ttt) ttt = tt
    ! Total improvement for the year (Fraction)
    iRate2 = ttt * ( levPrdPerf2(1) /  levPrdPerf2(2)) / 100.

    ! Oil production rate (Bbl/Well/Year)
    !v1 =  prdOilRatei(iPA, iEU) * (1 + iRate1) * (iFSC/(minFSC+10.))**2
    !v1 =  prdOilRatei(iPA, iEU) * (1 + iRate1) * ((iFSC)/(15.))**2
    !v1 =  prdOilRatei(iPA, iEU) * (1 + iRate1) * ((iFSC)/(17.))**1
    v1 =  prdOilRatei(iPA, iEU) * (1 + iRate1)
    v2 =  prdOilDecRatei(iPA, iEU) * (1 - iRate2)

    IF (v2 .LT. 0.) v2 = 0.
    IF (foilRes.gt.0.) THEN
        IF (iFlag .EQ. 0) THEN
            IF (fOilCum / fOilRes .LT.  prdOilFrc(iPA, iEU) .and. .not. decflagO) THEN
                ! Flat rate period
                qo = v1 * 365.
                tdecOil = tt
            ELSE
                ! Hyperbolic decline period
                qo = v1 * 365. * (1. + v2 *  prdOilHyp(iPA, iEU) * (tt - tdecOil)) &
                    ** (-1.0 / prdOilHyp(iPA, iEU))
                decflagO = .True.
            ENDIF
        ELSE
            v1 = prdORatei * (1 + iRate1)
            v2 = prdODecRatei * (1 - iRate2)
            IF (v2 .LT. 0.) v2 = 0.
            qo = v1 * 365. * (1. + v2 * prdOHyp * (tt - tdecOil)) ** (-1.0 / prdOHyp)
        ENDIF
    ELSE
        qo = 0.
    ENDIF

    !WRITE(OFFOUT,*) 'dh5bug ', FF%nmid, yearof(tt), nwls, nwlsrg, v1, v2,prdohyp,tt,tdecoil,irate1,irate2,qo
    ! Total field production (include reserve growth)
    !qo = qo * (nWls + nWlsRG) * (1.-GOProp(iPA,iEU))
    qo = qo * (nWls + nWlsRG)

    ! Cumulative oil produced
    fOilCum = fOilCum + qo
    IF (iFlag .EQ. 0) THEN
        IF (fOilCum .GE. fOilRes) THEN
            fOilCum = fOilCum - qo
            qo = fOilRes - fOilCum
            fOilCum = fOilRes
        ENDIF
    ENDIF


    ! Non-associated gas production rate (Mcf/Well/Year)
    !v1 =  prdGasRatei(iPA, iEU) * (1 + iRate1) * (iFSC/(minFSC+10.))**2
    !v1 =  prdGasRatei(iPA, iEU) * (1 + iRate1) * ((iFSC)/(15.))**2
    !v1 =  prdGasRatei(iPA, iEU) * (1 + iRate1) * ((iFSC)/(17.))**1
    v1 =  prdGasRatei(iPA, iEU) * (1 + iRate1)
    v2 =  prdGasDecRatei(iPA, iEU) * (1 - iRate2)
    IF (v2 .LT. 0.) v2 = 0.
    IF (fGasRes .GT. 0.) THEN
        IF (iFlag .EQ. 0) THEN
            IF (fGasCum / fGasRes .LT.  prdGasFrc(iPA, iEU) .and. .not. decflagG) THEN
                ! Flat rate period
                qg = v1 * 365.
                tdecGas = tt
            ELSE
                ! Hyperbolic decline period
                qg = v1 * 365. * (1. + v2 *  prdGasHyp(iPA, iEU) * (tt - tdecGas)) &
                    ** (-1.0 / prdGasHyp(iPA, iEU))
                decflagO = .True.
            ENDIF
        ELSE
            v1 = prdGRatei * (1 + iRate1)
            v2 = prdGDecRatei * (1 - iRate2)
            IF (v2 .LT. 0.) v2 = 0.
            qg = v1 * 365. * (1. + v2 * prdGHyp * (tt - tdecGas)) ** (-1.0 / prdGHyp)
        ENDIF
    ELSE
        qg = 0.
    ENDIF

    ! Total field production (include reserve growth)
    !qg = qg * (nWls + nWlsRG) * GOProp(iPA,iEU)
    qg = qg * (nWls + nWlsRG)

    ! Cumulative gas production
    fGasCum = fGasCum + qg
    IF (iFlag .EQ. 0) THEN
        IF (fGasCum .GT. fGasRes) THEN
            fGasCum = fGasCum - qg
            qg = fGasRes - fGasCum
            fGasCum = fGasRes
        ENDIF
    ENDIF

    IF (iFlag .EQ. 0) THEN

        ! Associated gas portion from oil (Mcf/Field/Year) (GOR=Scf/Bbl)
        qad = qo *  GOR(iPA, iEU) / 1000.
        v1 = qad /  BOEtoMcf
        ! This is a gas field
        IF (v1 .GT. qo) qad = qo *  BOEtoMcf

        ! Condensate portion from gas (Bbl/Field/Year)
        ! (Condensate Yield=Bbl/MMcf)
        qcnd = (qg / 1000.) *  cndYld(iPA, iEU)
        v1 = qcnd *  BOEtoMcf
        ! This is an oil field
        IF (v1 .GT. qg) qcnd = qg /  BOEtoMcf

        ! Remove associated gas from oil (Bbl/Field/Year)
        qo = qo - qad /  BOEtoMcf
        IF (qo .LT. 0.) qo = 0.

        ! Remove condensate from non-associated gas (Mcf/Field/Year)
        qg = qg - qcnd *  BOEtoMcf
        IF (qg .LT. 0.) qg = 0.
    ELSE
        IF (ff%ogtyp.eq.'G'.OR.ff%ogtyp.eq.'g') THEN
            qcnd = qo
            qo = 0.
            qad = 0.
        ENDIF
        IF (ff%ogtyp.eq.'O'.OR.ff%ogtyp.eq.'o') THEN
            qad = qg
            qg = 0.
            qcnd = 0.
        ENDIF
    ENDIF


    !   WRITE(offout,100) FF%nmid, yearof(tt), foilres, foilcum, qo, qcnd, fgasres, fgascum, qg, qad
100 FORMAT('known',A15,I4,8F16.1)

    ! *** FIELD ECONOMIC LIMIT CALCULATION *************************
    ! Economic Limit = Revenue - Costs - Royalties - Taxes
    ! => Stop field production if "Economic Limit" .LT.= 0

    ! Transportation cost (MM$/Year)
    trnCst = ((qo + qcnd) *  trnTrfOil(iPA, iEU) + &
        (qg + qad) *  trnTrfGas(iPA, iEU)) / 1000000.

    ! Revenue from production (MM$/Year)
    prdRev = (qo *  oilPrice(iogsm,toff) + (qg + qad) *  gasPrice(iogsm,toff) + &
        qcnd *  oilPrice(iogsm,toff)) / 1000000.

    ! Deduct royalties (MM$/Year)
    netRevenue = prdRev - (1. -  royRate(iPA, iEU, iFSC))

    ! Deduct operating cost (MM$/Year)
    netRevenue = netRevenue - (nWls + nWlsRG) * OprCost(iPA, iEU, iFSC, FF%wDpth, FF%dDpth, tt)

    opc = (nWls+nWlsRG) * oprcost(ipa,ieu,iFSC, ff%wdpth,ff%ddpth,tt)

    ! Deduct transportation cost (MM$/Year)
    netRevenue = netRevenue - trnCst

    ! Deduct severance taxes (MM$/Year)
    v1 = 1. -  royRate(iPA, iEU, iFSC)
    v2 = ((qo + qcnd) *  oilPrice(iogsm,toff)) / 1000000.
    netRevenue = netRevenue - v1 * v2 * ( oilSevTaxRate / 100.)

    v2 = qo + qcnd
    netRevenue = netRevenue - v2 *  oilSevTaxPrd / 1000000.

    v2 = (qg + qad) *  gasPrice(iogsm,toff) / 1000000.
    netRevenue = netRevenue - v1 * v2 * ( gasSevTaxRate / 100.)

    v2 = qg + qad
    netRevenue = netRevenue - v2 *  gasSevTaxPrd / 1000000.

    ! Deduct state and federal tax
    stateTax = netRevenue * ( stTaxRate(iPA, iEU) / 100.)
    fedTax = (netRevenue - stateTax) * ( fedTaxRate / 100.)

    ! Economic limit indicator
    econLimit = netRevenue - stateTax - fedTax

    ! Field reaches economic limit => STOP FIELD PRODUCTION
    !If (econLimit .LE. 0.) EXIT
    IF (toff.gt.1.and.econLimit .LE. 0..AND.FF%nmid.NE."STGOM".AND.FF%nmid.NE."STCA") EXIT

    ! Store number of operating wells
    FF%nOprWls(tt) = nWls
    FF%nOprWlsRG(tt) = nWlsRG

    ! Ratio number of wells for existing fields
    v = 1.*nWls / (nWls + nWlsRG)

    ! Store oil field production
    FF%oilPrd(tt) = v * qo
    FF%oilRGPrd(tt) = qo - FF%oilPrd(tt)

    ! Store non-associated gas field production
    FF%gasPrd(tt) = v * qg
    FF%gasRGPrd(tt) = qg - FF%gasPrd(tt)

    ! Store associated gas field production
    FF%asgPrd(tt) = v * qad
    FF%asgRGPrd(tt) = qad - FF%asgPrd(tt)

    ! Store condensate field production
    FF%cndPrd(tt) = v * qcnd
    FF%cndRGPrd(tt) = qcnd - FF%cndPrd(tt)
ENDDO

!WRITE(OFFOUT,*) 'EXITING SUBROUTINE FieldProductionProfile, YEAR =', yearof(toff)

END SUBROUTINE FieldProductionProfile

!************************************************************************************!
! Subroutine for some E&P technology levers.  Improvements for other technology
! levers are calculated in the corresponding subroutines
!************************************************************************************!
SUBROUTINE EPTechnologyLevers
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmoff'      ! offshore variables
INCLUDE 'dwpparm'

INTEGER iPA, iEU, iFSC
INTEGER NextInt
REAL*4 iRate

!WRITE(OFFOUT,*) 'ENTERING SUBROUTINE EPTechnologyLevers, YEAR =', yearof(toff) ! DBG_Flag_Ent_Ext_Sub

DO iPA = 1, nPA
    DO iEU = 1, nEU(iPA)
        DO iFSC = minFSC,minFSC+numFSC-1
            ! Seismic technology => increases exploration success rate
            IF (toff .LE. Int( levExpSucRate(2))) THEN
                ! Improvement rate per year (Fraction)
                iRate = toff * ( levExpSucRate(1) /  levExpSucRate(2)) / 100.
                expSucRate(iPA, iEU, iFSC) =  expSucRate(iPA, iEU, iFSC) * (1. + iRate)
            ENDIF
            ! Exploration drilling technology => reduces number of delineation wells
            IF (toff .LE. Int( levDelWls(2))) THEN
                ! Total improvement rate for the year (Fraction)
                iRate = toff * ( levDelWls(1) /  levDelWls(2)) / 100.
                nDelWls(iPA, iEU, iFSC) = nextInt( nDelWlsOrg(iPA, iEU, iFSC) * (1. - iRate))
                IF ( nDelWls(iPA, iEU, iFSC) .LE. 0) nDelWls(iPA, iEU, iFSC) = 1
            ENDIF
            ! Pricing impact on drilling delays =.GT. reduces delays to commence first
            ! exploration and total time to explore and appraise a field
            IF (toff .LE. Int( levExpDly(2))) THEN
                ! Total improvement rate for the year (Fraction)
                iRate = toff * ( levExpDly(1) /  levExpDly(2)) / 100.
                exp1stDly(iPA, iEU) = nextInt( exp1stDlyOrg(iPA, iEU) * (1. - iRate))
                IF ( exp1stDly(iPA, iEU) .LE. 0) exp1stDly(iPA, iEU) = 0
                exp2ndDly(iPA, iEU) = nextInt( exp2ndDlyOrg(iPA, iEU) * (1. - iRate))
                IF ( exp2ndDly(iPA, iEU) .LE. 0) exp2ndDly(iPA, iEU) = 0
            ENDIF
        ENDDO
    ENDDO
ENDDO

! WRITE(OFFOUT,*) 'EXITING SUBROUTINE EPTechnologyLevers, YEAR =', yearof(toff) ! DBG_Flag_Ent_Ext_Sub

END SUBROUTINE EPTechnologyLevers

!************************************************************************************!
! Convert evaluation unit name to subtotal/total ID
!************************************************************************************!
SUBROUTINE EUnm2DpthID(iPA, iEU, iDpth)
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmoff'      ! offshore variables
INCLUDE 'dwpparm'

INTEGER, INTENT(IN) ::  iPA
INTEGER, INTENT(IN) ::  iEU
INTEGER, INTENT(OUT) ::  iDpth

IF (iPA.LT.4.AND.iEU.EQ.1) THEN
    idpth = 1            ! GOM0002
ELSEIF ((iPA.EQ.1.OR.iPA.EQ.2).AND.iEU.EQ.2) THEN
    iDpth = 2            ! GOMDG02
ELSEIF (((iPA.EQ.1.OR.iPA.EQ.2).AND.iEU.EQ.3).OR. &
    (iPA.EQ.3.AND.iEU.EQ.2)) THEN
iDpth = 3            ! GOM0204
ELSEIF (((iPA.EQ.1.OR.iPA.EQ.2).AND.iEU.EQ.4).OR. &
    (iPA.EQ.3.AND.iEU.EQ.3)) THEN
iDpth = 4            ! GOM0408
ELSEIF (((iPA.EQ.1.OR.iPA.EQ.2).AND.iEU.EQ.5).OR. &
    (iPA.EQ.3.AND.iEU.EQ.4)) THEN
iDpth = 5            ! GOM0816
ELSEIF (((iPA.EQ.1.OR.iPA.EQ.2).AND.iEU.EQ.6).OR. &
    (iPA.EQ.3.AND.iEU.EQ.5)) THEN
iDpth = 6            ! GOM1624
ELSEIF (((iPA.EQ.1.OR.iPA.EQ.2).AND.iEU.EQ.7).OR. &
    (iPA.EQ.3.AND.iEU.EQ.6)) THEN
iDpth = 7            ! GOM2400
ELSEIF (iPA.EQ.3.AND.iEU.EQ.7) THEN
    idpth = 8            ! EGOML181
ELSE
    iDpth = -1
ENDIF

END SUBROUTINE EUnm2DpthID

!************************************************************************************!
! Subroutine to round up to the next integer of a real number
!************************************************************************************!
INTEGER FUNCTION nextInt(v)
IMPLICIT NONE

REAL*4, INTENT(IN) :: v
IF (v - Int(v) .GT. 0.01) THEN
    nextint = Int(v) + 1
ELSE
    nextInt = Int(v)
ENDIF

END FUNCTION nextInt

!************************************************************************************!
! Subroutine to initialize project fields
!************************************************************************************!
SUBROUTINE INITPROJECT(p)
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmoff'      ! offshore variables
INCLUDE 'dwpparm'

TYPE (ProjectObj), INTENT(INOUT) :: P

P%Typ = 0
P%Reg = 0
P%PA    = 0
P%EU     = 0
P%biggestFSC = 0
P%yrExpStart = 0
P%yrDevStart = 0
P%nFld = 0
P%f  = 0
P%expDrlNWls = 0
P%expDryNWls = 0
P%delDrlNWls = 0
P%fNwls = 0
P%nPf = 0
P%pfTyp = 0
P%pfNWls = 0
P%pfNWlsArr = 0
P%nFlowline = 0
P%pfTimeAvl = 0
P%devWlsDrl = 0
P%devDrlSch = 0
P%nOprWls = 0
P%wDpth = 0.
P%dDpth = 0.
P%expDrlCst = 0.
P%delDrlCst = 0.
!P%pfCst = 0.
!P%devDrlCst = 0.
P%oilPrd = 0.
P%gasPrd = 0.
P%cndPrd = 0.
P%asgPrd = 0.
!P%oilPrdF = 0.
!P%gasPrdF = 0.
!P%cndPrdF = 0.
!P%asgPrdF = 0.
!P%trnCst = 0.
!P%prdRev = 0.
!P%GRev = 0.
!P%ROY = 0.
!P%NRev = 0.
!P%STax = 0.
!P%OCst = 0.
!P%TCst = 0.
!P%Inv = 0.
!P%TInv = 0.
!P%IInv = 0.
!P%BTCF = 0.
!P%DBas = 0.
!P%DExp = 0.
!P%EInv = 0.
!P%NIBT = 0.
!P%SITax = 0.
!P%FTInc = 0.
!P%FITax = 0.
!P%NIAT = 0.
!P%ATCF = 0.
P%expNPV = 0.
P%devNPV = 0.
P%PI = 0.

END SUBROUTINE INITPROJECT

!************************************************************************************!
! Subroutine to print results
!************************************************************************************!
SUBROUTINE printResults
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ncntrl'       ! nems control variables
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmbfw'      ! ogsm system variables
INCLUDE 'ogsmoff'      ! offshore variables
INCLUDE 'dwpparm'

INTEGER  c, &
    iYr, iPA, iEU, &
    iFSC, iFld, ireg, iPF, rs, iPrj, &
    iFUEL, rs1, rs2, iRgn, iDpth, iTYP
CHARACTER nmSheet*22, ttlName*35, &
    Regnm*22, PAnm*22, EUnm*15, PFnm*6, FOptnm*6
REAL*4 v, tottyp(0:2,MNUMYR), totoff(MNUMYR)
REAL*4 arrR(nReg, nPA, nMaxEU, nreportTime, 4), sArrR(nReg,nReportTime,4)
REAL*4 cumprd
LOGICAL OK
CHARACTER*22 arrS(13)
REAL*4 total(MNUMYR,13)
CHARACTER*166 WK1NAME
INTEGER OFFWK1
INTEGER itmp, test

!WK1NAME = TRIM('OGSMOSS.WK1')
!OFFWK1 = FILE_MGR('O',WK1NAME,.TRUE.)
!CALL WKOPEN(OFFWK1,WK1NAME)

arrS(1) = "Total GOM"
arrS(2) = "  GOM 0-200m"
arrS(3) = "  GOM 0-200m (Deep Gas)"
arrS(4) = "  Total GOM Deep Water"
arrS(5) = "    GOM 200-400m"
arrS(6) = "    GOM 400-800m"
arrS(7) = "    GOM 800-1600m"
arrS(8) = "    GOM 1600-2400m"
arrS(9) = "    GOM > 2400m"
arrS(10) = "  EGOM Lease 181"
arrS(11) = "Atlantic"
arrS(12) = "Pacific"
arrS(13) = "Total Offshore"

!************************************************************************************'
!  Aggregate production from undiscovered fields by region, PA, EU, and year and
!  total production by region and year
!************************************************************************************'
arrR = 0.
sArrR = 0.

!DO iPrj = 1, nDevPrj
!    iPA = devPrj(iPrj)%PA
!    iReg = devPrj(iPrj)%Reg
!    iEU = devPrj(iPrj)%EU
!    DO iFSC = minFSC,minFSC+numFSC-1
!        DO iFld = 1, devPrj(iPrj)%nFld(iFSC)
!            f = devPrj(iPrj)%f(iFSC, iFld)
!            DO iYr = 1, nReportTime
!                arrR(iReg, iPA, iEU, iYr, 1) = arrR(iReg, iPA, iEU, iYr, 1) + Fld(f).oilPrd(iYr) / 1000.
!                arrR(iReg, iPA, iEU, iYr, 2) = arrR(iReg, iPA, iEU, iYr, 2) + Fld(f).gasPrd(iYr) / 1000.
!                arrR(iReg, iPA, iEU, iYr, 3) = arrR(iReg, iPA, iEU, iYr, 3) + Fld(f).cndPrd(iYr) / 1000.
!                arrR(iReg, iPA, iEU, iYr, 4) = arrR(iReg, iPA, iEU, iYr, 4) + Fld(f).asgPrd(iYr) / 1000.
!                sArrR(iReg, iYr, 1) = sArrR(iReg, iYr, 1) + Fld(f).oilPrd(iYr) / 1000.
!                sArrR(iReg, iYr, 2) = sArrR(iReg, iYr, 2) + Fld(f).gasPrd(iYr) / 1000.
!                sArrR(iReg, iYr, 3) = sArrR(iReg, iYr, 3) + Fld(f).cndPrd(iYr) / 1000.
!                sArrR(iReg, iYr, 4) = sArrR(iReg, iYr, 4) + Fld(f).asgPrd(iYr) / 1000.
!            ENDDO
!        ENDDO
!    ENDDO
!ENDDO
DO F = 1, ntFLD
    if(fld(f)%INITTYP.EQ.0) THEN
        iPA = fld(f)%PA
        iReg = fld(f)%Reg
        iEU = fld(f)%EU
        iFSC = fld(f)%FSC
        DO iYr = 1, nReportTime
            arrR(iReg, iPA, iEU, iYr, 1) = arrR(iReg, iPA, iEU, iYr, 1) + Fld(f).oilPrd(iYr) / 1000.
            arrR(iReg, iPA, iEU, iYr, 2) = arrR(iReg, iPA, iEU, iYr, 2) + Fld(f).gasPrd(iYr) / 1000.
            arrR(iReg, iPA, iEU, iYr, 3) = arrR(iReg, iPA, iEU, iYr, 3) + Fld(f).cndPrd(iYr) / 1000.
            arrR(iReg, iPA, iEU, iYr, 4) = arrR(iReg, iPA, iEU, iYr, 4) + Fld(f).asgPrd(iYr) / 1000.
            sArrR(iReg, iYr, 1) = sArrR(iReg, iYr, 1) + Fld(f).oilPrd(iYr) / 1000.
            sArrR(iReg, iYr, 2) = sArrR(iReg, iYr, 2) + Fld(f).gasPrd(iYr) / 1000.
            sArrR(iReg, iYr, 3) = sArrR(iReg, iYr, 3) + Fld(f).cndPrd(iYr) / 1000.
            sArrR(iReg, iYr, 4) = sArrR(iReg, iYr, 4) + Fld(f).asgPrd(iYr) / 1000.
        ENDDO
    ENDIF
ENDDO

!************************************************************************************!
!  Sheets PreFPrdOil, PreFPrdGas: Production from existing fields
!************************************************************************************!
IF (toff .EQ. nReportTime) THEN
    ntime0=ntime0

    DO iFUEL = 1,2
        tottyp = 0.
        totoff = 0.
        WRITE(OFFOUT,*) 'Field Production (MB) - Pre-2006 Fields'
        IF (iFUEL.EQ.1) THEN
            ttlName = "Oil Production (MBbl)"
        ELSE
            ttlName = "Gas Production (MMcf)"
        ENDIF
        !WRITE(OFFOUT,*) ttlName
        !WRITE(OFFOUT,410) (yearof0(iYR),iYR=1,ntime0),(yearof(iYR),iYR = 1,nreporttime)
        DO iPA = 1, nPA
            iRgn = PAREG(iPA)
            DO iEU = 1,nEU(iPA)
                DO iFSC = minFSC,minFSC+numFSC-1
                    IF (dNRR(iPA, iEU, iFSC).GT.0) THEN
                        DO iFld = 1, dNRR(iPA, iEU, iFSC)
                            ! Field ID of discovered-producing field (type 2)
                            !f = fldID(2, iPA, iEU, iFSC, iFld)
                            DO itmp = 1,ntfld
                                IF (fldID2(itmp).eq.2.and. &
                                    fldID3(itmp).eq.iPA.and. &
                                    fldID4(itmp).eq.iEU.and. &
                                    fldID5(itmp).eq.iFSC.and. &
                                    fldID6(itmp).eq.ifld) THEN
                                f = fldID1(itmp)
                                EXIT
                                ENDIF
                            ENDDO
                            IF (Fld(f)%initTyp.NE.2) WRITE (OFFOUT,*) 'WARNING: printresults: Expecting discovered-producing field flag.'
                            CALL EUnm2DpthID(iPA, iEU, iDpth)
                            DO iYR = 1, nTime0
                                IF (iFUEL.EQ.1) THEN
                                    v = Fld(f)%oilPRdHis(iYR) / 1000.
                                ELSE
                                    v = Fld(f)%gasPRdHis(iYR) / 1000.
                                ENDIF
                                ! Gulf of Mexico
                                IF (iRgn .EQ. 1.OR.iRgn.EQ.2) THEN
                                    total(iYr, 1) = total(iYr, 1) + v  ! Total GOM
                                    IF (iDpth .EQ. 1) THEN
                                        total(iYr, 2) = total(iYr, 2) + v  ! GOM 0-200m
                                    ELSEIF (iDpth .EQ. 2) THEN
                                        total(iYr, 3) = total(iYr, 3) + v  ! GOM 0-200m (deep gas)
                                    ELSEIF (iDpth .EQ. 3) THEN
                                        total(iYr, 5) = total(iYr, 5) + v  ! GOM 200-400m
                                    ELSEIF (iDpth .EQ. 4) THEN
                                        total(iYr, 6) = total(iYr, 6) + v  ! GOM 400-800m
                                    ELSEIF (iDpth .EQ. 5) THEN
                                        total(iYr, 7) = total(iYr, 7) + v  ! GOM 800-1600m
                                    ELSEIF (iDpth .EQ. 6) THEN
                                        total(iYr, 8) = total(iYr, 8) + v  ! GOM 1600-2400m
                                    ELSEIF (iDpth .EQ. 7) THEN
                                        total(iYr, 9) = total(iYr, 9) + v  ! GOM > 2400m
                                    ELSEIF (iDpth .EQ. 8) THEN
                                        total(iYr, 10) = total(iYr, 10) + v  ! Eastern GOM Lease-181
                                    ENDIF
                                    IF (iDpth .GE.3.And.iDpth .LE. 8) THEN
                                        total(iYr, 4) = total(iYr, 4) + v  ! Total GOM deep water
                                    ENDIF
                                    ! Atlantic
                                ELSEIF (iRgn .EQ. 3) THEN
                                    total(iYr, 11) = total(iYr, 11) + v
                                    ! Pacific
                                ELSEIF (iRgn .EQ. 4) THEN
                                    total(iYr, 12) = total(iYr, 12) + v
                                ENDIF
                                ! Total Offshore
                                total(iYr, 13) = total(iYR,13) + v

                            ENDDO
                            DO iYR = 1, nReportTime
                                IF (iFUEL.EQ.1) THEN
                                    v = (Fld(f)%oilPRd(iYR) + Fld(f)%cndPrd(iYr) + &
                                        Fld(f)%oilRGPrd(iYr) + Fld(f)%cndRGPrd(iYR)) / 1000.
                                ELSE
                                    v = (Fld(f)%gasPRd(iYR) + Fld(f)%asgPrd(iYR) + &
                                        Fld(f)%gasRGPRd(iYR) + Fld(f)%asgRGPrd(iYR)) / 1000.
                                ENDIF
                                ! Gulf of Mexico
                                IF (iRgn .EQ. 1.OR.iRgn.EQ.2) THEN
                                    total(iYr+nTime0, 1) = total(iYr+nTime0, 1) + v  ! Total GOM
                                    IF (iDpth .EQ. 1) THEN
                                        total(iYr+nTime0, 2) = total(iYr+nTime0, 2) + v  ! GOM 0-200m
                                    ELSEIF (iDpth .EQ. 2) THEN
                                        total(iYr+nTime0, 3) = total(iYr+nTime0, 3) + v  ! GOM 0-200m (deep gas)
                                    ELSEIF (iDpth .EQ. 3) THEN
                                        total(iYr+nTime0, 5) = total(iYr+nTime0, 5) + v  ! GOM 200-400m
                                    ELSEIF (iDpth .EQ. 4) THEN
                                        total(iYr+nTime0, 6) = total(iYr+nTime0, 6) + v  ! GOM 400-800m
                                    ELSEIF (iDpth .EQ. 5) THEN
                                        total(iYr+nTime0, 7) = total(iYr+nTime0, 7) + v  ! GOM 800-1600m
                                    ELSEIF (iDpth .EQ. 6) THEN
                                        total(iYr+nTime0, 8) = total(iYr+nTime0, 8) + v  ! GOM 1600-2400m
                                    ELSEIF (iDpth .EQ. 7) THEN
                                        total(iYr+nTime0, 9) = total(iYr+nTime0, 9) + v  ! GOM > 2400m
                                    ELSEIF (iDpth .EQ. 8) THEN
                                        total(iYr+nTime0, 10) = total(iYr+nTime0, 10) + v  ! Eastern GOM Lease-181
                                    ENDIF
                                    IF (iDpth .GE.3.And.iDpth .LE. 8) THEN
                                        total(iYr+nTime0, 4) = total(iYr+nTime0, 4) + v  ! Total GOM deep water
                                    ENDIF
                                    ! Atlantic
                                ELSEIF (iRgn .EQ. 3) THEN
                                    total(iYr+nTime0, 11) = total(iYr+nTime0, 11) + v
                                    ! Pacific
                                ELSEIF (iRgn .EQ. 4) THEN
                                    total(iYr+nTime0, 12) = total(iYr+nTime0, 12) + v
                                ENDIF
                                ! Total Offshore
                                total(iYr+nTime0, 13) = total(iYR+nTime0,13) + v
                                TOTTYP(2,iyr) = TOTTYP(2,iyr) + v / 1000.
                                TOTOFF(iyr) = TOTOFF(iyr) + v / 1000.

                            ENDDO
                            !IF (iFUEL.EQ.1) THEN
                            !    WRITE(OFFOUT,420) fld(f)%nmID,(fld(f)%oilprdhis(iyr)/1000.,iyr=1,ntime0), &
                            !        (fld(f)%oilprd(iyr)/1000.,iyr=1,nreporttime)
                            !ELSE
                            !    WRITE(OFFOUT,420) fld(f)%nmID,(fld(f)%gasprdhis(iyr)/1000.,iyr=1,ntime0), &
                            !        (fld(f)%gasprd(iyr)/1000.,iyr=1,nreporttime)
                            !ENDIF
                        ENDDO
                    ENDIF
                ENDDO
            ENDDO
        ENDDO
        WRITE(OFFOUT,*)
        WRITE(OFFOUT,*) ttlName
        WRITE(OFFOUT,410) (yearof0(iYR),iYR=1,ntime0),(yearof(iYR),iYR = 1,nreporttime)
        DO i = 1, 12
            WRITE(offout,420) arrs(i), (total(iyr,i),iyr=1,nTime0+nReporttime)
        ENDDO

        total = 0.
        WRITE(OFFOUT,*)
        WRITE(OFFOUT,*) 'Field Production (MB) - Announced Discovery Fields'
        IF (iFUEL.EQ.1) THEN
            ttlName = "Oil Production (MBbl)"
        ELSE
            ttlName = "Gas Production (MMcf)"
        ENDIF
        WRITE(OFFOUT,*) ttlName
        WRITE(OFFOUT,210) (yearof(iYR),iYR = 1,nreporttime)
        DO iPA = 1, nPA
            iRgn = PAREG(iPA)
            DO iEU = 1,nEU(iPA)
                DO iFSC = minFSC,minFSC+numFSC-1
                    IF (duNRR(iPA, iEU, iFSC).GT.0) THEN
                        DO iFld = 1, duNRR(iPA, iEU, iFSC)
                            ! Field ID of announce discovery (discovered/undeveloped) field (type 1)
                            !f = fldID(1, iPA, iEU, iFSC, iFld)
                            DO itmp = 1,ntfld
                                IF (fldID2(itmp).eq.1.and. &
                                    fldID3(itmp).eq.iPA.and. &
                                    fldID4(itmp).eq.iEU.and. &
                                    fldID5(itmp).eq.iFSC.and. &
                                    fldID6(itmp).eq.ifld) THEN
                                f = fldID1(itmp)
                                EXIT
                                ENDIF
                            ENDDO
                            IF (fld(f)%initTyp.NE.0) THEN
                                IF (Fld(f)%initTyp.NE.1) WRITE (OFFOUT,*) 'WARNING: printresults: Expecting discovered/undeveloped field flag.'
                                CALL EUnm2DpthID(iPA, iEU, iDpth)
                                DO iYR = 1, nReportTime
                                    IF (iFUEL.EQ.1) THEN
                                        v = Fld(f)%oilPRd(iYR) / 1000. + Fld(f)%oilRGPrd(iYR) / 1000. + &
                                            Fld(f)%cndPRd(iYR) / 1000. + Fld(f)%cndRGPrd(iYR) / 1000.
                                    ELSE
                                        v = Fld(f)%gasPRd(iYR) / 1000. + Fld(f)%gasRGPrd(iYR) / 1000. + &
                                            Fld(f)%asgPRd(iYR) / 1000. + Fld(f)%asgRGPrd(iYR) / 1000.
                                    ENDIF
                                    ! Gulf of Mexico
                                    IF (iRgn .EQ. 1.OR.iRgn.EQ.2) THEN
                                        total(iYr+nTime0, 1) = total(iYr+nTime0, 1) + v  ! Total GOM
                                        IF (iDpth .EQ. 1) THEN
                                            total(iYr+nTime0, 2) = total(iYr+nTime0, 2) + v  ! GOM 0-200m
                                        ELSEIF (iDpth .EQ. 2) THEN
                                            total(iYr+nTime0, 3) = total(iYr+nTime0, 3) + v  ! GOM 0-200m (deep gas)
                                        ELSEIF (iDpth .EQ. 3) THEN
                                            total(iYr+nTime0, 5) = total(iYr+nTime0, 5) + v  ! GOM 200-400m
                                        ELSEIF (iDpth .EQ. 4) THEN
                                            total(iYr+nTime0, 6) = total(iYr+nTime0, 6) + v  ! GOM 400-800m
                                        ELSEIF (iDpth .EQ. 5) THEN
                                            total(iYr+nTime0, 7) = total(iYr+nTime0, 7) + v  ! GOM 800-1600m
                                        ELSEIF (iDpth .EQ. 6) THEN
                                            total(iYr+nTime0, 8) = total(iYr+nTime0, 8) + v  ! GOM 1600-2400m
                                        ELSEIF (iDpth .EQ. 7) THEN
                                            total(iYr+nTime0, 9) = total(iYr+nTime0, 9) + v  ! GOM > 2400m
                                        ELSEIF (iDpth .EQ. 8) THEN
                                            total(iYr+nTime0, 10) = total(iYr+nTime0, 10) + v  ! Eastern GOM Lease-181
                                        ENDIF
                                        IF (iDpth .GE.3.And.iDpth .LE. 8) THEN
                                            total(iYr+nTime0, 4) = total(iYr+nTime0, 4) + v  ! Total GOM deep water
                                        ENDIF
                                        ! Atlantic
                                    ELSEIF (iRgn .EQ. 3) THEN
                                        total(iYr+nTime0, 11) = total(iYr+nTime0, 11) + v
                                        ! Pacific
                                    ELSEIF (iRgn .EQ. 4) THEN
                                        total(iYr+nTime0, 12) = total(iYr+nTime0, 12) + v
                                    ENDIF
                                    ! Total Offshore
                                    total(iYr+nTime0, 13) = total(iYR+nTime0,13) + v
                                    TOTTYP(1,iyr) = TOTTYP(1,iyr) + v / 1000.
                                    TOTOFF(iyr) = TOTOFF(iyr) + v / 1000.

                                ENDDO
                                !IF (iFUEL.EQ.1) THEN
                                !    WRITE(OFFOUT,220) fld(f)%nmID, fld(f)%FSC, (fld(f)%oilprd(iyr)/1000.,iyr=1,nreporttime), fld(f)%nickname
                                !ELSE
                                !    WRITE(OFFOUT,220) fld(f)%nmID, fld(f)%FSC, (fld(f)%gasprd(iyr)/1000.,iyr=1,nreporttime), fld(f)%nickname
                                !ENDIF
                            ENDIF
                        ENDDO
                    ENDIF
                ENDDO
            ENDDO
        ENDDO
        WRITE(OFFOUT,*)
        WRITE(OFFOUT,*) ttlName
        WRITE(OFFOUT,410) (yearof0(iYR),iYR=1,ntime0),(yearof(iYR),iYR = 1,nreporttime)
        DO i = 1, 12
            WRITE(offout,420) arrs(i), (total(iyr,i),iyr=1,nTime0+nReporttime)
        ENDDO

        total = 0.
        WRITE(OFFOUT,*)
        WRITE(OFFOUT,*) 'Field Production (MB) - New Discovered Fields'
        IF (iFUEL.EQ.1) THEN
            ttlName = "Oil Production (MBbl)"
        ELSE
            ttlName = "Gas Production (MMcf)"
        ENDIF
        !       WRITE(OFFOUT,*) ttlName
        !       WRITE(OFFOUT,410) (yearof0(iYR),iYR=1,ntime0),(yearof(iYR),iYR = 1,nreporttime)
        DO iPA = 1, nPA
            iRgn = PAREG(iPA)
            DO iEU = 1,nEU(iPA)
                CALL EUnm2DpthID(iPA, iEU, iDpth)
                DO iYR = 1, nReportTime
                    IF (iFUEL.EQ.1) THEN
                        v = arrR(iRgn, iPA, iEU, iYR, 1) + arrR(iRgn, iPA, iEU, iYR, 3)
                        !               v = arrR(iRgn, iPA, iEU, iYR, 1)
                    ELSE
                        v = arrR(iRgn, iPA, iEU, iYR, 2) + arrR(iRgn, iPA, iEU, iYR, 4)
                        !               v = arrR(iRgn, iPA, iEU, iYR, 2)
                    ENDIF
                    ! Gulf of Mexico
                    IF (iRgn .EQ. 1.OR.iRgn.EQ.2) THEN
                        total(iYr+nTime0, 1) = total(iYr+nTime0, 1) + v  ! Total GOM
                        IF (iDpth .EQ. 1) THEN
                            total(iYr+nTime0, 2) = total(iYr+nTime0, 2) + v  ! GOM 0-200m
                        ELSEIF (iDpth .EQ. 2) THEN
                            total(iYr+nTime0, 3) = total(iYr+nTime0, 3) + v  ! GOM 0-200m (deep gas)
                        ELSEIF (iDpth .EQ. 3) THEN
                            total(iYr+nTime0, 5) = total(iYr+nTime0, 5) + v  ! GOM 200-400m
                        ELSEIF (iDpth .EQ. 4) THEN
                            total(iYr+nTime0, 6) = total(iYr+nTime0, 6) + v  ! GOM 400-800m
                        ELSEIF (iDpth .EQ. 5) THEN
                            total(iYr+nTime0, 7) = total(iYr+nTime0, 7) + v  ! GOM 800-1600m
                        ELSEIF (iDpth .EQ. 6) THEN
                            total(iYr+nTime0, 8) = total(iYr+nTime0, 8) + v  ! GOM 1600-2400m
                        ELSEIF (iDpth .EQ. 7) THEN
                            total(iYr+nTime0, 9) = total(iYr+nTime0, 9) + v  ! GOM > 2400m
                        ELSEIF (iDpth .EQ. 8) THEN
                            total(iYr+nTime0, 10) = total(iYr+nTime0, 10) + v  ! Eastern GOM Lease-181
                        ENDIF
                        IF (iDpth .GE.3.And.iDpth .LE. 8) THEN
                            total(iYr+nTime0, 4) = total(iYr+nTime0, 4) + v  ! Total GOM deep water
                        ENDIF
                        ! Atlantic
                    ELSEIF (iRgn .EQ. 3) THEN
                        total(iYr+nTime0, 11) = total(iYr+nTime0, 11) + v
                        ! Pacific
                    ELSEIF (iRgn .EQ. 4) THEN
                        total(iYr+nTime0, 12) = total(iYr+nTime0, 12) + v
                    ENDIF
                    ! Total Offshore
                    total(iYr+nTime0, 13) = total(iYR+nTime0,13) + v
                    TOTTYP(0,iyr) = TOTTYP(0,iyr) + v / 1000.
                    TOTOFF(iyr) = TOTOFF(iyr) + v / 1000.

                ENDDO
            ENDDO
        ENDDO
        WRITE(OFFOUT,*)
        WRITE(OFFOUT,*) ttlName
        WRITE(OFFOUT,410) (yearof0(iYR),iYR=1,ntime0),(yearof(iYR),iYR = 1,nreporttime)
        DO i = 1, 12
            WRITE(offout,420) arrs(i), (total(iyr,i),iyr=1,nTime0+nReporttime)
        ENDDO
        WRITE(OFFOUT,*)
        WRITE(OFFOUT,*)


        IF (iFUEL.EQ.1) WRITE(OFFOUT,*) 'OIL PRODUCTION (MMB)                            OIL PRODUCTION (MMB/D)'
        IF (iFUEL.EQ.2) WRITE(OFFOUT,*) 'GAS PRODUCTION (TCF)                            GAS PRODUCTION (BCF/D)'
        WRITE (OFFOUT,*) '      OLD FIELD  ANNOUNCED  NEW DISC   TOTAL   OLD FIELD  ANNOUNCED  NEW DISC  TOTAL'
        DO iYR = 1, nReportTime
            IF (iFUEL.EQ.1) WRITE(OFFOUT,300) yearof(iYR), tottyp(2,iYR), tottyp(1,iYR), tottyp(0,iYR), totoff(iyr), &
                tottyp(2,iYR)/365., tottyp(1,iYR)/365., tottyp(0,iYR)/365., totoff(iyr)/365.
            IF (iFUEL.EQ.2) WRITE(OFFOUT,300) yearof(iYR), tottyp(2,iYR)/1000., tottyp(1,iYR)/1000., tottyp(0,iYR)/1000., totoff(iyr)/1000., &
                tottyp(2,iYR)/365., tottyp(1,iYR)/365., tottyp(0,iYR)/365., totoff(iyr)/365.
                
!==========================> HSM Code Start <==========================
            IF (iFUEL.EQ.1) write(hsm_offshore_prod , '(*(G0.16,:,","))') yearof(iYR), 'oil', tottyp(2,iYR) * 1000000, tottyp(1,iYR) * 1000000, tottyp(0,iYR) * 1000000
            IF (iFUEL.EQ.2) write(hsm_offshore_prod , '(*(G0.16,:,","))') yearof(iYR), 'gas', tottyp(2,iYR) * 1000000, tottyp(1,iYR) * 1000000, tottyp(0,iYR) * 1000000
!===========================> HSM Code End <===========================

        ENDDO

    ENDDO

    !Begin DBG_Flag_Prod_By_Field
    WRITE(OFFOUT,*)
    WRITE(OFFOUT,*)
    WRITE(OFFOUT,*) 'PRODUCTION BY FIELD'
    WRITE(OFFOUT,210) yearof0(ntime0-1), yearof0(ntime0), (yearof(iYR),iYR = 1,nreporttime)
    DO I = 1,ntFLD
        cumprd = 0.
        DO m = 1,nreporttime
            cumprd = cumprd + fld(i)%gasprd(m) + fld(i)%oilprd(m)
        ENDDO
        IF (cumprd.ne.0.) WRITE(OFFOUT,500) I, FLD(i)%nmeu, FLD(i)%nmid, fld(i)%ogtyp, fld(i)%fsc, fld(i)%inittyp, fld(i)%curtyp, 'OIL',  &
            fld(i)%oilprdhis(ntime0-1)/1000., &
            fld(i)%oilprdhis(ntime0)/1000., &
            ((fld(i)%oilprd(iyr)+fld(i)%cndprd(iyr)+fld(i)%oilRGprd(iyr)+fld(i)%cndRGprd(iyr))/1000.,iyr=1,nreporttime),fld(i)%nickname
        IF (cumprd.ne.0.) WRITE(OFFOUT,500) I, FLD(i)%nmeu, FLD(i)%nmid, fld(i)%ogtyp, fld(i)%fsc, fld(i)%inittyp, fld(i)%curtyp, 'GAS',  &
            fld(i)%gasprdhis(ntime0-1)/1000., &
            fld(i)%gasprdhis(ntime0)/1000., &
            ((fld(i)%gasprd(iyr)+fld(i)%asgprd(iyr)+fld(i)%gasRGprd(iyr)+fld(i)%asgRGprd(iyr))/1000.,iyr=1,nreporttime),fld(i)%nickname
    ENDDO
    !End DBG_Flag_Prod_By_Field

ENDIF

110 FORMAT(22X,<ntime0>I8)
120 FORMAT(A22,<ntime0>F8.0)
130 FORMAT('     TOTAL            ',<ntime0>F8.0)
210 FORMAT(58X,<nReporttime+2>I12)
215 FORMAT(22X,<nReporttime+2>I10)
220 FORMAT(A19,I3,<nReporttime>F10.2,2X,A30)
230 FORMAT('     TOTAL            ',<nReporttime>F8.0)
300 FORMAT(I6,8F10.3)
410 FORMAT(22X,<nReporttime+nTime0>I12)
420 FORMAT(A22,<nReporttime+nTime0>F12.2)
500 FORMAT(I6,A22,A15,A3,I4,I2,I2,A4,<nReporttime+2>F12.2,2X,A30)

END SUBROUTINE printResults

!************************************************************************************!
!
!************************************************************************************!
SUBROUTINE OGreporttoogsm
IMPLICIT NONE
!***********************************************************************
! subroutine maps Deep Water results into OGSM variables

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ncntrl'       ! nems control variables
INCLUDE 'ogsmparm'
INCLUDE 'ogsml48'
INCLUDE 'ogsmoff'
INCLUDE 'ogsmbfw'
INCLUDE 'ogsmout'
INCLUDE 'dwpparm'

INTEGER iyr                 ! Year
INTEGER ireg                ! Offshore region
INTEGER iPA, iEU, iFSC      ! Planning area, evaluaton unit, and field size class
INTEGER iPrj
INTEGER DIST
INTEGER ff                  ! Field index
INTEGER FirstPrdYr
INTEGER arrI(nReg, nPA, nMaxEU, nReportTime, 5)
INTEGER sArrI(nReg, nReportTime, 5)
REAL*4 tot_dev_wells          ! total number of successful develop wells
REAL*4 tot_exp_wells          ! total number of successful exploratory wells
REAL*4 foilres, fgasres, exprdoff2(L48RGN+1:L48RGN+3)
REAL*4 PR(OFFFUEL)

iyr = toff + offbyr - 1

NDIROFF = 0.
NRDOFF = 0.
REVOFF = 0.
EXTOFF = 0.
PRDOFF = 0.
DEVOFF = 0.
NFWOFF = 0.
OEXPOFF = 0.
DRYWELLOFF = 0.
SUCWELLOFF = 0.
DO ff = 1, ntFld
    iPA = fld(ff)%PA
    iEU = fld(ff)%EU
    iFSC = fld(ff)%FSC
    iREG = fld(ff)%OGSM
    IF (fld(ff)%YrStPrd.GT.0) THEN
        firstprdyr = fld(ff)%yrStPrd - yrBegForecast + 1
    ELSE
        firstprdyr = fld(ff)%yrDevStart + exp1stDly(iPA,iEU) + exp2ndDly(iPA,iEU)
    ENDIF
    ! SAVE EXPECTED PRODUCTION
    PRDOFF(iReg,1) = PRDOFF(iReg,1) + (fld(ff)%oilprd(toff) + fld(ff)%cndprd(toff))/1000000. +  &
        (fld(ff)%oilRGprd(toff) + fld(ff)%cndRGprd(toff))/1000000.
    PRDOFF(iReg,2) = PRDOFF(iReg,2) + (fld(ff)%gasprd(toff) + fld(ff)%gasRGprd(toff))/1000000.
    !IF (fld(ff)%inittyp.EQ.0) THEN     ! NEW DISCOVERIES
    IF (fld(ff)%yrDisc.EQ.yearof(toff)) THEN
        !IF (fld(ff)%yrDevStart.EQ.toff) THEN
        foilres = vMean(iFSC) * (1. -  GOprop(iPA, iEU))
        fGasRes = ( vMean(iFSC) * GOprop(iPA, iEU)) *  BOEtoMcf
        !IF (firstprdyr.le.ntime) WRITE(offout,*) 'disc (yrdisc,EU,ID,FSC,firstprdyr,pftype)', &
        !     yearof(toff), ff, fld(ff)%nmEU, fld(ff)%nmid, iFSC, yearof(firstprdyr), fld(ff)%pfoptflg
        !IF (firstprdyr.le.nreporttime) WRITE(offout,100) yearof(toff), ff, fld(ff)%nmEU, fld(ff)%nmID, fld(ff)%FSC, &
        !    fld(ff)%initTyp, foilres, fld(ff)%oilprd(firstprdyr)/(foilres*1000000.), (fld(ff)%oilprd(m)/1000.,m=1,nreporttime)
        !IF (firstprdyr.le.nreporttime) WRITE(offout,105) yearof(toff), ff, fld(ff)%nmEU, fld(ff)%nmID, fld(ff)%FSC, &
        !    fld(ff)%initTyp, foilres, fld(ff)%cndprd(firstprdyr)/(foilres*1000000.), (fld(ff)%cndprd(m)/1000.,m=1,nreporttime)
        !IF (firstprdyr.le.nreporttime) WRITE(offout,110) yearof(toff), ff, fld(ff)%nmEU, fld(ff)%nmID, fld(ff)%FSC, &
        !    fld(ff)%initTyp, fgasres, fld(ff)%gasprd(firstprdyr)/(fgasres*1000000.), (fld(ff)%gasprd(m)/1000.,m=1,nreporttime)
        !IF (firstprdyr.le.nreporttime) WRITE(offout,115) yearof(toff), ff, fld(ff)%nmEU, fld(ff)%nmID, fld(ff)%FSC, &
        !    fld(ff)%initTyp, fgasres, fld(ff)%asgprd(firstprdyr)/(fgasres*1000000.), (fld(ff)%asgprd(m)/1000.,m=1,nreporttime)

100     FORMAT('oil',2I6,A22,A15,I4,I2,F12.0,F8.4,<nReporttime>F12.2)
105     FORMAT('cnd',2I6,A22,A15,I4,I2,F12.0,F8.4,<nReporttime>F12.2)
110     FORMAT('gas',2I6,A22,A15,I4,I2,F12.0,F8.4,<nReporttime>F12.2)
115     FORMAT('asg',2I6,A22,A15,I4,I2,F12.0,F8.4,<nReporttime>F12.2)
200     FORMAT(A35,2X,2I6,A22,A15,I4,I6,A8)

        ! ...... remap Undiscovered inferred reserves into OGSM's discovered inferred
        NDIROFF(iREG,1) = NDIROFF(iREG,1) + foilres * (1. - 1./(oRGCGF(50))) ! oil in MMB
        NDIROFF(iREG,2) = NDIROFF(iREG,2) + fgasres * (1. - 1./(gRGCGF(50))) ! gas in BCF

        ! ...... remap undiscovered Reserve Additions
        NRDOFF(iREG,1) = NRDOFF(iREG,1) + foilres * (1./(oRGCGF(50)))        ! oil in MMB
        NRDOFF(iREG,2) = NRDOFF(iREG,2) + fgasres * (1./(gRGCGF(50)))        ! gas in BCF
    ENDIF
    !       ENDIF
    ! ...... remap Reserve Revisions + Extensions
    REVOFF(iREG,1) = REVOFF(iREG,1) + (fld(ff)%oilprd(toff)+fld(ff)%cndprd(toff))/1000000.  &
        + (fld(ff)%oilRGprd(toff)+fld(ff)%cndRGprd(toff))/1000000.
    REVOFF(iREG,2) = REVOFF(iREG,2) + (fld(ff)%gasprd(toff)+fld(ff)%gasRGprd(toff))/1000000.
    ! ...... remap AD Gas Reserve Additions
    IF (ireg.ge.3) THEN
        ADRESAD(L48RGN+2,iyr) = ADRESAD(L48RGN+2,iyr) + (fld(ff)%asgprd(toff)+fld(ff)%asgRGprd(toff))/1000000.
    ENDIF
    IF (ireg.eq.1) THEN
        ADRESAD(L48RGN+1,iyr) = ADRESAD(L48RGN+1,iyr) + (fld(ff)%asgprd(toff)+fld(ff)%asgRGprd(toff))/1000000.
    ENDIF
    IF (ireg.eq.2) THEN
        ADRESAD(L48RGN+3,iyr) = ADRESAD(L48RGN+3,iyr) + (fld(ff)%asgprd(toff)+fld(ff)%asgRGprd(toff))/1000000.
    ENDIF
    !..... Developmental  wells
    DEVOFF(iREG,1) = DEVOFF(iREG,1) + NINT(fld(ff)%devdrlsch(toff)*(1.-GOProp(iPA,iEU)))
    DEVOFF(iREG,2) = DEVOFF(iREG,2) + NINT(fld(ff)%devdrlsch(toff)*(GOProp(iPA,iEU)))
    !IF (fld(ff)%inittyp.EQ.1.AND.toff.EQ.fld(ff)%yrdevstart) THEN     ! OLD DISCOVERIES
    !    DEVOFF(iREG,1) = DEVOFF(iREG,1) + NINT(fld(ff)%ndevwls*(1.-GOProp(iPA,iEU)))
    !    DEVOFF(iREG,2) = DEVOFF(iREG,2) + NINT(fld(ff)%ndevwls*(GOProp(iPA,iEU)))
    !ENDIF
    IF (fld(ff)%inittyp.EQ.1.AND.toff.EQ.fld(ff)%yrdevstart-2) THEN     ! OLD DISCOVERIES
        OEXPOFF(iREG,2) = OEXPOFF(iREG,2) + ndelwls(iPA,iEU,iFSC)
        DRYWELLOFF(1,iREG,2) = OEXPOFF(iREG,2) + NINT((ndelwls(iPA,iEU,iFSC)*(1./expsucrate(iPA,iEU,iFSC)-1.)))
    ENDIF
ENDDO
revoff(2,2) = revoff(2,2) * 0.05

DO ireg = 1, OFFRGN
    PR(1) = HISTPRROFF(iREG,1,l48hyr)
    PR(2) = HISTPRROFF(iREG,2,l48hyr)
    IF (PR(1).LE.0.0) PR(1) = 0.05
    IF (PR(2).LE.0.0) PR(2) = 0.05
    REVOFF(iREG,1) = REVOFF(iREG,1)/PR(1) + PRDOFF(iREG,1) - CURRESOFF(iREG,1) - NRDOFF(iREG,1)
    REVOFF(iREG,2) = REVOFF(iREG,2)/PR(2) + PRDOFF(iREG,2) - CURRESOFF(iREG,2) - NRDOFF(iREG,2)
    !if (r_infrsvoff(ireg,1)+ndiroff(ireg,1).LE.revoff(ireg,1)) revoff(ireg,1) = (r_infrsvoff(ireg,1)+ndiroff(ireg,1))*0.9
    !if (r_infrsvoff(ireg,2)+ndiroff(ireg,2).LE.revoff(ireg,2)) revoff(ireg,2) = (r_infrsvoff(ireg,2)+ndiroff(ireg,2))*0.9
    ! Approximate drilling in old fields
    !if (iREG.EQ.4) THEN
    !    REVOFF(iREG,1) = MAX(REVOFF(iREG,1),300.)
    !    REVOFF(iREG,2) = MAX(REVOFF(iREG,2),300.)
    !ENDIF
    IF (iREG.EQ.3) THEN
        DEVOFF(iREG,2) = DEVOFF(iREG,2) + 6.*gasprice(3,1)
        DEVOFF(iREG,1) = DEVOFF(iREG,1) + 0.50*oilprice(3,1)
        !OEXPOFF(iREG,2) = OEXPOFF(iREG,2) + 1.4*gasprice(3,1)
        !OEXPOFF(iREG,1) = OEXPOFF(iREG,1) + 0.1*oilprice(3,1)
        DRYWELLOFF(1,iREG,2) = DRYWELLOFF(1,iREG,2) + NINT(10*(1./.3-1.))
        DRYWELLOFF(1,iREG,1) = DRYWELLOFF(1,iREG,1) + NINT(6*(1./.3-1.))
        DRYWELLOFF(2,iREG,2) = DRYWELLOFF(2,iREG,2) + NINT(5*(1./.3-1.))
        DRYWELLOFF(2,iREG,1) = DRYWELLOFF(2,iREG,1) + NINT(3*(1./.3-1.))
    ENDIF
    IF (iREG.EQ.4) THEN
        DEVOFF(iREG,2) = DEVOFF(iREG,2) + 5.*gasprice(3,1)
        DEVOFF(iREG,1) = DEVOFF(iREG,1) + 0.40*oilprice(3,1)
        OEXPOFF(iREG,1) = OEXPOFF(iREG,2)*0.35
        DRYWELLOFF(1,iREG,2) = DRYWELLOFF(1,iREG,2) + NINT(10*(1./.3-1.))
        DRYWELLOFF(1,iREG,1) = DRYWELLOFF(1,iREG,1) + NINT(6*(1./.3-1.))
        DRYWELLOFF(2,iREG,2) = DRYWELLOFF(2,iREG,2) + NINT(5*(1./.3-1.))
        DRYWELLOFF(2,iREG,1) = DRYWELLOFF(2,iREG,1) + NINT(3*(1./.3-1.))
    ENDIF
    !IF (iREG.EQ.2) THEN
    !    REVOFF(iREG,2) = REVOFF(iREG,2) + 5.
    !ENDIF
ENDDO
!DO ireg = 1, OFFRGN
!    WRITE(OFFOUT,*) 'nrdoff',yearof(toff),ireg,nrdoff(ireg,1),nrdoff(ireg,2)
!ENDDO
!DO ireg = 1, OFFRGN
!    WRITE(OFFOUT,*) 'revoff',yearof(toff),ireg,revoff(ireg,1),revoff(ireg,2)
!ENDDO
!WRITE(OFFOUT,*) 'adresad',yearof(toff),adresad(L48RGN+1,iyr),adresad(L48RGN+2,iyr),adresad(L48RGN+3,iyr)

! ...... remap Wells drilled during the year
IF (toff.eq.1) THEN
    arrI = 0
    sArrI = 0
ENDIF

iyr = toff
DO iPrj = 1, nDevPrj
    iPA = devPrj(iPrj)%PA
    !iReg = devPrj(iPrj)%Reg
    iEU = devPrj(iPrj)%EU
    iReg = OGSMREG(iPA,iEU)
    DO iFSC = minFSC,minFSC+numFSC-1
        IF (iyr .EQ. devPrj(iPrj)%yrExpStart) THEN
            ! ...... Exploratory  wells
            n = (devPrj(iPrj)%expDrlNWls(iFSC) - devPrj(iPrj)%delDrlNWls(iFSC))*expsucrate(iPA,iEU,iFSC)
            NFWOFF(iREG,1) = NFWOFF(iREG,1) + NINT(n*(1.-GOProp(iPA,iEU)))
            NFWOFF(iREG,2) = NFWOFF(iREG,2) + NINT(n*(GOProp(iPA,iEU)))
            ! Number of dry exploratory wells
            DRYWELLOFF(1,ireg,1)= DRYWELLOFF(1,ireg,1) + NINT(devPrj(iPrj)%expDryNWls(iFSC)*(1.-GOProp(iPA,iEU)))
            DRYWELLOFF(1,ireg,2)= DRYWELLOFF(1,ireg,2) + NINT(devPrj(iPrj)%expDryNWls(iFSC)*GOProp(iPA,iEU))
            ! Number of delineation wells
            OEXPOFF(iREG,1) = OEXPOFF(iREG,1) + NINT(devPrj(iPrj)%delDrlNWls(iFSC)*expsucrate(iPA,iEU,iFSC)*(1.-GOProp(iPA,iEU)))
            OEXPOFF(iREG,2) = OEXPOFF(iREG,2) + NINT(devPrj(iPrj)%delDrlNWls(iFSC)*expsucrate(iPA,iEU,iFSC)*(GOProp(iPA,iEU)))
            DRYWELLOFF(1,ireg,1)= DRYWELLOFF(1,ireg,1) + NINT(devPrj(iPrj)%delDrlNWls(iFSC)*(1.-GOProp(iPA,iEU))*(1.-expSucRate(iPA,iEU,iFSC)))
            DRYWELLOFF(1,ireg,2)= DRYWELLOFF(1,ireg,2) + NINT(devPrj(iPrj)%delDrlNWls(iFSC)*GOProp(iPA,iEU)*(1.-expSucRate(iPA,iEU,iFSC)))
        ENDIF
    ENDDO
    !..... Developmental  wells
    DEVOFF(iREG,1) = DEVOFF(iREG,1) + NINT(devPrj(iPrj)%devDrlSch(iYr)*(1.-GOProp(iPA,iEU)))
    DEVOFF(iREG,2) = DEVOFF(iREG,2) + NINT(devPrj(iPrj)%devDrlSch(iYr)*(GOProp(iPA,iEU)))
ENDDO
IF (curiyr.le.offnhyr) THEN
    DO ireg = 1, offrgn
        WELLSOFF(1,ireg,1)=HISTWELOFF(1,ireg,1,curiyr)
        WELLSOFF(2,ireg,1)=HISTWELOFF(2,ireg,1,curiyr)
        WELLSOFF(1,ireg,2)=HISTWELOFF(1,ireg,2,curiyr)
        WELLSOFF(2,ireg,2)=HISTWELOFF(2,ireg,2,curiyr)
        SUCWELLOFF(1,ireg,1)=ANINT(HISTSROFF(1,ireg,1,curiyr)*HISTWELOFF(1,ireg,1,curiyr))
        SUCWELLOFF(2,ireg,1)=ANINT(HISTSROFF(2,ireg,1,curiyr)*HISTWELOFF(2,ireg,1,curiyr))
        SUCWELLOFF(1,ireg,2)=ANINT(HISTSROFF(1,ireg,2,curiyr)*HISTWELOFF(1,ireg,2,curiyr))
        SUCWELLOFF(2,ireg,2)=ANINT(HISTSROFF(2,ireg,2,curiyr)*HISTWELOFF(2,ireg,2,curiyr))
        DRYWELLOFF(1,ireg,1)=HISTWELOFF(1,ireg,1,curiyr) - SUCWELLOFF(1,ireg,1)
        DRYWELLOFF(2,ireg,1)=HISTWELOFF(2,ireg,1,curiyr) - SUCWELLOFF(2,ireg,1)
        DRYWELLOFF(1,ireg,2)=HISTWELOFF(1,ireg,2,curiyr) - SUCWELLOFF(1,ireg,2)
        DRYWELLOFF(2,ireg,2)=HISTWELOFF(2,ireg,2,curiyr) - SUCWELLOFF(2,ireg,2)
        NFWOFF(ireg,1) = histnfwoff(ireg,1,curiyr)
        NFWOFF(ireg,2) = histnfwoff(ireg,2,curiyr)
        OEXPOFF(ireg,1) = SUCWELLOFF(1,ireg,1) - NFWOFF(ireg,1)
        OEXPOFF(ireg,2) = SUCWELLOFF(1,ireg,2) - NFWOFF(ireg,2)
        DEVOFF(ireg,1) = SUCWELLOFF(2,ireg,1)
        DEVOFF(ireg,2) = SUCWELLOFF(2,ireg,2)
        CURRESOFF(ireg,1)=HISTRESOFF(ireg,1,curiyr)
        CURRESOFF(ireg,2)=HISTRESOFF(ireg,2,curiyr)
        PRRATOFF(ireg,1)=HISTPRROFF(ireg,1,curiyr)
        PRRATOFF(ireg,2)=HISTPRROFF(ireg,2,curiyr)
        SROFF(1,ireg,1)=HISTSROFF(1,ireg,1,curiyr)
        SROFF(1,ireg,2)=HISTSROFF(1,ireg,2,curiyr)
        SROFF(2,ireg,1)=HISTSROFF(2,ireg,1,curiyr)
        SROFF(2,ireg,2)=HISTSROFF(2,ireg,2,curiyr)

        IF (curiyr.le.offnhyr) THEN
            REVOFF(ireg,1) = histREVoff(ireg,1,curiyr)
            REVOFF(ireg,2) = histREVoff(ireg,2,curiyr)
            EXTOFF(ireg,1) = histEXToff(ireg,1,curiyr)
            EXTOFF(ireg,2) = histEXToff(ireg,2,curiyr)
            NRDOFF(ireg,1) = histNRDoff(ireg,1,curiyr)
            NRDOFF(ireg,2) = histNRDoff(ireg,2,curiyr)
        ENDIF
    ENDDO
ELSE
    DO ireg = 1, OFFRGN
        DRYWELLOFF(2,ireg,1) = NINT(DEVOFF(iREG,1)*(1./0.75-1.))
        DRYWELLOFF(2,ireg,2) = NINT(DEVOFF(iREG,2)*(1./0.75-1.))
        SUCWELLOFF(1,ireg,1) = NFWOFF(ireg,1) + OEXPOFF(ireg,1)
        SUCWELLOFF(1,ireg,2) = NFWOFF(ireg,2) + OEXPOFF(ireg,2)
        SUCWELLOFF(2,ireg,1) = DEVOFF(ireg,1)
        SUCWELLOFF(2,ireg,2) = DEVOFF(ireg,2)
        WELLSOFF(1,ireg,1) = SUCWELLOFF(1,ireg,1) + &
            DRYWELLOFF(1,ireg,1)
        WELLSOFF(1,ireg,2) = SUCWELLOFF(1,ireg,2) + &
            DRYWELLOFF(1,ireg,2)
        WELLSOFF(2,ireg,1) = SUCWELLOFF(2,ireg,1) + &
            DRYWELLOFF(2,ireg,1)
        WELLSOFF(2,ireg,2) = SUCWELLOFF(2,ireg,2) + &
            DRYWELLOFF(2,ireg,2)
        IF (wellsoff(1,ireg,1).GT. 0.) SROFF(1,ireg,1) = SUCWELLOFF(1,ireg,1) / WELLSOFF(1,ireg,1)
        IF (wellsoff(1,ireg,2).GT. 0.) SROFF(1,ireg,2) = SUCWELLOFF(1,ireg,2) / WELLSOFF(1,ireg,2)
        IF (wellsoff(2,ireg,1).GT. 0.) SROFF(2,ireg,1) = SUCWELLOFF(2,ireg,1) / WELLSOFF(2,ireg,1)
        IF (wellsoff(2,ireg,2).GT. 0.) SROFF(2,ireg,2) = SUCWELLOFF(2,ireg,2) / WELLSOFF(2,ireg,2)
        !WRITE(OFFOUT,*) 'weloff',yearof(toff),ireg,wellsoff(1,ireg,1),wellsoff(1,ireg,2), &
        !    wellsoff(2,ireg,1),wellsoff(2,ireg,2)
    ENDDO
ENDIF
!DO ireg = 1, OFFRGN
!    WRITE(OFFOUT,*) 'sroff',yearof(toff),ireg,sroff(1,ireg,1),sroff(1,ireg,2), &
!        sroff(2,ireg,1),sroff(2,ireg,2)
!ENDDO
! ....... remap inferred reserves into OGSM's variable
DO ireg = 1, OFFRGN
    r_unresoff(ireg,1) = r_unresoff(ireg,1) - (NRDOFF(ireg,1)+NDIROFF(ireg,1))
    r_unresoff(ireg,2) = r_unresoff(ireg,2) - (NRDOFF(ireg,2)+NDIROFF(ireg,2))
    r_infrsvoff(ireg,1) = r_infrsvoff(ireg,1) + NDIROFF(ireg,1) - EXTOFF(ireg,1) - REVOFF(ireg,1)
    r_infrsvoff(ireg,2) = r_infrsvoff(ireg,2) + NDIROFF(ireg,2) - EXTOFF(ireg,2) - REVOFF(ireg,2)
ENDDO

OGEOYURR(L48RGN+1,1,CURIYR) = r_unresoff(1,1) * 0.001
OGEOYURR(L48RGN+2,1,CURIYR) = (r_unresoff(3,1) + r_unresoff(4,1) + r_unresoff(5,1)) * 0.001
OGEOYURR(L48RGN+3,1,CURIYR) = r_unresoff(2,1) * 0.001
OGEOYURR(L48RGN+1,2,CURIYR) = r_unresoff(1,2) * 0.001
OGEOYURR(L48RGN+2,2,CURIYR) = (r_unresoff(3,2) + r_unresoff(4,2) + r_unresoff(5,2)) * 0.001
OGEOYURR(L48RGN+3,2,CURIYR) = r_unresoff(2,2) * 0.001
OGEOYINF(L48RGN+1,1,CURIYR) = r_infrsvoff(1,1) * 0.001
OGEOYINF(L48RGN+2,1,CURIYR) = (r_infrsvoff(3,1) + r_infrsvoff(4,1) + r_infrsvoff(5,1)) * 0.001
OGEOYINF(L48RGN+3,1,CURIYR) = r_infrsvoff(2,1) * 0.001
OGEOYINF(L48RGN+1,2,CURIYR) = r_infrsvoff(1,2) * 0.001
OGEOYINF(L48RGN+2,2,CURIYR) = (r_infrsvoff(3,2) + r_infrsvoff(4,2) + r_infrsvoff(5,2)) * 0.001
OGEOYINF(L48RGN+3,2,CURIYR) = r_infrsvoff(2,2) * 0.001

! DETERMINE EXPECTED PRODUCTION
EXPRDOFF = 0.
DO ff = 1, ntFld
    iREG = fld(ff)%OGSM
    IF (curiyr-offbyr+1.GE.1) THEN
        EXPRDOFF(IREG,1) = EXPRDOFF(IREG,1) + (fld(ff)%oilprd(toff)+fld(ff)%cndprd(toff) +  &
            fld(ff)%oilRGprd(toff)+ fld(ff)%cndRGprd(toff))/1000000.
        EXPRDOFF(IREG,2) = EXPRDOFF(IREG,2) + (fld(ff)%gasprd(toff)+fld(ff)%gasRGprd(toff))/1000000.
        IF (INDEX(fld(ff)%nmID,"STATECA").NE.0) THEN			! To report Pacific (state)
            OGOILPRD(73,1,toff+offbyr-1) = OGOILPRD(73,1,toff+offbyr-1) + (fld(ff)%oilprd(toff)+fld(ff)%cndprd(toff) +  &
                fld(ff)%oilRGprd(toff)+ fld(ff)%cndRGprd(toff))/1000000.
            OGENAGPRD(73,1,toff+offbyr-1) =  OGENAGPRD(73,1,toff+offbyr-1) + (fld(ff)%gasprd(toff)+fld(ff)%gasRGprd(toff))/1000000.
            OGADGPRD(73,1,toff+offbyr-1) =  OGADGPRD(73,1,toff+offbyr-1) + (fld(ff)%asgprd(toff)+fld(ff)%asgRGprd(toff))/1000000.
        ELSEIF (INDEX(fld(ff)%nmID,"STATEAL").NE.0) THEN
            OGOILPRD(70,1,toff+offbyr-1) = OGOILPRD(70,1,toff+offbyr-1) + (fld(ff)%oilprd(toff)+fld(ff)%cndprd(toff) +  &
                fld(ff)%oilRGprd(toff)+ fld(ff)%cndRGprd(toff))/1000000.
            OGENAGPRD(70,1,toff+offbyr-1) =  OGENAGPRD(70,1,toff+offbyr-1) + (fld(ff)%gasprd(toff)+fld(ff)%gasRGprd(toff))/1000000.
            OGADGPRD(70,1,toff+offbyr-1) =  OGADGPRD(70,1,toff+offbyr-1) + (fld(ff)%asgprd(toff)+fld(ff)%asgRGprd(toff))/1000000.
        ELSEIF (INDEX(fld(ff)%nmID,"STATELA").NE.0) THEN
            OGOILPRD(71,1,toff+offbyr-1) = OGOILPRD(71,1,toff+offbyr-1) + (fld(ff)%oilprd(toff)+fld(ff)%cndprd(toff) +  &
                fld(ff)%oilRGprd(toff)+ fld(ff)%cndRGprd(toff))/1000000.
            OGENAGPRD(71,1,toff+offbyr-1) =  OGENAGPRD(71,1,toff+offbyr-1) + (fld(ff)%gasprd(toff)+fld(ff)%gasRGprd(toff))/1000000.
            OGADGPRD(71,1,toff+offbyr-1) =  OGADGPRD(71,1,toff+offbyr-1) + (fld(ff)%asgprd(toff)+fld(ff)%asgRGprd(toff))/1000000.
        ELSEIF (INDEX(fld(ff)%nmID,"STATETX").NE.0) THEN
            OGOILPRD(72,1,toff+offbyr-1) = OGOILPRD(72,1,toff+offbyr-1) + (fld(ff)%oilprd(toff)+fld(ff)%cndprd(toff) +  &
                fld(ff)%oilRGprd(toff)+ fld(ff)%cndRGprd(toff))/1000000.
            OGENAGPRD(72,1,toff+offbyr-1) =  OGENAGPRD(72,1,toff+offbyr-1) + (fld(ff)%gasprd(toff)+fld(ff)%gasRGprd(toff))/1000000.
            OGADGPRD(72,1,toff+offbyr-1) =  OGADGPRD(72,1,toff+offbyr-1) + (fld(ff)%asgprd(toff)+fld(ff)%asgRGprd(toff))/1000000.
        ELSE	! Compute federal
            IF (fld(ff)%PA .eq. 1) THEN
                OGOILPRD(81,1,toff+offbyr-1) = OGOILPRD(81,1,toff+offbyr-1) + (fld(ff)%oilprd(toff)+fld(ff)%cndprd(toff) +  &
                    fld(ff)%oilRGprd(toff)+ fld(ff)%cndRGprd(toff))/1000000.
                OGENAGPRD(81,1,toff+offbyr-1) =  OGENAGPRD(81,1,toff+offbyr-1) + (fld(ff)%gasprd(toff)+fld(ff)%gasRGprd(toff))/1000000.
                OGADGPRD(81,1,toff+offbyr-1) =  OGADGPRD(81,1,toff+offbyr-1) + (fld(ff)%asgprd(toff)+fld(ff)%asgRGprd(toff))/1000000.
            ENDIF
            IF (fld(ff)%PA .eq. 2) THEN
                OGOILPRD(80,1,toff+offbyr-1) = OGOILPRD(80,1,toff+offbyr-1) + (fld(ff)%oilprd(toff)+fld(ff)%cndprd(toff) +  &
                    fld(ff)%oilRGprd(toff)+ fld(ff)%cndRGprd(toff))/1000000.
                OGENAGPRD(80,1,toff+offbyr-1) =  OGENAGPRD(80,1,toff+offbyr-1) + (fld(ff)%gasprd(toff)+fld(ff)%gasRGprd(toff))/1000000.
                OGADGPRD(80,1,toff+offbyr-1) =  OGADGPRD(80,1,toff+offbyr-1) + (fld(ff)%asgprd(toff)+fld(ff)%asgRGprd(toff))/1000000.
            ENDIF
            IF (fld(ff)%PA .eq. 3) THEN
                OGOILPRD(79,1,toff+offbyr-1) = OGOILPRD(79,1,toff+offbyr-1) + (fld(ff)%oilprd(toff)+fld(ff)%cndprd(toff) +  &
                    fld(ff)%oilRGprd(toff)+ fld(ff)%cndRGprd(toff))/1000000.
                OGENAGPRD(79,1,toff+offbyr-1) =  OGENAGPRD(79,1,toff+offbyr-1) + (fld(ff)%gasprd(toff)+fld(ff)%gasRGprd(toff))/1000000.
                OGADGPRD(79,1,toff+offbyr-1) =  OGADGPRD(79,1,toff+offbyr-1) + (fld(ff)%asgprd(toff)+fld(ff)%asgRGprd(toff))/1000000.
            ENDIF
            IF (fld(ff)%PA .eq. 4) THEN
                OGOILPRD(76,1,toff+offbyr-1) = OGOILPRD(76,1,toff+offbyr-1) + (fld(ff)%oilprd(toff)+fld(ff)%cndprd(toff) +  &
                    fld(ff)%oilRGprd(toff)+ fld(ff)%cndRGprd(toff))/1000000.
                OGENAGPRD(76,1,toff+offbyr-1) =  OGENAGPRD(76,1,toff+offbyr-1) + (fld(ff)%gasprd(toff)+fld(ff)%gasRGprd(toff))/1000000.
                OGADGPRD(76,1,toff+offbyr-1) =  OGADGPRD(76,1,toff+offbyr-1) + (fld(ff)%asgprd(toff)+fld(ff)%asgRGprd(toff))/1000000.
            ENDIF
            IF (fld(ff)%PA .eq. 5) THEN
                OGOILPRD(77,1,toff+offbyr-1) = OGOILPRD(77,1,toff+offbyr-1) + (fld(ff)%oilprd(toff)+fld(ff)%cndprd(toff) +  &
                    fld(ff)%oilRGprd(toff)+ fld(ff)%cndRGprd(toff))/1000000.
                OGENAGPRD(77,1,toff+offbyr-1) =  OGENAGPRD(77,1,toff+offbyr-1) + (fld(ff)%gasprd(toff)+fld(ff)%gasRGprd(toff))/1000000.
                OGADGPRD(77,1,toff+offbyr-1) =  OGADGPRD(77,1,toff+offbyr-1) + (fld(ff)%asgprd(toff)+fld(ff)%asgRGprd(toff))/1000000.
            ENDIF
            IF (fld(ff)%PA .eq. 6) THEN
                OGOILPRD(78,1,toff+offbyr-1) = OGOILPRD(78,1,toff+offbyr-1) + (fld(ff)%oilprd(toff)+fld(ff)%cndprd(toff) +  &
                    fld(ff)%oilRGprd(toff)+ fld(ff)%cndRGprd(toff))/1000000.
                OGENAGPRD(78,1,toff+offbyr-1) =  OGENAGPRD(78,1,toff+offbyr-1) + (fld(ff)%gasprd(toff)+fld(ff)%gasRGprd(toff))/1000000.
                OGADGPRD(78,1,toff+offbyr-1) =  OGADGPRD(78,1,toff+offbyr-1) + (fld(ff)%asgprd(toff)+fld(ff)%asgRGprd(toff))/1000000.
            ENDIF
            IF (fld(ff)%PA .eq. 8) THEN
                OGOILPRD(83,1,toff+offbyr-1) = OGOILPRD(83,1,toff+offbyr-1) + (fld(ff)%oilprd(toff)+fld(ff)%cndprd(toff) +  &
                    fld(ff)%oilRGprd(toff)+ fld(ff)%cndRGprd(toff))/1000000.
                OGENAGPRD(83,1,toff+offbyr-1) =  OGENAGPRD(83,1,toff+offbyr-1) + (fld(ff)%gasprd(toff)+fld(ff)%gasRGprd(toff))/1000000.
                OGADGPRD(83,1,toff+offbyr-1) =  OGADGPRD(83,1,toff+offbyr-1) + (fld(ff)%asgprd(toff)+fld(ff)%asgRGprd(toff))/1000000.
            ENDIF
            IF (fld(ff)%PA .ge. 9 .and. fld(ff)%PA .le. 11) THEN
                OGOILPRD(82,1,toff+offbyr-1) = OGOILPRD(82,1,toff+offbyr-1) + (fld(ff)%oilprd(toff)+fld(ff)%cndprd(toff) +  &
                    fld(ff)%oilRGprd(toff)+ fld(ff)%cndRGprd(toff))/1000000.
                OGENAGPRD(82,1,toff+offbyr-1) =  OGENAGPRD(82,1,toff+offbyr-1) + (fld(ff)%gasprd(toff)+fld(ff)%gasRGprd(toff))/1000000.
                OGADGPRD(82,1,toff+offbyr-1) =  OGADGPRD(82,1,toff+offbyr-1) + (fld(ff)%asgprd(toff)+fld(ff)%asgRGprd(toff))/1000000.
            ENDIF
        ENDIF
    ENDIF
ENDDO

EXPRDOFF2 = 0.
DO I=67,ogdist
    IF (DISTMAP(I,1).ge.L48RGN+1.and.DISTMAP(I,1).le.L48RGN+3) ogoilprd(i,1,curiyr) = ogoilprd(i,1,curiyr)/365.  ! convert to mmb/d
    !       * SPLIT INTO NA/AD for STATE
    IF (I.ge.70.and.I.le.72) THEN
        IF(OGENAGPRD(I,1,L48HYR)+OGADGPRD(I,1,L48HYR).gt.0)   &
            OGADGPRD(I,1,curiyr) = OGENAGPRD(I,1,CURIYR) * OGADGPRD(I,1,L48HYR)/(OGENAGPRD(I,1,L48HYR)+OGADGPRD(I,1,L48HYR))
        OGENAGPRD(I,1,CURIYR) = OGENAGPRD(I,1,CURIYR) - OGADGPRD(I,1,CURIYR)
    ENDIF
    OGENAGPRD(I,gastypes,curiyr) =  sum(OGENAGPRD(I,1:gastypes-1,curiyr))
    IF (DISTMAP(I,1).GT.0) EXPRDOFF2(DISTMAP(I,1)) = EXPRDOFF2(DISTMAP(I,1)) + OGENAGPRD(I,gastypes,curiyr)
    OGADGPRD(I,oiltypes,curiyr) =  sum(OGADGPRD(I,1:oiltypes-1,curiyr))
ENDDO
DO R=1,offrgn
    IF(CURRESOFF(R,1).GT.0.) PRRATOFF(R,1) = EXPRDOFF(R,1)/CURRESOFF(R,1)
    IF(CURRESOFF(R,2).GT.0.) PRRATOFF(R,2) = EXPRDOFF(R,2)/CURRESOFF(R,2)
ENDDO
IF (OGRESNGOF(1,CURIYR).gt.0.) OGPRRNGOF(1,CURIYR) = EXPRDOFF2(L48RGN+1)/OGRESNGOF(1,curiyr)
IF (OGRESNGOF(3,CURIYR).gt.0.) OGPRRNGOF(3,CURIYR) = EXPRDOFF2(L48RGN+3)/OGRESNGOF(3,curiyr)
IF (OGRESNGOF(2,CURIYR).gt.0.) OGPRRNGOF(2,CURIYR) = EXPRDOFF2(L48RGN+2)/OGRESNGOF(2,curiyr)

ndevprj = 0

!     SET LAST HISTORICAL YEAR and FIRST STEO YEAR SO THAT THERE IS NO VARIATION ACROSS CASES
IF (curiyr.eq.l48hyr.and.ogSTEO.eq.1) THEN
    DO DIST=67,OGDIST
        OGENAGPRD(DIST,GASTYPES,CURIYR) = SUM(OGENAGPRD(DIST,1:GASTYPES-1,CURIYR))
        OGADGPRD(DIST,OILTYPES,CURIYR) = SUM(OGADGPRD(DIST,1:OILTYPES-1,CURIYR))
        DO K=1,GASTYPES-1
            IF(OGENAGPRD(DIST,GASTYPES,CURIYR).gt.0.)   &
                OGENAGPRD(DIST,K,CURIYR) = sOGENAGPRD(DIST,1) * OGENAGPRD(DIST,K,CURIYR)/OGENAGPRD(DIST,GASTYPES,CURIYR)
        ENDDO
        DO K=1,OILTYPES-1
            IF(OGADGPRD(DIST,OILTYPES,CURIYR).gt.0.)   &
                OGADGPRD(DIST,K,CURIYR) = sOGADGPRD(DIST,1) * OGADGPRD(DIST,K,CURIYR)/OGADGPRD(DIST,OILTYPES,CURIYR)
        ENDDO
        OGENAGPRD(DIST,GASTYPES,CURIYR) = SUM(OGENAGPRD(DIST,1:GASTYPES-1,CURIYR))
        OGADGPRD(DIST,OILTYPES,CURIYR) = SUM(OGADGPRD(DIST,1:OILTYPES-1,CURIYR))
    ENDDO
ENDIF
IF ((curiyr.eq.l48hyr+1.and.ogSTEO.eq.1)) THEN
    DO DIST=67,OGDIST
        OGENAGPRD(DIST,GASTYPES,CURIYR) = SUM(OGENAGPRD(DIST,1:GASTYPES-1,CURIYR))
        OGADGPRD(DIST,OILTYPES,CURIYR) = SUM(OGADGPRD(DIST,1:OILTYPES-1,CURIYR))
        DO K=1,GASTYPES-1
            IF(OGENAGPRD(DIST,GASTYPES,CURIYR).gt.0.)   &
                OGENAGPRD(DIST,K,CURIYR) = sOGENAGPRD(DIST,2) * OGENAGPRD(DIST,K,CURIYR)/OGENAGPRD(DIST,GASTYPES,CURIYR)
        ENDDO
        IF(OGENAGPRD(DIST,GASTYPES,CURIYR).le.0.) OGENAGPRD(DIST,1,CURIYR) = sOGENAGPRD(DIST,2)
        DO K=1,OILTYPES-1
            IF(OGADGPRD(DIST,OILTYPES,CURIYR).gt.0.)   &
                OGADGPRD(DIST,K,CURIYR) = sOGADGPRD(DIST,2) * OGADGPRD(DIST,K,CURIYR)/OGADGPRD(DIST,OILTYPES,CURIYR)
        ENDDO
        IF(OGADGPRD(DIST,OILTYPES,CURIYR).le.0.) OGADGPRD(DIST,K,CURIYR) = sOGADGPRD(DIST,2)
        OGENAGPRD(DIST,GASTYPES,CURIYR) = SUM(OGENAGPRD(DIST,1:GASTYPES-1,CURIYR))
        OGADGPRD(DIST,OILTYPES,CURIYR) = SUM(OGADGPRD(DIST,1:OILTYPES-1,CURIYR))
    ENDDO
ENDIF

END SUBROUTINE OGreporttoogsm

!************************************************************************************!
!
!************************************************************************************!
SUBROUTINE OGOUT1_OFF
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ncntrl'       ! nems control variables
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmbfw'      ! ogsm system variables
INCLUDE 'ogsmoff'      ! offshore variables
INCLUDE 'ogsml48'
INCLUDE 'ogsmout'
INCLUDE 'pmmout'
INCLUDE 'ngtdmout'
INCLUDE 'dwpparm'

! subroutine variables

INTEGER RGN(OFFRGN) ! HOLDS REGION NUMBERS
DATA RGN/1,2,3,4,5/
INTEGER ireg, ff, count1, count2
REAL*4 REMAIN1(OFFFUEL)  ! HOLDS RESOURCE DATA VALUES
REAL*4 REMAIN2(OFFFUEL)
REAL*4 REMAIN3(OFFFUEL)
REAL*4 RESTOTCR   ! HOLDS TOTAL GOM CRUDE PRODUCTION
REAL*4 RESTOTNG   ! HOLDS TOTAL GOM NG PRODUCTION
REAL*4 TOTRSVGOM(OFFFUEL)  ! ACCUMULATE GOM RESERVES FOR NEMS
REAL*4 TOTprdGOM(OFFFUEL)  ! ACCUMULATE GOM production
REAL*4 AGGPRROFF(OFFFUEL)  ! AGGREGATE GOM P/R RATIO
REAL*4 TOTPRD1(OFFFUEL)  ! USED TO COMPUTE AGGPRROFF
REAL*4 TOTPRD2(OFFFUEL)  ! USED TO COMPUTE AGGPRROFF
REAL*4 TOTRSV1(OFFFUEL)  ! USED TO COMPUTE AGGPRROFF
REAL*4 PRDTOTCR          !total expected crude oil production
REAL*4 PRDTOTNG          ! total expected natural gas production
REAL*4 PRRATNEW(offrgn,offfuel) ! P/R RATIO OF FOR RESERVE ADDITIONS
REAL*4 PRRATLAG                  ! LAGGED P/R RATIO
REAL*4 RESSUM, PRDSUM, ADGSHR
REAL*4 PRRATAVG(offrgn,offfuel)
REAL*4 ELAST_OFF(offrgn,offfuel)
REAL*4 sumwells1(3,offfuel), sumwells2(3,offfuel)
REAL PRCAP

DATA elast_off/0.1156,0.1812,0.1156,0.1156,0.1156, &
    0.1156,0.1812,0.1156,0.1156,0.1156/
DATA prratnew/0.10,0.10,0.20,0.18,0.10, &
    0.15,0.15,0.20,0.15,0.15/

! DETERMINE PACIFIC DRILLING AND RESERVE ADDITIONS
!IF (curiyr.gt.offnhyr) THEN
!    WELLSOFF(2,2,1) = ANINT(HISTWELOFF(2,2,1,offnhyr) * (dcrdwhp(L48RGN+3,curiyr) &
!        / dcrdwhp(L48RGN+3,offnhyr))**(1.0))
!    WELLSOFF(2,2,2) = 1.0
!    FR3OFF(2,1) = 2.00
!    FR3OFF(2,2) = 5.0
!    DO K=1,OFFFUEL
!        DEVOFF(2,K) = ANINT(WELLSOFF(2,2,K)*SROFF(2,2,K))
!        REVOFF(2,K) = DEVOFF(2,K) * FR3OFF(2,K)
!    ENDDO
!ENDIF

DO R=1,OFFRGN
    DO K=1,OFFFUEL
        ! COMPUTE FINDING RATES
        if (DEVOFF(r,k).gt.0.0) then
            fr3OFF(r,k) = REVOFF(r,k)/DEVOFF(r,k)
        ELSE
            fr3OFF(r,k) = 0.0
        endif
        if (nfwOFF(r,k).gt.0.0) then
            fr1OFF(r,k) = NRDOFF(r,k)/NFWOFF(r,k)
        ELSE
            fr1OFF(r,k) = 0.0
        endif

        ! COMPUTE RESERVE ADDITIONS
        RESADOFF(R,K) = NRDOFF(R,K) + REVOFF(R,K) + EXTOFF(R,K)
        !write(offout,600) curiyr+1989,r,k,resadOFF(r,k), &
        !    nrdOFF(r,k),revOFF(r,k),extOFF(r,k)
600     FORMAT('resadoff',I6,2I2,4F8.2)

        ! ACCUMULATE THE RESERVES FOR CALCULATION OF NEW DELTAS
        CUMR2OFF(R,K) = (CUMR2OFF(R,K)+NDIROFF(R,K))
        CUMR3OFF(R,K) = CUMR3OFF(R,K) + EXTOFF(R,K) + &
            REVOFF(R,K)

    ENDDO
ENDDO


!  COMPUTE EXPECTED PRODUCTION
!  Added variables to report state offshore versus federal offshore for Gulf, Pacific, and Atlantic (Sept 2014)

DO count1=1,2
    OGPRDOFF(1,count1,toff+offbyr-1) = 0.	! Initialize global reporting variables (for GOM)
    OGPRDOFF(2,count1,toff+offbyr-1) = 0.	! Initialize global reporting variables (for Pacific)
    OGPRDOFF(3,count1,toff+offbyr-1) = 0.	! Initialize global reporting variables (for Atlantic)
    !write(offout,*) 'tey1', curiyr, OGPRDOFF(1,count,curiyr), OGPRDOFF(2,count,curiyr), OGPRDOFF(3,count,curiyr)
ENDDO

EXPRDOFF = 0.
PRDOFF = 0.

DO ff = 1, ntFld
    iREG = fld(ff)%OGSM
    IF (curiyr-offbyr+1.GE.1) THEN
        IF (INDEX(fld(ff)%nmID,"STATECA").NE.0) THEN			! To report Pacific (state)
            EXPRDOFF(IREG,1) = EXPRDOFF(IREG,1) + (fld(ff)%oilprd(toff)+fld(ff)%cndprd(toff) +  &
                fld(ff)%oilRGprd(toff)+ fld(ff)%cndRGprd(toff))/1000000.
            EXPRDOFF(IREG,2) = EXPRDOFF(IREG,2) + (fld(ff)%gasprd(toff)+fld(ff)%gasRGprd(toff))/1000000.

            ! Putting STATECA oil and gas expected production numbers into reporting array
            OGPRDOFF(2,1,toff+offbyr-1) = OGPRDOFF(2,1,toff+offbyr-1) + (fld(ff)%oilprd(toff) + fld(ff)%cndprd(toff) +  &
                fld(ff)%oilRGprd(toff)+ fld(ff)%cndRGprd(toff))/1000000.
            OGPRDOFF(2,2,toff+offbyr-1) = OGPRDOFF(2,2,toff+offbyr-1)+ (fld(ff)%gasprd(toff) + fld(ff)%gasRGprd(toff) + &
                fld(ff)%asgprd(toff) + fld(ff)%asgRGprd(toff))/1000000.
            !write(offout,*) 'teyCAg', fld(ff)%nmID, toff, fld(ff)%gasprd(toff), fld(ff)%gasRGprd(toff), fld(ff)%asgprd(toff), fld(ff)%asgRGprd(toff) ! DBG_Flag_teyCA

            ! Used to set PR ratios for following year
            PRDOFF(IREG,1) = PRDOFF(IREG,1) + (fld(ff)%oilprd(toff+1)+fld(ff)%cndprd(toff+1) +  &
                fld(ff)%oilRGprd(toff+1)+ fld(ff)%cndRGprd(toff+1))/1000000.
            PRDOFF(IREG,2) = PRDOFF(IREG,2) + (fld(ff)%gasprd(toff+1)+fld(ff)%gasRGprd(toff+1))/1000000.
            !write(offout,*) 'tey2', 'ID ',fld(ff)%nmID, 'iREG ', fld(ff)%OGSM, curiyr, OGPRDOFF(2,1,curiyr), OGPRDOFF(2,2,curiyr)

        ELSE IF (INDEX(fld(ff)%nmID,"STATEAL").NE.0.OR.INDEX(fld(ff)%nmID,"STATELA").NE.0.OR.INDEX(fld(ff)%nmID,"STATETX").NE.0) THEN		! To report GOM (state)
            EXPRDOFF(IREG,1) = EXPRDOFF(IREG,1) + (fld(ff)%oilprd(toff)+fld(ff)%cndprd(toff) +  &
                fld(ff)%oilRGprd(toff)+ fld(ff)%cndRGprd(toff))/1000000.
            EXPRDOFF(IREG,2) = EXPRDOFF(IREG,2) + (fld(ff)%gasprd(toff)+fld(ff)%gasRGprd(toff))/1000000.
            !EXPRDOFF(IREG,2) = CURRESOFF(IREG,2) * PRRATOFF(IREG,2)

            ! Putting STATEAL + STATELA + STATETX oil and gas expected production numbers into reporting array
            OGPRDOFF(1,1,toff+offbyr-1) = OGPRDOFF(1,1,toff+offbyr-1) + (fld(ff)%oilprd(toff)+fld(ff)%cndprd(toff) +  &
                fld(ff)%oilRGprd(toff)+ fld(ff)%cndRGprd(toff))/1000000.
            OGPRDOFF(1,2,toff+offbyr-1) = OGPRDOFF(1,2,toff+offbyr-1)+ (fld(ff)%gasprd(toff)+fld(ff)%gasRGprd(toff) + &
                fld(ff)%asgprd(toff) + fld(ff)%asgRGprd(toff))/1000000.

            ! Used to set PR ratios for following year
            PRDOFF(IREG,1) = PRDOFF(IREG,1) + (fld(ff)%oilprd(toff+1)+fld(ff)%cndprd(toff+1) +  &
                fld(ff)%oilRGprd(toff+1)+ fld(ff)%cndRGprd(toff+1))/1000000.
            PRDOFF(IREG,2) = PRDOFF(IREG,2) + (fld(ff)%gasprd(toff+1)+fld(ff)%gasRGprd(toff+1))/1000000.
            !write(offout,*) 'tey3', 'ID ', fld(ff)%nmID, 'iREG ', fld(ff)%OGSM, curiyr, OGPRDOFF(1,1,curiyr), OGPRDOFF(1,2,curiyr)

        ELSE	! Compute federal
            EXPRDOFF(IREG,1) = EXPRDOFF(IREG,1) + (fld(ff)%oilprd(toff)+fld(ff)%cndprd(toff) +  &
                fld(ff)%oilRGprd(toff)+ fld(ff)%cndRGprd(toff))/1000000.
            EXPRDOFF(IREG,2) = EXPRDOFF(IREG,2) + (fld(ff)%gasprd(toff)+fld(ff)%gasRGprd(toff))/1000000.
            !EXPRDOFF(IREG,2) = CURRESOFF(IREG,2) * PRRATOFF(IREG,2)
            PRDOFF(IREG,1) = PRDOFF(IREG,1) + (fld(ff)%oilprd(toff+1)+fld(ff)%cndprd(toff+1) +  &
                fld(ff)%oilRGprd(toff+1)+ fld(ff)%cndRGprd(toff+1))/1000000.
            PRDOFF(IREG,2) = PRDOFF(IREG,2) + (fld(ff)%gasprd(toff+1)+fld(ff)%gasRGprd(toff+1))/1000000.
            !write(offout,*) 'teyfed ', 'ID ', fld(ff)%nmID, toff+offbyr-1, EXPRDOFF(iREG,1), EXPRDOFF(iREG,2)

        ENDIF
        !write(offout,*) 'teyn1 ', 'ID ',fld(ff)%nmID, 'iREG ', fld(ff)%OGSM, yearof(toff+offbyr-1), OGPRDOFF(2,1,toff+offbyr-1), OGPRDOFF(2,2,toff+offbyr-1), &
        !    OGPRDOFF(1,1,toff+offbyr-1), OGPRDOFF(1,2,toff+offbyr-1)

    ELSE
        EXPRDOFF(IREG,1) = EXPRDOFF(IREG,1) + (fld(ff)%oilprdhis(curiyr))/1000000.
        EXPRDOFF(IREG,2) = EXPRDOFF(IREG,2) + (fld(ff)%gasprdhis(curiyr))/1000000.
        IF (INDEX(fld(ff)%nmID,"STATECA").NE.0) THEN		! To report Pacific (state)
            OGPRDOFF(2,1,toff+offbyr-1) = OGPRDOFF(2,1,toff+offbyr-1) + (fld(ff)%oilprdhis(curiyr))/1000000.
            OGPRDOFF(2,2,toff+offbyr-1) = OGPRDOFF(2,2,toff+offbyr-1) + (fld(ff)%gasprdhis(curiyr))/1000000.
            !write(offout,*) 'tey4', 'ID ', fld(ff)%nmID, 'iREG ', fld(ff)%OGSM, curiyr, OGPRDOFF(2,1,curiyr), OGPRDOFF(2,2,curiyr)

        ELSE IF (INDEX(fld(ff)%nmID,"STATEAL").NE.0.OR.INDEX(fld(ff)%nmID,"STATELA").NE.0.OR.INDEX(fld(ff)%nmID,"STATETX").NE.0) THEN	! To report GOM (state)
            OGPRDOFF(1,1,toff+offbyr-1) = OGPRDOFF(1,1,toff+offbyr-1) + (fld(ff)%oilprdhis(curiyr))/1000000.
            OGPRDOFF(1,2,toff+offbyr-1) = OGPRDOFF(1,2,toff+offbyr-1) + (fld(ff)%gasprdhis(curiyr))/1000000.
            !write(offout,*) 'tey5', 'ID ',fld(ff)%nmID, 'iREG ', fld(ff)%OGSM, curiyr, OGPRDOFF(1,1,curiyr), OGPRDOFF(1,2,curiyr)

        ELSE
            PRDOFF(IREG,1) = PRDOFF(IREG,1) + (fld(ff)%oilprdhis(curiyr))/1000000.
            PRDOFF(IREG,2) = PRDOFF(IREG,2) + (fld(ff)%gasprdhis(curiyr))/1000000.*0.72 ! roughly 72% is NA gas
            !write(offout,*) 'tey ', 'ID ', fld(ff)%nmID, 'iREG ', fld(ff)%OGSM, yearof0(toff+offbyr-1), PRDOFF(iREG,1), PRDOFF(iREG,2)
        ENDIF
        !write(offout,*) 'teyn2 ', 'ID ',fld(ff)%nmID, 'iREG ', fld(ff)%OGSM, yearof0(toff+offbyr-1), OGPRDOFF(2,1,toff+offbyr-1), OGPRDOFF(2,2,toff+offbyr-1), &
        !    OGPRDOFF(1,1,toff+offbyr-1), OGPRDOFF(1,2,toff+offbyr-1)

    ENDIF
ENDDO

DO I=67,ogdist
    IF(OGRUNOP(5) == 0.and.CURIYR.GE.OFFBYR) THEN
        if (DISTMAP(I,1).eq.L48RGN+1.and.exprdoff(1,2).gt.0.)  &
            ogrnagprd(I,gastypes,curiyr) = ogprdngof(1,curiyr) * ogenagprd(I,1,curiyr) / exprdoff(1,2)
        if (DISTMAP(I,1).eq.L48RGN+2.and.sum(exprdoff(3:5,2)).gt.0.)  &
            ogrnagprd(I,gastypes,curiyr) = ogprdngof(2,curiyr) * ogenagprd(I,1,curiyr) / sum(exprdoff(3:5,2))
        if (DISTMAP(I,1).eq.L48RGN+3.and.exprdoff(2,2).gt.0.)  &
            ogrnagprd(I,gastypes,curiyr) = ogprdngof(3,curiyr) * ogenagprd(I,1,curiyr) / exprdoff(2,2)
    ENDIF
    OGRNAGPRD(I,1,curiyr) =  OGRNAGPRD(I,gastypes,curiyr)
    !if (DISTMAP(i,1).eq.L48RGN+2) write(6,*)  'dh5out', curiyr+1989, i, ogprdngof(2,curiyr), ogenagprd(i,1,curiyr), ogrnagprd(i,gastypes,curiyr)
ENDDO

!DO count2=1,MNUMYR
!    WRITE(offout,*) 'tey ', 'CA State oil and gas ', OGPRDOFF(2,1,count2), OGPRDOFF(2,2,count2), &
!        'AL+LA+TX State oil and gas ', OGPRDOFF(1,1,count2), OGPRDOFF(1,2,count2)
!ENDDO
!
!IF (curiyr-offbyr+1.LT.1) THEN
!    DO R=1,OFFRGN
!        DO K=1,OFFFUEL
!            EXPRDOFF(R,K) = CURRESOFF(R,K) * PRRATOFF(R,K)
!        ENDDO
!    ENDDO
!ENDIF

IF (ogrunop(5).eq.1.or.curiyr.lt.offbyr) THEN
    ogprdngof(1,curiyr) = 0.
    ogprdngof(2,curiyr) = 0.
    ogprdngof(3,curiyr) = 0.
    DO R=67,OGDIST
        IF (DISTMAP(R,1).eq.L48RGN+1) ogprdngof(1,curiyr) = ogprdngof(1,curiyr) + ogrnagprd(R,gastypes,curiyr)
        IF (DISTMAP(R,1).eq.L48RGN+2) ogprdngof(2,curiyr) = ogprdngof(2,curiyr) + ogrnagprd(R,gastypes,curiyr)
        IF (DISTMAP(R,1).eq.L48RGN+3) ogprdngof(3,curiyr) = ogprdngof(3,curiyr) + ogrnagprd(R,gastypes,curiyr)
    ENDDO
ENDIF

!  ------------ COMPUTE OFFSHORE PRODUCTION -------------
!  IF NOT STAND-ALONE (OGRUNOP(1)=0) THEN USE COMPUTED PRODUCTION
IF (OGRUNOP(1) .EQ. 0) THEN

    !  CONVERT PRODUCTION FROM 1 GOM REGION TO 3 GOM OGSM REGIONS
    !  SPLIT THE PRODUCTION USING EXPECTED PRODUCTION
    PRDTOTCR = 0.0
    PRDTOTNG = 0.0
    DO R=3,OFFRGN   ! LOOP OVER GOM REGIONS (3-5)
        PRDTOTCR = PRDTOTCR + EXPRDOFF(R,1)
        PRDTOTNG = PRDTOTNG + EXPRDOFF(R,2)
    ENDDO

    !  ASSIGN PRODUCTION VALUES FROM OTHER MODULES TO OGPRODOFF
    !  CONVERT RFQDCRD FROM MMB/DAY TO MMB/YEAR
    DO R=1,OFFRGN
        IF (R.EQ.1) THEN ! REGION IS OGSM ATLANTIC
            OGPRODOFF(1,1) = RFQDCRD(L48RGN+1,CURIYR) * 365.0
            OGPRODOFF(1,2) = OGPRDNGOF(1,CURIYR)
        ELSEIF (R.EQ.2) THEN  ! REGION IS OGSM PACIFIC
            OGPRODOFF(2,1) = RFQDCRD(L48RGN+3,CURIYR) * 365.0
            OGPRODOFF(2,2) = OGPRDNGOF(3,CURIYR) ! NGTDM RGN 3=PAC
        ELSE   ! REGION IS OGSM GOM
            !  CONVERT PRODUCTION FROM 1 GOM REGION TO 2 GOM OGSM REGIONS

            IF (PRDTOTCR .GT. 0) THEN
                OGPRODOFF(R,1) = (EXPRDOFF(R,1) / PRDTOTCR) * &
                    RFQDCRD(L48RGN+2,CURIYR) * 365.0
            ELSE
                OGPRODOFF(R,1) = 0.0
            ENDIF
            IF (PRDTOTNG .GT. 0) THEN
                OGPRODOFF(R,2) = (EXPRDOFF(R,2) / PRDTOTNG) * &
                    OGPRDNGOF(2,CURIYR) ! NGTDM RGN 2=GOM
            ELSE
                OGPRODOFF(R,2) = 0.0
            ENDIF
        ENDIF
    ENDDO
    !OGPRODOFF(3,2) = EXPRDOFF(3,2) + (OGPRDNGOF(2,CURIYR)-PRDTOTNG)*0.10
    !OGPRODOFF(4,2) = EXPRDOFF(4,2) + (OGPRDNGOF(2,CURIYR)-PRDTOTNG)*0.90

ELSEIF (OGRUNOP(1).EQ.1.OR.OGRUNOP(1).EQ.2) THEN

    DO R=1,OFFRGN
        DO K=1,OFFFUEL
            OGPRODOFF(R,K) = EXPRDOFF(R,K)
        ENDDO
    ENDDO

    DO I=67,ogdist
        DO K=1,gastypes
            OGRNAGPRD(I,K,curiyr) =  OGENAGPRD(I,K,curiyr)
        ENDDO
    ENDDO

ELSEIF (OGRUNOP(1).EQ.3) THEN    ! USE EXPECTED OIL PROD.

    ! CONVERT PRODUCTION FROM 1 GOM REGION TO 2 GOM OGSM REGIONS
    ! SPLIT THE PRODUCTION USING EXPECTED PRODUCTION
    PRDTOTNG = 0.0
    DO R=3,OFFRGN   ! LOOP OVER GOM REGIONS (3-5)
        PRDTOTNG = PRDTOTNG + EXPRDOFF(R,2)
    ENDDO

    ! ASSIGN PRODUCTION VALUES FROM OTHER MODULES TO OGPRODOFF
    ! CONVERT RFQDCRD FROM MMB/DAY TO MMB/YEAR
    DO R=1,OFFRGN
        IF (R.EQ.1) THEN ! REGION IS OGSM ATLANTIC
            OGPRODOFF(1,2) = OGPRDNGOF(1,CURIYR)
        ELSE IF (R.EQ.2) THEN  ! REGION IS OGSM PACIFIC
            OGPRODOFF(2,2) = OGPRDNGOF(3,CURIYR) ! NGTDM RGN 3=PAC
        ELSE   ! REGION IS OGSM GOM
            OGPRODOFF(R,2) = (EXPRDOFF(R,2) / PRDTOTNG) * &
                OGPRDNGOF(2,CURIYR) ! NGTDM RGN 2=GOM
        ENDIF
    ENDDO

    DO R=1,OFFRGN
        OGPRODOFF(R,1) = EXPRDOFF(R,1)
    ENDDO

ELSEIF (OGRUNOP(1).EQ.4) THEN    ! USE EXPECTED GAS PROD.

    ! CONVERT PRODUCTION FROM 1 GOM REGION TO 3 GOM OGSM REGIONS
    ! SPLIT THE PRODUCTION USING RESERVES.
    PRDTOTCR = 0.0
    PRDTOTNG = 0.0
    DO R=3,OFFRGN   ! LOOP OVER GOM REGIONS (3-5)
        PRDTOTCR = PRDTOTCR + EXPRDOFF(R,1)
        PRDTOTNG = PRDTOTNG + EXPRDOFF(R,2)
    ENDDO

    ! ASSIGN PRODUCTION VALUES FROM OTHER MODULES TO OGPRODOFF
    ! CONVERT RFQDCRD FROM MMB/DAY TO MMB/YEAR
    DO R=1,OFFRGN
        IF (R.EQ.1) THEN ! REGION IS OGSM ATLANTIC
            OGPRODOFF(1,1) = RFQDCRD(L48RGN+1,CURIYR) * 365.0
        ELSE IF (R.EQ.2) THEN  ! REGION IS OGSM PACIFIC
            OGPRODOFF(2,1) = RFQDCRD(L48RGN+3,CURIYR) * 365.0
        ELSE   ! REGION IS OGSM GOM
            OGPRODOFF(R,1) = (EXPRDOFF(R,1) / PRDTOTCR) * &
                RFQDCRD(L48RGN+2,CURIYR) * 365.0
        ENDIF
    ENDDO

    DO R=1,OFFRGN
        OGPRODOFF(R,2) = EXPRDOFF(R,2)
    ENDDO

    DO I=67,ogdist
        DO K=1,gastypes
            OGRNAGPRD(I,K,curiyr) =  OGENAGPRD(I,K,curiyr)
        ENDDO
    ENDDO

ENDIF   ! ENDIF THIS RUN IS A STAND-ALONE RUN OF OGSM

ogprdoff(1,2,curiyr) = 0.
ogprdoff(2,2,curiyr) = 0.
DO I=67,OGDIST
    IF (I.EQ.73) THEN                       ! To report Pacific (state)
        OGPRDOFF(2,2,curiyr) = OGRNAGPRD(I,gastypes,curiyr) + OGADGPRD(I,oiltypes,curiyr)
    ELSE IF (I.EQ.70.OR.I.EQ.71.OR.I.EQ.72) THEN            ! To report GOM (state)
        OGPRDOFF(1,2,curiyr) = OGPRDOFF(1,2,CURIYR) + OGRNAGPRD(I,gastypes,curiyr) + OGADGPRD(I,oiltypes,curiyr)
    ENDIF
ENDDO

! COMPUTE BEGINING-OF-YEAR RESERVES FOR EACH REGION/FUEL TYPE
!   FOR FOLLOWING YEAR (T+1), SO RESBOYOFF IS REALLY
!   RESEOYOFF FOR THE CURRENT YEAR (T), FOLLOWING THIS EQUATION:
DO R=1,OFFRGN
    !WRITE (OFFOUT,*) 'exprdoff',curiyr+1989,ireg,EXPRDOFF(IREG,2),CURRESOFF(R,K) * PRRATOFF(R,K),OGPRODOFF(iREG,2)
    DO K=1,OFFFUEL
        RESBOYOFF(R,K) = CURRESOFF(R,K) - OGPRODOFF(R,K) + &
            RESADOFF(R,K)
        !IF (K.EQ.1) RESBOYOFF(R,K) = CURRESOFF(R,K)
        IF(RESBOYOFF(R,K).LT.0.0) RESBOYOFF(R,K) = 0.0
    ENDDO
ENDDO

! ...... PR ratios
DO R=1,OFFRGN
    DO K=1,OFFFUEL
        ! SAVE PREVIOUS YEARS P/R RATIO
        PRRATLAG = PRRATOFF(R,K)

        ! COMPUTE THE P/R RATIO FOR THE CURRENT YEAR
        IF (RESBOYOFF(R,K).GT.0.0) THEN
            IF (CURRESOFF(R,K).GT.0.0) THEN
                PRRATOFF(R,K) = ((exPRDOFF(R,K) * &
                    (1 -(OGPRODOFF(R,K)/CURRESOFF(R,K)))) + &
                    (PRRATNEW(r,k) * RESADOFF(R,K))) / &
                    (RESBOYOFF(R,K))
            ELSE
                PRRATOFF(R,K) = PRRATNEW(R,K)
                IF (R.EQ.1) PRRATOFF(R,K) = PRRATNEW(R,K)/2
            ENDIF

            !  SET ATLANTIC  P/R RATIOS
            !    TO BE THE SAME AS THE SHALLOW CENTRAL GULF P/R RATIO
            !PRRATOFF(1,K) = PRRATOFF(3,K)
            !  BIND P/R RATIOS
            !IF (CURIYR.GT.OFFBYR.AND.PRRATLAG.GT.0.0) THEN
            !    IF (k.eq.1.or.REPPRCOFF(R,K,CURIYR).GT. &
            !        REPPRCOFF(R,K,CURIYR-1)) THEN
            !    PRRATOFF(R,K) = AMIN1(PRRATLAG*1.100,PRRATOFF(R,K))
            !    PRRATOFF(R,K) = AMAX1(PRRATLAG*0.900,PRRATOFF(R,K))
            !    ELSE
            !        PRRATOFF(R,K) = PRRATLAG*1.00
            !    ENDIF
            !ENDIF

            PRRATOFF(R,K) = PRDOFF(R,K) / RESBOYOFF(R,K)

            !  FORCE P/R RATIOS TO BE 25% OR LESS
            !PRCAP = 0.25
            !IF (PRRATOFF(R,K) .GT. PRCAP) THEN
            !    PRRATOFF(R,K) = PRCAP
            !ENDIF

        ELSE
            PRRATOFF(R,K) = 0.0
        ENDIF

    ENDDO
ENDDO


! OVERWRITE BOY RESERVES WITH HISTORICAL VALUES
DO R=1,OFFRGN
    DO K=1,OFFFUEL
        IF ((OGRUNOP(3).EQ.1).AND.(CURIYR.EQ.OFFnHYR)) THEN
            ! Calculate average intial PR ratio
            RESSUM = 0.
            PRDSUM = 0.
            DO M=OFFnHYR-4,OFFnHYR
                RESSUM = RESSUM + HISTRESOFF(R,K,M)
                PRDSUM = PRDSUM + HISTPRROFF(R,K,M)*HISTRESOFF(R,K,M)
            ENDDO
            IF (RESSUM.GT.0.) PRRATAVG(R,k) = PRDSUM/RESSUM
            IF (PRRATAVG(R,K).LT.0.01) PRRATAVG(R,K)=0.1
        ENDIF
        !IF (CURIYR+1989.LE.2004.AND.R.EQ.3) PRRATOFF(R,K) = PRRATAVG(R,K)*0.95
        !IF (CURIYR+1989.EQ.2003.AND.K.EQ.2.AND.R.EQ.4) PRRATOFF(R,K) = MAX(PRRATOFF(R,K),0.11)
        !IF (CURIYR+1989.EQ.2004.AND.K.EQ.2.AND.R.EQ.4) PRRATOFF(R,K) = MAX(PRRATOFF(R,K),0.14)
        !IF (R.EQ.2) PRRATOFF(R,K) = HISTPRROFF(R,K,OFFnHYR)

        IF ((OGRUNOP(3).EQ.1).AND.(CURIYR.LT.OFFnHYR)) THEN
            RESBOYOFF(R,K) = HISTRESOFF(R,K,CURIYR+1)
        ENDIF
        IF ((OGRUNOP(3).EQ.1).AND.(CURIYR.LT.OFFnHYR)) THEN
            PRRATOFF(R,K) = HISTPRROFF(R,K,CURIYR+1)
        ENDIF

        ! ASSIGN VARIABLES THAT ARE USED FOR SUMMARY REPORTING
        REPPRDOFF(R,K,CURIYR) = OGPRODOFF(R,K)
        REPRADOFF(R,K,CURIYR) = RESADOFF(R,K)
        REPRSVOFF(R,K,CURIYR) = CURRESOFF(R,K)

    ENDDO
ENDDO

!  COMPUTE TOTAL OIL & GAS RESERVES IN GOM TO REPORT TO NEMS
DO K=1,OFFFUEL
    TOTRSVGOM(K) = 0.0  ! INITIALIZE TO 0
    totprdgom(k) = 0.0
    DO R=3,OFFRGN       ! PROCESS GOM REGIONS ONLY
        TOTRSVGOM(K) = TOTRSVGOM(K) + RESBOYOFF(R,K)
        totprdgom(k) = totprdgom(k) + prratoff(r,k)*resboyoff(r,k)
    ENDDO
    AGGPRROFF(K) = totprdgom(k) / totrsvgom(k)
ENDDO

DO R=1,OFFRGN
    DO K=1,OFFFUEL
        CURRESOFF(R,K) = RESBOYOFF(R,K)
        CURPRROFF(R,K) = PRRATOFF(R,K)
    ENDDO
ENDDO

! REPORT CRUDE VALUES TO NEMS
IF (curiyr.lt.IJUMPYR) THEN
    OGRESCO(L48RGN+1,CURIYR+1) = RESBOYOFF(1,1)
    OGRESCO(L48RGN+3,CURIYR+1) = RESBOYOFF(2,1)
    OGRESCO(L48RGN+2,CURIYR+1) = TOTRSVGOM(1)
    OGPRRCO(L48RGN+1,CURIYR+1) = PRRATOFF(1,1)
    OGPRRCO(L48RGN+3,CURIYR+1) = PRRATOFF(2,1)
    OGPRRCO(L48RGN+2,CURIYR+1) = AGGPRROFF(1)
    OGELSCO(L48RGN+1,CURIYR+1) = ELAST_OFF(1,1)
    OGELSCO(L48RGN+3,CURIYR+1) = ELAST_OFF(2,1)
    OGELSCO(L48RGN+2,CURIYR+1) = ELAST_OFF(3,1)

    ! REPORT GAS VALUES TO NEMS
    ! OGSM RGN 2 => NGTDM/PMM RGN 3;  OGSM RGN 3-4 => NGTDM/PMM RNG 2
    OGRESNGOF(1,CURIYR+1) = RESBOYOFF(1,2)
    OGRESNGOF(3,CURIYR+1) = RESBOYOFF(2,2)
    OGRESNGOF(2,CURIYR+1) = TOTRSVGOM(2)
    IF(ogresngof(1,curiyr).gt.0.) ogprrngof(1,curiyr) = ogprdngof(1,curiyr)/ogresngof(1,curiyr)
    IF(ogresngof(2,curiyr).gt.0.) ogprrngof(2,curiyr) = ogprdngof(2,curiyr)/ogresngof(2,curiyr)
    IF(ogresngof(3,curiyr).gt.0.) ogprrngof(3,curiyr) = ogprdngof(3,curiyr)/ogresngof(3,curiyr)
    !OGPRRNGOF(1,CURIYR+1) = PRRATOFF(1,2)
    !OGPRRNGOF(3,CURIYR+1) = PRRATOFF(2,2)
    !OGPRRNGOF(2,CURIYR+1) = AGGPRROFF(2)
    OGELSNGOF(1,CURIYR+1) = ELAST_OFF(1,2)
    OGELSNGOF(3,CURIYR+1) = ELAST_OFF(2,2)
    OGELSNGOF(2,CURIYR+1) = ELAST_OFF(3,2)
ENDIF
! REPORT OFFSHORE SHALLOW AND DEEP PRODUCTION
ADGSHR = 0.
IF(PROD_ASG(3,curiyr)+PROD_ASG(4,curiyr).GT.0.) ADGSHR = PROD_ASG(3,curiyr)/(PROD_ASG(3,curiyr)+PROD_ASG(4,curiyr))
OGCOPRDGOM(1,curiyr) = OGPRODOFF(3,1)/365.
OGCOPRDGOM(2,curiyr) = OGPRODOFF(4,1)/365.
!OGNGPRDGOM(1,curiyr) = (OGPRODOFF(3,2)+PROD_ASG(3,curiyr))*0.001
!OGNGPRDGOM(2,curiyr) = (OGPRODOFF(4,2)+PROD_ASG(4,curiyr))*0.001
!OGNGPRDGOM(1,curiyr) = (OGPRODOFF(3,2))*0.001
!OGNGPRDGOM(2,curiyr) = (OGPRODOFF(4,2))*0.001
OGNGPRDGOM(1,curiyr) = (OGPRODOFF(3,2)+sum(OGADGPRD(70:72,oiltypes,curiyr)))*0.001
OGNGPRDGOM(2,curiyr) = (OGPRODOFF(4,2)+sum(OGADGPRD(79:81,oiltypes,curiyr)))*0.001
!WRITE(6,*) 'dh5out-state',curiyr+1989,sum(OGRNAGPRD(70:72,gastypes,curiyr)),sum(OGADGPRD(70:72,gastypes,curiyr)),ogprdoff(1,2,curiyr)
!WRITE(6,*) 'dh5out-fed',curiyr+1989,sum(OGRNAGPRD(79:81,gastypes,curiyr)),sum(OGADGPRD(79:81,gastypes,curiyr))
!WRITE(6,*) 'dh5out-shallow',curiyr+1989,ogprodoff(3,2),sum(OGADGPRD(70:72,oiltypes,curiyr))
!WRITE(6,*) 'dh5out-deep',curiyr+1989,ogprodoff(4,2),sum(OGADGPRD(79:81,oiltypes,curiyr))
! REPORT WELL COUNTS TO NEMSA
sumwells1 = 0.
sumwells2 = 0.
DO i=1,2
    DO r=1,offrgn
        DO k=1,offfuel
            IF (R.EQ.1) THEN ! REGION IS OGSM ATLANTIC
                sumwells1(1,k) = sumwells1(1,k) + wellsoff(i,r,k)
                sumwells2(1,k) = sumwells2(1,k) + sucwelloff(i,r,k)
            ELSEIF (R.EQ.2) THEN  ! REGION IS OGSM PACIFIC
                sumwells1(2,k) = sumwells1(2,k) + wellsoff(i,r,k)
                sumwells2(2,k) = sumwells2(2,k) + sucwelloff(i,r,k)
            ELSE   ! REGION IS OGSM GOM
                sumwells1(3,k) = sumwells1(3,k) + wellsoff(i,r,k)
                sumwells2(3,k) = sumwells2(3,k) + sucwelloff(i,r,k)
            ENDIF
        ENDDO
    ENDDO
ENDDO
OGWELLSL48(L48RGN+1,1,curiyr) = sumwells1(1,1)
OGWELLSL48(L48RGN+1,3,curiyr) = sumwells1(1,2)
IF (sumwells1(1,1).gt.0) THEN
    OGSRL48(L48RGN+1,1,curiyr) = sumwells2(1,1)/sumwells1(1,1)
ELSE
    OGSRL48(L48RGN+1,1,curiyr) = 0.
ENDIF
IF (sumwells1(1,2).gt.0) THEN
    OGSRL48(L48RGN+1,3,curiyr) = sumwells2(1,2)/sumwells1(1,2)
ELSE
    OGSRL48(L48RGN+1,3,curiyr) = 0.
ENDIF
OGWELLSL48(L48RGN+3,1,curiyr) = sumwells1(2,1)
OGWELLSL48(L48RGN+3,3,curiyr) = sumwells1(2,2)
IF (sumwells1(2,1).gt.0) THEN
    OGSRL48(L48RGN+3,1,curiyr) = sumwells2(2,1)/sumwells1(2,1)
ELSE
    OGSRL48(L48RGN+3,1,curiyr) = 0.
ENDIF
IF (sumwells1(2,2).gt.0) THEN
    OGSRL48(L48RGN+3,3,curiyr) = sumwells2(2,2)/sumwells1(2,2)
ELSE
    OGSRL48(L48RGN+3,3,curiyr) = 0.
ENDIF
OGWELLSL48(L48RGN+2,1,curiyr) = sumwells1(3,1)
OGWELLSL48(L48RGN+2,3,curiyr) = sumwells1(3,2)
IF (sumwells1(3,1).gt.0) THEN
    OGSRL48(L48RGN+2,1,curiyr) = sumwells2(3,1)/sumwells1(3,1)
ELSE
    OGSRL48(L48RGN+2,1,curiyr) = 0.
ENDIF
IF (sumwells1(3,2).gt.0) THEN
    OGSRL48(L48RGN+2,3,curiyr) = sumwells2(3,2)/sumwells1(3,2)
ELSE
    OGSRL48(L48RGN+2,3,curiyr) = 0.
ENDIF

END SUBROUTINE OGOUT1_OFF

!************************************************************************************!
! Subroutine to determine possible exploration projects for each EU
! =.GT. ONLY 1 PROJECT FOR EACH TYPE
!************************************************************************************!
SUBROUTINE DetermineEconomicResources
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ncntrl'       ! nems control variables
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmoff'      ! offshore variables
INCLUDE 'dwpparm'

INTEGER i, f, iReg, iPA, iEU, iFSC, tAvl,iPF
INTEGER iFSCOpt(1:16)
INTEGER itmp, test
REAL*4 ECONRES(nPA), TECHRES(nPA)
LOGICAL PRINTPA(nPA)
TYPE (Projectobj) prj(2)

PRINTPA = .FALSE.
DO iPA = 1, nPA

    TECHRES(iPA) = 0.
    ECONRES(iPA) = 0.

    DO iEU = 1, nEU(iPA)

        tAvl =  yrAvl(iPA, iEU) -  yrBegForecast + 1

        IF (tAvl .LE. toff) THEN     ! ELSE GOTO SKIPEU

            PRINTPA(iPA) = .TRUE.

            ! Get region ID
            iReg =  PAREG(iPA)

            ! Start from the biggest FSC
            DO iFSC = minFSC+numFSC-1, minFSC, -1

                ! If field exists in this FSC
                IF  (uNRR(iPA, iEU, iFSC) .GT. 0) THEN

                    ! Check if this FSC is suitable for Option 1
                    !f = fldID(0, iPA, iEU, iFSC, 1)
                    DO itmp = 1,ntfld
                        IF (fldID2(itmp).eq.0.and. &
                            fldID3(itmp).eq.iPA.and. &
                            fldID4(itmp).eq.iEU.and. &
                            fldID5(itmp).eq.iFSC.and. &
                            fldID6(itmp).eq.1) THEN
                        f = fldID1(itmp)
                        EXIT
                        ENDIF
                    ENDDO

                    ! Get platform and non-platform PF type index
                    iPF = 0
                    I=1
                    DO WHILE (wdpth(iPA,iEU,iFSC)*0.3048.GT.PFWDMIN(I))
                        I=I+1
                        IF (I.GT.5) EXIT
                    ENDDO
                    IF (pftype(i-1,iFSC).eq.'PF') iPF = 1
                    IF (pftype(i-1,iFSC).eq.'CT') iPF = 2
                    IF (pftype(i-1,iFSC).eq.'TLP') iPF = 3
                    IF (pftype(i-1,iFSC).eq.'FPS') iPF = 4
                    IF (pftype(i-1,iFSC).eq.'SPAR') iPF = 5
                    IF (pftype(i-1,iFSC).eq.'FPS-SS') iPF = 7

                    IF (iPF .EQ. 0) WRITE(OFFOUT,*) &
                        'WARNING: Could not assign type of production facility for [PA, EU, FSC]:', iPA,iEU,iFSC

                    ! Pick the cheapest (expNPV .GT. 0) project
                    i = 0

                    CALL INITPROJECT(prj(1))

                    IF (iPF .GT. 0) THEN
                        prj(1)%Typ = 1
                        prj(1)%Reg = iReg
                        prj(1)%PA = iPA
                        prj(1)%EU = iEU
                        prj(1)%biggestFSC = iFSC
                        prj(1)%nFld(iFSC) = 1
                        prj(1)%pfTyp(1) = iPF
                        CALL CreateExplorationProject(prj(1), .false.)
                        IF (prj(1)%expNPV .GT. 0) i = 1
                    ENDIF

                    TECHRES(iPA) = TECHRES(iPA) + uNRR(iPA,iEU,iFSC)*VMEAN(iFSC)
                    IF (i.eq.1) ECONRES(iPA) = ECONRES(iPA) + uNRR(iPA,iEU,iFSC)*VMEAN(iFSC)

                ENDIF
            ENDDO

        ENDIF    ! SkipEU:
    ENDDO

    IF (iPA.eq.1) WRITE (OFFOUT,*) 'econres', '            REGION', '                   ECONOMICALLY', '   TECHNICALLY' ! DBG_Flag_Recov_By_Reg (1/3)
    IF (iPA.eq.1) WRITE (OFFOUT,*) 'econres', '                                     RECOVERABLE ', '   RECOVERABLE' ! DBG_Flag_Recov_By_Reg (1/3)
    IF (PRINTPA(iPA)) WRITE (OFFOUT,*) 'econres', yearof(toff), nmPA(iPA), econres(iPA), techres(iPA) ! DBG_Flag_Recov_By_Reg (1/3)

ENDDO

END SUBROUTINE DetermineEconomicResources

!************************************************************************************!
! Subroutine to output PROJECTOBJ members for debugging 02/27/2020 apr
!************************************************************************************!
SUBROUTINE printprj(xPrj, xPa, xEu, xFSC)
IMPLICIT NONE

INCLUDE 'parametr'     ! nems dimension parameters
INCLUDE 'ncntrl'       ! nems control variables
INCLUDE 'ogsmparm'     ! ogsm parameter file
INCLUDE 'ogsmbfw'
INCLUDE 'ogsmoff'
INCLUDE 'dwpparm'

TYPE (Projectobj), INTENT(IN) :: xPrj
INTEGER, INTENT(IN) :: xPa
INTEGER, INTENT(IN) :: xEu
INTEGER, INTENT(IN) :: xFSC

WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") 'project', achar(9)
WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") yearof(toff), achar(9)
WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") nmEU(xPa,xEu), achar(9)
WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") xFSC, achar(9)
WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") xPrj%expDrlCst(xFSC), achar(9)
WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") xPrj%delDrlCst(xFSC), achar(9)
WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") xPrj%expDrlNWls(xFSC), achar(9)
WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") xPrj%nFld(xFSC), achar(9)
WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") xPrj%expDryNWls(xFSC), achar(9)
WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") xPrj%delDrlNWls(xFSC), achar(9)
WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") xPrj%devWlsDrl(xFSC), achar(9)
WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") xPrj%pfTyp(1), achar(9)
WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") xPrj%pfNWlsArr(1), achar(9)
WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") xPrj%nFlowline(1), achar(9)
WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") xPrj%pfTimeAvl(1), achar(9)
WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") xPrj%expNPV, achar(9)
WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") xPrj%devNPV, achar(9)
WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") xPrj%PI, achar(9)
WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") xPrj%Typ, achar(9)
WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") xPrj%biggestFSC, achar(9)
WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") xPrj%yrExpStart, achar(9)
WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") xPrj%yrDevStart, achar(9)
WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") xPrj%yrPrdStart, achar(9)
WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") xPrj%nPf, achar(9)
WRITE(OFFOUT,'(*(G0.6,:,"'//achar(9)//'"))', advance="no") xPrj%pfNWls, achar(9)
WRITE(OFFOUT,*)

END SUBROUTINE printprj

!************************************************************************************!
! Function to convert a value to a string with format
!************************************************************************************!
!FUNCTION v2s(v As Variant, w As Integer, d As Integer) As String
!Dim f As String
!Dim s As String
!Dim l As Integer
!
!IF (v = "") THEN
!    v2s = ""
!    EXIT Function
!    ENDIF
!
!    IF (d .GT. (w - 2)) THEN d = w - 2
!        IF (d .GT. 0) THEN
!            f = String(w - d - 1 - 1, "#") + "0." + String(d, "#")
!        ELSE
!            f = String(w - 1, "#") + "0"
!        ENDIF
!        s = Format(v, f)
!        IF (d .GT. 0) THEN
!            l = d - (Len(s) - InStr(1, s, "."))  ! number of zeros to be added to the end
!            IF (l .GT. 0) THEN s = s + String(l, "0")
!            ENDIF
!            l = w - Len(s)  ! number of spaces to be added to the front
!            IF (l .GT. 0) THEN s = String(l, " ") + s
!                v2s = s
!            ENDIF
!        ENDIF
!    ENDIF
!ENDIF
!END FUNCTION v2s

!************************************************************************************!
! Subroutine to print a string at [r,w] with format
!************************************************************************************!
!SUBROUTINE printFormat(r As Integer, c As Integer, v As Variant, w As Integer, d As Integer, &
!    fSize As Integer, fFace As String)
!Cells(r, c)%Select
!IF (w = 0) THEN
!    Selection.Value = v
!ELSE
!    Selection.Value = v2s(v, w, d)
!ENDIF
!WITH Selection.Font
!    .size = fSize
!    IF (fFace = "B") THEN
!        .Bold = True
!    ElseIF (fFace = "I") THEN
!        .Italic = True
!    ElseIF (fFace = "BI") THEN
!        .Bold = True
!        .Italic = True
!    ENDIF
!END WITH
!END SUBROUTINE printFormat