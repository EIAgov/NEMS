! $Header: M:/default/source/RCS/ftab.f,v 1.907 2021/05/20 15:49:10 NSK Exp $

       PROGRAM FTAB
       IMPLICIT NONE

! PROGRAM TO GENERATE NEMS OUTPUT TABLES.

! SUBROUTINE CALLING OUTLINE (OR FUNCTIONS):
!   FINIT      INITIALIZES THE TABLE ARRAYS IN /FTABLE/
!   ZEROR      ZEROES A REAL ARRAY PASSED AS AN ADJUSTABLE ARRAY
!   FREADF     READS THE TABLE LAYOUT FILE AND SETS UP FORMATTING OPTIONS
!   FDATA1     READS NEMS DATA AND FILLS TABLE ARRAYS BY YEAR, Tables 1-59
!   FDATA2     READS NEMS DATA AND FILLS TABLE ARRAYS BY YEAR, Tables 60-99
!   FDATA3     READS NEMS DATA AND FILLS TABLE ARRAYS BY YEAR, Tables 100-150
!       This doesn't count table 120, which has it's own subroutine (FDATA_IEA)
!            as it is in a different format than the others.
!       Their subroutines are:   FDATA_IEA
!   FINFLATE   MULTIPLIES PRICES BY INFLATION FACTOR
!   FSCALE     CONVERTS Q FROM TRILLS TO QUADS AND P FROM $82 TO $88
!   FCOPY(I,R) COPIES TABLE ARRAY I (REGION R) TO GENERIC TABLE ARRAY TABN
!   FWRITF(I)  WRITES OUT TABLE I USING ARRAY TABN AND /FTABLE/ LAYOUT SPECS

! INCLUDES
!     FTABLE   (USED BY FTAB ONLY)
!     PQ
!     NCNTRL
!     and all the rest of the includes in the Global Data Structure

! INPUT
!   UNIT 5 (ftab.dat)
!   TABLE LAYOUT FILE (table stubs and footnotes)
!   TABLE REQUEST FILE (which tables to print)
!   NEMS DATABASE (RESTART file)
!   citation (footnote citations)
!   ftabxml.txt  a template for excel xml output
! OUTPUT
!   UNIT 6
!   UNIT 20 ("fort.20") OUTPUT FILE FOR PRINTING WITH CARRIAGE CONTROL
!   You will need to set switches correctly in ftab.dat to get these:
!     UNIT 21 Sometimes known as RAN file, or Ladyfile.  AKA GRAF2000 input file.
!     UNIT 30 Lotus 1-2-3 spreadsheet containing output tables
!****************************************************************************
      include 'parametr'
      include 'ftable'
      include 'ncntrl'
      include 'emmparm'
      include 'uefdout'

      INTEGER ISKIPO,ISKIPN,IPRNTYRS(36),IGROWO,ISKIPY,IGRWYR,LLYR,IYR,IYSTRT,IS,IL,L,ILSTYR, &
           LRECL,I1,I2,ITAB,IF,IC,IN,IC2,IN2,IOPT,IR,IRW,IUNIT, &
           I,IROW,LYR,NUMITEMS,NUMCOLS,ICHAR,INDEX,LEN,III,RR
      INTEGER*4 FFUNFMT(MAXS)  ! RESTART FORMATS. =1 IF UNFORMATTED, =0 FORMATED
      CHARACTER COLFMT*10,BLNKFILE*50
      CHARACTER*4 CHAR4YEAR
      CHARACTER*80 SKIPLINE
      CHARACTER comma*1/','/
      character*65 ladyname ! name of lady's graf2000 interface file (output)
      LOGICAL NOROOM
! graf2000 interface file variables
      integer*2 zero2/0/,ie,ii
      integer*2 rectype,gtablenum,ftablenum,region,headnum,rownum,ladyrec
      integer*2 startsat(500)
      common/lady/rectype,gtablenum,ftablenum,region,headnum,rownum,startsat,ladyrec

!Additional variables for newer version of the RAN file.
      integer*4 RXPtr,RTPtr,RDPtr,RGPtr
      common/NewRan/RXPtr,RTPtr,RDPtr,RGPtr
      character*4 RMVer,RSVer
      integer*4 RMLen,RSLen,RTLen,RDLen
      integer*2 RSFLen,RSSLen
      character*200 RSFile,RSScen
!Additional variables for the alternative command line argument for opening unit 5.
      character*200 CmdArg
      integer CmdLen
      CHARACTER*20 t21_units(4)


      REGNAM(1:11)=(/'New England - 01', &
         'Middle Atlantic - 02', &
         'East North Central - 03', &
         'West North Central - 04', &
         'South Atlantic - 05', &
         'East South Central - 06', &
         'West South Central - 07', &
         'Mountain - 08', &
         'Pacific - 09', &
         'California - 10', &
         'United States - 11'/)



      FAKENAM(1:4)=(/'Volume Balance (billion cubic feet)', &
                     'Value (billion #### dollars)', &
                     'Mass Balance (billion tons)', &
                     'Energy Balance (quadrillion Btu)'/)

      t21_units(1:4)=(/'volume:  bcf', &
                       'value:  bil #### dol', &
                       'mass:  bil tons', &
                       'energy:  quad Btu'/)



! CALL ROUTINE TO INITIALIZE TABLE ARRAYS
      CALL FINIT

! SET DEFAULTS:
      NSCEN=1         ! Number of scenarios to show in tables
      MAXCOL=21       ! Maximum number of columns that can fit on 1 page, given ICOLWD
      ICOLWD=6        ! Column width (not to exceed MCOLWD parameter in FTABLE)
      COMENT=' '      ! Run comment, printed on each page
      ISKIPO=1        ! Option to skip to 5-year intervals
      IGROWO=1        ! Option to print average annual growth rate
      ISKIPY=2005     ! Year to begin skipping if ISKIP0 = 1
      FRSCEN=' '      ! Run scenario
      IPREGD=1        ! Option to print regions for those tables so dimensioned
      IPRFTN=0        ! Option to print footnotes
      IFSTYR=1        ! First year to print
      ILSTYR=21       ! Last year to print
      IGRWYR=6        ! Base year for average annual growth rate calculation
      FYRPRC=7        ! Year dollars for prices
      FDATE=' '       ! Run datekey
      LADYFILE=0      ! Option to create GRAF2000 file
      CUMCAPADD=0     !  set these two to 0 initially, as there is corrective action
      CUMOILGAS=0     !  after they are read, if they are still 0
      CARBON_OR_2='CO2' ! Emissions option:  c (carbon) or co2 (carbon dioxide) equivalent
      UT_DR=0.08      ! Discount Rate for Electric Utility NPV of Resource Costs
      ftabbone=1      ! option to print "bonus" (non-published) rows for debugging

!This checks to see if FTab was run with a command line argument. If it does have a
!command line argument then that is the file name to use to open unit 5. Otherwise,
!it is assumed that unit 5 is piped directly to the program.
      call getarg(1,CmdArg)
      CmdLen=len_trim(CmdArg)
      if(CmdLen.gt.0) open(unit=5,file=CmdArg(1:CmdLen),status='old',action='read',share='denynone')

      READ(5,'(A)',END=15) FRSCEN
      READ(5,'(A)',END=15) FDATE
      READ(5,'(A160)',END=15) COMENT
      READ(5,'(BN,I4)',END=15) MAXCOL
      READ(5,'(BN,I4)',END=15) ICOLWD
      IF(ICOLWD.GT.MCOLWD) ICOLWD=MCOLWD
      READ(5,*,END=15) IPREGD
      READ(5,*,END=15) IPRFTN
      READ(5,*,END=15) IFSTYR
      READ(5,*,END=15) ILSTYR
      READ(5,*,END=15) IGROWO,IGRWYR
      READ(5,'(A80)',END=15) SKIPLINE
      READ(SKIPLINE,*,END=15) ISKIPO
      IF (ISKIPO .EQ. 99) THEN
         WRITE(6,'(" Encountered Agent 99")')
         READ(SKIPLINE,*,END=15) ISKIPO,ISKIPN
         IF (ISKIPN .GT. 36) ISKIPN=36
         READ(SKIPLINE,*,END=15) ISKIPO,ISKIPN,(IPRNTYRS(III),III=1,ISKIPN)
         WRITE(6,'(38I5)') ISKIPO,ISKIPN,(IPRNTYRS(III),III=1,ISKIPN)
      ELSE
         READ(SKIPLINE,*,END=15) ISKIPO,ISKIPY
      ENDIF
      READ(5,*,END=15) NSCEN
      IF(NSCEN.GT.MAXS) NSCEN=MAXS
      READ(5,*,END=15) FYRPRC
      READ(5,*,END=15) ICRWK1
      READ(5,*,END=15) (FFUNFMT(I),I=1,NSCEN)
      READ(5,'(A)',END=15) TABREQ
      READ(5,'(A)',END=15) DICTNAME
      READ(5,'(A)',END=15) LAYOUT
      READ(5,'(A)',END=15) RESTART
      READ(5,'(A)',END=15) RESTART2
      READ(5,'(A)',END=15) RESTART3
      READ(5,'(A)',END=15) RESTART4
      READ(5,'(A)',END=15) RESTART5
      READ(5,'(A)',END=15) RESTART6
      READ(5,'(A)',END=15) RESTART7
      READ(5,'(A)',END=15) FTABXML  ! xml template input file
      READ(5,'(A)',END=15) CITATION
      READ(5,*,END=15) LADYFILE     ! Switch to create .RAN file for grafnem (set to 0 to not create)
      READ(5,*,END=15) CUMCAPADD
      READ(5,*,END=15) CUMOILGAS
      READ(5,*,END=15) CARBON_OR_2
      IF (CARBON_OR_2 .EQ. 'co2') THEN
          CARBON_OR_2='CO2'
      ELSE
          CARBON_OR_2='C  '
      ENDIF
      read(5,*,end=15) ftabbone
      READ(5,*,END=15) UT_DR

      if(CmdLen.gt.0) close(5)

! CALL ROUTINE TO READ IN TABLE LAYOUT FILE (stubs, etc.)
15    CALL FREADF

!        --- IF COMPARE 7 CASES SET THESE LIMITS AS REQUIRED
         IF ((NSCEN.GT.6).AND.((ILSTYR-IFSTYR).GT.20) .AND. ISKIPO .NE. 99) THEN
            WRITE(6,*) ' ***** Note:   *******************************'
            WRITE(6,*) ' Resetting report variables requested.'
            WRITE(6,*) ' Need to request less for 7-case comparison.'
            WRITE(6,*) ' *********************************************'
            ISKIPO=1
            IGROWO=0
            ISKIPY=2005
            IFSTYR=2005
            ILSTYR=2020
          ENDIF
! CONVERT IFSTYR TO NEMS YEAR INDEX IF INPUT AS CALENDAR YEAR
      IF(IFSTYR.GT.1900) THEN
        IFSTYR=IFSTYR-1989
      ELSEIF(IFSTYR.GT.89) THEN
        IFSTYR=IFSTYR-89
      ENDIF

! CONVERT ILSTYR TO NEMS YEAR INDEX IF INPUT AS CALENDAR YEAR
      IF(ILSTYR.GT.1900) THEN
        ILSTYR=ILSTYR-1989
      ELSEIF(ILSTYR.GT.89) THEN
        ILSTYR=ILSTYR-89
      ENDIF
! CONVERT IGRWYR TO NEMS YEAR INDEX IF INPUT AS CALENDAR YEAR
      IF(IGROWO.EQ.1) THEN
        IF(IGRWYR.GT.1900) THEN
          IGRWYR=IGRWYR-1989
        ELSEIF(IGRWYR.GT.89) THEN
          IGRWYR=IGRWYR-89
        ENDIF
      ENDIF
! Convert FYRPRC to year value if needed
      IF (FYRPRC .LT. 50) THEN
          FYRPRC=FYRPRC+1989
        ELSEIF (FYRPRC .LT. 1950) THEN
          FYRPRC=FYRPRC+1900
      ENDIF
      IF (CUMCAPADD .EQ. 0) CUMCAPADD = FYRPRC
      IF (CUMOILGAS .EQ. 0) CUMOILGAS = IGRWYR + 1989
      WRITE(CHAR4YEAR,'(I4)') FYRPRC
      DO IR=1,4
         CALL REPLACE(FAKENAM(IR),'####',CHAR4YEAR)
         CALL REPLACE(t21_units(IR),'####',CHAR4YEAR)
      ENDDO
! CONVERT ISKIPY TO NEMS YEAR INDEX IF INPUT AS CALENDAR YEAR
      IF(ISKIPO.EQ.1) THEN
        IF(ISKIPY.GT.1900) THEN
          ISKIPY=ISKIPY-1989
        ELSEIF(ISKIPY.GT.89)THEN
          ISKIPY=ISKIPY-89
        ENDIF
      ENDIF
      IF (ISKIPY.LE.0) ISKIPY = IFSTYR
!     --- RESET SOME ITEMS IF COLUMNS REQUESTED > MAXCOL & NSCEN > 1
      IF (NSCEN.GT.1) THEN
         NUMITEMS = 0
!        --- FIND NUMBER OF YEARS REQUESTED
         IF (ISKIPO.EQ.1) THEN
            DO I = 0,(MAXYR-1)/5
               IF ((ISKIPY+I*5).LE.ILSTYR) NUMITEMS = NUMITEMS + 1
            ENDDO
            IF (MOD(ISKIPY,5) .NE. 1) NUMITEMS = NUMITEMS + 1
            NUMITEMS = NUMITEMS + ISKIPY - IFSTYR
            IF (NSCEN .GT. 4) NUMITEMS = NUMITEMS - 1    ! NOT PRINTING 2015
         ELSE
            NUMITEMS = ILSTYR - IFSTYR + 1
         ENDIF
!        --- NUMBER OF YEARS + GROWTH = NUMBER OF ITEMS
         IF (IGROWO.EQ.1) NUMITEMS = NUMITEMS + 1
!        --- NUMBER OF ITEMS * NUMBER OF SCENARIOS = NUMBER OF COLUMNS REQUESTED
         NUMCOLS = NUMITEMS * NSCEN
!        --- IF NUMBER OF COLUMNS REQUESTED > MAXIMUM COLUMNS, ADJUST
         IF (NUMCOLS.GT.MAXCOL) THEN
            NOROOM = .TRUE.
         ELSE
            NOROOM = .FALSE.
         ENDIF
         IF (ISKIPO .EQ. 99) NOROOM = .FALSE.
         DO WHILE (NOROOM)
!           --- IF NUMBER OF COUMNS REQUESTED > MAXIMUM COLUMNS, ADJUST
            IF (NUMCOLS.GT.MAXCOL) THEN
!              --- TURN OFF GROWTH OPTION
               IF (((NUMCOLS-NSCEN).LE.MAXCOL).AND.(IGROWO.EQ.1)) THEN
                  IGROWO = 0
                  NOROOM = .FALSE.
               ELSE
                  IGROWO = 0
                  IF (ISKIPO .EQ. 0) THEN
                    ISKIPO=1                 ! TURN ON SKIP
                  ELSE                       ! OR
                    IF (ISKIPY .LE. IFSTYR) THEN  !  IF CANNOT SKIP EARLIER
                       IFSTYR=IFSTYR+1       ! START MOVING START YEAR UP
                       ISKIPY=ISKIPY+1       ! MOVE SKIP YEAR UP, TOO
                    ELSE                     ! OR
                      IF (MOD(ISKIPY,5) .EQ. 1) ISKIPY = ISKIPY - 1
                      ISKIPY = ISKIPY - 1    ! SKIP 1 YEAR EARLIER (or 2)
                    ENDIF
                  ENDIF
                  NUMITEMS=0
                  DO I = 0,(MAXYR-1)/5
                     IF ((ISKIPY+I*5).LE.ILSTYR) NUMITEMS = NUMITEMS + 1
                  ENDDO
                  IF (MOD(ISKIPY,5) .NE. 1) NUMITEMS = NUMITEMS + 1
                  NUMITEMS = NUMITEMS + ISKIPY - IFSTYR
                  IF (NSCEN .GT. 4) NUMITEMS = NUMITEMS - 1    ! NOT PRINTING 2015
                  NUMCOLS = NUMITEMS * NSCEN
               ENDIF
            ELSE
!              --- EXIT DO LOOP, REQUESTED COLUMNS NOW < MAXIMUM COLUMNS
               NOROOM = .FALSE.
            ENDIF
         ENDDO
      ENDIF
      IF(ICOLWD.LT.5) ICOLWD=6
      IF(ICOLWD.GT.MCOLWD) ICOLWD=MCOLWD
3     format(3x,5a)
4     format(3x,a,i4,a)
5     format(3x,a,F6.3)
      write(6,*) ' '
      WRITE(6,3) '                     FTAB Options'
      WRITE(6,3) '======================================================'
      WRITE(6,3) 'Nems Run................ ',trim(FRSCEN)//'.'//trim(FDATE)
      WRITE(6,3) 'Comment................. ',trim(COMENT)
      WRITE(6,4) 'Max Columns per Page.... ',MAXCOL
      WRITE(6,4) 'Report Column Width..... ',ICOLWD
      WRITE(6,4) 'Print Regions?(1=Yes)... ',IPREGD
      WRITE(6,4) 'Use Footnotes?(1=Yes)... ',IPRFTN
      WRITE(6,4) '1st Print Year (index).. ',IFSTYR
      WRITE(6,4) 'Last Print Year (index). ',ILSTYR
      WRITE(6,4) 'Growth Rates? (1=Yes)... ',IGROWO
      WRITE(6,4) '1st Yr of Growth Rates.. ',IGRWYR
      WRITE(6,4) '5 Year Option (1=Yes)... ',ISKIPO
      WRITE(6,4) '5-Year Start Yr Index... ',ISKIPY
      WRITE(6,4) 'Number of scenarios..... ',NSCEN
      WRITE(6,4) 'Year for Real $ Prices.. ',FYRPRC
      WRITE(6,3) 'Table Request fILE...... ',TRIM(TABREQ)
      WRITE(6,3) 'Restart File Dictionary. ',TRIM(DICTNAME)
      WRITE(6,3) 'Layout File............. ',TRIM(LAYOUT)
      WRITE(6,3) 'NEMS Restart File....... ',TRIM(RESTART)
      WRITE(6,3) '2nd Restart File........ ',TRIM(RESTART2)
      WRITE(6,3) '3rd Restart File........ ',TRIM(RESTART3)
      WRITE(6,3) '4th Restart File........ ',TRIM(RESTART4)
      WRITE(6,3) '5th Restart File........ ',TRIM(RESTART5)
      WRITE(6,3) '6th Restart File........ ',TRIM(RESTART6)
      WRITE(6,3) '7th Restart File........ ',TRIM(RESTART7)
      WRITE(6,3) 'Excel XML Template...... ',TRIM(FTABXML)
      WRITE(6,3) 'Footnote Sources........ ',trim(CITATION)
      WRITE(6,4) 'GRAF2000 (0=no file).... ',LADYFILE
      WRITE(6,4) 'Cumul. Capacity as of... ',CUMCAPADD
      WRITE(6,4) 'Cumul. Production as of. ',CUMOILGAS
      WRITE(6,3) 'Emissions printed as.... ',TRIM(CARBON_OR_2)
      WRITE(6,5) 'Resource Cost Discount.. ',UT_DR
      write(6,4) 'Bonus rows? (1=Yes,0=No).',ftabbone
      WRITE(6,3) '======================================================'
      write(6,*) ' '
      LLYR=ILSTYR
      LYR=ILSTYR
      IF(ISKIPO.EQ.1) LLYR=ISKIPY-1
      IF(ISKIPO.EQ.1.AND.ISKIPY.EQ.1) LLYR=ISKIPY
      NCOLS=0
! SET FORMAT FOR YEARS DEPENDING ON COLUMN WIDTH
      IF(ICOLWD.LE.7) THEN
        COLFMT='(A,I4)'
      ELSEIF(ICOLWD.LE.9)THEN
        COLFMT='(A,I6)'
      ELSE
        COLFMT='(A,I7)'
      ENDIF

      DO 7 IYR=IFSTYR,LLYR
        IF(NSCEN.EQ.1.OR.IYR.EQ.1) THEN
          NCOLS=NCOLS+1
          ICOLS(NCOLS)=IYR
          ISCEN(NCOLS)=NSCEN
          WRITE(COLLAB(NCOLS),FMT=COLFMT) '|',IYR+1989
        ENDIF
7     CONTINUE
      IF (ISKIPO .EQ. 99) THEN
         NCOLS=0
         DO III=1,ISKIPN
            DO IS=1,NSCEN
               IYR = IPRNTYRS(III) - 1989
               NCOLS=NCOLS+1
               ICOLS(NCOLS)=IYR
               ISCEN(NCOLS)=IS
               IF(IS.EQ.1) THEN
                  WRITE(COLLAB(NCOLS),FMT=COLFMT) '|',IYR+1989
               ELSE
                  WRITE(COLLAB(NCOLS),FMT=COLFMT) ' ',IYR+1989
               ENDIF
           END DO
        END DO
      ELSE IF(ISKIPO.EQ.1.OR.NCOLS.EQ.0.OR.NSCEN.GT.1) THEN
         IF (NCOLS.EQ.0.AND.ISKIPO.EQ.0) THEN
           ISKIPO = 1        ! DID THIS TO GET A VALUE FOR NCOLS
           ISKIPY = LLYR     ! FOR JOBS WITH MORE THAN 1 SCENARIO
           LLYR = ISKIPY - 1 ! THAT DO NOT START AT YEAR 1
         ENDIF
         IYSTRT=ISKIPY
         IF(NSCEN.GT.1) IYSTRT=MIN(2,ISKIPY)
         IF(IFSTYR.GT.IYSTRT) IYSTRT = IFSTYR  !ALLOWS STARTING AFTER 91
         DO 8 IYR=IYSTRT,LYR
         DO 8 IS=1,NSCEN
           IF (((MOD(IYR-1,5) .EQ. 0 .AND. (NSCEN .LE. 4 .OR. &
             (NSCEN .GT. 4 .AND. IYR+1989 .NE. 2015))) .OR. &
             IYR .EQ. IYSTRT .OR. IYR .LE. ISKIPY) .AND. &
             IYR .NE. 1) THEN
             NCOLS=NCOLS+1
             ICOLS(NCOLS)=IYR
             ISCEN(NCOLS)=IS
             IF(IS.EQ.1) THEN
               WRITE(COLLAB(NCOLS),FMT=COLFMT) '|',IYR+1989
             ELSE
               WRITE(COLLAB(NCOLS),FMT=COLFMT) ' ',IYR+1989
             ENDIF
           ENDIF
8       CONTINUE
      ENDIF
! FOR GROWTH RATE OR ROW SUM, USE LAST COLUMN POSITION (MAXYR=31+1=32)
      IF(IGROWO.EQ.1) THEN
        DO 86 IS=1,NSCEN
          NCOLS=NCOLS+1
          ICOLS(NCOLS)=MAXYR
          ISCEN(NCOLS)=IS
          WRITE(COLLAB(NCOLS),FMT=COLFMT) '|',LYR+1989
          WRITE(COLLB1,FMT=COLFMT) '|',IGRWYR+1989
          IL=LEN(COLLB1)
          L=LEN_TRIM(COLLB1(:IL))
          IF(L.LT.MCOLWD) COLLB1(L+1:L+1)='-'
86      CONTINUE
      ENDIF
      LRECL=36+ICOLWD*MIN(MAXCOL,NCOLS)
      write(6,4) 'The ftab output file, fort.20, can be printed with the'
      write(6,4) 'NEMS "nprint" korn shell command.'


! REMOVE FOOTNOTE REFERENCES FROM ROW LABELS IF NO FOOT NOTES WILL BE WRITTEN
!     IF (IPRFTN .NE. 1) THEN
!       I1=ICHAR('0')
!       I2=ICHAR('9')
!       DO 9 ITAB=1,NTABLE
!         DO 9 IF=1,NROWS(ITAB)
!  SEARCH FOR SLASH USING THE STRING INDEX FUNCTION.
!           IC=INDEX(ROWLAB(IF,ITAB),'/')
!           IF (IC.GT.3) THEN
!  CHECK FOR NUMBER INDICATING FOOTNOTE. IF NONE, LOOK FOR SECOND
!  SLASH, ELSE REMOVE SLASH AND FOOTNOTE NUMBER AND REPLACE WITH .
!             IN=ICHAR(ROWLAB(IF,ITAB)(IC-1:IC-1))
!             IF(IN.GE.I1.AND.IN.LE.I2) THEN
!               ROWLAB(IF,ITAB)(IC:IC)='.'
!               ROWLAB(IF,ITAB)(IC-1:IC-1)='.'
!               IN=ICHAR(ROWLAB(IF,ITAB)(IC-2:IC-2))
!               IF(IN.GE.I1.AND.IN.LE.I2) &
!                 ROWLAB(IF,ITAB)(IC-2:IC-2)='.'
!             ELSE
!               IC2=IC+INDEX(ROWLAB(IF,ITAB)(IC+1:),'/')
!               IN2=ICHAR(ROWLAB(IF,ITAB)(IC2-1:IC2-1))
!               IF (IC2.GT.IC.AND.IN2.GE.I1.AND.IN2.LE.I2) THEN
!                 ROWLAB(IF,ITAB)(IC2:IC2) = '.'
!                 ROWLAB(IF,ITAB)(IC2-1:IC2-1) = '.'
!                 IN2=ICHAR(ROWLAB(IF,ITAB)(IC2-2:IC2-2))
!                 IF(IN2.GE.I1.AND.IN2.LE.I2) &
!                   ROWLAB(IF,ITAB)(IC2-2:IC2-2)='.'
!               ENDIF
!               IC=IC2
!             ENDIF
!           ENDIF
!9       CONTINUE
!     ENDIF
!     --- CALL ROUTINE TO READ NEMS DATA AND FILL REPORT ARRAY TABLES BY YEAR
      CALL FDATA1(FFUNFMT)

! CALL ROUTINES TO COPY THE TABLE DATA (FCOPY) AND PRINT THE TABLES (FWRITF).
! ROUTINE FOVER IMPLEMENTS OPTIONAL HISTORICAL DATA OVERWRITES FROM 85 ON
! USING VALUES READ FROM THE TABLE LAYOUT FILE; NATIONAL TABLES ONLY.
! ROUTINE FGROW COMPUTES EITHER A GROWTH RATE OR A ROW SUM IN THE LAST COLUMN.

      if (LadyFile .ne. 0) then
        ladyname=trim(frscen)//'.'//fdate(2:5)//trim(fdate(8:9))//'.RAN'
        open(unit=21,file=LadyName,access='direct',form='binary',recl=1,buffered='yes')
        write(6,*) 'Graf2000++ file '//trim(ladyname)//' opened'

      !Write the initial header containing the version, header length, scenario header length,
      !and table location header length.
        RMVer='001B'  ! RMVer changed from 001A to 001B with addition of 8 row description strings on data rows
        RMLen=100
        RSLen=400
        RTLen=8000
        RDLen=750000
        RXPtr=1
        write(21,rec=RXPtr) RMVer,RMLen,RSLen,RTLen,RDLen

        !Write a scenario header.
        RSVer='AA01'
        RSFLen=len_trim(LadyName)
        RSFile(1:RSFLen)=LadyName(1:RSFLen)
        RSSLen=len_trim(restart)
        RSScen(1:RSSLen)=restart(1:RSSLen)
        RXPtr=RXPtr+RMLen
        write(21,rec=RXPtr) RSVer,RSFLen,RSFile(1:RSFLen),RSSLen,RSScen(1:RSSLen)
        RXPtr=1+RMLen+RSLen
        RTPtr=1+RMLen+RSLen+2
        RDPtr=1+RMLen+RSLen+RTLen
        RGPtr=1+RMLen+RSLen+RTLen+RDLen
      endif

      CALL SETREPORTSWITCH
      FIRSTWK = .TRUE.    ! LOGICAL FOR WK ROUTINES, 1st PAGE OF WK1
      DO 10 III=1,NTABLE
!      --- PUT IN ORDER AS SPECIFIED IN LAYOUT
       ITAB = ORDER(III)
       IF (REPORT_SWITCH(ITAB) .or. LADYFILE .ge. 1) THEN
        IF (NHED(ITAB) .GT. 0 .OR. NROWS(ITAB) .GT. 0) THEN
          DO 11 I=1,NSCEN
!           --- SET NATIONAL TOTAL TO REGION MNUMNR, NOT MNUMCR FOR EMM Region tables
            IF (ITAB .EQ.  59 .OR. ITAB .EQ.  61 .OR. ITAB .EQ.  62 .OR. ITAB .EQ.  67 .OR. &
                ITAB .EQ. 116 .OR. ITAB .EQ. 122 .OR. ITAB .EQ. 123) THEN
               CALL FCOPY(ITAB,MNUMNR,I)
            ELSE IF (ITAB .EQ. 21) THEN       ! four different versions of this table
               CALL FCOPY(ITAB, 1,I)          ! version 1 is primary version
            ELSE
               CALL FCOPY(ITAB,MNUMCR,I)
            ENDIF
            IF (IGROWO .EQ. 1) THEN
              IOPT=1
! The following IF statements set a flag (IOPT) to remove the growth rate from rows it is not relevant on
              IF (ITAB .EQ. 18)  IOPT=3
              IF (ITAB .EQ. 11)  IOPT=4
              IF (ITAB .EQ. 13 .OR. ITAB .EQ. 15)  IOPT=5
              IF (ITAB .EQ. 19)  IOPT=6
              IF (ITAB .EQ. 1)   IOPT=7
              IF (ITAB .EQ. 62)  IOPT=8
              IF (ITAB .EQ. 146) IOPT=10
              CALL FGROW(IOPT,IGRWYR,LYR,I)
            ENDIF
11        CONTINUE
          CALL FWRITF(ITAB,IGROWO,IGRWYR,0,IUNIT,IRW)
        ENDIF
       ENDIF              !IF REPORT_SWITCH
10    CONTINUE
      IF (IPREGD .EQ. 1 .or. LADYFILE .ne. 0) THEN
! WRITE REGIONAL VERSIONS OF TABLES 2, 3, 17, 21, 34, 48, 59, 61, 62, 67, 70, 93, 102, 109, 116, 119, 122, 123, 125.
!   INCREMENT NUMBER OF HEADER LINES TO INCLUDE REGION
        NHED(2)=NHED(2)+1
        NHED(3)=NHED(3)+1
        NHED(17)=NHED(17)+1
        NHED(21)=NHED(21)+1
        NHED(34)=NHED(34)+1
        NHED(48)=NHED(48)+1
        NHED(59)=NHED(59)+1
        NHED(61)=NHED(61)+1
        NHED(62)=NHED(62)+1
        NHED(67)=NHED(67)+1
        NHED(70)=NHED(70)+1
        NHED(93)=NHED(93)+1
        NHED(102)=NHED(102)+1
!       NHED(109)=NHED(109)+1
        NHED(116)=NHED(116)+1
        NHED(119)=NHED(119)+1
        NHED(122)=NHED(122)+1
        NHED(123)=NHED(123)+1
        NHED(125)=NHED(125)+1
!   INCREASE DECIMAL POINTS ON TABLE 2
        TABFMT(2)=TABFMT(2)+1
        DO 16 IROW=1,NROWS(2)
          IF(ROWFMT(IROW,2).GT.0) ROWFMT(IROW,2)=ROWFMT(IROW,2)+1
16      CONTINUE
! table 2
        III=2
        IF (REPORT_SWITCH(III) .or. LADYFILE .ne. 0) THEN
           DO IR=1,9
              TABHED(NHED(III),III)=REGNAM(IR)
              DO I=1,NSCEN
                 CALL FCOPY(III,IR,I)
                 IF(IGROWO.EQ.1) CALL FGROW(1,IGRWYR,LYR,I)
              ENDDO
              CALL FWRITF(III,IGROWO,IGRWYR,IR,IUNIT,IRW)
           END DO
        ENDIF
! table 3
        III=3
        IF (REPORT_SWITCH(III) .or. LADYFILE .ne. 0) THEN
           DO IR=1,9
              TABHED(NHED(III),III)=REGNAM(IR)
              DO I=1,NSCEN
                 CALL FCOPY(III,IR,I)
                 IF(IGROWO.EQ.1) CALL FGROW(1,IGRWYR,LYR,I)
              ENDDO
              CALL FWRITF(III,IGROWO,IGRWYR,IR,IUNIT,IRW)
           END DO
        ENDIF
! table 17
        III=17
        IF (REPORT_SWITCH(III) .or. LADYFILE .ne. 0) THEN
           DO IR=1,9
              TABHED(NHED(III),III)=REGNAM(IR)
              DO I=1,NSCEN
                 CALL FCOPY(III,IR,I)
                 IF(IGROWO.EQ.1) CALL FGROW(1,IGRWYR,LYR,I)
              ENDDO
              CALL FWRITF(III,IGROWO,IGRWYR,IR,IUNIT,IRW)
           END DO
        ENDIF
! table 21, fake regions
        III=21
        IF (REPORT_SWITCH(III) .or. LADYFILE .ne. 0) THEN
           DO IR=2,4                                 ! versions 2, 3, and 4 are alternate unit versions
              TABHED(NHED(III),III)=FAKENAM(IR)
              DO RR=1,NROWS(III)
                if (IROWS(RR,III) .ne. 0) graph_units(rr,III)=t21_units(ir)
              ENDDO
              DO I=1,NSCEN
                 CALL FCOPY(III,IR,I)
                 IF(IGROWO.EQ.1) CALL FGROW(1,IGRWYR,LYR,I)
              ENDDO
              CALL FWRITF(III,IGROWO,IGRWYR,IR,IUNIT,IRW)
           END DO
        ENDIF
! table 34
        III=34
        IF (REPORT_SWITCH(III) .or. LADYFILE .ne. 0) THEN
           DO IR=1,9
              TABHED(NHED(III),III)=REGNAM(IR)
              DO I=1,NSCEN
                 CALL FCOPY(III,IR,I)
                 IF(IGROWO.EQ.1) CALL FGROW(1,IGRWYR,LYR,I)
              ENDDO
              CALL FWRITF(III,IGROWO,IGRWYR,IR,IUNIT,IRW)
           END DO
        ENDIF
! table 48
        III=48
        IF (REPORT_SWITCH(III) .or. LADYFILE .ne. 0) THEN
           DO IR=1,9
              TABHED(NHED(III),III)=REGNAM(IR)
              DO I=1,NSCEN
                 CALL FCOPY(III,IR,I)
                 IF(IGROWO.EQ.1) CALL FGROW(1,IGRWYR,LYR,I)
              ENDDO
              CALL FWRITF(III,IGROWO,IGRWYR,IR,IUNIT,IRW)
           END DO
        ENDIF
! table 59
        III=59
        IF (REPORT_SWITCH(III) .or. LADYFILE .ne. 0) THEN
           DO IR=1,num_elec_regions
              TABHED(NHED(III),III)=NERCNAM(IR)
              DO I=1,NSCEN
                 CALL FCOPY(III,IR,I)
                 IF(IGROWO.EQ.1) CALL FGROW(1,IGRWYR,LYR,I)
              ENDDO
              CALL FWRITF(III,IGROWO,IGRWYR,IR,IUNIT,IRW)
           END DO
        ENDIF
! table 61
        III=61
        IF (REPORT_SWITCH(III) .or. LADYFILE .ne. 0) THEN
           DO IR=1,num_elec_regions
              TABHED(NHED(III),III)=NERCNAM(IR)
              DO I=1,NSCEN
                 CALL FCOPY(III,IR,I)
!                 IOPT = 8
                 IF(IGROWO.EQ.1) CALL FGROW(1,IGRWYR,LYR,I)
              ENDDO
              CALL FWRITF(III,IGROWO,IGRWYR,IR,IUNIT,IRW)
           END DO
        ENDIF
! table 62
        III=62
        IF (REPORT_SWITCH(III) .or. LADYFILE .ne. 0) THEN
           DO IR=1,num_elec_regions
              TABHED(NHED(III),III)=NERCNAM(IR)
              DO I=1,NSCEN
                 CALL FCOPY(III,IR,I)
                 IOPT = 8
                 IF(IGROWO.EQ.1) CALL FGROW(1,IGRWYR,LYR,I)
              ENDDO
              CALL FWRITF(III,IGROWO,IGRWYR,IR,IUNIT,IRW)
           END DO
        ENDIF
! table 67
        III=67
        IF (REPORT_SWITCH(III) .or. LADYFILE .ne. 0) THEN
           DO IR=1,num_elec_regions
              TABHED(NHED(III),III)=NERCNAM(IR)
              DO I=1,NSCEN
                 CALL FCOPY(III,IR,I)
                 IF(IGROWO.EQ.1) CALL FGROW(1,IGRWYR,LYR,I)
              ENDDO
              CALL FWRITF(III,IGROWO,IGRWYR,IR,IUNIT,IRW)
           END DO
        ENDIF
! table 70
        III=70
        IF (REPORT_SWITCH(III) .or. LADYFILE .ne. 0) THEN
           DO IR=1,9
              TABHED(NHED(III),III)=REGNAM(IR)
              DO I=1,NSCEN
                 CALL FCOPY(III,IR,I)
                 IF(IGROWO.EQ.1) CALL FGROW(1,IGRWYR,LYR,I)
              ENDDO
              CALL FWRITF(III,IGROWO,IGRWYR,IR,IUNIT,IRW)
           END DO
        ENDIF
! table 93
        III=93
        IF (REPORT_SWITCH(III) .or. LADYFILE .ne. 0) THEN
           DO IR=1,9
              TABHED(NHED(III),III)=REGNAM(IR)
              DO I=1,NSCEN
                 CALL FCOPY(III,IR,I)
                 IF(IGROWO.EQ.1) CALL FGROW(1,IGRWYR,LYR,I)
              ENDDO
              CALL FWRITF(III,IGROWO,IGRWYR,IR,IUNIT,IRW)
           END DO
        ENDIF
! table 102
        III=102
        IF (REPORT_SWITCH(III) .or. LADYFILE .ne. 0) THEN
           DO IR=1,9
              TABHED(NHED(III),III)=REGNAM(IR)
              DO I=1,NSCEN
                 CALL FCOPY(III,IR,I)
                 IF(IGROWO.EQ.1) CALL FGROW(1,IGRWYR,LYR,I)
              ENDDO
              CALL FWRITF(III,IGROWO,IGRWYR,IR,IUNIT,IRW)
           END DO
        ENDIF
! table 109
!       III=109
!       IF (REPORT_SWITCH(III) .or. LADYFILE .ne. 0) THEN
!          DO IR=1,5
!             TABHED(NHED(III),III)=PADDNAM(IR)
!             DO I=1,NSCEN
!                CALL FCOPY(III,IR,I)
!                IF(IGROWO.EQ.1) CALL FGROW(1,IGRWYR,LYR,I)
!             ENDDO
!             CALL FWRITF(III,IGROWO,IGRWYR,IR,IUNIT,IRW)
!          END DO
!       ENDIF
! table 116
        III=116
        IF (REPORT_SWITCH(III) .or. LADYFILE .ne. 0) THEN
           DO IR=1,num_elec_regions
              TABHED(NHED(III),III)=NERCNAM(IR)
              DO I=1,NSCEN
                 CALL FCOPY(III,IR,I)
                 IF(IGROWO.EQ.1) CALL FGROW(1,IGRWYR,LYR,I)
              ENDDO
              CALL FWRITF(III,IGROWO,IGRWYR,IR,IUNIT,IRW)
           END DO
        ENDIF
! table 119
        III=119
        IF (REPORT_SWITCH(III) .or. LADYFILE .ne. 0) THEN
           DO IR=1,9
              TABHED(NHED(III),III)=REGNAM(IR)
              DO I=1,NSCEN
                 CALL FCOPY(III,IR,I)
                 IF(IGROWO.EQ.1) CALL FGROW(1,IGRWYR,LYR,I)
              ENDDO
              CALL FWRITF(III,IGROWO,IGRWYR,IR,IUNIT,IRW)
           END DO
        ENDIF
! table 122
        III=122
        IF (REPORT_SWITCH(III) .or. LADYFILE .ne. 0) THEN
           DO IR=1,num_elec_regions
              TABHED(NHED(III),III)=NERCNAM(IR)
              DO I=1,NSCEN
                 CALL FCOPY(III,IR,I)
                 IF(IGROWO.EQ.1) CALL FGROW(1,IGRWYR,LYR,I)
              ENDDO
              CALL FWRITF(III,IGROWO,IGRWYR,IR,IUNIT,IRW)
           END DO
        ENDIF
! table 123
        III=123
        IF (REPORT_SWITCH(III) .or. LADYFILE .ne. 0) THEN
           DO IR=1,num_elec_regions
              TABHED(NHED(III),III)=NERCNAM(IR)
              DO I=1,NSCEN
                 CALL FCOPY(III,IR,I)
                 IF(IGROWO.EQ.1) CALL FGROW(1,IGRWYR,LYR,I)
              ENDDO
              CALL FWRITF(III,IGROWO,IGRWYR,IR,IUNIT,IRW)
           END DO
        ENDIF
! table 125
        III=125
        IF (REPORT_SWITCH(III) .or. LADYFILE .ne. 0) THEN
           DO IR=1,9
              TABHED(NHED(III),III)=REGNAM(IR)
              DO I=1,NSCEN
                 CALL FCOPY(III,IR,I)
                 IF(IGROWO.EQ.1) CALL FGROW(1,IGRWYR,LYR,I)
              ENDDO
              CALL FWRITF(III,IGROWO,IGRWYR,IR,IUNIT,IRW)
           END DO
        ENDIF
        IOPT = 1
      ENDIF
      IF(IPRFTN.EQ.1) CALL FOOT(IUNIT,IRW)
! create the .xml file if spreadsheet option is on
      IF (ICRWK1 .EQ. 1) then
         call fxml(9999,0,0,'      ',0)
         if (ftabbone.eq.0) then ! if bonus rows are being excluded, then this is a publication candidate.  Create table browser database
           call fxmldb(9999,0,0,'      ',0)
         endif
      endif
!     --- CREATE A LISTING OF THE TABLES
      CALL TABLELST
      if (ladyfile .ne. 0) close(21)
! normal exit
      STOP
 951  WRITE(6,*)' ERROR OPENING ftab.csv '
      STOP 50
 952  WRITE(6,*)' ERROR OPENING FTAB ERROR FILE'
      STOP 51
      END
!***********************************************************************
      SUBROUTINE FDATA1(FFUNFMT)
  use dflib
       IMPLICIT NONE
! First of three subroutines to set up the table arrays.  This one also reads
! the RESTART files and calls the other two (FDATA2 and FDATA3).

! These include files are all in the Global Data Structure (i.e., the RESTART file)
      include 'parametr'
      include 'emmparm'
      include 'qblk'
      include 'ampblk'          !  use adjusted prices
      include 'macout'
      include 'lfmmout'
      include 'pmmout'
      include 'pmmrpt'
      include 'pmmftab'
      include 'ogsmout'
      include 'convfact'
      include 'ngtdmrep'
      include 'angtdm'
      include 'ncntrl'
      include 'emission'
      include 'uefpout'
      include 'uefdout'
      include 'udatout'
      include 'uecpout'
      include 'uettout'
      include 'efpout'
      include 'comparm'
      include 'comout'
      include 'commrep'
      include 'cogen'
      include 'cdsparms'
      include 'coalout'
      include 'coalemm'
      include 'coalrep'
      include 'pqchar' ! for parameter mnoth used in include file converge
      include 'converge'
      include 'indrep'
      include 'indout'
      include 'intout'
      include 'resdrep'
      include 'tranrep'
      include 'wrenew'
      include 'ftable'
      include 'rseff'
      include 'emeblk'
      include 'ghgrep'
      include 'aponroad'
      include 'qonroad'
      include 'aeusprc'
      include 'mcdetail'

      INTEGER*4 FFUNFMT(MAXS) ! RESTART FORMATS. =1 IF UNFORMATTED, =0 FORMATED
      INTEGER*4 III,II,LOOP1,ISEC !COUNTERS
      INTEGER*4 FRTYPE,FSOURC,FUNITI,FUNITO,FRETCD,FUNFMT
      INTEGER*4 ORG,OTYP
      CHARACTER*100 FNAMEI,FNAMEO

      CHARACTER*165 SCENLINE
      REAL*4 FSUM,FFF,ELECLOSS
      REAL modREFCON(18,5,MNUMYR)

      EXTERNAL FSUM
! indcarb declarations
      real indcarb
      external indcarb

      REAL DENOM,MTOECONV,RDAYS
      REAL QUADS/.001/
      INTEGER I,IR,IS,IY,ICY,IYY
      LOGICAL IEXIST, FIRST
      INTEGER IUNIT

! Arrays and Constants for Energy Index Table 31 -- sw2
      integer IndexStartYear,ix,jx
      real*4 driver_cy, driver_py
          integer itotal_nbr,itotal_nbr_res,itotal_nbr_com,itotal_nbr_ind,itotal_nbr_tran,itotal_nbr_util
          integer itotal_nbr_carb,itotal_nbr_carb_res,itotal_nbr_carb_com,itotal_nbr_carb_ind,itotal_nbr_carb_tran,itotal_nbr_carb_util
      integer ifirst_nbr,ilast_nbr,iskip_nbr(10),iline_nbr,itest_nbr,iflag_nbr
      integer idrivers(10), iprintflag,iprintflag2,iprintfile,iprintindex,irpntr,itab,IB,irow
      real*4 tab34_cy(21),tab34_py(21),drv35_cy(21),drv35_py(21), primary
      real*4 trdrv_cy(10),trdrv_py(10),trnen_cy(10),trnen_py(10)
      Real*4 Res_energy,Com_energy,Utl_energy,Ind_energy,Trn_energy,Check_total
      Real*4 Res_carbon,Com_carbon,Utl_carbon,Ind_carbon,Trn_carbon
      real*4 ElecCarb_CY,ElecCarb_PY,CarbCnvFac
      real*4 rs(29),rsef(29),rsly(29),rsefly(29)
      real*4 cmindx_cy(29),cmindx_py(29),cmdriver_cy(29),cmdriver_py(29),cmen_cy(29),cmen_py(29)
      real*4 indcrb(13,10,2),inddriver(13,2)
! End Arrays and Constants for Energy Index Table 31 -- sw2
! Index Constants for Industrial Tables 35 to 44
      integer ixEL/1/,ixNG/2/,ixCL/3/,ixMC/4/, &
              ixCI/5/,ixRF/6/,ixDS/7/,ixLG/8/,ixMG/9/, &
              ixSG/10/,ixPC/11/,ixAS/12/,ixPF/13/,ixKS/14/, &
              ixOP/15/,ixNF/16/,ixLF/17/,ixRN/18/
      integer v8(6),v88(5),v888(3)
  character*60 nems,cmd*255
      logical lexist
      integer iss
      integer*4 filesize,stats(12),ISO2
      integer*2 il,inddir
      real efficiency,powerheat ! For industry-by-industry tables 36-44
      REAL CAR_PCT,TRK_PCT
      REAL C_CO2_FACTOR,C8(9)
      REAL MMBD_TO_VOLUME,MMBD_TO_MASS            ! variables for T21
      REAL AGSCALPR
      COMMON /AGSC/ AGSCALPR

      INTEGER CRfromCD(11) / 1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5/
      DATAB(1:7)= (/'DATAB','DATA2','DATA3','DATA4','DATA5','DATA6','DATA7'/)
 
      call getenv('NEMS',nems)
      if(nems.eq.' ') then
         nems='n:/default'
      endif
! GET THE DATA FROM THE DATA BASE
! FIRST:  READ DICTIONARY (FRTYPE=3)
      FRTYPE=3
      FSOURC=1
      FUNITI=25
      FUNITO=26
      FNAMEI= DICTNAME
      inquire(file=fnamei,exist=lexist)
      if(.not.lexist) then
        write(6,*)' Dictionary file not found: ',fnamei
        stop 49
      endif
      FNAMEO=' '
      FRETCD=0
      FUNFMT=0
      CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)

! THEN:  READ DATA (FRTYPE=1) LOOPING OVER REQUESTED SCENARIOS
      DATAB(1) = RESTART
      DATAB(2) = RESTART2
      DATAB(3) = RESTART3
      DATAB(4) = RESTART4
      DATAB(5) = RESTART5
      DATAB(6) = RESTART6
      DATAB(7) = RESTART7
! --- GET NAME OF FILE CONTAING OPTIONAL SCENARIO INFO FOR EACH RUN, AND STORE THAT NAME IN THE ARRAY
      FIRST = .TRUE.
      DO IS = 1,NSCEN
         DATAC(IS) = DATAB(IS)
         II = LEN_TRIM(DATAC(IS))
         DATAC(IS)(II-6:II+2)='scentext'
         INQUIRE(FILE=DATAC(IS),EXIST=IEXIST)
         EIA = ' '
         IF (IEXIST) THEN
            II = 1
            IF (FIRST) WRITE(20,'(I1)') II
            IUNIT=55+IS
            OPEN(IUNIT,FILE=DATAC(IS),READONLY)
            DO WHILE (II.EQ.1)
              READ(IUNIT,'(A165)',END=445)SCENLINE
              III = INDEX(SCENLINE,'DISCLAIM:')
              IF (III .NE. 0) THEN
                EIA = SCENLINE(III+9:165)
              ELSE
                SCENLINE(1:1) = ' '
                WRITE(20,'(A165)')SCENLINE
              ENDIF
            ENDDO
         ENDIF
  445    II = 0
         IF (IEXIST) CLOSE (IUNIT)
      ENDDO
      DO 9999 IS=1,NSCEN
        FRTYPE=2
        FSOURC=1
        FUNITI=35+IS
        FUNITO=45+IS
        FNAMEI=DATAB(IS)
        inquire(file=fnamei,exist=iexist)
        if(.not.iexist) then
          write(6,*) ' Missing restart file or bad restart file name in ftab, file=',trim(fnamei)
          write(6,*) ' If you see this message in nohup.out, it usually means that'
          write(6,*) ' NEMS bombed, so look above to see what happened.  If you are re-running'
          write(6,*) ' ftab, then the restart file you specified was not found. '
!         check other restart files while we're at it.
          do iss=is+1,nscen
            FNAMEI=DATAB(iss)
            inquire(file=fnamei,exist=iexist)
            if(.not.iexist) then
               write(6,*) ' By the way, ', trim(fnamei), ' is missing, too.'
            endif
          enddo
          stop 59
        endif
        call stat(fnamei,stats)
        filesize=stats(8)
        if(filesize.lt.5000000.and.filesize.gt.4000000) then
          write(6,*) trim(fnamei)//' is probably a 2020 restart file.  For your'
          write(6,*) 'convenience, I''ll make a 2025-compatible version using this command :'
          cmd=trim(nems)//'/scripts/makeit2025.exe '//trim(fnamei)//' temp2025.unf'
          write(6,'(a)') trim(cmd)
          call system(cmd)
          inquire(file='temp2025.unf',exist=lexist)
          if(lexist) then
            fnamei=' temp2025.unf'
          else
            write(6,*)' Hmmm.  I tried, but it didn''t seem to work.  Sorry.'
          endif
        elseif(filesize.le.0) then
          write(6,*) ' this restart file is empty: ',trim(fnamei)
          stop 60
        endif
        FNAMEO=' '
        FUNFMT=FFUNFMT(IS)
        CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
        CLOSE(UNIT=FUNITI)

        num_elec_regions = 22                 ! total EMM regions excluding the total (mnumnr) and filler
        IF (UNRGNS .GT. 0) num_elec_regions = UNRGNS      ! from restart file as of mnumnr=28 expansion

        regtables(1:no_reg_tables)= (/2, 3, 17, 21, 34, 48, 59, 61, 62, 67, 70, 93, 102, 116, 119, 122, 123, 125/)
        nregtables(1:no_reg_tables)=(/9, 9,  9,  4,  9,  9, 22, 22, 22, 22,  9,  9,   9,  22,   9,  22,  22,   9/)
        nregtables(7)=num_elec_regions
        nregtables(8)=num_elec_regions
        nregtables(9)=num_elec_regions
        nregtables(10)=num_elec_regions
        nregtables(14)=num_elec_regions
        nregtables(16)=num_elec_regions
        nregtables(17)=num_elec_regions

        IF (num_elec_regions .eq. 22) THEN
          NERCNAM(1:28)= (/ &
          '01 - Texas Reliability Entity', &
          '02 - Florida Reliability Coordinating Council', &
          '03 - Midwest Reliability Organization / East', &
          '04 - Midwest Reliability Organization / West', &
          '05 - Northeast Power Coordinating Council / New England', &
          '06 - Northeast Power Coordinating Council / NYC-Westchester', &
          '07 - Northeast Power Coordinating Council / Long Island', &
          '08 - Northeast Power Coordinating Council / Upstate New York', &
          '09 - Reliability First / East', &
          '10 - Reliability First / Michigan', &
          '11 - Reliability First / West', &
          '12 - SERC Reliability Corporation / Delta', &
          '13 - SERC Reliability Corporation / Gateway', &
          '14 - SERC Reliability Corporation / Southeastern', &
          '15 - SERC Reliability Corporation / Central', &
          '16 - SERC Reliability Corporation / Virginia-Carolina', &
          '17 - Southwest Power Pool / North', &
          '18 - Southwest Power Pool / South', &
          '19 - Western Electricity Coordinating Council / Southwest', &
          '20 - Western Electricity Coordinating Council / California', &
          '21 - Western Electricity Coordinating Council / Northwest Power Pool Area', &
          '22 - Western Electricity Coordinating Council / Rockies', &
          '23 - extra region 1', &
          '24 - extra region 2', &
          '25 - extra region 3', &
          '26 - Alaska', &
          '27 - Hawaii', &
          '28 - United States'/)

        ELSEIF (num_elec_regions .eq. 25) THEN
          NERCNAM(1:28)= (/ &
          '01 - Texas Reliability Entity', &
          '02 - Florida Reliability Coordinating Council', &
          '03 - Midcontinent / West', &
          '04 - Midcontinent / Central', &
          '05 - Midcontinent / East', &
          '06 - Midcontinent / South', &
          '07 - Northeast Power Coordinating Council / New England', &
          '08 - Northeast Power Coordinating Council / New York City and Long Island', &
          '09 - Northeast Power Coordinating Council / Upstate New York', &
          '10 - PJM / East', &
          '11 - PJM / West', &
          '12 - PJM / Commonwealth Edison', &
          '13 - PJM / Dominion', &
          '14 - SERC Reliability Corporation / East', &
          '15 - SERC Reliability Corporation / Southeastern', &
          '16 - SERC Reliability Corporation / Central', &
          '17 - Southwest Power Pool / South', &
          '18 - Southwest Power Pool / Central', &
          '19 - Southwest Power Pool / North', &
          '20 - Western Electricity Coordinating Council / Southwest', &
          '21 - Western Electricity Coordinating Council / California North', &
          '22 - Western Electricity Coordinating Council / California South', &
          '23 - Western Electricity Coordinating Council / Northwest Power Pool Area', &
          '24 - Western Electricity Coordinating Council / Rockies', &
          '25 - Western Electricity Coordinating Council / Basin', &
          '26 - Alaska', &
          '27 - Hawaii', &
          '28 - United States'/)
        ELSE
          WRITE(6,'("NUMBER OF EMM REGIONS NOT 22 OR 25!  NOT SETTING NERCNAMs")')
        ENDIF

! THIS SCALES QBLK FROM TRILLS TO QUADS AND PRICE VARIABLES FROM 1987 DOLLARS USING SCALPR VIA FINFLATE
! IT ALSO CONVERTS SEVERAL UTILITY ARRAYS TO PHYSICAL UNITS

! Take the GDP delator directly from the macro variable, after determining which year dollars to print
        IF (YEARPR .NE. FYRPRC .AND. NSCEN .EQ. 1) THEN
          WRITE(6,'(A,I6,A,F8.4)') ' THE DOLLAR YEAR FROM THE RESTART FILE WAS ',YEARPR, &
                                   ' DEFLATOR = ',SCALPR
        ENDIF
        SCALPR=MC_JPGDP(FYRPRC-1989)
        AGSCALPR=(MC_DETAIL(100,FYRPRC-1989)/(MC_DETAIL(100,1)*0.97))! $1990*.97=$1987
        IF (YEARPR .NE. FYRPRC .AND. NSCEN .EQ. 1) THEN
          WRITE(6,'(A,I6,A,F8.4)') ' SETTING THE DOLLAR YEAR TO ',FYRPRC, ' DEFLATOR = ',SCALPR
        ENDIF

!  COMPUTE SECTORAL AND FUEL TOTALS--THIS HELPS WITH CONSISTENCY
!  IN THE QBLK ARRAYS WHEN INDIVIDUAL ARRAYS ARE CHANGED IN THE
!  THE RESTART FILE OR CORRECTED HERE IN THE REPORT WRITER.
        CALL SUMQAS
!  FOR QUANTITIES IN QBLK, CONVERT FROM TRILLION BTUS TO QUADS
        DO 11 I=1,MQSIZE
          IF (ABS(MQARRY(I)) .GT. 0.0000000001 .AND. QUADS .GT. 0.0000000001) THEN
             MQARRY(I) = MQARRY(I) * QUADS       ! multiplying by .001
          ELSE
             MQARRY(I) = 0.0
          END IF
  11    CONTINUE
! Adjustments
        CALL FINFLATE        ! converts price variables to requested year dollars
        CALL CAPFLATE        ! other adjustments including rebasing capacity additions to requested first year
! FILLS THE REPORT TABLES FROM NEMS VARIABLES.  THE TABLE ARRAY NUMBERS
! ASSOCIATE WITH THE REFERENCE NUMBERS IN THE TABLE LAYOUT FILE.

! CONVERSION FACTOR BLOCK
! NAT GAS BTU FACTORS.  1: DRY GAS PRODUCTION, 2: NON UTILITY CONSUMPTION
!                       3: ELEC UTIL CONSUMP   4: IMPORTS, NET IMPORTS
!                       5: AVERAGE CONSUMP
! USED TO CONVERT TABLE 9 FROM BTUS TO TCF.  THE YEAR DIMENSION (31)
! PROVIDES FOR ACTUAL HISTORICAL, 1=1989.  SOURCE: MER 11/88, TABLE A5
!
! PETROLEUM PRODUCTS (BTU/BARRELS)
!   USED TO CONVERT SEVERAL /ALLPBL/ ARRAYS WITH PETROLEUM PRODUCTS
!   FROM PHYSICAL UNITS TO BTUS
! 1: NG PLANT LIQUIDS, 2:PRODUCT IMPORTS, 3: PRODUCT EXPORT, 4:CRUDE IMPORTS
! 1: TABLE A3          2: TABLE A4        3: TABLE A4        4:TABLE A3

!  TABLE ARRAYS ARE FILLED INDEPENDENTLY (THAT IS, ONE TABLE ARRAY IS NOT USED
!   TO FILL ANOTHER, EXCEPT FOR A BALANCING ITEM BETWEEN TABLES 11-12).

! TABLE 1.  YEARLY SUPPLY AND DISPOSITION SUMMARY OF TOTAL ENERGY
      DO 10 IY=1,LASTYR
        RDAYS = 365.       ! dont check for leap year
!   PRODUCTION
      T1(1,IY,IS)=RFQTDCRD(MNUMOR+2,IY)*RDAYS*CFCRDDOM(IY)*.001
      T1(2,IY,IS)=RFQNGPL(MNUMPR,IY,6)/1000.*RDAYS*CFNGL(IY)*.001
      T1(3,IY,IS)=CFNGC(IY)* (OGQNGREP(1,IY) + OGQNGREP(2,IY) + &
                              OGQNGREP(3,IY) + OGQNGREP(4,IY) + &
                              OGQNGREP(5,IY) + OGQNGREP(6,IY) + &
                              OGQNGREP(7,IY) + OGQNGREP(8,IY) + OGSHALENG(IY)) * .001
      T1(4,IY,IS)=(CQSBB(3,IY) + sum(WC_PROD_BTU(11,1:MNUMLR,IY)))/1000.
      T1(5,IY,IS)=QUREL(11,IY)
      T1(31,IY,IS) = QHOAS(11,IY)
      T1(32,IY,IS) = QBMAS(11,IY) - QBMRF(11,IY) + CORNCD(3,11,IY) * CFCORN / 1000000000. + &
                          sum(BIMQTYCD(1:4,11,IY))/1000000.*RDAYS*CFVEGGIE(IY) + &
                  (SBO2GDTPD(MNUMPR,IY) + YGR2GDTPD(MNUMPR,IY) + WGR2GDTPD(MNUMPR,IY))*CFVEGGIE(IY)*RDAYS/1000. + &
                          QBMRFBTL(11,IY)/1000.
      IF (CONEFF(IY) .NE. 0.0) T1(32,IY,IS) = &
             T1(32,IY,IS) + 0.9751*CLLETHCD(11,IY) * RDAYS * CFBMQ(IY) * 42. / CONEFF(IY) / 1000000.
! remove nonbiogenic from msw/renew and put into other (wncmsel)
      T1(6,IY,IS) = QTRAS(11,IY) - WNCMSEL(IY,11) - QGERS(11,IY) - QSTRS(11,IY) - &
                    QSTCM(11,IY) - QPVCM(11,IY) - QPVRS(11,IY) - &
                    QBMAS(11,IY) - QHOAS(11,IY) - QETTR(11,IY)
      T1(7,IY,IS)= WNCMSEL(IY,11) + RFCRDOTH(MNUMPR,IY) * CFCRDDOM(IY) * RDAYS / 1000. + &
                   RFHCXH2IN(MNUMPR,IY) * CFRSQ * RDAYS / 1000. + &
                   RFMETM85(MNUMPR,IY) * CFM85Q(IY) * RDAYS / 1000.
      T1(8,IY,IS)=FSUM(T1(1,IY,IS),7)+FSUM(T1(31,IY,IS),2)
!   IMPORTS
      T1(9,IY,IS) = (RFQICRD(MNUMPR,IY) + RFSPRIM(IY)) * CFCRDIMP(IY) *RDAYS*.001
! using CFDSQT for now for RENEWDIMP
      T1(10,IY,IS) = (RFPQIPRDT(MNUMPR,IY,2) * CFIMPRD(IY) + ETHIMP(11,IY)/1000. * CFPET + &
                     BIODIMP(11,IY)/1000. * CFBIOD(IY) + RENEWDIMP(11,IY)/1000. * CFDSQT(IY)+ &
 RFIPQCBOB(MNUMPR,IY,2) * CFCBOB(IY) / 1000. + &
 RFIPQRBOB(MNUMPR,IY,2) * CFRBOB(IY) / 1000. + &
                     RFMTBI(MNUMPR,IY) * 4.24 + &
                     RFPQUFC(MNUMPR,IY,2) * CFIMUO(IY)) * RDAYS * .001
      T1(11,IY,IS) = NGIMPVOL(4,IY) * CFNGI(IY) * .001
      T1(12,IY,IS) = QCIIN(11,IY)+CQDBFB(11,7,IY)*.001 +QEIEL(11,IY)
      T1(13,IY,IS) = FSUM(T1(9,IY,IS),4)
!   EXPORTS
      T1(14,IY,IS) = (RFQEXCRD(MNUMPR,IY) * .001 * CFCRDEXP(IY) + &
                      RFQEXPRDT(MNUMPR,IY) * CFEXPRD(IY) + ETHEXP(11,IY)/1000. * CFPET + &
                      BIODEXP(11,IY)/1000. * CFBIOD(IY)) * RDAYS * .001
      T1(15,IY,IS) = NGEXPVOL(4,IY) * CFNGE(IY) * .001
      T1(16,IY,IS) = CQDBFB(11,5,IY)*.001
      T1(17,IY,IS) = FSUM(T1(14,IY,IS),3)
!   CONSUMPTION --- IF THESE VALUES BELOW CHANGE, NEED TO ADJUST T18(15,
      T1(19,IY,IS) = QTPAS(11,IY) + QETTR(11,IY) + QMETR(11,IY)
      T1(20,IY,IS) = QNGAS(11,IY)+QGPTR(11,IY)+QLPIN(11,IY)+QNGLQ(11,IY) - QHYTR(11,IY) / .7
! for coal, subtract out coal gasification coal, but leave in losses
! this means we subtract the gas production number
      T1(21,IY,IS) = QCLAS(11,IY)+QMCIN(11,IY)+QCIIN(11,IY) - OGSUPGAS(1,11,IY) * CFNGC(IY) * .001
      T1(22,IY,IS) = QUREL(11,IY)
      T1(33,IY,IS) = QBMAS(11,IY) - QBMRF(11,IY) + CORNCD(3,11,IY) * CFCORN / 1000000000. - &
                     RFBIOBUTECD(MNUMCR,IY)*RDAYS*CFBIOBUTE(IY)/1000000. - &
                    0.9751*(CRNETHCD(11,IY)+CLLETHCD(11,IY)+OTHETHCD(11,IY))/1000. * CFPET * RDAYS / 1000. + &
                     sum(BIMQTYCD(1:4,11,IY))/1000000.*RDAYS*(CFVEGGIE(IY)-CFBIOD(IY)) + &
                 QBMRFBTL(11,IY)/1000. - RDAYS / 1000000. * &
                      (sum(BTLFRAC(1:4,MNUMPR,IY)) * CFBTLLIQ(IY) + &
                       sum(CBTLFRAC(2,1:4,MNUMPR,IY)) * CFCBTLLIQ(2,IY) + &
                       UBAVOL(MNUMPR,IY) * 5.763)
      IF (CONEFF(IY) .NE. 0.0) T1(33,IY,IS) = &
               T1(33,IY,IS) + 0.9751*CLLETHCD(11,IY) * RDAYS * CFBMQ(IY) * 42. / CONEFF(IY) / 1000000.
! remove nonbiogenic from msw/renew and put into other (wncmsel)
      T1(23,IY,IS)=QTRAS(11,IY) - WNCMSEL(IY,11) - QGERS(11,IY) - QSTRS(11,IY) - &
                   QSTCM(11,IY) - QPVCM(11,IY) - QPVRS(11,IY) - &
                   QBMAS(11,IY) - QHOAS(11,IY) - QETTR(11,IY)
      T1(24,IY,IS) = QEIEL(11,IY) + WNCMSEL(IY,11) + QHYTR(11,IY)
      T1(25,IY,IS)=FSUM(T1(19,IY,IS),6)+T1(31,IY,IS)+T1(33,IY,IS)
! ADJUSTMENTS (BALANCING ITEM) =PRODUCTION-CONSUMP+NSW+IMPORT-EXPORT)
      T1(18,IY,IS)=T1(8,IY,IS)-T1(25,IY,IS)+T1(13,IY,IS)-T1(17,IY,IS)
      T1(26,IY,IS)=T1(9,IY,IS)+T1(10,IY,IS)-T1(14,IY,IS)
      T1(26,IY,IS) = WTI_PRICE(IY)
      T1(27,IY,IS) = BRENT_PRICE(IY)
      T1(28,IY,IS) = START_PRICE(IY)
      T1(41,IY,IS) = T1(27,IY,IS) * 1.32
      T1(42,IY,IS) = T1(27,IY,IS) * 0.75
      T1(37,IY,IS) = IT_WOP(IY,2)
      T1(34,IY,IS) = OGHHPRNG(IY)
      T1(35,IY,IS) = OGWPRNG(MNUMOR,IY) / CFNGC(IY)
      T1(29,IY,IS) = CPSB(3,IY)*CPSBT(3,IY)
      T1(37,IY,IS) = CPSB(3,IY)
      T1(36,IY,IS) = (PCLSN(11,IY)*QCLSN(11,IY) + PCLIN(11,IY)*(QCLIN(11,IY)-QCTLRF(11,IY)) + &
            PCLCM(11,IY)*QCLCM(11,IY) + PMCIN(11,IY)*QMCIN(11,IY) + PCLEL(11,IY)*QCLEL(11,IY)) / &
           (QCLCM(11,IY) + QCLIN(11,IY) - QCTLRF(11,IY) + QMCIN(11,IY) + QCLEL(11,IY) + QCLSN(11,IY))
      T1(30,IY,IS) = PELAS(11,IY) * .3412
! nominal prices
      T1(38,IY,IS) = T1(26,IY,IS) / SCALPR * MC_JPGDP(IY)
      T1(39,IY,IS) = T1(27,IY,IS) / SCALPR * MC_JPGDP(IY)
      T1(40,IY,IS) = T1(34,IY,IS) / SCALPR * MC_JPGDP(IY)
      T1(35,IY,IS) = T1(37,IY,IS) / SCALPR * MC_JPGDP(IY)
      T1(43,IY,IS) = T1(29,IY,IS) / SCALPR * MC_JPGDP(IY)
      T1(44,IY,IS) = T1(36,IY,IS) / SCALPR * MC_JPGDP(IY)
      T1(45,IY,IS) = T1(30,IY,IS) / SCALPR * MC_JPGDP(IY)
10    CONTINUE

! TABLE 2 (REGIONAL) CONSUMPTION OF ENERGY BY SOURCE AND END-USE SECTORS
!
! QNTLOS (PIPELINE COMPRESSOR GAS) AND
! PFLLOS (NG LEASE AND PLANT) ARE IN NATIONAL TABLES ONLY
! RESIDENTIAL
      DO 20 IY=1,LASTYR
      DO 20 IR=1,11
          IF (IR .EQ. 10) CYCLE
          ICY=IY+BASEYR-1
          ELECLOSS = QTSEL(IR,IY) - QELAS(IR,IY)
          T2( 1,IR,IY,IS)=QDSRS(IR,IY)
          T2( 2,IR,IY,IS)=QKSRS(IR,IY)
          T2( 3,IR,IY,IS)=QLGRS(IR,IY)
          T2( 4,IR,IY,IS)=QTPRS(IR,IY)
          T2( 5,IR,IY,IS)=QNGRS(IR,IY)
          T2( 6,IR,IY,IS)=QCLRS(IR,IY)
          T2( 7,IR,IY,IS)=QBMRS(IR,IY)
          T2( 8,IR,IY,IS)=QELRS(IR,IY)
          T2( 9,IR,IY,IS)=QTSRS(IR,IY) - QSTRS(IR,IY) - QGERS(IR,IY) - QPVRS(IR,IY)
          T2(10,IR,IY,IS)=QELRS(IR,IY) / QELAS(IR,IY) * ELECLOSS
          T2(11,IR,IY,IS)=FSUM(T2(9,IR,IY,IS),2)
! COMMERCIAL
          T2(12,IR,IY,IS)=QDSCM(IR,IY)
          T2(13,IR,IY,IS)=QRSCM(IR,IY)
          T2(14,IR,IY,IS)=QKSCM(IR,IY)
          T2(15,IR,IY,IS)=QLGCM(IR,IY)
          T2(16,IR,IY,IS)=QMGCM(IR,IY)
          T2(17,IR,IY,IS)=QTPCM(IR,IY)
          T2(18,IR,IY,IS)=QNGCM(IR,IY)
          T2(19,IR,IY,IS)=QCLCM(IR,IY)
          T2(20,IR,IY,IS)=QTRCM(IR,IY) - QSTCM(IR,IY) - QPVCM(IR,IY)
          T2(21,IR,IY,IS)=QELCM(IR,IY)
          T2(22,IR,IY,IS)=QTSCM(IR,IY) - QSTCM(IR,IY) - QPVCM(IR,IY)
          T2(23,IR,IY,IS)=QELCM(IR,IY) / QELAS(IR,IY) * ELECLOSS
          T2(24,IR,IY,IS)=FSUM(T2(22,IR,IY,IS),2)
! INDUSTRIAL
          T2(25,IR,IY,IS)=QDSIN(IR,IY)
          T2(26,IR,IY,IS)=QLGIN(IR,IY)
          T2(27,IR,IY,IS)=QPFIN(IR,IY)
          T2(28,IR,IY,IS)=QRSIN(IR,IY)
          T2(29,IR,IY,IS)=QMGIN(IR,IY)
          T2(30,IR,IY,IS)=QOTIN(IR,IY)+QASIN(IR,IY)+QPCIN(IR,IY)+QKSIN(IR,IY)+QSGIN(IR,IY)
          T2(31,IR,IY,IS)=QTPIN(IR,IY)
          T2(115,IR,IY,IS)=QNGIN(IR,IY) - QGTLRF(IR,IY)
          T2(116,IR,IY,IS)=QLPIN(IR,IY)
          T2(120,IR,IY,IS)=QGTLRF(IR,IY)
          T2(32,IR,IY,IS)=QNGIN(IR,IY)+QLPIN(IR,IY)
          T2(33,IR,IY,IS)=QMCIN(IR,IY)
! for coal, subtract out coal gasification coal, but leave in losses
! this means we subtract the gas production number
          T2(34,IR,IY,IS)=QCLIN(IR,IY)-QCTLRF(IR,IY) - OGSUPGAS(1,IR,IY) * CFNGC(IY) * .001
          T2(122,IR,IY,IS)=QCLIN(IR,IY)
          T2(119,IR,IY,IS)=QCTLRF(IR,IY)
          T2(35,IR,IY,IS)=QCIIN(IR,IY)
          T2(36,IR,IY,IS)=FSUM(T2(33,IR,IY,IS),3)+T2(119,IR,IY,IS)
! the following formula shares corn to the corn ethanol producing regions
! cellulose and biodiesel losses are calculated from conversion factor differences; no need to share
          IF ((QMGTR(11,IY)-QMGBS(11,IY)) .NE. 0.0) &
          T2(73,IR,IY,IS)= ((QMGTR(IR,IY)-QMGBS(IR,IY))/(QMGTR(11,IY)-QMGBS(11,IY))) * &
             (CORNCD(1,11,IY) * CFCORN / 1000. - 0.9751*(CRNETHCD(11,IY)+OTHETHCD(11,IY)) * 365. * CFPET) / 1000000. + &
                           ((QDSTR(IR,IY)-QDSBS(IR,IY))/(QDSTR(11,IY)-QDSBS(11,IY))) * &
              sum(BIMQTYCD(1:4,11,IY))/1000000.*RDAYS*(CFVEGGIE(IY)-CFBIOD(IY))
          IF (QBMRFBTL(11,IY) .NE. 0.0) &
          T2(73,IR,IY,IS)= T2(73,IR,IY,IS) + &
              (QBMRFBTL(11,IY)/1000. - RDAYS / 1000000. * &
              (sum(BTLFRAC(1:4,MNUMPR,IY)) * CFBTLLIQ(IY) + &
               sum(CBTLFRAC(2,1:4,MNUMPR,IY)) * CFCBTLLIQ(2,IY) + &
               UBAVOL(MNUMPR,IY) * 5.763)) * &
              (QBMRFBTL(IR,IY)/QBMRFBTL(11,IY))    !  this regionalizes the biomass-to-liquids
          T2(73,IR,IY,IS) = T2(73,IR,IY,IS) + &
                    (CORNCD(2,IR,IY)*CFCORN / 1000. - RFBIOBUTECD(IR,IY)*RDAYS*CFBIOBUTE(IY))/1000000.
      IF ((QMGTR(11,IY)-QMGBS(11,IY)) .NE. 0.0 .AND. CONEFF(IY) .NE. 0.0) T2(73,IR,IY,IS) = T2(73,IR,IY,IS) + &
            ((QMGTR(IR,IY)-QMGBS(IR,IY))/(QMGTR(11,IY)-QMGBS(11,IY))) * &
             (0.9751*CLLETHCD(11,IY) * RDAYS * (CFBMQ(IY) * 42. / CONEFF(IY) - CFPET)) / 1000000.
          T2(37,IR,IY,IS)=QTRIN(IR,IY) - QBMRF(IR,IY)
          T2(38,IR,IY,IS)=QELIN(IR,IY)
          T2(39,IR,IY,IS)=QTSIN(IR,IY) - QBMRF(IR,IY) + T2(73,IR,IY,IS)
          if(ir.eq.11) T44(58,IY,IS) = 1000*T2(39,11,IY,IS) ! copy industrial delivered energy total for use in Table 44 balancing item

          T2(40,IR,IY,IS)=QELIN(IR,IY) / QELAS(IR,IY) * ELECLOSS
          T2(41,IR,IY,IS)=FSUM(T2(39,IR,IY,IS),2)
! TRANSPORTATION
          T2(42,IR,IY,IS)=QDSTR(IR,IY) - QDSBS(IR,IY)
          T2(43,IR,IY,IS)=QJFTR(IR,IY) - QJFBS(IR,IY)
          T2(44,IR,IY,IS)=QMGTR(IR,IY) - QMGBS(IR,IY) + QETTR(IR,IY) + QMETR(IR,IY)
          T2(45,IR,IY,IS)=QRSTR(IR,IY)
          T2(46,IR,IY,IS)=QLGTR(IR,IY)
          T2(47,IR,IY,IS)=QOTTR(IR,IY)
          T2(48,IR,IY,IS)=QTPTR(IR,IY) + QETTR(IR,IY) + QMETR(IR,IY) - &
                          QDSBS(IR,IY) - QJFBS(IR,IY) - QMGBS(IR,IY)
          T2(49,IR,IY,IS)=QGPTR(IR,IY)
		  T2(128,IR,IY,IS)=QNGLQ(IR,IY)
          T2(50,IR,IY,IS)=QNGTR(IR,IY) - QHYTR(IR,IY) / .7
          T2(51,IR,IY,IS)=QETTR(IR,IY)
          T2(52,IR,IY,IS)=0.0
          T2(53,IR,IY,IS)=QHYTR(IR,IY)
          T2(54,IR,IY,IS)=QELTR(IR,IY)
          T2(55,IR,IY,IS)=FSUM(T2(48,IR,IY,IS),3) + FSUM(T2(52,IR,IY,IS),3) + T2(128,IR,IY,IS)
          T2(56,IR,IY,IS)=QELTR(IR,IY) / QELAS(IR,IY) * ELECLOSS
          T2(57,IR,IY,IS)=FSUM(T2(55,IR,IY,IS),2)
! Unspecified sector
          T2(124,IR,IY,IS)=QMGBS(IR,IY)
          T2(125,IR,IY,IS)=QJFBS(IR,IY)
          T2(126,IR,IY,IS)=QDSBS(IR,IY)
          T2(127,IR,IY,IS)=FSUM(T2(124,IR,IY,IS),3)
! DELIVERED ENERGY CONSUMPTION FOR ALL SECTORS
          T2(58,IR,IY,IS)=QDSAS(IR,IY) - QDSEL(IR,IY)
          T2(59,IR,IY,IS)=QKSAS(IR,IY)
          T2(60,IR,IY,IS)=QJFTR(IR,IY)                       !  All sectors - don't subtract QJFBS
          T2(61,IR,IY,IS)=QLGAS(IR,IY)
          T2(62,IR,IY,IS)=QMGAS(IR,IY) + QETTR(IR,IY) + QMETR(IR,IY)
          T2(63,IR,IY,IS)=QPFIN(IR,IY)
          T2(64,IR,IY,IS)=QRSAS(IR,IY) - QRSEL(IR,IY)
          T2(65,IR,IY,IS)=QOTAS(IR,IY) + QSGIN(IR,IY) + QPCIN(IR,IY) + QASIN(IR,IY)
          T2(66,IR,IY,IS)=FSUM(T2(58,IR,IY,IS),8)
          T2(117,IR,IY,IS)=QNGAS(IR,IY) - QNGEL(IR,IY) - QHYTR(IR,IY) / .7 - QGTLRF(IR,IY)
          T2(67,IR,IY,IS)=T2(117,IR,IY,IS) + T2(120,IR,IY,IS) + T2(128,IR,IY,IS) +  &
                          T2(116,IR,IY,IS) + T2( 49,IR,IY,IS) + T2( 52,IR,IY,IS)
          T2(68,IR,IY,IS)=QMCIN(IR,IY)
! for coal, subtract out coal gasification coal, but leave in losses
! this means we subtract the gas production number
          T2(69,IR,IY,IS)=QCLAS(IR,IY) - QCLEL(IR,IY) - QCTLRF(IR,IY) - OGSUPGAS(1,IR,IY) * CFNGC(IY) * .001
          T2(70,IR,IY,IS)=QCIIN(IR,IY)
          T2(71,IR,IY,IS)=FSUM(T2(68,IR,IY,IS),3) + T2(119,IR,IY,IS)
          T2(72,IR,IY,IS)=QBMRS(IR,IY) + QBMCM(IR,IY) + QTRIN(IR,IY) - QBMRF(IR,IY) + QTRSN(IR,IY)
          T2(75,IR,IY,IS)=QHYTR(IR,IY)
          T2(74,IR,IY,IS)=QELAS(IR,IY)
          T2(76,IR,IY,IS)=T2(66,IR,IY,IS) + T2(67,IR,IY,IS) + FSUM(T2(71,IR,IY,IS),5)
          T2(77,IR,IY,IS)=ELECLOSS
          T2(78,IR,IY,IS)=FSUM(T2(76,IR,IY,IS),2)
! ELECTRIC GENERATORS (INCLUDES NONTRADITIONAL COGEN FOR AEO03)
          T2(79,IR,IY,IS)=QDSEL(IR,IY)
          T2(80,IR,IY,IS)=QRSEL(IR,IY)
          T2(81,IR,IY,IS)=FSUM(T2(79,IR,IY,IS),2)
          T2(82,IR,IY,IS)=QNGEL(IR,IY)
          T2(83,IR,IY,IS)=QCLEL(IR,IY)
          T2(84,IR,IY,IS)=QUREL(IR,IY)
!  subtract nonbiogenic consumption from qtrel, nonbio still in total, qtsel
          T2(85,IR,IY,IS)=QTREL(IR,IY) + QPCEL(IR,IY) - WNCMSEL(IY,IR)
          T2(86,IR,IY,IS)=QEIEL(IR,IY)
          T2(87,IR,IY,IS)=QTSEL(IR,IY)
! TOTAL PRIMARY
          T2(88,IR,IY,IS)=QDSAS(IR,IY)
          T2(89,IR,IY,IS)=QKSAS(IR,IY)
          T2(90,IR,IY,IS)=QJFTR(IR,IY)                       !  All sectors - don't subtract QJFBS
          T2(91,IR,IY,IS)=QLGAS(IR,IY)
          T2(92,IR,IY,IS)=QMGAS(IR,IY) + QETTR(IR,IY) + QMETR(IR,IY)
          T2(93,IR,IY,IS)=QPFIN(IR,IY)
          T2(94,IR,IY,IS)=QRSAS(IR,IY)
          T2(95,IR,IY,IS)=QOTAS(IR,IY) + QSGIN(IR,IY) + QPCIN(IR,IY) + QASIN(IR,IY)
          T2(96,IR,IY,IS)=FSUM(T2(88,IR,IY,IS),8)
          T2(118,IR,IY,IS)=QNGAS(IR,IY) - QHYTR(IR,IY) / .7 - QGTLRF(IR,IY)
          T2(97,IR,IY,IS)=T2(118,IR,IY,IS) + T2(120,IR,IY,IS) + T2(128,IR,IY,IS) +   &
                          T2(116,IR,IY,IS) + T2( 52,IR,IY,IS) + T2( 49,IR,IY,IS)
          T2(98,IR,IY,IS)=QMCIN(IR,IY)
! for coal, subtract out coal gasification coal, but leave in losses
! this means we subtract the gas production number
          T2(99,IR,IY,IS)=QCLAS(IR,IY)-QCTLRF(IR,IY) - OGSUPGAS(1,IR,IY) * CFNGC(IY) * .001
          T2(100,IR,IY,IS)=QCIIN(IR,IY)
          T2(101,IR,IY,IS)=FSUM(T2(98,IR,IY,IS),3) + T2(119,IR,IY,IS)
          T2(102,IR,IY,IS)=QUREL(IR,IY)
!  subtract nonbiogenic from msw/renew and add to total summed line
          T2(103,IR,IY,IS)=QBMRS(IR,IY) + QBMCM(IR,IY) + QTRIN(IR,IY) - QBMRF(IR,IY) + QTRSN(IR,IY) + &
                           QTREL(IR,IY) - WNCMSEL(IY,IR)
          T2(105,IR,IY,IS)=QHYTR(IR,IY)
          T2(123,IR,IY,IS)=WNCMSEL(IY,IR)
          T2(104,IR,IY,IS)=QEIEL(IR,IY)
          T2(106,IR,IY,IS)= FSUM(T2(96,IR,IY,IS),2) + FSUM(T2(101,IR,IY,IS),5) + &
                                 T2(73,IR,IY,IS) + T2(123,IR,IY,IS)
! RELATED STATISTICS;  use Energy use numbers from earlier in Table
          IF (MOD(IY+1989,4).EQ.0) RDAYS = 366.
          RDAYS = 365.
          T2(121,IR,IY,IS)=ETHTOTCD(IR,IY) * RDAYS * CFETQ(IY) / 1000000.
          T2(110,IR,IY,IS)=MC_NP(IR,IY)
          T2(111,IR,IY,IS)=MC_GDPR(IY)
          T2(112,IR,IY,IS) = sum(em_resd(1:iel_R,IR,ICY)) + sum(em_comm(1:iel_C,IR,ICY)) + &
                             sum(em_indy(1:iel_I,IR,ICY)) + sum(em_tran(1:iel_T,IR,ICY))
          T2(107,IR,IY,IS) = sum(GHG_OGHG(iy,1:14))
          IF (T2(111,IR,IY,IS) .NE. 0.0) THEN
            T2(113,IR,IY,IS) = T2(106,IR,IY,IS) / (T2(111,IR,IY,IS) / 1000.0)
            T2(114,IR,IY,IS) = T2(112,IR,IY,IS) / (T2(111,IR,IY,IS) / 1000.0)
            T2(108,IR,IY,IS) =(T2(112,IR,IY,IS)+T2(107,IR,IY,IS))/(T2(111,IR,IY,IS)/1000.0)
          ENDIF
          IF (T2(108,IR,13,IS) .NE. 0.0 .AND. IY .GE. 13) &
             T2(109,IR,IY,IS) =(1 - T2(108,IR,IY,IS) / T2(108,IR,13,IS))* 100.0
20    CONTINUE

! TABLE 3.  (REGIONAL) PRICE OF ENERGY BY SOURCE AND END-USE SECTOR

      DO 30 IY=1,LASTYR
      DO 30 IR=1,11
          IF (IR .EQ. 10) CYCLE
! RESIDENTIAL
      IF ((QTPRS(IR,IY)+QNGRS(IR,IY)+QCLRS(IR,IY)+QELRS(IR,IY)) .NE. 0.0) &
          T3(1,IR,IY,IS)= &
            (PTPRS(IR,IY)*QTPRS(IR,IY)+PNGRS(IR,IY)*QNGRS(IR,IY)+ &
             PCLRS(IR,IY)*QCLRS(IR,IY)+PELRS(IR,IY)*QELRS(IR,IY)) / &
            (QTPRS(IR,IY)+QNGRS(IR,IY)+QCLRS(IR,IY)+QELRS(IR,IY))
      IF ((QTPRS(IR,IY)+QNGRS(IR,IY)+QCLRS(IR,IY)) .NE. 0.0) &
          T3(2,IR,IY,IS)= &
            (PTPRS(IR,IY)*QTPRS(IR,IY)+PNGRS(IR,IY)*QNGRS(IR,IY)+ &
             PCLRS(IR,IY)*QCLRS(IR,IY)) / &
            (QTPRS(IR,IY)+QNGRS(IR,IY)+QCLRS(IR,IY))
          T3(3,IR,IY,IS)=PTPRS(IR,IY)
          T3(4,IR,IY,IS)=PDSRS(IR,IY)
          T3(5,IR,IY,IS)=PPRRS(IR,IY)
          T3(6,IR,IY,IS)=PNGRS(IR,IY)
          T3(7,IR,IY,IS)=PELRS(IR,IY)
! COMMERCIAL
      IF ((QTPCM(IR,IY)+QNGCM(IR,IY)+QCLCM(IR,IY)+QELCM(IR,IY)) .NE. 0.0) &
          T3( 8,IR,IY,IS)= &
            (PTPCM(IR,IY)*QTPCM(IR,IY)+PNGCM(IR,IY)*QNGCM(IR,IY)+ &
             PCLCM(IR,IY)*QCLCM(IR,IY)+PELCM(IR,IY)*QELCM(IR,IY)) / &
            (QTPCM(IR,IY)+QNGCM(IR,IY)+QCLCM(IR,IY)+QELCM(IR,IY))
      IF ((QTPCM(IR,IY)+QNGCM(IR,IY)+QCLCM(IR,IY)) .NE. 0.0) &
          T3( 9,IR,IY,IS)= &
            (PTPCM(IR,IY)*QTPCM(IR,IY)+PNGCM(IR,IY)*QNGCM(IR,IY)+ &
             PCLCM(IR,IY)*QCLCM(IR,IY)) / &
            (QTPCM(IR,IY)+QNGCM(IR,IY)+QCLCM(IR,IY))
          T3(10,IR,IY,IS)=PTPCM(IR,IY)
          T3(125,IR,IY,IS)=PPRCM(IR,IY)
          T3(11,IR,IY,IS)=PDSCM(IR,IY)
          T3(12,IR,IY,IS)=PRSCM(IR,IY)
          T3(13,IR,IY,IS)=PNGCM(IR,IY)
          T3(14,IR,IY,IS)=PELCM(IR,IY)
! INDUSTRIAL (PRICES AND QUANTITIES INCLUDE REFINERY)
      IF((QTPIN(IR,IY)+QNGIN(IR,IY)+QCLIN(IR,IY)+QELIN(IR,IY)+ &
          QMCIN(IR,IY)) .NE. 0.0)          T3(15,IR,IY,IS)= &
            (PTPIN(IR,IY)*QTPIN(IR,IY)+PNGIN(IR,IY)*QNGIN(IR,IY)+ &
             PCLIN(IR,IY)*(QCLIN(IR,IY)-QCTLRF(IR,IY))+PELIN(IR,IY)*QELIN(IR,IY)+ &
             PCLSN(IR,IY)*QCTLRF(IR,IY) + &
             PMCIN(IR,IY)*QMCIN(IR,IY)) / (QTPIN(IR,IY)+ &
             QNGIN(IR,IY)+QCLIN(IR,IY)+QELIN(IR,IY)+QMCIN(IR,IY))
      IF((QTPIN(IR,IY)+QNGIN(IR,IY)+QCLIN(IR,IY)+QMCIN(IR,IY)) .NE. 0.0) &
          T3(16,IR,IY,IS)= &
            (PTPIN(IR,IY)*QTPIN(IR,IY)+PNGIN(IR,IY)*QNGIN(IR,IY)+ &
             PCLIN(IR,IY)*(QCLIN(IR,IY)-QCTLRF(IR,IY))+ &
             PCLSN(IR,IY)*QCTLRF(IR,IY) + &
             PMCIN(IR,IY)*QMCIN(IR,IY)) / (QTPIN(IR,IY)+ &
             QNGIN(IR,IY)+QCLIN(IR,IY)+QMCIN(IR,IY))
          T3(17,IR,IY,IS)=PTPIN(IR,IY)
          T3(18,IR,IY,IS)=PDSIN(IR,IY)
! do this if we want an average including the feedstock price:
          IF(QLGIN(IR,IY) .NE. 0.0) &
          T3(19,IR,IY,IS)=(PLGIN(IR,IY)*(QLGIN(IR,IY)-INQLGPF(IR,IY)/1000.) + &
                           PLGINPF(IR,IY)*INQLGPF(IR,IY)/1000.) / QLGIN(IR,IY)
! but we don't, so do this, which is just the heat and power price:
          T3(19,IR,IY,IS)=PLGIN(IR,IY)
! but it is simpler (the row in now labelled "propane") if we just use the propane price variable:
          T3(19,IR,IY,IS)=PPRIN(IR,IY)
          T3(20,IR,IY,IS)=PRSIN(IR,IY)
          T3(21,IR,IY,IS)=PNGIN(IR,IY)
          T3(22,IR,IY,IS)=PMCIN(IR,IY)
          T3(23,IR,IY,IS)=PCLIN(IR,IY)
          T3(65,IR,IY,IS)=PCLSN(IR,IY)
          T3(24,IR,IY,IS)=PELIN(IR,IY)
          T3(46,IR,IY,IS)=0.0  !  PBMRFET(IR,IY)
! TRANSPORTATION
       IF ((QTPTR(IR,IY)-QDSBS(IR,IY)-QJFBS(IR,IY)-QMGBS(IR,IY)+QNGTR(IR,IY)+QELTR(IR,IY)+QETTR(IR,IY)).NE. 0.0) &
          T3(25,IR,IY,IS)= &
            (PTPTR(IR,IY)*(QTPTR(IR,IY)-QDSBS(IR,IY)-QJFBS(IR,IY)-QMGBS(IR,IY))+ &
             PNGTR(IR,IY)*QNGTR(IR,IY)+ &
             PELTR(IR,IY)*QELTR(IR,IY)+PETTR(IR,IY)*QTRTR(IR,IY))/ &
            (QTPTR(IR,IY)-QDSBS(IR,IY)-QJFBS(IR,IY)-QMGBS(IR,IY)+ &
             QNGTR(IR,IY)+QELTR(IR,IY)+QETTR(IR,IY))
       IF ((QTPTR(IR,IY)-QDSBS(IR,IY)-QJFBS(IR,IY)-QMGBS(IR,IY)+QNGTR(IR,IY)+QETTR(IR,IY)) .NE. 0.0) &
          T3(26,IR,IY,IS)= &
            (PTPTR(IR,IY)*(QTPTR(IR,IY)-QDSBS(IR,IY)-QJFBS(IR,IY)-QMGBS(IR,IY))+ &
             PNGTR(IR,IY)*QNGTR(IR,IY)+ &
             PETTR(IR,IY)*QETTR(IR,IY))/ &
            (QTPTR(IR,IY)-QDSBS(IR,IY)-QJFBS(IR,IY)-QMGBS(IR,IY)+QNGTR(IR,IY)+QETTR(IR,IY))
       IF ((QTPTR(IR,IY)-QDSBS(IR,IY)-QJFBS(IR,IY)-QMGBS(IR,IY)) .NE. 0.0) &
          T3(27,IR,IY,IS)=(PTPTR(IR,IY)*QTPTR(IR,IY)-PDSTR(IR,IY)*QDSBS(IR,IY)- &
                           PJFTR(IR,IY)*QJFBS(IR,IY)-PMGTR(IR,IY)*QMGBS(IR,IY))/ &
                          (QTPTR(IR,IY)-QDSBS(IR,IY)-QJFBS(IR,IY)-QMGBS(IR,IY))
          T3(28,IR,IY,IS)=PDSTRHWY(IR,IY)
          T3(29,IR,IY,IS)=PJFTR(IR,IY)
          T3(30,IR,IY,IS)=PMGTR(IR,IY)
          T3(31,IR,IY,IS)=PRSTR(IR,IY)
          T3(32,IR,IY,IS)=PPRTR(IR,IY)
          T3(33,IR,IY,IS)=PNGTR(IR,IY)
          T3(34,IR,IY,IS)=PETTR(IR,IY)
          T3(36,IR,IY,IS)=PELTR(IR,IY)
!  Total End-Use Energy
        DENOM = (QTPRS(IR,IY)+QNGRS(IR,IY)+QCLRS(IR,IY)+QELRS(IR,IY)+ &
         QTPCM(IR,IY)+QNGCM(IR,IY)+QCLCM(IR,IY)+QELCM(IR,IY)+ &
         QTPIN(IR,IY)+QNGIN(IR,IY)+QCLIN(IR,IY)+QELIN(IR,IY)+ &
         QMCIN(IR,IY)+QTPTR(IR,IY)+QNGTR(IR,IY)+QELTR(IR,IY)+QETTR(IR,IY))
        IF (DENOM .NE. 0.0) T3(37,IR,IY,IS)= &
         (T3( 1,IR,IY,IS)*(QTPRS(IR,IY)+QNGRS(IR,IY)+QCLRS(IR,IY)+QELRS(IR,IY))+ &
          T3( 8,IR,IY,IS)*(QTPCM(IR,IY)+QNGCM(IR,IY)+QCLCM(IR,IY)+QELCM(IR,IY))+ &
          T3(15,IR,IY,IS)*(QTPIN(IR,IY)+QNGIN(IR,IY)+QCLIN(IR,IY)+ &
                           QELIN(IR,IY)+QMCIN(IR,IY))+ &
          T3(25,IR,IY,IS)*(QTPTR(IR,IY)+QNGTR(IR,IY)+QELTR(IR,IY)+QETTR(IR,IY)))/ DENOM
        DENOM = (QTPRS(IR,IY)+QNGRS(IR,IY)+QCLRS(IR,IY)+ &
                 QTPCM(IR,IY)+QNGCM(IR,IY)+QCLCM(IR,IY)+ &
                 QTPIN(IR,IY)+QNGIN(IR,IY)+QCLIN(IR,IY)+QMCIN(IR,IY)+ &
                 QTPTR(IR,IY)+QNGTR(IR,IY)+QETTR(IR,IY))
       IF (DENOM .NE. 0.0) T3(38,IR,IY,IS)= &
         (T3( 2,IR,IY,IS)*(QTPRS(IR,IY)+QNGRS(IR,IY)+QCLRS(IR,IY))+ &
          T3( 9,IR,IY,IS)*(QTPCM(IR,IY)+QNGCM(IR,IY)+QCLCM(IR,IY))+ &
          T3(16,IR,IY,IS)*(QTPIN(IR,IY)+QNGIN(IR,IY)+QCLIN(IR,IY)+ QMCIN(IR,IY))+ &
          T3(26,IR,IY,IS)*(QTPTR(IR,IY)+QNGTR(IR,IY)+QETTR(IR,IY)))/ DENOM
          T3(39,IR,IY,IS)=PELAS(IR,IY)
!  ELEC UTILITIES
          IF((QTPEL(IR,IY)+QNGEL(IR,IY)+QCLEL(IR,IY)) .NE. 0.0) &
          T3(40,IR,IY,IS)=(PTPEL(IR,IY)*QTPEL(IR,IY)+ &
            PNGEL(IR,IY)*QNGEL(IR,IY)+PCLEL(IR,IY)*QCLEL(IR,IY)) / &
           (QTPEL(IR,IY)+QNGEL(IR,IY)+QCLEL(IR,IY))
          T3(41,IR,IY,IS)=PTPEL(IR,IY)
          T3(42,IR,IY,IS)=PDSEL(IR,IY)
          T3(43,IR,IY,IS)=PRSEL(IR,IY)
          T3(44,IR,IY,IS)=PNGEL(IR,IY)
          T3(45,IR,IY,IS)=PCLEL(IR,IY)
          T3(35,IR,IY,IS)=PUREL(IR,IY)
          T3(64,IR,IY,IS)=UPRWDCR(IR,IY)
!  ALL USERS
!          T3(46,IR,IY,IS)=PTPAS(IR,IY)   ! using T3(46 for PBMET above
          T3(47,IR,IY,IS)=PDSAS(IR,IY)
          T3(48,IR,IY,IS)=PJFTR(IR,IY)
! this includes the feedstock price:
          T3(49,IR,IY,IS)=PLGAS(IR,IY)
! calculated based on the sectoral prices shown (as stated in the footnote):
          IF (QPRRS(IR,IY)+QPRCM(IR,IY)+QPRIN(IR,IY)-QPRINPF(IR,IY)+QPRTR(IR,IY) .NE. 0.0) &
          T3(49,IR,IY,IS)= &
             (QPRRS(IR,IY)*PPRRS(IR,IY)+QPRCM(IR,IY)*PPRCM(IR,IY)+ &
             (QPRIN(IR,IY)-QPRINPF(IR,IY))*PPRIN(IR,IY)+QPRTR(IR,IY)*PPRTR(IR,IY)) / &
             (QPRRS(IR,IY)+QPRCM(IR,IY)+QPRIN(IR,IY)-QPRINPF(IR,IY)+QPRTR(IR,IY))
          T3(50,IR,IY,IS)=PMGAS(IR,IY)
          T3(51,IR,IY,IS)=PRSAS(IR,IY)
          T3(52,IR,IY,IS)=PNGAS(IR,IY)
          T3(53,IR,IY,IS)=PCLAS(IR,IY)
          T3(54,IR,IY,IS)=PETTR(IR,IY)
          T3(56,IR,IY,IS)=PELAS(IR,IY)
!         --- EXPENDITURES BY SECTOR
          T3(57,IR,IY,IS) = 0.
          T3(58,IR,IY,IS) = 0.
          T3(59,IR,IY,IS) = 0.
          T3(60,IR,IY,IS) = 0.
          T3(61,IR,IY,IS) = 0.
          T3(62,IR,IY,IS) = 0.
          T3(63,IR,IY,IS) = 0.
          T3(57,IR,IY,IS) =  &
             (PTPRS(IR,IY)*QTPRS(IR,IY) + PNGRS(IR,IY)*QNGRS(IR,IY) + &
              PCLRS(IR,IY)*QCLRS(IR,IY) + PELRS(IR,IY)*QELRS(IR,IY))
          T3(58,IR,IY,IS) = &
             (PTPCM(IR,IY)*QTPCM(IR,IY) + PNGCM(IR,IY)*QNGCM(IR,IY) + &
              PCLCM(IR,IY)*QCLCM(IR,IY) + PELCM(IR,IY)*QELCM(IR,IY))
          T3(59,IR,IY,IS) = PTPIN(IR,IY)*(QTPIN(IR,IY)-QTPRF(IR,IY))+ &
              PNGIN(IR,IY)*(QNGIN(IR,IY)-QNGRF(IR,IY)) + &
              PCLIN(IR,IY)*(QCLIN(IR,IY)-QCTLRF(IR,IY)) + &
              PELIN(IR,IY)*(QELIN(IR,IY)-QELRF(IR,IY)) + &
              PMCIN(IR,IY)*QMCIN(IR,IY) + PMCIN(IR,IY)*QCIIN(IR,IY)
          T3(60,IR,IY,IS)= (PTPTR(IR,IY)*QTPTR(IR,IY) + &
              PNGTR(IR,IY)*QNGTR(IR,IY) + PELTR(IR,IY)*QELTR(IR,IY) + &
              PMETR(IR,IY)*QMETR(IR,IY))
          T3(61,IR,IY,IS) = T3(57,IR,IY,IS) + T3(58,IR,IY,IS) + &
                            T3(59,IR,IY,IS) + T3(60,IR,IY,IS)
          T3(62,IR,IY,IS) = PETTR(IR,IY)*QTRTR(IR,IY)
          T3(63,IR,IY,IS) = T3(61,IR,IY,IS) + T3(62,IR,IY,IS)
! nominal prices
          T3(66,IR,IY,IS) = T3( 5,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(67,IR,IY,IS) = T3( 4,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(68,IR,IY,IS) = T3( 6,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(69,IR,IY,IS) = T3( 7,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(70,IR,IY,IS) = T3(11,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(71,IR,IY,IS) = T3(12,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(72,IR,IY,IS) = T3(13,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(73,IR,IY,IS) = T3(14,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(74,IR,IY,IS) = T3(19,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(75,IR,IY,IS) = T3(18,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(76,IR,IY,IS) = T3(20,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(77,IR,IY,IS) = T3(21,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(78,IR,IY,IS) = T3(22,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(79,IR,IY,IS) = T3(23,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(80,IR,IY,IS) = T3(65,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(81,IR,IY,IS) = T3(24,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(82,IR,IY,IS) = T3(46,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(83,IR,IY,IS) = T3(32,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(84,IR,IY,IS) = T3(34,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(85,IR,IY,IS) = T3(30,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(86,IR,IY,IS) = T3(29,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(87,IR,IY,IS) = T3(28,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(88,IR,IY,IS) = T3(31,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(89,IR,IY,IS) = T3(33,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(90,IR,IY,IS) = T3(36,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(91,IR,IY,IS) = T3(42,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(92,IR,IY,IS) = T3(43,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(93,IR,IY,IS) = T3(44,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(94,IR,IY,IS) = T3(45,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(55,IR,IY,IS) = T3(35,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(95,IR,IY,IS) = T3(64,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(96,IR,IY,IS) = T3(49,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(97,IR,IY,IS) = T3(54,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(98,IR,IY,IS) = T3(50,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(99,IR,IY,IS) = T3(48,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(100,IR,IY,IS) = T3(47,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(101,IR,IY,IS) = T3(51,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(102,IR,IY,IS) = T3(52,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(103,IR,IY,IS) = T3(22,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(104,IR,IY,IS) = T3(53,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(105,IR,IY,IS) = T3(65,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(106,IR,IY,IS) = T3(56,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(107,IR,IY,IS) = T3(57,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(108,IR,IY,IS) = T3(58,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(109,IR,IY,IS) = T3(59,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(110,IR,IY,IS) = T3(60,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(111,IR,IY,IS) = T3(61,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(112,IR,IY,IS) = T3(62,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(113,IR,IY,IS) = T3(63,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(114,IR,IY,IS) = T3( 1,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(115,IR,IY,IS) = T3( 8,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(116,IR,IY,IS) = T3(15,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(117,IR,IY,IS) = T3(25,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(118,IR,IY,IS) = T3(37,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(119,IR,IY,IS) = T3( 2,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(120,IR,IY,IS) = T3( 9,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(121,IR,IY,IS) = T3(16,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(122,IR,IY,IS) = T3(26,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(123,IR,IY,IS) = T3(38,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(124,IR,IY,IS) = T3(40,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
          T3(126,IR,IY,IS) = T3(125,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
30    CONTINUE

! TABLE 4 RESIDENTIAL SECTOR KEY INDICATORS AND END-USE CONSUMPTION
      DO 40 IY=RECSYEAR-BASEYR+1,LASTYR
!       --- NUMBER OF HOUSEHOLDS
        T4(1,IY,IS)= (RSEH(IY,1) + RSNH(IY,1)) / 1000000.
        T4(2,IY,IS)= (RSEH(IY,2) + RSNH(IY,2)) / 1000000.
        T4(3,IY,IS)= (RSEH(IY,3) + RSNH(IY,3)) / 1000000.
        T4(4,IY,IS) = FSUM(T4(1,IY,IS),3)
!       --- AVERAGE SQUARE FOOTAGE
        T4(5,IY,IS) = SQFTAVG(IY)
!       --- CONSUMPTION PER HOUSEHOLD
        IF (T4(4,IY,IS) .NE. 0.0) THEN
          T4(6,IY,IS) = 0.0
          DO LOOP1 = 1,8      !FUEL - SPACE HEATING
            T4(6,IY,IS) = T4(6,IY,IS) + RSHTRCON(IY,LOOP1)
          END DO
          T4(6,IY,IS) = T4(6,IY,IS) + &
            (RSREFCON(IY) + RSFRZCON(IY) + RSLTCON(IY) + RSCSWCON(IY) + &
             RSDSWCON(IY) + RSTVRCON(IY) + RSPCRCON(IY) + RSFANCON(IY))
          DO LOOP1 = 1,3      !FUEL - SPACE COOLING
            T4(6,IY,IS) = T4(6,IY,IS) + RSCOOLCN(IY,LOOP1)
          END DO
          DO LOOP1 = 1,4      !FUEL - APPLIANCES
            T4(6,IY,IS) = T4(6,IY,IS) + RSAPCON(IY,LOOP1)
          END DO
          DO LOOP1 = 1,5      !FUEL - HOT WATER
            T4(6,IY,IS) = T4(6,IY,IS) + RSH2OCON(IY,LOOP1)
          END DO
          DO LOOP1 = 1,2      !FUEL - CLOTHES DRYERS
            T4(6,IY,IS) = T4(6,IY,IS) + RSDRYCON(IY,LOOP1)
          END DO
          DO LOOP1 = 1,3      !FUEL - COOKING
            T4(6,IY,IS) = T4(6,IY,IS) + RSCKCON(IY,LOOP1)
          END DO
!                             !REMOVE NON-MARKETED RENEWABLES
          T4(6,IY,IS) = T4(6,IY,IS)   - RSHTRCON(IY,7) - &
                RSCOOLCN(IY,2) - RSH2OCON(IY,5)
          T4(6,IY,IS) = T4(6,IY,IS) / T4(4,IY,IS) / 1000000.
          T4(7,IY,IS) = T4(6,IY,IS) - (CGRESGEN(11,IY,3,2) + &
						CGRESGEN(11,IY,8,2) + CGRESGEN(11,IY,11,2)) * &
						3412. / (T4(4,IY,IS) * 1000000.)
          T4(76,IY,IS) = T4(6,IY,IS) / T4(5,IY,IS) * 1000.
          T4(77,IY,IS) = T4(7,IY,IS) / T4(5,IY,IS) * 1000.
        ENDIF
!        --- ELECTRICITY
        T4( 9,IY,IS) = RSHTRCON(IY,2) / 1000000000.
        T4(10,IY,IS) = RSCOOLCN(IY,1) / 1000000000.
        T4(11,IY,IS) = RSH2OCON(IY,2) / 1000000000.
        T4(12,IY,IS) = RSREFCON(IY) / 1000000000.
        T4(13,IY,IS) = RSCKCON(IY,3) / 1000000000.
        T4(14,IY,IS) = RSDRYCON(IY,2) / 1000000000.
        T4(15,IY,IS) = RSFRZCON(IY) / 1000000000.
        T4(16,IY,IS) = RSLTCON(IY) / 1000000000.
        T4(17,IY,IS) = RSCSWCON(IY) / 1000000000.
        T4(18,IY,IS) = RSDSWCON(IY) / 1000000000.
        T4(19,IY,IS) = RSTVRCON(IY) / 1000000000.
        T4(20,IY,IS) = RSPCRCON(IY) / 1000000000.
        T4(21,IY,IS) = RSFANCON(IY) / 1000000000.
        T4(22,IY,IS) = RSAPCON(IY,2) / 1000000000.
        T4(23,IY,IS) = SUM(T4(9:22,IY,IS))   ! Gross subtotal
        T4(109,IY,IS) = (CGRESGEN(11,IY,3,2) + CGRESGEN(11,IY,8,2) + & 
						CGRESGEN(11,IY,11,2)) * 3.412 * 0.000001 ! Own-use distributed Generation
		  T4(110,IY,IS) = T4(23,IY,IS) - T4(109,IY,IS) ! Total net of DG
!       --- NATURAL GAS
        T4(24,IY,IS) = RSHTRCON(IY,1) / 1000000000.
        T4(25,IY,IS) = RSCOOLCN(IY,3) / 1000000000.
        T4(26,IY,IS) = RSH2OCON(IY,1) / 1000000000.
        T4(27,IY,IS) = RSCKCON(IY,1) / 1000000000.
        T4(28,IY,IS) = RSDRYCON(IY,1) / 1000000000.
        T4(29,IY,IS) = RSAPCON(IY,1) / 1000000000.
        T4(30,IY,IS) = QNGRS(11,IY)    ! TOTAL
!        --- DISTILLATE
        T4(31,IY,IS) = RSHTRCON(IY,3) / 1000000000.
        T4(32,IY,IS) = RSH2OCON(IY,3) / 1000000000.
        T4(33,IY,IS) = RSAPCON(IY,3) / 1000000000.
        T4(34,IY,IS) = QDSRS(11,IY)    ! TOTAL
!       --- LIQUEFIED PETROLEUM GAS
        T4(35,IY,IS) = RSHTRCON(IY,4) / 1000000000.
        T4(36,IY,IS) = RSH2OCON(IY,4) / 1000000000.
        T4(37,IY,IS) = RSCKCON(IY,2) / 1000000000.
        T4(38,IY,IS) = RSAPCON(IY,4) / 1000000000.
        T4(39,IY,IS) = QLGRS(11,IY)    ! TOTAL
!      --- MARKETED RENEWABLES
        T4(40,IY,IS) = RSHTRCON(IY,6) / 1000000000.
!      --- OTHER FUELS
        T4(41,IY,IS)= QKSRS(11,IY) + QCLRS(11,IY) ! KEROSENE + COAL
!       --- DELIVERED ENERGY CONSUMPTION BY END-USE
        T4(42,IY,IS) = (RSHTRCON(IY,1) + RSHTRCON(IY,2) + RSHTRCON(IY,3) + &
                        RSHTRCON(IY,4) + RSHTRCON(IY,5) + RSHTRCON(IY,6) + &
                        RSHTRCON(IY,8)) / 1000000000.
        T4(43,IY,IS) = (RSCOOLCN(IY,1) + RSCOOLCN(IY,3)) / 1000000000.
        T4(44,IY,IS) = (RSH2OCON(IY,1) + RSH2OCON(IY,2) + RSH2OCON(IY,3) + &
                        RSH2OCON(IY,4)) / 1000000000.
        T4(45,IY,IS) =  RSREFCON(IY) / 1000000000.
        T4(46,IY,IS) = (RSCKCON(IY,1) + RSCKCON(IY,2) + &
                        RSCKCON(IY,3)) / 1000000000.
        T4(47,IY,IS) = (RSDRYCON(IY,1) + RSDRYCON(IY,2)) / 1000000000.
        T4(48,IY,IS) =  RSFRZCON(IY) / 1000000000.
        T4(49,IY,IS) =  RSLTCON(IY) / 1000000000.
        T4(50,IY,IS) =  RSCSWCON(IY) / 1000000000.
        T4(51,IY,IS) =  RSDSWCON(IY) / 1000000000.
        T4(52,IY,IS) =  RSTVRCON(IY) / 1000000000.
        T4(53,IY,IS) =  RSPCRCON(IY) / 1000000000.
        T4(54,IY,IS) =  RSFANCON(IY) / 1000000000.
        T4(55,IY,IS) = (RSAPCON(IY,1) + RSAPCON(IY,2) + RSAPCON(IY,3) + &
                        RSAPCON(IY,4)) / 1000000000.
        T4(56,IY,IS) = FSUM(T4(42,IY,IS),14)    ! Gross subtotal
		  T4(111,IY,IS) =(CGRESGEN(11,IY,3,2) + CGRESGEN(11,IY,8,2) + & 
						CGRESGEN(11,IY,11,2)) * 3.412 * 0.000001 ! Own-use distributed Generation
		  T4(112,IY,IS) = T4(56,IY,IS) -  T4(111,IY,IS) ! Total net of DG

!     --- ELECTRICITY RELATED LOSSES BY END-USE
        ELECLOSS = QTSEL(11,IY) - QELAS(11,IY)
        T4(57,IY,IS) = QELRS(11,IY)  / QELAS(11,IY) *ELECLOSS
!       --- TOTAL ENERGY CONSUMPTION BY END-USE
        T4(58,IY,IS) =   (RSHTRCON(IY,1) + RSHTRCON(IY,2) + &
         RSHTRCON(IY,3) + RSHTRCON(IY,4) + RSHTRCON(IY,5) + &
         RSHTRCON(IY,6) + RSHTRCON(IY,8)) / 1000000000.+ &
         RSHTRCON(IY,2) / 1000000000. / QELAS(11,IY) * ELECLOSS
        T4(59,IY,IS) = (RSCOOLCN(IY,1) + RSCOOLCN(IY,3)) / 1000000000. + &
                        RSCOOLCN(IY,1) / 1000000000. / QELAS(11,IY) * ELECLOSS
        T4(60,IY,IS) = (RSH2OCON(IY,1) + RSH2OCON(IY,2) + &
              RSH2OCON(IY,3) + RSH2OCON(IY,4)) / 1000000000. + &
              RSH2OCON(IY,2) / 1000000000. / QELAS(11,IY) * ELECLOSS
        T4(61,IY,IS) =  RSREFCON(IY) / 1000000000. + &
              RSREFCON(IY) / 1000000000. / QELAS(11,IY)* ELECLOSS
        T4(62,IY,IS) = (RSCKCON(IY,1) + RSCKCON(IY,2) + RSCKCON(IY,3)) / &
          1000000000. + RSCKCON(IY,3) / 1000000000. / QELAS(11,IY) * ELECLOSS
        T4(63,IY,IS) = (RSDRYCON(IY,1) + RSDRYCON(IY,2)) / 1000000000. + &
                        RSDRYCON(IY,2) / 1000000000. / QELAS(11,IY) * ELECLOSS
        T4(64,IY,IS) =  RSFRZCON(IY) / 1000000000. + &
                        RSFRZCON(IY) / 1000000000. / QELAS(11,IY) * ELECLOSS
        T4(65,IY,IS) =  RSLTCON(IY) / 1000000000. + &
                        RSLTCON(IY) / 1000000000. / QELAS(11,IY) * ELECLOSS
        T4(66,IY,IS) =  RSCSWCON(IY) / 1000000000. + &
                        RSCSWCON(IY) / 1000000000. / QELAS(11,IY) * ELECLOSS
        T4(67,IY,IS) =  RSDSWCON(IY) / 1000000000. + &
                        RSDSWCON(IY) / 1000000000. / QELAS(11,IY) * ELECLOSS
        T4(68,IY,IS) =  RSTVRCON(IY) / 1000000000. + &
                        RSTVRCON(IY) / 1000000000. / QELAS(11,IY) * ELECLOSS
        T4(69,IY,IS) =  RSPCRCON(IY) / 1000000000. + &
                        RSPCRCON(IY) / 1000000000. / QELAS(11,IY) * ELECLOSS
        T4(70,IY,IS) =  RSFANCON(IY) / 1000000000. + &
                        RSFANCON(IY) / 1000000000. / QELAS(11,IY) * ELECLOSS
        T4(71,IY,IS) = (RSAPCON(IY,1) + RSAPCON(IY,2) + RSAPCON(IY,3) + RSAPCON(IY,4)) / &
                        1000000000. + RSAPCON(IY,2) / 1000000000. / QELAS(11,IY) * ELECLOSS
        T4(72,IY,IS) = SUM(T4(58:71,IY,IS)) ! Gross subtotal
		  T4(113,IY,IS) =(CGRESGEN(11,IY,3,2) + CGRESGEN(11,IY,8,2) + & 
						CGRESGEN(11,IY,11,2)) * 3.412 * 0.000001 ! Own-use distributed Generation
		  T4(114,IY,IS) = T4(72,IY,IS) -  T4(113,IY,IS) ! Total net of DG

!       --- NON-MARKETED RENEWABLES
        T4(73,IY,IS) = (RSHTRCON(IY,7) + RSCOOLCN(IY,2)) / 1000000000.
        T4(74,IY,IS) = RSH2OCON(IY,5) / 1000000000.
        T4( 8,IY,IS) = QPVRS(11,IY)
        T4(75,IY,IS) = CGRESQ(11,IY,11) / 1000.
        T4(78,IY,IS) =  FSUM(T4(73,IY,IS),3) + T4( 8,IY,IS)

        T4(79:87,IY,IS) = HDDADJ(IY+1989,1:9)
        T4(88,IY,IS) = HDDADJ(IY+1989,11)
        T4(89:97,IY,IS) = CDDADJ(IY+1989,1:9)
        T4(98,IY,IS) = CDDADJ(IY+1989,11)
        T4( 99,IY,IS) = PELSHRS(11,IY)
        T4(100,IY,IS) = PELCLRS(11,IY)
        T4(101,IY,IS) = PELWHRS(11,IY)
        T4(102,IY,IS) = PELCKRS(11,IY)
        T4(103,IY,IS) = PELCDRS(11,IY)
        T4(104,IY,IS) = PELRFRS(11,IY)
        T4(105,IY,IS) = PELFZRS(11,IY)
        T4(106,IY,IS) = PELLTRS(11,IY)
        T4(107,IY,IS) = PELOTRS(11,IY)
        T4(108,IY,IS) = PELH2RS(11,IY)


        C_CO2_FACTOR = 1.
        IF (TRIM(CARBON_OR_2) .EQ. 'CO2') C_CO2_FACTOR = 44. / 12.
!
! emission factors only defined from 95 on, so use 95 for prior years
        iyy=max(iy,6)
!Space Heating
         c8(1)=engrs(iyy)   * C_CO2_FACTOR
         c8(2)=fsum(emel(1,1,iy),4)/qelas(11,iy)
         c8(3)=edsrs(iyy)   * C_CO2_FACTOR
         c8(4)=elgrs(iyy)   * C_CO2_FACTOR
         c8(5)=eksrs(iyy)  * C_CO2_FACTOR
         c8(6:7)=0.
         c8(8)=eclrs(iyy) * C_CO2_FACTOR
        T22( 1,IY,IS)=dot_product(RSHTRCON(iy,1:8),C8(1:8))
!Space Cooling
        c8=0.
        c8(1)=fsum(emel(1,1,iy),4)/qelas(11,iy)
        c8(3)=engrs(iyy)   * C_CO2_FACTOR
        T22( 2,IY,IS)=dot_product(RSCOOLCN(iy,1:3),C8(1:3))
!Water Heating
        c8=0.
        c8(1)=engrs(iyy)   * C_CO2_FACTOR
        c8(2)=fsum(emel(1,1,iy),4)/qelas(11,iy)
        c8(3)=edsrs(iyy)   * C_CO2_FACTOR
        c8(4)=elgrs(iyy)   * C_CO2_FACTOR
        T22( 3,IY,IS)=dot_product(RSH2OCON(iy,1:4),C8(1:4))
!Other Uses 6/
        T22(14,IY,IS)=dot_product(RSAPCON(iy,1:4),C8(1:4))
!Refrigeration
        T22( 4,IY,IS)=RSREFCON(IY)*c8(2)
!Cooking
        c8=0.
        c8(1)=engrs(iyy)   * C_CO2_FACTOR
        c8(2)=elgrs(iyy)   * C_CO2_FACTOR
        c8(3)=fsum(emel(1,1,iy),4)/qelas(11,iy)
        T22( 5,IY,IS)=dot_product(RSCKCON(iy,1:3),C8(1:3))
!Clothes Dryers
        c8=0.
        c8(1)=engrs(iyy)   * C_CO2_FACTOR
        c8(2)=fsum(emel(1,1,iy),4)/qelas(11,iy)
        T22( 6,IY,IS)=dot_product(RSDRYCON(iy,1:2),C8(1:2))
!All electric
        T22( 7,IY,IS)=RSFRZCON(IY)*c8(2)       !Freezers
        T22( 8,IY,IS)=RSLTCON(IY)*c8(2)       !Lighting
        T22( 9,IY,IS)=RSCSWCON(IY)*c8(2)      !Clothes Washers
        T22(10,IY,IS)=RSDSWCON(IY)*c8(2)      !Dishwashers
        T22(11,IY,IS)=RSTVRCON(IY)*c8(2)       !Color Televisions and Set-Top Boxes
        T22(12,IY,IS)=RSPCRCON(IY)*c8(2)       !Personal Computers
        T22(13,IY,IS)=RSFANCON(IY)*c8(2)      !Furnace Fans
        T22(15,IY,IS)=sum(T22(1:14,IY,IS))    !  Total
        T22(1:15,iy,is)=T22(1:15,iy,is)/10**9
        icy=iy+baseyr-1
        T22(65,IY,IS) = sum(em_resd(1:iel_R,11,icy))-T22(15,iy,is)   ! Discrepancy
        T22(15,IY,IS)= T22(15,IY,IS)+ T22(65,IY,IS)
   40 CONTINUE

! TABLE 5 COMMERCIAL SECTOR KEY INDICATORS AND END-USE CONSUMPTION
      DO 50 IY=1,LASTYR
!     --- KEY INDICATORS
      T5( 1,IY,IS) = CMUSSURVFLOORTOT(IY)
      T5( 2,IY,IS) = CMUSNEWFLOORTOT(IY)
      T5( 3,IY,IS) = T5(1,IY,IS) + T5(2,IY,IS)
      IF (T5( 3,IY,IS) .NE. 0.0) THEN
        T5( 4,IY,IS)=((QTSCM(11,IY) - QSTCM(11,IY) - QPVCM(11,IY) + &
					 SUM(CGCOMMGEN(11,IY,1:12,2)) * 3.412 * 0.000001) / T5(3,IY,IS)) * 1000.
        T5( 5,IY,IS)=((QTSCM(11,IY) - QSTCM(11,IY) - QPVCM(11,IY)) / T5(3,IY,IS)) * 1000
        T5( 6,IY,IS) = FSUM(T5(4,IY,IS),2)
      ENDIF
!     --- DELIVERED ENERGY CONSUMPTION
!     --- ELECTRIC
      T5( 7,IY,IS) = CMUSCONSUMPTION(1,1,IY)
      T5( 8,IY,IS) = CMUSCONSUMPTION(2,1,IY)
      T5( 9,IY,IS) = CMUSCONSUMPTION(3,1,IY)
      T5(10,IY,IS) = CMUSCONSUMPTION(4,1,IY)
      T5(11,IY,IS) = CMUSCONSUMPTION(5,1,IY)
      T5(12,IY,IS) = CMUSCONSUMPTION(6,1,IY)
      T5(13,IY,IS) = CMUSCONSUMPTION(7,1,IY)
      T5(14,IY,IS) = CMUSCONSUMPTION(8,1,IY)
      T5(15,IY,IS) = CMUSCONSUMPTION(9,1,IY)
      T5(16,IY,IS) = QELCM(11,IY) - FSUM(T5(7,IY,IS),9) &
         + SUM(CGCOMMGEN(11,IY,1:12,2)) * 3.412 * 0.000001 ! Other end uses
      T5(17,IY,IS) = QELCM(11,IY) + SUM(CGCOMMGEN(11,IY,1:12,2)) * 3.412 * 0.000001 ! Gross subtotal
      T5(87,IY,IS) = SUM(CGCOMMGEN(11,IY,1:12,2)) * 3.412 * 0.000001 ! Own-use distributed generation
	   T5(88,IY,IS) = QELCM(11,IY) ! Total net of DG
!     --- NATURAL GAS
      T5(18,IY,IS) = CMUSCONSUMPTION(1,2,IY)
      T5(19,IY,IS) = CMUSCONSUMPTION(2,2,IY)
      T5(20,IY,IS) = CMUSCONSUMPTION(3,2,IY)
      T5(21,IY,IS) = CMUSCONSUMPTION(5,2,IY)
      T5(22,IY,IS) = QNGCM(11,IY) - FSUM(T5(18,IY,IS),4)
      T5(23,IY,IS) = QNGCM(11,IY)
!     --- DISTILLATE
      T5(24,IY,IS) = CMUSCONSUMPTION(1,3,IY)
      T5(25,IY,IS) = CMUSCONSUMPTION(3,3,IY)
      T5(26,IY,IS) = QDSCM(11,IY) - T5(24,IY,IS) - T5(25,IY,IS)
      T5(27,IY,IS) = QDSCM(11,IY)
!     --- OTHER FUELS
      T5(28,IY,IS) = QTSCM(11,IY) - QELCM(11,IY) - QNGCM(11,IY) - &
                     QDSCM(11,IY) - QSTCM(11,IY) - QBMCM(11,IY) - QPVCM(11,IY)
!     --- MARKETED RENEWABLES
      T5(29,IY,IS) = QBMCM(11,IY)
!     --- DELIVERED ENERGY CONSUMPTION BY END-USE
      T5(31,IY,IS) = CMUSCONSUMPTION(1,1,IY) + CMUSCONSUMPTION(1,2,IY) + &
                     CMUSCONSUMPTION(1,3,IY)
      T5(32,IY,IS) = CMUSCONSUMPTION(2,1,IY) + CMUSCONSUMPTION(2,2,IY)
      T5(33,IY,IS) = CMUSCONSUMPTION(3,1,IY) + CMUSCONSUMPTION(3,2,IY) + &
                     CMUSCONSUMPTION(3,3,IY)
      T5(34,IY,IS) = CMUSCONSUMPTION(4,1,IY)
      T5(35,IY,IS) = CMUSCONSUMPTION(5,1,IY) + CMUSCONSUMPTION(5,2,IY)
      T5(36,IY,IS) = CMUSCONSUMPTION(6,1,IY)
      T5(37,IY,IS) = CMUSCONSUMPTION(7,1,IY)
      T5(38,IY,IS) = CMUSCONSUMPTION(8,1,IY)
      T5(39,IY,IS) = CMUSCONSUMPTION(9,1,IY)
      T5(40,IY,IS) = QTSCM(11,IY) - FSUM(T5(31,IY,IS),9) - QSTCM(11,IY) &
         - QPVCM(11,IY)+ SUM(CGCOMMGEN(11,IY,1:12,2)) * 3.412 * 0.000001 ! Other uses
      T5(41,IY,IS) = QTSCM(11,IY) - QSTCM(11,IY) - QPVCM(11,IY) &
         + SUM(CGCOMMGEN(11,IY,1:12,2)) * 3.412 * 0.000001 ! Gross subtotal
      T5(89,IY,IS) = SUM(CGCOMMGEN(11,IY,1:12,2)) * 3.412 * 0.000001 ! Own-use distributed generation
	   T5(90,IY,IS) = QTSCM(11,IY) -  QSTCM(11,IY) - QPVCM(11,IY) ! Total net of distributed generation

!     --- ELECTRICITY RELATED LOSSES BY END-USE
      ELECLOSS = QTSEL(11,IY) - QELAS(11,IY)
      T5(42,IY,IS) = QELCM(11,IY)  / QELAS(11,IY) *ELECLOSS
!     --- TOTAL ENERGY CONSUMPTION BY END-USE
      T5(43,IY,IS) = T5(31,IY,IS) + CMUSCONSUMPTION(1,1,IY) * (QELCM(11,IY)/T5(17,IY,IS)) / QELAS(11,IY) *ELECLOSS
      T5(44,IY,IS) = T5(32,IY,IS) + CMUSCONSUMPTION(2,1,IY) * (QELCM(11,IY)/T5(17,IY,IS)) / QELAS(11,IY) *ELECLOSS
      T5(45,IY,IS) = T5(33,IY,IS) + CMUSCONSUMPTION(3,1,IY) * (QELCM(11,IY)/T5(17,IY,IS)) / QELAS(11,IY) *ELECLOSS
      T5(46,IY,IS) = T5(34,IY,IS) + CMUSCONSUMPTION(4,1,IY) * (QELCM(11,IY)/T5(17,IY,IS)) / QELAS(11,IY) *ELECLOSS
      T5(47,IY,IS) = T5(35,IY,IS) + CMUSCONSUMPTION(5,1,IY) * (QELCM(11,IY)/T5(17,IY,IS)) / QELAS(11,IY) *ELECLOSS
      T5(48,IY,IS) = T5(36,IY,IS) + CMUSCONSUMPTION(6,1,IY) * (QELCM(11,IY)/T5(17,IY,IS)) / QELAS(11,IY) *ELECLOSS
      T5(49,IY,IS) = T5(37,IY,IS) + CMUSCONSUMPTION(7,1,IY) * (QELCM(11,IY)/T5(17,IY,IS)) / QELAS(11,IY) *ELECLOSS
      T5(50,IY,IS) = T5(38,IY,IS) + CMUSCONSUMPTION(8,1,IY) * (QELCM(11,IY)/T5(17,IY,IS)) / QELAS(11,IY) *ELECLOSS
      T5(51,IY,IS) = T5(39,IY,IS) + CMUSCONSUMPTION(9,1,IY) * (QELCM(11,IY)/T5(17,IY,IS)) / QELAS(11,IY) *ELECLOSS
      T5(52,IY,IS) = T5(40,IY,IS) + T5(16,IY,IS) &
         * (QELCM(11,IY)/T5(17,IY,IS)) / QELAS(11,IY) *ELECLOSS ! Other uses
      T5(53,IY,IS) = T5(41,IY,IS) + T5(42,IY,IS) ! Gross subtotal
	   T5(91,IY,IS) = SUM(CGCOMMGEN(11,IY,1:12,2)) * 3.412 * 0.000001 ! Own-use distributed generation
	   T5(92,IY,IS) = T5(90,IY,IS) + T5(42,IY,IS) ! Total net of distributed generation
!     --- NON-MARKETED RENEWABLES
      T5(55,IY,IS) = QSTCM(11,IY) + QPVCM(11,IY) + CGCOMMQ(11,IY,11) / 1000.
      T5(30,IY,IS) = QSTCM(11,IY)
      T5(54,IY,IS) = QPVCM(11,IY)
      T5(56,IY,IS) = CGCOMMQ(11,IY,11) / 1000.

      T5(57:65,IY,IS) = DegreeDays(1,1:9,IY)
      T5(66,IY,IS) = DegreeDays(1,MNUMCR,IY)
      T5(67:75,IY,IS) = DegreeDays(2,1:9,IY)
      T5(76,IY,IS) = DegreeDays(2,MNUMCR,IY)
      T5(77,IY,IS) = PELSHCM(11,IY)
      T5(78,IY,IS) = PELSCCM(11,IY)
      T5(79,IY,IS) = PELWHCM(11,IY)
      T5(80,IY,IS) = PELVTCM(11,IY)
      T5(81,IY,IS) = PELCKCM(11,IY)
      T5(82,IY,IS) = PELLTCM(11,IY)
      T5(83,IY,IS) = PELRFCM(11,IY)
      T5(84,IY,IS) = PELOPCM(11,IY)
      T5(85,IY,IS) = PELONCM(11,IY)
      T5(86,IY,IS) = PELOTCM(11,IY)

      iyy=max(iy,6)
      c8=0.
      c8(1)=fsum(emel(1,1,iy),4)/qelas(11,iy)     ! average co2 emitted per btu of electricity sales
      c8(2)=engcm(iyy)   * C_CO2_FACTOR
      c8(3)=edscm(iyy)   * C_CO2_FACTOR

      T22(16,IY,IS) = dot_product(CMUSCONSUMPTION(1,1:3,IY),c8(1:3))   ! Space Heating
      T22(17,IY,IS) = dot_product(CMUSCONSUMPTION(2,1:3,IY),c8(1:3))   ! Space Cooling
      T22(18,IY,IS) = dot_product(CMUSCONSUMPTION(3,1:3,IY),c8(1:3))   ! Water Heating
      T22(19,IY,IS) = dot_product(CMUSCONSUMPTION(4,1:3,IY),c8(1:3))   ! Ventilation
      T22(20,IY,IS) = dot_product(CMUSCONSUMPTION(5,1:3,IY),c8(1:3))   ! Cooking
      T22(21,IY,IS) = dot_product(CMUSCONSUMPTION(6,1:3,IY),c8(1:3))   ! Lighting
      T22(22,IY,IS) = dot_product(CMUSCONSUMPTION(7,1:3,IY),c8(1:3))   ! Refrigeration
      T22(23,IY,IS) = dot_product(CMUSCONSUMPTION(8,1:3,IY),c8(1:3))   ! Office Equipment (PC)
      T22(24,IY,IS) = dot_product(CMUSCONSUMPTION(9,1:3,IY),c8(1:3))   ! Office Equipment (non-PC)
      icy=iy+baseyr-1
      T22(25,IY,IS) = sum(em_comm(1:iel_C,11,icy))-sum(T22(16:24,iy,is))   ! Other Uses
      T22(26,IY,IS)= sum(T22(16:25,IY,IS))
 50   CONTINUE

! TABLE 19 INDUSTRIAL SECTOR KEY INDICATORS AND END-USE
! TABLE 6 INDUSTRIAL SECTOR KEY INDICATORS AND END-USE
!          CONSUMPTION
!      --- VALUE OF GROSS OUTPUT
      DO 60 IY=1,LASTYR
        T6( 1,IY,IS)= 0.0
        T6( 2,IY,IS)= 0.0
        DO LOOP1=1,41
          T6( 1,IY,IS)= T6(1,IY,IS) + MC_REVIND(11,LOOP1,IY)
        END DO
        DO LOOP1=1,7
          T6( 2,IY,IS)= T6(2,IY,IS) + MC_REVIND(11,LOOP1+41,IY)
        END DO
!  1 (food) and 16 (other chemical) are totals of 2-5 and 17-20, respectively, so subtract
!  also, ethanol (17) and flat glass (29) are subsets of 16 and 28
        T6( 1,IY,IS)= T6(1,IY,IS) - sum(MC_REVIND(11,2:5,IY)) - sum(MC_REVIND(11,11:13,IY)) - &
             sum(MC_REVIND(11,21:24,IY)) - MC_REVIND(11,17,IY) - MC_REVIND(11,29,IY)
        T6( 3,IY,IS) = T6(1,IY,IS) + T6(2,IY,IS)
!       --- ENERGY PRICES
        T6( 4,IY,IS) = PELIN(11,IY)
        T6( 5,IY,IS) = PGIIN(11,IY)
        T6(68,IY,IS) = PGFIN(11,IY)
        T6( 6,IY,IS) = PCLIN(11,IY)
        T6( 7,IY,IS) = PRSIN(11,IY)
        T6( 8,IY,IS) = PDSIN(11,IY)
        T6( 9,IY,IS) = PLGIN(11,IY)
! but it is simpler (the row in now labelled "propane") if we just use the propane price variable:
        T6( 9,IY,IS) = PPRIN(11,IY)
        T6(67,IY,IS) = PPRINPF(11,IY)
        T6(10,IY,IS) = PMGIN(11,IY)
        T6(61,IY,IS) = PPFIN(11,IY)
        T6(123,IY,IS) = PETIN(11,IY)
        T6(62,IY,IS) = PASIN(11,IY)
        T6(11,IY,IS) = PMCIN(11,IY)
        T6(60,IY,IS) = PCLSN(11,IY)
        T6(119,IY,IS) = T6( 9,IY,IS) / SCALPR * MC_JPGDP(IY)
        T6(120,IY,IS) = T6(10,IY,IS) / SCALPR * MC_JPGDP(IY)
        T6(121,IY,IS) = T6( 8,IY,IS) / SCALPR * MC_JPGDP(IY)
        T6(122,IY,IS) = T6( 7,IY,IS) / SCALPR * MC_JPGDP(IY)
        T6(124,IY,IS) = T6(62,IY,IS) / SCALPR * MC_JPGDP(IY)
        T6(125,IY,IS) = T6( 5,IY,IS) / SCALPR * MC_JPGDP(IY)
        T6(126,IY,IS) = T6(68,IY,IS) / SCALPR * MC_JPGDP(IY)
        T6(127,IY,IS) = T6(11,IY,IS) / SCALPR * MC_JPGDP(IY)
        T6(128,IY,IS) = T6( 6,IY,IS) / SCALPR * MC_JPGDP(IY)
        T6(129,IY,IS) = T6(60,IY,IS) / SCALPR * MC_JPGDP(IY)
        T6(130,IY,IS) = T6( 4,IY,IS) / SCALPR * MC_JPGDP(IY)
        T6(131,IY,IS) = PELINP(11,IY)
        T6(132,IY,IS) = PELINS(11,IY)
        T6(133,IY,IS) = PELINM(11,IY)
!       --- ENERGY CONSUMPTION
        ELECLOSS = QTSEL(11,IY) - QELAS(11,IY)
        T6(12,IY,IS)= QELIN(11,IY)
        T6(38,IY,IS)= QNGIN(11,IY) - INQNGPF(11,IY) / 1000. - QGTLRF(11,IY) - RFQNGPF(11,IY) / 1000.
        T6(64,IY,IS)= INQNGPF(11,IY) / 1000. + RFQNGPF(11,IY) / 1000.
        T6(39,IY,IS)= QLPIN(11,IY)
! for coal, subtract out coal gasification coal, but leave in losses
! this means we subtract the gas production number
        T6(14,IY,IS)= QCLIN(11,IY) - QCTLRF(11,IY) - OGSUPGAS(1,11,IY) * CFNGC(IY) * .001
        T6(58,IY,IS)= QCTLRF(11,IY)
        T6(15,IY,IS)= QMCIN(11,IY) + QCIIN(11,IY)
        T6(43,IY,IS)= FSUM(T6(14,IY,IS),2) + T6(58,IY,IS)
        T6(16,IY,IS)= QRSIN(11,IY)
        T6(17,IY,IS)= QDSIN(11,IY)
        T6(18,IY,IS)= QLGIN(11,IY) - INQLGPF(11,IY) / 1000.
        T6(63,IY,IS)= INQLGPF(11,IY) / 1000.
        T6(19,IY,IS)= QPFIN(11,IY)
        T6(20,IY,IS)= QOTIN(11,IY) + QKSIN(11,IY)
        T6(50,IY,IS)= QMGIN(11,IY)
        T6(51,IY,IS)= QPCIN(11,IY)
        T6(52,IY,IS)= QSGIN(11,IY)
        T6(53,IY,IS)= QASIN(11,IY)
        T6(21,IY,IS)= QTRIN(11,IY) - REFCON(ixRN,5,IY) / 1000.
! Digress to separate Refinery consumption (including ethanol plant for CL, NG, EL) from Industrial for diagnostics
      ! catalytic coke (QCCRF) is sub-category of petroleum coke; converted to quads in CAPFLATE
             T6(134,IY,IS) = QCCRF(11,IY)
             T6( 93,IY,IS) = REFCON(ixDS,5,IY) / 1000.
             T6( 94,IY,IS) = REFCON(ixLG,5,IY) / 1000.
             T6( 97,IY,IS) = REFCON(ixRF,5,IY) / 1000.
             T6( 99,IY,IS) = REFCON(ixPC,5,IY) / 1000.
             T6(101,IY,IS) = RFQSGPF(11,IY) / 1000.
             T6(100,IY,IS) = REFCON(ixSG,5,IY) / 1000. - T6(101,IY,IS)
             T6(108,IY,IS) = REFCON(ixSG,5,IY) / 1000.
             T6(102,IY,IS) = REFCON(ixOP,5,IY) / 1000.
             T6(105,IY,IS) = RFQNGPF(11,IY) / 1000.
             T6(106,IY,IS) = QGTLRF(11,IY)
             T6(104,IY,IS) = REFCON(ixNG,5,IY) / 1000. - T6(106,IY,IS) - T6(105,IY,IS)
             T6(109,IY,IS) = QCLETH(IY,11) / 1000.
             T6(110,IY,IS) =(REFCON(ixCL,5,IY) - QCLETH(IY,11)) / 1000.
             T6(112,IY,IS) = &
                      (CORNCD(3,11,IY) * CFCORN / 1000. -0.9751*(CRNETHCD(11,IY)+OTHETHCD(11,IY)) * 365. * CFPET - &
                       RFBIOBUTECD(MNUMCR,IY)*365*CFBIOBUTE(IY)) / 1000000. + &
                       sum(BIMQTYCD(1:4,11,IY))/1000000.*RDAYS*(CFVEGGIE(IY)-CFBIOD(IY)) + &
                 QBMRFBTL(11,IY)/1000. - RDAYS / 1000000. * &
                      (sum(BTLFRAC(1:4,MNUMPR,IY)) * CFBTLLIQ(IY) + &
                       sum(CBTLFRAC(2,1:4,MNUMPR,IY)) * CFCBTLLIQ(2,IY) + &
                       UBAVOL(MNUMPR,IY) * 5.763)
           IF (CONEFF(IY) .NE. 0.0) T6(112,IY,IS) = T6(112,IY,IS) + &
                (0.9751*CLLETHCD(11,IY) * RDAYS * (CFBMQ(IY) * 42. / CONEFF(IY) - CFPET)) / 1000000.
             T6(113,IY,IS) = REFCON(ixEL,5,IY) / 1000.
             T6(103,IY,IS) = FSUM(T6( 93,IY,IS),10)
             T6(107,IY,IS) = FSUM(T6(104,IY,IS),3)
             T6(111,IY,IS) = FSUM(T6(109,IY,IS),2)
             T6(114,IY,IS) = T6(103,IY,IS) + T6(107,IY,IS) + FSUM(T6(111,IY,IS),3)
             T6(115,IY,IS)= REFCON(ixEL,5,IY) / 1000. / QELAS(11,IY) * ELECLOSS
             T6(116,IY,IS) = FSUM(T6(114,IY,IS),2)

             T6( 69,IY,IS) = QDSIN(11,IY) - REFCON(ixDS,5,IY) / 1000.
             T6( 70,IY,IS) = QLGIN(11,IY) - REFCON(ixLG,5,IY) / 1000. - INQLGPF(11,IY) / 1000.
             T6( 71,IY,IS) = INQLGPF(11,IY) / 1000.
             T6(136,IY,IS) = QPPINPF(MNUMCR,IY)
             T6(137,IY,IS) = QETINPF(MNUMCR,IY)
             T6(138,IY,IS) = QPRINPF(MNUMCR,IY)
             T6(139,IY,IS) = QPROLENERF(MNUMCR,IY)
             T6(140,IY,IS) = QBUINPF(MNUMCR,IY) + QISINPF(MNUMCR,IY)
             T6( 72,IY,IS) = QPFIN(11,IY)
             T6( 73,IY,IS) = QRSIN(11,IY) - REFCON(ixRF,5,IY) / 1000.
             T6( 74,IY,IS) = QMGIN(11,IY)
             T6( 75,IY,IS) = QPCIN(11,IY) - REFCON(ixPC,5,IY) / 1000.
             T6( 76,IY,IS) = QSGIN(11,IY) - REFCON(ixSG,5,IY) / 1000.
             T6( 77,IY,IS) = QASIN(11,IY)
             T6( 78,IY,IS) = QOTIN(11,IY) + QKSIN(11,IY) - REFCON(ixOP,5,IY) / 1000.
             T6( 80,IY,IS) = QNGIN(11,IY) - REFCON(ixNG,5,IY) / 1000. - INQNGPF(11,IY) / 1000.
             T6( 81,IY,IS) = INQNGPF(11,IY) / 1000.
             T6( 84,IY,IS) = QMCIN(11,IY) + QCIIN(11,IY)
! for coal, subtract out coal gasification coal, but leave in losses
! this means we subtract the gas production number
             T6( 85,IY,IS) = QCLIN(11,IY) - REFCON(ixCL,5,IY) / 1000. - OGSUPGAS(1,11,IY) * CFNGC(IY) * .001
             T6( 88,IY,IS) = QTRIN(11,IY) - REFCON(ixRN,5,IY) / 1000.
             T6( 89,IY,IS) = QELIN(11,IY) - REFCON(ixEL,5,IY) / 1000.
             T6( 79,IY,IS) = FSUM(T6( 69,IY,IS),10)
             T6( 83,IY,IS) = FSUM(T6( 80,IY,IS),3) + T6( 39,IY,IS)
             T6( 87,IY,IS) = FSUM(T6( 84,IY,IS),2)
             T6( 90,IY,IS) = T6( 79,IY,IS) + T6( 83,IY,IS) + FSUM(T6( 87,IY,IS),3)
             T6( 91,IY,IS) = (QELIN(11,IY) - REFCON(ixEL,5,IY) / 1000.) / QELAS(11,IY) * ELECLOSS
             T6( 92,IY,IS) = FSUM(T6( 90,IY,IS),2)
! Calculate total industrial natural gas
        T6(13,IY,IS) = FSUM(T6(38,IY,IS),2) + T6(64,IY,IS) + T6(106,IY,IS)
!       --- CONSUMPTION PER UNIT OF OUTPUT
        IF (T6(3,IY,IS) .NE. 0.0) THEN
          T6(25,IY,IS)= QELIN(11,IY) * 1000. / T6(3,IY,IS)
          T6(40,IY,IS)= (QNGIN(11,IY) * 1000. - INQNGPF(11,IY) - QGTLRF(11,IY) * 1000. - RFQNGPF(11,IY)) / T6(3,IY,IS)
          T6(41,IY,IS)=(QLPIN(11,IY)) * 1000. / T6(3,IY,IS)
          T6(135,IY,IS)=(QNGLQ(11,IY)) * 1000. / T6(3,IY,IS)
          T6(66,IY,IS)= (INQNGPF(11,IY) + RFQNGPF(11,IY)) / T6(3,IY,IS)
! for coal, subtract out coal gasification coal, but leave in losses
! this means we subtract the gas production number
          T6(27,IY,IS)= (QCLIN(11,IY) - QCTLRF(11,IY) - OGSUPGAS(1,11,IY)*CFNGC(IY)*.001) * 1000. / T6(3,IY,IS)
          T6(59,IY,IS)= QCTLRF(11,IY) * 1000. / T6(3,IY,IS)
          T6(28,IY,IS)= (QMCIN(11,IY) + QCIIN(11,IY)) * 1000. / T6(3,IY,IS)
          T6(29,IY,IS)= QRLIN(11,IY) * 1000. / T6(3,IY,IS)
          T6(30,IY,IS)= QDSIN(11,IY) * 1000. / T6(3,IY,IS)
          T6(31,IY,IS)= (QLGIN(11,IY) * 1000. - INQLGPF(11,IY)) / T6(3,IY,IS)
          T6(65,IY,IS)= INQLGPF(11,IY) / T6(3,IY,IS)
          T6(32,IY,IS)= QPFIN(11,IY) * 1000. / T6(3,IY,IS)
          T6(54,IY,IS)= QMGIN(11,IY) * 1000. / T6(3,IY,IS)
          T6(55,IY,IS)= QPCIN(11,IY) * 1000. / T6(3,IY,IS)
          T6(56,IY,IS)= QSGIN(11,IY) * 1000. / T6(3,IY,IS)
          T6(57,IY,IS)= QASIN(11,IY) * 1000. / T6(3,IY,IS)
          T6(33,IY,IS)= (QOTIN(11,IY) + QKSIN(11,IY)) * 1000. / T6(3,IY,IS)
          T6(34,IY,IS)= (QTRIN(11,IY) - REFCON(ixRN,5,IY) / 1000.) * 1000. / T6(3,IY,IS)
          T6(117,IY,IS) = 1000. * QGTLRF(11,IY) / T6(3,IY,IS)
          T6(118,IY,IS) = 1000. * &
                     ((CORNCD(3,11,IY) * CFCORN / 1000. -0.9751*(CRNETHCD(11,IY)+OTHETHCD(11,IY)) * 365. * CFPET - &
                       RFBIOBUTECD(MNUMCR,IY)*365*CFBIOBUTE(IY))/1000000. + &
                 QBMRFBTL(11,IY)/1000. - RDAYS / 1000000. * &
                      (sum(BTLFRAC(1:4,MNUMPR,IY)) * CFBTLLIQ(IY) + &
                       sum(CBTLFRAC(2,1:4,MNUMPR,IY)) * CFCBTLLIQ(2,IY) + &
                       UBAVOL(MNUMPR,IY) * 5.763) + &
                       sum(BIMQTYCD(1:4,11,IY))/1000000.*RDAYS*(CFVEGGIE(IY)-CFBIOD(IY))) / T6(3,IY,IS)
           IF (CONEFF(IY) .NE. 0.0) T6(118,IY,IS) = T6(118,IY,IS) + 1000. / T6(3,IY,IS) * &
               ((0.9751*CLLETHCD(11,IY) * RDAYS * (CFBMQ(IY) * 42. / CONEFF(IY) - CFPET)) / 1000000.)
        ENDIF
        T6(26,IY,IS)= FSUM(T6(40,IY,IS),2) + T6(66,IY,IS) + T6(117,IY,IS)
        T6(44,IY,IS)= FSUM(T6(29,IY,IS),5) + FSUM(T6(54,IY,IS),4) + T6(65,IY,IS)
        T6(42,IY,IS)= FSUM(T6(16,IY,IS),5) + FSUM(T6(50,IY,IS),4) + T6(63,IY,IS)
        T6(22,IY,IS) = FSUM(T6(42,IY,IS),2) + FSUM(T6(12,IY,IS),2) + T6(21,IY,IS) + T6(112,IY,IS)
        T6(23,IY,IS)= QELIN(11,IY) / QELAS(11,IY) * ELECLOSS
        T6(24,IY,IS)= FSUM(T6(22,IY,IS),2)
        T6(45,IY,IS)= FSUM(T6(27,IY,IS),2) + T6(59,IY,IS)
        T6(35,IY,IS)= FSUM(T6(44,IY,IS),2) + FSUM(T6(25,IY,IS),2) + T6(34,IY,IS) + T6(118,IY,IS)
        T6(36,IY,IS)= QELIN(11,IY) / QELAS(11,IY) * ELECLOSS * 1000./ T6(3,IY,IS)
        T6(37,IY,IS)= FSUM(T6(35,IY,IS),2)
        T6(46,IY,IS)= .001 * ( &
             CGINDLCAP(11,IY, 1) + CGINDLCAP(11,IY, 2) + &
             CGINDLCAP(11,IY, 3) + CGINDLCAP(11,IY, 6) + &
             CGINDLCAP(11,IY, 7) + CGINDLCAP(11,IY, 9) + &
             CGINDLCAP(11,IY,10))
        T6(47,IY,IS)= .001 * ( &
             CGINDLGEN(11,IY, 1,1) + CGINDLGEN(11,IY, 1,2) + &
             CGINDLGEN(11,IY, 2,1) + CGINDLGEN(11,IY, 2,2) + &
             CGINDLGEN(11,IY, 3,1) + CGINDLGEN(11,IY, 3,2) + &
             CGINDLGEN(11,IY, 6,1) + CGINDLGEN(11,IY, 6,2) + &
             CGINDLGEN(11,IY, 7,1) + CGINDLGEN(11,IY, 7,2) + &
             CGINDLGEN(11,IY, 9,1) + CGINDLGEN(11,IY, 9,2) + &
             CGINDLGEN(11,IY,10,1) + CGINDLGEN(11,IY,10,2))
        T6(48,IY,IS)= .001 * ( &
             CGREFCAP(11,IY,1) + CGREFCAP(11,IY,2) + &
             CGREFCAP(11,IY,3) + CGREFCAP(11,IY,6) + &
             CGREFCAP(11,IY,7) + CGREFCAP(11,IY,9) + &
             CGOGSCAP(11,IY,1) + CGOGSCAP(11,IY,2) + &
             CGOGSCAP(11,IY,3) + CGOGSCAP(11,IY,4) + &
             CGINDLCAP(11,IY, 1) + CGINDLCAP(11,IY, 2) + &
             CGINDLCAP(11,IY, 3) + CGINDLCAP(11,IY, 6) + &
             CGINDLCAP(11,IY, 7) + CGINDLCAP(11,IY, 9) + &
             CGINDLCAP(11,IY,10))
        T6(49,IY,IS)= .001 * ( &
             CGREFGEN(11,IY,1,1) + CGREFGEN(11,IY,1,2) + &
             CGREFGEN(11,IY,2,1) + CGREFGEN(11,IY,2,2) + &
             CGREFGEN(11,IY,3,1) + CGREFGEN(11,IY,3,2) + &
             CGREFGEN(11,IY,6,1) + CGREFGEN(11,IY,6,2) + &
             CGREFGEN(11,IY,7,1) + CGREFGEN(11,IY,7,2) + &
             CGREFGEN(11,IY,9,1) + CGREFGEN(11,IY,9,2) + &
             CGOGSGEN(11,IY,1,1) + CGOGSGEN(11,IY,1,2) + &
             CGOGSGEN(11,IY,2,1) + CGOGSGEN(11,IY,2,2) + &
             CGOGSGEN(11,IY,3,1) + CGOGSGEN(11,IY,3,2) + &
             CGOGSGEN(11,IY,4,1) + CGOGSGEN(11,IY,4,2) + &
             CGINDLGEN(11,IY, 1,1) + CGINDLGEN(11,IY, 1,2) + &
             CGINDLGEN(11,IY, 2,1) + CGINDLGEN(11,IY, 2,2) + &
             CGINDLGEN(11,IY, 3,1) + CGINDLGEN(11,IY, 3,2) + &
             CGINDLGEN(11,IY, 6,1) + CGINDLGEN(11,IY, 6,2) + &
             CGINDLGEN(11,IY, 7,1) + CGINDLGEN(11,IY, 7,2) + &
             CGINDLGEN(11,IY, 9,1) + CGINDLGEN(11,IY, 9,2) + &
             CGINDLGEN(11,IY,10,1) + CGINDLGEN(11,IY,10,2))
   60  CONTINUE

! TABLE 7 TRANSPORTATION SECTOR KEY INDICATORS AND END-USE CONSUMPTION
      DO 70 IY=1,LASTYR
!       --- LEVEL OF TRAVEL INDEX
        T7( 1,IY,IS) = TRLDVMTE(1,IY)
        T7( 2,IY,IS) = BCLTVMT(10,IY)
        T7(83,IY,IS) = PAS_RPM(1,IY)
        T7(84,IY,IS) = TRLDVMTE(1,IY) - PAS_RPM(1,IY)
        T7(85,IY,IS) = PAS_RPM(2,IY)
        T7(86,IY,IS) = PAS_RPM(3,IY)
!		T7(88,IY,IS) = SUM(AIROUT(4:5,IY))		! MDRAEO2022
!          --- TWO-AXLE, FOUR TIRE TRUCKS
		T7( 3,IY,IS) = 	TRVMTTRK(1,1,IY) + TRVMTTRK(1,2,IY) + &
						TRVMTTRK(1,3,IY) + TRVMTTRK(1,4,IY) + &
						TRVMTTRK(2,1,IY) + TRVMTTRK(2,2,IY) + &
						TRVMTTRK(2,3,IY) + TRVMTTRK(2,4,IY) + &
						TRVMTTRK(1,5,IY) + TRVMTTRK(1,6,IY) + &
						TRVMTTRK(1,7,IY) + TRVMTTRK(1,8,IY) + &
						TRVMTTRK(1,9,IY) + TRVMTTRK(2,5,IY) + &
						TRVMTTRK(2,6,IY) + TRVMTTRK(2,7,IY) + &
						TRVMTTRK(2,8,IY) + TRVMTTRK(2,9,IY)
!		T7( 3,IY,IS) = SUM(TRVMTTRK(1:2, 1:9, IY))
        T7( 4,IY,IS) = TRSTMDEM(1,IY)
        T7( 5,IY,IS) = TRTMRR(1,IY)
        T7( 6,IY,IS) = TRTMSHIP(1,IY)
!       --- ENERGY EFFICIENCY INDICATORS
        T7(53,IY,IS) = CAFESTD(3,IY)
        T7(54,IY,IS) = CAFESTD(1,IY)
        T7(55,IY,IS) = CAFESTD(2,IY)
        T7(87,IY,IS) = LDV_MPG(1,IY)
        T7( 7,IY,IS) = NEWMPG(3,IY)
        T7( 8,IY,IS) = NEWMPG(1,IY)
        T7( 9,IY,IS) = NEWMPG(2,IY)
        T7(50,IY,IS) = TRUEMPG(3,IY)
        T7(51,IY,IS) = TRUEMPG(1,IY)
        T7(52,IY,IS) = TRUEMPG(2,IY)
        T7(45,IY,IS) = DEGRPT(1,IY) * TRUEMPG(1,IY)
        T7(46,IY,IS) = DEGRPT(2,IY) * TRUEMPG(2,IY)
        CAR_PCT = sum(TRLDSALC(1:16,11,IY))/(sum(TRLDSALC(1:16,11,IY))+sum(TRLDSALT(1:16,11,IY)))
        TRK_PCT = sum(TRLDSALT(1:16,11,IY))/(sum(TRLDSALC(1:16,11,IY))+sum(TRLDSALT(1:16,11,IY)))
        T7(47,IY,IS) = 1/(CAR_PCT/T7(45,IY,IS)+TRK_PCT/T7(46,IY,IS))
        T7(10,IY,IS) = TRLDMPGF(3,IY)
        T7(11,IY,IS) = NCLTMPGT(IY)
        T7(12,IY,IS) = CLTMPGT(IY)
        T7(13,IY,IS) = TRAIREFFS(4,IY)
        T7(14,IY,IS) = TRFTMPG(IY)
        T7(15,IY,IS) = TRTMRR(2,IY)
        T7(16,IY,IS) = TRTMSHIP(2,IY)
!       --- ENERGY USE BY MODE
        T7(17,IY,IS) = (TRQLDV(1,11,IY) + TRQLDV(2,11,IY) + &
             TRQLDV(3,11,IY) + TRQLDV(4,11,IY) + TRQLDV(5,11,IY) + &
             TRQLDV(6,11,IY)+TRQLDV(7,11,IY)+TRQLDV(8,11,IY)) * .001
        T7(18,IY,IS) = BCLTBTUT(10,IY) * .001
        T7(19,IY,IS) = (TRQFTRK(1,IY) + TRQFTRK(2,IY) + TRQFTRK(3,IY) + TRQFTRK(4,IY) + TRQFTRK(5,IY)) * .001
        T7(40,IY,IS) = sum(TRQBUS(:,:,IY)) * .001
        T7(20,IY,IS) =(TRQAIRT(1,IY) + TRQAIRT(2,IY)) * .001
        T7(21,IY,IS) =(TRQMIL(1,IY) + TRQMIL(2,IY) + TRQMIL(3,IY) + TRQMIL(4,IY)) * .001
        T7(22,IY,IS) =(TRQRRF(1,IY)  + TRQRRF(2,IY) + TRQRRF(3,IY) + TRQRRF(4,IY)) * .001
        T7(41,IY,IS) =(TRQRRP(1,IY)  + TRQRRP(2,IY) + TRQRRP(3,IY) + TRQRRP(4,IY) + &
                       TRQRRP(5,IY)  + TRQRRP(6,IY) + TRQRRP(7,IY) + TRQRRP(8,IY) + &
                       TRQRRP(9,IY))* .001
        T7(42,IY,IS) =(TRQDOMS(1,IY) + TRQDOMS(2,IY) + TRQDOMS(3,IY) + TRQDOMS(4,IY)) * .001
        T7(43,IY,IS) =(TRQINTS(1,IY) + TRQINTS(2,IY) + TRQINTS(3,IY) + TRQINTS(4,IY)) * .001
        T7(44,IY,IS) =(TRQBOAT(1,IY) + TRQBOAT(2,IY)) * .001
        T7(23,IY,IS) = QGPTR(11,IY)
        T7(24,IY,IS) = TRQLUB(IY) * .001
        T7(88,IY,IS) = QNGLQ(11,IY)
        T7(25,IY,IS) = sum(t7(17:24,iy,is)) + sum(t7(40:44,iy,is))+QNGLQ(11,IY)

!       --- Energy Use by Mode (million barrels/day crude oil equivalent)
        T7(26,IY,IS) = ((TRQLDV(1,11,IY)/CFMGQ(IY)) + (TRQLDV(2,11,IY)/CFM85Q(IY)) + &
                       (TRQLDV(3,11,IY)/CFETQ(IY)) + (TRQLDV(4,11,IY)/5.8) + &
                       (TRQLDV(5,11,IY)/CFLGQ(IY)) + (TRQLDV(6,11,IY)/5.8) + &
                       (TRQLDV(7,11,IY)/5.8) + (TRQLDV(8,11,IY)/CFDSTR(IY))) / 365.
        T7(27,IY,IS) = (CLTFUELBTU(1,IY)/CFMGQ(IY))/365.0  + (CLTFUELBTU(2,IY)/CFDSTR(IY))/365.0 + &
                       (CLTFUELBTU(3,IY)/CFPRQ)/365.0      + (CLTFUELBTU(4,IY)/5.8)/365.0 + &
                       (CLTFUELBTU(5,IY)/CFE85Q(IY))/365.0 + (CLTFUELBTU(6,IY)/5.8)/365.0 + &
                       (CLTFUELBTU(7,IY)/5.8)/365.0
        T7(28,IY,IS) = ((TRQFTRK(1,IY)/CFMGQ(IY)) + (TRQFTRK(2,IY)/CFDSTR(IY)) + &
                       (TRQFTRK(3,IY)/5.8) + (TRQFTRK(5,IY)/CFLGQ(IY)))/365.0
        T7(29,IY,IS) = (TRQRRF(1,IY)/CFDSTR(IY)+TRQRRF(2,IY)/CFRSQ+  &
                          (TRQRRF(3,IY)+TRQRRF(4,IY))/5.8)/365.0
        T7(30,IY,IS) = (TRQDOMS(1,IY)/CFDSTR(IY) + TRQDOMS(2,IY)/CFRSQ + &
                          (TRQDOMS(3,IY)+TRQDOMS(4,IY))/5.8)/365.0
        T7(31,IY,IS) = (TRQINTS(1,IY)/CFDSTR(IY) + TRQINTS(2,IY)/CFRSQ + &
                          (TRQINTS(3,IY)+TRQINTS(4,IY))/5.8)/365.0
        T7(32,IY,IS) = ((TRQAIRT(1,IY)/CFJFK) + (TRQAIRT(2,IY)/CFMGQ(IY)))/365.0
        T7(33,IY,IS) = (((TRQMIL(1,IY) + TRQMIL(2,IY))/CFJFK) + &
                       (TRQMIL(3,IY)/CFRSQ) + (TRQMIL(4,IY)/CFDSTR(IY)))/365.0
        T7(34,IY,IS) = (sum(TRQBUS(:,1,IY))/CFMGQ(IY) + &
                        sum(TRQBUS(:,2,IY))/CFDSTR(IY) + &
                        sum(TRQBUS(:,3,IY))/CFE85Q(IY) + &
                        sum(TRQBUS(:,5,IY))/5.8 + &
                        sum(TRQBUS(:,6,IY))/CFLGQ(IY) + &
                        sum(TRQBUS(:,7,IY))/5.8 + &
                        sum(TRQBUS(:,8,IY))/5.8)/365.0
        T7(35,IY,IS) = ((TRQRRP(1,IY) + TRQRRP(3,IY) + TRQRRP(4,IY) + TRQRRP(5,IY) + &
                         TRQRRP(6,IY) + TRQRRP(8,IY) + TRQRRP(9,IY))/5.8 +  &
                          (TRQRRP(2,IY) + TRQRRP(7,IY))/CFDSTR(IY))/365.0
        T7(36,IY,IS) = ((TRQBOAT(1,IY) + TRQBOAT(2,IY))/CFMGQ(IY))/365.0
        T7(37,IY,IS) = (TRQLUB(IY)/CFOTQ(IY))/365.0
        T7(38,IY,IS) = ((QGPTR(11,IY) * 1000.0)/5.8)/365.0
        T7(89,IY,IS) = ((QNGLQ(11,IY) * 1000.0)/5.8)/365.0
        T7(39,IY,IS) =  FSUM(T7(26,IY,IS),13) + T7(89,IY,IS)
        T7(48,IY,IS) = PELLTTR(11,IY)
        T7(49,IY,IS) = PELVHTR(11,IY)
        T7(56,IY,IS) = PGFTRFV(11,IY)
        T7(57,IY,IS) = PGFTRPV(11,IY)
        T7(58,IY,IS) = PGLTRFV(11,IY)
        T7(59,IY,IS) = PGLTRPV(11,IY)
        T7(60,IY,IS) = PGFTRRAIL(1,11,IY)
        T7(61,IY,IS) = PGLTRRAIL(1,11,IY)
        T7(62,IY,IS) = PGFTRRAIL(2,11,IY)
        T7(63,IY,IS) = PGLTRRAIL(2,11,IY)
        T7(64,IY,IS) = PGFTRRAIL(3,11,IY)
        T7(65,IY,IS) = PGLTRRAIL(3,11,IY)
        T7(66,IY,IS) = PGFTRRAIL(4,11,IY)
        T7(67,IY,IS) = PGLTRRAIL(4,11,IY)
        T7(68,IY,IS) = PGFTRSHIP(1,11,IY)
        T7(69,IY,IS) = PGLTRSHIP(1,11,IY)
        T7(70,IY,IS) = PGFTRSHIP(2,11,IY)
        T7(71,IY,IS) = PGLTRSHIP(2,11,IY)
        T7(72,IY,IS) = PGFTRSHIP(3,11,IY)
        T7(73,IY,IS) = PGLTRSHIP(3,11,IY)
        IF (fGFTRFV(11,IY)+fGFTRPV(11,IY) .NE. 0.0) &
            T7(74,IY,IS) = (PGFTRFV(11,IY)*fGFTRFV(11,IY) + PGFTRPV(11,IY)*fGFTRPV(11,IY))/ &
                           (fGFTRFV(11,IY)+fGFTRPV(11,IY))
        IF (sum(fGFTRRAIL(1:4,11,IY)) .NE. 0.0) &
            T7(75,IY,IS) = (PGFTRRAIL(1,11,IY)*fGFTRRAIL(1,11,IY) + PGFTRRAIL(2,11,IY)*fGFTRRAIL(2,11,IY) + &
                            PGFTRRAIL(3,11,IY)*fGFTRRAIL(3,11,IY) + PGFTRRAIL(4,11,IY)*fGFTRRAIL(4,11,IY))/ &
                           (fGFTRRAIL(1,11,IY)+fGFTRRAIL(2,11,IY)+fGFTRRAIL(3,11,IY)+fGFTRRAIL(4,11,IY))
        IF (sum(fGFTRSHIP(1:3,11,IY)) .NE. 0.0) &
            T7(76,IY,IS) = (PGFTRSHIP(1,11,IY)*fGFTRSHIP(1,11,IY) + PGFTRSHIP(2,11,IY)*fGFTRSHIP(2,11,IY) + &
                            PGFTRSHIP(3,11,IY)*fGFTRSHIP(3,11,IY)) / &
                           (fGFTRSHIP(1,11,IY)+fGFTRSHIP(2,11,IY)+fGFTRSHIP(3,11,IY))
        IF (fGLTRFV(11,IY)+fGLTRPV(11,IY) .NE. 0.0) &
            T7(77,IY,IS) = (PGLTRFV(11,IY)*fGLTRFV(11,IY) + PGLTRPV(11,IY)*fGLTRPV(11,IY))/ &
                           (fGLTRFV(11,IY)+fGLTRPV(11,IY))
        IF (sum(fGLTRRAIL(1:4,11,IY)) .NE. 0.0) &
            T7(78,IY,IS) = (PGLTRRAIL(1,11,IY)*fGLTRRAIL(1,11,IY) + PGLTRRAIL(2,11,IY)*fGLTRRAIL(2,11,IY) + &
                            PGLTRRAIL(3,11,IY)*fGLTRRAIL(3,11,IY) + PGLTRRAIL(4,11,IY)*fGLTRRAIL(4,11,IY))/ &
                           (fGLTRRAIL(1,11,IY)+fGLTRRAIL(2,11,IY)+fGLTRRAIL(3,11,IY)+fGLTRRAIL(4,11,IY))
        IF (sum(fGLTRSHIP(1:3,11,IY)) .NE. 0.0) &
            T7(79,IY,IS) = (PGLTRSHIP(1,11,IY)*fGLTRSHIP(1,11,IY) + PGLTRSHIP(2,11,IY)*fGLTRSHIP(2,11,IY) + &
                            PGLTRSHIP(3,11,IY)*fGLTRSHIP(3,11,IY)) / &
                           (fGLTRSHIP(1,11,IY)+fGLTRSHIP(2,11,IY)+fGLTRSHIP(3,11,IY))
        IF (fGFTRFV(11,IY)+fGFTRPV(11,IY)+sum(fGFTRRAIL(1:4,11,IY))+sum(fGFTRSHIP(1:3,11,IY)) .NE. 0.0) &
            T7(80,IY,IS) = (T7(74,IY,IS) * (fGFTRFV(11,IY) + fGFTRPV(11,IY)) + &
                            T7(75,IY,IS) * sum(fGFTRRAIL(1:4,11,IY)) + &
                            T7(76,IY,IS) * sum(fGFTRSHIP(1:3,11,IY))) / &
                    (fGFTRFV(11,IY)+fGFTRPV(11,IY)+sum(fGFTRRAIL(1:4,11,IY))+sum(fGFTRSHIP(1:3,11,IY)))
        IF (fGLTRFV(11,IY)+fGLTRPV(11,IY)+sum(fGLTRRAIL(1:4,11,IY))+sum(fGLTRSHIP(1:3,11,IY)) .NE. 0.0) &
            T7(81,IY,IS) = (T7(77,IY,IS) * (fGLTRFV(11,IY) + fGLTRPV(11,IY)) + &
                            T7(78,IY,IS) * sum(fGLTRRAIL(1:4,11,IY)) + &
                            T7(79,IY,IS) * sum(fGLTRSHIP(1:3,11,IY))) / &
                    (fGLTRFV(11,IY)+fGLTRPV(11,IY)+sum(fGLTRRAIL(1:4,11,IY))+sum(fGLTRSHIP(1:3,11,IY)))
        IF (fGFTRFV(11,IY)+fGFTRPV(11,IY)+sum(fGFTRRAIL(1:4,11,IY))+sum(fGFTRSHIP(1:3,11,IY))+ &
            fGLTRFV(11,IY)+fGLTRPV(11,IY)+sum(fGLTRRAIL(1:4,11,IY))+sum(fGLTRSHIP(1:3,11,IY)) .NE. 0.0) &
            T7(82,IY,IS) = &
               (T7(80,IY,IS) * (fGFTRFV(11,IY)+fGFTRPV(11,IY)+sum(fGFTRRAIL(1:4,11,IY))+sum(fGFTRSHIP(1:3,11,IY))) + &
                T7(81,IY,IS) * (fGLTRFV(11,IY)+fGLTRPV(11,IY)+sum(fGLTRRAIL(1:4,11,IY))+sum(fGLTRSHIP(1:3,11,IY))))/ &
                 (fGFTRFV(11,IY)+fGFTRPV(11,IY)+sum(fGFTRRAIL(1:4,11,IY))+sum(fGFTRSHIP(1:3,11,IY)) + &
                  fGLTRFV(11,IY)+fGLTRPV(11,IY)+sum(fGLTRRAIL(1:4,11,IY))+sum(fGLTRSHIP(1:3,11,IY)))
  70  CONTINUE

! TABLE 8.  Electricity Supply, Disposition, and Prices

      DO 80 IY=1,LASTYR

!       --- GENERATION BY FUEL TYPE (ADD NONTRADITIONAL COGEN FOR AEO03)
        T8( 1,IY,IS) = UGNCLNR(1,mnumnr,IY) + UGNCLNR(2,mnumnr,IY)
        T8( 2,IY,IS) = UGNDSNR(1,mnumnr,IY) + UGNDSNR(2,mnumnr,IY) + &
                       UGNRHNR(1,mnumnr,IY) + UGNRHNR(2,mnumnr,IY) + &
                       UGNRLNR(1,mnumnr,IY) + UGNRLNR(2,mnumnr,IY)
        T8( 3,IY,IS) = UGNGFNR(1,mnumnr,IY) + UGNGFNR(2,mnumnr,IY) + &
                       UGNGINR(1,mnumnr,IY) + UGNGINR(2,mnumnr,IY) + &
                       UGNGCNR(1,mnumnr,IY) + UGNGCNR(2,mnumnr,IY)
        T8( 4,IY,IS) = UGNURNR(1,mnumnr,IY) + UGNURNR(2,mnumnr,IY)
        T8( 5,IY,IS) = UGNPSNR(1,mnumnr,IY) + UGNPSNR(2,mnumnr,IY)  + &
                       UGNSDNR(1,mnumnr,IY) + UGNSDNR(2,mnumnr,IY)
        T8(37,IY,IS) = 0.0  ! UGNFCNR(1,mnumnr,IY) + UGNFCNR(2,mnumnr,IY)
        T8( 6,IY,IS) = UGNHYNR(1,MNUMNR,IY) + UGNHYNR(2,MNUMNR,IY) + &
                       UGNGENR(1,MNUMNR,IY) + UGNGENR(2,MNUMNR,IY) + &
                       UGNMSNR(1,MNUMNR,IY) + UGNMSNR(2,MNUMNR,IY) + &
                       UGNWDNR(1,MNUMNR,IY) + UGNWDNR(2,MNUMNR,IY) + &
                       UGNSONR(1,MNUMNR,IY) + UGNSONR(2,MNUMNR,IY) + &
                       UGNPVNR(1,MNUMNR,IY) + UGNPVNR(2,MNUMNR,IY) + &
                       UGNPTNR(1,MNUMNR,IY) + UGNPTNR(2,MNUMNR,IY) + &
                       UGNWNNR(1,MNUMNR,IY) + UGNWNNR(2,MNUMNR,IY) + &
                       UGNWLNR(1,MNUMNR,IY) + UGNWLNR(2,MNUMNR,IY) + &
                       UGNWFNR(1,MNUMNR,IY) + UGNWFNR(2,MNUMNR,IY)
        T8(41,IY,IS) = UGNDDNR(1,mnumnr,IY) + UGNDDNR(2,mnumnr,IY) + &
                       UGNDGNR(1,mnumnr,IY) + UGNDGNR(2,mnumnr,IY)
        T8( 8,IY,IS) = FSUM(T8( 1,IY,IS),6) + T8(37,IY,IS) + T8(41,IY,IS)
! Nontraditional cogeneration
        T8(42,IY,IS) =(CGNTGEN(mnumnr,IY, 1,1) + CGNTGEN(mnumnr,IY, 1,2)) * 0.001
        T8(43,IY,IS) =(CGNTGEN(mnumnr,IY, 2,1) + CGNTGEN(mnumnr,IY, 2,2)) * 0.001
        T8(44,IY,IS) =(CGNTGEN(mnumnr,IY, 3,1) + CGNTGEN(mnumnr,IY, 3,2)) * 0.001
        T8(45,IY,IS) =(CGNTGEN(mnumnr,IY, 9,1) + CGNTGEN(mnumnr,IY, 9,2) + &
                       CGNTGEN(mnumnr,IY,10,1) + CGNTGEN(mnumnr,IY,10,2)) * 0.001
        T8(46,IY,IS) =(CGNTGEN(mnumnr,IY, 4,1) + CGNTGEN(mnumnr,IY, 4,2) + &
                       CGNTGEN(mnumnr,IY, 5,1) + CGNTGEN(mnumnr,IY, 5,2) + &
                       CGNTGEN(mnumnr,IY, 6,1) + CGNTGEN(mnumnr,IY, 6,2) + &
                       CGNTGEN(mnumnr,IY, 7,1) + CGNTGEN(mnumnr,IY, 7,2) + &
                       CGNTGEN(mnumnr,IY, 8,1) + CGNTGEN(mnumnr,IY, 8,2)) * 0.001
        T8(48,IY,IS) = FSUM(T8(42,IY,IS),5)
        T8(55,IY,IS) = T8( 8,IY,IS) + T8(48,IY,IS)
        T8(47,IY,IS) = CGOTGEN(mnumnr,IY,1) + CGOTGEN(mnumnr,IY,2)
        T8(49,IY,IS) = T8( 8,IY,IS) + T8(48,IY,IS) - T8(47,IY,IS)

!       --- COGENERATORS
        T8( 9,IY,IS) = (CGOGSGEN(11,IY,1,1)  + CGOGSGEN(11,IY,1,2)  + &
                        CGINDLGEN(11,IY,1,1) + CGINDLGEN(11,IY,1,2) + &
                        CGCOMMGEN(11,IY,1,1) + CGCOMMGEN(11,IY,1,2) + &
                        CGREFGEN(11,IY,1,1)  + CGREFGEN(11,IY,1,2)) * .001
        T8(10,IY,IS) = (CGREFGEN(11,IY,2,1)  + CGREFGEN(11,IY,2,2)  + &
                        CGOGSGEN(11,IY,2,1)  + CGOGSGEN(11,IY,2,2) + &
                        CGINDLGEN(11,IY,2,1) + CGINDLGEN(11,IY,2,2) + &
                        CGCOMMGEN(11,IY,2,1) + CGCOMMGEN(11,IY,2,2)) * .001
        T8(11,IY,IS) = (CGREFGEN(11,IY,3,1)  + CGREFGEN(11,IY,3,2)  + &
                        CGOGSGEN(11,IY,3,1)  + CGOGSGEN(11,IY,3,2)  + &
                        CGINDLGEN(11,IY,3,1) + CGINDLGEN(11,IY,3,2) + &
                        CGCOMMGEN(11,IY,3,1) + CGCOMMGEN(11,IY,3,2) + &
                        CGRESGEN(11,IY,3,1)  + CGRESGEN(11,IY,3,2)) * .001
        T8(12,IY,IS) = (CGREFGEN(11,IY,9,1)   + CGREFGEN(11,IY,9,2) + &
                        CGINDLGEN(11,IY,9,1) + CGINDLGEN(11,IY,9,2) + &
                        CGCOMMGEN(11,IY,9,1) + CGCOMMGEN(11,IY,9,2)) * .001
        T8(13,IY,IS) = (CGREFGEN(11,IY,6,1)   + CGREFGEN(11,IY,6,2) + &
                        CGREFGEN(11,IY,7,1)   + CGREFGEN(11,IY,7,2) + &
                        CGINDLGEN(11,IY,6,1) + CGINDLGEN(11,IY,7,1) + &
                        CGINDLGEN(11,IY,6,2) + CGINDLGEN(11,IY,7,2) + &
                        CGCOMMGEN(11,IY,6,1) + CGCOMMGEN(11,IY,7,1) + &
                        CGCOMMGEN(11,IY,6,2) + CGCOMMGEN(11,IY,7,2)) * .001
        T8(14,IY,IS) = (CGOGSGEN(11,IY,4,1)  + CGOGSGEN(11,IY,4,2) + &
                        CGINDLGEN(11,IY,10,1)+ CGINDLGEN(11,IY,10,2) + &
                        CGCOMMGEN(11,IY,10,1)+ CGCOMMGEN(11,IY,10,2)) * .001
        T8(15,IY,IS) = FSUM(T8(9,IY,IS),6)
!        --- Other Generators
         T8(18,IY,IS)=(CGCOMMGEN(11,IY,8,1) + CGCOMMGEN(11,IY,8,2) + &
                       CGRESGEN(11,IY,8,1)  + CGRESGEN(11,IY,8,2) + &
                       CGRESGEN(11,IY,11,1) + CGRESGEN(11,IY,11,2) + &
                       CGINDLGEN(11,IY,8,1) + CGINDLGEN(11,IY,8,2) + &
                       CGINDLGEN(11,IY,11,1)+ CGINDLGEN(11,IY,11,2) + &
                       CGCOMMGEN(11,IY,4,1) + CGCOMMGEN(11,IY,4,2) + &
                       CGINDLGEN(11,IY,4,1) + CGINDLGEN(11,IY,4,2) + &
                       CGCOMMGEN(11,IY,5,1) + CGCOMMGEN(11,IY,5,2) + &
                       CGCOMMGEN(11,IY,11,1)+ CGCOMMGEN(11,IY,11,2) + &
                       CGINDLGEN(11,IY,5,1) + CGINDLGEN(11,IY,5,2)) * .001
! Include Other Generators with Renewable CHP
        T8(13,IY,IS) = T8(13,IY,IS) + T8(18,IY,IS)
        T8(15,IY,IS) = T8(15,IY,IS) + T8(18,IY,IS)
!  Sales to utilities
        T8(16,IY,IS) = CGREFGEN(11,IY,1,1)   + CGREFGEN(11,IY,2,1) + &
                       CGREFGEN(11,IY,3,1)   + CGREFGEN(11,IY,6,1) + &
                       CGREFGEN(11,IY,7,1)   + CGREFGEN(11,IY,9,1)    + &
                       CGINDLGEN(11,IY, 1,1) + CGINDLGEN(11,IY, 2,1) + &
                       CGINDLGEN(11,IY, 3,1) + CGINDLGEN(11,IY, 4,1) + &
                       CGINDLGEN(11,IY, 5,1) + CGINDLGEN(11,IY, 6,1) + &
                       CGINDLGEN(11,IY, 7,1) + CGINDLGEN(11,IY, 8,1) + &
                       CGINDLGEN(11,IY, 9,1) + CGINDLGEN(11,IY,10,1) + &
                       CGINDLGEN(11,IY,11,1) + &
                       CGOGSGEN(11,IY, 1,1)  + CGOGSGEN(11,IY, 2,1) + &
                       CGOGSGEN(11,IY, 3,1)  + CGOGSGEN(11,IY, 4,1) + &
                       CGRESGEN(11,IY,3,1)   + &
                       CGRESGEN(11,IY,8,1)   + CGRESGEN(11,IY,11,1) + &
                       CGCOMMGEN(11,IY, 1,1) + CGCOMMGEN(11,IY, 2,1) + &
                       CGCOMMGEN(11,IY, 3,1) + CGCOMMGEN(11,IY, 4,1) + &
                       CGCOMMGEN(11,IY, 5,1) + CGCOMMGEN(11,IY, 6,1) + &
                       CGCOMMGEN(11,IY, 7,1) + CGCOMMGEN(11,IY, 8,1) + &
                       CGCOMMGEN(11,IY, 9,1) + CGCOMMGEN(11,IY,10,1) + &
                       CGCOMMGEN(11,IY,11,1) + CGCOMMGEN(11,IY,12,1)
        T8(16,IY,IS) = T8(16,IY,IS) * .001
! Generation for own use
        T8(17,IY,IS) = CGREFGEN(11,IY, 1,2)  + CGREFGEN(11,IY, 2,2) + &
                       CGREFGEN(11,IY, 3,2)  + CGREFGEN(11,IY, 6,2) + &
                       CGREFGEN(11,IY, 7,2)  + CGREFGEN(11,IY, 9,2)   + &
                       CGINDLGEN(11,IY, 1,2) + CGINDLGEN(11,IY, 2,2) + &
                       CGINDLGEN(11,IY, 3,2) + CGINDLGEN(11,IY, 4,2) + &
                       CGINDLGEN(11,IY, 5,2) + CGINDLGEN(11,IY, 6,2) + &
                       CGINDLGEN(11,IY, 7,2) + CGINDLGEN(11,IY, 8,2) + &
                       CGINDLGEN(11,IY, 9,2) + CGINDLGEN(11,IY,10,2) + &
                       CGINDLGEN(11,IY,11,2) + &
                       CGOGSGEN(11,IY, 1,2)  + CGOGSGEN(11,IY, 2,2) + &
                       CGOGSGEN(11,IY, 3,2)  + CGOGSGEN(11,IY, 4,2) + &
                       CGRESGEN(11,IY,3,2)   + &
                       CGRESGEN(11,IY,8,2)   + CGRESGEN(11,IY,11,2) + &
                       CGCOMMGEN(11,IY, 1,2) + CGCOMMGEN(11,IY, 2,2) + &
                       CGCOMMGEN(11,IY, 3,2) + CGCOMMGEN(11,IY, 4,2) + &
                       CGCOMMGEN(11,IY, 5,2) + CGCOMMGEN(11,IY, 6,2) + &
                       CGCOMMGEN(11,IY, 7,2) + CGCOMMGEN(11,IY, 8,2) + &
                       CGCOMMGEN(11,IY, 9,2) + CGCOMMGEN(11,IY,10,2) + &
                       CGCOMMGEN(11,IY,11,2) + CGCOMMGEN(11,IY,12,2)
        T8(17,IY,IS) = T8(17,IY,IS) * 0.001
        T8(56,IY,IS) = T8( 8,IY,IS) + T8(48,IY,IS) + T8(15,IY,IS)
        T8(52,IY,IS) = T8(49,IY,IS) + T8(16,IY,IS)
!       --- NET IMPORTS
        T8(19,IY,IS) = (UTIMPF(mnumnr,IY) + UTIMPE(mnumnr,IY) - UTEXPF(mnumnr,IY) - UTEXPE(mnumnr,IY)) * .001
! --- CONVERSIONS:  "/.003412" CONVERTS FROM QUADS TO BKWH USING 3,412 BTUS/KWH
!       --- Electricity Sales
        T8(20,IY,IS) = QELRS(11,IY) / .003412
        T8(21,IY,IS) = QELCM(11,IY) / .003412
        T8(22,IY,IS) = QELIN(11,IY) / .003412
        T8(23,IY,IS) = QELTR(11,IY) / .003412
        T8(24,IY,IS) = QELAS(11,IY) / .003412
!       --- TOTAL DIRECT USE AND END-USE
        T8(53,IY,IS) = T8(47,IY,IS) + T8(17,IY,IS)
        T8(54,IY,IS) = T8(24,IY,IS) + T8(53,IY,IS)
!       --- End Use Prices
        T8(25,IY,IS) = PELRS(11,IY) * .3412
        T8(26,IY,IS) = PELCM(11,IY) * .3412
        T8(27,IY,IS) = PELIN(11,IY) * .3412
        T8(28,IY,IS) = PELTR(11,IY) * .3412
        T8(29,IY,IS) = PELAS(11,IY) * .3412
!       --- EMISSIONS (SO2 & NOX)
        T8(30,IY,IS) = sum(UTTSO2(1:NUTSEC,IY)) + &
                      (sum(so2other(IY,1:NUM_SO2_GRP))+sum(CTLSO2EM(1:NDREG,IY))) / 1000000.
        T8(31,IY,IS) = UNOXOTR(mnumnr,IY) + UNOXINR(mnumnr,IY) + sum(CTLNOXEM(1:NDREG,IY)) / 1000000.
        T8(50,IY,IS) = TOT_MERC(IY) / 2000.0 + sum(CTLHGEM(1:NDREG,IY))
        T8(51,IY,IS) = 0.0         ! this row was carbon emissions long ago

        T8(38,IY,IS) = PECGENN(mnumnr,IY) * .1
        T8(39,IY,IS) = PECTRNN(mnumnr,IY) * .1
        T8(40,IY,IS) = PECDISN(mnumnr,IY) * .1

! Generation by Everybody
        T8(57,IY,IS) = T8( 1,IY,IS) + T8(42,IY,IS) + T8( 9,IY,IS)
        T8(58,IY,IS) = T8( 2,IY,IS) + T8(43,IY,IS) + T8(10,IY,IS)
        T8(59,IY,IS) = T8( 3,IY,IS) + T8(41,IY,IS) + T8(44,IY,IS) + T8(11,IY,IS)
        T8(60,IY,IS) = T8( 4,IY,IS)
        T8(61,IY,IS) = T8( 6,IY,IS) + T8(46,IY,IS) + T8(13,IY,IS)
        T8(62,IY,IS) = T8( 5,IY,IS) + T8(45,IY,IS) + T8(12,IY,IS) + T8(14,IY,IS)
        T8(63,IY,IS) = FSUM(T8(57,IY,IS),6)
        T8(64,IY,IS) = (UTIMPF(mnumnr,IY) + UTIMPE(mnumnr,IY)) * .001
        T8(65,IY,IS) =-(UTEXPF(mnumnr,IY) + UTEXPE(mnumnr,IY)) * .001
        T8(66,IY,IS) = T8(63,IY,IS) + T8(64,IY,IS)
        T8(69,IY,IS) =(UTDMMF(mnumnr,IY) + UTDMME(mnumnr,IY)) / 1000.
        T8(70,IY,IS) =-T8(53,IY,IS)
        T8(67,IY,IS) = T8(66,IY,IS) + T8(65,IY,IS) + T8(70,IY,IS) + T8(69,IY,IS)
        T8(68,IY,IS) = T8(67,IY,IS) - T8(24,IY,IS)

! nominal prices

        T8(32,IY,IS) = T8(25,IY,IS) / SCALPR * MC_JPGDP(IY)
        T8(33,IY,IS) = T8(26,IY,IS) / SCALPR * MC_JPGDP(IY)
        T8(34,IY,IS) = T8(27,IY,IS) / SCALPR * MC_JPGDP(IY)
        T8(35,IY,IS) = T8(28,IY,IS) / SCALPR * MC_JPGDP(IY)
        T8(36,IY,IS) = T8(29,IY,IS) / SCALPR * MC_JPGDP(IY)
        T8(71,IY,IS) = T8(38,IY,IS) / SCALPR * MC_JPGDP(IY)
        T8(72,IY,IS) = T8(39,IY,IS) / SCALPR * MC_JPGDP(IY)
        T8(73,IY,IS) = T8(40,IY,IS) / SCALPR * MC_JPGDP(IY)

! the below is to not print history for the electricity price components
!        IF (IY .LE. (CUMCAPADD - 1989)) THEN    ! first published number will be 2014 for AEO2015
!           T8(38:40,IR,IY,IS) = -999.
!           T8(71:73,IR,IY,IS) = -999.
!        END IF

80    CONTINUE

!  TABLE 9.  ELECTRICITY GENERATING CAPABILITY (INCLUDES NONTRADITIONAL COGEN FOR AEO03)

      RUNTOTS = 0.0
      DO 90 IY=1,LASTYR
        T9( 1,IY,IS) = UCAPCSU(mnumnr,IY) + UCAPCSN(mnumnr,IY)
        T9( 2,IY,IS) = UCAPOSU(mnumnr,IY) + UCAPOSN(mnumnr,IY) + UCAPNGU(mnumnr,IY) + UCAPNGN(mnumnr,IY)
        T9( 3,IY,IS) = UCAPCCU(mnumnr,IY) + UCAPCCN(mnumnr,IY)
        T9( 4,IY,IS) = UCAPCTU(mnumnr,IY) + UCAPCTN(mnumnr,IY)
        T9( 5,IY,IS) = UCAPNUU(mnumnr,IY) + UCAPNUN(mnumnr,IY) + UCAPSMU(mnumnr,IY) + UCAPSMN(mnumnr,IY)
        T9( 6,IY,IS) = UCAPPSU(mnumnr,IY) + UCAPPSN(mnumnr,IY)
        T9(72,IY,IS) = UCAPDSU(mnumnr,IY) + UCAPDSN(mnumnr,IY)
        T9( 7,IY,IS) = UCAPFCU(mnumnr,IY) + UCAPFCN(mnumnr,IY)
        T9( 8,IY,IS) = UCAPHYU(mnumnr,IY) + UCAPGEU(mnumnr,IY) + &
                       UCAPMSU(mnumnr,IY) + UCAPWDU(mnumnr,IY) + &
                       UCAPSTU(mnumnr,IY) + UCAPPVU(mnumnr,IY) + &
                       UCAPWNU(mnumnr,IY) + UCAPWFU(mnumnr,IY) + &
                       UCAPPTU(mnumnr,IY) + UCAPWLU(mnumnr,IY) + &
                       UCAPHYN(mnumnr,IY) + UCAPGEN(mnumnr,IY) + &
                       UCAPMSN(mnumnr,IY) + UCAPWDN(mnumnr,IY) + &
                       UCAPSTN(mnumnr,IY) + UCAPPVN(mnumnr,IY) + &
                       UCAPWNN(mnumnr,IY) + UCAPWFN(mnumnr,IY) + &
                       UCAPPTN(mnumnr,IY) + UCAPWLN(mnumnr,IY)
        T9(48,IY,IS) = UCAPDBU(mnumnr,IY) + UCAPDBN(mnumnr,IY) + &
                       UCAPDPU(mnumnr,IY) + UCAPDPN(mnumnr,IY)
        T9( 9,IY,IS) = FSUM(T9( 1,IY,IS),8) + T9(48,IY,IS) + T9(72,IY,IS)
! Nontraditional cogeneration capacity
        T9(51,IY,IS) = UCAPCSC(mnumnr,IY)
        T9(52,IY,IS) = UCAPOSC(mnumnr,IY)
        T9(53,IY,IS) = UCAPCCC(mnumnr,IY)
        T9(54,IY,IS) = UCAPCTC(mnumnr,IY)
        T9(55,IY,IS) = UCAPHYC(mnumnr,IY) + UCAPGEC(mnumnr,IY) + &
                       UCAPMSC(mnumnr,IY) + UCAPWDC(mnumnr,IY) + &
                       UCAPSTC(mnumnr,IY) + UCAPPVC(mnumnr,IY) + &
                       UCAPWNC(mnumnr,IY) + UCAPWFC(mnumnr,IY) + &
                       UCAPWLC(mnumnr,IY) + UCAPPTC(mnumnr,IY)
        T9(56,IY,IS) = FSUM(T9(51,IY,IS),5)
        T9(57,IY,IS) = T9( 9,IY,IS) + T9(56,IY,IS)
! --- CUMULATIVE PLANNED ADDITIONS
         T9(10,IY,IS) = UADDCSU(1,mnumnr,IY) + UADDCSN(1,mnumnr,IY) + UADDCSC(mnumnr,IY)
         T9(11,IY,IS) = UADDOSU(1,mnumnr,IY) + UADDOSN(1,mnumnr,IY) + UADDOSC(mnumnr,IY)
         T9(12,IY,IS) = UADDCCU(1,mnumnr,IY) + UADDCCN(1,mnumnr,IY) + UADDCCC(mnumnr,IY)
         T9(13,IY,IS) = UADDCTU(1,mnumnr,IY) + UADDCTN(1,mnumnr,IY) + UADDCTC(mnumnr,IY)
         T9(14,IY,IS) = UADDNUU(1,mnumnr,IY) + UADDNUN(1,mnumnr,IY) + &
                        UADDSMU(1,mnumnr,IY) + UADDSMN(1,mnumnr,IY)
         T9(15,IY,IS) = UADDPSU(1,mnumnr,IY) + UADDPSN(1,mnumnr,IY)
         T9(73,IY,IS) = UADDDSU(1,mnumnr,IY) + UADDDSN(1,mnumnr,IY)
         T9(16,IY,IS) = UADDFCU(1,mnumnr,IY) + UADDFCN(1,mnumnr,IY)
         T9(17,IY,IS) = UADDRNU(1,mnumnr,IY) + UADDRNN(1,mnumnr,IY) + &
                        UADDHYC(mnumnr,IY) + UADDGEC(mnumnr,IY) + &
                        UADDMSC(mnumnr,IY) + UADDWDC(mnumnr,IY) + &
                        UADDSTC(mnumnr,IY) + UADDPVC(mnumnr,IY) + &
                        UADDWNC(mnumnr,IY) + UADDWFC(mnumnr,IY)
         T9(49,IY,IS) = UADDDBU(1,mnumnr,IY) + UADDDBN(1,mnumnr,IY) + &
                        UADDDPU(1,mnumnr,IY) + UADDDPN(1,mnumnr,IY)
         T9(18,IY,IS) = UADDTLU(1,mnumnr,IY) + UADDTLN(1,mnumnr,IY) + UADDTLC(mnumnr,IY)
!        --- CUMULATIVE UNPLANNED ADDITIONS
         T9(19,IY,IS) = UADDCSU(2,mnumnr,IY) + UADDCSN(2,mnumnr,IY)
         T9(20,IY,IS) = UADDOSU(2,mnumnr,IY) + UADDOSN(2,mnumnr,IY)
         T9(21,IY,IS) = UADDCCU(2,mnumnr,IY) + UADDCCN(2,mnumnr,IY)
         T9(22,IY,IS) = UADDCTU(2,mnumnr,IY) + UADDCTN(2,mnumnr,IY)
         T9(23,IY,IS) = UADDNUU(2,mnumnr,IY) + UADDNUN(2,mnumnr,IY) + &
                        UADDSMU(2,mnumnr,IY) + UADDSMN(2,mnumnr,IY)
         T9(24,IY,IS) = UADDPSU(2,mnumnr,IY) + UADDPSN(2,mnumnr,IY)
         T9(74,IY,IS) = UADDDSU(2,mnumnr,IY) + UADDDSN(2,mnumnr,IY)
         T9(25,IY,IS) = UADDFCU(2,mnumnr,IY) + UADDFCN(2,mnumnr,IY)
         T9(26,IY,IS) = UADDRNU(2,mnumnr,IY) + UADDRNN(2,mnumnr,IY)
         T9(50,IY,IS) = UADDDBU(2,mnumnr,IY) + UADDDBN(2,mnumnr,IY) + &
                        UADDDPU(2,mnumnr,IY) + UADDDPN(2,mnumnr,IY)
         T9(27,IY,IS) = UADDTLU(2,mnumnr,IY) + UADDTLN(2,mnumnr,IY)
!        --- Totals
         T9(28,IY,IS) = UADDTLU(1,mnumnr,IY) + UADDTLU(2,mnumnr,IY) + UADDTLC(mnumnr,IY) + &
                        UADDTLN(1,mnumnr,IY) + UADDTLN(2,mnumnr,IY)
!        --- CUMULATIVE RETIREMENTS
         T9(29,IY,IS) = URETCSU(mnumnr,IY)
         T9(30,IY,IS) = URETOSU(mnumnr,IY)
         T9(31,IY,IS) = URETCCU(mnumnr,IY)
         T9(32,IY,IS) = URETCTU(mnumnr,IY)
         T9(33,IY,IS) = URETNUU(mnumnr,IY) + URETSMU(mnumnr,IY)
         T9(34,IY,IS) = URETPSU(mnumnr,IY)
         T9(75,IY,IS) = URETDSU(mnumnr,IY)
         T9(35,IY,IS) = URETFCU(mnumnr,IY)
         T9(36,IY,IS) = URETRNU(mnumnr,IY)
         T9(37,IY,IS) = URETTLU(mnumnr,IY)
! --- COGENERATORS
         T9(38,IY,IS) =(CGREFCAP(11,IY,1) + &
                        CGOGSCAP(11,IY,1) + CGINDLCAP(11,IY,1) + &
                        CGCOMMCAP(11,IY,1)) * .001
         T9(39,IY,IS) =(CGREFCAP(11,IY,2) + &
                        CGOGSCAP(11,IY,2) + CGINDLCAP(11,IY,2) + &
                        CGCOMMCAP(11,IY,2)) * .001
         T9(40,IY,IS) =(CGREFCAP(11,IY,3) + &
                        CGOGSCAP(11,IY,3) + CGINDLCAP(11,IY,3) + &
                        CGCOMMCAP(11,IY,3) + &
                        CGRESCAP(11,IY,3)) * .001
         T9(41,IY,IS) =(CGINDLCAP(11,IY,9) + &
                        CGREFCAP(11,IY,9) + &
                        CGCOMMCAP(11,IY,9)) * .001
         T9(42,IY,IS) =(CGREFCAP(11,IY,6)  + CGREFCAP(11,IY,7) + &
                        CGINDLCAP(11,IY,6) + CGINDLCAP(11,IY,7) + &
                        CGCOMMCAP(11,IY,6) + &
                        CGCOMMCAP(11,IY,7)) * .001
         T9(43,IY,IS) =(CGOGSCAP(11,IY,4) + CGINDLCAP(11,IY,10) + &
                        CGCOMMCAP(11,IY,10)) *.001
         T9(44,IY,IS) = FSUM(T9(38,IY,IS),6)
!        --- Other Generators
         T9(46,IY,IS) =(CGRESCAP(11,IY,8) + CGRESCAP(11,IY,11) + &
                      CGCOMMCAP(11,IY,8) + &
                      CGINDLCAP(11,IY,8) + CGINDLCAP(11,IY,4) + &
                      CGCOMMCAP(11,IY,4) + &
                      CGCOMMCAP(11,IY,5) + &
                      CGCOMMCAP(11,IY,11) + &
                      CGINDLCAP(11,IY,5) + CGINDLCAP(11,IY,11)) / 1000.
! --- ADD OTHER END-USE GENERATORS TO RENEWABLES AND TOTAL CHP
         T9(42,IY,IS) = T9(42,IY,IS) + T9(46,IY,IS)
         T9(44,IY,IS) = T9(44,IY,IS) + T9(46,IY,IS)

!  capacity of everybody
         T9(58,IY,IS) = T9( 1,IY,IS) + T9(51,IY,IS) + T9(38,IY,IS)
         T9(59,IY,IS) = T9( 2,IY,IS) + T9(52,IY,IS) + T9(39,IY,IS)
         T9(60,IY,IS) = T9( 3,IY,IS) + T9( 4,IY,IS) + T9( 7,IY,IS) + T9(48,IY,IS) + &
                        T9(53,IY,IS) + T9(54,IY,IS) + T9(40,IY,IS)
         T9(61,IY,IS) = T9( 5,IY,IS)
         T9(62,IY,IS) = T9( 8,IY,IS) + T9(55,IY,IS) + T9(42,IY,IS)
         T9(63,IY,IS) = T9( 6,IY,IS) + T9(41,IY,IS) + T9(43,IY,IS)
         T9(64,IY,IS) = FSUM(T9(58,IY,IS),6)
!  capacity additions.  first we do end use sector additions, so we can go through the variables
!  only once.  then we sum it up for the end use capacity additions (published). then we tack on
!  the electric power sector additions for the bonus rows which are by (approximate) fuel.
!  as a reminder, tehse are the fuel indices for most (all but OGS) of the chap variables:
!            1=Coal            2=Petroleum      3=Natural Gas     4=Hydropower
!            5=Geothermal      6=MSW            7=Biomass         8=Solar Photovoltaic
!            9=Other Gaseous  10=Other         11=Wind           12=Solar Thermal
!  RUNTOTS fuels are:  1=coal, 2=oil, 3=natural gas, 4=nuclear, 5=renewables, 6=other
         T9(65:71,IY,IS) = 0.0
         T9(45,IY,IS) = 0.0
         IF (IY .GT. (CUMCAPADD - 1989)) THEN
         RUNTOTS(:,IY) = RUNTOTS(:,IY-1)
         DO IR=1,MNUMCR-2
! coal
               IF (CGREFCAP(IR,IY,1) .GT. CGREFCAP(IR,IY-1,1)) &
                  RUNTOTS(1,IY) = RUNTOTS(1,IY) + CGREFCAP(IR,IY,1) - CGREFCAP(IR,IY-1,1)
               IF (CGOGSCAP(IR,IY,1) .GT. CGOGSCAP(IR,IY-1,1)) &
                  RUNTOTS(1,IY) = RUNTOTS(1,IY) + CGOGSCAP(IR,IY,1) - CGOGSCAP(IR,IY-1,1)
               IF (CGINDLCAP(IR,IY,1) .GT. CGINDLCAP(IR,IY-1,1)) &
                  RUNTOTS(1,IY) = RUNTOTS(1,IY) + CGINDLCAP(IR,IY,1) - CGINDLCAP(IR,IY-1,1)
               IF (CGCOMMCAP(IR,IY,1) .GT. CGCOMMCAP(IR,IY-1,1)) &
                  RUNTOTS(1,IY) = RUNTOTS(1,IY) + CGCOMMCAP(IR,IY,1) - CGCOMMCAP(IR,IY-1,1)
! oil
               IF (CGREFCAP(IR,IY,2) .GT. CGREFCAP(IR,IY-1,2)) &
                  RUNTOTS(2,IY) = RUNTOTS(2,IY) + CGREFCAP(IR,IY,2) - CGREFCAP(IR,IY-1,2)
               IF (CGOGSCAP(IR,IY,2) .GT. CGOGSCAP(IR,IY-1,2)) &
                  RUNTOTS(2,IY) = RUNTOTS(2,IY) + CGOGSCAP(IR,IY,2) - CGOGSCAP(IR,IY-1,2)
               IF (CGINDLCAP(IR,IY,2) .GT. CGINDLCAP(IR,IY-1,2)) &
                  RUNTOTS(2,IY) = RUNTOTS(2,IY) + CGINDLCAP(IR,IY,2) - CGINDLCAP(IR,IY-1,2)
               IF (CGCOMMCAP(IR,IY,2) .GT. CGCOMMCAP(IR,IY,2)) &
                  RUNTOTS(2,IY) = RUNTOTS(2,IY) + CGCOMMCAP(IR,IY,2) - CGCOMMCAP(IR,IY,2)
! natural gas
               IF (CGREFCAP(IR,IY,3) .GT. CGREFCAP(IR,IY-1,3)) &
                  RUNTOTS(3,IY) = RUNTOTS(3,IY) + CGREFCAP(IR,IY,3) - CGREFCAP(IR,IY-1,3)
               IF (CGOGSCAP(IR,IY,3) .GT. CGOGSCAP(IR,IY-1,3)) &
                  RUNTOTS(3,IY) = RUNTOTS(3,IY) + CGOGSCAP(IR,IY,3) - CGOGSCAP(IR,IY-1,3)
               IF (CGINDLCAP(IR,IY,3) .GT. CGINDLCAP(IR,IY-1,3)) &
                  RUNTOTS(3,IY) = RUNTOTS(3,IY) + CGINDLCAP(IR,IY,3) - CGINDLCAP(IR,IY-1,3)
               IF (CGCOMMCAP(IR,IY,3) .GT. CGCOMMCAP(IR,IY-1,3)) &
                  RUNTOTS(3,IY) = RUNTOTS(3,IY) + CGCOMMCAP(IR,IY,3) - CGCOMMCAP(IR,IY-1,3)
               IF (CGRESCAP(IR,IY,3) .GT. CGRESCAP(IR,IY-1,3)) &
                  RUNTOTS(3,IY) = RUNTOTS(3,IY) + CGRESCAP(IR,IY,3) - CGRESCAP(IR,IY-1,3)
! RUNTOTS(4,year) would be nuclear.  The end use sectors will get some eventually.
! renewables
               IF (CGREFCAP(IR,IY,6) .GT. CGREFCAP(IR,IY-1,6)) &
                  RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGREFCAP(IR,IY,6) - CGREFCAP(IR,IY-1,6)
               IF (CGREFCAP(IR,IY,7) .GT. CGREFCAP(IR,IY-1,7)) &
                  RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGREFCAP(IR,IY,7) - CGREFCAP(IR,IY-1,7)
               IF (CGINDLCAP(IR,IY,6) .GT. CGINDLCAP(IR,IY-1,6)) &
                  RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGINDLCAP(IR,IY,6) - CGINDLCAP(IR,IY-1,6)
               IF (CGINDLCAP(IR,IY,7) .GT. CGINDLCAP(IR,IY-1,7)) &
                  RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGINDLCAP(IR,IY,7) - CGINDLCAP(IR,IY-1,7)
               IF (CGCOMMCAP(IR,IY,6) .GT. CGCOMMCAP(IR,IY-1,6)) &
                  RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGCOMMCAP(IR,IY,6) - CGCOMMCAP(IR,IY-1,6)
               IF (CGCOMMCAP(IR,IY,7) .GT. CGCOMMCAP(IR,IY-1,7)) &
                  RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGCOMMCAP(IR,IY,7) - CGCOMMCAP(IR,IY-1,7)
               IF (CGRESCAP(IR,IY,8) .GT. CGRESCAP(IR,IY-1,8)) &
                  RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGRESCAP(IR,IY,8) - CGRESCAP(IR,IY-1,8)
               IF (CGRESCAP(IR,IY,11) .GT. CGRESCAP(IR,IY-1,11)) &
                  RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGRESCAP(IR,IY,11) - CGRESCAP(IR,IY-1,11)
               IF (CGCOMMCAP(IR,IY,8) .GT. CGCOMMCAP(IR,IY-1,8)) &
                  RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGCOMMCAP(IR,IY,8) - CGCOMMCAP(IR,IY-1,8)
               IF (CGINDLCAP(IR,IY,8) .GT. CGINDLCAP(IR,IY-1,8)) &
                  RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGINDLCAP(IR,IY,8) - CGINDLCAP(IR,IY-1,8)
               IF (CGINDLCAP(IR,IY,4) .GT. CGINDLCAP(IR,IY-1,4)) &
                  RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGINDLCAP(IR,IY,4) - CGINDLCAP(IR,IY-1,4)
               IF (CGCOMMCAP(IR,IY,4) .GT. CGCOMMCAP(IR,IY-1,4)) &
                  RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGCOMMCAP(IR,IY,4) - CGCOMMCAP(IR,IY-1,4)
               IF (CGCOMMCAP(IR,IY,5) .GT. CGCOMMCAP(IR,IY-1,5)) &
                  RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGCOMMCAP(IR,IY,5) - CGCOMMCAP(IR,IY-1,5)
               IF (CGCOMMCAP(IR,IY,11) .GT. CGCOMMCAP(IR,IY-1,11)) &
                  RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGCOMMCAP(IR,IY,11) - CGCOMMCAP(IR,IY-1,11)
               IF (CGINDLCAP(IR,IY,5) .GT. CGINDLCAP(IR,IY-1,5)) &
                  RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGINDLCAP(IR,IY,5) - CGINDLCAP(IR,IY-1,5)
               IF (CGINDLCAP(IR,IY,11) .GT. CGINDLCAP(IR,IY-1,11)) &
                  RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGINDLCAP(IR,IY,11) - CGINDLCAP(IR,IY-1,11)
! other
               IF (CGINDLCAP(IR,IY,9) .GT. CGINDLCAP(IR,IY-1,9)) &
                  RUNTOTS(6,IY) = RUNTOTS(6,IY) + CGINDLCAP(IR,IY,9) - CGINDLCAP(IR,IY-1,9)
               IF (CGREFCAP(IR,IY,9) .GT. CGREFCAP(IR,IY-1,9)) &
                  RUNTOTS(6,IY) = RUNTOTS(6,IY) + CGREFCAP(IR,IY,9) - CGREFCAP(IR,IY-1,9)
               IF (CGCOMMCAP(IR,IY,9) .GT. CGCOMMCAP(IR,IY-1,9)) &
                  RUNTOTS(6,IY) = RUNTOTS(6,IY) + CGCOMMCAP(IR,IY,9) - CGCOMMCAP(IR,IY-1,9)
               IF (CGOGSCAP(IR,IY,4) .GT. CGOGSCAP(IR,IY-1,4)) &
                  RUNTOTS(6,IY) = RUNTOTS(6,IY) + CGOGSCAP(IR,IY,4) - CGOGSCAP(IR,IY-1,4)
               IF (CGINDLCAP(IR,IY,10) .GT. CGINDLCAP(IR,IY-1,10)) &
                  RUNTOTS(6,IY) = RUNTOTS(6,IY) + CGINDLCAP(IR,IY,10) - CGINDLCAP(IR,IY-1,10)
               IF (CGCOMMCAP(IR,IY,10) .GT. CGCOMMCAP(IR,IY-1,10)) &
                  RUNTOTS(6,IY) = RUNTOTS(6,IY) + CGCOMMCAP(IR,IY,10) - CGCOMMCAP(IR,IY-1,10)
            ENDDO
            T9(65,IY,IS) = T9(65,IY,IS) + RUNTOTS(1,IY) / 1000.
            T9(66,IY,IS) = T9(66,IY,IS) + RUNTOTS(2,IY) / 1000.
            T9(67,IY,IS) = T9(67,IY,IS) + RUNTOTS(3,IY) / 1000.
            T9(69,IY,IS) = T9(69,IY,IS) + RUNTOTS(5,IY) / 1000.
            T9(70,IY,IS) = T9(70,IY,IS) + RUNTOTS(6,IY) / 1000.
            T9(45,IY,IS) = FSUM(T9(65,IY,IS),6)
!  capacity additions in electric power sector
            T9(65,IY,IS) = T9(65,IY,IS) + T9(10,IY,IS) + T9(19,IY,IS)
            T9(66,IY,IS) = T9(66,IY,IS) + T9(11,IY,IS) + T9(20,IY,IS)
            T9(67,IY,IS) = T9(67,IY,IS) + T9(12,IY,IS) + T9(13,IY,IS) + T9(16,IY,IS) + T9(49,IY,IS) + &
                           T9(21,IY,IS) + T9(22,IY,IS) + T9(25,IY,IS) + T9(50,IY,IS)
            T9(68,IY,IS) = T9(68,IY,IS) + T9(14,IY,IS) + T9(23,IY,IS)
            T9(69,IY,IS) = T9(69,IY,IS) + T9(17,IY,IS) + T9(26,IY,IS)
            T9(70,IY,IS) = T9(70,IY,IS) + T9(15,IY,IS) + T9(24,IY,IS) + T9(73,IY,IS) + T9(74,IY,IS)
! uncomment this else section for creating published files.
        ELSE
if (ftabbone.eq.0) then          !  only do this if tables are elevated to publish level (no bonus rows)
           T9(10:37,IY,IS) = -999.
           T9(49,IY,IS) = -999.
           T9(50,IY,IS) = -999.
           T9(45,IY,IS) = -999.
           T9(73:75,IY,IS) = -999.
endif
         END IF
         T9(71,IY,IS) = FSUM(T9(65,IY,IS),6)

   90 CONTINUE

! calculate nuclear uprates?
      uprates = T9(5,LASTYR,IS) - T9(5,CUMCAPADD-1989,IS) - &
               (T9(14,LASTYR,IS) + T9(23,LASTYR,IS) - T9(33,LASTYR,IS))
      write (CHUPRATES,'(F8.1)') uprates
      chuprates = adjustl(chuprates)
! update table 9 footnote with uprates
      do ir=1,MAXTAB
      do iy=1,NFOOT(ir)
        CALL REPLACE(FOOTNT(iy,9),'#up#',trim(chuprates))
      enddo
      enddo

! TABLE 10  Electric Transmission, Trade, and DSM

        DO 100 IY=1,LASTYR
!          --- DOMESTIC TRANSMISSION AND TRADE
           T10( 1,IY,IS) = UTEXMF(mnumnr,IY) * .001
           T10( 2,IY,IS) = UTEXME(mnumnr,IY) * .001
           T10( 3,IY,IS) = (UTEXMF(mnumnr,IY) + UTEXME(mnumnr,IY)) * .001
!          --- DOMESTIC POWER SALES
           T10( 4,IY,IS) = UTEXDF(mnumnr,IY)
           T10( 5,IY,IS) = UTEXDE(mnumnr,IY)
           T10( 6,IY,IS) = UTEXDF(mnumnr,IY) + UTEXDE(mnumnr,IY)
!          --- IMPORT TRANSMISSION AND TRADE
           T10( 7,IY,IS) = UTIMPF(mnumnr,IY) * .001
           T10( 8,IY,IS) = UTIMPE(mnumnr,IY) * .001
           T10( 9,IY,IS) = (UTIMPF(mnumnr,IY) + UTIMPE(mnumnr,IY)) * .001
!          --- EXPORT TRANSMISSION AND TRADE
           T10(10,IY,IS) = UTEXPF(mnumnr,IY) * .001
           T10(11,IY,IS) = UTEXPE(mnumnr,IY) * .001
           T10(12,IY,IS) = (UTEXPF(mnumnr,IY) + UTEXPE(mnumnr,IY)) * .001
  100   CONTINUE

!  TABLE 11

      DO 110 IY=1,LASTYR
!  --- Crude Oil
        T11( 5,IY,IS) = RFQTDCRD(MNUMOR+2,IY)        ! Total production
        T11( 1,IY,IS) = RFQTDCRD(MNUMOR+0,IY)        ! Alaska
        T11( 2,IY,IS) = RFQTDCRD(MNUMOR+1,IY)        ! Lower 48
        T11( 6,IY,IS) = RFIMCR(MNUMPR,IY)            ! Net imports
        T11( 3,IY,IS) = RFQICRD(MNUMPR,IY)           ! Imports
        T11( 4,IY,IS) = RFQEXCRD(MNUMPR,IY) / 1000.  ! Exports
        T11( 7,IY,IS) = RFCRDOTH(MNUMPR,IY)          ! Other crude supply
        T11( 8,IY,IS) = RFQTDCRD(MNUMOR+2,IY) + RFIMCR(MNUMPR,IY) + RFCRDOTH(MNUMPR,IY)   ! add it all up
!  --- Other Petroleum Supply
        T11( 9,IY,IS) = RFPQIPRDT(MNUMPR,IY,2)       ! Imports of refined product
        T11(10,IY,IS) = RFPQUFC(MNUMPR,IY,2)         ! Imports of unfinished oils
        T11(11,IY,IS) = RFMTBI(MNUMPR,IY) + &        ! Imports of blending components
 RFIPQCBOB(MNUMPR,IY,2) / 1000. + &                  ! replacing BLDIMP with these
 RFIPQRBOB(MNUMPR,IY,2) / 1000.
        T11(12,IY,IS) = RFQEXPRDT(MNUMPR,IY)         ! Product exports
        T11(13,IY,IS) = RFIMTP(MNUMPR,IY)            ! Net product imports
        T11(14,IY,IS) = RFQPRCG(MNUMPR,IY)           ! Refinery processing gain
        T11(15,IY,IS) = PRDSTKWDR(MNUMPR,IY)         ! Product stock withdrawal
        T11(16,IY,IS) = FSUM(T11(13,IY,IS),3)
! Other non-petroleum supply
  ! Renewable
    ! Ethanol
        T11(17,IY,IS) = 0.9751*(CRNETHCD(11,IY)+CLLETHCD(11,IY)+OTHETHCD(11,IY)+GRNETHCD(11,IY))/1000.  ! Domestic ethanol minus denaturant
        T11(18,IY,IS) =(ETHIMP(11,IY)-ETHEXP(11,IY))/1000.     ! Net imports
        T11(28,IY,IS) = FSUM(T11(17,IY,IS),2)
    ! Biodiesel
        T11(20,IY,IS) = sum(BIMQTYCD(1:4,11,IY))/1000.         ! Domestic
        T11(21,IY,IS) =(BIODIMP(11,IY)-BIODEXP(11,IY))/1000.   ! Net imports
        T11(29,IY,IS) = FSUM(T11(20,IY,IS),2)
    ! Renewable Diesel
        T11(137,IY,IS) = GRD2DSQTY(MNUMPR,IY)/1000.         ! Domestic
        T11(138,IY,IS) = RENEWDIMP(11,IY)/1000.            ! Net imports
        T11(139,IY,IS) = FSUM(T11(137,IY,IS),2)
    ! Other liquids from biomass (includes biobutanol)
        T11(23,IY,IS) = sum(BTLFRAC(1:4,MNUMPR,IY))/1000. + &     ! Biomass to liquids
                        sum(CBTLFRAC(2,1:4,MNUMPR,IY))/1000. + &  ! Biomass portion of CBTL
                        UBAVOL(MNUMPR,IY)/1000. + &               ! Pyrolysis Oils
                        SAF2JTQTY(MNUMPR,IY)/1000. + &            ! Renewable Jet Fuel (HEFA-SPK) 
                        GRN2MGQTY(MNUMPR,IY) / 1000. + &          ! Refinery produced biogasoline
                        RFBIOBUTECD(MNUMCR,IY)/1000.              ! Biobutanol
        T11(24,IY,IS) =(BIOBUTEIMP(IY)-BIOBUTEEXP(IY))/1000.       ! Net imports
        T11(30,IY,IS) = FSUM(T11(23,IY,IS),3)
        T11(132,IY,IS) = sum(BTLFRAC(1:4,MNUMPR,IY))/1000. + &    ! Biomass to liquids
                         sum(CBTLFRAC(2,1:4,MNUMPR,IY))/1000.     ! Biomass portion of CBTL
        T11(133,IY,IS) = UBAVOL(MNUMPR,IY)/1000.                  ! Pyrolysis Oils
        T11(134,IY,IS) = SAF2JTQTY(MNUMPR,IY) / 1000.             ! Renewable Jet Fuel (HEFA-SPK) 
        T11(135,IY,IS) = GRN2MGQTY(MNUMPR,IY) / 1000.             ! Refinery produced biogasoline
        T11(136,IY,IS) = RFBIOBUTECD(MNUMCR,IY)/1000.             ! Biobutanol

        T11(31,IY,IS) = FSUM(T11(28,IY,IS),3)+T11(139,IY,IS)      ! Total Biofuels
  ! Not as renewable
    ! from natural gas
        T11(26,IY,IS) = RFQNGPL(MNUMPR,IY,6)/1000.                  ! Natural gas plant liquids
        T11(27,IY,IS) = sum(GTLFRAC(1:4,MNUMPR,IY))/1000.           ! Gas to liquids
        T11(32,IY,IS) = FSUM(T11(26,IY,IS),2)
    ! from coal
        T11(33,IY,IS) = sum(CTLFRAC(1:4,MNUMPR,IY))/1000 + &        ! Coal to liquids
                        sum(CBTLFRAC(1,1:4,MNUMPR,IY))/1000         ! Coal portion of CBTL
    ! other
        T11(34,IY,IS) = RFHCXH2IN(MNUMPR,IY) + RFMETM85(MNUMPR,IY)  ! other oxygenates and methanol
        T11(35,IY,IS) = FSUM(T11(31,IY,IS),4)
! Total supply
        T11(36,IY,IS) = T11( 8,IY,IS) + T11(16,IY,IS) + T11(35,IY,IS)
! Consumption
  ! by fuel
        T11(37,IY,IS)= RFQLG(11,IY)
        T11(38,IY,IS)= QETTR(11,IY)/CFE85Q(IY)/365.*1000.
        T11(39,IY,IS)= RFQMG(11,IY)                         !  includes all of E85
        T11(40,IY,IS)= RFQJF(11,IY)
        T11(41,IY,IS)= RFQDS(11,IY)
          T11(49,IY,IS) = TDIESEL(11,IY) / 1000.
        T11(42,IY,IS)= RFQRL(11,IY) + RFQRH(11,IY)
        T11(43,IY,IS)= RFQOTH(11,IY) + RFQKS(11,IY) + RFQPCK(11,IY) + &
                       RFQPF(11,IY) + RFQARO(11,IY) + RFQSTG(11,IY) + RFMETM85(MNUMPR,IY)

  ! by sector
        T11(44,IY,IS) =(QMGCM(11,IY)/CFMGQ(IY) + QDSCM(11,IY)/CFDSCM(IY) + &
                        QKSCM(11,IY)/CFKSQ + QPRCM(11,IY)/CFPRQ + &
                        QRSCM(11,IY)/CFRSQ + QDSRS(11,IY)/CFDSRS(IY) + &
                        QKSRS(11,IY)/CFKSQ + QPRRS(11,IY)/CFPRQ)/365.*1000.
        T11(45,IY,IS) =(QMGIN(11,IY)/CFMGQ(IY) + QDSIN(11,IY)/CFDSIN(IY) + &
                        QKSIN(11,IY)/CFJFQ(IY) + QLGIN(11,IY)/CFLGQ(IY) + &
                        QRSIN(11,IY)/CFRSQ + QPFIN(11,IY)/CFPFQ(IY) + &
                        QSGIN(11,IY)/CFSGQ + QPCIN(11,IY)/CFPCQ + &
                        QASIN(11,IY)/CFASQ + QLUIN(11,IY)/CFLUQ + &
                       (QOTIN(11,IY)-QLUIN(11,IY))/CFOTQ(IY))/365.*1000.
if (iy .gt. 23) &
        T11(45,IY,IS) =(QMGIN(11,IY)/CFMGQ(IY) + QDSIN(11,IY)/CFDSIN(IY) + &
                        QKSIN(11,IY)/CFKSQ + &
!  we're going to use the pieces for QLGIN(11,IY)/CFLGQ(IY) + &
                        QETIN(11,IY)/CFEEQ +(QPROLENERF(11,IY) + QPRIN(11,IY))/CFPRQ + &
                        QBUIN(11,IY)/CFBUQ + QISIN(11,IY)/CFIBQ + &
                        QPPIN(11,IY)/CFPPQ + &
                        QRSIN(11,IY)/CFRSQ + QPFIN(11,IY)/CFPFQ(IY) + &
                        QSGIN(11,IY)/CFSGQ +(QPCIN(11,IY)-QCCRF(11,IY))/CFPCQ + &
                        QCCRF(11,IY)/CFCCQ(IY) + &
                        QASIN(11,IY)/CFASQ + QLUIN(11,IY)/CFLUQ + &
                       (QOTIN(11,IY)-QLUIN(11,IY))/CFOTQ(IY))/365.*1000.
        T11(46,IY,IS) =((QMGTR(11,IY)-QMGBS(11,IY))/CFMGQ(IY) + &    !  remove balance sector
                        (QDSTR(11,IY)-QDSBS(11,IY))/CFDSTR(IY) + &   !  remove balance sector
                        (QJFTR(11,IY)-QJFBS(11,IY))/CFJFQ(IY) + &    !  remove balance sector
                        QPRTR(11,IY)/CFPRQ + QRSTR(11,IY)/CFRSQ + &
                        QAGTR(11,IY)/CFAVQ + QLUTR(11,IY)/CFLUQ + &
                        QETTR(11,IY)/CFE85Q(IY) + QMETR(11,IY)/CFM85Q(IY))/365.*1000.
        T11(47,IY,IS) =(QDSEL(11,IY)/CFDSEL(IY) + QRSEL(11,IY)/CFRSQ + QPCEL(11,IY)/CFPCQ)/365.*1000.
        T11(128,IY,IS)=    &       !  balance sector
               (QMGBS(11,IY)/CFMGQ(IY) + QJFBS(11,IY)/CFJFQ(IY) + QDSBS(11,IY)/CFDSTR(IY))/365.*1000
        T11(48,IY,IS) = FSUM(T11(37,IY,IS),7) - T11(38,IY,IS)
! Discrepancy
        T11(50,IY,IS) = T11(36,IY,IS)-T11(48,IY,IS)

! Other assorted stuff
!==     T11(51,IY,IS) =(REF_CAP(1,MNUMPR,IY) + REF_CAP(53,MNUMPR,IY) ) / 1000.
!==     T11(52,IY,IS) =(REF_UTL(1,MNUMPR,IY) * REF_CAP(1,MNUMPR,IY) +       &
!==                     REF_UTL(53,MNUMPR,IY) * REF_CAP(53,MNUMPR,IY) ) /   &
!==                    (1000*T11(51,IY,IS)) * 100.

! includes ACU, splitter, and % of UFO runs (per STEO definition), 10/20/2014
        T11(51,IY,IS) =RFDSTCAP(MNUMPR,IY)
        T11(52,IY,IS) =RFDSTUTL(MNUMPR,IY)

        T11(53,IY,IS) =(RFIMCR(MNUMPR,IY) + RFIMTP(MNUMPR,IY) + &
                       (ETHIMP(11,IY)-ETHEXP(11,IY)+BIODIMP(11,IY)-BIODEXP(11,IY)+ RENEWDIMP(11,IY))/1000.) / &
                        T11(36,IY,IS) * 100.
        T11(54,IY,IS) = T11(52,IY,IS) * T11(51,IY,IS) / 100.     ! multiply capacity and utilization
        T11(55,IY,IS) = RFIMPEXPEND(IY)
! Bonus rows
        T11(56,IY,IS) = T11(55,IY,IS) / SCALPR * MC_JPGDP(IY)

        T11(129,IY,IS)= RFQICRD(MNUMPR,IY) + RFPQIPRDT(MNUMPR,IY,2) + RFPQUFC(MNUMPR,IY,2) + &
                        RFMTBI(MNUMPR,IY) + (RFIPQCBOB(MNUMPR,IY,2) + RFIPQRBOB(MNUMPR,IY,2)) / 1000. + &
                       (ETHIMP(11,IY) + BIODIMP(11,IY)+ RENEWDIMP(11,IY)+ BIOBUTEIMP(IY)) / 1000.
        T11(130,IY,IS)= RFQEXCRD(MNUMPR,IY) / 1000. + RFQEXPRDT(MNUMPR,IY) + &
                       (ETHEXP(11,IY) + BIODEXP(11,IY) + BIOBUTEEXP(IY)) / 1000.
        T11(131,IY,IS)= T11(129,IY,IS) - T11(130,IY,IS)
        T11(57,IY,IS) =(RFIMTP(MNUMPR,IY)+(ETHIMP(11,IY)-ETHEXP(11,IY)+BIODIMP(11,IY)-BIODEXP(11,IY)+ RENEWDIMP(11,IY))/1000.) / &
                       (RFIMCR(MNUMPR,IY)+RFIMTP(MNUMPR,IY)+(ETHIMP(11,IY)-ETHEXP(11,IY)+BIODIMP(11,IY)-BIODEXP(11,IY)+ RENEWDIMP(11,IY))/1000.) * 100.
        T11(58,IY,IS) = T11(14,IY,IS) / T11(54,IY,IS) * 100.

! petroleum only section
        T11(59,IY,IS) = T11( 3,IY,IS) + FSUM(T11( 9,IY,IS),3)    ! Imports
        T11(60,IY,IS) = T11( 4,IY,IS) + T11(12,IY,IS)            ! Exports
        T11(61,IY,IS) = T11(59,IY,IS) - T11(60,IY,IS)            ! Net imports
        T11(62,IY,IS) = T11(48,IY,IS) - T11(35,IY,IS)            ! for demand, take total demand - other nonpetroleum supply
    ! Biobutanol by itself
        T11(63,IY,IS) = RFBIOBUTECD(MNUMCR,IY)/1000.                ! Domestic
        T11(64,IY,IS) =(BIOBUTEIMP(IY)-BIOBUTEEXP(IY))/1000.        ! Net imports
        T11(65,IY,IS) = BIOBUTESTK(IY)/1000.                        ! Stock change
        T11(66,IY,IS) = QBIOBUTE(MNUMCR,IY)/1000.                   ! this represents total biobutanol supply
        T11(67,IY,IS) = GLBCRDDMD(IY)/1000.          ! non-US crude-like demand (convergence variable)

!  Hydrocarbon gas liquids
        T11(70,IY,IS) = RFQNGPL(MNUMPR,IY,1)/1000.            ! ethane (cc2)
        T11(71,IY,IS) = RFQNGPL(MNUMPR,IY,2)/1000.            ! propane (cc3)
        T11(72,IY,IS) = RFQNGPL(MNUMPR,IY,3)/1000.            ! normal butane (nc4)
        T11(73,IY,IS) = RFQNGPL(MNUMPR,IY,4)/1000.            ! isobutane (ic4)
        T11(74,IY,IS) = RFQNGPL(MNUMPR,IY,5)/1000.            ! pentanes plus (nat)
        T11(75,IY,IS) = 0.0      !  balancing item, calculate at bottom
        T11(76,IY,IS) = 0.0      !  balancing item, calculate at bottom
        T11(77,IY,IS) = 0.0      !  balancing item, calculate at bottom
        T11(78,IY,IS) = 0.0      !  balancing item, calculate at bottom
        T11(79,IY,IS) = 0.0      !  balancing item, calculate at bottom
        T11(80,IY,IS) = 0.0      !  balancing item, calculate at bottom
        T11(81,IY,IS) = 0.0          ! no mechanism yet for other olefins
        T11(82,IY,IS) = RFIPQET(MNUMPR,IY,2)/1000.            ! ethane (cc2)
        T11(83,IY,IS) = RFIPQPR(MNUMPR,IY,2)/1000.            ! propane (cc3)
        T11(84,IY,IS) = RFIPQBU(MNUMPR,IY,2)/1000.            ! normal butane (nc4)
        T11(85,IY,IS) = RFIPQIS(MNUMPR,IY,2)/1000.            ! isobutane (ic4)
        T11(86,IY,IS) = RFIPQPP(MNUMPR,IY,2)/1000.            ! pentanes plus (nat)
        T11(87,IY,IS) = RFIPQPY(MNUMPR,IY,2)/1000.            ! propylene (uc3)
        T11(88,IY,IS) = 0.0          ! no mechanism yet for other olefins
        T11(89,IY,IS) = QPRDEX(27,IY)                         ! ethane (cc2)
        T11(90,IY,IS) = QPRDEX( 1,IY)                         ! propane (cc3)
        T11(91,IY,IS) = QPRDEX(15,IY)                         ! normal butane (nc4)
        T11(92,IY,IS) = QPRDEX(28,IY)                         ! isobutane (ic4)
        T11(93,IY,IS) = QPRDEX(29,IY)                         ! pentanes plus (nat)
        T11(94,IY,IS) = QPRDEX(14,IY)                         ! propylene (uc3)
        T11(95,IY,IS) = 0.0          ! no mechanism yet for other olefins
        T11(96,IY,IS) = QETINPF(MNUMCR,IY)/CFEEQ/365.*1000.
        T11(97,IY,IS) = QPRINPF(MNUMCR,IY)/CFPRQ/365.*1000.
        T11(98,IY,IS) = QBUINPF(MNUMCR,IY)/CFBUQ/365.*1000.
        T11(99,IY,IS) = QISINPF(MNUMCR,IY)/CFIBQ/365.*1000.
        T11(100,IY,IS)= QPPINPF(MNUMCR,IY)/CFPPQ/365.*1000.
        T11(101,IY,IS)= QPROLENERF(MNUMCR,IY)/CFPRQ/365.*1000.
        T11(102,IY,IS)= 0.0     !  currently no mechanism for other olefins
        T11(103,IY,IS)= QPRTR(MNUMCR,IY)/CFPRQ/365.*1000.
        T11(104,IY,IS)= sum(MOTOR_FUEL(1:3,3,IY))/1000.
        T11(105,IY,IS)= QPRRS(MNUMCR,IY)/CFPRQ/365.*1000.
        T11(106,IY,IS)= QPRCM(MNUMCR,IY)/CFPRQ/365.*1000.
        T11(107,IY,IS)=(QPRIN(MNUMCR,IY)-QPRINPF(MNUMCR,IY))/CFPRQ/365.*1000.
        T11(75,IY,IS) = T11( 96,IY,IS) + T11(89,IY,IS) - T11(70,IY,IS) - T11(82,IY,IS)
        T11(76,IY,IS) = T11( 97,IY,IS) + T11(90,IY,IS) - T11(71,IY,IS) - T11(83,IY,IS) + &
                        T11(103,IY,IS) + sum(T11(105:107,IY,IS))
        T11(77,IY,IS) = T11( 98,IY,IS) + T11(91,IY,IS) - T11(72,IY,IS) - T11(84,IY,IS)
        T11(78,IY,IS) = T11( 99,IY,IS) + T11(92,IY,IS) - T11(73,IY,IS) - T11(85,IY,IS)
        T11(79,IY,IS) = T11(100,IY,IS) + T11(104,IY,IS) + T11(93,IY,IS) - &
                        T11(74,IY,IS) - T11(86,IY,IS)
        T11(80,IY,IS) = T11(101,IY,IS) + T11(94,IY,IS)                 - T11(87,IY,IS)
        T11(108,IY,IS)= FSUM(T11(103,IY,IS),2)
        T11(109,IY,IS)= FSUM(T11(75,IY,IS),5)
        T11(110,IY,IS)= FSUM(T11(80,IY,IS),2)
        T11(111,IY,IS)= FSUM(T11(82,IY,IS),5)
        T11(112,IY,IS)= FSUM(T11(87,IY,IS),2)
        T11(113,IY,IS)= FSUM(T11(89,IY,IS),5)
        T11(114,IY,IS)= FSUM(T11(94,IY,IS),2)
        T11(115,IY,IS)= FSUM(T11(96,IY,IS),5)
        T11(116,IY,IS)= FSUM(T11(101,IY,IS),2)
        T11(117,IY,IS)= FSUM(T11(70,IY,IS),5)
        T11(118,IY,IS)= FSUM(T11(109,IY,IS),2)
        T11(119,IY,IS)= FSUM(T11(111,IY,IS),2)
        T11(120,IY,IS)= FSUM(T11(113,IY,IS),2)
        T11(121,IY,IS)= FSUM(T11(115,IY,IS),2)
        T11(122,IY,IS)= FSUM(T11(105,IY,IS),3)
        T11(123,IY,IS)= FSUM(T11(117,IY,IS),2)
        T11(124,IY,IS)= T11(119,IY,IS) - T11(120,IY,IS)
        T11(125,IY,IS)= FSUM(T11(123,IY,IS),2)
        T11(126,IY,IS)= FSUM(T11(121,IY,IS),2) + T11(108,IY,IS)
        T11(127,IY,IS)= T11(125,IY,IS) - T11(126,IY,IS)

 110  CONTINUE

!         Table 12. Petroleum Product Prices

! Dividing by 42 converts from dollars per barrel to dollars per gallon
      DO 120 IY=1,LASTYR
!  --- WORLD OIL PRICE
        T12(29,IY,IS) = WTI_PRICE(IY)
        T12( 1,IY,IS) = BRENT_PRICE(IY)
        T12(55,IY,IS) = IT_WOP(IY,1)
        T12(57,IY,IS) = BRENT_PRICE(IY) - WTI_PRICE(IY)
!  --- RESIDENTIAL
        T12( 2,IY,IS) = PDSRS(11,IY) * CFDSRS(IY) / 42.
        T12( 3,IY,IS) = PPRRS(11,IY) * CFPRQ / 42.
!  --- COMMERCIAL
        T12( 4,IY,IS) = PDSCM(11,IY) * CFDSCM(IY) / 42.
        T12( 5,IY,IS) = PRSCM(11,IY) * CFRSQ / 42.
        T12( 6,IY,IS) = PRSCM(11,IY) * CFRSQ
!  --- INDUSTRIAL
        T12( 7,IY,IS) = PDSIN(11,IY) * CFDSIN(IY) / 42.
! do this if we want to average in the feedstock price:
          IF(QLGIN(11,IY) .NE. 0.0) &
          T12( 8,IY,IS)=(PLGIN(11,IY)*(QLGIN(11,IY)-INQLGPF(11,IY)/1000.) + &
                         PLGINPF(11,IY)*INQLGPF(11,IY)/1000.) / QLGIN(11,IY) * CFLGQ(IY) / 42.
! otherwise, do this:
        T12( 8,IY,IS) = PLGIN(11,IY) * CFLGQ(IY) / 42.
! but it is simpler (the row in now labelled "propane") if we just use the propane price variable:
        T12( 8,IY,IS) = PPRIN(11,IY) * CFPRQ / 42.
        T12(58,IY,IS) = PPFIN(11,IY) * CFPFQ(IY) / 42.
        T12( 9,IY,IS) = PRSIN(11,IY) * CFRSQ / 42.
        T12(10,IY,IS) = PRSIN(11,IY) * CFRSQ
!  --- TRANSPORTATION
        T12(11,IY,IS) = PDSTRHWY(11,IY) * CFDSTRHWY(IY) / 42.
        T12(12,IY,IS) = PJFTR(11,IY) * CFJFK / 42.
        T12(13,IY,IS) = PMGTR(11,IY) * CFMGQ(IY) / 42.
        T12(14,IY,IS) = PPRTR(11,IY) * CFPRQ / 42.
        T12(15,IY,IS) = PRSTR(11,IY) * CFRSQ / 42.
        T12(16,IY,IS) = PRSTR(11,IY) * CFRSQ
        T12(17,IY,IS) = PETTR(11,IY) * CFE85Q(IY) / 42.
        T12(30,IY,IS) = PETHM(11,IY) / 42.
        T12(18,IY,IS) = PMETR(11,IY) * CFM85Q(IY) / 42.
!  --- ELECTRIC POWER
        T12(19,IY,IS) = PDSEL(11,IY) * CFDSEL(IY) / 42.
        T12(20,IY,IS) = PRSEL(11,IY) * CFRSQ / 42.
        T12(21,IY,IS) = PRSEL(11,IY) * CFRSQ
!  --- ALL SECTOR AVERAGE
        T12(22,IY,IS) = PDSAS(11,IY) * CFDSQT(IY) / 42.
        T12(23,IY,IS) = PJFTR(11,IY) * CFJFK / 42.
        T12(24,IY,IS) = PLGAS(11,IY) * CFLGQ(IY) / 42.
! calculated based on the sectoral prices shown (as stated in the footnote):
        IF (QPRRS(11,IY)+QPRCM(11,IY)+QPRIN(11,IY)-QPRINPF(11,IY)+QPRTR(11,IY) .NE. 0.0) &
          T12(24,IY,IS)= &
            ((QPRRS(11,IY)*PPRRS(11,IY)+QPRCM(11,IY)*PPRCM(11,IY)+ &
             (QPRIN(11,IY)-QPRINPF(11,IY))*PPRIN(11,IY)+QPRTR(11,IY)*PPRTR(11,IY)) / &
             (QPRRS(11,IY)+QPRCM(11,IY)+QPRIN(11,IY)-QPRINPF(11,IY)+QPRTR(11,IY))) * CFPRQ / 42.
        T12(25,IY,IS) = PMGAS(11,IY) * CFMGQ(IY) / 42.
        T12(26,IY,IS) = PRSAS(11,IY) * CFRSQ / 42.
        T12(27,IY,IS) = PRSAS(11,IY) * CFRSQ
        T12(28,IY,IS) = QMGAS(11,IY)/CFMGQ(IY)/365+ &
                0.15*QMETR(11,IY)/CFM85Q(IY)/365+ &
                TRGNE85*QETTR(11,IY)/CFE85Q(IY)/365+ &
                     QJFTR(11,IY)/CFJFK/365+QDSAS(11,IY)/CFDSQT(IY)/365 + &
                     QKSAS(11,IY)/CFKSQ/365+QRLAS(11,IY)/CFRSQ/365+ &
                     QRHAS(11,IY)/CFRSQ/365+QLGAS(11,IY)/CFLGQ(IY)/365+ &
                     QPFIN(11,IY)/CFPFQ(IY)/365
        IF (T12(28,IY,IS) .NE. 0.0) T12(28,IY,IS) = &
               (PDSAS(11,IY)*CFDSQT(IY)* QDSAS(11,IY)/CFDSQT(IY)/365 + &
                PKSAS(11,IY)*CFKSQ     * QKSAS(11,IY)/CFKSQ/365 + &
                PMGAS(11,IY)*CFMGQ(IY) *(QMGAS(11,IY)/CFMGQ(IY)/365+ &
              0.15*QMETR(11,IY)/CFM85Q(IY)/365+ &
              TRGNE85*QETTR(11,IY)/CFE85Q(IY)/365)+ &
                PJFTR(11,IY) * CFJFK  * QJFTR(11,IY)/CFJFK/365 + &
                PRSAS(11,IY) * CFRSQ  *(QRLAS(11,IY)/CFRSQ/365 + QRHAS(11,IY)/CFRSQ/365) + &
                PLGAS(11,IY) * CFLGQ(IY) *QLGAS(11,IY)/CFLGQ(IY)/365+ &
                PPFIN(11,IY) * CFPFQ(IY) *QPFIN(11,IY)/CFPFQ(IY)/365) / T12(28,IY,IS) / 42.
! nominal prices
        T12(31,IY,IS) = T12(29,IY,IS) / SCALPR * MC_JPGDP(IY)
        T12(32,IY,IS) = T12( 1,IY,IS) / SCALPR * MC_JPGDP(IY)
        T12(56,IY,IS) = T12(55,IY,IS) / SCALPR * MC_JPGDP(IY)
        T12(33,IY,IS) = T12( 3,IY,IS) / SCALPR * MC_JPGDP(IY)
        T12(34,IY,IS) = T12( 2,IY,IS) / SCALPR * MC_JPGDP(IY)
        T12(35,IY,IS) = T12( 4,IY,IS) / SCALPR * MC_JPGDP(IY)
        T12(36,IY,IS) = T12( 5,IY,IS) / SCALPR * MC_JPGDP(IY)
        T12(37,IY,IS) = T12( 8,IY,IS) / SCALPR * MC_JPGDP(IY)
        T12(38,IY,IS) = T12( 7,IY,IS) / SCALPR * MC_JPGDP(IY)
        T12(39,IY,IS) = T12( 9,IY,IS) / SCALPR * MC_JPGDP(IY)
        T12(40,IY,IS) = T12(14,IY,IS) / SCALPR * MC_JPGDP(IY)
        T12(41,IY,IS) = T12(17,IY,IS) / SCALPR * MC_JPGDP(IY)
        T12(42,IY,IS) = T12(30,IY,IS) / SCALPR * MC_JPGDP(IY)
        T12(43,IY,IS) = T12(13,IY,IS) / SCALPR * MC_JPGDP(IY)
        T12(44,IY,IS) = T12(12,IY,IS) / SCALPR * MC_JPGDP(IY)
        T12(45,IY,IS) = T12(11,IY,IS) / SCALPR * MC_JPGDP(IY)
        T12(46,IY,IS) = T12(15,IY,IS) / SCALPR * MC_JPGDP(IY)
        T12(47,IY,IS) = T12(19,IY,IS) / SCALPR * MC_JPGDP(IY)
        T12(48,IY,IS) = T12(20,IY,IS) / SCALPR * MC_JPGDP(IY)
        T12(49,IY,IS) = T12(24,IY,IS) / SCALPR * MC_JPGDP(IY)
        T12(50,IY,IS) = T12(25,IY,IS) / SCALPR * MC_JPGDP(IY)
        T12(51,IY,IS) = T12(23,IY,IS) / SCALPR * MC_JPGDP(IY)
        T12(52,IY,IS) = T12(22,IY,IS) / SCALPR * MC_JPGDP(IY)
        T12(53,IY,IS) = T12(27,IY,IS) / SCALPR * MC_JPGDP(IY)
        T12(54,IY,IS) = T12(28,IY,IS) / SCALPR * MC_JPGDP(IY)
 120  CONTINUE

! Table 13.  Natural Gas Supply, Disposition, and Prices

      DO 130,IY=1,LASTYR
! PRODUCTION
        T13( 1,IY,IS) =(OGQNGREP(1,IY) + OGQNGREP(2,IY) + &
                        OGQNGREP(3,IY) + OGQNGREP(4,IY) + &
                        OGQNGREP(5,IY) + OGQNGREP(6,IY) + &
                        OGQNGREP(7,IY) + OGQNGREP(8,IY) + OGSHALENG(IY)) * .001
        T13( 2,IY,IS) = OGPRSUP(IY) * .001
!     --- NET IMPORTS
        T13( 3,IY,IS) =(NGIMPVOL(4,IY)-NGEXPVOL(4,IY)) * .001
        T13( 4,IY,IS) =(NGIMPVOL(1,IY)-NGEXPVOL(1,IY)) * .001
        T13( 5,IY,IS) =(NGIMPVOL(2,IY)-NGEXPVOL(2,IY)) * .001
        T13(33,IY,IS) =(NGIMPVOL(1,IY)-NGEXPVOL(1,IY)) * .001 + &
                       (NGIMPVOL(2,IY)-NGEXPVOL(2,IY)) * .001
        T13( 6,IY,IS) =(NGIMPVOL(3,IY)-NGEXPVOL(3,IY)) * .001
!     --- TOTAL SUPPLY
        T13( 7,IY,IS) = FSUM(T13(1,IY,IS),3)
!     --- CONSUMPTION BY SECTOR - Convert from quads
        IF (CFNGN(IY) .NE. 0.0) THEN
           T13( 8,IY,IS)=QNGRS(11,IY)/CFNGN(IY)
           T13( 9,IY,IS)=QNGCM(11,IY)/CFNGN(IY)
           T13(10,IY,IS)=(QNGIN(11,IY)-QGTLRF(11,IY))/CFNGN(IY)
           T13(14,IY,IS)=QNGTR(11,IY)/CFNGN(IY)
        ENDIF
        IF (CFNGU(IY) .NE. 0.0) THEN
           T13(11,IY,IS)=QNGEL(11,IY)/CFNGU(IY)
        ENDIF
        IF (CFNGC(IY) .NE. 0.0) THEN
           T13(12,IY,IS)=QLPIN(11,IY)/CFNGC(IY)
           T13(56,IY,IS)=QNGLQ(11,IY)/CFNGC(IY)
           T13(13,IY,IS)=QGPTR(11,IY)/CFNGC(IY)
        ENDIF
        T13(35,IY,IS) = T13(10,IY,IS) + T13(12,IY,IS) + T13(17,IY,IS) + T13(37,IY,IS)
        T13(36,IY,IS) = T13(13,IY,IS) + T13(14,IY,IS) + T13(56,IY,IS)
!     --- Gas-to-Liquids
        T13(37,IY,IS)=sum(GTLFRAC(1:4,MNUMPR,IY))*365./1000000.*CFGTLLIQ(IY)/CFNGN(IY)
        T13(17,IY,IS)=QGTLRF(11,IY)/CFNGN(IY)
        T13(15,IY,IS)=FSUM(T13(8,IY,IS),7) + T13(17,IY,IS) + T13(37,IY,IS) + T13(56,IY,IS)
!     --- DISCREPANCY
        T13(16,IY,IS)=T13(7,IY,IS) - T13(15,IY,IS)
! Source prices - wellhead and import
        T13(34,IY,IS) = OGHHPRNG(IY)
        T13(18,IY,IS) = OGWPRNG(MNUMOR,IY)
        T13(19,IY,IS) = NGIMPPRC(4,IY)
        IF ((NGIMPVOL(4,IY) + (OGQNGREP(1,IY) + OGQNGREP(2,IY) + &
                               OGQNGREP(3,IY) + OGQNGREP(4,IY) + &
                               OGQNGREP(5,IY) + OGQNGREP(6,IY) + &
                               OGQNGREP(7,IY) + OGQNGREP(8,IY) + OGSHALENG(IY))) .NE. 0.0) &
          T13(20,IY,IS) = (NGIMPPRC(4,IY) * NGIMPVOL(4,IY) + OGWPRNG(MNUMOR,IY) * &
                          (OGQNGREP(1,IY) + OGQNGREP(2,IY) + &
                           OGQNGREP(3,IY) + OGQNGREP(4,IY) + &
                           OGQNGREP(5,IY) + OGQNGREP(6,IY) + &
                           OGQNGREP(7,IY) + OGQNGREP(8,IY) + OGSHALENG(IY))) / &
              (NGIMPVOL(4,IY) + (OGQNGREP(1,IY) + OGQNGREP(2,IY) + &
                                 OGQNGREP(3,IY) + OGQNGREP(4,IY) + &
                                 OGQNGREP(5,IY) + OGQNGREP(6,IY) + &
                                 OGQNGREP(7,IY) + OGQNGREP(8,IY) + OGSHALENG(IY)))
! Delivered prices
        T13(21,IY,IS) = PNGRS(11,IY)*CFNGN(IY)
        T13(22,IY,IS) = PNGCM(11,IY)*CFNGN(IY)
        T13(23,IY,IS) = PNGIN(11,IY)*CFNGN(IY)
        T13(24,IY,IS) = PNGEL(11,IY)*CFNGU(IY)
        T13(25,IY,IS) = PNGTR(11,IY)*CFNGN(IY)
        T13(26,IY,IS) = PNGAS(11,IY)*CFNGC(IY)
! Transmission and distribution margins by sector
        T13(27,IY,IS) = T13(21,IY,IS) - T13(20,IY,IS)
        T13(28,IY,IS) = T13(22,IY,IS) - T13(20,IY,IS)
        T13(29,IY,IS) = T13(23,IY,IS) - T13(20,IY,IS)
        T13(30,IY,IS) = T13(24,IY,IS) - T13(20,IY,IS)
        T13(31,IY,IS) = T13(25,IY,IS) - T13(20,IY,IS)
        T13(32,IY,IS) = T13(26,IY,IS) - T13(20,IY,IS)
! nominal prices
        T13(39,IY,IS) = T13(34,IY,IS) / SCALPR * MC_JPGDP(IY)
        T13(40,IY,IS) = T13(35,IY,IS) / SCALPR * MC_JPGDP(IY)
        T13(41,IY,IS) = T13(18,IY,IS) / SCALPR * MC_JPGDP(IY)
        T13(42,IY,IS) = T13(21,IY,IS) / SCALPR * MC_JPGDP(IY)
        T13(43,IY,IS) = T13(22,IY,IS) / SCALPR * MC_JPGDP(IY)
        T13(44,IY,IS) = T13(23,IY,IS) / SCALPR * MC_JPGDP(IY)
        T13(45,IY,IS) = T13(24,IY,IS) / SCALPR * MC_JPGDP(IY)
        T13(46,IY,IS) = T13(25,IY,IS) / SCALPR * MC_JPGDP(IY)
        T13(47,IY,IS) = T13(26,IY,IS) / SCALPR * MC_JPGDP(IY)
        T13(48,IY,IS) = T13(19,IY,IS) / SCALPR * MC_JPGDP(IY)
        T13(49,IY,IS) = T13(20,IY,IS) / SCALPR * MC_JPGDP(IY)
! Transmission and distribution margins by sector
        T13(27,IY,IS) = T13(21,IY,IS) - T13(20,IY,IS)
        T13(28,IY,IS) = T13(22,IY,IS) - T13(20,IY,IS)
        T13(29,IY,IS) = T13(23,IY,IS) - T13(20,IY,IS)
        T13(30,IY,IS) = T13(24,IY,IS) - T13(20,IY,IS)
        T13(31,IY,IS) = T13(25,IY,IS) - T13(20,IY,IS)
        T13(32,IY,IS) = T13(26,IY,IS) - T13(20,IY,IS)
! Transmission and distribution margins by sector
        T13(50,IY,IS) = T13(42,IY,IS) - T13(49,IY,IS)
        T13(51,IY,IS) = T13(43,IY,IS) - T13(49,IY,IS)
        T13(52,IY,IS) = T13(44,IY,IS) - T13(49,IY,IS)
        T13(53,IY,IS) = T13(45,IY,IS) - T13(49,IY,IS)
        T13(54,IY,IS) = T13(46,IY,IS) - T13(49,IY,IS)
        T13(55,IY,IS) = T13(47,IY,IS) - T13(49,IY,IS)
 130  CONTINUE

!     Table 14. Oil and Gas Supply

      DO 140, IY = 1,LASTYR
        RDAYS = 365.
        IF (IY.EQ.3.OR.IY.EQ.7.OR.IY.EQ.11.OR.IY.EQ.15) RDAYS = 366.  ! LEAP YEAR 4 HISTORIC DATA MATCH
!          --- CRUDE OIL
        T14( 1,IY,IS) = OGPCRWHP(IY)
! nominal price
        T14(36,IY,IS) = T14( 1,IY,IS) / SCALPR * MC_JPGDP(IY)
        T14(37,IY,IS) = OS_WOP(IY)
!          --- PRODUCTION
        T14( 2,IY,IS) =(OGQCRREP(1,IY) + OGQCRREP(2,IY) + &
                        OGQCRREP(3,IY) + OGQCRREP(4,IY)) / RDAYS
        T14( 3,IY,IS) =(OGQCRREP(1,IY) + OGQCRREP(2,IY)) / RDAYS
        T14(44,IY,IS) = sum(OGQSHLOIL(:,IY))
        T14(45,IY,IS) = sum(OGEORPRD(8,1:13,IY)) / 1000. / RDAYS
        T14(46,IY,IS) = T14( 3,IY,IS) - T14(44,IY,IS) - T14(45,IY,IS)
        T14( 4,IY,IS) = OGQCRREP(2,IY) /RDAYS
        T14( 5,IY,IS) = OGQCRREP(1,IY) /RDAYS
        T14( 6,IY,IS) = OGQCRREP(3,IY) /RDAYS
        T14( 7,IY,IS) = OGQCRREP(4,IY) /RDAYS
! state and federal
        T14(55,IY,IS) =(OGPRDOFF(1,1,IY) + OGPRDOFF(2,1,IY) + OGPRDOFF(3,1,IY))/RDAYS
        T14(56,IY,IS) = T14( 6,IY,IS) - T14(55,IY,IS)
        T14(58,IY,IS) = OGPRDOFF(5,1,IY)/RDAYS
        T14(59,IY,IS) = OGPRDOFF(4,1,IY)/RDAYS
        T14(57,IY,IS) = T14( 7,IY,IS) - T14(58,IY,IS) - T14(59,IY,IS)
!  End of Year Reserves
        T14( 8,IY,IS) = (OGQCRRSV(IY) * .001)
!          --- NATURAL GAS
        T14(31,IY,IS) = OGHHPRNG(IY)
        T14(32,IY,IS) = OGWPRNG(MNUMOR,IY) / CFNGC(IY)
        T14( 9,IY,IS) = OGWPRNG(MNUMOR,IY)
!          --- PRODUCTION
        T14(10,IY,IS) =(OGQNGREP(1,IY) + OGQNGREP(2,IY) + &
                        OGQNGREP(3,IY) + OGQNGREP(4,IY) + &
                        OGQNGREP(5,IY) + OGQNGREP(6,IY) + &
                        OGQNGREP(7,IY) + OGQNGREP(8,IY) + OGSHALENG(IY)) * .001
        T14(11,IY,IS) =(OGQNGREP(1,IY) + OGQNGREP(2,IY) + &
                        OGQNGREP(3,IY) + OGQNGREP(4,IY) + &
                        OGQNGREP(5,IY) + OGSHALENG(IY)) * .001
!       T14(12,IY,IS) =(OGQNGREP(5,IY) + OGSHALENG(IY)) * .001   !  note:  redefined below
        T14(13,IY,IS) =(OGQNGREP(1,IY) + OGQNGREP(2,IY) + &
                        OGQNGREP(3,IY) + OGQNGREP(4,IY)) * .001
!          T14(14,IY,IS) = OGQNGREP(4,IY) / 1000.
! define "other" as including associated-dissolved
        T14(14,IY,IS) =(OGQNGREP(4,IY) + OGQNGREP(5,IY) + OGSHALENG(IY)) * .001
        T14(15,IY,IS) = OGQNGREP(3,IY) / 1000.
        T14(26,IY,IS) = OGQNGREP(1,IY) / 1000.
        T14(27,IY,IS) = OGQNGREP(2,IY) / 1000.
        T14(16,IY,IS) =(OGQNGREP(6,IY) + OGQNGREP(7,IY)) * .001
!       T14(17,IY,IS) = OGQNGREP(7,IY) * .001   !  note:  redefined below
        T14(18,IY,IS) = OGQNGREP(6,IY) * .001
        T14(19,IY,IS) = OGQNGREP(8,IY) * .001
! state and federal
        T14(60,IY,IS) =(OGPRDOFF(1,2,IY) + OGPRDOFF(2,2,IY) + OGPRDOFF(3,2,IY))/1000.
        T14(61,IY,IS) = T14(16,IY,IS) - T14(60,IY,IS)
        T14(63,IY,IS) = OGPRDOFF(5,2,IY) / 1000.
        T14(64,IY,IS) = OGPRDOFF(4,2,IY) / 1000.
        T14(62,IY,IS) = T14(19,IY,IS) - T14(63,IY,IS) - T14(64,IY,IS)
!  End of Year Reserves
        T14(20,IY,IS) = OGQNGRSV(IY) * .001
!          --- OTHER NATURAL GAS SUPPLIES
        T14(21,IY,IS) = OGQNGREP(10,IY) + OGQNGREP(11,IY) + OGQNGREP(12,IY)
!          --- SUCCESSFUL WELLS COMPLETED
        T14(22,IY,IS) = OGNOWELL(IY) * .001
! Cumulative Production - Crude Oil, then Natural Gas, then Nonassociated
        IF(IY .LE. (CUMOILGAS - 1989))THEN
           T14(23,IY,IS) = 0.0
           T14(24,IY,IS) = 0.0
           T14(25,IY,IS) = 0.0
           T14(42,IY,IS) = 0.0
           T14(43,IY,IS) = 0.0
        ELSE
           T14(23,IY,IS) = T14(23,IY-1,IS) + T14( 2,IY,IS) * 365. / 1000.
           T14(24,IY,IS) = T14(24,IY-1,IS) + T14(11,IY,IS) + T14(16,IY,IS)
           T14(25,IY,IS) = T14(25,IY-1,IS) + T14(13,IY,IS) + T14(18,IY,IS)
           T14(42,IY,IS) = T14(42,IY-1,IS) + T14(26,IY,IS) + T14(27,IY,IS)
           T14(43,IY,IS) = T14(43,IY-1,IS) + T14(26,IY,IS) + T14(27,IY,IS) + T14(15,IY,IS)
        END IF
! Special bonus oil production rows
        T14(29,IY,IS) = OGQCRREP(1,IY) / RDAYS
        T14(30,IY,IS) = OGQCRREP(5,IY) / RDAYS
        T14(33,IY,IS) = sum(OGEORPRD(8,1:13,IY)) / 1000. / RDAYS
        T14(35,IY,IS) = sum(RFQTDCRD(1:MNL48N,IY)) - sum(RFQDCRD(1:MNL48N,IY)) - OGQCRREP(1,IY) / RDAYS
        T14(34,IY,IS) = T14(35,IY,IS) - T14(33,IY,IS)
!  subtract Alaska from expected and realized gas production:
        T14(38,IY,IS) =(sum(OGRNAGPRD(1:OGDIST,GASTYPES,IY)) - OGRNAGPRD(3,GASTYPES,IY) - &
                        OGRNAGPRD(75,GASTYPES,IY) - OGRNAGPRD(84,GASTYPES,IY))/1000.
        T14(39,IY,IS) =(sum(OGENAGPRD(1:OGDIST,GASTYPES,IY)) - OGENAGPRD(3,GASTYPES,IY) - &
                        OGENAGPRD(75,GASTYPES,IY) - OGENAGPRD(84,GASTYPES,IY))/1000.
        T14(40,IY,IS) =(OGQNGREP(1,IY) + OGQNGREP(2,IY))/ 1000.
        T14(41,IY,IS) =(OGQNGREP(3,IY) + OGQNGREP(1,IY) + OGQNGREP(2,IY))/ 1000.
! associated-dissolved, 12 and 17 subtract Alaska
        T14(12,IY,IS) = (sum(OGADGPRD( 1:66,OILTYPES,IY)) - OGADGPRD( 3,OILTYPES,IY)) / 1000.
        T14(17,IY,IS) = (sum(OGADGPRD(67:OGDIST,OILTYPES,IY)) - OGADGPRD(75,OILTYPES,IY) - OGADGPRD(84,OILTYPES,IY)) / 1000.
        T14(28,IY,IS) = T14(12,IY,IS) + T14(17,IY,IS)
        T14(65,IY,IS) =(OGADGPRD( 8,OILTYPES,IY) + OGADGPRD( 9,OILTYPES,IY) + OGADGPRD(10,OILTYPES,IY) + OGADGPRD(12,OILTYPES,IY) + &
                        OGADGPRD(15,OILTYPES,IY) + OGADGPRD(16,OILTYPES,IY) + OGADGPRD(19,OILTYPES,IY) + OGADGPRD(22,OILTYPES,IY) + &
                        OGADGPRD(23,OILTYPES,IY) + OGADGPRD(24,OILTYPES,IY) + OGADGPRD(25,OILTYPES,IY) + OGADGPRD(33,OILTYPES,IY) + &
                        OGADGPRD(34,OILTYPES,IY) + OGADGPRD(37,OILTYPES,IY) + OGADGPRD(38,OILTYPES,IY) + OGADGPRD(40,OILTYPES,IY) + &
                        OGADGPRD(43,OILTYPES,IY) + OGADGPRD(44,OILTYPES,IY) + OGADGPRD(45,OILTYPES,IY) + OGADGPRD(47,OILTYPES,IY) + &
                        OGADGPRD(61,OILTYPES,IY) + OGADGPRD(62,OILTYPES,IY) + OGADGPRD(64,OILTYPES,IY) + OGADGPRD(65,OILTYPES,IY))/1000.
        T14(66,IY,IS) =(OGADGPRD( 1,OILTYPES,IY) + OGADGPRD( 2,OILTYPES,IY) + OGADGPRD(11,OILTYPES,IY) + OGADGPRD(20,OILTYPES,IY) + &
                        OGADGPRD(21,OILTYPES,IY) + OGADGPRD(27,OILTYPES,IY) + OGADGPRD(28,OILTYPES,IY) + OGADGPRD(48,OILTYPES,IY) + &
                        OGADGPRD(49,OILTYPES,IY) + OGADGPRD(50,OILTYPES,IY) + OGADGPRD(51,OILTYPES,IY) + OGADGPRD(53,OILTYPES,IY))/1000.
        T14(67,IY,IS) =(OGADGPRD( 5,OILTYPES,IY) + OGADGPRD(17,OILTYPES,IY) + OGADGPRD(18,OILTYPES,IY) + OGADGPRD(26,OILTYPES,IY) + &
                        OGADGPRD(29,OILTYPES,IY) + OGADGPRD(31,OILTYPES,IY) + OGADGPRD(41,OILTYPES,IY) + OGADGPRD(59,OILTYPES,IY))/1000.
        T14(68,IY,IS) =(OGADGPRD(35,OILTYPES,IY) + OGADGPRD(52,OILTYPES,IY) + OGADGPRD(54,OILTYPES,IY) + OGADGPRD(55,OILTYPES,IY) + &
                        OGADGPRD(56,OILTYPES,IY) + OGADGPRD(57,OILTYPES,IY) + OGADGPRD(58,OILTYPES,IY))/1000.
        T14(69,IY,IS) =(OGADGPRD( 4,OILTYPES,IY) + OGADGPRD( 7,OILTYPES,IY) + OGADGPRD(14,OILTYPES,IY) + OGADGPRD(32,OILTYPES,IY) + &
                        OGADGPRD(36,OILTYPES,IY) + OGADGPRD(60,OILTYPES,IY) + OGADGPRD(66,OILTYPES,IY))/1000.
        T14(70,IY,IS) =(OGADGPRD(30,OILTYPES,IY) + OGADGPRD(39,OILTYPES,IY) + OGADGPRD(46,OILTYPES,IY))/1000.
        T14(71,IY,IS) =(OGADGPRD( 6,OILTYPES,IY) + OGADGPRD(42,OILTYPES,IY) + OGADGPRD(63,OILTYPES,IY))/1000.
        T14(72,IY,IS) =(OGADGPRD(67,OILTYPES,IY) + OGADGPRD(68,OILTYPES,IY) + OGADGPRD(69,OILTYPES,IY) + OGADGPRD(76,OILTYPES,IY) + &
                        OGADGPRD(77,OILTYPES,IY) + OGADGPRD(78,OILTYPES,IY))/1000.
        T14(73,IY,IS) =(OGADGPRD(73,OILTYPES,IY) + OGADGPRD(74,OILTYPES,IY) + OGADGPRD(82,OILTYPES,IY) + OGADGPRD(83,OILTYPES,IY))/1000.
        T14(74,IY,IS) =(OGADGPRD(70,OILTYPES,IY) + OGADGPRD(71,OILTYPES,IY) + OGADGPRD(72,OILTYPES,IY) + OGADGPRD(79,OILTYPES,IY) + &
                        OGADGPRD(80,OILTYPES,IY) + OGADGPRD(81,OILTYPES,IY))/1000.
        T14(75,IY,IS) =(OGADGPRD( 3,OILTYPES,IY) + OGADGPRD(75,OILTYPES,IY) + OGADGPRD(84,OILTYPES,IY))/1000.

! Natural gas plant liquids
        T14(48,IY,IS) = sum(OGNGPLPRD(4:66,IY)) + OGNGPLPRD(1,IY) + OGNGPLPRD(2,IY)
        T14(49,IY,IS) = sum(OGNGPLPRD(67:74,IY)) + sum(OGNGPLPRD(76:83,IY))
        T14(50,IY,IS) = OGNGPLPRD(3,IY) + OGNGPLPRD(75,IY) + OGNGPLPRD(84,IY)
        T14(47,IY,IS) = FSUM(T14(48,IY,IS),3)

        T14(51,IY,IS) = T14( 2,IY,IS) + T14(47,IY,IS)
        T14(52,IY,IS) = T14( 3,IY,IS) + T14(48,IY,IS)
        T14(53,IY,IS) = T14( 6,IY,IS) + T14(49,IY,IS)
        T14(54,IY,IS) = T14( 7,IY,IS) + T14(50,IY,IS)
 140  CONTINUE

! Table 15.  Coal Supply, Disposition and Prices

      DO 150 IY=1,LASTYR
        T15( 1,IY,IS) = APPSTOCKS(IY) + ABSULF(4,IY) + APSULF(4,IY)
        T15( 2,IY,IS) = INTSTOCKS(IY) + IBSULF(4,IY) + ILSULF(4,IY)
        T15( 3,IY,IS) = WESTSTOCKS(IY) + WBSULF(4,IY) + WSSULF(4,IY) + WLSULF(4,IY) + WPSULF(4,IY)
!  PRODUCTION
      IF(CQSBT(1,IY) .NE. 0.0)T15(4,IY,IS)=APPSTOCKS(IY) + INTSTOCKS(IY) + CQSBB(1,IY)/CQSBT(1,IY)
      IF(CQSBT(2,IY) .NE. 0.0)T15(5,IY,IS)=WESTSTOCKS(IY) + CQSBB(2,IY)/CQSBT(2,IY)
      IF(CQSBT(3,IY) .NE. 0.0)T15(6,IY,IS)=APPSTOCKS(IY) + INTSTOCKS(IY) +WESTSTOCKS(IY) + CQSBB(3,IY)/CQSBT(3,IY)
      T15(29,IY,IS) = sum(WC_PROD_ST(11,1:MNUMLR,IY))
!  IMPORT,EXPORT
       IF(CQDBFT(11,7,IY) .NE.  0.0) &
         T15(7,IY,IS) = CQDBFB(11,7,IY)/CQDBFT(11,7,IY)
       IF(CQDBFT(11,5,IY) .NE.  0.0) &
         T15(8,IY,IS) = CQDBFB(11,5,IY)/CQDBFT(11,5,IY)
      T15(9,IY,IS)=T15(7,IY,IS)-T15(8,IY,IS)
!  TOTAL SUPPLY
      T15(10,IY,IS)=T15(6,IY,IS)+T15(9,IY,IS)+T15(29,IY,IS)
!  CONSUMPTION BY SECTOR
             IF(CQDBFT(11,1,IY) .NE. 0.0) &
      T15(11,IY,IS)=(QCLRS(11,IY)+QCLCM(11,IY))/CQDBFT(11,1,IY)*1000.
             IF(CQDBFT(11,3,IY) .NE. 0.0) &
      T15(13,IY,IS)=INDCOKEBAL(IY) + QMCIN(11,IY)/CQDBFT(11,3,IY)*1000.
             IF(CQDBFT(11,6,IY) .NE. 0.0) &
      T15(14,IY,IS)=QCLEL(11,IY)/CQDBFT(11,6,IY)*1000.
if (IY+1989 .GT. (CUMCAPADD+2)) THEN
             IF(CQDBFT(11,2,IY) .NE. 0.0) &
      T15(12,IY,IS)=INDSTEAMBAL(IY) + (QCLIN(11,IY)-QCTLRF(11,IY))/CQDBFT(11,2,IY)*1000.
             IF(CQDBFT(11,4,IY) .NE. 0.0) THEN
                 T15(25,IY,IS)=(QCLSN(11,IY)-QCTLRF(11,IY))/CQDBFT(11,4,IY)*1000.
                 T15(26,IY,IS)=QCTLRF(11,IY)/CQDBFT(11,4,IY)*1000.
             ENDIF
else
             IF(CQDBFT(11,2,IY) .NE. 0.0) T15(12,IY,IS)=INDSTEAMBAL(IY) + QCLIN(11,IY)/CQDBFT(11,2,IY)*1000.
                 T15(25,IY,IS)=0.0
                 T15(26,IY,IS)=0.0
endif
      T15(15,IY,IS)=FSUM(T15(11,IY,IS),4)                          ! sectoral consumption
      T15(27,IY,IS)=FSUM(T15(11,IY,IS),4)+FSUM(T15(25,IY,IS),2)    ! total coal use
      T15(16,IY,IS)=T15(10,IY,IS)-T15(27,IY,IS)                    ! discrepancy
      T15(17,IY,IS)= CPSB(3,IY) * CPSBT(3,IY)
      T15(18,IY,IS)= CPSB(3,IY)
!     --- Delivered Prices
      T15(39,IY,IS)=PCLCM(11,IY)*CPDBFT(11,1,IY)
      T15(19,IY,IS)=PCLIN(11,IY)*CPDBFT(11,2,IY)
      T15(20,IY,IS)=PMCIN(11,IY)*CPDBFT(11,3,IY)
      T15(21,IY,IS)=PCLEL(11,IY)*CPDBFT(11,6,IY)
      T15(22,IY,IS)=PCLEL(11,IY)
      IF ((CPDBFT(11,2,IY) .NE. 0.0) .AND. (CPDBFT(11,3,IY) .NE. 0.0) .AND. &
          (CPDBFT(11,4,IY) .NE. 0.0) .AND. (QCLSN(11,IY) .NE. 0.0) .AND. &
          (CPDBFT(11,6,IY) .NE. 0.0) .AND. (CPDBFT(11,1,IY) .NE. 0.0)) THEN
         T15(23,IY,IS) = ( &
            PCLCM(11,IY)*QCLCM(11,IY) + &
            PCLSN(11,IY)*QCLSN(11,IY) + &
            PCLIN(11,IY)*(QCLIN(11,IY)-QCTLRF(11,IY)) + &
            PMCIN(11,IY)*(QMCIN(11,IY)) + &
            PCLEL(11,IY)*(QCLEL(11,IY)))/ &
         ((QCLCM(11,IY))/CPDBFT(11,1,IY)+ &
          (QCLIN(11,IY)-QCTLRF(11,IY))/CPDBFT(11,2,IY)+ &
          (QMCIN(11,IY))/CPDBFT(11,3,IY)+ &
          (QCLEL(11,IY))/CPDBFT(11,6,IY)+QCLSN(11,IY)/CPDBFT(11,4,IY))
      ELSE IF ((CPDBFT(11,1,IY) .NE. 0.0) .AND. (CPDBFT(11,2,IY) .NE. 0.0) .AND. (CPDBFT(11,3,IY) .NE. 0.0) .AND. (CPDBFT(11,6,IY) .NE. 0.0)) THEN
         T15(23,IY,IS) = ( &
            PCLCM(11,IY)*(QCLCM(11,IY)) + &
            PCLIN(11,IY)*(QCLIN(11,IY)-QCTLRF(11,IY)) + &
            PMCIN(11,IY)*(QMCIN(11,IY)) + &
            PCLEL(11,IY)*(QCLEL(11,IY)))/ &
         ((QCLCM(11,IY))/CPDBFT(11,1,IY)+ &
          (QCLIN(11,IY)-QCTLRF(11,IY))/CPDBFT(11,2,IY)+ &
          (QMCIN(11,IY))/CPDBFT(11,3,IY)+ &
          (QCLEL(11,IY))/CPDBFT(11,6,IY))
      ELSE IF ((CPDBFT(11,2,IY) .NE. 0.0) .AND. (CPDBFT(11,3,IY) .NE. 0.0) .AND. (CPDBFT(11,6,IY) .NE. 0.0)) THEN
         T15(23,IY,IS) = ( &
            PCLIN(11,IY)*(QCLIN(11,IY)-QCTLRF(11,IY)) + &
            PMCIN(11,IY)*(QMCIN(11,IY)) + &
            PCLEL(11,IY)*(QCLEL(11,IY)))/ &
         ((QCLIN(11,IY)-QCTLRF(11,IY))/CPDBFT(11,2,IY)+ &
          (QMCIN(11,IY))/CPDBFT(11,3,IY)+ &
          (QCLEL(11,IY))/CPDBFT(11,6,IY))
       ENDIF
      T15(24,IY,IS) = PCLEX(11,IY) * CPDBFT(11,5,IY)
      T15(28,IY,IS) = PCLSN(11,IY) * CPDBFT(11,4,IY)
      T15(30,IY,IS) = T15(17,IY,IS) / SCALPR * MC_JPGDP(IY)
      T15(31,IY,IS) = T15(18,IY,IS) / SCALPR * MC_JPGDP(IY)
      T15(32,IY,IS) = T15(20,IY,IS) / SCALPR * MC_JPGDP(IY)
      T15(33,IY,IS) = T15(19,IY,IS) / SCALPR * MC_JPGDP(IY)
      T15(34,IY,IS) = T15(28,IY,IS) / SCALPR * MC_JPGDP(IY)
      T15(35,IY,IS) = T15(21,IY,IS) / SCALPR * MC_JPGDP(IY)
      T15(36,IY,IS) = T15(22,IY,IS) / SCALPR * MC_JPGDP(IY)
      T15(37,IY,IS) = T15(23,IY,IS) / SCALPR * MC_JPGDP(IY)
      T15(38,IY,IS) = T15(24,IY,IS) / SCALPR * MC_JPGDP(IY)
      T15(40,IY,IS) = T15(39,IY,IS) / SCALPR * MC_JPGDP(IY)
150   CONTINUE

! Table 16 - Renewable Energy

      DO 160 IY=1,LASTYR
!     --- ELECTRICITY CAPABILITY
      T16(1,IY,IS)=UCAPHYU(mnumnr,IY) + UCAPHYN(mnumnr,IY) + UCAPHYC(mnumnr,IY)
      T16(2,IY,IS)=UCAPGEU(mnumnr,IY) + UCAPGEN(mnumnr,IY) + UCAPGEC(mnumnr,IY)
      T16(3,IY,IS)=UCAPMSU(mnumnr,IY) + UCAPMSN(mnumnr,IY) + UCAPMSC(mnumnr,IY)
      T16(4,IY,IS)=UCAPWDU(mnumnr,IY) + UCAPWDN(mnumnr,IY) + UCAPWDC(mnumnr,IY)
      T16(5,IY,IS)=UCAPSTU(mnumnr,IY) + UCAPSTN(mnumnr,IY) + UCAPSTC(mnumnr,IY)
      T16(6,IY,IS)=UCAPPVU(mnumnr,IY) + UCAPPVN(mnumnr,IY) + UCAPPVC(mnumnr,IY) + &
                   UCAPPTU(mnumnr,IY) + UCAPPTN(mnumnr,IY) + UCAPPTC(mnumnr,IY)
      T16(7,IY,IS)=UCAPWNU(mnumnr,IY) + UCAPWNN(mnumnr,IY) + UCAPWNC(mnumnr,IY) + &
                   UCAPWLU(mnumnr,IY) + UCAPWLN(mnumnr,IY) + UCAPWLC(mnumnr,IY)
      T16(8,IY,IS)=UCAPWFU(mnumnr,IY) + UCAPWFN(mnumnr,IY) + UCAPWFC(mnumnr,IY)
      T16(9,IY,IS)= FSUM(T16(1,IY,IS),8)
!     --- ELECTRIC GENERATION
      T16(10,IY,IS) = UGNHYNR(1,mnumnr,IY) + UGNHYNR(2,mnumnr,IY) + &
                     (CGNTGEN(mnumnr,IY, 4,1) + CGNTGEN(mnumnr,IY, 4,2)) * 0.001
      T16(11,IY,IS) = UGNGENR(1,mnumnr,IY) + UGNGENR(2,mnumnr,IY) + &
                     (CGNTGEN(mnumnr,IY, 5,1) + CGNTGEN(mnumnr,IY, 5,2)) * 0.001
      T16(12,IY,IS) = UGNMSNR(1,mnumnr,IY) + UGNMSNR(2,mnumnr,IY) + &
                     (CGNTGEN(mnumnr,IY, 6,1) + CGNTGEN(mnumnr,IY, 6,2)) * 0.001
      T16(13,IY,IS) = UGNWDNR(1,mnumnr,IY) + UGNWDNR(2,mnumnr,IY) + &
                     (CGNTGEN(mnumnr,IY, 7,1) + CGNTGEN(mnumnr,IY, 7,2)) * 0.001
      T16(14,IY,IS) = UGNWDNR(1,mnumnr,IY) + UGNWDNR(2,mnumnr,IY) + &
                     (CGNTGEN(mnumnr,IY, 7,1) + CGNTGEN(mnumnr,IY, 7,2)) * 0.001 - &
                      UGNCFNR(1,mnumnr,IY) - UGNCFNR(2,mnumnr,IY)
      T16(15,IY,IS) = UGNCFNR(1,mnumnr,IY) + UGNCFNR(2,mnumnr,IY)
      T16(16,IY,IS) = UGNSONR(1,mnumnr,IY) + UGNSONR(2,mnumnr,IY)
      T16(17,IY,IS) = UGNPVNR(1,mnumnr,IY) + UGNPVNR(2,mnumnr,IY) + &
                      UGNPTNR(1,mnumnr,IY) + UGNPTNR(2,mnumnr,IY) + &
                     (CGNTGEN(mnumnr,IY, 8,1) + CGNTGEN(mnumnr,IY, 8,2)) * 0.001
      T16(18,IY,IS) = UGNWNNR(1,mnumnr,IY) + UGNWNNR(2,mnumnr,IY) + &
                      UGNWLNR(1,mnumnr,IY) + UGNWLNR(2,mnumnr,IY)
      T16(19,IY,IS) = UGNWFNR(1,mnumnr,IY) + UGNWFNR(2,mnumnr,IY)
      T16(20,IY,IS) = FSUM(T16(10,IY,IS),4) + FSUM(T16(16,IY,IS),4)
!  --- Combined heat and power capacity
      T16(22,IY,IS) =(CGCOMMCAP(11,IY,6) + CGREFCAP(11,IY,6) + CGINDLCAP(11,IY,6)) / 1000.
      T16(23,IY,IS) =(CGCOMMCAP(11,IY,7) + CGREFCAP(11,IY,7) + CGINDLCAP(11,IY,7)) / 1000.
      T16(21,IY,IS) =(CGCOMMCAP(11,IY,4) + CGINDLCAP(11,IY,4)) / 1000.
      T16(29,IY,IS) =(CGCOMMCAP(11,IY,5) + CGINDLCAP(11,IY,5)) / 1000.
      T16(30,IY,IS) =(CGCOMMCAP(11,IY,8) + &
                      CGRESCAP(11,IY,8) + CGINDLCAP(11,IY,8)) / 1000.
      T16(35,IY,IS) =(CGCOMMCAP(11,IY,11) + &
                      CGRESCAP(11,IY,11) + CGINDLCAP(11,IY,11)) / 1000.
      T16(31,IY,IS) = FSUM(T16(21,IY,IS),3) + FSUM(T16(29,IY,IS),2) + T16(35,IY,IS)
!  --- Combined heat and power generation
      T16(26,IY,IS) =(CGCOMMGEN(11,IY,6,1) + CGCOMMGEN(11,IY,6,2) + &
                      CGREFGEN(11,IY,6,1)  + CGREFGEN(11,IY,6,2) + &
                      CGINDLGEN(11,IY,6,1) + CGINDLGEN(11,IY,6,2)) * .001
      T16(27,IY,IS) =(CGCOMMGEN(11,IY,7,1) + CGCOMMGEN(11,IY,7,2) + &
                      CGREFGEN(11,IY,7,1)  + CGREFGEN(11,IY,7,2) + &
                      CGINDLGEN(11,IY,7,1) + CGINDLGEN(11,IY,7,2)) * .001
      T16(25,IY,IS) =(CGCOMMGEN(11,IY,4,1) + CGCOMMGEN(11,IY,4,2) + &
                      CGINDLGEN(11,IY,4,1) + CGINDLGEN(11,IY,4,2)) * .001
      T16(32,IY,IS) =(CGCOMMGEN(11,IY,5,1) + CGCOMMGEN(11,IY,5,2) + &
                      CGINDLGEN(11,IY,5,1) + CGINDLGEN(11,IY,5,2)) * .001
      T16(33,IY,IS) =(CGCOMMGEN(11,IY,8,1) + CGCOMMGEN(11,IY,8,2) + &
                      CGRESGEN(11,IY,8,1)  + CGRESGEN(11,IY,8,2) + &
                      CGINDLGEN(11,IY,8,1) + CGINDLGEN(11,IY,8,2)) * .001
      T16(36,IY,IS) =(CGCOMMGEN(11,IY,11,1)+ CGCOMMGEN(11,IY,11,2) + &
                      CGRESGEN(11,IY,11,1) + CGRESGEN(11,IY,11,2) + &
                      CGINDLGEN(11,IY,11,1)+ CGINDLGEN(11,IY,11,2)) * .001
      T16(34,IY,IS) = FSUM(T16(25,IY,IS),3) + FSUM(T16(32,IY,IS),2) + T16(36,IY,IS)
! Everybody
      T16(39,IY,IS) = T16( 1,IY,IS) + T16(21,IY,IS)
      T16(40,IY,IS) = T16( 2,IY,IS) + T16(29,IY,IS)
      T16(41,IY,IS) = T16( 3,IY,IS) + T16(22,IY,IS)
      T16(42,IY,IS) = T16( 4,IY,IS) + T16(23,IY,IS)
      T16(43,IY,IS) = FSUM(T16( 5,IY,IS),2) + T16(30,IY,IS)
      T16(44,IY,IS) = FSUM(T16( 7,IY,IS),2) + T16(35,IY,IS)
      T16(45,IY,IS) = FSUM(T16(39,IY,IS),6)

      T16(46,IY,IS) = T16(10,IY,IS) + T16(25,IY,IS)
      T16(47,IY,IS) = T16(11,IY,IS) + T16(32,IY,IS)
      T16(48,IY,IS) = T16(12,IY,IS) + T16(26,IY,IS)
      T16(49,IY,IS) = T16(13,IY,IS) + T16(27,IY,IS)
      T16(50,IY,IS) = FSUM(T16(16,IY,IS),2) + T16(33,IY,IS)
      T16(51,IY,IS) = FSUM(T16(18,IY,IS),2) + T16(36,IY,IS)
      T16(52,IY,IS) = FSUM(T16(46,IY,IS),6)

160   CONTINUE

! Table 17  Carbon Emissions by Source and End-Use Sector

        DO 170 IY=1,LASTYR
        DO IR=1,MNUMCR                 !  Do regions in case we make this regional table
        ICY=IY+BASEYR-1
!         --- ELECTRIC POWER SECTOR
          T17(20,IR,IY,IS) = sum(em_elec(1:3,IR,ICY))
          T17(21,IR,IY,IS) = em_elec(5,IR,ICY)
          T17(22,IR,IY,IS) = em_elec(4,IR,ICY)
          T17(23,IR,IY,IS) = sum(em_elec(6:7,IR,ICY)) ! nonbiogenic portion of msw, plus geothermal
          T17(24,IR,IY,IS) = FSUM(T17(20,IR,IY,IS),4)
!         --- RESIDENTIAL
          T17( 1,IR,IY,IS) = sum(em_resd(1:3,IR,ICY))
          T17( 2,IR,IY,IS) = em_resd(5,IR,ICY)
          T17( 3,IR,IY,IS) = em_resd(4,IR,ICY)
          T17( 4,IR,IY,IS) = em_resd(6,IR,ICY)
          T17( 5,IR,IY,IS) = FSUM(T17(1,IR,IY,IS),4)
!         --- COMMERCIAL
          T17( 6,IR,IY,IS) = sum(em_comm(1:5,IR,ICY))
          T17( 7,IR,IY,IS) = em_comm(7,IR,ICY)
          T17( 8,IR,IY,IS) = em_comm(6,IR,ICY)
          T17( 9,IR,IY,IS) = em_comm(8,IR,ICY)
          T17(10,IR,IY,IS) = FSUM(T17(6,IR,IY,IS),4)
!         --- INDUSTRIAL
          T17(11,IR,IY,IS) = sum(em_indy(1:10,IR,ICY))
          T17(12,IR,IY,IS) = sum(em_indy(14:16,IR,ICY))
          T17(13,IR,IY,IS) = sum(em_indy(11:13,IR,ICY))
          T17(14,IR,IY,IS) = em_indy(17,IR,ICY)
          T17(15,IR,IY,IS) = FSUM(T17(11,IR,IY,IS),4)
!         --- TRANSPORTATION
          T17(16,IR,IY,IS) = sum(em_tran(1:9,IR,ICY)) + em_tran(11,IR,ICY)
          T17(17,IR,IY,IS) = em_tran(10,IR,ICY)
          T17(18,IR,IY,IS) = em_tran(12,IR,ICY)
          T17(19,IR,IY,IS) = FSUM(T17(16,IR,IY,IS),3)
!         --- FUEL TOTALS
          T17(25,IR,IY,IS) = T17( 1,IR,IY,IS) + T17( 6,IR,IY,IS) + T17(11,IR,IY,IS) + T17(16,IR,IY,IS) + T17(20,IR,IY,IS)
          T17(26,IR,IY,IS) = T17( 2,IR,IY,IS) + T17( 7,IR,IY,IS) + T17(12,IR,IY,IS) + T17(17,IR,IY,IS) + T17(21,IR,IY,IS)
          T17(27,IR,IY,IS) = T17( 3,IR,IY,IS) + T17( 8,IR,IY,IS) + T17(13,IR,IY,IS) + T17(22,IR,IY,IS)
          T17(28,IR,IY,IS) = T17(23,IR,IY,IS)
          T17(29,IR,IY,IS) = FSUM(T17(25,IR,IY,IS),4)
!         T118(74,iy,is)= T17(29,IR,IY,IS) ! See Table 118 if t17(29,,,,) changed. Search code for t17(29
          IF (MC_NP(IR,IY) .NE. 0.0) T17(30,IR,IY,IS) =T17(29,IR,IY,IS) / MC_NP(IR,IY)
        ENDDO
  170   CONTINUE

! TABLE 18 MACROECONOMIC

      T18(1:NTAB018,1:MNUMYR,IS) = 0.0
      DO 180 IY=1,LASTYR
        IF (MC_JPGDP(MACYR-1989) .NE. 0)   &
          T18( 1,IY,IS) = MC_JPGDP(IY) / MC_JPGDP(MACYR-1989)
        T18(32,IY,IS) = MC_GDPFER(IY)
        T18( 2,IY,IS) = MC_GDPR(IY)
        T18( 3,IY,IS) = MC_CONSR(IY)
        T18( 4,IY,IS) = MC_IFNRER(IY)
        T18( 5,IY,IS) = MC_GR(IY)
        T18( 6,IY,IS) = MC_XR(IY)
        T18( 7,IY,IS) = MC_MR(IY)
        T18( 8,IY,IS) = MC_YPDR(11,IY)
        T18( 9,IY,IS) = MC_RMCORPPUAA(IY)
        T18(10,IY,IS) = MC_RMFF(IY)
        T18(11,IY,IS) = MC_RLRMCORPPUAA(IY) ! redefined below when real AA bond rate dropped from table
        IF(MC_GDPR(IY) .NE. 0.0) THEN
! use table 2 rows so consumption is guaranteed consistent
          T18(12,IY,IS) = T2( 76,MNUMCR,IY,IS) / MC_GDPR(IY) * 1000.
          T18(13,IY,IS) = T2(106,MNUMCR,IY,IS) / MC_GDPR(IY) * 1000.
        ENDIF
        T18(14,IY,IS) = MC_CPI(11,IY)
        T18(15,IY,IS) = MC_RUC(IY)
        T18(33,IY,IS) = MC_WPI(IY)      ! wpi
        T18(34,IY,IS) = MC_WPI05(IY)    ! wpi, fuel and power
        T18(38,IY,IS) = MC_WPI10(IY)
        T18(41,IY,IS) = MC_WPIIND05(IY)   ! Industrial commodities excluding energy
        T18(11,IY,IS) = MC_RMTCM10Y(IY)   ! 10-year treasury note (used to be AA bond rate, real)
        T18(16,IY,IS) = MC_HUSPS1(11,IY)
        T18(17,IY,IS) = MC_HUSPS2A(11,IY)
        T18(18,IY,IS) = MC_HUSMFG(11,IY)
        T18(31,IY,IS) = FSUM(T18(16,IY,IS),3)
        T18(19,IY,IS) = CMUSSURVFLOORTOT(IY) + CMUSNEWFLOORTOT(IY)
        DO III=1,48
          IF ((III .ge. 2 .AND. III .le. 5) .OR. (III .ge. 21 .AND. III .le. 24) .OR.   &
              (III .ge. 11 .AND. III .le. 13) .OR.     &
              (III .eq. 17) .OR. (III .eq. 29)) CYCLE  ! these are subsets of other elements
          T18(20,IY,IS) = T18(20,IY,IS) + MC_REVIND(11,III,IY)
          IF (III .LE. 41) THEN
            T18(22,IY,IS) = T18(22,IY,IS) + MC_REVIND(11,III,IY)
            IF (III .EQ.  1 .OR. III .EQ. 10 .OR. (III .GE. 15 .AND. III .LE. 19) .OR. &
                III .EQ. 25 .OR. III .EQ. 28 .OR. III .EQ. 30 .OR. III .EQ. 31 .OR. &
                III .EQ. 33 .OR. III .EQ. 34) THEN
              T18(23,IY,IS) = T18(23,IY,IS) + MC_REVIND(11,III,IY)
            ELSE
              T18(24,IY,IS) = T18(24,IY,IS) + MC_REVIND(11,III,IY)
            ENDIF
          ELSE
            T18(21,IY,IS) = T18(21,IY,IS) + MC_REVIND(11,III,IY)
          ENDIF
        ENDDO
        T18(25,IY,IS) = MC_SUVTL(IY) + MC_SUVA(IY)
        T18(26,IY,IS) = MC_NP(11,IY)
        T18(27,IY,IS) = MC_NP16A(11,IY)
        T18(29,IY,IS) = sum(MC_EMPNA(11,1:19,IY))
        T18(28,IY,IS) = T18(29,IY,IS) + sum(MC_EMPNA(11,22:39,IY))
        T18(30,IY,IS) = MC_NLFC(IY)
        T18(35,IY,IS) = MC_CPIE(IY)
        T18(36,IY,IS) = MC_NP65A(IY)
        T18(37,IY,IS) = MC_JQPCMHNF(IY)
        T18(39,IY,IS) = sum(MC_REVSER(11,1:10,IY))
        T18(40,IY,IS) = T18(20,IY,IS) + T18(39,IY,IS)
180   CONTINUE

!  TABLE 19  International Energy Balance

      DO 190 IY=1,LASTYR
        T19(65,IY,IS) = WTI_PRICE(IY)
        T19(66,IY,IS) = BRENT_PRICE(IY)
        T19(67,IY,IS) = T19(65,IY,IS) / SCALPR * MC_JPGDP(IY)
        T19(68,IY,IS) = T19(66,IY,IS) / SCALPR * MC_JPGDP(IY)
!   Fill in entire table from international model array
        T19( 1:57,IY,IS) = REPORT(IY, 1:57)
        T19(70:87,IY,IS) = REPORT2(IY,1:18)
! Replace U.S. values with NEMS result - may have changed since last seen (international runs first)
! Domestic liquids production
        T19( 1,IY,IS)= RFQTDCRD(MNUMOR+2, IY) + &                  !Crude
           RFQNGPL(MNUMPR, IY, 6) / 1000. +     &                  !NGPLs
           RFQPRCG(MNUMPR,IY) + &                            !Refinery processing gain
          0.9751*(CRNETHCD(11,IY)+CLLETHCD(11,IY)+OTHETHCD(11,IY))/1000. + &       ! Domestic ethanol
          sum(BIMQTYCD(1:4,11,IY))/1000. + &                                ! Domestic biodiesel
          sum(GTLFRAC(1:4,MNUMPR,IY))/1000. + &                             ! Gas to liquids
          sum(CTLFRAC(1:4,MNUMPR,IY))/1000 + sum(CBTLFRAC(1,1:4,MNUMPR,IY))/1000. + & ! Coal to liquids
          sum(BTLFRAC(1:4,MNUMPR,IY))/1000 + sum(CBTLFRAC(2,1:4,MNUMPR,IY))/1000. + & ! Biomass to liquids
          UBAVOL(MNUMPR,IY)/1000. + &                                       ! pyrolysis
          RFBIOBUTECD(MNUMCR,IY)/1000. + &                                  ! biobutynol
          (GRD2DSQTY(MNUMPR,IY) + GRN2MGQTY(MNUMPR,IY)) / 1000. + &         ! renewable gasolie and diesel
          RFHCXH2IN(MNUMPR,IY) + RFMETM85(MNUMPR,IY)                        ! other oxygenates and methanol


!  Domestic liquids consumption
        T19(29,IY,IS) = RFQMG(11,IY) + RFQJF(11,IY) + RFQDS(11,IY) + RFQRL(11,IY) + &
                        RFQRH(11,IY) + RFQPF(11,IY) + RFQKS(11,IY) + RFQLG(11,IY) + &
                        RFQPCK(11,IY) + RFQARO(11,IY) + RFQSTG(11,IY) + RFQOTH(11,IY)

!  Domestic petroleum production
        T19(28,IY,IS) = RFQTDCRD(MNUMOR+2,IY) - (OGQCRREP(1,IY) / 365.)

!production totals, first liquids
        T19(46,IY,IS) = FSUM(T19(16,IY,IS),4)     ! OPEC total
        T19(47,IY,IS) = FSUM(T19( 1,IY,IS),6)     ! OECD total
        T19(48,IY,IS) = FSUM(T19( 7,IY,IS),8)     ! Non-OECD total
        T19(49,IY,IS) = FSUM(T19(46,IY,IS),3)     ! total liquids
                  ! then petroleum
        T19(88,IY,IS) = FSUM(T19(79,IY,IS),4)     ! OPEC total
        T19(89,IY,IS) = FSUM(T19(70,IY,IS),5) + T19(28,IY,IS)             ! OECD total
        T19(90,IY,IS) = FSUM(T19(75,IY,IS),3) + FSUM(T19(83,IY,IS),5)     ! Non-OECD total
        T19(69,IY,IS) = FSUM(T19(88,IY,IS),3)     ! total petroleum
! consumption totals
        T19(52,IY,IS) = FSUM(T19(29,IY,IS),7)  + T19(40,IY,IS)    ! OECD consumption
        T19(53,IY,IS) = FSUM(T19(36,IY,IS),10) - T19(40,IY,IS)    ! Non-OECD consumption
        T19(54,IY,IS) = FSUM(T19(52,IY,IS),2)     ! Total consumption
! OPEC market shares
        T19(55,IY,IS) = T19(46,IY,IS) / T19(49,IY,IS) * 100.
        T19(78,IY,IS) = T19(88,IY,IS) / T19(69,IY,IS) * 100.

        T19(20,IY,IS) = T19(69,IY,IS)     ! set crude oil in Selected section equal to total world crude
 190  CONTINUE

! TABLE 20 - Conversion Factors

      DO 200 IY=1,LASTYR
!        --- PETROLEUM PRODUCTS
         T20( 1,IY,IS) = CFASQ
         T20(64,IY,IS) = CFAVQ
         T20( 2,IY,IS) = CFBUQ
         T20( 3,IY,IS) = CFDSQ
         T20(40,IY,IS) = CFDSRS(IY)
         T20(41,IY,IS) = CFDSCM(IY)
         T20(42,IY,IS) = CFDSTR(IY)
         T20(43,IY,IS) = CFDSIN(IY)
         T20(44,IY,IS) = CFDSEL(IY)
         T20(45,IY,IS) = CFDSQT(IY)
         T20(73,IY,IS) = CFDSLQ(IY)
         T20(74,IY,IS) = CFDSUQ(IY)
         T20(72,IY,IS) = CFDSCQ(IY)
         T20(65,IY,IS) = CFEEQ
         T20( 4,IY,IS) = CFETQ(IY)
         T20(56,IY,IS) = CFE85Q(IY)
         T20( 5,IY,IS) = CFIBQ
         T20( 6,IY,IS) = CFJFK
         T20( 7,IY,IS) = CFJFN
         T20(81,IY,IS) = CFJFQ(IY)
         T20( 8,IY,IS) = CFKSQ
         T20( 9,IY,IS) = CFLGQ(IY)
         T20(66,IY,IS) = CFLUQ
         T20(57,IY,IS) = CFMEQT
         T20(58,IY,IS) = CFM85Q(IY)
         T20(10,IY,IS) = CFMGQ(IY)
         T20(52,IY,IS) = CFTGQ(IY)
         T20(53,IY,IS) = CFRGQ(IY)
         T20(55,IY,IS) = APICAMG(2,IY)
         T20(98,IY,IS) = CFCBOB(IY)
         T20(99,IY,IS) = CFRBOB(IY)
         T20(100,IY,IS)= CFCBQ(IY)
         T20(63,IY,IS) = CFNPQ
         T20(12,IY,IS) = CFPPQ
         T20(11,IY,IS) = CFOTQ(IY)
         T20(13,IY,IS) = CFPFQ(IY)
         T20(14,IY,IS) = CFPCQ
         T20(82,IY,IS) = CFCCQ(IY)
         T20(15,IY,IS) = CFPRQ
         T20(16,IY,IS) = CFRSQ
         T20(17,IY,IS) = CFSGQ
         T20(67,IY,IS) = CFWXQ
         T20(68,IY,IS) = CFMSQ
         T20(69,IY,IS) = CFUSQ
         T20(71,IY,IS) = CFIMPRD(IY)   ! don't use - leaves out blending components
         T20(33,IY,IS) = T1(10,IY,IS) / (((BIODIMP(11,IY) + ETHIMP(11,IY)+ RENEWDIMP(11,IY))/1000. + FSUM(T11( 9,IY,IS),3)) * 365. / 1000.)
         T20(70,IY,IS) = CFIMUO(IY)
         T20(34,IY,IS) = CFEXPRD(IY)
         T20(37,IY,IS) = CFNGL(IY)
         T20(35,IY,IS) = CFCRDDOM(IY)
         T20(36,IY,IS) = CFCRDIMP(IY)
         T20(83,IY,IS) = CFCRDEXP(IY)
         T20(38,IY,IS) = CFTPQ(IY)
         T20(38,IY,IS) = T1(19,IY,IS) / (T11(48,IY,IS) * 365. / 1000.)
!  types of crude oil
         T20(84,IY,IS) = CFCRDLTSWT(IY)
         T20(85,IY,IS) = CFCRDLTSOUR(IY)
         T20(86,IY,IS) = CFCRDMD2SOUR(IY)
         T20(87,IY,IS) = CFCRDMDSOUR(IY)
         T20(88,IY,IS) = CFCRDHVSWT(IY)
         T20(89,IY,IS) = CFCRDHVSOUR(IY)
         T20(90,IY,IS) = CFCRDCA(IY)
         T20(91,IY,IS) = CFCRDSYN(IY)
         T20(92,IY,IS) = CFCRDDILBIT(IY)
         T20(93,IY,IS) = CFCRDLT2SWT(IY)
         T20(94,IY,IS) = CFCRDLSCOND(IY)
! BOBs
         T20(95,IY,IS) = CFCBOB(IY)
         T20(96,IY,IS) = CFRBOB(IY)
         T20(97,IY,IS) = CFCBQ(IY)
!        --- NATURAL GAS
         T20(18,IY,IS) = CFNGU(IY)
         T20(19,IY,IS) = CFNGN(IY)
         T20(20,IY,IS) = CFNGC(IY)
         T20(21,IY,IS) = CFNGI(IY)
         T20(22,IY,IS) = CFNGE(IY)
         T20(62,IY,IS) = CFCNGQ(IY)
!        --- COAL
         T20(23,IY,IS) = CQSBT(1,IY)
         T20(24,IY,IS) = CQSBT(2,IY)
         T20(25,IY,IS) = CQSBT(3,IY)
         T20(26,IY,IS) = CQDBFT(11,1,IY)
         T20(27,IY,IS) = CQDBFT(11,2,IY)
         T20(28,IY,IS) = CQDBFT(11,3,IY)
         T20(29,IY,IS) = CQDBFT(11,5,IY)
         T20(30,IY,IS) = CQDBFT(11,6,IY)
         T20(31,IY,IS) = CQDBFT(11,7,IY)
         T20(46,IY,IS) = CQDBFT(11,4,IY)
         T20(39,IY,IS) = ((QCLRS(11,IY)+QCLCM(11,IY))*CQDBFT(11,1,IY) + &
           QMCIN(11,IY)*CQDBFT(11,3,IY) + QCLEL(11,IY)*CQDBFT(11,6,IY) + &
          (QCLIN(11,IY)-QCTLRF(11,IY))*CQDBFT(11,2,IY) + QCLSN(11,IY)*CQDBFT(11,4,IY)) / &
          (QCLRS(11,IY)+QCLCM(11,IY)+QMCIN(11,IY)+QCLEL(11,IY)+QCLIN(11,IY)-QCTLRF(11,IY)+QCLSN(11,IY))
         T20(50,IY,IS) = sum(WC_PROD_BTU(11,1:MNUMLR,IY))/sum(WC_PROD_ST(11,1:MNUMLR,IY))
!        --- ELECTRICITY
         T20(32,IY,IS) = CFELQ
         T20(101:109,IY,IS)= WHRFOSS(1:9,IY)
         T20(110,IY,IS)= WHRFOSS(MNUMCR,IY)
! odds and ends
         T20(47,IY,IS) = CFCORN / 1000.
         T20(77,IY,IS) = CFGRAIN(IY) / 1000.
         T20(48,IY,IS) = CFCELL / 1000.
         T20(51,IY,IS) = CONEFF(IY)
         T20(59,IY,IS) = CFBMQ(IY)
         T20(60,IY,IS) = CFBIOD(IY)
         T20(61,IY,IS) = CFVEGGIE(IY)
         T20(49,IY,IS) = 0.0   !  unused for now
         T20(54,IY,IS) = CFCTLLIQ(IY)
         T20(75,IY,IS) = CFBTLLIQ(IY)
         T20(76,IY,IS) = CFGTLLIQ(IY)
         T20(78,IY,IS) = CFCBTLLIQ(3,IY)
         T20(79,IY,IS) = CFCBTLLIQ(1,IY)
         T20(80,IY,IS) = CFCBTLLIQ(2,IY)
 200   CONTINUE

! Table 21.  Liquids Supply and Disposition, not to be confused with Table 11

      RDAYS=365.
      MMBD_TO_VOLUME = 365. * 42. / 7.48 / 1000.
      MMBD_TO_MASS = 62.4 / 7.48 * 42. / 2000.
      DO IY=1,LASTYR
!   feedstocks
        T21( 1,4,IY,IS) = RFQTDCRD(MNUMOR+2,IY) * RDAYS * CFCRDDOM(IY) / 1000.
        T21( 1,1,IY,IS) = RFQTDCRD(MNUMOR+2,IY)
        T21( 2,4,IY,IS) = RFQICRD(MNUMPR,IY) * RDAYS * CFCRDIMP(IY) / 1000.
        T21( 2,1,IY,IS) = RFQICRD(MNUMPR,IY)
        T21( 2,2,IY,IS) = RFQICRD(MNUMPR,IY) * IT_WOP(IY,1)
        T21( 2,3,IY,IS) = RFQICRD(MNUMPR,IY) * MMBD_TO_MASS * .9   ! .9 placeholding specific gravity
        T21( 3,4,IY,IS) = RFPQUFC(MNUMPR,IY,2) * RDAYS * CFIMUO(IY) / 1000.
        T21( 3,1,IY,IS) = RFPQUFC(MNUMPR,IY,2)
        T21( 4,4,IY,IS) = RFQNGPL(MNUMPR,IY,6) / 1000. * RDAYS * CFNGL(IY) / 1000.
        T21( 4,1,IY,IS) = RFQNGPL(MNUMPR,IY,6) / 1000.
        T21( 5,4,IY,IS) = 0.0
        T21( 6,4,IY,IS) = 0.0
        T21( 7,4,IY,IS) = 0.0
        T21( 8,4,IY,IS) = 0.0
        T21( 9,4,IY,IS) = 0.0
        T21(10,4,IY,IS) = 0.0
!   intermediate products
        T21(11,4,IY,IS) = 0.0
        T21(12,4,IY,IS) = 0.0
        T21(13,4,IY,IS) = 0.0
        T21(14,4,IY,IS) = 0.0
        T21(15,4,IY,IS) = 0.0
        T21(16,4,IY,IS) = 0.0
        T21(17,4,IY,IS) = 0.0
        T21(18,4,IY,IS) = 0.0
        T21(19,4,IY,IS) = 0.0
        T21(20,4,IY,IS) = 0.0
        T21(21,4,IY,IS) = 0.0
        T21(22,4,IY,IS) = 0.0
        T21(23,4,IY,IS) = 0.0
!   refined products
        T21(24,4,IY,IS) = 0.0
        T21(25,4,IY,IS) = 0.0
        T21(26,4,IY,IS) = 0.0   !  will be skipped in table - phantom motor gasoline
        T21(27,4,IY,IS) = 0.0
        T21(28,4,IY,IS) = 0.0
        T21(29,4,IY,IS) = 0.0
        T21(30,4,IY,IS) = 0.0
        T21(31,4,IY,IS) = 0.0
        T21(32,4,IY,IS) = 0.0
        T21(33,4,IY,IS) = 0.0
        T21(34,4,IY,IS) = 0.0
!   imports
        T21(36,4,IY,IS) = RFIPQCBOB(MNUMPR,IY,2) * RDAYS * CFCBOB(IY) / 1000. + &
                          RFIPQRBOB(MNUMPR,IY,2) * RDAYS * CFRBOB(IY) / 1000.
        T21(37,4,IY,IS) = ETHIMP(MNUMCR,IY) / 1000. * RDAYS * CFPET / 1000.
        T21(38,4,IY,IS) = RFIPQMG(MNUMPR,IY,2) * RDAYS * CFMGQ(IY) / 1000.
        T21(39,4,IY,IS) = RFIPQJF(MNUMPR,IY,2) * RDAYS * CFJFQ(IY) / 1000.
        T21(40,4,IY,IS) = 0.0
        T21(41,4,IY,IS) = 0.0
        T21(42,4,IY,IS) =(RFIPQRH(MNUMPR,IY,2) + RFIPQRL(MNUMPR,IY,2)) * RDAYS * CFRSQ / 1000.
        T21(43,4,IY,IS) = &
(           RFIPQPR(MNUMPR,IY,2) + RFIPQPY(MNUMPR,IY,2) + RFIPQPP(MNUMPR,IY,2) + &
            RFIPQET(MNUMPR,IY,2) + RFIPQBU(MNUMPR,IY,2) + RFIPQIS(MNUMPR,IY,2)) * RDAYS * CFLGQ(IY) / 1000.
        T21(44,4,IY,IS) = 0.0
        T21(45,4,IY,IS) = 0.0
        T21(46,4,IY,IS) = 0.0
        T21(47,4,IY,IS) = 0.0
!       T21(  ,4,IY,IS) = BIODIMP(MNUMCR,IY) / 1000. * RDAYS * CFBIOD(MNUMCR,IY) / 1000.
!   exports
        T21(48,4,IY,IS) = QPRDEX( 4,IY) * RDAYS * CFCBOB(IY) / 1000. + &
                          QPRDEX( 5,IY) * RDAYS * CFRBOB(IY) / 1000.
        T21(49,4,IY,IS) = ETHEXP(MNUMCR,IY) / 1000. * RDAYS * CFPET / 1000.
        T21(50,4,IY,IS) =(QPRDEX( 2,IY) + QPRDEX( 3,IY) + &
                          QPRDEX(26,IY)) * RDAYS * CFMGQ(IY) / 1000.
        T21(51,4,IY,IS) = QPRDEX( 6,IY) * RDAYS * CFJFQ(IY) / 1000.
        T21(52,4,IY,IS) =(QPRDEX(13,IY) + QPRDEX(24,IY)) * RDAYS * CFDSTR(IY) / 1000.
        T21(53,4,IY,IS) = QPRDEX( 7,IY) * RDAYS * CFDSRS(IY) / 1000.
        T21(54,4,IY,IS) =(QPRDEX( 8,IY) + QPRDEX( 9,IY)) * RDAYS * CFRSQ / 1000.
        T21(55,4,IY,IS) =(QPRDEX( 1,IY) + QPRDEX(27,IY) + QPRDEX(28,IY) + &
                          QPRDEX(14,IY) + QPRDEX(15,IY) + &
                          QPRDEX(29,IY)) * RDAYS * CFLGQ(IY) / 1000.
        T21(56,4,IY,IS) = 0.0
        T21(57,4,IY,IS) = QPRDEX(16,IY) * RDAYS * CFPCQ / 1000.
        T21(58,4,IY,IS) = BIODEXP(MNUMCR,IY) / 1000. * RDAYS * CFBIOD(IY) / 1000.
!   domestic consumption
        T21(60,4,IY,IS) = QPRRS(MNUMCR,IY) + QPRCM(MNUMCR,IY) + QPRTR(MNUMCR,IY) + &
                          QPRTR(MNUMCR,IY) - QPRINPF(MNUMCR,IY)  ! feedstock use goes several lines down
        T21(61,4,IY,IS) = 0.0
        T21(62,4,IY,IS) = 0.0
        T21(63,4,IY,IS) = QETTR(MNUMCR,IY)
        T21(64,4,IY,IS) = QJFTR(MNUMCR,IY) - QJFBS(MNUMCR,IY)
        T21(65,4,IY,IS) = 0.0
        T21(66,4,IY,IS) = 0.0
        T21(67,4,IY,IS) = QRSAS(MNUMCR,IY)
        T21(68,4,IY,IS) = 0.0
        T21(69,4,IY,IS) = 0.0
        T21(70,4,IY,IS) = 0.0
        T21(71,4,IY,IS) = 0.0
IF (sum(  FEEDSTOCKS(1,1:10,IY)) .GT. 0.0) T21( 1:10,1,IY,IS) =   FEEDSTOCKS(1,1:10,IY)
IF (sum(INTERMEDIATE(1,1:13,IY)) .GT. 0.0) T21(11:23,1,IY,IS) = INTERMEDIATE(1,1:13,IY)
!IF (sum( REFINE_PROD(1,1:12,IY)) .GT. 0.0) T21(24:35,1,IY,IS) =  REFINE_PROD(1,1:12,IY)
!IF (sum(GROSS_IMPORT(1,1:12,IY)) .GT. 0.0) T21(36:47,1,IY,IS) = GROSS_IMPORT(1,1:12,IY)

IF (sum( REFINE_PROD(1,1: 2,IY)) .GT. 0.0) T21(24:25,1,IY,IS) =  REFINE_PROD(1,1: 2,IY)
IF (sum( REFINE_PROD(1,3:11,IY)) .GT. 0.0) T21(27:35,1,IY,IS) =  REFINE_PROD(1,3:11,IY)
IF (sum(GROSS_IMPORT(1,1:11,IY)) .GT. 0.0) T21(36:46,1,IY,IS) = GROSS_IMPORT(1,1:11,IY)

IF (sum(GROSS_EXPORT(1,1:12,IY)) .GT. 0.0) T21(48:59,1,IY,IS) = GROSS_EXPORT(1,1:12,IY)
IF (sum( DOM_CONSUME(1,1:12,IY)) .GT. 0.0) T21(60:71,1,IY,IS) =  DOM_CONSUME(1,1:12,IY)
IF (sum(  FEEDSTOCKS(2,1:10,IY)) .GT. 0.0) T21( 1:10,2,IY,IS) =   FEEDSTOCKS(2,1:10,IY)
IF (sum(INTERMEDIATE(2,1:13,IY)) .GT. 0.0) T21(11:23,2,IY,IS) = INTERMEDIATE(2,1:13,IY)
!IF (sum( REFINE_PROD(2,1:12,IY)) .GT. 0.0) T21(24:35,2,IY,IS) =  REFINE_PROD(2,1:12,IY)
!IF (sum(GROSS_IMPORT(2,1:12,IY)) .GT. 0.0) T21(36:47,2,IY,IS) = GROSS_IMPORT(2,1:12,IY)

IF (sum( REFINE_PROD(2,1: 2,IY)) .GT. 0.0) T21(24:25,2,IY,IS) =  REFINE_PROD(2,1: 2,IY)
IF (sum( REFINE_PROD(2,3:11,IY)) .GT. 0.0) T21(27:35,2,IY,IS) =  REFINE_PROD(2,3:11,IY)
IF (sum(GROSS_IMPORT(2,1:11,IY)) .GT. 0.0) T21(36:46,2,IY,IS) = GROSS_IMPORT(2,1:11,IY)

IF (sum(GROSS_EXPORT(2,1:12,IY)) .GT. 0.0) T21(48:59,2,IY,IS) = GROSS_EXPORT(2,1:12,IY)
IF (sum( DOM_CONSUME(2,1:12,IY)) .GT. 0.0) T21(60:71,2,IY,IS) =  DOM_CONSUME(2,1:12,IY)
IF (sum(  FEEDSTOCKS(3,1:10,IY)) .GT. 0.0) T21( 1:10,3,IY,IS) =   FEEDSTOCKS(3,1:10,IY)
IF (sum(INTERMEDIATE(3,1:13,IY)) .GT. 0.0) T21(11:23,3,IY,IS) = INTERMEDIATE(3,1:13,IY)
!IF (sum( REFINE_PROD(3,1:12,IY)) .GT. 0.0) T21(24:35,3,IY,IS) =  REFINE_PROD(3,1:12,IY)
!IF (sum(GROSS_IMPORT(3,1:12,IY)) .GT. 0.0) T21(36:47,3,IY,IS) = GROSS_IMPORT(3,1:12,IY)

IF (sum( REFINE_PROD(3,1: 2,IY)) .GT. 0.0) T21(24:25,3,IY,IS) =  REFINE_PROD(3,1: 2,IY)
IF (sum( REFINE_PROD(3,3:11,IY)) .GT. 0.0) T21(27:35,3,IY,IS) =  REFINE_PROD(3,3:11,IY)
IF (sum(GROSS_IMPORT(3,1:11,IY)) .GT. 0.0) T21(36:46,3,IY,IS) = GROSS_IMPORT(3,1:11,IY)

IF (sum(GROSS_EXPORT(3,1:12,IY)) .GT. 0.0) T21(48:59,3,IY,IS) = GROSS_EXPORT(3,1:12,IY)
IF (sum( DOM_CONSUME(3,1:12,IY)) .GT. 0.0) T21(60:71,3,IY,IS) =  DOM_CONSUME(3,1:12,IY)
IF (sum(  FEEDSTOCKS(4,1:10,IY)) .GT. 0.0) T21( 1:10,4,IY,IS) =   FEEDSTOCKS(4,1:10,IY)
IF (sum(INTERMEDIATE(4,1:13,IY)) .GT. 0.0) T21(11:23,4,IY,IS) = INTERMEDIATE(4,1:13,IY)
!IF (sum( REFINE_PROD(4,1:12,IY)) .GT. 0.0) T21(24:35,4,IY,IS) =  REFINE_PROD(4,1:12,IY)
!IF (sum(GROSS_IMPORT(4,1:12,IY)) .GT. 0.0) T21(36:47,4,IY,IS) = GROSS_IMPORT(4,1:12,IY)

IF (sum( REFINE_PROD(4,1: 2,IY)) .GT. 0.0) T21(24:25,4,IY,IS) =  REFINE_PROD(4,1: 2,IY)
IF (sum( REFINE_PROD(4,3:11,IY)) .GT. 0.0) T21(27:35,4,IY,IS) =  REFINE_PROD(4,3:11,IY)
IF (sum(GROSS_IMPORT(4,1:11,IY)) .GT. 0.0) T21(36:46,4,IY,IS) = GROSS_IMPORT(4,1:11,IY)

IF (sum(GROSS_EXPORT(4,1:12,IY)) .GT. 0.0) T21(48:59,4,IY,IS) = GROSS_EXPORT(4,1:12,IY)
IF (sum( DOM_CONSUME(4,1:12,IY)) .GT. 0.0) T21(60:71,4,IY,IS) =  DOM_CONSUME(4,1:12,IY)
        T21(72,1,IY,IS) = FSUM(T21( 1,1,IY,IS),10)
        T21(72,2,IY,IS) = FSUM(T21( 1,2,IY,IS),10)
        T21(72,3,IY,IS) = FSUM(T21( 1,3,IY,IS),10)
        T21(72,4,IY,IS) = FSUM(T21( 1,4,IY,IS),10)
        T21(73,1,IY,IS) = FSUM(T21(11,1,IY,IS),13)
        T21(73,2,IY,IS) = FSUM(T21(11,2,IY,IS),13)
        T21(73,3,IY,IS) = FSUM(T21(11,3,IY,IS),13)
        T21(73,4,IY,IS) = FSUM(T21(11,4,IY,IS),13)
        T21(74,1,IY,IS) = FSUM(T21(24,1,IY,IS),12)
        T21(74,2,IY,IS) = FSUM(T21(24,2,IY,IS),12)
        T21(74,3,IY,IS) = FSUM(T21(24,3,IY,IS),12)
        T21(74,4,IY,IS) = FSUM(T21(24,4,IY,IS),12)
        T21(75,1,IY,IS) = FSUM(T21(36,1,IY,IS),12)
        T21(75,2,IY,IS) = FSUM(T21(36,2,IY,IS),12)
        T21(75,3,IY,IS) = FSUM(T21(36,3,IY,IS),12)
        T21(75,4,IY,IS) = FSUM(T21(36,4,IY,IS),12)
        T21(76,1,IY,IS) = FSUM(T21(48,1,IY,IS),12)
        T21(76,2,IY,IS) = FSUM(T21(48,2,IY,IS),12)
        T21(76,3,IY,IS) = FSUM(T21(48,3,IY,IS),12)
        T21(76,4,IY,IS) = FSUM(T21(48,4,IY,IS),12)
        T21(77,1,IY,IS) = FSUM(T21(60,1,IY,IS),12)
        T21(77,2,IY,IS) = FSUM(T21(60,2,IY,IS),12)
        T21(77,3,IY,IS) = FSUM(T21(60,3,IY,IS),12)
        T21(77,4,IY,IS) = FSUM(T21(60,4,IY,IS),12)
! reset these to 0; Volume version now has mixed units:
        T21(72,1,IY,IS) = 0.0
        T21(73,1,IY,IS) = 0.0
        T21(74,1,IY,IS) = 0.0
        T21(75,1,IY,IS) = 0.0
        T21(76,1,IY,IS) = 0.0
        T21(77,1,IY,IS) = 0.0
!  totals of supply and disposition
        T21(78,:,IY,IS) = T21(72,:,IY,IS) - T21(73,:,IY,IS)
        T21(79,:,IY,IS) = T21(73,:,IY,IS) - T21(74,:,IY,IS) - T21(75,1,IY,IS) + T21(76,1,IY,IS)
        T21(80,:,IY,IS) = T21(74,:,IY,IS) + T21(75,1,IY,IS) - T21(76,1,IY,IS) - T21(77,:,IY,IS)
      ENDDO

 ! table 22.
 !  Carbon Dioxide Emissions by End-Use (Some T22 calculations and assignments in end-use table sections, t4, t5, t46)
      DO 220 IY=1,LASTYR
        ICY=IY+BASEYR-1
        T22(41,IY,IS) = indcarb(REFCON(1,1,IY),IY)
        T22(42,IY,IS) = indcarb(FOODCON(1,1,IY),IY)
        T22(43,IY,IS) = indcarb(PAPERCON(1,1,IY),IY)
        T22(44,IY,IS) = indcarb(CHEMCON(1,1,IY),IY)
        T22(45,IY,IS) = indcarb(GLASSCON(1,1,IY),IY)
        T22(46,IY,IS) = indcarb(CEMENTCON(1,1,IY),IY)
        T22(47,IY,IS) = indcarb(STEELCON(1,1,IY),IY)
        T22(48,IY,IS) = indcarb(ALUMCON(1,1,IY),IY)
        T22(49,IY,IS) = indcarb(FABMETALCON(1,1,IY),IY)
        T22(50,IY,IS) = indcarb(MACHINECON(1,1,IY),IY)
        T22(51,IY,IS) = indcarb(COMPUTECON(1,1,IY),IY)
        T22(52,IY,IS) = indcarb(TRANEQUIPCON(1,1,IY),IY)
        T22(53,IY,IS) = indcarb(ELECEQUIPON(1,1,IY),IY)
        T22(54,IY,IS) = indcarb(WOODPRODCON(1,1,IY),IY)
        T22(55,IY,IS) = indcarb(PLASTICCON(1,1,IY),IY)
        T22(56,IY,IS) = indcarb(BOMOTHCON(1,1,IY),IY)
        T22(57,IY,IS) = sum(T22(41:56,IY,IS))

        T22(58,IY,IS) = indcarb(AGCON(1,1,IY),IY)
        T22(59,IY,IS) = indcarb(CONSTCON(1,1,IY),IY)
        T22(60,IY,IS) = indcarb(MINECON(1,1,IY),IY)
        T22(61,IY,IS) = sum(T22(58:60,IY,IS))
        T22(62,IY,IS) = sum(em_indy(1:IEL_I,11,ICY))-(T22(57,IY,IS)+T22(61,IY,IS)) ! benchmark discrepancy
        T22(63,IY,IS) = sum(em_indy(1:IEL_I,11,ICY))
! Fill T22 Biogenic CO2 SEE t24 FOR THESE
!        T22(67,IY,IS) = (T24(1,IY,IS)+T24(2,IY,IS)+T24(6,IY,IS)+T24(14,IY,IS))*93.81 ! Biomass                      2/2011 MER imputed
!        T22(68,IY,IS) = T24(13,IY,IS)                                         *90.64 ! Biogenic Waste               2/2011 MER imputed
!        T22(69,IY,IS) = T24(35,IY,IS)                                         *93.81 ! Biofuels Heat and Coproducts MER excluded this.  Assume biomass factor applies
!        T22(70,IY,IS) = sum(T24(8:9,IY,IS)                                    *68.42 ! Ethanol                      2/2011 MER imputed
!        T22(71,IY,IS) = T24(34,IY,IS)                                         *72.73 ! Biodiesel                    2/2011 MER imputed
!        T22(72,IY,IS) = T24(43,IY,IS)                                         *73.15 ! Liquids from Biomass         Assumed NEMS diesel co2 factor applies
!        T22(73,IY,IS) = T24(44,IY,IS)                                         *73.15 ! Green liquids                Assumed NEMS diesel co2 factor applies.
!        T22(74,IY,IS) = sum(T22(67:73,IY,IS))                                        ! Total
! Bonus: put out quads for the biogenic co2 calc
!        T22(75,IY,IS) = (T24(1,IY,IS)+T24(2,IY,IS)+T24(6,IY,IS)+T24(14,IY,IS))       ! Biomass
!        T22(76,IY,IS) = T24(13,IY,IS)                                                ! Biogenic Waste
!        T22(77,IY,IS) = T24(35,IY,IS)                                                ! Biofuels Heat and Coproducts
!        T22(78,IY,IS) = sum(T24(8:9,IY,IS)                                           ! Ethanol
!        T22(79,IY,IS) = T24(34,IY,IS)                                                ! Biodiesel
!        T22(80,IY,IS) = T24(43,IY,IS)                                                ! Liquids from Biomass
!        T22(81,IY,IS) = T24(44,IY,IS)                                                ! Green liquids
!        T22(82,IY,IS) = sum(T22(75:81,IY,IS))                                        ! Total





220   ENDDO
!     Table 24. Renewable Energy, by End-Use Sector and Source

      DO IY = 1,LASTYR
!       --- MARKETED RENEWABLE ENERGY
!       --- RESIDENTIAL
        T24( 1,IY,IS) = QBMRS(11,IY)
!       --- COMMERCIAL
        T24( 2,IY,IS) = QBMCM(11,IY)
!       --- INDUSTRIAL
        T24( 4,IY,IS) = QHOIN(11,IY)
        T24( 5,IY,IS) = QMSIN(11,IY)
        T24( 6,IY,IS) = QBMIN(11,IY) - QBMRF(11,IY)
        T24(35,IY,IS) = &
                   ((CORNCD(3,11,IY) * CFCORN / 1000. -0.9751*(CRNETHCD(11,IY)+OTHETHCD(11,IY)) * 365. * CFPET - &
                     RFBIOBUTECD(MNUMCR,IY)*365.*CFBIOBUTE(IY))/1000000. + &
                     sum(BIMQTYCD(1:4,11,IY))/1000000.*RDAYS*(CFVEGGIE(IY)-CFBIOD(IY)) + &
                 QBMRFBTL(11,IY)/1000. - RDAYS / 1000000. * &
                      (sum(BTLFRAC(1:4,MNUMPR,IY)) * CFBTLLIQ(IY) + &
                       sum(CBTLFRAC(2,1:4,MNUMPR,IY)) * CFCBTLLIQ(2,IY) + &
                       UBAVOL(MNUMPR,IY) * 5.763))
        IF (CONEFF(IY) .NE. 0.0) T24(35,IY,IS) = T24(35,IY,IS) + &
                    (0.9751*CLLETHCD(11,IY) * RDAYS * (CFBMQ(IY) * 42. / CONEFF(IY) - CFPET)) / 1000000.
        T24( 3,IY,IS) = FSUM(T24( 4,IY,IS),3) + T24(35,IY,IS)
!       --- TRANSPORTATION
        IF (MOD(IY+1989,4).EQ.0) RDAYS = 366.
        RDAYS = 365.
        T24( 7,IY,IS) =(0.9751*(CRNETHCD(11,IY)+CLLETHCD(11,IY)+OTHETHCD(11,IY))+ETHIMP(11,IY)-ETHEXP(11,IY))/1000. * RDAYS * .001 * CFPET
        T24( 8,IY,IS) = RFETHE85(MNUMPR,IY) * RDAYS *.001 * CFETQ(IY)
        T24( 8,IY,IS) = QETTR(11,IY) * ETHNE85 * CFETQ(IY) / CFE85Q(IY)
        T24( 9,IY,IS) = T24( 7,IY,IS) - RFETHE85(MNUMPR,IY) * RDAYS * .001 * CFETQ(IY)
        T24( 9,IY,IS) = T24( 7,IY,IS) - &
                        QETTR(11,IY) * ETHNE85 * CFETQ(IY) / CFE85Q(IY)
        T24(43,IY,IS) =  RDAYS / 1000000. * &
                        (sum(BTLFRAC(1:4,MNUMPR,IY)) * CFBTLLIQ(IY) + &
                         sum(CBTLFRAC(2,1:4,MNUMPR,IY)) * CFCBTLLIQ(2,IY) + &
                         UBAVOL(MNUMPR,IY) * 5.763)
        T24(34,IY,IS) =(sum(BIMQTYCD(1:4,11,IY))+BIODIMP(11,IY)-BIODEXP(11,IY))/1000000.*RDAYS*CFBIOD(IY)
        T24(44,IY,IS) =(GRD2DSQTY(MNUMPR,IY) * CFDSQ + GRN2MGQTY(MNUMPR,IY) * CFNPQ + RENEWDIMP(MNUMPR,IY)*CFDSQ) *RDAYS / 1000000.
        T24(45,IY,IS) = QBIOBUTE(MNUMCR,IY) * CFBIOBUTE(IY) * RDAYS / 1000000.
        T24( 7,IY,IS) = T24( 7,IY,IS) + T24(34,IY,IS) + FSUM(T24(43,IY,IS),3)
!       --- ELECTRIC POWER
!       T24(10,IY,IS) = see below
        T24(11,IY,IS) = QHOEL(11,IY)
        T24(12,IY,IS) = QGEEL(11,IY)
        T24(13,IY,IS) = QMSEL(11,IY) - WNCMSEL(IY,11)                ! subtract nonbiogenic from msw
        T24(31,IY,IS)=0.0
        T24(32,IY,IS)=0.0
        T24(14,IY,IS) = QBMEL(11,IY)
        T24(15,IY,IS) = 13500.0 * 0.000001 * &
                        (UGNWDNR(1,mnumnr,IY) + UGNWDNR(2,mnumnr,IY) + &
                         (CGNTGEN(mnumnr,IY, 7,1) + CGNTGEN(mnumnr,IY, 7,2)) * 0.001 - &
                         UGNCFNR(1,mnumnr,IY) - UGNCFNR(2,mnumnr,IY))
        T24(16,IY,IS) =  T24(14,IY,IS) - T24(15,IY,IS)

        T24(17,IY,IS) = QSTEL(11,IY)
        T24(18,IY,IS) = QPVEL(11,IY)
        T24(19,IY,IS) = QWIEL(11,IY)
        T24(10,IY,IS) = FSUM(T24(11,IY,IS),4) + FSUM(T24(17,IY,IS),3)
!       --- TOTAL
        T24(20,IY,IS) = T24( 1,IY,IS) + T24( 2,IY,IS) + T24( 3,IY,IS) + &
                        T24( 7,IY,IS) + T24(10,IY,IS)
!       --- NON-MARKETED RENEWABLE NERGY
!       --- RESIDENTIAL
        T24(21,IY,IS) = (RSH2OCON(IY,5) + RSHTRCON(IY,7) + RSCOOLCN(IY,2)) / 1000000000. + &
                        QPVRS(11,IY) + CGRESQ(11,IY,11)/1000.
        T24(22,IY,IS) = RSH2OCON(IY,5) / 1000000000.
        T24(23,IY,IS) = (RSHTRCON(IY,7) + RSCOOLCN(IY,2)) /  1000000000.
        T24(24,IY,IS) = QPVRS(11,IY)
        T24(41,IY,IS) = CGRESQ(11,IY,11)/1000.
!       --- COMMERCIAL
        T24(25,IY,IS) = QSTCM(11,IY) + QPVCM(11,IY) + CGCOMMQ(11,IY,11)/1000.
        T24(26,IY,IS) = QSTCM(11,IY)
        T24(27,IY,IS) = QPVCM(11,IY)
        T24(42,IY,IS) = CGCOMMQ(11,IY,11)/1000.
!       --- Ethanol Components
        T24(28,IY,IS) =0.9751*(CRNETHCD(11,IY)+OTHETHCD(11,IY)) * RDAYS * CFPET / 1000000.
        T24(29,IY,IS) = 0.9751*CLLETHCD(11,IY) * RDAYS * CFPET / 1000000.
        T24(30,IY,IS) = (ETHIMP(11,IY)-ETHEXP(11,IY)) * RDAYS * CFPET / 1000000.
        T24(33,IY,IS) = FSUM(T24(28,IY,IS),3)
!       --- Sources of biodiesel
        T24(37,IY,IS) =(BIMQTYCD(1,11,IY)+ &
                        BIMQTYCD(4,11,IY))* RDAYS * CFBIOD(IY) / 1000000.
        T24(38,IY,IS) = BIMQTYCD(2,11,IY) * RDAYS * CFBIOD(IY) / 1000000.
        T24(39,IY,IS) = BIMQTYCD(3,11,IY) * RDAYS * CFBIOD(IY) / 1000000.
        T24(40,IY,IS) =(BIODIMP(11,IY)-BIODEXP(11,IY)) * RDAYS * CFBIOD(IY) / 1000000.

! add up electric power biomass and cellulosic ethanol (they compete)
        T24(36,IY,IS) = T24(29,IY,IS) + T24(14,IY,IS)

! Fill T22 Biogenic CO2
        T22(67,IY,IS) = (T24(1,IY,IS)+T24(2,IY,IS)+T24(6,IY,IS)+T24(14,IY,IS))*93.81 ! Biomass                      2/2011 MER imputed
        T22(83,IY,IS) = (T24(14,IY,IS))                                       *93.81 ! Biomass
        T22(84,IY,IS) = (T24(1,IY,IS)+T24(2,IY,IS)+T24(6,IY,IS))              *93.81 ! Biomass
        T22(68,IY,IS) = T24(13,IY,IS)                                         *90.64 ! Biogenic Waste               2/2011 MER imputed
        T22(69,IY,IS) = T24(35,IY,IS)                                         *93.81 ! Biofuels Heat and Coproducts MER excluded this.  Assume biomass factor applies
        T22(70,IY,IS) = sum(T24(8:9,IY,IS))                                   *68.42 ! Ethanol                      2/2011 MER imputed
        T22(87,IY,IS) = T24(45,IY,IS) * 19.25 * 44. / 12.                            ! Biobutanol
        T22(71,IY,IS) = T24(34,IY,IS)                                         *72.73 ! Biodiesel                    2/2011 MER imputed
        T22(72,IY,IS) = T24(43,IY,IS)                                         *73.15 ! Liquids from Biomass         Assumed NEMS diesel co2 factor applies
        T22(73,IY,IS) = T24(44,IY,IS)                                         *73.15 ! Green liquids                Assumed NEMS diesel co2 factor applies.
        T22(74,IY,IS) = sum(T22(67:73,IY,IS))                                        ! Total
! Bonus: put out T22 quads for the biogenic co2 calc
        T22(75,IY,IS) = (T24(1,IY,IS)+T24(2,IY,IS)+T24(6,IY,IS)+T24(14,IY,IS))       ! Biomass
        T22(85,IY,IS) = T24(14,IY,IS)                                                 ! Electric power
        T22(86,IY,IS) = T24(1,IY,IS)+T24(2,IY,IS)+T24(6,IY,IS)                        ! Other
        T22(76,IY,IS) = T24(13,IY,IS)                                                ! Biogenic Waste
        T22(77,IY,IS) = T24(35,IY,IS)                                                ! Biofuels Heat and Coproducts
        T22(78,IY,IS) = sum(T24(8:9,IY,IS))                                          ! Ethanol
        T22(88,IY,IS) = T24(45,IY,IS)                                                  ! Biobutanol
        T22(79,IY,IS) = T24(34,IY,IS)                                                ! Biodiesel
        T22(80,IY,IS) = T24(43,IY,IS)                                                ! Liquids from Biomass
        T22(81,IY,IS) = T24(44,IY,IS)                                                ! Green liquids
        T22(82,IY,IS) = sum(T22(75:81,IY,IS))                                        ! Total
      ENDDO

      DO IY = 1,LASTYR
! available.  18000 converts from thousand cubic feet to million metric tons

        T25( 1:13,IY,IS) = OGCO2AVLs(8,1:13,IY) / 18000.
        T25(14,IY,IS) = sum(OGCO2AVLs(8,1:13,IY)) / 18000.

        T25(64,IY,IS) = T25(14,IY,IS) - T25( 4,IY,IS)
! purchased.  18000 converts from thousand cubic feet to million metric tons
        T25(15:27,IY,IS) = OGCO2PUR(8,1:13,IY) / 18000.
        T25(28,IY,IS) = sum(OGCO2PUR(8,1:13,IY)) / 18000.
! Total Purchased by All Industrial Sources
        T25(90,IY,IS) = T25(15,IY,IS) + T25(16,IY,IS) + T25(17,IY,IS) + T25(19,IY,IS) + T25(20,IY,IS) + T25(23,IY,IS) + T25(26,IY,IS) + T25(27,IY,IS)

        T25(67,IY,IS) = T25(28,IY,IS) - T25(18,IY,IS)
! recycled carbon dioxide
! Total Recycled Industrial Sources
        T25(93,IY,IS) = (OGCO2REC(8,1,IY) + OGCO2REC(8,2,IY) + OGCO2REC(8,3,IY) + OGCO2REC(8,5,IY) + OGCO2REC(8,6,IY) + OGCO2REC(8,9,IY) + OGCO2REC(8,12,IY) + OGCO2REC(8,13,IY)) / 18000.
        IF (T25(90,IY,IS) .GT. 0.0) THEN
           T25(36,IY,IS) = T25(93,IY,IS) * (T25(15,IY,IS) / T25(90,IY,IS))
           T25(37,IY,IS) = T25(93,IY,IS) * (T25(16,IY,IS) / T25(90,IY,IS))
           T25(38,IY,IS) = T25(93,IY,IS) * (T25(17,IY,IS) / T25(90,IY,IS))
           T25(40,IY,IS) = T25(93,IY,IS) * (T25(19,IY,IS) / T25(90,IY,IS))
           T25(41,IY,IS) = T25(93,IY,IS) * (T25(20,IY,IS) / T25(90,IY,IS))
           T25(44,IY,IS) = T25(93,IY,IS) * (T25(23,IY,IS) / T25(90,IY,IS))
           T25(47,IY,IS) = T25(93,IY,IS) * (T25(26,IY,IS) / T25(90,IY,IS))
           T25(48,IY,IS) = T25(93,IY,IS) * (T25(27,IY,IS) / T25(90,IY,IS))
        ELSE
           T25(36,IY,IS) = 0.0
           T25(37,IY,IS) = 0.0
           T25(38,IY,IS) = 0.0
           T25(40,IY,IS) = 0.0
           T25(41,IY,IS) = 0.0
           T25(44,IY,IS) = 0.0
           T25(47,IY,IS) = 0.0
           T25(48,IY,IS) = 0.0
        END IF
        T25(39,IY,IS) = OGCO2REC(8,4,IY) / 18000.
        T25(42,IY,IS) = OGCO2REC(8,7,IY) / 18000.
        T25(43,IY,IS) = OGCO2REC(8,8,IY) / 18000.
        T25(45,IY,IS) = OGCO2REC(8,10,IY) / 18000.
        T25(46,IY,IS) = OGCO2REC(8,11,IY) / 18000.
        T25(49,IY,IS) = sum(OGCO2REC(8,1:13,IY)) / 18000.
        T25(65,IY,IS) = T25(49,IY,IS) - T25(39,IY,IS)
! total carbon dioxide used
! Total Used Industrial Sources
        T25(94,IY,IS) = (OGCO2INJ(8,1,IY) + OGCO2INJ(8,2,IY) + OGCO2INJ(8,3,IY) + OGCO2INJ(8,5,IY) + OGCO2INJ(8,6,IY) + OGCO2INJ(8,9,IY) + OGCO2INJ(8,12,IY) + OGCO2INJ(8,13,IY)) / 18000.
        IF (T25(90,IY,IS) .GT. 0.0) THEN
           T25(50,IY,IS) = T25(94,IY,IS) * (T25(15,IY,IS) / T25(90,IY,IS))
           T25(51,IY,IS) = T25(94,IY,IS) * (T25(16,IY,IS) / T25(90,IY,IS))
           T25(52,IY,IS) = T25(94,IY,IS) * (T25(17,IY,IS) / T25(90,IY,IS))
           T25(54,IY,IS) = T25(94,IY,IS) * (T25(19,IY,IS) / T25(90,IY,IS))
           T25(55,IY,IS) = T25(94,IY,IS) * (T25(20,IY,IS) / T25(90,IY,IS))
           T25(58,IY,IS) = T25(94,IY,IS) * (T25(23,IY,IS) / T25(90,IY,IS))
           T25(61,IY,IS) = T25(94,IY,IS) * (T25(26,IY,IS) / T25(90,IY,IS))
           T25(62,IY,IS) = T25(94,IY,IS) * (T25(27,IY,IS) / T25(90,IY,IS))
        ELSE
           T25(50,IY,IS) = 0.0
           T25(51,IY,IS) = 0.0
           T25(52,IY,IS) = 0.0
           T25(54,IY,IS) = 0.0
           T25(55,IY,IS) = 0.0
           T25(58,IY,IS) = 0.0
           T25(61,IY,IS) = 0.0
           T25(62,IY,IS) = 0.0
        END IF
        T25(53,IY,IS) = OGCO2INJ(8,4,IY) / 18000.
        T25(56,IY,IS) = OGCO2INJ(8,7,IY) / 18000.
        T25(57,IY,IS) = OGCO2INJ(8,8,IY) / 18000.
        T25(59,IY,IS) = OGCO2INJ(8,10,IY) / 18000.
        T25(60,IY,IS) = OGCO2INJ(8,11,IY) / 18000.
        T25(63,IY,IS) = sum(OGCO2INJ(8,1:13,IY)) / 18000.
        T25(66,IY,IS) = T25(63,IY,IS) - T25(53,IY,IS)
! oil produced from carbon dioxide injection
        T25(29,IY,IS) = sum(OGEORPRD(1,1:13,IY)) / 365000.
        T25(30,IY,IS) = sum(OGEORPRD(2,1:13,IY)) / 365000.
        T25(31,IY,IS) = sum(OGEORPRD(3,1:13,IY)) / 365000.
        T25(32,IY,IS) = sum(OGEORPRD(4,1:13,IY)) / 365000.
        T25(33,IY,IS) = sum(OGEORPRD(5,1:13,IY)) / 365000. + sum(OGEORPRD(7,1:13,IY)) / 365000.
        T25(34,IY,IS) = sum(OGEORPRD(6,1:13,IY)) / 365000.
        T25(35,IY,IS) = sum(OGEORPRD(8,1:13,IY)) / 365000.
! oil produced from carbon dioxide injection (by source)
! Total oil produced all Industrial Sources
        T25(95,IY,IS) = (OGEORPRD(8,1,IY) + OGEORPRD(8,2,IY) + OGEORPRD(8,3,IY) + OGEORPRD(8,5,IY) + OGEORPRD(8,6,IY) + OGEORPRD(8,9,IY) + OGEORPRD(8,12,IY) + OGEORPRD(8,13,IY)) / 365000.
        IF (T25(90,IY,IS) .GT. 0.0) THEN
           T25(68,IY,IS) = T25(95,IY,IS) * (T25(15,IY,IS) / T25(90,IY,IS))
           T25(69,IY,IS) = T25(95,IY,IS) * (T25(16,IY,IS) / T25(90,IY,IS))
           T25(70,IY,IS) = T25(95,IY,IS) * (T25(17,IY,IS) / T25(90,IY,IS))
           T25(72,IY,IS) = T25(95,IY,IS) * (T25(19,IY,IS) / T25(90,IY,IS))
           T25(73,IY,IS) = T25(95,IY,IS) * (T25(20,IY,IS) / T25(90,IY,IS))
           T25(76,IY,IS) = T25(95,IY,IS) * (T25(23,IY,IS) / T25(90,IY,IS))
           T25(79,IY,IS) = T25(95,IY,IS) * (T25(26,IY,IS) / T25(90,IY,IS))
           T25(80,IY,IS) = T25(95,IY,IS) * (T25(27,IY,IS) / T25(90,IY,IS))
        ELSE
           T25(68,IY,IS) = 0.0
           T25(69,IY,IS) = 0.0
           T25(70,IY,IS) = 0.0
           T25(72,IY,IS) = 0.0
           T25(73,IY,IS) = 0.0
           T25(76,IY,IS) = 0.0
           T25(79,IY,IS) = 0.0
           T25(80,IY,IS) = 0.0
        END IF
        T25(71,IY,IS) = OGEORPRD(8,4,IY)  / 365000.
        T25(74,IY,IS) = OGEORPRD(8,7,IY)  / 365000.
        T25(75,IY,IS) = OGEORPRD(8,8,IY)  / 365000.
        T25(77,IY,IS) = OGEORPRD(8,10,IY)  / 365000.
        T25(78,IY,IS) = OGEORPRD(8,11,IY)  / 365000.
        T25(81,IY,IS) = sum(OGEORPRD(8,1:13,IY))  / 365000.
        T25(82,IY,IS) = T25(81,IY,IS) - T25(71,IY,IS)
! prices for carbon dioxide
!   weighted by carbon dioxide available (OGCO2AVL)

!       Alternative is to use marginal value from the EFD LP

!       T25(83,IY,IS) = UTCO2PEM(1,IY) * SCALPR
!       T25(84,IY,IS) = UTCO2PEM(2,IY) * SCALPR
!       T25(85,IY,IS) = UTCO2PEM(3,IY) * SCALPR
!       T25(86,IY,IS) = UTCO2PEM(4,IY) * SCALPR
!       T25(87,IY,IS) = UTCO2PEM(5,IY) * SCALPR
!       T25(88,IY,IS) = UTCO2PEM(6,IY) * SCALPR
!       T25(89,IY,IS) = UTCO2PEM(7,IY) * SCALPR

IF (sum(OGCO2AVLs(1:7, 1,IY)) .NE. 0.0) &
        T25(83,IY,IS) =(OGCO2AVLs(1, 1,IY)*OGCO2PRCs(1, 1,IY) + OGCO2AVLs(2, 1,IY)*OGCO2PRCs(2, 1,IY) + &
                        OGCO2AVLs(3, 1,IY)*OGCO2PRCs(3, 1,IY) + OGCO2AVLs(4, 1,IY)*OGCO2PRCs(4, 1,IY) + &
                        OGCO2AVLs(5, 1,IY)*OGCO2PRCs(5, 1,IY) + OGCO2AVLs(6, 1,IY)*OGCO2PRCs(6, 1,IY) + &
                        OGCO2AVLs(7, 1,IY)*OGCO2PRCs(7, 1,IY)) / (OGCO2AVLs(1, 1,IY) + &
                        OGCO2AVLs(2, 1,IY) + OGCO2AVLs(3, 1,IY) + OGCO2AVLs(4, 1,IY) + &
                        OGCO2AVLs(5, 1,IY) + OGCO2AVLs(6, 1,IY) + OGCO2AVLs(7, 1,IY))
IF (sum(OGCO2AVLs(1:7, 2,IY)) .NE. 0.0) &
        T25(84,IY,IS) =(OGCO2AVLs(1, 2,IY)*OGCO2PRCs(1, 2,IY) + OGCO2AVLs(2, 2,IY)*OGCO2PRCs(2, 2,IY) + &
                        OGCO2AVLs(3, 2,IY)*OGCO2PRCs(3, 2,IY) + OGCO2AVLs(4, 2,IY)*OGCO2PRCs(4, 2,IY) + &
                        OGCO2AVLs(5, 2,IY)*OGCO2PRCs(5, 2,IY) + OGCO2AVLs(6, 2,IY)*OGCO2PRCs(6, 2,IY) + &
                        OGCO2AVLs(7, 2,IY)*OGCO2PRCs(7, 2,IY)) / (OGCO2AVLs(1, 2,IY) + &
                        OGCO2AVLs(2, 2,IY) + OGCO2AVLs(3, 2,IY) + OGCO2AVLs(4, 2,IY) + &
                        OGCO2AVLs(5, 2,IY) + OGCO2AVLs(6, 2,IY) + OGCO2AVLs(7, 2,IY))
IF (sum(OGCO2AVLs(1:7, 3,IY)) .NE. 0.0) &
        T25(85,IY,IS) =(OGCO2AVLs(1, 3,IY)*OGCO2PRCs(1, 3,IY) + OGCO2AVLs(2, 3,IY)*OGCO2PRCs(2, 3,IY) + &
                        OGCO2AVLs(3, 3,IY)*OGCO2PRCs(3, 3,IY) + OGCO2AVLs(4, 3,IY)*OGCO2PRCs(4, 3,IY) + &
                        OGCO2AVLs(5, 3,IY)*OGCO2PRCs(5, 3,IY) + OGCO2AVLs(6, 3,IY)*OGCO2PRCs(6, 3,IY) + &
                        OGCO2AVLs(7, 3,IY)*OGCO2PRCs(7, 3,IY)) / (OGCO2AVLs(1, 3,IY) + &
                        OGCO2AVLs(2, 3,IY) + OGCO2AVLs(3, 3,IY) + OGCO2AVLs(4, 3,IY) + &
                        OGCO2AVLs(5, 3,IY) + OGCO2AVLs(6, 3,IY) + OGCO2AVLs(7, 3,IY))
IF (sum(OGCO2AVLs(1:7, 4,IY)) .NE. 0.0) &
        T25(86,IY,IS) =(OGCO2AVLs(1, 4,IY)*OGCO2PRCs(1, 4,IY) + OGCO2AVLs(2, 4,IY)*OGCO2PRCs(2, 4,IY) + &
                        OGCO2AVLs(3, 4,IY)*OGCO2PRCs(3, 4,IY) + OGCO2AVLs(4, 4,IY)*OGCO2PRCs(4, 4,IY) + &
                        OGCO2AVLs(5, 4,IY)*OGCO2PRCs(5, 4,IY) + OGCO2AVLs(6, 4,IY)*OGCO2PRCs(6, 4,IY) + &
                        OGCO2AVLs(7, 4,IY)*OGCO2PRCs(7, 4,IY)) / (OGCO2AVLs(1, 4,IY) + &
                        OGCO2AVLs(2, 4,IY) + OGCO2AVLs(3, 4,IY) + OGCO2AVLs(4, 4,IY) + &
                        OGCO2AVLs(5, 4,IY) + OGCO2AVLs(6, 4,IY) + OGCO2AVLs(7, 4,IY))
IF (sum(OGCO2AVLs(1:7, 5,IY)) .NE. 0.0) &
        T25(87,IY,IS) =(OGCO2AVLs(1, 5,IY)*OGCO2PRCs(1, 5,IY) + OGCO2AVLs(2, 5,IY)*OGCO2PRCs(2, 5,IY) + &
                        OGCO2AVLs(3, 5,IY)*OGCO2PRCs(3, 5,IY) + OGCO2AVLs(4, 5,IY)*OGCO2PRCs(4, 5,IY) + &
                        OGCO2AVLs(5, 5,IY)*OGCO2PRCs(5, 5,IY) + OGCO2AVLs(6, 5,IY)*OGCO2PRCs(6, 5,IY) + &
                        OGCO2AVLs(7, 5,IY)*OGCO2PRCs(7, 5,IY)) / (OGCO2AVLs(1, 5,IY) + &
                        OGCO2AVLs(2, 5,IY) + OGCO2AVLs(3, 5,IY) + OGCO2AVLs(4, 5,IY) + &
                        OGCO2AVLs(5, 5,IY) + OGCO2AVLs(6, 5,IY) + OGCO2AVLs(7, 5,IY))
IF (sum(OGCO2AVLs(1:7, 6,IY)) .NE. 0.0) &
        T25(88,IY,IS) =(OGCO2AVLs(1, 6,IY)*OGCO2PRCs(1, 6,IY) + OGCO2AVLs(2, 6,IY)*OGCO2PRCs(2, 6,IY) + &
                        OGCO2AVLs(3, 6,IY)*OGCO2PRCs(3, 6,IY) + OGCO2AVLs(4, 6,IY)*OGCO2PRCs(4, 6,IY) + &
                        OGCO2AVLs(5, 6,IY)*OGCO2PRCs(5, 6,IY) + OGCO2AVLs(6, 6,IY)*OGCO2PRCs(6, 6,IY) + &
                        OGCO2AVLs(7, 6,IY)*OGCO2PRCs(7, 6,IY)) / (OGCO2AVLs(1, 6,IY) + &
                        OGCO2AVLs(2, 6,IY) + OGCO2AVLs(3, 6,IY) + OGCO2AVLs(4, 6,IY) + &
                        OGCO2AVLs(5, 6,IY) + OGCO2AVLs(6, 6,IY) + OGCO2AVLs(7, 6,IY))
IF (sum(OGCO2AVLs(1:7, 7,IY)) .NE. 0.0) &
        T25(89,IY,IS) =(OGCO2AVLs(1, 7,IY)*OGCO2PRCs(1, 7,IY) + OGCO2AVLs(2, 7,IY)*OGCO2PRCs(2, 7,IY) + &
                        OGCO2AVLs(3, 7,IY)*OGCO2PRCs(3, 7,IY) + OGCO2AVLs(4, 7,IY)*OGCO2PRCs(4, 7,IY) + &
                        OGCO2AVLs(5, 7,IY)*OGCO2PRCs(5, 7,IY) + OGCO2AVLs(6, 7,IY)*OGCO2PRCs(6, 7,IY) + &
                        OGCO2AVLs(7, 7,IY)*OGCO2PRCs(7, 7,IY)) / (OGCO2AVLs(1, 7,IY) + &
                        OGCO2AVLs(2, 7,IY) + OGCO2AVLs(3, 7,IY) + OGCO2AVLs(4, 7,IY) + &
                        OGCO2AVLs(5, 7,IY) + OGCO2AVLs(6, 7,IY) + OGCO2AVLs(7, 7,IY))
IF (sum(OGCO2AVLs(1:7, 9,IY)) .NE. 0.0) &
        T25(91,IY,IS) =(OGCO2AVLs(1, 9,IY)*OGCO2PRCs(1, 9,IY) + OGCO2AVLs(2, 9,IY)*OGCO2PRCs(2, 9,IY) + &
                        OGCO2AVLs(3, 9,IY)*OGCO2PRCs(3, 9,IY) + OGCO2AVLs(4, 9,IY)*OGCO2PRCs(4, 9,IY) + &
                        OGCO2AVLs(5, 9,IY)*OGCO2PRCs(5, 9,IY) + OGCO2AVLs(6, 9,IY)*OGCO2PRCs(6, 9,IY) + &
                        OGCO2AVLs(7, 9,IY)*OGCO2PRCs(7, 9,IY)) / (OGCO2AVLs(1, 9,IY) + &
                        OGCO2AVLs(2, 9,IY) + OGCO2AVLs(3, 9,IY) + OGCO2AVLs(4, 9,IY) + &
                        OGCO2AVLs(5, 9,IY) + OGCO2AVLs(6, 9,IY) + OGCO2AVLs(7, 9,IY))
IF (sum(OGCO2AVLs(1:7,10,IY)) .NE. 0.0) &
        T25(92,IY,IS) =(OGCO2AVLs(1,10,IY)*OGCO2PRCs(1,10,IY) + OGCO2AVLs(2,10,IY)*OGCO2PRCs(2,10,IY) + &
                        OGCO2AVLs(3,10,IY)*OGCO2PRCs(3,10,IY) + OGCO2AVLs(4,10,IY)*OGCO2PRCs(4,10,IY) + &
                        OGCO2AVLs(5,10,IY)*OGCO2PRCs(5,10,IY) + OGCO2AVLs(6,10,IY)*OGCO2PRCs(6,10,IY) + &
                        OGCO2AVLs(7,10,IY)*OGCO2PRCs(7,10,IY)) / (OGCO2AVLs(1,10,IY) + &
                        OGCO2AVLs(2,10,IY) + OGCO2AVLs(3,10,IY) + OGCO2AVLs(4,10,IY) + &
                        OGCO2AVLs(5,10,IY) + OGCO2AVLs(6,10,IY) + OGCO2AVLs(7,10,IY))
IF (sum(OGCO2AVLs(1:7,1:3,IY))+sum(OGCO2AVLs(1:7,5:7,IY))+sum(OGCO2AVLs(1:7,9:10,IY)) .NE. 0.0) &
        T25(96,IY,IS) =(sum(OGCO2AVLs(1:7, 1,IY))*T25(83,IY,IS) + &
                        sum(OGCO2AVLs(1:7, 2,IY))*T25(84,IY,IS) + &
                        sum(OGCO2AVLs(1:7, 3,IY))*T25(85,IY,IS) + &
                        sum(OGCO2AVLs(1:7, 5,IY))*T25(87,IY,IS) + &
                        sum(OGCO2AVLs(1:7, 6,IY))*T25(88,IY,IS) + &
                        sum(OGCO2AVLs(1:7, 7,IY))*T25(89,IY,IS) + &
                        sum(OGCO2AVLs(1:7, 9,IY))*T25(91,IY,IS) + &
                        sum(OGCO2AVLs(1:7,10,IY))*T25(92,IY,IS))/ &
                       (sum(OGCO2AVLs(1:7,1:3,IY))+sum(OGCO2AVLs(1:7,5:7,IY))+sum(OGCO2AVLs(1:7,9:10,IY)))
IF (sum(OGCO2AVLs(1:7,1:7,IY))+sum(OGCO2AVLs(1:7,9:10,IY)) .NE. 0.0) &
        T25(97,IY,IS) =(sum(OGCO2AVLs(1:7, 1,IY))*T25(83,IY,IS) + &
                        sum(OGCO2AVLs(1:7, 2,IY))*T25(84,IY,IS) + &
                        sum(OGCO2AVLs(1:7, 3,IY))*T25(85,IY,IS) + &
                        sum(OGCO2AVLs(1:7, 4,IY))*T25(86,IY,IS) + &
                        sum(OGCO2AVLs(1:7, 5,IY))*T25(87,IY,IS) + &
                        sum(OGCO2AVLs(1:7, 6,IY))*T25(88,IY,IS) + &
                        sum(OGCO2AVLs(1:7, 7,IY))*T25(89,IY,IS) + &
                        sum(OGCO2AVLs(1:7, 9,IY))*T25(91,IY,IS) + &
                        sum(OGCO2AVLs(1:7,10,IY))*T25(92,IY,IS))/ &
                       (sum(OGCO2AVLs(1:7,1:7,IY))+sum(OGCO2AVLs(1:7,9:10,IY)))
!   weighted by carbon dioxide purchased (OGCO2PUR)
IF (sum(OGCO2PUR(1:7, 1,IY)) .NE. 0.0) &
        T25( 98,IY,IS) =(OGCO2PUR(1, 1,IY)*OGCO2PRCs(1, 1,IY) + OGCO2PUR(2, 1,IY)*OGCO2PRCs(2, 1,IY) + &
                         OGCO2PUR(3, 1,IY)*OGCO2PRCs(3, 1,IY) + OGCO2PUR(4, 1,IY)*OGCO2PRCs(4, 1,IY) + &
                         OGCO2PUR(5, 1,IY)*OGCO2PRCs(5, 1,IY) + OGCO2PUR(6, 1,IY)*OGCO2PRCs(6, 1,IY) + &
                         OGCO2PUR(7, 1,IY)*OGCO2PRCs(7, 1,IY)) / (OGCO2PUR(1, 1,IY) + &
                         OGCO2PUR(2, 1,IY) + OGCO2PUR(3, 1,IY) + OGCO2PUR(4, 1,IY) + &
                         OGCO2PUR(5, 1,IY) + OGCO2PUR(6, 1,IY) + OGCO2PUR(7, 1,IY))
IF (sum(OGCO2PUR(1:7, 2,IY)) .NE. 0.0) &
        T25( 99,IY,IS) =(OGCO2PUR(1, 2,IY)*OGCO2PRCs(1, 2,IY) + OGCO2PUR(2, 2,IY)*OGCO2PRCs(2, 2,IY) + &
                         OGCO2PUR(3, 2,IY)*OGCO2PRCs(3, 2,IY) + OGCO2PUR(4, 2,IY)*OGCO2PRCs(4, 2,IY) + &
                         OGCO2PUR(5, 2,IY)*OGCO2PRCs(5, 2,IY) + OGCO2PUR(6, 2,IY)*OGCO2PRCs(6, 2,IY) + &
                         OGCO2PUR(7, 2,IY)*OGCO2PRCs(7, 2,IY)) / (OGCO2PUR(1, 2,IY) + &
                         OGCO2PUR(2, 2,IY) + OGCO2PUR(3, 2,IY) + OGCO2PUR(4, 2,IY) + &
                         OGCO2PUR(5, 2,IY) + OGCO2PUR(6, 2,IY) + OGCO2PUR(7, 2,IY))
IF (sum(OGCO2PUR(1:7, 3,IY)) .NE. 0.0) &
        T25(100,IY,IS) =(OGCO2PUR(1, 3,IY)*OGCO2PRCs(1, 3,IY) + OGCO2PUR(2, 3,IY)*OGCO2PRCs(2, 3,IY) + &
                         OGCO2PUR(3, 3,IY)*OGCO2PRCs(3, 3,IY) + OGCO2PUR(4, 3,IY)*OGCO2PRCs(4, 3,IY) + &
                         OGCO2PUR(5, 3,IY)*OGCO2PRCs(5, 3,IY) + OGCO2PUR(6, 3,IY)*OGCO2PRCs(6, 3,IY) + &
                         OGCO2PUR(7, 3,IY)*OGCO2PRCs(7, 3,IY)) / (OGCO2PUR(1, 3,IY) + &
                         OGCO2PUR(2, 3,IY) + OGCO2PUR(3, 3,IY) + OGCO2PUR(4, 3,IY) + &
                         OGCO2PUR(5, 3,IY) + OGCO2PUR(6, 3,IY) + OGCO2PUR(7, 3,IY))
IF (sum(OGCO2PUR(1:7, 4,IY)) .NE. 0.0) &
        T25(101,IY,IS) =(OGCO2PUR(1, 4,IY)*OGCO2PRCs(1, 4,IY) + OGCO2PUR(2, 4,IY)*OGCO2PRCs(2, 4,IY) + &
                         OGCO2PUR(3, 4,IY)*OGCO2PRCs(3, 4,IY) + OGCO2PUR(4, 4,IY)*OGCO2PRCs(4, 4,IY) + &
                         OGCO2PUR(5, 4,IY)*OGCO2PRCs(5, 4,IY) + OGCO2PUR(6, 4,IY)*OGCO2PRCs(6, 4,IY) + &
                         OGCO2PUR(7, 4,IY)*OGCO2PRCs(7, 4,IY)) / (OGCO2PUR(1, 4,IY) + &
                         OGCO2PUR(2, 4,IY) + OGCO2PUR(3, 4,IY) + OGCO2PUR(4, 4,IY) + &
                         OGCO2PUR(5, 4,IY) + OGCO2PUR(6, 4,IY) + OGCO2PUR(7, 4,IY))
IF (sum(OGCO2PUR(1:7, 5,IY)) .NE. 0.0) &
        T25(102,IY,IS) =(OGCO2PUR(1, 5,IY)*OGCO2PRCs(1, 5,IY) + OGCO2PUR(2, 5,IY)*OGCO2PRCs(2, 5,IY) + &
                         OGCO2PUR(3, 5,IY)*OGCO2PRCs(3, 5,IY) + OGCO2PUR(4, 5,IY)*OGCO2PRCs(4, 5,IY) + &
                         OGCO2PUR(5, 5,IY)*OGCO2PRCs(5, 5,IY) + OGCO2PUR(6, 5,IY)*OGCO2PRCs(6, 5,IY) + &
                         OGCO2PUR(7, 5,IY)*OGCO2PRCs(7, 5,IY)) / (OGCO2PUR(1, 5,IY) + &
                         OGCO2PUR(2, 5,IY) + OGCO2PUR(3, 5,IY) + OGCO2PUR(4, 5,IY) + &
                         OGCO2PUR(5, 5,IY) + OGCO2PUR(6, 5,IY) + OGCO2PUR(7, 5,IY))
IF (sum(OGCO2PUR(1:7, 6,IY)) .NE. 0.0) &
        T25(103,IY,IS) =(OGCO2PUR(1, 6,IY)*OGCO2PRCs(1, 6,IY) + OGCO2PUR(2, 6,IY)*OGCO2PRCs(2, 6,IY) + &
                         OGCO2PUR(3, 6,IY)*OGCO2PRCs(3, 6,IY) + OGCO2PUR(4, 6,IY)*OGCO2PRCs(4, 6,IY) + &
                         OGCO2PUR(5, 6,IY)*OGCO2PRCs(5, 6,IY) + OGCO2PUR(6, 6,IY)*OGCO2PRCs(6, 6,IY) + &
                         OGCO2PUR(7, 6,IY)*OGCO2PRCs(7, 6,IY)) / (OGCO2PUR(1, 6,IY) + &
                         OGCO2PUR(2, 6,IY) + OGCO2PUR(3, 6,IY) + OGCO2PUR(4, 6,IY) + &
                         OGCO2PUR(5, 6,IY) + OGCO2PUR(6, 6,IY) + OGCO2PUR(7, 6,IY))
IF (sum(OGCO2PUR(1:7, 7,IY)) .NE. 0.0) &
        T25(104,IY,IS) =(OGCO2PUR(1, 7,IY)*OGCO2PRCs(1, 7,IY) + OGCO2PUR(2, 7,IY)*OGCO2PRCs(2, 7,IY) + &
                         OGCO2PUR(3, 7,IY)*OGCO2PRCs(3, 7,IY) + OGCO2PUR(4, 7,IY)*OGCO2PRCs(4, 7,IY) + &
                         OGCO2PUR(5, 7,IY)*OGCO2PRCs(5, 7,IY) + OGCO2PUR(6, 7,IY)*OGCO2PRCs(6, 7,IY) + &
                         OGCO2PUR(7, 7,IY)*OGCO2PRCs(7, 7,IY)) / (OGCO2PUR(1, 7,IY) + &
                         OGCO2PUR(2, 7,IY) + OGCO2PUR(3, 7,IY) + OGCO2PUR(4, 7,IY) + &
                         OGCO2PUR(5, 7,IY) + OGCO2PUR(6, 7,IY) + OGCO2PUR(7, 7,IY))
IF (sum(OGCO2PUR(1:7, 9,IY)) .NE. 0.0) &
        T25(106,IY,IS) =(OGCO2PUR(1, 9,IY)*OGCO2PRCs(1, 9,IY) + OGCO2PUR(2, 9,IY)*OGCO2PRCs(2, 9,IY) + &
                         OGCO2PUR(3, 9,IY)*OGCO2PRCs(3, 9,IY) + OGCO2PUR(4, 9,IY)*OGCO2PRCs(4, 9,IY) + &
                         OGCO2PUR(5, 9,IY)*OGCO2PRCs(5, 9,IY) + OGCO2PUR(6, 9,IY)*OGCO2PRCs(6, 9,IY) + &
                         OGCO2PUR(7, 9,IY)*OGCO2PRCs(7, 9,IY)) / (OGCO2PUR(1, 9,IY) + &
                         OGCO2PUR(2, 9,IY) + OGCO2PUR(3, 9,IY) + OGCO2PUR(4, 9,IY) + &
                         OGCO2PUR(5, 9,IY) + OGCO2PUR(6, 9,IY) + OGCO2PUR(7, 9,IY))
IF (sum(OGCO2PUR(1:7,10,IY)) .NE. 0.0) &
        T25(107,IY,IS) =(OGCO2PUR(1,10,IY)*OGCO2PRCs(1,10,IY) + OGCO2PUR(2,10,IY)*OGCO2PRCs(2,10,IY) + &
                         OGCO2PUR(3,10,IY)*OGCO2PRCs(3,10,IY) + OGCO2PUR(4,10,IY)*OGCO2PRCs(4,10,IY) + &
                         OGCO2PUR(5,10,IY)*OGCO2PRCs(5,10,IY) + OGCO2PUR(6,10,IY)*OGCO2PRCs(6,10,IY) + &
                         OGCO2PUR(7,10,IY)*OGCO2PRCs(7,10,IY)) / (OGCO2PUR(1,10,IY) + &
                         OGCO2PUR(2,10,IY) + OGCO2PUR(3,10,IY) + OGCO2PUR(4,10,IY) + &
                         OGCO2PUR(5,10,IY) + OGCO2PUR(6,10,IY) + OGCO2PUR(7,10,IY))
IF (sum(OGCO2PUR(1:7,1:3,IY))+sum(OGCO2PUR(1:7,5:7,IY))+sum(OGCO2PUR(1:7,9:10,IY)) .NE. 0.0) &
        T25(111,IY,IS) =(sum(OGCO2PUR(1:7, 1,IY))*T25( 98,IY,IS) + &
                         sum(OGCO2PUR(1:7, 2,IY))*T25( 99,IY,IS) + &
                         sum(OGCO2PUR(1:7, 3,IY))*T25(100,IY,IS) + &
                         sum(OGCO2PUR(1:7, 5,IY))*T25(102,IY,IS) + &
                         sum(OGCO2PUR(1:7, 6,IY))*T25(103,IY,IS) + &
                         sum(OGCO2PUR(1:7, 7,IY))*T25(104,IY,IS) + &
                         sum(OGCO2PUR(1:7, 9,IY))*T25(106,IY,IS) + &
                         sum(OGCO2PUR(1:7,10,IY))*T25(107,IY,IS))/ &
                        (sum(OGCO2PUR(1:7,1:3,IY))+sum(OGCO2PUR(1:7,5:7,IY))+sum(OGCO2PUR(1:7,9:10,IY)))
IF (sum(OGCO2PUR(1:7,1:7,IY))+sum(OGCO2PUR(1:7,9:10,IY)) .NE. 0.0) &
        T25(112,IY,IS) =(sum(OGCO2PUR(1:7, 1,IY))*T25( 98,IY,IS) + &
                         sum(OGCO2PUR(1:7, 2,IY))*T25( 99,IY,IS) + &
                         sum(OGCO2PUR(1:7, 3,IY))*T25(100,IY,IS) + &
                         sum(OGCO2PUR(1:7, 4,IY))*T25(101,IY,IS) + &
                         sum(OGCO2PUR(1:7, 5,IY))*T25(102,IY,IS) + &
                         sum(OGCO2PUR(1:7, 6,IY))*T25(103,IY,IS) + &
                         sum(OGCO2PUR(1:7, 7,IY))*T25(104,IY,IS) + &
                         sum(OGCO2PUR(1:7, 9,IY))*T25(106,IY,IS) + &
                         sum(OGCO2PUR(1:7,10,IY))*T25(107,IY,IS))/ &
                        (sum(OGCO2PUR(1:7,1:7,IY))+sum(OGCO2PUR(1:7,9:10,IY)))

      ENDDO

! Table 26. Cogenerator and Independent Power Producers Generation

        DO 260, IY = 1,LASTYR
!       --- NON UTILITY Producers (IPP less Nontraditional)
        T26( 1,IY,IS) = UGNCLNR(2,mnumnr,IY)
        T26( 2,IY,IS) = UGNDSNR(2,mnumnr,IY) + UGNRLNR(2,mnumnr,IY) + UGNRHNR(2,mnumnr,IY)
        T26( 3,IY,IS) = UGNGFNR(2,mnumnr,IY) + UGNGINR(2,mnumnr,IY) + UGNGCNR(2,mnumnr,IY)
        T26(58,IY,IS) = UGNURNR(2,mnumnr,IY)
        T26( 4,IY,IS) = UGNHYNR(2,MNUMNR,IY) + UGNGENR(2,MNUMNR,IY) + &
                        UGNMSNR(2,MNUMNR,IY) + UGNWDNR(2,MNUMNR,IY) + &
                        UGNSONR(2,MNUMNR,IY) + UGNPVNR(2,MNUMNR,IY) + &
                        UGNPTNR(2,MNUMNR,IY) + UGNWNNR(2,MNUMNR,IY) + &
                        UGNWLNR(2,MNUMNR,IY) + UGNWFNR(2,MNUMNR,IY) + &
                        UGNPSNR(2,mnumnr,IY) + UGNSDNR(2,mnumnr,IY)
        T26( 5,IY,IS) = FSUM(T26(1,IY,IS),4) + T26(58,IY,IS)
!       --- COGENERATORS
        T26( 6,IY,IS) = (CGINDLGEN(11,IY,1,1) + CGINDLGEN(11,IY,1,2)) * .001
        T26( 7,IY,IS) = (CGINDLGEN(11,IY,2,1) + CGINDLGEN(11,IY,2,2)) * .001
        T26( 8,IY,IS) = (CGINDLGEN(11,IY,3,1) + CGINDLGEN(11,IY,3,2)) * .001
        T26( 9,IY,IS) = (CGINDLGEN(11,IY,6,1) + CGINDLGEN(11,IY,6,2) + &
                         CGINDLGEN(11,IY,7,1) + CGINDLGEN(11,IY,7,2)) * .001
        T26(10,IY,IS) = (CGINDLGEN(11,IY,9,1) + CGINDLGEN(11,IY,9,2) + &
                         CGINDLGEN(11,IY,10,1) + CGINDLGEN(11,IY,10,2)) * .001
        T26(11,IY,IS) = FSUM(T26(6,IY,IS),5)
        T26(48,IY,IS) = (CGINDLGEN(11,IY,4,1) + CGINDLGEN(11,IY,4,2) + &
                         CGINDLGEN(11,IY,5,1) + CGINDLGEN(11,IY,5,2) + &
                         CGINDLGEN(11,IY,11,1) + CGINDLGEN(11,IY,11,2) + &
                         CGINDLGEN(11,IY,8,1) + CGINDLGEN(11,IY,8,2)) * .001
  ! As of AEO03, Nontraditional is no longer included in Industrial
  !  add in the nt stuff, as it currently represents the forecast of the above variables
!                     + (CGNTGEN(mnumnr,IY,4,1) + CGNTGEN(mnumnr,IY,4,2) + &
!                        CGNTGEN(mnumnr,IY,5,1) + CGNTGEN(mnumnr,IY,5,2) + &
!                        CGNTGEN(mnumnr,IY,8,1) + CGNTGEN(mnumnr,IY,8,2)) * .001
        T26(12,IY,IS) = (CGREFGEN(11,IY,9,1) + CGREFGEN(11,IY,9,2)) * .001
        T26(13,IY,IS) = (CGREFGEN(11,IY,2,1) + CGREFGEN(11,IY,2,2)) * .001
        T26(14,IY,IS) = (CGREFGEN(11,IY,3,1) + CGREFGEN(11,IY,3,2)) * .001
        T26(15,IY,IS) = (CGREFGEN(11,IY,6,1) + CGREFGEN(11,IY,6,2) + &
                         CGREFGEN(11,IY,7,1) + CGREFGEN(11,IY,7,2)) * .001
        T26(16,IY,IS) = (CGREFGEN(11,IY,1,1) + CGREFGEN(11,IY,1,2)) * .001
        T26(17,IY,IS) = FSUM(T26(12,IY,IS),5)
        T26(18,IY,IS) = (CGOGSGEN(11,IY,1,1) + CGOGSGEN(11,IY,1,2)) * .001
        T26(19,IY,IS) = (CGOGSGEN(11,IY,2,1) + CGOGSGEN(11,IY,2,2)) * .001
        T26(20,IY,IS) = (CGOGSGEN(11,IY,3,1) + CGOGSGEN(11,IY,3,2)) * .001
        T26(21,IY,IS) = (CGOGSGEN(11,IY,4,1) + CGOGSGEN(11,IY,4,2)) * .001
        T26(22,IY,IS) = (FSUM(T26(18,IY,IS),4))
        T26(23,IY,IS) = (CGCOMMGEN(11,IY,1,1) + CGCOMMGEN(11,IY,1,2) + &
                         CGRESGEN(11,IY,1,1) + CGRESGEN(11,IY,1,2)) * .001
        T26(24,IY,IS) = (CGCOMMGEN(11,IY,2,1) + CGCOMMGEN(11,IY,2,2) + &
                         CGRESGEN(11,IY,2,1) + CGRESGEN(11,IY,2,2)) * .001
        T26(25,IY,IS) = (CGCOMMGEN(11,IY,3,1) + CGCOMMGEN(11,IY,3,2) + &
                         CGRESGEN(11,IY,3,1) + CGRESGEN(11,IY,3,2)) * .001
        T26(26,IY,IS) = (CGCOMMGEN(11,IY,6,1)  + CGCOMMGEN(11,IY,7,1) + &
                         CGCOMMGEN(11,IY,6,2)  + CGCOMMGEN(11,IY,7,2) + &
                         CGRESGEN(11,IY,6,1) + CGRESGEN(11,IY,6,2) + &
                         CGRESGEN(11,IY,7,1) + CGRESGEN(11,IY,7,2)) * .001
        T26(27,IY,IS) = (CGCOMMGEN(11,IY,9,1) + CGCOMMGEN(11,IY,10,1) + &
                         CGCOMMGEN(11,IY,9,2) + CGCOMMGEN(11,IY,10,2) + &
                         CGRESGEN(11,IY,9,1) + CGRESGEN(11,IY,9,2) + &
                         CGRESGEN(11,IY,10,1) + CGRESGEN(11,IY,10,2)) * .001
        T26(28,IY,IS) = FSUM(T26(23,IY,IS),5)
        T26(49,IY,IS) = (CGCOMMGEN(11,IY,4,1) + CGCOMMGEN(11,IY,5,1) + &
                         CGCOMMGEN(11,IY,4,2) + CGCOMMGEN(11,IY,5,2) + &
                         CGCOMMGEN(11,IY,8,1) + CGCOMMGEN(11,IY,11,1) + &
                         CGCOMMGEN(11,IY,8,2) + CGCOMMGEN(11,IY,11,2) + &
                         CGRESGEN(11,IY,4,1) + CGRESGEN(11,IY,4,2) + &
                         CGRESGEN(11,IY,5,1) + CGRESGEN(11,IY,5,2) + &
                         CGRESGEN(11,IY,11,1) + CGRESGEN(11,IY,11,2) + &
                         CGRESGEN(11,IY,8,1) + CGRESGEN(11,IY,8,2)) * .001
        T26(29,IY,IS) = T26(6,IY,IS) + T26(16,IY,IS) + T26(18,IY,IS) + T26(23,IY,IS)
        T26(30,IY,IS) = T26(7,IY,IS) + T26(13,IY,IS) + T26(19,IY,IS) + T26(24,IY,IS)
        T26(31,IY,IS) = T26(12,IY,IS)
        T26(32,IY,IS) = T26(8,IY,IS) + T26(14,IY,IS) + T26(20,IY,IS) + T26(25,IY,IS)
        T26(33,IY,IS) = T26(9,IY,IS) + T26(15,IY,IS) + T26(26,IY,IS)
        T26(34,IY,IS) = T26(10,IY,IS) + T26(21,IY,IS) + T26(27,IY,IS)
        T26(35,IY,IS) = (FSUM(T26(29,IY,IS),6))
        T26(36,IY,IS) = (CGNTGEN(mnumnr,IY,1,1) + CGNTGEN(mnumnr,IY,1,2)) * .001
        T26(37,IY,IS) = (CGNTGEN(mnumnr,IY,2,1) + CGNTGEN(mnumnr,IY,2,2)) * .001
        T26(38,IY,IS) = (CGNTGEN(mnumnr,IY,3,1) + CGNTGEN(mnumnr,IY,3,2)) * .001
        T26(39,IY,IS) = (CGNTGEN(mnumnr,IY,4,1) + CGNTGEN(mnumnr,IY,4,2) +    &
                         CGNTGEN(mnumnr,IY,5,1) + CGNTGEN(mnumnr,IY,5,2) +    &
                         CGNTGEN(mnumnr,IY,6,1) + CGNTGEN(mnumnr,IY,6,2) +    &
                         CGNTGEN(mnumnr,IY,7,1) + CGNTGEN(mnumnr,IY,7,2) +    &
                         CGNTGEN(mnumnr,IY,8,1) + CGNTGEN(mnumnr,IY,8,2)) * .001
        T26(40,IY,IS) = (CGNTGEN(mnumnr,IY,9,1) + CGNTGEN(mnumnr,IY,9,2) + &
                         CGNTGEN(mnumnr,IY,10,1) + CGNTGEN(mnumnr,IY,10,2)) * .001
        T26(41,IY,IS) = (FSUM(T26(36,IY,IS),5))

        T26(42,IY,IS) = T26(36,IY,IS) + T26(29,IY,IS)
        T26(43,IY,IS) = T26(37,IY,IS) + T26(30,IY,IS)
        T26(44,IY,IS) = T26(38,IY,IS) + T26(31,IY,IS) + T26(32,IY,IS)
        T26(45,IY,IS) = T26(39,IY,IS) + T26(33,IY,IS)
        T26(46,IY,IS) = T26(40,IY,IS) + T26(34,IY,IS)
        T26(47,IY,IS) = (FSUM(T26(42,IY,IS),5))
!  Generation for own use
        T26(50,IY,IS) = CGOTGEN(mnumnr,IY,1)
        T26(51,IY,IS) = sum(CGINDLGEN(11,IY,:,2)) / 1000.
        T26(52,IY,IS) = sum(CGREFGEN(11,IY,:,2)) / 1000.
        T26(53,IY,IS) = sum(CGOGSGEN(11,IY,:,2)) / 1000.
        T26(54,IY,IS) = sum(CGRESGEN(11,IY,:,2)) / 1000. + sum(CGCOMMGEN(11,IY,:,2)) / 1000.
        T26(55,IY,IS) = FSUM(T26(50,IY,IS),5)
        T26(56,IY,IS) = CGOTGEN(mnumnr,IY,2)
        T26(57,IY,IS) = FSUM(T26(55,IY,IS),2)
 260    CONTINUE

! Table 27. Cogenerator and Independent Power Producers Capability

        DO 270, IY = 1,LASTYR
!       --- NON UTILITY Producers
!------>NON-UTILITY CAPACITY
         T27( 1,IY,IS) = UCAPCSN(mnumnr,IY)
         T27( 2,IY,IS) = UCAPOSN(mnumnr,IY)
         T27( 3,IY,IS) = UCAPCCN(mnumnr,IY)
         T27( 4,IY,IS) = UCAPCTN(mnumnr,IY)
         T27( 5,IY,IS) = UCAPNUN(mnumnr,IY) + UCAPSMN(mnumnr,IY)
         T27( 6,IY,IS) = UCAPPSN(mnumnr,IY)
         T27( 7,IY,IS) = UCAPFCN(mnumnr,IY)
         T27( 8,IY,IS) = UCAPHYN(mnumnr,IY) + UCAPGEN(mnumnr,IY) + UCAPMSN(mnumnr,IY) + UCAPWDN(mnumnr,IY) + &
                         UCAPSTN(mnumnr,IY) + UCAPPVN(mnumnr,IY) + UCAPWNN(mnumnr,IY) + UCAPPTN(mnumnr,IY) + UCAPWLN(mnumnr,IY)
         T27( 9,IY,IS) = FSUM(T27(1,IY,IS),8)

!       --- COGENERATORS
        T27(10,IY,IS) = CGINDLCAP(11,IY,1) / 1000.
        T27(11,IY,IS) = CGINDLCAP(11,IY,2) / 1000.
        T27(12,IY,IS) = CGINDLCAP(11,IY,3) / 1000.
        T27(13,IY,IS) = (CGINDLCAP(11,IY,6) + CGINDLCAP(11,IY,7)) / 1000.
        T27(14,IY,IS) = (CGINDLCAP(11,IY,9) + CGINDLCAP(11,IY,10)) / 1000.
        T27(15,IY,IS) = FSUM(T27(10,IY,IS),5)
        T27(52,IY,IS) = (CGINDLCAP(11,IY,4) + CGINDLCAP(11,IY,5) + &
                         CGINDLCAP(11,IY,8) + CGINDLCAP(11,IY,11)) / 1000.
    ! As if AEO03, Nontraditional is no longer included in Industrial
    ! add in the nt stuff, as it represents the forecast of the above variables
!                     + (CGNTCAP(16,IY,4) + CGNTCAP(16,IY,5) + CGNTCAP(16,IY,8)) * .001
        T27(16,IY,IS) =  CGREFCAP(11,IY,9) * .001
        T27(17,IY,IS) =  CGREFCAP(11,IY,2) * .001
        T27(18,IY,IS) =  CGREFCAP(11,IY,3) * .001
        T27(19,IY,IS) = (CGREFCAP(11,IY,6) + CGREFCAP(11,IY,7)) * .001
        T27(20,IY,IS) =  CGREFCAP(11,IY,1) * .001
        T27(21,IY,IS) = FSUM(T27(16,IY,IS),5)
        T27(22,IY,IS) = CGOGSCAP(11,IY,1) / 1000.
        T27(23,IY,IS) = CGOGSCAP(11,IY,2) / 1000.
        T27(24,IY,IS) = CGOGSCAP(11,IY,3) / 1000.
        T27(25,IY,IS) = CGOGSCAP(11,IY,4) / 1000.
        T27(26,IY,IS) = (FSUM(T27(22,IY,IS),4))
        T27(27,IY,IS) = (CGCOMMCAP(11,IY,1) + CGRESCAP(11,IY,1)) * .001
        T27(28,IY,IS) = (CGCOMMCAP(11,IY,2) + CGRESCAP(11,IY,2)) * .001
        T27(29,IY,IS) = (CGCOMMCAP(11,IY,3) + CGRESCAP(11,IY,3)) * .001
        T27(30,IY,IS) = (CGCOMMCAP(11,IY,6) + CGCOMMCAP(11,IY,7) + &
                         CGRESCAP(11,IY,6) + CGRESCAP(11,IY,7)) * .001
        T27(31,IY,IS) = (CGCOMMCAP(11,IY,9) + CGCOMMCAP(11,IY,10) + &
                         CGRESCAP(11,IY,9) + CGRESCAP(11,IY,10)) * .001
        T27(32,IY,IS) = FSUM(T27(27,IY,IS),5)
        T27(53,IY,IS) = (CGCOMMCAP(11,IY,4) + CGCOMMCAP(11,IY,5) + &
                         CGCOMMCAP(11,IY,8) + CGCOMMCAP(11,IY,11) + &
                         CGRESCAP(11,IY,4) + CGRESCAP(11,IY,5) + &
                         CGRESCAP(11,IY,8) + CGRESCAP(11,IY,11)) * .001
        T27(33,IY,IS) = T27(10,IY,IS) + T27(20,IY,IS) + T27(22,IY,IS) + T27(27,IY,IS)
        T27(34,IY,IS) = T27(11,IY,IS) + T27(17,IY,IS) + T27(23,IY,IS) + T27(28,IY,IS)
        T27(35,IY,IS) = T27(16,IY,IS)
        T27(36,IY,IS) = T27(12,IY,IS) + T27(18,IY,IS) + T27(24,IY,IS) + T27(29,IY,IS)
        T27(37,IY,IS) = T27(13,IY,IS) + T27(19,IY,IS) + T27(30,IY,IS)
        T27(38,IY,IS) = T27(14,IY,IS) + T27(25,IY,IS) + T27(31,IY,IS)
        T27(39,IY,IS) = (FSUM(T27(33,IY,IS),6))
        T27(40,IY,IS) = (CGNTCAP(mnumnr,IY,1)) * .001
        T27(41,IY,IS) = (CGNTCAP(mnumnr,IY,2)) * .001
        T27(42,IY,IS) = (CGNTCAP(mnumnr,IY,3)) * .001
        T27(43,IY,IS) = (CGNTCAP(mnumnr,IY,4) + CGNTCAP(mnumnr,IY,5) + &
                         CGNTCAP(mnumnr,IY,6) + CGNTCAP(mnumnr,IY,7) + &
                         CGNTCAP(mnumnr,IY,8)) * .001
        T27(44,IY,IS) = (CGNTCAP(mnumnr,IY,9) + CGNTCAP(mnumnr,IY,10)) * .001
        T27(45,IY,IS) = (FSUM(T27(40,IY,IS),5))
        T27(46,IY,IS) = (CGNTCAP(mnumnr,IY,1) * .001) + T27(33,IY,IS)
        T27(47,IY,IS) = CGNTCAP(mnumnr,IY,2) * .001 + T27(34,IY,IS) + T27(35,IY,IS)
        T27(48,IY,IS) = CGNTCAP(mnumnr,IY,3) * .001 + T27(36,IY,IS)
        T27(49,IY,IS) = ((CGNTCAP(mnumnr,IY,4) + CGNTCAP(mnumnr,IY,5) + &
                          CGNTCAP(mnumnr,IY,6) + CGNTCAP(mnumnr,IY,7) + &
                          CGNTCAP(mnumnr,IY,8)) * .001) + T27(37,IY,IS)
        T27(50,IY,IS) = ((CGNTCAP(mnumnr,IY,9) + CGNTCAP(mnumnr,IY,10)) * .001) + T27(38,IY,IS)
        T27(51,IY,IS) = (FSUM(T27(46,IY,IS),5))
 270    CONTINUE

! Table 28. Cogenerator and Independent Power Producers Fuel Consumption

        DO 280, IY = 1,LASTYR
!       --- NON UTILITY Fuel Consumption
        T28( 1,IY,IS) = UFLCLNR(2,mnumnr,IY)
        T28( 2,IY,IS) = UFLDSNR(2,mnumnr,IY) + UFLRLNR(2,mnumnr,IY)
        T28( 3,IY,IS) = UFLGFNR(2,mnumnr,IY) + UFLGINR(2,mnumnr,IY) + UFLGCNR(2,mnumnr,IY)
        T28( 4,IY,IS) = UFLURNR(2,mnumnr,IY)
        T28( 5,IY,IS) =(UGNHYNR(2,mnumnr,IY) * WHRHYEL(mnumnr,IY) + &
                        UGNMSNR(2,mnumnr,IY) * WHRMSEL(mnumnr,IY) + &
                        UGNSONR(2,mnumnr,IY) * WHRSTEL(mnumnr,IY) + &
                        UGNPVNR(2,mnumnr,IY) * WHRPVEL(mnumnr,IY) + &
                        UGNPTNR(2,mnumnr,IY) * WHRPTEL(mnumnr,IY) + &
                        UGNWNNR(2,mnumnr,IY) * WHRWIEL(mnumnr,IY) + &
                        UGNWLNR(2,mnumnr,IY) * WHRWLEL(mnumnr,IY))/1000000. + &
                        UFLGTNR(2,mnumnr,IY) + &
                        UFLWDNR(2,mnumnr,IY)
        T28( 6,IY,IS) = UFLOTNR(2,mnumnr,IY)
        T28(12,IY,IS) = FSUM(T28(1,IY,IS),6)
!       --- COGENERATORS
!       Industrial
        T28(13,IY,IS) = CGINDLQ(11,IY,1)
        T28(14,IY,IS) = CGINDLQ(11,IY,2)
        T28(15,IY,IS) = CGINDLQ(11,IY,3)
        T28(16,IY,IS) = CGINDLQ(11,IY,6) + CGINDLQ(11,IY,7)
        T28(17,IY,IS) = CGINDLQ(11,IY,9) + CGINDLQ(11,IY,10)
        T28(18,IY,IS) = FSUM(T28(13,IY,IS),5)
        T28(55,IY,IS) = CGINDLQ(11,IY,4) + CGINDLQ(11,IY,5) + &
                        CGINDLQ(11,IY,8) + CGINDLQ(11,IY,11)
    ! As if AEO03, Nontraditional is no longer included in Industrial
    ! add in the nt stuff, as it represents the forecast of the above variables
!                     + (CGNTQ(mnumnr,IY,4) + CGNTQ(mnumnr,IY,5) + CGNTQ(mnumnr,IY,8))
! Refining
        T28(19,IY,IS) = CGREFQ(11,IY,9)
        T28(20,IY,IS) = CGREFQ(11,IY,2)
        T28(21,IY,IS) = CGREFQ(11,IY,3)
        T28(22,IY,IS) = CGREFQ(11,IY,6) + CGREFQ(11,IY,7)
        T28(23,IY,IS) = CGREFQ(11,IY,1)
        T28(24,IY,IS) = FSUM(T28(19,IY,IS),5)
! EOR Consumption
        T28(25,IY,IS) = CGOGSQ(11,IY,1)
        T28(26,IY,IS) = CGOGSQ(11,IY,2)
        T28(27,IY,IS) = CGOGSQ(11,IY,3)
        T28(28,IY,IS) = CGOGSQ(11,IY,4)
        T28(29,IY,IS) = FSUM(T28(25,IY,IS),4)
! Commercial Consumption
        T28(30,IY,IS) = (CGCOMMQ(11,IY,1) + CGRESQ(11,IY,1))
        T28(31,IY,IS) = (CGCOMMQ(11,IY,2) + CGRESQ(11,IY,2))
        T28(32,IY,IS) = (CGCOMMQ(11,IY,3) + CGRESQ(11,IY,3))
        T28(33,IY,IS) = (CGCOMMQ(11,IY,6) + CGCOMMQ(11,IY,7) + &
                         CGRESQ(11,IY,6) + CGRESQ(11,IY,7))
        T28(34,IY,IS) = (CGCOMMQ(11,IY,9) + CGCOMMQ(11,IY,10) + &
                         CGRESQ(11,IY,9) + CGRESQ(11,IY,10))
        T28(35,IY,IS) = FSUM(T28(30,IY,IS),5)
        T28(56,IY,IS) = (CGCOMMQ(11,IY,4) + CGCOMMQ(11,IY,5) + CGCOMMQ(11,IY,8) + CGCOMMQ(11,IY,11) + &
                         CGRESQ(11,IY,4) + CGRESQ(11,IY,5) + CGRESQ(11,IY,8) + CGRESQ(11,IY,11))
! Traditional Cogenerators
        T28(36,IY,IS) = T28(13,IY,IS) + T28(23,IY,IS) + T28(25,IY,IS) + T28(30,IY,IS)
        T28(37,IY,IS) = T28(14,IY,IS) + T28(20,IY,IS) + T28(26,IY,IS) + T28(31,IY,IS)
        T28(38,IY,IS) = T28(19,IY,IS)
        T28(39,IY,IS) = T28(15,IY,IS) + T28(21,IY,IS) + T28(27,IY,IS) + T28(32,IY,IS)
        T28(40,IY,IS) = T28(16,IY,IS) + T28(22,IY,IS) + T28(33,IY,IS)
        T28(41,IY,IS) = T28(17,IY,IS) + T28(28,IY,IS) + T28(34,IY,IS)
        T28(42,IY,IS) = (FSUM(T28(36,IY,IS),6))
! Non-traditional Cogenerators
        T28(43,IY,IS) = (CGNTQ(mnumnr,IY,1))
        T28(44,IY,IS) = (CGNTQ(mnumnr,IY,2))
        T28(45,IY,IS) = (CGNTQ(mnumnr,IY,3))
        T28(46,IY,IS) = (CGNTQ(mnumnr,IY,4) + CGNTQ(mnumnr,IY,5) + CGNTQ(mnumnr,IY,6) + &
                         CGNTQ(mnumnr,IY,7) + CGNTQ(mnumnr,IY,8))
        T28(47,IY,IS) = (CGNTQ(mnumnr,IY,9) + CGNTQ(mnumnr,IY,10))
        T28(48,IY,IS) = (FSUM(T28(43,IY,IS),5))
! Total Cogenerators
        T28(49,IY,IS) = CGNTQ(mnumnr,IY,1) + T28(36,IY,IS)
        T28(50,IY,IS) = (CGNTQ(mnumnr,IY,2)) + T28(37,IY,IS) + T28(38,IY,IS)
        T28(51,IY,IS) = (CGNTQ(mnumnr,IY,3)) + T28(39,IY,IS)
        T28(52,IY,IS) = (CGNTQ(mnumnr,IY,4) + CGNTQ(mnumnr,IY,5) + CGNTQ(mnumnr,IY,6) + &
                         CGNTQ(mnumnr,IY,7) + CGNTQ(mnumnr,IY,8)) + T28(40,IY,IS)
        T28(53,IY,IS) = (CGNTQ(mnumnr,IY,9) + CGNTQ(mnumnr,IY,10)) + T28(41,IY,IS)
        T28(54,IY,IS) = (FSUM(T28(49,IY,IS),5))
 280    CONTINUE

!     Table 29.  Refinery Process Unit Capacity

         DO IY = 1,LASTYR
           T29(1:48,IY,IS)  = REF_CAP(1:48,MNUMPR,IY)
           T29(49:96,IY,IS) = REF_UTL(1:48,MNUMPR,IY)
           T29(97:99,IY,IS) = REF_CAP(49:51,MNUMPR,IY)
           T29(100:102,IY,IS) = REF_UTL(49:51,MNUMPR,IY)
           T29(103:104,IY,IS) = REF_CAP(52:53,MNUMPR,IY)
           T29(105:106,IY,IS) = REF_UTL(52:53,MNUMPR,IY)
           T29(107,IY,IS) = REF_CAP(1,MNUMPR,IY) + REF_CAP(53,MNUMPR,IY)
           T29(108,IY,IS) =(REF_CAP( 1,MNUMPR,IY) * REF_UTL( 1,MNUMPR,IY) +   &
                            REF_CAP(53,MNUMPR,IY) * REF_UTL(53,MNUMPR,IY)) /  &
                           (REF_CAP( 1,MNUMPR,IY) + REF_CAP(53,MNUMPR,IY))
         ENDDO

!     Table 30. NEMS Residential Supplement Table

        DO 300, IY = RECSYEAR-BASEYR+1,LASTYR
!         --- EQUIPMENT STOCK
!         --- MAIN SPACE HEATERS
          T30( 1,IY,IS) = RSHTRS(IY,1) * .000001
          T30( 2,IY,IS) = RSHTRS(IY,2) * .000001
          T30( 3,IY,IS) = RSHTRS(IY,3) * .000001
          T30( 4,IY,IS) = RSHTRS(IY,4) * .000001
          T30( 5,IY,IS) = RSHTRS(IY,5) * .000001
          T30( 6,IY,IS) = RSHTRS(IY,6) * .000001
          T30( 7,IY,IS) = RSHTRS(IY,7) * .000001
          T30( 8,IY,IS) = RSHTRS(IY,8) * .000001
          T30( 9,IY,IS) = RSHTRS(IY,9) * .000001
          T30(10,IY,IS) = FSUM(T30(1,IY,IS),9)
!         --- SPACE COOLING
          T30(11,IY,IS) = RSCOOLERS(IY,1) * .000001
          T30(12,IY,IS) = RSCOOLERS(IY,2) * .000001
          T30(13,IY,IS) = RSCOOLERS(IY,3) * .000001
          T30(14,IY,IS) = RSCOOLERS(IY,4) * .000001
          T30(15,IY,IS) = RSCOOLERS(IY,5) * .000001
          T30(16,IY,IS) = FSUM(T30(11,IY,IS),5)
!         --- WATER HEATERS
          T30(17,IY,IS) = RSWATER(IY,2) * .000001
          T30(18,IY,IS) = RSWATER(IY,1) * .000001
          T30(19,IY,IS) = RSWATER(IY,3) * .000001
          T30(20,IY,IS) = RSWATER(IY,4) * .000001
          T30(21,IY,IS) = RSWATER(IY,5) * .000001
          T30(22,IY,IS) = FSUM(T30(17,IY,IS),5)
!         --- COOKING EQUIPMENT
          T30(23,IY,IS) = RSCOOK(IY,3) * .000001
          T30(24,IY,IS) = RSCOOK(IY,1) * .000001
          T30(25,IY,IS) = RSCOOK(IY,2) * .000001
          T30(26,IY,IS) = FSUM(T30(23,IY,IS),3)
!         --- DRYERS
          T30(27,IY,IS) = RSDRY(IY,2) * .000001
          T30(28,IY,IS) = RSDRY(IY,1) * .000001
          T30(29,IY,IS) = FSUM(T30(27,IY,IS),2)
!         --- OTHER APPLIANCES
          T30(30,IY,IS) = RSREF(IY) * .000001
          T30(31,IY,IS) = RSFRZ(IY) * .000001
!         --- STOCK EFFICIENCIES
!         --- SPACE HEATERS
          T30(32,IY,IS) = RSEEFHT(IY,1) * 3.412
          T30(33,IY,IS) = RSEEFHT(IY,2)
          T30(34,IY,IS) = RSEEFHT(IY,3)
          T30(35,IY,IS) = RSEEFHT(IY,4)
          T30(36,IY,IS) = RSEEFHT(IY,5)
!         --- SPACE COOLING
          T30(37,IY,IS) = RSEEFCL(IY,1) * 3.412
          T30(38,IY,IS) = RSEEFCL(IY,2)
          T30(39,IY,IS) = RSEEFCL(IY,3)
          T30(40,IY,IS) = RSEEFCL(IY,4) * 3.412
          T30(41,IY,IS) = RSEEFCL(IY,5) * 3.412
!         --- WATER HEATING
          T30(42,IY,IS) = RSEEFHW(IY,1)
          T30(43,IY,IS) = RSEEFHW(IY,2)
          T30(44,IY,IS) = RSEEFHW(IY,3)
          T30(45,IY,IS) = RSEEFHW(IY,4)
!         --- OTHER APPLIANCES
          T30(46,IY,IS) = RSEEFRF(IY)
          T30(47,IY,IS) = RSEEFFZ(IY)
!         --- BUILDING SHELL EFFICIENCY INDEX
!         --- SPACE HEATING
          T30(48,IY,IS) = HSHELL1(IY)
          T30(49,IY,IS) = HSHELL2(IY)
          T30(50,IY,IS) = HSHELL3(IY)
!         --- SPACE COOLING
          T30(51,IY,IS) = CSHELL1(IY)
          T30(52,IY,IS) = CSHELL2(IY)
          T30(53,IY,IS) = CSHELL3(IY)
!    Capacity
          T30(54,IY,IS) = CGRESCAP(11,IY, 3) / 1000.
          T30(55,IY,IS) = CGRESCAP(11,IY, 8) / 1000.
          T30(56,IY,IS) = CGRESCAP(11,IY,11) / 1000.
          T30(57,IY,IS) = FSUM(T30(54,IY,IS),3)
!    Generation
          T30(58,IY,IS) =(CGRESGEN(11,IY, 3,1) + CGRESGEN(11,IY, 3,2)) / 1000.
          T30(59,IY,IS) =(CGRESGEN(11,IY, 8,1) + CGRESGEN(11,IY, 8,2)) / 1000.
          T30(60,IY,IS) =(CGRESGEN(11,IY,11,1) + CGRESGEN(11,IY,11,2)) / 1000.
          T30(61,IY,IS) = FSUM(T30(58,IY,IS),3)
          T30(62,IY,IS) =(CGRESGEN(11,IY, 3,1) + CGRESGEN(11,IY, 8,1) + CGRESGEN(11,IY,11,1)) / 1000.
          T30(63,IY,IS) =(CGRESGEN(11,IY, 3,2) + CGRESGEN(11,IY, 8,2) + CGRESGEN(11,IY,11,2)) / 1000.
!    Energy Input
          T30(64,IY,IS) = CGRESQ(11,IY, 3)
          T30(65,IY,IS) = CGRESQ(11,IY, 8)
          T30(66,IY,IS) = CGRESQ(11,IY,11)
          T30(67,IY,IS) = FSUM(T30(64,IY,IS),3)
 300    CONTINUE

!     Table 32. Commercial Sector Supplemntary Table

         DO 320, IY = 1,LASTYR
!          --- BUILDING ENERGY CONSUMPTION
           T32( 1,IY,IS) = CMFINALENDUSECON( 1,IY)
           T32( 2,IY,IS) = CMFINALENDUSECON( 2,IY)
           T32( 3,IY,IS) = CMFINALENDUSECON( 3,IY)
           T32( 4,IY,IS) = CMFINALENDUSECON( 4,IY)
           T32( 5,IY,IS) = CMFINALENDUSECON( 5,IY)
           T32( 6,IY,IS) = CMFINALENDUSECON( 6,IY)
           T32( 7,IY,IS) = CMFINALENDUSECON( 7,IY)
           T32( 8,IY,IS) = CMFINALENDUSECON( 8,IY)
           T32( 9,IY,IS) = CMFINALENDUSECON( 9,IY)
           T32(10,IY,IS) = CMFINALENDUSECON(10,IY)
           T32(11,IY,IS) = CMFINALENDUSECON(11,IY)
           T32(12,IY,IS) = FSUM(T32(1,IY,IS),11)
!          --- COMMERCIAL BUILDING FLOORSPACE
           T32(13,IY,IS) = CMSURVFLOORTOT(1,IY) + CMNEWFLRSPACE(1,IY)
           T32(14,IY,IS) = CMSURVFLOORTOT(2,IY) + CMNEWFLRSPACE(2,IY)
           T32(15,IY,IS) = CMSURVFLOORTOT(3,IY) + CMNEWFLRSPACE(3,IY)
           T32(16,IY,IS) = CMSURVFLOORTOT(4,IY) + CMNEWFLRSPACE(4,IY)
           T32(17,IY,IS) = CMSURVFLOORTOT(5,IY) + CMNEWFLRSPACE(5,IY)
           T32(18,IY,IS) = CMSURVFLOORTOT(6,IY) + CMNEWFLRSPACE(6,IY)
           T32(19,IY,IS) = CMSURVFLOORTOT(7,IY) + CMNEWFLRSPACE(7,IY)
           T32(20,IY,IS) = CMSURVFLOORTOT(8,IY) + CMNEWFLRSPACE(8,IY)
           T32(21,IY,IS) = CMSURVFLOORTOT(9,IY) + CMNEWFLRSPACE(9,IY)
           T32(22,IY,IS) = CMSURVFLOORTOT(10,IY) + CMNEWFLRSPACE(10,IY)
           T32(23,IY,IS) = CMSURVFLOORTOT(11,IY) + CMNEWFLRSPACE(11,IY)
           T32(24,IY,IS) = FSUM(T32(13,IY,IS),11)
!          --- HEATING
           T32(25,IY,IS) = CMUSAVGEFF(1,1,IY)
           T32(26,IY,IS) = CMUSAVGEFF(1,2,IY)
           T32(27,IY,IS) = CMUSAVGEFF(1,3,IY)
!          --- COOLING
           T32(28,IY,IS) = CMUSAVGEFF(2,1,IY)
           T32(29,IY,IS) = CMUSAVGEFF(2,2,IY)
!          --- WATER HEATING
           T32(30,IY,IS) = CMUSAVGEFF(3,1,IY)
           T32(31,IY,IS) = CMUSAVGEFF(3,2,IY)
           T32(32,IY,IS) = CMUSAVGEFF(3,3,IY)
!          --- VENTILATION
           T32(33,IY,IS) = CMUSAVGEFF(4,1,IY)
!          --- COOKING
           T32(34,IY,IS) = CMUSAVGEFF(5,1,IY)
           T32(35,IY,IS) = CMUSAVGEFF(5,2,IY)
!          --- LIGHTING
           T32(36,IY,IS) = CMUSAVGEFF(6,1,IY)
!          --- REFRIGERATION
           T32(37,IY,IS) = CMUSAVGEFF(7,1,IY)
!          --- OFFICE EQUIPEMENT (PC)
           T32(38,IY,IS) = CMUSAVGEFF(8,1,IY)
!          --- OFFICE EQUIPEMANT (NON-PC)
           T32(39,IY,IS) = CMUSAVGEFF(9,1,IY)
!    Capacity
          T32(40,IY,IS) = CGCOMMCAP(11,IY,2) / 1000.
          T32(41,IY,IS) = CGCOMMCAP(11,IY,3) / 1000.
          T32(42,IY,IS) = CGCOMMCAP(11,IY,8) / 1000.
          T32(43,IY,IS) = CGCOMMCAP(11,IY,11) / 1000.
          T32(44,IY,IS) =(CGCOMMCAP(11,IY,1) + CGCOMMCAP(11,IY,4) + &
                          CGCOMMCAP(11,IY,5) + CGCOMMCAP(11,IY,6) + &
                          CGCOMMCAP(11,IY,7) + CGCOMMCAP(11,IY,9) + &
                          CGCOMMCAP(11,IY,10) + CGCOMMCAP(11,IY,12)) / 1000.
          T32(45,IY,IS) = FSUM(T32(40,IY,IS),5)
!    Generation
          T32(46,IY,IS) =(CGCOMMGEN(11,IY,2,1) + CGCOMMGEN(11,IY,2,2)) / 1000.
          T32(47,IY,IS) =(CGCOMMGEN(11,IY,3,1) + CGCOMMGEN(11,IY,3,2)) / 1000.
          T32(48,IY,IS) =(CGCOMMGEN(11,IY,8,1) + CGCOMMGEN(11,IY,8,2)) / 1000.
          T32(49,IY,IS) =(CGCOMMGEN(11,IY,11,1)+ CGCOMMGEN(11,IY,11,2)) / 1000.
          T32(50,IY,IS) =(CGCOMMGEN(11,IY,1,1) + CGCOMMGEN(11,IY,4,1) + &
                          CGCOMMGEN(11,IY,5,1) + CGCOMMGEN(11,IY,6,1) + &
                          CGCOMMGEN(11,IY,7,1) + CGCOMMGEN(11,IY,9,1) + &
                          CGCOMMGEN(11,IY,10,1)+ CGCOMMGEN(11,IY,12,1)+ &
                          CGCOMMGEN(11,IY,1,2) + CGCOMMGEN(11,IY,4,2) + &
                          CGCOMMGEN(11,IY,5,2) + CGCOMMGEN(11,IY,6,2) + &
                          CGCOMMGEN(11,IY,7,2) + CGCOMMGEN(11,IY,9,2) + &
                          CGCOMMGEN(11,IY,10,2)+ CGCOMMGEN(11,IY,12,2)) / 1000.
          T32(51,IY,IS) = FSUM(T32(46,IY,IS),5)
          T32(52,IY,IS) = &
                  (CGCOMMGEN(11,IY, 1,1) + CGCOMMGEN(11,IY, 2,1) + &
                   CGCOMMGEN(11,IY, 3,1) + CGCOMMGEN(11,IY, 4,1) + &
                   CGCOMMGEN(11,IY, 5,1) + CGCOMMGEN(11,IY, 6,1) + &
                   CGCOMMGEN(11,IY, 7,1) + CGCOMMGEN(11,IY, 8,1) + &
                   CGCOMMGEN(11,IY, 9,1) + CGCOMMGEN(11,IY,10,1) + &
                   CGCOMMGEN(11,IY,11,1) + CGCOMMGEN(11,IY,12,1)) / 1000.
          T32(53,IY,IS) = &
                  (CGCOMMGEN(11,IY, 1,2) + CGCOMMGEN(11,IY, 2,2) + &
                   CGCOMMGEN(11,IY, 3,2) + CGCOMMGEN(11,IY, 4,2) + &
                   CGCOMMGEN(11,IY, 5,2) + CGCOMMGEN(11,IY, 6,2) + &
                   CGCOMMGEN(11,IY, 7,2) + CGCOMMGEN(11,IY, 8,2) + &
                   CGCOMMGEN(11,IY, 9,2) + CGCOMMGEN(11,IY,10,2) + &
                   CGCOMMGEN(11,IY,11,2) + CGCOMMGEN(11,IY,12,2)) / 1000.
!    Energy Input
          T32(54,IY,IS) = CGCOMMQ(11,IY,2)
          T32(55,IY,IS) = CGCOMMQ(11,IY,3)
          T32(56,IY,IS) = CGCOMMQ(11,IY,8)
          T32(57,IY,IS) = CGCOMMQ(11,IY,11)
          T32(58,IY,IS) = CGCOMMQ(11,IY,1)  + CGCOMMQ(11,IY,4) + &
                          CGCOMMQ(11,IY,5)  + CGCOMMQ(11,IY,6) + &
                          CGCOMMQ(11,IY,7)  + CGCOMMQ(11,IY,9) + &
                          CGCOMMQ(11,IY,10) + CGCOMMQ(11,IY,12)
          T32(59,IY,IS) = FSUM(T32(54,IY,IS),5)
  320    CONTINUE

!     Table 33. Other Commercial Sector Consumption

         DO IY = 1,LASTYR
!          --- MISC. END-USE SERVICES
           T33( 1,IY,IS) =  CMUSCONSUMPTION(10,1,IY)
           T33( 2,IY,IS) =  CMUSCONSUMPTION(10,2,IY)
           T33( 3,IY,IS) =  CMUSCONSUMPTION(10,3,IY)
!          --- COGENERATION
           T33( 4,IY,IS) = CMUSCOGEN(1,IY)
           T33( 5,IY,IS) = CMUSCOGEN(2,IY)
           T33( 6,IY,IS) = CMUSCOGEN(3,IY)
           T33( 7,IY,IS) = CMUSCOGEN(4,IY)
!          --- DISTRICT SERVICES
!          --- HEATING
           T33( 8,IY,IS) = CMUSDISTSERV(1,1,IY)
           T33( 9,IY,IS) = CMUSDISTSERV(1,2,IY)
           T33(10,IY,IS) = CMUSDISTSERV(1,3,IY)
!          --- COOLING
           T33(11,IY,IS) = CMUSDISTSERV(2,1,IY)
           T33(12,IY,IS) = CMUSDISTSERV(2,2,IY)
!          --- WATER HEATING
           T33(13,IY,IS) = CMUSDISTSERV(3,1,IY)
           T33(14,IY,IS) = CMUSDISTSERV(3,2,IY)
           T33(15,IY,IS) = CMUSDISTSERV(3,3,IY)
!          --- Avg. Eff. Purchased Equip.
!          --- HEATING
           T33(16,IY,IS) = CMUSPURCHEFF(1,1,IY)
           T33(17,IY,IS) = CMUSPURCHEFF(1,2,IY)
           T33(18,IY,IS) = CMUSPURCHEFF(1,3,IY)
!          --- COOLING
           T33(19,IY,IS) = CMUSPURCHEFF(2,1,IY)
           T33(20,IY,IS) = CMUSPURCHEFF(2,2,IY)
!          --- WATER HEATING
           T33(21,IY,IS) = CMUSPURCHEFF(3,1,IY)
           T33(22,IY,IS) = CMUSPURCHEFF(3,2,IY)
           T33(23,IY,IS) = CMUSPURCHEFF(3,3,IY)
!          --- VENTILATION
           T33(24,IY,IS) = CMUSPURCHEFF(4,1,IY)
!          --- COOKING
           T33(25,IY,IS) = CMUSPURCHEFF(5,1,IY)
           T33(26,IY,IS) = CMUSPURCHEFF(5,2,IY)
!          --- LIGHTING
           T33(27,IY,IS) = CMUSPURCHEFF(6,1,IY)
!          --- REFRIGERATION
           T33(28,IY,IS) = CMUSPURCHEFF(7,1,IY)

        ENDDO

!     Table 34. Industrial Sector Macroeconomic Indicators

         DO IY = 1,LASTYR
         DO IR=1,MNUMCR
           IF (IR .EQ. 10) CYCLE
           T34( 1,IR,IY,IS) =  MC_GDPR(IY)
!  6 is paper total with 7, 8, 9 subsets; 13 is ethanol and 21 is flat glass, both subsets (of 12 and 20)
           T34( 2,IR,IY,IS) =  sum(MC_EMPNA(IR,1:19,IY)) + sum(MC_EMPNA(IR,22:39,IY))

!          --- MULTIPLY BY .001, WHICH IS SAME AS / 1000., SO THAT
!          --- TOTAL INDUSTRIES IS NOT TOO MANY DIGITS FOR DISPLAY

!          --- NONMANUFACTURING SECTOR
           T34( 3,IR,IY,IS) = MC_REVIND(IR,42,IY) + MC_REVIND(IR,43,IY) + MC_REVIND(IR,44,IY)
           T34( 4,IR,IY,IS) = MC_REVIND(IR,45,IY) + MC_REVIND(IR,46,IY) + MC_REVIND(IR,47,IY)
           T34( 5,IR,IY,IS) = MC_REVIND(IR,48,IY)
           T34( 98,IR,IY,IS) = MC_REVIND(IR,45,IY)
           T34( 99,IR,IY,IS) = MC_REVIND(IR,46,IY)
           T34(100,IR,IY,IS) = MC_REVIND(IR,47,IY)
!          --- MANUFACTURING SECTOR
           T34( 6,IR,IY,IS) = MC_REVIND(IR, 1,IY)
           T34(63,IR,IY,IS) = MC_REVIND(IR, 2,IY)
           T34(64,IR,IY,IS) = MC_REVIND(IR, 3,IY)
           T34(65,IR,IY,IS) = MC_REVIND(IR, 4,IY)
           T34(66,IR,IY,IS) = MC_REVIND(IR, 5,IY)
           T34( 7,IR,IY,IS) = MC_REVIND(IR, 6,IY)
           T34( 8,IR,IY,IS) = MC_REVIND(IR, 7,IY)
           T34( 9,IR,IY,IS) = MC_REVIND(IR, 8,IY)
           T34(10,IR,IY,IS) = MC_REVIND(IR, 9,IY)
           T34(11,IR,IY,IS) = MC_REVIND(IR,10,IY)
           T34(95,IR,IY,IS) = MC_REVIND(IR,11,IY)
           T34(96,IR,IY,IS) = MC_REVIND(IR,12,IY)
           T34(97,IR,IY,IS) = MC_REVIND(IR,13,IY)
           T34(12,IR,IY,IS) = MC_REVIND(IR,14,IY)
           T34(13,IR,IY,IS) = MC_REVIND(IR,15,IY) + MC_REVIND(IR,16,IY) + MC_REVIND(IR,18,IY) + &
                              MC_REVIND(IR,19,IY) + MC_REVIND(IR,20,IY)
           T34(24,IR,IY,IS) = MC_REVIND(IR,15,IY) + MC_REVIND(IR,16,IY) + &
                              MC_REVIND(IR,18,IY) + MC_REVIND(IR,19,IY)
           T34(37,IR,IY,IS) = MC_REVIND(IR,15,IY)
           T34(38,IR,IY,IS) = MC_REVIND(IR,16,IY)
           T34(34,IR,IY,IS) = MC_REVIND(IR,17,IY)     !  ethanol, part of organic chemicals
           T34(39,IR,IY,IS) = MC_REVIND(IR,18,IY)
           T34(40,IR,IY,IS) = MC_REVIND(IR,19,IY)
           T34(25,IR,IY,IS) = MC_REVIND(IR,20,IY)
           T34(67,IR,IY,IS) = MC_REVIND(IR,21,IY)
           T34(68,IR,IY,IS) = MC_REVIND(IR,22,IY)
           T34(69,IR,IY,IS) = MC_REVIND(IR,23,IY)
           T34(70,IR,IY,IS) = MC_REVIND(IR,24,IY)
           T34(14,IR,IY,IS) = MC_REVIND(IR,25,IY) + MC_REVIND(IR,26,IY)
           T34(26,IR,IY,IS) = MC_REVIND(IR,25,IY)
           T34(27,IR,IY,IS) = MC_REVIND(IR,26,IY)
           T34(15,IR,IY,IS) = MC_REVIND(IR,27,IY)
           T34(16,IR,IY,IS) = MC_REVIND(IR,28,IY) + MC_REVIND(IR,30,IY) + MC_REVIND(IR,31,IY) + MC_REVIND(IR,32,IY)
           T34(28,IR,IY,IS) = MC_REVIND(IR,28,IY)
           T34(35,IR,IY,IS) = MC_REVIND(IR,29,IY)      !  flat glass, part of glass
           T34(29,IR,IY,IS) = MC_REVIND(IR,30,IY) + MC_REVIND(IR,31,IY)
           T34(61,IR,IY,IS) = MC_REVIND(IR,30,IY)
           T34(62,IR,IY,IS) = MC_REVIND(IR,31,IY)
           T34(30,IR,IY,IS) = MC_REVIND(IR,32,IY)
           T34(17,IR,IY,IS) = MC_REVIND(IR,33,IY) + MC_REVIND(IR,34,IY) + MC_REVIND(IR,35,IY)
           T34(31,IR,IY,IS) = MC_REVIND(IR,33,IY)
           T34(32,IR,IY,IS) = MC_REVIND(IR,34,IY)
           T34(33,IR,IY,IS) = MC_REVIND(IR,35,IY)
           T34(18,IR,IY,IS) = MC_REVIND(IR,36,IY)
           T34(19,IR,IY,IS) = MC_REVIND(IR,37,IY)
           T34(20,IR,IY,IS) = MC_REVIND(IR,38,IY)
           T34(21,IR,IY,IS) = MC_REVIND(IR,39,IY)
           T34(22,IR,IY,IS) = MC_REVIND(IR,40,IY)
           T34(23,IR,IY,IS) = MC_REVIND(IR,41,IY)
           T34(36,IR,IY,IS) = FSUM(T34(3,IR,IY,IS),21)

!          grouping as in Table 18:
           T34(71,IR,IY,IS) = sum(MC_REVSER(IR,1:10,IY))
           DO III=1,48
             IF ((III .ge. 2 .AND. III .le. 5) .OR. (III .ge. 21 .AND. III .le. 24) .OR.   &
                 (III .ge. 11 .AND. III .le. 13) .OR.     &
                 (III .eq. 17) .OR. (III .eq. 29)) CYCLE  ! these are subsets of other elements
             T34(72,IR,IY,IS) = T34(72,IR,IY,IS) + MC_REVIND(IR,III,IY)
             IF (III .LE. 41) THEN
               T34(74,IR,IY,IS) = T34(74,IR,IY,IS) + MC_REVIND(IR,III,IY)
               IF (III .EQ.  1 .OR. III .EQ. 10 .OR. (III .GE. 15 .AND. III .LE. 19) .OR. &
                   III .EQ. 25 .OR. III .EQ. 28 .OR. III .EQ. 30 .OR. III .EQ. 31 .OR. &
                   III .EQ. 33 .OR. III .EQ. 34) THEN
                 T34(75,IR,IY,IS) = T34(75,IR,IY,IS) + MC_REVIND(IR,III,IY)
               ELSE
                 T34(76,IR,IY,IS) = T34(76,IR,IY,IS) + MC_REVIND(IR,III,IY)
               ENDIF
             ELSE
               T34(73,IR,IY,IS) = T34(73,IR,IY,IS) + MC_REVIND(IR,III,IY)
             ENDIF
           ENDDO
           T34(77,IR,IY,IS) = T34(72,IR,IY,IS) + T34(71,IR,IY,IS)
!          grouping as transportation model does it:
!          freight
           T34(78,IR,IY,IS) =sum(MC_REVIND(IR,15:20,IY))-MC_REVIND(IR,17,IY)-MC_REVIND(IR,19,IY)-MC_REVIND(IR,21,IY)   !20 includes 21!
           T34(79,IR,IY,IS) =sum(MC_REVIND(IR,33:35,IY))
           T34(80,IR,IY,IS) =sum(MC_REVIND(IR,2:5,IY))
           T34(81,IR,IY,IS) =sum(MC_REVIND(IR,11:13,IY))
           T34(82,IR,IY,IS) = MC_REVIND(IR,25,IY) + MC_REVIND(IR,26,IY)
           T34(83,IR,IY,IS) = MC_REVIND(IR,28,IY) + MC_REVIND(IR,30,IY) + MC_REVIND(IR,32,IY)
           T34(84,IR,IY,IS) =sum(MC_REVIND(IR,36:40,IY)) - MC_REVIND(IR,38,IY)
           T34(85,IR,IY,IS) =sum(MC_REVIND(IR, 7: 8,IY))+MC_REVIND(IR,14,IY)+MC_REVIND(IR,41,IY)
           T34(86,IR,IY,IS) =sum(MC_REVIND(IR,42:44,IY))
           T34(87,IR,IY,IS) =sum(MC_REVIND(IR,45:47,IY))
           T34(88,IR,IY,IS) = MC_REVIND(IR, 6,IY)
           T34(89,IR,IY,IS) = MC_REVIND(IR,21,IY)
           T34(102,IR,IY,IS)= MC_REVIND(IR,19,IY)
           T34(103,IR,IY,IS)= MC_REVIND(IR,27,IY)
           T34(104,IR,IY,IS)= MC_REVIND(IR,38,IY)
           T34(105,IR,IY,IS)= MC_REVIND(IR, 9,IY)
        !  commercial light trucks
           T34(90,IR,IY,IS) = MC_REVIND(IR,42,IY)
           T34(91,IR,IY,IS) =sum(MC_REVIND(IR,45:47,IY))
           T34(92,IR,IY,IS) = MC_REVIND(IR,48,IY)
           T34(93,IR,IY,IS) =sum(MC_REVIND(IR, 1:41,IY)) - sum(MC_REVIND(IR,2:5,IY)) -  &
                             sum(MC_REVIND(IR,11:13,IY)) -     MC_REVIND(IR,17,IY)   -  &
                             sum(MC_REVIND(IR,21:24,IY)) -     MC_REVIND(IR,29,IY)
           T34(94,IR,IY,IS) =sum(MC_REVSER(IR, 3: 4,IY))

!  Industrial consumptions, which are in Census Regions rather than Census Divisions
!            so assign a region to each division it contains:
           T34(41,IR,IY,IS) = sum(       AGCON(:,CRfromCD(IR),IY))
           T34(42,IR,IY,IS) = sum(     MINECON(:,CRfromCD(IR),IY)) + QLPIN(IR,IY)*1000.
           T34(101,IR,IY,IS) = QNGLQ(IR,IY)*1000.
           IF (IR .EQ.  8 .OR. IR .EQ. 11) T34(42,IR,IY,IS) = T34(42,IR,IY,IS) + OGELSHALE(IY)
           T34(43,IR,IY,IS) = sum(    CONSTCON(:,CRfromCD(IR),IY))
           T34(44,IR,IY,IS) = sum(     FOODCON(:,CRfromCD(IR),IY))
           T34(45,IR,IY,IS) = sum(    PAPERCON(:,CRfromCD(IR),IY))
           T34(46,IR,IY,IS) = sum(     CHEMCON(:,CRfromCD(IR),IY))
           T34(47,IR,IY,IS) = sum(      REFCON(1:17,5,IY))
           T34(47,IR,IY,IS) = T34(47,IR,IY,IS) + 1000. * &
                     ((CORNCD(3,11,IY) * CFCORN / 1000. -0.9751*(CRNETHCD(11,IY)+OTHETHCD(11,IY)) * 365. * CFPET - &
                       RFBIOBUTECD(MNUMCR,IY)*365*CFBIOBUTE(IY)) / 1000000. + &
                       sum(BIMQTYCD(1:4,11,IY))/1000000.*RDAYS*(CFVEGGIE(IY)-CFBIOD(IY)) + &
                 QBMRFBTL(11,IY)/1000. - RDAYS / 1000000. * &
                      (sum(BTLFRAC(1:4,MNUMPR,IY)) * CFBTLLIQ(IY) + &
                       sum(CBTLFRAC(2,1:4,MNUMPR,IY)) * CFCBTLLIQ(2,IY) + &
                       UBAVOL(MNUMPR,IY) * 5.763))
           IF (CONEFF(IY) .NE. 0.0) T34(47,IR,IY,IS) = T34(47,IR,IY,IS) + 1000. *  &
                (0.9751*CLLETHCD(11,IY) * RDAYS * (CFBMQ(IY) * 42. / CONEFF(IY) - CFPET)) / 1000000.
           T34(48,IR,IY,IS) = sum(    GLASSCON(:,CRfromCD(IR),IY))
           T34(49,IR,IY,IS) = sum(   CEMENTCON(:,CRfromCD(IR),IY))
           T34(50,IR,IY,IS) = sum(    STEELCON(:,CRfromCD(IR),IY))
           T34(51,IR,IY,IS) = sum(     ALUMCON(:,CRfromCD(IR),IY))
           T34(52,IR,IY,IS) = sum( FABMETALCON(:,CRfromCD(IR),IY))
           T34(53,IR,IY,IS) = sum(  MACHINECON(:,CRfromCD(IR),IY))
           T34(54,IR,IY,IS) = sum(  COMPUTECON(:,CRfromCD(IR),IY))
           T34(55,IR,IY,IS) = sum(TRANEQUIPCON(:,CRfromCD(IR),IY))
           T34(56,IR,IY,IS) = sum( ELECEQUIPON(:,CRfromCD(IR),IY))
           T34(57,IR,IY,IS) = sum( WOODPRODCON(:,CRfromCD(IR),IY))
           T34(58,IR,IY,IS) = sum(  PLASTICCON(:,CRfromCD(IR),IY))
           T34(59,IR,IY,IS) = sum(   BOMOTHCON(:,CRfromCD(IR),IY))
           T34(60,IR,IY,IS) = FSUM(T34(41,IR,IY,IS),19)

         ENDDO
         ENDDO

!     Table 35. Energy Consumption: Refineries

!      integer ixEL/1/,ixNG/2/,ixCL/3/,ixMC/4/,
!     &        ixCI/5/,ixRF/6/,ixDS/7/,ixLG/8/,ixMG/9/,
!     &        ixSG/10/,ixPC/11/,ixAS/12/,ixPF/13/,ixKS/14/,
!     &        ixOP/15/,ixNF/16/,ixLF/17/,ixRN/18/
         modREFCON(:,:,:) = 0.0
         DO 350, IY = 1,LASTYR
           RDAYS=365.
!          --- INDUSTRY OUTPUT
           T35( 1,IY,IS) = MC_REVIND(11,25,IY)
           T35(62,IY,IS) = RFQTDCRD(MNUMOR+2,IY)+RFIMCR(MNUMPR,IY)+RFCRDOTH(MNUMPR,IY)+RFPQUFC(MNUMPR,IY,2)
!          --- ENERGY CONSUMPTION
           T35(67,IY,IS) = REFCON(ixLG,5,IY) + REFCON(ixSG,5,IY)
           T35( 2,IY,IS) = REFCON(ixRF,5,IY)
           T35( 3,IY,IS) = REFCON(ixDS,5,IY)
           T35( 4,IY,IS) = REFCON(ixLG,5,IY)
           T35( 5,IY,IS) = REFCON(ixPC,5,IY)
           T35( 6,IY,IS) = REFCON(ixSG,5,IY)
           T35( 7,IY,IS) = REFCON(ixOP,5,IY)
           T35( 8,IY,IS) = FSUM(T35(2,IY,IS),6)
           T35( 9,IY,IS) = REFCON(ixNG,5,IY)
           T35(68,IY,IS) = REFCON(ixNG,5,IY) - RFQNGPF(11,IY) - QGTLRF(11,IY) * 1000.
           T35(69,IY,IS) = RFQNGPF(11,IY)
           T35(70,IY,IS) = QGTLRF(11,IY) * 1000.
           T35(10,IY,IS) = REFCON(ixCL,5,IY)
           T35(63,IY,IS) =(CGREFQ(11,IY,6) + CGREFQ(11,IY,7))
           T35(11,IY,IS) = REFCON(ixEL,5,IY)
           T35(48,IY,IS)= 1000. * &
                   ((CORNCD(3,11,IY) * CFCORN / 1000. -0.9751*(CRNETHCD(11,IY)+OTHETHCD(11,IY)) * 365. * CFPET - &
                     RFBIOBUTECD(MNUMCR,IY)*365.*CFBIOBUTE(IY))/1000000. + &
                     sum(BIMQTYCD(1:4,11,IY))/1000000.*RDAYS*(CFVEGGIE(IY)-CFBIOD(IY)) + &
                QBMRFBTL(11,IY)/1000. - RDAYS / 1000000. * &
                (sum(BTLFRAC(1:4,MNUMPR,IY)) * CFBTLLIQ(IY) + &
                 sum(CBTLFRAC(2,1:4,MNUMPR,IY)) * CFCBTLLIQ(2,IY) + &
                 UBAVOL(MNUMPR,IY) * 5.763))
           IF (CONEFF(IY) .NE. 0.0) T35(48,IY,IS) = T35(48,IY,IS) + 1000. * &
                   ((0.9751*CLLETHCD(11,IY) * RDAYS * (CFBMQ(IY) * 42. / CONEFF(IY) - CFPET)) / 1000000.)
           T35(12,IY,IS) = FSUM(T35(8,IY,IS),4) + T35(48,IY,IS) !+ T35(63,IY,IS)
           T35(13,IY,IS) = REFCON(ixRF,5,IY)
           T35(14,IY,IS) = REFCON(ixDS,5,IY)
           T35(15,IY,IS) = REFCON(ixLG,5,IY)
           T35(16,IY,IS) = REFCON(ixPC,5,IY) - CGREFQ(11,IY,2)
           T35(17,IY,IS) = REFCON(ixSG,5,IY) - CGREFQ(11,IY,9)
           T35(18,IY,IS) = REFCON(ixOP,5,IY)
           T35(20,IY,IS) = REFCON(ixNG,5,IY) - CGREFQ(11,IY,3) - QNGETH(IY,11) - QGTLRF(11,IY) * 1000. - RFQNGPF(11,IY)
           T35(21,IY,IS) = MAX(0.0,(REFCON(ixCL,5,IY) - CGREFQ(11,IY,1) - QCLETH(IY,11)))
           T35(22,IY,IS) = REFCON(ixEL,5,IY) -QELETH(IY,11)
           T35(19,IY,IS) = FSUM(T35(13,IY,IS),6)
           T35(23,IY,IS) = FSUM(T35(19,IY,IS),4)
           T35(24,IY,IS) = INDCARB(REFCON(1,1,IY),IY) ! function indcarb calcs carbon emissions

! do carbon dioxide for refinery use only
           modREFCON(ixRF,5,IY) = T35(13,IY,IS)
           modREFCON(ixDS,5,IY) = T35(14,IY,IS)
           modREFCON(ixLG,5,IY) = T35(15,IY,IS)
           modREFCON(ixPC,5,IY) = T35(16,IY,IS)
           modREFCON(ixSG,5,IY) = T35(17,IY,IS)
           modREFCON(ixOP,5,IY) = T35(18,IY,IS)
           modREFCON(ixNG,5,IY) = T35(20,IY,IS)
           modREFCON(ixCL,5,IY) = T35(21,IY,IS)

           modREFCON(ixEL,1,IY) = REFCON(ixEL,1,IY) -sum(QELETH(IY,1:2))
           modREFCON(ixEL,2,IY) = REFCON(ixEL,2,IY) -sum(QELETH(IY,3:4))
           modREFCON(ixEL,3,IY) = REFCON(ixEL,3,IY) -sum(QELETH(IY,5:7))
           modREFCON(ixEL,4,IY) = REFCON(ixEL,4,IY) -sum(QELETH(IY,8:9))
           modREFCON(ixEL,5,IY) = sum(modREFCON(ixEL,1:4,IY))

           T35(66,IY,IS) = INDCARB(modREFCON(1,1,IY),IY) ! function indcarb calcs carbon emissions

           DENOM = T35(62,IY,IS) * RDAYS / 1000.
           IF (DENOM .NE. 0.0) THEN
             T35(50,IY,IS) =   T35(13,IY,IS) / DENOM
             T35(51,IY,IS) =   T35(14,IY,IS) / DENOM
             T35(52,IY,IS) =   T35(15,IY,IS) / DENOM
             T35(53,IY,IS) =   T35(16,IY,IS) / DENOM
             T35(54,IY,IS) =   T35(17,IY,IS) / DENOM
             T35(55,IY,IS) =   T35(18,IY,IS) / DENOM
             T35(57,IY,IS) =   T35(20,IY,IS) / DENOM
             T35(58,IY,IS) =   T35(21,IY,IS) / DENOM
             T35(60,IY,IS) =   T35(22,IY,IS) / DENOM
           ENDIF
           T35(56,IY,IS) = FSUM(T35(50,IY,IS),6)
           T35(61,IY,IS) = FSUM(T35(56,IY,IS),5)
        IL=24
        INDDIR=16
        T35(25,IY,IS) = CGREFCAP(11,IY,1) * .001  ! coal
        T35(26,IY,IS) = CGREFCAP(11,IY,2) * .001  ! oil
        T35(27,IY,IS) = CGREFCAP(11,IY,3) * .001  ! nat gas
        T35(28,IY,IS) =(CGREFCAP(11,IY,6) + CGREFCAP(11,IY,7) + CGREFCAP(11,IY,9)) * .001    ! Other
        T35(29,IY,IS) = sum(T35(25:29,IY,IS))
        IL=IL+5 !29

        T35(30,IY,IS) = (CGREFGEN(11,IY,1,1) + CGREFGEN(11,IY,1,2)) * .001  ! coal
        T35(31,IY,IS) = (CGREFGEN(11,IY,2,1) + CGREFGEN(11,IY,2,2)) * .001  ! oil
        T35(32,IY,IS) = (CGREFGEN(11,IY,3,1) + CGREFGEN(11,IY,3,2)) * .001  ! nat gas
        T35(33,IY,IS) = (CGREFGEN(11,IY,6,1) + CGREFGEN(11,IY,6,2) + &      ! Other
                           CGREFGEN(11,IY,7,1) + CGREFGEN(11,IY,7,2) + &
                           CGREFGEN(11,IY,9,1) + CGREFGEN(11,IY,9,2)) * .001
        T35(34,IY,IS) = sum(T35(30:33,IY,IS))
        IL=IL+5 !34
        T35(35,IY,IS)=sum(CGREFGEN(11,iy,:,1)) *.001 ! Sales to grid
        T35(36,IY,IS)=sum(CGREFGEN(11,iy,:,2)) *.001 ! Own-Use

        IL=IL+2 !37
        T35(37,IY,IS) =  CGREFQ(11,IY,1)                      ! coal
        T35(38,IY,IS) =  CGREFQ(11,IY,2)                      ! oil
        T35(39,IY,IS) =  CGREFQ(11,IY,3)                      ! nat gas
        T35(40,IY,IS) = (CGREFQ(11,IY,6) + CGREFQ(11,IY,7) + CGREFQ(11,IY,9))   ! other
        T35(41,IY,IS) = sum(T35(37:40,IY,IS))

        T35(44,IY,IS) = QNGETH(IY,11)
        T35(45,IY,IS) = QCLETH(IY,11)
        T35(46,IY,IS) = QELETH(IY,11)
        T35(47,IY,IS) = FSUM(T35(44,IY,IS),3)
  350    CONTINUE

!     Table 36. Energy Consumption: Food Industry

      DO 360, IY = 1,LASTYR
!       --- INDUSTRY OUTPUT
        T36( 1,IY,IS) = MC_REVIND(11,1,IY)
!       --- ENERGY CONSUMPTION
        T36( 2,IY,IS) = FOODCON(ixRF,5,IY)
        T36( 3,IY,IS) = FOODCON(ixDS,5,IY)
        T36( 4,IY,IS) = FOODCON(ixLG,5,IY)
        T36( 5,IY,IS) = FOODCON(ixOP,5,IY)
        T36( 6,IY,IS) = FSUM(T36(2,IY,IS),4)
        T36( 7,IY,IS) = FOODCON(ixNG,5,IY)
        T36( 8,IY,IS) = FOODCON(ixCL,5,IY)
        T36( 9,IY,IS) = FOODCON(ixRN,5,IY)
        T36(10,IY,IS) = FOODCON(ixEL,5,IY)
        T36(11,IY,IS) = FSUM(T36(6,IY,IS),5)
!       --- ENERGY CONSUMPTION PER UNIT OF OUTPUT
        IF (MC_REVIND(11,1,IY) .NE. 0.0) THEN
          T36(12,IY,IS) = FOODCON(ixRF,5,IY) / MC_REVIND(11,1,IY)
          T36(13,IY,IS) = FOODCON(ixDS,5,IY) / MC_REVIND(11,1,IY)
          T36(14,IY,IS) = FOODCON(ixLG,5,IY) / MC_REVIND(11,1,IY)
          T36(15,IY,IS) = FOODCON(ixOP,5,IY) / MC_REVIND(11,1,IY)
          T36(16,IY,IS) = FSUM(T36(12,IY,IS),4)
          T36(17,IY,IS) = FOODCON(ixNG,5,IY) / MC_REVIND(11,1,IY)
          T36(18,IY,IS) = FOODCON(ixCL,5,IY) / MC_REVIND(11,1,IY)
          T36(19,IY,IS) = FOODCON(ixRN,5,IY) / MC_REVIND(11,1,IY)
          T36(20,IY,IS) = FOODCON(ixEL,5,IY) / MC_REVIND(11,1,IY)
        ENDIF
        T36(21,IY,IS) = FSUM(T36(16,IY,IS),5)
        T36(22,IY,IS) = INDCARB(FOODCON(1,1,IY),IY) ! function indcarb calcs carbon emissions
        IL=22
        INDDIR=7
        T36(23,IY,IS)=CHPINDCAP(1,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T36(24,IY,IS)=CHPINDCAP(2,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T36(25,IY,IS)=CHPINDCAP(3,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T36(26,IY,IS)=sum(CHPINDCAP(4:6,inddir,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T36(27,IY,IS)=CHPINDCAP(7,inddir,iy) ! total across fuels
        IL=IL+5 !27
        T36(28,IY,IS)=CHPINDGEN(1,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T36(29,IY,IS)=CHPINDGEN(2,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T36(30,IY,IS)=CHPINDGEN(3,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T36(31,IY,IS)=sum(CHPINDGEN(4:6,inddir,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T36(32,IY,IS)=CHPINDGEN(7,inddir,iy) ! total across fuels
        IL=IL+5 !33
        T36(33,IY,IS)=CHPINDGEN(7,inddir,iy)*    CHPGRDSHR(inddir,iy) ! Sales to grid
        T36(34,IY,IS)=CHPINDGEN(7,inddir,iy)*(1.-CHPGRDSHR(inddir,iy)) ! Own-Use
        T36(35,IY,IS)=CHPINDSTM(7,inddir,iy) ! total across fuels

        IL=IL+3

360 CONTINUE

!     Table 37. Energy Consumption: Paper Industry

      DO 370, IY = 1,LASTYR
!       --- INDUSTRY OUTPUT
        T37( 1,IY,IS) = MC_REVIND(11,10,IY)
!       --- ENERGY CONSUMPTION
        T37( 2,IY,IS) = PAPERCON(ixRF,5,IY)
        T37( 3,IY,IS) = PAPERCON(ixDS,5,IY)
        T37( 4,IY,IS) = PAPERCON(ixLG,5,IY)
        T37( 5,IY,IS) = PAPERCON(ixPC,5,IY)
        T37(43,IY,IS) = PAPERCON(ixOP,5,IY)
        T37( 6,IY,IS) = FSUM(T37(2,IY,IS),4) + T37(43,IY,IS)
        T37( 7,IY,IS) = PAPERCON(ixNG,5,IY)
        T37( 8,IY,IS) = PAPERCON(ixCL,5,IY)
        T37( 9,IY,IS) = PAPERCON(ixRN,5,IY)
        T37(10,IY,IS) = PAPERCON(ixEL,5,IY)
        T37(11,IY,IS) = FSUM(T37(6,IY,IS),5)
!       --- ENERGY CONSUMPTION PER UNIT OF OUTPUT
        IF (MC_REVIND(11,10,IY) .NE. 0.0) THEN
          T37(12,IY,IS) = PAPERCON(ixRF,5,IY) / MC_REVIND(11,10,IY)
          T37(13,IY,IS) = PAPERCON(ixDS,5,IY) / MC_REVIND(11,10,IY)
          T37(14,IY,IS) = PAPERCON(ixLG,5,IY) / MC_REVIND(11,10,IY)
          T37(15,IY,IS) = PAPERCON(ixPC,5,IY) / MC_REVIND(11,10,IY)
          T37(44,IY,IS) = PAPERCON(ixOP,5,IY) / MC_REVIND(11,10,IY)
          T37(16,IY,IS) = FSUM(T37(12,IY,IS),4) + T37(44,IY,IS)
          T37(17,IY,IS) = PAPERCON(ixNG,5,IY) / MC_REVIND(11,10,IY)
          T37(18,IY,IS) = PAPERCON(ixCL,5,IY) / MC_REVIND(11,10,IY)
          T37(19,IY,IS) = PAPERCON(ixRN,5,IY) / MC_REVIND(11,10,IY)
          T37(20,IY,IS) = PAPERCON(ixEL,5,IY) / MC_REVIND(11,10,IY)
        ENDIF
        T37(21,IY,IS) = FSUM(T37(16,IY,IS),5)
        T37(22,IY,IS) = INDCARB(PAPERCON(1,1,IY),IY) ! function indcarb calcs carbon emissions
        IL=22
        INDDIR=8
        T37(23,IY,IS)=CHPINDCAP(1,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T37(24,IY,IS)=CHPINDCAP(2,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T37(25,IY,IS)=CHPINDCAP(3,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T37(26,IY,IS)=sum(CHPINDCAP(4:6,inddir,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T37(27,IY,IS)=CHPINDCAP(7,inddir,iy) ! total across fuels
        IL=IL+5 !27
        T37(28,IY,IS)=CHPINDGEN(1,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T37(29,IY,IS)=CHPINDGEN(2,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T37(30,IY,IS)=CHPINDGEN(3,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T37(31,IY,IS)=sum(CHPINDGEN(4:6,inddir,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T37(32,IY,IS)=CHPINDGEN(7,inddir,iy) ! total across fuels
        IL=IL+5 !32
        T37(33,IY,IS)=CHPINDGEN(7,inddir,iy)*    CHPGRDSHR(inddir,iy) ! Sales to grid
        T37(34,IY,IS)=CHPINDGEN(7,inddir,iy)*(1.-CHPGRDSHR(inddir,iy)) ! Own-Use
        T37(35,IY,IS)=CHPINDSTM(7,inddir,iy) ! total across fuels

        IL=IL+3

  370 CONTINUE

!     Table 38. Energy Consumption: Bulk Chemical Industry

      DO 380, IY = 1,LASTYR
!       --- INDUSTRY OUTPUT
        T38( 1,IY,IS) = MC_REVIND(11,15,IY) + MC_REVIND(11,16,IY) + &
                        MC_REVIND(11,18,IY) + MC_REVIND(11,19,IY)
!       --- HEAT AND POWER ENERGY CONSUMPTION
        T38( 2,IY,IS) = CHEMCON(ixRF,5,IY)
        T38( 3,IY,IS) = CHEMCON(ixDS,5,IY)
        T38( 4,IY,IS) = CHEMCON(ixLG,5,IY)
        T38( 5,IY,IS) = CHEMCON(ixPC,5,IY)
        T38( 6,IY,IS) = CHEMCON(ixOP,5,IY)
        T38(51,IY,IS) = FSUM(T38(2,IY,IS),5)
        T38( 7,IY,IS) = CHEMCON(ixNG,5,IY)
        T38( 8,IY,IS) = CHEMCON(ixCL,5,IY)
        T38(53,IY,IS) = CHEMCON(ixRN,5,IY)
        T38( 9,IY,IS) = CHEMCON(ixEL,5,IY)
        T38(10,IY,IS) = FSUM(T38(2,IY,IS),8) + T38(53,IY,IS)
!  propylene in it's own category; re-use propylene row from chemical section; multiply by 1000, this is in trills
!       --- FEEDSTOCK ENERGY CONSUMPTION
        T38(11,IY,IS) = CHEMCON(ixLF,5,IY)
        T38(91,IY,IS)=QPPINPF(MNUMCR,IY) * 1000.
        T38(92,IY,IS)=QETINPF(MNUMCR,IY) * 1000.
        T38(93,IY,IS)=QPRINPF(MNUMCR,IY) * 1000.
        T38(94,IY,IS)=QPROLENERF(MNUMCR,IY) * 1000.
        T38(95,IY,IS)=QBUINPF(MNUMCR,IY) * 1000.
        T38(96,IY,IS)=QISINPF(MNUMCR,IY) * 1000.
        T38(12,IY,IS) = CHEMCON(ixPF,5,IY) * 1.         ! Need to multiply by 1. for units to come out correctly
        T38(13,IY,IS) = CHEMCON(ixNF,5,IY)
        T38(14,IY,IS) = FSUM(T38(11,IY,IS),3)
        T38(15,IY,IS) = T38(10,IY,IS) + T38(14,IY,IS)
!       --- HEAT AND POWER ENERGY CONSUMPTION PER UNIT OF OUTPUT
        IF (T38(1,IY,IS) .NE. 0.0) THEN
          T38(16,IY,IS) = CHEMCON(ixRF,5,IY) / T38(1,IY,IS)
          T38(17,IY,IS) = CHEMCON(ixDS,5,IY) / T38(1,IY,IS)
          T38(18,IY,IS) = CHEMCON(ixLG,5,IY) / T38(1,IY,IS)
          T38(19,IY,IS) = CHEMCON(ixPC,5,IY) / T38(1,IY,IS)
          T38(20,IY,IS) = CHEMCON(ixOP,5,IY) / T38(1,IY,IS)
          T38(52,IY,IS) = FSUM(T38(16,IY,IS),5)
          T38(21,IY,IS) = CHEMCON(ixNG,5,IY) / T38(1,IY,IS)
          T38(22,IY,IS) = CHEMCON(ixCL,5,IY) / T38(1,IY,IS)
          T38(54,IY,IS) = CHEMCON(ixRN,5,IY) / T38(1,IY,IS)
          T38(23,IY,IS) = CHEMCON(ixEL,5,IY) / T38(1,IY,IS)
          T38(24,IY,IS) = FSUM(T38(16,IY,IS),8) + T38(54,IY,IS)
!  propylene in it's own category; re-use on-purpose propylene row from chemical section; multiply by 1000, this is in trills
!         --- FEEDSTOCK ENERGY CONSUMPTION PER UNIT OF OUTPUT
          T38(25,IY,IS) = CHEMCON(ixLF,5,IY) / T38(1,IY,IS)
          T38(26,IY,IS) = CHEMCON(ixPF,5,IY) / T38(1,IY,IS)
          T38(27,IY,IS) = CHEMCON(ixNF,5,IY) / T38(1,IY,IS)
          T38(28,IY,IS) = FSUM(T38(25,IY,IS),3)
        ENDIF
!       --- TOTAL
        T38(29,IY,IS) = T38(24,IY,IS) + T38(28,IY,IS)
        T38(30,IY,IS) = INDCARB(CHEMCON(1,1,IY),IY) ! function indcarb calcs carbon emissions
        IL=30
        INDDIR=9
        T38(31,IY,IS)=CHPINDCAP(1,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T38(32,IY,IS)=CHPINDCAP(2,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T38(33,IY,IS)=CHPINDCAP(3,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T38(34,IY,IS)=sum(CHPINDCAP(4:6,inddir,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T38(35,IY,IS)=CHPINDCAP(7,inddir,iy) ! total across fuels
        IL=IL+5 !35
        T38(36,IY,IS)=CHPINDGEN(1,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T38(37,IY,IS)=CHPINDGEN(2,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T38(38,IY,IS)=CHPINDGEN(3,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T38(39,IY,IS)=sum(CHPINDGEN(4:6,inddir,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T38(40,IY,IS)=CHPINDGEN(7,inddir,iy) ! total across fuels
        IL=IL+5 !40
        T38(41,IY,IS)=CHPINDGEN(7,inddir,iy)*    CHPGRDSHR(inddir,iy) ! Sales to grid
        T38(42,IY,IS)=CHPINDGEN(7,inddir,iy)*(1.-CHPGRDSHR(inddir,iy)) ! Own-Use
        T38(43,IY,IS)=CHPINDSTM(7,inddir,iy) ! total across fuels

!       IF (IY+1989 .GE. 2002) THEN
!         T38(55:69,IY,IS) = REPPRODORG( 1:15,IY+1989)
!         T38(89,IY,IS) = REPPRODORG(16,IY+1989)
!         T38(90,IY,IS) = REPPRODORG(17,IY+1989)
!         T38(70,IY,IS) = FSUM(T38(55,IY,IS),15) + FSUM(T38(89,IY,IS), 2)
!         T38(71:76,IY,IS) = REPPRODINORG( 1:6,IY+1989)
!         T38(77,IY,IS) = FSUM(T38(71,IY,IS), 6)
!         T38(78:83,IY,IS) = REPPRODRESINS( 1:6,IY+1989)
!         T38(84,IY,IS) = FSUM(T38(78,IY,IS), 6)
!         T38(85:87,IY,IS) = REPPRODAGRICHEM( 1:3,IY+1989)
!         T38(88,IY,IS) = FSUM(T38(85,IY,IS), 3)
!       ENDIF

        IL=IL+3

  380 CONTINUE

!     Table 39. Energy Consumption: Glass Industry

      DO 390, IY = 1,LASTYR
!       --- INDUSTRY OUTPUT
        T39( 1,IY,IS) = MC_REVIND(11,28,IY)
!       --- ENERGY CONSUMPTION
        T39( 2,IY,IS) = GLASSCON(ixRF,5,IY)
        T39( 3,IY,IS) = GLASSCON(ixDS,5,IY)
        T39( 4,IY,IS) = GLASSCON(ixLG,5,IY)
        T39( 5,IY,IS) = FSUM(T39(2,IY,IS),3)
        T39( 6,IY,IS) = GLASSCON(ixNG,5,IY)
        T39( 7,IY,IS) = GLASSCON(ixCL,5,IY)
        T39( 8,IY,IS) = GLASSCON(ixEL,5,IY)
        T39( 9,IY,IS) = FSUM(T39(5,IY,IS),4)
!       --- ENERGY CONSUMPTION PER UNIT OF OUTPUT
        IF (MC_REVIND(11,28,IY) .NE. 0.0) THEN
          T39(10,IY,IS) = GLASSCON(ixRF,5,IY) / MC_REVIND(11,28,IY)
          T39(11,IY,IS) = GLASSCON(ixDS,5,IY) / MC_REVIND(11,28,IY)
          T39(12,IY,IS) = GLASSCON(ixLG,5,IY) / MC_REVIND(11,28,IY)
          T39(13,IY,IS) = FSUM(T39(10,IY,IS),3)
          T39(14,IY,IS) = GLASSCON(ixNG,5,IY) / MC_REVIND(11,28,IY)
          T39(15,IY,IS) = GLASSCON(ixCL,5,IY) / MC_REVIND(11,28,IY)
          T39(16,IY,IS) = GLASSCON(ixEL,5,IY) / MC_REVIND(11,28,IY)
        ENDIF
        T39(17,IY,IS) = FSUM(T39(13,IY,IS),4)
        T39(18,IY,IS) = INDCARB(GLASSCON(1,1,IY),IY) ! function indcarb calcs carbon emissions
        IL=18
        INDDIR=10
        T39(19,IY,IS)=CHPINDCAP(1,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T39(20,IY,IS)=CHPINDCAP(2,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T39(21,IY,IS)=CHPINDCAP(3,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T39(22,IY,IS)=sum(CHPINDCAP(4:6,inddir,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T39(23,IY,IS)=CHPINDCAP(7,inddir,iy) ! total across fuels
        IL=IL+5 !23
        T39(24,IY,IS)=CHPINDGEN(1,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T39(25,IY,IS)=CHPINDGEN(2,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T39(26,IY,IS)=CHPINDGEN(3,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T39(27,IY,IS)=sum(CHPINDGEN(4:6,inddir,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T39(28,IY,IS)=CHPINDGEN(7,inddir,iy) ! total across fuels
        IL=IL+5 !28
        T39(29,IY,IS)=CHPINDGEN(7,inddir,iy)*    CHPGRDSHR(inddir,iy) ! Sales to grid
        T39(30,IY,IS)=CHPINDGEN(7,inddir,iy)*(1.-CHPGRDSHR(inddir,iy)) ! Own-Use
        T39(31,IY,IS)=CHPINDSTM(7,inddir,iy) ! total across fuels

        IL=IL+3

  390 CONTINUE

!     Table 40. Energy Consumption: Cement Industry

      DO 400, IY = 1,LASTYR
!       --- INDUSTRY OUTPUT
        T40( 1,IY,IS) = MC_REVIND(11,30,IY) + MC_REVIND(11,31,IY)
!       --- ENERGY CONSUMPTION
        T40( 2,IY,IS) = CEMENTCON(ixRF,5,IY)
        T40( 3,IY,IS) = CEMENTCON(ixDS,5,IY)
        T40(45,IY,IS) = CEMENTCON(ixLG,5,IY)
        T40(39,IY,IS) = CEMENTCON(ixPC,5,IY)
        T40( 4,IY,IS) = CEMENTCON(ixOP,5,IY)
        T40( 5,IY,IS) = FSUM(T40(2,IY,IS),3) + T40(39,IY,IS) + T40(45,IY,IS)
        T40( 6,IY,IS) = CEMENTCON(ixNG,5,IY)
        T40( 7,IY,IS) = CEMENTCON(ixCL,5,IY)
        T40(40,IY,IS) = CEMENTCON(ixMC,5,IY)
        T40(41,IY,IS) = T40(7,IY,IS) + T40(40,IY,IS)
        T40(37,IY,IS) = CEMENTCON(ixRN,5,IY)
        T40( 8,IY,IS) = CEMENTCON(ixEL,5,IY)
        T40( 9,IY,IS) = FSUM(T40(5,IY,IS),4) + T40(40,IY,IS) + T40(37,IY,IS)
!       --- ENERGY CONSUMPTION PER UNIT OF OUTPUT
        IF (MC_REVIND(11,30,IY) .NE. 0.0) THEN
          T40(10,IY,IS) = CEMENTCON(ixRF,5,IY) /(MC_REVIND(11,30,IY) + MC_REVIND(11,31,IY))
          T40(11,IY,IS) = CEMENTCON(ixDS,5,IY) /(MC_REVIND(11,30,IY) + MC_REVIND(11,31,IY))
          T40(46,IY,IS) = CEMENTCON(ixLG,5,IY) /(MC_REVIND(11,30,IY) + MC_REVIND(11,31,IY))
          T40(42,IY,IS) = CEMENTCON(ixPC,5,IY) /(MC_REVIND(11,30,IY) + MC_REVIND(11,31,IY))
          T40(12,IY,IS) = CEMENTCON(ixOP,5,IY) /(MC_REVIND(11,30,IY) + MC_REVIND(11,31,IY))
          T40(13,IY,IS) = FSUM(T40(10,IY,IS),3) + T40(42,IY,IS) + T40(46,IY,IS)
          T40(14,IY,IS) = CEMENTCON(ixNG,5,IY) /(MC_REVIND(11,30,IY) + MC_REVIND(11,31,IY))
          T40(15,IY,IS) = CEMENTCON(ixCL,5,IY) /(MC_REVIND(11,30,IY) + MC_REVIND(11,31,IY))
          T40(43,IY,IS) = CEMENTCON(ixMC,5,IY) /(MC_REVIND(11,30,IY) + MC_REVIND(11,31,IY))
          T40(44,IY,IS) = T40(15,IY,IS) + T40(43,IY,IS)
          T40(38,IY,IS) = CEMENTCON(ixRN,5,IY) /(MC_REVIND(11,30,IY) + MC_REVIND(11,31,IY))
          T40(16,IY,IS) = CEMENTCON(ixEL,5,IY) /(MC_REVIND(11,30,IY) + MC_REVIND(11,31,IY))
        ENDIF
        T40(17,IY,IS) = FSUM(T40(13,IY,IS),4) + T40(43,IY,IS) + T40(38,IY,IS)
        T40(18,IY,IS) = INDCARB(CEMENTCON(1,1,IY),IY)   ! function indcarb calcs combustion CO2 emissions
        T40(47,IY,IS) = GHG_PROCESSIN(11,IY)            ! Cement and lime process CO2 emissions
        IL=18
        INDDIR=11
        T40(19,IY,IS)=CHPINDCAP(1,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T40(20,IY,IS)=CHPINDCAP(2,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T40(21,IY,IS)=CHPINDCAP(3,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T40(22,IY,IS)=sum(CHPINDCAP(4:6,inddir,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T40(23,IY,IS)=CHPINDCAP(7,inddir,iy) ! total across fuels
        IL=IL+5 !23
        T40(24,IY,IS)=CHPINDGEN(1,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T40(25,IY,IS)=CHPINDGEN(2,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T40(26,IY,IS)=CHPINDGEN(3,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T40(27,IY,IS)=sum(CHPINDGEN(4:6,inddir,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T40(28,IY,IS)=CHPINDGEN(7,inddir,iy) ! total across fuels
        IL=IL+5 !28
        T40(29,IY,IS)=CHPINDGEN(7,inddir,iy)*    CHPGRDSHR(inddir,iy) ! Sales to grid
        T40(30,IY,IS)=CHPINDGEN(7,inddir,iy)*(1.-CHPGRDSHR(inddir,iy)) ! Own-Use
        T40(31,IY,IS)=CHPINDSTM(7,inddir,iy) ! total across fuels

        IL=IL+3

  400 CONTINUE

!     Table 41. Energy Consumption: Iron and Steel Industry

      DO 410, IY = 1,LASTYR
!       --- INDUSTRY OUTPUT
        T41( 1,IY,IS) = MC_REVIND(11,33,IY)
!       --- ENERGY CONSUMPTION
        T41( 2,IY,IS) = STEELCON(ixDS,5,IY)
        T41( 3,IY,IS) = STEELCON(ixRF,5,IY)
        T41(47,IY,IS) = STEELCON(ixLG,5,IY)
        T41( 4,IY,IS) = STEELCON(ixOP,5,IY)
        T41( 5,IY,IS) = FSUM(T41(2,IY,IS),3) + T41(47,IY,IS)
        T41( 6,IY,IS) = STEELCON(ixNG,5,IY)
        T41( 7,IY,IS) = STEELCON(ixMC,5,IY)
        T41( 8,IY,IS) = STEELCON(ixCI,5,IY)
        T41( 9,IY,IS) = STEELCON(ixCL,5,IY)
        T41(10,IY,IS) = FSUM(T41(7,IY,IS),3)
        T41(45,IY,IS) = STEELCON(ixRN,5,IY)
        T41(11,IY,IS) = STEELCON(ixEL,5,IY)
        T41(12,IY,IS) = FSUM(T41(5,IY,IS),2) + FSUM(T41(10,IY,IS),2) + T41(45,IY,IS)
!       --- ENERGY CONSUMPTION PER UNIT OF OUTPUT
        IF (MC_REVIND(11,33,IY) .NE. 0.0) THEN
          T41(13,IY,IS) = STEELCON(ixDS,5,IY) / MC_REVIND(11,33,IY)
          T41(14,IY,IS) = STEELCON(ixRF,5,IY) / MC_REVIND(11,33,IY)
          T41(48,IY,IS) = STEELCON(ixLG,5,IY) / MC_REVIND(11,33,IY)
          T41(15,IY,IS) = STEELCON(ixOP,5,IY) / MC_REVIND(11,33,IY)
          T41(16,IY,IS) = FSUM(T41(13,IY,IS),3) + T41(48,IY,IS)
          T41(17,IY,IS) = STEELCON(ixNG,5,IY) / MC_REVIND(11,33,IY)
          T41(18,IY,IS) = STEELCON(ixMC,5,IY) / MC_REVIND(11,33,IY)
          T41(19,IY,IS) = STEELCON(ixCI,5,IY) / MC_REVIND(11,33,IY)
          T41(20,IY,IS) = STEELCON(ixCL,5,IY) / MC_REVIND(11,33,IY)
          T41(21,IY,IS) = FSUM(T41(18,IY,IS),3)
          T41(46,IY,IS) = STEELCON(ixRN,5,IY) / MC_REVIND(11,33,IY)
          T41(22,IY,IS) = STEELCON(ixEL,5,IY) / MC_REVIND(11,33,IY)
        ENDIF
        T41(23,IY,IS) = FSUM(T41(16,IY,IS),2) + FSUM(T41(21,IY,IS),2) + T41(46,IY,IS)
        T41(24,IY,IS) = INDCARB(STEELCON(1,1,IY),IY) ! function indcarb calcs carbon emissions
        IL=24
        INDDIR=12
        T41(25,IY,IS)=CHPINDCAP(1,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T41(26,IY,IS)=CHPINDCAP(2,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T41(27,IY,IS)=CHPINDCAP(3,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T41(28,IY,IS)=sum(CHPINDCAP(4:6,inddir,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T41(29,IY,IS)=CHPINDCAP(7,inddir,iy) ! total across fuels
        IL=IL+5 !29
        T41(30,IY,IS)=CHPINDGEN(1,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T41(31,IY,IS)=CHPINDGEN(2,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T41(32,IY,IS)=CHPINDGEN(3,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T41(33,IY,IS)=sum(CHPINDGEN(4:6,inddir,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T41(34,IY,IS)=CHPINDGEN(7,inddir,iy) ! total across fuels
        IL=IL+5 !34
        T41(35,IY,IS)=CHPINDGEN(7,inddir,iy)*    CHPGRDSHR(inddir,iy) ! Sales to grid
        T41(36,IY,IS)=CHPINDGEN(7,inddir,iy)*(1.-CHPGRDSHR(inddir,iy)) ! Own-Use
        T41(37,IY,IS)=CHPINDSTM(7,inddir,iy) ! total across fuels

        IL=IL+3

  410 CONTINUE

!     Table 42. Energy Consumption: Aluminum Industry

      DO 420, IY = 1,LASTYR
!       --- INDUSTRY OUTPUT
        T42( 1,IY,IS) = MC_REVIND(11,34,IY)
!       --- ENERGY CONSUMPTION
        T42(34,IY,IS) = ALUMCON(ixRF,5,IY)
        T42( 2,IY,IS) = ALUMCON(ixDS,5,IY)
        T42( 3,IY,IS) = ALUMCON(ixLG,5,IY)
        T42( 4,IY,IS) = ALUMCON(ixPC,5,IY)
        T42( 5,IY,IS) = ALUMCON(ixOP,5,IY)
        T42( 6,IY,IS) = FSUM(T42( 2,IY,IS),4) + T42(34,IY,IS)
        T42( 7,IY,IS) = ALUMCON(ixNG,5,IY)
        T42( 8,IY,IS) = ALUMCON(ixCL,5,IY)
        T42(42,IY,IS) = ALUMCON(ixRN,5,IY)
        T42( 9,IY,IS) = ALUMCON(ixEL,5,IY)
        T42(10,IY,IS) = FSUM(T42(6,IY,IS),4) + T42(42,IY,IS)
!       --- ENERGY CONSUMPTION PER UNIT OF OUTPUT
        IF (MC_REVIND(11,34,IY) .NE. 0.0) THEN
          T42(35,IY,IS) = ALUMCON(ixRF,5,IY) / MC_REVIND(11,34,IY)
          T42(11,IY,IS) = ALUMCON(ixDS,5,IY) / MC_REVIND(11,34,IY)
          T42(12,IY,IS) = ALUMCON(ixLG,5,IY) / MC_REVIND(11,34,IY)
          T42(13,IY,IS) = ALUMCON(ixPC,5,IY) / MC_REVIND(11,34,IY)
          T42(14,IY,IS) = ALUMCON(ixOP,5,IY) / MC_REVIND(11,34,IY)
          T42(15,IY,IS) = FSUM(T42(11,IY,IS),4)
          T42(16,IY,IS) = ALUMCON(ixNG,5,IY) / MC_REVIND(11,34,IY)
          T42(17,IY,IS) = ALUMCON(ixCL,5,IY) / MC_REVIND(11,34,IY)
          T42(43,IY,IS) = ALUMCON(ixRN,5,IY) / MC_REVIND(11,34,IY)
          T42(18,IY,IS) = ALUMCON(ixEL,5,IY) / MC_REVIND(11,34,IY)
        ENDIF
        T42(19,IY,IS) = FSUM(T42(15,IY,IS),4) + T42(43,IY,IS)
        T42(20,IY,IS) = INDCARB(ALUMCON(1,1,IY),IY) ! function indcarb calcs carbon emissions
        IL=20
        INDDIR=13
        T42(21,IY,IS)=CHPINDCAP(1,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T42(22,IY,IS)=CHPINDCAP(2,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T42(23,IY,IS)=CHPINDCAP(3,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T42(24,IY,IS)=sum(CHPINDCAP(4:6,inddir,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T42(25,IY,IS)=CHPINDCAP(7,inddir,iy) ! total across fuels
        IL=IL+5 !25
        T42(26,IY,IS)=CHPINDGEN(1,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T42(27,IY,IS)=CHPINDGEN(2,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T42(28,IY,IS)=CHPINDGEN(3,inddir,iy) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T42(29,IY,IS)=sum(CHPINDGEN(4:6,inddir,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
        T42(30,IY,IS)=CHPINDGEN(7,inddir,iy) ! total across fuels
        IL=IL+5 !30
        T42(31,IY,IS)=CHPINDGEN(7,inddir,iy)*    CHPGRDSHR(inddir,iy) ! Sales to grid
        T42(32,IY,IS)=CHPINDGEN(7,inddir,iy)*(1.-CHPGRDSHR(inddir,iy)) ! Own-Use

        IL=IL+3

  420 CONTINUE

!     Table 43. Other Industrial Sector Energy Consumption

      DO 430 IY = 1,LASTYR
!      Agriculture
         T43(60,IY,IS) = AGCON(ixRF,5,IY)
         T43( 1,IY,IS) = AGCON(ixDS,5,IY)
         T43( 2,IY,IS) = AGCON(ixLG,5,IY)
         T43( 3,IY,IS) = AGCON(ixMG,5,IY)
         T43( 4,IY,IS) = AGCON(ixOP,5,IY)
         T43( 5,IY,IS) = FSUM(T43(1,IY,IS),4)+T43(60,IY,IS)
         T43( 6,IY,IS) = AGCON(ixNG,5,IY)
         T43( 7,IY,IS) = AGCON(ixCL,5,IY)
         T43( 8,IY,IS) = AGCON(ixRN,5,IY)
         T43( 9,IY,IS) = AGCON(ixEL,5,IY)
         T43(10,IY,IS) = FSUM(T43(5,IY,IS),5)
         T43(11,IY,IS) = MC_REVIND(11,42,IY)+MC_REVIND(11,43,IY)+MC_REVIND(11,44,IY)
         T43(12,IY,IS) = T43(10,IY,IS) / (MC_REVIND(11,42,IY)+MC_REVIND(11,43,IY)+MC_REVIND(11,44,IY))
         T43(13,IY,IS) = INDCARB(AGCON(1,1,IY),IY) ! function indcarb calcs carbon emissions
!      Construction
         T43(14,IY,IS) = CONSTCON(ixDS,5,IY)
         T43(15,IY,IS) = CONSTCON(ixLG,5,IY)
         T43(16,IY,IS) = CONSTCON(ixMG,5,IY)
         T43(17,IY,IS) = CONSTCON(ixAS,5,IY)
         T43(59,IY,IS) = CONSTCON(ixOP,5,IY)
         T43(18,IY,IS) = FSUM(T43(14,IY,IS),4) + T43(59,IY,IS)
         T43(19,IY,IS) = CONSTCON(ixNG,5,IY)
         T43(20,IY,IS) = CONSTCON(ixCL,5,IY)
         T43(21,IY,IS) = CONSTCON(ixEL,5,IY)
         T43(22,IY,IS) = FSUM(T43(18,IY,IS),4)
         T43(23,IY,IS) = MC_REVIND(11,48,IY)
         T43(24,IY,IS) = T43(22,IY,IS) / MC_REVIND(11,48,IY)
         T43(25,IY,IS) = INDCARB(CONSTCON(1,1,IY),IY) ! function indcarb calcs carbon emissions
!       Mining
         T43(26,IY,IS) = MINECON(ixRF,5,IY)
         T43(27,IY,IS) = MINECON(ixDS,5,IY)
         T43(28,IY,IS) = MINECON(ixMG,5,IY)
         T43(29,IY,IS) = MINECON(ixOP,5,IY)
         T43(30,IY,IS) = FSUM(T43(26,IY,IS),4)
         T43(31,IY,IS) = MINECON(ixNG,5,IY)
         T43(32,IY,IS) = QLPIN(11,IY) * 1000.
         T43(33,IY,IS) = MINECON(ixCL,5,IY)
         T43(34,IY,IS) = MINECON(ixRN,5,IY)
         T43(35,IY,IS) = MINECON(ixEL,5,IY)
         T43(36,IY,IS) = OGELSHALE(IY)
         T43(37,IY,IS) = FSUM(T43(30,IY,IS),7)
         T43(38,IY,IS) = MC_REVIND(11,45,IY)+MC_REVIND(11,46,IY)+MC_REVIND(11,47,IY)
         T43(39,IY,IS) = T43(37,IY,IS) /  &
                (MC_REVIND(11,45,IY)+MC_REVIND(11,46,IY)+MC_REVIND(11,47,IY))
         T43(40,IY,IS) = INDCARB(MINECON(1,1,IY),IY) ! function indcarb calcs carbon emissions
         T43(62,IY,IS) = QNGLQ(11,IY) * 1000.
         T43(63,IY,IS) = QNGLQ(11,IY) * ELPIN(IY)
         IL=40
         v8=(/1,2,3,4,5,6/)  ! vector subscript for industries, used similarly to triplet 1:6
 ! Fuels order: 1=coal, 2=oil, 3=nat gas, 4=wood, 5=other, 6=msw
         T43(41,IY,IS)=sum(CHPINDCAP(1,v8,iy))
         T43(42,IY,IS)=sum(CHPINDCAP(2,v8,iy))
         T43(43,IY,IS)=sum(CHPINDCAP(3,v8,iy))
         T43(44,IY,IS)=sum(CHPINDCAP(4:6,v8,iy))
         T43(45,IY,IS)=sum(CHPINDCAP(7,v8,iy))
         T43(46,IY,IS)=sum(CHPINDGEN(1,v8,iy))
         T43(47,IY,IS)=sum(CHPINDGEN(2,v8,iy))
         T43(48,IY,IS)=sum(CHPINDGEN(3,v8,iy))
         T43(49,IY,IS)=sum(CHPINDGEN(4:6,v8,iy))
         T43(50,IY,IS)=sum(CHPINDGEN(7,v8,iy))
         T43(51,IY,IS)=sum(CHPINDGEN(7,v8,iy)*    CHPGRDSHR(v8,iy))  ! Sales to grid
         T43(52,IY,IS)=sum(CHPINDGEN(7,v8,iy)*(1.-CHPGRDSHR(v8,iy))) ! Own-Use
         T43(53,IY,IS)=sum(CHPINDSTM(7,v8,iy))

  430 CONTINUE

! Table 139 - Metal Based Durables

       DO IY=1,LASTYR
         T139( 1,IY,IS) = FABMETALCON(ixRF,5,IY)
         T139(11,IY,IS) = MACHINECON(ixRF,5,IY)
         T139(21,IY,IS) = COMPUTECON(ixRF,5,IY)
         T139(31,IY,IS) = TRANEQUIPCON(ixRF,5,IY)
         T139(41,IY,IS) = ELECEQUIPON(ixRF,5,IY)

         T139( 2,IY,IS) = FABMETALCON(ixDS,5,IY)
         T139(12,IY,IS) = MACHINECON(ixDS,5,IY)
         T139(22,IY,IS) = COMPUTECON(ixDS,5,IY)
         T139(32,IY,IS) = TRANEQUIPCON(ixDS,5,IY)
         T139(42,IY,IS) = ELECEQUIPON(ixDS,5,IY)

         T139( 3,IY,IS) = FABMETALCON(ixLG,5,IY)
         T139(13,IY,IS) = MACHINECON(ixLG,5,IY)
         T139(23,IY,IS) = COMPUTECON(ixLG,5,IY)
         T139(33,IY,IS) = TRANEQUIPCON(ixLG,5,IY)
         T139(43,IY,IS) = ELECEQUIPON(ixLG,5,IY)

         T139( 4,IY,IS) = FABMETALCON(ixPC,5,IY)
         T139(14,IY,IS) = MACHINECON(ixPC,5,IY)
         T139(24,IY,IS) = COMPUTECON(ixPC,5,IY)
         T139(34,IY,IS) = TRANEQUIPCON(ixPC,5,IY)
         T139(44,IY,IS) = ELECEQUIPON(ixPC,5,IY)

         T139( 5,IY,IS) = FSUM(T139( 1,IY,IS),4)
         T139(15,IY,IS) = FSUM(T139(11,IY,IS),4)
         T139(25,IY,IS) = FSUM(T139(21,IY,IS),4)
         T139(35,IY,IS) = FSUM(T139(31,IY,IS),4)
         T139(45,IY,IS) = FSUM(T139(41,IY,IS),4)

         T139( 6,IY,IS) = FABMETALCON(ixNG,5,IY)
         T139(16,IY,IS) = MACHINECON(ixNG,5,IY)
         T139(26,IY,IS) = COMPUTECON(ixNG,5,IY)
         T139(36,IY,IS) = TRANEQUIPCON(ixNG,5,IY)
         T139(46,IY,IS) = ELECEQUIPON(ixNG,5,IY)

         T139(86,IY,IS) = FABMETALCON(ixMC,5,IY)

         T139( 7,IY,IS) = FABMETALCON(ixCL,5,IY)
         T139(17,IY,IS) = MACHINECON(ixCL,5,IY)
         T139(27,IY,IS) = COMPUTECON(ixCL,5,IY)
         T139(37,IY,IS) = TRANEQUIPCON(ixCL,5,IY)
         T139(47,IY,IS) = ELECEQUIPON(ixCL,5,IY)

         T139( 8,IY,IS) = FABMETALCON(ixRN,5,IY)
         T139(18,IY,IS) = MACHINECON(ixRN,5,IY)
         T139(28,IY,IS) = COMPUTECON(ixRN,5,IY)
         T139(38,IY,IS) = TRANEQUIPCON(ixRN,5,IY)
         T139(48,IY,IS) = ELECEQUIPON(ixRN,5,IY)

         T139( 9,IY,IS) = FABMETALCON(ixEL,5,IY)
         T139(19,IY,IS) = MACHINECON(ixEL,5,IY)
         T139(29,IY,IS) = COMPUTECON(ixEL,5,IY)
         T139(39,IY,IS) = TRANEQUIPCON(ixEL,5,IY)
         T139(49,IY,IS) = ELECEQUIPON(ixEL,5,IY)

         T139(10,IY,IS) = FSUM(T139( 5,IY,IS),5) + T139(86,IY,IS)
         T139(20,IY,IS) = FSUM(T139(15,IY,IS),5)
         T139(30,IY,IS) = FSUM(T139(25,IY,IS),5)
         T139(40,IY,IS) = FSUM(T139(35,IY,IS),5)
         T139(50,IY,IS) = FSUM(T139(45,IY,IS),5)

         T139(51,IY,IS) = MC_REVIND(11,36,IY)
         T139(52,IY,IS) = MC_REVIND(11,37,IY)
         T139(53,IY,IS) = MC_REVIND(11,38,IY)
         T139(54,IY,IS) = MC_REVIND(11,39,IY)
         T139(55,IY,IS) = MC_REVIND(11,40,IY)

         T139(56,IY,IS) = T139(10,IY,IS) / MC_REVIND(11,36,IY)
         T139(57,IY,IS) = T139(20,IY,IS) / MC_REVIND(11,37,IY)
         T139(58,IY,IS) = T139(30,IY,IS) / MC_REVIND(11,38,IY)
         T139(59,IY,IS) = T139(40,IY,IS) / MC_REVIND(11,39,IY)
         T139(60,IY,IS) = T139(50,IY,IS) / MC_REVIND(11,40,IY)

     ! function indcarb calcs carbon emissions
         T139(61,IY,IS) = INDCARB(FABMETALCON(1,1,IY),IY)
         T139(62,IY,IS) = INDCARB(MACHINECON(1,1,IY),IY)
         T139(63,IY,IS) = INDCARB(COMPUTECON(1,1,IY),IY)
         T139(64,IY,IS) = INDCARB(TRANEQUIPCON(1,1,IY),IY)
         T139(65,IY,IS) = INDCARB(ELECEQUIPON(1,1,IY),IY)

         IL=65
         v88=(/14,15,16,17,18/)  ! vector subscript , used similarly to triplet 1:6
         T139(66,IY,IS)=sum(CHPINDCAP(1,v88,iy))
         T139(67,IY,IS)=sum(CHPINDCAP(2,v88,iy))
         T139(68,IY,IS)=sum(CHPINDCAP(3,v88,iy))
         T139(69,IY,IS)=sum(CHPINDCAP(4:6,v88,iy))
         T139(70,IY,IS)=sum(CHPINDCAP(7,v88,iy))
         IL=IL+5
         T139(71,IY,IS)=sum(CHPINDGEN(1,v88,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
         T139(72,IY,IS)=sum(CHPINDGEN(2,v88,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
         T139(73,IY,IS)=sum(CHPINDGEN(3,v88,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
         T139(74,IY,IS)=sum(CHPINDGEN(4:6,v88,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
         T139(75,IY,IS)=sum(CHPINDGEN(7,v88,iy)) ! total across fuels
         IL=IL+5
         T139(76,IY,IS)=sum(CHPINDGEN(7,v88,iy)*    CHPGRDSHR(v88,iy))  ! Sales to grid
         T139(77,IY,IS)=sum(CHPINDGEN(7,v88,iy)*(1.-CHPGRDSHR(v88,iy))) ! Own-Use
         T139(78,IY,IS)=sum(CHPINDSTM(7,v88,iy))
         IL=IL+3
       ENDDO

! Table 140 - Balance of Manufacturing

       DO IY=1,LASTYR
         T140( 1,IY,IS) = WOODPRODCON(ixRF,5,IY)
         T140(11,IY,IS) = PLASTICCON(ixRF,5,IY)
         T140(21,IY,IS) = BOMOTHCON(ixRF,5,IY)

         T140( 2,IY,IS) = WOODPRODCON(ixDS,5,IY)
         T140(12,IY,IS) = PLASTICCON(ixDS,5,IY)
         T140(22,IY,IS) = BOMOTHCON(ixDS,5,IY)

         T140( 3,IY,IS) = WOODPRODCON(ixLG,5,IY)
         T140(13,IY,IS) = PLASTICCON(ixLG,5,IY)
         T140(23,IY,IS) = BOMOTHCON(ixLG,5,IY)

         T140(60,IY,IS) = BOMOTHCON(ixPC,5,IY)
         T140(61,IY,IS) = BOMOTHCON(ixOP,5,IY)

         T140( 4,IY,IS) = FSUM(T140( 1,IY,IS),3)
         T140(14,IY,IS) = FSUM(T140(11,IY,IS),3)
         T140(24,IY,IS) = FSUM(T140(21,IY,IS),3) + FSUM(T140(60,IY,IS),2)

         T140( 5,IY,IS) = WOODPRODCON(ixNG,5,IY)
         T140(15,IY,IS) = PLASTICCON(ixNG,5,IY)
         T140(25,IY,IS) = BOMOTHCON(ixNG,5,IY)

         T140( 6,IY,IS) = WOODPRODCON(ixCL,5,IY)
         T140(16,IY,IS) = PLASTICCON(ixCL,5,IY)
         T140(26,IY,IS) = BOMOTHCON(ixCL,5,IY)

         T140( 7,IY,IS) = WOODPRODCON(ixMC,5,IY)
         T140(17,IY,IS) = PLASTICCON(ixMC,5,IY)
         T140(27,IY,IS) = BOMOTHCON(ixMC,5,IY)

         T140( 8,IY,IS) = WOODPRODCON(ixRN,5,IY)
         T140(18,IY,IS) = PLASTICCON(ixRN,5,IY)
         T140(28,IY,IS) = BOMOTHCON(ixRN,5,IY)

         T140( 9,IY,IS) = WOODPRODCON(ixEL,5,IY)
         T140(19,IY,IS) = PLASTICCON(ixEL,5,IY)
         T140(29,IY,IS) = BOMOTHCON(ixEL,5,IY)

         T140(10,IY,IS) = FSUM(T140( 4,IY,IS),6)
         T140(20,IY,IS) = FSUM(T140(14,IY,IS),6)
         T140(30,IY,IS) = FSUM(T140(24,IY,IS),6)

         T140(31,IY,IS) = MC_REVIND(11, 8,IY)
         T140(32,IY,IS) = MC_REVIND(11,27,IY)
         T140(33,IY,IS) = MC_REVIND(11, 6,IY) + MC_REVIND(11, 7,IY) + &
                          MC_REVIND(11, 9,IY) + MC_REVIND(11,14,IY) + MC_REVIND(11,20,IY) + &
                          MC_REVIND(11,26,IY) + &
                          MC_REVIND(11,32,IY) + &
                          MC_REVIND(11,35,IY) + MC_REVIND(11,41,IY)

         T140(34,IY,IS) = T140(10,IY,IS) / MC_REVIND(11, 8,IY)
         T140(35,IY,IS) = T140(20,IY,IS) / MC_REVIND(11,27,IY)
         T140(36,IY,IS) = T140(30,IY,IS) / &
             (MC_REVIND(11, 6,IY) + MC_REVIND(11, 7,IY) + &
              MC_REVIND(11, 9,IY) + MC_REVIND(11,14,IY) + MC_REVIND(11,20,IY) + &
              MC_REVIND(11,26,IY) + &
              MC_REVIND(11,32,IY) + &
              MC_REVIND(11,35,IY) + MC_REVIND(11,41,IY))

  ! function indcarb calcs carbon emissions
         T140(37,IY,IS) = INDCARB(WOODPRODCON(1,1,IY),IY)
         T140(38,IY,IS) = INDCARB(PLASTICCON(1,1,IY),IY)
         T140(39,IY,IS) = INDCARB(BOMOTHCON(1,1,IY),IY)

         IL=39
         v888=(/19,20,21/)  ! vector subscript , used similarly to triplet 1:6
         T140(40,IY,IS)=sum(CHPINDCAP(1,v888,iy))
         T140(41,IY,IS)=sum(CHPINDCAP(2,v888,iy))
         T140(42,IY,IS)=sum(CHPINDCAP(3,v888,iy))
         T140(43,IY,IS)=sum(CHPINDCAP(4:6,v888,iy))
         T140(44,IY,IS)=sum(CHPINDCAP(7,v888,iy))
         IL=IL+5 !44
         T140(45,IY,IS)=sum(CHPINDGEN(1,v888,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
         T140(46,IY,IS)=sum(CHPINDGEN(2,v888,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
         T140(47,IY,IS)=sum(CHPINDGEN(3,v888,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
         T140(48,IY,IS)=sum(CHPINDGEN(4:6,v888,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
         T140(49,IY,IS)=sum(CHPINDGEN(7,v888,iy)) ! total across fuels
         IL=IL+5 !49
         T140(50,IY,IS)=sum(CHPINDGEN(7,v888,iy)*    CHPGRDSHR(v888,iy))  ! Sales to grid
         T140(51,IY,IS)=sum(CHPINDGEN(7,v888,iy)*(1.-CHPGRDSHR(v888,iy))) ! Own-Use
         T140(52,IY,IS)=sum(CHPINDSTM(7,v888,iy))
         IL=IL+3
       ENDDO

!     Table 44. Industrial Consumption by Sector

      DO 440, IY = 1,LASTYR
!        --- MFG HT & POWER CONSUMPTION
         T44( 1,IY,IS) = MANHP(ixRF,5,IY)
         T44( 2,IY,IS) = MANHP(ixDS,5,IY)
         T44( 3,IY,IS) = MANHP(ixLG,5,IY)
         T44( 4,IY,IS) = MANHP(ixPC,5,IY)
         T44( 5,IY,IS) = MANHP(ixSG,5,IY)
         T44( 6,IY,IS) = MANHP(ixOP,5,IY)
         T44( 7,IY,IS) = FSUM(T44(1,IY,IS),6)
         T44( 8,IY,IS) = MANHP(ixNG,5,IY) - QGTLRF(11,IY) * 1000. - RFQNGPF(11,IY)
         T44(61,IY,IS) = QGTLRF(11,IY) * 1000.
         T44( 9,IY,IS) = MANHP(ixMC,5,IY)
         T44(10,IY,IS) = MANHP(ixCI,5,IY)
         T44(11,IY,IS) = MANHP(ixCL,5,IY)
         T44(12,IY,IS) = FSUM(T44(9,IY,IS),3)
         T44(13,IY,IS) = MANHP(ixRN,5,IY)
         T44(14,IY,IS) = MANHP(ixEL,5,IY)
         T44(56,IY,IS)= 1000. * &
                   ((CORNCD(3,11,IY) * CFCORN / 1000. -0.9751*(CRNETHCD(11,IY)+OTHETHCD(11,IY)) * 365. * CFPET - &
                     RFBIOBUTECD(MNUMCR,IY)*365.*CFBIOBUTE(IY))/1000000. + &
                     sum(BIMQTYCD(1:4,11,IY))/1000000.*RDAYS*(CFVEGGIE(IY)-CFBIOD(IY)) + &
                QBMRFBTL(11,IY)/1000. - RDAYS / 1000000. * &
                (sum(BTLFRAC(1:4,MNUMPR,IY)) * CFBTLLIQ(IY) + &
                 sum(CBTLFRAC(2,1:4,MNUMPR,IY)) * CFCBTLLIQ(2,IY) + &
                 UBAVOL(MNUMPR,IY) * 5.763))
         IF (CONEFF(IY) .NE. 0.0) T44(56,IY,IS) = T44(56,IY,IS) + 1000. * &
                   ((0.9751*CLLETHCD(11,IY) * RDAYS * (CFBMQ(IY) * 42. / CONEFF(IY) - CFPET)) / 1000000.)
         T44(15,IY,IS) = SUM(T44(12:14,IY,IS)) + SUM(T44(7:8,IY,IS)) + T44(56,IY,IS) + T44(61,IY,IS)

!        --- NON-MANUFACTURING
         T44(16,IY,IS) = NONHP(ixRF,5,IY)
         T44(17,IY,IS) = NONHP(ixDS,5,IY)
         T44(18,IY,IS) = NONHP(ixLG,5,IY)
         T44(19,IY,IS) = NONHP(ixMG,5,IY)
         T44(51,IY,IS) = NONHP(ixOP,5,IY)
         T44(20,IY,IS) = FSUM(T44(16,IY,IS),4) + T44(51,IY,IS)
         T44(21,IY,IS) = NONHP(ixNG,5,IY)
         T44(55,IY,IS) = QLPIN(11,IY) * 1000.
         T44(54,IY,IS) = QNGLQ(11,IY) * 1000.
         T44(22,IY,IS) = NONHP(ixCL,5,IY)
         T44(52,IY,IS) = NONHP(ixRN,5,IY)
         T44(23,IY,IS) = NONHP(ixEL,5,IY)
         T44(53,IY,IS) = OGELSHALE(IY)
         T44(24,IY,IS) = FSUM(T44(20,IY,IS),4) + FSUM(T44(52,IY,IS),4)

!        --- FEEDSTOCK & MISC CONSUMPTION
         T44(25,IY,IS) = MANHP(ixNF,5,IY)+NONHP(ixNF,5,IY) + RFQNGPF(11,IY)
         T44(26,IY,IS) = MANHP(ixLF,5,IY)+NONHP(ixLF,5,IY)
         T44(27,IY,IS) = MANHP(ixPF,5,IY)+NONHP(ixPF,5,IY)
         T44(28,IY,IS) = MANHP(ixAS,5,IY)+NONHP(ixAS,5,IY)
         T44(29,IY,IS) = FSUM(T44(25,IY,IS),4)
         T44(30,IY,IS) = T44(15,IY,IS)+T44(24,IY,IS)+T44(29,IY,IS)
!!!      T44(58,IY,IS) = 1000*T2(39,11,IY,IS)                        ! See table 2 assignment
         T44(57,IY,IS) = T44(58,IY,IS)-T44(30,IY,IS)  ! benchmarking and non-itemized relative to Table 2 industrial total

         T44(31,IY,IS)=sum(CHPINDCAP(1,1:21,iy)) + & ! Fuels order: coal,oil,nat gas, wood, other,msw
                         (CGREFCAP(11,IY,1) + CGOGSCAP(11,IY,1)) * .001
         T44(32,IY,IS)=sum(CHPINDCAP(2,1:21,iy)) + & ! Fuels order: coal,oil,nat gas, wood, other,msw
                         (CGREFCAP(11,IY,2) + CGOGSCAP(11,IY,2)) * .001
         T44(33,IY,IS)=sum(CHPINDCAP(3,1:21,iy)) + & ! Fuels order: coal,oil,nat gas, wood, other,msw
                         (CGREFCAP(11,IY,3) + CGOGSCAP(11,IY,3)) * .001
         T44(34,IY,IS)=sum(CHPINDCAP(4:6,1:21,iy)) + & ! Fuels order: coal,oil,nat gas, wood, other,msw
                (CGREFCAP(11,IY,6) + CGREFCAP(11,IY,7) + CGREFCAP(11,IY,9) + CGOGSCAP(11,IY,4)) * .001
         T44(35,IY,IS)=sum(CHPINDCAP(7,1:21,iy)) + & ! total across fuels
                      (sum(CGREFCAP(11,IY,1:12)) + sum(CGOGSCAP(11,IY,1:4))) * .001

         T44(36,IY,IS)=sum(CHPINDGEN(1,1:21,iy)) + & ! Fuels order: coal,oil,nat gas, wood, other,msw
                          (CGREFGEN(11,IY,1,1) + CGREFGEN(11,IY,1,2)) * .001 + &
                          (CGOGSGEN(11,IY,1,1) + CGOGSGEN(11,IY,1,2)) * .001
         T44(37,IY,IS)=sum(CHPINDGEN(2,1:21,iy)) + & ! Fuels order: coal,oil,nat gas, wood, other,msw
                          (CGREFGEN(11,IY,2,1) + CGREFGEN(11,IY,2,2)) * .001 + &
                          (CGOGSGEN(11,IY,2,1) + CGOGSGEN(11,IY,2,2)) * .001
         T44(38,IY,IS)=sum(CHPINDGEN(3,1:21,iy)) + & ! Fuels order: coal,oil,nat gas, wood, other,msw
                          (CGREFGEN(11,IY,3,1) + CGREFGEN(11,IY,3,2)) * .001 + &
                          (CGOGSGEN(11,IY,3,1) + CGOGSGEN(11,IY,3,2)) * .001
         T44(39,IY,IS)=sum(CHPINDGEN(4:6,1:21,iy)) + & ! Fuels order: coal,oil,nat gas, wood, other,msw
                          (CGREFGEN(11,IY,6,1) + CGREFGEN(11,IY,6,2) + &
                           CGREFGEN(11,IY,7,1) + CGREFGEN(11,IY,7,2) + &
                           CGREFGEN(11,IY,9,1) + CGREFGEN(11,IY,9,2)) * .001 + &
                          (CGOGSGEN(11,IY,4,1) + CGOGSGEN(11,IY,4,2)) * .001
         T44(40,IY,IS)=sum(CHPINDGEN(7,1:21,iy)) + & ! total across fuels
                       sum(CGREFGEN(11,IY,1:12,1:2)) * .001 + &
                       sum(CGOGSGEN(11,IY,1:4,1:2)) * .001


         T44(41,IY,IS)=(sum(CGINDLGEN(11,iy,1:12,1)) + sum(CGREFGEN(11,iy,1:12,1)) + &   ! Sales to grid
                        sum(CGOGSGEN(11,IY,1:4,1))) * .001
         T44(42,IY,IS)=(sum(CGINDLGEN(11,iy,1:12,2)) + sum(CGREFGEN(11,iy,1:12,2)) + &   ! Own use
                        sum(CGOGSGEN(11,IY,1:4,2))) * .001
         T44(43,IY,IS)=sum(CHPINDSTM(7,1:21,iy))
  440 CONTINUE

! Calls the following routines only if any tables requested
!  The following won't work for two reasons:
!     1.  REPORT_SWITCH is not yet initialized
!     2.  GRAF2000 will be affected - it needs all tables run
!     IF (ANY(REPORT_SWITCH(60:99))) CALL FDATA2(IS)
!     IF (ANY(REPORT_SWITCH(100:150))) CALL FDATA3(IS)
      CALL FDATA1A(IS)
      CALL FDATA2(IS)
      CALL FDATA3(IS)
!  call this (FDATA31) after all the tables are done.  it references table arrays like table 46.
      CALL FDATA31

9999  CONTINUE        ! Scenario loop

      RETURN

      CONTAINS
      SUBROUTINE FDATA31
!
!     Table 31. Energy Index Table
!
!  Index Numbering Scheme for "T31" Array
!  Energy / RGDP Value                   --  1
!  Energy / RGDP Index                   --  8
!  Carbon / RGDP Value                   -- 14
!  Carbon / RGDP Index                   -- 15
!  Aggregate Composite Energy Efficiency --  7
!  Aggregate Composite Carbon Efficiency -- 13

!  Residential Energy Efficiency         --  2
!  Commercial Energy Efficiency          --  3
!  Industrial Energy Efficiency          --  4
!  Transportation Energy Efficiency      --  5
!  Electric Generation Energy Efficiency --  6

!  Residential Carbon Efficiency         --  9
!  Commercial Carbon Efficiency          -- 10
!  Industrial Carbon Efficiency          -- 11
!  Transportation Carbon Efficiency      -- 12

! Diagnostic Reporting
       Iprintflag=99 ! Dump all calculation data for replication
       Iprintfile=19 ! Printing Unit File Number
       Iprintflag2=1 ! Annual Diagnostics for total energy consumption and index weight checking
       Iprintflag=1  ! For first time through, print table of all years data for inspection
       Iprintflag=0  ! No diagnositcs
       Iprintflag2=0 ! No Annual Diagnostics

! Set Start Year
       IndexStartYear=28      !NEMS BaseYr is currently 1990 with an index value of 1, 28 = 2017
                              ! IndexStartYear is the first calculated year for the index (i.e., the year after it is 1.0)
                              ! First calculable year is 2016, or index year 27, since the RECS year is now 2015
                              !   and detailed EU data not available till 2016 the first detailed calc year for residential.
       T31(1:16,1:IndexStartYear-1,IS)=1.     !set index for year before calc start
       indcrb(1:13,1:10,1:2)=0.0
       inddriver(1:13,1:2)  =0.0

! Table 2 Energy Statistics Line Lookups
       itotal_nbr=78          !use totals primary from Table 2 (includes utility losses)
       itotal_nbr_res=9       !from Table 2
       itotal_nbr_com=22      !from Table 2
       itotal_nbr_ind=39      !from Table 2
       itotal_nbr_tran=55     !from Table 2
       itotal_nbr_util=77     !from Table 2

! Table 17 Carbon Statistics Line Lookups
       itotal_nbr_carb=29          !use total from Table 17
       itotal_nbr_carb_res=5       !from Table 17
       itotal_nbr_carb_com=10      !from Table 17
       itotal_nbr_carb_ind=15      !from Table 17
       itotal_nbr_carb_tran=19     !from Table 17
       itotal_nbr_carb_util=24     !from Table 17

        CarbCnvFac=1. !initialize for carbon (instead of CO2) switch to CO2 below if needed)
!        IF (TRIM(CARBON_OR_2) .EQ. 'CO2') CarbCnvFac = 44./12.

      DO IY=1,LASTYR
! Initialize Diagnostic Rollups
        Res_energy=0. ; Res_carbon=0.
        Com_energy=0. ; Com_carbon=0.
        Ind_energy=0. ; Ind_carbon=0.
        Trn_energy=0. ; Trn_carbon=0.
        Utl_energy=0. ; Utl_carbon=0.


!  --- Energy Efficiency Index Calculations ("Inverse Efficiency" to compare against intensities)

! Store Total Primary Energy
        PRIMARY =((QTPAS(11,IY) + QNGAS(11,IY)+QGPTR(11,IY)+QLPIN(11,IY)+QNGLQ(11,IY)+ QCLAS(11,IY)+QMCIN(11,IY)+ &
            QUREL(11,IY)+ QTRAS(11,IY)+ QEIEL(11,IY)+QCIIN(11,IY)+  QMETR(11,IY))) * 1000.

! Compute Ratios to RGDP for Energy and Carbon
        T31( 1,IY,IS) = T2(113,11,IY,IS) !kBtu / dollar of RGDP
        T31(14,IY,IS) = T2(114,11,IY,IS) !mtcO2e or mtce (depending on ftab choice) / millions of RGDP

        if(iy >= IndexStartYear) then ! Begin Sectoral Calculations
         ! Rebase aggregate indicators
           IF (T31( 1,IndexStartYear-1,IS) .GE. 0.0) T31( 8,IY,IS) = T31( 1,IY,IS)/T31( 1,IndexStartYear-1,IS)   !primary energy
           IF (T31(14,IndexStartYear-1,IS) .GE. 0.0) T31(15,IY,IS) = T31(14,IY,IS)/T31(14,IndexStartYear-1,IS)   !carbon

         ! Start index calcs in year 16 -- data not available for all sectors before 2004 (CBECS year + 1)

           T31(2: 7,IY,IS)=0. ! To be used as accumulators for the duration of these calcs (no need to accumulate Carbon/RGDP (8))
           T31(9:13,IY,IS)=0. ! To be used as accumulators for the duration of these calcs

!  Check for CO2 vs Carbon (CO2 factor around 190, carbon around 52)
! T17.L30 represents Electric Generation Emissions and T2.L25 is end use Electricity Consumption
        ElecCarb_CY=T17(24,11,IY,IS)/T2(74,11,IY,IS)
        ElecCarb_PY=T17(24,11,IY-1,IS)/T2(74,11,IY-1,IS)
!  Carbon factors from emeblk are pure carbon, if electric utilities factor indicates CO2, then scale for consistency
        If (ElecCarb_CY > 100. .or. CarbCnvFac > 1.) CarbCnvFac=44./12.  !(Once doing CO2, don't change back)

        if(iprintflag==1 .OR. iprintflag==99) then
           Write (iprintfile,*) "ElecCarb_CY", ElecCarb_CY, "ElecCarb_PY", ElecCarb_PY, iy
           Write (iprintfile,*) "ElecCarb_CY", ElecCarb_CY, "Alternate Compute", CarbCnvFac, iy+1989
         endif

!
!   Residential Energy Segment
!
        if(iprintflag==1 .OR. iprintflag==99) Write (iprintfile,*) "Residential Energy Diagnostics for year ", iy

        do IR=1,9
          do IB=1,3
            driver_cy=1.  !residential drivers are accounted for in the "intensity" variables passed from RESD
            driver_py=1.

!       --- Electricity
            rs(1) =(HTRCONWT(IY,2,IR,IB) + SHTCONWT(IY,2,IR,IB)) / 1000000000.
            rs(2) = COOLCNWT(IY,1,IR,IB) / 1000000000.
            rs(3) = H2OCONWT(IY,2,IR,IB) / 1000000000.
            rs(4) = REFCONWT(IY,IR,IB)   / 1000000000.
            rs(5) = CKCONWT(IY,3,IR,IB)  / 1000000000.
            rs(6) = DRYCONWT(IY,2,IR,IB) / 1000000000.
            rs(7) = FRZCONWT(IY,IR,IB)   / 1000000000.
            rs(8) = LTCONWT(IY,IR,IB)    / 1000000000.
            rs(9) = CSWCONWT(IY,IR,IB)   / 1000000000.
            rs(10) = DSWCONWT(IY,IR,IB)  / 1000000000.
            rs(11) = TVRCONWT(IY,IR,IB)   / 1000000000.
            rs(12) = PCRCONWT(IY,IR,IB)   / 1000000000.
            rs(13) = FANCONWT(IY,IR,IB)  / 1000000000.
            rs(14) = APCONWT(IY,IR,IB)   / 1000000000.

!       --- NATURAL GAS
            rs(15) =(HTRCONWT(IY,1,IR,IB) + SHTCONWT(IY,1,IR,IB)) / 1000000000.
            rs(16) = COOLCNWT(IY,3,IR,IB) / 1000000000.
            rs(17) = H2OCONWT(IY,1,IR,IB) / 1000000000.
            rs(18) = CKCONWT(IY,1,IR,IB)  / 1000000000.
            rs(19) = DRYCONWT(IY,1,IR,IB) / 1000000000.
            rs(20) = APLCONWT(IY,1,IR,IB) / 1000000000.

!        --- DISTILLATE
            rs(21) =(HTRCONWT(IY,3,IR,IB) + SHTCONWT(IY,3,IR,IB)) / 1000000000.
            rs(22) = H2OCONWT(IY,3,IR,IB) / 1000000000.
            rs(23) = APLCONWT(IY,3,IR,IB) / 1000000000.

!       --- LIQUEFIED PETROLEUM GAS
            rs(24) =(HTRCONWT(IY,4,IR,IB) + SHTCONWT(IY,4,IR,IB)) / 1000000000.
            rs(25) = H2OCONWT(IY,4,IR,IB) / 1000000000.
            rs(26) = CKCONWT(IY,2,IR,IB)  / 1000000000.
            rs(27) = APLCONWT(IY,2,IR,IB) / 1000000000.

!      --- MARKETED RENEWABLES
            rs(28) =(HTRCONWT(IY,6,IR,IB) + shtconwt(iy,7,ir,ib)) / 1000000000.
!      --- OTHER FUELS
            rs(29) = QKSRS(IR,IY)/3. ! KEROSENE

 !       --- Electricity
            rsef(1 ) =(HTRCONIN(IY,2,IR,IB) + SHTCONIN(IY,2,IR,IB))
            rsef(2 ) = COOLCNIN(IY,1,IR,IB)
            rsef(3 ) = H2OCONIN(IY,2,IR,IB)
            rsef(4 ) = REFCONIN(IY,IR,IB)
            rsef(5 ) = CKCONIN(IY,3,IR,IB)
            rsef(6 ) = DRYCONIN(IY,2,IR,IB)
            rsef(7 ) = FRZCONIN(IY,IR,IB)
            rsef(8 ) = LTCONIN(IY,IR,IB)
            rsef(9 ) = CSWCONIN(IY,IR,IB)
            rsef(10) = DSWCONIN(IY,IR,IB)
            rsef(11) = TVRCONIN(IY,IR,IB)
            rsef(12) = PCRCONIN(IY,IR,IB)
            rsef(13) = FANCONIN(IY,IR,IB)
            rsef(14) = APCONIN(IY,IR,IB)

!       --- NATURAL GAS
            rsef(15) =(HTRCONIN(IY,1,IR,IB) + SHTCONIN(IY,1,IR,IB))
            rsef(16) = COOLCNIN(IY,3,IR,IB)
            rsef(17) = H2OCONIN(IY,1,IR,IB)
            rsef(18) = CKCONIN(IY,1,IR,IB)
            rsef(19) = DRYCONIN(IY,1,IR,IB)
            rsef(20) = APLCONIN(IY,1,IR,IB)

!        --- DISTILLATE
            rsef(21) =(HTRCONIN(IY,3,IR,IB) + SHTCONIN(IY,3,IR,IB))
            rsef(22) = H2OCONIN(IY,3,IR,IB)
            rsef(23) = APLCONIN(IY,3,IR,IB)

!       --- LIQUEFIED PETROLEUM GAS
            rsef(24) =(HTRCONIN(IY,4,IR,IB) + SHTCONIN(IY,4,IR,IB))
            rsef(25) = H2OCONIN(IY,4,IR,IB)
            rsef(26) = CKCONIN(IY,2,IR,IB)
            rsef(27) = APLCONIN(IY,2,IR,IB)

!      --- MARKETED RENEWABLES
            rsef(28) =(HTRCONIN(IY,6,IR,IB) + shtconin(iy,7,ir,ib))
!      --- OTHER FUELS
            rsef(29) = QKSRS(IR,IY)/T4(4,IY,IS)*1000.   ! KEROSENE per HH

!      --- Electricity
            rsly(1 ) =(HTRCONWT(IY-1,2,IR,IB) + SHTCONWT(IY-1,2,IR,IB)) / 1000000000.
            rsly(2 ) = COOLCNWT(IY-1,1,IR,IB) / 1000000000.
            rsly(3 ) = H2OCONWT(IY-1,2,IR,IB) / 1000000000.
            rsly(4 ) = REFCONWT(IY-1,IR,IB)   / 1000000000.
            rsly(5 ) = CKCONWT(IY-1,3,IR,IB)  / 1000000000.
            rsly(6 ) = DRYCONWT(IY-1,2,IR,IB) / 1000000000.
            rsly(7 ) = FRZCONWT(IY-1,IR,IB)   / 1000000000.
            rsly(8 ) = LTCONWT(IY-1,IR,IB)    / 1000000000.
            rsly(9 ) = CSWCONWT(IY-1,IR,IB)   / 1000000000.
            rsly(10) = DSWCONWT(IY-1,IR,IB)   / 1000000000.
            rsly(11) = TVRCONWT(IY-1,IR,IB)    / 1000000000.
            rsly(12) = PCRCONWT(IY-1,IR,IB)    / 1000000000.
            rsly(13) = FANCONWT(IY-1,IR,IB)   / 1000000000.
            rsly(14) = APCONWT(IY-1,IR,IB)    / 1000000000.

!       --- NATURAL GAS
            rsly(15) =(HTRCONWT(IY-1,1,IR,IB) + SHTCONWT(IY-1,1,IR,IB)) / 1000000000.
            rsly(16) = COOLCNWT(IY-1,3,IR,IB) / 1000000000.
            rsly(17) = H2OCONWT(IY-1,1,IR,IB) / 1000000000.
            rsly(18) = CKCONWT(IY-1,1,IR,IB)  / 1000000000.
            rsly(19) = DRYCONWT(IY-1,1,IR,IB) / 1000000000.
            rsly(20) = APLCONWT(IY-1,1,IR,IB) / 1000000000.

!        --- DISTILLATE
            rsly(21) =(HTRCONWT(IY-1,3,IR,IB) + SHTCONWT(IY-1,3,IR,IB)) / 1000000000.
            rsly(22) = H2OCONWT(IY-1,3,IR,IB) / 1000000000.
            rsly(23) = APLCONWT(IY-1,3,IR,IB) / 1000000000.

!       --- LIQUEFIED PETROLEUM GAS
            rsly(24) =(HTRCONWT(IY-1,4,IR,IB) + SHTCONWT(IY-1,4,IR,IB)) / 1000000000.
            rsly(25) = H2OCONWT(IY-1,4,IR,IB) / 1000000000.
            rsly(26) = CKCONWT(IY-1,2,IR,IB)  / 1000000000.
            rsly(27) = APLCONWT(IY-1,2,IR,IB) / 1000000000.

!      --- MARKETED RENEWABLES
            rsly(28) = (HTRCONWT(IY-1,6,IR,IB) + shtconwt(IY-1,7,ir,ib)) / 1000000000.
!      --- OTHER FUELS
            rsly(29) = QKSRS(IR,IY-1)/3. ! KEROSENE

 !       --- Electricity
            rsefly(1 ) =(HTRCONIN(IY-1,2,IR,IB) + SHTCONIN(IY-1,2,IR,IB))
            rsefly(2 ) = COOLCNIN(IY-1,1,IR,IB)
            rsefly(3 ) = H2OCONIN(IY-1,2,IR,IB)
            rsefly(4 ) = REFCONIN(IY-1,IR,IB)
            rsefly(5 ) = CKCONIN(IY-1,3,IR,IB)
            rsefly(6 ) = DRYCONIN(IY-1,2,IR,IB)
            rsefly(7 ) = FRZCONIN(IY-1,IR,IB)
            rsefly(8 ) = LTCONIN(IY-1,IR,IB)
            rsefly(9 ) = CSWCONIN(IY-1,IR,IB)
            rsefly(10) = DSWCONIN(IY-1,IR,IB)
            rsefly(11) = TVRCONIN(IY-1,IR,IB)
            rsefly(12) = PCRCONIN(IY-1,IR,IB)
            rsefly(13) = FANCONIN(IY-1,IR,IB)
            rsefly(14) = APCONIN(IY-1,IR,IB)

!       --- NATURAL GAS
            rsefly(15) =(HTRCONIN(IY-1,1,IR,IB) + SHTCONIN(IY-1,1,IR,IB))
            rsefly(16) = COOLCNIN(IY-1,3,IR,IB)
            rsefly(17) = H2OCONIN(IY-1,1,IR,IB)
            rsefly(18) = CKCONIN(IY-1,1,IR,IB)
            rsefly(19) = DRYCONIN(IY-1,1,IR,IB)
            rsefly(20) = APLCONIN(IY-1,1,IR,IB)

!        --- DISTILLATE
            rsefly(21) =(HTRCONIN(IY-1,3,IR,IB) + SHTCONIN(IY-1,3,IR,IB))
            rsefly(22) = H2OCONIN(IY-1,3,IR,IB)
            rsefly(23) = APLCONIN(IY-1,3,IR,IB)

!       --- LIQUEFIED PETROLEUM GAS
            rsefly(24) =(HTRCONIN(IY-1,4,IR,IB) + SHTCONIN(IY-1,4,IR,IB))
            rsefly(25) = H2OCONIN(IY-1,4,IR,IB)
            rsefly(26) = CKCONIN(IY-1,2,IR,IB)
            rsefly(27) = APLCONIN(IY-1,2,IR,IB)

!      --- MARKETED RENEWABLES
            rsefly(28) = (HTRCONIN(IY-1,6,IR,IB) + shtconin(IY-1,7,ir,ib))
!      --- OTHER FUELS
            rsefly(29) = QKSRS(IR,IY-1)/T4(4,IY-1,IS) * 1000. ! KEROSENE per HH

            do iline_nbr = 1,29

                    if(iprintflag==1 .OR. iprintflag==99) then
                        write (iprintfile,97) "ResdTrill",IY,IR,IB,iline_nbr,  &
                                               rs(iline_nbr),   t2(itotal_nbr_res,11,iy,is),    &
                                               rsly(iline_nbr), t2(itotal_nbr_res,11,iy-1,is),  &
                                               rsef(iline_nbr),   driver_cy,   &
                                               rsefly(iline_nbr), driver_py
                    endif

              if (t2(itotal_nbr,11,iy,is)<=0.)   cycle
              if (t2(itotal_nbr,11,iy-1,is)<=0.) cycle
              if (rsef(iline_nbr)<=0.)           cycle
              if (rsefly(iline_nbr)<=0.)         cycle
              if (driver_cy<=0.)                 cycle
              if (driver_py<=0.)                 cycle

! Diagnostic rollup of residential energy
              Res_energy = Res_energy + rs(iline_nbr)
              if(isnan((rsef(iline_nbr)/driver_cy)/(rsefly(iline_nbr)/driver_py))) then
                T31(7,IY,IS)=-10. ! perturb index if NAN are encountered
                T31(2,IY,IS)=-10.
               else

                T31(7,IY,IS)= T31(7,IY,IS) + &
                  0.5*(rs(iline_nbr)/t2(itotal_nbr,11,iy,is)+ &
                        rsly(iline_nbr)/t2(itotal_nbr,11,iy-1,is)) &
                      * log((rsef(iline_nbr)/driver_cy)/(rsefly(iline_nbr)/driver_py))

                T31(2,IY,IS)= T31(2,IY,IS) + &
                  0.5*( rs(iline_nbr)/t2(itotal_nbr_res,11,iy,is)+ &
                        rsly(iline_nbr)/t2(itotal_nbr_res,11,iy-1,is)) &
                      * log((rsef(iline_nbr)/driver_cy)/(rsefly(iline_nbr)/driver_py))
               endif

            enddo  ! iline_nbr for end use fuel combinations
          enddo   ! for building types
        enddo    ! IR for census divisions

              if(iprintflag==1 .OR. iprintflag==99) Write (iprintfile,*) "Residential Energy ",Res_Energy, " IY ", IY+1989, " FTAB ", t2(itotal_nbr_res,11,iy,is)


!
!Residential Carbon Segment
!
        do IR=1,9
          do IB=1,3
            driver_cy=1.
            driver_py=1.

!       --- Electricity
            rs(1) = (HTRCONWT(IY,2,IR,IB) + SHTCONWT(IY,2,IR,IB)) / 1000000000. * ElecCarb_CY
            rs(2) =  COOLCNWT(IY,1,IR,IB) / 1000000000.*ElecCarb_CY
            rs(3) =  H2OCONWT(IY,2,IR,IB) / 1000000000.*ElecCarb_CY
            rs(4) =  REFCONWT(IY,IR,IB)   / 1000000000.*ElecCarb_CY
            rs(5) =  CKCONWT(IY,3,IR,IB)  / 1000000000.*ElecCarb_CY
            rs(6) =  DRYCONWT(IY,2,IR,IB) / 1000000000.*ElecCarb_CY
            rs(7) =  FRZCONWT(IY,IR,IB)   / 1000000000.*ElecCarb_CY
            rs(8) =  LTCONWT(IY,IR,IB)    / 1000000000.*ElecCarb_CY
            rs(9) =  CSWCONWT(IY,IR,IB)   / 1000000000.*ElecCarb_CY
            rs(10) = DSWCONWT(IY,IR,IB)   / 1000000000.*ElecCarb_CY
            rs(11) = TVRCONWT(IY,IR,IB)    / 1000000000.*ElecCarb_CY
            rs(12) = PCRCONWT(IY,IR,IB)    / 1000000000.*ElecCarb_CY
            rs(13) = FANCONWT(IY,IR,IB)   / 1000000000.*ElecCarb_CY
            rs(14) = APCONWT(IY,IR,IB)    / 1000000000.*ElecCarb_CY

!       --- NATURAL GAS
            rs(15) = (HTRCONWT(IY,1,IR,IB) + SHTCONWT(IY,1,IR,IB)) / 1000000000. * engrs(iy)*CarbCnvFac
            rs(16) = COOLCNWT(IY,3,IR,IB) / 1000000000.*engrs(iy)*CarbCnvFac
            rs(17) = H2OCONWT(IY,1,IR,IB) / 1000000000.*engrs(iy)*CarbCnvFac
            rs(18) = CKCONWT(IY,1,IR,IB)  / 1000000000.*engrs(iy)*CarbCnvFac
            rs(19) = DRYCONWT(IY,1,IR,IB) / 1000000000.*engrs(iy)*CarbCnvFac
            rs(20) = APLCONWT(IY,1,IR,IB) / 1000000000.*engrs(iy)*CarbCnvFac

!        --- DISTILLATE
            rs(21) = (HTRCONWT(IY,3,IR,IB) + SHTCONWT(IY,3,IR,IB)) / 1000000000. * edsrs(iy)*CarbCnvFac
            rs(22) = H2OCONWT(IY,3,IR,IB) / 1000000000.*edsrs(iy)*CarbCnvFac
            rs(23) = APLCONWT(IY,3,IR,IB) / 1000000000.*edsrs(iy)*CarbCnvFac

!       --- LIQUEFIED PETROLEUM GAS
            rs(24) = (HTRCONWT(IY,4,IR,IB) + SHTCONWT(IY,4,IR,IB)) / 1000000000. * elgrs(iy)*CarbCnvFac
            rs(25) = H2OCONWT(IY,4,IR,IB) / 1000000000.*elgrs(iy)*CarbCnvFac
            rs(26) = CKCONWT(IY,2,IR,IB)  / 1000000000.*engrs(iy)*CarbCnvFac
            rs(27) = APLCONWT(IY,2,IR,IB) / 1000000000.*engrs(iy)*CarbCnvFac

!      --- MARKETED RENEWABLES
            rs(28) = (HTRCONWT(IY,6,IR,IB) + shtconwt(iy,7,ir,ib))/ 1000000000. * 0.0
!      --- OTHER FUELS
            rs(29) = (QKSRS(IR,IY)*eksrs(iy) + QCLRS(IR,IY)*eclrs(iy))/3.*CarbCnvFac ! KEROSENE + COAL  (divide by 3 for building types)

 !       --- Electricity
            rsef(1 ) = (HTRCONIN(IY,2,IR,IB) + SHTCONIN(IY,2,IR,IB))  * ElecCarb_CY
            rsef(2 ) = COOLCNIN(IY,1,IR,IB)  * ElecCarb_CY
            rsef(3 ) = H2OCONIN(IY,2,IR,IB)  * ElecCarb_CY
            rsef(4 ) = REFCONIN(IY,IR,IB)    * ElecCarb_CY
            rsef(5 ) = CKCONIN(IY,3,IR,IB)   * ElecCarb_CY
            rsef(6 ) = DRYCONIN(IY,2,IR,IB)  * ElecCarb_CY
            rsef(7 ) = FRZCONIN(IY,IR,IB)    * ElecCarb_CY
            rsef(8 ) = LTCONIN(IY,IR,IB)     * ElecCarb_CY
            rsef(9 ) = CSWCONIN(IY,IR,IB)    * ElecCarb_CY
            rsef(10) = DSWCONIN(IY,IR,IB)    * ElecCarb_CY
            rsef(11) = TVRCONIN(IY,IR,IB)     * ElecCarb_CY
            rsef(12) = PCRCONIN(IY,IR,IB)     * ElecCarb_CY
            rsef(13) = FANCONIN(IY,IR,IB)    * ElecCarb_CY
            rsef(14) = APCONIN(IY,IR,IB)     * ElecCarb_CY

!       --- NATURAL GAS
            rsef(15) = (HTRCONIN(IY,1,IR,IB) + SHTCONIN(IY,1,IR,IB))  * engrs(iy)*CarbCnvFac
            rsef(16) = COOLCNIN(IY,3,IR,IB)  * engrs(iy)*CarbCnvFac
            rsef(17) = H2OCONIN(IY,1,IR,IB)  * engrs(iy)*CarbCnvFac
            rsef(18) = CKCONIN(IY,1,IR,IB)   * engrs(iy)*CarbCnvFac
            rsef(19) = DRYCONIN(IY,1,IR,IB)  * engrs(iy)*CarbCnvFac
            rsef(20) = APLCONIN(IY,1,IR,IB)  * engrs(iy)*CarbCnvFac

!        --- DISTILLATE
            rsef(21) = (HTRCONIN(IY,3,IR,IB) + SHTCONIN(IY,3,IR,IB))  * edsrs(iy)*CarbCnvFac
            rsef(22) = H2OCONIN(IY,3,IR,IB)  * edsrs(iy)*CarbCnvFac
            rsef(23) = APLCONIN(IY,3,IR,IB)  * edsrs(iy)*CarbCnvFac

!       --- LIQUEFIED PETROLEUM GAS
            rsef(24) = (HTRCONIN(IY,4,IR,IB)+SHTCONIN(IY,4,IR,IB))  * elgrs(iy)*CarbCnvFac
            rsef(25) = H2OCONIN(IY,4,IR,IB)  * elgrs(iy)*CarbCnvFac
            rsef(26) = CKCONIN(IY,2,IR,IB)   * elgrs(iy)*CarbCnvFac
            rsef(27) = APLCONIN(IY,2,IR,IB)  * elgrs(iy)*CarbCnvFac

!      --- MARKETED RENEWABLES
            rsef(28) = (HTRCONIN(IY,6,IR,IB)+shtconin(iy,7,ir,ib)) *0.0
!      --- OTHER FUELS
            rsef(29) = (QKSRS(IR,IY)*eksrs(iy) + QCLRS(IR,IY)*eclrs(iy))*1000./T4(4,IY,IS)*CarbCnvFac ! KEROSENE + COAL per HH

 !       --- Electricity
            rsly(1 ) = (HTRCONWT(IY-1,2,IR,IB) + SHTCONWT(IY-1,2,IR,IB)) / 1000000000. * ElecCarb_PY
            rsly(2 ) = COOLCNWT(IY-1,1,IR,IB) / 1000000000. * ElecCarb_PY
            rsly(3 ) = H2OCONWT(IY-1,2,IR,IB) / 1000000000. * ElecCarb_PY
            rsly(4 ) = REFCONWT(IY-1,IR,IB)   / 1000000000. * ElecCarb_PY
            rsly(5 ) = CKCONWT(IY-1,3,IR,IB)  / 1000000000. * ElecCarb_PY
            rsly(6 ) = DRYCONWT(IY-1,2,IR,IB) / 1000000000. * ElecCarb_PY
            rsly(7 ) = FRZCONWT(IY-1,IR,IB)   / 1000000000. * ElecCarb_PY
            rsly(8 ) = LTCONWT(IY-1,IR,IB)    / 1000000000. * ElecCarb_PY
            rsly(9 ) = CSWCONWT(IY-1,IR,IB)   / 1000000000. * ElecCarb_PY
            rsly(10) = DSWCONWT(IY-1,IR,IB)   / 1000000000. * ElecCarb_PY
            rsly(11) = TVRCONWT(IY-1,IR,IB)    / 1000000000. * ElecCarb_PY
            rsly(12) = PCRCONWT(IY-1,IR,IB)    / 1000000000. * ElecCarb_PY
            rsly(13) = FANCONWT(IY-1,IR,IB)   / 1000000000. * ElecCarb_PY
            rsly(14) = APCONWT(IY-1,IR,IB)    / 1000000000. * ElecCarb_PY

!       --- NATURAL GAS
            rsly(15) = (HTRCONWT(IY-1,1,IR,IB) + SHTCONWT(IY-1,1,IR,IB)) / 1000000000. * engrs(iy-1)*CarbCnvFac
            rsly(16) = COOLCNWT(IY-1,3,IR,IB) / 1000000000. * engrs(iy-1)*CarbCnvFac
            rsly(17) = H2OCONWT(IY-1,1,IR,IB) / 1000000000. * engrs(iy-1)*CarbCnvFac
            rsly(18) = CKCONWT(IY-1,1,IR,IB)  / 1000000000. * engrs(iy-1)*CarbCnvFac
            rsly(19) = DRYCONWT(IY-1,1,IR,IB) / 1000000000. * engrs(iy-1)*CarbCnvFac
            rsly(20) = APLCONWT(IY-1,1,IR,IB) / 1000000000. * engrs(iy-1)*CarbCnvFac

!        --- DISTILLATE
            rsly(21) = (HTRCONWT(IY-1,3,IR,IB) + SHTCONWT(IY-1,3,IR,IB)) / 1000000000. * edsrs(iy-1)*CarbCnvFac
            rsly(22) = H2OCONWT(IY-1,3,IR,IB) / 1000000000. * edsrs(iy-1)*CarbCnvFac
            rsly(23) = APLCONWT(IY-1,3,IR,IB) / 1000000000. * edsrs(iy-1)*CarbCnvFac

!       --- LIQUEFIED PETROLEUM GAS
            rsly(24) = (HTRCONWT(IY-1,4,IR,IB) + SHTCONWT(IY-1,4,IR,IB)) / 1000000000. * elgrs(iy-1)*CarbCnvFac
            rsly(25) = H2OCONWT(IY-1,4,IR,IB) / 1000000000. * elgrs(iy-1)*CarbCnvFac
            rsly(26) = CKCONWT(IY-1,2,IR,IB)  / 1000000000. * elgrs(iy-1)*CarbCnvFac
            rsly(27) = APLCONWT(IY-1,2,IR,IB) / 1000000000. * elgrs(iy-1)*CarbCnvFac

!      --- MARKETED RENEWABLES
            rsly(28) = (HTRCONWT(IY-1,6,IR,IB) + shtconwt(IY-1,7,ir,ib)) / 1000000000. * 0.0
!      --- OTHER FUELS
            rsly(29) = (QKSRS(IR,IY-1)*eksrs(iy-1)+QCLRS(IR,IY-1)*eclrs(iy-1))/3.*CarbCnvFac ! KEROSENE + COAL

 !       --- Electricity
            rsefly(1 ) = (HTRCONIN(IY-1,2,IR,IB) + SHTCONIN(IY-1,2,IR,IB))  * ElecCarb_PY
            rsefly(2 ) = COOLCNIN(IY-1,1,IR,IB)  * ElecCarb_PY
            rsefly(3 ) = H2OCONIN(IY-1,2,IR,IB)  * ElecCarb_PY
            rsefly(4 ) = REFCONIN(IY-1,IR,IB)    * ElecCarb_PY
            rsefly(5 ) = CKCONIN(IY-1,3,IR,IB)   * ElecCarb_PY
            rsefly(6 ) = DRYCONIN(IY-1,2,IR,IB)  * ElecCarb_PY
            rsefly(7 ) = FRZCONIN(IY-1,IR,IB)    * ElecCarb_PY
            rsefly(8 ) = LTCONIN(IY-1,IR,IB)     * ElecCarb_PY
            rsefly(9 ) = CSWCONIN(IY-1,IR,IB)    * ElecCarb_PY
            rsefly(10) = DSWCONIN(IY-1,IR,IB)    * ElecCarb_PY
            rsefly(11) = TVRCONIN(IY-1,IR,IB)     * ElecCarb_PY
            rsefly(12) = PCRCONIN(IY-1,IR,IB)     * ElecCarb_PY
            rsefly(13) = FANCONIN(IY-1,IR,IB)    * ElecCarb_PY
            rsefly(14) = APCONIN(IY-1,IR,IB)     * ElecCarb_PY

!       --- NATURAL GAS
            rsefly(15) = (HTRCONIN(IY-1,1,IR,IB)+SHTCONIN(IY-1,1,IR,IB))  * engrs(iy-1)*CarbCnvFac
            rsefly(16) = COOLCNIN(IY-1,3,IR,IB)  * engrs(iy-1)*CarbCnvFac
            rsefly(17) = H2OCONIN(IY-1,1,IR,IB)  * engrs(iy-1)*CarbCnvFac
            rsefly(18) = CKCONIN(IY-1,1,IR,IB)   * engrs(iy-1)*CarbCnvFac
            rsefly(19) = DRYCONIN(IY-1,1,IR,IB)  * engrs(iy-1)*CarbCnvFac
            rsefly(20) = APLCONIN(IY-1,1,IR,IB)  * engrs(iy-1)*CarbCnvFac

!        --- DISTILLATE
            rsefly(21) = (HTRCONIN(IY-1,3,IR,IB) + SHTCONIN(IY-1,3,IR,IB))  * edsrs(iy)*CarbCnvFac
            rsefly(22) = H2OCONIN(IY-1,3,IR,IB)  * edsrs(iy-1)*CarbCnvFac
            rsefly(23) = APLCONIN(IY-1,3,IR,IB)  * edsrs(iy-1)*CarbCnvFac

!       --- LIQUEFIED PETROLEUM GAS
            rsefly(24) = (HTRCONIN(IY-1,4,IR,IB)+SHTCONIN(IY-1,4,IR,IB))  * elgrs(iy-1)*CarbCnvFac
            rsefly(25) = H2OCONIN(IY-1,4,IR,IB)  * elgrs(iy-1)*CarbCnvFac
            rsefly(26) = CKCONIN(IY-1,2,IR,IB)   * elgrs(iy-1)*CarbCnvFac
            rsefly(27) = APLCONIN(IY-1,2,IR,IB)  * elgrs(iy-1)*CarbCnvFac

!      --- MARKETED RENEWABLES
            rsefly(28) = (HTRCONIN(IY-1,6,IR,IB) + shtconin(IY-1,7,ir,ib))  * 0.
!      --- OTHER FUELS
            rsefly(29) = (QKSRS(IR,IY-1)*eksrs(iy-1) + QCLRS(IR,IY-1)*eclrs(iy-1))*1000./T4(4,IY-1,IS)*CarbCnvFac ! KEROSENE + COAL per HH

            do iline_nbr = 1,29

                     if(iprintflag==99) then
                        write (iprintfile,97) "ResdCarb",IY,IR,IB,iline_nbr,  &
                                               rs(iline_nbr),   t17(itotal_nbr_carb_res,11,iy,is),    &
                                               rsly(iline_nbr), t17(itotal_nbr_carb_res,11,iy-1,is),  &
                                               rsef(iline_nbr),   driver_cy,   &
                                               rsefly(iline_nbr), driver_py
                     endif

              if (t17(itotal_nbr_carb,11,iy,is)<=0.)     cycle  !check for zero carbon emissions just in case, etc...
              if (t17(itotal_nbr_carb,11,iy-1,is)<=0.)   cycle
              if (rsef(iline_nbr)<=0.)           cycle
              if (rsefly(iline_nbr)<=0.)         cycle
              if (driver_cy<=0.)                 cycle
              if (driver_py<=0.)                 cycle

! Diagnostic rollup of residential carbon
              Res_carbon = Res_carbon + rs(iline_nbr)

              if(isnan((rsef(iline_nbr)/driver_cy)/(rsefly(iline_nbr)/driver_py))) then
                T31(13,IY,IS)=-10.
                T31(9,IY,IS)=-10.
               else
                T31(13,IY,IS)= T31(13,IY,IS) + &                                    !single stage index
                  0.5*(rs(iline_nbr)/t17(itotal_nbr_carb,11,iy,is)+ &
                        rsly(iline_nbr)/t17(itotal_nbr_carb,11,iy-1,is)) &
                      * log((rsef(iline_nbr)/driver_cy)/(rsefly(iline_nbr)/driver_py))

                T31(9,IY,IS)= T31(9,IY,IS) + &                                      !residential index
                  0.5*( rs(iline_nbr)/t17(itotal_nbr_carb_res,11,iy,is)+ &
                        rsly(iline_nbr)/t17(itotal_nbr_carb_res,11,iy-1,is)) &
                      * log((rsef(iline_nbr)/driver_cy)/(rsefly(iline_nbr)/driver_py))
              endif

            enddo  ! iline_nbr for end use fuel combinations
          enddo   ! for building types
        enddo    ! IR for census divisions

          if(iprintflag==1 .OR. iprintflag==99) Write (iprintfile,*) "Residential Carbon ",Res_Carbon, " IY ", IY+1989, " FTAB ", t17(itotal_nbr_carb_res,11,iy,is)

!Commercial
        if(iprintflag==1 .OR. iprintflag==99) Write (iprintfile,*) "Commercial Diagnostics for year ", iy

        do IR=1,9
          if(iprintflag==1) then
            Write (iprintfile,*)"Comm Reg=",ir
          endif
          do IB=1,11
            if(iprintflag==1) then
              Write (iprintfile,*)"Comm Bldg=",ib
          endif

          ! line numbers and filters
            ifirst_nbr=7  ;  ilast_nbr=29
            iskip_nbr(1:10)=[17,23,27,0,0,0,0,0,0,0]

!   Use Efficiency Index by End Use for "Major" End Uses, 1.0 for penetrating end uses, and Ftab for Other
!     Set up the Index, the driver when appropriate and the energy weight
!     Current Year Variables
        !  electricity
            cmindx_cy(7) = cmeffind(1,1,IB,IR,IY) ; cmdriver_cy(7) = 1.  ; cmen_cy(7)=EndUseConsump(1,1,IB,IR,IY)/1000.
            cmindx_cy(8) = cmeffind(1,2,IB,IR,IY) ; cmdriver_cy(8) = 1.  ; cmen_cy(8)=EndUseConsump(1,2,IB,IR,IY)/1000.
            cmindx_cy(9) = cmeffind(1,3,IB,IR,IY) ; cmdriver_cy(9) = 1.  ; cmen_cy(9)=EndUseConsump(1,3,IB,IR,IY)/1000.
            cmindx_cy(10)= cmeffind(1,4,IB,IR,IY) ; cmdriver_cy(10)= 1.  ; cmen_cy(10)=EndUseConsump(1,4,IB,IR,IY)/1000.
            cmindx_cy(11)= cmeffind(1,5,IB,IR,IY) ; cmdriver_cy(11)= 1.  ; cmen_cy(11)=EndUseConsump(1,5,IB,IR,IY)/1000.
            cmindx_cy(12)= cmeffind(1,6,IB,IR,IY) ; cmdriver_cy(12)= 1.  ; cmen_cy(12)=EndUseConsump(1,6,IB,IR,IY)/1000.
            cmindx_cy(13)= cmeffind(1,7,IB,IR,IY) ; cmdriver_cy(13)= 1.  ; cmen_cy(13)=EndUseConsump(1,7,IB,IR,IY)/1000.
            cmindx_cy(14)= 1./(1+CEffGrowth(8));cmdriver_cy(14)=1.       ; cmen_cy(14)=EndUseConsump(1,8,IB,IR,IY)/1000.
            cmindx_cy(15)= 1./(1+CEffGrowth(9));cmdriver_cy(15)=1.       ; cmen_cy(15)=EndUseConsump(1,9,IB,IR,IY)/1000.
            cmindx_cy(16)= 1.                  ;cmdriver_cy(16)=1.       ; cmen_cy(16)=t5(16,iy,is)/99.
        !  natural gas
            cmindx_cy(18)= cmeffind(2,1,IB,IR,IY) ; cmdriver_cy(18)= 1.  ; cmen_cy(18)=EndUseConsump(2,1,IB,IR,IY)/1000.
            cmindx_cy(19)= cmeffind(2,2,IB,IR,IY) ; cmdriver_cy(19)= 1.  ; cmen_cy(19)=EndUseConsump(2,2,IB,IR,IY)/1000.
            cmindx_cy(20)= cmeffind(2,3,IB,IR,IY) ; cmdriver_cy(20)= 1.  ; cmen_cy(20)=EndUseConsump(2,3,IB,IR,IY)/1000.
            cmindx_cy(21)= cmeffind(2,4,IB,IR,IY) ; cmdriver_cy(21)= 1.  ; cmen_cy(21)=EndUseConsump(2,5,IB,IR,IY)/1000.
            cmindx_cy(22)= 1.                     ; cmdriver_cy(22)= 1.  ; cmen_cy(22)=t5(22,iy,is)/99.
        !  distillate
            cmindx_cy(24)= cmeffind(3,1,IB,IR,IY) ; cmdriver_cy(24)= 1.  ; cmen_cy(24)=EndUseConsump(3,1,IB,IR,IY)/1000.
            cmindx_cy(25)= cmeffind(3,2,IB,IR,IY) ; cmdriver_cy(25)= 1.  ; cmen_cy(25)=EndUseConsump(3,3,IB,IR,IY)/1000.
            cmindx_cy(26)= 1.                     ; cmdriver_cy(26)= 1.  ; cmen_cy(26)=t5(26,iy,is)/99.
        !  other fuels(28) & biomass(29)
            cmindx_cy(28)= t5(28,iy,is)           ; cmdriver_cy(28)= t5(3,iy,is); cmen_cy(28)=t5(28,iy,is)/99.
            cmindx_cy(29)= t5(29,iy,is)           ; cmdriver_cy(29)= t5(3,iy,is); cmen_cy(29)=t5(29,iy,is)/99.
!  Prior Year Variables
        !  electricity
            cmindx_py(7) = cmeffind(1,1,IB,IR,iy-1) ;cmdriver_py(7) = 1. ;cmen_py(7) =EndUseConsump(1,1,IB,IR,iy-1)/1000.
            cmindx_py(8) = cmeffind(1,2,IB,IR,iy-1) ;cmdriver_py(8) = 1. ;cmen_py(8) =EndUseConsump(1,2,IB,IR,iy-1)/1000.
            cmindx_py(9) = cmeffind(1,3,IB,IR,iy-1) ;cmdriver_py(9) = 1. ;cmen_py(9) =EndUseConsump(1,3,IB,IR,iy-1)/1000.
            cmindx_py(10)= cmeffind(1,4,IB,IR,iy-1) ;cmdriver_py(10)= 1. ;cmen_py(10)=EndUseConsump(1,4,IB,IR,iy-1)/1000.
            cmindx_py(11)= cmeffind(1,5,IB,IR,iy-1) ;cmdriver_py(11)= 1. ;cmen_py(11)=EndUseConsump(1,5,IB,IR,iy-1)/1000.
            cmindx_py(12)= cmeffind(1,6,IB,IR,iy-1) ;cmdriver_py(12)= 1. ;cmen_py(12)=EndUseConsump(1,6,IB,IR,iy-1)/1000.
            cmindx_py(13)= cmeffind(1,7,IB,IR,iy-1) ;cmdriver_py(13)= 1. ;cmen_py(13)=EndUseConsump(1,7,IB,IR,iy-1)/1000.
            cmindx_py(14)= 1.      ;cmdriver_py(14)= 1.                  ;cmen_py(14)=EndUseConsump(1,8,IB,IR,iy-1)/1000.
            cmindx_py(15)= 1.      ;cmdriver_py(15)= 1.                  ;cmen_py(15)=EndUseConsump(1,9,IB,IR,iy-1)/1000.
            cmindx_py(16)= 1.      ;cmdriver_py(16)= 1.                  ;cmen_py(16)= t5(16,iy-1,is)/99.
        !  natural gas
            cmindx_py(18)= cmeffind(2,1,IB,IR,iy-1) ;cmdriver_py(18)= 1. ;cmen_py(18)=EndUseConsump(2,1,IB,IR,iy-1)/1000.
            cmindx_py(19)= cmeffind(2,2,IB,IR,iy-1) ;cmdriver_py(19)= 1. ;cmen_py(19)=EndUseConsump(2,2,IB,IR,iy-1)/1000.
            cmindx_py(20)= cmeffind(2,3,IB,IR,iy-1) ;cmdriver_py(20)= 1. ;cmen_py(20)=EndUseConsump(2,3,IB,IR,iy-1)/1000.
            cmindx_py(21)= cmeffind(2,4,IB,IR,iy-1) ;cmdriver_py(21)= 1. ;cmen_py(21)=EndUseConsump(2,5,IB,IR,iy-1)/1000.
            cmindx_py(22)= 1.                       ;cmdriver_py(22)= 1. ;cmen_py(22)= t5(22,iy-1,is)/99.
        !  distillate
            cmindx_py(24)= cmeffind(3,1,IB,IR,iy-1) ;cmdriver_py(24)= 1. ;cmen_py(24)=EndUseConsump(3,1,IB,IR,iy-1)/1000.
            cmindx_py(25)= cmeffind(3,2,IB,IR,iy-1) ;cmdriver_py(25)= 1. ;cmen_py(25)=EndUseConsump(3,3,IB,IR,iy-1)/1000.
            cmindx_py(26)= 1.                       ;cmdriver_py(26)= 1. ;cmen_py(26)= t5(26,iy-1,is)/99.
        !  other fuels(28) & biomass(29)
            cmindx_py(28)= t5(28,iy-1,is)           ;cmdriver_py(28)= t5(3,iy-1,is); cmen_py(28)= t5(28,iy-1,is)/99.
            cmindx_py(29)= t5(29,iy-1,is)           ;cmdriver_py(29)= t5(3,iy-1,is); cmen_py(29)= t5(29,iy-1,is)/99.


            do iline_nbr = ifirst_nbr,ilast_nbr

              iflag_nbr=0
              do itest_nbr = 1,10
                if (iline_nbr.eq.iskip_nbr(itest_nbr)) iflag_nbr=1
              enddo
                    if(iprintflag==99) then
                        write (iprintfile,97) "CommTrill",IY,IR,IB,iline_nbr,  &
                                               cmen_cy(iline_nbr),   t2(itotal_nbr_com,11,iy,is),    &
                                               cmen_py(iline_nbr),   t2(itotal_nbr_com,11,iy-1,is),  &
                                               cmindx_cy(iline_nbr), cmdriver_cy(iline_nbr),    &
                                               cmindx_py(iline_nbr), cmdriver_py(iline_nbr)
                    endif


              if (iflag_nbr==1)                  cycle
              if (t2(itotal_nbr,11,iy,is)<=0.)   cycle
              if (t2(itotal_nbr,11,iy-1,is)<=0.) cycle
              if (t5(iline_nbr,iy,is)<=0.)       cycle
              if (t5(iline_nbr,iy-1,is)<=0.)     cycle
              if (cmindx_cy(iline_nbr)<=0.)      cycle
              if (cmindx_py(iline_nbr)<=0.)      cycle
              if (cmdriver_cy(iline_nbr)<=0.)    cycle
              if (cmdriver_py(iline_nbr)<=0.)    cycle

! Diagnostic rollup of commercial energy
              Com_energy = Com_energy + cmen_cy(iline_nbr)

              if(iprintflag==1) then
                do irow =1,nrows(5); if(irows(irow,5)==iline_nbr)irpntr=irow; enddo    ! filter to find row label pointer
                Write (iprintfile,98)rowlab(irpntr,5),cmindx_cy(iline_nbr),cmdriver_cy(iline_nbr),cmen_cy(iline_nbr)
              endif

          if(isnan(( (cmindx_cy(iline_nbr)/cmdriver_cy(iline_nbr))/ (cmindx_py(iline_nbr)/cmdriver_py(iline_nbr))))) then
                T31(7,IY,IS)=-10.
                T31(3,IY,IS)=-10.
               else
                T31(7,IY,IS)= T31(7,IY,IS) + &
                 0.5 * (cmen_cy(iline_nbr)/t2(itotal_nbr,11,iy,is)+ &
                       cmen_py(iline_nbr)/t2(itotal_nbr,11,iy-1,is)) &
                    *  log( (cmindx_cy(iline_nbr)/cmdriver_cy(iline_nbr))/ &
                      (cmindx_py(iline_nbr)/cmdriver_py(iline_nbr)))

                T31(3,IY,IS)= T31(3,IY,IS) + &
                 0.5 * (cmen_cy(iline_nbr)/t2(itotal_nbr_com,11,iy,is)+ &
                       cmen_py(iline_nbr)/t2(itotal_nbr_com,11,iy-1,is))  &
                    *  log( (cmindx_cy(iline_nbr)/cmdriver_cy(iline_nbr))/ &
                      (cmindx_py(iline_nbr)/cmdriver_py(iline_nbr)))
              endif

            enddo !iline_nbr
          enddo  !building type
        enddo   !region

             if(iprintflag==1 .OR. iprintflag==99)  Write (iprintfile,*) "Commercial Energy ",Com_Energy, " IY ", IY+1989, " FTAB ", t2(itotal_nbr_com,11,iy,is)
        if(iprintflag==1) then
          Write (iprintfile,*) "Commercial Floorspace"
          do ir =1,nrows(5); if(irows(ir,5)==3)irpntr=ir; enddo
          Write (iprintfile,98) rowlab(irows(irpntr,5),5), t5(3,11:41,is)  !floorspace
        endif

!Carbon
        if(iprintflag==1) then
          Write (iprintfile,*) "Commercial Diagnostics for year ", iy
        endif
        do IR=1,9
          if(iprintflag==1) then
          Write (iprintfile,*)"Comm Reg=",ir
          endif
          do IB=1,11
            if(iprintflag==1) then
              Write (iprintfile,*)"Comm Bldg=",ib
            endif

          ! line numbers and filters
            ifirst_nbr=7  ;  ilast_nbr=29
            iskip_nbr(1:10)=[17,23,27,0,0,0,0,0,0,0]

!   Use Efficiency Index by End Use for "Major" End Uses, 1.0 for penetrating end uses, and Ftab for Other
!     Set up the Index, the driver when appropriate and the energy weight
!     Current Year Variables
        !  electricity
            cmindx_cy(7) = cmeffind(1,1,IB,IR,IY)*ElecCarb_CY ; cmdriver_cy(7) = 1.  ; cmen_cy(7)=EndUseConsump(1,1,IB,IR,IY)/1000.*ElecCarb_CY
            cmindx_cy(8) = cmeffind(1,2,IB,IR,IY)*ElecCarb_CY ; cmdriver_cy(8) = 1.  ; cmen_cy(8)=EndUseConsump(1,2,IB,IR,IY)/1000.*ElecCarb_CY
            cmindx_cy(9) = cmeffind(1,3,IB,IR,IY)*ElecCarb_CY ; cmdriver_cy(9) = 1.  ; cmen_cy(9)=EndUseConsump(1,3,IB,IR,IY)/1000.*ElecCarb_CY
            cmindx_cy(10)= cmeffind(1,4,IB,IR,IY)*ElecCarb_CY ; cmdriver_cy(10)= 1.  ; cmen_cy(10)=EndUseConsump(1,4,IB,IR,IY)/1000.*ElecCarb_CY
            cmindx_cy(11)= cmeffind(1,5,IB,IR,IY)*ElecCarb_CY ; cmdriver_cy(11)= 1.  ; cmen_cy(11)=EndUseConsump(1,5,IB,IR,IY)/1000.*ElecCarb_CY
            cmindx_cy(12)= cmeffind(1,6,IB,IR,IY)*ElecCarb_CY ; cmdriver_cy(12)= 1.  ; cmen_cy(12)=EndUseConsump(1,6,IB,IR,IY)/1000.*ElecCarb_CY
            cmindx_cy(13)= cmeffind(1,7,IB,IR,IY)*ElecCarb_CY ; cmdriver_cy(13)= 1.  ; cmen_cy(13)=EndUseConsump(1,7,IB,IR,IY)/1000.*ElecCarb_CY
            cmindx_cy(14)= 1./(1+CEffGrowth(8))*ElecCarb_CY;    cmdriver_cy(14)=1.   ; cmen_cy(14)=EndUseConsump(1,8,IB,IR,IY)/1000.*ElecCarb_CY
            cmindx_cy(15)= 1./(1+CEffGrowth(9))*ElecCarb_CY;    cmdriver_cy(15)=1.   ; cmen_cy(15)=EndUseConsump(1,9,IB,IR,IY)/1000.*ElecCarb_CY
            cmindx_cy(16)= 1.*ElecCarb_CY                  ;    cmdriver_cy(16)=1.   ; cmen_cy(16)=t5(16,iy,is)/99.*ElecCarb_CY
        !  natural gas
            cmindx_cy(18)= cmeffind(2,1,IB,IR,IY)*engcm(iy)*CarbCnvFac ; cmdriver_cy(18)= 1.  ; cmen_cy(18)=EndUseConsump(2,1,IB,IR,IY)/1000.*engcm(iy)*CarbCnvFac
            cmindx_cy(19)= cmeffind(2,2,IB,IR,IY)*engcm(iy)*CarbCnvFac ; cmdriver_cy(19)= 1.  ; cmen_cy(19)=EndUseConsump(2,2,IB,IR,IY)/1000.*engcm(iy)*CarbCnvFac
            cmindx_cy(20)= cmeffind(2,3,IB,IR,IY)*engcm(iy)*CarbCnvFac ; cmdriver_cy(20)= 1.  ; cmen_cy(20)=EndUseConsump(2,3,IB,IR,IY)/1000.*engcm(iy)*CarbCnvFac
            cmindx_cy(21)= cmeffind(2,4,IB,IR,IY)*engcm(iy)*CarbCnvFac ; cmdriver_cy(21)= 1.  ; cmen_cy(21)=EndUseConsump(2,5,IB,IR,IY)/1000.*engcm(iy)*CarbCnvFac
            cmindx_cy(22)= 1.*engcm(iy)*CarbCnvFac                     ; cmdriver_cy(22)= 1.  ; cmen_cy(22)=t5(22,iy,is)/99.*engcm(iy)*CarbCnvFac
        !  distillate
            cmindx_cy(24)= cmeffind(3,1,IB,IR,IY)*edscm(iy)*CarbCnvFac ; cmdriver_cy(24)= 1.  ; cmen_cy(24)=EndUseConsump(3,1,IB,IR,IY)/1000.*edscm(iy)*CarbCnvFac
            cmindx_cy(25)= cmeffind(3,2,IB,IR,IY)*edscm(iy)*CarbCnvFac ;     cmdriver_cy(25)= 1.  ; cmen_cy(25)=EndUseConsump(3,3,IB,IR,IY)/1000.*edscm(iy)*CarbCnvFac
            cmindx_cy(26)= 1.*edscm(iy)*CarbCnvFac                     ;     cmdriver_cy(26)= 1.  ; cmen_cy(26)=t5(26,iy,is)/99.*edscm(iy)*CarbCnvFac
        !  other fuels(28) & biomass(29)
            cmindx_cy(28)= t5(28,iy,is)*(.22*eclcm(iy)+.78*erlcm(iy))*CarbCnvFac           ; cmdriver_cy(28)= t5(3,iy,is); cmen_cy(28)=t5(28,iy,is)/99.*(.22*eclcm(iy)+.78*erlcm(iy))*CarbCnvFac
            cmindx_cy(29)= t5(29,iy,is)*0.           ; cmdriver_cy(29)= t5(3,iy,is); cmen_cy(29)=t5(29,iy,is)/99.*0 !biomass

!  Prior Year Variables
        !  electricity
            cmindx_py(7) = cmeffind(1,1,IB,IR,iy-1)*ElecCarb_PY ;cmdriver_py(7) = 1. ;cmen_py(7) =EndUseConsump(1,1,IB,IR,iy-1)/1000.*ElecCarb_PY
            cmindx_py(8) = cmeffind(1,2,IB,IR,iy-1)*ElecCarb_PY ;cmdriver_py(8) = 1. ;cmen_py(8) =EndUseConsump(1,2,IB,IR,iy-1)/1000.*ElecCarb_PY
            cmindx_py(9) = cmeffind(1,3,IB,IR,iy-1)*ElecCarb_PY ;cmdriver_py(9) = 1. ;cmen_py(9) =EndUseConsump(1,3,IB,IR,iy-1)/1000.*ElecCarb_PY
            cmindx_py(10)= cmeffind(1,4,IB,IR,iy-1)*ElecCarb_PY ;cmdriver_py(10)= 1. ;cmen_py(10)=EndUseConsump(1,4,IB,IR,iy-1)/1000.*ElecCarb_PY
            cmindx_py(11)= cmeffind(1,5,IB,IR,iy-1)*ElecCarb_PY ;cmdriver_py(11)= 1. ;cmen_py(11)=EndUseConsump(1,5,IB,IR,iy-1)/1000.*ElecCarb_PY
            cmindx_py(12)= cmeffind(1,6,IB,IR,iy-1)*ElecCarb_PY ;cmdriver_py(12)= 1. ;cmen_py(12)=EndUseConsump(1,6,IB,IR,iy-1)/1000.*ElecCarb_PY
            cmindx_py(13)= cmeffind(1,7,IB,IR,iy-1)*ElecCarb_PY ;cmdriver_py(13)= 1. ;cmen_py(13)=EndUseConsump(1,7,IB,IR,iy-1)/1000.*ElecCarb_PY
            cmindx_py(14)= 1.*ElecCarb_PY      ;cmdriver_py(14)= 1.                  ;cmen_py(14)=EndUseConsump(1,8,IB,IR,iy-1)/1000.*ElecCarb_PY
            cmindx_py(15)= 1.*ElecCarb_PY      ;cmdriver_py(15)= 1.                  ;cmen_py(15)=EndUseConsump(1,9,IB,IR,iy-1)/1000.*ElecCarb_PY
            cmindx_py(16)= 1.*ElecCarb_PY      ;cmdriver_py(16)= 1.                  ;cmen_py(16)= t5(16,iy-1,is)/99.*ElecCarb_PY
        !  natural gas
            cmindx_py(18)= cmeffind(2,1,IB,IR,iy-1)*engcm(iy-1)*CarbCnvFac ;cmdriver_py(18)= 1. ;cmen_py(18)=EndUseConsump(2,1,IB,IR,iy-1)/1000.*engcm(iy-1)*CarbCnvFac
            cmindx_py(19)= cmeffind(2,2,IB,IR,iy-1)*engcm(iy-1)*CarbCnvFac ;cmdriver_py(19)= 1. ;cmen_py(19)=EndUseConsump(2,2,IB,IR,iy-1)/1000.*engcm(iy-1)*CarbCnvFac
            cmindx_py(20)= cmeffind(2,3,IB,IR,iy-1)*engcm(iy-1)*CarbCnvFac ;cmdriver_py(20)= 1. ;cmen_py(20)=EndUseConsump(2,3,IB,IR,iy-1)/1000.*engcm(iy-1)*CarbCnvFac
            cmindx_py(21)= cmeffind(2,4,IB,IR,iy-1)*engcm(iy-1)*CarbCnvFac ;cmdriver_py(21)= 1. ;cmen_py(21)=EndUseConsump(2,5,IB,IR,iy-1)/1000.*engcm(iy-1)*CarbCnvFac
            cmindx_py(22)= 1.*engcm(iy-1)*CarbCnvFac                       ;cmdriver_py(22)= 1. ;cmen_py(22)= t5(22,iy-1,is)/99.*engcm(iy-1)*CarbCnvFac
        !  distillate
            cmindx_py(24)= cmeffind(3,1,IB,IR,iy-1)*edscm(iy-1)*CarbCnvFac ;cmdriver_py(24)= 1. ;cmen_py(24)=EndUseConsump(3,1,IB,IR,iy-1)/1000.*edscm(iy-1)*CarbCnvFac
            cmindx_py(25)= cmeffind(3,2,IB,IR,iy-1)*edscm(iy-1)*CarbCnvFac ;cmdriver_py(25)= 1. ;cmen_py(25)=EndUseConsump(3,3,IB,IR,iy-1)/1000.*edscm(iy-1)*CarbCnvFac
            cmindx_py(26)= 1.*edscm(iy-1)*CarbCnvFac                       ;cmdriver_py(26)= 1. ;cmen_py(26)= t5(26,iy-1,is)/99.*edscm(iy-1)*CarbCnvFac
        !  other fuels(28) & biomass(29)
            cmindx_py(28)= t5(28,iy-1,is)*(.22*eclcm(iy-1)+.78*erlcm(iy-1))*CarbCnvFac           ;cmdriver_py(28)= t5(3,iy-1,is); cmen_py(28)= t5(28,iy-1,is)/99.*(.22*eclcm(iy-1)+.78*erlcm(iy-1))*CarbCnvFac
            cmindx_py(29)= t5(29,iy-1,is)*0.           ;cmdriver_py(29)= t5(3,iy-1,is); cmen_py(29)= t5(29,iy-1,is)/99.*0.


            do iline_nbr = ifirst_nbr,ilast_nbr

              iflag_nbr=0
              do itest_nbr = 1,10
                if (iline_nbr.eq.iskip_nbr(itest_nbr)) iflag_nbr=1
              enddo
                    if(iprintflag==99) then
                        write (iprintfile,97) "CommCarb",IY,IR,IB,iline_nbr,  &
                                               cmen_cy(iline_nbr),   t17(itotal_nbr_carb_com,11,iy,is),    &
                                               cmen_py(iline_nbr),   t17(itotal_nbr_carb_com,11,iy-1,is),  &
                                               cmindx_cy(iline_nbr), cmdriver_cy(iline_nbr),    &
                                               cmindx_py(iline_nbr), cmdriver_py(iline_nbr)
                    endif


              if (iflag_nbr==1)                  cycle
              if (t17(itotal_nbr_carb,11,iy,is)<=0.)   cycle
              if (t17(itotal_nbr_carb,11,iy-1,is)<=0.) cycle
              if (t5(iline_nbr,iy,is)<=0.)       cycle
              if (t5(iline_nbr,iy-1,is)<=0.)     cycle
              if (cmindx_cy(iline_nbr)<=0.)      cycle
              if (cmindx_py(iline_nbr)<=0.)      cycle
              if (cmdriver_cy(iline_nbr)<=0.)    cycle
              if (cmdriver_py(iline_nbr)<=0.)    cycle

! Diagnostic rollup of commercial energy
              Com_carbon = Com_carbon + cmen_cy(iline_nbr)

              if(iprintflag==1) then
                do irow =1,nrows(5); if(irows(irow,5)==iline_nbr)irpntr=irow; enddo    ! filter to find row label pointer
                Write (iprintfile,98)rowlab(irpntr,5),cmindx_cy(iline_nbr),cmdriver_cy(iline_nbr),cmen_cy(iline_nbr)
              endif

              if(isnan(( (cmindx_cy(iline_nbr)/cmdriver_cy(iline_nbr))/ (cmindx_py(iline_nbr)/cmdriver_py(iline_nbr))))) then
                T31(13,IY,IS)=-10.
                T31(10,IY,IS)=-10.
               else
                T31(13,IY,IS)= T31(13,IY,IS) + &
                 0.5 * (cmen_cy(iline_nbr)/t17(itotal_nbr_carb,11,iy,is)+ &
                       cmen_py(iline_nbr)/t17(itotal_nbr_carb,11,iy-1,is)) &
                    *  log( (cmindx_cy(iline_nbr)/cmdriver_cy(iline_nbr))/ &
                      (cmindx_py(iline_nbr)/cmdriver_py(iline_nbr)))

                T31(10,IY,IS)= T31(10,IY,IS) + &
                 0.5 * (cmen_cy(iline_nbr)/t17(itotal_nbr_carb_com,11,iy,is)+ &
                       cmen_py(iline_nbr)/t17(itotal_nbr_carb_com,11,iy-1,is))  &
                    *  log( (cmindx_cy(iline_nbr)/cmdriver_cy(iline_nbr))/ &
                      (cmindx_py(iline_nbr)/cmdriver_py(iline_nbr)))
               endif

            enddo !iline_nbr
          enddo  !building type
        enddo   !region

              if(iprintflag==1 .OR. iprintflag==99)  Write (iprintfile,*) "Commercial Carbon ",Com_Carbon, " IY ", IY+1989, " FTAB ", t17(itotal_nbr_carb_com,11,iy,is)


        if(iprintflag==1) then
          Write (iprintfile,*) "Commercial Floorspace"
          do ir =1,nrows(5); if(irows(ir,5)==3)irpntr=ir; enddo
          Write (iprintfile,98) rowlab(irows(irpntr,5),5), t5(3,11:41,is)  !floorspace
        endif

!  Transportation
        if(iprintflag==1 .OR. iprintflag==99) Write (iprintfile,*) "Transportation Diagnostics for year ", iy

        IB=0  !no building dimension from here on
        IR=0  !no regionality from here on
!   current year drivers (VMT, TMT, seat miles, etc...)
!       calculate VMT for cars, light trucks and motorcycles, all light duty vehicles
!       5.24 = avg. mmbtu of a 42 gallon barrel of fuel (with weights for diesel--5.825-- and gasoline--5.234--)
        trdrv_cy(1)=t50(42,iy,is)*t45(2,iy,is)/((5.24/42)/1000000.)   !VMT measure for cars
        trdrv_cy(2)=t50(43,iy,is)*t45(3,iy,is)/((5.24/42)/1000000.)   !VMT measure for light trucks
        trdrv_cy(3)=50.*t45(4,iy,is)/((5.234/42)/1000000.)            !VMT measure for motorcycles assume 50 mpg
        trdrv_cy(4)=t7(2,iy,is)         !com. light trucks
        trdrv_cy(5)=t7(3,iy,is)         !tmt freight trucks
        trdrv_cy(6)=t7(4,iy,is)         !seat miles for air transportation
        trdrv_cy(7)=t7(5,iy,is)         !tmt for rail
        trdrv_cy(8)=t7(6,iy,is)         !tmt for water
        trdrv_cy(9)=t34(1,mnumcr,iy,is)        !RGDP for pipeline fuel
        trdrv_cy(10)=t34(1,mnumcr,iy,is)       !RGDP for lubricants
!  current year energy consumption
        trnen_cy(1)=t45(2,iy,is)/1000.  !energy consumption for cars -- convert trills to quads
        trnen_cy(2)=t45(3,iy,is)/1000.  !energy consumption for light trucks -- convert trills to quads
        trnen_cy(3)=t45(4,iy,is)/1000.  !energy consumption for motorcycles -- convert trills to quads
        trnen_cy(4)=t7(18,iy,is)+t7(40,iy,is)   !energy consumption for commercial light trucks & buses -- quads
        trnen_cy(5)=t7(19,iy,is)        !energy consumption for heavytrucks -- quads
        trnen_cy(6)=t7(20,iy,is)        !energy consumption for air -- quads
        trnen_cy(7)=t7(22,iy,is)+t7(41,iy,is)   !energy consumption for rail -- quads
        trnen_cy(8)=t7(42,iy,is)+t7(43,iy,is)+t7(44,iy,is)      !energy consumption for water -- quads
        trnen_cy(9)=t7(23,iy,is)+t7(21,iy,is)+t7(24,iy,is)      !energy consumption for pipeline fuel, military & lubricants -- quads
        trnen_cy(10)=t2(itotal_nbr_tran,11,iy,is)-trnen_cy(1)-trnen_cy(2)-trnen_cy(3)-trnen_cy(4)-trnen_cy(5) &
                    -trnen_cy(6)-trnen_cy(7)-trnen_cy(8)-trnen_cy(9) !energy consumption for other, computed as a residual -- quads
!  prior year drivers
        trdrv_py(1)=t50(42,iy-1,is)*t45(2,iy-1,is)/((5.24/42)/1000000.) !VMT measure for cars
        trdrv_py(2)=t50(43,iy-1,is)*t45(3,iy-1,is)/((5.24/42)/1000000.) !VMT measure for light trucks
        trdrv_py(3)=50.*t45(4,iy-1,is)/((5.234/42)/1000000.)            !VMT measure for motorcycles assume 50 mpg
        trdrv_py(4)=t7(2,iy-1,is)        !com. light trucks
        trdrv_py(5)=t7(3,iy-1,is)        !tmt freight trucks
        trdrv_py(6)=t7(4,iy-1,is)        !seat miles for air transportation
        trdrv_py(7)=t7(5,iy-1,is)        !tmt for rail
        trdrv_py(8)=t7(6,iy-1,is)        !tmt for water
        trdrv_py(9)=t34(1,mnumcr,iy-1,is)       !RGDP for pipeline fuel
        trdrv_py(10)=t34(1,mnumcr,iy-1,is)      !RGDP for lubricants
!  prior year energy consumption
        trnen_py(1)=t45(2,iy-1,is)/1000.  !energy consumption for cars -- convert trills to quads
        trnen_py(2)=t45(3,iy-1,is)/1000.  !energy consumption for light trucks -- convert trills to quads
        trnen_py(3)=t45(4,iy-1,is)/1000.  !energy consumption for motorcycles -- convert trills to quads
        trnen_py(4)=t7(18,iy-1,is)+t7(40,iy-1,is)   !energy consumption for commercial light trucks & buses -- quads
        trnen_py(5)=t7(19,iy-1,is)        !energy consumption for heavytrucks -- quads
        trnen_py(6)=t7(20,iy-1,is)        !energy consumption for air -- quads
        trnen_py(7)=t7(22,iy-1,is)+t7(41,iy-1,is)   !energy consumption for rail -- quads
        trnen_py(8)=t7(42,iy-1,is)+t7(43,iy-1,is)+t7(44,iy-1,is)      !energy consumption for water -- quads
        trnen_py(9)=t7(23,iy-1,is)+t7(21,iy-1,is)+t7(24,iy-1,is)      !energy consumption for pipeline fuel, military & lubricants -- quads
        trnen_py(10)=t2(itotal_nbr_tran,11,iy-1,is)-trnen_py(1)-trnen_py(2)-trnen_py(3)-trnen_py(4)-trnen_py(5) &
                    -trnen_py(6)-trnen_py(7)-trnen_py(8)-trnen_py(9) !energy consumption for other, computed as a residual -- quads

        do iline_nbr = 1,10
          if(iprintflag==1) then
            Write (iprintfile,98)"trans energy",trnen_cy(iline_nbr)
            Write (iprintfile,98)"trans driver",trdrv_cy(iline_nbr)
          endif
                    if(iprintflag==99) then
                     write (iprintfile,97) "TranTrill",IY,IR,IB,iline_nbr,  &
                                               trnen_cy(iline_nbr),   t2(itotal_nbr_tran,11,iy,is),    &
                                               trnen_py(iline_nbr),   t2(itotal_nbr_tran,11,iy-1,is),  &
                                               trnen_cy(iline_nbr),  trdrv_cy(iline_nbr),   &
                                               trnen_py(iline_nbr),  trdrv_py(iline_nbr)
                    endif


          if (t2(itotal_nbr,11,iy,is)<=0.)    cycle
          if (t2(itotal_nbr,11,iy-1,is)<=0.)  cycle
          if (trnen_cy(iline_nbr)<=0.)        cycle
          if (trnen_py(iline_nbr)<=0.)        cycle
          if (trdrv_cy(iline_nbr)<=0.)        cycle
          if (trdrv_py(iline_nbr)<=0.)        cycle

! Diagnostic rollup of transportation energy
          Trn_energy = Trn_energy + trnen_cy(iline_nbr)

          if(isnan((trnen_cy(iline_nbr)/trdrv_cy(iline_nbr))/(trnen_py(iline_nbr)/trdrv_py(iline_nbr))))  then
                T31(7,IY,IS)=-10.
                T31(5,IY,IS)=-10.
               else
            T31(7,IY,IS)= T31(7,IY,IS) + 0.5 * &
              (trnen_cy(iline_nbr)/t2(itotal_nbr,11,iy,is)+ &
               trnen_py(iline_nbr)/t2(itotal_nbr,11,iy-1,is)) * &
               log((trnen_cy(iline_nbr)/trdrv_cy(iline_nbr))/(trnen_py(iline_nbr)/trdrv_py(iline_nbr)))

            T31(5,IY,IS)= T31(5,IY,IS) + 0.5 * &
              (trnen_cy(iline_nbr)/t2(itotal_nbr_tran,11,iy,is)+ &
               trnen_py(iline_nbr)/t2(itotal_nbr_tran,11,iy-1,is)) * &
               log((trnen_cy(iline_nbr)/trdrv_cy(iline_nbr))/(trnen_py(iline_nbr)/trdrv_py(iline_nbr)))

           endif

        enddo

             if(iprintflag==1 .OR. iprintflag==99) Write (iprintfile,*) "Transportation Energy ", Trn_Energy, " IY ", IY+1989, " FTAB ", t2(itotal_nbr_tran,11,iy,is)


!   current year drivers (VMT, TMT, seat miles, etc...)
!       calculate VMT for cars, light trucks and motorcycles, all light duty vehicles
!       5.24 = avg. mmbtu of a 42 gallon barrel of fuel (with weights for diesel--5.825-- and gasoline--5.234--)
        trdrv_cy(1)=t50(42,iy,is)*t45(2,iy,is)/((5.24/42)/1000000.)   !VMT measure for cars
        trdrv_cy(2)=t50(43,iy,is)*t45(3,iy,is)/((5.24/42)/1000000.)   !VMT measure for light trucks
        trdrv_cy(3)=50.*t45(4,iy,is)/((5.234/42)/1000000.)            !VMT measure for motorcycles assume 50 mpg
        trdrv_cy(4)=t7(2,iy,is)         !com. light trucks
        trdrv_cy(5)=t7(3,iy,is)         !tmt freight trucks
        trdrv_cy(6)=t7(4,iy,is)         !seat miles for air transportation
        trdrv_cy(7)=t7(5,iy,is)         !tmt for rail
        trdrv_cy(8)=t7(6,iy,is)         !tmt for water
        trdrv_cy(9)=t34(1,mnumcr,iy,is)        !RGDP for pipeline fuel
        trdrv_cy(10)=t34(1,mnumcr,iy,is)       !RGDP for lubricants
!  current year carbon emissions
!   adjust LDV emissions for fuel mix, espically for ethanol (temporarily use postitions 4 and 5 for scaling)
        trnen_cy(4)=                                  &
           T46( 1,IY,IS)/1000.*emgtr(iy)*CarbCnvFac + & !mg
           T46( 3,IY,IS)/1000.*0.*CarbCnvFac        + & !eth (eettr?)
           T46( 4,IY,IS)/1000.*engtr(iy)*CarbCnvFac + & !cng
           T46( 5,IY,IS)/1000.*elgtr(iy)*CarbCnvFac + & !lg
           T46( 6,IY,IS)/1000.*ElecCarb_CY          + & !el
           T46( 7,IY,IS)/1000.*0.*CarbCnvFac        + & !hy
           T46( 8,IY,IS)/1000.*edstr(iy)*CarbCnvFac     !ds  !   meth is out T46( 2,IY,IS)
        trnen_cy(1)=t45(2,iy,is)/1000.*emgtr(iy)*CarbCnvFac                                               !carbon emissions for cars -- convert trills to quads
        trnen_cy(2)=t45(3,iy,is)/1000.*emgtr(iy)*CarbCnvFac                                               !carbon emissions for light trucks -- convert trills to quads
        trnen_cy(3)=t45(4,iy,is)/1000.*emgtr(iy)*CarbCnvFac                                               !carbon emissions for motorcycles -- convert trills to quads
        trnen_cy(5)=trnen_cy(1)+trnen_cy(2)+trnen_cy(3)
        trnen_cy(1)=trnen_cy(1)*trnen_cy(4)/trnen_cy(5)
        trnen_cy(2)=trnen_cy(2)*trnen_cy(4)/trnen_cy(5)
        trnen_cy(3)=trnen_cy(3)*trnen_cy(4)/trnen_cy(5)
!        Write (iprintfile,*) "tran by fuel ", trnen_cy(4)
!        Write (iprintfile,*) "tran by ldvvehtype ", trnen_cy(5)
        trnen_cy(4)=(t7(18,iy,is)+t7(40,iy,is))*emgtr(iy)*CarbCnvFac                                      !carbon emissions for commercial light trucks & busses -- quads
!        trnen_cy(5)=t7(19,iy,is)*edstr(iy)*CarbCnvFac                                                     !carbon emissions for heavytrucks -- quads
        trnen_cy(5)=                                   & !by fuel for heavy trucks
           T46( 11,IY,IS)/1000.*emgtr(iy)*CarbCnvFac + & !mg
           T46( 12,IY,IS)/1000.*edstr(iy)*CarbCnvFac + & !ds
           T46( 13,IY,IS)/1000.*engtr(iy)*CarbCnvFac + & !cng
           T46( 14,IY,IS)/1000.*elgtr(iy)*CarbCnvFac  !lg
        trnen_cy(6)=t7(20,iy,is)*ejftr(iy)*CarbCnvFac                                                     !carbon emissions for air -- quads
        trnen_cy(7)=(t7(22,iy,is)+t7(41,iy,is))*erltr(iy)*CarbCnvFac                                      !carbon emissions for rail -- quads
        trnen_cy(8)=(t7(42,iy,is)+t7(43,iy,is)+t7(44,iy,is))*erhtr(iy)*CarbCnvFac                         !carbon emissions for water -- quads
        trnen_cy(9)=t7(23,iy,is)*egptr(iy)*CarbCnvFac +t7(24,iy,is)*eottr(iy)*CarbCnvFac                  !carbon emissions for pipeline fuel -- quads
!        trnen_cy(10)=t7(21,iy,is)*ejftr(iy)*CarbCnvFac                                                    !carbon emissions for other
        trnen_cy(10)=                                   &                                  !by fuel for heavy trucks
           T46( 28,IY,IS)/1000.*ejftr(iy)*CarbCnvFac + &                                 !jf
           T46( 29,IY,IS)/1000.*erltr(iy)*CarbCnvFac + &                                 !resid
           T46( 39,IY,IS)/1000.*edstr(iy)*CarbCnvFac                                     !ds
!  prior year drivers
        trdrv_py(1)=t50(42,iy-1,is)*t45(2,iy-1,is)/((5.24/42)/1000000.) !VMT measure for cars
        trdrv_py(2)=t50(43,iy-1,is)*t45(3,iy-1,is)/((5.24/42)/1000000.) !VMT measure for light trucks
        trdrv_py(3)=50.*t45(4,iy-1,is)/((5.234/42)/1000000.)            !VMT measure for motorcycles assume 50 mpg
        trdrv_py(4)=t7(2,iy-1,is)        !com. light trucks
        trdrv_py(5)=t7(3,iy-1,is)        !tmt freight trucks
        trdrv_py(6)=t7(4,iy-1,is)        !seat miles for air transportation
        trdrv_py(7)=t7(5,iy-1,is)        !tmt for rail
        trdrv_py(8)=t7(6,iy-1,is)        !tmt for water
        trdrv_py(9)=t34(1,mnumcr,iy-1,is)       !RGDP for pipeline fuel
        trdrv_py(10)=t34(1,mnumcr,iy-1,is)      !RGDP for lubricants
!  prior year carbon emissions
!   adjust LDV emissions for fuel mix, espically for ethanol
        trnen_py(4)=                                      &
           T46( 1,iy-1,IS)/1000.*emgtr(iy-1)*CarbCnvFac + & !mg
           T46( 3,iy-1,IS)/1000.*0.*CarbCnvFac        + &   !eth (eettr?)
           T46( 4,iy-1,IS)/1000.*engtr(iy-1)*CarbCnvFac + & !cng
           T46( 5,iy-1,IS)/1000.*elgtr(iy-1)*CarbCnvFac + & !lg
           T46( 6,iy-1,IS)/1000.*ElecCarb_py          + &   !el
           T46( 7,iy-1,IS)/1000.*0.*CarbCnvFac        + &   !hy
           T46( 8,iy-1,IS)/1000.*edstr(iy-1)*CarbCnvFac     !ds  !   meth is out T46( 2,iy-1,IS)
        trnen_py(1)=t45(2,iy-1,is)/1000.*emgtr(iy-1)*CarbCnvFac !carbon emissions for cars -- convert trills to quads
        trnen_py(2)=t45(3,iy-1,is)/1000.*emgtr(iy-1)*CarbCnvFac !carbon emissions for light trucks -- convert trills to quads
        trnen_py(3)=t45(4,iy-1,is)/1000.*emgtr(iy-1)*CarbCnvFac !carbon emissions for motorcycles -- convert trills to quads
        trnen_py(5)=trnen_py(1)+trnen_py(2)+trnen_py(3)                                   !before scaling
        trnen_py(1)=trnen_py(1)*trnen_py(4)/trnen_py(5)                                   !scaled
        trnen_py(2)=trnen_py(2)*trnen_py(4)/trnen_py(5)                                   !scaled
        trnen_py(3)=trnen_py(3)*trnen_py(4)/trnen_py(5)                                   !scaled
        trnen_py(4)=(t7(18,iy-1,is)+t7(40,iy-1,is))*emgtr(iy-1)*CarbCnvFac   !carbon emissions for commercial light trucks & busses -- quads
!        trnen_py(5)=t7(19,iy-1,is)*edstr(iy-1)*CarbCnvFac       !carbon emissions for heavytrucks -- quads
        trnen_py(5)=                                   &                                  !by fuel for heavy trucks
           T46( 11,IY-1,IS)/1000.*emgtr(iy-1)*CarbCnvFac + &                                 !mg
           T46( 12,IY-1,IS)/1000.*edstr(iy-1)*CarbCnvFac + &                                 !ds
           T46( 13,IY-1,IS)/1000.*engtr(iy-1)*CarbCnvFac + &                                 !cng
           T46( 14,IY-1,IS)/1000.*elgtr(iy-1)*CarbCnvFac                                     !lg
        trnen_py(6)=t7(20,iy-1,is)*ejftr(iy-1)*CarbCnvFac       !carbon emissions for air -- quads
        trnen_py(7)=(t7(22,iy-1,is)+t7(41,iy-1,is))*erltr(iy-1)*CarbCnvFac             !carbon emissions for rail -- quads
        trnen_py(8)=(t7(42,iy-1,is)+t7(43,iy-1,is)+t7(44,iy-1,is))*erhtr(iy-1)*CarbCnvFac !carbon emissions for water -- quads
        trnen_py(9)=(t7(23,iy-1,is)*egptr(iy-1)+t7(24,iy-1,is)*eottr(iy))*CarbCnvFac      !carbon emissions for pipeline fuel -- quads
!        trnen_py(10)=t7(21,iy-1,is)*ejftr(iy)*CarbCnvFac                                  !carbon emissions for military
        trnen_py(10)=                                   &                                  !by fuel for heavy trucks
           T46( 28,IY-1,IS)/1000.*ejftr(iy-1)*CarbCnvFac + &                                 !jf
           T46( 29,IY-1,IS)/1000.*erltr(iy-1)*CarbCnvFac + &                                 !resid
           T46( 39,IY-1,IS)/1000.*edstr(iy-1)*CarbCnvFac                                     !ds

        do iline_nbr = 1,10
          if(iprintflag==1) then
            Write (iprintfile,98)"trans carbon",trnen_cy(iline_nbr)
            Write (iprintfile,98)"trans driver",trdrv_cy(iline_nbr)
          endif
                    if(iprintflag==99) then
                     write (iprintfile,97) "TranCarb",IY,IR,IB,iline_nbr,  &
                                               trnen_cy(iline_nbr),  t17(itotal_nbr_carb_tran,11,iy,is),    &
                                               trnen_py(iline_nbr),  t17(itotal_nbr_carb_tran,11,iy-1,is),  &
                                               trnen_cy(iline_nbr),  trdrv_cy(iline_nbr),   &
                                               trnen_py(iline_nbr),  trdrv_py(iline_nbr)
                    endif

          if (t17(itotal_nbr_carb,11,iy,is)<=0.)    cycle
          if (t17(itotal_nbr_carb,11,iy-1,is)<=0.)  cycle
          if (trnen_cy(iline_nbr)<=0.)        cycle
          if (trnen_py(iline_nbr)<=0.)        cycle
          if (trdrv_cy(iline_nbr)<=0.)        cycle
          if (trdrv_py(iline_nbr)<=0.)        cycle

! Diagnostic rollup of transportation carbon

          Trn_carbon = Trn_carbon + trnen_cy(iline_nbr)

          if(isnan((trnen_cy(iline_nbr)/trdrv_cy(iline_nbr))/(trnen_py(iline_nbr)/trdrv_py(iline_nbr))))  then
                T31(13,IY,IS)=-10.
                T31(12,IY,IS)=-10.
               else
            T31(13,IY,IS)= T31(13,IY,IS) + 0.5 * &
              (trnen_cy(iline_nbr)/t17(itotal_nbr_carb,11,iy,is)+ &
               trnen_py(iline_nbr)/t17(itotal_nbr_carb,11,iy-1,is)) * &
               log((trnen_cy(iline_nbr)/trdrv_cy(iline_nbr))/(trnen_py(iline_nbr)/trdrv_py(iline_nbr)))

            T31(12,IY,IS)= T31(12,IY,IS) + 0.5 * &
              (trnen_cy(iline_nbr)/t17(itotal_nbr_carb_tran,11,iy,is)+ &
               trnen_py(iline_nbr)/t17(itotal_nbr_carb_tran,11,iy-1,is)) * &
               log((trnen_cy(iline_nbr)/trdrv_cy(iline_nbr))/(trnen_py(iline_nbr)/trdrv_py(iline_nbr)))
          endif
        enddo

              if(iprintflag==1 .OR. iprintflag==99) Write (iprintfile,*) "Transportation Carbon ",Trn_Carbon, " IY ", IY+1989, " FTAB ", t17(itotal_nbr_carb_tran,11,iy,is)


!  Electric Generators
        if(iprintflag==1 .OR. iprintflag==99) Write (iprintfile,*) "Electric Generator Diagnostics for year ", iy

  ! line numbers and filters

        iline_nbr=77                                 ! 77
!  drivers for electric utilities are total sales
        driver_cy=t8(24,iy,is)
        driver_py=t8(24,iy-1,is)

                   if(iprintflag==99) then
                     write (iprintfile,97) "Elec",IY,IR,IB,iline_nbr,  &
                                               t2(iline_nbr,11,iy,is),     t2(itotal_nbr_util,11,iy,is),    &
                                               t2(iline_nbr,11,iy-1,is),   t2(itotal_nbr_util,11,iy-1,is),  &
                                               t2(iline_nbr,11,iy,is),     driver_cy,   &
                                               t2(iline_nbr,11,iy-1,is),   driver_py
                    endif


        if (t2(itotal_nbr,11,iy,is)<=0.)     cycle
        if (t2(itotal_nbr,11,iy-1,is)<=0.)   cycle
        if (t2(iline_nbr,11,iy,is)<=0.)      cycle
        if (t2(iline_nbr,11,iy-1,is)<=0.)    cycle
        if (driver_cy<=0.)                   cycle
        if (driver_py<=0.)                   cycle

! Diagnostic rollup of electric utility energy
        Utl_energy = Utl_energy + t2(iline_nbr,11,iy,is)

        if(isnan((t2(iline_nbr,11,iy,is)/driver_cy)/(t2(iline_nbr,11,iy-1,is)/driver_py)))  then
                T31(7,IY,IS)=-10.
                T31(6,IY,IS)=-10.
         else
          T31(7,IY,IS)= T31(7,IY,IS) + &
            0.5 * (t2(iline_nbr,11,iy,is)/t2(itotal_nbr,11,iy,is)+ &
                    t2(iline_nbr,11,iy-1,is)/t2(itotal_nbr,11,iy-1,is)) &
                *   log((t2(iline_nbr,11,iy,is)/driver_cy)/(t2(iline_nbr,11,iy-1,is)/driver_py))

          T31(6,IY,IS)= T31(6,IY,IS) + &
             0.5 * (t2(iline_nbr,11,iy,is)/t2(itotal_nbr_util,11,iy,is)+ &
                    t2(iline_nbr,11,iy-1,is)/t2(itotal_nbr_util,11,iy-1,is)) &
                 *  log((t2(iline_nbr,11,iy,is)/driver_cy)/(t2(iline_nbr,11,iy-1,is)/driver_py))

                    endif

        if(iprintflag==1) then
          Write (iprintfile,*) "Electric Utility Diagnostics"
          do ir =1,nrows(8); if(irows(ir,8)==24)irpntr=ir; enddo              !table 8 data
          Write (iprintfile,98) rowlab(irpntr,8), t8(24,11:41,is)              !sales
          do ir =1,nrows(2); if(irows(ir,2)==77)irpntr=ir; enddo              !table 2 data
          Write (iprintfile,98) rowlab(irpntr,7), t2(iline_nbr,11,11:41,is)    !energy use
        endif

              if(iprintflag==1 .OR. iprintflag==99) Write (iprintfile,*) "Elec Generator Energy ",Utl_Energy, " IY ", IY+1989, " FTAB ", t2(itotal_nbr_util,11,iy,is)


!  Industrial
        if(iprintflag==1 .OR. iprintflag==99) Write (iprintfile,*) "Industrial Diagnostics for year ", iy
!   Set up dummy ftable for industrial summary results
!    "tab34" values are energy, "drv35" values are industrial output indicators
! Energy Consumption
     ! current year
        tab34_cy(2)= t35(10,iy,is)/1000.-t35(45,iy,is)/1000.       !refining coal-to-liquids coal use = steam coal minus ethanol use
        tab34_cy(21)= t35(47,iy,is)/1000.+t35(48,iy,is)/1000.      !biofuels refining consumption plus losses
        tab34_cy(1)= t35(12,iy,is)/1000.-tab34_cy(2)-tab34_cy(21)  !traditional petroleum refining
        tab34_cy(3)= t36(11,iy,is)/1000.  !food industry
        tab34_cy(4)= t37(11,iy,is)/1000.  !paper
        tab34_cy(5)= t38(15,iy,is)/1000.  !bulk chemicals
        tab34_cy(6)= t39(9,iy,is)/1000.   !glass
        tab34_cy(7)= t40(9,iy,is)/1000.   !cement
        tab34_cy(8)= t41(12,iy,is)/1000.  !iron & steel
        tab34_cy(9)= t42(10,iy,is)/1000.  !aluminum
        tab34_cy(10)= t43(10,iy,is)/1000. !agriculture
        tab34_cy(11)= t43(22,iy,is)/1000. !construction
        tab34_cy(12)= t43(37,iy,is)/1000. !mining
        tab34_cy(13)=t139(10,iy,is)/1000. !fabricated metal
        tab34_cy(14)=t139(20,iy,is)/1000. !machinery
        tab34_cy(15)=t139(30,iy,is)/1000. !computers
        tab34_cy(16)=t139(40,iy,is)/1000. !transportation equip
        tab34_cy(17)=t139(50,iy,is)/1000. !electronics
        tab34_cy(18)=t140(10,iy,is)/1000. !wood
        tab34_cy(19)=t140(20,iy,is)/1000. !plastics
        tab34_cy(20)=t140(30,iy,is)/1000. !balance of manuf
   ! Prior year
        tab34_py(2)= t35(10,iy-1,is)/1000.-t35(45,iy-1,is)/1000.       !refining coal-to-liquids coal use = steam coal minus ethanol use
        tab34_py(21)= t35(47,iy-1,is)/1000.+t35(48,iy-1,is)/1000.      !biofuels refining consumption plus losses
        tab34_py(1)= t35(12,iy-1,is)/1000.-tab34_py(2)-tab34_py(21)    !traditional petroleum refining
        tab34_py(3)= t36(11,iy-1,is)/1000.
        tab34_py(4)= t37(11,iy-1,is)/1000.
        tab34_py(5)= t38(15,iy-1,is)/1000.
        tab34_py(6)= t39(9,iy-1,is)/1000.
        tab34_py(7)= t40(9,iy-1,is)/1000.
        tab34_py(8)= t41(12,iy-1,is)/1000.
        tab34_py(9)= t42(10,iy-1,is)/1000.
        tab34_py(10)= t43(10,iy-1,is)/1000.
        tab34_py(11)=t43(22,iy-1,is)/1000.
        tab34_py(12)=t43(37,iy-1,is)/1000.
        tab34_py(13)=t139(10,iy-1,is)/1000.
        tab34_py(14)=t139(20,iy-1,is)/1000.
        tab34_py(15)=t139(30,iy-1,is)/1000.
        tab34_py(16)=t139(40,iy-1,is)/1000.
        tab34_py(17)=t139(50,iy-1,is)/1000.
        tab34_py(18)=t140(10,iy-1,is)/1000.
        tab34_py(19)=t140(20,iy-1,is)/1000.
        tab34_py(20)=t140(30,iy-1,is)/1000.
  ! Drivers
   ! current year
        drv35_cy(1)= t35(1,iy,is)
        drv35_cy(2)= t11(35,iy,is)*1000. !CTL liquid production
        drv35_cy(3)= t36(1,iy,is)
        drv35_cy(4)= t37(1,iy,is)
        drv35_cy(5)= t38(1,iy,is)
        drv35_cy(6)= t39(1,iy,is)
        drv35_cy(7)= t40(1,iy,is)
        drv35_cy(8)= t41(1,iy,is)
        drv35_cy(9)= t42(1,iy,is)
        drv35_cy(10)= t43(11,iy,is)
        drv35_cy(11)= t43(23,iy,is)
        drv35_cy(12)= t43(38,iy,is)
        drv35_cy(13)= t139(51,iy,is)
        drv35_cy(14)= t139(52,iy,is)
        drv35_cy(15)= t139(53,iy,is)
        drv35_cy(16)= t139(54,iy,is)
        drv35_cy(17)= t139(55,iy,is)
        drv35_cy(18)= t140(31,iy,is)
        drv35_cy(19)= t140(32,iy,is)
        drv35_cy(20)= t140(33,iy,is)
        drv35_cy(21)= (t11(37,iy,is)+t11(45,iy,is)+t11(49,iy,is))*1000.     !Biofuels (ethanol, biodiesel & biomass)
   ! Prior year
        drv35_py(1)= t35(1,iy-1,is)
        drv35_py(2)= t11(35,iy-1,is)*1000.     !CTL liquid production
        drv35_py(3)= t36(1,iy-1,is)
        drv35_py(4)= t37(1,iy-1,is)
        drv35_py(5)= t38(1,iy-1,is)
        drv35_py(6)= t39(1,iy-1,is)
        drv35_py(7)= t40(1,iy-1,is)
        drv35_py(8)= t41(1,iy-1,is)
        drv35_py(9)= t42(1,iy-1,is)
        drv35_py(10)= t43(11,iy-1,is)
        drv35_py(11)= t43(23,iy-1,is)
        drv35_py(12)= t43(38,iy-1,is)
        drv35_py(13)= t139(51,iy-1,is)
        drv35_py(14)= t139(52,iy-1,is)
        drv35_py(15)= t139(53,iy-1,is)
        drv35_py(16)= t139(54,iy-1,is)
        drv35_py(17)= t139(55,iy-1,is)
        drv35_py(18)= t140(31,iy-1,is)
        drv35_py(19)= t140(32,iy-1,is)
        drv35_py(20)= t140(33,iy-1,is)
        drv35_py(21)= (t11(37,iy-1,is)+t11(45,iy-1,is)+t11(49,iy-1,is))*1000.  !Biofuels (ethanol, biodiesel & biomass)

     ! line numbers and filters
        ifirst_nbr=1  ;  ilast_nbr=21

        do iline_nbr = ifirst_nbr,ilast_nbr

          driver_cy=drv35_cy(iline_nbr)
          driver_py=drv35_py(iline_nbr)

          if (t2(itotal_nbr,11,iy,is)<=0.)    cycle
          if (t2(itotal_nbr,11,iy-1,is)<=0.)  cycle
          if (tab34_cy(iline_nbr)<=0.)        cycle
          if (tab34_py(iline_nbr)<=0.)        cycle
          if (driver_cy<=0.)                  cycle
          if (driver_py<=0.)                  cycle

! Diagnostic rollup of industrial energy
          Ind_energy = Ind_energy + tab34_cy(iline_nbr)

        if(isnan( (tab34_cy(iline_nbr)/driver_cy)/(tab34_py(iline_nbr)/driver_py) ))     then
                T31(7,IY,IS)=-10.
                T31(4,IY,IS)=-10.
         else

             T31(7,IY,IS)= T31(7,IY,IS)  +                                 &
               0.5 * ( tab34_cy(iline_nbr)/t2(itotal_nbr,11,iy,is)+      &
                       tab34_py(iline_nbr)/t2(itotal_nbr,11,iy-1,is) )   &
                  *    log( (tab34_cy(iline_nbr)/driver_cy)/(tab34_py(iline_nbr)/driver_py) )

             T31(4,IY,IS)= T31(4,IY,IS)  +                                      &
               0.5 * ( tab34_cy(iline_nbr)/t2(itotal_nbr_ind,11,iy,is)+      &
                       tab34_py(iline_nbr)/t2(itotal_nbr_ind,11,iy-1,is) )   &
                  *    log( (tab34_cy(iline_nbr)/driver_cy)/(tab34_py(iline_nbr)/driver_py) )

                    if(iprintflag2==1) then
                     write (iprintfile,97) "Indy",IY,IR,IB,iline_nbr,  &
                                               tab34_cy(iline_nbr),   t2(itotal_nbr,11,iy,is),    &
                                               tab34_py(iline_nbr),   t2(itotal_nbr,11,iy-1,is),  &
                                               tab34_cy(iline_nbr),  driver_cy,   &
                                               tab34_py(iline_nbr),  driver_py
                    endif


          endif
        enddo
        if(iprintflag==99) write (iprintfile,*) ""  ! split years in the output file

        if(iprintflag==1) then
          Write (iprintfile,*) "Industrial Diagnostics"
        !real output drivers
          iline_nbr=1; itab=35
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t35(iline_nbr,11:41,is)
          iline_nbr=1; itab=36
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t36(iline_nbr,11:41,is)
          iline_nbr=1; itab=37
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t37(iline_nbr,11:41,is)
          iline_nbr=1; itab=38
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t38(iline_nbr,11:41,is)
          iline_nbr=1; itab=39
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t39(iline_nbr,11:41,is)
          iline_nbr=1; itab=40
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t40(iline_nbr,11:41,is)
          iline_nbr=1; itab=41
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t41(iline_nbr,11:41,is)
          iline_nbr=1; itab=42
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t42(iline_nbr,11:41,is)
          iline_nbr=11; itab=43
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t43(iline_nbr,11:41,is)
          iline_nbr=23; itab=43
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t43(iline_nbr,11:41,is)
          iline_nbr=38; itab=43
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t43(iline_nbr,11:41,is)
          iline_nbr=51; itab=139
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t139(iline_nbr,11:41,is)
          iline_nbr=52; itab=139
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t139(iline_nbr,11:41,is)
          iline_nbr=53; itab=139
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t139(iline_nbr,11:41,is)
          iline_nbr=54; itab=139
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t139(iline_nbr,11:41,is)
          iline_nbr=55; itab=139
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t139(iline_nbr,11:41,is)
          iline_nbr=31; itab=140
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t140(iline_nbr,11:41,is)
          iline_nbr=32; itab=140
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t140(iline_nbr,11:41,is)
          iline_nbr=33; itab=140
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t140(iline_nbr,11:41,is)
          iline_nbr=1; itab=34
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t34(iline_nbr,mnumcr,11:41,is)
        ! consumption
          iline_nbr=12; itab=35
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t35(iline_nbr,11:41,is)
          iline_nbr=11; itab=36
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t36(iline_nbr,11:41,is)
          iline_nbr=11; itab=37
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t37(iline_nbr,11:41,is)
          iline_nbr=15; itab=38
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t38(iline_nbr,11:41,is)
          iline_nbr=9; itab=39
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t39(iline_nbr,11:41,is)
          iline_nbr=9; itab=40
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t40(iline_nbr,11:41,is)
          iline_nbr=12; itab=41
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t41(iline_nbr,11:41,is)
          iline_nbr=10; itab=42
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t42(iline_nbr,11:41,is)
          iline_nbr=10; itab=43
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t43(iline_nbr,11:41,is)
          iline_nbr=22; itab=43
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t43(iline_nbr,11:41,is)
          iline_nbr=37; itab=43
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t43(iline_nbr,11:41,is)
          iline_nbr=10; itab=139
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t139(iline_nbr,11:41,is)
          iline_nbr=20; itab=139
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t139(iline_nbr,11:41,is)
          iline_nbr=30; itab=139
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t139(iline_nbr,11:41,is)
          iline_nbr=40; itab=139
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t139(iline_nbr,11:41,is)
          iline_nbr=50; itab=139
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t139(iline_nbr,11:41,is)
          iline_nbr=10; itab=140
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t140(iline_nbr,11:41,is)
          iline_nbr=20; itab=140
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t140(iline_nbr,11:41,is)
          iline_nbr=30; itab=140
          do ir =1,nrows(itab); if(irows(ir,itab)==iline_nbr)irpntr=ir; enddo    !table 34 - 43 data
          Write (iprintfile,98) rowlab(irpntr,itab), t140(iline_nbr,11:41,is)

          Write (iprintfile,98) "Other Industrial (compute as residual)"

          Write (iprintfile,98) "Total Industrial", t2(39,11,11:41,is)    !write total to compute other as residual
        endif

             if(iprintflag==1 .OR. iprintflag==99) Write (iprintfile,*) "Industrial Energy ",Ind_Energy, " IY ", IY+1989, " FTAB ", t2(itotal_nbr_ind,11,iy,is)


! Industrial Carbon

!        Carbon Variable Nomenclature
!            (i,j,k) -  Sector, Fuel Number, Year (2=cy, 1=py)
!        Drivers (i,j) - Sector, Year

!     Table 35. Refining
          IndCrb(1,1,2) = REFCON(ixRF,5,IY)* ErlIN(iy)*.001*CarbCnvFac
          IndCrb(1,2,2) = REFCON(ixDS,5,IY)* EdsIN(iy)*.001*CarbCnvFac
          IndCrb(1,3,2) = REFCON(ixLG,5,IY)* ElgIN(iy)*.001*CarbCnvFac
          IndCrb(1,4,2) = REFCON(ixPC,5,IY)* EpcIN(iy)*.001*CarbCnvFac
          IndCrb(1,5,2) = REFCON(ixSG,5,IY)* EsgIN(iy)*.001*CarbCnvFac
          IndCrb(1,6,2) = REFCON(ixOP,5,IY)* EotIN(iy)*.001*CarbCnvFac
          IndCrb(1,7,2) = REFCON(ixNG,5,IY)* EngIN(iy)*.001*CarbCnvFac
          IndCrb(1,8,2) = REFCON(ixCL,5,IY)* EclIN(iy)*.001*CarbCnvFac
          IndCrb(1,9,2) = REFCON(ixEL,5,IY)* ElecCarb_CY*.001
          IndDriver(1,2)= MC_REVIND(11,25,IY)

!     Table 36. Food Industry
          IndCrb(2,1,2) = FOODCON(ixRF,5,IY)* ErlIN(iy)*.001*CarbCnvFac
          IndCrb(2,2,2) = FOODCON(ixDS,5,IY)* EdsIN(iy)*.001*CarbCnvFac
          IndCrb(2,3,2) = FOODCON(ixLG,5,IY)* ElgIN(iy)*.001*CarbCnvFac
          IndCrb(2,4,2) = FOODCON(ixOP,5,IY)* EotIN(iy)*.001*CarbCnvFac
          IndCrb(2,5,2) = FOODCON(ixNG,5,IY)* EngIN(iy)*.001*CarbCnvFac
          IndCrb(2,6,2) = FOODCON(ixCL,5,IY)* EclIN(iy)*.001*CarbCnvFac
          IndCrb(2,7,2) = FOODCON(ixRN,5,IY)*  0.
          IndCrb(2,8,2) = FOODCON(ixEL,5,IY)* ElecCarb_CY*.001
          IndDriver(2,2)= MC_REVIND(11,1,IY)

!     Table 37. Paper Industry
          IndCrb(3,1,2) = PAPERCON(ixRF,5,IY) * ErlIN(iy)*.001*CarbCnvFac
          IndCrb(3,2,2) = PAPERCON(ixDS,5,IY) * EdsIN(iy)*.001*CarbCnvFac
          IndCrb(3,3,2) = PAPERCON(ixLG,5,IY) * ElgIN(iy)*.001*CarbCnvFac
          IndCrb(3,4,2) = PAPERCON(ixNG,5,IY) * EngIN(iy)*.001*CarbCnvFac
          IndCrb(3,5,2) = PAPERCON(ixCL,5,IY) * EclIN(iy)*.001*CarbCnvFac
          IndCrb(3,6,2) = PAPERCON(ixRN,5,IY) * 0.
          IndCrb(3,7,2) = PAPERCON(ixEL,5,IY) * ElecCarb_CY*.001
          IndDriver(3,2)= MC_REVIND(11,10,IY)

!     Table 38. Bulk Chemical Industry
!       --- HEAT AND POWER
           IndCrb(4,1,2) = CHEMCON(ixRF,5,IY) * ErlIN(iy) *.001*CarbCnvFac
           IndCrb(4,2,2) = CHEMCON(ixDS,5,IY) * EdsIN(iy) *.001*CarbCnvFac
           IndCrb(4,3,2) = CHEMCON(ixLG,5,IY) * ElgIN(iy) *.001*CarbCnvFac
           IndCrb(4,4,2) = CHEMCON(ixOP,5,IY) * EotIN(iy) *.001*CarbCnvFac + CHEMCON(ixPC,5,IY) * EpcIN(iy) *.001*CarbCnvFac
           IndCrb(4,5,2) = CHEMCON(ixNG,5,IY) * EngIN(iy) *.001*CarbCnvFac
           IndCrb(4,6,2) = CHEMCON(ixCL,5,IY) * EclIN(iy) *.001*CarbCnvFac
           IndCrb(4,7,2) = CHEMCON(ixEL,5,IY) * ElecCarb_CY*.001
!         --- FEEDSTOCK
           IndCrb(4,8,2) = CHEMCON(ixLF,5,IY) * Enqlgpf(iy)*.001*CarbCnvFac !lpg feedstocks
           IndCrb(4,9,2) = CHEMCON(ixPF,5,IY) * EpfIN(iy)*.001*CarbCnvFac   !pet feedstocks
           IndCrb(4,10,2)= CHEMCON(ixNF,5,IY) * Enqngpf(iy)*.001*CarbCnvFac !nat gas feedstocks
           IndDriver(4,2)=T38(1,IY,IS)

!     Table 39. Glass Industry
           IndCrb(5,1,2) = GLASSCON(ixRF,5,IY) * ErlIN(iy)*.001*CarbCnvFac
           IndCrb(5,2,2) = GLASSCON(ixDS,5,IY) * EdsIN(iy)*.001*CarbCnvFac
           IndCrb(5,3,2) = GLASSCON(ixLG,5,IY) * ElgIN(iy)*.001*CarbCnvFac
           IndCrb(5,4,2) = GLASSCON(ixNG,5,IY) * EngIN(iy)*.001*CarbCnvFac
           IndCrb(5,5,2) = GLASSCON(ixCL,5,IY) * EclIN(iy)*.001*CarbCnvFac
           IndCrb(5,6,2) = GLASSCON(ixEL,5,IY) * ElecCarb_CY*.001
           IndDriver(5,2)=MC_REVIND(11,28,IY)

!     Table 40. Cement Industry
           IndCrb(6,1,2) = CEMENTCON(ixRF,5,IY) * ErlIN(iy)*.001*CarbCnvFac
           IndCrb(6,2,2) = CEMENTCON(ixDS,5,IY) * EdsIN(iy)*.001*CarbCnvFac
           IndCrb(6,3,2) = CEMENTCON(ixOP,5,IY) * EotIN(iy)*.001*CarbCnvFac
           IndCrb(6,4,2) = CEMENTCON(ixNG,5,IY) * EngIN(iy)*.001*CarbCnvFac
           IndCrb(6,5,2) = CEMENTCON(ixCL,5,IY) * EclIN(iy)*.001*CarbCnvFac
           IndCrb(6,6,2) = CEMENTCON(ixEL,5,IY) * ElecCarb_CY*.001
           IndDriver(6,2)=MC_REVIND(11,30,IY)

!     Table 41. Iron and Steel Industry                                     IndCrb(7,1,2) = STEELCON(ixDS,5,IY) * EdsIN(iy)*.001*CarbCnvFac
           IndCrb(7,2,2) = STEELCON(ixRF,5,IY) * ErlIN(iy)*.001*CarbCnvFac
           IndCrb(7,3,2) = STEELCON(ixOP,5,IY) * EotIN(iy)*.001*CarbCnvFac
           IndCrb(7,4,2) = STEELCON(ixNG,5,IY) * EngIN(iy)*.001*CarbCnvFac
           IndCrb(7,5,2) = STEELCON(ixMC,5,IY) * EmcIN(iy)*.001*CarbCnvFac
           IndCrb(7,6,2) = STEELCON(ixCI,5,IY) * EpcIN(iy)*.001*CarbCnvFac
           IndCrb(7,7,2) = STEELCON(ixCL,5,IY) * EclIN(iy)*.001*CarbCnvFac
           IndCrb(7,8,2) = STEELCON(ixEL,5,IY) * ElecCarb_CY*.001
           IndDriver(7,2)=MC_REVIND(11,33,IY)

!     Table 42. Aluminum Industry
           IndCrb(8,1,2) = ALUMCON(ixDS,5,IY) * EdsIN(iy)*.001*CarbCnvFac
           IndCrb(8,2,2) = ALUMCON(ixLG,5,IY) * ElgIN(iy)*.001*CarbCnvFac
           IndCrb(8,3,2) = ALUMCON(ixPC,5,IY) * EpcIN(iy)*.001*CarbCnvFac
           IndCrb(8,4,2) = ALUMCON(ixOP,5,IY) * EotIN(iy)*.001*CarbCnvFac
           IndCrb(8,5,2) = ALUMCON(ixNG,5,IY) * EngIN(iy)*.001*CarbCnvFac
           IndCrb(8,6,2) = ALUMCON(ixCL,5,IY) * EclIN(iy)*.001*CarbCnvFac
           IndCrb(8,7,2) = ALUMCON(ixEL,5,IY) * ElecCarb_CY*.001
           IndDriver(8,2)=MC_REVIND(11,34,IY)

!     Table 43. Other Industrial Sectors
!      Agriculture
          IndCrb(9,1,2) = AGCON(ixDS,5,IY) * EdsIN(iy)*.001*CarbCnvFac
          IndCrb(9,2,2) = AGCON(ixLG,5,IY) * ElgIN(iy)*.001*CarbCnvFac
          IndCrb(9,3,2) = AGCON(ixMG,5,IY) * EmgIN(iy)*.001*CarbCnvFac
          IndCrb(9,4,2) = AGCON(ixOP,5,IY) * EotIN(iy)*.001*CarbCnvFac
          IndCrb(9,5,2) = AGCON(ixNG,5,IY) * EngIN(iy)*.001*CarbCnvFac
          IndCrb(9,6,2) = AGCON(ixCL,5,IY) * EclIN(iy)*.001*CarbCnvFac
          IndCrb(9,7,2) = AGCON(ixRN,5,IY) * 0.
          IndCrb(9,8,2) = AGCON(ixEL,5,IY) * ElecCarb_CY*.001
          IndDriver(9,2) = MC_REVIND(11,42,IY)+MC_REVIND(11,43,IY)+MC_REVIND(11,44,IY)

!      Construction
          IndCrb(10,1,2) = CONSTCON(ixDS,5,IY) * EdsIN(iy)*.001*CarbCnvFac
          IndCrb(10,2,2) = CONSTCON(ixMG,5,IY) * EmgIN(iy)*.001*CarbCnvFac
          IndCrb(10,3,2) = CONSTCON(ixAS,5,IY) * 0.
          IndCrb(10,4,2) = CONSTCON(ixNG,5,IY) * EngIN(iy)*.001*CarbCnvFac
          IndCrb(10,5,2) = CONSTCON(ixCL,5,IY) * EclIN(iy)*.001*CarbCnvFac
          IndCrb(10,6,2) = CONSTCON(ixEL,5,IY) * ElecCarb_CY*.001
          IndDriver(10,2)=MC_REVIND(11,48,IY)

!       Mining
          IndCrb(11,1,2) = MINECON(ixRF,5,IY) * ErlIN(iy)*.001*CarbCnvFac
          IndCrb(11,2,2) = MINECON(ixDS,5,IY) * EdsIN(iy)*.001*CarbCnvFac
          IndCrb(11,3,2) = MINECON(ixMG,5,IY) * EmgIN(iy)*.001*CarbCnvFac
          IndCrb(11,4,2) = MINECON(ixOP,5,IY) * EotIN(iy)*.001*CarbCnvFac
          IndCrb(11,5,2) = MINECON(ixNG,5,IY) * EngIN(iy)*.001*CarbCnvFac
          IndCrb(11,6,2) = QLPIN(11,IY) * ElgIN(iy)*CarbCnvFac +QNGLQ(11,IY)* ElgIN(iy)*CarbCnvFac
          IndCrb(11,7,2) = MINECON(ixCL,5,IY) * EclIN(iy)*.001*CarbCnvFac
          IndCrb(11,8,2) = MINECON(ixRN,5,IY) * 0.
          IndCrb(11,9,2) = MINECON(ixEL,5,IY) * ElecCarb_CY*.001
          IndDriver(11,2) = MC_REVIND(11,45,IY)+MC_REVIND(11,46,IY)+MC_REVIND(11,47,IY)

!        --- METALS BASED DURABLES
          IndCrb(12,1,2) = (FABMETALCON(ixRF,5,IY) + &
                          MACHINECON(ixRF,5,IY) + &
                          COMPUTECON(ixRF,5,IY) + &
                        TRANEQUIPCON(ixRF,5,IY) + &
                         ELECEQUIPON(ixRF,5,IY))  * ErlIN(iy)*.001*CarbCnvFac
          IndCrb(12,2,2) = (FABMETALCON(ixDS,5,IY) + &
                          MACHINECON(ixDS,5,IY) + &
                          COMPUTECON(ixDS,5,IY) + &
                        TRANEQUIPCON(ixDS,5,IY) + &
                         ELECEQUIPON(ixDS,5,IY))  * EdsIN(iy)*.001*CarbCnvFac
          IndCrb(12,3,2) = (FABMETALCON(ixLG,5,IY) + &
                          MACHINECON(ixLG,5,IY) + &
                          COMPUTECON(ixLG,5,IY) + &
                        TRANEQUIPCON(ixLG,5,IY) + &
                         ELECEQUIPON(ixLG,5,IY) ) * ElgIN(iy)*.001*CarbCnvFac
          IndCrb(12,4,2) = (FABMETALCON(ixNG,5,IY) + &
                          MACHINECON(ixNG,5,IY) + &
                          COMPUTECON(ixNG,5,IY) + &
                        TRANEQUIPCON(ixNG,5,IY) + &
                         ELECEQUIPON(ixNG,5,IY) ) * EngIN(iy)*.001*CarbCnvFac
          IndCrb(12,5,2) = (FABMETALCON(ixCL,5,IY) + &
                          MACHINECON(ixCL,5,IY) + &
                          COMPUTECON(ixCL,5,IY) + &
                        TRANEQUIPCON(ixCL,5,IY) + &
                         ELECEQUIPON(ixCL,5,IY)) * EclIN(iy)*.001*CarbCnvFac
          IndCrb(12,6,2) = (FABMETALCON(ixRN,5,IY) + &
                          MACHINECON(ixRN,5,IY) + &
                          COMPUTECON(ixRN,5,IY) + &
                        TRANEQUIPCON(ixRN,5,IY) + &
                         ELECEQUIPON(ixRN,5,IY))  * 0.
          IndCrb(12,7,2) = (FABMETALCON(ixEL,5,IY) + &
                          MACHINECON(ixEL,5,IY) + &
                          COMPUTECON(ixEL,5,IY) + &
                        TRANEQUIPCON(ixEL,5,IY) + &
                         ELECEQUIPON(ixEL,5,IY)) * ElecCarb_CY*.001
          IndDriver(12,2) = MC_REVIND(11,36,IY)+MC_REVIND(11,37,IY)+ &
                   MC_REVIND(11,38,IY)+MC_REVIND(11,39,IY)+MC_REVIND(11,40,IY)

!        --- OTHER MANUFACTURING
          IndCrb(13,1,2) = (WOODPRODCON(ixRF,5,IY) + &
                             PLASTICCON(ixRF,5,IY) + &
                              BOMOTHCON(ixRF,5,IY)) * ErlIN(iy)*.001*CarbCnvFac
          IndCrb(13,2,2) = (WOODPRODCON(ixDS,5,IY) + &
                             PLASTICCON(ixDS,5,IY) + &
                              BOMOTHCON(ixDS,5,IY)) * EdsIN(iy)*.001*CarbCnvFac
          IndCrb(13,3,2) = (WOODPRODCON(ixLG,5,IY) + &
                             PLASTICCON(ixLG,5,IY) + &
                              BOMOTHCON(ixLG,5,IY)) * ElgIN(iy)*.001*CarbCnvFac
          IndCrb(13,4,2) = (WOODPRODCON(ixNG,5,IY) + &
                             PLASTICCON(ixNG,5,IY) + &
                              BOMOTHCON(ixNG,5,IY)) * EngIN(iy)*.001*CarbCnvFac
          IndCrb(13,5,2) = (WOODPRODCON(ixCL,5,IY) + &
                             PLASTICCON(ixCL,5,IY) + &
                              BOMOTHCON(ixCL,5,IY))* EclIN(iy)*.001*CarbCnvFac
          IndCrb(13,6,2) = (WOODPRODCON(ixRN,5,IY) + &
                             PLASTICCON(ixRN,5,IY) + &
                              BOMOTHCON(ixRN,5,IY)) * 0.
          IndCrb(13,7,2) = (WOODPRODCON(ixEL,5,IY) + &
                             PLASTICCON(ixEL,5,IY) + &
                              BOMOTHCON(ixEL,5,IY))  * ElecCarb_CY*.001
          IndDriver(13,2) = MC_REVIND(11, 6,IY) + MC_REVIND(11, 7,IY) + &
                                    MC_REVIND(11, 8,IY) + MC_REVIND(11, 9,IY) + &
              MC_REVIND(11,14,IY) + MC_REVIND(11,20,IY) + MC_REVIND(11,26,IY) + &
              MC_REVIND(11,27,IY) +                       MC_REVIND(11,32,IY) + &
              MC_REVIND(11,35,IY) + MC_REVIND(11,41,IY)

!     PRIOR YEAR
!     Table 35. Refining
          IndCrb(1,1,1) = REFCON(ixRF,5,IY-1)* ErlIN(iy-1)*.001*CarbCnvFac
          IndCrb(1,2,1) = REFCON(ixDS,5,IY-1)* EdsIN(iy-1)*.001*CarbCnvFac
          IndCrb(1,3,1) = REFCON(ixLG,5,IY-1)* ElgIN(iy-1)*.001*CarbCnvFac
          IndCrb(1,4,1) = REFCON(ixPC,5,IY-1)* EpcIN(iy-1)*.001*CarbCnvFac
          IndCrb(1,5,1) = REFCON(ixSG,5,IY-1)* EsgIN(iy-1)*.001*CarbCnvFac
          IndCrb(1,6,1) = REFCON(ixOP,5,IY-1)* EotIN(iy-1)*.001*CarbCnvFac
          IndCrb(1,7,1) = REFCON(ixNG,5,IY-1)* EngIN(iy-1)*.001*CarbCnvFac
          IndCrb(1,8,1) = REFCON(ixCL,5,IY-1)* EclIN(iy-1)*.001*CarbCnvFac
          IndCrb(1,9,1) = REFCON(ixEL,5,IY-1)* ElecCarb_PY *.001
          IndDriver(1,1)= MC_REVIND(11,25,iy-1)

!     Table 36. Food Industry
          IndCrb(2,1,1) = FOODCON(ixRF,5,IY-1)* ErlIN(iy-1)*.001*CarbCnvFac
          IndCrb(2,2,1) = FOODCON(ixDS,5,IY-1)* EdsIN(iy-1)*.001*CarbCnvFac
          IndCrb(2,3,1) = FOODCON(ixLG,5,IY-1)* ElgIN(iy-1)*.001*CarbCnvFac
          IndCrb(2,4,1) = FOODCON(ixOP,5,IY-1)* EotIN(iy-1)*.001*CarbCnvFac
          IndCrb(2,5,1) = FOODCON(ixNG,5,IY-1)* EngIN(iy-1)*.001*CarbCnvFac
          IndCrb(2,6,1) = FOODCON(ixCL,5,IY-1)* EclIN(iy-1)*.001*CarbCnvFac
          IndCrb(2,7,1) = FOODCON(ixRN,5,IY-1)*  0.
          IndCrb(2,8,1) = FOODCON(ixEL,5,IY-1)* ElecCarb_PY *.001
          IndDriver(2,1)= MC_REVIND(11,1,iy-1)

!     Table 37. Paper Industry
          IndCrb(3,1,1) = PAPERCON(ixRF,5,IY-1) * ErlIN(iy-1)*.001*CarbCnvFac
          IndCrb(3,2,1) = PAPERCON(ixDS,5,IY-1) * EdsIN(iy-1)*.001*CarbCnvFac
          IndCrb(3,3,1) = PAPERCON(ixLG,5,IY-1) * ElgIN(iy-1)*.001*CarbCnvFac
          IndCrb(3,4,1) = PAPERCON(ixNG,5,IY-1) * EngIN(iy-1)*.001*CarbCnvFac
          IndCrb(3,5,1) = PAPERCON(ixCL,5,IY-1) * EclIN(iy-1)*.001*CarbCnvFac
          IndCrb(3,6,1) = PAPERCON(ixRN,5,IY-1) * 0.
          IndCrb(3,7,1) = PAPERCON(ixEL,5,IY-1) * ElecCarb_PY*.001
          IndDriver(3,1)= MC_REVIND(11,10,iy-1)

!     Table 38. Bulk Chemical Industry
!       --- HEAT AND POWER
           IndCrb(4,1,1) = CHEMCON(ixRF,5,IY-1) * ErlIN(iy-1)*.001*CarbCnvFac
           IndCrb(4,2,1) = CHEMCON(ixDS,5,IY-1) * EdsIN(iy-1)*.001*CarbCnvFac
           IndCrb(4,3,1) = CHEMCON(ixLG,5,IY-1) * ElgIN(iy-1)*.001*CarbCnvFac
           IndCrb(4,4,1) = CHEMCON(ixOP,5,IY-1) * EotIN(iy-1)*.001*CarbCnvFac
           IndCrb(4,5,1) = CHEMCON(ixNG,5,IY-1) * EngIN(iy-1)*.001*CarbCnvFac
           IndCrb(4,6,1) = CHEMCON(ixCL,5,IY-1) * EclIN(iy-1)*.001*CarbCnvFac
           IndCrb(4,7,1) = CHEMCON(ixEL,5,IY-1) * ElecCarb_PY*.001
!         --- FEEDSTOCK
           IndCrb(4,8,1) = CHEMCON(ixLF,5,IY-1) * Enqlgpf(iy-1)*.001*CarbCnvFac !lpg feedstocks
           IndCrb(4,9,1) = CHEMCON(ixPF,5,IY-1) * EpfIN(iy-1)*.001*CarbCnvFac   !pet feedstocks
           IndCrb(4,10,1)= CHEMCON(ixNF,5,IY-1) * Enqngpf(iy-1)*.001*CarbCnvFac !nat gas feedstocks
           IndDriver(4,1)=T38(1,iy-1,IS)

!     Table 39. Glass Industry
           IndCrb(5,1,1) = GLASSCON(ixRF,5,IY-1) * ErlIN(iy-1)*.001*CarbCnvFac
           IndCrb(5,2,1) = GLASSCON(ixDS,5,IY-1) * EdsIN(iy-1)*.001*CarbCnvFac
           IndCrb(5,3,1) = GLASSCON(ixLG,5,IY-1) * ElgIN(iy-1)*.001*CarbCnvFac
           IndCrb(5,4,1) = GLASSCON(ixNG,5,IY-1) * EngIN(iy-1)*.001*CarbCnvFac
           IndCrb(5,5,1) = GLASSCON(ixCL,5,IY-1) * EclIN(iy-1)*.001*CarbCnvFac
           IndCrb(5,6,1) = GLASSCON(ixEL,5,IY-1) * ElecCarb_PY*.001
           IndDriver(5,1)=MC_REVIND(11,28,iy-1)

!     Table 40. Cement Industry
           IndCrb(6,1,1) = CEMENTCON(ixRF,5,IY-1) * ErlIN(iy-1)*.001*CarbCnvFac
           IndCrb(6,2,1) = CEMENTCON(ixDS,5,IY-1) * EdsIN(iy-1)*.001*CarbCnvFac
           IndCrb(6,3,1) = CEMENTCON(ixOP,5,IY-1) * EotIN(iy-1)*.001*CarbCnvFac
           IndCrb(6,4,1) = CEMENTCON(ixNG,5,IY-1) * EngIN(iy-1)*.001*CarbCnvFac
           IndCrb(6,5,1) = CEMENTCON(ixCL,5,IY-1) * EclIN(iy-1)*.001*CarbCnvFac
           IndCrb(6,6,1) = CEMENTCON(ixEL,5,IY-1) * ElecCarb_PY*.001
           IndDriver(6,1)=MC_REVIND(11,30,iy-1)

!     Table 41. Iron and Steel Industry
           IndCrb(7,1,1) = STEELCON(ixDS,5,IY-1) * EdsIN(iy-1)*.001*CarbCnvFac
           IndCrb(7,2,1) = STEELCON(ixRF,5,IY-1) * ErlIN(iy-1)*.001*CarbCnvFac
           IndCrb(7,3,1) = STEELCON(ixOP,5,IY-1) * EotIN(iy-1)*.001*CarbCnvFac
           IndCrb(7,4,1) = STEELCON(ixNG,5,IY-1) * EngIN(iy-1)*.001*CarbCnvFac
           IndCrb(7,5,1) = STEELCON(ixMC,5,IY-1) * EmcIN(iy-1)*.001*CarbCnvFac
           IndCrb(7,6,1) = STEELCON(ixCI,5,IY-1) * EpcIN(iy-1)*.001*CarbCnvFac
           IndCrb(7,7,1) = STEELCON(ixCL,5,IY-1) * EclIN(iy-1)*.001*CarbCnvFac
           IndCrb(7,8,1) = STEELCON(ixEL,5,IY-1) * ElecCarb_PY*.001
           IndDriver(7,1)=MC_REVIND(11,33,iy-1)

!     Table 42. Aluminum Industry
           IndCrb(8,1,1) = ALUMCON(ixDS,5,IY-1) * EdsIN(iy-1)*.001*CarbCnvFac
           IndCrb(8,2,1) = ALUMCON(ixLG,5,IY-1) * ElgIN(iy-1)*.001*CarbCnvFac
           IndCrb(8,3,1) = ALUMCON(ixPC,5,IY-1) * EpcIN(iy-1)*.001*CarbCnvFac
           IndCrb(8,4,1) = ALUMCON(ixOP,5,IY-1) * EotIN(iy-1)*.001*CarbCnvFac
           IndCrb(8,5,1) = ALUMCON(ixNG,5,IY-1) * EngIN(iy-1)*.001*CarbCnvFac
           IndCrb(8,6,1) = ALUMCON(ixCL,5,IY-1) * EclIN(iy-1)*.001*CarbCnvFac
           IndCrb(8,7,1) = ALUMCON(ixEL,5,IY-1) * ElecCarb_PY*.001
           IndDriver(8,1)=MC_REVIND(11,34,iy-1)

!     Table 43. Other Industrial Sectors
!      Agriculture
          IndCrb(9,1,1) = AGCON(ixDS,5,IY-1) * EdsIN(iy-1)*.001*CarbCnvFac
          IndCrb(9,2,1) = AGCON(ixLG,5,IY-1) * ElgIN(iy-1)*.001*CarbCnvFac
          IndCrb(9,3,1) = AGCON(ixMG,5,IY-1) * EmgIN(iy-1)*.001*CarbCnvFac
          IndCrb(9,4,1) = AGCON(ixOP,5,IY-1) * EotIN(iy-1)*.001*CarbCnvFac
          IndCrb(9,5,1) = AGCON(ixNG,5,IY-1) * EngIN(iy-1)*.001*CarbCnvFac
          IndCrb(9,6,1) = AGCON(ixCL,5,IY-1) * EclIN(iy-1)*.001*CarbCnvFac
          IndCrb(9,7,1) = AGCON(ixRN,5,IY-1) * 0.
          IndCrb(9,8,1) = AGCON(ixEL,5,IY-1) * ElecCarb_PY*.001
          IndDriver(9,1) = MC_REVIND(11,42,iy-1)+MC_REVIND(11,43,iy-1)+MC_REVIND(11,44,iy-1)

!      Construction
          IndCrb(10,1,1) = CONSTCON(ixDS,5,IY-1) * EdsIN(iy-1)*.001*CarbCnvFac
          IndCrb(10,2,1) = CONSTCON(ixMG,5,IY-1) * EmgIN(iy-1)*.001*CarbCnvFac
          IndCrb(10,3,1) = CONSTCON(ixAS,5,IY-1) * 0.
          IndCrb(10,4,1) = CONSTCON(ixNG,5,IY-1) * EngIN(iy-1)*.001*CarbCnvFac
          IndCrb(10,5,1) = CONSTCON(ixCL,5,IY-1) * EclIN(iy-1)*.001*CarbCnvFac
          IndCrb(10,6,1) = CONSTCON(ixEL,5,IY-1) * ElecCarb_PY*.001
          IndDriver(10,1)=MC_REVIND(11,48,iy-1)

!       Mining
          IndCrb(11,1,1) = MINECON(ixRF,5,IY-1) * ErlIN(iy-1)*.001*CarbCnvFac
          IndCrb(11,2,1) = MINECON(ixDS,5,IY-1) * EdsIN(iy-1)*.001*CarbCnvFac
          IndCrb(11,3,1) = MINECON(ixMG,5,IY-1) * EmgIN(iy-1)*.001*CarbCnvFac
          IndCrb(11,4,1) = MINECON(ixOP,5,IY-1) * EotIN(iy-1)*.001*CarbCnvFac
          IndCrb(11,5,1) = MINECON(ixNG,5,IY-1) * EngIN(iy-1)*.001*CarbCnvFac
          IndCrb(11,6,1) = QLPIN(11,IY-1) * ElgIN(iy-1)*CarbCnvFac +QNGLQ(11,IY-1)*ElgIN(iy-1)*CarbCnvFac
          IndCrb(11,7,1) = MINECON(ixCL,5,IY-1) * EclIN(iy-1)*.001*CarbCnvFac
          IndCrb(11,8,1) = MINECON(ixRN,5,IY-1) * 0. !renewables
          IndCrb(11,9,1) = MINECON(ixEL,5,IY-1) * ElecCarb_PY*.001
          IndDriver(11,1) = MC_REVIND(11,45,iy-1)+MC_REVIND(11,46,iy-1)+MC_REVIND(11,47,iy-1)

!        --- METALS BASED DURABLES
          IndCrb(12,1,1) = (FABMETALCON(ixRF,5,IY-1) + &
                          MACHINECON(ixRF,5,IY-1) + &
                          COMPUTECON(ixRF,5,IY-1) + &
                        TRANEQUIPCON(ixRF,5,IY-1) + &
                         ELECEQUIPON(ixRF,5,IY-1))  * ErlIN(iy-1)*.001*CarbCnvFac
          IndCrb(12,2,1) = (FABMETALCON(ixDS,5,IY-1) + &
                          MACHINECON(ixDS,5,IY-1) + &
                          COMPUTECON(ixDS,5,IY-1) + &
                        TRANEQUIPCON(ixDS,5,IY-1) + &
                         ELECEQUIPON(ixDS,5,IY-1))  * EdsIN(iy-1)*.001*CarbCnvFac
          IndCrb(12,3,1) = (FABMETALCON(ixLG,5,IY-1) + &
                          MACHINECON(ixLG,5,IY-1) + &
                          COMPUTECON(ixLG,5,IY-1) + &
                        TRANEQUIPCON(ixLG,5,IY-1) + &
                         ELECEQUIPON(ixLG,5,IY-1) ) * ElgIN(iy-1)*.001*CarbCnvFac
          IndCrb(12,4,1) = (FABMETALCON(ixNG,5,IY-1) + &
                          MACHINECON(ixNG,5,IY-1) + &
                          COMPUTECON(ixNG,5,IY-1) + &
                        TRANEQUIPCON(ixNG,5,IY-1) + &
                         ELECEQUIPON(ixNG,5,IY-1) ) * EngIN(iy-1)*.001*CarbCnvFac
          IndCrb(12,5,1) = (FABMETALCON(ixCL,5,IY-1) + &
                          MACHINECON(ixCL,5,IY-1) + &
                          COMPUTECON(ixCL,5,IY-1) + &
                        TRANEQUIPCON(ixCL,5,IY-1) + &
                         ELECEQUIPON(ixCL,5,IY-1)) * EclIN(iy-1)*.001*CarbCnvFac
          IndCrb(12,6,1) = (FABMETALCON(ixRN,5,IY-1) + &
                          MACHINECON(ixRN,5,IY-1) + &
                          COMPUTECON(ixRN,5,IY-1) + &
                        TRANEQUIPCON(ixRN,5,IY-1) + &
                         ELECEQUIPON(ixRN,5,IY-1))  * 0.
          IndCrb(12,7,1) = (FABMETALCON(ixEL,5,IY-1) + &
                          MACHINECON(ixEL,5,IY-1) + &
                          COMPUTECON(ixEL,5,IY-1) + &
                        TRANEQUIPCON(ixEL,5,IY-1) + &
                         ELECEQUIPON(ixEL,5,IY-1)) * ElecCarb_PY*.001
          IndDriver(12,1) = MC_REVIND(11,36,iy-1)+MC_REVIND(11,37,iy-1)+ &
                   MC_REVIND(11,38,iy-1)+MC_REVIND(11,39,iy-1)+MC_REVIND(11,40,iy-1)

!        --- OTHER MANUFACTURING
          IndCrb(13,1,1) = (WOODPRODCON(ixRF,5,IY-1) + &
                             PLASTICCON(ixRF,5,IY-1) + &
                              BOMOTHCON(ixRF,5,IY-1)) * ErlIN(iy-1)*.001*CarbCnvFac
          IndCrb(13,2,1) = (WOODPRODCON(ixDS,5,IY-1) + &
                             PLASTICCON(ixDS,5,IY-1) + &
                              BOMOTHCON(ixDS,5,IY-1)) * EdsIN(iy-1)*.001*CarbCnvFac
          IndCrb(13,3,1) = (WOODPRODCON(ixLG,5,IY-1) + &
                             PLASTICCON(ixLG,5,IY-1) + &
                              BOMOTHCON(ixLG,5,IY-1)) * ElgIN(iy-1)*.001*CarbCnvFac
          IndCrb(13,4,1) = (WOODPRODCON(ixNG,5,IY-1) + &
                             PLASTICCON(ixNG,5,IY-1) + &
                              BOMOTHCON(ixNG,5,IY-1)) * EngIN(iy-1)*.001*CarbCnvFac
          IndCrb(13,5,1) = (WOODPRODCON(ixCL,5,IY-1) + &
                             PLASTICCON(ixCL,5,IY-1) + &
                              BOMOTHCON(ixCL,5,IY-1))* EclIN(iy-1)*.001*CarbCnvFac
          IndCrb(13,6,1) = (WOODPRODCON(ixRN,5,IY-1) + &
                             PLASTICCON(ixRN,5,IY-1) + &
                              BOMOTHCON(ixRN,5,IY-1)) * 0.
          IndCrb(13,7,1) = (WOODPRODCON(ixEL,5,IY-1) + &
                             PLASTICCON(ixEL,5,IY-1) + &
                              BOMOTHCON(ixEL,5,IY-1))  * ElecCarb_PY*.001
          IndDriver(13,1) = MC_REVIND(11, 6,iy-1) + MC_REVIND(11, 7,iy-1) + &
                                      MC_REVIND(11, 8,iy-1) + MC_REVIND(11, 9,iy-1) + &
              MC_REVIND(11,14,iy-1) + MC_REVIND(11,20,iy-1) + MC_REVIND(11,26,iy-1) + &
              MC_REVIND(11,27,iy-1) +                         MC_REVIND(11,32,iy-1) + &
              MC_REVIND(11,35,iy-1) + MC_REVIND(11,41,iy-1)

! Diagnostic rollup of industrial carbon

        Do ix=1,13 !sub-industry / sector
          Do jx=1,10!fuel

                   if(iprintflag==99) then
                     write (iprintfile,97) "IndyCarb",IY,ix,jx,IB,  &
                                               IndCrb(ix,jx,2),   t17(itotal_nbr_carb_ind,11,iy,is),    &
                                               IndCrb(ix,jx,1),   t17(itotal_nbr_carb_ind,11,iy-1,is),    &
                                               IndCrb(ix,jx,2),   IndDriver(ix,2),   &
                                               IndCrb(ix,jx,1),   IndDriver(ix,1)
                    endif


              If (IndCrb(ix,jx,1) <= 0.0) cycle                                   !skip zeros
              If (IndCrb(ix,jx,2) <= 0.0) cycle                                   !skip zeros
              If (IndDriver(ix,1) <= 0.0) cycle                                   !skip zeros
              If (IndDriver(ix,2) <= 0.0) cycle                                   !skip zeros

              Ind_carbon = Ind_carbon + IndCrb(ix,jx,2)

        if(isnan((IndCrb(ix,jx,2)/IndDriver(ix,2))/(IndCrb(ix,jx,1)/IndDriver(ix,1)) ))  then
                T31(13,IY,IS)=-10.
                T31(11,IY,IS)=-10.
         else

              T31(13,IY,IS)= T31(13,IY,IS) + &                                    !single stage index
                  0.5*(IndCrb(ix,jx,2)/t17(itotal_nbr_carb,11,iy,is)+ &
                        IndCrb(ix,jx,1)/t17(itotal_nbr_carb,11,iy-1,is)) &
                      * log( (IndCrb(ix,jx,2)/IndDriver(ix,2))/(IndCrb(ix,jx,1)/IndDriver(ix,1)) )

              T31(11,IY,IS)= T31(11,IY,IS) + &                                      !industrial index
                  0.5*(IndCrb(ix,jx,2)/t17(itotal_nbr_carb_ind,11,iy,is)+ &
                        IndCrb(ix,jx,1)/t17(itotal_nbr_carb_ind,11,iy-1,is)) &
                      * log((IndCrb(ix,jx,2)/IndDriver(ix,2))/(IndCrb(ix,jx,1)/IndDriver(ix,1)) )
         endif


          Enddo !fuel
        Enddo !sub-industry / sector

!             Print Ind-Carbon

              if(iprintflag==1 .OR. iprintflag==99) Write (iprintfile,*) "Industrial Carbon ",Ind_Carbon, " IY ", IY+1989, " FTAB ", t17(itotal_nbr_carb_ind,11,iy,is)

! End Industrial Carbon Calculations

        if(iprintflag .eq. 1 .and. iy .gt. 50) iprintflag=0
        if(iprintflag .eq. 99 .and. iy .gt. 18) iprintflag=0  ! turn off detailed printing after several times through loop

! End Sectoral Calculations
!  Expand Indices by Exponentiating
        ! Energy Indices
        T31(2,iy,is) = T31(2,iy-1,is)*exp(T31(2,iy,is))
        T31(3,iy,is) = T31(3,iy-1,is)*exp(T31(3,iy,is))
        T31(4,iy,is) = T31(4,iy-1,is)*exp(T31(4,iy,is))
        T31(5,iy,is) = T31(5,iy-1,is)*exp(T31(5,iy,is))
        T31(6,iy,is) = T31(6,iy-1,is)*exp(T31(6,iy,is))
        T31(7,iy,is) = T31(7,iy-1,is)*exp(T31(7,iy,is))
        ! Carbon (no utility sector for carbon, modeled in end uses)
        T31( 9,iy,is) = T31( 9,iy-1,is)*exp(T31( 9,iy,is))
        T31(10,iy,is) = T31(10,iy-1,is)*exp(T31(10,iy,is))
        T31(11,iy,is) = T31(11,iy-1,is)*exp(T31(11,iy,is))
        T31(12,iy,is) = T31(12,iy-1,is)*exp(T31(12,iy,is))
        T31(13,iy,is) = T31(13,iy-1,is)*exp(T31(13,iy,is))
!        T31(16,iy,is) = T31(16,iy-1,is)*exp(T31(13,iy,is))  !unused in report placeholder for elec generators

        If(iprintflag2==1) then
           Write (iprintfile,99) "Tornquist", T31(7,iy,is)
           Write (iprintfile,99) "E/RGDP",T31(1,iy,is)
           Write (iprintfile,99) "Residential",Res_energy,"Ftab Energy",t2(itotal_nbr_res,11,iy,is)
           Write (iprintfile,99) "Commercial",Com_energy,"Ftab Energy",t2(itotal_nbr_com,11,iy,is)
           Write (iprintfile,99) "Transportation",Trn_energy,"Ftab Energy",t2(itotal_nbr_tran,11,iy,is)
           Write (iprintfile,99) "Electric Utility",Utl_energy,"Ftab Energy",t2(itotal_nbr_util,11,iy,is)
           Write (iprintfile,99) "Industrial",Ind_energy,"Ftab Energy",t2(itotal_nbr_ind,11,iy,is)
           Check_total=Res_energy+Com_energy+Ind_energy+Trn_energy+Utl_energy
           Write (iprintfile,99) "Total",Check_total,"Ftab Energy",t2(itotal_nbr,11,iy,is)
        endif

      endif !check for year to compute index

 96     Format(1x,A15,3I5,2e15.8)
 97     Format(1x,A8,4I5,8e15.8)
 98     Format(A20,31(1x,e12.5))
 99     Format(2(A20,1x,e12.5))

         Enddo  ! End Tornquist index calculations -- End Table 31 processing

      RETURN
      END SUBROUTINE FDATA31

      END SUBROUTINE FDATA1

      SUBROUTINE FDATA1A(IS)
      IMPLICIT NONE
      include 'parametr'
      include 'emmparm'
      include 'qblk'
      include 'ampblk'          !  use adjusted prices
      include 'macout'
      include 'lfmmout'
      include 'pmmout'
      include 'pmmrpt'
      include 'pmmftab'
      include 'ogsmout'
      include 'convfact'
      include 'ngtdmrep'
      include 'angtdm'
      include 'ncntrl'
      include 'emission'
      include 'uefpout'
      include 'uefdout'
      include 'udatout'
      include 'uecpout'
      include 'uettout'
      include 'efpout'
      include 'comparm'
      include 'comout'
      include 'commrep'
      include 'cogen'
      include 'cdsparms'
      include 'coalout'
      include 'coalemm'
      include 'coalrep'
      include 'pqchar' ! for parameter mnoth used in include file converge
      include 'converge'
      include 'indrep'
      include 'indout'
      include 'intout'
      include 'resdrep'
      include 'tranrep'
      include 'wrenew'
      include 'ftable'
      include 'rseff'
      include 'emeblk'
      include 'ghgrep'
      include 'aponroad'
      include 'qonroad'
      include 'aeusprc'

      INTEGER*4 III,II,PADD       !COUNTERS
      REAL*4 FSUM,FFF,RRR,CUMRET,so2coal,CommCoal,CommGas,CommOil,CommRenew,CommOther,RDAYS
      REAL CTLSO2,CTLNOX,CTLHG
      EXTERNAL FSUM
      INTEGER IR,IS,IY,COUNT,NUMREP,LOOP1,ISO2,ICY,MAPERG(MNUMNR),IR2,IR3,IX
      REAL C_CO2_FACTOR,C8(9)

!     Table 45. Summary Report, Transportation Sector Energy Use by Mode & Type

         DO 450, IY = 1,LASTYR
!           --- HIGHWAY
!           --- LIGHT-DUTY VEHICLES
            T45( 1,IY,IS) = TRQHWY(1,IY) + TRQHWY(2,IY) + TRQHWY(3,IY)
            T45( 2,IY,IS) = TRQHWY(1,IY)
            T45( 3,IY,IS) = TRQHWY(2,IY)
            T45( 4,IY,IS) = TRQHWY(3,IY)
            T45( 5,IY,IS) = BCLTBTUT(10,IY)
!           --- BUSES
            T45( 6,IY,IS) = sum(TRQBUS(:,:,IY))
            T45( 7,IY,IS) = sum(TRQBUS(1,:,IY))
            T45( 8,IY,IS) = sum(TRQBUS(2,:,IY))
            T45( 9,IY,IS) = sum(TRQBUS(3,:,IY))
!           --- FREIGHT TRUCKS
            T45(10,IY,IS) = TRQHWY(5,IY) + TRQHWY(6,IY)
            T45(51,IY,IS) = TRQHWY_NEW(5,IY)
            T45(11,IY,IS) = TRQHWY(5,IY)
            T45(52,IY,IS) = TRQHWY_NEW(6,IY)
            T45(12,IY,IS) = TRQHWY(6,IY)
!           --- NON-HIGHWAY
!           --- AIR
            T45(13,IY,IS) = TRQNHWY(1,IY) + TRQNHWY(2,IY) + TRQNHWY(3,IY) + TRQNHWY(4,IY)
            T45(14,IY,IS) = TRQNHWY(1,IY)
            T45(15,IY,IS) = TRQNHWY(2,IY)
            T45(16,IY,IS) = TRQNHWY(3,IY)
            T45(17,IY,IS) = TRQNHWY(4,IY)
!           --- WATER
            T45(18,IY,IS) = sum(TRQDOMS(1:4,IY)) + sum(TRQINTS(1:4,IY)) + &
                            TRQBOAT(1,IY) + TRQBOAT(2,IY)
            T45(19,IY,IS) = sum(TRQDOMS(1:4,IY)) + sum(TRQINTS(1:4,IY))
            T45(20,IY,IS) = TRQDOMS(1,IY) + TRQDOMS(2,IY) + TRQDOMS(3,IY) + TRQDOMS(4,IY)
            T45(21,IY,IS) = TRQINTS(1,IY) + TRQINTS(2,IY) + TRQINTS(3,IY) + TRQINTS(4,IY)
            T45(22,IY,IS) = TRQBOAT(1,IY) + TRQBOAT(2,IY)
!           --- RAIL
            T45(23,IY,IS) = sum(TRQRRF(1:4,IY)) + sum(TRQRRP(1:9,IY))
            T45(24,IY,IS) = TRQRRF(1,IY) + TRQRRF(2,IY) + TRQRRF(3,IY) + TRQRRF(4,IY)
            T45(25,IY,IS) = sum(TRQRRP(1:9,IY))
            T45(26,IY,IS) = sum(TRQRRP(1:4,IY))
            T45(27,IY,IS) = TRQRRP(5,IY)
            T45(28,IY,IS) = sum(TRQRRP(6:9,IY))
!           --- LUBRICANTS
            T45(29,IY,IS) = TRQLUB(IY)
!           --- PIPELINE FUEL NATURAL GAS
            T45(30,IY,IS) = QGPTR(11,IY) * 1000.
            T45(53,IY,IS) = QNGLQ(11,IY) * 1000.
!           --- MILITARY
            T45(31,IY,IS) = TRQMIL(1,IY) + TRQMIL(2,IY) + TRQMIL(3,IY) + TRQMIL(4,IY)
            T45(32,IY,IS) = TRQMIL(1,IY) + TRQMIL(2,IY)
            T45(33,IY,IS) = TRQMIL(3,IY)
            T45(34,IY,IS) = TRQMIL(4,IY)
!           --- TOTAL --- ASSIGNED BELOW AFTER T45(50,IY,IS)
           T45(35,IY,IS) = T45( 1,IY,IS) + T45( 6,IY,IS) + T45(10,IY,IS) + &
                           T45(13,IY,IS) + T45(18,IY,IS) + T45(23,IY,IS) + &
                           T45(29,IY,IS) + T45(30,IY,IS) + T45(31,IY,IS) + T45( 5,IY,IS)
!           --- ENERGY USE BY TYPE
            T45(36,IY,IS) = TRQLDV(1,11,IY) + CLTFUELBTU(1,IY) + TRQFTRK(1,IY) + &
                            TRQDOMS(3,IY) + sum(TRQBUS(:,1,IY)) + TRQBOAT(1,IY)
            T45(45,IY,IS) = TRQLDV(3,11,IY) + CLTFUELBTU(5,IY) + sum(TRQBUS(:,3,IY))
            T45(37,IY,IS) = TRQLDV(8,11,IY) + TRQFTRK(2,IY) + TRQRRF(1,IY) + &
                            TRQDOMS(1,IY) + TRQINTS(1,IY) + TRQMIL(4,IY) + &
                            sum(TRQBUS(:,2,IY)) + CLTFUELBTU(2,IY) + &
                            TRQRRP(2,IY) + TRQRRP(7,IY) + TRQBOAT(2,IY)
            T45(38,IY,IS) = TRQAIRT(1,IY) + TRQMIL(1,IY) + TRQMIL(2,IY)
            T45(39,IY,IS) = TRQDOMS(2,IY) + TRQINTS(2,IY) + TRQMIL(3,IY)
            T45(40,IY,IS) = TRQAIRT(2,IY)
            T45(41,IY,IS) = TRQLDV(5,11,IY) + TRQFTRK(5,IY) + sum(TRQBUS(:,6,IY)) + CLTFUELBTU(3,IY)
            T45(42,IY,IS) = TRQLUB(IY)
            T45(43,IY,IS) = FSUM(T45(36,IY,IS),7) + T45(45,IY,IS)
            T45(44,IY,IS) = TRQLDV(2,11,IY)
            T45(46,IY,IS) = TRQLDV(6,11,IY) + TRQRRP(1,IY) + TRQRRP(5,IY) + TRQRRP(6,IY) + &
                                    CLTFUELBTU(6,IY) + sum(TRQBUS(:,7,IY))
            T45(47,IY,IS) = TRQLDV(4,11,IY) + TRQFTRK(3,IY) + sum(TRQBUS(:,5,IY)) + CLTFUELBTU(4,IY) + &
                            TRQRRF(3,IY) + TRQRRF(4,IY) + &
                            TRQDOMS(3,IY) + TRQDOMS(4,IY) + &
                            TRQINTS(3,IY) + TRQINTS(4,IY) + &
                            TRQRRP(3,IY) + TRQRRP(4,IY) + TRQRRP(8,IY) + TRQRRP(9,IY)
            T45(48,IY,IS) = TRQLDV(7,11,IY) + CLTFUELBTU(7,IY) + sum(TRQBUS(:,8,IY))
            T45(49,IY,IS) = QGPTR(11,IY) * 1000.
            T45(54,IY,IS) = QNGLQ(11,IY) * 1000.
!           --- TOTAL CONSUMPTION
            T45(50,IY,IS) = FSUM(T45(43,IY,IS),7) - T45(45,IY,IS) + T45(54,IY,IS) ! T45(45 now in T45(43
  450    CONTINUE

!     Table 46. Summary Transportation Sector Energy Use by Fuel Type

         DO 460, IY = 1,LASTYR
           ICY=IY+BASEYR-1
!          --- LIGHT-DUTY VEHICLES
           T46( 1,IY,IS) = TRQLDV(1,11,IY)
           T46( 2,IY,IS) = TRQLDV(2,11,IY)    ! methanol-not printed
           T46( 3,IY,IS) = TRQLDV(3,11,IY)
           T46( 4,IY,IS) = TRQLDV(4,11,IY)
           T46( 5,IY,IS) = TRQLDV(5,11,IY)
           T46( 6,IY,IS) = TRQLDV(6,11,IY)
           T46( 7,IY,IS) = TRQLDV(7,11,IY)
           T46( 8,IY,IS) = TRQLDV(8,11,IY)
           T46( 9,IY,IS) = FSUM(T46(1,IY,IS),8)
!          --- 2- AND 3- WHEELED VEHICLES
           T46(86,IY,IS) = TTHCONS(1,IY)
           T46(87,IY,IS) = 0.0                ! methanol.  yuck.
           T46(88,IY,IS) = 0.0
           T46(89,IY,IS) = TTHCONS(5,IY)
           T46(90,IY,IS) = TTHCONS(3,IY)
           T46(91,IY,IS) = TTHCONS(4,IY)
           T46(92,IY,IS) = TTHCONS(6,IY)
           T46(93,IY,IS) = TTHCONS(2,IY)
           T46(94,IY,IS) = FSUM(T46(86,IY,IS),8)
!          --- LIGHT-DUTY VEHICLES excluding 2- and 3- wheeled vehicles
           T46( 95,IY,IS) = T46( 1,IY,IS) - T46(86,IY,IS)
           T46( 96,IY,IS) = T46( 2,IY,IS) - T46(87,IY,IS)    ! methanol-not printed
           T46( 97,IY,IS) = T46( 3,IY,IS) - T46(88,IY,IS)
           T46( 98,IY,IS) = T46( 4,IY,IS) - T46(89,IY,IS)
           T46( 99,IY,IS) = T46( 5,IY,IS) - T46(90,IY,IS)
           T46(100,IY,IS) = T46( 6,IY,IS) - T46(91,IY,IS)
           T46(101,IY,IS) = T46( 7,IY,IS) - T46(92,IY,IS)
           T46(102,IY,IS) = T46( 8,IY,IS) - T46(93,IY,IS)
           T46(103,IY,IS) = FSUM(T46(95,IY,IS),8)
!          --- COMMERCIAL LIGHT TRUCKS
           T46(58,IY,IS) = CLTFUELBTU(1,IY)
           T46(59,IY,IS) = CLTFUELBTU(2,IY)
           T46(76,IY,IS) = CLTFUELBTU(3,IY)
           T46(77,IY,IS) = CLTFUELBTU(4,IY)
           T46(78,IY,IS) = CLTFUELBTU(5,IY)
           T46(79,IY,IS) = CLTFUELBTU(6,IY)
           T46(80,IY,IS) = CLTFUELBTU(7,IY)
!          T46(81,IY,IS) and T46(82,IY,IS) are reserved for future use for = CLTFUELBTU(8,IY) and = CLTFUELBTU(9,IY)
           T46(10,IY,IS) = CLTFUELBTU(10,IY)
!          --- FREIGHT TRUCKS
           T46(11,IY,IS) = TRQFTRK(1,IY)
           T46(12,IY,IS) = TRQFTRK(2,IY)
           T46(13,IY,IS) = TRQFTRK(3,IY)
           T46(14,IY,IS) = TRQFTRK(5,IY)
           T46(83,IY,IS) = TRQFTRK(6,IY)
           T46(84,IY,IS) = TRQFTRK(7,IY)
           T46(85,IY,IS) = TRQFTRK(8,IY)
!          TRQFTRK(4,IY) and TRQFTRK(9,IY) currently not assigned
           T46(15,IY,IS) = FSUM(T46(11,IY,IS),4) + FSUM(T46(83,IY,IS),3)
!          --- RAILROAD
           T46(16,IY,IS) = TRQRRF(1,IY)
           T46(63,IY,IS) = TRQRRF(2,IY)
           T46(64,IY,IS) = TRQRRF(3,IY)
           T46(65,IY,IS) = TRQRRF(4,IY)
           T46(17,IY,IS) = T46(16,IY,IS) + FSUM(T46(63,IY,IS),3)
!          --- DOMESTIC SHIPPING
           T46(18,IY,IS) = TRQDOMS(1,IY)
           T46(19,IY,IS) = TRQDOMS(2,IY)
           T46(20,IY,IS) = TRQDOMS(3,IY)
           T46(66,IY,IS) = TRQDOMS(4,IY)
           T46(21,IY,IS) = FSUM(T46(18,IY,IS),3) + T46(66,IY,IS)
!          --- INTERNATIONAL SHIPPING
           T46(22,IY,IS) = TRQINTS(1,IY)
           T46(23,IY,IS) = TRQINTS(2,IY)
           T46(67,IY,IS) = TRQINTS(3,IY)
           T46(68,IY,IS) = TRQINTS(4,IY)
           T46(24,IY,IS) = FSUM(T46(22,IY,IS),2) + FSUM(T46(67,IY,IS),2)
!          --- AIR TRANSPORTATION
           T46(25,IY,IS) = TRQAIRT(1,IY)
           T46(26,IY,IS) = TRQAIRT(2,IY)
           T46(27,IY,IS) = T46(25,IY,IS) + T46(26,IY,IS)
!          --- MILITARY USE
           T46(28,IY,IS) = TRQMIL(1,IY) + TRQMIL(2,IY)
           T46(29,IY,IS) = TRQMIL(3,IY)
           T46(30,IY,IS) = TRQMIL(4,IY)
           T46(31,IY,IS) = FSUM(T46(28,IY,IS),3)
!          --- BUS TRANSPORTATION
           T46(32,IY,IS) = TRQBUS(1,1,IY)
           T46(33,IY,IS) = TRQBUS(1,2,IY)
           T46(104,IY,IS)= TRQBUS(1,3,IY)
           T46(105,IY,IS)= TRQBUS(1,4,IY)
           T46(51,IY,IS) = TRQBUS(1,5,IY)
           T46(52,IY,IS) = TRQBUS(1,6,IY)
           T46(106,IY,IS)= TRQBUS(1,7,IY)
           T46(107,IY,IS)= TRQBUS(1,8,IY)
           T46(53,IY,IS) = TRQBUS(2,1,IY)
           T46(34,IY,IS) = TRQBUS(2,2,IY)
           T46(108,IY,IS)= TRQBUS(2,3,IY)
           T46(109,IY,IS)= TRQBUS(2,4,IY)
           T46(54,IY,IS) = TRQBUS(2,5,IY)
           T46(55,IY,IS) = TRQBUS(2,6,IY)
           T46(110,IY,IS)= TRQBUS(2,7,IY)
           T46(111,IY,IS)= TRQBUS(2,8,IY)
           T46(35,IY,IS) = TRQBUS(3,1,IY)
           T46(36,IY,IS) = TRQBUS(3,2,IY)
           T46(112,IY,IS)= TRQBUS(3,3,IY)
           T46(113,IY,IS)= TRQBUS(3,4,IY)
           T46(56,IY,IS) = TRQBUS(3,5,IY)
           T46(57,IY,IS) = TRQBUS(3,6,IY)
           T46(114,IY,IS)= TRQBUS(3,7,IY)
           T46(115,IY,IS)= TRQBUS(3,8,IY)
           T46(60,IY,IS) = sum(TRQBUS(1,1:8,IY))
           T46(61,IY,IS) = sum(TRQBUS(2,1:8,IY))
           T46(62,IY,IS) = sum(TRQBUS(3,1:8,IY))
           T46(37,IY,IS) = FSUM(T46(32,IY,IS),5) + FSUM(T46(51,IY,IS),7)
!          --- RAIL TRANSPORTATION
           T46(38,IY,IS) = TRQRRP(1,IY)
           T46(39,IY,IS) = TRQRRP(2,IY)
           T46(69,IY,IS) = TRQRRP(3,IY)
           T46(70,IY,IS) = TRQRRP(4,IY)
           T46(40,IY,IS) = TRQRRP(5,IY)
           T46(41,IY,IS) = TRQRRP(6,IY)
           T46(42,IY,IS) = TRQRRP(7,IY)
           T46(71,IY,IS) = TRQRRP(8,IY)
           T46(72,IY,IS) = TRQRRP(9,IY)
           T46(43,IY,IS) = FSUM(T46(38,IY,IS),5) + FSUM(T46(69,IY,IS),4)
           T46(73,IY,IS) = FSUM(T46(38,IY,IS),2) + FSUM(T46(69,IY,IS),2)
           T46(74,IY,IS) = T46(40,IY,IS)
           T46(75,IY,IS) = FSUM(T46(41,IY,IS),2) + FSUM(T46(71,IY,IS),2)
!          --- REC. BOATS, LUBRICANTS, PIPELINE FUEL, & TOTAL OF MISC.
           T46(44,IY,IS) = TRQBOAT(1,IY) + TRQBOAT(2,IY)
           T46(49,IY,IS) = TRQBOAT(1,IY)
           T46(50,IY,IS) = TRQBOAT(2,IY)
           T46(45,IY,IS) = TRQLUB(IY)
           T46(46,IY,IS) = QGPTR(11,IY) * 1000.
           T46(116,IY,IS) = QNGLQ(11,IY) * 1000.
           T46(47,IY,IS) = T46(31,IY,IS) + T46(37,IY,IS) + FSUM(T46(43,IY,IS),4)
           T46(48,IY,IS) = T46( 9,IY,IS) + T46(10,IY,IS) + T46(15,IY,IS) + &
                           T46(17,IY,IS) + T46(21,IY,IS) + T46(24,IY,IS) + &
                           T46(27,IY,IS) + T46(47,IY,IS) + T46(116,IY,IS)

        C_CO2_FACTOR = 1.
        IF (TRIM(CARBON_OR_2) .EQ. 'CO2') C_CO2_FACTOR = 44. / 12.
! light-duty vehicles
           c8=0.
           c8(1)=(em_tran(1,11,icy)+em_tran(3,11,icy))/qMGtr(11,iy) ! motor gasoline adjusted for ethanol
           c8(2)=0.
           c8(3)=0.
           if(qETtr(11,iy).gt.0) c8(3)=em_tran(2,11,icy)/qETtr(11,iy) ! e85
           c8(4)=eNGtr(iy)* C_CO2_FACTOR !   cng
           c8(5)=ePRtr(iy)* C_CO2_FACTOR !   propane
           c8(6)=fsum(emel(1,1,iy),4)/qelas(11,iy) !   elec
           c8(7)=0.!  hydrogen
           c8(8)=(em_tran(6,11,icy)+em_tran(7,11,icy))/qDStr(11,iy) !  diesel with biodiesel adjustment
           T22(27,iy,is) = dot_product(TRQLDV(1:8,11,IY),c8(1:8))   !  Light-Duty Vehicles
! commercial light trucks
           c8=0.
           c8(1)=(em_tran(1,11,icy)+em_tran(3,11,icy))/qMGtr(11,iy) ! motor gasoline adjusted for ethanol
           c8(2)=(em_tran(6,11,icy)+em_tran(7,11,icy))/qDStr(11,iy) ! diesel with biodiesel adjustment
           c8(3)=ePRtr(iy)* C_CO2_FACTOR !   propane
           c8(4)=eNGtr(iy)* C_CO2_FACTOR !   cng
           if(qETtr(11,iy).gt.0) c8(5)=em_tran(2,11,icy)/qETtr(11,iy) ! e85
           c8(6)=fsum(emel(1,1,iy),4)/qelas(11,iy) !   elec
           T22(28,iy,is) = dot_product(CLTFUELBTU(1:6,iy),c8(1:6))  !  Commercial Light Trucks skipping hydrogen with 0 carbon factor
! Bus transport
           c8=0.
           c8(1)=(em_tran(1,11,icy)+em_tran(3,11,icy))/qMGtr(11,iy) ! motor gasoline adjusted for ethanol
           c8(2)=(em_tran(6,11,icy)+em_tran(7,11,icy))/qDStr(11,iy) ! diesel with biodiesel adjustment
           c8(3)=eNGtr(iy)* C_CO2_FACTOR
           c8(4)=ePRtr(iy)* C_CO2_FACTOR !   propane
           c8(5)=sum(TRQBUS(:,1,IY))
           c8(6)=sum(TRQBUS(:,2,IY))
           c8(7)=sum(TRQBUS(:,5,IY))
           c8(8)=sum(TRQBUS(:,6,IY))
           T22(29,iy,is) = dot_product(c8(5:8),c8(1:4))             ! Bus Transportation
! freight trucks
           c8=0.
           c8(1)=(em_tran(1,11,icy)+em_tran(3,11,icy))/qMGtr(11,iy) ! motor gasoline adjusted for ethanol
           c8(2)=(em_tran(6,11,icy)+em_tran(7,11,icy))/qDStr(11,iy) ! diesel with biodiesel adjustment
           c8(3)=eNGtr(iy)* C_CO2_FACTOR !   cng
           c8(4)=0.                      !   ???
           c8(5)=ePRtr(iy)* C_CO2_FACTOR !   propane
           T22(30,iy,is) = dot_product(TRQFTRK(1:5,IY),c8(1:5))     !  Freight Trucks
! passenger rail
           c8=0.
           c8(1)=fsum(emel(1,1,iy),4)/qelas(11,iy) !   elec
           c8(2)=(em_tran(6,11,icy)+em_tran(7,11,icy))/qDStr(11,iy) !   diesel with biodiesel adjustment
           c8(3)=eNGtr(iy)* C_CO2_FACTOR !   cng
           c8(4)=eNGtr(iy)* C_CO2_FACTOR !   lng
           c8(5)=c8(1)
           c8(6)=c8(1)
           c8(7)=c8(2)
           c8(8)=c8(3)
           c8(9)=c8(4)
           T22(31,iy,is) = dot_product(TRQRRP(1:9,IY),c8(1:9))      !  Rail, Passenger
!  Note next three categories (rail freight, domestic shipping, international shipping) all have 4
!  fuels in the same order (distillate, residual, CNG, LNG) so we can set c() once and cover all 3
! Rail, freight
           c8=0.
           c8(1)=(em_tran(6,11,icy)+em_tran(7,11,icy))/qDStr(11,iy) !   diesel with biodiesel adjustment
           c8(2)=eRStr(iy)* C_CO2_FACTOR !   resid
           c8(3)=eNGtr(iy)* C_CO2_FACTOR !   cng
           c8(4)=eNGtr(iy)* C_CO2_FACTOR !   lng
           T22(32,iy,is) = dot_product(TRQRRF(1:4,IY),c8(1:4))      !  Rail, freight
! Domestic Shipping
           T22(33,iy,is) = dot_product(TRQDOMS(1:4,IY),c8(1:4))     !  Shipping, Domestic
! International Shipping
           T22(34,iy,is) = dot_product(TRQINTS(1:4,IY),c8(1:4))     !  Shipping, International
! Recreational Boats
           c8=0.
           c8(1)=(em_tran(1,11,icy)+em_tran(3,11,icy))/qMGtr(11,iy) ! motor gasoline adjusted for ethanol
           c8(2)=(em_tran(6,11,icy)+em_tran(7,11,icy))/qDStr(11,iy) !   diesel with biodiesel adjustment
           T22(35,iy,is) = dot_product(TRQBOAT(1:2,IY),c8(1:2))     !  Recreational Boats
! Air
           c8(1)=eJFtr(iy)*C_CO2_FACTOR
           c8(2)=eMGtr(iy)*C_CO2_FACTOR  ! aviation gas is carried in other tran category, which is mostly lubricants so has a low factor.  use pure mg factor instead here.
           T22(36,iy,is) = dot_product(TRQAIRT(1:2,IY),c8(1:2))     !  Air
!  Military Use
           c8=0.
           c8(1:2)=eJFtr(iy)*C_CO2_FACTOR
           c8(3)=eRStr(iy)*C_CO2_FACTOR
           c8(4)=(em_tran(6,11,icy)+em_tran(7,11,icy))/qDStr(11,iy) !   diesel with biodiesel adjustment
           T22(37,iy,is) = dot_product(TRQMIL(1:4,IY),c8(1:4))      !  Military Use
!  Lubricants
           T22(38,iy,is) = TRQLUB(IY)*eOTtr(iy)*C_CO2_FACTOR      !  Lubricants

           T22(39,iy,is) = QGPTR(11,IY)*eGPtr(iy)*C_CO2_FACTOR*1000.    !  Pipeline Fuel
           T22(40,iy,is) = sum(T22(27:39,iy,is))                  !    Total
           T22(27:40,iy,is) = T22(27:40,iy,is)*.001
           T22(66,iy,is)= sum(em_tran(1:iel_T,11,icy))-T22(40,iy,is)  ! discrepancy
           T22(40,iy,is) = T22(40,iy,is) + T22(66,iy,is)           ! total with discrepancy to match table 17
 460     CONTINUE

!     Table 47. Light-Duty Vehicle Energy Consumption by Tech and Fuel

         DO 470, IY = 1,LASTYR
!          --- LIGHT-DUTY CONSUMPTION BY TECHNOLOGY TYPE
!          --- CONVENTIONAL VEHICLES
           T47( 1,IY,IS) = TRLDQTEK( 1,IY)
           T47( 2,IY,IS) = TRLDQTEK( 2,IY)
!          --- ALTERNATIVE FUEL VEHICLES
!          --- ALCOHOL
           T47( 4,IY,IS) = TRLDQTEK( 3,IY)
           T47( 5,IY,IS) = TRLDQTEK( 4,IY)
!          --- NATURAL GAS
           T47( 7,IY,IS) = TRLDQTEK(11,IY)
           T47( 8,IY,IS) = TRLDQTEK( 9,IY)
           T47( 9,IY,IS) = TRLDQTEK(12,IY)
           T47(10,IY,IS) = TRLDQTEK(10,IY)
!          --- ELECTRIC
           T47(11,IY,IS) = TRLDQTEK( 7,IY)
           T47(12,IY,IS) = TRLDQTEK( 5,IY)
           T47( 3,IY,IS) = TRLDQTEK( 6,IY)
           T47(13,IY,IS) = TRLDQTEK( 8,IY)
           T47(14,IY,IS) = TRLDQTEK(16,IY)
!          --- FUEL CELL
           T47(15,IY,IS) = TRLDQTEK(15,IY)
           T47(16,IY,IS) = TRLDQTEK(13,IY)
           T47(17,IY,IS) = TRLDQTEK(14,IY)
           T47(18,IY,IS) = T47( 1,IY,IS) + T47( 2,IY,IS)
           T47(19,IY,IS) = FSUM(T47( 3,IY,IS),15)
           T47(20,IY,IS) = T47(18,IY,IS) + T47(19,IY,IS)
!          --- LIGHT-DUTY CONSUMPTION BY FUEL TYPE
           T47(21,IY,IS) = TRQLDV(1,11,IY)
           T47(22,IY,IS) = TRQLDV(8,11,IY)
           T47(23,IY,IS) = TRQLDV(2,11,IY)
           T47(24,IY,IS) = TRQLDV(3,11,IY)
           T47(25,IY,IS) = TRQLDV(4,11,IY)
           T47(26,IY,IS) = TRQLDV(5,11,IY)
           T47(27,IY,IS) = TRQLDV(6,11,IY)
           T47(28,IY,IS) = TRQLDV(7,11,IY)
  470    CONTINUE

!     Table 48. Light-Duty Vehicle Sales by Technology

         DO 480, IY = 1,LASTYR
         DO      IR = 1,11
!          --- CONVENTIONAL CARS
           T48( 1,IR,IY,IS) = TRLDSALC(1,IR,IY) *1000.
           T48( 2,IR,IY,IS) = TRLDSALC(2,IR,IY) * 1000.
           T48( 3,IR,IY,IS) = T48(1,IR,IY,IS) + T48(2,IR,IY,IS)
!          --- ALTERNATIVE FUEL CARS
!          --- ETHANOL
           T48( 5,IR,IY,IS) = TRLDSALC(3,IR,IY)* 1000.
!          --- ELECTRIC
           T48( 6,IR,IY,IS) = TRLDSALC(4,IR,IY)* 1000.
           T48( 7,IR,IY,IS) = TRLDSALC(7,IR,IY) * 1000.
           T48(15,IR,IY,IS) = TRLDSALC(15,IR,IY) * 1000.
           T48( 8,IR,IY,IS) = TRLDSALC(5,IR,IY) * 1000.
           T48( 4,IR,IY,IS) = TRLDSALC(6,IR,IY)* 1000.
           T48( 9,IR,IY,IS) = TRLDSALC(8,IR,IY) * 1000.
           T48(10,IR,IY,IS) = TRLDSALC(16,IR,IY) * 1000.
!          --- NATURAL GAS
           T48(11,IR,IY,IS) = TRLDSALC(11,IR,IY) * 1000.
           T48(12,IR,IY,IS) = TRLDSALC(9,IR,IY) * 1000.
           T48(13,IR,IY,IS) = TRLDSALC(12,IR,IY) * 1000.
           T48(14,IR,IY,IS) = TRLDSALC(10,IR,IY) * 1000.
!          --- Fuel Cell
           T48(16,IR,IY,IS) = TRLDSALC(13,IR,IY) * 1000.
           T48(17,IR,IY,IS) = TRLDSALC(14,IR,IY) * 1000.
           T48(18,IR,IY,IS) = FSUM(T48(4,IR,IY,IS),14)
           T48(20,IR,IY,IS) = T48(3,IR,IY,IS) + T48(18,IR,IY,IS)
           IF (T48(20,IR,IY,IS) .NE. 0.0) &
             T48(19,IR,IY,IS) = T48(18,IR,IY,IS) / T48(20,IR,IY,IS) * 100.
!          --- CONVENTIONAL TRUCKS
           T48(21,IR,IY,IS) = TRLDSALT(1,IR,IY) * 1000.
           T48(22,IR,IY,IS) = TRLDSALT(2,IR,IY) * 1000.
           T48(23,IR,IY,IS) = T48(21,IR,IY,IS) + T48(22,IR,IY,IS)
!          --- ALTERNATIVE FUEL TRUCKS
!          --- ETHANOL
           T48(25,IR,IY,IS) = TRLDSALT(3,IR,IY) * 1000.
!          --- ELECTRIC
           T48(26,IR,IY,IS) = TRLDSALT(4,IR,IY) * 1000.
           T48(27,IR,IY,IS) = TRLDSALT(7,IR,IY) * 1000.
           T48(35,IR,IY,IS) = TRLDSALT(15,IR,IY) * 1000.
           T48(28,IR,IY,IS) = TRLDSALT(5,IR,IY) * 1000.
           T48(24,IR,IY,IS) = TRLDSALT(6,IR,IY) * 1000.
           T48(29,IR,IY,IS) = TRLDSALT(8,IR,IY) * 1000.
           T48(30,IR,IY,IS) = TRLDSALT(16,IR,IY) * 1000.
!          --- NATURAL GAS
           T48(31,IR,IY,IS) = TRLDSALT(11,IR,IY) * 1000.
           T48(32,IR,IY,IS) = TRLDSALT(9,IR,IY) * 1000.
           T48(33,IR,IY,IS) = TRLDSALT(12,IR,IY) * 1000.
           T48(34,IR,IY,IS) = TRLDSALT(10,IR,IY) * 1000.
!          --- Fuel Cell
           T48(36,IR,IY,IS) = TRLDSALT(13,IR,IY) * 1000.
           T48(37,IR,IY,IS) = TRLDSALT(14,IR,IY) * 1000.
           T48(38,IR,IY,IS) = FSUM(T48(24,IR,IY,IS),14)
           T48(40,IR,IY,IS) = (T48(23,IR,IY,IS) +T48(38,IR,IY,IS))
           IF (T48(40,IR,IY,IS) .NE. 0.0) &
             T48(39,IR,IY,IS) = T48(38,IR,IY,IS) / T48(40,IR,IY,IS) * 100.
!          --- PERCENT OF SALES
           T48(41,IR,IY,IS) = LEGALTSAL(1,IR,IY) * 100.
           T48(42,IR,IY,IS) = LEGALTSAL(2,IR,IY) * 1000.
           T48(43,IR,IY,IS) = LEGALTSAL(3,IR,IY) * 1000.
           T48(44,IR,IY,IS) = T48(20,IR,IY,IS) + T48(40,IR,IY,IS)

           T48(51,IR,IY,IS) =(TRLDSALC( 1,IR,IY) + TRLDSALT( 1,IR,IY)) * 1000.
           T48(45,IR,IY,IS) =(TRLDSALC( 2,IR,IY) + TRLDSALT( 2,IR,IY)) * 1000.
           T48(46,IR,IY,IS) =(TRLDSALC( 8,IR,IY) + TRLDSALT( 8,IR,IY) + &
                              TRLDSALC(16,IR,IY) + TRLDSALT(16,IR,IY)) * 1000.
           T48(53,IR,IY,IS) =(TRLDSALC( 5,IR,IY) + TRLDSALT( 5,IR,IY) + &
                              TRLDSALC( 6,IR,IY) + TRLDSALT( 6,IR,IY)) * 1000.
           T48(47,IR,IY,IS) =(TRLDSALC( 3,IR,IY) + TRLDSALT( 3,IR,IY)) * 1000.
           T48(48,IR,IY,IS) =(TRLDSALC( 9,IR,IY) + TRLDSALT( 9,IR,IY) + &
                              TRLDSALC(10,IR,IY) + TRLDSALT(10,IR,IY) + &
                              TRLDSALC(11,IR,IY) + TRLDSALT(11,IR,IY) + &
                              TRLDSALC(12,IR,IY) + TRLDSALT(12,IR,IY)) * 1000.
           T48(49,IR,IY,IS) =(TRLDSALC( 7,IR,IY) + TRLDSALT( 7,IR,IY) + &
                              TRLDSALC( 4,IR,IY) + TRLDSALT( 4,IR,IY) + &
                              TRLDSALC(15,IR,IY) + TRLDSALT(15,IR,IY)) * 1000.
           T48(50,IR,IY,IS) =(TRLDSALC(13,IR,IY) + TRLDSALT(13,IR,IY) + &
                              TRLDSALC(14,IR,IY) + TRLDSALT(14,IR,IY)) * 1000.

! Microhybrid section (engine off at idle)
! The portion of this section involving Conventional vehicles (Gasoline, Diesel) needs
! to be added to other alternative fueled vehicles to get total alternative vehicles
           T48(54,IR,IY,IS) =(TRMICROS( 1, 1,IR,IY) + TRMICROS( 2, 1,IR,IY)) * 1000.
           T48(55,IR,IY,IS) =(TRMICROS( 1, 2,IR,IY) + TRMICROS( 2, 2,IR,IY)) * 1000.
           T48(57,IR,IY,IS) =(TRMICROS( 1, 8,IR,IY) + TRMICROS( 2, 8,IR,IY) + &
                              TRMICROS( 1,16,IR,IY) + TRMICROS( 2,16,IR,IY)) * 1000.
           T48(58,IR,IY,IS) =(TRMICROS( 1, 5,IR,IY) + TRMICROS( 2, 5,IR,IY) + &
                              TRMICROS( 1, 6,IR,IY) + TRMICROS( 2, 6,IR,IY)) * 1000.
           T48(59,IR,IY,IS) =(TRMICROS( 1, 3,IR,IY) + TRMICROS( 2, 3,IR,IY)) * 1000.
           T48(60,IR,IY,IS) =(TRMICROS( 1, 9,IR,IY) + TRMICROS( 2, 9,IR,IY) + &
                              TRMICROS( 1,10,IR,IY) + TRMICROS( 2,10,IR,IY) + &
                              TRMICROS( 1,11,IR,IY) + TRMICROS( 2,11,IR,IY) + &
                              TRMICROS( 1,12,IR,IY) + TRMICROS( 2,12,IR,IY)) * 1000.
           T48(61,IR,IY,IS) =(TRMICROS( 1, 7,IR,IY) + TRMICROS( 2, 7,IR,IY) + &
                              TRMICROS( 1, 4,IR,IY) + TRMICROS( 2, 4,IR,IY) + &
                              TRMICROS( 1,15,IR,IY) + TRMICROS( 2,15,IR,IY)) * 1000.
           T48(62,IR,IY,IS) =(TRMICROS( 1,13,IR,IY) + TRMICROS( 2,13,IR,IY) + &
                              TRMICROS( 1,14,IR,IY) + TRMICROS( 2,14,IR,IY)) * 1000.
           T48(63,IR,IY,IS) = FSUM(T48(54,IR,IY,IS),9)
           T48(64,IR,IY,IS) = FSUM(T48(45,IR,IY,IS),9) + T48(54,IR,IY,IS) - T48(51,IR,IY,IS)

! ZEV credit bank balance bonus rows
           IF (IR .LE. MNUMCR-2) THEN
              T48(65,IR,IY,IS) = ZEV_CREDIT_BANK(IR,1,IY)
              T48(66,IR,IY,IS) = ZEV_CREDIT_BANK(IR,2,IY)
              T48(67,IR,IY,IS) = ZEV_CREDIT_BANK(IR,3,IY)
           ELSE IF (IR .EQ. MNUMCR) THEN
              T48(65,IR,IY,IS) = sum(T48(65,1:MNUMCR-2,IY,IS))
              T48(66,IR,IY,IS) = sum(T48(66,1:MNUMCR-2,IY,IS))
              T48(67,IR,IY,IS) = sum(T48(67,1:MNUMCR-2,IY,IS))
           ENDIF
           ENDDO
  480    CONTINUE

!     Table 49. Light-Duty Vehicle Stock by Technology Type

         DO 490, IY = 1,LASTYR
!          --- CONVENTIONAL CARS
           T49( 1,IY,IS) = TRLDSTKC(1,IY)
           T49( 2,IY,IS) = TRLDSTKC(2,IY)
           T49( 3,IY,IS) = T49(1,IY,IS) + T49(2,IY,IS)
!          --- ALTERNATIVE FUEL CARS
!          --- ETHANOL
           T49( 5,IY,IS) = TRLDSTKC(3,IY)
!          --- ELECTRIC
           T49( 6,IY,IS) = TRLDSTKC(4,IY)
           T49( 7,IY,IS) = TRLDSTKC(7,IY)
           T49(15,IY,IS) = TRLDSTKC(15,IY)
           T49( 8,IY,IS) = TRLDSTKC(5,IY)
           T49( 4,IY,IS) = TRLDSTKC(6,IY)
           T49( 9,IY,IS) = TRLDSTKC(8,IY)
           T49(10,IY,IS) = TRLDSTKC(16,IY)
!          --- NATURAL GAS
           T49(11,IY,IS) = TRLDSTKC(11,IY)
           T49(12,IY,IS) = TRLDSTKC(9,IY)
           T49(13,IY,IS) = TRLDSTKC(12,IY)
           T49(14,IY,IS) = TRLDSTKC(10,IY)
!          --- FUEL CELL
           T49(16,IY,IS) = TRLDSTKC(13,IY)
           T49(17,IY,IS) = TRLDSTKC(14,IY)
           T49(18,IY,IS) = FSUM(T49(4,IY,IS),14)
           T49(20,IY,IS) = T49(3,IY,IS) + T49(18,IY,IS)
!          --- CONVENTIONAL TRUCKS
           T49(21,IY,IS) = TRLDSTKT(1,IY)
           T49(22,IY,IS) = TRLDSTKT(2,IY)
           T49(23,IY,IS) = T49(21,IY,IS) + T49(22,IY,IS)
!          --- ALTERNATIVE FUEL TRUCKS
!          --- ETHANOL
           T49(25,IY,IS) = TRLDSTKT(3,IY)
!          --- ELECTRIC
           T49(26,IY,IS) = TRLDSTKT(4,IY)
           T49(27,IY,IS) = TRLDSTKT(7,IY)
           T49(35,IY,IS) = TRLDSTKT(15,IY)
           T49(28,IY,IS) = TRLDSTKT(5,IY)
           T49(24,IY,IS) = TRLDSTKT(6,IY)
           T49(29,IY,IS) = TRLDSTKT(8,IY)
           T49(30,IY,IS) = TRLDSTKT(16,IY)
!          --- NATURAL GAS
           T49(31,IY,IS) = TRLDSTKT(11,IY)
           T49(32,IY,IS) = TRLDSTKT(9,IY)
           T49(33,IY,IS) = TRLDSTKT(12,IY)
           T49(34,IY,IS) = TRLDSTKT(10,IY)
!          --- FUEL CELL
           T49(36,IY,IS) = TRLDSTKT(13,IY)
           T49(37,IY,IS) = TRLDSTKT(14,IY)
           T49(38,IY,IS) = FSUM(T49(24,IY,IS),14)
           T49(40,IY,IS) = T49(23,IY,IS) + T49(38,IY,IS)
           T49(41,IY,IS) = T49(20,IY,IS) + T49(40,IY,IS)

!		MDRAEO2023		   
           T49(42,IY,IS) =TRLDSTKT( 1,IY) + TRLDSTKC( 1,IY)		! Gasoline
           T49(43,IY,IS) =TRLDSTKT( 2,IY) + TRLDSTKC( 2,IY)		! Diesel
           T49(44,IY,IS) =TRLDSTKT( 8,IY) + TRLDSTKC( 8,IY) + &	! Hybrid
                              TRLDSTKT(16,IY) + TRLDSTKC(16,IY)
           T49(45,IY,IS) =TRLDSTKT( 5,IY) + TRLDSTKC( 5,IY) + &	! PHEV
                              TRLDSTKT( 6,IY) + TRLDSTKC( 6,IY)
           T49(46,IY,IS) =TRLDSTKT( 3,IY) + TRLDSTKC( 3,IY)		! Ethanol
           T49(47,IY,IS) =TRLDSTKT( 9,IY) + TRLDSTKC( 9,IY) + &	! CNG/LPG
                              TRLDSTKT(10,IY) + TRLDSTKC(10,IY) + &
                              TRLDSTKT(11,IY) + TRLDSTKC(11,IY) + &
                              TRLDSTKT(12,IY) + TRLDSTKC(12,IY)
           T49(48,IY,IS) =TRLDSTKT( 7,IY) + TRLDSTKC( 7,IY) + &	! BEV
                              TRLDSTKT( 4,IY) + TRLDSTKC( 4,IY) + &
                              TRLDSTKT(15,IY) + TRLDSTKC(15,IY)
           T49(49,IY,IS) =TRLDSTKT(13,IY) + TRLDSTKC(13,IY) + &	! Fuel cells
                              TRLDSTKT(14,IY) + TRLDSTKC(14,IY)
							  
  490    CONTINUE

!     Table 50. Light-Duty Vehicle MPG by Technology Type

         DO 500, IY = 1,LASTYR
!          --- CONVENTIONAL CARS
           T50( 1,IY,IS) = TRLDMPGC( 1,IY)
           T50( 2,IY,IS) = TRLDMPGC( 2,IY)
           T50( 3,IY,IS) = 0.0
!          --- ALTERNATIVE FUEL CARS
!          --- ETHANOL
           T50( 5,IY,IS) = TRLDMPGC( 3,IY)
!          --- ELECTRICITY
           T50( 6,IY,IS) = TRLDMPGC( 4,IY)
           T50( 7,IY,IS) = TRLDMPGC( 7,IY)
           T50(15,IY,IS) = TRLDMPGC(15,IY)
           T50( 8,IY,IS) = TRLDMPGC( 5,IY)
           T50( 4,IY,IS) = TRLDMPGC( 6,IY)
           T50( 9,IY,IS) = TRLDMPGC( 8,IY)
           T50(10,IY,IS) = TRLDMPGC(16,IY)
!          --- NATURAL GAS
           T50(11,IY,IS) = TRLDMPGC(11,IY)
           T50(12,IY,IS) = TRLDMPGC( 9,IY)
           T50(13,IY,IS) = TRLDMPGC(12,IY)
           T50(14,IY,IS) = TRLDMPGC(10,IY)
!          --- FUEL CELL
           T50(16,IY,IS) = TRLDMPGC(13,IY)
           T50(17,IY,IS) = TRLDMPGC(14,IY)

           T50(20,IY,IS) = NEWMPG(1,IY)
!          --- CONVENTIONAL TRUCKS
           T50(21,IY,IS) = TRLDMPGT( 1,IY)
           T50(22,IY,IS) = TRLDMPGT( 2,IY)
!          --- ALTERNATIVE FUEL TRUCKS
!          --- ETHANOL
           T50(25,IY,IS) = TRLDMPGT( 3,IY)
!          --- ELECTRICITY
           T50(26,IY,IS) = TRLDMPGT( 4,IY)
           T50(27,IY,IS) = TRLDMPGT( 7,IY)
           T50(35,IY,IS) = TRLDMPGT(15,IY)
           T50(28,IY,IS) = TRLDMPGT( 5,IY)
           T50(24,IY,IS) = TRLDMPGT( 6,IY)
           T50(29,IY,IS) = TRLDMPGT( 8,IY)
           T50(30,IY,IS) = TRLDMPGT(16,IY)
!          --- NATURAL GAS
           T50(31,IY,IS) = TRLDMPGT(11,IY)
           T50(32,IY,IS) = TRLDMPGT( 9,IY)
           T50(33,IY,IS) = TRLDMPGT(12,IY)
           T50(34,IY,IS) = TRLDMPGT(10,IY)
!          --- FUEL CELL
           T50(36,IY,IS) = TRLDMPGT(13,IY)
           T50(37,IY,IS) = TRLDMPGT(14,IY)

           T50(40,IY,IS) = NEWMPG(2,IY)
           T50(41,IY,IS) = NEWMPG(3,IY)
!          --- FLEET AVERAGES
           T50(42,IY,IS) = TRLDMPGF(1,IY)
           T50(43,IY,IS) = TRLDMPGF(2,IY)
           T50(44,IY,IS) = TRLDMPGF(3,IY)
!  New and Stock averages for Car + Light truck + Commercial light truck
           T50(45,IY,IS) = TECHMPG(1,IY)
           T50(46,IY,IS) = TECHMPG(2,IY)
           T50(47,IY,IS) = TECHMPG(3,IY)
           T50(48,IY,IS) = TECHMPG(4,IY)
           T50(49,IY,IS) = TECHMPG(5,IY)
           T50(50,IY,IS) = TECHMPG(6,IY)
           T50(51,IY,IS) = TECHMPG(7,IY)
           T50(52,IY,IS) = TECHMPG(8,IY)
           T50(53,IY,IS) = LDV_MPG(2,IY)
           T50(54,IY,IS) = STKMPG(1,IY)
           T50(55,IY,IS) = STKMPG(2,IY)
           T50(56,IY,IS) = STKMPG(3,IY)
           T50(57,IY,IS) = STKMPG(4,IY)
           T50(58,IY,IS) = STKMPG(5,IY)
           T50(59,IY,IS) = STKMPG(6,IY)
           T50(60,IY,IS) = STKMPG(7,IY)
           T50(61,IY,IS) = STKMPG(8,IY)
           T50(62,IY,IS) = LDV_MPG(3,IY)
  500    CONTINUE

!     Table 51. Light-Duty Vehicle VMT by Technology Type

         DO 510, IY = 1,LASTYR
!          --- CONVENTIONAL VEHICLES
           T51( 1,IY,IS) = TRLDVMT( 1,IY)
           T51( 2,IY,IS) = TRLDVMT( 2,IY)
!          --- ALTENATIVE FUEL VEHICLES
!          --- ETHANOL
           T51( 5,IY,IS) = TRLDVMT( 3,IY)
!          --- ELECTRIC
           T51( 6,IY,IS) = TRLDVMT( 4,IY)
           T51( 7,IY,IS) = TRLDVMT( 7,IY)
           T51(15,IY,IS) = TRLDVMT(15,IY)
           T51( 8,IY,IS) = TRLDVMT( 5,IY)
           T51( 4,IY,IS) = TRLDVMT( 6,IY)
           T51( 9,IY,IS) = TRLDVMT( 8,IY)
           T51(10,IY,IS) = TRLDVMT(16,IY)
!          --- NATURAL GAS
           T51(11,IY,IS) = TRLDVMT(11,IY)
           T51(12,IY,IS) = TRLDVMT( 9,IY)
           T51(13,IY,IS) = TRLDVMT(12,IY)
           T51(14,IY,IS) = TRLDVMT(10,IY)
!          --- FUEL CELL
           T51(16,IY,IS) = TRLDVMT(13,IY)
           T51(17,IY,IS) = TRLDVMT(14,IY)
!          --- VMT EQUATION COMPONENTS
           T51(21,IY,IS) = TRLDVMTE(1,IY)
           T51(22,IY,IS) = TRLDVMTE(2,IY)
           T51(23,IY,IS) = TRLDVMTE(3,IY)
!          --- PRICE EFFECTS
           T51(24,IY,IS) = TRLDVMTE(4,IY)
           T51(25,IY,IS) = TRLDVMTE(5,IY)
           T51(26,IY,IS) = TRLDVMTE(6,IY)
           T51(27,IY,IS) = TRLDVMTE(7,IY)
!          --- INCOME EFFECTS
           T51(28,IY,IS) = TRLDVMTE(8,IY)
           T51(29,IY,IS) = TRLDVMTE(9,IY)
!          --- DEMOGRAPHIC DRIVING POPULATION EFFECT
           T51(30,IY,IS) = TRLDVMTE(10,IY)
           T51(31,IY,IS) = TRLDVMTE(11,IY)
  510    CONTINUE

!     Table 52. Summary of New Light-Duty Vehicle Size Class Attributes

         DO 520, IY = 1,LASTYR
!        --- EFFICIENCY
         T52( 1:10,IY,IS) = TREFFCAR(1:10,IY)    ! Cars
         T52(11:20,IY,IS) = TREFFTRK(1:10,IY)    ! Trucks
!        ---DEGADATION FACTORS
         T52(21:22,IY,IS) = DEGRPT(1:2,IY)
!        --- ALTERNATIVE FUEL CARS
         T52(23:31,IY,IS) = TREFFALTC(1:9,IY)
!        --- ALTERNATIVE FUEL LIGHT TRUCKS
         T52(32:40,IY,IS) = TREFFALTT(1:9,IY)
!        --- FLEET VEHICLE FUEL EFF.
         T52(41:44,IY,IS) = TREFFFLT(1:4,IY)
!        --- SALES SHARE
         T52(45:52,IY,IS) = TRSLSHRC(1:8,IY) * 100.    ! Cars
         T52(53:60,IY,IS) = TRSLSHRT(1:8,IY) * 100.    ! Trucks
!        --- HORSE POWER
         T52(61:69,IY,IS) = TRHPCAR(1:9,IY)    ! Cars
         T52(70:78,IY,IS) = TRHPTRK(1:9,IY)    ! Trucks
!        --- WEIGHT-New Vehicles
         T52(79:87,IY,IS) = TRWTCAR(1:9,IY)    ! Cars
         T52(88:96,IY,IS) = TRWTTRK(1:9,IY)    ! Trucks
!        --- WEIGHT-Stock
         T52(97,IY,IS) = TRWTCAR_STOCK(IY)    ! Cars
         T52(98,IY,IS) = TRWTTRK_STOCK(IY)    ! Trucks
  520    CONTINUE

!     Table 53. NEMS Transportation Fleet Car and Truck Consumption

        DO 530, IY = 1,LASTYR
!          --- CONVENTIONAL CARS
           T53( 1,IY,IS) = FLTFCLDVBTU(1,1,IY)
           T53( 2,IY,IS) = FLTFCLDVBTU(1,2,IY)
           T53( 3,IY,IS) = T53(1,IY,IS) + T53(2,IY,IS)
!          --- ALTERNATIVE FUEL CARS
!          --- ETHANOL
           T53( 5,IY,IS) = FLTFCLDVBTU(1,3,IY)
!          --- ELECTRIC
           T53( 6,IY,IS) = FLTFCLDVBTU(1,4,IY)
           T53( 7,IY,IS) = FLTFCLDVBTU(1,7,IY)
           T53(15,IY,IS) = FLTFCLDVBTU(1,15,IY)
           T53( 8,IY,IS) = FLTFCLDVBTU(1,5,IY)
           T53( 4,IY,IS) = FLTFCLDVBTU(1,6,IY)
           T53( 9,IY,IS) = FLTFCLDVBTU(1,8,IY)
           T53(10,IY,IS) = FLTFCLDVBTU(1,16,IY)
!          --- NATURAL GAS
           T53(11,IY,IS) = FLTFCLDVBTU(1,11,IY)
           T53(12,IY,IS) = FLTFCLDVBTU(1,9,IY)
           T53(13,IY,IS) = FLTFCLDVBTU(1,12,IY)
           T53(14,IY,IS) = FLTFCLDVBTU(1,10,IY)
!          --- FUEL CELL
           T53(16,IY,IS) = FLTFCLDVBTU(1,13,IY)
           T53(17,IY,IS) = FLTFCLDVBTU(1,14,IY)
           T53(18,IY,IS) = FSUM(T53(4,IY,IS),14)
           T53(20,IY,IS) = T53(3,IY,IS) + T53(18,IY,IS)
!          --- CONVENTIONAL TRUCKS
           T53(21,IY,IS) = FLTFCLDVBTU(2,1,IY)
           T53(22,IY,IS) = FLTFCLDVBTU(2,2,IY)
           T53(23,IY,IS) = T53(21,IY,IS) + T53(22,IY,IS)
!          --- ALTERNATIVE FUEL TRUCKS
!          --- ETHANOL
           T53(25,IY,IS) = FLTFCLDVBTU(2,3,IY)
!          --- ELECTRIC
           T53(26,IY,IS) = FLTFCLDVBTU(2,4,IY)
           T53(27,IY,IS) = FLTFCLDVBTU(2,7,IY)
           T53(35,IY,IS) = FLTFCLDVBTU(2,15,IY)
           T53(28,IY,IS) = FLTFCLDVBTU(2,5,IY)
           T53(24,IY,IS) = FLTFCLDVBTU(2,6,IY)
           T53(29,IY,IS) = FLTFCLDVBTU(2,8,IY)
           T53(30,IY,IS) = FLTFCLDVBTU(2,16,IY)
!          --- NATURAL GAS
           T53(31,IY,IS) = FLTFCLDVBTU(2,11,IY)
           T53(32,IY,IS) = FLTFCLDVBTU(2,9,IY)
           T53(33,IY,IS) = FLTFCLDVBTU(2,12,IY)
           T53(34,IY,IS) = FLTFCLDVBTU(2,10,IY)
!          --- FUEL CELL
           T53(36,IY,IS) = FLTFCLDVBTU(2,13,IY)
           T53(37,IY,IS) = FLTFCLDVBTU(2,14,IY)
           T53(38,IY,IS) = FSUM(T53(24,IY,IS),14)
           T53(40,IY,IS) = T53(23,IY,IS) + T53(38,IY,IS)
           T53(41,IY,IS) = T53(20,IY,IS) + T53(40,IY,IS)
!         --- COMMERCIAL LIGHT TUCKS
           T53(42:51,IY,IS) = BCLTBTUT(1:10,IY)
  530   CONTINUE

!     Table 54. NEMS Transportation Fleet Car and Truck Sales by Type and Tech.

        DO 540, IY = 1,LASTYR
!          --- CONVENTIONAL CARS
           T54( 1,IY,IS) = FLTECHRPT(1,1,IY)
           T54( 2,IY,IS) = FLTECHRPT(1,2,IY)
           T54( 3,IY,IS) = T54(1,IY,IS) + T54(2,IY,IS)
!          --- ALTERNATIVE FUEL CARS
!          --- ETHANOL
           T54( 5,IY,IS) = FLTECHRPT(1,3,IY)
!          --- ELECTRIC
           T54( 6,IY,IS) = FLTECHRPT(1,4,IY)
           T54( 7,IY,IS) = FLTECHRPT(1,7,IY)
           T54(15,IY,IS) = FLTECHRPT(1,15,IY)
           T54( 8,IY,IS) = FLTECHRPT(1,5,IY)
           T54( 4,IY,IS) = FLTECHRPT(1,6,IY)
           T54( 9,IY,IS) = FLTECHRPT(1,8,IY)
           T54(10,IY,IS) = FLTECHRPT(1,16,IY)
!          --- NATURAL GAS
           T54(11,IY,IS) = FLTECHRPT(1,11,IY)
           T54(12,IY,IS) = FLTECHRPT(1,9,IY)
           T54(13,IY,IS) = FLTECHRPT(1,12,IY)
           T54(14,IY,IS) = FLTECHRPT(1,10,IY)
!          --- FUEL CELL
           T54(16,IY,IS) = FLTECHRPT(1,13,IY)
           T54(17,IY,IS) = FLTECHRPT(1,14,IY)
           T54(18,IY,IS) = FSUM(T54(4,IY,IS),14)
           T54(20,IY,IS) = T54(3,IY,IS) + T54(18,IY,IS)
           T54(19,IY,IS) = T54(18,IY,IS) / T54(20,IY,IS) * 100.
!          --- CONVENTIONAL TRUCKS
           T54(21,IY,IS) = FLTECHRPT(2,1,IY)
           T54(22,IY,IS) = FLTECHRPT(2,2,IY)
           T54(23,IY,IS) = T54(21,IY,IS) + T54(22,IY,IS)
!          --- ALTERNATIVE FUEL TRUCKS
!          --- ETHANOL
           T54(25,IY,IS) = FLTECHRPT(2,3,IY)
!          --- ELECTRIC
           T54(26,IY,IS) = FLTECHRPT(2,4,IY)
           T54(27,IY,IS) = FLTECHRPT(2,7,IY)
           T54(35,IY,IS) = FLTECHRPT(2,15,IY)
           T54(28,IY,IS) = FLTECHRPT(2,5,IY)
           T54(24,IY,IS) = FLTECHRPT(2,6,IY)
           T54(29,IY,IS) = FLTECHRPT(2,8,IY)
           T54(30,IY,IS) = FLTECHRPT(2,16,IY)
!          --- NATURAL GAS
           T54(31,IY,IS) = FLTECHRPT(2,11,IY)
           T54(32,IY,IS) = FLTECHRPT(2,9,IY)
           T54(33,IY,IS) = FLTECHRPT(2,12,IY)
           T54(34,IY,IS) = FLTECHRPT(2,10,IY)
!          --- FUEL CELL
           T54(36,IY,IS) = FLTECHRPT(2,13,IY)
           T54(37,IY,IS) = FLTECHRPT(2,14,IY)
           T54(38,IY,IS) = FSUM(T54(24,IY,IS),14)
           T54(40,IY,IS) = T54(23,IY,IS) + T54(38,IY,IS)
           T54(39,IY,IS) = T54(38,IY,IS) / T54(40,IY,IS) * 100.
           T54(41,IY,IS) = T54(20,IY,IS) + T54(40,IY,IS)
!         --- COMMERCIAL LIGHT TUCKS
           T54(42:51,IY,IS) = CLTSALT(1:10,IY) * .001
  540   CONTINUE

!     Table 55. NEMS Transportation Fleet Car and Truck Stock by Type and Tech.

        DO 550, IY = 1,LASTYR
!          --- CONVENTIONAL CARS
           T55( 1,IY,IS) = FLTECHSTKRPT(1,1,IY)
           T55( 2,IY,IS) = FLTECHSTKRPT(1,2,IY)
           T55( 3,IY,IS) = T55(1,IY,IS) + T55(2,IY,IS)
!          --- ALTERNATIVE FUEL CARS
!          --- ETHANOL
           T55( 5,IY,IS) = FLTECHSTKRPT(1,3,IY)
!          --- ELECTRIC
           T55( 6,IY,IS) = FLTECHSTKRPT(1,4,IY)
           T55( 7,IY,IS) = FLTECHSTKRPT(1,7,IY)
           T55(15,IY,IS) = FLTECHSTKRPT(1,15,IY)
           T55( 8,IY,IS) = FLTECHSTKRPT(1,5,IY)
           T55( 4,IY,IS) = FLTECHSTKRPT(1,6,IY)
           T55( 9,IY,IS) = FLTECHSTKRPT(1,8,IY)
           T55(10,IY,IS) = FLTECHSTKRPT(1,16,IY)
!          --- NATURAL GAS
           T55(11,IY,IS) = FLTECHSTKRPT(1,11,IY)
           T55(12,IY,IS) = FLTECHSTKRPT(1,9,IY)
           T55(13,IY,IS) = FLTECHSTKRPT(1,12,IY)
           T55(14,IY,IS) = FLTECHSTKRPT(1,10,IY)
!          --- FUEL CELL
           T55(16,IY,IS) = FLTECHSTKRPT(1,13,IY)
           T55(17,IY,IS) = FLTECHSTKRPT(1,14,IY)
           T55(18,IY,IS) = FSUM(T55(4,IY,IS),14)
           T55(20,IY,IS) = T55(3,IY,IS) + T55(18,IY,IS)
!          --- CONVENTIONAL TRUCKS
           T55(21,IY,IS) = FLTECHSTKRPT(2,1,IY)
           T55(22,IY,IS) = FLTECHSTKRPT(2,2,IY)
           T55(23,IY,IS) = T55(21,IY,IS) + T55(22,IY,IS)
!          --- ALTERNATIVE FUEL TRUCKS
!          --- ETHANOL
           T55(25,IY,IS) = FLTECHSTKRPT(2,3,IY)
!          --- ELECTRIC
           T55(26,IY,IS) = FLTECHSTKRPT(2,4,IY)
           T55(27,IY,IS) = FLTECHSTKRPT(2,7,IY)
           T55(35,IY,IS) = FLTECHSTKRPT(2,15,IY)
           T55(28,IY,IS) = FLTECHSTKRPT(2,5,IY)
           T55(24,IY,IS) = FLTECHSTKRPT(2,6,IY)
           T55(29,IY,IS) = FLTECHSTKRPT(2,8,IY)
           T55(30,IY,IS) = FLTECHSTKRPT(2,16,IY)
!          --- NATURAL GAS
           T55(31,IY,IS) = FLTECHSTKRPT(2,11,IY)
           T55(32,IY,IS) = FLTECHSTKRPT(2,9,IY)
           T55(33,IY,IS) = FLTECHSTKRPT(2,12,IY)
           T55(34,IY,IS) = FLTECHSTKRPT(2,10,IY)
!          --- FUEL CELL
           T55(36,IY,IS) = FLTECHSTKRPT(2,13,IY)
           T55(37,IY,IS) = FLTECHSTKRPT(2,14,IY)
           T55(38,IY,IS) = FSUM(T55(24,IY,IS),14)
           T55(40,IY,IS) = T55(23,IY,IS) + T55(38,IY,IS)
           T55(41,IY,IS) = T55(20,IY,IS) + T55(40,IY,IS)
!         --- COMMERCIAL LIGHT TUCKS
           T55(42:51,IY,IS) = CLTSTKT(1:10,IY)
  550   CONTINUE

!     Table 56. NEMS Transportation Fleet Car and Truck VMT

        DO 560, IY = 1,LASTYR
!          --- CONVENTIONAL CARS
           T56( 1,IY,IS) = FLTECHVMTRPT(1,1,IY)
           T56( 2,IY,IS) = FLTECHVMTRPT(1,2,IY)
           T56( 3,IY,IS) = T56(1,IY,IS) + T56(2,IY,IS)
!          --- ALTERNATIVE FUEL CARS
!          --- ETHANOL
           T56( 5,IY,IS) = FLTECHVMTRPT(1,3,IY)
!          --- ELECTRIC
           T56( 6,IY,IS) = FLTECHVMTRPT(1,4,IY)
           T56( 7,IY,IS) = FLTECHVMTRPT(1,7,IY)
           T56(15,IY,IS) = FLTECHVMTRPT(1,15,IY)
           T56( 8,IY,IS) = FLTECHVMTRPT(1,5,IY)
           T56( 4,IY,IS) = FLTECHVMTRPT(1,6,IY)
           T56( 9,IY,IS) = FLTECHVMTRPT(1,8,IY)
           T56(10,IY,IS) = FLTECHVMTRPT(1,16,IY)
!          --- NATURAL GAS
           T56(11,IY,IS) = FLTECHVMTRPT(1,11,IY)
           T56(12,IY,IS) = FLTECHVMTRPT(1,9,IY)
           T56(13,IY,IS) = FLTECHVMTRPT(1,12,IY)
           T56(14,IY,IS) = FLTECHVMTRPT(1,10,IY)
!          --- FUEL CELL
           T56(16,IY,IS) = FLTECHVMTRPT(1,13,IY)
           T56(17,IY,IS) = FLTECHVMTRPT(1,14,IY)
           T56(18,IY,IS) = FSUM(T56(4,IY,IS),14)
           T56(20,IY,IS) = T56(3,IY,IS) + T56(18,IY,IS)
!          --- CONVENTIONAL TRUCKS
           T56(21,IY,IS) = FLTECHVMTRPT(2,1,IY)
           T56(22,IY,IS) = FLTECHVMTRPT(2,2,IY)
           T56(23,IY,IS) = T56(21,IY,IS) + T56(22,IY,IS)
!          --- ALTERNATIVE FUEL TRUCKS
!          --- ETHANOL
           T56(25,IY,IS) = FLTECHVMTRPT(2,3,IY)
!          --- ELECTRIC
           T56(26,IY,IS) = FLTECHVMTRPT(2,4,IY)
           T56(27,IY,IS) = FLTECHVMTRPT(2,7,IY)
           T56(35,IY,IS) = FLTECHVMTRPT(2,15,IY)
           T56(28,IY,IS) = FLTECHVMTRPT(2,5,IY)
           T56(24,IY,IS) = FLTECHVMTRPT(2,6,IY)
           T56(29,IY,IS) = FLTECHVMTRPT(2,8,IY)
           T56(30,IY,IS) = FLTECHVMTRPT(2,16,IY)
!          --- NATURAL GAS
           T56(31,IY,IS) = FLTECHVMTRPT(2,11,IY)
           T56(32,IY,IS) = FLTECHVMTRPT(2,9,IY)
           T56(33,IY,IS) = FLTECHVMTRPT(2,12,IY)
           T56(34,IY,IS) = FLTECHVMTRPT(2,10,IY)
!          --- FUEL CELL
           T56(36,IY,IS) = FLTECHVMTRPT(2,13,IY)
           T56(37,IY,IS) = FLTECHVMTRPT(2,14,IY)
           T56(38,IY,IS) = FSUM(T56(24,IY,IS),14)
           T56(40,IY,IS) = T56(23,IY,IS) + T56(38,IY,IS)
           T56(41,IY,IS) = T56(20,IY,IS) + T56(40,IY,IS)
!         --- COMMERCIAL LIGHT TUCKS
           T56(42:51,IY,IS) = BCLTVMT(1:10,IY)
  560   CONTINUE

!     Table 57. Air Travel Energy Use		! MDRAEO2023

         DO 570, IY = 1,LASTYR
! FUEL PRICE & YIELD(1-4); RPM & LF & RTM(US&NUS) (5-12); ASMD(US&NUS) (13-20); A/C SALES(US&NUS) (21-28);
! US STOCK & ACTIVE & PARKED (29-40); NUS STOCK & ACTIVE & PARKED (41-52); CARGO STOCK(US&NUS) (53-60)
		   T57( 1:193,IY,IS) = AIROUT(1:193,IY)		! demand info & sales
!          --- AIRCRAFT NEW EFFICIENCY
           T57(194:197,IY,IS) = TRAIREFFN(1:4,IY)
!          --- AIRCRAFT STOCK EFFICIENCY
           T57(198:201,IY,IS) = TRAIREFFS(1:4,IY)
!          --- FUEL CONSUMPTION
           T57(202:217,IY,IS) = AIROUT(415:430,IY) 	! commercial jet fuel consumption
           T57(218,IY,IS) = SUM(AIROUT(415:430,IY)) ! commercial jet fuel consumption - world
           T57(219,IY,IS) = TRQAIRT(2,IY)
           T57(220,IY,IS) = TRQMIL(1,IY) + TRQMIL(2,IY)
           T57(221:236,IY,IS) = WLD_GDP(1:16,IY)
           T57(237:252,IY,IS) = WLD_POP(1:16,IY)
  570    CONTINUE

!     Table 58. Freight Transportation Energy Use

         DO 580, IY = 1,LASTYR
!  Drive train order (the third subscript in variables with four):
!       1 = Diesel         2 = Gasoline       3 = Propane        4 = Natural gas    5 = E85 flex
!       6 = Electric       7 = PHEV diesel    8 = PHEV gasoline  9 = Fuel cell
!    --- EXISTING TRUCKS
!          --- VMT
           T58(  1,IY,IS) = TFR_VMT_FAS_T(IY,1,1,2)
           T58(  2,IY,IS) = TFR_VMT_FAS_T(IY,1,2,2)
           T58(  3,IY,IS) = TFR_VMT_FAS_T(IY,1,3,2)
           T58(  4,IY,IS) = TFR_VMT_FAS_T(IY,1,4,2)
           T58(  5,IY,IS) = TFR_VMT_FAS_T(IY,1,5,2)
           T58(  6,IY,IS) = TFR_VMT_FAS_T(IY,1,6,2)
           T58(  7,IY,IS) = TFR_VMT_FAS_T(IY,1,7,2)
           T58(  8,IY,IS) = TFR_VMT_FAS_T(IY,1,8,2)
           T58(  9,IY,IS) = TFR_VMT_FAS_T(IY,1,9,2)
           T58( 10,IY,IS) = TFR_VMT_FASF_T(IY,1,2)
           T58( 11,IY,IS) = TFR_VMT_FAS_T(IY,2,1,2)
           T58( 12,IY,IS) = TFR_VMT_FAS_T(IY,2,2,2)
           T58( 13,IY,IS) = TFR_VMT_FAS_T(IY,2,3,2)
           T58( 14,IY,IS) = TFR_VMT_FAS_T(IY,2,4,2)
           T58( 15,IY,IS) = TFR_VMT_FAS_T(IY,2,5,2)
           T58( 16,IY,IS) = TFR_VMT_FAS_T(IY,2,6,2)
           T58( 17,IY,IS) = TFR_VMT_FAS_T(IY,2,7,2)
           T58( 18,IY,IS) = TFR_VMT_FAS_T(IY,2,8,2)
           T58( 19,IY,IS) = TFR_VMT_FAS_T(IY,2,9,2)
           T58( 20,IY,IS) = TFR_VMT_FASF_T(IY,2,2)
           T58( 21,IY,IS) = TFR_VMT_FAS_T(IY,3,1,2)
           T58( 22,IY,IS) = TFR_VMT_FAS_T(IY,3,2,2)
           T58( 23,IY,IS) = TFR_VMT_FAS_T(IY,3,3,2)
           T58( 24,IY,IS) = TFR_VMT_FAS_T(IY,3,4,2)
           T58( 25,IY,IS) = TFR_VMT_FAS_T(IY,3,5,2)
           T58( 26,IY,IS) = TFR_VMT_FAS_T(IY,3,6,2)
           T58( 27,IY,IS) = TFR_VMT_FAS_T(IY,3,7,2)
           T58( 28,IY,IS) = TFR_VMT_FAS_T(IY,3,8,2)
           T58( 29,IY,IS) = TFR_VMT_FAS_T(IY,3,9,2)
           T58( 30,IY,IS) = TFR_VMT_FASF_T(IY,3,2)
           T58(191,IY,IS) = TFR_VMT_TR(IY,2)
!          --- CONSUMPTION
           T58( 31,IY,IS) = TFR_FBTU_FAS_T(IY,1,1,2)
           T58( 32,IY,IS) = TFR_FBTU_FAS_T(IY,1,2,2)
           T58( 33,IY,IS) = TFR_FBTU_FAS_T(IY,1,3,2)
           T58( 34,IY,IS) = TFR_FBTU_FAS_T(IY,1,4,2)
           T58( 35,IY,IS) = TFR_FBTU_FAS_T(IY,1,5,2)
           T58( 36,IY,IS) = TFR_FBTU_FAS_T(IY,1,6,2)
           T58( 37,IY,IS) = TFR_FBTU_FAS_T(IY,1,7,2)
           T58( 38,IY,IS) = TFR_FBTU_FAS_T(IY,1,8,2)
           T58( 39,IY,IS) = TFR_FBTU_FAS_T(IY,1,9,2)
           T58( 40,IY,IS) = TFR_FBTU_FASF_T(IY,1,2)
           T58( 41,IY,IS) = TFR_FBTU_FAS_T(IY,2,1,2)
           T58( 42,IY,IS) = TFR_FBTU_FAS_T(IY,2,2,2)
           T58( 43,IY,IS) = TFR_FBTU_FAS_T(IY,2,3,2)
           T58( 44,IY,IS) = TFR_FBTU_FAS_T(IY,2,4,2)
           T58( 45,IY,IS) = TFR_FBTU_FAS_T(IY,2,5,2)
           T58( 46,IY,IS) = TFR_FBTU_FAS_T(IY,2,6,2)
           T58( 47,IY,IS) = TFR_FBTU_FAS_T(IY,2,7,2)
           T58( 48,IY,IS) = TFR_FBTU_FAS_T(IY,2,8,2)
           T58( 49,IY,IS) = TFR_FBTU_FAS_T(IY,2,9,2)
           T58( 50,IY,IS) = TFR_FBTU_FASF_T(IY,2,2)
           T58( 51,IY,IS) = TFR_FBTU_FAS_T(IY,3,1,2)
           T58( 52,IY,IS) = TFR_FBTU_FAS_T(IY,3,2,2)
           T58( 53,IY,IS) = TFR_FBTU_FAS_T(IY,3,3,2)
           T58( 54,IY,IS) = TFR_FBTU_FAS_T(IY,3,4,2)
           T58( 55,IY,IS) = TFR_FBTU_FAS_T(IY,3,5,2)
           T58( 56,IY,IS) = TFR_FBTU_FAS_T(IY,3,6,2)
           T58( 57,IY,IS) = TFR_FBTU_FAS_T(IY,3,7,2)
           T58( 58,IY,IS) = TFR_FBTU_FAS_T(IY,3,8,2)
           T58( 59,IY,IS) = TFR_FBTU_FAS_T(IY,3,9,2)
           T58( 60,IY,IS) = TFR_FBTU_FASF_T(IY,3,2)
           T58( 61,IY,IS) = T58(31,IY,IS) + T58(41,IY,IS) + T58(51,IY,IS)
           T58( 62,IY,IS) = T58(32,IY,IS) + T58(42,IY,IS) + T58(52,IY,IS)
           T58( 63,IY,IS) = T58(33,IY,IS) + T58(43,IY,IS) + T58(53,IY,IS)
           T58( 64,IY,IS) = T58(34,IY,IS) + T58(44,IY,IS) + T58(54,IY,IS)
           T58( 65,IY,IS) = T58(35,IY,IS) + T58(45,IY,IS) + T58(55,IY,IS)
           T58( 66,IY,IS) = T58(36,IY,IS) + T58(46,IY,IS) + T58(56,IY,IS)
           T58( 67,IY,IS) = T58(37,IY,IS) + T58(47,IY,IS) + T58(57,IY,IS)
           T58( 68,IY,IS) = T58(38,IY,IS) + T58(48,IY,IS) + T58(58,IY,IS)
           T58( 69,IY,IS) = T58(39,IY,IS) + T58(49,IY,IS) + T58(59,IY,IS)
           T58( 70,IY,IS) = TFR_FBTU_TR(IY,2)
!          --- EFFICIENCY
           T58( 71,IY,IS) = TFR_FTMPG(IY,1,1,2)
           T58( 72,IY,IS) = TFR_FTMPG(IY,1,2,2)
           T58( 73,IY,IS) = TFR_FTMPG(IY,1,3,2)
           T58( 74,IY,IS) = TFR_FTMPG(IY,1,4,2)
           T58( 75,IY,IS) = TFR_FTMPG(IY,1,5,2)
           T58( 76,IY,IS) = TFR_FTMPG(IY,1,6,2)
           T58( 77,IY,IS) = TFR_FTMPG(IY,1,7,2)
           T58( 78,IY,IS) = TFR_FTMPG(IY,1,8,2)
           T58( 79,IY,IS) = TFR_FTMPG(IY,1,9,2)
           T58( 80,IY,IS) = TFR_FTMPG_S(IY,1,2) ! class 3 (light medium)
           T58( 81,IY,IS) = TFR_FTMPG(IY,2,1,2)
           T58( 82,IY,IS) = TFR_FTMPG(IY,2,2,2)
           T58( 83,IY,IS) = TFR_FTMPG(IY,2,3,2)
           T58( 84,IY,IS) = TFR_FTMPG(IY,2,4,2)
           T58( 85,IY,IS) = TFR_FTMPG(IY,2,5,2)
           T58( 86,IY,IS) = TFR_FTMPG(IY,2,6,2)
           T58( 87,IY,IS) = TFR_FTMPG(IY,2,7,2)
           T58( 88,IY,IS) = TFR_FTMPG(IY,2,8,2)
           T58( 89,IY,IS) = TFR_FTMPG(IY,2,9,2)
           T58( 90,IY,IS) = TFR_FTMPG_S(IY,2,2) ! class 4-6 (medium)
           T58( 91,IY,IS) = TFR_FTMPG(IY,3,1,2)
           T58( 92,IY,IS) = TFR_FTMPG(IY,3,2,2)
           T58( 93,IY,IS) = TFR_FTMPG(IY,3,3,2)
           T58( 94,IY,IS) = TFR_FTMPG(IY,3,4,2)
           T58( 95,IY,IS) = TFR_FTMPG(IY,3,5,2)
           T58( 96,IY,IS) = TFR_FTMPG(IY,3,6,2)
           T58( 97,IY,IS) = TFR_FTMPG(IY,3,7,2)
           T58( 98,IY,IS) = TFR_FTMPG(IY,3,8,2)
           T58( 99,IY,IS) = TFR_FTMPG(IY,3,9,2)
           T58(100,IY,IS) = TFR_FTMPG_S(IY,3,2) ! class 7-8 (heavy)
           T58(192,IY,IS) = TFR_FTMPG_TR(IY,2)           ! average across types
!          --- STOCK
           T58(101,IY,IS) = TFR_TRK_FAS_T(IY,1,1,2)
           T58(102,IY,IS) = TFR_TRK_FAS_T(IY,1,2,2)
           T58(103,IY,IS) = TFR_TRK_FAS_T(IY,1,3,2)
           T58(104,IY,IS) = TFR_TRK_FAS_T(IY,1,4,2)
           T58(105,IY,IS) = TFR_TRK_FAS_T(IY,1,5,2)
           T58(106,IY,IS) = TFR_TRK_FAS_T(IY,1,6,2)
           T58(107,IY,IS) = TFR_TRK_FAS_T(IY,1,7,2)
           T58(108,IY,IS) = TFR_TRK_FAS_T(IY,1,8,2)
           T58(109,IY,IS) = TFR_TRK_FAS_T(IY,1,9,2)
           T58(110,IY,IS) = TFR_TRK_FASF_T(IY,1,2)
           T58(111,IY,IS) = TFR_TRK_FAS_T(IY,2,1,2)
           T58(112,IY,IS) = TFR_TRK_FAS_T(IY,2,2,2)
           T58(113,IY,IS) = TFR_TRK_FAS_T(IY,2,3,2)
           T58(114,IY,IS) = TFR_TRK_FAS_T(IY,2,4,2)
           T58(115,IY,IS) = TFR_TRK_FAS_T(IY,2,5,2)
           T58(116,IY,IS) = TFR_TRK_FAS_T(IY,2,6,2)
           T58(117,IY,IS) = TFR_TRK_FAS_T(IY,2,7,2)
           T58(118,IY,IS) = TFR_TRK_FAS_T(IY,2,8,2)
           T58(119,IY,IS) = TFR_TRK_FAS_T(IY,2,9,2)
           T58(120,IY,IS) = TFR_TRK_FASF_T(IY,2,2)
           T58(121,IY,IS) = TFR_TRK_FAS_T(IY,3,1,2)
           T58(122,IY,IS) = TFR_TRK_FAS_T(IY,3,2,2)
           T58(123,IY,IS) = TFR_TRK_FAS_T(IY,3,3,2)
           T58(124,IY,IS) = TFR_TRK_FAS_T(IY,3,4,2)
           T58(125,IY,IS) = TFR_TRK_FAS_T(IY,3,5,2)
           T58(126,IY,IS) = TFR_TRK_FAS_T(IY,3,6,2)
           T58(127,IY,IS) = TFR_TRK_FAS_T(IY,3,7,2)
           T58(128,IY,IS) = TFR_TRK_FAS_T(IY,3,8,2)
           T58(129,IY,IS) = TFR_TRK_FAS_T(IY,3,9,2)
           T58(130,IY,IS) = TFR_TRK_FASF_T(IY,3,2)
           T58(193,IY,IS) = TFR_TRK_TR(IY,2)             !  total stock
!    --- NEW TRUCKS
!          --- EFFICIENCY
           T58(131,IY,IS) = TFR_FTMPG(IY,1,1,1)
           T58(132,IY,IS) = TFR_FTMPG(IY,1,2,1)
           T58(133,IY,IS) = TFR_FTMPG(IY,1,3,1)
           T58(134,IY,IS) = TFR_FTMPG(IY,1,4,1)
           T58(135,IY,IS) = TFR_FTMPG(IY,1,5,1)
           T58(136,IY,IS) = TFR_FTMPG(IY,1,6,1)
           T58(137,IY,IS) = TFR_FTMPG(IY,1,7,1)
           T58(138,IY,IS) = TFR_FTMPG(IY,1,8,1)
           T58(139,IY,IS) = TFR_FTMPG(IY,1,9,1)
           T58(140,IY,IS) = TFR_FTMPG_S(IY,1,1) ! class 3 (light medium)
           T58(141,IY,IS) = TFR_FTMPG(IY,2,1,1)
           T58(142,IY,IS) = TFR_FTMPG(IY,2,2,1)
           T58(143,IY,IS) = TFR_FTMPG(IY,2,3,1)
           T58(144,IY,IS) = TFR_FTMPG(IY,2,4,1)
           T58(145,IY,IS) = TFR_FTMPG(IY,2,5,1)
           T58(146,IY,IS) = TFR_FTMPG(IY,2,6,1)
           T58(147,IY,IS) = TFR_FTMPG(IY,2,7,1)
           T58(148,IY,IS) = TFR_FTMPG(IY,2,8,1)
           T58(149,IY,IS) = TFR_FTMPG(IY,2,9,1)
           T58(150,IY,IS) = TFR_FTMPG_S(IY,2,1) ! class 4-5 (medium)
           T58(151,IY,IS) = TFR_FTMPG(IY,3,1,1)
           T58(152,IY,IS) = TFR_FTMPG(IY,3,2,1)
           T58(153,IY,IS) = TFR_FTMPG(IY,3,3,1)
           T58(154,IY,IS) = TFR_FTMPG(IY,3,4,1)
           T58(155,IY,IS) = TFR_FTMPG(IY,3,5,1)
           T58(156,IY,IS) = TFR_FTMPG(IY,3,6,1)
           T58(157,IY,IS) = TFR_FTMPG(IY,3,7,1)
           T58(158,IY,IS) = TFR_FTMPG(IY,3,8,1)
           T58(159,IY,IS) = TFR_FTMPG(IY,3,9,1)
           T58(160,IY,IS) = TFR_FTMPG_S(IY,3,1) ! class 7-8 (heavy)
           T58(194,IY,IS) = TFR_FTMPG_TR(IY,1)             ! average all types, new
!          --- Sales
           T58(161,IY,IS) = TFR_TRK_FAS_T(IY,1,1,1) * 1000.
           T58(162,IY,IS) = TFR_TRK_FAS_T(IY,1,2,1) * 1000.
           T58(163,IY,IS) = TFR_TRK_FAS_T(IY,1,3,1) * 1000.
           T58(164,IY,IS) = TFR_TRK_FAS_T(IY,1,4,1) * 1000.
           T58(165,IY,IS) = TFR_TRK_FAS_T(IY,1,5,1) * 1000.
           T58(166,IY,IS) = TFR_TRK_FAS_T(IY,1,6,1) * 1000.
           T58(167,IY,IS) = TFR_TRK_FAS_T(IY,1,7,1) * 1000.
           T58(168,IY,IS) = TFR_TRK_FAS_T(IY,1,8,1) * 1000.
           T58(169,IY,IS) = TFR_TRK_FAS_T(IY,1,9,1) * 1000.
           T58(170,IY,IS) = TFR_TRK_FASF_T(IY,1,1) * 1000.
           T58(171,IY,IS) = TFR_TRK_FAS_T(IY,2,1,1) * 1000.
           T58(172,IY,IS) = TFR_TRK_FAS_T(IY,2,2,1) * 1000.
           T58(173,IY,IS) = TFR_TRK_FAS_T(IY,2,3,1) * 1000.
           T58(174,IY,IS) = TFR_TRK_FAS_T(IY,2,4,1) * 1000.
           T58(175,IY,IS) = TFR_TRK_FAS_T(IY,2,5,1) * 1000.
           T58(176,IY,IS) = TFR_TRK_FAS_T(IY,2,6,1) * 1000.
           T58(177,IY,IS) = TFR_TRK_FAS_T(IY,2,7,1) * 1000.
           T58(178,IY,IS) = TFR_TRK_FAS_T(IY,2,8,1) * 1000.
           T58(179,IY,IS) = TFR_TRK_FAS_T(IY,2,9,1) * 1000.
           T58(180,IY,IS) = TFR_TRK_FASF_T(IY,2,1) * 1000.
           T58(181,IY,IS) = TFR_TRK_FAS_T(IY,3,1,1) * 1000.
           T58(182,IY,IS) = TFR_TRK_FAS_T(IY,3,2,1) * 1000.
           T58(183,IY,IS) = TFR_TRK_FAS_T(IY,3,3,1) * 1000.
           T58(184,IY,IS) = TFR_TRK_FAS_T(IY,3,4,1) * 1000.
           T58(185,IY,IS) = TFR_TRK_FAS_T(IY,3,5,1) * 1000.
           T58(186,IY,IS) = TFR_TRK_FAS_T(IY,3,6,1) * 1000.
           T58(187,IY,IS) = TFR_TRK_FAS_T(IY,3,7,1) * 1000.
           T58(188,IY,IS) = TFR_TRK_FAS_T(IY,3,8,1) * 1000.
           T58(189,IY,IS) = TFR_TRK_FAS_T(IY,3,9,1) * 1000.
           T58(190,IY,IS) = TFR_TRK_FASF_T(IY,3,1) * 1000.
           T58(195,IY,IS) = TFR_TRK_TR(IY,1) * 1000.          !  total sales
!  Freight truck consumption by fuel and size class
           T58(215,IY,IS) = TRQFTRK_NEW(1,1,IY)
           T58(216,IY,IS) = TRQFTRK_NEW(1,2,IY)
           T58(217,IY,IS) = TRQFTRK_NEW(1,3,IY)
           T58(218,IY,IS) = TRQFTRK_NEW(1,4,IY)
           T58(219,IY,IS) = TRQFTRK_NEW(1,5,IY)
           T58(220,IY,IS) = TRQFTRK_NEW(1,6,IY)
           T58(221,IY,IS) = FSUM(T58(215,IY,IS),6)
           T58(222,IY,IS) = TRQFTRK_NEW(2,1,IY)
           T58(223,IY,IS) = TRQFTRK_NEW(2,2,IY)
           T58(224,IY,IS) = TRQFTRK_NEW(2,3,IY)
           T58(225,IY,IS) = TRQFTRK_NEW(2,4,IY)
           T58(226,IY,IS) = TRQFTRK_NEW(2,5,IY)
           T58(227,IY,IS) = TRQFTRK_NEW(2,6,IY)
           T58(228,IY,IS) = FSUM(T58(222,IY,IS),6)
           T58(229,IY,IS) = TRQFTRK_NEW(3,1,IY)
           T58(230,IY,IS) = TRQFTRK_NEW(3,2,IY)
           T58(231,IY,IS) = TRQFTRK_NEW(3,3,IY)
           T58(232,IY,IS) = TRQFTRK_NEW(3,4,IY)
           T58(233,IY,IS) = TRQFTRK_NEW(3,5,IY)
           T58(234,IY,IS) = TRQFTRK_NEW(3,6,IY)
           T58(235,IY,IS) = FSUM(T58(229,IY,IS),6)
           T58(236,IY,IS) = T58(221,IY,IS) + T58(228,IY,IS) + T58(235,IY,IS)
!  Ton miles traveled by size class
           T58(237,IY,IS) = TRK_TMT(1,IY)
           T58(238,IY,IS) = TRK_TMT(2,IY)
           T58(239,IY,IS) = TRK_TMT(3,IY)
           T58(240,IY,IS) = FSUM(T58(237,IY,IS),3)
!          --- RAILROADS
           T58(196,IY,IS) = TRTMRR(1,IY)
           T58(197,IY,IS) = TRTMRR(2,IY)
           T58(198,IY,IS) = TRQRRF(1,IY)
           T58(199,IY,IS) = TRQRRF(2,IY)
           T58(200,IY,IS) = TRQRRF(3,IY)
           T58(201,IY,IS) = TRQRRF(4,IY)
!          --- DOMESTIC SHIPPING
           T58(202,IY,IS) = TRTMSHIP(1,IY)
           T58(203,IY,IS) = TRTMSHIP(2,IY)
           T58(204,IY,IS) = TRQDOMS(1,IY)
           T58(205,IY,IS) = TRQDOMS(2,IY)
           T58(206,IY,IS) = TRQDOMS(3,IY)
           T58(207,IY,IS) = TRQDOMS(4,IY)
!          --- INTERNATIONAL SHIPPING
           T58(208,IY,IS) = TRTRAVLD(8,IY) + TRIMSHIP(IY)
           T58(209,IY,IS) = TRTRAVLD(8,IY)
           T58(210,IY,IS) = TRIMSHIP(IY)
           T58(211,IY,IS) = TRQINTS(1,IY)
           T58(212,IY,IS) = TRQINTS(2,IY)
           T58(213,IY,IS) = TRQINTS(3,IY)
           T58(214,IY,IS) = TRQINTS(4,IY)
  580    CONTINUE

!     Table 59. Electric Generating Capability

      DO 590 IY=1,LASTYR
        DO IR = 1, MNUMNR
         T59( 1,IR,IY,IS) = UCAPCSU(IR,IY) + UCAPCSN(IR,IY) + UCAPCSC(IR,IY)
         T59( 2,IR,IY,IS) = UCAPIGU(IR,IY) + UCAPIGN(IR,IY) + UCAPIGC(IR,IY)
         T59(73,IR,IY,IS) = UCAPISU(IR,IY) + UCAPISN(IR,IY)
         T59(161,IR,IY,IS) = UCAPPQU(IR,IY) + UCAPPQN(IR,IY)
         T59(142,IR,IY,IS) = UCAPOCU(IR,IY) + UCAPOCN(IR,IY)
         T59(143,IR,IY,IS) = UCAPI2U(IR,IY) + UCAPI2N(IR,IY)
         T59( 3,IR,IY,IS) = T59(1,IR,IY,IS) - T59(2,IR,IY,IS) - T59(73,IR,IY,IS) - T59(161,IR,IY,IS) - &
                            T59(142,IR,IY,IS) - T59(143,IR,IY,IS)
         T59( 4,IR,IY,IS) = UCAPOSU(IR,IY) + UCAPOSN(IR,IY) + UCAPOSC(IR,IY) + &
                            UCAPNGU(IR,IY) + UCAPNGN(IR,IY)
         T59(133,IR,IY,IS) = UCAPOSU(IR,IY) + UCAPOSN(IR,IY) + UCAPOSC(IR,IY)
         T59(134,IR,IY,IS) = UCAPNGU(IR,IY) + UCAPNGN(IR,IY)
         T59( 5,IR,IY,IS) = UCAPCCU(IR,IY) + UCAPCCN(IR,IY) + UCAPCCC(IR,IY)
         T59( 6,IR,IY,IS) = UCAPACU(IR,IY) + UCAPACN(IR,IY) + UCAPACC(IR,IY)
         T59(74,IR,IY,IS) = UCAPASU(IR,IY) + UCAPASN(IR,IY)
         T59(146,IR,IY,IS) = UCAPA2U(IR,IY) + UCAPA2N(IR,IY)
         T59( 7,IR,IY,IS) = T59(5,IR,IY,IS) - T59(6,IR,IY,IS) - T59(74,IR,IY,IS) - T59(146,IR,IY,IS)
         T59( 8,IR,IY,IS) = UCAPCTU(IR,IY) + UCAPCTN(IR,IY) + UCAPCTC(IR,IY)
         T59( 9,IR,IY,IS) = UCAPATU(IR,IY) + UCAPATN(IR,IY) + UCAPATC(IR,IY)
         T59(144,IR,IY,IS) = UCAPICU(IR,IY) + UCAPICN(IR,IY)
         T59(145,IR,IY,IS) = UCAPT2U(IR,IY) + UCAPT2N(IR,IY)
         T59( 10,IR,IY,IS) = T59(8,IR,IY,IS) - T59(9,IR,IY,IS) - T59(144,IR,IY,IS) - T59(145,IR,IY,IS)
         T59( 11,IR,IY,IS) = UCAPNUU(IR,IY) + UCAPNUN(IR,IY)
         T59(137,IR,IY,IS) = UCAPSMU(IR,IY) + UCAPSMN(IR,IY)
         T59( 12,IR,IY,IS) = UCAPPSU(IR,IY) + UCAPPSN(IR,IY)
         T59( 13,IR,IY,IS) = UCAPFCU(IR,IY) + UCAPFCN(IR,IY)
         T59( 14,IR,IY,IS) = UCAPHYU(IR,IY) + UCAPHYN(IR,IY) + UCAPHYC(IR,IY)
         T59(112,IR,IY,IS) = UCAPGEU(IR,IY) + UCAPGEN(IR,IY) + UCAPGEC(IR,IY)
         T59(113,IR,IY,IS) = UCAPMSU(IR,IY) + UCAPMSN(IR,IY) + UCAPMSC(IR,IY)
         T59(114,IR,IY,IS) = UCAPWDU(IR,IY) + UCAPWDN(IR,IY) + UCAPWDC(IR,IY)
         T59(115,IR,IY,IS) = UCAPSTU(IR,IY) + UCAPSTN(IR,IY) + UCAPSTC(IR,IY)
         T59(116,IR,IY,IS) = UCAPPVU(IR,IY) + UCAPPVN(IR,IY) + UCAPPVC(IR,IY)
         T59(117,IR,IY,IS) = UCAPWNU(IR,IY) + UCAPWNN(IR,IY) + UCAPWNC(IR,IY)
         T59(118,IR,IY,IS) = UCAPWFU(IR,IY) + UCAPWFN(IR,IY) + UCAPWFC(IR,IY)
         T59( 70,IR,IY,IS) = UCAPDBU(IR,IY) + UCAPDBN(IR,IY) + &
                             UCAPDPU(IR,IY) + UCAPDPN(IR,IY)


         T59(147,IR,IY,IS) = UCAPGNU(IR,IY) + UCAPGNN(IR,IY)
         T59(148,IR,IY,IS) = UCAPBIU(IR,IY) + UCAPBIN(IR,IY)
         T59(149,IR,IY,IS) = UCAPAGU(IR,IY) + UCAPAGN(IR,IY)
         T59(150,IR,IY,IS) = UCAPHOU(IR,IY) + UCAPHON(IR,IY)
         T59(151,IR,IY,IS) = UCAPHIU(IR,IY) + UCAPHIN(IR,IY)
         T59(152,IR,IY,IS) = UCAPTIU(IR,IY) + UCAPTIN(IR,IY)
         T59(153,IR,IY,IS) = UCAPQSU(IR,IY) + UCAPQSN(IR,IY)
         T59(154,IR,IY,IS) = UCAPDSU(IR,IY) + UCAPDSN(IR,IY)
         T59(155,IR,IY,IS) = UCAPZSU(IR,IY) + UCAPZSN(IR,IY)
         T59(156,IR,IY,IS) = UCAPWLU(IR,IY) + UCAPWLN(IR,IY) + UCAPWLC(IR,IY)
         T59(157,IR,IY,IS) = UCAPSSU(IR,IY) + UCAPSSN(IR,IY)
         T59(158,IR,IY,IS) = UCAPS2U(IR,IY) + UCAPS2N(IR,IY)
         T59(159,IR,IY,IS) = UCAPPTU(IR,IY) + UCAPPTN(IR,IY) + UCAPPTC(IR,IY)
         T59(160,IR,IY,IS) = UCAPINU(IR,IY) + UCAPINN(IR,IY)

         T59(15,IR,IY,IS) = T59(1,IR,IY,IS) + T59(4,IR,IY,IS) + T59(5,IR,IY,IS) + &
                            T59(70,IR,IY,IS) + FSUM(T59(9,IR,IY,IS),6) + FSUM(T59(112,IR,IY,IS),7) + &
                            T59(137,IR,IY,IS) + T59(144,IR,IY,IS) + T59(145,IR,IY,IS) + &
                            FSUM(T59(147,IR,IY,IS),14)
          
          T59(257,IR,IY,IS) = T59(159,IR,IY,IS) / 3.0
          T59(258,IR,IY,IS) = T59(257,IR,IY,IS) + T59(154,IR,IY,IS)
          
! --- CUMULATIVE PLANNED ADDITIONS
         T59(16,IR,IY,IS) = UADDCSU(1,IR,IY) + UADDCSN(1,IR,IY) + UADDCSC(IR,IY)
         T59(17,IR,IY,IS) = UADDIGU(1,IR,IY) + UADDIGN(1,IR,IY) + UADDIGC(IR,IY)
         T59(75,IR,IY,IS) = UADDISU(1,IR,IY) + UADDISN(1,IR,IY)
         T59(181,IR,IY,IS) = UADDPQU(1,IR,IY) + UADDPQN(1,IR,IY)
         T59(162,IR,IY,IS) = UADDOCU(1,IR,IY) + UADDOCN(1,IR,IY)
         T59(163,IR,IY,IS) = UADDI2U(1,IR,IY) + UADDI2N(1,IR,IY)
         T59(18,IR,IY,IS) = T59(16,IR,IY,IS) - T59(17,IR,IY,IS) - T59(75,IR,IY,IS) - T59(181,IR,IY,IS) -  &
                            T59(162,IR,IY,IS) - T59(163,IR,IY,IS)
         T59(19,IR,IY,IS) = UADDOSU(1,IR,IY) + UADDOSN(1,IR,IY) + UADDOSC(IR,IY)
         T59(20,IR,IY,IS) = UADDCCU(1,IR,IY) + UADDCCN(1,IR,IY) + UADDCCC(IR,IY)
         T59(21,IR,IY,IS) = UADDACU(1,IR,IY) + UADDACN(1,IR,IY) + UADDACC(IR,IY)
         T59(76,IR,IY,IS) = UADDASU(1,IR,IY) + UADDASN(1,IR,IY)
         T59(166,IR,IY,IS) = UADDA2U(1,IR,IY) + UADDA2N(1,IR,IY)
         T59(22,IR,IY,IS) = T59(20,IR,IY,IS) - T59(21,IR,IY,IS) - T59(76,IR,IY,IS) - T59(166,IR,IY,IS)
         T59(23,IR,IY,IS) = UADDCTU(1,IR,IY) + UADDCTN(1,IR,IY) + UADDCTC(IR,IY)
         T59(24,IR,IY,IS) = UADDATU(1,IR,IY) + UADDATN(1,IR,IY) + UADDATC(IR,IY)
         T59(25,IR,IY,IS) = T59(23,IR,IY,IS) - T59(24,IR,IY,IS)  - T59(164,IR,IY,IS) - T59(165,IR,IY,IS)
         T59(26,IR,IY,IS) = UADDNUU(1,IR,IY) + UADDNUN(1,IR,IY)
         T59(138,IR,IY,IS) = UADDSMU(1,IR,IY) + UADDSMN(1,IR,IY)
         T59(27,IR,IY,IS) = UADDPSU(1,IR,IY) + UADDPSN(1,IR,IY)
         T59(28,IR,IY,IS) = UADDFCU(1,IR,IY) + UADDFCN(1,IR,IY)
         T59(29,IR,IY,IS) = UADDRNU(1,IR,IY) + UADDRNN(1,IR,IY) + UADDRNC(IR,IY)

         T59(164,IR,IY,IS) = UADDICU(1,IR,IY) + UADDICN(1,IR,IY)
         T59(165,IR,IY,IS) = UADDT2U(1,IR,IY) + UADDT2N(1,IR,IY)
         T59(167,IR,IY,IS) = UADDGNU(1,IR,IY) + UADDGNN(1,IR,IY)
         T59(168,IR,IY,IS) = UADDBIU(1,IR,IY) + UADDBIN(1,IR,IY)
         T59(169,IR,IY,IS) = UADDAGU(1,IR,IY) + UADDAGN(1,IR,IY)
         T59(170,IR,IY,IS) = UADDHOU(1,IR,IY) + UADDHON(1,IR,IY)
         T59(171,IR,IY,IS) = UADDHIU(1,IR,IY) + UADDHIN(1,IR,IY)
         T59(172,IR,IY,IS) = UADDTIU(1,IR,IY) + UADDTIN(1,IR,IY)
         T59(173,IR,IY,IS) = UADDSQU(1,IR,IY) + UADDSQN(1,IR,IY)
         T59(174,IR,IY,IS) = UADDDSU(1,IR,IY) + UADDDSN(1,IR,IY)
         T59(175,IR,IY,IS) = UADDZSU(1,IR,IY) + UADDZSN(1,IR,IY)
         T59(176,IR,IY,IS) = UADDWLU(1,IR,IY) + UADDWLN(1,IR,IY)
         T59(177,IR,IY,IS) = UADDSSU(1,IR,IY) + UADDSSN(1,IR,IY)
         T59(178,IR,IY,IS) = UADDS2U(1,IR,IY) + UADDS2N(1,IR,IY)
         T59(179,IR,IY,IS) = UADDPTU(1,IR,IY) + UADDPTN(1,IR,IY)
         T59(180,IR,IY,IS) = UADDINU(1,IR,IY) + UADDINN(1,IR,IY)

         ! for now:
         T59(29,IR,IY,IS) = UADDRNU(1,IR,IY) + UADDRNN(1,IR,IY) + &
                            UADDHYC(IR,IY) + UADDGEC(IR,IY) + &
                            UADDMSC(IR,IY) + UADDWDC(IR,IY) + &
                            UADDSTC(IR,IY) + UADDPVC(IR,IY) + &
                            UADDWNC(IR,IY) + UADDWFC(IR,IY)
         T59( 29,IR,IY,IS) = UADDHYU(1,IR,IY) + UADDHYN(1,IR,IY) + UADDHYC(IR,IY)
         T59(119,IR,IY,IS) = UADDGEU(1,IR,IY) + UADDGEN(1,IR,IY) + UADDGEC(IR,IY)
         T59(120,IR,IY,IS) = UADDMSU(1,IR,IY) + UADDMSN(1,IR,IY) + UADDMSC(IR,IY)
         T59(121,IR,IY,IS) = UADDWDU(1,IR,IY) + UADDWDN(1,IR,IY) + UADDWDC(IR,IY)
         T59(122,IR,IY,IS) = UADDSTU(1,IR,IY) + UADDSTN(1,IR,IY) + UADDSTC(IR,IY)
         T59(123,IR,IY,IS) = UADDPVU(1,IR,IY) + UADDPVN(1,IR,IY) + UADDPVC(IR,IY)
         T59(124,IR,IY,IS) = UADDWNU(1,IR,IY) + UADDWNN(1,IR,IY) + UADDWNC(IR,IY)
         T59(125,IR,IY,IS) = UADDWFU(1,IR,IY) + UADDWFN(1,IR,IY) + UADDWFC(IR,IY)
         T59(71,IR,IY,IS) = UADDDBU(1,IR,IY) + UADDDBN(1,IR,IY) + &
                            UADDDPU(1,IR,IY) + UADDDPN(1,IR,IY)
         T59(30,IR,IY,IS) = UADDTLU(1,IR,IY) + UADDTLN(1,IR,IY) + UADDTLC(IR,IY)
         T59(30,IR,IY,IS) = FSUM(T59( 16,IR,IY,IS),14) + FSUM(T59(119,IR,IY,IS),7) + &
                            FSUM(T59(167,IR,IY,IS),14) + &
                            T59( 71,IR,IY,IS) + T59( 75,IR,IY,IS) + T59( 76,IR,IY,IS) + &
                            T59(162,IR,IY,IS) + T59(163,IR,IY,IS) + T59(181,IR,IY,IS) - &
                            T59( 16,IR,IY,IS) - T59( 20,IR,IY,IS) - T59( 23,IR,IY,IS) + &
                            T59(138,IR,IY,IS) + T59(164,IR,IY,IS) + T59(165,IR,IY,IS) + &
                            T59(166,IR,IY,IS)
! --- CUMULATIVE UNPLANNED ADDITIONS
         T59(31,IR,IY,IS) = UADDCSU(2,IR,IY) + UADDCSN(2,IR,IY)
         T59(32,IR,IY,IS) = UADDIGU(2,IR,IY) + UADDIGN(2,IR,IY)
         T59(77,IR,IY,IS) = UADDISU(2,IR,IY) + UADDISN(2,IR,IY)
         T59(182,IR,IY,IS) = UADDOCU(2,IR,IY) + UADDOCN(2,IR,IY)
         T59(183,IR,IY,IS) = UADDI2U(2,IR,IY) + UADDI2N(2,IR,IY)
         T59(201,IR,IY,IS) = UADDPQU(2,IR,IY) + UADDPQN(2,IR,IY)
         T59(33,IR,IY,IS) = T59( 31,IR,IY,IS) - T59( 32,IR,IY,IS) - T59( 77,IR,IY,IS) - &
                            T59(201,IR,IY,IS) - T59(182,IR,IY,IS) - T59(183,IR,IY,IS)
         T59(34,IR,IY,IS) = UADDOSU(2,IR,IY) + UADDOSN(2,IR,IY)
         T59(35,IR,IY,IS) = UADDCCU(2,IR,IY) + UADDCCN(2,IR,IY)
         T59(36,IR,IY,IS) = UADDACU(2,IR,IY) + UADDACN(2,IR,IY)
         T59(78,IR,IY,IS) = UADDASU(2,IR,IY) + UADDASN(2,IR,IY)
         T59(186,IR,IY,IS) = UADDA2U(2,IR,IY) + UADDA2N(2,IR,IY)
         T59(37,IR,IY,IS) = T59(35,IR,IY,IS) - T59(36,IR,IY,IS) - T59(78,IR,IY,IS)- T59(186,IR,IY,IS)
         T59(38,IR,IY,IS) = UADDCTU(2,IR,IY) + UADDCTN(2,IR,IY)
         T59(39,IR,IY,IS) = UADDATU(2,IR,IY) + UADDATN(2,IR,IY)
         T59(184,IR,IY,IS) = UADDICU(2,IR,IY) + UADDICN(2,IR,IY)
         T59(185,IR,IY,IS) = UADDT2U(2,IR,IY) + UADDT2N(2,IR,IY)
         T59(40,IR,IY,IS) = T59(38,IR,IY,IS) - T59(39,IR,IY,IS)- T59(184,IR,IY,IS) - T59(185,IR,IY,IS)
         T59(41,IR,IY,IS) = UADDNUU(2,IR,IY) + UADDNUN(2,IR,IY)
         T59(139,IR,IY,IS) = UADDSMU(2,IR,IY) + UADDSMN(2,IR,IY)
         T59(42,IR,IY,IS) = UADDPSU(2,IR,IY) + UADDPSN(2,IR,IY)
         T59(43,IR,IY,IS) = UADDFCU(2,IR,IY) + UADDFCN(2,IR,IY)
         T59(44,IR,IY,IS) = UADDHYU(2,IR,IY) + UADDHYN(2,IR,IY)
         T59(126,IR,IY,IS) = UADDGEU(2,IR,IY) + UADDGEN(2,IR,IY)
         T59(127,IR,IY,IS) = UADDMSU(2,IR,IY) + UADDMSN(2,IR,IY)
         T59(128,IR,IY,IS) = UADDWDU(2,IR,IY) + UADDWDN(2,IR,IY)
         T59(129,IR,IY,IS) = UADDSTU(2,IR,IY) + UADDSTN(2,IR,IY)
         T59(130,IR,IY,IS) = UADDPVU(2,IR,IY) + UADDPVN(2,IR,IY)
         T59(131,IR,IY,IS) = UADDWNU(2,IR,IY) + UADDWNN(2,IR,IY)
         T59(132,IR,IY,IS) = UADDWFU(2,IR,IY) + UADDWFN(2,IR,IY)
         T59(72,IR,IY,IS) = UADDDBU(2,IR,IY) + UADDDBN(2,IR,IY) + &
                            UADDDPU(2,IR,IY) + UADDDPN(2,IR,IY)
         T59(187,IR,IY,IS) = UADDGNU(2,IR,IY) + UADDGNN(2,IR,IY)
         T59(188,IR,IY,IS) = UADDBIU(2,IR,IY) + UADDBIN(2,IR,IY)
         T59(189,IR,IY,IS) = UADDAGU(2,IR,IY) + UADDAGN(2,IR,IY)
         T59(190,IR,IY,IS) = UADDHOU(2,IR,IY) + UADDHON(2,IR,IY)
         T59(191,IR,IY,IS) = UADDHIU(2,IR,IY) + UADDHIN(2,IR,IY)
         T59(192,IR,IY,IS) = UADDTIU(2,IR,IY) + UADDTIN(2,IR,IY)
         T59(193,IR,IY,IS) = UADDSQU(2,IR,IY) + UADDSQN(2,IR,IY)
         T59(194,IR,IY,IS) = UADDDSU(2,IR,IY) + UADDDSN(2,IR,IY)
         T59(195,IR,IY,IS) = UADDZSU(2,IR,IY) + UADDZSN(2,IR,IY)
         T59(196,IR,IY,IS) = UADDWLU(2,IR,IY) + UADDWLN(2,IR,IY)
         T59(197,IR,IY,IS) = UADDSSU(2,IR,IY) + UADDSSN(2,IR,IY)
         T59(198,IR,IY,IS) = UADDS2U(2,IR,IY) + UADDS2N(2,IR,IY)
         T59(199,IR,IY,IS) = UADDPTU(2,IR,IY) + UADDPTN(2,IR,IY)
         T59(200,IR,IY,IS) = UADDINU(2,IR,IY) + UADDINN(2,IR,IY)

         T59(45,IR,IY,IS) = FSUM(T59(31,IR,IY,IS),14) + FSUM(T59(126,IR,IY,IS),7) + &
                            T59( 72,IR,IY,IS) + T59( 77,IR,IY,IS) + T59( 78,IR,IY,IS) + &
                            T59(201,IR,IY,IS) + T59(182,IR,IY,IS) + T59(183,IR,IY,IS) + &
                            T59(186,IR,IY,IS) - &
                            T59( 31,IR,IY,IS) - T59( 35,IR,IY,IS) - T59( 38,IR,IY,IS) + &
                            T59(139,IR,IY,IS) + T59(184,IR,IY,IS) + T59(185,IR,IY,IS) + &
                            FSUM(T59(187,IR,IY,IS),14)
!        --- Totals
         T59(46,IR,IY,IS) = UADDTLU(1,IR,IY) + UADDTLU(2,IR,IY) + &
                            UADDTLN(1,IR,IY) + UADDTLN(2,IR,IY) + UADDTLC(IR,IY)
         T59(46,IR,IY,IS) = T59(30,IR,IY,IS) + T59(45,IR,IY,IS)
! --- Cumulative Retirements
         T59(47,IR,IY,IS) = URETCSU(IR,IY)
         T59(48,IR,IY,IS) = URETIGU(IR,IY) + URETISU(IR,IY)
         T59(202,IR,IY,IS) = URETOCU(IR,IY)
         T59(203,IR,IY,IS) = URETI2U(IR,IY)
         T59(221,IR,IY,IS) = URETPQU(IR,IY)
         T59(49,IR,IY,IS) = T59( 47,IR,IY,IS) - T59( 48,IR,IY,IS) - T59(221,IR,IY,IS) - &
                            T59(202,IR,IY,IS) - T59(203,IR,IY,IS)
         T59(50,IR,IY,IS) = URETOSU(IR,IY)
         T59(51,IR,IY,IS) = URETCCU(IR,IY)
         T59(52,IR,IY,IS) = URETACU(IR,IY) + URETASU(IR,IY)
         T59(206,IR,IY,IS) = URETA2U(IR,IY)
         T59(53,IR,IY,IS) = T59(51,IR,IY,IS) - T59(52,IR,IY,IS)- T59(206,IR,IY,IS)
         T59(54,IR,IY,IS) = URETCTU(IR,IY)
         T59(55,IR,IY,IS) = URETATU(IR,IY)
         T59(204,IR,IY,IS) = URETICU(IR,IY)
         T59(205,IR,IY,IS) = URETT2U(IR,IY)
         T59(56,IR,IY,IS) = T59(54,IR,IY,IS) - T59(55,IR,IY,IS)- T59(204,IR,IY,IS) - T59(205,IR,IY,IS)
         T59(57,IR,IY,IS) = URETNUU(IR,IY)
         T59(140,IR,IY,IS) = URETSMU(IR,IY)
         T59(58,IR,IY,IS) = URETPSU(IR,IY)
         T59(59,IR,IY,IS) = URETFCU(IR,IY)
         T59(60,IR,IY,IS) = URETRNU(IR,IY)
         T59(62,IR,IY,IS) = URETHYU(IR,IY)
         T59(63,IR,IY,IS) = URETGEU(IR,IY)
         T59(64,IR,IY,IS) = URETMSU(IR,IY)
         T59(65,IR,IY,IS) = URETWDU(IR,IY)
         T59(66,IR,IY,IS) = URETSTU(IR,IY)
         T59(67,IR,IY,IS) = URETPVU(IR,IY)
         T59(68,IR,IY,IS) = URETWNU(IR,IY)
         T59(69,IR,IY,IS) = URETWFU(IR,IY)

         T59(207,IR,IY,IS) = URETGNU(IR,IY)
         T59(208,IR,IY,IS) = URETBIU(IR,IY)
         T59(209,IR,IY,IS) = URETAGU(IR,IY)
         T59(210,IR,IY,IS) = URETHOU(IR,IY)
         T59(211,IR,IY,IS) = URETHIU(IR,IY)
         T59(212,IR,IY,IS) = URETTIU(IR,IY)
         T59(213,IR,IY,IS) = URETSQU(IR,IY)
         T59(214,IR,IY,IS) = URETDSU(IR,IY)
         T59(215,IR,IY,IS) = URETZSU(IR,IY)
         T59(216,IR,IY,IS) = URETWLU(IR,IY)
         T59(217,IR,IY,IS) = URETSSU(IR,IY)
         T59(218,IR,IY,IS) = URETS2U(IR,IY)
         T59(219,IR,IY,IS) = URETPTU(IR,IY)
         T59(220,IR,IY,IS) = URETINU(IR,IY)

         T59(61,IR,IY,IS) = T59( 47,IR,IY,IS) + T59( 50,IR,IY,IS) + T59( 51,IR,IY,IS) + &
                            T59(140,IR,IY,IS) + T59(204,IR,IY,IS) + T59(205,IR,IY,IS) + &
                            FSUM(T59(55,IR,IY,IS),5) + FSUM(T59(62,IR,IY,IS),8) + &
                            FSUM(T59(207,IR,IY,IS),14)
!    Generation by Plant Type
         T59( 80,IR,IY,IS) = UGENPC(IR,IY) + UGENIG(IR,IY) + UGENIS(IR,IY) + UGENPQ(IR,IY) + &
                             UGENOC(IR,IY) + UGENI2(IR,IY)
         T59( 81,IR,IY,IS) = UGENIG(IR,IY)
         T59( 82,IR,IY,IS) = UGENIS(IR,IY)
         T59( 83,IR,IY,IS) = UGENPC(IR,IY)
         T59( 84,IR,IY,IS) = UGENOS(IR,IY)
         T59( 85,IR,IY,IS) = UGENCC(IR,IY) +  UGENAC(IR,IY) + UGENCS(IR,IY)  + UGENA2(IR,IY)
         T59( 86,IR,IY,IS) = UGENAC(IR,IY)
         T59( 87,IR,IY,IS) = UGENCS(IR,IY)
         T59( 88,IR,IY,IS) = UGENCC(IR,IY)
         T59( 89,IR,IY,IS) = UGENCT(IR,IY) +  UGENAT(IR,IY) + UGENIC(IR,IY) + UGENT2(IR,IY)
         T59( 90,IR,IY,IS) = UGENAT(IR,IY)
         T59( 91,IR,IY,IS) = UGENCT(IR,IY)
         T59( 92,IR,IY,IS) = UGENNU(IR,IY)
         T59(141,IR,IY,IS) = UGENSM(IR,IY)
         T59( 93,IR,IY,IS) = UGENPS(IR,IY)
         T59( 94,IR,IY,IS) = UGENFC(IR,IY)
         T59( 96,IR,IY,IS) = UGENDG(IR,IY)

         T59( 95,IR,IY,IS) = UGNHYNR(1,IR,IY) + UGNHYNR(2,IR,IY) + &
                            (CGNTGEN(IR,IY, 4,1) + CGNTGEN(IR,IY, 4,2)) * 0.001
         T59(222,IR,IY,IS) = UGENOC(IR,IY)
         T59(223,IR,IY,IS) = UGENI2(IR,IY)
         T59(224,IR,IY,IS) = UGENIC(IR,IY)
         T59(225,IR,IY,IS) = UGENT2(IR,IY)
         T59(226,IR,IY,IS) = UGENA2(IR,IY)
         T59(227,IR,IY,IS) = UGENGN(IR,IY)
         T59(228,IR,IY,IS) = UGENBI(IR,IY)
         T59(229,IR,IY,IS) = UGENAG(IR,IY)
         T59(230,IR,IY,IS) = UGENHO(IR,IY)
         T59(231,IR,IY,IS) = UGENHI(IR,IY)
         T59(232,IR,IY,IS) = UGENTI(IR,IY)
         T59(233,IR,IY,IS) = UGENQS(IR,IY)
         T59(234,IR,IY,IS) = UGENDS(IR,IY)
         T59(235,IR,IY,IS) = UGENZS(IR,IY)
         T59(236,IR,IY,IS) = UGNWLNR(1,IR,IY) + UGNWLNR(2,IR,IY)
         T59(237,IR,IY,IS) = UGENSS(IR,IY)
         T59(238,IR,IY,IS) = UGENS2(IR,IY)
         T59(239,IR,IY,IS) = UGENPT(IR,IY)
         T59(240,IR,IY,IS) = UGENIN(IR,IY)
         T59(241,IR,IY,IS) = UGENPQ(IR,IY)
         T59(242,IR,IY,IS) = 0.0
         T59(243,IR,IY,IS) = 0.0
         T59(244,IR,IY,IS) = 0.0
         T59(245,IR,IY,IS) = 0.0
         T59(246,IR,IY,IS) = 0.0
         T59(247,IR,IY,IS) = 0.0
         T59(248,IR,IY,IS) = 0.0
         T59(249,IR,IY,IS) = 0.0
         T59(250,IR,IY,IS) = UGNGENR(1,IR,IY) + UGNGENR(2,IR,IY) + &
                            (CGNTGEN(IR,IY, 5,1) + CGNTGEN(IR,IY, 5,2)) * 0.001
         T59(251,IR,IY,IS) = UGNMSNR(1,IR,IY) + UGNMSNR(2,IR,IY) + &
                            (CGNTGEN(IR,IY, 6,1) + CGNTGEN(IR,IY, 6,2)) * 0.001
         T59(252,IR,IY,IS) = UGNWDNR(1,IR,IY) + UGNWDNR(2,IR,IY) + &
                            (CGNTGEN(IR,IY, 7,1) + CGNTGEN(IR,IY, 7,2)) * 0.001 - &
                             UGNCFNR(1,IR,IY) - UGNCFNR(2,IR,IY)
         T59(253,IR,IY,IS) = UGNSONR(1,IR,IY) + UGNSONR(2,IR,IY)    ! UGNSONR is solar thermal only
         T59(254,IR,IY,IS) = UGNPVNR(1,IR,IY) + UGNPVNR(2,IR,IY) + &
                            (CGNTGEN(IR,IY, 8,1) + CGNTGEN(IR,IY, 8,2)) * 0.001
         T59(239,IR,IY,IS) = UGNPTNR(1,IR,IY) + UGNPTNR(2,IR,IY)
         T59(255,IR,IY,IS) = UGNWNNR(1,IR,IY) + UGNWNNR(2,IR,IY)
         T59(256,IR,IY,IS) = UGNWFNR(1,IR,IY) + UGNWFNR(2,IR,IY)

         T59(97,IR,IY,IS) = T59( 80,IR,IY,IS) + T59( 84,IR,IY,IS) + T59( 85,IR,IY,IS) + &
                            T59( 89,IR,IY,IS) + T59(141,IR,IY,IS) + &
                            FSUM(T59(92,IR,IY,IS),5) + FSUM(T59(227,IR,IY,IS),14) + &
                            FSUM(T59(242,IR,IY,IS),15)
!    Sequestration Capacity and Generation
           T59(98,IR,IY,IS) = UCAPISU(IR,IY) + UCAPISN(IR,IY) + UCAPSQU(IR,IY) + UCAPSQN(IR,IY) + UCAPPQU(IR,IY) + UCAPPQN(IR,IY)
           T59(135,IR,IY,IS) = UCAPPQU(IR,IY) + UCAPPQN(IR,IY)
           T59( 99,IR,IY,IS) = UCAPISU(IR,IY) + UCAPISN(IR,IY)
           T59(100,IR,IY,IS) = UCAPSQU(IR,IY) + UCAPSQN(IR,IY)
           T59(101,IR,IY,IS) = UCAPASU(IR,IY) + UCAPASN(IR,IY) + UCAPA2U(IR,IY) + UCAPA2N(IR,IY)   ! rows 101 and 102
           T59(102,IR,IY,IS) = UCAPASU(IR,IY) + UCAPASN(IR,IY)
           T59(103,IR,IY,IS) = UCAPA2U(IR,IY) + UCAPA2N(IR,IY)
           T59(104,IR,IY,IS) = T59(98,IR,IY,IS) + T59(101,IR,IY,IS)

           T59(105,IR,IY,IS) = UGENIS(IR,IY) + UGENSQ(IR,IY) + UGENPQ(IR,IY) - (UGENIS_ALT(IR,IY) + UGENSQ_ALT(IR,IY) + UGENPQ_ALT(IR,IY))
           T59(136,IR,IY,IS) = UGENPQ(IR,IY) - UGENPQ_ALT(IR,IY)
           T59(106,IR,IY,IS) = UGENIS(IR,IY) - UGENIS_ALT(IR,IY)
           T59(107,IR,IY,IS) = UGENSQ(IR,IY) - UGENSQ_ALT(IR,IY)
           T59(108,IR,IY,IS) = UGENCS(IR,IY) - UGENCS_ALT(IR,IY) + UGENA2(IR,IY) - UGENA2_ALT(IR,IY)   ! T59(109) + T59(110)
           T59(109,IR,IY,IS) = UGENCS(IR,IY) - UGENCS_ALT(IR,IY)
           T59(110,IR,IY,IS) = UGENA2(IR,IY) - UGENA2_ALT(IR,IY)
           T59(111,IR,IY,IS) = T59(105,IR,IY,IS) + T59(108,IR,IY,IS)
        ENDDO
  590 CONTINUE

      RETURN

      END SUBROUTINE FDATA1A

      SUBROUTINE FDATA2(IS)
      IMPLICIT NONE
      include 'parametr'
      include 'emmparm'
      include 'qblk'
      include 'ampblk'          !  use adjusted prices
      include 'macout'
      include 'pmmout'
      include 'pmmrpt'
      include 'pmmftab'
      include 'ogsmout'
      include 'convfact'
      include 'ngtdmrep'
      include 'ngrpt'
      include 'angtdm'
      include 'ncntrl'
      include 'emission'
      include 'emablk'
      include 'ghgrep'
      include 'uefpout'
      include 'uefdout'
      include 'udatout'
      include 'uecpout'
      include 'uettout'
      include 'efpout'
      include 'comparm'
      include 'comout'
      include 'commrep'
      include 'cogen'
      include 'cdsparms'
      include 'coalout'
      include 'coalemm'
      include 'coalrep'
      include 'indrep'
      include 'intout'
      include 'resdrep'
      include 'tranrep'
      include 'wrenew'
      include 'ftable'
      include 'aponroad'
      include 'qonroad'
      include 'lfmmout'

      INTEGER*4 III,II,PADD       !COUNTERS
      REAL*4 FSUM,FFF,RRR,CUMRET,so2coal,CommCoal,CommGas,CommOil,CommRenew,CommOther,RDAYS,EUPVCAPADJ(MNUMYR),EUPVGENADJ(MNUMYR)
      REAL CTLSO2,CTLNOX,CTLHG
      EXTERNAL FSUM
      INTEGER IR,IS,IY,COUNT,NUMREP,LOOP1,ISO2,ICY,MAPERG(MNUMNR),IR2,IR3,IX
      INTEGER*4      CD2PADD(11)/1,1,2,2,1,3,3,4,5,6,6/       !use cd proxy for padd

      DATA MAPERG/2,8,4,5,7,6,6,6,3,4,1,9,9,9,9,9,10,10,12,13,11,12,0,0,0,0,0,0/

      EUPVCAPADJ = 1.0
      EUPVGENADJ = 1.0
      DO IY = (YEARPR + 1) - 1989, LASTYR
         IF (CGTOTCAPNR(MNUMNR,IY,10) .GT. 0.0) THEN
         EUPVCAPADJ(IY) = (CGCOMMCAP(11,IY,8) + CGRESCAP(11,IY,8) + CGINDLCAP(11,IY,8)) / &
                          CGTOTCAPNR(MNUMNR,IY,10)                                             !calculate adj for end use PV to match P1 results
         ENDIF
         IF (CGTOTGENNR(MNUMNR,IY,10,1) + CGTOTGENNR(MNUMNR,IY,10,2) .GT. 0.0) THEN
         EUPVGENADJ(IY) = (CGCOMMGEN(11,IY,8,1) + CGRESGEN(11,IY,8,1) + CGINDLGEN(11,IY,8,1) &
                     + CGCOMMGEN(11,IY,8,2) + CGRESGEN(11,IY,8,2) + CGINDLGEN(11,IY,8,2) ) / &
                     (CGTOTGENNR(MNUMNR,IY,10,1) + CGTOTGENNR(MNUMNR,IY,10,2))
         ENDIF
      ENDDO
      
!     Table 60. Technology Penetration in Light-Duty Vehicles

      DO IY = 1,LASTYR
         T60(1:88,IY,IS) = MKT_D_P(1,1:88,IY+BASEYR-1)      ! cars
         T60(89:176,IY,IS) = MKT_D_P(2,1:88,IY+BASEYR-1)    ! light trucks
         T60(177:264,IY,IS) = MKT_D_P(3,1:88,IY+BASEYR-1)   ! total
      ENDDO   

!     Table 61. Utility End-Use Service Prices

      DO IY = 1,LASTYR
        DO IR=1,MNUMNR
!         --- RESIDENTIAL
          T61( 1,IR,IY,IS) = PECRSRLN(IR,IY)
          T61( 2,IR,IY,IS) = PECRSMEN(IR,IY)
          T61( 3,IR,IY,IS) = PECRSTXN(IR,IY)
          T61( 4,IR,IY,IS) = PECRSTDN(IR,IY)
          T61( 5,IR,IY,IS) = FSUM(T61( 1,IR,IY,IS),4)
!         --- COMMERCIAL
          T61( 6,IR,IY,IS) = PECCMRLN(IR,IY)
          T61( 7,IR,IY,IS) = PECCMMEN(IR,IY)
          T61( 8,IR,IY,IS) = PECCMTXN(IR,IY)
          T61( 9,IR,IY,IS) = PECCMTDN(IR,IY)
          T61(10,IR,IY,IS) = FSUM(T61( 6,IR,IY,IS),4)
!         --- RESIDENTIAL
          T61(11,IR,IY,IS) = PECINRLN(IR,IY)
          T61(12,IR,IY,IS) = PECINMEN(IR,IY)
          T61(13,IR,IY,IS) = PECINTXN(IR,IY)
          T61(14,IR,IY,IS) = PECINTDN(IR,IY)
          T61(15,IR,IY,IS) = FSUM(T61(11,IR,IY,IS),4)
!         --- All Sectors
          T61(16,IR,IY,IS) = PECASRLN(IR,IY)
          T61(17,IR,IY,IS) = PECASMEN(IR,IY)
          T61(18,IR,IY,IS) = PECASTXN(IR,IY)
          T61(19,IR,IY,IS) = PECASTDN(IR,IY)
          T61(20,IR,IY,IS) = FSUM(T61(16,IR,IY,IS),4)
         ENDDO
      ENDDO

!     Table 62 Electric Power and Projections for NERC region

      DO 620 IY=1,LASTYR
!     Sum up emissions from CTL
           CTLSO2 = 0.0
           CTLNOX = 0.0
           CTLHG  = 0.0
        DO IR = 1 , NDREG
           CTLSO2 = CTLSO2 + CTLSO2EM(IR,IY) * 0.000001
           CTLNOX = CTLNOX + CTLNOXEM(IR,IY) * 0.000001
           CTLHG  = CTLHG  + CTLHGEM(IR,IY)
        END DO
        DO 620 IR=1,MNUMNR
         ICY=IY+BASEYR-1
         T62( 1,IR,IY,IS) = UCAPCSU(IR,IY) + UCAPCSN(IR,IY) + UCAPCSC(IR,IY)
         T62( 2,IR,IY,IS) = UCAPOSU(IR,IY) + UCAPOSN(IR,IY) + UCAPOSC(IR,IY) + &
                            UCAPNGU(IR,IY) + UCAPNGN(IR,IY)
         T62( 3,IR,IY,IS) = UCAPCCU(IR,IY) + UCAPCCN(IR,IY) + UCAPCCC(IR,IY)
         T62( 4,IR,IY,IS) = UCAPCTU(IR,IY) + UCAPCTN(IR,IY) + UCAPCTC(IR,IY)
         T62( 5,IR,IY,IS) = UCAPNUU(IR,IY) + UCAPNUN(IR,IY) + UCAPSMU(IR,IY) + UCAPSMN(IR,IY)
         T62( 6,IR,IY,IS) = UCAPPSU(IR,IY) + UCAPPSN(IR,IY)
         T62(124,IR,IY,IS) = UCAPDSU(IR,IY) + UCAPDSN(IR,IY)
         T62( 7,IR,IY,IS) = UCAPFCU(IR,IY) + UCAPFCN(IR,IY)
         T62( 8,IR,IY,IS) = UCAPHYU(IR,IY) + UCAPGEU(IR,IY) + &
                            UCAPMSU(IR,IY) + UCAPWDU(IR,IY) + &
                            UCAPSTU(IR,IY) + UCAPPVU(IR,IY) + &
                            UCAPWNU(IR,IY) + UCAPWFU(IR,IY) + &
                            UCAPWLU(IR,IY) + UCAPPTU(IR,IY) + &
                            UCAPHYN(IR,IY) + UCAPGEN(IR,IY) + &
                            UCAPMSN(IR,IY) + UCAPWDN(IR,IY) + &
                            UCAPSTN(IR,IY) + UCAPPVN(IR,IY) + &
                            UCAPWNN(IR,IY) + UCAPWFN(IR,IY) + &
                            UCAPWLN(IR,IY) + UCAPPTN(IR,IY) + &
                            UCAPHYC(IR,IY) + UCAPGEC(IR,IY) + &
                            UCAPMSC(IR,IY) + UCAPWDC(IR,IY) + &
                            UCAPSTC(IR,IY) + UCAPPVC(IR,IY) + &
                            UCAPWNC(IR,IY) + UCAPWFC(IR,IY) + &
                            UCAPWLC(IR,IY) + UCAPPTC(IR,IY)
         T62(96,IR,IY,IS) = UCAPDBU(IR,IY) + UCAPDBN(IR,IY) + &
                            UCAPDPU(IR,IY) + UCAPDPN(IR,IY)
         T62( 9,IR,IY,IS) = UCAPTLU(IR,IY) + UCAPTLN(IR,IY) + UCAPTLC(IR,IY)
         T62( 9,IR,IY,IS) = FSUM(T62( 1,IR,IY,IS),8) + T62(96,IR,IY,IS) + T62(124,IR,IY,IS)
!        ---- CAPACITY PHYSICAL LOCATED
         T62(10,IR,IY,IS) = UCAPINR(IR,IY) - UCAPSRV(IR,IY) + UCAPPSU(IR,IY) + &
                            UCAPHYU(IR,IY) + UCAPGEU(IR,IY) + &
                            UCAPMSU(IR,IY) + UCAPWDU(IR,IY) + &
                            UCAPSTU(IR,IY) + UCAPPVU(IR,IY) + UCAPWNU(IR,IY)
         T62(11,IR,IY,IS) = UCAPOTR(IR,IY)
!        --- CUMULATIVE PLANNED ADDITIONS
         T62(12,IR,IY,IS) = UADDCSU(1,IR,IY) + UADDCSN(1,IR,IY) + UADDCSC(IR,IY)
         T62(13,IR,IY,IS) = UADDOSU(1,IR,IY) + UADDOSN(1,IR,IY) + UADDOSC(IR,IY)
         T62(14,IR,IY,IS) = UADDCCU(1,IR,IY) + UADDCCN(1,IR,IY) + UADDCCC(IR,IY)
         T62(15,IR,IY,IS) = UADDCTU(1,IR,IY) + UADDCTN(1,IR,IY) + UADDCTC(IR,IY)
         T62(16,IR,IY,IS) = UADDNUU(1,IR,IY) + UADDNUN(1,IR,IY) + UADDSMU(1,IR,IY) + UADDSMN(1,IR,IY)
         T62(17,IR,IY,IS) = UADDPSU(1,IR,IY) + UADDPSN(1,IR,IY)
         T62(125,IR,IY,IS) = UADDDSU(1,IR,IY) + UADDDSN(1,IR,IY)
         T62(18,IR,IY,IS) = UADDFCU(1,IR,IY) + UADDFCN(1,IR,IY)
         T62(19,IR,IY,IS) = UADDRNU(1,IR,IY) + UADDRNN(1,IR,IY) + &
                            UADDHYC(IR,IY) + UADDGEC(IR,IY) + &
                            UADDMSC(IR,IY) + UADDWDC(IR,IY) + &
                            UADDSTC(IR,IY) + UADDPVC(IR,IY) + &
                            UADDWNC(IR,IY) + UADDWFC(IR,IY)
         T62(97,IR,IY,IS) = UADDDBU(1,IR,IY) + UADDDBN(1,IR,IY) + &
                            UADDDPU(1,IR,IY) + UADDDPN(1,IR,IY)
         T62(20,IR,IY,IS) = UADDTLU(1,IR,IY) + UADDTLN(1,IR,IY) + UADDTLC(IR,IY)
!        ---- CAPACITY PHYSICAL LOCATED
         T62(21,IR,IY,IS) = UADDINR(1,IR,IY) - UADDSRV(1,IR,IY) + &
                            UADDRNU(1,IR,IY) + UADDPSU(1,IR,IY)
         T62(22,IR,IY,IS) = UADDOTR(1,IR,IY)
!        --- CUMULATIVE UNPLANNED ADDITIONS
         T62(23,IR,IY,IS) = UADDCSU(2,IR,IY) + UADDCSN(2,IR,IY)
         T62(24,IR,IY,IS) = UADDOSU(2,IR,IY) + UADDOSN(2,IR,IY)
         T62(25,IR,IY,IS) = UADDCCU(2,IR,IY) + UADDCCN(2,IR,IY)
         T62(26,IR,IY,IS) = UADDCTU(2,IR,IY) + UADDCTN(2,IR,IY)
         T62(27,IR,IY,IS) = UADDNUU(2,IR,IY) + UADDNUN(2,IR,IY) +  UADDSMU(2,IR,IY) + UADDSMN(2,IR,IY)
         T62(28,IR,IY,IS) = UADDPSU(2,IR,IY) + UADDPSN(2,IR,IY)
         T62(126,IR,IY,IS) = UADDDSU(2,IR,IY) + UADDDSN(2,IR,IY)
         T62(29,IR,IY,IS) = UADDFCU(2,IR,IY) + UADDFCN(2,IR,IY)
         T62(30,IR,IY,IS) = UADDRNU(2,IR,IY) + UADDRNN(2,IR,IY)
         T62(31,IR,IY,IS) = UADDTLU(2,IR,IY) + UADDTLN(2,IR,IY)
         T62(98,IR,IY,IS) = UADDDBU(2,IR,IY) + UADDDBN(2,IR,IY) + &
                            UADDDPU(2,IR,IY) + UADDDPN(2,IR,IY)
!        ---- CAPACITY PHYSICAL LOCATED
         T62(32,IR,IY,IS) = UADDINR(2,IR,IY) - UADDSRV(2,IR,IY) + &
                            UADDRNU(2,IR,IY) + UADDPSU(2,IR,IY)
         T62(33,IR,IY,IS) = UADDOTR(2,IR,IY)
         T62(34,IR,IY,IS) = UADDTLU(1,IR,IY) + UADDTLU(2,IR,IY) + UADDTLC(IR,IY) + &
                            UADDTLN(1,IR,IY) + UADDTLN(2,IR,IY)
         T62(35,IR,IY,IS) = URETTLU(IR,IY)
!        --- CUMULATIVE RETIREMENTS
         T62(102,IR,IY,IS) = URETCSU(IR,IY)
         T62(103,IR,IY,IS) = URETOSU(IR,IY)
         T62(104,IR,IY,IS) = URETCCU(IR,IY)
         T62(105,IR,IY,IS) = URETCTU(IR,IY)
         T62(106,IR,IY,IS) = URETNUU(IR,IY) + URETSMU(IR,IY)
         T62(107,IR,IY,IS) = URETPSU(IR,IY)
         T62(127,IR,IY,IS) = URETDSU(IR,IY)
         T62(108,IR,IY,IS) = URETFCU(IR,IY)
         T62(109,IR,IY,IS) = URETRNU(IR,IY)
         T62(110,IR,IY,IS) = URETTLU(IR,IY)
if (ftabbone.eq.0) then           !  only do this if tables are elevated to publish level (no bonus rows)
         IF (IY .LE. (CUMCAPADD - 1989)) THEN
            T62(12:35,IR,IY,IS) = -999.
            T62(97,IR,IY,IS) = -999.
            T62(98,IR,IY,IS) = -999.
            T62(102:110,IR,IY,IS) = -999.
            T62(125:127,IR,IY,IS) = -999.
         ENDIF
endif
! --- Combined heat and power
         T62(36,IR,IY,IS) = CGTOTCAPNR(IR,IY,1) * 0.001
         T62(37,IR,IY,IS) = CGTOTCAPNR(IR,IY,2) * 0.001
         T62(38,IR,IY,IS) = CGTOTCAPNR(IR,IY,4) * 0.001
         T62(39,IR,IY,IS) = CGTOTCAPNR(IR,IY,3) * 0.001
         T62(40,IR,IY,IS) = (CGTOTCAPNR(IR,IY,5) + CGTOTCAPNR(IR,IY,6) +   &
                             CGTOTCAPNR(IR,IY,7) + CGTOTCAPNR(IR,IY,8) +   &
                             CGTOTCAPNR(IR,IY,9) + (CGTOTCAPNR(IR,IY,10) * EUPVCAPADJ(IY)) + &   !adj PV
                             CGTOTCAPNR(IR,IY,12)) * 0.001
         T62(41,IR,IY,IS) = CGTOTCAPNR(IR,IY,11) * 0.001
         T62(42,IR,IY,IS) = FSUM(T62(36,IR,IY,IS),6)
!        --- NET ENERGY FOR LOAD
         T62(48,IR,IY,IS) = (UTIMPF(IR,IY) + UTIMPE(IR,IY)) * .001
         T62(49,IR,IY,IS) = (UTEXPF(IR,IY) + UTEXPE(IR,IY)) * .001
         T62(50,IR,IY,IS) = (UTDMMF(IR,IY) + UTDMME(IR,IY) + &
                             UTEXMF(IR,IY) + UTEXME(IR,IY)) * .001
         T62(51,IR,IY,IS) = (UTEXMF(IR,IY) + UTEXME(IR,IY)) * .001
!        --- GENERATION BY FUEL TYPE (INCLUDE NONTRADITIONAL COGEN FOR AEO03)
         T62(55,IR,IY,IS) = UGNCLNR(1,IR,IY) + UGNCLNR(2,IR,IY) + &
                           (CGNTGEN(IR,IY, 1,1) + CGNTGEN(IR,IY, 1,2)) * 0.001
         T62(56,IR,IY,IS) = UGNDSNR(1,IR,IY) + UGNRLNR(1,IR,IY) + UGNRHNR(1,IR,IY) + &
                            UGNDSNR(2,IR,IY) + UGNRLNR(2,IR,IY) + UGNRHNR(2,IR,IY) + &
                           (CGNTGEN(IR,IY, 2,1) + CGNTGEN(IR,IY, 2,2)) * 0.001
         T62(57,IR,IY,IS) = UGNGFNR(1,IR,IY) + UGNGINR(1,IR,IY) + UGNGCNR(1,IR,IY) + &
                            UGNGFNR(2,IR,IY) + UGNGINR(2,IR,IY) + UGNGCNR(2,IR,IY) + &
                           (CGNTGEN(IR,IY, 3,1) + CGNTGEN(IR,IY, 3,2)) * 0.001
         T62(58,IR,IY,IS) = UGNURNR(1,IR,IY) + UGNURNR(2,IR,IY)
         T62(59,IR,IY,IS) = UGNPSNR(1,IR,IY) + UGNPSNR(2,IR,IY)  + &
                            UGNSDNR(1,IR,IY) + UGNSDNR(2,IR,IY)  + &
                      (CGNTGEN(IR,IY, 9,1) + CGNTGEN(IR,IY, 9,2) + &
                       CGNTGEN(IR,IY,10,1) + CGNTGEN(IR,IY,10,2)) * 0.001
         T62(60,IR,IY,IS) = UGNHYNR(1,IR,IY) + UGNHYNR(2,IR,IY) + &
                            UGNGENR(1,IR,IY) + UGNGENR(2,IR,IY) + &
                            UGNMSNR(1,IR,IY) + UGNMSNR(2,IR,IY) + &
                            UGNWDNR(1,IR,IY) + UGNWDNR(2,IR,IY) + &
                            UGNSONR(1,IR,IY) + UGNSONR(2,IR,IY) + &
                            UGNPVNR(1,IR,IY) + UGNPVNR(2,IR,IY) + &
                            UGNPTNR(1,IR,IY) + UGNPTNR(2,IR,IY) + &
                            UGNWNNR(1,IR,IY) + UGNWNNR(2,IR,IY) + &
                            UGNWLNR(1,IR,IY) + UGNWLNR(2,IR,IY) + &
                            UGNWFNR(1,IR,IY) + UGNWFNR(2,IR,IY) + &
                           (CGNTGEN(IR,IY, 4,1) + CGNTGEN(IR,IY, 4,2) + &
                            CGNTGEN(IR,IY, 5,1) + CGNTGEN(IR,IY, 5,2) + &
                            CGNTGEN(IR,IY, 6,1) + CGNTGEN(IR,IY, 6,2) + &
                            CGNTGEN(IR,IY, 7,1) + CGNTGEN(IR,IY, 7,2) + &
                            CGNTGEN(IR,IY, 8,1) + CGNTGEN(IR,IY, 8,2)) * 0.001
         T62(99,IR,IY,IS) = UGNDDNR(1,IR,IY) + UGNDDNR(2,IR,IY) + &
                            UGNDGNR(1,IR,IY) + UGNDGNR(2,IR,IY)
         T62(61,IR,IY,IS) = FSUM(T62(55,IR,IY,IS),6) + T62(99,IR,IY,IS)
         T62(62,IR,IY,IS) = T62(61,IR,IY,IS) - CGOTGEN(IR,IY,1) - CGOTGEN(IR,IY,2)
         T62(63,IR,IY,IS) = CGOTGEN(IR,IY,1) + CGOTGEN(IR,IY,2)
!        --- GENERATION BY CAPACITY LOCATED
         T62(64,IR,IY,IS) = UGNINR(IR,IY) - UGNSRV(IR,IY) + &
                            UGNHYNR(1,IR,IY) + UGNGENR(1,IR,IY) + &
                            UGNMSNR(1,IR,IY) + UGNWDNR(1,IR,IY) + &
                            UGNSONR(1,IR,IY) + UGNPVNR(1,IR,IY) + &
                            UGNPTNR(1,IR,IY) + UGNWNNR(1,IR,IY) + &
                            UGNWLNR(1,IR,IY) + UGNWFNR(1,IR,IY) + &
                            UGNSDNR(1,IR,IY) + &
                            UGNPSNR(1,IR,IY)
         T62(65,IR,IY,IS) = UGNOTR(IR,IY)
!       --- Combined heat and power
        T62(66,IR,IY,IS) = (CGTOTGENNR(IR,IY,1,1) + CGTOTGENNR(IR,IY,1,2)) * 0.001
        T62(67,IR,IY,IS) = (CGTOTGENNR(IR,IY,2,1) + CGTOTGENNR(IR,IY,2,2)) * 0.001
        T62(68,IR,IY,IS) = (CGTOTGENNR(IR,IY,4,1) + CGTOTGENNR(IR,IY,4,2)) * 0.001
        T62(69,IR,IY,IS) = (CGTOTGENNR(IR,IY,3,1) + CGTOTGENNR(IR,IY,3,2)) * 0.001
        T62(70,IR,IY,IS) = (CGTOTGENNR(IR,IY,5,1) + CGTOTGENNR(IR,IY,5,2)  + &
                            CGTOTGENNR(IR,IY,6,1) + CGTOTGENNR(IR,IY,6,2)  + &
                            CGTOTGENNR(IR,IY,7,1) + CGTOTGENNR(IR,IY,7,2)  + &
                            CGTOTGENNR(IR,IY,8,1) + CGTOTGENNR(IR,IY,8,2)  + &
                            CGTOTGENNR(IR,IY,9,1) + CGTOTGENNR(IR,IY,9,2)  + &
                            (CGTOTGENNR(IR,IY,10,1) + CGTOTGENNR(IR,IY,10,2)) * EUPVGENADJ(IY) + &  !adj PV
                            CGTOTGENNR(IR,IY,12,1) + CGTOTGENNR(IR,IY,12,2) ) * 0.001
        T62(71,IR,IY,IS) = (CGTOTGENNR(IR,IY,11,1) + CGTOTGENNR(IR,IY,11,2)) * 0.001
        T62(72,IR,IY,IS) = FSUM(T62(66,IR,IY,IS),6)
! ---
        T62(73,IR,IY,IS) = (CGTOTGENNR(IR,IY,1,1) + CGTOTGENNR(IR,IY,2,1) + CGTOTGENNR(IR,IY,3,1) +  &
                            CGTOTGENNR(IR,IY,4,1) + CGTOTGENNR(IR,IY,5,1) + CGTOTGENNR(IR,IY,6,1) + &
                            CGTOTGENNR(IR,IY,7,1) + CGTOTGENNR(IR,IY,8,1) + CGTOTGENNR(IR,IY,9,1) + &
                            (CGTOTGENNR(IR,IY,10,1) * EUPVGENADJ(IY)) + CGTOTGENNR(IR,IY,11,1)+ CGTOTGENNR(IR,IY,12,1)) & !adj pv
                            / 1000.
!       --- BOTH DEPEND ON T62(73,IR,IY,IS) SO MOVED DOWN HERE - JBJ
        T62(52,IR,IY,IS) = T62(73,IR,IY,IS)
        T62(53,IR,IY,IS) = T62(62,IR,IY,IS)
        T62(54,IR,IY,IS) = T62(48,IR,IY,IS) - T62(49,IR,IY,IS) + T62(50,IR,IY,IS) - &
                           T62(51,IR,IY,IS) + T62(52,IR,IY,IS) + T62(53,IR,IY,IS)

        T62(74,IR,IY,IS) = (CGTOTGENNR(IR,IY,1,2) + CGTOTGENNR(IR,IY,2,2) + CGTOTGENNR(IR,IY,3,2) +  &
                            CGTOTGENNR(IR,IY,4,2) + CGTOTGENNR(IR,IY,5,2) + CGTOTGENNR(IR,IY,6,2) + &
                            CGTOTGENNR(IR,IY,7,2) + CGTOTGENNR(IR,IY,8,2) + CGTOTGENNR(IR,IY,9,2) + &
                            (CGTOTGENNR(IR,IY,10,2) * EUPVGENADJ(IY)) + CGTOTGENNR(IR,IY,11,2)+ CGTOTGENNR(IR,IY,12,2)) &  !adj pv
                            / 1000.

        T62(101,IR,IY,IS) = T62( 61,IR,IY,IS) + T62( 72,IR,IY,IS)

        IF (IR.EQ.mnumnr) THEN
!        --- Sales
           T62(43,IR,IY,IS) = QELRS(11,IY) / .003412
           T62(44,IR,IY,IS) = QELCM(11,IY) / .003412
           T62(45,IR,IY,IS) = QELIN(11,IY) / .003412
           T62(46,IR,IY,IS) = QELTR(11,IY) / .003412
           T62(47,IR,IY,IS) = QELAS(11,IY) / .003412
!          --- End Use Prices
           T62(75,IR,IY,IS) = PELRS(11,IY) * .3412
           T62(76,IR,IY,IS) = PELCM(11,IY) * .3412
           T62(77,IR,IY,IS) = PELIN(11,IY) * .3412
           T62(78,IR,IY,IS) = PELTR(11,IY) * .3412
           T62(79,IR,IY,IS) = PELAS(11,IY) * .3412
!        --- FUEL PRICES
           T62(80,IR,IY,IS) = PCLEL(11,IY)
           T62(81,IR,IY,IS) = PNGEL(11,IY)
           T62(82,IR,IY,IS) = PDSEL(11,IY)
           T62(83,IR,IY,IS) = PRSEL(11,IY)
        ELSE
!        --- Sales
           T62(43,IR,IY,IS) = QELRSN(IR,IY) * .001
           T62(44,IR,IY,IS) = QELCMN(IR,IY) * .001
           T62(45,IR,IY,IS) = QELINN(IR,IY) * .001
           T62(46,IR,IY,IS) = QELTRN(IR,IY) * .001
           T62(47,IR,IY,IS) = QELASN(IR,IY) * .001
!          --- End Use Prices
           T62(75,IR,IY,IS) = PELRSNR(IR,IY) * .1
           T62(76,IR,IY,IS) = PELCMNR(IR,IY) * .1
           T62(77,IR,IY,IS) = PELINNR(IR,IY) * .1
           T62(78,IR,IY,IS) = PELTRNR(IR,IY) * .1
           T62(79,IR,IY,IS) = PELASNR(IR,IY) * .1
!        --- FUEL PRICES
           T62(80,IR,IY,IS) = UPCOALAVG(IR,IY)
           T62(81,IR,IY,IS) = UPGASPRC(IR,IY)
           T62(82,IR,IY,IS) = UPDISPRC(IR,IY)
           T62(83,IR,IY,IS) = UPRESPRC(IR,IY)
         ENDIF
         T62(84,IR,IY,IS) = UPRWDNR(IR,IY)
         T62(93,IR,IY,IS) = PECGENN(IR,IY) * .1
         T62(94,IR,IY,IS) = PECTRNN(IR,IY) * .1
         T62(95,IR,IY,IS) = PECDISN(IR,IY) * .1

         T62(111:115,IR,IY,IS) = T62(75:79,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
         T62(116:118,IR,IY,IS) = T62(93:95,IR,IY,IS) / SCALPR * MC_JPGDP(IY)
         T62(119:123,IR,IY,IS) = T62(80:84,IR,IY,IS) / SCALPR * MC_JPGDP(IY)

!        --- FUEL CONSUMPTION : UTILITY (INCLUDES NONTRADITIONAL COGEN IN AEO03)
         T62(85,IR,IY,IS) = (UFLCLNR(1,IR,IY) + UFLCLNR(2,IR,IY)) * 0.001
         T62(86,IR,IY,IS) = (UFLGFNR(1,IR,IY) + UFLGINR(1,IR,IY) + UFLGCNR(1,IR,IY) + &
                             UFLGFNR(2,IR,IY) + UFLGINR(2,IR,IY) + UFLGCNR(2,IR,IY) + &
                             UFLDGNR(1,IR,IY) + UFLDGNR(2,IR,IY)) * 0.001
         T62(87,IR,IY,IS) = (UFLDSNR(1,IR,IY) + UFLRLNR(1,IR,IY) + UFLRHNR(1,IR,IY) + &
                             UFLDSNR(2,IR,IY) + UFLRLNR(2,IR,IY) + UFLRHNR(2,IR,IY) + &
                             UFLDDNR(1,IR,IY) + UFLDDNR(2,IR,IY)) * 0.001
         T62(88,IR,IY,IS) = FSUM(T62(85,IR,IY,IS),3)
! CARBON IS IN Short Tons, NOT Metric Tons LIKE IN TABLE 17
         FFF=1.
         IF (TRIM(CARBON_OR_2) .EQ. 'CO2') FFF = 12./44.
         T62(89,IR,IY,IS) =(UCARINR(IR,IY) + UCAROTR(IR,IY)) * em_elec(8,11,ICY) / &
                      (UCO2INR(mnumnr,IY) + UCO2OTR(mnumnr,IY)) * 2204. / 2000.
         T62(90,IR,IY,IS) =(UCO2INR(IR,IY) + UCO2OTR(IR,IY)) * em_elec(8,11,ICY) / &
                      (UCO2INR(mnumnr,IY) + UCO2OTR(mnumnr,IY)) * 2204. / 2000.
         so2coal = 0.0
         DO III = 1, NUTSEC
           so2coal = so2coal + UTTSO2(III,IY)
         ENDDO
         DO ISO2 = 1 , NUM_SO2_GRP
            so2coal = so2coal + so2other(IY,ISO2) * 0.000001
         END DO
         T62(91,IR,IY,IS) = so2coal * (USO2INR(IR,IY) + USO2OTR(IR,IY))/ &
                           (USO2INR(MNUMNR,IY) + USO2OTR(MNUMNR,IY)) + CTLSO2 * CTLGEN(IR,IY)
         T62(92,IR,IY,IS) = UNOXOTR(IR,IY) + UNOXINR(IR,IY) + CTLNOX * CTLGEN(IR,IY)
         T62(100,IR,IY,IS) = (TOT_MERC(IY) / 2000.0) * (UHGINR(IR,IY) + UHGOTR(IR,IY))/ &
                           (UHGINR(MNUMNR,IY) + UHGOTR(MNUMNR,IY)) + CTLHG * CTLGEN(IR,IY)
620   CONTINUE

!     Table 63 Electricity Generation by North American Electric Reliability Council (NERC) Region and Source

         NUMREP=8
         DO IY = 1,LASTYR
           DO IR = 1,num_elec_regions
           T63((IR-1)*NUMREP+1,IY,IS) = UGNCLNR(1,IR,IY) + UGNCLNR(2,IR,IY) + &
                      (CGNTGEN(IR,IY, 1,1) + CGNTGEN(IR,IY, 1,2)) * 0.001
           T63((IR-1)*NUMREP+2,IY,IS) = UGNDSNR(1,IR,IY) + UGNDSNR(2,IR,IY) + &
                           UGNRLNR(1,IR,IY) + UGNRLNR(2,IR,IY) + &
                           UGNRHNR(1,IR,IY) + UGNRHNR(2,IR,IY) + &
                      (CGNTGEN(IR,IY, 2,1) + CGNTGEN(IR,IY, 2,2)) * 0.001
           T63((IR-1)*NUMREP+3,IY,IS) = UGNGFNR(1,IR,IY) + UGNGFNR(2,IR,IY) + &
                           UGNGINR(1,IR,IY) + UGNGINR(2,IR,IY) + &
                           UGNGCNR(1,IR,IY) + UGNGCNR(2,IR,IY) + &
                      (CGNTGEN(IR,IY, 3,1) + CGNTGEN(IR,IY, 3,2)) * 0.001
           T63((IR-1)*NUMREP+4,IY,IS) = UGNURNR(1,IR,IY) + UGNURNR(2,IR,IY)
           T63((IR-1)*NUMREP+5,IY,IS) = UGNPSNR(1,IR,IY) + UGNPSNR(2,IR,IY)  + &
                                        UGNSDNR(1,IR,IY) + UGNSDNR(2,IR,IY)  + &
                      (CGNTGEN(IR,IY, 9,1) + CGNTGEN(IR,IY, 9,2) + &
                       CGNTGEN(IR,IY,10,1) + CGNTGEN(IR,IY,10,2)) * 0.001
           T63((IR-1)*NUMREP+6,IY,IS) = UGNHYNR(1,IR,IY) + UGNHYNR(2,IR,IY) + &
                       UGNGENR(1,IR,IY) + UGNGENR(2,IR,IY) + &
                       UGNMSNR(1,IR,IY) + UGNMSNR(2,IR,IY) + &
                       UGNWDNR(1,IR,IY) + UGNWDNR(2,IR,IY) + &
                       UGNSONR(1,IR,IY) + UGNSONR(2,IR,IY) + &
                       UGNPVNR(1,IR,IY) + UGNPVNR(2,IR,IY) + &
                       UGNPTNR(1,IR,IY) + UGNPTNR(2,IR,IY) + &
                       UGNWNNR(1,IR,IY) + UGNWNNR(2,IR,IY) + &
                       UGNWLNR(1,IR,IY) + UGNWLNR(2,IR,IY) + &
                       UGNWFNR(1,IR,IY) + UGNWFNR(2,IR,IY) + &
                      (CGNTGEN(IR,IY, 4,1) + CGNTGEN(IR,IY, 4,2) + &
                       CGNTGEN(IR,IY, 5,1) + CGNTGEN(IR,IY, 5,2) + &
                       CGNTGEN(IR,IY, 6,1) + CGNTGEN(IR,IY, 6,2) + &
                       CGNTGEN(IR,IY, 7,1) + CGNTGEN(IR,IY, 7,2) + &
                       CGNTGEN(IR,IY, 8,1) + CGNTGEN(IR,IY, 8,2)) * 0.001
           T63((IR-1)*NUMREP+7,IY,IS) = UGNDDNR(1,IR,IY) + UGNDDNR(2,IR,IY) + &
                                        UGNDGNR(1,IR,IY) + UGNDGNR(2,IR,IY)
           T63((IR-1)*NUMREP+8,IY,IS) =  &
UGNCLNR(1,IR,IY) + UGNCLNR(2,IR,IY) + &
UGNDSNR(1,IR,IY) + UGNDSNR(2,IR,IY) + &
UGNRLNR(1,IR,IY) + UGNRLNR(2,IR,IY) + &
UGNRHNR(1,IR,IY) + UGNRHNR(2,IR,IY) + &
UGNGFNR(1,IR,IY) + UGNGFNR(2,IR,IY) + &
UGNGINR(1,IR,IY) + UGNGINR(2,IR,IY) + &
UGNGCNR(1,IR,IY) + UGNGCNR(2,IR,IY) + &
UGNURNR(1,IR,IY) + UGNURNR(2,IR,IY) + &
UGNPSNR(1,IR,IY) + UGNPSNR(2,IR,IY) + &
UGNSDNR(1,IR,IY) + UGNSDNR(2,IR,IY) + &
UGNHYNR(1,IR,IY) + UGNHYNR(2,IR,IY) + &
UGNGENR(1,IR,IY) + UGNGENR(2,IR,IY) + &
UGNMSNR(1,IR,IY) + UGNMSNR(2,IR,IY) + &
UGNWDNR(1,IR,IY) + UGNWDNR(2,IR,IY) + &
UGNSONR(1,IR,IY) + UGNSONR(2,IR,IY) + &
UGNPVNR(1,IR,IY) + UGNPVNR(2,IR,IY) + &
UGNPTNR(1,IR,IY) + UGNPTNR(2,IR,IY) + &
UGNWNNR(1,IR,IY) + UGNWNNR(2,IR,IY) + &
UGNWLNR(1,IR,IY) + UGNWLNR(2,IR,IY) + &
UGNWFNR(1,IR,IY) + UGNWFNR(2,IR,IY) + &
UGNDDNR(1,IR,IY) + UGNDDNR(2,IR,IY) + &
UGNDGNR(1,IR,IY) + UGNDGNR(2,IR,IY) + &
                      (CGNTGEN(IR,IY, 1,1) + CGNTGEN(IR,IY, 1,2) + &
                       CGNTGEN(IR,IY, 2,1) + CGNTGEN(IR,IY, 2,2) + &
                       CGNTGEN(IR,IY, 3,1) + CGNTGEN(IR,IY, 3,2) + &
                       CGNTGEN(IR,IY, 4,1) + CGNTGEN(IR,IY, 4,2) + &
                       CGNTGEN(IR,IY, 5,1) + CGNTGEN(IR,IY, 5,2) + &
                       CGNTGEN(IR,IY, 6,1) + CGNTGEN(IR,IY, 6,2) + &
                       CGNTGEN(IR,IY, 7,1) + CGNTGEN(IR,IY, 7,2) + &
                       CGNTGEN(IR,IY, 8,1) + CGNTGEN(IR,IY, 8,2) + &
                       CGNTGEN(IR,IY, 9,1) + CGNTGEN(IR,IY, 9,2) + &
                       CGNTGEN(IR,IY,10,1) + CGNTGEN(IR,IY,10,2)) * 0.001
           ENDDO
         ENDDO

!     Table 64 Electric Generation Capacity by NERC Region and Source

         NUMREP=10
         DO IY = 1,LASTYR
           DO IR=1,num_elec_regions
             T64(NUMREP*(IR-1)+1,IY,IS) = UCAPCSU(IR,IY) + UCAPCSN(IR,IY) + UCAPCSC(IR,IY)
             T64(NUMREP*(IR-1)+2,IY,IS) = UCAPOSU(IR,IY) + UCAPOSN(IR,IY) + UCAPOSC(IR,IY) + &
                                          UCAPNGU(IR,IY) + UCAPNGN(IR,IY)
             T64(NUMREP*(IR-1)+3,IY,IS) = UCAPCCU(IR,IY) + UCAPCCN(IR,IY) + UCAPCCC(IR,IY)
             T64(NUMREP*(IR-1)+4,IY,IS) = UCAPCTU(IR,IY) + UCAPCTN(IR,IY) + UCAPCTC(IR,IY)
             T64(NUMREP*(IR-1)+5,IY,IS) = UCAPNUU(IR,IY) + UCAPNUN(IR,IY) + &
                                          UCAPSMU(IR,IY) + UCAPSMN(IR,IY)
             T64(NUMREP*(IR-1)+6,IY,IS) = UCAPPSU(IR,IY) + UCAPPSN(IR,IY)
             T64(NUMREP*(IR-1)+7,IY,IS) = UCAPFCU(IR,IY) + UCAPFCN(IR,IY)
             T64(NUMREP*(IR-1)+8,IY,IS) = UCAPHYU(IR,IY) + UCAPGEU(IR,IY) + &
                                          UCAPMSU(IR,IY) + UCAPWDU(IR,IY) + &
                                          UCAPSTU(IR,IY) + UCAPPVU(IR,IY) + &
                                          UCAPWNU(IR,IY) + UCAPWFU(IR,IY) + &
                                          UCAPWLU(IR,IY) + UCAPPTU(IR,IY) + &
                                          UCAPHYN(IR,IY) + UCAPGEN(IR,IY) + &
                                          UCAPMSN(IR,IY) + UCAPWDN(IR,IY) + &
                                          UCAPSTN(IR,IY) + UCAPPVN(IR,IY) + &
                                          UCAPWNN(IR,IY) + UCAPWFN(IR,IY) + &
                                          UCAPWLN(IR,IY) + UCAPPTN(IR,IY) + &
                                          UCAPHYC(IR,IY) + UCAPGEC(IR,IY) + &
                                          UCAPMSC(IR,IY) + UCAPWDC(IR,IY) + &
                                          UCAPSTC(IR,IY) + UCAPPVC(IR,IY) + &
                                          UCAPWNC(IR,IY) + UCAPWFC(IR,IY) + &
                                          UCAPWLC(IR,IY) + UCAPPTC(IR,IY)
             T64(NUMREP*(IR-1)+9,IY,IS) = UCAPDBU(IR,IY) + UCAPDBN(IR,IY) + &
                                          UCAPDPU(IR,IY) + UCAPDPN(IR,IY)
             T64(NUMREP*(IR-1)+10,IY,IS) =UCAPTLU(IR,IY) + UCAPTLN(IR,IY) + UCAPTLC(IR,IY)
           ENDDO
         ENDDO

!  Table 65.
        DO IY = 1, LASTYR
!  Ethane natural gas plant liquid production
           T65( 1,IY,IS) = OGNGPLET( 8,IY) + OGNGPLET( 9,IY) + OGNGPLET(10,IY) + OGNGPLET(12,IY) + &
                           OGNGPLET(15,IY) + OGNGPLET(16,IY) + OGNGPLET(19,IY) + OGNGPLET(22,IY) + &
                           OGNGPLET(23,IY) + OGNGPLET(24,IY) + OGNGPLET(25,IY) + OGNGPLET(33,IY) + &
                           OGNGPLET(34,IY) + OGNGPLET(37,IY) + OGNGPLET(38,IY) + OGNGPLET(40,IY) + &
                           OGNGPLET(43,IY) + OGNGPLET(44,IY) + OGNGPLET(45,IY) + OGNGPLET(47,IY) + &
                           OGNGPLET(61,IY) + OGNGPLET(62,IY) + OGNGPLET(64,IY) + OGNGPLET(65,IY)
! not including Hawaii:                      OGNGPLET(13,IY)
           T65( 2,IY,IS) = OGNGPLET( 1,IY) + OGNGPLET( 2,IY) + OGNGPLET(11,IY) + OGNGPLET(20,IY) + &
                           OGNGPLET(21,IY) + OGNGPLET(27,IY) + OGNGPLET(28,IY) + OGNGPLET(48,IY) + &
                           OGNGPLET(49,IY) + OGNGPLET(50,IY) + OGNGPLET(51,IY) + OGNGPLET(53,IY)
           T65( 3,IY,IS) = OGNGPLET( 5,IY) + OGNGPLET(17,IY) + OGNGPLET(18,IY) + OGNGPLET(26,IY) + &
                           OGNGPLET(29,IY) + OGNGPLET(31,IY) + OGNGPLET(41,IY) + OGNGPLET(59,IY)
           T65( 4,IY,IS) = OGNGPLET(35,IY) + OGNGPLET(52,IY) + OGNGPLET(54,IY) + OGNGPLET(55,IY) + &
                           OGNGPLET(56,IY) + OGNGPLET(57,IY) + OGNGPLET(58,IY)
           T65( 5,IY,IS) = OGNGPLET( 4,IY) + OGNGPLET( 7,IY) + OGNGPLET(14,IY) + OGNGPLET(32,IY) + &
                           OGNGPLET(36,IY) + OGNGPLET(60,IY) + OGNGPLET(66,IY)
           T65( 6,IY,IS) = OGNGPLET(30,IY) + OGNGPLET(39,IY) + OGNGPLET(46,IY)
           T65( 7,IY,IS) = OGNGPLET( 6,IY) + OGNGPLET(42,IY) + OGNGPLET(63,IY)
           T65( 8,IY,IS) = OGNGPLET(67,IY) + OGNGPLET(68,IY) + OGNGPLET(69,IY) + OGNGPLET(76,IY) + &
                           OGNGPLET(77,IY) + OGNGPLET(78,IY)
           T65( 9,IY,IS) = OGNGPLET(73,IY) + OGNGPLET(74,IY) + OGNGPLET(82,IY) + OGNGPLET(83,IY)
           T65(10,IY,IS) = OGNGPLET(70,IY) + OGNGPLET(71,IY) + OGNGPLET(72,IY) + OGNGPLET(79,IY) + &
                           OGNGPLET(80,IY) + OGNGPLET(81,IY)
           T65(11,IY,IS) = OGNGPLET( 3,IY) + OGNGPLET(75,IY) + OGNGPLET(84,IY)
           T65(12,IY,IS) = FSUM(T65( 1,IY,IS),11)

!  Propane natural gas plant liquid production
           T65(13,IY,IS) = OGNGPLPR( 8,IY) + OGNGPLPR( 9,IY) + OGNGPLPR(10,IY) + OGNGPLPR(12,IY) + &
                           OGNGPLPR(15,IY) + OGNGPLPR(16,IY) + OGNGPLPR(19,IY) + OGNGPLPR(22,IY) + &
                           OGNGPLPR(23,IY) + OGNGPLPR(24,IY) + OGNGPLPR(25,IY) + OGNGPLPR(33,IY) + &
                           OGNGPLPR(34,IY) + OGNGPLPR(37,IY) + OGNGPLPR(38,IY) + OGNGPLPR(40,IY) + &
                           OGNGPLPR(43,IY) + OGNGPLPR(44,IY) + OGNGPLPR(45,IY) + OGNGPLPR(47,IY) + &
                           OGNGPLPR(61,IY) + OGNGPLPR(62,IY) + OGNGPLPR(64,IY) + OGNGPLPR(65,IY)
! not including Hawaii:                      OGNGPLPR(13,IY)
           T65(14,IY,IS) = OGNGPLPR( 1,IY) + OGNGPLPR( 2,IY) + OGNGPLPR(11,IY) + OGNGPLPR(20,IY) + &
                           OGNGPLPR(21,IY) + OGNGPLPR(27,IY) + OGNGPLPR(28,IY) + OGNGPLPR(48,IY) + &
                           OGNGPLPR(49,IY) + OGNGPLPR(50,IY) + OGNGPLPR(51,IY) + OGNGPLPR(53,IY)
           T65(15,IY,IS) = OGNGPLPR( 5,IY) + OGNGPLPR(17,IY) + OGNGPLPR(18,IY) + OGNGPLPR(26,IY) + &
                           OGNGPLPR(29,IY) + OGNGPLPR(31,IY) + OGNGPLPR(41,IY) + OGNGPLPR(59,IY)
           T65(16,IY,IS) = OGNGPLPR(35,IY) + OGNGPLPR(52,IY) + OGNGPLPR(54,IY) + OGNGPLPR(55,IY) + &
                           OGNGPLPR(56,IY) + OGNGPLPR(57,IY) + OGNGPLPR(58,IY)
           T65(17,IY,IS) = OGNGPLPR( 4,IY) + OGNGPLPR( 7,IY) + OGNGPLPR(14,IY) + OGNGPLPR(32,IY) + &
                           OGNGPLPR(36,IY) + OGNGPLPR(60,IY) + OGNGPLPR(66,IY)
           T65(18,IY,IS) = OGNGPLPR(30,IY) + OGNGPLPR(39,IY) + OGNGPLPR(46,IY)
           T65(19,IY,IS) = OGNGPLPR( 6,IY) + OGNGPLPR(42,IY) + OGNGPLPR(63,IY)
           T65(20,IY,IS) = OGNGPLPR(67,IY) + OGNGPLPR(68,IY) + OGNGPLPR(69,IY) + OGNGPLPR(76,IY) + &
                           OGNGPLPR(77,IY) + OGNGPLPR(78,IY)
           T65(21,IY,IS) = OGNGPLPR(73,IY) + OGNGPLPR(74,IY) + OGNGPLPR(82,IY) + OGNGPLPR(83,IY)
           T65(22,IY,IS) = OGNGPLPR(70,IY) + OGNGPLPR(71,IY) + OGNGPLPR(72,IY) + OGNGPLPR(79,IY) + &
                           OGNGPLPR(80,IY) + OGNGPLPR(81,IY)
           T65(23,IY,IS) = OGNGPLPR( 3,IY) + OGNGPLPR(75,IY) + OGNGPLPR(84,IY)
           T65(24,IY,IS) = FSUM(T65(13,IY,IS),11)

!  Normal butane natural gas plant liquid production
           T65(25,IY,IS) = OGNGPLBU( 8,IY) + OGNGPLBU( 9,IY) + OGNGPLBU(10,IY) + OGNGPLBU(12,IY) + &
                           OGNGPLBU(15,IY) + OGNGPLBU(16,IY) + OGNGPLBU(19,IY) + OGNGPLBU(22,IY) + &
                           OGNGPLBU(23,IY) + OGNGPLBU(24,IY) + OGNGPLBU(25,IY) + OGNGPLBU(33,IY) + &
                           OGNGPLBU(34,IY) + OGNGPLBU(37,IY) + OGNGPLBU(38,IY) + OGNGPLBU(40,IY) + &
                           OGNGPLBU(43,IY) + OGNGPLBU(44,IY) + OGNGPLBU(45,IY) + OGNGPLBU(47,IY) + &
                           OGNGPLBU(61,IY) + OGNGPLBU(62,IY) + OGNGPLBU(64,IY) + OGNGPLBU(65,IY)
! not including Hawaii:                      OGNGPLBU(13,IY)
           T65(26,IY,IS) = OGNGPLBU( 1,IY) + OGNGPLBU( 2,IY) + OGNGPLBU(11,IY) + OGNGPLBU(20,IY) + &
                           OGNGPLBU(21,IY) + OGNGPLBU(27,IY) + OGNGPLBU(28,IY) + OGNGPLBU(48,IY) + &
                           OGNGPLBU(49,IY) + OGNGPLBU(50,IY) + OGNGPLBU(51,IY) + OGNGPLBU(53,IY)
           T65(27,IY,IS) = OGNGPLBU( 5,IY) + OGNGPLBU(17,IY) + OGNGPLBU(18,IY) + OGNGPLBU(26,IY) + &
                           OGNGPLBU(29,IY) + OGNGPLBU(31,IY) + OGNGPLBU(41,IY) + OGNGPLBU(59,IY)
           T65(28,IY,IS) = OGNGPLBU(35,IY) + OGNGPLBU(52,IY) + OGNGPLBU(54,IY) + OGNGPLBU(55,IY) + &
                           OGNGPLBU(56,IY) + OGNGPLBU(57,IY) + OGNGPLBU(58,IY)
           T65(29,IY,IS) = OGNGPLBU( 4,IY) + OGNGPLBU( 7,IY) + OGNGPLBU(14,IY) + OGNGPLBU(32,IY) + &
                           OGNGPLBU(36,IY) + OGNGPLBU(60,IY) + OGNGPLBU(66,IY)
           T65(30,IY,IS) = OGNGPLBU(30,IY) + OGNGPLBU(39,IY) + OGNGPLBU(46,IY)
           T65(31,IY,IS) = OGNGPLBU( 6,IY) + OGNGPLBU(42,IY) + OGNGPLBU(63,IY)
           T65(32,IY,IS) = OGNGPLBU(67,IY) + OGNGPLBU(68,IY) + OGNGPLBU(69,IY) + OGNGPLBU(76,IY) + &
                           OGNGPLBU(77,IY) + OGNGPLBU(78,IY)
           T65(33,IY,IS) = OGNGPLBU(73,IY) + OGNGPLBU(74,IY) + OGNGPLBU(82,IY) + OGNGPLBU(83,IY)
           T65(34,IY,IS) = OGNGPLBU(70,IY) + OGNGPLBU(71,IY) + OGNGPLBU(72,IY) + OGNGPLBU(79,IY) + &
                           OGNGPLBU(80,IY) + OGNGPLBU(81,IY)
           T65(35,IY,IS) = OGNGPLBU( 3,IY) + OGNGPLBU(75,IY) + OGNGPLBU(84,IY)
           T65(36,IY,IS) = FSUM(T65(25,IY,IS),11)

!  Isobutane natural gas plant liquid production
           T65(37,IY,IS) = OGNGPLIS( 8,IY) + OGNGPLIS( 9,IY) + OGNGPLIS(10,IY) + OGNGPLIS(12,IY) + &
                           OGNGPLIS(15,IY) + OGNGPLIS(16,IY) + OGNGPLIS(19,IY) + OGNGPLIS(22,IY) + &
                           OGNGPLIS(23,IY) + OGNGPLIS(24,IY) + OGNGPLIS(25,IY) + OGNGPLIS(33,IY) + &
                           OGNGPLIS(34,IY) + OGNGPLIS(37,IY) + OGNGPLIS(38,IY) + OGNGPLIS(40,IY) + &
                           OGNGPLIS(43,IY) + OGNGPLIS(44,IY) + OGNGPLIS(45,IY) + OGNGPLIS(47,IY) + &
                           OGNGPLIS(61,IY) + OGNGPLIS(62,IY) + OGNGPLIS(64,IY) + OGNGPLIS(65,IY)
! not including Hawaii:                      OGNGPLIS(13,IY)
           T65(38,IY,IS) = OGNGPLIS( 1,IY) + OGNGPLIS( 2,IY) + OGNGPLIS(11,IY) + OGNGPLIS(20,IY) + &
                           OGNGPLIS(21,IY) + OGNGPLIS(27,IY) + OGNGPLIS(28,IY) + OGNGPLIS(48,IY) + &
                           OGNGPLIS(49,IY) + OGNGPLIS(50,IY) + OGNGPLIS(51,IY) + OGNGPLIS(53,IY)
           T65(39,IY,IS) = OGNGPLIS( 5,IY) + OGNGPLIS(17,IY) + OGNGPLIS(18,IY) + OGNGPLIS(26,IY) + &
                           OGNGPLIS(29,IY) + OGNGPLIS(31,IY) + OGNGPLIS(41,IY) + OGNGPLIS(59,IY)
           T65(40,IY,IS) = OGNGPLIS(35,IY) + OGNGPLIS(52,IY) + OGNGPLIS(54,IY) + OGNGPLIS(55,IY) + &
                           OGNGPLIS(56,IY) + OGNGPLIS(57,IY) + OGNGPLIS(58,IY)
           T65(41,IY,IS) = OGNGPLIS( 4,IY) + OGNGPLIS( 7,IY) + OGNGPLIS(14,IY) + OGNGPLIS(32,IY) + &
                           OGNGPLIS(36,IY) + OGNGPLIS(60,IY) + OGNGPLIS(66,IY)
           T65(42,IY,IS) = OGNGPLIS(30,IY) + OGNGPLIS(39,IY) + OGNGPLIS(46,IY)
           T65(43,IY,IS) = OGNGPLIS( 6,IY) + OGNGPLIS(42,IY) + OGNGPLIS(63,IY)
           T65(44,IY,IS) = OGNGPLIS(67,IY) + OGNGPLIS(68,IY) + OGNGPLIS(69,IY) + OGNGPLIS(76,IY) + &
                           OGNGPLIS(77,IY) + OGNGPLIS(78,IY)
           T65(45,IY,IS) = OGNGPLIS(73,IY) + OGNGPLIS(74,IY) + OGNGPLIS(82,IY) + OGNGPLIS(83,IY)
           T65(46,IY,IS) = OGNGPLIS(70,IY) + OGNGPLIS(71,IY) + OGNGPLIS(72,IY) + OGNGPLIS(79,IY) + &
                           OGNGPLIS(80,IY) + OGNGPLIS(81,IY)
           T65(47,IY,IS) = OGNGPLIS( 3,IY) + OGNGPLIS(75,IY) + OGNGPLIS(84,IY)
           T65(48,IY,IS) = FSUM(T65(37,IY,IS),11)

!  Natural gasoline natural gas plant liquid production
           T65(49,IY,IS) = OGNGPLPP( 8,IY) + OGNGPLPP( 9,IY) + OGNGPLPP(10,IY) + OGNGPLPP(12,IY) + &
                           OGNGPLPP(15,IY) + OGNGPLPP(16,IY) + OGNGPLPP(19,IY) + OGNGPLPP(22,IY) + &
                           OGNGPLPP(23,IY) + OGNGPLPP(24,IY) + OGNGPLPP(25,IY) + OGNGPLPP(33,IY) + &
                           OGNGPLPP(34,IY) + OGNGPLPP(37,IY) + OGNGPLPP(38,IY) + OGNGPLPP(40,IY) + &
                           OGNGPLPP(43,IY) + OGNGPLPP(44,IY) + OGNGPLPP(45,IY) + OGNGPLPP(47,IY) + &
                           OGNGPLPP(61,IY) + OGNGPLPP(62,IY) + OGNGPLPP(64,IY) + OGNGPLPP(65,IY)
! not including Hawaii:                      OGNGPLPP(13,IY)
           T65(50,IY,IS) = OGNGPLPP( 1,IY) + OGNGPLPP( 2,IY) + OGNGPLPP(11,IY) + OGNGPLPP(20,IY) + &
                           OGNGPLPP(21,IY) + OGNGPLPP(27,IY) + OGNGPLPP(28,IY) + OGNGPLPP(48,IY) + &
                           OGNGPLPP(49,IY) + OGNGPLPP(50,IY) + OGNGPLPP(51,IY) + OGNGPLPP(53,IY)
           T65(51,IY,IS) = OGNGPLPP( 5,IY) + OGNGPLPP(17,IY) + OGNGPLPP(18,IY) + OGNGPLPP(26,IY) + &
                           OGNGPLPP(29,IY) + OGNGPLPP(31,IY) + OGNGPLPP(41,IY) + OGNGPLPP(59,IY)
           T65(52,IY,IS) = OGNGPLPP(35,IY) + OGNGPLPP(52,IY) + OGNGPLPP(54,IY) + OGNGPLPP(55,IY) + &
                           OGNGPLPP(56,IY) + OGNGPLPP(57,IY) + OGNGPLPP(58,IY)
           T65(53,IY,IS) = OGNGPLPP( 4,IY) + OGNGPLPP( 7,IY) + OGNGPLPP(14,IY) + OGNGPLPP(32,IY) + &
                           OGNGPLPP(36,IY) + OGNGPLPP(60,IY) + OGNGPLPP(66,IY)
           T65(54,IY,IS) = OGNGPLPP(30,IY) + OGNGPLPP(39,IY) + OGNGPLPP(46,IY)
           T65(55,IY,IS) = OGNGPLPP( 6,IY) + OGNGPLPP(42,IY) + OGNGPLPP(63,IY)
           T65(56,IY,IS) = OGNGPLPP(67,IY) + OGNGPLPP(68,IY) + OGNGPLPP(69,IY) + OGNGPLPP(76,IY) + &
                           OGNGPLPP(77,IY) + OGNGPLPP(78,IY)
           T65(57,IY,IS) = OGNGPLPP(73,IY) + OGNGPLPP(74,IY) + OGNGPLPP(82,IY) + OGNGPLPP(83,IY)
           T65(58,IY,IS) = OGNGPLPP(70,IY) + OGNGPLPP(71,IY) + OGNGPLPP(72,IY) + OGNGPLPP(79,IY) + &
                           OGNGPLPP(80,IY) + OGNGPLPP(81,IY)
           T65(59,IY,IS) = OGNGPLPP( 3,IY) + OGNGPLPP(75,IY) + OGNGPLPP(84,IY)
           T65(60,IY,IS) = FSUM(T65(49,IY,IS),11)

!  Total natural gas plant liquid production
           T65(61,IY,IS) = OGNGPLPRD( 8,IY) + OGNGPLPRD( 9,IY) + OGNGPLPRD(10,IY) + OGNGPLPRD(12,IY) + &
                           OGNGPLPRD(15,IY) + OGNGPLPRD(16,IY) + OGNGPLPRD(19,IY) + OGNGPLPRD(22,IY) + &
                           OGNGPLPRD(23,IY) + OGNGPLPRD(24,IY) + OGNGPLPRD(25,IY) + OGNGPLPRD(33,IY) + &
                           OGNGPLPRD(34,IY) + OGNGPLPRD(37,IY) + OGNGPLPRD(38,IY) + OGNGPLPRD(40,IY) + &
                           OGNGPLPRD(43,IY) + OGNGPLPRD(44,IY) + OGNGPLPRD(45,IY) + OGNGPLPRD(47,IY) + &
                           OGNGPLPRD(61,IY) + OGNGPLPRD(62,IY) + OGNGPLPRD(64,IY) + OGNGPLPRD(65,IY)
! not including Hawaii:                       OGNGPLPRD(13,IY)
           T65(62,IY,IS) = OGNGPLPRD( 1,IY) + OGNGPLPRD( 2,IY) + OGNGPLPRD(11,IY) + OGNGPLPRD(20,IY) + &
                           OGNGPLPRD(21,IY) + OGNGPLPRD(27,IY) + OGNGPLPRD(28,IY) + OGNGPLPRD(48,IY) + &
                           OGNGPLPRD(49,IY) + OGNGPLPRD(50,IY) + OGNGPLPRD(51,IY) + OGNGPLPRD(53,IY)
           T65(63,IY,IS) = OGNGPLPRD( 5,IY) + OGNGPLPRD(17,IY) + OGNGPLPRD(18,IY) + OGNGPLPRD(26,IY) + &
                           OGNGPLPRD(29,IY) + OGNGPLPRD(31,IY) + OGNGPLPRD(41,IY) + OGNGPLPRD(59,IY)
           T65(64,IY,IS) = OGNGPLPRD(35,IY) + OGNGPLPRD(52,IY) + OGNGPLPRD(54,IY) + OGNGPLPRD(55,IY) + &
                           OGNGPLPRD(56,IY) + OGNGPLPRD(57,IY) + OGNGPLPRD(58,IY)
           T65(65,IY,IS) = OGNGPLPRD( 4,IY) + OGNGPLPRD( 7,IY) + OGNGPLPRD(14,IY) + OGNGPLPRD(32,IY) + &
                           OGNGPLPRD(36,IY) + OGNGPLPRD(60,IY) + OGNGPLPRD(66,IY)
           T65(66,IY,IS) = OGNGPLPRD(30,IY) + OGNGPLPRD(39,IY) + OGNGPLPRD(46,IY)
           T65(67,IY,IS) = OGNGPLPRD( 6,IY) + OGNGPLPRD(42,IY) + OGNGPLPRD(63,IY)
           T65(68,IY,IS) = OGNGPLPRD(67,IY) + OGNGPLPRD(68,IY) + OGNGPLPRD(69,IY) + OGNGPLPRD(76,IY) + &
                           OGNGPLPRD(77,IY) + OGNGPLPRD(78,IY)
           T65(69,IY,IS) = OGNGPLPRD(73,IY) + OGNGPLPRD(74,IY) + OGNGPLPRD(82,IY) + OGNGPLPRD(83,IY)
           T65(70,IY,IS) = OGNGPLPRD(70,IY) + OGNGPLPRD(71,IY) + OGNGPLPRD(72,IY) + OGNGPLPRD(79,IY) + &
                           OGNGPLPRD(80,IY) + OGNGPLPRD(81,IY)
           T65(71,IY,IS) = OGNGPLPRD( 3,IY) + OGNGPLPRD(75,IY) + OGNGPLPRD(84,IY)
           T65(72,IY,IS) = FSUM(T65(61,IY,IS),11)

! this section adds up the piece variables for each region to cross-check against the total variable in the preceding section
           T65(73,IY,IS) = T65( 1,IY,IS) + T65(13,IY,IS) + T65(25,IY,IS) + T65(37,IY,IS) + T65(49,IY,IS)
           T65(74,IY,IS) = T65( 2,IY,IS) + T65(14,IY,IS) + T65(26,IY,IS) + T65(38,IY,IS) + T65(50,IY,IS)
           T65(75,IY,IS) = T65( 3,IY,IS) + T65(15,IY,IS) + T65(27,IY,IS) + T65(39,IY,IS) + T65(51,IY,IS)
           T65(76,IY,IS) = T65( 4,IY,IS) + T65(16,IY,IS) + T65(28,IY,IS) + T65(40,IY,IS) + T65(52,IY,IS)
           T65(77,IY,IS) = T65( 5,IY,IS) + T65(17,IY,IS) + T65(29,IY,IS) + T65(41,IY,IS) + T65(53,IY,IS)
           T65(78,IY,IS) = T65( 6,IY,IS) + T65(18,IY,IS) + T65(30,IY,IS) + T65(42,IY,IS) + T65(54,IY,IS)
           T65(79,IY,IS) = T65( 7,IY,IS) + T65(19,IY,IS) + T65(31,IY,IS) + T65(43,IY,IS) + T65(55,IY,IS)
           T65(80,IY,IS) = T65( 8,IY,IS) + T65(20,IY,IS) + T65(32,IY,IS) + T65(44,IY,IS) + T65(56,IY,IS)
           T65(81,IY,IS) = T65( 9,IY,IS) + T65(21,IY,IS) + T65(33,IY,IS) + T65(45,IY,IS) + T65(57,IY,IS)
           T65(82,IY,IS) = T65(10,IY,IS) + T65(22,IY,IS) + T65(34,IY,IS) + T65(46,IY,IS) + T65(58,IY,IS)
           T65(83,IY,IS) = T65(11,IY,IS) + T65(23,IY,IS) + T65(35,IY,IS) + T65(47,IY,IS) + T65(59,IY,IS)
           T65(84,IY,IS) = FSUM(T65(73,IY,IS),11)
        ENDDO

!  Table 66.
        DO IY = 1, LASTYR
           T66( 1,IY,IS) = OGEOYURR( 1, 2,IY)
           T66( 2,IY,IS) = OGEOYURR( 2, 2,IY)
           T66( 3,IY,IS) = OGEOYURR( 3, 2,IY)
           T66( 4,IY,IS) = OGEOYURR( 4, 2,IY)
           T66( 5,IY,IS) = OGEOYURR( 5, 2,IY)
           T66( 6,IY,IS) = OGEOYURR( 6, 2,IY)
           T66( 7,IY,IS) = OGEOYURR( 7, 2,IY)
           T66( 8,IY,IS) = OGEOYURR( 8, 2,IY)
           T66( 9,IY,IS) = OGEOYURR( 9, 2,IY)
           T66(10,IY,IS) = OGEOYURR(10, 2,IY)
           T66(11,IY,IS) = FSUM(T66( 1,IY,IS),7)
           T66(12,IY,IS) = FSUM(T66( 8,IY,IS),3)
           T66(13,IY,IS) = FSUM(T66( 1,IY,IS),10)
           T66(14,IY,IS) = OGEOYINF( 1, 2,IY)
           T66(15,IY,IS) = OGEOYINF( 2, 2,IY)
           T66(16,IY,IS) = OGEOYINF( 3, 2,IY)
           T66(17,IY,IS) = OGEOYINF( 4, 2,IY)
           T66(18,IY,IS) = OGEOYINF( 5, 2,IY)
           T66(19,IY,IS) = OGEOYINF( 6, 2,IY)
           T66(20,IY,IS) = OGEOYINF( 7, 2,IY)
           T66(21,IY,IS) = OGEOYINF( 8, 2,IY)
           T66(22,IY,IS) = OGEOYINF( 9, 2,IY)
           T66(23,IY,IS) = OGEOYINF(10, 2,IY)
           T66(24,IY,IS) = FSUM(T66(14,IY,IS),7)
           T66(25,IY,IS) = FSUM(T66(21,IY,IS),3)
           T66(26,IY,IS) = FSUM(T66(14,IY,IS),10)
           T66(27,IY,IS) = OGEOYUGR( 1, 1,IY)
           T66(28,IY,IS) = OGEOYUGR( 2, 1,IY)
           T66(29,IY,IS) = OGEOYUGR( 3, 1,IY)
           T66(30,IY,IS) = OGEOYUGR( 4, 1,IY)
           T66(31,IY,IS) = OGEOYUGR( 5, 1,IY)
           T66(32,IY,IS) = OGEOYUGR( 6, 1,IY)
           T66(33,IY,IS) = OGEOYUGR( 7, 1,IY)
           T66(34,IY,IS) = OGEOYUGR( 1, 2,IY)
           T66(35,IY,IS) = OGEOYUGR( 2, 2,IY)
           T66(36,IY,IS) = OGEOYUGR( 3, 2,IY)
           T66(37,IY,IS) = OGEOYUGR( 4, 2,IY)
           T66(38,IY,IS) = OGEOYUGR( 5, 2,IY)
           T66(39,IY,IS) = OGEOYUGR( 6, 2,IY)
           T66(40,IY,IS) = OGEOYUGR( 7, 2,IY)
           T66(41,IY,IS) = OGEOYUGR( 1, 3,IY)
           T66(42,IY,IS) = OGEOYUGR( 2, 3,IY)
           T66(43,IY,IS) = OGEOYUGR( 3, 3,IY)
           T66(44,IY,IS) = OGEOYUGR( 4, 3,IY)
           T66(45,IY,IS) = OGEOYUGR( 5, 3,IY)
           T66(46,IY,IS) = OGEOYUGR( 6, 3,IY)
           T66(47,IY,IS) = OGEOYUGR( 7, 3,IY)
           T66(48,IY,IS) = FSUM(T66(27,IY,IS),7)   ! tight gas
           T66(49,IY,IS) = FSUM(T66(34,IY,IS),7)   ! shale gas
           T66(50,IY,IS) = FSUM(T66(41,IY,IS),7)   ! coalbed methane
           T66(51,IY,IS) = FSUM(T66(27,IY,IS),21)     ! or FSUM(T66(48,IY,IS),3)
           T66(52,IY,IS) = T66(13,IY,IS) + T66(26,IY,IS) + T66(51,IY,IS)
           T66(57,IY,IS) = sum(OGEOYAD(1:MNL48N,IY))
           T66(53,IY,IS) = FSUM(OGEOYURR(11, 2,IY),3) + FSUM(OGEOYINF(11, 2,IY),3)
           T66(54,IY,IS) = FSUM(T66(52,IY,IS),2) + T66(57,IY,IS)
           T66(55,IY,IS) = OGEOYRSV(MNUMOR,2,IY)
           T66(56,IY,IS) = FSUM(T66(54,IY,IS),2)
        ENDDO

!  Table 67. Renewable Energy Capacity, Generation, and Consumption by EMM Region and Fuel

        DO 670 IY = 1,LASTYR
          DO 670 IR = 1,MNUMNR
!  Electric Utilities
!      --- Capacity
          T67( 1,IR,IY,IS) = UCAPHYU(IR,IY) + UCAPHYN(IR,IY) + UCAPHYC(IR,IY)
          T67( 2,IR,IY,IS) = UCAPGEU(IR,IY) + UCAPGEN(IR,IY) + UCAPGEC(IR,IY)
          T67( 3,IR,IY,IS) = UCAPMSU(IR,IY) + UCAPMSN(IR,IY) + UCAPMSC(IR,IY)
          T67( 4,IR,IY,IS) = UCAPWDU(IR,IY) + UCAPWDN(IR,IY) + UCAPWDC(IR,IY)
          T67( 5,IR,IY,IS) = UCAPSTU(IR,IY) + UCAPSTN(IR,IY) + UCAPSTC(IR,IY)
          T67( 6,IR,IY,IS) = UCAPPVU(IR,IY) + UCAPPVN(IR,IY) + UCAPPVC(IR,IY) + &
                             UCAPPTU(IR,IY) + UCAPPTN(IR,IY) + UCAPPTC(IR,IY)
          T67( 7,IR,IY,IS) = UCAPWNU(IR,IY) + UCAPWNN(IR,IY) + UCAPWNC(IR,IY) + &
                             UCAPWLU(IR,IY) + UCAPWLN(IR,IY) + UCAPWLC(IR,IY)
          T67( 8,IR,IY,IS) = UCAPWFU(IR,IY) + UCAPWFN(IR,IY) + UCAPWFC(IR,IY)
          T67( 9,IR,IY,IS) = FSUM(T67( 1,IR,IY,IS),8)
! Capacity additions, planned and unplanned
          IF (IY .GT. (CUMCAPADD - 1989)) THEN
          T67(70,IR,IY,IS) = UADDHYU(1,IR,IY) + UADDHYN(1,IR,IY) + &
                             UADDHYU(2,IR,IY) + UADDHYN(2,IR,IY) + &
                             UADDHYC(IR,IY)
          T67(71,IR,IY,IS) = UADDGEU(1,IR,IY) + UADDGEN(1,IR,IY) + &
                             UADDGEU(2,IR,IY) + UADDGEN(2,IR,IY) + &
                             UADDGEC(IR,IY)
          T67(72,IR,IY,IS) = UADDMSU(1,IR,IY) + UADDMSN(1,IR,IY) + &
                             UADDMSU(2,IR,IY) + UADDMSN(2,IR,IY) + &
                             UADDMSC(IR,IY)
          T67(73,IR,IY,IS) = UADDWDU(1,IR,IY) + UADDWDN(1,IR,IY) + &
                             UADDWDU(2,IR,IY) + UADDWDN(2,IR,IY) + &
                             UADDWDC(IR,IY)
          T67(74,IR,IY,IS) = UADDSTU(1,IR,IY) + UADDSTN(1,IR,IY) + &
                             UADDSTU(2,IR,IY) + UADDSTN(2,IR,IY) + &
                             UADDSTC(IR,IY)
          T67(75,IR,IY,IS) = UADDPVU(1,IR,IY) + UADDPVN(1,IR,IY) + &
                             UADDPVU(2,IR,IY) + UADDPVN(2,IR,IY) + &
                             UADDPVC(IR,IY)                      + &
                             UADDPTU(1,IR,IY) + UADDPTN(1,IR,IY) + &
                             UADDPTU(2,IR,IY) + UADDPTN(2,IR,IY) + &
                             UADDPTC(IR,IY)
          T67(76,IR,IY,IS) = UADDWNU(1,IR,IY) + UADDWNN(1,IR,IY) + &
                             UADDWNU(2,IR,IY) + UADDWNN(2,IR,IY) + &
                             UADDWNC(IR,IY)                      + &
                             UADDWLU(1,IR,IY) + UADDWLN(1,IR,IY) + &
                             UADDWLU(2,IR,IY) + UADDWLN(2,IR,IY) + &
                             UADDWLC(IR,IY)
          T67(77,IR,IY,IS) = UADDWFU(1,IR,IY) + UADDWFN(1,IR,IY) + &
                             UADDWFU(2,IR,IY) + UADDWFN(2,IR,IY) + &
                             UADDWFC(IR,IY)
          T67(78,IR,IY,IS) = FSUM(T67(70,IR,IY,IS),8)
     ELSE
if (ftabbone.eq.0) then          !  only do this if tables for publication
          T67(70:78,IR,IY,IS) = -999.
endif
     ENDIF
!      --- Generation
          T67(10,IR,IY,IS) = UGNHYNR(1,IR,IY) + UGNHYNR(2,IR,IY) + &
                            (CGNTGEN(IR,IY, 4,1) + CGNTGEN(IR,IY, 4,2)) * 0.001
          T67(11,IR,IY,IS) = UGNGENR(1,IR,IY) + UGNGENR(2,IR,IY) + &
                            (CGNTGEN(IR,IY, 5,1) + CGNTGEN(IR,IY, 5,2)) * 0.001
          T67(12,IR,IY,IS) = UGNMSNR(1,IR,IY) + UGNMSNR(2,IR,IY) + &
                            (CGNTGEN(IR,IY, 6,1) + CGNTGEN(IR,IY, 6,2)) * 0.001
          T67(13,IR,IY,IS) = UGNWDNR(1,IR,IY) + UGNWDNR(2,IR,IY) + &
                            (CGNTGEN(IR,IY, 7,1) + CGNTGEN(IR,IY, 7,2)) * 0.001
          T67(14,IR,IY,IS) = UGNSONR(1,IR,IY) + UGNSONR(2,IR,IY)
          T67(15,IR,IY,IS) = UGNPVNR(1,IR,IY) + UGNPVNR(2,IR,IY) + &
                             UGNPTNR(1,IR,IY) + UGNPTNR(2,IR,IY) + &
                            (CGNTGEN(IR,IY, 8,1) + CGNTGEN(IR,IY, 8,2)) * 0.001
          T67(16,IR,IY,IS) = UGNWNNR(1,IR,IY) + UGNWNNR(2,IR,IY) + &
                             UGNWLNR(1,IR,IY) + UGNWLNR(2,IR,IY)
          T67(17,IR,IY,IS) = UGNWFNR(1,IR,IY) + UGNWFNR(2,IR,IY)
          T67(18,IR,IY,IS) = FSUM(T67(10,IR,IY,IS),8)
!      --- Consumption
!         T67(19,IR,IY,IS) =(UFLHYNR(1,IR,IY) + UFLHYNR(2,IR,IY) + UFLPSNR(1,IR,IY) + UFLPSNR(2,IR,IY))/1000.
          T67(19,IR,IY,IS) =(UFLHYNR(1,IR,IY) + UFLHYNR(2,IR,IY))/1000.
          T67(20,IR,IY,IS) =(UFLGTNR(1,IR,IY) + UFLGTNR(2,IR,IY))/1000.
          T67(21,IR,IY,IS) =((UFLMSNR(1,IR,IY) + UFLMSNR(2,IR,IY))/1000.) - WNCMSELN(IY,IR)
          T67(22,IR,IY,IS) =(UFLWDNR(1,IR,IY) + UFLWDNR(2,IR,IY))/1000.
          T67(23,IR,IY,IS) =(UFLSONR(1,IR,IY) + UFLSONR(2,IR,IY))/1000.
          T67(24,IR,IY,IS) =(UFLPVNR(1,IR,IY) + UFLPVNR(2,IR,IY))/1000.
          T67(25,IR,IY,IS) =(UFLWNNR(1,IR,IY) + UFLWNNR(2,IR,IY))/1000.
          T67(26,IR,IY,IS) =(UFLWFNR(1,IR,IY) + UFLWFNR(2,IR,IY))/1000.

          T67(27,IR,IY,IS) = FSUM(T67(19,IR,IY,IS),8)
!     -- end use generators Capacity
          T67(28,IR,IY,IS) = CGTOTCAPNR(IR,IY,5) * 0.001
          T67(29,IR,IY,IS) = CGTOTCAPNR(IR,IY,6) * 0.001
          T67(30,IR,IY,IS) = CGTOTCAPNR(IR,IY,7) * 0.001
          T67(31,IR,IY,IS) = CGTOTCAPNR(IR,IY,8) * 0.001
          T67(32,IR,IY,IS) = CGTOTCAPNR(IR,IY,10) * EUPVCAPADJ(IY) * 0.001  ! adj PV
          T67(33,IR,IY,IS) = CGTOTCAPNR(IR,IY,12) * 0.001
          T67(34,IR,IY,IS) = FSUM(T67(28,IR,IY,IS),6)
!     -- end use generators Capacity additions
          IF (IY .GT. (CUMCAPADD - 1989)) THEN
             IF (IR .LE. MNUMNR-1) THEN
                T67(79,IR,IY,IS) = T67(28,IR,IY,IS) - T67(28,IR,(CUMCAPADD - 1989),IS)
                T67(80,IR,IY,IS) = T67(29,IR,IY,IS) - T67(29,IR,(CUMCAPADD - 1989),IS)
                T67(81,IR,IY,IS) = T67(30,IR,IY,IS) - T67(30,IR,(CUMCAPADD - 1989),IS)
                T67(82,IR,IY,IS) = T67(31,IR,IY,IS) - T67(31,IR,(CUMCAPADD - 1989),IS)
                T67(83,IR,IY,IS) = T67(32,IR,IY,IS) - T67(32,IR,(CUMCAPADD - 1989),IS)
                T67(84,IR,IY,IS) = T67(33,IR,IY,IS) - T67(33,IR,(CUMCAPADD - 1989),IS)
                T67(85,IR,IY,IS) = FSUM(T67(79,IR,IY,IS),6)
             ELSE
                T67(79,IR,IY,IS) = sum(T67(79,1:MNUMNR-1,IY,IS))
                T67(80,IR,IY,IS) = sum(T67(80,1:MNUMNR-1,IY,IS))
                T67(81,IR,IY,IS) = sum(T67(81,1:MNUMNR-1,IY,IS))
                T67(82,IR,IY,IS) = sum(T67(82,1:MNUMNR-1,IY,IS))
                T67(83,IR,IY,IS) = sum(T67(83,1:MNUMNR-1,IY,IS))
                T67(84,IR,IY,IS) = sum(T67(84,1:MNUMNR-1,IY,IS))
                T67(85,IR,IY,IS) = FSUM(T67(79,IR,IY,IS),6)
             ENDIF
          ELSE
if (ftabbone.eq.0) then          !  only do this if tables for publication
             T67(79:85,IR,IY,IS) = -999.
endif
          ENDIF
!     -- end use generators Generation
          T67(35,IR,IY,IS) = (CGTOTGENNR(IR,IY,5,1) + CGTOTGENNR(IR,IY,5,2) ) * 0.001
          T67(36,IR,IY,IS) = (CGTOTGENNR(IR,IY,6,1) + CGTOTGENNR(IR,IY,6,2) ) * 0.001
          T67(37,IR,IY,IS) = (CGTOTGENNR(IR,IY,7,1) + CGTOTGENNR(IR,IY,7,2) ) * 0.001
          T67(38,IR,IY,IS) = (CGTOTGENNR(IR,IY,8,1) + CGTOTGENNR(IR,IY,8,2) ) * 0.001
          T67(39,IR,IY,IS) = (CGTOTGENNR(IR,IY,10,1) + CGTOTGENNR(IR,IY,10,2) ) * EUPVGENADJ(IY) * 0.001    ! adj PV
          T67(40,IR,IY,IS) = (CGTOTGENNR(IR,IY,12,1) + CGTOTGENNR(IR,IY,12,2) ) * 0.001
          T67(41,IR,IY,IS) = FSUM(T67(35,IR,IY,IS),6)
!     -- end use generators Consumption
          T67(42,IR,IY,IS) = CGTOTQNR(IR,IY,5) * 0.001
          T67(43,IR,IY,IS) = CGTOTQNR(IR,IY,6) * 0.001
          T67(44,IR,IY,IS) = CGTOTQNR(IR,IY,7) * 0.001
          T67(45,IR,IY,IS) = CGTOTQNR(IR,IY,8) * 0.001
          T67(46,IR,IY,IS) = CGTOTQNR(IR,IY,10) * EUPVGENADJ(IY) * 0.001 !adj PV consistent with gen
          T67(47,IR,IY,IS) = CGTOTQNR(IR,IY,12) * 0.001
          T67(48,IR,IY,IS) = FSUM(T67(42,IR,IY,IS),6)
!     -- all sectors Capacity
          T67(49,IR,IY,IS) = T67( 1,IR,IY,IS) + T67(28,IR,IY,IS)
          T67(50,IR,IY,IS) = T67( 2,IR,IY,IS) + T67(29,IR,IY,IS)
          T67(51,IR,IY,IS) = T67( 3,IR,IY,IS) + T67(30,IR,IY,IS)
          T67(52,IR,IY,IS) = T67( 4,IR,IY,IS) + T67(31,IR,IY,IS)
          T67(53,IR,IY,IS) = T67( 5,IR,IY,IS) + T67( 6,IR,IY,IS) + T67(32,IR,IY,IS)
          T67(54,IR,IY,IS) = T67( 7,IR,IY,IS) + T67( 8,IR,IY,IS) + T67(33,IR,IY,IS)
          T67(55,IR,IY,IS) = FSUM(T67(49,IR,IY,IS),6)
!     -- all sectors Generation
          T67(56,IR,IY,IS) = T67(10,IR,IY,IS) + T67(35,IR,IY,IS)
          T67(57,IR,IY,IS) = T67(11,IR,IY,IS) + T67(36,IR,IY,IS)
          T67(58,IR,IY,IS) = T67(12,IR,IY,IS) + T67(37,IR,IY,IS)
          T67(59,IR,IY,IS) = T67(13,IR,IY,IS) + T67(38,IR,IY,IS)
          T67(60,IR,IY,IS) = T67(14,IR,IY,IS) + T67(15,IR,IY,IS) + T67(39,IR,IY,IS)
          T67(61,IR,IY,IS) = T67(16,IR,IY,IS) + T67(17,IR,IY,IS) + T67(40,IR,IY,IS)
          T67(62,IR,IY,IS) = FSUM(T67(56,IR,IY,IS),6)
!     -- all sectors Consumption
          T67(63,IR,IY,IS) = T67(19,IR,IY,IS) + T67(42,IR,IY,IS)
          T67(64,IR,IY,IS) = T67(20,IR,IY,IS) + T67(43,IR,IY,IS)
          T67(65,IR,IY,IS) = T67(21,IR,IY,IS) + T67(44,IR,IY,IS)
          T67(66,IR,IY,IS) = T67(22,IR,IY,IS) + T67(45,IR,IY,IS)
          T67(67,IR,IY,IS) = T67(23,IR,IY,IS) + T67(24,IR,IY,IS) + T67(46,IR,IY,IS)
          T67(68,IR,IY,IS) = T67(25,IR,IY,IS) + T67(26,IR,IY,IS) + T67(47,IR,IY,IS)
          T67(69,IR,IY,IS) = FSUM(T67(63,IR,IY,IS),6)

 670   CONTINUE

!     Table 68. Domestic Refinery Distillation Base Capacity, Expansion, and Utilization

         NUMREP=5                       ! equals number of rows per PADD
         DO IY = 1,LASTYR
           DO PADD=1,MNUMPR
             T68(((PADD-1)*NUMREP)+1,IY,IS) = REF_CAP(1,PADD,CUMCAPADD-1989) / 1000.
             T68(((PADD-1)*NUMREP)+2,IY,IS) =(REF_CAP(1,PADD,IY) - REF_CAP(1,PADD,CUMCAPADD-1989))/ 1000.
             T68(((PADD-1)*NUMREP)+3,IY,IS) = REF_CAP(1,PADD,IY) * (1-REF_UTL(1,PADD,IY)) / 1000.
             T68(((PADD-1)*NUMREP)+4,IY,IS) = REF_CAP(1,PADD,IY) / 1000.
             T68(((PADD-1)*NUMREP)+5,IY,IS) = REF_UTL(1,PADD,IY) * 100.

             T68(PADD+50,IY,IS) = PROFIT_BBL(PADD,IY)
           ENDDO
         ENDDO

!  T69(    No table 69

! this call needs to be after Table 17 and before Table 70 (see comment about 10 lines down)
      CALL DOT118(IS)   ! Does both table 118 and Table 119 (unadjusted Table 3 prices)

!     Table 70. COMPONENTS OF SELECTED PETROLEUM PRODUCT PRICES

         DO IY=1,LASTYR
           DO IR = 1, MNUMCR
!            --- World Oil Price
             T70(57,IR,IY,IS) = BRENT_PRICE(IY) / 42.    ! same WTI price for every region
             T70(19,IR,IY,IS) = WTI_PRICE(IY) / 42.      ! same WTI price for every region
             T70(33,IR,IY,IS) = IT_WOP(IY,1) / 42.       ! same IRAC for every region

!  Note:    T70(50, T70(51, T70(52, T70(53 done in DOT118

!            --- Transportation Diesel
             T70( 1,IR,IY,IS) = PDSTRHWY(IR,IY) * CFDSTRHWY(IY) / 42.
             T70( 2,IR,IY,IS) = MUFTAX(IY,10) / MC_JPGDP(IY) * CFDSTRHWY(IY) / 42.
             T70( 3,IR,IY,IS) = DSMUTR(IR,IY,2) * CFDSTRHWY(IY) / 42.
             T70( 4,IR,IY,IS) = DSMUTR(IR,IY,1) * CFDSTRHWY(IY) / 42.
             T70( 5,IR,IY,IS) = T70( 1,IR,IY,IS) - FSUM(T70( 2,IR,IY,IS),3) - T70(50,IR,IY,IS)
             T70(20,IR,IY,IS) = T70(5,IR,IY,IS) - WTI_PRICE(IY) / 42.      ! WTI Margin
             T70(27,IR,IY,IS) = T70(5,IR,IY,IS) - IT_WOP(IY,1) / 42.       ! Imported RAC margin
             T70(45,IR,IY,IS) = RFSDSTR(IR,IY)/42.
             T70(32,IR,IY,IS) = T70( 5,IR,IY,IS) - T70(45,IR,IY,IS)    ! marginal production cost

!            --- Motor Gasoline
             T70( 6,IR,IY,IS) = PMGAS(IR,IY) * CFMGQ(IY) / 42.
             T70( 7,IR,IY,IS) = MUFTAX(IY,2) / MC_JPGDP(IY) * CFMGQ(IY) / 42.
             T70( 8,IR,IY,IS) = MGMUTR(IR,IY,2) * CFMGQ(IY) / 42.
             T70( 9,IR,IY,IS) = MGMUTR(IR,IY,1) * CFMGQ(IY) / 42.
             T70(10,IR,IY,IS) = T70( 6,IR,IY,IS) - FSUM(T70( 7,IR,IY,IS),3) - T70(51,IR,IY,IS)
             T70(21,IR,IY,IS) = T70(10,IR,IY,IS) - WTI_PRICE(IY) / 42.      ! WTI Margin
             T70(28,IR,IY,IS) = T70(10,IR,IY,IS) - IT_WOP(IY,1) / 42.       ! Imported RAC margin
             T70(46,IR,IY,IS) = RFSMGTR(IR,IY)/42.
             T70(35,IR,IY,IS) = T70(10,IR,IY,IS) - T70(46,IR,IY,IS)    ! marginal production cost

!            --- RBOB
             T70(37,IR,IY,IS) = (WS_RBOB(IR,IY) + RFENVFX(IR,IY,2)) / 42.
             T70(38,IR,IY,IS) = T70(37,IR,IY,IS) - WTI_PRICE(IY) / 42.
             T70(54,IR,IY,IS) = T70(37,IR,IY,IS) - IT_WOP(IY,1) / 42.       ! Imported RAC margin
             T70(47,IR,IY,IS) = RFSRBOB(IR,IY)/42.

!            --- Jet Fuel
             T70(11,IR,IY,IS) = PJFTR(IR,IY) * CFJFK / 42.
             T70(12,IR,IY,IS) = MUFTAX(IY,3) / MC_JPGDP(IY) * CFJFK / 42.
             T70(13,IR,IY,IS) = JFMUTR(IR,IY,2) * CFJFK / 42.
             T70(14,IR,IY,IS) = JFMUTR(IR,IY,1) * CFJFK / 42.
             T70(15,IR,IY,IS) = T70(11,IR,IY,IS) - FSUM(T70(12,IR,IY,IS),3) - T70(52,IR,IY,IS)
             T70(22,IR,IY,IS) = T70(15,IR,IY,IS) - WTI_PRICE(IY) / 42.      ! WTI Margin
             T70(29,IR,IY,IS) = T70(15,IR,IY,IS) - IT_WOP(IY,1) / 42.       ! Imported RAC margin
             T70(48,IR,IY,IS) = RFSJFTR(IR,IY)/42.
             T70(36,IR,IY,IS) = T70(15,IR,IY,IS) - T70(48,IR,IY,IS)    ! marginal production cost

!            --- Residential Distillate Fuel/ Heating Oil
             T70(16,IR,IY,IS) = PDSRS(IR,IY) * CFDSRS(IY) / 42.
             T70(17,IR,IY,IS) = DSMURS(IR,IY,1) * CFDSRS(IY) / 42.
             T70(18,IR,IY,IS) = T70(16,IR,IY,IS) - FSUM(T70(17,IR,IY,IS),1) - T70(53,IR,IY,IS)
             T70(23,IR,IY,IS) = T70(18,IR,IY,IS) - WTI_PRICE(IY) / 42.      ! WTI Margin
             T70(30,IR,IY,IS) = T70(18,IR,IY,IS) - IT_WOP(IY,1) / 42.       ! Imported RAC margin
             T70(49,IR,IY,IS) = RFSDSRS(IR,IY)/42.
             T70(40,IR,IY,IS) = T70(18,IR,IY,IS) - T70(49,IR,IY,IS)    ! marginal production cost

!            Residual fuel oil, no taxes
             T70(34,IR,IY,IS) = PRSAS(IR,IY) * CFRSQ / 42.
             T70(26,IR,IY,IS) = PRSAS(IR,IY) * CFRSQ / 42. - WTI_PRICE(IY) / 42.
             T70(31,IR,IY,IS) = PRSAS(IR,IY) * CFRSQ / 42. - IT_WOP(IY,1) / 42.

!            Crack spreads
                T70(24,IR,IY,IS) = (T70(37, 2,IY,IS)*2*42 + T70(18, 2,IY,IS)*42 - WTI_PRICE(IY)*3)/3
                T70(25,IR,IY,IS) = (T70(37,IR,IY,IS)*2*42 + T70(18,IR,IY,IS)*42 - WTI_PRICE(IY)*3)/3

             T70(41,IR,IY,IS) = RFSCREDPRC(1,IY)/42.
             T70(42,IR,IY,IS) = RFSCREDPRC(2,IY)/42.
             T70(43,IR,IY,IS) = RFSCREDPRC(3,IY)/42.
             T70(44,IR,IY,IS) = RFSCREDPRC(4,IY)/42.

             T70(55,IR,IY,IS) = E85ICCREDIT(IY)/42.

             T70(56,IR,IY,IS) = PROFIT_BBL(10,IY)/42.

           ENDDO
         ENDDO

!     Table 71. Crude Oil production and Wellhead Prices by Region

         RDAYS=365.
         DO IY=1,LASTYR
!          --- PRODUCTION
           T71(1:10,IY,IS) = OGCOPRD(1:MNL48T-1,IY)
           T71(23,IY,IS) = FSUM(T71(1,IY,IS),7)
           T71(24,IY,IS) = FSUM(T71(8,IY,IS),3)
           T71(115,IY,IS) = FSUM(T71(1,IY,IS),10)
           T71(21,IY,IS) = OGPRDOFF(1,1,IY)/RDAYS
           T71(25,IY,IS) = OGCOPRDGOM(1,IY) - OGPRDOFF(1,1,IY)/RDAYS
           T71(22,IY,IS) = OGCOPRDGOM(2,IY)
           T71(26,IY,IS) = OGPRDOFF(2,1,IY)/RDAYS
           T71(27,IY,IS) = T71(9,IY,IS) - T71(26,IY,IS)
           T71(28,IY,IS) = OGPRDOFF(3,1,IY)/RDAYS
           T71(29,IY,IS) = T71(10,IY,IS) - T71(28,IY,IS)
!          --- WELLHEAD PRICES
           T71(11:20,IY,IS) = OGCOWHP(1:MNL48T-1,IY)
           T71(116,IY,IS) = OGCOWHP(MNL48T,IY)
!  Bonus rows, production by OGSM district
           T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
           T71(30+OGDIST,IY,IS) = FSUM(T71(30,IY,IS),OGDIST)
         ENDDO

!     Table 72. Lower 48 Natural Gas Production and Wellhead Prices by Region

         DO IY=1,LASTYR
! PRODUCTION
           T72(1:10,IY,IS) = OGNGPRD(1:10,IY) * .001
           T72(117,IY,IS) = OGNGPRD(11,IY) * .001
           T72(25,IY,IS) = FSUM(T72(1,IY,IS),7)
           T72(26,IY,IS) = FSUM(T72(8,IY,IS),3)
           T72(21,IY,IS) = OGPRDOFF(1,2,IY) / 1000.
           T72(27,IY,IS) = OGNGPRDGOM(1,IY) - OGPRDOFF(1,2,IY) / 1000.
           T72(22,IY,IS) = OGNGPRDGOM(2,IY)
           T72(28,IY,IS) = OGPRDOFF(2,2,IY) / 1000.
           T72(29,IY,IS) = T72(9,IY,IS) - T72(28,IY,IS)
           T72(30,IY,IS) = OGPRDOFF(3,2,IY) / 1000.
           T72(31,IY,IS) = T72(10,IY,IS) - T72(30,IY,IS)
! WELLHEAD PRICES
           T72(23,IY,IS) = OGHHPRNG(IY)
           T72(24,IY,IS) = OGNGWHP(11,IY) / CFNGC(IY)
           T72(11:20,IY,IS) = OGNGWHP(1:10,IY)
           T72(118,IY,IS) = OGNGWHP(11,IY)
if (ftabbone.eq.0) then           !  only do this if tables are elevated to publish level (no bonus rows)
           IF (T72(10,IY,IS) .EQ. 0.0) T72(20,IY,IS) = -999.
endif
!  Bonus rows, production by OGSM district
           DO IR=1,GASTYPES-1
              T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,IR,IY)/1000.
           ENDDO
           T72(32+OGDIST,IY,IS) = FSUM(T72(32,IY,IS),OGDIST)
         ENDDO

!     Table 73. Oil and Gas Reserves  removing calculations for this table T73(


!     Table 74. Lower 48 successful well completions
!  Lower 48 successful wells (sum(ogogwells(ogdist,1-6,mnumyr)))
!    by onshore district (1-66 minus Alaska (3) and Hawaii (13))

         DO IY = 1,LASTYR
!  Filling with all districts, will exclude Alaska, Hawaii, and offshore via table stubs.
          DO IR =1,OGDIST
             T74(IR,IY,IS) = sum(OGOGWELLS(IR,1:6,IY))
          ENDDO
!  Total is onshore only.  Subtract Alaska (3) and Hawaii (13)
           T74(OGDIST+1,IY,IS) = FSUM(T74( 1,IY,IS),66) - T74( 3,IY,IS) - T74(13,IY,IS)
!  Offshore lower 48 total = total offshore (67 through 84) minus Alaska (75 and 84)
           T74(OGDIST+2,IY,IS) = FSUM(T74(67,IY,IS),18) - T74(75,IY,IS) - T74(84,IY,IS)
!  Other subtotals
           T74(OGDIST+3,IY,IS) = T74(75,IY,IS) + T74(84,IY,IS)           ! Alaska offshore
           T74(OGDIST+4,IY,IS) = T74(13,IY,IS) + T74(OGDIST+3,IY,IS)     ! Alaska
           T74(OGDIST+5,IY,IS) = FSUM(T74( 1,IY,IS),84)                  ! total successful wells
!  drilling information
           T74(OGDIST+6,IY,IS) = (EXFTAGE(4,IY)+DVFTAGE(4,IY)) / 1000.         ! Total lower 48 drilling
           T74(OGDIST+7,IY,IS) = (EXOILFT(4,IY)+DVOILFT(4,IY)) / 1000.         ! Oil formations
           T74(OGDIST+8,IY,IS) = (EXGASFT(4,IY)+DVGASFT(4,IY)) / 1000.         ! Gas formations
           T74(OGDIST+9,IY,IS) = (EXDRYFT(4,IY)+DVDRYFT(4,IY)) / 1000.         ! Dry holes
         ENDDO

!     Table 75. Technology Cost for Light-Duty Vehicles

      DO IY = 1,LASTYR         ! AVGCOST gets converted from 1998 dollars to 2000 dollars
         T75(1:86,IY,IS) = AVGCOST(1,1:86,IY+BASEYR-1) / MC_JPGDP(1) * MC_JPGDP(11)      ! cars
         T75(87:172,IY,IS) = AVGCOST(2,1:86,IY+BASEYR-1) / MC_JPGDP(1) * MC_JPGDP(11)    ! light trucks
         T75(173:258,IY,IS) = AVGCOST(3,1:86,IY+BASEYR-1) / MC_JPGDP(1) * MC_JPGDP(11)   ! total
if (ftabbone.eq.0) &              !  only do this if tables are elevated to publish level (no bonus rows)
         T75(175,IY,IS) = -999.   !  n/a mass reduction ii:  different definition for cars and trucks
      ENDDO

!     Table 76. Natural Gas Imports and Exports

         DO IY = 1,LASTYR
!          --- VOLUMES
           T76( 1,IY,IS) = NGIMPVOL(1,IY) * .001
           T76( 2,IY,IS) = NGIMPVOL(2,IY) * .001
           T76( 3,IY,IS) = NGIMPVOL(3,IY) * .001
           T76( 4,IY,IS) = NGEXPVOL(1,IY) * .001
           T76( 5,IY,IS) = NGEXPVOL(2,IY) * .001
           T76( 6,IY,IS) = NGEXPVOL(3,IY) * .001
           T76( 7,IY,IS) = T76( 1,IY,IS) - T76( 4,IY,IS)
           T76( 8,IY,IS) = T76( 2,IY,IS) - T76( 5,IY,IS)
           T76( 9,IY,IS) = T76( 3,IY,IS) - T76( 6,IY,IS)
           T76(10,IY,IS) = FSUM(T76( 1,IY,IS),3)
           T76(11,IY,IS) = FSUM(T76( 4,IY,IS),3)
           T76(12,IY,IS) = FSUM(T76( 7,IY,IS),3)
!          --- PRICES
           T76(13,IY,IS) = NGIMPPRC(4,IY)
           T76(14,IY,IS) = NGIMPPRC(1,IY)
           T76(15,IY,IS) = NGIMPPRC(2,IY)
           T76(16,IY,IS) = NGIMPPRC(3,IY)

!   LNG terminals (regions) are:
!       1 = New England           2 = Mid Atlantic      3 = South Atlantic      4 = Florida
!       5 = Mississippi/Alabama   6 = Texas/Louisiana   7 = Washington/Oregon   8 = California

           T76(17,IY,IS) = NALNGEXP( 1,IY+BASEYR-1)
           T76(18,IY,IS) = NALNGEXP( 2,IY+BASEYR-1)
           T76(19,IY,IS) = NALNGEXP( 3,IY+BASEYR-1)
           T76(20,IY,IS) = NALNGEXP( 4,IY+BASEYR-1)
           T76(21,IY,IS) = NALNGEXP( 5,IY+BASEYR-1)
           T76(22,IY,IS) = NALNGEXP( 6,IY+BASEYR-1)
           T76(23,IY,IS) = NALNGEXP( 7,IY+BASEYR-1)
           T76(24,IY,IS) = NALNGEXP( 8,IY+BASEYR-1)
           T76(25,IY,IS) = NALNGEXP( 9,IY+BASEYR-1)
           T76(26,IY,IS) = NALNGEXP(10,IY+BASEYR-1)
           T76(27,IY,IS) = NALNGEXP(11,IY+BASEYR-1)
           T76(28,IY,IS) = FSUM(T76(17,IY,IS),11)
!  prices of natural gas in regions getting our liquefied natural gas exports
           T76(29,IY,IS) = PINTLNG(1,IY+BASEYR-1)    !  natural gas price over there in Europe
           T76(30,IY,IS) = PINTLNG(2,IY+BASEYR-1)    !  natural gas price over the other there in Japan
!  natural gas prices from terminals
           T76(31,IY,IS) = PUSNG(1,IY+BASEYR-1)
           T76(32,IY,IS) = PUSNG(2,IY+BASEYR-1)
           T76(33,IY,IS) = PUSNG(3,IY+BASEYR-1)
           T76(34,IY,IS) = PUSNG(4,IY+BASEYR-1)
           T76(35,IY,IS) = PUSNG(5,IY+BASEYR-1)
           T76(36,IY,IS) = PUSNG(6,IY+BASEYR-1)
           T76(37,IY,IS) = PUSNG(7,IY+BASEYR-1)
           T76(38,IY,IS) = PUSNG(8,IY+BASEYR-1)
! repeat the US LNG terminal prices adjusted with transportation costs:
! putting this in so that these lines will show up in grafnem for the time being, as PTRANSNG is Infinity in 2018
IF (ISNAN(PTRANSNG(1,1,IY+BASEYR-1)) .OR. PTRANSNG(1,1,IY+BASEYR-1) .GT. 1E32) PTRANSNG(1,1,IY+BASEYR-1) = 0.0
    ! to Europe
           T76(39,IY,IS) = PUSNG(1,IY+BASEYR-1) + PTRANSNG(1,1,IY+BASEYR-1)
           T76(40,IY,IS) = PUSNG(2,IY+BASEYR-1) + PTRANSNG(1,2,IY+BASEYR-1)
           T76(41,IY,IS) = PUSNG(3,IY+BASEYR-1) + PTRANSNG(1,3,IY+BASEYR-1)
           T76(42,IY,IS) = PUSNG(4,IY+BASEYR-1) + PTRANSNG(1,4,IY+BASEYR-1)
           T76(43,IY,IS) = PUSNG(5,IY+BASEYR-1) + PTRANSNG(1,5,IY+BASEYR-1)
           T76(44,IY,IS) = PUSNG(6,IY+BASEYR-1) + PTRANSNG(1,6,IY+BASEYR-1)
           T76(45,IY,IS) = PUSNG(7,IY+BASEYR-1) + PTRANSNG(1,7,IY+BASEYR-1)
           T76(46,IY,IS) = PUSNG(8,IY+BASEYR-1) + PTRANSNG(1,8,IY+BASEYR-1)
    ! to Japan
           T76(47,IY,IS) = PUSNG(1,IY+BASEYR-1) + PTRANSNG(2,1,IY+BASEYR-1)
           T76(48,IY,IS) = PUSNG(2,IY+BASEYR-1) + PTRANSNG(2,2,IY+BASEYR-1)
           T76(49,IY,IS) = PUSNG(3,IY+BASEYR-1) + PTRANSNG(2,3,IY+BASEYR-1)
           T76(50,IY,IS) = PUSNG(4,IY+BASEYR-1) + PTRANSNG(2,4,IY+BASEYR-1)
           T76(51,IY,IS) = PUSNG(5,IY+BASEYR-1) + PTRANSNG(2,5,IY+BASEYR-1)
           T76(52,IY,IS) = PUSNG(6,IY+BASEYR-1) + PTRANSNG(2,6,IY+BASEYR-1)
           T76(53,IY,IS) = PUSNG(7,IY+BASEYR-1) + PTRANSNG(2,7,IY+BASEYR-1)
           T76(54,IY,IS) = PUSNG(8,IY+BASEYR-1) + PTRANSNG(2,8,IY+BASEYR-1)
         ENDDO

!  Table 77  Natural Gas Consumption by End-Use Sector and Region

        DO 770 IY=1,LASTYR
!         --- RESIDENTIAL, COMMERCIAL, INDUSTRIAL, and TRANSPORTATION
          IF (CFNGN(IY) .NE. 0.0) THEN
          T77(1:9,IY,IS) = QNGRS(1:9,IY)/CFNGN(IY)
          T77(11:19,IY,IS) = QNGCM(1:9,IY)/CFNGN(IY)
!             Industrial excludes lease and plant fuel and pipeline use and gas-to-liquids
          T77(21:29,IY,IS) =(QNGIN(1:9,IY)-QGTLRF(1:9,IY))/CFNGN(IY)
          T77(41:49,IY,IS) = QNGTR(1:9,IY)/CFNGN(IY)
          ENDIF
          T77(10,IY,IS) = FSUM(T77(1,IY,IS),9)
          T77(20,IY,IS) = FSUM(T77(11,IY,IS),9)
          T77(30,IY,IS) = FSUM(T77(21,IY,IS),9)
          T77(50,IY,IS) = FSUM(T77(41,IY,IS),9)
!         --- ELECTRIC UTILITIES
          IF (CFNGU(IY) .NE. 0.0) T77(31:39,IY,IS) = QNGEL(1:9,IY)/CFNGU(IY)
          T77(40,IY,IS) = FSUM(T77(31,IY,IS),9)
!         --- ALL SECTORS
          DO IR=1,9
            T77(50+IR,IY,IS) = T77(IR,IY,IS) + T77(IR+10,IY,IS) + T77(IR+20,IY,IS) + &
                 T77(IR+30,IY,IS) + T77(IR+40,IY,IS)
          ENDDO
          T77(60,IY,IS) = FSUM(T77(51,IY,IS),9)
 770    CONTINUE

!  Table 78  Natural Gas Prices by End-Use Sector and Region

        DO 780 IY=1,LASTYR
!         --- RESIDENTIAL
          T78(1:9,IY,IS) = PNGRS(1:9,IY)*CFNGN(IY)
          T78(10,IY,IS) = PNGRS(11,IY)*CFNGN(IY)
!         --- COMMERCIAL
          T78(11:19,IY,IS) = PNGCM(1:9,IY)*CFNGN(IY)
          T78(20,IY,IS) = PNGCM(11,IY)*CFNGN(IY)
!         --- INDUSTRIAL
          T78(21:29,IY,IS) = PNGIN(1:9,IY)*CFNGN(IY)
          T78(30,IY,IS) = PNGIN(11,IY)*CFNGN(IY)
!         --- ELECTRIC UTILITIES
          T78(31:39,IY,IS) = PNGEL(1:9,IY)*CFNGU(IY)
          T78(40,IY,IS) = PNGEL(11,IY)*CFNGU(IY)
!         --- TRANSPORTATION
          T78(41:49,IY,IS) = PNGTR(1:9,IY)*CFNGN(IY)
          T78(50,IY,IS) = PNGTR(11,IY)*CFNGN(IY)
!         --- ALL SECTORS
          T78(51:59,IY,IS) = PNGAS(1:9,IY)*CFNGC(IY)
          T78(60,IY,IS) = PNGAS(11,IY)*CFNGC(IY)
 780    CONTINUE

!     Table 79.  Lower 48 Onshore Crude Oil Production

         DO IY = 1,LASTYR
           T79( 1:15,IY,IS) = OGQSHLOIL( 1:15,IY)
           T79(11,IY,IS) = sum(OGQSHLOIL(11:15,IY))    ! overwrite "other" position with sum of the rest
           T79(16:22,IY,IS) = OGREGPRD(1:7,1,IY)
           T79(23:29,IY,IS) = OGREGPRD(1:7,2,IY)
           T79(30:36,IY,IS) = T79(16:22,IY,IS) + T79(23:29,IY,IS)
           T79(37,IY,IS) = FSUM(T79(16,IY,IS),7)
           T79(38,IY,IS) = FSUM(T79(23,IY,IS),7)
           T79(39,IY,IS) = FSUM(T79(30,IY,IS),7)
           T79(40,IY,IS) = SUM(OGQSHLOIL( 1:15,IY))
         ENDDO

!     Table 80.  Lower 48 Onshore Natural Gas Production

         DO IY = 1,LASTYR
           T80( 1:15,IY,IS) = OGQSHLGAS( 1:15,IY)
           T80(10,IY,IS) = sum(OGQSHLGAS(10:15,IY))    ! overwrite "other" position with sum of the rest
           T80(16:22,IY,IS) = OGREGPRD(1:7,3,IY) + OGREGPRD(1:7,4,IY)
           T80(23:29,IY,IS) = OGREGPRD(1:7,5,IY)
           T80(30:36,IY,IS) = OGREGPRD(1:7,6,IY)
           T80(37:43,IY,IS) = OGREGPRD(1:7,7,IY)
           T80(44:50,IY,IS) = T80(16:22,IY,IS) + T80(23:29,IY,IS) + T80(30:36,IY,IS) +  &
                              T80(37:43,IY,IS)
           T80(51,IY,IS) = FSUM(T80(16,IY,IS),7)
           T80(52,IY,IS) = FSUM(T80(23,IY,IS),7)
           T80(53,IY,IS) = FSUM(T80(30,IY,IS),7)
           T80(54,IY,IS) = FSUM(T80(37,IY,IS),7)
           T80(55,IY,IS) = FSUM(T80(44,IY,IS),7)
           T80(56,IY,IS) = SUM(OGQSHLGAS( 1:15,IY))
         ENDDO

! currently no Table 81 -   T81(

! currently no Table 82 -   T82(

!     Table 83. Trace Report for Gas Price to Electric Generators
!
         DO IY = 1,LASTYR
           T83(1,IY,IS) = NGIMPPRC(4,IY)
           T83(2,IY,IS) = OGWPRNG(MNUMOR,IY)
           FFF = (NGIMPVOL(4,IY) + (OGQNGREP(1,IY) + OGQNGREP(2,IY) + &
                                    OGQNGREP(3,IY) + OGQNGREP(4,IY) + &
                                    OGQNGREP(5,IY) + OGQNGREP(6,IY) + &
                                    OGQNGREP(7,IY) + OGQNGREP(8,IY) + OGSHALENG(IY)))
           IF (FFF .NE. 0.0) &
            T83(3,IY,IS) = (NGIMPPRC(4,IY) * NGIMPVOL(4,IY) + OGWPRNG(MNUMOR,IY) * &
                           (OGQNGREP(1,IY) + OGQNGREP(2,IY) + &
                            OGQNGREP(3,IY) + OGQNGREP(4,IY) + &
                            OGQNGREP(5,IY) + OGQNGREP(6,IY) + &
                            OGQNGREP(7,IY) + OGQNGREP(8,IY) + OGSHALENG(IY))) / FFF

           T83( 4:19,IY,IS) = NGSPOT_EMM(1:16,IY)                         ! spot prices
           DO III=1,NNGEM-1
             IF (NGSPOT_EMM(III,IY) .NE. 0.00) &
              T83(19+III,IY,IS) = NGSPOT_EMM(III,IY) -  T83(3,IY,IS)      ! spot markup
           ENDDO
           T83(36:51,IY,IS) = UDTAR(1:16,IY)                              ! distribution tariff
           T83(52,IY,IS) = sum(SQNGELGR( 1,IY,1:3)) / CFNGU(IY)           ! electric power quantities
           T83(53,IY,IS) = sum(SQNGELGR( 2,IY,1:3)) / CFNGU(IY)           ! electric power quantities
           T83(54,IY,IS) = sum(SQNGELGR( 3,IY,1:3)) / CFNGU(IY)           ! electric power quantities
           T83(55,IY,IS) = sum(SQNGELGR( 4,IY,1:3)) / CFNGU(IY)           ! electric power quantities
           T83(56,IY,IS) = sum(SQNGELGR( 5,IY,1:3)) / CFNGU(IY)           ! electric power quantities
           T83(57,IY,IS) = sum(SQNGELGR( 6,IY,1:3)) / CFNGU(IY)           ! electric power quantities
           T83(58,IY,IS) = sum(SQNGELGR( 7,IY,1:3)) / CFNGU(IY)           ! electric power quantities
           T83(59,IY,IS) = sum(SQNGELGR( 8,IY,1:3)) / CFNGU(IY)           ! electric power quantities
           T83(60,IY,IS) = sum(SQNGELGR( 9,IY,1:3)) / CFNGU(IY)           ! electric power quantities
           T83(61,IY,IS) = sum(SQNGELGR(10,IY,1:3)) / CFNGU(IY)           ! electric power quantities
           T83(62,IY,IS) = sum(SQNGELGR(11,IY,1:3)) / CFNGU(IY)           ! electric power quantities
           T83(63,IY,IS) = sum(SQNGELGR(12,IY,1:3)) / CFNGU(IY)           ! electric power quantities
           T83(64,IY,IS) = sum(SQNGELGR(13,IY,1:3)) / CFNGU(IY)           ! electric power quantities
           T83(65,IY,IS) = sum(SQNGELGR(14,IY,1:3)) / CFNGU(IY)           ! electric power quantities
           T83(66,IY,IS) = sum(SQNGELGR(15,IY,1:3)) / CFNGU(IY)           ! electric power quantities
           T83(67,IY,IS) = sum(SQNGELGR(16,IY,1:3)) / CFNGU(IY)           ! electric power quantities
           T83(68:83,IY,IS) = PNGELGR(1:16,IY) * CFNGU(IY)                ! electric power prices
           T83(84:99,IY,IS) = PNGELGR(1:16,IY) * CFNGU(IY) - T83(3,IY,IS) ! electric power markup

!  average, NGSPOT section
           FFF=0.0
           DO III=1,NNGEM-1
             FFF = FFF + NGSPOT_EMM(III,IY) * sum(SQNGELGR(III,IY,1:3))
           ENDDO
           IF (sum(SQNGELGR(1:NNGEM-1,IY,1:3)) .NE. 0.0) THEN
               T83(100,IY,IS) = FFF / sum(SQNGELGR(1:NNGEM-1,IY,1:3))
           ELSE
               T83(100,IY,IS) = 0.0
           ENDIF

!  average spot markups
           IF (T83(100,IY,IS) .NE. 0.00) THEN
              T83(101,IY,IS) = T83(100,IY,IS) - T83(3,IY,IS)
              T83(102,IY,IS) = T83(100,IY,IS) - T83(2,IY,IS)
           ENDIF

!  average electric power distributor tariff
           FFF = 0.0
           DO III=1,NNGEM-1
             FFF = FFF + T83(III+35,IY,IS) * sum(SQNGELGR(III,IY,1:3))
           ENDDO
           IF (sum(SQNGELGR(1:NNGEM-1,IY,1:3)) .NE. 0.0) THEN
               T83(103,IY,IS) = FFF / sum(SQNGELGR(1:NNGEM-1,IY,1:3))
           ELSE
               T83(103,IY,IS) = 0.0
           ENDIF

!  electric power consumption total
           T83(104,IY,IS) = FSUM(T83(52,IY,IS),NNGEM-1)

!  electric power sector price average
           FFF=0.0
           DO III=1,NNGEM-1
             FFF = FFF + SPNGELGR(III,IY,1) * SQNGELGR(III,IY,1) + &
                         SPNGELGR(III,IY,2) * SQNGELGR(III,IY,2) + &
                         SPNGELGR(III,IY,3) * SQNGELGR(III,IY,3)
           ENDDO
           IF ((T83(104,IY,IS) * CFNGU(IY)) .NE. 0.0) THEN
               T83(105,IY,IS) = FFF / (T83(104,IY,IS) * CFNGU(IY))
           ELSE
               T83(105,IY,IS) = 0.0
           ENDIF

!  electric power markup average
           IF (T83(105,IY,IS) .NE. 0.00) THEN
              T83(107,IY,IS) = T83(105,IY,IS) - T83(3,IY,IS)
              T83(108,IY,IS) = T83(105,IY,IS) - T83(2,IY,IS)
           ENDIF
         ENDDO

!        --- Avg Gas Prices to EL Gen weighted by 2015 elec cons
!  do in separate loop because pre-2015 averages need to be calculated using a number that wouldn't be calculated yet if done in the same loop
         DO IY = 1,LASTYR
           FFF=0.0
           DO III=1,NNGEM-1
             FFF = FFF + SPNGELGR(III,IY,1) * SQNGELGR(III,26,1) + &
                         SPNGELGR(III,IY,2) * SQNGELGR(III,26,2) + &
                         SPNGELGR(III,IY,3) * SQNGELGR(III,26,3)
           ENDDO
           IF ((T83(104,26,IS) * CFNGU(IY)) .NE. 0.0) THEN
               T83(106,IY,IS) = FFF /  (T83(104,26,IS) * CFNGU(IY))
           ELSE
               T83(106,IY,IS) = 0.0
           ENDIF
           T83(109,IY,IS) = T83(106,IY,IS) - T83(2,IY,IS)
         ENDDO

! currently no Table 84 -   T84(

! currently no Table 85 -   T85(

! currently no Table 86 -   T86(

! currently no Table 87 -   T87(

! currently no Table 88 -   T88(

! currently no Table 89 -   T89(


!  Table 90.  Natural Gas Flows Entering NGTDM Regions from Neighboring Regions

      DO IY=1,LASTYR
        T90( 1,IY,IS) = NGFLOWS( 1,1,IY)
        T90( 2,IY,IS) = NGFLOWS( 1,2,IY)
        T90( 3,IY,IS) = NGFLOWS( 2,1,IY)
        T90( 4,IY,IS) = NGFLOWS( 2,2,IY)
        T90( 5,IY,IS) = NGFLOWS( 2,3,IY)
        T90( 6,IY,IS) = NGFLOWS( 3,1,IY)
        T90( 7,IY,IS) = NGFLOWS( 3,2,IY)
        T90( 8,IY,IS) = NGFLOWS( 3,3,IY)
        T90( 9,IY,IS) = NGFLOWS( 3,4,IY)
        T90(10,IY,IS) = NGFLOWS( 3,5,IY)
        T90(11,IY,IS) = NGFLOWS( 4,1,IY)
        T90(12,IY,IS) = NGFLOWS( 4,2,IY)
        T90(13,IY,IS) = NGFLOWS( 4,3,IY)
        T90(14,IY,IS) = NGFLOWS( 5,1,IY)
        T90(15,IY,IS) = NGFLOWS( 5,2,IY)
        T90(16,IY,IS) = NGFLOWS( 6,1,IY)
        T90(17,IY,IS) = NGFLOWS( 6,2,IY)
        T90(18,IY,IS) = NGFLOWS( 6,3,IY)
        T90(19,IY,IS) = NGFLOWS( 6,4,IY)
        T90(20,IY,IS) = NGFLOWS( 7,1,IY)
        T90(21,IY,IS) = NGFLOWS( 7,2,IY)
        T90(22,IY,IS) = NGFLOWS( 7,3,IY)
        T90(23,IY,IS) = NGFLOWS( 7,4,IY)
        T90(24,IY,IS) = NGFLOWS( 7,5,IY)
        T90(25,IY,IS) = NGFLOWS( 8,1,IY)
        T90(26,IY,IS) = NGFLOWS( 8,2,IY)
        T90(27,IY,IS) = NGFLOWS( 8,3,IY)
        T90(28,IY,IS) = NGFLOWS( 9,1,IY)
        T90(29,IY,IS) = NGFLOWS( 9,2,IY)
        T90(30,IY,IS) = NGFLOWS(10,1,IY)
        T90(31,IY,IS) = NGFLOWS(10,2,IY)
        T90(32,IY,IS) = NGFLOWS(10,3,IY)
        T90(33,IY,IS) = NGFLOWS(11,1,IY)
        T90(34,IY,IS) = NGFLOWS(11,2,IY)
        T90(35,IY,IS) = NGFLOWS(11,3,IY)
      ENDDO

!  Table 91.  Natural Gas Capacity Entering NGTDM Regions from Neighboring Regions

      DO IY=1,LASTYR
        T91( 1,IY,IS) = NGCAPS( 1,1,IY)
        T91( 2,IY,IS) = NGCAPS( 1,2,IY)
        T91( 3,IY,IS) = NGCAPS( 2,1,IY)
        T91( 4,IY,IS) = NGCAPS( 2,2,IY)
        T91( 5,IY,IS) = NGCAPS( 2,3,IY)
        T91( 6,IY,IS) = NGCAPS( 3,1,IY)
        T91( 7,IY,IS) = NGCAPS( 3,2,IY)
        T91( 8,IY,IS) = NGCAPS( 3,3,IY)
        T91( 9,IY,IS) = NGCAPS( 3,4,IY)
        T91(10,IY,IS) = NGCAPS( 3,5,IY)
        T91(11,IY,IS) = NGCAPS( 4,1,IY)
        T91(12,IY,IS) = NGCAPS( 4,2,IY)
        T91(13,IY,IS) = NGCAPS( 4,3,IY)
        T91(14,IY,IS) = NGCAPS( 5,1,IY)
        T91(15,IY,IS) = NGCAPS( 5,2,IY)
        T91(16,IY,IS) = NGCAPS( 6,1,IY)
        T91(17,IY,IS) = NGCAPS( 6,2,IY)
        T91(18,IY,IS) = NGCAPS( 6,3,IY)
        T91(19,IY,IS) = NGCAPS( 6,4,IY)
        T91(20,IY,IS) = NGCAPS( 7,1,IY)
        T91(21,IY,IS) = NGCAPS( 7,2,IY)
        T91(22,IY,IS) = NGCAPS( 7,3,IY)
        T91(23,IY,IS) = NGCAPS( 7,4,IY)
        T91(24,IY,IS) = NGCAPS( 7,5,IY)
        T91(25,IY,IS) = NGCAPS( 8,1,IY)
        T91(26,IY,IS) = NGCAPS( 8,2,IY)
        T91(27,IY,IS) = NGCAPS( 8,3,IY)
        T91(28,IY,IS) = NGCAPS( 9,1,IY)
        T91(29,IY,IS) = NGCAPS( 9,2,IY)
        T91(30,IY,IS) = NGCAPS(10,1,IY)
        T91(31,IY,IS) = NGCAPS(10,2,IY)
        T91(32,IY,IS) = NGCAPS(10,3,IY)
        T91(33,IY,IS) = NGCAPS(11,1,IY)
        T91(34,IY,IS) = NGCAPS(11,2,IY)
        T91(35,IY,IS) = NGCAPS(11,3,IY)
      ENDDO

! Currently no Table 92 - T92(

! --- Table 93 Domestic Coal Supply Disposition and Prices

         DO 930 IY = 1,LASTYR
         DO 930 IR = 1,11
!          --- DISTRIBUTION
           T93( 1,IR,IY,IS) = COALPROD(IR,1,IY) + COALPROD(IR,2,IY) + COALPROD(IR,3,IY)
           T93( 2,IR,IY,IS) = COALPROD(IR,4,IY) + COALPROD(IR,5,IY) + COALPROD(IR,6,IY)
           T93( 3,IR,IY,IS) = COALPROD(IR,7,IY) + COALPROD(IR,8,IY) + &
                              COALPROD(IR,9,IY) + COALPROD(IR,10,IY)+ COALPROD(IR,11,IY)
           T93( 4,IR,IY,IS) = COALPROD(IR,12,IY) + COALPROD(IR,13,IY)+ COALPROD(IR,14,IY)
           T93( 5,IR,IY,IS) = FSUM(T93(1,IR,IY,IS),4)
           T93(21,IR,IY,IS) = sum(WC_DIST_ST(IR,1:MNUMLR,IY))
!          --- IMPORTS
           IF((CQDBFT(IR,7,IY) .NE.  0.0)) &
             T93( 6,IR,IY,IS) = CQDBFB(IR,7,IY)/CQDBFT(IR,7,IY)
!          --- TOTAL SUPPLY
           T93( 7,IR,IY,IS) = T93( 5,IR,IY,IS) + T93( 6,IR,IY,IS) + T93(21,IR,IY,IS)
!          --- CONSUMPTION
           IF(CQDBFT(IR,1,IY) .NE. 0.0) &
             T93(8,IR,IY,IS) = (QCLRS(IR,IY) + QCLCM(IR,IY)) / CQDBFT(IR,1,IY) * 1000.
           IF(CQDBFT(IR,2,IY) .NE. 0.0) THEN
             T93(9,IR,IY,IS)=(QCLIN(IR,IY)-QCTLRF(IR,IY))/CQDBFT(IR,2,IY)*1000.
           ENDIF
           IF(CQDBFT(IR,3,IY) .NE. 0.0) &
             T93(10,IR,IY,IS) = QMCIN(IR,IY) / CQDBFT(IR,3,IY) * 1000.
           IF(CQDBFT(IR,6,IY) .NE. 0.0) &
             T93(11,IR,IY,IS) = QCLEL(IR,IY) / CQDBFT(IR,6,IY) * 1000.
           IF(CQDBFT(IR,4,IY) .NE. 0.0) THEN
             T93(18,IR,IY,IS) = QCTLRF(IR,IY) / CQDBFT(IR,4,IY) * 1000.
             T93(19,IR,IY,IS) =(QCLSN(IR,IY)-QCTLRF(IR,IY))/ CQDBFT(IR,4,IY) * 1000.
           ENDIF
           T93(12,IR,IY,IS) = FSUM(T93(8,IR,IY,IS),4) + FSUM(T93(18,IR,IY,IS),2)
!          --- DISCREPANCY
           T93(13,IR,IY,IS) = T93(7,IR,IY,IS) - T93(12,IR,IY,IS)
!          --- PRICES
           T93(22,IR,IY,IS) = PCLCM(IR,IY) * CPDBFT(IR,1,IY)
           T93(20,IR,IY,IS) = PCLSN(IR,IY) * CPDBFT(IR,4,IY)
           T93(14,IR,IY,IS) = PCLIN(IR,IY) * CPDBFT(IR,2,IY)
           T93(15,IR,IY,IS) = PMCIN(IR,IY) * CPDBFT(IR,3,IY)
           T93(16,IR,IY,IS) = PCLEL(IR,IY) * CPDBFT(IR,6,IY)
           FFF = 0.0
           RRR = 0.0
           IF (CQDBFT(IR,1,IY) .NE. 0.0) THEN
              RRR = RRR + PCLCM(IR,IY)*(QCLCM(IR,IY))
              FFF = FFF + (QCLCM(IR,IY)) / CQDBFT(IR,1,IY)
           ENDIF
           IF (CQDBFT(IR,4,IY) .NE. 0.0) THEN
              RRR = RRR + PCLSN(IR,IY)*CPDBFT(IR,4,IY)*QCLSN(IR,IY)/CQDBFT(IR,4,IY) !Imports are not an option for qclsn
              FFF = FFF + QCLSN(IR,IY) / CQDBFT(IR,4,IY)
           ENDIF
           IF (CQDBFT(IR,2,IY) .NE. 0.0) THEN
              RRR = RRR + PCLIN(IR,IY)*(QCLIN(IR,IY)-QCTLRF(IR,IY))
              FFF = FFF +(QCLIN(IR,IY)-QCTLRF(IR,IY))/CQDBFT(IR,2,IY)
           ENDIF
           IF (CQDBFT(IR,3,IY) .NE. 0.0) THEN
              RRR = RRR + PMCIN(IR,IY)*(QMCIN(IR,IY))  !Total dollars without imports
              FFF = FFF + (QMCIN(IR,IY)) / CQDBFT(IR,3,IY)
           ENDIF
           IF (CQDBFT(IR,6,IY) .NE. 0.0) THEN
              RRR = RRR + PCLEL(IR,IY)*(QCLEL(IR,IY))
              FFF = FFF + (QCLEL(IR,IY)) / CQDBFT(IR,6,IY)
           ENDIF
           IF (FFF .NE. 0.0) T93(17,IR,IY,IS) = RRR / FFF
 930     CONTINUE

!     Table 94. Coal Production and Minemouth Prices by Region

         DO IY = 1,LASTYR
!          --- PRODUCTION
           T94( 1,IY,IS) = APPSTOCKS(IY) + ABSULF(4,IY) + APSULF(4,IY)
           T94( 2,IY,IS) = INTSTOCKS(IY) + IBSULF(4,IY) + ILSULF(4,IY)
           T94( 7,IY,IS) = WESTSTOCKS(IY) + WBSULF(4,IY) + WSSULF(4,IY) + WLSULF(4,IY) + WPSULF(4,IY)
! Dakota, Western Montana, Wyoming, Western Wyoming
           T94( 3,IY,IS) = WESTSTOCKS(IY) + CLSULF( 7,3,2,IY) + CLSULF( 8,1,1,IY) + CLSULF( 8,2,1,IY) + &
                        CLSULF( 8,2,2,IY) + CLSULF( 9,2,1,IY) + CLSULF(10,2,1,IY) + CLSULF( 9,2,2,IY) + &
                        CLSULF(11,1,1,IY) + CLSULF(11,2,1,IY) + CLSULF(11,2,2,IY)
! Rocky Mountain, Arizona/New Mexico, Washington/Alaska
           T94( 4,IY,IS) = CLSULF(12,4,1,IY) + CLSULF(12,1,1,IY) + CLSULF(12,2,1,IY) + &
                        CLSULF(13,1,1,IY) + CLSULF(13,1,2,IY) + CLSULF(13,2,2,IY) + CLSULF(14,2,1,IY)

!          --- EAST AND WEST OF THE MISSISSIPPI
      IF (CQSBT(1,IY) .NE. 0.0) T94( 8,IY,IS) = APPSTOCKS(IY) + INTSTOCKS(IY) + CQSBB(1,IY)/CQSBT(1,IY)
      IF (CQSBT(2,IY) .NE. 0.0) T94( 9,IY,IS) = WESTSTOCKS(IY) + CQSBB(2,IY)/CQSBT(2,IY)
!          --- U.S. TOTAL
           T94(10,IY,IS) = T94(8,IY,IS) + T94(9,IY,IS)

!          --- MINEMOUTH PRICES
! Appalachia
           IF ((ABSULF(4,IY) + APSULF(4,IY)) .NE. 0.0) &
             T94(11,IY,IS) = (PABSULF(4,IY) + PAPSULF(4,IY)) / (ABSULF(4,IY) + APSULF(4,IY))
! Interior
           IF ((IBSULF(4,IY) + ILSULF(4,IY)) .NE. 0.0) &
             T94(12,IY,IS) = (PIBSULF(4,IY) + PILSULF(4,IY)) / (IBSULF(4,IY) + ILSULF(4,IY))
! West
! Dakota, Western Montana, Wyoming, Western Wyoming
           IF ((CLSULF( 7,3,2,IY) + CLSULF( 8,1,1,IY) + CLSULF( 8,2,1,IY) + &
                CLSULF( 8,2,2,IY) + CLSULF( 9,2,1,IY) + CLSULF(10,2,1,IY) + CLSULF( 9,2,2,IY) + &
                CLSULF(11,1,1,IY) + CLSULF(11,2,1,IY) + CLSULF(11,2,2,IY)) .NE. 0.0) &
             T94(13,IY,IS) = (PCLSULF( 7,3,2,IY) + PCLSULF( 8,1,1,IY) + PCLSULF( 8,2,1,IY) + &
                              PCLSULF( 8,2,2,IY) + PCLSULF( 9,2,1,IY) + PCLSULF(10,2,1,IY) + &
                              PCLSULF( 9,2,2,IY) + PCLSULF(11,1,1,IY) + PCLSULF(11,2,1,IY) + &
                              PCLSULF(11,2,2,IY)) / &
               (CLSULF( 7,3,2,IY) + CLSULF( 8,1,1,IY) + CLSULF( 8,2,1,IY) + &
                CLSULF( 8,2,2,IY) + CLSULF( 9,2,1,IY) + CLSULF(10,2,1,IY) + CLSULF( 9,2,2,IY) + &
                CLSULF(11,1,1,IY) + CLSULF(11,2,1,IY) + CLSULF(11,2,2,IY))
! Rocky Mountain, Arizona/New Mexico, Washington/Alaska
           IF ((CLSULF(12,4,1,IY) + CLSULF(12,1,1,IY) + CLSULF(12,2,1,IY) + CLSULF(13,1,1,IY) + &
                CLSULF(13,1,2,IY) + CLSULF(13,2,2,IY) + CLSULF(14,2,1,IY)) .NE. 0.0) &
             T94(14,IY,IS) = (PCLSULF(12,4,1,IY) + PCLSULF(12,1,1,IY) + PCLSULF(12,2,1,IY) + &
                              PCLSULF(13,1,1,IY) + PCLSULF(13,1,2,IY) + &
                              PCLSULF(13,2,2,IY) + PCLSULF(14,2,1,IY)) / &
               (CLSULF(12,4,1,IY) + CLSULF(12,1,1,IY) + CLSULF(12,2,1,IY) + CLSULF(13,1,1,IY) + &
                CLSULF(13,1,2,IY) + CLSULF(13,2,2,IY) + CLSULF(14,2,1,IY))
! all West
           IF ((WBSULF(4,IY) + WSSULF(4,IY) + WLSULF(4,IY) + WPSULF(4,IY)) .NE. 0.0) &
             T94(17,IY,IS) = (PWBSULF(4,IY) + PWSSULF(4,IY) + PWLSULF(4,IY) + PWPSULF(4,IY)) / &
                             ( WBSULF(4,IY) +  WSSULF(4,IY) +  WLSULF(4,IY) +  WPSULF(4,IY))
! EAST OF THE MISSISSIPPI
           IF ((ABSULF(4,IY) + APSULF(4,IY) + CLSULF(4,3,2,IY) + &
                CLSULF(4,1,2,IY) + CLSULF(4,1,3,IY)) .NE. 0.0) &
             T94(18,IY,IS) = (PABSULF(4,IY) + PAPSULF(4,IY) + &
                              PCLSULF(4,3,2,IY) + PCLSULF(4,1,2,IY) + PCLSULF(4,1,3,IY)) / &
                             (ABSULF(4,IY) + APSULF(4,IY) + CLSULF(4,3,2,IY) + &
                              CLSULF(4,1,2,IY) + CLSULF(4,1,3,IY))
! WEST OF THE MISSISSIPPI
           IF ((CLSULF( 5,1,2,IY) + CLSULF( 5,1,3,IY) + CLSULF( 6,3,2,IY) + CLSULF( 6,3,3,IY) + &
                CLSULF( 7,3,2,IY) + CLSULF( 8,1,1,IY) + CLSULF( 8,2,1,IY) + &
                CLSULF( 8,2,2,IY) + CLSULF( 9,2,1,IY) + CLSULF(10,2,1,IY) + CLSULF( 9,2,2,IY) + &
                CLSULF(11,1,1,IY) + CLSULF(11,2,1,IY) + CLSULF(11,2,2,IY) + &
                CLSULF(12,4,1,IY) + CLSULF(12,1,1,IY) + CLSULF(12,2,1,IY) + CLSULF(13,1,1,IY) + &
                CLSULF(13,1,2,IY) + CLSULF(13,2,2,IY) + CLSULF(14,2,1,IY)) .NE. 0.0) &
             T94(19,IY,IS) = (PCLSULF( 5,1,2,IY) + PCLSULF( 5,1,3,IY) + PCLSULF( 6,3,2,IY) + &
                              PCLSULF( 6,3,3,IY) + PCLSULF( 7,3,2,IY) + PCLSULF( 8,1,1,IY) + &
                              PCLSULF( 8,2,1,IY) + PCLSULF( 8,2,2,IY) + PCLSULF( 9,2,1,IY) + &
                              PCLSULF(10,2,1,IY) + PCLSULF( 9,2,2,IY) + PCLSULF(11,1,1,IY) + &
                              PCLSULF(11,2,1,IY) + PCLSULF(11,2,2,IY) + PCLSULF(12,4,1,IY) + &
                              PCLSULF(12,1,1,IY) + PCLSULF(12,2,1,IY) + PCLSULF(13,1,1,IY) + &
                              PCLSULF(13,1,2,IY) + PCLSULF(13,2,2,IY) + PCLSULF(14,2,1,IY)) / &
               (CLSULF( 5,1,2,IY) + CLSULF( 5,1,3,IY) + CLSULF( 6,3,2,IY) + CLSULF( 6,3,3,IY) + &
                CLSULF( 7,3,2,IY) + CLSULF( 8,1,1,IY) + CLSULF( 8,2,1,IY) + &
                CLSULF( 8,2,2,IY) + CLSULF( 9,2,1,IY) + CLSULF(10,2,1,IY) + CLSULF( 9,2,2,IY) + &
                CLSULF(11,1,1,IY) + CLSULF(11,2,1,IY) + CLSULF(11,2,2,IY) + &
                CLSULF(12,4,1,IY) + CLSULF(12,1,1,IY) + CLSULF(12,2,1,IY) + CLSULF(13,1,1,IY) + &
                CLSULF(13,1,2,IY) + CLSULF(13,2,2,IY) + CLSULF(14,2,1,IY))
!          --- U.S. TOTAL
           IF (( ABSULF(4,IY) + IBSULF(4,IY) + ILSULF(4,IY) + WPSULF(4,IY) + &
                 WBSULF(4,IY) + WSSULF(4,IY) + WLSULF(4,IY) + APSULF(4,IY)) .NE. 0.0) &
             T94(20,IY,IS) =(PABSULF(4,IY) + PIBSULF(4,IY) + PILSULF(4,IY) + PWPSULF(4,IY) + &
                             PWBSULF(4,IY) + PWSSULF(4,IY) + PWLSULF(4,IY) + PAPSULF(4,IY)) / &
                            (ABSULF(4,IY) + IBSULF(4,IY) + ILSULF(4,IY) + WPSULF(4,IY) + &
                             WBSULF(4,IY) + WSSULF(4,IY) + WLSULF(4,IY) + APSULF(4,IY))
         ENDDO

!     Table 95 NEMS Regional Coal Production

         DO IY = 1,LASTYR
!  CLSULF array dimension 1 is region
!  CLSULF array dimension 2:  coal type:  1=Bituminous; 2=Subbituminous; 3=Lignite; 4=Premium (coking)
!  CLSULF array dimension 3:  sulfur content:  1=Low; 2=Medium; 3=High
!          --- Northern Appalachia
           T95( 2,IY,IS) = CLSULF(1,4,2,IY)
           T95( 3,IY,IS) = CLSULF(1,1,1,IY)
           T95( 4,IY,IS) = CLSULF(1,1,2,IY)
           T95( 5,IY,IS) = CLSULF(1,1,3,IY)
           T95( 6,IY,IS) = 0.0
           T95( 1,IY,IS) = FSUM(T95(2,IY,IS),4)
!          --- Central Appalachia
           T95( 8,IY,IS) = CLSULF(2,4,2,IY)
           T95( 9,IY,IS) = CLSULF(2,1,1,IY)
           T95(10,IY,IS) = CLSULF(2,1,2,IY)
           T95( 7,IY,IS) = FSUM(T95(8,IY,IS),3)
!          --- Southern Appalachia
           T95(12,IY,IS) = CLSULF(3,4,1,IY)
           T95(13,IY,IS) = CLSULF(3,1,1,IY)
           T95(14,IY,IS) = CLSULF(3,1,2,IY)
           T95(11,IY,IS) = FSUM(T95(12,IY,IS),3)
!          --- Eastern Interior
           T95(16,IY,IS) = CLSULF(4,3,2,IY)
           T95(17,IY,IS) = CLSULF(4,1,2,IY)
           T95(18,IY,IS) = CLSULF(4,1,3,IY)
           T95(15,IY,IS) = FSUM(T95(16,IY,IS),3)
!          --- Western Interior
           T95(19,IY,IS) = CLSULF(5,1,2,IY) + CLSULF(5,1,3,IY)
!          --- Gulf
           T95(21,IY,IS) = CLSULF(6,3,2,IY)
           T95(22,IY,IS) = CLSULF(6,3,3,IY)
           T95(20,IY,IS) = FSUM(T95(21,IY,IS),2)
!          --- Dakota
           T95(23,IY,IS) = CLSULF(7,3,2,IY)
!          --- Western Montana
           T95(25,IY,IS) = CLSULF(8,1,1,IY)
           T95(26,IY,IS) = CLSULF(8,2,1,IY)
           T95(27,IY,IS) = CLSULF(8,2,2,IY)
           T95(24,IY,IS) = FSUM(T95(25,IY,IS),3)
!          --- Wyoming, Northern Powder River Basin
!  changed to uniting Wyomings into one big happy Powder River Basin
           T95(29,IY,IS) = CLSULF(9,2,1,IY) + CLSULF(10,2,1,IY)
           T95(30,IY,IS) = CLSULF(9,2,2,IY)
           T95(28,IY,IS) = FSUM(T95(29,IY,IS),2)
!          --- Wyoming, Southern Powder River Basin
           T95(31,IY,IS) = CLSULF(10,2,1,IY)
!          --- Western Wyoming
           T95(33,IY,IS) = CLSULF(11,1,1,IY)
           T95(34,IY,IS) = CLSULF(11,2,1,IY)
           T95(35,IY,IS) = CLSULF(11,2,2,IY)
           T95(32,IY,IS) = FSUM(T95(33,IY,IS),3)
!          --- Rocky Mountain
           T95(54,IY,IS) = CLSULF(12,4,1,IY)
           T95(37,IY,IS) = CLSULF(12,1,1,IY)
           T95(38,IY,IS) = CLSULF(12,2,1,IY)
           T95(36,IY,IS) = FSUM(T95(37,IY,IS),2)+T95(54,IY,IS)
!          --- Arizona/New Mexico
           T95(40,IY,IS) = CLSULF(13,1,1,IY)
           T95(41,IY,IS) = CLSULF(13,1,2,IY)
           T95(42,IY,IS) = CLSULF(13,2,2,IY)
           T95(39,IY,IS) = FSUM(T95(40,IY,IS),3)
!          --- Washington/Alaska
           T95(43,IY,IS) = CLSULF(14,2,1,IY)
!          --- TYPE TOTALS
           T95(44,IY,IS) = APSULF(4,IY) + WPSULF(4,IY)
           T95(45,IY,IS) = ABSULF(4,IY) + IBSULF(4,IY) + WBSULF(4,IY)
           T95(46,IY,IS) = WSSULF(4,IY)
           T95(47,IY,IS) = ILSULF(4,IY) + WLSULF(4,IY)
!          --- SULFUR TOTALS
           T95(48,IY,IS) = ABSULF(1,IY) + APSULF(1,IY)+ &
                           IBSULF(1,IY) + ILSULF(1,IY) + &
                           WBSULF(1,IY) + WSSULF(1,IY) + WLSULF(1,IY) + WPSULF(1,IY)
           T95(49,IY,IS) = ABSULF(2,IY) + APSULF(2,IY)+ &
                           IBSULF(2,IY) + ILSULF(2,IY) + &
                           WBSULF(2,IY) + WSSULF(2,IY) + WLSULF(2,IY) + WPSULF(2,IY)
           T95(50,IY,IS) = ABSULF(3,IY) + APSULF(3,IY)+ &
                           IBSULF(3,IY) + ILSULF(3,IY) + &
                           WBSULF(3,IY) + WSSULF(3,IY) + WLSULF(3,IY) + WPSULF(3,IY)
! Underground and surface
           T95(51,IY,IS) = sum(PMTD(1:NSREG,IY))
           T95(52,IY,IS) = sum(PMTS(1:NSREG,IY))
!          --- U.S. TOTAL
           T95(53,IY,IS) = ABSULF(4,IY) + APSULF(4,IY)+ &
                           IBSULF(4,IY) + ILSULF(4,IY) + &
                           WBSULF(4,IY) + WSSULF(4,IY) + WLSULF(4,IY) + WPSULF(4,IY) + &
                           APPSTOCKS(IY) + INTSTOCKS(IY) + WESTSTOCKS(IY)
           T95( 6,IY,IS) = CLSULF(1,3,3,IY)
! bonus rows:  coal production by type in Btus * 25.2 = mtoes for IEA submission
           T95(55,IY,IS) =(APSULF_BTU(4,IY) + WPSULF_BTU(4,IY)) * 25.2 / 1000.                    ! premium (coking)
           T95(56,IY,IS) =(ABSULF_BTU(4,IY) + IBSULF_BTU(4,IY) + WBSULF_BTU(4,IY)) * 25.2 / 1000. ! bituminous
           T95(57,IY,IS) =(WSSULF_BTU(4,IY)) * 25.2 / 1000.                                       ! subbituminous
           T95(58,IY,IS) =(ILSULF_BTU(4,IY) + WLSULF_BTU(4,IY) + ALSULF_BTU(4,IY)) * 25.2 / 1000. ! lignite, adds in gob (ALSULF)

! Coal imports and exports
           T95(59,IY,IS) = CLIMPRANK(4,IY) * 25.2 / 1000.
           T95(60,IY,IS) = CLIMPRANK(1,IY) * 25.2 / 1000.
           T95(61,IY,IS) = CLIMPRANK(2,IY) * 25.2 / 1000.
           T95(62,IY,IS) = CLIMPRANK(3,IY) * 25.2 / 1000.
           IF (QCIIN(MNUMCR,IY) .GT. 0.0) &
           T95(63,IY,IS) = QCIIN(MNUMCR,IY) * 25.2
           T95(64,IY,IS) = CLEXPRANK(4,IY) * 25.2 / 1000.
           T95(65,IY,IS) = CLEXPRANK(1,IY) * 25.2 / 1000.
           T95(66,IY,IS) = CLEXPRANK(2,IY) * 25.2 / 1000.
           T95(67,IY,IS) = CLEXPRANK(3,IY) * 25.2 / 1000.
           IF (QCIIN(MNUMCR,IY) .LT. 0.0) &
           T95(68,IY,IS) = QCIIN(MNUMCR,IY) * 25.2 * (-1.0)
         ENDDO

!     Table 96. Steam Coal Flows by World Importing and Exporting Regions

         DO IY = 1,LASTYR
           DO III=1,11
              T96(III,IY,IS) = WSCF(1,III,IY)       !  --- TO EUROPE
              T96(III+11,IY,IS) = WSCF(2,III,IY)    !  --- TO ASIA
              T96(III+22,IY,IS) = WSCF(3,III,IY)    !  --- TO OTHERS
              T96(III+33,IY,IS) = WSCF(4,III,IY)    !  --- TOTALS
           ENDDO
         ENDDO

!     Table 97. Metallugical Coal Flows by World Importing and Exporting Region

         DO IY = 1,LASTYR
           DO III=1,11
              T97(III,IY,IS) = WMCF(1,III,IY)       !  --- TO EUROPE
              T97(III+11,IY,IS) = WMCF(2,III,IY)    !  --- TO ASIA
              T97(III+22,IY,IS) = WMCF(3,III,IY)    !  --- TO OTHERS
              T97(III+33,IY,IS) = WMCF(4,III,IY)    !  --- TOTALS
           ENDDO
         ENDDO

!     Table 98. Total Coal Flows by World Importing and Exporting Regions

         DO IY = 1,LASTYR
           DO III=1,11
              T98(III,IY,IS) = WTCF(1,III,IY)       !  --- TO EUROPE
              T98(III+11,IY,IS) = WTCF(2,III,IY)    !  --- TO ASIA
              T98(III+22,IY,IS) = WTCF(3,III,IY)    !  --- TO OTHERS
              T98(III+33,IY,IS) = WTCF(4,III,IY)    !  --- TOTALS
           ENDDO
         ENDDO

!     Table 99 NEMS Regional Coal Prices

         DO IY = 1,LASTYR
!          --- Northern Appalachia
           IF ( (CLSULF(1,1,1,IY) + CLSULF(1,1,2,IY) + CLSULF(1,1,3,IY) + &
                 CLSULF(1,4,2,IY)) .NE. 0.0) &
            T99( 1,IY,IS) = (PCLSULF(1,1,1,IY) + PCLSULF(1,1,2,IY) +  &
                 PCLSULF(1,1,3,IY) + PCLSULF(1,4,2,IY)) / &
                (CLSULF(1,1,1,IY) + CLSULF(1,1,2,IY) + CLSULF(1,1,3,IY) + CLSULF(1,4,2,IY))
           IF (CLSULF(1,4,2,IY) .NE. 0.0) T99( 2,IY,IS) = PCLSULF(1,4,2,IY) / CLSULF(1,4,2,IY)
           IF (CLSULF(1,1,1,IY) .NE. 0.0) T99( 3,IY,IS) = PCLSULF(1,1,1,IY) / CLSULF(1,1,1,IY)
           IF (CLSULF(1,1,2,IY) .NE. 0.0) T99( 4,IY,IS) = PCLSULF(1,1,2,IY) / CLSULF(1,1,2,IY)
           IF (CLSULF(1,1,3,IY) .NE. 0.0) T99( 5,IY,IS) = PCLSULF(1,1,3,IY) / CLSULF(1,1,3,IY)
!          --- Central Appalachia
           IF ((CLSULF(2,1,1,IY) +  CLSULF(2,1,2,IY) + CLSULF(2,4,2,IY)) .NE. 0.0) &
             T99( 7,IY,IS) = (PCLSULF(2,1,1,IY) + PCLSULF(2,1,2,IY) + PCLSULF(2,4,2,IY)) / &
                             ( CLSULF(2,1,1,IY) +  CLSULF(2,1,2,IY) + CLSULF(2,4,2,IY))
           IF (CLSULF(2,4,2,IY) .NE. 0.0) T99( 8,IY,IS) = PCLSULF(2,4,2,IY) / CLSULF(2,4,2,IY)
           IF (CLSULF(2,1,1,IY) .NE. 0.0) T99( 9,IY,IS) = PCLSULF(2,1,1,IY) / CLSULF(2,1,1,IY)
           IF (CLSULF(2,1,2,IY) .NE. 0.0) T99(10,IY,IS) = PCLSULF(2,1,2,IY) / CLSULF(2,1,2,IY)
!          --- Southern Appalachia
           IF (( CLSULF(3,1,1,IY) +  CLSULF(3,1,2,IY) + CLSULF(3,4,1,IY)) .NE. 0.0) &
             T99(11,IY,IS) = (PCLSULF(3,1,1,IY) + PCLSULF(3,1,2,IY) + PCLSULF(3,4,1,IY)) / &
                             ( CLSULF(3,1,1,IY) +  CLSULF(3,1,2,IY) +  CLSULF(3,4,1,IY))
           IF (CLSULF(3,4,1,IY) .NE. 0.0) T99(12,IY,IS) = PCLSULF(3,4,1,IY) / CLSULF(3,4,1,IY)
           IF (CLSULF(3,1,1,IY) .NE. 0.0) T99(13,IY,IS) = PCLSULF(3,1,1,IY) / CLSULF(3,1,1,IY)
           IF (CLSULF(3,1,2,IY) .NE. 0.0) T99(14,IY,IS) = PCLSULF(3,1,2,IY) / CLSULF(3,1,2,IY)
!          --- Eastern Interior
           IF (( CLSULF(4,1,2,IY) +  CLSULF(4,1,3,IY) +  CLSULF(4,3,2,IY)) .NE. 0.0) &
             T99(16,IY,IS) = (PCLSULF(4,1,2,IY) + PCLSULF(4,1,3,IY) + PCLSULF(4,3,2,IY))/ &
                             ( CLSULF(4,1,2,IY) +  CLSULF(4,1,3,IY) +  CLSULF(4,3,2,IY))
           IF (CLSULF(4,1,2,IY) .NE. 0.0) T99(17,IY,IS) = PCLSULF(4,1,2,IY) / CLSULF(4,1,2,IY)
           IF (CLSULF(4,1,3,IY) .NE. 0.0) T99(18,IY,IS) = PCLSULF(4,1,3,IY) / CLSULF(4,1,3,IY)
           IF (CLSULF(4,3,2,IY) .NE. 0.0) T99(15,IY,IS) = PCLSULF(4,3,2,IY) / CLSULF(4,3,2,IY)
!          --- Western Interior
           IF (( CLSULF(5,1,2,IY) +  CLSULF(5,1,3,IY)) .NE. 0.0) &
             T99(19,IY,IS) = (PCLSULF(5,1,2,IY) + PCLSULF(5,1,3,IY))/ &
                             ( CLSULF(5,1,2,IY) +  CLSULF(5,1,3,IY))
!          --- Gulf
           IF (( CLSULF(6,3,2,IY) +  CLSULF(6,3,3,IY)) .NE. 0.0) &
             T99(20,IY,IS) = (PCLSULF(6,3,2,IY) + PCLSULF(6,3,3,IY))/ &
                             ( CLSULF(6,3,2,IY) +  CLSULF(6,3,3,IY))
           IF (CLSULF(6,3,2,IY) .NE. 0.0) T99(21,IY,IS) = PCLSULF(6,3,2,IY) / CLSULF(6,3,2,IY)
           IF (CLSULF(6,3,3,IY) .NE. 0.0) T99(22,IY,IS) = PCLSULF(6,3,3,IY) / CLSULF(6,3,3,IY)
!          --- Dakota
           IF (CLSULF(7,3,2,IY) .NE. 0.0) T99(23,IY,IS) = PCLSULF(7,3,2,IY) / CLSULF(7,3,2,IY)
!          --- Western Montana
           IF ((CLSULF(8,1,1,IY) + CLSULF(8,2,1,IY) + CLSULF(8,2,2,IY)) .NE. 0.0) &
             T99(24,IY,IS) = (PCLSULF(8,1,1,IY) + PCLSULF(8,2,1,IY) + PCLSULF(8,2,2,IY)) / &
                             ( CLSULF(8,1,1,IY) +  CLSULF(8,2,1,IY) +  CLSULF(8,2,2,IY))
           IF (CLSULF(8,1,1,IY) .NE. 0.0) T99(25,IY,IS) = PCLSULF(8,1,1,IY) / CLSULF(8,1,1,IY)
           IF (CLSULF(8,2,1,IY) .NE. 0.0) T99(26,IY,IS) = PCLSULF(8,2,1,IY) / CLSULF(8,2,1,IY)
           IF (CLSULF(8,2,2,IY) .NE. 0.0) T99(27,IY,IS) = PCLSULF(8,2,2,IY) / CLSULF(8,2,2,IY)
!          --- Wyoming, Northern Powder River Basin
           IF ((CLSULF(9,2,1,IY) + CLSULF(9,2,2,IY) + CLSULF(10,2,1,IY)) .NE. 0.0) &
             T99(28,IY,IS) = (PCLSULF(9,2,1,IY) + PCLSULF(9,2,2,IY) + PCLSULF(10,2,1,IY)) / &
                             ( CLSULF(9,2,1,IY) +  CLSULF(9,2,2,IY) +  CLSULF(10,2,1,IY))
           IF ((CLSULF(9,2,1,IY) + CLSULF(10,2,1,IY)) .NE. 0.0) &
             T99(29,IY,IS) = (PCLSULF(9,2,1,IY) + PCLSULF(10,2,1,IY)) / &
                             ( CLSULF(9,2,1,IY) +  CLSULF(10,2,1,IY))
           IF (CLSULF(9,2,2,IY) .NE. 0.0) T99(30,IY,IS) = PCLSULF(9,2,2,IY) / CLSULF(9,2,2,IY)
!          --- Wyoming, Southern Powder River Basin
           IF (CLSULF(10,2,1,IY) .NE. 0.0) T99(31,IY,IS) = PCLSULF(10,2,1,IY) / CLSULF(10,2,1,IY)
!          --- Western Wyoming
           IF ((CLSULF(11,1,1,IY) + CLSULF(11,2,1,IY) + CLSULF(11,2,2,IY)) .NE. 0.0) &
             T99(32,IY,IS) = (PCLSULF(11,1,1,IY) + PCLSULF(11,2,1,IY) + PCLSULF(11,2,2,IY)) / &
                             ( CLSULF(11,1,1,IY) +  CLSULF(11,2,1,IY) +  CLSULF(11,2,2,IY))
           IF (CLSULF(11,1,1,IY) .NE. 0.0) T99(33,IY,IS) = PCLSULF(11,1,1,IY) / CLSULF(11,1,1,IY)
           IF (CLSULF(11,2,1,IY) .NE. 0.0) T99(34,IY,IS) = PCLSULF(11,2,1,IY) / CLSULF(11,2,1,IY)
           IF (CLSULF(11,2,2,IY) .NE. 0.0) T99(35,IY,IS) = PCLSULF(11,2,2,IY) / CLSULF(11,2,2,IY)
!          --- Rocky Mountain
           IF (( CLSULF(12,1,1,IY) +  CLSULF(12,2,1,IY) + CLSULF(12,4,1,IY)) .NE. 0.0) &
             T99(36,IY,IS) = (PCLSULF(12,1,1,IY) + PCLSULF(12,2,1,IY) + PCLSULF(12,4,1,IY))/ &
                             ( CLSULF(12,1,1,IY) +  CLSULF(12,2,1,IY) +  CLSULF(12,4,1,IY))
           IF (CLSULF(12,4,1,IY) .NE. 0.0) T99(54,IY,IS) = PCLSULF(12,4,1,IY) / CLSULF(12,4,1,IY)
           IF (CLSULF(12,1,1,IY) .NE. 0.0) T99(37,IY,IS) = PCLSULF(12,1,1,IY) / CLSULF(12,1,1,IY)
           IF (CLSULF(12,2,1,IY) .NE. 0.0) T99(38,IY,IS) = PCLSULF(12,2,1,IY) / CLSULF(12,2,1,IY)
!          --- Arizona/New Mexico
           IF (( CLSULF(13,1,1,IY) +  CLSULF(13,2,2,IY) +  CLSULF(13,1,2,IY)) .NE. 0.0) &
             T99(39,IY,IS) = (PCLSULF(13,1,1,IY)+PCLSULF(13,2,2,IY) + PCLSULF(13,1,2,IY))/ &
                             ( CLSULF(13,1,1,IY)+ CLSULF(13,2,2,IY) +  CLSULF(13,1,2,IY))
           IF (CLSULF(13,1,1,IY) .NE. 0.0) T99(40,IY,IS) = PCLSULF(13,1,1,IY) / CLSULF(13,1,1,IY)
           IF (CLSULF(13,1,2,IY) .NE. 0.0) T99(41,IY,IS) = PCLSULF(13,1,2,IY) / CLSULF(13,1,2,IY)
           IF (CLSULF(13,2,2,IY) .NE. 0.0) T99(42,IY,IS) = PCLSULF(13,2,2,IY) / CLSULF(13,2,2,IY)
!          --- Washington/Alaska
           IF (CLSULF(14,2,1,IY) .NE. 0.0) T99(43,IY,IS) = PCLSULF(14,2,1,IY) / CLSULF(14,2,1,IY)
!          --- TYPE TOTALS
           IF ((APSULF(4,IY) + WPSULF(4,IY)) .NE. 0.0) &
             T99(44,IY,IS) =(PAPSULF(4,IY) + PWPSULF(4,IY)) /(APSULF(4,IY) + WPSULF(4,IY))
           IF (( ABSULF(4,IY)+ IBSULF(4,IY)+ WBSULF(4,IY)) .NE. 0.0) &
             T99(45,IY,IS)=(PABSULF(4,IY)+PIBSULF(4,IY)+PWBSULF(4,IY))/ &
                           ( ABSULF(4,IY)+ IBSULF(4,IY)+ WBSULF(4,IY))
           IF (WSSULF(4,IY) .NE. 0.0) T99(46,IY,IS) = PWSSULF(4,IY) / WSSULF(4,IY)
           IF ((ILSULF(4,IY) + WLSULF(4,IY)) .NE. 0.0) &
             T99(47,IY,IS)=(PILSULF(4,IY)+PWLSULF(4,IY))/ &
                           (ILSULF(4,IY)+ WLSULF(4,IY))
!          --- SULFUR TOTALS
           IF ((ABSULF(1,IY)+ IBSULF(1,IY) + ILSULF(1,IY) + WPSULF(1,IY) + &
                WBSULF(1,IY) + WSSULF(1,IY) + WLSULF(1,IY) + APSULF(1,IY)) .NE. 0.0) &
             T99(48,IY,IS)=(PABSULF(1,IY) + PIBSULF(1,IY) + PILSULF(1,IY) + PWPSULF(1,IY) + &
                            PWBSULF(1,IY) + PWSSULF(1,IY) + PWLSULF(1,IY) + PAPSULF(1,IY)) / &
                           ( ABSULF(1,IY) + IBSULF(1,IY) + ILSULF(1,IY) + WPSULF(1,IY) + &
                             WBSULF(1,IY) + WSSULF(1,IY) + WLSULF(1,IY) + APSULF(1,IY))
           IF ((ABSULF(2,IY) + IBSULF(2,IY) + ILSULF(2,IY) + WPSULF(2,IY) + &
                WBSULF(2,IY) + WSSULF(2,IY) + WLSULF(2,IY) + APSULF(2,IY)) .NE. 0.0) &
             T99(49,IY,IS) =(PABSULF(2,IY) + PIBSULF(2,IY) + PILSULF(2,IY) + PWPSULF(2,IY) + &
                             PWBSULF(2,IY) + PWSSULF(2,IY) + PWLSULF(2,IY) + PAPSULF(2,IY)) / &
                            ( ABSULF(2,IY) + IBSULF(2,IY) + ILSULF(2,IY) + WPSULF(2,IY) + &
                              WBSULF(2,IY) + WSSULF(2,IY) + WLSULF(2,IY) + APSULF(2,IY))
           IF (( ABSULF(3,IY) + IBSULF(3,IY) + ILSULF(3,IY) + WPSULF(3,IY) + &
                 WBSULF(3,IY) + WSSULF(3,IY) + WLSULF(3,IY) + APSULF(3,IY)) .NE. 0.0) &
             T99(50,IY,IS) =(PABSULF(3,IY) + PIBSULF(3,IY) + PILSULF(3,IY) + PWPSULF(3,IY) + &
                             PWBSULF(3,IY) + PWSSULF(3,IY) + PWLSULF(3,IY) + PAPSULF(3,IY)) / &
                            ( ABSULF(3,IY) + IBSULF(3,IY) + ILSULF(3,IY) + WPSULF(3,IY) + &
                              WBSULF(3,IY) + WSSULF(3,IY) + WLSULF(3,IY) + APSULF(3,IY))
!    Surface and underground.  Subtract waste coal from surface!  Dividing by Table 95 quantity.
           T99(51,IY,IS) =  sum(PMTDP(1:NSREG,IY)) / sum(PMTD(1:NSREG,IY))
           T99(52,IY,IS) = (sum(PMTSP(1:NSREG,IY)) - PCLSULF(1,3,3,IY))/ &
                           (sum(PMTS(1:NSREG,IY)) - CLSULF(1,3,3,IY))
!          --- U.S. TOTAL
           IF (( ABSULF(4,IY) + IBSULF(4,IY) + ILSULF(4,IY) + WPSULF(4,IY) + &
                 WBSULF(4,IY) + WSSULF(4,IY) + WLSULF(4,IY) + APSULF(4,IY)) .NE. 0.0) &
             T99(53,IY,IS) =(PABSULF(4,IY) + PIBSULF(4,IY) + PILSULF(4,IY) + PWPSULF(4,IY) + &
                             PWBSULF(4,IY) + PWSSULF(4,IY) + PWLSULF(4,IY) + PAPSULF(4,IY)) / &
                            ( ABSULF(4,IY) + IBSULF(4,IY) + ILSULF(4,IY) + WPSULF(4,IY) + &
                              WBSULF(4,IY) + WSSULF(4,IY) + WLSULF(4,IY) + APSULF(4,IY))
! Waste Coal
           IF (CLSULF(1,3,3,IY) .NE. 0.0) T99( 6,IY,IS) = PCLSULF(1,3,3,IY) / CLSULF(1,3,3,IY)
         ENDDO
! mark certain data as withheld if primary or auxiliary historical year and bonus rows are suppressed
! NO withheld for AEO2018, data for 2016 are model results
!if (ftabbone.eq.0) then
!         DO IY=CUMCAPADD-1989-1,CUMCAPADD-1989
!         DO IR=1,NTAB099
!            IF (IR .EQ. 15 .OR. IR .EQ. 16 .OR. IR .EQ. 19 .OR. &
!                IR .EQ. 20 .OR. IR .EQ. 22 .OR. IR .EQ. 23 .OR. &
!                IR .EQ. 24 .OR. IR .EQ. 25 .OR. IR .EQ. 27 .OR. &
!                IR .EQ. 28 .OR. IR .EQ. 30 .OR. IR .EQ. 32 .OR. &
!                IR .EQ. 34 .OR. (IR .GE. 37 .AND. IR .LE. 43) .OR. IR .EQ. 54) THEN
!                         T99(IR,CUMCAPADD-1989-1,IS) = -888.
!            ENDIF
!         ENDDO
!         ENDDO
!endif
       RETURN
       END

      SUBROUTINE FDATA3(IS)
       IMPLICIT NONE

      include 'parametr'
      include 'emmparm'
      include 'qblk'
      include 'ampblk'          !  use adjusted prices
      include 'macout'
      include 'mcdetail'
      include 'pmmout'
      include 'pmmrpt'
      include 'pmmftab'
      include 'ogsmout'
      include 'convfact'
      include 'ngtdmrep'
      include 'ngrpt'
      include 'angtdm'
      include 'ncntrl'
      include 'emission'
      include 'ghgrep'
      include 'uefpout'
      include 'uefdout'
      include 'udatout'
      include 'uecpout'
      include 'uettout'
      include 'efpout'
      include 'comparm'
      include 'comout'
      include 'commrep'
      include 'cogen'
      include 'cdsparms'
      include 'coalout'
      include 'coalemm'
      include 'coalrep'
      include 'indrep'
      include 'intout'
      include 'resdrep'
      include 'tranrep'
      include 'wrenew'
      include 'ftable'
      include 'indout'
      include 'pqchar' ! for parameter mnoth used in include file converge
      include 'converge'
      include 'hmmblk'
      include 'lfmmout'
      include 'e111d'
      include 'tcs45q'

      REAL*4 PRIMARY,INDOUTPT,PTSRS(11)
      REAL*4 FSUM,FFF,GGG,NPV,TMP(MAXYR-1)
      REAL CTLSO2,CTLNOX,CTLHG
      INTEGER*4 III,II,LOOP1,JJJ,ISEC,IC,J,K,PDD,PADD,NUMREP      !COUNTERS
      INTEGER*4 INOX,ISO2
      real so2coal,so2cnt,hgcnt,tlcoal
      EXTERNAL FSUM
      integer techlist(40) ! for table 146-holds list of technology subscripts
      INTEGER IR,IS,IY,IY1,COUNT,I,ICY
      REAL CUMRET,CUMRETP,CUMNOX,CUMNOXP,CUMSCR,CUMSCRP,CUMNCR,CUMNCRP,CUMCCS,CUMCDR,CUMSC,CUMFF,CUMDSI,CUMHRI,CUMGCS,CUMGDR
      REAL CUMRPS,CUMPEN,CUMPTC
      REAL RDAYS,ELECLOSS,TOTALHSE,COMSQFT
! For the Hydrogen table:
      INTEGER endProd,startTransp,endTransp,startDist,endDist,startDisp,endDisp,startCG,endCG
      real PQsumM(HMKT,MNUMCR,MNUMYR),PQsum(MNUMCR,MNUMYR)
      real QsumM(HMKT,MNUMCR,MNUMYR),Qsum(MNUMCR,MNUMYR)
      real HPRM(HMKT,MNUMCR,MNUMYR)
      INTEGER I_BEG_P(6), I_END_P(6), IBP, IEP
      DATA I_BEG_P /1,2,4,6,7,1/
      DATA I_END_P /1,3,5,6,8,8/

! Index Constants for Industrial Tables 35 to 44, 120-124
      integer ixEL/1/,ixNG/2/,ixCL/3/,ixMC/4/, &
              ixCI/5/,ixRF/6/,ixDS/7/,ixLG/8/,ixMG/9/, &
              ixSG/10/,ixPC/11/,ixAS/12/,ixPF/13/,ixKS/14/, &
              ixOP/15/,ixNF/16/,ixLF/17/,ixRN/18/

! DECLARATIONS FOR FEBEN TABLE CALCULATIONS
      REAL T157IYIS, T158IYIS

!     Table 100. Outputs from Macroeconomic Activity Module

         DO 1000, IY = 1,LASTYR
! Value of shipments
           T100(21:61,IY,IS) = MC_REVIND(11, 1:41,IY)
           T100(69,IY,IS) = sum(MC_REVIND(11,15:19,IY)) - MC_REVIND(11,17,IY)    !  Bulk chemicals
           T100(70,IY,IS) = T100(69,IY,IS) + T100(40,IY,IS)                      !  Chemicals total
           T100(71,IY,IS) = MC_REVIND(11,25,IY) + MC_REVIND(11,26,IY)            !  Petroleum and coal
           T100(72,IY,IS) = MC_REVIND(11,30,IY) + MC_REVIND(11,31,IY)            !  Cement and lime
           T100(73,IY,IS) = T100(48,IY,IS) + T100(72,IY,IS) + T100(52,IY,IS)     !  Stone, clay, glass
           T100(74,IY,IS) = sum(MC_REVIND(11,33:35,IY))                          !  Primary metals
           T100(75,IY,IS) = sum(MC_REVIND(11, 1:41,IY)) - sum(MC_REVIND(11, 2: 5,IY)) - &
                            sum(MC_REVIND(11,11:13,IY)) - sum(MC_REVIND(11,21:24,IY)) - &
                            MC_REVIND(11,17,IY) - MC_REVIND(11,29,IY)            !  Manufacturing
           T100(64:67,IY,IS) = MC_REVIND(11,45:48,IY)
           T100(62,IY,IS) = sum(MC_REVIND(11,42:44,IY))                          !  Agriculture
           T100(63,IY,IS) = sum(MC_REVIND(11,45:47,IY))                          !  Mining
           T100(68,IY,IS) = sum(MC_REVIND(11,42:48,IY))                          !  Nonmanufacturing
           T100(76,IY,IS) = T100(68,IY,IS) + T100(75,IY,IS)                      !  Industrial
           T100(77,IY,IS) = sum(MC_REVSER(11, 1:10,IY))                          !  Services
           T100(78,IY,IS) = T100(76,IY,IS) + T100(77,IY,IS)                      !  Total!!!
! Employment
           T100( 79:97,IY,IS) = MC_EMPNA(11, 1:19,IY)
           T100(120,IY,IS) = T100( 86,IY,IS) + T100( 87,IY,IS)           !  Chemicals total
           T100(125,IY,IS) = sum(MC_EMPNA(11, 1:19,IY))                  ! Manufacturing
           T100(112,IY,IS) = sum(MC_EMPNA(11,20:21,IY))                  !  Agriculture
           T100(113,IY,IS) = sum(MC_EMPNA(11,22:24,IY))                  !  Mining
           T100(114:116,IY,IS) = MC_EMPNA(11,22:24,IY)                   !    Mining pieces
           T100(117,IY,IS) = sum(MC_EMPNA(11,25:27,IY))                  !  Construction
           T100(109:111,IY,IS) = MC_EMPNA(11,25:27,IY)                   !    Construction pieces
           T100(118,IY,IS) = sum(MC_EMPNA(11,20:27,IY))                  ! Nonmanufacturing
           T100(126,IY,IS) = T100(118,IY,IS) + T100(125,IY,IS)           ! Industrial
           T100(127,IY,IS) = sum(MC_EMPNA(11,28:39,IY))                  !  Services
           T100(128,IY,IS) = T100(126,IY,IS) + T100(127,IY,IS)           !Total!!!

! Employment by region
           DO IR=1,9
              T100(IR,IY,IS) = sum(MC_EMPNA(IR,1:19,IY)) + sum(MC_EMPNA(IR,22:39,IY))
           ENDDO
           T100(10,IY,IS) = sum(MC_EMPNA(MNUMCR,1:19,IY)) + sum(MC_EMPNA(MNUMCR,22:39,IY))
! Disposable income by region
           T100(11:19,IY,IS) = MC_YPDR(1:9,IY)
           T100(20,IY,IS) = MC_YPDR(MNUMCR,IY)
 1000    CONTINUE

!     Table 101.  Sources of Imported Petroleum

      DO IY = 1,LASTYR
   ! Crude oil
        T101( 1,IY,IS) = ICOCanada(IY)
        T101( 2,IY,IS) = ICOMexico(IY)
        T101( 3,IY,IS) = ICONorthSea(IY)
        T101( 4,IY,IS) = ICOOPEC(IY)
        T101( 5,IY,IS) = ICOOPAmericas(IY)
        T101( 6,IY,IS) = ICOOPNoAfrica(IY)
        T101( 7,IY,IS) = ICOOPWestAfrica(IY)
        T101( 8,IY,IS) = ICOOPIndonesia(IY)
        T101( 9,IY,IS) = ICOOPPersianGulf(IY)
        T101(10,IY,IS) = ICOOtherMidEast(IY)
        T101(11,IY,IS) = ICOOtherAmericas(IY)
        T101(12,IY,IS) = ICOOtherAfrica(IY)
        T101(13,IY,IS) = ICOOtherAsia(IY)
        T101(14,IY,IS) = ICOTotal(IY)
   ! Light refined products
        T101(15,IY,IS) = ILPCanada(IY)
        T101(16,IY,IS) = ILPNorthEurope(IY)
        T101(17,IY,IS) = ILPSouthEurope(IY)
        T101(18,IY,IS) = ILPOPEC(IY)
        T101(19,IY,IS) = ILPOPAmericas(IY)
        T101(20,IY,IS) = ILPOPNoAfrica(IY)
        T101(21,IY,IS) = ILPOPWestAfrica(IY)
        T101(22,IY,IS) = ILPOPIndonesia(IY)
        T101(23,IY,IS) = ILPOPPersianGulf(IY)
        T101(24,IY,IS) = ILPCaribbean(IY)
        T101(25,IY,IS) = ILPAsia(IY)
        T101(26,IY,IS) = ILPOther(IY)
        T101(27,IY,IS) = ILPTotal(IY)
   ! Heavy refined products
        T101(28,IY,IS) = IHPCanada(IY)
        T101(29,IY,IS) = IHPNorthEurope(IY)
        T101(30,IY,IS) = IHPSouthEurope(IY)
        T101(31,IY,IS) = IHPOPEC(IY)
        T101(32,IY,IS) = IHPOPAmericas(IY)
        T101(33,IY,IS) = IHPOPNoAfrica(IY)
        T101(34,IY,IS) = IHPOPWestAfrica(IY)
        T101(35,IY,IS) = IHPOPIndonesia(IY)
        T101(36,IY,IS) = IHPOPPersianGulf(IY)
        T101(37,IY,IS) = IHPCaribbean(IY)
        T101(38,IY,IS) = IHPAsia(IY)
        T101(39,IY,IS) = IHPOther(IY)
        T101(40,IY,IS) = IHPTotal(IY)
      END DO

!     Table 102. Alternative Fuels

      T102(1:NTAB102,1:MNUMCR,1:MNUMYR,IS) = 0.0
      DO IY=1,LASTYR
      DO IR=1,MNUMCR

!  Map Census divisions to PADD regions - note overall omission of Kentucky, Tennessee, Mississippi, Alabama
!       IF (IR .EQ. 1) PADD=7        ! not much in New England, put California here so it's not left out
!       IF (IR .EQ. 2) PADD=1        ! most activity of PADD 1 is in NY, NJ, PA
!       IF (IR .EQ. 3) PADD=3        ! PADD includes Minnesota
!       IF (IR .EQ. 4) PADD=2        ! CD omits Oklahoma, Kentucky, Tennessee, Minnesota
!  mapping CD 5 to PADD 9 to get everyone included
!       IF (IR .EQ. 5) PADD=4
!  Kentucky, Tennessee, Alabama, Mississippi in CD 6 are PADDed elsewhere, so map PADD 9 here:
!       IF (IR .EQ. 6) PADD=9
!  PADD 4 consists of Texas and Louisiana coast and Gulf of Mexico
!       IF (IR .EQ. 7) PADD=5        ! CD includes Oklahoma; omits New Mexico, Mississippi, Alabama
!       IF (IR .EQ. 8) PADD=6        ! CD includes Nevada, Arizona, New Mexico
!       IF (IR .EQ. 9) PADD=8        ! CD omits Nevada, Arizona; PADD omits California
!  PADD 9 in outside the U.S.
        PADD=IR
        IF (IR .EQ. MNUMCR) PADD=MNUMPR
        IF (IR .EQ. 10) CYCLE      ! skip California

        RDAYS = 365.
!       IF (MOD(IY+1989,4).EQ.0) RDAYS = 366.
!       --- ETHANOL
        T102( 1,IR,IY,IS) = 0.9751*CRNETHCD(IR,IY) * RDAYS * 42. / 1000000
        T102( 2,IR,IY,IS) = 0.9751*CLLETHCD(IR,IY) * RDAYS * 42. / 1000000.
        T102( 3,IR,IY,IS) = 0.9751*OTHETHCD(IR,IY)* RDAYS * 42. / 1000000.
        T102( 5,IR,IY,IS) = ETHIMP(IR,IY) * RDAYS * 42. / 1000000.
        T102( 6,IR,IY,IS) = T102( 5,IR,IY,IS) * CELLIMPFRAC(IR,IY)
        T102( 7,IR,IY,IS) = T102( 5,IR,IY,IS) * (1.0-CELLIMPFRAC(IR,IY))
        T102( 8,IR,IY,IS) = ETHEXP(IR,IY) * RDAYS * 42. / 1000000.
        T102( 9,IR,IY,IS) = FSUM(T102(1,IR,IY,IS),5) - T102(8,IR,IY,IS)
        T102(118,IR,IY,IS) = RFBIOBUTECD(IR,IY) * RDAYS * 42. / 1000000.              ! Biobutanol
! plant capacity
        T102(103,IR,IY,IS) = CRNCAPCD(IR,IY) * RDAYS * 42. / 1000000.
        T102(105,IR,IY,IS) = CLLCAPCD(IR,IY) * RDAYS * 42. / 1000000.
        T102(106,IR,IY,IS) = ADVCAPCD(IR,IY) * RDAYS * 42. / 1000000.
        T102(107,IR,IY,IS) = FSUM(T102(103,IR,IY,IS),4)
        IF (T102(103,IR,IY,IS) .NE. 0.0) T102(108,IR,IY,IS) = T102( 1,IR,IY,IS) / T102(103,IR,IY,IS)
        IF (T102(105,IR,IY,IS) .NE. 0.0) T102(110,IR,IY,IS) = T102( 2,IR,IY,IS) / T102(105,IR,IY,IS)
        IF (T102(106,IR,IY,IS) .NE. 0.0) T102(111,IR,IY,IS) = T102( 3,IR,IY,IS) / T102(106,IR,IY,IS)

        T102(13,IR,IY,IS) = ETHE85CD(IR,IY) * RDAYS * 42. / 1000000.
        T102(11,IR,IY,IS) = (ETHTOTCD(IR,IY) -ETHE85CD(IR,IY))* RDAYS * 42. / 1000000.
        T102(12,IR,IY,IS) = ETHTOTCD(IR,IY)* RDAYS * 42. / 1000000.
        T102(10,IR,IY,IS) = QETTR(IR,IY) / CFE85Q(IY) * 42. * ETHNE85  ! billion gallons ethanol in E85
        T102(119,IR,IY,IS) = QBIOBUTE(IR,IY) * RDAYS * 42. / 1000000.
        T102(14,IR,IY,IS) = E85AVAIL(IR,IY) * 100.
!   Total ethanol percent of motor gasoline = (total ethanol)/(motor gas + E85 including ethanol)
        IF (RFQMG(IR,IY)*1000 .NE. 0.0) &
        T102(15,IR,IY,IS) =(ETHTOTCD(IR,IY) /(RFQMG(IR,IY)*1000)) * 100
!   Splash blend percentage ethanol less E85 ethanol /gasoline less E85 gasoline
        IF ((RFQMG(IR,IY)*1000 - ETHE85CD(IR,IY)) .NE. 0.0) &
        T102(16,IR,IY,IS) =((ETHTOTCD(IR,IY) - ETHE85CD(IR,IY))/(RFQMG(IR,IY)*1000. - &
                             (QETTR(11,IY)/CFE85Q(IY)/365.*1000000.))) * 100.
        IF ((RFQMG(IR,IY)*1000 - ETHE85CD(IR,IY)) .NE. 0.0) &
        T102(120,IR,IY,IS) =((QBIOBUTE(IR,IY))/(RFQMG(IR,IY)*1000. - &
                             (QETTR(11,IY)/CFE85Q(IY)/365.*1000000.))) * 100.
        IF ((RFQMG(IR,IY)*1000 - ETHE85CD(IR,IY)) .NE. 0.0) &
        T102(121,IR,IY,IS) =((QBIOBUTE(IR,IY) + ETHTOTCD(IR,IY) - ETHE85CD(IR,IY))/(RFQMG(IR,IY)*1000. - &
                             (QETTR(11,IY)/CFE85Q(IY)/365.*1000000.))) * 100.

!       --- BIODIESEL
        T102(17,IR,IY,IS) = BIMQTYCD(1,IR,IY)* 42. * RDAYS / 1000000.
        T102(18,IR,IY,IS) = BIMQTYCD(3,IR,IY)* 42. * RDAYS / 1000000.
        T102(26,IR,IY,IS) = FSUM(T102(17,IR,IY,IS),2)
        T102(20,IR,IY,IS) = BIODIMP(IR,IY)* 42. * RDAYS / 1000000.
        T102(112,IR,IY,IS)= BIODEXP(IR,IY)* 42. * RDAYS / 1000000.
        T102(21,IR,IY,IS) = BIMQTYCD(4,IR,IY)* 42. * RDAYS / 1000000.
        T102(22,IR,IY,IS) = FSUM(T102(17,IR,IY,IS),5)- T102(112,IR,IY,IS)
        IF (RFQDS(IR,IY) .NE. 0.0) &
        T102(23,IR,IY,IS) = T102(22,IR,IY,IS)/RFQDS(IR,IY)*100. /42./RDAYS

!  These are refinery region variables, so this table mixes Census divisions and refinery regions
!       --- RENEWABLE DIESEL/GASOLINE
        T102(28,IR,IY,IS) = SBOQGD(PADD,IY)* 42. * RDAYS / 1000000.
        T102(29,IR,IY,IS) = WGRQGD(PADD,IY)* 42. * RDAYS / 1000000.
        T102(122,IR,IY,IS) = FSUM(T102(28,IR,IY,IS),2)
        T102(31,IR,IY,IS) = RENEWDIMP(IR,IY)* 42.* RDAYS / 1000000.
        T102(32,IR,IY,IS) = FSUM(T102(28,IR,IY,IS),4)
        T102(123,IR,IY,IS) = T102(32,IR,IY,IS)/RFQDS(IR,IY)*100. /42./RDAYS
        T102(33,IR,IY,IS) = GRD2DSQTY(PADD,IY) * 42. * RDAYS / 1000000.
        T102(34,IR,IY,IS) = GRN2MGQTY(PADD,IY) * 42. * RDAYS / 1000000.
        T102(124,IR,IY,IS) = (T102(22,IR,IY,IS)+T102(32,IR,IY,IS))/RFQDS(IR,IY)*100. /42./RDAYS
		
!       --- RENEWABLE JET FUEL (HEFA-SPK)
        T102(125,IR,IY,IS) = SBOQRJH(PADD,IY)* 42. * RDAYS / 1000000.
        T102(126,IR,IY,IS) = WGRQRJH(PADD,IY)* 42. * RDAYS / 1000000.
        T102(127,IR,IY,IS) = FSUM(T102(125,IR,IY,IS),2)
        T102(128,IR,IY,IS) = T102(127,IR,IY,IS)/RFQJF(IR,IY)*100. /42./RDAYS
        T102(129,IR,IY,IS) = SAF2JTQTY(PADD,IY) * 42. * RDAYS / 1000000.

!       --- BTL
        T102(35,IR,IY,IS) = BTLFRAC(1,PADD,IY)/1000.
        T102(36,IR,IY,IS) = BTLFRAC(2,PADD,IY)/1000.
        T102(37,IR,IY,IS) = (BTLFRAC(3,PADD,IY)+BTLFRAC(4,PADD,IY))/1000.
        T102(38,IR,IY,IS) = FSUM(T102(35,IR,IY,IS),3)
        T102(40,IR,IY,IS) = T102(38,IR,IY,IS) * 365. * 42. / 1000.

        T102(39,IR,IY,IS) = UBAVOL(PADD,IY)/1000.
        T102(113,IR,IY,IS) = UBAVOLMG(PADD,IY)/1000.
        T102(114,IR,IY,IS) = UBAVOLDS(PADD,IY)/1000.
        T102(115,IR,IY,IS) = T102(39,IR,IY,IS) - T102(113,IR,IY,IS) - T102(114,IR,IY,IS)
        T102(41,IR,IY,IS) = UBAVOL(PADD,IY)/1000. * 365. * 42. / 1000.
        T102(42,IR,IY,IS) = T102(38,IR,IY,IS)+ T102(39,IR,IY,IS)
        T102(43,IR,IY,IS) = T102(40,IR,IY,IS)+ T102(41,IR,IY,IS)
        IF (TDIESEL(IR,IY) .NE. 0.0) &
        T102(44,IR,IY,IS) =(T102(37,IR,IY,IS)+T102(114,IR,IY,IS))/TDIESEL(IR,IY)*100.*1000.
!       --- CTL
        T102(45,IR,IY,IS) = CTLFRAC(1,PADD,IY)/1000.
        T102(46,IR,IY,IS) = CTLFRAC(2,PADD,IY)/1000.
        T102(47,IR,IY,IS) = (CTLFRAC(3,PADD,IY)+CTLFRAC(4,PADD,IY))/1000.
        T102(48,IR,IY,IS) = FSUM(T102(45,IR,IY,IS),3)
        T102(49,IR,IY,IS) = T102(48,IR,IY,IS) * 365. * 42. / 1000.
        IF (TDIESEL(IR,IY) .NE. 0.0) &
        T102(50,IR,IY,IS) = T102(47,IR,IY,IS)/TDIESEL(IR,IY)*100.*1000.
!       --- C and B TL
        T102(51,IR,IY,IS) = CBTLFRAC(1,1,PADD,IY)/1000.
        T102(52,IR,IY,IS) = CBTLFRAC(1,2,PADD,IY)/1000.
        T102(53,IR,IY,IS) = (CBTLFRAC(1,3,PADD,IY)+CBTLFRAC(1,4,PADD,IY))/1000.
        T102(57,IR,IY,IS) = FSUM(T102(51,IR,IY,IS),3)
        T102(54,IR,IY,IS) = CBTLFRAC(2,1,PADD,IY)/1000.
        T102(55,IR,IY,IS) = CBTLFRAC(2,2,PADD,IY)/1000.
        T102(56,IR,IY,IS) = (CBTLFRAC(2,3,PADD,IY)+CBTLFRAC(2,4,PADD,IY))/1000.
        T102(58,IR,IY,IS) = FSUM(T102(54,IR,IY,IS),3)
        T102(59,IR,IY,IS) = FSUM(T102(57,IR,IY,IS),2)
        T102(60,IR,IY,IS) = T102(57,IR,IY,IS) * 365. * 42. / 1000.
        T102(61,IR,IY,IS) = T102(58,IR,IY,IS) * 365. * 42. / 1000.
        T102(62,IR,IY,IS) = FSUM(T102(60,IR,IY,IS),2)
        IF (TDIESEL(IR,IY) .NE. 0.0) &
        T102(63,IR,IY,IS) =(T102(53,IR,IY,IS)+T102(56,IR,IY,IS))/TDIESEL(IR,IY)*100.*1000.
!       --- GTL
        T102(64,IR,IY,IS) = GTLFRAC(1,PADD,IY)/1000.
        T102(65,IR,IY,IS) = GTLFRAC(2,PADD,IY)/1000.
        T102(66,IR,IY,IS) = (GTLFRAC(3,PADD,IY)+GTLFRAC(4,PADD,IY))/1000.
        T102(67,IR,IY,IS) = FSUM(T102(64,IR,IY,IS),3)
        T102(68,IR,IY,IS) = T102(67,IR,IY,IS) * 365. * 42. / 1000.
        IF (TDIESEL(IR,IY) .NE. 0.0) &
        T102(69,IR,IY,IS) = T102(66,IR,IY,IS)/TDIESEL(IR,IY)*100.*1000.
! total distillate stream percents
        T102(70,IR,IY,IS) = T102(44,IR,IY,IS) + T102(50,IR,IY,IS) + T102(63,IR,IY,IS) + T102(69,IR,IY,IS)

!  These are Census division variables again
!       --- Prices
        T102( 71,IR,IY,IS) = BRENT_PRICE(IY) / 42.
        T102( 72,IR,IY,IS) = WTI_PRICE(IY) / 42.
        T102(116,IR,IY,IS) = IT_WOP(IY,1) / 42.
        T102( 73,IR,IY,IS) = PETHM(IR,IY) / 42.
        T102( 74,IR,IY,IS) = (PALMG(IR,IY) + RFENVFX(IR,IY,2)) / 42.     ! not PALBOB(IR,IY)
        T102( 75,IR,IY,IS) = T102( 73,IR,IY,IS) / T102( 74,IR,IY,IS)
        T102( 76,IR,IY,IS) = PMGTR(IR,IY) * CFMGQ(IY) / 42.
        T102( 77,IR,IY,IS) = PETTR(IR,IY) * CFE85Q(IY) / 42.
        T102( 78,IR,IY,IS) = SBO_PRICE(IR,IY) / 42.
        T102( 79,IR,IY,IS) = YGR_PRICE(IR,IY) / 42.
        T102( 80,IR,IY,IS) = BIODPRICE(IR,IY) / 42.
      
        T102( 81,IR,IY,IS) = CRNPRICE(IR,IY)

        T102( 83,IR,IY,IS) = BRENT_PRICE(IY) / CFCRDLTSWT(IY)
        T102( 84,IR,IY,IS) = WTI_PRICE(IY) / CFCRDLTSWT(IY)
        T102(117,IR,IY,IS) = IT_WOP(IY,2)
        T102( 85,IR,IY,IS) = PETHM(IR,IY) / CFETQ(IY)
        T102( 86,IR,IY,IS) = PMGTR(IR,IY)
        T102( 87,IR,IY,IS) = PETTR(IR,IY)
        T102( 88,IR,IY,IS) = SBO_PRICE(IR,IY) / CFBIOD(IY)
        T102( 89,IR,IY,IS) = YGR_PRICE(IR,IY) / CFBIOD(IY)
        T102( 90,IR,IY,IS) = BIODPRICE(IR,IY) / CFBIOD(IY)
        T102( 91,IR,IY,IS) = CRNPRICE(IR,IY) / CFCORN * 1000000.

        T102( 93,IR,IY,IS) = CORNCD(3,IR,IY)
        T102( 98,IR,IY,IS) = GRAINCD(IR,IY)
!       T102(   ,IR,IY,IS) = cellulose to ethanol in bushels
!       T102(   ,IR,IY,IS) = cellulose to liquids
!       T102(   ,IR,IY,IS) = FSUM(T102(   ,IR,IY,IS),2)
        T102(101,IR,IY,IS) = SBOQTYCD(IR,IY) * 42. * RDAYS / 1000000.
        T102(102,IR,IY,IS) = PLMQTYCD(IR,IY) * 42. * RDAYS / 1000000.

      ENDDO
      ENDDO

! Table 103 - Alternate Fuels, mainly RFS related

      DO IY = 1,LASTYR

!  Credit prices and RIN prices
        T103( 1,IY,IS) = RFSCREDPRC(1,IY)/42.
        T103( 2,IY,IS) = RFSCREDPRC(2,IY)/42.
        T103( 3,IY,IS) = RFSCREDPRC(3,IY)/42.
        T103( 4,IY,IS) = RFSCREDPRC(4,IY)/42.
        T103( 5,IY,IS) =(RFSCREDPRC(1,IY) + RFSCREDPRC(2,IY)) / 42.
        T103( 6,IY,IS) =(RFSCREDPRC(1,IY) + RFSCREDPRC(2,IY) + RFSCREDPRC(3,IY)) / 42.
        T103( 7,IY,IS) =(RFSCREDPRC(1,IY) + RFSCREDPRC(2,IY) + RFSCREDPRC(4,IY)) / 42.
!  and E85 infrastructure price
        T103( 8,IY,IS) = E85ICCREDIT(IY)/42.
!  for RFS variables (including credits, in following section), the first subscript is defined as:
!      1 = Total     2 = Advanced     3 = Cellulosic     4 = Biodiesel
!  RFS mandates
        T103( 9,IY,IS) = RFSMANDATES(1,IY)
        T103(10,IY,IS) = RFSMANDATES(2,IY)
        T103(11,IY,IS) = RFSMANDATES(3,IY)
        T103(12,IY,IS) = RFSMANDATES(4,IY)

!  for RFS credit variables, the second subscript is defined as:
!      1 = Corn Ethanol       2 = Advanced Ethanol    3 = Cellulosic Ethanol  4 = Imported Ethanol
!      5 = Biodiesel          6 = Renewable Diesel    7 = Renewable Gasoline  8 = BTL
!      9 = Pyrolysis         10 = CBTL               11 = Safety Valve       12 = Biobutanol
!  RFS credits and safety
!      Total
        T103(13,IY,IS) = RFSCREDITS(1,1,IY)
        T103(14,IY,IS) = RFSCREDITS(1,2,IY)
        T103(15,IY,IS) = RFSCREDITS(1,3,IY)
        T103(16,IY,IS) = RFSCREDITS(1,4,IY)
        T103(17,IY,IS) = RFSCREDITS(1,5,IY)
        T103(18,IY,IS) = RFSCREDITS(1,6,IY)
        T103(19,IY,IS) = RFSCREDITS(1,7,IY)
        T103(20,IY,IS) = RFSCREDITS(1,8,IY)
        T103(21,IY,IS) = RFSCREDITS(1,9,IY)
        T103(22,IY,IS) = RFSCREDITS(1,10,IY)
        T103(23,IY,IS) = RFSCREDITS(1,12,IY)
        T103(24,IY,IS) = RFSCREDITS(1,11,IY)
        T103(25,IY,IS) = RFSCREDITS(1,13,IY)
        T103(26,IY,IS) = RFSSAFETY(1,IY)
!      Advanced
        T103(27,IY,IS) = RFSCREDITS(2,2,IY)
        T103(28,IY,IS) = RFSCREDITS(2,3,IY)
        T103(29,IY,IS) = RFSCREDITS(2,4,IY)
        T103(30,IY,IS) = RFSCREDITS(2,5,IY)
        T103(31,IY,IS) = RFSCREDITS(2,6,IY)
        T103(32,IY,IS) = RFSCREDITS(2,7,IY)
        T103(33,IY,IS) = RFSCREDITS(2,8,IY)
        T103(34,IY,IS) = RFSCREDITS(2,9,IY)
        T103(35,IY,IS) = RFSCREDITS(2,10,IY)
        T103(36,IY,IS) = RFSCREDITS(2,12,IY)
        T103(37,IY,IS) = RFSCREDITS(2,11,IY)
        T103(38,IY,IS) = RFSCREDITS(2,13,IY)
        T103(39,IY,IS) = RFSSAFETY(2,IY)
!      Cellulosic
        T103(40,IY,IS) = RFSCREDITS(3,3,IY)
        T103(41,IY,IS) = RFSCREDITS(3,8,IY)
        T103(42,IY,IS) = RFSCREDITS(3,9,IY)
        T103(43,IY,IS) = RFSCREDITS(3,10,IY)
        T103(44,IY,IS) = RFSCREDITS(3,11,IY)
        T103(45,IY,IS) = RFSCREDITS(3,13,IY)
        T103(46,IY,IS) = RFSSAFETY(3,IY)
!      Regular (not advanced)
        T103(47,IY,IS) = RFSCREDITS(1,13,IY) - RFSCREDITS(2,13,IY)
!      Biodiesel
        T103(48,IY,IS) = RFSCREDITS(4,5,IY)
        T103(49,IY,IS) = RFSCREDITS(4,6,IY)
        T103(50,IY,IS) = RFSCREDITS(4,11,IY)
        T103(51,IY,IS) = RFSCREDITS(4,13,IY)
        T103(52,IY,IS) = RFSSAFETY(4,IY)

!      LCFS
        T103(53,IY,IS) = LCFS_BaseLine(1,IY)
        T103(54,IY,IS) = LCFS_Actual(1,IY)
        T103(55,IY,IS) = LCFS_Waiver(1,IY)
        T103(56,IY,IS) = LCFS_Offset_Prc(1,IY)
        T103(57,IY,IS) = LCFS_Carb_Offset(1,IY)
        T103(58,IY,IS) = LCFS_PeToTrills(1,IY)* LCFS_Actual(1,IY)/1000.
        T103(59,IY,IS) = LCFS_PeToTrills(1,IY)/1000.

        T103(60,IY,IS) = LCFS_BaseLine(2,IY)
        T103(61,IY,IS) = LCFS_Actual(2,IY)
        T103(62,IY,IS) = LCFS_Waiver(2,IY)
        T103(63,IY,IS) = LCFS_Offset_Prc(2,IY)
        T103(64,IY,IS) = LCFS_Carb_Offset(2,IY)
        T103(65,IY,IS) = LCFS_PeToTrills(2,IY)* LCFS_Actual(2,IY)/1000.
        T103(66,IY,IS) = LCFS_PeToTrills(2,IY)/1000.
		
!      CFP		
        T103(67,IY,IS) = CFP_BaseLine(1,IY)
        T103(68,IY,IS) = CFP_Actual(1,IY)
        T103(69,IY,IS) = CFP_Waiver(1,IY)
        T103(70,IY,IS) = CFP_Offset_Prc(1,IY)
        T103(71,IY,IS) = CFP_Carb_Offset(1,IY)
        T103(72,IY,IS) = CFP_PeToTrills(1,IY)* CFP_Actual(1,IY)/1000.
        T103(73,IY,IS) = CFP_PeToTrills(1,IY)/1000.

        T103(74,IY,IS) = CFP_BaseLine(2,IY)
        T103(75,IY,IS) = CFP_Actual(2,IY)
        T103(76,IY,IS) = CFP_Waiver(2,IY)
        T103(77,IY,IS) = CFP_Offset_Prc(2,IY)
        T103(78,IY,IS) = CFP_Carb_Offset(2,IY)
        T103(79,IY,IS) = CFP_PeToTrills(2,IY)* CFP_Actual(2,IY)/1000.
        T103(80,IY,IS) = CFP_PeToTrills(2,IY)/1000.
      ENDDO

! Table 104 - blank - T104(

!     Table 105.  Domestic crude oil production, price, and refinery inputs

       DO IY = 1,LASTYR
! region 1:
          T105(001:006,IY,IS) = OGCRUDEREF(1,1:6,IY) / 365. * 1000.
          T105(061:066,IY,IS) = RFCRUDEWHP(1,1:6,IY)
          T105(121:122,IY,IS) = OGCRUDEREF(1,10:11,IY) / 365. * 1000.
          T105(137:138,IY,IS) = RFCRUDEWHP(1,10:11,IY)
          T105(007,IY,IS) = FSUM(T105(001,IY,IS),6) + FSUM(T105(121,IY,IS),2)
! region 2:
          T105(008:013,IY,IS) = OGCRUDEREF(2,1:6,IY) / 365. * 1000.
          T105(068:073,IY,IS) = RFCRUDEWHP(2,1:6,IY)
          T105(123:124,IY,IS) = OGCRUDEREF(2,10:11,IY) / 365. * 1000.
          T105(139:140,IY,IS) = RFCRUDEWHP(2,10:11,IY)
          T105(014,IY,IS) = FSUM(T105(008,IY,IS),6) + FSUM(T105(123,IY,IS),2)
! region 3:
          T105(015:020,IY,IS) = OGCRUDEREF(3,1:6,IY) / 365. * 1000.
          T105(075:080,IY,IS) = RFCRUDEWHP(3,1:6,IY)
          T105(125:126,IY,IS) = OGCRUDEREF(3,10:11,IY) / 365. * 1000.
          T105(141:142,IY,IS) = RFCRUDEWHP(3,10:11,IY)
          T105(021,IY,IS) = FSUM(T105(015,IY,IS),6) + FSUM(T105(125,IY,IS),2)
! region 4:
          T105(022:027,IY,IS) = OGCRUDEREF(4,1:6,IY) / 365. * 1000.
          T105(082:087,IY,IS) = RFCRUDEWHP(4,1:6,IY)
          T105(127:128,IY,IS) = OGCRUDEREF(4,10:11,IY) / 365. * 1000.
          T105(143:144,IY,IS) = RFCRUDEWHP(4,10:11,IY)
          T105(028,IY,IS) = FSUM(T105(022,IY,IS),6) + FSUM(T105(127,IY,IS),2)
! region 5:
          T105(029:034,IY,IS) = OGCRUDEREF(5,1:6,IY) / 365. * 1000.
          T105(089:094,IY,IS) = RFCRUDEWHP(5,1:6,IY)
          T105(129:130,IY,IS) = OGCRUDEREF(5,10:11,IY) / 365. * 1000.
          T105(145:146,IY,IS) = RFCRUDEWHP(5,10:11,IY)
          T105(035,IY,IS) = FSUM(T105(029,IY,IS),6) + FSUM(T105(129,IY,IS),2)
! region 6:
          T105(036:041,IY,IS) = OGCRUDEREF(6,1:6,IY) / 365. * 1000.
          T105(096:101,IY,IS) = RFCRUDEWHP(6,1:6,IY)
          T105(131:132,IY,IS) = OGCRUDEREF(6,10:11,IY) / 365. * 1000.
          T105(147:148,IY,IS) = RFCRUDEWHP(6,10:11,IY)
          T105(042,IY,IS) = FSUM(T105(036,IY,IS),6) + FSUM(T105(131,IY,IS),2)
! region 7 (California):
          T105(043:043,IY,IS) = OGCRUDEREF(7,7:7,IY) / 365. * 1000.
          T105(103:103,IY,IS) = RFCRUDEWHP(7,7:7,IY)
          T105(044,IY,IS) = FSUM(T105(043,IY,IS),1)
! region 8:
          T105(045:050,IY,IS) = OGCRUDEREF(8,1:6,IY) / 365. * 1000.
          T105(105:110,IY,IS) = RFCRUDEWHP(8,1:6,IY)
          T105(133:134,IY,IS) = OGCRUDEREF(8,10:11,IY) / 365. * 1000.
          T105(149:150,IY,IS) = RFCRUDEWHP(8,10:11,IY)
          T105(051,IY,IS) = FSUM(T105(045,IY,IS),6) + FSUM(T105(133,IY,IS),2)
!  total United States:
          T105(052,IY,IS) =(sum(OGCRUDEREF(1:6,1,IY))+OGCRUDEREF(8,1,IY)) / 365. * 1000.
          T105(053,IY,IS) =(sum(OGCRUDEREF(1:6,2,IY))+OGCRUDEREF(8,2,IY)) / 365. * 1000.
          T105(054,IY,IS) =(sum(OGCRUDEREF(1:6,3,IY))+OGCRUDEREF(8,3,IY)) / 365. * 1000.
          T105(055,IY,IS) =(sum(OGCRUDEREF(1:6,4,IY))+OGCRUDEREF(8,4,IY)) / 365. * 1000.
          T105(056,IY,IS) =(sum(OGCRUDEREF(1:6,5,IY))+OGCRUDEREF(8,5,IY)) / 365. * 1000.
          T105(057,IY,IS) =(sum(OGCRUDEREF(1:6,6,IY))+OGCRUDEREF(8,6,IY)) / 365. * 1000.
          T105(058,IY,IS) = OGCRUDEREF(7,7,IY) / 365. * 1000.
          T105(135,IY,IS) =(sum(OGCRUDEREF(1:6,10,IY))+OGCRUDEREF(8,10,IY)) / 365. * 1000.
          T105(136,IY,IS) =(sum(OGCRUDEREF(1:6,11,IY))+OGCRUDEREF(8,11,IY)) / 365. * 1000.
          T105(059,IY,IS) = FSUM(T105(052,IY,IS),7) + FSUM(T105(135,IY,IS),2)
          IF ((sum(OGCRUDEREF(1:6,1,IY))+OGCRUDEREF(8,1,IY)) .NE. 0.0) &
              T105(112,IY,IS) = &
                  (OGCRUDEREF(1,1,IY)*RFCRUDEWHP(1,1,IY) + OGCRUDEREF(2,1,IY)*RFCRUDEWHP(2,1,IY) + &
                   OGCRUDEREF(3,1,IY)*RFCRUDEWHP(3,1,IY) + OGCRUDEREF(4,1,IY)*RFCRUDEWHP(4,1,IY) + &
                   OGCRUDEREF(5,1,IY)*RFCRUDEWHP(5,1,IY) + OGCRUDEREF(6,1,IY)*RFCRUDEWHP(6,1,IY) + &
                   OGCRUDEREF(8,1,IY)*RFCRUDEWHP(8,1,IY)) / &
               (OGCRUDEREF(1,1,IY) + OGCRUDEREF(2,1,IY) + OGCRUDEREF(3,1,IY) + OGCRUDEREF(4,1,IY) + &
                OGCRUDEREF(5,1,IY) + OGCRUDEREF(6,1,IY) + OGCRUDEREF(8,1,IY))
          IF ((sum(OGCRUDEREF(1:6,2,IY))+OGCRUDEREF(8,2,IY)) .NE. 0.0) &
              T105(113,IY,IS) = &
                  (OGCRUDEREF(1,2,IY)*RFCRUDEWHP(1,2,IY) + OGCRUDEREF(2,2,IY)*RFCRUDEWHP(2,2,IY) + &
                   OGCRUDEREF(3,2,IY)*RFCRUDEWHP(3,2,IY) + OGCRUDEREF(4,2,IY)*RFCRUDEWHP(4,2,IY) + &
                   OGCRUDEREF(5,2,IY)*RFCRUDEWHP(5,2,IY) + OGCRUDEREF(6,2,IY)*RFCRUDEWHP(6,2,IY) + &
                   OGCRUDEREF(8,2,IY)*RFCRUDEWHP(8,2,IY)) / &
               (OGCRUDEREF(1,2,IY) + OGCRUDEREF(2,2,IY) + OGCRUDEREF(3,2,IY) + OGCRUDEREF(4,2,IY) + &
                OGCRUDEREF(5,2,IY) + OGCRUDEREF(6,2,IY) + OGCRUDEREF(8,2,IY))
          IF ((sum(OGCRUDEREF(1:6,3,IY))+OGCRUDEREF(8,3,IY)) .NE. 0.0) &
              T105(114,IY,IS) = &
                  (OGCRUDEREF(1,3,IY)*RFCRUDEWHP(1,3,IY) + OGCRUDEREF(2,3,IY)*RFCRUDEWHP(2,3,IY) + &
                   OGCRUDEREF(3,3,IY)*RFCRUDEWHP(3,3,IY) + OGCRUDEREF(4,3,IY)*RFCRUDEWHP(4,3,IY) + &
                   OGCRUDEREF(5,3,IY)*RFCRUDEWHP(5,3,IY) + OGCRUDEREF(6,3,IY)*RFCRUDEWHP(6,3,IY) + &
                   OGCRUDEREF(8,3,IY)*RFCRUDEWHP(8,3,IY)) / &
               (OGCRUDEREF(1,3,IY) + OGCRUDEREF(2,3,IY) + OGCRUDEREF(3,3,IY) + OGCRUDEREF(4,3,IY) + &
                OGCRUDEREF(5,3,IY) + OGCRUDEREF(6,3,IY) + OGCRUDEREF(8,3,IY))
          IF ((sum(OGCRUDEREF(1:6,4,IY))+OGCRUDEREF(8,4,IY)) .NE. 0.0) &
              T105(115,IY,IS) = &
                  (OGCRUDEREF(1,4,IY)*RFCRUDEWHP(1,4,IY) + OGCRUDEREF(2,4,IY)*RFCRUDEWHP(2,4,IY) + &
                   OGCRUDEREF(3,4,IY)*RFCRUDEWHP(3,4,IY) + OGCRUDEREF(4,4,IY)*RFCRUDEWHP(4,4,IY) + &
                   OGCRUDEREF(5,4,IY)*RFCRUDEWHP(5,4,IY) + OGCRUDEREF(6,4,IY)*RFCRUDEWHP(6,4,IY) + &
                   OGCRUDEREF(8,4,IY)*RFCRUDEWHP(8,4,IY)) / &
               (OGCRUDEREF(1,4,IY) + OGCRUDEREF(2,4,IY) + OGCRUDEREF(3,4,IY) + OGCRUDEREF(4,4,IY) + &
                OGCRUDEREF(5,4,IY) + OGCRUDEREF(6,4,IY) + OGCRUDEREF(8,4,IY))
          IF ((sum(OGCRUDEREF(1:6,5,IY))+OGCRUDEREF(8,5,IY)) .NE. 0.0) &
              T105(116,IY,IS) = &
                  (OGCRUDEREF(1,5,IY)*RFCRUDEWHP(1,5,IY) + OGCRUDEREF(2,5,IY)*RFCRUDEWHP(2,5,IY) + &
                   OGCRUDEREF(3,5,IY)*RFCRUDEWHP(3,5,IY) + OGCRUDEREF(4,5,IY)*RFCRUDEWHP(4,5,IY) + &
                   OGCRUDEREF(5,5,IY)*RFCRUDEWHP(5,5,IY) + OGCRUDEREF(6,5,IY)*RFCRUDEWHP(6,5,IY) + &
                   OGCRUDEREF(8,5,IY)*RFCRUDEWHP(8,5,IY)) / &
               (OGCRUDEREF(1,5,IY) + OGCRUDEREF(2,5,IY) + OGCRUDEREF(3,5,IY) + OGCRUDEREF(4,5,IY) + &
                OGCRUDEREF(5,5,IY) + OGCRUDEREF(6,5,IY) + OGCRUDEREF(8,5,IY))
          IF ((sum(OGCRUDEREF(1:6,6,IY))+OGCRUDEREF(8,6,IY)) .NE. 0.0) &
              T105(117,IY,IS) = &
                  (OGCRUDEREF(1,6,IY)*RFCRUDEWHP(1,6,IY) + OGCRUDEREF(2,6,IY)*RFCRUDEWHP(2,6,IY) + &
                   OGCRUDEREF(3,6,IY)*RFCRUDEWHP(3,6,IY) + OGCRUDEREF(4,6,IY)*RFCRUDEWHP(4,6,IY) + &
                   OGCRUDEREF(5,6,IY)*RFCRUDEWHP(5,6,IY) + OGCRUDEREF(6,6,IY)*RFCRUDEWHP(6,6,IY) + &
                   OGCRUDEREF(8,6,IY)*RFCRUDEWHP(8,6,IY)) / &
               (OGCRUDEREF(1,6,IY) + OGCRUDEREF(2,6,IY) + OGCRUDEREF(3,6,IY) + OGCRUDEREF(4,6,IY) + &
                OGCRUDEREF(5,6,IY) + OGCRUDEREF(6,6,IY) + OGCRUDEREF(8,6,IY))
          T105(118,IY,IS) = RFCRUDEWHP(7,7,IY)
          IF ((sum(OGCRUDEREF(1:6,10,IY))+OGCRUDEREF(8,10,IY)) .NE. 0.0) &
              T105(151,IY,IS) = &
                  (OGCRUDEREF(1,10,IY)*RFCRUDEWHP(1,10,IY) + OGCRUDEREF(2,10,IY)*RFCRUDEWHP(2,10,IY) + &
                   OGCRUDEREF(3,10,IY)*RFCRUDEWHP(3,10,IY) + OGCRUDEREF(4,10,IY)*RFCRUDEWHP(4,10,IY) + &
                   OGCRUDEREF(5,10,IY)*RFCRUDEWHP(5,10,IY) + OGCRUDEREF(6,10,IY)*RFCRUDEWHP(6,10,IY) + &
                   OGCRUDEREF(8,10,IY)*RFCRUDEWHP(8,10,IY)) / &
               (OGCRUDEREF(1,10,IY) + OGCRUDEREF(2,10,IY) + OGCRUDEREF(3,10,IY) + OGCRUDEREF(4,10,IY) + &
                OGCRUDEREF(5,10,IY) + OGCRUDEREF(6,10,IY) + OGCRUDEREF(8,10,IY))
          IF ((sum(OGCRUDEREF(1:6,11,IY))+OGCRUDEREF(8,11,IY)) .NE. 0.0) &
              T105(152,IY,IS) = &
                  (OGCRUDEREF(1,11,IY)*RFCRUDEWHP(1,11,IY) + OGCRUDEREF(2,11,IY)*RFCRUDEWHP(2,11,IY) + &
                   OGCRUDEREF(3,11,IY)*RFCRUDEWHP(3,11,IY) + OGCRUDEREF(4,11,IY)*RFCRUDEWHP(4,11,IY) + &
                   OGCRUDEREF(5,11,IY)*RFCRUDEWHP(5,11,IY) + OGCRUDEREF(6,11,IY)*RFCRUDEWHP(6,11,IY) + &
                   OGCRUDEREF(8,11,IY)*RFCRUDEWHP(8,11,IY)) / &
               (OGCRUDEREF(1,11,IY) + OGCRUDEREF(2,11,IY) + OGCRUDEREF(3,11,IY) + OGCRUDEREF(4,11,IY) + &
                OGCRUDEREF(5,11,IY) + OGCRUDEREF(6,11,IY) + OGCRUDEREF(8,11,IY))
          IF ((sum(OGCRUDEREF(1:6,1:6,IY))+sum(OGCRUDEREF(8,1:6,IY))+OGCRUDEREF(7,7,IY)) .NE. 0.0) &
              T105(119,IY,IS) = &
                  (T105(112,IY,IS)*T105(052,IY,IS) + T105(113,IY,IS)*T105(053,IY,IS) + &
                   T105(114,IY,IS)*T105(054,IY,IS) + T105(115,IY,IS)*T105(055,IY,IS) + &
                   T105(116,IY,IS)*T105(056,IY,IS) + T105(117,IY,IS)*T105(057,IY,IS) + &
                   T105(118,IY,IS)*T105(058,IY,IS) + &
                   T105(151,IY,IS)*T105(135,IY,IS) + T105(152,IY,IS)*T105(136,IY,IS)) / &
                          (FSUM(T105(52,IY,IS),7) + FSUM(T105(135,IY,IS),2))
! any not listed:
          T105(060,IY,IS) = sum(OGCRUDEREF(1:8,1:MNCRUD,IY)) / 365. * 1000. - T105(059,IY,IS)
       ENDDO

!  Table 106:  crude oil refinery inputs

       DO IY = 1,LASTYR
! region 1:
          T106(01:11,IY,IS) = RFCRUDEINP(1,1:11,IY)
          T106(12,IY,IS) = FSUM(T106(01,IY,IS),11)
! region 2:
          T106(13:23,IY,IS) = RFCRUDEINP(2,1:11,IY)
          T106(24,IY,IS) = FSUM(T106(13,IY,IS),11)
! region 3:
          T106(25:35,IY,IS) = RFCRUDEINP(3,1:11,IY)
          T106(36,IY,IS) = FSUM(T106(25,IY,IS),11)
! region 4:
          T106(37:47,IY,IS) = RFCRUDEINP(4,1:11,IY)
          T106(48,IY,IS) = FSUM(T106(37,IY,IS),11)
! region 5:
          T106(49:59,IY,IS) = RFCRUDEINP(5,1:11,IY)
          T106(60,IY,IS) = FSUM(T106(49,IY,IS),11)
! region 6:
          T106(61:71,IY,IS) = RFCRUDEINP(6,1:11,IY)
          T106(72,IY,IS) = FSUM(T106(61,IY,IS),11)
! region 7:
          T106(73:83,IY,IS) = RFCRUDEINP(7,1:11,IY)
          T106(84,IY,IS) = FSUM(T106(73,IY,IS),11)
! region 8:
          T106(85:95,IY,IS) = RFCRUDEINP(8,1:11,IY)
          T106(96,IY,IS) = FSUM(T106(85,IY,IS),11)
!  total United States:
          DO PADD=1,11             ! not PADDS but crude types, but PADD exists
            T106(96+PADD,IY,IS) = sum(RFCRUDEINP(1:8,PADD,IY))
          ENDDO
          T106(108,IY,IS) = FSUM(T106(97,IY,IS),11)
! region 9:
          T106(109:119,IY,IS) = RFCRUDEINP(9,1:11,IY)
          T106(120,IY,IS) = FSUM(T106(109,IY,IS),11)
       ENDDO

!  TABLE 107 Breaking Table 1 up into fuels - Petroleum, Natural Gas, Coal to allocate the discrepancy

      DO IY=1,LASTYR
        RDAYS = 365.        ! dont adjust for leap year

!Petroleum
        T107( 1,IY,IS) = RFQTDCRD(MNUMOR+2,IY)*RDAYS*CFCRDDOM(IY)*.001         ! Crude oil production
        T107( 3,IY,IS) = RFQICRD(MNUMPR,IY) * CFCRDIMP(IY) * RDAYS / 1000.     ! Crude oil imports
        T107( 5,IY,IS) = RFQEXCRD(MNUMPR,IY) * CFCRDEXP(IY) * RDAYS / 1000000. ! Crude oil exports
        T107( 4,IY,IS) = RFCRDOTH(MNUMPR,IY) * CFCRDDOM(IY) * RDAYS / 1000.    ! Other crude oil supply
        T107(126,IY,IS)= RFPQIPRDT(MNUMPR,IY,2) * CFIMPRD(IY) * RDAYS / 1000.
        T107(127,IY,IS)= RFPQUFC(MNUMPR,IY,2) * CFIMUO(IY) * RDAYS / 1000.
        T107(128,IY,IS)= RFIPQCBOB(MNUMPR,IY,2) * CFCBOB(IY) * RDAYS / 1000000.
        T107(129,IY,IS)= RFIPQRBOB(MNUMPR,IY,2) * CFRBOB(IY) * RDAYS / 1000000.
        T107(130,IY,IS)= RFMTBI(MNUMPR,IY) * 4.24 * RDAYS / 1000.
        T107( 6,IY,IS) = RFQEXPRDT(MNUMPR,IY) * CFEXPRD(IY) * RDAYS / 1000.    ! Product exports
        T107(131,IY,IS)= PRDSTKWDR(MNUMPR,IY) * CFCRDDOM(IY) * RDAYS / 1000.   ! Product stock withdrawal
        T107( 2,IY,IS) = RFQNGPL(MNUMPR,IY,6)*RDAYS*CFNGL(IY) / 1000000.       ! Natural gas plant liquids
        T107(132,IY,IS)= 0.9751 * CRNETHCD(MNUMCR,IY) * RDAYS * CFPET / 1000000.
        T107(133,IY,IS)= 0.9751 * CLLETHCD(MNUMCR,IY) * RDAYS * CFPET / 1000000.
        T107(86,IY,IS) = 0.9751 * OTHETHCD(MNUMCR,IY) * RDAYS * CFPET / 1000000.
        T107(134,IY,IS)= 0.9751 * GRNETHCD(MNUMCR,IY) * RDAYS * CFPET / 1000000.
        T107(135,IY,IS)= ETHSTKCHG(IY) * RDAYS * CFETQ(IY) / 1000000.
        T107(87,IY,IS) = sum(BIMQTYCD(1:4,MNUMCR,IY)) * CFBIOD(IY) * RDAYS / 1000000.
        T107(136,IY,IS)= BIODSTKCHG(IY) * RDAYS * CFBIOD(IY) / 1000000.
        T107(118,IY,IS)= RFBIOBUTECD(MNUMCR,IY) * RDAYS * CFBIOBUTE(IY) / 1000000.
        T107(137,IY,IS)= BIOBUTESTK(IY) * RDAYS * CFBIOBUTE(IY) / 1000000.
        T107(88,IY,IS) = GRN2MGQTY(MNUMPR,IY) * CFBIOD(IY) * RDAYS / 1000000.
        T107(82,IY,IS) = GRD2DSQTY(MNUMPR,IY) * CFBIOD(IY) * RDAYS / 1000000.
        T107(84,IY,IS) = RFHCXH2IN(MNUMPR,IY) * CFRSQ * RDAYS / 1000.
        T107(81,IY,IS) =(sum(BTLFRAC(1:4,MNUMPR,IY)) * CFBTLLIQ(IY))*RDAYS/1000000. + &
                        (sum(CBTLFRAC(2,1:4,MNUMPR,IY)) * CFCBTLLIQ(2,IY))*RDAYS/1000000.
        T107(83,IY,IS) = UBAVOL(MNUMPR,IY)*5.763*RDAYS/1000000.
        T107(138,IY,IS)= RFMETM85(MNUMPR,IY) * CFM85Q(IY) * RDAYS / 1000.
        T107( 7,IY,IS) = QTPAS(11,IY)
        T107(139,IY,IS)= QETTR(11,IY)
        T107(140,IY,IS)= QMETR(11,IY)

!Natural Gas
        T107( 9,IY,IS) =CFNGC(IY) * (OGQNGREP(1,IY) + OGQNGREP(2,IY) + &       ! Dry gas production
                                     OGQNGREP(3,IY) + OGQNGREP(4,IY) + &
                                     OGQNGREP(5,IY) + OGQNGREP(6,IY) + &
                                     OGQNGREP(7,IY) + OGQNGREP(8,IY) + OGSHALENG(IY)) * .001
        T107(10,IY,IS) = OGPRSUP(IY) * CFNGC(IY) * .001                        ! supplemental gas
        T107(11,IY,IS) = NGIMPVOL(4,IY) * CFNGI(IY) * .001                     ! imports
        T107(12,IY,IS) = NGEXPVOL(4,IY) * CFNGE(IY) * .001                     ! exports
        T107(13,IY,IS) = QNGAS(11,IY) + QGPTR(11,IY) + QLPIN(11,IY) +QNGLQ(11,IY) + &        ! consumption
                         QNGRFPD(MNUMPR,IY) - QGTLRF(MNUMCR,IY)               ! add in liquids portion
        T107(106,IY,IS)= NGBAL(IY) * CFNGC(IY) / 1000.
        T107(14,IY,IS) = FSUM(T107(9,IY,IS),3) - FSUM(T107(12,IY,IS),2)        ! discrepancy

!Coal
        T107(15,IY,IS) = CQSBB(3,IY)*.001                                      ! production
        T107(104,IY,IS) = sum(WC_PROD_BTU(11,1:MNUMLR,IY))/1000.               ! waste coal
        T107(16,IY,IS) = CQDBFB(11,7,IY)*.001                                  ! imports
        T107(17,IY,IS) = CQDBFB(11,5,IY)*.001                                  ! exports
        T107(18,IY,IS) = QCLAS(11,IY) + QMCIN(11,IY) + QCLSN(11,IY) - QCTLRF(11,IY)   ! consumption (add in liquid part of ctl)
        T107(19,IY,IS) = FSUM(T107(15,IY,IS),2) + T107(104,IY,IS) - FSUM(T107(17,IY,IS),2)   ! discrepancy
!Petroleum
        T107(20,IY,IS) = RFQTDCRD(MNUMOR+2,IY)                            ! Crude oil production
        T107(21,IY,IS) = RFQNGPL(MNUMPR,IY,6) / 1000.                     ! Natural gas plant liquids
        T107(22,IY,IS) = RFQPRCG(MNUMPR,IY)
        T107(176,IY,IS)= RFCRDOTH(MNUMPR,IY)
        T107(148,IY,IS)= PRDSTKWDR(MNUMPR,IY)
   ! Ethanol
        T107(149,IY,IS)= 0.9751 * CRNETHCD(MNUMCR,IY) / 1000.
        T107(150,IY,IS)= 0.9751 * CLLETHCD(MNUMCR,IY) / 1000.
        T107(151,IY,IS)= 0.9751 * GRNETHCD(MNUMCR,IY) / 1000.
        T107(152,IY,IS)= 0.9751 * OTHETHCD(MNUMCR,IY) / 1000.
        T107(154,IY,IS)= ETHSTKCHG(IY)/1000.
   ! Biodiesel
        T107(155,IY,IS)= sum(BIMQTYCD(1:4,11,IY))/1000.
        T107(157,IY,IS)= BIODSTKCHG(IY)/1000.
   ! Biobutanol
        T107(158,IY,IS)= RFBIOBUTECD(MNUMCR,IY)/1000.
        T107(160,IY,IS)= BIOBUTESTK(IY)/1000.
   ! Other
        T107(161,IY,IS)= GRN2MGQTY(MNUMPR,IY) / 1000.
        T107(162,IY,IS)= GRD2DSQTY(MNUMPR,IY) / 1000.
        T107(163,IY,IS)= UBAVOL(MNUMPR,IY)/1000.
        T107(164,IY,IS)=(sum(CTLFRAC(1:4,MNUMPR,IY)) + sum(CBTLFRAC(1,1:4,MNUMPR,IY))) / 1000.
        T107(165,IY,IS)= sum(GTLFRAC(1:4,MNUMPR,IY)) / 1000.
        T107(166,IY,IS)=(sum(BTLFRAC(1:4,MNUMPR,IY)) + sum(CBTLFRAC(2,1:4,MNUMPR,IY))) / 1000.
        T107(167,IY,IS)= RFHCXH2IN(MNUMPR,IY)              ! natural gas to hydrogen
        T107(168,IY,IS)= RFMETM85(MNUMPR,IY)               ! M85
   ! Imports
        T107(23,IY,IS) = RFQICRD(MNUMPR,IY)
        T107(24,IY,IS) = RFPQIPRDT(MNUMPR,IY,2)
        T107(25,IY,IS) = RFPQUFC(MNUMPR,IY,2)
        T107(145,IY,IS)= RFIPQCBOB(MNUMPR,IY,2) / 1000.
        T107(146,IY,IS)= RFIPQRBOB(MNUMPR,IY,2) / 1000.
        T107(147,IY,IS)= RFMTBI(MNUMPR,IY)
        T107(153,IY,IS)= ETHIMP(11,IY)/1000.
        T107(156,IY,IS)= (BIODIMP(11,IY)+ RENEWDIMP(11,IY))/1000.
        T107(159,IY,IS)= BIOBUTEIMP(IY)/1000.
   ! Exports
        T107(169,IY,IS)= RFQEXCRD(MNUMPR,IY) / 1000.
        T107(170,IY,IS)= RFQEXPRDT(MNUMPR,IY)
        T107(171,IY,IS)= ETHEXP(11,IY) / 1000.
        T107(172,IY,IS)= BIODEXP(11,IY) / 1000.
        T107(173,IY,IS)= BIOBUTEEXP(IY) / 1000.
   ! Product supplied
        T107(26,IY,IS) = 0.0                                              ! build total consumption
        IF (CFPRQ .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + &
                                (QPRRS(11,IY) + QPRCM(11,IY) + QPRTR(11,IY) + QPRIN(11,IY) + &
                                 QPROLENERF(11,IY))/CFPRQ/365.*1000.
        IF (CFEEQ .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QETIN(11,IY)/CFEEQ/365.*1000.
        IF (CFBUQ .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QBUIN(11,IY)/CFBUQ/365.*1000.
        IF (CFIBQ .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QISIN(11,IY)/CFIBQ/365.*1000.
        IF (CFPPQ .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QPPIN(11,IY)/CFPPQ/365.*1000.
        IF (CFMGQ(IY) .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QMGAS(11,IY)/CFMGQ(IY)/365.*1000.
        IF (CFJFK .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QJFTR(11,IY)/CFJFK/365*1000.
        IF (CFDSRS(IY) .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QDSRS(11,IY)/CFDSRS(IY)/365.*1000.
        IF (CFDSCM(IY) .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QDSCM(11,IY)/CFDSCM(IY)/365.*1000.
        IF (CFDSIN(IY) .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QDSIN(11,IY)/CFDSIN(IY)/365.*1000.
        IF (CFDSTR(IY) .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QDSTR(11,IY)/CFDSTR(IY)/365.*1000.
        IF (CFDSEL(IY) .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QDSEL(11,IY)/CFDSEL(IY)/365.*1000.
        IF (CFRSQ .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + (QRLAS(11,IY) + QRHAS(11,IY))/CFRSQ/365.*1000.
        IF (CFPFQ(IY) .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QPFIN(11,IY)/CFPFQ(IY)/365.*1000.
        IF (CFKSQ .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QKSAS(11,IY)/CFKSQ/365.*1000.
        IF (CFPCQ .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QPCAS(11,IY)/CFPCQ/365.*1000.
        IF (CFASQ .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QASIN(11,IY)/CFASQ/365.*1000.
        IF (CFSGQ .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QSGIN(11,IY)/CFSGQ/365.*1000.
        IF (CFOTQ(IY) .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) +(QOTIN(11,IY)-QLUIN(11,IY))/CFOTQ(IY)/365.*1000.
        IF (CFLUQ .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) +(QLUIN(11,IY)+QLUTR(11,IY))/CFLUQ/365.*1000.
        IF (CFAVQ .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QAGTR(11,IY)/CFAVQ/365.*1000.
        IF (CFE85Q(IY) .NE. 0.0) T107(174,IY,IS) = QETTR(11,IY)/CFE85Q(IY)/365.*1000.
        IF (CFM85Q(IY) .NE. 0.0) T107(175,IY,IS) = QMETR(11,IY)/CFM85Q(IY)/365.*1000.
        T107(27,IY,IS) = FSUM(T107(20,IY,IS),6) + FSUM(T107(145,IY,IS),24) + &     ! Discrepancy
                         T107(176,IY,IS) - T107(26,IY,IS) - FSUM(T107(169,IY,IS),7)

!Natural Gas
        T107(28,IY,IS) =(OGQNGREP(1,IY) + OGQNGREP(2,IY) + &                   ! Dry gas production
                         OGQNGREP(3,IY) + OGQNGREP(4,IY) + &
                         OGQNGREP(5,IY) + OGQNGREP(6,IY) + &
                         OGQNGREP(7,IY) + OGQNGREP(8,IY) + OGSHALENG(IY)) * .001
        T107(29,IY,IS) = OGPRSUP(IY) * .001                                    ! supplemental gas
        T107(30,IY,IS) = NGIMPVOL(4,IY) * .001                                 ! imports
        T107(31,IY,IS) = NGEXPVOL(4,IY) * .001                                 ! exports
        T107(32,IY,IS) =  0.0                                                  ! build total consumption
        IF (CFNGN(IY) .NE. 0.0) T107(32,IY,IS) =  T107(32,IY,IS) + &
              (QNGRS(11,IY)+QNGCM(11,IY)+QNGIN(11,IY)+QNGTR(11,IY))/CFNGN(IY)
        IF (CFNGU(IY) .NE. 0.0) T107(32,IY,IS) = T107(32,IY,IS) + QNGEL(11,IY)/CFNGU(IY)
        IF (CFNGC(IY) .NE. 0.0) T107(32,IY,IS) = T107(32,IY,IS) + (QLPIN(11,IY)+QGPTR(11,IY)+QNGLQ(11,IY) + &
                     QNGRFPD(MNUMPR,IY) - QGTLRF(MNUMCR,IY))/CFNGC(IY)
        T107(107,IY,IS)= NGBAL(IY) / 1000.
        T107(33,IY,IS) = FSUM(T107(28,IY,IS),3) - FSUM(T107(31,IY,IS),2)       ! discrepancy

!Coal
        IF (CQSBT(3,IY) .NE. 0.0) T107(34,IY,IS)=CQSBB(3,IY)/CQSBT(3,IY)                     ! production
        T107(105,IY,IS) = sum(WC_PROD_ST(11,1:MNUMLR,IY))                                    ! waste coal
        IF (CQDBFT(11,7,IY) .NE. 0.0) T107(35,IY,IS)=CQDBFB(11,7,IY)/CQDBFT(11,7,IY)         ! imports
        IF (CQDBFT(11,5,IY) .NE. 0.0) T107(36,IY,IS)=CQDBFB(11,5,IY)/CQDBFT(11,5,IY)         ! exports
        T107(37,IY,IS) = 0.0                                                   ! build total consumption
        IF (CQDBFT(11,4,IY) .NE. 0.0) T107(37,IY,IS) = T107(37,IY,IS) + QCLSN(11,IY)/CQDBFT(11,4,IY)*1000.
        IF (CQDBFT(11,1,IY) .NE. 0.0) T107(37,IY,IS) = T107(37,IY,IS) + QCLRS(11,IY)/CQDBFT(11,1,IY)*1000.
        IF (CQDBFT(11,1,IY) .NE. 0.0) T107(37,IY,IS) = T107(37,IY,IS) + QCLCM(11,IY)/CQDBFT(11,1,IY)*1000.
IF (IY .GT. (CUMCAPADD+2)) THEN
        IF (CQDBFT(11,2,IY) .NE. 0.0) T107(37,IY,IS) = T107(37,IY,IS) +(QCLIN(11,IY)-QCTLRF(11,IY))/CQDBFT(11,2,IY)*1000.
ELSE
        IF (CQDBFT(11,2,IY) .NE. 0.0) T107(37,IY,IS) = T107(37,IY,IS) + QCLIN(11,IY)/CQDBFT(11,2,IY)*1000.
ENDIF
        IF (CQDBFT(11,3,IY) .NE. 0.0) T107(37,IY,IS) = T107(37,IY,IS) + QMCIN(11,IY)/CQDBFT(11,3,IY)*1000.
        IF (CQDBFT(11,6,IY) .NE. 0.0) T107(37,IY,IS) = T107(37,IY,IS) + QCLEL(11,IY)/CQDBFT(11,6,IY)*1000.
        T107(38,IY,IS) = FSUM(T107(34,IY,IS),2) + T107(105,IY,IS) - FSUM(T107(36,IY,IS),2)   ! discrepancy

! Breaking Petroleum into product balances
        T107(80,IY,IS) = (CRNETHCD(11,IY)+CLLETHCD(11,IY)+OTHETHCD(11,IY)+GRNETHCD(11,IY)+ETHIMP(11,IY)-ETHEXP(11,IY))/1000.
        T107(40,IY,IS) =(RFIPQMG(MNUMPR,IY,2) + RFIPQRG(MNUMPR,IY,2) + &
 RFIPQCBOB(MNUMPR,IY,2) + RFIPQRBOB(MNUMPR,IY,2) + RFIPQCG(MNUMPR,IY,2)) / 1000.
        T107(42,IY,IS) = RFQMG(11,IY)
        T107(41,IY,IS) = QPRDEX(2,IY) + QPRDEX(3, IY) + QPRDEX(4,IY) + QPRDEX(5,IY) + QPRDEX(26,IY)
        T107(39,IY,IS) = T107(42,IY,IS) + T107(41,IY,IS) - T107(40,IY,IS) - T107(80,IY,IS)
        T107(116,IY,IS)=(sum(BIMQTYCD(1:4,11,IY)) + sum(GTLFRAC(1:4,MNUMPR,IY)) + &
                         sum(CTLFRAC(1:4,MNUMPR,IY)) + sum(BTLFRAC(1:4,MNUMPR,IY)) + &
                         sum(CBTLFRAC(1:2,1:4,MNUMPR,IY)) + &
                         SBO2GDTPD(MNUMPR,IY) + YGR2GDTPD(MNUMPR,IY) + WGR2GDTPD(MNUMPR,IY))/1000.
        T107(44,IY,IS) =(RFIPQDS(MNUMPR,IY,2) + RFIPQDL(MNUMPR,IY,2) + RFIPQDU(MNUMPR,IY,2) + &
                         BIODIMP(11,IY) - BIODEXP(11,IY)+ RENEWDIMP(11,IY)) / 1000.
        T107(46,IY,IS) = RFQDS(11,IY)
        T107(45,IY,IS) = QPRDEX(7,IY) + QPRDEX(13,IY) + QPRDEX(24,IY) + QPRDEX(25,IY)
        T107(43,IY,IS) = T107(46,IY,IS) + T107(45,IY,IS) - T107(44,IY,IS) - T107(116,IY,IS)
        T107(48,IY,IS) =(RFIPQRL(MNUMPR,IY,2) + RFIPQRH(MNUMPR,IY,2)) / 1000.
        T107(50,IY,IS) = RFQRL(11,IY) + RFQRH(11,IY)
        T107(49,IY,IS) = QPRDEX(8,IY) + QPRDEX(9,IY)
        T107(47,IY,IS) = T107(50,IY,IS) + T107(49,IY,IS) - T107(48,IY,IS)
        T107(52,IY,IS) = RFIPQJF(MNUMPR,IY,2) / 1000.
        T107(54,IY,IS) = RFQJF(11,IY)
        T107(53,IY,IS) = QPRDEX(6,IY)
        T107(51,IY,IS) = T107(54,IY,IS) + T107(53,IY,IS) - T107(52,IY,IS)
        T107(70,IY,IS) = RFIPQPF(MNUMPR,IY,2) / 1000.
        T107(72,IY,IS) = RFQPF(11,IY)
        T107(71,IY,IS) = QPRDEX(11,IY)
        T107(69,IY,IS) = T107(72,IY,IS) + T107(71,IY,IS) - T107(70,IY,IS)

        T107(177,IY,IS) = RFQNGPL(MNUMPR,IY,1) / 1000.         ! Ethane
        T107(179,IY,IS) = RFIPQET(MNUMPR,IY,2) / 1000.
        T107(180,IY,IS) = QPRDEX(27,IY)
        T107(181,IY,IS) = 0.0  !  for now.  Next line will be "net refinery outputs" (implied)
        T107(182,IY,IS) = QETIN(MNUMCR,IY) / CFEEQ / 365 * 1000.
        T107(178,IY,IS) = FSUM(T107(180,IY,IS),3) - T107(177,IY,IS) - T107(179,IY,IS)
        T107(55,IY,IS) = RFQNGPL(MNUMPR,IY,2) / 1000.                  !  Propane takes over for LPG
        T107(57,IY,IS) = RFIPQPR(MNUMPR,IY,2) / 1000.
        T107(59,IY,IS) =(QPRRS(MNUMCR,IY)+QPRCM(MNUMCR,IY)+QPRIN(MNUMCR,IY)+QPRTR(MNUMCR,IY))/CFPRQ/365.*1000.
        T107(58,IY,IS) = QPRDEX(1,IY)
        T107(85,IY,IS) = 0.0  !  for now.  Next line will be "net refinery outputs" (implied)
        T107(56,IY,IS) = T107(59,IY,IS) + T107(58,IY,IS) + T107(85,IY,IS) - T107(55,IY,IS) - T107(57,IY,IS)
        T107(183,IY,IS) = RFQNGPL(MNUMPR,IY,3) / 1000.         ! Butane
        T107(185,IY,IS) = RFIPQBU(MNUMPR,IY,2) / 1000.
        T107(186,IY,IS) = QPRDEX(15,IY)
        T107(187,IY,IS) = 0.0  !  for now.  Next line will be "net refinery outputs" (implied)
        T107(188,IY,IS) = QBUIN(MNUMCR,IY) / CFBUQ / 365 * 1000.
        T107(184,IY,IS) = FSUM(T107(186,IY,IS),3) - T107(183,IY,IS) - T107(185,IY,IS)
        T107(189,IY,IS) = RFQNGPL(MNUMPR,IY,4) / 1000.         ! Isobutane
        T107(191,IY,IS) = RFIPQIS(MNUMPR,IY,2) / 1000.
        T107(192,IY,IS) = QPRDEX(28,IY)
        T107(193,IY,IS) = 0.0  !  for now.  Next line will be "net refinery outputs" (implied)
        T107(194,IY,IS) = QISIN(MNUMCR,IY) / CFIBQ / 365 * 1000.
        T107(190,IY,IS) = FSUM(T107(192,IY,IS),3) - T107(189,IY,IS) - T107(191,IY,IS)
        T107(195,IY,IS) = 0.0                          ! Propylene
        T107(197,IY,IS) = RFIPQPY(MNUMPR,IY,2) / 1000.
        T107(198,IY,IS) = QPRDEX(14,IY)
        T107(199,IY,IS) = 0.0  !  for now.  Next line will be "net refinery outputs" (implied)
        T107(200,IY,IS) = QPROLENERF(MNUMCR,IY) / CFPRQ / 365 * 1000.
        T107(196,IY,IS) = FSUM(T107(198,IY,IS),3) - T107(195,IY,IS) - T107(197,IY,IS)
        T107(201,IY,IS) = RFQNGPL(MNUMPR,IY,5) / 1000.         ! Natural gasoline (pentanes plus)
        T107(203,IY,IS) = RFIPQPP(MNUMPR,IY,2) / 1000.
        T107(204,IY,IS) = QPRDEX(29,IY)
        T107(205,IY,IS) = 0.0  !  for now.  Next line will be "net refinery outputs" (implied)
        T107(206,IY,IS) = QPPIN(MNUMCR,IY) / CFPPQ / 365 * 1000.
        T107(202,IY,IS) = FSUM(T107(204,IY,IS),3) - T107(201,IY,IS) - T107(203,IY,IS)

        T107(62,IY,IS) = RFQPCK(11,IY)
        T107(61,IY,IS) = QPRDEX(16,IY)
        T107(68,IY,IS) = 0.0
        T107(60,IY,IS) = T107(62,IY,IS) + T107(61,IY,IS) - T107(68,IY,IS)
        T107(64,IY,IS) = RFQSTG(11,IY)
        T107(63,IY,IS) = T107(64,IY,IS)
        T107(66,IY,IS) = RFQARO(11,IY)
        T107(73,IY,IS) = QPRDEX(12,IY)
        T107(89,IY,IS) = RFIPQAS(MNUMPR,IY,2) / 1000.
        T107(65,IY,IS) = T107(66,IY,IS) + T107(73,IY,IS) - T107(89,IY,IS)
        T107(67,IY,IS) = RFPQUFC(MNUMPR,IY,2)
        T107(207,IY,IS) = QPRDEX(20,IY) + QPRDEX(21,IY) + QPRDEX(10,IY) + QPRDEX(23,IY)
        T107(208,IY,IS) = T107(67,IY,IS) - T107(207,IY,IS)
        T107(209,IY,IS) = RFIPQAR3(MNUMPR,IY,2) / 1000.
        T107(210,IY,IS) = RFIPQMN3(MNUMPR,IY,2) / 1000.
        T107(211,IY,IS) = RFIPQGO3(MNUMPR,IY,2) / 1000.
        T107(212,IY,IS) = QPRDEX(20,IY)
        T107(213,IY,IS) = QPRDEX(21,IY)
        T107(214,IY,IS) = QPRDEX(10,IY)
        T107(215,IY,IS) = QPRDEX(23,IY)
        T107(75,IY,IS) =(RFIPQLU(MNUMPR,IY,2)+RFIPQAG(MNUMPR,IY,2))/1000.
        T107(76,IY,IS) = 0.0
        T107(78,IY,IS) = RFQOTH(11,IY) + RFQKS(11,IY)
        T107(77,IY,IS) = QPRDEX(18,IY) + QPRDEX(19,IY) + QPRDEX(20,IY) + QPRDEX(21,IY)
        T107(74,IY,IS) = T107(78,IY,IS) + T107(77,IY,IS) + T107(76,IY,IS) - T107(75,IY,IS)

!  odd balances (CTL, GTL, Ethanol), not that there's anything wrong with that
        T107(90,IY,IS) = QCLSN(11,IY)
        T107(91,IY,IS) = sum(CTLFRAC(1:4,MNUMPR,IY))*RDAYS/1000000.*CFCTLLIQ(IY) + &
                         (sum(CBTLFRAC(1,1:4,MNUMPR,IY)) * CFCBTLLIQ(1,IY))*RDAYS/1000000.
        T107(92,IY,IS) = QCTLRF(11,IY)
        T107(93,IY,IS) = QGTLSN(11,IY)
        T107(95,IY,IS) = sum(GTLFRAC(1:4,MNUMPR,IY))*RDAYS/1000000.*CFGTLLIQ(IY)
        T107(96,IY,IS) = QGTLRF(11,IY)
        T107(97,IY,IS) = CORNCD(1,11,IY)*CFCORN / 1000000000.
        IF (CONEFF(IY) .NE. 0.0) &
        T107(98,IY,IS) = 0.9751*CLLETHCD(11,IY) * RDAYS * CFBMQ(IY) * 42. / CONEFF(IY) / 1000000.
        T107(99,IY,IS) = ETHIMP(11,IY) *RDAYS/1000000.*CFPET
        T107(100,IY,IS) = ETHEXP(11,IY) *RDAYS/1000000.*CFPET
        T107(101,IY,IS) =(0.9751*(CRNETHCD(11,IY)+CLLETHCD(11,IY)+OTHETHCD(11,IY))+ETHIMP(11,IY)-ETHEXP(11,IY))/1000. * RDAYS * .001 * CFPET
        T107(102,IY,IS) = CORNCD(1,11,IY) * CFCORN / 1000000000. -0.9751*(CRNETHCD(11,IY)+OTHETHCD(11,IY)) * 365. * CFPET / 1000000.
        IF (CONEFF(IY) .NE. 0.0) &
        T107(103,IY,IS) = 0.9751*CLLETHCD(11,IY) * RDAYS / 1000000. * (CFBMQ(IY) * 42. / CONEFF(IY) - CFPET)
        T107(121,IY,IS) = CORNCD(2,MNUMCR,IY)*CFCORN / 1000000000.
        T107(122,IY,IS) = BIOBUTEIMP(IY)/1000000.*RDAYS*CFBIOBUTE(IY)
        T107(123,IY,IS) = BIOBUTEEXP(IY)/1000000.*RDAYS*CFBIOBUTE(IY)
        T107(124,IY,IS) = CORNCD(2,MNUMCR,IY)*CFCORN / 1000000000. - &
                          RFBIOBUTECD(MNUMCR,IY)/1000000.*RDAYS*CFBIOBUTE(IY)
        T107(125,IY,IS) = QBIOBUTE(MNUMCR,IY)/1000000.*RDAYS*CFBIOBUTE(IY)
        T107(108,IY,IS) =(BIMQTYCD(1,11,IY)+ &
                          BIMQTYCD(4,11,IY))/1000000.*RDAYS*CFVEGGIE(IY)
        T107(109,IY,IS) = BIMQTYCD(2,11,IY)/1000000.*RDAYS*CFVEGGIE(IY)
        T107(110,IY,IS) = BIMQTYCD(3,11,IY)/1000000.*RDAYS*CFVEGGIE(IY)
        T107(111,IY,IS) = BIODIMP(11,IY)/1000000.*RDAYS*CFBIOD(IY)
        T107(120,IY,IS) = BIODEXP(11,IY)/1000000.*RDAYS*CFBIOD(IY)
        T107(112,IY,IS) = sum(BIMQTYCD(1:4,11,IY))/1000000.*RDAYS*(CFVEGGIE(IY)-CFBIOD(IY))
        T107(113,IY,IS) =(sum(BIMQTYCD(1:4,11,IY))+BIODIMP(11,IY)-BIODEXP(11,IY))/1000000.*RDAYS*CFBIOD(IY)
        T107(117,IY,IS) = QBMRFBTL(11,IY) / 1000.
        T107(119,IY,IS) = T107(117,IY,IS) - T107(81,IY,IS) - T107(83,IY,IS)
!  circling back to calculate discrepancy in the first section:
        T107( 8,IY,IS) =  &
                       T107(  1,IY,IS) + T107(  3,IY,IS) + T107(  4,IY,IS) - T107(  5,IY,IS) + &
                       T107(126,IY,IS) + T107(127,IY,IS) + T107(128,IY,IS) + &
                       T107(129,IY,IS) + T107(130,IY,IS) - T107(  6,IY,IS) + T107(131,IY,IS) + &
                       T107(  2,IY,IS) + T107(132,IY,IS) + T107(133,IY,IS) + T107(134,IY,IS) + &
                       T107( 86,IY,IS) + T107( 99,IY,IS) - T107(100,IY,IS) + T107(135,IY,IS) + &
                       T107( 87,IY,IS) + T107(111,IY,IS) - T107(120,IY,IS) + T107(136,IY,IS) + &
                       T107(118,IY,IS) + T107(122,IY,IS) - T107(123,IY,IS) + T107(137,IY,IS) + &
                       T107( 88,IY,IS) + T107( 82,IY,IS) + T107( 83,IY,IS) + T107( 91,IY,IS) + &
                       T107( 95,IY,IS) + T107( 81,IY,IS) + T107( 84,IY,IS) + T107(138,IY,IS) - &
                       T107(  7,IY,IS) - T107(139,IY,IS) - T107(140,IY,IS)
     ! compare this to the discrepancy shown in table 1:
        T107(115,IY,IS) = T107( 8,IY,IS) + T107(14,IY,IS) + T107(19,IY,IS)
        T107(141,IY,IS) = T1(18,IY,IS)
        T107(142,IY,IS) = T11(50,IY,IS)
        T107(143,IY,IS) = T13(16,IY,IS)
        T107(144,IY,IS) = T15(16,IY,IS)
      ENDDO

!  TABLE 108 Refinery balance

      DO IY=1,LASTYR
! for OGCRDPRD, first dimension is region, second is crude type
          T108(72:73,IY,IS) = OGCRDPRD(1,10:11,IY)
          T108(74:75,IY,IS) = OGCRDPRD(2,10:11,IY)
          T108(76:77,IY,IS) = OGCRDPRD(3,10:11,IY)
          T108(78:79,IY,IS) = OGCRDPRD(4,10:11,IY)
          T108(80:81,IY,IS) = OGCRDPRD(5,10:11,IY)
          T108(82:83,IY,IS) = OGCRDPRD(6,10:11,IY)
          T108( 1: 6,IY,IS) = OGCRDPRD(1,1:6,IY)
          T108( 7:12,IY,IS) = OGCRDPRD(2,1:6,IY)
          T108(13:18,IY,IS) = OGCRDPRD(3,1:6,IY)
          T108(19:24,IY,IS) = OGCRDPRD(4,1:6,IY)
          T108(25:30,IY,IS) = OGCRDPRD(5,1:6,IY)
          T108(31:36,IY,IS) = OGCRDPRD(6,1:6,IY)
          T108(37,IY,IS) = OGCRDPRD( 7,7,IY)
          T108(38,IY,IS) = OGCRDPRD( 8,3,IY)
          T108(39,IY,IS) = OGCRDPRD( 9,3,IY)
          T108(40,IY,IS) = OGCRDPRD(10,7,IY)
          T108(41,IY,IS) = OGCRDPRD(11,3,IY)
          T108(42,IY,IS) = OGCRDPRD(12,3,IY)
          T108(43,IY,IS) = OGCRDPRD(13,3,IY)
          T108(44,IY,IS) = FSUM(T108( 1,IY,IS),6) + FSUM(T108(72,IY,IS),2)
          T108(45,IY,IS) = FSUM(T108( 7,IY,IS),6) + FSUM(T108(74,IY,IS),2)
          T108(46,IY,IS) = FSUM(T108(13,IY,IS),6) + FSUM(T108(76,IY,IS),2)
          T108(47,IY,IS) = FSUM(T108(19,IY,IS),6) + FSUM(T108(78,IY,IS),2)
          T108(48,IY,IS) = FSUM(T108(25,IY,IS),6) + FSUM(T108(80,IY,IS),2)
          T108(49,IY,IS) = FSUM(T108(31,IY,IS),6) + FSUM(T108(82,IY,IS),2)
          T108(50,IY,IS) = FSUM(T108(44,IY,IS),6) + T108(37,IY,IS)
          T108(51,IY,IS) = FSUM(T108(38,IY,IS),3)
          T108(52,IY,IS) = FSUM(T108(41,IY,IS),3)
          T108(53,IY,IS) = sum(OGCRDPRD(1:MNUMOR-1,1:MNCRUD,IY)) - FSUM(T108(50,IY,IS),3)
          T108(54,IY,IS) = FSUM(T108(50,IY,IS),4)
          T108(56,IY,IS) = sum(OGCRDPRD(1:MNUMOR-1,1,IY))
          T108(57,IY,IS) = sum(OGCRDPRD(1:MNUMOR-1,2,IY))
          T108(58,IY,IS) = sum(OGCRDPRD(1:MNUMOR-1,3,IY))
          T108(59,IY,IS) = sum(OGCRDPRD(1:MNUMOR-1,4,IY))
          T108(60,IY,IS) = sum(OGCRDPRD(1:MNUMOR-1,5,IY))
          T108(61,IY,IS) = sum(OGCRDPRD(1:MNUMOR-1,6,IY))
          T108(62,IY,IS) = sum(OGCRDPRD(1:MNUMOR-1,7,IY))
          T108(84,IY,IS) = sum(OGCRDPRD(1:MNUMOR-1,10,IY))
          T108(85,IY,IS) = sum(OGCRDPRD(1:MNUMOR-1,11,IY))
          T108(55,IY,IS) = FSUM(T108(56,IY,IS),7) + FSUM(T108(84,IY,IS),2)
          T108(63:71,IY,IS) = OGCRDHEAT(1:9,IY)
          T108(86:87,IY,IS) = OGCRDHEAT(10:11,IY)
      ENDDO

!  Table 109:  for EPA 111d Electricity Table
!  this is the CO2 Performance Intensity Standard for existing sources -- EMM  Regions

      NUMREP = 9
      DO IY=1,LASTYR
        DO III = 1, num_elec_regions
!  Results are for EMM  Regions in Electricity Model
         T109((III - 1) * NUMREP + 1,IY,IS) = ECO2NRRT(III,IY)           ! Intensity Rate Target
         IF ((EGENNRQF(III,IY)+EGENNREE(III,IY)) .GT. 0.0) &
         T109((III - 1) * NUMREP + 2,IY,IS) = ECO2NRQF(III,IY) * 1000.0 / (EGENNRQF(III,IY) + EGENNREE(III,IY))  ! Intensity Rate Achieved
         T109((III - 1) * NUMREP + 3,IY,IS) = ECO2NRPP(III,IY) * 2.2040 * SCALPR  ! Intensity Standard Allowance Price
         T109((III - 1) * NUMREP + 4,IY,IS) = ECO2NRQY(III,IY)           ! CO2 Emissions target for Affected Plants
         T109((III - 1) * NUMREP + 5,IY,IS) = ECO2NRQF(III,IY) / 2.204   ! CO2 Emissions from Affected Plants
         T109((III - 1) * NUMREP + 6,IY,IS) = ECO2NRTL(III,IY) / 2.204   ! CO2 Emissions from All Plants
         T109((III - 1) * NUMREP + 7,IY,IS) = EGENNRQF(III,IY)           ! Generation from Affected Plants
         T109((III - 1) * NUMREP + 8,IY,IS) = EGENNRTL(III,IY)           ! Generation from All Plants
         T109((III - 1) * NUMREP + 9,IY,IS) = EGENNREE(III,IY)           ! Generation Savings from Energy Efficiency
        END DO
!  National Results
         T109(num_elec_regions*NUMREP+1,IY,IS) = ECO2NRRT(MNUMNR,IY)     ! Intensity Rate Target
         IF ((EGENNRQF(MNUMNR,IY)+EGENNREE(MNUMNR,IY)) .GT. 0.0) &
         T109(num_elec_regions*NUMREP+2,IY,IS) = ECO2NRQF(MNUMNR,IY) * 1000.0 / (EGENNRQF(MNUMNR,IY) + EGENNREE(MNUMNR,IY))  ! Intensity Rate Achieved
         T109(num_elec_regions*NUMREP+3,IY,IS) = ECO2NRPP(MNUMNR,IY) * 2.2040 * SCALPR   ! Intensity Standard Allowance Price
         T109(num_elec_regions*NUMREP+4,IY,IS) = ECO2NRQY(MNUMNR,IY)     ! CO2 Emissions target for Affected Plants
         T109(num_elec_regions*NUMREP+5,IY,IS) = ECO2NRQF(MNUMNR,IY) / 2.204   ! CO2 Emissions from Affected Plants
         T109(num_elec_regions*NUMREP+6,IY,IS) = ECO2NRTL(MNUMNR,IY) / 2.204   ! CO2 Emissions from All Plants
         T109(num_elec_regions*NUMREP+7,IY,IS) = EGENNRQF(MNUMNR,IY)     ! Generation from Affected Plants
         T109(num_elec_regions*NUMREP+8,IY,IS) = EGENNRTL(MNUMNR,IY)     ! Generation from All Plants
         T109(num_elec_regions*NUMREP+9,IY,IS) = EGENNREE(MNUMNR,IY)     ! Generation Savings from Energy Efficiency
      ENDDO

!  Table 110:  Net Negative Revenue Results

      NUMREP = 10
      DO IY=1,LASTYR
        DO III = 1, num_elec_regions
!  Results are for EMM  Regions in Electricity Model
         T110((III - 1) * NUMREP + 1,IY,IS) = UNRVCOL(III,IY)               ! Net Negative Revenues for Coal
         T110((III - 1) * NUMREP + 2,IY,IS) = UNRVCCY(III,IY)               ! Net Negative Revenues for CC
         T110((III - 1) * NUMREP + 3,IY,IS) = UNRVSTM(III,IY)               ! Net Negative Revenues for OGST
         T110((III - 1) * NUMREP + 4,IY,IS) = UNRVNUC(III,IY)               ! Net Negative Revenues for Nuc
         T110((III - 1) * NUMREP + 5,IY,IS) = FSUM(T110((III - 1) * NUMREP + 1,IY,IS),4)
         T110((III - 1) * NUMREP + 6,IY,IS) = UNCPCOL(III,IY) / 1000.0      ! Net Negative Capacity for Coal
         T110((III - 1) * NUMREP + 7,IY,IS) = UNCPCCY(III,IY) / 1000.0      ! Net Negative Capacity for CC
         T110((III - 1) * NUMREP + 8,IY,IS) = UNCPSTM(III,IY) / 1000.0      ! Net Negative Capacity for OGST
         T110((III - 1) * NUMREP + 9,IY,IS) = UNCPNUC(III,IY) / 1000.0      ! Net Negative Capacity for Nuc
         T110((III - 1) * NUMREP +10,IY,IS) = FSUM(T110((III - 1) * NUMREP + 6,IY,IS),4)
        END DO
!  National Results
         T110((MNUMNR-3) * NUMREP + 1,IY,IS) = UNRVCOL(MNUMNR,IY)           ! Net Negative Revenues for Coal
         T110((MNUMNR-3) * NUMREP + 2,IY,IS) = UNRVCCY(MNUMNR,IY)           ! Net Negative Revenues for CC
         T110((MNUMNR-3) * NUMREP + 3,IY,IS) = UNRVSTM(MNUMNR,IY)           ! Net Negative Revenues for OGST
         T110((MNUMNR-3) * NUMREP + 4,IY,IS) = UNRVNUC(MNUMNR,IY)           ! Net Negative Revenues for Nuc
         T110((MNUMNR-3) * NUMREP + 5,IY,IS) = FSUM(T110((MNUMNR-3) * NUMREP + 1,IY,IS),4)
         T110((MNUMNR-3) * NUMREP + 6,IY,IS) = UNCPCOL(MNUMNR,IY) / 1000.0  ! Net Negative Capacity for Coal
         T110((MNUMNR-3) * NUMREP + 7,IY,IS) = UNCPCCY(MNUMNR,IY) / 1000.0  ! Net Negative Capacity for CC
         T110((MNUMNR-3) * NUMREP + 8,IY,IS) = UNCPSTM(MNUMNR,IY) / 1000.0  ! Net Negative Capacity for OGST
         T110((MNUMNR-3) * NUMREP + 9,IY,IS) = UNCPNUC(MNUMNR,IY) / 1000.0  ! Net Negative Capacity for Nuc
         T110((MNUMNR-3) * NUMREP +10,IY,IS) = FSUM(T110((MNUMNR-3) * NUMREP + 6,IY,IS),4)
      ENDDO

! Tables 111 and 112, biomass prices (111) and quantities (112) at the coal region level
!                   PBMPWCL and QBMPWCL are electric power
!                   PBMETCL and QBMETCL are refining sector ethanol
!                   PBMBTCL and QBMBTCL are refining sector liquids production
      DO IY = 1,LASTYR
        T111( 1:16,IY,IS) = PBMPWCL(0,1:16,IY)
IF (sum(QBMPWCL(0,1:16,IY)) .NE. 0.0) &
        T111(161,IY,IS) =(PBMPWCL(0, 1,IY) * QBMPWCL(0, 1,IY) + &
                          PBMPWCL(0, 2,IY) * QBMPWCL(0, 2,IY) + &
                          PBMPWCL(0, 3,IY) * QBMPWCL(0, 3,IY) + &
                          PBMPWCL(0, 4,IY) * QBMPWCL(0, 4,IY) + &
                          PBMPWCL(0, 5,IY) * QBMPWCL(0, 5,IY) + &
                          PBMPWCL(0, 6,IY) * QBMPWCL(0, 6,IY) + &
                          PBMPWCL(0, 7,IY) * QBMPWCL(0, 7,IY) + &
                          PBMPWCL(0, 8,IY) * QBMPWCL(0, 8,IY) + &
                          PBMPWCL(0, 9,IY) * QBMPWCL(0, 9,IY) + &
                          PBMPWCL(0,10,IY) * QBMPWCL(0,10,IY) + &
                          PBMPWCL(0,11,IY) * QBMPWCL(0,11,IY) + &
                          PBMPWCL(0,12,IY) * QBMPWCL(0,12,IY) + &
                          PBMPWCL(0,13,IY) * QBMPWCL(0,13,IY) + &
                          PBMPWCL(0,14,IY) * QBMPWCL(0,14,IY) + &
                          PBMPWCL(0,15,IY) * QBMPWCL(0,15,IY) + &
                          PBMPWCL(0,16,IY) * QBMPWCL(0,16,IY)) / sum(QBMPWCL(0,1:16,IY))
!  urban wood waste
        T111(33:48,IY,IS) = PBMPWCL(1,1:16,IY)
IF (sum(QBMPWCL(1,1:16,IY)) .NE. 0.0) &
        T111(164,IY,IS) =(PBMPWCL(1, 1,IY) * QBMPWCL(1, 1,IY) + &
                          PBMPWCL(1, 2,IY) * QBMPWCL(1, 2,IY) + &
                          PBMPWCL(1, 3,IY) * QBMPWCL(1, 3,IY) + &
                          PBMPWCL(1, 4,IY) * QBMPWCL(1, 4,IY) + &
                          PBMPWCL(1, 5,IY) * QBMPWCL(1, 5,IY) + &
                          PBMPWCL(1, 6,IY) * QBMPWCL(1, 6,IY) + &
                          PBMPWCL(1, 7,IY) * QBMPWCL(1, 7,IY) + &
                          PBMPWCL(1, 8,IY) * QBMPWCL(1, 8,IY) + &
                          PBMPWCL(1, 9,IY) * QBMPWCL(1, 9,IY) + &
                          PBMPWCL(1,10,IY) * QBMPWCL(1,10,IY) + &
                          PBMPWCL(1,11,IY) * QBMPWCL(1,11,IY) + &
                          PBMPWCL(1,12,IY) * QBMPWCL(1,12,IY) + &
                          PBMPWCL(1,13,IY) * QBMPWCL(1,13,IY) + &
                          PBMPWCL(1,14,IY) * QBMPWCL(1,14,IY) + &
                          PBMPWCL(1,15,IY) * QBMPWCL(1,15,IY) + &
                          PBMPWCL(1,16,IY) * QBMPWCL(1,16,IY)) / sum(QBMPWCL(1,1:16,IY))
!  public forestry residue
        T111(65:80,IY,IS) = PBMPWCL(2,1:16,IY)
IF (sum(QBMPWCL(2,1:16,IY)) .NE. 0.0) &
        T111(167,IY,IS) =(PBMPWCL(2, 1,IY) * QBMPWCL(2, 1,IY) + &
                          PBMPWCL(2, 2,IY) * QBMPWCL(2, 2,IY) + &
                          PBMPWCL(2, 3,IY) * QBMPWCL(2, 3,IY) + &
                          PBMPWCL(2, 4,IY) * QBMPWCL(2, 4,IY) + &
                          PBMPWCL(2, 5,IY) * QBMPWCL(2, 5,IY) + &
                          PBMPWCL(2, 6,IY) * QBMPWCL(2, 6,IY) + &
                          PBMPWCL(2, 7,IY) * QBMPWCL(2, 7,IY) + &
                          PBMPWCL(2, 8,IY) * QBMPWCL(2, 8,IY) + &
                          PBMPWCL(2, 9,IY) * QBMPWCL(2, 9,IY) + &
                          PBMPWCL(2,10,IY) * QBMPWCL(2,10,IY) + &
                          PBMPWCL(2,11,IY) * QBMPWCL(2,11,IY) + &
                          PBMPWCL(2,12,IY) * QBMPWCL(2,12,IY) + &
                          PBMPWCL(2,13,IY) * QBMPWCL(2,13,IY) + &
                          PBMPWCL(2,14,IY) * QBMPWCL(2,14,IY) + &
                          PBMPWCL(2,15,IY) * QBMPWCL(2,15,IY) + &
                          PBMPWCL(2,16,IY) * QBMPWCL(2,16,IY)) / sum(QBMPWCL(2,1:16,IY))
!  agricultural residue and energy crops
        T111( 97:112,IY,IS) = PBMPWCL(3,1:16,IY)
IF (sum(QBMPWCL(3,1:16,IY)) .NE. 0.0) &
        T111(170,IY,IS) =(PBMPWCL(3, 1,IY) * QBMPWCL(3, 1,IY) + &
                          PBMPWCL(3, 2,IY) * QBMPWCL(3, 2,IY) + &
                          PBMPWCL(3, 3,IY) * QBMPWCL(3, 3,IY) + &
                          PBMPWCL(3, 4,IY) * QBMPWCL(3, 4,IY) + &
                          PBMPWCL(3, 5,IY) * QBMPWCL(3, 5,IY) + &
                          PBMPWCL(3, 6,IY) * QBMPWCL(3, 6,IY) + &
                          PBMPWCL(3, 7,IY) * QBMPWCL(3, 7,IY) + &
                          PBMPWCL(3, 8,IY) * QBMPWCL(3, 8,IY) + &
                          PBMPWCL(3, 9,IY) * QBMPWCL(3, 9,IY) + &
                          PBMPWCL(3,10,IY) * QBMPWCL(3,10,IY) + &
                          PBMPWCL(3,11,IY) * QBMPWCL(3,11,IY) + &
                          PBMPWCL(3,12,IY) * QBMPWCL(3,12,IY) + &
                          PBMPWCL(3,13,IY) * QBMPWCL(3,13,IY) + &
                          PBMPWCL(3,14,IY) * QBMPWCL(3,14,IY) + &
                          PBMPWCL(3,15,IY) * QBMPWCL(3,15,IY) + &
                          PBMPWCL(3,16,IY) * QBMPWCL(3,16,IY)) / sum(QBMPWCL(3,1:16,IY))
!  private forestry residue
        T111(129:144,IY,IS) = PBMPWCL(4,1:16,IY)
IF (sum(QBMPWCL(4,1:16,IY)) .NE. 0.0) &
        T111(173,IY,IS) =(PBMPWCL(4, 1,IY) * QBMPWCL(4, 1,IY) + &
                          PBMPWCL(4, 2,IY) * QBMPWCL(4, 2,IY) + &
                          PBMPWCL(4, 3,IY) * QBMPWCL(4, 3,IY) + &
                          PBMPWCL(4, 4,IY) * QBMPWCL(4, 4,IY) + &
                          PBMPWCL(4, 5,IY) * QBMPWCL(4, 5,IY) + &
                          PBMPWCL(4, 6,IY) * QBMPWCL(4, 6,IY) + &
                          PBMPWCL(4, 7,IY) * QBMPWCL(4, 7,IY) + &
                          PBMPWCL(4, 8,IY) * QBMPWCL(4, 8,IY) + &
                          PBMPWCL(4, 9,IY) * QBMPWCL(4, 9,IY) + &
                          PBMPWCL(4,10,IY) * QBMPWCL(4,10,IY) + &
                          PBMPWCL(4,11,IY) * QBMPWCL(4,11,IY) + &
                          PBMPWCL(4,12,IY) * QBMPWCL(4,12,IY) + &
                          PBMPWCL(4,13,IY) * QBMPWCL(4,13,IY) + &
                          PBMPWCL(4,14,IY) * QBMPWCL(4,14,IY) + &
                          PBMPWCL(4,15,IY) * QBMPWCL(4,15,IY) + &
                          PBMPWCL(4,16,IY) * QBMPWCL(4,16,IY)) / sum(QBMPWCL(4,1:16,IY))
        DO IR=1,16
              IF ((QBMETCL(0,IR,IY) + QBMBTCL(0,IR,IY)) .NE. 0.0) T111(16+IR,IY,IS) = &
                 (PBMETCL(0,IR,IY) * QBMETCL(0,IR,IY) + PBMBTCL(0,IR,IY) * QBMBTCL(0,IR,IY)) / (QBMETCL(0,IR,IY) + QBMBTCL(0,IR,IY))
IF (sum(QBMETCL(0,1:16,IY))+sum(QBMBTCL(0,1:16,IY)) .NE. 0.0) T111(162,IY,IS) = T111(162,IY,IS) + &
                  (PBMETCL(0,IR,IY) * QBMETCL(0,IR,IY) + PBMBTCL(0,IR,IY) * QBMBTCL(0,IR,IY))/ &
                  (sum(QBMETCL(0,1:16,IY)) + sum(QBMBTCL(0,1:16,IY)))
!  urban wood waste
              IF ((QBMETCL(1,IR,IY) + QBMBTCL(1,IR,IY)) .NE. 0.0) T111(48+IR,IY,IS) = &
                 (PBMETCL(1,IR,IY) * QBMETCL(1,IR,IY) + PBMBTCL(1,IR,IY) * QBMBTCL(1,IR,IY)) / (QBMETCL(1,IR,IY) + QBMBTCL(1,IR,IY))
IF (sum(QBMETCL(1,1:16,IY))+sum(QBMBTCL(1,1:16,IY)) .NE. 0.0) T111(165,IY,IS) = T111(165,IY,IS) + &
                  (PBMETCL(1,IR,IY) * QBMETCL(1,IR,IY) + PBMBTCL(1,IR,IY) * QBMBTCL(1,IR,IY))/ &
                  (sum(QBMETCL(1,1:16,IY)) + sum(QBMBTCL(1,1:16,IY)))
!  public forestry residue
              IF ((QBMETCL(2,IR,IY) + QBMBTCL(2,IR,IY)) .NE. 0.0) T111(80+IR,IY,IS) = &
                 (PBMETCL(2,IR,IY) * QBMETCL(2,IR,IY) + PBMBTCL(2,IR,IY) * QBMBTCL(2,IR,IY)) / (QBMETCL(2,IR,IY) + QBMBTCL(2,IR,IY))
IF (sum(QBMETCL(2,1:16,IY))+sum(QBMBTCL(2,1:16,IY)) .NE. 0.0) T111(168,IY,IS) = T111(168,IY,IS) + &
                  (PBMETCL(2,IR,IY) * QBMETCL(2,IR,IY) + PBMBTCL(2,IR,IY) * QBMBTCL(2,IR,IY))/ &
                  (sum(QBMETCL(2,1:16,IY)) + sum(QBMBTCL(2,1:16,IY)))
!  agricultural residue and energy crops
              IF ((QBMETCL(3,IR,IY) + QBMBTCL(3,IR,IY)) .NE. 0.0) T111(112+IR,IY,IS) = &
                 (PBMETCL(3,IR,IY) * QBMETCL(3,IR,IY) + PBMBTCL(3,IR,IY) * QBMBTCL(3,IR,IY)) / (QBMETCL(3,IR,IY) + QBMBTCL(3,IR,IY))
IF (sum(QBMETCL(3,1:16,IY))+sum(QBMBTCL(3,1:16,IY)) .NE. 0.0) T111(171,IY,IS) = T111(171,IY,IS) + &
                  (PBMETCL(3,IR,IY) * QBMETCL(3,IR,IY) + PBMBTCL(3,IR,IY) * QBMBTCL(3,IR,IY))/ &
                  (sum(QBMETCL(3,1:16,IY)) + sum(QBMBTCL(3,1:16,IY)))
!  private forestry residue
              IF ((QBMETCL(4,IR,IY) + QBMBTCL(4,IR,IY)) .NE. 0.0) T111(144+IR,IY,IS) = &
                 (PBMETCL(4,IR,IY) * QBMETCL(4,IR,IY) + PBMBTCL(4,IR,IY) * QBMBTCL(4,IR,IY)) / (QBMETCL(4,IR,IY) + QBMBTCL(4,IR,IY))
IF (sum(QBMETCL(4,1:16,IY))+sum(QBMBTCL(4,1:16,IY)) .NE. 0.0) T111(174,IY,IS) = T111(174,IY,IS) + &
                  (PBMETCL(4,IR,IY) * QBMETCL(4,IR,IY) + PBMBTCL(4,IR,IY) * QBMBTCL(4,IR,IY))/ &
                  (sum(QBMETCL(4,1:16,IY)) + sum(QBMBTCL(4,1:16,IY)))
        ENDDO
IF (sum(QBMPWCL(0,1:16,IY))+sum(QBMETCL(0,1:16,IY))+sum(QBMBTCL(0,1:16,IY)) .NE. 0.0) &
        T111(163,IY,IS) =(T111(161,IY,IS) * sum(QBMPWCL(0,1:16,IY)) + &
                          T111(162,IY,IS) *(sum(QBMETCL(0,1:16,IY))+sum(QBMBTCL(0,1:16,IY))))/ &
                   (sum(QBMPWCL(0,1:16,IY))+sum(QBMETCL(0,1:16,IY))+sum(QBMBTCL(0,1:16,IY)))
IF (sum(QBMPWCL(1,1:16,IY))+sum(QBMETCL(1,1:16,IY))+sum(QBMBTCL(1,1:16,IY)) .NE. 0.0) &
        T111(166,IY,IS) =(T111(164,IY,IS) * sum(QBMPWCL(1,1:16,IY)) + &
                          T111(165,IY,IS) *(sum(QBMETCL(1,1:16,IY))+sum(QBMBTCL(1,1:16,IY))))/ &
                   (sum(QBMPWCL(1,1:16,IY))+sum(QBMETCL(1,1:16,IY))+sum(QBMBTCL(1,1:16,IY)))
IF (sum(QBMPWCL(2,1:16,IY))+sum(QBMETCL(2,1:16,IY))+sum(QBMBTCL(2,1:16,IY)) .NE. 0.0) &
        T111(169,IY,IS) =(T111(167,IY,IS) * sum(QBMPWCL(2,1:16,IY)) + &
                          T111(168,IY,IS) *(sum(QBMETCL(2,1:16,IY))+sum(QBMBTCL(2,1:16,IY))))/ &
                   (sum(QBMPWCL(2,1:16,IY))+sum(QBMETCL(2,1:16,IY))+sum(QBMBTCL(2,1:16,IY)))
IF (sum(QBMPWCL(3,1:16,IY))+sum(QBMETCL(3,1:16,IY))+sum(QBMBTCL(3,1:16,IY)) .NE. 0.0) &
        T111(172,IY,IS) =(T111(170,IY,IS) * sum(QBMPWCL(3,1:16,IY)) + &
                          T111(171,IY,IS) *(sum(QBMETCL(3,1:16,IY))+sum(QBMBTCL(3,1:16,IY))))/ &
                   (sum(QBMPWCL(3,1:16,IY))+sum(QBMETCL(3,1:16,IY))+sum(QBMBTCL(3,1:16,IY)))
IF (sum(QBMPWCL(4,1:16,IY))+sum(QBMETCL(4,1:16,IY))+sum(QBMBTCL(4,1:16,IY)) .NE. 0.0) &
        T111(175,IY,IS) =(T111(173,IY,IS) * sum(QBMPWCL(4,1:16,IY)) + &
                          T111(174,IY,IS) *(sum(QBMETCL(4,1:16,IY))+sum(QBMBTCL(4,1:16,IY))))/ &
                   (sum(QBMPWCL(4,1:16,IY))+sum(QBMETCL(4,1:16,IY))+sum(QBMBTCL(4,1:16,IY)))
      ENDDO

      DO IY = 1,LASTYR
        T112( 1:16,IY,IS) = QBMPWCL(0,1:16,IY)
        T112(17:32,IY,IS) = QBMETCL(0,1:16,IY) + QBMBTCL(0,1:16,IY)
        T112(161,IY,IS) = FSUM(T112( 1,IY,IS),16)
        T112(162,IY,IS) = FSUM(T112(17,IY,IS),16)
        T112(163,IY,IS) = T112(161,IY,IS) + T112(162,IY,IS)
!  urban wood waste
        T112(33:48,IY,IS) = QBMPWCL(1,1:16,IY)
        T112(49:64,IY,IS) = QBMETCL(1,1:16,IY) + QBMBTCL(1,1:16,IY)
        T112(164,IY,IS) = FSUM(T112(33,IY,IS),16)
        T112(165,IY,IS) = FSUM(T112(49,IY,IS),16)
        T112(166,IY,IS) = T112(164,IY,IS) + T112(165,IY,IS)
!  forestry residue
        T112(65:80,IY,IS) = QBMPWCL(2,1:16,IY)
        T112(81:96,IY,IS) = QBMETCL(2,1:16,IY) + QBMBTCL(2,1:16,IY)
        T112(167,IY,IS) = FSUM(T112(65,IY,IS),16)
        T112(168,IY,IS) = FSUM(T112(81,IY,IS),16)
        T112(169,IY,IS) = T112(167,IY,IS) + T112(168,IY,IS)
!  agricultural residue and energy crops
        T112( 97:112,IY,IS) = QBMPWCL(3,1:16,IY)
        T112(113:128,IY,IS) = QBMETCL(3,1:16,IY) + QBMBTCL(3,1:16,IY)
        T112(170,IY,IS) = FSUM(T112( 97,IY,IS),16)
        T112(171,IY,IS) = FSUM(T112(113,IY,IS),16)
        T112(172,IY,IS) = T112(170,IY,IS) + T112(171,IY,IS)
!  private forestry residue
        T112(129:144,IY,IS) = QBMPWCL(4,1:16,IY)
        T112(145:160,IY,IS) = QBMETCL(4,1:16,IY) + QBMBTCL(4,1:16,IY)
        T112(173,IY,IS) = FSUM(T112(129,IY,IS),16)
        T112(174,IY,IS) = FSUM(T112(145,IY,IS),16)
        T112(175,IY,IS) = T112(173,IY,IS) + T112(174,IY,IS)
      ENDDO

! TABLE 113 - New Light-Duty Vehicle Fuel Economy

      NUMREP=TNUMCLASS-1          ! number of vehicle classes, same for tables 113, 114, and 115
      DO IY=1,LASTYR
        DO II=1,8
!        --- GASOLINE
         T113( 0*NUMREP+II,IY,IS) = LDVMPG(1, 1,II,1989+IY)
         T113( 1*NUMREP+II,IY,IS) = LDVMPG(2, 1,II,1989+IY)
!        --- TDI DIESEL
         T113( 2*NUMREP+II,IY,IS) = LDVMPG(1, 2,II,1989+IY)
         T113( 3*NUMREP+II,IY,IS) = LDVMPG(2, 2,II,1989+IY)
!        --- PLUG-IN 10 GASOLINE HYBRID
         T113( 4*NUMREP+II,IY,IS) = LDVMPG(1, 5,II,1989+IY)
         T113( 5*NUMREP+II,IY,IS) = LDVMPG(2, 5,II,1989+IY)
!        --- PLUG-IN 40 GASOLINE HYBRID
         T113( 6*NUMREP+II,IY,IS) = LDVMPG(1, 6,II,1989+IY)
         T113( 7*NUMREP+II,IY,IS) = LDVMPG(2, 6,II,1989+IY)
!        --- ETHANOL FLEX
         T113( 8*NUMREP+II,IY,IS) = LDVMPG(1, 3,II,1989+IY)
         T113( 9*NUMREP+II,IY,IS) = LDVMPG(2, 3,II,1989+IY)
!        --- CNG
         T113(10*NUMREP+II,IY,IS) = LDVMPG(1,11,II,1989+IY)
         T113(11*NUMREP+II,IY,IS) = LDVMPG(2,11,II,1989+IY)
!        --- CNG BI-FUEL
         T113(12*NUMREP+II,IY,IS) = LDVMPG(1, 9,II,1989+IY)
         T113(13*NUMREP+II,IY,IS) = LDVMPG(2, 9,II,1989+IY)
!        --- LPG
         T113(14*NUMREP+II,IY,IS) = LDVMPG(1,12,II,1989+IY)
         T113(15*NUMREP+II,IY,IS) = LDVMPG(2,12,II,1989+IY)
!        --- LPG BI-FUEL
         T113(16*NUMREP+II,IY,IS) = LDVMPG(1,10,II,1989+IY)
         T113(17*NUMREP+II,IY,IS) = LDVMPG(2,10,II,1989+IY)
!        --- 100 MILE ELECTRIC
         T113(18*NUMREP+II,IY,IS) = LDVMPG(1, 4,II,1989+IY)
         T113(19*NUMREP+II,IY,IS) = LDVMPG(2, 4,II,1989+IY)
!        --- 200 MILE ELECTRIC
         T113(20*NUMREP+II,IY,IS) = LDVMPG(1, 7,II,1989+IY)
         T113(21*NUMREP+II,IY,IS) = LDVMPG(2, 7,II,1989+IY)
!        --- 300 MILE ELECTRIC
         T113(22*NUMREP+II,IY,IS) = LDVMPG(1,15,II,1989+IY)
         T113(23*NUMREP+II,IY,IS) = LDVMPG(2,15,II,1989+IY)
!        --- DIESEL-ELECTRIC HYBRID
         T113(24*NUMREP+II,IY,IS) = LDVMPG(1, 8,II,1989+IY)
         T113(25*NUMREP+II,IY,IS) = LDVMPG(2, 8,II,1989+IY)
!        --- GASOLINE-ELECTRIC HYBRID
         T113(26*NUMREP+II,IY,IS) = LDVMPG(1,16,II,1989+IY)
         T113(27*NUMREP+II,IY,IS) = LDVMPG(2,16,II,1989+IY)
!        --- FUEL CELL METHANOL
         T113(28*NUMREP+II,IY,IS) = LDVMPG(1,13,II,1989+IY)
         T113(29*NUMREP+II,IY,IS) = LDVMPG(2,13,II,1989+IY)
!        --- FUEL CELL HYDROGEN
         T113(30*NUMREP+II,IY,IS) = LDVMPG(1,14,II,1989+IY)
         T113(31*NUMREP+II,IY,IS) = LDVMPG(2,14,II,1989+IY)
      ENDDO
      ENDDO

! TABLE 114 - New Light-Duty Vehicle Prices
      DO IY=1,LASTYR
        DO II=1,8
!        --- GASOLINE
         T114( 0*NUMREP+II,IY,IS) = LDVPRI(1, 1,II,1989+IY)
         T114( 1*NUMREP+II,IY,IS) = LDVPRI(2, 1,II,1989+IY)
!        --- TDI DIESEL
         T114( 2*NUMREP+II,IY,IS) = LDVPRI(1, 2,II,1989+IY)
         T114( 3*NUMREP+II,IY,IS) = LDVPRI(2, 2,II,1989+IY)
!        --- PLUG-IN 10 GASOLINE HYBRID
         T114( 4*NUMREP+II,IY,IS) = LDVPRI(1, 5,II,1989+IY)
         T114( 5*NUMREP+II,IY,IS) = LDVPRI(2, 5,II,1989+IY)
!        --- PLUG-IN 40 GASOLINE HYBRID
         T114( 6*NUMREP+II,IY,IS) = LDVPRI(1, 6,II,1989+IY)
         T114( 7*NUMREP+II,IY,IS) = LDVPRI(2, 6,II,1989+IY)
!        --- ETHANOL FLEX
         T114( 8*NUMREP+II,IY,IS) = LDVPRI(1, 3,II,1989+IY)
         T114( 9*NUMREP+II,IY,IS) = LDVPRI(2, 3,II,1989+IY)
!        --- CNG
         T114(10*NUMREP+II,IY,IS) = LDVPRI(1,11,II,1989+IY)
         T114(11*NUMREP+II,IY,IS) = LDVPRI(2,11,II,1989+IY)
!        --- CNG BI-FUEL
         T114(12*NUMREP+II,IY,IS) = LDVPRI(1, 9,II,1989+IY)
         T114(13*NUMREP+II,IY,IS) = LDVPRI(2, 9,II,1989+IY)
!        --- LPG
         T114(14*NUMREP+II,IY,IS) = LDVPRI(1,12,II,1989+IY)
         T114(15*NUMREP+II,IY,IS) = LDVPRI(2,12,II,1989+IY)
!        --- LPG BI-FUEL
         T114(16*NUMREP+II,IY,IS) = LDVPRI(1,10,II,1989+IY)
         T114(17*NUMREP+II,IY,IS) = LDVPRI(2,10,II,1989+IY)
!        --- 100 MILE ELECTRIC
         T114(18*NUMREP+II,IY,IS) = LDVPRI(1, 4,II,1989+IY)
         T114(19*NUMREP+II,IY,IS) = LDVPRI(2, 4,II,1989+IY)
!        --- 200 MILE ELECTRIC
         T114(20*NUMREP+II,IY,IS) = LDVPRI(1, 7,II,1989+IY)
         T114(21*NUMREP+II,IY,IS) = LDVPRI(2, 7,II,1989+IY)
!        --- 300 MILE ELECTRIC
         T114(22*NUMREP+II,IY,IS) = LDVPRI(1,15,II,1989+IY)
         T114(23*NUMREP+II,IY,IS) = LDVPRI(2,15,II,1989+IY)
!        --- DIESEL-ELECTRIC HYBRID
         T114(24*NUMREP+II,IY,IS) = LDVPRI(1, 8,II,1989+IY)
         T114(25*NUMREP+II,IY,IS) = LDVPRI(2, 8,II,1989+IY)
!        --- GASOLINE-ELECTRIC HYBRID
         T114(26*NUMREP+II,IY,IS) = LDVPRI(1,16,II,1989+IY)
         T114(27*NUMREP+II,IY,IS) = LDVPRI(2,16,II,1989+IY)
!        --- FUEL CELL METHANOL
         T114(28*NUMREP+II,IY,IS) = LDVPRI(1,13,II,1989+IY)
         T114(29*NUMREP+II,IY,IS) = LDVPRI(2,13,II,1989+IY)
!        --- FUEL CELL HYDROGEN
         T114(30*NUMREP+II,IY,IS) = LDVPRI(1,14,II,1989+IY)
         T114(31*NUMREP+II,IY,IS) = LDVPRI(2,14,II,1989+IY)
        ENDDO
!        --- AVERAGE VEHICLE PRICES - CARS, TRUCKS, AND TOTAL LIGHT DUTY VEHICLES
         T114(257,IY,IS) = AVG_PRC_CAR(IY)
         T114(258,IY,IS) = AVG_PRC_TRK(IY)
         T114(259,IY,IS) = AVG_PRC_VEH(IY)
         T114(260,IY,IS) = FULL_PRC_CAR(IY)
         T114(261,IY,IS) = FULL_PRC_TRK(IY)
         T114(262,IY,IS) = FULL_PRC_VEH(IY)
		 T114(263,IY,IS) = Li_ion_Cost(15,1989+IY) * MC_JPGDP(33)/MC_JPGDP(1)
		 T114(264,IY,IS) = Li_ion_Cost(5,1989+IY)  * MC_JPGDP(33)/MC_JPGDP(1)
		 T114(265,IY,IS) = Li_ion_Cost(16,1989+IY) * MC_JPGDP(33)/MC_JPGDP(1)
      ENDDO

! TABLE 115 - New Light-Duty Vehicle Range
      DO IY=1,LASTYR
        DO II=1,8
!        --- GASOLINE
         T115( 0*NUMREP+II,IY,IS) = LDVRNG(1, 1,II,1989+IY)
         T115( 1*NUMREP+II,IY,IS) = LDVRNG(2, 1,II,1989+IY)
!        --- TDI DIESEL
         T115( 2*NUMREP+II,IY,IS) = LDVRNG(1, 2,II,1989+IY)
         T115( 3*NUMREP+II,IY,IS) = LDVRNG(2, 2,II,1989+IY)
!        --- PLUG-IN 10 GASOLINE HYBRID
         T115( 4*NUMREP+II,IY,IS) = LDVRNG(1, 5,II,1989+IY)
         T115( 5*NUMREP+II,IY,IS) = LDVRNG(2, 5,II,1989+IY)
!        --- PLUG-IN 40 GASOLINE HYBRID
         T115( 6*NUMREP+II,IY,IS) = LDVRNG(1, 6,II,1989+IY)
         T115( 7*NUMREP+II,IY,IS) = LDVRNG(2, 6,II,1989+IY)
!        --- ETHANOL FLEX
         T115( 8*NUMREP+II,IY,IS) = LDVRNG(1, 3,II,1989+IY)
         T115( 9*NUMREP+II,IY,IS) = LDVRNG(2, 3,II,1989+IY)
!        --- CNG
         T115(10*NUMREP+II,IY,IS) = LDVRNG(1,11,II,1989+IY)
         T115(11*NUMREP+II,IY,IS) = LDVRNG(2,11,II,1989+IY)
!        --- CNG BI-FUEL
         T115(12*NUMREP+II,IY,IS) = LDVRNG(1, 9,II,1989+IY)
         T115(13*NUMREP+II,IY,IS) = LDVRNG(2, 9,II,1989+IY)
!        --- LPG
         T115(14*NUMREP+II,IY,IS) = LDVRNG(1,12,II,1989+IY)
         T115(15*NUMREP+II,IY,IS) = LDVRNG(2,12,II,1989+IY)
!        --- LPG BI-FUEL
         T115(16*NUMREP+II,IY,IS) = LDVRNG(1,10,II,1989+IY)
         T115(17*NUMREP+II,IY,IS) = LDVRNG(2,10,II,1989+IY)
!        --- 100 MILE ELECTRIC
         T115(18*NUMREP+II,IY,IS) = LDVRNG(1, 4,II,1989+IY)
         T115(19*NUMREP+II,IY,IS) = LDVRNG(2, 4,II,1989+IY)
!        --- 200 MILE ELECTRIC
         T115(20*NUMREP+II,IY,IS) = LDVRNG(1, 7,II,1989+IY)
         T115(21*NUMREP+II,IY,IS) = LDVRNG(2, 7,II,1989+IY)
!        --- 300 MILE ELECTRIC
         T115(22*NUMREP+II,IY,IS) = LDVRNG(1,15,II,1989+IY)
         T115(23*NUMREP+II,IY,IS) = LDVRNG(2,15,II,1989+IY)
!        --- DIESEL-ELECTRIC HYBRID
         T115(24*NUMREP+II,IY,IS) = LDVRNG(1, 8,II,1989+IY)
         T115(25*NUMREP+II,IY,IS) = LDVRNG(2, 8,II,1989+IY)
!        --- GASOLINE-ELECTRIC HYBRID
         T115(26*NUMREP+II,IY,IS) = LDVRNG(1,16,II,1989+IY)
         T115(27*NUMREP+II,IY,IS) = LDVRNG(2,16,II,1989+IY)
!        --- FUEL CELL METHANOL
         T115(28*NUMREP+II,IY,IS) = LDVRNG(1,13,II,1989+IY)
         T115(29*NUMREP+II,IY,IS) = LDVRNG(2,13,II,1989+IY)
!        --- FUEL CELL HYDROGEN
         T115(30*NUMREP+II,IY,IS) = LDVRNG(1,14,II,1989+IY)
         T115(31*NUMREP+II,IY,IS) = LDVRNG(2,14,II,1989+IY)
         ENDDO
      ENDDO

!     Table 116. Total Resource Costs - Electric Sector

      NUMREP = 3

      DO IY=1,LASTYR
        DO IR=1,MNUMNR

!        Installed Capacity

         TMP(:) = G_INST_ALL(IR,:)
         T116(1,IR,IY,IS) = G_INST_ALL(IR,IY) * SCALPR * 0.001
         IF (IY .GT. CUMCAPADD - 1989) THEN
            T116(11,IR,IY,IS) = NPV(UT_DR,TMP,CUMCAPADD-1989,IY,0,MAXYR-1) * SCALPR * 0.001
         END IF
         TMP(:) = G_ANN(IR,:)
         T116(21,IR,IY,IS) = G_ANN(IR,IY) * SCALPR * 0.001
         IF (IY .GT. CUMCAPADD - 1989) THEN
            T116(31,IR,IY,IS) = NPV(UT_DR,TMP,CUMCAPADD-1989,IY,0,MAXYR-1) * SCALPR * 0.001
         END IF

!        Transmission

         TMP(:) = T_OVR(IR,:)
         T116(2,IR,IY,IS) = T_OVR(IR,IY) * SCALPR * 0.001
         IF (IY .GT. CUMCAPADD - 1989) THEN
            T116(12,IR,IY,IS) = NPV(UT_DR,TMP,CUMCAPADD-1989,IY,0,MAXYR-1) * SCALPR * 0.001
         END IF
         TMP(:) = T_ANN(IR,:)
         T116(22,IR,IY,IS) = T_ANN(IR,IY) * SCALPR * 0.001
         IF (IY .GT. CUMCAPADD - 1989) THEN
            T116(32,IR,IY,IS) = NPV(UT_DR,TMP,CUMCAPADD-1989,IY,0,MAXYR-1) * SCALPR * 0.001
         END IF

!        Retrofits

         TMP(:) = RET_INV(IR,:)
         T116(3,IR,IY,IS) = RET_INV(IR,IY) * SCALPR * 0.001
         IF (IY .GT. CUMCAPADD - 1989) THEN
            T116(13,IR,IY,IS) = NPV(UT_DR,TMP,CUMCAPADD-1989,IY,0,MAXYR-1) * SCALPR * 0.001
         END IF
         TMP(:) = RET_Cst(IR,:)
         T116(23,IR,IY,IS) = Ret_Cst(IR,IY) * SCALPR * 0.001
         IF (IY .GT. CUMCAPADD - 1989) THEN
            T116(33,IR,IY,IS) = NPV(UT_DR,TMP,CUMCAPADD-1989,IY,0,MAXYR-1) * SCALPR * 0.001
         END IF

!        Fixed O&M Costs

         TMP(:) = Total_FOM(IR,:)
         T116(4,IR,IY,IS) = Total_FOM(IR,IY) * SCALPR * 0.001
         IF (IY .GT. CUMCAPADD - 1989) THEN
            T116(14,IR,IY,IS) = NPV(UT_DR,TMP,CUMCAPADD-1989,IY,0,MAXYR-1) * SCALPR * 0.001
         END IF
         T116(24,IR,IY,IS) = Total_FOM(IR,IY) * SCALPR * 0.001
         IF (IY .GT. CUMCAPADD - 1989) THEN
            T116(34,IR,IY,IS) = NPV(UT_DR,TMP,CUMCAPADD-1989,IY,0,MAXYR-1) * SCALPR * 0.001
         END IF

!        Capital Additions

         TMP(:) = CP_ADD(IR,:)
         T116(5,IR,IY,IS) = CP_ADD(IR,IY) * SCALPR * 0.001
         IF (IY .GT. CUMCAPADD - 1989) THEN
            T116(15,IR,IY,IS) = NPV(UT_DR,TMP,CUMCAPADD-1989,IY,0,MAXYR-1) * SCALPR * 0.001
         END IF
         T116(25,IR,IY,IS) = CP_ADD(IR,IY) * SCALPR * 0.001
         IF (IY .GT. CUMCAPADD - 1989) THEN
            T116(35,IR,IY,IS) = NPV(UT_DR,TMP,CUMCAPADD-1989,IY,0,MAXYR-1) * SCALPR * 0.001
         END IF

!        Non-Fuel Variable O&M

         TMP(:) = Non_Fuel_VOM(IR,:)
         T116(6,IR,IY,IS) = Non_Fuel_VOM(IR,IY) * SCALPR * 0.001
         IF (IY .GT. CUMCAPADD - 1989) THEN
            T116(16,IR,IY,IS) = NPV(UT_DR,TMP,CUMCAPADD-1989,IY,0,MAXYR-1) * SCALPR * 0.001
         END IF
         T116(26,IR,IY,IS) = Non_Fuel_VOM(IR,IY) * SCALPR * 0.001
         IF (IY .GT. CUMCAPADD - 1989) THEN
            T116(36,IR,IY,IS) = NPV(UT_DR,TMP,CUMCAPADD-1989,IY,0,MAXYR-1) * SCALPR * 0.001
         END IF

!        Fuel Expenses

         DO IY1 = 1 , LASTYR
            TMP(IY1) = Fuel_VOM(IR,IY1) - Carbon_Rev(IR,IY1)
         END DO

         T116(7,IR,IY,IS) = (Fuel_VOM(IR,IY) - Carbon_Rev(IR,IY)) * SCALPR * 0.001
         IF (IY .GT. CUMCAPADD - 1989) THEN
            T116(17,IR,IY,IS) = NPV(UT_DR,TMP,CUMCAPADD-1989,IY,0,MAXYR-1) * SCALPR * 0.001
         END IF
         T116(27,IR,IY,IS) = (Fuel_VOM(IR,IY) - Carbon_Rev(IR,IY)) * SCALPR * 0.001
         IF (IY .GT. CUMCAPADD - 1989) THEN
            T116(37,IR,IY,IS) = NPV(UT_DR,TMP,CUMCAPADD-1989,IY,0,MAXYR-1) * SCALPR * 0.001
         END IF

!        Purchased Power

         DO IY1 = 1 , LASTYR
            TMP(IY1) = T_DomEcon(IR,IY1) + T_DomFirm(IR,IY1) - T_IntExp(IR,IY1) + &
                       T_IntImp(IR,IY1) + Total_RCC(IR,IY1) + Total_RIC(IR,IY1)
         END DO

         T116(8,IR,IY,IS) = (T_DomEcon(IR,IY) + T_DomFirm(IR,IY) - T_IntExp(IR,IY) + &
                       T_IntImp(IR,IY) + Total_RCC(IR,IY) + Total_RIC(IR,IY)) * SCALPR * 0.001
         IF (IY .GT. CUMCAPADD - 1989) THEN
            T116(18,IR,IY,IS) = NPV(UT_DR,TMP,CUMCAPADD-1989,IY,0,MAXYR-1) * SCALPR * 0.001
         END IF
         T116(28,IR,IY,IS) = (T_DomEcon(IR,IY) + T_DomFirm(IR,IY) - T_IntExp(IR,IY) + &
                           T_IntImp(IR,IY) + Total_RCC(IR,IY) + Total_RIC(IR,IY)) * SCALPR * 0.001
         IF (IY .GT. CUMCAPADD - 1989) THEN
            T116(38,IR,IY,IS) = NPV(UT_DR,TMP,CUMCAPADD-1989,IY,0,MAXYR-1) * SCALPR * 0.001
         END IF

!        RPS Credit Expenses

!        use RPS penalty payment variable from Table 122 for consistency

         TMP(:) = URPSPEN(IR,:)
         T116(9,IR,IY,IS) = URPSPEN(IR,IY) * SCALPR * 0.001
         IF (IY .GT. CUMCAPADD - 1989) THEN
            T116(19,IR,IY,IS) = NPV(UT_DR,TMP,CUMCAPADD-1989,IY,0,MAXYR-1) * SCALPR * 0.001
         END IF
         T116(29,IR,IY,IS) = URPSPEN(IR,IY) * SCALPR * 0.001
         IF (IY .GT. CUMCAPADD - 1989) THEN
            T116(39,IR,IY,IS) = NPV(UT_DR,TMP,CUMCAPADD-1989,IY,0,MAXYR-1) * SCALPR * 0.001
         END IF

!        Energy Efficiency Expenses

         TMP(:) = ECSTNREE(IR,:)
         T116(41,IR,IY,IS) = ECSTNREE(IR,IY) * SCALPR
         IF (IY .GT. CUMCAPADD - 1989) THEN
            T116(42,IR,IY,IS) = NPV(UT_DR,TMP,CUMCAPADD-1989,IY,0,MAXYR-1) * SCALPR
         ENDIF
         T116(43,IR,IY,IS) = T116(41,IR,IY,IS)
         T116(44,IR,IY,IS) = T116(42,IR,IY,IS)


!        Total

         DO IY1 = 1 , LASTYR
            TMP(IY1) = G_INST_ALL(IR,IY1) + T_OVR(IR,IY1) + RET_INV(IR,IY1) + Total_FOM(IR,IY1) + &
                 CP_ADD(IR,IY1) + Non_Fuel_VOM(IR,IY1) + Fuel_VOM(IR,IY1) - Carbon_Rev(IR,IY1) + T_DomEcon(IR,IY1) + &
                 T_DomFirm(IR,IY1) - T_IntExp(IR,IY1) + T_IntImp(IR,IY1) + Total_RCC(IR,IY1) + &
                 Total_RIC(IR,IY1) + URPSPEN(IR,IY1) + ECSTNREE(IR,IY1)*1000.0
         END DO

         T116(10,IR,IY,IS) = (G_INST_ALL(IR,IY) + T_OVR(IR,IY) + RET_INV(IR,IY) + Total_FOM(IR,IY) + &
              CP_ADD(IR,IY) + Non_Fuel_VOM(IR,IY) + Fuel_VOM(IR,IY) - Carbon_Rev(IR,IY) + T_DomEcon(IR,IY) + &
              T_DomFirm(IR,IY) - T_IntExp(IR,IY) + T_IntImp(IR,IY) + Total_RCC(IR,IY) + &
              Total_RIC(IR,IY) + URPSPEN(IR,IY) + ECSTNREE(IR,IY)*1000.0 ) * SCALPR * 0.001
         IF (IY .GT. CUMCAPADD - 1989) THEN
            T116(20,IR,IY,IS) = NPV(UT_DR,TMP,CUMCAPADD-1989,IY,0,MAXYR-1) * SCALPR * 0.001
         END IF

         DO IY1 = 1 , LASTYR
            TMP(IY1) = G_ANN(IR,IY1) + T_ANN(IR,IY1) + RET_Cst(IR,IY1) + Total_FOM(IR,IY1) + &
                 CP_ADD(IR,IY1) + Non_Fuel_VOM(IR,IY1) + Fuel_VOM(IR,IY1) - Carbon_Rev(IR,IY1) + T_DomEcon(IR,IY1) + &
                 T_DomFirm(IR,IY1) - T_IntExp(IR,IY1) + T_IntImp(IR,IY1) + Total_RCC(IR,IY1) + &
                 Total_RIC(IR,IY1) + URPSPEN(IR,IY1) + ECSTNREE(IR,IY1)*1000.0
         END DO

         T116(30,IR,IY,IS) = (G_ANN(IR,IY) + T_ANN(IR,IY) + RET_Cst(IR,IY) + Total_FOM(IR,IY) + &
              CP_ADD(IR,IY) + Non_Fuel_VOM(IR,IY) + Fuel_VOM(IR,IY) - Carbon_Rev(IR,IY) + T_DomEcon(IR,IY) + &
              T_DomFirm(IR,IY) - T_IntExp(IR,IY) + T_IntImp(IR,IY) + Total_RCC(IR,IY) + &
              Total_RIC(IR,IY) + URPSPEN(IR,IY) + ECSTNREE(IR,IY)*1000.0 ) * SCALPR * 0.001
         IF (IY .GT. CUMCAPADD - 1989) THEN
            T116(40,IR,IY,IS) = NPV(UT_DR,TMP,CUMCAPADD-1989,IY,0,MAXYR-1) * SCALPR * 0.001
         END IF

!        Grid Resiliency/Reliability

         DO III = 1 , NUMREP
            T116(44+(III - 1)*10 +1,IR,IY,IS) = UGRD_STC(III,IR,IY)     ! Capacity Target
            T116(44+(III - 1)*10 +2,IR,IY,IS) = UGRD_RTC(III,IR,IY)     ! Capacity Rating Achieved
            T116(44+(III - 1)*10 +3,IR,IY,IS) = UGRD_PRC(III,IR,IY)     ! Capacity Price
            T116(44+(III - 1)*10 +4,IR,IY,IS) = UGRD_CAP(III,IR,IY)     ! Qualifying Capacity
            T116(44+(III - 1)*10 +5,IR,IY,IS) = T62( 9,IR,IY,IS)        ! Total Capacity
            T116(44+(III - 1)*10 +6,IR,IY,IS) = UGRD_STG(III,IR,IY)
            T116(44+(III - 1)*10 +7,IR,IY,IS) = UGRD_RTG(III,IR,IY)
            T116(44+(III - 1)*10 +8,IR,IY,IS) = UGRD_PRG(III,IR,IY)
            T116(44+(III - 1)*10 +9,IR,IY,IS) = UGRD_GEN(III,IR,IY)
            T116(44+(III - 1)*10+10,IR,IY,IS) = T62(61,IR,IY,IS)
         END DO

        END DO
      END DO

!     Table 117. National Impacts of the Clean Air Amendment of 1990

         DO IY=1,LASTYR
          ICY=IY+BASEYR-1
!     ---  CUMULATIVE COAL RETROFITS
          CUMRET = 0.0
          CUMRETP = 0.0
          CUMNOXP = 0.0
          CUMSCRP = 0.0
          CUMNCRP = 0.0
          CUMNOX=0.0
          CUMSCR=0.0
          CUMNCR=0.0
          CUMCCS=0.0
          CUMCDR=0.0
          CUMGCS=0.0
          CUMGDR=0.0
          CUMSC=0.0
          CUMFF=0.0
          CUMDSI=0.0
          CUMHRI=0.0
          DO IY1 = (YEARPR + 1) - 1989, IY
             CUMRET = CUMRET + EMELRET(MNUMNR,IY1)
             CUMRETP = CUMRETP + EMELRETP(MNUMNR,IY1)
             CUMNOX = CUMNOX + EMELNOX(MNUMNR,IY1)
             CUMNOXP = CUMNOXP + EMELNOXP(MNUMNR,IY1)
             CUMSCR = CUMSCR + EMELSCR(MNUMNR,IY1)
             CUMSCRP = CUMSCRP + EMELSCRP(MNUMNR,IY1)
             CUMNCR = CUMNCR + EMELNCR(MNUMNR,IY1)
             CUMNCRP = CUMNCRP + EMELNCRP(MNUMNR,IY1)
             CUMSC = CUMSC + EMELSC(MNUMNR,IY1)
             CUMFF = CUMFF + EMELFF(MNUMNR,IY1)
             CUMDSI = CUMDSI + EMELDSI(MNUMNR,IY1)
             CUMHRI = CUMHRI + ECAPNRHR(MNUMNR,IY1)
          END DO
          CUMCCS = EMELCCS(MNUMNR,IY)
          CUMCDR = EMELCDR(MNUMNR,IY)
          CUMGCS = EMELGCS(MNUMNR,IY)
          CUMGDR = EMELGDR(MNUMNR,IY)
          T117( 1,IY,IS) = CUMRET
          T117(27,IY,IS) = CUMRETP
          T117(28,IY,IS) = CUMRETP + CUMRET
              CUMRET = 0.0
          DO ISO2 = 1 , NUM_SO2_GRP
             CUMRET = CUMRET + EMRFSA(IY,ISO2) / 1.0D6
          END DO
          T117(46,IY,IS) = EMRFSA(IY,1) / 1.0D6
          T117(47,IY,IS) = EMRFSA(IY,2) / 1.0D6
          T117( 2,IY,IS) = CUMRET
          CUMRET = 0.0
          IF (IY .GT. 1) THEN
             DO ISO2 = 1 , NUM_SO2_GRP
                CUMRET = CUMRET + EMELBNK(IY,ISO2) * 0.000001
             END DO
             T117(48,IY,IS) = EMELBNK(IY,1) * 0.000001
             T117(49,IY,IS) = EMELBNK(IY,2) * 0.000001
          END IF
          T117( 3,IY,IS) = CUMRET
          T117( 4,IY,IS) = EMELPSO2(IY,1)
          T117(26,IY,IS) = ECP_PSO2(0,IY,1)
          IF (NUM_SO2_GRP .LT. MX_SO2_GRP)THEN
           DO ISO2 = NUM_SO2_GRP + 1 , MX_SO2_GRP
           EMELPSO2(IY,ISO2) = EMELPSO2(IY,1)
           ECP_PSO2(0,IY,ISO2) = ECP_PSO2(0,IY,1)
           END DO
          END IF
          T117(44,IY,IS) = EMELPSO2(IY,2)
          T117(45,IY,IS) = ECP_PSO2(0,IY,2)
          so2coal = 0.0
          so2cnt = 0.0
          hgcnt = 0.0
          tlcoal = 0.0
          DO ISEC = 1, NUTSEC
           so2coal = so2coal + UTTSO2(ISEC,IY)
           so2cnt = so2cnt + UCSO2(ISEC,IY)
           hgcnt = hgcnt + UCMERC(ISEC,IY)
           DO IC = 1 , NDREG
            tlcoal = tlcoal + QCLCLNR(IC,IY,ISEC)
           END DO
          ENDDO
!     Sum up emissions from CTL
              CTLSO2 = 0.0
              CTLNOX = 0.0
              CTLHG  = 0.0
           DO IR = 1 , NDREG
              CTLSO2 = CTLSO2 + CTLSO2EM(IR,IY) * 0.000001
              CTLNOX = CTLNOX + CTLNOXEM(IR,IY) * 0.000001
              CTLHG  = CTLHG  + CTLHGEM(IR,IY)
           END DO
           T117( 5,IY,IS) = so2coal
           T117( 7,IY,IS) = 0.0
           DO ISO2 = 1 , NUM_SO2_GRP
              T117( 5,IY,IS) = T117( 5,IY,IS) + so2other(IY,ISO2) * 0.000001
              T117( 7,IY,IS) = T117( 7,IY,IS) + so2other(IY,ISO2) * 0.000001
           END DO
           T117( 5,IY,IS) = T117( 5,IY,IS) + CTLSO2
           T117( 6,IY,IS) = so2coal + CTLSO2
! NOX allowance price and emissions
           T117( 8,IY,IS) = EPNOXPR(1,IY)
           DO INOX = 2 , NOX_D_GRP
              T117(INOX+27,IY,IS) = EPNOXPR(INOX,IY)
           END DO
           T117( 9,IY,IS) = UNOXOTR(MNUMNR,IY) + UNOXINR(MNUMNR,IY) + CTLNOX
           T117(13,IY,IS) = CUMNOX
           T117(38,IY,IS) = CUMNOXP
           T117(39,IY,IS) = CUMNOXP + CUMNOX
           T117(14,IY,IS) = CUMSCR
           T117(40,IY,IS) = CUMSCRP
           T117(41,IY,IS) = CUMSCRP + CUMSCR
           T117(15,IY,IS) = CUMNCR
           T117(42,IY,IS) = CUMNCRP
           T117(43,IY,IS) = CUMNCRP + CUMNCR
! Mercury allowance price and emissions
!  million $/ton
           T117(16,IY,IS) = EMEL_PHG(1,IY)
!  thousand $/lb
           T117(17,IY,IS) = EMEL_PHG(1,IY) / 2.0
! ECP mercury - thousand $/lb
           T117(50,IY,IS) = ECP_PHG(1,IY) / 2.0
           T117(18,IY,IS) = TOT_MERC(IY) / 2000.0 + CTLHG
! SO2 and Mercury Contents
           IF (tlcoal .ne. 0.0) then
              T117(19,IY,IS) = so2cnt * 2000.0 / tlcoal
              T117(20,IY,IS) = hgcnt / tlcoal
           ENDIF
! ACI Spray Cooling and Supplemental Fabric Filter Investments
           T117(21,IY,IS) = CUMSC
           T117(22,IY,IS) = CUMFF
           T117(23,IY,IS) = ACMERC(IY)
! CCS Retrofits
           T117(51,IY,IS) = CUMCCS
           T117(52,IY,IS) = CUMCDR
           T117(56,IY,IS) = CUMGCS
           T117(57,IY,IS) = CUMGDR
!  generation from retrofits:
           T117(53,IY,IS) = sum(UGENSQ(1:MNUMNR-1,IY)) - sum(UGENSQ_ALT(1:MNUMNR-1,IY))
           T117(58,IY,IS) = sum(UGENA2(1:MNUMNR-1,IY)) - sum(UGENA2_ALT(1:MNUMNR-1,IY))
! DSI Retrofits
           T117(54,IY,IS) = CUMDSI
! HRI Retrofits
           T117(55,IY,IS) = CUMHRI
!          --- SULFUR TOTALS
           T117(10,IY,IS) = ABSULF(1,IY) + ALSULF(1,IY) + &
                           IBSULF(1,IY) + ILSULF(1,IY) + APSULF(1,IY) + &
                          WBSULF(1,IY) + WSSULF(1,IY) + WLSULF(1,IY) + WPSULF(1,IY)
           T117(11,IY,IS) = ABSULF(2,IY) + ALSULF(2,IY) + &
                           IBSULF(2,IY) + ILSULF(2,IY) + APSULF(2,IY) + &
                          WBSULF(2,IY) + WSSULF(2,IY) + WLSULF(2,IY) + WPSULF(2,IY)
           T117(12,IY,IS) = ABSULF(3,IY) + ALSULF(3,IY) + &
                           IBSULF(3,IY) + ILSULF(3,IY) + APSULF(3,IY) + &
                          WBSULF(3,IY) + WSSULF(3,IY) + WLSULF(3,IY) + WPSULF(3,IY)
!   Carbon Dioxide emissions and allowance price
           T117(24,IY,IS) = em_elec(8,11,ICY)
           T117(25,IY,IS) = EMETAX(1,IY) * 1000. * SCALPR
!   Allowance Revenues
           T117(33,IY,IS) = T117( 8,IY,IS) * (EMRFNA(1,IY) * 0.001) * 0.001
          DO INOX = 2 , NOX_D_GRP
           T117(33,IY,IS) = T117(33,IY,IS) + T117(INOX+27,IY,IS) *  &
                           (EMRFNA(INOX,IY) * 0.001) * 0.001
          END DO
           T117(34,IY,IS) = T117( 5,IY,IS) * T117( 4,IY,IS) * 0.001
           T117(35,IY,IS) = T117(16,IY,IS) * T117(18,IY,IS) * 0.001
           T117(36,IY,IS) = T117(24,IY,IS) * T117(25,IY,IS) * 0.001
           T117(37,IY,IS) = FSUM(T117(33,IY,IS),4)
         ENDDO

!  Table 118 and Table 119:  see routine DOT118 which is called before we do Table 1

!  Table 121 blank (again)  T121(

!  Table 120:  Do not use.  This is reserved for the IEA table done in subroutine FDATA_IEA

!     Table 122. National Impacts of the Renewable Portolio Standards (RPS) and Production Tax Credits (PTC)
      DO IY=1,LASTYR
        DO IR=1,MNUMNR
         CUMRPS = 0.0
         CUMPEN = 0.0
         CUMPTC = 0.0
! RPS/CPS Credits
         T122( 1,IR,IY,IS) = UCRCLNR(2,IR,IY)
         T122( 2,IR,IY,IS) = UCRNGNR(2,IR,IY)
         T122( 3,IR,IY,IS) = UCRNUNR(2,IR,IY)
         T122( 4,IR,IY,IS) = UCRHYNR(2,IR,IY)
         T122( 5,IR,IY,IS) = UCRGENR(2,IR,IY)
         T122( 6,IR,IY,IS) = UCRMSNR(2,IR,IY)
         T122( 7,IR,IY,IS) = UCRWDNR(2,IR,IY) + UCRCFNR(2,IR,IY)
         T122( 8,IR,IY,IS) = UCRWDNR(2,IR,IY)
         T122( 9,IR,IY,IS) = UCRCFNR(2,IR,IY)
         T122(10,IR,IY,IS) = UCRSONR(2,IR,IY)
         T122(11,IR,IY,IS) = UCRPVNR(2,IR,IY) + UCRPTNR(2,IR,IY)
         T122(12,IR,IY,IS) = UCRWNNR(2,IR,IY) + UCRWLNR(2,IR,IY) + UCRWFNR(2,IR,IY)
!        T122(42,IR,IY,IS) = UCRWFNR(2,IR,IY)
!        CHP Credits
         DO ISEC = 1 , 4
            T122(51+ISEC,IR,IY,IS) = UCRCHNR(ISEC,IR,IY)
            UCRTLNR(2,IR,IY) = UCRTLNR(2,IR,IY) + UCRCHNR(ISEC,IR,IY)
         END DO
         T122(13,IR,IY,IS) = UCRTLNR(2,IR,IY)
         T122(14,IR,IY,IS) = UCRPNNR(IR,IY)
         IF (IY .GT. 1)THEN
            T122(44,IR,IY,IS) = UCRBKNR(IR,IY) - UCRBKNR(IR,IY-1)
         ELSE
            T122(44,IR,IY,IS) = 0.0
         END IF
         T122(45,IR,IY,IS) = UCRBKNR(IR,IY)
! Electricity Sales (48 States)
         if (ir .eq. mnumnr) then
            T122(15,IR,IY,IS) = (QELAS(MNUMCR,IY) / 0.003412) - (QELASN(MNUMNR - 2,IY) + QELASN(MNUMNR - 1,IY)) * 0.001
         else
         T122(15,IR,IY,IS) = QELASN(IR,IY) * 0.001
         endif
! Baseline Sales with Exclusions, If Any (48 States)
         T122(47,IR,IY,IS) = UCRBSNR(IR,IY)
! RPS/CPS Levels
!  Percent of Total Sales
         T122(16,IR,IY,IS) = EPRPSLM(IY) * 100.0 * T122(47,IR,IY,IS) / T122(15,IR,IY,IS)
         T122(17,IR,IY,IS) = (UCRTLNR(2,IR,IY) / T122(15,IR,IY,IS)) * 100.0
         T122(46,IR,IY,IS) = ((UCRTLNR(2,IR,IY) - T122(44,IR,IY,IS)) / T122(15,IR,IY,IS)) * 100.0
!        T122(18,IR,IY,IS) = (UCRTLNR(1,IR,IY) / T122(15,IR,IY,IS)) * 100.0
!  Percent of Baseline Sales
         T122(48,IR,IY,IS) = EPRPSLM(IY) * 100.0
         T122(49,IR,IY,IS) = (UCRTLNR(2,IR,IY) / T122(47,IR,IY,IS)) * 100.0
         T122(50,IR,IY,IS) = ((UCRTLNR(2,IR,IY) - T122(44,IR,IY,IS)) / T122(47,IR,IY,IS)) * 100.0
!        T122(51,IR,IY,IS) = (UCRTLNR(1,IR,IY) / T122(47,IR,IY,IS)) * 100.0
! RPS/CPS Prices
         T122(19,IR,IY,IS) = URPSPRC(IY) * SCALPR
         T122(20,IR,IY,IS) = EPRPSCP(IY) * SCALPR
! RPS/CPS Payments
         T122(21,IR,IY,IS) = URPSCST(IR,IY) * 0.001 * SCALPR
         T122(22,IR,IY,IS) = URPSPEN(IR,IY) * 0.001 * SCALPR
         T122(23,IR,IY,IS) = FSUM(T122(21,IR,IY,IS),2)
         DO IY1 = (YEARPR + 1) - 1989, IY
            CUMRPS = CUMRPS + URPSCST(IR,IY1)
            CUMPEN = CUMPEN + URPSPEN(IR,IY1)
         END DO
         T122(24,IR,IY,IS) = CUMRPS * 0.001 * SCALPR
         T122(25,IR,IY,IS) = CUMPEN * 0.001 * SCALPR
         T122(26,IR,IY,IS) = FSUM(T122(24,IR,IY,IS),2)
! PTC Generation
         T122(27,IR,IY,IS) = UPTCLNR(IR,IY)
         T122(28,IR,IY,IS) = UPTNGNR(IR,IY)
         T122(29,IR,IY,IS) = UPTNUNR(IR,IY)
         T122(30,IR,IY,IS) = UPTHYNR(IR,IY)
         T122(31,IR,IY,IS) = UPTGENR(IR,IY)
         T122(32,IR,IY,IS) = UPTMSNR(IR,IY)
         T122(33,IR,IY,IS) = UPTWDNR(IR,IY) + UPTCFNR(IR,IY)
         T122(34,IR,IY,IS) = UPTWDNR(IR,IY)
         T122(35,IR,IY,IS) = UPTCFNR(IR,IY)
         T122(36,IR,IY,IS) = UPTSONR(IR,IY)
         T122(37,IR,IY,IS) = UPTPVNR(IR,IY) + UPTPTNR(IR,IY)
         T122(38,IR,IY,IS) = UPTWNNR(IR,IY) + UPTWLNR(IR,IY) + UPTWFNR(IR,IY)
!        T122(43,IR,IY,IS) = UPTWFNR(IR,IY)
         T122(39,IR,IY,IS) = FSUM(T122(27,IR,IY,IS),6) + FSUM(T122(34,IR,IY,IS),5)
!                           T122(43,IR,IY,IS)
! PTC Payment
         T122(40,IR,IY,IS) = UPTCCST(IR,IY) * 0.001 * SCALPR
         DO IY1 = (YEARPR + 1) - 1989, IY
            CUMPTC = CUMPTC + UPTCCST(IR,IY1)
         END DO
         T122(41,IR,IY,IS) = CUMPTC * 0.001 * SCALPR
        ENDDO


      ENDDO


      NUMREP=20
      DO IY=1,LASTYR
        DO IR=1,MNUMNR
          T123( 1,IR,IY,IS) = UPIGCCST(IR,IY)
          T123( 2,IR,IY,IS) = UPISCCST(IR,IY)
          T123( 3,IR,IY,IS) = UPPCCCST(IR,IY)
          T123( 4,IR,IY,IS) = UPACCCST(IR,IY)
          T123( 5,IR,IY,IS) = UPCSCCST(IR,IY)
          T123( 6,IR,IY,IS) = UPCCCCST(IR,IY)
          T123( 7,IR,IY,IS) = UPATCCST(IR,IY)
          T123( 8,IR,IY,IS) = UPCTCCST(IR,IY)
          T123( 9,IR,IY,IS) = UPANCCST(IR,IY)
          T123(10,IR,IY,IS) = UPFCCCST(IR,IY)
          T123(11,IR,IY,IS) = UPHYCCST(IR,IY)
          T123(12,IR,IY,IS) = UPGTCCST(IR,IY)
          T123(13,IR,IY,IS) = UPMSCCST(IR,IY)
          T123(14,IR,IY,IS) = UPWDCCST(IR,IY)
          T123(15,IR,IY,IS) = UPSOCCST(IR,IY)
          T123(16,IR,IY,IS) = UPPVCCST(IR,IY)
          T123(17,IR,IY,IS) = UPWNCCST(IR,IY)
          T123(18,IR,IY,IS) = UPWFCCST(IR,IY)
          T123(19,IR,IY,IS) = UPDBCCST(IR,IY)
          T123(20,IR,IY,IS) = UPDPCCST(IR,IY)
          T123(21,IR,IY,IS) = UPPQCCST(IR,IY)
          T123(22,IR,IY,IS) = UPSMCCST(IR,IY)

          T123(23,IR,IY,IS) = UPOCCCST(IR,IY)
          T123(24,IR,IY,IS) = UPI2CCST(IR,IY)
          T123(25,IR,IY,IS) = UPICCCST(IR,IY)
          T123(26,IR,IY,IS) = UPT2CCST(IR,IY)
          T123(27,IR,IY,IS) = UPA2CCST(IR,IY)
          T123(28,IR,IY,IS) = UPGNCCST(IR,IY)
          T123(29,IR,IY,IS) = UPBICCST(IR,IY)
          T123(30,IR,IY,IS) = UPAGCCST(IR,IY)
          T123(31,IR,IY,IS) = UPHOCCST(IR,IY)
          T123(32,IR,IY,IS) = UPHICCST(IR,IY)
          T123(33,IR,IY,IS) = UPTICCST(IR,IY)
          T123(34,IR,IY,IS) = UPSQCCST(IR,IY)
          T123(35,IR,IY,IS) = UPDSCCST(IR,IY)
          T123(36,IR,IY,IS) = UPZSCCST(IR,IY)
          T123(37,IR,IY,IS) = UPWLCCST(IR,IY)
          T123(38,IR,IY,IS) = UPSSCCST(IR,IY)
          T123(39,IR,IY,IS) = UPS2CCST(IR,IY)
          T123(40,IR,IY,IS) = UPPTCCST(IR,IY)
          T123(41,IR,IY,IS) = UPINCCST(IR,IY)




        ENDDO
      ENDDO

! Table 124 - Key Results for Residential and Commercial Sector Technology Cases

      DO IY=1,LASTYR
! Residential
         T124( 1,IY,IS)=QDSRS(11,IY)
         T124( 2,IY,IS)=QKSRS(11,IY)
         T124( 3,IY,IS)=QLGRS(11,IY)
         T124( 4,IY,IS)=QTPRS(11,IY)
         T124( 5,IY,IS)=QNGRS(11,IY)
         T124( 6,IY,IS)=QCLRS(11,IY)
         T124( 7,IY,IS)=QBMRS(11,IY)
         T124( 8,IY,IS)=QELRS(11,IY)
         T124( 9,IY,IS)=QTSRS(11,IY) - QSTRS(11,IY) - QGERS(11,IY) - QPVRS(11,IY)
!        --- Note that this value for electric losses is incorrect for
!        --- stand alone runs, and needs to be done in a worksheet - JBJ
         T124(10,IY,IS)=T4(57,IY,IS)
         T124(11,IY,IS)=FSUM(T124( 9,IY,IS),2)
         T124(12,IY,IS)=T4(6,IY,IS)
         T124(13,IY,IS)=(RSH2OCON(IY,5) + RSHTRCON(IY,7) + RSCOOLCN(IY,2)) / 1000000000. + &
                         QPVRS(11,IY) + CGRESQ(11,IY,11)/1000.
! Commercial
         T124(14,IY,IS)=QDSCM(11,IY)
         T124(15,IY,IS)=QRSCM(11,IY)
         T124(16,IY,IS)=QKSCM(11,IY)
         T124(17,IY,IS)=QLGCM(11,IY)
         T124(18,IY,IS)=QMGCM(11,IY)
         T124(19,IY,IS)=QTPCM(11,IY)
         T124(20,IY,IS)=QNGCM(11,IY)
         T124(21,IY,IS)=QCLCM(11,IY)
         T124(22,IY,IS)=QTRCM(11,IY) - QSTCM(11,IY) - QPVCM(11,IY)
         T124(23,IY,IS)=QELCM(11,IY)
         T124(24,IY,IS)=QTSCM(11,IY) - QSTCM(11,IY) - QPVCM(11,IY)
         T124(25,IY,IS)=T5(42,IY,IS)
         T124(26,IY,IS)=FSUM(T124(24,IY,IS),2)
         T124(27,IY,IS)=T5(4,IY,IS)
         T124(28,IY,IS)=CGCOMMCAP(11,IY,3)+CGRESCAP(11,IY,3)
         T124(29,IY,IS)=CGCOMMCAP(11,IY,8)+CGRESCAP(11,IY,8)
         T124(31,IY,IS)=(CGRESGEN(11,IY,3,1)+CGRESGEN(11,IY,3,2)+ &
                         CGCOMMGEN(11,IY,3,1)+CGCOMMGEN(11,IY,3,2))/1000.
         T124(32,IY,IS)=(CGRESGEN(11,IY,8,1)+CGRESGEN(11,IY,8,2)+ &
                         CGCOMMGEN(11,IY,8,1)+CGCOMMGEN(11,IY,8,2))/1000.
! redo including only commercial
         T124(28,IY,IS)=CGCOMMCAP(11,IY,3)
         T124(29,IY,IS)=CGCOMMCAP(11,IY,8)
         T124(30,IY,IS)=CGCOMMCAP(11,IY,11)
         T124(31,IY,IS)=(CGCOMMGEN(11,IY,3,1)+CGCOMMGEN(11,IY,3,2))/1000.
         T124(32,IY,IS)=(CGCOMMGEN(11,IY,8,1)+CGCOMMGEN(11,IY,8,2))/1000.
         T124(33,IY,IS)=(CGCOMMGEN(11,IY,11,1)+CGCOMMGEN(11,IY,11,2))/1000.
         T124(34,IY,IS)=QSTCM(11,IY) + QPVCM(11,IY) + CGCOMMQ(11,IY,11)/1000.
      ENDDO


! TABLE 125 - Hydrogen Model Results

!      set up subscripts
!
       endProd     = PCHTCH + PGHTCH-1 + PFHTCH
       startTransp = endProd+1
       endTransp = endProd + TCHTCH + TGHTCH
       startDist = endTransp + 1
       endDist = endTransp + DOHTCH
       startDisp = endDist + 1
       endDisp     = endDist + SOHTCH-1
       startCG     = endDisp + 1
       endCG       = endDisp + SGHTCH

!       write(*,'(a,9i4)')'TOTNDX check:',endProd,startTransp,endTransp,startDist,endDist,  &
!            startDisp,endDisp,startCG,endCG
!
      DO 1250 IY=1,LASTYR


      DO 1250 IR=1,MNUMCR

!         Hydrogen Production

          DO J=1,endProd
             DO K=1,HMKT
               T125(J,IR,IY,IS) = T125(J,IR,IY,IS) + HMMPRD(IY,IR,K,J)
             enddo
          enddo
          DO j=1,endProd
             T125(endProd+1,IR,IY,IS) = T125(endProd+1,IR,IY,IS) + T125(J,IR,IY,IS)
             enddo

          ! Large City Market
          DO J=1,endProd
              T125(75+J,IR,IY,IS) = HMMPRD(IY,IR,1,J)
          enddo
          DO j=1,endProd
              T125(75+endProd+1,IR,IY,IS) = T125(75+endProd+1,IR,IY,IS) + T125(75+J,IR,IY,IS)
          enddo

          ! Small City Market
          DO J=1,endProd
              T125(88+J,IR,IY,IS) = HMMPRD(IY,IR,2,J)
             enddo
          DO j=1,endProd
              T125(88+endProd+1,IR,IY,IS) = T125(88+endProd+1,IR,IY,IS) + T125(88+J,IR,IY,IS)
          enddo

          ! Rural Market
          DO J=1,endProd
              T125(101+J,IR,IY,IS) = HMMPRD(IY,IR,3,J)
             enddo

         do J=1,endProd
              T125(101+endProd+1,IR,IY,IS) = T125(101+endProd+1,IR,IY,IS) + T125(101+J,IR,IY,IS)
         enddo


!         Hydrogen Transport (Large City ONLY)

      ! 1=compressed H truck, 2=liquefied truck, 3=pipeline
      DO J=1,3
           T125(9+endProd+1+J,IR,IY,IS) = T125(9+endProd+1+J,IR,IY,IS) +   &
                                        HMMPRD(IY,IR,1,endProd+J) +   &
                                        HMMPRD(IY,IR,1,endProd+J+3)
      ENDDO
      T125(9+endProd+1+4,IR,IY,IS) = FSUM(T125(9+endProd+1+1,IR,IY,IS),3)


!         Hydrogen Distribution

      DO K=1,HMKT                   !   all markets
          ! Compressed H Truck
          T125(9+endProd+1+5,IR,IY,IS) = T125(9+endProd+1+5,IR,IY,IS) + &
              HMMPRD(IY,IR,K,startDist+1) + HMMPRD(IY,IR,K,startDist+4)

          ! Liquified H Truck
          T125(9+endProd+1+6,IR,IY,IS) = T125(9+endProd+1+6,IR,IY,IS) + &
              HMMPRD(IY,IR,K,startDist+2) + HMMPRD(IY,IR,K,startDist+3)

          ! Pipeline
          T125(9+endProd+1+7,IR,IY,IS) = T125(9+endProd+1+7,IR,IY,IS) + &
              HMMPRD(IY,IR,K,startDist)
          enddo
      T125(9+endProd+1+8,IR,IY,IS) = FSUM(T125(9+endProd+1+5,IR,IY,IS),3)

      ! Large City
      ! Compressed H Truck
      T125(124,IR,IY,IS) = T125(124,IR,IY,IS) + &
         HMMPRD(IY,IR,1,startDist+1) + HMMPRD(IY,IR,1,startDist+4)

      ! Liquified H Truck
      T125(125,IR,IY,IS) = T125(125,IR,IY,IS) + &
         HMMPRD(IY,IR,1,startDist+2) + HMMPRD(IY,IR,1,startDist+3)

      ! Pipeline
      T125(126,IR,IY,IS) = T125(126,IR,IY,IS) + &
         HMMPRD(IY,IR,1,startDist)

      T125(127,IR,IY,IS) = FSUM(T125(124,IR,IY,IS),3)

      ! Small City
      ! Compressed H Truck
      T125(128,IR,IY,IS) = T125(128,IR,IY,IS) + &
         HMMPRD(IY,IR,2,startDist+1) + HMMPRD(IY,IR,2,startDist+4)

      ! Liquefied H Truck
      T125(129,IR,IY,IS) = T125(129,IR,IY,IS) + &
         HMMPRD(IY,IR,2,startDist+2) + HMMPRD(IY,IR,2,startDist+3)

      ! Pipeline
      T125(130,IR,IY,IS) = T125(130,IR,IY,IS) + &
         HMMPRD(IY,IR,2,startDist)

      T125(131,IR,IY,IS) = FSUM(T125(128,IR,IY,IS),3)

      ! Rural
      ! Compressed H Truck
      T125(132,IR,IY,IS) = T125(132,IR,IY,IS) + &
         HMMPRD(IY,IR,3,startDist+1) + HMMPRD(IY,IR,3,startDist+4)

      ! Liquified H Truck
      T125(133,IR,IY,IS) = T125(133,IR,IY,IS) + &
         HMMPRD(IY,IR,3,startDist+2) + HMMPRD(IY,IR,3,startDist+3)

      ! Pipeline
      T125(134,IR,IY,IS) = T125(134,IR,IY,IS) + &
         HMMPRD(IY,IR,3,startDist)

      T125(135,IR,IY,IS) = FSUM(T125(132,IR,IY,IS),3)


!        Hydrogen Demand by market type

      DO J=1,endProd
         T125(42,IR,IY,IS) = T125(42,IR,IY,IS) + HMMPRD(IY,IR,1,J)
         T125(43,IR,IY,IS) = T125(43,IR,IY,IS) + HMMPRD(IY,IR,2,J)
         T125(44,IR,IY,IS) = T125(44,IR,IY,IS) + HMMPRD(IY,IR,3,J)
           enddo
      T125(45,IR,IY,IS) = 0
      T125(46,IR,IY,IS) = FSUM(T125(42,IR,IY,IS),4)

!     Fuel Consumption

      DO J=1,HMKT
         T125(47,IR,IY,IS) = T125(47,IR,IY,IS) + HMCLCNS(IY,IR,J)          !coal
         T125(48,IR,IY,IS) = T125(48,IR,IY,IS) + HMGSCNS(IY,IR,J)          !gas
         T125(49,IR,IY,IS) = T125(49,IR,IY,IS) + HMBICNS(IY,IR,J)          !biomass
         T125(50,IR,IY,IS) = T125(50,IR,IY,IS) + HMURCNS(IY,IR,J)          !nuclear
         T125(51,IR,IY,IS) = T125(51,IR,IY,IS) + HMELCNS(IY,IR,J)          !electricity
         T125(31,IR,IY,IS) = T125(31,IR,IY,IS) + HMETCNS(IY,IR,J)          !ethanol
        enddo
      T125(52,IR,IY,IS) = FSUM(T125(47,IR,IY,IS),5) + T125(31,IR,IY,IS)

!     Compute MARGINAL hydrogen price in $/kg for each market in pricing year dollars

      T125(53,IR,IY,IS) = PH1TR(IR,IY) * 0.1345
      T125(54,IR,IY,IS) = PH2TR(IR,IY) * 0.1345
      T125(55,IR,IY,IS) = PH3TR(IR,IY) * 0.1345
      T125(56,IR,IY,IS) = 0

!     Compute average marginal hydrogen price over all uses

        PQsum(IR,IY) = 0.00
        Qsum(IR,IY) = 0.0

      PQsum(IR,IY) = PQsum(IR,IY) + (PH1TR(IR,IY) * 0.1345 * T125(42,IR,IY,IS)) + &
                     (PH2TR(IR,IY) * 0.1345 * T125(43,IR,IY,IS)) + &
                     (PH3TR(IR,IY) * 0.1345 * T125(44,IR,IY,IS))

      IF (T125(46,IR,IY,IS) .GT. 0.000001) THEN
         T125(57,IR,IY,IS) = (PQsum(IR,IY) / T125(46,IR,IY,IS))
        else
         T125(57,IR,IY,IS) = 0.0
        endif

!     Combined heat and power

      T125(136,IR,IY,IS) = CGHMMGEN(IR,IY,1,1) * 0.001  ! to convert to BkWh
      T125(137,IR,IY,IS) = CGHMMCAP(IR,IY,1) * 0.001    ! to convert to GW
      T125(138,IR,IY,IS) = PELBS(IR,IY) * 3.412         ! marginal baseload electricity price

!     Hydrogen Emissions

      T125(62,11,IY,IS) = EMHM(1,1,IY)
      T125(63,11,IY,IS) = EMHM(2,1,IY)
      T125(64,11,IY,IS) = EMHM(3,1,IY)

1250   CONTINUE

!Tables 126 and 127 are crude oil import volumes (126) and prices (127)
       NUMREP=12                        ! equal to number of rows per PADD
       DO IY = 1,LASTYR
         DO PADD=1,MNUMPR
           T126(((PADD-1)*NUMREP)+ 1,IY,IS) = Q_CRUDE_IMPORTA(PADD, 1,IY+1989)
           T126(((PADD-1)*NUMREP)+ 2,IY,IS) = Q_CRUDE_IMPORTA(PADD, 2,IY+1989)
           T126(((PADD-1)*NUMREP)+ 3,IY,IS) = Q_CRUDE_IMPORTA(PADD, 3,IY+1989)
           T126(((PADD-1)*NUMREP)+ 4,IY,IS) = Q_CRUDE_IMPORTA(PADD, 4,IY+1989)
           T126(((PADD-1)*NUMREP)+ 5,IY,IS) = Q_CRUDE_IMPORTA(PADD, 5,IY+1989)
           T126(((PADD-1)*NUMREP)+ 6,IY,IS) = Q_CRUDE_IMPORTA(PADD, 6,IY+1989)
           T126(((PADD-1)*NUMREP)+ 7,IY,IS) = Q_CRUDE_IMPORTA(PADD, 7,IY+1989)
           T126(((PADD-1)*NUMREP)+ 8,IY,IS) = Q_CRUDE_IMPORTA(PADD, 8,IY+1989)
           T126(((PADD-1)*NUMREP)+ 9,IY,IS) = Q_CRUDE_IMPORTA(PADD, 9,IY+1989)
           T126(((PADD-1)*NUMREP)+10,IY,IS) = Q_CRUDE_IMPORTA(PADD,10,IY+1989)
           T126(((PADD-1)*NUMREP)+11,IY,IS) = Q_CRUDE_IMPORTA(PADD,11,IY+1989)
           T126(((PADD-1)*NUMREP)+12,IY,IS) = sum(Q_CRUDE_IMPORTA(PADD,1:11,IY+1989))
!  go into table 127, why don't we?
           T127(((PADD-1)*NUMREP)+ 1,IY,IS) = P_CRUDE_IMPORTS(PADD, 1,IY+1989)
           T127(((PADD-1)*NUMREP)+ 2,IY,IS) = P_CRUDE_IMPORTS(PADD, 2,IY+1989)
           T127(((PADD-1)*NUMREP)+ 3,IY,IS) = P_CRUDE_IMPORTS(PADD, 3,IY+1989)
           T127(((PADD-1)*NUMREP)+ 4,IY,IS) = P_CRUDE_IMPORTS(PADD, 4,IY+1989)
           T127(((PADD-1)*NUMREP)+ 5,IY,IS) = P_CRUDE_IMPORTS(PADD, 5,IY+1989)
           T127(((PADD-1)*NUMREP)+ 6,IY,IS) = P_CRUDE_IMPORTS(PADD, 6,IY+1989)
           T127(((PADD-1)*NUMREP)+ 7,IY,IS) = P_CRUDE_IMPORTS(PADD, 7,IY+1989)
           T127(((PADD-1)*NUMREP)+ 8,IY,IS) = P_CRUDE_IMPORTS(PADD, 8,IY+1989)
           T127(((PADD-1)*NUMREP)+ 9,IY,IS) = P_CRUDE_IMPORTS(PADD, 9,IY+1989)
           T127(((PADD-1)*NUMREP)+10,IY,IS) = P_CRUDE_IMPORTS(PADD,10,IY+1989)
           T127(((PADD-1)*NUMREP)+11,IY,IS) = P_CRUDE_IMPORTS(PADD,11,IY+1989)

            IF (( Q_CRUDE_IMPORTA(PADD,1,IY+1989) + Q_CRUDE_IMPORTA(PADD,2,IY+1989) + Q_CRUDE_IMPORTA(PADD,3,IY+1989) +  &
                  Q_CRUDE_IMPORTA(PADD,4,IY+1989) + Q_CRUDE_IMPORTA(PADD,5,IY+1989) + Q_CRUDE_IMPORTA(PADD,6,IY+1989) +  &
                  Q_CRUDE_IMPORTA(PADD,7,IY+1989) + Q_CRUDE_IMPORTA(PADD,8,IY+1989) + Q_CRUDE_IMPORTA(PADD,9,IY+1989)  +  &
                  Q_CRUDE_IMPORTA(PADD,10,IY+1989) + Q_CRUDE_IMPORTA(PADD,11,IY+1989)) .GT. 0.0 ) &
           T127(((PADD-1)*NUMREP)+12,IY,IS) = &
               ((P_CRUDE_IMPORTS(PADD,1,IY+1989) * Q_CRUDE_IMPORTA(PADD,1,IY+1989) ) + &
                (P_CRUDE_IMPORTS(PADD,2,IY+1989) * Q_CRUDE_IMPORTA(PADD,2,IY+1989) ) + &
                (P_CRUDE_IMPORTS(PADD,3,IY+1989) * Q_CRUDE_IMPORTA(PADD,3,IY+1989) ) + &
                (P_CRUDE_IMPORTS(PADD,4,IY+1989) * Q_CRUDE_IMPORTA(PADD,4,IY+1989) ) + &
                (P_CRUDE_IMPORTS(PADD,5,IY+1989) * Q_CRUDE_IMPORTA(PADD,5,IY+1989) ) + &
                (P_CRUDE_IMPORTS(PADD,6,IY+1989) * Q_CRUDE_IMPORTA(PADD,6,IY+1989) ) + &
                (P_CRUDE_IMPORTS(PADD,7,IY+1989) * Q_CRUDE_IMPORTA(PADD,7,IY+1989) ) + &
                (P_CRUDE_IMPORTS(PADD,8,IY+1989) * Q_CRUDE_IMPORTA(PADD,8,IY+1989) ) + &
                (P_CRUDE_IMPORTS(PADD,9,IY+1989) * Q_CRUDE_IMPORTA(PADD,9,IY+1989) ) + &
                (P_CRUDE_IMPORTS(PADD,10,IY+1989) * Q_CRUDE_IMPORTA(PADD,10,IY+1989) ) + &
                (P_CRUDE_IMPORTS(PADD,11,IY+1989) * Q_CRUDE_IMPORTA(PADD,11,IY+1989) )) / &
               ( Q_CRUDE_IMPORTA(PADD,1,IY+1989) + Q_CRUDE_IMPORTA(PADD,2,IY+1989) + Q_CRUDE_IMPORTA(PADD,3,IY+1989) +  &
                 Q_CRUDE_IMPORTA(PADD,4,IY+1989) + Q_CRUDE_IMPORTA(PADD,5,IY+1989) + Q_CRUDE_IMPORTA(PADD,6,IY+1989) +  &
                 Q_CRUDE_IMPORTA(PADD,7,IY+1989) + Q_CRUDE_IMPORTA(PADD,8,IY+1989) + Q_CRUDE_IMPORTA(PADD,9,IY+1989) +  &
                 Q_CRUDE_IMPORTA(PADD,10,IY+1989) + Q_CRUDE_IMPORTA(PADD,11,IY+1989))
         ENDDO

! PADD Totals for II, III, and V:
         T126(121,IY,IS) = Q_CRUDE_IMPORTA(2, 1,IY+1989) + Q_CRUDE_IMPORTA(3, 1,IY+1989)
         T126(122,IY,IS) = Q_CRUDE_IMPORTA(2, 2,IY+1989) + Q_CRUDE_IMPORTA(3, 2,IY+1989)
         T126(123,IY,IS) = Q_CRUDE_IMPORTA(2, 3,IY+1989) + Q_CRUDE_IMPORTA(3, 3,IY+1989)
         T126(124,IY,IS) = Q_CRUDE_IMPORTA(2, 4,IY+1989) + Q_CRUDE_IMPORTA(3, 4,IY+1989)
         T126(125,IY,IS) = Q_CRUDE_IMPORTA(2, 5,IY+1989) + Q_CRUDE_IMPORTA(3, 5,IY+1989)
         T126(126,IY,IS) = Q_CRUDE_IMPORTA(2, 6,IY+1989) + Q_CRUDE_IMPORTA(3, 6,IY+1989)
         T126(127,IY,IS) = Q_CRUDE_IMPORTA(2, 7,IY+1989) + Q_CRUDE_IMPORTA(3, 7,IY+1989)
         T126(128,IY,IS) = Q_CRUDE_IMPORTA(2, 8,IY+1989) + Q_CRUDE_IMPORTA(3, 8,IY+1989)
         T126(129,IY,IS) = Q_CRUDE_IMPORTA(2, 9,IY+1989) + Q_CRUDE_IMPORTA(3, 9,IY+1989)
         T126(130,IY,IS) = Q_CRUDE_IMPORTA(2,10,IY+1989) + Q_CRUDE_IMPORTA(3,10,IY+1989)
         T126(131,IY,IS) = Q_CRUDE_IMPORTA(2,11,IY+1989) + Q_CRUDE_IMPORTA(3,11,IY+1989)
         T126(132,IY,IS) = FSUM(T126(121,IY,IS),11)
         T126(133,IY,IS) = Q_CRUDE_IMPORTA(4, 1,IY+1989) + Q_CRUDE_IMPORTA(5, 1,IY+1989)
         T126(134,IY,IS) = Q_CRUDE_IMPORTA(4, 2,IY+1989) + Q_CRUDE_IMPORTA(5, 2,IY+1989)
         T126(135,IY,IS) = Q_CRUDE_IMPORTA(4, 3,IY+1989) + Q_CRUDE_IMPORTA(5, 3,IY+1989)
         T126(136,IY,IS) = Q_CRUDE_IMPORTA(4, 4,IY+1989) + Q_CRUDE_IMPORTA(5, 4,IY+1989)
         T126(137,IY,IS) = Q_CRUDE_IMPORTA(4, 5,IY+1989) + Q_CRUDE_IMPORTA(5, 5,IY+1989)
         T126(138,IY,IS) = Q_CRUDE_IMPORTA(4, 6,IY+1989) + Q_CRUDE_IMPORTA(5, 6,IY+1989)
         T126(139,IY,IS) = Q_CRUDE_IMPORTA(4, 7,IY+1989) + Q_CRUDE_IMPORTA(5, 7,IY+1989)
         T126(140,IY,IS) = Q_CRUDE_IMPORTA(4, 8,IY+1989) + Q_CRUDE_IMPORTA(5, 8,IY+1989)
         T126(141,IY,IS) = Q_CRUDE_IMPORTA(4, 9,IY+1989) + Q_CRUDE_IMPORTA(5, 9,IY+1989)
         T126(142,IY,IS) = Q_CRUDE_IMPORTA(4,10,IY+1989) + Q_CRUDE_IMPORTA(5,10,IY+1989)
         T126(143,IY,IS) = Q_CRUDE_IMPORTA(4,11,IY+1989) + Q_CRUDE_IMPORTA(5,11,IY+1989)
         T126(144,IY,IS) = FSUM(T126(133,IY,IS),11)
         T126(145,IY,IS) = Q_CRUDE_IMPORTA(7, 1,IY+1989) + Q_CRUDE_IMPORTA(8, 1,IY+1989)
         T126(146,IY,IS) = Q_CRUDE_IMPORTA(7, 2,IY+1989) + Q_CRUDE_IMPORTA(8, 2,IY+1989)
         T126(147,IY,IS) = Q_CRUDE_IMPORTA(7, 3,IY+1989) + Q_CRUDE_IMPORTA(8, 3,IY+1989)
         T126(148,IY,IS) = Q_CRUDE_IMPORTA(7, 4,IY+1989) + Q_CRUDE_IMPORTA(8, 4,IY+1989)
         T126(149,IY,IS) = Q_CRUDE_IMPORTA(7, 5,IY+1989) + Q_CRUDE_IMPORTA(8, 5,IY+1989)
         T126(150,IY,IS) = Q_CRUDE_IMPORTA(7, 6,IY+1989) + Q_CRUDE_IMPORTA(8, 6,IY+1989)
         T126(151,IY,IS) = Q_CRUDE_IMPORTA(7, 7,IY+1989) + Q_CRUDE_IMPORTA(8, 7,IY+1989)
         T126(152,IY,IS) = Q_CRUDE_IMPORTA(7, 8,IY+1989) + Q_CRUDE_IMPORTA(8, 8,IY+1989)
         T126(153,IY,IS) = Q_CRUDE_IMPORTA(7, 9,IY+1989) + Q_CRUDE_IMPORTA(8, 9,IY+1989)
         T126(154,IY,IS) = Q_CRUDE_IMPORTA(7,10,IY+1989) + Q_CRUDE_IMPORTA(8,10,IY+1989)
         T126(155,IY,IS) = Q_CRUDE_IMPORTA(7,11,IY+1989) + Q_CRUDE_IMPORTA(8,11,IY+1989)
         T126(156,IY,IS) = FSUM(T126(145,IY,IS),11)
         DO PADD=1,11    !  re-using PADD counter variable here though we're looping over crude types
            IF (Q_CRUDE_IMPORTA(2,PADD,IY+1989) + Q_CRUDE_IMPORTA(3,PADD,IY+1989) .GT. 0.0) &
           T127(120+PADD,IY,IS) = &
               ((P_CRUDE_IMPORTS(2,PADD,IY+1989) * Q_CRUDE_IMPORTA(2,PADD,IY+1989)) + &
                (P_CRUDE_IMPORTS(3,PADD,IY+1989) * Q_CRUDE_IMPORTA(3,PADD,IY+1989))) / &
                (Q_CRUDE_IMPORTA(2,PADD,IY+1989) + Q_CRUDE_IMPORTA(3,PADD,IY+1989))
            IF (Q_CRUDE_IMPORTA(4,PADD,IY+1989) + Q_CRUDE_IMPORTA(5,PADD,IY+1989) .GT. 0.0) &
           T127(132+PADD,IY,IS) = &
               ((P_CRUDE_IMPORTS(4,PADD,IY+1989) * Q_CRUDE_IMPORTA(4,PADD,IY+1989)) + &
                (P_CRUDE_IMPORTS(5,PADD,IY+1989) * Q_CRUDE_IMPORTA(5,PADD,IY+1989))) / &
                (Q_CRUDE_IMPORTA(4,PADD,IY+1989) + Q_CRUDE_IMPORTA(5,PADD,IY+1989))
            IF (Q_CRUDE_IMPORTA(7,PADD,IY+1989) + Q_CRUDE_IMPORTA(8,PADD,IY+1989) .GT. 0.0) &
           T127(144+PADD,IY,IS) = &
               ((P_CRUDE_IMPORTS(7,PADD,IY+1989) * Q_CRUDE_IMPORTA(7,PADD,IY+1989)) + &
                (P_CRUDE_IMPORTS(8,PADD,IY+1989) * Q_CRUDE_IMPORTA(8,PADD,IY+1989))) / &
                (Q_CRUDE_IMPORTA(7,PADD,IY+1989) + Q_CRUDE_IMPORTA(8,PADD,IY+1989))
         ENDDO

         IF ((sum(T126(121:131,IY,IS))) .GT. 0.0)     T127(132,IY,IS) = &
               ((T126(121,IY,IS) * T127(121,IY,IS)) + (T126(122,IY,IS) * T127(122,IY,IS)) + &
                (T126(123,IY,IS) * T127(123,IY,IS)) + (T126(124,IY,IS) * T127(124,IY,IS)) + &
                (T126(125,IY,IS) * T127(125,IY,IS)) + (T126(126,IY,IS) * T127(126,IY,IS)) + &
                (T126(127,IY,IS) * T127(127,IY,IS)) + (T126(128,IY,IS) * T127(128,IY,IS)) + &
                (T126(129,IY,IS) * T127(129,IY,IS)) + &
                (T126(130,IY,IS) * T127(130,IY,IS)) + (T126(131,IY,IS) * T127(131,IY,IS))) / &
                              (sum(T126(121:131,IY,IS)))
         IF ((sum(T126(133:143,IY,IS))) .GT. 0.0)     T127(144,IY,IS) = &
               ((T126(133,IY,IS) * T127(133,IY,IS)) + (T126(134,IY,IS) * T127(134,IY,IS)) + &
                (T126(135,IY,IS) * T127(135,IY,IS)) + (T126(136,IY,IS) * T127(136,IY,IS)) + &
                (T126(137,IY,IS) * T127(137,IY,IS)) + (T126(138,IY,IS) * T127(138,IY,IS)) + &
                (T126(139,IY,IS) * T127(139,IY,IS)) + (T126(140,IY,IS) * T127(140,IY,IS)) + &
                (T126(141,IY,IS) * T127(141,IY,IS)) + &
                (T126(142,IY,IS) * T127(142,IY,IS)) + (T126(143,IY,IS) * T127(143,IY,IS))) / &
                              (sum(T126(133:143,IY,IS)))
         IF ((sum(T126(145:155,IY,IS))) .GT. 0.0)     T127(156,IY,IS) = &
               ((T126(145,IY,IS) * T127(145,IY,IS)) + (T126(146,IY,IS) * T127(146,IY,IS)) + &
                (T126(147,IY,IS) * T127(147,IY,IS)) + (T126(148,IY,IS) * T127(148,IY,IS)) + &
                (T126(149,IY,IS) * T127(149,IY,IS)) + (T126(150,IY,IS) * T127(150,IY,IS)) + &
                (T126(151,IY,IS) * T127(151,IY,IS)) + (T126(152,IY,IS) * T127(152,IY,IS)) + &
                (T126(153,IY,IS) * T127(153,IY,IS)) + &
                (T126(154,IY,IS) * T127(154,IY,IS)) + (T126(155,IY,IS) * T127(155,IY,IS))) / &
                              (sum(T126(145:155,IY,IS)))
       ENDDO

!  tables 128 and 129 are petroleum product import volumes (128) and prices (129)
       NUMREP=18                        ! equal to number of rows per PADD
       DO IY = 1,LASTYR
         DO PADD=1,MNUMPR
           T128(((PADD-1)*NUMREP)+ 1,IY,IS) = RFIPQAS(PADD,IY,2)
           T128(((PADD-1)*NUMREP)+ 2,IY,IS) = RFIPQAG(PADD,IY,2)
           T128(((PADD-1)*NUMREP)+ 3,IY,IS) = RFIPQCG(PADD,IY,2)
           T128(((PADD-1)*NUMREP)+ 4,IY,IS) = RFIPQMG(PADD,IY,2) + RFIPQCBOB(PADD,IY,2)
           T128(((PADD-1)*NUMREP)+ 5,IY,IS) = RFIPQCD(PADD,IY,2)
           T128(((PADD-1)*NUMREP)+ 6,IY,IS) = RFIPQDL(PADD,IY,2)
           T128(((PADD-1)*NUMREP)+ 7,IY,IS) = RFIPQDU(PADD,IY,2)
           T128(((PADD-1)*NUMREP)+ 8,IY,IS) = RFIPQJF(PADD,IY,2)
           T128(((PADD-1)*NUMREP)+ 9,IY,IS) = &
                RFIPQPR(PADD,IY,2) + RFIPQPY(PADD,IY,2) + RFIPQPP(PADD,IY,2) + &
                RFIPQET(PADD,IY,2) + RFIPQBU(PADD,IY,2) + RFIPQIS(PADD,IY,2)
           T128(((PADD-1)*NUMREP)+10,IY,IS) = RFIPQLU(PADD,IY,2)
           T128(((PADD-1)*NUMREP)+11,IY,IS) = RFIPQDS(PADD,IY,2)
           T128(((PADD-1)*NUMREP)+12,IY,IS) = RFIPQRL(PADD,IY,2)
           T128(((PADD-1)*NUMREP)+13,IY,IS) = RFIPQRH(PADD,IY,2)
           T128(((PADD-1)*NUMREP)+14,IY,IS) = RFIPQPF(PADD,IY,2)
           T128(((PADD-1)*NUMREP)+15,IY,IS) = RFIPQPC(PADD,IY,2)
           T128(((PADD-1)*NUMREP)+16,IY,IS) = RFIPQRG(PADD,IY,2) + RFIPQRBOB(PADD,IY,2)
           T128(((PADD-1)*NUMREP)+17,IY,IS) = RFPQUFC(PADD,IY,2) * 1000.
           T128(((PADD-1)*NUMREP)+18,IY,IS) = FSUM(T128(((PADD-1)*NUMREP)+1,IY,IS),17)
           T128(181:190,IY,IS) = RFQEXPRDT(1:10,IY) * 1000.

!  go into table 129

           T129(((PADD-1)*NUMREP)+ 1,IY,IS) = RFIPQAS(PADD,IY,1)
           T129(((PADD-1)*NUMREP)+ 2,IY,IS) = RFIPQAG(PADD,IY,1)
           T129(((PADD-1)*NUMREP)+ 3,IY,IS) = RFIPQCG(PADD,IY,1)

           IF ((RFIPQMG(PADD,IY,2) + RFIPQCBOB(PADD,IY,2)) .NE. 0.0) &
           T129(((PADD-1)*NUMREP)+ 4,IY,IS) = (RFIPQMG(PADD,IY,1) * RFIPQMG(PADD,IY,2) + &
                                               RFIPQCBOB(PADD,IY,1) * RFIPQCBOB(PADD,IY,2)) / &
                                              (RFIPQMG(PADD,IY,2) + RFIPQCBOB(PADD,IY,2))
           T129(((PADD-1)*NUMREP)+ 5,IY,IS) = RFIPQCD(PADD,IY,1)
           T129(((PADD-1)*NUMREP)+ 6,IY,IS) = RFIPQDL(PADD,IY,1)
           T129(((PADD-1)*NUMREP)+ 7,IY,IS) = RFIPQDU(PADD,IY,1)
           T129(((PADD-1)*NUMREP)+ 8,IY,IS) = RFIPQJF(PADD,IY,1)
           IF (RFIPQPR(PADD,IY,2) + RFIPQPY(PADD,IY,2) + RFIPQPP(PADD,IY,2) + &
               RFIPQET(PADD,IY,2) + RFIPQBU(PADD,IY,2) + RFIPQIS(PADD,IY,2) .NE. 0.0) &
               T129(((PADD-1)*NUMREP)+ 9,IY,IS) = &
              (RFIPQPR(PADD,IY,1) * RFIPQPR(PADD,IY,2) + &
               RFIPQPY(PADD,IY,1) * RFIPQPY(PADD,IY,2) + &
               RFIPQET(PADD,IY,1) * RFIPQET(PADD,IY,2) + &
               RFIPQBU(PADD,IY,1) * RFIPQBU(PADD,IY,2) + &
               RFIPQIS(PADD,IY,1) * RFIPQIS(PADD,IY,2) + &
               RFIPQPP(PADD,IY,1) * RFIPQPP(PADD,IY,2)) / &
              (RFIPQPR(PADD,IY,2) + RFIPQPY(PADD,IY,2) + RFIPQPP(PADD,IY,2) + &
               RFIPQET(PADD,IY,2) + RFIPQBU(PADD,IY,2) + RFIPQIS(PADD,IY,2))
           T129(((PADD-1)*NUMREP)+10,IY,IS) = RFIPQLU(PADD,IY,1)
           T129(((PADD-1)*NUMREP)+11,IY,IS) = RFIPQDS(PADD,IY,1)
           T129(((PADD-1)*NUMREP)+12,IY,IS) = RFIPQRL(PADD,IY,1)
           T129(((PADD-1)*NUMREP)+13,IY,IS) = RFIPQRH(PADD,IY,1)
           T129(((PADD-1)*NUMREP)+14,IY,IS) = RFIPQPF(PADD,IY,1)
           T129(((PADD-1)*NUMREP)+15,IY,IS) = RFIPQPC(PADD,IY,1)

           IF ((RFIPQRG(PADD,IY,2) + RFIPQRBOB(PADD,IY,2)) .NE. 0.0) &
           T129(((PADD-1)*NUMREP)+16,IY,IS) = (RFIPQRG(PADD,IY,1) * RFIPQRG(PADD,IY,2) + &
                                               RFIPQRBOB(PADD,IY,1) * RFIPQRBOB(PADD,IY,2))/ &
                                              (RFIPQRG(PADD,IY,2) + RFIPQRBOB(PADD,IY,2))

           T129(((PADD-1)*NUMREP)+17,IY,IS) = RFPQUFC(PADD,IY,1)

           IF ((RFIPQAS(PADD,IY,2) + RFIPQAG(PADD,IY,2) + RFIPQCG(PADD,IY,2) + RFIPQMG(PADD,IY,2) + &
                RFIPQRG(PADD,IY,2) + RFIPQCBOB(PADD,IY,2) + RFIPQRBOB(PADD,IY,2) + RFIPQCD(PADD,IY,2) + &
                RFIPQDL(PADD,IY,2) + RFIPQDU(PADD,IY,2) + RFIPQJF(PADD,IY,2) + &
                RFIPQPR(PADD,IY,2) + RFIPQPY(PADD,IY,2) + RFIPQPP(PADD,IY,2) + &
                RFIPQET(PADD,IY,2) + RFIPQBU(PADD,IY,2) + RFIPQIS(PADD,IY,2) + &
                RFIPQDS(PADD,IY,2) + RFIPQRL(PADD,IY,2) + RFIPQRH(PADD,IY,2) + &
                RFIPQPC(PADD,IY,2) + RFIPQPF(PADD,IY,2) + RFPQUFC(PADD,IY,2)*1000.) .NE. 0.0) &
           T129(((PADD-1)*NUMREP)+18,IY,IS) = &
                 (RFIPQMG(PADD,IY,1) * RFIPQMG(PADD,IY,2) + RFIPQRG(PADD,IY,1) * RFIPQRG(PADD,IY,2) + &
                  RFIPQAS(PADD,IY,1) * RFIPQAS(PADD,IY,2) + &
                  RFIPQAG(PADD,IY,1) * RFIPQAG(PADD,IY,2) + &
                  RFIPQCG(PADD,IY,1) * RFIPQCG(PADD,IY,2) + RFIPQCD(PADD,IY,1) * RFIPQCD(PADD,IY,2) + &
                  RFIPQLU(PADD,IY,1) * RFIPQLU(PADD,IY,2) + &
                  RFIPQCBOB(PADD,IY,1) * RFIPQCBOB(PADD,IY,2) + RFIPQRBOB(PADD,IY,1) * RFIPQRBOB(PADD,IY,2) + &
                  RFIPQJF(PADD,IY,1) * RFIPQJF(PADD,IY,2) + &
                  RFIPQPR(PADD,IY,1) * RFIPQPR(PADD,IY,2) + RFIPQPY(PADD,IY,1) * RFIPQPY(PADD,IY,2) + &
                  RFIPQET(PADD,IY,1) * RFIPQET(PADD,IY,2) + RFIPQPP(PADD,IY,1) * RFIPQPP(PADD,IY,2) + &
                  RFIPQBU(PADD,IY,1) * RFIPQBU(PADD,IY,2) + RFIPQIS(PADD,IY,1) * RFIPQIS(PADD,IY,2) + &
                  RFIPQDS(PADD,IY,1) * RFIPQDS(PADD,IY,2) + RFIPQDL(PADD,IY,1) * RFIPQDL(PADD,IY,2) + &
                  RFIPQDU(PADD,IY,1) * RFIPQDU(PADD,IY,2) + RFIPQRL(PADD,IY,1) * RFIPQRL(PADD,IY,2) + &
                  RFIPQRH(PADD,IY,1) * RFIPQRH(PADD,IY,2) + RFIPQPF(PADD,IY,1) * RFIPQPF(PADD,IY,2) + &
                  RFIPQPC(PADD,IY,1) * RFIPQPC(PADD,IY,2) + RFPQUFC(PADD,IY,1) * RFPQUFC(PADD,IY,2)*1000.) / &
                 (RFIPQMG(PADD,IY,2) + RFIPQRG(PADD,IY,2) + RFIPQJF(PADD,IY,2) + &
                  RFIPQPR(PADD,IY,2) + RFIPQPY(PADD,IY,2) + RFIPQPP(PADD,IY,2) + &
                  RFIPQET(PADD,IY,2) + RFIPQBU(PADD,IY,2) + RFIPQIS(PADD,IY,2) + &
                  RFIPQCBOB(PADD,IY,2) + RFIPQRBOB(PADD,IY,2) + RFIPQCD(PADD,IY,2) + &
                  RFIPQDS(PADD,IY,2) + RFIPQDL(PADD,IY,2) + RFIPQDU(PADD,IY,2) + RFIPQRL(PADD,IY,2) + &
                  RFIPQAS(PADD,IY,2) + RFIPQAG(PADD,IY,2) + RFIPQCG(PADD,IY,2) + RFIPQLU(PADD,IY,2) + &
                  RFIPQRH(PADD,IY,2) + RFIPQPC(PADD,IY,2) + RFIPQPF(PADD,IY,2) + RFPQUFC(PADD,IY,2)*1000.)
         ENDDO
       ENDDO


!Tables 130 and 131 are crude oil import volumes (130) and prices (131)
       NUMREP=12                        ! equal to number of rows per PADD
       DO IY = 1,LASTYR
         DO PADD=1,MNUMPR
           T130(((PADD-1)*NUMREP)+ 1,IY,IS) = Q_CRUDE_EXPORTS(PADD, 1,IY+1989)
           T130(((PADD-1)*NUMREP)+ 2,IY,IS) = Q_CRUDE_EXPORTS(PADD, 2,IY+1989)
           T130(((PADD-1)*NUMREP)+ 3,IY,IS) = Q_CRUDE_EXPORTS(PADD, 3,IY+1989)
           T130(((PADD-1)*NUMREP)+ 4,IY,IS) = Q_CRUDE_EXPORTS(PADD, 4,IY+1989)
           T130(((PADD-1)*NUMREP)+ 5,IY,IS) = Q_CRUDE_EXPORTS(PADD, 5,IY+1989)
           T130(((PADD-1)*NUMREP)+ 6,IY,IS) = Q_CRUDE_EXPORTS(PADD, 6,IY+1989)
           T130(((PADD-1)*NUMREP)+ 7,IY,IS) = Q_CRUDE_EXPORTS(PADD, 7,IY+1989)
           T130(((PADD-1)*NUMREP)+ 8,IY,IS) = Q_CRUDE_EXPORTS(PADD, 8,IY+1989)
           T130(((PADD-1)*NUMREP)+ 9,IY,IS) = Q_CRUDE_EXPORTS(PADD, 9,IY+1989)
           T130(((PADD-1)*NUMREP)+10,IY,IS) = Q_CRUDE_EXPORTS(PADD,10,IY+1989)
           T130(((PADD-1)*NUMREP)+11,IY,IS) = Q_CRUDE_EXPORTS(PADD,11,IY+1989)
           T130(((PADD-1)*NUMREP)+12,IY,IS) = sum(Q_CRUDE_EXPORTS(PADD,1:11,IY+1989))
!  go into table 127, why don't we?
           T131(((PADD-1)*NUMREP)+ 1,IY,IS) = P_CRUDE_EXPORTS(PADD, 1,IY+1989)
           T131(((PADD-1)*NUMREP)+ 2,IY,IS) = P_CRUDE_EXPORTS(PADD, 2,IY+1989)
           T131(((PADD-1)*NUMREP)+ 3,IY,IS) = P_CRUDE_EXPORTS(PADD, 3,IY+1989)
           T131(((PADD-1)*NUMREP)+ 4,IY,IS) = P_CRUDE_EXPORTS(PADD, 4,IY+1989)
           T131(((PADD-1)*NUMREP)+ 5,IY,IS) = P_CRUDE_EXPORTS(PADD, 5,IY+1989)
           T131(((PADD-1)*NUMREP)+ 6,IY,IS) = P_CRUDE_EXPORTS(PADD, 6,IY+1989)
           T131(((PADD-1)*NUMREP)+ 7,IY,IS) = P_CRUDE_EXPORTS(PADD, 7,IY+1989)
           T131(((PADD-1)*NUMREP)+ 8,IY,IS) = P_CRUDE_EXPORTS(PADD, 8,IY+1989)
           T131(((PADD-1)*NUMREP)+ 9,IY,IS) = P_CRUDE_EXPORTS(PADD, 9,IY+1989)
           T131(((PADD-1)*NUMREP)+10,IY,IS) = P_CRUDE_EXPORTS(PADD,10,IY+1989)
           T131(((PADD-1)*NUMREP)+11,IY,IS) = P_CRUDE_EXPORTS(PADD,11,IY+1989)

            IF (( Q_CRUDE_EXPORTS(PADD,1,IY+1989) + Q_CRUDE_EXPORTS(PADD,2,IY+1989) + Q_CRUDE_EXPORTS(PADD,3,IY+1989) +  &
                  Q_CRUDE_EXPORTS(PADD,4,IY+1989) + Q_CRUDE_EXPORTS(PADD,5,IY+1989) + Q_CRUDE_EXPORTS(PADD,6,IY+1989) +  &
                  Q_CRUDE_EXPORTS(PADD,7,IY+1989) + Q_CRUDE_EXPORTS(PADD,8,IY+1989) + Q_CRUDE_EXPORTS(PADD,9,IY+1989)  +  &
                  Q_CRUDE_EXPORTS(PADD,10,IY+1989) + Q_CRUDE_EXPORTS(PADD,11,IY+1989)) .GT. 0.0 ) &
           T131(((PADD-1)*NUMREP)+12,IY,IS) = &
               ((P_CRUDE_EXPORTS(PADD,1,IY+1989) * Q_CRUDE_EXPORTS(PADD,1,IY+1989) ) + &
                (P_CRUDE_EXPORTS(PADD,2,IY+1989) * Q_CRUDE_EXPORTS(PADD,2,IY+1989) ) + &
                (P_CRUDE_EXPORTS(PADD,3,IY+1989) * Q_CRUDE_EXPORTS(PADD,3,IY+1989) ) + &
                (P_CRUDE_EXPORTS(PADD,4,IY+1989) * Q_CRUDE_EXPORTS(PADD,4,IY+1989) ) + &
                (P_CRUDE_EXPORTS(PADD,5,IY+1989) * Q_CRUDE_EXPORTS(PADD,5,IY+1989) ) + &
                (P_CRUDE_EXPORTS(PADD,6,IY+1989) * Q_CRUDE_EXPORTS(PADD,6,IY+1989) ) + &
                (P_CRUDE_EXPORTS(PADD,7,IY+1989) * Q_CRUDE_EXPORTS(PADD,7,IY+1989) ) + &
                (P_CRUDE_EXPORTS(PADD,8,IY+1989) * Q_CRUDE_EXPORTS(PADD,8,IY+1989) ) + &
                (P_CRUDE_EXPORTS(PADD,9,IY+1989) * Q_CRUDE_EXPORTS(PADD,9,IY+1989) ) + &
                (P_CRUDE_EXPORTS(PADD,10,IY+1989) * Q_CRUDE_EXPORTS(PADD,10,IY+1989) ) + &
                (P_CRUDE_EXPORTS(PADD,11,IY+1989) * Q_CRUDE_EXPORTS(PADD,11,IY+1989) )) / &
               ( Q_CRUDE_EXPORTS(PADD,1,IY+1989) + Q_CRUDE_EXPORTS(PADD,2,IY+1989) + Q_CRUDE_EXPORTS(PADD,3,IY+1989) +  &
                 Q_CRUDE_EXPORTS(PADD,4,IY+1989) + Q_CRUDE_EXPORTS(PADD,5,IY+1989) + Q_CRUDE_EXPORTS(PADD,6,IY+1989) +  &
                 Q_CRUDE_EXPORTS(PADD,7,IY+1989) + Q_CRUDE_EXPORTS(PADD,8,IY+1989) + Q_CRUDE_EXPORTS(PADD,9,IY+1989) +  &
                 Q_CRUDE_EXPORTS(PADD,10,IY+1989) + Q_CRUDE_EXPORTS(PADD,11,IY+1989))
         ENDDO

! PADD Totals for II, III, and V:
         T130(121,IY,IS) = Q_CRUDE_EXPORTS(2, 1,IY+1989) + Q_CRUDE_EXPORTS(3, 1,IY+1989)
         T130(122,IY,IS) = Q_CRUDE_EXPORTS(2, 2,IY+1989) + Q_CRUDE_EXPORTS(3, 2,IY+1989)
         T130(123,IY,IS) = Q_CRUDE_EXPORTS(2, 3,IY+1989) + Q_CRUDE_EXPORTS(3, 3,IY+1989)
         T130(124,IY,IS) = Q_CRUDE_EXPORTS(2, 4,IY+1989) + Q_CRUDE_EXPORTS(3, 4,IY+1989)
         T130(125,IY,IS) = Q_CRUDE_EXPORTS(2, 5,IY+1989) + Q_CRUDE_EXPORTS(3, 5,IY+1989)
         T130(126,IY,IS) = Q_CRUDE_EXPORTS(2, 6,IY+1989) + Q_CRUDE_EXPORTS(3, 6,IY+1989)
         T130(127,IY,IS) = Q_CRUDE_EXPORTS(2, 7,IY+1989) + Q_CRUDE_EXPORTS(3, 7,IY+1989)
         T130(128,IY,IS) = Q_CRUDE_EXPORTS(2, 8,IY+1989) + Q_CRUDE_EXPORTS(3, 8,IY+1989)
         T130(129,IY,IS) = Q_CRUDE_EXPORTS(2, 9,IY+1989) + Q_CRUDE_EXPORTS(3, 9,IY+1989)
         T130(130,IY,IS) = Q_CRUDE_EXPORTS(2,10,IY+1989) + Q_CRUDE_EXPORTS(3,10,IY+1989)
         T130(131,IY,IS) = Q_CRUDE_EXPORTS(2,11,IY+1989) + Q_CRUDE_EXPORTS(3,11,IY+1989)
         T130(132,IY,IS) = FSUM(T130(121,IY,IS),11)
         T130(133,IY,IS) = Q_CRUDE_EXPORTS(4, 1,IY+1989) + Q_CRUDE_EXPORTS(5, 1,IY+1989)
         T130(134,IY,IS) = Q_CRUDE_EXPORTS(4, 2,IY+1989) + Q_CRUDE_EXPORTS(5, 2,IY+1989)
         T130(135,IY,IS) = Q_CRUDE_EXPORTS(4, 3,IY+1989) + Q_CRUDE_EXPORTS(5, 3,IY+1989)
         T130(136,IY,IS) = Q_CRUDE_EXPORTS(4, 4,IY+1989) + Q_CRUDE_EXPORTS(5, 4,IY+1989)
         T130(137,IY,IS) = Q_CRUDE_EXPORTS(4, 5,IY+1989) + Q_CRUDE_EXPORTS(5, 5,IY+1989)
         T130(138,IY,IS) = Q_CRUDE_EXPORTS(4, 6,IY+1989) + Q_CRUDE_EXPORTS(5, 6,IY+1989)
         T130(139,IY,IS) = Q_CRUDE_EXPORTS(4, 7,IY+1989) + Q_CRUDE_EXPORTS(5, 7,IY+1989)
         T130(140,IY,IS) = Q_CRUDE_EXPORTS(4, 8,IY+1989) + Q_CRUDE_EXPORTS(5, 8,IY+1989)
         T130(141,IY,IS) = Q_CRUDE_EXPORTS(4, 9,IY+1989) + Q_CRUDE_EXPORTS(5, 9,IY+1989)
         T130(142,IY,IS) = Q_CRUDE_EXPORTS(4,10,IY+1989) + Q_CRUDE_EXPORTS(5,10,IY+1989)
         T130(143,IY,IS) = Q_CRUDE_EXPORTS(4,11,IY+1989) + Q_CRUDE_EXPORTS(5,11,IY+1989)
         T130(144,IY,IS) = FSUM(T130(133,IY,IS),11)
         T130(145,IY,IS) = Q_CRUDE_EXPORTS(7, 1,IY+1989) + Q_CRUDE_EXPORTS(8, 1,IY+1989)
         T130(146,IY,IS) = Q_CRUDE_EXPORTS(7, 2,IY+1989) + Q_CRUDE_EXPORTS(8, 2,IY+1989)
         T130(147,IY,IS) = Q_CRUDE_EXPORTS(7, 3,IY+1989) + Q_CRUDE_EXPORTS(8, 3,IY+1989)
         T130(148,IY,IS) = Q_CRUDE_EXPORTS(7, 4,IY+1989) + Q_CRUDE_EXPORTS(8, 4,IY+1989)
         T130(149,IY,IS) = Q_CRUDE_EXPORTS(7, 5,IY+1989) + Q_CRUDE_EXPORTS(8, 5,IY+1989)
         T130(150,IY,IS) = Q_CRUDE_EXPORTS(7, 6,IY+1989) + Q_CRUDE_EXPORTS(8, 6,IY+1989)
         T130(151,IY,IS) = Q_CRUDE_EXPORTS(7, 7,IY+1989) + Q_CRUDE_EXPORTS(8, 7,IY+1989)
         T130(152,IY,IS) = Q_CRUDE_EXPORTS(7, 8,IY+1989) + Q_CRUDE_EXPORTS(8, 8,IY+1989)
         T130(153,IY,IS) = Q_CRUDE_EXPORTS(7, 9,IY+1989) + Q_CRUDE_EXPORTS(8, 9,IY+1989)
         T130(154,IY,IS) = Q_CRUDE_EXPORTS(7,10,IY+1989) + Q_CRUDE_EXPORTS(8,10,IY+1989)
         T130(155,IY,IS) = Q_CRUDE_EXPORTS(7,11,IY+1989) + Q_CRUDE_EXPORTS(8,11,IY+1989)
         T130(156,IY,IS) = FSUM(T130(145,IY,IS),11)
         DO PADD=1,11    !  re-using PADD counter variable here though we're looping over crude types
            IF (Q_CRUDE_EXPORTS(2,PADD,IY+1989) + Q_CRUDE_EXPORTS(3,PADD,IY+1989) .GT. 0.0) &
           T131(120+PADD,IY,IS) = &
               ((P_CRUDE_EXPORTS(2,PADD,IY+1989) * Q_CRUDE_EXPORTS(2,PADD,IY+1989)) + &
                (P_CRUDE_EXPORTS(3,PADD,IY+1989) * Q_CRUDE_EXPORTS(3,PADD,IY+1989))) / &
                (Q_CRUDE_EXPORTS(2,PADD,IY+1989) + Q_CRUDE_EXPORTS(3,PADD,IY+1989))
            IF (Q_CRUDE_EXPORTS(4,PADD,IY+1989) + Q_CRUDE_EXPORTS(5,PADD,IY+1989) .GT. 0.0) &
           T131(132+PADD,IY,IS) = &
               ((P_CRUDE_EXPORTS(4,PADD,IY+1989) * Q_CRUDE_EXPORTS(4,PADD,IY+1989)) + &
                (P_CRUDE_EXPORTS(5,PADD,IY+1989) * Q_CRUDE_EXPORTS(5,PADD,IY+1989))) / &
                (Q_CRUDE_EXPORTS(4,PADD,IY+1989) + Q_CRUDE_EXPORTS(5,PADD,IY+1989))
            IF (Q_CRUDE_EXPORTS(7,PADD,IY+1989) + Q_CRUDE_EXPORTS(8,PADD,IY+1989) .GT. 0.0) &
           T131(144+PADD,IY,IS) = &
               ((P_CRUDE_EXPORTS(7,PADD,IY+1989) * Q_CRUDE_EXPORTS(7,PADD,IY+1989)) + &
                (P_CRUDE_EXPORTS(8,PADD,IY+1989) * Q_CRUDE_EXPORTS(8,PADD,IY+1989))) / &
                (Q_CRUDE_EXPORTS(7,PADD,IY+1989) + Q_CRUDE_EXPORTS(8,PADD,IY+1989))
         ENDDO

         IF ((sum(T130(121:131,IY,IS))) .GT. 0.0)     T131(132,IY,IS) = &
               ((T130(121,IY,IS) * T131(121,IY,IS)) + (T130(122,IY,IS) * T131(122,IY,IS)) + &
                (T130(123,IY,IS) * T131(123,IY,IS)) + (T130(124,IY,IS) * T131(124,IY,IS)) + &
                (T130(125,IY,IS) * T131(125,IY,IS)) + (T130(126,IY,IS) * T131(126,IY,IS)) + &
                (T130(127,IY,IS) * T131(127,IY,IS)) + (T130(128,IY,IS) * T131(128,IY,IS)) + &
                (T130(129,IY,IS) * T131(129,IY,IS)) + &
                (T130(130,IY,IS) * T131(130,IY,IS)) + (T130(131,IY,IS) * T131(131,IY,IS))) / &
                              (sum(T130(121:131,IY,IS)))
         IF ((sum(T130(133:143,IY,IS))) .GT. 0.0)     T131(144,IY,IS) = &
               ((T130(133,IY,IS) * T131(133,IY,IS)) + (T130(134,IY,IS) * T131(134,IY,IS)) + &
                (T130(135,IY,IS) * T131(135,IY,IS)) + (T130(136,IY,IS) * T131(136,IY,IS)) + &
                (T130(137,IY,IS) * T131(137,IY,IS)) + (T130(138,IY,IS) * T131(138,IY,IS)) + &
                (T130(139,IY,IS) * T131(139,IY,IS)) + (T130(140,IY,IS) * T131(140,IY,IS)) + &
                (T130(141,IY,IS) * T131(141,IY,IS)) + &
                (T130(142,IY,IS) * T131(142,IY,IS)) + (T130(143,IY,IS) * T131(143,IY,IS))) / &
                              (sum(T130(133:143,IY,IS)))
         IF ((sum(T130(145:155,IY,IS))) .GT. 0.0)     T131(156,IY,IS) = &
               ((T130(145,IY,IS) * T131(145,IY,IS)) + (T130(146,IY,IS) * T131(146,IY,IS)) + &
                (T130(147,IY,IS) * T131(147,IY,IS)) + (T130(148,IY,IS) * T131(148,IY,IS)) + &
                (T130(149,IY,IS) * T131(149,IY,IS)) + (T130(150,IY,IS) * T131(150,IY,IS)) + &
                (T130(151,IY,IS) * T131(151,IY,IS)) + (T130(152,IY,IS) * T131(152,IY,IS)) + &
                (T130(153,IY,IS) * T131(153,IY,IS)) + &
                (T130(154,IY,IS) * T131(154,IY,IS)) + (T130(155,IY,IS) * T131(155,IY,IS))) / &
                              (sum(T130(145:155,IY,IS)))
       ENDDO


!  Table 132:  crude oil refinery inputs

       DO IY = 1,LASTYR
! assign individual types in individual regions
          T132(01:11,IY,IS) = P_RFCRUDEINP(1,1:11,IY)
          T132(13:23,IY,IS) = P_RFCRUDEINP(2,1:11,IY)
          T132(25:35,IY,IS) = P_RFCRUDEINP(3,1:11,IY)
          T132(37:47,IY,IS) = P_RFCRUDEINP(4,1:11,IY)
          T132(49:59,IY,IS) = P_RFCRUDEINP(5,1:11,IY)
          T132(61:71,IY,IS) = P_RFCRUDEINP(6,1:11,IY)
          T132(73:83,IY,IS) = P_RFCRUDEINP(7,1:11,IY)
          T132(85:95,IY,IS) = P_RFCRUDEINP(8,1:11,IY)
          T132(109:119,IY,IS) = P_RFCRUDEINP(9,1:11,IY)
!  average for United States:
          DO PADD=1,11             ! not PADDS but crude types, but PADD exists
            IF (sum(RFCRUDEINP(1:8,PADD,IY)) .NE. 0.0) &
            T132(96+PADD,IY,IS) =(P_RFCRUDEINP(1,PADD,IY)*RFCRUDEINP(1,PADD,IY) + &
                                  P_RFCRUDEINP(2,PADD,IY)*RFCRUDEINP(2,PADD,IY) + &
                                  P_RFCRUDEINP(3,PADD,IY)*RFCRUDEINP(3,PADD,IY) + &
                                  P_RFCRUDEINP(4,PADD,IY)*RFCRUDEINP(4,PADD,IY) + &
                                  P_RFCRUDEINP(5,PADD,IY)*RFCRUDEINP(5,PADD,IY) + &
                                  P_RFCRUDEINP(6,PADD,IY)*RFCRUDEINP(6,PADD,IY) + &
                                  P_RFCRUDEINP(7,PADD,IY)*RFCRUDEINP(7,PADD,IY) + &
                                  P_RFCRUDEINP(8,PADD,IY)*RFCRUDEINP(8,PADD,IY))/ &
                          sum(RFCRUDEINP(1:8,PADD,IY))
          ENDDO
! averages for regions
          IF (sum(T106(01:11,IY,IS)) .NE. 0.0) T132(12,IY,IS) = &
             (T106(01,IY,IS) * T132(01,IY,IS) + T106(02,IY,IS) * T132(02,IY,IS) + &
              T106(03,IY,IS) * T132(03,IY,IS) + T106(04,IY,IS) * T132(04,IY,IS) + &
              T106(05,IY,IS) * T132(05,IY,IS) + T106(06,IY,IS) * T132(06,IY,IS) + &
              T106(07,IY,IS) * T132(07,IY,IS) + T106(08,IY,IS) * T132(08,IY,IS) + &
              T106(09,IY,IS) * T132(09,IY,IS) + T106(10,IY,IS) * T132(10,IY,IS) + &
              T106(11,IY,IS) * T132(11,IY,IS))/ sum(T106(01:11,IY,IS))
          IF (sum(T106(13:23,IY,IS)) .NE. 0.0) T132(24,IY,IS) = &
             (T106(13,IY,IS) * T132(13,IY,IS) + T106(14,IY,IS) * T132(14,IY,IS) + &
              T106(15,IY,IS) * T132(15,IY,IS) + T106(16,IY,IS) * T132(16,IY,IS) + &
              T106(17,IY,IS) * T132(17,IY,IS) + T106(18,IY,IS) * T132(18,IY,IS) + &
              T106(19,IY,IS) * T132(19,IY,IS) + T106(20,IY,IS) * T132(20,IY,IS) + &
              T106(21,IY,IS) * T132(21,IY,IS) + T106(22,IY,IS) * T132(22,IY,IS) + &
              T106(23,IY,IS) * T132(23,IY,IS))/ sum(T106(13:23,IY,IS))
          IF (sum(T106(25:35,IY,IS)) .NE. 0.0) T132(36,IY,IS) = &
             (T106(25,IY,IS) * T132(25,IY,IS) + T106(26,IY,IS) * T132(26,IY,IS) + &
              T106(27,IY,IS) * T132(27,IY,IS) + T106(28,IY,IS) * T132(28,IY,IS) + &
              T106(29,IY,IS) * T132(29,IY,IS) + T106(30,IY,IS) * T132(30,IY,IS) + &
              T106(31,IY,IS) * T132(31,IY,IS) + T106(32,IY,IS) * T132(32,IY,IS) + &
              T106(33,IY,IS) * T132(33,IY,IS) + T106(34,IY,IS) * T132(34,IY,IS) + &
              T106(35,IY,IS) * T132(35,IY,IS))/ sum(T106(25:35,IY,IS))
          IF (sum(T106(37:47,IY,IS)) .NE. 0.0) T132(48,IY,IS) = &
             (T106(37,IY,IS) * T132(37,IY,IS) + T106(38,IY,IS) * T132(38,IY,IS) + &
              T106(39,IY,IS) * T132(39,IY,IS) + T106(40,IY,IS) * T132(40,IY,IS) + &
              T106(41,IY,IS) * T132(41,IY,IS) + T106(42,IY,IS) * T132(42,IY,IS) + &
              T106(43,IY,IS) * T132(43,IY,IS) + T106(44,IY,IS) * T132(44,IY,IS) + &
              T106(45,IY,IS) * T132(45,IY,IS) + T106(46,IY,IS) * T132(46,IY,IS) + &
              T106(47,IY,IS) * T132(47,IY,IS))/ sum(T106(37:47,IY,IS))
          IF (sum(T106(49:59,IY,IS)) .NE. 0.0) T132(60,IY,IS) = &
             (T106(49,IY,IS) * T132(49,IY,IS) + T106(50,IY,IS) * T132(50,IY,IS) + &
              T106(51,IY,IS) * T132(51,IY,IS) + T106(52,IY,IS) * T132(52,IY,IS) + &
              T106(53,IY,IS) * T132(53,IY,IS) + T106(54,IY,IS) * T132(54,IY,IS) + &
              T106(55,IY,IS) * T132(55,IY,IS) + T106(56,IY,IS) * T132(56,IY,IS) + &
              T106(57,IY,IS) * T132(57,IY,IS) + T106(58,IY,IS) * T132(58,IY,IS) + &
              T106(59,IY,IS) * T132(59,IY,IS))/ sum(T106(49:59,IY,IS))
          IF (sum(T106(61:71,IY,IS)) .NE. 0.0) T132(72,IY,IS) = &
             (T106(61,IY,IS) * T132(61,IY,IS) + T106(62,IY,IS) * T132(62,IY,IS) + &
              T106(63,IY,IS) * T132(63,IY,IS) + T106(64,IY,IS) * T132(64,IY,IS) + &
              T106(65,IY,IS) * T132(65,IY,IS) + T106(66,IY,IS) * T132(66,IY,IS) + &
              T106(67,IY,IS) * T132(67,IY,IS) + T106(68,IY,IS) * T132(68,IY,IS) + &
              T106(69,IY,IS) * T132(69,IY,IS) + T106(70,IY,IS) * T132(70,IY,IS) + &
              T106(71,IY,IS) * T132(71,IY,IS))/ sum(T106(61:71,IY,IS))
          IF (sum(T106(73:83,IY,IS)) .NE. 0.0) T132(84,IY,IS) = &
             (T106(73,IY,IS) * T132(73,IY,IS) + T106(74,IY,IS) * T132(74,IY,IS) + &
              T106(75,IY,IS) * T132(75,IY,IS) + T106(76,IY,IS) * T132(76,IY,IS) + &
              T106(77,IY,IS) * T132(77,IY,IS) + T106(78,IY,IS) * T132(78,IY,IS) + &
              T106(79,IY,IS) * T132(79,IY,IS) + T106(80,IY,IS) * T132(80,IY,IS) + &
              T106(81,IY,IS) * T132(81,IY,IS) + T106(82,IY,IS) * T132(82,IY,IS) + &
              T106(83,IY,IS) * T132(83,IY,IS))/ sum(T106(73:83,IY,IS))
          IF (sum(T106(85:95,IY,IS)) .NE. 0.0) T132(96,IY,IS) = &
             (T106(85,IY,IS) * T132(85,IY,IS) + T106(86,IY,IS) * T132(86,IY,IS) + &
              T106(87,IY,IS) * T132(87,IY,IS) + T106(88,IY,IS) * T132(88,IY,IS) + &
              T106(89,IY,IS) * T132(89,IY,IS) + T106(90,IY,IS) * T132(90,IY,IS) + &
              T106(91,IY,IS) * T132(91,IY,IS) + T106(92,IY,IS) * T132(92,IY,IS) + &
              T106(93,IY,IS) * T132(93,IY,IS) + T106(94,IY,IS) * T132(94,IY,IS) + &
              T106(95,IY,IS) * T132(95,IY,IS))/ sum(T106(85:95,IY,IS))
          IF (sum(T106(97:107,IY,IS)) .NE. 0.0) T132(108,IY,IS) = &
             (T106( 97,IY,IS) * T132( 97,IY,IS) + T106( 98,IY,IS) * T132( 98,IY,IS) + &
              T106( 99,IY,IS) * T132( 99,IY,IS) + T106(100,IY,IS) * T132(100,IY,IS) + &
              T106(101,IY,IS) * T132(101,IY,IS) + T106(102,IY,IS) * T132(102,IY,IS) + &
              T106(103,IY,IS) * T132(103,IY,IS) + T106(104,IY,IS) * T132(104,IY,IS) + &
              T106(105,IY,IS) * T132(105,IY,IS) + T106(106,IY,IS) * T132(106,IY,IS) + &
              T106(107,IY,IS) * T132(107,IY,IS))/ sum(T106(97:107,IY,IS))
          IF (sum(T106(109:119,IY,IS)) .NE. 0.0) T132(120,IY,IS) = &
             (T106(109,IY,IS) * T132(109,IY,IS) + T106(110,IY,IS) * T132(110,IY,IS) + &
              T106(111,IY,IS) * T132(111,IY,IS) + T106(112,IY,IS) * T132(112,IY,IS) + &
              T106(113,IY,IS) * T132(113,IY,IS) + T106(114,IY,IS) * T132(114,IY,IS) + &
              T106(115,IY,IS) * T132(115,IY,IS) + T106(116,IY,IS) * T132(116,IY,IS) + &
              T106(117,IY,IS) * T132(117,IY,IS) + T106(118,IY,IS) * T132(118,IY,IS) + &
              T106(119,IY,IS) * T132(119,IY,IS))/ sum(T106(109:119,IY,IS))
       ENDDO


! Table 133 - blank - T133(
       NUMREP=5                       ! equal to number of rows per IR 24*5=120
       DO IY = 1,LASTYR
         DO IR=1,MAXNFR
           T133(((IR-1)*NUMREP)+1,IY,IS) = CAPCCS(1,IR,IY)
           T133(((IR-1)*NUMREP)+2,IY,IS) = CAPCCS(2,IR,IY)
           T133(((IR-1)*NUMREP)+3,IY,IS) = CAPCCS(3,IR,IY)
           T133(((IR-1)*NUMREP)+4,IY,IS) = CAPCCS(4,IR,IY)
           T133(((IR-1)*NUMREP)+5,IY,IS) = FSUM(T133(((IR-1)*NUMREP)+1,IY,IS),4)
         ENDDO
       ENDDO


         NUMREP=1                       ! equal to number of rows per IR 24
         DO IY = 1,LASTYR
              T133(144,IY,IS) = Copccs(MAXNFR,IY) + C_AVGTF(8,IY)
         ENDDO

         NUMREP=1                       ! equal to number of rows per IR 24
       DO IY = 1,LASTYR
         DO IR=1,MAXNFR
             T133(((IR-1)*NUMREP)+145,IY,IS) = Copccs(IR,IY)
           ENDDO
         ENDDO

        NUMREP=1                       ! equal to number of rows per IR 8
         DO IY = 1,LASTYR
           DO IR=1,8
              T133(((IR-1)*NUMREP)+169,IY,IS) = C_AVGTF(IR,IY)
         ENDDO
       ENDDO

! IF (RUN45Q .GT. 0) THEN
   DO IY = 1,LASTYR
!45Q Sequestration Tax Credits
!CO2 Captured - eligible for credits (MMT)
!by plant type
         T133(177,IY,IS) = CO2_CCS(0,0,2,IY)/18000
         T133(178,IY,IS) = sum(OGCO245Q(MNLNP1,1:13,IY))/18000
         T133(179,IY,IS) = T133(177,IY,IS) +  T133(178,IY,IS)
!by storage type
         T133(180,IY,IS) = sum(CO2_CCS(0,1:7,2,IY))/18000 + sum(OGCO245Q(MNLNP1,1:13,IY))/18000
         T133(181,IY,IS) = CO2_CCS(0,-1,2,IY)/18000
         T133(182,IY,IS) = T133(180,IY,IS) +  T133(181,IY,IS)

!45Q Credit Payments ($ Million)
!by plant type
         T133(183,IY,IS) = ((sum(CO2_CCS(0,1:7,2,IY))/18000 * CCS_EOR_45Q(IY)) + (CO2_CCS(0,-1,2,IY)/18000 * CCS_SALINE_45Q(IY))) * SCALPR
         T133(184,IY,IS) = (sum(OGCO245Q(MNLNP1,1:13,IY))/18000 * CCS_EOR_45Q(IY)) * SCALPR
         T133(185,IY,IS) = T133(183,IY,IS) +  T133(184,IY,IS)
!by storage type
         T133(186,IY,IS) = ((sum(CO2_CCS(0,1:7,2,IY))/18000 * CCS_EOR_45Q(IY))  + (sum(OGCO245Q(MNLNP1,1:13,IY))/18000 * CCS_EOR_45Q(IY))) * SCALPR
         T133(187,IY,IS) = (CO2_CCS(0,-1,2,IY)/18000 * CCS_SALINE_45Q(IY)) * SCALPR
         T133(188,IY,IS) = T133(186,IY,IS) +  T133(187,IY,IS)

!        WRITE(6,2710) IS, IY+1989, UCAPISU(MNUMNR,IY), UCAPISN(MNUMNR,IY), UCAPSQU(MNUMNR,IY), UCAPSQN(MNUMNR,IY), UCAPPQU(MNUMNR,IY), UCAPPQN(MNUMNR,IY)
!2710    FORMAT(1X,"COAL_CAP",2(",",I5),6(",",F21.6))
!
!        WRITE(6,2711) IS, IY+1989, UCAPASU(MNUMNR,IY), UCAPASN(MNUMNR,IY), UCAPA2U(MNUMNR,IY), UCAPA2N(MNUMNR,IY)
!2711    FORMAT(1X,"NG___CAP",2(",",I5),4(",",F21.6))
!
!        WRITE(6,2712) IS, IY+1989, UGENIS(MNUMNR,IY), UGENSQ(MNUMNR,IY), UGENPQ(MNUMNR,IY), UGENIS_ALT(MNUMNR,IY), UGENSQ_ALT(MNUMNR,IY), UGENPQ_ALT(MNUMNR,IY)
!2712    FORMAT(1X,"COAL_GEN",2(",",I5),6(",",F21.6))
!
!        WRITE(6,2713) IS, IY+1989, UGENCS(MNUMNR,IY), UGENCS_ALT(MNUMNR,IY), UGENA2(MNUMNR,IY), UGENA2_ALT(MNUMNR,IY)
!2713    FORMAT(1X,"NG___GEN",2(",",I5),4(",",F21.6))
!
!        WRITE(6,2714) IS, IY+1989, BTU_CCS(0,0,2,IY), BTU_CCS(1,0,2,IY), BTU_CCS(2,0,2,IY), BTU_CCS(3,0,2,IY), BTU_CCS(4,0,2,IY), &
!           BTU_CCS(0,0,0,IY), BTU_CCS(1,0,0,IY), BTU_CCS(2,0,0,IY), BTU_CCS(3,0,0,IY), BTU_CCS(4,0,0,IY)
!2714    FORMAT(1X,"BTU_CCS",2(",",I5),10(",",F21.6))
!
!        WRITE(6,2715) IS, IY+1989, CO2_CCS(0,0,2,IY), CO2_CCS(0,-1,2,IY), CO2_CCS(0,1,2,IY), CO2_CCS(0,2,2,IY), CO2_CCS(0,3,2,IY), CO2_CCS(0,4,2,IY), CO2_CCS(0,5,2,IY), &
!           CO2_CCS(0,6,2,IY), CO2_CCS(0,7,2,IY), CO2_CCS(0,8,2,IY)
!2715    FORMAT(1X,"CO2_CCS",2(",",I5),10(",",F21.6))

!Power Plants Eligible for Credits
!Capacity by Fuel (GW)
         T133(189,IY,IS) = 0.0
         T133(190,IY,IS) = 0.0

         IF ((BTU_CCS(1,0,0,IY) + BTU_CCS(2,0,0,IY)) .GT. 0.001) THEN
            T133(189,IY,IS) = (UCAPISU(MNUMNR,IY) + UCAPISN(MNUMNR,IY) + UCAPSQU(MNUMNR,IY) + UCAPSQN(MNUMNR,IY) + UCAPPQU(MNUMNR,IY) + UCAPPQN(MNUMNR,IY)) * &
               (BTU_CCS(1,0,2,IY) + BTU_CCS(2,0,2,IY)) / (BTU_CCS(1,0,0,IY) + BTU_CCS(2,0,0,IY))
         END IF
         IF ((BTU_CCS(3,0,0,IY) + BTU_CCS(4,0,0,IY)) .GT. 0.001) THEN
            T133(190,IY,IS) = (UCAPASU(MNUMNR,IY) + UCAPASN(MNUMNR,IY) + UCAPA2U(MNUMNR,IY) + UCAPA2N(MNUMNR,IY)) * &
               (BTU_CCS(3,0,2,IY) + BTU_CCS(4,0,2,IY)) / (BTU_CCS(3,0,0,IY) + BTU_CCS(4,0,0,IY))
         END IF
         T133(191,IY,IS) = T133(189,IY,IS) + T133(190,IY,IS)
!Generation by Fuel (GW)
         T133(192,IY,IS) = 0.0
         T133(193,IY,IS) = 0.0

         IF ((BTU_CCS(1,0,0,IY) + BTU_CCS(2,0,0,IY)) .GT. 0.001) THEN
            T133(192,IY,IS) = (UGENIS(MNUMNR,IY) + UGENSQ(MNUMNR,IY) + UGENPQ(MNUMNR,IY) - (UGENIS_ALT(MNUMNR,IY) + UGENSQ_ALT(MNUMNR,IY) + UGENPQ_ALT(MNUMNR,IY))) * &
               (BTU_CCS(1,0,2,IY) + BTU_CCS(2,0,2,IY)) / (BTU_CCS(1,0,0,IY) + BTU_CCS(2,0,0,IY))
         END IF
         IF ((BTU_CCS(3,0,0,IY) + BTU_CCS(4,0,0,IY)) .GT. 0.001) THEN
            T133(193,IY,IS) = (UGENCS(MNUMNR,IY) - UGENCS_ALT(MNUMNR,IY) + UGENA2(MNUMNR,IY) - UGENA2_ALT(MNUMNR,IY)) * &
               (BTU_CCS(3,0,2,IY) + BTU_CCS(4,0,2,IY)) / (BTU_CCS(3,0,0,IY) + BTU_CCS(4,0,0,IY))
         END IF
         T133(194,IY,IS) = T133(192,IY,IS) + T133(193,IY,IS)
!Co2 captured by fuel (MMT)
         T133(195,IY,IS) = 0.0
         T133(196,IY,IS) = 0.0
         IF (BTU_CCS(0,0,2,IY) .GT. 0.001) THEN
            T133(195,IY,IS) = (CO2_CCS(0,0,2,IY) / 18000.0) * ( (BTU_CCS(1,0,2,IY) + BTU_CCS(2,0,2,IY)) / BTU_CCS(0,0,2,IY) )
            T133(196,IY,IS) = (CO2_CCS(0,0,2,IY) / 18000.0) * ( (BTU_CCS(3,0,2,IY) + BTU_CCS(4,0,2,IY)) / BTU_CCS(0,0,2,IY) )
         END IF
         T133(197,IY,IS) = T133(195,IY,IS) + T133(196,IY,IS)
        ENDDO
!      ENDIF

  ! Table 134 - Pipeline, Transport, and Storage Costs for Captured Carbon
  !  assumed these are in 1987$
         NUMREP=3                       ! equal to number of rows per IR 3*24=72
       DO IY = 1,LASTYR
         DO IR=1,MAXNFR
             T134(((IR-1)*NUMREP)+1,IY,IS) = TFCCS(IR,IY) * SCALPR
             T134(((IR-1)*NUMREP)+2,IY,IS) = TVCCS(IR,IY) * SCALPR
             T134(((IR-1)*NUMREP)+3,IY,IS) = FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
         ENDDO
       ENDDO

         NUMREP=3                       ! equal to number of rows per IR 3*24=72
       DO IY = 1,LASTYR
         DO IR=1,MAXNFR
             T134(((IR-1)*NUMREP)+73,IY,IS) = IFCCS(IR,IY) * SCALPR
             T134(((IR-1)*NUMREP)+74,IY,IS) = IVCCS(IR,IY) * SCALPR
             T134(((IR-1)*NUMREP)+75,IY,IS) = FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
         ENDDO
       ENDDO


! Table 135 - Key Results for Coal Mining Costs Cases

       DO IY=1,LASTYR
!        --- Prices
         T135(1,IY,IS)=CPSB(3,IY)*CPSBT(3,IY)
         T135(2,IY,IS)=PCLEL(11,IY)
!        --- Labor Productivity
         T135(3,IY,IS)=TOTLABPROD(IY)
         T135(4,IY,IS)=LABPRODGROWTH(IY)
!        --- Wages
         T135(5,IY,IS)=WAGEPHOUR(IY)
         T135(6,IY,IS)=WAGEGROWTH(IY)
!         --- Carbon Emissions by Electric Generators
         T135( 7,IY,IS) = EMEL(2,1,IY)
         T135( 8,IY,IS) = EMEL(1,1,IY)
         T135( 9,IY,IS) = EMEL(3,1,IY)
         T135(10,IY,IS) = EMEL(4,1,IY)
         T135(11,IY,IS) = sum(EMEL(1:4,1,IY))
!         --- Electric Power Sector Capability
         T135(12,IY,IS) = UCAPCSU(mnumnr,IY) + UCAPCSN(mnumnr,IY) + UCAPCSC(mnumnr,IY)
         T135(13,IY,IS) = UCAPCCU(mnumnr,IY) + UCAPCCN(mnumnr,IY) + &
                          UCAPCTU(mnumnr,IY) + UCAPCTN(mnumnr,IY) + &
                          UCAPFCU(mnumnr,IY) + UCAPFCN(mnumnr,IY) + &
                          UCAPNUU(mnumnr,IY) + UCAPNUN(mnumnr,IY) + &
                          UCAPSMU(mnumnr,IY) + UCAPSMN(mnumnr,IY) + &
                          UCAPOSU(mnumnr,IY) + UCAPOSN(mnumnr,IY) + &
                          UCAPNGU(mnumnr,IY) + UCAPNGN(mnumnr,IY) + &
                          UCAPPSU(mnumnr,IY) + UCAPPSN(mnumnr,IY) + &
                          UCAPHYU(mnumnr,IY) + UCAPGEU(mnumnr,IY) + &
                          UCAPMSU(mnumnr,IY) + UCAPWDU(mnumnr,IY) + &
                          UCAPSTU(mnumnr,IY) + UCAPPVU(mnumnr,IY) + UCAPWNU(mnumnr,IY) + &
                          UCAPWFU(mnumnr,IY) + UCAPWLU(mnumnr,IY) + UCAPPTU(mnumnr,IY) + &
                          UCAPHYN(mnumnr,IY) + UCAPGEN(mnumnr,IY) + &
                          UCAPMSN(mnumnr,IY) + UCAPWDN(mnumnr,IY) + &
                          UCAPSTN(mnumnr,IY) + UCAPPVN(mnumnr,IY) + UCAPWNN(mnumnr,IY) + &
                          UCAPWFN(mnumnr,IY) + UCAPWLN(mnumnr,IY) + UCAPPTN(mnumnr,IY) + &
                          UCAPDBU(mnumnr,IY) + UCAPDBN(mnumnr,IY) + &
                          UCAPDPU(mnumnr,IY) + UCAPDPN(mnumnr,IY) + UCAPOSC(mnumnr,IY) + &
                          UCAPCCC(mnumnr,IY) + UCAPCTC(mnumnr,IY) + &
                          UCAPHYC(mnumnr,IY) + UCAPGEC(mnumnr,IY) + &
                          UCAPMSC(mnumnr,IY) + UCAPWDC(mnumnr,IY) + &
                          UCAPSTC(mnumnr,IY) + UCAPPVC(mnumnr,IY) + UCAPPTC(mnumnr,IY) + &
                          UCAPWNC(mnumnr,IY) + UCAPWLC(mnumnr,IY) + UCAPWFC(mnumnr,IY)
         T135(14,IY,IS) = UCAPCSU(mnumnr,IY) + UCAPCSN(mnumnr,IY) + &
                          UCAPCCU(mnumnr,IY) + UCAPCCN(mnumnr,IY) + &
                          UCAPCTU(mnumnr,IY) + UCAPCTN(mnumnr,IY) + &
                          UCAPFCU(mnumnr,IY) + UCAPFCN(mnumnr,IY) + &
                          UCAPNUU(mnumnr,IY) + UCAPNUN(mnumnr,IY) + &
                          UCAPOSU(mnumnr,IY) + UCAPOSN(mnumnr,IY) + &
                          UCAPNGU(mnumnr,IY) + UCAPNGN(mnumnr,IY) + &
                          UCAPPSU(mnumnr,IY) + UCAPPSN(mnumnr,IY) + &
                          UCAPHYU(mnumnr,IY) + UCAPGEU(mnumnr,IY) + &
                          UCAPMSU(mnumnr,IY) + UCAPWDU(mnumnr,IY) + &
                          UCAPSTU(mnumnr,IY) + UCAPPVU(mnumnr,IY) + UCAPWNU(mnumnr,IY) + &
                          UCAPWFU(mnumnr,IY) + UCAPWLU(mnumnr,IY) + UCAPPTU(mnumnr,IY) + &
                          UCAPHYN(mnumnr,IY) + UCAPGEN(mnumnr,IY) + &
                          UCAPMSN(mnumnr,IY) + UCAPWDN(mnumnr,IY) + &
                          UCAPSTN(mnumnr,IY) + UCAPPVN(mnumnr,IY) + UCAPWNN(mnumnr,IY) + &
                          UCAPWFN(mnumnr,IY) + UCAPWLN(mnumnr,IY) + UCAPPTN(mnumnr,IY) + &
                          UCAPDBU(mnumnr,IY) + UCAPDBN(mnumnr,IY) + &
                          UCAPDPU(mnumnr,IY) + UCAPDPN(mnumnr,IY) + &
                          UCAPCSC(mnumnr,IY) + UCAPOSC(mnumnr,IY) + &
                          UCAPCCC(mnumnr,IY) + UCAPCTC(mnumnr,IY) + &
                          UCAPHYC(mnumnr,IY) + UCAPGEC(mnumnr,IY) + &
                          UCAPMSC(mnumnr,IY) + UCAPWDC(mnumnr,IY) + &
                          UCAPSTC(mnumnr,IY) + UCAPPVC(mnumnr,IY) + UCAPPTC(mnumnr,IY) + &
                          UCAPWNC(mnumnr,IY) + UCAPWLC(mnumnr,IY) + UCAPWFC(mnumnr,IY)
      ENDDO

! Table 136 - Key Results for Nuclear Cases

       DO IY=1,LASTYR
         T136( 1,IY,IS) = T9(1,IY,IS) + T9(51,IY,IS)
         T136( 2,IY,IS) = T9(2,IY,IS) + T9(52,IY,IS)
         T136( 3,IY,IS) = T9(3,IY,IS) + T9(53,IY,IS)
         T136( 4,IY,IS) = T9(4,IY,IS) + T9(54,IY,IS)
         T136( 5,IY,IS) = T9(5,IY,IS)
         T136( 6,IY,IS) = T9(6,IY,IS)
         T136( 7,IY,IS) = T9(7,IY,IS)
         T136( 8,IY,IS) = T9(8,IY,IS) + T9(55,IY,IS)
         T136( 9,IY,IS) = T9(48,IY,IS)
         T136(10,IY,IS) = T9(44,IY,IS)
         T136(11,IY,IS) = FSUM(T136(1,IY,IS),10)
!        --- CUMULATIVE ADDITIONS
         T136(12,IY,IS) = T9(10,IY,IS) + T9(19,IY,IS)
         T136(13,IY,IS) = T9(11,IY,IS) + T9(20,IY,IS)
         T136(14,IY,IS) = T9(12,IY,IS) + T9(21,IY,IS)
         T136(15,IY,IS) = T9(13,IY,IS) + T9(22,IY,IS)
         T136(16,IY,IS) = T9(14,IY,IS) + T9(23,IY,IS)
         T136(17,IY,IS) = T9(15,IY,IS) + T9(24,IY,IS)
         T136(18,IY,IS) = T9(16,IY,IS) + T9(25,IY,IS)
         T136(19,IY,IS) = T9(17,IY,IS) + T9(26,IY,IS)
         T136(20,IY,IS) = T9(49,IY,IS) + T9(50,IY,IS)
         T136(21,IY,IS) = T9(45,IY,IS)
         T136(22,IY,IS) = FSUM(T136(12,IY,IS),10)
! Retirements
         T136(23,IY,IS) = T9(37,IY,IS)
! Generation
         T136(24,IY,IS) = T8( 1,IY,IS) + T8(42,IY,IS)
         T136(25,IY,IS) = T8( 2,IY,IS) + T8(43,IY,IS)
         T136(26,IY,IS) = T8( 3,IY,IS) + T8(44,IY,IS)
         T136(27,IY,IS) = T8( 4,IY,IS)
         T136(28,IY,IS) = T8( 5,IY,IS) + T8(45,IY,IS)
         T136(29,IY,IS) = T8( 6,IY,IS) + T8(46,IY,IS)
         T136(30,IY,IS) = T8(41,IY,IS)
         T136(31,IY,IS) = T8(15,IY,IS)
         T136(32,IY,IS) = FSUM(T136(24,IY,IS),8)
! Emissions
         T136(33,IY,IS) = T17(20,11,IY,IS)
         T136(34,IY,IS) = T17(21,11,IY,IS)
         T136(35,IY,IS) = T17(22,11,IY,IS)
         T136(40,IY,IS) = T17(23,11,IY,IS)
         T136(36,IY,IS) = FSUM(T136(33,IY,IS),3) + T136(40,IY,IS)
! Prices
         T136(37,IY,IS) = PTPEL(11,IY)
         T136(38,IY,IS) = PNGEL(11,IY)
         T136(39,IY,IS) = PCLEL(11,IY)
! electricity sales, price
         T136(41,IY,IS) = QELAS(11,IY) / .003412
         T136(42,IY,IS) = PELAS(11,IY) * .3412
       ENDDO

! Table 137 - Key Results for High Renewable Energy Cases

       DO IY=1,LASTYR
!        --- ELECTRIC GENERATOR CAPABILITY
         T137(1,IY,IS)=UCAPHYU(mnumnr,IY) + UCAPHYN(mnumnr,IY) + UCAPHYC(mnumnr,IY)
         T137(2,IY,IS)=UCAPGEU(mnumnr,IY) + UCAPGEN(mnumnr,IY) + UCAPGEC(mnumnr,IY)
         T137(3,IY,IS)=UCAPMSU(mnumnr,IY) + UCAPMSN(mnumnr,IY) + UCAPMSC(mnumnr,IY)
         T137(4,IY,IS)=UCAPWDU(mnumnr,IY) + UCAPWDN(mnumnr,IY) + UCAPWDC(mnumnr,IY)
         T137(5,IY,IS)=UCAPSTU(mnumnr,IY) + UCAPSTN(mnumnr,IY) + UCAPSTC(mnumnr,IY)
         T137(6,IY,IS)=UCAPPVU(mnumnr,IY) + UCAPPVN(mnumnr,IY) + UCAPPVC(mnumnr,IY) +&
                       UCAPPTU(mnumnr,IY) + UCAPPTN(mnumnr,IY) + UCAPPTC(mnumnr,IY)
         T137(7,IY,IS)=UCAPWNU(mnumnr,IY) + UCAPWNN(mnumnr,IY) + UCAPWNC(mnumnr,IY) + &
                       UCAPWLU(mnumnr,IY) + UCAPWLN(mnumnr,IY) + UCAPWLC(mnumnr,IY) + &
                       UCAPWFU(mnumnr,IY) + UCAPWFN(mnumnr,IY) + UCAPWFC(mnumnr,IY)
         T137(8,IY,IS)= FSUM(T137(1,IY,IS),7)
!       --- COGENERATORS CAPABILITY
         T137( 9,IY,IS) =(CGCOMMCAP(11,IY,6) + CGREFCAP(11,IY,6) + CGINDLCAP(11,IY,6)) / 1000.
         T137(10,IY,IS) =(CGCOMMCAP(11,IY,7) + CGREFCAP(11,IY,7) + CGINDLCAP(11,IY,7)) / 1000.
         T137(11,IY,IS) = FSUM(T137( 9,IY,IS),2)
!        --- Other Generators
         T137(12,IY,IS) =(CGCOMMCAP(11,IY,4) + CGINDLCAP(11,IY,4)) / 1000.
         T137(13,IY,IS) =(CGCOMMCAP(11,IY,5) + CGINDLCAP(11,IY,5)) / 1000.
         T137(14,IY,IS) =(CGCOMMCAP(11,IY,8) + CGRESCAP(11,IY,8) + CGINDLCAP(11,IY,8)) / 1000.
         T137(49,IY,IS) =(CGCOMMCAP(11,IY,11) + CGRESCAP(11,IY,11) + CGINDLCAP(11,IY,11)) / 1000.
         T137(41,IY,IS) = FSUM(T137(11,IY,IS),4) + T137(49,IY,IS)
!        --- SELECTED FOSSIL FUEL'S ELEC. GENERATOR GENERATION
         T137(15,IY,IS) = UGNCLNR(1,mnumnr,IY) + UGNCLNR(2,mnumnr,IY) + &
                      (CGNTGEN(mnumnr,IY, 1,1) + CGNTGEN(mnumnr,IY, 1,2)) * 0.001
         T137(16,IY,IS) = UGNDSNR(1,mnumnr,IY) + UGNRLNR(1,mnumnr,IY) + &
                       UGNDSNR(2,mnumnr,IY) + UGNRLNR(2,mnumnr,IY) + &
                       UGNRHNR(1,mnumnr,IY) + UGNRHNR(2,mnumnr,IY) + &
                      (CGNTGEN(mnumnr,IY, 2,1) + CGNTGEN(mnumnr,IY, 2,2)) * 0.001
         T137(17,IY,IS) = UGNGFNR(1,mnumnr,IY) + UGNGINR(1,mnumnr,IY) + &
                       UGNGFNR(2,mnumnr,IY) + UGNGINR(2,mnumnr,IY) + &
                       UGNGCNR(1,mnumnr,IY) + UGNGCNR(2,mnumnr,IY) + &
                      (CGNTGEN(mnumnr,IY, 3,1) + CGNTGEN(mnumnr,IY, 3,2)) * 0.001
         T137(18,IY,IS) = FSUM(T137(15,IY,IS),3)
!     --- ELECTRIC GENERATOR GENERATION
         T137(19,IY,IS) = UGNHYNR(1,mnumnr,IY) + UGNHYNR(2,mnumnr,IY) + &
                     (CGNTGEN(mnumnr,IY, 4,1) + CGNTGEN(mnumnr,IY, 4,2)) * 0.001
         T137(20,IY,IS) = UGNGENR(1,mnumnr,IY) + UGNGENR(2,mnumnr,IY) + &
                     (CGNTGEN(mnumnr,IY, 5,1) + CGNTGEN(mnumnr,IY, 5,2)) * 0.001
         T137(21,IY,IS) = UGNMSNR(1,mnumnr,IY) + UGNMSNR(2,mnumnr,IY) + &
                     (CGNTGEN(mnumnr,IY, 6,1) + CGNTGEN(mnumnr,IY, 6,2)) * 0.001
         T137(22,IY,IS) = UGNWDNR(1,mnumnr,IY) + UGNWDNR(2,mnumnr,IY) + &
                     (CGNTGEN(mnumnr,IY, 7,1) + CGNTGEN(mnumnr,IY, 7,2)) * 0.001
         T137(43,IY,IS) = UGNWDNR(1,mnumnr,IY) + UGNWDNR(2,mnumnr,IY) + &
                     (CGNTGEN(mnumnr,IY, 7,1) + CGNTGEN(mnumnr,IY, 7,2)) * 0.001 - &
                          UGNCFNR(1,mnumnr,IY) - UGNCFNR(2,mnumnr,IY)
         T137(44,IY,IS) = UGNCFNR(1,mnumnr,IY) + UGNCFNR(2,mnumnr,IY)
         T137(23,IY,IS) = UGNSONR(1,mnumnr,IY) + UGNSONR(2,mnumnr,IY)
         T137(24,IY,IS) = UGNPVNR(1,mnumnr,IY) + UGNPVNR(2,mnumnr,IY) + &
                          UGNPTNR(1,mnumnr,IY) + UGNPTNR(2,mnumnr,IY) + &
                     (CGNTGEN(mnumnr,IY, 8,1) + CGNTGEN(mnumnr,IY, 8,2)) * 0.001
         T137(25,IY,IS) = UGNWNNR(1,mnumnr,IY) + UGNWNNR(2,mnumnr,IY) + &
                          UGNWLNR(1,mnumnr,IY) + UGNWLNR(2,mnumnr,IY) + &
                          UGNWFNR(1,mnumnr,IY) + UGNWFNR(2,mnumnr,IY)
         T137(26,IY,IS) = FSUM(T137(19,IY,IS),4)+FSUM(T137(23,IY,IS),3)
!        --- SELECTED FOSSIL FUEL'S COGENERATOR GENERATION
         T137(27,IY,IS) = (CGOGSGEN(11,IY,1,1)  + CGOGSGEN(11,IY,1,2) + &
                           CGREFGEN(11,IY,1,1)  + CGREFGEN(11,IY,1,2) + &
                           CGINDLGEN(11,IY,1,1) + CGINDLGEN(11,IY,1,2) + &
                           CGCOMMGEN(11,IY,1,1) + CGCOMMGEN(11,IY,1,2)) * .001
         T137(28,IY,IS) = (CGREFGEN(11,IY,2,1)  + CGREFGEN(11,IY,2,2) + &
                           CGOGSGEN(11,IY,2,1)  + CGOGSGEN(11,IY,2,2) + &
                           CGINDLGEN(11,IY,2,1) + CGINDLGEN(11,IY,2,2) + &
                           CGCOMMGEN(11,IY,2,1) + CGCOMMGEN(11,IY,2,2)) * .001
         T137(29,IY,IS) = (CGREFGEN(11,IY,3,1)  + CGREFGEN(11,IY,3,2) + &
                           CGOGSGEN(11,IY,3,1)  + CGOGSGEN(11,IY,3,2) + &
                           CGINDLGEN(11,IY,3,1) + CGINDLGEN(11,IY,3,2) + &
                           CGCOMMGEN(11,IY,3,1) + CGCOMMGEN(11,IY,3,2) + &
                           CGRESGEN(11,IY,3,1)  + CGRESGEN(11,IY,3,2)) * .001
         T137(30,IY,IS) = FSUM(T137(27,IY,IS),3)
!        --- COGENERATOR GENERATION
         T137(31,IY,IS) =(CGCOMMGEN(11,IY,6,1) + CGCOMMGEN(11,IY,6,2) + &
                          CGREFGEN(11,IY,6,1)  + CGREFGEN(11,IY,6,2) + &
                          CGINDLGEN(11,IY,6,1) + CGINDLGEN(11,IY,6,2)) * .001
         T137(32,IY,IS) =(CGCOMMGEN(11,IY,7,1) + CGCOMMGEN(11,IY,7,2) + &
                          CGREFGEN(11,IY,7,1)  + CGREFGEN(11,IY,7,2) + &
                          CGINDLGEN(11,IY,7,1) + CGINDLGEN(11,IY,7,2)) * .001
         T137(33,IY,IS) = FSUM(T137(31,IY,IS),2)
!        --- Other Generation
         T137(34,IY,IS) =(CGCOMMGEN(11,IY,4,1) + CGCOMMGEN(11,IY,4,2) + &
                          CGINDLGEN(11,IY,4,1) + CGINDLGEN(11,IY,4,2)) * .001
         T137(35,IY,IS) =(CGCOMMGEN(11,IY,5,1) + CGCOMMGEN(11,IY,5,2) + &
                          CGINDLGEN(11,IY,5,1) + CGINDLGEN(11,IY,5,2)) * .001
         T137(36,IY,IS) =(CGCOMMGEN(11,IY,8,1) + CGCOMMGEN(11,IY,8,2) + &
                          CGRESGEN(11,IY,8,1)  + CGRESGEN(11,IY,8,2) + &
                          CGINDLGEN(11,IY,8,1) + CGINDLGEN(11,IY,8,2)) * .001
         T137(50,IY,IS) =(CGCOMMGEN(11,IY,11,1)+ CGCOMMGEN(11,IY,11,2) + &
                          CGRESGEN(11,IY,11,1) + CGRESGEN(11,IY,11,2) + &
                          CGINDLGEN(11,IY,11,1)+ CGINDLGEN(11,IY,11,2)) * .001
         T137(42,IY,IS) = FSUM(T137(33,IY,IS),4) + T137(50,IY,IS)
!       --- Ethanol Components
!        --- Carbon Emissions by Electric Generators
         T137(37,IY,IS) = EMEL(2,1,IY)
         T137(38,IY,IS) = EMEL(1,1,IY)
         T137(39,IY,IS) = EMEL(3,1,IY)
         T137(48,IY,IS) = EMEL(4,1,IY)
         T137(40,IY,IS) = SUM(EMEL(1:4,1,IY))
       ENDDO


! Table 138 - Key Results for Electric Generation Sector Technology Cases

!      --- Electic Generator Capability
       DO IY=1,LASTYR
         T138( 1,IY,IS) = UCAPCSU(mnumnr,IY) + UCAPCSN(mnumnr,IY) + UCAPCSC(mnumnr,IY) - &
                          UCAPIGU(mnumnr,IY) - UCAPIGN(mnumnr,IY) - UCAPIGC(mnumnr,IY) - &
                          UCAPISU(mnumnr,IY) - UCAPISN(mnumnr,IY)
         T138( 2,IY,IS) = UCAPIGU(mnumnr,IY) + UCAPIGN(mnumnr,IY) + UCAPIGC(mnumnr,IY) + &
                          UCAPISU(mnumnr,IY) + UCAPISN(mnumnr,IY)
         T138( 3,IY,IS) = UCAPCCU(mnumnr,IY) + UCAPCCN(mnumnr,IY) + UCAPCCC(mnumnr,IY) - &
                          UCAPACU(mnumnr,IY) - UCAPACN(mnumnr,IY) - UCAPACC(mnumnr,IY) - &
                          UCAPASU(mnumnr,IY) - UCAPASN(mnumnr,IY)
         T138( 4,IY,IS) = UCAPACU(mnumnr,IY) + UCAPACN(mnumnr,IY) + UCAPACC(mnumnr,IY) + &
                          UCAPASU(mnumnr,IY) + UCAPASN(mnumnr,IY)
         T138( 5,IY,IS) = UCAPCTU(mnumnr,IY) + UCAPCTN(mnumnr,IY) + UCAPCTC(mnumnr,IY) - &
                          UCAPATU(mnumnr,IY) - UCAPATN(mnumnr,IY) - UCAPATC(mnumnr,IY)
         T138( 6,IY,IS) = UCAPATU(mnumnr,IY) + UCAPATN(mnumnr,IY) + UCAPATC(mnumnr,IY)
         T138( 7,IY,IS) = UCAPFCU(mnumnr,IY) + UCAPFCN(mnumnr,IY)
         T138( 8,IY,IS) = UCAPNUU(mnumnr,IY) + UCAPNUN(mnumnr,IY) + UCAPSMU(mnumnr,IY) + UCAPSMN(mnumnr,IY)
         T138( 9,IY,IS) = UCAPOSU(mnumnr,IY) + UCAPOSN(mnumnr,IY) + UCAPOSC(mnumnr,IY) + &
                          UCAPNGU(mnumnr,IY) + UCAPNGN(mnumnr,IY)
         T138(10,IY,IS) = UCAPPSU(mnumnr,IY) + UCAPPSN(mnumnr,IY) + &
                          UCAPHYU(mnumnr,IY) + UCAPGEU(mnumnr,IY) + &
                          UCAPMSU(mnumnr,IY) + UCAPWDU(mnumnr,IY) + &
                          UCAPSTU(mnumnr,IY) + UCAPPVU(mnumnr,IY) + UCAPWNU(mnumnr,IY) + &
                          UCAPWLU(mnumnr,IY) + UCAPWFU(mnumnr,IY) + UCAPPTU(mnumnr,IY) + &
                          UCAPHYN(mnumnr,IY) + UCAPGEN(mnumnr,IY) + &
                          UCAPMSN(mnumnr,IY) + UCAPWDN(mnumnr,IY) + &
                          UCAPSTN(mnumnr,IY) + UCAPPVN(mnumnr,IY) + UCAPWNN(mnumnr,IY) + &
                          UCAPWFN(mnumnr,IY) + UCAPWLN(mnumnr,IY) + UCAPPTN(mnumnr,IY) + &
                          UCAPHYC(mnumnr,IY) + UCAPGEC(mnumnr,IY) + &
                          UCAPMSC(mnumnr,IY) + UCAPWDC(mnumnr,IY) + &
                          UCAPSTC(mnumnr,IY) + UCAPPVC(mnumnr,IY) + UCAPPTC(mnumnr,IY) + &
                          UCAPWNC(mnumnr,IY) + UCAPWLC(mnumnr,IY)
         T138(11,IY,IS) = UCAPDBU(mnumnr,IY) + UCAPDBN(mnumnr,IY) + UCAPDPU(mnumnr,IY) + UCAPDPN(mnumnr,IY)
         T138(12,IY,IS) = T9(44,IY,IS)
         T138(13,IY,IS) = FSUM(T138(1,IY,IS),12)
!        --- Electric Generators - Additions, Planned + Unplanned
         T138(14,IY,IS) = UADDCSU(1,mnumnr,IY) + UADDCSN(1,mnumnr,IY) - &
                          UADDIGU(1,mnumnr,IY) - UADDIGN(1,mnumnr,IY) + &
                          UADDCSU(2,mnumnr,IY) + UADDCSN(2,mnumnr,IY) - &
                          UADDIGU(2,mnumnr,IY) - UADDIGN(2,mnumnr,IY) + UADDCSC(mnumnr,IY)
         T138(15,IY,IS) = UADDIGU(1,mnumnr,IY) + UADDIGN(1,mnumnr,IY) + &
                          UADDIGU(2,mnumnr,IY) + UADDIGN(2,mnumnr,IY) + UADDIGC(mnumnr,IY)
         T138(16,IY,IS) = UADDCCU(1,mnumnr,IY) + UADDCCN(1,mnumnr,IY) - &
                          UADDACU(1,mnumnr,IY) - UADDACN(1,mnumnr,IY) + &
                          UADDCCU(2,mnumnr,IY) + UADDCCN(2,mnumnr,IY) - &
                          UADDACU(2,mnumnr,IY) - UADDACN(2,mnumnr,IY) + UADDCCC(mnumnr,IY)
         T138(17,IY,IS) = UADDACU(1,mnumnr,IY) + UADDACN(1,mnumnr,IY) + &
                          UADDACU(2,mnumnr,IY) + UADDACN(2,mnumnr,IY) + UADDACC(mnumnr,IY)
         T138(18,IY,IS) = UADDCTU(1,mnumnr,IY) + UADDCTN(1,mnumnr,IY) - &
                          UADDATU(1,mnumnr,IY) - UADDATN(1,mnumnr,IY) + &
                          UADDCTU(2,mnumnr,IY) + UADDCTN(2,mnumnr,IY) - &
                          UADDATU(2,mnumnr,IY) - UADDATN(2,mnumnr,IY) + UADDCTC(mnumnr,IY)
         T138(19,IY,IS) = UADDATU(1,mnumnr,IY) + UADDATN(1,mnumnr,IY) + &
                          UADDATU(2,mnumnr,IY) + UADDATN(2,mnumnr,IY) + UADDATC(mnumnr,IY)
         T138(20,IY,IS) = UADDFCU(1,mnumnr,IY) + UADDFCN(1,mnumnr,IY) + &
                          UADDFCU(2,mnumnr,IY) + UADDFCN(2,mnumnr,IY)
         T138(21,IY,IS) = UADDNUU(1,mnumnr,IY) + UADDNUN(1,mnumnr,IY) + &
                          UADDNUU(2,mnumnr,IY) + UADDNUN(2,mnumnr,IY) + &
                          UADDSMU(1,mnumnr,IY) + UADDSMN(1,mnumnr,IY) + &
                          UADDSMU(2,mnumnr,IY) + UADDSMN(2,mnumnr,IY)
         T138(22,IY,IS) = UADDOSU(1,mnumnr,IY) + UADDOSN(1,mnumnr,IY) + &
                          UADDOSU(2,mnumnr,IY) + UADDOSN(2,mnumnr,IY) + UADDOSC(mnumnr,IY)
         T138(23,IY,IS) = UADDPSU(1,mnumnr,IY) + UADDPSN(1,mnumnr,IY) + &
                          UADDRNU(1,mnumnr,IY) + UADDRNN(1,mnumnr,IY) + &
                          UADDPSU(2,mnumnr,IY) + UADDPSN(2,mnumnr,IY) + &
                          UADDRNU(2,mnumnr,IY) + UADDRNN(2,mnumnr,IY) + &
                          UADDHYC(mnumnr,IY) + UADDGEC(mnumnr,IY) + UADDMSC(mnumnr,IY) + UADDWDC(mnumnr,IY) + &
                          UADDSTC(mnumnr,IY) + UADDPVC(mnumnr,IY) + UADDWNC(mnumnr,IY) + UADDWFC(mnumnr,IY) + &
                          UADDWLC(mnumnr,IY) + UADDPTC(mnumnr,IY)
         T138(24,IY,IS) = UADDDBU(1,mnumnr,IY) + UADDDBN(1,mnumnr,IY) + &
                          UADDDPU(1,mnumnr,IY) + UADDDPN(1,mnumnr,IY) + &
                          UADDDBU(2,mnumnr,IY) + UADDDBN(2,mnumnr,IY) + &
                          UADDDPU(2,mnumnr,IY) + UADDDPN(2,mnumnr,IY)
         T138(25,IY,IS) = T9(45,IY,IS)
         T138(26,IY,IS) = FSUM(T138(14,IY,IS),12)
!        --- Retirements
         T138(27,IY,IS) = URETTLU(mnumnr,IY)
!  Generation
         T138(28,IY,IS) = T8( 1,IY,IS) + T8(42,IY,IS)
         T138(29,IY,IS) = T8( 2,IY,IS) + T8(43,IY,IS)
         T138(30,IY,IS) = T8( 3,IY,IS) + T8(44,IY,IS)
         T138(31,IY,IS) = T8( 4,IY,IS)
         T138(32,IY,IS) = T8( 5,IY,IS) + T8(6,IY,IS) + T8(46,IY,IS)
         T138(33,IY,IS) = T8(41,IY,IS)
         T138(34,IY,IS) = T8(15,IY,IS)
         T138(35,IY,IS) = FSUM(T138(28,IY,IS),7)
!  Consumption
         T138(36,IY,IS) = QCLEL(11,IY)
         T138(37,IY,IS) = QDSEL(11,IY) + QRSEL(11,IY) + QPCEL(11,IY)
         T138(38,IY,IS) = QNGEL(11,IY)
         T138(39,IY,IS) = QUREL(11,IY)
! subtract nonbiogenic from msw but add to summed total line
         T138(40,IY,IS) = QTREL(11,IY) - WNCMSEL(IY,11)
         T138(41,IY,IS) = FSUM(T138(36,IY,IS),5) + WNCMSEL(IY,11)
!         --- Carbon Emissions by Electric Generators
         T138(42,IY,IS) = T17(20,11,IY,IS)
         T138(43,IY,IS) = T17(21,11,IY,IS)
         T138(44,IY,IS) = T17(22,11,IY,IS)
         T138(45,IY,IS) = T17(23,11,IY,IS)
         T138(46,IY,IS) = FSUM(T138(42,IY,IS),4)
       ENDDO

! Table 141 - blank - T141(
!     Table 141. Summary of the U.S. Economy

      DO IY = 1,LASTYR
         IF (IY .GT. 1) T141( 1,IY,IS) = ((MC_GDPR(IY)/MC_GDPR(IY-1))-1)*100
         IF (IY .GT. 1) T141( 2,IY,IS) = ((MC_SFDPRODR(IY)/MC_SFDPRODR(IY-1))-1)*100
         IF (IY .GT. 1) T141( 3,IY,IS) = ((MC_CONSR(IY)/MC_CONSR(IY-1))-1)*100
         IF (IY .GT. 1) T141( 4,IY,IS) = ((MC_CDR(IY)/MC_CDR(IY-1))-1)*100
         IF (IY .GT. 1) T141( 5,IY,IS) = ((MC_CNR(IY)/MC_CNR(IY-1))-1)*100
         IF (IY .GT. 1) T141( 6,IY,IS) = ((MC_CSVR(IY)/MC_CSVR(IY-1))-1)*100
         IF (IY .GT. 1) T141( 7,IY,IS) = ((MC_IFNRER(IY)/MC_IFNRER(IY-1))-1)*100
         IF (IY .GT. 1) T141( 8,IY,IS) = ((MC_IFNREER(IY)/MC_IFNREER(IY-1))-1)*100
         IF (IY .GT. 1) T141( 9,IY,IS) = ((MC_IFNREEIPR(IY)/MC_IFNREEIPR(IY-1))-1)*100
         IF (IY .GT. 1) T141(10,IY,IS) = ((MC_IFNREEIPCCR(IY)/MC_IFNREEIPCCR(IY-1))-1)*100
         IF (IY .GT. 1) T141(11,IY,IS) = ((MC_IFNREEIPCTR(IY)/MC_IFNREEIPCTR(IY-1))-1)*100
         IF (IY .GT. 1) T141(12,IY,IS) = ((MC_IFNREEINDR(IY)/MC_IFNREEINDR(IY-1))-1)*100
         IF (IY .GT. 1) T141(13,IY,IS) = ((MC_IFNREETR(IY)/MC_IFNREETR(IY-1))-1)*100
         IF (IY .GT. 1) T141(14,IY,IS) = ((MC_IFNREETACR(IY)/MC_IFNREETACR(IY-1))-1)*100
         IF (IY .GT. 1) T141(15,IY,IS) = ((MC_IFNREEOR(IY)/MC_IFNREEOR(IY-1))-1)*100
         IF (IY .GT. 1) T141(16,IY,IS) = ((MC_IFNRESR(IY)/MC_IFNRESR(IY-1))-1)*100
         IF (IY .GT. 1) T141(17,IY,IS) = ((MC_IFNRESCR(IY)/MC_IFNRESCR(IY-1))-1)*100
         IF (IY .GT. 1) T141(18,IY,IS) = ((MC_IFNRESMFGR(IY)/MC_IFNRESMFGR(IY-1))-1)*100
         IF (IY .GT. 1) T141(19,IY,IS) = ((MC_IFNRESPR(IY)/MC_IFNRESPR(IY-1))-1)*100
         IF (IY .GT. 1) T141(20,IY,IS) = ((MC_IFNRESMIR(IY)/MC_IFNRESMIR(IY-1))-1)*100
         IF (IY .GT. 1) T141(21,IY,IS) = ((MC_IFNRESOR(IY)/MC_IFNRESOR(IY-1))-1)*100
         IF (IY .GT. 1) T141(22,IY,IS) = ((MC_IFRER(IY)/MC_IFRER(IY-1))-1)*100
         IF (IY .GT. 1) T141(23,IY,IS) = ((MC_XR(IY)/MC_XR(IY-1))-1)*100
         IF (IY .GT. 1) T141(24,IY,IS) = ((MC_MR(IY)/MC_MR(IY-1))-1)*100
         IF (IY .GT. 1) T141(25,IY,IS) = ((MC_GFR(IY)/MC_GFR(IY-1))-1)*100
         IF (IY .GT. 1) T141(26,IY,IS) = ((MC_GSLR(IY)/MC_GSLR(IY-1))-1)*100
         T141(27,IY,IS) = MC_GDPR(IY)
         T141(28,IY,IS) = MC_GDP(IY)
         IF (IY .GT. 1) T141(29,IY,IS) = ((MC_JPGDP(IY)/MC_JPGDP(IY-1))-1)*100
         IF (IY .GT. 1) T141(30,IY,IS) = ((MC_CPI(MNUMCR,IY)/MC_CPI(MNUMCR,IY-1))-1)*100
         IF (IY .GT. 1) T141(31,IY,IS) = ((MC_CPIXFAE(IY)/MC_CPIXFAE(IY-1))-1)*100
         IF (IY .GT. 1) T141(32,IY,IS) = ((MC_WPISOP3000(IY)/MC_WPISOP3000(IY-1))-1)*100
         IF (IY .GT. 1) T141(33,IY,IS) = ((MC_JECIWSSP(IY)/MC_JECIWSSP(IY-1))-1)*100
         T141(34,IY,IS) = MC_POILWTI(IY)
         IF (IY .GT. 1) T141(35,IY,IS) = ((MC_JQPCMHNF(IY)/MC_JQPCMHNF(IY-1))-1)*100
         IF (IY .GT. 1) T141(36,IY,IS) = ((MC_IPSB50001(IY)/MC_IPSB50001(IY-1))-1)*100
         T141(37,IY,IS) = MC_UTLB00004(IY)
         T141(38,IY,IS) = MC_IINFR(IY)
         T141(39,IY,IS) = MC_JCSMICH(IY)
         T141(40,IY,IS) = MC_SUVLV(IY)
         T141(41,IY,IS) = MC_HUSPS(IY)
         T141(42,IY,IS) = MC_HUESOLD(IY)
         T141(43,IY,IS) = MC_RUC(IY)
         IF (IY .GT. 1) T141(44,IY,IS) = ((MC_EEA(IY)/MC_EEA(IY-1))-1)*100
         T141(45,IY,IS) = MC_NETSAVGFUNIFY(IY)
         T141(46,IY,IS) = MC_BOPCRNTAC(IY)
         T141(47,IY,IS) = MC_RMFF(IY)
         T141(48,IY,IS) = MC_RMTB3M(IY)
         T141(49,IY,IS) = MC_RMTCM10Y(IY)
         T141(50,IY,IS) = MC_RMMTG30CON(IY)
         T141(51,IY,IS) = MC_SP500(IY)
         IF (IY .GT. 1) T141(52,IY,IS) = ((MC_SP500(IY)/MC_SP500(IY-1))-1)*100
         T141(53,IY,IS) = MC_JEXCHMTP(IY)
         IF (IY .GT. 1) T141(54,IY,IS) = ((MC_JEXCHMTP(IY)/MC_JEXCHMTP(IY-1))-1)*100
         IF (IY .GT. 1) T141(55,IY,IS) = ((MC_YP(MNUMCR,IY)/MC_YP(MNUMCR,IY-1))-1)*100
         IF (IY .GT. 1) T141(56,IY,IS) = ((MC_YPDR(MNUMCR,IY)/MC_YPDR(MNUMCR,IY-1))-1)*100
         T141(57,IY,IS) = MC_SAVPRATE(IY)
         T141(58,IY,IS) = MC_ZA(IY)
         IF (IY .GT. 1) T141(59,IY,IS) = ((MC_ZA(IY)/MC_ZA(IY-1))-1)*100
         IF (IY .GT. 1) T141(60,IY,IS) = ((MC_YPD(IY)/MC_YPD(IY-1))-1)*100
      ENDDO

!     Table 142.  Composition of Gross Domestic Product in real dollars

      DO IY = 1, LASTYR
         T142( 1,IY,IS) = MC_GDPR(IY)
         IF (IY .GT. 1) T142( 2,IY,IS) = ((MC_GDPR(IY)/MC_GDPR(IY-1))-1)*100
         T142( 3,IY,IS) = MC_CONSR(IY)
         T142( 4,IY,IS) = MC_CGOODSR(IY)
         T142( 5,IY,IS) = MC_CDR(IY)
         T142( 6,IY,IS) = MC_CDMVR(IY)
         T142( 7,IY,IS) = MC_CDFHER(IY)
         T142( 8,IY,IS) = MC_CDRECR(IY)
         T142( 9,IY,IS) = MC_CDOR(IY)
         T142(10,IY,IS) = MC_CNR(IY)
         T142(11,IY,IS) = MC_CNFR(IY)
         T142(12,IY,IS) = MC_CNCSR(IY)
         T142(13,IY,IS) = MC_CNEGAOR(IY)
         T142(14,IY,IS) = MC_CNEFAOR(IY)
         T142(15,IY,IS) = MC_CNOPMPR(IY)
         T142(16,IY,IS) = MC_CNOTOBR(IY)
         T142(17,IY,IS) = MC_CNOOR(IY)
         T142(18,IY,IS) = MC_CSVR(IY)
         T142(19,IY,IS) = MC_CSVHHR(IY)
         T142(20,IY,IS) = MC_CSVHR(IY)
         T142(21,IY,IS) = MC_CSVUR(IY)
         T142(22,IY,IS) = MC_CSVHCR(IY)
         T142(23,IY,IS) = MC_CSVTSR(IY)
         T142(24,IY,IS) = MC_CSVRECR(IY)
         T142(25,IY,IS) = MC_CSVFR(IY)
         T142(26,IY,IS) = MC_CSVACR(IY)
         T142(27,IY,IS) = MC_CSVFINR(IY)
         T142(28,IY,IS) = MC_CSVINSR(IY)
         T142(29,IY,IS) = MC_CSVOR(IY)
         T142(30,IY,IS) = MC_CSVNPISHR(IY)
         T142(31,IY,IS) = MC_IR(IY)
         T142(32,IY,IS) = MC_IFNRER(IY)
         T142(33,IY,IS) = MC_IFNREER(IY)
         T142(34,IY,IS) = MC_IFNREEIPR(IY)
         T142(35,IY,IS) = MC_IFNREEINDR(IY)
         T142(36,IY,IS) = MC_IFNREETLVR(IY)
         T142(37,IY,IS) = MC_IFNREETACR(IY)
         T142(38,IY,IS) = MC_IFNREETOR(IY)
         T142(39,IY,IS) = MC_IFNREEOR(IY)
         T142(40,IY,IS) = MC_IFNRESR(IY)
         T142(41,IY,IS) = MC_IFNRESCR(IY)
         T142(42,IY,IS) = MC_IFNRESMFGR(IY)
         T142(43,IY,IS) = MC_IFNRESPR(IY)
         T142(44,IY,IS) = MC_IFNRESPPR(IY)
         T142(45,IY,IS) = MC_IFNRESPCR(IY)
         T142(46,IY,IS) = MC_IFNRESMIR(IY)
         T142(47,IY,IS) = MC_IFNRESOR(IY)
         T142(48,IY,IS) = MC_IFRER(IY)
         T142(49,IY,IS) = MC_IFRESR(IY)
         T142(50,IY,IS) = MC_IFREER(IY)
         T142(51,IY,IS) = MC_IIR(IY)
         T142(52,IY,IS) = MC_IINFR(IY)
         T142(53,IY,IS) = MC_IIMR(IY)
         T142(54,IY,IS) = MC_IIWR(IY)
         T142(55,IY,IS) = MC_IIRTR(IY)
         T142(56,IY,IS) = MC_IICMIUR(IY)
         T142(57,IY,IS) = MC_IIFR(IY)
         T142(58,IY,IS) = MC_XR(IY)
         T142(59,IY,IS) = MC_XGR(IY)
         T142(60,IY,IS) = MC_XSVTOTR(IY)
         T142(61,IY,IS) = MC_MR(IY)
         T142(62,IY,IS) = MC_MGR(IY)
         T142(63,IY,IS) = MC_MSVTOTR(IY)
         T142(64,IY,IS) = MC_GR(IY)
         T142(65,IY,IS) = MC_GFR(IY)
         T142(66,IY,IS) = MC_GFMLR(IY)
         T142(67,IY,IS) = MC_GFOR(IY)
         T142(68,IY,IS) = MC_GSLR(IY)
         T142(69,IY,IS) = MC_XGFFBR(IY)
         T142(70,IY,IS) = MC_XGINR(IY)
         T142(71,IY,IS) = MC_XGCR(IY)
         T142(72,IY,IS) = MC_XGKR(IY)
         T142(73,IY,IS) = MC_XGAUTOR(IY)
         T142(74,IY,IS) = MC_MGFFBR(IY)
         T142(75,IY,IS) = MC_MGINAPETR(IY)
         T142(76,IY,IS) = MC_MGCR(IY)
         T142(77,IY,IS) = MC_MGKR(IY)
         T142(78,IY,IS) = MC_MGAUTOR(IY)
      END DO

!     Table 143.  Composition of Gross Domestic Product in nominal dollars

      DO IY = 1, LASTYR
         T143( 1,IY,IS) = MC_GDP(IY)
         IF (IY .GT. 1) T143( 2,IY,IS) = ((MC_GDP(IY)/MC_GDP(IY-1))-1)*100
         T143( 3,IY,IS) = MC_CONS(IY)
         T143( 4,IY,IS) = MC_CGOODS(IY)
         T143( 5,IY,IS) = MC_CD(IY)
         T143( 6,IY,IS) = MC_CDMV(IY)
         T143( 7,IY,IS) = MC_CDFHE(IY)
         T143( 8,IY,IS) = MC_CDREC(IY)
         T143( 9,IY,IS) = MC_CDO(IY)
         T143(10,IY,IS) = MC_CN(IY)
         T143(11,IY,IS) = MC_CNF(IY)
         T143(12,IY,IS) = MC_CNCS(IY)
         T143(13,IY,IS) = MC_CNEGAO(IY)
         T143(14,IY,IS) = MC_CNEFAO(IY)
         T143(15,IY,IS) = MC_CNOPMP(IY)
         T143(16,IY,IS) = MC_CNOTOB(IY)
         T143(17,IY,IS) = MC_CNOO(IY)
         T143(18,IY,IS) = MC_CSV(IY)
         T143(19,IY,IS) = MC_CSVHH(IY)
         T143(20,IY,IS) = MC_CSVH(IY)
         T143(21,IY,IS) = MC_CSVU(IY)
         T143(22,IY,IS) = MC_CSVHC(IY)
         T143(23,IY,IS) = MC_CSVTS(IY)
         T143(24,IY,IS) = MC_CSVREC(IY)
         T143(25,IY,IS) = MC_CSVF(IY)
         T143(26,IY,IS) = MC_CSVAC(IY)
         T143(27,IY,IS) = MC_CSVFIN(IY)
         T143(28,IY,IS) = MC_CSVINS(IY)
         T143(29,IY,IS) = MC_CSVO(IY)
         T143(30,IY,IS) = MC_CSVNPISH(IY)
         T143(31,IY,IS) = MC_I(IY)
         T143(32,IY,IS) = MC_IFNRE(IY)
         T143(33,IY,IS) = MC_IFNREE(IY)
         T143(34,IY,IS) = MC_IFNREEIP(IY)
         T143(35,IY,IS) = MC_IFNREEIND(IY)
         T143(36,IY,IS) = MC_IFNREETLV(IY)
         T143(37,IY,IS) = MC_IFNREETAC(IY)
         T143(38,IY,IS) = MC_IFNREETO(IY)
         T143(39,IY,IS) = MC_IFNREEO(IY)
         T143(40,IY,IS) = MC_IFNRES(IY)
         T143(41,IY,IS) = MC_IFNRESC(IY)
         T143(42,IY,IS) = MC_IFNRESMFG(IY)
         T143(43,IY,IS) = MC_IFNRESP(IY)
         T143(44,IY,IS) = MC_IFNRESPP(IY)
         T143(45,IY,IS) = MC_IFNRESPC(IY)
         T143(46,IY,IS) = MC_IFNRESMI(IY)
         T143(47,IY,IS) = MC_IFNRESO(IY)
         T143(48,IY,IS) = MC_IFRE(IY)
         T143(49,IY,IS) = MC_IFRES(IY)
         T143(50,IY,IS) = MC_IFREE(IY)
         T143(51,IY,IS) = MC_II(IY)
         T143(52,IY,IS) = MC_IINF(IY)
         T143(53,IY,IS) = MC_IIM(IY)
         T143(54,IY,IS) = MC_IIW(IY)
         T143(55,IY,IS) = MC_IIRT(IY)
         T143(56,IY,IS) = MC_IICMIU(IY)
         T143(57,IY,IS) = MC_IIF(IY)
         T143(58,IY,IS) = MC_X(IY)
         T143(59,IY,IS) = MC_XG(IY)
         T143(60,IY,IS) = MC_XSVTOT(IY)
         T143(61,IY,IS) = MC_M(IY)
         T143(62,IY,IS) = MC_MG(IY)
         T143(63,IY,IS) = MC_MSVTOT(IY)
         T143(64,IY,IS) = MC_G(IY)
         T143(65,IY,IS) = MC_GF(IY)
         T143(66,IY,IS) = MC_GFML(IY)
         T143(67,IY,IS) = MC_GFO(IY)
         T143(68,IY,IS) = MC_GSL(IY)
      END DO

!     Table 144.  Distribution of GDP, GNP, and National Income

      DO IY = 1, LASTYR
         T144( 1,IY,IS) = MC_GDP(IY)
         T144( 2,IY,IS) = MC_XFY(IY)
         T144( 3,IY,IS) = MC_MFY(IY)
         T144( 4,IY,IS) = MC_GNP(IY)
         T144( 5,IY,IS) = MC_CKF(IY)
         T144( 6,IY,IS) = MC_NNP(IY)
         T144( 7,IY,IS) = MC_STAT(IY)
         T144( 8,IY,IS) = MC_YN(IY)
         T144( 9,IY,IS) = MC_YPCOMP(IY)
         T144(10,IY,IS) = MC_YPPROPADJNF(IY)
         T144(11,IY,IS) = MC_YPPROPADJF(IY)
         T144(12,IY,IS) = MC_YPRENTADJ(IY)
         T144(13,IY,IS) = MC_INTNETBUS(IY)
         T144(14,IY,IS) = MC_ZBECON(IY)
         T144(15,IY,IS) = MC_TXIM(IY)
         T144(16,IY,IS) = MC_TRFBUS(IY)
         T144(17,IY,IS) = MC_SUBLSURPGF(IY)+MC_SUBLSURPGSL(IY)
         T144(18,IY,IS) = 100*MC_YPCOMP(IY)/MC_YN(IY)
         T144(19,IY,IS) = 100*MC_YPPROPADJNF(IY)/MC_YN(IY)
         T144(20,IY,IS) = 100*MC_YPPROPADJF(IY)/MC_YN(IY)
         T144(21,IY,IS) = 100*MC_YPRENTADJ(IY)/MC_YN(IY)
         T144(22,IY,IS) = 100*MC_INTNETBUS(IY)/MC_YN(IY)
         T144(23,IY,IS) = 100*MC_ZBECON(IY)/MC_YN(IY)
         T144(24,IY,IS) = 100*MC_TXIM(IY)/MC_YN(IY)
         T144(25,IY,IS) = 100*MC_TRFBUS(IY)/MC_YN(IY)
         T144(26,IY,IS) = 100*(MC_SUBLSURPGF(IY)+MC_SUBLSURPGSL(IY))/MC_YN(IY)
         T144(27,IY,IS) = MC_ZBECON(IY)
         IF (IY .GT. 1) T144(28,IY,IS) = ((MC_ZBECON(IY)/MC_ZBECON(IY-1))-1)*100
         T144(29,IY,IS) = MC_CKFADJCORP(IY)
         T144(30,IY,IS) = MC_IVACORP(IY)
         T144(31,IY,IS) = MC_ZB(IY)
         IF (IY .GT. 1) T144(32,IY,IS) = ((MC_ZB(IY)/MC_ZB(IY-1))-1)*100
         T144(33,IY,IS) = MC_TXCORP(IY)
         T144(34,IY,IS) = MC_TXCORPGF(IY)
         T144(35,IY,IS) = MC_TXCORPGSL(IY)
         T144(36,IY,IS) = MC_TXCORPRW(IY)
         T144(37,IY,IS) = MC_ZA(IY)
         T144(38,IY,IS) = MC_ZADIV(IY)
         T144(39,IY,IS) = MC_ZARE(IY)
         T144(40,IY,IS) = MC_ZBECON(IY)
         T144(41,IY,IS) = MC_ZBECON(IY)-MC_ZBIVARW(IY)-MC_ZBIVADFIN521(IY)
         T144(42,IY,IS) = MC_ZBIVARW(IY)
         T144(43,IY,IS) = MC_ZBIVADFIN521(IY)
         T144(44,IY,IS) = MC_ZAR(IY)
         T144(45,IY,IS) = 100*MC_ZADIV(IY)/MC_ZA(IY)
         T144(46,IY,IS) = MC_CKFCORPBK(IY)
         T144(47,IY,IS) = MC_CKFCORP(IY)
         T144(48,IY,IS) = MC_CKFADJCORP(IY)
         T144(49,IY,IS) = MC_ZA(IY)
         T144(50,IY,IS) = MC_CKFCORPBK(IY)
         T144(51,IY,IS) = MC_IVACORP(IY)
         T144(52,IY,IS) = MC_ZADIV(IY)
         T144(53,IY,IS) = MC_NETCAPTRF(IY)
         T144(54,IY,IS) = MC_NETCFIVA(IY)
      END DO

! Table 145 - Detailed GDP Breakout and Some Other Macro Stuff

      DO IY=1,LASTYR
         T145(1:102,IY,IS) = MC_DETAIL(1:102,IY)
      ENDDO

! Table 146 - Freight Technology Penetration

      DO IY=1,LASTYR
! Class 3 Pickup, van, and vocational
         techlist(1:18)=[ 1, 5, 8,11,12,14,15,16,17,18,19,20,21,22,23,25,36,37]   ! Diesel
         T146(  1: 18,IY,IS) = TECHSHARE(IY,techlist(1:18),1,1) * 100.
         techlist(1:17)=[ 1, 5, 8,11,12,14,15,16,29,30,31,32,33,34,35,36,37]      ! Gasoline
         T146( 19: 35,IY,IS) = TECHSHARE(IY,techlist(1:17),1,2) * 100.
! Class 4-6 Vocational
         techlist(1:12)=[ 5, 9,10,16,19,20,21,22,23,27,36,37]             ! Diesel
         T146( 36: 47,IY,IS) = TECHSHARE(IY,techlist(1:12),2,1) * 100.
         techlist(1:12)=[ 5, 9,10,16,30,31,32,33,34,35,36,37]             ! Gasoline
         T146( 48: 59,IY,IS) = TECHSHARE(IY,techlist(1:12),2,2) * 100.
! Classes 7-8 Vocational
         techlist(1:12)=[ 5, 9,10,16,19,20,21,22,23,27,28,37]             ! Diesel
         T146( 60: 71,IY,IS) = TECHSHARE(IY,techlist(1:12),3,1) * 100.
         techlist(1:12)=[ 5, 9,10,16,30,31,32,33,34,35,36,37]             ! Gasoline
         T146( 72: 83,IY,IS) = TECHSHARE(IY,techlist(1:12),3,2) * 100.
! Classes 7-8 Day
         techlist(1:21)=[ 1, 2, 3, 4, 5, 6, 7, 8, 9,10,16,19,20,21,22,23,24,26,27,28,37]    ! Diesel
         T146( 84:104,IY,IS) = TECHSHARE(IY,techlist(1:21),4,1) * 100.
!do not show gasoline for groups 4 and 5
!        techlist(1:12)=[ 1, 2, 3, 4, 5, 6, 7, 8, 9,10,16,37]                               ! Gasoline
!        T146(105:116,IY,IS) = TECHSHARE(IY,techlist(1:12),4,2) * 100.
! Classes 7-8 Sleeper
         techlist(1:22)=[ 1, 2, 3, 4, 5, 6, 7, 8, 9,10,13,16,19,20,21,22,23,24,26,27,28,37] ! Diesel
         T146(117:138,IY,IS) = TECHSHARE(IY,techlist(1:22),5,1) * 100.
!        techlist(1:13)=[ 1, 2, 3, 4, 5, 6, 7, 8, 9,10,13,16,37]                            ! Gasoline
!        T146(139:151,IY,IS) = TECHSHARE(IY,techlist(1:13),5,2) * 100.
      ENDDO

! Table 147 - NEMS Transportation Sector Criteria Emissions Table

      DO IY=1,LASTYR
!        --- HC Emissions
         T147( 1:12,IY,IS)=TOT_EMIS(IY,1,1:12)
         T147(13,IY,IS)=FSUM(T147( 1,IY,IS),12) ! Tot HC emissions
!        --- CO EMISSIONS
         T147(14:25,IY,IS)=TOT_EMIS(IY,2,1:12)
         T147(26,IY,IS)=FSUM(T147(14,IY,IS),12) ! Tot CO emissions
!        --- NOx EMISSIONS
         T147(27:38,IY,IS)=TOT_EMIS(IY,3,1:12)
         T147(39,IY,IS)=FSUM(T147(27,IY,IS),12) ! Tot NOx emissions
         T147(40,IY,IS) = T147(13,IY,IS) + T147(26,IY,IS) + T147(39,IY,IS)  ! Total
      ENDDO

! Table 148 Aircraft Stock			! MDRAEO2023

      DO IY = 1, LASTYR
        T148(1:221,IY,IS ) = AIROUT(194:414,IY)
      ENDDO

! Table 149 - prices for FEMP at the Census region level suitable for extrapolating

      DO IY=1,LASTYR
         T149( 1,IY,IS) = BRENT_PRICE(IY)
         T149( 2,IY,IS) = OGHHPRNG(IY) * CFNGC(IY)    ! OGWPRNG(13,IY)
         T149( 3,IY,IS) = CPSB(3,IY)*CPSBT(3,IY)
         T149(89,IY,IS) = CFCRDLTSWT(IY)
         T149(90,IY,IS) = BRENT_PRICE(IY) / T149(89,IY,IS)
         T149(91,IY,IS) = CFNGC(IY)
         T149(92,IY,IS) = OGHHPRNG(IY)                ! OGWPRNG(13,IY) / CFNGC(IY)
         T149(93,IY,IS) = CQSBT(3,IY)
         T149(94,IY,IS) = CPSB(3,IY)
!  Residential
         T149( 4,IY,IS) =(PDSRS(1,IY)*QDSRS(1,IY)+PDSRS(2,IY)*QDSRS(2,IY))/(QDSRS(1,IY)+QDSRS(2,IY))
         T149( 5,IY,IS) =(PPRRS(1,IY)*QPRRS(1,IY)+PPRRS(2,IY)*QPRRS(2,IY))/(QPRRS(1,IY)+QPRRS(2,IY))
         T149( 6,IY,IS) =(PNGRS(1,IY)*QNGRS(1,IY)+PNGRS(2,IY)*QNGRS(2,IY))/(QNGRS(1,IY)+QNGRS(2,IY))
         T149( 7,IY,IS) =(PELRS(1,IY)*QELRS(1,IY)+PELRS(2,IY)*QELRS(2,IY))/(QELRS(1,IY)+QELRS(2,IY))
         T149( 8,IY,IS) =(PDSRS(3,IY)*QDSRS(3,IY)+PDSRS(4,IY)*QDSRS(4,IY))/(QDSRS(3,IY)+QDSRS(4,IY))
         T149( 9,IY,IS) =(PPRRS(3,IY)*QPRRS(3,IY)+PPRRS(4,IY)*QPRRS(4,IY))/(QPRRS(3,IY)+QPRRS(4,IY))
         T149(10,IY,IS) =(PNGRS(3,IY)*QNGRS(3,IY)+PNGRS(4,IY)*QNGRS(4,IY))/(QNGRS(3,IY)+QNGRS(4,IY))
         T149(11,IY,IS) =(PELRS(3,IY)*QELRS(3,IY)+PELRS(4,IY)*QELRS(4,IY))/(QELRS(3,IY)+QELRS(4,IY))
         T149(12,IY,IS) =(PDSRS(5,IY)*QDSRS(5,IY)+PDSRS(6,IY)*QDSRS(6,IY)+PDSRS(7,IY)*QDSRS(7,IY))/ &
                         (QDSRS(5,IY)+QDSRS(6,IY)+QDSRS(7,IY))
         T149(13,IY,IS) =(PPRRS(5,IY)*QPRRS(5,IY)+PPRRS(6,IY)*QPRRS(6,IY)+PPRRS(7,IY)*QPRRS(7,IY))/ &
                         (QPRRS(5,IY)+QPRRS(6,IY)+QPRRS(7,IY))
         T149(14,IY,IS) =(PNGRS(5,IY)*QNGRS(5,IY)+PNGRS(6,IY)*QNGRS(6,IY)+PNGRS(7,IY)*QNGRS(7,IY))/ &
                         (QNGRS(5,IY)+QNGRS(6,IY)+QNGRS(7,IY))
         T149(15,IY,IS) =(PELRS(5,IY)*QELRS(5,IY)+PELRS(6,IY)*QELRS(6,IY)+PELRS(7,IY)*QELRS(7,IY))/ &
                         (QELRS(5,IY)+QELRS(6,IY)+QELRS(7,IY))
         T149(16,IY,IS) =(PDSRS(8,IY)*QDSRS(8,IY)+PDSRS(9,IY)*QDSRS(9,IY))/(QDSRS(8,IY)+QDSRS(9,IY))
         T149(17,IY,IS) =(PPRRS(8,IY)*QPRRS(8,IY)+PPRRS(9,IY)*QPRRS(9,IY))/(QPRRS(8,IY)+QPRRS(9,IY))
         T149(18,IY,IS) =(PNGRS(8,IY)*QNGRS(8,IY)+PNGRS(9,IY)*QNGRS(9,IY))/(QNGRS(8,IY)+QNGRS(9,IY))
         T149(19,IY,IS) =(PELRS(8,IY)*QELRS(8,IY)+PELRS(9,IY)*QELRS(9,IY))/(QELRS(8,IY)+QELRS(9,IY))
         T149(20,IY,IS) = PDSRS(11,IY)
         T149(21,IY,IS) = PPRRS(11,IY)
         T149(22,IY,IS) = PNGRS(11,IY)
         T149(23,IY,IS) = PELRS(11,IY)
!  Commercial
   !  Northeast
         T149(24,IY,IS) =(PDSCM(1,IY)*QDSCM(1,IY)+PDSCM(2,IY)*QDSCM(2,IY))/(QDSCM(1,IY)+QDSCM(2,IY))
         T149(95,IY,IS) =(PPRCM(1,IY)*QPRCM(1,IY)+PPRCM(2,IY)*QPRCM(2,IY))/(QPRCM(1,IY)+QPRCM(2,IY))
         T149(25,IY,IS) =(PRSCM(1,IY)*QRSCM(1,IY)+PRSCM(2,IY)*QRSCM(2,IY))/(QRSCM(1,IY)+QRSCM(2,IY))
         T149(26,IY,IS) =(PNGCM(1,IY)*QNGCM(1,IY)+PNGCM(2,IY)*QNGCM(2,IY))/(QNGCM(1,IY)+QNGCM(2,IY))
         T149(27,IY,IS) =(PCLCM(1,IY)*QCLCM(1,IY)+PCLCM(2,IY)*QCLCM(2,IY))/(QCLCM(1,IY)+QCLCM(2,IY))
         T149(28,IY,IS) =(PELCM(1,IY)*QELCM(1,IY)+PELCM(2,IY)*QELCM(2,IY))/(QELCM(1,IY)+QELCM(2,IY))
   !  Midwest
         T149(29,IY,IS) =(PDSCM(3,IY)*QDSCM(3,IY)+PDSCM(4,IY)*QDSCM(4,IY))/(QDSCM(3,IY)+QDSCM(4,IY))
         T149(96,IY,IS) =(PPRCM(3,IY)*QPRCM(3,IY)+PPRCM(4,IY)*QPRCM(4,IY))/(QPRCM(3,IY)+QPRCM(4,IY))
         T149(30,IY,IS) =(PRSCM(3,IY)*QRSCM(3,IY)+PRSCM(4,IY)*QRSCM(4,IY))/(QRSCM(3,IY)+QRSCM(4,IY))
         T149(31,IY,IS) =(PNGCM(3,IY)*QNGCM(3,IY)+PNGCM(4,IY)*QNGCM(4,IY))/(QNGCM(3,IY)+QNGCM(4,IY))
         T149(32,IY,IS) =(PCLCM(3,IY)*QCLCM(3,IY)+PCLCM(4,IY)*QCLCM(4,IY))/(QCLCM(3,IY)+QCLCM(4,IY))
         T149(33,IY,IS) =(PELCM(3,IY)*QELCM(3,IY)+PELCM(4,IY)*QELCM(4,IY))/(QELCM(3,IY)+QELCM(4,IY))
   !  South
         T149(34,IY,IS) =(PDSCM(5,IY)*QDSCM(5,IY)+PDSCM(6,IY)*QDSCM(6,IY)+PDSCM(7,IY)*QDSCM(7,IY))/ &
                         (QDSCM(5,IY)+QDSCM(6,IY)+QDSCM(7,IY))
         T149(97,IY,IS) =(PPRCM(5,IY)*QPRCM(5,IY)+PPRCM(6,IY)*QPRCM(6,IY)+PPRCM(7,IY)*QPRCM(7,IY))/ &
                         (QPRCM(5,IY)+QPRCM(6,IY)+QPRCM(7,IY))
         T149(35,IY,IS) =(PRSCM(5,IY)*QRSCM(5,IY)+PRSCM(6,IY)*QRSCM(6,IY)+PRSCM(7,IY)*QRSCM(7,IY))/ &
                         (QRSCM(5,IY)+QRSCM(6,IY)+QRSCM(7,IY))
         T149(36,IY,IS) =(PNGCM(5,IY)*QNGCM(5,IY)+PNGCM(6,IY)*QNGCM(6,IY)+PNGCM(7,IY)*QNGCM(7,IY))/ &
                         (QNGCM(5,IY)+QNGCM(6,IY)+QNGCM(7,IY))
         T149(37,IY,IS) =(PCLCM(5,IY)*QCLCM(5,IY)+PCLCM(6,IY)*QCLCM(6,IY)+PCLCM(7,IY)*QCLCM(7,IY))/ &
                         (QCLCM(5,IY)+QCLCM(6,IY)+QCLCM(7,IY))
         T149(38,IY,IS) =(PELCM(5,IY)*QELCM(5,IY)+PELCM(6,IY)*QELCM(6,IY)+PELCM(7,IY)*QELCM(7,IY))/ &
                         (QELCM(5,IY)+QELCM(6,IY)+QELCM(7,IY))
   !  West
         T149(39,IY,IS) =(PDSCM(8,IY)*QDSCM(8,IY)+PDSCM(9,IY)*QDSCM(9,IY))/(QDSCM(8,IY)+QDSCM(9,IY))
         T149(98,IY,IS) =(PPRCM(8,IY)*QPRCM(8,IY)+PPRCM(9,IY)*QPRCM(9,IY))/(QPRCM(8,IY)+QPRCM(9,IY))
         T149(40,IY,IS) =(PRSCM(8,IY)*QRSCM(8,IY)+PRSCM(9,IY)*QRSCM(9,IY))/(QRSCM(8,IY)+QRSCM(9,IY))
         T149(41,IY,IS) =(PNGCM(8,IY)*QNGCM(8,IY)+PNGCM(9,IY)*QNGCM(9,IY))/(QNGCM(8,IY)+QNGCM(9,IY))
         T149(42,IY,IS) =(PCLCM(8,IY)*QCLCM(8,IY)+PCLCM(9,IY)*QCLCM(9,IY))/(QCLCM(8,IY)+QCLCM(9,IY))
         T149(43,IY,IS) =(PELCM(8,IY)*QELCM(8,IY)+PELCM(9,IY)*QELCM(9,IY))/(QELCM(8,IY)+QELCM(9,IY))
         T149(44,IY,IS) = PDSCM(11,IY)
         T149(99,IY,IS) = PPRCM(11,IY)
         T149(45,IY,IS) = PRSCM(11,IY)
         T149(46,IY,IS) = PNGCM(11,IY)
         T149(47,IY,IS) = PCLCM(11,IY)
         T149(48,IY,IS) = PELCM(11,IY)
!  Industrial
   !  Northeast
         T149(49,IY,IS) =(PDSIN(1,IY)*(QDSIN(1,IY)-QDSRF(1,IY))+PDSIN(2,IY)*(QDSIN(2,IY)-QDSRF(2,IY)))/(QDSIN(1,IY)+QDSIN(2,IY)-QDSRF(1,IY)-QDSRF(2,IY))
         T149(100,IY,IS) =(PPRIN(1,IY)*(QPRIN(1,IY)-QPRRF(1,IY))+PPRIN(2,IY)*(QPRIN(2,IY)-QPRRF(2,IY)))/(QPRIN(1,IY)+QPRIN(2,IY)-QPRRF(1,IY)-QPRRF(2,IY))
         T149(50,IY,IS) =(PRSIN(1,IY)*(QRSIN(1,IY)-QRSRF(1,IY))+PRSIN(2,IY)*(QRSIN(2,IY)-QRSRF(2,IY)))/(QRSIN(1,IY)+QRSIN(2,IY)-QRSRF(1,IY)-QRSRF(2,IY))
         T149(51,IY,IS) =(PNGIN(1,IY)*QNGIN(1,IY)+PNGIN(2,IY)*QNGIN(2,IY))/(QNGIN(1,IY)+QNGIN(2,IY))
         T149(52,IY,IS) =(PCLIN(1,IY)*(QCLIN(1,IY)-QCTLRF(1,IY))+PCLIN(2,IY)*(QCLIN(2,IY)-QCTLRF(2,IY)))/(QCLIN(1,IY)+QCLIN(2,IY)-QCTLRF(1,IY)-QCTLRF(2,IY))
         T149(53,IY,IS) =(PELIN(1,IY)*QELIN(1,IY)+PELIN(2,IY)*QELIN(2,IY))/(QELIN(1,IY)+QELIN(2,IY))
   !  Midwest
         T149(54,IY,IS) =(PDSIN(3,IY)*(QDSIN(3,IY)-QDSRF(3,IY))+PDSIN(4,IY)*(QDSIN(4,IY)-QDSRF(4,IY)))/(QDSIN(3,IY)+QDSIN(4,IY)-QDSRF(3,IY)-QDSRF(4,IY))
         T149(101,IY,IS) =(PPRIN(3,IY)*(QPRIN(3,IY)-QPRRF(3,IY))+PPRIN(4,IY)*(QPRIN(4,IY)-QPRRF(4,IY)))/(QPRIN(3,IY)+QPRIN(4,IY)-QPRRF(3,IY)-QPRRF(4,IY))
         T149(55,IY,IS) =(PRSIN(3,IY)*(QRSIN(3,IY)-QRSRF(3,IY))+PRSIN(4,IY)*(QRSIN(4,IY)-QRSRF(4,IY)))/(QRSIN(3,IY)+QRSIN(4,IY)-QRSRF(3,IY)-QRSRF(4,IY))
         T149(56,IY,IS) =(PNGIN(3,IY)*QNGIN(3,IY)+PNGIN(4,IY)*QNGIN(4,IY))/(QNGIN(3,IY)+QNGIN(4,IY))
         T149(57,IY,IS) =(PCLIN(3,IY)*(QCLIN(3,IY)-QCTLRF(3,IY))+PCLIN(4,IY)*(QCLIN(4,IY)-QCTLRF(4,IY)))/(QCLIN(3,IY)+QCLIN(4,IY)-QCTLRF(3,IY)-QCTLRF(4,IY))
         T149(58,IY,IS) =(PELIN(3,IY)*QELIN(3,IY)+PELIN(4,IY)*QELIN(4,IY))/(QELIN(3,IY)+QELIN(4,IY))
   !  South
         T149(59,IY,IS) =(PDSIN(5,IY)*(QDSIN(5,IY)-QDSRF(5,IY))+PDSIN(6,IY)*(QDSIN(6,IY)-QDSRF(6,IY))+PDSIN(7,IY)*(QDSIN(7,IY)-QDSRF(7,IY)))/ &
                         (QDSIN(5,IY)+QDSIN(6,IY)+QDSIN(7,IY)-QDSRF(5,IY)-QDSRF(6,IY)-QDSRF(7,IY))
         T149(102,IY,IS) =(PPRIN(5,IY)*(QPRIN(5,IY)-QPRRF(5,IY))+PPRIN(6,IY)*(QPRIN(6,IY)-QPRRF(6,IY))+PPRIN(7,IY)*(QPRIN(7,IY)-QPRRF(7,IY)))/ &
                         (QPRIN(5,IY)+QPRIN(6,IY)+QPRIN(7,IY)-QPRRF(5,IY)-QPRRF(6,IY)-QPRRF(7,IY))
         T149(60,IY,IS) =(PRSIN(5,IY)*(QRSIN(5,IY)-QRSRF(5,IY))+PRSIN(6,IY)*(QRSIN(6,IY)-QRSRF(6,IY))+PRSIN(7,IY)*(QRSIN(7,IY)-QRSRF(7,IY)))/ &
                         (QRSIN(5,IY)+QRSIN(6,IY)+QRSIN(7,IY)-QRSRF(5,IY)-QRSRF(6,IY)-QRSRF(7,IY))
         T149(61,IY,IS) =(PNGIN(5,IY)*QNGIN(5,IY)+PNGIN(6,IY)*QNGIN(6,IY)+PNGIN(7,IY)*QNGIN(7,IY))/ &
                         (QNGIN(5,IY)+QNGIN(6,IY)+QNGIN(7,IY))
         T149(62,IY,IS) =(PCLIN(5,IY)*(QCLIN(5,IY)-QCTLRF(5,IY))+PCLIN(6,IY)*(QCLIN(6,IY)-QCTLRF(6,IY))+PCLIN(7,IY)*(QCLIN(7,IY)-QCTLRF(7,IY)))/ &
                         (QCLIN(5,IY)+QCLIN(6,IY)+QCLIN(7,IY)-QCTLRF(5,IY)-QCTLRF(6,IY)-QCTLRF(7,IY))
         T149(63,IY,IS) =(PELIN(5,IY)*QELIN(5,IY)+PELIN(6,IY)*QELIN(6,IY)+PELIN(7,IY)*QELIN(7,IY))/ &
                         (QELIN(5,IY)+QELIN(6,IY)+QELIN(7,IY))
   !  West
         T149(64,IY,IS) =(PDSIN(8,IY)*(QDSIN(8,IY)-QDSRF(8,IY))+PDSIN(9,IY)*(QDSIN(9,IY)-QDSRF(9,IY)))/(QDSIN(8,IY)+QDSIN(9,IY)-QDSRF(8,IY)-QDSRF(9,IY))
         T149(103,IY,IS) =(PPRIN(8,IY)*(QPRIN(8,IY)-QPRRF(8,IY))+PPRIN(9,IY)*(QPRIN(9,IY)-QPRRF(9,IY)))/(QPRIN(8,IY)+QPRIN(9,IY)-QPRRF(8,IY)-QPRRF(9,IY))
         T149(103,IY,IS) =(PPRIN(8,IY)*QPRIN(8,IY)+PPRIN(9,IY)*QPRIN(9,IY))/(QPRIN(8,IY)+QPRIN(9,IY))
         IF ((QRSIN(8,IY)+QRSIN(9,IY)-QRSRF(8,IY)-QRSRF(9,IY)) .NE. 0.0) THEN
         T149(65,IY,IS) =(PRSIN(8,IY)*(QRSIN(8,IY)-QRSRF(8,IY))+PRSIN(9,IY)*(QRSIN(9,IY)-QRSRF(9,IY)))/(QRSIN(8,IY)+QRSIN(9,IY)-QRSRF(8,IY)-QRSRF(9,IY))
         ELSE
         T149(65,IY,IS) =(PRSIN(8,IY)+PRSIN(9,IY))/2
         ENDIF
         T149(66,IY,IS) =(PNGIN(8,IY)*QNGIN(8,IY)+PNGIN(9,IY)*QNGIN(9,IY))/(QNGIN(8,IY)+QNGIN(9,IY))
         T149(67,IY,IS) =(PCLIN(8,IY)*(QCLIN(8,IY)-QCTLRF(8,IY))+PCLIN(9,IY)*(QCLIN(9,IY)-QCTLRF(9,IY)))/(QCLIN(8,IY)+QCLIN(9,IY)-QCTLRF(8,IY)-QCTLRF(9,IY))
         T149(68,IY,IS) =(PELIN(8,IY)*QELIN(8,IY)+PELIN(9,IY)*QELIN(9,IY))/(QELIN(8,IY)+QELIN(9,IY))
         T149(69,IY,IS) = PDSIN(11,IY)
         T149(104,IY,IS) = PPRIN(11,IY)
         T149(70,IY,IS) = PRSIN(11,IY)
         T149(71,IY,IS) = PNGIN(11,IY)
         T149(72,IY,IS) = PCLIN(11,IY)
         T149(73,IY,IS) = PELIN(11,IY)
!  Transportation
         T149(74,IY,IS) =(PMGTR(1,IY)*(QMGTR(1,IY)-QMGBS(1,IY))+PMGTR(2,IY)*(QMGTR(2,IY)-QMGBS(2,IY)))/(QMGTR(1,IY)+QMGTR(2,IY)-QMGBS(1,IY)-QMGBS(2,IY))
         T149(75,IY,IS) =(PMGTR(3,IY)*(QMGTR(3,IY)-QMGBS(3,IY))+PMGTR(4,IY)*(QMGTR(4,IY)-QMGBS(4,IY)))/(QMGTR(3,IY)+QMGTR(4,IY)-QMGBS(3,IY)-QMGBS(4,IY))
         T149(76,IY,IS) =(PMGTR(5,IY)*(QMGTR(5,IY)-QMGBS(5,IY))+PMGTR(6,IY)*(QMGTR(6,IY)-QMGBS(6,IY))+PMGTR(7,IY)*(QMGTR(7,IY)-QMGBS(7,IY)))/ &
                         (QMGTR(5,IY)+QMGTR(6,IY)+QMGTR(7,IY)-QMGBS(5,IY)-QMGBS(6,IY)-QMGBS(7,IY))
         T149(77,IY,IS) =(PMGTR(8,IY)*(QMGTR(8,IY)-QMGBS(8,IY))+PMGTR(9,IY)*(QMGTR(9,IY)-QMGBS(9,IY)))/(QMGTR(8,IY)+QMGTR(9,IY)-QMGBS(8,IY)-QMGBS(9,IY))
         T149(78,IY,IS) = PMGTR(11,IY)
         T149(79,IY,IS) =(PETTR(1,IY)*QETTR(1,IY)+PETTR(2,IY)*QETTR(2,IY))/(QETTR(1,IY)+QETTR(2,IY))
         T149(80,IY,IS) =(PETTR(3,IY)*QETTR(3,IY)+PETTR(4,IY)*QETTR(4,IY))/(QETTR(3,IY)+QETTR(4,IY))
         T149(81,IY,IS) =(PETTR(5,IY)*QETTR(5,IY)+PETTR(6,IY)*QETTR(6,IY)+PETTR(7,IY)*QETTR(7,IY))/ &
                         (QETTR(5,IY)+QETTR(6,IY)+QETTR(7,IY))
         T149(82,IY,IS) =(PETTR(8,IY)*QETTR(8,IY)+PETTR(9,IY)*QETTR(9,IY))/(QETTR(8,IY)+QETTR(9,IY))
         T149(83,IY,IS) = PETTR(11,IY)
!  Electric Power
         T149(84,IY,IS) =(UPRWDCR(1,IY)*QBMEL(1,IY)+UPRWDCR(2,IY)*QBMEL(2,IY))/(QBMEL(1,IY)+QBMEL(2,IY))
         T149(85,IY,IS) =(UPRWDCR(3,IY)*QBMEL(3,IY)+UPRWDCR(4,IY)*QBMEL(4,IY))/(QBMEL(3,IY)+QBMEL(4,IY))
         T149(86,IY,IS) =(UPRWDCR(5,IY)*QBMEL(5,IY)+UPRWDCR(6,IY)*QBMEL(6,IY)+UPRWDCR(7,IY)*QBMEL(7,IY))/ &
                         (QBMEL(5,IY)+QBMEL(6,IY)+QBMEL(7,IY))
         T149(87,IY,IS) =(UPRWDCR(8,IY)*QBMEL(8,IY)+UPRWDCR(9,IY)*QBMEL(9,IY))/(QBMEL(8,IY)+QBMEL(9,IY))
         T149(88,IY,IS) = UPRWDCR(11,IY)


!  regional quantities:
         T149(105,IY,IS) = QDSRS(1,IY) + QDSRS(2,IY)
         T149(106,IY,IS) = QPRRS(1,IY) + QPRRS(2,IY)
         T149(107,IY,IS) = QNGRS(1,IY) + QNGRS(2,IY)
         T149(108,IY,IS) = QELRS(1,IY) + QELRS(2,IY)
         T149(109,IY,IS) = QDSRS(3,IY) + QDSRS(4,IY)
         T149(110,IY,IS) = QPRRS(3,IY) + QPRRS(4,IY)
         T149(111,IY,IS) = QNGRS(3,IY) + QNGRS(4,IY)
         T149(112,IY,IS) = QELRS(3,IY) + QELRS(4,IY)
         T149(113,IY,IS) = QDSRS(5,IY) + QDSRS(6,IY) + QDSRS(7,IY)
         T149(114,IY,IS) = QPRRS(5,IY) + QPRRS(6,IY) + QPRRS(7,IY)
         T149(115,IY,IS) = QNGRS(5,IY) + QNGRS(6,IY) + QNGRS(7,IY)
         T149(116,IY,IS) = QELRS(5,IY) + QELRS(6,IY) + QELRS(7,IY)
         T149(117,IY,IS) = QDSRS(8,IY) + QDSRS(9,IY)
         T149(118,IY,IS) = QPRRS(8,IY) + QPRRS(9,IY)
         T149(119,IY,IS) = QNGRS(8,IY) + QNGRS(9,IY)
         T149(120,IY,IS) = QELRS(8,IY) + QELRS(9,IY)

         T149(121,IY,IS) = QDSCM(1,IY) + QDSCM(2,IY)
         T149(122,IY,IS) = QPRCM(1,IY) + QPRCM(2,IY)
         T149(123,IY,IS) = QRSCM(1,IY) + QRSCM(2,IY)
         T149(124,IY,IS) = QNGCM(1,IY) + QNGCM(2,IY)
         T149(125,IY,IS) = QCLCM(1,IY) + QCLCM(2,IY)
         T149(126,IY,IS) = QELCM(1,IY) + QELCM(2,IY)
         T149(127,IY,IS) = QDSCM(3,IY) + QDSCM(4,IY)
         T149(128,IY,IS) = QPRCM(3,IY) + QPRCM(4,IY)
         T149(129,IY,IS) = QRSCM(3,IY) + QRSCM(4,IY)
         T149(130,IY,IS) = QNGCM(3,IY) + QNGCM(4,IY)
         T149(131,IY,IS) = QCLCM(3,IY) + QCLCM(4,IY)
         T149(132,IY,IS) = QELCM(3,IY) + QELCM(4,IY)
         T149(133,IY,IS) = QDSCM(5,IY) + QDSCM(6,IY) + QDSCM(7,IY)
         T149(134,IY,IS) = QPRCM(5,IY) + QPRCM(6,IY) + QPRCM(7,IY)
         T149(135,IY,IS) = QRSCM(5,IY) + QRSCM(6,IY) + QRSCM(7,IY)
         T149(136,IY,IS) = QNGCM(5,IY) + QNGCM(6,IY) + QNGCM(7,IY)
         T149(137,IY,IS) = QCLCM(5,IY) + QCLCM(6,IY) + QCLCM(7,IY)
         T149(138,IY,IS) = QELCM(5,IY) + QELCM(6,IY) + QELCM(7,IY)
         T149(139,IY,IS) = QDSCM(8,IY) + QDSCM(9,IY)
         T149(140,IY,IS) = QPRCM(8,IY) + QPRCM(9,IY)
         T149(141,IY,IS) = QRSCM(8,IY) + QRSCM(9,IY)
         T149(142,IY,IS) = QNGCM(8,IY) + QNGCM(9,IY)
         T149(143,IY,IS) = QCLCM(8,IY) + QCLCM(9,IY)
         T149(144,IY,IS) = QELCM(8,IY) + QELCM(9,IY)

         T149(145,IY,IS) = QDSIN(1,IY) + QDSIN(2,IY)
         T149(146,IY,IS) = QPRIN(1,IY) + QPRIN(2,IY)
         T149(147,IY,IS) = QRSIN(1,IY) + QRSIN(2,IY)
         T149(148,IY,IS) = QNGIN(1,IY) + QNGIN(2,IY)
         T149(149,IY,IS) = QCLIN(1,IY) + QCLIN(2,IY) - QCTLRF(1,IY) - QCTLRF(2,IY)
         T149(150,IY,IS) = QELIN(1,IY) + QELIN(2,IY)
         T149(151,IY,IS) = QDSIN(3,IY) + QDSIN(4,IY)
         T149(152,IY,IS) = QPRIN(3,IY) + QPRIN(4,IY)
         T149(153,IY,IS) = QRSIN(3,IY) + QRSIN(4,IY)
         T149(154,IY,IS) = QNGIN(3,IY) + QNGIN(4,IY)
         T149(155,IY,IS) = QCLIN(3,IY) + QCLIN(4,IY) - QCTLRF(3,IY) - QCTLRF(4,IY)
         T149(156,IY,IS) = QELIN(3,IY) + QELIN(4,IY)
         T149(157,IY,IS) = QDSIN(5,IY) + QDSIN(6,IY) + QDSIN(7,IY)
         T149(158,IY,IS) = QPRIN(5,IY) + QPRIN(6,IY) + QPRIN(7,IY)
         T149(159,IY,IS) = QRSIN(5,IY) + QRSIN(6,IY) + QRSIN(7,IY)
         T149(160,IY,IS) = QNGIN(5,IY) + QNGIN(6,IY) + QNGIN(7,IY)
         T149(161,IY,IS) = QCLIN(5,IY) + QCLIN(6,IY) + QCLIN(7,IY) - QCTLRF(5,IY) - QCTLRF(6,IY) - QCTLRF(7,IY)
         T149(162,IY,IS) = QELIN(5,IY) + QELIN(6,IY) + QELIN(7,IY)
         T149(163,IY,IS) = QDSIN(8,IY) + QDSIN(9,IY)
         T149(164,IY,IS) = QPRIN(8,IY) + QPRIN(9,IY)
         T149(165,IY,IS) = QRSIN(8,IY) + QRSIN(9,IY)
         T149(166,IY,IS) = QNGIN(8,IY) + QNGIN(9,IY)
         T149(167,IY,IS) = QCLIN(8,IY) + QCLIN(9,IY) - QCTLRF(8,IY) - QCTLRF(9,IY)
         T149(168,IY,IS) = QELIN(8,IY) + QELIN(9,IY)

         T149(169,IY,IS) = QMGTR(1,IY) + QMGTR(2,IY) - QMGBS(1,IY) - QMGBS(2,IY)
         T149(170,IY,IS) = QETTR(1,IY) + QETTR(2,IY)
         T149(171,IY,IS) = QMGTR(3,IY) + QMGTR(4,IY) - QMGBS(3,IY) - QMGBS(4,IY)
         T149(172,IY,IS) = QETTR(3,IY) + QETTR(4,IY)
         T149(173,IY,IS) = QMGTR(5,IY) + QMGTR(6,IY) + QMGTR(7,IY) - QMGBS(5,IY) - QMGBS(6,IY) - QMGBS(7,IY)
         T149(174,IY,IS) = QETTR(5,IY) + QETTR(6,IY) + QETTR(7,IY)
         T149(175,IY,IS) = QMGTR(8,IY) + QMGTR(9,IY) - QMGBS(8,IY) - QMGBS(9,IY)
         T149(176,IY,IS) = QETTR(8,IY) + QETTR(9,IY)

         T149(177,IY,IS) = QBMEL(1,IY) + QBMEL(2,IY)
         T149(178,IY,IS) = QBMEL(3,IY) + QBMEL(4,IY)
         T149(179,IY,IS) = QBMEL(5,IY) + QBMEL(6,IY) + QBMEL(7,IY)
         T149(180,IY,IS) = QBMEL(8,IY) + QBMEL(9,IY)
      ENDDO

! Table 150 - Convergence Summary

      DO IY=1,LASTYR

        do i=1,102
          T150(i,IY,IS)=CVTAB(i,4,IY)
        enddo
        t150(103,iy,is)=cvscore(iy)
        t150(104,iy,is)=cvscore_US(iy)
        t150(105,iy,is)=oscor
        t150(106:115,iy,is)=cvscorehist(iy,:)

      ENDDO

      RETURN
      END
      SUBROUTINE FDATA_IEA
      IMPLICIT NONE

      include 'parametr'
      include 'emmparm'
      include 'qblk'
      include 'ampblk'          !  use adjusted prices
      include 'macout'
      include 'lfmmout'
      include 'pmmout'
      include 'pmmrpt'
      include 'pmmftab'
      include 'ogsmout'
      include 'convfact'
      include 'ngtdmrep'
      include 'ngrpt'
      include 'angtdm'
      include 'ncntrl'
      include 'emission'
      include 'uefpout'
      include 'uefdout'
      include 'udatout'
      include 'uettout'
      include 'uecpout'
      include 'efpout'
      include 'comparm'
      include 'comout'
      include 'commrep'
      include 'cogen'
      include 'cdsparms'
      include 'coalout'
      include 'coalemm'
      include 'coalrep'
      include 'indrep'
      include 'intout'
      include 'resdrep'
      include 'tranrep'
      include 'wrenew'
      include 'ftable'
      include 'indout'

      INTEGER FTIEAYRS
      PARAMETER(FTIEAYRS=12)
      REAL*4 FSUM,RDAYS
      EXTERNAL FSUM
      INTEGER I,IY,II,III,COUNT,UNIT_IEA,NEXTROW,TOTALROWS
      REAL*4 IEAGROSS,MTOECONV,TOE_PJ
      REAL I120(NTAB120,FTIEAYRS,16)  ! Note:  this table does not represent the regular ..,IY,IS concept
      REAL*8 REALNO
      CHARACTER*50 FILE_IEA,TEXTSTR*166
      LOGICAL CONTINUE120

! Index Constants for Industrial Tables 35 to 44, 120-124
      integer ixEL/1/,ixNG/2/,ixCL/3/,ixMC/4/, &
              ixCI/5/,ixRF/6/,ixDS/7/,ixLG/8/,ixMG/9/, &
              ixSG/10/,ixPC/11/,ixAS/12/,ixPF/13/,ixKS/14/, &
              ixOP/15/,ixNF/16/,ixLF/17/,ixRN/18/

!  IYEAR represents the years needed to print.
      INTEGER IYEAR(FTIEAYRS) /2010, 2015, 2016, 2017, 2018, 2019, 2020, 2025, 2030, 2035, 2040, 2050/

!    IEAGROSS: USED TO CONVERT NET ELECTRIC GENERATION TO GROSS FOR IEA:
!              1.07 -> COMBUSTIBLE FUELS
!              1.06 -> GEOTHERMAL, NUCLEAR
!              1.01 -> HYDRO, WIND, SOLAR

! U.S. Coal

      REPORT_SWITCH(120) = .FALSE.
      IEAGROSS = 1.07
      MTOECONV = 25.2
      TOE_PJ = 41.68
      DO I=1,FTIEAYRS
        IY=IYEAR(I)-BASEYR+1
        I120( 1,I,1) =(CQSBB(3,IY) + sum(WC_PROD_BTU(11,1:MNUMLR,IY)))/1000. * MTOECONV
        I120( 2,I,1) = CQDBFB(11,7,IY)*.001 * MTOECONV
        I120( 3,I,1) = (-1.)*CQDBFB(11,5,IY)*.001 * MTOECONV
! add net coal coke imports to imports if positive; to exports if negative
        IF (QCIIN(11,IY) .GT. 0.0) THEN
           I120( 2,I,1) = I120( 2,I,1) + QCIIN(11,IY) * MTOECONV
        ELSE            ! exports are negative here, and so is QCIIN!
           I120( 3,I,1) = I120( 3,I,1) + QCIIN(11,IY) * MTOECONV
        ENDIF
        I120( 5,I,1) = 0.0
        I120( 6,I,1) = (-1.)*(QCLEL(11,IY) + (CGCOMMQ(11,IY,1) + &
                       CGREFQ(11,IY,1) + &
                       CGOGSQ(11,IY,1) + &
                       CGINDLQ(11,IY,1)) * .001) * MTOECONV
!       --- SWITCHING rows 7 and 8 SO NEMS MATCHES EMEU --- JBJ
        I120( 7,I,1) = (-1.) * QMCIN(11,IY) * MTOECONV
!       Demand breakout
        I120(10,I,1) = ((QCLIN(11,IY) + QCIIN(11,IY)) - &
               (CGREFQ(11,IY,1) + CGOGSQ(11,IY,1) + CGINDLQ(11,IY,1)) * .001) * MTOECONV + &
                       CTLFRAC(1,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(1,IY) * MTOECONV + &
                       CBTLFRAC(1,1,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(1,IY) * MTOECONV
        I120(11,I,1) = 0.0
        I120(12,I,1) = sum(CTLFRAC(1:4,MNUMPR,IY))/1000. * RDAYS * .001 * CFCTLLIQ(IY) * MTOECONV + &
                       sum(CBTLFRAC(1,1:4,MNUMPR,IY))/1000. * RDAYS * .001 * CFCBTLLIQ(1,IY) * MTOECONV - &
                       CTLFRAC(1,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(1,IY) * MTOECONV - &
                       CBTLFRAC(1,1,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(1,IY) * MTOECONV
        I120(13,I,1) = sum(CTLFRAC(1:4,MNUMPR,IY))/1000. * RDAYS * .001 * CFCTLLIQ(IY) * MTOECONV + &
                       sum(CBTLFRAC(1,1:4,MNUMPR,IY))/1000. * RDAYS * .001 * CFCBTLLIQ(1,IY) * MTOECONV - &
                       CTLFRAC(1,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(1,IY) * MTOECONV - &
                       CBTLFRAC(1,1,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(1,IY) * MTOECONV
        I120(14,I,1) =(QCLRS(11,IY) + QCLCM(11,IY) - CGCOMMQ(11,IY,1) * .001)* MTOECONV
        I120(15,I,1) = QCLRS(11,IY) * MTOECONV
        I120(17,I,1) =((CGNTGEN(mnumnr,IY,1,1)  + CGOGSGEN(11,IY,1,1) + &
            CGOGSGEN(11,IY,1,2) + CGINDLGEN(11,IY,1,1) + CGINDLGEN(11,IY,1,2) + &
            CGREFGEN(11,IY,1,1) + CGREFGEN(11,IY,1,2) + CGCOMMGEN(11,IY,1,1) + CGCOMMGEN(11,IY,1,2) + &
            CGNTGEN(mnumnr,IY,1,2)) * .001 + UGNCLNR(1,mnumnr,IY)+ UGNCLNR(2,mnumnr,IY)) * IEAGROSS
!       Statistical differences
        I120( 9,I,1) = I120(10,I,1) + I120(12,I,1) + I120(14,I,1) - FSUM(I120(1,I,1),8)
        I120(18,I,1) =((CGCOMMQ(11,IY,1)+CGINDLQ(11,IY,1) + &
            CGNTQ(mnumnr,IY,1) + CGREFQ(11,IY,1) + &
            CGOGSQ(11,IY,1)) * .001 * MTOECONV - &
           (CGNTGEN(mnumnr,IY,1,1)  + CGOGSGEN(11,IY,1,1) + CGOGSGEN(11,IY,1,2) + &
            CGREFGEN(11,IY,1,1) + CGREFGEN(11,IY,1,2) + &
            CGINDLGEN(11,IY,1,1) + CGINDLGEN(11,IY,1,2) + &
            CGCOMMGEN(11,IY,1,1) + CGCOMMGEN(11,IY,1,2) + &
            CGNTGEN(mnumnr,IY,1,2)) * .001 * .0859845228 / .355) * TOE_PJ
        I120(18,I,1) = (CGCOMMQ(11,IY,1)+CGINDLQ(11,IY,1) + &
            CGNTQ(mnumnr,IY,1) + CGREFQ(11,IY,1) + &
            CGOGSQ(11,IY,1)) * .001 * .10 * MTOECONV * TOE_PJ

! Peat
      I120( 1,I,2) = 0.0
	  
! U.S. Other Solid Fuels (Non-Renewable Solid Waste)

      IEAGROSS = 1.07
!        --- INDIGENOUS PRODUCTION
      RDAYS = 365.
	  IF (MOD(IYEAR(I),4).EQ.0) RDAYS = 366.
	  I120( 1,I,12) =MTOECONV*WNCMSEL(IY,11)  
	  I120( 6,I,12) = (-1.)*MTOECONV* (WNCMSEL(IY,11))
	  I120( 9,I,12) = I120(10,I,12) + I120(12,I,12) + I120(14,I,12) - FSUM(I120( 1,I,12),8)
	  I120(17,I,12) =((WNGMSEL(IY,mnumnr))*.001) * IEAGROSS
	  
! U.S. Other Solid Fuels (Renewable Solid Waste)

      IEAGROSS = 1.07
!        --- INDIGENOUS PRODUCTION
      RDAYS = 365.
      IF (MOD(IYEAR(I),4).EQ.0) RDAYS = 366.
      I120( 1,I,11) =MTOECONV*(QBMEL(11,IY) +  (QMSEL(11,IY)-WNCMSEL(IY,11)) + &
            QBMIN(11,IY)+QMSIN(11,IY) + QBMRS(11,IY) + QBMCM(11,IY) + &
            QBMRFBTL(11,IY)/1000. + sum(BIMQTYCD(1:4,11,IY))/1000000.*RDAYS*CFVEGGIE(IY) + &
           (SBO2GDTPD(MNUMPR,IY) + YGR2GDTPD(MNUMPR,IY) + WGR2GDTPD(MNUMPR,IY))*CFVEGGIE(IY)*RDAYS / 1000000. + &
            CORNCD(3,11,IY) * CFCORN / 1000000000.)
      IF (CONEFF(IY) .NE. 0.0) I120( 1,I,11) = I120( 1,I,11) + &
            MTOECONV*(0.9751*CLLETHCD(11,IY) * RDAYS * CFBMQ(IY) * 42. / CONEFF(IY) / 1000000.)
      I120( 2,I,11) =(BIODIMP(11,IY) * CFBIOD(IY) + ETHIMP(11,IY) * CFPET)/1000. *MTOECONV*RDAYS*.001
      I120( 3,I,11) =(BIODEXP(11,IY) * CFBIOD(IY) + ETHEXP(11,IY) * CFPET)/1000. * RDAYS * .001 * MTOECONV * (-1.)
      I120( 6,I,11) = (-1.)*MTOECONV* (QBMEL(11,IY) + (QMSEL(11,IY) - WNCMSEL(IY,11)) + &
          (CGINDLQ(11,IY,6) + CGINDLQ(11,IY,7) + CGINDLQ(11,IY,10) + &
           CGCOMMQ(11,IY,6) + CGCOMMQ(11,IY,7) + CGCOMMQ(11,IY,10) + &
           CGREFQ(11,IY,6) + CGREFQ(11,IY,7) + CGOGSQ(11,IY,4))*.001)
      I120( 8,I,11) = (-1.) * MTOECONV * &
                  ((CORNCD(3,11,IY) * CFCORN / 1000. -0.9751*(CRNETHCD(11,IY)+OTHETHCD(11,IY)) * RDAYS * CFPET - &
                    RFBIOBUTECD(MNUMCR,IY)*365.*CFBIOBUTE(IY))/1000000. + &
                  (SBO2GDTPD(MNUMPR,IY) + YGR2GDTPD(MNUMPR,IY) + WGR2GDTPD(MNUMPR,IY) + &
                    sum(BIMQTYCD(1:4,11,IY)))/1000000.*RDAYS*(CFVEGGIE(IY)-CFBIOD(IY)) + &
              QBMRFBTL(11,IY)/1000. - RDAYS / 1000000. * &
                           (sum(BTLFRAC(1:4,MNUMPR,IY)) * CFBTLLIQ(IY) + &
                            sum(CBTLFRAC(2,1:4,MNUMPR,IY)) * CFCBTLLIQ(2,IY) + &
                            UBAVOL(MNUMPR,IY) * 5.763))
      IF (CONEFF(IY) .NE. 0.0) I120( 8,I,11) = I120( 8,I,11) + (-1.) * MTOECONV * &
              ((0.9751*CLLETHCD(11,IY) * RDAYS * (CFBMQ(IY) * 42. / CONEFF(IY) - CFPET)) / 1000000.)
      I120(10,I,11) =(QBMIN(11,IY) + QMSIN(11,IY) - &
               (CGREFQ(11,IY,6) + CGREFQ(11,IY,7) + &
                CGINDLQ(11,IY,6) + CGINDLQ(11,IY,7) + CGINDLQ(11,IY,10) + &
                CGOGSQ(11,IY,4)) * .001) * MTOECONV + &
                      BTLFRAC(1,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(1,IY) * MTOECONV + &
                      CBTLFRAC(2,1,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(1,IY) * MTOECONV
      I120(12,I,11) =(0.9751*(CRNETHCD(11,IY)+CLLETHCD(11,IY)+OTHETHCD(11,IY))+ETHIMP(11,IY)-ETHEXP(11,IY))/1000. * RDAYS * .001 * CFPET * MTOECONV + &
                     (sum(BTLFRAC(1:4,MNUMPR,IY)))/1000. * RDAYS * .001 * CFBTLLIQ(IY) * MTOECONV - &
                      BTLFRAC(1,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(1,IY) * MTOECONV + &
                     (sum(CBTLFRAC(2,1:4,MNUMPR,IY)))/1000. * RDAYS * .001 * CFCBTLLIQ(2,IY) * MTOECONV - &
                      CBTLFRAC(2,1,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(1,IY) * MTOECONV + &
                      UBAVOL(MNUMPR,IY)/1000. *5.763 * RDAYS * .001 * MTOECONV + &
           (SBO2GDTPD(MNUMPR,IY) + YGR2GDTPD(MNUMPR,IY) + WGR2GDTPD(MNUMPR,IY))*CFBIOD(IY)*RDAYS / 1000000. * MTOECONV + &
                     (sum(BIMQTYCD(1:4,11,IY))+BIODIMP(11,IY)-BIODEXP(11,IY))/1000. * RDAYS * .001 * CFBIOD(IY) * MTOECONV
      I120(13,I,11) = I120(12,I,11)  !  All transportation use is highway
      I120(14,I,11) = (QBMRS(11,IY) + QBMCM(11,IY) -(CGCOMMQ(11,IY,6) + &
                       CGCOMMQ(11,IY,7) + CGCOMMQ(11,IY,10))*.001) * MTOECONV
      I120(15,I,11) =  QBMRS(11,IY) * MTOECONV
!     I120(10,I,11) = I120(10,I,11) + I120(12,I,11) + I120(14,I,11) - FSUM(I120( 1,I,11),8)
      I120(17,I,11) =(UGNMSNR(1,mnumnr,IY) + UGNWDNR(1,mnumnr,IY)+ &
                      UGNMSNR(2,mnumnr,IY) + UGNWDNR(2,mnumnr,IY)+ &
                      (CGINDLGEN(11,IY,6,1) + CGINDLGEN(11,IY,6,2)+ &
                      CGINDLGEN(11,IY,7,1) + CGINDLGEN(11,IY,7,2)+ &
                      CGINDLGEN(11,IY,10,1) + CGINDLGEN(11,IY,10,2)+ &
                      CGREFGEN(11,IY,6,1) + CGREFGEN(11,IY,6,2) + &
                      CGREFGEN(11,IY,7,1) + CGREFGEN(11,IY,7,2) + &
                      CGCOMMGEN(11,IY,6,1) + CGCOMMGEN(11,IY,7,1) + CGCOMMGEN(11,IY,10,1) + &
                      CGCOMMGEN(11,IY,6,2) + CGCOMMGEN(11,IY,7,2) + CGCOMMGEN(11,IY,10,2) + &
                      CGOGSGEN(11,IY,4,1) + CGOGSGEN(11,IY,4,2) + &
                      CGNTGEN(mnumnr,IY,6,1) + CGNTGEN(mnumnr,IY,7,1) + &
                      CGNTGEN(mnumnr,IY,6,2) + CGNTGEN(mnumnr,IY,7,2) + &
                      CGNTGEN(mnumnr,IY,10,1) + CGNTGEN(mnumnr,IY,10,2))*.001) * IEAGROSS
      I120( 9,I,11) = I120(10,I,11) + I120(12,I,11) + I120(14,I,11) - FSUM(I120( 1,I,11),8)
! Keeping a constant 36% calculated efficiency for row 18
      I120(18,I,11) = (-.36 * I120(6,I,11) - I120(17,I,11) * .0859845228) / .02388
        I120(18,I,11) = (CGCOMMQ(11,IY,6) + CGINDLQ(11,IY,6) + CGNTQ(mnumnr,IY,6) + &
            CGREFQ(11,IY,6) + CGREFQ(11,IY,7) + CGOGSQ(11,IY,4) + &
            CGCOMMQ(11,IY,7) + CGINDLQ(11,IY,7) + CGNTQ(mnumnr,IY,7) + &
            CGCOMMQ(11,IY,10) + CGINDLQ(11,IY,10) + &
            CGNTQ(mnumnr,IY,10)) * .001 * .02 * MTOECONV * TOE_PJ

! U.S. Oil

      IEAGROSS = 1.07
      RDAYS = 365.
      I120( 1,I,3)=((RFQTDCRD(MNUMOR+2,IY) + RFCRDOTH(MNUMPR,IY)) * CFCRDDOM(IY)+ &
                   RFHCXH2IN(MNUMPR,IY)*CFRSQ + &
                   RFQNGPL(MNUMPR,IY,6) / 1000. * CFNGL(IY)) * MTOECONV * RDAYS * .001
      I120( 2,I,3) =((RFQICRD(MNUMPR,IY)+RFSPRIM(IY)) * CFCRDIMP(IY) + RFPQUFC(MNUMPR,IY,2)*CFIMUO(IY) + &
                RFMTBI(MNUMPR,IY) * 4.24 + &
 RFIPQCBOB(MNUMPR,IY,2) * CFTGQ(IY) / 1000. + &
 RFIPQRBOB(MNUMPR,IY,2) * CFRGQ(IY) / 1000. + &
                RFPQIPRDT(MNUMPR,IY,2)*CFIMPRD(IY)) *MTOECONV*RDAYS*.001
      I120( 3,I,3) = (RFQEXCRD(MNUMPR,IY) * CFCRDEXP(IY) * .001 + RFQEXPRDT(MNUMPR,IY) * CFEXPRD(IY)) &
                      * RDAYS * .001 * MTOECONV * (-1.)
      I120( 4,I,3) = (-1.)*(TRQINTS(1,IY) +TRQINTS(2,IY)) * .001 * MTOECONV
      I120( 5,I,3) = RFSPRFR(IY) * RDAYS * CFCRDIMP(IY) * .001 * MTOECONV
      I120( 6,I,3) = (-1.) * MTOECONV * (QDSEL(11,IY)+QRSEL(11,IY) + &
              (CGREFQ(11,IY,9) + CGREFQ(11,IY,2) + CGOGSQ(11,IY,2) + &
               CGINDLQ(11,IY,2) + CGINDLQ(11,IY,9) + &
!              CGNTQ(mnumnr,IY,2) + CGNTQ(mnumnr,IY,9) + &
               CGCOMMQ(11,IY,2) + CGCOMMQ(11,IY,9))*.001 + &
              (CMUSDISTSERV(1,3,IY) + CMUSDISTSERV(2,3,IY) + CMUSDISTSERV(3,3,IY)))
      I120( 7,I,3)= 0.0
      I120( 8,I,3)= (REFCON(ixRF,5,IY) + REFCON(ixDS,5,IY) + &
            REFCON(ixLG,5,IY) + REFCON(ixMG,5,IY) + REFCON(ixSG,5,IY) + &
            REFCON(ixPC,5,IY)) * (-1.) * .001 * MTOECONV
      I120(10,I,3) = (QTPIN(11,IY) - QMGIN(11,IY) - QOTIN(11,IY) - QASIN(11,IY) - &
               (CGREFQ(11,IY,2) + CGREFQ(11,IY,9) + CGOGSQ(11,IY,2) + &
                CGINDLQ(11,IY,2) + CGINDLQ(11,IY,9) + &
                                   REFCON(ixRF,5,IY) + REFCON(ixDS,5,IY) + &
            REFCON(ixLG,5,IY) + REFCON(ixMG,5,IY) + REFCON(ixSG,5,IY) + &
            REFCON(ixPC,5,IY)) * .001) * MTOECONV - &
            (CTLFRAC(1,MNUMPR,IY)+GTLFRAC(1,MNUMPR,IY)+BTLFRAC(1,MNUMPR,IY)+ &
             CBTLFRAC(1,1,MNUMPR,IY)+CBTLFRAC(2,1,MNUMPR,IY))/1000. * RDAYS * .001 * CFFTLIQ(1,IY) * MTOECONV
      I120(11,I,3) = (QPFIN(11,IY) + INQLGPF(11,IY)*.001)*MTOECONV
      I120(12,I,3) = (QDSTR(11,IY) + QJFTR(11,IY) + QMGAS(11,IY) + QRSTR(11,IY) + &
                      QLGTR(11,IY) + QOTTR(11,IY) + QETTR(11,IY)) * MTOECONV - &
                     (TRQENUSE(7,IY) + TRQMIL(3,IY) + TRQMIL(4,IY)) * .001 * MTOECONV - &
                     (0.9751*(CRNETHCD(11,IY)+CLLETHCD(11,IY)+OTHETHCD(11,IY))+ETHIMP(11,IY)-ETHEXP(11,IY))/1000. * RDAYS * .001 * CFPET * MTOECONV - &
                      sum(CTLFRAC(1:4,MNUMPR,IY))/1000. * RDAYS * .001 * CFCTLLIQ(IY) * MTOECONV + &
                      CTLFRAC(1,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(1,IY) * MTOECONV - &
                      sum(GTLFRAC(1:4,MNUMPR,IY))/1000. * RDAYS * .001 * CFGTLLIQ(IY) * MTOECONV + &
                      GTLFRAC(1,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(1,IY) * MTOECONV - &
                      sum(BTLFRAC(1:4,MNUMPR,IY))/1000. * RDAYS * .001 * CFBTLLIQ(IY) * MTOECONV + &
                      BTLFRAC(1,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(1,IY) * MTOECONV - &
                      sum(CBTLFRAC(1,1:4,MNUMPR,IY))/1000. * RDAYS * .001 * CFCBTLLIQ(1,IY) * MTOECONV + &
                      CBTLFRAC(1,1,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(1,IY) * MTOECONV - &
                      sum(CBTLFRAC(2,1:4,MNUMPR,IY))/1000. * RDAYS * .001 * CFCBTLLIQ(2,IY) * MTOECONV + &
                      CBTLFRAC(2,1,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(1,IY) * MTOECONV - &
                      sum(BIMQTYCD(1:4,11,IY))/1000. * RDAYS * .001 * CFBIOD(IY) * MTOECONV - &
                     (BIODIMP(11,IY)-BIODEXP(11,IY))/1000. * RDAYS * .001 * CFBIOD(IY) * MTOECONV - &
                     (GRD2DSQTY(MNUMPR,IY) * CFDSQ + GRN2MGQTY(MNUMPR,IY) * CFNPQ) *RDAYS / 1000000. * MTOECONV - &
                      UBAVOL(MNUMPR,IY)/1000. * RDAYS *.001 * 5.763 * MTOECONV - &
                     (TRQINTS(1,IY) +TRQINTS(2,IY)) * .001 * MTOECONV
      I120(13,I,3) =(QMGCM(11,IY) + QMGIN(11,IY)) * MTOECONV + .001 * MTOECONV * &
            (TRQLDV(1,11,IY) + TRQLDV(3,11,IY) + TRQLDV(5,11,IY) + TRQLDV(8,11,IY) + &
             BCLTBTUT(10,IY) + &
             TRQFTRK(1,IY) + TRQFTRK(2,IY) + TRQFTRK(5,IY) +  &
             sum(TRQBUS(:,1,IY)) + sum(TRQBUS(:,2,IY)) + sum(TRQBUS(:,6,IY))) - &
                     (0.9751*(CRNETHCD(11,IY)+CLLETHCD(11,IY)+OTHETHCD(11,IY))+ETHIMP(11,IY)-ETHEXP(11,IY))/1000. * RDAYS * .001 * CFPET * MTOECONV - &
                      CTLFRAC(2,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(2,IY) * MTOECONV - &
                      CTLFRAC(3,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(3,IY) * MTOECONV - &
                      CTLFRAC(4,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(4,IY) * MTOECONV - &
                      GTLFRAC(2,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(2,IY) * MTOECONV - &
                      GTLFRAC(3,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(3,IY) * MTOECONV - &
                      GTLFRAC(4,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(4,IY) * MTOECONV - &
                      BTLFRAC(2,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(2,IY) * MTOECONV - &
                      BTLFRAC(3,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(3,IY) * MTOECONV - &
                      BTLFRAC(4,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(4,IY) * MTOECONV - &
                      sum(CBTLFRAC(1:2,2,MNUMPR,IY))/1000. * RDAYS * .001 * CFFTLIQ(2,IY) * MTOECONV - &
                      sum(CBTLFRAC(1:2,3,MNUMPR,IY))/1000. * RDAYS * .001 * CFFTLIQ(3,IY) * MTOECONV - &
                      sum(CBTLFRAC(1:2,4,MNUMPR,IY))/1000. * RDAYS * .001 * CFFTLIQ(4,IY) * MTOECONV - &
                      sum(BIMQTYCD(1:4,11,IY))/1000. * RDAYS * .001 * CFBIOD(IY) * MTOECONV - &
                     (BIODIMP(11,IY)-BIODEXP(11,IY))/1000. * RDAYS * .001 * CFBIOD(IY) * MTOECONV - &
                     (TRQINTS(1,IY) +TRQINTS(2,IY)) * .001 * MTOECONV
      I120(14,I,3) = MTOECONV * (QDSRS(11,IY) + QKSRS(11,IY) +QLGRS(11,IY) + &
             QDSCM(11,IY) + QKSCM(11,IY) +QLGCM(11,IY) + QRSCM(11,IY) + &
            (TRQMIL(3,IY) + TRQMIL(4,IY)) * .001 - &
               (CMUSDISTSERV(1,3,IY) + CMUSDISTSERV(2,3,IY) + CMUSDISTSERV(3,3,IY)) - &
               (CGCOMMQ(11,IY,2) + CGCOMMQ(11,IY,9)) * .001)
      I120(15,I,3) =(QDSRS(11,IY) +QKSRS(11,IY) +QLGRS(11,IY)) * MTOECONV
      I120(16,I,3) = MTOECONV * (QOTIN(11,IY) + QASIN(11,IY) + TRQENUSE(7,IY) * .001)
!  move petrochemical feedstocks from Industrial to Non-Energy
      I120(10,I,3) = I120(10,I,3) - I120(11,I,3)
      I120(16,I,3) = I120(16,I,3) + I120(11,I,3)
      I120( 9,I,3) = I120(10,I,3) + I120(12,I,3) + I120(14,I,3) + I120(16,I,3) - &
                     FSUM(I120( 1,I,3),8)
      I120(17,I,3) =(UGNDSNR(1,mnumnr,IY)+UGNRLNR(1,mnumnr,IY)+ &
               UGNRHNR(1,mnumnr,IY)+UGNDSNR(2,mnumnr,IY)+UGNRLNR(2,mnumnr,IY)+ &
               UGNRHNR(2,mnumnr,IY)+ (CGNTGEN(mnumnr,IY,2,2) + &
                 CGNTGEN(mnumnr,IY,2,1)  + CGREFGEN(11,IY,2,1) + &
                 CGREFGEN(11,IY,2,2)  + CGOGSGEN(11,IY,2,1) + &
                 CGOGSGEN(11,IY,2,2)  + CGINDLGEN(11,IY,2,1) + &
                 CGINDLGEN(11,IY,2,2) + CGCOMMGEN(11,IY,2,1) + CGCOMMGEN(11,IY,2,2) + &
                 CGNTGEN(mnumnr,IY,9,1)  + CGREFGEN(11,IY,9,1) + &
                 CGREFGEN(11,IY,9,2)  + &
                 CGNTGEN(mnumnr,IY,9,2)  + CGINDLGEN(11,IY,9,1) + &
                 CGINDLGEN(11,IY,9,2) + &
                 CGCOMMGEN(11,IY,9,1) + CGCOMMGEN(11,IY,9,2)) * .001) * IEAGROSS
! Keeping a constant 45% calculated efficiency for row 18
      I120(18,I,3) = (-.45 * I120(6,I,3) - I120(17,I,3) * .0859845228) / .02388
        I120(18,I,3) = (CGCOMMQ(11,IY,2) + CGINDLQ(11,IY,2) + &
            CGNTQ(mnumnr,IY,2) + CGREFQ(11,IY,9) + &
            CGREFQ(11,IY,2) + CGOGSQ(11,IY,2) + &
            CGCOMMQ(11,IY,9) + CGINDLQ(11,IY,9) + &
            CGNTQ(mnumnr,IY,9)) * .001 * .10 * MTOECONV * TOE_PJ

! U.S. Natural Gas

      IEAGROSS = 1.07
      I120( 1,I,4) =(OGPRSUP(IY) + OGQNGREP(1,IY) + OGQNGREP(2,IY) + &
                                   OGQNGREP(3,IY) + OGQNGREP(4,IY) + &
                                   OGQNGREP(5,IY) + OGQNGREP(6,IY) + &
                                   OGQNGREP(7,IY) + OGQNGREP(8,IY) + OGSHALENG(IY)) * &
                    .001 * CFNGC(IY)* MTOECONV
      I120( 2,I,4) = NGIMPVOL(4,IY) * CFNGI(IY)*.001 * MTOECONV
      I120( 3,I,4) = NGEXPVOL(4,IY) * CFNGE(IY)*.001 * MTOECONV * (-1.0)
      I120( 4,I,4) = (-1.)*(TRQINTS(3,IY) +TRQINTS(4,IY)) * .001 * MTOECONV
      I120( 6,I,4) = (-1.) * MTOECONV * (QNGEL(11,IY) + &
!           (CGNTQ(mnumnr,IY,3) + CGRESQ(11,IY,3) + &
            (CGREFQ(11,IY,3) + CGOGSQ(11,IY,3) + CGINDLQ(11,IY,3) + &
             CGCOMMQ(11,IY,3))*.001 + (CMUSDISTSERV(1,2,IY) + &
             CMUSDISTSERV(2,2,IY) + CMUSDISTSERV(3,2,IY)))
      I120( 7,I,4) = 0.0
      I120( 8,I,4) = (-1.) * MTOECONV * (QLPIN(11,IY)+QNGLQ(11,IY) + QGTLRF(11,IY))
      I120(10,I,4) = (QNGIN(11,IY) - QGTLRF(11,IY) - &
               (CGREFQ(11,IY,3) + CGOGSQ(11,IY,3) + CGINDLQ(11,IY,3)) * .001) * MTOECONV + &
                GTLFRAC(1,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(1,IY) * MTOECONV
      I120(11,I,4) = MTOECONV * .001 * INQNGPF(11,IY)
      I120(12,I,4) =(QNGTR(11,IY) + QGPTR(11,IY))* MTOECONV + &
                       sum(GTLFRAC(1:4,MNUMPR,IY))/1000. * RDAYS * .001 * CFGTLLIQ(IY) * MTOECONV - &
                       GTLFRAC(1,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(1,IY) * MTOECONV - &
                     (TRQINTS(3,IY) +TRQINTS(4,IY)) * .001 * MTOECONV
      I120(13,I,4) = QNGTR(11,IY) * MTOECONV + &
                       sum(GTLFRAC(1:4,MNUMPR,IY))/1000. * RDAYS * .001 * CFGTLLIQ(IY) * MTOECONV - &
                       GTLFRAC(1,MNUMPR,IY)/1000. * RDAYS * .001 * CFFTLIQ(1,IY) * MTOECONV - &
                     (TRQINTS(3,IY) +TRQINTS(4,IY)) * .001 * MTOECONV
      I120(14,I,4) = MTOECONV * (QNGRS(11,IY) + QNGCM(11,IY) - CGCOMMQ(11,IY,3) *.001 - &
            (CMUSDISTSERV(1,2,IY) + CMUSDISTSERV(2,2,IY) + CMUSDISTSERV(3,2,IY)))
      I120(15,I,4) = QNGRS(11,IY) * MTOECONV
      I120(16,I,4) = 0.0
!  move petrochemical feedstocks from Industrial to Non-Energy
      I120(10,I,4) = I120(10,I,4) - I120(11,I,4)
      I120(16,I,4) = I120(16,I,4) + I120(11,I,4)
      I120( 9,I,4) = I120(10,I,4) + I120(12,I,4) + I120(14,I,4) + I120(16,I,4) - &
                     FSUM(I120( 1,I,4),8)
      I120(17,I,4) =(UGNGFNR(1,mnumnr,IY) + UGNGFNR(2,mnumnr,IY) + &
                     UGNGINR(1,mnumnr,IY) + UGNGINR(2,mnumnr,IY) + &
                     UGNGCNR(1,mnumnr,IY) + UGNGCNR(2,mnumnr,IY) + &
            (CGNTGEN(mnumnr,IY,3,1)  + CGNTGEN(mnumnr,IY,3,2) + &
             CGINDLGEN(11,IY,3,1) + CGINDLGEN(11,IY,3,2) + &
             CGREFGEN(11,IY,3,1) + CGREFGEN(11,IY,3,2)  + &
             CGOGSGEN(11,IY,3,1) + CGOGSGEN(11,IY,3,2)  + &
             CGCOMMGEN(11,IY,3,1) + CGCOMMGEN(11,IY,3,2)) * .001) * IEAGROSS
! Keeping a constant 57% calculated efficiency for row 18
      I120(18,I,4) = (-.57 * I120(6,I,4) - I120(17,I,4) * .0859845228) / .02388
        I120(18,I,4) = (CGCOMMQ(11,IY,3)+CGINDLQ(11,IY,3) + &
            CGNTQ(mnumnr,IY,3) + CGREFQ(11,IY,3) + &
            CGOGSQ(11,IY,3)) * .001 * .10 * MTOECONV * TOE_PJ

! U.S. Nuclear

      IEAGROSS = 1.06
!  Row 1:  nuclear production/consumption calculated based on IEA required 33% efficiency
      I120( 1,I,5) = (UGNURNR(1,mnumnr,IY) + UGNURNR(2,mnumnr,IY)) * 0.0859845228 / .33 * IEAGROSS
!  Row 2 is the actual nuclear consumption/production from NEMS.
      I120( 2,I,5) = QUREL(11,IY) * MTOECONV * IEAGROSS
      I120( 6,I,5) = (-1.) * (UGNURNR(1,mnumnr,IY) + UGNURNR(2,mnumnr,IY)) * .0859845228 / .33*IEAGROSS
      I120(17,I,5) = (UGNURNR(1,mnumnr,IY) + UGNURNR(2,mnumnr,IY)) * IEAGROSS

! U.S. Hydro

      IEAGROSS = 1.01
!  Row 1:  hydro production/consumption calculated based on IEA required 100% efficiency
      I120( 1,I,6) =((UGNHYNR(1,mnumnr,IY) + UGNHYNR(2,mnumnr,IY))*.0859845228 + &
          (CGNTGEN(mnumnr,IY,4,1)+CGNTGEN(mnumnr,IY,4,2) + &
           CGCOMMGEN(11,IY,4,1) + CGCOMMGEN(11,IY,4,2) + &
           CGINDLGEN(11,IY,4,1)+CGINDLGEN(11,IY,4,2))*.001 * .0859845228)* IEAGROSS
!  Row 2 is the actual hydro consumption/production from NEMS.
      I120( 2,I,6) = QHOAS(11,IY) * MTOECONV * IEAGROSS
      I120( 6,I,6) =((UGNHYNR(1,mnumnr,IY) + UGNHYNR(2,mnumnr,IY))*.0859845228)* IEAGROSS * (-1) + &
                   ((CGNTGEN(mnumnr,IY,4,1)+CGNTGEN(mnumnr,IY,4,2) + &
         CGCOMMGEN(11,IY,4,1) + CGCOMMGEN(11,IY,4,2) + &
         CGINDLGEN(11,IY,4,1)+CGINDLGEN(11,IY,4,2))*.001 *.0859845228*(-1.))* IEAGROSS
      I120(17,I,6) = (UGNHYNR(1,mnumnr,IY) + UGNHYNR(2,mnumnr,IY) + &
          (CGNTGEN(mnumnr,IY,4,1)+CGNTGEN(mnumnr,IY,4,2) + &
           CGCOMMGEN(11,IY,4,1) + CGCOMMGEN(11,IY,4,2) + &
           CGINDLGEN(11,IY,4,1) + CGINDLGEN(11,IY,4,2))*.001 )* IEAGROSS

! U.S. Wind

      IEAGROSS = 1.01
!  Row 1:  wind production/consumption calculated based on IEA required 100% efficiency
      I120( 1,I,7) =((UGNWFNR(1,mnumnr,IY) + UGNWFNR(2,mnumnr,IY) + &
                      UGNWNNR(1,mnumnr,IY) + UGNWNNR(2,mnumnr,IY) + &
                      UGNWLNR(1,mnumnr,IY) + UGNWLNR(2,mnumnr,IY) + &
                  (CGRESGEN(11,IY,11,1)  + CGRESGEN(11,IY,11,2) + &
                   CGINDLGEN(11,IY,11,1) + CGINDLGEN(11,IY,11,2) + &
                   CGCOMMGEN(11,IY,11,1) + CGCOMMGEN(11,IY,11,2))*.001) * .0859845228 + &
                     (QWIIN(11,IY) * MTOECONV)) * IEAGROSS
!  Row 2 is the actual wind consumption/production from NEMS.
      I120( 2,I,7) = QWIEL(11,IY) * MTOECONV * IEAGROSS
      I120( 6,I,7) =(UGNWFNR(1,mnumnr,IY) + UGNWFNR(2,mnumnr,IY) + &
                     UGNWNNR(1,mnumnr,IY) + UGNWNNR(2,mnumnr,IY) + &
                     UGNWLNR(1,mnumnr,IY) + UGNWLNR(2,mnumnr,IY) + &
                  (CGRESGEN(11,IY,11,1)  + CGRESGEN(11,IY,11,2) + &
                   CGINDLGEN(11,IY,11,1) + CGINDLGEN(11,IY,11,2) + &
                   CGCOMMGEN(11,IY,11,1) + CGCOMMGEN(11,IY,11,2))*.001) * .0859845228 * IEAGROSS * (-1.)
      I120(10,I,7) = QWIIN(11,IY) * MTOECONV * IEAGROSS
      I120(14,I,7) = 0.0
      I120(15,I,7) = 0.0
      I120(17,I,7) =(UGNWFNR(1,mnumnr,IY) + UGNWFNR(2,mnumnr,IY) + &
                     UGNWNNR(1,mnumnr,IY) + UGNWNNR(2,mnumnr,IY) + &
                     UGNWLNR(1,mnumnr,IY) + UGNWLNR(2,mnumnr,IY) + &
                      (CGRESGEN(11,IY,11,1)  + CGRESGEN(11,IY,11,2) + &
                       CGINDLGEN(11,IY,11,1) + CGINDLGEN(11,IY,11,2) + &
                       CGCOMMGEN(11,IY,11,1) + CGCOMMGEN(11,IY,11,2)) * .001) * IEAGROSS

! U.S. Geothermal

      IEAGROSS = 1.06
!  Row 1:  geothermal production/consumption calculated based on IEA required 10% efficiency
      I120( 1,I,8) =((UGNGENR(1,mnumnr,IY) + UGNGENR(2,mnumnr,IY) + &
                     (CGNTGEN(mnumnr,IY,5,1)  + CGNTGEN(mnumnr,IY,5,2) + &
                      CGINDLGEN(11,IY,5,1) + CGINDLGEN(11,IY,5,2) + &
                  sum(CGRESGEN(11,IY,5,1:2)) + &
                  sum(CGCOMMGEN(11,IY,5,1:2))) * .001) * .86 + &
                     (QGEIN(11,IY) + QGERS(11,IY) - &
                     (CGINDLQ(11,IY,5) + CGRESQ(11,IY,5)) * .001) * MTOECONV) * IEAGROSS
!  Row 2 is the actual geothermal consumption from NEMS equivalent to row 6 below
                              !  (which sets a 10% efficiency (by 10 * .0859845228 = .86)
      I120( 2,I,8) =(QGEEL(11,IY) + CGINDLQ(11,IY,5)/1000. + CGCOMMQ(11,IY,5) /1000. + &
                     CGNTQ(11,IY,5) /1000. + CGRESQ(11,IY,5) /1000.) * MTOECONV * IEAGROSS * (-1)
      I120( 6,I,8) = (UGNGENR(1,mnumnr,IY) + UGNGENR(2,mnumnr,IY) + &
                     (CGNTGEN(mnumnr,IY,5,1)  + CGNTGEN(mnumnr,IY,5,2) + &
                      CGINDLGEN(11,IY,5,1) + CGINDLGEN(11,IY,5,2) + &
                  sum(CGRESGEN(11,IY,5,1:2)) + &
                  sum(CGCOMMGEN(11,IY,5,1:2))) * .001 ) *.86 * IEAGROSS * (-1.)
      I120(10,I,8) = (QGEIN(11,IY) - CGINDLQ(11,IY,5)/1000.) * MTOECONV * IEAGROSS
      I120(14,I,8) = (QGERS(11,IY) - CGRESQ(11,IY,5) * .001) * MTOECONV * IEAGROSS
      I120(15,I,8) = (QGERS(11,IY) - CGRESQ(11,IY,5) * .001) * MTOECONV * IEAGROSS
      I120(17,I,8) = (UGNGENR(1,mnumnr,IY) + UGNGENR(2,mnumnr,IY) + &
                     (CGNTGEN(mnumnr,IY,5,1) + CGNTGEN(mnumnr,IY,5,2) + &
                      CGCOMMGEN(11,IY,5,1) + CGCOMMGEN(11,IY,5,2) + &
                      CGRESGEN(11,IY,5,1)  + CGRESGEN(11,IY,5,2) + &
                      CGINDLGEN(11,IY,5,1) + CGINDLGEN(11,IY,5,2)) * .001 ) * IEAGROSS

! U.S. Solar

      IEAGROSS = 1.01
!  Row 1:  solar production/consumption calculated based on IEA required 100% efficiency
      I120( 1,I,9) =((UGNSONR(1,mnumnr,IY) + UGNSONR(2,mnumnr,IY) + &
                    UGNPVNR(1,mnumnr,IY) + UGNPVNR(2,mnumnr,IY) + &
                      UGNPTNR(1,mnumnr,IY) + UGNPTNR(2,mnumnr,IY) + &
                  (CGNTGEN(mnumnr,IY,8,1)  + CGNTGEN(mnumnr,IY,8,2) + &
                   CGINDLGEN(11,IY,8,1) + CGINDLGEN(11,IY,8,2) + &
                   CGINDLGEN(11,IY,12,1) + CGINDLGEN(11,IY,12,2) + &
                   CGCOMMGEN(11,IY,8,1) + CGCOMMGEN(11,IY,12,1) + &
                   CGCOMMGEN(11,IY,8,2) + CGCOMMGEN(11,IY,12,2) + &
                   sum(CGRESGEN(11,IY,8,1:2))+sum(CGRESGEN(11,IY,12,1:2)))*.001) * .0859845228 + &
                  (QSTRS(11,IY) + QSTCM(11,IY) + QPVRS(11,IY) + QPVCM(11,IY) - &
                  (CGRESQ(11,IY, 8) + CGCOMMQ(11,IY, 8) + &
                   CGRESQ(11,IY,12) + CGCOMMQ(11,IY,12)) * .001) * MTOECONV) * IEAGROSS
!  Row 2 is the actual solar consumption/production from NEMS.
      I120( 2,I,9) = (QSTEL(11,IY) + QPVEL(11,IY)) * MTOECONV * IEAGROSS
      I120( 6,I,9) =(UGNSONR(1,mnumnr,IY) + UGNSONR(2,mnumnr,IY) + &
                    UGNPVNR(1,mnumnr,IY) + UGNPVNR(2,mnumnr,IY) + &
                    UGNPTNR(1,mnumnr,IY) + UGNPTNR(2,mnumnr,IY) + &
                  (CGNTGEN(mnumnr,IY,8,1)  + CGNTGEN(mnumnr,IY,8,2) + &
                   CGINDLGEN(11,IY,8,1) + CGINDLGEN(11,IY,8,2) + &
                   CGINDLGEN(11,IY,12,1) + CGINDLGEN(11,IY,12,2) + &
                   sum(CGRESGEN(11,IY,8,1:2)) + sum(CGRESGEN(11,IY,12,1:2)) + &
                   CGCOMMGEN(11,IY,8,1)+CGCOMMGEN(11,IY,12,1) + &
                   CGCOMMGEN(11,IY,8,2)+CGCOMMGEN(11,IY,12,2))*.001) * .0859845228 * IEAGROSS * (-1.)
      I120(10,I,9) = 0.0
      I120(14,I,9) = (QSTRS(11,IY) + QSTCM(11,IY) + QPVRS(11,IY) + QPVCM(11,IY) - &
                     (CGRESQ(11,IY,8) + CGCOMMQ(11,IY,8) + &
                      CGRESQ(11,IY,12) + CGCOMMQ(11,IY,12)) * .001) * MTOECONV * IEAGROSS
      I120(15,I,9) = (QSTRS(11,IY) + QPVRS(11,IY) - &
                      CGRESQ(11,IY,8) * .001 - CGRESQ(11,IY,12) * .001) * MTOECONV * IEAGROSS
      I120(17,I,9) =(UGNSONR(1,mnumnr,IY) + UGNSONR(2,mnumnr,IY) + &
                       UGNPVNR(1,mnumnr,IY) + UGNPVNR(2,mnumnr,IY) + &
                       UGNPTNR(1,mnumnr,IY) + UGNPTNR(2,mnumnr,IY) + &
                      (CGNTGEN(mnumnr,IY,8,1)  + CGNTGEN(mnumnr,IY,8,2) + &
                       CGINDLGEN(11,IY,8,1) + CGINDLGEN(11,IY,8,2) + &
                       CGINDLGEN(11,IY,12,1) + CGINDLGEN(11,IY,12,2) + &
                       sum(CGRESGEN(11,IY,8,1:2)) + sum(CGRESGEN(11,IY,12,1:2)) + &
                       CGCOMMGEN(11,IY,8,1)+CGCOMMGEN(11,IY,12,1) + &
                       CGCOMMGEN(11,IY,8,2)+CGCOMMGEN(11,IY,12,2)) * .001) * IEAGROSS

! U.S. Electric

       IEAGROSS = 1.07
       I120( 2,I,13) = (UTIMPF(mnumnr,IY) +UTIMPE(mnumnr,IY))*.0859845228*.001
       I120( 3,I,13) = (UTEXPF(mnumnr,IY) +UTEXPE(mnumnr,IY))*.0859845228*.001*(-1.)
       I120( 6,I,13) = (I120(17,I,1) + I120(17,I,3) + I120(17,I,4) + &
                        I120(17,I,5) + I120(17,I,6) + I120(17,I,7) + &
                        I120(17,I,8) + I120(17,I,9) + I120(17,I,11)) * .0859845228
       I120( 8,I,13) = (-1.) * (FSUM(I120( 1,I,13),6) - (QELAS(11,IY) * MTOECONV))
       I120(10,I,13) = QELIN(11,IY) * MTOECONV
       I120(12,I,13) = QELTR(11,IY) * MTOECONV
       I120(13,I,13) = TRQLDV(6,11,IY) * .001 * MTOECONV
       I120(14,I,13) = FSUM(I120(1,I,13),8) - I120(10,I,13) - I120(12,I,13)
       IF ((QELCM(11,IY)+QELRS(11,IY)) .NE. (0.0)) &
         I120(15,I,13) = I120(14,I,13) * (QELRS(11,IY) / (QELRS(11,IY) + QELCM(11,IY)))
       I120(15,I,13) = QELRS(11,IY) * MTOECONV
       I120( 9,I,13) = FSUM(I120( 1,I,13),8) - I120(10,I,13) - I120(12,I,13) - I120(14,I,13)

! U.S. Heat

       I120( 1,I,14) = (((RSHTRCON(IY,7) + RSCOOLCN(IY,2) + RSH2OCON(IY,5)) / &
             1000000000) + QSTCM(11,IY) + QPVCM(11,IY)) * MTOECONV
!        --- .02388 OFF IEA WORKSHEET FOR PJ TO MTOE CHECK
       I120( 6,I,14) = (I120(18,I,1) + I120(18,I,11) + &
                        I120(18,I,3) + I120(18,I,4)) * .02388
       IF ((QCLAS(11,IY) + QMCIN(11,IY)+QCIIN(11,IY)) .NE. 0.0) &
       I120( 8,I,14) = (-1.) * MTOECONV * ((((RSHTRCON(IY,7) + RSCOOLCN(IY,2) + &
             RSH2OCON(IY,5)) / 1000000000) + QSTCM(11,IY) + QPVCM(11,IY)) + &
           ((QCLCM(11,IY)-((CGCOMMGEN(11,IY,1,1)+CGCOMMGEN(11,IY,1,2))* &
             .0859845228*.001)/23.81)/ (QCLAS(11,IY) + QMCIN(11,IY)+QCIIN(11,IY))) * &
             FSUM(I120(1,I,1),5) / MTOECONV + (CMUSDISTSERV(1,3,IY) + &
               CMUSDISTSERV(2,3,IY) + CMUSDISTSERV(3,3,IY) + CMUSDISTSERV(1,2,IY) + &
               CMUSDISTSERV(2,2,IY) + CMUSDISTSERV(3,2,IY)) * .35)
       I120(14,I,14) = (((RSHTRCON(IY,7) + RSCOOLCN(IY,2) + RSH2OCON(IY,5)) / &
               1000000000) + QSTCM(11,IY) + QPVCM(11,IY)) * MTOECONV
       I120(15,I,14) = ((RSHTRCON(IY,7) + RSCOOLCN(IY,2) + RSH2OCON(IY,5)) / &
               1000000000) * MTOECONV
       I120(10,I,14) = FSUM(I120(1,I,14),8) - I120(12,I,14) - I120(14,I,14) - I120(16,I,14)


! U.S. Total

         DO III = 1,NTAB120
           I120(III,I,15)= I120(III,I, 1) + I120(III,I, 2) + I120(III,I, 3) + &
                           I120(III,I, 4) + I120(III,I, 5) + I120(III,I, 6) + &
                           I120(III,I, 7) + I120(III,I, 8) + I120(III,I, 9) + &
                           I120(III,I,11) + I120(III,I,12) + I120(III,I,13) + &
						   I120(III,I,14)
         ENDDO
		 
!TEST ADD CCS
		!I120(19,I,16) = EMCARBON(5,28,IY)+SUM(CCS_PMM(1:4,10,IY))*1000
		I120(19,I,16) = ((EMCARBON(5,28,IY)+SUM(CCS_PMM(1:4,10,IY)))+ &
						((SUM(OGCO2PUR(8,1:13,IY))-OGCO2PUR(8,4,IY))/18000)- &
						(OGCO2PUR(8,7,IY)/18000))*1000
		I120(20,I,16) = (OGCO2PUR(8,9,IY) / 18000)*1000
		I120(21,I,16) = (SUM(OGCO2PUR(8,1:3,IY)/18000)+SUM(OGCO2PUR(8,5:6,IY)/18000))*1000
		I120(22,I,16) = (SUM(EMCARBON(1:2,28,IY))+EMCARBON(4,28,IY) +EMCARBON(3,28,IY))*1000
		I120(23,I,16) = (OGCO2PUR(8,6,IY)/18000)*1000
		I120(24,I,16) = (OGCO2PUR(8,11,IY)/18000)*1000
		

      ENDDO
	  
	  

	  
      OPEN(UNIT=92,FILE='ieatable.csv',ERR=999)
      DO IY = 1,FTIEAYRS
        CONTINUE120=.TRUE.
        COUNT=0
        WRITE(92,'(//)')
        DO III=1,NHED(120)
          WRITE(92,'(a,a,a)') '"',trim(TABHED(III,120)),'"'
        ENDDO
        WRITE(92,'(/)')
        WRITE(92,'(I4,a,a)') &
              IYEAR(IY),',"Coal","Peat","Oil","Gas","Nuclear","Hydro","Wind","Geotherm",', &
              '"Solar","Tide","Renew Combust","Non Rewnew Combust","Electric","Heat","U.S. Total","CCS"'
        TOTALROWS=1
        DO WHILE (CONTINUE120)
          TOTALROWS=TOTALROWS+1
          IF (IROWS(TOTALROWS,120) .EQ. 0) THEN
            WRITE(92,'(a,a,a)') '"',ROWLAB(TOTALROWS,120),'"'
          ELSE IF (IROWS(TOTALROWS,120) .GT. 0) THEN
            COUNT=COUNT+1
            IF (COUNT .EQ. NTAB120) CONTINUE120 = .FALSE.
            WRITE(92,'(a,a,a,16(F14.6,a))') '"',trim(ROWLAB(TOTALROWS,120)),'",', &
                      ((I120(IROWS(TOTALROWS,120),IY,I),','),I=1,16)
          ENDIF
        ENDDO
      ENDDO
      CLOSE(UNIT=92)

      IF (ANY(REPORT_SWITCH(1:MAXTAB))) THEN
        RETURN
      ELSE
        STOP 53
      ENDIF

 999  WRITE(6,*) '  open failed for iea table text file'
      RETURN
      END

      SUBROUTINE DOT118(IS)
      IMPLICIT NONE
! Table 118 needs to use both the adjusted and unadjusted fuel prices,
! so it is done in its own subroutine to allow this. -- jbj ---

!  THIS INCLUDE DEFINES THE PRICE AND QUANTITY VARIABLES
      include 'parametr'
      include 'qblk'
      include 'mpblk'         ! unadjusted prices
      include 'epmmpblk'      ! adjusted prices
      include 'emmparm'
      include 'emission'
      include 'ncntrl'
      include 'macout'
      include 'epmbank'
      include 'ftable'
      include 'ghgrep'
      include 'uefdout'
      include 'indout'
      include 'ngtdmout'
      include 'ponroad'
      include 'qonroad'
      include 'convfact'
      include 'ab32'
      include 'rggi'

      REAL*4 FSUM
      EXTERNAL FSUM

      REAL DENOM,COVERED_OGHG,UNCOVERED_OGHG,BANK
      real allow_per_offset/.8/
      INTEGER I,IR,IY,IS,ighg,ICY,itapstart

! TABLE 119.   Unadjusted price version of Table 3.

      DO IY=1,LASTYR
      DO IR=1,11
          IF (IR .EQ. 10) CYCLE
! RESIDENTIAL
      IF ((QTPRS(IR,IY)+QNGRS(IR,IY)+QCLRS(IR,IY)+QELRS(IR,IY)) .NE. 0.0) &
          T119(1,IR,IY,IS)= &
            (PTPRS(IR,IY)*QTPRS(IR,IY)+PNGRS(IR,IY)*QNGRS(IR,IY)+ &
             PCLRS(IR,IY)*QCLRS(IR,IY)+PELRS(IR,IY)*QELRS(IR,IY)) / &
            (QTPRS(IR,IY)+QNGRS(IR,IY)+QCLRS(IR,IY)+QELRS(IR,IY))
      IF ((QTPRS(IR,IY)+QNGRS(IR,IY)+QCLRS(IR,IY)) .NE. 0.0) &
          T119(2,IR,IY,IS)= &
            (PTPRS(IR,IY)*QTPRS(IR,IY)+PNGRS(IR,IY)*QNGRS(IR,IY)+ &
             PCLRS(IR,IY)*QCLRS(IR,IY)) / &
            (QTPRS(IR,IY)+QNGRS(IR,IY)+QCLRS(IR,IY))
          T119(3,IR,IY,IS)=PTPRS(IR,IY)
          T119(4,IR,IY,IS)=PDSRS(IR,IY)
          T119(5,IR,IY,IS)=PPRRS(IR,IY)
          T119(6,IR,IY,IS)=PNGRS(IR,IY)
          T119(7,IR,IY,IS)=PELRS(IR,IY)
! COMMERCIAL
      IF ((QTPCM(IR,IY)+QNGCM(IR,IY)+QCLCM(IR,IY)+QELCM(IR,IY)) .NE. 0.0) &
          T119( 8,IR,IY,IS)= &
            (PTPCM(IR,IY)*QTPCM(IR,IY)+PNGCM(IR,IY)*QNGCM(IR,IY)+ &
             PCLCM(IR,IY)*QCLCM(IR,IY)+PELCM(IR,IY)*QELCM(IR,IY)) / &
            (QTPCM(IR,IY)+QNGCM(IR,IY)+QCLCM(IR,IY)+QELCM(IR,IY))
      IF ((QTPCM(IR,IY)+QNGCM(IR,IY)+QCLCM(IR,IY)) .NE. 0.0) &
          T119( 9,IR,IY,IS)= &
            (PTPCM(IR,IY)*QTPCM(IR,IY)+PNGCM(IR,IY)*QNGCM(IR,IY)+ &
             PCLCM(IR,IY)*QCLCM(IR,IY)) / &
            (QTPCM(IR,IY)+QNGCM(IR,IY)+QCLCM(IR,IY))
          T119(10,IR,IY,IS)=PTPCM(IR,IY)
          T119(11,IR,IY,IS)=PDSCM(IR,IY)
          T119(12,IR,IY,IS)=PRSCM(IR,IY)
          T119(13,IR,IY,IS)=PNGCM(IR,IY)
          T119(14,IR,IY,IS)=PELCM(IR,IY)
! INDUSTRIAL (PRICES AND QUANTITIES INCLUDE REFINERY)
      IF((QTPIN(IR,IY)+QNGIN(IR,IY)+QCLIN(IR,IY)+QELIN(IR,IY)+ &
          QMCIN(IR,IY)) .NE. 0.0)          T119(15,IR,IY,IS)= &
            (PTPIN(IR,IY)*QTPIN(IR,IY)+PNGIN(IR,IY)*QNGIN(IR,IY)+ &
             PCLIN(IR,IY)*(QCLIN(IR,IY)-QCTLRF(IR,IY))+PELIN(IR,IY)*QELIN(IR,IY)+ &
             PCLSN(IR,IY)*QCTLRF(IR,IY)+ &
             PMCIN(IR,IY)*QMCIN(IR,IY)) / &
            (QTPIN(IR,IY)+QNGIN(IR,IY)+QCLIN(IR,IY)+QELIN(IR,IY)+QMCIN(IR,IY))
      IF((QTPIN(IR,IY)+QNGIN(IR,IY)+QCLIN(IR,IY)+QMCIN(IR,IY)) .NE. 0.0) &
          T119(16,IR,IY,IS)= &
            (PTPIN(IR,IY)*QTPIN(IR,IY)+PNGIN(IR,IY)*QNGIN(IR,IY)+ &
             PCLIN(IR,IY)*(QCLIN(IR,IY)-QCTLRF(IR,IY))+ &
             PCLSN(IR,IY)*QCTLRF(IR,IY)+ &
             PMCIN(IR,IY)*QMCIN(IR,IY)) / &
            (QTPIN(IR,IY)+QNGIN(IR,IY)+QCLIN(IR,IY)+QMCIN(IR,IY))
          T119(17,IR,IY,IS)=PTPIN(IR,IY)
          T119(18,IR,IY,IS)=PDSIN(IR,IY)
! do this if we want to average in feedstock prices:
          IF(QLGIN(IR,IY) .NE. 0.0) &
          T119(19,IR,IY,IS)=(PLGIN(IR,IY)*(QLGIN(IR,IY)-INQLGPF(IR,IY)/1000.) + &
                             PLGINPF(IR,IY)*INQLGPF(IR,IY)/1000.) / QLGIN(IR,IY)
! otherwise do this:
          T119(19,IR,IY,IS)=PLGIN(IR,IY)
! but it is simpler (the row in now labelled "propane") if we just use the propane price variable:
          T119(19,IR,IY,IS)=PPRIN(IR,IY)
          T119(20,IR,IY,IS)=PRSIN(IR,IY)
          T119(21,IR,IY,IS)=PNGIN(IR,IY)
          T119(22,IR,IY,IS)=PMCIN(IR,IY)
          T119(23,IR,IY,IS)=PCLIN(IR,IY)
          T119(65,IR,IY,IS)=PCLSN(IR,IY)
          T119(24,IR,IY,IS)=PELIN(IR,IY)
! TRANSPORTATION
       IF ((QTPTR(IR,IY)-QDSBS(IR,IY)-QJFBS(IR,IY)-QMGBS(IR,IY)+QNGTR(IR,IY)+QELTR(IR,IY)+QETTR(IR,IY)).NE. 0.0) &
          T119(25,IR,IY,IS)= &
            (PTPTR(IR,IY)*(QTPTR(IR,IY)-QDSBS(IR,IY)-QJFBS(IR,IY)-QMGBS(IR,IY))+ &
             PNGTR(IR,IY)*QNGTR(IR,IY)+ &
             PELTR(IR,IY)*QELTR(IR,IY)+PETTR(IR,IY)*QTRTR(IR,IY))/ &
            (QTPTR(IR,IY)-QDSBS(IR,IY)-QJFBS(IR,IY)-QMGBS(IR,IY)+ &
             QNGTR(IR,IY)+QELTR(IR,IY)+QETTR(IR,IY))
       IF ((QTPTR(IR,IY)-QDSBS(IR,IY)-QJFBS(IR,IY)-QMGBS(IR,IY)+QNGTR(IR,IY)+QETTR(IR,IY)) .NE. 0.0) &
          T119(26,IR,IY,IS)= &
            (PTPTR(IR,IY)*(QTPTR(IR,IY)-QDSBS(IR,IY)-QJFBS(IR,IY)-QMGBS(IR,IY))+ &
             PNGTR(IR,IY)*QNGTR(IR,IY)+ &
             PETTR(IR,IY)*QETTR(IR,IY))/ &
            (QTPTR(IR,IY)-QDSBS(IR,IY)-QJFBS(IR,IY)-QMGBS(IR,IY)+QNGTR(IR,IY)+QETTR(IR,IY))
       IF ((QTPTR(IR,IY)-QDSBS(IR,IY)-QJFBS(IR,IY)-QMGBS(IR,IY)) .NE. 0.0) &
          T119(27,IR,IY,IS)=(PTPTR(IR,IY)*QTPTR(IR,IY)-PDSTR(IR,IY)*QDSBS(IR,IY)- &
                           PJFTR(IR,IY)*QJFBS(IR,IY)-PMGTR(IR,IY)*QMGBS(IR,IY))/ &
                          (QTPTR(IR,IY)-QDSBS(IR,IY)-QJFBS(IR,IY)-QMGBS(IR,IY))
          T119(28,IR,IY,IS)=PDSTRHWY(IR,IY)
          T119(29,IR,IY,IS)=PJFTR(IR,IY)
          T119(30,IR,IY,IS)=PMGTR(IR,IY)
          T119(31,IR,IY,IS)=PRSTR(IR,IY)
          T119(32,IR,IY,IS)=PPRTR(IR,IY)
          T119(33,IR,IY,IS)=PNGTR(IR,IY)
          T119(34,IR,IY,IS)=PETTR(IR,IY)
          T119(35,IR,IY,IS)=PMETR(IR,IY)
          T119(36,IR,IY,IS)=PELTR(IR,IY)
!  Total End-Use Energy
        DENOM = (QTPRS(IR,IY)+QNGRS(IR,IY)+QCLRS(IR,IY)+QELRS(IR,IY)+ &
         QTPCM(IR,IY)+QNGCM(IR,IY)+QCLCM(IR,IY)+QELCM(IR,IY)+ &
         QTPIN(IR,IY)+QNGIN(IR,IY)+QCLIN(IR,IY)+QELIN(IR,IY)+ &
         QMCIN(IR,IY)+QTPTR(IR,IY)+QNGTR(IR,IY)+QELTR(IR,IY)+QETTR(IR,IY))
        IF (DENOM .NE. 0.0) T119(37,IR,IY,IS)= &
         (T119( 1,IR,IY,IS)*(QTPRS(IR,IY)+QNGRS(IR,IY)+QCLRS(IR,IY)+QELRS(IR,IY))+ &
          T119( 8,IR,IY,IS)*(QTPCM(IR,IY)+QNGCM(IR,IY)+QCLCM(IR,IY)+QELCM(IR,IY))+ &
          T119(15,IR,IY,IS)*(QTPIN(IR,IY)+QNGIN(IR,IY)+QCLIN(IR,IY)+QELIN(IR,IY)+QMCIN(IR,IY))+ &
          T119(25,IR,IY,IS)*(QTPTR(IR,IY)+QNGTR(IR,IY)+QELTR(IR,IY)+QETTR(IR,IY)))/ DENOM
        DENOM = (QTPRS(IR,IY)+QNGRS(IR,IY)+QCLRS(IR,IY)+ &
                 QTPCM(IR,IY)+QNGCM(IR,IY)+QCLCM(IR,IY)+ &
                 QTPIN(IR,IY)+QNGIN(IR,IY)+QCLIN(IR,IY)+QMCIN(IR,IY)+ &
                 QTPTR(IR,IY)+QNGTR(IR,IY)+QETTR(IR,IY))
       IF (DENOM .NE. 0.0) T119(38,IR,IY,IS)= &
         (T119( 2,IR,IY,IS)*(QTPRS(IR,IY)+QNGRS(IR,IY)+QCLRS(IR,IY))+ &
          T119( 9,IR,IY,IS)*(QTPCM(IR,IY)+QNGCM(IR,IY)+QCLCM(IR,IY))+ &
          T119(16,IR,IY,IS)*(QTPIN(IR,IY)+QNGIN(IR,IY)+QCLIN(IR,IY)+ QMCIN(IR,IY))+ &
          T119(26,IR,IY,IS)*(QTPTR(IR,IY)+QNGTR(IR,IY)+QETTR(IR,IY)))/ DENOM
          T119(39,IR,IY,IS)=PELAS(IR,IY)
!  ELEC UTILITIES
          IF((QTPEL(IR,IY)+QNGEL(IR,IY)+QCLEL(IR,IY)) .NE. 0.0) &
          T119(40,IR,IY,IS)=(PTPEL(IR,IY)*QTPEL(IR,IY)+ &
            PNGEL(IR,IY)*QNGEL(IR,IY)+PCLEL(IR,IY)*QCLEL(IR,IY)) / &
           (QTPEL(IR,IY)+QNGEL(IR,IY)+QCLEL(IR,IY))
          T119(41,IR,IY,IS)=PTPEL(IR,IY)
          T119(42,IR,IY,IS)=PDSEL(IR,IY)
          T119(43,IR,IY,IS)=PRSEL(IR,IY)
          T119(44,IR,IY,IS)=PNGEL(IR,IY)
          T119(45,IR,IY,IS)=PCLEL(IR,IY)
          T119(64,IR,IY,IS)=UPRWDCR(IR,IY)
!  ALL USERS
          T119(46,IR,IY,IS)=PTPAS(IR,IY)
          T119(47,IR,IY,IS)=PDSAS(IR,IY)
          T119(48,IR,IY,IS)=PJFTR(IR,IY)
! this has the feedstock price averaged in:
          T119(49,IR,IY,IS)=PLGAS(IR,IY)
! calculated based on the sectoral prices shown (as stated in the footnote):
          IF (QPRRS(IR,IY)+QPRCM(IR,IY)+QPRIN(IR,IY)-QPRINPF(IR,IY)+QPRTR(IR,IY) .NE. 0.0) &
          T119(49,IR,IY,IS)= &
             (QPRRS(IR,IY)*PPRRS(IR,IY)+QPRCM(IR,IY)*PPRCM(IR,IY)+ &
             (QPRIN(IR,IY)-QPRINPF(IR,IY))*PPRIN(IR,IY)+QPRTR(IR,IY)*PPRTR(IR,IY)) / &
             (QPRRS(IR,IY)+QPRCM(IR,IY)+QPRIN(IR,IY)-QPRINPF(IR,IY)+QPRTR(IR,IY))
          T119(50,IR,IY,IS)=PMGAS(IR,IY)
          T119(51,IR,IY,IS)=PRSAS(IR,IY)
          T119(52,IR,IY,IS)=PNGAS(IR,IY)
          T119(53,IR,IY,IS)=PCLAS(IR,IY)
          T119(54,IR,IY,IS)=PETTR(IR,IY)
          T119(55,IR,IY,IS)=PMETR(IR,IY)
          T119(56,IR,IY,IS)=PELAS(IR,IY)
!         --- EXPENDITURES BY SECTOR
          T119(57,IR,IY,IS) = 0.
          T119(58,IR,IY,IS) = 0.
          T119(59,IR,IY,IS) = 0.
          T119(60,IR,IY,IS) = 0.
          T119(61,IR,IY,IS) = 0.
          T119(62,IR,IY,IS) = 0.
          T119(63,IR,IY,IS) = 0.
          T119(57,IR,IY,IS) =  &
             (PTPRS(IR,IY)*QTPRS(IR,IY) + PNGRS(IR,IY)*QNGRS(IR,IY) + &
              PCLRS(IR,IY)*QCLRS(IR,IY) + PELRS(IR,IY)*QELRS(IR,IY))
          T119(58,IR,IY,IS) = &
             (PTPCM(IR,IY)*QTPCM(IR,IY) + PNGCM(IR,IY)*QNGCM(IR,IY) + &
              PCLCM(IR,IY)*QCLCM(IR,IY) + PELCM(IR,IY)*QELCM(IR,IY))
          T119(59,IR,IY,IS) = PTPIN(IR,IY)*(QTPIN(IR,IY)-QTPRF(IR,IY))+ &
              PNGIN(IR,IY)*(QNGIN(IR,IY)-QNGRF(IR,IY)) + &
              PCLIN(IR,IY)*(QCLIN(IR,IY)-QCTLRF(IR,IY)) + &
              PELIN(IR,IY)*(QELIN(IR,IY)-QELRF(IR,IY)) + &
              PMCIN(IR,IY)*QMCIN(IR,IY) + PMCIN(IR,IY)*QCIIN(IR,IY)
          T119(60,IR,IY,IS)= (PTPTR(IR,IY)*QTPTR(IR,IY) + &
              PNGTR(IR,IY)*QNGTR(IR,IY) + PELTR(IR,IY)*QELTR(IR,IY) + &
              PMETR(IR,IY)*QMETR(IR,IY))
          T119(61,IR,IY,IS) = T119(57,IR,IY,IS) + T119(58,IR,IY,IS) + &
                            T119(59,IR,IY,IS) + T119(60,IR,IY,IS)
          T119(62,IR,IY,IS) = PETTR(IR,IY)*QTRTR(IR,IY)
          T119(63,IR,IY,IS) = T119(61,IR,IY,IS) + T119(62,IR,IY,IS)
      ENDDO
      ENDDO

! TABLE 118.   FUEL TAX OF ENERGY BY SOURCE AND END-USE SECTOR
      itapstart=0
      DO 1180 IY=1,LASTYR
! Transmission and distribution margins by sector
        T13(27,IY,IS) = PNGRS(11,IY)*CFNGN(IY) - T13(20,IY,IS)
        T13(28,IY,IS) = PNGCM(11,IY)*CFNGN(IY) - T13(20,IY,IS)
        T13(29,IY,IS) = PNGIN(11,IY)*CFNGN(IY) - T13(20,IY,IS)
        T13(30,IY,IS) = PNGEL(11,IY)*CFNGU(IY) - T13(20,IY,IS)
        T13(31,IY,IS) = PNGTR(11,IY)*CFNGN(IY) - T13(20,IY,IS)
        T13(32,IY,IS) = PNGAS(11,IY)*CFNGC(IY) - T13(20,IY,IS)
! Transmission and distribution margins by sector
        T13(50,IY,IS) =(PNGRS(11,IY)*CFNGN(IY) - T13(20,IY,IS)) / SCALPR * MC_JPGDP(IY)
        T13(51,IY,IS) =(PNGCM(11,IY)*CFNGN(IY) - T13(20,IY,IS)) / SCALPR * MC_JPGDP(IY)
        T13(52,IY,IS) =(PNGIN(11,IY)*CFNGN(IY) - T13(20,IY,IS)) / SCALPR * MC_JPGDP(IY)
        T13(53,IY,IS) =(PNGEL(11,IY)*CFNGU(IY) - T13(20,IY,IS)) / SCALPR * MC_JPGDP(IY)
        T13(54,IY,IS) =(PNGTR(11,IY)*CFNGN(IY) - T13(20,IY,IS)) / SCALPR * MC_JPGDP(IY)
        T13(55,IY,IS) =(PNGAS(11,IY)*CFNGC(IY) - T13(20,IY,IS)) / SCALPR * MC_JPGDP(IY)
! RESIDENTIAL
      IF ((QTPRS(11,IY)+QNGRS(11,IY)+QCLRS(11,IY)+QELRS(11,IY)) .NE. 0.0) &
          T118(1,IY,IS)= &
            (ATPRS(11,IY)*QTPRS(11,IY)+ANGRS(11,IY)*QNGRS(11,IY)+ &
             ACLRS(11,IY)*QCLRS(11,IY)+AELRS(11,IY)*QELRS(11,IY)) / &
            (QTPRS(11,IY)+QNGRS(11,IY)+QCLRS(11,IY)+QELRS(11,IY)) - &
            (PTPRS(11,IY)*QTPRS(11,IY)+PNGRS(11,IY)*QNGRS(11,IY)+ &
             PCLRS(11,IY)*QCLRS(11,IY)+PELRS(11,IY)*QELRS(11,IY)) / &
            (QTPRS(11,IY)+QNGRS(11,IY)+QCLRS(11,IY)+QELRS(11,IY))
      IF ((QTPRS(11,IY)+QNGRS(11,IY)+QCLRS(11,IY)) .NE. 0.0) &
          T118(2,IY,IS)= &
            (ATPRS(11,IY)*QTPRS(11,IY)+ANGRS(11,IY)*QNGRS(11,IY)+ &
             ACLRS(11,IY)*QCLRS(11,IY)) / &
            (QTPRS(11,IY)+QNGRS(11,IY)+QCLRS(11,IY)) - &
            (PTPRS(11,IY)*QTPRS(11,IY)+PNGRS(11,IY)*QNGRS(11,IY)+ &
             PCLRS(11,IY)*QCLRS(11,IY)) / &
            (QTPRS(11,IY)+QNGRS(11,IY)+QCLRS(11,IY))
          T118(3,IY,IS) = ATPRS(11,IY) - PTPRS(11,IY)
          T118(4,IY,IS) = ADSRS(11,IY) - PDSRS(11,IY)
          T118(5,IY,IS) = ALGRS(11,IY) - PLGRS(11,IY)
          T118(6,IY,IS) = ANGRS(11,IY) - PNGRS(11,IY)
          T118(7,IY,IS) = AELRS(11,IY) - PELRS(11,IY)
! COMMERCIAL
      IF ((QTPCM(11,IY)+QNGCM(11,IY)+QCLCM(11,IY)+QELCM(11,IY)) .NE. 0.0) &
          T118( 8,IY,IS)= &
            (ATPCM(11,IY)*QTPCM(11,IY)+ANGCM(11,IY)*QNGCM(11,IY)+ &
             ACLCM(11,IY)*QCLCM(11,IY)+AELCM(11,IY)*QELCM(11,IY)) / &
            (QTPCM(11,IY)+QNGCM(11,IY)+QCLCM(11,IY)+QELCM(11,IY)) - &
            (PTPCM(11,IY)*QTPCM(11,IY)+PNGCM(11,IY)*QNGCM(11,IY)+ &
             PCLCM(11,IY)*QCLCM(11,IY)+PELCM(11,IY)*QELCM(11,IY)) / &
            (QTPCM(11,IY)+QNGCM(11,IY)+QCLCM(11,IY)+QELCM(11,IY))
      IF ((QTPCM(11,IY)+QNGCM(11,IY)+QCLCM(11,IY)) .NE. 0.0) &
          T118( 9,IY,IS)= &
            (ATPCM(11,IY)*QTPCM(11,IY)+ANGCM(11,IY)*QNGCM(11,IY)+ &
             ACLCM(11,IY)*QCLCM(11,IY)) / &
            (QTPCM(11,IY)+QNGCM(11,IY)+QCLCM(11,IY)) - &
            (PTPCM(11,IY)*QTPCM(11,IY)+PNGCM(11,IY)*QNGCM(11,IY)+ &
             PCLCM(11,IY)*QCLCM(11,IY)) / &
            (QTPCM(11,IY)+QNGCM(11,IY)+QCLCM(11,IY))
          T118(10,IY,IS) = ATPCM(11,IY) - PTPCM(11,IY)
          T118(11,IY,IS) = ADSCM(11,IY) - PDSCM(11,IY)
          T118(12,IY,IS) = ARSCM(11,IY) - PRSCM(11,IY)
          T118(13,IY,IS) = ANGCM(11,IY) - PNGCM(11,IY)
          T118(14,IY,IS) = AELCM(11,IY) - PELCM(11,IY)
! INDUSTRIAL (PRICES AND QUANTITIES INCLUDE REFINERY)
      IF((QTPIN(11,IY)+QNGIN(11,IY)+QCLIN(11,IY)+QELIN(11,IY)+ &
          QMCIN(11,IY)) .NE. 0.0)          T118(15,IY,IS)= &
            (ATPIN(11,IY)*QTPIN(11,IY)+ANGIN(11,IY)*QNGIN(11,IY)+ &
             ACLIN(11,IY)*(QCLIN(11,IY)-QCTLRF(11,IY))+AELIN(11,IY)*QELIN(11,IY)+ &
             ACLSN(11,IY)*QCTLRF(11,IY) + &
             AMCIN(11,IY)*QMCIN(11,IY)) / (QTPIN(11,IY)+ &
             QNGIN(11,IY)+QCLIN(11,IY)+QELIN(11,IY)+QMCIN(11,IY)) - &
            (PTPIN(11,IY)*QTPIN(11,IY)+PNGIN(11,IY)*QNGIN(11,IY)+ &
             PCLIN(11,IY)*(QCLIN(11,IY)-QCTLRF(11,IY))+PELIN(11,IY)*QELIN(11,IY)+ &
             PCLSN(11,IY)*QCTLRF(11,IY) + &
             PMCIN(11,IY)*QMCIN(11,IY)) / (QTPIN(11,IY)+ &
             QNGIN(11,IY)+QCLIN(11,IY)+QELIN(11,IY)+QMCIN(11,IY))
      IF((QTPIN(11,IY)+QNGIN(11,IY)+QCLIN(11,IY)+QMCIN(11,IY)) .NE. 0.0) &
          T118(16,IY,IS)= &
            (ATPIN(11,IY)*QTPIN(11,IY)+ANGIN(11,IY)*QNGIN(11,IY)+ &
             ACLIN(11,IY)*(QCLIN(11,IY)-QCTLRF(11,IY))+ &
             ACLSN(11,IY)*QCTLRF(11,IY) + &
             AMCIN(11,IY)*QMCIN(11,IY)) / (QTPIN(11,IY)+ &
             QNGIN(11,IY)+QCLIN(11,IY)+QMCIN(11,IY)) - &
            (PTPIN(11,IY)*QTPIN(11,IY)+PNGIN(11,IY)*QNGIN(11,IY)+ &
             PCLIN(11,IY)*(QCLIN(11,IY)-QCTLRF(11,IY))+ &
             PCLSN(11,IY)*QCTLRF(11,IY)+ &
             PMCIN(11,IY)*QMCIN(11,IY)) / (QTPIN(11,IY)+ &
             QNGIN(11,IY)+QCLIN(11,IY)+QMCIN(11,IY))
          T118(17,IY,IS) = ATPIN(11,IY) - PTPIN(11,IY)
          T118(18,IY,IS) = ADSIN(11,IY) - PDSIN(11,IY)
! do this if averaging in feedstock price:
          IF(QLGIN(11,IY) .NE. 0.0) &
          T118(19,IY,IS)=((ALGIN(11,IY) - PLGIN(11,IY))*(QLGIN(11,IY)-INQLGPF(11,IY)/1000.) + &
                             (ALGINPF(11,IY) - PLGINPF(11,IY))*INQLGPF(11,IY)/1000.) / QLGIN(11,IY)
! otherwise, do this:
          T118(19,IY,IS) = ALGIN(11,IY) - PLGIN(11,IY)
! or this since we now label the row "propane"
          T118(19,IY,IS) = APRIN(11,IY) - PPRIN(11,IY)
          T118(20,IY,IS) = ARSIN(11,IY) - PRSIN(11,IY)
          T118(21,IY,IS) = ANGIN(11,IY) - PNGIN(11,IY)
          T118(22,IY,IS) = AMCIN(11,IY) - PMCIN(11,IY)
          T118(23,IY,IS) = ACLIN(11,IY) - PCLIN(11,IY)
          T118(24,IY,IS) = AELIN(11,IY) - PELIN(11,IY)
! TRANSPORTATION
       IF ((QTPTR(11,IY)-QDSBS(11,IY)-QJFBS(11,IY)-QMGBS(11,IY)+QNGTR(11,IY)+QELTR(11,IY)) .NE. 0.0) &
          T118(25,IY,IS)= &
!           (ATPTR(11,IY)*
            ((ATPTR(11,IY)*QTPTR(11,IY)-ADSTR(11,IY)*QDSBS(11,IY)- &
              AJFTR(11,IY)*QJFBS(11,IY)-AMGTR(11,IY)*QMGBS(11,IY))/ &
             (QTPTR(11,IY)-QDSBS(11,IY)-QJFBS(11,IY)-QMGBS(11,IY))* &
                           (QTPTR(11,IY)-QDSBS(11,IY)-QJFBS(11,IY)-QMGBS(11,IY))+ &
             ANGTR(11,IY)*QNGTR(11,IY)+ &
             AELTR(11,IY)*QELTR(11,IY)) / &
            (QTPTR(11,IY)-QDSBS(11,IY)-QJFBS(11,IY)-QMGBS(11,IY)+QNGTR(11,IY)+QELTR(11,IY)) - &
!           (PTPTR(11,IY)*QTPTR(11,IY)+
            (PTPTR(11,IY)*QTPTR(11,IY)-PDSTR(11,IY)*QDSBS(11,IY)- &
             PJFTR(11,IY)*QJFBS(11,IY)-PMGTR(11,IY)*QMGBS(11,IY)+ &
             PNGTR(11,IY)*QNGTR(11,IY)+ &
             PELTR(11,IY)*QELTR(11,IY)) / &
            (QTPTR(11,IY)-QDSBS(11,IY)-QJFBS(11,IY)-QMGBS(11,IY)+QNGTR(11,IY)+QELTR(11,IY))
       IF ((QTPTR(11,IY)-QDSBS(11,IY)-QJFBS(11,IY)-QMGBS(11,IY)+QNGTR(11,IY)) .NE. 0.0) &
          T118(26,IY,IS)= &
            (ATPTR(11,IY)*QTPTR(11,IY)-ADSTR(11,IY)*QDSBS(11,IY)- &
             AJFTR(11,IY)*QJFBS(11,IY)-AMGTR(11,IY)*QMGBS(11,IY)+ &
             ANGTR(11,IY)*QNGTR(11,IY)) / &
            (QTPTR(11,IY)-QDSBS(11,IY)-QJFBS(11,IY)-QMGBS(11,IY)+QNGTR(11,IY)) - &
            (PTPTR(11,IY)*QTPTR(11,IY)-PDSTR(11,IY)*QDSBS(11,IY)- &
             PJFTR(11,IY)*QJFBS(11,IY)-PMGTR(11,IY)*QMGBS(11,IY)+ &
             PNGTR(11,IY)*QNGTR(11,IY)) / &
            (QTPTR(11,IY)-QDSBS(11,IY)-QJFBS(11,IY)-QMGBS(11,IY)+QNGTR(11,IY))
          T118(27,IY,IS) = ATPTR(11,IY) - PTPTR(11,IY)
          T118(28,IY,IS) = ADSTR(11,IY) - PDSTR(11,IY)
          T118(29,IY,IS) = AJFTR(11,IY) - PJFTR(11,IY)
          T118(30,IY,IS) = AMGTR(11,IY) - PMGTR(11,IY)
          T118(31,IY,IS) = ARSTR(11,IY) - PRSTR(11,IY)
          T118(32,IY,IS) = ANGTR(11,IY) - PNGTR(11,IY)
          T118(33,IY,IS) = AELTR(11,IY) - PELTR(11,IY)
! Assign table 70 allowance/tax values here:
          DO IR=1,11
             T70(50,IR,IY,IS) =(ADSTR(IR,IY) - PDSTR(IR,IY))*CFDSTRHWY(IY)/42.   ! was JDSTR(IY)
             T70(51,IR,IY,IS) =(AMGTR(IR,IY) - PMGTR(IR,IY))*CFMGQ(IY)/42.       ! was JMGTR(IY)
             T70(52,IR,IY,IS) =(AJFTR(IR,IY) - PJFTR(IR,IY))*CFJFK/42.           ! was JJFTR(IY)
             T70(53,IR,IY,IS) =(ADSRS(IR,IY) - PDSRS(IR,IY))*CFDSRS(IY)/42.      ! was JDSRS(IY)
          ENDDO
!  Total End-Use Energy
        DENOM = (QTPRS(11,IY)+QNGRS(11,IY)+QCLRS(11,IY)+QELRS(11,IY)+ &
         QTPCM(11,IY)+QNGCM(11,IY)+QCLCM(11,IY)+QELCM(11,IY)+ &
         QTPIN(11,IY)+QNGIN(11,IY)+QCLIN(11,IY)+QELIN(11,IY)+ &
         QMCIN(11,IY)+QTPTR(11,IY)+QNGTR(11,IY)+QELTR(11,IY))
        IF (DENOM .NE. 0.0) T118(34,IY,IS)= &
         (T118( 1,IY,IS)*(QTPRS(11,IY)+QNGRS(11,IY)+QCLRS(11,IY)+QELRS(11,IY))+ &
          T118( 1,IY,IS)*(QTPCM(11,IY)+QNGCM(11,IY)+QCLCM(11,IY)+QELCM(11,IY))+ &
          T118(15,IY,IS)*(QTPIN(11,IY)+QNGIN(11,IY)+QCLIN(11,IY)+ &
                           QELIN(11,IY)+QMCIN(11,IY))+ &
          T118(25,IY,IS)*(QTPTR(11,IY)+QNGTR(11,IY)+QELTR(11,IY)))/ &
          DENOM
        DENOM = (QTPRS(11,IY)+QNGRS(11,IY)+QCLRS(11,IY)+ &
                 QTPCM(11,IY)+QNGCM(11,IY)+QCLCM(11,IY)+ &
                 QTPIN(11,IY)+QNGIN(11,IY)+QCLIN(11,IY)+ &
                 QMCIN(11,IY)+QTPTR(11,IY)+QNGTR(11,IY))
       IF (DENOM .NE. 0.0) T118(35,IY,IS)= &
         (T118( 1,IY,IS)*(QTPRS(11,IY)+QNGRS(11,IY)+QCLRS(11,IY))+ &
          T118( 8,IY,IS)*(QTPCM(11,IY)+QNGCM(11,IY)+QCLCM(11,IY))+ &
          T118(15,IY,IS)*(QTPIN(11,IY)+QNGIN(11,IY)+QCLIN(11,IY)+QMCIN(11,IY))+ &
          T118(25,IY,IS)*(QTPTR(11,IY)+QNGTR(11,IY)))/DENOM
          T118(36,IY,IS) = AELAS(11,IY) - PELAS(11,IY)
!  ELEC UTILITIES
          IF((QTPEL(11,IY)+QNGEL(11,IY)+QCLEL(11,IY)) .NE. 0.0) &
          T118(37,IY,IS) = &
            (ATPEL(11,IY)*QTPEL(11,IY)+ &
            ANGEL(11,IY)*QNGEL(11,IY)+ACLEL(11,IY)*QCLEL(11,IY)) / &
            (QTPEL(11,IY)+QNGEL(11,IY)+QCLEL(11,IY)) - &
            (PTPEL(11,IY)*QTPEL(11,IY)+ &
            PNGEL(11,IY)*QNGEL(11,IY)+PCLEL(11,IY)*QCLEL(11,IY)) / &
            (QTPEL(11,IY)+QNGEL(11,IY)+QCLEL(11,IY))
          T118(38,IY,IS) = ATPEL(11,IY) - PTPEL(11,IY)
          T118(39,IY,IS) = ADSEL(11,IY) - PDSEL(11,IY)
          T118(40,IY,IS) = ARSEL(11,IY) - PRSEL(11,IY)
          T118(41,IY,IS) = ANGEL(11,IY) - PNGEL(11,IY)
          T118(42,IY,IS) = ACLEL(11,IY) - PCLEL(11,IY)

!  ALL USERS
          T118(43,IY,IS) = ATPAS(11,IY) - PTPAS(11,IY)
          T118(44,IY,IS) = ADSAS(11,IY) - PDSAS(11,IY)
          T118(45,IY,IS) = AJFTR(11,IY) - PJFTR(11,IY)
          T118(46,IY,IS) = ALGAS(11,IY) - PLGAS(11,IY)
! calculated based on the sectoral prices shown (as stated in the footnote):
          IF (QPRRS(11,IY)+QPRCM(11,IY)+QPRIN(11,IY)-QPRINPF(11,IY)+QPRTR(11,IY) .NE. 0.0) &
          T118(46,IY,IS)= &
             (QPRRS(11,IY)*(APRRS(11,IY)-PPRRS(11,IY))+QPRCM(11,IY)*(APRRS(11,IY)-PPRCM(11,IY))+ &
             (QPRIN(11,IY)-QPRINPF(11,IY))*(APRIN(11,IY)-PPRIN(11,IY))+QPRTR(11,IY)*(APRRS(11,IY)-PPRTR(11,IY))) / &
             (QPRRS(11,IY)+QPRCM(11,IY)+QPRIN(11,IY)-QPRINPF(11,IY)+QPRTR(11,IY))
          T118(47,IY,IS) = AMGAS(11,IY) - PMGAS(11,IY)
          T118(48,IY,IS) = ARSAS(11,IY) - PRSAS(11,IY)
          T118(49,IY,IS) = ANGAS(11,IY) - PNGAS(11,IY)
          T118(50,IY,IS) = ACLAS(11,IY) - PCLAS(11,IY)
          T118(51,IY,IS) = AELAS(11,IY) - PELAS(11,IY)
!= Page Break==================================================
          T118(141,IY,IS)= EMETAX(2,IY) * 1000. * SCALPR
          T118(52,IY,IS) = EMETAX(1,IY) * 1000. * SCALPR
          T118(55,IY,IS) = GHG_OffsetP(IY) * 1000. * SCALPR
          T118(53,IY,IS) = GHG_OffsetPInt(IY) * 1000. * SCALPR
          T118(54,IY,IS) = EMETAX(1,IY) * 1000. * MC_JPGDP(IY)
          T118(138,IY,IS)= EMETAX(3,IY) * 1000. * SCALPR   ! max price (TAP or safety price).  corresponding layout.txt line may be commented
!   Allowance Value
          IF (MC_JPGDP(IY) .NE. 0.0) THEN                                     ! Allowance Value (billion #### dollars)
            T118(56,IY,IS) = (EMREV( 6,IY) / MC_JPGDP(IY)) * SCALPR           !   Potential If All Auctioned
            T118(57,IY,IS) = (EMREV( 8,IY) / MC_JPGDP(IY)) * SCALPR           !   Auctioned Allowances
            T118(58,IY,IS) =((EMREV(6,IY)-EMREV(12,IY))/MC_JPGDP(IY)) * SCALPR!   Distributed Allowances
            T118(59,IY,IS) = (EMREV( 9,IY) / MC_JPGDP(IY)) * SCALPR           !     Carbon Capture and Sequestration Bonus
            T118(60,IY,IS) = (EMREV(10,IY) / MC_JPGDP(IY)) * SCALPR           !     Other Sequestration Incentives
            T118(61,IY,IS) = T118(58,IY,IS)-T118(59,IY,IS)-T118(60,IY,IS)     !     Other Allocations
                                                                              !   Value of Offset Credits
            T118(62,IY,IS) = GHG_Rev(2,iy) * SCALPR                           !     Noncovered Greenhouse Gases
            T118(63,IY,IS) = GHG_Rev(3,iy) * SCALPR                           !     Biogenic Sequestration
            T118(154,IY,IS) = offset(4,iy) * EMETAX(1,IY) * SCALPR            !     international allowances
            T118(64,IY,IS) = GHG_Rev(4,iy) * SCALPR  - t118(154,iy,is)        !     International  offsets
          ENDIF
! compute covered other ghg as sum of ghg emissions with a ghg classification of 1 (see offsets.wk1 for defn)
          covered_oghg=0.
          uncovered_oghg=0.
          do i=1,ghg_ncat
            if(ghg_class(i).eq.1) then
              covered_oghg=covered_oghg+ghg_oghg(iy,i)
            elseif(ghg_class(i).eq.2.or.ghg_class(i).eq.0) then
              uncovered_oghg=uncovered_oghg+ghg_oghg(iy,i)
            endif
          enddo
          bank=0.
          if(iy+1989.ge.bank_startyr) then
            bank=emlim(1,iy) + sum(offset(:,iy)) - emsol(1,iy)   ! calculate here
            bank=banking(iy)         !  calculate in epm
          endif
! GHG Cap Compliance, Goal, and Banking
! Covered CO2 is total covered emissions minus covered other ghg
          T118(65,IY,IS) = EMSOL(1,IY)-covered_oghg
!!!!!!!   if(iy.le.17) T118(65,IY,IS) =T17(29,11,IY,IS)  ! 2006 and before: use co2 history override data from table 17
          T118(66,IY,IS) = covered_oghg
          t118(148,iy,is)= emsol(1,iy)

          T118(142,IY,IS)= Off_limits(1,iy)
          T118(143,IY,IS)= Off_limits(2,iy)
          T118(144,IY,IS)= Off_limits(3,iy)
          T118(145,IY,IS)= Off_limits(3,iy)
          T118(146,IY,IS)= Off_limits(4,iy)
          if(iy+1989.gt.2017) then
            T118(133,iy,is) = offset(1,iy)+offset(2,iy)+allow_per_offset*OFFSET(3,iy)+offset(4,iy)
          else
            T118(133,iy,is) = sum(OFFSET(1:4,iy))
          endif
!         T118(133,IY,IS)= sum(OFFSET(1:4,iy))        !  Total Offsets.  if BIOSEQOK=0, should omit OFFSET(2,IY).  put BIOSEQOK in restart
          T118(67,IY,IS) = OFFSET(1,iy)               !    Noncovered GHG Offsets
          T118(68,IY,IS) = OFFSET(2,IY)               !    Biogenic Sequestration
          T118(147,IY,IS)=Offset(1,iy)+OFFSET(2,iy)
          if(iy+1989.gt.2017) then
            T118(69,IY,IS)=OFFSET(3,IY)*allow_per_offset
          else
          T118(69,IY,IS) = OFFSET(3,IY)               !    International Offsets
          endif
          T118(140,iy,IS)= OFFSET(4,IY)               !    International Allowances
          if(iy+1989.gt.2017) then
          T118(70,IY,IS) = emsol(1,iy)- &             !  Covered Emissions less offsets
            (OFFSET(1,iy)+OFFSET(2,IY)+ allow_per_offset*OFFSET(3,iy)+offset(4,iy)) ! if BIOSEQOK=0, should omit offset(2,iy)
          else
            T118(70,IY,IS) = emsol(1,iy)- &             !  Covered Emissions less offsets
            (OFFSET(1,iy)+OFFSET(2,IY)+ OFFSET(3,iy)+offset(4,iy)) ! if BIOSEQOK=0, should omit offset(2,iy)
          endif
          T118(71,IY,IS) = EMLIM(1,IY)                !  Covered Emissions Goal
!S1766 changes
!          T118(72,IY,IS) = banking(iy)                         ! s1766 only.  will have TAP row to represent borrowing
!          T118(73,IY,IS) = max(0.,balance(iy))                  ! s1766 only.  will have TAP row to represent borrowing
!          T118(139,IY,IS)= 0.                                  ! s1766 only.  TAP Row
!          if(iy.gt.1) then                                     ! s1766 only.  TAP Row
!            if(balance(iy-1).gt.0 .and.balance(iy).lt.0) then  ! s1766 only.  TAP Row
!              T118(139,IY,IS)=-banking(iy)-balance(iy-1)       ! S1766 only.  Transition year. mix of remaining bank and TAP
!              itapstart=1
!            elseif(itapstart.eq.1) then                        ! s1766 only.  TAP Row
!              T118(139,IY,IS)=-banking(iy)                     ! s1766 only.  TAP Row.  post banking
!              T118(72,IY,IS) = 0.                              ! s1766 only.  TAP row represents borrowing
!            endif                                              ! s1766 only.  TAP Row
!          endif                                                ! s1766 only.  TAP Row
!=======
          T118(72,IY,IS) = bank                                 !  Net Allowance Bank Change
          T118(73,IY,IS) = balance(iy)                          !  Cumulative Bank Balance
! Total ghg emissions: CO2 plus Other GHG
          T118(74,IY,IS) = T17(29,11,IY,IS)
          T118(75,IY,IS) = covered_oghg + uncovered_oghg
          T118(76,IY,IS) = sum(T118(74:75,IY,IS))
          t118(153,iy,is)=t118(76,iy,is)-sum(offset(2:4,iy)) ! total emissions net of bioseq (2), and int'l offsets (3:4)

          T118(134,iy,is)= EMCARBON(5,mnumnr,iy)+sum(ccs_pmm(1:4,mnumpr,iy))   !    Total Carbon Capture and Storage
          T118(135,iy,is)= sum(EMCARBON(1:2,mnumnr,iy))      !      Petroleum
          T118(136,iy,is)= EMCARBON(4,mnumnr,iy)             !      Natural Gas
          T118(137,iy,is)= EMCARBON(3,mnumnr,iy)             !      Coal
          t118(149,iy,is)= ccs_pmm(1,mnumpr,iy)
          t118(150,iy,is)= ccs_pmm(2,mnumpr,iy)
          t118(151,iy,is)= ccs_pmm(3,mnumpr,iy)
          t118(152,iy,is)= ccs_pmm(4,mnumpr,iy)
! Baseline
          ighg=77  ! first line of other greenhouse gases section
          T118(77 ,IY,IS) = sum(GHG_BL(iy,1:5)) ! sum CH4
          T118(78,IY,IS) = GHG_BL(iy,1)
          T118(79,IY,IS) = GHG_BL(iy,2)
          T118(80,IY,IS) = GHG_BL(iy,3)
          T118(81,IY,IS) = GHG_BL(iy,4)
          T118(82,IY,IS) = GHG_BL(iy,5)
          T118(83,IY,IS) = GHG_BL(iy,6) ! other co2
          T118(84,IY,IS) = sum(GHG_BL(iy,7:10)) ! sum of N2O
          T118(85,IY,IS) = GHG_BL(iy,7)
          T118(86,IY,IS) = GHG_BL(iy,8)
          T118(87,IY,IS) = GHG_BL(iy,9)
          T118(88,IY,IS) = GHG_BL(iy,10)
          T118(89,IY,IS) = sum(GHG_BL(iy,11:14)) ! sum of F-Gases
          T118(90,IY,IS) = GHG_BL(iy,11)
          T118(91,IY,IS) = GHG_BL(iy,12)
          T118(92,IY,IS) = GHG_BL(iy,13)
          T118(93,IY,IS) = GHG_BL(iy,14)
          T118(94,IY,IS) = sum(GHG_BL(iy,1:14)) ! sum of other GHG
! Abatement/Offsets
          ighg=95
          T118(95,IY,IS) = sum(GHG_Abate(iy,1:5)) ! sum CH4
          T118(96,IY,IS) = GHG_Abate(iy,1)
          T118(97,IY,IS) = GHG_Abate(iy,2)
          T118(98,IY,IS) = GHG_Abate(iy,3)
          T118(99,IY,IS) = GHG_Abate(iy,4)
          T118(100,IY,IS) = GHG_Abate(iy,5)
          T118(101,IY,IS) = GHG_Abate(iy,6) ! other co2
          T118(102,IY,IS) = sum(GHG_Abate(iy,7:10)) ! sum of N2O
          T118(103,IY,IS) = GHG_Abate(iy,7)
          T118(104,IY,IS) = GHG_Abate(iy,8)
          T118(105,IY,IS) = GHG_Abate(iy,9)
          T118(106,IY,IS) = GHG_Abate(iy,10)
          T118(107,IY,IS) = sum(GHG_Abate(iy,11:14)) ! sum of F-Gases
          T118(108,IY,IS) = GHG_Abate(iy,11)
          T118(109,IY,IS) = GHG_Abate(iy,12)
          T118(110,IY,IS) = GHG_Abate(iy,13)
          T118(111,IY,IS) = GHG_Abate(iy,14)
          T118(112,IY,IS) = sum(GHG_Abate(iy,1:14)) ! sum of other GHG
! Resulting/Controlled emissions
          ighg=113
          T118(113,IY,IS) = sum(GHG_OGHG(iy,1:5)) ! sum CH4
          T118(114,IY,IS) = GHG_OGHG(iy,1)
          T118(115,IY,IS) = GHG_OGHG(iy,2)
          T118(116,IY,IS) = GHG_OGHG(iy,3)
          T118(117,IY,IS) = GHG_OGHG(iy,4)
          T118(118,IY,IS) = GHG_OGHG(iy,5)
          T118(119,IY,IS) = GHG_OGHG(iy,6) ! other co2
          T118(120,IY,IS) = sum(GHG_OGHG(iy,7:10)) ! sum of N2O
          T118(121,IY,IS) = GHG_OGHG(iy,7)
          T118(122,IY,IS) = GHG_OGHG(iy,8)
          T118(123,IY,IS) = GHG_OGHG(iy,9)
          T118(124,IY,IS) = GHG_OGHG(iy,10)
          T118(125,IY,IS) = sum(GHG_OGHG(iy,11:14)) ! sum of F-Gases
          T118(126,IY,IS) = GHG_OGHG(iy,11)
          T118(127,IY,IS) = GHG_OGHG(iy,12)
          T118(128,IY,IS) = GHG_OGHG(iy,13)
          T118(129,IY,IS) = GHG_OGHG(iy,14)
          T118(130,IY,IS) = sum(GHG_OGHG(iy,1:14)) ! sum of other GHG
! rows 131 and 132 are unused.  138 and 139 are for TAP price and TAP sales. 140 is expected price. 141-150 available for changes


!  do this to overwrite historical data that may vary slightly from Table 17 and row 74 in this table
       if (ABS(T118(65,LASTYR,IS) - T17(29,11,LASTYR,IS)) .LT. .01 ) &
              T118(65,:,IS) = T17(29,11,:,IS)

! AB32
       IGHG= 155
       T118(155,IY,IS) = AB_CAP_TOT(IY)                               ! Allowances issued (cap)
       T118(156,IY,IS) = AB_CAP_TOT(IY)*AB_CSTCONT_FRAC(IY)           ! Allowances held in reserve, cost containment
       T118(157,IY,IS) = T118(155,IY,IS) - T118(156,IY,IS)            ! Allowances net of reserves
       T118(158,IY,IS) = AB_ALLBANK_AVL(IY)                           ! Allowance Banks Balance
       T118(159,IY,IS) = AB_CSTCONT_AVL(IY)                           ! Cost Containment Reserve Balance
       T118(160,IY,IS) = AB_CAP_TOT(IY)*AB_OFFSET_FRAC(IY)            ! Offsets assumed
       T118(161,IY,IS) = AB_COVD_EM_ELE(IY)                           ! Cov Emis Electric sector
       T118(162,IY,IS) = AB_COVD_EM_REF(IY)                           ! Cov Emis Refining
       T118(163,IY,IS) = AB_COVD_EM_IND(IY)                           ! Cov Emis Industrial energy-related
       T118(164,IY,IS) = AB_COVD_EM_FUE(IY)                           ! Cov Emis Fuel providers
       T118(165,IY,IS) = AB_COVD_EM_OTH(IY)                           ! Cov Emis Other (non-CO2 or not modeled explicitly)
       T118(166,IY,IS) = AB_COVD_EM_TOT(IY)                           ! Cov Emis  Total covered emissions
       IF (IY .EQ. 1)THEN
         IGHG=167  ! use symbol so only one row in parse_ftab output
         T118(IGHG,IY,IS) = AB_ALLBANK_AVL(IY)                         ! Allowance Bank Activity
       ELSE
         T118(167,IY,IS) = AB_ALLBANK_AVL(IY)-AB_ALLBANK_AVL(IY - 1)  ! Allowance Bank Activity
       END IF
       T118(168,IY,IS) = AB_CSTCONT_USE(IY)                           ! Cost Containment Reserves Purchased
       T118(169,IY,IS) = AB_OFFSET_USED(IY)                           ! Offsets Submitted
       T118(170,IY,IS) = T118(157,IY,IS) - T118(167,IY,IS) + T118(168,IY,IS) +  &
                             T118(169,IY,IS) - T118(166,IY,IS)        ! Balance (Allow/Bnk/Off/RSV-emis)
       T118(171,IY,IS) = AB_ALLOW_P(IY) * 1000. * SCALPR              ! Allowance price
       T118(172,IY,IS) = AB_RESERVE_P(2,IY) * 1000. * SCALPR          ! Cost containment trigger price

! RGGI
       IGHG= 173
       T118(173,IY,IS) = RG_CAP_TOT(IY)                              ! Allowances issued (cap)
       T118(185,IY,IS) = RG_RSRVECR_AVL(IY)                          ! Emissions Containment Reserves Available
       T118(186,IY,IS) = T118(173,IY,IS) - T118(185,IY,IS)           ! Allowances net of ECR
       T118(174,IY,IS) = RG_ALLBANK_AVL(IY)                          ! Allowance Banks Balance
       T118(187,IY,IS) = RG_RSRVECR_AVL(IY)                          ! Emissions Containment Reserves Available
       T118(175,IY,IS) = RG_RESERVE_AVL(IY)                          ! Cost Containment Reserves Available
       T118(176,IY,IS) = RG_OFFSETS_AVL(IY)                          ! Offsets Available
       T118(177,IY,IS) = RG_COVD_EM_ELE(IY)                          ! Cov Emis Electric sector
       T118(178,IY,IS) = RG_ALLBANK_USE(IY)                          ! Allowance Banks Used
       T118(188,IY,IS) = RG_RSRVECR_USE(IY)                          ! Emissions Containment Reserves Used
       T118(179,IY,IS) = RG_RESERVE_USE(IY)                          ! Cost Containment Reserves Used
       T118(180,IY,IS) = RG_OFFSETS_USE(IY)                          ! Offsets Used
       T118(181,IY,IS) = T118(173 ,IY,IS) - T118(178 ,IY,IS) + T118(179 ,IY,IS) + T118(188, IY,IS) +  &
                         T118(180 ,IY,IS) - T118(177 ,IY,IS)         ! Balance (Allow/Bnk/Off/RSV-emis)
       T118(182,IY,IS) = RG_ALLOW_P(IY)                              ! Allowance price
       T118(183,IY,IS) = RG_AUCTION_P(IY)                            ! Floor price
       T118(189,IY,IS) = RG_RSRVECR_P(IY)                            ! Emissions Containment Reserve price
       T118(184,IY,IS) = RG_RESERVE_P(IY)                            ! Cost Containment Reserve price

1180   CONTINUE


       RETURN
       END
! ***********************************************************
      SUBROUTINE CARBFLATE
      implicit none
      include 'parametr'
      include 'ftable'
      include 'ncntrl'
      include 'emmparm'
      include 'emission'
      include 'epmbank'
      include 'ghgrep'
      include 'ab32'
      include 'rggi'
      include 'lfmmout'
      include 'uecpout'
      integer iy
      REAL C_CO2_FACTOR

      C_CO2_FACTOR = 44. / 12.

      EMRS(:,1,:) = EMRS(:,1,:) * C_CO2_FACTOR
      EMCM(:,1,:) = EMCM(:,1,:) * C_CO2_FACTOR
      EMINC(:,1,:) = EMINC(:,1,:) * C_CO2_FACTOR
      EMTR(:,1,:) = EMTR(:,1,:) * C_CO2_FACTOR
      EMEL(:,1,:) = EMEL(:,1,:) * C_CO2_FACTOR
      EMNT(:,1,:) = EMNT(:,1,:) * C_CO2_FACTOR

      EMRSC(:,1,:) = EMRSC(:,1,:) * C_CO2_FACTOR
      EMCMC(:,1,:) = EMCMC(:,1,:) * C_CO2_FACTOR
      EMINCC(:,1,:) = EMINCC(:,1,:) * C_CO2_FACTOR
      EMTRC(:,1,:) = EMTRC(:,1,:) * C_CO2_FACTOR
      EMELC(:,1,:) = EMELC(:,1,:) * C_CO2_FACTOR
      EMCARBON = EMCARBON * C_CO2_FACTOR

      EMLIM(1,:) = EMLIM(1,:) * C_CO2_FACTOR
      EMSOL(1,:) = EMSOL(1,:) * C_CO2_FACTOR

      COPCCS = COPCCS * C_CO2_FACTOR

      OFFSET = OFFSET * C_CO2_FACTOR
      OFF_LIMITS(2:,:)=OFF_LIMITS(2:,:) * C_CO2_FACTOR
      BALANCE = BALANCE * C_CO2_FACTOR

      GHG_BL = GHG_BL * C_CO2_FACTOR
      GHG_OGHG = GHG_OGHG * C_CO2_FACTOR
      GHG_Abate = GHG_Abate * C_CO2_FACTOR
      banking = banking * C_CO2_FACTOR

! esh 1/15/21: commented out conversions--now being done in lfreport.gms so that gdx files consistent      
! LCFS variables:
!      LCFS_BaseLine = LCFS_BaseLine * C_CO2_FACTOR
!      LCFS_Actual = LCFS_Actual * C_CO2_FACTOR
!     LCFS_Waiver = LCFS_Waiver * C_CO2_FACTOR
!      LCFS_Offset_Prc = LCFS_Offset_Prc / C_CO2_FACTOR
!     LCFS_Carb_Offset = LCFS_Carb_Offset * C_CO2_FACTOR

      do iy=1,mnumyr
        if (emetax(1,iy) .ne. embtax(1,iy)) then
          EMETAX(1,iy) = EMETAX(1,iy) / C_CO2_FACTOR
          EMETAX(2,iy) = EMETAX(2,iy) / C_CO2_FACTOR
          EMETAX(3,iy) = EMETAX(3,iy) / C_CO2_FACTOR
          GHG_OffsetP(iy) = GHG_OffsetP(iy) / C_CO2_FACTOR
          GHG_OffsetPInt(iy) = GHG_OffsetPInt(iy) / C_CO2_FACTOR
        else
          EMETAX(1,iy) = EMETAX(1,iy) / 1000.
          EMETAX(2,iy) = EMETAX(2,iy) / 1000.
          EMETAX(3,iy) = EMETAX(3,iy) / 1000.
          GHG_OffsetP(iy) = GHG_OffsetP(iy) / 1000.
          GHG_OffsetPInt(iy) = GHG_OffsetPInt(iy) / 1000.
        endif
      enddo

! new regional carbon dioxide emission variables
      em_resd = em_resd * C_CO2_FACTOR
      em_comm = em_comm * C_CO2_FACTOR
      em_indy = em_indy * C_CO2_FACTOR
      em_tran = em_tran * C_CO2_FACTOR
      em_elec = em_elec * C_CO2_FACTOR

      ccs_pmm = ccs_pmm * C_CO2_FACTOR

      ab_covd_em_ind   = ab_covd_em_ind    * C_CO2_FACTOR
      ab_covd_em_ref   = ab_covd_em_ref    * C_CO2_FACTOR
      ab_covd_em_ele   = ab_covd_em_ele    * C_CO2_FACTOR
      ab_covd_em_fue   = ab_covd_em_fue    * C_CO2_FACTOR
      ab_covd_em_oth   = ab_covd_em_oth    * C_CO2_FACTOR
      ab_covd_em_tot   = ab_covd_em_tot    * C_CO2_FACTOR
      ab_cap_tot       = ab_cap_tot        * C_CO2_FACTOR
      ab_allbank_avl   = ab_allbank_avl    * C_CO2_FACTOR
      ab_cstcont_avl   = ab_cstcont_avl    * C_CO2_FACTOR
      ab_cstcont_use   = ab_cstcont_use    * C_CO2_FACTOR
      ab_offset_used   = ab_offset_used    * C_CO2_FACTOR
      ab_reserve_p     = ab_reserve_p      / C_CO2_FACTOR
      ab_allow_p       = ab_allow_p        / C_CO2_FACTOR
      ab_allow_pold    = ab_allow_pold     / C_CO2_FACTOR

      rg_covd_em_ele   = rg_covd_em_ele     * C_CO2_FACTOR
      rg_cap_tot       = rg_cap_tot         * C_CO2_FACTOR
      rg_allbank_avl   = rg_allbank_avl     * C_CO2_FACTOR
      rg_allbank_use   = rg_allbank_use     * C_CO2_FACTOR
      rg_reserve_avl   = rg_reserve_avl     * C_CO2_FACTOR
      rg_reserve_use   = rg_reserve_use     * C_CO2_FACTOR
      rg_rsrvecr_avl   = rg_rsrvecr_avl     * C_CO2_FACTOR
      rg_rsrvecr_use   = rg_rsrvecr_use     * C_CO2_FACTOR
      rg_offsets_avl   = rg_offsets_avl     * C_CO2_FACTOR
      rg_offsets_use   = rg_offsets_use     * C_CO2_FACTOR
      rg_allow_p       = rg_allow_p         / C_CO2_FACTOR
      rg_auction_p     = rg_auction_p       / C_CO2_FACTOR
      rg_reserve_p     = rg_reserve_p       / C_CO2_FACTOR
      rg_rsrvecr_p     = rg_rsrvecr_p       / C_CO2_FACTOR

      RETURN
      END

      SUBROUTINE CAPFLATE
      IMPLICIT NONE
      include 'parametr'
      include 'ncntrl'
      include 'qblk'
      include 'ampblk'          !  use adjusted prices
      include 'emmparm'
      include 'macout'
      include 'cogen'
      include 'uecpout'
      include 'uefdout'
      include 'udatout'
      include 'cdsparms'
      include 'coalout'
      include 'coalrep'
      include 'tranrep'
      include 'pmmrpt'
      include 'pmmftab'
      include 'pmmout'
      include 'lfmmout'
      include 'ogsmout'
      include 'intout'
      include 'indout'
      include 'indrep'
      include 'ghgrep'
      include 'emission'
      include 'convfact'
      include 'ngtdmrep'
      include 'ftable'
      include 'resdrep'
      include 'comparm'
      include 'commrep'

      INTEGER IY,IR,I,J
      REAL FSUM, temp_sum
      EXTERNAL FSUM
      REAL LIMEOUT(MNUMCR)
      INTEGER IRG

! propane price national averages don't seem to be averages of the regional values, so calculate here:
      DO IY=1,LASTYR

         IF (CFCRDLTSWT(IY) .EQ. 0.0) CFCRDLTSWT(IY) = 5.713498
         IF (CFCRDEXP(IY) .EQ. 0.0) CFCRDEXP(IY) = CFCRDDOM(IY)

! make sure national PV generation matches sum of regions
      UGNPVNR(1,mnumnr,IY) = sum(UGNPVNR(1,1:mnumnr-1,IY))
      UGNPVNR(2,mnumnr,IY) = sum(UGNPVNR(2,1:mnumnr-1,IY))
      UGNPTNR(1,mnumnr,IY) = sum(UGNPTNR(1,1:mnumnr-1,IY))
      UGNPTNR(2,mnumnr,IY) = sum(UGNPTNR(2,1:mnumnr-1,IY))
DO IR=1,mnumnr
UGNHONR(1,ir,IY) = UGNHYNR(1,ir,IY) + UGNGENR(1,ir,IY) + UGNMSNR(1,ir,IY) +  &
                   UGNWDNR(1,ir,IY) + UGNSONR(1,ir,IY) + UGNPVNR(1,ir,IY) + UGNPTNR(1,ir,IY) +  &
                   UGNWNNR(1,ir,IY) + UGNWLNR(1,ir,IY) + UGNWFNR(1,ir,IY)
UGNHONR(2,ir,IY) = UGNHYNR(2,ir,IY) + UGNGENR(2,ir,IY) + UGNMSNR(2,ir,IY) +  &
                   UGNWDNR(2,ir,IY) + UGNSONR(2,ir,IY) + UGNPVNR(2,ir,IY) + UGNPTNR(2,ir,IY) +  &
                   UGNWNNR(2,ir,IY) + UGNWLNR(2,ir,IY) + UGNWFNR(2,ir,IY)
ENDDO
UGNHONR(1,mnumnr,IY)=sum(UGNHONR(1,1:mnumnr-1,IY))
UGNHONR(2,mnumnr,IY)=sum(UGNHONR(2,1:mnumnr-1,IY))
UGNMSNR(1,mnumnr,IY)=sum(UGNMSNR(1,1:mnumnr-1,IY))
UGNMSNR(2,mnumnr,IY)=sum(UGNMSNR(2,1:mnumnr-1,IY))


      IF (IY .GE. 2009-1989) em_resd(4,1:9,iy+1989) = 0.0
      IF (sum(QPRRS(1:9,IY)) .NE. 0.0) THEN
      PPRRS(MNUMCR,IY) = (QPRRS( 1,IY)*PPRRS( 1,IY) + QPRRS( 2,IY)*PPRRS( 2,IY) + &
                          QPRRS( 3,IY)*PPRRS( 3,IY) + QPRRS( 4,IY)*PPRRS( 4,IY) + &
                          QPRRS( 5,IY)*PPRRS( 5,IY) + QPRRS( 6,IY)*PPRRS( 6,IY) + &
                          QPRRS( 7,IY)*PPRRS( 7,IY) + QPRRS( 8,IY)*PPRRS( 8,IY) + &
                          QPRRS( 9,IY)*PPRRS( 9,IY)) / sum(QPRRS(1:9,IY))
      ELSE
          PPRRS(MNUMCR,IY) = sum(PPRRS(1:9,IY)) / 9.
      ENDIF
      IF (sum(QPRCM(1:9,IY)) .NE. 0.0) THEN
      PPRCM(MNUMCR,IY) = (QPRCM( 1,IY)*PPRCM( 1,IY) + QPRCM( 2,IY)*PPRCM( 2,IY) + &
                          QPRCM( 3,IY)*PPRCM( 3,IY) + QPRCM( 4,IY)*PPRCM( 4,IY) + &
                          QPRCM( 5,IY)*PPRCM( 5,IY) + QPRCM( 6,IY)*PPRCM( 6,IY) + &
                          QPRCM( 7,IY)*PPRCM( 7,IY) + QPRCM( 8,IY)*PPRCM( 8,IY) + &
                          QPRCM( 9,IY)*PPRCM( 9,IY)) / sum(QPRCM(1:9,IY))
      ELSE
          PPRCM(MNUMCR,IY) = sum(PPRCM(1:9,IY)) / 9.
      ENDIF
      IF (sum(QPRIN(1:9,IY)) .NE. 0.0) THEN
      PPRIN(MNUMCR,IY) = (QPRIN( 1,IY)*PPRIN( 1,IY) + QPRIN( 2,IY)*PPRIN( 2,IY) + &
                          QPRIN( 3,IY)*PPRIN( 3,IY) + QPRIN( 4,IY)*PPRIN( 4,IY) + &
                          QPRIN( 5,IY)*PPRIN( 5,IY) + QPRIN( 6,IY)*PPRIN( 6,IY) + &
                          QPRIN( 7,IY)*PPRIN( 7,IY) + QPRIN( 8,IY)*PPRIN( 8,IY) + &
                          QPRIN( 9,IY)*PPRIN( 9,IY)) / sum(QPRIN(1:9,IY))
      ELSE
          PPRIN(MNUMCR,IY) = sum(PPRIN(1:9,IY)) / 9.
      ENDIF
      IF (sum(QPRTR(1:9,IY)) .NE. 0.0) THEN
      PPRTR(MNUMCR,IY) = (QPRTR( 1,IY)*PPRTR( 1,IY) + QPRTR( 2,IY)*PPRTR( 2,IY) + &
                          QPRTR( 3,IY)*PPRTR( 3,IY) + QPRTR( 4,IY)*PPRTR( 4,IY) + &
                          QPRTR( 5,IY)*PPRTR( 5,IY) + QPRTR( 6,IY)*PPRTR( 6,IY) + &
                          QPRTR( 7,IY)*PPRTR( 7,IY) + QPRTR( 8,IY)*PPRTR( 8,IY) + &
                          QPRTR( 9,IY)*PPRTR( 9,IY)) / sum(QPRTR(1:9,IY))
      ELSE
          PPRTR(MNUMCR,IY) = sum(PPRTR(1:9,IY)) / 9.
      ENDIF
      ENDDO
      QMOREQ = QMOREQ / 1000.
      QCLRFPD = QCLRFPD / 1000.
      QNGRFPD = QNGRFPD / 1000.
      QCCRF = QCCRF / 1000.
      QAGTR = QAGTR / 1000.
      QLUTR = QLUTR / 1000.

      OGCRDPRD = OGCRDPRD / 365.    ! converting to million barrels per day

   ! If some new variables haven't been filled (=0) fill with values from somewhere else
      DO IY=1,LASTYR
   !  or, in this case, approximate
       temp_sum = sum(CRNETHCD(1:9,IY))
       IF (CRNETHCD(11,IY) .NE. temp_sum .AND. temp_sum .NE. 0.0) THEN
           DO IR=1,9
             CRNETHCD(IR,IY)=CRNETHCD(IR,IY)/temp_sum*CRNETHCD(11,IY)
           ENDDO
       ENDIF
       temp_sum = sum(OTHETHCD(1:9,IY))
       IF (OTHETHCD(11,IY) .NE. temp_sum .AND. temp_sum .NE. 0.0) THEN
           DO IR=1,9
             OTHETHCD(IR,IY)=OTHETHCD(IR,IY)/temp_sum*OTHETHCD(11,IY)
           ENDDO
       ENDIF
       temp_sum = sum(CLLETHCD(1:9,IY))
       IF (CLLETHCD(11,IY) .NE. temp_sum .AND. temp_sum .NE. 0.0) THEN
           DO IR=1,9
             IF (ISNAN(CLLETHCD(IR,IY))) THEN
           CLLETHCD(IR,IY)=0.0
             ELSE
           CLLETHCD(IR,IY)=CLLETHCD(IR,IY)/temp_sum*CLLETHCD(11,IY)
             ENDIF
           ENDDO
       ENDIF
       IF (ETHTOTCD(11,IY) .EQ. 0.0) THEN
           ETHTOTCD(11,IY) = CRNETHCD(11,IY) + CLLETHCD(11,IY) + OTHETHCD(11,IY) + (ETHIMP(11,IY) - ETHEXP(11,IY))/0.9751
          IF (ETHTOTCD(11,IY) .GT. 0.0) THEN
   !find first year with non-zero
             J=0
             DO IR=IY+1,MNUMYR
               IF (ETHTOTCD(11,IR) .NE. 0.0 .AND. J .EQ. 0) THEN
                  J=IR
   !                    write(6,'(" First non-zero year is ",I4)') 1989+J
               ENDIF
             ENDDO
             IF (J .GT. 0) THEN
                DO IR=1,9
                   ETHTOTCD(IR,IY) = ETHTOTCD(IR,J)/ETHTOTCD(11,J)*ETHTOTCD(11,IY)
                ENDDO
             ENDIF
          ENDIF
       ENDIF

! Zero out Bahamas price if quantity is 0
         IF (OGQNGIMP(18,IY) .EQ. 0.0) OGPNGIMP(18,IY) = 0.0

      ENDDO

! 0 out coal to liquids price if there is 0 quantity
      DO IY=1,LASTYR
        DO IR=MNUMCR-2,1,-1
           IF (PCLSN(IR,IY) .NE. 0.0) PCLSN(MNUMCR,IY) = PCLSN(IR,IY)
        ENDDO
      ENDDO

! adjust PCLIN, PMCIN, and PCEL for national average (census region 11) so CTL is excluded
! the purpose of this is: to undue the work of main.f so that weighting schema on regions is correct;
! (not that main is incorrect).  these are "pure" industrial prices and domestic electric power and met
      DO IY=16,MNUMYR
        PCLIN(11,IY)=(PCLIN(1,IY)*(QCLIN(1,IY)-QCTLRF(1,IY)) + &
                     PCLIN(2,IY)*(QCLIN(2,IY)-QCTLRF(2,IY)) + &
                     PCLIN(3,IY)*(QCLIN(3,IY)-QCTLRF(3,IY)) + &
                     PCLIN(4,IY)*(QCLIN(4,IY)-QCTLRF(4,IY)) + &
                     PCLIN(5,IY)*(QCLIN(5,IY)-QCTLRF(5,IY)) + &
                     PCLIN(6,IY)*(QCLIN(6,IY)-QCTLRF(6,IY)) + &
                     PCLIN(7,IY)*(QCLIN(7,IY)-QCTLRF(7,IY)) + &
                     PCLIN(8,IY)*(QCLIN(8,IY)-QCTLRF(8,IY)) + &
                     PCLIN(9,IY)*(QCLIN(9,IY)-QCTLRF(9,IY)) + &
                     PCLIN(10,IY)*(QCLIN(10,IY)-QCTLRF(10,IY)))/ &
                     (QCLIN(11,IY)-QCTLRF(11,IY))
        PCLEL(11,IY)=(PCLEL(1,IY)*(QCLEL(1,IY)) + &
                     PCLEL(2,IY)*(QCLEL(2,IY))+ &
                     PCLEL(3,IY)*(QCLEL(3,IY))+ &
                     PCLEL(4,IY)*(QCLEL(4,IY))+ &
                     PCLEL(5,IY)*(QCLEL(5,IY))+ &
                     PCLEL(6,IY)*(QCLEL(6,IY))+ &
                     PCLEL(7,IY)*(QCLEL(7,IY))+ &
                     PCLEL(8,IY)*(QCLEL(8,IY))+ &
                     PCLEL(9,IY)*(QCLEL(9,IY))+ &
                     PCLEL(10,IY)*(QCLEL(10,IY)))/ &
                     (QCLEL(11,IY))
        PMCIN(11,IY)=(PMCIN(1,IY)*(QMCIN(1,IY)) + &
                     PMCIN(2,IY)*(QMCIN(2,IY))+ &
                     PMCIN(3,IY)*(QMCIN(3,IY))+ &
                     PMCIN(4,IY)*(QMCIN(4,IY))+ &
                     PMCIN(5,IY)*(QMCIN(5,IY))+ &
                     PMCIN(6,IY)*(QMCIN(6,IY))+ &
                     PMCIN(7,IY)*(QMCIN(7,IY))+ &
                     PMCIN(8,IY)*(QMCIN(8,IY))+ &
                     PMCIN(9,IY)*(QMCIN(9,IY))+ &
                     PMCIN(10,IY)*(QMCIN(10,IY)))/ &
                     (QMCIN(11,IY))
      END DO

! Now do the same for the unadjusted prices
      CALL UNCAPFLATE
      CALL EPMCAPFLATE

      DO IR=1,MNUMCR
        DO IY=16,MNUMYR
! adjust TRQLDV for natural gas consumed to produce hydrogen; even remove losses part
          TRQLDV(4,IR,IY) = TRQLDV(4,IR,IY) - QHYTR(IR,IY) / .7
! note that we are also not including all coal-to-liquid coal, so PCLAS should be the steam coal average price
          PCLAS(IR,IY)=(PCLRS(IR,IY)*QCLRS(IR,IY)+PCLCM(IR,IY)*QCLCM(IR,IY)+ &
                        PCLIN(IR,IY)*(QCLIN(IR,IY)-QCTLRF(IR,IY))+ &
                        PCLEL(IR,IY)*QCLEL(IR,IY))/ &
             (QCLRS(IR,IY)+QCLCM(IR,IY)+QCLIN(IR,IY)-QCTLRF(IR,IY)+QCLEL(IR,IY))
        END DO
      END DO
! Recalculate EMM capacity addition and retirement variables based on CUMCAPADD.

      DO IY=CUMCAPADD-1989+1,LASTYR    !  start 1st year after CUMCAPADD
        DO IR=1,MNUMNR
! Additions
          UADDCSC(IR,IY)=UADDCSC(IR,IY) - UADDCSC(IR,CUMCAPADD-1989)
          UADDIGC(IR,IY)=UADDIGC(IR,IY) - UADDIGC(IR,CUMCAPADD-1989)
          UADDOSC(IR,IY)=UADDOSC(IR,IY) - UADDOSC(IR,CUMCAPADD-1989)
          UADDCCC(IR,IY)=UADDCCC(IR,IY) - UADDCCC(IR,CUMCAPADD-1989)
          UADDACC(IR,IY)=UADDACC(IR,IY) - UADDACC(IR,CUMCAPADD-1989)
          UADDCTC(IR,IY)=UADDCTC(IR,IY) - UADDCTC(IR,CUMCAPADD-1989)
          UADDATC(IR,IY)=UADDATC(IR,IY) - UADDATC(IR,CUMCAPADD-1989)
          UADDRNC(IR,IY)=UADDRNC(IR,IY) - UADDRNC(IR,CUMCAPADD-1989)
          UADDHYC(IR,IY)=UADDHYC(IR,IY) - UADDHYC(IR,CUMCAPADD-1989)
          UADDGEC(IR,IY)=UADDGEC(IR,IY) - UADDGEC(IR,CUMCAPADD-1989)
          UADDMSC(IR,IY)=UADDMSC(IR,IY) - UADDMSC(IR,CUMCAPADD-1989)
          UADDWDC(IR,IY)=UADDWDC(IR,IY) - UADDWDC(IR,CUMCAPADD-1989)
          UADDSTC(IR,IY)=UADDSTC(IR,IY) - UADDSTC(IR,CUMCAPADD-1989)
          UADDPVC(IR,IY)=UADDPVC(IR,IY) - UADDPVC(IR,CUMCAPADD-1989)
          UADDWNC(IR,IY)=UADDWNC(IR,IY) - UADDWNC(IR,CUMCAPADD-1989)
          UADDWFC(IR,IY)=UADDWFC(IR,IY) - UADDWFC(IR,CUMCAPADD-1989)
          UADDPTC(IR,IY)=UADDPTC(IR,IY) - UADDPTC(IR,CUMCAPADD-1989)
          UADDWLC(IR,IY)=UADDWLC(IR,IY) - UADDWLC(IR,CUMCAPADD-1989)
          UADDTLC(IR,IY)=UADDTLC(IR,IY) - UADDTLC(IR,CUMCAPADD-1989)
          DO I=1,2
            UADDCSU(I,IR,IY)=UADDCSU(I,IR,IY) - UADDCSU(I,IR,CUMCAPADD-1989)
            UADDIGU(I,IR,IY)=UADDIGU(I,IR,IY) - UADDIGU(I,IR,CUMCAPADD-1989)
            UADDISU(I,IR,IY)=UADDISU(I,IR,IY) - UADDISU(I,IR,CUMCAPADD-1989)
            UADDOSU(I,IR,IY)=UADDOSU(I,IR,IY) - UADDOSU(I,IR,CUMCAPADD-1989)
            UADDCCU(I,IR,IY)=UADDCCU(I,IR,IY) - UADDCCU(I,IR,CUMCAPADD-1989)
            UADDACU(I,IR,IY)=UADDACU(I,IR,IY) - UADDACU(I,IR,CUMCAPADD-1989)
            UADDASU(I,IR,IY)=UADDASU(I,IR,IY) - UADDASU(I,IR,CUMCAPADD-1989)
            UADDCTU(I,IR,IY)=UADDCTU(I,IR,IY) - UADDCTU(I,IR,CUMCAPADD-1989)
            UADDATU(I,IR,IY)=UADDATU(I,IR,IY) - UADDATU(I,IR,CUMCAPADD-1989)
            UADDNUU(I,IR,IY)=UADDNUU(I,IR,IY) - UADDNUU(I,IR,CUMCAPADD-1989)
            UADDFCU(I,IR,IY)=UADDFCU(I,IR,IY) - UADDFCU(I,IR,CUMCAPADD-1989)
            UADDRNU(I,IR,IY)=UADDRNU(I,IR,IY) - UADDRNU(I,IR,CUMCAPADD-1989)
            UADDPSU(I,IR,IY)=UADDPSU(I,IR,IY) - UADDPSU(I,IR,CUMCAPADD-1989)
            UADDHYU(I,IR,IY)=UADDHYU(I,IR,IY) - UADDHYU(I,IR,CUMCAPADD-1989)
            UADDGEU(I,IR,IY)=UADDGEU(I,IR,IY) - UADDGEU(I,IR,CUMCAPADD-1989)
            UADDMSU(I,IR,IY)=UADDMSU(I,IR,IY) - UADDMSU(I,IR,CUMCAPADD-1989)
            UADDWDU(I,IR,IY)=UADDWDU(I,IR,IY) - UADDWDU(I,IR,CUMCAPADD-1989)
            UADDSTU(I,IR,IY)=UADDSTU(I,IR,IY) - UADDSTU(I,IR,CUMCAPADD-1989)
            UADDPVU(I,IR,IY)=UADDPVU(I,IR,IY) - UADDPVU(I,IR,CUMCAPADD-1989)
            UADDWNU(I,IR,IY)=UADDWNU(I,IR,IY) - UADDWNU(I,IR,CUMCAPADD-1989)
            UADDWFU(I,IR,IY)=UADDWFU(I,IR,IY) - UADDWFU(I,IR,CUMCAPADD-1989)
            UADDDSU(I,IR,IY)=UADDDSU(I,IR,IY) - UADDDSU(I,IR,CUMCAPADD-1989)
            UADDWLU(I,IR,IY)=UADDWLU(I,IR,IY) - UADDWLU(I,IR,CUMCAPADD-1989)
            UADDPTU(I,IR,IY)=UADDPTU(I,IR,IY) - UADDPTU(I,IR,CUMCAPADD-1989)
            UADDDBU(I,IR,IY)=UADDDBU(I,IR,IY) - UADDDBU(I,IR,CUMCAPADD-1989)
            UADDDPU(I,IR,IY)=UADDDPU(I,IR,IY) - UADDDPU(I,IR,CUMCAPADD-1989)
            UADDTLU(I,IR,IY)=UADDTLU(I,IR,IY) - UADDTLU(I,IR,CUMCAPADD-1989)
            UADDCSN(I,IR,IY)=UADDCSN(I,IR,IY) - UADDCSN(I,IR,CUMCAPADD-1989)
            UADDIGN(I,IR,IY)=UADDIGN(I,IR,IY) - UADDIGN(I,IR,CUMCAPADD-1989)
            UADDISN(I,IR,IY)=UADDISN(I,IR,IY) - UADDISN(I,IR,CUMCAPADD-1989)
            UADDOSN(I,IR,IY)=UADDOSN(I,IR,IY) - UADDOSN(I,IR,CUMCAPADD-1989)
            UADDCCN(I,IR,IY)=UADDCCN(I,IR,IY) - UADDCCN(I,IR,CUMCAPADD-1989)
            UADDACN(I,IR,IY)=UADDACN(I,IR,IY) - UADDACN(I,IR,CUMCAPADD-1989)
            UADDASN(I,IR,IY)=UADDASN(I,IR,IY) - UADDASN(I,IR,CUMCAPADD-1989)
            UADDFCN(I,IR,IY)=UADDFCN(I,IR,IY) - UADDFCN(I,IR,CUMCAPADD-1989)
            UADDCTN(I,IR,IY)=UADDCTN(I,IR,IY) - UADDCTN(I,IR,CUMCAPADD-1989)
            UADDATN(I,IR,IY)=UADDATN(I,IR,IY) - UADDATN(I,IR,CUMCAPADD-1989)
            UADDNUN(I,IR,IY)=UADDNUN(I,IR,IY) - UADDNUN(I,IR,CUMCAPADD-1989)
            UADDRNN(I,IR,IY)=UADDRNN(I,IR,IY) - UADDRNN(I,IR,CUMCAPADD-1989)
            UADDPSN(I,IR,IY)=UADDPSN(I,IR,IY) - UADDPSN(I,IR,CUMCAPADD-1989)
            UADDHYN(I,IR,IY)=UADDHYN(I,IR,IY) - UADDHYN(I,IR,CUMCAPADD-1989)
            UADDGEN(I,IR,IY)=UADDGEN(I,IR,IY) - UADDGEN(I,IR,CUMCAPADD-1989)
            UADDMSN(I,IR,IY)=UADDMSN(I,IR,IY) - UADDMSN(I,IR,CUMCAPADD-1989)
            UADDWDN(I,IR,IY)=UADDWDN(I,IR,IY) - UADDWDN(I,IR,CUMCAPADD-1989)
            UADDSTN(I,IR,IY)=UADDSTN(I,IR,IY) - UADDSTN(I,IR,CUMCAPADD-1989)
            UADDPVN(I,IR,IY)=UADDPVN(I,IR,IY) - UADDPVN(I,IR,CUMCAPADD-1989)
            UADDWNN(I,IR,IY)=UADDWNN(I,IR,IY) - UADDWNN(I,IR,CUMCAPADD-1989)
            UADDWFN(I,IR,IY)=UADDWFN(I,IR,IY) - UADDWFN(I,IR,CUMCAPADD-1989)
            UADDOCU(I,IR,IY)=UADDOCU(I,IR,IY) - UADDOCU(I,IR,CUMCAPADD-1989)
            UADDOCN(I,IR,IY)=UADDOCN(I,IR,IY) - UADDOCN(I,IR,CUMCAPADD-1989)
            UADDI2U(I,IR,IY)=UADDI2U(I,IR,IY) - UADDI2U(I,IR,CUMCAPADD-1989)
            UADDI2N(I,IR,IY)=UADDI2N(I,IR,IY) - UADDI2N(I,IR,CUMCAPADD-1989)
            UADDPQU(I,IR,IY)=UADDPQU(I,IR,IY) - UADDPQU(I,IR,CUMCAPADD-1989)
            UADDPQN(I,IR,IY)=UADDPQN(I,IR,IY) - UADDPQN(I,IR,CUMCAPADD-1989)
            UADDNGU(I,IR,IY)=UADDNGU(I,IR,IY) - UADDNGU(I,IR,CUMCAPADD-1989)
            UADDNGN(I,IR,IY)=UADDNGN(I,IR,IY) - UADDNGN(I,IR,CUMCAPADD-1989)
            UADDICU(I,IR,IY)=UADDICU(I,IR,IY) - UADDICU(I,IR,CUMCAPADD-1989)
            UADDICN(I,IR,IY)=UADDICN(I,IR,IY) - UADDICN(I,IR,CUMCAPADD-1989)
            UADDT2U(I,IR,IY)=UADDT2U(I,IR,IY) - UADDT2U(I,IR,CUMCAPADD-1989)
            UADDT2N(I,IR,IY)=UADDT2N(I,IR,IY) - UADDT2N(I,IR,CUMCAPADD-1989)
            UADDA2U(I,IR,IY)=UADDA2U(I,IR,IY) - UADDA2U(I,IR,CUMCAPADD-1989)
            UADDA2N(I,IR,IY)=UADDA2N(I,IR,IY) - UADDA2N(I,IR,CUMCAPADD-1989)
            UADDSMU(I,IR,IY)=UADDSMU(I,IR,IY) - UADDSMU(I,IR,CUMCAPADD-1989)
            UADDSMN(I,IR,IY)=UADDSMN(I,IR,IY) - UADDSMN(I,IR,CUMCAPADD-1989)
            UADDGNU(I,IR,IY)=UADDGNU(I,IR,IY) - UADDGNU(I,IR,CUMCAPADD-1989)
            UADDGNN(I,IR,IY)=UADDGNN(I,IR,IY) - UADDGNN(I,IR,CUMCAPADD-1989)
            UADDBIU(I,IR,IY)=UADDBIU(I,IR,IY) - UADDBIU(I,IR,CUMCAPADD-1989)
            UADDBIN(I,IR,IY)=UADDBIN(I,IR,IY) - UADDBIN(I,IR,CUMCAPADD-1989)
            UADDAGU(I,IR,IY)=UADDAGU(I,IR,IY) - UADDAGU(I,IR,CUMCAPADD-1989)
            UADDAGN(I,IR,IY)=UADDAGN(I,IR,IY) - UADDAGN(I,IR,CUMCAPADD-1989)
            UADDHOU(I,IR,IY)=UADDHOU(I,IR,IY) - UADDHOU(I,IR,CUMCAPADD-1989)
            UADDHON(I,IR,IY)=UADDHON(I,IR,IY) - UADDHON(I,IR,CUMCAPADD-1989)
            UADDHIU(I,IR,IY)=UADDHIU(I,IR,IY) - UADDHIU(I,IR,CUMCAPADD-1989)
            UADDHIN(I,IR,IY)=UADDHIN(I,IR,IY) - UADDHIN(I,IR,CUMCAPADD-1989)
            UADDTIU(I,IR,IY)=UADDTIU(I,IR,IY) - UADDTIU(I,IR,CUMCAPADD-1989)
            UADDTIN(I,IR,IY)=UADDTIN(I,IR,IY) - UADDTIN(I,IR,CUMCAPADD-1989)
            UADDSQU(I,IR,IY)=UADDSQU(I,IR,IY) - UADDSQU(I,IR,CUMCAPADD-1989)
            UADDSQN(I,IR,IY)=UADDSQN(I,IR,IY) - UADDSQN(I,IR,CUMCAPADD-1989)
            UADDZSU(I,IR,IY)=UADDZSU(I,IR,IY) - UADDZSU(I,IR,CUMCAPADD-1989)
            UADDZSN(I,IR,IY)=UADDZSN(I,IR,IY) - UADDZSN(I,IR,CUMCAPADD-1989)
            UADDSSU(I,IR,IY)=UADDSSU(I,IR,IY) - UADDSSU(I,IR,CUMCAPADD-1989)
            UADDSSN(I,IR,IY)=UADDSSN(I,IR,IY) - UADDSSN(I,IR,CUMCAPADD-1989)
            UADDS2U(I,IR,IY)=UADDS2U(I,IR,IY) - UADDS2U(I,IR,CUMCAPADD-1989)
            UADDS2N(I,IR,IY)=UADDS2N(I,IR,IY) - UADDS2N(I,IR,CUMCAPADD-1989)
            UADDINU(I,IR,IY)=UADDINU(I,IR,IY) - UADDINU(I,IR,CUMCAPADD-1989)
            UADDINN(I,IR,IY)=UADDINN(I,IR,IY) - UADDINN(I,IR,CUMCAPADD-1989)
            UADDDSN(I,IR,IY)=UADDDSN(I,IR,IY) - UADDDSN(I,IR,CUMCAPADD-1989)
            UADDWLN(I,IR,IY)=UADDWLN(I,IR,IY) - UADDWLN(I,IR,CUMCAPADD-1989)
            UADDPTN(I,IR,IY)=UADDPTN(I,IR,IY) - UADDPTN(I,IR,CUMCAPADD-1989)
            UADDDBN(I,IR,IY)=UADDDBN(I,IR,IY) - UADDDBN(I,IR,CUMCAPADD-1989)
            UADDDPN(I,IR,IY)=UADDDPN(I,IR,IY) - UADDDPN(I,IR,CUMCAPADD-1989)
            UADDTLN(I,IR,IY)=UADDTLN(I,IR,IY) - UADDTLN(I,IR,CUMCAPADD-1989)
            UADDP2(I,IR,IY) = UADDP2(I,IR,IY) - UADDP2(I,IR,CUMCAPADD-1989)
            UADDINR(I,IR,IY)=UADDINR(I,IR,IY) - UADDINR(I,IR,CUMCAPADD-1989)
            UADDOTR(I,IR,IY)=UADDOTR(I,IR,IY) - UADDOTR(I,IR,CUMCAPADD-1989)
            UADDSRV(I,IR,IY)=UADDSRV(I,IR,IY) - UADDSRV(I,IR,CUMCAPADD-1989)
          ENDDO
          UADDHYT(IR,IY)=UADDHYT(IR,IY) - UADDHYT(IR,CUMCAPADD-1989)
          UADDGET(IR,IY)=UADDGET(IR,IY) - UADDGET(IR,CUMCAPADD-1989)
          UADDMST(IR,IY)=UADDMST(IR,IY) - UADDMST(IR,CUMCAPADD-1989)
          UADDWDT(IR,IY)=UADDWDT(IR,IY) - UADDWDT(IR,CUMCAPADD-1989)
          UADDSTT(IR,IY)=UADDSTT(IR,IY) - UADDSTT(IR,CUMCAPADD-1989)
          UADDPVT(IR,IY)=UADDPVT(IR,IY) - UADDPVT(IR,CUMCAPADD-1989)
          UADDPTT(IR,IY)=UADDPTT(IR,IY) - UADDPTT(IR,CUMCAPADD-1989)
          UADDWNT(IR,IY)=UADDWNT(IR,IY) - UADDWNT(IR,CUMCAPADD-1989)
          UADDWLT(IR,IY)=UADDWLT(IR,IY) - UADDWLT(IR,CUMCAPADD-1989)
          UADDWFT(IR,IY)=UADDWFT(IR,IY) - UADDWFT(IR,CUMCAPADD-1989)
          UADDPST(IR,IY)=UADDPST(IR,IY) - UADDPST(IR,CUMCAPADD-1989)
          UADDHYD(IR,IY)=UADDHYD(IR,IY) - UADDHYD(IR,CUMCAPADD-1989)
          UADDGED(IR,IY)=UADDGED(IR,IY) - UADDGED(IR,CUMCAPADD-1989)
          UADDMSD(IR,IY)=UADDMSD(IR,IY) - UADDMSD(IR,CUMCAPADD-1989)
          UADDWDD(IR,IY)=UADDWDD(IR,IY) - UADDWDD(IR,CUMCAPADD-1989)
          UADDSTD(IR,IY)=UADDSTD(IR,IY) - UADDSTD(IR,CUMCAPADD-1989)
          UADDPVD(IR,IY)=UADDPVD(IR,IY) - UADDPVD(IR,CUMCAPADD-1989)
          UADDWND(IR,IY)=UADDWND(IR,IY) - UADDWND(IR,CUMCAPADD-1989)
          UADDWFD(IR,IY)=UADDWFD(IR,IY) - UADDWFD(IR,CUMCAPADD-1989)
          UADDPSD(IR,IY)=UADDPSD(IR,IY) - UADDPSD(IR,CUMCAPADD-1989)
          UADDWLD(IR,IY)=UADDWLD(IR,IY) - UADDWLD(IR,CUMCAPADD-1989)
          UADDPTD(IR,IY)=UADDPTD(IR,IY) - UADDPTD(IR,CUMCAPADD-1989)
! Retirements
          URETCSU(IR,IY)=URETCSU(IR,IY) - URETCSU(IR,CUMCAPADD-1989)
          URETIGU(IR,IY)=URETIGU(IR,IY) - URETIGU(IR,CUMCAPADD-1989)
          URETISU(IR,IY)=URETISU(IR,IY) - URETISU(IR,CUMCAPADD-1989)
          URETOSU(IR,IY)=URETOSU(IR,IY) - URETOSU(IR,CUMCAPADD-1989)
          URETCCU(IR,IY)=URETCCU(IR,IY) - URETCCU(IR,CUMCAPADD-1989)
          URETACU(IR,IY)=URETACU(IR,IY) - URETACU(IR,CUMCAPADD-1989)
          URETASU(IR,IY)=URETASU(IR,IY) - URETASU(IR,CUMCAPADD-1989)
          URETCTU(IR,IY)=URETCTU(IR,IY) - URETCTU(IR,CUMCAPADD-1989)
          URETATU(IR,IY)=URETATU(IR,IY) - URETATU(IR,CUMCAPADD-1989)
          URETNUU(IR,IY)=URETNUU(IR,IY) - URETNUU(IR,CUMCAPADD-1989)
          URETFCU(IR,IY)=URETFCU(IR,IY) - URETFCU(IR,CUMCAPADD-1989)
          URETPSU(IR,IY)=URETPSU(IR,IY) - URETPSU(IR,CUMCAPADD-1989)
          URETHYU(IR,IY)=URETHYU(IR,IY) - URETHYU(IR,CUMCAPADD-1989)
          URETGEU(IR,IY)=URETGEU(IR,IY) - URETGEU(IR,CUMCAPADD-1989)
          URETMSU(IR,IY)=URETMSU(IR,IY) - URETMSU(IR,CUMCAPADD-1989)
          URETWDU(IR,IY)=URETWDU(IR,IY) - URETWDU(IR,CUMCAPADD-1989)
          URETSTU(IR,IY)=URETSTU(IR,IY) - URETSTU(IR,CUMCAPADD-1989)
          URETPVU(IR,IY)=URETPVU(IR,IY) - URETPVU(IR,CUMCAPADD-1989)
          URETWNU(IR,IY)=URETWNU(IR,IY) - URETWNU(IR,CUMCAPADD-1989)
          URETWFU(IR,IY)=URETWFU(IR,IY) - URETWFU(IR,CUMCAPADD-1989)
          URETWLU(IR,IY)=URETWLU(IR,IY) - URETWLU(IR,CUMCAPADD-1989)
          URETPTU(IR,IY)=URETPTU(IR,IY) - URETPTU(IR,CUMCAPADD-1989)
          URETDSU(IR,IY)=URETDSU(IR,IY) - URETDSU(IR,CUMCAPADD-1989)
          URETRNU(IR,IY)=URETRNU(IR,IY) - URETRNU(IR,CUMCAPADD-1989)
          URETTLU(IR,IY)=URETTLU(IR,IY) - URETTLU(IR,CUMCAPADD-1989)
          URETSQU(IR,IY)=URETSQU(IR,IY) - URETSQU(IR,CUMCAPADD-1989)
          URETNGU(IR,IY)=URETNGU(IR,IY) - URETNGU(IR,CUMCAPADD-1989)
          URETOCU(IR,IY)=URETOCU(IR,IY) - URETOCU(IR,CUMCAPADD-1989)
          URETI2U(IR,IY)=URETI2U(IR,IY) - URETI2U(IR,CUMCAPADD-1989)
          URETPQU(IR,IY)=URETPQU(IR,IY) - URETPQU(IR,CUMCAPADD-1989)
          URETICU(IR,IY)=URETICU(IR,IY) - URETICU(IR,CUMCAPADD-1989)
          URETT2U(IR,IY)=URETT2U(IR,IY) - URETT2U(IR,CUMCAPADD-1989)
          URETA2U(IR,IY)=URETA2U(IR,IY) - URETA2U(IR,CUMCAPADD-1989)
          URETSMU(IR,IY)=URETSMU(IR,IY) - URETSMU(IR,CUMCAPADD-1989)
          URETGNU(IR,IY)=URETGNU(IR,IY) - URETGNU(IR,CUMCAPADD-1989)
          URETBIU(IR,IY)=URETBIU(IR,IY) - URETBIU(IR,CUMCAPADD-1989)
          URETAGU(IR,IY)=URETAGU(IR,IY) - URETAGU(IR,CUMCAPADD-1989)
          URETHOU(IR,IY)=URETHOU(IR,IY) - URETHOU(IR,CUMCAPADD-1989)
          URETHIU(IR,IY)=URETHIU(IR,IY) - URETHIU(IR,CUMCAPADD-1989)
          URETTIU(IR,IY)=URETTIU(IR,IY) - URETTIU(IR,CUMCAPADD-1989)
          URETQSU(IR,IY)=URETQSU(IR,IY) - URETQSU(IR,CUMCAPADD-1989)
          URETZSU(IR,IY)=URETZSU(IR,IY) - URETZSU(IR,CUMCAPADD-1989)
          URETSSU(IR,IY)=URETSSU(IR,IY) - URETSSU(IR,CUMCAPADD-1989)
          URETS2U(IR,IY)=URETS2U(IR,IY) - URETS2U(IR,CUMCAPADD-1989)
          URETINU(IR,IY)=URETINU(IR,IY) - URETINU(IR,CUMCAPADD-1989)
        ENDDO
        DO IR=1,NDREG
          UADDCST(IR,IY)=UADDCST(IR,IY) - UADDCST(IR,CUMCAPADD-1989)
        ENDDO
      ENDDO

! Now set every year <= CUMCAPADD = 0.
      UADDCSC(:,1:(CUMCAPADD-1989))=0.0
      UADDIGC(:,1:(CUMCAPADD-1989))=0.0
      UADDOSC(:,1:(CUMCAPADD-1989))=0.0
      UADDCCC(:,1:(CUMCAPADD-1989))=0.0
      UADDACC(:,1:(CUMCAPADD-1989))=0.0
      UADDCTC(:,1:(CUMCAPADD-1989))=0.0
      UADDATC(:,1:(CUMCAPADD-1989))=0.0
      UADDRNC(:,1:(CUMCAPADD-1989))=0.0
      UADDHYC(:,1:(CUMCAPADD-1989))=0.0
      UADDGEC(:,1:(CUMCAPADD-1989))=0.0
      UADDMSC(:,1:(CUMCAPADD-1989))=0.0
      UADDWDC(:,1:(CUMCAPADD-1989))=0.0
      UADDSTC(:,1:(CUMCAPADD-1989))=0.0
      UADDPVC(:,1:(CUMCAPADD-1989))=0.0
      UADDPTC(:,1:(CUMCAPADD-1989))=0.0
      UADDWNC(:,1:(CUMCAPADD-1989))=0.0
      UADDWLC(:,1:(CUMCAPADD-1989))=0.0
      UADDWFC(:,1:(CUMCAPADD-1989))=0.0
      UADDTLC(:,1:(CUMCAPADD-1989))=0.0
      UADDCSU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDIGU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDISU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDOSU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDCCU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDACU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDASU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDCTU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDATU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDNUU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDFCU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDRNU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDPSU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDHYU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDGEU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDMSU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDWDU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDSTU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDPVU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDPTU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDWNU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDWLU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDWFU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDDSU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDDBU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDDPU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDTLU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDCSN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDIGN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDISN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDOSN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDCCN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDACN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDASN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDFCN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDCTN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDATN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDNUN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDRNN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDPSN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDHYN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDGEN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDMSN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDWDN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDSTN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDPVN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDPTN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDWNN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDWLN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDWFN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDOCU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDOCN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDI2U(:,:,1:(CUMCAPADD-1989))=0.0
      UADDI2N(:,:,1:(CUMCAPADD-1989))=0.0
      UADDPQU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDPQN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDNGU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDNGN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDICU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDICN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDT2U(:,:,1:(CUMCAPADD-1989))=0.0
      UADDT2N(:,:,1:(CUMCAPADD-1989))=0.0
      UADDA2U(:,:,1:(CUMCAPADD-1989))=0.0
      UADDA2N(:,:,1:(CUMCAPADD-1989))=0.0
      UADDSMU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDSMN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDGNU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDGNN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDBIU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDBIN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDAGU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDAGN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDHOU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDHON(:,:,1:(CUMCAPADD-1989))=0.0
      UADDHIU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDHIN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDTIU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDTIN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDSQU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDSQN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDZSU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDZSN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDSSU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDSSN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDS2U(:,:,1:(CUMCAPADD-1989))=0.0
      UADDS2N(:,:,1:(CUMCAPADD-1989))=0.0
      UADDINU(:,:,1:(CUMCAPADD-1989))=0.0
      UADDINN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDDSN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDDBN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDDPN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDTLN(:,:,1:(CUMCAPADD-1989))=0.0
      UADDP2(:,:,1:(CUMCAPADD-1989))=0.0
      UADDINR(:,:,1:(CUMCAPADD-1989))=0.0
      UADDOTR(:,:,1:(CUMCAPADD-1989))=0.0
      UADDSRV(:,:,1:(CUMCAPADD-1989))=0.0
      UADDHYT(:,1:(CUMCAPADD-1989))=0.0
      UADDGET(:,1:(CUMCAPADD-1989))=0.0
      UADDMST(:,1:(CUMCAPADD-1989))=0.0
      UADDWDT(:,1:(CUMCAPADD-1989))=0.0
      UADDSTT(:,1:(CUMCAPADD-1989))=0.0
      UADDPVT(:,1:(CUMCAPADD-1989))=0.0
      UADDPTT(:,1:(CUMCAPADD-1989))=0.0
      UADDWNT(:,1:(CUMCAPADD-1989))=0.0
      UADDWLT(:,1:(CUMCAPADD-1989))=0.0
      UADDWFT(:,1:(CUMCAPADD-1989))=0.0
      UADDPST(:,1:(CUMCAPADD-1989))=0.0
      UADDHYD(:,1:(CUMCAPADD-1989))=0.0
      UADDGED(:,1:(CUMCAPADD-1989))=0.0
      UADDMSD(:,1:(CUMCAPADD-1989))=0.0
      UADDWDD(:,1:(CUMCAPADD-1989))=0.0
      UADDSTD(:,1:(CUMCAPADD-1989))=0.0
      UADDPVD(:,1:(CUMCAPADD-1989))=0.0
      UADDWND(:,1:(CUMCAPADD-1989))=0.0
      UADDWFD(:,1:(CUMCAPADD-1989))=0.0
      UADDPSD(:,1:(CUMCAPADD-1989))=0.0
      UADDWLD(:,1:(CUMCAPADD-1989))=0.0
      UADDPTD(:,1:(CUMCAPADD-1989))=0.0
      UADDCST(:,1:(CUMCAPADD-1989))=0.0

      URETTLU(:,1:(CUMCAPADD-1989))=0.0
      URETCSU(:,1:(CUMCAPADD-1989))=0.0
      URETSQU(:,1:(CUMCAPADD-1989))=0.0
      URETIGU(:,1:(CUMCAPADD-1989))=0.0
      URETISU(:,1:(CUMCAPADD-1989))=0.0
      URETOSU(:,1:(CUMCAPADD-1989))=0.0
      URETCCU(:,1:(CUMCAPADD-1989))=0.0
      URETACU(:,1:(CUMCAPADD-1989))=0.0
      URETASU(:,1:(CUMCAPADD-1989))=0.0
      URETCTU(:,1:(CUMCAPADD-1989))=0.0
      URETATU(:,1:(CUMCAPADD-1989))=0.0
      URETNUU(:,1:(CUMCAPADD-1989))=0.0
      URETFCU(:,1:(CUMCAPADD-1989))=0.0
      URETPSU(:,1:(CUMCAPADD-1989))=0.0
      URETHYU(:,1:(CUMCAPADD-1989))=0.0
      URETGEU(:,1:(CUMCAPADD-1989))=0.0
      URETMSU(:,1:(CUMCAPADD-1989))=0.0
      URETWDU(:,1:(CUMCAPADD-1989))=0.0
      URETSTU(:,1:(CUMCAPADD-1989))=0.0
      URETPVU(:,1:(CUMCAPADD-1989))=0.0
      URETWNU(:,1:(CUMCAPADD-1989))=0.0
      URETWFU(:,1:(CUMCAPADD-1989))=0.0
      URETRNU(:,1:(CUMCAPADD-1989))=0.0
      URETNGU(:,1:(CUMCAPADD-1989))=0.0
      URETOCU(:,1:(CUMCAPADD-1989))=0.0
      URETI2U(:,1:(CUMCAPADD-1989))=0.0
      URETPQU(:,1:(CUMCAPADD-1989))=0.0
      URETICU(:,1:(CUMCAPADD-1989))=0.0
      URETT2U(:,1:(CUMCAPADD-1989))=0.0
      URETA2U(:,1:(CUMCAPADD-1989))=0.0
      URETSMU(:,1:(CUMCAPADD-1989))=0.0
      URETGNU(:,1:(CUMCAPADD-1989))=0.0
      URETBIU(:,1:(CUMCAPADD-1989))=0.0
      URETAGU(:,1:(CUMCAPADD-1989))=0.0
      URETHOU(:,1:(CUMCAPADD-1989))=0.0
      URETHIU(:,1:(CUMCAPADD-1989))=0.0
      URETTIU(:,1:(CUMCAPADD-1989))=0.0
      URETQSU(:,1:(CUMCAPADD-1989))=0.0
      URETDSU(:,1:(CUMCAPADD-1989))=0.0
      URETZSU(:,1:(CUMCAPADD-1989))=0.0
      URETWLU(:,1:(CUMCAPADD-1989))=0.0
      URETSSU(:,1:(CUMCAPADD-1989))=0.0
      URETS2U(:,1:(CUMCAPADD-1989))=0.0
      URETPTU(:,1:(CUMCAPADD-1989))=0.0
      URETINU(:,1:(CUMCAPADD-1989))=0.0

      RETURN
      END

      SUBROUTINE UNCAPFLATE
      IMPLICIT NONE
      include 'parametr'
      include 'ncntrl'
      include 'qblk'
      include 'mpblk'
      include 'emmparm'
      include 'cdsparms'
      include 'coalout'
      INTEGER IY,IR

! propane price national averages don't seem to be averages of the regional values, so calculate here:
      DO IY=1,LASTYR
      IF (sum(QPRRS(1:9,IY)) .NE. 0.0) THEN
      PPRRS(MNUMCR,IY) = (QPRRS( 1,IY)*PPRRS( 1,IY) + QPRRS( 2,IY)*PPRRS( 2,IY) + &
                          QPRRS( 3,IY)*PPRRS( 3,IY) + QPRRS( 4,IY)*PPRRS( 4,IY) + &
                          QPRRS( 5,IY)*PPRRS( 5,IY) + QPRRS( 6,IY)*PPRRS( 6,IY) + &
                          QPRRS( 7,IY)*PPRRS( 7,IY) + QPRRS( 8,IY)*PPRRS( 8,IY) + &
                          QPRRS( 9,IY)*PPRRS( 9,IY)) / sum(QPRRS(1:9,IY))
      ELSE
          PPRRS(MNUMCR,IY) = sum(PPRRS(1:9,IY)) / 9.
      ENDIF
      IF (sum(QPRCM(1:9,IY)) .NE. 0.0) THEN
      PPRCM(MNUMCR,IY) = (QPRCM( 1,IY)*PPRCM( 1,IY) + QPRCM( 2,IY)*PPRCM( 2,IY) + &
                          QPRCM( 3,IY)*PPRCM( 3,IY) + QPRCM( 4,IY)*PPRCM( 4,IY) + &
                          QPRCM( 5,IY)*PPRCM( 5,IY) + QPRCM( 6,IY)*PPRCM( 6,IY) + &
                          QPRCM( 7,IY)*PPRCM( 7,IY) + QPRCM( 8,IY)*PPRCM( 8,IY) + &
                          QPRCM( 9,IY)*PPRCM( 9,IY)) / sum(QPRCM(1:9,IY))
      ELSE
          PPRCM(MNUMCR,IY) = sum(PPRCM(1:9,IY)) / 9.
      ENDIF
      IF (sum(QPRIN(1:9,IY)) .NE. 0.0) THEN
      PPRIN(MNUMCR,IY) = (QPRIN( 1,IY)*PPRIN( 1,IY) + QPRIN( 2,IY)*PPRIN( 2,IY) + &
                          QPRIN( 3,IY)*PPRIN( 3,IY) + QPRIN( 4,IY)*PPRIN( 4,IY) + &
                          QPRIN( 5,IY)*PPRIN( 5,IY) + QPRIN( 6,IY)*PPRIN( 6,IY) + &
                          QPRIN( 7,IY)*PPRIN( 7,IY) + QPRIN( 8,IY)*PPRIN( 8,IY) + &
                          QPRIN( 9,IY)*PPRIN( 9,IY)) / sum(QPRIN(1:9,IY))
      ELSE
          PPRIN(MNUMCR,IY) = sum(PPRIN(1:9,IY)) / 9.
      ENDIF
      IF (sum(QPRTR(1:9,IY)) .NE. 0.0) THEN
      PPRTR(MNUMCR,IY) = (QPRTR( 1,IY)*PPRTR( 1,IY) + QPRTR( 2,IY)*PPRTR( 2,IY) + &
                          QPRTR( 3,IY)*PPRTR( 3,IY) + QPRTR( 4,IY)*PPRTR( 4,IY) + &
                          QPRTR( 5,IY)*PPRTR( 5,IY) + QPRTR( 6,IY)*PPRTR( 6,IY) + &
                          QPRTR( 7,IY)*PPRTR( 7,IY) + QPRTR( 8,IY)*PPRTR( 8,IY) + &
                          QPRTR( 9,IY)*PPRTR( 9,IY)) / sum(QPRTR(1:9,IY))
      ELSE
          PPRTR(MNUMCR,IY) = sum(PPRTR(1:9,IY)) / 9.
      ENDIF
      ENDDO

! adjust PCLIN, PMCIN, and PCEL for national average (census region 11) so CTL is excluded
! the purpose of this is: to undo the work of main.f so that weighting schema on regions is correct;
! (not that main is incorrect).  these are "pure" industrial prices and domestic electr and met
      DO IY=16,MNUMYR
        PCLIN(11,IY)=(PCLIN(1,IY)*(QCLIN(1,IY)-QCTLRF(1,IY)) + &
                     PCLIN(2,IY)*(QCLIN(2,IY)-QCTLRF(2,IY)) + &
                     PCLIN(3,IY)*(QCLIN(3,IY)-QCTLRF(3,IY)) + &
                     PCLIN(4,IY)*(QCLIN(4,IY)-QCTLRF(4,IY)) + &
                     PCLIN(5,IY)*(QCLIN(5,IY)-QCTLRF(5,IY)) + &
                     PCLIN(6,IY)*(QCLIN(6,IY)-QCTLRF(6,IY)) + &
                     PCLIN(7,IY)*(QCLIN(7,IY)-QCTLRF(7,IY)) + &
                     PCLIN(8,IY)*(QCLIN(8,IY)-QCTLRF(8,IY)) + &
                     PCLIN(9,IY)*(QCLIN(9,IY)-QCTLRF(9,IY)) + &
                     PCLIN(10,IY)*(QCLIN(10,IY)-QCTLRF(10,IY)))/ &
                     (QCLIN(11,IY)-QCTLRF(11,IY))
        PCLEL(11,IY)=(PCLEL(1,IY)*(QCLEL(1,IY)) + &
                     PCLEL(2,IY)*(QCLEL(2,IY))+ &
                     PCLEL(3,IY)*(QCLEL(3,IY))+ &
                     PCLEL(4,IY)*(QCLEL(4,IY))+ &
                     PCLEL(5,IY)*(QCLEL(5,IY))+ &
                     PCLEL(6,IY)*(QCLEL(6,IY))+ &
                     PCLEL(7,IY)*(QCLEL(7,IY))+ &
                     PCLEL(8,IY)*(QCLEL(8,IY))+ &
                     PCLEL(9,IY)*(QCLEL(9,IY))+ &
                     PCLEL(10,IY)*(QCLEL(10,IY)))/ &
                     (QCLEL(11,IY))
        PMCIN(11,IY)=(PMCIN(1,IY)*(QMCIN(1,IY)) + &
                     PMCIN(2,IY)*(QMCIN(2,IY))+ &
                     PMCIN(3,IY)*(QMCIN(3,IY))+ &
                     PMCIN(4,IY)*(QMCIN(4,IY))+ &
                     PMCIN(5,IY)*(QMCIN(5,IY))+ &
                     PMCIN(6,IY)*(QMCIN(6,IY))+ &
                     PMCIN(7,IY)*(QMCIN(7,IY))+ &
                     PMCIN(8,IY)*(QMCIN(8,IY))+ &
                     PMCIN(9,IY)*(QMCIN(9,IY))+ &
                     PMCIN(10,IY)*(QMCIN(10,IY)))/ &
                     (QMCIN(11,IY))
      END DO

      DO IR=1,MNUMCR
        DO IY=16,MNUMYR
! adjust weighted average price to exclude imports from quantities because imports not included in prices
! note that we are also not including all coal-to-liquid coal, so PCLAS should be the steam coal average price
          PCLAS(IR,IY)=(PCLRS(IR,IY)*QCLRS(IR,IY)+PCLCM(IR,IY)*QCLCM(IR,IY)+ &
                        PCLIN(IR,IY)*(QCLIN(IR,IY)-QCTLRF(IR,IY))+ &
                        PCLEL(IR,IY)*QCLEL(IR,IY))/ &
             (QCLRS(IR,IY)+QCLCM(IR,IY)+QCLIN(IR,IY)-QCTLRF(IR,IY)+QCLEL(IR,IY))
        END DO
      END DO

      RETURN
      END

      SUBROUTINE EPMCAPFLATE
      IMPLICIT NONE
      include 'parametr'
      include 'ncntrl'
      include 'qblk'
      include 'epmmpblk'
      include 'emmparm'
      include 'cdsparms'
      include 'coalout'
      INTEGER IY,IR

! propane price national averages don't seem to be averages of the regional values, so calculate here:
      DO IY=1,LASTYR
      IF (sum(QPRRS(1:9,IY)) .NE. 0.0) THEN
      APRRS(MNUMCR,IY) = (QPRRS( 1,IY)*APRRS( 1,IY) + QPRRS( 2,IY)*APRRS( 2,IY) + &
                          QPRRS( 3,IY)*APRRS( 3,IY) + QPRRS( 4,IY)*APRRS( 4,IY) + &
                          QPRRS( 5,IY)*APRRS( 5,IY) + QPRRS( 6,IY)*APRRS( 6,IY) + &
                          QPRRS( 7,IY)*APRRS( 7,IY) + QPRRS( 8,IY)*APRRS( 8,IY) + &
                          QPRRS( 9,IY)*APRRS( 9,IY)) / sum(QPRRS(1:9,IY))
      ELSE
          APRRS(MNUMCR,IY) = sum(APRRS(1:9,IY)) / 9.
      ENDIF
      IF (sum(QPRCM(1:9,IY)) .NE. 0.0) THEN
      APRCM(MNUMCR,IY) = (QPRCM( 1,IY)*APRCM( 1,IY) + QPRCM( 2,IY)*APRCM( 2,IY) + &
                          QPRCM( 3,IY)*APRCM( 3,IY) + QPRCM( 4,IY)*APRCM( 4,IY) + &
                          QPRCM( 5,IY)*APRCM( 5,IY) + QPRCM( 6,IY)*APRCM( 6,IY) + &
                          QPRCM( 7,IY)*APRCM( 7,IY) + QPRCM( 8,IY)*APRCM( 8,IY) + &
                          QPRCM( 9,IY)*APRCM( 9,IY)) / sum(QPRCM(1:9,IY))
      ELSE
          APRCM(MNUMCR,IY) = sum(APRCM(1:9,IY)) / 9.
      ENDIF
      IF (sum(QPRIN(1:9,IY)) .NE. 0.0) THEN
      APRIN(MNUMCR,IY) = (QPRIN( 1,IY)*APRIN( 1,IY) + QPRIN( 2,IY)*APRIN( 2,IY) + &
                          QPRIN( 3,IY)*APRIN( 3,IY) + QPRIN( 4,IY)*APRIN( 4,IY) + &
                          QPRIN( 5,IY)*APRIN( 5,IY) + QPRIN( 6,IY)*APRIN( 6,IY) + &
                          QPRIN( 7,IY)*APRIN( 7,IY) + QPRIN( 8,IY)*APRIN( 8,IY) + &
                          QPRIN( 9,IY)*APRIN( 9,IY)) / sum(QPRIN(1:9,IY))
      ELSE
          APRIN(MNUMCR,IY) = sum(APRIN(1:9,IY)) / 9.
      ENDIF
      IF (sum(QPRTR(1:9,IY)) .NE. 0.0) THEN
      APRTR(MNUMCR,IY) = (QPRTR( 1,IY)*APRTR( 1,IY) + QPRTR( 2,IY)*APRTR( 2,IY) + &
                          QPRTR( 3,IY)*APRTR( 3,IY) + QPRTR( 4,IY)*APRTR( 4,IY) + &
                          QPRTR( 5,IY)*APRTR( 5,IY) + QPRTR( 6,IY)*APRTR( 6,IY) + &
                          QPRTR( 7,IY)*APRTR( 7,IY) + QPRTR( 8,IY)*APRTR( 8,IY) + &
                          QPRTR( 9,IY)*APRTR( 9,IY)) / sum(QPRTR(1:9,IY))
      ELSE
          APRTR(MNUMCR,IY) = sum(APRTR(1:9,IY)) / 9.
      ENDIF
      ENDDO

! adjust PCLIN, PMCIN, and PCEL for national average (census region 11) so CTL is excluded
! the purpose of this is: to undue the work of main.f so that weighting schema on regions is correct;
! (not that main is incorrect).  these are "pure" industrial prices and domestic electr and met
      DO IY=16,MNUMYR
        ACLIN(11,IY)=(ACLIN(1,IY)*(QCLIN(1,IY)-QCTLRF(1,IY)) + &
                     ACLIN(2,IY)*(QCLIN(2,IY)-QCTLRF(2,IY)) + &
                     ACLIN(3,IY)*(QCLIN(3,IY)-QCTLRF(3,IY)) + &
                     ACLIN(4,IY)*(QCLIN(4,IY)-QCTLRF(4,IY)) + &
                     ACLIN(5,IY)*(QCLIN(5,IY)-QCTLRF(5,IY)) + &
                     ACLIN(6,IY)*(QCLIN(6,IY)-QCTLRF(6,IY)) + &
                     ACLIN(7,IY)*(QCLIN(7,IY)-QCTLRF(7,IY)) + &
                     ACLIN(8,IY)*(QCLIN(8,IY)-QCTLRF(8,IY)) + &
                     ACLIN(9,IY)*(QCLIN(9,IY)-QCTLRF(9,IY)) + &
                     ACLIN(10,IY)*(QCLIN(10,IY)-QCTLRF(10,IY)))/ &
                     (QCLIN(11,IY)-QCTLRF(11,IY))
        ACLEL(11,IY)=(ACLEL(1,IY)*(QCLEL(1,IY)) + &
                     ACLEL(2,IY)*(QCLEL(2,IY))+ &
                     ACLEL(3,IY)*(QCLEL(3,IY))+ &
                     ACLEL(4,IY)*(QCLEL(4,IY))+ &
                     ACLEL(5,IY)*(QCLEL(5,IY))+ &
                     ACLEL(6,IY)*(QCLEL(6,IY))+ &
                     ACLEL(7,IY)*(QCLEL(7,IY))+ &
                     ACLEL(8,IY)*(QCLEL(8,IY))+ &
                     ACLEL(9,IY)*(QCLEL(9,IY))+ &
                     ACLEL(10,IY)*(QCLEL(10,IY)))/ &
                     (QCLEL(11,IY))
        AMCIN(11,IY)=(AMCIN(1,IY)*(QMCIN(1,IY)) + &
                     AMCIN(2,IY)*(QMCIN(2,IY))+ &
                     AMCIN(3,IY)*(QMCIN(3,IY))+ &
                     AMCIN(4,IY)*(QMCIN(4,IY))+ &
                     AMCIN(5,IY)*(QMCIN(5,IY))+ &
                     AMCIN(6,IY)*(QMCIN(6,IY))+ &
                     AMCIN(7,IY)*(QMCIN(7,IY))+ &
                     AMCIN(8,IY)*(QMCIN(8,IY))+ &
                     AMCIN(9,IY)*(QMCIN(9,IY))+ &
                     AMCIN(10,IY)*(QMCIN(10,IY)))/ &
                     (QMCIN(11,IY))
      END DO

      DO IR=1,MNUMCR
        DO IY=16,MNUMYR
! adjust weighted average price to exclude imports from quantities because imports not included in prices
! note that we are also not including all coal-to-liquid coal, so PCLAS should be the steam coal average price
          ACLAS(IR,IY)=(ACLRS(IR,IY)*QCLRS(IR,IY)+ACLCM(IR,IY)*QCLCM(IR,IY)+ &
                        ACLIN(IR,IY)*(QCLIN(IR,IY)-QCTLRF(IR,IY))+ &
                        ACLEL(IR,IY)*QCLEL(IR,IY))/ &
             (QCLRS(IR,IY)+QCLCM(IR,IY)+QCLIN(IR,IY)-QCTLRF(IR,IY)+QCLEL(IR,IY))
        END DO
      END DO

      RETURN
      END

      SUBROUTINE FINFLATE
      IMPLICIT NONE
      include 'parametr'
      include 'qblk'
      include 'ampblk'          !  use adjusted prices
      include 'emmparm'
      include 'pmmrpt'
      include 'pmmftab'
      include 'lfmmout'
      include 'ogsmout'
      include 'ngtdmrep'
      include 'angtdm'
      include 'macout'
      include 'uefpout'
      include 'uefdout'
      include 'udatout'
      include 'uecpout'
      include 'uettout'
      include 'efpout'
      include 'ncntrl'
      include 'cdsparms'
      include 'coalout'
      include 'coalrep'
      include 'intout'
      include 'pmmout'
      include 'emission'
      include 'ghgrep'
      include 'aponroad'
      include 'tranrep'
      include 'ftable'
      include 'wrenew'
      include 'coalprc'
      include 'aeusprc'
      include 'rggi'

      INTEGER I,IY,IR
      REAL AGSCALPR
      COMMON /AGSC/ AGSCALPR
    


! --- MULTIPLIES VALUES IN 87 DOLLARS BY DEFLATOR TO GET DOLLARS IN THE USER SPECIFIED YEAR
      DO 12 I=1,MPSIZE
        MPARRY(I) = MPARRY(I) * SCALPR  ! Adjusted Prices
12    CONTINUE
      PMOREQ = PMOREQ * SCALPR

! convert new regional carbon dioxide emission variables - have to divide by 1000 either way
      em_resd = em_resd / 1000.
      em_comm = em_comm / 1000.
      em_indy = em_indy / 1000.
      em_tran = em_tran / 1000.
      em_elec = em_elec / 1000.
      COPCCS = COPCCS / 44. * 12.    ! do this before calling carbflate so it can be carbflated, too
      IF (TRIM(CARBON_OR_2) .EQ. 'CO2') CALL CARBFLATE  ! inflate carbon to carbon dioxide if requested

      WAGEPHOUR = WAGEPHOUR * SCALPR / MC_JPGDP(1992 - 1989)
      EMELPSO2 = EMELPSO2 * SCALPR
      ECP_PSO2 = ECP_PSO2 * SCALPR
      ECP_PHG = ECP_PHG * SCALPR
      EPNOXPR = EPNOXPR * SCALPR
      EMEL_PHG = EMEL_PHG * SCALPR
      OGPNGWHP = OGPNGWHP * SCALPR
      OGPCRWHP = OGPCRWHP * SCALPR
      PUSNG = PUSNG * SCALPR
      PINTLNG = PINTLNG * SCALPR
      PTRANSNG = PTRANSNG * SCALPR
      CPSB = CPSB * SCALPR
      WTI_PRICE = WTI_PRICE * SCALPR
      PROFIT_BBL = PROFIT_BBL * SCALPR
      RFSDSTR = RFSDSTR * SCALPR
      RFSMGTR = RFSMGTR * SCALPR
      RFSJFTR = RFSJFTR * SCALPR
      BRENT_PRICE = BRENT_PRICE * SCALPR
      START_PRICE = START_PRICE * SCALPR
      IT_WOP = IT_WOP * SCALPR
      OS_WOP = OS_WOP * SCALPR
      WS_RBOB = WS_RBOB * SCALPR
      FEEDSTOCKS(2,:,:) = FEEDSTOCKS(2,:,:) * SCALPR
      INTERMEDIATE(2,:,:) = INTERMEDIATE(2,:,:) * SCALPR
      REFINE_PROD(2,:,:) = REFINE_PROD(2,:,:) * SCALPR
      GROSS_IMPORT(2,:,:) = GROSS_IMPORT(2,:,:) * SCALPR
      GROSS_EXPORT(2,:,:) = GROSS_EXPORT(2,:,:) * SCALPR
      DOM_CONSUME(2,:,:) = DOM_CONSUME(2,:,:) * SCALPR
      RFIPQCLL(:,:,1) = RFIPQCLL(:,:,1) * SCALPR
      RFIPQMG(:,:,1) = RFIPQMG(:,:,1) * SCALPR
      RFIPQDS(:,:,1) = RFIPQDS(:,:,1) * SCALPR
      RFIPQRL(:,:,1) = RFIPQRL(:,:,1) * SCALPR
      RFIPQRH(:,:,1) = RFIPQRH(:,:,1) * SCALPR
      RFIPQJF(:,:,1) = RFIPQJF(:,:,1) * SCALPR
      RFIPQPC(:,:,1) = RFIPQPC(:,:,1) * SCALPR
      RFIPQPR(:,:,1) = RFIPQPR(:,:,1) * SCALPR
      RFIPQPY(:,:,1) = RFIPQPY(:,:,1) * SCALPR
      RFIPQET(:,:,1) = RFIPQET(:,:,1) * SCALPR
      RFIPQBU(:,:,1) = RFIPQBU(:,:,1) * SCALPR
      RFIPQIS(:,:,1) = RFIPQIS(:,:,1) * SCALPR
      RFIPQPP(:,:,1) = RFIPQPP(:,:,1) * SCALPR
      RFIPQDL(:,:,1) = RFIPQDL(:,:,1) * SCALPR
      RFIPQDU(:,:,1) = RFIPQDU(:,:,1) * SCALPR
      RFIPQPF(:,:,1) = RFIPQPF(:,:,1) * SCALPR
      RFIPQAS(:,:,1) = RFIPQAS(:,:,1) * SCALPR
      RFIPQAG(:,:,1) = RFIPQAG(:,:,1) * SCALPR
      RFIPQLU(:,:,1) = RFIPQLU(:,:,1) * SCALPR
      RFIPQCG(:,:,1) = RFIPQCG(:,:,1) * SCALPR
      RFIPQCD(:,:,1) = RFIPQCD(:,:,1) * SCALPR
      RFIPQRG(:,:,1) = RFIPQRG(:,:,1) * SCALPR
      RFIPQMN3(:,:,1) = RFIPQMN3(:,:,1) * SCALPR
      RFIPQGO3(:,:,1) = RFIPQGO3(:,:,1) * SCALPR
      RFIPQAR3(:,:,1) = RFIPQAR3(:,:,1) * SCALPR
      RFIPQRBOB(:,:,1) = RFIPQRBOB(:,:,1) * SCALPR
      RFIPQCBOB(:,:,1) = RFIPQCBOB(:,:,1) * SCALPR
      RFPQUFC(:,:,1) = RFPQUFC(:,:,1) * SCALPR
      MUFTAX = MUFTAX * SCALPR
      OGWPRNG = OGWPRNG * SCALPR
      NGIMPPRC = NGIMPPRC * SCALPR
      NGEXPPRC = NGEXPPRC * SCALPR
      OGPNGIMP = OGPNGIMP * SCALPR
      OGPNGEXP = OGPNGEXP * SCALPR
      OGCNPPRD = OGCNPPRD * SCALPR
      OGCOWHP = OGCOWHP * SCALPR
      OGNGWHP = OGNGWHP * SCALPR
      OGHHPRNG = OGHHPRNG * SCALPR
      NGSPOT_EMM = NGSPOT_EMM * SCALPR
      OGCO2PRC = OGCO2PRC * SCALPR / MC_JPGDP(2008-1989)
      OGCO2PRCs = OGCO2PRCs * SCALPR / MC_JPGDP(2008-1989)
      OGCO2PEM = OGCO2PEM * SCALPR / MC_JPGDP(2008-1989)
      OGCO2PLF = OGCO2PLF * SCALPR / MC_JPGDP(2008-1989)
      EXSPEND = EXSPEND * SCALPR
      DVSPEND = DVSPEND * SCALPR

      PECRSRLN = PECRSRLN * SCALPR
      PECRSMEN = PECRSMEN * SCALPR
      PECRSTXN = PECRSTXN * SCALPR
      PECRSTDN = PECRSTDN * SCALPR
      PECCMRLN = PECCMRLN * SCALPR
      PECCMMEN = PECCMMEN * SCALPR
      PECCMTXN = PECCMTXN * SCALPR
      PECCMTDN = PECCMTDN * SCALPR
      PECINRLN = PECINRLN * SCALPR
      PECINMEN = PECINMEN * SCALPR
      PECINTXN = PECINTXN * SCALPR
      PECINTDN = PECINTDN * SCALPR
      PECASRLN = PECASRLN * SCALPR
      PECASMEN = PECASMEN * SCALPR
      PECASTXN = PECASTXN * SCALPR
      PECASTDN = PECASTDN * SCALPR

      PELRSNR = PELRSNR * SCALPR
      PELCMNR = PELCMNR * SCALPR
      PELINNR = PELINNR * SCALPR
      PELTRNR = PELTRNR * SCALPR
      PELASNR = PELASNR * SCALPR
      PELFLNR = PELFLNR * SCALPR
      PELOMNR = PELOMNR * SCALPR
      PELWHNR = PELWHNR * SCALPR
      PECGENN = PECGENN * SCALPR
      PECDISN = PECDISN * SCALPR
      PECTRNN = PECTRNN * SCALPR

      UTDMDF = UTDMDF * SCALPR
      UTDMDE = UTDMDE * SCALPR
      UTEXDE = UTEXDE * SCALPR
      UTEXDF = UTEXDF * SCALPR
      PELAV = PELAV * SCALPR
      PELCP = PELCP * SCALPR
      PELFL = PELFL * SCALPR
      PELOM = PELOM * SCALPR
      PELWH = PELWH * SCALPR
      PELME = PELME * SCALPR
      PELTL = PELTL * SCALPR
      PELBS = PELBS * SCALPR
      PCLEX = PCLEX * SCALPR
      PALMG = PALMG * SCALPR
      PALBOB = PALBOB * SCALPR
      PDSTRHWY = PDSTRHWY * SCALPR
      PDS = PDS * SCALPR
      PDSU = PDSU * SCALPR
      PDSL = PDSL * SCALPR
      PDSCRB = PDSCRB * SCALPR
      PJF = PJF * SCALPR
      MGMUTR = MGMUTR * SCALPR
      DSMURS = DSMURS * SCALPR
      DSMUTR = DSMUTR * SCALPR
      JFMUTR = JFMUTR * SCALPR
      RFENVFX = RFENVFX * SCALPR
      RFS_WAIVER = RFS_WAIVER * SCALPR
      E85ICCREDIT = E85ICCREDIT * SCALPR
      CRNPRICE = CRNPRICE * AGSCALPR !td
    
      BIODPRICE = BIODPRICE * SCALPR
      BIOBUTEPRICE = BIOBUTEPRICE * SCALPR
      SBO_PRICE = SBO_PRICE * AGSCALPR !esh
      YGR_PRICE = YGR_PRICE * SCALPR
      RFCRUDEWHP = RFCRUDEWHP * SCALPR

      PGFELGR = PGFELGR * SCALPR
      PGCELGR = PGCELGR * SCALPR
      PGIELGR = PGIELGR * SCALPR
      PGFTRFV = PGFTRFV * SCALPR
      PGFTRPV = PGFTRPV * SCALPR
      PGLTRFV = PGLTRFV * SCALPR
      PGLTRPV = PGLTRPV * SCALPR
      PGFTRRAIL = PGFTRRAIL * SCALPR
      PGLTRRAIL = PGLTRRAIL * SCALPR
      PGFTRSHIP = PGFTRSHIP * SCALPR
      PGLTRSHIP = PGLTRSHIP * SCALPR
      SPGFELGR = SPGFELGR * SCALPR
      SPGIELGR = SPGIELGR * SCALPR
      SPNGELGR = SPNGELGR * SCALPR
      PNGELGR = PNGELGR * SCALPR
      PBAJA = PBAJA * SCALPR

!  end use sector electricity prices
      PELSHRS = PELSHRS * SCALPR
      PELCLRS = PELCLRS * SCALPR
      PELWHRS = PELWHRS * SCALPR
      PELCKRS = PELCKRS * SCALPR
      PELCDRS = PELCDRS * SCALPR
      PELRFRS = PELRFRS * SCALPR
      PELFZRS = PELFZRS * SCALPR
      PELLTRS = PELLTRS * SCALPR
      PELOTRS = PELOTRS * SCALPR
      PELH2RS = PELH2RS * SCALPR
      PELSHCM = PELSHCM * SCALPR
      PELSCCM = PELSCCM * SCALPR
      PELWHCM = PELWHCM * SCALPR
      PELVTCM = PELVTCM * SCALPR
      PELCKCM = PELCKCM * SCALPR
      PELLTCM = PELLTCM * SCALPR
      PELRFCM = PELRFCM * SCALPR
      PELOPCM = PELOPCM * SCALPR
      PELONCM = PELONCM * SCALPR
      PELOTCM = PELOTCM * SCALPR
      PELINP = PELINP * SCALPR
      PELINS = PELINS * SCALPR
      PELINM = PELINM * SCALPR
      PELLTTR = PELLTTR * SCALPR
      PELVHTR = PELVHTR * SCALPR

      PCLELCDR = PCLELCDR * SCALPR
      UPCOALAVG = UPCOALAVG * SCALPR

      UPCOLPRC = UPCOLPRC * SCALPR
      UPGASPRC = UPGASPRC * SCALPR
      UPDISPRC = UPDISPRC * SCALPR
      UPRESPRC = UPRESPRC * SCALPR
      UPRWDNR = UPRWDNR * SCALPR
      UPRWDCR = UPRWDCR * SCALPR
      PABSULF = PABSULF * SCALPR
      PALSULF = PALSULF * SCALPR
      PIBSULF = PIBSULF * SCALPR
      PILSULF = PILSULF * SCALPR
      PWBSULF = PWBSULF * SCALPR
      PWSSULF = PWSSULF * SCALPR
      PWLSULF = PWLSULF * SCALPR
      PAPSULF = PAPSULF * SCALPR
      PCLSULF = PCLSULF * SCALPR
      PWPSULF = PWPSULF * SCALPR
      PMTDP = PMTDP * SCALPR
      PMTSP = PMTSP * SCALPR
      DSSTTX = DSSTTX * SCALPR
      MGSTTX = MGSTTX * SCALPR
      JFSTTX = JFSTTX * SCALPR
      PETHM = PETHM * SCALPR
      PETHANOL = PETHANOL * SCALPR
      PBMET = PBMET * SCALPR
      PBMRFET = PBMRFET * SCALPR
      RFIMPEXPEND = RFIMPEXPEND * SCALPR
      ACGPR_RESCOM = ACGPR_RESCOM * SCALPR
      UDTAR = UDTAR * SCALPR

! overnight cost variables
      UPIGCCST = UPIGCCST * SCALPR
      UPISCCST = UPISCCST * SCALPR
      UPPCCCST = UPPCCCST * SCALPR
      UPACCCST = UPACCCST * SCALPR
      UPCSCCST = UPCSCCST * SCALPR
      UPCCCCST = UPCCCCST * SCALPR
      UPATCCST = UPATCCST * SCALPR
      UPCTCCST = UPCTCCST * SCALPR
      UPANCCST = UPANCCST * SCALPR
      UPFCCCST = UPFCCCST * SCALPR
      UPHYCCST = UPHYCCST * SCALPR
      UPGTCCST = UPGTCCST * SCALPR
      UPMSCCST = UPMSCCST * SCALPR
      UPWDCCST = UPWDCCST * SCALPR
      UPSOCCST = UPSOCCST * SCALPR
      UPPVCCST = UPPVCCST * SCALPR
      UPWNCCST = UPWNCCST * SCALPR
      UPWFCCST = UPWFCCST * SCALPR
      UPDBCCST = UPDBCCST * SCALPR
      UPDPCCST = UPDPCCST * SCALPR

      UPOCCCST = UPOCCCST * SCALPR
      UPI2CCST = UPI2CCST * SCALPR
      UPPQCCST = UPPQCCST * SCALPR
      UPNGCCST = UPNGCCST * SCALPR
      UPICCCST = UPICCCST * SCALPR
      UPT2CCST = UPT2CCST * SCALPR
      UPA2CCST = UPA2CCST * SCALPR
      UPSMCCST = UPSMCCST * SCALPR
      UPGNCCST = UPGNCCST * SCALPR
      UPBICCST = UPBICCST * SCALPR
      UPAGCCST = UPAGCCST * SCALPR
      UPHOCCST = UPHOCCST * SCALPR
      UPHICCST = UPHICCST * SCALPR
      UPTICCST = UPTICCST * SCALPR
      UPSQCCST = UPSQCCST * SCALPR
      UPDSCCST = UPDSCCST * SCALPR
      UPZSCCST = UPZSCCST * SCALPR
      UPWLCCST = UPWLCCST * SCALPR
      UPSSCCST = UPSSCCST * SCALPR
      UPS2CCST = UPS2CCST * SCALPR
      UPPTCCST = UPPTCCST * SCALPR
      UPINCCST = UPINCCST * SCALPR

! some transportation prices that are in 1990 dollars

      PRI = PRI / 1000. / MC_JPGDP(1) * SCALPR
      LDVPRI = LDVPRI / 1000. / MC_JPGDP(1) * SCALPR
      AVG_PRC_CAR = AVG_PRC_CAR / MC_JPGDP(1) * SCALPR
      AVG_PRC_TRK = AVG_PRC_TRK / MC_JPGDP(1) * SCALPR
      AVG_PRC_VEH = AVG_PRC_VEH / MC_JPGDP(1) * SCALPR
      FULL_PRC_CAR = FULL_PRC_CAR / MC_JPGDP(1) * SCALPR
      FULL_PRC_TRK = FULL_PRC_TRK / MC_JPGDP(1) * SCALPR
      FULL_PRC_VEH = FULL_PRC_VEH / MC_JPGDP(1) * SCALPR

      PBMPWCL = PBMPWCL * SCALPR
      !td ag deflator for biomass; take it back to $87 then use Ag deflator
      PBMPWCL(3,:,:)=PBMPWCL(3,:,:)/SCALPR
      PBMPWCL(3,:,:)=PBMPWCL(3,:,:)*AGSCALPR
      PBMETCL = PBMETCL * SCALPR
      PBMBTCL = PBMBTCL * SCALPR

      P_CRUDE_IMPORTS = P_CRUDE_IMPORTS * SCALPR
      P_CRUDE_EXPORTS = P_CRUDE_EXPORTS * SCALPR
      P_RFCRUDEINP = P_RFCRUDEINP * SCALPR
      P_CRUDE_TO_CAN = P_CRUDE_TO_CAN * SCALPR
      RFSCREDPRC = RFSCREDPRC * SCALPR
      LCFS_Offset_Prc = LCFS_Offset_Prc * SCALPR / 1000.

!  Regional Greenhouse Gas Initiative prices
      RG_ALLOW_P = RG_ALLOW_P * SCALPR
      RG_AUCTION_P = RG_AUCTION_P * SCALPR
      RG_RSRVECR_P = RG_RSRVECR_P * SCALPR
      RG_RESERVE_P = RG_RESERVE_P * SCALPR

! net negative revenuc variables
      UNRVCOL = UNRVCOL * SCALPR
      UNRVCCY = UNRVCCY * SCALPR
      UNRVSTM = UNRVSTM * SCALPR
      UNRVNUC = UNRVNUC * SCALPR

! grid resilience price variables
      UGRD_PRC = UGRD_PRC * SCALPR
      UGRD_PRG = UGRD_PRG * SCALPR

      CALL FINUFLATE

      RETURN
      END

      SUBROUTINE FINUFLATE
      IMPLICIT NONE
! same as FINFLATE, but inflates unadjusted prices, except coalprc which is all backwards
      include 'parametr'
      include 'ncntrl'
      include 'qblk'
      include 'mpblk'
      include 'ngtdmout'
      include 'acoalprc'
      include 'macout'
      include 'ponroad'
      include 'ftable'
      include 'eusprc'

      INTEGER I,IR,IY


! --- MULTIPLIES VALUES IN 87 DOLLARS BY DEFLATOR TO GET DOLLARS IN THE USER SPECIFIED YEAR
      DO 12 I=1,MPSIZE
        MPARRY(I) = MPARRY(I) * SCALPR  ! Adjusted Prices
12    CONTINUE
      PMOREQ = PMOREQ * SCALPR

      PDSTRHWY = PDSTRHWY * SCALPR
      PGFELGR = PGFELGR * SCALPR
      PGCELGR = PGCELGR * SCALPR
      PGIELGR = PGIELGR * SCALPR

      PGFTRFV = PGFTRFV * SCALPR
      PGFTRPV = PGFTRPV * SCALPR
      PGLTRFV = PGLTRFV * SCALPR
      PGLTRPV = PGLTRPV * SCALPR
      PGFTRRAIL = PGFTRRAIL * SCALPR
      PGLTRRAIL = PGLTRRAIL * SCALPR
      PGFTRSHIP = PGFTRSHIP * SCALPR
      PGLTRSHIP = PGLTRSHIP * SCALPR
      SPGFELGR = SPGFELGR * SCALPR
      SPGIELGR = SPGIELGR * SCALPR
      SPNGELGR = SPNGELGR * SCALPR
      PNGELGR = PNGELGR * SCALPR
      PBAJA = PBAJA * SCALPR

      PCLELCDR = PCLELCDR * SCALPR

!  need to carry these variables from ngtdmout to the adjusted price tables
!  (which is nearly all of them) because the adjusted price include file
!  angtdm omits the quantities and we need them for weighted averages.
      fGFTRFV = QGFTRFV
      fGFTRPV = QGFTRPV
      fGLTRFV = QGLTRFV
      fGLTRPV = QGLTRPV
      fGFTRRAIL = QGFTRRAIL
      fGLTRRAIL = QGLTRRAIL
      fGFTRSHIP = QGFTRSHIP
      fGLTRSHIP = QGLTRSHIP

!  end use sector electricity prices
      PELSHRS = PELSHRS * SCALPR
      PELCLRS = PELCLRS * SCALPR
      PELWHRS = PELWHRS * SCALPR
      PELCKRS = PELCKRS * SCALPR
      PELCDRS = PELCDRS * SCALPR
      PELRFRS = PELRFRS * SCALPR
      PELFZRS = PELFZRS * SCALPR
      PELLTRS = PELLTRS * SCALPR
      PELOTRS = PELOTRS * SCALPR
      PELH2RS = PELH2RS * SCALPR
      PELSHCM = PELSHCM * SCALPR
      PELSCCM = PELSCCM * SCALPR
      PELWHCM = PELWHCM * SCALPR
      PELVTCM = PELVTCM * SCALPR
      PELCKCM = PELCKCM * SCALPR
      PELLTCM = PELLTCM * SCALPR
      PELRFCM = PELRFCM * SCALPR
      PELOPCM = PELOPCM * SCALPR
      PELONCM = PELONCM * SCALPR
      PELOTCM = PELOTCM * SCALPR
      PELINP = PELINP * SCALPR
      PELINS = PELINS * SCALPR
      PELINM = PELINM * SCALPR
      PELLTTR = PELLTTR * SCALPR
      PELVHTR = PELVHTR * SCALPR

      RETURN
      END

!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      function indcarb(con,iy)
      implicit none
!   designed for use by ftab:
!   calculates carbon emissions for a industrial sector for a given year
!   "con" is one of the arrays in indrep holding fuel
!          consumption for the sector
!   iy is the nems year index
!   indcarb returns the result in mmtons of carbon

      include 'parametr'
      include 'ftable'
      include 'ncntrl'
      include 'qblk'
      include 'emmparm'
      include 'emission'
      include 'emeblk'
      include 'ghgrep'
      real con(18,5)
      real emis(18)
      real divs(4,2)
      integer i,iy,icy,ir,DS,DE
      integer iyy
      real indcarb
      real fsum
      external fsum   ! function fsum sums the first dimension of an array

      REAL C_CO2_FACTOR

      C_CO2_FACTOR = 1.
      IF (TRIM(CARBON_OR_2) .EQ. 'CO2') C_CO2_FACTOR = 44. / 12.

! emel calculated in epm.
!
! emission factors only defined from 95 on, so use 95 for prior years
      iyy=max(iy,6)
      icy=iy+baseyr-1
      emis(1)=em_elec(8,11,icy)/qelas(11,iy)       ! elec note: qelas in quads here
      emis(2)=engin(iyy)   * C_CO2_FACTOR          ! ngas
      emis(3)=eclin(iyy)   * C_CO2_FACTOR          ! steam coal
      emis(4)=emcin(iyy)   * C_CO2_FACTOR          ! met coal
      emis(5)=eclin(iyy)   * C_CO2_FACTOR          ! net coke import (steel only)
      emis(6)=erlin(iyy)   * C_CO2_FACTOR          ! resid (low sulphur)
      emis(7)=edsin(iyy)   * C_CO2_FACTOR          ! distillate
      emis(8)=elgin(iyy)   * C_CO2_FACTOR          ! liquid petrol gases
      emis(9)=emgin(iyy)   * C_CO2_FACTOR          ! motor gasoline
      emis(10)=esgin(iyy)  * C_CO2_FACTOR          ! still gas (refine only)
      emis(11)=epcin(iyy)  * C_CO2_FACTOR          ! petroleum coke
      emis(12)=0.0         * C_CO2_FACTOR         ! asphalt, all sequestered
      emis(13)=epfin(iyy)  * C_CO2_FACTOR          ! petroleum feedstock
      emis(14)=eksin(iyy)  * C_CO2_FACTOR          ! kerosene
      emis(15)=eotin(iyy)  * C_CO2_FACTOR          ! other ind
      emis(16)=enqngpf(iyy)  * C_CO2_FACTOR        ! nat gas feed stocks
      emis(17)=enqlgpf(iyy)  * C_CO2_FACTOR        ! lpg feedstock
      emis(18)=0.          * C_CO2_FACTOR         ! renewables, all sequestered
! multiply fuel use in trill btu times emission factor.
! and divide by 1000
      indcarb=0.
      do i =2,18
        indcarb=indcarb+con(i,5)*emis(i)/1000.
      enddo
! calculate electricity emissions on a census region basis
      divs(1:4,1)=(/1,3,5,8/)
      divs(1:4,2)=(/2,4,7,9/)
      i=1
      do ir=1,4

        DS=divs(ir,1) ! starting census division number for this region
        DE=divs(ir,2) ! ending census divsion number for this region

        emis(1)=sum(em_elec(8,DS:DE,icy))/sum(qelas(DS:DE,iy))       ! elec note: qelas in quads here

        indcarb=indcarb+con(i,ir)*emis(i)/1000.
      enddo
      return
      END
!
!     FUNCTION to Calculate the Net Present Value of a Vector of Numbers
!
!     D(I) Vector of Numbers to be Present Valued
!     XIR Discount Rate
!     N is size of the array
!     S is the first position to be used in the NPV
!     E is the last position to be used in the NPV
!     A is any additional discount periods to be included
!
      FUNCTION NPV(XIR,D,S,E,A,N)
!
      IMPLICIT NONE
!
      INTEGER N,M,I,J,S,E,A
      REAL*4 NPV
      REAL*4 D(N)
      REAL*4 XIR
      NPV = 0.0
!
      DO J = S , E
         I = J - S + A + 1
         NPV = NPV + D(J)/((1 + XIR) ** I)
      END DO
      END

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
      include 'mpblk'
      include 'ngtdmrep'
      include 'indrep'
      include 'convfact'
 
      INTEGER IY,IC,IVAR
     
!
! +++ FIRST sum NEMS QUANTITIES Across Regions for the current year.
      DO 20 IY=1,MNUMYR
        
         DO IVAR=1,MNUMQ
            CALL SUMARY(MQTY(1,IY,IVAR),MNUMCR)
         ENDDO
         DO 10 IC=1,MNUMCR
         
    
            QELAS(IC,IY)=QELRS(IC,IY)+QELCM(IC,IY)+QELTR(IC,IY)+QELIN(IC,IY)+QELHM(IC,IY)
            QGFAS(IC,IY)=QGFRS(IC,IY)+QGFCM(IC,IY)+QGFTR(IC,IY)+QGFIN(IC,IY)+QGFEL(IC,IY)+QGFHM(IC,IY)
            QGIAS(IC,IY)=QGIRS(IC,IY)+QGICM(IC,IY)+QGITR(IC,IY)+QGIIN(IC,IY)+QGIEL(IC,IY)+QGIHM(IC,IY)
            QNGAS(IC,IY)=QNGRS(IC,IY)+QNGCM(IC,IY)+QNGTR(IC,IY)+QNGIN(IC,IY)+QNGEL(IC,IY)+QNGHM(IC,IY)
            QCLAS(IC,IY)=QCLRS(IC,IY)+QCLCM(IC,IY)+QCLIN(IC,IY)+QCLEL(IC,IY)+QCLHM(IC,IY)
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
            QBMAS(IC,IY)=QBMRS(IC,IY)+QBMCM(IC,IY)+QBMIN(IC,IY)+QBMEL(IC,IY)+QBMSN(IC,IY)+QBMHM(IC,IY)
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
                         QMETR(IC,IY)+QETTR(IC,IY)+QHYTR(IC,IY)

! +++ Total Refinery:
            QTPRF(IC,IY)=QDSRF(IC,IY)+QLGRF(IC,IY)+QRSRF(IC,IY)+QSGRF(IC,IY)+QPCRF(IC,IY)+QCCRF(IC,IY)+QOTRF(IC,IY)
            QTSRF(IC,IY)=QELRF(IC,IY)+QNGRF(IC,IY)+QCLRF(IC,IY)+QTPRF(IC,IY)+QBMRF(IC,IY)

! +++ Total Industrial:
            QTPIN(IC,IY)=QMGIN(IC,IY)+QDSIN(IC,IY)+QKSIN(IC,IY)+QLGIN(IC,IY)+QRSIN(IC,IY)+ &
                         QPFIN(IC,IY)+QSGIN(IC,IY)+QPCIN(IC,IY)+QASIN(IC,IY)+QOTIN(IC,IY)
            QTRIN(IC,IY)=QHOIN(IC,IY)+QGEIN(IC,IY)+QBMIN(IC,IY)+QMSIN(IC,IY)+QSTIN(IC,IY)+ &
                         QPVIN(IC,IY)+QWIIN(IC,IY)
            QTSIN(IC,IY)=QELIN(IC,IY)+QNGIN(IC,IY)+QLPIN(IC,IY)+QNGLQ(IC,IY)+   &
                         QCLIN(IC,IY)+QMCIN(IC,IY)+ &
                         QCIIN(IC,IY)+QTPIN(IC,IY)+QTRIN(IC,IY) - OGSUPGAS(1,IC,IY) * CFNGC(IY)

! +++ Total Electric Utility:
            QTPEL(IC,IY)=QDSEL(IC,IY)+QRSEL(IC,IY)+QPCEL(IC,IY)
            QTREL(IC,IY)=QHOEL(IC,IY)+QGEEL(IC,IY)+QBMEL(IC,IY)+QMSEL(IC,IY)+QSTEL(IC,IY)+ &
                         QPVEL(IC,IY)+QWIEL(IC,IY)
            QTSEL(IC,IY)=QNGEL(IC,IY)+QCLEL(IC,IY)+QTPEL(IC,IY)+QUREL(IC,IY)+QTREL(IC,IY)+QEIEL(IC,IY)

! +++ Total Hydrogen Conversion:
            QTRHM(IC,IY)=QETHM(IC,IY)+QBMHM(IC,IY)
            QTSHM(IC,IY)=QELHM(IC,IY)+QNGHM(IC,IY)+QCLHM(IC,IY)+QETHM(IC,IY)+QURHM(IC,IY)+QBMHM(IC,IY)

! +++ Total Synthetics:
            QTRSN(IC,IY)=QBMSN(IC,IY)
            QTSSN(IC,IY)=QTRSN(IC,IY)

! +++ Sum ALL-SECTOR variables for TOTAL PETROLEUM, TOTAL RENEWABLE, and TOTAL SECTOR
            QTPAS(IC,IY)=QTPRS(IC,IY)+QTPCM(IC,IY)+QTPTR(IC,IY)+QTPIN(IC,IY)+QTPEL(IC,IY)
            QTRAS(IC,IY)=QTRRS(IC,IY)+QTRCM(IC,IY)+QTRTR(IC,IY)+QTRIN(IC,IY)+QTREL(IC,IY)+QTRSN(IC,IY)
            QTSAS(IC,IY)=QTSRS(IC,IY)+QTSCM(IC,IY)+QTSTR(IC,IY)+QTSIN(IC,IY)+QTSEL(IC,IY)+ &
                         QTSSN(IC,IY)-QELAS(IC,IY)-QHYTR(IC,IY)
10       CONTINUE
         DO IVAR=1,MNUMQ
            CALL SUMARY(MQTY(1,IY,IVAR),MNUMCR)
         ENDDO
20    CONTINUE

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

      ARRAY(N)=0.
      DO 10 I=1,N-2
         ARRAY(N)=ARRAY(N)+ARRAY(I)
10    CONTINUE

      RETURN
      END
