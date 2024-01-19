! $Header: M:/default/source/RCS/pack_all.f,v 1.8 2017/03/06 20:33:29 dsa Exp $
      PROGRAM PACK_EM_ALL
      use dfport

!     This program initializes OML, loads an ACTFILE and creates an Analyze pack file

!     PARAMETERS:
!
!     OML LIBRARY FUNCTIONS:  WFINIT WFDEF, WFLOAD, WFINSRT, WFOPT
!                             DFOPEN, DFPINIT, DFSINIT, DFMINIT, DFMSTAT, DFMEND, DFCLOSE
!
!     OML.XSLPNZ                Number of revises allowed to matrix before each new optimization
!     OML.XSOLPRNT              Flag to print solution
!     ACTFIL                    DDName containing ACTFILE input
!     PROBLEM_NAME              OML DB containing LP problem
!     BASISIN                   DDName containing basis file (use output basis here)

      IMPLICIT NONE

      include 'omlall.fi'
      
      real(4) ::     NIOBUF     ! SIZE OF WORKSPACE FOR ALL OF OML
      real(4) ::     NPMMIO     ! SIZE OF WORKSPACE FOR PMM MATRIX
      real(4) ::     NEMMIO     ! SIZE OF WORKSPACE FOR EMM MATRIX
      real(4) ::     NCESIO     ! SIZE OF WORKSPACE FOR CES MATRIX
      real(4) ::     NEFDIO     ! SIZE OF WORKSPACE FOR EFD MATRIX
      real(4) ::     NLNGIO     ! SIZE OF WORKSPACE FOR LNG MATRIX
      real(4) ::     NHMMIO     ! SIZE OF WORKSPACE FOR HMM MATRIX

                 
       PARAMETER (NIOBUF=20500000.,NPMMIO=2300000.,NEMMIO=15000000.,NCESIO=3000000.,NEFDIO=3200000.,NLNGIO=200000.,NHMMIO=400000.)
 

      INTEGER*4  YEAR,I,ILOOP
      INTEGER*4  IRET               ! OML RETURN CODE
      INTEGER*4  LPWORKBYTE         ! SIZE OF LP WORKSPACE IN BYTES
      INTEGER*4  MATRIX_MODE
      INTEGER*4  STATS(9)

!     Identifier structure (black-box) for the OML database file:
      TYPE(OMLDBFILE) DBFILE
!     Identifier structure (black-box) for a problem in the OML database:
      TYPE(OMLDBPROB) DB

!     For dynamically allocated model space, declare a scalar pointer
!        to real(8). Initiallize it to null. Then wfdef will assign it
!        an address if allocation is successful.
!     Specify the desired size (in KB) to wfdef.
      REAL*8, POINTER :: EMMMODEL=>NULL()

      CHARACTER*4    FULLYR
      CHARACTER*3    WHICH_MODEL, MIN_OR_MAX
      CHARACTER*8    BASISYR,ACTFIL,PROBLEM_NAME
      CHARACTER*8    UPBND                       ! BOUND ROW NAME
      CHARACTER*16   UPOBJ                       ! OBJECTIVE FUNCTION NAME
      CHARACTER*16   UPRHS                       ! RIGHT HAND SIDE NAME
      CHARACTER*8    TEMPACT,SOLNAME
      CHARACTER*11   BASISIN,PCKDDN
      CHARACTER*50   MPS

      CHARACTER*3 CmdArgOne
      CHARACTER*4 CmdArgTwo
      INTEGER CmdLen

      CHARACTER*12 cmndfile
      INTEGER      icmdfile/88/
      INTEGER      YEARS_TO_DO(99),HOW_MANY_FILES
      LOGICAL      IEXIST
      CHARACTER*1 version_letter(26)/'a','b','c','d','e','f','g','h','i','j','k','l','m', &
                                     'n','o','p','q','r','s','t','u','v','w','x','y','z'/

!     Null pointer(s) for passing to functions that do not populate them.
      REAL*8, POINTER :: NULLPD=>NULL()   ! for wfinit - database memory pointer
      INTEGER*4 OMLDBKB   ! for wfinit - KB of memory to allocate for OML database
      OMLDBKB = 73*1024   ! = 8*9500000 bytes rounded up to nearest megabyte (2**20), to Kilobyte
      OMLDBKB= ceiling(NIOBUF*8./(2**20))*1024

      call getarg(1,CmdArgOne)
      call getarg(2,CmdArgTwo)
      CmdLen=len_trim(CmdArgTwo)

      IF (CmdLen .gt. 0) THEN
         FULLYR=CmdArgTwo
      ELSE
         WRITE(6,'(1X,"ENTER YEAR (Please) > "$)')
         READ(5,'(A4)') FULLYR
      ENDIF
      CmdLen=len_trim(CmdArgOne)
      IF (CmdLen .gt. 0) THEN
         IF (CmdArgOne .EQ. "cmm" .OR. CmdArgOne .EQ. "CMM") WHICH_MODEL="CMM"
         IF (CmdArgOne .EQ. "emm" .OR. CmdArgOne .EQ. "EMM") WHICH_MODEL="ECP"
         IF (CmdArgOne .EQ. "ecp" .OR. CmdArgOne .EQ. "ECP") WHICH_MODEL="ECP"
         IF (CmdArgOne .EQ. "efd" .OR. CmdArgOne .EQ. "EFD") WHICH_MODEL="EFD"
      ELSE
5000  CONTINUE
      WRITE(6,'(1X,"ENTER MODEL (CMM, ECP, EFD, PMM, XPM) (Please) > "$)')
      READ(5,'(A3)') WHICH_MODEL

      IF (WHICH_MODEL .EQ. "cmm") WHICH_MODEL="CMM"
      IF (WHICH_MODEL .EQ. "ecp") WHICH_MODEL="ECP"
      IF (WHICH_MODEL .EQ. "efd") WHICH_MODEL="EFD"
      IF (WHICH_MODEL .EQ. "pmm") WHICH_MODEL="PMM"
      IF (WHICH_MODEL .EQ. "xpm") WHICH_MODEL="XPM"
      IF (WHICH_MODEL .NE. "CMM" .AND. WHICH_MODEL .NE. "ECP" .AND. WHICH_MODEL .NE. "EFD" .AND. &
          WHICH_MODEL .NE. "PMM" .AND. WHICH_MODEL .NE. "XPM") THEN 
         WRITE(*,'("  You bonehead!  That model was not a valid choice!")')
         GOTO 5000
      ENDIF
      ENDIF
      IF (FULLYR(1:3) .EQ. 'All' .OR. FULLYR(1:3) .EQ. 'all' .OR. FULLYR(1:3) .EQ. 'ALL') THEN

         IEXIST=.TRUE.
         I = 1 
         DO WHILE (IEXIST)
         write(cmndfile,'("cmndfile",A1,".sh")') version_letter(i)
         INQUIRE(FILE=cmndfile,EXIST=IEXIST)
         IF (.NOT. IEXIST) THEN
            write(6,'("version ",a1)') version_letter(i)
         ELSE
            i = i + 1
         ENDIF
         ENDDO
         open(unit=icmdfile,file=cmndfile,STATUS='NEW')
         WRITE(icmdfile,'("ls ",A3,"?20??.act | wc -l > cmdfile",A1,".out")') WHICH_MODEL,version_letter(i)
         WRITE(icmdfile,'("ls ",A3,"?20??.act >> cmdfile",A1,".out")') WHICH_MODEL,version_letter(i)
         close(unit=icmdfile)
         write(6,*) ' Calling system to do this:  ',cmndfile
         iret=system(cmndfile)
         if(iret.ne.0) write(6,'("  Command failed with return code of ",I2,":  ",a)') iret,cmndfile
         write(cmndfile,'("cmdfile",A1,".out")') version_letter(i)
         open(unit=icmdfile,file=cmndfile,STATUS='OLD')
         READ(ICMDFILE,*) HOW_MANY_FILES
         write(6,'(" This many files: ",I2)') HOW_MANY_FILES
         DO I=1,HOW_MANY_FILES
            READ(ICMDFILE,'(4X,I4)') YEARS_TO_DO(I)
            write(6,'(" Will do this year: ",I4)') YEARS_TO_DO(I)
         ENDDO
      ELSE
          READ(FULLYR,'(I4)') YEARS_TO_DO(1)
          HOW_MANY_FILES = 1
      ENDIF

      MIN_OR_MAX="MIN"
      PROBLEM_NAME = "ACTPROB"

      DO ILOOP=1,HOW_MANY_FILES
      WRITE (FULLYR,'(I4)') YEARS_TO_DO(ILOOP)
      IF (WHICH_MODEL .EQ. "CMM" ) THEN
         SOLNAME = "CMM_SOLN"
         UPRHS = "VALS"
         UPBND = "BOUND"
         UPOBJ = "COST"
         ACTFIL = "CMM_" // FULLYR
!        PARAMETER (NCESIO=3000000)
         LPWORKBYTE = 8 * NCESIO              ! DEFINE MODEL SIZE, old
         LPWORKBYTE= ceiling(NCESIO*8./(2**20))*1024
!  Set up basis file and pack file names
         PCKDDN = 'CL'//ACTFIL(7:)//'.PCK'
         BASISYR = 'CMM' // FULLYR
         BASISIN = 'BASCMMO.dat'
      ELSE IF (WHICH_MODEL .EQ. "ECP" ) THEN
         SOLNAME = "ECP_SOLN"
         UPRHS = "RHSECP"
         UPBND = "BND1"
         UPOBJ = "ECPCOSTS"
         ACTFIL = "EMM_" // FULLYR
!       PARAMETER (NEMMIO=9000000)
!        LPWORKBYTE = 8 * NEMMIO              ! DEFINE MODEL SIZE, old
!        LPWORKBYTE= ceiling(NEMMIO*8./(2**20))*1024
         LPWORKBYTE= 1000*1024

!  Set up basis file and pack file names
         PCKDDN = 'EC'//ACTFIL(7:)//'.PCK'
         BASISYR = 'ECP' // FULLYR
         BASISIN = 'BASEMMO.dat'
      ELSE IF (WHICH_MODEL .EQ. "EFD" ) THEN
         SOLNAME = "EFD_SOLN"
         UPRHS = "RHSEFD"
         UPBND = "BOUND"
         UPOBJ = "EFDCOSTS"
         ACTFIL = "EFD_" // FULLYR
!       PARAMETER (NEFDIO=3200000)
         LPWORKBYTE = 8 * NEFDIO              ! DEFINE MODEL SIZE, old
         LPWORKBYTE= ceiling(NEFDIO*8./(2**20))*1024

!  Set up basis file and pack file names
         PCKDDN = 'ED'//ACTFIL(7:)//'.PCK'
         BASISYR = 'EFD' // FULLYR
         BASISIN = 'BASEFDO.dat'
      ELSE IF (WHICH_MODEL .EQ. "PMM" ) THEN
         UPRHS = "RHS"
         UPBND = "BND"
         UPOBJ = "OBJ"
         MIN_OR_MAX = "MAX"
         ACTFIL = "PMM_" // FULLYR
!       PARAMETER (NPMMIO=2300000)
         LPWORKBYTE = 8 * NPMMIO              ! DEFINE MODEL SIZE, old
         LPWORKBYTE= ceiling(NPMMIO*8./(2**20))*1024

!  Set up basis file and pack file names
         PCKDDN = 'PM' // FULLYR(3:4) // '.PCK'
         BASISYR = 'Y' // FULLYR(3:4)
         BASISIN = 'BASPMM1.dat'
      ELSE IF (WHICH_MODEL .EQ. "XPM" ) THEN
         UPRHS = "RHS"
         UPBND = "BND"
         UPOBJ = "OBJ"
         MIN_OR_MAX = "MAX"
         ACTFIL = "XPMM" // FULLYR
!       PARAMETER (NPMMIO=2300000)
         LPWORKBYTE = 8 * NPMMIO              ! DEFINE MODEL SIZE
         LPWORKBYTE= ceiling(NPMMIO*8./(2**20))*1024

!  Set up basis file and pack file names
         PCKDDN = 'XP' // FULLYR(3:4) // '.PCK'
         BASISYR = 'X' // FULLYR(3:4)
         BASISIN = 'BAXPMM1.dat'
      ENDIF
!
      WRITE(6,*) 'BEGIN OML SECTION'

      IRET=0
!      CALL OMLSTART(IRET)                      ! STORED IN omlanal.f
      IRET=WFINIT('NEMS'//char(0),nullpd,OMLDBKB)

      IF (IRET .NE. 0) THEN
         WRITE(6,'(1X,A,I3)') 'OML WFINIT ERROR--LP SPACE ALLOCATION PROBLEM, RETURN CODE= ',IRET
         WRITE(6,'("  Thus, we are stopping right here.")')
         STOP
      ELSE
         WRITE(6,'(1X,a)') 'MAIN: OML WFINIT SUCCESSFUL--DATABASE BUFFER SPACE ALLOCATED'
         !          Obtain access to the optimization subset of the MPSIII "Communication Region" (CR).
         !          Include file wcr.fi (as adapted for NEMS) declares OML as a pointer to a structure of type FWCR.
         !          OML is placed in named common, so wfgetpwcr needs to be called only once.
         CALL WFGETPWCR(OML)
      ENDIF

      WRITE(6,'(" Settings at start:")')
      WRITE(6,*) "   ACTFILE = ", OML.XACTFILE 
      WRITE(6,*) "   ACTPROB = ", OML.XACTPROB 
      WRITE(6,*) "   OBJ = ", OML.XOBJ 
      WRITE(6,*) "   RHS = ", OML.XRHS 
      WRITE(6,*) "   BOUND = ", OML.XBOUND 
      WRITE(6,*) "   SOLFILE = ", OML.XSOLFILE 

! Set OML parameters to limit output to SYSPRNT
      OML.XFREQLOG=10000
      OML.XFREQSUM=10000

! Set ACTFILE and problem name
      OML.XACTFILE = ACTFIL
      OML.XACTPROB = PROBLEM_NAME
      OML.XOBJ = UPOBJ
      OML.XRHS = UPRHS
      OML.XBOUND = UPBND
      OML.XSOLFILE = SOLNAME

      WRITE(6,'(" Settings after variable initialization:")')
      WRITE(6,*) "   ACTFILE = ", OML.XACTFILE 
      WRITE(6,*) "   ACTPROB = ", OML.XACTPROB 
      WRITE(6,*) "   OBJ = ", OML.XOBJ 
      WRITE(6,*) "   RHS = ", OML.XRHS 
      WRITE(6,*) "   BOUND = ", OML.XBOUND 
      WRITE(6,*) "   SOLFILE = ", OML.XSOLFILE 

      WRITE(6,*) 'CALLING DFOPEN'
      IRET = DFOPEN(DBFILE,ACTFIL)
      IF (IRET .NE. 0) WRITE(6,'("  DFOPEN ERROR,CODE=",I4)') IRET

! Specify a problem in database for processing
      WRITE(6,'("CALLING DFPINIT")')
      IRET = DFPINIT(DB,DBFILE,PROBLEM_NAME)
      IF (IRET .NE. 0) WRITE(6,'("  DFPINIT ERROR,CODE=",I4)') IRET

! Retrieve the solution data
!     WRITE(6,'("CALLING DFSINIT")')
!     IRET = DFSINIT(DB,SOLNAME)
!     IF (IRET .NE. 0) WRITE(6,'("  DFSINIT ERROR,CODE=",I4)') IRET

! Set up matrix in revise mode
      MATRIX_MODE = 1
      WRITE(6,'("CALLING DFMINIT")')
      IRET = DFMINIT(DB,MATRIX_MODE)
      IF (IRET .NE. 0) WRITE(6,'("  DFMINIT ERROR,CODE=",I4)') IRET

      IRET = DFMSTAT(STATS)
      WRITE(6,'("  STATS 1 through 9: ",9I8)') (STATS(I),I=1,9)

      WRITE(6,'("CALLING DFMEND")')
      IRET = DFMEND()
      IF (IRET .NE. 0) WRITE(6,'(1X,"  DFMEND ERROR,CODE=",I4)') IRET

      WRITE(6,'("CALLING DFCLOSE")')
      IRET = DFCLOSE(DBFILE)
      IF (IRET .NE. 0) WRITE(6,'(1X,"  DFCLOSE ERROR,CODE=",I4)') IRET

!  End of matrix processing.     Now define and load model.

      EMMMODEL=>NULL()
      WRITE(6,*) 'WFDEF'
      IRET = WFDEF(EMMMODEL,LPWORKBYTE,'EMMECP  ')  ! IRET>0 IS KB ALLOCATED
      IF (IRET .le. 0) WRITE(6,'(1X,"  WFDEF ERROR, CODE=",I4)') IRET
      IF (IRET .Gt. 0) WRITE(6,'(1X,"  WFDEF Successful, memory allocated (KB)=",I10)') IRET

      OML.XFREQINV = 500
      OML.XFREQSUM = 500
      OML.XMINMAX=MIN_OR_MAX
      OML.XOBJ = UPOBJ
      OML.XRHS = UPRHS
      OML.XBOUND = UPBND

      WRITE(6,'("WFLOAD")')
      IRET = WFLOAD(ACTFIL,PROBLEM_NAME)        ! LOAD MATRIX INTO MEMORY
      IF (IRET .NE. 0) WRITE(6,'(1X,"  WFLOAD ERROR, CODE=",I4)') IRET

      WRITE(6,'("WFINSRT")')
      IRET = WFINSRT(BASISIN,BASISYR)           !  LOAD BASIS

      OML.XSOLPRNT = 'NO'
      WRITE(6,'("WFOPT")')
      OML.XRUNMODE = 8
      IRET = WFOPT()                            !  OPTIMIZE MATRIX
      IF (IRET .NE. 0) IRET = WFOPT()
      OML.XRUNMODE = 0
      OML.XCRASHSW = 0
      IRET = WFOPT()

      TEMPACT = OML.XACTFILE
      OML.XACTFILE = ACTFIL
      OML.XRANGE = '        '
      IRET = 0
      WRITE(6,'(1X,"BEFORE GOMHOT:",A4,":",A12,":",I4)') FULLYR,PCKDDN,IRET
      
      !     A negative value for XSLPNZ causes the "whiz directory" to be
      !        maintained on a temporary actfile rather than in memory.
      !        This works MUCH better for solution retrievals based on masks.
      !      OML.XSLPNZ=-1

      
      
      
      CALL GOMHOT(PCKDDN,IRET)
      WRITE(6,'(1X,"AFTER GOMHOT:",A4,":",A12,":",I4)') FULLYR,PCKDDN,IRET
      IF (IRET .NE. 0) WRITE(6,'(1X,"  GOMHOT ERROR,CODE=",I4)') IRET
      OML.XACTFILE = TEMPACT

!      CALL OMLEND(IRET)
      call wfend

      WRITE(6,'(" PACK completed for year ",I4,".")') YEARS_TO_DO(ILOOP)
      ENDDO

      STOP
      END
