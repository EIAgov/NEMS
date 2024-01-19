! $Header: M:/default/source/RCS/filemgr.f,v 1.15 2017/10/06 18:52:29 dsa Exp $
      INTEGER FUNCTION FILE_MGR(FUNCTION,UNIQUE_NAMEIN,NEW)
! *******************************************************************
! *                                                                 *
! *                     FUNCTION FILE_MGR()                         *
! *                                                                 *
! *******************************************************************
! *                                                                 *
! * VALID FUNCTION VALUES:                                          *
! *      'O' - OPEN FILE                                            *
! *      'C' - CLOSE FILE                                           *
! *      'T' - WRITE FILE TABLE                                     *
! *      'I' - INITIALIZATION                                       *
! *      'P' - WRITE OPEN OUTPUT FILES TO BE COMPRESSED             *
! *                                                                 *
! * RETURNS:                                                        *
! * POSITIVE NUMBER = UNIT NUMBER                                   *
! * NEGATIVE NUMBER = ERROR NUMBER                                  *
! *                                                                 *
! * CLOSE        0= FILE CLOSED NORMALLLY                           *
! * OPEN/CLOSE  -1= NO NAME EXISTS                                  *
! * OPEN        -2= COULD NOT ASSIGN UNIT NUMBER TO FILE TO OPEN    *
! * OPEN        -3= FILEINF ALLOCATION ERROR                        *
! *             -4=                                                 *
! * FILE_MGR    -5= INCORRECT FUNCTION SPECIFIED                    *
! * FILE_MGR    -6= INCORRECT PLATFORM                              *
! * INIT        -7= COULD NOT ASSIGN UNIT NUMBER TO FILELIST FILE   *
! * INIT        -8= OPEN ERROR ON FILELIST FILE                     *
! * INIT        -9= READ ERROR ON FILELIST FILE                     *
! * INIT       -10= FILELIST FILE DOES NOT EXIST                    *
! * CLOSE      -11= UNABLE TO CLOSE FILE                            *
! * OPEN       -12= UNABLE TO OPEN FILE                             *
! *            -13=                                                 *
! *            -14=                                                 *
! *            -15=                                                 *
! *                                                                 *
! *******************************************************************
      IMPLICIT NONE
      include'fmgr'

      CHARACTER*1  FUNCTION       !INITIALIZE/OPEN/CLOSE/TABLE (PASSED)
      CHARACTER*(*) UNIQUE_NAMEIN  !UNIQUE NAME OF FILE   (COPIED IN)
      CHARACTER*18 UNIQUE_NAME    !UNIQUE NAME OF FILE   (COPIED IN)
      LOGICAL      NEW            !NEW:TRUE/OLD:FALSE    (PASSED)
      INTEGER*4    STATUS         !STATUS RETURNED FROM FUNCTIONS

      INTEGER*4    I1,I2          !TEMPORARY VARIABLES
      CHARACTER*18 DD_NAME        !DDNAME FOR FILE REQUEST
      CHARACTER*18 PDS_NAME       !PDS MEMBER NAME INCLUDING '()'
      LOGICAL      PDS_FLAG       !.T. FOR PDS FILE REQUEST, ELSE .F.
      INTEGER*4    IOCCUR
      INTEGER*4    ILENGTH

      INTEGER      FMGR_INIT      !FMGR_INIT FUNCTION DEFINE
      INTEGER      FMGR_OPEN      !FMGR_OPEN FUNCTION DEFINE
      INTEGER      FMGR_CLOSE     !FMGR_CLOSE FUNCTION DEFINE
      INTEGER      FMGR_TABLE     !FMGR_TABLE FUNCTION DEFINE
      INTEGER      FMGR_PRESS     !FMGR_PRESS FUNCTION DEFINE
      INTEGER      FINDCHAR       !FINDCHAR FUNCTION DEFINE

      FILE_MGR=0                  !INITIALIZE FUNCTION RETURN
      UNIQUE_NAME=UNIQUE_NAMEIN

!.....DETERMINE IF FILE REQUEST IS FOR A PDS
      PDS_FLAG=.FALSE.                 !INITIALIZE PDS_FLAG
      PDS_NAME=' '
      IOCCUR=1
      ILENGTH=18
      I1=FINDCHAR('(',UNIQUE_NAME,IOCCUR,ILENGTH)
      IF(I1 .GT. 0) THEN               !REQUEST IS FOR A PDS
        PDS_FLAG=.TRUE.
!.......ESTABLISH PDS_NAME
        I2=FINDCHAR(')',UNIQUE_NAME,IOCCUR,ILENGTH)
        PDS_NAME=UNIQUE_NAME(I1:I2)
!.......ESTABLISH DD_NAME
        DD_NAME=UNIQUE_NAME(1:I1-1)
      ENDIF

!.....DETERMINE TYPE OF FUNCTION BEING CALLED
      IF(FUNCTION .EQ. 'I') THEN       !INITIALIZE ARRAYS
!.......OPEN FILELIST FILE AND INITIALIZE THE ARRAYS
        FILE_MGR=FMGR_INIT(NEW)

      ELSEIF(FUNCTION .EQ. 'O') THEN        !OPEN
!.......OPEN FILE
        write(6,*) ' open ',unique_name
        FILE_MGR=FMGR_OPEN(UNIQUE_NAME,DD_NAME,PDS_NAME,PDS_FLAG,NEW)

      ELSEIF(FUNCTION .EQ. 'C') THEN        !CLOSE
!.......CLOSE FILE
        FILE_MGR=FMGR_CLOSE(UNIQUE_NAME,DD_NAME,PDS_NAME,PDS_FLAG)

      ELSEIF(FUNCTION .EQ. 'T') THEN        !FMGR_TABLE
!.......PRINT FILE MANAGER TABLE
        FILE_MGR=FMGR_TABLE()

      ELSE                             !INCORRECT FUNCTION SPECIFIED
!.......RETURN INVALID FUNCTION ERROR
        FILE_MGR=-5                    !SET ERROR TO INCORRECT FUNCTION
      ENDIF

      WRITE(MSG_UNIT,'(A,I4,/)') 'RETURN CODE FROM FILE_MGR()=',FILE_MGR

      RETURN
      END
      INTEGER FUNCTION FMGR_INIT(NEW)
! *******************************************************************
! *                                                                 *
! *                     FUNCTION FMGR_INIT(NEW)                        *
! *                                                                 *
! *******************************************************************
! *                                                                 *
! *FUNCTION INITIALIZES AND ESTABLISHES VALUES FOR THE COMMON ARRAYS*
! *RETURNS 0 = OK                                                   *
! *        7 = COULD NOT ASSIGN UNIT NUMBER TO FILELIST FILE        *
! *        8 = OPEN ERROR ON FILELIST FILE                          *
! *        9 = READ ERROR ON FILELIST FILE                          *
! *        10= FILELIST FILE DOES NOT EXIST                         *
! *                                                                 *
! *******************************************************************
      IMPLICIT NONE
      include'fmgr'

      INTEGER*4 IFILE                   !COUNTS FILE RECORDS
      INTEGER*4 IDATA                   !COUNTS DATA RECORDS
      INTEGER*4 ICOMMENT                !COUNTS COMMENT RECORDS
      INTEGER*4 IRECORD                 !COUNTS TOTAL RECORDS

      INTEGER*4 I,NARG                  !COUNTER VARIABLE
      INTEGER*4 I1,I2,NADJ              !TEMPORARY VARIABLES
      CHARACTER*255 RECREAD             !RECORD READ VARIABLE
      INTEGER*4 FINDCHAR                !FUNCTION FINDCHAR DEFINE
      INTEGER*4 FMGR_TABLE              !FMGR_TABLE FUNCTION DEFINE
      INTEGER*4 IUNIT                   !UNIT NUMBER FOR FILELIST FILE

      INTEGER*4 RETCODE                 !RETURN CODE FOR FUNCTION CALLS
      LOGICAL   IEXIST                  !LOGICAL FOR INQUIRE EXIST

      INTEGER   FILE_MGR                !INITIALIZE FUNCTION FILE_MGR
      logical new
      integer ipos,ii
      character*20 filen
      FMGR_INIT=0                       !INITIALIZE RETURN VALUE

      if(new) then
         filen='FMGROUT.txt'            ! for nems
      else
         filen='FMGROUT.tfiler.txt'     ! for tfiler
      endif

      IF(MSG_UNIT.NE.6) THEN
        OPEN(MSG_UNIT,FILE=filen,STATUS='UNKNOWN')
      ENDIF

!.....ASSIGN .TRUE. FLAGS TO UNIT_AVAIL FOR PC UNIT NUMBER ASSIGNS
      DO II=MIN_UNIT,MAX_UNIT,1
        UNIT_AVAIL(II)=.TRUE.
      ENDDO

!.....ASSIGN UNIT NUMBER TO FILE_MGR DATA FILE
      ii=-1
      CALL UNTNOFD(RETCODE,MIN_UNIT,ii,IUNIT)
        IF(RETCODE.NE.0) THEN
          FMGR_INIT=-7                  !SET RETURN TO ASSIGN ERROR
          WRITE(MSG_STRING,'(3A,I4,1A,I4)') 'UNABLE TO ASSIGN UNIT', &
                              ' NUMBER FOR DATA INPUT FILE. ', &
                              ' UNTNOFD()=',RETCODE, &
                              ' RETURN CODE=',FMGR_INIT
          CALL MSGDISP(ERR_FLAG,MSG_STRING)
        ELSE
          INQUIRE(FILE='FILELIST',EXIST=IEXIST)
          IF(IEXIST) THEN
            OPEN(UNIT=IUNIT,FILE='FILELIST',BLANK='NULL',STATUS='OLD',ERR=200)
          ELSE
            GOTO 202                    !FILELIST FILE DOES NOT EXIST
          ENDIF

          ICOMMENT=0
          IRECORD=0
          IFILE=0
          IDATA=0
!.........BEGIN READ LOOP
  100     READ(IUNIT,'(A)',END=101,ERR=201) RECREAD
            IRECORD=IRECORD+1
            IF(RECREAD(1:1).NE.' ') THEN       !SKIP COMMENT RECORD
              ICOMMENT=ICOMMENT+1
              GOTO 100
            ELSE
!.........FILE NAMES RECORD
              IFILE=IFILE+1
              F_UNIT(IFILE)=0                    !INITIALIZE UNIT NUMBER
              F_IOSTATUS(IFILE)='UNUSED'         !INITIALIZE IOSTATUS
!.............PREVENT FROM GOING BEYOND END OF ARRAYS
              IF(IFILE .GT. MAX_FILES) THEN
                WRITE(MSG_STRING,'(A)') 'PARM MAX_FILES REACHED MAXIMUM IN COMMON'
                GOTO 101
              ENDIF
!  Parse the file information using the space (" ") character.
!  We tried to adjust for more than one space in a row.
              NADJ=0
              DO 151 NARG=1,7
111             I1=FINDCHAR(' ',RECREAD,NARG+NADJ,255)
                IF (I1 .EQ. 0) THEN
                   WRITE(6,*) ' No delimiting characters (spaces) ', &
                     'found on FILELIST record ', IRECORD
                   GOTO 100
                ENDIF
                I2=FINDCHAR(' ',RECREAD,NARG+NADJ+1,255)
                IF((I2-I1).EQ.1) THEN
                  I1 = I2
                  NADJ = NADJ + 1
                  GOTO 111
                ENDIF
                IF(NARG.EQ.1) THEN
                  F_ID(IFILE)=RECREAD(I1+1:I2-1)
                ELSEIF(NARG.EQ.2) THEN
                  F_NAME(IFILE)=RECREAD(I1+1:I2-1)
!    ESTABLISH IF PDS - Designated as such by trailing () in name
                  F_PDS_FLAG(IFILE)=.FALSE.
                  F_PDS_NAME(IFILE)=' '
                  I1=FINDCHAR('(',F_NAME(IFILE),1,93)
                  IF(I1 .NE. 0) THEN
                    F_PDS_FLAG(IFILE)=.TRUE.              !FILE IS A PDS
                    F_IOSTATUS(IFILE)='*PDS*'
                    F_NAME(IFILE)=F_NAME(IFILE)(1:I1-1)   !SO TRIM OFF ()
                  ENDIF
                ELSEIF(NARG.EQ.3) THEN
                  F_IOTYPE(IFILE)=RECREAD(I1+1:I2-1)
                ELSEIF(NARG.EQ.4) THEN
                  F_ACCESS(IFILE)=RECREAD(I1+1:I2-1)
                ELSEIF(NARG.EQ.5) THEN
                  F_FORM(IFILE)=RECREAD(I1+1:I2-1)
                ELSEIF(NARG.EQ.6) THEN
                  CHAR_LRECL=RECREAD(I1+1:I2-1)
                  READ(CHAR_LRECL,*) F_LRECL(IFILE)
                ELSEIF(NARG.EQ.7) THEN
                  F_DESCRIPTION(IFILE)=RECREAD(I1+1:255)
                ENDIF
151           CONTINUE

              GOTO 100
            ENDIF
  101     CONTINUE                !CONTINUE AFTER READING ALL DATA
!   END READ LOOP

!  Close FILELIST and make unit usable.
          CLOSE(UNIT=IUNIT)
          UNIT_AVAIL(IUNIT)=.TRUE.

!.........WRITE SUMMARY STATISTICS
          WRITE(MSG_STRING,'(A)') '*=========FILELIST INPUT COMPLETE========*'
          CALL MSGDISP(MSG_FLAG,MSG_STRING)
          WRITE(MSG_STRING,'(1A,I4,1A)') '* TOTAL RECORDS READ: ',IRECORD,'*'
          CALL MSGDISP(MSG_FLAG,MSG_STRING)
          WRITE(MSG_STRING,'(1A,I4,1A)') '* TOTAL COMMENT RECORDS READ:  ',ICOMMENT,'*'
          CALL MSGDISP(MSG_FLAG,MSG_STRING)
          WRITE(MSG_STRING,'(1A,I4,1A)') '* TOTAL FILE RECORDS READ:  ',IFILE,'*'
          CALL MSGDISP(MSG_FLAG,MSG_STRING)
          WRITE(MSG_STRING,'(1A,I4,1A)') '* TOTAL DATA RECORDS READ:   ',IDATA,'*'
          CALL MSGDISP(MSG_FLAG,MSG_STRING)
          WRITE(MSG_STRING,'(2A)') '*=================================', &
          '=======*'
          CALL MSGDISP(MSG_FLAG,MSG_STRING)
          WRITE(MSG_STRING,'(A)') 'FILE_MGR FILELIST INITIALIZATION COMPLETE'
          CALL MSGDISP(MSG_FLAG,MSG_STRING)

          F_ELEMENTS=IFILE
          RETCODE=FMGR_TABLE()
        ENDIF

      RETCODE=0              !SET RETURN CODE TO OK
      GOTO 250               !SKIP ERROR SETTINGS

!.....RETURN CODE SETTINGS FOR READ OR OPEN ERRORS
  200 CONTINUE
      RETCODE=-8             !SET UNABLE TO OPEN FILELIST FILE ERROR
      WRITE(MSG_STRING,'(A,I4)') 'UNABLE TO OPEN FILE_MGR DATA FILE.  RETURN CODE=',RETCODE
      CALL MSGDISP(ERR_FLAG,MSG_STRING)
      GOTO 250

  201 CONTINUE
      RETCODE=-9             !SET FILELIST FILE READ ERROR
      WRITE(MSG_STRING,'(A,I4)') 'ERROR READING FILE_MGR DATA FILE.  RETURN CODE=',RETCODE
      CALL MSGDISP(ERR_FLAG,MSG_STRING)
      GOTO 250

  202 CONTINUE
      RETCODE=-10            !SET FILELIST FILE DOES NOT EXIST
      WRITE(MSG_STRING,'(A,I4)') 'FILE_MGR DATA FILE DOES NOT EXIST.   RETURN CODE=',RETCODE
      CALL MSGDISP(ERR_FLAG,MSG_STRING)
      GOTO 250

  250 CONTINUE

      RETURN
      END
      INTEGER FUNCTION FMGR_OPEN(UNIQUE_NAME,DD_NAME,PDS_NAME,PDS_FLAG,NEW)
! *******************************************************************
! *                                                                 *
! *                    FUNCTION FMGR_OPEN()                         *
! *                                                                 *
! *******************************************************************
! *                                                                 *
! *FUNCTION OPENS FILES BASED ON PARMS PASSED TO FILE_MGR()         *
! *RETURNS VALUES IN STATUS, NUMBER, AND RETCODE                    *
! *  STATUS:  0 = OK                                                *
! *          -1 = NO NAME EXISTS                                    *
! *             =                                                   *
! *          -5 = INCORRECT FUNCTION SPECIFIED (RETURNED FROM FMGR) *
! *                                                                 *
! *  NUMBER:  0 = FAILED                                            *
! *           # = UNIT NUMBER                                       *
! *******************************************************************
      IMPLICIT NONE
      include 'fmgr'

      CHARACTER*18 UNIQUE_NAME !UNIQUE NAME OF FILE TO OPEN     (PASSED)
      CHARACTER*18 DD_NAME   !                                  (PASSED)
      CHARACTER*18 PDS_NAME  !                                  (PASSED)
      LOGICAL      PDS_FLAG  !                                  (PASSED)
      LOGICAL      NEW       !EXISTENCE OF FILE .T.=NEW .F.=OLD (PASSED)

      LOGICAL      ISOPEN    !FLAG IF FILE IS ALREADY OPEN
      INTEGER      GETINDEX  !GETINDEX FUNCTION
      INTEGER*4    IRCODE    !RETURN CODE FROM FILEINF FUNCTION
      INTEGER*4    INDEX     !INDEX NUMBER FOR UNIQUE_NAME IN ARRAYS
      INTEGER*4    PC_NAME   !FULL PC FILE DRIVE, PATH, AND NAME
      LOGICAL      IEXIST    !LOGICAL IF FILE EXISTS
      INTEGER*4    IUNIT     !UNIT NUMBER ASSIGNED FROM UNTNOFD
      CHARACTER*7  FSTATUS   !FILE STATUS CALCULATED FROM LOGICAL NEW
      CHARACTER*1  FUNCTION  !FUNCTION ='O' FOR OPEN, 'C' FOR CLOSE
                             !NECESSARY FOR GETINDEX FUNCTION
      integer*4 ii
      integer*4 ios
!.....INITIALIZE
      IUNIT=0
      FUNCTION='O'
      FMGR_OPEN=0
      IRCODE = 0


!.....GET INDEX NUMBER FOR UNIQUE_NAME REFERENCE FROM COMMAN ARRAYS
!.....GETINDEX() WILL ADD RECORD IF REQUEST IS FOR A NEW PDS
      INDEX=GETINDEX(UNIQUE_NAME,DD_NAME,PDS_NAME,PDS_FLAG,FUNCTION)
      IF(INDEX .EQ. 0) THEN       !NAME NOT FOUND IN ARRAY LIST
        FMGR_OPEN=-1
        WRITE(MSG_STRING,'(4A,I4)') 'NAME:',UNIQUE_NAME, &
                            ' NOT FOUND IN ARRAY LIST.', &
                            ' RETURN CODE=',FMGR_OPEN
        CALL MSGDISP(ERR_FLAG,MSG_STRING)
        RETURN
      ENDIF

!.....NAME WAS FOUND IN ARRAY LIST

      ISOPEN=.FALSE.

      IF(F_IOSTATUS(INDEX) .EQ. 'OPENED') ISOPEN=.TRUE.

!.....ASSIGN UNIT NUMBER FOR FILE TO OPEN
      IF(.NOT. ISOPEN) THEN !FILE IS NOT PREVIOUSLY OPENED
        ii=-1
        CALL UNTNOFD(IRCODE,MIN_UNIT,ii,IUNIT)
        IF(IRCODE .EQ. 0) THEN    !UNIT NUMBER ASSIGNED SUCCESSFULLY
          FMGR_OPEN=IUNIT
          F_UNIT(INDEX)=FMGR_OPEN
        ELSE                  !UNIT NUMBER ASSIGN FAILED
          FMGR_OPEN=-2        !SET ERROR TO COULD NOT ASSIGN UNIT NUMBER
          WRITE(MSG_STRING,'(3A,I4)') 'COULD NOT ASSIGN UNIT NUMBER TO:' &
                              ,UNIQUE_NAME, &
                              'RETURN CODE=',FMGR_OPEN
          CALL MSGDISP(ERR_FLAG,MSG_STRING)
          RETURN
        ENDIF
      ELSE  !FILE IS ALREADY OPENED
        IUNIT=F_UNIT(INDEX)
        FMGR_OPEN=IUNIT
      ENDIF

!.....SET FILE STATUS TO 'NEW', 'OLD', OR 'UNKNOWN'
      IF(NEW) THEN
        FSTATUS='NEW'
      ELSE
        FSTATUS='OLD'
      ENDIF
!.....CHECK IF FILE ALREADY EXISTS
      INQUIRE(FILE=F_NAME(INDEX),EXIST=IEXIST)
      IF(IEXIST .AND. NEW) THEN
        FSTATUS='UNKNOWN'
      ENDIF

!.....CLOSE FILE TO RE-OPEN LATER
      IF((.NOT. NEW) .AND. ISOPEN) THEN !CLOSE INPUT FILE TO
        CLOSE(UNIT=IUNIT,STATUS='KEEP')        !RE-OPEN LATER
        FSTATUS='OLD'
        ISOPEN=.FALSE.
      ENDIF

!.....OPEN FILE APPROPRIATELY
!.....SET READWRITE ATTRIBUTES BASED ON F_STATE,FSTATUS,F_ACCESS
!      IF((FSTATUS .EQ. 'OLD') .AND.
!     +  (F_STATE(INDEX) .EQ. 'PERMANENT')) THEN
!        F_TYPE_IO(INDEX) ='READ'
!      ELSE
!        F_TYPE_IO(INDEX)='READWRITE'
!      ENDIF
      ios=0
!.....OPEN ONLY IF NOT ALREADY OPEN
      IF(.NOT. ISOPEN) THEN             ! THEN OPEN FILE
        IF(.NOT.NEW .AND. .NOT.IEXIST) GOTO 100  ! existing file not found--stop
        IF(F_ACCESS(INDEX) .EQ. 'SEQUENTIAL') THEN
            OPEN(UNIT   =IUNIT, &
                 FILE   =F_NAME(INDEX), &
                 ACCESS =F_ACCESS(INDEX), &
                 FORM   =F_FORM(INDEX), &
                 ACTION =F_IOTYPE(INDEX),    & ! Read, Write, or ReadWrite
                 CONVERT='BIG_ENDIAN',       & ! aix/pc compatibility for unformatted files
                 STATUS =FSTATUS, &
                 BUFFERED='YES', BUFFERCOUNT=5, &
                 ERR    =100, &
                 IOSTAT =IOS)
        ELSE                ! Else a Direct Access File--need RECL= specifier
            OPEN(UNIT   =IUNIT, &
                 FILE   =F_NAME(INDEX), &
                 ACCESS =F_ACCESS(INDEX), &
                 FORM   =F_FORM(INDEX), &
                 ACTION =F_IOTYPE(INDEX),    & ! Read, Write, or ReadWrite
                 CONVERT='BIG_ENDIAN',       & ! aix/pc compatibility for unformatted files
                 STATUS =FSTATUS, &
                 BUFFERED='YES', BUFFERCOUNT=5, &
                 RECL   =F_LRECL(INDEX), &
                 SHARE  ='DENYNONE', &
                 ERR    =100, &
                 IOSTAT =IOS)

        ENDIF
        WRITE(MSG_STRING,'(3A)') 'FILE OPENED: ',F_ID(INDEX),F_NAME(INDEX)
      ELSE
        WRITE(MSG_STRING,'(3A)') 'FILE LEFT OPEN: ',F_ID(INDEX),F_NAME(INDEX)
      ENDIF
!.....GENERATE MSG THAT FILE IS OPENED
      CALL MSGDISP(MSG_FLAG,MSG_STRING)
      F_IOSTATUS(INDEX)='OPENED'       !SET VALUE IN ARRAY TO 'OPENED'
      GOTO 200

!.....RETURN CODE SETTING FOR OPEN ERRORS
 100  CONTINUE                                        !MF OPEN ERROR
      FMGR_OPEN=-12
      UNIT_AVAIL(IUNIT)=.TRUE.
      WRITE(MSG_STRING,'(3A,I4,a,i4)') 'ERROR OPENING FILE:', &
                          UNIQUE_NAME,'IOSTAT=',ios, &
                          ', RETURN CODE=',FMGR_OPEN
      IF(F_ID(INDEX).NE.'LASTYR'.and. &
       F_ID(INDEX).NE.'JCLDAT') THEN
        CALL MSGDISP(ERR_FLAG,MSG_STRING)
        CALL DUMPARRAYS(INDEX)
        STOP 12 ! STOP in file manager -- cannot OPEN file'
      ELSE
        CALL MSGDISP(.FALSE.,MSG_STRING)
      ENDIF

 200  CONTINUE
      RETURN
      END
      INTEGER FUNCTION FMGR_CLOSE(UNIQUE_NAME,DD_NAME,PDS_NAME,PDS_FLAG)
! *******************************************************************
! *                                                                 *
! *                    FUNCTION FMGR_CLOSE()                        *
! *                                                                 *
! *******************************************************************
! *                                                                 *
! *FUNCTION CLOSES FILES BASED ON DDNAME PASSED                     *
! *RETURNS 0 = OK                                                   *
! *       -1 = COULD NOT FIND DDNAME IN LIST                        *
! *      -11 = COULD NOT CLOSE THE FILE                             *
! *                                                                 *
! *******************************************************************
      IMPLICIT NONE
      include'fmgr'

      CHARACTER*18 UNIQUE_NAME     !(PASSED)
      CHARACTER*18 PDS_NAME
      CHARACTER*18 DD_NAME
      LOGICAL     PDS_FLAG

      INTEGER*4   IUNIT
      INTEGER*4   INDEX
      INTEGER     GETINDEX,IBLNK,I
      CHARACTER*1 FUNCTION

      FMGR_CLOSE=0
      FUNCTION='C'

!.....GET INDEX NUMBER FOR UNIQUE_NAME REFERENCE FROM COMMAN ARRAYS
      INDEX=GETINDEX(UNIQUE_NAME,DD_NAME,PDS_NAME,PDS_FLAG,FUNCTION)

      IF(INDEX.GT.0) THEN                   !FOUND DDNAME IN ARRAY LIST
! MAKE SURE THIS FILE IS OPENED, OTHERWISE, CODE WILL
! CLOBBER SOME OTHER FILE NOW ASSIGNED THAT UNIT NUMBER
        IF( F_IOSTATUS(INDEX).EQ.'OPENED') THEN
          CLOSE(UNIT=F_UNIT(INDEX),ERR=100)
          F_IOSTATUS(INDEX)='CLOSED'
          FMGR_CLOSE=0
          UNIT_AVAIL(F_UNIT(INDEX))=.TRUE.
        ENDIF
      ELSE              !COULD NOT FIND DDNAME IN ARRAY
        FMGR_CLOSE=-1   !SET ERROR NUMBER FOR UNABLE TO LOCATE NAME

        WRITE(MSG_STRING,'(4A,I4)') 'FILE:',UNIQUE_NAME, &
                            ' NOT FOUND IN ARRAY LIST.', &
                            ' RETURN CODE=',FMGR_CLOSE
        CALL MSGDISP(ERR_FLAG,MSG_STRING)
      ENDIF
      GOTO 101

!.....ERROR IN CLOSING FILE IN CLOSE() FUNCTION
  100 CONTINUE          !ERROR IN CLOSE() FUNCTION
      FMGR_CLOSE=-11   !SET RETURN VALUE TO ERROR IN CLOSE() FUNCTION
      GOTO 101

  101 CONTINUE

      IF(FMGR_CLOSE.EQ.0)  THEN
        WRITE(MSG_STRING,'(3A)') 'FILE CLOSED: ',F_ID(INDEX),F_NAME(INDEX)
        CALL MSGDISP(MSG_FLAG,MSG_STRING)
      ELSE
        WRITE(MSG_STRING,'(3A,I4)') 'UNABLE TO CLOSE ',UNIQUE_NAME,' RETURN CODE=',FMGR_CLOSE
        CALL MSGDISP(ERR_FLAG,MSG_STRING)
        IF(INDEX.GT.0)CALL DUMPARRAYS(INDEX)
      ENDIF

      RETURN
      END
      INTEGER FUNCTION GETINDEX(UNIQUE_NAME,DD_NAME,PDS_NAME,PDS_FLAG,FUNCTION)
! *******************************************************************
! *                                                                 *
! *                     FUNCTION GETINDEX()                         *
! *                                                                 *
! *******************************************************************
! *                                                                 *
! * FUNCTION RETURNS THE INDEX OF THE ARRAY ELEMENT CONTAINING      *
! * THE UNIQUE_NAME.  IF FILE IS A PDS, FUNCTION WILL CALCULATE     *
! * THE INDEX ACCORDINGLY AND RESET F_PDS_NAME, F_NAME, AND         *
! * F_ID.                                                           *
! *                                                                 *
! * RETURNS  INDEX NUMBER = OK                                      *
! *                     0 = NOT FOUND IN LIST                       *
! *                                                                 *
! *******************************************************************
      IMPLICIT NONE
      include'fmgr'

      CHARACTER*18 UNIQUE_NAME
      CHARACTER*18 DD_NAME
      CHARACTER*18 PDS_NAME
      LOGICAL      PDS_FLAG
      CHARACTER*1  FUNCTION
      INTEGER      FINDCHAR

      INTEGER*4   I        !INDEX COUNTER
      INTEGER*4   J        !INDEX FOR ADDING NEW PDS RECORD
      LOGICAL     LOOP     !VALUE TO REMAIN IN LOOP OR EXIT
      LOGICAL     LOOP2    !VALUE TO REMAIN IN LOOP FOR PDS OR EXIT
      INTEGER IRET,SYSTEM
      LOOP=.TRUE.
      I=0
      DO WHILE(LOOP)
        I=I+1
        IF(UNIQUE_NAME .EQ. F_ID(I)) THEN
          LOOP=.FALSE.
          GETINDEX=I
        ENDIF
        IF(I.GT.F_ELEMENTS) THEN  !NOT FOUND IN LIST
          LOOP=.FALSE.
          IF(PDS_FLAG .AND. (FUNCTION .EQ. 'O')) THEN !FILE IS PDS
                                                      !SEARCH ON DD_NAME
            LOOP2=.TRUE.
            I=0
            DO WHILE(LOOP2)
              I=I+1
              IF(DD_NAME .EQ. F_ID(I)) THEN
                LOOP2=.FALSE.
                F_ELEMENTS=F_ELEMENTS+1
                J=F_ELEMENTS
                CALL ADDPDS(I,J)
!...............ESTABLISH VARIABLES
                F_PDS_NAME(J)=PDS_NAME
                CALL PDSMEM(F_NAME(J),UNIQUE_NAME,F_PDS_NAME(J),IRET)
                WRITE(6,'(5a)') ' Making ',trim(f_id(j)),trim(f_pds_name(j)), &
                           ' from ',trim(f_name(j))
                IF(IRET.NE.0) THEN
                   WRITE(6,*) 'In file_mgr, couldn''t do this:'
                   WRITE(6,'(5a,i5)') ' Make',trim(f_id(j)),trim(f_pds_name(j)), &
                           ' from ',trim(f_name(j)),iret
                ENDIF
                GETINDEX=J
! Reset file name from original file to created member.
                F_NAME(J)=TRIM(F_ID(J)) // TRIM(F_PDS_NAME(J))
              ENDIF
              IF(I .GT. F_ELEMENTS) THEN  !NOT FOUND IN DDNAME LIST
                LOOP2=.FALSE.
                GETINDEX=0
              ENDIF
            ENDDO
          ELSE IF(PDS_FLAG .AND. (FUNCTION .EQ. 'C')) THEN
            LOOP2=.TRUE.
            I=0
            DO WHILE(LOOP2)
              I=I+1
              IF(UNIQUE_NAME .EQ. F_NAME(I)) THEN
                LOOP2=.FALSE.
                GETINDEX=I
              ENDIF
              IF(I .GT. F_ELEMENTS) THEN  !NOT FOUND IN DDNAME LIST
                LOOP2=.FALSE.
                GETINDEX=0
              ENDIF
            ENDDO
          ELSE                    !FILE IS NOT A PDS
            GETINDEX=0
            LOOP=.FALSE.
          ENDIF
        ENDIF
      ENDDO

      RETURN
      END
      INTEGER FUNCTION FINDCHAR(CHRSTR,STRING,IOCCUR,ILENGTH)
! *******************************************************************
! *                                                                 *
! *                     FUNCTION FINDCHAR()                         *
! *                                                                 *
! *******************************************************************
! *                                                                 *
! * FUNCTION FINDS POSITION OF NTH OCCURRENCE OF A SPECIFIC         *
! * CHARACTER IN STRING                                             *
! *                                                                 *
! *******************************************************************
      IMPLICIT NONE
      INTEGER*4 IOCCUR           !Nth OCCURRENCE (PASSED)
      INTEGER*4 ILENGTH          !LENGTH OF STRING (PASSED)
      CHARACTER*(*) STRING       !CHARACTER STRING (PASSED)
      CHARACTER*1 CHRSTR         !CHARACTER TO SEARCH FOR (PASSED)
      INTEGER*4 IFOUND           !COUNTER OF OCCURRENCES FOUND
      INTEGER*4 I                !COUNTER

      IFOUND=0
      FINDCHAR=0
!.....LOCATE Nth (IOCCUR) OCCURRENCE OF CHAR IN ARRAY A
      DO I=1,ILENGTH
        IF(STRING(I:I).EQ.CHRSTR) IFOUND=IFOUND+1
        IF(IFOUND.EQ.IOCCUR) THEN         !FOUND OCCURRENCE
          FINDCHAR=I
          RETURN
        ENDIF
      END DO
      RETURN
      END
      SUBROUTINE ADDPDS(I,J)
! *******************************************************************
! *                                                                 *
! *                     SUBROUTINE ADDPDS()                         *
! *                                                                 *
! *******************************************************************
! *                                                                 *
! * SUBROUTINE COPIES ALL ELEMENTS FOR (I) TO ELEMENTS OF (J)       *
! * AND ESTABLISHES VALUES FOR:                                     *
! *     F_FILE_NAME(J)                                              *
! *     F_PDS_NAME(J)                                               *
! *                                                                 *
! *******************************************************************
      IMPLICIT NONE
      include'fmgr'

      INTEGER*4 I
      INTEGER*4 J

!.....SET ALL (J) ELEMENTS EQUAL TO (I) ELEMENTS
!  From filemgr.shell
      F_ID(J)         =F_ID(I)
      F_NAME(J)       =F_NAME(I)
      F_IOTYPE(J)     =F_IOTYPE(I)
      F_ACCESS(J)     =F_ACCESS(I)
      F_FORM(J)       =F_FORM(I)
      F_LRECL(J)      =F_LRECL(I)
      F_DESCRIPTION(J)=F_DESCRIPTION(I)
!  From file manager
      F_UNIT(J)       =F_UNIT(I)
      F_IOSTATUS(J)   =F_IOSTATUS(I)
      F_PDS_NAME(J)   =F_PDS_NAME(I)
      F_PDS_FLAG(J)   =F_PDS_FLAG(I)

      RETURN
      END
      SUBROUTINE MSGDISP(IS_ERROR,STRING)
! *******************************************************************
! *                                                                 *
! *                  SUBROUTINE MSGDISP()                           *
! *                                                                 *
! *******************************************************************
! *                                                                 *
! * SUBROUTINE WRITES A MESSAGE TO THE MESSAGE OR ERROR LOG         *
! *                                                                 *
! *******************************************************************
      IMPLICIT NONE
      include'fmgr'
      INTEGER*4     LS       !LENGTH OF STRING
      CHARACTER*(*) STRING   !MESSAGE STRING TO PRINT
      LOGICAL       IS_ERROR !ERROR=TRUE MESSAGE=FALSE
      LS=LEN_TRIM(STRING)
      IF(LS.GT.132) LS=132
      IF (LS .EQ. 0) THEN
         IF(IS_ERROR) THEN
             STRING='Blank error message'
             LS=LEN_TRIM(STRING)
         ELSE
             STRING='Blank string'
             LS=LEN_TRIM(STRING)
         ENDIF
      ENDIF
      IF(IS_ERROR) THEN
        WRITE(ERR_UNIT,*) ' '
        WRITE(ERR_UNIT,*) '###ERROR###  ', STRING(1:LS)
        WRITE(ERR_UNIT,*) ' '
      ENDIF
      WRITE(MSG_UNIT,'(A)') STRING(1:LS)
      RETURN
      END
      SUBROUTINE RESULTS(FUNCTION,UNIQUE_NAME,NEW,STATUS)
! *******************************************************************
! *DEBUG SUBROUTINE                                                 *
! *                   SUBROUTINE RESULTS()                          *
! *                                                                 *
! *******************************************************************
! * ROUTINE TO PRINT RESULTS FROM FILEMGR() CALL                    *
! *******************************************************************
      IMPLICIT NONE
      include'fmgr'

      CHARACTER*1  FUNCTION       !(PASSED)
      CHARACTER*18 UNIQUE_NAME    !(PASSED)
      LOGICAL      NEW            !(PASSED)
      INTEGER*4    STATUS         !(PASSED)

      WRITE(MSG_UNIT,*) ' '
      WRITE(MSG_UNIT,*) '**************************'
      WRITE(MSG_UNIT,*) '*** CALL TO FILE_MGR() ***'
      WRITE(MSG_UNIT,*) '*** RESULTS:           ***'
      WRITE(MSG_UNIT,*) '*** FUNCTION   =',FUNCTION
      WRITE(MSG_UNIT,*) '*** UNIQUE_NAME=',UNIQUE_NAME
      WRITE(MSG_UNIT,*) '*** NEW        =',NEW
      WRITE(MSG_UNIT,*) '*** STATUS     =',STATUS
      WRITE(MSG_UNIT,*) '**************************'
      WRITE(MSG_UNIT,*) ' '

      RETURN
      END
      SUBROUTINE DUMPARRAYS(I)
! *******************************************************************
! * DEBUG ROUTINE                                                   *
! *                   SUBROUTINE DUMPARRAYS(I)                      *
! *                                                                 *
! *******************************************************************
! * ROUTINE DUMPS ARRAY VALUES FOR GIVEN INDEX VALUE                *
! *******************************************************************
      IMPLICIT NONE
      include'fmgr'

      INTEGER*4 I


!.........ECHO WRITE OF ARRAYS
      WRITE(MSG_UNIT,*) ' '
      WRITE(MSG_UNIT,*) 'I               =',I
      WRITE(MSG_UNIT,*) 'F_ID(I)         =',F_ID(I)
      WRITE(MSG_UNIT,*) 'F_NAME(I)       =',F_NAME(I)
      WRITE(MSG_UNIT,*) 'F_PDS_NAME(I)   =',F_PDS_NAME(I)
      WRITE(MSG_UNIT,*) 'F_PDS_FLAG(I)   =',F_PDS_FLAG(I)
      WRITE(MSG_UNIT,*) 'F_IOTYPE(I)     =',F_IOTYPE(I)
      WRITE(MSG_UNIT,*) 'F_ACCESS(I)     =',F_ACCESS(I)
      WRITE(MSG_UNIT,*) 'F_FORM(I)       =',F_FORM(I)
      WRITE(MSG_UNIT,*) 'F_LRECL(I)      =',F_LRECL(I)
      WRITE(MSG_UNIT,*) 'F_DESCRIPTION(I)=',F_DESCRIPTION(I)
      WRITE(MSG_UNIT,*) 'F_UNIT(I)       =',F_UNIT(I)
      WRITE(MSG_UNIT,*) 'F_IOSTATUS(I)   =',F_IOSTATUS(I)
      RETURN
      END
      INTEGER FUNCTION FMGR_TABLE()
! *******************************************************************
! * FUNCTION PRINTS A TABLE OUTPUT OF FILE MANAGER DATA             *
! *                   FUNCTION FMGR_TABLE()                         *
! *                                                                 *
! *******************************************************************
! * ROUTINE PRINTS ALL FILE INFORMATION IN TABLE FORMAT             *
! *******************************************************************
      IMPLICIT NONE
      include'fmgr'

      LOGICAL NEWPAGE
      INTEGER*4 LINES
      INTEGER*4 I

      FMGR_TABLE=0

!.....ECHO WRITE OF ARRAYS
!X    WRITE(MSG_UNIT,*) '****OUTPUTTING FINAL ARRAYS****'
      NEWPAGE=.TRUE.
      LINES=0
      DO I=1,F_ELEMENTS,1
        IF(LINES .GT. 51) NEWPAGE=.TRUE.
        IF(NEWPAGE) THEN
          LINES=0
          LINES=LINES+1
          WRITE(MSG_UNIT,'(2A)') &
                     '1========================================', &
                     '========================================='
          LINES=LINES+1
          WRITE(MSG_UNIT,'(1X,1A)') 'FILE SUMMARY (PART 1)'
          LINES=LINES+3
          WRITE(MSG_UNIT,'(1X,3A)') &
              '===============================================', &
              '===============================================', &
              '==============================================='
          WRITE(MSG_UNIT,'(1X,3A)') &
              'File ID       File Name                 ', &
              '                                        ', &
              '                           Description' 
          WRITE(MSG_UNIT,'(1X,3A)') &
              '===============================================', &
              '===============================================', &
              '==============================================='
          NEWPAGE=.FALSE.
        ENDIF

        WRITE(MSG_UNIT,'(1X,3(A,5X))') F_ID(I),F_NAME(I), &
                                       F_DESCRIPTION(I)
        LINES=LINES+1
      END DO
      WRITE(MSG_UNIT,'(1X,3A)') &
              '===============================================', &
              '===============================================', &
              '==============================================='


      NEWPAGE=.TRUE.
      LINES=0
      DO I=1,F_ELEMENTS,1
        IF(LINES .GT. 50) NEWPAGE=.TRUE.
        IF(NEWPAGE) THEN
          LINES=0
          LINES=LINES+1
          WRITE(MSG_UNIT,'(2A)') &
            '1==================================================', &
            '========================='
          LINES=LINES+1
          WRITE(MSG_UNIT,*) 'FILE SUMMARY (PART 2)'
          LINES=LINES+1
          WRITE(MSG_UNIT,'(1X,2A)') &
            '==================================================', &
            '========================='
          WRITE(MSG_UNIT,'(1X,2A)') &
            'File ID  º IO Type º  Access  º  Format   º RECL º', &
            'PDS Name  º ºUnitºStatusº'
          WRITE(MSG_UNIT,'(1X,2A)') &
            '========= ========= ========== =========== ====== ', &
            '========== = ==== ====== '
          NEWPAGE=.FALSE.
        ENDIF
 100    FORMAT(1X,A8,2X,A9,1X,A10,1X,A11,1X,I6,1X,A10,1X,L1,1X,I4,1X,A6)
        WRITE(MSG_UNIT,100) &
          F_ID(I), &
          F_IOTYPE(I), &
          F_ACCESS(I), &
          F_FORM(I), &
          F_LRECL(I), &
          F_PDS_NAME(I), &
          F_PDS_FLAG(I), &
          F_UNIT(I), &
          F_IOSTATUS(I)
        LINES=LINES+1
      END DO
          WRITE(MSG_UNIT,'(1X,A)') &
            '==========================================================================='

      RETURN
      END
      SUBROUTINE UNTNOFD(RETCODE,LO_UNIT,HI_UNIT,IUNIT)
! *******************************************************************
! *                                                                 *
! *                   SUBTOUTINE UNTNOFD()                          *
! *                                                                 *
! *******************************************************************
! * SUBROUTINE KEEPS TRACK OF UNIT NUMBERS ASSIGNED AND AVAILABLE.  *
! * ROUTINE RETURNS:                                                *
! *   RETCODE:  0=OK                                                *
! *            -1=ERROR                                             *
! *   IUNIT:    POS. NUMBER FOR UNIT NUMBER                         *
! *                                                                 *
! *******************************************************************
      IMPLICIT NONE
      include 'fmgr'

      INTEGER*4 RETCODE      !RETURN CODE O:OK -1:ERROR
      INTEGER*4 HI_UNIT      !MAXIMUM UNIT NUMBER FOR PC
      INTEGER*4 LO_UNIT      !MINIMUM UNIT NUMBER FOR PC
      INTEGER*4 IUNIT        !UNIT NUMBER TO ASSIGN TO FILE

      INTEGER*4 I       !LOOP COUNTER
      LOGICAL   LOOP    !LOOP FLAG
      logical   lopened ! true if a unit listed as available is already open. if so, don't use it

      IF(HI_UNIT .EQ. -1) HI_UNIT=MAX_UNIT

      IUNIT=0
      I=LO_UNIT-1
      LOOP=.TRUE.
      DO WHILE(LOOP)
        I=I+1
        IF(I.GT. MAX_UNIT) THEN
          RETCODE=-1
          LOOP=.FALSE.
        ENDIF

        IF(UNIT_AVAIL(I)) THEN
          inquire(unit=i,opened=lopened)
          if(.not. lopened) then
            LOOP=.FALSE.
            UNIT_AVAIL(I)=.FALSE.
            IUNIT=I                          !RETURN IUNIT AS UNIT NUMBER
          endif
        ENDIF
      END DO

      RETURN
      END
      SUBROUTINE PDSMEM(file_name,file_id,member_name,return_code)
      implicit none
   ! This subroutine breaks a requested 'PDS' member out from its 'PDS'
      character*(*) file_name, member_name
      character*999 line
      character*18 file_id
      integer return_code, in_unit_number, out_unit_number, high
      integer u_return, rec_length, max_rec_len
      logical iexist

      high=-1
      return_code=0
      max_rec_len=999
  ! open file
      call untnofd(u_return,25,high,in_unit_number)
      call untnofd(u_return,25,high,out_unit_number)
      if (u_return .ne. 0) then
        return_code=-1
        return
      endif
      inquire(file=file_name,exist=iexist,recl=rec_length)
      if ( .not. iexist) then
        return_code=-2
        return
      endif
      if ( rec_length .gt. max_rec_len) then
        write (6,'(3a,i4)') 'Record length of file ',trim(file_name), &
                            ' is greater than ',max_rec_len
        return_code=-4
        return
      endif
      open(unit=in_unit_number, file=file_name, err=12, readonly)
      open(unit=out_unit_number, file=file_id, err=13, status='unknown')
      return_code=-8
1     read(in_unit_number,'(a999)',end=14) line
      if (trim(line) .eq. member_name) then
2       read(in_unit_number,'(a999)',end=24) line
        if(line(1:1) .eq. '(') goto 24
        write(out_unit_number,'(a)') trim(line)
        goto 2
      endif
      goto 1
12    return_code=-5
      return
13    return_code=-6
      return
14    return_code=-7
      return
24    return_code=0
      close(unit=in_unit_number)
      close(unit=out_unit_number)
      return
      end
