      PROGRAM CAC_FILELIST
      USE DFLIB
      IMPLICIT NONE
      CHARACTER*255 FILE_RECORD, F_COMMAND(1000), CHANGED_RECORD(1000)
                   ! IN_NAME is file name in default directory
                   ! OUT_NAME is changed name; in input subdirectory
      CHARACTER*93 F_NAME(1000), IN_NAME, OUT_NAME, UNCMPRSS_FILES(1000)
      CHARACTER*93 CHK_COMMAND(1000,2)   ! 1st column = filename, 2nd = filename.gz
      INTEGER ISFILE_THERE(1000,2)  ! 1 in 1st column if file is there, 1 in 2nd if .gz is
      CHARACTER*93 INPUT_FILE_NAME, OUTPUT_FILE_NAME, COPY_COMMANDS, UNCMPRSS_CMDS
      CHARACTER*20  F_IOTYPE(1000), IN_TYPE, BASE_NAME
      CHARACTER*35 NEW_FILE_PREFIX
      CHARACTER*93 TEMP_FNAME
      INTEGER IFILE, NUMBER_OF_ARGS, I1, I2, NADJ, NARG, PRINT_HEADER
      INTEGER IUNIT, OUNIT, COPY_UNIT, UNCMPRSS_UNIT     ! Unit numbers
      INTEGER FINDCHAR, FILE_NAME_LOCATION(2),dotloc
      EXTERNAL FINDCHAR
      LOGICAL FILE_EXIST, FILEgz_EXIST, IS_PDS, UNCOMPRESS_ONLY

      ISFILE_THERE=1   ! initialize entire array to 1 for 'there', below will reset if not
      UNCOMPRESS_ONLY=.FALSE.
      IUNIT=10
      OUNIT=20
      COPY_UNIT=30
      UNCMPRSS_UNIT=40
! Output file names
      OUTPUT_FILE_NAME='filelist.changed'    !  FILELIST with changed input file references
      COPY_COMMANDS='cpcmds.sh'              !  commands to copy input files
      UNCMPRSS_CMDS='uncmprss.sh'            !  commands to uncompress input files
!  Two possible arguments (NUMBER_OF_ARGS counts command as first argument):
!           1 = FILELIST
!           2 = either 'u' for uncompress only, or input file copy destination
      NUMBER_OF_ARGS=NARGS()
      IF (NUMBER_OF_ARGS .LT. 2) THEN
        WRITE (6,*) ' No FILELIST file name -- exiting'
        GO TO 101
      ELSE
        CALL GETARG(1,INPUT_FILE_NAME)
        INQUIRE (EXIST=FILE_EXIST,FILE=INPUT_FILE_NAME)
        IF (FILE_EXIST) THEN
          IF (NUMBER_OF_ARGS .LT. 3) THEN
            WRITE (6,*) ' No file prefix specified -- using ./input'
            NEW_FILE_PREFIX='./input'
          ELSE
            CALL GETARG(2,NEW_FILE_PREFIX)
            IF (TRIM(NEW_FILE_PREFIX) .EQ. 'u' .OR. &
                TRIM(NEW_FILE_PREFIX) .EQ. 'U') THEN
              UNCOMPRESS_ONLY=.TRUE.
              WRITE(6,*) ' Checking input files for existence and/or compression'
            ENDIF
          ENDIF
          OPEN (UNIT=IUNIT, FILE=INPUT_FILE_NAME, STATUS='OLD')
          IF (.NOT. UNCOMPRESS_ONLY) THEN
            OPEN (UNIT=OUNIT, FILE=OUTPUT_FILE_NAME, STATUS='UNKNOWN')
            OPEN (UNIT=COPY_UNIT, FILE=COPY_COMMANDS, STATUS='UNKNOWN')
            WRITE(COPY_UNIT,'(a)') 'echo "Now copying input files.  There may be a lengthy small pause."'
          ENDIF
          OPEN (UNIT=UNCMPRSS_UNIT, FILE=UNCMPRSS_CMDS, STATUS='UNKNOWN')
        ELSE
          WRITE (6,*) ' File name boo-boo.  I could not find:  ', INPUT_FILE_NAME
          GO TO 101
        ENDIF
      ENDIF
      IFILE=0
  100 READ(IUNIT,'(A)',END=101,ERR=201) FILE_RECORD
        IS_PDS=.FALSE.
        IF(FILE_RECORD(1:1).NE.' ') THEN
          IF (.NOT. UNCOMPRESS_ONLY) WRITE(OUNIT,'(A)') FILE_RECORD
          GOTO 100                         ! SKIP COMMENT RECORD
        ELSE
!........FILE NAMES RECORD
!  We tried to adjust for more than one space in a row.
          NADJ=0
!  Only need the first three:  file ID, file name, and 'READ'
          DO 151 NARG=1,3
!  Parse the file information using the space (" ") character.
            I1=FINDCHAR(' ',FILE_RECORD,NARG+NADJ,255)
            IF (I1 .EQ. 0) THEN
               WRITE(6,*) ' Not enough delimiting characters (spaces) ', &
                 'found on FILELIST record:  ', FILE_RECORD
               GOTO 100
            ENDIF
111         I2=FINDCHAR(' ',FILE_RECORD,NARG+NADJ+1,255)
            IF((I2-I1).EQ.1) THEN
              I1 = I2
              NADJ = NADJ + 1
              GOTO 111
            ENDIF
            IF (NARG.EQ.1) THEN
              BASE_NAME=FILE_RECORD(I1+1:I2-1)
            ELSEIF (NARG.EQ.2) THEN
              IN_NAME=FILE_RECORD(I1+1:I2-1)
              FILE_NAME_LOCATION(1)=I1
              FILE_NAME_LOCATION(2)=I2
              dotloc=index(file_record(:i2),'.',.true.)
              if(dotloc.eq.0) dotloc=file_name_location(2)
!  establish if PDS - trailing () in name necessitates different handling
              I1=FINDCHAR('(',IN_NAME,1,93)
              IF (I1 .NE. 0) THEN
                IN_NAME=IN_NAME(1:I1-1)   ! trim off ()
                IS_PDS=.TRUE.
              ENDIF
            ELSEIF (NARG.EQ.3) THEN
!  only need to process input (READ) files, excluding MOREOPT and jcl.dat
              IN_TYPE=FILE_RECORD(I1+1:I2-1)
              I1=FINDCHAR(' ',IN_TYPE,1,20)
              IF (I1 .NE. 0) THEN
                IF (IN_TYPE(1:I1) .EQ. 'READ' .AND. &
                     BASE_NAME(1:7) .NE. 'MOREOPT' .AND. &
                     BASE_NAME(1:6) .NE. 'JCLDAT') THEN
                  IFILE=IFILE+1
                  F_NAME(IFILE)=IN_NAME
                  F_IOTYPE(IFILE)=IN_TYPE
     !  Process input file name.  Need to copy file and change name in FILELIST
                  I1=FINDCHAR(' ',BASE_NAME,1,20)
                  CALL LOWER(BASE_NAME)
                  IF (I1 .EQ. 0) I1 = 20
                  IF (.NOT. UNCOMPRESS_ONLY) THEN
                    IF (IS_PDS) THEN
                      TEMP_FNAME = TRIM(NEW_FILE_PREFIX) // '/' // BASE_NAME(1:I1-1) // &
                        FILE_RECORD(FILE_NAME_LOCATION(2)-6:FILE_NAME_LOCATION(2)-3)
                      F_COMMAND(IFILE)='if [ ' // TRIM(IN_NAME) // ' -nt ' &
                        //  TRIM(TEMP_FNAME) // ' ]; then cp ' &
                        //  TRIM(IN_NAME) // ' ' // TRIM(TEMP_FNAME) // '; fi'
                      CHANGED_RECORD(IFILE)=FILE_RECORD(1:FILE_NAME_LOCATION(1)) // &
                        TRIM(NEW_FILE_PREFIX) // '/' // &
                        BASE_NAME(1:I1-1) // &
                        FILE_RECORD(FILE_NAME_LOCATION(2)-6:)
                    ELSE
                      TEMP_FNAME = TRIM(NEW_FILE_PREFIX) // '/' // BASE_NAME(1:I1-1) // &
                        FILE_RECORD(dotloc:FILE_NAME_LOCATION(2)-1)
                      F_COMMAND(IFILE)='if [ ' // TRIM(IN_NAME) // ' -nt ' &
                        // TRIM(TEMP_FNAME) // ' ]; then cp ' &
                        //  TRIM(IN_NAME) // ' ' // TRIM(TEMP_FNAME) // '; fi'
                      CHANGED_RECORD(IFILE)=FILE_RECORD(1:FILE_NAME_LOCATION(1)) // &
                        TRIM(NEW_FILE_PREFIX) // '/' // &
                        BASE_NAME(1:I1-1) // &
                        FILE_RECORD(dotloc:)
                    ENDIF
!  F_COMMAND is the copy command
                    WRITE (COPY_UNIT,'(A)') TRIM(F_COMMAND(IFILE))
!  CHANGED_RECORD is the changed FILELIST record
                    WRITE (OUNIT,'(A)') TRIM(CHANGED_RECORD(IFILE))
                  ENDIF
!  Begin checking for compressed input files
!  CHK_COMMAND(?,1) is file name  (?,2) is file name with '.gz' appended
                  CHK_COMMAND(IFILE,1)= IN_NAME
                  CHK_COMMAND(IFILE,2)= TRIM(IN_NAME) // '.gz'
                  INQUIRE(EXIST=FILE_EXIST,FILE=CHK_COMMAND(IFILE,1))
                  IF (.NOT. FILE_EXIST) THEN
                    ISFILE_THERE(IFILE,1)=0
                    INQUIRE(EXIST=FILEgz_EXIST,FILE=CHK_COMMAND(IFILE,2))
                    IF (.NOT. FILEgz_EXIST) THEN
                      CHK_COMMAND(IFILE,2)= TRIM(IN_NAME) // '.Z'
                      INQUIRE(EXIST=FILEgz_EXIST,FILE=CHK_COMMAND(IFILE,2))
                      IF (.NOT. FILEgz_EXIST) ISFILE_THERE(IFILE,2)=0
                    ENDIF
                  ENDIF
                ELSE
                !  Write record for WRITE files
                  IF (.NOT. UNCOMPRESS_ONLY) WRITE (OUNIT,'(A)') TRIM(FILE_RECORD)
                ENDIF
              ELSE
                !  Write record for READWRITE files
                IF (.NOT. UNCOMPRESS_ONLY) WRITE (OUNIT,'(A)') TRIM(FILE_RECORD)
              ENDIF
            ENDIF
151       CONTINUE
          IF (IFILE .GE. 1000) THEN
            WRITE (6,*) ' Too many files in FILELIST.', TRIM(INPUT_FILE_NAME)
            GO TO 101
          ENDIF
          GOTO 100
        ENDIF
201   WRITE (6,*) ' Unknown error in FILELIST read'
101   CONTINUE                 !CONTINUE AFTER READING ALL DATA
      PRINT_HEADER=0
      WRITE(UNCMPRSS_UNIT,'(A)') '# files below need to be uncompressed for this run'
      DO I1=1,IFILE
        IF ((ISFILE_THERE(I1,1)+ISFILE_THERE(I1,2)).EQ.0) THEN ! if both=0, file exists not
          IF (PRINT_HEADER .EQ. 0) WRITE (6,'(A)') ' The following files do not exist: '
          PRINT_HEADER=1
          WRITE(6,'(A)') CHK_COMMAND(I1,1)
        ELSE IF (ISFILE_THERE(I1,1).EQ.0.AND.ISFILE_THERE(I1,2).EQ.1) THEN
          WRITE(UNCMPRSS_UNIT,'(2A)') 'uncompress ', CHK_COMMAND(I1,2)
        ENDIF
      ENDDO
      STOP
      END PROGRAM CAC_FILELIST
      INTEGER FUNCTION FINDCHAR(CHRSTR,STRING,IOCCUR,ILENGTH)

! FINDS POSITION OF NTH OCCURRENCE OF A SPECIFIC CHARACTER IN STRING

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
      subroutine lower(a)
      implicit none
      character*(*) a
      integer L,j  ,i
      L=len(a)
      do i=1,L
        j=ichar(a(i:i))
        if(j.ge.65.and.j.le.90)then
          a(i:i)=char(j+32)
        endif
      enddo
      return
      end
