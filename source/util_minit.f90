!-*- f90 -*-
!*******************************************************************
!*    Subroutine RESETOSW(OPT_NAME,NEW_VALUE)
!*
!*    Resets the value for a run-time option integer switch from the value read from the "moreopt" file
!*******************************************************************
      SUBROUTINE RESETOSW(OPT_NAME,NEW_VALUE)
!This resets the value for a run-time option integer switch from the value read from the "moreopt" file
      IMPLICIT NONE
      INTEGER I,NEW_VALUE
      INTEGER MAXRTOPTS
      PARAMETER (MAXRTOPTS=230)
      CHARACTER*8 RTOPTS(MAXRTOPTS),OPT_NAME,RESTOFLINE(MAXRTOPTS)*80
      INTEGER RTOPTSV(MAXRTOPTS),NUMRTOPTS
      COMMON /RTOOPTIONS/ RTOPTS,RTOPTSV,RESTOFLINE,NUMRTOPTS

      DO I=1,NUMRTOPTS
         IF (RTOPTS(I) .EQ. OPT_NAME) THEN
             RTOPTSV(I) = NEW_VALUE
             WRITE(6,'(" Setting option ",A8," to ",I2)') RTOPTS(I),RTOPTSV(I)
             exit
         ENDIF
      ENDDO

      END

!*******************************************************************
!*    Subroutine mnclose
!*
!*    closes all files
!*******************************************************************
      subroutine mnclose
      implicit none
! closes all files
      integer low/1/,high/999/
      integer i
      logical opn
      do i=low,high
         if (i .ne. 6 .and. i .ne. 5) then
           inquire(unit=i,opened=opn)
           if (opn) close(i)
         endif
      enddo
      end
!*******************************************************************

!============================NROTHR and INITIALIZE_API=================================

!*******************************************************************
!*    Subroutine INITIALIZE_API
!*
! Initialize_API reads the table assigning Btu conversion factors to API gravities from:
!   Thermal Properties of Petroleum Products    November 9, 1929
!  input file is apitable.txt   (comma-delimited)
!  this is for function API_TO_BTU to look through and interpolate from
!*******************************************************************
      SUBROUTINE INITIALIZE_API
      IMPLICIT NONE

      include 'apiblk'

      CHARACTER*80 LINE
      LOGICAL STILL_COMMENTS, NOT_END_OF_DATA
      REAL API_HOLD, BTU_PER_GAL_HOLD
      INTEGER I, API_UNIT

      CHARACTER*18 API_FILENAME/'APITABLE          '/
      LOGICAL NEW/.FALSE./
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR

      STILL_COMMENTS = .TRUE.
      NOT_END_OF_DATA = .TRUE.
      API_GRAV=0.0
      BTU_PER_GAL=0.0
      API_UNIT=FILE_MGR('O',API_FILENAME,NEW)
!  read comments at top of file ending with line that has an @ in column 1
      DO WHILE (STILL_COMMENTS)
         READ (API_UNIT,'(A80)') LINE
         IF (LINE(1:1) .EQ. '@') STILL_COMMENTS = .FALSE.
      ENDDO
!  and now read numbers in comma-delimited format (can read list-directed)
      API_COUNT=0
      DO WHILE (NOT_END_OF_DATA)
         READ (API_UNIT,*) API_HOLD, BTU_PER_GAL_HOLD
         IF (API_HOLD .NE. 9999) THEN
             API_COUNT=API_COUNT+1
             API_GRAV(API_COUNT) = API_HOLD
             BTU_PER_GAL(API_COUNT) = BTU_PER_GAL_HOLD
         ELSE
             NOT_END_OF_DATA = .FALSE.
         ENDIF
         IF (API_COUNT .GT. MAX_API_LINES) THEN
             NOT_END_OF_DATA = .FALSE.
!   "eg" command should flag "Encountered" in the following write
             WRITE(6,'(" Encountered possibly early end of read:  MAX_API_LINES exceeded.")')
         ENDIF
      ENDDO
      API_UNIT=FILE_MGR('C',API_FILENAME,NEW)
      DO I=1,API_COUNT
         MM_BTU_PER_BBL = BTU_PER_GAL(I) * 42 / 1000000.
      ENDDO
      RETURN
      END