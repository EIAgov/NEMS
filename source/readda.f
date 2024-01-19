! $Header: m:/default/source/RCS/readda.f,v 1.5.1.1 2009/02/18 17:32:10 dgr Exp $
! PROGRAM TO READ DIRECT ACCESS DEBUG FILE WRITTEN BY INTEGRATING MODULE
! WHEN RUNTTIME OPTION "DBDUMP=1".  THE FILE CONTAINS ALL CONVERGENCE
! VARIABLES BY ITERATION FOR UP TO 10 ITERATIONS.  THIS PROGRAM WRITES
! RANGES OF THE VALUES OUT AND PROVIDES FOR SORTING OPTIONS THROUGH A MACRO.
      PROGRAM READDA
      IMPLICIT NONE
      CHARACTER*12 NAMES(100),FILEN*65,ans*1
      CHARACTER*12 SCEN, DATE
      CHARACTER*2 SECTOR,FUEL
      CHARACTER*1 REGIONS(23),REGLET(23),REGCOD(3)*3/'CN-','GR-','CL-'/
      REAL DATA(10,100),TOLER
      LOGICAL LEXIST
      INTEGER I,IREC,J,YEAR,STARTYR,LASTYR,REGNUM,K,L,LC,LENG
      LOGICAL UNDERFLAG/.FALSE./
      INTEGER UNDEROPT/0/
      CHARACTER*13 UNDER/' ____________'/,BLANK/'             '/
      CHARACTER*13 UNDERLINE(10)
!      CALL ERRSET(232,256,-1,1)
!      STARTYR =1990   !<STARTYR>   ! SET UP BY THE MACRO
!      LASTYR  =2010    !<LASTYR>    ! "
       READ(5,'(A)') SCEN
       READ(5,'(A)') DATE
       READ(5,'(A)') SECTOR
      DO I=1,23
        READ(5,'(A,5X,A)') REGIONS(I),REGLET(I)
      ENDDO
      READ(5,*) STARTYR
      READ(5,*) LASTYR
      READ(5,'(A)') FILEN
      inquire(file=filen,exist=lexist)
      if (.not. lexist) then
        write(*,*) ' MNPQIT FILE NOT FOUND,FILEN=',FILEN
        stop ' '
      endif
      OPEN(30,FILE=FILEN,STATUS='OLD', &
       FORM='UNFORMATTED',ACCESS='DIRECT',RECL=5200,convert='big_endian')
      READ(5,'(BN,a1)') ans
      if(ans.eq.'Y'.or.ans.eq.'y') underopt=1
      if(ans.eq.'1') underopt=1
      TOLER=.05
      IF(UNDEROPT.EQ.1) READ(5,*) TOLER
      IF(TOLER.GT.1.) TOLER=TOLER/100.
      IF(UNDEROPT.EQ.1.AND.TOLER.GT.1.) THEN
       WRITE(6,*) ' TOLERANCE TOO BIG (DECIMAL POINT?), TOLER=',TOLER
       WRITE(6,*) ' RESETTING TO .05'
       TOLER=.05
      ENDIF
      IREC=1
      WRITE(6,*)'            -----------------------------------'
      WRITE(6,*)'             SCENARIO DATEKEY:',SCEN,' ',DATE
      WRITE(6,*)'            -----------------------------------'
 10   CONTINUE
!      write(6,*) ' irec=',irec
      READ(30,REC=IREC,ERR=99) (NAMES(I), &
       (DATA(J,I),J=1,10),I=1,100) 
!    write(*,*) NAMES(1),DATA(1,1)
      DO 100 I=1,100
      IF(NAMES(I)(11:12).EQ.'  ') GOTO 100
      IF(NAMES(I)(11:12).LT.'00'.OR. &
         NAMES(I)(11:12).GT.'99') GOTO 100
        READ(NAMES(I),800,ERR=100)YEAR
 800  FORMAT(10X,I2)
      IF (YEAR .GE. 90 .AND. YEAR .LE. 99) THEN
         YEAR = YEAR + 1900
      ELSE
         YEAR = YEAR + 2000
      ENDIF


! Due to an unfortunate occurence, the coal /utility variables
! don't have EL in them anymore, so we must look for CL
!     WRITE(6,*) 'YEAR(',I,') = ',YEAR
      IF((SECTOR.EQ.'AS') .OR.(SECTOR.EQ.NAMES(I)(4:5)) &
         .OR. ( SECTOR .EQ. 'EL' .AND. NAMES(I)(6:7) &
         .EQ. 'NR'))THEN
!          IF (sector.ne.'EL'.or.(NAMES(I)(2:3).EQ.'RL'.OR.
!     +      NAMES(I)(2:3).EQ.'RH'.OR.
!     +      NAMES(I)(2:3).EQ.'GI'.OR.
!     +      NAMES(I)(2:3).EQ.'GF'.OR.
!     +      NAMES(I)(2:3).EQ.'DS'.OR.
!     +      NAMES(I)(2:3).EQ.'NG')) THEN

            IF(STARTYR .EQ. 26 .OR. &
              (YEAR .GE. STARTYR .AND. &
              YEAR .LE. LASTYR)) THEN
              DO K=1,23
!              write(*,*) REGIONS(K),REGLET(K),NAMES(I)(10:10)
               IF(REGIONS(K).EQ.'1'.AND.REGLET(K).EQ.NAMES(I)(10:10)) &
                THEN
                  L=LENG(NAMES(I)(1:9),9)
                  IF(L.LE.5) THEN
                    LC=1  ! CENSUS REGION ID
                  ELSE
                    LC=3  ! COAL REGION ID
                    IF(NAMES(I)(2:7).EQ.'GIELGR') LC=2  !GAS REGION ID
                    IF(NAMES(I)(2:7).EQ.'GFELGR') LC=2
                    IF(NAMES(I)(2:7).EQ.'GCELGR') LC=2
                    IF(K.EQ.11)THEN
                      IF(NAMES(I)(:9).EQ.'MC_GDP   ') LC=1
                      IF(NAMES(I)(:9).EQ.'IT_WOP   ') LC=1
                      IF(NAMES(I)(:9).EQ.'RFQICRD  ') LC=1
                      IF(NAMES(I)(:9).EQ.'RFPQIPRDT') LC=1
                      IF(NAMES(I)(:9).EQ.'EMETAX   ') LC=1
                      IF(NAMES(I)(:9).EQ.'COSTSO2  ') LC=1
                      IF(NAMES(I)(:5).EQ.' FILL')     LC=1

                    ENDIF
                  ENDIF
                  WRITE(6,'(1X,I4,1X,A3,I2.2,1X,A,10F13.6)') YEAR, &
                   REGCOD(LC),K, &
                   NAMES(I)(:9),(DATA(J,I),J=1,10)
                  IF(UNDEROPT.EQ.1) &
                     CALL CHKCONV(UNDERFLAG,DATA(1,I),UNDERLINE,TOLER)
                  IF(UNDERFLAG) &
                   WRITE(6,'(1H+,I4,1X,A3,I2.2,1X,A,10A13)') YEAR, &
                   REGCOD(LC),K,NAMES(I)(:9),UNDERLINE
                ENDIF
              ENDDO
            ENDIF
!          ENDIF
      ENDIF
100   CONTINUE
 98   IREC=IREC+1
      GOTO 10
 99   CONTINUE
      STOP ' '
      END

      SUBROUTINE CHKCONV(UNDERFLAG,DATA,UNDERLINE,TOLER)
      IMPLICIT NONE
! CHECK VALUES FROM ITERATION TO ITERATION AND FLAG ITEMS IN WHICH
! THE PERCENTAGE CHANGE EXCEEDS A GIVEN TOLERANCE.  THE FLAGGING
! IS DONE USING UNDERLINING, SO IT CAN OLD BE OBSERVED EASILY AFTER
! PRINTING
      INTEGER J
      REAL DATA(10),TOLER,ABSVAL,PCTCHG,ABSCHG,AVE
      LOGICAL UNDERFLAG
      CHARACTER*13 UNDER/' ____________'/,BLANK/'             '/
      CHARACTER*13 UNDERLINE(10)
      UNDERFLAG=.FALSE.
      DO J=1,10
        UNDERLINE(J)=BLANK
      ENDDO
      DO J=2,10
        PCTCHG=0.
        ABSCHG=ABS(DATA(J)-DATA(J-1))
        AVE=(DATA(J)+DATA(J-1))/2.
        IF(AVE.NE.0..and.data(j).ne.0.) PCTCHG=ABSCHG/AVE
        IF(PCTCHG.GT.TOLER) THEN
          UNDERFLAG=.TRUE.
          UNDERLINE(J)=UNDER
          UNDERLINE(J-1)=UNDER
        ENDIF
      ENDDO
      RETURN
      END
      FUNCTION LENG(A,N)
!     FINDS POSITION OF LAST NON-BLANK CHARACTER IN CHARACTER VARIABLE A
      CHARACTER*1 A(N)
      LENG=0
      DO 10 I=N,1,-1
        IF(A(I).NE.' ') THEN
          LENG=I
          GO TO 99
        ENDIF
   10 CONTINUE
   99 CONTINUE
      RETURN
      END
