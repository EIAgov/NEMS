C                ::: FLIPMAIN.FOR  5-12-93 :::
C
C LAST DATE:  7-22-90...STRING command put into FLSTRING.FOR
C             9-20-90...Added ; for multiple commands on 1 line
C
C This file contains MAIN program of FLIP
C
C               Fortran Language Interactive Program
C               ~       ~        ~           ~
C
      PROGRAM MAIN
C    :::::::::::::::::::::
C    : DATA DECLARATIONS :
C    :::::::::::::::::::::
C
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCFLIP.'
      INCLUDE 'DCFLCMND.'
CITSO      INCLUDE (DCFLIP)
CITSO      INCLUDE (DCFLCMND)
CI$$INSERT DCFLIP
CI$$INSERT DCFLCMND
C
C LOCAL
      CHARACTER*128 CLIST,CTEMP,CLIST2
      CHARACTER*1   CHAR,DELMTR
C :::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
C INITIALIZE
      CALL FLINIT
      LINE = 0
      CLIST2 = ' '
C           HERE IS TOP OF USER INTERFACE LOOP
C           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
500   CONTINUE
      CLIST=' '
      RCODE=0
      SWFDBG = SWVALU(0)
      IF( CLIST2.NE.' ' )THEN
C NEW COMMAND FROM PREVIOUS INPUT
         CTEMP = CLIST2
         CLIST2= ' '
         GOTO 510
      ENDIF
      IF(INPUT.EQ.TTYIN)THEN
         WRITE(*,501)SYSNAM
501      FORMAT(1X,A8,'...')
         EXENUM = 0
      ENDIF
C
C GET COMAND FROM INPUT
      CALL FLRDLN(CTEMP,INPUT,RCODE)
      IF(RCODE)9900,510,505
C              :    :   :...Fatal error
C              :    :...OK
C              :...EOF
C
505   CALL FLCMND(CLIST,FIRST,LAST,RCODE)
      GOTO 500
C
510   CLIST(2:) = CTEMP
C NOTE: CLIST(1:1) IS DELIBERATELY BLANK TO EASE STRING INSERTION
C
      FIRST = 2
      CALL FSLEN(CLIST,128,LAST)
C SEE IF THIS HAS 2ND COMMAND
      CALL FLOOKF(CLIST,FIRST,LAST,';',IBREAK)
      IF( IBREAK.GT.0 )THEN
C ...IT DOES
         CLIST2 = CLIST(IBREAK+1:)
         CLIST(IBREAK:) = ' '
         LAST = IBREAK-1
      ENDIF
      CALL FTOKEN(CLIST,FIRST,LAST,CHAR,1,DELMTR)
      IF(CHAR.EQ.'?')THEN
         CALL FQUEST(CLIST,FIRST,LAST,RCODE)
         GOTO 500
      ENDIF
C
      IF(CLIST(2:2).EQ.'$')THEN
C ECHO COMMENT
         CTEMP = ' '//CLIST(3:)
         CALL FSLEN(CTEMP,128,LAST)
         IF(LAST.GT.SCRWTH)LAST = SCRWTH
C                           :...CLIP AT SCREEN WIDTH
         PRINT *,CTEMP(:LAST+1)
         IF(CLIST(3:3).EQ.'$' .AND. OUTPUT.NE.TTYOUT)THEN
C ...AND TO OUTPUT FILE
            CTEMP = CLIST(4:)
            CALL FTEXT(CTEMP,LAST,1,0,LINE,'CLEAR ',*500)
         ENDIF
         GOTO 500
      ENDIF
C
C   .~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.
C   : NOTE: THERE ARE 2 TYPES OF COMMENT LINES...   :
C   : * IN COLUMN 1 SKIPS LINE (SEE FLRDLN)         :
C   : $ IN COLUMN 1 PRINTS LINE TO, THEN SKIPS      :
C   : $$ ALSO ECHOES LINE TO OUTPUT FILE (IF ANY)   :
C   :   ...USEFUL WHEN EXECUTING COMMAND FILE       :
C   : USE _TYPE COMMAND JUST TO TYPE TO OUTPUT FILE :
C   :...............................................:
C
      IF(CLIST(2:2).EQ.',')THEN
C SYS DEBUG (HIDDEN COMMAND)
         CALL FDEBUG
         GOTO 500
      ENDIF
C
C  ::: NO SPECIAL CHAR IN COLUMN 1...BEGIN PARSE :::
C
      CALL FSLEN(CLIST,128,LAST)
C
      IF(SWECHO .AND. LAST.GE.2)THEN
C ECHO INPUT (AFTER STRING SUBSTITUTION)
         PRINT *,CLIST(2:LAST)
         IF(OUTPUT.NE.TTYOUT)WRITE(OUTPUT,661)SYSNAM,
     1     (CLIST(I:I),I=2,LAST)
661      FORMAT(A8,'...',127A1)
      ENDIF
      FIRST=2
C
      CALL FLCMND(CLIST,FIRST,LAST,RCODE)
      IF( RCODE.NE.0 )CLIST2 = ' '
      GOTO 500
C
9900  CONTINUE
C EOF ON INPUT FILE...TREAT AS RETURN
      CLIST = 'RETURN'
      RCODE = 0
      FIRST = 1
      LAST =  6
      CALL FLCMND(CLIST,FIRST,LAST,RCODE)
      GOTO 500
C
      END
