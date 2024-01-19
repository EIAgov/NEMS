C           ::: FLSTRING.FOR  7-31-95 :::
C
C LAST DATES: Earlier dates deleted
C         1-21-94...FSCASE added
C         4-06-95...FSLIST changed to list special strings last
C         5-07-95..." corrected footnote
C
C This file contains the following FLIP subroutines.
C
C For string management and related functions.
C ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C    FSINIT......initializes string management
C                MUST BE CALLED BEFORE OTHER STRING ROUTINES
C    FSTRNG......executes STRING command
C    FSGET.......get string value
C    FSDEF.......define new string
C    FSCLR.......clear strings
C    FSLIST......list strings
C    FSLEN.......give length of string
C    FSSUBS......substitute strings in character list
C    FSCASE......converts string to upper case  (ADDED 4-6-95)
C
      SUBROUTINE FSINIT
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C THIS ENTRY CLEARS ALL STRINGS AND INITIALIZES
C (MUST BE CALLED BEFORE OTHER ENTRIES)
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C ::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
      NUMSTR=0
C
      ASCII( 0) = '0'
      ASCII( 1) = '1'
      ASCII( 2) = '2'
      ASCII( 3) = '3'
      ASCII( 4) = '4'
      ASCII( 5) = '5'
      ASCII( 6) = '6'
      ASCII( 7) = '7'
      ASCII( 8) = '8'
      ASCII( 9) = '9'
      ASCII(10) = 'A'
      ASCII(11) = 'B'
      ASCII(12) = 'C'
      ASCII(13) = 'D'
      ASCII(14) = 'E'
      ASCII(15) = 'F'
      ASCII(16) = 'G'
      ASCII(17) = 'H'
      ASCII(18) = 'I'
      ASCII(19) = 'J'
      ASCII(20) = 'K'
      ASCII(21) = 'L'
      ASCII(22) = 'M'
      ASCII(23) = 'N'
      ASCII(24) = 'O'
      ASCII(25) = 'P'
      ASCII(26) = 'Q'
      ASCII(27) = 'R'
      ASCII(28) = 'S'
      ASCII(29) = 'T'
      ASCII(30) = 'U'
      ASCII(31) = 'V'
      ASCII(32) = 'W'
      ASCII(33) = 'X'
      ASCII(34) = 'Y'
      ASCII(35) = 'Z'
      ASCII(36) = '0'
C
      RETURN
C
C ** FSINIT ENDS HERE
      END
      SUBROUTINE FSTRNG(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This executes STRING command.
C       Syntax:  STRING [ name [= value]
C                        |INCREMENT  name  ]
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*64  STRVAL
      CHARACTER*10  TOKEN
      CHARACTER*8   NAME
      CHARACTER*1   CHAR
C ::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
C SEE IF NAME SPECIFIED
      CALL FTOKEN(CLIST,FIRST,LAST,TOKEN,10,CHAR)
      IF(TOKEN(10:10).NE.' ')THEN
         PRINT *,' STRING NAME TOO LONG...LIMIT=8 CHARS'
         RCODE = -1
         RETURN
      ENDIF
C
      IF(TOKEN.EQ.' ')THEN
C ...NO NAME...LIST STRINGS
         CALL FSLIST
      ELSE IF(TOKEN(9:9).EQ.' ')THEN
         NAME = TOKEN
C GET STRING VALUE
         IF(FIRST.LE.LAST)THEN
C ...TO BE SET
            STRVAL=CLIST(FIRST:LAST)
            CALL FSDEF(NAME,STRVAL,RCODE)
         ELSE
C ...TO BE CLEARED
            CALL FSCLR(NAME,MASK)
         ENDIF
C SPECIAL STRING COMMANDS (> 8 CHARS @)
      ELSE IF(TOKEN.EQ.'INCREMENT')THEN
C INCREMENT STRING'S VALUE (SEE STRING.HLP)
C ...GET STRING'S NAME
         CALL FTOKEN(CLIST,FIRST,LAST,NAME,8,CHAR)
         CALL FSGET(NAME,STRVAL,RCODE)
         IF(RCODE.NE.0)RETURN
C ...INCREMENT VALUE
         CALL FSLEN(STRVAL,64,L)
         CHAR = STRVAL(L:L)
         DO 200 I=0,35
            IF(CHAR.EQ.ASCII(I))THEN
               STRVAL(L:) = ASCII(I+1)
               CALL FSDEF(NAME,STRVAL,RCODE)
               RETURN
            ENDIF
200      CONTINUE
         PRINT *,' INCREMENT FAILED BECAUSE ',CHAR,' IS NOT A DIGIT',
     1           ' OR LETTER'
         RCODE = 1
      ELSE
         PRINT *,' ?',TOKEN,' NOT RECOGNIZED'
         RCODE = -1
      ENDIF
C
      RETURN
C
C ** FSTRNG ENDS HERE
      END
      SUBROUTINE FSGET(NAME,STRVAL,RCODE)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C GET VALUE OF STRING NAME
C
C RCODE = 0...SUCCESSFUL
C       = 1...STRING NOT RECOGNIZED
C       = 2...NAME AMBIGUOUS
C
      INCLUDE 'DCFLIP.'
      INCLUDE 'DCFLCMND.'
CITSO      INCLUDE (DCFLIP)
CITSO      INCLUDE (DCFLCMND)
CI$$INSERT DCFLIP
CI$$INSERT DCFLCMND
C
      CHARACTER*8   NAME
      CHARACTER*(*) STRVAL
C ::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
      DO 100 S=1,NUMSTR
         IF(SNAME(S).EQ.NAME)THEN
            STRVAL = SVALUE(S)
            RCODE = 0
            RETURN
         ENDIF
100   CONTINUE
C NO MATCH
      IF(SWERR) PRINT *,' **',NAME,' not recognized string'
      RCODE=1
      RETURN
C
C ** FSGET ENDS HERE
      END
      SUBROUTINE FSDEF(NAME,STRVAL,RCODE)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C DEFINE NEW STRING: (NAME)=(STRVAL)
C
C RCODE = 0...STRING ADDED
C       = 1...WILL CAUSE AMBIGUITY
C       = 2...OUT OF SPACE
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C
      CHARACTER*8   NAME
      CHARACTER*(*) STRVAL
C ::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
      IF(NUMSTR.GT.0)THEN
C CHECK FOR EXISTENCE/AMBIGUITY
         DO 100 I=1,NUMSTR
            IF(NAME.EQ.SNAME(I))THEN
C   SET/CHANGE VALUE OF STRING (I)
               SVALUE(I) = STRVAL
               RETURN
            ENDIF
100      CONTINUE
      ENDIF
C NEW STRING...CHECK SPACE TO ADD
      IF(NUMSTR.GE.MAXSTR)THEN
         PRINT *,' ** Sorry, no more space for new string.'
         RCODE=2
      ELSE
C OK, ADD STRING
         NUMSTR=NUMSTR+1
         SNAME(NUMSTR) = NAME
         SVALUE(NUMSTR)=STRVAL
      ENDIF
      RETURN
C
C ** FSDEF ENDS HERE
      END
      SUBROUTINE FSCLR(NAME,CHAR)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C CLEAR STRINGS WHOSE NAME MATCHES (USING CHAR AS MASK)
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C
      CHARACTER*1   CHAR
      CHARACTER*8   NAME
      LOGICAL*1     SW
C ::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
      IF(NUMSTR.EQ.0)RETURN
      I=1
300   CONTINUE
      CALL FMATCH(NAME,SNAME(I),CHAR,SW)
      IF(SW)THEN
C CLEAR THIS STRING
         SNAME(I)=SNAME(NUMSTR)
         SVALUE(I)=SVALUE(NUMSTR)
         NUMSTR=NUMSTR-1
      ELSE
         I=I+1
      ENDIF
      IF(I.LE.NUMSTR)GOTO 300
      RETURN
C
C ** FSCLR ENDS HERE
      END
      SUBROUTINE FSLIST
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C LIST STRINGS (TO OUTPUT)
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C LOCAL
      CHARACTER*80 STR80
C ::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
      WRITE(OUTPUT,1)NUMSTR,SDELIM
1     FORMAT(I3,' string(s)...String delimeter is ',A1)
      LINE = 1
      IF( NUMSTR.GT.9 )THEN
C GIVE USER'S STRINGS FIRST
         DO 100 I=10,NUMSTR
            STR80 = SNAME(I)
            STR80(9:) = '='//SVALUE(I)
            CALL FSLEN(STR80,80,LAST)
            CALL FTEXT(STR80,LAST,9,-8,LINE,'CLEAR',*900)
100      CONTINUE
      ENDIF
C NOW GIVE SPECIAL STRINGS THAT HAVE VALUES
      STR80 = ' '
      NULL = 9
      DO 200 I=1,9
         IF( SVALUE(I).EQ.' ' )GOTO 200
         IF( NULL.EQ.9 )THEN
            STR80 = 'Special strings:'
            CALL FSLEN(STR80,80,LAST)
            CALL FTEXT(STR80,LAST,9,-8,LINE,'CLEAR',*900)
         ENDIF
         STR80 = SNAME(I)(:2)//'='//SVALUE(I)
         CALL FSLEN(STR80,80,LAST)
         CALL FTEXT(STR80,LAST,9,-8,LINE,'CLEAR',*900)
         NULL = NULL-1
200   CONTINUE
      IF( NULL.EQ.0 )RETURN
      IF( NULL.EQ.9 )THEN
        STR80 = '(All special strings are null.)'
      ELSE IF( NULL.EQ.1 )THEN
        STR80 = '(Other special string is null.)'
      ELSE
        STR80 = '(Other special strings are null.)'
      ENDIF
      CALL FSLEN(STR80,80,LAST)
      CALL FTEXT(STR80,LAST,1,0,LINE,'CLEAR',*900)
C
900   RETURN
C
C ** FSLIST ENDS HERE
      END
      SUBROUTINE FSLEN(STRING,MAX,LENGTH)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C RETURNS TRUE LENGTH OF STRING (EXCLUDING TRAILING BLANKS)
C
       CHARACTER*(*) STRING
C ::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
      DO 10 LENGTH=MAX,1,-1
10    IF(STRING(LENGTH:LENGTH).NE.' ')RETURN
      LENGTH = 0
      RETURN
C
C ** FSLEN ENDS HERE
      END
      SUBROUTINE FSSUBS(CLIST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This substitutes strings in CLIST(:LAST)
C ...LAST is advanced
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C
      CHARACTER*(*) CLIST
C LOCAL
      CHARACTER*128 CTEMP
      CHARACTER*64  STRVAL
      CHARACTER*10  TOKEN
      CHARACTER*1   DELMTR
C ::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
      IF(NUMSTR.EQ.0)RETURN
      I = 1
C
C          ::: MAIN LOOP OVER CLIST :::
100   CONTINUE
      IF(CLIST(I:I).NE.SDELIM)THEN
         I = I+1
         IF(I.LT.LAST)GOTO 100
         RETURN
      ENDIF
C WE HAVE STRING DELIMETER (SDELIM) AT CLIST(I:I)
C ...TREAT AS LITERAL IF DOUBLED
      IF(CLIST(I+1:I+1).EQ.SDELIM)THEN
         CTEMP = CLIST(:I)//CLIST(I+2:)
         I=I+2
         CLIST = CTEMP
         LAST = LAST-1
         IF(I.LT.LAST)GOTO 100
         RETURN
      ENDIF
C SUBSTITUTE STRING WHOSE NAME BEGINS WITH CLIST(I+1:)
      FIRST = I+1
C PARSE FOR STRING NAME (POSSIBLY WITH TRAILING DELIMETER TOO)
      CALL FTOKEN(CLIST,FIRST,LAST,TOKEN,9,DELMTR)
      IF(TOKEN.EQ.' ')RETURN
C
C SEE IF STRING ENDS WITH STRING DELIMETER (TO MAKE RUN-ON)
      CALL FSLEN(TOKEN,9,LS)
      IF(TOKEN(LS:LS).EQ.SDELIM)THEN
C ..IT DOES, SO REMOVE IT FROM TOKEN (TO HAVE STRING NAME)
           TOKEN(LS:) = ' '
C    AND SUPPRESS ADDING A BLANK
           ADDBL = 0
       ELSE
C ..IT DOESN'T, SO ADD A BLANK AFTER STRING INSERTION
           ADDBL = 1
       ENDIF
C GET STRING VALUE
       CALL FSGET(TOKEN,STRVAL,RCODE)
       IF(RCODE.NE.0)THEN
C STRING NOT FOUND...TREAT AS NULL
          STRVAL = ' '
          LS = 1
          RCODE = 0
       ELSE
C CHECK THAT STRING FITS
          CALL FSLEN(STRVAL,64,LS)
          LS = LS + ADDBL
C  ...ADD LENGTHS
          LC = LS + LAST - FIRST + I
C              :    ::::::::::::   :...CURRENT LENGTH + 1
C              :    REMAINING - 1
C              :
C              :...LENGTH OF INSERTED STRING
          IF(LC.GT.128)THEN
             PRINT *,TOKEN,' ** too long to substitute'
             GOTO 1300
          ENDIF
       ENDIF
C OK, INSERT STRING VALUE INTO CLIST
       IF(FIRST.GT.LAST)THEN
          CTEMP = CLIST(:I-1)//STRVAL(:LS)//DELMTR
          CLIST = CTEMP
          CALL FSLEN(CLIST,128,LAST)
          RETURN
       ENDIF
       IF(DELMTR.EQ.' ')THEN
C DON'T ADD EXTRA BLANK
          CTEMP = CLIST(:I-1)//STRVAL(:LS)//CLIST(FIRST:)
          I = I + LS
       ELSE
C INCLUDE DELIMETER
          CTEMP = CLIST(:I-1)//STRVAL(:LS)//DELMTR//CLIST(FIRST:)
          I = I + LS + 1
       ENDIF
C
C NOW WE HAVE CTEMP = 1ST PART OF CLIST WITH STRING SUBSTITUTED +
C                     DELIMETER (MAYBE) + 2ND PART OF CLIST
C ...I POINTS TO 2ND PART OF CLIST
      CLIST = CTEMP
      CALL FSLEN(CLIST,128,LAST)
      GOTO 100
C
C ERROR RETURN
1300  CONTINUE
      CTEMP = ' '
      PRINT *,' ',CLIST(:LAST)
      CTEMP(I:) = ':...ERROR'
      PRINT *,' ',CTEMP(:I+9)
      IF(RCODE.EQ.0) RCODE = -1
      RETURN
C
C ** FSSUBS ENDS HERE
      END
      SUBROUTINE FSCASE(STRING,FIRST,LAST)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C CONVERT LOWER TO UPPER CASE IN STRING(FIRST:LAST)
C
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C
      CHARACTER*(*) STRING
C LOCAL
      CHARACTER*1   CHAR, LOWCAS(26)
      DATA LOWCAS/'a','b','c','d','e','f','g','h','i','j','k','l','m',
     1            'n','o','p','q','r','s','t','u','v','w','x','y','z'/
C :::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::::
      DO 100 I=FIRST,LAST
         CHAR = STRING(I:I)
         DO 50 K=1,26
            IF( CHAR.EQ.LOWCAS(K) )THEN
               STRING(I:I) = ASCII(K+9)
               GOTO 100
            ENDIF
50       CONTINUE
100   CONTINUE
      RETURN
C
C  ** FSCASE ENDS HERE
      END
