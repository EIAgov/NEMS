C            ::: AQUERY1.FOR  7-31-95 :::
C
C Earlier dates deleted.
C      7-31-93...Added aliases X,Y,D to DISPLAY options (ADSPLY)
C  7-31-95  Moved AQSHOW, AQSHTC, AQSHTJ, and AQSHRC to ASHOW.FOR
C
C This contains the following subroutines for ANALYZE
C
C    ADDRIM.....adds rim elements for row/col
C    ADSPLY.....displays (prints) rim elements for row/col
C    ATALLY.....tallies row or column set members
C
      SUBROUTINE ADDRIM(CLIST,FIRST,LAST,FILOUT,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This adds rim elements with syntax:
C
C    ADDRIM.................use current submatrix and SWDISP setting
C  | ADDRIM ROW|COL.........use current submatrix but reset SWDISP
C  | ADDRIM ROW|COL  cond...change submatrix for row/column
C
      CHARACTER*128 CLIST
C LOCAL
C                       TABS FOR TABLE COLUMNS
      PARAMETER (TABSUM=5,TABMIN=18,TABMAX=31,TABAVG=44)
      CHARACTER*16  RNAME
      CHARACTER*4   ROWCOL
C ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C GET ROW|COLUMN...absence means use current settings
      CALL AGETRC(CLIST,FIRST,LAST,ROWCOL,RCODE)
      IF(RCODE.NE.0)RETURN
C
      IF(ROWCOL.NE.' ')THEN
C
C     ROW|COL SPECIFIED
C
C     PARSE CONDITIONAL...absence means use current submatrix
C
           CALL ACONDN(CLIST,FIRST,LAST,ROWCOL,RNAME,RCNUM,RCODE)
           IF(RCODE.NE.0)RETURN
           IF(RNAME.NE.' ')CALL ASBNEW(ROWCOL,RNAME,RCNUM)
      ELSE
           IF(SWDISP)THEN
              ROWCOL = 'COL '
           ELSE
              ROWCOL = 'ROW '
           ENDIF
      ENDIF
C
C NOW SWDISP IS SET AND SUBMATRIX IS SET
C
      IF(FILOUT.EQ.0)RETURN
C     (FILOUT = 0 means caller just wanted parsing to set submatrix)
C
C WRITE ADDRIM RESULT TO OUTPUT (USING FTEXT)
C
      IF(SWDISP)THEN
           K=2
      ELSE
           K=1
      ENDIF
C
      LINE = 1
      CLIST = 'Submatrix has'
      RNAME = ' '
      CALL FI2C(RNAME,NRCSUB(K),F)
      CLIST(15:) = RNAME(F:9)//ROWCOL
      CALL FSLEN(CLIST,50,LAST)
      IF(NRCSUB(K).NE.1)THEN
         LAST = LAST+1
         CLIST(LAST:)='S'
      ENDIF
      CALL FTEXT(CLIST,LAST,1,0,LINE,'SKIP ',*900)
      IF(NRCSUB(K).EQ.0)RETURN
C
      CLIST = ' '
      CLIST(TABSUM:) = '     SUM'
      CLIST(TABMIN:) = '     MIN'
      CLIST(TABMAX:) = '     MAX'
      CLIST(TABAVG:) = '   AVERAGE'
      LAST = TABAVG+12
      L = LAST
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
C
      DO 50 I=TABSUM,L
50    CLIST(I:I) = '-'
      LAST = L
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
C
      DO 190 I=1,NKEYS
           CLIST = ' '//RIMKEY(K,I)
           IF(VMINSB(K,I).GE.VINF)THEN
C ALL RIM VALUES FOR THIS KEY ARE INFINITE...CHANGE VALUES
              CLIST(TABSUM:) = '...ALL VALUES ARE INFINITE'
           ELSE
              CALL FR2C12(CLIST(TABSUM:),VSUMSB(K,I))
              CALL FR2C12(CLIST(TABMIN:),VMINSB(K,I))
              CALL FR2C12(CLIST(TABMAX:),VMAXSB(K,I))
              CALL FR2C12(CLIST(TABAVG:),VAVGSB(K,I))
           ENDIF
C
           LAST = L
           CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
190   CONTINUE
C
      DO 200 I=TABSUM,L
200   CLIST(I:I) = '-'
      CALL FTEXT(CLIST,L,1,0,LINE,'CLEAR ',*900)
C
900   RETURN
C
C ** ADDRIM ENDS HERE
      END
      SUBROUTINE ADSPLY(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCFLIP.'
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCFLIP)
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCFLIP
CI$$INSERT DCANAL
C
C This writes rim display to OUTPUT file (using FTEXT)
C ...DCFLIP is used for screen width (SCRWTH).
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*128 CTEMP
      CHARACTER*16  RNAME,CNAME
      CHARACTER*1   STAT
      CHARACTER*4   ROWCOL
      LOGICAL*1     SW
C
      PARAMETER (MARGIN=1,INDENT=0)
      PARAMETER (NUMOPS=10)
      INTEGER*2     TAB(NUMOPS)
      LOGICAL*1     PICK(NUMOPS)
      CHARACTER*8   OPTION(NUMOPS)
      REAL          V(NUMOPS)
      DATA OPTION/'STATUS ','LEVEL ','LO_BOUND','UP_BOUND','PRICE ',
     1            'SYNTAX ','NOSYNTAX','X','Y','D'/
C ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C X|Y ADDED AS ALIASES FOR LEVEL, AND D FOR DUAL PRICE
C PICK ALL VALUE OPTIONS AS DEFAULT
      DO 10 L=1,5
10    PICK(L) = .TRUE.
C DEFAULT FOR SYNTAX = SYNTAX SWITCH SETTING
      PICK(6) = SWSYN
      PICK(7) = .FALSE.
C PARSE CLIST TO SEE IF FIELD OPTIONS HAVE BEEN SPECIFIED
      CALL FLOOK(CLIST,FIRST,LAST,'//',L)
      IF( L.GT.0 )THEN
C OPTIONS SPECIFIED
         F = L+2
         CALL FOPSET(CLIST,F,LAST,OPTION,NUMOPS,PICK,RCODE)
         IF( RCODE.NE.0 )RETURN
C SET LEVEL AND PRICE USING ALIASES
         PICK(2) = PICK(2) .OR. PICK(8) .OR. PICK(9)
         PICK(5) = PICK(5) .OR. PICK(10)
C PREPARE FOR CONDITIONAL PARSE
         LAST = L-1
      ENDIF
C RE-SET SYNTAX OPTION, WHERE SWEXPL = T IFF SYNTAX AVAILABLE
      PICK(6) = PICK(6) .AND. SWEXPL
C
C CLIST(FIRST:LAST) NOW HAS CONDITIONAL
C
C SET ROW|COLUMN [conditional]....USING ADDRIM
      CALL ADDRIM(CLIST,FIRST,LAST,0,RCODE)
      IF(RCODE.NE.0)RETURN
C SET STUFF, DEPENDING ON ROW VS COL
      IF(SWDISP)THEN
         NRC    = NRCSUB(2)
         CNAME  = 'Column'
         ROWCOL = 'COL '
         BEGIN  = RCSUB1(2)
         END    = NCOLS
      ELSE
         NRC    = NRCSUB(1)
         CNAME  = 'Row'
         ROWCOL = 'ROW '
         BEGIN  = RCSUB1(1)
         END    = NROWS
      ENDIF
C
C NOW CLIST IS PARSED, SO WE CAN USE IT FOR OUTPUT
      LINE = 1
      IF(.NOT.SWRULE)THEN
C GIVE NUMBER OF ROWS/COLS
         RNAME = ' '
         CALL FI2C(RNAME,NRC,FIRST)
         CLIST = RNAME(FIRST:9)//CNAME
         CALL FSLEN(CLIST,30,LAST)
         IF(NRC.EQ.1)THEN
            CLIST(LAST+2:) = 'is in submatrix.'
         ELSE
            CLIST(LAST+1:) = 's are in submatrix.'
         ENDIF
         CALL FSLEN(CLIST,80,LAST)
         CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'CLEAR ',*9000)
      ENDIF
      IF(NRC.EQ.0)RETURN
C
C USER MAY HAVE SPECIFIED NOSYNTAX AS THE ONLY OPTION, IN WHICH CASE
C WE ASSUME ALL VALUES ARE WANTED
      IF(PICK(7))THEN
C SUPPRESS SYNTAX (USER MAY BE PLAYING GAMES WITH BOTH SYN AND NOSYN)
         PICK(6) = .FALSE.
         DO 50 I=1,5
50       IF(PICK(I))GOTO 75
C YEP, THAT'S WHAT HE/SHE DID...DISPLAY ALL VALUES
         DO 60 I=1,5
60       PICK(I) = .TRUE.
      ENDIF
75    CONTINUE
C
C FORM HEADER
C
      CLIST = ROWCOL
      IF(NAMELN.LT.4)THEN
         TAB0 = 4
      ELSE
         TAB0 = NAMELN+1
      ENDIF
C
      IF(PICK(1))THEN
         TAB(1) = TAB0+2
         TAB0   = TAB0+5
         T = TAB(1)-1
         CLIST(T:) = 'STAT'
      ENDIF
      DO 110 I=2,5
         IF(PICK(I))THEN
            TAB(I) = TAB0
            TAB0   = TAB0+13
            T = TAB(I)+2
            CLIST(T:) = OPTION(I)
         ENDIF
110   CONTINUE
      ENDU = TAB0-1
      IF(TAB0.GT.NAMELN+3)GOTO 1000
C
C NO FIELDS...JUST LIST OF ROWCOLS...USE MANY PER LINE
C NOTE:  WE GET HERE ONLY IF USER SPECIFIED // WITHOUT ANY OPTIONS
C        (RECALL //NOSYNTAX DOES NOT GET HERE)
C
      LAST = 0
      CLIST = ' '
      TAB0 = NAMELN+1
      DO 500 NUMBER=BEGIN,END
         CALL GETMAP(ROWCOL,NUMBER,SW)
         IF(.NOT.SW)GOTO 500
C ROWCOL (NUMBER) IS IN SUBMATRIX
         NRC = NRC - 1
         CALL GETNAM(ROWCOL,NUMBER,CLIST(LAST+1:))
         LAST = LAST+TAB0
         IF(LAST+NAMELN.GT.127)THEN
C WRITE SOME ROWCOLS
            CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'KEEP ',*9000)
            IF(LAST.GT.0)THEN
C ...ADVANCE LAST TO NEXT TAB
               MULT = LAST/TAB0
               LAST = (MULT+1)*TAB0
            ENDIF
         ENDIF
         IF(NRC.EQ.0)GOTO 510
500   CONTINUE
C CLEAR TEXT (IF ANY)
510   IF(LAST.GT.0)
     1   CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'CLEAR ',*9000)
      RETURN
C
C NORMAL DISPLAY...1 LINE PER ROWCOL
C
1000  CONTINUE
      IF( ENDU.GT.SCRWTH )THEN
         PRINT *,' ** SCREEN WIDTH TOO SMALL FOR DISPLAY'
         PRINT *,' ...MUST=',ENDU+2
         PRINT *,' ...USE DISPLAY PARAMETERS TO LIMIT FIELDS'
         RCODE = 1
         RETURN
      ENDIF
C
C AUGMENT FIELD HEADS WITH RIMKEYS
      IF(PICK(2))THEN
C ...LEVEL (X or Y)
         T = TAB(2)+8
         IF(SWDISP)THEN
            CLIST(T:T+2) = '(X)'
         ELSE
            CLIST(T:T+2) = '(Y)'
         ENDIF
      ENDIF
      IF(PICK(5))THEN
C ...PRICE (D or P)
         T = TAB(5)+8
         IF(SWDISP)THEN
            CLIST(T:T+2) = '(D)'
         ELSE
            CLIST(T:T+2) = '(P)'
         ENDIF
      ENDIF
C
C PRINT HEADER
      LAST = ENDU
      CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'CLEAR ',*9000)
C
      DO 1090 I=1,ENDU
1090  CLIST(I:I) = '-'
      LAST = ENDU
      CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'CLEAR ',*9000)
C
C     ::: LOOP OVER ROW|COL :::
C
      DO 2000 NUMBER=BEGIN,END
         CALL GETMAP(ROWCOL,NUMBER,SW)
         IF(.NOT.SW)GOTO 2000
C ROWCOL (NUMBER) IS IN SUBMATRIX
         NRC = NRC - 1
C PREPARE LINE FOR OUTPUT
         CLIST = ' '
         CALL GETNAM(ROWCOL,NUMBER,RNAME)
         CLIST = RNAME
         CALL GETBND(ROWCOL,NUMBER,V(3),V(4))
         CALL GETSOL(ROWCOL,NUMBER,V(2),V(5),STAT,STNUM)
C NOTE:  V(2)=LEVEL, V(3)=LO, V(4)=UP, V(5)=PRICE
C
C PUT RESULTS INTO CLIST FIELDS
         IF(PICK(1))THEN
            T = TAB(1)
            CLIST(T:) = STAT
         ENDIF
         DO 1500 I=2,5
            IF(PICK(I))THEN
               T = TAB(I)
               CALL FR2C12(CLIST(T:),V(I))
            ENDIF
1500     CONTINUE
C
         IF(PICK(6))THEN
C GIVE SYNTAX OF ROWCOL
            LAST = 1
            CALL EXTRC(ROWCOL,RNAME,CTEMP,LAST,RCODE)
            IF(RCODE.EQ.0)THEN
C STRIP AWAY ROWCOL NAME
               F = 1
               CALL FTOKEN(CTEMP,F,LAST,RNAME,8,STAT)
               CALL FTOKEN(CTEMP,F,LAST,RNAME,NAMELN,STAT)
               IF(CTEMP(LAST:LAST).EQ.'.')THEN
C REMOVE PERIOD AT END OF SYNTAX
                  CTEMP(LAST:) = ' '
                  LAST = LAST-1
               ENDIF
               IF(ENDU+LAST-F+2.LT.SCRWTH)THEN
                  CLIST(ENDU+1:) = CTEMP(F:)
               ELSE
C MUST PUT SYNTAX ON SEPARATE LINE
                  LAST = ENDU
                  CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'CLEAR ',
     1                       *9000)
                  CLIST = ' ...'//CTEMP(F:)
               ENDIF
            ELSE
               RCODE = 0
            ENDIF
         ENDIF
C OK, NOW WRITE TO OUTPUT
         CALL FSLEN(CLIST,128,LAST)
         CALL FTEXT(CLIST,LAST,MARGIN,INDENT,LINE,'CLEAR ',*9000)
         IF(NRC.EQ.0)RETURN
2000  CONTINUE
C
9000  RETURN
C
C ** ADSPLY ENDS HERE
      END
      SUBROUTINE ATALLY(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This tallies row/column.
C   Syntax: TALLY query_mask [ ROW|COL [conditional]] [//FILL char]
C
C ...Using FTEXT (FILOUT removed with Version 8.2).
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*16  RNAME,CNAME
      CHARACTER*1   CHAR,FILCHR,FILL,QCHAR
      CHARACTER*4   ROWCOL
      LOGICAL*1     SW,SWRC
C
      LOGICAL*1     QMAP(16)
      PARAMETER     (MAXCL=80)
C                          :...MAX NUMBER OF CLASSES
      CHARACTER*16  NAMECL(MAXCL)
      DATA FILCHR /'~'/, QCHAR/'?'/
C ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C SET DEFAULT FILL CHAR
      FILL = FILCHR
C SEE IF OPTION SET (IF SO, PARSE)
      CALL FLOOK(CLIST,FIRST,LAST,'//',I)
      IF(I.GT.0)THEN
C ...YES, OPTION SPECIFIED...PARSE FILL CHAR
         F = I+2
         CALL FTOKEN(CLIST,F,LAST,RNAME,8,CHAR)
         CALL FMATCH(RNAME,'FILL ',' ',SW)
         IF(.NOT.SW)THEN
            PRINT *,' ** OPTION ',RNAME(:8),' NOT RECOGNIZED'
            PRINT *,' ...FILL IS ONLY OPTION POSSIBLE'
            RCODE = -1
            RETURN
         ENDIF
C FILL SPECIFIED...GET FILL CHAR
         IF(CHAR.NE.' ')THEN
C ...IT IS THE DELIMITER (CHAR)
            FILL = CHAR
         ELSE
            CALL FTOKEN(CLIST,F,LAST,RNAME,1,CHAR)
            IF(RNAME.NE.' ')FILL = RNAME
         ENDIF
C MOVE LAST BACK
         LAST = I-1
      ENDIF
C NOW CLIST(FIRST:LAST) HAS TALLY SPECS
C     FILL = FILL CHAR (BLANK, IF NO FILL)
C
C GET QUERY MASK
      CALL FTOKEN(CLIST,FIRST,LAST,RNAME,NAMELN,CHAR)
      SW = .FALSE.
C
      DO 100 I=1,NAMELN
           IF(RNAME(I:I).EQ.QCHAR)THEN
             QMAP(I) = .TRUE.
             RNAME(I:I) = '*'
             SW = .TRUE.
           ELSE
             QMAP(I) = .FALSE.
           ENDIF
100   CONTINUE
C
      IF(.NOT.SW)THEN
           PRINT *,' ** MISSING QUERY MASK (USE ',QCHAR,')'
           RCODE = -1
           RETURN
      ENDIF
C
C SET SUBMATRIX (AND ROW|COL SPEC)
      CALL ADDRIM(CLIST,FIRST,LAST,0,RCODE)
      IF(RCODE.NE.0)RETURN
C
      IF(SWDISP)THEN
           ROWCOL = 'COL '
           LAST = NCOLS
           K = 2
      ELSE
           ROWCOL = 'ROW '
           LAST = NROWS
           K = 1
      ENDIF
C
C GIVE NUMBER TO BE TALLIED
      NRC = NRCSUB(K)
      CALL FI2C(CNAME,NRC,FIRST)
      CLIST = CNAME(FIRST:8)//' '//ROWCOL(:3)//'S TALLIED'
      CALL FSLEN(CLIST,30,L)
      LINE = 0
      CALL FTEXT(CLIST,L,1,0,LINE,'CLEAR ',*900)
      IF(NRC.EQ.0)RETURN
C
      NCLASS = 0
      SWRC = .FALSE.
      NOMTCH = 0
C
C    ::: LOOP OVER ELEMENTS TO TALLY :::
C
      DO 500 NUMBER=RCSUB1(K),LAST
         CALL GETMAP(ROWCOL,NUMBER,SW)
         IF(.NOT.SW)GOTO 500
         NRC = NRC - 1
C
         CALL GETNAM(ROWCOL,NUMBER,CNAME)
         IF(FILL.NE.' ')THEN
C BEFORE MATCHING, FILL TRAILING BLANKS
            CALL FSLEN(CNAME,NAMELN,L)
            IF(L.LT.NAMELN)THEN
               DO 350 C=L+1,NAMELN
350            CNAME(C:C) = FILL
            ENDIF
         ENDIF
         CALL FMATCH(RNAME,CNAME,'*',SW)
         IF(.NOT.SW)THEN
            NOMTCH = NOMTCH + 1
            GOTO 490
         ENDIF
C
C       LOOKUP CLASS
C
          IF(NCLASS.GT.0)THEN
             DO 400 C=1,NCLASS
                CALL FMATCH(NAMECL(C),CNAME,'*',SW)
                IF(SW)THEN
                   ROWLST(C) = ROWLST(C) + 1
                   GOTO 490
                 ENDIF
400          CONTINUE
C   ...NEW CLASS
             IF(NCLASS.EQ.MAXCL)THEN
                SWRC = .TRUE.
                GOTO 490
             ENDIF
          ENDIF
C   ...ADD CLASS
          NCLASS = NCLASS + 1
          DO 450 I=1,NAMELN
450       IF(.NOT.QMAP(I))CNAME(I:I)=RNAME(I:I)
          NAMECL(NCLASS) = CNAME
          ROWLST(NCLASS) = 1
C READY FOR NEXT ROWCOL
490      IF(NRC.EQ.0)GOTO 510
500   CONTINUE
C
C    ::: END LOOP :::
C
510   CONTINUE
C    :::TALLY COMPLETE:::
C
      CALL FI2C(CNAME,NCLASS,FIRST)
      CLIST = CNAME(FIRST:8)//' '//ROWCOL//'CLASSES IN SUBMATRIX'
      CALL FSLEN(CLIST,60,LAST)
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
      IF(NCLASS.EQ.0)RETURN
C
      DO 650 C=1,NCLASS,2
         NUMBER = ROWLST(C)
         CNAME = ' '
         CALL FI2C(CNAME,NUMBER,FIRST)
         CLIST = CNAME(:8)//' IN'
         CNAME  = NAMECL(C)
C REMOVE FILL CHAR FROM CLASS NAME
         CALL FLOOKF(CNAME,1,16,FILL,I)
         IF(I.GT.0) CNAME(I:) = ' '
         CLIST(13:) = CNAME
         IF(C.LT.NCLASS)THEN
            NUMBER = ROWLST(C+1)
            CNAME = ' '
            CALL FI2C(CNAME,NUMBER,FIRST)
            CLIST(30:) = CNAME(:8)//' IN'
            CNAME  = NAMECL(C+1)
            CALL FLOOKF(CNAME,1,16,FILL,I)
            IF(I.GT.0) CNAME(I:) = ' '
            CLIST(42:) = CNAME
         ENDIF
         CALL FSLEN(CLIST,60,LAST)
         CALL FTEXT(CLIST,LAST,2,0,LINE,'CLEAR ',*900)
650   CONTINUE
C
      IF(NOMTCH.GT.0)THEN
         CALL FI2C(CNAME,NOMTCH,FIRST)
         CLIST = '...'//CNAME(FIRST:8)//' IN SUBMATRIX DID NOT MATCH '
     1           //RNAME
         CALL FSLEN(CLIST,80,LAST)
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
      ENDIF
      IF(SWRC)THEN
         CALL FI2C(CNAME,MAXCL,FIRST)
         CLIST = '...THERE MAY BE MORE CLASSES (TALLY LIMIT = '//
     1           CNAME(FIRST:8)//')'
         CALL FSLEN(CLIST,80,LAST)
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
      ENDIF
C
900   RETURN
C
C ** ATALLY ENDS HERE
      END
