C            ::: TABLE.FOR 10-23-94 :::
C
C LAST DATES:  earlier dates deleted
C              5-08-93...Added SYNTAX option (CC 10-22)
C
C This contains the following routines to form tables from
C schema syntax.
C
C       TABLE.....Executes TABLE command (link with ANALYZE)
C       TBLDOM....Gets domain sets of class
C
      SUBROUTINE TABLE(CLIST,FIRST,LAST,RCODE)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
      INCLUDE 'DCSCHEMA.'
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCSCHEMA)
CI$$INSERT DCANAL
CI$$INSERT DCSCHEMA
C
C This executes TABLE command.
C    Syntax: TABLE {ROW | COL} name [,...] key [FOR set(s)] [//opt]
C                              :           :                CC 10-22
C                              :           :...L,U,X/Y,D/P,C
C                              :...of class        :...:...:..for col
C //opt := SYNTAX | NOSYNTAX | NOSORT
C          ================= Overides SWSYN
      CHARACTER*(*)   CLIST
C LOCAL
      CHARACTER*128 CTEMP,CTEMP2,HEADER
      PARAMETER     (TBLMXH=11,TBLMXS=50)
C                           :  :...MAX NUMBER OF TABLE STUBS/HEADS
C                           :...MAX NUMBER OF TABLE HEADS
      CHARACTER*4   ROWCOL,SET,MEMBER,
     1              TBLSET(PMXENT),TBLMEM(PMXENT),
     2              TBLHED(TBLMXS),TBLSTB(TBLMXS)
CC 10-22
      CHARACTER*8   SYNHED(0:TBLMXS),SYNSTB(-1:TBLMXS)
CC =====
      CHARACTER*8   OPTION(5),STR8,TBLVAL(TBLMXS,TBLMXS)
C                                  :...USING CHAR TO DISTINGUISH
C                                      BLANK FROM 0
      CHARACTER*1   TBLRIM,CHAR,FTNOTE,CHRFTN
      CHARACTER*16  RNAME,CNAME,TBLCLS(PMSCOL)
      LOGICAL*1     SW,SWFOOT,SWTBLS,SWSORT
CC 10-22                      =============
      INTEGER*2     TBLPSN(PMXENT+1),TBLPST(PMSCOL)
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
C CHECK THAT WE HAVE RESIDENT SCHEMA
      IF(SCHNR.LE.0 .OR. SCHNC.LE.0)THEN
C SET SCHEMA
         CALL ASCHDO(RCODE)
         IF(RCODE.NE.0)THEN
            PRINT *,' ...TABLE NEEDS SCHEMA'
            RETURN
         ENDIF
      ENDIF
CC 10-22
C SET SYNTAX OPTION DEFAULT TO SWITCH
      SWTBLS = SWSYN
C SET SORT OPTION DEFAULT
      SWSORT = .TRUE.
C SEE IF OPTION SPECIFIED
      CALL FLOOK(CLIST,FIRST,LAST,'//',IPARM)
      IF( IPARM.GT.0 )THEN
C -YES, PARSE
         F = IPARM+2
         OPTION(1) = 'SYNTAX '
         OPTION(2) = 'NOSYNTAX'
         OPTION(3) = 'NOSORT '
         RCODE = -1
40       CONTINUE
           NUMBER = 3
           CALL FOPTN(CLIST,F,LAST,OPTION,NUMBER,RCODE)
           IF( RCODE.NE.0 )GOTO 1305
           IF( NUMBER.EQ.0 )GOTO 49
           IF( NUMBER.EQ.3 )THEN
              SWSORT = .FALSE.
           ELSE
              SWTBLS = (NUMBER.EQ.1)
           ENDIF
         GOTO 40
49       CONTINUE
C END OPTION SPECS
         LAST = IPARM-1
      ENDIF
CC =====
C GET ROW | COL ...absence means use current settings
      CALL AGETRC(CLIST,FIRST,LAST,ROWCOL,RCODE)
      IF(RCODE.NE.0)RETURN
      IF(ROWCOL.EQ.' ')GOTO 1310
C GET ROW|COL CLASS(S)
      TBLNUM = 0
100   CONTINUE
         IF(TBLNUM.GE.TBLMXS)GOTO 1340
         IF(TBLNUM.GE.PMSCOL)GOTO 1375
         CALL FTOKEN(CLIST,FIRST,LAST,RNAME,16,CHAR)
         TBLNUM = TBLNUM+1
         TBLCLS(TBLNUM) = RNAME
      IF(CHAR.EQ.',')GOTO 100
      IF(RNAME.EQ.' ')GOTO 1320
C NOW TBLNUM = NUMBER OF ROWCOL CLASSES SPECIFIED >= 1
C     RNAME = LAST CLASS SPECIFIED
C SET OPTIONS
      OPTION(1) = 'L'
      OPTION(2) = 'U'
C ...OTHERS DEPEND ON ROWCOL
      IF(ROWCOL.EQ.'ROW ')THEN
         OPTION(3) = 'Y'
         OPTION(4) = 'P'
         NUMBER = 4
         I1 = RCSUB1(1)
         I2 = NROWS
         REMAIN = NRCSUB(1)
      ELSE
         OPTION(3) = 'X'
         OPTION(4) = 'D'
         OPTION(5) = 'C'
         NUMBER = 5
         I1 = RCSUB1(2)
         I2 = NCOLS
         REMAIN = NRCSUB(2)
      ENDIF
      IF( REMAIN.EQ.0 )GOTO 1335
      REMBER = REMAIN
      RCODE = -1
      CALL FOPTN(CLIST,FIRST,LAST,OPTION,NUMBER,RCODE)
      IF( RCODE.NE.0 )GOTO 1315
      TBLRIM = OPTION(NUMBER)(1:1)
C SEE IF 'FOR' OPTION SPECIFIED
      CALL FTOKEN(CLIST,FIRST,LAST,SET,4,CHAR)
      IF(SET.NE.' ')THEN
         CALL FMATCH(SET,'FOR ',' ',SW)
         IF(SW)CALL FTOKEN(CLIST,FIRST,LAST,SET,4,CHAR)
         IF(SET.EQ.' ')GOTO 1370
      ENDIF
C ==================== PARSE COMPLETE ========================
C       RNAME  = LAST CLASS SPECIFIED
C       SET    = SET SPECIFIED AFTER FOR (MAY BE ANOTHER ONE)
C                ...blank if FOR not specified
C       TBRIM  = key (TABLE ENTRIES)
C       TBLNUM = NUMBER OF CLASSES SPECIFIED
CC 10-22
C       SWTBLS = T MEANS SYNTAX SPECIFIED (SUBSTITUTE MEANINGS)
C       SWSORT = T MEANS SORT STUBS AND HEADS
CC =====
C ============================================================
C INITIALIZE STUB AND HEAD
      STUB = 0
      HEAD = 0
      IF( TBLNUM.GT.1 )GOTO 2000
C ============= 2-D TABLE FOR ONE ROW|COL CLASS ==============
C GET SCHEMA STRIP NUMBER
      CALL ASTRIP(ROWCOL,RNAME,NUMBER)
      IF( NUMBER.EQ.0 )GOTO 1325
      IF( ROWCOL.EQ.'ROW ' )THEN
         RNAME = SCHROW(NUMBER)
      ELSE
         RNAME = SCHCOL(NUMBER)
      ENDIF
C NOW RNAME = FULL CLASS SPEC, INCLUDING DOMAIN
C GET DOMAIN SETS AND RELATED INFO
      CALL TBLDOM(RNAME,NSET,TBLSET,TBLMEM,TBLPSN,LENCLS,*1390)
      IF(NSET.LT.2)GOTO 1330
C
C NOW LENCLS = LENGTH OF CLASS NAME AND
C     THE DOMAIN HAS NSET SETS...TBLMEM IS NULL AND TBLPSN(i) IS
C     THE POSITION IN THE ROWCOL NAME FOR SET i
C
      IF(SET.NE.' ')THEN
C USER SPECIFIED A SET...CHECK ITS IN DOMAIN OF CLASS
         DO 300 STUB=1,NSET
            IF(TBLSET(STUB).EQ.SET)GOTO 310
300      CONTINUE
         GOTO 1372
310      NSTUB = 0
C SET IS THE STUB...SEE IF USER SPECIFIED A SECOND SET
         CALL FTOKEN(CLIST,FIRST,LAST,SET,4,CHAR)
         IF(SET.NE.' ')THEN
C YES, CHECK DOMAIN
            DO 320 HEAD=1,NSET
               IF(TBLSET(HEAD).EQ.SET)GOTO 330
320         CONTINUE
            GOTO 1372
330         NHEAD = 0
C     SECOND SET IS HEAD
         ENDIF
      ENDIF
C
      FTNOTE = '#'
C FORM HEADER (MAY BE EXTENDED BY FIXED SET MEMBERS)
      IF(TBLRIM.EQ.'X'.OR.TBLRIM.EQ.'Y')THEN
         HEADER = 'Levels of '//RNAME
      ELSE IF(TBLRIM.EQ.'D'.OR.TBLRIM.EQ.'P')THEN
         HEADER = 'Prices of '//RNAME
         FTNOTE = '?'
      ELSE IF(TBLRIM.EQ.'C')THEN
         HEADER = 'Objective coefficients of '//RNAME
         FTNOTE = '?'
      ELSE IF(TBLRIM.EQ.'L')THEN
         HEADER = 'Lower bounds of '//RNAME
      ELSE
         HEADER = 'Upper bounds of '//RNAME
      ENDIF
C NOTE:  FTNOTE = FOOTNOTE CHAR IN CASE WE HAVE MULTIPLE ENTRIES
C               = # FOR QUANTITY AND ? FOR PRICE|COST
C
C FIND OUT WHICH SETS IN DOMAIN ARE VARYING IN THE SUBMATRIX
      DO 600 I=I1,I2
         CALL GETMAP(ROWCOL,I,SW)
         IF(.NOT.SW)GOTO 600
         REMAIN = REMAIN-1
         CALL GETNAM(ROWCOL,I,CNAME)
         IF(RNAME(:LENCLS).NE.CNAME(:LENCLS))GOTO 590
C THIS ROWCOL MATCHES CLASS NAME...LOOP OVER ITS DOMAIN
         DO 550 S=1,NSET
            BEGIN = TBLPSN(S)
            END   = TBLPSN(S+1)-1
            MEMBER= CNAME(BEGIN:END)
            IF(MEMBER.EQ.TBLMEM(S))GOTO 550
C     WE HAVE A MULTIPLE-CONDITION WITH ACTIONS AS FOLLOWS
C
C ======= CONDITIONS ========
C TBLMEM    S     HEAD  STUB                 ACTION
C ===========================  =======================================
C blank   =HEAD    >0   any    TBLMEM=MEMBER, initialize TBLHED=MEMBER
C         =STUB   any    >0    TBLMEM=MEMBER, initialize TBLSTB=MEMBER
C        <>HD,STB any   any    TBLMEM=MEMBER
C  *      =HEAD    >0   any    add MEMBER to TBLHED (if not there)
C         =STUB   any    >0    add MEMBER to TBLSTB (if not there)
C        <>HD,STB any   any    no action
C  m      =HEAD    >0   any    add MEMBER to TBLHED, TBLMEM=*
C         =STUB   any    >0    add MEMBER to TBLSTB, TBLMEM=*
C        <>HD,STB  =0   any    TBLMEM=*, MAKE HEAD=S...
C                              put m and MEMBER in TBLHED
C        <>HD,STB  >0    =0    TBLMEM=*, MAKE STUB=S...
C                              put m and MEMBER in TBLSTB
C        <>HD,STB  >0    >0    TBLMEM=*
C ====================================================================
            IF(TBLMEM(S).EQ.' ')THEN
C THIS IS FIRST MEMBER OF SET S
               TBLMEM(S) = MEMBER
               IF(S.EQ.HEAD)THEN
                  NHEAD = 1
                  TBLHED(1) = MEMBER
               ELSE IF(S.EQ.STUB)THEN
                  NSTUB = 1
                  TBLSTB(1) = MEMBER
               ENDIF
            ELSE IF(TBLMEM(S).EQ.'*')THEN
C SET S HAS ALREADY VARIED
               IF(S.EQ.HEAD)THEN
                  DO 400 H=1,NHEAD
400               IF(TBLHED(H).EQ.MEMBER)GOTO 550
                  IF(NHEAD.GE.TBLMXS)GOTO 1340
                  NHEAD = NHEAD+1
                  TBLHED(NHEAD) = MEMBER
               ELSE IF(S.EQ.STUB)THEN
                  DO 425 H=1,NSTUB
425               IF(TBLSTB(H).EQ.MEMBER)GOTO 550
                  IF(NSTUB.GE.TBLMXS)GOTO 1341
                  NSTUB = NSTUB+1
                  TBLSTB(NSTUB) = MEMBER
               ENDIF
            ELSE
C WE HAVE SEEN 1 MEMBER OF SET S (NOT THIS ONE)
               IF(S.EQ.HEAD)THEN
                  IF(NHEAD.GE.TBLMXS)GOTO 1340
                  NHEAD = NHEAD+1
                  TBLHED(NHEAD) = MEMBER
               ELSE IF(S.EQ.STUB)THEN
                  IF(NSTUB.GE.TBLMXS)GOTO 1341
                  NSTUB = NSTUB+1
                  TBLSTB(NSTUB) = MEMBER
               ELSE IF(HEAD.EQ.0)THEN
                  HEAD = S
                  NHEAD= 2
                  TBLHED(1) = TBLMEM(S)
                  TBLHED(2) = MEMBER
               ELSE IF(STUB.EQ.0)THEN
                  STUB = S
                  NSTUB= 2
                  TBLSTB(1) = TBLMEM(S)
                  TBLSTB(2) = MEMBER
               ENDIF
               TBLMEM(S) = '*'
            ENDIF
550      CONTINUE
C
590      IF(REMAIN.EQ.0)GOTO 610
600   CONTINUE
C
610   CONTINUE
      IF( HEAD.EQ.0.OR.(STUB.EQ.0.AND.NSET.NE.2) )GOTO 1350
      IF( STUB.EQ.0 )THEN
C ONLY ONE SET VARIES (HEAD), BUT DOMAIN HAS EXACTLY 2 SETS
C ...MAKE THE ONE THAT DOES NOT VARY THE STUB
         IF(HEAD.EQ.1)THEN
            STUB = 2
         ELSE
            STUB = 1
         ENDIF
         NSTUB = 1
         TBLSTB(1) = TBLMEM(STUB)
      ENDIF
C
C NOW STUB, HEAD = DOMAIN SETS THAT VARY IN SUBMATRIX FOR THIS CLASS
C     TBLHED(j) = j-TH COLUMN HEAD FOR j=1,...,NHEAD
C IF SETS NOT SPECIFIED (WITH FOR OPTION), THE FIRST VARYING SET IS
C HEAD AND 2ND IS STUB.  THIS AGREES WITH SYNTAX IF NSETS=2.
C
      IF( NHEAD.GT.TBLMXH ) GOTO 1340
      IF( NSTUB.GT.TBLMXS ) GOTO 1341
CC 10-22
      SYNSTB(-1) = ' '
C SET STUB AND HEAD LENGTHS
      IF( .NOT.SWTBLS )THEN
C SIMPLY SET BY SET NAME LENGTH (= MEMBER LENGTHS)
         CALL FSLEN(TBLSET(STUB),4,STBLEN)
         CALL FSLEN(TBLSET(HEAD),4,HEDLEN)
C COPY STUB AND HEAD TO SYNSTB AND SYNHED (IS WHAT WILL BE PRINTED)
         SYNSTB(0) = TBLSET(STUB)
         SYNHED(0) = TBLSET(HEAD)
         DO 6100 S=1,NSTUB
6100     SYNSTB(S) = TBLSTB(S)
         DO 6110 H=1,NHEAD
6110     SYNHED(H) = TBLHED(H)
      ELSE
C SYNTAX SPECIFIED
C ...COPY MEANINGS TO SYNSTB AND SYNHED, RESP. (TRUNCATED TO 8 CHARS)
         L = 1
         CTEMP = ' '
         CALL EXENTY(TBLSET(STUB),' ',CTEMP,L,RCODE)
         IF( RCODE.NE.0 )THEN
C NO TRANSLATION...USE SET KEY AND KEEP GOING
            CTEMP = TBLSET(STUB)
            RCODE = 0
         ENDIF
         F = 6
         L = F+20
         CALL FTOKEN(CTEMP,F,L,CNAME,16,CHAR)
         CALL FTOKEN(CTEMP,F,L,STR8,8,CHAR)
         CALL FSLEN(CNAME,9,LC)
         IF( LC.GT.8 )CNAME(8:) = '.'
         CALL FSLEN(STR8,8,LS)
         IF( LC.GT.LS )THEN
            STBLEN = LC
         ELSE
            STBLEN = LS
         ENDIF
         IF( STR8.EQ.' ' )THEN
            SYNSTB(0) = CNAME(:8)
         ELSE
            SYNSTB(-1)= CNAME(:8)
            SYNSTB(0) = STR8
         ENDIF
C
         DO 620 S=1,NSTUB
            L = 1
            CTEMP = ' '
            CALL EXENTY(TBLSET(STUB),TBLSTB(S),CTEMP,L,RCODE)
            IF( RCODE.NE.0 )THEN
C NO TRANSLATION...USE MEMBER KEY AND KEEP GOING
               CTEMP = TBLSTB(S)
               RCODE = 0
            ENDIF
            SYNSTB(S) = CTEMP(:8)
            CALL FSLEN(SYNSTB(S),8,L)
            IF( STBLEN.LT.L )STBLEN = L
620      CONTINUE
C SYNSTB(-1) & SYNSTB(0) = 1ST 2 WORDS OF SET MEANING
C                         (EACH TRUNCATED TO 8 CHARS)
C SYNSTB(S) = MEANING OF MEMBER S (TRUNCATED TO 8 CHARS)
C STBLEN    = MAX{SYNSTB LENGTHS}
C
         L = 1
         CTEMP = ' '
         CALL EXENTY(TBLSET(HEAD),' ',CTEMP,L,RCODE)
         IF( RCODE.NE.0 )THEN
C NO TRANSLATION...USE SET KEY AND KEEP GOING
            CTEMP = TBLSET(HEAD)
            RCODE = 0
         ENDIF
         SYNHED(0) = CTEMP(6:13)
         CALL FSLEN(SYNHED(0),8,HEDLEN)
         DO 625 H=1,NHEAD
            L = 1
            CTEMP = ' '
            CALL EXENTY(TBLSET(HEAD),TBLHED(H),CTEMP,L,RCODE)
            IF( RCODE.NE.0 )THEN
C NO TRANSLATION...USE MEMBER KEY AND KEEP GOING
               CTEMP = TBLHED(H)
               RCODE = 0
            ENDIF
            SYNHED(H) = CTEMP(:8)
            CALL FSLEN(SYNHED(H),8,L)
            IF( HEDLEN.LT.L )HEDLEN = L
625      CONTINUE
C SYNHED(0) = MEANING OF SET (TRUNCATED TO 8 CHARS)
C SYNHED(H) = MEANING OF MEMBER H (TRUNCATED TO 8 CHARS)
C HEDLEN    = MAX{SYNHED LENGTHS}
      ENDIF
C ============================================================
C NOW SYNSTB(S) = STUB MEMBER S TO BE PRINTED (KEY OR MEANING)
C NOW SYNHED(H) = HEAD MEMBER H TO BE PRINTED (KEY OR MEANING)
C ============================================================
CC =====
      IF( NHEAD.GT.1 .AND. SWSORT )THEN
CC 10-22             ============ THIS AFFECTS SORT, WHICH NOW USES SYN
C SORT HEADS (USE BUBBLE BECAUSE LIST IS SMALL)
640     SW = .FALSE.
          DO 641 H=1,NHEAD-1
             CNAME = SYNHED(H+1)
CC 10-22             :...WAS TBLHED (AND BELOW)
             IF( CNAME(:8).LT.SYNHED(H) )THEN
C   BUBBLE UP
                SYNHED(H+1)= SYNHED(H)
                SYNHED(H)  = CNAME
                CNAME      = TBLHED(H+1)
                TBLHED(H+1)= TBLHED(H)
                TBLHED(H)  = CNAME
                SW = .TRUE.
             ENDIF
641       CONTINUE
        IF( SW )GOTO 640
      ENDIF
      IF( NSTUB.GT.1 .AND. SWSORT )THEN
CC 10-22             ============
C SORT STUBS
645     SW = .FALSE.
          DO 646 S=1,NSTUB-1
             CNAME = SYNSTB(S+1)
CC 10-22             :...WAS TBLSTB (AND BELOW)
             IF( CNAME(:8).LT.SYNSTB(S) )THEN
C   BUBBLE UP
                SYNSTB(S+1)= SYNSTB(S)
                SYNSTB(S)  = CNAME
                CNAME      = TBLSTB(S+1)
                TBLSTB(S+1)= TBLSTB(S)
                TBLSTB(S)  = CNAME
                SW = .TRUE.
             ENDIF
646       CONTINUE
        IF( SW )GOTO 645
      ENDIF
C
      IF(NSET.GT.2)THEN
C ADD FIXED SETS TO HEADER
         CALL FSLEN(HEADER,128,LAST)
         HEADER(LAST+2:) = 'for'
         LAST = LAST+5
         DO 650 S=1,NSET
            IF(S.EQ.STUB.OR.S.EQ.HEAD)GOTO 650
            SET = TBLSET(S)
            CALL FSLEN(SET,4,LS)
            CALL FSLEN(TBLMEM(S),4,LM)
            HEADER(LAST+1:) = SET(:LS)//'='//TBLMEM(S)(:LM)//','
            LAST = LAST + LS+LM + 2
650      CONTINUE
C REMOVE LAST COMMA
         HEADER(LAST:) = ' '
      ENDIF
C
C PRINT HEADER
      CALL FCENTR(HEADER,FIRST,LAST)
      LINE = 0
      CALL FTEXT(HEADER,LAST,1,0,LINE,'CLEAR ',*9000)
C
C FORM TABLE HEADS
      CTEMP = SYNSTB(0)
      TAB   = STBLEN+3
CC 10-22             :...WAS 4
      DO 705 H=1,NHEAD
         CTEMP(TAB:) = SYNHED(H)
CC 10-22               :...WAS TBLHED
         TAB = TAB+9
C INITIALIZE TABLE ENTRIES
         DO 700 S=1,NSTUB
700      TBLVAL(S,H) = ' '
705   CONTINUE
C
      CTEMP(TAB:) = '.'
C                    :...THIS IS TO ENSURE SPACE FOR LAST VALUE
      CALL FSLEN(CTEMP,128,L)
C                          :...THIS IS USED ONLY FOR ERROR MESSAGE
      CALL FCENTR(CTEMP,FIRST,LAST)
      IF(CTEMP(LAST:LAST).NE.'.')GOTO 1360
      CTEMP(LAST:) = ' '
C                     :...NOW LAST PERIOD IS REMOVED
C SET BEGINNINGS OF STUB AND HEAD WITH INDENT FOR CENTERING
      BEGSTB = FIRST+1
      BEGHED = BEGSTB+STBLEN+2
C
C PRINT HEAD SET NAME
      CTEMP2 = ' '
CC 10-23
      CTEMP2(BEGSTB-1:) = SYNSTB(-1)
CC =====
      L = LAST-3
      DO 710 I=BEGHED-1,L
CC 10-22             ==
710   CTEMP2(I:I) = '='
      K = (L+BEGHED-HEDLEN)/2
      CTEMP2(K:K+HEDLEN) = ' '//SYNHED(0)(:HEDLEN)
CC 10-22                        :...WAS TBLSET(HEAD)
      CALL FTEXT(CTEMP2,L,1,0,LINE,'CLEAR ',*9000)
C PRINT HEAD MEMBERS
      L = LAST
      CALL FTEXT(CTEMP,L,1,0,LINE,'CLEAR ',*9000)
C UNDERLINE HEAD
      DO 720 I=BEGSTB-1,LAST-3
720   CTEMP(I:I) = '='
      CTEMP(BEGHED-2:BEGHED-2)    = ' '
CC 10-22           :.WAS 1  :.WAS 1  :...HAD 2 BLANKS
      CALL FTEXT(CTEMP,LAST,1,0,LINE,'CLEAR ',*9000)
C
      REMAIN = REMBER
      SWFOOT = .FALSE.
C
C ::: FINAL LOOP OVER SUBMATRIX TO GET TABLE ENTRIES :::
      DO 800 I=I1,I2
         CALL GETMAP(ROWCOL,I,SW)
         IF(.NOT.SW)GOTO 800
         REMAIN = REMAIN-1
         CALL GETNAM(ROWCOL,I,CNAME)
         IF(RNAME(:LENCLS).NE.CNAME(:LENCLS))GOTO 790
C THIS ROWCOL MATCHES CLASS NAME
C ...LOCATE STUB
         BEGIN = TBLPSN(STUB)
         END   = TBLPSN(STUB+1)-1
         MEMBER= CNAME(BEGIN:END)
         DO 740 S=1,NSTUB
740      IF(MEMBER.EQ.TBLSTB(S))GOTO 741
         PRINT *,' ** SYSERR IN TABLE...STUB ',MEMBER,NSTUB
         IF(SWDBG.AND.NSTUB.GT.0)
     1      PRINT *,' TBLSTB:',(TBLSTB(S),S=1,NSTUB)
         GOTO 1390
741      CONTINUE
C LOCATE HEAD
         BEGIN = TBLPSN(HEAD)
         END   = TBLPSN(HEAD+1)-1
         MEMBER= CNAME(BEGIN:END)
         DO 750 H=1,NHEAD
750      IF(MEMBER.EQ.TBLHED(H))GOTO 751
         PRINT *,' ** SYSERR IN TABLE...HEAD ',MEMBER,NHEAD
         IF(SWDBG.AND.NHEAD.GT.0)
     1      PRINT *,' TBLHED:',(TBLHED(H),H=1,NHEAD)
         GOTO 1390
751      CONTINUE
C
         STR8   = TBLVAL(S,H)
         CHRFTN = ' '
         IF(STR8.NE.' ')THEN
C A VALUE WAS ALREADY PUT THERE
C                        (EG, SEE S(MT,SR)n FOR n=1,2 IN WOODNET)
            CALL FLOOK(STR8,1,8,FTNOTE,FN)
            IF(FN.GT.0)THEN
               CHRFTN = FTNOTE
               STR8(FN:FN)=' '
            ENDIF
            CALL FC2R(STR8,VOLD,RCODE)
            IF(RCODE.NE.0)THEN
               PRINT *,' ** SYSERR IN TABLE...STR8=',STR8
               GOTO 1390
            ENDIF
         ENDIF
C GET VALUE TO PUT INTO TABLE
         IF(TBLRIM.EQ.'C'.OR.TBLRIM.EQ.'L'.OR.TBLRIM.EQ.'U')THEN
            CALL GETRIM(ROWCOL,I,VL,VU,V,NZ)
            IF(TBLRIM.EQ.'L')V = VL
            IF(TBLRIM.EQ.'U')V = VU
         ELSE
            CALL GETSOL(ROWCOL,I,V,VP,CHAR,STNUM)
            IF(TBLRIM.EQ.'P' .OR. TBLRIM.EQ.'D')V = VP
         ENDIF
         IF(STR8.NE.' ')THEN
C WE MUST COMBINE WITH OLD VALUE (VOLD)
            IF(TBLRIM.EQ.'L' .OR. TBLRIM.EQ.'U' .OR.
     1         TBLRIM.EQ.'X' .OR. TBLRIM.EQ.'Y')THEN
C    ADD QUANTITIES
               IF(VOLD.NE.0.)THEN
                  IF(V.EQ.0.)THEN
                     V = VOLD
                  ELSE
C    ...AND PUT FOOTNOTE MARK
                     CHRFTN = FTNOTE
                     SWFOOT = .TRUE.
                     V = V+VOLD
                  ENDIF
               ENDIF
C     NOTE:  JUST IGNORE (NO FOOTNOTE) IF V OR VOLD = 0
            ELSE IF(V.NE.VOLD)THEN
C    PRICE | COST
C     NOTE:  JUST IGNORE IF V = VOLD
               CHRFTN = FTNOTE
               SWFOOT = .TRUE.
               IF(TBLRIM.EQ.'C')THEN
C    MAX COSTS
                  IF(V.LT.VOLD)V = VOLD
               ELSE
C    MAX PRICES, BUT ADJUST FOR SIGN (IE, MOST NEGATIVE FOR STAT=U)
                  IF(ABS(V).LT.ABS(VOLD))V = VOLD
               ENDIF
            ENDIF
         ENDIF
C CONVERT VALUE TO STRING
         CALL FR2C(CNAME,V)
C LEFT JUSTIFY
         L = 8
         CALL FSQUEZ(CNAME,L)
         IF(L.EQ.8.AND.CHRFTN.EQ.FTNOTE)L=7
         CNAME(L+1:) = CHRFTN
C INSERT INTO TABLE
         TBLVAL(S,H) = CNAME(1:8)
C OK, READY FOR NEXT ROWCOL
790      IF(REMAIN.EQ.0)GOTO 810
800   CONTINUE
C
810   CONTINUE
C
C PRINT BY ROWS
      CTEMP = ' '
      DO 1100 S=1,NSTUB
         CTEMP(BEGSTB:) = SYNSTB(S)
CC 10-22                  :...WAS TBLSTB
         TAB = BEGHED
         DO 1000 H=1,NHEAD
            CTEMP(TAB:) = TBLVAL(S,H)
            TAB = TAB+9
1000     CONTINUE
         CALL FTEXT(CTEMP,TAB,1,0,LINE,'CLEAR',*9000)
1100  CONTINUE
C
C CHECK FOR FOOTNOTE
      IF(SWFOOT)THEN
         CTEMP = FTNOTE//' multiple entries'
         CALL FSLEN(CTEMP,50,LAST)
         IF(FTNOTE.EQ.'?')THEN
            IF(TBLRIM.EQ.'C')THEN
               CTEMP(LAST+1:) = '; the max is shown.'
            ELSE
               CTEMP(LAST+1:) = '; the max magnitude is shown.'
            ENDIF
         ELSE
            CTEMP(LAST+1:) = ', which were added.'
         ENDIF
         CALL FCENTR(CTEMP,FIRST,LAST)
         CALL FTEXT(CTEMP,LAST,1,0,LINE,'CLEAR ',*9000)
      ENDIF
C
      RETURN
C
2000  CONTINUE
C =============== 1-D WITH MULTIPLE CLASSES ================
C CLASSES FORM HEADS AND COMMON SET FORMS STUBS
C SET = COMMON SET IN DOMAIN (BLANK IF NOT SPECIFIED)
C NOTE:  IF COMMON SET NOT SPECIFIED, WE'LL LOOK FOR UNIQUE CHOICE
C        FROM SYNTAX.
C LOOKUP CLASSES AND SET DOMAINS
      DO 2300 T=1,TBLNUM
         RNAME = TBLCLS(T)
         CALL ASTRIP(ROWCOL,RNAME,NUMBER)
         IF(NUMBER.EQ.0)GOTO 1325
         IF(ROWCOL.EQ.'ROW ')THEN
            RNAME = SCHROW(NUMBER)
         ELSE
            RNAME = SCHCOL(NUMBER)
         ENDIF
         TBLCLS(T) = RNAME
         CALL TBLDOM(RNAME,NSET,TBLSET,TBLMEM,TBLPSN,LENCLS,*1390)
         IF(NSET.EQ.0)GOTO 1331
         IF(LENCLS.GT.8)GOTO 1380
         IF(SET.NE.' ')THEN
C MAKE SURE COMMON SET IS IN DOMAIN
            DO 2150 S=1,NSET
               IF(TBLSET(S).EQ.SET)THEN
C   RECORD SET MEMBER POSITION IN NAME FOR THIS CLASS
                  TBLPST(T) = TBLPSN(S)
                  GOTO 2300
               ENDIF
2150        CONTINUE
            PRINT *,' ** ',SET,' IS NOT IN DOMAIN OF ',RNAME
            GOTO 1390
         ELSE
C   PUT SETS IN COMMON DOMAIN CANDIDATE LIST (USING TBLHED)
            IF(T.EQ.1)THEN
C   THIS IS FIRST CLASS, SO INITIALIZE CANDIDATE LIST TO THESE SETS
               DO 2160 C=1,NSET
2160           TBLHED(C) = TBLSET(C)
               NDOMS = NSET
            ELSE
C   REMOVE DOMAIN SETS NOT IN THIS DOMAIN
               D = 1
2170           CONTINUE
C   SEE IF DOMAIN SET, TBLHED(D), IS IN THIS DOMAIN
                 DO 2175 C=1,NSET
2175             IF(TBLHED(D).EQ.TBLSET(C))GOTO 2180
C   IT ISN'T, SO REMOVE D-TH DOMAIN SET FROM LIST OF CANDIDATES
                 TBLHED(D) = TBLHED(NDOMS)
                 IF(NDOMS.EQ.1)GOTO 1371
C                        :...NO COMMON SET IN CANDIDATE LIST REMAINS
                 NDOMS = NDOMS-1
                 IF(D.GT.NDOMS)GOTO 2185
               GOTO 2170
2180             IF(D.GE.NDOMS)GOTO 2185
                 D = D+1
               GOTO 2170
C    NOW TBLHED(1),...,TBLHED(NDOMS) ARE SETS THAT ARE IN ALL
C    DOMAINS SO FAR
2185           CONTINUE
            ENDIF
         ENDIF
C RECORD SET MEMBER POSITION IN NAME FOR THIS CLASS
2190     TBLPST(T) = TBLPSN(S)
2300  CONTINUE
C
      IF(SET.EQ.' ')THEN
C SEE IF UNIQUE CHOICE OF COMMON SET
         IF(NDOMS.GT.1)GOTO 1371
C YES
         SET = TBLHED(1)
C ...GO BACK AND PUT NAME POSITION
         DO 2400 T=1,TBLNUM
            CALL TBLDOM(TBLCLS(T),NSET,TBLSET,TBLMEM,TBLPSN,LENCLS,
     1                  *1390)
            DO 2350 S=1,NSET
               IF(TBLSET(S).EQ.SET)THEN
                  TBLPST(T) = TBLPSN(S)
                  GOTO 2360
               ENDIF
2350        CONTINUE
2360              CONTINUE
2400     CONTINUE
      ENDIF
      CALL FSLEN(SET,4,STBLEN)
C                      :...STUB LENGTH
C
C NOW SET = COMMON SET IN DOMAINS OF CLASSES TBLCLS(t) AND
C     TBLPST(t) = POSITION IN CLASS NAME, FOR t=1,...,TBLNUM
C
C FIRST LOOP OVER SUBMATRIX TO GET SET VARIATION
C
      NSTUB = 0
C
      DO 2500 I=I1,I2
         CALL GETMAP(ROWCOL,I,SW)
         IF(.NOT.SW)GOTO 2500
         REMAIN = REMAIN-1
         CALL GETNAM(ROWCOL,I,CNAME)
         DO 2450 T=1,TBLNUM
            RNAME = TBLCLS(T)
            CALL FLOOK(RNAME,2,10,'(',BEGDOM)
            LENCLS = BEGDOM-1
            IF(RNAME(:LENCLS).EQ.CNAME(:LENCLS))GOTO 2460
2450     CONTINUE
C ROWCOL NOT IN ANY CLASS
         GOTO 2490
2460     CONTINUE
C THIS ROWCOL (CNAME) MATCHES CLASS NAME (RNAME(:LENCLS))
C GET MEMBER OF COMMON SET
         BEGIN = TBLPST(T)
         END   = BEGIN+STBLEN-1
         MEMBER= CNAME(BEGIN:END)
         IF(NSTUB.EQ.0)THEN
C FIRST MEMBER TO PUT IN STUB
            NSTUB = 1
         ELSE
C SEE IF MEMBER ALREADY IN STUB
            DO 2475 S=1,NSTUB
               IF(TBLSTB(S).EQ.MEMBER)GOTO 2490
2475        CONTINUE
C NO, CHECK OVERFLOW
            IF(NSTUB.GE.TBLMXS)GOTO 1341
            NSTUB = NSTUB+1
         ENDIF
         TBLSTB(NSTUB)  = MEMBER
         DO 2480 T=1,TBLNUM
2480     TBLVAL(NSTUB,T)= ' '
C NEXT ROWCOL
2490     IF(REMAIN.LE.0)GOTO 2510
2500  CONTINUE
C
2510  CONTINUE
C
C NOW TBLSTB(i) = i-TH STUB OF TABLE (= MEMBER OF COMMON SET)
C     TBLVAL = NULL FOR ALL CLASSES (IE, TABLE HEADS)
C
      IF(NSTUB.EQ.0)GOTO 1385
C
C PRINT HEADER
      CTEMP = 'Table Entries ='
      IF(TBLRIM.EQ.'X'.OR.TBLRIM.EQ.'Y')THEN
         CTEMP(17:) = '(Total) Levels'
      ELSE IF(TBLRIM.EQ.'L')THEN
         CTEMP(17:) = '(Total) Lower bounds'
      ELSE IF(TBLRIM.EQ.'U')THEN
         CTEMP(17:) = '(Total) Upper bounds'
      ELSE IF(TBLRIM.EQ.'D'.OR.TBLRIM.EQ.'P')THEN
         CTEMP(17:) = '(Absolute Max) Prices'
      ELSE
         CTEMP(17:) = '(Max) Objective coefficients'
      ENDIF
      CALL FCENTR(CTEMP,FIRST,LAST)
      LINE = 0
      CALL FTEXT(CTEMP,LAST,1,0,LINE,'CLEAR ',*9000)
C
C FORM TABLE HEADS
      CTEMP = SET
      TAB = STBLEN+3
C                  :...Changed from 4, 5-8-93
      DO 2700 T=1,TBLNUM
         RNAME = TBLCLS(T)
         CALL FLOOK(RNAME,2,10,'(',BEGDOM)
         CTEMP(TAB:) = RNAME(:BEGDOM-1)
         TAB = TAB+9
2700  CONTINUE
      CTEMP(TAB:) = '.'
C                    :...THIS IS TO ENSURE SPACE FOR LAST VALUE
      CALL FSLEN(CTEMP,128,L)
C                          :...THIS IS USED ONLY FOR ERROR MESSAGE
      CALL FCENTR(CTEMP,FIRST,LAST)
      IF(CTEMP(LAST:LAST).NE.'.')GOTO 1360
      CTEMP(LAST:) = ' '
C                     :...NOW IT'S REMOVED
C PRINT HEAD MEMBERS
      L = LAST
      CALL FTEXT(CTEMP,L,1,0,LINE,'CLEAR ',*9000)
C SET BEGINNINGS OF STUB AND HEAD WITH INDENT FOR CENTERING
      BEGSTB = FIRST
      BEGHED = BEGSTB+STBLEN+2
C UNDERLINE HEAD
      DO 2810 I=BEGSTB,LAST-2
2810  CTEMP(I:I) = '='
      CTEMP(BEGHED-1:BEGHED-1) = ' '
CC 10-22           :...WAS 2
      CALL FTEXT(CTEMP,LAST,1,0,LINE,'CLEAR ',*9000)
C
C ::::: FINAL LOOP OVER SUBMATRIX TO FORM ENTRIES (TBLVAL) :::::
C
      REMAIN = REMBER
      DO 3500 I=I1,I2
         CALL GETMAP(ROWCOL,I,SW)
         IF(.NOT.SW)GOTO 3500
         REMAIN = REMAIN-1
         CALL GETNAM(ROWCOL,I,CNAME)
         DO 3450 T=1,TBLNUM
            RNAME = TBLCLS(T)
            CALL FLOOK(RNAME,2,10,'(',BEGDOM)
            LENCLS = BEGDOM-1
            IF(RNAME(:LENCLS).EQ.CNAME(:LENCLS))GOTO 3460
3450     CONTINUE
C ROWCOL NOT IN ANY CLASS
         GOTO 3490
3460     CONTINUE
C THIS ROWCOL (CNAME) MATCHES CLASS NAME
C GET MEMBER OF COMMON SET
         BEGIN = TBLPST(T)
         END   = BEGIN+STBLEN-1
         MEMBER= CNAME(BEGIN:END)
C LOCATE MEMBER IN STUB
         DO 3475 S=1,NSTUB
            IF(TBLSTB(S).EQ.MEMBER)GOTO 3480
3475     CONTINUE
         PRINT *,' ** SYSERR IN TABLE...',MEMBER
         GOTO 1390
3480     CONTINUE
C NOW S = STUB, T=HEAD
C GET VALUE
         IF(TBLRIM.EQ.'C'.OR.TBLRIM.EQ.'L'.OR.TBLRIM.EQ.'U')THEN
            CALL GETRIM(ROWCOL,I,VL,VU,V,NZ)
            IF(TBLRIM.EQ.'L')V = VL
            IF(TBLRIM.EQ.'U')V = VU
         ELSE
            CALL GETSOL(ROWCOL,I,V,VP,CHAR,STNUM)
            IF(TBLRIM.EQ.'P' .OR. TBLRIM.EQ.'D')V = VP
         ENDIF
         STR8 = TBLVAL(S,T)
         IF(STR8.NE.' ')THEN
C WE MUST COMBINE WITH OLD VALUE
            CALL FC2R(STR8,VOLD,RCODE)
            IF(RCODE.NE.0)THEN
               PRINT *,' ** SYSERR IN TABLE...STR8=',STR8
               GOTO 1390
            ENDIF
            IF(TBLRIM.EQ.'L' .OR. TBLRIM.EQ.'U' .OR.
     1         TBLRIM.EQ.'X' .OR. TBLRIM.EQ.'Y')THEN
C    ADD QUANTITIES
               V = V+VOLD
            ELSE
C    PRICE | COST
               IF(TBLRIM.EQ.'C')THEN
C    MAX COSTS
                  IF(V.LT.VOLD)V = VOLD
               ELSE
C    MAX PRICES, BUT ADJUST FOR SIGN (IE, MOST NEGATIVE FOR STAT=U)
                  IF(ABS(V).LT.ABS(VOLD))V = VOLD
               ENDIF
            ENDIF
         ENDIF
C CONVERT VALUE TO STRING
         CALL FR2C(CNAME,V)
C LEFT JUSTIFY
         L = 8
         CALL FSQUEZ(CNAME,L)
C PUT TABLE ENTRY
         TBLVAL(S,T) = CNAME
C NEXT ROWCOL
3490     IF(REMAIN.LE.0)GOTO 3510
3500  CONTINUE
C
3510  CONTINUE
C
C NOW TBLSTB(i) = i-TH MEMBER OF COMMON SET = STUB
C     TBLVAL(i,j) = ENTRY i,j  FOR i=1,...,NSTUB AND j=1,...,TBLNUM
C
C PRINT BY ROWS
      CTEMP = ' '
      DO 3900 S=1,NSTUB
         CTEMP(BEGSTB:) = TBLSTB(S)
         TAB = BEGHED
         DO 3800 T=1,TBLNUM
            CTEMP(TAB:) = TBLVAL(S,T)
            TAB = TAB+9
3800     CONTINUE
         CALL FTEXT(CTEMP,TAB,1,0,LINE,'CLEAR',*9000)
3900  CONTINUE
C
9000  RETURN
C
C ERROR RETURNS
CC 10-22
1305  PRINT *,' ** SYNTAX SPEC EXPECTED AFTER //'
      GOTO 1390
CC =====
1310  PRINT *,' ** ROW or COL expected after TABLE command'
      GOTO 1390
1315  PRINT *,' ...THESE ARE THE ',ROWCOL,'RIM KEYS FOR TABLE'
      GOTO 1390
1320  PRINT *,' ** ',ROWCOL,'CLASS NAME EXPECTED'
      GOTO 1390
1325  PRINT *,' ** ',ROWCOL,RNAME,' NOT RECOGNIZED'
      GOTO 1390
1330  PRINT *,' ** DOMAIN OF ',ROWCOL,RNAME,' HAS < 2 SETS'
      GOTO 1390
1331  PRINT *,' ** ',ROWCOL,RNAME,' HAS NULL DOMAIN'
      GOTO 1390
1335  PRINT *,' ** SUBMATRIX HAS NO ',ROWCOL
      GOTO 1390
1340  PRINT *,' ** TABLE HEAD TOO LARGE...MAX =',TBLMXH
      GOTO 1390
1341  PRINT *,' ** TABLE STUB TOO LARGE...MAX =',TBLMXS
      GOTO 1390
1350  PRINT *,' ** ',ROWCOL,RNAME,' DOES NOT HAVE 2 SETS VARYING',
     1        ' IN SUBMATRIX'
      GOTO 1390
1360  PRINT *,' ** SCREEN WIDTH TOO SMALL FOR TABLE DISPLAY',
     1        '...NEED',L
      GOTO 1390
1370  PRINT *,' ** SET EXPECTED AFTER FOR'
      GOTO 1390
1371  PRINT *,' ** COMMON SET NOT DEFINED...USE FOR'
      GOTO 1390
1372  PRINT *,' ** SET ',SET,' NOT IN DOMAIN OF ',RNAME
      GOTO 1390
1375  PRINT *,' ** TOO MANY ',ROWCOL,'CLASSES...MAX =',PMSCOL
      GOTO 1390
1380  PRINT *,' ** CLASS NAME TOO LARGE...MAX = 8'
      GOTO 1390
1385  PRINT *,' ** SET ',SET,' DOES NOT VARY IN SUBMATRIX'
1390  CONTINUE
      IF(SWRULE)THEN
         PRINT *,' ...RULEBASE ERROR WITH TABLE COMMAND'
      ELSE
         PRINT *,' ...TABLE ABORTED'
      ENDIF
      RCODE = 1
      RETURN
C
C ** TABLE ENDS HERE
      END
      SUBROUTINE TBLDOM(RNAME,NSET,TBLSET,TBLMEM,TBLPSN,LENCLS,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
      INCLUDE 'DCSCHEMA.'
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCSCHEMA)
CI$$INSERT DCANAL
CI$$INSERT DCSCHEMA
C
C This gets domain of class RNAME = c(a,...,z)
C                                   : :     :...last set in domain
C                                   : :...first set in domain
C                                   :...class name
C  Returns:
C     NSET      = Number of sets in domain
C     TBLSET(i) = Name of i-th set (i=1,...,NSET)
C     TBLMEM initalized to blanks (member of set)
C     TBLPSN(i) = Position of i-th set member in row/col name
C     LENCLS    = Length of class name -- ie, name = RNAME(:LENCLS)
C
C ...Alternate return is SYSERR.
C
      CHARACTER*16  RNAME
      CHARACTER*4   TBLSET(PMXENT),TBLMEM(PMXENT)
      INTEGER*2     TBLPSN(PMXENT+1)
C LOCAL
      CHARACTER*4   SET
      CHARACTER*1   CHAR
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
      NSET = 0
C LOCATE BEGINNING OF DOMAIN
      CALL FLOOK(RNAME,2,15,'(',BEGDOM)
      IF(BEGDOM.EQ.0)RETURN
      LENCLS = BEGDOM-1
      BEGDOM = BEGDOM+1
C NOW LENCLS = LENGTH OF CLASS NAME AND
C     RNAME(BEGDOM:) CONTAINS DOMAIN OF RNAME
C
C INITIALIZE DOMAIN SET MEMBERS
      POSN = LENCLS+1
100   CONTINUE
        CALL FTOKEN(RNAME,BEGDOM,16,SET,4,CHAR)
        IF(SET.EQ.' ')GOTO 1300
        CALL FSLEN(SET,4,LS)
        NSET = NSET+1
        TBLSET(NSET) = SET
        TBLMEM(NSET) = ' '
        TBLPSN(NSET) = POSN
        POSN = POSN+LS
      IF(CHAR.NE.')')GOTO 100
      TBLPSN(NSET+1) = POSN
C
      RETURN
C
1300  PRINT *,' ** SYSERR IN TBLDOM...',RNAME
      RETURN 1
C
C ** TBLDOM ENDS HERE
      END
