C               ::: BLKPLOT.FOR  1-14-94 :::
C
C LAST DATE:  11-25-91
C             12-03-91...Changed sort default in STEPLT
C              1-14-92...Added BY option to SHOW PLOT and changed
C                        command input syntax (APLOT removed)
C              7-24-93...Added step XMIN,YMIN
C              8-03-93...Added SLABEL in STEPLT
C              8-14-93...Change for increasing EXTABL
C
C This contains the following ANALYZE routines (for NONDOS only).
C
C       BLKPLT....Plot row/col blocks
C       STEPLT....Execute SHOW PLOT
C
C These require the SCREEN module.
C
      SUBROUTINE BLKPLT(ROWCOL,CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This plots row/col blocks.
C   Syntax:  BLOCK {ROW | COL} PLOT [header [//form]]
C            :                    :  :...CLIST(FIRST:)
C            :....................:
C           already parsed by caller
C
C       form   := GRAPHIC | PRINTABL
C       ROWCOL := ROW | COL
C
      CHARACTER*(*)  ROWCOL
      CHARACTER*128  CLIST
C LOCAL
      CHARACTER*8    FORM,OPTION(2)
      REAL           VNUMBR(PMXBLK)
      DATA    OPTION/'GRAPHIC','PRINTABL'/
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
C SET BLOCK INFO ACCORDING TO ROWCOL
      IF(ROWCOL.EQ.'ROW ')THEN
         IF(NRBLKS.EQ.0)THEN
            PRINT *,' NO ROW BLOCKS TO PLOT'
            RETURN
         ENDIF
         BLK1  = 1
         NUMBER= NRBLKS
      ELSE
         IF(NCBLKS.EQ.0)THEN
            PRINT *,' NO COLUMN BLOCKS TO PLOT'
            RETURN
         ENDIF
         BLK1  = MAXBLK-NCBLKS+1
         NUMBER= NCBLKS
      ENDIF
C SEE IF FORM SPECIFIED
      CALL FLOOK(CLIST,FIRST,LAST,'//',I)
      IF( I.GT.0 )THEN
         F = I+2
         N = 2
         RCODE = -1
         CALL FOPTN(CLIST,F,LAST,OPTION,N,RCODE)
         IF(RCODE.NE.0)RETURN
         FORM = OPTION(N)
C ERASE SPEC (FROM HEADER)
         CLIST(I:) = ' '
      ELSE
C DEFAULT FORM = GRAPHIC
         FORM = OPTION(1)
      ENDIF
C
C CONVERT BLOCK NUMBERS TO REAL VALUES (FOR SCRBAR)
      DO 100 I=1,NUMBER
100   VNUMBR(I) = BNUMBR(BLK1+I-1)
C                      HEADER        XLABEL  YLABEL  # BLOCKS
C                      :             :       :       :
      CALL SCRBAR(FORM,CLIST(FIRST:),ROWCOL,'NUMBER',NUMBER,
     1            BNAME(BLK1),  VNUMBR,*1300)
C                 :             :
C                 BLOCK LABELS  BLOCK VALUES (=# IN BLOCK)
      RETURN
1300  RCODE = 1
      RETURN
C
C ** BLKPLT ENDS HERE
      END
      SUBROUTINE STEPLT(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This executes SHOW PLOT command.
C  Syntax: PLOT [row] [//[BY row]][LABEL set][NOLABEL][NOSORT][PRINTABL]
C
      CHARACTER*(*)  CLIST
C LOCAL
      PARAMETER    (LCLNUM=20,YMIN=1.0E-6)
C                   :         :...MIN STEP HEIGHT (PURIFIED TO 0)
C                   :...MAX NUMBER OF ACTIVITIES IN A PLOT
C                                     (# STEPS)
      REAL         XVALUE(LCLNUM),YVALUE(LCLNUM)
      LOGICAL*1    SW,SWFAC,SWSORT,SWLABL
      CHARACTER*64 HEADER
      CHARACTER*1  CHAR
      CHARACTER*16 RNAME,CNAME,YNAME,XNAME,SLABEL(LCLNUM)
      CHARACTER*8  FORM,OPTION(5),STR8,DOMSET
      DATA    OPTION/'BY ','LABEL','NOLABEL','NOSORT','PRINTABL'/
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
C SET DEFAULT OPTIONS
      FORM = 'GRAPHIC'
      SWSORT = .TRUE.
      SWLABL = .TRUE.
      DOMSET = ' '
      BYROW = 0
      XNAME = ' '
C SEE IF OPTION SPECIFIED
      CALL FLOOK(CLIST,FIRST,LAST,'//',IFORM)
      IF( IFORM.GT.0 )THEN
C YES, PARSE OPTION(S)
         F = IFORM+2
100      CONTINUE
           NUMBER = 5
           CALL FOPTN(CLIST,F,LAST,OPTION,NUMBER,RCODE)
           IF( RCODE.NE.0 )RETURN
           IF( NUMBER.EQ.0 )GOTO 190
           STR8 = OPTION(NUMBER)
           IF( STR8.EQ.'PRINTABL' )THEN
              FORM = STR8
           ELSE IF( STR8.EQ.'NOSORT' )THEN
              SWSORT = .FALSE.
           ELSE IF( STR8.EQ.'BY ' )THEN
C GET BY ROW
              CALL FTOKEN(CLIST,F,LAST,XNAME,NAMELN,CHAR)
              IF( XNAME.EQ.' ' )THEN
                 PRINT *,' ** BY MISSING ROW NAME'
                 GOTO 1300
              ENDIF
              CALL GETNUM('ROW ',XNAME,BYROW)
              IF( BYROW.EQ.0 )THEN
                 PRINT *,' ** ROW ',XNAME(:NAMELN),' NOT FOUND'
                 GOTO 1300
              ENDIF
           ELSE IF( STR8.EQ.'NOLABEL' )THEN
C NO LABEL
              SWLABL = .FALSE.
           ELSE IF( STR8.EQ.'LABEL' )THEN
C LABEL DOMAIN SET ...GET DOMAIN SET
              CALL FTOKEN(CLIST,F,LAST,DOMSET,8,CHAR)
C ...CHECK WE HAVE SYNTAX
              IF( .NOT.SWEXPL )THEN
                 IF( .NOT.SWRULE )
     1              PRINT *,' ** NO SYNTAX IN MEMORY...LABEL IGNORED'
                 GOTO 100
              ENDIF
              IF( DOMSET.EQ.' ' )GOTO 190
C                           :...THIS MEANS WE IGNORE LABEL
C                              (USE ACTIVITY NAME IF SWSORT)
              IF( NENTY.EQ.0 )THEN
                 PRINT *,' ** NO SETS IN SYNTAX'
                 GOTO 1300
              ENDIF
C ...LOOKUP SET
              DO 150 ISET=1,NENTY
                 EPOINT = EXINDX(ISET-1) + 1
                 IF( EXKEY(EPOINT).EQ.DOMSET )GOTO 155
150           CONTINUE
C ...FALL THRU MEANS SET NOT FOUND
              CALL FSLEN(DOMSET,8,L)
              PRINT *,' ** SET ',DOMSET(:L),' NOT FOUND'
              GOTO 1300
155           CONTINUE
C ...WE FOUND SET NUMBER = ISET AT EPOINT
C                          ====    ====== SAVE FOR LOOKING UP MEMBER
           ENDIF
        GOTO 100
190     CONTINUE
C ===== END OF OPTIONS =====
         LAST = IFORM-1
      ENDIF
C GET ROW SPEC
      CALL FTOKEN(CLIST,FIRST,LAST,YNAME,NAMELN,CHAR)
      IF( YNAME.EQ.' ' )THEN
         YNAME = OBJNAM
         ROWNUM= OBJNUM
      ELSE
         CALL GETNUM('ROW ',YNAME,ROWNUM)
         IF( ROWNUM.EQ.0 )THEN
            PRINT *,' ** ROW ',YNAME(:NAMELN),' NOT RECOGNIZED'
            GOTO 1300
         ENDIF
      ENDIF
C =====================================================
C WE NOW HAVE ALL SPECS:
C     ROWNUM= VERTICAL AXIS ROW NUMBER
C     YNAME = VERTICAL AXIS (ROWNUM) NAME
C     BYROW = BY ROW NUMBER (= 0 IF NONE)
C     XNAME = HORIZONTAL AXIS (BYROW) NAME
C     FORM  = GRAPHIC | PRINTABL
C     SWSORT= T IFF PLOT SORTED (BY HEIGHTS)
C     SWLABL= T IFF LABEL STEPS
C     DOMSET NOT NULL IFF USE SET IN SYNTAX FOR LABEL
C =====================================================
      IF( ROWNUM.EQ.BYROW )THEN
         PRINT *,' ** CANNOT HAVE BY ROW = PLOT ROW'
         GOTO 1300
      ENDIF
C INITIALIZE
      NSTEP = 0
      REMAIN = NRCSUB(2)
      SWFAC = .FALSE.
      VYMAX = 0.
      VSTINF= VINF
C
C ::: LOOP OVER SUBMATRIX COLUMNS :::
      DO 500 COL=RCSUB1(2),NCOLS
         CALL GETMAP('COL ',COL,SW)
         IF( .NOT.SW )GOTO 500
C COL IS IN SUBMATRIX
         REMAIN = REMAIN-1
C ...GET COEFFICIENTS
         CALL GETCOL(COL,NZ,MAXLST,ROWLST,VALLST)
         IF( NZ.EQ.0 )GOTO 490
         VBYROW = 0.
         VY = 0.
C ::: LOOP OVER NONZEROES OF COL :::
         DO 400 I=1,NZ
            ROW = ROWLST(I)
            IF( ROW.EQ.ROWNUM )THEN
               VY = VY + VALLST(I)
            ELSE IF( ROW.EQ.BYROW )THEN
               VBYROW = VALLST(I)
            ELSE IF( ROWNUM.EQ.OBJNUM )THEN
               CALL GETMAP('ROW ',ROW,SW)
               IF( SW )THEN
C ADD PRICE FACTOR TO VY
                  CALL GETSOL('ROW ',ROW,VX,VP,CHAR,DUMMY)
                  VY = VY - VALLST(I)*VP
                  SWFAC = .TRUE.
               ENDIF
            ENDIF
400      CONTINUE
C ::: END OF NONZERO LOOP :::
C
C NOW VBYROW = COEF IN BY ROW, VY = PLOT ROW COEF [+ PRICE FACTORS]
         IF( BYROW.EQ.0 )VBYROW = 1.
         IF( VY.LT.-VTOLAB .OR. VBYROW.LE.VTOLAB )GOTO 490
C                                     :...MEANS WIDTH NOT POSITIVE
         CALL GETRIM('COL ',COL,VL,VU,VC,NZ)
C OK, STEP WIDTH = (VU-VL)*VBYROW, HEIGHT = COEF [+ PRICE FACTORS] = VY
C                                           :     :...IF OBJ ROW
C                                           :...OF ROW BEING PLOTTED
         CALL GETNAM('COL ',COL,CNAME)
         VX = (VU-VL)*VBYROW
         VY = VY/VBYROW
         IF( VY.LE.YMIN )VY=0.
         IF( VY.GT.VYMAX )VYMAX = VY
         IF( VU.GE.VINF .OR. VL.LE.-VINF )THEN
            IF( .NOT.SWSORT )THEN
               PRINT *,' ** CANNOT PLOT DUE TO COLUMN WITH INFINITE',
     1                 ' RANGE...NAMELY, ',CNAME
               GOTO 1300
            ENDIF
            IF( VY.LT.VSTINF )THEN
               VSTINF = VY
               SLABEL(LCLNUM) = CNAME
            ENDIF
            GOTO 490
         ENDIF
C COLUMN ENTERS PLOT
         IF( NSTEP.GE.LCLNUM )THEN
            PRINT *,' ** Too many activities (steps)...Max =',LCLNUM
            GOTO 1300
         ENDIF
         NSTEP = NSTEP+1
         XVALUE(NSTEP) = VX
         YVALUE(NSTEP) = VY
         SLABEL(NSTEP) = CNAME
490      CONTINUE
C READY FOR NEXT COL (CHECK IF ALL SUBMATRIX COLS ARE DONE)
         IF( REMAIN.LE.0 )GOTO 510
500   CONTINUE
C  ::: END OF LOOP OVER COLUMNS :::
C
510   CONTINUE
C SEE IF WE HAVE STEP OF INFINITE WIDTH (ALLOWED WITH SORT)
      IF( VSTINF.LT.VYMAX )THEN
         PRINT *,' ** CANNOT PLOT DUE TO COLUMN WITH INFINITE',
     1           ' RANGE...NAMELY, ',SLABEL(LCLNUM)
         GOTO 1300
      ENDIF
      IF( VSTINF.LT.VINF .AND. NSTEP.LT.LCLNUM )THEN
         NSTEP = NSTEP+1
         XVALUE(NSTEP) = VINF
         YVALUE(NSTEP) = VSTINF
         SLABEL(NSTEP) = SLABEL(LCLNUM)
      ENDIF
C SEE IF WE GOT ANY STEPS
      IF( NSTEP.EQ.0 )RETURN
C
      IF( SWSORT .AND. NSTEP.GT.1 )THEN
C SORT STEPS...USE BUBBLE SINCE NSTEP IS SMALL
700      CONTINUE
         SWSORT = .TRUE.
         DO 800 N=1,NSTEP-1
            Y = YVALUE(N)
            IF( YVALUE(N+1).LT.Y )THEN
               YVALUE(N) = YVALUE(N+1)
               YVALUE(N+1)= Y
               Y = XVALUE(N)
               XVALUE(N) = XVALUE(N+1)
               XVALUE(N+1)= Y
               CNAME = SLABEL(N)
               SLABEL(N) = SLABEL(N+1)
               SLABEL(N+1) = CNAME
               SWSORT = .FALSE.
            ENDIF
800      CONTINUE
         IF( .NOT.SWSORT )GOTO 700
      ELSE IF( NSTEP.EQ.1 )THEN
C MOVE LABEL
         SLABEL(1) = SLABEL(LCLNUM)
      ENDIF
      IF( .NOT.SWLABL )THEN
C REMOVE LABELS
         DO 850 N=1,NSTEP
850      SLABEL(N) = ' '
      ELSE IF( DOMSET.NE.' ' )THEN
C REPLACE LABELS WITH SYNTAX, USING DOMAIN SET (DOMSET)
         IF( NCSYN.EQ.0 )GOTO 9000
C SET RANGE TO LOOKUP MEMBER OF DOMSET
         MEMF = EPOINT + 1
         MEML = EXINDX(ISET)
         IF( MEML.LT.MEMF )GOTO 9000
C
         CALL FSLEN(DOMSET,8,LDOM)
         HEADER = '&'//DOMSET(1:LDOM)//':'
C        :...TEMPORARY USE FOR LOOKUP...REAL USE IS BELOW
C
C  ::: LOOP OVER STEPS TO TRANSLATE DOMAIN SET IN COL NAME :::
         DO 1100 N=1,NSTEP
            CNAME = SLABEL(N)
C FIND ACTIVITY CLASS
            DO 1000 I=ICSYN+1,ICSYN+NCSYN
               RNAME = EXKEY(I)
               CALL FMATCH(RNAME,CNAME,' ',SW)
               IF( SW )GOTO 1010
1000        CONTINUE
C ...NO MATCH...COLUMN NOT IN ANY ACTIVITY CLASS
            GOTO 1100
C
1010        CONTINUE
C ...COL IN THIS ACTIVITY CLASS...LOOKUP DOMSET REFERENCE
            FEX = 1
1025        CONTINUE
              CALL FLOOKF(EXTABL(I),FEX,78,HEADER,IAMIT)
              IF( IAMIT.EQ.0 )GOTO 1100
C ...WE GOT DOMSET REFERENCE AT EXTABL(I)(IAMIT:)...GET POSN IN NAME
              F = IAMIT+LDOM+2
              L = F+2
              CALL FTOKEN(EXTABL(I),F,L,STR8,2,CHAR)
C ...STR8 = 1 OR 2 DIGIT NUMBER, GIVING POSITION IN NAME
              CALL FC2I(STR8,8,POSN,RCODE)
              IF( RCODE.NE.0 )THEN
                 RCODE = 0
                 FEX = IAMIT+1
                 GOTO 1025
              ENDIF
C ::: END LOOP TO 1025 THAT LOOKS FOR SET REFERENCE
            STR8 = CNAME(POSN:POSN+LDOM-1)
C ...STR8 = MEMBER NAME...LOOKUP
            DO 1050 MEMBER=MEMF,MEML
               IF( STR8.EQ.EXKEY(MEMBER) )GOTO 1055
1050        CONTINUE
C ...MEMBER NOT FOUND
            GOTO 1100
C
1055        CONTINUE
C WE GOT MEMBER...REPLACE COL NAME WITH MEANING OF DOMSET MEMBER
C           ================================
            SLABEL(N) = EXTABL(MEMBER)(1:16)
C           ================================ (TRUNCATED TO 16 CHARS)
1090        RCODE = 0
1100     CONTINUE
C
      ENDIF
C
9000  CONTINUE
C SET HEADER
      IF( SWFAC )THEN
         HEADER = 'Step heights include price factors'
      ELSE
         HEADER = ' '
      ENDIF
C FINALLY, PLOT STEP FUNCTION
      CALL SCRSTP(FORM,HEADER,XNAME,YNAME,NSTEP,XVALUE,YVALUE,
     1            SLABEL,*1300)
      RETURN
C
C ERROR RETURN
1300  CONTINUE
      RCODE = 1
      RETURN
C
C ** STEPLT ENDS HERE
      END
