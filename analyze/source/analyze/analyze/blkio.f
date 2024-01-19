C                 ::: BLOCKIO.FOR  3-23-92 :::
C
C LAST DATES:  8-16-90 (Version 8.1)...changed 80A1 to 120A1
C
C This contains the following ANALYZE routines.
C
C    BLDISP.....displays block statistics for row/column
C    BLKPIC.....pictures blocks
C    BLKGPH.....graphs blocks
C    BLKIO......saves or loads blocks
C
      SUBROUTINE BLDISP(ROWCOL)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This displays Row or Column blocks (uses FTEXT for output).
C
      CHARACTER*4   ROWCOL
C LOCAL
      CHARACTER*128 CLIST
      CHARACTER*16  RNAME,CNAME
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
      IF(ROWCOL.EQ.'ROW ')THEN
           NUMBER = NRBLKS
           FIRST  = 1
           LAST   = NRBLKS
      ELSE
           NUMBER = NCBLKS
           FIRST  = MAXBLK - NCBLKS + 1
           LAST   = MAXBLK
      ENDIF
C
      RNAME = ' '
      CNAME = ' '
      LINE = 1
      CALL FI2C(RNAME,NUMBER,F)
      CLIST = RNAME(F:)//' '//ROWCOL//'Block'
      CALL FSLEN(CLIST,32,L)
      IF(NUMBER.NE.1)THEN
         L=L+1
         CLIST(L:) = 's'
      ENDIF
      CALL FTEXT(CLIST,L,1,0,LINE,'CLEAR ',*900)
      IF(NUMBER.EQ.0)RETURN
C HEADER
      CLIST = 'Block'
      CLIST(10:) = 'Mask'
      CLIST(10+NAMELN:) = 'Number in Block'
      CALL FSLEN(CLIST,80,LUNDER)
      L = LUNDER
      CALL FTEXT(CLIST,L,1,0,LINE,'CLEAR ',*900)
C UNDERLINE HEADER
      DO 10 K=1,LUNDER
10    CLIST(K:K)='-'
      L = LUNDER
      CALL FTEXT(CLIST,L,1,0,LINE,'CLEAR ',*900)
C
C   ::: LOOP OVER BLOCKS :::
      DO 100 BLOC=FIRST,LAST
         CLIST = BNAME(BLOC)
C REMOVE TRAILING * FROM MASK
         CNAME = BMASK(BLOC)
         CALL FSLEN(CNAME,8,L)
25       IF(CNAME(L:L).EQ.'*'.AND.L.GT.1)THEN
            CNAME(L:)=' '
            L = L-1
            GOTO 25
         ENDIF
         CLIST(10:) = CNAME
         N = BNUMBR(BLOC)
         CALL FI2C(RNAME,N,F)
         CLIST(11+NAMELN:) = RNAME(F:)
         CALL FSLEN(CLIST,80,L)
         CALL FTEXT(CLIST,L,1,0,LINE,'CLEAR ',*900)
100   CONTINUE
C
900   RETURN
C
C ** BLDISP ENDS HERE
      END
      SUBROUTINE BLKPIC(FILOUT,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This pictures blocks (to FILOUT)
C
C LOCAL
      CHARACTER*128 CLIST
      CHARACTER*1   CHAR
      LOGICAL*1     SWRC
C
      LOGICAL*1     SWQUES
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
C CHECK DIMENSIONS
      IF(NRBLKS.GT.BMXROW.OR.NCBLKS.GT.BMXCOL)THEN
           PRINT *,' Too many row or column blocks (limits are',
     1             BMXROW,' by',BMXCOL,')'
           RCODE = 1
           RETURN
      ENDIF
C
      IF(NRBLKS.EQ.0.OR.NCBLKS.EQ.0)THEN
         PRINT *,' No row or column blocks...picture not possible'
         RETURN
      ENDIF
C
C INITIALIZE
C
      DO 10 I=1,NRBLKS
      DO 10 J=1,NCBLKS
10    BMATRX(I,J) = ' '
C
      IF(FILOUT.GT.0)WRITE(FILOUT,1)
1     FORMAT(15X,'BLOCK PICTURE')
      SWQUES = .FALSE.
C
C LOOP OVER COLUMNS TO SET CHARS IN BMATRX
C
      DO 500 J=1,NCOLS
         IF(BINDEX(NROWS+J).EQ.0)GOTO 500
         CBLK = MAXBLK - BINDEX(NROWS+J) + 1
         CALL GETCOL(J,NZ,MAXLST,ROWLST,VALLST)
         IF(NZ.EQ.0)GOTO 500
         SWRC = NZ.GT.MAXLST
         IF(SWRC)NZ = MAXLST
C          ...SWRC notes that we could not interrogate all rows of column J
C
C        ::: LOOP OVER COLUMN'S NONZEROES :::
C
         DO 100 I=1,NZ
              RBLK = BINDEX(ROWLST(I))
              IF(RBLK.EQ.0)GOTO 100
              CHAR = BMATRX(RBLK,CBLK)
              IF(CHAR.EQ.'*')GOTO 100
C
              V = VALLST(I)
              IF(V.LT.0.0)THEN
                 IF(CHAR.EQ.'+')THEN
                        BMATRX(RBLK,CBLK) = '*'
                 ELSE
                        BMATRX(RBLK,CBLK) = '-'
                 ENDIF
              ELSE IF(V.GT.0.0)THEN
                 IF(CHAR.EQ.'-')THEN
                        BMATRX(RBLK,CBLK) = '*'
                 ELSE
                        BMATRX(RBLK,CBLK) = '+'
                 ENDIF
              ENDIF
100      CONTINUE
C
         IF(SWRC)THEN
C     ...Put ? in row blocks that are not * (may have other nonzeroes)
            DO 200 RBLK=1,NRBLKS
200         IF(BMATRX(RBLK,CBLK).NE.'*')BMATRX(RBLK,CBLK)='?'
            SWQUES = .TRUE.
         ENDIF
C
500   CONTINUE
C
      IF(FILOUT.EQ.0)RETURN
C
C    PRINT BLOCK PICTURE (BMATRX)
C
C FIRST, PRINT COLUMN BLOCK NAMES (VERTICALLY)
C
      DO 700 K=1,8
         CLIST = ' '
         SCRCOL = 11
C
         DO 600 BLOC=MAXBLK,MAXBLK-NCBLKS+1,-1
            CLIST(SCRCOL:) = BNAME(BLOC)(K:K)
            SCRCOL = SCRCOL + 2
600      CONTINUE
C
         IF(CLIST.EQ.' ')GOTO 710
         WRITE(FILOUT,2)(CLIST(I:I),I=1,SCRCOL)
2        FORMAT(120A1)
700   CONTINUE
C
710   LINE = K + 1
C
C NOW PRINT BY ROWS
C
      DO 900 RBLK=1,NRBLKS
         CALL FPRMPT(LINE,*999)
         CLIST = ' '//BNAME(RBLK)
         SCRCOL = 11
C
         DO 800 CBLK=1,NCBLKS
            CLIST(SCRCOL:) = BMATRX(RBLK,CBLK)
            SCRCOL = SCRCOL + 2
800      CONTINUE
C
         WRITE(FILOUT,2)(CLIST(I:I),I=1,SCRCOL)
900   CONTINUE
C
      IF(SWQUES)WRITE(FILOUT,901)
901   FORMAT(/' ? means we missed some row of some column in block')
C
999   RETURN
C
C ** BLKPIC ENDS HERE
      END
      SUBROUTINE BLKGPH(FILOUT,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This graphs blocks (to FILOUT).
C
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
      PRINT *,' Sorry, block graph not installed in this version.'
      RCODE = 100
      RETURN
C
C ** BLKGPH ENDS HERE
      END
      SUBROUTINE BLKIO(OPTION,RCODE)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This saves blocks to PCKFIL (UNFORMATTED), presumed open
C ...OPTION = SAVE|LOAD
      CHARACTER*(*)  OPTION
C
C LOCAL
      CHARACTER*128 CLIST
      CHARACTER*64  ERRMSG
      CHARACTER*16  RNAME
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
      ERRMSG = 'BLOCK '//OPTION
      CLIST = ERRMSG
      NRC = NROWS + NCOLS
      IF(OPTION.EQ.'SAVE ')THEN
         IF(NRBLKS.EQ.0 .AND. NCBLKS.EQ.0)THEN
            ERRMSG = 'NO BLOCKS TO SAVE'
            GOTO 13
         ENDIF
C FIRST PUT PROB NAME AND SIZES
         WRITE(PCKFIL,ERR=13)PRBNAM,NROWS,NCOLS,NRBLKS,NCBLKS
         IF(NRBLKS.GT.0)THEN
             ERRMSG = 'WRITING ROW BLOCKS'
             WRITE(PCKFIL,ERR=13)
     1         (BNAME(I),BMASK(I),BNUMBR(I),I=1,NRBLKS)
         ENDIF
         IF(NCBLKS.GT.0)THEN
             ERRMSG = 'WRITING COLUMN BLOCKS'
             WRITE(PCKFIL,ERR=13)
     1         (BNAME(I),BMASK(I),BNUMBR(I),I=MAXBLK-NCBLKS+1,MAXBLK)
         ENDIF
         ERRMSG = 'WRITING INDEXES'
         WRITE(PCKFIL,ERR=13)(BINDEX(I),I=1,NRC)
      ELSE IF(OPTION.EQ.'LOAD ')THEN
C FIRST CHECK PROB NAME AND SIZE
         READ(PCKFIL,END=99,ERR=13)RNAME,NR,NC,NRB,NCB
         IF(RNAME.NE.PRBNAM .OR. NR.NE.NROWS .OR. NC.NE.NCOLS)THEN
            ERRMSG = 'DISCREPENCY...PROBLEM '//RNAME
            GOTO 13
         ENDIF
C CHECK SPACE (MAXBLK MAY BE DIFFERENT IN ANOTHER ENVIRONMENT)
         IF(NRB+NCB.GT.MAXBLK)THEN
            ERRMSG = 'NOT ENOUGH BLOCK SPACE'
            GOTO 13
         ENDIF
C ALL IS WELL SO FAR...SET NUMBERS OF BLOCKS
         NRBLKS=NRB
         NCBLKS=NCB
C FROM THIS POINT ERRORS ARE FATAL AND OLD BLOCKS ARE LOST
         RCODE = 13
         IF(NRBLKS.GT.0)THEN
            ERRMSG = 'READING ROW BLOCKS'
            READ(PCKFIL,END=99,ERR=13)
     1         (BNAME(I),BMASK(I),BNUMBR(I),I=1,NRBLKS)
         ENDIF
         IF(NCBLKS.GT.0)THEN
            ERRMSG = 'READING COLUMN BLOCKS'
            READ(PCKFIL,END=99,ERR=13)
     1        (BNAME(I),BMASK(I),BNUMBR(I),I=MAXBLK-NCBLKS+1,MAXBLK)
         ENDIF
         ERRMSG = 'READING INDEXES'
         READ(PCKFIL,END=99,ERR=13)(BINDEX(I),I=1,NRC)
      ELSE
         ERRMSG = ' ** SYSERR IN BLKIO...'//OPTION
         GOTO 13
      ENDIF
C
C NORMAL RETURN
      RCODE = 0
      IF(SWMSG)PRINT *,' BLOCK ',OPTION(:4),' COMPLETE'
      CLOSE(PCKFIL)
      RETURN
C
C  ERROR RETURNS
99    CLIST = 'EOF REACHED '//ERRMSG
      ERRMSG = CLIST
13    PRINT *,' BLOCK IO ERROR...'//ERRMSG
      IF(RCODE.EQ.13)CALL BLINIT
131   RCODE = 1
      PRINT *,' ** ',OPTION,' aborted'
      CLOSE(PCKFIL)
      RETURN
C
C ** BLKIO ENDS HERE
      END
