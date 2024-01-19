C            ::: BLKFIND.FOR  3-27-92 :::
C
C LAST DATES:  4-21-90 (in Version 8.1)
C
C This file contains the following subroutines of BLOCK
C support for ANALYZE.
C
C    BLOCK.......blocks by components
C    BLKSYN......blocks by name syntax
C    BLKNET......blocks by netform
C
C   ............................................
C   :             ** NOTE **                   :
C   :   All blocking operates on submatrix.    :
C   : A row/col can belong to at most 1 block. :
C   :..........................................:
C
C
      SUBROUTINE BLOCK(RCODE)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This blocks by components.
C ...blocks are named COMP.001,COMP.002,...
C
C LOCAL
      CHARACTER*128 CLIST
      CHARACTER*16  RNAME,CNAME
      LOGICAL*1     SW,SWRC
C :::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      IF(NROWS.GT.SCRIND(0))THEN
         PRINT *,' Sorry, not enough scratch space to support',
     1           ' component BLOCK...need ',NROWS
         RCODE = 1
         RETURN
      ENDIF
      IF(NRCSUB(1).EQ.0.OR.NRCSUB(2).EQ.0)THEN
         PRINT *,' No rows and/or columns in submatrix'
         RETURN
      ENDIF
      MAXCMP = (MAXBLK - NRBLKS - NCBLKS)/2
      IF(MAXCMP.LT.2)THEN
         PRINT *,' Cannot block due to max block limit...',MAXBLK
         PRINT *,' ...Erase or merge some blocks'
         RETURN
      ENDIF
C
C PUT EACH ROW INTO ITS OWN COMPONENT...INITIALIZE CIRCULAR LISTS
C
      NCOMP = 0
      NUMBER = 0
C
      DO 100 I=1,NROWS
         IF(BINDEX(I).GT.0)GOTO 80
C   ROW I IS NOT IN ANY BLOCK...SEE IF IN SUBMATRIX
         CALL GETMAP('ROW ',I,SW)
         IF(.NOT.SW)GOTO 80
C   IGNORE IF FREE ROW (DO NOT BLOCK)
         CALL GETBND('ROW ',I,VL,VU)
         IF(VL.GT.-VINF .OR. VU.LT.VINF)GOTO 90
            NUMBER = NUMBER + 1
C   ROW I IS NOT TO BE BLOCKED (ALREADY IN BLOCK OR NOT IN SUBMATRIX)
80       SCRIND(I) = 0
         GOTO 100
C
C   ROW I IS TO BE BLOCKED...INITIALIZE IT TO BE ITS OWN COMPONENT
90       CONTINUE
         BINDEX(I) = -I
         NCOMP = NCOMP + 1
         SCRIND(I) = NCOMP
100   CONTINUE
C
      IF(NUMBER.GT.0.AND.SWMSG)PRINT *,NUMBER,' free row(s) ignored'
C
      IF(NCOMP.EQ.0)THEN
         PRINT *,' All rows in submatrix are already in some block'
         RETURN
      ENDIF
C
C    ::: LOOP OVER COLUMNS TO MERGE COMPONENTS
C
      CNAME = ' '
      SWRC = .FALSE.
C
      NRC = NRCSUB(2)
C
      DO 500 J=RCSUB1(2),NCOLS
         IF(NRC.EQ.0)GOTO 510
         CALL GETMAP('COL ',J,SW)
         IF(.NOT.SW)GOTO 500
         NRC = NRC - 1
         IF(BINDEX(NROWS+J).GT.0)GOTO 500
C
C   COLUMN J IS IN SUBMATRIX AND NOT IN ANOTHER BLOCK...GET ITS NONZEROES
         CALL GETCOL(J,NZ,MAXLST,ROWLST,VALLST)
         IF(NZ.EQ.0)GOTO 500
         IF(NZ.GT.MAXLST)THEN
            CALL GETNAM('COL ',J,CNAME)
            NZ = MAXLST
         ENDIF
C
         MINCMP = NROWS + 1
C
C    LOOP OVER NONZEROES
C
         DO 300 I=1,NZ
            ROW = ROWLST(I)
            IF(SCRIND(ROW).EQ.0)GOTO 300
            IF(SCRIND(ROW).LT.MINCMP)THEN
               MINROW = ROW
               MINCMP = SCRIND(MINROW)
            ENDIF
300      CONTINUE
C
         IF(MINCMP.GT.NROWS)GOTO 500
         SWRC = .TRUE.
C
C    COLUMN J CAUSES ITS ROW CLIQUE TO MERGE INTO 1 COMPONENT (MINCMP)
C
         DO 400 I=1,NZ
            ROW = ROWLST(I)
            IF(SCRIND(ROW).LE.MINCMP)GOTO 400
C    MERGE COMPONENT OF ROW WITH THAT OF MINROW (MINCMP)
C
C   FIRST, TRAVERSE ROW'S COMPONENT TO SET COMPONENT NUMBERS
            L = ROW
350         CONTINUE
              SCRIND(L) = MINCMP
              L = -BINDEX(L)
            IF(L.NE.ROW)GOTO 350
C   NOW MERGE CIRCULAR LIST OF ROW WITH THAT OF MINROW
            LTEMP          = BINDEX(MINROW)
            BINDEX(MINROW) = BINDEX(ROW)
            BINDEX(ROW)    = LTEMP
C   FINALLY, DECREMENT NUMBER OF COMPONENTS
            NCOMP = NCOMP - 1
400      CONTINUE
C
500   CONTINUE
C
510   CONTINUE
      IF(.NOT.SWRC)THEN
         PRINT *,' All columns in submatrix are in some block, so'
         PRINT *,' each row is in its own component...no blocks formed'
         GOTO 1900
      ENDIF
      IF(CNAME.NE.' ')THEN
         CLIST = ' WARNING: Blocks may be not be disjoint because'
     1           //' column '//CNAME
         CALL FSLEN(CLIST,80,LAST)
         CALL FTEXT(CLIST,LAST,2,0,LINE,'KEEP ',*1900)
         CLIST(LAST+2:) = '(perhaps others) could not be fully'
     1          //' interrogated due to space limit.'
         CALL FSLEN(CLIST,128,LAST)
         CALL FTEXT(CLIST,LAST,2,0,LINE,'EJECT ',*1900)
      ENDIF
C
C NOW ROWS IN SAME COMPONENT ARE IN CIRCULAR LIST
C
      IF(SWMSG)PRINT *,NCOMP,' components found'
      IF(NCOMP.GT.MAXCMP)THEN
         NCOMP = MAXCMP
         IF(SWMSG)PRINT *,' ...only first ',NCOMP,' formed'
      ENDIF
C
C FORM BLOCKS
C
      LAST = RCSUB1(1)
1000  CONTINUE
      DO 1290 COMP=1,NCOMP
C SET NAME OF NEW COMPONENT
        CNAME(6:) = '000'
        CALL FI2C(CNAME,COMP,FIRST)
        RNAME = 'COMP.'//CNAME(6:)
        CALL BLKNEW('ROW ',RNAME,BLOC,RCODE)
        CALL BLKNEW('COL ',RNAME,CBLOC,RCODE)
        IF(RCODE.NE.0)THEN
           PRINT *,' ** SYSERR FROM BLNEW:',COMP,MAXCMP
           GOTO 1299
        ENDIF
C
C    LOOK FOR NEXT COMPONENT (ROW WITH SCRIND > 0)
C
        DO 1100 ROW=LAST,NROWS
1100    IF(SCRIND(ROW).GT.0)GOTO 1200
        PRINT *,' ** SYSERR 1100 IN BLOCK:',NCOMP
C
1200    CONTINUE
C      TRAVERSE ROWS IN THIS COMPONENT
        L = ROW
1210    CONTINUE
        LNEXT = -BINDEX(L)
C        ADD ROW (L) TO BLOCK
           SCRIND(L) = -CBLOC
           BNUMBR(BLOC) = BNUMBR(BLOC) + 1
           BINDEX(L) = BLOC
           CALL GETNAM('ROW ',L,RNAME)
           CALL BMSKIN(BMASK(BLOC),RNAME)
           IF(LNEXT.NE.ROW)THEN
              L = LNEXT
              GOTO 1210
           ENDIF
C       TRAVERSAL COMPLETE
C
C RESUME SEARCH FOR NEXT COMPONENT
         LAST = ROW + 1
1290  CONTINUE
1299  CONTINUE
C     .....may have come here after truncating number of components
C
C NOW PUT COLUMNS INTO BLOCKS
C
      NRC = NRCSUB(2)
C
      DO 1500 J=RCSUB1(2),NCOLS
         IF(NRC.EQ.0)GOTO 1510
         CALL GETMAP('COL ',J,SW)
         IF(.NOT.SW)GOTO 1500
         NRC = NRC - 1
         IF(BINDEX(NROWS+J).GT.0)GOTO 1500
C
C  COLUMN J IS IN SUBMATRIX AND NOT IN ANOTHER BLOCK...GET ITS NONZEROES
         CALL GETCOL(J,NZ,MAXLST,ROWLST,VALLST)
         IF(NZ.EQ.0)GOTO 1500
         IF(NZ.GT.MAXLST)NZ = MAXLST
C
C     LOOP OVER NONZEROES
         DO 1400 I=1,NZ
            ROW = ROWLST(I)
            IF(SCRIND(ROW).LT.0)GOTO 1410
1400     CONTINUE
         GOTO 1500
C
1410     CONTINUE
C  COLUMN J IS IN
                  CBLOCK = -SCRIND(ROW)
C ...INCREMENT NUMBER
         BNUMBR(CBLOCK) = BNUMBR(CBLOCK) + 1
C ...SET ITS BLOCK MEMBERSHIP INDEX
         BINDEX(NROWS + J) = CBLOCK
C ...UPDATE NAME MASK
         CALL GETNAM('COL ',J,RNAME)
         CALL BMSKIN(BMASK(CBLOCK),RNAME)
C NEXT COLUMN
1500  CONTINUE
C
1510  CONTINUE
C
C     ERASE NULL COLUMN COMPONENT BLOCKS
C
      FIRST = MAXBLK-NCBLKS+1
      LAST = MAXBLK-NCBLKS+NCOMP
C NOTE:  WE INTERROGATE BLOCKS SUCH THAT THE ERASE WORKS PROPERLY
C        EVEN THOUGH NCBLKS IS DECREMENTED IF ANY ARE DELETED.
      DO 1600 BLOC=FIRST,LAST
         IF(BNUMBR(BLOC).EQ.0)THEN
            SW = SWMSG
            SWMSG = .FALSE.
            CALL BERASE('COL ',BLOC)
            SWMSG = SW
         ENDIF
1600  CONTINUE
C
C   ::: DONE :::
C
1900  CONTINUE
C RESTORE BINDEX = 0 FOR UNFORMED BLOCKS
      DO 1990 I=LAST,NROWS
1990  IF(BINDEX(I).LT.0)BINDEX(I)=0
C
      RETURN
C
C ** BLOCK ENDS HERE
      END
      SUBROUTINE BLKSYN(ROWCOL,RNAME,FILL,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This blocks rows/cols in submatrix by name syntax
C ...RNAME = ?_mask
C
C Blocks are named SYN.1st 4 chars in ?_mask
C ...Method is same as TALLY
C
      CHARACTER*4   ROWCOL
      CHARACTER*16  RNAME
      CHARACTER*1   FILL
C LOCAL
      CHARACTER*16  CNAME
      CHARACTER*1   QCHAR
      LOGICAL*1     SW,SWRC,QMAP(16)
      DATA        QCHAR/'?'/
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::
      IF(SWDBG)THEN
         PRINT *,' ENTERED BLKSYN WITH ',ROWCOL,RNAME
         PAUSE = PAUSE-1
         IF(PAUSE.LE.0)CALL SYSDBG
      ENDIF
      MAXCLS = MAXBLK - NRBLKS - NCBLKS
      IF(MAXCLS.EQ.0)THEN
         PRINT *,' Sorry, no room for more blocks'
         RCODE = 1
         RETURN
      ENDIF
C Set Q_mask
      BEGQRY = 0
      DO 10 K=1,NAMELN
        IF(RNAME(K:K).EQ.QCHAR)THEN
           QMAP(K) = .TRUE.
           RNAME(K:K) = '*'
           IF(BEGQRY.EQ.0)BEGQRY = K
           ENDQRY = K
        ELSE
           QMAP(K) = .FALSE.
        ENDIF
10    CONTINUE
      IF(BEGQRY.EQ.0)THEN
         PRINT *,' ** MISSING QUERY CHARACTER (',QCHAR,')'
         RCODE = -1
         RETURN
      ENDIF
C
      IF(ROWCOL.EQ.'ROW ')THEN
         FIRST = RCSUB1(1)
         LAST  = NROWS
         REMAIN= NRCSUB(1)
         BZERO = 0
         CLASS = NRBLKS
         STEP  = 1
      ELSE
         FIRST = RCSUB1(2)
         LAST  = NCOLS
         REMAIN= NRCSUB(2)
         BZERO = NROWS
         CLASS = MAXBLK + 1
         STEP  = -1
      ENDIF
C
      NCLASS = 0
      SWRC = .FALSE.
C
      DO 500 NUMBER=FIRST,LAST
         CALL GETMAP(ROWCOL,NUMBER,SW)
         IF(.NOT.SW)GOTO 500
C ROWCOL NUMBER IS IN SUBMATRIX
         REMAIN = REMAIN-1
         IF(BINDEX(BZERO+NUMBER).GT.0)GOTO 490
C GET NAME (CNAME) OF ROWCOL NUMBER
         CALL GETNAM(ROWCOL,NUMBER,CNAME)
         IF(FILL.NE.' ')THEN
C FILL TRAILING BLANKS
            CALL FSLEN(CNAME,NAMELN,L)
            IF(L.LT.NAMELN)THEN
               DO 150 K=L+1,NAMELN
150            CNAME(K:K) = FILL
            ENDIF
         ENDIF
C
C DOES CNAME MATCH QUERY MASK?
         CALL FMATCH(RNAME,CNAME,'*',SW)
         IF(.NOT.SW)GOTO 490
C -YES...WE NOW HAVE ROWCOL CNAME IN SUBMATRIX, NOT IN ANOTHER BLOCK,
C        WHICH MATCHES QUERY MASK (RNAME)
C
          IF(NCLASS.GT.0)THEN
C ::: LOOP OVER CLASSES TO SEE IF ROW|COL IS ALREADY IN ONE
             DO 300 C=CLASS+STEP,CLASS+STEP*NCLASS,STEP
                CALL FMATCH(BMASK(C),CNAME,'*',SW)
                IF(SW)THEN
                   BNUMBR(C) = BNUMBR(C) + 1
                   BINDEX(BZERO+NUMBER) = C
                   GOTO 490
                ENDIF
300          CONTINUE
          ENDIF
C
C     NEW CLASS
           IF(NCLASS.EQ.MAXCLS)THEN
              SWRC = .TRUE.
              GOTO 490
           ENDIF
           NCLASS = NCLASS + 1
           DO 400 K=1,NAMELN
400        IF(.NOT.QMAP(K))CNAME(K:K)=RNAME(K:K)
C NOW CNAME = INITIAL MASK FOR CLASS
C
           BLOC = CLASS + STEP*NCLASS
           BMASK(BLOC) = CNAME
C (BLOCK NAMED BELOW)
C LINK TO LIST (BECOMES BLOCK 1)
           BNUMBR(BLOC) = 1
           BINDEX(BZERO+NUMBER) = BLOC
490      CONTINUE
         IF(REMAIN.EQ.0)GOTO 510
C READY FOR NEXT ROWCOL
500   CONTINUE
C  ::: END LOOP OVER ROWCOL :::
510   CONTINUE
C
C NAME SYNTAX BLOCKS BY THEIR CLASS (PREFIXED BY 'SYN.' IF ROOM)
C       mask = BLOCK MASK WITH MULTIPLE * REPLACED BY ~
C
      DO 800 C=1,NCLASS
         BLOC = CLASS + STEP*C
         CNAME = BMASK(BLOC)
C REMOVE TRAILING FILL CHARS
         IF(FILL.NE.' ')THEN
            CALL FLOOKF(CNAME,1,16,FILL,I)
            IF(I.GT.0)THEN
               CNAME(I:) = ' '
               BMASK(BLOC) = CNAME
            ENDIF
         ENDIF
         IF(ENDQRY-BEGQRY.LE.3)THEN
C NUMBER OF QUERY CHARS <= 4, SO PREFIX BLOCK NAME
            BNAME(BLOC) = 'SYN.'//CNAME(BEGQRY:ENDQRY)
         ELSE
C NUMBER OF QUERY CHARS > 4, SO JUST USE CLASS NAME
            BNAME(BLOC) = CNAME(BEGQRY:ENDQRY)
         ENDIF
800   CONTINUE
C
      IF(ROWCOL.EQ.'ROW ')THEN
         NRBLKS = NRBLKS + NCLASS
      ELSE
         NCBLKS = NCBLKS + NCLASS
      ENDIF
C
      IF(SWMSG)PRINT *,NCLASS,' block(s) added'
      IF(SWRC)
     1  PRINT *,' Sorry, not enough room to obtain all syntax blocks'
C
      RETURN
C
C ** BLKSYN ENDS HERE
      END
      SUBROUTINE BLKNET(RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This blocks submatrix by netform...
C
C      .......columns in netform put into blocks:
C      :       NET.PROD  + only, NET.CONS  - only,  NET.TRAN  -+
C      :
C      N *
C      0 *
C
C LOCAL
      CHARACTER*16  RNAME,CNAME
      LOGICAL*1     SW,SWPOS,SWNEG
C ::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
      IF(NRBLKS+NCBLKS+3.GE.MAXBLK)THEN
           PRINT *,' Sorry, not enough room to add NET blocks'
           RETURN
      ENDIF
C
C ADD NET BLOCKS (PROD, CONS and TRAN)
C
      RNAME = 'NET.PROD'
      CALL BLKNEW('COL ',RNAME,PBLOCK,RCODE)
         IF(RCODE.NE.0)RETURN
      RNAME = 'NET.CONS'
      CALL BLKNEW('COL ',RNAME,CBLOCK,RCODE)
         IF(RCODE.NE.0)RETURN
      RNAME = 'NET.TRAN'
      CALL BLKNEW('COL ',RNAME,TBLOCK,RCODE)
         IF(RCODE.NE.0)RETURN
C
C OK, PREPARE TO ADD COLUMNS TO BLOCKS
      NRC   = NRCSUB(2)
C
C   LOOP OVER COLUMNS IN SUBMATRIX
C
      DO 500 COL=RCSUB1(2),NCOLS
          IF(NRC.EQ.0)GOTO 510
          CALL GETMAP('COL ',COL,SW)
          IF(.NOT.SW)GOTO 500
          NRC = NRC - 1
C
          IF(BINDEX(NROWS + COL).GT.0)GOTO 500
C  COL IS IN SUBMATRIX AND NOT IN ANOTHER BLOCK
C  ...GET ITS NONZEROES
          CALL GETCOL(COL,NZ,MAXLST,ROWLST,VALLST)
          IF(NZ.GT.MAXLST.OR.NZ.EQ.0)GOTO 500
C
C   Loop over rows of COL
C
          SWPOS = .FALSE.
          SWNEG = .FALSE.
C
          DO 300 I=1,NZ
             CALL GETMAP('ROW ',ROWLST(I),SW)
             IF(.NOT.SW)GOTO 300
             CALL GETBND('ROW ',ROWLST(I),VL,VU)
             IF(VL.LE.-VINF)THEN
                 IF(VU.GE.VINF)GOTO 300
C       L-ROW...REVERSE SIGN OF NONZERO
                 VALLST(I) = -VALLST(I)
              ENDIF
C
             IF(VALLST(I).LT.0.0)THEN
                IF(SWNEG)GOTO 500
                SWNEG = .TRUE.
             ELSE IF(VALLST(I).GT.0.0)THEN
                IF(SWPOS)GOTO 500
                SWPOS = .TRUE.
             ENDIF
300       CONTINUE
C
C    Classify netform activity
C
          IF(.NOT.SWPOS)THEN
               IF(.NOT.SWNEG)GOTO 500
C     CONSUMPTION
               BLOC = CBLOCK
          ELSE IF(.NOT.SWNEG)THEN
C     PRODUCTION
               BLOC = PBLOCK
          ELSE
C     TRANSFER
               BLOC = TBLOCK
          ENDIF
C  ...ADD TO BLOC
          BINDEX(NROWS + COL) = BLOC
          BNUMBR(BLOC) = BNUMBR(BLOC) + 1
          CALL GETNAM('COL ',COL,CNAME)
          CALL BMSKIN(BMASK(BLOC),CNAME)
C
500   CONTINUE
C
510   CONTINUE
C
C ERASE NULL BLOCKS THAT WERE DEFINED, BUT WE MUST DO THIS
C IN THE REVERSE ORDER OF DECLARATION, SO BERASE WORKS PROPERLY.
C (OTHERWISE, BLOCK NUMBERS GET REDEFINED.)
      IF(BNUMBR(TBLOCK).EQ.0)THEN
         SW = SWMSG
         SWMSG = .FALSE.
         CALL BERASE('COL ',TBLOCK)
         SWMSG = SW
         IF(SWMSG)PRINT *,' NO TRANSFER BLOCK'
      ELSE
         IF(SWMSG)PRINT *,' ',BNAME(TBLOCK)(:8),' has ',
     1                    BNUMBR(TBLOCK),' activities'
      ENDIF
C ...NOW CBLOCK AND PBLOCK ARE STILL THE SAME NUMBERS,
C    EVEN IF TBLOCK WAS ERASED
      IF(BNUMBR(CBLOCK).EQ.0)THEN
         SW = SWMSG
         SWMSG = .FALSE.
         CALL BERASE('COL ',CBLOCK)
         SWMSG = SW
         IF(SWMSG)PRINT *,' NO CONSUMPTION BLOCK'
      ELSE
         IF(SWMSG)PRINT *,' ',BNAME(CBLOCK)(:8),' has ',
     1                    BNUMBR(CBLOCK),' activities'
      ENDIF
      IF(BNUMBR(PBLOCK).EQ.0)THEN
         SW = SWMSG
         SWMSG = .FALSE.
         CALL BERASE('COL ',PBLOCK)
         SWMSG = SW
         IF(SWMSG)PRINT *,' NO PRODUCTION BLOCK'
      ELSE
         IF(SWMSG)PRINT *,' ',BNAME(PBLOCK)(:8),' has ',
     1                    BNUMBR(PBLOCK),' activities'
      ENDIF
C
      RETURN
C
C ** BLKNET ENDS HERE
      END
