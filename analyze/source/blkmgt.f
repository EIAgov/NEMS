C            ::: BLKMGT.FOR  9-13-92 :::
C
C LAST DATE:  8-09-90...REMOVED CALLING SYSDBG FROM BDEBUG
C            12-21-90...REMOVED ERROR CONDITION WHEN COPYING
C                       FROM A NON-EXISTENT BLOCK (BLCOPY)
C             3-08-92...VERSION 8.1
C             3-23-92...CHANGED BLOCK SYNTAX AS TALLY AND CHANGED
C                       SYNTAX BLOCK NAMES TO USE REDUCED MASK
C             3-26-92...USED FGTCHR, AS PER DAVE HELTNE'S STUFF
C             5-18-92...Replaced OPEN with FLOPEN
C             6-05-92...Fixed CMS bug by removing 520 and GOTO 510
C             6-05-92...Fixed bug with PLOT option (statement 170)
C             7-03-92...Removed DOS/NONDOS Autoedit
C
C This file contains the following subroutines that comprise block
C support for ANALYZE.
C
C    ABLOCK.....parses BLOCK command
C    BLINIT.....initializes block data
C             (MUST BE CALLED BEFORE OTHER BLOCK ROUTINES)
C    BLMERG.....merges blocks
C    BLCOPY.....copies TO or FROM submatrix
C    BERASE.....erases block(s)
C    BLKCLR.....clears null blocks
C    BLKREN.....renames block
C    BLKNEW.....creates new block
C    BLKADD.....adds a row/col to block
C    BLOC8......gives location of block from name
C    BMSKIN.....intersects mask with name
C    BDEBUG.....debug facility (called by SYSDBG)
C
      SUBROUTINE ABLOCK(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCANAL
CI$$INSERT DCFLIP
C
C This parses CLIST(FIRST:LAST) ...
C
C BLOCK [ROW|COL |SYNTAX [?_mask]
C                |MERGE  block1 block2 [new_name]
C                |ERASE  block [...] | *
C                |COPY   TO|FROM block
C                |RENAME block new_name
C                |DISPLAY
C       |PICTURE  [IO]
C       |SAVE     [filename]
C       |LOAD     [filename]
C       |NETFORM
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*64  FILNAM
      CHARACTER*8   OPTION(7)
      CHARACTER*16  RNAME,CNAME
      CHARACTER*4   ROWCOL
      CHARACTER*1   CHAR,QCHAR,FILL,FILCHR
      LOGICAL       EXIST
      DATA        FILCHR /'~'/, QCHAR /'?'/
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
      IF(MAXBLK.EQ.0)THEN
         PRINT *,' Sorry, not enough space to support BLOCK command'
         RCODE = 1
         RETURN
      ENDIF
C LOOK FOR SPECIAL OPTIONS (FOLLOWING //)
      CALL FLOOK(CLIST,FIRST,LAST,'//',IOPTN)
      IF(IOPTN.GT.0)THEN
         LOPTN = LAST
         LAST = IOPTN-1
      ENDIF
C
C GET OPTION
C
      OPTION(1) = 'ROW '
      OPTION(2) = 'COL '
      OPTION(3) = 'PICTURE'
      OPTION(4) = 'SAVE'
      OPTION(5) = 'LOAD'
      OPTION(6) = 'NETFORM'
      OPTION(7) = 'GRAPH'
C
      RCODE = 0
      NUMBER = 7
C
      CALL FOPTN(CLIST,FIRST,LAST,OPTION,NUMBER,RCODE)
      IF(RCODE.NE.0)RETURN
      IF(NUMBER.EQ.0)THEN
          CALL BLOCK(RCODE)
          RETURN
      ENDIF
C
C    BRANCH ON OPTION (NUMBER)
C    ------------------------------------
      GOTO(100,100,300,500,500,600,700),NUMBER
C    ------------------------------------
C
100   ROWCOL = OPTION(NUMBER)
C
C ROW | COL ...get next spec
C
      OPTION(1) = 'SYNTAX'
      OPTION(2) = 'MERGE'
      OPTION(3) = 'ERASE'
      OPTION(4) = 'COPY'
      OPTION(5) = 'RENAME'
      OPTION(6) = 'DISPLAY'
      OPTION(7) = 'PLOT '
      RCODE = -1
      NUMBER = 7
      CALL FOPTN(CLIST,FIRST,LAST,OPTION,NUMBER,RCODE)
      IF(RCODE.NE.0)RETURN
C
      GOTO(110,120,130,140,150,160,170),NUMBER
C     ------------------------------------
110   CONTINUE
C
C  SYNTAX [?_mask] [//FILL char]
C GET QUERY MASK
      CALL FTOKEN(CLIST,FIRST,LAST,RNAME,NAMELN,CHAR)
      IF(RNAME.EQ.' ')RNAME = QCHAR
C SET DEFAULT FILL CHAR
      FILL = FILCHR
      IF(IOPTN.GT.0)THEN
C USER SPECIFIED A FILL CHAR (MAYBE BLANK, WHICH MEANS NO FILL)
         OPTION(1) = 'FILL'
         NUMBER = 1
         FIRST = IOPTN+2
         RCODE = -1
         CALL FOPTN(CLIST,FIRST,LOPTN,OPTION,NUMBER,RCODE)
         IF(RCODE.NE.0)RETURN
         CALL FTOKEN(CLIST,FIRST,LOPTN,FILL,1,CHAR)
      ENDIF
      CALL BLKSYN(ROWCOL,RNAME,FILL,RCODE)
      RETURN
C
120   CONTINUE
C
C  MERGE block1 block2 [new_name]
C GET BLOCK1
      CALL FTOKEN(CLIST,FIRST,LAST,RNAME,8,CHAR)
C      Lookup block numbers (RCODE = -1 makes mandatory)
      RCODE = -1
      CALL BLOC8(ROWCOL,RNAME,BLOCK1,RCODE)
      IF(RCODE.NE.0)RETURN
      CALL FTOKEN(CLIST,FIRST,LAST,RNAME,8,CHAR)
      RCODE = -1
      CALL BLOC8(ROWCOL,RNAME,BLOCK2,RCODE)
      IF(RCODE.NE.0)RETURN
C
      IF(BLOCK1.EQ.BLOCK2)THEN
          PRINT *,' Cannot merge block into itself'
          RCODE = -1
          RETURN
      ENDIF
C      See if new name was specified
      CALL FTOKEN(CLIST,FIRST,LAST,RNAME,8,CHAR)
      IF(RNAME.NE.' ')THEN
          CALL BLKREN(ROWCOL,BLOCK1,RNAME,RCODE)
          IF(RCODE.NE.0)RETURN
      ENDIF
C       Ok, perform merge (BLOCK1 has already been renamed)
      CALL BLMERG(ROWCOL,BLOCK1,BLOCK2)
      RETURN
C
130   CONTINUE
C
C     ERASE block [...] | *
C GET BLOCK
      CALL FTOKEN(CLIST,FIRST,LAST,RNAME,8,CHAR)
      IF(RNAME.EQ.'*')THEN
         BLOC = 0
      ELSE IF(RNAME.EQ.' ')THEN
         RETURN
      ELSE
         RCODE = -1
         CALL BLOC8(ROWCOL,RNAME,BLOC,RCODE)
         IF(RCODE.NE.0)RETURN
      ENDIF
      CALL BERASE(ROWCOL,BLOC)
      GOTO 130
C
140   CONTINUE
C
C  COPY TO|FROM block
C
      OPTION(1) = 'TO'
      OPTION(2) = 'FROM'
      NUMBER = 2
      RCODE = -1
      CALL FOPTN(CLIST,FIRST,LAST,OPTION,NUMBER,RCODE)
      IF(RCODE.NE.0)RETURN
C      Get block name (CNAME)
      CALL FTOKEN(CLIST,FIRST,LAST,CNAME,8,CHAR)
      IF(CNAME.EQ.' ')THEN
          PRINT *,' Missing block name to COPY ',OPTION(NUMBER)
          RCODE = -1
          RETURN
      ENDIF
C
      RNAME = OPTION(NUMBER)
      CALL BLCOPY(ROWCOL,RNAME,CNAME,RCODE)
      RETURN
C
150   CONTINUE
C
C  RENAME block new_name
C GET NEW NAME
      CALL FTOKEN(CLIST,FIRST,LAST,RNAME,8,CHAR)
      IF(RNAME.EQ.' ')THEN
          PRINT *,' Missing block name (and new name)'
          RCODE = -1
          RETURN
      ENDIF
C      Lookup block
      RCODE = -1
      CALL BLOC8(ROWCOL,RNAME,BLOCK1,RCODE)
      IF(RCODE.NE.0)RETURN
C      Get new name
      CALL FTOKEN(CLIST,FIRST,LAST,CNAME,8,CHAR)
      IF(CNAME.EQ.' ')THEN
           PRINT *,' Missing new name of ',RNAME
           RCODE = -1
           RETURN
      ENDIF
C
      CALL BLKREN(ROWCOL,BLOCK1,CNAME,RCODE)
      RETURN
C
160   CONTINUE
C
C  DISPLAY
      CALL BLDISP(ROWCOL)
      RETURN
C
170   CONTINUE
C PLOT [header] [//option]
C PUT OPTION BACK INTO CLIST FOR BLKPLT TO PARSE
      IF( IOPTN.GT.0 )LAST = LOPTN
      CALL BLKPLT(ROWCOL,CLIST,FIRST,LAST,RCODE)
      RETURN
C
C ::: END BLOCK ROW|COL (first branch on option NUMBER) :::
C
300   CONTINUE
C
C  PICTURE
      CALL BLKPIC(OUTPUT,RCODE)
      RETURN
C
500   CONTINUE
C
C SAVE|LOAD [filename]
C
      CALL FTOKEN(CLIST,FIRST,LAST,FILNAM,NAMLEN,CHAR)
      IF(FILNAM.EQ.' ')FILNAM = PRBNAM
      CALL FSETNM('BLOCK   ',FILNAM,BEGEXT,RCODE)
      IF(RCODE.NE.0)RETURN
C
      IF(OPTION(NUMBER).EQ.'LOAD')THEN
         CALL FINQUR(FILNAM,IOSTAT,EXIST,*513)
         IF(.NOT.EXIST)GOTO 5131
      ENDIF
      CALL FLOPEN(PCKFIL,FILNAM,'UNFORMATTED','UNKNOWN',*5139)
      CALL BLKIO(OPTION(NUMBER),RCODE)
      RETURN
C ERRORS FROM SAVE|LOAD
513   CONTINUE
      PRINT *,' ** IO ERROR INQUIRING ABOUT ',FILNAM
      GOTO 5139
5131  CONTINUE
      CALL FSLEN(FILNAM,64,L)
      PRINT *,' ** ',FILNAM(:L),' DOES NOT EXIST'
5139  CLOSE(PCKFIL)
      RETURN
C
600   CONTINUE
C  NETFORM
      CALL BLKNET(RCODE)
      RETURN
C
700   CONTINUE
C GRAPH
      CALL BLKGPH(OUTPUT,RCODE)
      RETURN
C
C ** ABLOCK ENDS HERE
      END
      SUBROUTINE BLINIT
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This initializes data for BLOCK module of ANALYZE
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
      VERBLK = 'N.26'
      MXBIND = PMXBIN
C
      NRC = NROWS + NCOLS
      IF(NRC.GT.MXBIND)THEN
         MAXBLK = 0
         RETURN
      ENDIF
      MAXBLK = PMXBLK
      NRBLKS = 0
      NCBLKS = 0
      DO 10 I=1,NRC
10    BINDEX(I) = 0
C
      RETURN
C
C ** BLINIT ENDS HERE
      END
      SUBROUTINE BLMERG(ROWCOL,BLOCK1,BLOCK2)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This merges BLOCK2 into BLOCK1
C
      CHARACTER*(*)  ROWCOL
C LOCAL
      CHARACTER*16  RNAME,CNAME
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
      IF(ROWCOL.EQ.'ROW ')THEN
         BZERO = 0
         NUMBER= NROWS
      ELSE
         BZERO = NROWS
         NUMBER= NCOLS
      ENDIF
C
      CNAME = BMASK(BLOCK1)
C
      DO 100 I=1,NUMBER
         IF(BINDEX(BZERO+I).NE.BLOCK2)GOTO 100
         CALL GETNAM(ROWCOL,I,RNAME)
         CALL BMSKIN(CNAME,RNAME)
         BINDEX(BZERO+I) = BLOCK1
100   CONTINUE
C
      BMASK(BLOCK1) = CNAME
      BNUMBR(BLOCK1)= BNUMBR(BLOCK1) + BNUMBR(BLOCK2)
      BNUMBR(BLOCK2)= 0
C
      CALL BERASE(ROWCOL,BLOCK2)
C
      RETURN
C
C ** BLMERG ENDS HERE
      END
      SUBROUTINE BLCOPY(ROWCOL,WHERE,CNAME,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C COPY  TO | FROM  block (ROWCOL CNAME) from|to, resp., submatrix
C ...WHERE is 'TO' or 'FROM'
C
      CHARACTER*16  CNAME
      CHARACTER*(*) ROWCOL,WHERE
C LOCAL
      CHARACTER*16  RNAME
      LOGICAL*1     SW
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
C GET BLOCK LOCATION (BLOC)
      CALL BLOC8(ROWCOL,CNAME,BLOC,RCODE)
      IF(RCODE.NE.0)THEN
         IF(WHERE.EQ.'FROM')THEN
C TRIED TO COPY FROM A NON-EXISTENT BLOCK
C ...ERROR MESSAGE WAS GIVEN, BUT WE RETURN NO ERROR TO ALLOW
C    EXECUTE FILE TO CONTINUE
            IF(RCODE.LT.0) RCODE = 0
            RETURN
         ENDIF
         IF(SWMSG)PRINT *,' ...creating ',ROWCOL,'block ',CNAME
         RCODE = 0
         CALL BLKNEW(ROWCOL,CNAME,BLOC,RCODE)
         IF(RCODE.NE.0)RETURN
      ENDIF
C
      IF(ROWCOL.EQ.'ROW ')THEN
         K = 1
         LAST  = NROWS
         BZERO = 0
      ELSE
         K = 2
         LAST  = NCOLS
         BZERO = NROWS
      ENDIF
C
      IF(WHERE.EQ.'TO')GOTO 500
C
C COPY FROM BLOCK (BLOC) TO SUBMATRIX
C
      DO 100 NUMBER=1,LAST
         SW = BINDEX(BZERO + NUMBER).EQ.BLOC
         IF(SW)CALL GPUTMP(ROWCOL,NUMBER,SW)
100   CONTINUE
      CALL ASETSB(ROWCOL)
      RETURN
C
500   CONTINUE
C
C COPY TO (BLOC) FROM SUBMATRIX
C
      NRC = NRCSUB(K)
C Initialize NUMber added and name mask (CNAME)
C
      NUM = 0
      CNAME = BMASK(BLOC)
C
      DO 600 NUMBER=RCSUB1(K),LAST
         IF(NRC.EQ.0)GOTO 610
         CALL GETMAP(ROWCOL,NUMBER,SW)
         IF(.NOT.SW)GOTO 600
         NRC = NRC - 1
C
         IF(BINDEX(BZERO + NUMBER).EQ.0)THEN
C    ROW/COL IS NOT IN BLOCK, SO ADD TO THIS ONE
C ...if row/col is in submatrix but is in any block, it does not get added
C
            BINDEX(BZERO + NUMBER) = BLOC
            NUM = NUM + 1
            CALL GETNAM(ROWCOL,NUMBER,RNAME)
            CALL BMSKIN(CNAME,RNAME)
         ENDIF
600   CONTINUE
C
610   CONTINUE
      IF(SWMSG)PRINT *,NUM,' added to block'
C NOW ADD TO BLOCK
      BNUMBR(BLOC) = BNUMBR(BLOC) + NUM
      IF(BNUMBR(BLOC).EQ.0)THEN
         CALL BERASE(ROWCOL,BLOC)
      ELSE
         BMASK(BLOC)  = CNAME
      ENDIF
C
      RETURN
C
C ** BLCOPY ENDS HERE
      END
      SUBROUTINE BERASE(ROWCOL,BLOC)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This erases row/col BLOC
C ...BLOC = 0 means erase all row|col blocks
C
      CHARACTER*(*) ROWCOL
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
      IF(BLOC.EQ.0)THEN
C ERASE ALL BLOCKS
         IF(ROWCOL.EQ.'ROW ')THEN
            DO 10 I=1,NROWS
10          BINDEX(I) = 0
            NRBLKS = 0
         ELSE IF(ROWCOL.EQ.'COL ')THEN
            DO 20 I=NROWS+1,NROWS+NCOLS
20          BINDEX(I) = 0
            NCBLKS = 0
         ENDIF
         RETURN
      ENDIF
C
      IF(SWMSG .OR. SWDBG)THEN
         PRINT *,' ERASING ',ROWCOL,'BLOCK ',BNAME(BLOC)
         PAUSE = PAUSE-1
      ENDIF
C ERASE BLOC
C
      IF(ROWCOL.EQ.'ROW ')THEN
         IF(NRBLKS.EQ.0)RETURN
C DELETE ROW BLOC
C ...FIRST THE MEMBERSHIP INDEXES
         DO 100 I=1,NROWS
100      IF(BINDEX(I).EQ.BLOC)BINDEX(I)=0
         IF(BLOC.LT.NRBLKS)THEN
C ...MOVE LAST BLOCK INTO VACATED POSITION
            DO 150 I=1,NROWS
150         IF(BINDEX(I).EQ.NRBLKS)BINDEX(I)=BLOC
            BNAME(BLOC) =BNAME(NRBLKS)
            BNUMBR(BLOC)=BNUMBR(NRBLKS)
            BMASK(BLOC) =BMASK(NRBLKS)
         ENDIF
C ...NOW DECREMENT THE NUMBER OF BLOCKS
         NRBLKS= NRBLKS - 1
      ELSE
         IF(NCBLKS.EQ.0)RETURN
C DELETE COLUMN BLOCK
C ...DO AS ABOVE (EXCEPT COL BLOCKS GROW FROM BOTTOM)
         DO 200 I=1+NROWS,NCOLS+NROWS
200      IF(BINDEX(I).EQ.BLOC)BINDEX(I)=0
         LASTBL = MAXBLK-NCBLKS+1
         IF(BLOC.GT.LASTBL)THEN
C EXCHANGE THE LAST BLOCK WITH THIS VACATED POSITION
            DO 250 I=1+NROWS,NCOLS+NROWS
250         IF(BINDEX(I).EQ.LASTBL)BINDEX(I)=BLOC
            BNAME(BLOC) =BNAME(LASTBL)
            BNUMBR(BLOC)=BNUMBR(LASTBL)
            BMASK(BLOC) =BMASK(LASTBL)
         ENDIF
         NCBLKS= NCBLKS - 1
      ENDIF
C
      RETURN
C
C ** BERASE ENDS HERE
      END
      SUBROUTINE BLKCLR
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This clears null blocks.
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
C ROW BLOCKS
      BLOC = 1
100   IF(NRBLKS.EQ.0) GOTO 500
      IF(BNUMBR(NRBLKS).EQ.0)THEN
         NRBLKS = NRBLKS-1
         GOTO 100
      ENDIF
200   IF(BLOC.GE.NRBLKS)GOTO 500
      IF(BNUMBR(BLOC).GT.0)THEN
         BLOC = BLOC+1
         GOTO 200
      ELSE
C BLOC IS NULL
C ...MOVE LAST BLOCK INTO VACATED POSITION
         DO 250 I=1,NROWS
250      IF(BINDEX(I).EQ.NRBLKS)BINDEX(I)=BLOC
         BNAME(BLOC) =BNAME(NRBLKS)
         BNUMBR(BLOC)=BNUMBR(NRBLKS)
         BMASK(BLOC) =BMASK(NRBLKS)
C ...NOW DECREMENT THE NUMBER OF BLOCKS
         NRBLKS= NRBLKS - 1
         GOTO 100
      ENDIF
C
500   BLOC = MAXBLK
C COLUMN BLOCKS
510   IF(NCBLKS.EQ.0)RETURN
      LASTBL = MAXBLK-NCBLKS+1
      IF(BNUMBR(LASTBL).EQ.0)THEN
         NCBLKS = NCBLKS-1
         GOTO 510
      ENDIF
600   IF(BLOC.LE.LASTBL)RETURN
      IF(BNUMBR(BLOC).GT.0)THEN
         BLOC = BLOC-1
         GOTO 600
      ENDIF
C BLOC IS NULL
C ...MOVE LAST BLOCK INTO VACATED POSITION
      DO 650 I=1+NROWS,NCOLS+NROWS
650   IF(BINDEX(I).EQ.LASTBL)BINDEX(I)=BLOC
      BNAME(BLOC) =BNAME(LASTBL)
      BNUMBR(BLOC)=BNUMBR(LASTBL)
      BMASK(BLOC) =BMASK(LASTBL)
      NCBLKS= NCBLKS - 1
      GOTO 510
C
C ** BLKCLR ENDS HERE
      END
      SUBROUTINE BLKREN(ROWCOL,BLOC,CNAME,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This renames BLOC to be CNAME
C
      CHARACTER*(*) ROWCOL
      CHARACTER*16  CNAME
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
C Make sure block with new name (CNAME) does not already exist
      CALL BLOC8(ROWCOL,CNAME,BLOCK2,RCODE)
      IF(RCODE.EQ.0)THEN
         PRINT *,ROWCOL,' BLOCK ',CNAME,' already exists'
         RCODE = -1
         RETURN
      ENDIF
C OK
      RCODE = 0
      BNAME(BLOC) = CNAME
      RETURN
C
C ** BLKREN ENDS HERE
      END
      SUBROUTINE BLKNEW(ROWCOL,RNAME,BLOC,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This creates new row/col block (RNAME)
C
      CHARACTER*16  RNAME
      CHARACTER*(*) ROWCOL
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
C CHECK FOR SPACE
      IF(NRBLKS+NCBLKS.EQ.MAXBLK)THEN
         PRINT *,' Sorry, not enough room to add new block'
         RCODE = 1
         RETURN
      ENDIF
C CHECK FOR DUPLICATE NAME
      CALL BLOC8(ROWCOL,RNAME,BLOC,RCODE)
      IF(RCODE.EQ.0)THEN
         PRINT *,ROWCOL,'block ',RNAME,' already exists...',
     1                  'Erase or Rename'
         RCODE = 1
         RETURN
      ELSE
C RCODE NOT 0 MEANS BLOCK DOES NOT EXIST (WHICH IS WHAT WE WANT)
         RCODE = 0
      ENDIF
C OK, INCREMENT NUMBER OF BLOCKS (ROW OR COL)
      IF(ROWCOL.EQ.'ROW ')THEN
         NRBLKS = NRBLKS + 1
         BLOC   = NRBLKS
      ELSE
         NCBLKS = NCBLKS + 1
         BLOC   = MAXBLK - NCBLKS + 1
      ENDIF
C INITIALIZE NAME, MASK AND NUMBER
      BNAME(BLOC) = RNAME
      BMASK(BLOC) = ' '
      BNUMBR(BLOC)= 0
C
      RETURN
C
C ** BLKNEW ENDS HERE
      END
      SUBROUTINE BLKADD(ROWCOL,NUMBER,BLOC)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This adds ROWCOL NUMBER to BLOC
C
      CHARACTER*(*) ROWCOL
C LOCAL
      CHARACTER*16  RNAME
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
      IF(ROWCOL.EQ.'ROW ')THEN
         BIND = NUMBER
      ELSE
         BIND = NROWS + NUMBER
      ENDIF
      OLDBLK = BINDEX(BIND)
      IF(OLDBLK.EQ.BLOC)RETURN
C
      BINDEX(BIND) = BLOC
      BNUMBR(BLOC) = BNUMBR(BLOC) + 1
      CALL GETNAM(ROWCOL,NUMBER,RNAME)
      CALL BMSKIN(BMASK(BLOC),RNAME)
      IF(OLDBLK.GT.0)THEN
C SUBTRACT FROM ITS OLD BLOCK
         BNUMBR(OLDBLK) = BNUMBR(OLDBLK) - 1
         IF(SWMSG)PRINT *,' ',ROWCOL,RNAME,' MOVED FROM ',ROWCOL,
     1      ' BLOCK ',BNAME(OLDBLK)
      ENDIF
C
      RETURN
C
C ** BLKADD ENDS HERE
      END
      SUBROUTINE BLOC8(ROWCOL,RNAME,LOC,RCODE)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This gets LOCation of block ROWCOL RNAME
C ...RCODE = -1 if RNAME not found (else, RCODE = 0)
C ...Error message printed if RCODE NE 0 upon entrance
C
      CHARACTER*(*) ROWCOL
      CHARACTER*16  RNAME
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
C  SET POINTERS FOR SEARCH
C     ROW BLOCKS GROW DOWN: 1,...,NRBLKS
C     COL BLOCKS GROW UP: MAXBLK,...,MAXBLK-NCBLKS+1
      IF(ROWCOL.EQ.'ROW ')THEN
           IF(NRBLKS.EQ.0)GOTO 13
           FIRST = 1
           LAST  = NRBLKS
      ELSE
           IF(NCBLKS.EQ.0)GOTO 13
           FIRST = MAXBLK - NCBLKS + 1
           LAST  = MAXBLK
      ENDIF
C LOOP OVER BLOCKS TO MATCH NAME (RNAME)
      DO 100 LOC=FIRST,LAST
          IF(BNAME(LOC).EQ.RNAME(:8))THEN
             RCODE = 0
             RETURN
          ENDIF
100   CONTINUE
C
13    IF(RCODE.NE.0 .AND. SWMSG)
     1   PRINT *,' ** ',ROWCOL,' BLOCK ',RNAME(:8),' not recognized'
      RCODE = -1
      RETURN
C
C ** BLOC8 ENDS HERE
      END
      SUBROUTINE BMSKIN(RNAME,CNAME)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This intersects mask RNAME with name CNAME
C ...result is put into RNAME
C
      CHARACTER*16  RNAME,CNAME
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
      IF(RNAME.EQ.' ')THEN
         RNAME = CNAME
         RETURN
      ENDIF
      DO 10 K=1,NAMELN
         IF(RNAME(K:K).NE.CNAME(K:K))RNAME(K:K) = '*'
10    CONTINUE
C
      RETURN
C
C ** BMSKIN ENDS HERE
      END
      SUBROUTINE BDEBUG
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C LOCAL
      CHARACTER*16  RNAME
      CHARACTER*1   CHAR
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
      PRINT *,' BLOCK DEBUG ENTERED'
      PRINT *,' NRBLKS=',NRBLKS,'  NCBLKS=',NCBLKS
      PRINT *,' MAXBLK=',MAXBLK,' MXBIND=',MXBIND
C
10    RCODE = 0
      PRINT *,' Index, Blocks'
      READ(*,1)CHAR
1     FORMAT(A1)
      IF(CHAR.EQ.' ')RETURN
C
      IF(CHAR.EQ.'I')THEN
         RNAME = ' BINDEX'
         BRANCH = 1
      ELSE IF(CHAR.EQ.'B')THEN
         RNAME = ' BLOCKS'
         BRANCH = 2
      ELSE
         PRINT *,'  ? ',CHAR
         GOTO 10
      ENDIF
C
100   PRINT *,RNAME
      PRINT *,' ENTER RANGE (v[/v]) '
      READ(*,101)RNAME
101   FORMAT(A16)
      IF(RNAME.EQ.' ')GOTO 10
      CALL FSLEN(RNAME,NAMLEN,LAST)
      FIRST = 1
      CALL FVRNG(RNAME,FIRST,LAST,VL,VU,VINF,CHAR,RCODE)
      IF(RCODE.NE.0)GOTO 10
C
      I1 = VL + .1
      I2 = VU + .1
      IF(I1.GT.I2)I2=I1+I2-1
      GOTO(110,120),BRANCH
C
110   WRITE(*,111)(I,BINDEX(I),I=I1,I2)
111   FORMAT(5(I5,'=',I9))
      GOTO 100
120   WRITE(*,121)(I,BNAME(I),BMASK(I),BNUMBR(I),I=I1,I2)
121   FORMAT(I5,' NAME=',A8,'  MASK=',A16,'  NUMBR=',I5)
      GOTO 100
C
C ** BDEBUG ENDS HERE
      END
