C             ::: GETIOPCK.FOR  8-09-95 :::
C
C Earlier dates deleted
C    9-27-94...Added GVERFY (8-8-95) & GPRDIF ===> GETBRVER.FOR
C
C This contains the following GETMAT IO subroutines.
C
C     GRDPCK.....reads unformatted packed file
C     GWRPCK.....writes unformatted packed file
C     GRDFPK.....reads formatted packed file
C     GWRFPK.....writes formatted packed file
C THE FOLLOWING ROUTINES SUPPORT GETIOSOL (PUT HERE FOR SPACE REASONS)
C     GRDSTA.....decode row or column solution status
C     GFRCOL.....process non-basic free column
C     GSLFRE.....sets levels of free rows (OSL drops them)
C
      SUBROUTINE GRDPCK(SWRATE,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This gets packed file from PCKFIL, presumed open (UNFORMATTED)
C
      LOGICAL*1 SWRATE
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::
C FIRST SAVE OLD BASE (EXCEPT MAX'S)
      DO 10 I=1,EBASE2
10    BASE2(I) = BASE(I)
      RCODE = 1
      PCKSAV = PCKFIL
      READ(PCKFIL,END=99,ERR=13)(BASE(I),I=1,EBASE2)
C PCKFIL MIGHT HAVE CHANGED SINCE WRITTEN (IN BASE)
      PCKFIL = PCKSAV
C
      INDLEN = IFREE
      IF(IVFREE.GT.PMXVAL.OR.INDLEN.GT.PMXIND.OR.ENDNAM.GT.PMXNAM)THEN
         PRINT *,' ** DIMENSION OF NEW FILE EXCEEDS MEMORY ALLOCATION'
         PRINT *,' INDEX NEEDS ',INDLEN,'...HAS ',PMXIND
         PRINT *,' VALUE NEEDS ',IVFREE,'...HAS ',PMXVAL
         PRINT *,' NAME  NEEDS ',ENDNAM,'...HAS ',PMXNAM
         GOTO 75
      ENDIF
C
      RCODE = 2
C
      DO 1 I0=ENDIND,IFREE,256
         I1 = I0 + 255
         IF(I1.GT.IFREE)I1 = IFREE
         READ(PCKFIL,END=99,ERR=13)(INDEX(I),I=I0,I1)
1     CONTINUE
C
      RCODE = 3
      READ(PCKFIL,END=99,ERR=13)
     1     PRBNAM,OBJNAM,RHSNAM,RNGNAM,BNDNAM,SOLST,SOLNAM
      DO 2 I0=1,ENDNAM,128
         I1 = I0 + 127
         IF(I1.GT.ENDNAM)I1 = ENDNAM
         READ(PCKFIL,END=99,ERR=13)(NAMERC(I),I=I0,I1)
2     CONTINUE
C
      RCODE = 4
      DO 3 I0=2,IVFREE,256
         I1 = I0 + 255
         IF(I1.GT.IVFREE)I1 = IVFREE
         READ(PCKFIL,END=99,ERR=13)(VALUE(I),I=I0,I1)
3     CONTINUE
C
      RCODE = 5
      READ(PCKFIL,END=99,ERR=13)PIVNUM
      IF( PIVNUM.GT.0 )READ(PCKFIL,END=99,ERR=13)
     P  ( PIVIN(I0),PIVOUT(I0),PIVNBL(I0),PIVST0(I0),
     P   PIVSTI(I0),PIVSTO(I0),I0=1,PIVNUM)
C
      RCODE = 0
      CLOSE(PCKFIL)
      SWRATE = IPIVOT .GT. 0
      RETURN
C
C PROBLEM, BUT CAN RESTORE BASE TO KEEP LP RESIDENT
75    CONTINUE
      DO 80 I=1,EBASE2
80    BASE(I) = BASE2(I)
      GOTO 1390
C
99    RCODE = -RCODE
C
13    PRINT *,' ** IO ERROR READING PACKED FILE',PCKFIL,' RCODE=',RCODE
1390  CLOSE(PCKFIL)
      RETURN
C
C ** GRDPCK ENDS HERE
      END
      SUBROUTINE GWRPCK(RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This writes PCKFIL
C
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::
C WRITE BASE FIRST
      RCODE = 1
      WRITE(PCKFIL,ERR=13)(BASE(I),I=1,EBASE2)
      DO 1 I0=ENDIND,IFREE,256
         I1 = I0 + 255
         IF(I1.GT.IFREE)I1 = IFREE
1     WRITE(PCKFIL,ERR=13)(INDEX(I),I=I0,I1)
      RCODE = 2
      WRITE(PCKFIL,ERR=13)
     1     PRBNAM,OBJNAM,RHSNAM,RNGNAM,BNDNAM,SOLST,SOLNAM
      DO 2 I0=1,ENDNAM,128
         I1 = I0 + 127
         IF(I1.GT.ENDNAM)I1 = ENDNAM
2     WRITE(PCKFIL,ERR=13)(NAMERC(I),I=I0,I1)
      RCODE = 3
      DO 3 I0=2,IVFREE,256
         I1 = I0 + 255
         IF(I1.GT.IVFREE)I1 = IVFREE
3     WRITE(PCKFIL,ERR=13)(VALUE(I),I=I0,I1)
C
      RCODE = 5
      WRITE(PCKFIL,ERR=13)PIVNUM
      IF( PIVNUM.GT.0 )WRITE(PCKFIL,ERR=13)
     P  ( PIVIN(I0),PIVOUT(I0),PIVNBL(I0),PIVST0(I0),
     P   PIVSTI(I0),PIVSTO(I0),I0=1,PIVNUM)
C
C WRITE A DUMMY RECORD TO AVOID EOF
      WRITE(PCKFIL,ERR=13)VALUE(0)
      RCODE = 0
      CLOSE(PCKFIL)
      RETURN
C
13    CLOSE(PCKFIL)
      PRINT *,' ** IO ERROR WRITING PACKED FILE',PCKFIL
      PRINT *,' RC I0 I =',RCODE,I0,I
      RETURN
C
C ** GWRPCK ENDS HERE
      END
      SUBROUTINE GRDFPK(SWRATE,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This gets formatted packed file from DATFIL, presumed open.
C    SWRATE = TRUE means the basis is setup to support the RATEOF command.
C        (This is determined after reading in the LP...see below.)
C
      LOGICAL*1 SWRATE
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::
C FIRST SAVE OLD BASE (EXCEPT MAX'S)
      DO 10 I=1,EBASE2
10    BASE2(I) = BASE(I)
      RCODE = 1
      DATSAV = DATFIL
      READ(DATFIL,1,END=99,ERR=13)(BASE(I),I=1,EBASE2)
1     FORMAT(5I15)
C DATFIL MIGHT HAVE CHANGED SINCE WRITTEN (IN BASE)
      DATFIL = DATSAV
C
      IF(IVFREE.GT.MAXVAL.OR.IFREE.GT.MAXIND.OR.ENDNAM.GT.MAXNAM)THEN
         PRINT *,' ** DIMENSION OF NEW FILE EXCEEDS MEMORY ALLOCATION'
         PRINT *,' INDEX NEEDS ',IFREE ,'...HAS ',MAXIND
         PRINT *,' VALUE NEEDS ',IVFREE,'...HAS ',MAXVAL
         PRINT *,' NAME  NEEDS ',ENDNAM,'...HAS ',MAXNAM
         GOTO 75
      ENDIF
C
      RCODE = 2
      READ(DATFIL,21,END=99,ERR=13)(BASE(I),I=ENDBAS+1,IFREE)
21    FORMAT(2I35)
      RCODE = 3
      READ(DATFIL,31,END=99,ERR=13)
     1     PRBNAM,OBJNAM,RHSNAM,RNGNAM,BNDNAM,SOLST,SOLNAM
31    FORMAT(4A16)
      RCODE = 4
      READ(DATFIL,32,END=99,ERR=13)(NAMERC(I),I=1,ENDNAM)
32    FORMAT(80A1)
      RCODE = 5
CC 9-27 READ SINGLE PRECISION VALUES, NOT ALPHA'S
      IF( IZBLST.EQ.0 )THEN
         READ(DATFIL,4,END=99,ERR=13)(VALUE(I),I=1,IVFREE)
4        FORMAT(5E15.6)
      ELSE
         READ(DATFIL,4,END=99,ERR=13)(VALUE(I),I=1,2*IZBLST)
         READ(DATFIL,41,END=99,ERR=13)(ZVALUE(I),I=IZBLST,IVFREE/2+1)
41       FORMAT(3D20.10)
      ENDIF
CC ====
C
      RCODE = 6
      READ(DATFIL,61,END=99,ERR=13)PIVNUM
61    FORMAT(I5)
      IF( PIVNUM.GT.0 )READ(DATFIL,62,END=99,ERR=13)
     P  ( PIVIN(I0),PIVOUT(I0),PIVNBL(I0),PIVST0(I0),
     P   PIVSTI(I0),PIVSTO(I0),I0=1,PIVNUM)
62    FORMAT(3I5,2L1)
C
      RCODE = 0
      CLOSE(DATFIL)
      SWRATE = IPIVOT .GT. 0
      RETURN
C
C PROBLEM, BUT CAN RESTORE BASE TO KEEP LP RESIDENT
75    CONTINUE
      DO 80 I=1,EBASE2
80    BASE(I) = BASE2(I)
      GOTO 1390
C
99    RCODE = -RCODE
C
13    PRINT *,' ** IO ERROR READING FORMATTED PACKED FILE',DATFIL
      PRINT *,' RC I =',RCODE,I
1390  CLOSE(DATFIL)
      RETURN
C
C ** GRDFPK ENDS HERE
      END
      SUBROUTINE GWRFPK(RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This writes formatted packed file to DATFIL, presumed open
C
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::
C WRITE BASE FIRST
      RCODE = 1
      WRITE(DATFIL,1,ERR=13)(BASE(I),I=1,EBASE2)
1     FORMAT(5I15)
      RCODE = 2
      WRITE(DATFIL,21,ERR=13)(BASE(I),I=ENDBAS+1,IFREE)
21    FORMAT(2I35)
C              :...BASE CONTAINS INDEX (INT*2), SO ITS NUMERIC
C                  VALUE COULD BE HUGE
      RCODE = 3
      WRITE(DATFIL,31,ERR=13)
     1     PRBNAM,OBJNAM,RHSNAM,RNGNAM,BNDNAM,SOLST,SOLNAM
31    FORMAT(4A16)
      RCODE = 4
      WRITE(DATFIL,32,ERR=13)(NAMERC(I),I=1,ENDNAM)
32    FORMAT(80A1)
      RCODE = 5
CC 9-27 WRITE SINGLE PRECISION VALUES, NOT ALPHA'S
      IF( IZBLST.EQ.0 )THEN
         WRITE(DATFIL,4,ERR=13)(VALUE(I),I=1,IVFREE)
4        FORMAT(5E15.6)
      ELSE
         WRITE(DATFIL,4,ERR=13)(VALUE(I),I=1,2*IZBLST)
         WRITE(DATFIL,41,ERR=13)(ZVALUE(I),I=IZBLST,IVFREE/2+1)
41       FORMAT(3D20.10)
      ENDIF
CC ====
C
      RCODE = 6
      WRITE(DATFIL,61,ERR=13)PIVNUM
61    FORMAT(I5)
      IF( PIVNUM.GT.0 )WRITE(DATFIL,62,ERR=13)
     P  ( PIVIN(I0),PIVOUT(I0),PIVNBL(I0),PIVST0(I0),
     P   PIVSTI(I0),PIVSTO(I0),I0=1,PIVNUM)
62    FORMAT(3I5,2L1)
C
C WRITE AN EXTRA LINE TO AVOID EOF WHEN READING
      WRITE(DATFIL,39,ERR=13)
39    FORMAT(' ::: END FORMATTED PACKED FILE :::')
C
      RCODE = 0
      CLOSE(DATFIL)
      RETURN
C
13    CLOSE(DATFIL)
      PRINT *,' ** IO ERROR WRITING FORMATTED PACKED FILE',DATFIL,
     1        ' RC =',RCODE
      RETURN
C
C ** GWRFPK ENDS HERE
      END
      SUBROUTINE GRDSTA(ROWCOL,NUMBER,VX,VP,STAT,NFRCOL,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This decodes solution status (STAT) of ROWCOL NUMBER
C       VX = LEVEL (needed only if free column)
C       VP = PRICE (needed only if STAT depends on price)
C       NFRCOL = Number of free columns whose bound had to be
C                adjusted (incremented if done)
C ...Alternate return is unrecognized status (fatal error).
C
      CHARACTER*(*) ROWCOL, STAT
C
C RECOGNIZED STATUSES                STAT
C   BS...BASIC                  ===>  B
C   **...INFEASIBLE (AND BASIC) ===>  I
C   FR...FREE                   ===>  B if ROW
C                                     L or U if COL
C                                     :....:...depends on sign of price
C                                              bound is changed = level
C                                              & NFRCOL is incremented
C   EQ...EQUATION               ===> depends on price
C   FX...FIXED COLUMN           ===> depends on price
C   UL...UPPER LIMIT            ===> U
C   LL...LOWER LIMIT            ===> L
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::
      IF( STAT.EQ.'*' )STAT = 'I'
      IF( STAT.EQ.'L' .OR. STAT.EQ.'U' .OR. STAT.EQ.'B' .OR.
     1    STAT.EQ.'I' )RETURN
      IF( STAT.EQ.'E' )THEN
C EQUATION (IN MINOS, THIS COULD BE A FIXED COLUMN)
C ...SET BOUND STATUS ACCORDING TO PRICE
          IF( OPT*VP.LE.0. )THEN
            STAT = 'L'
         ELSE
            STAT = 'U'
         ENDIF
      ELSE IF( STAT.EQ.'F' )THEN
         IF( ROWCOL.EQ.'ROW ' )THEN
C FREE ROW IS BASIC
            STAT = 'B'
         ELSE IF( ROWCOL.EQ.'COL ' )THEN
C WE MUST SEE IF COLUMN IS FREE OR FIXED
            CALL GETBND(ROWCOL,NUMBER,VL,VU)
            IF( VL.GE.VU )THEN
C FIXED COLUMN...SET BOUND STATUS ACCORDING TO PRICE
               IF( OPT*VP.LE.0. )THEN
                  STAT = 'L'
               ELSE
                  STAT = 'U'
               ENDIF
            ELSE IF( VL.LT.-VINF .AND. VU.GE.VINF )THEN
C FREE COLUMN (THAT IS NOT BASIC)
               CALL GFRCOL(NUMBER,VX,VP,STAT)
               NFRCOL = NFRCOL + 1
            ELSE
               GOTO 1300
            ENDIF
         ELSE
            GOTO 1300
         ENDIF
      ELSE
         GOTO 1300
      ENDIF
C
      RETURN
C
C STATUS NOT RECOGNIZED
1300  CONTINUE
      IF( ROWCOL.EQ.'ROW' )THEN
         I = IRNAME
      ELSE IF( ROWCOL.EQ.'COL ' )THEN
         I = ICNAME
      ELSE
         PRINT *,' ** SOLUTION STATUS NOT RECOGNIZED: ',STAT
         PRINT *,'    ...ALSO, SYSERR IN GRDSTA...',ROWCOL
         RETURN 1
      ENDIF
      PRINT *,' ** SOLUTION STATUS NOT RECOGNIZED FOR ',ROWCOL,
     1        NAME(I+NUMBER),': ',STAT
      RETURN 1
C
C ** GRDSTA ENDS HERE
      END
      SUBROUTINE GFRCOL(COL,VX,VP,STAT)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This processes a non-basic free column (COL) with level = VX
C and reduced cost = VP.
C
      CHARACTER*1 STAT
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      IF( OPT.EQ.OPTMIN .OR. SOLST(1:6).EQ.'INFEAS' )THEN
C USE REDUCED COST AS IS
          VPRICE = VP
      ELSE
C REVERSE SIGN OF REDUCED COST
          VPRICE = -VP
      ENDIF
      IF( VPRICE.GE.0. )THEN
C CHANGE TO STAT = L WITH LOWER BOUND = LEVEL
         STAT = 'L'
         IBOUND = ICLBND
      ELSE
C CHANGE TO STAT = U WITH UPPER BOUND = LEVEL
         STAT = 'U'
         IBOUND = ICUBND
      ENDIF
C NOW CHANGE BOUND POINTER
      IF( ABS(VX).LE.VTOLAB )THEN
         INDEX( IBOUND+COL ) = 0
      ELSE IF( ABS(VX-1.).LE.VTOLAB )THEN
         INDEX( IBOUND+COL ) = 1
      ELSE IF( ABS(VX+1.).LE.VTOLAB )THEN
         INDEX( IBOUND+COL ) = -1
      ELSE
C ADD VALUE
         IF( IVFREE.GE.MAXVAL )THEN
C ...OH, OH, NO MORE VALUE SPACE...MAKE BASIC AND CORRUPT SOLUTION
C (MOST LIKELY, THE SUPER-BASIC HAS 0 LEVEL, SO THIS IS NOT NECESSARY)
            STAT = 'B'
            VP = 0.
         ELSE
            IF( VX.GT.0. )THEN
               INDEX( IBOUND+COL ) = IVFREE
               VALUE( IVFREE )     = VX
            ELSE
               INDEX( IBOUND+COL ) = -IVFREE
               VALUE( IVFREE )     = -VX
            ENDIF
            IVFREE = IVFREE+1
         ENDIF
      ENDIF
C
      RETURN
C
C ** GFRCOL ENDS HERE
      END
      SUBROUTINE GSLFRE(SWMSG,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This sets levels of free rows (stat = B).
C ...Alternate return is not enough space to set levels.
C
      LOGICAL*1   SWMSG
      CHARACTER*1 CHAR
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      IF( IVFREE+NROWS.GT.PMXVAL )THEN
        PRINT *,' ** NOT ENOUGH VALUE SPACE TO SET LEVELS OF FREE ROWS'
        PRINT *,'    ...SETTING LEVELS = 0 (EXCEPT OBJECTIVE)'
         DO 100 I=1,NROWS
            IF( I.EQ.OBJNUM )GOTO 100
            CALL GETYPE(I,CHAR)
            IF( CHAR.NE.'N' )GOTO 100
C ROW I IS FREE AND NOT THE OBJ...SET STAT=B AND LEVEL=PRICE=0
            CALL GPUTSL(IRSTAT+I,IRSOLV+I,'B',0.,0.,RCODE)
            IF( RCODE.NE.0 )RETURN
100      CONTINUE
         RETURN
      ENDIF
C
      IF( SWMSG )PRINT *,' Setting levels of free rows'
      DO 500 I=1,NROWS
500   VALUE(IVFREE+I) = 0.
C
      DO 600 J=1,NCOLS
         CALL GETSOL('COL ',J,VX,VP,CHAR,DUMMY)
         NZ = BASE(ICINFO + J) - BASE(ICINFO + J-1)
         IF( NZ.EQ.0 )GOTO 600
         I1 = INONZ + 2*BASE(ICINFO+J-1)
         I2 = I1 + 2*NZ - 1
         DO 550 I=I1,I2,2
            ROW = INDEX(I)
            CALL GETVAL(V,I+1)
            VALUE(IVFREE+ROW) = VALUE(IVFREE+ROW) + VX*V
550      CONTINUE
600   CONTINUE
C NOW VALUE(IVFREE+i) = ROW i LEVEL (ALL i)
C
      DO 900 I=1,NROWS
         IF( I.EQ.OBJNUM )GOTO 900
         CALL GETYPE(I,CHAR)
         IF( CHAR.NE.'N' )GOTO 900
C ROW I IS FREE AND NOT THE OBJ
         VX = VALUE(IVFREE+I)
C STORE ITS LEVEL (STAT=B AND PRICE=0)
         CALL GPUTSL(IRSTAT+I,IRSOLV+I,'B',VX,0.,RCODE)
         IF( RCODE.NE.0 )RETURN
900   CONTINUE
C
      RETURN
C
C ** GSLFRE ENDS HERE
      END
