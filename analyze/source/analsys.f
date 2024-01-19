C              ::: ANALSYS.FOR  8-15-95 :::
C
C Earlier dates deleted
C     BEGIN 12.4   7-23-94
C     BEGIN 12.5  12-09-94
C     4-07-94...Added VERIFY command
C     5-11-95...Fixed bug in SYSDBG (STAT is CHAR)
C     5-28-95...Removed OB1 for _SETUP (CC 7-21)
C     7-21-95...BEGIN 12.6  8-03-95
C     8-06-95...BEGIN 13.0  8-15-95
C
C This file contains the SYS routines for FLIP:
C
C       SYSIN.....initial call from FLIP
C       SYSCMD....execute ANALYZE command
C       SYSDBG....debug routine
C
      SUBROUTINE SYSIN(SYSNAM,FVERSN,NCSYS,COMAND,NSWSYS,SWNAME,SWVALU)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C EXTENSION NAMES (IN ADDITION TO THOSE IN DCFLIP)
      CHARACTER*4   EXTMAT,EXTPCK,EXTFPK,EXTSYN,EXTMNS,       EXTOSL,
     1              EXTMPS,EXTSUB,EXTBLK,EXTRUL,EXTSCH,EXTIIS,EXTGPH
C
      CHARACTER*(*) FVERSN
      CHARACTER*8   SYSNAM,COMAND(29),SWNAME(3)
      LOGICAL*1     SWVALU(3)
      INTEGER*2     CMDMAP(29)
C                   :...THIS IS TO HAVE COMMANDS APPEAR IN SORT ORDER
      COMMON/ANLCMD/CMDMAP
C            :...APPEARS IN ONLY THIS FILE
C LOCAL
      CHARACTER*48  PREFIX
      CHARACTER*8   SCHEMA
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
C SET SWITCHES
      NSWSYS = 3
      SWNAME(1) = 'DISP_COL'
      SWVALU(1)= .TRUE.
      SWNAME(2) = 'OPT_MIN'
      SWVALU(2)= .TRUE.
      SWNAME(3) = 'SYNTAX'
      SWVALU(3)= .FALSE.
      SWRULE = .FALSE.
C INITIALIZE ANALYZE TO HAVE NO LP IN MEMORY
      PRBNAM = ' '
C INITIALIZE OPTIMIZATION
      OPTNAM = 'MINIMIZE'
      SOLST  = 'unknown'
C INITIALIZE MAX NONZERO LIST LENGTH (ROWLST AND VALLST)
      MAXLST = PMXROW
C INITIALIZE SCRATCH LIST
      SCRIND(0) = PMXROW
C SET FILE EXTENSIONS
      EXTBLK = '.BLK'
      EXTFPK = '.FPK'
      EXTGPH = '.GPH'
      EXTIIS = '.IIS'
      EXTMAT = '.MAT'
      EXTMNS = '.MNS'
      EXTMPS = '.MPS'
      EXTOSL = '.OSL'
      EXTPCK = '.PCK'
      EXTRUL = '.RUL'
      EXTSCH = '.SCH'
      EXTSUB = '.SUB'
      EXTSYN = '.SYN'
      NAMLEN = 64
C
C TSO EXTENSIONS ARE NULL
CXTSO      EXTBLK = ' '
CXTSO      EXTFPK = ' '
CXTSO      EXTGPH = ' '
CXTSO      EXTIIS = ' '
CXTSO      EXTMAT = ' '
CXTSO      EXTMNS = ' '
CXTSO      EXTMPS = ' '
CXTSO      EXTOSL = ' '
CXTSO      EXTPCK = ' '
CXTSO      EXTRUL = ' '
CXTSO      EXTSCH = ' '
CXTSO      EXTSUB = ' '
CXTSO      EXTSYN = ' '
C
CXTSO      NAMLEN = 7
C
C CMS EXTENSIONS USE BLANK INSTEAD OF DOT
CICMS      EXTBLK = ' BLK'
CICMS      EXTFPK = ' FPK'
CICMS      EXTIIS = ' GPH'
CICMS      EXTIIS = ' IIS'
CICMS      EXTMAT = ' MAT'
CICMS      EXTMNS = ' MNS'
CICMS      EXTMPS = ' MPS'
CICMS      EXTOSL = ' OSL'
CICMS      EXTPCK = ' PCK'
CICMS      EXTRUL = ' RUL'
CICMS      EXTSCH = ' SCH'
CICMS      EXTSUB = ' SUB'
CICMS      EXTSYN = ' SYN'
C
CICMS      NAMLEN = 8
C
      RCODE = 0
      PREFIX = ' '
      CALL FSET0('BLOCK   ',PREFIX,EXTBLK,RCODE)
      CALL FSET0('FORMAT  ',PREFIX,EXTFPK,RCODE)
      CALL FSET0('GRAPH   ',PREFIX,EXTGPH,RCODE)
      CALL FSET0('IIS     ',PREFIX,EXTIIS,RCODE)
      CALL FSET0('MATRIX  ',PREFIX,EXTMAT,RCODE)
      CALL FSET0('MINOS   ',PREFIX,EXTMNS,RCODE)
      CALL FSET0('MPS     ',PREFIX,EXTMPS,RCODE)
CC 7-21-95      CALL FSET0('OB1     ',PREFIX,EXTOB1,RCODE)
      CALL FSET0('OSL     ',PREFIX,EXTOSL,RCODE)
      CALL FSET0('PACKED  ',PREFIX,EXTPCK,RCODE)
      CALL FSET0('RULEBASE',PREFIX,EXTRUL,RCODE)
      CALL FSET0('SCHEMA  ',PREFIX,EXTSCH,RCODE)
      CALL FSET0('SUBMAT  ',PREFIX,EXTSUB,RCODE)
      CALL FSET0('SYNTAX  ',PREFIX,EXTSYN,RCODE)
      IF(RCODE.NE.0)THEN
         PRINT *,' ** CANNOT INITIALIZE SETUP...PLEASE REPORT...',
     1           RCODE
         STOP
      ENDIF
C SET FILE UNITS FOR SESSION
      DATFIL = 10
      PCKFIL = 11
C  ...PCKFIL IS UNFORMATTED
      RULFIL = 21
C
      SYSNAM = 'ANALYZE'
      VERSYS = '8.06'
C
C INITIALIZE DEBUG
      SWDBG = .FALSE.
      PAUSE0 = 1
      PAUSE  = 0
C INITIALIZE GETMAT
      CALL GINITL(DATFIL,PCKFIL,VERGET)
C INITIALIZE BLOCKS
      CALL BLINIT
C INITIALIZE EXPLAIN
      CALL EXINIT
C INITIALIZE SUBMATRIX
      CALL ASBMAT
C INITIALIZE RULEBASE
      CALL RUINIT
C INITIALIZE SCHEMA
      CALL SCHCLR(0,0,*111)
      SCHEMA = 'SCHEMA  '
      GOTO 112
111   PRINT *,' ** SYSERR INITIALIZING SCHEMA'
      SCHEMA = 'noschema'
112   CONTINUE
C
      FVERSN = '13.0'
C     ~~~~~~~~~~~~~~~
C SET COMMANDS...ORIGINAL ORDER MAINTAINED FOR BRANCHING IN SYSCMD,
C                AND CMDMAP USED AS CMDMAP(c)=b MEANS THE COMMAND IN
C                SORT ORDER c IS BRANCH NUMBER b (WHICH = ORGINAL ORDER)
C NOTE THAT I WANT TO HAVE READIN AS BRANCH #1 (SEE SYSCMD)
C HERE ARE THE COMMANDS IN BRANCH ORDER
      COMAND(15)='READIN  '
                            CMDMAP(15)= 1
      COMAND(23)='SUMMARY '
                            CMDMAP(23)= 2
      COMAND(29)='WRITEOUT'
                            CMDMAP(29)= 3
      COMAND(22)='SUBMAT  '
                            CMDMAP(22)= 4
      COMAND( 1)='ADDRIM  '
                            CMDMAP( 1)= 5
      COMAND( 6)='DISPLAY '
                            CMDMAP( 6)= 6
      COMAND( 5)='COUNT   '
                            CMDMAP( 5)= 7
      COMAND(12)='LIST    '
                            CMDMAP(12)= 8
      COMAND(13)='PICTURE '
                            CMDMAP(13)= 9
      COMAND(20)='SHOW    '
                            CMDMAP(20)=10
      COMAND(26)='TALLY   '
                            CMDMAP(26)=11
      COMAND(27)='TRACE   '
                            CMDMAP(27)=12
      COMAND( 7)='EXPLAIN '
                            CMDMAP( 7)=13
      COMAND( 4)='BLOCK   '
                            CMDMAP( 4)=14
      COMAND(24)='SYNTAX  '
                            CMDMAP(24)=15
      COMAND(16)='REDUCE  '
                            CMDMAP(16)=16
      COMAND(14)='RATEOF  '
                            CMDMAP(14)=17
      COMAND(21)='SOLUTION'
                            CMDMAP(21)=18
      COMAND(11)='INTERPRT'
                            CMDMAP(11)=19
      COMAND( 3)='BASIS   '
                            CMDMAP( 3)=20
      COMAND(19)=SCHEMA
                            CMDMAP(19)=21
      COMAND( 2)='AGGREGAT'
                            CMDMAP( 2)=22
      COMAND(10)='GRAPH   '
                            CMDMAP(10)=23
      COMAND(25)='TABLE   '
                            CMDMAP(25)=24
      COMAND( 9)='FREE    '
                            CMDMAP( 9)=25
      COMAND(17)='RENAME  '
                            CMDMAP(17)=26
      COMAND( 8)='FIX     '
                            CMDMAP( 8)=27
      COMAND(18)='ROUND   '
                            CMDMAP(18)=28
      COMAND(28)='VERIFY  '
                            CMDMAP(28)=29
      NCSYS = 29
C     ========== DON'T FORGET TO CHANGE DIMENSIONS OF COMAND AND CMDMAP
C
C ====== LIST OF COMMANDS IN SORT ORDER ======
C
C COMMAND    SORT ORDER  BRANCH ORDER
C -----------------------------------
C ADDRIM         1           5
C AGGREGAT       2          22
C BASIS          3          20
C BLOCK          4          14
C COUNT          5           7
C DISPLAY        6           6
C EXPLAIN        7           3
C FIX            8          27
C FREE           9          25
C GRAPH         10          23
C INTERPRT      11          19
C LIST          12           8
C PICTURE       13           9
C RATEOF        14          17
C READIN        15           1
C REDUCE        16          16
C RENAME        17          26
C ROUND         18          28
C SCHEMA        19          21
C SHOW          20          10
C SOLUTION      21          18
C SUBMAT        22           4
C SUMMARY       23           2
C SYNTAX        24          15
C TABLE         25          24
C TALLY         26          11
C TRACE         27          12
C VERIFY        28          29
C WRITEOUT      29           3
C ==================================
      RETURN
C
C ** SYSIN ENDS HERE
      END
      SUBROUTINE SYSCMD(CMD,SWVALU,NSWFLP,CLIST,FIRST,LAST,RCODE)
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
C This executes an ANALYZE command.
C
C ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C : NOTE:  CHANGES MUST ALSO BE MADE TO SYSCM0 IN RBSTACKS.FOR :
C ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
      CHARACTER*128 CLIST
      LOGICAL*1     SWVALU(0:20)
      INTEGER*2     CMDMAP(29)
      COMMON/ANLCMD/CMDMAP
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
C ADJUST COMMAND NUMBER (CMD) FOR BRANCH
      CMD = CMDMAP(CMD)
C
C IF CMD > 1, THERE MUST BE LP IN MEMORY
C
      IF(CMD.GT.1.AND.PRBNAM.EQ.' ')THEN
           PRINT *,' No LP in memory...use READIN command'
           RCODE=1
           RETURN
      ENDIF
C
C COPY SWITCH VALUES
      SWDBG  = SWVALU(0) .OR. SWDBG
      SWMSG  = SWVALU(NSWFLP)
C     ^^^^^^^^^^^^^^^^^^^^^^^FLIP SWITCHES, NOW COPY ANALYZE SWITCHES
      SWDISP = SWVALU(NSWFLP+1)
      IF(SWVALU(NSWFLP+2))THEN
           OPTNAM = 'MINIMIZE'
      ELSE
           OPTNAM = 'MAXIMIZE'
      ENDIF
      SWSYN = SWVALU(NSWFLP+3)
C
C LET GETMAT KNOW IF SENSE OF OPTIMIZATION HAS CHANGED
      CALL GSETOP(OPTNAM)
C     ______________________
C     BRANCH ON COMAND (CMD)
C
      GOTO(100,200,300,400,500,600,700,800,900,
     1    1000,1100,1200,1300,1400,1500,1600,
     2    2000,
     4    4000,4100,4200,  5000, 6000, 7000, 7100, 7500, 7600, 7700,
     5    8000,8100
     X ),CMD
           PRINT *,' SYSERR C',CMD
           STOP
C
100   CONTINUE
C READIN
      CALL AREAD(CLIST,FIRST,LAST,RCODE)
      GOTO 9000
C
200   CONTINUE
C SUMMARY
      CALL ASUMRY
      RETURN
C
300   CONTINUE
C WRITEOUT
      CALL AWRITE(CLIST,FIRST,LAST,RCODE)
      RETURN
C
400   CONTINUE
C SUBMAT
      CALL ASBSET(CLIST,FIRST,LAST,RCODE)
      RETURN
C
500   CONTINUE
C ADDRIM
      CALL ADDRIM(CLIST,FIRST,LAST,OUTPUT,RCODE)
      GOTO 9000
C
600   CONTINUE
C DISPLAY
      CALL ADSPLY(CLIST,FIRST,LAST,RCODE)
      GOTO 9000
C
700   CONTINUE
C COUNT
      CALL ACOUNT(CLIST,FIRST,LAST,OUTPUT,RCODE)
      GOTO 9000
C
800   CONTINUE
C LIST
      CALL AQLIST(CLIST,FIRST,LAST,RCODE)
      GOTO 9000
C
900   CONTINUE
C PICTURE
      CALL APICTR(CLIST,FIRST,LAST,OUTPUT,RCODE)
      GOTO 9000
C
1000  CONTINUE
C SHOW
      CALL AQSHOW(CLIST,FIRST,LAST,RCODE)
      GOTO 9000
C
1100  CONTINUE
C TALLY
      CALL ATALLY(CLIST,FIRST,LAST,RCODE)
      GOTO 9000
C
1200  CONTINUE
C TRACE
      CALL ATRACE(CLIST,FIRST,LAST,RCODE)
      GOTO 9000
C
1300  CONTINUE
C EXPLAIN
      IF( SWEXPL )THEN
         CALL EXPLAN(CLIST,FIRST,LAST,RCODE)
         GOTO 9000
      ENDIF
      PRINT *,' No syntax in memory...use READIN SYNTAX command'
      RETURN
C
1400  CONTINUE
C BLOCK
      CALL ABLOCK(CLIST,FIRST,LAST,RCODE)
      RETURN
C
1500  CONTINUE
C SYNTAX
      LINE = 1
      CALL EXSYN(CLIST,FIRST,LAST,LINE,RCODE)
      RETURN
C
1600  CONTINUE
C REDUCE
      CALL AREDUC(CLIST,FIRST,LAST,RCODE)
      RETURN
C
2000  CONTINUE
C RATEOF
      CALL ARATE(CLIST,FIRST,LAST,RCODE)
      GOTO 9000
C
4000  CONTINUE
C SOLUTION
      CALL ARDSOL(CLIST,FIRST,LAST,RCODE)
      RETURN
C
4100  CONTINUE
C INTERPRT
      CALL RULEIN(CLIST,FIRST,LAST,RCODE)
      RETURN
C
4200  CONTINUE
C BASIS
      CALL ABASIS(CLIST,FIRST,LAST,RCODE)
      RETURN
C
5000  CONTINUE
C SCHEMA
      CALL ASCHMA(CLIST,FIRST,LAST,RCODE)
      RETURN
C
6000  CONTINUE
C AGGREGAT
      CALL AGGRG8(CLIST,FIRST,LAST,RCODE)
      GOTO 9000
C
7000  CONTINUE
C GRAPH
      CALL AGRAPH(CLIST,FIRST,LAST,RCODE)
      RETURN
C
7100  CONTINUE
C TABLE
      CALL TABLE(CLIST,FIRST,LAST,RCODE)
      RETURN
C
7500  CONTINUE
C FREE
      CALL ANFREE(CLIST,FIRST,LAST,RCODE)
      GOTO 9000
C
7600  CONTINUE
C RENAME
      CALL ARENAM(CLIST,FIRST,LAST,RCODE)
      RETURN
C
7700  CONTINUE
C FIX
      CALL ANFIX(CLIST,FIRST,LAST,RCODE)
      GOTO 9000
8000  CONTINUE
C ROUND
      CALL AROUND(CLIST,FIRST,LAST,RCODE)
      GOTO 9000
8100  CONTINUE
C VERIFY
      CALL AVERFY(CLIST,FIRST,LAST,RCODE)
C
9000  CONTINUE
C SET SWITCHES FOR FLIP (MAY HAVE CHANGED)
      SWVALU(NSWFLP+1) = SWDISP
      SWVALU(NSWFLP+2) = OPTNAM.EQ.'MINIMIZE'
C
      RETURN
C
C ** SYSCMD ENDS HERE
      END
      SUBROUTINE SYSDBG
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This is debug routine entered from FLIP
C
C LOCAL
      CHARACTER*16  RNAME
      CHARACTER*4   ROWCOL
      CHARACTER*1   CHAR
C :::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
      IF(PAUSE.LT.0)READ(*,1)CHAR
      PRINT *,' ANALYZE Debug...SYS=',VERSYS,' SUB=',VERSUB,
     1        ' BLK=',VERBLK,' EXP=',VEREXP,' RUL=',VERRUL
      PAUSE = PAUSE0
C
10    PRINT *,' Amap, Lists, Numbers, Files, Versions, Submat'
     1       ,' naMes, Getmat'
      PRINT *,' Explain, Block, Rulebase, sChema, graPh, Qrc, Debug'
C
      READ(*,1)CHAR
1     FORMAT(A1)
      IF(CHAR.EQ.' ')RETURN
C
      IF(CHAR.EQ.'A')THEN
          LINE = 1
            DO 100 I=1,NROWS
               CALL FPRMPT(LINE,*900)
               CALL GETNAM('ROW ',I,RNAME)
               PRINT *,I,RNAME,AMAP(TRNEG+I),AMAP(TRPOS+I)
100         CONTINUE
      ELSE IF(CHAR.EQ.'F')THEN
            PRINT *,' MAT=',MATNAM
            PRINT *,' SOL=',SOLNAM
            PRINT *,' PCK=',PCKNAM
            PRINT *,' SYN=',SYNAM
            PRINT *,' UNITS:  DATFIL=',DATFIL,'  PCKFIL=',PCKFIL
      ELSE IF(CHAR.EQ.'N')THEN
            PRINT *,' NROWS=',NROWS,' NCOLS=',NCOLS,' NRFREE=',NRFREE
            PRINT *,' NCFIX=',NCFIX,' NONZER=',NONZER,' NVALS=',NVALS
            PRINT *,' NONES=',NONES,' MAXRLN=',MAXRLN,' MAXCLN=',MAXCLN
            PRINT *,' OBJNUM=',OBJNUM,'   VINF=',VINF
            PRINT *,' VTOLAB=',VTOLAB,'   VTOLRE=',VTOLRE
            PRINT *,' DATFIL=',DATFIL,' PCKFIL=',PCKFIL,
     1              ' NAMELN=',NAMELN
      ELSE IF(CHAR.EQ.'M')THEN
         PRINT *,' PRBNAM=',PRBNAM,' OPTNAM=',OPTNAM,' OBJNAM=',OBJNAM
         PRINT *,' RHSNAM=',RHSNAM,' RNGNAM=',RNGNAM,' BNDNAM=',BNDNAM
         PRINT *,' SOLST =',SOLST
      ELSE IF(CHAR.EQ.'E')THEN
            CALL EDEBUG
            RETURN
      ELSE IF(CHAR.EQ.'G')THEN
            CALL GDEBUG
            RETURN
      ELSE IF(CHAR.EQ.'B')THEN
            CALL BDEBUG
            RETURN
      ELSE IF(CHAR.EQ.'L')THEN
            PRINT *,' LISTS...ROW,VAL,SCR'
            LINE = 2
            DO 110 I=1,MAXLST
               CALL FPRMPT(LINE,*900)
               PRINT *,I,ROWLST(I),VALLST(I),SCRIND(I)
110         CONTINUE
      ELSE IF(CHAR.EQ.'S')THEN
            CALL ASBDBG
            RETURN
      ELSE IF(CHAR.EQ.'R')THEN
            CALL RULDBG
            RETURN
      ELSE IF(CHAR.EQ.'C')THEN
           CALL SCHDBG
      ELSE IF(CHAR.EQ.'P')THEN
           CALL GPHDBG
      ELSE IF(CHAR.EQ.'Q')THEN
610        PRINT *,' {Row | Col} name: '
           READ(*,'(A1,1X,A16)')CHAR,RNAME
           IF( CHAR.EQ.'R' )THEN
              ROWCOL = 'ROW '
           ELSE IF( CHAR.EQ.'C' )THEN
              ROWCOL = 'COL '
           ELSE
              GOTO 10
           ENDIF
           CALL GETNUM(ROWCOL,RNAME,NUMBER)
           IF( NUMBER.EQ.0 )THEN
              PRINT *,' NUMBER=0'
              GOTO 610
           ENDIF
           CALL GETRIM(ROWCOL,NUMBER,VL,VU,VC,NZ)
           PRINT *,' VL,VU,VC,NZ =',VL,VU,VC,NZ
           CALL GETSOL(ROWCOL,NUMBER,VX,VP,CHAR,STNUM)
           PRINT *,' STAT,VX,VP =',CHAR,VX,VP
           GOTO 610
      ELSE IF(CHAR.EQ.'D')THEN
           PRINT *,' Currently, SWDBG=',SWDBG
           CALL FPAUSE(PAUSE0)
           PAUSE = PAUSE0
           SWDBG = (PAUSE0.GT.0)
           PRINT *,' Now SWDBG=',SWDBG
           IF(.NOT.SWDBG)PAUSE = -PAUSE0
           RETURN
      ELSE
            PRINT *,'?',CHAR
      ENDIF
      GOTO 10
C
900   RETURN
C
C ** SYSDBG ENDS HERE
      END
