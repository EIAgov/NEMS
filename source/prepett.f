! $Header: m:/default/source/RCS/prepett.f,v 1.51 2020/07/07 18:26:50 LC2 Exp $
!
!     ETT PRE-PROCESSOR PROGRAM
!
      USE SQLITE
      IMPLICIT NONE
!
      include'parametr'
      include'ncntrl'
      include'emmparm'
      include'control'
      include'dispett'
      include'dsmdimen'
      include'dsmtoefd'
      include'dsmtfecp'
      include'dsmnemsc'
      include'dsmunits'
      include'elcntl'
      include'elout'
      include'wrenew'
      include'uefdout'
      
      
      type(sqlite_database)                      :: db
      type(sqlite_statement)                     :: stmt
      type(sqlite_statement)                     :: stmt2
      type(sqlite_statement)                     :: stmt3
      type(sqlite_column), dimension(:), pointer :: col
      type(sqlite_column), dimension(:), pointer :: col2
      type(sqlite_column), dimension(:), pointer :: col3
      character(len=40), pointer, dimension(:)   :: result
      character(len=80)                          :: errmsg
      logical                                    :: finished

      INTEGER*4 ID,DBY,DBR
      
      
      
      INTEGER*4 UF_PETT
      INTEGER ITAB,JROW,IY,EFD_SP,ECP_SP,ETT_SP,X
      INTEGER DBREC,IPARM1,IPARM2,DBCMB
      INTEGER NFTAB,IDTYP,NUMNEWREC,NUMYEARS
      external getindex,rtovalue
      integer getindex,rtovalue

      REAL DNUM(MNUMYR)
      REAL TMW,MW,CNCTIM(MNUMYR,MNUMNR+EFD_D_PROV,ETT_D_MSP), &
         CNCTEX(MNUMYR,MNUMNR+EFD_D_PROV,ETT_D_MSP),TMPCNTRCT
      REAL PIPEPCT(MNUMYR)
      REAL CNSTRNTS(ETT_D_MSP,MNUMYR,MNUMNR+EFD_D_PROV,MNUMNR+EFD_D_PROV), &
         CNTRCTS(ETT_D_MSP,MNUMNR+EFD_D_PROV,MNUMNR+EFD_D_PROV,MNUMYR), &
         FIRMTRADE(MNUMNR+EFD_D_PROV,MNUMNR+EFD_D_PROV,MNUMYR), &
         DIFF(ETT_D_MSP),PCTDFS(ETT_D_MSP)

      REAL EETIME(ETT_D_MSP), &
         IM(MNUMYR,MNUMNR),PTHRESH1(MNUMYR,MNUMNR+EFD_D_PROV, &
         MNUMNR+EFD_D_PROV), &
         PTHRESH2(MNUMYR,MNUMNR+EFD_D_PROV,MNUMNR+EFD_D_PROV), &
         EX(MNUMYR,MNUMNR)
      REAL PT1,PT2                       ! RISC CHANGE  MRD 11/01/94
      REAL XTDMMF(MNUMYR,MNUMNR),XTDMDF(MNUMYR,MNUMNR), &
         XTIMPF(MNUMYR,MNUMNR),USPRICE(MNUMYR,MNUMNR+EFD_D_PROV), &
         XTIMPD(MNUMYR,MNUMNR),EEITJ(MNUMYR,MNUMNR,ETT_D_MSP), &
         XTEXPF(MNUMYR,MNUMNR),XTEXPD(MNUMYR,MNUMNR), &
         CNPRICE(MNUMYR,MNUMNR+EFD_D_PROV), &
         MXPRICE(MNUMYR,MNUMNR+EFD_D_PROV), &
         TDLS(MNUMYR,MNUMNR),USIPRICE,INFLAT(MNUMYR),AVEFOR, &
         INFLT1(MNUMYR), &
         TOTEXPCNS(ETT_D_MSP,MNUMNR,MNUMYR), &
         TOTIMPCNS(ETT_D_MSP,MNUMNR,MNUMYR), &
         PCTEXPCNS(ETT_D_MSP,MNUMNR,MNUMYR), &
         PCTIMPCNS(ETT_D_MSP,MNUMNR,MNUMYR), &
         XRNCSTEX(ETT_D_MSP,MNUMNR,MNUMYR),TEMPC(ETT_D_MSP), &
         XRNCSTIM(ETT_D_MSP,MNUMNR,MNUMYR),CNIPRICE,MXIPRICE
      REAL FMW(MNUMYR,2,mnumnr),PMW(MNUMYR,2,mnumnr), &
         CF(MNUMYR,2,mnumnr), &
         CST(MNUMYR,2,mnumnr),CNEXPF(MNUMYR,MNUMNR), &
         CRG(MNUMYR,2,mnumnr),CNIMPF(MNUMYR,MNUMNR), &
         MXIMPF(MNUMYR,MNUMNR),XTEXDF(MNUMYR,MNUMNR), &
         MXEXPF(MNUMYR,MNUMNR),XTEXMF(MNUMYR,MNUMNR)
      REAL SPCANRGN,SPNEMS,SPRHS,SPPKRHS,SPCF,SPDOLL,SPMILLS
      REAL TSUMCAP,TDMWH,TDMW,TCFC,TFOR, &
         CSSUMCAP(ECP$CS2),CSDMWH(ECP$CS2),CSDMW(ECP$CS2), &
         CSCFC(ECP$CS2),CSFOR(ECP$CS2)
      INTEGER*4 TNUMRGS,TRGN(ECP$CS3),TMODYR,TPRJYR,TLEAD,PRJNUM, &
         CSNUMRGS(ECP$CS2),CSRGN(ECP$CS2,ECP$CS3), &
         CSMODYR(ECP$CS2),CSPRJYR(ECP$CS2),CSLEAD(ECP$CS2)
      INTEGER SPSTEP,SPYR,NEWSYR,SYR,EYR,OLDYR,MXCSYR,B1,B2
      INTEGER IRG,JYEAR(MNUMYR),JYR,NYR,ISP,YR,STRTYR,ENDYEAR 
      INTEGER I_CNTL,UF_CNSR,UF_RCNSR,UF_CAN,IYR,CS_SW,UF_CST,CSTRGN
      INTEGER I,J,K,Y,L,JRG,IRGEX,IRGIM,IRECL,IREC,IFTAB
      INTEGER RGIMP,RGEXP,SEASN,ICOUNT,YI,YE,INTRG1,INTRG2,USECODE,PHOUTCODE,DIRSW
      INTEGER  NUMEXPCNS(ETT_D_MSP,MNUMNR,MNUMYR), &
               NUMIMPCNS(ETT_D_MSP,MNUMNR,MNUMYR)
      LOGICAL WHOOPS ! error flag
      CHARACTER*145 DUMMY
      CHARACTER*80 FILENM,LINE
      CHARACTER*5 TEMPI,TEMPE
      CHARACTER*4 RECTYP
      CHARACTER*5 RECTYPE
      CHARACTER*1 SN
      CHARACTER*2 S
      CHARACTER*26 TPRJNAME,CSPRJNAME(ECP$CS2),IRGIMDESC,IRGEXDESC
      CHARACTER*40 IMPRGDESC,EXPRGDESC,FTYPE,SOURCE
      CHARACTER*18 TPROV,CSPROV(ECP$CS2)
!
      REAL FRMFCST(MNUMYR,MNUMNR+EFD_D_PROV)
      REAL FRMVCST(MNUMYR,MNUMNR+EFD_D_PROV)
      REAL CAPDMDF(MNUMYR,MNUMNR+EFD_D_PROV)
      REAL CAPIMPD(MNUMYR,MNUMNR+EFD_D_PROV)
      REAL CAPEXPD(MNUMYR,MNUMNR+EFD_D_PROV)
      REAL FLDMDF(MNUMYR,MNUMNR+EFD_D_PROV)
      REAL FLIMPD(MNUMYR,MNUMNR+EFD_D_PROV)
      REAL FLEXPD(MNUMYR,MNUMNR+EFD_D_PROV)
      REAL FLIMPF(MNUMYR,MNUMNR+EFD_D_PROV)
      REAL FLEXPF(MNUMYR,MNUMNR+EFD_D_PROV)
      REAL IMPCAP(MNUMYR,MNUMNR)
      REAL EXPCAP(MNUMYR,MNUMNR)
      REAL phsout(MNUMYR)
      REAL phsout22(MNUMYR)
      REAL phsca(MNUMYR)
!
!     VARIABLES TO CALL FILE_MGR FOR INITIALIZATION
!
      CHARACTER*18 FM_NAME/'FILELIST'/
      INTEGER ISTATUS,IFILE
      LOGICAL NEW/.FALSE./
      INTEGER FILE_MGR
      !INTEGER res1,n 
      INTEGER DUMMY1,DUMMY2,p
      Real FRMFCST_k1(MNUMNR+EFD_D_PROV),FRMVCST_k1(MNUMNR+EFD_D_PROV)
      EXTERNAL FILE_MGR
!
!     CALL FILE_MGR FIRST TIME TO OPEN UP AND READ THE LIST OF FILES
!
      ISTATUS=FILE_MGR('I',FM_NAME,NEW)
!
      DO Y=1, MNUMYR
        phsout22(Y) = 1.0
        phsout(Y) = 1.0
        phsca(Y) = 1.0
      ENDDO

      phsout22(1) = 0.7
      phsout22(2) = 0.7
      phsout22(3) = 0.7
      phsout22(4) = 0.7
      phsout22(5) = 0.7
      phsout22(6) = 0.7
      phsout22(7) = 0.7
      phsout22(8) = 0.7
      phsout22(9) = 0.7
      phsout22(10) = 0.8
      phsout22(11) = 0.8
      phsout22(12) = 0.8
      phsout22(13) = 0.9
      phsout22(14) = 0.9
      phsout22(15) = 0.9
      phsout22(16) = 1.0
      phsout22(17) = 1.0

      phsout(1) = 0.9
      phsout(2) = 0.8
      phsout(3) = 0.7
      phsout(4) = 0.6
      phsout(5) = 0.5
      phsout(6) = 0.4
      phsout(7) = 0.3
      phsout(8) = 0.2
      phsout(9) = 0.1
      phsout(10) = 0.0

      NYR = MNUMYR
      AVEFOR = .91
      USIPRICE = 35
      CNIPRICE = 35
      MXIPRICE = 35

      NEW=.TRUE.
      FILENM='LDSMRPT'
      IMSG=FILE_MGR('O',FILENM,NEW)  !Open LDSM REPORT FILE
      NEW=.FALSE.
      FILENM='LDSMDAF'
      IODB=FILE_MGR('O',FILENM,NEW)  !Open DAF-LSR-DB
      NEW=.FALSE.
!      FILENM='LDSMCRS'
!      IOCR=FILE_MGR('O',FILENM,NEW)  !Open COMMERCIAL RESTART FILE
!      FILENM='LDSMRRS'
!      IORR=FILE_MGR('O',FILENM,NEW)  !Open RESIDENTIAL RESTART FILE
!
      CALL DSMRST(WHOOPS) ! Read structure file and DSM option database
      IF(WHOOPS) THEN
         WRITE(6,*)'<))) Data passed by LDSM may be CORRUPTED'
      ENDIF
 
!     OPEN AND READ THE CONTROL FILE
!
      I_CNTL=80
      CALL RDCNTRL(I_CNTL)
      NEW=.TRUE.
!
      NEW=.TRUE.
      FILENM='PREPDB'
      UF_PETT=FILE_MGR('O',FILENM,NEW)
!
      DO I = 1 , ETTns
         EETIME(I) = DBLE(0.0)
         B1 = ETTSEDEF(I,1)
         B2 = ETTSEDEF(I,2)
         DO J = B1 , B2
            EETIME(I) = EETIME(I) + ETTblWidth(J)
         END DO
      END DO
!
      WRITE(UF_PETT,7091)'EETIME 1-6 =',(EETIME(I),I=1,ETTns)
 7091 FORMAT(A15,10(F12.4))
!

      NEW=.TRUE.
      FILENM='ETTDEM'
      UF_ETDM=FILE_MGR('O',FILENM,NEW)
!
      NEW=.TRUE.
      FILENM='ETTIN'
      UF_RCNSR=FILE_MGR('O',FILENM,NEW)
!
      WRITE(UF_PETT,3877)'REGIONS', &
         (URGNUM(I),'*',URGNME(I)(1:4),'*',I=1,UNRGNS)
 3877 FORMAT(A8,13(/,I4,A1,A4,A1))
!
!     CALCULATE PRICE OVER TIME
!
      DO I = 1 , MNUMYR
          INFLAT(I)=1.0
          INFLT1(I)=1.0
      END DO
!
      DO I = 1 , MNUMYR
         DO J = 1 , MNUMNR + EFD_D_PROV
             USPRICE(I,J) = USIPRICE*INFLAT(I)
             CNPRICE(I,J) = CNIPRICE*INFLAT(I)
             MXPRICE(I,J) = MXIPRICE*INFLAT(I)
         END DO
      END DO

      DO 6400 I=1,MNUMYR
         JYEAR(I)=(USYEAR(1)-1)+I                  !03/17/94
 6400 CONTINUE


! INITIALIZE
        DO 9000 I=1,MNUMYR

             PIPEPCT(I)=1.0

        IF (USW_XP .GT.0) THEN
!  TDLS FACTORS FOR CANADA
             TDLS(I,1)=0.0
             TDLS(I,2)=0.0
             TDLS(I,3)=0.0
             TDLS(I,4)=0.0
             TDLS(I,5)=0.0
             TDLS(I,6)=0.0
             TDLS(I,7)=0.0
             TDLS(I,8)=0.0
             TDLS(I,9)=0.0
             TDLS(I,10)=0.0
             TDLS(I,11)=0.0
             TDLS(I,12)=0.0
             TDLS(I,13)=0.0
             TDLS(I,14)=0.0
             TDLS(I,15)=0.0
             TDLS(I,16)=0.0
             TDLS(I,17)=0.0
             TDLS(I,18)=0.0
             TDLS(I,19)=0.0
             TDLS(I,20)=0.0
             TDLS(I,21)=0.0
             TDLS(I,22)=0.0
        ENDIF

          DO 9008 J=1,MNUMNR
            XTDMMF(I,J)=0
            XTDMDF(I,J)=0
            XTIMPF(I,J)=0
            XTIMPD(I,J)=0
            XTEXPF(I,J)=0
            XTEXPD(I,J)=0
            XTEXMF(I,J)=0
            XTEXDF(I,J)=0
            CNIMPF(I,J)=0
            MXIMPF(I,J)=0
            CNEXPF(I,J)=0
            MXEXPF(I,J)=0
              EX(I,J)=0
              IM(I,J)=0
              IMPCAP(I,J)=0
              EXPCAP(I,J)=0
             DO 9015 K=1,ETT_D_MSP
                EEITJ(I,J,K)=0
                XRNCSTEX(K,J,I)=0
                XRNCSTIM(K,J,I)=0
                TOTEXPCNS(K,J,I)=0
                TOTIMPCNS(K,J,I)=0
                NUMEXPCNS(K,J,I)=0
                NUMIMPCNS(K,J,I)=0
                PCTEXPCNS(K,J,I)=0
                PCTIMPCNS(K,J,I)=0
9015    CONTINUE
9008    CONTINUE

         DO 9200 J=1,MNUMNR+EFD_D_PROV
          FRMVCST(I,J)=0
          FRMFCST(I,J)=0
          CAPDMDF(I,J)=0
          CAPIMPD(I,J)=0
          CAPEXPD(I,J)=0
          FLDMDF(I,J)=0
          FLIMPD(I,J)=0
          FLEXPD(I,J)=0
          FLIMPF(I,J)=0
          FLEXPF(I,J)=0
9200     CONTINUE

            DO 9005 J=1,MNUMNR+EFD_D_PROV
             DO 9010 L=1,MNUMNR+EFD_D_PROV
              DO 9016 K=1,ETT_D_MSP
                CNSTRNTS(K,I,J,L)=0
                CNSTRNTS_PREFIRM(K,I,J,L)=0
                CNTRCTS(K,J,L,I)=0
                CNCTIM(I,J,K)=0
                CNCTEX(I,J,K)=0
9016        CONTINUE
9010       CONTINUE
9005    CONTINUE

9000    CONTINUE
         firmtrade = 0.0
         utfirm = 0.0

        DO 9222 I=1,MNUMNR+EFD_D_PROV

         DO 9223 J=1,MNUMNR+EFD_D_PROV
           DO 9224 K=1,MNUMYR                           !03/08/94
             PTHRESH1(K,I,J)=0.5
             PTHRESH2(K,I,J)=4.38
9224       CONTINUE
9223     CONTINUE
9222    CONTINUE

        DO 9100 I=1,MNUMYR
         DO 9110 J=1,ECP_D_CIS
          DO 9120 K=1,MNUMNR                        !02/07/94
            FMW(I,J,K)=0
            PMW(I,J,K)=0
            CF(I,J,K)=0
            CST(I,J,K)=0
            CRG(I,J,K)=0
9120     CONTINUE
9110    CONTINUE
9100    CONTINUE
!
!   READ AND STORE demand and energy costs for firm contracts
!

        call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )
        allocate ( col(5) )
        ! QUERY THE DATABASE TABLE
        call sqlite3_column_query( col(1), 'ID', sqlite_int )
        call sqlite3_column_query( col(2), 'EMM_REG', sqlite_int )
        call sqlite3_column_query( col(3), 'YEAR', sqlite_int )
        call sqlite3_column_query( col(4), 'FRMFCST', sqlite_real )
        call sqlite3_column_query( col(5), 'FRMVCST', sqlite_real )       
        call sqlite3_prepare_select( db, 'V_EMM_FRMCHRG', col, stmt)              
        ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
        do          
          call sqlite3_next_row( stmt, col, finished )  
          if ( finished ) exit           
          call sqlite3_get_column( col(1), ID)
          call sqlite3_get_column( col(2), DBR)
          call sqlite3_get_column( col(3), DBY)
          call sqlite3_get_column( col(4), FRMFCST(DBY,DBR))    
          call sqlite3_get_column( col(5), FRMVCST(DBY,DBR))    
          write(6,*) 'FRMCHRG_km1 ',DBY,DBR,FRMFCST(DBY,DBR)
          write(6,*) 'FRMCHRG_km2 ',DBY,DBR,FRMVCST(DBY,DBR)
        ENDDO        
        deallocate ( col )       
        call sqlite3_close( db )               

 !      NEW=.FALSE.
 !      FILENM='FRMCHRG'
 !      UF_CST=FILE_MGR('O',FILENM,NEW)

 !      DO I = 1 , MNUMNR + EFD_D_PROV
 !         READ(UF_CST,*)DUMMY,(FRMFCST(J,I),J=1,MNUMYR)
 !      END DO
 !      DO I = 1 , MNUMNR + EFD_D_PROV
 !         READ(UF_CST,*)DUMMY,(FRMVCST(J,I),J=1,MNUMYR)
 !      END DO

       IF (UNRGNS.EQ.13) THEN
         DO J=1,MNUMYR
          DO I=MNUMNR+EFD_D_PROV,MNUMNR-2
              X=I-9
              FRMFCST(J,I) = FRMFCST(J,X)   
              FRMVCST(J,I) = FRMVCST(J,X)            
          ENDDO
          DO I=MNUMNR-2,MNUMNR-3
              FRMFCST(J,I)=0.0
              FRMVCST(J,I)=0.0              
          ENDDO
        ENDDO
       ENDIF
              
       DO I = 1 , MNUMYR
          DO J = 1 , MNUMNR + EFD_D_PROV
             FRMFCST(I,J)=FRMFCST(I,J)*INFLAT(I) * 1000
             FRMVCST(I,J)=FRMVCST(I,J)*INFLAT(I) * 1000
          END DO
       END DO
!
!      READ AND STORE CANADIAN SUPPLY DATA FILE
!

       DO I=1,ECP$CS2
         UCI$CAP(I)=0.0
         UCI$MWH(I)=0.0
         UCI$DMW(I)=0.0
         UCI$CFC(I)=0.0
         UCI$FOR(I)=0.0
         UCI$RGN(I)=0
         MODYR(I)=0
         PROJYR(I)=0
         LEAD(I)=0
         PNAME(I)='           '
         PROVINCE(I)='        '
         DO J=1,ECP$CS3
          UCI$RGS(I,J)=0
         END DO
       END DO

       IF ( USW_CANACC .GT. 0 ) THEN

         NEW=.FALSE.
         FILENM='CANSPLY'
         UF_CAN=FILE_MGR('O',FILENM,NEW)
         I = 0
         SPCANRGN = 0.0
         DO WHILE (SPCANRGN .LT. 99.0)
            I = I + 1
            READ(UF_CAN,*,END=799)SPCANRGN,SPNEMS,SPSTEP,SPYR,SPRHS, &
             SPPKRHS,SPCF,SPDOLL,SPMILLS
890       FORMAT(I4,1X,2(F3.0,1X),I4,1X,I4,1X,5(F8.4,1X))

           IF (UNRGNS.EQ.13) THEN
             SPCANRGN = SPCANRGN+9
           ENDIF
          
           IF (SPNEMS .GT. 0.0) THEN
             YR=SPYR-(USYEAR(1)-1)
             FMW(YR,SPSTEP,SPNEMS)=SPRHS
             PMW(YR,SPSTEP,SPNEMS)=SPPKRHS
             CF (YR,SPSTEP,SPNEMS)=SPCF
             CST(YR,SPSTEP,SPNEMS)=SPMILLS
             CRG(YR,SPSTEP,SPNEMS)=SPCANRGN
           END IF
         END DO

          WRITE(UF_PETT,*)'New Canadian Supply File'
          PRJNUM=0
          READ(UF_CAN,*)DUMMY
          WRITE(UF_PETT,*)DUMMY

          DO 9151 I=1,ECP$CS2

            READ(UF_CAN,889,END=799)TPRJNAME,TPROV,TSUMCAP,TLEAD,TMODYR, &
               TPRJYR,TCFC,TDMW,TDMWH,TFOR,TNUMRGS,(TRGN(J),J=1,5)
889           FORMAT(1X,A22,A11,1X,F6.0,7X,I2,5X,I4,5X,I4,4X,F5.3, &
               3X,F6.0,1X,F8.3,4X,F5.3,8X,I1,5(7X,I2))
             

          PRJNUM=PRJNUM+1
          UCI$CAP(PRJNUM)=TSUMCAP
          UCI$MWH(PRJNUM)=TDMWH
          UCI$DMW(PRJNUM)=TDMW
          UCI$CFC(PRJNUM)=TCFC
          UCI$FOR(PRJNUM)=TFOR
          UCI$RGN(PRJNUM)=TNUMRGS
          MODYR(PRJNUM)=TMODYR
          PROJYR(PRJNUM)=TPRJYR
          LEAD(PRJNUM)=TLEAD
          PNAME(PRJNUM)=TPRJNAME
          PROVINCE(PRJNUM)=TPROV
          DO J=1,ECP$CS3
            UCI$RGS(PRJNUM,J)=TRGN(J)
          END DO

9151      CONTINUE

799      CONTINUE

       ENDIF                   ! end if allowing acceleration of canadian projects




      call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )
        allocate ( col3(12) )
        ! QUERY THE DATABASE TABLE
        call sqlite3_column_query( col3(1), 'ID', sqlite_int )
        call sqlite3_column_query( col3(2), 'IRGEX', sqlite_int )
        call sqlite3_column_query( col3(3), 'IRGEXDESC', sqlite_char )
        call sqlite3_column_query( col3(4), 'IRGIM', sqlite_int )
        call sqlite3_column_query( col3(5), 'IRGIMDESC', sqlite_char )
        call sqlite3_column_query( col3(6), 'DIRSW', sqlite_int )
        call sqlite3_column_query( col3(7), 'STRTYR', sqlite_int )
        call sqlite3_column_query( col3(8), 'ENDYEAR', sqlite_int )
        call sqlite3_column_query( col3(9), 'PT1', sqlite_real )
        call sqlite3_column_query( col3(10), 'PT2', sqlite_real )
        call sqlite3_column_query( col3(11), 'TEMPC1', sqlite_real )
        call sqlite3_column_query( col3(12), 'TEMPC2', sqlite_real )        
        call sqlite3_prepare_select( db, 'V_EMM_PRETTIN', col3, stmt3)       
        ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
        do        
          call sqlite3_next_row( stmt3, col3, finished )
          if ( finished ) exit      
          call sqlite3_get_column( col3(1), ID)
          call sqlite3_get_column( col3(2), IRGEX)
          call sqlite3_get_column( col3(3), IRGEXDESC)
          call sqlite3_get_column( col3(4), IRGIM)
          call sqlite3_get_column( col3(5), IRGIMDESC)
          call sqlite3_get_column( col3(6), DIRSW)
          call sqlite3_get_column( col3(7), STRTYR)
          call sqlite3_get_column( col3(8), ENDYEAR)
          call sqlite3_get_column( col3(9), PT1)
          call sqlite3_get_column( col3(10), PT2)
          call sqlite3_get_column( col3(11), TEMPC(1))
          call sqlite3_get_column( col3(12), TEMPC(2))
!
        write(6,*)  ' prettin read',irgex,irgexdesc,irgim,irgimdesc,dirsw,strtyr,endyear,pt1,pt2,tempc(1),tempc(2)
          
        IF ( ENDYEAR .EQ. 9999) THEN 
          EYR = MNUMYR
        ELSE
          EYR = ENDYEAR - BASEYR + 1
        ENDIF
   
        SYR = STRTYR - BASEYR + 1

        DO IYR = SYR,EYR
          PTHRESH1(IYR,IRGEX,IRGIM) = PT1
          PTHRESH2(IYR,IRGEX,IRGIM) = PT2

          DO ISP=1,ETTns
            CNSTRNTS(ISP,IYR,IRGIM,IRGEX)=CNSTRNTS(ISP,IYR,IRGIM,IRGEX)+ &
               TEMPC(ISP)/1000.0
            CNSTRNTS_PREFIRM(ISP,IYR,IRGIM,IRGEX)=CNSTRNTS_PREFIRM(ISP,IYR,IRGIM,IRGEX)+ &
               TEMPC(ISP)/1000.0
!  If constraint is for both directions (2 directional) than create constraints for both directions
            IF ( DIRSW .EQ. 2 ) THEN
              CNSTRNTS(ISP,IYR,IRGEX,IRGIM)=CNSTRNTS(ISP,IYR,IRGEX,IRGIM)+ &
                 TEMPC(ISP)/1000.0
              CNSTRNTS_PREFIRM(ISP,IYR,IRGEX,IRGIM)=CNSTRNTS_PREFIRM(ISP,IYR,IRGEX,IRGIM)+ &
                 TEMPC(ISP)/1000.0
            ENDIF
          ENDDO
       ENDDO

        end do                
        deallocate ( col3 )              
        call sqlite3_close( db )
        
       DO  Y=1,MNUMYR
         DO I=1,MNUMNR+EFD_D_PROV
            DO J=1,MNUMNR+EFD_D_PROV
                PTHRESH2(Y,I,J)=PTHRESH2(Y,I,J)/8760*1000
            ENDDO
         ENDDO
       ENDDO  

445         FORMAT(A35,1X,3I4,F8.3)

! LOAD HIGHEST YEARS CONSTRAINTS INTO REMAINING MODEL YEARS
!
!       DO I=MXCSYR+1,MNUMYR
!         DO J=1,MNUMNR+EFD_D_PROV
!           DO K=1,MNUMNR+EFD_D_PROV
!             DO ISP=1,ETTns
!               CNSTRNTS(ISP,I,J,K)=CNSTRNTS(ISP,MXCSYR,J,K)
!             ENDDO
!           ENDDO
!         ENDDO
!       ENDDO

! DETERMINE REGION'S LARGEST IMPORT AND EXPORT TRANSMISSION CONSTRAINT
! CHANGED 6/27/96 TO FIND 75% of the total IMP AND EXP CONSTRAINTs
!      for each unrgn

      DO J=1,MNUMYR
        DO JRG=1,UNRGNS
          DO K=1,MNUMNR
            DO  I=1,MNUMNR
             IF(I.EQ.JRG) THEN

              DO ISP=1,ETTns
                XRNCSTEX(ISP,JRG,J)=MAX(XRNCSTEX(ISP,JRG,J), &
                                        CNSTRNTS(ISP,J,K,I))
                IF (CNSTRNTS(ISP,J,K,I) .GT. 0.0) THEN
                  TOTEXPCNS(ISP,JRG,J) = TOTEXPCNS(ISP,JRG,J) + &
                                          CNSTRNTS(ISP,J,K,I)
                  NUMEXPCNS(ISP,JRG,J) = NUMEXPCNS(ISP,JRG,J) + 1
                ENDIF

              ENDDO   
             ENDIF

             IF (K.EQ.JRG) THEN
               DO ISP=1,ETTns
                XRNCSTIM(ISP,JRG,J)=MAX(XRNCSTIM(ISP,JRG,J), &
                                        CNSTRNTS(ISP,J,K,I))

                IF (CNSTRNTS(ISP,J,K,I) .GT. 0.0) THEN
                  TOTIMPCNS(ISP,JRG,J) = TOTIMPCNS(ISP,JRG,J) + &
                                          CNSTRNTS(ISP,J,K,I)
                  NUMIMPCNS(ISP,JRG,J) = NUMIMPCNS(ISP,JRG,J) + 1
                ENDIF

              ENDDO
             ENDIF

            ENDDO
          ENDDO
        ENDDO
      ENDDO

7184 FORMAT(2(A4,I4),2(A9,1X,F8.2))
9174 FORMAT(A22,1X,5I4,F8.3,1X,A14,F12.3,I5,F12.3)

        DO J=1,MNUMYR
         DO JRG=1,UNRGNS
          DO ISP=1,ETTns
            PCTEXPCNS(ISP,JRG,J) = TOTEXPCNS(ISP,JRG,J)*0.75
            PCTIMPCNS(ISP,JRG,J) = TOTIMPCNS(ISP,JRG,J)*0.75
          ENDDO
         ENDDO
        ENDDO

9715 FORMAT(A28,A19,1X,3I4,1X,4F12.3)

!  LOAD HIGHEST YEARS PRICE THRESHOLDS INTO REMAINING MODEL YEARS

!        DO 5040 Y=MXCSYR+1,MNUMYR                                 !03/17/94
!          DO 5044 I=1,MNUMNR+EFD_D_PROV
!           DO 5046 J=1,MNUMNR+EFD_D_PROV
!             PTHRESH1(Y,I,J) = PTHRESH1(MXCSYR,I,J)
!             PTHRESH2(Y,I,J) = PTHRESH2(MXCSYR,I,J)
!         ENDDO
!        ENDDO
!       ENDDO

!  ESCALATE PRICE THRESHOLDS

        DO Y=1,MNUMYR
          DO I=1,MNUMNR+EFD_D_PROV
            DO J=1,MNUMNR+EFD_D_PROV
              PTHRESH1(Y,I,J)=PTHRESH1(Y,I,J)*INFLT1(Y)
              PTHRESH2(Y,I,J)=PTHRESH2(Y,I,J)*INFLAT(Y)
            ENDDO
          ENDDO
        ENDDO

446 FORMAT(A35,1X,3I4,2F8.3)



        !V_EMM_ETCNCT data read in------------------------------------------- 
        call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )
        allocate ( col2(11) )
        ! QUERY THE DATABASE TABLE
        call sqlite3_column_query( col2(1), 'ID', sqlite_int )
        call sqlite3_column_query( col2(2), 'SOURCE', sqlite_char )
        call sqlite3_column_query( col2(3), 'S', sqlite_char )
        call sqlite3_column_query( col2(4), 'RECTYPE', sqlite_char )
        call sqlite3_column_query( col2(5), 'USECODE', sqlite_int )
        call sqlite3_column_query( col2(6), 'PHOUTCODE', sqlite_int )
        call sqlite3_column_query( col2(7), 'RGIMP', sqlite_int )
        call sqlite3_column_query( col2(8), 'RGEXP', sqlite_int )
        call sqlite3_column_query( col2(9), 'STRTYR', sqlite_int )
        call sqlite3_column_query( col2(10), 'ENDYEAR', sqlite_int )
        call sqlite3_column_query( col2(11), 'TMPCNTRCT', sqlite_real )        
        call sqlite3_prepare_select( db, 'V_EMM_ETCNCT', col2, stmt2)            
        ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
        do       
          call sqlite3_next_row( stmt2, col2, finished )
          if ( finished ) exit     
          call sqlite3_get_column( col2(1), ID)
          call sqlite3_get_column( col2(2), SOURCE)
          call sqlite3_get_column( col2(3), S)
          call sqlite3_get_column( col2(4), RECTYPE)
          call sqlite3_get_column( col2(5), USECODE)
          call sqlite3_get_column( col2(6), PHOUTCODE)
          call sqlite3_get_column( col2(7), RGIMP)
          call sqlite3_get_column( col2(8), RGEXP)
          call sqlite3_get_column( col2(9), STRTYR)
          call sqlite3_get_column( col2(10), ENDYEAR)
          call sqlite3_get_column( col2(11), TMPCNTRCT)




! Use contract?
         IF ( USECODE .NE. 0 ) THEN
!
!          LOAD CONTRACT DATA INTO ARRAYS DIMENSIONED YEAR,RG,SEASN AND INTO
!          ARRAYS DIMENSIONED RGIMPORT,RGEXPORT,YEAR

           YI = STRTYR - BASEYR + 1
           YE = ENDYEAR - BASEYR + 1  
    write(6,*) ' yi,ye ',yi,ye

           YI = MAX(YI,1)
           YE = MIN(YE,MNUMYR)
    write(6,*) ' after yi ye ',yi,ye

!
!  If phaseout code equal 1 then phase out contract starting in endyr of contract + 1
!
             DO Y = YI, YE + 10

              IF ( Y .LE. MNUMYR ) THEN

                 MW = TMPCNTRCT 

                 IF (Y .GT. YE) THEN
               write(6,*) ' inside y gt ye '
                   IF ( PHOUTCODE .EQ. 1 ) THEN
                    write(6,*) ' inside phoutcode if ye y ',ye,y
                     MW = MW * PHSOUT(Y-YE)
                   ELSE
                     MW = 0.0
                   ENDIF
                 ENDIF

                 IF ( (S .EQ.'S ') .OR. (S .EQ. ' S') .OR. (S .EQ. 'WS') .OR. (S .EQ. 'SW') ) THEN
                   write(6,*) ' summer y rgimp rgexp mw ',y,rgimp,rgexp,mw
                   CNCTIM(Y,RGIMP,2) = CNCTIM(Y,RGIMP,2) + (MW/1000)
                   CNCTEX(Y,RGEXP,2) = CNCTEX(Y,RGEXP,2) + (MW/1000)
                   CNTRCTS(2,RGIMP,RGEXP,Y) = CNTRCTS(2,RGIMP,RGEXP,Y) + (MW/1000)
                 ENDIF

                 IF ( (S .EQ.'W ') .OR. (S .EQ. ' W') .OR. (S .EQ. 'WS') .OR. (S .EQ. 'SW') ) THEN
                   write(6,*) ' winter y rgimp rgexp mw ',y,rgimp,rgexp,mw
                   CNCTIM(Y,RGIMP,1) = CNCTIM(Y,RGIMP,1) + (MW/1000)
                   CNCTEX(Y,RGEXP,1) = CNCTEX(Y,RGEXP,1) + (MW/1000)
                   CNTRCTS(1,RGIMP,RGEXP,Y) = CNTRCTS(1,RGIMP,RGEXP,Y) + (MW/1000)
                 ENDIF

              ENDIF

             ENDDO
          ENDIF
        
        end do                
        deallocate ( col2 )              
        call sqlite3_close( db )


!     write(6,3333) (cnctim(y,20,1),y=21,41)
!     write(6,3333) (cnctim(y,20,2),y=21,41)
!3333 format(1h ,'!caim',21f8.1)
!     write(6,4444) (cnctex(y,20,1),y=21,41)
!     write(6,4444) (cnctex(y,20,2),y=21,41)
!4444 format(1h ,'!caex',21f8.1)

      PRINT *,'after contracts'

      DUMMY='D1'


!  INTERPOLATE 1993 CONTRACT AND CONSTRAINT VALUES (FOR NEMS AEO95)

         DO 204 ISP=1,ETTns
            DO 2045 I=1,MNUMNR+EFD_D_PROV

            CNCTEX(4,I,ISP) = (CNCTEX(3,I,ISP) + CNCTEX(5,I,ISP)) / 2
            CNCTIM(4,I,ISP) = (CNCTIM(3,I,ISP) + CNCTIM(5,I,ISP)) / 2

          DO 205 J=1,MNUMNR+EFD_D_PROV
            CNTRCTS(ISP,I,J,4) = (CNTRCTS(ISP,I,J,3) + &
                                   CNTRCTS(ISP,I,J,5))  / 2
            CNSTRNTS(ISP,4,I,J) = (CNSTRNTS(ISP,3,I,J) + &
                                   CNSTRNTS(ISP,5,I,J)) / 2

205      CONTINUE
2045      CONTINUE
204    CONTINUE

2041  FORMAT(3(A20,1X,F6.2),3X,3(A10,1X,I3))

      DUMMY='D2'

          DO 8000 I=1,MNUMYR
           DO 8005 J=1,UNRGNS

! LOAD INTERNATIONAL IMPORTS AND EXPORTS REPORTWRITER VARIALBES
!   FROM CONTRACTS
       DO 8022 INTRG1=MNUMNR+1,MNUMNR+EFD_D_PROV
         DO 8002 ISP=1,ETTns

             FLIMPF(I,J)=FLIMPF(I,J)+ (1000*( &
         ( CNTRCTS(ISP,J,INTRG1,I)*AVEFOR * EETIME(ISP) ) ) )

             FLEXPF(I,J)=FLEXPF(I,J)+ (1000*( &
         ( CNTRCTS(ISP,INTRG1,J,I)*AVEFOR * EETIME(ISP) ) ) )

             FLIMPD(I,J)=FLIMPD(I,J)+ (1000*( &
         ( CNTRCTS(ISP,J,INTRG1,I)*AVEFOR * EETIME(ISP) ) ) ) &
                     *FRMVCST(I,INTRG1) /1000000

             FLEXPD(I,J)=FLEXPD(I,J)+ (1000*( &
         ( CNTRCTS(ISP,INTRG1,J,I)*AVEFOR * EETIME(ISP) ) ) ) &
                     *FRMVCST(I,J)     /1000000

             CAPIMPD(I,J)=CAPIMPD(I,J) + &
                          CNTRCTS(ISP,J,INTRG1,I)*1000 &
                          * FRMFCST(I,INTRG1)

             CAPEXPD(I,J)=CAPEXPD(I,J) + &
                          CNTRCTS(ISP,INTRG1,J,I)*1000 &
                          * FRMFCST(I,J)

             FIRMTRADE(J,INTRG1,I)=FIRMTRADE(J,INTRG1,I)+ (1000*( &
                       ( CNTRCTS(ISP,J,INTRG1,I)*AVEFOR * EETIME(ISP) ) ) )
             FIRMTRADE(INTRG1,J,I)=FIRMTRADE(INTRG1,J,I)+ (1000*( &
                       ( CNTRCTS(ISP,INTRG1,J,I)*AVEFOR * EETIME(ISP) ) ) )

             IF (INTRG1 .LT. MNUMNR+EFD_D_PROV) THEN
              CNIMPF(I,J)=CNIMPF(I,J)+ (1000*( &
          ( CNTRCTS(ISP,J,INTRG1,I)*AVEFOR * EETIME(ISP) ) ) )

              CNEXPF(I,J)=CNEXPF(I,J)+ (1000*( &
          ( CNTRCTS(ISP,INTRG1,J,I)*AVEFOR * EETIME(ISP) ) ) )

             ELSE
              MXEXPF(I,J)=MXEXPF(I,J)+ (1000*( &
          ( CNTRCTS(ISP,INTRG1,J,I)*AVEFOR * EETIME(ISP) ) ) )

              MXIMPF(I,J)=MXIMPF(I,J)+ (1000*( &
          (CNTRCTS(ISP,J,INTRG1,I)*AVEFOR * EETIME(ISP) ) ) )
             ENDIF

8002     CONTINUE
8022   CONTINUE

              XTIMPF(I,J) = CNIMPF(I,J) + MXIMPF(I,J)
                 ULFIME(J) = XTIMPF(I,J) * 0.001
              XTEXPF(I,J) = CNEXPF(I,J) + MXEXPF(I,J)
                 ULFIXE(J) = XTEXPF(I,J) * 0.001
              XTIMPD(I,J)=(CNIMPF(I,J)*CNPRICE(I,J) + &
                           MXIMPF(I,J)*MXPRICE(I,J)) /1000000
                 ULFIMR(J) = XTIMPD(I,J) * 0.001
              XTEXPD(I,J)=(XTEXPF(I,J)*USPRICE(I,J))/1000000
                 ULFIXR(J) = XTEXPD(I,J) * 0.001

! SUMMARIZE CONTRACT DOMESTIC IMPORTS AND EXPORTS (generation-IM,EX)
!                                                (and capacity-IMPCAP,EXPCAP)
            DO 8004 K=1,UNRGNS
             DO 8003 ISP=1,ETTns

                 IM(I,J)=IM(I,J)+ (1000*( &
         ( CNTRCTS(ISP,J,K,I)*AVEFOR * EETIME(ISP) ) ) )
                 EX(I,J)=EX(I,J)+ (1000*( &
         ( CNTRCTS(ISP,K,J,I)*AVEFOR * EETIME(ISP) ) ) )

                 IMPCAP(I,J)=IMPCAP(I,J) + 1000* &
                                    CNTRCTS(ISP,J,K,I)*AVEFOR
                 EXPCAP(I,J)=EXPCAP(I,J) + 1000* &
                                    CNTRCTS(ISP,K,J,I)*AVEFOR
                 FIRMTRADE(J,K,I)=FIRMTRADE(J,K,I)+ (1000*( &
                                  ( CNTRCTS(ISP,J,K,I)*AVEFOR * EETIME(ISP) ) ) )
!                FIRMTRADE(K,J,I)=FIRMTRADE(K,J,I)+ (1000*( &
!        ( CNTRCTS(ISP,K,J,I)*AVEFOR * EETIME(ISP) ) ) )

8003     CONTINUE
8004   CONTINUE


             DO 8011 K=1,ETTns

! LOAD EEITJ VARIABLE WITH NET IMPORTS (FROM CONTRACTS) DERATED
!  BY AVERAGE FORCED OUTAGE RATE  (INCLUDES CANADA)

         EEITJ(I,J,K)=(CNCTEX(I,J,K)-CNCTIM(I,J,K))*AVEFOR
8011         CONTINUE

! LOAD REPORTWRITE VARIABLES FOR DOMESTIC FIRM CONTRACTS - MWH AND MM$

        XTDMMF(I,J)=XTDMMF(I,J) + (EX(I,J)-IM(I,J))
        XTDMDF(I,J)=(XTDMMF(I,J)*USPRICE(I,J))/1000000
        FLDMDF(I,J)=(XTDMMF(I,J)*FRMVCST(I,J))/1000000
        CAPDMDF(I,J)=((EXPCAP(I,J)-IMPCAP(I,J)) * FRMFCST(I,J)) &
                                              /1000000

        XTEXMF(I,J)=XTEXMF(I,J) + EX(I,J)
        ULFDXE(J) = EX(I,J) * 0.001
!
        XTEXDF(I,J)=(XTEXMF(I,J)*FRMVCST(I,J))/1000000
        ULFDXR(J) = XTEXDF(I,J)

8005    CONTINUE
8000    CONTINUE

         DO 6000 I=1,MNUMYR
           DO 6010 J=1,MNUMNR+EFD_D_PROV
             DO 6020 K=1,MNUMNR+EFD_D_PROV                                    !02/07
             DO 6030 ISP=1,ETTns

! FOR DEBUG, COMPARE DIFFERENCE BETWEEN CONTRACTS AND CONSTRAINTS

        DIFF(ISP)=CNSTRNTS(ISP,I,J,K)-CNTRCTS(ISP,J,K,I)
       IF(CNSTRNTS(ISP,I,J,K).NE.0) THEN
          PCTDFS(ISP)=((CNSTRNTS(ISP,I,J,K)-CNTRCTS(ISP,J,K,I))/ &
                        CNSTRNTS(ISP,I,J,K))*100
       ELSE
          PCTDFS(ISP)=0
       ENDIF

! ADJUST CONSTRAINTS FOR CONTRACTS

      IF (J.GT.UNRGNS  .OR.  K.GT.UNRGNS) THEN
        CNSTRNTS(ISP,I,J,K)=(CNSTRNTS(ISP,I,J,K)) &
                             -CNTRCTS(ISP,J,K,I)
      ELSE
        CNSTRNTS(ISP,I,J,K)=(PIPEPCT(I) * CNSTRNTS(ISP,I,J,K)) &
                             -CNTRCTS(ISP,J,K,I)
      ENDIF

        IF(CNSTRNTS(ISP,I,J,K).LT.0) THEN
         write(6,8374) ' constraint limit hit seas year isp i j k ',isp,i,j,k,cntrcts(isp,j,k,i),cnstrnts(isp,i,j,k)
 8374 format(a,4(i,2x),2(f12.3,2x))
        ENDIF
        IF(CNSTRNTS(ISP,I,J,K).LT.0)CNSTRNTS(ISP,I,J,K)=0

6500   FORMAT(A5,2X,4(A4,1X,I2),2(A10,1X,F8.3))
6030    CONTINUE
6020    CONTINUE
6010    CONTINUE
6000    CONTINUE
       DUMMY='D6'

!
!  SAVE NEW CONSTRAINTS IN ETTIN
           PRINT *,'create new cnstrnts file'

       DO 8500 I=1,MNUMNR+EFD_D_PROV
        DO 8510  J=1,MNUMNR+EFD_D_PROV
           DO 8504 K=1,MNUMYR

            CS_SW=0
            DO 8511 L=1,ETTns
                !IF(CNSTRNTS(L,K,I,J) .GT. 0) CS_SW=1
                IF(CNSTRNTS_PREFIRM(L,K,I,J) .GT. 0) CS_SW=1
8511        CONTINUE

           IF (CS_SW .EQ. 1) THEN
        WRITE(UF_RCNSR,8505) J, I,K, PTHRESH1(K,J,I),PTHRESH2(K,J,I), &
               (CNSTRNTS(ISP,K,I,J),ISP=1,ETTns),(CNSTRNTS_PREFIRM(ISP,K,I,J),ISP=1,ETTns)
           ENDIF

8505          FORMAT(3I4,1X,2(F5.3,1X),<ETTns>(F7.3),<ETTns>(F7.3))

8504    CONTINUE
8510   CONTINUE
8500   CONTINUE

!     ADJUSTMENT FACTORS FOR T AND D LOSSES OVER TIME

      if (USW_XP .gt. 0) then         !Canada
         do I=1,MNUMYR
           ULOSSADJ(I) = 0.900
         enddo
      else                            !NEMS
         ULOSSADJ(1) = 1.000
         ULOSSADJ(2) = 1.000
         ULOSSADJ(3) = 1.000
         ULOSSADJ(4) = 1.000
         ULOSSADJ(5) = 1.000
         ULOSSADJ(6) = 1.000
         ULOSSADJ(7) = 1.000
         ULOSSADJ(8) = 1.000
         ULOSSADJ(9) = 1.000
         ULOSSADJ(10) = 0.974
         ULOSSADJ(11) = 0.965
         ULOSSADJ(12) = 0.956
         ULOSSADJ(13) = 0.946
         ULOSSADJ(14) = 0.937
         ULOSSADJ(15) = 0.928
         ULOSSADJ(16) = 0.918
         ULOSSADJ(17) = 0.909
         ULOSSADJ(18) = 1.000
         ULOSSADJ(19) = 1.000
         ULOSSADJ(20) = 1.000     
         ULOSSADJ(21) = 1.000
         ULOSSADJ(22) = 1.050     ! calibrate to STEO
         ULOSSADJ(23) = 1.050
         ULOSSADJ(24) = 1.050
         ULOSSADJ(25) = 1.020
         ULOSSADJ(26) = 1.000
         ULOSSADJ(27) = 1.000
         ULOSSADJ(28) = 1.000
         ULOSSADJ(29) = 1.000
         ULOSSADJ(30) = 1.000
         ULOSSADJ(31) = 1.000
         ULOSSADJ(32) = 0.990
         ULOSSADJ(33) = 0.980
         ULOSSADJ(34) = 0.970
         ULOSSADJ(35) = 0.960
         ULOSSADJ(36) = 0.950
       do I = 37,MNUMYR
         ULOSSADJ(I) = ULOSSADJ(36)
       end do
      endif

!  RECORD RESULTS IN THE NEW DEMAND FILE
!
      DO J=1,MNUMYR
         DO I=1,UNRGNS
            ZTDMMF(I)=XTDMMF(J,I)
            ZTDMDF(I)=XTDMDF(J,I)
            UQTDLS(I)=TDLS(J,I)
            ZTIMPF(I)=XTIMPF(J,I)
            ZTIMPD(I)=XTIMPD(J,I)
            ZTEXPF(I)=XTEXPF(J,I)
            ZTEXPD(I)=XTEXPD(J,I)
            ZTEXMF(I)=XTEXMF(J,I)
            ZTEXDF(I)=XTEXDF(J,I)
            FTDMDF(I)=FLDMDF(J,I)
            FTIMPD(I)=FLIMPD(J,I)
            FTEXPD(I)=FLEXPD(J,I)
            CTDMDF(I)=CAPDMDF(J,I)
            CTIMPD(I)=CAPIMPD(J,I)
            CTEXPD(I)=CAPEXPD(J,I)

            DO K=1,ETTns
               UEITAJ(K,I)=EEITJ(J,I,K)
               URNCSTEX(K,I) = PCTEXPCNS(K,I,J)
               URNCSTIM(K,I) = PCTIMPCNS(K,I,J)
            END DO

            DO IRG = 1 , MNUMNR + EFD_D_PROV
               UTFIRM(I,IRG) = FIRMTRADE(I,IRG,J)
             IF ( UTFIRM(I,IRG) .GT. 0.0 )  THEN
              write(6,8257) ' utfirm ',j,i,irg,utfirm(i,IRG)/1000.0
             ENDIF
             IF ( IRG .GT. UNRGNS ) THEN
               UTFIRM(IRG,I) = FIRMTRADE(IRG,I,J)
               IF ( UTFIRM(IRG,I) .GT. 0.0 )  THEN
                write(6,8257) ' utfirm ',j,irg,i,utfirm(irg,I)/1000.0
               ENDIF
             ENDIF
 8257  format(a,3I5,2(1x,F16.4))
            ENDDO

            DO L=1,ECP_D_CIS
               UCI$FMW(L,I)=FMW(J,L,I)
               UCI$PMW(L,I)=PMW(J,L,I)
               UCI$CF(L,I) = CF(J,L,I)
               UCI$CST(L,I)=CST(J,L,I)
               UCI$CRG(L,I)=CRG(J,L,I)
            END DO

!           Map to EFD Seasons
   
            DO EFD_SP = 1 , EFDns
               IF (HrsInETTEfdSPs(0,EFD_SP) .GT. 0.0) THEN
                  DO ETT_SP = 1 , ETTns
                     UEITAJ_EFD(EFD_SP,I) = UEITAJ_EFD(EFD_SP,I) + UEITAJ(ETT_SP,I) * HrsInETTEfdSPs(ETT_SP,EFD_SP)
                     URNCSTEX_EFD(EFD_SP,I) = URNCSTEX_EFD(EFD_SP,I) + URNCSTEX(ETT_SP,I) * HrsInETTEfdSPs(ETT_SP,EFD_SP)
                     URNCSTIM_EFD(EFD_SP,I) = URNCSTIM_EFD(EFD_SP,I) + URNCSTIM(ETT_SP,I) * HrsInETTEfdSPs(ETT_SP,EFD_SP)

                     WRITE(IMSG,8331) J+1989,I,EFD_SP,ETT_SP,UEITAJ_EFD(EFD_SP,I), UEITAJ(ETT_SP,I), URNCSTEX_EFD(EFD_SP,I), URNCSTEX(ETT_SP,I), &
                        URNCSTIM_EFD(EFD_SP,I), URNCSTIM(ETT_SP,I), HrsInETTEfdSPs(ETT_SP,EFD_SP)
 8331                FORMAT(1X,"HRSInETTEfdSPs",4(":",I4),7(":",F15.6))
                  END DO
               END IF
            END DO
            DO EFD_SP = 1 , EFDns
               IF (HrsInETTEfdSPs(0,EFD_SP) .GT. 0.0) THEN
                  UEITAJ_EFD(EFD_SP,I) = UEITAJ_EFD(EFD_SP,I) / HrsInETTEfdSPs(0,EFD_SP)
                  URNCSTEX_EFD(EFD_SP,I) = URNCSTEX_EFD(EFD_SP,I) / HrsInETTEfdSPs(0,EFD_SP)
                  URNCSTIM_EFD(EFD_SP,I) = URNCSTIM_EFD(EFD_SP,I) / HrsInETTEfdSPs(0,EFD_SP)

                  WRITE(IMSG,9331) J+1989,I, EFD_SP, UEITAJ_EFD(EFD_SP,I), URNCSTEX_EFD(EFD_SP,I), URNCSTIM_EFD(EFD_SP,I), HrsInETTEfdSPs(0,EFD_SP)
 9331             FORMAT(1X,"UEITAJ_EFD",3(":",I4),4(":",F15.6))
               END IF
            END DO

!           Map to ECP Seasons
   
            DO ECP_SP = 1 , ECPns
               IF (HrsInETTecpSPs(0,ECP_SP) .GT. 0.0) THEN
                  DO ETT_SP = 1 , ETTns
                     UEITAJ_ECP(ECP_SP,I) = UEITAJ_ECP(ECP_SP,I) + UEITAJ(ETT_SP,I) * HrsInETTecpSPs(ETT_SP,ECP_SP)
                     URNCSTEX_ECP(ECP_SP,I) = URNCSTEX_ECP(ECP_SP,I) + URNCSTEX(ETT_SP,I) * HrsInETTecpSPs(ETT_SP,ECP_SP)
                     URNCSTIM_ECP(ECP_SP,I) = URNCSTIM_ECP(ECP_SP,I) + URNCSTIM(ETT_SP,I) * HrsInETTecpSPs(ETT_SP,ECP_SP)
                     WRITE(IMSG,8332) J+1989,I,ECP_SP,ETT_SP,UEITAJ_ECP(ECP_SP,I), UEITAJ(ETT_SP,I), URNCSTEX_ECP(ECP_SP,I), URNCSTEX(ETT_SP,I), &
                        URNCSTIM_ECP(ECP_SP,I), URNCSTIM(ETT_SP,I), HrsInETTEcpSPs(ETT_SP,ECP_SP)
 8332                FORMAT(1X,"HRSInETTEcpSPs",4(":",I4),7(":",F15.6))
                  END DO
               END IF
            END DO
            DO ECP_SP = 1 , ECPns
               IF (HrsInETTecpSPs(0,ECP_SP) .GT. 0.0) THEN
                  UEITAJ_ECP(ECP_SP,I) = UEITAJ_ECP(ECP_SP,I) / HrsInETTecpSPs(0,ECP_SP)
                  URNCSTEX_ECP(ECP_SP,I) = URNCSTEX_ECP(ECP_SP,I) / HrsInETTecpSPs(0,ECP_SP)
                  URNCSTIM_ECP(ECP_SP,I) = URNCSTIM_ECP(ECP_SP,I) / HrsInETTecpSPs(0,ECP_SP)
                  WRITE(IMSG,9332) J+1989,I, ECP_SP, UEITAJ_ECP(ECP_SP,I), URNCSTEX_ECP(ECP_SP,I), URNCSTIM_ECP(ECP_SP,I), HrsInETTEcpSPs(0,ECP_SP)
 9332             FORMAT(1X,"UEITAJ_ECP",3(":",I4),4(":",F15.6))
               END IF
            END DO

         END DO
         IRECL =  J
         CALL STREIJ(J)
      END DO

!     USE LAST BLOCK FOR ECP LOOK AHEAD
!
      DO J = MNUMYR + 1 , MNUMYR + ECP_D_XPH - 1
         CALL STREIJ(J)
      END DO
!
  991 CONTINUE

!     WRITE OUT DEBUG REPORT

      DO 2020 IRG=1,UNRGNS
!
         WRITE (UF_PETT,100) IRG,NYR
  100    FORMAT(I4,40X,I11)
         WRITE (UF_PETT,110) 'YEARS',(JYEAR(JYR), JYR = 1 , MNUMYR)
  110    FORMAT(A7,39X,31(I11))
         WRITE(UF_PETT,120)'TRANSMISSION AND DISTRIBUTION LOSS FACTOR  ', (TDLS(JYR,IRG), JYR = 1 , MNUMYR)
  120    FORMAT(A45,2X,31(F11.5))
  125    FORMAT(A45,2X,31(F11.0))
  135    FORMAT(A45,2X,31(F11.1))
         DO ISP=1,ETTns
            WRITE(UF_PETT,130)'NET INTERRGIONAL ELECTRICITY FLOWS (GW)    ', ISP, (EEITJ(JYR,IRG,ISP), JYR = 1 , MNUMYR)
  130       FORMAT(A45,1X,I1,31(F11.3))
         END DO
      WRITE(UF_PETT,125) 'ZTDMMF - MWH FIRM PWR SALES (NET)          ' , &
       (XTDMMF(JYR,IRG), JYR = 1 , MNUMYR)
      WRITE(UF_PETT,125) 'ZTDMDF - $   FIRM PWR SALES (NET)          ' , &
       (XTDMDF(JYR,IRG), JYR = 1 , MNUMYR)
      WRITE(UF_PETT,125) 'ZTEXMF - MWH FIRM PWR SALES (GROSS)        ' , &
       (XTEXMF(JYR,IRG), JYR = 1 , MNUMYR)
      WRITE(UF_PETT,125) 'ZTEXDF - $   FIRM PWR SALES (GROSS)        ' , &
       (XTEXDF(JYR,IRG), JYR = 1 , MNUMYR)

      DO 14 ISP=1,ETTns
      WRITE(UF_PETT,130) '75% IMPORT CONSTRAINT (GW)                 ' , &
       ISP,(PCTIMPCNS(ISP,IRG,JYR), JYR = 1 , MNUMYR)
14      CONTINUE
      DO 16 ISP=1,ETTns
      WRITE(UF_PETT,130) 'MAX IMPORT CONSTRAINT (GW)                 ' , &
       ISP,(XRNCSTIM(ISP,IRG,JYR), JYR = 1 , MNUMYR)
16      CONTINUE
      DO 15 ISP=1,ETTns
      WRITE(UF_PETT,130) '75% EXPORT CONSTRAINT (GW)                 ' , &
       ISP,(PCTEXPCNS(ISP,IRG,JYR), JYR = 1 , MNUMYR)
15      CONTINUE
      DO 18 ISP=1,ETTns
      WRITE(UF_PETT,130) 'MAX EXPORT CONSTRAINT (GW)                 ' , &
       ISP,(XRNCSTEX(ISP,IRG,JYR), JYR = 1 , MNUMYR)
18      CONTINUE
      WRITE(UF_PETT,125)'ZTEXPF - INTERNAT FIRM PWR EXPORTS (MWH)   ', &
       (XTEXPF(JYR,IRG), JYR = 1 , MNUMYR)
      WRITE(UF_PETT,125) 'ZTEXPD - INTERNAT FIRM PWR EXPORTS (MM$)  ' , &
       (XTEXPD(JYR,IRG), JYR = 1 , MNUMYR)
      WRITE(UF_PETT,125)'ZTIMPF - INTERNAT FIRM PWR IMPORTS (MWH)   ', &
       (XTIMPF(JYR,IRG), JYR = 1 , MNUMYR)
      WRITE(UF_PETT,125) 'ZTIMPD - INTERNAT FIRM PWR IMPORTS (MM$)  ' , &
       (XTIMPD(JYR,IRG), JYR = 1 , MNUMYR)

        WRITE(UF_PETT,125)'PURCHASED POWER COSTS VARIBLES          '
      WRITE(UF_PETT,125)'FTDMDF - DOMESTIC FIRM PWR FL COSTS(MM$)   ', &
       (FLDMDF(JYR,IRG), JYR = 1 , MNUMYR)
      WRITE(UF_PETT,125) 'FTEXPD - INTERNAT FIRM PWR EXPORTS (MM$)  ' , &
       (FLEXPD(JYR,IRG), JYR = 1 , MNUMYR)
      WRITE(UF_PETT,125) 'FTIMPD - INTERNAT FIRM PWR IMPORTS (MM$)  ' , &
       (FLIMPD(JYR,IRG), JYR = 1 , MNUMYR)
      WRITE(UF_PETT,125)'CTDMDF - DOMESTIC FIRM PWR CAP COSTS(MM$)   ', &
       (CAPDMDF(JYR,IRG), JYR = 1 , MNUMYR)
      WRITE(UF_PETT,125) 'CTEXPD - INTERNAT FIRM PWR EXPORTS (MM$)  ' , &
       (CAPEXPD(JYR,IRG), JYR = 1 , MNUMYR)
      WRITE(UF_PETT,125) 'CTIMPD - INTERNAT FIRM PWR IMPORTS (MM$)  ' , &
       (CAPIMPD(JYR,IRG), JYR = 1 , MNUMYR)


        WRITE(UF_PETT,125)'CANADIAN SUPPLY - STEP 1                '
        WRITE(UF_PETT,135)'UCI$FMW -CANADIAN RHS                  ', &
         (FMW(J,1,IRG), J=1,MNUMYR)
        WRITE(UF_PETT,135)'UCI$PMW - CANADIAN PK RHS               ', &
         (PMW(J,1,IRG), J=1,MNUMYR)
        WRITE(UF_PETT,135)'UCI$CF - CANADIAN CF                    ', &
         (CF(J,1,IRG), J=1,MNUMYR)
        WRITE(UF_PETT,135)'UCI$CST - CANADIAN MILLS                ', &
          (CST(J,1,IRG), J=1,MNUMYR)
        WRITE(UF_PETT,135)'UCI$CRG - CANADIAN REGION               ', &
          (CRG(J,1,IRG), J=1,MNUMYR)
        WRITE(UF_PETT,125)'CANADIAN SUPPLY - STEP 2                '
        WRITE(UF_PETT,135)'UCI$FMW -CANADIAN RHS                  ', &
         (FMW(J,2,IRG), J=1,MNUMYR)
        WRITE(UF_PETT,135)'UCI$PMW - CANADIAN PK RHS               ', &
         (PMW(J,2,IRG), J=1,MNUMYR)
        WRITE(UF_PETT,135)'UCI$CF - CANADIAN CF                    ', &
         (CF(J,2,IRG), J=1,MNUMYR)
        WRITE(UF_PETT,135)'UCI$CST - CANADIAN MILLS                ', &
          (CST(J,2,IRG), J=1,MNUMYR)
        WRITE(UF_PETT,135)'UCI$CRG - CANADIAN REGION               ', &
          (CRG(J,2,IRG), J=1,MNUMYR)


        DO K = 1 , MNUMNR + EFD_D_PROV
          IF (FIRMTRADE(IRG,K,21) .NE. 0.0) THEN
            write(UF_PETT,145) ' FTRADE ',irg,k,(FIRMTRADE(IRG,k,IYR)/1000.0,IYR=16,31)
          ENDIF
          IF ( K .GT. UNRGNS) THEN
            IF (FIRMTRADE(k,IRG,21) .NE. 0.0) THEN
              write(UF_PETT,145) ' FTRADE ',k,irg,(FIRMTRADE(k,IRG,IYR)/1000.0,IYR=16,31)
            ENDIF
          ENDIF
!!!!!     DO iy = 1 , MNUMYR
!           CALL GETEIJ(IY)
!           write(uf_pett,152) ' utfirm ',iy,irg,k,utfirm(irg,k),utfirm(k,irg)
!         ENDDO
        ENDDO
2020  CONTINUE

  145  FORMAT(A,2X,i4,i4,16(F12.2))
  152  FORMAT(a,2x,3i5,2(f12.2))

        FILENM='CANSPLY'
        UF_CAN=FILE_MGR('C',FILENM,NEW)
        FILENM='ETTIN'
        UF_RCNSR=FILE_MGR('C',FILENM,NEW)
        FILENM='ETTDEM'
!       UF_ETDM=FILE_MGR('C',FILENM,NEW)
        FILENM='PREPDB'
        UF_PETT=FILE_MGR('C',FILENM,NEW)
        FILENM='LOADDAF'
!       UF_LD=FILE_MGR('C',FILENM,NEW)

      END

      FUNCTION RTOVALUE(RTONAME,RTODEFVAL)

! ---------------------------------------------------------------*

!   THIS FUNCTION SEARCHES THE MOREOPT UNIT IN THE JCL FOR THE   *
!   RUN TIME OPTION (RTO) NAME SENT IN THE FUNCTION INVOCATION,  *
!   AND RETURNS THE VALUE SET FOR THE RUN.  IF NOT FOUND, THE    *
!   FUNCTION RETURNS 9999.  THE UNIT IS REWOUND AT THE END.      *

! ---------------------------------------------------------------*

      IMPLICIT NONE
      INTEGER UNIT,VVALUE,RTOVALUE,FILE_MGR,RTODEFVAL
      CHARACTER FILENM*18,RTONAME*8,VNAME*8
      LOGICAL NEW,FINDRTO
      EXTERNAL FILE_MGR
      RTOVALUE = 0
      RETURN
      END



subroutine unitunopened(start,end,iunit)
	implicit none
	logical 	lopen
	INTEGER 	start,end,iunit
	INTEGER 	I
	
	iunit=-1
	do I= start,end
	  iunit=I
	  inquire(unit=iunit,opened=lopen)
	  if(.not. lopen) then
		 return
	  endif
	enddo
	return
end

!===================================================================================================================
!******************************************************************
! subroutine callsys(iret,cmd)
!******************************************************************
subroutine callsys(iret,cmd)
use dfport
character*(*) cmd
integer iret
write(6,'(a,a)') ' Calling system to do this:  ',trim(cmd)
iret=system(cmd)
if(iret.ne.0) then
  write(6,'(a,I2,a,a)') '   Command failed with return code of ',iret,':  ',trim(cmd)
endif
return
end
!===================================================================================================================