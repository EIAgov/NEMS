! $Header: m:/default/source/RCS/ftab2.f,v 1.108 2020/09/24 18:36:28 dgr Exp $
! ***********************************************************
! SUBROUTINES USED BY THE PROGRAM FTAB
! ***********************************************************
      SUBROUTINE FREADF
       IMPLICIT NONE
! READS IN TABLE LAYOUT, INCLUDING HEADINGS, ROW LABELS, ROW FORMATS,
! PAGE BREAKS, AND ARRAY REFERENCE NUMBERS.
      include 'parametr'
      include 'ftable'

      INTEGER MAX_CITE
      PARAMETER(MAX_CITE=40)   ! Number of citation replacement strings
      CHARACTER*100 ERRMSG
      CHARACTER*2048 LINE,RTYPE*6,LABEL*46,CROW*3,CFMT*2
      INTEGER I,J,ITAB,IROW,IHED,IR,IY,MXROW,NPAGE,NREC,III,JJJ,INCITE
      CHARACTER*4 CHAR4YEAR
      CHARACTER*4 CHAR4CAP
      CHARACTER*30 SCEN_AND_DATE
      CHARACTER*75 CITE_RSTR(MAX_CITE)   !  Citation replacement string
      CHARACTER*75 CITE_WITH(MAX_CITE)   !  Citation replace with this string
      CHARACTER*75 MAC_GDP, MAC_OUT      !  Assign proper value from citation file to apply to all lines
      CHARACTER*12 DIOXIDE/' equivalent '/
! CSV file variables--start
      integer maxfield ! maximum number of columns to be processed on a row
      parameter(maxfield=30) ! currently 30 in layout.txt or layout.csv
      integer fields(2,maxfield) ! starting and ending position of each column-field
      integer cnt,nfield,ifield
      character*1536 afields(maxfield) ! character contents of each column-field
! CSV file variables--end
      integer longest(maxfield)/maxfield*0/

!  set up a few replacement string variables
      WRITE(CHAR4YEAR,'(I4)') FYRPRC             !  reporting year for real prices
      WRITE(CHAR4CAP,'(I4)') CUMCAPADD           !  cumulative reporting items after this year
      SCEN_AND_DATE=trim(frscen) // '.' // trim(fdate)              ! scenario.datekey
      IF (TRIM(CARBON_OR_2) .EQ. 'CO2') DIOXIDE = ' dioxide    '    ! whether reporting carbon or carbon dioxide
      NTABLE=0
      III = 0  ! order counter
      JJJ = 0  ! order counter
      DO 10 I=1,MAXROW
      DO 10 J=1,MAXTAB
        ROWLAB(I,J)=' '
        ROWFMT(I,J)=0
        IROWS(I,J)=0
10    CONTINUE
      DO 40 I=1,MAXTAB
        NROWS(I)=0
        NHED(I)=0
        NFOOT(I)=0
        ROWHED(I)=' '
        TABFMT(I)=0
        IPAGE(I,1)=1
        IPAGE(I,2)=1
        IPAGE(I,3)=1
        IPAGE(I,4)=1
        DO 20 J=1,MAXHED
           TABHED(J,I)=' '
20      CONTINUE
        DO 30 J=1,MAXNOT
           FOOTNT(J,I)=' '
30      CONTINUE
40    CONTINUE

! read both the search string and replacement strings from the citation file
      OPEN(UNIT=10,FILE=CITATION,ERR=97,STATUS='OLD',ACTION='READ')
      DO INCITE=1,MAX_CITE
        READ(10,*,END=49) CITE_RSTR(INCITE),CITE_WITH(INCITE)
        IF (TRIM(CITE_RSTR(INCITE)) .EQ. "m__m") READ(CITE_WITH(INCITE),'(I4)') MACYR
        IF (TRIM(CITE_RSTR(INCITE)) .EQ. "m__m") MAC_GDP = trim(CITE_WITH(INCITE))
        IF (TRIM(CITE_RSTR(INCITE)) .EQ. "m==m") MAC_OUT = trim(CITE_WITH(INCITE))
      ENDDO
49    CLOSE(10)
      INCITE = INCITE - 1      ! should be 1 more than the number of records at this point
      if(INCITE .GE. MAX_CITE) WRITE(6,'(I4,A,I4)') INCITE,' records were read from the citation file with MAX_CITE set at',MAX_CITE

      open(100,file='layout.debug.txt',status='unknown')

! READ LOOP
      NREC=0
      ERRMSG= ' *** ERROR OPENING LAYOUT FILE *** '
      OPEN(UNIT=10,FILE=LAYOUT,ERR=98,STATUS='OLD',ACTION='READ')

! Read and process each line of the CSV layout file
      DO WHILE (.not. EOF(10))
        READ(10,'(a)') LINE
        NREC=NREC+1

! Do global string replacements
        CALL REPLACE(LINE,'|',' ')
        CALL REPLACE(LINE,'####',CHAR4YEAR)
        CALL REPLACE(LINE,'@@@@',CHAR4CAP)
        CALL REPLACE(LINE,'_cc_',TRIM(CARBON_OR_2))
        CALL REPLACE(LINE,'_dd_',TRIM(DIOXIDE))
        CALL REPLACE(LINE,'##',CHAR4YEAR(3:4))
        CALL REPLACE(LINE,'m__m',trim(MAC_GDP))
        CALL REPLACE(LINE,'m==m',trim(MAC_OUT))
        CALL REPLACE(LINE,'scenario_and_datekey',trim(scen_and_date))

! Separate the CSV line into the columns-fields.
! 1) determine starting and ending position of each column-field on the CSV line.
        afields(1:maxfield)=' '
        call csv_field(line,fields,maxfield,cnt)
! 2) collect contents of each column-field into character array AFIELDS().  The Ith field
!    is put in afields(i). The fields are placed in appropriate variables based on the row type
!    (RTYPE).

        nfield=cnt
        if(nfield.gt.maxfield) nfield=maxfield
        do ifield=1,nfield
          call get_csv_field(line,fields,maxfield,ifield,afields(ifield))
          longest(ifield)=max(   len_trim(afields(ifield))  ,longest(ifield))
        enddo

        RTYPE=afields(1)

!  CHECK FOR COMMENT
        IF (RTYPE .EQ. 'CM') cycle
        if(RTYPE .EQ. 'RG' .OR. RTYPE .EQ. 'RD')then
          cycle
        endif
        IF (RTYPE(1:2).EQ.'TN') THEN
! "TN III" STARTS NEW TABLE III, WHERE III IS THE TABLE NUMBER.
!  TABLE NUMBERS MUST BE IN SEQUENCE.
           ERRMSG='BAD TABLE NUMBER IN TABLE LAYOUT FILE'
           READ(RTYPE,'(3x,I3)',ERR=98) ITAB
           IF (ITAB.LE.0) GOTO 98
           NTABLE=NTABLE+1
           NPAGE=1
           IF (ITAB.NE.NTABLE) THEN
              ERRMSG='TABLE NUMBERS OUT OF SEQUENCE'
              GOTO 98
           ENDIF
           IF (NTABLE.GT.MAXTAB)THEN
              ERRMSG='NUMBER OF TABLES EXCEEDS MAX'
              GOTO 98
           ENDIF

        ELSEIF (RTYPE.EQ.'RN') THEN
! "RN" RANGE NAME (USED ONLY IN WORKSHEET)
               RGNAME(ITAB)=trim(afields(2))

        ELSEIF (RTYPE.EQ.'HD') THEN
! "HD" TABLE HEADERS
               NHED(ITAB)=NHED(ITAB)+1
               IF (NHED(ITAB).GT.MAXHED) THEN
                  ERRMSG='MAXIMUM NUMBER OF HEADERS EXCEEDED'
                  GOTO 98
               ENDIF
               IHED=NHED(ITAB)
               TABHED(IHED,ITAB)=trim(afields(2))
               if(ihed.eq.1) then
                  SUBJECT(ITAB)=trim(afields(3))
               endif

        ELSEIF (RTYPE.EQ.'RH') THEN
! "RH" ROW HEADER
               ROWHED(ITAB)=trim(afields(2))


        ELSEIF (RTYPE.EQ.'RL') THEN
! "RL" ROW LABEL.  LABEL IS 35 CHARS, THEN I2 FOR TABLE ARRAY REFERENCE NUMBER
!                  (IROWS), THEN OPTIONAL ROW FORMAT (ROWFMT(IROW,ITAB)
               NROWS(ITAB)=NROWS(ITAB)+1
               IF (NROWS(ITAB).GT.MAXROW) THEN
                  ERRMSG='MAXIMUM ROWS IN A TABLE EXCEEDED'
                  WRITE(6,990)ITAB,NROWS(ITAB),MAXROW
  990             FORMAT('NROWS(',I4,')=',I4,'MAXROW=',I4)
                  GOTO 98
               ENDIF
               IROW=NROWS(ITAB)
               ROWLAB(IROW,ITAB)=trim(afields(2))
               CROW=trim(afields(3))
               CFMT=trim(afields(4))

! 5:e
               if(len_trim(afields(5)).gt.len(DRName(IROW,ITAB))) then
                 write(100,'(a,i6,a,2i4)') 'On line ',nrec,' of layout, column 5 string too big: ',len_trim(afields(5))
               endif
               DRName(IROW,ITAB)=trim(afields(5))
! 6:f
               if(len_trim(afields(6)).gt.len(DRForm(IROW,ITAB))) then
                 write(100,'(a,i6,a,2i4)') 'On line ',nrec,' of layout, column 6 string too big: ',len_trim(afields(6))
               endif
               DRForm(IROW,ITAB)=trim(afields(6))
! 7:g
               if(len_trim(afields(7)).gt.len(graph_units(irow,itab))) then
                 write(100,'(a,i6,a,2i4)') 'On line ',nrec,' of layout, column 7 string too big: ',len_trim(afields(7))
               endif
               graph_units(irow,itab)=trim(afields(7))
! 8:h
               if(len_trim(afields(8)).gt.len(graph_label(irow,itab))) then
                 write(100,'(a,i6,a,2i4)') 'On line ',nrec,' of layout, column 8 string too big: ',len_trim(afields(8))
               endif
               graph_label(irow,itab)=trim(afields(8))
               if (DRForm(IROW,ITAB)(6:6) .EQ. '1') then             ! designates bonus row
                  if (LEN_TRIM(afields(8)) .LE. (132-10)) then       ! checks how much room left in label
                     graph_label(irow,itab)=trim(afields(8)) // '*BONUS ROW'
                  else if (LEN_TRIM(afields(8)) .LE. (132-6)) then
                     graph_label(irow,itab)=trim(afields(8)) // '*BONUS'
                  else if (LEN_TRIM(afields(8)) .LE. (132-3)) then
                     graph_label(irow,itab)=trim(afields(8)) // '*BR'
                  else
                     graph_label(irow,itab)=afields(8)(1:129) // '*BR'
                  endif
               endif
! 9:i
               if(len_trim(afields(9)).gt.len(data_type(irow,itab))) then
                 write(100,'(a,i6,a,2i4)') 'On line ',nrec,' of layout, column 9 string too big: ',len_trim(afields(9))
               endif
               data_type(irow,itab)=trim(afields(9))
! 10:j
               if(len_trim(afields(10)).gt.len(sub_data_type(irow,itab))) then
                 write(100,'(a,i6,a,2i4)') 'On line ',nrec,' of layout, column 10 string too big: ',len_trim(afields(10))
               endif
               sub_data_type(irow,itab)=trim(afields(10))
! 11:k
               if(len_trim(afields(11)).gt.len(sector(irow,itab))) then
                 write(100,'(a,i6,a,2i4)') 'On line ',nrec,' of layout, column 11 string too big: ',len_trim(afields(11))
               endif
               sector(irow,itab)=trim(afields(11))
! 12:l
               if(len_trim(afields(12)).gt.len(subsector(irow,itab))) then
                 write(100,'(a,i6,a,2i4)') 'On line ',nrec,' of layout, column 12 string too big: ',len_trim(afields(12))
               endif
               subsector(irow,itab)=trim(afields(12))

! 13:m
               if(len_trim(afields(13)).gt.len(energy_source(irow,itab))) then
                 write(100,'(a,i6,a,2i4)') 'On line ',nrec,' of layout, column 13 string too big: ',len_trim(afields(13))
               endif
               energy_source(irow,itab)=trim(afields(13))
! 14:n
               if(len_trim(afields(14)).gt.len(sub_energy_source(irow,itab))) then
                 write(100,'(a,i6,a,2i4)') 'On line ',nrec,' of layout, column 14 string too big: ',len_trim(afields(14))
               endif
               sub_energy_source(irow,itab)=trim(afields(14))
! 15:o
               if(len_trim(afields(15)).gt.len(geography(irow,itab))) then
                 write(100,'(a,i6,a,2i4)') 'On line ',nrec,' of layout, column 15 string too big: ',len_trim(afields(15))
               endif
               geography(irow,itab)=trim(afields(15))

! 16:p
               if(len_trim(afields(16)).gt.len(var_units(irow,itab))) then
                 write(100,'(a,i6,a,2i4)') 'On line ',nrec,' of layout, column 16 string too big: ',len_trim(afields(16))
               endif
               var_units(irow,itab)=trim(afields(16))

! 17-24:qrstuvwx
               do i=1,8
                 ifield=16+i
                 if(len_trim(afields(ifield)).gt.len(var_abbrev(ifield,irow,itab))) then
                   write(100,'(a,i6,a,i1,a,2i4)') 'On line ',nrec,' of layout, column ',ifield,' string too big: ',len_trim(afields(ifield))
                 endif
                 var_abbrev(i,irow,itab)=trim(afields(ifield))
               enddo

! 25:y
               if(len_trim(afields(25)).gt.len(varname(irow,itab))) then
                 write(100,'(a,i6,a,2i4)') 'On line ',nrec,' of layout, column 25 string too big: ',len_trim(afields(25))
               endif
               varname(irow,itab)=trim(afields(25))

! 30:ad
               if(len_trim(afields(30)).gt.len(expression(irow,itab))) then
                 write(100,'(a,i6,a,2i4)') 'On line ',nrec,' of layout, column 30 string too big: ',len_trim(afields(30))
               endif
               expression(irow,itab)=trim(afields(30))
               
 

               CALL REPLACE(ROWLAB(IROW,ITAB),'_cc_',TRIM(CARBON_OR_2))
               CALL REPLACE(ROWLAB(IROW,ITAB),'_dd_',TRIM(DIOXIDE))

               ERRMSG='BAD ROW FORMAT NUMBER'
               READ(CFMT,'(BN,I2)',ERR=98) ROWFMT(IROW,ITAB)
               ERRMSG='BAD TABLE ROW REFERENCE NUMBER'
               READ(CROW,'(BN,I3)',ERR=98) IROWS(IROW,ITAB)
               IF (IROWS(IROW,ITAB).GT.MAXIRW) THEN
                  ERRMSG='MAXIMUM ROW REFERENCE EXCEEDED'
                  GOTO 98
               ENDIF


        ELSEIF (RTYPE.EQ.'PB') THEN
               IPAGE(ITAB,NPAGE)=NROWS(ITAB)+1
               IF (NPAGE.LT.4) NPAGE=NPAGE+1
               DRForm(IROW,ITAB)=trim(afields(6))

        ELSEIF (RTYPE.EQ.'TF') THEN
! "TF" TABLE FORMAT:  DEFAULT FORMAT USED FOR ROWS WITH ROW FORMAT BLANK
               ERRMSG='BAD TABLE FORMAT'
               READ(AFIELDS(2),'(BN,I2)',ERR=98) TABFMT(ITAB)

        ELSEIF (RTYPE.EQ.'FN') THEN
! "FN" FOOT NOTE TEXT (U-FORMAT, U-PUT IN FOOT NOTE REFERENCES IN ROW LABELS)
               NFOOT(ITAB)=NFOOT(ITAB)+1
               IF (NFOOT(ITAB).GT.MAXNOT) THEN
                  ERRMSG='MAXIMUM NUMBER OF FOOTNOTE LINES EXCEEDED'
                  GOTO 98
               ENDIF
               if( len_trim(afields(2)) .gt. len(FOOTNT(NFOOT(ITAB),ITAB)) ) then
                 write(100,'(a,i6,a,2i4)') 'On line ',nrec,'of layout, footnote too big: ',len_trim(afields(2))
               endif
               FOOTNT(NFOOT(ITAB),ITAB)=afields(2)

               DO J=1,INCITE
                  CALL REPLACE(FOOTNT(NFOOT(itab),itab),trim(CITE_RSTR(J)),trim(CITE_WITH(J)))
               ENDDO

        ELSEIF (RTYPE.EQ.'OR') THEN
! "OR" READ TABLE ORDER AS SPECIFIED BY USER FOR OUTPUT.  Now overridden by "seqnum" optionally read from tabreq.txt
           i=1
           do jjj=iii+1,III+10
             i=i+1
             read(afields(i),'(BN,10I4)',ERR=98) ORDER(jjj)
           enddo
           III = III+10
        ELSE
               ERRMSG='BAD TABLE RECORD TYPE--'//RTYPE
               GOTO 98
        ENDIF
      end do  ! END OF do while READ LOOP

! If here, end of file was reached

!  By default, order will be by the table numbers in the layout file
      if(iii.eq.0) then
        do iii=1,ntable
          order(iii)=iii
        enddo
      endif

      CLOSE(10)

      WRITE(100,*) ' NUMBER OF RECORDS READ FROM TABLE FILE:',NREC
      WRITE(100,*) ' NUMBER OF TABLES:',NTABLE
      WRITE(100,*) ' '
      WRITE(100,*) ' TABLE    NUMBER   NUMB   MAX IROWS  TABLE   # FOOT'
      WRITE(100,*) '   #     HEADERS   ROWS   REFERENCE  FORMAT   NOTES '
      DO 991 ITAB=1,NTABLE
        MXROW=0
        DO 992 IROW=1,NROWS(ITAB)
          MXROW=MAX(MXROW,IROWS(IROW,ITAB))
992     CONTINUE
        WRITE(100,'(I4,5I9)') ITAB,NHED(ITAB),NROWS(ITAB),MXROW, &
        TABFMT(ITAB),NFOOT(ITAB)
991   CONTINUE
      write(100,'(a)') 'Length of longest fields for each column in layout file'
      do ifield=1,maxfield
        write(100,'(2i5)') ifield,longest(ifield)
      enddo
      close(100)
      RETURN

! ERROR BRANCH
98    CONTINUE
      WRITE(6,980) trim(layout),trim(ERRMSG),NREC
980   FORMAT(' *** ERROR READING TABLE LAYOUT (FTAB2.F, SUBROUTINE FREADF)'/ &
             ' *** layout file: ',A/ &
             ' *** ',A/ &
             ' *** ERROR OCCURRED AT LAYOUT LINE NUMBER=',I6/)
      CLOSE(10)
      STOP 57
97    CONTINUE
      WRITE(6,'(A)') ' ---> ERROR OPENING FOOTNOTE SOURCE FILE: '//trim(citation)
      STOP 58
      END
!*************************************************************************
      SUBROUTINE FWRITF(ITAB,IGROWO,IGRWYR,IREGN,IUNIT,IRW)
       IMPLICIT NONE
!  WRITES OUT A TABLE SPECIFIED BY THE TABLE LAYOUT.  THE NUMERIC INFORMATION
!  FOR THE TABLE IS ASSUMED TO BE IN REAL ARRAY TABN(MAXIRW,MAXYR,SCEN).
!
      include 'parametr'
      include 'ncntrl'
      include 'ftable'
!
      CHARACTER*(MCOLWD) USCORE,USBAR(MAXYR),VBAR(MAXYR),VBAR2(MAXYR), &
       SCENID(MAXYR),LSCORE*47,BOXHED*160
      CHARACTER*166 CENTERLN,MASKLN,VBARLN
      CHARACTER*61 NEMS,DISCLM*75,VERT*1,comma*1/','/,apos*1/'"'/
      CHARACTER*35 TEMPLAB
      INTEGER CW(MAXYR),I,IYR,IROW,ITAB,IGROWO,IGRWYR,IREGN,DOTLEAD
      INTEGER ib,IC,IE,II,IR,IRW,IUNIT,L,NLINE,LRECL,LCOM,IS, Redo_vbar
!      LOGICAL CENTERIT
      integer*2 zero2/0/
      integer*2 rectype,gtablenum,ftablenum,region,headnum,rownum,ladyrec
      integer*2 gtable
      real*4 rowdata(MAXYR-5-1)
      integer*2 startsat(500)
      character*2 datarow
      character*80 header
      common/lady/rectype,gtablenum,ftablenum,region,headnum,rownum,startsat,ladyrec
!Additional variables for newer version of the RAN file.
      integer*4 RXPtr,RTPtr,RDPtr,RGPtr
      common/NewRan/RXPtr,RTPtr,RDPtr,RGPtr
      integer*2 RGType,RGSTyp,RGLRem,RGTNum,RGTAct,RGTReg,RGRNum,RGTTyp,RGTLen,RGDTyp,RGDFYr,RGDLYr
      integer*2 RTNum,RTTRow,RTDRow
      integer*4 RTDLoc,RTILoc,RDLoc(MaxRow)
      real*4 RGData(MNUMYR)
      character*6 RGTKey,RTKey,RGForm
      character*16 RDKey(MaxRow),RGRKey
      character*500 RGText
      common/NewRan2/RGType,RGSTyp,RGLRem,RGTNum,RGTAct,RGTReg,RGRNum,RGTTyp,RGTLen,RGDTyp,RGDFYr, &
       RGDLYr,RTNum,RTTRow,RTDRow
      common/NewRan3/RTDLoc,RTILoc,RDLoc,RGData
      common/NewRan4/RGTKey,RTKey,RGForm,RDKey,RGRKey,RGText
      character*1536 RowStrings(8)
      integer*2 LenRowStr(8),LenRowStrs,j,LenSubject

!=================================================================================
      DATA LSCORE/'___________________________________'/

      NEMS= &
      'N A T I O N A L   E N E R G Y   M O D E L I N G   S Y S T E M'
      DISCLM = 'Phase I NEMS -- Software Test Platform -- ' &
            // 'Results for Testing Linkages Only'
      IF (EIA.EQ.' ') &
          EIA='E N E R G Y   I N F O R M A T I O N   A D M I N I S T R A T I O N'
      VERT='|'
      VBARLN(1:2)='+'//' '
      DO 2 I=3,166
2       VBARLN(I:I)=' '
      LRECL=(MIN(NCOLS,MAXCOL)*ICOLWD+32)
!      IF(IGROWO.EQ.1)VBARLN((LRECL+4):(LRECL+4))=' '
!      IF(IGROWO.EQ.0)VBARLN((LRECL+3):(LRECL+3))=' '
      MASKLN=' '
      DO 3 I=1,MCOLWD
3       USCORE(I:I)='_'
      USBAR(I)=USCORE
      USBAR(I)(1:1)=VERT
      DO 4 I=1,MAXYR
        CW(I)=ICOLWD
        USBAR(I)=USCORE
        IF(ISCEN(I).EQ.1) THEN
          VBAR(I)=VERT
          VBAR2(I)=VERT
          USBAR(I)(1:1)=VERT
          SCENID(I)=' '
          IF(NSCEN.GT.1) SCENID(I)=' (1)'
        ELSE
          IF(I.GT.1) THEN
            VBAR(I)=' '
            VBAR2(I)=' '
          ELSE
            VBAR(I)=VERT
            VBAR2(I)=VERT
            USBAR(I)(1:1)=VERT
          ENDIF
          IF(ISCEN(I).EQ.2) SCENID(I)=' (2)'
          IF(ISCEN(I).EQ.3) SCENID(I)=' (3)'
          IF(ISCEN(I).EQ.4) SCENID(I)=' (4)'
          IF(ISCEN(I).EQ.5) SCENID(I)=' (5)'
          IF(ISCEN(I).EQ.6) SCENID(I)=' (6)'
          IF(ISCEN(I).EQ.7) SCENID(I)=' (7)'
        ENDIF
4     CONTINUE
      IF (IGROWO .EQ. 1) THEN
        do i=nscen,1,-1
          vbar(ncols-i+1)=collb1   ! to display start year for growth rate
          if(icolwd.lt.mcolwd) cw(ncols-i+1)=cw(ncols-i+1)+1
          if(i.lt.nscen) then
              vbar(ncols-i+1)(1:1)= ' '
            collab(ncols-i+1)(1:1)= ' '
          endif
        enddo
      ENDIF
      L=LEN_TRIM(FRSCEN)
      IF(FRSCEN.NE.' ') BOXHED=TRIM(FRSCEN)//'.'//TRIM(FDATE)//' '//COMENT
! FILL THE TABLE ARRAY FROM THE TABLE NUMBERS USING THE APPROPRIATE ROW FORMAT
!
!  ASSIGN DEFAULT ROW FORMAT TO THOSE WITH ROWFMT=0
!  THE FORMAT IS THE NUMBER OF DECIMAL PLACES, EXCEPT 9 DENOTES INTEGER FORMAT.
!
      if (report_switch(itab) .or. LADYFILE .ne. 0) then
        IF(TABFMT(ITAB).EQ.0) TABFMT(ITAB)=2
        DO 20 I=1,NSCEN
        DO 20 IROW=1,NROWS(ITAB)
          IR=IROWS(IROW,ITAB)
          IF(IR.GT.0) THEN
            IF(ROWFMT(IROW,ITAB).EQ.0) ROWFMT(IROW,ITAB)=TABFMT(ITAB)
            DO 10 IYR=1,MAXYR-1
              CALL FILL(TABN(IR,IYR,I),TABC(IR,IYR,I),ROWFMT(IROW,ITAB),ICOLWD)
              IF(TABN(IR,MAXYR,I) .EQ. -999.) TABC(IR,MAXYR,I)=' - -'
              IF(TABN(IR,MAXYR,I) .EQ. -888.) TABC(IR,MAXYR,I)='  w '
10          CONTINUE
!       DO GROWTH RATE OR ROW SUM COLUMN WITH FIXED FORMAT
            IF(IGROWO.EQ.1) THEN
              CALL FILL(TABN(IR,MAXYR,I),TABC(IR,MAXYR,I),1,ICOLWD)
              L=LEN_TRIM(TABC(IR,MAXYR,I)(:ICOLWD))
              IF(L.LT.MCOLWD) TABC(IR,MAXYR,I)(L+1:)='%'
              IF(TABN(IR,MAXYR,I) .EQ. -999.) TABC(IR,MAXYR,I)=' - -'
              IF (IR .LE. NTAB099) THEN
              IF (ITAB .EQ. 99 .AND. T99(IR,IGRWYR,I) .EQ. -888.) TABC(IR,MAXYR,I)='  w '
              ENDIF
            ENDIF
          ENDIF
20      CONTINUE
      END IF

!  Optionally, Create the GRAF200 Interface File, Format 1: .csv
      if (LADYFILE .ne. 0) then
! Create the GRAF2000 Interface File, a binary random-access file (.RAN file)
        !Write out DataType 1 records (Title, Label, Text).
        RGType=1         ! Record type (1 for headings)
        RGSTyp=1
        RGTNum=RGTNum+1  ! table sequence number
        RGTKey(1:6)=RGName(itab)//'A00'  ! Unique string identifying this table (1:3)
        if(iregn.gt.0.and.iregn.lt.10) write(RGTKey(6:6),'(i1)') iregn
        if(iregn.ge.10.and.iregn.lt.100) write(RGTKey(5:6),'(i2)') iregn
        RGTAct=itab      ! Actual table number in ftab arrays
        RGTReg=iregn     ! Region number
        RTNum=RGTNum     ! table sequence number
        RTKey=RGTKey     ! table ID string
        RTDLoc=RGPtr     ! byte pointer to the start of the table within the file
        RTILoc=RDPtr     ! byte pointer to the start of the data
        RTTRow=0         ! initialize count of irow records in the table
        RTDRow=0         ! initialize count of data row records in the table
        do i=1,3
         RGTTyp=i        ! heading Label index (up to 3 headings per table)
         RGTLen=len_trim(tabhed(i,itab))  ! length of this heading
         RGText=tabhed(i,itab)            ! heading text
         RGForm=repeat(' ',6)             ! heading format (possible future use?) fill with blanks
         LenSubject=len_trim(Subject(itab)) ! length of table subject (short heading for this table)
         if(LenSubject.eq.0) LenSubject=1
         RGLRem=RGTLen + 5*2 + 6 + 6 + 2+ LenSubject      ! Length of this record
         ! length of fields: 2      2      2      2      6      2      2      2      6      2      RGTLen
         write(21,rec=RGPtr) RGType,RGSTyp,RGLRem,RGTNum,RGTKey,RGTAct,RGTReg,RGTTyp,RGForm,RGTLen,RGText(1:RGTLen),&
         LenSubject,  Subject(itab)(1:LenSubject)

         RGPtr=RGPtr + 6 + RGLRem
        end do
        RGTTyp=4
        RGTLen=len_trim(rowhed(itab))
        RGText=rowhed(itab)(1:RGTLen)
        RGForm=repeat(' ',6)
        RGLRem=RGTLen + 5*2 + 6 + 6   !  Length of this record
        ! length of fields: 2      2      2      2      6      2      2      2      6      2      RGTLen
        write(21,rec=RGPtr) RGType,RGSTyp,RGLRem,RGTNum,RGTKey,RGTAct,RGTReg,RGTTyp,RGForm,RGTLen,RGText(1:RGTLen)
        RGPtr=RGPtr + 6 + RGLRem

        do i=1,nrows(itab)
         ir=irows(i,itab)
         if(ir.le.0) then
          !Write out DataType 2 record (Mid-Table Text).
          RGType=2         ! Record type (2 for mid-table text without data (stub only)
          RGSTyp=1
          RGRNum=i      ! row number
          RGRKey=repeat(' ',16)
          RGTTyp=1
          RGTLen=len_trim(rowlab(i,itab))
          RGText=rowlab(i,itab)(1:RGTLen)
          RGForm=DRForm(i,itab)
          RGLRem=4*2 + 6 + 6 + 16 + RGTLen
          !length of fields   2      2      2      2      6      2      6     2       6      2      RGTLen
          write(21,rec=RGPtr) RGType,RGSTyp,RGLRem,RGTNum,RGTKey,RGRNum,RGRKey,RGTTyp,RGForm,RGTLen,RGText(1:RGTLen)
          RTTRow=RTTRow+1   ! Count of all row records in the table
          RGPtr=RGPtr + 6 + RGLRem    ! next byte location (number of bytes written so far)
         else
          !Write out DataType 3 record (Data).
          RGType=3        ! Record type (3 for mid-table text with data)
          RGSTyp=1
          RGRNum=i
          RGRKey=DRName(i,itab)
          RGTTyp=1
          RGTLen=len_trim(rowlab(i,itab))
          RGText=rowlab(i,itab)(1:RGTLen)
          RGForm=DRForm(i,itab)
          if(RowFmt(i,itab).eq.0) then
           RGDTyp=TabFmt(itab)
          else
           RGDTyp=RowFmt(i,itab)
          endif
          !RGDTyp=1
          RGDFYr=1995
          RGDLYr=max(IJUMPCALYR,LASTCALYR)
          is=1
          do iyr=1,RGDLYr-RGDFYr+1
           RGData(iyr)=tabn(ir,iyr+5,is)
          end do
! Collect variable length row strings and find their lengths
          RowStrings(1)=graph_units(i,itab)
          RowStrings(2)=graph_label(i,itab)
          RowStrings(3)=trim(data_type(i,itab))//'|'//trim(sub_data_type(i,itab))
          RowStrings(4)=trim(sector(i,itab))//'|'//trim(subsector(i,itab))
          RowStrings(5)=trim(energy_source(i,itab))//'|'//trim(sub_energy_source(i,itab))
          RowStrings(6)=geography(i,itab)
          RowStrings(7)=var_units(i,itab)
   !!!!   RowStrings(8)=varname(i,itab)     ! api variable name...not used by grafnem so use space for expression
          RowStrings(8)=expression(i,itab)  ! ftab.f expression for the row.

          LenRowStrs=0
          do j=1,8
            LenRowStr(j)=len_trim(RowStrings(j))
            if(LenRowStr(j).eq.0) LenRowStr(j)=1  ! may help if a minimum 1 character blank string written
          enddo
          LenRowStrs=sum(LenRowStr(1:8))

          RGLRem=4*2 + 6 + 6 + 16 + RGTLen + 3*2 + (RGDLYr-RGDFYr+1)*4 + LenRowStrs + 2*8

          write(21,rec=RGPtr) RGType,RGSTyp,RGLRem,RGTNum,RGTKey,RGRNum,RGRKey,RGTTyp,RGForm,RGTLen,RGText(1:RGTLen), &
           RGDTyp,RGDFYr,RGDLYr,(RGData(iyr),iyr=1,RGDLYr-RGDFYr+1), &
           (LenRowStr(j),      RowStrings(j)(1:LenRowStr(j))  , j=1,8)    ! each string, preceded by its length to allow variable-length storage
          RTTRow=RTTRow+1    ! Count of all row records in the table
          RTDRow=RTDRow+1    ! Count of data row records in the table
          RDKey(RTDRow)=RGRKey  ! unique string identifier (unique within this table) for a data row
          RDLoc(RTDRow)=RGPtr   ! byte location for start of this data row
          RGPtr=RGPtr + 6 + RGLRem   ! next byte location (number of bytes written so far)
         endif
        end do

        !Add footnotes to the new RAN file.
        do i=1,nfoot(itab)
         !Write out DataType 4 record (Footnote).
         RGType=4        ! Record type (4 for footnotes)
         RGSTyp=1
         RGTTyp=1
         RGTLen=len_trim(footnt(i,itab))
         RGText=footnt(i,itab)(1:RGTLen)
         RGForm=repeat(' ',6)
         RGLRem=3*2 + 6 + 6 + RGTLen
         write(21,rec=RGPtr) RGType,RGSTyp,RGLRem,RGTNum,RGTKey,RGTTyp,RGForm,RGTLen,RGText(1:RGTLen)
         RGPtr=RGPtr + 6 + RGLRem   ! next byte location (number of bytes written so far)
        end do

        !Write out the data for the table location header and the data location header.
        write(21,rec=RXPtr) RTNum
        write(21,rec=RTPtr) RTKey,RTTRow,RTDRow,RTDLoc,RTILoc
        RTPtr=RTPtr + 6 + 2*2 + 2*4
        write(21,rec=RDPtr) (RDKey(i),RDLoc(i),i=1,RTDRow)
        RDPtr=RDPtr + RTDRow*(16 + 4)

      endif

!  Exit now if the report switch for this table is disabled, or
!  if this is a regional part of table  and the regional switch is off
      if (.not. report_switch(itab)) return
      if (IPREGD .ne. 1 .and. IREGN .ge. 1) return

1000  FORMAT(A,A,A,A,A)
1001  FORMAT('  Table ',I2,'.  ',A)
1002  FORMAT('  Table ',I1,'.  ',A)
1007  FORMAT('  Table ',I3,'.  ',A)
1005  FORMAT('  ',A35,36A)
1003  FORMAT(' ',A32,35A)
1004  FORMAT('+ ',A31,35A)
1103  FORMAT(' ',A44,35A)
      Redo_vbar=NSCEN*ICOLWD-12
      IF (Redo_vbar .LT. 0) Redo_vbar=NSCEN*ICOLWD*2-12
1104  FORMAT('+ ',A43,<Redo_vbar>X,35A)
      L=LEN_TRIM(TABHED(1,ITAB))
      IF(L.GT.10)THEN
      IF(TABHED(1,ITAB)(L-10:L).EQ.'(Continued)') &
         TABHED(1,ITAB)(L-10:L)=   '           '
      ENDIF
      IC=0
      I=1
      IE=NCOLS
      IF(IE.GT.MAXCOL) IE=MAXCOL
30    CONTINUE
      NLINE=99

      DO 50 IROW=1,NROWS(ITAB)
        if( ftabbone.ne.1 .and. DRForm(IROW,ITAB)(6:6).eq.'1') cycle ! skip bonus rows and page breaks
! WRITE HEADING IF AT PAGE BREAK
        IF(IPAGE(ITAB,1).EQ.IROW.OR.IPAGE(ITAB,2).EQ.IROW.OR. &
           IPAGE(ITAB,3).EQ.IROW.OR.IPAGE(ITAB,4).EQ.IROW.OR. &
           IROW.EQ.1) THEN
           IF(IROW.GT.1) THEN
! WRITE BOTTOM-OF-PAGE STUFF
            WRITE(20,1004) LSCORE,(USCORE(:CW(II)),II=I,IE)
      IF(NSCEN.GT.1) WRITE(20,1004) LSCORE,(USBAR(II)(:CW(II)),II=I,IE)
            CENTERLN=' '//VBARLN(2:)
            LCOM=LEN_TRIM(BOXHED)+3
            CENTERLN(4:LCOM)=BOXHED
            WRITE(20,'(A)') CENTERLN
            WRITE(20,1004) LSCORE,(USCORE(:CW(II)),II=I,IE)
!            CENTERLN=EIA
!            CALL CENTER(CENTERLN,MASKLN,LRECL+4)
!            CENTERLN(1:1) = ' '
!            WRITE(20,'(A)') CENTERLN
          ENDIF
! WRITE TOP-OF-PAGE STUFF
          CENTERLN=NEMS
          CALL CENTER(CENTERLN,MASKLN,LRECL+4)
          CENTERLN(1:1)='1'
          WRITE(20,'(A)') CENTERLN
!          CENTERLN=DISCLM
!          CALL CENTER(CENTERLN,MASKLN,LRECL+4)
!          WRITE(20,'(A)') CENTERLN
          WRITE(20,1004) LSCORE,(USCORE(:CW(II)),II=I,IE)
          IF (ITAB.GE.100) THEN
            WRITE(CENTERLN,1007) ITAB,TABHED(1,ITAB)
          ELSEIF (ITAB.GE.10) THEN
            WRITE(CENTERLN,1001) ITAB,TABHED(1,ITAB)
          ELSEIF (ITAB.LT.10) THEN
            WRITE(CENTERLN,1002) ITAB,TABHED(1,ITAB)
          ENDIF
          CALL CENTER(CENTERLN,VBARLN,LRECL+4)
          WRITE(20,1000) CENTERLN
          DO II=2,NHED(ITAB)
            CENTERLN=TABHED(II,ITAB)
            CALL CENTER(CENTERLN,VBARLN,LRECL+4)
            WRITE(20,'(A)') CENTERLN
          ENDDO
          WRITE(20,1004) LSCORE,(USCORE(:CW(II)),II=I,IE)
          L=LEN_TRIM(FRSCEN)
          WRITE(20,1003) ' ',(VBAR(II)(:CW(II)),II=I,IE)
          WRITE(20,1003) ROWHED(ITAB), &
           (COLLAB(II)(:CW(II)),II=I,IE)
          WRITE(20,1003) LSCORE,(USBAR(II)(:CW(II)),II=I,IE)
          WRITE(20,1004) LSCORE,(USCORE(:CW(II)),II=I,IE)
          IF(NSCEN.GT.1)WRITE(20,1004) ' ',(SCENID(II)(:CW(II)),II=I,IE)
          NLINE=9
          IF(IC.EQ.0)THEN
            IC=1
            L=LEN_TRIM(TABHED(1,ITAB))
            TABHED(1,ITAB)(L+1:) = ' (Continued)'
          ENDIF
        ENDIF
        IR=IROWS(IROW,ITAB)
        IF(IR.EQ.0) THEN
          WRITE(20,1103) ROWLAB(IROW,ITAB)
          IF (LEN_TRIM(ROWLAB(IROW,ITAB)) .LE. 35) THEN
            IF(NSCEN.GT.1)WRITE(20,1004) ' ',(VBAR2(II)(:CW(II)),II=I,IE)
          ELSE
            IF(NSCEN.GT.1)WRITE(20,1104) ' ',(VBAR2(II)(:CW(II)),II=I,IE-NSCEN)
          ENDIF
          IF(NSCEN.EQ.1)WRITE(20,'(A)') VBARLN
        ELSE
          TEMPLAB = ROWLAB(IROW,ITAB)
          DO DOTLEAD=LEN_TRIM(ROWLAB(IROW,ITAB))+1,35
             TEMPLAB(DOTLEAD:DOTLEAD)='.'
          ENDDO
          WRITE(20,1003) TEMPLAB,VERT, &
          TABC(IR,ICOLS(I),ISCEN(I))(2:CW(I)), &
          (TABC(IR,ICOLS(II),ISCEN(II))(:CW(II)),II=I+1,IE)
          IF(NSCEN.GT.1)WRITE(20,1004) ' ',(VBAR2(II)(:CW(II)),II=I,IE)
        ENDIF
        NLINE=NLINE+1
50    CONTINUE
! WRITE BOTTOM-OF-PAGE STUFF
      WRITE(20,1004) LSCORE,(USCORE(:CW(II)),II=I,IE)
      IF(NSCEN.GT.1) WRITE(20,1004) LSCORE,(USBAR(II)(:CW(II)),II=I,IE)
      CENTERLN=' '//VBARLN(2:)
      LCOM=LEN_TRIM(BOXHED)+3
      CENTERLN(4:LCOM)=BOXHED
      WRITE(20,'(A)') CENTERLN
      WRITE(20,1004) LSCORE,(USCORE(:CW(II)),II=I,IE)
!      CENTERLN=EIA
!      CALL CENTER(CENTERLN,MASKLN,LRECL+4)
!      CENTERLN(1:1) = ' '
!      WRITE(20,'(A)') CENTERLN
      IF(IE.LT.NCOLS)THEN
        I=IE+1
        IE=I+MAXCOL-1
        IF(IE.GT.NCOLS) IE=NCOLS
        GOTO 30
      ENDIF
! restore table heading if "continued" was added previously.  Might need for regional tables
      l=len_trim(tabhed(1,itab))
      ic=index(tabhed(1,itab),' (Continued)')
      if(ic.gt.0) then
         l=ic-1
      else
         ic=l+1
      endif
      tabhed(1,itab)(ic:)=' '

 ! GENERATE THE XML SPREADSHEET FILE
      IF(ICRWK1.EQ.1) THEN
         CALL FXML(ITAB,IGROWO,IGRWYR,SCENID,IREGN)
         if (ftabbone.eq.0) then ! if bonus rows are being excluded, then this is a publication candidate.  Create table browser database
           ! CALL FXMLDB(ITAB,IGROWO,IGRWYR,SCENID,IREGN)      ! not used currently, but save for possible future use
           CALL Browser_Out(ITAB,IGROWO,IGRWYR,SCENID,IREGN)   ! browser csv files
         endif
      ENDIF
      CALL API_Out(ITAB,IGROWO,IGRWYR,SCENID,IREGN)       ! database/api output in flat csv file
      RETURN
      END
!********************************************************************
      subroutine reverse(a,n)
! reverse the ordering of bytes in a number up to length 8
      character*1 a(n)
      character*1 b(8)
      integer*2 num,i,j
      num=min(n,8)
      do i=1,num
        j=num-i+1
        b(j)=a(i)
      enddo
      do i=1,num
        a(i)=b(i)
      enddo
      return
      end
!********************************************************************
      subroutine cwrite(line)
! calls routine stored in cio4wk1.c to write a string to
! a binary file.
      implicit none
      integer i,l
      character*(*) line
      l=len(line)
      do i=1,l
         call cwkwrite(line(i:i) )
      enddo
      return
      end
!********************************************************************
      SUBROUTINE CENTER(CENTERLN,MASK,NIN)
      IMPLICIT NONE
      CHARACTER*(*) CENTERLN,MASK
      CHARACTER*166 HOLD
      INTEGER N,NIN,IS,IE,IC,LS
      N=NIN
      IF(N.GT.166) N=166
      HOLD=MASK
      HOLD(1:1)=' '
      IS=1
      IE=N
      DO WHILE (CENTERLN(IS:IS).EQ.' '.AND.IS.LT.N)
        IS=IS+1
      ENDDO
      DO WHILE (CENTERLN(IE:IE).EQ.' '.AND.IE.GT.IS)
        IE=IE-1
      ENDDO
      LS=IE-IS+1
      IF(LS.GT.166) LS=166
      IC=((N-LS)/2)+1
      IF(IC.LT.1) IC=1
      HOLD(IC:IC+LS-1)=CENTERLN(IS:IE)
      CENTERLN=HOLD
      RETURN
      END
!********************************************************************
      SUBROUTINE FCOPY(ITAB,IR,IS)
       IMPLICIT NONE
!
! COPIES NUMERIC DATA TO REPORT TABLE
      include 'parametr'
      include 'ftable'
      INTEGER I,IYR,ITAB,IR,IS
      DO 300 IYR=1,MAXYR
        DO 5 I=1,MAXIRW
          TABN(I,IYR,IS)=0.
5       CONTINUE
        IF(ITAB.LE.50) THEN
          CALL FCOPY1(ITAB,IR,IS,I,IYR)
        ELSEIF(ITAB.LE.100) THEN
          CALL FCOPY2(ITAB,IR,IS,I,IYR)
        ELSE
          CALL FCOPY3(ITAB,IR,IS,I,IYR)
        ENDIF
300   CONTINUE
      RETURN
      END
!********************************************************************
      SUBROUTINE FCOPY1(ITAB,IR,IS,I,IYR)
      IMPLICIT NONE
!
! COPIES NUMERIC DATA TO REPORT TABLE
      include 'parametr'
      include 'ftable'
      INTEGER I,IYR,ITAB,IR,IS
!     ---------------------------------------
        IF(ITAB.EQ.1) THEN
          DO 10 I=1,NTAB001
            TABN(I,IYR,IS)=T1(I,IYR,IS)
10        CONTINUE
        ELSEIF(ITAB.EQ.2) THEN
          DO 20 I=1,NTAB002
            TABN(I,IYR,IS)=T2(I,IR,IYR,IS)
20        CONTINUE
        ELSEIF(ITAB.EQ.3) THEN
          DO 30 I=1,NTAB003
            TABN(I,IYR,IS)=T3(I,IR,IYR,IS)
30        CONTINUE
        ELSEIF(ITAB.EQ.4) THEN
          DO 40 I=1,NTAB004
            TABN(I,IYR,IS)=T4(I,IYR,IS)
40        CONTINUE
        ELSEIF(ITAB.EQ.5) THEN
          DO 50 I=1,NTAB005
            TABN(I,IYR,IS)=T5(I,IYR,IS)
50        CONTINUE
        ELSEIF(ITAB.EQ.6) THEN
          DO 60 I=1,NTAB006
            TABN(I,IYR,IS)=T6(I,IYR,IS)
60        CONTINUE
        ELSEIF(ITAB.EQ.7) THEN
          DO 70 I=1,NTAB007
            TABN(I,IYR,IS)=T7(I,IYR,IS)
70        CONTINUE
        ELSEIF(ITAB.EQ.8) THEN
          DO 80 I=1,NTAB008
            TABN(I,IYR,IS)=T8(I,IYR,IS)
80        CONTINUE
        ELSEIF(ITAB.EQ.9) THEN
          DO 90 I=1,NTAB009
            TABN(I,IYR,IS)=T9(I,IYR,IS)
90        CONTINUE
        ELSEIF(ITAB.EQ.10) THEN
          DO 100 I=1,NTAB010
            TABN(I,IYR,IS)=T10(I,IYR,IS)
100       CONTINUE
        ELSEIF(ITAB.EQ.11) THEN
          DO 110 I=1,NTAB011
            TABN(I,IYR,IS)=T11(I,IYR,IS)
110       CONTINUE
        ELSEIF(ITAB.EQ.12) THEN
          DO 120 I=1,NTAB012
            TABN(I,IYR,IS)=T12(I,IYR,IS)
120       CONTINUE
        ELSEIF(ITAB.EQ.13) THEN
          DO 130 I=1,NTAB013
            TABN(I,IYR,IS)=T13(I,IYR,IS)
130       CONTINUE
        ELSEIF(ITAB.EQ.14) THEN
          DO 140 I=1,NTAB014
            TABN(I,IYR,IS)=T14(I,IYR,IS)
140       CONTINUE
        ELSEIF(ITAB.EQ.15) THEN
          DO 141 I=1,NTAB015
            TABN(I,IYR,IS)=T15(I,IYR,IS)
141       CONTINUE
        ELSEIF(ITAB.EQ.16) THEN
          DO 142 I=1,NTAB016
142         TABN(I,IYR,IS)=T16(I,IYR,IS)
        ELSEIF (ITAB.EQ.17) THEN
          DO 143 I=1,NTAB017
143         TABN(I,IYR,IS)=T17(I,IR,IYR,IS)
        ELSEIF(ITAB.EQ.18) THEN
          DO 144 I=1,NTAB018
144         TABN(I,IYR,IS)=T18(I,IYR,IS)
        ELSEIF (ITAB.EQ.19) THEN
          DO 145 I=1,NTAB019
145         TABN(I,IYR,IS)=T19(I,IYR,IS)
        ELSEIF (ITAB.EQ.20) THEN
          DO 146 I=1,NTAB020
146         TABN(I,IYR,IS)=T20(I,IYR,IS)
        ELSEIF (ITAB.EQ.21) THEN
          DO 147 I=1,NTAB021
147         TABN(I,IYR,IS)=T21(I,IR,IYR,IS)
        ELSEIF (ITAB.EQ.22) THEN
          DO 148 I=1,NTAB022
148         TABN(I,IYR,IS)=T22(I,IYR,IS)
        ELSEIF (ITAB.EQ.23) THEN
          DO 149 I=1,NTAB023
149         TABN(I,IYR,IS)=T23(I,IYR,IS)
        ELSEIF (ITAB.EQ.24) THEN
          DO 150 I=1,NTAB024
150         TABN(I,IYR,IS)=T24(I,IYR,IS)
        ELSEIF (ITAB.EQ.25) THEN
          DO 151 I=1,NTAB025
151         TABN(I,IYR,IS)=T25(I,IYR,IS)
        ELSEIF (ITAB.EQ.26) THEN
          DO 152 I=1,NTAB026
152         TABN(I,IYR,IS)=T26(I,IYR,IS)
        ELSEIF (ITAB.EQ.27) THEN
          DO 153 I=1,NTAB027
153         TABN(I,IYR,IS)=T27(I,IYR,IS)
        ELSEIF (ITAB.EQ.28) THEN
          DO 154 I=1,NTAB028
154         TABN(I,IYR,IS)=T28(I,IYR,IS)
        ELSEIF (ITAB.EQ.29) THEN
          DO 155 I=1,NTAB029
155         TABN(I,IYR,IS)=T29(I,IYR,IS)
        ELSEIF (ITAB.EQ.30) THEN
          DO 156 I=1,NTAB030
156         TABN(I,IYR,IS)=T30(I,IYR,IS)
        ELSEIF (ITAB.EQ.31) THEN
          DO 157 I=1,NTAB031
157         TABN(I,IYR,IS)=T31(I,IYR,IS)
        ELSEIF (ITAB.EQ.32) THEN
          DO 158 I=1,NTAB032
158         TABN(I,IYR,IS)=T32(I,IYR,IS)
        ELSEIF (ITAB.EQ.33) THEN
          DO 159 I=1,NTAB033
159         TABN(I,IYR,IS)=T33(I,IYR,IS)
        ELSEIF (ITAB.EQ.34) THEN
          DO 160 I=1,NTAB034
160         TABN(I,IYR,IS)=T34(I,IR,IYR,IS)
        ELSEIF (ITAB.EQ.35) THEN
          DO 161 I=1,NTAB035
161         TABN(I,IYR,IS)=T35(I,IYR,IS)
        ELSEIF (ITAB.EQ.36) THEN
          DO 162 I=1,NTAB036
162         TABN(I,IYR,IS)=T36(I,IYR,IS)
        ELSEIF (ITAB.EQ.37) THEN
          DO 163 I=1,NTAB037
163         TABN(I,IYR,IS)=T37(I,IYR,IS)
        ELSEIF (ITAB.EQ.38) THEN
          DO 164 I=1,NTAB038
164         TABN(I,IYR,IS)=T38(I,IYR,IS)
        ELSEIF (ITAB.EQ.39) THEN
          DO 165 I=1,NTAB039
165         TABN(I,IYR,IS)=T39(I,IYR,IS)
        ELSEIF (ITAB.EQ.40) THEN
          DO 166 I=1,NTAB040
166         TABN(I,IYR,IS)=T40(I,IYR,IS)
        ELSEIF (ITAB.EQ.41) THEN
          DO 167 I=1,NTAB041
167         TABN(I,IYR,IS)=T41(I,IYR,IS)
        ELSEIF (ITAB.EQ.42) THEN
          DO 168 I=1,NTAB042
168         TABN(I,IYR,IS)=T42(I,IYR,IS)
        ELSEIF (ITAB.EQ.43) THEN
          DO 169 I=1,NTAB043
169         TABN(I,IYR,IS)=T43(I,IYR,IS)
        ELSEIF (ITAB.EQ.44) THEN
          DO 170 I=1,NTAB044
170         TABN(I,IYR,IS)=T44(I,IYR,IS)
        ELSEIF (ITAB.EQ.45) THEN
          DO 171 I=1,NTAB045
171         TABN(I,IYR,IS)=T45(I,IYR,IS)
        ELSEIF (ITAB.EQ.46) THEN
          DO 172 I=1,NTAB046
172         TABN(I,IYR,IS)=T46(I,IYR,IS)
        ELSEIF (ITAB.EQ.47) THEN
          DO 173 I=1,NTAB047
173         TABN(I,IYR,IS)=T47(I,IYR,IS)
        ELSEIF (ITAB.EQ.48) THEN
          DO 174 I=1,NTAB048
174         TABN(I,IYR,IS)=T48(I,IR,IYR,IS)
        ELSEIF (ITAB.EQ.49) THEN
          DO 175 I=1,NTAB049
175         TABN(I,IYR,IS)=T49(I,IYR,IS)
        ELSEIF (ITAB.EQ.50) THEN
          DO 176 I=1,NTAB050
176         TABN(I,IYR,IS)=T50(I,IYR,IS)
        ENDIF
      RETURN
      END
!********************************************************************
      SUBROUTINE FCOPY2(ITAB,IR,IS,I,IYR)
      IMPLICIT NONE
!
! COPIES NUMERIC DATA TO REPORT TABLE
      include 'parametr'
      include 'ftable'
      INTEGER I,IYR,ITAB,IR,IS
!     ---------------------------------------
         IF (ITAB.EQ.51) THEN
           DO 177 I= 1, NTAB051
177          TABN(I,IYR,IS)=T51(I,IYR,IS)
         ELSEIF (ITAB.EQ.52) THEN
           DO 178 I= 1, NTAB052
178          TABN(I,IYR,IS)=T52(I,IYR,IS)
         ELSEIF (ITAB.EQ.53) THEN
           DO 179 I= 1, NTAB053
179          TABN(I,IYR,IS)=T53(I,IYR,IS)
         ELSEIF (ITAB.EQ.54) THEN
           DO 180 I= 1, NTAB054
180          TABN(I,IYR,IS)=T54(I,IYR,IS)
         ELSEIF (ITAB.EQ.55) THEN
           DO 181 I= 1, NTAB055
181          TABN(I,IYR,IS)=T55(I,IYR,IS)
         ELSEIF (ITAB.EQ.56) THEN
           DO 182 I= 1, NTAB056
182          TABN(I,IYR,IS)=T56(I,IYR,IS)
         ELSEIF (ITAB.EQ.57) THEN
           DO 183 I= 1, NTAB057
183          TABN(I,IYR,IS)=T57(I,IYR,IS)
         ELSEIF (ITAB.EQ.58) THEN
           DO 184 I= 1, NTAB058
184          TABN(I,IYR,IS)=T58(I,IYR,IS)
         ELSEIF (ITAB.EQ.59) THEN
           DO 185 I= 1, NTAB059
185          TABN(I,IYR,IS)=T59(I,IR,IYR,IS)
         ELSEIF (ITAB.EQ.60) THEN
           DO 186 I= 1, NTAB060
186          TABN(I,IYR,IS)=T60(I,IYR,IS)
         ELSEIF (ITAB.EQ.61) THEN
           DO 187 I= 1, NTAB061
187          TABN(I,IYR,IS)=T61(I,IR,IYR,IS)
         ELSEIF (ITAB.EQ.62) THEN
           DO 188 I= 1, NTAB062
188          TABN(I,IYR,IS)=T62(I,IR,IYR,IS)
         ELSEIF (ITAB.EQ.63) THEN
           DO 189 I= 1, NTAB063
189          TABN(I,IYR,IS)=T63(I,IYR,IS)
         ELSEIF (ITAB.EQ.64) THEN
           DO 190 I= 1, NTAB064
190          TABN(I,IYR,IS)=T64(I,IYR,IS)
         ELSEIF (ITAB.EQ.65) THEN
           DO 191 I= 1, NTAB065
191          TABN(I,IYR,IS)=T65(I,IYR,IS)
         ELSEIF (ITAB.EQ.66) THEN
           DO 192 I= 1, NTAB066
192          TABN(I,IYR,IS)=T66(I,IYR,IS)
         ELSEIF (ITAB.EQ.67) THEN
           DO 193 I= 1, NTAB067
193          TABN(I,IYR,IS)=T67(I,IR,IYR,IS)
         ELSEIF (ITAB.EQ.68) THEN
           DO 194 I= 1, NTAB068
194          TABN(I,IYR,IS)=T68(I,IYR,IS)
         ELSEIF (ITAB.EQ.69) THEN
           DO 195 I= 1, NTAB069
195          TABN(I,IYR,IS)=T69(I,IYR,IS)
         ELSEIF (ITAB.EQ.70) THEN
           DO 196 I= 1, NTAB070
196          TABN(I,IYR,IS)=T70(I,IR,IYR,IS)
         ELSEIF (ITAB.EQ.71) THEN
           DO 197 I= 1, NTAB071
197          TABN(I,IYR,IS)=T71(I,IYR,IS)
         ELSEIF (ITAB.EQ.72) THEN
           DO 198 I= 1, NTAB072
198          TABN(I,IYR,IS)=T72(I,IYR,IS)
         ELSEIF (ITAB.EQ.73) THEN
           DO 199 I= 1, NTAB073
199          TABN(I,IYR,IS)=T73(I,IYR,IS)
         ELSEIF (ITAB.EQ.74) THEN
           DO 200 I= 1, NTAB074
200          TABN(I,IYR,IS)=T74(I,IYR,IS)
         ELSEIF (ITAB.EQ.75) THEN
           DO 201 I= 1, NTAB075
201          TABN(I,IYR,IS)=T75(I,IYR,IS)
         ELSEIF (ITAB.EQ.76) THEN
           DO 202 I= 1, NTAB076
202          TABN(I,IYR,IS)=T76(I,IYR,IS)
         ELSEIF (ITAB.EQ.77) THEN
           DO 203 I= 1, NTAB077
203          TABN(I,IYR,IS)=T77(I,IYR,IS)
         ELSEIF (ITAB.EQ.78) THEN
           DO 204 I= 1, NTAB078
204          TABN(I,IYR,IS)=T78(I,IYR,IS)
         ELSEIF (ITAB.EQ.79) THEN
           DO 205 I= 1, NTAB079
205          TABN(I,IYR,IS)=T79(I,IYR,IS)
         ELSEIF (ITAB.EQ.80) THEN
           DO 206 I= 1, NTAB080
206          TABN(I,IYR,IS)=T80(I,IYR,IS)
         ELSEIF (ITAB.EQ.81) THEN
           DO 207 I= 1, NTAB081
207          TABN(I,IYR,IS)=T81(I,IYR,IS)
         ELSEIF (ITAB.EQ.82) THEN
           DO 208 I= 1, NTAB082
208          TABN(I,IYR,IS)=T82(I,IYR,IS)
         ELSEIF (ITAB.EQ.83) THEN
           DO 209 I= 1, NTAB083
209          TABN(I,IYR,IS)=T83(I,IYR,IS)
         ELSEIF (ITAB.EQ.84) THEN
           DO 210 I= 1, NTAB084
210          TABN(I,IYR,IS)=T84(I,IYR,IS)
         ELSEIF (ITAB.EQ.85) THEN
           DO 211 I= 1, NTAB085
211          TABN(I,IYR,IS)=T85(I,IYR,IS)
         ELSEIF (ITAB.EQ.86) THEN
           DO 212 I= 1, NTAB086
212          TABN(I,IYR,IS)=T86(I,IYR,IS)
         ELSEIF (ITAB.EQ.87) THEN
           DO 213 I= 1, NTAB087
213          TABN(I,IYR,IS)=T87(I,IYR,IS)
         ELSEIF (ITAB.EQ.88) THEN
           DO 214 I= 1, NTAB088
214          TABN(I,IYR,IS)=T88(I,IYR,IS)
         ELSEIF (ITAB.EQ.89) THEN
           DO 215 I= 1, NTAB089
215          TABN(I,IYR,IS)=T89(I,IYR,IS)
         ELSEIF (ITAB.EQ.90) THEN
           DO 216 I= 1, NTAB090
216          TABN(I,IYR,IS)=T90(I,IYR,IS)
         ELSEIF (ITAB.EQ.91) THEN
           DO 217 I= 1, NTAB091
217          TABN(I,IYR,IS)=T91(I,IYR,IS)
         ELSEIF (ITAB.EQ.92) THEN
           DO 218 I= 1, NTAB092
218          TABN(I,IYR,IS)=T92(I,IYR,IS)
         ELSEIF (ITAB.EQ.93) THEN
           DO 219 I= 1, NTAB093
219          TABN(I,IYR,IS)=T93(I,IR,IYR,IS)
         ELSEIF (ITAB.EQ.94) THEN
           DO 220 I= 1, NTAB094
220          TABN(I,IYR,IS)=T94(I,IYR,IS)
         ELSEIF (ITAB.EQ.95) THEN
           DO 221 I= 1, NTAB095
221          TABN(I,IYR,IS)=T95(I,IYR,IS)
         ELSEIF (ITAB.EQ.96) THEN
           DO 222 I= 1, NTAB096
222          TABN(I,IYR,IS)=T96(I,IYR,IS)
         ELSEIF (ITAB.EQ.97) THEN
           DO I= 1, NTAB097
             TABN(I,IYR,IS)=T97(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.98) THEN
           DO I= 1, NTAB098
             TABN(I,IYR,IS)=T98(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.99) THEN
           DO I= 1, NTAB099
             TABN(I,IYR,IS)=T99(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.100) THEN
           DO I= 1, NTAB100
             TABN(I,IYR,IS)=T100(I,IYR,IS)
          ENDDO
        ENDIF
      RETURN
      END
!********************************************************************
      SUBROUTINE FCOPY3(ITAB,IR,IS,I,IYR)
       IMPLICIT NONE
!
! COPIES NUMERIC DATA TO REPORT TABLE
      include 'parametr'
      include 'ftable'
      INTEGER I,IYR,ITAB,IR,IS
!     --------------------------------------------
        IF (ITAB.EQ.101) THEN
           DO I= 1, NTAB101
             TABN(I,IYR,IS)=T101(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.102) THEN
           DO I= 1, NTAB102
             TABN(I,IYR,IS)=T102(I,IR,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.103) THEN
           DO I= 1, NTAB103
             TABN(I,IYR,IS)=T103(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.104) THEN
           DO I= 1, NTAB104
             TABN(I,IYR,IS)=T104(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.105) THEN
           DO I= 1, NTAB105
             TABN(I,IYR,IS)=T105(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.106) THEN
           DO I= 1, NTAB106
             TABN(I,IYR,IS)=T106(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.107) THEN
           DO I= 1, NTAB107
             TABN(I,IYR,IS)=T107(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.108) THEN
           DO I= 1, NTAB108
             TABN(I,IYR,IS)=T108(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.109) THEN
           DO I= 1, NTAB109
             TABN(I,IYR,IS)=T109(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.110) THEN
           DO I= 1, NTAB110
             TABN(I,IYR,IS)=T110(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.111) THEN
           DO I= 1, NTAB111
             TABN(I,IYR,IS)=T111(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.112) THEN
           DO I= 1, NTAB112
             TABN(I,IYR,IS)=T112(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.113) THEN
           DO I= 1, NTAB113
             TABN(I,IYR,IS)=T113(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.114) THEN
           DO I= 1, NTAB114
             TABN(I,IYR,IS)=T114(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.115) THEN
           DO I= 1, NTAB115
             TABN(I,IYR,IS)=T115(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.116) THEN
           DO I= 1, NTAB116
             TABN(I,IYR,IS)=T116(I,IR,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.117) THEN
           DO I= 1, NTAB117
             TABN(I,IYR,IS)=T117(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.118) THEN
           DO I= 1, NTAB118
             TABN(I,IYR,IS)=T118(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.119) THEN
           DO I= 1, NTAB119
             TABN(I,IYR,IS)=T119(I,IR,IYR,IS)
          ENDDO
!  skip table 120
         ELSEIF (ITAB.EQ.121) THEN
           DO I= 1, NTAB121
             TABN(I,IYR,IS)=T121(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.122) THEN
           DO I= 1, NTAB122
             TABN(I,IYR,IS)=T122(I,IR,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.123) THEN
           DO I= 1, NTAB123
             TABN(I,IYR,IS)=T123(I,IR,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.124) THEN
           DO I= 1, NTAB124
             TABN(I,IYR,IS)=T124(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.125) THEN
           DO I= 1, NTAB125
             TABN(I,IYR,IS)=T125(I,IR,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.126) THEN
           DO I= 1, NTAB126
             TABN(I,IYR,IS)=T126(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.127) THEN
           DO I= 1, NTAB127
             TABN(I,IYR,IS)=T127(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.128) THEN
           DO I= 1, NTAB128
             TABN(I,IYR,IS)=T128(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.129) THEN
           DO I= 1, NTAB129
             TABN(I,IYR,IS)=T129(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.130) THEN
           DO I= 1, NTAB130
             TABN(I,IYR,IS)=T130(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.131) THEN
           DO I= 1, NTAB131
             TABN(I,IYR,IS)=T131(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.132) THEN
           DO I= 1, NTAB132
             TABN(I,IYR,IS)=T132(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.133) THEN
           DO I= 1, NTAB133
             TABN(I,IYR,IS)=T133(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.134) THEN
           DO I= 1, NTAB134
             TABN(I,IYR,IS)=T134(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.135) THEN
           DO I= 1, NTAB135
             TABN(I,IYR,IS)=T135(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.136) THEN
           DO I= 1, NTAB136
             TABN(I,IYR,IS)=T136(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.137) THEN
           DO I= 1, NTAB137
             TABN(I,IYR,IS)=T137(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.138) THEN
           DO I= 1, NTAB138
             TABN(I,IYR,IS)=T138(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.139) THEN
           DO I= 1, NTAB139
             TABN(I,IYR,IS)=T139(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.140) THEN
           DO I= 1, NTAB140
             TABN(I,IYR,IS)=T140(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.141) THEN
           DO I= 1, NTAB141
             TABN(I,IYR,IS)=T141(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.142) THEN
           DO I= 1, NTAB142
             TABN(I,IYR,IS)=T142(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.143) THEN
           DO I= 1, NTAB143
             TABN(I,IYR,IS)=T143(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.144) THEN
           DO I= 1, NTAB144
             TABN(I,IYR,IS)=T144(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.145) THEN
           DO I= 1, NTAB145
             TABN(I,IYR,IS)=T145(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.146) THEN
           DO I= 1, NTAB146
             TABN(I,IYR,IS)=T146(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.147) THEN
           DO I= 1, NTAB147
             TABN(I,IYR,IS)=T147(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.148) THEN
           DO I= 1, NTAB148
             TABN(I,IYR,IS)=T148(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.149) THEN
           DO I= 1, NTAB149
             TABN(I,IYR,IS)=T149(I,IYR,IS)
          ENDDO
         ELSEIF (ITAB.EQ.150) THEN
           DO I= 1, NTAB150
             TABN(I,IYR,IS)=T150(I,IYR,IS)
          ENDDO
        ENDIF
        RETURN
        END
!**********************************************************
      SUBROUTINE FGROW( IOPT,IGRWYR,LYR,IS)
       IMPLICIT NONE
!
! COMPUTES GROWTH RATES FOR LAST COLUMN.  IF IOPT=2 COMPUTES ROWSUM
      include 'parametr'
      include 'ftable'
      REAL RATIO,RAISE,RN,ROWSUM
      INTEGER I,IYR,IOPT,IGRWYR,LYR,IS,N
      IF(IOPT.EQ.1.OR.IOPT.GT.2) THEN
        N=LYR-IGRWYR
        IF(N.LE.0) N=1
        RN=N
        RAISE=1./RN
!       --- IOPT 3=T18, 4=T11, 5=T13, 6=T15, 7=T1, 8=T62, 9=T106, 10=T146
        DO 10 I=1,MAXIRW
! For growth rate column, tag as not applicable ('- -') designated rows (via IOPT in ftab.f)
! (Rows designated thusly tend to be showing percents, such as unemployment or interest rates)
! Also tag rows with base year = 0, rows with end year = 0, and rows with a negative ratio
! -999 will get changed to '- -' in FWRITF for text file and in FWK1 for spreadsheet
          IF(TABN(I,IGRWYR,IS).NE.-888. .AND. TABN(I,IGRWYR,IS).NE.-999.) THEN
          IF(TABN(I,IGRWYR,IS).NE.0.) THEN
            IF(IOPT.LT.3.OR.(IOPT.EQ.3.AND.I.NE.9.AND.I.NE.10.AND. &
              I.NE.11.AND.I.NE.15) &
              .OR.(IOPT.EQ.4.AND.I.NE.29) &
              .OR.(IOPT.EQ.5.AND.I.NE.16) &
              .OR.(IOPT.EQ.6.AND.I.NE.55) &
              .OR.(IOPT.EQ.7.AND.I.NE.18) &
              .OR.(IOPT.EQ.8.AND.(I.LT.12.OR.I.GT.35)) &
              .OR.(IOPT.EQ.9.AND.I.NE.42.AND.I.NE.59.AND. &
                   I.NE.61.AND.I.NE.62.AND.I.NE.63.AND. &
                   I.NE.64.AND.I.NE.65.AND. &
                   I.NE.84.AND.I.NE.85.AND.I.NE.86.AND. &
                   I.NE.87.AND.I.NE.88) &
              .OR.(IOPT.EQ.10.AND.I.LT.0)) THEN
              RATIO=TABN(I,LYR,IS)/TABN(I,IGRWYR,IS)
              IF(RATIO .GT. 0.0) THEN
                 TABN(I,MAXYR,IS)=((RATIO**RAISE)-1.)*100.
              ELSE
                 TABN(I,MAXYR,IS)=-999.
              ENDIF
              IF(TABN(I,MAXYR,IS) .GT. 100.0 .OR. RATIO .EQ. 0.0) TABN(I,MAXYR,IS)=-999.
            ELSE
              TABN(I,MAXYR,IS)=-999.
            ENDIF
          ELSE
            TABN(I,MAXYR,IS)=-999.
          ENDIF
          ELSE
            IF(TABN(I,IGRWYR,IS).EQ.-888.) TABN(I,MAXYR,IS)=-888.
            IF(TABN(I,IGRWYR,IS).EQ.-999.) TABN(I,MAXYR,IS)=-999.
          ENDIF
10      CONTINUE
      ELSE
        DO 20 I=1,MAXIRW
          ROWSUM=0.
          DO 15 IYR=IGRWYR,LYR
            ROWSUM=ROWSUM+TABN(I,IYR,IS)
15        CONTINUE
          TABN(I,MAXYR,IS)=ROWSUM
20      CONTINUE
      ENDIF
      RETURN
      END
! ************************************************************************
      SUBROUTINE FOOT(IUNIT,IRW)
       IMPLICIT NONE
! WRITES OUT FOOTNOTES TO END OF TABLES
      include 'parametr'
      include 'ftable'
      INTEGER IF,ITAB,NLINE,L,NEED,IRW,IUNIT
      CHARACTER*50 TITLE,INLAB*166,LABEL*166
      TITLE='Explanatory Notes for Tables'
      NLINE=99
      DO 10 ITAB=1,NTABLE
        IF (.NOT. REPORT_SWITCH(ITAB)) CYCLE
        L=LEN_TRIM(TABHED(1,ITAB))
        IF(L.GT.10)THEN
        IF(TABHED(1,ITAB)(L-10:L).EQ.'(Continued)') &
          TABHED(1,ITAB)(L-10:L)=   '           '
      ENDIF
        IF(NFOOT(ITAB).GT.0) THEN
          NEED=NLINE+NFOOT(ITAB)+3
          IF(NEED.GT.60) THEN
            WRITE(20,100) TITLE
100         FORMAT('1',20X,A//)
            TITLE='Explanatory Notes for Tables (Continued)'
            NLINE=3
          ENDIF
          NLINE=NLINE+3
          WRITE(20,101) 'Table ',ITAB,TABHED(1,ITAB)
101       FORMAT(1X,A,I3,'. ',A/)
          DO 5 IF=1,NFOOT(ITAB)
            NLINE=NLINE+1
            WRITE(20,102) FOOTNT(IF,ITAB)
5         CONTINUE
102       FORMAT(1X,A)
          WRITE(20,*) ' '
        ENDIF
10    CONTINUE

      RETURN
      END
! ***********************************************************************
      SUBROUTINE FINIT
      IMPLICIT NONE
!
! INITIALIZES TABLE ARRAYS
      include 'parametr'
      include 'ftable'
      CALL ZEROR(T1,NTAB001*MAXYR*MAXS)
      CALL ZEROR(T2,MNUMCR*NTAB002*MAXYR*MAXS)
      CALL ZEROR(T3,MNUMCR*NTAB003*MAXYR*MAXS)
      CALL ZEROR(T4,NTAB004*MAXYR*MAXS)
      CALL ZEROR(T5,NTAB005*MAXYR*MAXS)
      CALL ZEROR(T6,NTAB006*MAXYR*MAXS)
      CALL ZEROR(T7,NTAB007*MAXYR*MAXS)
      CALL ZEROR(T8,NTAB008*MAXYR*MAXS)
      CALL ZEROR(T9,NTAB009*MAXYR*MAXS)
      CALL ZEROR(T10,NTAB010*MAXYR*MAXS)
      CALL ZEROR(T11,NTAB011*MAXYR*MAXS)
      CALL ZEROR(T12,NTAB012*MAXYR*MAXS)
      CALL ZEROR(T13,NTAB013*MAXYR*MAXS)
      CALL ZEROR(T14,NTAB014*MAXYR*MAXS)
      CALL ZEROR(T15,NTAB015*MAXYR*MAXS)
      CALL ZEROR(T16,NTAB016*MAXYR*MAXS)
      CALL ZEROR(T17,MNUMCR*NTAB017*MAXYR*MAXS)
      CALL ZEROR(T18,NTAB018*MAXYR*MAXS)
      CALL ZEROR(T19,NTAB019*MAXYR*MAXS)
      CALL ZEROR(T20,NTAB020*MAXYR*MAXS)
! skip table 21
      CALL ZEROR(T22,NTAB022*MAXYR*MAXS)
      CALL ZEROR(T23,NTAB023*MAXYR*MAXS)
      CALL ZEROR(T24,NTAB024*MAXYR*MAXS)
      CALL ZEROR(T25,NTAB025*MAXYR*MAXS)
      CALL ZEROR(T26,NTAB026*MAXYR*MAXS)
      CALL ZEROR(T27,NTAB027*MAXYR*MAXS)
      CALL ZEROR(T28,NTAB028*MAXYR*MAXS)
      CALL ZEROR(T29,NTAB029*MAXYR*MAXS)
      CALL ZEROR(T30,NTAB030*MAXYR*MAXS)
      CALL ZEROR(T31,NTAB031*MAXYR*MAXS)
      CALL ZEROR(T32,NTAB032*MAXYR*MAXS)
      CALL ZEROR(T33,NTAB033*MAXYR*MAXS)
      CALL ZEROR(T34,MNUMCR*NTAB034*MAXYR*MAXS)
      CALL ZEROR(T35,NTAB035*MAXYR*MAXS)
      CALL ZEROR(T36,NTAB036*MAXYR*MAXS)
      CALL ZEROR(T37,NTAB037*MAXYR*MAXS)
      CALL ZEROR(T38,NTAB038*MAXYR*MAXS)
      CALL ZEROR(T39,NTAB039*MAXYR*MAXS)
      CALL ZEROR(T40,NTAB040*MAXYR*MAXS)
      CALL ZEROR(T41,NTAB041*MAXYR*MAXS)
      CALL ZEROR(T42,NTAB042*MAXYR*MAXS)
      CALL ZEROR(T43,NTAB043*MAXYR*MAXS)
      CALL ZEROR(T44,NTAB044*MAXYR*MAXS)
      CALL ZEROR(T45,NTAB045*MAXYR*MAXS)
      CALL ZEROR(T46,NTAB046*MAXYR*MAXS)
      CALL ZEROR(T47,NTAB047*MAXYR*MAXS)
      CALL ZEROR(T48,MNUMCR*NTAB048*MAXYR*MAXS)
      CALL ZEROR(T49,NTAB049*MAXYR*MAXS)
      CALL ZEROR(T50,NTAB050*MAXYR*MAXS)
      CALL ZEROR(T51,NTAB051*MAXYR*MAXS)
      CALL ZEROR(T52,NTAB052*MAXYR*MAXS)
      CALL ZEROR(T53,NTAB053*MAXYR*MAXS)
      CALL ZEROR(T54,NTAB054*MAXYR*MAXS)
      CALL ZEROR(T55,NTAB055*MAXYR*MAXS)
      CALL ZEROR(T56,NTAB056*MAXYR*MAXS)
      CALL ZEROR(T57,NTAB057*MAXYR*MAXS)
      CALL ZEROR(T58,NTAB058*MAXYR*MAXS)
      CALL ZEROR(T59,mnumnr*NTAB059*MAXYR*MAXS)
      CALL ZEROR(T60,NTAB060*MAXYR*MAXS)
      CALL ZEROR(T61,mnumnr*NTAB061*MAXYR*MAXS)
      CALL ZEROR(T62,mnumnr*NTAB062*MAXYR*MAXS)
      CALL ZEROR(T63,NTAB063*MAXYR*MAXS)
      CALL ZEROR(T64,NTAB064*MAXYR*MAXS)
      CALL ZEROR(T65,NTAB065*MAXYR*MAXS)
      CALL ZEROR(T66,NTAB066*MAXYR*MAXS)
      CALL ZEROR(T67,mnumnr*NTAB067*MAXYR*MAXS)
      CALL ZEROR(T68,NTAB068*MAXYR*MAXS)
      CALL ZEROR(T69,NTAB069*MAXYR*MAXS)
      CALL ZEROR(T70,MNUMCR*NTAB070*MAXYR*MAXS)
      CALL ZEROR(T71,NTAB071*MAXYR*MAXS)
      CALL ZEROR(T72,NTAB072*MAXYR*MAXS)
      CALL ZEROR(T73,NTAB073*MAXYR*MAXS)
      CALL ZEROR(T74,NTAB074*MAXYR*MAXS)
      CALL ZEROR(T75,NTAB075*MAXYR*MAXS)
      CALL ZEROR(T76,NTAB076*MAXYR*MAXS)
      CALL ZEROR(T77,NTAB077*MAXYR*MAXS)
      CALL ZEROR(T78,NTAB078*MAXYR*MAXS)
      CALL ZEROR(T79,NTAB079*MAXYR*MAXS)
      CALL ZEROR(T80,NTAB080*MAXYR*MAXS)
      CALL ZEROR(T81,NTAB081*MAXYR*MAXS)
      CALL ZEROR(T82,NTAB082*MAXYR*MAXS)
      CALL ZEROR(T83,NTAB083*MAXYR*MAXS)
      CALL ZEROR(T84,NTAB084*MAXYR*MAXS)
      CALL ZEROR(T85,NTAB085*MAXYR*MAXS)
      CALL ZEROR(T86,NTAB086*MAXYR*MAXS)
      CALL ZEROR(T87,NTAB087*MAXYR*MAXS)
      CALL ZEROR(T88,NTAB088*MAXYR*MAXS)
      CALL ZEROR(T89,NTAB089*MAXYR*MAXS)
      CALL ZEROR(T90,NTAB090*MAXYR*MAXS)
      CALL ZEROR(T91,NTAB091*MAXYR*MAXS)
      CALL ZEROR(T92,NTAB092*MAXYR*MAXS)
      CALL ZEROR(T93,MNUMCR*NTAB093*MAXYR*MAXS)
      CALL ZEROR(T94,NTAB094*MAXYR*MAXS)
      CALL ZEROR(T95,NTAB095*MAXYR*MAXS)
      CALL ZEROR(T96,NTAB096*MAXYR*MAXS)
      CALL ZEROR(T97,NTAB097*MAXYR*MAXS)
      CALL ZEROR(T98,NTAB098*MAXYR*MAXS)
      CALL ZEROR(T99,NTAB099*MAXYR*MAXS)
      CALL ZEROR(T100,NTAB100*MAXYR*MAXS)
      CALL ZEROR(T101,NTAB101*MAXYR*MAXS)
      CALL ZEROR(T102,MNUMCR*NTAB102*MAXYR*MAXS)
      CALL ZEROR(T103,NTAB103*MAXYR*MAXS)
      CALL ZEROR(T104,NTAB104*MAXYR*MAXS)
      CALL ZEROR(T105,NTAB105*MAXYR*MAXS)
      CALL ZEROR(T106,NTAB106*MAXYR*MAXS)
      CALL ZEROR(T107,NTAB107*MAXYR*MAXS)
      CALL ZEROR(T108,NTAB108*MAXYR*MAXS)
      CALL ZEROR(T109,NTAB109*MAXYR*MAXS)
      CALL ZEROR(T110,NTAB110*MAXYR*MAXS)
      CALL ZEROR(T111,NTAB111*MAXYR*MAXS)
      CALL ZEROR(T112,NTAB112*MAXYR*MAXS)
      CALL ZEROR(T113,NTAB113*MAXYR*MAXS)
      CALL ZEROR(T114,NTAB114*MAXYR*MAXS)
      CALL ZEROR(T115,NTAB115*MAXYR*MAXS)
      CALL ZEROR(T116,mnumnr*NTAB116*MAXYR*MAXS)
      CALL ZEROR(T117,NTAB117*MAXYR*MAXS)
      CALL ZEROR(T118,NTAB118*MAXYR*MAXS)
      CALL ZEROR(T119,11*NTAB119*MAXYR*MAXS)
! skip table 120
      CALL ZEROR(T121,NTAB121*MAXYR*MAXS)
      CALL ZEROR(T122,mnumnr*NTAB122*MAXYR*MAXS)
      CALL ZEROR(T123,mnumnr*NTAB123*MAXYR*MAXS)
      CALL ZEROR(T124,NTAB124*MAXYR*MAXS)
      CALL ZEROR(T125,MNUMCR*NTAB125*MAXYR*MAXS)
      CALL ZEROR(T126,NTAB126*MAXYR*MAXS)
      CALL ZEROR(T127,NTAB127*MAXYR*MAXS)
      CALL ZEROR(T128,NTAB128*MAXYR*MAXS)
      CALL ZEROR(T129,NTAB129*MAXYR*MAXS)
      CALL ZEROR(T130,NTAB130*MAXYR*MAXS)
      CALL ZEROR(T131,NTAB131*MAXYR*MAXS)
      CALL ZEROR(T132,NTAB132*MAXYR*MAXS)
      CALL ZEROR(T133,NTAB133*MAXYR*MAXS)
      CALL ZEROR(T134,NTAB134*MAXYR*MAXS)
      CALL ZEROR(T135,NTAB135*MAXYR*MAXS)
      CALL ZEROR(T136,NTAB136*MAXYR*MAXS)
      CALL ZEROR(T137,NTAB137*MAXYR*MAXS)
      CALL ZEROR(T138,NTAB138*MAXYR*MAXS)
      CALL ZEROR(T139,NTAB139*MAXYR*MAXS)
      CALL ZEROR(T140,NTAB140*MAXYR*MAXS)
      CALL ZEROR(T141,NTAB141*MAXYR*MAXS)
      CALL ZEROR(T142,NTAB142*MAXYR*MAXS)
      CALL ZEROR(T143,NTAB143*MAXYR*MAXS)
      CALL ZEROR(T144,NTAB144*MAXYR*MAXS)
      CALL ZEROR(T145,NTAB145*MAXYR*MAXS)
      CALL ZEROR(T146,NTAB146*MAXYR*MAXS)
      CALL ZEROR(T147,NTAB147*MAXYR*MAXS)
      CALL ZEROR(T148,NTAB148*MAXYR*MAXS)
      CALL ZEROR(T149,NTAB149*MAXYR*MAXS)
      CALL ZEROR(T150,NTAB150*MAXYR*MAXS)
      RETURN
      END
!************************************************************************
      SUBROUTINE ZEROR(X,N)
       IMPLICIT NONE
! ZEROES ARRAY X WITH ADJUSTABLE DIMENSION SIZE X
      INTEGER I,N
      REAL X(N)
      DO 10 I=1,N
      X(I)=0.
10    CONTINUE
      RETURN
      END
! ******************************************************************
      FUNCTION FSUM(X,N)
      IMPLICIT NONE
! COMPUTES THE SUM OF ADJUSTABLE ARRAY X WITH N PLACES
      INTEGER I,N
      REAL X(N),SUM,FSUM
      SUM=0.
      DO 10 I=1,N
      SUM=SUM+X(I)
10    CONTINUE
      FSUM=SUM
      RETURN
      END
!*****************************************************************
      SUBROUTINE FILL(RFIELD,CFIELD,IDEC2,ICOLWD)
       IMPLICIT NONE
! FORMATS "RFIELD" INTO CHARACTER STRING "CFIELD" WITH "IDEC" DECIMAL POINTS.
! IF IDEC=9, THEN INTEGER FORMAT;
! IF NUMBER TOO LARGE FOR FIELD, REDUCES IDEC TO NUMBER THAT WILL FIT;
! LEAVES FIRST CHAR BLANK TO SEPARATE FROM OTHER NUMBERS.
      CHARACTER*7 FORM
      CHARACTER*(*) CFIELD
      REAL RFIELD,AFIELD,RFIT
      INTEGER IDEC,IDEC2,ICOLWD,LF,LFIT,IFIELD
      IDEC=IDEC2
      IF(IDEC.GT.9) IDEC=9
      AFIELD=ABS(RFIELD)
      LF=ICOLWD-1
10    LFIT=LF
      IF(RFIELD.LT.0.) LFIT=LFIT-1
      IF(IDEC.NE.9) THEN
        LFIT=LFIT-IDEC-1
        IF(LFIT.LE.0)THEN
          RFIT=0.
        ELSE
          RFIT=10**LFIT-1.
        ENDIF
        IF(AFIELD.GT.RFIT) THEN
          IDEC=IDEC-1
          IF(IDEC.LE.0) IDEC=9
          GOTO 10
        ENDIF
        IF(LF.GE.9) THEN
          WRITE(FORM,'(A,I2,A,I1,A)')  '(F',LF+1,'.',IDEC,')'
        ELSE
          WRITE(FORM,'(A,I1,A,I1,A)')  '(F',LF+1,'.',IDEC,')'
        ENDIF
        WRITE(CFIELD,FORM,ERR=99) RFIELD

      ELSE
        IF(LFIT.LE.0.)THEN
          RFIT=0.
        ELSE
          RFIT=10**LFIT-1.
        ENDIF
        IF(AFIELD.GT.RFIT) THEN
          CFIELD=' *******'
          RETURN
        ENDIF
        IFIELD=NINT(RFIELD)
        IF(LF.GE.9) THEN
          WRITE(FORM,'(A,I2,A)')  '(I',LF+1,')'
        ELSE
          WRITE(FORM,'(A,I1,A)')  '(I',LF+1,')'
        ENDIF
        WRITE(CFIELD,FORM,ERR=99) IFIELD
      ENDIF
      IF (RFIELD .EQ. -999.) WRITE(CFIELD,'(" - -")')
      IF (RFIELD .EQ. -888.) WRITE(CFIELD,'("   w")')
      RETURN
99    CFIELD=' *******'
      RETURN
      END
!****************************************************************
       SUBROUTINE SETREPORTSWITCH
! SUBROUTINE TO TURN ON/OFF REPORTS
! IE: IF USER SELECTS AEO REPORTS - THIS ROUTINE TURNS ON THE
!     APPLICABLE REPORTS

      IMPLICIT NONE
      include 'parametr'
      include 'ftable'
      integer*4 sortseq(maxtab)
      INTEGER I,UNITNUM,SWITCH,ilast
      integer IFMT/0/  ! 0: old fmt, 1:new fmt
      CHARACTER*2 CH

      UNITNUM=11
      OPEN(UNIT=UNITNUM,FILE=TABREQ,STATUS='OLD',ACTION='READ',ERR=99)

! READ UNTIL @ IN 1ST COLUMN
      CH = 'A'
      DO WHILE (CH .NE. '@')
         READ(UNITNUM,10) CH
         if(ch.eq.'99') ifmt=1  ! 99 in comment area triggers new format
      ENDDO
  10  FORMAT(A2)
  20  FORMAT(I1)

      DO I=1,NTABLE
        if(ifmt.eq.0) then
          READ(UNITNUM,20) SWITCH
        else
          read(unitnum,*) SWITCH,SEQNUM(I)
        endif
        IF (SWITCH .EQ. 1)THEN
          REPORT_SWITCH(I)=.TRUE. ! TURN ON REPORT
        ELSE
          REPORT_SWITCH(I)=.FALSE.
        ENDIF
      ENDDO
      CLOSE(UNIT=UNITNUM)
      IF (REPORT_SWITCH(120)) CALL FDATA_IEA

! fill in sequence numbers if not specified
      ilast=0
      do i=1,ntable
        ilast=max(ilast,seqnum(i))
      enddo
      if (ilast.gt.0) then
        do i=1,ntable
          if(report_switch(i).and.seqnum(i).eq.0) then
             ilast=ilast+1
             seqnum(i)=ilast
          elseif(report_switch(i).eq.0) then
             seqnum(i)=ntable+1
          endif
          order(i)=i
        enddo
        sortseq=seqnum ! preserve the sequence number by ftab table number
        call isort(sortseq,order,ntable,maxtab)

      endif
      RETURN
 99   WRITE(6,*) 'ERROR OPENING TABREQ FILE'
      STOP 61
      END
!****************************************************************
       SUBROUTINE TABLELST
! SUBROUTINE TO CREATE A LIST OF ALL THE TABLES THAT WERE REPORTED

      IMPLICIT NONE
      include 'parametr'
      include 'ftable'
      INTEGER ITAB
      WRITE(20,2000)
      DO ITAB = 1,MAXTAB
         IF (REPORT_SWITCH(ITAB)) THEN
            IF (ITAB.GE.100) THEN
               WRITE(20,2007) ITAB,TABHED(1,ITAB)
            ELSEIF (ITAB.GE.10) THEN
               WRITE(20,2001) ITAB,TABHED(1,ITAB)
            ELSEIF (ITAB.LT.10) THEN
               WRITE(20,2002) ITAB,TABHED(1,ITAB)
            ENDIF
         ENDIF
      ENDDO
2000  FORMAT('1',' LIST OF TABLES THAT WERE REPORTED')
2001  FORMAT('  Table ',I2,'.  ',A)
2002  FORMAT('  Table ',I1,'.  ',A)
2007  FORMAT('  Table ',I3,'.  ',A)
      RETURN
      END
       subroutine replace(string,finds,repls)
       implicit none
       character*(*) string, finds, repls
       character*2048 temp
       integer L,LF,LR,I
       l=len_trim(string)
       lf=len_trim(finds)
       lr=len_trim(repls)
       if (lr .lt. 1 .and. finds .eq. '|') lr=1
       i=1
10     continue
         if(string(i:(i+LF-1)).eq.finds) then
           temp=string((i+Lf):)
           string(i:(i+LR-1))=trim(repls)
           string((i+LR):)=trim(temp)
           l=len_trim(string)
           i=i+LR-1
         endif
         i=i+1
       if(i.le.L-LF+1) goto 10
       return
       end
!============================================================================================
  subroutine FXML(ITAB,IGROWO,IGRWYR,SCENID,IREGN)
  implicit none
! writes out tables to an excel worksheet in excel XML format based on a template input file that
! controls the format and has replacement sections that are filled by this routine

    include 'parametr'
    include 'ncntrl'
    include 'ftable'

    INTEGER ITAB ! table index
    INTEGER IGROWO ! growth rate option, =1 for growth rates
    INTEGER IGRWYR ! starting year for growth rate option
    CHARACTER*(MCOLWD) SCENID(MAXYR)
    INTEGER IREGN ! Region index, if applicable, 0 for national-level table

! Variables to hold and fill the xml template
   integer nheadlines  ! number of lines other than table rows, including footnotes
   integer ntemplate ! number of non-comment lines in the xml template file and template array
!
   integer nlink     ! number of hyperlinks, one for each national and regional table
   integer i,ir,iii,itb,irow,iplaces    ! index variables
   character*255 template(400)          ! lines, not counting comments, in the xml template file
   integer nreplaces ! number of replacement strings in a given section
! data structure for search strings and replacement values
   type switcheroo
      character*25 str     ! search string
      character*180 val    ! replacement value
   end type switcheroo
   type (switcheroo) :: replaces(10)    ! replacement strings
   real xml_row            ! row count in xml worksheet
   logical once_template   ! set to true when the template code has been read so it will only happen once
   logical lexist          ! used with open statement, true if file found
   real rcolval(maxyr)     ! used to fill up numeric data for a single row
   real lastrow            ! number of the last row in the table
   real lastcol            ! number of the last column in the table
   character*6 RGTKey      ! unique identifying key for each table
   character*1 testc
   character*9 ch_itb
   character*26 scendate
   character*26 column_head(maxyr),style_head(maxyr)*3,style_col(7)*3
   style_col(1:7)=(/'s34','s35','s36','s37','s38','s39','s40'/)
   
   
     If(.not. once_template) then
! On first time through, read the xml template file that has the basic format
! with substitution strings to be filled in by this program as tables are added.
       once_template=.true.
       inquire(exist=lexist,file=FTABXML)
       if(lexist) then
         open(109,file=FTABXML,status='old',readonly)
         read(109,'(a)') testc
         if(testc.ne.'!'.and.testc.ne.'|'.and.testc.ne.'<') then
           once_template=.false.
           close(109)
           return
         else
           backspace 109
         endif
         ntemplate=1
!  drop and comment lines found in the file.
         do while (.not. eof(109))
           read(109,'(a)') template(ntemplate)
           if(index(template(ntemplate),'!!').gt.0) then  ! eliminate comment lines
             template(ntemplate)=' '
             ntemplate=ntemplate-1
           endif
           ntemplate=ntemplate+1
         enddo
         close(109)
         ntemplate=ntemplate-1
       endif
 ! the number of rows must be known and written to first section of xml file, so must do some counting
 !  loop through tables to count number of rows for the hyperlinks section.  One link for each national and regional table
       nlink=0
       do iii=1,ntable
         IF(REPORT_SWITCH(iii).and. (NHED(iii).GT.0.OR.NROWS(iii).GT.0)) THEN
           nlink=nlink+1
           if(IPREGD.EQ.1)then ! Check for regional versions of tables
             do i=1,no_reg_tables             ! loop over tables in array regtable
               if(regtables(i).eq.iii) nlink=nlink+nregtables(i)
             enddo
           endif
         ENDIF
       ENDDO
! hyperlinks start on row 11, leaving top 10 rows for helpful info
! first table will start 10 rows after end of hyperlinks
       xml_row=nlink+10+10
 ! Loop through tables, figure out the row where each table will start, and write a hyperlink to each table
       DO III=1,NTABLE
         ITB = ORDER(III)          ! PUT IN ORDER AS SPECIFIED IN TABREQ
         nheadlines=nhed(itb)+7+nfoot(itb)     !
         if(nscen.gt.1) nheadlines=nheadlines+1  ! in compare format, there is an extra heading line in each table
         IF(REPORT_SWITCH(ITB).and. (NHED(ITB).GT.0.OR.NROWS(ITB).GT.0)) THEN
           xml_row=xml_row+(25-mod(int(xml_row),25))  ! start each table on a row divisible by 25
           xml_row=xml_row+nheadlines+nrows(itb)   ! add length of this table to determine start of next table
         ENDIF
       ENDDO
        IF(IPREGD.EQ.1) THEN
  ! count HYPERLINKS FOR REGIONAL VERSIONS OF TABLES 2, 3, 17, 21, 48, 61, 62, 67, 70, 93, 102, 116, 119, 122, 123, 125.
         DO i=1,no_reg_tables                ! loop over tables in array regtable
           itb=regtables(i)
           IF (REPORT_SWITCH(itb)) THEN
             nheadlines=nhed(itb)+7+nfoot(itb)      !
             if(nscen.gt.1) nheadlines=nheadlines+1  ! in compare format, there is an extra heading line in each table
             DO IR=1,nregtables(i)
if (itb .eq. 2) write (6,'("Table ",I3,": ",I4,I6,2F12.2,I4)') itb,nhed(itb),nfoot(itb),xml_row,xml_row+(25-mod(int(xml_row),25)),nrows(itb)
               IF (nregtables(i) .EQ. 4 .AND. IR .EQ. 1) CYCLE
               xml_row=xml_row+(25-mod(int(xml_row),25))  ! start each table on row divisible by 25
               xml_row=xml_row+nheadlines+nrows(itb)   ! add length of this table to determine start of next table
             ENDDO
           ENDIF
         ENDDO
       ENDIF
       lastrow=xml_row
       lastcol=ncols+2  ! stub column plus the symbolic name column are extra
 ! write top sections of the xml file
       open(110,file=trim(frscen)//'.'//fdate(2:5)//trim(fdate(8:9))//'.xml',status='unknown')
       write(6,*) 'The Excel XML output file is named '//trim(frscen)//'.'//fdate(2:5)//trim(fdate(8:9))//'.xml'
       replaces(1).str='##sheetname##'
       replaces(1).val=trim(frscen)//'.'//fdate(2:5)//trim(fdate(8:9))

       replaces(2).str='##datekey##'
       replaces(2).val=trim(frscen)//'.'//trim(fdate)

       replaces(3).str='##colcount##'
       call fill(lastcol,replaces(3).val,9,5)    ! write lastcol to character field as integer
       replaces(3).val=adjustl(replaces(3).val)  ! left justify string

       replaces(4).str='##rowcount##'
       call fill(lastrow,replaces(4).val,9,7)    ! write lastrow to character field as integer
       replaces(4).val=adjustl(replaces(4).val)  ! left justify string

       nreplaces=4
! write out the xml defined by the ||BEGINNING|| section of the template, doing the replacements
       call expandsect('||BEGINNING||',template,ntemplate,replaces,nreplaces,,,,,,,,)
! write year column headings on first row already in progress
     do iii=1,ncols
       read(collab(iii)(2:),*) rcolval(iii) ! put the year headings into a real array to pass to row expansion
     enddo
     if(igrowo.eq.0) then
       call expandsect('||YEARHEAD||',template,ntemplate,replaces,nreplaces,'##YEAR##',ncols,rcolval,9,igrowo,'B',,)
     else
       call expandsect('||YEARHEAD||',template,ntemplate,replaces,nreplaces,'##YEAR##',ncols-nscen,rcolval,9,igrowo,'B',,)
     endif
! write scenario ids for comparison format
     if (nscen .gt. 1) then  ! identify scenarios in column headings
       do i=1,nscen
         call get_scendate(datab(i),scendate)
         do iii=i,ncols,nscen
            column_head(iii)=scendate
            style_head(iii)=style_col(i)  ! alternate fill colors used for each scenario id, requires a different style
         enddo

       enddo
       nreplaces=0
       call expandsect('||COLHEADMAIN||',template,ntemplate,replaces,nreplaces,'##SCENID##',ncols,rcolval,9,igrowo,'B',column_head,style_head) ! scenid is optional character argument
     endif

!   Write Hyperlinks Section. First line is an instruction message
    nreplaces=0
    call expandsect('||HYPERLINKSMSG||',template,ntemplate,replaces,nreplaces,,,,,,,,)
!   Hyperlink replacement strings for table IDs and their starting row numbers
       replaces(1).str='##sheetname##'   ! used as the worksheet name so needed for the hyperlink address
       replaces(1).val=trim(frscen)//'.'//fdate(2:5)//trim(fdate(8:9))

       replaces(2).str='##LINKROW##'    ! replacement value for row entered in loop below

       replaces(3).str='##TABTITLE1##'  ! replacement value for table heading entered in loop below

       nreplaces=3
       xml_row=nlink+10
! Loop through tables, figure out the row where each table will start, and write a hyperlink to each table
       DO III=1,NTABLE
         ITB = ORDER(III)                             ! PUT IN ORDER AS SPECIFIED IN TABREQ
         i=iii
         if(seqnum(itb).ne.0) i=seqnum(itb)
         if(i.le.9) iplaces=1
         if(i.gt.9 .and. iii.le.99) iplaces=2
         if(i.gt.99)iplaces=3
         write(ch_itb,'(i<iplaces>,a)') i,'. '
         nheadlines=nhed(itb)+7+nfoot(itb)
         if(nscen.gt.1) nheadlines=nheadlines+1       ! in compare format, there is an extra heading line in each table

         IF(REPORT_SWITCH(ITB).and. (NHED(ITB).GT.0.OR.NROWS(ITB).GT.0)) THEN
           xml_row=xml_row+(25-mod(int(xml_row),25))  ! start each table on a row divisible by 25
           call fill(xml_row+27.,replaces(2).val,9,7)     ! have hyperlink start 27 rows down so title is up at top of window
           replaces(2).val=adjustl(replaces(2).val)   ! replacement for ##LINKROW##
           replaces(3).val=trim(ch_itb)//' '//trim(tabhed(1,itb))
           call expandsect('||HYPERLINKS||',template,ntemplate,replaces,nreplaces,,,,,,,,)
           xml_row=xml_row+nheadlines+nrows(itb)   ! add length of this table to determine start of next table
         ENDIF
       ENDDO
       IF(IPREGD.EQ.1) THEN
  ! WRITE HYPERLINKS FOR REGIONAL VERSIONS OF TABLES listed in array regtables
  !  INCLUDE REGION NAME IN LINK NAME
         DO i=1,no_reg_tables          ! loop over the tables in array regtable
           itb=regtables(i)
           iii=itb
           if(seqnum(itb).ne.0) iii=seqnum(itb)  ! this will get the regional table numbers printed to match ordered number for the national, but the order of the regional tables may not be the same
           IF (REPORT_SWITCH(itb)) THEN
             if(iii.le.9) iplaces=1
             if(iii.gt.9 .and. iii.le.99) iplaces=2
             if(iii.gt.99)iplaces=3
             nheadlines=nhed(itb)+7+nfoot(itb)
             if(nscen.gt.1) nheadlines=nheadlines+1       ! in compare format, there is an extra heading line in each table
             DO IR=1,nregtables(i)
               if (ir .lt. 10) then
                 write(ch_itb,'(i<iplaces>,a,i1,a)') iii,'.',ir,'. '
               else
                 write(ch_itb,'(i<iplaces>,a,i2,a)') iii,'.',ir,'. '
               endif
               if (nregtables(i) .EQ. 4 .AND. IR .EQ. 1) CYCLE     !  in this case, region 1 is the "national" version
if (itb .eq. 2 .or. itb .eq. 3) write (6,'("Table ",I3,": ",I4,I6,2F12.2,I4,I4)') itb,nhed(itb),nfoot(itb),xml_row,xml_row+(25-mod(int(xml_row),25)),nrows(itb),nheadlines
               xml_row=xml_row+(25-mod(int(xml_row),25))  ! start each table on row divisible by 25
               call fill(xml_row+27.,replaces(2).val,9,7) ! have hyperlink start 27 rows down so title is up at top of window
               replaces(2).val=adjustl(replaces(2).val)
               if(nregtables(i).eq.9) then
                 replaces(3).val=trim(ch_itb)//' '//trim(tabhed(1,itb))//' '//trim(regnam(ir))  ! census division names
               else if(nregtables(i).eq.4) then
                 replaces(3).val=trim(ch_itb)//' '//trim(tabhed(1,itb))//' '//trim(fakenam(ir)) ! value, mass, or energy
               else
                 replaces(3).val=trim(ch_itb)//' '//trim(tabhed(1,itb))//' '//trim(nercnam(ir)) ! nerc (EMM) region names
               endif
               call expandsect('||HYPERLINKS||',template,ntemplate,replaces,nreplaces,,,,,,,,)
               xml_row=xml_row+nheadlines+nrows(itb)   ! add length of this table to determine start of next table
             ENDDO
           ENDIF
         ENDDO
       ENDIF   ! if(IPREGD.EQ.1)
! first table will start 10 rows after hyperlinks
       xml_row=nlink+10
       xml_row=xml_row+(25-mod(int(xml_row),25))  ! start each table on a row divisible by 25
     endif  !  If(.not. once_template)
     if(itab.eq.9999) then
       nreplaces=3
       replaces(1).str='##SPLITROW##'     ! determines row for the freeze pane horizontal split
       replaces(1).val='1'                ! normally on row 1 (where row 0 is first row).
       replaces(2).str='##TOPROW##'
       replaces(2).val='1'                ! row 1 (actually the second row)
       replaces(3).str='##ACTIVEROW##'
       replaces(3).val='2'                ! active row is 2, the third row
       if(nscen.gt.1)then
         replaces(1).val='2'  ! but if compare format, do after row 2 (third row) so scenario ids show up
         replaces(2).val='2'  ! top row is 2 (third row)
         replaces(3).val='3'  ! active row is 3, the fourth row
       endif
       call expandsect('||ENDING||',template,ntemplate,replaces,nreplaces,,,,,,,,)
       close(110)
       return
     endif
! write TABLETEMPLATE section that defines the top of each table
!
     ITB = itab
     iii=itab
     if(seqnum(iii).ne.0) iii=seqnum(itb)
     if(iii.le.9) iplaces=1
     if(iii.gt.9 .and. iii.le.99) iplaces=2
     if(iii.gt.99)iplaces=3
     write(ch_itb,'(i<iplaces>,a)') iii,'. '
     if (iregn .gt. 0 .and. iregn .le. 9) then
       write(ch_itb,'(i<iplaces>,a,I1,a)') iii,'.',iregn,'. '
     else if (iregn .ge. 10) then
       write(ch_itb,'(i<iplaces>,a,I2,a)') iii,'.',iregn,'. '
     endif

     RGTKey=RGName(itab)
     write(RGTKey(4:6),'(i3.3)') iregn
     replaces(1).str='##TABKEY##'
     replaces(1).val=RGTKey
     replaces(2).str='##StartRow##'
     call fill(xml_row,replaces(2).val,9,7)    ! write xml_row to character field as integer
if (itab .eq. 2 .or. itab .eq. 3) write (6,'("Table ",I3,": ",I4,I6,2F12.2,I4,I4)') itab,nhed(itab),nfoot(itab),xml_row,xml_row+(25-mod(int(xml_row),25)),nrows(itab),nheadlines
     replaces(2).val=adjustl(replaces(2).val)  ! left justify string
     replaces(3).str='##TABTITLE1##'
     replaces(3).val=trim(ch_itb)//' '//tabhed(1,itab)
     replaces(4).str='##TABTITLE2##'
     if(nhed(itab).lt.2) tabhed(2,itab)=' '
     replaces(4).val=tabhed(2,itab)
     replaces(5).str='##TABTITLE3##'
     if(nhed(itab).lt.3) tabhed(3,itab)=' '
     replaces(5).val=tabhed(3,itab)
     nreplaces=5
     call expandsect('||TABLETEMPLATE||',template,ntemplate,replaces,nreplaces,,,,,,,,)
!
     if(igrowo.eq.1) then
        do iii=1,ncols
          column_head(iii)=' '
          if(iii.gt.ncols-nscen) then
            write(column_head(iii),'(i4,a)') igrwyr+1989,'-'
          endif
        enddo
        call expandsect('||COLHEAD_GROW||',template,ntemplate,replaces,nreplaces,'##SCENID##',ncols,rcolval,9,igrowo,'B',column_head,) ! scenid is optional character argument
     else
        write(110,'(a)')   '</Row>'
     endif
     replaces(1).str='##STUBHEAD##'
     replaces(1).val=rowhed(itab)
     nreplaces=1
     call expandsect('||STUBROW||',template,ntemplate,replaces,nreplaces,,,,,,,,)

! write year column headings
     do iii=1,ncols
       read(collab(iii)(2:),*) rcolval(iii) ! put the year headings into a real array to pass to row expansion
     enddo
     call expandsect('||YEARHEAD||',template,ntemplate,replaces,nreplaces,'##YEAR##',ncols,rcolval,9,igrowo,'B',,)

! write scenario ids for comparison format
     if (nscen .gt. 1) then  ! identify scenarios in column headings
       nreplaces=0
       call expandsect('||COLHEAD||',template,ntemplate,replaces,nreplaces,'##SCENID##',ncols,rcolval,9,igrowo,'B',scenid,) ! scenid is optional character argument
     endif

     do irow=1,nrows(itab)
       if(ftabbone.ne.1 .and. drform(irow,itab)(6:6).eq.'1') cycle  ! skip "bonus" rows
       ir=irows(irow,itab)
       if(ir.eq.0.and.len_trim(rowlab(irow,itab)).eq.0) then !  then this is a blank line
         write(110,'(a)') '<Row/>'
       else
! there are three replacements to make in these lines, so set nreplaces to 3 and fill in
! replaces(1), replaces(2), and replaces(3)
         replaces(1).str='##TABKEY##'
         replaces(1).val=RGTKey                    ! unique table key
         replaces(2).str='##ROWKEY##'
         replaces(2).val=DRName(irow,itab)         ! unique row key
         replaces(3).str='##ROWSTUB##'
         replaces(3).val=rowlab(irow,itab)
         nreplaces=3
         if(ir.eq.0) then                                  !   heading only
           call expandsect('||BOLDSUBHEAD||',template,ntemplate,replaces,nreplaces,,,,,,,,)
         else
           do iii=1,ncols
             rcolval(iii)=tabn(ir,icols(iii),iscen(iii))
           enddo
           if(DRForm(irow,itab)(1:1).eq.'p') then
             call expandsect('||DATAROWPLAIN||',template,ntemplate,replaces,nreplaces,'##VAL##',ncols,rcolval,ROWFMT(IROW,ITAB),igrowo,'p',,)
           elseif(DRForm(irow,itab)(1:1).eq.'B') then
             call expandsect('||DATAROWBOLD||', template,ntemplate,replaces,nreplaces,'##VAL##',ncols,rcolval,ROWFMT(IROW,ITAB),igrowo,'B',,)
           endif
         endif
       endif
     enddo
!  3 replacements in footnotes.  first footnote row is joined across the table (##MERGE## replacement)
!  and has a top border (really a border for the bottom of the table)
     nreplaces=3
     replaces(1).str='##FOOTNT##'
     replaces(1).val=footnt(1,itab)
     replaces(2).str='##STYLE##'
     replaces(2).val='s21'
     replaces(3).str='##MERGE##'
! insert leading space on this replacement, so all the others only have only one space when string is removed
     write(replaces(3).val,'(A,I2.2,A)') ' ss:MergeAcross="', int(lastcol-2), '"'
     call expandsect('||FOOTNOTE||',template,ntemplate,replaces,nreplaces,,,,,,,,)
     do i=2,nfoot(itab)
       replaces(1).val=footnt(i,itab)
       replaces(2).val='s51'
       replaces(3).val=''
       call expandsect('||FOOTNOTE||',template,ntemplate,replaces,nreplaces,,,,,,,,)
     enddo
     nheadlines=nhed(itab)+7+nfoot(itab)
     if(nscen.gt.1) nheadlines=nheadlines+1       ! in compare format, there is an extra heading line in each table
     xml_row=xml_row+nheadlines+nrows(itab)
     xml_row=xml_row+(25-mod(int(xml_row),25))  ! start each table on a row divisible by 25

  end subroutine FXML
!============================================================================================
  subroutine expandsect(sect,template,ntemplate,replaces,nreplaces, &
           dupstr,ndupes,dupvals,irowfmt,igrowo,face,scenid,style_id)
  implicit none
  optional dupstr,ndupes,dupvals,irowfmt,igrowo,face
  optional scenid,style_id
  character*(*) scenid(62)  !     CHARACTER*(MCOLWD) SCENID(MAXYR)
  character*3 style_id(62)
!  identifies a section in the xml template array and performs a series of string
!  substitutions, writing the results to the xml output file, assumed to be unit 110 and open for output
    character*(*) sect
    integer ntemplate,nreplaces
    character*255 template(ntemplate)
    character*255 line
    character*(*) dupstr
    character*1 face ! "B" or "p" for bold or plain
    integer ndupes
    real dupvals(62)
    integer igrowo
    integer irowfmt  ! In ftab, number of positions of significance after decimal.  used here to detect integer
    type switcheroo
      character*25 str    ! search string
      character*180 val   ! replacement string
    end type switcheroo
    type (switcheroo) :: replaces(nreplaces)    ! replacement strings

    integer it,ir  ! index variables
    integer is,ie  ! index in template for start (is) and end (ie) of the section
    integer ls     ! trimmed length of section name
    integer i,iii
    character*10 style ! font style
    character*16 c16   ! 16 character string to hold numeric field

    include 'parametr'
    include 'ncntrl'
    include 'ftable'

! find section in the template

    is=0
    ie=0
    ls=len(sect)
    ls=len_trim(sect)
    do it=1,ntemplate
      if(index(template(it),sect(:ls)).gt.0) then
        if (is.eq.0) then
          is=it+1
        else
          ie=it-1
          exit ! exit do loop
        endif
      endif
    enddo
    if(is.eq.0) then
      write(6,*) 'Writing XML file, subroutine expandsect, section name not found in template:',trim(sect)
      return
    elseif(ie.eq.0) then
      write(6,*) 'Writing XML file, subroutine expandsect, ending of section not found in template:',trim(sect)
      return
    endif
    if(present(dupstr)) then
! The style definitions are in the template.
      if(face.eq.'B') then     ! choose among the bold styles in the template
        if(irowfmt.eq.9) then
          style='s47'          ! format="0"
        elseif(irowfmt.eq.1) then
          style='s48'          ! format="0.0"
        elseif(irowfmt.eq.3) then
          style='s50'          ! format="0.000"
        else
          style='s49'          ! format="fixed" (0.00)
        endif
      else  ! plain, not bold
        if(irowfmt.eq.9) then
          style='s43'          ! format="0"
        elseif(irowfmt.eq.1) then
          style='s44'          ! format="0.0"
        elseif(irowfmt.eq.3) then
          style='s46'          ! format="0.000"
        else
          style='s45'          ! format="fixed" (0.00)
        endif
      endif
    endif
!  write out the contents of the xml section, replace the strings (assumes the strings only appear once)
    do it=is,ie
      line=template(it)
      if(present(dupstr)) then
        if(index(line,trim(dupstr)).gt.0) then
          do i=1,ndupes
            line=template(it)
            if(present(scenid)) then
              call replace(line,trim(dupstr),trim(scenid(i)))
              if(present(style_id)) then
                call replace(line,'##STYLE##',trim(style_id(i)))
              endif
            else
              IF ((IGROWO .EQ. 1) .AND. (I .GT. (NCOLS-NSCEN)).and. (trim(dupstr).ne.'##YEAR##')) THEN
              ! switch style to "percentage" for growth rate column(s)
                style='s41'
                if(face.eq.'B') style='s42'
                write(c16,'(F16.6)') dupvals(i)/100.
              else
              ! for non-growth-rate items
                if(irowfmt.eq.9) then
                  if(nint(dupvals(i)).eq.dupvals(i)) then
                    write(c16,'(i16)') nint(dupvals(i))
                  else
                    write(c16,'(F16.6)') dupvals(i)
                  endif
                else
                  write(c16,'(F16.6)') dupvals(i)
                endif
              endif
              if(index(c16,'*')  .ge.1) c16='999999.99'
              if(index(c16,'NaN').ge.1) c16='0.'
              if(index(c16,'Infinity').ge.1) c16='0.'
              c16=adjustl(c16)
              call replace(line,trim(dupstr),trim(c16))
              call replace(line,'##STYLE##',trim(style))
            endif
            write(110,'(a)') trim(line)
          enddo
          cycle
        endif
      endif
      do ir=1,nreplaces
        call replace(line,trim(replaces(ir).str),trim(replaces(ir).val))
      enddo
      write(110,'(a)') trim(line)
    enddo
  return
  end subroutine expandsect
      subroutine get_scendate(restart,scendate)
! get scenario datekey from restart file path name.
      implicit none
      character*(*) restart,scendate
      character*16 scenario,datekey*9,basename*25
      character*255 path
      integer islash
      scendate=' '
      islash=0
      islash=scan(restart,'/\',.true.)  ! scan backwards for position of last slash
      basename=restart(islash+1:)       ! file name without the path
      scendate=basename                 ! backup name if scenario/datekey can't be obtained from file name
      path=' '
      if(islash.gt.1) then
        path=restart(:islash-1)
        islash=scan(path,'/\',.true.)  ! scan backwards for position of last slash
        if(islash.gt.1) then
          datekey=path(islash+1:)
          path=path(1:islash-1)
          islash=scan(path,'/\',.true.)  ! scan backwards for position of last slash
          if(islash.ge.1) then
            scenario=path(islash+1:)
            scendate=trim(scenario)//' '//trim(datekey)
          endif
        endif
      endif
      return
      end subroutine get_scendate
!============================================================================================
  subroutine FXMLDB(ITAB,IGROWO,IGRWYR,SCENID,IREGN)
  implicit none
! writes out tables to an XML Database format

    include 'parametr'
    include 'ncntrl'
    include 'ftable'

    INTEGER ITAB ! table index
    INTEGER IGROWO ! growth rate option, =1 for growth rates
    INTEGER IGRWYR ! starting year for growth rate option
    CHARACTER*(MCOLWD) SCENID(MAXYR)
    INTEGER IREGN ! Region index, if applicable, 0 for national-level table
   integer i,ir,iii,itb,irow    ! index variables
   integer io/111/,scedes/112/   ! output unit 110
   integer indent
   logical once            ! set to true after file is opened so it will only happen once
   logical lexist
   character*166 line
   character*6 RGTKey      ! unique identifying key for each table
   character*16 key,rowstr*3
   character*26 scendate
   character*26 column_head(maxyr),style_head(maxyr)*3,style_col(7)*3

     if(nscen.gt.1) return  ! this option is only relevant for a one-scenario run
     If(.not. once) then
! On first time through, open the xml output file and write top sections of the xml file
       once=.true.
       open(unit=io,file=trim(frscen)//'.'//fdate(2:5)//trim(fdate(8:9))//'.db.xml',status='unknown')
! write header info that would not be repeated for each run but is presented here for reference.
! When runs are combined for a study or group of studies, the header stuff would be
! put in as appropriate.
       indent=2
       write(io,100)'<?xml version="1.0" encoding="UTF-8"?>'
       write(io,100)'<!--   Ftab database in XML format.  See ftabdb.xsd for file structure schema.'
       write(io,100)'       Make sure ftabdb.xsd is in same folder before importing into MS access. -->'
       write(io,100)'<FTABDB version="1.0" author="FtabMan"'
       write(io,100)'  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"'
       write(io,100)'  xsi:noNamespaceSchemaLocation="ftabdb.xsd">'
! Structured for multiple studies with multiple cases.
       write(io,100)' <Study> <!-- Group of runs of similar vintage with same table formats -->'
       call writekeystr(io,'StudyID',trim(StudyID),indent) !                               !  need to get from a scedes variable or ftab.dat
       call writekeystr(io,'LongTitle',trim(LongTitle),indent)  !  need to get from a scedes variable or ftab.dat
! multiple cases in a study.  Link cases to Study via StudyID
       write(io,100) repeat(' ',indent)//'<Case>'
       indent=indent+1
       call writekeystr(io,'StudyID',trim(StudyID),indent)
       call writekeystr(io,'CaseDescription',trim(CaseDescription),indent)
       call writekeystr(io,'ScenLabel',trim(ScenLabel),indent)
       call writekeystr(io,'Scenario',frscen,indent)
! multiple runs made under each case.  normally one is published with specific datekey, but set up
! data structure to allow more so two runs with same scenario and different datekeys can be compared.
! Remainder is the start of a run's xml hierarchy
       write(io,'(10a)') ' <!--',('==========',i=1,8),' -->'
       write(io,100) repeat(' ',indent)//'<Run> <!-- To hold results of a single NEMS run -->'
100 format(a)
       indent=indent+1
       call writekeystr(io,'StudyID',trim(StudyID),indent)
       call writekeystr(io,'Scenario',frscen,indent)
       call writekeystr(io,'Datekey',fdate,indent)
       call writekeystr(io,'ScenDate',trim(frscen)//'.'//trim(fdate),indent)
     else
       indent=4
     endif  !  If(.not. once)

     if(itab.eq.9999) then  ! close file signal
       indent=indent-1
       write(io,'(a)') repeat(' ',indent)//'</Run>'
       indent=indent-1
       write(io,'(a)') repeat(' ',indent)//'</Case>'
       indent=indent-1
       write(io,'(a)') repeat(' ',indent)//'</Study>'
       write(io,'(a)') '</FTABDB>'
       close(unit=io)
       return
     endif

     RGTKey=RGName(itab)
     write(RGTKey(4:6),'(i3.3)') iregn

     write(io,'(10a)') repeat(' ',indent)//'<!--',('==========',i=1,8),' -->'
     write(io,100) repeat(' ',indent)//'<Table>'
     indent=indent+1
     call writekeystr(io,'TabKey',RGTKey,indent)
     call writekeystr(io,'Scenario',frscen,indent)
     call writekeystr(io,'Datekey',fdate,indent)
     call writekeyint(io,'TableNumber',itab,indent)
     call writekeystr(io,'Title',tabhed(1,itab),indent)
     call writekeystr(io,'SubTitle',tabhed(2,itab),indent)
     call writekeystr(io,'Stubhead',rowhed(itab),indent)
     if(iregn.eq.0) then
       call writekeystr(io,'Region','United States',indent)
     else
       do i=1,no_reg_tables                 ! loop over regional tables in array regtables
         if (regtables(i) .eq. itab) then
           if (nregtables(i) .eq. 9) then
             call writekeystr(io,'Region',regnam(iregn),indent)
           elseif (nregtables(i) .eq. num_elec_regions) then
             call writekeystr(io,'Region',nercnam(iregn),indent)
           endif
           exit  ! exit do loop
         endif
       enddo
     endif

     call writekeyint(io,'RegionNum',iregn,indent)

! go through rows of table.

     indent=indent+1
     do irow=1,nrows(itab)
       if(ftabbone.ne.1 .and. drform(irow,itab)(6:6).eq.'1') then
         cycle  ! skip "bonus" rows
       endif

       ir=irows(irow,itab)
       write(rowstr,'(i3.3)') irow
       if(ir.eq.0.and.len_trim(rowlab(irow,itab)).eq.0) then !  then this is a blank line
         write(io,100) repeat(' ',indent-1)//'<Row>'
         call writekeystr(io,'TabKey',RGTKey,indent)
         call writekeystr(io,'Scenario',frscen,indent)
         call writekeystr(io,'Datekey',fdate,indent)
         call writekeystr(io,'RowNum',rowstr,indent)
         write(io,100) repeat(' ',indent-1)//'</Row>'
       elseif(ir.eq.0 .and. len_trim(rowlab(irow,itab)).gt.0) then  ! non-data row with stub
         write(io,100)repeat(' ',indent-1)//'<Row Font="Bold">'
         call writekeystr(io,'TabKey',RGTKey,indent)
         call writekeystr(io,'Scenario',frscen,indent)
         call writekeystr(io,'Datekey',fdate,indent)
         call writekeystr(io,'RowNum',rowstr,indent)
         call writekeystr(io,'Stub',rowlab(irow,itab),indent)
         write(io,100)repeat(' ',indent-1)//'</Row>'
       elseif(ir.gt.0) then
         if(DRForm(irow,itab)(1:1).eq.'B') then
           write(io,100)repeat(' ',indent-1)//'<Row Font="Bold">'
         else
           write(io,100)repeat(' ',indent-1)//'<Row Font="Plain">'
         endif
         call writekeystr(io,'TabKey',RGTKey,indent)
         call writekeystr(io,'Scenario',frscen,indent)
         call writekeystr(io,'Datekey',fdate,indent)
         call writekeystr(io,'RowNum',rowstr,indent)
         call writekeystr(io,'ID',trim(RGTKEY)//trim(DRName(irow,itab)),indent)
         call writekeystr(io,'Stub',rowlab(irow,itab),indent)
         call writekeyint(io,'RowFmt',rowfmt(irow,itab),indent)
         call writekeystr(io,'GLabel',graph_label(irow,itab),indent)
         call writekeystr(io,'GUnits',graph_units(irow,itab),indent)
         if(len_trim(data_type(irow,itab)).gt.0)         call writekeystr(io,'DaType',data_type(irow,itab),indent)
         if(len_trim(sub_data_type(irow,itab)).gt.0)     call writekeystr(io,'SubDat',sub_data_type(irow,itab),indent)
         if(len_trim(sector(irow,itab)).gt.0)            call writekeystr(io,'Sector',sector(irow,itab),indent)
         if(len_trim(subsector(irow,itab)).gt.0)         call writekeystr(io,'SubSec',subsector(irow,itab),indent)
         if(len_trim(energy_source(irow,itab)).gt.0)     call writekeystr(io,'Source',energy_source(irow,itab),indent)
         if(len_trim(sub_energy_source(irow,itab)).gt.0) call writekeystr(io,'SubSrc',sub_energy_source(irow,itab),indent)
         if(len_trim(geography(irow,itab)).gt.0)         call writekeystr(io,'Geogra',geography(irow,itab),indent)
         if(len_trim(varname(irow,itab)).gt.0)           call writekeystr(io,'VarNam',varname(irow,itab),indent)

         do i=1,ncols-igrowo  ! skip growth column for now, if there is one.
           call writekeyval(io,'Y'//trim(collab(i)(2:)),tabn(ir,icols(i),iscen(i)),indent)
         enddo
         if(igrowo) then
           call writekeyval(io,'Growth',tabn(ir,icols(ncols),iscen(ncols)),indent)
         endif
         write(io,100) repeat(' ',indent-1)//'</Row>'
       endif
     enddo
     do i=1,nfoot(itab)
       write(io,100) repeat(' ',indent-1)//'<Foot>'
       call writekeystr(io,'TabKey',RGTKey,indent)
       call writekeystr(io,'Scenario',frscen,indent)
       call writekeystr(io,'Datekey',fdate,indent)
       write(rowstr,'(i3.3)') i
       call writekeystr(io,'FootNum',rowstr,indent)
       call writekeystr(io,'FootNote',footnt(i,itab),indent)
       write(io,100) repeat(' ',indent-1)//'</Foot>'
     enddo
     indent=indent-2
     write(io,100) repeat(' ',indent)//'</Table>'
  return
  end subroutine FXMLDB
!===============================================================================
  subroutine writekeystr(io,key,str,indent)
  implicit none
  integer io  ! unit number
  character*(*) key
  character*(*) str
  integer indent,is
  is=index(str,'&')
  if(is.gt.0) then
    str=str(1:is-1)//'and'//str(is+1:)
  endif
  is=scan(str,'<>')
    if(is.gt.0) then
    if(str(is:is).eq.'<') str=str(1:is-1)//' lt '//str(is+1:)
    if(str(is:is).eq.'>') str=str(1:is-1)//' gt '//str(is+1:)
  endif

  write(io,'(a)') REPEAT(' ',indent)//'<'//trim(key)//'>'//trim(str)//'</'//trim(key)//'>'
  return
  end
 !===============================================================================
  subroutine writekeyint(io,key,val,indent)
  implicit none
  integer io  ! unit number
  character*(*) key
  integer val

  integer indent
  character*16 field
  write(field,'(i16)') val
  if(index(field,'*').gt.0) field='0'
  if(index(field,'Nan').gt.0) field='0'
  if(index(field,'Infinity').gt.0) field='0'
  field=adjustl(field)
  write(io,'(a)') REPEAT(' ',indent)//'<'//key//'>'//trim(field)//'</'//key//'>'
  return
  end
 !===============================================================================
  subroutine writekeyval(io,key,val,indent)
  implicit none
  integer io  ! unit number
  character*(*) key
  real val
  integer indent
  character*16 field
  write(field,'(f16.6)') val
  if(index(field,'*').gt.0) field='0'
  if(index(field,'Nan').gt.0) field='0'
  if(index(field,'Infinity').gt.0) field='0'
  field=adjustl(field)
  write(io,'(a)') REPEAT(' ',indent)//'<'//key//'>'//trim(field)//'</'//key//'>'
  return
  end
  !============================================================================================================================





 !==========================================================================
 subroutine csv_field(line,fields,maxfield,cnt)
 implicit none

!  This subroutine counts the comma-delimited fields on a line
! and sets the starting and ending position of each field number
!
! line:  character string holding the line of input (intent::in)
! fields: starting and ending position of each of the fields (intent::out)
! maxfield: size of array fields (intent::in)
! cnt:  number of fields found on the line (intent::out)

 integer cnt,maxfield
 integer fields(2,maxfield)
 character*(*) line
 integer line_len,ipos,icomma,iquote

 cnt=1
 line_len=len_trim(line)
 ipos=0
 fields=0
 if(line_len.eq.0) then
   cnt=0
   return
 endif

 do while (ipos .le. (line_len) .and. cnt .le. maxfield)   ! .and. cnt.le.maxfield)

! set field count, position, and starting position of field

   ipos=ipos+1
   fields(1,cnt)=ipos

   ! if field starts with a " find next "
   if(line(ipos:ipos).eq.'"') then
      iquote=index(line(ipos+1:line_len),'"')
      fields(1,cnt)=ipos+1
      do while(iquote.gt.0)
        if(line(ipos+iquote+1:ipos+iquote+1).eq.'"')then
          ipos=ipos+iquote+2  ! skip double quotes which signify
          iquote=index(line(ipos+1:line_len),'"')
        else
          ipos=ipos+iquote
          iquote=0
        endif
      enddo
   endif
   ! find next comma
   icomma=index(line(ipos:line_len),',')
   if(icomma.ne.0) then
     fields(2,cnt)=ipos+icomma-2
     if(fields(2,cnt).lt.fields(1,cnt)) fields(2,cnt)=fields(1,cnt)
     cnt=cnt+1
     ipos=ipos+icomma-1

   elseif(line(line_len:line_len).eq.'"') then  ! last field, ends in a "
     fields(2,cnt)=line_len-1
     if(fields(2,cnt).lt.fields(1,cnt)) fields(2,cnt)=fields(1,cnt)
!     cnt=cnt+1
     ipos=line_len+1
   else                                         ! last field, ends in a character is empty
     fields(2,cnt)=line_len
     if(fields(2,cnt).lt.fields(1,cnt)) fields(2,cnt)=fields(1,cnt)
!     cnt=cnt+1
     ipos=line_len+1
   endif

 enddo
 return
 end subroutine csv_field

!*********************************************************************

 subroutine get_csv_field(line,fields,maxfield,ifield,afield)
 implicit none
  character*(*) afield
  character*(*) line

  integer maxfield
  integer fields(2,maxfield)

  integer ifield,i,j,n

  afield=' '
  afield=line(fields(1,ifield):fields(2,ifield))
  if(fields(1,ifield).eq.fields(2,ifield).and. &
    afield(1:1).eq.',') then
      afield=' '
      return
  endif

  n=len_trim(afield)
  if(n.gt.2) then
    if(afield(n:n).eq.'"'.and.(afield(n-1:n-1).ne.'"'.or.afield(n-2:n-1).eq.'""')) then
      afield(n:n)=' '
    endif
    if(afield(1:1).eq.'"'.and.afield(2:2).ne.'"') then
      afield=afield(2:)
    endif
  endif
  i=index(afield,'""')
  do while (i.gt.0)
    if(i.gt.1) then
      afield=afield(:i-1)//afield(i+1:)
    else
      afield=afield(2:)
    endif
    i=index(afield,'""')
  end do
  return
 end subroutine get_csv_field

!*********************************************************************

 subroutine Browser_Out(ITAB,IGROWO,IGRWYR,SCENID,IREGN)
  implicit none
! writes out run record, table records, and long-thin file  for table-browser interface

    include 'parametr'
    include 'ncntrl'
    include 'ftable'

    INTEGER ITAB ! table index
    INTEGER IGROWO ! growth rate option, =1 for growth rates
    INTEGER IGRWYR ! starting year for growth rate option
    CHARACTER*(MCOLWD) SCENID(MAXYR)
    INTEGER IREGN ! Region index, if applicable, 0 for national-level table
   integer i,ir,iii,itb,irow    ! index variables
   integer io/113/   ! output unit 113

   logical once            ! set to true after file is opened so it will only happen once
   logical lexist
   character*166 line
   character*6 RGTKey      ! unique identifying key for each table
   character*16 key,str*150,rowstr*3,rowstr2*3
   character*26 scendate
   character*1  RegionType    ! 0=not; 1=Census divisions; 2=defunct 13 EMM regions; 3=22 EMM regions; 5=25 EMM regions; 4=IEO use
   character    RegionNum*2,TableNumber*3,SequenceNumber*3,font*6,ID*24,Scenario*20,Datekey*9,CRowFmt*1
   character*1 cbonus(0:1)
   integer ibonus
   integer nf100 ! Number of fields to write using Format 100
   character*26 column_head(maxyr),style_head(maxyr)*3,style_col(7)*3
   cbonus(0:1)=(/'0','1'/)
   

     if(nscen.gt.1) return  ! this option is only relevant for a one-scenario run
     If(.not. once) then
! On first time through, open the four output files and write header lines
       once=.true.

       open(unit=io,  file=trim(frscen)//'.'//fdate(2:5)//trim(fdate(8:9))//'.db_study.txt',status='unknown')
       open(unit=io+1,file=trim(frscen)//'.'//fdate(2:5)//trim(fdate(8:9))//'.db_heading.txt',status='unknown')
       open(unit=io+2,file=trim(frscen)//'.'//fdate(2:5)//trim(fdate(8:9))//'.db_footnote.txt',status='unknown')
       open(unit=io+3,file=trim(frscen)//'.'//fdate(2:5)//trim(fdate(8:9))//'.db_row.txt',status='unknown')
       open(unit=io+4,file=trim(frscen)//'.'//fdate(2:5)//trim(fdate(8:9))//'.db_longthin.txt',status='unknown')

       Scenario=frscen
       Datekey=fdate
       ScenDate=trim(frscen)//'.'//trim(fdate)

       write(io,'(a)') 'StudyID|LongTitle|CaseDescription|ScenLabel|Scenario|DateKey|ScenDate'
       nf100=7
       write(io,100)         &
       trim(StudyID),        &
       trim(LongTitle),      &
       trim(CaseDescription),&
       trim(ScenLabel),      &
       trim(Scenario),       &
       trim(Datekey),        &
       trim(ScenDate)

100   format(a,<nf100-1>('|',a))

! Header line for Heading File

       write(io+1,'(a)') 'StudyID|TableNumber|SequenceNumber|Title|SubTitle|StubHead|RegionType' ! ,Subject'

! Header line for Footnote File

       write(io+2,'(a)') 'StudyID|TableNumber|Footnum|Footnote'

! Header line for Row file

       write(io+3,'(a)') 'StudyID|TableNumber|RowNum|Stub|Font|RowFmt|GLabel|Gunits|DaType|SubDat|Sector|SubSec|Source|SubSrc|Geogr|VarName|Bonus|Irow'

! Header line for Long-Thin file

       write(io+4,'(a)') 'TableNumber|RowNum|RegionNum|Year|Data|StudyID='//trim(StudyID)//'| Scenario='//trim(frscen)//'| Datekey='//trim(DateKey)

     endif  !  If(.not. once)


     RGTKey=RGName(itab)
     write(RGTKey(4:6),'(i3.3)') iregn

! Assign Region
! removed 13 EMM region code as outdated and 4 fake region option as IEO uses option 4 and we won't publish that table (21)
     RegionType='0'
     do i=1,no_reg_tables                 ! loop over regional tables in array regtables
       if (regtables(i) .eq. itab) then
         if (nregtables(i) .eq. 9) then
           RegionType='1'
         elseif (nregtables(i) .eq. 22) then
           RegionType='3'
         elseif (nregtables(i) .eq. 25) then
           RegionType='5'
         endif
         exit  ! found it, exit do loop
       endif
     enddo

     write(RegionNum,'(i2)') iregn
     write(TableNumber,'(i3.3)') itab
     write(SequenceNumber,'(i3)') SeqNum(itab)

! Write record for Headings File (only do for first/national region)
     if(iregn.eq.0) then
       nf100=7
       write(io+1,100)          &
       trim(StudyID),           &   ! StudyID
       trim(TableNumber),       &   ! TableNumber
       trim(SequenceNumber),    &   ! Sequence Number
!      trim(tabhed(1,itab)),    &   ! Title
       "Table " // trim(adjustl(SequenceNumber)) // ".  " // trim(tabhed(1,itab)),    &   ! Title
       trim(tabhed(2,itab)),    &   ! SubTitle
       trim(rowhed(itab)),      &   ! Stubhead
       trim(RegionType)             ! RegionType

! Write Footnote records
       nf100=4
       do i=1,nfoot(itab)
         write(rowstr,'(i3.3)') i
         write(io+2,100)     &
         trim(StudyID),      & ! StudyID
         trim(TableNumber),  & ! TableNumber
         trim(rowstr),       & ! FootNum
         trim(footnt(i,itab))  ! FootNote
       enddo
     endif
! go through rows of table.


     do irow=1,nrows(itab)
       ir=irows(irow,itab)
       write(rowstr,'(i3.3)') irow
       write(rowstr2,'(i3.3)') ir

! set bonus row indicator
       ibonus=0
       if (drform(irow,itab)(6:6).eq.'1') then
         ibonus=1
       endif
!my attempt:
       if(ftabbone.ne.1 .and. drform(irow,itab)(6:6).eq.'1') then
         cycle  ! skip "bonus" rows
       endif

       if(iregn.eq.0) then
! set row font
        if(    ir.eq.0 .and. len_trim(rowlab(irow,itab)).eq.0) Then
         font=''
         ID=''
        elseif(ir.eq.0 .and. len_trim(rowlab(irow,itab)).gt.0) Then
         font='Bold'
         ID=''
        else
         if(DRForm(irow,itab)(1:1).eq.'B') then
           Font='Bold'
         else
           Font='Plain'
         endif
         ID=trim(RGTKEY)//trim(DRName(irow,itab))
        endif
        if(rowfmt(irow,itab).eq.0) then
         write(CRowFmt,'(i1)') TABFMT(itab)
         if(tabfmt(itab).eq.9) CRowFMT='0'
        elseif(rowfmt(irow,itab).eq.9) then
         CRowFMT='0'
        else
         write(CRowFmt,'(i1)') rowfmt(irow,itab)
        endif
! Write record for Rows file
       nf100=18
       write(io+3,100) &
       trim(StudyID),                      &    ! StudyID
       trim(TableNumber),                  &    ! TableNumber
       trim(rowstr),                       &    ! RowNum
       trim(rowlab(irow,itab)),            &    ! Stub
       trim(Font),                         &    ! Font
       trim(CRowFmt),                      &    ! RowFmt
       trim(graph_label(irow,itab)),       &    ! GLabel
       trim(graph_units(irow,itab)),       &    ! Gunits
       trim(data_type(irow,itab)),         &    ! DaType
       trim(sub_data_type(irow,itab)),     &    ! SubDat
       trim(sector(irow,itab)),            &    ! Sector
       trim(subsector(irow,itab)),         &    ! SubSec
       trim(energy_source(irow,itab)),     &    ! Source
       trim(sub_energy_source(irow,itab)), &    ! SubSrc
       trim(geography(irow,itab)),         &    ! Geogr
       trim(varname(irow,itab)),           &    ! VarName
       cbonus(ibonus),                     &    ! Bonus
       rowstr2                                  ! irow
       endif

! Write Long-Thin records
       if(ir.gt.0) then
         do i=1,ncols-igrowo  ! skip growth column for now, if there is one.
           if (tabn(ir,icols(i),iscen(i)) .eq. -999.0) then
             write(io+4,201)              &
             trim(TableNumber),           &   ! TableNumber
             trim(rowstr),                &   ! RowNum
             trim(RegionNum),             &   ! RegionNum
             trim(collab(i)(2:)),         &   ! Year
             "- -"                            ! not applicable
           else if (tabn(ir,icols(i),iscen(i)) .eq. -888.0) then
             write(io+4,201)              &
             trim(TableNumber),           &   ! TableNumber
             trim(rowstr),                &   ! RowNum
             trim(RegionNum),             &   ! RegionNum
             trim(collab(i)(2:)),         &   ! Year
             "w"                              ! withheld
           else
           write(io+4,200)              &
           trim(TableNumber),           &   ! TableNumber
           trim(rowstr),                &   ! RowNum
           trim(RegionNum),             &   ! RegionNum
           trim(collab(i)(2:)),         &   ! Year
           tabn(ir,icols(i),iscen(i))       ! Data
           endif
         enddo
         i=ncols
         if(igrowo) then
           if (tabn(ir,icols(i),iscen(i)) .eq. -999.0) then
             write(io+4,201)              &
             trim(TableNumber),           &   ! TableNumber
             trim(rowstr),                &   ! RowNum
             trim(RegionNum),             &   ! RegionNum
             '9999',                      &   ! Year to signify Growth Rate
             "- -"
           else if (tabn(ir,icols(i),iscen(i)) .eq. -888.0) then
             write(io+4,201)              &
             trim(TableNumber),           &   ! TableNumber
             trim(rowstr),                &   ! RowNum
             trim(RegionNum),             &   ! RegionNum
             '9999',                      &   ! Year to signify Growth Rate
             "w"
           else
             write(io+4,200)              &
             trim(TableNumber),           &   ! TableNumber
             trim(rowstr),                &   ! RowNum
             trim(RegionNum),             &   ! RegionNum
             '9999',                      &   ! Year to signify Growth Rate
             tabn(ir,icols(i),iscen(i))
         endif
       endif
       endif
     enddo

200  format(a,'|',a,'|',a,'|',a,'|',f13.6)
201  format(a,'|',a,'|',a,'|',a,'|',a)
  return
  end subroutine Browser_Out

!*********************************************************************

 subroutine API_Out(ITAB,IGROWO,IGRWYR,SCENID,IREGN)
  implicit none
! writes out flat csv file for database and excel-filtering. Also for a planned web-based Application Program Interface (API).

    include 'parametr'
    include 'ncntrl'
    include 'ftable'

    INTEGER ITAB ! table index
    INTEGER IGROWO ! growth rate option, =1 for growth rates
    INTEGER IGRWYR ! starting year for growth rate option
    CHARACTER*(MCOLWD) SCENID(MAXYR)
    INTEGER IREGN ! Region index, if applicable, 0 for national-level table
   integer i,ir,iii,itb,irow    ! index variables
   integer io/213/   ! output unit
   integer Region4api
   logical once            ! set to true after file is opened so it will only happen once
   logical lexist
   character*16 key,rowstr*3,rowstr2*3
   character*26 scendate
   character    RegionNum*2,TableNumber*3,SequenceNumber*3,font*6,ID*24,Scenario*20,Datekey*9,CRowFmt*1

    character*20 div_name(0:9),div_abbr(0:9)
    character*55 nerc_name(0:25),nerc_abbr(0:25)
    character*52 geo_override,geo_over_abr*10
    character*60 varnam2(MaxRow,MaxTab)
    




   integer ibonus
   integer nf100 ! Number of alphanumeric fields to write using Format 100
   integer nnumeric ! number of numeric fields to write in Format 100
   character*26 column_head(maxyr),style_head(maxyr)*3,style_col(7)*3,sep*1


!  these names are for filling in the geography field for regional versions of tables
    div_name=(/   &
     'united states',         &
     'new england',           &
     'middle atlantic',       &
     'east north central',    &
     'west north central',    &
     'south atlantic',        &
     'east south central',    &
     'west south central',    &
     'mountain',              &
     'pacific'/)

    div_abbr=(/    &
     'usa',        &
     'nengl',      &
     'mdat1',      &
     'enc',        &
     'wnc',        &
     'soatl',      &
     'esc',        &
     'wsc',        &
     'mtn',        &
     'pcf'/)

      IF (num_elec_regions .eq. 22) THEN
      nerc_name=(/      &
     'united states',                                        &
     'texas regional entity',                                &
     'florida reliability coordinating council',             &
     'midwest reliability council/ east',                    &
     'midwest reliability council/ west',                    &
     'northeast power coordinating council/ northeast',      &
     'northeast power coordinating council/ nyc-westchest',  &
     'northeast power coordinating council/ long island',    &
     'northeast power coordinating council/ upstate ny',     &
     'reliability first corporation/ east',                  &
     'reliability first corporation/ michigan',              &
     'reliability first corporation/ west',                  &
     'serc reliability corporation/ delta',                  &
     'serc reliability corporation/ gateway',                &
     'serc reliability corporation/ southeastern',           &
     'serc reliability corporation/ central',                &
     'serc reliability corporation/ virginia-carolina',      &
     'southwest power pool/ north',                          &
     'southwest power pool/ south',                          &
     'western electricity coordinating council/ southwest',  &
     'western electricity coordinating council/ california', &
     'western electricity/ northwest power pool',            &
     'western electricity coordinating council/ rockies',    &
     'extra23',                                              &
     'extra24',                                              &
     'extra25'/)       

        
   nerc_abbr=(/   &
     'usa',       &
     'tre',       &
     'flrc',      &
     'mwrce',     &
     'mwrcw',     &
     'npccne',    &
     'npccnywe',  &
     'npccli',    &
     'npccupny',  &
     'rfcet',     &
     'rfcmi',     &
     'rfcwt',     &
     'sercdlt',   &
     'sercgw',    &
     'sercsoes',  &
     'serccnt',   &
     'sercvc',    &
     'swppno',    &
     'swppso',    &
     'weccsw',    &
     'weccca',    &
     'wenwpp',    &
     'weccrks',   &
     'extra23',   &
     'extra24',   &
     'extra25'/)       

        
      ELSEIF (num_elec_regions .eq. 25) THEN
       nerc_name=(/      &
     'united states',                                        &
     'texas regional entity',                                &
     'florida reliability coordinating council',             &
     'midwest reliability council/ east',                    &
     'midwest reliability council/ west',                    &
     'northeast power coordinating council/ northeast',      &
     'northeast power coordinating council/ nyc-westchest',  &
     'northeast power coordinating council/ long island',    &
     'northeast power coordinating council/ upstate ny',     &
     'reliability first corporation/ east',                  &
     'reliability first corporation/ michigan',              &
     'reliability first corporation/ west',                  &
     'serc reliability corporation/ delta',                  &
     'serc reliability corporation/ gateway',                &
     'serc reliability corporation/ southeastern',           &
     'serc reliability corporation/ central',                &
     'serc reliability corporation/ virginia-carolina',      &
     'southwest power pool/ north',                          &
     'southwest power pool/ south',                          &
     'western electricity coordinating council/ southwest',  &
     'western electricity coordinating council/ california', &
     'western electricity/ northwest power pool',            &
     'western electricity coordinating council/ rockies',    &
     'real23',                                               &
     'real24',                                               &
     'real25'/)       
 
  nerc_abbr=(/   &
     'usa',      &
     'tre',      &
     'flrc',     &
     'mwrce',    &
     'mwrcw',    &
     'npccne',   &
     'npccnywe', &
     'npccli',   &
     'npccupny', &
     'rfcet',    &
     'rfcmi',    &
     'rfcwt',    &
     'sercdlt',  &
     'sercgw',   &
     'sercsoes', &
     'serccnt',  &
     'sercvc',   &
     'swppno',   &
     'swppso',   &
     'weccsw',   &
     'weccca',   &
     'wenwpp',   &
     'weccrks',  &
     'real23',   &
     'real24',   &
     'real25'/)

      ENDIF




     if(nscen.gt.1) return  ! this option is only relevant for a one-scenario run
     If(.not. once) then
! On first time through, open the output file and write header line
       once=.true.

       open(unit=io,  file=trim(frscen)//'.'//fdate(2:5)//trim(fdate(8:9))//'.api.csv',status='unknown')
       write(6,'(a)') 'The Database/API csv output is named '//trim(frscen)//'.'//fdate(2:5)//trim(fdate(8:9))//'.api.csv'

       Scenario=frscen
       Datekey=fdate
       ScenDate=trim(frscen)//'.'//trim(fdate)

! Header line

       sep=','  ! field separator: use comma or bar (|)
       write(io,'(150a)') &
       'Scenario',   sep,    &
       'Datekey',    sep,    &
       'TableNumber',sep,    &
       'RowNum',     sep,    &
       'RegionNum',  sep,    &
       'VarName',    sep,    &
       'VarNam2',    sep,    &
       'GLabel',     sep,    &
       'Gunits',     sep,    &
       'RowFmt',     sep,    &
       'DaType',     sep,    &
       'SubDat',     sep,    &
       'Sector',     sep,    &
       'SubSec',     sep,    &
       'Source',     sep,    &
       'SubSrc',     sep,    &
       'Geogr',              &
       (sep,trim(collab(i)(2:)),i=1,ncols-igrowo),sep, &
       'StudyID=',trim(StudyID),sep

     endif  !  If(.not. once)

! Assign Region
     Region4api=0
     do i=1,no_reg_tables                 ! loop over regional tables in array regtables
       if (regtables(i) .eq. itab) then
         if     (nregtables(i) .eq.  9) then
           Region4api=1
         elseif (nregtables(i) .eq. 22) then
           Region4api=3
         elseif (nregtables(i) .eq. 25) then
           Region4api=5
         endif
         exit  ! found, exit de do loop
       endif
     enddo

     if (region4api .eq. 0 .or. iregn .eq. 0) then
       geo_override='united states'
       geo_over_abr='usa'

     else
       if     (region4api .eq. 1) then           ! Census Division
         geo_override=div_name(iregn)
         geo_over_abr=div_abbr(iregn)
       elseif (region4api .eq. 3) then           ! 22 EMM regions
         geo_override=nerc_name(iregn)
         geo_over_abr=nerc_abbr(iregn)
       elseif (region4api .eq. 5) then           ! 25 EMM regions
         geo_override=nerc_name(iregn)
         geo_over_abr=nerc_abbr(iregn)
       endif
     endif
     write(RegionNum,'(i2)') iregn
     write(TableNumber,'(i3.3)') itab



! loop through rows of table



     do irow=1,nrows(itab)
       ir=irows(irow,itab)
       write(rowstr,'(i3.3)') irow
       write(rowstr2,'(i3.3)') ir

! set bonus row indicator
       ibonus=0
       if (drform(irow,itab)(6:6).eq.'1') then
         ibonus=1
       endif

       if(rowfmt(irow,itab).eq.0) then
         write(CRowFmt,'(i1)') TABFMT(itab)
         if(tabfmt(itab).eq.9) CRowFMT='0'
       elseif(rowfmt(irow,itab).eq.9) then
         CRowFMT='0'
       else
         write(CRowFmt,'(i1)') rowfmt(irow,itab)
       endif


       if(ir.gt.0) then ! data rows only
        if(iregn.eq.0) then  ! if national table
           if(len_trim(geography(irow,itab)).eq.0) then   ! and geography is not filled
              geography(irow,itab)=geo_override           ! then override it with united states
              if(len_trim(varname(irow,itab)).gt.0) then
                call replace_node(varname(irow,itab),'_',7,geo_over_abr)  ! replace the 7th node of the string with the geography override abbreviation
              endif
           endif
         else
           geography(irow,itab)=geo_override
           call replace_node(varname(irow,itab),'_',7,geo_over_abr)  ! replace the 7th node of the string with the geography override abbreviation
         endif
       endif
! write in csv format
! TableNumber,RowNum,RegionNum,VarName,GLabel,Gunits,RowFmt,DaType,SubDat,Sector,SubSec,Source,SubSrc,Geogr,(numeric(year),year=1,numeric)

       nf100=17
       nnumeric=ncols-igrowo

       if(ir.gt.0 .and. (ftabbone.eq.1 .or. ibonus.eq.0 ))  then    ! write data rows skipping bonus rows unless switch to write them is on.

         if(iregn.eq.0) then  ! if national table
           if(len_trim(geography(irow,itab)).eq.0) then   ! and geography is not filled
              geography(irow,itab)=geo_override           ! then override it
              call replace_node(varname(irow,itab),'_',7,geo_over_abr)  ! replace the 7th node of the string with the geography override abbreviation
           endif
         else
           geography(irow,itab)=geo_override
           call replace_node(varname(irow,itab),'_',7,geo_over_abr)  ! replace the 7th node of the string with the geography override abbreviation
         endif

         varnam2(irow,itab)=trim(RGNAME(itab))//'000:'//trim(DRNAME(irow,itab))


 100     format('"',a,<nf100-1>('"',a1,'"',a),'"',<nnumeric>(a1,F25.6))
         write(io,100) &
         trim(Scenario),                     sep,      &    ! Scenario
         trim(Datekey),                      sep,      &    ! DateKey
         trim(TableNumber),                  sep,      &    ! TableNumber
         trim(rowstr),                       sep,      &    ! RowNum
         trim(RegionNum),                    sep,      &    ! RegionNum
         trim(varname(irow,itab)),           sep,      &    ! VarName
         trim(varnam2(irow,itab)),           sep,      &    ! VarNam2
         trim(graph_label(irow,itab)),       sep,      &    ! GLabel
         trim(graph_units(irow,itab)),       sep,      &    ! Gunits
         trim(CRowFmt),                      sep,      &    ! RowFmt
         trim(data_type(irow,itab)),         sep,      &    ! DaType
         trim(sub_data_type(irow,itab)),     sep,      &    ! SubDat
         trim(sector(irow,itab)),            sep,      &    ! Sector
         trim(subsector(irow,itab)),         sep,      &    ! SubSec
         trim(energy_source(irow,itab)),     sep,      &    ! Source
         trim(sub_energy_source(irow,itab)), sep,      &    ! SubSrc
         trim(geography(irow,itab)),                   &    ! Geogr
         (sep,tabn(ir,icols(i),iscen(i)),i=1,nnumeric)

       endif
     enddo
  return
  end subroutine API_Out

!===============================================================================
      SUBROUTINE ISORT(A,IDX,N,NMAX)
      IMPLICIT NONE
! SORTS FIRST N ELEMENTS OF INTEGER ARRAY A.  IT ALSO SORTS AN INDEXING
! ARRAY, IDX, WHICH CAN BE USED TO MAP THE SORT ORDER TO OTHER ARRAYS.
! IDX SHOULD BE PASSED WITH THE SERIES 1, 2, ... N.
! THIS SORT METHODOLOGY, CALLED QUICKSORT, IS CONSIDERED AN EFFICIENT
! APPROACH FOR SORTING LARGE ARRAYS

      INTEGER*4 NMAX,N
      integer*4 A(NMAX),X,W
      INTEGER*4 IDX(NMAX)
      INTEGER*4 STACK(16,2),L,R,S,J,I,II,ITEMP
      S=1
      STACK(1,1)=1
      STACK(1,2)=N
10    CONTINUE
      L=STACK(S,1)
      R=STACK(S,2)
      S=S-1
20    CONTINUE
        I=L
        J=R
        II=(L+R)/2
        X=A(II)
30      CONTINUE
40        CONTINUE
            IF(A(I).LT.X) THEN
               I=I+1
               GO TO 40
            ENDIF
50        CONTINUE
            IF(X.LT.A(J)) THEN
              J=J-1
              GO TO 50
            ENDIF
          IF(I.LE.J) THEN
            W=A(I)
            A(I)=A(J)
            A(J)=W
            ITEMP=IDX(I)
            IDX(I)=IDX(J)
            IDX(J)=ITEMP
            I=I+1
            J=J-1
          ENDIF
          IF(I.LE.J) GO TO 30
        IF((J-L).LT.(R-I)) THEN
          IF(I.LT.R) THEN
            S=S+1
            STACK(S,1)=I
            STACK(S,2)=R
          ENDIF
          R=J
        ELSE
          IF(L.LT.J) THEN
            S=S+1
            STACK(S,1)=L
            STACK(S,2)=J
          ENDIF
          L=I
        ENDIF
        IF(L.LT.R) GO TO 20
      IF(S.GT.0) GO TO 10
      RETURN
      END

 !=====================================================================================
      subroutine replace_node(instr,sep,Nth,repl)
      implicit none
 ! count the number of substring "nodes" in the input string, instr
 ! "node" meaning a substring separated by the character sep
 ! replace the Nth such node with the replacement string, repl

      character*(*) instr
      character*1 sep
      integer Nth
      character*(*) repl
      integer is,ie,nrepl,L,i
      integer nnode
      character*10 nodes(10)

     L=len_trim(instr)
      if(l.lt.2) return
      nnode=1
      is=1

      do i=2,L
        if(instr(i:i).eq.sep) then
          nodes(nnode)=instr(is:i)
          nnode=nnode+1
          is=i+1
        endif
      enddo

      if(instr(L:L).ne.sep) then
        nodes(nnode)=instr(is:L)
      else
        nnode=nnode-1
      endif

      if(nth.le.nnode) then
        if(nth.lt.nnode) then
          nodes(nth)=trim(repl)//sep
        else
          nodes(nth)=trim(repl)
        endif
        write(instr,'(10a)') (trim(nodes(i)),i=1,nnode)
      endif

      return
      end

