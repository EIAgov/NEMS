! $Header: m:/default/source/RCS/filer.f,v 1.105 2019/08/05 18:28:29 pkc Exp $
      SUBROUTINE FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)

!DSA!       use gdxf9def
!
! FILER UTILITY ROUTINE.
!
! -  CALLED BY NEMS MAIN (INTEGRATING MODULE), FTAB, AND A STANDALONE DRIVER CALLED TFILER.
!    PROCESSES (READS AND WRITES) THE RESTART DATA FILE.
!
! OVERVIEW OF THE APPROACH
! -  "FILER" READS OR WRITES VARIABLES TO/FROM FILE USING A GENERIC, READABLE
!    FORMAT BY VARIABLE NAME.  THE VARIABLES MUST BE DEFINED IN COMMON BLOCKS
!    AND DECLARED USING "INCLUDE" FILES.
!
! -  FILER USES A DICTIONARY FILE DEFINING THE VARIABLES IN COMMON BLOCKS.  THE
!    DICTIONARY FILE INCLUDES THE SIZE/DIMENSIONS OF THE VARIABLES AND
!    MUST BE MAINTAINED IN COORDINATION WITH THE COMMON BLOCKS.
!    SHORT (1-LINE) DEFINITIONS OF VARIABLES ARE ALSO MAINTAINED
!    IN THE DATA DICTIONARY AND INCLUDED IN OUTPUT FILES TO DOCUMENT DATA.
!    THE DICTIONARY FILE IS A SIMPLE FLAT FILE DESIGNED FOR MANUAL EDITING.
!
! -  FILER CAN READ AND WRITE PARTS OR ALL OF MULTI-DIMENSIONAL ARRAYS,
!    AS SPECIFIED BY USER OPTIONS.
!
! -  THE FILER SUBROUTINES HANDLE INPUT REQUESTS AND OUTPUT REQUESTS TO READ
!    DATA FROM FILES OR TO WRITE DATA TO FILES IN A GENERIC FORMAT.  THE
!    FORMAT IS DESIGNED SO THAT FILES FROM AN OUTPUT REQUEST CAN BE LOADED IN
!    FOR AN INPUT REQUEST.  HOWEVER, THE DEFAULT FORMAT IS TEXT READABLE AND
!    INCLUDES EACH VARIABLE'S NAME, COMMON BLOCK, DESCRIPTION, AND
!    ARRAY INDICES.  THE USER MAY SELECT ALTERNATIVE FORMATS.
!
! ARGUMENTS:
!  FRTYPE    INTEGER*4    FILER REQUEST TYPE: 1=OUTPUT(QUERY), 2=INPUT(UPDATE), 3=READ DICTIONARY
!  FSOURC    INTEGER*4    OPTION FOR INPUT SOURCE OF AN OUTPUT REQUEST (USED IF FRTYPE=1).
!                         FSOURC=1 CAUSES REQUESTS TO BE READ FROM A FILE.
!                         OTHERWISE, A SINGLE REQUEST, AS STORED IN THE
!                         FILERC COMMON BLOCK VARIABLE, FREQST, IS PROCESSED.
!  FUNITI    INTEGER*4    UNIT NUMBER TO USER FOR INPUT
!                         (USED IF FRTYPE=2 OR 3, OR IF FRTYPE=1 AND FSOURC=1)
!  FNAMEI    CHARACTER*100 FILE NAME (PC) OR DD NAME (OS/MVS) FOR INPUT
!                         (USED IF FRTYPE=2 OR 3, OR IF FRTYPE=1 AND FSOURC=1)
!                         IF FNAMEI IS BLANK, THEN INPUT IS INITIATED WITHOUT
!                         EXECUTION OF AN OPEN STATEMENT.  THIS ALLOWS USE WITH
!                         OS/MVS JCL WHERE DD STATEMENTS HAVE DEFAULT NAME.
!  FUNITO    INTEGER*4    UNIT NUMER TO USE FOR OUTPUT (IF FRTYPE=1)
!  FNAMEO    CHARACTER*100 FILE NAME (PC) OR DD NAME (OS/MVS) FOR OUTPUT.
!                         (USED FOR AN OUTPUT REQUEST, WHERE FRTYPE=1.
!                         IF FNAMEO BLANK, THEN OUTPUT IS INITIATED WITHOUT AN OPEN STATEMENT.
!  FRETCD    INTEGER*4    FILER RETURN CODE. 0=NO ERRORS OR PROBLEMS DETECTED
!                         FRETCD > 0 MEANS SOME ERROR OCCURED.
!  FUNFMT    INTEGER*4    FILER OPTION FOR UNFORMATTED INPUT OR OUTPUT FILE
!            FUNFMTS are:  0=FORMATTED (.txt)
!                          1=UNFORMATTED (.unf)
!                          2=PC UNFORMATTED
!                          3=WK1, not pertinent these days
!                          4=Comma delimited file (.csv)
!                          5=GDX file (.gdx)
!                          6=was AIMMS interface via System Development Kit, but this has been expunged
!                          7=AIMMS composite table format (.txt)
      IMPLICIT NONE

      INTEGER*4    FRTYPE,FSOURC,FUNITI,FUNITO,FRETCD,FUNFMT
      INTEGER*4    ICOUNT,IRET,IBLOCK,ICOM,IV,IVAR,ISTART,IEND,J, &
                   IPOINT,NSIZE,NELEM,NELEMC,K,IOPT,FLEN,yeardim
      CHARACTER*80 TRQST,PCSTR
      CHARACTER*100 FNAMEI,FNAMEO,fname
      integer y
      include'fdict'
      include'parametr'! need mnumyr
      external gdxCreate, gdxClose, gdxGetDLLVersion, gdxOpenRead, gdxOpenWrite, gdxOpenWriteEx
      logical gdxCreate, gdxClose
      integer(KIND=4) gdxGetDLLVersion, gdxOpenRead, gdxOpenWrite, gdxOpenWriteEx
      integer(KIND=4) ngrow
      real(KIND=4) fgrowfac
!  FREQST (FILER I/O REQUEST) CAN BE SET BY CALLING PROGRAM IN LIEU OF READING EACH FREQST FROM A FILE.
      CHARACTER*80   FREQST,RQST_buffer
      COMMON/FILERC/FREQST
! LOCAL VARIABLES
      CHARACTER QLABEL*(FMAXCHARCOM),VAR*(FMAXCHARVAR)
      CHARACTER*8    FORM*8,RORI*1
      CHARACTER*4    RTEMP,ITEMP
      INTEGER*4      DIM(5,2),AEND,LEND,LVAR,ORDER(6),ios,numreaderr/0/
      save numreaderr
      LOGICAL LDIM(5),LDIMYR(5) ! for testing of looping subsets

      real(kind=4), allocatable :: array(:)
      character(kind=1), allocatable :: tarray(:,:)

      CHARACTER      LABEL*8,FINDNM*8,DEFN*80
      CHARACTER*166 INLAB
      INTEGER NWK1ROW,NWK1COL
      COMMON/FWK1BLK/NWK1ROW,NWK1COL
      DATA LABEL/':'/
      COMMON/GDX/pgdx
      integer,parameter :: biggest_set=2001 ! biggest set + 1
      character*4 LabelArrayX(biggest_set)
      character*1 atemp(1)/'1'/
      integer dloc,dstart,dcopy,dend

      !GDX OUTPUT VARS
      integer                           i1,j1,k1,l1,m1,i2,j2
      integer*1                         ok2
      integer*8                         pgdx, rc, i, n,l,m,o
!      integer                           synr1, synr2, synr3, synr4, synr5, readDim, iaType, nrrecs, adim, iatyp
!      CHARACTER*32                      gdxElementNames(5)
!      CHARACTER*32                      ReadElementNames
!      real*4                            outMatrix(65536)
!      integer                           OutElemQty(5)
 !     real,allocatable::                ReturnMatrix(:,:,:,:,:),SourceMatrix(:,:,:,:,:)
 !     character*32,allocatable::        DimLabels(:,:)
      character*12                      varName, dimIn
      character*255                     dllstr
      logical                           ok
      character*100                     errMsg
      CHARACTER                         Afn
      CHARACTER                         AProducer
      INTEGER*4                         ErrNr
      INTEGER                           afdim, aelements(20)
      real*8                            avals(5)
      integer                           dim1,dim2,dim3,dim4,dim5,dimmax(5),flaglong,nlong,cntx
      character                         desc1, desc2, desc3, desc4, desc5
      integer                           CNT(FmaxDim)
      logical                           lopened
      character*3 dstr
      integer ls,ld,lf,s,slicepos,ll


      FORM='11F10.3'
  1   FORMAT(A8)
  2   FORMAT(A1)
  5   FORMAT(' *** ERROR READING DICTIONARY, LINE IS:',/,A165)
  6   FORMAT(A)
  7   FORMAT(1X,A6,6X,I2,5X,I2,4X,A8)
 75   FORMAT(41X,A1)
  8   FORMAT(A6,I2,I3,A8)
  9   FORMAT(9X,A1,3X,A6,3X,I1)
 10   FORMAT(27X,5(I2,1X),3X,5(A2,1X),2X,4(A2,3X),2X,A1,2(5X,A1))
 11   FORMAT(62X,4(A2,3X),2X,A1,2(5X,A1))
 14   FORMAT(I4)
! FRTYPE:  1=OUTPUT DATA,2=INPUT DATA,3=READ DICTIONARY

       CNT=0
      !if we are calling an output request in .gdx format
      IF(FUNFMT.EQ.5.AND.FRTYPE.EQ.1 .and. fnameo.ne. ' ') then
        !create the gdx file
        ok = gdxCreate(pgdx,errMsg)
        IF(.not.ok) then
           write(6,*) 'Error in gdxCreate: '//trim(errmsg)
           return
        ENDIF
        ok = gdxOpenWrite(pgdx,fnameo,'filer.f',ErrNr)
        if(.not.ok) then
          write(6,*) 'Error in gdxCreate: ',ErrNr
          stop
        endif
      ELSEIF(FUNFMT.EQ.5.AND.FRTYPE.EQ.2 .and. fnamei.ne.' ') then
       !else if we are calling a gdx read request
       ! gdxCreate and gdxOpenRead would normally be done by calling program. Otherwise,
       ! the calling program assigns a file name to fnamei. So if fnamei is set,
       ! open the gdx file here with a gdxCreate and a gdxOpenRead call:

       !Create a pointer to the .gdx object
        ok = gdxCreate(pgdx,errMsg)
        IF(.not.ok) then
           write(6,*) 'Error creating .GDX pointer: '//trim(errMsg)
           return
        ENDIF
        ok = gdxOpenRead(pgdx,fnamei,ErrNr)
        IF(.not.ok) then
          write(6,*) 'Error in gdxOpenRead: ',ErrNr
          return
        ENDIF
! use varlist file to control which variables are read from GDX file.
        inquire(unit=funito,opened=lopened)
        if(len_trim(var).gt.0 .and. .not. lopened) then
          open(funito,file=var,status='old')
        endif
      ENDIF

      IF (FRTYPE.NE.1.AND.FRTYPE.NE.2.AND.FRTYPE.NE.3) THEN
        WRITE(6,*) ' *** FRTYPE MUST BE 1 (OUTPUT REQUEST), 2 (INPUT), OR 3 (READ DICTIONARY)'
        WRITE(6,*) ' *** ROUTINE RETURNING EARLY'
        RETURN
      ENDIF

! OPEN FILES
      FRETCD=0
      CALL FFILE(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
      IF(FRTYPE.EQ.3) RETURN
      IF(FRETCD.NE.0) RETURN

      if(frtype.eq.2 .and. funfmt.eq.7) then
! get year slice off of file name with convention "path/prefix_yyyy_ii.txt"
        y=0; ld=0; ls=0; ll=0
        inquire(unit=funiti,name=fname) ! don't use fnamei because it may be passed as a string constant
        ld=index(fname,'_',.true.)-1       ! position of last "_" minus 1
        ls=index(fname(:ld),'_',.true.)+1  ! position of second-to-last "_" plus 1
        ll=index(fname,'\',.true.)         ! position of last "\" (end of directory part of name)
        if(ld .gt. ll .and. ls .gt. ll .and. (ld-ls+1) .eq. 4) then
          read(fname(ls:ld),'(i4)') y
          y=y-baseyr+1   ! baseyr is nems starting year, 1990
        endif
      endif

!     PROCESS EACH input or OUTPUT REQUEST.  191 IS THE MAJOR RETURN LOOP FROM BELOW
      ICOUNT=0
191 CONTINUE
      if(allocated(array)) then
        deallocate(array)
      endif
      if(allocated(tarray)) then
        deallocate(tarray)
      endif
      IF(FRTYPE.EQ.1) THEN
! IF AN OUTPUT REQUEST, GET SPECIFIC VARIABLE REQUEST FROM THE INPUT FILE, IF FSOURC=1.
! OTHERWISE, FREQST IS ASSUMED TO BE IN COMMON/FILERC/, AND ASSIGNED OUTSIDE THE PROGRAM
        IF(FSOURC.EQ.1) THEN
          READ(FUNITI,'(A)',END=1000) FREQST
          IF(FREQST(1:1).EQ.'*') GOTO 191
        ENDIF
        IF(ICOUNT.GE.1.AND.FSOURC.NE.1) RETURN
        ICOUNT=ICOUNT+1
      ELSE ! frtype=2, input request
        goto 192
888     continue
        numreaderr=numreaderr+1
        write(6,*) 'Error Reading Request Label, error number=',ios,', FREQST=',trim(FREQST)
        if(numreaderr.gt.250) goto 1000
!  IF INPUT REQUEST, THE REQUEST LINE SPECIFIES THE DATA TO BE UPDATED.
!  THE REQUEST LINE IS ASSUMED TO FOLLOW A LINE WITH "1" IN COLUMN 1
192     CONTINUE
        IF(FUNFMT.EQ.0) THEN
          READ(FUNITI,'(A)',END=1000) FREQST
          IF(FREQST(1:1).NE.'1')GOTO 192
          READ(FUNITI,'(A)',END=1000) FREQST
        ELSEIF(FUNFMT.EQ.5) THEN  ! for gdx file, use a varlist file to determine which variables read in
          READ(FUNITO,'(A)',END=1000) FREQST
          IF(FREQST(1:1).EQ.'*' .or. len_trim(freqst).eq.0) GOTO 192
          IF(INDEX(FREQST,'LABEL').EQ.0) GOTO 192
        elseif(funfmt.eq.7) then ! for aimms composite table data from "display" statements;
          READ(FUNITI,'(A)',END=1000) FREQST
          call FToUpper(freqst)

          IF(index(FREQST,'COMPOSITE TABLE:').eq.0 ) GOTO 192
          READ(FUNITI,'(A)',END=1000) FREQST
! line following "Composite Table:" has header with variable name on far right. The variable may have
! the domain specified within parenthesis (). if so, remove it
          ls=index(freqst,'(',.true.)
          if(ls.gt.0) then
            freqst(ls:)=' '
          endif

          l=len_trim(freqst)
          ls=index(freqst(1:l),' ',.true.)+1
          ld=index(freqst(ls:l),'_') ! get position of first "_" in name (separates common label and variable). assumes no common block label has a "_" in its name.
          if(ld.gt.0) then
            qlabel=freqst(ls:ls+ld-2)
            var=freqst(ls+ld:l)
! determine year slice and add slice range to freqst via ",Di=y/y", where i is the year dimension and y is the year index.
! COAL uses "globalyr" as the MNUMYR set element to indicate a year slice
            slicepos=0
            if(index(freqst,'globalyr').gt.1) then ! then this input is for a single-year slice of mnumyr
              if(freqst(1:1).ne.' ') slicepos=1
              do j=2,l
                if(freqst(j:j).ne.' '.and.freqst(j-1:j-1).eq.' ') slicepos=slicepos+1
                if(freqst(j:j+7).eq.'globalyr') exit
              enddo
            endif
            freqst=' LABEL='//trim(qlabel)//',VAR='//trim(var) ! set up FREQST in the format expected by "parseq"
            if(slicepos.gt.0.and. y.gt.0) then
              l=len_trim(freqst)
              s=2
              if(y.lt.10) s=1
              write(freqst(l+1:),'(a,i1,a,i<s>,a,i<s>)') ',D',slicepos,'=',y,'/',y
           endif
          else
            write(6,'(a)') 'ERROR reading composite table header. name has no dash to divide block label and var: '//trim(freqst)
            goto 192
          endif
        else
          READ(FUNITI,END=1000,err=888,iostat=ios) FREQST
          IF(INDEX(FREQST,'LABEL').EQ.0) GOTO 192

        ENDIF
      ENDIF
      !!if(funfmt.eq.7) then
      !!   write(6,'(1x,a)') trim(freqst)  ! debug
      !!endif
      IRET=0
      dstart=1
      dend=1
      dloc=0

      if(frtype.eq.1) dloc=index(freqst,'COPY5,D')+7


        CALL PARSEQ(FREQST,QLABEL,VAR,LDIM,DIM,FORM,ORDER,IRET)
!! DEBUG:
!       IF (FRTYPE.EQ.1) WRITE(6,'(2a)') "filer output: ",trim(FREQST)
!       IF (FRTYPE.EQ.2) WRITE(6,'(2a)') "filer input: ",trim(FREQST)
       IF(IRET.NE.0.AND.FSOURC.EQ.1) GOTO 191
! SEARCH FOR COMMON BLOCK LABEL IN DICTIONARY LIST OF COMMON BLOCKS. WHEN FOUND, BREAK
        IBLOCK=0
        DO 22 ICOM=1,FNBLOC
          IF(QLABEL.EQ.FBLOCK(ICOM))THEN
            IBLOCK=ICOM
            GOTO 23
          ENDIF
22      CONTINUE
! IF HERE, THE COMMON BLOCK WAS NOT FOUND. CHECK IF THE VARIABLE IS LISTED UNDER SOME OTHER COMMON BLOCK
        FINDNM=' '
        DO 221 IV=1,FNDICT
          IF(VAR.EQ.FVARNM(IV))THEN
            FINDNM=FBLKNM(IV)
            GOTO 71
          ENDIF
221     CONTINUE
        GOTO 71
23      CONTINUE
! IF HERE, COMMON BLOCK LABEL HAS BEEN FOUND IN DICTIONARY. SEARCH THE
! VARIABLES IN THIS COMMON BLOCK TO FIND THE VARIABLE
        IVAR=0
        ISTART=FINDEX(IBLOCK)
        IEND=FINDEX(IBLOCK)+FNVARS(IBLOCK)-1
        DO 25 IV=ISTART,IEND
          IF(VAR.EQ.FVARNM(IV).OR.VAR.EQ.'ALL')THEN
            IVAR=IV
            GOTO 27
          ENDIF
25      CONTINUE
! IF HERE, THE VARIABLE WAS NOT FOUND IN THE DICTIONARY UNDER THE DESIGNATED COMMON BLOCK.
! CHECK IF THE VARIABLE IS LISTED UNDER SOME OTHER COMMON BLOCK
        FINDNM=' '
        DO 251 IV=1,FNDICT
          IF(VAR.EQ.FVARNM(IV))THEN
            FINDNM=FBLKNM(IV)
            GOTO 35
          ENDIF
251     CONTINUE
        GOTO 35
27      CONTINUE
! IF HERE, BOTH THE COMMON BLOCK AND VARIABLE HAVE BEEN FOUND.
!   IF VAR='ALL', LOOP THROUGH ALL THE VARIABLES IN THE COMMON BLOCK.
!   OTHERWISE, SET LOOP RANGE FOR THE SINGLE VARIABLE.
        IF(VAR.NE.'ALL') THEN
          ISTART=IVAR
          IEND=IVAR
        ENDIF
        DO 30 IV=ISTART,IEND  ! loop through all variables in common block when "var=all" or just one variable otherwise
          IVAR = IV
          VAR = FVARNM(IV)
!! debug          IF (FRTYPE.EQ.1) WRITE(6,'(2a)') "filer output:   ",trim(var)

          ! LAST CHECK ON ORDER= SPECIFICATION.
! CHECK NUMBER OF DIMENSIONS SPECIFIED IN REQUEST
          IF(ORDER(6).NE.0 .AND. ORDER(6).NE.FNDIMS(IVAR)) THEN
            WRITE(6,*) ' *** WRONG NUMBER OF DIMENSIONS SPECIFIED IN ORDER= CLAUSE, ORDER REQUEST IGNORED'
            WRITE(6,*) ' *** REQUEST = ' // FREQST
            DO J = 1,5
              ORDER(J) = J
            END DO
            ORDER(6) = 0
          ENDIF
! SET BYTE POINTER AND ARRAY SUBSCRIPTS IF NOT SET BY USER
!*****  FOR NOW, SET IPOINT AS A 4-BYTE WORD POINTER USE THE BYTE LOCATION.
!*****  MAY HAVE TO REVISE TO ACCOMODATE DIFFERENT SIZE VARIABLES.
          IPOINT=((FBPOIN(IVAR)-1)/4)+1
          NSIZE=FNDIMS(IVAR)
          NELEM=FVARSZ(IVAR)/4
          if(fvarln(ivar).gt.4)then
            NELEM=FVARSZ(IVAR)/FVARLN(IVAR)
          endif
          RORI=FVARTY(IVAR)
          DEFN=FDESCR(IVAR)
          ldimyr(1:5)=ldim(1:5)  ! make copy because if var=all, this will be repeated and redone for each variable

          yeardim=0  ! determine what k represents the year dimension
          do k=1,5
            if(fkdim(k,ivar).eq.mnumyr) yeardim=k  ! looking for the dimension that represents years
          enddo
! if option to write a subset of yearly data is in effect, set indices for the year dimension to
! match the start and end year range.

          if(yeardim.gt.0.and.FYearSubset.eq.1.and. FRTYPE.eq.1 ) then  ! output request only
            dim(yeardim,1)=FYearStart
            dim(yeardim,2)=FYearEnd
            ldimyr(yeardim)=.true.
          endif

          do K=1,5
            IF(.NOT.LDIMYR(K)) THEN
              DIM(K,1)=1
              DIM(K,2)=FKDIM(K,IVAR)
            ENDIF
          enddo
 ! Allocate a generic array to hold each array processed.
          if(allocated(array))then
            deallocate(array)
          endif
          if(FVARTY(IVAR).ne.'C') then
            allocate (array(nelem+10))
          else
            allocate (array(1))
          endif
          array=0.  ! fill with zeros as a precaution
! allocate character tarray
          if(allocated(tarray))then
            deallocate(tarray)
          endif
          if(FVARTY(IVAR).eq.'C') then
            allocate (tarray(FVARLN(IVAR),nelem+10))
            nelemc=nelem
            nelem=1
          else
            allocate (tarray(FVARLN(IVAR),1))
            nelemc=1
          endif
          tarray=' '

! Now initialize the generic array by copying the common block variable to it.  This
! is for both input and output requests.  If the input request is only a partial array,
! or slice, the array's full initialization provide any parts not read from the file.
! COPY RELEVANT PORTION OF THE COMMON BLOCK TO THE GENERIC ARRAY

          IOPT=1
          IRET=0
          CALL FCOPA(QLABEL,ARRAY,NELEM,TARRAY,NELEMC,FVARLN(IVAR),IPOINT,IOPT,IRET)
          IF (IRET.EQ.1) GOTO 81

          IF (FRTYPE.EQ.1) THEN   ! If Output Request, then....

! IF 'VAR=ALL' OPTION IS BEING USED TO GENERATE OUTPUT REQUEST, THEN FILL IN THE VARIABLE NAME
            RQST_BUFFER = FREQST
            DO K = 1,72
              IF (RQST_BUFFER(K:K+6) .EQ. 'VAR=ALL' ) THEN
                LVAR=len_trim(VAR)
                AEND=K+4+3   -1
                LEND=K+4+LVAR-1
                RQST_BUFFER(K:LEND) = 'VAR='//VAR(:LVAR)
                RQST_BUFFER(LEND+1:80)= &
                FREQST(AEND+1:80-MAX(LEND-AEND,0))
              ENDIF
            ENDDO
            dloc=index(rqst_buffer,'COPY5,D')+7
            if(dloc.gt.7) then                               ! only true if ",copy5,D" found
              do k=1,5
                if(k.eq.yeardim) then  ! looking for the dimension that represents years
                  write(rqst_buffer(dloc:),'(i1,a)') k,'=52/61'  ! makes copy of data
                  ldimyr(k)=.true.
                  dim(k,1)=52  ! copy or grow values from year 51 to 52:61
                  dim(k,2)=61  !
                  fgrowfac=1.02      ! growth factor for extrapolation. use 1.0 for copy
                else
                  ldimyr(k)=.false.
                endif
              enddo
              if (FUNFMT.NE.5 .and. FUNFMT .ne.6 ) THEN
                if (rori.eq.'R'.and.yeardim.ne.0) then
                   write(6,*) trim(rqst_buffer)
                   write(6,'(5i5,5(i4,a,i4),i5,f6.3)' ) (fkdim(k,ivar),K=1,5),(dim(k,1),':',dim(k,2),k=1,5),yeardim,fgrowfac
                   CALL fgrowit(ARRAY,FKDIM(1,IVAR),FKDIM(2,IVAR),FKDIM(3,IVAR),FKDIM(4,IVAR),FKDIM(5,IVAR), &
                   DIM, YEARDIM, fgrowfac)
        ! copy array back so it will be updated for next pass/year
                   IOPT=2
                   IRET=0
                   CALL FCOPA(QLABEL,ARRAY,NELEM,TARRAY,NELEMC,FVARLN(IVAR),IPOINT,IOPT,IRET)
                   IF (IRET.EQ.1) GOTO 81

                endif
              endif
            endif
            if (FYearSubset.eq.1) then
! Check for pre-existing dimension subset string on request.  If found, remove
              write(dstr,'(a,i1,a)') 'D',yeardim,'='
              k=index(rqst_buffer,dstr)
              if(k.gt.0) then
                L=index(rqst_buffer(k:),',')
                if(L.gt.0) then
                  rqst_buffer(k:)=rqst_buffer(k+L:)
                else
                  rqst_buffer(k:)=' '
                endif
              endif
! get rid of extra comma at end or in middle
              k=len_trim(rqst_buffer)
              if (rqst_buffer(k:k).eq.',') rqst_buffer(k:k)=' '
              k=index(rqst_buffer,',,')
              if (k.gt.0) rqst_buffer=rqst_buffer(1:k)//rqst_buffer(k+1:)
! insert dimension subset string (like, D2=15/20)

              k=len_trim(rqst_buffer)+1
              if (k.le.len(rqst_buffer)-6) then
                rqst_buffer(k:k+1)=',D'
                k=k+2
                write(rqst_buffer(k:k),'(i1)') yeardim
                k=k+1
                rqst_buffer(k:k)='='
                k=k+1
                if (FYearStart.le.9) then
                  write(rqst_buffer(k:k),'(i1)') FYearStart
                  k=k+1
                else
                  write(rqst_buffer(k:k+1),'(i2)') FYearStart
                  k=k+2
                endif
                if(k.le.len(rqst_buffer)-3) then
                  if (FYearEnd.gt.FYearStart) then
                    rqst_buffer(k:k)='/'
                    k=k+1
                    if (FYearEnd.le.9) then
                      write(rqst_buffer(k:k),'(i1)') FYearEnd
                      k=k+1
                    else
                      write(rqst_buffer(k:k+1),'(i2)') FYearEnd
                      k=k+2
                    endif
                  endif
                else
                  write(6,*)' Not enough room for FYearEnd,var='//trim(var)
                endif
              else
                write(6,*)' Not enough room for FYearStart,var='//trim(var)
              endif
            endif

            IF (FUNFMT.EQ.0) THEN
              WRITE(FUNITO,592) RQST_BUFFER
!             write(6,*) trim(rqst_buffer)
592           FORMAT(' '/'1 FILER INPUT/OUTPUT'/1X,A)
            ELSEIF (FUNFMT.EQ.1) THEN
              WRITE(FUNITO) RQST_BUFFER
            ELSEIF (FUNFMT.EQ.2) THEN
              TRQST=PCSTR(RQST_BUFFER)
              WRITE(FUNITO) TRQST
            ELSEIF (FUNFMT.EQ.3) THEN
              INLAB=RQST_BUFFER
              NWK1ROW=NWK1ROW+1
              CALL WKSTR(FUNITO,NWK1ROW,1,INLAB,1,1,1)
            ELSEIF (FUNFMT.EQ.4) THEN
               INLAB=RQST_BUFFER
               NWK1ROW=NWK1ROW+1
               CALL WCKSTR(FUNITO,NWK1ROW,1,INLAB,1,1,1)
            ENDIF
            if (FUNFMT.NE.5) THEN
              CALL WRITQ(IBLOCK,IVAR,ARRAY,ARRAY,TARRAY,FVARLN(IVAR),ORDER, &
                FKDIM(1,IVAR),FKDIM(2,IVAR),FKDIM(3,IVAR),FKDIM(4,IVAR), &
                FKDIM(5,IVAR),NSIZE,DIM,VAR,FORM,FUNITO,RORI,DEFN,FUNFMT)
            ELSEIF (FUNFMT.EQ.5 .and. dloc.le.7) THEN
           !   WRITE(6,*) 'WRITING .GDX MATRIX: ',TRIM(QLABEL),'_',TRIM(VAR)

              Call FFill_Strs(QLABEL,VAR,IVAR)

              flaglong=0
              nlong=0

              do i2=1,5
                dimmax(i2)=fkdim(i2,ivar)
!               Write(6,3344)var, i2,fkdim(i2,ivar),fndims(ivar),fdimstr(1,i2),Fvardim(i2)
3344 format(A25,1x,I4,1x,I4,1x,I4,2(1x,A25))
                if(dimmax(i2).gt.FMaxDims) then
                  dimmax(i2)=FMaxDims
                  flaglong=i2
                  nlong=fkdim(i2,ivar)
                  if(nlong.le.999) then
                    call FFillLong(LabelArrayX,nlong,3)
                  else
                    if(nlong.gt.biggest_set-1) then
                      write(6,'(a)')' error. size of dimension in '//var//' of ',nlong,' exceeds "biggest_set-1" in filer.f'
                      write(6,'(a)')' increase biggest_set to nlong+1'
                      stop ' '
                    endif
                    call FFillLong(LabelArrayX,nlong,4)
                  endif
                endif
              enddo
!         Scalar
              IF(Fndims(Ivar).eq.1 .and. FDimStr(1,1) .eq. '') THEN
                FDimStr(1,1) = '1'
              ENDIF

!Set definitions - can only be written to the gdx once

              DO J=1,Fndims(ivar)
                IF (fkdim(j,ivar).GT.1) THEN
199               Do I=1,FMaxDim
! check for successive duplicates once caused by a now-corrected error in reading dimension strings.
                    IF (Fvardim(J) .EQ. FdimNam(I) .AND. CNT(I).EQ.0 ) THEN
                      CNT(I) = CNT(I)+1
                      if(I.gt.1) then
                        IF (FDimNam(I) .EQ. FDimNam(I-1)) THEN
                          CNT(I) = CNT(I)+1
                          GO To 199
                        ENDIF
                      endif
                      if(flaglong.ne.J) then
                        call WriteGDXElement_domains(pgdx,0,FdimNam(I),FDimDescrip(I),1,FDimSiz(I),1,1,1,1,fdimstr(1,j),atemp,atemp,atemp,atemp, RORI   ,FVarDim)
                      else
                        call WriteGDXElement_domains(pgdx,0,FdimNam(I),FDimDescrip(I),1,FDimSiz(I),1,1,1,1,labelarrayx(1),atemp,atemp,atemp,atemp, RORI ,FVarDim)
                      endif

                    ENDIF
                  ENDDO
                ENDIF
              ENDDO

! replace $ in var name to _, as this character causes a problem in the writGDXelement routine
              do j2=1,len_trim(var)
                if(var(j2:j2).eq.'$') then
                  var(j2:j2)='_'
                endif
              enddo
!Write out VARs to gdx
             if(RORI.NE.'C') then
              if(flaglong.eq.0) then
                call WriteGDXElement_domains(pgdx,1,TRIM(QLABEL)//'_'//TRIM(VAR),DEFN,ARRAY,FKDIM(1,IVAR),FKDIM(2,IVAR),FKDIM(3,IVAR),FKDIM(4,IVAR),FKDIM(5,IVAR), &
                   FDimStr(1,1), &
                   FDimStr(1,2), &
                   FDimStr(1,3), &
                   FDimStr(1,4), &
                   FDimStr(1,5), RORI ,FVarDim)
              elseif(flaglong.eq.1) then
                call WriteGDXElement_domains(pgdx,1,TRIM(QLABEL)//'_'//TRIM(VAR),DEFN,ARRAY,   nlong,     FKDIM(2,IVAR),FKDIM(3,IVAR),FKDIM(4,IVAR),FKDIM(5,IVAR), &
                   LabelArrayX, &
                   FDimStr(1,2), &
                   FDimStr(1,3), &
                   FDimStr(1,4), &
                   FDimStr(1,5), RORI ,FVarDim )
              elseif(flaglong.eq.2) then
                call WriteGDXElement_domains(pgdx,1,TRIM(QLABEL)//'_'//TRIM(VAR),DEFN,ARRAY,FKDIM(1,IVAR),   nlong,     FKDIM(3,IVAR),FKDIM(4,IVAR),FKDIM(5,IVAR), &
                   FDimStr(1,1), &
                   LabelArrayX, &
                   FDimStr(1,3), &
                   FDimStr(1,4), &
                   FDimStr(1,5), RORI ,FVarDim )
              elseif(flaglong.eq.3) then
                call WriteGDXElement_domains(pgdx,1,TRIM(QLABEL)//'_'//TRIM(VAR),DEFN,ARRAY,FKDIM(1,IVAR),FKDIM(2,IVAR),   nlong,     FKDIM(4,IVAR),FKDIM(5,IVAR), &
                   FDimStr(1,1), &
                   FDimStr(1,2), &
                   LabelArrayX, &
                   FDimStr(1,4), &
                   FDimStr(1,5), RORI ,FVarDim)
              elseif(flaglong.eq.4) then
                call WriteGDXElement_domains(pgdx,1,TRIM(QLABEL)//'_'//TRIM(VAR),DEFN,ARRAY,FKDIM(1,IVAR),FKDIM(2,IVAR),FKDIM(3,IVAR),  nlong,      FKDIM(5,IVAR), &
                   FDimStr(1,1), &
                   FDimStr(1,2), &
                   FDimStr(1,3), &
                   LabelArrayX, &
                   FDimStr(1,5), RORI ,FVarDim)
              elseif(flaglong.eq.5) then
                call WriteGDXElement_domains(pgdx,1,TRIM(QLABEL)//'_'//TRIM(VAR),DEFN,ARRAY,FKDIM(1,IVAR),FKDIM(2,IVAR),FKDIM(3,IVAR),FKDIM(4,IVAR),  nlong,       &
                   FDimStr(1,1), &
                   FDimStr(1,2), &
                   FDimStr(1,3), &
                   FDimStr(1,4), &
                   LabelArrayX, RORI ,FVarDim)
              endif
             endif
            endif
          ELSE  ! frtype=2, input request
            IF (FUNFMT.NE.5) THEN
              CALL READQ(IVAR,ARRAY,ARRAY,TARRAY,FVARLN(IVAR),ORDER, &
                FKDIM(1,IVAR),FKDIM(2,IVAR),FKDIM(3,IVAR),FKDIM(4,IVAR), &
                FKDIM(5,IVAR),DIM,FUNITI,FORM,RORI,FUNFMT,IRET,FREQST)
              IF(IRET.EQ.1) GOTO 191
              IF(IRET.EQ.2) GOTO 999
! COPY GENERIC ARRAY TO THE APPROPRIATE PART OF THE COMMON BLOCK
              IOPT=2
              CALL FCOPA(QLABEL,ARRAY,NELEM,TARRAY,NELEMC,FVARLN(IVAR),IPOINT,IOPT,IRET)
            ELSEIF(FUNFMT.EQ.5.AND.FRTYPE.EQ.2) THEN
! IF 'VAR=ALL' OPTION IS BEING USED TO GENERATE INPUT REQUEST, THEN FILL IN THE VARIABLE NAME.
! It creates the a variable like freqst changing "var=all" to "var=[specific variable name]"
              RQST_BUFFER = FREQST
              DO K = 1,72
                IF (RQST_BUFFER(K:K+6) .EQ. 'VAR=ALL' ) THEN
                  LVAR=len_trim(VAR)
                  AEND=K+4+3   -1
                  LEND=K+4+LVAR-1
                  RQST_BUFFER(K:LEND) = 'VAR='//VAR(:LVAR)
                  RQST_BUFFER(LEND+1:80)=FREQST(AEND+1:80-MAX(LEND-AEND,0))
                ENDIF
              ENDDO
!             write(6,'(3x,a)') trim( RQST_BUFFER)  ! debug

              Call FFill_Strs(QLABEL,VAR,IVAR)
              flaglong=0
              nlong=0
!            write(6,*)' '
              do i2=1,5
                dimmax(i2)=fkdim(i2,ivar)
!               Write(6,3355)var, i2,fkdim(i2,ivar),fndims(ivar),fdimstr(1,i2),Fvardim(i2)
3355 format(A25,1x,I4,1x,I4,1x,I4,2(1x,A25))
                if(dimmax(i2).gt.FMaxDims) then
                  dimmax(i2)=FMaxDims
                  flaglong=i2
                  nlong=fkdim(i2,ivar)
                  if (nlong.le.999) then
                    call FFillLong(LabelArrayX,nlong,3)
                  else
                    call FFillLong(LabelArrayX,nlong,4)
                  endif
                endif
              enddo
!         Scalar
              IF (Fndims(Ivar).eq.1 .and. FDimStr(1,1) .eq. '') THEN
                FDimStr(1,1) = '1'
              ENDIF
! replace $ in var name to _, as this character causes a problem in the writGDXelement routine
              do j2=1,len_trim(var)
                if (var(j2:j2).eq.'$') then
                  var(j2:j2)='_'
                endif
              enddo
!read VARs from gdx
             if (RORI.NE.'C') then
              if (flaglong.eq.0) then
                call ReadGDXElement(pgdx,1,TRIM(QLABEL)//'_'//TRIM(VAR),DEFN,ARRAY,FKDIM(1,IVAR),FKDIM(2,IVAR),FKDIM(3,IVAR),FKDIM(4,IVAR),FKDIM(5,IVAR), &
                   FDimStr(1,1), &
                   FDimStr(1,2), &
                   FDimStr(1,3), &
                   FDimStr(1,4), &
                   FDimStr(1,5), RorI )
              elseif (flaglong.eq.1) then
                call ReadGDXElement(pgdx,1,TRIM(QLABEL)//'_'//TRIM(VAR),DEFN,ARRAY,   nlong,     FKDIM(2,IVAR),FKDIM(3,IVAR),FKDIM(4,IVAR),FKDIM(5,IVAR), &
                   LabelArrayX, &
                   FDimStr(1,2), &
                   FDimStr(1,3), &
                   FDimStr(1,4), &
                   FDimStr(1,5), RorI )
              elseif (flaglong.eq.2) then
                call ReadGDXElement(pgdx,1,TRIM(QLABEL)//'_'//TRIM(VAR),DEFN,ARRAY,FKDIM(1,IVAR),   nlong,     FKDIM(3,IVAR),FKDIM(4,IVAR),FKDIM(5,IVAR), &
                   FDimStr(1,1), &
                   LabelArrayX, &
                   FDimStr(1,3), &
                   FDimStr(1,4), &
                   FDimStr(1,5), RorI )
              elseif (flaglong.eq.3) then
                call ReadGDXElement(pgdx,1,TRIM(QLABEL)//'_'//TRIM(VAR),DEFN,ARRAY,FKDIM(1,IVAR),FKDIM(2,IVAR),   nlong,     FKDIM(4,IVAR),FKDIM(5,IVAR), &
                   FDimStr(1,1), &
                   FDimStr(1,2), &
                   LabelArrayX, &
                   FDimStr(1,4), &
                   FDimStr(1,5), RORI  )
              elseif (flaglong.eq.4) then
                call ReadGDXElement(pgdx,1,TRIM(QLABEL)//'_'//TRIM(VAR),DEFN,ARRAY,FKDIM(1,IVAR),FKDIM(2,IVAR),FKDIM(3,IVAR),  nlong,      FKDIM(5,IVAR), &
                   FDimStr(1,1), &
                   FDimStr(1,2), &
                   FDimStr(1,3), &
                   LabelArrayX, &
                   FDimStr(1,5), RORI  )
              elseif (flaglong.eq.5) then
                call ReadGDXElement(pgdx,1,TRIM(QLABEL)//'_'//TRIM(VAR),DEFN,ARRAY,FKDIM(1,IVAR),FKDIM(2,IVAR),FKDIM(3,IVAR),FKDIM(4,IVAR),  nlong,       &
                   FDimStr(1,1), &
                   FDimStr(1,2), &
                   FDimStr(1,3), &
                   FDimStr(1,4), &
                   LabelArrayX, RORI  )
              endif

              IOPT=2
              CALL FCOPA(QLABEL,ARRAY,NELEM,TARRAY,NELEMC,FVARLN(IVAR),IPOINT,IOPT,IRET)
             endif
            ENDIF
          ENDIF
30      CONTINUE

GO TO 191

!  IF HERE, THE VARIABLE WAS NOT FOUND IN THE DICTIONARY UNDER THE COMMON BLOCK LABEL DESIGNATED.
!  IF FINDNM IS NONBLANK, IT HOLDS ANOTHER COMMON BLOCK NAME IN THE DICTIONARY CONTAINING THE VARIABLE
 35   WRITE(6,351)' *** VARIABLE ',VAR,' NOT FOUND IN THE DICTIONARY UNDER COMMON BLOCK ', QLABEL
      IF (FINDNM.NE.'        ') THEN
        WRITE(6,351)' *** BUT IT IS LISTED UNDER COMMON ', FINDNM
      ELSE
        WRITE(6,351)' *** AND IS NOT LISTED ELSEWHERE.'
      ENDIF
351   FORMAT(5A)
      GOTO 191

 71   WRITE(6,85) QLABEL,FREQST(:78)
 85   FORMAT(' ***  ERROR:  THE COMMON BLOCK ',A, ' IS NOT IN THE DICTIONARY FOR THIS REQUEST:'/ 1X,A)
      IF (FINDNM.NE.'       ') THEN
        WRITE(6,351)' *** BUT VARIABLE '//VAR//' IS LISTED UNDER COMMON BLOCK ', FINDNM
      ELSE
        WRITE(6,351)' *** THE VARIABLE '//VAR//' IS NOT LISTED EITHER'
      ENDIF
      GOTO 191

! ERROR FROM FCOPA.
81    WRITE(6,86) QLABEL,FREQST(:78)
86    FORMAT(' *** ERROR:  THE COMMON BLOCK ',A,' IS IN THE DICTIONARY,'/ &
       ' BUT IT IS NOT CODED IN SUBROUTINE FCOPA FOR THIS REQUEST: '/ 1X,A)
      GOTO 191

!  ERROR EXIT FROM READQ. IRET=2, EOF READING DATA TO FILL THE VARIABLE,
!  SO UNABLE TO COMPLETE THE REQUEST. (ERR MSG IN READQ)
999   CONTINUE

      FRETCD=1
      if (allocated(array))then
        deallocate(array)
      endif
      if (allocated(tarray)) then
        deallocate(tarray)
      endif

      RETURN

! END OF FILE ON EITHER READING EITHER THE INPUT REQUEST FILE OR THE OUTPUT REQUEST FILE
1000  CONTINUE

      FRETCD=0
      IF ((FRTYPE.EQ.1).AND.(FUNFMT.EQ.2)) CLOSE(UNIT=FUNITO)
!     IF (FRTYPE.EQ.1.AND.FUNFMT.EQ.3) CALL WKCLOSE(FUNITO)

      !close .GDX file if funfmt = 4 and frtype = 1
      IF(FUNFMT.EQ.5.AND.FRTYPE.EQ.1) THEN
              WRITE(6,*) 'CLOSING .GDX FILE...'
              ok2 = gdxClose(pgdx)
      ELSEIF(FUNFMT.EQ.5.AND.FRTYPE.EQ.2) THEN
        WRITE(6,*) 'CLOSING .GDX File...'
        ok2 = gdxClose(pgdx)
      END IF
      if(allocated(array))then
        deallocate(array)
      endif
      if(allocated(tarray)) then
        deallocate(tarray)
      endif

      RETURN
      END
!***********************************************************************
      SUBROUTINE FFILE(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
! THIS OPENS INPUT AND OUTPUT FILES IF THEY ARE NAMED.  CALLS FDICT TO READ DICTIONARY IF FRTYPE=3.
! IF UNFORMATTED OPTION IN EFFECT (FUNFMT), THEN INPUT REQUESTS ARE READ FROM UNFORMATTED FILE,
! AND OUTPUT REQUESTS ARE WRITTEN TO UNFORMATTED FILES.


      IMPLICIT NONE
      integer irecl
      character*1 nul
      parameter (irecl=400)
      character*irecl longline
      logical lexist

      INTEGER*4 FRTYPE,FSOURC,FUNITI,FUNITO,FRETCD,FUNFMT
      CHARACTER*100 FNAMEI,FNAMEO,BLNKFILE*50
      CHARACTER*6 CODE

      IF (FRTYPE.EQ.3) THEN
! OPEN FILE AND READ DICTIONARY FROM FUNITI. WRITE SUMMARY TO
! DESIGNATED OUTPUT FILE OR SYSTEM OUTPUT
        IF (FNAMEI.NE.' ') OPEN(UNIT=FUNITI,FILE=FNAMEI,STATUS='OLD',READONLY)
        IF (FNAMEO.NE.' ') OPEN(UNIT=FUNITO,FILE=FNAMEO,STATUS='UNKNOWN')
        CALL FDICT(FUNITI,FUNITO)
!CLOSE        CLOSE(UNIT=FUNITI)
!CLOSE        IF(FUNITO.NE.6)CLOSE(UNIT=FUNITO)
        FRETCD=0
        RETURN
      ELSEIF(FRTYPE.EQ.1)THEN
! IF FSOURC IS 1 (INDICATING FILE INPUT OF THE OUTPUT REQUEST SPECS) OPEN FILE
        IF (FSOURC.EQ.1.AND.FNAMEI.NE.' ') &
           OPEN(UNIT=FUNITI,FILE=FNAMEI,STATUS='OLD',READONLY)
!  OPEN OUTPUT FILE (IF IT IS NAMED)
        IF (FUNFMT.EQ.0) THEN
          IF (FNAMEO.NE.' ') OPEN(UNIT=FUNITO,FILE=FNAMEO,STATUS='UNKNOWN')
        ELSEIF (FUNFMT.GE.1.AND.FUNFMT.LE.4) THEN
          IF (FNAMEO.NE.' ') THEN
            IF (FUNFMT.EQ.3)THEN    ! WK1
              OPEN(UNIT=FUNITO,FILE=FNAMEO,FORM='UNFORMATTED',ACCESS='SEQUENTIAL',STATUS='UNKNOWN')
            ELSEIF (FUNFMT.EQ.4) THEN ! comma delimited (CSV)
              inquire(file=fnameo,exist=lexist)
              if (lexist) then
                OPEN(UNIT=funito,FILE=fnameo,FORM='FORMATTED',ACCESS='DIRECT',recl=irecl,err=1000)
                close(funito,status='delete',err=1000)
1000            continue
              endif
              OPEN(UNIT=funito,FILE=fnameo,FORM='FORMATTED',ACCESS='DIRECT',status='new',recl=irecl)
            ELSE
              OPEN(UNIT=FUNITO,FILE=FNAMEO,FORM='UNFORMATTED', &
              ACCESS='SEQUENTIAL',STATUS='UNKNOWN',CONVERT='BIG_ENDIAN')
            ENDIF
          ENDIF
! IF WK1 FORMAT, WRITE HEADER CODE
          IF (FUNFMT.EQ.3) THEN
            CODE(1:6)=CHAR(0)//CHAR(0)//CHAR(2)//CHAR(0)//CHAR(4)//CHAR(4)
          ENDIF
        ENDIF
      ELSEIF (FRTYPE.EQ.2) THEN
        IF (FUNFMT.EQ.0) THEN
          IF (FNAMEI.NE.' ') OPEN(UNIT=FUNITI,FILE=FNAMEI,STATUS='OLD',READONLY)
        ELSEIF (FUNFMT.ne.5.and.funfmt.ne.6) then
          IF (FNAMEI.NE.' ') &
            OPEN(UNIT=FUNITI,FILE=FNAMEI,STATUS='OLD', READONLY, &
            FORM='UNFORMATTED',ACCESS='SEQUENTIAL',CONVERT='BIG_ENDIAN')
        ENDIF
      ENDIF
      RETURN
      END
!***********************************************************************
      SUBROUTINE PARSEQ(Q,LABEL,VAR,LDIM,DIM,FORM,ORDER,IRET)
! READS AN INPUT OR OUTPUT REQUEST LINE AND PARSES INFORMATION INTO
! THE LABEL, VARIABLE NAME, OPTIONAL SUBSCRIPT RANGES, AND OUTPUT FORMAT
!

      IMPLICIT NONE

      CHARACTER*(*) LABEL,VAR,Q
      CHARACTER TEMP*4,TEST*5,FORM*8
      LOGICAL LDIM(5)
      INTEGER*4 DIM(5,2),LCOM,LVAR,I,J,IS,IPLACE,IRET

      INTEGER*4 RECLEN,ORDER(6)

      RECLEN = 80
      LCOM=LEN(LABEL)
      LVAR=LEN(VAR)
! SET DEFAULTS: ORDER=(1,2,3,4,5) AND NO DIMENSION SUBSETS
      DO 10 J=1,5
        ORDER(J) = J
        LDIM(J)=.FALSE.
        DO 10 I=1,2
          DIM(J,I)=0
10    CONTINUE
! GET COMMON BLOCK LABEL NAME (LABEL=)
      DO 20 I=1,RECLEN-6
        IS=I+5
        IF(Q(I:IS).EQ.'LABEL=') THEN
          IS=IS+1
          CALL FINDCS(Q,RECLEN-6,IS,IPLACE)
          IF(IPLACE-IS.GT.(LCOM-1)) IPLACE=IS+(LCOM-1)
          LABEL=Q(IS:IPLACE)
          GOTO 21
        ENDIF
20    CONTINUE
      WRITE(6,100) ' *** I/O REQUEST MISSING THE COMMON LABEL (LABEL=):'
      WRITE(6,100) Q
      IRET=1
      RETURN
21    CONTINUE

!  GET VARIABLE NAME (VAR=)
      DO 30 I=1,RECLEN-4
        IS=I+3
        IF(Q(I:IS).EQ.'VAR=') THEN
          IS=IS+1
          CALL FINDCS(Q,RECLEN-4,IS,IPLACE)
          IF((IPLACE-IS).GT.(LVAR-1)) IPLACE=IS+(LVAR-1)
          VAR=Q(IS:IPLACE)
          GOTO 31
        ENDIF
30    CONTINUE
      WRITE(6,100) ' '
      WRITE(6,100) ' *** I/O REQUEST MISSING THE VARIABLE (VAR=):'
      WRITE(6,100) Q
      IRET=1
      RETURN
31    CONTINUE

!  GET OPTIONAL DIMENSION RANGES (D1=X{/X}), D2=X{/X}, ETC)
      TEMP='    '
      TEST='   '
      DO 70 J=1,5
        WRITE(TEST,'(A1,I1,A1)') 'D',J,'='
        DO 60 I=1,RECLEN-3
          IS=I+2
          IF(Q(I:IS).EQ.TEST) THEN
            IS=IS+1
            CALL FINDCS(Q,RECLEN-1,IS,IPLACE)
            IF((IPLACE-IS).GT.3) IPLACE=IS+3
            TEMP=Q(IS:IPLACE)
            READ(TEMP,'(BN,I4)',ERR=61) DIM(J,1)
            LDIM(J)=.TRUE.
            GOTO 62
          ENDIF
60      CONTINUE
        GOTO 70
62      CONTINUE
        IS=IPLACE+1
        IF(Q(IS:IS).NE.'/') THEN
          DIM(J,2)=DIM(J,1)
          GOTO 69
        ENDIF
        TEMP=' '
        IS=IS+1
        CALL FINDCS(Q,RECLEN-1,IS,IPLACE)
        IF((IPLACE-IS).GT.3) IPLACE=IS+3
        TEMP=Q(IS:IPLACE)
        READ(TEMP,'(BN,I4)',ERR=61) DIM(J,2)
        IF(DIM(J,2).LT.DIM(J,1)) GOTO 61
        GOTO 69
61      CONTINUE
        WRITE(6,100) '*** I/O REQUEST HAS INVALID DIMENSIONS:'
        WRITE(6,100) ' ',Q
        IRET=1
        RETURN
69      CONTINUE
70    CONTINUE
!  OPTIONAL ROW FORMAT (FORMAT=(RFW.D) OR (REW.D)
      FORM=' '
      DO 80 I=1,RECLEN-7
        IS=I+6
        IF (Q(I:IS).EQ.'FORMAT=') THEN
          IS=IS+1
          CALL FINDCS(Q,RECLEN-1,IS,IPLACE)
          IF (IPLACE-IS.GT.7) IPLACE=IS+7
          FORM=Q(IS:IPLACE)
          GOTO 81
        ENDIF
80    CONTINUE
81    CONTINUE
100   FORMAT(1X,A)
! GET OPTIONAL DIMENSION REORDERING
      IRET = 0
      CALL FINDORDR(Q,ORDER,RECLEN,IRET)
      RETURN
      END

!**********************************************************************
      SUBROUTINE FINDORDR(Q,ORDER,RECLEN,IRET)

! SEARCH FOR THE 'ORDER=' REORDERING COMMAND IN THE REQUEST LINE.
! CHECK SYNTAX.  SAVE NEW ORDER IN THE ORDER ARRAY.

      IMPLICIT NONE

      INTEGER*4 J,K,IS,IE,RECLEN,ORDER(6),IRET,MAXPOS
      CHARACTER OSTRING*6
      CHARACTER Q*(*)
      LOGICAL CHECK(5)

      IRET = 0
      ORDER(6) = 0
! FIND STRING 'ORDER='
      OSTRING = 'ORDER='
      MAXPOS = RECLEN-10
      IS = INDEX(Q,OSTRING)
!      WRITE(6,*) 'IS,OSTRING(IS:IS+5) = ',IS,Q(IS:IS+5)
      IF (IS.GT.MAXPOS) THEN
        GOTO 91
      ENDIF
      IF (IS.EQ.0) THEN
        GOTO 99
      ENDIF
! FIND LEFT PARENTHESIS '('
      IS = IS+6
      MAXPOS = RECLEN-4
      DO WHILE(IS.LE.MAXPOS .AND. Q(IS:IS).NE.'(')
        IS = IS+1
      END DO
      IF (IS.GT.MAXPOS) THEN
        GOTO 91
      ENDIF
! IS IDENTIFIES NEXT POSITION AFTER '('
!     WRITE(6,*) 'FOUND ( AT POSITION ',IS
      IS = IS+1
! FIND RIGHT PARENTHESIS ')'
      IE = IS
      DO WHILE(IE.LE.RECLEN .AND. Q(IE:IE).NE.')')
        IE = IE+1
      END DO
      IF (IE.GT.RECLEN) THEN
        GOTO 91
      ENDIF
! IE IDENTIFIES POSITION JUST BEFORE ')'
!     WRITE(6,*) 'FOUND ) AT POSITION ',IE
      IE = IE-1
! FIND ORDER OF DIMENSIONS.
! SET ORDER(6) TO NUMBER OF DIMENSIONS READ
      K = IS
      DO J = 1,5
        DO WHILE(K.LE.IE .AND. Q(K:K).EQ.' ' .OR. Q(K:K).EQ.',' .OR. Q(K:K).EQ.'/')
          K = K+1
        END DO
!       WRITE(6,*) 'FOUND ',Q(K:K), ' AT POSITION ',K
        IF (K.LE.IE) THEN
          READ(Q(K:K),'(I1)',ERR=91) ORDER(J)
          ORDER(6) = J
          K = K+1
        ENDIF
      END DO
!      WRITE(6,901) (ORDER(J),J=1,5)
!901   FORMAT(1X,'ORDER:',5(1X,I1))
      DO J = 1,5
        CHECK(J) = .FALSE.
      END DO
      DO J = 1,5
        CHECK(ORDER(J)) = .TRUE.
      END DO
      DO J = 1,5
        IF (.NOT.CHECK(J)) THEN
          GOTO 91
        ENDIF
      END DO
      GOTO 99
91    WRITE(6,*) ' *** ORDER REQUEST IS INVALID:'
      WRITE(6,601) Q
601   FORMAT(2X,A)
      IRET = 1
99    RETURN
      END
!***********************************************************************

      SUBROUTINE FINDCS(Q,N,IS,IPLACE)
! FIND POSITION (IPLACE) OF THE NEXT COMMA, BLANK, OR SLASH IN THE QUERY BEGINING AT COLUMN IS

      IMPLICIT NONE

      CHARACTER*80 Q
      INTEGER*4 IPLACE,I,IS,N

      IPLACE=80
      DO 10 I=IS,N
        IF (Q(I:I).EQ.' '.OR.Q(I:I).EQ.','.OR.Q(I:I).EQ.'/') THEN
          IPLACE=I-1
          RETURN
        ENDIF
10    CONTINUE
      RETURN
      END
!*******************************************************************
      SUBROUTINE WRITQ(IBLOCK,IVAR,ARRAY,IARRAY,TARRAY,VARLN,ORDER, &
        N1,N2,N3,N4,N5,NSIZE,DIM,VAR,FORM,FUNITO,RORI,DEFN,FUNFMT)
      use ifport
      IMPLICIT NONE
      include 'fdict'

      INTEGER      N1,N2,N3,N4,N5,VARLN
      integer      IARRAY(N1,N2,N3,N4,N5)
      REAL         ARRAY(N1,N2,N3,N4,N5)
      character*(VARLN) TARRAY(N1,N2,N3,N4,N5) ! text
      real*8 realno
      INTEGER*2 iwk1val
      CHARACTER*4  RTEMP,ITEMP,PCNUM,PCINT4     !For MAINFRAME
!     EQUIVALENCE (ARRAY,IARRAY)
      INTEGER*4    IBLOCK,IVAR,DIM(5,2),FUNITO,FUNFMT,LVAR,J,K
      INTEGER*4    LENDIM(5),POSDIM(5,2),LENPRE,LONE
      INTEGER*4    ORDER(6),CNT(5)
      INTEGER*4    FLEN,I,N,MAXDATA,I5,I4,I3,I2,I1,L,IC,LDEFN,IWIDTH, &
                   ICOL,LENGTH,LA,NEXTPOS,NSIZE,IS,IE,NVAL
      CHARACTER*60 PREHEAD,PREBLNK,PREDASH,PRE,PREFIVE,PREONE
      CHARACTER    VAR*(FMAXCHARVAR)
      CHARACTER*16 RNGNAM ! RANGE NAME FOR WK1 FORMAT OPTION
      CHARACTER    FORM*8,FORMAT*25,IFORM*25,LINE*487,DEFN*80
      CHARACTER    AttsStr*84,DimStr*36
      CHARACTER*26 SPACE/'                         '/
      CHARACTER*26 DASH/'-------------------------'/
      CHARACTER*10 IFORMT(0:26)/'(I1)','(I1)', &
        '(I2)','(I3)','(I4)','(I5)', &
        '(I6)','(I7)','(I8)','(I9)', &
        '(I10)','(I11)','(I12)','(I13)', &
        '(I14)','(I15)','(I16)','(I17)', &
        '(I18)','(I19)','(I20)','(I21)','(I22)','(I23)','(I24)','(I25)','(I26)'/
      CHARACTER*1  A1(8),A*8,COLUMN*2,WIDTH*2,RORI*1
      CHARACTER    CARRAY*26(100),CFORM*7,CRFORM*25,DIMNAM*6,tempc*8
      INTEGER      DEC,DECLEN,REPEAT,IDIM
      PARAMETER (MAXDATA=487) ! 487+25=512, a reasonable max record length
      EQUIVALENCE(A1(1),A)
      INTEGER*4    IORDARR(100)
      REAL         RORDARR(100)
      EQUIVALENCE(IORDARR,RORDARR)
      CHARACTER*166 INLAB
      INTEGER NWK1ROW,NWK1COL,IRANGE(2,2)
      COMMON/FWK1BLK/NWK1ROW,NWK1COL
! for composite table:
    character*50 stub,header*70,val_field*15,field*6,identifier*25
    integer field_srt(5),field_end(5),ldims(5),hs

      logical nonan/.true./  ! if true, converts NaN values to zero

      real*4 time_begin, time_end
      real*4 timer

      IF (FUNFMT.EQ.1) THEN
         IF(RORI.ne.'C') then
           WRITE(FUNITO) (((((ARRAY(I1,I2,I3,I4,I5), &
                        I1=DIM(1,1),DIM(1,2)), &
                        I2=DIM(2,1),DIM(2,2)), &
                        I3=DIM(3,1),DIM(3,2)), &
                        I4=DIM(4,1),DIM(4,2)), &
                        I5=DIM(5,1),DIM(5,2))
         else
           WRITE(FUNITO) (((((TARRAY(I1,I2,I3,I4,I5), &
                        I1=DIM(1,1),DIM(1,2)), &
                        I2=DIM(2,1),DIM(2,2)), &
                        I3=DIM(3,1),DIM(3,2)), &
                        I4=DIM(4,1),DIM(4,2)), &
                        I5=DIM(5,1),DIM(5,2))
         endif
         RETURN

      ENDIF
!---- create unformatted PC restart file ----
      IF (FUNFMT.EQ.2) THEN
         IF (RORI.EQ.'I'.OR.RORI.EQ.'H') THEN
!---- call function PCINT4 to convert mainframe int*4 to PC int*4 ---
            DO I5=DIM(5,1),DIM(5,2)
               DO I4=DIM(4,1),DIM(4,2)
                  DO I3=DIM(3,1),DIM(3,2)
                     DO I2=DIM(2,1),DIM(2,2)
                        DO I1=DIM(1,1),DIM(1,2)
                           ITEMP=PCINT4(IARRAY(I1,I2,I3,I4,I5))
                           WRITE(FUNITO) ITEMP
                        ENDDO
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
         ELSE
! --- call function PCNUM to convert EBCDIC real*4 to IEEE real*4 ---
            DO I5=DIM(5,1),DIM(5,2)
               DO I4=DIM(4,1),DIM(4,2)
                  DO I3=DIM(3,1),DIM(3,2)
                     DO I2=DIM(2,1),DIM(2,2)
                        DO I1=DIM(1,1),DIM(1,2)
                           RTEMP=PCNUM(ARRAY(I1,I2,I3,I4,I5))
                           WRITE(FUNITO) RTEMP
                        ENDDO
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
         ENDIF
         RETURN
      ENDIF
! Convert NaN to zero if option in effect.  For debugging, often better to leave them in.
      if(nonan.and.RORI.ne.'C') then
        do I5=DIM(5,1),DIM(5,2)
          do I4=DIM(4,1),DIM(4,2)
            do I3=DIM(3,1),DIM(3,2)
              do I2=DIM(2,1),DIM(2,2)
                do I1=DIM(1,1),DIM(1,2)
                  if(isnan(array(i1,i2,i3,i4,i5)) .and. RorI.eq.'R')then
                    array(i1,i2,i3,i4,i5)=0.
                  endif
                enddo
              enddo
            enddo
          enddo
        enddo
      endif
      if(funfmt.eq.7) then ! Write data in AIMMS composite Table format

! Fill this variable's dimension labels in FDimStr(FMaxDim,5)*25  to be used in the header
        CALL FFill_Strs(fblknm(ivar),fvarnm(ivar),IVAR) ! 1st arg: common block label. second arg: variable name
        write(funito,'(a)') 'COMPOSITE TABLE:'
        header=' '
        field_end(1:5)=0
        field_srt(1:5)=0
        field_srt(1)=1
        hs=1
        do j=1,fndims(ivar)
          ldims(j)=len_trim(FVarDims(j,ivar))
          header(hs:)=trim(FVardims(j,ivar))
          hs=hs+len_trim(FVardims(j,ivar))+2
          field_end(j)=hs-3
          if(j.le.4) field_srt(j+1)=field_end(j)+3
        enddo
        hs=len_trim(header)+3
        header(hs:)=trim(fblknm(ivar))//'_'//trim(fvarnm(ivar))
        identifier=trim(FBLKNM(ivar))//'_'//trim(FVARNM(IVAR))

        write(funito,'(a)') trim(header)
        stub=' '
        field=' '

        ! the format writes real*8 data like list-directed output format "*" and prevents loss of significance
        FORMAT='(a,t<hs>,1PG24.15E3)'

        do I5=DIM(5,1),DIM(5,2)
          if(fndims(ivar).ge.5) then
            write(field,'(i6)') I5
            stub(field_srt(5):field_end(5))=trim(adjustl(field))
          endif
          do I4=DIM(4,1),DIM(4,2)
            if(fndims(ivar).ge.4) then
              write(field,'(i6)') I4
              stub(field_srt(4):field_end(4))=trim(adjustl(field))
            endif
            do I3=DIM(3,1),DIM(3,2)
              if(fndims(ivar).ge.3) then
                write(field,'(i6)') I3
                stub(field_srt(3):field_end(3))=trim(adjustl(field))
              endif
              do I2=DIM(2,1),DIM(2,2)
                if(fndims(ivar).ge.2) then
                  write(field,'(i6)') I2
                  stub(field_srt(2):field_end(2))=trim(adjustl(field))
                endif
                do I1=DIM(1,1),DIM(1,2)
                  if(RorI.eq.'R') then
                    write(field,'(i6)') I1
                    stub(field_srt(1):field_end(1))=trim(adjustl(field))
                    write(funito,'(a,t<hs>,1PG24.15E3)') trim(stub),array(i1,i2,i3,i4,i5)
                  elseif(RorI.eq.'I'.or.RorI.eq.'H') then
                    write(field,'(i6)') I1
                    stub(field_srt(1):field_end(1))=trim(adjustl(field))
                    write(val_field,'(i10)') iarray(i1,i2,i3,i4,i5)
                    write(funito,'(a,t<hs>,a)') trim(stub),adjustl(val_field)
                  elseif(RorI.eq.'C') then
                    write(field,'(i6)') I1
                    stub(field_srt(1):field_end(1))=trim(adjustl(field))
                    if(index(tarray(i1,i2,i3,i4,i5),char(0)).eq.0 ) then
                      if(index(identifier,'CL_UNITS').gt.0) then
                        if(len_trim(tarray(i1,i2,i3,i4,i5)).eq.0) then
                          write(funito,'(a,t<hs>,a)') trim(stub),'"'//trim(tarray(i1,i2,i3,i4,i5))//'"'
                        else
                          write(funito,'(a,t<hs>,a)') trim(stub),'"'//trim(tarray(i1,i2,i3,i4,i5)(:5))//'_'//trim(tarray(I1,I2,I3,I4,I5)(7:))//'"'
                        endif
                      else
                        write(funito,'(a,t<hs>,a)') trim(stub),'"'//trim(tarray(i1,i2,i3,i4,i5))//'"'
                      endif
                    endif
                  endif
                enddo
              enddo
            enddo
          enddo
        enddo
        write(funito,'(a)') ';'
        return
      endif



      FORMAT=' '
      L=len_trim(FORM)
      LVAR=len_trim(VAR)
      IF(L.EQ.0) THEN
       FORM='12F10.3'
        L=7
      ENDIF
      A=FORM
      IC=INDEX(A,'I')
      IF (RORI.EQ.'R'.AND.IC.GT.0) A(IC:IC)='F'
      IC=INDEX(A,'F')
      IF ((RORI.EQ.'I'.OR.RORI.EQ.'H').AND.IC.GT.0) A(IC:IC)='I'
      IF (A1(1).EQ.'*') THEN
       IF (FUNFMT.NE.3) THEN
        IF (RORI.EQ.'I'.OR.RORI.EQ.'H') THEN
          WRITE(FUNITO,*)(((((IARRAY(I1,I2,I3,I4,I5), &
          I1=DIM(1,1),DIM(1,2)), &
          I2=DIM(2,1),DIM(2,2)), &
          I3=DIM(3,1),DIM(3,2)), &
          I4=DIM(4,1),DIM(4,2)), &
          I5=DIM(5,1),DIM(5,2))
        ELSEIF (RORI.EQ.'C') THEN
          WRITE(FUNITO,*)(((((TARRAY(I1,I2,I3,I4,I5), &
          I1=DIM(1,1),DIM(1,2)), &
          I2=DIM(2,1),DIM(2,2)), &
          I3=DIM(3,1),DIM(3,2)), &
          I4=DIM(4,1),DIM(4,2)), &
          I5=DIM(5,1),DIM(5,2))
        ELSE
          WRITE(FUNITO,*)(((((ARRAY(I1,I2,I3,I4,I5), &
          I1=DIM(1,1),DIM(1,2)), &
          I2=DIM(2,1),DIM(2,2)), &
          I3=DIM(3,1),DIM(3,2)), &
          I4=DIM(4,1),DIM(4,2)), &
          I5=DIM(5,1),DIM(5,2))
        ENDIF
       ENDIF
       RETURN
      ELSEIF (A(:3).EQ.'123') THEN
! TEST 123 IMPORT FORMAT--NOT AS FOOLPROOF AS OTHER FORMATS. 240 RECORD LENGTH MAX.
! 3 DESCRIPTIVE COLUMNS UP TO 110 CHARACTERS, THEN 130 CHARACTERS FOR UP TO 13 10-CHARACTER FIELDS.
! REQUIRES DIFFERENT DD STATEMENT THAN NORMAL 133 RECORD LENGTH
        LDEFN=len_trim(DEFN)
        DO 70 I5=DIM(5,1),DIM(5,2)
        DO 70 I4=DIM(4,1),DIM(4,2)
        DO 70 I3=DIM(3,1),DIM(3,2)
        DO 70 I2=DIM(2,1),DIM(2,2)
          WRITE(FUNITO,'(A,4I4,A,(13F10.3))') &
           '"'//VAR(:LVAR)//'" "'//DEFN(:LDEFN)//'" "',I5,I4,I3,I2,'"', &
          (ARRAY(I1,I2,I3,I4,I5),I1=DIM(1,1),DIM(1,2))
70      CONTINUE
        RETURN
      ENDIF
      WRITE(FORMAT,1000) '(',(A1(I),I=1,L),')'
1000  FORMAT(A,13A1)

! Fill variable dimension labels in FDimStr(FMaxDim,5)*25
      CALL FFill_Strs(fblknm(ivar),fvarnm(ivar),IVAR) ! 1st arg: common block label. can send ' '; second arg: variable name

      COLUMN=' '
      WIDTH=' '
      DEC = 0
      IRANGE(1,1)=0
      IRANGE(2,1)=2
      IRANGE(1,2)=0
      IRANGE(2,2)=2
! parse format specifier in string A (equivalenced to character*1 array A1),
! Need a format of the same width but with a the F/E/A etc converted to I and adjusted
! to be as wide as the heading string over each column, given by IWIDTH.
      DO I=1,L
        IF (A1(I).EQ.'D'.OR.A1(I).EQ.'E' &
           .OR.A1(I).EQ.'F'.OR.A1(I).EQ.'G'.OR.A1(I).EQ.'I'.OR.A1(I).eq.'A') THEN
           A1(I)='I'
           COLUMN=A(:I-1)
           I1=I+1
           I2=Index(a,'.')
           if(i2.eq.0) I2=I1+1
           WIDTH=A(I1:I2)
           IF (WIDTH(2:2).LT.'0'.OR.WIDTH(2:2).GT.'9') WIDTH(2:2)=' '
           READ(WIDTH,'(BN,I2)') IWIDTH
           READ(COLUMN,'(BN,I2)') ICOL
           IF (FUNFMT.EQ.3.AND.IWIDTH.GT.9) then
             IWIDTH=9
           ENDIF
           if (FDimLong(Order(1)).gt.IWIDTH) then
             IWIDTH=FDimLong(order(1))+1
           endif
           if (iwidth.gt.9) then
             write(width,'(i2)') iwidth
           else
             write(width,'(i1)') iwidth
           endif
           tempc=a(i2:)
           a=a(:i1-1)//trim(width)//tempc
           LENGTH=IWIDTH*ICOL
           IF (ICOL.GT.(DIM(ORDER(1),2)-DIM(ORDER(1),1)+1)) &
              LENGTH=IWIDTH*(DIM(ORDER(1),2)-DIM(ORDER(1),1)+1)
          exit
        ENDIF
      enddo
      L=len_trim(a)
! read number of decimal points (mantissa, value after ".")
      i=index(a,'.')
      if (i.gt.0) then
        DECLEN = L-I
        IF (DECLEN .GT. 0) THEN
          IF (DECLEN .EQ. 1) THEN
            READ(A(I+1:L),'(I1)') DEC
          ELSE
            READ(A(I+1:L),'(I2)') DEC
          ENDIF
        ENDIF
        A(I:)=' '
        LA=I-1
      ENDIF
      LA=len_trim(a)


!  DEFINE PREFIX STRINGS TO PRINT DIMENSION NAMES AND VALUES IN GENERIC FORMAT.
!  LENGTH OF PREFIX STRINGS IS DRIVEN BY THE SIZE OF THE DIMENSION NAMES
      J = 4
      PREHEAD = ' > '
      PREDASH = ' >-'
      PREBLNK = ' > '
      PREFIVE = ' > '
      PRE     = ' | '
      NEXTPOS = 4
      lendim(:)=0
      IS=DIM(ORDER(1),1)
      IE=DIM(ORDER(1),2)
      IF(IE-IS.GT.ICOL-1) IE=IS+ICOL-1
      DO J=4,2,-1
        K = len_trim(FVarDims(ORDER(J),IVAR))
        if (FDimLong(order(j)).gt.k) k=FDimLong(order(j))   !  space for dimension element strings
        IF (K.GT.0)THEN
          LENDIM(ORDER(J))=K
          POSDIM(ORDER(J),1)=NEXTPOS
          POSDIM(ORDER(J),2)=NEXTPOS+K-1
          PREHEAD(NEXTPOS:NEXTPOS+K) = &
          FVarDims(ORDER(J),IVAR)(:len_trim(fvardims(order(j),ivar)))//' '
          PREBLNK(NEXTPOS:NEXTPOS+K) = SPACE(1:K+1)
          PREDASH(NEXTPOS:NEXTPOS+K) = DASH(1:K+1)
          PREFIVE(NEXTPOS:NEXTPOS+K) = SPACE(1:K+1)
          NEXTPOS = NEXTPOS+K+1
        ENDIF
      END DO
      LENPRE=NEXTPOS
      PREHEAD(LENPRE-1:LENPRE) = ' |'
      PREBLNK(LENPRE-1:LENPRE) = ' |'
      PREDASH(LENPRE-1:LENPRE) = '--'
      PREFIVE(LENPRE-1:LENPRE) = ' |'
      K=len_trim(FVARDIMS(ORDER(5),IVAR))
      LENDIM(ORDER(5))=K
      IF (NSIZE.EQ.5) PREFIVE(4:K+4-1)=FVarDims(ORDER(5),IVAR)(:K)
      K=len_trim(FVARDIMS(ORDER(1),IVAR))
      PREONE(1:K+1)=FVarDims(ORDER(1),IVAR)(:K)//'='
      LONE=K+1
      IFORM=' '
      WRITE(IFORM,1000) '(',(A1(I),I=1,LA),')'
      IF (FUNFMT.EQ.3) THEN
        WRITE(INLAB,'(A)')  PREDASH(:LENPRE)
        NWK1ROW=NWK1ROW+1
        IF (NWK1ROW.GT.8192) GOTO 9991
        CALL WKSTR(FUNITO,NWK1ROW,1,INLAB,1,1,1)
        inlab='-'
        do i=1,ie
          CALL WKSTR(FUNITO,NWK1ROW,1+i,INLAB,4,1,1)
        enddo
      ELSEIF (FUNFMT.EQ.4) THEN
        WRITE(INLAB,'(A)')  PREDASH(:LENPRE)
        NWK1ROW=NWK1ROW+1
        CALL WCKSTR(FUNITO,NWK1ROW,1,INLAB,1,1,1)
        inlab='----------'
        do i=1,ie
          CALL WCKSTR(FUNITO,NWK1ROW,1+i,INLAB,4,1,1)
        enddo
      ELSEIF (FUNFMT.LE.4) then
        WRITE(FUNITO,'(A,487A1)')  PREDASH(:LENPRE),('-',I=1,LENGTH)
      ENDIF
      PRE=PREBLNK
      LDEFN=len_trim(DEFN)
      AttsStr(1:1) = ' '
      NEXTPOS = 2
      DO J=1,5
         IF (FBlkAtts(J,IBLOCK) .NE. '      ') THEN
            K = len_trim(FBlkAtts(J,IBLOCK))
            AttsStr(NEXTPOS:NEXTPOS+K) = FBlkAtts(J,IBLOCK)(:K)//'='
            NEXTPOS = NEXTPOS+K+1
            K = len_trim(FVarAtts(J,IVAR))
            AttsStr(NEXTPOS:NEXTPOS+K) = FVarAtts(J,IVAR)(:K)//' '
            NEXTPOS = NEXTPOS+K+1
         ENDIF
      END DO
      IF (FUNFMT.EQ.3) THEN
        WRITE(INLAB,2001) PREBLNK(:LENPRE),DEFN(:LDEFN),AttsStr(1:NEXTPOS-1)
        NWK1ROW=NWK1ROW+1
        IF(NWK1ROW.GT.8192) GOTO 9991
        CALL WKSTR(FUNITO,NWK1ROW,1,INLAB,1,1,1)
      ELSEIF (FUNFMT.EQ.4) THEN
        WRITE(INLAB,2001) PREBLNK(:LENPRE),DEFN(:LDEFN),AttsStr(1:NEXTPOS-1)
        NWK1ROW=NWK1ROW+1
        CALL WCKSTR(FUNITO,NWK1ROW,1,INLAB,1,1,1)
      ELSEIF (FUNFMT.LE.4) THEN
        WRITE(FUNITO,2001) PREBLNK(:LENPRE),DEFN(:LDEFN),AttsStr(1:NEXTPOS-1)
      ENDIF
      DimStr(1:1) = '('
      NEXTPOS = 2
      DO J=1,5
         IF (FVarDims(J,IVAR) .NE. '      ') THEN
            K = len_trim(FVarDims(J,IVAR))
            LENDIM(J)=K
            DimStr(NEXTPOS:NEXTPOS+K) = FVarDims(J,IVAR)(:K)//','
            NEXTPOS = NEXTPOS+K+1
         ENDIF
      END DO
      DimStr(NEXTPOS-1:NEXTPOS-1) = ')'
      IF (FUNFMT.EQ.3) THEN
        WRITE(INLAB,2000) PREBLNK(:LENPRE) &
           ,VAR(:LVAR),DimStr(1:NEXTPOS-1)
        NWK1ROW=NWK1ROW+1
        IF (NWK1ROW.GT.8192) GOTO 9991
        CALL WKSTR(FUNITO,NWK1ROW,1,INLAB,1,1,1)
        WRITE(INLAB,'(A)') PREDASH(:LENPRE)
        NWK1ROW=NWK1ROW+1
        IF (NWK1ROW.GT.8192) GOTO 9991
        CALL WKSTR(FUNITO,NWK1ROW,1,INLAB,1,1,1)
        inlab='-'
        do i=1,ie
          CALL WKSTR(FUNITO,NWK1ROW,1+i,INLAB,4,1,1)
        enddo
      ELSEIF (FUNFMT.EQ.4) THEN
        WRITE(INLAB,2000) PREBLNK(:LENPRE) &
           ,VAR(:LVAR),DimStr(1:NEXTPOS-1)
        NWK1ROW=NWK1ROW+1
        CALL WCKSTR(FUNITO,NWK1ROW,1,INLAB,1,1,1)
        WRITE(INLAB,'(A)') PREDASH(:LENPRE)
        NWK1ROW=NWK1ROW+1
        CALL WCKSTR(FUNITO,NWK1ROW,1,INLAB,1,1,1)
        inlab='----------'
        do i=1,ie
          CALL WCKSTR(FUNITO,NWK1ROW,1+i,INLAB,4,1,1)
        enddo
      ELSEIF (FUNFMT.LE.4) THEN
        WRITE(FUNITO,2000) PREBLNK(:LENPRE),VAR(:LVAR),DimStr(1:NEXTPOS-1)
        WRITE(FUNITO,'(A,487A1)') PREDASH(:LENPRE),('-',I=1,LENGTH)
      ENDIF
2000  FORMAT(A,1X,A,1X,A)
2001  FORMAT(A,1X,A,A)
100   FORMAT(/1X,A/)
      PRE=' |'//PREHEAD(3:)
      IS=DIM(ORDER(1),1)
      IE=DIM(ORDER(1),2)
7     IF(IE-IS.GT.ICOL-1) IE=IS+ICOL-1
      REPEAT = IE-IS+1
      IF(REPEAT*IWIDTH.GT.MAXDATA) THEN
        GOTO 996
      ENDIF

      CRFORM=IFORM
      do j=1,len_trim(crform)
        if(crform(j:j).eq.'I'.or.crform(j:j).eq.'i') crform(j:j)='A'
      enddo
      if (FdimStr(1,order(1)).eq.' '.or.DIM(ORDER(1),2).gt.FMaxDims) then
        WRITE(LINE,IFORM,ERR=999) (I1,I1=IS,IE)
      else
        WRITE(LINE,CRFORM,ERR=999) (trim(FDimStr(i1,order(1))(:min(iwidth,25))),I1=IS,IE)
      endif
!!!!  IF (IS.EQ.DIM(ORDER(1),1)) LINE(1:LONE+1)=' '//PREONE(1:LONE)
      IF (FUNFMT.EQ.3) THEN
        WRITE(INLAB,'(2A)') PREHEAD(:LENPRE)
        NWK1ROW=NWK1ROW+1
        IF (NWK1ROW.GT.8192) GOTO 9991
        CALL WKSTR(FUNITO,NWK1ROW,1,INLAB,1,1,1)
        if (FdimStr(1,order(1)).NE.' '.AND.ie.LE.FMaxDims) then
          do i1=is,ie
            inlab=trim(FDimStr(i1,order(1))(:iwidth-1))
            CALL WKSTR(FUNITO,NWK1ROW,i1-is+2,INLAB,1,1,1)
          enddo
        else
          do i1=is,ie
            iwk1val=i1
            CALL WKINT(FUNITO,NWK1ROW,i1-is+2,iwk1val,0)
          enddo
        endif
      ELSEIF (FUNFMT.EQ.4) THEN
        WRITE(INLAB,'(2A)') PREHEAD(:LENPRE)
        NWK1ROW=NWK1ROW+1
        CALL WCKSTR(FUNITO,NWK1ROW,1,INLAB,1,1,1)
        i1=FMaxDims
        if (FdimStr(1,order(1)).NE.' '.AND.ie.LE.FMaxDims) then
          do i1=is,ie
            inlab=trim(FDimStr(i1,order(1))(:iwidth-1))
            CALL WCKSTR(FUNITO,NWK1ROW,i1-is+2,INLAB,1,1,1)
          enddo
        else

          do i1=is,ie
            iwk1val=i1
            CALL WCKINT(FUNITO,NWK1ROW,i1-is+2,iwk1val,0)
          enddo

        endif
      ELSEIF (FUNFMT.LE.4) THEN
        WRITE(FUNITO,'(2A)') PREHEAD(:LENPRE),LINE(:len_trim(line))
      ENDIF
      IF (DIM(ORDER(1),2).GT.IE) THEN
        PREHEAD=PREBLNK
        IS=IE+1
        IE=DIM(ORDER(1),2)
        GOTO 7
      ENDIF
998   CONTINUE
      IF (FUNFMT.EQ.3)THEN
        WRITE(INLAB,'(A)')  PREDASH(:LENPRE)
        NWK1ROW=NWK1ROW+1
        IF (NWK1ROW.GT.8192) GOTO 9991
        CALL WKSTR(FUNITO,NWK1ROW,1,INLAB,1,1,1)
        inlab='-'
        do i=1,icol
          CALL WKSTR(FUNITO,NWK1ROW,1+i,INLAB,4,1,1)
        enddo
      ELSEIF (FUNFMT.EQ.4)THEN
        WRITE(INLAB,'(A)')  PREDASH(:LENPRE)
        NWK1ROW=NWK1ROW+1
        CALL WCKSTR(FUNITO,NWK1ROW,1,INLAB,1,1,1)
        inlab='----------'
        do i=1,icol
          CALL WCKSTR(FUNITO,NWK1ROW,1+i,INLAB,4,1,1)
        enddo
      ELSEIF (FUNFMT.LE.4) THEN
        WRITE(FUNITO,'(A,487A1)') PREDASH(:LENPRE),('-',I=1,LENGTH)
      ENDIF
      CFORM = '(40A'//WIDTH//')'
      DO 50 I5=DIM(ORDER(5),1),DIM(ORDER(5),2)
        CNT(ORDER(5)) = I5
        IF (NSIZE.GT.4) THEN
          WRITE(PREFIVE(4+LENDIM(ORDER(5)):6+LENDIM(ORDER(5))),'(I3)') I5
           if (i5.le.FMaxDims) then
             if (len_trim(FDimStr(i5,order(5))).gt.0) PREFIVE(4+15:4+17)=FDimStr(i5,order(5))
           endif

          IF (FUNFMT.EQ.3) THEN
            WRITE(INLAB,'(A)') PREFIVE(:LENPRE)
            NWK1ROW=NWK1ROW+1
            IF (NWK1ROW.GT.8192) GOTO 9991
            CALL WKSTR(FUNITO,NWK1ROW,1,INLAB,1,1,1)
            WRITE(INLAB,'(A)')  PREDASH(:LENPRE)
            NWK1ROW=NWK1ROW+1
            IF (NWK1ROW.GT.8192) GOTO 9991
            CALL WKSTR(FUNITO,NWK1ROW,1,INLAB,1,1,1)
            inlab='-'
            do i=1,icol
             CALL WKSTR(FUNITO,NWK1ROW,1+i,INLAB,4,1,1)
            enddo
          ELSEIF (FUNFMT.EQ.4) THEN
            WRITE(INLAB,'(A)') PREFIVE(:LENPRE)
            NWK1ROW=NWK1ROW+1
            CALL WCKSTR(FUNITO,NWK1ROW,1,INLAB,1,1,1)
            WRITE(INLAB,'(A)')  PREDASH(:LENPRE)
            NWK1ROW=NWK1ROW+1
            CALL WCKSTR(FUNITO,NWK1ROW,1,INLAB,1,1,1)
            inlab='---------'
            do i=1,icol
             CALL WCKSTR(FUNITO,NWK1ROW,1+i,INLAB,4,1,1)
            enddo
          ELSEIF (FUNFMT.LE.4) THEN
            WRITE(FUNITO,'(A)') PREFIVE(:LENPRE)
            WRITE(FUNITO,'(A,487A1)') PREDASH(:LENPRE),('-',I=1,LENGTH)
          ENDIF
        ENDIF
        DO 40 I4=DIM(ORDER(4),1),DIM(ORDER(4),2)
          CNT(ORDER(4)) = I4
          IF (NSIZE.GT.3) THEN
            WRITE(PRE(POSDIM(ORDER(4),1):POSDIM(ORDER(4),2)),IFORMT(LENDIM(ORDER(4)))) I4
            if (i4.le.FMaxDims) then
              if (len_trim(FDimStr(i4,order(4))).gt.0) PRE(POSDIM(ORDER(4),1):POSDIM(ORDER(4),2))=FDimStr(i4,order(4))
            endif
          ENDIF
          DO 30 I3=DIM(ORDER(3),1),DIM(ORDER(3),2)
            CNT(ORDER(3)) = I3
            IF (NSIZE.GT.2) THEN
              WRITE(PRE(POSDIM(ORDER(3),1):POSDIM(ORDER(3),2)), &
                IFORMT(LENDIM(ORDER(3)))) I3
              if (i3.le.FMaxDims) then
                if (len_trim(FDimStr(i3,order(3))).gt.0) PRE(POSDIM(ORDER(3),1):POSDIM(ORDER(3),2))=FDimStr(i3,order(3))
              endif
            ENDIF
            DO 20 I2=DIM(ORDER(2),1),DIM(ORDER(2),2)
              CNT(ORDER(2)) = I2
              IF (NSIZE.GT.1) THEN
                WRITE(PRE(POSDIM(ORDER(2),1):POSDIM(ORDER(2),2)), &
                  IFORMT(LENDIM(ORDER(2))))I2
                if (i2.le.FMaxDims) then
                  if (len_trim(FDimStr(i2,order(2))).gt.0) PRE(POSDIM(ORDER(2),1):POSDIM(ORDER(2),2))=FDimStr(i2,order(2))
                endif
              ENDIF
              IS=DIM(ORDER(1),1)
              IE=DIM(ORDER(1),2)
              CNT(ORDER(1)) = IS
              IF (FUNFMT.EQ.3) THEN  !WK1--SET STARTING RANGE, ROW
                IF (IRANGE(1,1).EQ.0) IRANGE(1,1)=NWK1ROW+1
              ENDIF
8             IF(IE-IS.GT.ICOL-1 .AND. FUNFMT.NE.3)  & !if too many columns, wrap
                 IE=IS+ICOL-1
              NVAL = IE-IS+1                      ! num values on a line

              IF (FUNFMT.EQ.3) THEN                  ! IF WK1 format write WK1 cells
                DO I1 = 1,NVAL
                  RORDARR(I1) = ARRAY(CNT(1),CNT(2),CNT(3),CNT(4),CNT(5))
                  CNT(ORDER(1)) = CNT(ORDER(1))+1
                END DO
                WRITE(INLAB,'(2A)') PRE(:LENPRE)
                NWK1ROW=NWK1ROW+1
                IF (NWK1ROW.GT.8192) GOTO 9991
                CALL WKSTR(FUNITO,NWK1ROW,1,INLAB,1,1,1)
                NWK1COL=1
                DO I1=1,NVAL
                  NWK1COL=NWK1COL+1
                  IF (ORDER(6).NE.0) THEN                ! if reordering array
                    IF (RORI.NE.'I'.AND.RORI.NE.'H'.AND.RORI.NE.'C') THEN  ! if not an integer
                      realno=RORDARR(I1)
                      CALL WKNUM(FUNITO,NWK1ROW,NWK1COL,realno,DEC,4)
                    ELSEIF (RORI.EQ.'C') THEN
                    ELSE                                  ! else an integer
                      iwk1val=iordarr(i1)
                      CALL WKINT(FUNITO,NWK1ROW,NWK1COL,iwk1val,4)
                    ENDIF
                  ELSE                                  ! else standard order
                    IF (RORI.NE.'I'.AND.RORI.NE.'H'.AND.RORI.NE.'C') THEN  ! if not an integer
                      realno=array(is+i1-1,i2,i3,i4,i5)
                      CALL WKNUM(FUNITO,NWK1ROW,NWK1COL,realno,DEC,4)
                    ELSEIF (RORI.EQ.'C') THEN
                    ELSE                                  ! else an integer
                      iwk1val=iarray(is+i1-1,i2,i3,i4,i5)
                      CALL WKINT(FUNITO,NWK1ROW,NWK1COL,iwk1val,4)
                    ENDIF
                  ENDIF
                ENDDO
                IRANGE(1,2)=NWK1ROW
                IRANGE(2,2)=NWK1COL
              ELSEIF (FUNFMT.EQ.4) THEN                  ! IF CSV format
                if (ORDER(6).NE.0)THEN
                  if (RORI.NE.'C') then
                    DO I1 = 1,NVAL
                      RORDARR(I1) = ARRAY(CNT(1),CNT(2),CNT(3),CNT(4),CNT(5))
                      CNT(ORDER(1)) = CNT(ORDER(1))+1
                    END DO
                  else
                    DO I1 = 1,NVAL
                      CARRAY(I1) = TARRAY(CNT(1),CNT(2),CNT(3),CNT(4),CNT(5))
                      CNT(ORDER(1)) = CNT(ORDER(1))+1
                    END DO
                  endif
                ENDIF
                WRITE(INLAB,'(2A)') PRE(:LENPRE)
                NWK1ROW=NWK1ROW+1
                CALL WCKSTR(FUNITO,NWK1ROW,1,INLAB,1,1,1)
                NWK1COL=1
                DO I1=1,NVAL
                  NWK1COL=NWK1COL+1
                  IF (ORDER(6).NE.0) THEN                ! if reordering array
                    IF (RORI.NE.'I'.AND.RORI.NE.'H'.AND.RORI.NE.'C') THEN  ! if not an integer
                      realno=RORDARR(I1)
                      CALL WCKNUM(FUNITO,NWK1ROW,NWK1COL,realno,DEC,4)
                    ELSEIF (RORI.EQ.'C') THEN
                      CALL WCKSTR(FUNITO,NWK1ROW,NWK1COL,CARRAY(I1),1,1,1)
                    ELSE                                  ! else an integer
                      iwk1val=iordarr(i1)
                      CALL WCKINT(FUNITO,NWK1ROW,NWK1COL,iwk1val,4)
                    ENDIF
                  ELSE                                  ! else standard order
                    IF (RORI.NE.'I'.AND.RORI.NE.'H'.AND.RORI.NE.'C') THEN  ! if not an integer
                      realno=array(is+i1-1,i2,i3,i4,i5)
                      CALL WCKNUM(FUNITO,NWK1ROW,NWK1COL,realno,DEC,4)
                    ELSEIF (RORI.EQ.'C') THEN
                      CALL WCKSTR(FUNITO,NWK1ROW,NWK1COL,TARRAY(IS+I1-1,i2,i3,i4,i5),1,1,1)
                    ELSE                                  ! else an integer
                      iwk1val=iarray(is+i1-1,i2,i3,i4,i5)
                      CALL WCKINT(FUNITO,NWK1ROW,NWK1COL,iwk1val,4)
                    ENDIF
                  ENDIF
                ENDDO
              ELSEIF (FUNFMT.LE.4) THEN  ! if not GDX  format, write text
                IF (ORDER(6).NE.0) THEN  ! if reordering, fill reorder arrays
                  DO I1 = 1,NVAL
                    RORDARR(I1) = &
                    ARRAY(CNT(1),CNT(2),CNT(3),CNT(4),CNT(5))
                    CNT(ORDER(1)) = CNT(ORDER(1))+1
                    if (rori.eq.'C') then
                      carray(i1)=tarray(CNT(1),CNT(2),CNT(3),CNT(4),CNT(5))
                    endif
                  END DO
                  IF (RORI.NE.'I'.AND.RORI.NE.'H'.AND.RORI.NE.'C') THEN  ! if not an integer
                    CALL FILLF(RORDARR,CARRAY,NVAL,DEC,IWIDTH)
                    WRITE(LINE,CFORM,ERR=997) (CARRAY(J),J=1,NVAL)
                  ELSEIF (RORI.EQ.'C') THEN
                    WRITE(LINE,CFORM,ERR=997) (CARRAY(J),J=1,NVAL)
                  ELSE                                  ! else an integer
                    WRITE(LINE,FORMAT,ERR=997) (IORDARR(J),J=1,NVAL)
                  ENDIF
                ELSE                    ! else standard array ordering
                  IF (RORI.NE.'I'.AND.RORI.NE.'H'.AND.RORI.NE.'C') THEN  ! if not an integer
                    CALL FILLF(ARRAY(IS,I2,I3,I4,I5),CARRAY,NVAL,DEC,IWIDTH)
                    WRITE(LINE,CFORM,ERR=997) (CARRAY(J),J=1,NVAL)
                  ELSEIF (RORI.EQ.'C') THEN
                    carray(1:nval)=tarray(is:ie,i2,i3,i4,i5)
                    WRITE(LINE,CFORM,ERR=997) (CARRAY(J),J=1,NVAL)
                  ELSE                                  ! else an integer
                    WRITE(LINE,FORMAT,ERR=997) (IARRAY(I1,I2,I3,I4,I5),I1=IS,IE)
                  ENDIF
                ENDIF
                WRITE(FUNITO,'(2A)') PRE(:LENPRE),LINE(:len_trim(line))
              ENDIF
              PRE=' |'//PREBLNK(3:LENPRE)
              IF (DIM(ORDER(1),2).GT.IE) THEN
                IS=IE+1
                IE=DIM(ORDER(1),2)
                GOTO 8
              ENDIF
20          CONTINUE
30      CONTINUE
40     CONTINUE
50    CONTINUE
      IF (FUNFMT.EQ.3) THEN ! WK1--WRITE RANGE RECORD FOR THIS VARIABLE
        RNGNAM=VAR  ! RNGNAM IS *16, VAR IS CURRENTLY *16 WITH PARAMETER SIZE
        CALL WKNAME(FUNITO,IRANGE(1,1),IRANGE(2,1),IRANGE(1,2),IRANGE(2,2),RNGNAM)
      ENDIF
      RETURN
999   WRITE(6,*) ' BAD INDEX LABEL FORMAT: ',IFORM
      GO TO 998
997   WRITE(6,*) ' BAD ROW FORMAT: ',FORMAT  ! may need to change 40 in "CFORM = '(40A'//WIDTH//')" to higher number
      RETURN
996   WRITE(6,*) '*** CANNOT WRITE DATA TO RESTART FILE.  LINE LENGTH EXCEEDS MAXIMUM FOR:'
      WRITE(6,*) '*** VARIABLE '//VAR(1:LVAR)//' WITH FORMAT '//FORM
      RETURN
9991  WRITE(6,*) '*** IN FILER, WRITQ, WK1 FILE ROWS EXCEED 8192 MAX'
      RETURN
      END
!******************************************************************
!---> the following codes apply only to MAINFRAME ----
!---- program to convert mainframe INTEGER*4 to pc INTEGER*4 ----
      CHARACTER*4   FUNCTION PCINT4(INTNO)
      IMPLICIT NONE

      CHARACTER*4   COUT,INUM,CHROUT
      INTEGER*4     INTNO,INTVAL,INTOUT
      EQUIVALENCE   (INTVAL,COUT),(INTOUT,CHROUT)

      INTVAL = INTNO

!---- flip bytes -----
      CHROUT(1:1) = COUT(4:4)
      CHROUT(2:2) = COUT(3:3)
      CHROUT(3:3) = COUT(2:2)
      CHROUT(4:4) = COUT(1:1)

!---- write it out ----
      WRITE(INUM,'(A4)') INTOUT
      PCINT4=INUM
      RETURN
      END
!******************************************************************
!---- program to convert mainframe real*4 to pc real*4 ----
      CHARACTER*4   FUNCTION PCNUM(REALNO)
      IMPLICIT NONE

!  The following is the format of the input EBCDIC REAL*4 number:
!  (counting from right to left, rightmost bit=0 and leftmost bit=31)
!     Bits            Description
!      31  (1 bit)    sign (1=-, 0=+)
!    30-24 (7 bits)   exponent in base 16 (excess Z40 notation, Z40=0)
!    23-0 (24 bits)   fractional part
!
!
!  The following is the format of the output IEEE REAL*4 number:
!
!     Bits            Description
!      31  (1 bit)    sign (1=-, 0=+)
!    30-23 (8 bits)   exponent in base 2 (excess Z7F notation, Z7F=0)
!    22-0 (23 bits)   fractional part
!
      CHARACTER*1   COUT(4),A1(4),A1IN(4)
      CHARACTER*4   CHROUT,NUM
      REAL*4        REALVAL,REALNUM,REALNO
      INTEGER       I
      INTEGER*4     INTNUM,INTOUT,I4, &
                    IRIGHT,Z40/Z00000040/,Z7F/Z0000007F/
      EQUIVALENCE   (REALNUM,INTNUM,A1IN(1))
      EQUIVALENCE   (INTOUT,CHROUT,COUT(1))
      EQUIVALENCE   (I4,A1(1))

      INTOUT = 0

      REALVAL = REALNO
!---- no bits need to be set for a value of zero ----
      IF (REALVAL.EQ.0) THEN
         PCNUM= CHROUT
         RETURN
      ENDIF
      REALNUM = ABS(REALVAL)     ! remove the sign bit so that the
                                 ! first byte contains only the exponent

!---- copy the fractional part first ----
      DO I=2,4                   ! fill COUT/INTOUT with the
         COUT(I) = A1IN(I)       ! fractional part of the input.
      ENDDO

!---- search for the first 1 bit in the fraction ----
!---- (which is found among the first 4 bits numbered 23-20) ----
      IRIGHT=0
      DO I = 20,23
         IF (BTEST(INTOUT,I)) IRIGHT = I
      ENDDO
      IRIGHT = 23 - IRIGHT       ! number of bits to shift right.
                                 ! PC number always starts with 1,
                                 !  we'll shift 1 less than required
                                 !  and discard the leading 1 bit.

!---- shift left that number of bits ----
      IF (IRIGHT .LT. 0) WRITE(6,'('' ERROR in REAL exponent in PCNUM'')')
      IF (IRIGHT .GT. 0) THEN
         INTOUT = ISHFT(INTOUT,IRIGHT) ! shift for reposition of binary point to the right of first 1 bit
      ENDIF
      INTOUT = IBCLR(INTOUT,23)        ! zap that first 1 bit - used on PC as part of the exponent

!---- now the exponent part ----
!---- convert to base 16 (by multiplying by 4), subtract 1 for ----
!---- each position that binary point is shifted to the right, ----
!---- then add Z7F to convert to excess 127d notation. ---
      I4 = 0                     !  Integer in which to calculate.
      A1(4) = A1IN(1)            !  Exponent in its low-order area.
      I4 = (I4 - Z40)*4 - (IRIGHT + 1) + Z7F
      I4 = ISHFT(I4,23)          !  Push it to the exponent position.
      INTOUT = IOR(INTOUT,I4)    !  OR it onto the first half.
      IF (REALVAL .LT. 0)  INTOUT = IBSET(INTOUT,31)  !  Sign.

!---- write it out ----
      WRITE(NUM,'(4A)') (COUT(I),I=4,1,-1)
      PCNUM=NUM
      RETURN
      END

!*********************************************************************
! --- function to convert mainframe EBCDIC characters to pc ASCII ---
      CHARACTER*80  FUNCTION PCSTR(LABELIN)
      IMPLICIT NONE
      CHARACTER*80  LABEL,LABELIN,STR
      INTEGER       LLEN
      LABEL=LABELIN
      LLEN=LEN(LABEL)
!---- convert EBCDIC to ASCII ----
      CALL EBC2ASCI (LABEL,LLEN)
!---- write out ASCII string ----
      WRITE(STR,'(A)') LABEL(1:LLEN)
      PCSTR=STR
!---- clear label for reuse ----
      LABEL(1:20)='                    '
      LABEL(21:80)=LABEL(1:20)//LABEL(1:20)//LABEL(1:20)
      RETURN
      END

      SUBROUTINE EBC2ASCI(LABEL,LLEN)
!---- convert each character along the string to its ASCII equivalence. ----
!---- If no equivalence is found for a character, then the character is set to '?' ----
      CHARACTER*(*)    LABEL
      CHARACTER*1      TEMP
      INTEGER          I,J,MAP(255)
! --- assign the equivalent decimal value corresponding to a particular character ---
      DATA MAP(64)/32/, &
          (MAP(I),I=75,80)/46,60,40,43,124,38/, &
          (MAP(I),I=90,94)/33,36,42,41,59/, &
          (MAP(I),I=96,97)/45,47/, &
          (MAP(I),I=107,111)/44,37,95,62,63/, &
          (MAP(I),I=122,127)/58,35,64,39,61,34/, &
          (MAP(I),I=129,137)/97,98,99,100,101,102,103,104,105/, &
          (MAP(I),I=145,153)/106,107,108,109,110,111,112,113,114/, &
          (MAP(I),I=162,169)/115,116,117,118,119,120,121,122/, &
          (MAP(I),I=192,201)/123,65,66,67,68,69,70,71,72,73/, &
          (MAP(I),I=208,217)/125,74,75,76,77,78,79,80,81,82/, &
          (MAP(I),I=226,233)/83,84,85,86,87,88,89,90/, &
          (MAP(I),I=240,249)/48,49,50,51,52,53,54,55,56,57/
! --- assign the equivalent decimal value of '?' to undefined EBCDIC characters ---
      DATA (MAP(I),I=1,63)/63*63/, &
           (MAP(I),I=65,74)/10*63/, &
           (MAP(I),I=81,89)/9*63/,MAP(95)/63/, &
           (MAP(I),I=98,106)/9*63/, &
           (MAP(I),I=112,121)/10*63/,MAP(128)/63/, &
           (MAP(I),I=138,144)/7*63/, &
           (MAP(I),I=154,161)/8*63/, &
           (MAP(I),I=170,191)/22*63/, &
           (MAP(I),I=202,207)/6*63/, &
           (MAP(I),I=218,225)/8*63/, &
           (MAP(I),I=234,239)/6*63/, &
           (MAP(I),I=250,255)/6*63/
      DO J = 1,LLEN
         TEMP=CHAR(MAP(ICHAR(LABEL(J:J))))
         LABEL(J:J)=TEMP
      ENDDO
      RETURN
      END
!---> the above codes apply only to MAINFRAME ----
!*******************************************************************
      SUBROUTINE FILLF(RFIELD,CFIELD,NVAL,IDEC2,ICOLWD)

       IMPLICIT NONE

! FORMATS RFIELD ARRAY INTO CHARACTER ARRAY "CFIELD" WITH "IDEC" DECIMAL POINTS.
! IF IDEC=9, THEN INTEGER FORMAT;
! IF NUMBER TOO LARGE FOR FIELD, REDUCE IDEC TO NUMBER THAT WILL FIT
! LEAVING FIRST CHARACTER BLANK TO SEPARATE FROM OTHER NUMBERS.
      INTEGER       IDEC,IDEC2,ICOLWD,LF,LFIT,NVAL,J,NEG
      CHARACTER*7   FORM
      CHARACTER*(*) CFIELD(NVAL)
      REAL          RFIELD(NVAL),MAXNO,RFIT
      IDEC=IDEC2
      IF(IDEC.GT.9) IDEC=9
      NEG = 0
      MAXNO = RFIELD(1)
      IF (MAXNO.LT.0) NEG = 1
      DO J = 1,NVAL
        MAXNO = AMAX1(ABS(MAXNO),ABS(RFIELD(J)))
        IF (RFIELD(J).LT.0) NEG = 1
      END DO
      LF=ICOLWD-1
10    LFIT=LF
      IF (NEG.EQ.1) LFIT=LFIT-1
      IF (IDEC.NE.9) THEN
        LFIT = LFIT-IDEC-1
        IF(LFIT.LE.0) THEN
          RFIT = -0.1
        ELSE
          RFIT = 10.**LFIT-1.
        ENDIF
        IF (MAXNO.GT.RFIT) THEN
          IDEC = IDEC-1
          IF (IDEC.LE.0) IDEC = 9
          GOTO 10
        ENDIF
        IF (LF.GE.9) THEN
          WRITE(FORM,'(A,I2,A,I1,A)')  '(F',LF+1,'.',IDEC,')'
        ELSE
          WRITE(FORM,'(A,I1,A,I1,A)')  '(F',LF+1,'.',IDEC,')'
        ENDIF
        DO J=1,NVAL
          WRITE(CFIELD(J),FORM,ERR=98) RFIELD(J)
        END DO
      ELSE
        IF (LFIT.LE.0) THEN
          RFIT = 0.
        ELSE
          RFIT=10.**LFIT-1.
        ENDIF
        IF (LF.GE.9) THEN
          WRITE(FORM,'(A,I2,A)')  '(I',LF+1,')'
        ELSE
          WRITE(FORM,'(A,I1,A)')  '(I',LF+1,')'
        ENDIF
        DO J=1,NVAL
          IF (RFIELD(J).GT.RFIT) THEN
            CFIELD(J) = ' *******'
          ELSE
            WRITE(CFIELD(J),FORM,ERR=98) NINT(RFIELD(J))
          ENDIF
        END DO
      ENDIF
      GOTO 99
98    WRITE(6,*) '*** FILLF: ERROR CONVERTING ', RFIELD(J)
99    RETURN
      END
!*****************************************************************
      SUBROUTINE READQ(IVAR,ARRAY,IARRAY,TARRAY,VARLN,ORDER, &
       N1,N2,N3,N4,N5,DIM,FUNITI,FORM,RORI,FUNFMT,IRET,RQST_BUFFER)
       use ifport

!   READS UPDATE DATA FOR A SINGLE VARIABLE, YEAR.  DATA FORMATTED FROM
!   A PREVIOUS QUERY IS ACCEPTED BUT NOT REQUIRED.  THE ROUTINE ACCEPTS
!   DATA IN THE FREE FORMAT OF LIST-DIRECTED I/O, WITH EXCEPTIONS:
!
!  1) DATA MUST BE ORDERED ACCORDING TO ARRAY DIMENSIONS AND YEARS.
!  2) DATA FOR SKIPPED PORTIONS OF ARRAYS DESIGNATED BY EXPLICIT DIMENSION
!     SPECIFICATIONS MUST NOT BE INCLUDED
!  5) COLUMN 1 IS NOT TO BE USED FOR UPDATE DATA.  A "1" IN COLUMN 1 SIGNALS A NEW UPDATE REQUEST
!  6) BLANK LINES AND LINES WITH A ">' IN COLUMN 2 ARE SKIPPED

      IMPLICIT NONE
      include 'fdict'
      INTEGER FILENG
      EXTERNAL FILENG
      CHARACTER*8 FORM,FORM2,RORI*1,RQST_BUFFER*(*)
      INTEGER     J1,J2,J3,J4,J5,ios,nerr/0/,length
      INTEGER iletter,FieldWidth,NPerLine,LText
      save nerr
      INTEGER     NVAL,FLEN,N1,N2,N3,N4,N5,IARRAY(N1,N2,N3,N4,N5), &
                  IBAR,IB2,ICOM,NFIELD,ISTAR,IERR,IRET,J, &
                  IV,I1,I2,I3,I4,I5,IFS,IFE,I,L,ORDER(6),CNT(5)
      REAL        ARRAY(N1,N2,N3,N4,N5),V
      integer varln
      character*(varln) tarray(n1,n2,n3,n4,n5)
      INTEGER*4   DIM(5,2),FUNITI,FUNFMT,ind(5)
      CHARACTER   LINE*512,FIELD*20,TEMPF*20
      CHARACTER*10 FFORM(20)/'(BN,F1.0)', &
        '(BN,F2.0)','(BN,F3.0)','(BN,F4.0)','(BN,F5.0)', &
        '(BN,F6.0)','(BN,F7.0)','(BN,F8.0)','(BN,F9.0)', &
        '(BN,F10.0)','(BN,F11.0)','(BN,F12.0)','(BN,F13.0)', &
        '(BN,F14.0)','(BN,F15.0)','(BN,F16.0)','(BN,F17.0)', &
        '(BN,F18.0)','(BN,F19.0)','(BN,F20.0)'/
       CHARACTER*10 IFORM(20)/'(BN,I1)', &
        '(BN,I2)','(BN,I3)','(BN,I4)','(BN,I5)', &
        '(BN,I6)','(BN,I7)','(BN,I8)','(BN,I9)', &
        '(BN,I10)','(BN,I11)','(BN,I12)','(BN,I13)', &
        '(BN,I14)','(BN,I15)','(BN,I16)','(BN,I17)', &
        '(BN,I18)','(BN,I19)','(BN,I20)'/

      character*25 Identifier
      integer chkdim

      real time_begin, time_end
      real*4 timer

      integer ivar,k,z
!=======================================================


!       WRITE(6,601) (ORDER(J),J=1,5)
!601   FORMAT(' IN READQ: ORDER = (',I1,',',I1,',',I1,',',I1,',',I1,')')
      IF (FUNFMT.EQ.1) THEN
        if (RORI.ne.'C') then
          READ(FUNITI,END=999,ERR=997,iostat=ios) &
          (((((ARRAY(I1,I2,I3,I4,I5), &
          I1=DIM(1,1),DIM(1,2)), &
          I2=DIM(2,1),DIM(2,2)), &
          I3=DIM(3,1),DIM(3,2)), &
          I4=DIM(4,1),DIM(4,2)), &
          I5=DIM(5,1),DIM(5,2))
        else
          READ(FUNITI,END=999,ERR=997,iostat=ios) &
          (((((TARRAY(I1,I2,I3,I4,I5), &
          I1=DIM(1,1),DIM(1,2)), &
          I2=DIM(2,1),DIM(2,2)), &
          I3=DIM(3,1),DIM(3,2)), &
          I4=DIM(4,1),DIM(4,2)), &
          I5=DIM(5,1),DIM(5,2))
        endif
        RETURN
      elseif (funfmt.eq.7) then  ! aimms inputs via composite table
            ! check dimension count. Sometimes dictionary has the wrong number
         chkdim=0
         do j=1,5
           if (fkdim(j,ivar).gt.1) then
             chkdim=chkdim+1
           endif
         enddo
         if (chkdim.gt.fndims(ivar))then
           fndims(ivar)=chkdim
         endif

         identifier=trim(FBLKNM(ivar))//'_'//trim(FVARNM(IVAR))//char(0)
         call removechar(identifier,'$')
         if (rori.eq.'R') then

!   initialize all to zero
           do I5=DIM(5,1),DIM(5,2)
           do I4=DIM(4,1),DIM(4,2)
           do I3=DIM(3,1),DIM(3,2)
           do I2=DIM(2,1),DIM(2,2)
           do I1=DIM(1,1),DIM(1,2)
             array(i1,i2,i3,i4,i5)=0.
           enddo
           enddo
           enddo
           enddo
           enddo
         elseif (rori.eq.'I') then

!   now initialize all to zero
           do I5=DIM(5,1),DIM(5,2)
           do I4=DIM(4,1),DIM(4,2)
           do I3=DIM(3,1),DIM(3,2)
           do I2=DIM(2,1),DIM(2,2)
           do I1=DIM(1,1),DIM(1,2)
             iarray(i1,i2,i3,i4,i5)=0
           enddo
           enddo
           enddo
           enddo
           enddo
         elseif (rori.eq.'C') then
           do I5=DIM(5,1),DIM(5,2)
           do I4=DIM(4,1),DIM(4,2)
           do I3=DIM(3,1),DIM(3,2)
           do I2=DIM(2,1),DIM(2,2)
           do I1=DIM(1,1),DIM(1,2)
             tarray(i1,i2,i3,i4,i5)=' '
           enddo
           enddo
           enddo
           enddo
           enddo
         endif

         ind(1:5)=1  ! default index is one
75       continue
           read(funiti,'(a)',end=199) line
           if (index(line,';').gt.0) return
           if (index(line,'-------').gt.0) goto 75
           z=index(line,' zero ')
           if (z.gt.0) then
             if(RorI.eq.'R')  array(ind(1),ind(2),ind(3),ind(4),ind(5))=0.
             if(RorI.eq.'I') iarray(ind(1),ind(2),ind(3),ind(4),ind(5))=0
           elseif (RorI.eq.'R') then
             read(line,*,end=1956,err=1956) ind(1:fndims(ivar)),array(ind(1),ind(2),ind(3),ind(4),ind(5))
1956         continue
           elseif (RorI.eq.'I'.or. RorI.eq.'H') then
             v=0
             read(line,*) ind(1:fndims(ivar)), v  ! integers will be formatted with decimals, read with real "v" and convert
1957         continue
             iarray(ind(1),ind(2),ind(3),ind(4),ind(5))=v
           elseif (RorI.eq.'C') then
             if (index(line,char(0)).eq.0) then
               read(line,*) ind(1:fndims(ivar)),tarray(ind(1),ind(2),ind(3),ind(4),ind(5))
             endif
           endif
55         goto 75
199        continue

           return

      elseif (funfmt.gt.1) then
        write(6,'(A,I2,A)') ' funfmt=',funfmt,' not supported for input'
      ENDIF

      IF (FORM.EQ.'*') THEN
        IF (RORI.EQ.'I'.OR.RORI.EQ.'H') THEN
          READ(FUNITI,*,END=999,ERR=997)(((((IARRAY(I1,I2,I3,I4,I5), &
          I1=DIM(1,1),DIM(1,2)), &
          I2=DIM(2,1),DIM(2,2)), &
          I3=DIM(3,1),DIM(3,2)), &
          I4=DIM(4,1),DIM(4,2)), &
          I5=DIM(5,1),DIM(5,2))
          return
        ELSEIF (RORI.EQ.'C') THEN
          NPerLine=512/varln
          FieldWidth=varln
          write(form,'(i2,a,i2.2)') NPerline,'A',FieldWidth
        ELSE
          READ(FUNITI,*,END=999,ERR=997) (((((ARRAY(I1,I2,I3,I4,I5), &
          I1=DIM(1,1),DIM(1,2)), &
          I2=DIM(2,1),DIM(2,2)), &
          I3=DIM(3,1),DIM(3,2)), &
          I4=DIM(4,1),DIM(4,2)), &
          I5=DIM(5,1),DIM(5,2))
          return
        ENDIF
      ENDIF
      NVAL=0
      IFS=134
      L=0
      if (rori.eq.'C') then
        FieldWidth=varln
        iletter=index(form,'A')
        if (iletter.gt.0) then
          form2=form(iletter+1:)
          read(form2,*) FieldWidth
          form2=form(1:iletter-1)
          read(form2,*) NPerLine
          LText=FieldWidth*NPerLine
        endif
      endif

      DO 50 I5=DIM(ORDER(5),1),DIM(ORDER(5),2)
        CNT(ORDER(5)) = I5
      DO 40 I4=DIM(ORDER(4),1),DIM(ORDER(4),2)
        CNT(ORDER(4)) = I4
      DO 30 I3=DIM(ORDER(3),1),DIM(ORDER(3),2)
        CNT(ORDER(3)) = I3
      DO 20 I2=DIM(ORDER(2),1),DIM(ORDER(2),2)
        CNT(ORDER(2)) = I2
!       WRITE(6,*) ' ORDER(1),DIM(ORDER(1),1),DIM(ORDER(1),2) = ',
!    +             ORDER(1),DIM(ORDER(1),1),DIM(ORDER(1),2)
        I1=DIM(ORDER(1),1)
        CNT(ORDER(1)) = I1
        IF (NVAL.GT.0) GOTO 17
        IF (IFS.LT.L) GOTO 14
12      READ(FUNITI,100,END=999) LINE
        IF (LINE(1:1).EQ.'1') THEN
          GOTO 997
        ENDIF
        IF (LINE(2:2).EQ.'>') GOTO 12
        L=len_trim(LINE)
        IF (L.EQ.0.and.RORI.ne.'C') GOTO 12
! FOR READING QUERY OUTPUT FORMAT, GET RID OF STUFF BETWEEN BARS ("º")
        IBAR=1
        IBAR=INDEX(LINE,':')
        IF (IBAR.EQ.0) IBAR=INDEX(LINE,'|')
        IF (IBAR.GT.0) THEN
          IB2=INDEX(LINE(IBAR+1:),':')
          IF (IB2.EQ.0) IB2=INDEX(LINE(IBAR+1:L),'|')
          LINE=line(IBAR+IB2+1:)  ! line(IBAR:IBAR+IB2)=' '
          L=len_trim(LINE)
        else
!  for reading list-directed output of text variables, there is an unwanted space in column 1.
!  This removes it wthout harming other text input.
          if (line(1:1).eq.' ') then
            line=line(2:)
          endif
        ENDIF
      IF (RORI.EQ.'C') THEN
        L=LText  ! character text may be blank so need to read empty fields. So don't use non-blank line length to mark data fields
      ELSE
! REMOVE COMMAS
        ICOM=INDEX(LINE,',')
        IF (ICOM.GT.0) THEN
          DO 13 I=ICOM,L
            IF (LINE(I:I).EQ.',') LINE(I:I)=' '
13        CONTINUE
        ENDIF
!
      endif


        IFS=1
        IFE=0
!  FIND NEXT NON-BLANK (START OF FIELD)
14      CONTINUE
        IF (LINE(IFS:IFS).EQ.' '.and.RORI.ne.'C')THEN
          IFS=IFS+1
          IF (IFS.LE.L) GOTO 14
          GOTO 12
        ENDIF
      if (rori.ne.'C') then
!  FIND NEXT NON-BLANK (END OF FIELD)
          IFE=IFS+1
15      CONTINUE
        IF (LINE(IFE:IFE).NE.' ')THEN
          IFE=IFE+1
          IF (IFE.LT.L)GOTO 15
        ENDIF
        NFIELD=IFE-IFS+1
        IF (NFIELD.GT.20) GOTO 998
      else
        IF (IFE.GT.0) IFS=IFE+1
        IFE=IFS+FieldWidth-1
        if (IFE.gt.L) IFE=L
        endif
!  READ THE VALUE FROM THE FIELD.  CHECK FOR REPEAT VALUES, SUCH AS 10*0.0, BY LOOKING
! FOR AN ASTERISK IN THE FIELD.  ALL ASTERISKS IN THE FIELD IS AN ERROR.
        FIELD=' '
        I=0
        ISTAR=0
        FIELD=LINE(IFS:IFE)
        ISTAR=INDEX(FIELD,'*')
        IERR=0
        IF (ISTAR.EQ.1) THEN
          IERR=1
        ELSEIF (ISTAR.GT.0) THEN
          IERR=INDEX(FIELD(ISTAR+1:),'*')
        ENDIF
        IF (IERR.GT.0) THEN
          WRITE(6,*)' *** ASTERISKS FOUND IN INPUT RESTART FILE'
          GOTO 998
        ENDIF
        IF (ISTAR.GT.0)THEN
          READ(FIELD,IFORM(ISTAR-1),ERR=998) NVAL
          TEMPF=FIELD(ISTAR+1:)
          FIELD=TEMPF
          NFIELD=NFIELD-ISTAR
          ISTAR=0
        ELSE
          NVAL=1
        ENDIF
        IF (RORI.NE.'I'.AND.RORI.NE.'H'.and.RORI.ne.'C') THEN
          READ(FIELD,FFORM(NFIELD),ERR=998) V
        ELSEIF(RORI.eq.'C') then
          ! field
        ELSE
          READ(FIELD,IFORM(NFIELD),ERR=998) IV
        ENDIF
17      CONTINUE
        IF (NVAL.GT.0)THEN
          IF (RORI.NE.'I'.AND.RORI.NE.'H'.and.RORI.ne.'C') THEN
            ARRAY(CNT(1),CNT(2),CNT(3),CNT(4),CNT(5)) = V
!           WRITE(6,602) CNT(1),CNT(2),CNT(3),CNT(4),CNT(5),
!    +                   ARRAY(CNT(1),CNT(2),CNT(3),CNT(4),CNT(5))
!602         FORMAT(' ARRAY(',I1,',',I1,',',I1,',',I1,',',I1,') = ',
!     +             F10.3)
          ELSEIF (RORI.eq.'C') then
            tarray(CNT(1),CNT(2),CNT(3),CNT(4),CNT(5))=line(ifs:ife)
          ELSE
            IARRAY(CNT(1),CNT(2),CNT(3),CNT(4),CNT(5)) = IV
!            WRITE(6,603) CNT(1),CNT(2),CNT(3),CNT(4),CNT(5),
!     +                   IARRAY(CNT(1),CNT(2),CNT(3),CNT(4),CNT(5))
!603         FORMAT(' IARRAY(',I1,',',I1,',',I1,',',I1,',',I1,') = ',I9)
          ENDIF
          NVAL=NVAL-1
          I1=I1+1
          CNT(ORDER(1)) = I1
!         WRITE(6,*) ' ORDER(1), CNT(ORDER(1)) = ',ORDER(1),CNT(ORDER(1))
          IF(I1.LE.DIM(ORDER(1),2)) GOTO 17
        ENDIF
19      IFS=IFE
        IF (I1.LE.DIM(ORDER(1),2)) THEN
          IF (IFS.LT.L) GOTO 14
          GOTO 12
        ENDIF
20    CONTINUE
30    CONTINUE
40    CONTINUE
50    CONTINUE
      IRET=0
      RETURN
100   FORMAT(A)
999   WRITE(6,100)' *** Error Reading: '// RQST_BUFFER(:FILENG(RQST_BUFFER))
      WRITE(6,100)' *** EARLY END OF FILE FOUND ON INPUT RESTART FILE.'
      WRITE(6,100)' *** THE VARIABLE IS NOT COPIED TO COMMON.'
      WRITE(6,100)' *** PROCESSING OF THIS INPUT REQUEST STOPPED.'
      IRET=2
      RETURN
998   WRITE(6,100) ' *** Error Reading: '// RQST_BUFFER(:FILENG(RQST_BUFFER))
      WRITE(6,100) ' *** INVALID DATA FORMAT IN INPUT RESTART FILE.  THE LINE IS:'
      WRITE(6,100) LINE(:FILENG(LINE))
      WRITE(6,100) ' *** THE VARIABLE IS NOT COPIED TO COMMON'
      WRITE(6,100) ' *** PROCESSING CONTINUING'
      IRET=1
      RETURN
997   WRITE(6,100) ' *** Error Reading: '// RQST_BUFFER(:FILENG(RQST_BUFFER))
      write(6,'(a,i5)') 'Read Error number (IOSTAT)=',ios
      WRITE(6,100) ' *** DATA FOR THE INPUT REQUEST IS INCOMPLETE'
      WRITE(6,100) ' *** THE VARIABLE IS NOT COPIED TO COMMON'
      WRITE(6,100) ' *** PROCESSING CONTINUING'
      nerr=nerr+1
      IRET=1
      if (funfmt.eq.0.and.nerr.lt.100) BACKSPACE FUNITI
      if (funfmt.eq.1.and.ios.eq.22) then
        inquire(unit=funiti,recl=length)
        write(6,'(a,i15)') 'max record length=',length
      endif
      RETURN
      END
!*********************************************************************
      SUBROUTINE FDICT(FUNITI,FUNITO)
! READS DICTIONARY FILE CONTAINING A LIST OF VARIABLES IN EACH COMMON BLOCK.
! THE LIST MUST BE IN THE SAME ORDER AS THE VARIABLES AS DECLARED IN COMMON.

      IMPLICIT NONE

      INTEGER*4 FUNITI,FUNITO
      include 'fdict'

!  DICTIONARY VARIABLES:
!  FMAXDT = PARAMETER FOR MAXIMUM NUMBER OF VARIABLES IN DICTIONARY.
!  FMAXCM = PARAMETER FOR MAXIMUM NUMBER OF COMMON BLOCKS IN DICTIONARY
!  FVARNM(FMAXDT) = VARIABLE NAME
!  FBLKNM(FMAXDT) = COMMON BLOCK NAME FOR EACH VARIABLE
!  FDESCR(FMAXDT) = VARIABLE DEFINITION
!  FVARTY(FMAXDT) = VARIABLE TYPE (I, R, C)
!  FVARLN(FMAXDT) = BYTE LENGTH OF THE VARIABLE TYPE (2 OR 4 FOR INTEGER,
!                   4 OR 8 FOR REAL, OR THE STRING LENGTH IF A CHARACTER VARIABLE
!  FNDIMS(FMAXDT) = NUMBER OF DIMENSIONS (0 TO 5).  0 FOR SCALAR.
!  FKDIM(5,FMAXDT)= RANK OF EACH DIMENSION, UP TO 5.  DEFAULT IS 1 FOR EACH
!  FVARSZ(FMAXDT) = COMPUTED BYTE LENGTH OF THE VARIABLE, GIVEN "VARLEN" AND
!                   (KDIM(I,),I=1,NDIMEN)
!  FBPOIN(FMAXDT) = COMPUTED STARTING BYTE LOCATION IN COMMON BLOCK
!  FNDICT         = TOTAL NUMBER OF DICTIONARY ENTRIES (MUST BE <=FMAXDT)
!  FINDEX(FMAXCM) = DICTIONARY INDEX TO START OF EACH COMMON BLOCK
!  FBLOCK(FMAXCM) = COMMON BLOCK NAMES CONTAINED IN DICTIONARY
!  FNVARS(FMAXCM) = NUMBER OF VARIABLES/ENTRIES IN EACH COMMON BLOCK
!  FBLKSZ(FMAXCM) = TOTAL SIZE OF EACH COMMON BLOCK IN BYTES. MUST
!                   AGREE WITH THE SIZE OF THE COMMON AS REPORTED IN THE LINK EDITOR MAP.
!  FNBLOC         = TOTAL NUMBER OF COMMON BLOCKS (MUST BE <= FMAXCM)
      LOGICAL*1  ENDDIM,ENDATT,ERRFLG
      INTEGER*4  RETCODE,I,J,K,KPROD,IB,ISUM1,ISUM2
      CHARACTER*1 COL1
      CHARACTER*6 OLDATTTYP
      CHARACTER OLDNAM*FMAXCHARCOM,LINE*165,OLDVAR*FMAXCHARVAR
      CHARACTER*31 BLANK,EQUAL  ! VARIABLES FOR FORMATTING
      DATA BLANK/'                               '/
      DATA EQUAL/'==============================='/
      ENDDIM = .FALSE.
      ENDATT = .FALSE.
      ERRFLG = .FALSE.
      FDIMINDEX = 0
      FATTINDEX = 0
      OLDATTTYP = '######'
101   FORMAT(A)

      FNBLOC=0
      OLDNAM='        '
      OLDVAR='######'
      FNDICT=1
      FVARNM=' '
      FBLKNM=' '
      FDESCR=' '
      FVARTY=' '
      FVARLN=0
      FNDIMS=0
      FKDIM=0
      FVARSZ=0
      FBPOIN=0
      FINDEX=0
      FBLOCK=' '
      FNVARS=0
      FBLKSZ=0
      count_str=0
  FDimNam(:) = ' '              ! ARRAY DIMENSION MNEMONIC (6 CHARS MAX)
  FDimSiz(:) = 0                ! ARRAY DIMENSION SIZE
  FDimDescrip(:) = ' '          ! ARRAY DIMENSION NAME (25 CHARS MAX)
  FDimStr_Index(:) = 0          ! Index to first dimension string in FDimStrs
  FDimStr_Long(:) = 0           ! length of longest string for this dimension
  FDimStrs(:)  = ' '            ! Dimension strings
  FDimStr(:,:) = ' '            ! Dimension strings for up to 5 dimensions
  FVarDim(:) = ' '              ! dimension mnemonics for each dimension
  FDimLong(:) = 0               ! Length of longest dimension string
  FAttNam(:,:) = ' '            ! 1ST ROW: ARRAY ATTRIBUTE MNEMONIC (FAttTyp)
  FAttDescrip(:) = ' '          ! ARRAY ATTRIBUTE NAME (25 CHARS MAX)
  FVarDims(:,:) = ' '           ! DIMENSION MNEMONICS FOR EACH VARIABLE
  FVarAtts(:,:) = ' '           ! ATTRIBUTE MNEMONICS FOR EACH VARIABLE
  FBlkAtts(:,:) = ' '           ! ATTRIBUTE TYPE MNEMONICS FOR EACH COMMON BLOCK
  FBlkDesc(:) = ' '             ! COMMON BLOCK DESCRIPTION


 10   CONTINUE
! READ A LINE FROM THE DICTIONARY FILE. DISCARD IF IT IS A COMMENT
        READ(FUNITI,101,END=40) LINE
        IF (LINE(1:1).NE.' ') GOTO 10
! CHECK FOR THE DIMENSION AND ATTRIBUTE RECORDS
! READ DIMENSION RECORDS
        IF (.NOT. ENDDIM) THEN
          CALL READDIM(LINE,RETCODE)
          IF(RETCODE .EQ. 1) THEN
            ENDDIM = .TRUE.
          ENDIF
          GOTO 10
        ENDIF
! READ ATTRIBUTE RECORDS
        IF (.NOT. ENDATT) THEN
          CALL READATT(LINE,OLDATTTYP,RETCODE)
          IF(RETCODE .EQ. 1) THEN
            ENDATT = .TRUE.
          ENDIF
          GOTO 10
        ENDIF

! READ THE USER-SPECIFIED DICTIONARY VARIABLES FROM THE PREVIOUSLY READ LINE
! IF COMMON BLOCK FIELD IS BLANK, USE PREVIOUS ENTRY.  THIS ALLOWS
! USER TO ENTER COMMON BLOCK NAME ONLY ONCE.  HERE IS THE FORMAT:
! (TWO FIELDS IN THE FILE MUST AGREE WITH THE CHARACTER VARIABLE
!  DECLARATIONS FOR COMMON BLOCK AND VARIABLE NAME.  THESE ARE
!  CONTROLLED BY PARAMETERS FMAXCHARCOM AND FMAXCHARVAR
!FBLKNM   FVARNM FVARTY
!:        :      : FVARLN
!:        :      : :    FNDIMS
!:        :      : :    :  FKDIM(I,),I=1,5)          FDESCR
!XXXXXXXX XXXXXX X XXXX X (XXXX,XXXX,XXXX,XXXX,XXXX) XXXXXX....XXXX
!
100   FORMAT(A1,A,1X,A,1X,A1,1X,I4,1X,I1,2X,9(A6,1X),A)


        READ(LINE,100) COL1,FBLKNM(FNDICT),FVARNM(FNDICT), &
          FVARTY(FNDICT),FVARLN(FNDICT),FNDIMS(FNDICT),    &
          (FVarDims(I,FNDICT),I=1,5),(FVarAtts(I,FNDICT),I=1,4), &
          FDESCR(FNDICT)
! IF COMMON FIELD BLANK, USE PREVIOUS VALUE.
        IF (FBLKNM(FNDICT) .EQ. '        ') FBLKNM(FNDICT) = OLDNAM
        IF (FVARNM(FNDICT) .NE. '        ') THEN
          ERRFLG = .FALSE.
          CALL FindDims(ERRFLG)
          ERRFLG = .FALSE.
          CALL FindAtts(ERRFLG)
        ENDIF


! CHECK FOR DUPLICATE VARIABLES (ARTIFACT OF OLD BUFVARS FILE)
        IF (FVARNM(FNDICT).EQ.OLDVAR) THEN
          WRITE(6,*) '*** DUPLICATE VARIABLE FOUND: ',OLDVAR
          WRITE(6,*) '*** WILL SKIP THAT ENTRY'
          GOTO 10
        ENDIF
        OLDVAR=FVARNM(FNDICT)

! IF NEW COMMON BLOCK, UPDATE COMMON BLOCK LIST. RESET POINTER.
        IF (FBLKNM(FNDICT) .NE. OLDNAM) THEN
          IF (FNBLOC .GE. 1) THEN
            FBLKSZ(FNBLOC)=FVARSZ(FNDICT-1)+FBPOIN(FNDICT-1)-1
          ENDIF
          FNBLOC=FNBLOC+1
          IF (FNBLOC .GT. FMAXCM) THEN
            WRITE(6,*) '*** DICTIONARY COMMON BLOCKS EXCEED MAXIMUM.'
            WRITE(6,*) '*** INCREASE PARAMETER FMAXCM AND RECOMPILE.'
            GOTO 40
          ENDIF
          FBLOCK(FNBLOC)=FBLKNM(FNDICT)
          OLDNAM=FBLKNM(FNDICT)
          FINDEX(FNBLOC)=FNDICT
          FBPOIN(FNDICT)=1
          FNVARS(FNBLOC)=0
          ERRFLG = .FALSE.
          CALL AddBlkAttrs(ERRFLG)
          GOTO 10
        ENDIF
        FNVARS(FNBLOC)=FNVARS(FNBLOC)+1
        IF (FNDIMS(FNDICT).EQ.0) THEN
          DO 20 K=1,5
            FKDIM(K,FNDICT)=1
20        CONTINUE
          FNDIMS(FNDICT)=1
        ENDIF
! COMPUTE SIZE OF THE VARIABLE IN BYTES, TAKING INTO ACCOUNT THE ARRAY
! DIMENSIONS AND THE UNIT SIZE OF THE VARIABLE
        KPROD=1
        DO 30 K=1,5
          IF (FKDIM(K,FNDICT).EQ.0) FKDIM(K,FNDICT)=1
          KPROD=KPROD*FKDIM(K,FNDICT)
30      CONTINUE
        FVARSZ(FNDICT)=KPROD*FVARLN(FNDICT)
!        IF (FVARSZ(FNDICT).GT.65536*4) THEN
!          WRITE(6,*)'*** AN ARRAY EXCEEDS MAX SIZE.'
!          WRITE(6,*)'*** VARIABLE IS: ',FVARNM(FNDICT)
!          GOTO 40
!        ENDIF
! SET BYTE POINTER FOR START OF VARIABLE WITHIN THE COMMON BLOCK
        IF (FNDICT.LT.FMAXDT) FBPOIN(FNDICT+1)= &
                             FBPOIN(FNDICT)+FVARSZ(FNDICT)

! INCREMENT THE COUNT OF VARIABLES
        FNDICT=FNDICT+1
        IF (FNDICT.EQ.FMAXDT) THEN
          WRITE(6,*) '*** DICTIONARY RECORDS EXCEED MAXIMUM.'
          WRITE(6,*) '*** INCREASE PARAMETER FMAXDT AND RECOMPILE.'
        ELSE
          GOTO 10
        ENDIF
40    CONTINUE
      FNDICT=FNDICT-1
      FBLKSZ(FNBLOC)=FVARSZ(FNDICT)+FBPOIN(FNDICT)-1

! SUMMARIZE THE DICTIONARY RESULTS
      WRITE(FUNITO,201) 'COMMON BLOCK DICTIONARY SUMMARY:'
      WRITE(FUNITO,200) 'NUMBER OF VARIABLES DEFINED....:',FNDICT
      WRITE(FUNITO,200) 'NUMBER OF COMMON BLOCKS DEFINED:',FNBLOC
200   FORMAT(1X,A,I5)
201   FORMAT(1X,A,A,10A)
202   FORMAT(1X,A,I6,I11,I13)
203   FORMAT(1X,A,6X,I11,I13)
      WRITE(FUNITO,201) EQUAL(:FMAXCHARCOM)  //            '================================'
      WRITE(FUNITO,201) 'COMMON ',BLANK(1:(FMAXCHARCOM-7)),'STARTING  NUMBER OF   TOTAL SIZE'
      WRITE(FUNITO,201) 'BLOCK  ',BLANK(1:(FMAXCHARCOM-7)),' INDEX    VARIABLES    OF BLOCK'
      WRITE(FUNITO,201) EQUAL(:FMAXCHARCOM)  //            '================================'
      ISUM1=0
      ISUM2=0
      DO 50 IB=1,FNBLOC
        WRITE(FUNITO,202) FBLOCK(IB),FINDEX(IB),FNVARS(IB),FBLKSZ(IB)
        ISUM1=ISUM1+FNVARS(IB)
        ISUM2=ISUM2+FBLKSZ(IB)
50    CONTINUE
      WRITE(FUNITO,201) EQUAL(:FMAXCHARCOM)  //            '================================'
      WRITE(FUNITO,203) 'SUMMARY'//BLANK(1:(FMAXCHARCOM-7)),ISUM1,ISUM2
      WRITE(FUNITO,201) EQUAL(:FMAXCHARCOM)  //            '================================'
      RETURN
      END
!********************************************************************
      SUBROUTINE READDIM(LINE,RETCODE)
!
! READ THE DIMENSION RECORDS FROM THE DATA DICTIONARY.
! SAVE DATA READ IN FDIMNAM, FDIMSIZ, FDIMDESCRIP

      IMPLICIT NONE

      INTEGER*4     RETCODE,J,SIZE,I,firstval,idim,L
      CHARACTER*(*) LINE
      CHARACTER*3   DIM,DIMD*1
      CHARACTER*6   NAME
      CHARACTER*25  DESCRIP,STRG
      character*10   istr
      LOGICAL*1     ErrFlag

      include'fdict'
      READ(line,100) DIM,DIMD
      if (DIMD.eq.' ') then
         READ(LINE,100) DIM,DIMD,NAME,SIZE,DESCRIP
      else
! Get strings for dimension elements by one of two methods
!  0:  fill strings based on numeric values starting with a given value, like 1990 for mnumyr
!      and name of dimension (1990_MNUMYR, 1991_MNUMYR...)
!  n:  identify the nth dimension element individually with a string
!  If DIMD record(s) omitted:  default strings created like 01..DIMNAM
!  FDimStrs(FMaxStrs)  = Dimension strings
!  FDimStr_Index(FMaxDim) = Index to first dimension string in FDimStrs
!  FDimStr_long(FMaxDim) = length of longest string for this dimension
         READ(LINE,'(1X,A3,A1,1X,A6,i6)') DIM,DIMD,NAME,I
         if (i.eq.0) then
           READ(LINE,'(BN,1X,A3,A1,1X,A6,i6,1x,i4)') DIM,DIMD,NAME,I, firstval
           idim=0
           do j=1,FDimIndex
             if (NAME.eq.FDimNam(j)) then
               idim=j
               exit
             endif
           enddo
           if (idim.eq.0) then
             errflag=.true.
             write(6,*) '***Dimension string not found: ',strg
             goto 998
           endif
           FDimStr_Index(idim)=count_str+1
           largest=FDimSiz(idim)+firstval-1
           FDimStr_Long(idim)=len_trim(NAME)+1+floor(log10(real(largest)))+1
           do j=1,FDimSiz(idim)
             istr=' '
             if (largest.le.9) then
               write(istr,'(i1)') firstval+j-1
             elseif (largest.le.99) then
               write(istr,'(i2.2)') firstval+j-1
             elseif (largest.le.999)then
               write(istr,'(i3.3)') firstval+j-1
             else
               write(istr,'(i4.4)') firstval+j-1
             endif
             count_str=count_str+1
             FDimStrS(count_str)=trim(istr)//'_'//trim(FDimNam(idim))
           enddo
         else
           READ(LINE,'(1X,A3,A1,1x,A6,I6,8x,A)') DIM,DIMD,NAME,I,STRG
 ! convert embedded blanks to underscore
           L=len_trim(strg)
           do j=1,L
             if (strg(j:j).eq.' ') strg(j:j)='_'
           enddo
 ! find dimension string in the list
           idim=0
           do j=1,FDimIndex
             if (NAME.eq.FDimNam(j)) then
               idim=j
               exit
             endif
           enddo
           if (idim.eq.0) then
             errflag=.true.
             write(6,*) '***Dimension string not found: ',strg
             goto 998
           endif
 ! add the dimension element to the list of strings and identify
 ! the starting location if on the first one.
           count_str=count_str+1
           if (i.eq.1) then
             FDimStr_Index(idim)=count_str
           endif
           if (i.le.FMaxDim) then
              istr=' '
             if (FDimSiz(idim).le.9) then
               write(istr,'(i1)') i
             elseif (FDimSiz(idim).le.99) then
               write(istr,'(i2.2)') i
             elseif (FDimSiz(idim).le.999) then
               write(istr,'(i3.3)') i
             else
               write(istr,'(i4.4)') i
             endif
            FDimStrs(count_str)=trim(istr)//'_'//trim(strg)
           endif
         endif

!  FDimStr(FMaxDims,5) = Dimension strings for up to 5 dimensions
      endif
100   FORMAT(1X,A3,A1,1X,A6,2X,I4,6X,A25)
      IF (NAME .EQ. 'ENDDIM') THEN
! create generic dimension element strings for those that have not been specified
        firstval=1
        do idim=1,FDimIndex
          if (FDimStr_Index(idim).eq.0.and.FDimSiz(idim).gt.0.and.FDimSiz(idim).le.FMaxDims.and.count_str.lt.FMaxStrs) then
           FDimStr_Index(idim)=count_str+1
           largest=FDimSiz(idim)+firstval-1
           do j=1,FDimSiz(idim)
             istr=' '
             if (largest.le.9) then
               write(istr,'(i1)') firstval+j-1
             elseif (largest.le.99) then
               write(istr,'(i2.2)') firstval+j-1
             elseif (largest.le.999)then
               write(istr,'(i3.3)') firstval+j-1
             else
               write(istr,'(i4.4)') firstval+j-1
             endif
             count_str=count_str+1
             FDimStrS(count_str)=trim(istr)//'_'//trim(FDimNam(idim))
           enddo
          elseif (FDimStr_Index(idim).eq.0) then
            !write(6,'(a,i4,a,i4,a,2i6)') 'Omitting dict.txt dim strings for '//trim(FDimNam(idim))//'=',&
            !  FDimSiz(idim),' FMaxDims=',FMaxDims,',  String count_str and FMaxStrs=',count_str,FMaxStrs
          endif

        enddo
        RETCODE = 1
        GOTO 999
      ENDIF
      IF (FDimINDEX .EQ. FMaxDim) THEN
        ErrFlag = .TRUE.
        WRITE(6,*) '***MAXIMUM NUMBER OF DIMENSIONS READ AT NAME: '// NAME
        RETCODE = 1
        GOTO 998
      ENDIF
      ErrFlag = .FALSE.
!      IF (FDimINDEX .EQ. 0) GOTO 998
      IF (DIM .NE. 'DIM') THEN
        ErrFlag = .TRUE.
        WRITE(6,*) '***DIMENSION RECORD SHOULD START WITH DIM, NOT '// DIM
        GOTO 998
      ENDIF
      IF (DIMD.EQ.' ') then
        J = 1
        DO WHILE(.NOT. ErrFlag .AND. J .LE. FDimINDEX)
          IF (J .GT. FMaxDim) THEN
            ErrFlag = .TRUE.
            WRITE(6,*) '***MAXIMUM NUMBER OF DIMENSIONS READ AT NAME: '// NAME
          ENDIF
          IF (FDimNam(J) .EQ. NAME) THEN
            ErrFlag = .TRUE.
            WRITE(6,*) '***DUPLICATE DIMENSION NAME '// NAME // ' FOUND.'
          ENDIF
          J = J+1
        END DO
      endif
998   IF (ErrFlag) THEN
        WRITE(6,*) '***WILL SKIP THAT ENTRY.'
      ELSE
        if (dimd.eq.'D')then
    !      write(6,*)' debug: not adding '//trim(NAME)//' again for subscript ', i
        else
          FDimINDEX = FDimINDEX+1
          FDimNam(FDimINDEX) = NAME
          FDimSiz(FDimINDEX) = SIZE
          FDimDescrip(FDimINDEX) = DESCRIP
        endif
        RETCODE = 0
      ENDIF
999   RETURN
      END
!*******************************************************************
      SUBROUTINE READATT(LINE,ATTTYP,RETCODE)
!
! READ THE ATTRIBUTE RECORDS FROM THE DATA DICTIONARY.
! SAVE DATA READ IN FATTNAM, FATTDESCRIP
!
      IMPLICIT NONE

      INTEGER*4     RETCODE,J
      CHARACTER*(*) LINE
      CHARACTER*1   L
      CHARACTER*3   ATT
      CHARACTER*6   ATTTYP,NAME
      CHARACTER*25  DESCRIP
      LOGICAL*1     ErrFlag

      include 'fdict'

      READ(LINE,100) ATT,L,NAME,DESCRIP
100   FORMAT(1X,A3,1X,A1,1X,A6,2X,A25)
      IF (NAME .EQ. 'ENDATT') THEN
        RETCODE = 1
        GOTO 999
      ENDIF
      IF (L .EQ. 'L') THEN
        ATTTYP = NAME
        RETCODE = 0
        GOTO 999
      ENDIF
      IF (FAttINDEX .EQ. FMaxAtt) THEN
        ErrFlag = .TRUE.
        WRITE(6,*) '***MAXIMUM NUMBER OF ATTRIBUTES READ AT NAME: '// NAME
        RETCODE = 1
        GOTO 998
      ENDIF
      ErrFlag = .FALSE.
!      IF (FAttINDEX .EQ. 0) GOTO 998
      IF (ATT .NE. 'ATT') THEN
        ErrFlag = .TRUE.
        WRITE(6,*) '*** ATTRIBUTE RECORD SHOULD START WITH ATT, NOT '// ATT
        GOTO 998
      ENDIF
      J = 1
      DO WHILE(.NOT. ErrFlag .AND. J .LE. FAttINDEX)
        IF (J .GT. FMaxAtt) THEN
          ErrFlag = .TRUE.
          WRITE(6,*) '*** MAXIMUM NUMBER OF ATTRIBUTES READ AT NAME: '// NAME
        ENDIF
        IF (FAttNam(1,J) .EQ. ATTTYP .AND. FAttNam(2,J) .EQ. NAME) THEN
          ErrFlag = .TRUE.
          WRITE(6,*) '*** DUPLICATE ATTRIBUTE TYPE/NAME '// ATTTYP // '/' // NAME // ' FOUND.'
        ENDIF
        J = J+1
      END DO
998   IF (ErrFlag) THEN
        WRITE(6,*) '*** WILL SKIP THAT ENTRY.'
      ELSE
        FAttINDEX = FAttINDEX+1
        FAttNam(1,FAttINDEX) = ATTTYP
        FAttNam(2,FAttINDEX) = NAME
        FAttDescrip(FAttINDEX) = DESCRIP
        RETCODE = 0
      ENDIF
999   RETURN
      END
!********************************************************************
      SUBROUTINE AddBlkAttrs(ERROR)
!
! COMPARE THE ATTRIBUTES FOR A GLOBAL COMMON BLOCK RECORD WITH THOSE
! PREVIOUSLY READ INTO FVARATTS.  IF FOUND, ASSIGN THE ATTRIBUTES TO
! FBLKATTS.  IF NOT FOUND, WRITE AN ERROR MESSAGE.

      IMPLICIT NONE

      include'fdict'

      INTEGER*4 J,K
      LOGICAL*1 ERROR,FOUND
      J = 1
      DO WHILE(J .LE. 5 .AND. FVarAtts(J,FNDICT) .NE. '      ')
        FOUND = .FALSE.
        K = 1
        DO WHILE(K .LE. FAttINDEX .AND. .NOT. FOUND)
          IF (FVarAtts(J,FNDICT) .EQ. FAttNam(1,K)) THEN
            FOUND = .TRUE.
          ELSE
            K = K+1
          ENDIF
        END DO
        IF (.NOT. FOUND) THEN
          WRITE(6,*) '*** COULD NOT FIND ATTRIBUTE: ',FVarAtts(J,FNDICT),' FOR BLOCK: ',FBLKNM(FNBLOC)
          ERROR = .TRUE.
          RETURN
        ENDIF
        J = J+1
      END DO
      J = J-1
      DO K = 1,J
        FBlkAtts(K,FNBLOC) = FVarAtts(K,FNDICT)
      END DO
! FILL UNUSED ATTRIBUTES WITH BLANKS
      DO K = J+1,5
        FBlkAtts(K,FNBLOC) = '      '
      END DO
      FBlkDesc(FNBLOC) = FDESCR(FNDICT)
      ERROR = .FALSE.
      RETURN
      END
!*******************************************************************
      SUBROUTINE FindDims(ERROR)
!
! COMPARE THE DIMENSION MNEMONICS FOR THIS GLOBAL VARIABLE TO THOSE
! READ IN EARLIER AND STORED IN FDIMNAM.  IF FOUND, ASSIGN THE STORED
! LENGTH (FDIMSIZ) TO THE FKDIM ARRAY INDEX FOR THIS DIMENSION OF
! THIS GLOBAL VARIABLE.  IF NOT FOUND, WRITE AN ERROR MESSAGE.

      IMPLICIT NONE

      include'fdict'

      INTEGER*4 J,K,ICONST,WIDTH(5),I,iwidth,last_str_pos
      LOGICAL*1 ERROR,FOUND
      character*6 a6

      width = 0
      DO J=1,FNDIMS(FNDICT)
        CALL DIMISNUM(FVARDIMS(J,FNDICT),ICONST)
        if (ICONST.EQ.1) then
          READ(FVARDIMS(J,FNDICT),'(BN,I6)') FKDIM(J,FNDICT)
          if (FKDIM(J,FNDICT).lt.10) then
             width(j)=1
          elseif (FKDIM(J,FNDICT).lt.100) then
             width(j)=2
          elseif (FKDIM(J,FNDICT).lt.1000) then
             width(j)=3
          elseif (FKDIM(J,FNDICT).lt.10000) then
             width(j)=4
          endif
          iwidth=width(j)
          write(a6,'(I<iwidth>)') FKDIM(J,FNDICT)
          FVARDIMS(J,FNDICT)='M'//trim(a6)
        endif
      ENDDO


      J = 1
      DO WHILE(J .LE. 5 .AND. FVarDims(J,FNDICT) .NE. '      ')
        FOUND = .FALSE.
        K = 1
        DO WHILE(K .LE. FDimINDEX .AND. .NOT. FOUND)
          IF (FVarDims(J,FNDICT) .EQ. FDimNam(K)) THEN
            FOUND = .TRUE.
            FKDIM(J,FNDICT) = FDimSiz(K)
          ELSE
            K = K+1
          ENDIF
        END DO
        IF (.NOT. FOUND) THEN
          IF (WIDTH(J) .EQ. 0) WRITE(6,'(a,I2,a,a)') '*** COULD NOT FIND DIMENSION #',J,' NAME: ',FVarDims(J,FNDICT)
          last_str_pos=FDimStr_Index(FDimIndex)+FDimSiz(FdimIndex)-1
          IF (FDimIndex.LT. FMaxDim.and.last_str_pos+min0(fkdim(j,fndict),FMaxDims-1).lt.FMaxStrs) then
            FDimIndex=FDimIndex+1
            FDimNam(FDimIndex)=FVarDims(J,FNDICT)
            FDimSiz(FDimIndex)=fkdim(j,fndict)
            FDimDescrip(FDimIndex)='Unnamed'
            FDimStr_Long(FDimIndex)=2*width(j)+4
            FDimStr_Index(FDimIndex)=last_str_pos+1
            do i=1,min0(FDimSiz(FDimIndex),FMaxDims-1)
              last_str_pos=last_str_pos+1
              write(FDimStrs(last_str_pos),'(i<width(j)>.<width(j)>,a)') i,'_'//trim(FDimNam(FDimIndex))
            enddo
          else
            WRITE(6,*) '*** Maximum dimension strings exceeded while adding generic dimension ', &
            FVarDims(J,FNDICT), ' last_str_pos=',last_str_pos
            ERROR = .TRUE.
            return
          endif
        ENDIF
        J = J+1
        if (j.eq.6) exit  ! added to prevent out-of-bounds on do while condition
      END DO
      DO K = J,5
        FVarDims(K,FNDICT) = '      '
      END DO
      ERROR = .FALSE.
      RETURN
      END
!*******************************************************************
      SUBROUTINE DIMISNUM(A,I)
! CHECKS CHARACTER FIELD A TO SEE IF IT CONSISTS ALL NUMBERS OR
! SPACES.  IF SO, RETURNS 1 IN INTEGER ARGUMENT I

      IMPLICIT NONE

      CHARACTER*(*) A
      INTEGER I,LA,J,FLEN

      CHARACTER NUMERAL*11/'0123456789 '/
      I=0
      LA=len_trim(A)
      IF (LA.EQ.0) RETURN
      DO J=1,LA
        IF (INDEX(NUMERAL,A(J:J)).EQ.0) RETURN
      ENDDO
      I=1
      RETURN
      END
!*******************************************************************
      SUBROUTINE FindAtts(ERROR)
!
! CHECK THE GLOBAL VARIABLE ATTRIBUTE MNEMONICS AGAINST THOSE
! READ EARLIER INTO THE FATTNAM GLOBAL.  THE FVARRATTS(J,FNDICT)
! AND FBLKATS(J,FNBLOC) PAIR FORM A LOGICAL RELATIONSHIP: LABEL AND
! VALUE.  THIS PAIR SHOULD CORRESPOND TO ONE OF THE FATTNAM(2,K) AND
! FATTNAM(1,K) PAIRS.  IF NOT, AN ERROR MESSAGE IS WRITTEN.

      IMPLICIT NONE

      include'fdict'

      INTEGER*4 J,K
      LOGICAL*1 ERROR,FOUND

      J = 1
      DO WHILE(J .LE. 5 .AND. FVarAtts(J,FNDICT) .NE. '      ')
        FOUND = .FALSE.
        K = 1
        DO WHILE(K .LE. FAttINDEX .AND. .NOT. FOUND)
          IF (FVarAtts(J,FNDICT) .EQ. FAttNam(2,K) .AND. &
              FBlkAtts(J,FNBLOC) .EQ. FAttNam(1,K)) THEN
            FOUND = .TRUE.
          ELSE
            K = K+1
          ENDIF
        END DO
        IF (.NOT. FOUND) THEN
          WRITE(6,*) '*** COULD NOT FIND ATTRIBUTE: ',FVarAtts(J,FNDICT),' FOR VARIABLE: ',FVARNM(FNDICT)
          ERROR = .TRUE.
          RETURN
        ENDIF
        J = J+1
        if (j.eq.6) exit  ! added to prevent out-of-bounds on do while condition
      END DO
      DO K = J,5
        FVarAtts(K,FNDICT) = '      '
      END DO
      ERROR = .FALSE.
      RETURN
      END
!*******************************************************************
      SUBROUTINE FCOP(COMON,ARRAY,NSIZE,IOPT)
! COPIES N REAL ELEMENTS FROM ADJUSTABLE ARRAY 1 TO ARRAY 2

      IMPLICIT NONE

      INTEGER*4   IOPT,I,NSIZE
      REAL*4      COMON(NSIZE)
      REAL*4      ARRAY(NSIZE)

      IF (IOPT.EQ.1)THEN
        DO 10 I=1,NSIZE
          ARRAY(I)=COMON(I)
10      CONTINUE
      ELSEIF (IOPT.EQ.2) THEN
        DO 20 I=1,NSIZE
          COMON(I)=ARRAY(I)
20      CONTINUE
      ENDIF
      RETURN
      END
      SUBROUTINE FCOPC(COMON,TARRAY,NSIZE,NLEN,IOPT)
! COPIES NSIZE character ELEMENTS of size nlen FROM ADJUSTABLE ARRAY COMON TO TARRAY, or vice versa

      IMPLICIT NONE

      INTEGER*4   IOPT,I,NSIZE,nlen
      CHARACTER*1 COMON(nlen,NSIZE)
      CHARACTER*1 TARRAY(nlen,NSIZE)

      IF (IOPT.EQ.1)THEN
        DO 10 I=1,NSIZE
          TARRAY(1:nlen,I)=COMON(1:nlen,I)
10      CONTINUE
      ELSEIF (IOPT.EQ.2) THEN
        DO 20 I=1,NSIZE
          COMON(1:nlen,I)=TARRAY(1:nlen,I)
20      CONTINUE
      ENDIF
      RETURN
      END

      FUNCTION FILENG(A)
       IMPLICIT NONE
!     FINDS POSITION OF LAST NON-BLANK CHARACTER IN CHARACTER VARIABLE A
      CHARACTER A*(*)
      INTEGER FILENG,I,N
      N=LEN(A)
      FILENG=0
      DO 10 I=N,1,-1
        IF (A(I:I).NE.' ') THEN
          FILENG=I
          RETURN
        ENDIF
   10 CONTINUE
      RETURN
      END

!********************************************************************
      SUBROUTINE FCOPA(LABEL,ARRAY,NSIZE,TARRAY,NSIZEC,VARLN,IPOINT,IOPT,IRET)
! COPIES AN ARRAY TO/FROM COMMON STORAGE TO A GENERIC ARRAY FOR USE WITH
! THE FILER I/O ROUTINES.  THE ARGUMENTS OF THE FUNCTIONS ARE ALL INPUTS:
!   LABEL =INDICATES WHAT COMMON BLOCK THE VARIABLE TO BE COPIED IS
!   ARRAY =GENERIC VARIABLE USED TO HOLD THE CONTENTS OF THE ARRAY FOR COPYING
!   NSIZE =THE NUMBER OF ELEMENTS IN THE ARRAY (OR WORD SIZE)
!   IPOINT=THE WORD POSITION IN THE COMMON BLOCK HOLDING THE ARRAY TO BE COPIED
!   IOPT=TYPE OF COPY--1: COPY FROM COMMON TO THE GENERIC ARRAY
!                    --2: COPY FROM THE GENERIC ARRAY BACK TO COMMON

      IMPLICIT NONE

      INTEGER*4 NSIZE,NSIZEC,VARLN,IOPT,IRET,IPOINT
      REAL ARRAY(NSIZE)
      CHARACTER*1 TARRAY(varln,NSIZEC)
      CHARACTER*(*) LABEL
!  THE FOLLOWING ARE THE INCLUDE FILES THAT ARE TO CONTAIN
!  THE NAMES OF THE INCLUDE FILES INCLUDED FOR THIS APPLICATION.
      include'parametr'
      include'emmparm'
      include'qblk'
      include'mpblk'
      include'mxqblk'
      include'mxpblk'
      include'qsblk'
      include'ncntrl'
      include'cdsparms'
      include'coalout'
      include'coalrep'
      include'cogen'
      include'efpout'
      include'uefpout'
      include'uefdout'
      include'udatout'
      include'uecpout'
      include'dsmdimen'
      include'dsmtfefp'
      include'uldsmout'
      include'uettout'
      include'emission'
      include'emablk'
      include'indout'
      include'indrep'
      include'intout'
      include'angtdm'
      include'coalprc'
      include 'coalemm'
      include 'pqchar' !(for parameter mnoth, used in include converge)
      include 'converge'
      include 'qonroad'
      include 'ponroad'
      include 'eusprc'


! BRANCH ON THE NAME OF THE COMMON BLOCK.  THIS BRANCHING IS THE CODE
! THAT MUST BE CUSTOMIZED FOR CHANGING COMMON BLOCKS AND FOR DIFFERENT
! APPLICATIONS.  FIRST FCOP ARGUMENT IS A SINGLE DIMENSION ARRAY EQUIVALENCED
! TO THE START OF THE COMMON BLOCK AND EQUAL TO THE LENGTH OF THE COMMON BLOCK

      IF (LABEL.EQ.'QBLK') THEN
             CALL FCOP(MQARRY(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'MPBLK') THEN
             CALL FCOP(MPARRY(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'PMORE') THEN
             CALL FCOP(PMOREQ(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'QMORE') THEN
             CALL FCOP(QMOREQ(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'MXQBLK') THEN
             CALL FCOP(MXQARRY(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'MXPBLK') THEN
             CALL FCOP(MXPARRY(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'QSBLK') THEN
             CALL FCOP(MQSARY(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'NCNTRL') THEN
             CALL FCOP(NCNTRL_EQUIV(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'CYCLEINFO') THEN
             CALL FCOP(CYC_INF_EQ(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.eq.'NCHAR') then
             CALL FCOPC(NCHAR_EQ(IPOINT),TARRAY,NSIZEC,VARLN,IOPT)
      ELSEIF (LABEL.EQ.'COALOUT') THEN
             CALL FCOP(EQ_CLOUT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'COALREP') THEN
             CALL FCOP(EQ_CLREP(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'COGEN') THEN
             CALL FCOP(EQ_CGOUT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'EFPOUT') THEN
             CALL FCOP(EFPDAT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'UEFPOUT') THEN
             CALL FCOP(UEFPDAT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'UEFDOUT') THEN
             CALL FCOP(UEFDDAT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'UDATOUT') THEN
             CALL FCOP(UDATDAT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'UECPOUT') THEN
             CALL FCOP(UECPDAT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'DSMTFEFP') THEN
             CALL FCOP(DSMEFDAT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'ULDSMOUT') THEN
             CALL FCOP(ULDSMDAT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'UETTOUT') THEN
             CALL FCOP(UETTDAT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'EMISSION') THEN
             CALL FCOP(EQ_EMOUT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'EMABLK') THEN
             CALL FCOP(EQ_EAOUT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'INDOUT') THEN
             CALL FCOP(EQ_INOUT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'INDREP') THEN
             CALL FCOP(EQ_INREP(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'INDREP2') THEN
             CALL FCOP(EQ_INREP2(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'INTOUT') THEN
             CALL FCOP(EQ_ITOUT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'ANGTDM') THEN
             CALL FCOP(EQ_NTOUT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'COALPRC') THEN
             CALL FCOP(CLPRCOUT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'COALEMM') THEN
             if (NSIZEC.LE.1) then
                CALL FCOP( EMMCLOUT(IPOINT), ARRAY,NSIZE ,IOPT)
             elseif (NSIZEC.GT.1) then
                CALL FCOPC(EMMCLOUT(IPOINT),TARRAY,NSIZEC,VARLN,IOPT)
             endif
      ELSEIF (LABEL.EQ.'CONVERGE') THEN
             CALL FCOP(EQ_CONVERGE(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'QONROAD') THEN
             CALL FCOP(OQARRY(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'PONROAD') THEN
             CALL FCOP(OPARRY(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'EUSPRC') THEN
             CALL FCOP(PELOUT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSE
             CALL FCOPB(LABEL,ARRAY,NSIZE,TARRAY,NSIZEC,VARLN,IPOINT,IOPT,IRET)
      ENDIF
      RETURN
      END
!********************************************************************
      SUBROUTINE FCOPB(LABEL,ARRAY,NSIZE,TARRAY,NSIZEC,VARLN,IPOINT,IOPT,IRET)
! COPIES AN ARRAY TO/FROM COMMON STORAGE TO A GENERIC ARRAY FOR USE WITH
! THE FILER I/O ROUTINES.  THE ARGUMENTS OF THE FUNCTIONS ARE ALL INPUTS:
!   LABEL =INDICATES WHAT COMMON BLOCK THE VARIABLE TO BE COPIED IS
!   ARRAY =GENERIC VARIABLE USED TO HOLD THE CONTENTS OF THE ARRAY FOR COPYING
!   NSIZE =THE NUMBER OF ELEMENTS IN THE ARRAY (OR WORD SIZE)
!   IPOINT=THE WORD POSITION IN THE COMMON BLOCK HOLDING THE ARRAY TO BE COPIED
!   IOPT=TYPE OF COPY--1: COPY FROM COMMON TO THE GENERIC ARRAY
!                    --2: COPY FROM THE GENERIC ARRAY BACK TO COMMON

      IMPLICIT NONE

      INTEGER*4 NSIZE,NSIZEC,VARLN,IPOINT,IOPT,IRET
      REAL ARRAY(NSIZE)
      CHARACTER*(VARLN) TARRAY(NSIZEC)
      CHARACTER*(*) LABEL
!  THE FOLLOWING ARE THE INCLUDE FILES THAT ARE TO CONTAIN
!  THE NAMES OF THE INCLUDE FILES INCLUDED FOR THIS APPLICATION.
      include 'parametr'
      include 'resdrep'
      include 'rscon'
      include 'rseff'
      include 'tranrep'
      include 'comparm'
      include 'commrep'
      include 'macout'
      include 'mcdetail'
      include 'ngtdmout'
      include 'ngtdmrep'
      include 'ngrpt'
      include 'ogsmout'
      include 'lfmmout'
      include 'pmmout'
      include 'pmmrpt'
      include 'pmmftab'
      include 'emmparm'
      include 'wrenew'
      include 'convfact'
      include 'ampblk'
      include 'acoalprc'
      include 'emeblk'
      include 'cdsparms'
      include 'uso2grp'
      include 'bldglrn'
      include 'bifurc'
      include 'epmbank'
      include 'ghgrep'
      include 'aponroad'
      include 'hmmblk'
      include 'aeusprc'
      include 'continew'
      include 'ab32'
      include 'rggi'
      include 'csapr'
      include 'e111d'
      include 'tcs45q'

! BRANCH ON THE NAME OF THE COMMON BLOCK.  THIS BRANCHING IS THE CODE
! THAT MUST BE CUSTOMIZED FOR CHANGING COMMON BLOCKS AND FOR DIFFERENT
! APPLICATIONS.  FIRST FCOP ARGUMENT IS A SINGLE DIMENSION ARRAY EQUIVALENCED
! TO THE START OF THE COMMON BLOCK AND EQUAL TO THE LENGTH OF THE COMMON BLOCK

      IF (LABEL.EQ.'RESDREP') THEN
             CALL FCOP(EQ_RSREP(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'RSCON') THEN
             CALL FCOP(EQ_RSCON(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'RSEFF') THEN
             CALL FCOP(EQ_RSEFF(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'TRANREP') THEN
             CALL FCOP(EQ_TRREP(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'COMPARM') THEN
             CALL FCOP(EQ_CMPRM(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'COMMREP') THEN
             CALL FCOP(EQ_CMREP(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'MACOUT') THEN
             CALL FCOP(EQ_MCOUT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'MCDETAIL') THEN
             CALL FCOP(EQ_MCDTL(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'NGTDMOUT') THEN
             CALL FCOP(EQ_NTOUT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'NGTDMREP') THEN
             CALL FCOP(EQ_NTREP(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'NGRPT') THEN
             CALL FCOP(EQ_NGRPT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'OGSMOUT') THEN
             CALL FCOP(EQ_OGOUT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'LFMMOUT') THEN
             CALL FCOP(EQ_LFMMOUT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'PMMOUT') THEN
             CALL FCOP(EQ_RFOUT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'PMMRPT') THEN
             CALL FCOP(EQ_RFRPT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'PMMFTAB') THEN
             CALL FCOP(EQ_RFFTAB(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'WRENEW') THEN
             CALL FCOP(WRARRY(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'CONVFACT') THEN
             CALL FCOP(EQ_CFOUT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'AMPBLK') THEN
             CALL FCOP(MPARRY(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'APMORE') THEN
             CALL FCOP(PMOREQ(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'ACOALPRC') THEN
             CALL FCOP(CLPRCOUT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'AEUSPRC') THEN
             CALL FCOP(PELOUT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'EMEBLK') THEN
             CALL FCOP(EQ_EMEBLK(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'USO2GRP') THEN
             CALL FCOP(EQ_SO2GRP(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'BldgLrn') THEN
             CALL FCOP(EQ_BldgLrn(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'BIFURC') THEN
             CALL FCOP(EQ_BIFURC(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'EPMBANK') THEN
             CALL FCOP(EQ_EPMBANK(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'GHGREP') THEN
             CALL FCOP(EQ_GHGREP(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'APONROAD') THEN
             CALL FCOP(OPARRY(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'HMMBLK') THEN
             CALL FCOP(HMARRY(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'REGCO2') THEN
             CALL FCOP(EQ_REGCO2(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'CONTINEW') THEN
             CALL FCOP(EQ_CONT(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'AB32') THEN
             CALL FCOP(EQ_AB32(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'RGGI') THEN
             CALL FCOP(EQ_RGGI(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'CSAPR') THEN
             CALL FCOP(EQ_CSAPR(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'E111D') THEN
             CALL FCOP(EQ_E111D(IPOINT),ARRAY,NSIZE,IOPT)
      ELSEIF (LABEL.EQ.'TCS45Q') THEN
             CALL FCOP(TCS45QEQ(IPOINT),ARRAY,NSIZE,IOPT)
       ELSE
             IRET=1
      ENDIF
      RETURN
      END
      subroutine FFill_Strs(BLOCKNAM,VARNAM,IVAR)
      ! fills dimension labeling strings for a given array variable.  the strings are stored in
      ! FDimStr(FMaxDims,5)  up to 5 dimensions for any array
      implicit none
      include 'fdict'
      character*(*) blocknam  ! common block label (because variables can appear under more than one common)
      character*(*) varnam    ! variable name
      integer NSize
      integer j,i1,idim,iblock,ivar,icom,istart,iend,iv,iwidth,chkdim
      character*6 DimNam


! find variable name in dictionary.
! if common block name omitted, assume variable name is unique across common blocks.
! if variable listed in more than one common block, should specify common block label, too.
    if (ivar.eq.0) ivar=1
    IF (VARNAM.NE.FVARNM(IVAR))THEN
      iblock=0
      ivar=0
      if (len_trim(blocknam).gt.0) then
        do icom=1,FNBLOC
          if (blocknam.eq.FBLOCK(icom)) then
            iblock=icom
            exit
          endif
        enddo
      endif
      if (iblock.gt.0) then
        ISTART=FINDEX(IBLOCK)
        IEND=FINDEX(IBLOCK)+FNVARS(IBLOCK)-1
        DO IV=ISTART,IEND
          IF (VARNAM.EQ.FVARNM(IV)) THEN
            IVAR=IV
            EXIT
          ENDIF
        ENDDO
      endif
      if (ivar.eq.0) then
        DO IV=1,FNDICT
          IF (VARNAM.EQ.FVARNM(IV)) THEN
            IVAR=IV
            EXIT
          ENDIF
        ENDDO
      endif
      if (ivar.eq.0)then
        return
      endif
    endif

! check dimension count. Sometimes dictionary has the wrong number
    chkdim=0
    do j=1,5
      if (fkdim(j,ivar).gt.1) then
        chkdim=chkdim+1
      endif
    enddo
    if (chkdim.gt.fndims(ivar)) then
      fndims(ivar)=chkdim
    endif

! fill in names of elements in each dimension

      do j=1,fndims(ivar) ! number or dimensions
         DimNam=FVarDims(j,ivar)
         if (FVarDim(j).ne.DimNam.or.FDimStr(1,j).eq.' '.or.index('D1D2D3D4D5',trim(dimnam)).gt.0) then ! if not already done, fill up this dimension with strings
           FDimLong(j)=0
           FDimStr(:,j)=' '
           idim=0
           do i1=1,FDimIndex
             if (DimNam.eq.FDimNam(i1)) then
               idim=i1
               exit
             endif
           enddo
           if (idim.ne.0) then
             FVarDim(j)=DimNam

             iwidth=1
             if (fkdim(j,ivar).gt.0) iwidth=floor(log10(real(fkdim(j,ivar))))+1
             do i1=1,FKDIM(j,ivar) ! rank of each dimension
               if (FDimStr_Index(idim).gt.0.and.i1.le.FMaxDims) then
                 FDimStr(i1,j)=FDimStrs(FDimStr_Index(idim)+i1-1)
               else
                 if (i1.lt.FMaxDims) then
                    write(FDimStr(i1,j),'(i<iwidth>.<iwidth>)') i1
                 endif
               endif
               FDimLong(j)=max(FDimLong(j),len_trim(FDimstr(min(i1,FMaxDims),j)))
             enddo
           else
             FVarDim(j)=DimNam
             iwidth=1
             if (fkdim(j,ivar).gt.0) iwidth=floor(log10(real(fkdim(j,ivar))))+1
             do i1=1,FKDIM(j,ivar) ! rank of each dimension
               if (i1.lt.FMaxDims)then
                  write(FDimStr(i1,j),'(i<iwidth>.<iwidth>)') i1
               endif
               FDimLong(j)=max(FDimLong(j),len_trim(FDimstr(min(i1,FMaxDims),j)))
             enddo
           endif
         endif
      enddo
      do j=fndims(ivar)+1,5
        FDimLong(j)=0
        FDimstr(:,j)=' '
      enddo
!      write(6,'(11a)')'DEFS ', blocknam, ' ',varnam,('  ',trim(FVarDims(j,ivar)),j=1,fndims(ivar))
!             do i1=1,FKDIM(j,ivar) ! rank of each dimension
!                   write(6,'(11a)')'DEFL ', varnam,('  ',trim(FDimStr(i1,j)),j=1,fndims(ivar))
!             enddo

!      write(6,'(11a)') varnam,('  ',trim(FDimStr(1,j)),j=1,fndims(ivar))

      return
      end subroutine FFill_Strs

      subroutine FFillLong(a,n,L)
! Fills character array a(n) with strings of length L
      implicit none
      integer n,L
      character*(*) a(n)
      integer i
      do i=1,n
         write(a(i),'(i<L>.<L>)') i
      enddo
      a(n+1:)=' '
      return
      end subroutine FFillLong
!sets:     call WriteGDXElement(pgdx,0,     FdimNam(I)                  ,FDimDescrip(I),1,    FDimSiz(I),   1,            1,            1,            1,             fdimstr(1,j),atemp,       atemp,       atemp,       atemp,        RORI, FVarDim )
!elem:     call WriteGDXElement(pgdx,1,     TRIM(QLABEL)//'_'//TRIM(VAR),DEFN,          ARRAY,FKDIM(1,IVAR),FKDIM(2,IVAR),FKDIM(3,IVAR),FKDIM(4,IVAR),FKDIM(5,IVAR), FDimStr(1,1),FDimStr(1,2),FDimStr(1,3),FDimStr(1,4),FDimStr(1,5), RORI, FVarDim )
!    SUBROUTINE WriteGDXElement(pgdx,dtype, id,                          expltext,      a,    n,            o,            p,            q,            r,             label1,      label2,      label3,      label4,      label5,       RORI, idd)


SUBROUTINE WriteGDXElement_domains(pgdx, dtype, id, expltext, a, n, o, p, q, r, label1, label2, label3, label4, label5, RORI,idd)
!This routine writes a matrix to a .GDX file.  If it is a parameter, sets the domains (set names) for each dimension
!dtype (0=set,1=parameter,2=table)
        implicit none
        include 'gdxiface'
        include 'gamsglobalsf'
        integer n, o, p, q, r, dimensions    !dimension variables and dimension counter
        integer ok, i, j, k, l, m            !integers for dimension do loops
        real a(n,o,p,q,r)
        integer*8 pgdx
        character*1 RorI
        character(len=*) id, expltext
        character(len=*) label1(n), label2(o), label3(p), label4(q), label5(r)
        character(len=*):: idd(5) ! set names for each dimension of a parametr

        character*16 mName
        character*uel_ident_len elements(max_index_dim)
        real*8 vals(5)
        integer*4 ival
        real*4 rval
        equivalence(rval,ival)
        integer dtype
        CHARACTER(LEN=256) :: S
        dimensions = 0


        !determine dimensionality from the 5 dimension variables (n,o,p,q, and r)
        if(n.gt.1) dimensions = 1
        if(o.gt.1) dimensions = 2
        if(p.gt.1) dimensions = 3
        if(q.gt.1) dimensions = 4
        if(r.gt.1) dimensions = 5



        ok = GDXDataWriteStrStart(pgdx, id, expltext, dimensions, dtype, 0)

        if (ok==0) then
          ok=gdxErrorStr(pgdx,gdxGetLastError(pgdx),s)
          write(6,'(a)') 'Error: '//trim(s)//' '//trim(id)
          WRITE(6,'(a)') 'Error: in subroutine WriteGDXElement'
          write(6,'(a)') 'Error: check for duplications in the varlist file.'
!!!          stop 'GDXDataWriteStrStart'
          return
        endif
! if this is a parameter, define the domain of the set for each index position or dimension
        if (dtype.eq.1 ) then
          ok = gdxSymbolSetDomain(pgdx, idd(1:dimensions)) ! define the domain of each indexing set
          if (ok==0) then
             ok=gdxErrorStr(pgdx,gdxGetLastError(pgdx),s)
!            write(6,'(a,5(a,1x))') 'Error: '//trim(s)//' when processing '//trim(id)//' for domains: ',idd(1:dimensions)
          endif
        endif
        do m=1,r
          elements(5) = label5(m)
          do l=1,q
            elements(4) = label4(l)
            do k=1,p
              elements(3) = label3(k)
              do j=1,o
                elements(2) = label2(j)
                do i=1,n
                  elements(1)=label1(i)
                  vals(1)=0
                  if(dtype.NE.0) then
                    vals(1) = a(i,j,k,l,m)
                    if (RorI.eq.'I') then
                      rval = a(i,j,k,l,m) ! rval, ival equivalenced.  ival is integer
                      vals(1)=real(ival)
                    endif
                  endif
                  if (vals(1).NE.0.OR.dtype.EQ.0)then  ! This skips 0 values.  the readgdxelement routine may need placeholders, so go ahead and write them out
                    ok = GDXDataWriteStr(pgdx,elements,vals)
                  endif
                end do
              end do
            end do
          end do
        end do
        ok = GDXDataWriteDone(pgdx)
        if (ok==0) then
          stop 'GDXDataWriteDone'
        endif
END
SUBROUTINE WriteGDXElement(pgdx, dtype, id, expltext, a, n, o, p, q, r, label1, label2, label3, label4, label5, RORI)
!This routine writes a matrix to a .GDX file.  This version omits domain name definition for each matrix dimension
!dtype (0=set,1=parameter,2=table)
        implicit none
        include 'gdxiface'
        include 'gamsglobalsf'
        integer n, o, p, q, r, dimensions    !dimension variables and dimension counter
        integer ok, i, j, k, l, m            !integers for dimension do loops
        real a(n,o,p,q,r)
        integer*8 pgdx
        character*1 RorI
        character(len=*) id, expltext
        character(len=*) label1(n), label2(o), label3(p), label4(q), label5(r)

        character*16 mName
        character*uel_ident_len elements(max_index_dim)
        real*8 vals(5)
        integer*4 ival
        real*4 rval
        equivalence(rval,ival)
        integer dtype
        CHARACTER(LEN=256) :: S
        dimensions = 0


        !determine dimensionality from the 5 dimension variables (n,o,p,q, and r)
        if(n.gt.1) dimensions = 1
        if(o.gt.1) dimensions = 2
        if(p.gt.1) dimensions = 3
        if(q.gt.1) dimensions = 4
        if(r.gt.1) dimensions = 5



        ok = GDXDataWriteStrStart(pgdx, id, expltext, dimensions, dtype, 0)

        if(ok==0) then
          ok=gdxErrorStr(pgdx,gdxGetLastError(pgdx),s)
          write(6,*) 'Error: '//trim(s)
          WRITE(6,*) 'Error: WriteGDXElement is bombing before the nested loops.'
          write(6,*) 'Error: One time this happened when attempting to write a variable a second time.'
          write(6,*) 'Error: So check for duplications in the varlist file.'
          stop 'GDXDataWriteStrStart'
        endif

        do m=1,r
          elements(5) = label5(m)
          do l=1,q
            elements(4) = label4(l)
            do k=1,p
              elements(3) = label3(k)
              do j=1,o
                elements(2) = label2(j)
                do i=1,n
                  elements(1)=label1(i)
                  vals(1)=0
                  if(dtype.NE.0) then
                    vals(1) = a(i,j,k,l,m)
                    if(RorI.eq.'I') then
                      rval = a(i,j,k,l,m) ! rval, ival equivalenced.  ival is integer
                      vals(1)=real(ival)
                    endif
                  endif
                  if(vals(1).NE.0.OR.dtype.EQ.0)then  ! This skips 0 values.  the readgdxelement routine may need placeholders, so go ahead and write them out
                    ok = GDXDataWriteStr(pgdx,elements,vals)
                  endif
                end do
              end do
            end do
          end do
        end do
        ok = GDXDataWriteDone(pgdx)
        if(ok==0) then
          stop 'GDXDataWriteDone'
        endif
END

SUBROUTINE ReadGDXElement(pgdx, dtype, id, expltext, a, n, o, p, q, r, label1, label2, label3, label4, label5, RorI)
!This routine reads a matrix from a .GDX file using strings to identify array elements
!dtype (0=set,1=parameter,2=table)
        implicit none
!DSA!         use gdxf9def
        include 'gdxiface'
!DSA!      use gamsglobals
        include 'gamsglobalsf'
        integer n, o, p, q, r, dimensions    !dimension variables and dimension counter
        integer ok, i, j, k, l, m            !integers for dimension do loops
        integer ii,i2,i3,i4,i5
        real a(n,o,p,q,r)


        integer*8 pgdx
        character*1 RorI
        character(len=*) id, expltext
        character(len=*) label1(n), label2(o), label3(p), label4(q), label5(r)
        character*16 mName
        character*uel_ident_len elements(max_index_dim)
        real*8 vals(5)
        real*4 rval*4
        integer*4 ival
        equivalence(rval,ival)
        integer dtype,ReadSyNr,ReadDim,UserInfo,NrRecs,Afdim
        integer imissing

        dimensions = 0

        !determine dimensionality from the 5 dimension variables (n,o,p,q, and r)
        if(n.gt.1) dimensions = 1
        if(o.gt.1) dimensions = 2
        if(p.gt.1) dimensions = 3
        if(q.gt.1) dimensions = 4
        if(r.gt.1) dimensions = 5

        ok = gdxfindsymbol(Pgdx,ID,ReadSyNr)
        if (.not.ok) then
       !   WRITE(6,*) 'Error finding set: ',ID
          return
        endif


        ok = gdxsymbolinfo(Pgdx,ReadSynr,ExplText,ReadDim,UserInfo)

        if (.not.ok) then
          WRITE(6,*) 'Error finding variable info for '//trim(id)//' ReadSyNr=',ReadSyNr
          return
        endif
        if (ReadDim.ne.dimensions) then
          write(6,*)' Dimensions for variable '//trim(id)//' in GDX do not match filer dictionary.',readdim,dimensions
          return
        endif
        ok = GDXDataReadStrStart(pgdx, ReadSynr, NrRecs)
        if (ok==0) then
          WRITE(6,*) 'Error reading '//trim(id)//', GDXDataReadStart not ok. Skipping.'
          return
        endif

        a = 0
        do ii=1,5
           vals(ii) = 0.0
        enddo

        m=1
        l=1
        k=1
        j=1
        i=1
        do while (GDXDataReadStr(pgdx,elements,vals,afdim)) ! read the available values, returning the indexing string elements
          imissing=0
          if (dimensions.gt.0) call GDXGetSubscript(elements(1),label1(1),n,i,1,imissing)
          if (dimensions.gt.1) call GDXGetSubscript(elements(2),label2(1),o,j,2,imissing)
          if (dimensions.gt.2) call GDXGetSubscript(elements(3),label3(1),p,k,3,imissing)
          if (dimensions.gt.3) call GDXGetSubscript(elements(4),label4(1),q,l,4,imissing)
          if (dimensions.gt.4) call GDXGetSubscript(elements(5),label5(1),r,m,5,imissing)

          if (imissing.eq.0) then
            a(i,j,k,l,m)=vals(1)
            if (RorI.eq.'I')then
              ival=int(vals(1))
              a(i,j,k,l,m)=rval
            endif
          else
             write(6,*)' Could not find subscript for '//trim(elements(imissing)),' dimension#=',imissing
          endif
        enddo

        if (.not. gdxDataReadDone(pgdx)) then
          write(6,*)' error reading array element '//trim(id)
         ! call ReportGDXError(pgdx)
        endif
        return
END
subroutine GDXGetSubscript(element,label,n,i,idim,imissing)
   implicit none
! search for the element among the N strings in LABEL, and return the index position I.
! Start the search with index I, then increment I until reaching N.  Then start looking at index 1.
! Do up to N searches.

   character(len=*) element
   integer i,n,idim
   character(len=*) label(n)
   integer imissing
   integer cnt
! starting from current position, search for each element string among the dimension labels.
   cnt=1
   do while (label(i).ne.element .and. cnt.le.n)
     cnt=cnt+1
     i=i+1
     if (i.gt.n) i=1
   enddo
   if (cnt.gt.n) then
     imissing=idim
   endif
   return
   end

!*******************************************************************

      subroutine removechar(a,remove)
      implicit none
      character*(*) a
      character*1 remove
      integer i
      do i=1,len_trim(a)
        if (a(i:i).eq.remove) then
          a(i:)=a(i+1:)
        endif
      enddo
      return
      end

!*******************************************************************
      SUBROUTINE FGROWIT(ARRAY,N1,N2,N3,N4,N5,DIM,YEARDIM,fgrow)
      IMPLICIT NONE
! extrapolate the array over
      INTEGER(kind=4)     N1,N2,N3,N4,N5
      INTEGER(kind=4)     DIM(5,2),YEARDIM
      REAL(kind=4)        ARRAY(N1,N2,N3,N4,N5)
      REAL(kind=4) fgrow


      INTEGER(kind=4)     I5,I4,I3,I2,I1
      INTEGER(kind=4)     J5,J4,J3,J2,J1

      do I1=dim(1,1),dim(1,2)
        j1=i1
        if (yeardim.eq.1) j1=i1-1

        do i2=dim(2,1),dim(2,2)
          j2=i2
          if (yeardim.eq.2) j2=i2-1

          do i3=dim(3,1),dim(3,2)
            j3=i3
            if (yeardim.eq.3) j3=i3-1

            do i4=dim(4,1),dim(4,2)
              j4=i4
              if (yeardim.eq.4) j4=i4-1

              do i5=dim(5,1),dim(5,2)
                j5=i5
                if (yeardim.eq.5) j5=i5-1


                ARRAY(I1,I2,I3,I4,I5)= array(j1,j2,j3,j4,j5)*fgrow

              enddo
            enddo
          enddo
        enddo
      enddo

      return
      end

!*******************************************************************

      SUBROUTINE FToUpper(A)
!     CONVERTS STRING A TO UPPER CASE
      CHARACTER*(*) A
      integer i, j, n
      n=len_trim(a)
      DO 10 I=1,N
        J=ICHAR(A(I:I))
        IF (J.GT.96.AND.J.LT.123) A(I:I)=CHAR(J-32)
   10 CONTINUE
      RETURN
      END

