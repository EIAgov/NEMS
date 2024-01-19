
! $Header: M:/default/scripts/RCS/make_aimms_gds.f90,v 1.3 2015/10/22 15:23:04 DSA Exp $
      PROGRAM Make_Aimms_GDS
      implicit none

!   READS NEMS Global Data Structure from dict.txt.  Reads an AIMMS 4 Model file (.ams).
!   Writes a new .ams file, replacing the AIMMS Global Data Section with one
!   created from the NEMS Dictionary 

! (Code started with tfiler.f and filer.f, so just ignore unrelated stuff)

!
      INTEGER*4 FUNITI,FUNITO,FRETCD,FRTYPE,FSOURC,funfmt,stat,fmt, &
       infmt,I,L
      integer*4 leng
      external leng
      CHARACTER*11 trash
      CHARACTER*60 dict,restarti,var,restarto
      character*50 blnkfile
      CHARACTER*100 FNAMEI,FNAMEO
      CHARACTER ANS
      character*80 filervers,rundate

! vars for csv output file closing !
      integer irecl,irow                !
      character*1 nul              !
      parameter (irecl=400)        !
      character*irecl longline     !
!----------------------------------!
      open(9,file='make_aimms_gds.files', iostat=stat)
      read(9,'(11x,a)') dict
      read(9,'(11x,a)')restarti   ! .ams input file
      read(9,'(11x,a)')restarto   ! .ams output file

      rundate=' '
      filervers=' '
      write(6,'(11x,a)') dict
      write(6,'(11x,a)')restarti
      write(6,'(11x,a)')restarto
      open(8,file=restarti,status='old',READONLY)
      open(9,file=restarto, status='unknown')

      WRITE(6,*) 'READ DICTIONARY'
! READ DICTIONARY
      FUNFMT=0
      FRTYPE=3
      FRETCD=0
      FUNITI=1
      FNAMEI=' '
      open(1,file=dict,status='old') 
      FUNITO=2
      FNAMEO='dictout'
      CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
      close(2)


      FRTYPE=1
      FSOURC=0
      FUNITI=1
      FUNITO=2
      FNAMEO=' '
      FUNFMT=0
      fnameo=' '
      WRITE(6,*) 'Aimms model as  input file (.ams)  ',trim(restarti)
      WRITE(6,*) 'Aimms model as output file (.ams)  ',trim(restarto)
      CALL FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)

      END
      FUNCTION LENG(A)
       IMPLICIT NONE
!     FINDS POSITION OF LAST NON-BLANK CHARACTER IN CHARACTER VARIABLE A
      CHARACTER A*(*)
      INTEGER*4 LENG,I,N
      N=LEN(A)
      LENG=0
      DO 10 I=N,1,-1
        IF(A(I:I).NE.' ') THEN
          LENG=I
          RETURN
        ENDIF
   10 CONTINUE
      RETURN
      END
      
      SUBROUTINE FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD, &
       FUNFMT)
!
! ARGUMENTS:
!  FRTYPE    INTEGER*4    FILER REQUEST TYPE: 1=OUTPUT(QUERY), 2=INPUT(UPDATE),
!                         3=READ DICTIONARY
!  FSOURC    INTEGER*4    OPTION FOR INPUT SOURCE OF AN OUTPUT REQUEST
!                         (USED IF FRTYPE=1).
!                         FSOURC=1 CAUSES REQUESTS TO BE READ FROM A FILE.
!                         OTHERWISE, A SINGLE REQUEST, AS STORED IN THE
!                         FILERC COMMON BLOCK VARIABLE, FREQST, IS PROCESSED.
!  FUNITI    INTEGER*4    UNIT NUMBER TO USER FOR INPUT
!                         (USED IF FRTYPE=2 OR 3, OR IF FRTYPE=1 AND FSOURC=1)
!  FNAMEI    CHARACTER*100 FILE NAME (PC) OR DD NAME (OS/MVS) FOR INPUT
!                         (USED IF FRTYPE=2 OR 3, OR IF FRTYPE=1 AND FSOURC=1)
!                         IF FNAMEI BLANK, THEN INPUT IS INITIATED WITHOUT
!                         EXECUTION OF AN OPEN STATEMENT.  THIS ALLOWS
!                         USE WITH OS/MVS JCL WHERE DD STATEMENTS HAVE DEFAULT
!                         NAME.
!  FUNITO    INTEGER*4    UNIT NUMER TO USE FOR OUTPUT (IF FRTYPE=1)
!  FNAMEO    CHARACTER*100 FILE NAME (PC) OR DD NAME (OS/MVS) FOR OUTPUT.
!                         (USED FOR AN OUTPUT REQUEST, WHERE FRTYPE=1.
!                         IF FNAMEO BLANK, THEN OUTPUT IS INITIATED WITHOUT
!                         AN OPEN STATEMENT.
!  FRETCD    INTEGER*4    FILER RETURN CODE. 0=NO ERRORS OR PROBLEMS DETECTED
!                         FRETCD > 0 MEANS SOME ERROR OCCURED.
!  FUNFMT    INTEGER*4    FILER OPTION FOR UNFORMATTED INPUT OR OUTPUT FILE
!                         (0=FORMATTED,
!                          1=UNFORMATTED, 2=PC UNFORMATTED)
!                          3=WK1
      IMPLICIT NONE

      INTEGER*4    FRTYPE,FSOURC,FUNITI,FUNITO,FRETCD,FUNFMT
      INTEGER*4    ICOUNT,IRET,IBLOCK,ICOM,IV,IVAR,ISTART,IEND,J, &
                   IPOINT,NSIZE,NELEM,K,IOPT,FLEN,yeardim
      CHARACTER*80 TRQST,PCSTR       
      CHARACTER*100 FNAMEI,FNAMEO
      character*300 line
      include'fdict'  
      include'parametr'! need mnumyr

!  FILER I/O REQUEST DATA.  FREQST CAN BE SET BY CALLING PROGRAM IN LIEU OF
!  READING EACH FREQST FROM A FILE.
      CHARACTER*80   FREQST,RQST_buffer
      COMMON/FILERC/FREQST
! LOCAL VARIABLES
      CHARACTER QLABEL*(FMAXCHARCOM),VAR*(FMAXCHARVAR)
      CHARACTER*8    FORM*8,RORI*1
      CHARACTER*4    RTEMP,ITEMP        
      INTEGER*4      DIM(5,2),AEND,LEND,LVAR,ORDER(6),ios,numreaderr/0/,i
      save numreaderr
      LOGICAL LDIM(5)

      CHARACTER      LABEL*8,FINDNM*8,DEFN*80
      CHARACTER*166 INLAB
      INTEGER nsets/0/
      DATA LABEL/':'/

      character*4 LabelArrayX(1801)
      character*1 atemp(1)/'1'/
      integer dloc,dstart,dcopy,dend


      character*12                      varName, dimIn
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
          
          
      integer nleft,nright
                  
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

      IF(FRTYPE.NE.1.AND.FRTYPE.NE.2.AND.FRTYPE.NE.3) THEN
        WRITE(6,*) ' *** FRTYPE MUST BE 1 (OUTPUT REQUEST), 2 (INPUT)'// &
        ' OR 3 (READ DICTIONARY)'
        WRITE(6,*) ' *** ROUTINE RETURNING EARLY'
        RETURN
      ENDIF

! OPEN FILES
      FRETCD=0
      
      CALL FFILE(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD, &
       FUNFMT)
      IF(FRTYPE.EQ.3) RETURN
      IF(FRETCD.NE.0) RETURN
      

      

      
      do while (.not. eof(8))
        read(8,'(a)') line
        if(index(line,'Section Global_Data_Structure').gt.0) then
          nright=0
          nleft=1
          write(9,'(a)') '  Section Global_Data_Structure {' 
          write(9,*) 
          write(9,'(a)') '    Section GDS_Parameters {'
          write(9,*) 
          
! skip over the remainder of the global_data_structure section in the input file by counting nesting brackets.
          do while (.not. eof(8) .and. nleft.gt.nright)
            read(8,'(a)') line
            if(index(line,'{').gt.0) nleft=nleft+1
            if(index(line,'}').gt.0) nright=nright+1
          end do

! now write out the new global data structure parameters first
          do ivar=1,fndict
             call removechar(fdescr(ivar),'"')
             qlabel=' '
             Call FFill_Strs(qlabel,FVARNM(IVAR),IVAR)
             
             if(ivar.eq.1) then
               write(9,'(6x,3a)') 'Section ',trim(fblknm(ivar)), '_Declarations {'
               write(9,'(7x,3a)') 'DeclarationSection ',trim(fblknm(ivar)), ' {'
             else
               if(fblknm(ivar).ne.fblknm(ivar-1)) then
                  write(9,'(7x,a)') '}'
                  write(9,'(6x,a)') '}'   
                  write(9,'(6x,3a)') 'Section ',trim(fblknm(ivar)), '_Declarations {'
                  write(9,'(7x,3a)') 'DeclarationSection ',trim(fblknm(ivar)), ' {'
               endif
             endif
            ! create something like this for each restart variable (AIMMS parameter)
            !   Parameter qblk_qelrs {
            !     index domain :  (mnumcr,mnumyr) ;
            !     text         :  "residential electricity consumption"
            !   }
             call mreplace(fvarnm(ivar),'$','_')
             do j=1,fndims(ivar)
               call mreplace(FVarDims(j,ivar),'$','_')
        
             enddo
 
             write(9,'(7x,3a)')   'Parameter ',trim(fblknm(ivar))//'_'//trim(fvarnm(ivar)), ' {'
             write(9,'(8x,16a)') '  IndexDomain :  (',  &
              (   trim(FVarDims(j,ivar)),',',   j=1,fndims(ivar)-1  ), &
                  trim(FVarDims(fndims(ivar),ivar)),') ;'
             write(9,'(8x,3a)')  '  Text        :  "',trim(fdescr(ivar)),'" ;'
             write(9,'(7x,a)') '}'          
          enddo
          write(9,'(7x,a)') '}'
          write(9,'(6x,a)') '}' 
          write(9,'(a)') '    }' 
                   
          write(9,'(2a)') '    DeclarationSection GDS_Sets {'
          do ivar=1,fndict
            Call FFill_Strs(qlabel,FVARNM(IVAR),IVAR)

!           go through each dimension of each variable.  Add new dimension names as sets.  Newly found ones have a cnt of 0
            DO j=1,Fndims(ivar)        
	      IF (fkdim(j,ivar).GE.1) THEN
	199     Do I=1,FMaxDim  
	          IF (Fvardim(J) .EQ. FdimNam(I)) THEN                 
	             CNT(I) = CNT(I)+1
!create something like this for each set (dimension name):
!
!      Set mnumcr_{
!         SubsetOf:  Integers;
!         Text:         "Census Regions";
!         Index:        mnumcr;
!         InitialData:  {1..11} ;

                    if(cnt(i).eq.1) then
                      call removechar(FDimDescrip(i),'"')
                      call mreplace(FDimNam(i),'$','_')
                      write(9,'(6x,3a)')   'Set ',trim(FDimNam(i))//'_ {'
                      write(9,'(8x,2a)')     'Subsetof:   Integers ;'
                      write(9,'(8x,3a)')     'Text:       "',trim(FDimDescrip(i)),'" ;'
                      write(9,'(8x,3a)')     'Index:     ',trim(FDimNam(i)), ' ;'
                      write(9,'(8x,a,i4.4,a)') 'InitialData:  {'
                      write(9,'(8x,a,i4.4,a)') '  {1..',FDimSiz(i),'}'
                      write(9,'(8x,a)') '}'
                      write(9,'(6x,a)') '}'

                      nsets=nsets+1
                    endif
                    exit  ! exit do loop, now that it's been found
                  endif
	        ENDDO
	      ENDIF   
               
	    ENDDO           
	  enddo
	  write(9,'(a)') '    }'
	  write(9,'(a)') '  }'
	else
	  write(9,'(a)') trim(line) 
      
	endif


      enddo
      write(6,*)' Number of sets declared:',nsets
      write(6,*)' Number of parameters declared:',fndict
      RETURN
      END
!***********************************************************************
      SUBROUTINE FFILE(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD, &
       FUNFMT)
! THIS OPENS INPUT AND OUTPUT FILES IF THEY ARE NAMED.  CALLS
! FDICT TO READ DICTIONARY IF FRTYPE=3.
! IF UNFORMATTED OPTION IN EFFECT (FUNFMT), THEN INPUT REQUESTS ARE
! READ FROM UNFORMATTED FILE, AND OUTPUT REQUESTS ARE WRITTEN TO
! UNFORMATTED FILES.


      IMPLICIT NONE
      integer irecl
      character*1 nul
      parameter (irecl=400)
      character*irecl longline
      logical lexist

      INTEGER*4 FRTYPE,FSOURC,FUNITI,FUNITO,FRETCD,FUNFMT
      CHARACTER*100 FNAMEI,FNAMEO,BLNKFILE*50
      CHARACTER*6 CODE
      IF(FRTYPE.EQ.3) THEN
! OPEN FILE AND READ DICTIONARY FROM FUNITI. WRITE SUMMARY TO
! DESIGNATED OUTPUT FILE OR SYSTEM OUTPUT
        IF(FNAMEI.NE.' ') OPEN(UNIT=FUNITI,FILE=FNAMEI,STATUS='OLD',READONLY)
        IF(FNAMEO.NE.' ') OPEN(UNIT=FUNITO,FILE=FNAMEO,STATUS='UNKNOWN')
        CALL FDICT(FUNITI,FUNITO)
!CLOSE        CLOSE(UNIT=FUNITI)
!CLOSE        IF(FUNITO.NE.6)CLOSE(UNIT=FUNITO)
        FRETCD=0
        RETURN
      ELSEIF(FRTYPE.EQ.1)THEN
! IF FSOURC IS 1 (INDICATING FILE INPUT OF THE OUTPUT REQUEST SPECS) OPEN FILE
        IF(FSOURC.EQ.1.AND.FNAMEI.NE.' ') &
           OPEN(UNIT=FUNITI,FILE=FNAMEI,STATUS='OLD',READONLY)
!  OPEN OUTPUT FILE (IF IT IS NAMED)
        IF(FUNFMT.EQ.0) THEN
          IF(FNAMEO.NE.' ') &
            OPEN(UNIT=FUNITO,FILE=FNAMEO,STATUS='UNKNOWN')
        ELSEIF (FUNFMT.GE.1.AND.FUNFMT.LE.4) THEN
          IF(FNAMEO.NE.' ') THEN
            IF(FUNFMT.EQ.3)THEN    ! WK1
              OPEN(UNIT=FUNITO,FILE=FNAMEO,FORM='UNFORMATTED',    &
              ACCESS='SEQUENTIAL',STATUS='UNKNOWN') 
            ELSEIF(FUNFMT.EQ.4) THEN ! CSV
              inquire(file=fnameo,exist=lexist)
              if(lexist) then
                OPEN(UNIT=funito,FILE=fnameo,FORM='FORMATTED', &
                ACCESS='DIRECT',recl=irecl,err=1000)
                close(funito,status='delete',err=1000)
1000            continue
              endif
              OPEN(UNIT=funito,FILE=fnameo,FORM='FORMATTED', &
              ACCESS='DIRECT',status='new',recl=irecl)
            ELSE
              OPEN(UNIT=FUNITO,FILE=FNAMEO,FORM='UNFORMATTED', &
              ACCESS='SEQUENTIAL',STATUS='UNKNOWN')
            ENDIF
          ENDIF
! IF WK1 FORMAT, WRITE HEADER CODE
          IF(FUNFMT.EQ.3) THEN
            CODE(1:6)=CHAR(0)//CHAR(0)//CHAR(2)//CHAR(0)// &
                      CHAR(4)//CHAR(4)
          ENDIF
        ENDIF
      ELSEIF(FRTYPE.EQ.2) THEN
        IF(FUNFMT.EQ.0) THEN
          IF(FNAMEI.NE.' ') OPEN(UNIT=FUNITI,FILE=FNAMEI,STATUS='OLD',READONLY)
        ELSEIF(FUNFMT.ne.5) then
          !IF(FNAMEI.NE.' ')write(6,*) 'big_endian'
          IF(FNAMEI.NE.' ') &
          OPEN(UNIT=FUNITI,FILE=FNAMEI,STATUS='OLD', READONLY, &
          FORM='UNFORMATTED',ACCESS='SEQUENTIAL',CONVERT='BIG_ENDIAN')
        ENDIF
      ENDIF
      RETURN
      END
!*******************************************************************
!DBG  DEBUG SUBCHK
!DBG  END DEBUG
      INTEGER FUNCTION FLEN(A)
!     FINDS POSITION OF LAST NON-BLANK CHARACTER IN CHARACTER VARIABLE A

      IMPLICIT NONE
      INTEGER*4 I,N
      CHARACTER*(*) A
      N=LEN(A)
      FLEN=0
      DO 10 I=N,1,-1
        IF(A(I:I).NE.' ') THEN
          FLEN=I
          RETURN
        ENDIF
   10 CONTINUE
      RETURN
      END
!******************************************************************
!---> the following codes apply only to MAINFRAME ----
!---- program to convert mainframe INTEGER*4 to pc INTEGER*4 ----
!DBG  DEBUG SUBCHK
!DBG  END DEBUG
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


      SUBROUTINE FDICT(FUNITI,FUNITO)
! READS DICTIONARY FILE CONTAINING A LIST OF VARIABLES IN EACH COMMON
! BLOCK.  THE LIST MUST BE IN THE SAME ORDER AS THE VARIABLES AS
! DECLARED IN COMMON.

      IMPLICIT NONE

      INTEGER*4 FUNITI,FUNITO
      include'fdict'
      
!  DICTIONARY VARIABLES.
!  FMAXDT = PARAMETER FOR MAXIMUM NUMBER OF VARIABLES IN DICTIONARY.
!  FMAXCM = PARAMETER FOR MAXIMUM NUMBER OF COMMON BLOCKS IN DICTIONARY
!  FVARNM(FMAXDT) = VARIABLE NAME
!  FBLKNM(FMAXDT) = COMMON BLOCK NAME FOR EACH VARIABLE
!  FDESCR(FMAXDT) = VARIABLE DEFINITION
!  FVARTY(FMAXDT) = VARIABLE TYPE (I, R, C)
!  FVARLN(FMAXDT) = BYTE LENGTH OF THE VARIABLE TYPE (2 OR 4 FOR INTEGER,
!                   4 OR 8 FOR REAL, OR THE STRING LENGTH IF A
!                   CHARACTER VARIABLE
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
!                   AGREE WITH THE SIZE OF THE COMMON AS REPORTED IN THE LINK
!                   EDITOR MAP.
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
        IF(LINE(1:1).NE.' ') GOTO 10
! CHECK FOR THE DIMENSION AND ATTRIBUTE RECORDS
! READ DIMENSION RECORDS
        IF(.NOT. ENDDIM) THEN
          CALL READDIM(LINE,RETCODE)
          IF(RETCODE .EQ. 1) THEN
            ENDDIM = .TRUE.
          ENDIF
          GOTO 10
        ENDIF
! READ ATTRIBUTE RECORDS
        IF(.NOT. ENDATT) THEN
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
        IF(FBLKNM(FNDICT) .EQ. '        ') FBLKNM(FNDICT) = OLDNAM
        IF(FVARNM(FNDICT) .NE. '        ') THEN
          ERRFLG = .FALSE.
          CALL FindDims(ERRFLG)
          ERRFLG = .FALSE.
          CALL FindAtts(ERRFLG)
        ENDIF


! CHECK FOR DUPLICATE VARIABLES (ARTIFACT OF OLD BUFVARS FILE)
        IF(FVARNM(FNDICT).EQ.OLDVAR) THEN
          WRITE(6,*) '*** DUPLICATE VARIABLE FOUND: ',OLDVAR
          WRITE(6,*) '*** WILL SKIP THAT ENTRY'
          GOTO 10
        ENDIF
        OLDVAR=FVARNM(FNDICT)

! IF NEW COMMON BLOCK, UPDATE COMMON BLOCK LIST. RESET POINTER.
        IF(FBLKNM(FNDICT) .NE. OLDNAM) THEN
          IF(FNBLOC .GE. 1) THEN
            FBLKSZ(FNBLOC)=FVARSZ(FNDICT-1)+FBPOIN(FNDICT-1)-1
          ENDIF
          FNBLOC=FNBLOC+1
          IF(FNBLOC .GT. FMAXCM) THEN
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
        IF(FNDIMS(FNDICT).EQ.0) THEN
          DO 20 K=1,5
            FKDIM(K,FNDICT)=1
20        CONTINUE
          FNDIMS(FNDICT)=1
        ENDIF
! COMPUTE SIZE OF THE VARIABLE IN BYTES, TAKING INTO ACCOUNT THE ARRAY
! DIMENSIONS AND THE UNIT SIZE OF THE VARIABLE
        KPROD=1
        DO 30 K=1,5
          IF(FKDIM(K,FNDICT).EQ.0) FKDIM(K,FNDICT)=1
          KPROD=KPROD*FKDIM(K,FNDICT)
30      CONTINUE
        FVARSZ(FNDICT)=KPROD*FVARLN(FNDICT)
!        IF(FVARSZ(FNDICT).GT.65536*4) THEN
!          WRITE(6,*)'*** AN ARRAY EXCEEDS MAX SIZE.'
!          WRITE(6,*)'*** VARIABLE IS: ',FVARNM(FNDICT)
!          GOTO 40
!        ENDIF
! SET BYTE POINTER FOR START OF VARIABLE WITHIN THE COMMON BLOCK
        IF(FNDICT.LT.FMAXDT) FBPOIN(FNDICT+1)= &
                             FBPOIN(FNDICT)+FVARSZ(FNDICT)

! INCREMENT THE COUNT OF VARIABLES
        FNDICT=FNDICT+1
        IF(FNDICT.EQ.FMAXDT) THEN
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
      if(DIMD.eq.' ') then
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
         if(i.eq.0) then
           READ(LINE,'(BN,1X,A3,A1,1X,A6,i6,1x,i4)') DIM,DIMD,NAME,I, firstval
           idim=0
           do j=1,FDimIndex
             if(NAME.eq.FDimNam(j)) then
               idim=j
               exit
             endif
           enddo
           if(idim.eq.0) then
             errflag=.true.
             write(6,*) '***Dimension string not found: ',strg
             goto 998
           endif
           FDimStr_Index(idim)=count_str+1
           largest=FDimSiz(idim)+firstval-1
           FDimStr_Long(idim)=len_trim(NAME)+1+floor(log10(real(largest)))+1
           do j=1,FDimSiz(idim)
             istr=' '
             if(largest.le.9) then
               write(istr,'(i1)') firstval+j-1
             elseif(largest.le.99) then
               write(istr,'(i2.2)') firstval+j-1
             elseif(largest.le.999)then
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
             if(strg(j:j).eq.' ') strg(j:j)='_'
           enddo
 ! find dimension string in the list
           idim=0
           do j=1,FDimIndex
             if(NAME.eq.FDimNam(j)) then
               idim=j
               exit
             endif
           enddo
           if(idim.eq.0) then
             errflag=.true.
             write(6,*) '***Dimension string not found: ',strg
             goto 998
           endif
 ! add the dimension element to the list of strings and identify
 ! the starting location if on the first one.
           count_str=count_str+1
           if(i.eq.1) then
             FDimStr_Index(idim)=count_str
           endif
           if(i.le.FMaxDim) then
              istr=' '
             if(FDimSiz(idim).le.9) then
               write(istr,'(i1)') i
             elseif(FDimSiz(idim).le.99) then
               write(istr,'(i2.2)') i
             elseif(FDimSiz(idim).le.999) then
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
      IF(NAME .EQ. 'ENDDIM') THEN
! create generic dimension element strings for those that have not been specified
        firstval=1
        do idim=1,FDimIndex
          if(FDimStr_Index(idim).eq.0.and.FDimSiz(idim).gt.0.and.FDimSiz(idim).le.FMaxDims.and.count_str.lt.FMaxStrs) then
           FDimStr_Index(idim)=count_str+1
           largest=FDimSiz(idim)+firstval-1
           do j=1,FDimSiz(idim)
             istr=' '
             if(largest.le.9) then
               write(istr,'(i1)') firstval+j-1
             elseif(largest.le.99) then
               write(istr,'(i2.2)') firstval+j-1
             elseif(largest.le.999)then
               write(istr,'(i3.3)') firstval+j-1
             else
               write(istr,'(i4.4)') firstval+j-1
             endif
             count_str=count_str+1
             FDimStrS(count_str)=trim(istr)//'_'//trim(FDimNam(idim))
           enddo  
          elseif(FDimStr_Index(idim).eq.0) then
            !write(6,'(a,i4,a,i4,a,2i6)') 'Omitting dict.txt dim strings for '//trim(FDimNam(idim))//'=',& 
            !  FDimSiz(idim),' FMaxDims=',FMaxDims,',  String count_str and FMaxStrs=',count_str,FMaxStrs  
          endif
      
        enddo
        RETCODE = 1
        GOTO 999
      ENDIF
      IF(FDimINDEX .EQ. FMaxDim) THEN
        ErrFlag = .TRUE.
        WRITE(6,*) '***MAXIMUM NUMBER OF DIMENSIONS READ AT NAME: '// &
                   NAME
        RETCODE = 1
        GOTO 998
      ENDIF
      ErrFlag = .FALSE.
!      IF(FDimINDEX .EQ. 0) GOTO 998
      IF(DIM .NE. 'DIM') THEN
        ErrFlag = .TRUE.
        WRITE(6,*) '***DIMENSION RECORD SHOULD START WITH DIM, NOT '// &
                   DIM
        GOTO 998
      ENDIF
      IF(DIMD.EQ.' ') then
        J = 1
        DO WHILE(.NOT. ErrFlag .AND. J .LE. FDimINDEX)
          IF(J .GT. FMaxDim) THEN
            ErrFlag = .TRUE.
            WRITE(6,*) '***MAXIMUM NUMBER OF DIMENSIONS READ AT NAME: '// &
                       NAME
          ENDIF
          IF(FDimNam(J) .EQ. NAME) THEN
            ErrFlag = .TRUE.
            WRITE(6,*) '***DUPLICATE DIMENSION NAME '// NAME // ' FOUND.'
          ENDIF
          J = J+1
        END DO
      endif
998   IF(ErrFlag) THEN
        WRITE(6,*) '***WILL SKIP THAT ENTRY.'
      ELSE
        if(dimd.eq.'D')then
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

      include'fdict'

      READ(LINE,100) ATT,L,NAME,DESCRIP
100   FORMAT(1X,A3,1X,A1,1X,A6,2X,A25)
      IF(NAME .EQ. 'ENDATT') THEN
        RETCODE = 1
        GOTO 999
      ENDIF
      IF(L .EQ. 'L') THEN
        ATTTYP = NAME
        RETCODE = 0
        GOTO 999
      ENDIF
      IF(FAttINDEX .EQ. FMaxAtt) THEN
        ErrFlag = .TRUE.
        WRITE(6,*) '***MAXIMUM NUMBER OF ATTRIBUTES READ AT NAME: '// &
                   NAME
        RETCODE = 1
        GOTO 998
      ENDIF
      ErrFlag = .FALSE.
!      IF(FAttINDEX .EQ. 0) GOTO 998
      IF(ATT .NE. 'ATT') THEN
        ErrFlag = .TRUE.
        WRITE(6,*) '*** ATTRIBUTE RECORD SHOULD START WITH ATT, NOT '// &
                   ATT
        GOTO 998
      ENDIF
      J = 1
      DO WHILE(.NOT. ErrFlag .AND. J .LE. FAttINDEX)
        IF(J .GT. FMaxAtt) THEN
          ErrFlag = .TRUE.
          WRITE(6,*) '*** MAXIMUM NUMBER OF ATTRIBUTES READ AT NAME: '// &
                     NAME
        ENDIF
        IF(FAttNam(1,J) .EQ. ATTTYP .AND. FAttNam(2,J) .EQ. NAME) THEN
          ErrFlag = .TRUE.
          WRITE(6,*) '*** DUPLICATE ATTRIBUTE TYPE/NAME '// ATTTYP // &
                     '/' // NAME // ' FOUND.'
        ENDIF
        J = J+1
      END DO
998   IF(ErrFlag) THEN
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
          IF(FVarAtts(J,FNDICT) .EQ. FAttNam(1,K)) THEN
            FOUND = .TRUE.
          ELSE
            K = K+1
          ENDIF
        END DO
        IF (.NOT. FOUND) THEN
          WRITE(6,*) '*** COULD NOT FIND ATTRIBUTE: ', &
          FVarAtts(J,FNDICT), &
                      ' FOR BLOCK: ',FBLKNM(FNBLOC)
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
        if(ICONST.EQ.1) then
          READ(FVARDIMS(J,FNDICT),'(BN,I6)') FKDIM(J,FNDICT)
          if(FKDIM(J,FNDICT).lt.10) then
             width(j)=1
          elseif(FKDIM(J,FNDICT).lt.100) then
             width(j)=2
          elseif(FKDIM(J,FNDICT).lt.1000) then
             width(j)=3
          elseif(FKDIM(J,FNDICT).lt.10000) then
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
          IF(FVarDims(J,FNDICT) .EQ. FDimNam(K)) THEN
            FOUND = .TRUE.
            FKDIM(J,FNDICT) = FDimSiz(K)
          ELSE
            K = K+1
          ENDIF
        END DO
        IF (.NOT. FOUND) THEN
          IF (WIDTH(J) .EQ. 0) WRITE(6,'(a,I2,a,a)') '*** COULD NOT FIND DIMENSION #',J,' NAME: ',FVarDims(J,FNDICT)
          last_str_pos=FDimStr_Index(FDimIndex)+FDimSiz(FdimIndex)-1
          IF(FDimIndex.LT. FMaxDim.and.last_str_pos+min0(fkdim(j,fndict),FMaxDims-1).lt.FMaxStrs) then
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
        if(j.eq.6) exit  ! added to prevent out-of-bounds on do while condition
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
      IF(LA.EQ.0) RETURN
      DO J=1,LA
        IF(INDEX(NUMERAL,A(J:J)).EQ.0) RETURN
      ENDDO
      I=1
      RETURN
      END
!*******************************************************************
      SUBROUTINE FindAtts(ERROR)
!
! CHECK THE GLOBAL VARIABLE'S ATTRIBUTES MNEMONICS AGAINST THOSE
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
          IF(FVarAtts(J,FNDICT) .EQ. FAttNam(2,K) .AND. &
             FBlkAtts(J,FNBLOC) .EQ. FAttNam(1,K)) THEN
            FOUND = .TRUE.
          ELSE
            K = K+1
          ENDIF
        END DO
        IF (.NOT. FOUND) THEN
          WRITE(6,*) '*** COULD NOT FIND ATTRIBUTE: ', &
           FVarAtts(J,FNDICT), &
                      ' FOR VARIABLE: ',FVARNM(FNDICT)
          ERROR = .TRUE.
          RETURN
        ENDIF
        J = J+1
        if(j.eq.6) exit  ! added to prevent out-of-bounds on do while condition
      END DO
      DO K = J,5
        FVarAtts(K,FNDICT) = '      '
      END DO
      ERROR = .FALSE.
      RETURN
      END
      
      subroutine removechar(a,remove)
      implicit none
      character*(*) a
      character*1 remove
      integer i
      do i=1,len_trim(a)
        if(a(i:i).eq.remove) then
          a(i:)=a(i+1:)
        endif
      enddo
      return
      end


        
   
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
      
           
! find variable name in dictionary.  if common block name omitted, assume variable name is unique across common.
!   If variable listed in more than one common block, should specify common block label, too.
    if(ivar.eq.0) ivar=1
    IF(VARNAM.NE.FVARNM(IVAR))THEN
      iblock=0
      ivar=0
      if(len_trim(blocknam).gt.0) then
        do icom=1,FNBLOC
          if(blocknam.eq.FBLOCK(icom))then
            iblock=icom
            exit
          endif
        enddo
      endif
      if(iblock.gt.0) then
        ISTART=FINDEX(IBLOCK)
        IEND=FINDEX(IBLOCK)+FNVARS(IBLOCK)-1
        DO IV=ISTART,IEND
          IF(VARNAM.EQ.FVARNM(IV))THEN
            IVAR=IV
            EXIT
          ENDIF
        ENDDO
      endif
      if(ivar.eq.0) then
        DO IV=1,FNDICT
          IF(VARNAM.EQ.FVARNM(IV))THEN
            IVAR=IV
            EXIT
          ENDIF
        ENDDO
      endif
      if(ivar.eq.0)then
        return
      endif
    endif

! check dimension count. Sometimes dictionary has the wrong number  
    chkdim=0
    do j=1,5
      if(fkdim(j,ivar).gt.1) then
        chkdim=chkdim+1
      endif
    enddo
    if(chkdim.gt.fndims(ivar))then
      fndims(ivar)=chkdim
    endif
     
! fill in names of elements in each dimension      

      do j=1,fndims(ivar) ! number or dimensions
         DimNam=FVarDims(j,ivar)
         if(FVarDim(j).ne.DimNam.or.FDimStr(1,j).eq.' '.or.index('D1D2D3D4D5',trim(dimnam)).gt.0) then ! if not already done, fill up this dimension with strings
           FDimLong(j)=0
           FDimStr(:,j)=' '         
           idim=0
           do i1=1,FDimIndex
             if(DimNam.eq.FDimNam(i1)) then
               idim=i1
               exit
             endif
           enddo 
           if(idim.ne.0) then
             FVarDim(j)=DimNam

             iwidth=1
             if(fkdim(j,ivar).gt.0) iwidth=floor(log10(real(fkdim(j,ivar))))+1
             do i1=1,FKDIM(j,ivar) ! rank of each dimension
               if(FDimStr_Index(idim).gt.0.and.i1.le.FMaxDims) then
                 FDimStr(i1,j)=FDimStrs(FDimStr_Index(idim)+i1-1) 
               else
                 if(i1.lt.FMaxDims)then
                    write(FDimStr(i1,j),'(i<iwidth>.<iwidth>)') i1
                 endif
               endif
               FDimLong(j)=max(FDimLong(j),len_trim(FDimstr(min(i1,FMaxDims),j)))
             enddo
           else
             FVarDim(j)=DimNam
             iwidth=1
             if(fkdim(j,ivar).gt.0) iwidth=floor(log10(real(fkdim(j,ivar))))+1
             do i1=1,FKDIM(j,ivar) ! rank of each dimension
               if(i1.lt.FMaxDims)then
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
         
       subroutine Mreplace(string,finds,repls)
       implicit none
       character*(*) string, finds, repls
       character*255 temp
       integer L,LF,LR,I,leng
       l=len(string)
       lf=len(finds)
       lr=len(repls)
       i=1
10     continue
         if(string(i:i+LF-1).eq.finds) then
           temp=string(i+Lf:)
           string(i:i+LR-1)=repls
           string(i+LR:)=trim(temp)
           l=len(string)
           i=i+LR-1
         endif
         i=i+1
       if(i.le.L-LF+1) goto 10
       return
       end subroutine Mreplace
