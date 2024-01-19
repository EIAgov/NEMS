! $Header: m:/default/source/RCS/uesql.f90,v 1.6 2020/10/16 14:49:41 tdm Exp $


!        PROGRAM GET_EMM
!          
!          implicit none
!
!          call emmsql
!        END PROGRAM GET_EMM


        SUBROUTINE EMMSQL

          use sqlite

          implicit none

          type(sqlite_database)                      :: db
          type(sqlite_statement)                     :: stmt
          type(sqlite_column), dimension(:), pointer :: col

          integer                                    :: i
          integer                                    :: j  

          character(len=40), pointer, dimension(:)   :: result
          character(len=80)                          :: errmsg

          logical                                    :: finished

          real                                       :: start_time
          real                                       :: stop_time

          !call cpu_time(start_time)  

          open ( unit = 901, file = "emmdbin" )

          !write( 901, '(a18)') "VSALELB"

          !write( 901, '(a1)') ""

           !call cpu_time(stop_time)

          !write( 901, '(a1)') ""
          !write( 901, '(a20,f15.5,f15.5,a20)') "Retrieve dB time: ", &
          ! stop_time,start_time, " seconds"

        END SUBROUTINE EMMSQL

        SUBROUTINE SALELBSQL
                
          USE SQLITE

          IMPLICIT NONE

        !***************************************************************
        ! THIS SUBROUTINE READS IN THE SALES LEASEBACK DATABASE
        !***************************************************************
        ! INTERNAL VARIABLES:
        !        IYR = YEAR OF SALE/LEASEBACK PLAN
        !        BKGAIN = ?????
        ! OUTPUT VARIABLES:
        !        NSL = TOTAL NUMBER OF SALE/LEASEBACK TRANSACTIONS
        !        SAVNSL = TOTAL NUMBER OF SALE/LEASEBACK TRANSACTIONS
        !                 (NEVER CHANGED THROUGHOUT EXECUTION)
        !        IROSL(1,ISL) = REGION OF SALE/LEASEBACK
        !        IROSL(2,ISL) = OWNERSHIP TYPE OF SALE/LEASEBACK
        !        IBYRSL = 1ST YEAR OF SALE/LEASEBACK PERIOD
        !        SLBKVL = BOOK VALUE OF SALE/LEASEBACK PLANT
        !        SLPROC = GROSS SALE PROCEEDS
        !        SLGAIN = NET OF TAX GAIN OVER BOOK VALUE FROM SALE
        !        SLTERM = TERM OF THE LEASE (YEARS)
        !        SLLP = ANNUAL LEASE PAYMENT
        !        SLTAXS = ?????
        !***************************************************************

          include 'parametr'
          include 'emmparm'
          include 'control'
          include 'salelb'

          type(sqlite_database)                      :: db
          type(sqlite_statement)                     :: stmt
          type(sqlite_column), dimension(:), pointer :: col
          character(len=40), pointer, dimension(:)   :: result
          character(len=80)                          :: errmsg

          integer                                    :: i
          integer                                    :: j
          integer                                    :: id
          integer                                    :: iyr
          real                                       :: bkgain
          logical                                    :: finished

          !open ( unit = 901, file = "salelb_input" )
          
          call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )
          allocate ( col(9) )
        ! QUERY THE DATABASE TABLE
          call sqlite3_column_query( col(1), 'ID', sqlite_int )
          call sqlite3_column_query( col(2), 'YR', sqlite_int )
          call sqlite3_column_query( col(3), 'OWN', sqlite_int )
          call sqlite3_column_query( col(4), 'EMM_REG', sqlite_int )
          call sqlite3_column_query( col(5), 'SLPROC', sqlite_real )
          call sqlite3_column_query( col(6), 'BKGAIN', sqlite_real )
          call sqlite3_column_query( col(7), 'SLTAXS', sqlite_real )
          call sqlite3_column_query( col(8), 'SLLP', sqlite_real )
          call sqlite3_column_query( col(9), 'SLTERM', sqlite_real )

          call sqlite3_prepare_select( db, 'V_EMM_SALELB', col, stmt)


!          write (901,'(a8,a2,a8,a2,a8,a2,a8,a2,a8,a2,a8,a2,a8,a2,a8)') 'IYR',' ', & 
!                'IROSL(2)',' ','IROSL(1)',' ','SLPROC',' ','BKGAIN', &
!                ' ','SLTAXS',' ','SLLP',' ','SLTERM'

        ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
          do
             call sqlite3_next_row( stmt, col, finished )
             if ( finished ) exit

             call sqlite3_get_column( col(1), NSL)
             call sqlite3_get_column( col(2), IYR)
             call sqlite3_get_column( col(3), IROSL(2,NSL))
             call sqlite3_get_column( col(4), IROSL(1,NSL))
             call sqlite3_get_column( col(5), SLPROC(NSL))
             call sqlite3_get_column( col(6), BKGAIN)
             call sqlite3_get_column( col(7), SLTAXS(NSL))
             call sqlite3_get_column( col(8), SLLP(NSL))
             call sqlite3_get_column( col(9), SLTERM(NSL)) 

             SLBKVL(NSL) = SLPROC(NSL) - BKGAIN
             SLGAIN(NSL) = BKGAIN - SLTAXS(NSL)
             IBYRSL(NSL) = IYR + 1 - UHBSYR
             SAVNSL = NSL
             !write(901,'(i4,a3,i4,a3,i4,a3,i4,a3,f8.2,a3,f8.2,a3,f8.2,a3,f8.2,a3,f8.2)' ) & 
             !       NSL, ' ',IYR,&
             !   ' ',IROSL(1,NSL), ' ',IROSL(2,NSL),' ',SLPROC(NSL),' ',& 
             !       BKGAIN,' ',SLTAXS(NSL),' ',SLLP(NSL),' ',SLTERM(NSL)
          enddo

          deallocate(col)

          call sqlite3_close( db )

        END SUBROUTINE SALELBSQL 

        SUBROUTINE PHASEINSQL
          USE SQLITE

          IMPLICIT NONE
!*****************************************************************
!      THIS SUBROUTINE READS IN THE PHASEIN DATA
!*****************************************************************
! INTERNAL VARIABLES:
!    DISPR1 = FRACTION OF EXCESS CAPACITY DISALLOWANCE TO TREAT AS DISALLOWANCE
!    DISPR2 = FRACTION OF IMPRUDENCE DISALLOWANCE TO TREAT AS DISALLOWANCE
!    DISXCS = EXCESS CAPACITY DISALLOWANCE
!    DISPRU = IMPRUDENCE DISALLOWANCE
!    IASSMP = LOWEST ASSUMPTION CODE TO USE IN THIS RUN (SCREENING VARIABLE)
!    IASMPT = ASSUMPTION CODE FOR EACH PHASE-IN
!    ICT    = FILLS PIDFS with 1 in years after PIDFS is 1 from PHASEIN file
!    ICT1   = FILLS PIRCS with 1 in years after PIRCS is 1 from PHASEIN file
! OUTPUT VARIABLES:
!    PITXBS = TAX BASIS AS A FRACTION OF BOOKED COST OF
!    PIBKLF = BOOK LIFE OF PHASED-IN PLANT
!    IBYRPI = 1ST YEAR OF PHASE-IN PERIOD
!    IROPI(1,IPI) = REGION OF PHASE-IN PLAN
!    IROPI(2,IPI) = OWNERSHIP TYPE OF PHASE-IN PLAN
!    LPI = LENGTH OF PHASE-IN PLAN
!    PIBKVL = BOOK VALUE OF PLANT TO BE PHASED-IN
!    NPI = TOTAL NUMBER OF PHASE-IN PLANS
!    SAVNPI = TOTAL NUMBER OF PHASE-IN PLANS (NEVER CHANGED FROM
!             INITIAL VALUE)
!    IRDPI = CAPITALIZE RETURN ON DEFERRED COST? (1=YES,2=NO)
!    PIRCS = FRACTION OF REMAINING DEFERRED REVENUES TO BE
!    PIDEF = TOTAL CUMULATIVE DEFERRED REVENUES
!    DISPER = FRACTION OF PLANT TOTALLY DISALLOWED
!    PIDFS = CUMULATIVE FRACTION OF TOTAL COST PHASED-IN BY
!*****************************************************************
      include 'parametr'
      include 'emmparm'
      include 'control'
      include 'phasin'
      
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

      integer                                    :: i
      integer                                    :: j
      integer                                    :: id
      integer                                    :: iyr
      integer                                    :: iyrtmp
      real                                       :: bkgain
      integer                                    :: IASSMP
      integer                                    :: IASMPT(125)
      integer                                    :: ICT
      integer                                    :: ICT1
      real*4                                     :: DISPR1
      real*4                                     :: DISPR2
      real*4                                     :: DISPRU
      real*4                                     :: DISXCS
      integer                                    :: NPIREC(125)
      integer                                    :: NPIREC2
      integer                                    :: NPITMP
      character(len=4)                           :: IASSMPstr
      INTEGER IN
  
      
          call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )

          allocate ( col(5) )
        ! QUERY THE DATABASE TABLE
          call sqlite3_column_query( col(1), 'PIBKLF', sqlite_real )
          call sqlite3_column_query( col(2), 'PITXBS', sqlite_real )
          call sqlite3_column_query( col(3), 'DISPR1', sqlite_real )
          call sqlite3_column_query( col(4), 'DISPR2', sqlite_real )
          call sqlite3_column_query( col(5), 'IASSMP', sqlite_int )

          call sqlite3_prepare_select( db, 'V_EMM_PHASEIN_PARAM', col, stmt)
          !write(901,'(a64)') 'V_EMM_PHASEIN_PARAM'

          !write (901,'(a8,a2,a8,a2,a8,a2,a8,a2,a8)') 'PIBKLF',' ', & 
          !      'PITXBS',' ','DISPR1',' ','DISPR2',' ','IASSMP'

          do
            call sqlite3_next_row( stmt, col, finished )
            if ( finished ) exit

            call sqlite3_get_column( col(1), PIBKLF )
            call sqlite3_get_column( col(2), PITXBS )
            call sqlite3_get_column( col(3), DISPR1 )
            call sqlite3_get_column( col(4), DISPR2 )
            call sqlite3_get_column( col(5), IASSMP )

!            write (901,'(f8.2,a3,f8.2,a3,f8.2,a3,f8.2,a3,i4)') PIBKLF,' ', & 
!            PITXBS,' ',DISPR1,' ',DISPR2,' ',IASSMP            
            
          end do

          write(IASSMPstr,'(i4)'), IASSMP

          deallocate ( col )
          finished = .FALSE.



          allocate ( col2(10) )
        ! QUERY THE DATABASE TABLE
          call sqlite3_column_query( col2(1), 'ID', sqlite_int )
          call sqlite3_column_query( col2(2), 'RECORD', sqlite_int )
          call sqlite3_column_query( col2(3), 'IROPI', sqlite_int )
          call sqlite3_column_query( col2(4), 'IBYRPI', sqlite_int )
          call sqlite3_column_query( col2(5), 'PIBKVL', sqlite_real )
          call sqlite3_column_query( col2(6), 'DISXCS', sqlite_real )          
          call sqlite3_column_query( col2(7), 'DISPRU', sqlite_real )
          call sqlite3_column_query( col2(8), 'LPI', sqlite_int )
          call sqlite3_column_query( col2(9), 'IRDPI', sqlite_int )
          call sqlite3_column_query( col2(10), 'IASMPT', sqlite_int )

          call sqlite3_prepare_select( db, 'V_EMM_PHASEIN', col2, stmt2,'WHERE IASMPT >= '//IASSMPstr)

!          write(901,'(a64)') 'V_EMM_PHASEIN'
!
!          write (901,'(a8,a2,a8,a2,a8,a2,a8,a2,a8,a2,a8,a2,a8,a2,a8,a2,a8,a2,a8,a2,a8)') 'ID',' ', & 
!                'RECORD',' ','IROPI',' ','IROPI2',' ','IBYRPI',' ','PIBKVL', &
!                ' ','DISXCS',' ','DISPRU',' ','LPI',' ','IRDPI',' ','IASMPT',' ','PIDEF',' ','DISPER'

        ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
          i=0
          do
             call sqlite3_next_row( stmt2, col2, finished )
             if ( finished ) exit
            
             i=i+1
             NPI=i
             call sqlite3_get_column( col2(1), ID)
             call sqlite3_get_column( col2(2), NPIREC(NPI))
             call sqlite3_get_column( col2(3), IROPI(1,NPI))
             call sqlite3_get_column( col2(4), IBYRPI(NPI) )
             call sqlite3_get_column( col2(5), PIBKVL(NPI) )
             call sqlite3_get_column( col2(6), DISXCS )
             call sqlite3_get_column( col2(7), DISPRU )
             call sqlite3_get_column( col2(8), LPI(NPI) )
             call sqlite3_get_column( col2(9), IRDPI(NPI) )
             call sqlite3_get_column( col2(10), IASMPT(NPI) )

             IBYRPI(NPI) = IBYRPI(NPI) - UHBSYR
             IROPI(2,NPI) = 1
             PIDEF(NPI) = 0.0
             PIBKVL(NPI) = PIBKVL(NPI)*1000.0
             DISPER(NPI) = (DISXCS*DISPR1) + (DISPRU*DISPR2)

!             write(901,'(i4,a3,i4,a3,i4,a3,i4,a3,i4,a3,f8.2,a3,f8.2,a3,f8.2,a3,i4,a3,i4,a3,i4,a3,f8.2,a3,f8.2)' ) & 
!                    ID, ' ',NPI,' ',IROPI(1,NPI), ' ',IROPI(2,NPI),' ',IBYRPI(NPI),' ', &
!                    PIBKVL(NPI),' ',DISXCS,' ',DISPRU,' ',LPI(NPI), &
!                    ' ',IRDPI(NPI),' ',IASMPT(NPI),' ',PIDEF(NPI),' ',DISPER(NPI)
          enddo

          SAVNPI=NPI

          deallocate(col2)
          finished = .FALSE.


          allocate ( col3(4) )
        ! QUERY THE DATABASE TABLE
          call sqlite3_column_query( col3(1), 'RECORD', sqlite_int )
          call sqlite3_column_query( col3(2), 'YR', sqlite_int )
          call sqlite3_column_query( col3(3), 'PIDFS', sqlite_real )
          call sqlite3_column_query( col3(4), 'PIRCS', sqlite_real )


          call sqlite3_prepare_select( db, 'V_EMM_PHASEIN_RECOV_YR', col3, stmt3, 'WHERE YR <= 20')

!          write(901,'(a64)') 'V_EMM_PHASEIN_RECOV_YR'
!
!          write (901,'(a8,a2,a8,a2,a8,a2,a8,a2)') 'RECORD',' ', & 
!                'YR',' ','PIDFS',' ','PIRCS'

        ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
          i = 0
          do
             call sqlite3_next_row( stmt3, col3, finished )
             if ( finished ) exit

             call sqlite3_get_column( col3(1), NPIREC2)

             do j = 1, SAVNPI
              if (NPIREC2 .eq. NPIREC(j)) then 
                NPITMP = j
              end if 
             enddo

             call sqlite3_get_column( col3(2), IYRTMP)
             call sqlite3_get_column( col3(3), PIDFS(IYRTMP,NPITMP))
             call sqlite3_get_column( col3(4), PIRCS(IYRTMP,NPITMP))

          enddo

          deallocate(col3)

! FILL THE SCHEDULES WITH 1.0 IN OUT YEARS

          DO j = 1, SAVNPI
            ICT = 0
            ICT1 = 0
            DO IYR=1,LPI(j)
                IF (ICT .EQ. 1) PIDFS(IYR,j) = 1.0
                IF (PIDFS(IYR,j) .EQ. 1.0) ICT = 1
                IF (ICT1 .EQ. 1) PIRCS(IYR,j) = 1.0
                IF (PIRCS(IYR,j) .EQ. 1.0) ICT1 = 1
 !             write(901,'(i4,a3,i4,a3,f8.2,a3,f8.2)' ) & 
!                    NPITMP, ' ',IYRTMP,' ',PIDFS(IYRTMP,NPITMP), ' ',PIRCS(IYRTMP,NPITMP)
                
            END DO
          END DO

          call sqlite3_close( db )
        
        END SUBROUTINE PHASEINSQL
        
        SUBROUTINE NUCDECOMMSQL
          USE SQLITE

          IMPLICIT NONE
          !**************************************************************************
          ! THIS SUBROUTINE READS IN THE NUCLEAR DECOMMISSIONING DATA
          ! It is called only in the first forecast year processing.
          ! totals are in nominal dollars.
          INTEGER IN           ! unit number assigned to the ND input file
          !**************************************************************************
          ! get includes in order later in the process
            include 'parametr'   !
            include 'emmparm'
            include 'control'    !
            include 'ncntrl'     !
            include 'macout'     !
            include 'efpnd'
            include 'efpgen'
            include 'eusprc'
            include 'efpint'

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

          ! Locals
            real*4                                     :: RATE
            real*4                                     :: RT 
            real*4                                     :: NUMINR
            real*4                                     :: ESTP 
            real*4                                     :: NDAFCR
            real*4                                     :: RANN  ! real annuity amount in NDCBALY
            integer                                    :: IY 
            integer                                    :: IYB 
            integer                                    :: IYC
            integer                                    :: N 
            integer                                    :: I
            integer                                    :: K
            integer                                    :: J
            integer                                    :: M
            integer                                    :: YRIN(5)
            character(len=256)                         :: LINE
            real                                       :: NDTAXRTMP(NDLIFE)
            real                                       :: NESTESCTMP(NDLIFE)
              
            call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )      

            allocate ( col(10) )
!            ! QUERY THE DATABASE TABLE
              call sqlite3_column_query( col(1), 'NDADMNR', sqlite_real )
              call sqlite3_column_query( col(2), 'NDEARNR', sqlite_real )
              call sqlite3_column_query( col(3), 'NDCATCH', sqlite_int )
              call sqlite3_column_query( col(4), 'NDTCMON', sqlite_real )
              call sqlite3_column_query( col(5), 'NDPSER_1', sqlite_real )
              call sqlite3_column_query( col(6), 'NDPSER_2', sqlite_real )
              call sqlite3_column_query( col(7), 'NDPSER_3', sqlite_real )
              call sqlite3_column_query( col(8), 'NDPSCU_1', sqlite_real )
              call sqlite3_column_query( col(9), 'NDPSCU_2', sqlite_real )
              call sqlite3_column_query( col(10), 'NDPSCU_3', sqlite_real )
              call sqlite3_prepare_select( db, 'V_EMM_PHASEIN_DECOMM_PARM_GLOB', col, stmt)    
!              write(901,'(a64)') 'V_EMM_PHASEIN_DECOMM_PARM_GLOB'
!
!              write (901,'(a8,a2,a8,a2,a8,a2,a8,a2,a8,a2,a8,a2,a8,a2,a8,a2,a8,a2,a8)') 'NDADMNR',' ', & 
!                    'NDEARNR',' ','NDCATCH',' ','NDTCMON',' ','NDPSER_1',' ','NDPSER_2',' ','NDPSER_3', &
!                    'NDPSCU_1',' ','NDPSCU_2',' ','NDPSCU_3'             
              do
                call sqlite3_next_row( stmt, col, finished )
                if ( finished ) exit    
                call sqlite3_get_column( col(1), NDADMNR )
                call sqlite3_get_column( col(2), NDEARNR )
                call sqlite3_get_column( col(3), NDCATCH )
                call sqlite3_get_column( col(4), NDTCMON )
                call sqlite3_get_column( col(5), NDPSER(1) )
                call sqlite3_get_column( col(6), NDPSER(2) )
                call sqlite3_get_column( col(7), NDPSER(3) )
                call sqlite3_get_column( col(8), NDPSCU(1) )
                call sqlite3_get_column( col(9), NDPSCU(2) )
                call sqlite3_get_column( col(10), NDPSCU(3) )  
!                write (*,'(f8.2,a3,f8.2,a3,i4,a3,f8.2,a3,f8.2,a3,f8.2,a3,f8.2,a3,f8.2,a3,f8.2,a3,f8.2)') NDADMNR,' ', & 
!                NDEARNR,' ',NDCATCH,' ',NDTCMON,' ',NDPSER(1),' ',NDPSER(2),' ',&
!                NDPSER(3),' ',NDPSCU(1),' ',NDPSCU(2),' ',NDPSCU(3)                
              end do              
            deallocate ( col )            
!           finished = .FALSE.              
              
            allocate ( col2(3) )
            ! QUERY THE DATABASE TABLE
              call sqlite3_column_query( col2(1), 'YEAR', sqlite_int )
              call sqlite3_column_query( col2(2), 'NDTAXR', sqlite_real )
              call sqlite3_column_query( col2(3), 'NESTESC', sqlite_real )
              call sqlite3_prepare_select( db, 'V_EMM_PHASEIN_DECOMM_PARMYR', col2, stmt2)            
            M=1
            do
              call sqlite3_next_row( stmt2, col2, finished )
              if ( finished ) exit
              call sqlite3_get_column( col2(1), IY )
              YRIN(M) = IY
              call sqlite3_get_column( col2(2), NDTAXRTMP(M) )
              call sqlite3_get_column( col2(3), NESTESCTMP(M) )
              M=M+1
            enddo           
            IYC = 1
            do I = 1,2
              if (I .eq. 1) then
                IYB = IYC
                NDTAXR(YRIN(I)-USYEAR(1)+1) = NDTAXRTMP(I)
                NESTESC(YRIN(I)-USYEAR(1)+1) = NESTESCTMP(I)
              else
                do J = 2,NDLIFE
                  if ( j .le. M-1 ) then
                  !IYC = IYC + 1
! !         store the new entry in appropriet array element  
                    NDTAXR(YRIN(J)-USYEAR(1)+1) = NDTAXRTMP(J)
                    NESTESC(YRIN(J)-USYEAR(1)+1) = NESTESCTMP(J)
                  if ( YRIN(J) - USYEAR(1) .GT. IYB ) then 
                      do K = IYB+1,YRIN(J)-USYEAR(1)  
                        NDTAXR(K) = NDTAXR(IYB)
                        NESTESC(K) = NESTESC(IYB)     
                        IYC = IYC + 1
                      enddo    
                  end if     
! !       backup last year read
                  IYB = YRIN(J) - USYEAR(1) + 1
                  end if
!                    IF(IYB.NE.IYC) STOP 'uefp: DEBUG IYB<>IYC / aligning logic.'
                enddo
              end if
            enddo
! !     if not all years are specified assume the last specified year data
! !     is valid for all remaining years of Nuc's life
            do K = IYB+1, NDLIFE
              NDTAXR(K) = NDTAXR(IYB)
              NESTESC(K) = NESTESC(IYB)
              IYC = IYC + 1
            enddo            
            deallocate ( col2 ) 
            finished = .FALSE.

            allocate ( col3(12) )
            ! QUERY THE DATABASE TABLE
              call sqlite3_column_query( col3(1), 'N', sqlite_int )
              call sqlite3_column_query( col3(2), 'EMM_REG', sqlite_int )
              call sqlite3_column_query( col3(3), 'CAPACITY', sqlite_real )
              call sqlite3_column_query( col3(4), 'NUPR', sqlite_real )
              call sqlite3_column_query( col3(5), 'NDEST', sqlite_real )               
              call sqlite3_column_query( col3(6), 'NDESTY', sqlite_int )             
              call sqlite3_column_query( col3(7), 'NDCBAL', sqlite_real )  
              call sqlite3_column_query( col3(8), 'NDCAPMT', sqlite_real )                  
              call sqlite3_column_query( col3(9), 'NDCBALY', sqlite_int )  
              call sqlite3_column_query( col3(10), 'NRET', sqlite_int )  
              call sqlite3_column_query( col3(11), 'NDERET', sqlite_int )  
              call sqlite3_column_query( col3(12), 'NDPRMON', sqlite_int ) 
              call sqlite3_prepare_select( db, 'V_EMM_PHASEIN_DECOMM', col3, stmt3)
              do
                call sqlite3_next_row( stmt3, col3, finished )
                if ( finished ) exit   
                call sqlite3_get_column( col3(1), N )
                call sqlite3_get_column( col3(2), NUREG(N) )
                call sqlite3_get_column( col3(3), NUCAP(N) )
                call sqlite3_get_column( col3(4), NUPPR(N) )
                call sqlite3_get_column( col3(5), NDEST(N) )
                call sqlite3_get_column( col3(6), NDESTY(N) )
                call sqlite3_get_column( col3(7), NDCBAL(N) )
                call sqlite3_get_column( col3(8), NDCAPMT(N) )
                call sqlite3_get_column( col3(9), NDCBALY(N) )
                call sqlite3_get_column( col3(10), NDRET(N) )
                call sqlite3_get_column( col3(11), NDERET(N) )
                call sqlite3_get_column( col3(12), NDPRMON(N) )                
!                Write(*,*)'DECOM ',N,NUREG(N),NUCAP(N),NUPPR(N),NDEST(N)
              enddo
              NUCNUM = N

             deallocate ( col3 ) 
             
          call sqlite3_close( db )

        END SUBROUTINE NUCDECOMMSQL



        SUBROUTINE EFPCNTLSQL
          USE SQLITE

          IMPLICIT NONE
!*****************************************************************
!     THIS SUBROUTINE READS IN THE EFPCNTL FILE CONTAINING CONTROL OPTIONS FOR REPORT WRITING
!*****************************************************************
! INPUT VARIABLES:
!     UNYEAR = NUMBER OF YEARS
! INTERNAL VARIABLES:
!     NRGN1 = NUMBER OF REGIONS PLUS ONE FOR NATIONAL Total
!     NUM = # OF PLANT TYPES IN EACH Capacity Requirement Report
!     IYRFFY = first forecast year - use same as in rest of NEMS - UHBSYR (+1)
! OUTPUT VARIABLES:
!     NAME3 = RUN TITLE
!     NOPREG = NUMBER OF PRINT REGIONS
!     FRACRG = FRACTIONS USED TO AGGREGATE/DISAGGREGATE INPUT
!              REGIONS TO PRINT REGIONS
!     IPREG = FLAGS INDICATING WHICH REGIONS TO PRINT
!     IPOWN = FLAGS INDICATING WHICH OWNERSHIP CLASSES TO PRINT
!     IPCOMP = FLAGS INDICATING WHICH OPERATING COMPONENTS TO PRINT
!     IYRRPT = first year to be reported
!     IYRRL = YEAR IN WHICH TO REPORT REAL DOLLARS - use same as in rest of NEMS - YEARPR
!     IYRS = YEARS FOR5REPORT HEADERS
!     EFPREALDOLLARS = FLAG INDICATING IF REPORTS ARE IN REAL OR NOMINAL DOLLARS
!     NOCAP = # OF PLANT TYPE REPORTS
!     CAPTYP = LIST OF PLANT TYPES IN EACH PLANT TYPE REPORT
!     CAPTIT = PLANT TYPE NAMES FOR REPORT HEADERS
!     NUMCAP = # OF PLANT TYPES IN EACH PLANT TYPE REPORT
!*****************************************************************

          include 'parametr'
          include 'emmparm'
          include 'control'
          include 'efpcntrl'
          include 'efpname'
          include 'ncntrl'
          include 'efpgen'
          include 'eusprc'
          include 'efpint'
          include 'uefdout'
          type(sqlite_database)                      :: db
          type(sqlite_statement)                     :: stmt
          type(sqlite_statement)                     :: stmt2
          type(sqlite_statement)                     :: stmt3
          type(sqlite_column), dimension(:), pointer :: col
          type(sqlite_column), dimension(:), pointer :: col2
          type(sqlite_column), dimension(:), pointer :: col3
          character(len=40), pointer, dimension(:)   :: result
          character(len=80)                          :: errmsg
          character(len=4)                           :: intstr
          character(len=80)                          :: condstr
          logical                                    :: finished
          integer                                    :: I
          integer                                    :: IEFD
          integer                                    :: J
          integer                                    :: YR
          integer                                    :: IY
          integer                                    :: IYRFFY
          integer                                    :: NRGN1
          integer                                    :: NUM
          integer                                    :: IN
          logical                                    :: NEW             ! IS FILE NEW (.TRUE.) OR OLD (.FALSE.)

          call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )
          allocate ( col(1) )
        ! QUERY THE DATABASE TABLE
          call sqlite3_column_query( col(1), 'UNRGNS', sqlite_int )
          call sqlite3_prepare_select( db, 'V_EMM_UNRGNS', col, stmt )
          do
            call sqlite3_next_row( stmt, col, finished )
            if ( finished ) exit
            call sqlite3_get_column( col(1), NOPREG )
          enddo

          finished = .FALSE.

          allocate ( col2(3) )
        ! QUERY THE DATABASE TABLE
          call sqlite3_column_query( col2(1), 'EMM_REG', sqlite_int )
          call sqlite3_column_query( col2(2), 'EMM_REG1', sqlite_int )
          call sqlite3_column_query( col2(3), 'FRACRG', sqlite_real )
          call sqlite3_prepare_select( db, 'V_EMM_EFP_CNTL', col2, stmt2 )

        ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
          do
            call sqlite3_next_row( stmt2, col2, finished )
            if ( finished ) exit

            call sqlite3_get_column( col2(1), I )
            call sqlite3_get_column( col2(2), J )
            call sqlite3_get_column( col2(3), FRACRG(I,J) )
          enddo
                   
          deallocate ( col2 )
          finished = .FALSE.

          do I = 1,NOPREG
            write(intstr,'(I2.2)') I
            NAMES(I) = 'EMM REGION '//intstr
          enddo

          allocate ( col3(2) )
        ! QUERY THE DATABASE TABLE
          call sqlite3_column_query( col3(1), 'EMM_REG', sqlite_int )
          call sqlite3_column_query( col3(2), 'IPREG', sqlite_int )
          write(condstr,'(A)') 'where EMM_REG1 = 1'
          call sqlite3_prepare_select( db, 'V_EMM_EFP_CNTL', col3, stmt3, condstr )

          do
            call sqlite3_next_row( stmt3, col3, finished )
            if ( finished ) exit

            call sqlite3_get_column( col3(1), I )
            call sqlite3_get_column( col3(2), IPREG(I) )
          enddo

          deallocate ( col3 )

          IPREG( NOPREG + 1 ) = 1

          call sqlite3_close( db )

        END SUBROUTINE EFPCNTLSQL 

        SUBROUTINE ECPDAT_REG1

          USE SQLITE
          IMPLICIT NONE
 
 !      THIS SUBROUTINE READS CAPACITY EXPANSION INPUT DATA FROM ECPDATY.xlsx
 
       include 'parametr'
       include 'ncntrl'
       include 'emmparm'
       include 'control'
       include 'entcntl'
       include 'ecpcntl'
       include 'bildin'
       include 'dispinyr'
       include 'uefdout'
       include 'uecpout'
       include 'emission'
       include 'ecp_coal'
       include 'ecp_nuc'
       include 'macout'
       include 'eusprc'
       include 'edbdef'
       include 'elout'

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

       INTEGER*4 NRG,IRG,ECP,IC,IECP,FRG,IY,KNOTS
       
!       Write(6,*)'Inside ecpdat_reg1 '
       call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )
       allocate ( col(9) )
     ! QUERY THE DATABASE TABLE
       call sqlite3_column_query( col(1), 'EMM_REG', sqlite_int )
       call sqlite3_column_query( col(2), 'UPBLDTYP', sqlite_int )
       call sqlite3_column_query( col(3), 'UPRNWCASR', sqlite_int )
       call sqlite3_column_query( col(4), 'UPRNWRPSR', sqlite_int )
       call sqlite3_column_query( col(5), 'UPRNWSUNR', sqlite_int )
       call sqlite3_column_query( col(6), 'UPRNWBNKR', sqlite_int )    
       call sqlite3_column_query( col(7), 'UPRNWRGN', sqlite_int )
       call sqlite3_column_query( col(8), 'UPRNWCOGR', sqlite_int )
       call sqlite3_column_query( col(9), 'UPRNWCAPR', sqlite_real )

       call sqlite3_prepare_select( db, 'V_EMM_ECPDAT_REG', col, stmt)  

      ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
        do
        call sqlite3_next_row( stmt, col, finished )
        if ( finished ) exit

        call sqlite3_get_column( col(1), NRG)
        call sqlite3_get_column( col(2), UPBLDTYP(NRG))
        call sqlite3_get_column( col(3), UPRNWCASR(NRG))
        call sqlite3_get_column( col(4), UPRNWRPSR(NRG))
        call sqlite3_get_column( col(5), UPRNWSUNR(NRG))
        call sqlite3_get_column( col(6), UPRNWBNKR(NRG))
        call sqlite3_get_column( col(7), UPRNWRGN(NRG))
        call sqlite3_get_column( col(8), UPRNWCOGR(NRG))
        call sqlite3_get_column( col(9), UPRNWCAPR(NRG))
          
!        write(6,*)'check inside UPBLDTYP ',NRG, UPBLDTYP(NRG)
!        write(6,*)'check inside UPRNWCOGR ',NRG, UPRNWCOGR(NRG)
       
        end do
       
      
       deallocate ( col )
       call sqlite3_close( db )
       end subroutine ECPDAT_REG1
       
        SUBROUTINE ECPDAT_REG2(NERC)
             USE SQLITE
             IMPLICIT NONE
 
 !      THIS SUBROUTINE READS CAPACITY EXPANSION INPUT DATA FROM ECPDATY TABLES

       include 'parametr'
       include 'ncntrl'
       include 'emmparm'
       include 'control'
       include 'entcntl'
       include 'ecpcntl'
       include 'bildin'
       include 'dispinyr'
       include 'uefdout'
       include 'uecpout'
       include 'emission'
       include 'ecp_coal'
       include 'ecp_nuc'
       include 'macout'
       include 'eusprc'
       include 'edbdef'
       include 'elout'

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
       character(len=80)                          :: condstr

       INTEGER*4 NRG,IRG,ECP,IC,IECP,FRG,IY,KNOTS
       INTEGER NERC
       Integer tmp_EPNGRG(MNUMNR), tmp_EPCLRG(MNUMNR), tmp_EPCARG(MNUMNR)
       real tmp_EPMRM(MNUMNR)
       
!       write(6,*)'check inside REG2 nerc= ',nerc
       
       call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )
       allocate ( col(7) )
     ! QUERY THE DATABASE TABLE
       call sqlite3_column_query( col(1), 'EMM_REG', sqlite_int )
       call sqlite3_column_query( col(2), 'EPMRM', sqlite_real )
       call sqlite3_column_query( col(3), 'SR_RQMT_HGHT', sqlite_real )
       call sqlite3_column_query( col(4), 'SR_RQMT_DIFF', sqlite_real )
       call sqlite3_column_query( col(5), 'EPNGRG', sqlite_int )
       call sqlite3_column_query( col(6), 'EPCLRG', sqlite_int )
       call sqlite3_column_query( col(7), 'EPCARG', sqlite_int )
       write(condstr,'(A,I2.2)') 'where EMM_REG = ',NERC              
       call sqlite3_prepare_select( db, 'V_EMM_ECPDAT_REG', col, stmt, condstr)  

      ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
        do
        call sqlite3_next_row( stmt, col, finished )
        if ( finished ) exit

        call sqlite3_get_column( col(1), NRG)
        call sqlite3_get_column( col(2), EPMRM)   !tmp_EPMRM(NRG))
        call sqlite3_get_column( col(3), SR_RQMT_HGHT(NRG))
        call sqlite3_get_column( col(4), SR_RQMT_DIFF(NRG))
        call sqlite3_get_column( col(5), EPNGRG)  !tmp_EPNGRG(NRG))
        call sqlite3_get_column( col(6), EPCLRG)  !tmp_EPCLRG(NRG))
        call sqlite3_get_column( col(7), EPCARG)  !tmp_EPCARG(NRG))
        !EPMRM  = tmp_EPMRM(NERC)
        !EPNGRG = tmp_EPNGRG(NERC)        
        !EPCLRG = tmp_EPCLRG(NERC)        
        !EPCARG= tmp_EPCARG(NERC)        
        !write(6,*)'check inside REG2 ',' ',NERC,NRG,EPMRM,SR_RQMT_HGHT(NRG)

       end do
       deallocate ( col )
       call sqlite3_close( db )
       end subroutine ECPDAT_REG2 
       
            SUBROUTINE ECPDAT_REG_ECPT1
       
             USE SQLITE
             IMPLICIT NONE
       
 !      THIS SUBROUTINE READS CAPACITY EXPANSION INPUT DATA FROM ECPDATY TABLES
       
          include 'parametr'
          include 'ncntrl'
          include 'emmparm'
          include 'control'
          include 'entcntl'
          include 'ecpcntl'
          include 'bildin'
          include 'dispinyr'
          include 'uefdout'
          include 'uecpout'
          include 'emission'
          include 'ecp_coal'
          include 'ecp_nuc'
          include 'macout'
          include 'eusprc'
          include 'edbdef'
          include 'elout'
       
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
       
          INTEGER*4 NRG,IRG,ECP,IC,IECP,FRG,IY
       
          call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )
          allocate ( col(6) )
        ! QUERY THE DATABASE TABLE
          call sqlite3_column_query( col(1), 'EMM_REG', sqlite_int )
          call sqlite3_column_query( col(2), 'ECP_TYP', sqlite_int )
          call sqlite3_column_query( col(3), 'UPBLDREG', sqlite_real )
          call sqlite3_column_query( col(4), 'UPRNWBASR', sqlite_real )
          call sqlite3_column_query( col(5), 'UPRNWEXGR', sqlite_real )
          call sqlite3_column_query( col(6), 'UPRNWSHRR', sqlite_real )
          call sqlite3_prepare_select( db, 'V_EMM_ECPDAT_REG_ECPT', col, stmt)
       
       
         ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
           do
           call sqlite3_next_row( stmt, col, finished )
           if ( finished ) exit
       
           call sqlite3_get_column( col(1), NRG)
           call sqlite3_get_column( col(2), ECP)
           call sqlite3_get_column( col(3), UPBLDREG(ECP,NRG))
           call sqlite3_get_column( col(4), UPRNWBASR(ECP,NRG))
           call sqlite3_get_column( col(5), UPRNWEXGR(ECP,NRG))
           call sqlite3_get_column( col(6), UPRNWSHRR(ECP,NRG))
       
          end do
          deallocate ( col )
          call sqlite3_close( db )
          end subroutine ECPDAT_REG_ECPT1

          SUBROUTINE ECPDAT_REG_ECPT2(NERC)
          USE SQLITE
          IMPLICIT NONE
      
 !      THIS SUBROUTINE READS CAPACITY EXPANSION INPUT DATA FROM ECPDATY TABLES
      
       include 'parametr'
       include 'ncntrl'
       include 'emmparm'
       include 'control'
       include 'entcntl'
       include 'ecpcntl'
       include 'bildin'
       include 'dispinyr'
       include 'uefdout'
       include 'uecpout'
       include 'emission'
       include 'ecp_coal'
       include 'ecp_nuc'
       include 'macout'
       include 'eusprc'
       include 'edbdef'
       include 'elout'
      
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
       character(len=80)                          :: condstr
      
      
       INTEGER*4 NRG,IRG,ECP,IECP,NERC
       
       character(len=4)                           :: EMM_TEMP
       real     EPCRBRT_TEMP(ECP_D_CAP,MNUMNR)
       real     EPCO1RT_TEMP(ECP_D_CAP,MNUMNR)
       real     EPCO2RT_TEMP(ECP_D_CAP,MNUMNR)
       real     EPCH4RT_TEMP(ECP_D_CAP,MNUMNR)
       real     EPSOXRT_TEMP(ECP_D_CAP,MNUMNR)
       real     EPNOXRT_TEMP(ECP_D_CAP,MNUMNR)
       real     EPVOCRT_TEMP(ECP_D_CAP,MNUMNR)
       real     EPCOVR_TEMP(ECP_D_CAP,MNUMNR)
       real     EPCCRF_TEMP(ECP_D_CAP,MNUMNR)
       real     EPCFOM_TEMP(ECP_D_CAP,MNUMNR)
       real     EPRGM_TEMP(ECP_D_CAP,MNUMNR)
       real     EPACM_TEMP(ECP_D_CAP,MNUMNR)
       real     SR_INT_TEMP(ECP_D_CAP,MNUMNR)       
       Integer     EPCENSUS_TEMP(ECP_D_CAP,MNUMNR)
       
       write(EMM_TEMP,'(i4)'), NERC
       
       call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )
      
       allocate (col(16))
       call sqlite3_column_query( col(1), 'EMM_REG', sqlite_int )
       call sqlite3_column_query( col(2), 'ECP_TYP', sqlite_int )
       call sqlite3_column_query( col(3), 'EPCOVR', sqlite_real )
       call sqlite3_column_query( col(4), 'EPCCRF', sqlite_real )
       call sqlite3_column_query( col(5), 'EPCFOM', sqlite_real )
       call sqlite3_column_query( col(6), 'EPRGM',  sqlite_real )
       call sqlite3_column_query( col(7), 'EPACM',  sqlite_real )
       call sqlite3_column_query( col(8), 'EPCRBRT', sqlite_real )
       call sqlite3_column_query( col(9), 'EPCO1RT', sqlite_real )
       call sqlite3_column_query( col(10), 'EPCO2RT', sqlite_real )
       call sqlite3_column_query( col(11), 'EPCH4RT', sqlite_real )
       call sqlite3_column_query( col(12), 'EPSOXRT', sqlite_real )
       call sqlite3_column_query( col(13), 'EPNOXRT', sqlite_real )
       call sqlite3_column_query( col(14), 'EPVOCRT', sqlite_real )
       call sqlite3_column_query( col(15), 'SR_INT',  sqlite_real )
       call sqlite3_column_query( col(16), 'EPCENSUS', sqlite_int )
       write(condstr,'(A,I2.2)') 'where EMM_REG = ',NERC              
       call sqlite3_prepare_select( db, 'V_EMM_ECPDAT_REG_ECPT', col, stmt,condstr)
     
      ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
       do
        call sqlite3_next_row( stmt, col, finished )
        if ( finished ) exit
      
        call sqlite3_get_column( col(1), NRG)
        call sqlite3_get_column( col(2), ECP)
        call sqlite3_get_column( col(3), EPCOVR(ECP))  !EPCOVR_TEMP(ECP,NRG))
        call sqlite3_get_column( col(4), EPCCRF(ECP))  !EPCCRF_TEMP(ECP,NRG))
        call sqlite3_get_column( col(5), EPCFOM(ECP))  !EPCFOM_TEMP(ECP,NRG))
        call sqlite3_get_column( col(6), EPRGM(ECP))   !EPRGM_TEMP(ECP,NRG))
        call sqlite3_get_column( col(7), EPACM(ECP))   !EPACM_TEMP(ECP,NRG))
        call sqlite3_get_column( col(8), EPCRBRT_TEMP(ECP,NRG))
        call sqlite3_get_column( col(9), EPCO1RT_TEMP(ECP,NRG))
        call sqlite3_get_column( col(10),EPCO2RT_TEMP(ECP,NRG))
        call sqlite3_get_column( col(11),EPCH4RT_TEMP(ECP,NRG))
        call sqlite3_get_column( col(12),EPSOXRT_TEMP(ECP,NRG))
        call sqlite3_get_column( col(13),EPNOXRT_TEMP(ECP,NRG))
        call sqlite3_get_column( col(14),EPVOCRT_TEMP(ECP,NRG))
        call sqlite3_get_column( col(15),SR_INT(ECP,NRG))
        call sqlite3_get_column( col(16),EPCENSUS(ECP)) !EPCENSUS_TEMP(ECP,NRG))
!           Write(6,*)'check EPCENSUS1 in sql ',NERC,NRG,ECP,EPCENSUS_TEMP(ECP,NRG)
     
       end do
       deallocate ( col )
       
!       DO IECP = 1 , ECP_D_CAP
!          EPCOVR(IECP) = EPCOVR_TEMP(IECP,NERC)
!          EPCCRF(IECP) = EPCCRF_TEMP(IECP,NERC)
!          EPCFOM(IECP) = EPCFOM_TEMP(IECP,NERC)
!          EPRGM(IECP)  = EPRGM_TEMP(IECP,NERC)
!          EPACM(IECP)  = EPACM_TEMP(IECP,NERC)
!          EPCENSUS(IECP) = EPCENSUS_TEMP(IECP,NERC)
!          Write(6,*)'check EPCENSUS2 in sql ',NERC,IECP,EPCENSUS(IECP),EPCENSUS_TEMP(IECP,NERC)
!       ENDDO       
      DO IECP = 1 , 23
       EPCRBRT(IECP) = EPCRBRT_TEMP(IECP+55,NERC)
       EPCO1RT(IECP) = EPCO1RT_TEMP(IECP+55,NERC)
       EPCO2RT(IECP) = EPCO2RT_TEMP(IECP+55,NERC)
       EPCH4RT(IECP) = EPCH4RT_TEMP(IECP+55,NERC)
       EPSOXRT(IECP) = EPSOXRT_TEMP(IECP+55,NERC)
       EPNOXRT(IECP) = EPNOXRT_TEMP(IECP+55,NERC)
       EPVOCRT(IECP) = EPVOCRT_TEMP(IECP+55,NERC)
      END DO
       
              
       call sqlite3_close( db )
       end subroutine ECPDAT_REG_ECPT2   
       
       
       SUBROUTINE ECPDAT_REG_ECPT_P(NERC)

        USE SQLITE
        IMPLICIT NONE
      
 !      THIS SUBROUTINE READS CAPACITY EXPANSION INPUT DATA FROM ECPDATY TABLES
      
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'entcntl'
      include 'ecpcntl'
      include 'bildin'
      include 'dispinyr'
      include 'uefdout'
      include 'uecpout'
      include 'emission'
      include 'ecp_coal'
      include 'ecp_nuc'
      include 'macout'
      include 'eusprc'
      include 'edbdef'
      include 'elout'
      
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
      character(len=80)                          :: condstr
      
      
      INTEGER*4 NRG,IRG,ECP,IECP,POLLUTANT_TYP,NERC
      real EPEXT_TEMP(MNUMNR,ECP_D_CAP,MNPOLLUT)
      
      character(len=4)                           :: EMM_TEMP
      
      write(EMM_TEMP,'(i4)'), NRG
      call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )
   
      allocate (col(4))
      call sqlite3_column_query( col(1), 'EMM_REG', sqlite_int )
      call sqlite3_column_query( col(2), 'ECP_TYP', sqlite_int )
      call sqlite3_column_query( col(3), 'POLLUTANT_TYP', sqlite_int )
      call sqlite3_column_query( col(4), 'EPEXT', sqlite_real )
      write(condstr,'(A,I2.2)') 'where EMM_REG = ',NERC              
      call sqlite3_prepare_select( db, 'V_EMM_ECPDAT_REG_ECPT_P', col, stmt, condstr)
      
      ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
      do
      call sqlite3_next_row( stmt, col, finished )
      if ( finished ) exit
      
      call sqlite3_get_column( col(1), NRG)
      call sqlite3_get_column( col(2), ECP)
      call sqlite3_get_column( col(3), POLLUTANT_TYP)
      call sqlite3_get_column( col(4), EPEXT(ECP,POLLUTANT_TYP))  !EPEXT_TEMP(NRG,ECP,POLLUTANT_TYP))     
      !IF (NERC.eq.NRG)EPEXT(ECP,POLLUTANT_TYP)= EPEXT_TEMP(NERC,ECP,POLLUTANT_TYP)

       end do
      deallocate ( col )

!      DO ECP=1,ECP_D_CAP
!        DO POLLUTANT_TYP=1,MNPOLLUT
!           WRITE(6,*)'check epext in sql ',NERC,ECP,POLLUTANT_TYP,EPEXT(ECP,POLLUTANT_TYP)            
!        ENDDO
!      ENDDO

      
      call sqlite3_close( db )
      end subroutine ECPDAT_REG_ECPT_P
      
       SUBROUTINE ECPDAT_REG_ECPT_S(NERC)
      
        USE SQLITE
        IMPLICIT NONE
      
 !      THIS SUBROUTINE READS CAPACITY EXPANSION INPUT DATA FROM ECPDATY TABLES
      
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'entcntl'
      include 'ecpcntl'
      include 'bildin'
      include 'dispinyr'
      include 'uefdout'
      include 'uecpout'
      include 'emission'
      include 'ecp_coal'
      include 'ecp_nuc'
      include 'macout'
      include 'eusprc'
      include 'edbdef'
      include 'elout'
      
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
      character(len=80)                          :: condstr
      
      
      INTEGER*4 NRG,IRG,ECP,IECP,ECPDGS,NERC
      real EPDPCT_TEMP(MNUMNR,ECP_D_DGS),EPDAVD_TEMP(MNUMNR,ECP_D_DGS)
      
      character(len=4)                           :: EMM_TEMP
      
      write(EMM_TEMP,'(i4)'), NRG
      call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )
      
      allocate (col(4))
      call sqlite3_column_query( col(1), 'EMM_REG', sqlite_int )
      call sqlite3_column_query( col(2), 'ECPDGS', sqlite_int )
      call sqlite3_column_query( col(3), 'EPDPCT', sqlite_real )
      call sqlite3_column_query( col(4), 'EPDAVD', sqlite_real )
      write(condstr,'(A,I2.2)') 'where EMM_REG = ',NERC   
      call sqlite3_prepare_select( db, 'V_EMM_ECPDAT_REG_ECPT_S', col, stmt, condstr)
           
      
      ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
      do
      call sqlite3_next_row( stmt, col, finished )
      if ( finished ) exit
      
      call sqlite3_get_column( col(1), NRG)
      call sqlite3_get_column( col(2), ECPDGS)
      call sqlite3_get_column( col(3), EPDPCT(ECPDGS))  !EPDPCT_TEMP(NRG,ECPDGS))
      call sqlite3_get_column( col(4), EPDAVD(ECPDGS))  !EPDAVD_TEMP(NRG,ECPDGS))
       !IF (NERC.eq.NRG)EPDPCT(ECPDGS)= EPDPCT_TEMP(NRG,ECPDGS)
       !IF (NERC.eq.NRG)EPDAVD(ECPDGS)= EPDAVD_TEMP(NRG,ECPDGS)
     
      end do
      deallocate ( col )
      
 !     DO ECPDGS=1,ECP_D_DGS
 !          WRITE(6,*)'check EPDAVD(in sql ',NERC,ECPDGS,EPDAVD(ECPDGS)          
 !     ENDDO
          
      
      call sqlite3_close( db )
      end subroutine ECPDAT_REG_ECPT_S
      
      
      subroutine ECPDAT_REG_FUEL
 
        USE SQLITE
        IMPLICIT NONE
      
      !      THIS SUBROUTINE READS CAPACITY EXPANSION INPUT DATA FROM ECPDATY tables
      
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'entcntl'
      include 'ecpcntl'
      include 'bildin'
      include 'dispinyr'
      include 'uefdout'
      include 'uecpout'
      include 'emission'
      include 'ecp_coal'
      include 'ecp_nuc'
      include 'macout'
      include 'eusprc'
      include 'edbdef'
      include 'elout'
      
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
      
      
      INTEGER*4 NRG,IRG,ECP,IECP,FRG
      !real rtemp16(mnumnr,maxnfr)
      character(len=4)                           :: EMM_TEMP
      
      write(EMM_TEMP,'(i4)'), NRG
      call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )
      
      allocate (col(3))
      call sqlite3_column_query( col(1), 'EMM_REG', sqlite_int )
      call sqlite3_column_query( col(2), 'FUEL_RGN', sqlite_int )
      call sqlite3_column_query( col(3), 'FL_CNXT_CST', sqlite_real )
      call sqlite3_prepare_select( db, 'V_EMM_ECPDAT_REG_FUEL', col, stmt)
     
      ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
      do
      call sqlite3_next_row( stmt, col, finished )
      if ( finished ) exit
      
      call sqlite3_get_column( col(1), NRG)
      call sqlite3_get_column( col(2), FRG)
      call sqlite3_get_column( col(3), FL_CNXT_CST(NRG,FRG))
 !     Write(6,*)'fl_cnxt_cst in uesql ',NRG,FRG,FL_CNXT_CST(NRG,FRG)
      
      end do
          
      deallocate ( col )
      call sqlite3_close( db )
      end subroutine ECPDAT_REG_FUEL
      
      subroutine ECPDAT_REG_NOX
!      THIS SUBROUTINE READS CAPACITY EXPANSION INPUT DATA FROM ECPDATY TABLES
      
        USE SQLITE
        IMPLICIT NONE
            
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'entcntl'
      include 'ecpcntl'
      include 'bildin'
      include 'dispinyr'
      include 'uefdout'
      include 'uecpout'
      include 'emission'
      include 'ecp_coal'
      include 'ecp_nuc'
      include 'macout'
      include 'eusprc'
      include 'edbdef'
      include 'elout'
      
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
      
      
      INTEGER*4 NRG,IRG,ECP,IECP,NOXRG
      !real rtemp(NOX_D_GRP)
      character(len=4)                           :: EMM_TEMP
      
      write(EMM_TEMP,'(i4)'), NRG
      call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )
      
      allocate (col(3))
      call sqlite3_column_query( col(1), 'EMM_REG', sqlite_int )
      call sqlite3_column_query( col(2), 'NOX_GRP', sqlite_int )
      call sqlite3_column_query( col(3), 'NOX_RGN', sqlite_int )
      call sqlite3_prepare_select( db, 'V_EMM_ECPDAT_REG_NOX', col, stmt)
      
      ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
      do
      call sqlite3_next_row( stmt, col, finished )
      if ( finished ) exit
      
      call sqlite3_get_column( col(1), NRG)
      call sqlite3_get_column( col(2), NOXRG)
      call sqlite3_get_column( col(3), NOX_RGN(NRG,NOXRG))
      
!      WRITE(6,*)'nox_rgn ',NRG,NOXRG,NOX_RGN(NRG,NOXRG)
      
      end do
      deallocate ( col )
      call sqlite3_close( db )
      end subroutine ECPDAT_REG_NOX
      
      
      SUBROUTINE ECPDAT_REG_REG
      
        USE SQLITE
        IMPLICIT NONE
      
 !      THIS SUBROUTINE READS CAPACITY EXPANSION INPUT DATA FROM ECPDATY TABLES
     
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'entcntl'
      include 'ecpcntl'
      include 'bildin'
      include 'dispinyr'
      include 'uefdout'
      include 'uecpout'
      include 'emission'
      include 'ecp_coal'
      include 'ecp_nuc'
      include 'macout'
      include 'eusprc'
      include 'edbdef'
      include 'elout'
      
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
      
      
      INTEGER*4 NRG,IRG,ECP,IECP,NRG2
      !real rtemp(NOX_D_GRP)
      !character(len=4)                           :: EMM_TEMP
      
     ! write(EMM_TEMP,'(i4)'), NRG
      call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )
      
      allocate (col(5))
      call sqlite3_column_query( col(1), 'EMM_REG', sqlite_int )
      call sqlite3_column_query( col(2), 'EMM_REG2', sqlite_int )
      call sqlite3_column_query( col(3), 'EMM_CNXT_CST', sqlite_real )
      call sqlite3_column_query( col(4), 'EMM_HVDC_CST', sqlite_real )
      call sqlite3_column_query( col(5), 'EMM_HVDC_LOSS', sqlite_real )
      call sqlite3_prepare_select( db, 'V_EMM_ECPDAT_REG_REG', col, stmt)
      
      ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
      do
      call sqlite3_next_row( stmt, col, finished )
      if ( finished ) exit
      
      call sqlite3_get_column( col(1), NRG)
      call sqlite3_get_column( col(2), NRG2)
      call sqlite3_get_column( col(3), EMM_CNXT_CST(NRG,NRG2))
      call sqlite3_get_column( col(4), EMM_CNDC_CST(NRG,NRG2))
      call sqlite3_get_column( col(5), EPTLOSS(NRG,NRG2))
     
!     Write(6,*)' EMM_CNXT_CST ',NRG,NRG2,EMM_CNXT_CST(NRG,NRG2)
      
      end do
      deallocate ( col )
      call sqlite3_close( db )
      end subroutine ECPDAT_REG_REG

      SUBROUTINE ECPDAT_REG_YR

        USE SQLITE
        IMPLICIT NONE
      
  !      THIS SUBROUTINE READS CAPACITY EXPANSION INPUT DATA FROM ECPDATY TABLES
     
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'entcntl'
      include 'ecpcntl'
      include 'bildin'
      include 'dispinyr'
      include 'uefdout'
      include 'uecpout'
      include 'emission'
      include 'ecp_coal'
      include 'ecp_nuc'
      include 'macout'
      include 'eusprc'
      include 'edbdef'
      include 'elout'
      
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
      
      INTEGER*4 NRG,IRG,ECP,IC,IECP,FRG,IY,KNOTS
      
      
      call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )
      allocate ( col(4) )
      ! QUERY THE DATABASE TABLE
      call sqlite3_column_query( col(1), 'EMM_REG', sqlite_int )
      call sqlite3_column_query( col(2), 'YEAR', sqlite_int )
      call sqlite3_column_query( col(3), 'UPCFGEN', sqlite_real )
      call sqlite3_column_query( col(4), 'UPRNWBNDR', sqlite_real )
      call sqlite3_prepare_select( db, 'V_EMM_ECPDAT_REG_YR', col, stmt)
      
      
      ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
      do
      call sqlite3_next_row( stmt, col, finished )
      if ( finished ) exit
      
      call sqlite3_get_column( col(1), NRG)
      call sqlite3_get_column( col(2), IY)
      call sqlite3_get_column( col(3), UPCFGEN(IY-1989,NRG))
      call sqlite3_get_column( col(4), UPRNWBNDR(IY-1989,NRG))
!      WRITE(6,*)'upcfgen ',IY,NRG,UPCFGEN(IY-1989,NRG)
      
      end do
      deallocate ( col )
      call sqlite3_close( db )
      end subroutine ECPDAT_REG_YR

      Subroutine EMM_MSW
 !      THIS SUBROUTINE READS DATA FROM MSW TABLES
      
        USE SQLITE
        implicit none
        include 'parametr'
        include 'ncntrl'
        include 'emmparm'
        include 'wrenew'
        include 'wmscomon'
        include 'control'
        include 'uefdout'
      
        type(sqlite_database)                      :: db
        type(sqlite_statement)                     :: stmt
        type(sqlite_statement)                     :: stmt2
        type(sqlite_statement)                     :: stmt3
        type(sqlite_statement)                     :: stmt4
        type(sqlite_statement)                     :: stmt5
        type(sqlite_statement)                     :: stmt6
        type(sqlite_column), dimension(:), pointer :: col
        type(sqlite_column), dimension(:), pointer :: col2
        type(sqlite_column), dimension(:), pointer :: col3
        type(sqlite_column), dimension(:), pointer :: col4
        type(sqlite_column), dimension(:), pointer :: col5
        type(sqlite_column), dimension(:), pointer :: col6
        character(len=40), pointer, dimension(:)   :: result
        character(len=80)                          :: errmsg
        logical                                    :: finished
        integer ITECHNO
        INTEGER NRG,CRG,TYP,ID,IYR,MTYP,ISTP,IRG   
        Real CCOSTM(MNUMNR,MSPTMX), FCOSTM(MNUMNR,MSPTMX),VCOSTM(MNUMNR,MSPTMX)
        data ITECHNO/ 5/                ! MSW is technology no 5        
      
        call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )
        
       !V_EMM_MSWCTON data read in-------------------------------------------        
        allocate ( col(3) )
        ! QUERY THE DATABASE TABLE
        call sqlite3_column_query( col(1), 'EMM_REG', sqlite_int )
        call sqlite3_column_query( col(2), 'CEN_DIV', sqlite_int )
        call sqlite3_column_query( col(3), 'WMSCTON', sqlite_real )
        call sqlite3_prepare_select( db, 'V_EMM_MSWCTON', col, stmt)            
        ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
        do
        call sqlite3_next_row( stmt, col, finished )
        if ( finished ) exit      
        call sqlite3_get_column( col(1), NRG)
        call sqlite3_get_column( col(2), CRG)
        call sqlite3_get_column( col(3), WMSCTON(NRG,CRG))
        end do
        deallocate ( col )
               
        !V_EMM_MSW_POP data read in------------------------------------------- 
      
         allocate ( col2(7) )
         ! QUERY THE DATABASE TABLE
         call sqlite3_column_query( col2(1), 'ID', sqlite_int )         
         call sqlite3_column_query( col2(2), 'EMM_REG', sqlite_int )
         call sqlite3_column_query( col2(3), 'LFSHR', sqlite_real )
         call sqlite3_column_query( col2(4), 'PCSHR', sqlite_real )
         call sqlite3_column_query( col2(5), 'FSHR', sqlite_real )
         call sqlite3_column_query( col2(6), 'WNRPOPSHR', sqlite_real )
		 call sqlite3_column_query( col2(7), 'WIPSHR', sqlite_real )
         call sqlite3_prepare_select( db, 'V_EMM_MSW_POP', col2, stmt2)
         ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
         do
         call sqlite3_next_row( stmt2, col2, finished )
         if ( finished ) exit
         call sqlite3_get_column( col2(2), NRG)
         call sqlite3_get_column( col2(3), LFSHR(NRG))
         call sqlite3_get_column( col2(4), PCSHR(NRG))
         call sqlite3_get_column( col2(5), FSHR(NRG))
         call sqlite3_get_column( col2(6), WNRPOPSHR(NRG))
		 call sqlite3_get_column( col2(7), WIPSHR(NRG))
         end do
         deallocate ( col2 )
      
         allocate ( col3(3) )
         ! QUERY THE DATABASE TABLE
         call sqlite3_column_query( col3(1), 'EMM_REG', sqlite_int )
         call sqlite3_column_query( col3(2), 'WRGLF_TYP', sqlite_int )
         call sqlite3_column_query( col3(3), 'YIELD_RATIOS', sqlite_real )
         call sqlite3_prepare_select( db, 'V_EMM_MSW_YIELD', col3, stmt3)
         ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
         do
         call sqlite3_next_row( stmt3, col3, finished )
         if ( finished ) exit
         call sqlite3_get_column( col3(1), NRG)
         call sqlite3_get_column( col3(2), TYP)
         call sqlite3_get_column( col3(3), YIELD_RATIOS(NRG,TYP))
         end do
         deallocate ( col3 )    
!     
!       !V_EMM_MSW_REG_ANN data read in------------------------------------------- 
!
         allocate ( col4(5) )
        ! QUERY THE DATABASE TABLE
         call sqlite3_column_query( col4(1), 'EMM_REG', sqlite_int )
         call sqlite3_column_query( col4(2), 'YEAR', sqlite_int )
         call sqlite3_column_query( col4(3), 'MSWSUPPADD', sqlite_real )
         call sqlite3_column_query( col4(4), 'WNGMSEL', sqlite_real )
         call sqlite3_column_query( col4(5), 'WNCMSELN', sqlite_real )
         call sqlite3_prepare_select( db, 'V_EMM_MSW_REG_ANN', col4, stmt4)
         ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
         do
         call sqlite3_next_row( stmt4, col4, finished )
         if ( finished ) exit
         call sqlite3_get_column( col4(1), NRG)
         call sqlite3_get_column( col4(2), IYR)
         call sqlite3_get_column( col4(3), MSWSUPPADD(IYR-1989,NRG))
         call sqlite3_get_column( col4(4), WNGMSEL(IYR-1989,NRG))
         call sqlite3_get_column( col4(5), WNCMSELN(IYR-1989,NRG))
         end do
        deallocate ( col4 )    
 
!     
!       !V_EMM_MSW_CEN_ANN data read in------------------------------------------- 
!
         allocate ( col5(4) )
        ! QUERY THE DATABASE TABLE
         call sqlite3_column_query( col5(1), 'YEAR', sqlite_int )
         call sqlite3_column_query( col5(2), 'CEN_DIV', sqlite_int )
         call sqlite3_column_query( col5(3), 'WNGMSELC', sqlite_real )
         call sqlite3_column_query( col5(4), 'WNCMSEL', sqlite_real )
         call sqlite3_prepare_select( db, 'V_EMM_MSW_CEN_ANN', col5, stmt5)
         ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
         do
         call sqlite3_next_row( stmt5, col5, finished )
         if ( finished ) exit
         call sqlite3_get_column( col5(1), IYR)
         call sqlite3_get_column( col5(2), CRG)
         call sqlite3_get_column( col5(3), WNGMSELC(IYR-1989,CRG))
         call sqlite3_get_column( col5(4), WNCMSEL(IYR-1989,CRG))
         end do
        deallocate ( col5 )    
 
 
         allocate ( col6(5) )
         ! QUERY THE DATABASE TABLE
          call sqlite3_column_query( col6(1), 'EMM_REG', sqlite_int )
          call sqlite3_column_query( col6(2), 'MSPTMX', sqlite_int )
          call sqlite3_column_query( col6(3), 'UTCSFN', sqlite_real )
          call sqlite3_column_query( col6(4), 'FOMMULT', sqlite_real )
          call sqlite3_column_query( col6(5), 'VOMMULT', sqlite_real )
          call sqlite3_prepare_select( db, 'V_EMM_MSW_STEP', col6, stmt6)
          ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
          do
          call sqlite3_next_row( stmt6, col6, finished )
          if ( finished ) exit
          call sqlite3_get_column( col6(1), NRG)
          call sqlite3_get_column( col6(2), ISTP)
          call sqlite3_get_column( col6(3), CCOSTM(NRG,ISTP))
          call sqlite3_get_column( col6(4), FCOSTM(NRG,ISTP))
          call sqlite3_get_column( col6(5), VCOSTM(NRG,ISTP))
          end do
        deallocate ( col6 )    
 
 
      call sqlite3_close( db )
 
      DO NRG = 1 , MNUMNR 
        DO ISTP = 1 , MSPTMX
          UTCSFN(NRG,ITECHNO,ISTP) = CCOSTM(NRG,ISTP)
          UTFXFN(NRG,ITECHNO,ISTP) = FCOSTM(NRG,ISTP)
          UTVRFN(NRG,ITECHNO,ISTP) = VCOSTM(NRG,ISTP)
        ENDDO
      ENDDO
 
        end subroutine EMM_MSW
 !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXstarting reading ldsm tables hereXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      Subroutine EMM_LDSM
        USE SQLITE
        implicit none
!**********Declaring variables and common blocks via NEMS include files********
      INTEGER*2 ONE
      PARAMETER(ONE=1)
      include 'parametr' !<< nems parameter declarations
      include 'dsmdimen' !<< all ldsm parameter declarations
      include 'emmparm'
      include 'ecpcntl'
      include 'dispett'
!**********Declaring LDSM variables and common blocks via NEMS include files********
      include 'dsmunits' !<< include file with unit number for ldsm message file
      include 'dsmfmgrd' !<< file_mgr variables declarations
      include 'dsmsectr' !<< sector specific data and other associated variables
      include 'dsmtoefd' !<< commumication with efd
      include 'dsmnemsc' !<< results of ldsm to be passed to the rest of nems
      include 'dsmtfecp' !<< communication with ecp
      include 'dsmtfefp' !<< communication with efp
      include 'dsmcaldr' !<< calendar data
      include 'dsmnercr' !<< nerc region data
      include 'dsmoptdb' !<< dsm option data base
      include 'dsmhelm'
      include 'dsmrept'  !<< ldsm reports specificaton
      include 'control'
      include 'ncntrl'
      include 'wrenew'    ! mapping vars for ecp and efd hours to group number
      include 'eusprc'
      include 'edbdef'
      include 'uefdout'

      
      type(sqlite_database)                      :: db
      type(sqlite_statement)                     :: stmt
      type(sqlite_statement)                     :: stmt2
      type(sqlite_statement)                     :: stmt3
      type(sqlite_statement)                     :: stmt4
      type(sqlite_statement)                     :: stmt5
      type(sqlite_statement)                     :: stmt6
      type(sqlite_statement)                     :: stmt7
      type(sqlite_statement)                     :: stmt8
      type(sqlite_statement)                     :: stmt9
      type(sqlite_statement)                     :: stmt10
      type(sqlite_statement)                     :: stmt11
      type(sqlite_column), dimension(:), pointer :: col
      type(sqlite_column), dimension(:), pointer :: col2
      type(sqlite_column), dimension(:), pointer :: col3
      type(sqlite_column), dimension(:), pointer :: col4
      type(sqlite_column), dimension(:), pointer :: col5
      type(sqlite_column), dimension(:), pointer :: col6
      type(sqlite_column), dimension(:), pointer :: col7
      type(sqlite_column), dimension(:), pointer :: col8
      type(sqlite_column), dimension(:), pointer :: col9
      type(sqlite_column), dimension(:), pointer :: col10
      type(sqlite_column), dimension(:), pointer :: col11
      character(len=40), pointer, dimension(:)   :: result
      character(len=80)                          :: errmsg
      logical                                    :: finished
      
      INTEGER*4 nNERCreg_sq,id,NUMSEC_sq,MNUMNR_sq,NEUSGRP_sq,NREPREG_sq,MNUMCR_sq,MAXSEC_sq,NEUSE,EUGRP_sq(MAXEU+1),NEUSES_sq(MAXSEC)
      INTEGER*4 I,J,K,p,n,q, new_sq, RRlist_sq(MAXRLST),CRlist_sq(MAXRLST)
      INTEGER*4 RdecTYPix_sq(MAXDECT),RRlistN_sq,CRlistN_sq,REGNUM
      CHARACTER*1  RdecTYPid_sq(MAXDECT),RRlistID_sq(MAXRLST),CRlistID_sq(MAXRLST),secname
      REAL EUELAS_sq(MNUMNR,MAXSEC,MAXEUGRP),UQTDLS_sq(mnumnr),MEFAC_sq(MNUMNR),DEMINT_sq(mnumnr)
      REAL*4 MappCtoN_sq(MNUMNR,MNUMCR,MAXSEC),x_sq
      CHARACTER*8 NERCnam_sq(mnumnr), EUNAM_sq(MAXEU), SLNAM_sq(MAXSEC), m_sq, SECTOR_sq
      CHARACTER*8 y_sq, LREG(MAXRLST), LREG_sq(MAXRLST)
      !V_EMM_MSWCTON data read in------------------------------------------- 
      
      call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )
            allocate ( col(3) )
                 ! QUERY THE DATABASE TABLE
                 
                 call sqlite3_column_query( col(1), 'ID', sqlite_int )
                 call sqlite3_column_query( col(2), 'UNRGNS', sqlite_int )
                 call sqlite3_column_query( col(3), 'NUMSEC', sqlite_int )
                 call sqlite3_prepare_select( db, 'V_EMM_LDSMSTR_DIM', col, stmt)
                 
!                 write(6,'(a3,a3,i4,a3,i4)')'B4 ',  ' ',   nNERCreg_sq, ' ',NUMSEC
                 
                 ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
                 do
                 call sqlite3_next_row( stmt, col, finished )
                 if ( finished ) exit
                   
                 call sqlite3_get_column( col(1), ID)
                 call sqlite3_get_column( col(2), nNERCreg_sq)
                 call sqlite3_get_column( col(3), NUMSEC_sq)
                  nNERCreg = nNERCreg_sq
                 NUMSEC = NUMSEC_sq         
                                 
                 end do
!                 write(6,'(a3,a3,i4,a3,i4)')'Aft ',  ' ',   nNERCreg, ' ',NUMSEC
            deallocate ( col )
            finished = .FALSE.         
         
            allocate ( col2(5) )
                 ! QUERY THE DATABASE TABLE
                   call sqlite3_column_query( col2(1), 'ID', sqlite_int )
                   call sqlite3_column_query( col2(2), 'EMM_REG', sqlite_int )
                   call sqlite3_column_query( col2(3), 'SECTOR', sqlite_int )
                   call sqlite3_column_query( col2(4), 'EU_GRP', sqlite_int )
                   call sqlite3_column_query( col2(5), 'EUELAS', sqlite_real )
                   call sqlite3_prepare_select( db, 'V_EMM_LDSMSTR_EU', col2, stmt2 )
         
                 ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
                   do
                     call sqlite3_next_row( stmt2, col2, finished )
                     if ( finished ) exit
                     
                     !write(6,*) 'can you read this2b4'
                     call sqlite3_get_column( col2(1), ID )
                     call sqlite3_get_column( col2(2), MNUMNR_sq )
                     call sqlite3_get_column( col2(3), MAXSEC_sq )
                     call sqlite3_get_column( col2(4), NEUSGRP_sq )
                     call sqlite3_get_column( col2(5), EUELAS_sq(MNUMNR_sq,MAXSEC_sq,NEUSGRP_sq) )
                     
                     !MAXSEC = MAXSEC_sq
                     !NEUSGRP = NEUSGRP_sq
                     EUELAS(MNUMNR_sq,MAXSEC_sq,NEUSGRP_sq) = EUELAS_sq(MNUMNR_sq,MAXSEC_sq,NEUSGRP_sq)              
                    
            enddo
!           write(6,'(a5,a3,i4,a3,i4)')'Aft2 ',  ' ',   nNERCreg, ' ',NUMSEC                 
           
           deallocate ( col2 )
           finished = .FALSE.
         
         

         
         
           allocate ( col4(5) )
                 ! QUERY THE DATABASE TABLE
                   call sqlite3_column_query( col4(1), 'ID', sqlite_int ) 
                   call sqlite3_column_query( col4(2), 'EMM_REG', sqlite_int )
                   call sqlite3_column_query( col4(3), 'CENSUS', sqlite_int )
                   call sqlite3_column_query( col4(4), 'SECTOR', sqlite_int )
                   call sqlite3_column_query( col4(5), 'MappCtoN', sqlite_real )
       
                   call sqlite3_prepare_select( db, 'V_EMM_LDSMSTR_RGN_CEN', col4, stmt4 )
         
                 ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
                   do
                     call sqlite3_next_row( stmt4, col4, finished )
                     if ( finished ) exit
                     
                     !write(6,*) 'can you read this2aft'
         
                     call sqlite3_get_column( col4(1), ID )
                     call sqlite3_get_column( col4(2), MNUMNR_sq )
                     call sqlite3_get_column( col4(3), MNUMCR_sq )
                     call sqlite3_get_column( col4(4), MAXSEC_sq )
                     call sqlite3_get_column( col4(5), MappCtoN_sq(MNUMNR_sq,MNUMCR_sq,MAXSEC_sq) )
                     
!                    write(6,*)'MappCtoNb4', MNUMNR_sq,MNUMCR_sq,MAXSEC_sq 
                     !MNUMNR = MNUMNR_sq
                     !MNUMCR = MNUMCR_sq
                     !MAXSEC = MAXSEC_sq
                     MappCtoN(MNUMNR_sq,MNUMCR_sq,MAXSEC_sq) = MappCtoN_sq(MNUMNR_sq,MNUMCR_sq,MAXSEC_sq)/1000
       
!                   write(6,*)'MappCtoN AF ',MNUMNR_sq,MNUMCR_sq,MAXSEC_sq,MappCtoN(MNUMNR_sq,MNUMCR_sq,MAXSEC_sq)  
                   enddo
                                
           deallocate ( col4 )
           finished = .FALSE.
         
         
           allocate ( col5(5) )
                 ! QUERY THE DATABASE TABLE
                   call sqlite3_column_query( col5(1), 'ID', sqlite_int ) 
                   call sqlite3_column_query( col5(2), 'SECTOR_NUM', sqlite_int )
                   call sqlite3_column_query( col5(3), 'SECTOR_NM', sqlite_char )
                   call sqlite3_column_query( col5(4), 'NEUSES', sqlite_int )
                   call sqlite3_column_query( col5(5), 'NEUSGRP', sqlite_int )
       
                   call sqlite3_prepare_select( db, 'V_EMM_LDSMSTR_SECT_DIM', col5, stmt5 )
         
                 ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
                   do
                     call sqlite3_next_row( stmt5, col5, finished )
                     if ( finished ) exit
         
                     call sqlite3_get_column( col5(1), ID )
                     call sqlite3_get_column( col5(2), NUMSEC_sq )
                     call sqlite3_get_column( col5(3), SLNAM(ID) )
                     call sqlite3_get_column( col5(4), NEUSES_sq(ID) )
                     call sqlite3_get_column( col5(5), NEUSGRP(ID) )                     
                      
                      NEUSES(ID)  = NEUSES_sq(ID)
                      !NEUSGRP(ID) = NEUSGRP_sq(ID)
!                write(6,*)'SLNAM ',SLNAM(ID), ' ',NEUSES(ID),' ',NEUSGRP(ID)     
                   enddo
                               
           deallocate ( col5 )
           finished = .FALSE.
         
         
           allocate ( col6(5) )
                 ! QUERY THE DATABASE TABLE
                   call sqlite3_column_query( col6(1), 'ID', sqlite_int ) 
                   call sqlite3_column_query( col6(2), 'SECTOR', sqlite_int )
                   call sqlite3_column_query( col6(3), 'EUSE', sqlite_int )
                   call sqlite3_column_query( col6(4), 'EUNAM', sqlite_char )
                   call sqlite3_column_query( col6(5), 'EUGRP', sqlite_int )
       
                   call sqlite3_prepare_select( db, 'V_EMM_LDSMSTR_SECT_EU_DIM', col6, stmt6 )
         
                 ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
                   do
                     call sqlite3_next_row( stmt6, col6, finished )
                     if ( finished ) exit
         
                     call sqlite3_get_column( col6(1), ID )
                     call sqlite3_get_column( col6(2), NUMSEC_sq )
                     call sqlite3_get_column( col6(3), NEUSE )
                     call sqlite3_get_column( col6(4), EUNAM_sq(ID) )
                     call sqlite3_get_column( col6(5), EUGRP_sq(ID) )
                     
                    ! NUMSEC = NUMSEC_sq
                     !NEUSE = NEUSE_sq
                     EUNAM(ID) = EUNAM_sq(ID)
                     EUGRP(ID) = EUGRP_sq(ID)
                     
                   enddo
!                  write(6,*)'Aft6 EUNAM ',ID,EUNAM(ID),EUGRP(ID)             
           deallocate ( col6 )
           finished = .FALSE.
                  
         
           allocate ( col10(6) )
                 ! QUERY THE DATABASE TABLE
                   call sqlite3_column_query( col10(1), 'ID', sqlite_int ) 
                   call sqlite3_column_query( col10(2), 'SECTOR', sqlite_char )
                   call sqlite3_column_query( col10(3), 'NGROUP', sqlite_int )
                   call sqlite3_column_query( col10(4), 'GROUPNM', sqlite_char )
                   call sqlite3_column_query( col10(5), 'RGN_CD', sqlite_int )
                   call sqlite3_column_query( col10(6), 'EMM_REG', sqlite_char )
              
                   call sqlite3_prepare_select( db, 'V_EMM_LDSMSTR_GRPS', col10, stmt10 )
                                                  
                 ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
                  q = 0 
                   do
                   
                     call sqlite3_next_row( stmt10, col10, finished )
                     if ( finished ) exit
         
                     call sqlite3_get_column( col10(1), ID )
                     call sqlite3_get_column( col10(2), SECTOR_sq )
                     call sqlite3_get_column( col10(5), new_sq )
                     
                     IF (SECTOR_sq .eq. 'RES') p =1
                     IF (SECTOR_sq .eq. 'COM') p =2

                    If (p.eq.1) then
                      call sqlite3_get_column( col10(3), RRlist_sq(new_sq))
                      call sqlite3_get_column( col10(4), RRlistID_sq(new_sq) )                     
                      call sqlite3_get_column( col10(6), LREG(new_sq) )
                      RRlist(1,new_sq) = RRlist_sq(new_sq)
                      RRlistID(1) = RRlistID_sq(new_sq)                  
!             write(6,*)'Aft8',new_sq,' RRlistID ', RRlistID(1),' RRlist(1,new_sq) ',RRlist(1,new_sq), 'LREG=',LREG(new_sq)
                    Else
                      call sqlite3_get_column( col10(3), CRlist_sq(new_sq))
                      call sqlite3_get_column( col10(4), CRlistID_sq(new_sq) )                      
                      call sqlite3_get_column( col10(6), LREG(new_sq) )
                      CRlist(1,new_sq) = CRlist_sq(new_sq)
                      CRlistID(1) = CRlistID_sq(new_sq)
!             write(6,*)'Aft8',new_sq,' CRlistID ', CRlistID(1),' CRlist(1,new_sq) ',CRlist(1,new_sq) , 'LREG=',LREG(new_sq)
                    Endif
            
                 
         
                   enddo
      
                   
        
           deallocate ( col10 ) 
       
       
        call sqlite3_close( db )
        end subroutine EMM_LDSM


       SUBROUTINE EMM_CNTL
      
          USE SQLITE
          IMPLICIT NONE
        
 !      THIS SUBROUTINE READS DATA FROM EMMCNTL TABLES
 
        
        include 'parametr'
        include 'ncntrl'
        include 'emmparm'
        include 'control'
        include 'entcntl'
        include 'ecpcntl'
        include 'bildin'
        include 'dispinyr'
        include 'uefdout'
        include 'uecpout'
        include 'emission'
        include 'ecp_coal'
        include 'ecp_nuc'
        include 'macout'
        include 'eusprc'
        include 'edbdef'
        include 'elout'
        
        type(sqlite_database)                      :: db
        type(sqlite_statement)                     :: stmt
        type(sqlite_statement)                     :: stmt2
        type(sqlite_statement)                     :: stmt3
        type(sqlite_statement)                     :: stmt4
        type(sqlite_statement)                     :: stmt5
        type(sqlite_statement)                     :: stmt6
        type(sqlite_statement)                     :: stmt7
        type(sqlite_column), dimension(:), pointer :: col
        type(sqlite_column), dimension(:), pointer :: col2
        type(sqlite_column), dimension(:), pointer :: col3
        type(sqlite_column), dimension(:), pointer :: col4
        type(sqlite_column), dimension(:), pointer :: col5
        type(sqlite_column), dimension(:), pointer :: col6
        type(sqlite_column), dimension(:), pointer :: col7
        character(len=40), pointer, dimension(:)   :: result
        character(len=80)                          :: errmsg
        logical                                    :: finished
        
        
        INTEGER*4 NRG,IRG,ECP,IECP,YR,CRGN,NRG2,STID,I
        real*4 TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,CO2GEN
        !real rtemp(NOX_D_GRP)
        character*2 SRG
        CHARACTER*4 RGN_NAM
        Character*1 tmp_CHCOD
        
       ! write(EMM_TEMP,'(i4)'), NRG
        call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )
        
        allocate (col(6))
        call sqlite3_column_query( col(1), 'EMM_REG', sqlite_int )
        call sqlite3_column_query( col(2), 'YEAR', sqlite_int )
        call sqlite3_column_query( col(3), 'NUCDRAT', sqlite_real )
        call sqlite3_column_query( col(4), 'UQHYFAC', sqlite_real )
        call sqlite3_column_query( col(5), 'USGCOST', sqlite_real )
        call sqlite3_column_query( col(6), 'ULABASEDMD', sqlite_real )
        call sqlite3_prepare_select( db, 'V_EMM_CNTL_ANNUAL', col, stmt)
        
        ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
        do
        call sqlite3_next_row( stmt, col, finished )
        if ( finished ) exit
        
        call sqlite3_get_column( col(1), NRG)
        call sqlite3_get_column( col(2), YR)
        call sqlite3_get_column( col(3), NUCDRAT(YR-1989,NRG))
        call sqlite3_get_column( col(4), UQHYFAC(YR - 1989,NRG))
        call sqlite3_get_column( col(5), USGCOST(YR - 1989,NRG))
        call sqlite3_get_column( col(6), ULABASEDMD(YR -1989,NRG))
        
        end do
        deallocate ( col )
        
        DO NRG=1,MNUMNR
            UQHYFAC(20,NRG) = 1.0
        ENDDO

        allocate (col2(5))
        call sqlite3_column_query( col2(1), 'EMM_REG', sqlite_int )
        call sqlite3_column_query( col2(2), 'RGN_NAM', sqlite_char )
        call sqlite3_column_query( col2(3), 'UPRGCD', sqlite_char )
        call sqlite3_column_query( col2(4), 'URGNME', sqlite_char )
        call sqlite3_column_query( col2(5), 'URGNUM', sqlite_int )
        call sqlite3_prepare_select( db, 'V_EMM_DIMS', col2, stmt2)
        
        ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
        do
        call sqlite3_next_row( stmt2, col2, finished )
        if ( finished ) exit
        
        call sqlite3_get_column( col2(1), NRG)
        call sqlite3_get_column( col2(2), RGN_NAM)
        call sqlite3_get_column( col2(3), UPRGCD(NRG))
        call sqlite3_get_column( col2(4), URGNME(NRG))
        call sqlite3_get_column( col2(5), URGNUM(NRG))
        Write(18,*)'URGNUS ',NRG,RGN_NAM, URGNME(NRG),URGNUM(NRG),UPRGCD(NRG)
        
        end do
        
        
        allocate (col3(6))
        call sqlite3_column_query( col3(1), 'EMM_REG', sqlite_int )
        call sqlite3_column_query( col3(2), 'CO2_STDTN', sqlite_int )
        call sqlite3_column_query( col3(3), 'CO2_EMSET', sqlite_real )
        call sqlite3_column_query( col3(4), 'UDMLOSSADJ', sqlite_real )
        call sqlite3_column_query( col3(5), 'URWDCFST', sqlite_real )
        call sqlite3_column_query( col3(6), 'URWDCFRG', sqlite_real )
        call sqlite3_prepare_select( db, 'V_EMM_CNTL_RGN', col3, stmt3)
       
        ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
        do
        call sqlite3_next_row( stmt3, col3, finished )
        if ( finished ) exit
        
        call sqlite3_get_column( col3(1), NRG)
        call sqlite3_get_column( col3(2), CO2_STDTN(NRG))
        call sqlite3_get_column( col3(3), CO2_EMSET(NRG))
        call sqlite3_get_column( col3(4), UDMLOSSADJ(NRG))
        call sqlite3_get_column( col3(5), URWDCFST(NRG))
        call sqlite3_get_column( col3(6), URWDCFRG(NRG))
        end do
        deallocate ( col3 )
        
!V_EMM_CNTL_STATES
         allocate (col4(9))
         call sqlite3_column_query( col4(1), 'EMM_REG', sqlite_int )
         call sqlite3_column_query( col4(2), 'STATE', sqlite_char )
         call sqlite3_column_query( col4(3), 'SALES_IOU', sqlite_real )
         call sqlite3_column_query( col4(4), 'SALES_COOP', sqlite_real )
         call sqlite3_column_query( col4(5), 'SALES_MUNI', sqlite_real )
         call sqlite3_column_query( col4(6), 'SALES_OTHR', sqlite_real )
         call sqlite3_column_query( col4(7), 'SALES_TOT', sqlite_real )
         call sqlite3_column_query( col4(8), 'CO2_GENSN', sqlite_real )
         call sqlite3_column_query( col4(9), 'STATE_ID', sqlite_int )
         
         call sqlite3_prepare_select( db, 'V_EMM_CNTL_STATES', col4, stmt4)
        I=0
        ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
         do
         call sqlite3_next_row( stmt4, col4, finished )
         if ( finished ) exit
         call sqlite3_get_column( col4(1), NRG)
         call sqlite3_get_column( col4(2), SRG)
         call sqlite3_get_column( col4(3), TEMP1)
         call sqlite3_get_column( col4(4), TEMP2)
         call sqlite3_get_column( col4(5), TEMP3)
         call sqlite3_get_column( col4(6), TEMP4)
         call sqlite3_get_column( col4(7), TEMP5)
         call sqlite3_get_column( col4(8), CO2GEN)
         call sqlite3_get_column( col4(9), STID)
         I=I+1
         
 !        WRITE(*,3204)'H_SALES ', I,NRG,SRG,TEMP1,TEMP2,TEMP3,TEMP4,TEMp5,CO2GEN,STID
3204 FORMAT(A25,1x,2(i4,2x),A2,1x,6(F12.3,1x),I4)
        IF (STID.gt.0) THEN
          H_SALES(STID,1,NRG) = TEMP1
          H_SALES(STID,2,NRG) = TEMP2
          H_SALES(STID,3,NRG) = TEMP3
          H_SALES(STID,4,NRG) = TEMP4
          H_SALES(STID,0,NRG) = TEMP5
          CO2_GENSN(STID,NRG) = CO2GEN
          ST_CODES(STID)=SRG
 !        WRITE(*,3204)'H_SALESAF ', I,NRG, ST_CODES(STID), H_SALES(STID,1,NRG), H_SALES(STID,2,NRG), H_SALES(STID,3,NRG), &
 !           H_SALES(STID,4,NRG), H_SALES(STID,0,NRG),CO2_GENSN(STID,NRG), STID     
        ENDIF
         
         end do
         deallocate ( col4 )

!V_EMM_CNTL_CO2_RGN
        allocate (col5(6))
        call sqlite3_column_query( col5(1), 'EMM_REG', sqlite_int )
        call sqlite3_column_query( col5(2), 'CO2_RGN', sqlite_int )
        call sqlite3_column_query( col5(3), 'CO2_IM_BY_RG', sqlite_real )
        call sqlite3_column_query( col5(4), 'CO2_DF_BY_RG', sqlite_real )
        call sqlite3_column_query( col5(5), 'CO2_OS_BY_RG', sqlite_real )
        call sqlite3_column_query( col5(6), 'CO2_DE_BY_RG', sqlite_real )
        call sqlite3_prepare_select( db, 'V_EMM_CNTL_CO2_RGN', col5, stmt5)
       
        ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
        do
        call sqlite3_next_row( stmt5, col5, finished )
        if ( finished ) exit
        
        call sqlite3_get_column( col5(1), NRG)
        call sqlite3_get_column( col5(2), CRGN)
        call sqlite3_get_column( col5(3), CO2_IM_BY_RG(NRG,CRGN))
        call sqlite3_get_column( col5(4), CO2_DF_BY_RG(NRG,CRGN))
        call sqlite3_get_column( col5(5), CO2_OS_BY_RG(NRG,CRGN))
        call sqlite3_get_column( col5(6), CO2_DE_BY_RG(NRG,CRGN))
!        Write(6,*)'co2_IM ',NRG,CRGN, CO2_IM_BY_RG(NRG,CRGN)
!        Write(6,*)'co2_DF ',NRG,CRGN, CO2_DF_BY_RG(NRG,CRGN)        
!        Write(6,*)'co2_OS ',NRG,CRGN, CO2_OS_BY_RG(NRG,CRGN)       
!        Write(6,*)'co2_DE ',NRG,CRGN, CO2_DE_BY_RG(NRG,CRGN)
        end do
        deallocate ( col5 )

!V_EMM_CNTL_CPP_RATE_SW
        allocate (col6(3))
        call sqlite3_column_query( col6(1), 'EMM_REG', sqlite_int )
        call sqlite3_column_query( col6(2), 'EMM_REG_2', sqlite_int )
        call sqlite3_column_query( col6(3), 'CO2_STDGN', sqlite_real )
        call sqlite3_prepare_select( db, 'V_EMM_CNTL_CPP_RATE_SW', col6, stmt6)
       
        ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
        do
        call sqlite3_next_row( stmt6, col6, finished )
        if ( finished ) exit
        
        call sqlite3_get_column( col6(1), NRG)
        call sqlite3_get_column( col6(2), NRG2)
        call sqlite3_get_column( col6(3), CO2_STDGN(NRG,NRG2))
        end do
        deallocate ( col6 )
        
!V_EMM_CNTL_CHCOD
        allocate (col7(1))
        call sqlite3_column_query( col7(1), 'CHCOD', sqlite_char )
        call sqlite3_prepare_select( db, 'V_EMM_CHCOD', col7, stmt7)
       
        ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
        I=0
        do
        call sqlite3_next_row( stmt7, col7, finished )
        I=I+1
        if ( finished ) exit
        
        call sqlite3_get_column( col7(1), tmp_CHCOD)
        CHCOD(I) = tmp_CHCOD
       ! Write(*,*)'check chcod ',I,CHCOD(I),tmp_CHCOD
        end do
        
        deallocate ( col7 )
        call sqlite3_close( db )
        
        end subroutine EMM_CNTL
        
         SUBROUTINE EMM_RENDAT
      
        USE SQLITE
        IMPLICIT NONE
        
  !      THIS SUBROUTINE READS DATA FROM RENDAT TABLES
       
        include 'parametr'
        include 'ncntrl'
        include 'emmparm'
        include 'control'
        include 'wrenew'
        include 'rencntl'
        include 'wrenio'
        include 'uefdout'
        
        type(sqlite_database)                      :: db
        type(sqlite_statement)                     :: stmt
        type(sqlite_statement)                     :: stmt2
        type(sqlite_column), dimension(:), pointer :: col
        type(sqlite_column), dimension(:), pointer :: col2
         character(len=80)                          :: errmsg
        logical                                    :: finished
        character(len=8)                           :: itech
       
        
        INTEGER*4 NRG,IRG,ECP,IECP,YR,SRG,CRGN,NRG2,i,mx,itc
        !real rtemp(NOX_D_GRP)
        !character(len=4)                           :: EMM_TEMP
        
       ! write(EMM_TEMP,'(i4)'), NRG
        call sqlite3_open( 'emm_db/NEMS_INPUT.db', db )
        
        allocate (col(3))
        call sqlite3_column_query( col(1), 'EMM_REG', sqlite_int )
        call sqlite3_column_query( col(2), 'ITECH', sqlite_char )
        call sqlite3_column_query( col(3), 'WBTECH', sqlite_int )
        call sqlite3_prepare_select( db, 'V_EMM_RENDAT_WBTECH', col, stmt)
        
        ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
        do
        call sqlite3_next_row( stmt, col, finished )
        if ( finished ) exit
        
        call sqlite3_get_column( col(1), NRG)
        call sqlite3_get_column( col(2), ITECH)
        if (trim(ITECH) .eq. 'BIOMASS') then 
           i = 6
        elseif (trim(ITECH) .eq. 'ETHANOL') then 
           i = 13
        elseif (trim(ITECH) .eq. 'GEO EX') then 
           i = 3
        elseif (trim(ITECH) .eq. 'GEO PL') then 
           i = 2
        elseif (trim(ITECH) .eq. 'GEO PUMP') then 
           i = 4
        elseif (trim(ITECH) .eq. 'HYDRO') then 
           i = 1
        elseif (trim(ITECH) .eq. 'MSW') then 
           i = 5
        elseif (trim(ITECH) .eq. 'PV') then 
           i = 8
        elseif (trim(ITECH) .eq. 'PV TILT') then 
           i = 9
        elseif (trim(ITECH) .eq. 'SOLAR TH') then 
           i = 7
        elseif (trim(ITECH) .eq. 'WIND') then 
           i = 10
        elseif (trim(ITECH) .eq. 'WIND_ofs') then 
           i = 12
        elseif (trim(ITECH) .eq. 'WIND_ons') then 
           i = 11
        endif
        call sqlite3_get_column( col(3), WBTECH(i,NRG))
        
        end do
        deallocate ( col )

        allocate (col2(5))
        call sqlite3_column_query( col2(1), 'EMM_REG', sqlite_int )
        call sqlite3_column_query( col2(2), 'MLPTMX', sqlite_int )
        call sqlite3_column_query( col2(3), 'ITECH', sqlite_char)
        call sqlite3_column_query( col2(4), 'UTCSFC', sqlite_real )
        call sqlite3_column_query( col2(5), 'UTRSFC', sqlite_real )
        call sqlite3_prepare_select( db, 'V_EMM_RENDAT_LTEL', col2, stmt2)
        
        ! LOAD RESULTS INTO FORTRAN VARIABLES FOR NEMS
        do
        call sqlite3_next_row( stmt2, col2, finished )
        if ( finished ) exit
 
        call sqlite3_get_column( col2(1), NRG)
        call sqlite3_get_column( col2(2), mx)
        call sqlite3_get_column( col2(3), ITECH)
        if (trim(ITECH) .eq. 'BIOMASS') then 
           i = 6
        elseif (trim(ITECH) .eq. 'ETHANOL') then 
           i = 13
        elseif (trim(ITECH) .eq. 'GEO EX') then 
           i = 3
        elseif (trim(ITECH) .eq. 'GEO PL') then 
           i = 2
        elseif (trim(ITECH) .eq. 'GEO PUMP') then 
           i = 4
        elseif (trim(ITECH) .eq. 'HYDRO') then 
           i = 1
        elseif (trim(ITECH) .eq. 'MSW') then 
           i = 5
        elseif (trim(ITECH) .eq. 'PV') then 
           i = 8
        elseif (trim(ITECH) .eq. 'PV TILT') then 
           i = 9
        elseif (trim(ITECH) .eq. 'SOLAR TH') then 
           i = 7
        elseif (trim(ITECH) .eq. 'WIND') then 
           i = 10
        elseif (trim(ITECH) .eq. 'WIND_ofs') then 
           i = 12
        elseif (trim(ITECH) .eq. 'WIND_ons') then 
           i = 11
        endif
        call sqlite3_get_column( col2(4), UTCSFC(NRG,i,mx))
        call sqlite3_get_column( col2(5), UTRSFC(NRG,i,mx))
        end do
        deallocate ( col2 )

        call sqlite3_close( db )
        end subroutine EMM_RENDAT  
        

    
