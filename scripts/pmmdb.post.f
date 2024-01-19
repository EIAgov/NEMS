!$Header: m:/default/scripts/RCS/pmmdb.post.f,v 1.1 2010/08/23 20:23:32 pkc Exp $
!ORCLTABS.f90 program
!
      PROGRAM POSTPMMDB

      use dfwin
      use f90SQLConstants
      use f90SQL
      IMPLICIT NONE
      INCLUDE 'PARAMETR'
      INCLUDE 'NCNTRL'
      INCLUDE 'PDBDEF'
      INCLUDE 'CANF90SQL'

       CALL CONNECTDB
       CALL PMMPRNTDB
       CALL ORCLDISC

       STOP
       END


      SUBROUTINE CONNECTDB
!     **********************  test connection to Microsoft Access database

!    load f90SQL modules
     use f90SQLConstants
     use f90SQL
     use dflib 

      IMPLICIT NONE
      INCLUDE 'PARAMETR'
      INCLUDE 'NCNTRL'
      INCLUDE 'PDBDEF'
      INCLUDE 'CANF90SQL'

      integer(SQLRETURN_KIND),parameter:: Maxstringlen=255
      integer(SQLRETURN_KIND)::iRet
      integer(SQLSMALLINT_KIND)::Connstrlen
      integer(4) ldir
      integer I
      character*11 uid
      character(len=maxstringlen) fname,connstr,dir,outdir
 
      dir = FILE$CURDRIVE
      ldir = GETDRIVEDIRQQ(dir)

      do i=1,len_trim(dir)
        if(dir(i:i).eq.'\') then
          dir(i:i)='/'
        endif
      enddo
      outdir=trim(dir)
      fname = trim(outdir) // '/PMMDB.mdb' 
!     write(6,*) ' fname ',fname

!      allocate an environment handle
      call f90SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, EnvHndl, iRet)
!      write(6,*) ' done with tstacc environ handle '

!      Set ODBC version we will be using (3.x in this case)
      call f90SQLSetEnvAttr(EnvHndl, SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC3, iRet)
!      write(6,*) ' done with environ attr '

!      Allocate a connection handle
       call f90SQLAllocHandle(SQL_HANDLE_DBC,EnvHndl, ConnHndlpmm, iRet)
!      write(6,*) ' done with connection handle '

!      Create a connection string  
!     connstr = 'nemsdb' 
      connstr = 'DBQ='//trim(fname)//';DRIVER={Microsoft Access Driver (*.mdb)}'
       write(6,*) ' connstr ',trim(connstr)             

!      connect to access data source nemsdb  
!     call f90SQLConnect(ConnHndlpmm,connstr,'','',iRet)

!      connect to Microsoft access database
      call f90SQLDriverConnect(ConnHndlpmm, f90SQL_NULL_PTR,ConnStr, & 
                               ConnStr,ConnStrLen,SQL_DRIVER_COMPLETE,iRet)

      if (iRet.eq.SQL_SUCCESS .or. iRet.eq. SQL_SUCCESS_WITH_INFO) then
           print *,'microsoft access database connection successful'
      else
          print *,'Error connecting to data source'
          call ShowDiags(SQL_HANDLE_DBC,ConnHndlpmm)
      endif

!     set autocommit to off
!     call f90SQLSetConnectAttr(ConnHndlpmm,SQL_ATTR_AUTOCOMMIT,SQL_AUTOCOMMIT_OFF,iRet)
!      if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
!       write(6,*) ' autocommit error '
!call ShowDiags(SQL_HANDLE_DBC,ConnHndlpmm)
!      endif
!    write(uf_dbs,*) ' done with set autocommit off '            

!      Allocate statement handle
       call f90SQLAllocHandle(SQL_HANDLE_STMT,ConnHndlpmm, StmtHndlpmm, iRet)
       if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' allocate statememt error version_number '
        call ShowDiags(SQL_HANDLE_STMT,StmtHndlpmm)
       endif
!    write(uf_dbs,*) ' done with allocate statement handle '

        PSQLSTMTOLD = ' '
        ORAERR = 0

      Return
      end
!
! *******************************************************************
! PMM_LOAD_DATA   
!
      SUBROUTINE PMM_LOAD_DATA (SQLStmt, NUMCOL, LOOPINGS, COLVAL, CHCOLVAL, MSGFIL)
!
!    load f90SQL modules
     USE f90SQLConstants
     USE f90SQL

      IMPLICIT NONE
      INCLUDE 'PARAMETR'     
      INCLUDE 'NCNTRL'    
      INCLUDE 'PDBDEF'
      INCLUDE 'CANF90SQL'

      INTEGER(SQLUINTEGER_KIND),parameter:: DSIZE=PMAXRECS
      INTEGER(SQLINTEGER_KIND),parameter:: MaxStringLen=256
      INTEGER(SQLINTEGER_KIND),parameter:: StringLen2=25
      INTEGER(SQLRETURN_KIND)::iRet
      CHARACTER(len=220)::SQLStmt
      CHARACTER(len=25)::stmtshort
      CHARACTER(len=35)::tablename
      INTEGER(SQLSMALLINT_KIND)::ColNumber
      INTEGER(SQLUINTEGER_KIND)::I,ICOL,NUMCOL,LOOPINGS,NRECORD,MSGFIL,DBTYPE
      INTEGER(SQLUINTEGER_KIND)::Y,LLL,III,TTT,VVV,IMSG,LOAD_TIME_BEGIN,LOAD_TIME_END

      !data vectors
      CHARACTER(len=StringLen2)::ScenCol(DSIZE)
      CHARACTER(len=StringLen2):: CHCOLS(PMAXCOLS), CHCOLVAL(PMAXCOLS,DSIZE)
      INTEGER(SQLSMALLINT_KIND)::INDEXCOL(DSIZE),CalYear(DSIZE)
      !len/indicator vectors
      INTEGER(SQLINTEGER_KIND):: ChLen(DSIZE),ScenColLen(DSIZE),IndexColInd(DSIZE)
      INTEGER(SQLINTEGER_KIND):: COLSInd(PMAXCOLS),CHLens(PMAXCOLS)
      INTEGER(SQLINTEGER_KIND):: REALCOLInd(PMAXCOLS,DSIZE),CalYearInd(DSIZE),CHCOL(PMAXCOLS)


      !data real*4 vectors
!     REAL(SQLREAL_KIND):: COLS(PMAXCOLS)
!     REAL(SQLREAL_KIND):: COLVAL(PMAXCOLS,DSIZE),REALCOL(PMAXCOLS,DSIZE)

      !data real*8 vectors
      REAL(SQLDOUBLE_KIND):: COLS(PMAXCOLS)
      REAL(SQLDOUBLE_KIND):: COLVAL(PMAXCOLS,DSIZE),REALCOL(PMAXCOLS,DSIZE)

      chcol = 0
      chlen = stringlen2
      stmtshort = sqlstmt(:25)
      IMSG = MSGFIL
!     IMSG = 6    ! for testing send writes to nohup.out
!
      IF (ORAERR .NE. 0) RETURN
!
!     WRITE(IMSG,*) ' Begin pmm_load_data subroutine in orcltabs.f '

      CALL MPTIM2(LOAD_TIME_BEGIN)
      WRITE (IMSG,1111) ' Begin pmm_load_data table ',sqlstmt(12:47),FLOAT(LOAD_TIME_BEGIN)/100.
1111  FORMAT(10X,A,1x,A,1x,' CPU TIME (SECONDS) = ',F7.2,A,F7.2)
!
!
      DO III = 1,LOOPINGS
         SCENCOL(III) = trim(scen_date)
        ScenColLen(III)=len_trim(ScenCol(III))
      ENDDO 
!
!   If new table is being written then close old statement handles and do prepares, etc for new table
!    If same table is being processed then skip to insert statement
!
    write(imsg,*) ' psqlstmtold ',psqlstmtold(12:47)
!
      IF (SQLSTMT .NE. PSQLSTMTOLD) THEN 
!
        IF (PSQLSTMTOLD .NE. ' ') THEN
!
!   close the cursor associated to statement, so it can process another insert
        call f90SQLFreeStmt(StmtHndlpmm,SQL_CLOSE, iRet)
        if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
          write(6,*) ' free cursor error ',sqlstmt
          call ShowDiags(SQL_HANDLE_STMT,StmtHndlpmm)
         endif
!
!   !Release statement handle
!        call f90SQLFreeHandle(SQL_HANDLE_STMT, StmtHndl, iRet)
!        if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
!          write(6,*) ' release statement handle error ',sqlstmt
!   call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
!        endif
!
        ENDIF
!
        psqlstmtold = sqlstmt
!
      DO I = 1 , LOOPINGS
        DO ICOL = 1 , NUMCOL
          IF ( CHCOLVAL(ICOL,I) .NE. ' ') &   
            CHCOL(ICOL) = 1
        ENDDO
      ENDDO
!     DO ICOL = 1 , NUMCOL
!      if ( chcol(icol) .ne. 0 ) then
!       write(6,*) ' chcol icol ',icol,chcol(icol)
!      endif
!     enddo
!
!   write(IMSG,*) ' sql statement ',sqlstmt
!     Allocate statement handle
!       call f90SQLAllocHandle(SQL_HANDLE_STMT,ConnHndl, StmtHndl, iRet)
!       if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
!         write(6,*) ' allocate statement handle error ',sqlstmt
!  call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
!       endif

!  write(IMSG,*) ' SQLStmt loopings numcol scencol ',SQLStmt,LOOPINGS,NUMCOL,SCENCOL(1)

    ColNumber = 1
        call bindchar(ColNumber,ScenCol,ScenColLen,iret)
 
        DO ICOL = 1 , NUMCOL
          IF (chcol(icol) .eq. 1) THEN
            call bindchar(icol+1,chcols(icol),chlens(icol),iret)
          ELSE
            call binddble(icol+1,COLS(icol),COLSind(icol),iret)
          ENDIF
        ENDDO
!
!   Prepare the query    
    call f90SQLPrepare(StmtHndlpmm, SQLStmt, iRet)
      if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' prepare  error ',sqlstmt
        call ShowDiags(SQL_HANDLE_STMT,StmtHndlpmm)
      endif

!   !set size of parameter vector to looping 
!   call f90SQLSetStmtAttr(StmtHndl, SQL_ATTR_PARAMSET_SIZE, LOOPINGS, iRet)
!     if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
!       write(6,*) ' parameter set error ',sqlstmt
!call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
!     endif

     ENDIF
!
!   start here if same table being written
!   attempt to write maxrecs records with access database driver
!
     DO I = 1 , loopings
          DO icol = 1 , numcol
           cols(icol) = colval(icol,i)
           chcols(icol) = chcolval(icol,I)
           chlens(icol) = len_trim(chcols(icol))
          enddo  ! end icol do
     
    !Execute insert query
     call f90SQLExecute(StmtHndlpmm,iRet)
      if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' execute error ',sqlstmt
           write(6,*) ' load colval ',(cols(icol),icol=1,numcol)
           write(6,*) 'load chcolval ',(chcols(icol),icol=1,numcol)
        call ShowDiags(SQL_HANDLE_STMT,StmtHndlpmm)
      endif
    enddo   ! end loopings do
!
!
 
!   !close the cursor associated to statement, so it can process another insert
!    call f90SQLFreeStmt(StmtHndl,SQL_CLOSE, iRet)
!     if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
!       write(6,*) ' free cursor error ',sqlstmt
!call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
!     endif
!
!   !Release statement handle
!    call f90SQLFreeHandle(SQL_HANDLE_STMT, StmtHndl, iRet)
!     if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
!       write(6,*) ' release statement handle error ',sqlstmt
!call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
!     endif
!
!      write(IMSG,*) ' end load_data '
      CALL MPTIM2(LOAD_TIME_END)
      WRITE (IMSG,2222) '** End Load_data table ',sqlstmt(12:47),FLOAT(LOAD_TIME_END)/100., &
       FLOAT(LOAD_TIME_END)/100. - FLOAT(LOAD_TIME_BEGIN)/100.
2222  FORMAT(10X,A,1x,a,' CPU TIME (SECONDS) = ',F7.2,', TIME USED =',F7.2)

      RETURN
      END

SUBROUTINE SHOWDIAGS(HndlType,Hndl)

!This subroutine prints error diagnostics

!load f90SQL modules
use f90SQLConstants
use f90SQL
      IMPLICIT NONE
      INCLUDE 'PARAMETR'
      INCLUDE 'NCNTRL'
      INCLUDE 'PDBDEF'
      INCLUDE 'CANF90SQL'

integer(SQLHANDLE_KIND)::Hndl
integer(SQLSMALLINT_KIND)::HndlType

character(len=6):: SqlState 
character(len= SQL_MAX_MESSAGE_LENGTH)::Msg 
integer(SQLINTEGER_KIND)::NativeError 
integer(SQLSMALLINT_KIND):: iDiag, MsgLen 
integer(SQLRETURN_KIND):: DiagRet

iDiag = 1 
do while (.true.)
   call f90SQLGetDiagRec(HndlType, Hndl, iDiag, SqlState, NativeError, Msg, MsgLen, DiagRet) 
   write(6,*) diagret
        write(6,*) ' sql show diags diagret = ',diagret
        write(6,*) ' sql_success ',SQL_SUCCESS
        write(6,*) ' sql_success_with_info ',SQL_SUCCESS_WITH_INFO
        write(6,*) ' sql_error ',SQL_ERROR             
        write(6,*) ' sql_invalid_handle ',SQL_INVALID_HANDLE    
        write(6,*) ' sql_no_data ',SQL_NO_DATA                  
        write(6,*) ' sql_need_data ',SQL_NEED_DATA                  
        write(6,*) ' sql_still_executing ',SQL_STILL_EXECUTING                  
 1000 FORMAT(1x,i5,1x,i5,1x,i5,1x,a6,1x,i5,a,i5,i5)
   if (DiagRet.ne.SQL_SUCCESS.and.DiagRet.ne.SQL_SUCCESS_WITH_INFO) exit 
   write(6,*)trim(SqlState),',', NativeError,',', Msg(1:MsgLen)
   iDiag=iDiag+1 
enddo 
!
!   production runs.  stop database writes, continue run
!     IF (ORACLESW .LE. 2) THEN
!       ORAERR=1
!       ORACLESW=0
!       ORCLEFD=0
!       ORCLECP=0
!       ORCLEFP=0
!       ORCLCL=0
!       CALL ORCLDISC
!       RETURN
!  database test runs.  end run                       
!     ELSEIF (ORACLESW .GE. 3) THEN
!       CALL ORCLDISC
        STOP
!     ENDIF

return
end
!
!   Disconnect Oracle Connection
!
    SUBROUTINE ORCLDISC
!
!   load f90SQL modules
    USE f90SQLConstants
    USE f90SQL

      IMPLICIT NONE
      INCLUDE 'PARAMETR'
      INCLUDE 'NCNTRL'
      INCLUDE 'PDBDEF'
      INCLUDE 'CANF90SQL'

      INTEGER(SQLRETURN_KIND)::iRet

!     IF (ORAERR .NE. 0) RETURN

!disconnect
call f90SQLDisconnect(ConnHndlpmm, iRet)
      if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' disconnect handle error '
        call ShowDiags(SQL_HANDLE_DBC,ConnHndlpmm)
      endif

!release connection handles
call f90SQLFreeHandle(SQL_HANDLE_DBC, ConnHndlpmm, iRet)
      if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' release connection handle error '
        call ShowDiags(SQL_HANDLE_DBC,ConnHndlpmm) 
      endif

!release environment handle
call f90SQLFreeHandle(SQL_HANDLE_ENV, EnvHndl, iRet)
      if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' release environment handle error '
        call ShowDiags(SQL_HANDLE_ENV,EnvHndl)
      endif

!release statement handle
!   call f90SQLFreeHandle(SQL_HANDLE_STMT, StmtHndlpmm, iRet)
      if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' release environment handle error '
        call ShowDiags(SQL_HANDLE_ENV,EnvHndl)
      endif


return
end

! ****BIND SMALLINT VARIABLES **********************************************
      SUBROUTINE BINDINT(Colnum,colvar,colind,bindret)

!     load f90SQL modules
      use f90SQLConstants
      use f90SQL

      IMPLICIT NONE
      INCLUDE 'PARAMETR'
      INCLUDE 'NCNTRL'
      INCLUDE 'PDBDEF'
      INCLUDE 'CANF90SQL'

      integer(SQLUINTEGER_KIND),parameter::ParamVectSize=1
      integer(SQLINTEGER_KIND),parameter:: StringLen2=25
      integer(SQLRETURN_KIND)::BindRet
      integer(SQLSMALLINT_KIND)::ColNum

!     data vectors
!     character(len=StringLen2)::ColVar(ParamVectSize)
      integer(SQLSMALLINT_KIND)::ColVar(ParamVectSize)

!     len/indicator vectors
      integer(SQLINTEGER_KIND):: ColInd(ParamVectSize), ColLen(ParamVectSize)
!
    call f90SQLBindParameter(StmtHndlpmm, ColNum, SQL_PARAM_INPUT, SQL_F_SHORT, &
                   SQL_SMALLINT, int (5,SQLUINTEGER_KIND), int(0,SQLSMALLINT_KIND), &
                   loc(Colvar), int(0,SQLINTEGER_KIND), loc(COLInd), BindRet) 
!
      if (BindRet.ne.SQL_SUCCESS .and. BindRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' BindInt  error ',Colvar
        call ShowDiags(SQL_HANDLE_STMT,StmtHndlpmm)
      endif
!
    return
    end
!
! ****BIND VARCHAR  VARIABLES **********************************************
!
      SUBROUTINE BINDCHAR(Colnum,colvar,collen,bindret)

!     load f90SQL modules
      use f90SQLConstants
      use f90SQL

      IMPLICIT NONE
      INCLUDE 'PARAMETR'
      INCLUDE 'NCNTRL'
      INCLUDE 'PDBDEF'
      INCLUDE 'CANF90SQL'

      integer(SQLUINTEGER_KIND),parameter::ParamVectSize=1
      integer(SQLINTEGER_KIND),parameter:: StringLen=25
      integer(SQLRETURN_KIND)::BindRet
      integer(SQLSMALLINT_KIND)::ColNum
!
!     len/indicator vectors
      integer(SQLINTEGER_KIND):: ColInd(ParamVectSize), ColLen(ParamVectSize)

!     data vectors
      character(len=stringlen)::ColVar(ParamVectSize)
!     integer(SQLSMALLINT_KIND)::ColVar(ParamVectSize)

      call f90SQLBindParameter(StmtHndlpmm, ColNum, SQL_PARAM_INPUT, SQL_F_CHAR, &
                             SQL_VARCHAR, StringLen-1, int(0,SQLSMALLINT_KIND), &
                             loc(ColVar), StringLen, loc(ColLen), BindRet)
!
      if (BindRet.ne.SQL_SUCCESS .and. BindRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' BindChar error ',Colvar          
        call ShowDiags(SQL_HANDLE_STMT,StmtHndlpmm)
      endif
!
      return
      end
!
! ****BIND 40 VARCHAR  VARIABLES **********************************************
!
      SUBROUTINE BINDCHAR40(Colnum,colvar,collen,bindret)

!     load f90SQL modules
      use f90SQLConstants
      use f90SQL

      IMPLICIT NONE
      INCLUDE 'PARAMETR'
      INCLUDE 'NCNTRL'
      INCLUDE 'PDBDEF'
      INCLUDE 'CANF90SQL'

      integer(SQLUINTEGER_KIND),parameter::ParamVectSize=1
      integer(SQLINTEGER_KIND),parameter:: StringLen=40
      integer(SQLRETURN_KIND)::BindRet
      integer(SQLSMALLINT_KIND)::ColNum
!
!     len/indicator vectors
      integer(SQLINTEGER_KIND):: ColInd(ParamVectSize), ColLen(ParamVectSize)

!     data vectors
      character(len=stringlen)::ColVar(ParamVectSize)
!     integer(SQLSMALLINT_KIND)::ColVar(ParamVectSize)

      call f90SQLBindParameter(StmtHndlpmm, ColNum, SQL_PARAM_INPUT, SQL_F_CHAR, &
                             SQL_VARCHAR, StringLen-1, int(0,SQLSMALLINT_KIND), &
                             loc(ColVar), StringLen, loc(ColLen), BindRet)
!
      if (BindRet.ne.SQL_SUCCESS .and. BindRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' BindChar error ',Colvar          
        call ShowDiags(SQL_HANDLE_STMT,StmtHndlpmm)
      endif
!
      return
      end
!
! ****BIND REAL VARIABLES **********************************************
      SUBROUTINE BINDREAL(Colnum,colvar,colind,bindret)

!     load f90SQL modules
      use f90SQLConstants
      use f90SQL

      IMPLICIT NONE
      INCLUDE 'PARAMETR'
      INCLUDE 'NCNTRL'
      INCLUDE 'PDBDEF'
      INCLUDE 'CANF90SQL'

      integer(SQLUINTEGER_KIND),parameter::ParamVectSize=1
      integer(SQLINTEGER_KIND),parameter:: StringLen2=25
      integer(SQLRETURN_KIND)::BindRet
      integer(SQLSMALLINT_KIND)::ColNum

!     data vectors
!     character(len=StringLen2)::ColVar(ParamVectSize)
!     integer(SQLSMALLINT_KIND)::ColVar(ParamVectSize)
      REAL(SQLREAL_KIND):: ColVar(ParamVectSize)

!     len/indicator vectors
      integer(SQLINTEGER_KIND):: ColInd(ParamVectSize), ColLen(ParamVectSize)
!
      call f90SQLBindParameter(StmtHndlpmm, ColNum, SQL_PARAM_INPUT, SQL_F_FLOAT, &
                    SQL_REAL, int(7,SQLUINTEGER_KIND), int(0,SQLSMALLINT_KIND), &
                    loc(Colvar), int(4,SQLINTEGER_KIND), loc(COLInd), BindRet) 
!
      if (BindRet.ne.SQL_SUCCESS .and. BindRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' Bindreal  error ',Colvar          
        call ShowDiags(SQL_HANDLE_STMT,StmtHndlpmm)
      endif
!
    return
    end

!
! ****BIND REAL*8 VARIABLES **********************************************
      SUBROUTINE BINDDBLE(Colnum,colvar,colind,bindret)

!     load f90SQL modules
      use f90SQLConstants
      use f90SQL

      IMPLICIT NONE
      INCLUDE 'PARAMETR'
      INCLUDE 'NCNTRL'
      INCLUDE 'PDBDEF'
      INCLUDE 'CANF90SQL'

      integer(SQLUINTEGER_KIND),parameter::ParamVectSize=1
      integer(SQLINTEGER_KIND),parameter:: StringLen2=25
      integer(SQLRETURN_KIND)::BindRet
      integer(SQLSMALLINT_KIND)::ColNum

!     data vectors
!     character(len=StringLen2)::ColVar(ParamVectSize)
!     integer(SQLSMALLINT_KIND)::ColVar(ParamVectSize)
      REAL(SQLDOUBLE_KIND):: ColVar(ParamVectSize)

!     len/indicator vectors
      integer(SQLINTEGER_KIND):: ColInd(ParamVectSize), ColLen(ParamVectSize)
!
      call f90SQLBindParameter(StmtHndlpmm, ColNum, SQL_PARAM_INPUT, SQL_F_DOUBLE, &
                    SQL_DOUBLE, int(15,SQLUINTEGER_KIND), int(0,SQLSMALLINT_KIND), &
                    loc(Colvar), int(8,SQLINTEGER_KIND), loc(COLInd), BindRet) 
!
      if (BindRet.ne.SQL_SUCCESS .and. BindRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' Binddble  error ',Colvar          
        call ShowDiags(SQL_HANDLE_STMT,StmtHndlpmm)
      endif
!
    return
    end

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     PMMPRNTDB
!     Read pmmprnt values then writes to pmmdb database file
!
      SUBROUTINE PMMPRNTDB
      use f90SQLConstants
      use f90SQL

      IMPLICIT NONE
      INCLUDE 'PARAMETR'
      INCLUDE 'NCNTRL'
      INCLUDE 'PDBDEF'
      INCLUDE 'CANF90SQL'

      INTEGER*4      I
      INTEGER*4      CALYR          
      CHARACTER*1    SOL_ID        
      CHARACTER*1    VAR_ID       

      CHARACTER*4    tmpvar
      CHARACTER*2    STAT
      CHARACTER*16   NAME
      CHARACTER*80   LINE
      REAL*8         VALUE(5)
      INTEGER*4      IRET
      INTEGER NUMTABS
      PARAMETER (NUMTABS = 1)        ! total number of database tables


      PLOOPING = 0
      PNUMCOLS = 0
      PDYNSTM = ' '
      PCOLVALS = 0.0
      PCOLV = 0.0
      PCHCOLVALS = ' '
      PCHCOLV = ' '
!
      OPEN(UNIT=79,FILE='jcl.dat',ERR=979,status='Old') !
      OPEN(UNIT=22,FILE='PMMPRNT',ERR=952,STATUS='OLD') !

!     read in scenario date key from jcl.dat file

      READ(79,*) LINE
      READ(79,'(A)')  SCEN
      READ(79,'(A)')  DATE
      SCEN_DATE = trim(SCEN) // '.' // DATE
!
      write(6,*) scen_date
!     OPEN(UNIT=52,FILE='out.' // trim(scen_date) // '.pmmprntdb',status='new') !
      OPEN(UNIT=52,FILE='out.pmmprntdb',status='new') !

!     RD IN PMMPRNT INFORMATION HERE                 
!
10    CONTINUE
!
      READ(22,1000,END=110) CALYR, SOL_ID, VAR_ID,       &
                NAME, STAT, VALUE(1), VALUE(2),             & 
                VALUE(3), VALUE(4), VALUE(5)
!     write(52,*) CALYR, SOL_ID, VAR_ID,       &
!               NAME, STAT,VALUE(1),VALUE(2),VALUE(3),VALUE(4),VALUE(5)

 1000 FORMAT(I2,A1,A1,1X,A16,1X,A2,1X,E11.4E3,2x,E11.4E3,2x,E11.4E3,2x,E11.4E3,2x,E11.4E3) ! EM4
 1300 FORMAT(I2,A1,A1,1X,A16,1X,A2) ! EM4
 1400 FORMAT(I2,A1,A1,1X,A16,1X,A2,1x,E9.4E1,2X,E9.4E1,2X,E11.4E3) ! EM4
! write(6,1000) tmpvar,name,stat,(value(i),i=1,5)
!1000 FORMAT(I2,A1,A1,1X,A,  1X,A2,1X,E9.4E1,2x,E9.4E1,2x,E11.4E3,2x,E11.4E3,2x,E9.4E1) ! EM4



!        --- Write PMM Solution Value to Access Database

            PTNUM = 1
            IF (PLOOPING(PTNUM) .EQ. 0) THEN
              PNUMCOLS(PTNUM) = 9 
              PDYNSTM(PTNUM) = 'INSERT INTO PMMPRNT VALUES(?,?,?,?,?,?,?,?,?,?)'
            ENDIF
            PLOOPING(PTNUM) = PLOOPING(PTNUM) + 1
            PCOLV(PTNUM,1,PLOOPING(PTNUM)) = CALYR
            PCHCOLV(PTNUM,2,PLOOPING(PTNUM)) = SOL_ID // VAR_ID
            PCHCOLV(PTNUM,3,PLOOPING(PTNUM)) = NAME  
            PCHCOLV(PTNUM,4,PLOOPING(PTNUM)) = STAT  
            PCOLV(PTNUM,5,PLOOPING(PTNUM)) = VALUE(1)
            PCOLV(PTNUM,6,PLOOPING(PTNUM)) = VALUE(2)
            PCOLV(PTNUM,7,PLOOPING(PTNUM)) = VALUE(3)
            PCOLV(PTNUM,8,PLOOPING(PTNUM)) = VALUE(4)
            PCOLV(PTNUM,9,PLOOPING(PTNUM)) = VALUE(5)
            IF (PLOOPING(PTNUM) .EQ. PMAXRECS) THEN
              PCHCOLVALS(:,:) = PCHCOLV(PTNUM,:,:)
              PCOLVALS(:,:) = PCOLV(PTNUM,:,:)
              CALL PMM_LOAD_DATA(PDYNSTM(PTNUM),PNUMCOLS(PTNUM),PLOOPING(PTNUM),PCOLVALS,PCHCOLVALS,52)
              PLOOPING(PTNUM) = 0
            ENDIF

        GO TO 10
110     CONTINUE
 
        CLOSE(22)


!      --- WRITE OUT ANY LEFT OVER RECORDS TO THE NEMS DATA BASE
       DO PTNUM = 1 , NUMTABS 
         IF (PLOOPING(PTNUM) .NE. 0) THEN
           PCOLVALS(:,:) = PCOLV(PTNUM,:,:)
           PCHCOLVALS(:,:) = PCHCOLV(PTNUM,:,:)
           CALL PMM_LOAD_DATA(PDYNSTM(PTNUM),PNUMCOLS(PTNUM),PLOOPING(PTNUM),PCOLVALS,PCHCOLVALS,52)
           PLOOPING(PTNUM) = 0
         ENDIF
       ENDDO


      RETURN
!
952   write(6,*) ' error opening pmmprnt file - check that pmmprnt file is uncompressed '   
      stop

979   write(6,*) ' error opening jcl.dat file '
      stop

      END                                        ! END RPT1PMM

!******************************************************************
!*    Subroutine MPTIM2(ICSEC)
!*
!*    Returns the hundredth CPU sec passed since start of job. Calls
!*    MPTIME, an external routine.
!******************************************************************
      SUBROUTINE MPTIM2(ICSEC)
      IMPLICIT NONE
      integer*4 icsec ! cpu seconds (100ths of secs) since start of program
      common/mptims/start_sec,ionce,curcpu,wstart_sec,wonce,cursec
      integer*4 ionce,wonce,wstart_sec,iwsec
      real*4 rsecs
      integer*4 start_sec
      integer*4 curcpu,cursec
      integer*4 time ! portability function time

      call cpu_time(rsecs)   ! processor dependent: seconds on cpu clock 

      icsec=rsecs*100.       ! convert to hundreths of seconds
      if(ionce.ne.1) then
         ionce=1
         start_sec=icsec     ! store first result as start of program 
      endif
      icsec=icsec-start_sec  ! return time since the start of program
      curcpu=icsec           ! store in common mptims

      iwsec=time()         ! number of wall clock seconds since 1970  
      if(wonce.ne.1) then
         wonce=1
         wstart_sec=iwsec
      endif
      iwsec=iwsec-wstart_sec   ! wall clock seconds since start of program
      cursec=iwsec*100     ! store in common, in hundredths

      RETURN
      END
!******************************************************************
!*    Subroutine MPTIM3(ICSEC,ICWALL)
!*
!*    Same as MPTIM2, above, and also returns the wall clock time.
!******************************************************************
      SUBROUTINE MPTIM3(ICSEC,ICWALL)
      IMPLICIT NONE

      integer*4 icsec ! cpu seconds (100ths of secs) since start of program
      integer*4 icwall! wall seconds (100ths of secs) since start of program
      common/mptims/start_sec,ionce,curcpu,wstart_sec,wonce,cursec
      integer*4 ionce,wonce,wstart_sec,iwsec
      real*4 rsecs
      integer*4 start_sec
      integer*4 curcpu,cursec
      call mptim2(icsec)
      icwall=cursec
      RETURN
      END
!
