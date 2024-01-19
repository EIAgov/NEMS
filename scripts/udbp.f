! $Header: m:/default/source/RCS/udbp.f,v 1.10 2014/08/09 04:49:56 cga Exp $
!Header: m:/default/source/RCS/udbp.f,v 1.8 2011/09/13 18:17:52 lc2 Exp $
!ORCLTABS.f90 program
!
      PROGRAM DBPGRP

      use dfwin
      use f90SQLConstants
      use f90SQL
      IMPLICIT NONE
      INCLUDE 'PARAMETR'
      INCLUDE 'NCNTRL'
      INCLUDE 'EMMPARM'
      INCLUDE 'CONTROL'
      INCLUDE 'EUSPRC'
      INCLUDE 'EDBDEF'
      INCLUDE 'CANF90SQL'
      integer P_Handle,iret

!   lower the priority level of this program
        P_Handle=GetCurrentProcess()
        iret=SetPriorityClass(P_Handle,IDLE_PRIORITY_CLASS)
       oraclesw=3

       CALL TSTACC  
!      CALL WRGRPDB
!      CALL DEFNTABS
       CALL READ_DB_DATA 
       CALL CRMXITR
       CALL ORCLDISC

       STOP
       END


      SUBROUTINE TSTACC
!     **********************  test connection to Microsoft Access database

!    load f90SQL modules
     use f90SQLConstants
     use f90SQL
     use dflib 

      IMPLICIT NONE
      INCLUDE 'PARAMETR'
      INCLUDE 'NCNTRL'
      INCLUDE 'EMMPARM'
      INCLUDE 'CONTROL'
      INCLUDE 'EUSPRC'
      INCLUDE 'EDBDEF'
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
      fname = trim(outdir) // '/EMMDB.mdb'
!     write(6,*) ' fname ',fname

!      allocate an environment handle
      call f90SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, EnvHndl, iRet)
!      write(6,*) ' done with tstacc environ handle '

!      Set ODBC version we will be using (3.x in this case)
      call f90SQLSetEnvAttr(EnvHndl, SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC3, iRet)
!      write(6,*) ' done with environ attr '

!      Allocate a connection handle
       call f90SQLAllocHandle(SQL_HANDLE_DBC,EnvHndl, ConnHndl, iRet)
!      write(6,*) ' done with connection handle '

!      Create a connection string  
!     connstr = 'nemsdb' 
      connstr = 'DBQ='//trim(fname)//';DRIVER={Microsoft Access Driver (*.mdb)}'
       write(6,*) ' connstr ',trim(connstr)             

!      connect to access data source nemsdb  
!     call f90SQLConnect(ConnHndl,connstr,'','',iRet)

!      connect to Microsoft access database
      call f90SQLDriverConnect(ConnHndl, f90SQL_NULL_PTR,ConnStr, & 
                               ConnStr,ConnStrLen,SQL_DRIVER_COMPLETE,iRet)

      if (iRet.eq.SQL_SUCCESS .or. iRet.eq. SQL_SUCCESS_WITH_INFO) then
           print *,'microsoft access database connection successful'
      else
          print *,'Error connecting to data source'
          call ShowDiags(SQL_HANDLE_DBC,ConnHndl)
      endif

!     set autocommit to off
!     call f90SQLSetConnectAttr(ConnHndl,SQL_ATTR_AUTOCOMMIT,SQL_AUTOCOMMIT_OFF,iRet)
!      if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
!       write(6,*) ' autocommit error '
!call ShowDiags(SQL_HANDLE_DBC,ConnHndl)
!      endif
!    write(uf_dbs,*) ' done with set autocommit off '            

!      Allocate statement handle
       call f90SQLAllocHandle(SQL_HANDLE_STMT,ConnHndl, StmtHndl, iRet)
       if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' allocate statememt error version_number '
	call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
       endif
!    write(uf_dbs,*) ' done with allocate statement handle '

        SQLSTMTOLD = ' '
        ORAERR = 0

      Return
      end


! *******************************************************************
! LOAD_DATA   
!
      SUBROUTINE LOAD_DATA (SQLStmt, NUMCOL, LOOPINGS, COLVAL, CHCOLVAL, SCEN2,MSGFIL)
!
!    load f90SQL modules
     USE f90SQLConstants
     USE f90SQL

      IMPLICIT NONE
      INCLUDE 'PARAMETR'
      INCLUDE 'NCNTRL'
      INCLUDE 'EMMPARM'
      INCLUDE 'CONTROL'
      INCLUDE 'EUSPRC'
      INCLUDE 'EDBDEF'
      INCLUDE 'CANF90SQL'

      INTEGER(SQLUINTEGER_KIND),parameter:: DSIZE=MAXRECS
      INTEGER(SQLINTEGER_KIND),parameter:: MaxStringLen=256
      INTEGER(SQLINTEGER_KIND),parameter:: StringLen2=40
      INTEGER(SQLINTEGER_KIND),parameter:: StringLen3=25
      INTEGER(SQLRETURN_KIND)::iRet
      CHARACTER(len=220)::SQLStmt
      CHARACTER(len=25)::stmtshort
      CHARACTER(len=35)::tablename
      INTEGER(SQLSMALLINT_KIND)::ColNumber
      INTEGER(SQLUINTEGER_KIND)::I,ICOL,NUMCOL,LOOPINGS,NRECORD,MSGFIL
      INTEGER(SQLUINTEGER_KIND)::Y,LLL,III,TTT,VVV,IMSG,LOAD_TIME_BEGIN,LOAD_TIME_END

      !data vectors
      CHARACTER(len=StringLen3)::ScenCol(DSIZE),Scen2
      CHARACTER*18 FRSCEN,FDATE
      CHARACTER(len=StringLen2):: CHCOLS(MAXCOLS), CHCOLVAL(MAXCOLS,DSIZE)
      INTEGER(SQLSMALLINT_KIND)::INDEXCOL(DSIZE),CalYear(DSIZE)
      !len/indicator vectors
      INTEGER(SQLINTEGER_KIND):: ChLen(DSIZE),ScenColLen(DSIZE),IndexColInd(DSIZE)
      INTEGER(SQLINTEGER_KIND):: COLSInd(MAXCOLS),CHLens(MAXCOLS)
      INTEGER(SQLINTEGER_KIND):: REALCOLInd(MAXCOLS,DSIZE),CalYearInd(DSIZE),CHCOL(MAXCOLS)


      !data real*4 vectors
!     REAL(SQLREAL_KIND):: COLS(MAXCOLS)
!     REAL(SQLREAL_KIND):: COLVAL(MAXCOLS,DSIZE),REALCOL(MAXCOLS,DSIZE)

      !data real*8 vectors
      REAL(SQLDOUBLE_KIND):: COLS(MAXCOLS)
      REAL(SQLDOUBLE_KIND):: COLVAL(MAXCOLS,DSIZE),REALCOL(MAXCOLS,DSIZE)

      chcol = 0
      chlen = stringlen2
      stmtshort = sqlstmt(:25)
      IMSG = MSGFIL
!     IMSG = 6    ! for testing send writes to nohup.out
!
      IF (ORAERR .NE. 0) RETURN

!     WRITE(IMSG,*) ' Begin load_data subroutine in orcltabs.f '

      CALL MPTIM2(LOAD_TIME_BEGIN)
      WRITE (IMSG,1111) ' Begin load_data table ',sqlstmt(12:47),FLOAT(LOAD_TIME_BEGIN)/100.
1111  FORMAT(10X,A,1x,A,1x,' CPU TIME (SECONDS) = ',F7.2,A,F7.2)
!
      DO III = 1,LOOPINGS
         SCENCOL(III) = trim(SCEN2)
        ScenColLen(III)=len_trim(ScenCol(III))
      ENDDO 
     write(6,*) ' scen1 ',scen1
!
!   If new table is being written then close old statement handles and do prepares, etc for new table
!    If same table is being processed then skip to insert statement
!
    write(imsg,*) ' sqlstmtold ',sqlstmtold(12:47)
!
      IF (SQLSTMT .NE. SQLSTMTOLD) THEN 
!
        IF (SQLSTMTOLD .NE. ' ') THEN
!
!   close the cursor associated to statement, so it can process another insert
        call f90SQLFreeStmt(StmtHndl,SQL_CLOSE, iRet)
        if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
          write(6,*) ' free cursor error ',sqlstmt
	  call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
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
        sqlstmtold = sqlstmt
!
        DO I = 1 , LOOPINGS
          DO ICOL = 1 , NUMCOL
            IF ( CHCOLVAL(ICOL,I) .NE. ' ') &   
              CHCOL(ICOL) = 1
          ENDDO
        ENDDO
!       DO ICOL = 1 , NUMCOL
!        if ( chcol(icol) .ne. 0 ) then
!         write(6,*) ' chcol icol ',icol,chcol(icol)
!        endif
!       enddo
!
!       write(IMSG,*) ' sql statement ',sqlstmt
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
            call bindchar40(icol+1,chcols(icol),chlens(icol),iret)
          ELSE
            call binddble(icol+1,COLS(icol),COLSind(icol),iret)
          ENDIF
        ENDDO
!
!   Prepare the query    
        call f90SQLPrepare(StmtHndl, SQLStmt, iRet)
        if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
          write(6,*) ' prepare  error ',sqlstmt
          call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
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
          call f90SQLExecute(StmtHndl,iRet)
          if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
           write(6,*) ' execute error ',sqlstmt
           write(6,*) ' load colval ',(cols(icol),icol=1,numcol)
           write(6,*) 'load chcolval ',(chcols(icol),icol=1,numcol)
           call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
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
      INCLUDE 'EMMPARM'
      INCLUDE 'CONTROL'
      INCLUDE 'EUSPRC'
      INCLUDE 'EDBDEF'
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
      IF (ORACLESW .LE. 2) THEN
        ORAERR=1
        ORACLESW=0
        ORCLEFD=0
        ORCLECP=0
        ORCLEFP=0
        ORCLCL=0
!       CALL ORCLDISC
        RETURN
!  database test runs.  end run                       
      ELSEIF (ORACLESW .GE. 3) THEN
!       CALL ORCLDISC
        STOP
      ENDIF

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
      INCLUDE 'EMMPARM'
      INCLUDE 'CONTROL'
      INCLUDE 'EUSPRC'
      INCLUDE 'EDBDEF'
      INCLUDE 'CANF90SQL'

      INTEGER(SQLRETURN_KIND)::iRet

!     IF (ORAERR .NE. 0) RETURN

!disconnect
call f90SQLDisconnect(ConnHndl, iRet)
      if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' disconnect handle error '
	call ShowDiags(SQL_HANDLE_DBC,ConnHndl)
      endif

!release connection handles
call f90SQLFreeHandle(SQL_HANDLE_DBC, ConnHndl, iRet)
      if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' release connection handle error '
	call ShowDiags(SQL_HANDLE_DBC,ConnHndl)
      endif

!release environment handle
call f90SQLFreeHandle(SQL_HANDLE_ENV, EnvHndl, iRet)
      if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' release environment handle error '
	call ShowDiags(SQL_HANDLE_ENV,EnvHndl)
      endif

!release statement handle
!   call f90SQLFreeHandle(SQL_HANDLE_STMT, StmtHndl, iRet)
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
      INCLUDE 'EMMPARM'
      INCLUDE 'CONTROL'
      INCLUDE 'EUSPRC'
      INCLUDE 'EDBDEF'
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
    call f90SQLBindParameter(StmtHndl, ColNum, SQL_PARAM_INPUT, SQL_F_SHORT, &
	           SQL_SMALLINT, int (5,SQLUINTEGER_KIND), int(0,SQLSMALLINT_KIND), &
	           loc(Colvar), int(0,SQLINTEGER_KIND), loc(COLInd), BindRet) 
!
      if (BindRet.ne.SQL_SUCCESS .and. BindRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' BindInt  error ',Colvar
	call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
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
      INCLUDE 'EMMPARM'
      INCLUDE 'CONTROL'
      INCLUDE 'EUSPRC'
      INCLUDE 'EDBDEF'
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

      call f90SQLBindParameter(StmtHndl, ColNum, SQL_PARAM_INPUT, SQL_F_CHAR, &
	                     SQL_VARCHAR, StringLen-1, int(0,SQLSMALLINT_KIND), &
	                     loc(ColVar), StringLen, loc(ColLen), BindRet)
!
      if (BindRet.ne.SQL_SUCCESS .and. BindRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' BindChar error ',Colvar          
	call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
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
      INCLUDE 'EMMPARM'
      INCLUDE 'CONTROL'
      INCLUDE 'EUSPRC'
      INCLUDE 'EDBDEF'
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

      call f90SQLBindParameter(StmtHndl, ColNum, SQL_PARAM_INPUT, SQL_F_CHAR, &
	                     SQL_VARCHAR, StringLen-1, int(0,SQLSMALLINT_KIND), &
	                     loc(ColVar), StringLen, loc(ColLen), BindRet)
!
      if (BindRet.ne.SQL_SUCCESS .and. BindRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' BindChar error ',Colvar          
	call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
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
      INCLUDE 'EMMPARM'
      INCLUDE 'CONTROL'
      INCLUDE 'EUSPRC'
      INCLUDE 'EDBDEF'
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
      call f90SQLBindParameter(StmtHndl, ColNum, SQL_PARAM_INPUT, SQL_F_FLOAT, &
                    SQL_REAL, int(7,SQLUINTEGER_KIND), int(0,SQLSMALLINT_KIND), &
                    loc(Colvar), int(4,SQLINTEGER_KIND), loc(COLInd), BindRet) 
!
      if (BindRet.ne.SQL_SUCCESS .and. BindRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' Bindreal  error ',Colvar          
	call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
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
      INCLUDE 'EMMPARM'
      INCLUDE 'CONTROL'
      INCLUDE 'EUSPRC'
      INCLUDE 'EDBDEF'
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
      call f90SQLBindParameter(StmtHndl, ColNum, SQL_PARAM_INPUT, SQL_F_DOUBLE, &
                    SQL_DOUBLE, int(15,SQLUINTEGER_KIND), int(0,SQLSMALLINT_KIND), &
                    loc(Colvar), int(8,SQLINTEGER_KIND), loc(COLInd), BindRet) 
!
      if (BindRet.ne.SQL_SUCCESS .and. BindRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' Binddble  error ',Colvar          
	call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
      endif
!
    return
    end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     WRGRPDB
!     WRITE OUT PLANT GROUP INFORMATION TO EMM DATABASE
!
      SUBROUTINE WRGRPDB
!
!     load f90SQL modules
      use f90SQLConstants
      use f90SQL

      IMPLICIT NONE
      INCLUDE 'PARAMETR'
      INCLUDE 'NCNTRL'
      INCLUDE 'EMMPARM'
      INCLUDE 'CONTROL'
      INCLUDE 'EUSPRC'
      INCLUDE 'EDBDEF'
      INCLUDE 'CANF90SQL'
!
      INTEGER IYR,IRG,IFL,ISP,DUMMYI,INTZERO
      INTEGER I,IGRP,IFP,L,J,ONE,IS,IFUEL
      INTEGER NUMTABS
      PARAMETER (NUMTABS = 1)        ! total number of database tables
      INTEGER RECCOUNT(NUMTABS)
      REAL*4 DUMMYR,DUMZERO,DUM9999
      REAL*8 RCOL(75)
      INTEGER ICOL(75)
      CHARACTER*2 COL,DUMCHAR
!
!
      LOOPING = 0
      NUMCOLS = 0
      RECCOUNT = 0
      DYNSTM = ' '
      CHCOLVALS = ' '
      CHCOLV = ' '
      COLVALS = 0.0
      COLV = 0.0
      TNUM = 1
!
!
      OPEN(UNIT=22,FILE='edbpgrp.txt',ERR=952,STATUS='OLD') !
!
!     RD IN PLANT GROUP INFORMATION HERE             
!
10    CONTINUE
      READ(22,2045,END=110) ICOL(1),ICOL(2),ICOL(3),ICOL(4), &
            ICOL(5),ICOL(6),ICOL(7), &
            ICOL(8),ICOL(9),ICOL(10),ICOL(11), &
            ICOL(12),ICOL(13),ICOL(14), &
            ICOL(15),ICOL(16),ICOL(17), &
            RCOL(1),ICOL(21),RCOL(52), &
            RCOL(53),RCOL(2),RCOL(3), &
            RCOL(4),RCOL(5),RCOL(6),ICOL(24),  &
            ICOL(25),RCOL(7),RCOL(8), &
            RCOL(9),RCOL(10), &
            RCOL(11),RCOL(12),RCOL(13),RCOL(14),  &  
            RCOL(15),RCOL(16), &
            RCOL(17),RCOL(18),RCOL(19), &
            RCOL(20),RCOL(21),     &
            RCOL(22),RCOL(23),RCOL(24), &
            RCOL(25),RCOL(26),ICOL(26), &
            RCOL(27),RCOL(28),RCOL(29), &
            RCOL(30),RCOL(31),ICOL(27), &
            RCOL(32),RCOL(33),RCOL(34), &
            RCOL(35),RCOL(36),ICOL(28),SCEN1 

 2045  FORMAT(1X,5X,2(2X,I3),2(2X,I6),2(2X,I3),2X,I6,10(2X,I3), &
                  2X,F12.3,2X,I6,2X,5(F12.6,2X),F12.4,2X,F9.5,2X,2(I3,2X),15(F15.6,2X), &
                  5(F15.6,2X),I6,2X,    &
                  5(F15.6,2X),I6,2X,    &
                  5(F15.6,2X),I6,2X,A)
!
!     write(6,2045) ICOL(1),ICOL(2),ICOL(3),ICOL(4), &
!           ICOL(5),ICOL(6),ICOL(7), &
!           ICOL(8),ICOL(9),ICOL(10),ICOL(11), &
!           ICOL(12),ICOL(13),ICOL(14), &
!           ICOL(15),ICOL(16),ICOL(17),ICOL(18), &
!           ICOL(19),ICOL(20),  &
!           RCOL(1),ICOL(21),RCOL(52), &
!           RCOL(53),RCOL(2),RCOL(3), &
!           RCOL(4),RCOL(5),RCOL(6),ICOL(24),  &
!           ICOL(25),RCOL(7),RCOL(8), &
!           RCOL(9),RCOL(10), &
!           RCOL(11),RCOL(12),RCOL(13),RCOL(14),  &  
!           RCOL(15),RCOL(16), &
!           RCOL(17),RCOL(18),RCOL(19), &
!           RCOL(20),RCOL(21),     &
!           RCOL(22),RCOL(23),RCOL(24), &
!           RCOL(25),RCOL(26),ICOL(26), &
!           RCOL(27),RCOL(28),RCOL(29), &
!           RCOL(30),RCOL(31),ICOL(27), &
!           RCOL(32),RCOL(33),RCOL(34), &
!           RCOL(35),RCOL(36),ICOL(28),   & 
!           RCOL(37),RCOL(38),RCOL(39), &
!           RCOL(40),RCOL(41),ICOL(29),SCEN1


            IF (LOOPING(TNUM) .EQ. 0) THEN
              NUMCOLS(TNUM) = 61
              DYNSTM(TNUM) = 'INSERT INTO EFD_DPGDT VALUES(?,?,?,?,?,'  &
                             //'?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,'   &
                             //'?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,'   &
                             //'?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
            ENDIF
            LOOPING(TNUM) = LOOPING(TNUM) + 1
            RECCOUNT(TNUM) = RECCOUNT(TNUM) + 1
            COLV(TNUM,1,LOOPING(TNUM))  = ICOL(1)
            COLV(TNUM,2,LOOPING(TNUM))  = ICOL(2)
            COLV(TNUM,3,LOOPING(TNUM))  = ICOL(3)
            COLV(TNUM,4,LOOPING(TNUM))  = ICOL(4)
            COLV(TNUM,5,LOOPING(TNUM))  = ICOL(5)
            COLV(TNUM,6,LOOPING(TNUM))  = ICOL(6)
            COLV(TNUM,7,LOOPING(TNUM))  = ICOL(7)
            COLV(TNUM,8,LOOPING(TNUM))  = ICOL(8)
            COLV(TNUM,9,LOOPING(TNUM))  = ICOL(9)
            COLV(TNUM,10,LOOPING(TNUM)) = ICOL(10)
            COLV(TNUM,11,LOOPING(TNUM)) = ICOL(11)
            COLV(TNUM,12,LOOPING(TNUM)) = ICOL(12)
            COLV(TNUM,13,LOOPING(TNUM)) = ICOL(13)
            COLV(TNUM,14,LOOPING(TNUM)) = ICOL(14)
            COLV(TNUM,15,LOOPING(TNUM)) = ICOL(15)
            COLV(TNUM,16,LOOPING(TNUM)) = ICOL(16)
            COLV(TNUM,17,LOOPING(TNUM)) = ICOL(17)
!           COLV(TNUM,18,LOOPING(TNUM)) = ICOL(18)
!           COLV(TNUM,19,LOOPING(TNUM)) = ICOL(19)
!           COLV(TNUM,20,LOOPING(TNUM)) = ICOL(20)
            COLV(TNUM,18,LOOPING(TNUM)) = RCOL(1)
            COLV(TNUM,19,LOOPING(TNUM)) = ICOL(21)
            COLV(TNUM,20,LOOPING(TNUM)) = RCOL(52)
            COLV(TNUM,21,LOOPING(TNUM)) = RCOL(53)
            COLV(TNUM,22,LOOPING(TNUM)) = RCOL(2)
            COLV(TNUM,23,LOOPING(TNUM)) = RCOL(3)
            COLV(TNUM,24,LOOPING(TNUM)) = RCOL(4)
            COLV(TNUM,25,LOOPING(TNUM)) = RCOL(5)
            COLV(TNUM,26,LOOPING(TNUM)) = RCOL(6)
            COLV(TNUM,27,LOOPING(TNUM)) = ICOL(24)
            COLV(TNUM,28,LOOPING(TNUM)) = ICOL(25)
            COLV(TNUM,29,LOOPING(TNUM)) = RCOL(7)
            COLV(TNUM,30,LOOPING(TNUM)) = RCOL(8)
            COLV(TNUM,31,LOOPING(TNUM)) = RCOL(9)
            COLV(TNUM,32,LOOPING(TNUM)) = RCOL(10)
            COLV(TNUM,33,LOOPING(TNUM)) = RCOL(11)
            COLV(TNUM,34,LOOPING(TNUM)) = RCOL(12)
            COLV(TNUM,35,LOOPING(TNUM)) = RCOL(13)
            COLV(TNUM,36,LOOPING(TNUM)) = RCOL(14)
            COLV(TNUM,37,LOOPING(TNUM)) = RCOL(15)
            COLV(TNUM,38,LOOPING(TNUM)) = RCOL(16)
            COLV(TNUM,39,LOOPING(TNUM)) = RCOL(17)
            COLV(TNUM,40,LOOPING(TNUM)) = RCOL(18)
            COLV(TNUM,41,LOOPING(TNUM)) = RCOL(19)
            COLV(TNUM,42,LOOPING(TNUM)) = RCOL(20)
            COLV(TNUM,43,LOOPING(TNUM)) = RCOL(21)
            COLV(TNUM,44,LOOPING(TNUM)) = RCOL(22)
            COLV(TNUM,45,LOOPING(TNUM)) = RCOL(23)
            COLV(TNUM,46,LOOPING(TNUM)) = RCOL(24)
            COLV(TNUM,47,LOOPING(TNUM)) = RCOL(25)
            COLV(TNUM,48,LOOPING(TNUM)) = RCOL(26)
            COLV(TNUM,49,LOOPING(TNUM)) = ICOL(26) 
            COLV(TNUM,50,LOOPING(TNUM)) = RCOL(27)
            COLV(TNUM,51,LOOPING(TNUM)) = RCOL(28)
            COLV(TNUM,52,LOOPING(TNUM)) = RCOL(29)
            COLV(TNUM,53,LOOPING(TNUM)) = RCOL(30)
            COLV(TNUM,54,LOOPING(TNUM)) = RCOL(31)
            COLV(TNUM,55,LOOPING(TNUM)) = ICOL(27)
            COLV(TNUM,56,LOOPING(TNUM)) = RCOL(32)
            COLV(TNUM,57,LOOPING(TNUM)) = RCOL(33)
            COLV(TNUM,58,LOOPING(TNUM)) = RCOL(34)
            COLV(TNUM,59,LOOPING(TNUM)) = RCOL(35)
            COLV(TNUM,60,LOOPING(TNUM)) = RCOL(36)
            COLV(TNUM,61,LOOPING(TNUM)) = ICOL(28)
            IF (LOOPING(TNUM) .EQ. MAXRECS) THEN
              COLVALS(:,:) = COLV(TNUM,:,:)
              CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,SCEN1,6)
              LOOPING(TNUM) = 0
            ENDIF
    
        GO TO 10
110     CONTINUE
 
        CLOSE(22)

!
!      --- WRITE OUT ANY LEFT OVER RECORDS TO THE NEMS DATA BASE
       DO TNUM = 1 , NUMTABS 
         IF (LOOPING(TNUM) .NE. 0) THEN
           COLVALS(:,:) = COLV(TNUM,:,:)
           CALL LOAD_DATA(DYNSTM(TNUM),NUMCOLS(TNUM),LOOPING(TNUM),COLVALS,CHCOLVALS,SCEN1,6)
           LOOPING(TNUM) = 0
         ENDIF
       ENDDO
       RETURN
!
952     write(6,*) ' error opening dbpgrp.txt file '
        stop

        END

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
!
      SUBROUTINE CRMXITR 
!  ******************************** WRITE GLOBAL DEFINITION TABLES *****
!     load f90SQL modules
      USE f90SQLConstants
      USE f90SQL

      IMPLICIT NONE
      INCLUDE 'PARAMETR'
      INCLUDE 'NCNTRL'
      INCLUDE 'EMMPARM'
      INCLUDE 'CONTROL'
      INCLUDE 'EUSPRC'
      INCLUDE 'EDBDEF'
      INCLUDE 'CANF90SQL'

      integer(SQLUINTEGER_KIND),parameter::ParamVectSize=1
      integer(SQLINTEGER_KIND),parameter:: StringLen2=25
      integer(SQLRETURN_KIND)::iRet
      character(len=255)::SQLStmt
      integer(SQLSMALLINT_KIND)::ColNumber
      integer(SQLUINTEGER_KIND)::I,NRECORD,LLL,III,ITST,INDEX,INT
      character(len=2)::ci

   !close the cursor associated to statement, so it can process another insert
    call f90SQLFreeStmt(StmtHndl,SQL_CLOSE, iRet)
     if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
       write(6,*) ' free cursor error ',sqlstmt
       call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
     endif

   !Release statement handle
    call f90SQLFreeHandle(SQL_HANDLE_STMT, StmtHndl, iRet)
     if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
       write(6,*) ' release statement handle error ',sqlstmt
       call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
     endif
!
!
!    Allocate statement handle
       call f90SQLAllocHandle(SQL_HANDLE_STMT,ConnHndl, StmtHndl, iRet)
       if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
         write(6,*) ' allocate statement handle error ',sqlstmt
         call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
       endif

!Prepare the query    
    SQLStmt= 'SELECT EFD_DOS.CY_INDEX, Max(EFD_DOS.NITR_INDEX) AS MaxOfNITR_INDEX INTO runmaxitr From EFD_DOS '  &
    //'GROUP BY EFD_DOS.CY_INDEX'


    call f90SQLPrepare(StmtHndl, SQLStmt, iRet)
      if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' prepare error version_number ',sqlstmt
	call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
      endif

    !Execute query
     call f90SQLExecute(StmtHndl,iRet)
      if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' execute error version_number ',sqlstmt
	call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
      endif


   !close the cursor associated to statement, so it can process another insert
    call f90SQLFreeStmt(StmtHndl,SQL_CLOSE, iRet)
     if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
       write(6,*) ' free cursor error ',sqlstmt
       call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
     endif

   !Release statement handle
    call f90SQLFreeHandle(SQL_HANDLE_STMT, StmtHndl, iRet)
     if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
       write(6,*) ' release statement handle error ',sqlstmt
       call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
     endif
!
!
!    Allocate statement handle
       call f90SQLAllocHandle(SQL_HANDLE_STMT,ConnHndl, StmtHndl, iRet)
       if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
         write(6,*) ' allocate statement handle error ',sqlstmt
         call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
       endif

!Prepare the query    
    SQLStmt= 'INSERT INTO MAXITR SELECT * FROM RUNMAXITR'


    call f90SQLPrepare(StmtHndl, SQLStmt, iRet)
      if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' prepare error version_number ',sqlstmt
	call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
      endif

    !Execute query
     call f90SQLExecute(StmtHndl,iRet)
      if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
        write(6,*) ' execute error version_number ',sqlstmt
	call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
      endif

   !close the cursor associated to statement, so it can process another insert
    call f90SQLFreeStmt(StmtHndl,SQL_CLOSE, iRet)
     if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
       write(6,*) ' free cursor error ',sqlstmt
       call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
     endif

   !Release statement handle
    call f90SQLFreeHandle(SQL_HANDLE_STMT, StmtHndl, iRet)
     if (iRet.ne.SQL_SUCCESS .and. iRet .ne. SQL_SUCCESS_WITH_INFO) then
       write(6,*) ' release statement handle error ',sqlstmt
       call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
     endif
!

      RETURN
      END
! *******************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! READ_DB_DATA
!  read database table data and write to database tables
!
      SUBROUTINE READ_DB_DATA
!
!     load f90SQL modules
      use f90SQLConstants
      use f90SQL

      IMPLICIT NONE
      INCLUDE 'PARAMETR'
      INCLUDE 'NCNTRL'
      INCLUDE 'EMMPARM'
      INCLUDE 'CONTROL'
      INCLUDE 'EUSPRC'
      INCLUDE 'EDBDEF'
      INCLUDE 'CANF90SQL'
!
      INTEGER MAXTABSW
      PARAMETER (MAXTABSW=200)        ! total number of database tables
      INTEGER NUMCHCOLS,CHCOLIND(MAXCOLS),NUMCOL,TABSET,NUMCOLSW(MAXTABSW)
      INTEGER IYR,IRG,IFL,ISP,DUMMYI,INTZERO, NUMTABS
      INTEGER I,IGRP,IFP,L,J,ONE,IS,IFUEL,ICOL,ICHCOLS,LOOPINGW(MAXTABSW)
      REAL*8  COLVW(MAXTABSW,MAXCOLS,MAXRECS)
      CHARACTER*2 COL
      CHARACTER*40 CHCOL(MAXCOLS),CHCOLVW(MAXTABSW,MAXCOLS,MAXRECS)
      CHARACTER*35 TBLENAME,TABNAME(MAXTABSW)
      CHARACTER*220  TSTSTM,DYNSTMW(MAXTABSW)
!
!
      NUMTABS = 0
      LOOPINGW = 0
      NUMCOLSW = 0
      DYNSTMW = ' '
      CHCOLVALS = ' '
      CHCOLVW = ' '
      TABNAME = ' '
      COLVALS = 0.0
      COLVW = 0.0
      TNUM = 0
      TABSET = 0
      
!
!
      OPEN(UNIT=22,FILE='edbpgrp.txt',ERR=952,STATUS='OLD') !
      READ(22,*) SCEN1
      write(6,*) scen1
!
!
10    CONTINUE
      READ(22,*,END=110) tblename,numcol,numchcols
!     write(6,15) ' after read tble numcol numchcols ',tblename,numcol,numchcols
!15 format(a,a,i5,i5)
      tblename = trim(tblename)
  
      TABSET = 0
      IF (NUMTABS .NE. 0) THEN
        DO I = 1 , NUMTABS
          IF ( tabname(I) .EQ. tblename ) then
            TNUM = I
            TABSET = 1
          ENDIF
        ENDDO
      ELSE  
         TNUM = NUMTABS + 1
         TABNAME(TNUM) = TBLENAME
         NUMCOLSW(TNUM) = NUMCOL
         TABSET = 1
         NUMTABS = NUMTABS + 1
      ENDIF

      IF ( TABSET .EQ. 0 ) THEN
         TNUM = NUMTABS + 1
         TABNAME(TNUM) = TBLENAME
         NUMCOLSW(TNUM) = NUMCOL
         TABSET = 1
         NUMTABS = NUMTABS + 1
      ENDIF
!     write(6,25) 'tblename tnum tabname numcolsw ',tblename,tnum,tabname(tnum),numcolsw(tnum)
!25 format(a,a25,2x,i5,2x,a25,2x,i5)
 
      IF ( TABSET .EQ. 0 )  &
        write(6,*) ' tabset still 0  ',tblename


        DYNSTMW(TNUM) = 'INSERT INTO ' // TRIM(TBLENAME) // ' VALUES(' // repeat('?,',numcolsw(tnum)) // '?)'
        DYNSTMW(TNUM) = TRIM(DYNSTMW(TNUM))
!       write(6,*) ' DYNSTM ',dynstmw(tnum)

       LOOPINGW(TNUM) = LOOPINGW(TNUM) + 1
       BACKSPACE 22   

       IF ( numchcols .eq.0 ) then
         read(22,*) tblename,numcol,numchcols,(colvw(tnum,icol,loopingw(tnum)),icol=1,numcolsw(tnum))
       ELSE 
         read(22,*) tblename,numcol,numchcols,(colvw(tnum,icol,loopingw(tnum)),icol=1,numcolsw(tnum)),   &
            (chcolind(ichcols),ichcols=1,numchcols),(chcol(ichcols),ichcols=1,numchcols)    
       ENDIF

!      IF ( NUMCHCOLS .EQ. 0 ) THEN
!        write(6,2343)  tblename,numcol,numchcols,(colvw(tnum,icol,loopingw(tnum)),icol=1,numcolsw(tnum))
!      ELSE
!        write(6,2345)  tblename,numcol,numchcols,(colvw(tnum,icol,loopingw(tnum)),icol=1,numcolsw(tnum)),       &
!           (chcolind(ichcols),ichcols=1,numchcols),(chcol(ichcols),ichcols=1,numchcols)    
!      ENDIF

 2343 format(a,2x,I3,2x,I3,<numcolsw(tnum)>(f15.6,2x))
 2345 format(a,2x,I3,2x,I3,<numcolsw(tnum)>(f15.6,2x),2x,<numchcols>(I3,2x),<numchcols>(a,2x))
       DO ICHCOLS = 1 , NUMCHCOLS
        CHCOLVW(TNUM,CHCOLIND(ICHCOLS),LOOPINGW(TNUM)) = CHCOL(ICHCOLS)
       enddo

        IF (LOOPINGW(TNUM) .EQ. MAXRECS) THEN
          COLVALS(:,:) = COLVW(TNUM,:,:)
          CHCOLVALS(:,:) = CHCOLVW(TNUM,:,:)
          CALL LOAD_DATA(DYNSTMW(TNUM),NUMCOLSW(TNUM),LOOPINGW(TNUM),COLVALS,CHCOLVALS,SCEN1,6)
          LOOPINGW(TNUM) = 0
        ENDIF
    
        GO TO 10
110     CONTINUE
 
        CLOSE(22)

!
!      --- WRITE OUT ANY LEFT OVER RECORDS TO THE NEMS DATA BASE
       DO TNUM = 1 , NUMTABS 
         IF (LOOPINGW(TNUM) .NE. 0) THEN
           COLVALS(:,:) = COLVW(TNUM,:,:)
           CHCOLVALS(:,:) = CHCOLVW(TNUM,:,:)
!          IF ( tnum .eq. 38 ) then
!         write(6,*) ' before load_data call ',dynstmw(tnum),numcolsw(tnum),loopingw(tnum),(colvals(i,1),i=1,numcolsw(tnum)),(chcolvals(i,1),i=1,numcolsw(tnum)),scen1
!          ENDIF
           CALL LOAD_DATA(DYNSTMW(TNUM),NUMCOLSW(TNUM),LOOPINGW(TNUM),COLVALS,CHCOLVALS,SCEN1,6)
           LOOPINGW(TNUM) = 0
         ENDIF
       ENDDO
       RETURN
!
952     write(6,*) ' error opening dbpgrp.txt file '
        stop

        END
