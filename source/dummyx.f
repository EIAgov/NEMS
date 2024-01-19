! $Header: m:/default/source/RCS/dummyx.f,v 1.6 2019/08/02 14:18:07 pkc Exp $
      subroutine load_data(SQLStmt, NUMCOL, LOOPINGS, COLVAL, CHCOLVAL, MSGFIL)
      implicit none    
      INTEGER,parameter:: MAXCOLS=75
      INTEGER,parameter:: MAXRECS=100
      INTEGER,parameter:: DSIZE=MAXRECS
      REAL*8 :: COLVAL(MAXCOLS,DSIZE)

      integer,parameter:: StringLen2=25
      
      CHARACTER(len=StringLen2):: CHCOLVAL(MAXCOLS,DSIZE)
      CHARACTER(len=220)::SQLStmt
      integer numcol,loopings,msgfil
      write(6,*) 'This module not linked in to the current run. '         
      return
      end
 
! warning:  the 64-bit linker generates a fatal error if duplicate object modules
! are found. the message is "invalid or corrupt file.  File contains invalid .pdata contributions.'
! Therefore, duplicates for NEMS modules that are turned off have been eliminated.  This
! generates harmless link warnings for unresolved references that can be ignored.

