C                  ::: GREDEXP2.FOR  8-15-95 :::
C
C This contains REDUCE routine REDEX2...more expensive tests.
C
      SUBROUTINE REDEX2(CLIST,ZALPHA,NALPHA,*,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
      INCLUDE 'DCREDUCE.'
CITSO      INCLUDE (DCREDUCE)
CI$$INSERT DCREDUCE
C
C This applies more expensive reduction tests (not iterative).
C    First  alternate return is for infeasibility.
C    Second alternate return is for i/o error.
C Tests are:   FORTHCOMING
C
      CHARACTER*128    CLIST
      DOUBLE PRECISION ZALPHA(NALPHA)
C ::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C
C NORMAL RETURN
9000  RETURN
C ERROR RETURN
CC13010 RETURN 2
C
C ** REDEX2 ENDS HERE
      END
