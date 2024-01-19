! $Header: M:/default/source/RCS/dumpack.f,v 1.4 2014/01/29 01:11:46 dsa Exp $
! OML-related subroutines removed (as obsolete) by John Stone in September 2013

 SUBROUTINE GOMHOT(pckfil,rcode)
 implicit none
! this contains a dummy routine in place of gomhot (the OML/ANALYZE Interface that will
! create a ".PCK" (pack) file.  Howver, that uses up a lot of memory, so this is a substitute
! for it
! dummy version for use when PACK=0 (no analyze file creation) is on
      character*8 pckfil
      integer*4 rcode
      return
      end
