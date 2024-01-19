! $Header: m:/default/source/RCS/refrpt.f,v 1.176.1.1 2012/04/26 19:09:59 pkc Exp $
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  TW4 7/7/95 Moved RPTA8PMM to REFRPT.F from refine.f
!  TW4 7/27/95 Moved ALKRPT, USTOT, PMMRPTRW, OBJRPT, FCCRPT from refine.f
!  TW4 8/17/95 Reformat and organize reports
!  EM4 6/14/96 UPDATE FOR RPT1PMM
!  EM4 3/18/97 FORMATS ON WRITE STATEMENTS MADE IN RPT6PMM
!  EM4 3/18/97 CODE AVAILABLE TO WRITE PROD EXP LL & UL TO @.IMPCURV (SEE EM4P:)
!  EM4 4/28/97 CODE ADDED TO WRITE GASOBLND DATA TO @.IMPCURV (SEE EM4G)
!  EM4 5/06/97 ADDED CODE TO WRITE T:VPELAS FOR COGENER.DAT
!  EM4 5/9/97  ADDED CODE TO WRITE ETH,E85 COST COEF FOR ETHANOL.DAT

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE RPT1PMM
      IMPLICIT NONE

      write(6,'("  refrpt:  empty")')
      RETURN
      END
