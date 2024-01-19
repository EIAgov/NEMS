! $Header: m:/default/source/RCS/nemsfunct.f,v 1.1 2019/06/28 14:40:06 pkc Exp $
      SUBROUTINE CHECK_US(Q,A)
! checks the sum of first 9 elements to see that it matches the 11th.  If
! they don't, scales the first 9 elements such that they total the 11th.
! Used to regionalize values that have changed at a national level but which
! are not yet reflected in the SEDS data available .
      implicit none
      real q(11)
      character*(*) a
      real ussum,scale_factor,toler/.1/
      ussum=sum(q(1:9))
      if(abs(ussum-q(11)).gt.toler) then
        if(ussum.gt.0.) then
          write(6,*) ' Revising regional values for ',trim(a),' from ',ussum,' to ', q(11)
          scale_factor=q(11)/ussum
          q(1:9)=q(1:9)*scale_factor
        endif
      else
        ussum=0. ! for debug stop only     
      endif
      RETURN
    
      END
!*********************************************************************
      FUNCTION CENSUS(IS)
      INTEGER CENSUS,ICENSD(53)
! RETURNS A CENSUS REGION NUMBER FROM 1 TO 9 OR 11, GIVEN A
! NUMBER FROM 1 TO 53 FROM THE LISTS BELOW
!  1.  New England
!  2.  Middle Atlantic
!  3.  East North Central
!  4.  West North Central
!  5.  South Atlantic
!  6.  East South Central
!  7.  West South Central
!  8.  Mountain
!  9.  Pacific
! 10.  Possible future use for California
! 11.  United States
!
! INDEX FROM 1 TO 53, FIPS ABBREVIATION, CENSUS INDEX:
!    1 AK   9           27 MT   8
!    2 AL   6           28 NC   5
!    3 AR   7           29 ND   4
!    4 AZ   8           30 NE   4
!    5 CA   9           31 NH   1
!    6 CO   8           32 NJ   2
!    7 CT   1           33 NM   8
!    8 DC   5           34 NV   8
!    9 DE   5           35 NY   2
!   10 FL   5           36 OH   3
!   11 GA   5           37 OK   7
!   12 HI   9           38 OR   9
!   13 IA   4           39 PA   2
!   14 ID   8           40 RI   1
!   15 IL   3           41 SC   5
!   16 IN   3           42 SD   4
!   17 KS   4           43 TN   6
!   18 KY   6           44 TX   7
!   19 LA   7           45 UT   8
!   20 MA   1           46 VA   5
!   21 MD   5           47 VT   1
!   22 ME   1           48 WA   9
!   23 MI   3           49 WI   3
!   24 MN   4           50 WV   5
!   25 MO   4           51 WY   8
!   26 MS   6           52 US  11
!                       53 48  11
      DATA ICENSD/ 9, 6, 7, 8, 9, 8, 1, 5, 5, 5, &
                   5, 9, 4, 8, 3, 3, 4, 6, 7, 1, &
                   5, 1, 3, 4, 4, 6, 8, 5, 4, 4, &
                   1, 2, 8, 8, 2, 3, 7, 9, 2, 1, &
                   5, 4, 6, 7, 8, 5, 1, 9, 3, 5, &
                   8,11,11/
      CENSUS=0
      IF(IS.GE.1.AND.IS.LE.53) THEN
        CENSUS=ICENSD(IS)
      ELSE
        WRITE(6,*) 'BAD STATE INDEX IN FUNCTION CENSUS. IS=',IS
      ENDIF
      RETURN
      END
!******************************************************************
      FUNCTION ISTATE(STATE)
      CHARACTER*2 STATES(53),STATE
      DATA STATES/ 'AK','AL','AR','AZ','CA','CO','CT','DC','DE','FL', &
                   'GA','HI','IA','ID','IL','IN','KS','KY','LA','MA', &
                   'MD','ME','MI','MN','MO','MS','MT','NC','ND','NE', &
                   'NH','NJ','NM','NV','NY','OH','OK','OR','PA','RI', &
                   'SC','SD','TN','TX','UT','VA','VT','WA','WI','WV', &
                   'WY','US','48'/
      ISTATE=0
      DO 10 I=1,53
        IF(STATE.EQ.STATES(I))THEN
          ISTATE=I
          RETURN
        ENDIF
10    CONTINUE
      RETURN
      END
