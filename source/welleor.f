! $Header: m:/default/source/RCS/welleor.f,v 1.43 2019/08/22 18:27:39 dh5 Exp $
!***************************************************************
!***********************************************************************************
!
!  This subroutine was created 3.10.10 by MC.  It determines if the project is
!  eligible for hydraulic fracturing.  If so, it calculates the fracture length
!  for the project.
!
!***********************************************************************************

       subroutine frac_screen(ires,ifrac)

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
       include 'ogsmugr'
       INCLUDE 'ogsml48'
       INCLUDE 'ogsmbfw'

       INTEGER ires                             !reservoir number (without frac)
       logical ifrac                             !indicates if reservoir passes screen
       integer irc

       real flen                                !fracture length
       real fw_use,fc_use,kf_use,mxf_use

       if(apcode(ires).eq.0) irc = 1
       if(apcode(ires).eq.10.or.apcode(ires).eq.17) irc = 2

       fw_use = 0.0
       fc_use = 0.0
       kf_use = 0.0
       mxf_use = 0.0
       flen = 0.0
       ifrac = .false.

       if (fracture) then                                !technology check
        if(hfrac_proc(irc)) then                         !resource check
         if (aperm(ires).le.hfrac_perm) then             !perm check
           if (aapi(ires).le.hfrac_api) then             !api check
             if(adepth(ires).le.hfrac_depth) then        !depth check
               if(hfrac_region(aregion(ires))) then      !region check
                 ifrac = .true.
                 if (aperm(ires).lt.hfp_cut) then
                   fw_use = fw_low
                   fc_use = fc_low
                   kf_use = kf_low
                   mxf_use = mxf_low
                 else
                   fw_use = fw_high
                   fc_use = fc_high
                   kf_use = kf_high
                   mxf_use = mxf_high
                 end if
                 flen = (kf_use*fw_use)/(aperm(ires)*fc_use)
                 if(flen.le.0.0) then
                   flen = 0.0
                   ifrac = .false.
                 end if
                 if(flen.gt.mxf_use) flen = mxf_use
                 if(flen.gt.((apatsiz(ires)*43560.0)/8.0)**0.5) flen = ((apatsiz(ires)*43560.0)/8.0)**0.5 
                                                                      !restrict within the pattern
               end if   !region check
             end if   !end depth check
           end if   !end api check
         end if     !end perm check
        end if   !end resource check
       end if
       aflen(ires) = flen
       frac(ires) = ifrac

!         if(aresid(ires).eq.'UOMT2272610') then
!           print*, aresid(ires),aflen(ires),aperm(ires),frac(ires),'frac'
!           print*, fw_use,fc_use,kf_use,mxf_use
!           pause
!         end if


!       if(aresid(ires).eq.'UOND2325710') then
!          write(793,1795) aresid(ires),aflen(ires),aperm(ires),adepth(ires),frac(ires)
!       end if
 1795   format (a11,3x,f10.2,3x,f10.2,2x,f10.2,2x,l1)

       end subroutine

!***********************************************************************************
!
!  This subroutine was created 3.11.10 by MC.  It determines the oil and associated
!  gas/water production from the hydraulic fracturing of an oil well.  The fractured
!  well production is stored in the advanced case (itech = 2), the non-fractured
!  well production data is stored in the base case (itech = 1).
!
!***********************************************************************************

       subroutine hydrofrac(ires)

       implicit none


        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
       include 'ogsmugr'
       INCLUDE 'ogsml48'
       INCLUDE 'ogsmbfw'

       INTEGER ires                             !reservoir number for non-fractured project
       INTEGER itech,iy,ic

       real gorh,apih,depthh,vdph,payh,permh,ooiph,porh,boih,techoil
       real cumoil(max_yr),rfann_nofrac(max_yr),rfcum_nofrac(max_yr),lnrf_nofrac(max_yr)
       real rfcum_frac(max_yr),rfann_frac(max_yr),annoil(max_yr)
       real rfcum_max
       real omc,omc2

! initialization of variables
       gorh = 0.0
       apih = 0.0
       depthh = 0.0
       vdph = 0.0
       payh = 0.0
       permh = 0.0
       porh = 0.0
       boih = 0.0
       ooiph = 0.0
       techoil = 0.0

       do iy = 1,max_yr
         cumoil(iy) = 0.0
         annoil(iy) = 0.0
         rfann_nofrac(iy) = 0.0
         rfcum_nofrac(iy) = 0.0
         lnrf_nofrac(iy) = 0.0
         rfcum_Frac(iy) = 0.0
         rfann_frac(iy) = 0.0
       end do

! assignment of the necessary parameters
       gorh = agor(ires)
       apih = aapi(ires)
       depthh = adepth(ires)
       payh = apay(ires)
       permh = aperm(ires)
       porh = aphi(ires)/100.0
       boih = aboi(ires)

       ooiph = 7758.8*(apatsiz(ires)*payh*porh*(asoi(ires)/100.0))/boih/1000.0

! overwrite for unconventional undiscovered oil
!  Reason: the data coming into the model is for a project consisting of wells from different
!  size classes and with different pays.  For this reason, the ooip and pay will be back calculated.
       if(apcode(ires).eq.17) then
        ooiph = 0.0
        payh  = 0.0
        do iy=1,max_yr
          if(iy.eq.1) then
            techoil = aprodoil(ires,iy,1)
          else
            techoil = techoil + aprodoil(ires,iy,1)
          end if
        end do
        ooiph = techoil/0.2                          !mbbl
        payh = (techoil*1000.0*boih)/(0.2*apatsiz(ires)*(aphi(ires)/100.0)*(asoi(ires)/100.0)*7758.0)
       end if

       do iy = 1,max_yr
        if(iy.eq.1) then
          if(apcode(ires).eq.0) then
            cumoil(iy) = aprodoil(ires,iy,1)/anwelloil(ires)
          else
            cumoil(iy) = aprodoil(ires,iy,1)
          end if
        else
          if(apcode(ires).eq.0) then
            cumoil(iy) = cumoil(iy-1)+aprodoil(ires,iy,1)/anwelloil(ires)
          else
            cumoil(iy) = cumoil(iy-1)+aprodoil(ires,iy,1)
          end if
        end if
        if(apcode(ires).eq.0) then
          rfann_nofrac(iy) = aprodoil(ires,iy,1)/anwelloil(ires)/ooiph
        else
          rfann_nofrac(iy) = aprodoil(ires,iy,1)/ooiph
        end if

        rfcum_nofrac(iy) = cumoil(iy)/ooiph
        if(rfcum_nofrac(iy).gt.0.0) lnrf_nofrac(iy) = alog(rfcum_nofrac(iy))
       end do

! calculation of rfcum_max
       rfcum_max = 0.0
       rfcum_max = rfcum_nofrac(max_yr)*1.254


! calculation of the rfcum_frac
       do iy = 1,max_yr
         rfcum_frac(iy) = -0.884
         rfcum_frac(iy) = rfcum_frac(iy) + 1.081*boih
         rfcum_frac(iy) = rfcum_frac(iy) + (-0.01275)*payh*rfann_nofrac(iy)
         rfcum_frac(iy) = rfcum_frac(iy) + 0.05516*boih*boih*lnrf_nofrac(iy)
         rfcum_frac(iy) = rfcum_frac(iy) + 0.00177*apih
         rfcum_frac(iy) = rfcum_frac(iy) + (-0.09685)*boih*boih*boih
         rfcum_frac(iy) = rfcum_frac(iy) + (-0.147)*porh*payh*rfann_nofrac(iy)
         rfcum_frac(iy) = rfcum_frac(iy) + 1.046e-5*permh*gorh*rfann_nofrac(iy)
         rfcum_frac(iy) = rfcum_frac(iy) + (-1.174e-5)*permh*permh*rfann_nofrac(iy)
         rfcum_frac(iy) = rfcum_frac(iy) + 0.04854*gorh*rfann_nofrac(iy)*rfann_nofrac(iy)
         rfcum_frac(iy) = rfcum_frac(iy) + 0.317*payh*rfann_nofrac(iy)*rfann_nofrac(iy)
         rfcum_frac(iy) = rfcum_frac(iy) + 0.01727*gorh*rfann_nofrac(iy)
         rfcum_frac(iy) = rfcum_frac(iy) + (-3.855e-5)*apih*gorh*rfann_nofrac(iy)
         rfcum_frac(iy) = rfcum_frac(iy) + 0.00846*gorh*lnrf_nofrac(iy)*rfann_nofrac(iy)
         rfcum_frac(iy) = rfcum_frac(iy) + (-1.699e-5)*payh*gorh*rfann_nofrac(iy)
         rfcum_frac(iy) = rfcum_frac(iy) + 6.044e-9*payh*apih*gorh
         rfcum_frac(iy) = rfcum_frac(iy) + (-7.901e-6)*porh*payh*payh

         if(iy.eq.1) then
            if(rfcum_frac(iy).gt.0.1) rfcum_frac(iy) = 0.1
         end if
       end do

! if the type curve yields no production, the reservoir is not a candidate for fracturing.
       if(rfcum_frac(max_yr).le.0.0) then
         aflen(ires) = 0.0
         frac(ires)  = .false.
         goto 321
       end if

!       write (798,11) aresid(ires),rfcum_nofrac(max_yr),rfcum_frac(max_yr)
! 11    format (a11,3x,f12.5,3x,f12.5)

! scale the fractured production
       if(rfcum_frac(max_yr).lt.rfcum_nofrac(max_yr)) then
         do iy = 1,max_yr
            rfcum_frac(iy) = rfcum_frac(iy)*(rfcum_nofrac(max_yr)/rfcum_frac(max_yr))
         end do
       end if

!       if(rfcum_frac(max_yr).gt.rfcum_nofrac(max_yr)) rfcum_frac(max_yr) = rfcum_nofrac(max_yr)

! calculation of the rfann_frac and annual oil production
       do iy = 1,max_yr
         if(iy.eq.1) then
           rfann_frac(iy) = rfcum_frac(iy)
         else
           rfann_frac(iy) = rfcum_frac(iy) - rfcum_frac(iy-1)
         end if
         if(rfann_frac(iy).lt.0.0) rfann_frac(iy) = 0.0
         annoil(iy) = rfann_frac(iy)*ooiph
       end do

       if(annoil(1).eq.0.0) then
         annoil(1) = 0.33*annoil(2)
         annoil(2) = 0.67*annoil(2)
       end if

       omc = 0.0
       omc2 = 0.0

       do iy = 1,max_yr
         if(iy.eq.1.and.annoil(iy).gt.rfcum_max*ooiph) then
           annoil(iy) = rfcum_max*ooiph
         elseif(iy.gt.1) then
           if(annoil(iy)+omc.gt.rfcum_max*ooiph) then
             annoil(iy) = max(rfcum_max*ooiph-omc,0.0)
           end if
         end if
         omc = omc + annoil(iy)
       end do

! final check - if there are 'gaps' in the production, then the reservoir is not a candidate for fracturing.
       do iy = 1,max_yr-1
         if(annoil(iy).eq.0.0.and.annoil(iy+1).gt.0.0) then
            aflen(ires) = 0.0
            frac(ires) = .false.
            goto 321
         end if
       end do

! tranfer the variables to the appropriate arrays for the new project
       do iy = 1,max_yr
           APRODOIL(ires,iy,2) = annoil(iy)
           if(apcode(ires).eq.0) aprodoil(ires,iy,2) = aprodoil(ires,iy,2)*anwelloil(ires)

           if(aprodoil(ires,iy,1).gt.0.0) then
              APRODGAS(ires,iy,2) = annoil(iy)*(aprodgas(ires,iy,1)/aprodoil(ires,iy,1))
           else
              APRODGAS(ires,iy,2) = aprodgas(ires,iy,2)
           end if

           if(aprodoil(ires,iy,1).gt.0.0) then
              APRODWAT(ires,iy,2) = annoil(iy)*(aprodwat(ires,iy,1)/aprodoil(ires,iy,1))
           else
              APRODWAT(ires,iy,2) = APRODWAT(ires,iy,2)
           end if
           aresvoil(ires,iy,2) = 0.0
           aresvgas(ires,iy,2) = 0.0
           airsvoil(ires,iy,2) = 0.0
           airsvgas(ires,iy,2) = 0.0
       end do

!        if(apcode(ires).eq.0) then
!         write (795,1798) aresid(ires),1,ooiph,rfcum_nofrac(max_yr),(aprodoil(ires,iy,1)/anwelloil(ires),iy=1,max_yr-1)
!         write (795,1798) aresid(ires),2,ooiph,rfcum_max,(aprodoil(ires,iy,2)/anwelloil(ires),iy=1,max_yr-1)
!        else
!         write (795,1798) aresid(ires),1,ooiph,rfcum_nofrac(max_yr),(aprodoil(ires,iy,1),iy=1,max_yr-1)
!         write (795,1798) aresid(ires),2,ooiph,rfcum_max,(aprodoil(ires,iy,2),iy=1,max_yr-1)
!        end if
!
! 1798  format (a11,3x,i1,3x,f12.3,3x,f12.5,3x,<max_yr-1>(f12.3,3x))

 321   continue

       end subroutine
