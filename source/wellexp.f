!/ $Header: m:/default/source/RCS/wellexp.f,v 1.109 2020/10/27 19:17:49 DH5 Exp $
!***************************************************************
!     from TIMING.FOR
!     Last change:  MC   12 May 2009    3:36 pm

      SUBROUTINE TIMING

      use ctus_mod, only : co2cost_calc

      implicit none

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include 'ogsmbfw'
      include 'ogsmugr'
      include 'ogsml48'
      include 'ngtdmrep'
      include 'pmmout'
      include 'intout'
      include 'ctus'

      logical ifrac

      real temp_npv
      integer maxyr, maxnum
      integer iyr, inum,ires,jres,kres,ir,ii,jj,xyr,jr
      INTEGER scount                                                !scount = 1 -> run project sort/timing
      INTEGER nprj                                                  !number of EOR projects to consider for selection
      INTEGER eprj                                                  !number of exploration projects to consider for selection
      INTEGER eor_chk
      INTEGER time(8),time2(8),timcode,time0(8)

      INTEGER stmin,stend
      INTEGER eor_cnt,temp_ir,cnt, OGSM_REG

      CHARACTER*9, rid,rid2
      CHARACTER*2  rprc
      CHARACTER*2  jcde
      CHARACTER*1 DEV_ID
      CHARACTER*2 ST_ID

!     START YEAR LOOP

!     do itimeyr = 1,years_study
!        write (725,*) 'starting timing of year: ',itimeyr

      WRITE(OGBUG1,*) 'evaluating year: ',itimeyr
      ecount = 0
      scount = 0
      nprj = 0
      eprj = 0
      eor_cnt = 0

      DO II = itimeyr , max_yr-1
         do kres = 1,max_src
            nat_availco2(8,kres,ii) = 0.
            do ir = 1,7
               nat_availco2(8,kres,ii) = nat_availco2(8,kres,ii) + nat_availco2(IR,kres,ii)
            enddo

            IF (ii.eq.itimeyr) write(ogbug1,*) 'availCO2', curiyr+1989, kres, nat_availco2(8,kres,ii)

         enddo
      enddo

!     DETERMINE CO2 COST MATRIX at CO2 Supply Region (OGSM Regions)

      do OGSM_REG = 1 , max_reg-1
         CALL CO2COST_CALC(OGSM_REG)
      enddo

!     Ranks available CO2 sources by supply region and source type from cheapest to most expensive in each OGSM Production Region

      do OGSM_REG = 1 , max_reg-1
         call co2cost_sort(OGSM_REG)
      enddo

!     ****************************************************************

!     EXPLORATION PROJECTS DETERMINED IN EXTERNAL MODULE

!     call timer(time2)
!     call date_and_time(values=time2)

!     run economics on conventional exploration projects

      do ires = EX_FCRES,EX_CONRES

         if (ogrunop(1).lt.5) then                                          ! set prices -- use NEMS prices not the prices in the input file
            ir = aregion(ires)
            do m=1,max_yr
               oilpricec(m)=dcrdwhp(ir,curiyr)*dladj
               gaspricec(m)=ogwprng(ir,curiyr)*dladj
               oilpriceo(m)=ogprcref(apadd(ires),acrdtype(ires),1)*dladj
               gaspriceo(m)=ogprcl48(ir,3,1)*dladj
            enddo
         endif

         if (.not.timed(ires).and.eligible(ires)) then                                      !only consider the projects not previously timed
            if (apcode(ires).eq.10.or.apcode(ires).eq.16) then         !CONVENTIONAL EXPLORATION RESOURCES!
               CALL CHECK_ACCESS(ires)                                      !check if resource access category is available
               call scenario_screen(ires)
               if (apcode(ires).eq.10) then                                   !mc change B
                  ifrac = .false.                                             !mc change B

!                 if (aresid(ires).eq.'UOND2325710') then
!                    print*, aresiD(ires),aperm(ires),ifrac,' before'
!                 end if

                  call frac_screen(ires,ifrac)                                !mc change B
                  if (ifrac) call hydrofrac(ires)                              !mc change B

!                 write (798,1798) aresid(ires),apcode(ires),itimeyr,ifrac,'lp'
!1798             format (a11,3x,i2,3x,i2,3x,l1,2x,a3)

!                 if (aresid(ires).eq.'UOND2325710') then
!                    print*, aresiD(ires),aperm(ires),ifrac,' after'
!                    pause
!                 end if

               end if                                                        !mc change B

               call economics(ires,itimeyr,1,0,0,aregion(ires))                           !set base and advanced technology levers

               if (apcode(ires).eq.10) then
                  call window_year(IReS,EX_CONRES,1)
               else
                  call window_year(IReS,EX_CONRES,2)
               end if
               first_dec(ires) = first_dec(ires)+itimeyr
               last_dec(ires) = last_dec(ires)+itimeyr

            ELSEIF(APCODE(Ires).EQ.17.OR.APCODE(Ires).EQ.18            & !MC CHANGE 5.12.09
               .OR.APCODE(Ires).EQ.19.OR.APCODE(Ires).EQ.20             & !MC CHANGE 5.12.09
               .OR.APCODE(Ires).EQ.21.OR.APCODE(Ires).EQ.22             & !MC CHANGE 5.12.09
               .OR.APCODE(Ires).EQ.23) THEN      !MC CHANGE 5.12.09    !UNCONVENTIONAL EXPLORATION RESOURCES!

               call scenario_screen(ires)
               if (apcode(ires).eq.17) then                                 
                  ifrac = .false.                                          
                  call frac_screen(ires,ifrac)                            
                  if(ifrac) call hydrofrac(ires)                         

!                 write (795,1798) aresid(ires),apcode(ires),itimeyr,ifrac,'lp'

               end if                                                  

               call economics(ires,itimeyr,1,0,0,aregion(ires))

               if (apcode(ires).eq.17) then
                  call window_year(IReS,EX_CONRES,1)
               else
                  call window_year(IReS,EX_CONRES,2)
               end if
               first_dec(ires) = first_dec(ires)+itimeyr
               last_dec(ires) = last_dec(ires)+itimeyr

               CALL CHECK_ACCESS(ires)                                     !check if resource access category is available
            end if
            ecount = ecount+1

         end if                                                          !end of timing check
      end do

      do ires = fdec,ldec
         if (first_dec(ires).le.itimeyr.and. &
            itimeyr.le.last_dec(ires)) then                                !project is eligible for EOR/ASR

            candidate(apcode(ires)+1,itimeyr) =                        & !track the number of projects which are eligible for EOR/ASR (decline)
               candidate(apcode(ires)+1,itimeyr) + 1
         END if
      end do

!     call timer(time)
!     WRITE(OGBUG1,*) 'end conv expl economics'
!     call date_and_time(values=time)
!     call elapsed_time(time2,time)
!     write (725,*) 'conv expl economics in: ',
!     *   (time-time2)/100.0

!     WRITE(OGBUG1,*) 'revised number of projects: ', new_nres                                    !MC CHANGE 5.5.09

      WRITE(OGBUG1,*) 'total number of projects:   ', nres

!     ****************************************************************
!     ****************************************************************

!     CALCULATE ECONOMICS FOR EOR/ASR PROJECTS
!     ONLY CALCULATE THE ECONOMICS FOR THE ELIGIBLE EOR/ASR PROJECTS

!     EOR PROCESSES : (3) CO2 FLOODING
!                     (4) STEAM FLOODING
!                     (5) POLYMER FLOODING
!                     (7) PROFILE MODIFICATION

!     ASR PROCESSES : (6) INFILL DRILLING
!                     (8) HORIZONTAL CONTINUITY
!                     (9) HORIZONTAL PROFILE

      write (ogbug1,*) 'start economics of EOR in yr: ',itimeyr

!     call timer(time2)
!     call date_and_time(values=time0)

      cnt=0
      do ires = 1, nres         

         if (ogrunop(1).lt.5) then                                          ! set prices -- use NEMS prices not the prices in the input file
            ir = aregion(ires)
            do m = 1 , max_yr
               oilpricec(m)=dcrdwhp(ir,curiyr)*dladj
               gaspricec(m)=ogwprng(ir,curiyr)*dladj
               oilpriceo(m)=ogprcref(apadd(ires),acrdtype(ires),1)*dladj
               gaspriceo(m)=ogprcl48(ir,3,1)*dladj
            enddo
         endif

!        IF (apcode(ires).eq.17.and.aplay_cde(ires).eq.3110) then      ! BAKKEN
!           write(6,*) 'dh5out', aresid(ires),aplay_cde(ires), project_npv(ires),eligible(ires),timed(ires)
!        ENDIF

!        write (695,*) ires,aresid(ires)

         if (.not.timed(ires)) then                                  !consider only the reservoirs not previously timed
            IF (.not.industrial(ires)) THEN                             !do not consider the industrial CO2 projects which have previously been created
               if (eligible(ires)) Then
                  IF (apcode(ires).ge.21.or.(apcode(ires).eq.17.and.aresflag(ires).eq.9)) then      ! DISCOVERED UNDEVELOPED UNCONVENTIONAL GAS AND OIL
!        if (apcode(ires).eq.17) write(6,*) 'dh5out', curiyr+1989, aplay_cde(ires),ir,apadd(ires),acrdtype(ires),anwellinj(ires)
!        if (apcode(ires).eq.17) write(6,*) 'dh5out', curiyr+1989, aplay_cde(ires),ogprcl48(ir,1,1)*dladj,oilpriceo(1)
                     DEV_ID = ''
                     READ(aresid(ires)(1:1),'(a1)') dev_id
                     READ(aresid(ires)(3:4),'(a2)') st_id
                     timcode = 1                                           !mc change 6.19.09 FORCE TIMING
                     IF (itimeyr.le.5.and.apcode(ires).eq.21.and.  &
                        (st_id.eq.'NY'.or.st_id.eq.'MD'.or.st_id.eq.'VA') .and.  &
                        (aplay_cde(ires).ge.6761.and.aplay_cde(ires).le.6777)) timcode = 0    ! MARCELLUS UNDEVELOPED
                     IF (itimeyr.eq.1.and.apcode(ires).eq.21.and.  &
                        (aplay_cde(ires).eq.6461)) timcode = 0     ! NEW ALBANY
                     IF (apcode(ires).ge.18.and.apcode(ires).le.20) timcode = 0    ! 
                     IF (itimeyr.le.1.and.(apcode(ires).eq.17).and.(anwellgas(ires)+anwellinj(ires)).eq.0.) timcode = 0    ! 
                     IF (itimeyr.le.1.and.(apcode(ires).eq.21).and.(anwellgas(ires)+anwellinj(ires)).eq.0.) timcode = 0    ! 
                     IF (itimeyr.le.1.and.(apcode(ires).eq.22).and.(anwellgas(ires)+anwellinj(ires)).eq.0.) timcode = 0    ! 
                     IF (curiyr.le.l48hyr+3.and.(aplay_cde(ires).ge.9901.and.aplay_cde(ires).le.9907)) timcode = 0    ! hypothetical plays
                     IF (curiyr+1989.le.2021.and.(apcode(ires).eq.17).and.(anwellgas(ires)+anwellinj(ires)).eq.0.) timcode = 0    ! COVID-19 no expansion to new areas
                     IF (curiyr+1989.le.2021.and.(apcode(ires).eq.21).and.(anwellgas(ires)+anwellinj(ires)).eq.0.) timcode = 0    ! COVID-19 no expansion to new areas
                     IF (curiyr+1989.le.2021.and.(apcode(ires).eq.22).and.(anwellgas(ires)+anwellinj(ires)).eq.0.) timcode = 0    ! COVID-19 no expansion to new areas
                     IF (aresacc(ires).le.3) timcode = 0

                     IF (timcode.eq.1) THEN
                        call scenario_screen(ires)                      !set base and advanced technology levers
                        call economics(ires,itimeyr,1,0,0,aregion(ires))

!                       if (apcode(ires).eq.21.and.aregion(ires).eq.2) WRITE(OGBUG1,*) 'end UGR economics', itimeyr, aresid(ires), project_npv(ires)
                        if (apcode(ires).eq.23) write(ogbug1,*) 'dh5cbm', curiyr+1989, aresid(ires), aplay_cde(ires), project_npv(ires),anwellinj(ires)
                        if (apcode(ires).eq.22) write(ogbug1,*) 'dh5tgm', curiyr+1989, aresid(ires), aplay_cde(ires), project_npv(ires),anwellinj(ires)
                        if (apcode(ires).eq.21) write(ogbug1,*) 'dh5shl', curiyr+1989, aresid(ires), aplay_cde(ires), project_npv(ires),anwellinj(ires)
                        if (apcode(ires).eq.17) write(ogbug1,*) 'dh5toil', curiyr+1989, aresid(ires), aplay_cde(ires), project_npv(ires),anwellinj(ires)
!                       call date_and_time(values=time)
!                       call elapsed_time(time2,time)

                     END if                                                   !mc change 6.19.09
                  END IF                                                    

                  if (apcode(ires).ge.1.and.apcode(ires).le.9) then      !their economics have been calculated in other sections

!                    WRITE(OGBUG1,*) itimeyr,aresid(ires),'timing'

                     eor_chk = 0
                     rid = '         '
                     read (aresid(ires)(1:9),'(a9)') rid
                     do jres = fdec,ldec                                      !start loop for matching projects
                        IF (eor_chk.eq.0) then
                           rid2 = '         '
                           READ (DRESID(Jres)(1:9),'(A9)') RID2                        !MC CHANGE 5.8.09
                           READ (DRESID(Jres)(10:11),'(A2)') JCDE                      !MC CHANGE 5.8.09

!                          read (aresid(jres)(1:9),'(a9)') rid2                       !MC CHANGE 5.8.09
!                          read (aresid(jres)(10:11),'(a2)') jcde                     !MC CHANGE 5.8.09
!                          WRITE(OGBUG1,*) aresid(ires),'  ',rid,'   ',rid2

                           if (rid.eq.rid2) then
                              eor_chk = 1

!                             if (apcode(jres).eq.0.and.timed(jres)) then         !MC CHANGE 5.8.09  !match project ires to corresponding decline project

                              IF (DPCODE(JRES).EQ.0.AND.DTIMED(JRES)) THEN     !mc change 6.19.09

!                                WRITE(OGBUG1,*) itimeyr,aresid(ires),'timing',DLIFE(jres),stmin,stend,'a'

                                 timcode = 0

!                                co2 window year

                                 IF (apcode(ires).eq.3) then
                                    stend = 10   !5
                                    if (itimeyr .gt. 1 .and. itimeyr .le.  stend+DLIFE(jres)) then               !MC CHANGE 5.8.09
                                       timcode = 1
                                    else
                                       timcode =0
                                    endif
                                    if (itimeyr.eq.1.and.(aresid(ires).eq.'DOTX6041303'.or.aresid(ires).eq.'DOTX6064503')) &   
                                       timcode = 1   ! DENBURY PROJECTS
                                 Endif

!                                Steam process

                                 IF (apcode(ires) .eq. 4) then
                                    stmin = 5    !8
                                    stend = 10   !5

!                                   if (itimeyr.ge.3) then
   
                                    if (itimeyr .ge. DLIFE(jres) - stmin .and. itimeyr .le. stend + DLIFE(jres)) then         !MC CHANGE 5.8.09
                                       timcode = 1
                                    else
                                       timcode =0
                                    endif

!                                   end if

                                 Endif

!                                POLYMER FLOODING

                                 IF (apcode(ires) .eq. 5) then
                                    stmin = 5
                                    stend = 10   !5
                                    if (itimeyr .ge. DLIFE(jres) - stmin .and. itimeyr .le. stend + DLIFE(jres)) then            !MC CHANGE 5.8.09
                                       timcode = 1
                                    else
                                       timcode =0
                                    endif
                                 Endif

!                                INFILL DRILLING

                                 IF (apcode(ires) .eq. 6) then
                                    stmin = 5
                                    stend = 7    !5
                                    if (itimeyr .ge. 1 .and. itimeyr .le. stend + DLIFE(jres)) then                             !MC CHANGE 5.8.09
                                       timcode = 1
                                    else
                                       timcode =0
                                    endif

!                                   end if

                                 Endif

!                                PROFILE MODIFICATION

                                 IF (apcode(ires) .eq. 7) then
                                    stmin = 5      !10
                                    stend = 10     !5

!                                   if (itimeyr.ge.2) then

                                    if (itimeyr .ge. DLIFE(jres) - stmin .and. itimeyr .le. stend + DLIFE(jres)) then            !MC CHANGE 5.8.09
                                       timcode = 1
                                    else
                                       timcode =0
                                    endif
                                 Endif

!                                HORIZONTAL CONTINUITY

                                 IF (apcode(ires) .eq. 8) then
                                    stmin = 5          !10
                                    stend = 7          !5
                                    if (itimeyr .ge. 2 .and. itimeyr .le. stend + DLIFE(jres)) then                             !MC CHANGE 5.8.09
                                       timcode = 1
                                    else
                                       timcode = 0
                                    endif

!                                   END if

                                 Endif

!                                HORIZONTAL PROFILE

                                 IF (apcode(ires) .eq. 9) then
                                    stmin = 5       !10
                                    stend = 7       !5

!                                   if (itimeyr.ge.2) then

                                    if (itimeyr .ge. DLIFE(jres) - stmin .and. itimeyr .le. stend + DLIFE(jres)) then              !MC CHANGE 5.8.09
                                       timcode = 1
                                    else
                                       timcode =0
                                    endif
                                 Endif

                                 IF (timcode.eq.1) THEN
                                    eor_cnt = eor_cnt + 1

!                                   START EOR LOOP

!                                   if (first_dec(jres).le.itimeyr.and.itimeyr.le.last_dec(jres)) then                 !project is eligible for EOR

!                                   track the number of projects which are eligible for EOR/ASR (EOR/ASR)

                                    candidate(apcode(ires)+1,itimeyr) = candidate(apcode(ires)+1,itimeyr) + 1  
                                    read (aresid(ires)(10:11),'(a2)') rprc

!                                   write(774,1774) aresid(ires),itimeyr,timcode
!1774                               format (a11,3x,i2,3x,i2)

                                    if (rprc.eq.'03') then

!                                      call date_and_time(values=time2)

!                                      Determine economic supplies of CO2

                                       OGSM_REG = AREGION(ires)

                                       do ii = s_num(OGSM_REG,itimeyr),1,-1
                                          call economics(IRES,itimeyr,0,s_src(OGSM_REG,ii,itimeyr),0,s_reg(OGSM_REG,ii,itimeyr))
                                          write(ogbug1,*) 'dh5CO2', curiyr+1989, aresid(ires), aplay_cde(ires), project_npv(ires),s_src(OGSM_REG,ii,itimeyr)
                                          if (project_npv(ires).gt.0.) EXIT
                                       enddo
                                       if (ii.eq.0) then
                                          co2cost_max(ires) = -99.99
                                          res_num(ires,itimeyr) = 0
                                          DO xyr = itimeyr + 1 , max_yr - 1
                                             res_num(ires,xyr) = 0
                                          END DO
                                       else
                                          co2cost_max(ires) = s_cost(OGSM_REG,ii,itimeyr)
                                          res_num(ires,itimeyr) = ii
                                          DO xyr = itimeyr + 1 , max_yr - 1
                                             res_num(ires,xyr) = min(ii,S_NUM(OGSM_REG,xyr))
                                             DO jj = ii + 1 , S_NUM(OGSM_REG,xyr)
                                                IF (s_cost(OGSM_REG,jj,xyr) .LE. co2cost_max(ires)) THEN
                                                   res_num(ires,xyr) = jj
                                                END IF
                                             END DO
                                          END DO
                                       endif

!                                      co2cst_min = minval(reg_costco2,DIM=2)
!                                      co2reg_min = minloc(reg_costco2,DIM=2)
!                                      co2src_min = minloc(reg_costco2,DIM=3)

                                       IF (co2cost_max(ires) .gt. -99.) then

                                          write(ogbug1,1701) curiyr+1989, ires, aregion(ires), res_num(ires,itimeyr), co2cost_max(ires)
 1701                                     format (1x,'co2max  ',':',i4,':',i8,':',i2,':',i4,':',f10.2)

                                          do ii = 1,res_num(ires,itimeyr)

!                                            ir = aregion(ires)
!                                            kres = co2src_min(aregion(ires),aregion(ires))
!                                            IF(itimeyr.gt.4) then                         !PIPELINE CONSTRUCTION TIME

                                             ir = s_reg(OGSM_REG,ii,itimeyr)
                                             kres = s_src(OGSM_REG,ii,itimeyr)

!                                            endif

                                             ECO2RANKVAL(IRES,KRES,IR) = -99999.0                                          !MC CHANGE 5.6.09
                                             ECO2LIM(IRES,KRES,IR) = 0                                                     !MC CHANGE 5.6.09

                                             IF(nat_availco2(ir,kres,itimeyr).gt.0.) then   !only run the economics if there is CO2 available in

!                                               IF(nat_availco2(ir,kres,itimeyr)      & !only run the economics if there is CO2 available in
!                                               .gt.0.0.and.cregpr(AREGION(ires),kres).lt.99)    & 
!                                               the first year considered for the project & price is not the default

                                                CALL SCENARIO_SCREEN(IRES)

                                                call economics(IRES,itimeyr,0,kres,0,ir)                                      !MC CHANGE 5.6.09

                                                IF(project_npv(IRES).gt.0.0) then        !if the CO2 project is economically viable	

                                                   ECO2RANKVAL(IRES,KRES,IR) = RANKING_VAL(IRES) 

!                                                  IF THE PROJECT IS ECONOMIC FOR INDUSTRIAL SOURCE STORE THE RANKING VALUE

                                                   ECO2LIM(IRES,KRES,IR)     = ENYRSI(IRES)+1                             !MC CHANGE 5.6.09

                                                   IF(opt_dbg) write (208,1208) aresid(Ires),itimeyr, apcode(Ires), project_npv(IRES),ires

                                                else                                    !if the CO2 project is NOT economically viable
                                                   ECO2RANKVAL(IRES,kres,ir) = -99999.00                                 !MC CHANGE 5.6.09
                                                   ECO2LIM(IRES,KRES,ir)     = 0                                         !MC CHANGE 5.6.09
                                                END if                                  !end CO2 project economic viability check

!                                               WRITE(OGBUG1,*) ARESID(Ires),KRES,PROJECT_NPV(IRES)

                                             ELSE                                                                       !MC CHANGE 5.6.09
                                                ECO2RANKVAL(IRES,KRES,IR) = -99999.00                                       !MC CHANGE 5.6.09
                                                ECO2LIM(IRES,KRES,IR)     = 0                                               !MC CHANGE 5.6.09
                                             END if

!                                            WRITE(OGBUG1,*) KRES,RANKING_VAL(IRES)

                                             IF(ECO2LIM(IRES,KRES,IR).LE.0) ECO2LIM(IRES,KRES,IR)=0                          !MC CHANGE 5.6.09

!                                            WRITE(OGBUG1,*) 'CO2 EOR economics:', ires, kres, eco2rankval(ires,kres,IR)

                                          end do   !kres
                                       endif

!                                      WRITE(OGBUG1,*) 'end CO2 EOR economics', curiyr+1989, ires
!                                      call date_and_time(values=time)
!                                      call elapsed_time(time2,time)

                                    else

!                                      call date_and_time(values=time2)

                                       call scenario_screen(ires)                      !set base and advanced technology levers
                
                                       call economics(ires,itimeyr,1,0,0,aregion(ires))

!                                      WRITE(OGBUG1,*) 'end other EOR economics', ires
!                                      call date_and_time(values=time)
!                                      call elapsed_time(time2,time)

                                    end if

                                    IF(project_npv(ires).gt.0.0)                       & !track the number of economically viable projects
                                       viable(apcode(ires)+1,itimeyr) = &
                                       viable(apcode(ires)+1,itimeyr) + 1

!                                   call rpt_oneliner(ires)                          !write the oneliner report for timed EOR projects

                                    scount = scount+1

!                                   first_dec(ires) = first_dec(jres)
!                                   last_dec(ires)  = last_Dec(jres)

                                    IF(opt_dbg) write (208,1208) aresid(ires),itimeyr,           & !debug - economically viable EOR projects
                                       apcode(ires),project_npv(ires),ires
 1208                               format (a11,2x,i7,2x,i7,2x,f12.2,2x,i7)

                                 else
                                    project_npv(ires) = -99999.99

!                                   ECO2RANKVAL(IRES,KRES,IR)= -99999.99                                              !MC CHANGE 5.6.09
!                                   ECO2LIM(IRES,KRES,IR)    = 0                                                      !MC CHANGE 5.6.09

                                 end if

!                                END EOR LOOP

                              end if                                                !end eligible (timcode=1)
                           end if                                                 !end decline project matching
                        END if
                     end do                                                   !end loop for matching projects (jres)

                  end if                                                     !end screening OUT exploration projects
               end if                                                      !end checking eligiblility
            end if                                                      !end CO2 project duplication check
         END IF                                                      !end of timing consideration

!        WRITE(OGBUG1,*) ARESID(Ires), '   END   ', NRES, ITIMEYR
!        WRITE(OGBUG1,*) (ECO2RANKVAL(IRES,KRES,IR),KRES=1,MAX_SRC)
!        WRITE(OGBUG1,*) ''
!        WRITE(OGBUG1,*) (ECO2LIM(IRES,KRES,IR),KRES=1,MAX_SRC)

      end do ! ires

!     call timer(time)
!     WRITE(OGBUG1,*) 'end EOR economics', itimeyr, eor_cnt
!     call date_and_time(values=time)
!     call elapsed_time(time0,time)
!     write (725,*) 'complete EOR economics: ',
!     *   (time-time2)/100.0

      IF(opt_dbg) write (208,*)

!     ****************************************************************

!     PROJECT RANKING

      if (scount+ecount.gt.0) then                                  !only sort if there are eligible projects from EOR & exploration

!        call timer(time2)
!        call date_and_time(values=time2)

         CALL PROJECT_SORT(nprj,eprj)

!        call timer(time)
!        WRITE(OGBUG1,*) 'end project sort'
!        call date_and_time(values=time)
!        call elapsed_time(time2,time)
!        write (725,*) 'projects sorted in: ',
!        (time-time2)/100.0

         WRITE(OGBUG1,*) 'number of projects ',nprj,eprj,nprj+eprj

!        ****************************************************************

!        SELECTION AND TIMING

!        call timer(time2)
!        call date_and_time(values=time2)

         IF(nprj+eprj.gt.0) CALL PROJECT_SELECT(nprj,eprj)          !only select if there are projects which have been sorted

!        call timer(time)
!        WRITE(OGBUG1,*) 'end project select'
!        call date_and_time(values=time)
!        call elapsed_time(time2,time)
!        write (725,*) 'projects selected in: ',
!        *   (time-time2)/100.0

         IF(opt_dbg) write (210,*)

!        write (725,*)
      end if

!     END YEAR LOOP
!     end do  ! ITIMEYR LOOP
        
      END SUBROUTINE

!***************************************************************
!     from PRJ_SELECT.for
!     Last change:  MC   19 May 2009    5:44 pm

      SUBROUTINE PROJECT_SELECT(nprj,eprj)

!     NOTES: THIS SUBROUTINE IS BROKEN INTO TWO SECTIONS:
!        1. RESERVES GROWTH (EOR PROJECTS)
!       2. EXPLORATION (CONVENTIONAL AND UNCONVENTIONAL)
!
!     DEVELOPMENT ORDER IS RESERVES GROWTH FOLLOWED BY EXPLORATION.
!     UNCONVENTIONAL UNDISCOVERED OIL PROJECTS ARE SUBJECT TO WATERFLOOD.  THIS
!     WILL BE TIMED IN THE FIRST YEAR THE PROJECT IS ELIGIBLE FOR WATERFLOOD AND
!     THE WATERFLOOD PROJECT IS ECONOMIC.  IT IS ASSUMED THAT THE WATERFLOOD WILL
!     BE DONE REGARDLESS OF AVAILABILITY OF CONSTRAINTS, THIS IS WHY THE PROJECT
!     IS NOT CHECKED FOR CONSTRAINT AVAILABILITY PRIOR TO TIMING.
!

      implicit none

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include 'ogsmbfw'
      include 'ogsmugr'
      include 'ogsml48'
      include 'ngtdmrep'
      include 'pmmout'


      INTEGER jtimeyr
      INTEGER nprj                                                              !number of EOR projects which are considered this year
      INTEGER eprj                                                              !number of exploration projects considered this year
      INTEGER ires                                                              !number of reservoir evaluated
      INTEGER jres                                                              !matched reservoir number
      INTEGER kres                                                              !corresponding waterflood project (conventional exploration)
      INTEGER lres                                                              !corresponding decline curve project
      INTEGER expdisc                                                           !0 -> exploration possible, 1 -> exploration prohibited
      INTEGER itech
      INTEGER cres
      INTEGER creg
      INTEGER JSRC                                                              !MATCHED RESERVOIR CO2 SOURCE MC CHANGE 5.6.09
      INTEGER JSRCR                                                             !MATCHED RESERVOIR CO2 SOURCE MC CHANGE 5.6.09

      INTEGER time,time1,eor_chk,ir,iy                                          !mc change ver 7
      integer im
      real rtemp, rtemp2                                                        !mc change ver 7

      CHARACTER*9, rid,rid2
      CHARACTER*11 SV_ARESID

      LOGICAL drill                                                             !indicates whether the project has available constraints
      LOGICAL water

!     logical frac

      expdisc = 0

!     call timer(time)

!     selection loop for EOR projects

      do ires = 1,nprj                                                          !start EOR project selection loop

         drill = .true.
         jres = sortires2(ires)
         JSRC = SORTISRC2(IRES)                                                      !MC CHANGE 5.6.09
         JSRCR = SORTIREG2(ires)

         if (ogrunop(1).lt.5) then                                          ! set prices -- use NEMS prices not the prices in the input file
            ir = aregion(jres)
            do m=1,max_yr
               oilpricec(m)=dcrdwhp(ir,curiyr)*dladj
               gaspricec(m)=ogwprng(ir,curiyr)*dladj
               oilpriceo(m)=ogprcref(apadd(jres),acrdtype(jres),1)*dladj
               gaspriceo(m)=ogprcl48(ir,3,1)*dladj
            enddo
         endif

         if (eligible(jres)) then                                                !check if project is eligible for development

            call scenario_screen(jres)                                            !set the technology variables

            IF (APCODE(Jres).EQ.3) THEN                                            !MC CHANGE 5.6.09 - RELOAD THE ARRAYS
               CALL ECONOMICS(JRES,itimeyr,0,JSRC,0,JSRCR)                                     !MC CHANGE 5.6.09
               ECO2CODE(JRES) = JSRC                                                 !MC CHANGE 5.6.09
               ECO2REG(JRES) = JSRCR                                                !MC CHANGE 5.6.09

!              WRITE (903,1903) ARESID(Jres),JSRC,RANKING_VAL(JRES),   &              !MC CHANGE 5.6.09
!                 enyrsi(JRES)+1,(EPRODOIL(JRES,I),I=1,MAX_YR-1)                      !MC CHANGE 5.6.09
! 1903         FORMAT (A11,3X,I2,3X,F16.3,3X,I2,3X,<MAX_YR-1>(F12.2,3X))              !MC CHANGE 5.6.09

!              WRITE(6,3194) CURIRUN, CURIYR+1989, CURITR, JRES, AREGION(JRES), ITIMEYR, JSRCR, JSRC, IRES, &
!                 ARESID(Jres),ARESID(Jres),  ARESID(Jres), RANKING_VAL(JRES),  enyrsi(JRES)+1, ires, jres, jres, jres, ir
!3194          FORMAT(1X,"SELECT_PROJECTS_1",9(":",I6),3(":",A11),":",F21.6,":",I4,5(":",I6))

            END IF                                                                  !MC CHANGE 5.6.09

!           WRITE(OGBUG1,*) ARESID(Jres),JSRC, ' SELECTION', RANKING_VAL(JRES), ECO2CODE(JRES)

            call check_constraints(jres,drill)                                    !check if the project can be developed
          
            call project_preferences(jres,nprj,drill)                             !check the technology/timing preferences

            if (drill) then                                                       !constraints available

                  timed(jres) = .true.
                  eligible(jres) = .false.
                  timedyr(jres) = itimeyr
                  call remove_constraints(jres)                                  !adjust constraints
                  call remove_process(jres)                                          !remove competing projects for the reservoir
                  call write_output(jres)                                        !write the details of the TIMED project
                  CALL AGG_OGSM(jres,4)                                           !aggregate the production
                  IF(opt_dbg) &
                     write (210,1210) aresid(jres),first_dec(jres), &
                     last_Dec(jres),itimeyr

!                 WRITE(6,3198) CURIRUN, CURIYR+1989, CURITR, JRES, AREGION(JRES), ITIMEYR, JSRCR, JSRC, IRES, &
!                    ARESID(Jres), RANKING_VAL(JRES),  enyrsi(JRES)+1
!3198             FORMAT(1X,"SELECT_PROJECTS_5",9(":",I6),":",A11,":",F21.6,":",I4)

            end if                                                               !end constraints check
         END if                                                                  !end eligibility check
      end do                                                                    !end EOR project selection loop


      WRITE(OGBUG1,*) NRES, ' BEFORE EXPLORATION', eprj

!     selection loop for Exploration projects

      do ires = 1,eprj                                                          !start exploration project selection loop
!        jres = explsortires2(ires)                                                 !MC CHANGE 5.19.09

         JRES = UND_COUNTER(EXPLSORTIRES2(IRES))                                     !MC CHANGE 5.19.09

         if (ogrunop(1).lt.5) then                                          ! set prices -- use NEMS prices not the prices in the input file
            ir = aregion(jres)
            do m=1,max_yr
               oilpricec(m)=dcrdwhp(ir,curiyr)*dladj
               gaspricec(m)=ogwprng(ir,curiyr)*dladj
               oilpriceo(m)=ogprcref(apadd(jres),acrdtype(jres),1)*dladj
               gaspriceo(m)=ogprcl48(ir,3,1)*dladj
            enddo
         endif

!        write(ogbug1,*) 'exploration:', itimeyr, aplay_cde(jres)
!        WRITE(OGBUG1,*) JRES,ARESID(Jres),PROJECT_NPV(JRES),ANUMACC(Jres), &
!           apcode(jres),eligible(jres)

         IF (apcode(jres).eq.10.or.apcode(jres).ge.16) THEN                        !mc change 5.19.09
            if (itimeyr.gt.1.and.project_npv(jres).gt.0.0) then                    !only consider the economically viable projects
               drill = .true.
               if (eligible(jres)) then                                                !check if the project is eligible for discovery
                  call scenario_screen(jres)                                            !set the technology variables
                  call check_constraints(jres,drill)                                    !check if the project can be developed
                  call project_preferences(jres,eprj,drill)                             !check the technology/timing preferences
           
                  if (drill) then                                                       !constraints available

!                    REDUCE THE NUMBER OF ACCUMULATIONS                                     !MC CHANGE 5.19.09

                     ANUMACC(Jres) = ANUMACC(Jres) - 1                                 !MC CHANGE 5.19.09
                     timedyr(jres) = itimeyr                                         !mc change ver 4
                     IF (ANUMACC(Jres).LE.0.0) ANUMACC(Jres) = 0.0                      !MC CHANGE 5.19.09
                     IF (ANUMACC(Jres).EQ.0.0) THEN                                       !MC CHANGE 5.19.09
                        timed(jres) = .true.                                                !MC CHANGE 5.19.09
                        eligible(jres) = .false.                                            !MC CHANGE 5.19.09
                        timedyr(jres) = itimeyr                                             !MC CHANGE 5.19.09
                     END IF                                                                !MC CHANGE 5.19.09

                     IF(opt_dbg) &
                        write (210,1210) aresid(jres),first_dec(jres), &
                        last_Dec(jres),itimeyr
                     call remove_constraints(jres)                                  !adjust the constraints
                     call write_output(jres)                                        !write the details of the TIMED project
                     CALL AGG_OGSM(jres,5)                                           !aggregate the production
                     if (apcode(jres).eq.10) then
                        water = .FALSE.                                                  !start waterflood of conventional exploration

!                       WRITE(881,1881)  aresid(jres),jres,itimeyr, aecon_life(jres),'exptimed'

!                       do jtimeyr = itimeyr+5,aecon_life(jres)+5                        !waterflood window - used to be based on window
!                       do jtimeyr = aecon_life(jres), aecon_life(jres)+10                                            !3.30.09 v3


                        if (itimeyr+aecon_life(jres)-4.ge.1) then
                           do jtimeyr = itimeyr+aecon_life(jres)-4, &
                              itimeyr+aecon_life(jres)+6

!                             WRITE(OGBUG1,*) itimeyr,jtimeyr
!                                3.27.09 v4 - changed window from aecon_life(jres),aecon_life(jres)+5

                              if (.not.water) then
                                 NEW_NRES = MAX_RES-1                                             !MC CHANGE 5.5.09

!                                new_nres = nres+1                                                !MC CHANGE 5.5.09
!                                nres = new_nres                                                  !MC CHANGE 5.5.09

                                 IF (jtimeyr.le.max_yr) then                                   !don't start waterflood projects beyond max year
                                    call waterflood(new_nres,jres,jtimeyr)                     !calculate the waterflood profile for year jtimeyr
                                    if (aresid(new_nres).eq."") return
                                    call scenario_screen(new_nres)
                                    call economics(new_nres,jtimeyr,1,0,0,aregion(new_nres))                     !calculate economic
                                    if (project_npv(new_nres).gt.0.0) then                     !if the waterflood project is economic, time it
                                       water = .true.

!                                      timed(new_nres) = .true.                                     !MC CHANGE 5.5.09
!                                      eligible(new_nres) = .false.                                 !MC CHANGE 5.5.09

                                       timedyr(new_nres) = jtimeyr                                  !MC CHANGE 5.5.09      !mc change ver 4

                                       rtemp = 0.0                                              !mc change ver 7
                                       rtemp2 = 0.0                                             !mc change ver 7
                                       do iy = 1,max_yr-1                                       !mc change ver 7
                                          rtemp = rtemp + (aprodoil(new_nres,iy,1)/1000.0)       !mc change ver 7
                                          rtemp2 = rtemp2 + (aprodgas(new_nres,iy,1)/1000.0)     !mc change ver 7
                                       end do                                                   !mc change ver 7

                                       ucnt = ucnt + 1                                          !mc change ver 7
                                       udepth(ucnt)  = adepth(new_nres)                         !mc change ver 7
                                       uname(ucnt)   = aresid(new_nres)                         !mc change ver 7
                                       ureg(ucnt)    = aregion(new_nres)                        !mc change ver 7
                                       uvalue(ucnt)  = rtemp * atotpat(new_nres,1)              !mc change ver 7
                                       uvalue2(ucnt) = rtemp2 * atotpat(new_nres,1)             !mc change ver 7

                                       IF(opt_dbg) &
                                          write (210,1210) aresid(new_nres), &
                                          first_dec(new_nres),last_Dec(new_nres),jtimeyr

                                       call write_output(new_nres,jtimeyr)                      !write the details of the TIMED waterflood project

!                                      CALL AGG_OGSM(new_nres,jtimeyr,6)                     !aggregate the production      !mc change ver 4

                                       call agg_ogsm(new_nres,6)                                                            !mc change ver 4

!                                      ccc   call remove_constraints(new_nres,jtimeyr)              !adjust the constraints
!                                      WRITE(881,1881) aresid(new_nres),new_nres,jtimeyr, &
!                                         aecon_life(jres),'wtrtimed'
 1881                                  format (a11,3x,i6,3x,i5,3x,i5,3x,a10)
!                                      else                                                           !MC CHANGE 5.5.09
!                                      nres = nres-1                                                !MC CHANGE 5.5.09

                                    end if                                                     !end waterflood economic check
                                 END if                                                       !end year check
                              end if
                           end do
                        else

                           WRITE (ogbug1,*)  'WARNING: SHORT ECON_LIFE', curiyr, aplay_cde(jres),aresid(jres),aecon_life(jres)

                        end if
                     end if
                  end if
               end if                                                                  !end eligibility check
            end if                                                                   !end economic viability check
         END if                                                          !              mc change 5.19.09
      end do                                                                    !end exploration project selection loop

      WRITE(OGBUG1,*) NRES, '  AFTER EXPLORATION'
 1210 format (a11,2x,3(i11,2x))

      END SUBROUTINE

!     *************************************************************************************
!     *************************************************************************************
!     *************************************************************************************
!     *************************************************************************************

      subroutine check_constraints(irs,drill)

      implicit none

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include 'ogsmbfw'
      include 'ogsmugr'
      include 'ogsml48'
      include 'ctus'

      INTEGER ii,ijk,id,id2,it
      INTEGER irs                                         !project number
      INTEGER itech
      INTEGER pcode                                       !process code
      INTEGER ccode                                       !CO2 source code
      INTEGER rcode
      INTEGER IR                                        !region number
      INTEGER jreg
      INTEGER dcode
      INTEGER idrill                                                            !failure code
      INTEGER OGSM_REG                                  ! region resevoir is in

      real DEPTH_PRJ(max_tech)                                !reservoir depth
      REAL cap_req
      REAL gas_req
      REAL SUC_RATEKd(max_tech)                             !SUCCESS RATE - known development 4.15.09
      REAL suc_rateue(max_tech)                             !success rate - undiscovered exploration 4.15.09
      REAL suc_Rateud(max_tech)                             !success rate - undiscovered development 4.16.09
      REAL footreq(max_yr)
      REAL drl_avail(max_yr)
      REAL TWPPBAS
      REAL TWPPADV
      REAL DMDADJ

      REAL cap_temp(max_yr)                               !temporary variable - for debuging the capital constraint
      REAL drl_temp(max_yr)                               !temporary variable - for debuging the drilling constraint
      REAL co2_temp(max_yr)                               !temporary variable - for debuging the CO2 constraint
      REAL co2vol_max

      LOGICAL drill                                       !true -> constraints available, false -> constraints unavailable
      LOGICAL oil_prj                                     !true -> oil project
      LOGICAL gas_prj                                     !true -> gas project
      LOGICAL exp_prj                                     !true -> exploration project
      LOGICAL con_prj                                     !true -> conventional project (applied to exploration only)
      LOGICAL hor_prj                                     !true -> horizontal drilling project

!     Initialize and set variables

      oil_prj = .false.
      gas_prj = .false.
      exp_prj = .false.
      con_prj = .false.
      hor_prj = .false.

      pcode = 0
      ccode = 0
      IR = 0
      dcode = 0
      TWPPBAS = 0.0    ! TOTAL WELLS TO DRILL BASE
      TWPPADV = 0.0    ! TOTAL WELLS TO DRILL ADVANCED
      idrill = 0

      OGSM_REG = AREGION(irs)


      do i = 1,max_tech
         DEPTH_PRJ(i)    = 0.0
         SUC_RATEkd(i) = 0.0  !4.15.09
         suc_Rateue(i) = 0.0  !4.15.09
         suc_rateud(i) = 0.0  !4.16.09
      end do

      do ii = 1,max_yr
         footreq(ii) = 0.0
         drl_avail(ii) = 0.0
         cap_temp(ii) = 0.0
         drl_temp(ii) = 0.0
         co2_temp(ii) = 0.0
      end do

      read (aresid(irs)(10:11),'(i2)') pcode
      ccode = eco2code(irs)
      IR = aregion(irs)

      do i = 1,max_tech
         DEPTH_PRJ(i) = adepth(irs)
      end do

      if (pcode.le.10.or.pcode.eq.17) oil_prj = .true.
      if (pcode.ge.11.and.pcode.ne.17) gas_prj = .true.
      if (pcode.eq.10.or.(pcode.ge.16.and.pcode.le.20)) exp_prj = .true.
      if (pcode.eq.10.or.pcode.eq.16) con_prj = .true.
      if (pcode.eq.8.or.pcode.eq.9)   hor_prj = .true.
!     if (pcode.eq.8.or.pcode.eq.9.or.pcode.eq.17)   hor_prj = .true.

      TWPPBAS  = ATOTPROD(irs,1) + ATOTINJ(irs,1)
      TWPPADV  = ATOTPROD(irs,2) + ATOTINJ(irs,2)


      do itech = 1,max_tech                                                          !4.16.09
         IF(hor_prj) then                                                             !4.16.09
            suc_ratekd(itech) = (SUCCHDEV/100.0)*(1.0 - DRILL_FAC(ITECH))               !4.16.09
         else                                                                         !4.16.09
            suc_Ratekd(itech) = (sucdeve(IR)/100.0)*(1-drill_fac(itech))              !4.16.09
         END if                                                                       !4.16.09
         suc_rateue(itech)  = (sucexp(IR)/100.0) *(1-drill_fac(itech))               & !4.16.09
                              * explr_fac(itech)                                      !4.16.09
         IF(pcode.ge.21.and.pcode.le.23) then                                         !4.16.09
            suc_rateud(itech) = aheatval(irs)*(1-drill_fac(itech))                     !4.16.09
         else                                                                         !4.16.09
            suc_rateud(itech) = (SUCEXPD(IR)/100.0)*                                   & !4.16.09
                              (1.0 - DRILL_FAC(ITECH))                                !4.16.09
         END if                                                                       !4.16.09
      end do                                                                         !4.16.09

!     WRITE(OGBUG1,*) aresid(irs),itech,suc_ratee(itech),suc_Rated(itech)


!     CHECK DRILLING CONSTRAINTS                                                    !this section revised MC 4.15.09
!     revision idea: the first well of an exploration package uses the exploration
!     dryhole, all others use a development dry hole rate


      if (drill) then !start of drilling constraint check

!        determine annual drilling requirements

         DO II=itimeyr,max_yr-1
            IF(.not.exp_prj) then
               FOOTREQ(II)=((DEPTH_PRJ(1)*(1.0+SUC_RATEkd(1))) &
                      *PATDEV(irs,II-itimeyr+1,1)* &
                      ((atotprod(irs,1)+atotinj(irs,1))))+ &
                      (DEPTH_PRJ(1)*patdev(irs,ii-itimeyr+1,1) &
                      *0.5*atotconv(irs,1))
   
               FOOTREQ(II)=FOOTREQ(II) + &
                      ((DEPTH_PRJ(2)*(1.0+SUC_RATEkd(2))) &
                      *PATDEV(irs,II-itimeyr+1,2)* &
                      ((atotprod(irs,2)+atotinj(irs,2))))+ &
                      (DEPTH_PRJ(2)*patdev(irs,ii-itimeyr+1,2) &
                      *0.5*atotconv(irs,2))
            END if
            IF(exp_prj) then
               IF(ii.eq.itimeyr) then
                  footreq(ii) = (DEPTH_PRJ(1)*(1.0+suc_rateue(1)))*                          & !tech = 1
                     (atotprod(irs,1)+atotinj(irs,1)+0.5*atotconv(irs,1)) +               & !exploration well
                     (DEPTH_PRJ(1)*(1.0+suc_rateud(1)))* &
                     (patdev(irs,ii-itimeyr+1,1)-1 &
                     *(atotprod(irs,1)+atotinj(irs,1)+0.5*atotconv(irs,1)))              !development wells

!                 WRITE(OGBUG1,*) 1.0+suc_ratee(1),DEPTH_PRJ(1),
!                 &           (atotprod(irs,1)+atotinj(irs,1)+0.5*atotconv(irs,1)),
!                 &           1.0+suc_rated(1),patdev(irs,ii-itimeyr+1,1)-1,footreq(ii)

                  IF(patdev(irs,ii-itimeyr+1,2).gt.0) then
                     footreq(ii) = footreq(ii) + (DEPTH_PRJ(2)* &
                        (1.0+suc_rateue(2)))*                          & !tech = 2
                        (atotprod(irs,2)+atotinj(irs,2)+0.5*atotconv(irs,2)) +               & !exploration well
                        (DEPTH_PRJ(2)*(1.0+suc_rateud(2)))* &
                        (patdev(irs,ii-itimeyr+1,2)-1 &
                        *(atotprod(irs,2)+atotinj(irs,2)+0.5*atotconv(irs,2)))              !development wells
                  END if

!                 WRITE(OGBUG1,*) 1.0+suc_ratee(2),DEPTH_PRJ(2),
!                    &       (atotprod(irs,2)+atotinj(irs,2)+0.5*atotconv(irs,2)),
!                    &        1.0+suc_rated(2),patdev(irs,ii-itimeyr+1,2)-1,footreq(ii)



              else
                 FOOTREQ(II)=((DEPTH_PRJ(1)*(1.0+SUC_RATEud(1))) &
                      *PATDEV(irs,II-itimeyr+1,1)* &
                      ((atotprod(irs,1)+atotinj(irs,1))))+ &
                      (DEPTH_PRJ(1)*patdev(irs,ii-itimeyr+1,1) &
                      *0.5*atotconv(irs,1))

                 FOOTREQ(II)=FOOTREQ(II) + &
                      ((DEPTH_PRJ(2)*(1.0+SUC_RATEud(2))) &
                      *PATDEV(irs,II-itimeyr+1,2)* &
                      ((atotprod(irs,2)+atotinj(irs,2))))+ &
                      (DEPTH_PRJ(2)*patdev(irs,ii-itimeyr+1,2) &
                      *0.5*atotconv(irs,2))
               END if
            END if

            FOOTREQ(II)=FOOTREQ(II) / 1000.0

!           WRITE(OGBUG1,*) ii,patdev(irs,ii-itimeyr+1,1),patdev(irs,ii-itimeyr+1,2),
!              &   footreq(ii),exp_prj
         END DO

!        end section revision MC 4.15.09

!        add laterals for horizontal wells

         IF (HOR_prj.or.pcode.ge.17) THEN
            DO II=itimeyr,max_yr-1
               FOOTREQ(II) = FOOTREQ(II) + &
                  (ALATNUM(irs,1) * ALATLEN(irs,1) *(1.0+ SUC_RATEkd(1)) * &
                  PATDEV(irs,II-itimeyr+1,1) ) / 1000.0
               FOOTREQ(II) = FOOTREQ(II) + &
                  (ALATNUM(irs,2) * ALATLEN(irs,2) *(1.0+ SUC_RATEkd(2)) * &
                  PATDEV(irs,II-itimeyr+1,2) ) / 1000.0
            END DO
         END IF

!        determine footage available for project
!
!        HORIZONTAL DRILLING
!
         IF (HOR_prj) THEN
            DO II=1,max_yr-1
               DRL_AVAIL(II) = DRL_AVAIL(II) + TOTHWCAP(II)
            END DO
         END IF

!        do check between oil and gas constraints

         if (.not.hor_prj) then !start of vertical wells
!
!           CONVENTIONAL EXPLORATION
!
            IF (pCODE.EQ.1.OR.pCODE.EQ.10.or.pcode.eq.16) THEN
               if(pcode.eq.10) then                                            !mc change ver 8
                  DO II=1,max_yr-1
                     DRL_AVAIL(II) = DRL_AVAIL(II) + EXPCDRCAP(IR,II) + &
                             NAT_EXPCDRCAP(II)
                  END DO          
               else                                                            !mc change ver 8
                  DO II=1,max_yr-1                                               !mc change ver 8
                     DRL_AVAIL(II) = DRL_AVAIL(II) + EXPCDRCAPg(IR,II) + &       !mc change ver 8
                             NAT_EXPCDRCAPg(II)                          !mc change ver 8
                  END DO                                                         !mc change ver 8
               end if                                                          !mc change ver 8
               dcode = 1
            END IF
!
!           UNCONVENTIONAL EXPLORATION
!
            IF ((pcode.eq.17.and.aresflag(irs).ne.9).or.(pcode.ge.18.and.pcode.le.20)) then
               if (SPLIT_ED.eq.'y'.or.split_ed.eq.'Y') THEN
                  if(pcode.eq.17) then                                            !mc change ver 8
                     DO II=1,max_yr-1
                        DRL_AVAIL(II) = DRL_AVAIL(II) + EXPUDRCAP(IR,II) + &
                              NAT_EXPUDRCAP(II)
                     END DO
                  else                                                            !mc change ver 8
                     DO II=1,max_yr-1                                              !mc change ver 8
                        DRL_AVAIL(II) = DRL_AVAIL(II) + EXPUDRCAPg(IR,II) + &      !mc change ver 8
                              NAT_EXPUDRCAPg(II)                         !mc change ver 8
                     END DO                                                        !mc change ver 8
                  end if                                                          !mc change ver 8
                  dcode = 1
               END IF
            END if
!
!           DEVELOPMENT
!
            if (pcode.ne.1.and.pcode.ne.10.and.pcode.ne.16) then !end of development check
               if (dcode.eq.0) then
                  DO II=1,max_yr-1
                     IF ( SPLIT_OG.eq.'Y'.or.split_og.eq.'y' ) THEN

!                       SPLIT CONSTRAINTS BETWEEN OIL AND GAS PROJECTS

                        IF ( oil_prj ) THEN
                           DRL_AVAIL(II) = DRL_AVAIL(II) + DRCAP_O(IR,II) &
                                   + NAT_DRCAP_O(II)
                        ELSE
                           DRL_AVAIL(II) = DRL_AVAIL(II) + DRCAP_G(IR,II) &
                                   + NAT_DRCAP_G(II)
                        END IF
!                       drl_avail(ii)=drl_avail(ii)+drcap_d(IR,ii)+nat_drcap_d(ii)              !mc change 6.22.09
                     ELSE

!                       DO NOT SPLIT CONSTRAINTS BETWEEN OIL AND GAS PROJECTS

                        DRL_AVAIL(II) = DRL_AVAIL(II) + DRCAP_O(IR,II) &
                             + NAT_DRCAP_O(II) + DRCAP_G(IR,II) &
                             + NAT_DRCAP_G(II)
!                       drl_avail(ii)=drl_avail(ii)+drcap_d(IR,ii)+nat_drcap_d(ii)              !mc change 6.22.09
                     END IF
                  END DO

!                 add in the unconventional footage if no split between exploration and development

                  IF ( SPLIT_ED.eq.'N'.or.split_ed.eq.'n' ) THEN
                     DO II=1,max_yr-1
                        DRL_AVAIL(II) = DRL_AVAIL(II) + EXPUDRCAP(IR,II) + &
                                NAT_EXPUDRCAP(II)                + &        !mc change ver 8
                                expudrcapg(ir,ii)+nat_expudrcapg(ii)        !mc change ver 8
                     END DO
                  END IF
               end if
            end if !end of development check
         end if !end of vertical wells
!
!        ***********************************************************************
!
!        FOOTAGE REQUIREMENTS AND AVAILABLE FOOTAGE HAVE BEEN DETEREMINED
!        COMPARE AND DETERMINE IF PROJECT CAN BE DONE.
!   
!        ***********************************************************************
!

         IF(opt_dbg) &
            write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
            itimeyr,IR,ccode,'int', &
            (drl_avail(ii),ii=1,max_yr-1),' '
         IF(opt_dbg) &
            write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
            itimeyr,IR,ccode,'use', &
            (footreq(ii),ii=1,max_yr-1),' '
   
         IF(opt_dbg) &
            write (672,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
            itimeyr,IR,ccode,'use', &
            (footreq(ii),ii=1,max_yr-1),' '
   
   
         DO II=itimeyr,max_yr-1
            IF (FOOTREQ(II).GT.DRL_AVAIL(II)) THEN
               DRILL = .FALSE.
               idrill = 2            
            ENDIF
         ENDDO
         if (itimeyr.eq.1.and.(pcode.ge.21.or.(pcode.eq.17.and.aresflag(irs).eq.9))) then 
            DRILL = .TRUE.
            idrill = 0            
         endif
         if (idrill.eq.2.and.dcode.eq.0)  &
            WRITE(BUGOUT,1031) 'WARNING: FOOTAGE CONSTRAINT REACHED ', CURIYR+BASEYR-1, aplay_cde(irs), aresid(irs), &
            footreq(ii),drl_avail(ii)
      end if  !end of drilling constraint check

 1031 FORMAT (A40,1X,I5,I8,A12,2F15.3)
!     IF(idrill.eq.2)!
      IF(idrill.eq.2.and.opt_dbg) &
         WRITE(231,1231) aresid(irs),ITIMEYR,irs,'DRILLING'

!     CHECK CO2 CONSTRAINTS - ONLY FOR CO2 EOR PROJECTS

      if (drill) then
         if (pcode.eq.3) then
            IF(opt_dbg) &
               write (603,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
               itimeyr,IR,ccode,'int', &
               (nat_availco2(IR,eco2code(irs),ii)*1000.0,ii=1,max_yr-1),' '


            DO II=itimeyr,max_yr-1
               GAS_REQ=0
               GAS_REQ= ECO2INJ(irs,II-itimeyr+1)-ECO2RCY(irs,II-itimeyr+1)
               IF (GAS_REQ.LT.0.) GAS_REQ=0.
               co2_temp(ii) = gas_req

!              if (gas_req.GT.nat_availco2(IR,eco2code(irs),ii)*1000.0) &

               if (gas_req.GT.nat_availco2(eco2reg(irs),eco2code(irs),ii)*1000.0) then
                  co2vol_max = 0.
                  do ir=1,res_num(irs,II)
                     IF (s_reg(OGSM_REG,ir,II) .GT. 0 .AND.  s_src(OGSM_REG,ir,II) .GT.0) THEN
                        co2vol_max = co2vol_max + nat_availco2(s_reg(OGSM_REG,ir,II),s_src(OGSM_REG,ir,II),ii)
                     ELSE
                        WRITE(OGBUG1,2377) CURIRUN, CURIYR+1989, CURITR, II, irs, ir, OGSM_REG, res_num(irs,II), s_num(OGSM_REG,II),  &
                           s_reg(OGSM_REG,ir,II), s_src(OGSM_REG,ir,II), co2vol_max
 2377                   FORMAT(1X,"WELLEXP_CHECK_CO2",11(":",I5),":",F20.6)
                     END IF
                  end do
                  if (gas_req.GT.co2vol_max*1000.) then
                     drill = .false.
                     idrill = 3
                     EXIT
                  end if
               end if

            END do
            if (idrill.eq.3) WRITE(BUGOUT,*) 'WARNING: CO2 CONSTRAINT REACHED ', CURIYR+BASEYR-1,aplay_cde(irs),aresid(irs),  &
                         eco2code(irs),eco2reg(irs)
            if (idrill.eq.3) WRITE(BUGOUT,*) 'WARNING: CO2 CONSTRAINT REACHED ', ii,  &
                         gas_req,nat_availco2(eco2reg(irs),eco2code(irs),ii)*1000.,co2vol_max*1000.
            IF(opt_dbg) &
               write (603,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
               itimeyr,IR,ccode,'use', &
               (co2_temp(ii),ii=1,max_yr-1),' '

         end if
      end if

!     IF(idrill.eq.3)!

      IF(idrill.eq.3.and.opt_dbg) &
         WRITE(231,1231) aresid(irs),ITIMEYR,irs,'CO2 CONST'

!     CHECK RIG DEPTH RATING - AVAILABILITY

      IF (USE_RDR.eq.'Y'.or.use_rdr.eq.'y') THEN !start of rig depth rating check
         if (drill.and.itimeyr.gt.1) then
            rcode = 0
            DO II=1,MAX_RDR
               IF (DEPTH_PRJ(1).LE.RDR(1,0,II)) THEN
                  if (rcode.eq.0) then
                     ID = II
                     rcode = 1
                  end if
               END IF
            END DO
            IF(opt_dbg) &
               write (605,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
               itimeyr,IR,rcode,'int', &
               (rdr_ft(id,ii),ii=1,max_yr-1),' '

            DO II=itimeyr,max_yr-1
               IF (( RDR_FT(ID,II)+FOOTREQ(II) ) .GT. RDR_FOOTAGE(ID,II)) THEN
                  DRILL = .FALSE.
                  idrill = 4
               ENDIF
            ENDDO
            IF (idrill.eq.4) WRITE(BUGOUT,*) 'WARNING: RIG DEPTH CONSTRAINT REACHED ', CURIYR+BASEYR-1, aplay_cde(irs), aresid(irs)
            IF(opt_dbg) &
               write (605,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
               itimeyr,IR,rcode,'use', &
               (footreq(ii),ii=1,max_yr-1),' '
         end if
      end if !end of rig depth rating check

!     IF(idrill.eq.4)!

      IF(idrill.eq.4.and.opt_dbg) &
         WRITE(231,1231) aresid(irs),ITIMEYR,irs,'RIG DEPTH'

!     CHECK CAPITAL CONSTRAINTS

      if (drill) then
         IF(opt_dbg) &
            write (601,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
            itimeyr,IR,ccode,'int', &
            (invcap(ii),ii=1,max_yr-1),' '

         DO II=itimeyr,max_yr-1
            CAP_REQ = 0.0
            CAP_REQ = EII(irs,II-itimeyr+1) + ETI(irs,II-itimeyr+1)
            IF (pcode.eq.8.or.pcode.eq.9.or.pcode.eq.10.or.pcode.eq.1.or.pcode.eq.16)      & !Lease acquisition applied to horizontal and primary
                CAP_REQ = CAP_REQ + ELA(irs,II-itimeyr+1)
            CAP_REQ = CAP_REQ / 1000.0
            cap_temp(ii) = cap_req
            IF (CAP_REQ.GT.INVCAP(II)) THEN
               DRILL = .FALSE.
               idrill = 1
            END IF
         END DO
         IF (idrill.eq.1) WRITE(BUGOUT,*) 'WARNING: CAPITAL CONSTRAINT REACHED ', CURIYR+BASEYR-1, aplay_cde(irs), aresid(irs)
         IF(opt_dbg) &
            write (601,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
            itimeyr,IR,ccode,'use', &
            (cap_temp(ii),ii=1,max_yr-1),' '
 1601    format (a11,4(2x,l1),2x,i4,2x,i2,2x,i2,1x,a3,1x, &
            <max_yr-1>(2x,f10.2),2x,a1)
      end if

      IF(idrill.eq.1.and.opt_dbg) &
         WRITE(231,1231) aresid(irs),ITIMEYR,irs,'CAPITAL'
 1231 FORMAT (A11,3X,I2,3x,i5,3X,A12)

!     CHECK GAS DEMAND CONSTRAINT - ONLY FOR GAS PROJECTS

      if (drill.and.itimeyr.GT.3) then  ! DO NOT APPLY IN HISTORICAL OR STEO YEARS

         if (pcode.ge.16.and.pcode.ne.17) then                   ! NOT APPLIED TO DECLINE 
            do ii = itimeyr,itimeyr 
               if (eprodgas(irs,ii)/1000.0.gt.nat_dmdgas(7,ii)) then      ! SINCE PRODUCTION CAN MOVE ACROSS REGIONS,
                  drill = .false.                                             ! COMPARE TO LOWER 48 TOTAL
                  idrill = 5
               end if
            end do
         end if
      end if
      IF (idrill.eq.5) WRITE(BUGOUT,*) 'WARNING: GAS DEMAND CONSTRAINT REACHED ', CURIYR+BASEYR-1, aplay_cde(irs), aresid(irs)

!     IF(idrill.eq.5)!

      IF(idrill.eq.5.and.opt_dbg) &
         WRITE(231,1231) aresid(irs),ITIMEYR,irs,'GASDEMAND'

      return
      end subroutine

!     *************************************************************************************
!     *************************************************************************************
!     *************************************************************************************
!     *************************************************************************************

      subroutine remove_constraints(irs)

      implicit none

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include 'ogsmbfw'
      include 'ogsmugr'
      include 'ogsml48'
      include 'ctus'

      INTEGER irs,itech,iii, I_REG, I_SRC, T_YR, EOR_RG, OGSM_REG
      INTEGER pcode,ccode,IR,ii,id,rcode,id2,dcode

      real DEPTH_PRJ(max_tech)                                !reservoir depth
      REAL cap_req(max_yr)
      REAL tempdry(max_yr)
      REAL gas_req(max_yr)
      REAL SUC_RATEeu(max_tech)                             !SUCCESS RATE - undiscovered exploration    4.15.09
      REAL suc_rateed(max_tech)                             !success rate - undiscovered development    4.16.09
      REAL suc_ratekd(max_tech)                             !success rate - discovered development      4.15.09
      REAL footreq(max_yr)
      REAL drl_avail(max_yr)
      REAL DELTA(MAX_YR)
      REAL TWPPBAS
      REAL TWPPADV
      REAL co2_temp(max_yr)                               !temporary variable - for debuging the CO2 constraint
      REAL*8 src_shr, shrtot

      LOGICAL drill                                       !true -> constraints available, false -> constraints unavailable
      LOGICAL oil_prj                                     !true -> oil project
      LOGICAL gas_prj                                     !true -> gas project
      LOGICAL exp_prj                                     !true -> exploration project
      LOGICAL con_prj                                     !true -> conventional project (applied to exploration only)
      LOGICAL hor_prj                                     !true -> horizontal drilling project

!     Initialize and set variables

      oil_prj = .false.
      gas_prj = .false.
      exp_prj = .false.
      con_prj = .false.
      hor_prj = .false.

      pcode = 0
      ccode = 0
      IR = 0
      TWPPBAS = 0.0    ! TOTAL WELLS TO DRILL BASE
      TWPPADV = 0.0    ! TOTAL WELLS TO DRILL ADVANCE
      dcode = 0

      OGSM_REG = AREGION(irs)

      do i = 1,max_tech
         DEPTH_PRJ(i) = 0.0
         suc_rateeu(i) = 0.0   !4.15.09
         suc_rateed(i) = 0.0   !4.16.09
         suc_ratekd(i) = 0.0   !4.15.09
      end do

      do ii = 1,max_yr
         footreq(ii) = 0.0
         drl_avail(ii) = 0.0
         cap_req(ii) = 0.0
         gas_req(ii) = 0.0
         sum_dry(irs,ii) = 0.0
      end do

      read (aresid(irs)(10:11),'(i2)') pcode
      ccode = eco2code(irs)
      IR = aregion(irs)
      EOR_RG = IR

      do i = 1,max_tech
         DEPTH_PRJ(i) = adepth(irs)
      end do

      if (pcode.le.10.or.pcode.eq.17) oil_prj = .true.
      if (pcode.ge.11.and.pcode.ne.17) gas_prj = .true.
      if (pcode.eq.10.or.(pcode.ge.16.and.pcode.le.20)) exp_prj = .true.
      if (pcode.eq.10.or.pcode.eq.16) con_prj = .true.
      if (pcode.eq.8.or.pcode.eq.9)   hor_prj = .true.
!     if (pcode.eq.8.or.pcode.eq.9.or.pcode.eq.17)   hor_prj = .true.

      TWPPBAS  = ATOTPROD(irs,1) + ATOTINJ(irs,1)
      TWPPADV  = ATOTPROD(irs,2) + ATOTINJ(irs,2)

!     calculate the base and advanced case success rates - for drilling purposes
!     do itech = 1,max_tech                                                                      !4.16.09
!     if (oil_prj) then                                                                          !4.16.09
!     if (exp_prj) then                         !exploration oil                              !4.16.09
!     SUC_RATEe(itech) = (sucexp(IR)/100.0) * (1-drill_fac(itech))*explr_fac(itech)        !4.16.09
!     else                                                                                    !4.16.09
!     if (hor_prj) then                      !horizontal oil (EOR)                         !4.16.09
!     SUC_RATEd(itech) = (1-h_success/100.0) * (1-drill_fac(itech))                     !4.16.09
!     else                                   !vertical oil (EOR)                           !4.16.09
!     SUC_RATEd(itech) = (1-sucdevo(IR)/100.0) * (1-drill_fac(itech))                   !4.16.09
!     end if                                                                               !4.16.09
!     end if                                                                                  !4.16.09
!     else                                                                                       !4.16.09
!     if (exp_prj) then                         !exploration gas                              !4.16.09
!     SUC_RATEe(itech) = (sucexp(IR)/100.0) * (1-drill_fac(itech))*explr_fac(itech)        !4.16.09
!     else                                      !development gas                 !4.15.09     !4.16.09
!     SUC_RATEd(itech) = (1-sucdevg(IR)/100.0) * (1-drill_fac(itech))                      !4.16.09
!     end if                                                                                  !4.16.09
!     end if                                                                                     !4.16.09
!     suc_rate(itech) = 1-suc_rate(itech)                                                        !4.16.09
!     IF(pcode.ge.21.and.pcode.le.23) suc_rate(itech) = 1.0    !10.27.08 - dryhole incorporated into profile at time of discovery
!     end do                                                                                     !4.16.09

      do itech = 1,max_tech                                                          !4.16.09
         IF (hor_prj) then                                                                  !4.16.09
            suc_ratekd(itech) = (SUCCHDEV/100.0)*(1.0 - DRILL_FAC(ITECH))               !4.16.09
         else                                                                         !4.16.09
            suc_Ratekd(itech) = (sucdeve(IR)/100.0)*(1-drill_fac(itech))              !4.16.09
         END if                                                                       !4.16.09
         suc_rateeu(itech)  = (sucexp(IR)/100.0) *(1-drill_fac(itech)) * explr_fac(itech)   !4.16.09
         IF (pcode.ge.21.and.pcode.le.23) then                                              !4.16.09
            suc_rateed(itech) = aheatval(irs)*(1-drill_fac(itech))                     !4.16.09
         else                                                                         !4.16.09
            suc_rateed(itech) = (SUCEXPD(IR)/100.0)* (1.0 - DRILL_FAC(ITECH))               !4.16.09
         END if                                                                       !4.16.09
      end do                                                                         !4.16.09

!     SUBTRACT CAPITAL CONSTRAINTS

      IF (opt_dbg) &
         write (601,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
         itimeyr,IR,ccode,'bgn', &
         (invcap(ii),ii=1,max_yr-1),' '
 1601 format (a11,4(2x,l1),2x,i4,2x,i2,2x,i2,1x,a3,1x, &
         <max_yr-1>(2x,f10.2),2x,a1)

      DO II=itimeyr,max_yr-1
         CAP_REQ(ii) = 0.0
         CAP_REQ(ii) = EII(irs,II-itimeyr+1) + ETI(irs,II-itimeyr+1)
         IF (pcode.eq.8.or.pcode.eq.9.or.pcode.eq.10.or.pcode.eq.1.or.pcode.eq.16)      & !Lease acquisition applied to horizontal and primary
            CAP_REQ(ii) = CAP_REQ(ii) + ELA(irs,II-itimeyr+1)
         CAP_REQ(ii) = CAP_REQ(ii) / 1000.0
         invcap(ii) = invcap(ii)-cap_req(ii)
         usecap(ii) = usecap(ii)+cap_req(ii)                            !store the total capital used in development
      END DO

      IF (opt_dbg) then
         write (601,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
            itimeyr,IR,ccode,'req', &
            (cap_req(ii),ii=1,max_yr-1),' '

         write (601,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
            itimeyr,IR,ccode,'end', &
            (invcap(ii),ii=1,max_yr-1),' '
         write (601,*)
      END if

!     SUBTRACT DRILLING CONSTRAINTS
!     determine annual drilling and dryhole requirements

      DO II=itimeyr,max_yr-1
         IF (.not.exp_prj) then                                             !4.15.09
            FOOTREQ(II)=((DEPTH_PRJ(1)*(1.0+SUC_RATEkd(1))) &
                      *PATDEV(irs,II-itimeyr+1,1)* &
                      (atotprod(irs,1)+atotinj(irs,1)))+ &
                      (DEPTH_PRJ(1)*patdev(irs,ii-itimeyr+1,1)* &
                      0.5*atotconv(irs,1))
            FOOTREQ(II)=FOOTREQ(II) + &
                      ((DEPTH_PRJ(2)*(1.0+SUC_RATEkd(2)))                        & !4.15.09
                      *PATDEV(irs,II-itimeyr+1,2)* &
                      (atotprod(irs,2)+atotinj(irs,2)))+ &
                      (DEPTH_PRJ(2)*patdev(irs,ii-itimeyr+1,2)* &
                      0.5*atotconv(irs,2))
         END if                                                           !4.15.09
                                                                          !4.15.09
         IF (exp_prj) then                                                 !4.15.09
            IF (ii.eq.itimeyr) then                                         !4.15.09
               footreq(ii) = (DEPTH_PRJ(1)*(1.0+suc_rateeu(1)))*                   & !4.16.09       !tech = 1
                  (atotprod(irs,1)+atotinj(irs,1)+0.5*atotconv(irs,1)) +         & !4.15.09      !exploration well
                  (DEPTH_PRJ(1)*(1.0+suc_rateed(1)))* &
                  (patdev(irs,ii-itimeyr+1,1)-1    & !4.16.09
                  *(atotprod(irs,1)+atotinj(irs,1)+0.5*atotconv(irs,1)))        !4.15.09      !development wells

!              WRITE(OGBUG1,*) 1.0+suc_ratee(1),DEPTH_PRJ(1),                           !4.15.09
!              &       (atotprod(irs,1)+atotinj(irs,1)+0.5*atotconv(irs,1)),       !4.15.09
!              &        1.0+suc_rated(1),patdev(irs,ii-itimeyr+1,1)-1,footreq(ii)  !4.15.09

               IF (patdev(irs,ii-itimeyr+1,2).gt.0) then                      !4.15.09
                  footreq(ii) = footreq(ii) + (DEPTH_PRJ(2)* &
                     (1.0+suc_rateeu(2)))*    & !4.16.09                     !tech = 2
                     (atotprod(irs,2)+atotinj(irs,2)+0.5*atotconv(irs,2)) +         & !4.15.09      !exploration well
                     (DEPTH_PRJ(2)*(1.0+suc_rateed(2)))* &
                     (patdev(irs,ii-itimeyr+1,2)-1   & !4.16.09
                     *(atotprod(irs,2)+atotinj(irs,2)+0.5*atotconv(irs,2)))        !4.15.09      !development wells
               END if                                                        !4.15.09

!              WRITE(OGBUG1,*) 1.0+suc_ratee(2),DEPTH_PRJ(2),                           !4.15.09
!              &        (atotprod(irs,2)+atotinj(irs,2)+0.5*atotconv(irs,2)),       !4.15.09
!              &        1.0+suc_rated(2),patdev(irs,ii-itimeyr+1,2)-1,footreq(ii)  !4.15.09
            else                                                           !4.15.09
               FOOTREQ(II)=((DEPTH_PRJ(1)*(1.0+SUC_RATEed(1)))                    & !4.16.09
                      *PATDEV(irs,II-itimeyr+1,1)*                         & !4.15.09
                      ((atotprod(irs,1)+atotinj(irs,1))))+                 & !4.15.09
                      (DEPTH_PRJ(1)*patdev(irs,ii-itimeyr+1,1)                 & !4.15.09
                      *0.5*atotconv(irs,1))                               !4.15.09

               FOOTREQ(II)=FOOTREQ(II) +                                      & !4.15.09
                      ((DEPTH_PRJ(2)*(1.0+SUC_RATEed(2)))                      & !4.16.09
                      *PATDEV(irs,II-itimeyr+1,2)*                         & !4.15.09
                      ((atotprod(irs,2)+atotinj(irs,2))))+                 & !4.15.09
                      (DEPTH_PRJ(2)*patdev(irs,ii-itimeyr+1,2)                 & !4.15.09
                      *0.5*atotconv(irs,2))                               !4.15.09
            END if                                                         !4.15.09
         END if                                                           !4.15.09

         FOOTREQ(II)=FOOTREQ(II) / 1000.0

!        SUM_DRY(irs,II) =                                              !4.15.09
!        &                ((PATDEV(irs,II-itimeyr+1,1)*(1.0+suc_rate(1))) +  !4.15.09
!        &                (PATDEV(irs,II-itimeyr+1,2) *(1.0+ SUC_RATE(2)))-  !4.15.09
!        &                (PATDEV(irs,II-itimeyr+1,1) +                      !4.15.09
!        &                PATDEV(irs,II-itimeyr+1,2)))                       !4.15.09

!        WRITE(OGBUG1,*) aresid(irs),patdev(irs,ii-itimeyr+1,1),DEPTH_PRJ(1),
!        &    atotprod(irs,1),atotinj(irs,1),atotconv(irs,1),footreq(ii)

      END DO

!     remove first well from exploration package (treat as exploratory well)

!     IF ((pcode.EQ.10.OR.pcode.EQ.16).AND.(ATOTPAT(irs,1).GT.1.0)) THEN        !4.15.09
!     FOOTREQ(itimeyr)=FOOTREQ(itimeyr) -                                   !4.15.09
!     &                   ((DEPTH_PRJ(1)*(1.0+SUC_RATE(1))) *  1.0)/1000.0           !4.15.09
!     SUM_DRY(irs,itimeyr) = SUM_DRY(irs,itimeyr)-                          !4.15.09
!     &                           ((1.0*(1.0+SUC_RATE(1)))-1.0)                  !4.15.09
!     ENDIF                                                                    !4.15.09

!     IF (pcode.ge.17.and.pcode.le.20) THEN                                    !4.15.09
!     FOOTREQ(itimeyr)=FOOTREQ(itimeyr) -                                   !4.15.09
!     &                   ((DEPTH_PRJ(1)*(1.0+SUC_RATE(1))) * 1.0)/1000.0            !4.15.09
!     SUM_DRY(irs,itimeyr) = SUM_DRY(irs,itimeyr)-                          !4.15.09
!     &                           ((1.0*(1.0+SUC_RATE(1)))-1.0)                  !4.15.09
!     ENDIF                                                                    !4.15.09

!     write(855,1855) aresid(irs),itimeyr,sum_dry(irs,itimeyr),
!     &  patdev(irs,itimeyr,1),patdev(irs,itimeyr,2),suc_Rate(1)
!1855 format(a11,3x,i2,4(3x,f10.3))

!     add laterals for horizontal wells

      IF (HOR_prj.or.pcode.ge.17) THEN
         DO II=itimeyr,max_yr-1
            FOOTREQ(II) = FOOTREQ(II) + &
               (ALATNUM(irs,1) * ALATLEN(irs,1) *(1.0+ SUC_RATEkd(1)) *          & !4.16.09
               PATDEV(irs,II-itimeyr+1,1)) / 1000.0
            FOOTREQ(II) = FOOTREQ(II) + &
               (ALATNUM(irs,2) * ALATLEN(irs,2) *(1.0+ SUC_RATEkd(2)) *          & !4.16.09
               PATDEV(irs,II-itimeyr+1,2)) / 1000.0
         END DO
      END IF

!     ***********************************************************************
!
!     STORE SUMMARY DRILLING FOOTAGE USED BY RESOURCE TYPE
!
!     ***********************************************************************
!
      IF ( OIL_prj ) THEN
         DO II=itimeyr,max_yr-1
            IF (pCODE.EQ.17) THEN
               SUM_OIL_UNCONV(IR,II) =SUM_OIL_UNCONV(IR,II) + &
                                          FOOTREQ(II)
            ELSE
               SUM_OIL_CONV(IR,II)   =SUM_OIL_CONV(IR,II) + &
                                          FOOTREQ(II)
            END IF
         END DO
      ELSE
         DO II=itimeyr,max_yr-1
            IF (pCODE.ge.16.and.pcode.ne.17.and.pcode.le.20)THEN
               SUM_GAS_UNCONV(IR,II) =SUM_GAS_UNCONV(IR,II) + &
                                          FOOTREQ(II)
            ELSE
               SUM_GAS_CONV(IR,II)   =SUM_GAS_CONV(IR,II) + &
                                          FOOTREQ(II)
            END IF
         END DO
      END IF

!     determine footage available for project

      IF (HOR_prj) THEN                                                  !horizontal drilling

         IF (opt_dbg) then
            write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
               itimeyr,IR,ccode, &
               'bgn',(tothwcap(ii),ii=1,max_yr-1),' '
            write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
               itimeyr,IR,ccode, &
               'req',(footreq(ii),ii=1,max_yr-1),' '
         END if

         DO II=ITIMEYR,max_yr-1
            Tothwcap(II) = TOTHWCAP(II) - footreq(ii)
         END DO

         IF (opt_dbg) then
            write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
               itimeyr,IR,ccode, &
               'end',(tothwcap(ii),ii=1,max_yr-1),' '
            write (602,*)
         END if
      END IF !end horizontal wells

      if (.not.hor_prj) then  !start vertical wells

!        exploration

         if (pcode.eq.1.or.pcode.eq.10) then                                         !mc change ver 8 - conventional oil

            IF (opt_dbg) then
               write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
                  itimeyr,IR,ccode, &
                  'bgn',(expcdrcap(IR,ii),ii=1,max_yr-1),' '
               write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
                  itimeyr,IR,ccode,'req', &
                  (footreq(ii),ii=1,max_yr-1),' '
            END if

            DO II=ITIMEYR,max_yr-1
               IF (FOOTREQ(II).GT.EXPCDRCAP(IR,II)) THEN
                  DELTA(II)         = FOOTREQ(II) - EXPCDRCAP(IR,II)
                  EXPCDRCAP(IR,II) = 0.0
                  NAT_EXPCDRCAP(II) = NAT_EXPCDRCAP(II) - DELTA(II)
               ELSE
                  EXPCDRCAP(IR,II) = EXPCDRCAP(IR,II) - FOOTREQ(II)
               END IF
            ENDDO

            IF (opt_dbg) then
               write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
                  itimeyr,IR,ccode, &
                  'end',(expcdrcap(IR,ii),ii=1,max_yr-1),' '
               write (602,*)
            END if

         else                                      !mc change ver 8  - conventional gas

            IF (opt_dbg) then                                                       !mc change ver 8
               write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &        !mc change ver 8
                  itimeyr,IR,ccode, &                                                 !mc change ver 8
                  'bgn',(expcdrcapg(IR,ii),ii=1,max_yr-1),' '                         !mc change ver 8
               write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &        !mc change ver 8
                  itimeyr,IR,ccode,'req', &                                           !mc change ver 8
                  (footreq(ii),ii=1,max_yr-1),' '                                     !mc change ver 8
            END if                                                                 !mc change ver 8

            DO II=ITIMEYR,max_yr-1                                              !mc change ver 8
               IF (FOOTREQ(II).GT.EXPCDRCAPg(IR,II)) THEN                        !mc change ver 8
                  DELTA(II)         = FOOTREQ(II) - EXPCDRCAPg(IR,II)            !mc change ver 8
                  EXPCDRCAPg(IR,II) = 0.0                                        !mc change ver 8
                  NAT_EXPCDRCAPg(II) = NAT_EXPCDRCAPg(II) - DELTA(II)            !mc change ver 8
               ELSE                                                              !mc change ver 8
                  EXPCDRCAPg(IR,II) = EXPCDRCAPg(IR,II) - FOOTREQ(II)            !mc change ver 8
               END IF                                                            !mc change ver 8
            ENDDO                                                               !mc change ver 8

            IF (opt_dbg) then                                                       !mc change ver 8
               write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &        !mc change ver 8
                  itimeyr,IR,ccode, &                                                 !mc change ver 8
                  'end',(expcdrcapg(IR,ii),ii=1,max_yr-1),' '                         !mc change ver 8
               write (602,*)                                                          !mc change ver 8
            END if                                                                 !mc change ver 8
         end if                                                                     !mc change ver 8

         IF (pcode.ge.17.and.pcode.le.20) then                              !unconventional exploration
            if (SPLIT_ED.eq.'y'.or.split_ed.eq.'Y') THEN
               if (pcode.eq.17) then                                                   !mc change ver 8  - unconventional oil

                  IF (opt_dbg) then
                     write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
                        itimeyr,IR,ccode, &
                        'bgn',(expudrcap(IR,ii),ii=1,max_yr-1),' '
                     write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
                        itimeyr,IR,ccode,'req', &
                        (footreq(ii),ii=1,max_yr-1),' '
                  END if

                  DO II=ITIMEYR,max_yr-1
                     IF (FOOTREQ(II).GT.EXPUDRCAP(IR,II)) THEN
                        DELTA(II)         = FOOTREQ(II) - EXPUDRCAP(IR,II)
                        EXPUDRCAP(IR,II) = 0.0
                        NAT_EXPUDRCAP(II) = NAT_EXPUDRCAP(II) - DELTA(II)
                     ELSE
                        EXPUDRCAP(IR,II) = EXPUDRCAP(IR,II) - FOOTREQ(II)
                     END IF
                  ENDDO
                  dcode = 1
 
                  IF (opt_dbg) then
                     write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
                        itimeyr,IR,ccode, &
                        'end',(expudrcap(IR,ii),ii=1,max_yr-1),' '
                     write (602,*)
                  END if

               else                                                                   !mc change ver 8 - unconventional gas

                  IF (opt_dbg) then                                                     !mc change ver 8
                     write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &      !mc change ver 8
                        itimeyr,IR,ccode, &                                               !mc change ver 8
                        'bgn',(expudrcapg(IR,ii),ii=1,max_yr-1),' '                       !mc change ver 8
                     write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &      !mc change ver 8
                        itimeyr,IR,ccode,'req', &                                         !mc change ver 8
                        (footreq(ii),ii=1,max_yr-1),' '                                   !mc change ver 8
                  END if                                                               !mc change ver 8

                  DO II=ITIMEYR,max_yr-1                                            !mc change ver 8
                     IF (FOOTREQ(II).GT.EXPUDRCAPg(IR,II)) THEN                      !mc change ver 8
                        DELTA(II)         = FOOTREQ(II) - EXPUDRCAPg(IR,II)          !mc change ver 8
                        EXPUDRCAPg(IR,II) = 0.0                                      !mc change ver 8
                        NAT_EXPUDRCAPg(II) = NAT_EXPUDRCAPg(II) - DELTA(II)          !mc change ver 8
                     ELSE                                                            !mc change ver 8
                        EXPUDRCAPg(IR,II) = EXPUDRCAPg(IR,II) - FOOTREQ(II)          !mc change ver 8
                     END IF                                                          !mc change ver 8
                  ENDDO                                                             !mc change ver 8
                  dcode = 1                                                         !mc change ver 8

                  IF (opt_dbg) then                                                     !mc change ver 8
                     write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &      !mc change ver 8
                        itimeyr,IR,ccode, &                                               !mc change ver 8
                        'end',(expudrcapg(IR,ii),ii=1,max_yr-1),' '                       !mc change ver 8
                     write (602,*)                                                        !mc change ver 8
                  END if                                                               !mc change ver 8
               end if                                                                 !mc change ver 8
            END IF
         endif

!        end exploration

!        start development

         if (pcode.ne.1.and.pcode.ne.10.and.pcode.ne.16) then               !for development projects
            if (dcode.eq.0) then
               IF ( SPLIT_OG.eq.'Y'.or.split_og.eq.'y' ) THEN                  !development

!                 SPLIT CONSTRAINTS BETWEEN OIL AND GAS PROJECTS

                  IF ( OIL_prj ) THEN

                     IF (opt_dbg) THEN
                        write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
                           itimeyr,IR,ccode, &
                           'bgn',(drcap_o(IR,ii),ii=1,max_yr-1),' '
                        write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
                           itimeyr,IR,ccode,'req', &
                           (footreq(ii),ii=1,max_yr-1),' '
                     END if

                     do ii = ITIMEYR,max_yr-1
                        IF (FOOTREQ(II).GT.DRCAP_O(IR,II)) THEN
                           DELTA(II)       = FOOTREQ(II) - DRCAP_O(IR,II)
                           IF (delta(ii).gt.nat_drcap_o(ii)) then                  !use regional dual use
                              delta(ii)  = delta(ii) - nat_drcap_o(ii)
                              IF (delta(ii).gt.drcap_d(IR,ii)) then               !use national moveable dual use footage
                                 delta(ii) = delta(ii) - drcap_d(IR,ii)
                                 drcap_d(IR,ii) = 0.0
                                 nat_drcap_d(ii) = nat_drcap_d(ii) - delta(ii)
                              else
                                 nat_drcap_o(ii) = 0.0
                                 drcap_d(IR,ii) = drcap_d(IR,ii) - delta(ii)
                              END if
                           else
                              DRCAP_O(IR,II) = 0.0
                              NAT_DRCAP_O(II) = NAT_DRCAP_O(II) - DELTA(II)
                           END if
                        ELSE
                           DRCAP_O(IR,II) = DRCAP_O(IR,II) - FOOTREQ(II)
                        END IF
                     end do

                     IF (opt_dbg) then
                        write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
                           itimeyr,IR,ccode, &
                           'end',(drcap_o(IR,ii),ii=1,max_yr-1),' '
                     END if

                  ELSE
   
                     IF (opt_dbg) then
                        write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
                           itimeyr,IR,ccode, &
                           'bgn',(drcap_g(IR,ii),ii=1,max_yr-1),' '
                        write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
                           itimeyr,IR,ccode,'req', &
                           (footreq(ii),ii=1,max_yr-1),' '
                     END if
                     do ii = ITIMEYR,max_yr-1
                        IF (FOOTREQ(II).GT.DRCAP_G(IR,II)) THEN
                           DELTA(II)       = FOOTREQ(II) - DRCAP_G(IR,II)
                           IF (delta(ii).gt.nat_drcap_g(ii)) then                  !use regional dual use
                              delta(ii)  = delta(ii) - nat_drcap_g(ii)
                              IF (delta(ii).gt.drcap_d(IR,ii)) then               !use national moveable dual use footage
                                 delta(ii) = delta(ii) - drcap_d(IR,ii)
                                 drcap_d(IR,ii) = 0.0
                                 nat_drcap_d(ii) = nat_drcap_d(ii) - delta(ii)
                              else
                                 nat_drcap_g(ii) = 0.0
                                 drcap_d(IR,ii) = drcap_d(IR,ii) - delta(ii)
                              END if
                           else
                              DRCAP_G(IR,II) = 0.0
                              NAT_DRCAP_G(II) = NAT_DRCAP_G(II) - DELTA(II)
                           END if
                        ELSE
                           DRCAP_G(IR,II) = DRCAP_G(IR,II) - FOOTREQ(II)
                        END IF
                     end do

                     IF (opt_dbg) then
                        write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
                           itimeyr,IR,ccode, &
                           'end',(drcap_g(IR,ii),ii=1,max_yr-1),' '
                     END if

                  END IF
               ELSE

!                 DO NOT SPLIT CONSTRAINTS BETWEEN OIL AND GAS PROJECTS

                  IF (opt_dbg) THEN
                     write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
                        itimeyr,IR,ccode, &
                        'bgn',(drcap_o(IR,ii),ii=1,max_yr-1),' '
                     write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
                        itimeyr,IR,ccode,'req', &
                        (footreq(ii),ii=1,max_yr-1),' '
                  END if

                  do ii = ITIMEYR,max_yr-1
                     IF (FOOTREQ(II).GT.DRCAP_O(IR,II)) THEN
                        DELTA(II)       = FOOTREQ(II) - DRCAP_O(IR,II)
                        DRCAP_O(IR,II) = 0.0
                        NAT_DRCAP_O(II) = NAT_DRCAP_O(II) - DELTA(II)
                     ELSE
                        DRCAP_O(IR,II) = DRCAP_O(IR,II) - FOOTREQ(II)
                     END IF
                  end do

                  IF (opt_dbg) then
                     write (602,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
                        itimeyr,IR,ccode, &
                        'end',(drcap_o(IR,ii),ii=1,max_yr-1),' '
                  END if
               END IF
            end if !end development loop

            IF (opt_dbg) write (602,*)

         end if
      END if !end vertical wells

!     STORE DRILLING IN APPROPRIATE RIG DEPTH CATEGORY
!     THE RIG DEPTH ONLY APPLIES TO THE VERTICAL (NOT HORIZ.) COMPONENT OF THE WELL

      IF (HOR_prj) THEN
         DO II=itimeyr,max_yr-1
            FOOTREQ(II)=0.0
            FOOTREQ(II)=(DEPTH_PRJ(1)*(1.0+SUC_RATEkd(1))) *                      & !4.16.09
                PATDEV(irs,II-itimeyr+1,1)
            FOOTREQ(II)=FOOTREQ(II)+(DEPTH_PRJ(2)*(1.0+SUC_RATEkd(1)))*        & !4.16.09
                PATDEV(irs,II-itimeyr+1,1)
            FOOTREQ(II)=FOOTREQ(II) / 1000.0
         END DO
      ENDIF

      do ii = 1,max_yr-1                                                    !4.15.09
         tempdry(ii) = 0.0                                                   !4.15.09
         IF (hor_prj) then                                                     !4.16.09
            tempdry(ii) = (patdev(irs,ii,1)+patdev(irs,ii,2))*suc_ratekd(1)      & !4.16.09
               *(atotprod(irs,1)+atotinj(irs,1))
         ELSEIF (exp_prj) then                                                 !4.16.09
            IF (ii.eq.1) then                                                    !4.15.09
               tempdry(ii) = suc_rateeu(1) + (patdev(irs,ii,1)-1)*                 & !4.16.09
                  suc_rateed(1)      & !4.15.09                                      !4.16.09
                  *(atotprod(irs,1)+atotinj(irs,1))
               IF (patdev(irs,ii,2).gt.1) then                                     !4.15.09
                  tempdry(ii) = tempdry(ii) +                                       & !4.15.09
                     suc_rateeu(2) + (patdev(irs,ii,1)-2)*suc_rateed(2)                & !4.15.09
                     *(atotprod(irs,1)+atotinj(irs,1))
               END if                                                             !4.15.09
            else                                                                !4.15.09
               tempdry(ii) =(patdev(irs,ii,1)+patdev(irs,ii,2))*suc_rateed(1)      & !4.15.09
                  *(atotprod(irs,1)+atotinj(irs,1))
            END if                                                              !4.16.09
         ELSEIF(pcode.ge.21.and.pcode.le.23) then                             !4.16.09
            tempdry(ii) = (patdev(irs,ii,1)+patdev(irs,ii,2))*suc_rateed(1)      & !4.16.09
               *(atotprod(irs,1)+atotinj(irs,1))
         else                                                                 !4.16.09
            tempdry(ii) = (patdev(irs,ii,1)+patdev(irs,ii,2))*suc_ratekd(1)      & !4.16.09
               *(atotprod(irs,1)+atotinj(irs,1))
         END if                                                               !4.16.09
      end do                                                                !4.15.09

!     write (OGBUG1,2832) ARESID(irs),ITIMEYR,IR,ADEPTH(irs), &
!     SUC_RATEeu(1),suc_rateed(1),suc_ratekd(1), &
!     (PATDEV(IRS,II,1)+PATDEV(IRS,II,2),II=1,10),                     & !PATTERNS             MC 4.10.09
!     ((PATDEV(IRS,II,1)+PATDEV(IRS,II,2))*ATOTPROD(IRS,1),II=1,10),    & !NEW PRODUCERS        MC 4.10.09
!     ((PATDEV(IRS,II,1)+PATDEV(IRS,II,2))*ATOTINJ(IRS,1),II=1,10),     & !NEW INJECTORS        MC 4.10.09
!     ((PATDEV(IRS,II,1)+PATDEV(IRS,II,2))*ATOTCONV(IRS,1),II=1,10),    & !PRD - INJ            MC 4.10.09
!     ((PATDEV(IRS,II,1)+PATDEV(IRS,II,2))*ATOTPS(IRS,1),II=1,10),      & !PRI - SEC            MC 4.10.09
!     (tempdry(ii),II=1,10),                                            & !DRY HOLES            MC 4.15.09
!     (FOOTREQ(II),II=1,10)                                            !FOOTAGE

!2832 FORMAT (A11,3X,I6,3X,I6,3X,F9.1,3X,F9.4,3x,f9.4,3x,f9.4, &
!     3X,7(10(F18.2,3X)))!,
!     &  40(F18.2,3X))                                                    !                     MC 4.10.09

!
!     ADD NEW PROJECT TO TOTAL FOOTAGE IN RDR
!
      IF (pcode.ge.3.and..not.(pcode.ge.11.and.pcode.le.15)) then   ! DO NOT INCLUDE DECLINE
         rcode = 0
         DO ID=1,MAX_RDR
            IF (DEPTH_PRJ(1).LE.RDR(1,0,ID)) THEN
               if (rcode.eq.0) then
                  id2 = id

                  IF (opt_dbg) then
                     write (605,1601) aresid(irs),oil_prj,exp_prj,con_prj, &
                        hor_prj, &
                        itimeyr,IR,rcode,'bgn', &
                        (rdr_ft(id2,ii),ii=1,max_yr-1),' '
                  END if

                  DO II=itimeyr,max_yr-1
                     RDR_FT(ID,II) = RDR_FT(ID,II) + FOOTREQ(II)
                     rcode = 1
                  ENDDO
               end if
            END IF
         END DO

         IF (opt_dbg) then
            write (605,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
               itimeyr,IR,rcode,'req', &
               (footreq(ii),ii=1,max_yr-1),' '
            write (605,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
               itimeyr,IR,rcode,'end', &
               (rdr_ft(id2,ii),ii=1,max_yr-1),' '
         END if
      END if

!     SUBTRACT CO2 CONSTRAINTS - ONLY FOR CO2 EOR PROJECTS

      IF (IRS .EQ. 25000) THEN
         DO T_YR = itimeyr , max_yr-2
            GAS_REQ(T_YR) = ECO2INJ(irs,T_YR-itimeyr+1) - ECO2RCY(irs,T_YR-itimeyr+1)
            iii = 1
            I_SRC = S_SRC(OGSM_REG,III,T_YR)
            I_REG = S_REG(OGSM_REG,III,T_YR)
            CO2_share(max(I_REG,1),max(I_SRC,1),T_YR) = 1.0
            src_shr = 1.0

!           write(6,2701) curirun, curiyr+1989, T_YR+2009, irs, pcode, res_num(irs,T_YR), I_REG, I_SRC, eco2reg(irs), eco2code(irs), EOR_RG, &
!              aresid(irs), gas_req(T_YR), nat_availco2(max(I_REG,1),max(I_SRC,1),T_YR), CO2_share(max(I_REG,1),max(I_SRC,1),T_YR), &
!              ECO2INJ(irs,T_YR-itimeyr+1), ECO2RCY(irs,T_YR-itimeyr+1), ESURFVOL(irs,T_YR-itimeyr+1), ETORECY(irs,T_YR-itimeyr+1), &
!              S_COST(OGSM_REG,III,T_YR), EPRODOIL(irs,T_YR-itimeyr+1), EPRODGAS(irs,T_YR-itimeyr+1), src_shr
!2701       format(1x,"WELLEXP_25000____CO2",11(":",I8),":",A11,11(":",F21.6))

         END DO
      END IF

      if (pcode.eq.3) then

         IF (opt_dbg) then
            write (603,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
               itimeyr,IR,ccode,'bgn', &
               (nat_availco2(IR,eco2code(irs),ii)*1000.0,ii=1,max_yr-1),' '
         END if

         DO T_YR = itimeyr , max_yr-2
            GAS_REQ(T_YR) = ECO2INJ(irs,T_YR-itimeyr+1) - ECO2RCY(irs,T_YR-itimeyr+1)
            IF (GAS_REQ(T_YR) .LT. 0.0) GAS_REQ(T_YR) = 0.0
            gas_req(T_YR) = gas_req(T_YR) / 1000.0
            co2_temp(T_YR) = gas_req(T_YR)
            CO2_share(:,:,T_YR) = 0.0
            IF (GAS_REQ(T_YR) .GT. 0.0) THEN
               shrtot = 0.
               do iii = 1 , res_num(irs,T_YR)
                  IF (GAS_REQ(T_YR) .GT. 0.0) THEN
                     I_SRC = S_SRC(OGSM_REG,III,T_YR)
                     I_REG = S_REG(OGSM_REG,III,T_YR)
                     if (gas_req(T_YR) .le. nat_availco2(I_REG,I_SRC,T_YR)) then
                        CO2_share(I_REG,I_SRC,T_YR) = 1.0 - sum(co2_share(:,:,T_YR))
                        src_shr = gas_req(T_YR) / co2_temp(T_YR)
                        shrtot = shrtot + src_shr

                         write(ogbug1,3701) curirun, curiyr+1989, T_YR+2009, irs, iii, I_REG, I_SRC, eco2reg(irs), eco2code(irs), EOR_RG, &
                            aresid(irs), gas_req(T_YR), nat_availco2(I_REG,I_SRC,T_YR), CO2_share(I_REG,I_SRC,T_YR), &
                            ECO2INJ(irs,T_YR-itimeyr+1), ECO2RCY(irs,T_YR-itimeyr+1), ESURFVOL(irs,T_YR-itimeyr+1), ETORECY(irs,T_YR-itimeyr+1), &
                            S_COST(OGSM_REG,III,T_YR), EPRODOIL(irs,T_YR-itimeyr+1), EPRODGAS(irs,T_YR-itimeyr+1), src_shr
 3701                   format(1x,"WELLEXP_Purchase_CO2",10(":",I8),":",A11,11(":",F21.6))

                        nat_availco2(I_REG,I_SRC,T_YR) = nat_availco2(I_REG,I_SRC,T_YR) - gas_req(T_YR)
                        use_availco2(I_REG,I_SRC,T_YR) = use_availco2(I_REG,I_SRC,T_YR) + gas_req(T_YR)                     
                        pur_co2_at_eor(EOR_RG,I_SRC,T_YR) = pur_co2_at_eor(EOR_RG,I_SRC,T_YR) + gas_req(T_YR)                     
                        gas_req(T_YR) = 0.0
                     else
                        CO2_share(I_REG,I_SRC,T_YR) = nat_availco2(I_REG,I_SRC,T_YR) / co2_temp(T_YR)
                        src_shr = nat_availco2(I_REG,I_SRC,T_YR) / co2_temp(T_YR)
                        shrtot = shrtot + src_shr

!                       YA 10.01.10 Re-instate line determining nat_availco2                        

                        write(ogbug1,3701) curirun, curiyr+1989, T_YR+2009, irs, iii, I_REG, I_SRC, eco2reg(irs), eco2code(irs), EOR_RG, &
                           aresid(irs), gas_req(T_YR), nat_availco2(I_REG,I_SRC,T_YR), CO2_share(I_REG,I_SRC,T_YR), &
                           ECO2INJ(irs,T_YR-itimeyr+1), ECO2RCY(irs,T_YR-itimeyr+1), ESURFVOL(irs,T_YR-itimeyr+1), ETORECY(irs,T_YR-itimeyr+1), &
                           S_COST(OGSM_REG,III,T_YR), EPRODOIL(irs,T_YR-itimeyr+1), EPRODGAS(irs,T_YR-itimeyr+1), src_shr

                        src_shr = nat_availco2(I_REG,I_SRC,T_YR) / co2_temp(T_YR)
                        gas_req(T_YR) = gas_req(T_YR) - nat_availco2(I_REG,I_SRC,T_YR) 
                        use_availco2(I_REG,I_SRC,T_YR) = use_availco2(I_REG,I_SRC,T_YR) + nat_availco2(I_REG,I_SRC,T_YR)
                        pur_co2_at_eor(EOR_RG,I_SRC,T_YR) = pur_co2_at_eor(EOR_RG,I_SRC,T_YR) + nat_availco2(I_REG,I_SRC,T_YR)
                        nat_availco2(I_REG,I_SRC,T_YR) = 0.0
                     endif
                     co2_recycle(EOR_RG,I_SRC,T_YR) = co2_recycle(EOR_RG,I_SRC,T_YR) + ECO2RCY(irs,T_YR-itimeyr+1) * src_shr * 0.001
                     co2_inject(EOR_RG,I_SRC,T_YR) = co2_inject(EOR_RG,I_SRC,T_YR) + ECO2INJ(irs,T_YR-itimeyr+1)  * src_shr * 0.001
                     eor_oil_prod(EOR_RG,I_SRC,T_YR) = eor_oil_prod(EOR_RG,I_SRC,T_YR) + EPRODOIL(irs,T_YR-itimeyr+1)  * src_shr
                     eor_gas_prod(EOR_RG,I_SRC,T_YR) = eor_gas_prod(EOR_RG,I_SRC,T_YR) + EPRODGAS(irs,T_YR-itimeyr+1)  * src_shr
                     eco2code(irs) = I_SRC
                     eco2reg(irs) = I_REG
                  END IF
                enddo
                if (shrtot.lt.0.99) then
                     src_shr = 1.0 - shrtot
                     i_src = 4
                     co2_recycle(EOR_RG,I_SRC,T_YR) = co2_recycle(EOR_RG,I_SRC,T_YR) + ECO2RCY(irs,T_YR-itimeyr+1) * src_shr * 0.001
                     co2_inject(EOR_RG,I_SRC,T_YR) = co2_inject(EOR_RG,I_SRC,T_YR) + ECO2INJ(irs,T_YR-itimeyr+1)  * src_shr * 0.001
                     eor_oil_prod(EOR_RG,I_SRC,T_YR) = eor_oil_prod(EOR_RG,I_SRC,T_YR) + EPRODOIL(irs,T_YR-itimeyr+1)  * src_shr
                     eor_gas_prod(EOR_RG,I_SRC,T_YR) = eor_gas_prod(EOR_RG,I_SRC,T_YR) + EPRODGAS(irs,T_YR-itimeyr+1)  * src_shr
                        write(ogbug1,3701) curirun, curiyr+1989, T_YR+2009, irs, iii, I_REG, I_SRC, eco2reg(irs), eco2code(irs), EOR_RG, &
                           aresid(irs), gas_req(T_YR), nat_availco2(I_REG,I_SRC,T_YR), CO2_share(I_REG,I_SRC,T_YR), &
                           ECO2INJ(irs,T_YR-itimeyr+1), ECO2RCY(irs,T_YR-itimeyr+1), ESURFVOL(irs,T_YR-itimeyr+1), ETORECY(irs,T_YR-itimeyr+1), &
                           S_COST(OGSM_REG,III,T_YR), EPRODOIL(irs,T_YR-itimeyr+1), EPRODGAS(irs,T_YR-itimeyr+1), src_shr
                endif
            ENDIF
         END do

         IF (opt_dbg) then
            write (603,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
               itimeyr,IR,ccode,'req', &
               (gas_req(ii)*1000.0,ii=1,max_yr-1),' '

            write (603,1601) aresid(irs),oil_prj,exp_prj,con_prj,hor_prj, &
               itimeyr,IR,ccode,'end', &
               (nat_availco2(IR,eco2code(irs),ii)*1000.0,ii=1,max_yr-1),' '
            write (603,*)
         END if

      end if

!     SUBTRACT GAS DEMAND CONSTRAINT - ONLY FOR EXPLORATION GAS PROJECTS

      if (pcode.ge.11.and.pcode.ne.17) then                   !APPLIED TO GAS PROJECTS
         ir = aregion(irs)
         do ii = itimeyr,max_yr-1
            nat_dmdgas(IR,ii) = nat_dmdgas(IR,ii) - &
               eprodgas(irs,ii)/1000.0
            nat_dmdgas(7,ii) = nat_dmdgas(7,ii) - &
               eprodgas(irs,ii)/1000.0

!           IF (nat_dmdgas(IR,ii).le.0.0) nat_dmdgas(IR,ii) = 0.0

         end do
      end if

      end subroutine

!*************************************************************************************
!*************************************************************************************
!*************************************************************************************
!*************************************************************************************

       subroutine remove_process(irs,iyear)

!  this subroutine checks the remaining projects and tags any rival EOR projects for the
!  reservoir as ineligible.

       implicit none

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
       include 'ogsmugr'
       include 'ogsml48'

       INTEGER irs,iyear
       INTEGER i
       INTEGER combo,combo1

       CHARACTER*9 rid,rid2

       rid = '         '
       read (aresid(irs)(1:9),'(a9)') rid
       IF(opt_dbg) write (211,*)
       IF(opt_dbg) write (211,*) aresid(irs),irs,eligible(irs), &
        timed(irs)

       do i = 1,nres
       combo1 = 0                                                       !flag for process which will NOT get removed
       combo  = 0                                                       !flag for approved process combination
          if (i.ne.irs) then
            rid2 = '         '
            read (aresid(i)(1:9),'(a9)') rid2
            if (rid.eq.rid2) then
              if (apcode(i).eq.0) combo1 = 1                          !decline curve does not get removed
              if (apcode(i).ge.11.and.apcode(i).le.20 &
                 .and.apcode(i).ne.17) combo1 = 1                     !discovered and undiscovered gas processes are not removed
              if (apcode(irs).eq.3.and.apcode(i).eq.6) combo = 1    !infill & CO2 Flood combination is permitted
              if (apcode(irs).eq.6.and.apcode(i).eq.3) combo = 1    !CO2 Flood and infill combination is permitted

              if (apcode(irs).eq.3.and.apcode(i).eq.8) combo = 1    !CO2 FLOOD AND HORIZONTAL CONTINUITY COMBINATION IS PERMITTED
              if (apcode(irs).eq.8.and.apcode(i).eq.3) combo = 1    !HORIZONTAL CONTINUITY AND CO2 FLOOD COMBINATION IS PERMITTED

!              if (apcode(irs).eq.3.and.apcode(i).eq.9) combo = 1    !CO2 FLOOD AND HORIZONTAL PROFILE COMBINATION IS PERMITTED
!              if (apcode(irs).eq.9.and.apcode(i).eq.3) combo = 1    !HORIZONTAL PROFILE AND CO2 FLOOD COMBINATION IS PERMITTED


              IF ( COMBO.EQ.0.AND.COMBO1.EQ.0 ) THEN
                      ELIGIBLE(I) = .FALSE.
                      TIMED(I)    = .FALSE.
              END if
              IF(opt_dbg) write (211,*) iyear,irs, &
               aresid(i),apcode(i), &
              apcode(irs),combo,combo1,eligible(i),timed(i)
            end if
          end if
       end do

       end subroutine


!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************

       subroutine project_preferences(jres,nprj,drill)

       implicit none

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
       include 'ogsmugr'
       include 'ogsml48'
       include 'intout'
       include 'ngtdmrep'

       INTEGER i
       INTEGER jres                               !reservoir pointer
       INTEGER nprj                               !number of reservoirs in the list of candidates
       LOGICAL drill                              !whether a project is to be developed
       CHARACTER*9 res1,res2
       CHARACTER*2 st1
       REAL co2cap


       READ(aresid(jres)(3:4),'(a2)') st1

       IF (APCODE(Jres).eq.4.or.APCODE(Jres).eq.5.or.APCODE(Jres).eq.7) THEN

         if (itimeyr+l48b4yr-1.gt.2040) then
            drill = .false.
            IF(opt_dbg) write (217,1217) itimeyr,aresid(jres), &
               apcode(jres),drill,'PRJ LIMIT',5
            GOTO 101
         end if
       END IF

       IF (APCODE(Jres).EQ.6) THEN

         if ((itimeyr.gt.1.and.timed_proc(7,itimeyr).ge.it_wop(curiyr,1)*.1).or.  &
             (itimeyr.eq.1.and.timed_proc(7,itimeyr).ge.it_wop(curiyr,1)*.1)) then
            drill = .false.
            IF(opt_dbg) write (217,1217) itimeyr,aresid(jres), &
               apcode(jres),drill,'PRJ LIMIT',6
            GOTO 101
         end if
       END IF

       IF (APCODE(Jres).EQ.10) THEN

         if (timed_proc(11,itimeyr).ge.it_wop(curiyr,1)*0.05) then
            drill = .false.
            IF(opt_dbg) write (217,1217) itimeyr,aresid(jres), &
               apcode(jres),drill,'PRJ LIMIT',10
            GOTO 101
         end if
       END IF

       IF (APCODE(Jres).EQ.16) THEN

         if (timed_proc(17,itimeyr).ge.ogwprng(mnumor,curiyr)*0.2) then
            drill = .false.
            IF(opt_dbg) write (217,1217) itimeyr,aresid(jres), &
               apcode(jres),drill,'PRJ LIMIT',16
            GOTO 101
         end if
       END IF

       IF (APCODE(Jres).EQ.19) THEN
            drill = .false.
            GOTO 101
       END IF

 102   continue     


 1217  format (i4,2x,a11,2x,i8,2x,l6,2x,a11,2x,i8)

 101   continue

       end subroutine
!***************************************************************
!     from PRJ_SORT.FOR
!     Last change:  MC   19 May 2009    3:22 pm

      SUBROUTINE PROJECT_SORT(isort,esort)

!     this subroutine sorts the projects by either investment efficiency or rate of return

      implicit none

      include'parametr'     ! nems dimension parameters
      include'ncntrl'       ! nems control variables
      include'ogsmparm'     ! ogsm parameter file
      include 'ogsmbfw'
      include 'ogsmugr'
      include 'ogsml48'

      INTEGER ISRC                                      ! MC CHANGE 5.6.09
      INTEGER ISORT                                     ! RETURN FOR NUMBER OF EOR PROJECTS TO EVALUATE
      INTEGER esort                                     ! return for number of exploration projects to evaluate
      INTEGER NUM_RES                                   ! INPUT NUMBER OF RESERVOIRS TO RANK
      INTEGER SORT
      INTEGER I1,nn,tt,jj,ii,ir
      INTEGER TEM1
      INTEGER EXPLSORT
      INTEGER OGSM_REG

      INTEGER TEM3
      INTEGER NUMSORT

      REAL TEM2,g
      REAL SORTINV(MAX_RES)
      INTEGER SORTSRC(MAX_RES)                         !MC CHANGE 5.6.09
      INTEGER EXPLSORTINV(MAX_RES)

      INTEGER INSERTPOS
      INTEGER INSDONE
      INTEGER KK
      INTEGER SORTCOUNT
      CHARACTER*1 DEV_ID

!     step 1: initialize sort variables

      DO I = 1,MAX_RES
         SORTINV(I)      = 0.0
         sortires(i)     = 0
         SORTiSRC2(I)    = 0                          !MC CHANGE 5.6.09
         SORTiREG2(I)    = 0                          !MC CHANGE 5.6.09
         EXPLSORTIRES(I) = 0
         EXPLSORTINV(I)  = 0
         EXPLSORTIRES2(I)= 0
         SORTIRES2(I)= 0
      END DO
      EXPLSORT=0
      SORT=0

!     step 2: index exploration projects
!     both conventional and unconventional
!
!     NOTE: THE DISCOVERY ORDER IS TO BE PRESERVED.  NO CONSIDERATION FOR ECONOMICS AT THIS TIME.
!

!     DO I= EX_FCRES,EX_CONRES                             !MC CHANGE 5.19.09
!     IF (APCODE(I,1).EQ.10.OR.APCODE(I,1).GE.16) THEN              !MC CHANGE 5.19.09
!     EXPLSORT = EXPLSORT+1                                      !MC CHANGE 5.19.09
!     EXPLSORTIRES2(EXPLSORT) = I                                !MC CHANGE 5.19.09
!     END IF                                                        !MC CHANGE 5.19.09
!     ENDDO                                                           !MC CHANGE 5.19.09

      DO I=1,NUMDISC                                                    !MC CHANGE 5.19.09
         TEM3 = 0                                                        !MC CHANGE 5.19.09
         READ(UND_RESID(I)(10:11),'(I2)') TEM3                           !MC CHANGE 5.19.09
         IF (ELIGIBLE(UND_COUNTER(I)).and. &
            (TEM3.EQ.10.OR.TEM3.GE.16)) THEN                              !MC CHANGE 5.19.09
            EXPLSORT = EXPLSORT+1                                        !MC CHANGE 5.19.09
            EXPLSORTIRES2(EXPLSORT) = I                                  !MC CHANGE 5.19.09
         END IF                                                         !MC CHANGE 5.19.09
      END DO                                                            !MC CHANGE 5.19.09

      IF (opt_dbg) then
         do i = 1,explsort

!           write (212,1208) aresid(explsortires2(i),1),itimeyr,            !MC CHANGE 5.19.09 !debug - economically viable EOR projects
!           &    apcode(explsortires2(i),1),project_npv(explsortires2(i)),     !MC CHANGE 5.19.09
!           &    explsortires2(i),first_dec(explsortires2(i)),                 !MC CHANGE 5.19.09
!           &    last_Dec(explsortires2(i)),EXPLsortinv(explsortires2(i))      !MC CHANGE 5.19.09

            WRITE (212,1213) UND_RESID(EXPLSORTIRES2(I)),ITIMEYR,            & !MC CHANGE 5.19.09
               APCODE(UND_COUNTER(EXPLSORTIRES2(I))),                        & !MC CHANGE 5.19.09
               PROJECT_NPV(UND_COUNTER(EXPLSORTIRES2(I)))                     !MC CHANGE 5.19.09
 1213       FORMAT (A11,3X,I2,3X,I2,3X,F12.3)                               !MC CHANGE 5.19.09
         end do
         write (212,*)
      END if

!     step 4: sort the reserves growth (EOR/ASR)

      DO I=1,Nres
         OGSM_REG = AREGION(I)
         IF (APCODE(I).EQ.3.AND.ELIGIBLE(I)) THEN                  !FOR CO2EOR ONES
            DO II = 1,res_num(i,itimeyr)
               ISRC = s_src(OGSM_REG,ii,itimeyr)
               IR = s_reg(OGSM_REG,ii,itimeyr)
               IF(ECO2RANKVAL(I,ISRC,IR).GT.0.0) THEN
                  SORT = SORT+1
                  SORTIRES2(SORT) = I
                  SORTINV(SORT) = ECO2RANKVAL(I,ISRC,IR)
                  SORTISRC2(SORT) = ISRC
                  SORTIREG2(SORT) = IR
               END IF
            END DO
         ELSE

!           IF (ELIGIBLE(I).and.project_npv(i).gt.0.0 &                                            !mc change ver 4
!              .AND.APCODE(I).lt.16) THEN                                                         !mc change ver 4

            IF (ELIGIBLE(I).and.project_npv(i).gt.0.0    &                                               !mc change ver 4
               .AND.(apcode(i).lt.16.or.apcode(i).gt.20  &
	       .or.(apcode(i).eq.17.and.aresflag(i).eq.9))) then          !mc change 6.19.09              !mc change ver 4

               if (apcode(i).ne.10) then

                  SORT = SORT+1
                  SORTIRES2(SORT) = I
                  SORTINV(SORT)   = RANKING_VAL(I)
                  SORTISRC2(SORT) = 0                                                   !MC CHANGE 5.6.09
                  SORTIREG2(SORT) = 0                                                   !MC CHANGE 5.6.09

!                 WRITE(925,1925) curiyr+1989,i,sort,sortires2(sort),sortinv(sort),aresid(i),aplay_cde(i),  &
!                          first_dec(sort),last_dec(sort)
!1925             format (i4,2x,i6,2x,i4,2x,i6,2x,f12.2,2x,a11,2x,i12,2x,i2,2x,i2)

               end if
            ENDIF
         END IF                                   !MC CHANGE 5.6.09
      END do

!     WRITE(OGBUG1,*) 'BEFORE SORT'
!     do i = 1,sort
!        WRITE(OGBUG1,*) sort,sortires2(i),sortinv(i),SORTISRC2(I),i
!     end do

      n = sort
      nn = n-1
      do j = 1, nn
         l = j
         jj = j + 1
         do i = jj, n
            if (sortinv(l).lt.sortinv(i)) then
               l = i
            end if
         end do

         tt = sortires2(l)
         sortires2(l) = sortires2(j)
         sortires2(j) = tt

         tt = sortISRC2(l)                               !MC CHANGE 5.6.09
         sortISRC2(l) = sortISRC2(j)                    !MC CHANGE 5.6.09
         sortISRC2(j) = tt                               !MC CHANGE 5.6.09

         tt = sortIREG2(l)                               !MC CHANGE 5.6.09
         sortIREG2(l) = sortIREG2(j)                    !MC CHANGE 5.6.09
         sortIREG2(j) = tt                               !MC CHANGE 5.6.09

         g = sortinv(l)
         sortinv(l) = sortinv(j)
         sortinv(j) = g
      end do

!     WRITE(OGBUG1,*) ''
!     WRITE(OGBUG1,*) 'AFTER SORT'
!     do i = 1,sort
!        WRITE(OGBUG1,*) sort,sortires2(i),sortinv(sortires2(i)),
!        &       SORTISRC2(I),sortinv(i)
!     end do

!     ***********************************************************************
!
!     PROJECTS HAVE BEEN SORTED.  THE MASTER POINTER ARRAY IS SORTIRES2(I).
!     WRITE A RANKED ORDER LIST TO FILE.
!
!     ***********************************************************************

!     IF (opt_dbg) then

         do i = 1,sort
            write (OGBUG1,1209) 'EOR projects:  ',aresid(sortires2(i)),itimeyr,           & !debug - economically viable EOR projects
               apcode(sortires2(i)),project_npv(sortires2(i)),sortires2(i), &
               first_Dec(sortires2(i)),last_Dec(sortires2(i)), &
               sortinv(i)
 1209       format (a15,a11,2x,i7,2x,i7,2x,f12.2,3(2x,i7),2x,f12.2)

!           call rpt_oneliner(sortires2(i),itimeyr)

         end do

!     END if

 1208 format (a11,2x,i7,2x,i7,2x,f12.2,4(2x,i7))

      ISORT = SORT
      esort = EXPlsort

      END SUBROUTINE

!***************************************************************
!from window_year.For
!     Last change:  MC   16 Mar 2009    1:52 pm
       SUBROUTINE WINDOW_YEAR(IRES,N_PRJ,WINDOW_TYPE)
!
! THIS SUBROUTINE SPECIFIES THE WINDOW YEARS FOR ASR/EOR
! PROJECTS SUBSEQUENT TO THE DECLINE PORTION OF RECOVERY.
!
       IMPLICIT NONE

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
       INCLUDE 'ogsmugr'           
       INCLUDE 'ogsml48'

       INTEGER IRES
       INTEGER IJK
       INTEGER ILAST
       INTEGER N_PRJ
       INTEGER WINDOW_TYPE
       INTEGER ishift
       INTEGER vyear(max_res)        ! volumetric window year
       INTEGER vchk,itech

       REAL ooip
       REAL SOC_WIN,SOI_WIN,sorw
       REAL boc,BOI_WIN
       REAL cumoil
!
! WINDOW TYPE IS 1 - OIL DECLINE
!
       ishift = 0
       IF (WINDOW_TYPE.EQ.1) THEN
!
! Determine Window Year Based on Volumetrics
!
          vyear(ires) = max_yr-1
          vchk = 0
          cumoil = 0.0
          ooip = 0.0
          SOC_WIN = 0.0
          boc = 0.0

          SOI_WIN = asoi(ires)
          sorw= asor(ires)
          BOI_WIN = aboi(ires)

!  calculate ooip (mbbl)
          ooip = 7758.0*(ATOTACRES(ires)*apay(ires)*(SOI_WIN/100.0)* &
             (aphi(ires)/100.0))/BOI_WIN
          ooip = ooip/1000.0

!  calculate boc
          call calbo(atemp(ires),aapi(ires),apresin(ires), &
           agas_grav(ires),boc)

          do IJK = 1,max_yr-1
            if (vchk.eq.0) then
             cumoil = cumoil+eprodoil(ires,ijk)
             IF(BOI_WIN.gt.0.0) SOC_WIN = SOI_WIN*(1-cumoil/ooip)* &
                                         (boc/BOI_WIN)
             IF(SOC_WIN.gt.SOI_WIN) SOC_WIN = SOI_WIN
             IF(SOC_WIN.le.sorw) SOC_WIN = sorw
             IF ((SOC_WIN-sorw).le.veorcp) THEN
               vyear(ires) = ijk
               vchk = 1
               do itech = 1,max_tech
                 asoc(ires) = SOC_WIN
                 abo(ires) = boc
               end do
             END IF
            end if
          end do
!
!  Determine window year based on production
!
          ILAST = max_yr-1
          DO IJK=max_yr-1,1,-1
            IF (EPRODOIL(IRES,IJK).GT.0.0) GOTO 302
            IF (EPRODOIL(IRES,IJK).LE.0.0) ILAST = IJK-1
          ENDDO
302       CONTINUE
!
!         SPECIFY WINDOW FOR IMPLEMENTATION OF ASR/EOR PROJECTS
!
          DO IJK=1,N_PRJ
             IF (ARESID(IJK).EQ.ARESID(Ires)) THEN
                FIRST_DEC(IJK) = MIN(AECON_LIFE(IRES),vyear(ires)) &
                  - PSHUT+ishift
                LAST_DEC(IJK)  = Max(MIN(AECON_LIFE(IRES),vyear(ires)) &
                  + NSHUT,ILAST)+ishift
                IF(last_Dec(ijk).gt.max_yr-1) last_dec(ijk)=max_yr-1
                IF(first_dec(ijk).le.1) first_dec(ijk) = 1
!
                IF (FIRST_DEC(IJK).GE.max_yr-3) FIRST_DEC(IJK) = max_yr

                first_asr(ijk) = MIN(aecon_life(ires),vyear(ires)) - &
                  asr_st + ishift
                last_asr(ijk) = MAX(MIN(aecon_life(ires),vyear(ires))+ &
                  asr_ed,ilast)+ishift
                IF(last_asr(ijk).gt.max_yr-1) last_asr(ijk) = max_yr-1
                IF(first_asr(ijk).le.1) first_asr(ijk) = 1
                IF(first_asr(ijk).ge.max_yr-3) first_asr(ijk) = MAX_yr


       IF(opt_dbg) &
        write (102,1) ires,aresid(ijk),aecon_life(ires),vyear(ires) &
        ,first_dec(ijk),last_dec(ijk),first_asr(ijk),last_asr(ijk)
             ENDIF
          ENDDO
!
 1     format (i6,2x,a11,2x,6(i2,2x))
!
! WINDOW TYPE IS 2 - GAS DECLINE
!
       ELSE
!
!         FIND CHANGE FROM CONSTANT RATE -
!
          ILAST = max_yr-1
          DO IJK=2,max_yr-1
            IF (EPRODGAS(IRES,IJK).LE.(0.95*EPRODGAS(IRES,IJK-1))) THEN
               ILAST = IJK
               GOTO 303
            ENDIF
          ENDDO
303       CONTINUE
          FIRST_DEC(ires) = ILAST+ishift
          first_asr(ires) = ilast+ishift
!
!         FIND LAST YEAR OF PRODUCTION
!
          last_Dec(ires) = max_yr-1
          DO IJK=2,max_yr-1
            IF (EPRODGAS(IRES,IJK).LE.0.0) THEN
               LAST_DEC(IRES) = IJK+ishift
               last_asr(ires) = ijk+ishift
               GOTO 304
            ENDIF
          ENDDO
304       CONTINUE
!
       IF(opt_dbg) &
        write (102,1) ires,aresid(ires),aecon_life(ires),99, &
        first_dec(ires),last_dec(ires),first_asr(ires),last_asr(ires)
       END IF
!
       RETURN
       END SUBROUTINE
!***************************************************************
!from RESERVES.For       
!     Last change:  MC    8 Apr 2009   11:49 am
       SUBROUTINE RESERVES

       IMPLICIT NONE

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
       INCLUDE 'ogsmbfw'
       INCLUDE 'ogsml48'

       REAL INIT_OIL,INIT_ADGAS,INIT_CONVGAS,INIT_UNCONVGAS

       REAL DECL_OIL(MAX_YR)
       REAL OTHR_OIL(MAX_YR)
       REAL BOOK_OIL(MAX_YR)
       REAL OIL_RESV(MAX_YR)

       REAL DECL_ADGAS(MAX_YR)
       REAL DECL_CONVGAS(MAX_YR)
       REAL DECL_UNCONVGAS(MAX_YR)

       REAL OTHR_ADGAS(MAX_YR)
       REAL OTHR_CONVGAS(MAX_YR)
       REAL OTHR_UNCONVGAS(MAX_YR)

       REAL BOOK_ADGAS(MAX_YR)
       REAL BOOK_CONVGAS(MAX_YR)
       REAL BOOK_UNCONVGAS(MAX_YR)

       REAL ADGAS_RESV(MAX_YR)
       REAL CONVGAS_RESV(MAX_YR)
       REAL UNCONVGAS_RESV(MAX_YR)

       DO I = 1,MAX_YR
         DECL_OIL(I) = 0.0
         OTHR_OIL(I) = 0.0
         BOOK_OIL(I) = 0.0
         OIL_RESV(I) = 0.0

         DECL_ADGAS(I) = 0.0
         DECL_CONVGAS(I) = 0.0
         DECL_UNCONVGAS(I) = 0.0

         OTHR_ADGAS(I) = 0.0
         OTHR_CONVGAS(I) = 0.0
         OTHR_UNCONVGAS(I) = 0.0

         BOOK_ADGAS(I) = 0.0
         BOOK_CONVGAS(I) = 0.0
         BOOK_UNCONVGAS(I) = 0.0

         ADGAS_RESV(I) = 0.0
         CONVGAS_RESV(I) = 0.0
         UNCONVGAS_RESV(I) = 0.0
       END DO

! INCLUDES BOTH CRUDE OIL AND LEASE CONDENSATE FROM DECLINING PROCESSES
       DO I = 1,MAX_CLASS
         DO J = 1,MAX_FUEL
            DO K = 1,MAX_YR
               DECL_OIL(K) = DECL_OIL(K) + &
                 PC_EPRODOIL(I,J,1,K)+PC_EPRODOILCO2(I,J,1,K)+ &
                 PC_EPRODOILEOR(I,J,1,K)! + PC_EPRODCON(I,J,12,K) +
!     &           PC_EPRODCON(I,J,13,K)   + PC_EPRODCON(I,J,14,K) +
!     &           PC_EPRODCON(I,J,15,K)   + PC_EPRODCON(I,J,16,K)
            END DO
         END DO
       END DO

! DECLINE ADGAS
       DO I = 1,MAX_CLASS
         DO J = 1,MAX_FUEL
            DO K = 1,MAX_YR
               DECL_ADGAS(K) = DECL_ADGAS(K) + PC_EPRODADD(I,J,1,K)
            END DO
         END DO
       END DO

! DECLINE CONVENTIONAL GAS
       DO I = 1,MAX_CLASS
         DO J = 1,MAX_FUEL
            DO K = 1,MAX_YR
               DECL_CONVGAS(K) = DECL_CONVGAS(K) + &
                 PC_EPRODGAS(I,J,12,K) + PC_EPRODGAS(I,J,13,K)
            END DO
         END DO
       END DO

! DECLINE UNCONVENTIONAL GAS
       DO I = 1,MAX_CLASS
         DO J = 1,MAX_FUEL
            DO K = 1,MAX_YR
               DECL_UNCONVGAS(K) = DECL_UNCONVGAS(K) + &
                 PC_EPRODGAS(I,J,14,K) + PC_EPRODGAS(I,J,15,K) + &
                 PC_EPRODGAS(I,J,16,K)
            END DO
         END DO
       END DO

       DO I = 1,MAX_CLASS
         DO J = 1,MAX_FUEL
           DO K = 1,MAX_YR
              DO L = 1,MAX_PROC
                 IF(L.GE.2.AND.L.LE.11) THEN   !EOR/ASR OIL, CONV OIL DISC
                   OTHR_OIL(K) = OTHR_OIL(K) + &
                     PC_EPRODOIL(I,J,L,K)
                 ELSEIF(L.EQ.18) THEN          !UNCONV OIL DISC
                   OTHR_OIL(K) = OTHR_OIL(K) + &
                     PC_EPRODOIL(I,J,L,K)
!                 ELSEIF(L.EQ.17) THEN          !CONV GAS DISC
!                   OTHR_OIL(K) = OTHR_OIL(K) +
!     &               PC_EPRODCON(I,J,L,K)
!                 ELSEIF(L.GE.19) THEN          !UNCONV GAS DISC, UNCONV GAS DEV
!                   OTHR_OIL(K) = OTHR_OIL(K) +
!     &               PC_EPRODCON(I,J,L,K)
                 END IF
              END DO
           END DO
         END DO
       END DO

       DO I = 1,MAX_CLASS
         DO J = 1,MAX_FUEL
           DO K = 1,MAX_YR
             DO L = 1,MAX_PROC
               IF(L.GE.2.AND.L.LE.11) THEN  !ADGAS
                OTHR_ADGAS(K) = OTHR_ADGAS(K) + PC_EPRODGAS(I,J,L,K)
               ELSEIF(L.EQ.18) THEN         !ADGAS
                OTHR_ADGAS(K) = OTHR_ADGAS(K) + PC_EPRODGAS(I,J,L,K)
               ELSEIF(L.EQ.17) THEN         !CONVGAS
                OTHR_CONVGAS(K) = OTHR_CONVGAS(K) + PC_EPRODGAS(I,J,L,K)
               ELSEIF(L.GE.19) THEN         !UNCONVGAS
                OTHR_UNCONVGAS(K) = OTHR_UNCONVGAS(K) + &
                PC_EPRODGAS(I,J,L,K)
               END IF
             END DO
           END DO
         END DO
       END DO

       DO I = 1,MAX_PROC
         DO J = 1,MAX_CLASS
           DO K = 1,MAX_FUEL
             DO L = 1,MAX_YR
                BOOK_OIL(L) = BOOK_OIL(L) + PC_ADDPRODOIL(J,K,I,L)
             END DO
           END DO
         END DO
       END DO

       DO I = 1,MAX_PROC
         DO J = 1,MAX_CLASS
           DO K = 1,MAX_FUEL
             DO L = 1,MAX_YR
                 IF(I.GE.2.AND.I.LE.11) THEN   !EOR/ASR OIL, CONV OIL DISC
                  BOOK_ADGAS(L) = BOOK_ADGAS(L) + PC_ADDPRODGAS(J,K,I,L)
                 ELSEIF(I.EQ.18) THEN          !UNCONV OIL DISC
                  BOOK_ADGAS(L) = BOOK_ADGAS(L) + PC_ADDPRODGAS(J,K,I,L)
                 ELSEIF(I.EQ.17) THEN          !CONV GAS DISC
                  BOOK_CONVGAS(L) = BOOK_CONVGAS(L) + &
                  PC_ADDPRODGAS(J,K,I,L)
                 ELSEIF(I.GE.19) THEN          !UNCONV GAS DISC, UNCONV GAS DEV
                  BOOK_UNCONVGAS(L) = BOOK_UNCONVGAS(L) + &
                  PC_ADDPRODGAS(J,K,I,L)
                 END IF
             END DO
           END DO
         END DO
       END DO





       !CONVERT TO MILLION BBL
       DO I = 1,MAX_YR
          DECL_OIL(I) = DECL_OIL(I)/1000.0
          OTHR_OIL(I) = OTHR_OIL(I)/1000.0
          BOOK_OIL(I) = BOOK_OIL(I)/1000.0
       END DO

       !CONVERT TO BCF
       DO I = 1,MAX_YR
          DECL_ADGAS(I) = DECL_ADGAS(I)/1000.0
          DECL_CONVGAS(I) = DECL_CONVGAS(I)/1000.0
          DECL_UNCONVGAS(I) = DECL_UNCONVGAS(I)/1000.0

          OTHR_ADGAS(I) = OTHR_ADGAS(I)/1000.0
          OTHR_CONVGAS(I) = OTHR_CONVGAS(I)/1000.0
          OTHR_UNCONVGAS(I) = OTHR_UNCONVGAS(I)/1000.0

          BOOK_ADGAS(I) = BOOK_ADGAS(I)/1000.0
          BOOK_CONVGAS(I) = BOOK_CONVGAS(I)/1000.0
          BOOK_UNCONVGAS(I) = BOOK_UNCONVGAS(I)/1000.0
       END DO

       INIT_OIL = RESV_OIL*1000.0   !RESERVES IN MILLION OF BARRELS
       INIT_ADGAS = RESV_ADGAS*1000.0
       INIT_CONVGAS = RESV_CONVGAS*1000.0
       INIT_UNCONVGAS  = RESV_UNCONGAS*1000.0

       DO I = 1,MAX_YR
          IF(I.EQ.1) THEN
            OIL_RESV(I) = INIT_OIL - DECL_OIL(I) - OTHR_OIL(I) + &
             BOOK_OIL(I)
            ADGAS_RESV(I) = INIT_ADGAS - DECL_ADGAS(I) - OTHR_ADGAS(I) + &
             BOOK_ADGAS(I)
            CONVGAS_RESV(I) = INIT_CONVGAS - DECL_CONVGAS(I) - &
             OTHR_CONVGAS(I) + BOOK_CONVGAS(I)
            UNCONVGAS_RESV(I) = INIT_UNCONVGAS - DECL_UNCONVGAS(I) - &
             OTHR_UNCONVGAS(I) + BOOK_UNCONVGAS(I)
          ELSE
            OIL_RESV(I) = OIL_RESV(I-1) - DECL_OIL(I) - OTHR_OIL(I) + &
             BOOK_OIL(I)
            ADGAS_RESV(I) = ADGAS_RESV(I-1) - DECL_ADGAS(I) - &
             OTHR_ADGAS(I) + BOOK_ADGAS(I)
            CONVGAS_RESV(I) = CONVGAS_RESV(I-1) - DECL_CONVGAS(I) - &
             OTHR_CONVGAS(I) + BOOK_CONVGAS(I)
            UNCONVGAS_RESV(I) = UNCONVGAS_RESV(I-1) - DECL_UNCONVGAS(I) &
             - OTHR_UNCONVGAS(I) + BOOK_UNCONVGAS(I)
          END IF
       END DO

       IF(OGRUNOP(1).EQ.5)THEN
        open (UNIT=843,FILE='added_prod.dbg',STATUS='unknown')
        write(843,*) '***** ADDED_PROD.DBG *****'
         do i = 1,max_class
           do j = 1,max_fuel
              do k = 1,max_proc
                write (843,1843) i,j,k-1,(pc_addprodoil(i,j,k,l),l=1,max_yr-1)
              end do
              write (843,*)
           end do
         end do

        close (843)
 1843    format (i3,3x,i3,3x,i3,3x,<max_yr-1>(f16.3,3x))
       ENDIF

       IF(OGRUNOP(1).EQ.5)THEN
        OPEN (UNIT=842,FILE='RESERVES.OUT',STATUS='UNKNOWN')
        WRITE (842,*) '***** RESERVES.OUT *****'
        WRITE (842,4) 'MILLION BARRELS CRUDE'
        WRITE (842,1) 'INITIAL RESERVES: ', INIT_OIL
        WRITE (842,*)
        WRITE (842,3) (L48B4YR-1+I,I=1,MAX_YR-1)
        WRITE (842,2) 'DECLINE PRODUCTION: ',(DECL_OIL(I),I=1,MAX_YR-1)
        WRITE (842,2) 'OTHER   PRODUCTION: ',(OTHR_OIL(I),I=1,MAX_YR-1)
        WRITE (842,2) 'NEW BOOKED PROD   : ',(BOOK_OIL(I),I=1,MAX_YR-1)
        WRITE (842,*)
        WRITE (842,2) 'END OF YEAR RESERVES: ',(OIL_RESV(I),I=1,MAX_YR-1)
        WRITE (842,*)
  
        WRITE (842,4) 'BILLION CUBIC FEET'
        WRITE (842,1) 'INITIAL ADGAS RESV: ',INIT_ADGAS
        WRITE (842,*)
        WRITE (842,3) (L48B4YR-1+I,I=1,MAX_YR-1)
        WRITE (842,2) 'DECLINE ADGAS PROD: ',(DECL_ADGAS(I),I=1,MAX_YR-1)
        WRITE (842,2) 'OTHER   ADGAS PROD: ',(OTHR_ADGAS(I),I=1,MAX_YR-1)
        WRITE (842,2) 'NEW BOOKED ADGAS PROD: ', &
         (BOOK_ADGAS(I),I=1,MAX_YR-1)
        WRITE (842,*)
        WRITE (842,2) 'EOY ADGAS RESV: ',(ADGAS_RESV(I),I=1,MAX_YR-1)
        WRITE (842,*)
  
        WRITE (842,4) 'BILLION CUBIC FEET'
        WRITE (842,1) 'INITIAL CONVGAS RESV: ',INIT_CONVGAS
        WRITE (842,*)
        WRITE (842,3) (L48B4YR-1+I,I=1,MAX_YR-1)
        WRITE (842,2) 'DECLINE CONVGAS PROD: ', &
         (DECL_CONVGAS(I),I=1,MAX_YR-1)
        WRITE (842,2) 'OTHER   CONVGAS PROD: ', &
         (OTHR_CONVGAS(I),I=1,MAX_YR-1)
        WRITE (842,2) 'NEW BOOKED CONVGAS PROD: ', &
         (BOOK_CONVGAS(I),I=1,MAX_YR-1)
        WRITE (842,*)
        WRITE (842,2) 'EOY CONVGAS RESV: ',(CONVGAS_RESV(I),I=1,MAX_YR-1)
        WRITE (842,*)
 
        WRITE (842,4) 'BILLION CUBIC FEET'
        WRITE (842,1) 'INITIAL UNCONVGAS RESV: ',INIT_UNCONVGAS
        WRITE (842,*)
        WRITE (842,3) (L48B4YR-1+I,I=1,MAX_YR-1)
        WRITE (842,2) 'DECLINE UNCONVGAS PROD: ', &
         (DECL_UNCONVGAS(I),I=1,MAX_YR-1)
        WRITE (842,2) 'OTHER   UNCONVGAS PROD: ', &
         (OTHR_UNCONVGAS(I),I=1,MAX_YR-1)
        WRITE (842,2) 'NEW BOOKED UNCONVGAS PROD: ', &
         (BOOK_UNCONVGAS(I),I=1,MAX_YR-1)
        WRITE (842,*)
        WRITE (842,2) 'EOY UNCONVGAS RESV: ', &
         (UNCONVGAS_RESV(I),I=1,MAX_YR-1)
        WRITE (842,*)
 
        CLOSE (842)

 1     FORMAT (A30,3X,F15.2)
 2     FORMAT (A30,3X,<MAX_YR-1>(F15.2,3X))
 3     FORMAT (30X,3X,<MAX_YR-1>(11X,I4,3X))
 4     FORMAT (A30)
     ENDIF
       END SUBROUTINE
      
!************************************************************************************************
       SUBROUTINE PRESCREEN 

       implicit none

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
       include 'ogsmbfw'
       include 'ogsmugr'
       include 'ogsml48'
       include 'intout'

       integer ires, cnt

!  set prices
       do m=1,max_yr
         oilpriceo(m) = it_wop(lastyr,1)*dladj
         gaspriceo(m) = it_wop(lastyr,1)*dladj/15.
       enddo
       oilco2 = it_wop(l48b4yr-baseyr+1,1)*dladj

       cnt = 0
       do ires=1,nres

           IF ((apcode(ires).ge.3.and.apcode(ires).le.10).OR.  &
                apcode(ires).eq.16.or.(apcode(ires).eq.17.and.aresflag(ires).ne.9)) THEN
             if (apcode(ires).eq.3) then
               call economics(IRES,itimeyr,2,1,1,aregion(ires))
             else
               call economics(IRES,itimeyr,1,0,1,aregion(ires))
             endif
             if (project_npv(ires).le.0.0) then
                eligible(ires) = .false.
                cnt = cnt + 1
             endif
           ENDIF
           PROJECT_npv(ires) = -999.99  ! RESET NPV SO THAT PROJECTS THAT ARE NOT ELIGIBLE DO NOT SLIP THROUGH THE TIMING

       ENDDO

       WRITE(OGBUG1,*) 'NUMBER OF PROJECTS REMOVED IN PRESCREENING: ', cnt, oilpriceo(1),oilco2,co2cost

       RETURN
       END SUBROUTINE
!*************************************************************************************************
!*************************************************************************************************
!*************************************************************************************************
!*************************************************************************************************
       subroutine rig_dist

       implicit none

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
       include 'ogsmbfw'
       include 'ogsmugr'
       include 'ogsml48'

!      integer i,j,k
       real tot_ftg(7)
       real temp_rig(10)

       do i = 1,10
         temp_rig(i) = 0.0
       end do


       do i = 1,7
         rigs_reg(i) = 0.0
         tot_ftg(i) = 0.0
         do j = 1,max_rdr
            rigs_drill(i,j) = 0.0
            rigs_class(i,j) = 0.0
            rigs_used(i,j)  = 0.0
            rigs_acct(i,j,itimeyr) = 0.0
         end do
         rigs_reg(i) = anint(drega(i)*log(oilpriceo(itimeyr))+dregb(i))
         write(441,*) i,itimeyr,drega(i),dregb(i),oilpricec(1),rigs_reg(i)                           !mc change 11.10.11 o / c?
         tot_ftg(i) = rigs_reg(i)*dreg_util*dreg_rate*24.0*365.0

!        write(441,*) 'tot_ftg before',i,itimeyr,tot_ftg(i),rigs_frac(i,itimeyr)
! apply the regional gas constraint factor - to isolate just the rigs which are available for gas drilling
          tot_ftg(i) = tot_ftg(i)*rigs_frac(i,itimeyr)
         write(441,*) 'tot_ftg after ',i,itimeyr,tot_ftg(i),rigs_frac(i,itimeyr)



         rigs_year(i,itimeyr) = 0.0
         rigs_year(i,itimeyr) = tot_ftg(i)
         do j = 1,max_rdr
           rigs_drill(i,j) = tot_ftg(i)*rdr(2,i,j)/100.0 
           if(j.lt.max_rdr) then
             rigs_class(i,j) = tot_ftg(i)*(rdr(2,i,j)-rdr(2,7,j+1))/100.0 
           else
             rigs_class(i,j) = tot_ftg(i)*rdr(2,i,j)/100.0 
           end if
         end do
       end do

        write (441,442)              (rdr(2,0,j),j=1,max_rdr),(rdr(2,0,j),j=1,max_rdr)
        do i = 1,7
          write (441,1441) itimeyr,i,(rigs_drill(i,j),j=1,max_rdr),(rigs_class(i,j),j=1,max_rdr)
        end do
        write (441,*)
  442  format (2x,3x,2x,<max_rdr>(3x,f16.2),<max_rdr>(3x,f16.2))
 1441  format (i2,3x,i2,<max_rdr>(3x,f16.2),<max_rdr>(3x,f16.2))

       end subroutine
!*************************************************************************************************
!*************************************************************************************************
!*************************************************************************************************
!*************************************************************************************************

       subroutine drill_well(jres)

       implicit none

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
       include 'ogsmbfw'
       include 'ogsmugr'
       include 'ogsml48'

       integer jres
!      integer i,j,k
       integer tempwell
       real finwell
       real twel
       real tdepth
       common/rigtran/finwell                       !mc change 11.10
       tempwell = 0
       tdepth = 0.0
       j = 1
! depth class.
       do i = 1,max_rdr
        if(i.eq.1) then
          if(adepth(jres).le.rdr(2,0,i)) then
            j = i
          end if
        else
          if(adepth(jres).le.rdr(2,0,i).and.adepth(jres).gt.rdr(2,0,i-1)) then
            j = i
          end if
        end if
       end do

!      write(441,*) itimeyr,j,rigs_drill(aregion(jres),j),'available',adepth(jres)

       tdepth = adepth(jres)*(1.0 + sucexp(aregion(jres))/100.0)
!       write (441,*) itimeyr,tdepth,1

       if(tdepth.lt.rigs_drill(aregion(jres),j)) tempwell = 1
       if(atotpat(jres,1).gt.2) then
         do i = 1,20000
           tdepth = tdepth + adepth(jres)*(1.0+aheatval(jres))
           if(tdepth.lt.rigs_drill(aregion(jres),j)) then
             tempwell = tempwell + 1
!            write (441,*) itimeyr,tdepth,tempwell
           else
             tdepth = tdepth - adepth(jres)*(1.0+aheatval(jres))
             goto 328
           end if
         end do
       end if
  328  continue
!       write (441,*)
!       write(441,*) 'theoretical',itimeyr,tempwell,adepth(jres)

! determine the number of wells which can be drilled.
!       tempwell = 0
       finwell = 0.0
       twel = 0.0

       finwell = anint(rigs_conc(j)/100.0*  &
                 (anint(min(tempwell*1.,PATT_DEV(apcode(jres)+1)*atotpat(jres,1)/100.0))))/1.0

!      if(itimeyr.eq.2) write(6,*) 'dh5out', tempwell, 'theoretical',adepth(jres)
!      if(itimeyr.eq.2) write(6,*) 'dh5out', patt_dev(apcode(jres)+1)*atotpat(jres,1)/100.0,'bounded',adepth(jres)
!      if(itimeyr.eq.2) write(6,*) 'dh5out', anint(min(tempwell*1.,PATT_DEV(apcode(jres)+1)*atotpat(jres,1)/100.0)),'min',adepth(jres)
!      if(itimeyr.eq.2) write(6,*) 'dh5out', atotpat(jres,1),itimeyr,finwell,'final',patt_dev(apcode(jres)+1),adepth(jres)

!      write(6,*) 'dh5out  ', aresid(jres),itimeyr,tempwell,twel,  &
!           adepth(jres)*(1.0 + sucexp(aregion(jres))/100.0)+((finwell-1.0)*adepth(jres)*(1.0+aheatval(jres)))

        write(339,1339) itimeyr,aresid(jres),adepth(jres),aregion(jres),apcode(jres),atotpat(jres,1),  &
                        patt_dev(apcode(jres)+1),j,PATT_DEV(apcode(jres)+ 1)*atotpat(jres,1)/100.0,  &
                        tempwell,rigs_conc(j),finwell,rigs_drill(aregion(jres),j),oilpricec(itimeyr)
  1339  format (i2,3x,a11,3x,f12.3,3x,i1,3x,i2,3x,f12.3,3x,f10.3,3x,i1,3x,f12.2,3x,i5,7(3x,f12.3))

       end subroutine

!*************************************************************************************************
!*************************************************************************************************
!*************************************************************************************************
!*************************************************************************************************

       subroutine remove_drill(jres)

       implicit none

        include'parametr'     ! nems dimension parameters
        include'ncntrl'       ! nems control variables
        include'ogsmparm'     ! ogsm parameter file
       include 'ogsmbfw'
       include 'ogsmugr'
       include 'ogsml48'

       integer jres
!      integer i,j,k,l
       integer tempwell
       integer cat1(max_rdr),cat2(max_rdr)
       real ftgdisto
       real ftgdist(max_rdr)
       real ftgused(max_rdr)
       real finwell
       real tftg
       real usewell(max_rdr)
       real avlwell(max_rdr)
       real useftg
       common/rigtran/finwell                       !mc change 11.10

       j = 1
       k = 0
       ftgdisto = 0.0
       do i = 1,max_rdr
         cat1(i) = 0
         cat2(i) = 0
         ftgused(i) = 0.0
         ftgdist(i) = 0.0
       end do

!       print*, 'evaluating',itimeyr,aresid(jres),adepth(jres)

!step 1: calculate the footage to be removed
       useftg = 0.0
       if(finwell.gt.1.0) then
        useftg = adepth(jres)*(1.0 + sucexp(aregion(jres))/100.0)+((finwell-1.0)*adepth(jres)*(1.0+aheatval(jres)))
       else
        useftg = adepth(jres)*(1.0 + sucexp(aregion(jres))/100.0)
       end if
!       useftg = adepth(jres)*finwell
!       if (itimeyr.eq.2) print*, itimeyr,'to be removed', useftg,' number of wells',finwell
       ftgdisto = useftg


!       if(itimeyr.eq.2) then
!         do i = 1,max_Rdr
!           print*, itimeyr,aresid(jres),adepth(jres),i,rigs_class(aregion(jres),i),rigs_drill(aregion(jres),i),'footage by category'
!         end do
!         print*, ''
!       end if

!step 2: determine the categories which it can be removed from
!  locate the depth range for the project
       do i = 1,max_Rdr
         if(i.eq.1) then
           if(adepth(jres).le.rdr(2,0,i)) cat1(i) = 1
         else
           if(adepth(jres).le.rdr(2,0,i).and.adepth(jres).gt.rdr(2,0,i-1)) cat1(i) = 1
         end if
       end do
!  determine the eligible categories
       do i = 1,max_rdr
         if(i.eq.1) then
            cat2(i) = cat1(i)
         else
            cat2(i) = cat2(i-1) + cat1(i)
         end if
       end do

!       do i = 1,max_Rdr
!         print*, itimeyr,aresid(jres),adepth(jres),i,cat1(i),cat2(i),'categories'
!       end do

!step 3: remove the footage
        do i = 1,max_rdr
          if(cat2(i).eq.0) ftgdist(i) = ftgdisto
          if(i.eq.1) then
            if(cat2(i).gt.0) then
              ftgused(i) = min(ftgdisto,rigs_Class(aregion(jres),i))
              ftgdist(i) = ftgdisto-ftgused(i)
              if(ftgdist(i).lt.0.0) ftgdist(i) = 0.0
            end if
          else
            if(cat2(i).gt.0) then
              ftgused(i) = min(ftgdist(i-1),rigs_Class(aregion(jres),i))
              ftgdist(i) = ftgdist(i-1)-ftgused(i)
              if(ftgdist(i).lt.0.0) ftgdist(i) = 0.0
            end if
          end if
!          if (itimeyr.eq.2) print*, itimeyr,aresid(jres),i,cat2(i),useftg,ftgused(i),ftgdist(i),rigs_class(aregion(jres),i),'dist'
        end do

!step 4: recalculate the remaining footage
!        print*, ''
        do i = 1,max_rdr
          rigs_used(aregion(jres),i) = rigs_used(aregion(jres),i) + ftgused(i)
          rigs_acct(aregion(jres),i,itimeyr) = rigs_acct(aregion(jres),i,itimeyr) + ftgused(i)
          rigs_class(aregion(jres),i) = rigs_class(aregion(jres),i) - ftgused(i)
          if(rigs_class(aregion(jres),i).lt.0.0) rigs_class(aregion(jres),i) = 0.0

          tftg = 0.0
          do l = i,max_Rdr
            tftg = tftg + ftgused(l)
          end do

          rigs_drill(aregion(jres),i) = max(rigs_drill(aregion(jres),i)-tftg,0.0)
!          if (itimeyr.eq.2) print*, itimeyr,aresid(jres),adepth(jres),i,tftg,rigs_used(aregion(jres),i),rigs_class(aregion(jres),i),rigs_Drill(aregion(jres),i),'aft'
        end do
!        print*, ''

 1882  format (<max_yr>(3x,f16.2))
 1883  format (i2,3x,i3,3x,f16.2)
       end subroutine
