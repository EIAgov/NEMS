        
    subroutine set_sec45_parameters
    implicit none
    
    include 'parametr'
    include 'emmparm'
    
    !WE CAN'T USE THE BRACKET DECLARTION IN EMMPARMS SINCE IT BREAKS PYFILER. HERE WE MANUALLY INITITLIZE
    !REAL, PARAMETER:: SEC45V_CO2_REQ(2,4) = [[0.45,1.0], [1.5,0.334], [2.5,0.25], [4.0,0.2]] 
    
    ! TODO: FUTURE IMPLEMENTATION OF THESE PARAMETERS SHOULD COME FROM A GLOBAL DATA STRUCTURE DEFINED BY THE HMM GROUP

    ! THIS IS THE PAIRS OF THE CO2 LIMITS (KG OF CO2 PER KGS H2) AND CREDIT PERCENTAGE (%) APPLIED
    sec45v_co2_req(1,1) = 0.45
    sec45v_co2_req(2,1) = 1.0
    
    sec45v_co2_req(1,2) = 1.5
    sec45v_co2_req(2,2) = 0.334
    
    sec45v_co2_req(1,3) = 2.5
    sec45v_co2_req(2,3) = 0.25
   
    sec45v_co2_req(1,4) = 4.0
    sec45v_co2_req(2,4) = 0.2
    
    end subroutine
    
    subroutine populate_efd_ecp_aimms_vars
   
    implicit none
    include 'parametr'
    include 'emmparm'
    include 'efpcntrl'
    include 'control'
    include 'ncntrl'
    include 'mpblk'
    include 'qblk'
    include 'uefdout'
    include 'udatout'
    include 'ecp_nuc'
    include 'cdsparms'
    include 'emm_aimms'
    include 'hmmblk'
    
    integer regions, steps, seasons, years
    !  REAL H2SCRV_Q(MNUMCR,H2STEP,3,MNUMYR)     ! Quantity from HMM
	!  REAL H2SCRV_P(MNUMCR,H2STEP,3,MNUMYR)     ! Prices from HMM
    
    ! set prices and quantities to temp vars for emm_aimms common block
    do regions = 1, MNUMCR
        do steps = 1, H2STEP
            do seasons = 1, 3
                ephrts_prices(curiyr, regions, seasons, steps) = H2SCRV_P(regions, steps, seasons, curiyr)
                ephrts_quantity(curiyr, regions, seasons, steps) = H2SCRV_Q(regions, steps, seasons, curiyr)
            enddo
        enddo
    enddo

     
    end subroutine 
    
    
   subroutine get_d_fuel_from_ephrts
   use ephrts_swtiches
   use ephrts_file_unit_numbers
   ! ephrts - this routine is called on the second iteration of the ephrts model. it's purpose is to extract the amount of demand for fuel from the
   !          aimms ephrts model. the amount of electricity is then utilized in the electricity load duration (eld) submodule by adding to the overall system load.
   !          the reason why it is called on the second iteration is because the eld submodule utilizes nerc regions whereas the first ephrts iteration uses census regions
      
   implicit none
   include 'parametr'
   include 'ncntrl'
   include 'emmparm'
   include 'ecpcntl'
   include 'control'
   include 'dsmdimen'
   include 'dsmcaldr'
   include 'dsmnercr'
   include 'dsmsectr'
   include 'qblk'
   include 'indout'
       
   ! ephrts (aimms) output vars
   real d_fuel(4*25*24), QELIN_ADDH2
   integer seasons(4*25*24),regions(4*25*24),hours(4*25*24), irg
   logical file_exists, e_debug_exist
   ! 864 vars
   !real h2_elec_consumed(25,3*24*12)
       
   ! temp indexes
   integer i, r, h, d, m, j, h_864, reason
   character(len=4) year_str
   character*40 nemspyenv
   
   ! temp indexes
   integer temp,t_index,t_r,t_m,t_d,t_h,t_season,t_daytq,t_totaldays,t_seasons,t_regions,t_hours
   real t_dfuel,t_h2_elec_consumed,t_h2_mwh_consumed,t_annual_total
   
   call rtostring('NEMSPYENV',nemspyenv)

   write(year_str,'(I4)') curiyr + 1989
   
   !1. Execute python script to generate df_HmmTimeConversion.csv
   call execute_command_line (trim(nemspyenv)//"\\Scripts\\python.exe ephrts\\src\\HmmTimeConversion.py "//year_str)
   
   !2. read in file and set variables
  ! h2_mwh_consumed = 0.0
   t_annual_total_r = 0.0
   inquire(file=".\ephrts_debug\df_HmmTimeConversion_"//year_str//".csv", exist=file_exists)
   !
   if ( file_exists .eq. .true.) then
      open(unit=unit_num_ephrts_dfuel, file=".\ephrts_debug\df_HmmTimeConversion_"//year_str//".csv", status='unknown')
      read(unit_num_ephrts_dfuel,*) ! skip header of file
      do
         read(unit_num_ephrts_dfuel,*,iostat=reason) temp,t_index,t_r,t_m,t_d,t_h,t_season,t_daytq,t_totaldays,t_seasons,t_regions,t_hours,t_dfuel,t_h2_elec_consumed,t_h2_mwh_consumed,t_annual_total

         !write(*,*) 'hmmld',curiyr,temp,t_index,t_r,t_m,t_d,t_h,t_season,t_daytq,t_totaldays,t_seasons,t_regions,t_hours,t_dfuel,t_h2_elec_consumed,t_h2_mwh_consumed,t_annual_total

         ! skip if empty
         if (t_r .gt. 0.0) then
             
             ! fill vars from converted file
             
             ! 031124 - We are adding the variables to themselves here while reading in  from a csv file because we 
             ! want to run both HMM and Ephrts at the same time. How this works is that if running just HMM and no Ephrts
             ! we just use the receive_from_hmm routine an populate these variables with a restart file variable (QELHMHR) and
             ! the code below is not active.
             ! The more complex use case is, if HMM and EPHRTS are both on, we give HMM zero demand for hydrogen but they are allowed to purchased 
             ! our electiricty and give us a load profile for other end-users that require hydrogen but consume electricity. So the work around here
             ! Is that we are populating the laod profile using the receivne from hmm routine and then adding it to itself below with the additional
             ! load from EPHRTS. 
             ! 
             annual_h2_mwh_consumed(t_r) = annual_h2_mwh_consumed(t_r) + t_h2_mwh_consumed !total annual mwh consumed from hydrogen prodcution by region added to syload
             h2_elec_consumed(t_r,t_index) = h2_elec_consumed(t_r,t_index) + t_h2_elec_consumed
             h2_mwh_consumed(t_r,t_index) = h2_mwh_consumed(t_r,t_index) +t_h2_mwh_consumed  
             !debug
             t_annual_total_r(t_r) = t_annual_total_r(t_r) + t_h2_mwh_consumed 
             
             !write(*,*) annual_h2_mwh_consumed(t_r), h2_elec_consumed(t_r,t_index),h2_mwh_consumed(t_r,t_index),t_annual_total_r(t_r)
             
         end if
         
         if (reason > 0)  then
            write(*, *) "warning - possible corrupted file when reading in dfuel.csv"
			close(unit_num_ephrts_dfuel) ! close d_fuel
            exit
         else if (reason < 0) then
            close(unit_num_ephrts_dfuel) ! close d_fuel
            exit
         endif
      
      end do
   else
      write(*,*) "error: file df_HmmTimeConversion.csv does not exists"
      stop
   end if
   ! For ephrts only, we populate the QELIN_ADDH2 using Annual_H2_MWH_Consumed and feed it into QELIN
   !       REAL QELINH2E(MNUMCR,MNUMYR)    !   Industrial hydrogen electricity

   DO IRG = 1,nCENSUSreg        
		CALL DSMCENV(t_annual_total_r,IRG,SEC(IND),QELIN_ADDH2)  !This routine is mapping census to nerc regions
        QELINH2E(IRG,CURIYR) = QELINH2E(IRG,CURIYR) + QELIN_ADDH2 * (3.41214 / 1000000.0) ! checked units, converting mwh to tbtu (same as QELINP)                       
    END DO

   
   end subroutine
    
   
   subroutine run_ephrts(year_str,pass_itr,start_year) 
   ! this subroutine calls the python pre-processor, python launcher, and python post-processor for the ephrts run. 
   ! it is called differently depending on the ephrts iteration, if iteration = 1 then regionality is census 
   ! if iteration = 2, then the regional is in nerc. also the post processor that computes the hydrogen supply 
   ! curves is only called the first iteration, it is not called on the second iteration. this is because the first iteartion's 
   ! goal is to compute the supply curves in census regions and the second iterations goal is to compute the fuel demand for the 
   ! electricity load duration submodule, that is adding the additional electricity load to the grid. 
   
   implicit none
   include 'parametr'
   include 'ncntrl'
   logical :: file_exists, debug
   character(len=4) year_str, start_year_str
   character(len=1) itr_str,pass_str, current_run_char
   integer pass_itr,start_year
   logical post_processor_flag
   character(80) aimmsloc*66
   character*40 nemspyenv
   integer*4 cpu_time,wall_time
   
   ! set sec45 parameters
   call set_sec45_parameters

   ! fetch aimms and python paths from scedes files
   call rtostring('AIMMSLOC',aimmsloc) !  E.G., 'C:\AIMMS_INSTALLATION_FREE_RELEASES\4.8.1.299-X64'
   call rtostring('NEMSPYENV',nemspyenv)
    
   ! cast argument parameters to strings
   write(pass_str,'(I1)') pass_itr
   write(start_year_str,'(I4)') start_year
   write(current_run_char,'(I1)') curirun

   post_processor_flag = .true. ! used to turn off the post processor for fixed supply runs for debugging only, set fixed supply in hydrogen data file

   call mptim3(cpu_time,wall_time)
   write(*,*) "before calling EphrtsWrapper.py", cpu_time/100, wall_time/100
   ! the runephrts batch file is dynamically written depending on the ephrts iteration, it will activate the post processor or not.
   call execute_command_line (trim(nemspyenv)//"\\Scripts\\python.exe ephrts\\src\\EphrtsWrapper.py "//year_str//" "//pass_str//" "//start_year_str//" "//trim(aimmsloc))
   
   call mptim3(cpu_time,wall_time)
   write(*,*) "after calling EphrtsWrapper.py", cpu_time/100, wall_time/100
   
   call execute_command_line ("copy ephrts\\output\\D_Fuel.csv "//"ephrts_debug\\D_Fuel_"//year_str//"_"//pass_str//".csv")
    
   debug = .true.

   if (debug .eq. .true.) then
    
   call mptim3(cpu_time,wall_time)
   write(*,*) "before copy all debug files", cpu_time/100, wall_time/100
    
       call execute_command_line ("copy ephrts\\debug.data "//"ephrts_debug\\hydrogen_data_"//"EPHRTS_"//year_str//"_"//pass_str//".data")
       call execute_command_line ("copy input\\hydrogen_data.csv "//"ephrts_debug\\hydrogen_data_"//current_run_char//"_"//year_str//"_"//pass_str//".csv")
       call execute_command_line ("copy ephrts\\debug.data "//"ephrts_debug\\hydrogen_data_"//"EPHRTS_"//year_str//"_"//pass_str//".data")
       call execute_command_line ("copy ephrts\\output\\h2_production_capacity_nxtyr.csv "//"ephrts_debug\\h2_production_capacity_nxtyr"//year_str//"_"//pass_str//".csv")
       call execute_command_line ("copy ephrts\\output\\SCAPMAX_nxtyr.csv "//"ephrts_debug\\SCAPMAX_nxtyr"//year_str//"_"//pass_str//".csv")
       call execute_command_line ("copy ephrts\\input\\fuel_costs.txt "//"ephrts_debug\\fuel_costs"//year_str//"_"//pass_str//".txt")
       call execute_command_line ("copy ephrts\\src\\sec45v_electricity_price_reducer.csv "//"ephrts_debug\\sec45v_electricity_price_reducer_"//year_str//".csv")
   
   call mptim3(cpu_time,wall_time)
   write(*,*) "done copying all debug files", cpu_time/100, wall_time/100
     
   end if
   end subroutine
   
   
   subroutine read_ephrts_supply_curve_for_efp(year)
   ! This routine is used to prepare data to eventually compute UPFUEL for EFP (Electricity Fuel Price Module). How it works is it that it reads the supply curve data file output 
   ! by ephrts and places them into different multi-dimensional arrays for the supply and prices. Then it aggregates the supply so that it is cumulative per step. 
   ! In another routine, the supply 
   use ephrts_file_unit_numbers
   use ephrts_swtiches

   implicit none
   include 'parametr'
   include 'ncntrl'
   include 'qblk'
   include 'mpblk'
   include 'emmparm'
   include 'bildout'
   include 'emission'
   include 'uefpout'     ! UEFP output variables
   include 'uefdout'     ! EFD output variables
   include 'udatout'     ! UDAT output variables
   include 'uecpout'     ! UECP output variables
   include 'uldsmout'    ! LDSM output variables
   include 'uettout'     ! UETT output variables
   include 'ecpcntl'
   include 'control'
   include 'entcntl'
   include 'dispuse'
   include 'elout'
   include 'dsmdimen'
   include 'dsmcaldr'
   
   character(len=4) year_str
   integer year, prior_year, region, season_slice, i, j, k, reason
   logical :: file_exists
   character(len=1) itr_str,pass_str, current_run_char

   integer regions, season_slices, steps, total_steps
   real h2_consumption, h2_prices, counter 
   
   total_steps = 10
   
   ! to compute the marginal price, we use the prior year's data. The current year's data could be used but we would need to figure out
   ! how in elcost subroutine in efd, the upfuel (fuel costs) is aggregated while the sqfuel (consumption) at the same time. To avoid that
   ! complexity we lag the fuel costs by using the prior year let the upfuel variable flow into elcosts.
   
   write(current_run_char,'(I1)') curirun
   prior_year = year - 1 ! prior year
   write(year_str,'(I4)') prior_year 

   !write(*,*) " file = ", "ephrts_debug\\hydrogen_data_"//current_run_char//"_"//year_str//"_1.csv"
   inquire(file="ephrts_debug\\hydrogen_data_"//current_run_char//"_"//year_str//"_1.csv", exist=file_exists)
   i = 1
   if ( file_exists .eq. .true.) then
      open(unit=unit_num_ephrts_supply, file= "ephrts_debug\\hydrogen_data_"//current_run_char//"_"//year_str//"_1.csv", status='unknown')
      read(unit_num_ephrts_supply,*) ! skip header of file
      do
         read(unit_num_ephrts_supply,*,iostat=reason) regions, season_slices, steps,  h2_consumption, h2_prices

         ephrts_h2_supply(curiyr-1,regions,season_slices, steps) = h2_consumption 
         ephrts_h2_prices(curiyr-1,regions,season_slices, steps) = h2_prices
         
         i = i + 1
         if (reason > 0)  then
                  
            write(*, *) "warning - possible corrupted file when reading in hydrogen data file for ephrts"
			close(unit_num_ephrts_supply) ! close d_fuel
            exit

         else if (reason < 0) then
            close(unit_num_ephrts_supply) 
            exit
      
         !else
         !   ... do normal stuff ...
         end if
      end do
   else
      write(*,*) "error: file d_fuel.csv does not exists"
      stop
   end if
   
   ! now aggregate the consumption of hydrogen so that each step's quantity is cumulative. This is then compared against the consumption from sqfuel.
   do regions = 1, mnumcr
      do season_slices = 1 , efd_d_msp
         counter = 0.0
         do steps = 1, total_steps
            counter = counter + ephrts_h2_supply(curiyr-1, regions, season_slices, steps)
            ephrts_h2_supply(curiyr-1, regions, season_slices, steps) = counter
            !write(*,*) curiyr-1, regions, season_slices, steps, ephrts_h2_supply(curiyr-1, regions, season_slices, steps),  ephrts_h2_prices(curiyr-1, regions, season_slices, steps) 
         end do 
      end do
   end do
   
   ! debug   
   !do i = 1, mnumcr
   !   do j = 1 , efd_d_msp
   !      do k = 1, total_steps
   !         write(*,*) curiyr-1, i, j, k, ephrts_h2_supply(curiyr-1,i,j, k),  ephrts_h2_prices(curiyr-1,i, j, k) 
   !      end do 
   !   end do
   !end do
   
   end subroutine
   
   
   subroutine compute_upfuel_for_efp()
   ! This routine computes the fuel prices (upfuel) for the EFP's hydrogen turbine (UIGC). It does this by comparing the amount of hydrogen from the 
   ! EFD turbine to the cumulative supply. And then selecting the step that satifies the demand from EFD and the price. Then it takes a seasonal weighted average
   ! for a given region. 
   use ephrts_file_unit_numbers
   use ephrts_swtiches
   use, intrinsic :: ieee_arithmetic, only: ieee_is_finite      

   implicit none
   include 'parametr'
   include 'ncntrl'
   include 'qblk'
   include 'mpblk'
   include 'emmparm'
   include 'bildout'
   include 'emission'
   include 'uefpout'     ! UEFP output variables
   include 'uefdout'     ! EFD output variables
   include 'udatout'     ! UDAT output variables
   include 'uecpout'     ! UECP output variables
   include 'uldsmout'    ! LDSM output variables
   include 'uettout'     ! UETT output variables
   include 'ecpcntl'
   include 'control'
   include 'entcntl'
   include 'bildin'
   include 'fuelin'
   include 'dispuse'
   include 'elout'
   include 'dsmdimen'
   include 'dsmcaldr'
   
   integer region, season, step, counter, total_steps
   real weighted_regional_prices, seasonal_price, sqfuel_annual_consumption, lowest_price
   logical file_exist
   
   ! debug file
   if (turn_on_debugging .eq. .true.) then 
      inquire(file="ephrts_debug\\ephrts_efp_fuel_prices.txt", exist=file_exist)
      if (file_exist) then
         open(unit_num_ephrts_efp_fuel_prices, file="ephrts_debug\\ephrts_efp_fuel_prices.txt", status="old", position="append", action="write")
      else
         open(unit_num_ephrts_efp_fuel_prices, file="ephrts_debug\\ephrts_efp_fuel_prices.txt", status="new", action="write")
      end if
      
      ! write to debug if on
	  
      write(unit_num_ephrts_efp_fuel_prices,*) 'computed_step', 'weighted_regional_prices', 'curiyr - 1', 'region', 'season', 'step', 'sqfuel_yr_for_efp', 'ephrts_h2_supply', 'ephrts_h2_prices', 'sqfuel_annual_consumption'
   
   end if
   
   total_steps = 10 ! total_steps is adjustable must be greater than the steps computed from the ephrts python post processor
   
   do region = 1, mnumcr ! mnumcr = 11
   
      weighted_regional_prices = 0.0 ! initialized for each region, then computed over the seasons
      lowest_price = 999.9 ! used to compute lowest price if inifinity found due to zero consumption in efd sqfuel, programatically lowers this value to the lowest steps and applies it across the region
      upfuel(uigc,region) = 0.0
      
      ! get annual consumption per season, this is used later in the denomintor to comptue the seasonal weighted average for a given region
      sqfuel_annual_consumption = 0.0
      do season = 1 , efd_d_msp
         sqfuel_annual_consumption = sqfuel_annual_consumption + sqfuel_yr_for_efp(curiyr - 1,uigc,region,season)
      end do
      
      do season = 1 , efd_d_msp ! efd_d_msp = 3
         
         ! iterate over step and break when the amount of hydrogen from the turbine in efd (sqfuel) is 1 step larger than the ephrts h2 supply 
         !(e.g. efd wants 30 kgs, ephrts supply gives a price for 50 kgs assuming this is the cheapest step to meet the demand)
         
         do step = 1, total_steps ! generic placholder for steps. EFD does not handle more than 10 steps very well without convergence issues.
            ! debug
            !write(*,*) curiyr - 1, region, season, step, sqfuel_yr_for_efp(curiyr - 1, uigc, region, season), ephrts_h2_supply(curiyr - 1, region, season, step),  ephrts_h2_prices(curiyr - 1, region, season, step)

            ! use the last year's sqfuel price since the ephrts supply curve is computed using the last year as well
            if (ephrts_h2_supply(curiyr - 1, region, season, step) .gt. sqfuel_yr_for_efp(curiyr - 1, uigc, region, season)) then
               
               ! write out to debug file
               if (turn_on_debugging .eq. .true.) then 
                  write(unit_num_ephrts_efp_fuel_prices,*) 'selected_step', curiyr - 1, region, season, step, sqfuel_yr_for_efp(curiyr - 1, uigc, region, season), ephrts_h2_supply(curiyr - 1, region, season, step),  ephrts_h2_prices(curiyr - 1, region, season, step), sqfuel_annual_consumption
               end if
               
               ! compute lowest price, used if no consumption found from efd instead of weighted regional price if inifitiy encountered.
               if (lowest_price .gt. ephrts_h2_prices(curiyr - 1,region,season, step)) then 
                  lowest_price = ephrts_h2_prices(curiyr - 1,region,season, step)
               end if
               
               exit ! exit loop in order to proceed with step
            
            ! check if the consumption of the turbine from efd is less than the supply in the first step, if so, then exit the loop and use the first step's price   
            else if (sqfuel_yr_for_efp(curiyr - 1, uigc, region, season) .le. ephrts_h2_supply(curiyr - 1, region, season, 1)) then
               
               ! write out to debug file
               if (turn_on_debugging .eq. .true.) then 
                  write(unit_num_ephrts_efp_fuel_prices,*) 'selected_step', curiyr - 1, region, season, step, sqfuel_yr_for_efp(curiyr - 1, uigc, region, season), ephrts_h2_supply(curiyr - 1, region, season, step),  ephrts_h2_prices(curiyr - 1, region, season, step), sqfuel_annual_consumption
               end if    
               
               ! compute lowest price, used if no consumption found from efd instead of weighted regional price if inifitiy encountered.
               if (lowest_price .gt. ephrts_h2_prices(curiyr - 1,region,season, step)) then 
                  lowest_price = ephrts_h2_prices(curiyr - 1,region,season, step) ! use the lowest price, step will be equal to 1 since there's an exit after this and it's trapped in the conditional if statement step = 1
               end if
               
               exit ! exit loop in order to proceed with step
            end if   
         end do 
         
         ! once do loop above exits it should retain step found during if condition, compute weighted average using quantity/annual amount times price
         if (step .le. total_steps) then
            ! this if statement guards againsts the example where there could be a region with zero fuel consumption from efd but supply from ephrts. It prevents it from adding the weighted price for that zero efd fuel step since the annual sqfuel consumption was not updated in this case leading to a higher price
            ! in practical terms, this just means that it will take the weighed average of two regions instead of three.
            if (sqfuel_yr_for_efp(curiyr - 1, uigc, region, season) .gt. 0.0) then
               
               ! compute weighted regional price using the selected ephrts h2 price for a given step that has enough quantity to satifsy the demand from efd's sqfuel turbine consumption for a given season.
               weighted_regional_prices = weighted_regional_prices + ephrts_h2_prices(curiyr - 1,region,season, step) * ( sqfuel_yr_for_efp(curiyr - 1, uigc, region, season) /sqfuel_annual_consumption)
               
               ! write to debug if on
               if (turn_on_debugging .eq. .true.) then 
				       WRITE(unit_num_ephrts_efp_fuel_prices,4181) CURIRUN,  curiyr - 1, region, season, step, sqfuel_yr_for_efp(curiyr - 1, uigc, region, season), ephrts_h2_supply(curiyr - 1, region, season, step),  ephrts_h2_prices(curiyr - 1, region, season, step), sqfuel_annual_consumption, weighted_regional_prices
 4181                 FORMAT(1X,"computed_step",5(",",I5),5(",",F21.6))
                  !write(unit_num_ephrts_efp_fuel_prices,'(a)') 'computed_step', weighted_regional_prices, curiyr - 1, region, season, step, sqfuel_yr_for_efp(curiyr - 1, uigc, region, season), ephrts_h2_supply(curiyr - 1, region, season, step),  ephrts_h2_prices(curiyr - 1, region, season, step), sqfuel_annual_consumption
               end if
            end if
         end if
 
      end do
      
      ! This code assigns regional values to upfuel by just using the regional weighted price or the lowest price found
      if (ieee_is_finite(weighted_regional_prices) .and. weighted_regional_prices .gt. 0.0) then
         ! use weighted regional price if the number is finite and above zero
         upfuel(uigc,region) = weighted_regional_prices
         
         ! write to debug
         if (turn_on_debugging .eq. .true.) then 
            write(unit_num_ephrts_efp_fuel_prices,*) 'using weighted regional price', weighted_regional_prices
         end if
      else
         ! if inifinity detected or zero then use lowest price across steps, this means there's likely zero consumption in efd for those seasons.
         upfuel(uigc,region) = lowest_price
         
         ! write to debug
         if (turn_on_debugging .eq. .true.) then 
            write(unit_num_ephrts_efp_fuel_prices,*) 'infinity detected or zero, skipping', weighted_regional_prices, " use lowest price instead ", lowest_price
         end if
      end if         
      
   end do
   
   ! debug
   if (turn_on_debugging .eq. .true.) then 
      write(unit_num_ephrts_efp_fuel_prices,*) "region, upfuel" 
      do region = 1, mnumcr
         write(unit_num_ephrts_efp_fuel_prices, *) 'efp_price', region, upfuel(uigc,region)
      end do
      close(unit_num_ephrts_efp_fuel_prices)
   end if      
   end subroutine
    
   
   subroutine read_ephrts_minimum_demand_threshold
   use ephrts_file_unit_numbers
   ! in order to kickstart the hydrogen module, there needs to be a minimum demand that is input exgenously. note, 
   ! although this demand flows through ephrts and generates a hydrogen supply curve for efd and ecp, it is efd/ecp that 
   ! utilimately decide whether or not to build additional capacity or dispatch fuel. 
   
   implicit none
   
   include 'parametr'
   include 'ncntrl'
   include 'emmparm'
   include 'ecpcntl'
   include 'control'
   include 'dsmdimen'
   include 'dsmcaldr'
      
   integer i,j   
   integer region, season
   real min_h2_demand
   
   minimum_h2_demand_threshold = 0.0 ! zero out before reading
   open(unit=unit_num_ephrts_minimum_demand, file= "ephrts\\input\\minimum_demand_threshold.txt", status='old')

   read(unit_num_ephrts_minimum_demand,*) ! skip header
   read(unit_num_ephrts_minimum_demand,*) ! skip header
   
   do i = 1, 4*11
      read(unit_num_ephrts_minimum_demand,*) region, season, min_h2_demand
      minimum_h2_demand_threshold(region,season) = min_h2_demand
   enddo
   
   close(unit_num_ephrts_minimum_demand)
   
   end subroutine
   
   
   subroutine write_fixed_cost_factors()
   use ephrts_file_unit_numbers
   ! this routine fetches the fixed costs factor for the hydrogen technology type from ecp for ephrts, outputs to a file used later in the ephrts preprocessor, model and post processor for debt planning.
   implicit none
   include 'parametr'
   include 'ncntrl'
   include 'qblk'
   include 'mpblk'
   include 'emmparm'
   include 'bildout'
   include 'ecpcntl'
   logical :: file_exists
   real eplvfcf_output
   
   inquire(file="ephrts\\input\\ephrts_costs_factor.txt", exist=file_exists)
   if (file_exists) then
      open(unit_num_ephrts_fixed_cost_factors, file="ephrts\\input\\ephrts_costs_factor.txt", status="replace", action="write")
      open(unit_num_ephrts_fixed_cost_factors_yearly, file="ephrts\\input\\ephrts_costs_factor_yearly.txt", status="old", position="append", action="write") ! debug file
   else
      open(unit_num_ephrts_fixed_cost_factors, file="ephrts\\input\\ephrts_costs_factor.txt", status="new", action="write")
      open(unit_num_ephrts_fixed_cost_factors_yearly, file="ephrts\\input\\ephrts_costs_factor_yearly.txt", status="new", action="write") ! debug file
   end if
   
   ! fetch eplvfcf from build out, the idea is we fetch fixed cost factors from ecp used to compute annual project finances
   eplvfcf_output = eplvfcf(wiic,2) ! note - we investigated the cost factors for each region and found they are the same - around 0.07, but change slightly year to year. see the ecp_levlized_cost table in emmdb.

   ! eplvfcf float check ! todo - add to restart file?
   if (eplvfcf_output .eq. 0.0) then
      eplvfcf_output = 0.069476612 ! this value is taken from the emm.db and is the average ecp_levlized_cost table in emmdb for the hydrogen turbine. this should programtically update year, however on the first pass through ecp the value may be zero if not initialized in ecp.
      write(*,*) "warning eplvfcf is zero setting value to last known value ", eplvfcf_output
   end if
         
   ! write into input file, note ephrts_debug folder does not exists on first call place into input folder
   write(unit_num_ephrts_fixed_cost_factors, *) 'eplvfcf' ! header
   write(unit_num_ephrts_fixed_cost_factors,*)  eplvfcf_output ! value
   
   ! write into debug file
   write(unit_num_ephrts_fixed_cost_factors_yearly, *) curiyr+1989, eplvfcf_output ! yearly cost factor appended to debug file
         
   close(unit_num_ephrts_fixed_cost_factors) ! input file read into preprocessor, ephrts.aims. and post processor
   close(unit_num_ephrts_fixed_cost_factors_yearly) ! debug file yearly
      
   end subroutine 
    
    
   subroutine write_ephrts_run_bat_input_file(year_str,current_year, pass_itr)
   ! this creates the first half of the cmd file where the rest is populated with the python preprocessor. 
   ! the cmd txt file is fed into the aimscmd.exe in order to run aimms projects programatically. 
   ! in energy model terms however, this routine is complex. it behaves differently depending on the iteration that is past into it. 
   ! iteration 0 and 1: 
   !  in this iteration the demand for the fuel is taken from efd's sqfuel variable in census regions, converted over to ephrts timeslices, converted from bbtus to kgs using the heat rate 
   ! for input into the ephrts model via the cmd file. if there is no demand present from efd, it will assert the minimum demand threshold as an input to ephrts. also, there
   ! is a multipiler that will increase the demand year over year in order to spur growth. remember, the dispatch of fuel is from efd, efd will decide how much to dispatch.
   ! iteration : 2:
   ! in this iteration, the fuel is in nerc regions and the idea here is that we want to find the actual fuel demand without the multipler or minimum demand multipler
   ! in order to feed an accurate fuel demand into the ephrts model. this represents the actual amount of additional electricity load that is placed on the grid. 
   ! later in a separate routine we get the fuel demand from the ephrts models output folder and feed it into the electricity load duration submodule.
   
   use ephrts_swtiches
   use ephrts_file_unit_numbers
   
   implicit none
   include 'parametr'
   include 'ncntrl'
   include 'qblk'
   include 'mpblk'
   include 'emmparm'
   include 'bildout'
   include 'emission'
   include 'uefpout'     ! UEFP output variables
   include 'uefdout'     ! EFD output variables
   include 'udatout'     ! UDAT output variables
   include 'uecpout'     ! UECP output variables
   include 'uldsmout'    ! LDSM output variables
   include 'uettout'     ! UETT output variables
   include 'ecpcntl'
   include 'control'
   include 'entcntl'
   include 'dispuse'
   include 'elout'
   include 'dsmdimen'
   include 'dsmcaldr'
   include 'convfact'

   logical :: file_exists,e_debug_exist
   integer region, current_year,season_slice,last_row, pass_itr, previous_year, emm_season
   character(len=200) :: fmt ! format descriptor
   character(len=4) year_str
   real amt_h2_from_emm, h2_minimum_demand, h2_supply_increment_multiplier
   character(len=1) pass_str
      
   if (turn_on_debugging .eq. .true.) then 
      inquire(file="EPHRTS_DEBUG_FILE.TXT", exist=e_debug_exist)
      if (e_debug_exist) then
         open(unit_num_ephrts_debug_file, file="EPHRTS_DEBUG_FILE.TXT", status="old", position="append", action="write")
      else
         open(unit_num_ephrts_debug_file, file="EPHRTS_DEBUG_FILE.TXT", status="new", action="write")
      end if
   end if

   write(pass_str,'(I1)') pass_itr

   amt_h2_from_emm = 0.0
    
   ! this changes the amount of supply of hydrogen using the previous year's efd sqfuel h2 consumption (or the minimum default) as a baseline times this multiplier     
   h2_supply_increment_multiplier = 1.2
    
   ! sqfuel fuel is the molecular fuel consumed by turbine used in the first iteration in census
   ! h2_turbine_consumption is the same as squel but in emm regions, both seasonal
   ! convert emm to hmm seasons when writing out hmm results from emm.
   ! emm 1: winter, 2: summer, 3: shoulder 
   ! hmm 1: spring, 2: summer, 3: fall, 4: winter
   ! hmm to emm : 1 to 2, 2 to 3 and divide by 2, 3 to 1, 4 to 3 and divide by 2            
   ! for output: hmm to emm in python post-processing supply algorithm for hydrogen_data 
       
   if (turn_on_debugging .eq. .true.) then 
       write(unit_num_ephrts_debug_file,*) "sqfuel from previous year used in cmd ephrts input before mapping and conversion"
       do region = 1, 9 
           do season_slice = 1, 3
               write(unit_num_ephrts_debug_file,*) region, season_slice, sqfuel(uigc,region,season_slice), curiyr+1988
           enddo
       enddo
       write(unit_num_ephrts_debug_file,*) "end sqfuel from previous year used in cmd ephrts input before mapping and conversion"
   end if
    
   open(unit=unit_num_ephrts_cmd_file, file= "ephrts_debug\\ephrts_cmds_"//year_str//"_"//pass_str//".txt", status='replace')
   write(unit_num_ephrts_cmd_file,'(4a)') "Let iteration :="//pass_str//";" 
   write(unit_num_ephrts_cmd_file,'(4a)') "Let demand_hydrogen(season,region) := List {"
      
   if (pass_itr .eq. 0 .or. pass_itr .eq. 1) then 
      
      ! fetch data from minimum h2 demand input file
      call read_ephrts_minimum_demand_threshold
      
      last_row = 1
         
      if (turn_on_debugging .eq. .true.) then 
         write(unit_num_ephrts_debug_file,*) "season (ephrts), ",  "region (census), ", " previous year's h2 fuel consumption (kg) "
      end if
         
      do season_slice = 1, 4 ! ephrts seasons
          do region = 1, 11 
              amt_h2_from_emm = 0.0
               
              FMT = "(A, I1, A, I1, A, F14.2, A)"

              if (region > 9) then
                  FMT = "(A, I1, A, I2, A, F14.2, A)"
              end if
               
              emm_season = ephrts_to_emm_seasonal_mapping(season_slice) ! returns emm seasons using mappings, defined in dsmcaldr include block
			   
			  ! take largest value of previous year's and current year's sqfuel so amt_h2_from_emm never decreases
			  if (pass_itr .eq. 1) then
			      sqfuel_yr(curiyr,uigc,region,emm_season) = max(sqfuel_yr(curiyr-1,uigc,region,emm_season), sqfuel(uigc,region,emm_season))
                  sqfuel_yr_for_efp(curiyr,uigc,region,emm_season) = sqfuel(uigc,region,emm_season) * 1000.0 * CFH2Q_KG! coverts lp bbtus to kgs where bbtu * 1000.0  = mmbtu * kg/mmbtu =kg 
              else
                  sqfuel_yr(curiyr,uigc,region,emm_season) = sqfuel(uigc,region,emm_season) 
              endif

              ! if there's existing demand in sqfuel, where  sqfuel fuel is the molecular hydrogen fuel consumed by turbine used in the first iteration in census
              if (season_slice .eq. 1 ) then ! spring
                  amt_h2_from_emm =  sqfuel_yr(curiyr,uigc,region,emm_season) / 2.0
              else if ( season_slice .eq. 3) then  ! spring  
                  amt_h2_from_emm =  sqfuel_yr(curiyr,uigc,region,emm_season) / 2.0
              else if (season_slice .eq. 2) then! summer
                  amt_h2_from_emm =  sqfuel_yr(curiyr,uigc,region,emm_season)
              else if (season_slice .eq. 4) then ! winter
                  amt_h2_from_emm =  sqfuel_yr(curiyr,uigc,region,emm_season)
              end if
               
              if (turn_on_debugging .eq. .true.) then 
                  write(unit_num_ephrts_debug_file,*) season_slice, region, amt_h2_from_emm * 1000.0 * CFH2Q_KG
              end if
               
              h2_minimum_demand = minimum_h2_demand_threshold(region, season_slice) ! organized by ehprts seasons
               
              ! this ensures that there's a minimum amount of demand in billion btus
              if (amt_h2_from_emm .le. h2_minimum_demand) then
                  amt_h2_from_emm = h2_minimum_demand
              end if

              ! we do not consider census regions 10 and 11
              if (region > 9) then
                  amt_h2_from_emm = 0.0
              end if
            
              ! we want to convert billion btus (the input from the lp) to then to kgs ( output to the aimms model)
              amt_h2_from_emm = amt_h2_from_emm * 1000.0 * CFH2Q_KG ! coverts lp bbtus to kgs where bbtu * 1000.0  = mmbtu * kg/mmbtu =kg 
               
              if (last_row .lt. 4*11) then
                  write(unit_num_ephrts_cmd_file,fmt) "('", season_slice, "', '",region,"')  : ", h2_supply_increment_multiplier * amt_h2_from_emm  ,"," 
              else if (last_row .eq. 4*11) then 
                  write(unit_num_ephrts_cmd_file,fmt) "('", season_slice,"', '",region,"')  : ", h2_supply_increment_multiplier * amt_h2_from_emm
              end if
               
              last_row = last_row + 1
          enddo
      enddo
   else if (pass_itr .eq. 2) then 
       last_row = 1
         
       if (turn_on_debugging .eq. .true.) then
           write(unit_num_ephrts_debug_file,*) "start h2 turbine consumption in emm seasons no unit conversions"
           do region = 1, 25 
               do season_slice = 1, 3
                   write(unit_num_ephrts_debug_file,*) region, season_slice, h2_turbine_consumption((curiyr+1989) - 1, season_slice, region)
               enddo
            enddo
            write(unit_num_ephrts_debug_file,*) "end h2 turbine consumption in emm seasons no unit conversions"
       end if
         
       do season_slice = 1, 4
           do region = 1, 25 
               amt_h2_from_emm = 0.0
            
               FMT = "(A, I1, A, I1, A, F14.2, A)"

               if (region > 9) then
                   FMT = "(A, I1, A, I2, A, F14.2, A)"
               end if
            
               previous_year = (curiyr+1989) - 1
               
               emm_season = ephrts_to_emm_seasonal_mapping(season_slice) ! returns emm seasons using mappings, defined in dsmcaldr include block
               
               if (turn_on_debugging .eq. .true.) then
                  write(unit_num_ephrts_debug_file,*) region, season_slice, emm_season, h2_turbine_consumption(previous_year, emm_season, region)
               end if
               
               if (season_slice .eq. 1) then ! spring
                  amt_h2_from_emm = h2_turbine_consumption(previous_year, emm_season, region) / 2.0
               elseif (season_slice .eq. 3) then   ! and fall 
                  amt_h2_from_emm = h2_turbine_consumption(previous_year, emm_season, region) / 2.0
               else if (season_slice .eq. 2) then  ! summer
                  amt_h2_from_emm = h2_turbine_consumption(previous_year, emm_season, region)
               else if (season_slice .eq. 4) then ! winter
                  amt_h2_from_emm = h2_turbine_consumption(previous_year, emm_season, region)
               end if
               
               if (turn_on_debugging .eq. .true.) then
                  write(unit_num_ephrts_debug_file,*) region, season_slice, amt_h2_from_emm
               end if
               amt_h2_from_emm = amt_h2_from_emm * 1000.0 * CFH2Q_KG ! coverts lp bbtus to kgs where bbtu * 1000.0  = mmbtu * kg/mmbtu =kg

               ! we want to convert billion btus (the input from the lp) to then to kgs ( output to the aimms model) 
               if (last_row .lt. 4*25) then
                  write(unit_num_ephrts_cmd_file,fmt) "('", season_slice, "', '",region,"')  : ",amt_h2_from_emm  ,","  ! the conversion is 1000000000.0/ ((51628.0 * 2.20462)) ! bbtus to kgs where 51628 btu/lb times 2.20462 lbs/kgs where 1 bbtu is 1,000,000,000 btus
               else if (last_row .eq. 4*25) then 
                  write(unit_num_ephrts_cmd_file,fmt) "('", season_slice,"', '",region,"')  : ", amt_h2_from_emm
               end if
               
               last_row = last_row + 1
               
           enddo
       enddo
   end if

   write(unit_num_ephrts_cmd_file,'(4a)') "};"
   close(unit_num_ephrts_cmd_file)
   if (turn_on_debugging .eq. .true.) then
       write(unit_num_ephrts_debug_file,*) ''
       close(unit_num_ephrts_debug_file)
   end if
   end subroutine
    
        
   subroutine read_ephrts_log(error, year_str,itr_str)
   use ephrts_file_unit_numbers
   ! edt <edward.thomas@eia.gov> 07/20/2021
   ! this subroutine checks the hmm error log produced by the "aimms cmd tool" called in hmmrun.bat
   ! which then outputs the results to the hmm_error_log.txt. this goes to line 5 of the error file
   ! and checks to see if returns something other than '0', if so switch the error bool to true
   ! this routine reads the ephrts aimms log file, checks for any errors and crashes nems if error present
   implicit none
   integer i
   character(len=30) :: lineformat
   logical error
   character(len=4) year_str
   character(len=1) itr_str
       
   error = .true.
    
   open(unit=unit_num_ephrts_error_log, file= "ephrts_debug\\ephrts_error_log-"//year_str//"-"//itr_str//".txt", status='unknown')
   do i = 1, 20
      read(unit_num_ephrts_error_log,'(A)', end=99) lineformat
            
      if (lineformat(5:20) .eq. 'Return value = 0') then
         error = .false.
         end if
      end do  
   close(unit_num_ephrts_error_log)
99 continue
   end subroutine
    
   ! todo: do not modify until new common block structure is put in place
   !SUBROUTINE READ_EPHRTS_NEMS_PARAMETERS()
   !! THIS ROUTINE WILL NEED TO BE REPLACED WITH THE GLOBAL H2 MARKET DATA STRUCTURE IN THE FUTURE. 
   !! FOR NOW, WE USE A SIMPLE IMPLEMENTATION THAT READS DATA THROUGH A TXT FILE.
   !IMPLICIT NONE
   !
   !include 'parametr'
   !include 'ncntrl'
   !include 'qblk'
   !include 'mpblk'
   !include 'emmparm'
   !
   !INTEGER I
   !INTEGER*4 CPU_TIME,WALL_TIME
   !
   !OPEN(UNIT=23423, FILE= ".\ephrts\inputs\ephrts_nems_parameters.txt", STATUS='OLD')
   !   
   !!READ(23423,*) ! SKIP HEADER OF FILE
   !READ(23423,*) ! SKIP LINE
   !!READ(23423,*) SEC45V_H2_PTC ! 
   !READ(23423,*) ! SKIP LINE
   !!READ(23423,*) WAGE_APP_MULTIPLIER
   !READ(23423,*) ! SKIP LINE
   !! READ MULTI ARRAY SEC45V_CO2_REQ
   !DO I = 1, 4
   !!   READ(23423,*) SEC45V_CO2_REQ(1,I), SEC45V_CO2_REQ(2,I)
   !END DO
   !! READ IN THE ELECTROLYZER_EFFICIENCY
   !READ(23423,*) ! SKIP LINE
   !!READ(23423,*) ELECTROLYZER_EFFICIENCY
   !
   !CLOSE(23423)
   !
   !CALL MPTIM3(CPU_TIME,WALL_TIME)
   !WRITE(*,*) "debug output before READ_EPHRTS_NEMS_PARAMETERS - ", CPU_TIME/100, WALL_TIME/100
   !
   ! DEBUG 
   !WRITE(*,*) 'SEC45V_H2_PTC', SEC45V_H2_PTC
   !WRITE(*,*) 'WAGE_APP_MULTIPLIER', WAGE_APP_MULTIPLIER
   !DO I = 1, 4
   !   READ(23423,*) SEC45V_CO2_REQ(1,I), SEC45V_CO2_REQ(2,I)
   !END DO
   !! READ IN THE ELECTROLYZER_EFFICIENCY
   !READ(23423,*) ! SKIP LINE
   !READ(23423,*) ELECTROLYZER_EFFICIENCY
   !
   !CLOSE(23423)
   !
   !CALL MPTIM3(CPU_TIME,WALL_TIME)
   !WRITE(*,*) "debug output before READ_EPHRTS_NEMS_PARAMETERS - ", CPU_TIME/100, WALL_TIME/100
   !
   !! DEBUG 
   !!WRITE(*,*) 'SEC45V_H2_PTC', SEC45V_H2_PTC
   !!WRITE(*,*) 'WAGE_APP_MULTIPLIER', WAGE_APP_MULTIPLIER
   !!DO I = 1, 4
   !!   WRITE(*,*) SEC45V_CO2_REQ(1,I), SEC45V_CO2_REQ(2,I)
   !!END DO
   !!WRITE(*,*) ELECTROLYZER_EFFICIENCY
   !!
   !!CALL MPTIM3(CPU_TIME,WALL_TIME)
   !!WRITE(*,*) "debug output after READ_EPHRTS_NEMS_PARAMETERS - ", CPU_TIME/100, WALL_TIME/100
   !
   !END SUBROUTINE
   !
    
    
   subroutine write_sec45v_electricity_price_reducer(electricity_price_reducer)
   use ephrts_file_unit_numbers
   ! this routine writes out the results of the price reducer for sec45v into a separate file where it is then 
   ! read in by the ephrts python pre-processor.
   implicit none
   include 'parametr'
   include 'ncntrl'
   include 'qblk'
   include 'mpblk'
   include 'emmparm'
   include 'emission'
   include 'uefpout'     ! uefp output variables
   include 'uefdout'     ! efd output variables
   include 'udatout'     ! udat output variables
   include 'uecpout'     ! uecp output variables
   include 'uldsmout'    ! ldsm output variables
   include 'uettout'     ! uett output variables
   include 'ecpcntl'
   include 'control'
   include 'entcntl'
   include 'dispuse'
   include 'elout'
   include 'dsmdimen'
   include 'dsmcaldr'

   real electricity_price_reducer(mnumnr, months_ephrts, hours_ephrts)
   logical file_exist
   integer i,j,k
   
   open(unit_num_ephrts_price_reducer, file="ephrts\\src\\sec45v_electricity_price_reducer.csv", status='replace', action="write") ! todo: integration adds to file manager system
   
   write(unit_num_ephrts_price_reducer, *) "NERC_RGNS, MONTHS_EPHRTS, HOURS_EPHRTS, PRICE_REDUCER"
   write(*,*) "writing sec45v price reducer file"
   do i = 1, mnumnr
      do j = 1, months_ephrts
         do k = 1, hours_ephrts
            write(unit_num_ephrts_price_reducer, *), i, ",", j, ",", k, ",", electricity_price_reducer(i, j, k)
         end do
      end do
   end do
    
   close(unit_num_ephrts_price_reducer) 
   
   end subroutine
    
    
   subroutine get_efd_to_restore_tech_types(mapping)
   use ephrts_file_unit_numbers
   !
   ! this subroutine simply fetches the mapping between efd and restore technology types using a flat file
   !
   
   implicit none
   include'parametr'
   include'ncntrl'
   include'emmparm'
   include'control'
   include'ecpcntl'

   integer mapping(efd_d_dsp), i, efd_index, restore_index ! given the efd technology index, return the corresponding restore index
   logical file_exist
   
   inquire(file="rest\\input\\efd-restore-mapping.csv", exist=file_exist) ! todo: integration adds this path to the file manager, along with unit numbers
   if (file_exist) then
      open(unit_num_ephrts_price_restore_mapping, file="rest\\input\\efd-restore-mapping.csv", status="old", action="read")
   else
      write(*,*) "crash nems - mapping from efd to restore does not exist within the ephrts model, see util.f subroutine get_efd_to_restore_tech_types" 
      stop
   end if
   
   read(unit_num_ephrts_price_restore_mapping, *) ! skip header 
   
   do i = 1, efd_d_dsp
      read(unit_num_ephrts_price_restore_mapping, *) efd_index, restore_index
      mapping(efd_index) = restore_index
   end do
    
   close(unit_num_ephrts_price_restore_mapping) 
   
   end subroutine
    
    
   subroutine sec45v_calculate_emissions(electricity_price_reducer)
   ! this is the sec45v implementation for ephrts, specifically the data preparation portion. what the algorithm does
   ! is take the hourly dispatch from restore along with the amount of co2 from each efd plant. it then computes a ratio
   ! of each and depending on the ratio and the sec45 carbon limit requirments a price reducer is computed by applying tax credits
   ! that price reducer is written out for input into ephrts. 
   
   use hourly_restore_data
   
   implicit none
   
   include'parametr'
   include'ncntrl'
   include'emmparm'
   include'control'
   include'ecpcntl'
   include'dispin'
   include'dispout'
   include'dispuse'
   include'dispcrv'
   include'fuelin'
   include'bildin'
   include'dispett'
   include'uettout'
   include'postpr'
   include'dsmdimen'
   include'dsmsectr'
   include'dsmunits'
   include'dsmtoefd'
   include'dsmcaldr'
   include'elcntl'
   include'elout'
   !include'ecpcntl'
   include'plntctl'
   include'emission'
   include'efdin'
   include 'macout'
   
   real*4 electricity_price_reducer(mnumnr, months_ephrts, hours_ephrts), co2byrestoreplant(mnumnr,maxptypesp), co2_per_mwhr(mnumnr, months_ephrts, hours_ephrts)
   real*4 total_co2_from_electricity_dispatch(mnumnr, months_ephrts, hours_ephrts)
   real*4 temp_gen(mnumnr,maxptypesp) , sec45v_credit, eqpco2_restore(mnumnr,maxptypesp)
   integer irg, igrp, iown, restore_plant_type, ephrt_month, ephrt_hour,ptypes,dtype
   integer stp, ergn, etyp, pstp, mo, dy, hr, restore_mapping(efd_d_dsp), i,j
   
   ! passed in from restore in ecp calc_storage_val
   
   real*4 dispatch_ephrts(mnumnr, months_ephrts, hours_ephrts) 
   
   ! we want to use, the prior year's getout data and we can use the existing data in the common block because ephrts is called first.
   sec45v_credit = 0.0
   temp_gen = 0.0
   eqpco2_restore = 0.0
   
   do irg = 1 , mnumnr-3  ! mnumnr=28  nerc regions (25+alaska+hawaii+us total)
      
      !write(*,*) " year, irg ", curiyr-1+1989, irg
      call getout(curiyr-1, irg) ! update the data in common block, do not call with alaska, hawaii + us total or run time error will be encountered
         
      do igrp = 1 , efd_d_dsp ! efd_d_dsp dispatchable cpacity types, subset of planttype_ecp
         
         restore_mapping = 0
         call get_efd_to_restore_tech_types(restore_mapping) ! in t
         restore_plant_type = restore_mapping(igrp)
         
         do iown = 1 , usw_own ! ownership, for example public vs private
            temp_gen(irg, restore_plant_type)  = temp_gen(irg, restore_plant_type) +  eqpgn(igrp,iown) 
         end do ! ownership types
         
         eqpco2_restore(irg, restore_plant_type) = eqpco2_restore(irg, restore_plant_type) + eqpco2(igrp)
         
      end do ! plant types
   end do ! emm regions

   co2byrestoreplant = 0.0 ! co2byrestoreplant(emm regions, restore plant types)

   do irg = 1 , mnumnr-3  ! mnumnr=28  nerc regions (25+alaska+hawaii+us total)
      do etyp = 1 , maxptypesp ! loop over restore plant types
         
         if (temp_gen(irg, etyp) .gt. 0.0) then
            co2byrestoreplant(irg, etyp) = eqpco2_restore(irg, etyp) / temp_gen(irg, etyp)   ! debug - eqpco2 has a lot of zeros in , still some values
         else
            co2byrestoreplant(irg, etyp) = 0.0
         end if
         
      end do ! plant types
   end do ! emm regions
   
   ! convert dispatch to ephrts time 12*24 and set to new variable?
   dispatch_ephrts = 0.0
   total_co2_from_electricity_dispatch = 0.0
   electricity_price_reducer = 0.0
   co2_per_mwhr = 0.0
   
   !debug 
   !do i = 4, 1, -1
   !   write(*,*) "sec45v : 1 = ",  sec45v_co2_req(1,i), ", 2 = ", sec45v_co2_req(2,i)
   !end do
   
   
   if (sw_ephrts_sec45v .eq. 1) then
	  ! reuse-ing code from calc_storage_val ...   
      do ergn = 1,mnumnr
         do mo=1,12
            do hr = 1,24
               do dy=1,3
                  do etyp = 1, maxptypesp
                     do pstp = 1 , num_steps_per_type(etyp)
                        ! compute dispatch for ephrts, dispatch from restore is in gwhrs
                        dispatch_ephrts(ergn, mo, hr) = dispatch_ephrts(ergn, mo, hr) + ( idaytq(dy,mo) * dispatch(1,ergn,etyp,pstp,mo,dy,hr) )
                        
                        ! total co2 from electricity dispatch, ! tons of co2/per million kwhr by efd plant type and nerc regios
                        total_co2_from_electricity_dispatch(ergn, mo, hr) = total_co2_from_electricity_dispatch(ergn, mo, hr) + ( co2byrestoreplant(ergn, etyp) *  idaytq(dy,mo) * dispatch(1,ergn,etyp,pstp,mo,dy,hr) )
                        
                     enddo
                     ! also add on storage dispatch which does not have steps
					 if (existsto(1,ergn,etyp,mo,dy,hr) .gt. 0.0) then
						dispatch_ephrts(ergn, mo, hr) = dispatch_ephrts(ergn, mo, hr) + ( idaytq(dy,mo) * existsto(1,ergn,etyp,mo,dy,hr) )
						total_co2_from_electricity_dispatch(ergn, mo, hr) = total_co2_from_electricity_dispatch(ergn, mo, hr) + ( co2byrestoreplant(ergn, etyp) *  idaytq(dy,mo) * existsto(1,ergn,etyp,mo,dy,hr) )
					 endif
                  enddo
               enddo
               ! computes co2 per mwhr using the total co2 from dispatch each region and the amount of electricity dispatched, short tonnes of co2/ gwhrs where 1 short ton is 907.185 kgs
			   if (dispatch_ephrts(ergn, mo, hr) .gt. 0) then
				  co2_per_mwhr(ergn, mo, hr) = (907.185/1000.0) *  total_co2_from_electricity_dispatch(ergn, mo, hr) / dispatch_ephrts(ergn, mo, hr) ! 907.185 kgs in a short ton, and 1000 mwhr in a gwhr
                  
                  ! send co2 per mwhrs to hmm
                  ! todo: replace co2_per_mwhr directly with co2_el_hr ?
                  !co2_el_hr(ergn, mo, hr, curiyr) = co2_per_mwhr(ergn, mo, hr)  ! (kgs of co2/ mwhr) HOURLY POWER GENERATION AVG EMISSIONS BY NERC REGION, MONTH, HOUR 
               else
				  co2_per_mwhr(ergn, mo, hr) = 0.0
                  ! todo: replace co2_per_mwhr directly with co2_el_hr ?                
                  !co2_el_hr(ergn, mo, hr, curiyr) = co2_per_mwhr(ergn, mo, hr)  ! (kgs of co2/ mwhr) HOURLY POWER GENERATION AVG EMISSIONS BY NERC REGION, MONTH, HOUR 
			   endif
               ! debugging
               !WRITE(*,*) "*************************CO2 PER MWHR********************************"
               !IF (CO2_PER_MWHR(ERGN, MO, HR) .GT. 0.0) THEN
               !   WRITE(*,*) 'CO2_PER_MWHR', CURIYR, ERGN,  MO, HR, CO2_PER_MWHR(ERGN, MO, HR), TOTAL_CO2_FROM_ELECTRICITY_DISPATCH(ERGN, MO, HR), DISPATCH_EPHRTS(ERGN, MO, HR)
               !END IF
               !WRITE(*,*) "*************************CO2 PER MWHR********************************"

               ! Deflator notes:
               ! 1987$ that is the NEMS baseline. MC_JPGDP(year) gives you the deflator for the requested year where it is based on 1987=1.0.  
               ! Since year 1 = 1990, you never see the 1.0 value, but MC_JPGDP(1) reflects inflation from 1987 to 1990, and each year goes up from there.
               ! We have an internal EMM variable, UHBSYR that is set = 1989 and often used to convert a calendar year to the proper CURIYR
               ! Then in the code: CREDIT45V = InputCRED / MC_JPGDP(CRED_YR$ - UHBSYR)
               
               sec45v_credit = (sec45v_h2_ptc * wage_app_multiplier) / mc_jpgdp(2022 - uhbsyr) ! sec45v policy starts in 2022
               
               ! todo: intial implementation of shared sec45v varaibles are contained in emmparm, integration guidance is required for restart file additions?
               do i = 4, 1, -1
                  if ( co2_per_mwhr(ergn, mo, hr) .lt. ( sec45v_co2_req(1,i) /electrolyzer_efficiency) ) then ! 0.45 kg of co2 per kgs h2 from sec 45v -> kgs co2/mwhr by using electrolyzer efficiency
                     electricity_price_reducer(ergn, mo, hr) = sec45v_credit * ( sec45v_co2_req(2,i) )/electrolyzer_efficiency ! $ per kg of h2 from sec 45v * (fraction of credit) -> $ per mwhr by using electrolyzer efficiency
                  endif
               end do
               
               !write(*,*) "*************************electricity_price_reducer********************************"
               !if (electricity_price_reducer(ergn, mo, hr) .ge. 0) then
               !   write(*,*) "electricity_price_reducer(ergn, mo, hr) ", curiyr, ergn, mo, hr, electricity_price_reducer(ergn, mo, hr), co2_per_mwhr(ergn, mo, hr), electrolyzer_efficiency, sec45v_co2_req(1,i), sec45v_co2_req(2,i)
               ! debugging
               !end if
               !write(*,*) "*************************electricity_price_reducer********************************"
               
            enddo
         enddo
      enddo  
	endif
    end subroutine
    
    
    
    
   subroutine check_if_feasible()
   ! checks to see if an aimms.err file exists, if so crash nems
   implicit none
   logical error_file_exists
    
   inquire(file=".\ephrts\log\aimms.err", exist=error_file_exists)
   !
   if ( error_file_exists .eq. .true.) then
      write(*,*) "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      write(*,*) "!! ephrts aimms model went infeasible, crashing nems now            !"
      write(*,*) "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      stop
   end if
    
   end subroutine
    
    
    subroutine delete_error_file()
    ! sometimes when we run the model locally for debugging, we forget to delete the .err and then launch a run
    ! in this case the run will automatically crash even though there is no error. this is a convenience function
    ! that deletes this file only on the first projection year prior to avoid this run time error. 
    ! nems will still check for this file and crash if the file is present
    ! also, the capacity vintage file needs to be reset each cycle

    ! TODO: find a final home for the capacity vintage reset
    implicit none
    logical error_file_exists, vintage_file_exists
    
    inquire(file=".\ephrts\log\aimms.err", exist=error_file_exists)
	
	inquire(file=".\ephrts\src\h2_production_capacity_vintage.csv", exist=vintage_file_exists)
    
    !
    if ( error_file_exists .eq. .true.) then
      call execute_command_line ("del ephrts\log\aimms.err")
      write(*,*) "warning - ephrts.err file found in log folder, deleting only on first projyr"
    end if
	
	! removing vintage capacity file before each cycle begins
	IF ( vintage_file_exists .eq. .true.) then
      call execute_command_line ("del ephrts\src\h2_production_capacity_vintage.csv")
    end if
    
    end subroutine
    
    
   subroutine ephrts_aimms_wrapper(current_year, start_year) !start year = yearpr + 2, curitr not begin used for anything in this subroutine
    !
    ! run ephrts model 
    ! the model is called different depending on the year and iterations. the first year is just to setup and ititalize the data into the efd and ecp supply curves
    ! using low quantities and high prices. the model is called twice, the first iteration uses census data to populate the supply curves, the 2nd iteration uses
    ! efd generation dispatch as the demand input for the model in order to calculate the electricity consumption of the electrolyzers in nerc regions
    ! for the load duration submodule.
   
   use dfport
   use ephrts_swtiches

   implicit none

   include'parametr'
   !include'ncntrl' ! conflicts with DFPORT
   include'emmparm'
   include'control'
   include'ecpcntl'
   include'dispin'
   include'dispout'
   include'dispuse'
   include'dispcrv'
   include'fuelin'
   include'bildin'
   include'dispett'
   include'uettout'
   include'postpr'
   include'dsmdimen'
   include'dsmsectr'
   include'dsmunits'
   include'dsmtoefd'
   include'dsmcaldr'
    
   integer iret, current_year,curitr, start_year
   logical read_hem_error, dir_exists
   character(len=4) year_str
   character*216 line
   logical error_file_exists, crash_nems
   integer*4 cpu_time,wall_time
   real electricity_price_reducer(mnumnr, months_ephrts, hours_ephrts)
    
   ! only on start year, check for aimms.err file and then delete
   if (current_year - 1 .eq. start_year) then
      call delete_error_file()              
   endif
    
   !call mptim3(cpu_time,wall_time)  
   !write(*,*) "ephrts entry point - ", cpu_time/100, wall_time/100
   
   ! call to read in ephrts parameters, todo: add common block to preserve values between routines
   !if (ephrts_read_parameter_file .eq. .false. ) then
   !  call read_ephrts_nems_parameters
   !  ephrts_read_parameter_file = .true.
   !end if

   ! fetch cost factors from ecp
   call write_fixed_cost_factors
   
   !call mptim3(cpu_time,wall_time)  
   !write(*,*) "write fixed costs factors - ", cpu_time/100, wall_time/100
    
   ! error checking, check if crash_nems.txt is present, this file comes from the ephrts python prepocessor and is a generic feasiblity check
   inquire(file="ephrts\crash_nems.txt", exist=crash_nems)
   if (crash_nems) then
      write(*,*) "ephrts went infeasible in prior year, crashing nems. note: this is likely due to the load_ratio constraint"
      write(*,*) "there is also a check in the python aimms wrapper that will output this result as well"
      stop
   end if
    
   write(year_str,'(I4)') current_year ! cast current year to year_str to write out to batch file
    
   write(line,'(a)') 'IF NOT EXIST ephrts_debug MKDIR ephrts_debug' ! CREATES THE EPHRTS DEBUG FOLDER IF NOT PRESENT
   call callsys(iret,line)  ! calls a subroutine in main.f to send the command to the system  like a console command.
   
   !call mptim3(cpu_time,wall_time)  
   !write(*,*) "making debug ephrts folder - ", cpu_time/100, wall_time/100
   
   ! initialize data for the input file
   if (current_year - 1 .eq. start_year) then
      call write_ephrts_run_bat_input_file(year_str, current_year, 0)
   else if (current_year .gt. start_year) then
      call write_ephrts_run_bat_input_file(year_str, current_year, 1)
   endif
   
   !call mptim3(cpu_time,wall_time)  
   !write(*,*) "creating batch file for ephrts run input - ", cpu_time/100, wall_time/100
   
   electricity_price_reducer = 0.0
    
   ! this calls the sec45 algorithm which returns an electricity price reducer based on policy emissions requirements
   call sec45v_calculate_emissions(electricity_price_reducer) ! uses previous years data since efd is run after ephrts !current_year-1989-1,

   !call mptim3(cpu_time,wall_time)  
   !write(*,*) "calculating sec45v emissions ", cpu_time/100, wall_time/100
   
   ! write the result of the electricity price reducer for the aimms project
   call write_sec45v_electricity_price_reducer(electricity_price_reducer)

   !call mptim3(cpu_time,wall_time)  
   !write(*,*) "write_sec45v_electricity_price_reducer ", cpu_time/100, wall_time/100
    
   ! this runs the python processors and ephrts aimms module
   if (current_year - 1 .eq. start_year) then
      call run_ephrts(year_str,0,start_year+1) ! only on the very first pass through
   else if (current_year .gt. start_year) then
      call run_ephrts(year_str,1,start_year+1)
   endif

   !call mptim3(cpu_time,wall_time)  
   !write(*,*) "finished running ephrts", cpu_time/100, wall_time/100

   ! this call is for computing the fuel costs, it lags an additional year, note the ephrts_aimms_wrapper's start_year is yearpr+2 so here we do yearpr+3 by setting start_year + 1
   if (current_year .gt. start_year+2) then 
      call read_ephrts_supply_curve_for_efp(current_year)
      !call mptim3(cpu_time,wall_time)  
      !write(*,*) "reading epherts supply curve for efp", cpu_time/100, wall_time/100
      
      call compute_upfuel_for_efp
      !call mptim3(cpu_time,wall_time)  
      !write(*,*) "computing upfuel for efp", cpu_time/100, wall_time/100
   endif
   
   
   ! second pass is used to generate an hourly d_fuel using the sqfuel, directly
   call write_ephrts_run_bat_input_file(year_str, current_year, 2)
   !call mptim3(cpu_time,wall_time)  
   !write(*,*) "write_ephrts_run_bat_input_file", cpu_time/100, wall_time/100
   
   ! this runs the python processors and ephrts aimms module
   call run_ephrts(year_str,2,start_year+1)
   !call mptim3(cpu_time,wall_time)  
   !write(*,*) "running ephrts model", cpu_time/100, wall_time/100
   
   ! fetch the d_fuel (electricity to create hydrogen) 
   call get_d_fuel_from_ephrts
   !call mptim3(cpu_time,wall_time)  
   !write(*,*) "obtain demand fuel from ephrts model", cpu_time/100, wall_time/100
    
   call check_if_feasible          
   ! check for aimms error log

   !call mptim3(cpu_time,wall_time)
   !write(*,*) "check_if_feasible, completed - ", cpu_time/100, wall_time/100
    end subroutine
    
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   Send to HMM Code !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   Send to HMM Code !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   Send to HMM Code !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
   subroutine send_hmm_sec45v_hourly_emissions
   ! this is the sec45v implementation for ephrts, specifically the data preparation portion. what the algorithm does
   ! is take the hourly dispatch from restore along with the amount of co2 from each efd plant. it then computes a ratio
   ! of each and depending on the ratio and the sec45 carbon limit requirments a price reducer is computed by applying tax credits
   ! that price reducer is written out for input into ephrts. 
   
   use hourly_restore_data
   
   implicit none
   
   include'parametr'
   include'ncntrl'
   include'emmparm'
   include'control'
   include'ecpcntl'
   include'dispin'
   include'dispout'
   include'dispuse'
   include'dispcrv'
   include'fuelin'
   include'bildin'
   include'dispett'
   include'uettout'
   include'postpr'
   include'dsmdimen'
   include'dsmsectr'
   include'dsmunits'
   include'dsmtoefd'
   include'dsmcaldr'
   include'elcntl'
   include'elout'
   !include'ecpcntl'
   include'plntctl'
   include'emission'
   include'efdin'
   include 'macout'
   
   real*4 co2byrestoreplant(mnumnr,maxptypesp)
   real*4 total_co2_from_electricity_dispatch(mnumnr, months_ephrts, hours_ephrts)
   real*4 temp_gen(mnumnr,maxptypesp) , sec45v_credit, eqpco2_restore(mnumnr,maxptypesp)
   integer irg, igrp, iown, restore_plant_type, ephrt_month, ephrt_hour,ptypes,dtype
   integer stp, ergn, etyp, pstp, mo, dy, hr, restore_mapping(efd_d_dsp), i,j
   
   ! passed in from restore in ecp calc_storage_val
   
   real*4 dispatch_ephrts(mnumnr, months_ephrts, hours_ephrts) 
   
   ! we want to use, the prior year's getout data and we can use the existing data in the common block because ephrts is called first.
   sec45v_credit = 0.0
   temp_gen = 0.0
   eqpco2_restore = 0.0
   
   do irg = 1 , mnumnr-3  ! mnumnr=28  nerc regions (25+alaska+hawaii+us total)
      
      !write(*,*) " year, irg ", curiyr-1+1989, irg
      call getout(curiyr, irg) ! update the data in common block, do not call with alaska, hawaii + us total or run time error will be encountered
         
      do igrp = 1 , efd_d_dsp ! efd_d_dsp dispatchable cpacity types, subset of planttype_ecp
         
         restore_mapping = 0
         call get_efd_to_restore_tech_types(restore_mapping) ! in t
         restore_plant_type = restore_mapping(igrp)
         
         do iown = 1 , usw_own ! ownership, for example public vs private
            temp_gen(irg, restore_plant_type)  = temp_gen(irg, restore_plant_type) +  eqpgn(igrp,iown) 
         end do ! ownership types
         
         eqpco2_restore(irg, restore_plant_type) = eqpco2_restore(irg, restore_plant_type) + eqpco2(igrp)
         
      end do ! plant types
   end do ! emm regions

   co2byrestoreplant = 0.0 ! co2byrestoreplant(emm regions, restore plant types)

   do irg = 1 , mnumnr-3  ! mnumnr=28  nerc regions (25+alaska+hawaii+us total)
      do etyp = 1 , maxptypesp ! loop over restore plant types
         
         if (temp_gen(irg, etyp) .gt. 0.0) then
            co2byrestoreplant(irg, etyp) = eqpco2_restore(irg, etyp) / temp_gen(irg, etyp)   ! debug - eqpco2 has a lot of zeros in , still some values
         else
            co2byrestoreplant(irg, etyp) = 0.0
         end if
         
      end do ! plant types
   end do ! emm regions
   
   ! convert dispatch to ephrts time 12*24 and set to new variable?
   dispatch_ephrts = 0.0
   total_co2_from_electricity_dispatch = 0.0
   !co2_el_hr = 0.0
   
   !debug 
   !do i = 4, 1, -1
   !   write(*,*) "sec45v : 1 = ",  sec45v_co2_req(1,i), ", 2 = ", sec45v_co2_req(2,i)
   !end do
   
	! reuse-ing code from calc_storage_val ...   
    do ergn = 1,mnumnr
        do mo=1,12
        do hr = 1,24
            do dy=1,3
                do etyp = 1, maxptypesp
                    do pstp = 1 , num_steps_per_type(etyp)
                    ! compute dispatch for ephrts, dispatch from restore is in gwhrs
                    dispatch_ephrts(ergn, mo, hr) = dispatch_ephrts(ergn, mo, hr) + ( idaytq(dy,mo) * dispatch(1,ergn,etyp,pstp,mo,dy,hr) )
                        
                    ! total co2 from electricity dispatch, ! tons of co2/per million kwhr by efd plant type and nerc regios
                    total_co2_from_electricity_dispatch(ergn, mo, hr) = total_co2_from_electricity_dispatch(ergn, mo, hr) + ( co2byrestoreplant(ergn, etyp) *  idaytq(dy,mo) * dispatch(1,ergn,etyp,pstp,mo,dy,hr) )
                        
                    enddo
                    ! also add on storage dispatch which does not have steps
					if (existsto(1,ergn,etyp,mo,dy,hr) .gt. 0.0) then
					    dispatch_ephrts(ergn, mo, hr) = dispatch_ephrts(ergn, mo, hr) + ( idaytq(dy,mo) * existsto(1,ergn,etyp,mo,dy,hr) )
					    total_co2_from_electricity_dispatch(ergn, mo, hr) = total_co2_from_electricity_dispatch(ergn, mo, hr) + ( co2byrestoreplant(ergn, etyp) *  idaytq(dy,mo) * existsto(1,ergn,etyp,mo,dy,hr) )
					endif
                enddo
            enddo
            ! computes co2 per mwhr using the total co2 from dispatch each region and the amount of electricity dispatched, short tonnes of co2/ gwhrs where 1 short ton is 907.185 kgs
			!if (dispatch_ephrts(ergn, mo, hr) .gt. 0) then
			!	co2_el_hr(ergn, mo, hr, curiyr) = (907.185/1000.0) *  total_co2_from_electricity_dispatch(ergn, mo, hr) / dispatch_ephrts(ergn, mo, hr) ! 907.185 kgs in a short ton, and 1000 mwhr in a gwhr
   !              ! (kgs of co2/ mwhr) HOURLY POWER GENERATION AVG EMISSIONS BY NERC REGION, MONTH, HOUR 
   !         else
   !             ! todo: replace co2_per_mwhr directly with co2_el_hr ?                
   !             co2_el_hr(ergn, mo, hr, curiyr) = 0.0  ! (kgs of co2/ mwhr) HOURLY POWER GENERATION AVG EMISSIONS BY NERC REGION, MONTH, HOUR 
			!endif
               
        enddo
        enddo
    enddo  
    end subroutine

        
   subroutine send_hmm_hydrogen_consumption
   ! set hydrogen consumption variables from efd into variable for hmm
   
   use ephrts_swtiches
   use ephrts_file_unit_numbers
   
   implicit none
   include 'parametr'
   include 'ncntrl'
   include 'qblk'
   include 'mpblk'
   include 'emmparm'
   include 'bildout'
   include 'bildin'
   include 'emission'
   include 'uefpout'     ! UEFP output variables
   include 'uefdout'     ! EFD output variables
   include 'udatout'     ! UDAT output variables
   include 'uecpout'     ! UECP output variables
   include 'uldsmout'    ! LDSM output variables
   include 'uettout'     ! UETT output variables
   include 'ecpcntl'
   include 'control'
   include 'entcntl'
   include 'dispuse'
   include 'elout'
   include 'dsmdimen'
   include 'dsmcaldr'

   integer isp,irg      

   ! populate
   
   QH2EL(MNUMCR,curiyr) = 0.0
   
   do irg = 1, MNUMCR - 1
       QH2EL(irg,curiyr) = 0.0 ! inititalize for each region but not season since this is an annual value

       do isp = 1, EPNMSP ! ecp seasons = 3
           
            !intialize before populating
            SQH2EL(irg,isp,curiyr) = 0.0
           
            !if (ephrts .eq. 0) then  ! Note: use_hmm_vars means that ephrts is turned off, so we send to HMM the consumption
                                               !       if false, this means ephrts is turned on (and HMM could be too), and we want to send them zero instead so we skip the code below.
                 !populating
                 SQH2EL(irg,isp,curiyr) = sqfuel(uigc,irg,isp)/1000.0 ! Converts bbtus to tbtus, note that REAL*4 SQH2EL(MNUMNR,3,MNUMYR) H2 CONSUMPTION FROM ELECTRIC POWER SECTOR
                 QH2EL(irg,curiyr) = QH2EL(irg,curiyr) + SQH2EL(irg,isp,curiyr) ! H2 consumed in electric power sector by census division, year (QUNTY  tBTU)
            !endif             
            
       enddo
       
       QH2EL(MNUMCR,curiyr) = QH2EL(MNUMCR,curiyr) + QH2EL(irg,curiyr) !Sets total national H2 fuel demand from the power sector in the last index of MNUMCR in QH2EL
       
   enddo
    
   end subroutine
    
    
   subroutine send_hmm_hourly_restore_data
   ! set hydrogen consumption variables from efd into variable for hmm
   
   use ephrts_swtiches
   use ephrts_file_unit_numbers
   
   implicit none
   include 'parametr'
   include 'ncntrl'
   include 'qblk'
   include 'mpblk'
   include 'emmparm'
   include 'bildout'
   include 'emission'
   include 'uefpout'     ! UEFP output variables
   include 'uefdout'     ! EFD output variables
   include 'udatout'     ! UDAT output variables
   include 'uecpout'     ! UECP output variables
   include 'uldsmout'    ! LDSM output variables
   include 'uettout'     ! UETT output variables
   include 'ecpcntl'
   include 'control'
   include 'entcntl'
   include 'dispuse'
   include 'elout'
   include 'dsmdimen'
   include 'dsmcaldr'
   include 'efprp2'
   include 'macout'

   integer isp,irg,months, hrs, s, tech
   integer, parameter :: month2season(12) = [4,4,1,1,2,2,2,2,3,3,4,4] ! todo : hmm puts into restart variable
   integer, parameter :: numberOfMonthsPerSeason(4) = [2,4,2,4] ! todo : hmm puts into restart variable

   ! REAL*4 PRICE_EL_HR(MNUMNR,12,24,MNUMYR) ! ELECTRICITY PRICE PER NERC REGION, MONTH, HOUR (NO 45V CREDIT)
   ! REAL*4 ElecPriceTot(MNUMNR,12,24,MNUMYR)				! ReStore hourly electricity price by Nerc region
    ! REAL*4 CurtailsTot(MNUMNR,9,12,24,MNUMYR)		! ReStore hourly curtailment by Nerc region

   do irg = 1, MNUMNR 
       do s = 1, 4
          do hrs = 1, 24
                 PRICE_EL_HR(irg,s,hrs,curiyr) = 0.0
                 qelcur(irg,s,hrs,curiyr) = 0.0
          end do
       end do
   end do
   
   ! sum up the electricity prices over seasons by looping over 12 months and accumulating the variable to itself, later then divide by number of season types per season to obtain average
   do irg = 1, MNUMNR 
       do months = 1, 12
            ! fetch season  
            s = month2season(months)
            do hrs = 1, 24   
                
               ! populate PRICE_EL_HR with ReStore hourly electricity price by NERC region
               ! compute total electricity price over seasons before later taking average, the average is provided to hmm
                PRICE_EL_HR(irg,s,hrs,curiyr) = PRICE_EL_HR(irg,s,hrs,curiyr) + ElecPriceTot(irg,months,hrs,curiyr)
              
               ! provide total curtailment to hmm by summing up the total curtailment per technology type
               do tech = 1, 9 ! restore technologies
                  qelcur(irg,s,hrs,curiyr) = qelcur(irg,s,hrs,curiyr) + CurtailsTot(irg,tech,months,hrs,curiyr)
               end do
               
               !qelcur(irg,s,hrs,curiyr) = qelcur(irg,s,hrs,curiyr) + 2000 !Increasing curtailment to all regions for 45V credit in HMM (testing)
               
            enddo
       enddo
   enddo
   
   ! now take average by dividing by the number of seasons per season type
   do irg = 1, MNUMNR 
       do s = 1, 4
          do hrs = 1, 24
              PRICE_EL_HR(irg,s,hrs,curiyr) = PRICE_EL_HR(irg,s,hrs,curiyr)/numberOfMonthsPerSeason(s)
              
              ! TODO: If use EPRICE for transmission only cost, move and test send_to_hmm routine after EFP
              ! Adds transmission cost to marginal prices from REStore (distribution cost left out)
              if (irg .le. mnumnr - 3) then
                PRICE_EL_HR(irg,s,hrs,curiyr) = (EPRICE(3,2,irg) / MC_JPGDP(curiyr) + PRICE_EL_HR(irg,s,hrs,curiyr)) ! * 0.1  ! 3 = industrial, 2 = transmission 
              end if
              
          
           !debug
               !if (irg .eq. 1) then
               !   write(*,*) 'p/c', irg,s,hrs,curiyr+1989,PRICE_EL_HR(irg,s,hrs,curiyr), qelcur(irg,s,hrs,curiyr), ElecPriceTot(irg,7,hrs,curiyr)
               !endif
          
          end do
       end do
   end do
    
    end subroutine 
   
   subroutine send_hmm_45v_eligible_generation
   ! set hydrogen consumption variables from efd into variable for hmm
   
   use ephrts_swtiches
   use ephrts_file_unit_numbers
   use hourly_restore_data
   
   implicit none
   include 'parametr'
   include 'ncntrl'
   include 'qblk'
   include 'mpblk'
   include 'emmparm'
   include 'control'
   include 'bildout'
   include 'emission'
   include 'cogen'
   include 'uefpout'     ! UEFP output variables
   include 'uefdout'     ! EFD output variables
   include 'udatout'     ! UDAT output variables
   include 'uecpout'     ! UECP output variables
   include 'uldsmout'    ! LDSM output variables
   include 'uettout'     ! UETT output variables
   include 'ecpcntl'
   include 'entcntl'
   include 'dispuse'
   include 'elout'
   include 'dsmdimen'
   include 'dsmcaldr'
   include 'efprp2'
   include 'macout'
   
   ! generation technology groupings
    ! (1) wind : on shore/off shore
    ! (2) solar : solarthermal, standalone pv, hybrid, end-use pv
    ! (3) other : geotherma, waste/biomass, hydro
    
    real, save :: generation(3,mnumnr,24,4,mnumyr)
    
    ! total capacity technology groupings (GWS)
    real capacity(3,mnumnr)
    !real, save :: capacity_end_use_pv(mnumnr,mnumyr)

    ! cumulative additions technology groupings (GWS)
    real, save :: cumulative_additions(3, mnumnr,mnumyr) 
    
    ! capacity technology groupings
    
    ! output 
    real, save :: renewable_generation_by_vintage(mnumnr,24,4,mnumyr,mnumyr) ! nerc regions, current year, vintage year
    real, save :: vintage_ratio(3, mnumnr,mnumyr, mnumyr)! tech, nerc regions, current year, vintage year
   
   ! iterators
   integer irg, s, hrs, vintage_year
   
   ! temps
   integer temp_tech, temp_irg, temp_year, temp_vintage_year, temp_hrs, temp_s, reason
   real temp_vintage_ratio, temp_generation
   
   ! restore declarations
   integer isp, months, tech
   integer, parameter :: month2season(12) = [4,4,1,1,2,2,2,2,3,3,4,4] ! todo : hmm puts into restart variable
   integer, parameter :: numberOfMonthsPerSeason(4) = [2,4,2,4] ! todo : hmm puts into restart variable

   logical file1, file2, file3  
   
   
   REAL*4 TempDispatchTot(MNUMNR,MaxPtypesP,4,24,MNUMYR)		! ReStore hourly generation by Nerc region
   LOGICAL SKIP 
   INTEGER MO,HR,DY,ERGN,PSTP,ETYP
   
   
   ! initialize 
   ! generation = 0.0 ! Note: do not initilize if restart file variable
   ! vintage_ratio = 0.0 ! Note: do not initilize if restart file variable 
    
    ! total capacity technology groupings (GWS)
     capacity = 0.0

    ! initialize cumulative additions + end-use capacity
    do irg = 1, MNUMNR - 3
        cumulative_additions(1,irg,curiyr) = 0.0
        cumulative_additions(2,irg,curiyr) = 0.0
        cumulative_additions(3,irg,curiyr) = 0.0
   !     capacity_end_use_pv(irg,mnumyr) = 0.0
    end do
    
    TempDispatchTot = 0.0
    
    ! getting the restore dispatch data from the variable "dispatch" but filtering out end-use pv
    DO ERGN = 1,MNUMNR
       DO ETYP = 1, MaxPtypes
         DO MO=1,12
            s = month2season(MO)
           DO DY=1,3
             DO HR = 1,24
                DO PSTP = 1 , Num_Steps_per_Type(ETYP)
                    
                        skip = .false.
                        if (ETYP .eq. 20 .and. PSTP .eq. 2) then ! Skip end-use pv here, where 20 is PV and bin is enduse
                            skip = .true.
                        endif
                           
                        if ( skip .eq. .false. ) then
                            TempDispatchTot(ERGN,ETYP,s,hr,CURIYR) = TempDispatchTot(ERGN,ETYP,s,hr,CURIYR) + Dispatch(1,ERGN,ETYP,PSTP,MO,DY,HR) * IDAYTQ(dy,mo)
                        endif
                    
                ENDDO
               ENDDO
             ENDDO
           ENDDO
         ENDDO
    ENDDO  
    
   
   ! set generation from restore
   do irg = 1, MNUMNR - 3
       do months = 1, 12
            ! fetch season  
            s = month2season(months)
            do hrs = 1, 24   
                QELGEN(1,irg,hrs,s,curiyr) = TempDispatchTot(irg,14,s,hrs,curiyr) + TempDispatchTot(irg,16,s,hrs,curiyr) ! wind 
                QELGEN(2,irg,hrs,s,curiyr) = TempDispatchTot(irg,20,s,hrs,curiyr) + TempDispatchTot(irg,21,s,hrs,curiyr) + TempDispatchTot(irg,17,s,hrs,curiyr) + TempDispatchTot(irg,18,s,hrs,curiyr) + TempDispatchTot(irg,19,s,hrs,curiyr) !- hourlyendusePV(irg,s,hrs,curiyr)! solar (subtract out enduse pv)
                QELGEN(3,irg,hrs,s,curiyr) = TempDispatchTot(irg,6,s,hrs,curiyr) + TempDispatchTot(irg,10,s,hrs,curiyr) + TempDispatchTot(irg,8,s,hrs,curiyr) + TempDispatchTot(irg,9,s,hrs,curiyr) + TempDispatchTot(irg,7,s,hrs,curiyr) ! other (e.g. nuclear)
            enddo
       enddo
   enddo
                                              
   
    ! populate capacity groupings from ftab
    do irg = 1, MNUMNR - 3
        capacity(1,irg) = UCAPWNU(irg,curiyr) + UCAPWNN(irg,curiyr) + UCAPWNC(irg,curiyr) + UCAPWFU(irg,curiyr) + UCAPWFN(irg,curiyr) + UCAPWFC(irg,curiyr) ! wind
        capacity(2,irg) = UCAPSTU(irg,curiyr) + UCAPSTN(irg,curiyr) + UCAPSTC(irg,curiyr) + UCAPPVU(irg,curiyr) + UCAPPVN(irg,curiyr) + UCAPPVC(irg,curiyr) &
                            + UCAPPTU(irg,curiyr) + UCAPPTN(irg,curiyr) + UCAPPTC(irg,curiyr) !+ CGTOTCAPNR(irg,curiyr,10)*0.001 ! solar
        capacity(3,irg) = UCAPHYU(irg,curiyr) + UCAPHYN(irg,curiyr) + UCAPHYC(irg,curiyr) + UCAPGEU(irg,curiyr) + UCAPGEN(irg,curiyr) + UCAPGEC(irg,curiyr) + &
                          UCAPMSU(irg,curiyr) + UCAPMSN(irg,curiyr) + UCAPMSC(irg,curiyr)  + UCAPWDU(irg,curiyr) + UCAPWDN(irg,curiyr) + UCAPWDC(irg,curiyr) + &
                          UCAPNUU(irg,curiyr) + UCAPNUN(irg,curiyr) +  UCAPSMU(irg,curiyr) + UCAPSMN(irg,curiyr) ! other
        
        !capacity_end_use_pv(irg,curiyr) = CGTOTCAPNR(irg,curiyr,10)*0.001 ! MWs to Gws
    end do    
    
    ! populate cumulative additions from ftab using planned and unplanned capacities together
    do irg = 1, MNUMNR - 3
        cumulative_additions(1,irg,curiyr) = UADDWNU(2,irg,curiyr) + UADDWNN(2,irg,curiyr) + UADDWNU(1,irg,curiyr) + UADDWNN(1,irg,curiyr) + UADDWNC(irg,curiyr) + UADDWFU(2,irg,curiyr) + UADDWFN(2,irg,curiyr) + UADDWFU(1,irg,curiyr) + UADDWFN(1,irg,curiyr) + UADDWFC(irg,curiyr) ! wind
        
        cumulative_additions(2,irg,curiyr) = UADDPVU(2,irg,curiyr) + UADDPVN(2,irg,curiyr) + UADDPVU(1,irg,curiyr) + UADDPVN(1,irg,curiyr) + UADDPVC(irg,curiyr) + UADDSTU(2,irg,curiyr) + &
              UADDSTN(2,irg,curiyr) + UADDSTU(1,irg,curiyr) + UADDSTN(1,irg,curiyr) + UADDSTC(irg,curiyr) + UADDPTU(2,irg,curiyr) + UADDPTN(2,irg,curiyr) + UADDPTU(1,irg,curiyr) + &
               UADDPTN(1,irg,curiyr) ! solar 
        
        cumulative_additions(3,irg,curiyr) = UADDHYU(1,irg,curiyr) + UADDHYN(1,irg,curiyr) + UADDHYU(2,irg,curiyr) + UADDHYN(2,irg,curiyr)  +  UADDGEU(2,irg,curiyr) +   &
                                             UADDGEN(2,irg,curiyr) + UADDGEU(1,irg,curiyr) + UADDGEN(1,irg,curiyr) + UADDGEC(irg,curiyr) &
                                             + UADDMSU(2,irg,curiyr) + UADDMSN(2,irg,curiyr) + UADDMSU(1,irg,curiyr) + UADDMSN(1,irg,curiyr) & 
                                              + UADDMSC(irg,curiyr) + UADDWDU(2,irg,curiyr) + UADDWDN(2,irg,curiyr) + UADDWDU(1,irg,curiyr) &
                                             + UADDWDN(1,irg,curiyr) + UADDWDC(irg,curiyr) + UADDNUU(1,irg,curiyr) + UADDNUN(1,irg,curiyr) &
                                             + UADDSMU(1,irg,curiyr) + UADDSMN(1,irg,curiyr) + UADDNUU(2,irg,curiyr) + UADDNUN(2,irg,curiyr) &
                                             + UADDSMU(2,irg,curiyr) + UADDSMN(2,irg,curiyr)! other
    
    end do
  
    ! if any equal to zero then set to 1.0
    do irg = 1, MNUMNR - 3
        if (capacity(1,irg) .le. 0.0) then
            capacity(1,irg) = 1.0
        end if
        
        if (capacity(2,irg) .le. 0.0) then
            capacity(2,irg) = 1.0
        end if
    
        if (capacity(3,irg) .le. 0.0) then
            capacity(3,irg) = 1.0
        end if
        
    end do
    
   ! Wait to do calculations until the first ECP year, this allows data to be populated for a few years prior before using them 
   if(curcalyr.ge.UPSTYR) then
       ! Compute vintage_ratio ratios 
       do irg = 1, MNUMNR - 3
          do vintage_year = 23, curiyr ! loop until the current projection, not future
              
              
              QELGEN_VINT(1,irg,curiyr,vintage_year) = ( cumulative_additions(1,irg,vintage_year) - cumulative_additions(1,irg,vintage_year-1) )/ capacity(1,irg) ! wind
 !             vintage_ratio(2,irg,curiyr,vintage_year) = ( (cumulative_additions(2,irg,vintage_year) - cumulative_additions(2,irg,vintage_year-1)) + (capacity_end_use_pv(irg,vintage_year) - capacity_end_use_pv(irg,vintage_year-1)) )/ (capacity(2,irg)) ! solar
              QELGEN_VINT(2,irg,curiyr,vintage_year) =( cumulative_additions(2,irg,vintage_year) - cumulative_additions(2,irg,vintage_year-1) ) / (capacity(2,irg)) ! solar
              QELGEN_VINT(3,irg,curiyr,vintage_year) = ( cumulative_additions(3,irg,vintage_year) - cumulative_additions(3,irg,vintage_year-1) )/ capacity(3,irg) ! other
        
              
              if (QELGEN_VINT(1,irg,curiyr,vintage_year) .lt. 0.0) then
              
                  write(*,*) "wind less than 0.0 vintage found, write debug statments "
              
                  write(*,*) curcalyr, irg, vintage_year+1989, curitr
              
                  write(*,*) QELGEN_VINT(1,irg,curiyr,vintage_year)
                  write(*,*) cumulative_additions(1,irg,vintage_year), cumulative_additions(1,irg,vintage_year-1), capacity(1,irg) ! wind
                  write(*,*) 'diff:', cumulative_additions(1,irg,vintage_year) - cumulative_additions(1,irg,vintage_year-1)
                  write(*,*) capacity(1,irg)
              
                  write(*,*) 'technologies - current year'
                  write(*,*) UADDWNU(2,irg,curiyr), UADDWNN(2,irg,curiyr),UADDWNU(1,irg,curiyr),UADDWNN(1,irg,curiyr)
                  write(*,*) UADDWNC(irg,curiyr), UADDWFU(2,irg,curiyr),UADDWFN(2,irg,curiyr),UADDWFU(1,irg,curiyr)
                  write(*,*) UADDWFN(1,irg,curiyr), UADDWFC(irg,curiyr) ! wind
              
                  write(*,*) 'technologies - previous year'
                  write(*,*) UADDWNU(2,irg,curiyr-1), UADDWNN(2,irg,curiyr-1),UADDWNU(1,irg,curiyr-1),UADDWNN(1,irg,curiyr-1)
                  write(*,*) UADDWNC(irg,curiyr-1), UADDWFU(2,irg,curiyr-1),UADDWFN(2,irg,curiyr-1),UADDWFU(1,irg,curiyr-1)
                  write(*,*) UADDWFN(1,irg,curiyr-1), UADDWFC(irg,curiyr-1) ! wind

                  write(*,*) 'technologies - delta current year - previous year'
                  write(*,*) UADDWNU(2,irg,curiyr) - UADDWNU(2,irg,curiyr-1), UADDWNN(2,irg,curiyr) - UADDWNN(2,irg,curiyr-1)
                  write(*,*) UADDWNU(1,irg,curiyr) - UADDWNU(1,irg,curiyr-1),UADDWNN(1,irg,curiyr) - UADDWNN(1,irg,curiyr-1)
                  write(*,*) UADDWNC(irg,curiyr) - UADDWNC(irg,curiyr-1), UADDWFU(2,irg,curiyr) - UADDWFU(2,irg,curiyr-1)
                  write(*,*) UADDWFN(2,irg,curiyr)-UADDWFN(2,irg,curiyr-1),UADDWFU(1,irg,curiyr) - UADDWFU(1,irg,curiyr-1)
                  write(*,*) UADDWFN(1,irg,curiyr) - UADDWFN(1,irg,curiyr-1), UADDWFC(irg,curiyr) - UADDWFC(irg,curiyr-1) ! wind

              
              end if
              
              if (QELGEN_VINT(2,irg,curiyr,vintage_year) .lt. 0.0) then
                  write(*,*) "solar less than 0.0 vintage found, write debug statments "
              end if      
              
              if (QELGEN_VINT(3,irg,curiyr,vintage_year) .lt. 0.0) then
                  write(*,*) "other less than 0.0 vintage found, write debug statments "
              end if      
              

          end do
       end do
   
   endif

   
 end subroutine 

 
    
    subroutine read_generation_delta_from_restore
   ! set hourly electrolyzer hourly electrolyzer load limits
   ! this prevents hmm from asking for too much load in a given hour that would exceed installed capacity by given hmm a quanitity that can not be exceeded
   ! and hmm also uses current prices
   
   ! todo: another way to add fidelity to the electrolyzer load limits so that way hmm can constrain electrolyzer utilization would be to 
   !      create multiple steps where the first would be 
   !      1) current marginal prices, current capacity - hourly load excluding nat. gas
   !      2) CT/CC marginal prices, nat. gas. capacity
   
   
   use ephrts_swtiches
   use ephrts_file_unit_numbers
   use hourly_restore_data
   
   implicit none
   include 'parametr'
   include 'ncntrl'
   include 'qblk'
   include 'mpblk'
   include 'emmparm'
   include 'control'
   include 'bildout'
   include 'emission'
   include 'cogen'
   include 'uefpout'     ! UEFP output variables
   include 'uefdout'     ! EFD output variables
   include 'udatout'     ! UDAT output variables
   include 'uecpout'     ! UECP output variables
   include 'uldsmout'    ! LDSM output variables
   include 'uettout'     ! UETT output variables
   include 'ecpcntl'
   include 'entcntl'
   include 'dispuse'
   include 'elout'
   include 'dsmdimen'
   include 'dsmcaldr'
   include 'efprp2'
   include 'macout'
   
   ! iterators
   integer solveindex,emmregion,ecpgroup,gensteps,month,daytype,ecphour,year, reason
   character*80 putFileName
   CHARACTER*4  CYEAR
   Logical file_exists
   real value
   
    ! Read in Dispatch Delta
    DispatchDelta = 0.0
      
    WRITE(CYEAR,'(I4)') CURIYR+UHBSYR
    write(putFileName,'(a,i4,a,a)') '.\rest\fromAIMMS\output_'//CYEAR//'\Rep_GenerationDelta.csv'
    
   inquire(file=putFileName, exist=file_exists)
   !
   if ( file_exists .eq. .true.) then
      open(unit=unit_num_read_delta_gen, file=putFileName, status='unknown')
      read(unit_num_read_delta_gen,*) ! skip header of file
      do
         read(unit_num_read_delta_gen,*,iostat=reason) year,ecpgroup, emmregion, gensteps, month, daytype, ecphour, solveindex, value
         
         DispatchDelta(solveindex,emmregion,ecpgroup,gensteps,month,daytype,ecphour) = value
         
         !debug 
        ! write(*,*)  solveindex,emmregion,ecpgroup,gensteps,month,daytype,ecphour, value, DispatchDelta(solveindex,emmregion,ecpgroup,gensteps,month,daytype,ecphour)

         
         if (reason > 0)  then
            write(*, *) "warning - possible corrupted file when reading in Rep_GenerationDelta.csv"
			close(unit_num_read_delta_gen) ! close file
            exit
         else if (reason < 0) then
            close(unit_num_read_delta_gen) ! close file
            exit
         endif
      
      end do
   !else
   !   write(*,*) "error: file Rep_GenerationDelta does not exists, is restore populating this?"
   !   stop
   end if
   ! 

end subroutine 
      
    
   subroutine send_hmm_electrolyzer_load_limits
   ! set hourly electrolyzer hourly electrolyzer load limits
   ! this prevents hmm from asking for too much load in a given hour that would exceed installed capacity by given hmm a quanitity that can not be exceeded
   ! and hmm also uses current prices
   
   ! todo: another way to add fidelity to the electrolyzer load limits so that way hmm can constrain electrolyzer utilization would be to 
   !      create multiple steps where the first would be 
   !      1) current marginal prices, current capacity - hourly load excluding nat. gas
   !      2) CT/CC marginal prices, nat. gas. capacity
   
   
   use ephrts_swtiches
   use ephrts_file_unit_numbers
   use hourly_restore_data
   
   implicit none
   include 'parametr'
   include 'ncntrl'
   include 'qblk'
   include 'mpblk'
   include 'emmparm'
   include 'control'
   include 'bildout'
   include 'emission'
   include 'cogen'
   include 'uefpout'     ! UEFP output variables
   include 'uefdout'     ! EFD output variables
   include 'udatout'     ! UDAT output variables
   include 'uecpout'     ! UECP output variables
   include 'uldsmout'    ! LDSM output variables
   include 'uettout'     ! UETT output variables
   include 'ecpcntl'
   include 'entcntl'
   include 'dispuse'
   include 'elout'
   include 'dsmdimen'
   include 'dsmcaldr'
   include 'efprp2'
   include 'macout'
   
   ! iterators
   integer irg, s, months, days, hrs
   
   ! restore declarations
   integer p_type, bin
   
   integer s_days(4)
   integer, parameter :: month2season(12) = [4,4,1,1,2,2,2,2,3,3,4,4]
        
   real total_capacity(mnumnr,mnumyr), hourly_generation(mnumnr,24,4,mnumyr)
   logical file3, skip

   ! send to hmm
   !real :: electrolyzer_load_limits(mnumnr,24,4,mnumyr)
   real :: temp_electrolyzer_load_limits(mnumnr,24,4)

   real :: hourly_total_capacity_seasonal(28,4,24) ! nerc regions, hmm season, hour
   
   integer temp_year, temp_irg, temp_hrs, temp_s, reason
   real temp_value
   real :: electrolyzer_load_limitsCSV(mnumnr,24,4,mnumyr)
   
   ! todo: delete later when restart file
   
      !if(curcalyr.le.UPSTYR .and. curitr .eq. 1) then
      !   electrolyzer_load_limits = 0.0 
      !endif
   
      
   ! Use the delta dispatch from restore to set the load limit
   temp_electrolyzer_load_limits = 0.0
   do irg = 1, MNUMNR - 3
       do months = 1, 12
           s = month2season(months)
           do days=1,3
               do hrs = 1, 24 
                   do p_type = 1, MaxPtypesP
                       do bin = 1, MaxNumbinsP
                           
                           skip = .false.
                           if (p_type .eq. 20 .and. bin .eq. 2) then ! Skip end-use pv here, where 20 is PV and bin is enduse
                               skip = .true.
                           endif
                           
                           if ( skip .eq. .false.) then
                             temp_electrolyzer_load_limits(irg,hrs,s) = temp_electrolyzer_load_limits(irg,hrs,s) + DispatchDelta(1,irg,p_type,bin,months,days,hrs)* IDAYTQ(days,months)
                           endif
                       enddo
                   enddo
                enddo   
           enddo
       enddo
   enddo

   ! Set the load limit to the temp variable per year
   do irg = 1, MNUMNR - 3
       do s = 1, 4
           do hrs = 1, 24 
               LOADMAXH2EL(irg,s,hrs,curiyr) = temp_electrolyzer_load_limits(irg,hrs,s) !*0.5
           enddo
       enddo
   enddo
   
     
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   !         ! read in only once
   !if (curitr .eq. 1 .and. curcalyr .eq. upstyr) then
   !     
   !     electrolyzer_load_limitsCSV = 0.0
   !     
   !     ! read data in from csvs
   !       open(unit=342346, file=".\hmm\toaimms\emm_files\debug_electrolyzer_limits.csv", status='unknown')
   !       read(342346,*) ! skip header of file
   !       do
   !            
   !          read(342346,*,iostat=reason) temp_year, temp_irg, temp_hrs, temp_s, temp_value
   !          
   !          electrolyzer_load_limitsCSV(temp_irg,temp_hrs,temp_s,temp_year) = temp_value
   !      
   !          if (reason > 0)  then
   !               
   !             write(*, *) "warning - possible corrupted file when reading in csv file from hmm into emm"
			!    close(342346)
   !             exit
   !
   !          else if (reason < 0) then
   !             close(342346) 
   !             exit
   !          end if
   !       end do
   !end if
   !    
   !
   ! do irg = 1, MNUMNR - 3
   !         do s = 1, 4   
   !             do hrs = 1, 24
   !                   electrolyzer_load_limitsCSV(irg,hrs,s,curiyr) = LOADMAXH2EL(irg,s,hrs,curiyr)
   !             enddo
   !         enddo
   !    enddo
   !
   !
   !   if (curitr .eq. 1) then
   !
   !    open(209304, file=".\hmm\toaimms\emm_files\debug_electrolyzer_limits.csv", status="replace", action="write")
   !    write(209304, *) "mnumyr,mnumnr,m24,m4,LDLM"
   !         
   !    
   !    do temp_year = upstyr-1989, mnumyr
   !       do irg = 1, mnumnr - 3
   !         do  s = 1, 4
   !             do hrs = 1, 24  
   !                    write(209304,'(i0,",",i0,",",i0,",",i0,",",f0.3)') temp_year,irg,hrs,s,electrolyzer_load_limitsCSV(irg,hrs,s,curiyr)
   !             enddo
   !         enddo
   !      enddo
   !   enddo
   !   close(209304) 
   !  
   !   endif
   !   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   
     
      
    end subroutine 
    
    
   subroutine send_to_hmm(iteration, cycle_number)
   
   use dfport
   use ephrts_swtiches

   implicit none

   include'parametr'
   !include'ncntrl' ! conflicts with DFPORT
   include'emmparm'
   include'control'
   include'ecpcntl'
   include'dispin'
   include'dispout'
   include'dispuse'
   include'dispcrv'
   include'fuelin'
   include'bildin'
   include'dispett'
   include'uettout'
   include'postpr'
   include'dsmdimen'
   include'dsmsectr'
   include'dsmunits'
   include'dsmtoefd'
   include'dsmcaldr'
   
   integer iteration, cycle_number
    
   ! this calls the sec45 algorithm which returns an electricity price reducer based on policy emissions requirements
   
   ! EDT 04/16/24 Turning off the Sec45 hourly emissions because instead HMM is going to use 100% renewable EACs (energy certs)  
   ! HMM is only going to use zero emissions electricity through EACs, keeping this code commented out in case in the future we want to use it
   !call send_hmm_sec45v_hourly_emissions ! uses previous years data since efd is run after ephrts !current_year-1989-1,

   ! send hydrogen consumed from efd to hmm via restart variable
   call send_hmm_hydrogen_consumption
   
   
   ! send hmm hourly restore electricity prices
   call send_hmm_hourly_restore_data ! Always send this to HMM
   
   ! this call is for computing the fuel costs, it lags an additional year, note the ephrts_aimms_wrapper's start_year is yearpr+2 so here we do yearpr+3 by setting start_year + 1
   !if (current_year .gt. start_year+2) then 
   !   call read_ephrts_supply_curve_for_efp(current_year)
   !   call compute_upfuel_for_efp
   !endif
   
   ! fetch the d_fuel (electricity to create hydrogen) 
   !call get_d_fuel_from_ephrts
   
   
   call read_generation_delta_from_restore 
   
   ! send 45v eligible generation after the first cycle and first iteration
   !if (iteration .ne. 1 .and. cycle_number .ne. 1) then
       call send_hmm_45v_eligible_generation
   !end if
   
   call send_hmm_electrolyzer_load_limits
    
   end subroutine

    
    subroutine receive_from_hmm_populate_efd_ecp_aimms_vars
   
    implicit none
    include 'parametr'
    include 'emmparm'
    include 'efpcntrl'
    include 'control'
    include 'ncntrl'
    include 'mpblk'
    include 'qblk'
    include 'uefdout'
    include 'udatout'
    include 'ecp_nuc'
    include 'cdsparms'
    include 'emm_aimms'
    include 'hmmblk'
    include 'convfact'

    
    integer regions, steps, seasons, years
    !  REAL H2SCRV_Q(MNUMCR,H2STEP,3,MNUMYR)     ! Quantity from HMM
	!  REAL H2SCRV_P(MNUMCR,H2STEP,3,MNUMYR)     ! Prices from HMM

!	  REAL CFH2Q_KG           !  CONVERSION FACTOR KILOGRAMS PER MILLION Btu

    ! set prices and quantities to temp vars for emm_aimms common block

    !write(*,*) 'hmm p/q', 'region','season','step','e-p','e-q','h-p','h-q','CFH2Q_KG= ',CFH2Q_KG, 'Year=', curiyr+1989
    
    do regions = 1, MNUMCR
        do steps = 1, H2STEP
            do seasons = 1, 3
                
                    ephrts_prices(curiyr, regions, seasons, steps) = H2SCRV_P(regions, steps, seasons, curiyr) * CFH2Q_KG !* 1000.0 ! HMM : $1987/KG  -> $1987/KG * KGS/MILLION BTU = $1987/MILLION BTU  
                
                    !Making negative prices zero in order to avoid EFP EPRICE issues with negative prices
                    if (ephrts_prices(curiyr, regions, seasons, steps) .le. 0.0) then
                        ephrts_prices(curiyr, regions, seasons, steps) = 0.00001 
                    end if
                
                    ephrts_quantity(curiyr, regions, seasons, steps) =( ( H2SCRV_Q(regions, steps, seasons, curiyr) * 1000000000.0 ) / CFH2Q_KG ) / 1000000.0 ! HMM : quantity is million metric tons of H2 - > MMTONS H2 * 1000000000 kgs / mmtons -> mbtus / 1000000 - > tbtu

                    !write(*,*) regions, seasons, steps, ephrts_prices(curiyr, regions, seasons, steps), ephrts_quantity(curiyr, regions, seasons, steps), H2SCRV_P(regions, steps, seasons, curiyr), H2SCRV_Q(regions, steps, seasons, curiyr)
                    
            enddo
        enddo
    enddo
    
    end subroutine 
    
    
   subroutine receive_from_hmm_electricity_demand
   use ephrts_swtiches
   use ephrts_file_unit_numbers
   
   implicit none
   include 'parametr'
   include 'ncntrl'
   include 'emmparm'
   include 'ecpcntl'
   include 'control'
   include 'dsmdimen'
   include 'dsmcaldr'
   include 'hmmblk' ! home of QELHM_HR from HMM
   
   integer r, m, d, h, s, h_864 

! This routine takes the hourly electricity demand from HMM and converts it to hourly (864) variables (Power and Energy units) used within EMM by using mappers
! These variables h2_elec_consumed and h2_mwh_consumed are used later in EMM within the load duration submodule which accounts for the extra electricity demand from electrolyzers
   
!  REAL QELHM_HR(MNUMNR,4,24,MNUMYR) ! Hourly Electricity demand (electrolyzers) from HMM (MWhrs)
   
   integer, parameter :: month2season(12) = [4,4,1,1,2,2,2,2,3,3,4,4] ! todo : hmm puts into restart variable
!   integer, parameter :: totaldays(4) = [121,61,121,61] 
   integer :: totaldays(4) 

   ! intitialize parameters
   totaldays = 0.0

   ! compute totaldays used to compute electricity demand 
   do m = 1, 12
      s = month2season(m)
      do d = 1, 3
           totaldays(s) = totaldays(s) + idaytq(d,m) 
      enddo
   enddo
   
   !  write(*,*) 'QELHM_HR ', 'r ','m ','d ','h '
   
   ! get amount of electricity consumed for a given region and hour (864 index)
   do r = 1 , 25 ! nerc region
      h_864 = 0                                                                           
      do m = 1, 12 ! months
        
        ! fetch season  
        s = month2season(m)
          
        do d = 1, 3 ! daytype (1 : weekday, 2: weekend, 3: peak)
            do h = 1, 24 ! hours 
                     
               ! convert to 864
               ! 3 * 24 * 12
               h_864 = h_864 + 1
               
               ! electricity demand/ total number of days
               h2_elec_consumed(r,h_864) = QELHM_HR(r,s,h,curiyr) / totaldays(s) ! power units (megawatts)
               h2_mwh_consumed(r,h_864) = h2_elec_consumed(r,h_864) * idaytq(d,m) ! energy units (megawatts hours)
   
               ! sum of electricity consumed hourly over the year to compute annual amount
               annual_h2_mwh_consumed(r) = annual_h2_mwh_consumed(r) + h2_mwh_consumed(r,h_864) !total annual mwh consumed from hydrogen prodcution by region added to syload
               
                !if (QELHM_HR(r,s,h,curiyr) .gt. 0.0) then
                !    write(*,*) 'QELHM_HR', r, m, d, h, QELHM_HR(r,s,h,curiyr)
                !end if
               
            end do
         end do        
      end do
   end do
               
   end subroutine    
    
    
   subroutine receive_from_hmm_compute_upfuel_for_efp()
   ! Set the upfuel (the fuel price of hydrogen within EMM) to the fuel price provided by HMM. 
   use ephrts_file_unit_numbers
   use ephrts_swtiches
   use, intrinsic :: ieee_arithmetic, only: ieee_is_finite      

   implicit none
   include 'parametr'
   include 'ncntrl'
   include 'ampblk'
   include 'emmparm'
   include 'ecpcntl'
   include 'control'
   include 'bildin'
   include 'fuelin'
   
   integer region
   
   ! REAL PH2EL(MNUMCR,MNUMYR)              !  price of H2 to electric power sector by census division, year
   
   do region = 1, mnumcr ! mnumcr = 11
       upfuel(uigc,region) = PH2EL(region,curiyr)
   end do
   
   end subroutine
   
    
    
   subroutine receive_from_hmm
   
   use dfport
   use ephrts_swtiches

   implicit none

   include'parametr'
   !include'ncntrl' ! conflicts with DFPORT
   include'emmparm'
   include'control'
   include'ecpcntl'
   include'dispin'
   include'dispout'
   include'dispuse'
   include'dispcrv'
   include'fuelin'
   include'bildin'
   include'dispett'
   include'uettout'
   include'postpr'
   include'dsmdimen'
   include'dsmsectr'
   include'dsmunits'
   include'dsmtoefd'
   include'dsmcaldr'
    
   ! populate aimms variables with hmm supply curves
   call receive_from_hmm_populate_efd_ecp_aimms_vars
   
   ! get electricity demand (electrolyzer only) from hmm
   call receive_from_hmm_electricity_demand
   
   ! set fuel price from HMM
   call receive_from_hmm_compute_upfuel_for_efp
   
   end subroutine

    