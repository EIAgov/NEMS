! EMISSIONS POLICY MODULE.


!> Emissions Policy Module (EPM) primary driver routine.

!> Calculates carbon emissions and implements carbon policy options. The first
!> three policy options are mutually exclusive (one at a time), and the fourth
!> option is a variation on the third option:
!> 
!> 1. Carbon Tax: A nominal or real $ tax per kilogram of carbon for fossil
!> fuels. It is converted to a $/btu tax for each fuel/sector based on its
!> carbon content. Revenue from the tax is passed to the macroeconomic module.
!> There, treatment of such revenue (e.g., reducing the deficit or reducing
!> other taxes, etc.) depends on options in effect. Generally, large changes in
!> gov't revenue would require additional offline analysis to asses
!> macroeconomic feedbacks.
!> 
!> 2. Auction of Permits: A carbon goal is specified by the user. The goal or
!> cap is achieved by requiring an allowance to emit carbon. An auction
!> clearing price is determined, through the NEMS iterative process, that will
!> clear the auction market. Essentially, this option determines the carbon tax
!> (or allowance fee) necessary to achieve the total carbon cap. The auction to
!> price and allocate emissions allowances is assumed to operate with no
!> initial allocation of allowances. As in option 1, Carbon Tax, revenue from
!> the auction is passed to the macroeconomic module, where its effect may
!> require additional analysis.
!> 
!> 3. Market for Permits: Same as option 2, but permits are transferable within
!> the country (but not bankable). An initial distribution of emissions
!> permits, equal to base year emissions, is assumed to take place. As a
!> result, the revenue from the sale of allowances is assumed to be
!> redistributed back to the individual sectors. For regulated electric
!> utilities, the initial revenue from the allowance distribution would be
!> considered independent of the cost (or opportunity cost) of purchasing the
!> permits. The amount of this initial revenue or subsidy is calculated, but no
!> treatment of it is performed for pricing purposes. The full cost of the
!> permits, however, does feed through to the electricity price. Therefore, the
!> effect on the electricity price is probably overstated (unless marginal cost
!> pricing is assumed).
!> 
!> 4. Market for Permits with Emissions Offsets: If options 3 AND 4 are
!> selected, option 4 allows for the total cap on emission allowances to
!> increase through a supply of offsets. The amount of offsets for
!> reforestation (increasing carbon sequestration) and coal bed methane capture
!> are specified at various permit prices; the higher the price, the greater
!> the assumed offsets (i.e., a supply curve of exogenous emission allowance
!> offsets). The offset (in tons) available at a given allowance price is added
!> to the carbon goal.
!> 
!> 5. Early-Compliance Banking w/ Cap-and-Trade and Smooth Carbon Fee Growth: A
!> cap and trade with banking is implemented by finding the starting carbon
!> price, escalated at a fixed rate, that clears the bank over the compliance
!> period. The bank is determined as the sum of cap minus emissions over the
!> relevant period. The starting price is guessed based on results of prior
!> NEMS cycles. The projected prices are set in the start year based on the
!> guess, then the case is runs as a carbon fee case.

!> @see sum_emissions
!> @see epm_addoff
!> @see regfalsibank
!> @see accntrev
!> @see initrev
!> @see regfalsi
!> @see etax_adjust
!> @see price_adjust

subroutine epm

implicit none

include 'parametr'
include 'ncntrl'
include 'emeblk'
include 'epmmpblk'
include 'emoblk'
include 'emmparm'
include 'emission'
include 'uecpout'
include 'epmbank'
include 'macout'
include 'ghgrep'
include 'epmcntl'

! save variables read in so control variables are saved for each iteration
character*18, cntl, data  ! for control flags and data
integer ii, iy, is, io, ie ,i  ! loop counters and ranges
real*4 new_tax  ! local variable with carbon penalty
real*4 r  ! real, after-tax escalation rate for fee case with banking (or discount rate)
real dollarftab, pbefore, pafter
real wgtguess, e_goal, e_goal_future
real bank(mnumyr)
save bank
integer rtovalue
external rtovalue

real begin_and_end(2, mnumyr)  ! position 1 is beginning tax; 2 is ending tax; write this out at end
integer banksafety  ! run time option. with banking & safety-valve, if banksafety = 1, sets price in bank_endyr to safety-valve price
integer bioseqok  ! run time option. with offsets, bioseqok = 1, bio sequestration offsets count towards goal. 0==> incentives given but don't count toward goal
real qoff  ! temporarily holds the quantity of offsets that count towards the goal

! set this to 1 allowance credit per international offset
! HR2454 provided 1 allowance credit per 1.25 international offset credit after 2017.  1.00 / 1.25 = 0.8
real allow_per_offset /1.0/

integer ip, iminyr, ils, ile, idir
character*1 asafety  ! used to flag price at safety-valve level in epmout.txt 
integer iunit1, bank_onyr
common /epm_out/ iunit1, bank_onyr
real epm_addoff

if (curitr == 1) then
    new_tax = emtax(curiyr)
    begin_and_end(1, curiyr) = new_tax  ! keep track to write out summary of beginning and ending tax
endif

r = 7.4  ! Real escalation rate in % for fee case with banking
r = real(rtovalue('BANKDSCR', 74)) / 10.0  ! now entered in as permille. change to percent.

bioseqok = rtovalue('BIOSEQOK', 1)  ! if 1, bio sequestration counts as an offsets.  0: incentive only

offset(1:5, curiyr) = 0
dollarftab = mc_jpgdp(yearpr - 1989)

call sum_emissions
emsol(1, curiyr) = total_emissions(curiyr)  ! covered emissions only

! Energy-Related Methane Emissions No longer estimated. zero them to avoid confusion
emmethane(1:18, :) = 0.

! Record Data for Electric Utility Resource Calculations
! mechanism for setting a phantom carbon fee that will be incorporated for EMM planning purposes
! but not get added to energy prices
if (bank_flag .and. bank_startyr > ijumpcalyr .and. curiyr == firsyr .and. curitr == 1) then
    write(iunit1, '(a)') '  Applying a phantom carbon allowance as follows:'
    write(iunit1, '(/"Year    Allowance")')
    do iy = bank_priceyr, lastcalyr
        if (etax_flag) then
            write(iunit1, '(i4,3x,f10.3)') iy, emtax(iy - 1989)
        else
            write(iunit1, '(i4,3x,f10.3)') iy, emtax(iy - 1989) * 1000.
        endif
    enddo
    emetax(1, :) = emtax(:)
    emetax(2, :) = emtax(:)
elseif ((permit_flag .or. market_flag) .and. .not. bank_flag &
  .and. curiyr == firsyr .and. curitr == 1 .and. bank_startyr <= ijumpcalyr) then
    ip = bank_priceyr - 1989  ! start of allowance pricing (earlier than bank_startyr)
    is = bank_startyr - 1989  ! start of a cap and trade period, interpreted here even if bank_flag false
    if (ip < is) then ! interval between passage and start of cap and trade
        do iy = is-1, ip, -1
            emtax(iy) = emtax(iy + 1) / (1. + r/100.)
        enddo
    endif
endif

if ((tax_flag .or. permit_flag .or. market_flag) .and. bank_flag &
  .and. curiyr == firsyr .and. curitr == 1 .and. bank_startyr <= ijumpcalyr) then
    ! evaluate bank balance and set new bracket and price guess as appropriate.  Sets up the fee case for
    ! this cycle to begin in the the following model year.
    ip = bank_priceyr - 1989  ! start of allowance pricing (earlier than bank_startyr)
    is = bank_startyr - 1989  ! start of a cap and trade period that includes banking (year index)
    io = bank_onyr - 1989     ! First year within cap-and-trade period in which banking is allowed (normally equal to bank_startyr but can be later if borrowing in first few years would results (year index) 
    if (bank_endyr == 0) bank_endyr = baseyr + lastyr - 1
    ie = bank_endyr - 1989  ! end of banking, year index

    write(iunit1, *)            'Before Guessing new prices (results of prior run)'
    write(iunit1, '(a,i4)')     ' Bank Balance end target year ', bank_endyr
    write(iunit1, *)            'year iter covered emissions  offset  Bank    Carbon  Carbon    Expected'
    write(iunit1, '(a,i2.2,a)') '           solution    goal    used  Balance  Fee 87$ Fee ', yearpr - 2000, '$'// ' Fee 87$'

    wgtguess = real(rtovalue('WGTGUESS', 50)) / 100.

    ! overwrite the taxes read from epmdata using those in the restart file, since this
    ! banking option requires iterative trials.  Exception is when starting from a restart
    ! file without taxes set.
    if (isnan(sum(emetax(1, ip:lastyr)))) then  ! ADR TODO
    else
        if (sum(emetax(1, ip:lastyr)) > 0.) then
            emtax(ip:lastyr) = emetax(1, ip:lastyr)
        endif
    endif

    ! Calculate balance based on covered emissions, goal, offset used, and bank balance from last run
    balance(1:io-1) = 0.0
    bank(1:io-1) = 0.0

    do iy = io, lastyr  ! accumulate from onset of banking to projection end
        qoff = epm_addoff(offset, 5, mnumyr, baseyr, iy, bioseqok, allow_per_offset, 2012)
        bank(iy) = emissions_goal(iy) + qoff-emsol(1, iy)   
        balance(iy) = balance(iy - 1) + bank(iy)
    enddo

    do iy = ip, lastyr
        asafety = ' '
        if (emtax(iy) >= (max_tax(iy) - .0001)) asafety = '!'
        if (bank_flag) then
            if (emtax(iy - 1) /= 0 .and. (emtax(iy) / emtax(iy - 1)) > (1. + r/100.)) then
                asafety = 'X'
            endif
        endif
        qoff = epm_addoff(offset, 5, mnumyr, baseyr, iy, bioseqok, allow_per_offset, 2012)
        
        write(iunit1, '(i5,i5,2f9.1,f8.1,f8.0,f8.2,a1,3f8.2)') iy + baseyr - 1, iter, &
            emsol(1, iy), emissions_goal(iy), qoff, balance(iy), &   
            emtax(iy) * 1000., asafety, emtax(iy) * 1000. * dollarftab, emetax(1:1, iy) * 1000.
    enddo

    ! only reset banking prices in integrated runs where maxitr > 0. Allows standalone testing
    if (maxitr > 0) then  

        ! Banking price path determination: The objective is normally to determine the starting price that results in a zero balance
        ! the target year (bank_endyr or year index IE). The prices during the bank period grow at the annual rate of r, the banking 
        ! discount rate or required rate of return.
        
        r = 7.4  ! Real escalation rate in % for fee case with banking
        r = real(rtovalue('BANKDSCR', 74)) / 10.0  ! now entered in as permille. change to percent.
        pbefore = emtax(io)
        emtax(1:ip-1) = 0.
        call regfalsibank(emtax(io), bank_end_balance - balance(ie))

        ! If there is a safety-valve provision, however, this approach may yield a price path resulting in a zero
        ! bank balance but then a jump up in price in the next
        ! year exceeding the banking rate of increase. And the solution with a later bank year causes the safety valve 
        ! price to occur earlier in the banking period. So the option banksafety=1 is used to get the best alternative: 
        ! the bank period ends and the safety-valve price is reached in the same year. With this option, we set
        ! the price in the bank_endyr = IE equal to the safety valve price and accept a negative cumulative banking balance
        ! in the end year. The bank balance should change from positive to negative in the year the safety-valve is first hit.      
        ! This option is normally used after the bank_endyr has been determined by trial and error with banksafety = 0
        banksafety = rtovalue('BANKSAFE', 0)
        if (banksafety == 1) then
            emtax(io) = max_tax(ie) / ((1. + r/100.) ** (ie - io))  
            write(iunit1, '(a,f10.3)') 'BANKSAFETY=1, so safety-valve price in Bank_Endyr with starting price of:', emtax(io) * 1000.
        endif

        if (emtax(io) > max_tax(io)) then
            emtax(io) = max_tax(io)
            write(iunit1, '(a,f10.3)') 'Setting starting tax to safety-valve (max) of:', emtax(io)
        endif

        ! Have price in banking period grow at the discount rate, r, subject to safety valve, if applicable
        do iy = io+1, ie
            emtax(iy) = min(max_tax(iy), emtax(iy - 1) * (1. + r/100.))  ! the "min(max_tax(iy)," applies the allowance cost safety valve
        enddo

        ! In non-banking years, applying the perfect foresight guessing algorithm
        do iy = is, lastyr
            if(iy < io .or. iy > ie) then
                emtax(iy) = min(max_tax(iy), emetax(1, iy) * (1. - wgtguess) + emetax(2, iy) * wgtguess)  ! the "min(max_tax(iy)," applies the allowance cost safety valve
            endif
        enddo

        ! In years prior to start of the compliance period ("is"=bank_startyr-1989), establish an allowance price for planning purposes
        ! that does not actually get added into delivered energy prices.
        if (is > ip) then
            do iy = is-1, ip, -1
                emtax(iy) = emtax(iy + 1) / (1. + r/100.)
            enddo
        endif

        write(iunit1, *) ' '
        write(iunit1, *) 'Emission fees after guessing new start price'
        write(iunit1, *) ' year iter Carbon  Carbon'
        write(iunit1, '(a,i2.2,a)') '           Fee 87$ Fee ', yearpr - 2000, '$'
        do iy = ip, lastyr
            asafety = ' '
            if (emtax(iy) >= (max_tax(iy) - .0001)) asafety = '!'
            if (bank_flag) then
                if (emtax(iy - 1) /= 0 .and. (emtax(iy) / emtax(iy - 1)) > (1. + r/100.)) then
                    asafety = 'X'
                endif
            endif
            write(iunit1, '(i5,i4,f8.2,a1,f8.2)') iy + baseyr - 1, iter, emtax(iy) * 1000., asafety, emtax(iy) * 1000. * dollarftab
        enddo

        emetax(1, ip:) = emtax(ip:)  ! starting values and expectations.
        emetax(2, ip:) = emtax(ip:)  ! save this run's guess
    endif
endif

! Driver loop for policy options
! energy tax only: call etax_adjust, count revenue and emissions
if (etax_flag) then
    call accntrev
endif

iy = curiyr
balance(iy) = 0.0
bank(iy) = 0.0
banking(iy) = 0.0
if (bank_flag .and. (tax_flag .or. permit_flag .or. market_flag) .and. curcalyr >= bank_onyr) then
    ! if banking allowance, accumulate the bank balance by adding in excess emission reductions (or subtracting 
    ! insufficient emissions, taking into account any offsets allowed.
    qoff = epm_addoff(offset, 5, mnumyr, baseyr, iy, bioseqok, allow_per_offset, 2012)
    bank(iy) = emissions_goal(iy) + qoff - emsol(1, iy)  
    banking(iy) = emissions_goal(iy) + qoff - emsol(1, iy)
    balance(iy) = balance(iy - 1) + bank(iy) 
endif

! if banking option on, switch to year-by-year cap once compliance period
! starts.  
if (bank_flag) then
    if (tax_flag .or. permit_flag .or. market_flag) then
        if (curcalyr > bank_endyr) then 
            tax_flag = .false.
            permit_flag = .true.
            market_flag = .false.
        elseif (curcalyr >= bank_onyr .and. curcalyr <= bank_endyr) then
            tax_flag = .true.
            permit_flag = .false.
            market_flag = .false.
        elseif (curcalyr >= bank_startyr .and. curcalyr < bank_onyr) then
            tax_flag = .false.
            permit_flag = .true.
            market_flag = .false.
        elseif (curcalyr < bank_startyr) then
            tax_flag = .true.
            permit_flag = .false.
            market_flag = .false.
        endif
    endif
endif

if ((permit_flag .or. market_flag) .and. curitr == 1) then
    e_goal = emissions_goal(iy)
    if (bank_flag .and. (bank_startyr /= bank_endyr) .and. (curcalyr > bank_endyr) .and. curitr == 1) then
        ! Constrain price growth after bank period ends to be no more than the price trajectory
        ! that would encourage more trajectory. See epmout.txt to see where excess post-banking price growth 
        ! in any particular year is flagged.  If so, a longer banking interval, or maybe another banking interval, is
        ! indicated.  Since this code only supports one banking interval, the maximum post-banking price growth is set to
        ! the banking discount rate.
        ie = bank_endyr - 1989  ! end of banking, converted year index
        max_tax(iy) = min(max_tax(iy), emtax(ie) * ((1. + r/100) ** (curcalyr - bank_endyr)))

        ! in post banking period, alter the normal emissions goal to include any over- or under-compliance remaining from prior year
        if (curcalyr > bank_endyr) then  ! add in any excess end-of-banking balance to current year goal.
            e_goal = e_goal + (balance(iy - 1) - bank_end_balance)
        endif
    endif
endif

if (dbugepm > 0 .and. (market_flag .or. permit_flag)) then
    if (curitr == 1) then
        write(iunit1, 201)
201     format(/1x,'          Current  Covd           Emiss  Over    Tax Bracket  Carb Bracket   New'/ &
            1x,'Year Iter   Tax    Emiss Offsets  Goal  (under)   Low   High    Low   High   Tax   Slope'/ &
            1x,'---- ---- ------- ------ ------- ------ ------ ------------- ------------- ------ -------')
    endif
    if (curitr <= maxitr + 1) then
        if (etax_flag) then
            write(iunit1, 204) curiyr + baseyr - 1, curitr, new_tax
204         format(2i5,f8.2,$)
        else
            write(iunit1, 202) curiyr + baseyr - 1, curitr, new_tax * 1000.
202         format(2i5,f8.1,$)
        endif

        qoff = epm_addoff(offset, 5, mnumyr, baseyr, curiyr, bioseqok, allow_per_offset, 2012)

        write(iunit1, 203) total_emissions(curiyr), qoff,emissions_goal(iy), & 
            total_emissions(curiyr) - qoff - emissions_goal(iy)
203     format(f7.1,f8.1,2f7.1,$)
    endif
endif

! tax only: just compute tax and total revenue raised
if (tax_flag .or. permit_flag .or. market_flag) then
    call accntrev

    ! for markets, sum emissions, revenue,
    ! need to subtract initial allocs from revenue and find new tax
    if (market_flag) call initrev
endif

if (ncrl == 1 .and. (permit_flag .or. market_flag)) then
    begin_and_end(2, curiyr) = emtax(curiyr)
    if (curiyr == lastyr) then
        write(iunit1, *)
        write(iunit1, '(a,i2,a)') "Beginning and ending allowance fees during this cycle (cycle ", curirun, "):"
        write(iunit1, '(8x,a)') "Year    Begin      End"
        do i = 1, lastyr
            if (begin_and_end(1, i) /= 0.0 .or. begin_and_end(2, i) /= 0.0) &
                write(iunit1, '(8x,i4,2f9.2)') i + 1989, begin_and_end(1, i) * 1000., begin_and_end(2, i) * 1000.
        enddo
    endif
endif

if (ncrl == 1) return

! call a scalar search routine to determine the new tax/permit price
if (permit_flag .or. market_flag) then
    qoff = epm_addoff(offset, 5, mnumyr, baseyr, curiyr, bioseqok, allow_per_offset, 2012)
    call regfalsi(new_tax, total_emissions(curiyr) - qoff - e_goal)
endif

! store new tax rate in emission block
emtax(curiyr) = new_tax

! If energy tax flag set, adjust prices with btu tax
if (etax_flag) then
    call etax_adjust

! For any flag with a carbon tax/trading fee, add the fee to energy prices
elseif (tax_flag .or. market_flag .or. permit_flag) then
    call price_adjust
endif

if (dbugepm > 0 .and. (.not. market_flag .and. .not. permit_flag) .or. bank_flag) then
    if (.not. bank_flag .or. (fcrl == 1 .and. curcalyr >= bank_startyr)) then
        write(iunit1, '(/a,3i5)') 'curcalyr,curitr,iter=', curcalyr, curitr, iter
        do ii = 1, 12
            write(iunit1, 103) 'Emrev(ii,curiyr)', ii, emrev(ii, curiyr)
103         format(a28,i2,f16.6)
        enddo
        write(iunit1, 102) 'Total Covered Emissions:', total_emissions(curiyr)
        write(iunit1, 102) 'Emissions Goal,  e_goal:', emissions_goal(curiyr), e_goal
        write(iunit1, 102) 'Offsets (if any)       :', (offset(ii, curiyr), ii = 1, 5)
        qoff = epm_addoff(offset, 5, mnumyr, baseyr, curiyr, bioseqok, allow_per_offset, 2012)
        write(iunit1, 102) 'Over (under)compliance :', emissions_goal(curiyr) + qoff - total_emissions(curiyr) 
        write(iunit1, 102) 'Allowance Bank Balance :', balance(curiyr)
        write(iunit1, 102) 'Emissions Fee (87$,01$):', emtax(curiyr) * 1000., emtax(curiyr) * 1000. * dollarftab
        write(iunit1, 102) ' '
102     format(a30,5f16.6)
    endif
endif

if ((bank_flag .or. offset_flag) .and. curiyr == lastyr .and. fcrl == 1) then
    write(iunit1, '(a)') ' "!" next to fee indicates it is greater or equal to max_tax (safety).'
    if (bank_flag) then
        write(iunit1, '(a)') ' "X" next to fee indicates it grew faster than the bank discount'
        write(iunit1, '(a,f5.2,a)') '      rate, r=', r, ' suggesting the bank_end_year should be moved out.'
    endif
    write(iunit1, '(a)')                ' Summary of Results         |Covered |Uncovd|'
    write(iunit1, '(a)')                ' year iter|covered emissions|Abatem. |offset|Banking  Bank   |Carbon   Carbon   Expected Fees'
    write(iunit1, '(a,i2.2,2a,i2.2,a)') '          |solution    goal |Oth.GHG | used |        Balance |Fee 87$  Fee ', yearpr - 2000, '$', '  Fee 87$ Fee ', yearpr - 2000, '$'

    do iy = bank_priceyr - 1989, lastyr
        asafety = ' '
        if (emtax(iy) >= (max_tax(iy) - .00001)) then
            asafety = '!'
        endif
        if (bank_flag) then
            if (emtax(iy - 1) /= 0 .and. (emtax(iy) / emtax(iy - 1)) > (1. + r/100.)) then
                asafety = 'X'
            endif
        endif
        qoff = epm_addoff(offset, 5, mnumyr, baseyr, iy, bioseqok, allow_per_offset, 2012)
        write(iunit1, '(i5,i5,2f9.1,4f8.1,f8.2,a1,3f8.2)') iy + baseyr - 1, iter, &
            emsol(1, iy), emissions_goal(iy),                                     &
            sum(ghg_abate(iy, 1:14)) - offset(1, iy),                             &
            qoff,                                                                 &
            bank(iy), balance(iy),                                                &
            emtax(iy) * 1000., asafety,                                           &
            emtax(iy) * 1000. * dollarftab,                                       &
            emetax(2, iy) * 1000.,                                                &
            emetax(2, iy) * 1000. * dollarftab
    enddo
endif

return

120 write(6, *) ' File access or end-of-file Input Error in EPM '  ! ADR TODO

return
end subroutine epm


!> Add up and return qualifying offsets.

!> Depends on whether biosequestration is okay, the exchange rate for
!> international offset credits, and the year.
!> 
!> @param offset Array of offset values with dimensions (ncat, inumyr).
!> @param ncat Number of offset categories (?).
!> @param inumyr Number of offset years (?).
!> @param baseyr EPM history base year (?).
!> @param iy Current year (?).
!> @param bioseqok Whether biosequestration is okay (1) or not (0).
!> @param allow_per_offset Exchange rate for international offset credits.
!> @param offyear Year that offsets begin (?).

function epm_addoff(offset, ncat, inumyr, baseyr, iy, bioseqok, &
                    allow_per_offset, offyear)

implicit none

real epm_addoff

integer ncat, inumyr, baseyr, iy, bioseqok, offyear
real offset(ncat, inumyr), allow_per_offset
intent(in) :: ncat, inumyr, baseyr, iy, bioseqok, allow_per_offset
intent(in) :: offset, offyear

integer calyr
real qoff

calyr = baseyr + iy - 1

if (bioseqok == 1) then

    ! Carbon sequestration counts as an offset
    if (calyr > offyear) &
        qoff = offset(1, iy) + offset(2, iy) + allow_per_offset * offset(3, iy) + offset(4, iy)
    if (calyr <= offyear) &
        qoff = offset(1, iy) + offset(2, iy) + offset(3, iy) + offset(4, iy)

else

    ! Omit offset(2, iy) carbon sequestration if incentive only
    if (calyr > offyear) &
        qoff = offset(1, iy) + allow_per_offset * offset(3, iy) + offset(4, iy)
    if (calyr <= offyear) &
        qoff = offset(1, iy) + offset(3, iy) + offset(4, iy)

endif

epm_addoff = qoff

return
end function epm_addoff


!> Use regula falsi root finding to determine the new tax/permit price.

!> The regula falsi routine finds the root of a function given two points, one
!> with positive and one with negative functional value. It has linear
!> convergence rate, and is guaranteed to converge if the function is
!> continuous.
!> 
!> @param new_tax Latest tax rate.
!> @param new_sum Latest sum of pollutants.

subroutine regfalsi(new_tax, new_sum)

implicit none

include 'parametr'
include 'ncntrl'
include 'emmparm'
include 'emission'
include 'emoblk'
include 'emablk'
include 'emeblk'

real*4 new_tax, new_sum
intent(inout) :: new_tax
intent(in) :: new_sum

real*4 low_sum,           &  ! lower sum of pollutants
       high_sum,          &  ! higher sum of pollutants
       low_tax,           &  ! tax rate for lower sum of pollutants
       high_tax,          &  ! tax rate for higher sum of pollutants
       tol /1.0/,         &  ! tolerance on reaching pollution goal
       epm_factor /3.00/, &  ! the factor to branch outward to get the bracketing points
       base_tax              ! used to calculate tax in first iteration
integer j, i
integer iunit1, bank_onyr, iter
common /epm_out/ iunit1, bank_onyr
real newsum(11), newtax(11)  ! store new_sum, new_tax by iteration
real slope
save  ! need to save between iterations
logical bracket /.false./  ! do we have two points bracketing zero (note we subtracted the goal)

! first iteration, set upper and lower points equal to only point
j = curiyr
slope = -epm_factor
iter = min(11, curitr)
newsum(iter) = new_sum
newtax(iter) = new_tax

! first iteration need only find one point
if (curitr == 1) then
    bracket = .false.
    low_sum = new_sum
    high_sum = new_sum
    do i = 2, 11
        newsum(i) = 0.
        newtax(i) = 0.
    enddo

    ! epm is run last, but the initial tax for this run is
    ! already read in. we look at what was actually used through
    ! the adjustments in the restart file. if the factors have
    ! changed, this is an estimate if jngel = 0 assume no restart,
    ! then we use the starting value from the input file, new_tax
    if (etax_flag) then
        base_tax = jngel(j)
    else
        base_tax = jngel(j) / engel(j)
    endif
    high_tax = base_tax
    low_tax = base_tax

    if (base_tax > 0.00001 .and. etax_flag) then
        new_tax = new_tax + new_sum / 100. * epm_factor
    elseif (base_tax > 0.00001) then
        new_tax = new_tax + new_sum / 1000. * epm_factor
    endif

    if (new_tax < min_tax(j)) new_tax = min_tax(j)
    if (new_tax > max_tax(j)) new_tax = max_tax(j)

    if (dbugepm > 0) then
        if (etax_flag) then
            write(iunit1, 203) low_tax, high_tax, &
                low_sum, high_sum, new_tax, slope
        else
            write(iunit1, 204) low_tax * 1000., high_tax * 1000., &
                low_sum, high_sum, new_tax * 1000., slope
        endif
    endif  ! (dbugepm)

    if (fcrl == 1) new_tax = newtax(iter)

    return
endif  ! iteration 1

! second iteration, set low and high correctly and find new point for third
! this starts with average of low and high and moves appropriately
if (curitr == 2) then
    if (new_sum > high_sum) then
        low_sum = high_sum
        low_tax = high_tax
        high_sum = new_sum
        high_tax = new_tax
    else
        low_sum = new_sum
        low_tax = new_tax
    endif

    ! check if bracketing
    if (low_sum * high_sum <= 0) then
        bracket = .true.
    endif
endif  ! end of iteration two

! for iterations greater than two, we continue
! if not bracketing, we need to find two points bracketing zero
if (curitr > 2) then
    if (low_sum * high_sum > 0) then

        ! if not bracketed previously, there are four possibilities
        ! 1. both still below zero, reset high and keep trying
        ! if carbon total still below, but tax higher, use it.
        ! this avoids certain anomalies in the function
        if (low_sum < 0 .and. new_sum < 0) then
            low_sum = new_sum
            low_tax = new_tax

        ! 2. now bracketing; use reg falsi
        elseif (low_sum < 0 .and. new_sum > 0) then
            high_sum = new_sum
            high_tax = new_tax
            bracket = .true.

        ! 3. also bracketing; use reg falsi
        elseif (low_sum > 0 .and. new_sum < 0) then
            low_sum = new_sum
            low_tax = new_tax
            bracket = .true.

        ! 4. not bracketing both over user lesser tax value: assume monoton
        elseif (low_sum > 0 .and. new_sum > 0) then
            high_sum = new_sum
            high_tax = new_tax
        endif  ! four cases, not bracketing before
    else

        !  bracketed, new positive value replaces old positive
        if (new_sum * low_sum > 0) then

            ! new_sum and low_sum both negative. new_sum becomes the NEW low_sum.
            low_sum = new_sum
            low_tax = new_tax
        else

            ! new_sum and high_sum both positive. new_sum becomes the NEW high_sum.
            high_sum = new_sum
            high_tax = new_tax
        endif  ! new_sum * low_sum w/r/t 0
    endif  ! previous bracket test
endif  ! curitr w/r/t 2

if (.not. bracket) then
    slope = -epm_factor
    if (newsum(iter) - newsum(iter - 1) /= 0.0) then
        if (etax_flag) then
            slope = (100. * (newtax(iter) - newtax(iter - 1))) &
                / (newsum(iter) - newsum(iter - 1))
        else
            slope = (1000. * (newtax(iter) - newtax(iter - 1))) &
                / (newsum(iter) - newsum(iter - 1))
        endif
        if (slope >= -.25) slope = -epm_factor
        if (slope <= (-1. * (epm_factor ** 2))) slope = -1. * (epm_factor ** 2)
    endif
    if (etax_flag) then
        new_tax = new_tax - slope * new_sum / 100.
    else
        new_tax = new_tax - slope * new_sum / 1000.
    endif
    if (new_tax > max_tax(j)) new_tax = max_tax(j)
    if (new_tax < min_tax(j)) new_tax = min_tax(j)
else

    ! bracketed, given two points r-f calculates new estimate
    if (abs(new_sum) > tol) then
        if (curitr /= 2) then

            ! if estimate bad three times in a row, and carbon tax not changing much (less than 5%),
            ! conclude opposite bracket tax is bad. to correct, give that tax bracket a little shove
            ! using the heuristic that it takes about a dollar/ton to change 1 mton
            if (newsum(iter) < 0 .and. newsum(iter - 1) < 0 .and. newsum(iter - 2) < 0 &
              .and. abs(newtax(iter) - newtax(iter - 1)) / (newtax(iter - 1) + .0001) < 0.05) then
                if (etax_flag) then
                    high_tax = high_tax + new_sum * .01
                else
                    high_tax = high_tax + new_sum * .001
                endif
                if (high_tax < 0.) high_tax = 0.
            elseif (newsum(iter) > 0 .and. newsum(iter-1) > 0 .and. newsum(iter-2) > 0 &
              .and. abs(newtax(iter) - newtax(iter - 1)) / (newtax(iter - 1) + .0001) < 0.05) then
                if (etax_flag) then
                    low_tax = low_tax + new_sum * .01
                else
                    low_tax = low_tax + new_sum * .001
                endif
                low_tax = min(max_tax(j), low_tax)
            endif
            new_tax = (high_tax * low_sum - low_tax * high_sum) &
                / (low_sum - high_sum)
        else
            new_tax = (high_tax + low_tax) / 2.  ! on iteration 2, do simple average
        endif

        if (new_tax > max_tax(j)) new_tax = max_tax(j)
        if (new_tax < min_tax(j)) new_tax = min_tax(j)
    endif  ! outside tolerance
endif  ! setting new value

! if it is last iteration, don't change tax from what models saw because EPM runs last
if (fcrl == 1) new_tax = newtax(iter)

if (dbugepm > 0) then
    if (etax_flag) then
        write(iunit1, 203) low_tax, high_tax, low_sum, high_sum, new_tax, slope
    else
        write(iunit1, 204) low_tax * 1000., high_tax * 1000., low_sum, high_sum, new_tax * 1000., slope
    endif
203 format(2f7.2,2f7.1,2f7.2)
204 format(6f7.1)
endif  ! (dbugepm)

return
end subroutine regfalsi


!> Calculate the price adjustment (carbon content) of end-use fuels.

!> In each case, we find the price that the demand sectors respond to and
!> adjust the price based on the carbon content of the fuel and the tax. This
!> version will use the new adjusted price blocks.

subroutine price_adjust

implicit none

include 'parametr'
include 'emmparm'
include 'cdsparms'
include 'ncntrl'
include 'macout'
include 'mpblk'
include 'ngtdmout'
include 'uefpout'  ! UEFP output variables
include 'uefdout'  ! EFD output variables
include 'udatout'  ! UDAT output variables
include 'uecpout'  ! UECP output variables
include 'uettout'  ! UETT output variables
include 'emeblk'
include 'emablk'
include 'epmmpblk'
include 'epmngtdm'
include 'emission'
include 'emoblk'
include 'convfact'
include 'indout'
include 'qblk'
include 'epmcntl'
include 'coalemm'
include 'control'
include 'pmmout'

integer iunit1, bank_onyr
common /epm_out/ iunit1, bank_onyr
logical doonce
integer i, j, k
integer x, y, z
real*4 costemis  ! temp var--helps to calculate aclel var
real*4 qtyemis  ! temp var--helps to calculate aclel var

! emissions tax in 87$'s
real*4 etax, etaxendu, etaxresd, etaxcomm, etaxtran, etaxelec, etaxhm
real totclclnr(nclut1)  ! total trills per sector
real e85_gasoline_share, e85_ethanol_share 
real sumchk, wgtguess
integer jstart, jend
external rtovalue
integer rtovalue

jstart = curiyr
jend = curiyr
if (curitr == 1) doonce = .true.

! run this routine
! for all years the first time through.  This is so that EMM will
! will have some initial tax values to work with in setting price
! expectations (for environmental adders).

if (curiyr == firsyr .and. curitr == 1) then
    sumchk = 0.0
    do j = firsyr, lastyr
        sumchk = sumchk + abs(emetax(1, j))  ! sums carbon tax using ftab variable
    enddo

    ! for carbon fee case, set values for taxes from epmdata.txt.
    ! If it is a cap case (permit_flag or market_flag or bank_flag true ) 
    ! then carbon fees will be reset based on what is in the restart file, 
    ! so we don't want to overwrite those numbers with the epmdata.txt values.
    ! Since banking cases start out with the "tax_flag" true, then switch to a
    ! "permit_flag" or "market_flag" cap case at the end of the
    ! banking interval, we want to avoid overwriting the restart file prices 
    ! for the bank_flag case too.
    if ((sumchk == 0.0 .or. tax_flag) .and. .not. bank_flag) then  
        do j = 1, lastyr
            emetax(1, j) = emtax(j)
            emetax(2, j) = emtax(j)
            if (nominal_flag) then
                emetax(1, j) = emtax(j) / mc_jpgdp(j)
                emetax(2, j) = emtax(j) / mc_jpgdp(j)
            endif
        enddo
    endif

    ! For electricity expectations, average taxes of prior run (Dad)
    ! with those of Dad's Dad (Granddad) for expectations
    if (.not. tax_flag ) then  ! must be cap case (permit or auction)
        write(iunit1, *) ' Year   Guess      Dad    His-Guess '
        do j = 1, lastyr

            ! Average Dad' results and Dad's guess for expectations based on runtime option wgtguess
            wgtguess = real(rtovalue('WGTGUESS', 50)) / 100.
            emtax(j) = (1 - wgtguess) * emetax(1, j) + wgtguess * emetax(2, j)
            write(iunit1, '(i5,4f10.4)') j + 1989, emtax(j), emetax(1, j), emetax(2, j)
            emetax(2, j) = emtax(j)  ! store guess
        enddo
    endif

    jend = lastyr

endif

do j = jstart, jend
    emetax(3, j) = max_tax(j)  ! Max tax or TAP or Safety-Value 

    ! set etax equal to the 87$ amount of emtax(j)
    if (nominal_flag) then
        etax = emtax(j) / mc_jpgdp(j)  ! convert to 87$'s
    else
        etax = emtax(j)  ! already in 87$'s
    endif

    emetax(1, j) = etax              ! carbon penalty -- for ftab rpt
    emlim(1, j) = emissions_goal(j)  ! carbon goal -- ftab
    etaxelec = etax
    etaxtran = etax
    etaxendu = etax
    etaxresd = etax 
    etaxcomm = etax
    etaxhm = etax
    if (elec_flag) then 
        etaxendu = 0.0  ! if elec-only, zero end-use tax copy
        etaxtran = 0.0
    endif
    if (tran_flag) then 
        etaxendu = 0.0  ! if elec-only, zero end-use tax copy
        etaxelec = 0.0
    endif
    if (resd_flag .or. elec_flag .or. tran_flag) etaxresd = 0.0  ! if residential excluded or elec-only, zero resd tax copy
    if (comm_flag .or. elec_flag .or. tran_flag) etaxcomm = 0.0  ! if commercial excluded or elec-only, zero comm tax copy

    ! natural gas, core (resid, comm, trans, ind, util)
    jgfrs(j) = 0.
    jgfcm(j) = 0.
    jgftr(j) = 0.
    jgfin(j) = 0.
    jgfrs(j) = etaxresd * egfrs(j)
    jgfcm(j) = etaxcomm * egfcm(j)
    jgftr(j) = etaxtran * egftr(j)
    jgfin(j) = etaxendu * egfin(j)
    jgfel(j) = etaxelec * egfel(j)

    ! natural gas, noncore (resid,comm,trans,ind,util)
    jgirs(j) = 0.
    jgicm(j) = 0.
    jgitr(j) = 0.
    jgiin(j) = 0.
    jgirs(j) = etaxresd * egirs(j)
    jgicm(j) = etaxcomm * egicm(j)
    jgitr(j) = etaxtran * egitr(j)
    jgiin(j) = etaxendu * egiin(j)
    jgiel(j) = etaxelec * egiel(j)

    ! natural gas, total (resid, comm, trans, ind, util)
    jngrs(j) = etaxresd * engrs(j)
    jngcm(j) = etaxcomm * engcm(j)
    jngtr(j) = etaxtran * engtr(j)
    jngin(j) = etaxendu * engin(j)
    jnqngpf(j) = etaxendu * enqngpf(j)  ! natural gas feed
    jngel(j) = etaxelec * engel(j)

    ! coal
    jclrs(j) = etaxresd * eclrs(j)
    jclcm(j) = etaxcomm * eclcm(j)
    jclin(j) = etaxendu * eclin(j)
    jclel(j) = etaxelec * eclel(j) * (1.0 - urcclsub)
    jmcin(j) = etaxendu * emcin(j)

    ! petroleum products
    jgptr(j) = etaxtran * egptr(j)
    jlpin(j) = etaxendu * elpin(j)
    jmgcm(j) = etaxcomm * emgcm(j)
    jmgtr(j) = etaxtran * emgtr(j)
    jmgin(j) = etaxendu * emgin(j)
    jjftr(j) = etaxtran * ejftr(j)
    jdsrs(j) = etaxresd * edsrs(j)
    jdscm(j) = etaxcomm * edscm(j)
    jdstr(j) = etaxtran * edstr(j)
    jdsin(j) = etaxendu * edsin(j)
    jdsel(j) = etaxelec * edsel(j)
    jksrs(j) = etaxresd * eksrs(j)
    jkscm(j) = etaxcomm * ekscm(j)
    jksin(j) = etaxendu * eksin(j)
    jprrs(j) = etaxresd * eprrs(j)
    jlgrs(j) = jprrs(j)
    jprcm(j) = etaxcomm * eprcm(j)
    jlgcm(j) = jprcm(j)
    jprtr(j) = etaxtran * eprtr(j)
    jlgtr(j) = jprtr(j)
    jetin(j) = etaxendu * eetin(j)
    jprin(j) = etaxendu * eprin(j)
    jbuin(j) = etaxendu * ebuin(j)
    jisin(j) = etaxendu * eisin(j)
    if ((qetin(mnumcr, j) + qprin(mnumcr, j) + qbuin(mnumcr, j) + qisin(mnumcr, j)) /= 0.0) then
        jlgin(j) = (qetin(mnumcr, j) * jetin(j) + qprin(mnumcr, j) * jprin(j) &
            + qbuin(mnumcr, j) * jbuin(j) + qisin(mnumcr, j) * jisin(j)) &
            / (qetin(mnumcr, j) + qprin(mnumcr, j) + qbuin(mnumcr, j) + qisin(mnumcr, j))
    else
        jlgin(j) = etaxendu * elgin(j)
    endif
    jetinpf(j) = etaxendu * eetinpf(j)
    jprinpf(j) = etaxendu * eprinpf(j)
    jbuinpf(j) = etaxendu * ebuinpf(j)
    jisinpf(j) = etaxendu * eisinpf(j)
    if ((qetinpf(mnumcr, j) + qprinpf(mnumcr, j) + qbuinpf(mnumcr, j) + qisinpf(mnumcr, j)) /= 0.0) then
        jnqlgpf(j) = (qetinpf(mnumcr, j) * jetinpf(j) + qprinpf(mnumcr, j) * jprinpf(j) &
            + qbuinpf(mnumcr, j) * jbuinpf(j) + qisinpf(mnumcr, j) * jisinpf(j)) &
            / (qetinpf(mnumcr, j) + qprinpf(mnumcr, j) + qbuinpf(mnumcr, j) + qisinpf(mnumcr, j))
    else
        jnqlgpf(j) = etaxendu * enqlgpf(j)
    endif
    jrlcm(j) = etaxcomm * erlcm(j)
    jrltr(j) = etaxtran * erltr(j)
    jrlin(j) = etaxendu * erlin(j)
    jrlel(j) = etaxelec * erlel(j)
    jrhtr(j) = etaxtran * erhtr(j)
    jrhel(j) = etaxelec * erhel(j)
    jrscm(j) = etaxcomm * erscm(j)
    jrstr(j) = etaxtran * erstr(j)
    jrsin(j) = etaxendu * ersin(j)
    jrsel(j) = etaxelec * ersel(j)
    jpfin(j) = etaxendu * epfin(j)
    jpcin(j) = etaxendu * epcin(j)
    jppin(j) = etaxendu * eppin(j)
    jppinpf(j) = etaxendu * eppinpf(j)
    jluin(j) = etaxendu * eluin(j)
    jpcel(j) = etaxendu * epcel(j)
    jsgin(j) = etaxendu * esgin(j)
    jotin(j) = etaxendu * eotin(j)
    jottr(j) = etaxtran * eottr(j)
    jmetr(j) = etaxtran * emetr(j)

    e85_gasoline_share = (trgne85 * cfrbob(j)) / (trgne85 * cfrbob(j) + ethne85 * cfetq(j))
    e85_ethanol_share = (ethne85 * cfetq(j)) / (trgne85 * cfrbob(j) + ethne85 * cfetq(j))
    jettr(j) = etaxtran * emgtr(j) * e85_gasoline_share 

    ! initialize coal adjustment variables  for quantity weighting
    costemis = 0
    qtyemis = 0.0
    jclelcdr(j, :) = 0
    do k = 1, nclut1
        jclclnr(j, k) = 0
        totclclnr(k) = 0
    enddo

    do i = 1, ndrgg
        do k = 1, nclut1
            jclclnr(j, k) = jclclnr(j, k) + etaxelec * qclclnr(i, j, k) &
                * cclclnr(i, j, k) * (1.0 - urcclsub)
            jclelcdr(j,1) = jclelcdr(j, 1) + etaxelec * qclclnr(i, j, k) &
                * cclclnr(i, j, k) * (1.0 - urcclsub)
            costemis = costemis + etaxelec * qclclnr(i, j, k) &
                * cclclnr(i, j, k) * (1.0 - urcclsub)
            totclclnr(k) = totclclnr(k) + qclclnr(i, j, k)
            qtyemis = qtyemis + qclclnr(i, j, k)
        enddo
    enddo

    ! divide total dollars by total trills in each sector
    do k = 1, nclut1
        if (totclclnr(k) > 0.0) then
            jclclnr(j, k) = jclclnr(j, k) / totclclnr(k)
        else
            jclclnr(j, k) = etaxelec * 25.
        endif
    enddo
    if (sum(totclclnr(:)) > 0.0) then  ! assign same value to both
        jclelcdr(j, 1) = jclelcdr(j, 1) / sum(totclclnr(:))
        jclelcdr(j, 2) = jclelcdr(j, 1) / sum(totclclnr(:))
    else
        jclelcdr(j, 1) = etaxelec * 25.
        jclelcdr(j, 2) = etaxelec * 25.
    endif
    if (qtyemis > 0) then
        jclel(j) = costemis / qtyemis
    else
        jclel(j) = 0
    endif

    ! adjusts by gas/utility regions
    jgfelgr(j) = etaxelec * egfelgr(j)
    jgielgr(j) = etaxelec * egielgr(j)
    jgcelgr(j) = etaxelec * egcelgr(j)

    if (dbugepm > 1) then
        write(iunit1, *) '---------------------'
        write(iunit1, *) '  J == ', j
        write(iunit1, *) '---------------------'
        write(iunit1, *) 'JMETR JETTR,JNGEL', jmetr(j), jettr(j), jngel(j)
    endif
    if (dbugepm > 2) then
        write(iunit1, *) '---ADJUSTED FUEL PRICES--== REGION 1 YEAR 1'
        write(iunit1, *) 'VARIABLES FROM MPRC --- PRICES'
        write(iunit1, 300) (mprc(1, j, k), k = 1, 10)
        write(iunit1, 300) (mprc(1, j, k), k = 11, 20)
        write(iunit1, 300) (mprc(1, j, k), k = 21, 30)
        write(iunit1, 300) (mprc(1, j, k), k = 31, 40)
        write(iunit1, 300) (mprc(1, j, k), k = 41, 50)
        write(iunit1, 300) (mprc(1, j, k), k=51, 60)
        write(iunit1, 300) (mprc(1, j, k), k=61, 70)
        write(iunit1, 300) (mprc(1, j, k), k=71, 80)
        write(iunit1, 310) (mprc(1, j, k), k=81, 91)
        write(iunit1, *)
        write(iunit1, *) 'VARIABLES FROM AMPRC'
        write(iunit1, 300) (amprc(1, j, k), k = 1, 10)
        write(iunit1, 300) (amprc(1, j, k), k = 11, 20)
        write(iunit1, 300) (amprc(1, j, k), k = 21, 30)
        write(iunit1, 300) (amprc(1, j, k), k = 31, 40)
        write(iunit1, 300) (amprc(1, j, k), k = 41, 50)
        write(iunit1, 300) (amprc(1, j, k), k = 51, 60)
        write(iunit1, 300) (amprc(1, j, k), k = 61, 70)
        write(iunit1, 300) (amprc(1, j, k), k = 71, 80)
        write(iunit1, 310) (amprc(1, j, k), k = 81, 91)
        write(iunit1, *)
        write(iunit1, *) 'NATURAL GAS UTILITY VARIABLES(NOT COMPLETED)'
        write(iunit1, *) ' ==== REGION 1 YEAR 1'
        write(iunit1, *) 'JGFELGR(1) = ', jgfelgr(1)
        write(iunit1, *) 'JGIELGR(1) = ', jgielgr(1)
        write(iunit1, *) 'JGCELGR(1) = ', jgcelgr(1)
        write(iunit1, *)
300     format(2x,5(f8.5,2x),/1x,5(f8.5,2x))
310     format(2x,6(f8.5,2x),/1x,5(f8.5,2x))
    endif
enddo

return
end subroutine price_adjust


!> Calculate the price adjustment (energy tax) of end-use fuels.

!> In each case, we find the price that the demand sectors respond to and
!> adjust the price based on the an energy tax. This version will use the new
!> adjusted price blocks.

subroutine etax_adjust

implicit none

include 'parametr'
include 'emmparm'
include 'cdsparms'
include 'ncntrl'
include 'macout'
include 'mpblk'
include 'ngtdmout'
include 'uefpout'  ! UEFP output variables
include 'uefdout'  ! EFD output variables
include 'udatout'  ! UDAT output variables
include 'uecpout'  ! UECP output variables
include 'uettout'  ! UETT output variables
include 'coalemm'
include 'emablk'
include 'epmmpblk'
include 'epmngtdm'
include 'emission'
include 'emoblk'
include 'convfact'
include 'indout'
include 'qblk'
include 'epmcntl'

integer iunit1, bank_onyr
common /epm_out/ iunit1, bank_onyr
integer i, j, k
integer x, y, z
real*4 costemis  ! temp var--helps to calculate aclel var
real*4 qtyemis  ! temp var--helps to calculate aclel var
real*4 etax, etaxendu, etaxresd, etaxcomm, etaxtran, etaxelec, etaxhm  ! emissions tax in 87$'s
real totclc1nr(nclut1)  ! total trills per sector
real sumchk
integer jstart, jend

jstart = curiyr
jend = curiyr

! run this routine for all years the first time through.
! this is so that EMM will have some initial tax values to work with in setting
! price expectations (for environmental adders).
if (curiyr == firsyr .and. curitr == 1) then
    sumchk = 0.0
    do j = firsyr, lastyr
        sumchk = sumchk + abs(emetax(1, j))  ! sums carbon tax using ftab variable
    enddo
    if (sumchk == 0.0) then
        do j = 1, lastyr
            emetax(1, j) = emtax(j)
            emetax(2, j) = emtax(j)
            if (nominal_flag) then
                emetax(1, j) = emtax(j) / mc_jpgdp(j)
                emetax(2, j) = emtax(j) / mc_jpgdp(j)
            endif
        enddo
    endif

    ! For electricity expectations, average taxes of prior run (Dad)
    ! with those of Dad's Dad (Granddad) for expectations
    if (permit_flag .or. market_flag) then  ! must be cap case (permit or auction)
        write(iunit1, *) ' Year   Guess      Dad    His-Guess '
        do j = 1, lastyr
            emtax(j) = (emetax(1, j) + emetax(2, j)) / 2.  ! Average Dad results and Dad's guess for expectations
            write(iunit1, '(i5,4f10.4)') j + 1989, emtax(j), emetax(1, j), emetax(2, j)
            emetax(2, j) = emtax(j)  ! store guess
        enddo
    endif

    jend = lastyr
endif

do j = jstart, jend

    ! set etax equal to the 87$ amount of emtax(j)
    if (nominal_flag) then
        etax = emtax(j) / mc_jpgdp(j)  ! convert to 87$'s
    else
        etax = emtax(j)  ! already in 87$'s
    endif

    emetax(1, j) = etax              ! btu tax -- for ftab rpt
    emlim(1, j) = emissions_goal(j)  ! carbon goal -- ftab
    etaxelec = etax
    etaxtran = etax
    etaxendu = etax
    etaxresd = etax
    etaxcomm = etax
    etaxhm = etax
    if (elec_flag) then
        etaxendu = 0.0  ! if elec-only, zero end-use tax copy
        etaxtran = 0.0  ! if elec-only, zero end-use tax copy
    endif
    if (tran_flag) then
        etaxendu = 0.0  ! if elec-only, zero end-use tax copy
        etaxelec = 0.0  ! if elec-only, zero end-use tax copy
    endif
    if (resd_flag .or. elec_flag .or. tran_flag) etaxresd = 0.0  ! if residential excluded or elec-only, zero resd tax copy
    if (comm_flag .or. elec_flag .or. tran_flag) etaxcomm = 0.0  ! if commercial  excluded or elec-only, zero comm tax copy

    ! natural gas, core (resid, comm, trans, ind, util)
    jgfrs(j) = etaxresd
    jgfcm(j) = etaxcomm
    jgftr(j) = etaxtran
    jgfin(j) = etaxendu
    jgfel(j) = etaxelec

    ! natural gas, noncore (resid, comm, trans, ind, util)
    jgirs(j) = etaxresd
    jgicm(j) = etaxcomm
    jgitr(j) = etaxtran
    jgiin(j) = etaxendu
    jgiel(j) = etaxelec

    ! natural gas, total (resid, comm, trans, ind, util)
    jngrs(j) = etaxresd
    jngcm(j) = etaxcomm
    jngtr(j) = etaxtran
    jngin(j) = etaxendu
    jnqngpf(j) = etaxendu  ! natural gas feed
    jngel(j) = etaxelec

    ! petroleum products
    jgptr(j) = etaxtran
    jlpin(j) = etaxendu
    jclrs(j) = etaxresd
    jclcm(j) = etaxcomm
    jclin(j) = etaxendu
    jclel(j) = etaxelec
    jmcin(j) = etaxendu
    jmgcm(j) = etaxcomm
    jmgtr(j) = etaxtran
    jmgin(j) = etaxendu
    jjftr(j) = etaxtran
    jdsrs(j) = etaxresd
    jdscm(j) = etaxcomm
    jdstr(j) = etaxtran
    jdsin(j) = etaxendu
    jdsel(j) = etaxelec
    jksrs(j) = etaxresd
    jkscm(j) = etaxcomm
    jksin(j) = etaxendu
    jlgrs(j) = etaxresd
    jprrs(j) = etaxresd
    jlgcm(j) = etaxcomm
    jprcm(j) = etaxcomm
    jlgtr(j) = etaxtran
    jprtr(j) = etaxtran
    jlgin(j) = etaxendu
    jetin(j) = etaxendu
    jprin(j) = etaxendu
    jbuin(j) = etaxendu
    jisin(j) = etaxendu
    jnqlgpf(j) = etaxendu
    jetinpf(j) = etaxendu
    jprinpf(j) = etaxendu
    jbuinpf(j) = etaxendu
    jisinpf(j) = etaxendu
    jrlcm(j) = etaxcomm
    jrltr(j) = etaxtran
    jrlin(j) = etaxendu
    jrlel(j) = etaxelec
    jrhtr(j) = etaxtran
    jrhel(j) = etaxelec
    jrscm(j) = etaxcomm
    jrstr(j) = etaxtran
    jrsin(j) = etaxendu
    jrsel(j) = etaxelec
    jpfin(j) = etaxendu
    jpcin(j) = etaxendu
    jpcel(j) = etaxelec
    jsgin(j) = etaxendu
    jotin(j) = etaxendu
    jottr(j) = etaxtran
    jmetr(j) = etaxtran
    jettr(j) = etaxtran

    do k = 1, nclut1
        jclclnr(j, k) = etaxelec
    enddo

    jclelcdr(j, 1) = etaxelec
    jclelcdr(j, 2) = etaxelec

    ! adjusts by gas/utility regions
    jgfelgr(j) = etaxelec
    jgielgr(j) = etaxelec
    jgcelgr(j) = etaxelec

    if (dbugepm > 1) then
        write(iunit1, *) '---------------------'
        write(iunit1, *) '  J == ', j
        write(iunit1, *) '---------------------'
        write(iunit1, *) 'JMETR JETTR,JNGEL', jmetr(j), jettr(j), jngel(j)
    endif
    if (dbugepm > 2) then
        write(iunit1, *) '---ADJUSTED FUEL PRICES--== REGION 1 YEAR 1'
        write(iunit1, *) 'VARIABLES FROM MPRC --- PRICES'
        write(iunit1, 300) (mprc(1, j, k), k = 1, 10)
        write(iunit1, 300) (mprc(1, j, k), k = 11, 20)
        write(iunit1, 300) (mprc(1, j, k), k = 21, 30)
        write(iunit1, 300) (mprc(1, j, k), k = 31, 40)
        write(iunit1, 300) (mprc(1, j, k), k = 41, 50)
        write(iunit1, 300) (mprc(1, j, k), k = 51, 60)
        write(iunit1, 300) (mprc(1, j, k), k = 61, 70)
        write(iunit1, 300) (mprc(1, j, k), k = 71, 80)
        write(iunit1, 310) (mprc(1, j, k), k = 81, 91)
        write(iunit1, *)
        write(iunit1, *) 'VARIABLES FROM AMPRC'
        write(iunit1, 300) (amprc(1, j, k), k = 1, 10)
        write(iunit1, 300) (amprc(1, j, k), k = 11, 20)
        write(iunit1, 300) (amprc(1, j, k), k = 21, 30)
        write(iunit1, 300) (amprc(1, j, k), k = 31, 40)
        write(iunit1, 300) (amprc(1, j, k), k = 41, 50)
        write(iunit1, 300) (amprc(1, j, k), k = 51, 60)
        write(iunit1, 300) (amprc(1, j, k), k = 61, 70)
        write(iunit1, 300) (amprc(1, j, k), k = 71, 80)
        write(iunit1, 310) (amprc(1, j, k), k = 81, 91)
        write(iunit1, *)
        write(iunit1, *) 'NATURAL GAS UTILITY VARIABLES(NOT COMPLETED)'
        write(iunit1, *) ' ==== REGION 1 YEAR 1'
        write(iunit1, *) 'JGFELGR(1) = ', jgfelgr(1)
        write(iunit1, *) 'JGIELGR(1) = ', jgielgr(1)
        write(iunit1, *) 'JGCELGR(1) = ', jgcelgr(1)
        write(iunit1, *)
300     format(2x,10(f8.5,2x))
310     format(2x,11(f8.5,2x))
    endif
enddo

return
end subroutine etax_adjust


!> Sum pollutants from each fuel to give aggregate totals across the country.

!> Determined by summing quantity of fuel use weighted by a conversion factor.
!> Note: we add only the first 9 regions, otherwise double count. We sum
!> emissions by sector:
!> 1. Residential
!> 2. Commercial
!> 3. Industrial
!> 4. Transportation
!> 5. Utility

!> @see epm

subroutine sum_emissions

implicit none

include 'parametr'
include 'emmparm'
include 'cdsparms'
include 'ncntrl'
include 'qblk'
include 'uefdout'  ! EFD output variables
include 'emeblk'
include 'emoblk'
include 'indout'
include 'emission'
include 'pmmrpt'
include 'convfact'
include 'coalemm'
include 'bifurc'
include 'cogen'
include 'hmmblk'
include 'ghgrep'  ! For regional co2 variables
include 'wrenew'  ! For wncmsel
include 'pmmout'
include 'pmmftab'
include 'lfmmout'
include 'calshr'
include 'qsblk'

integer i, j, k, jcalyr
real ccs(5, mnumnr)  ! Carbon captured and stored by 1:dist, 2:resid, 3:ng, 4:coal, and 5:total
common /ccsblk/ ccs  ! Currently not included in any other routine, but could be used in accntrev
real co2f  ! Temporary variable to hold co2 factor for a fuel
integer iunit1, bank_onyr
common /epm_out/ iunit1, bank_onyr
integer file_mgr
character*18 fname
external file_mgr
integer iy, is, i_fuel, iunit, doonce /0/, ifl, ic
real indshr(3)  ! Used to share historical carbon emissions to nems sectors
integer base_yr  ! Last year of history-overwrites (also, see hist in epm_read)
parameter(base_yr = 32)  ! 2021 is last data year when reading epmdata history
real emishist(4, 5, base_yr)  ! Historical carb by 4 fuels, 5 sect, year
common /emishistory/ emishist
real biodiesel  ! For biodiesel calcs
character*280 line
integer fileng
external fileng
real egeel  ! Temporary variable for geothermal emissions factor
real geogen_bkwh, geogen_bkwh_hist
real sumc
real e85_gasoline_share, e85_ethanol_share
real ethanol_tot, ethanol_e85, ethanol_blended, allrenewablegasoline, allrenewable_diesel
real adj_resd, adj_comm, adj_indy, adj_tran, reg_shr
real temp_reg(mnumnr)
character*25 nm_resd(iel_r)
character*25 nm_comm(iel_c)
character*25 nm_indy(iel_i)
character*25 nm_tran(iel_t)
character*25 nm_elec(etot), regnam(11)*20

! Names of the 10 regions (plus the entire country)
regnam(1) = 'New England '
regnam(2) = 'Middle Atlantic'
regnam(3) = 'East North Central'
regnam(4) = 'West North Central'
regnam(5) = 'South Atlantic'
regnam(6) = 'East South Central'
regnam(7) = 'West South Central'
regnam(8) = 'Mountain'
regnam(9) = 'Pacific'
regnam(10) = 'California'
regnam(11) = 'United States'

j = curiyr
jcalyr = curcalyr

! Read historical report data for Table 17 from EPMDATA file.
! GSV TODO: extract the contents of this if-block into a dedicated subroutine
! and re-structure its control flow to remove the gotos.
if (doonce == 0) then
    emishist = 0.
    doonce = 1
    iunit = file_mgr('O', 'EPMDATA           ', .false.)

929 read(iunit, '(a)', end=930) line
    if (index(line, 'RESIDE') /= 1) goto 929

    ! If @ found, no hist dat in file.
    if (index(line, '@') == 1) then
        write(6, *) 'no hist data in epm file'
        goto 930
    endif

    do is = 1, 5  ! 5 sector s
        do i_fuel = 1, 3  ! 3 fuel lines except sector 5
            read(iunit, '(a)') line
            ic = index(line, ',')
            if (ic > 0) then
                line(1:ic ) =' '
            endif
            read(line, *) (emishist(i_fuel, is, iy), iy = 1, base_yr)
        enddo
        read(iunit, '(a)') line
    enddo

    line(1:14) = ' '
    read(line, *) (emishist(4, 5, iy), iy = 1, base_yr)  ! Read fuel 4, electric sector 5, msw/geothermal

930 continue

    iunit = file_mgr('C', 'EPMDATA           ', .false.)
endif

! Coal - electric power emission factor
! Values for eclel are not read in. Only used for tax calcs because emissions
! factors for coal are specified on a coal rank/region basis elsewhere.
eclel(:) = 25.72  ! GHG pub 2003 value 10/4/2004

!! Begin residential sector carbon emissions !!

! Residential sector emissions by pollutant
emrs(1:4, 1, j) = 0.
emrsc(11, 1, j) = 0.
do i = 1, mnumcr - 2
    emrs(1, 1, j) = emrs(1, 1, j) + qgfrs(i, j) * egfrs(j) / 1000.
    emrs(1, 1, j) = emrs(1, 1, j) + qgirs(i, j) * egirs(j) / 1000.
    emrs(3, 1, j) = emrs(3, 1, j) + qclrs(i, j) * eclrs(j) / 1000.
    emrs(2, 1, j) = emrs(2, 1, j) + qdsrs(i, j) * edsrs(j) / 1000.
    emrs(2, 1, j) = emrs(2, 1, j) + qksrs(i, j) * eksrs(j) / 1000.
    emrs(2, 1, j) = emrs(2, 1, j) + qprrs(i, j) * eprrs(j) / 1000.
enddo
emrsc(11, 1, j) = emrs(1, 1, j) + emrs(2, 1, j) + emrs(3, 1, j) + emrs(4, 1, j)

! Residential sector emissions by fuel (initially populate array)
em_resd(1, 11, jcalyr) = qprrs(11, j) * eprrs(j)  ! Propane
em_resd(2, 11, jcalyr) = qdsrs(11, j) * edsrs(j)  ! Distillate
em_resd(3, 11, jcalyr) = qksrs(11, j) * eksrs(j)  ! Kerosene
em_resd(4, 11, jcalyr) = qclrs(11, j) * eclrs(j)  ! Coal
em_resd(5, 11, jcalyr) = qngrs(11, j) * engrs(j)  ! Natural gas
! Purchased electricity
do i = 1, mnumcr
    if (qelas(i, j) /= 0.) then
        em_resd(6, i, jcalyr) = qelrs(i, j) / qelas(i, j)  ! Initially just the sector share
    else
        em_resd(6, i, jcalyr) = 0.
    endif
enddo

! Residential sector fuel consumption
fl_resd(1, 1:11, jcalyr) = qprrs(1:11, j)  ! Propane
fl_resd(2, 1:11, jcalyr) = qdsrs(1:11, j)  ! Distillate
fl_resd(3, 1:11, jcalyr) = qksrs(1:11, j)  ! Kerosene
fl_resd(4, 1:11, jcalyr) = qclrs(1:11, j)  ! Coal
fl_resd(5, 1:11, jcalyr) = qngrs(1:11, j)  ! Natural gas
fl_resd(6, 1:11, jcalyr) = qelrs(1:11, j)  ! Purchased electricity

! Scale SEDS California number based on Pacific if there is no California number
if (qslgrs(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_resd(1, 10, jcalyr) = qslgrs(10, j) * qlgrs(9, j) / qslgrs(9, j)
if (qsdsrs(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_resd(2, 10, jcalyr) = qsdsrs(10, j) * qdsrs(9, j) / qsdsrs(9, j)
if (qsksrs(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_resd(3, 10, jcalyr) = qsksrs(10, j) * qksrs(9, j) / qsksrs(9, j)
if (qsclrs(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_resd(4, 10, jcalyr) = qsclrs(10, j) * qclrs(9, j) / qsclrs(9, j)
if (qsngrs(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_resd(5, 10, jcalyr) = qsngrs(10, j) * qngrs(9, j) / qsngrs(9, j)
if (qselrs(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_resd(6, 10, jcalyr) = qselrs(10, j) * qelrs(9, j) / qselrs(9, j)

! Names of the 5 residential sector fuels (plus electricity)
nm_resd(1) = 'Propane'
nm_resd(2) = 'Distillate'
nm_resd(3) = 'Kerosene'
nm_resd(4) = 'Coal'
nm_resd(5) = 'Natural Gas'
nm_resd(6) = 'Electricity'

!! End residential sector carbon emissions !!

!! Begin commercial sector carbon emissions !!

! Commercial sector emissions by pollutant
emcm(1:4, 1, j) = 0.
emcmc(11, 1, j) = 0.
do i = 1, mnumcr - 2
    emcm(1, 1, j) = emcm(1, 1, j) + qgfcm(i, j) * egfcm(j) / 1000.
    emcm(1, 1, j) = emcm(1, 1, j) + qgicm(i, j) * egicm(j) / 1000.
    emcm(3, 1, j) = emcm(3, 1, j) + qclcm(i, j) * eclcm(j) / 1000.
    emcm(2, 1, j) = emcm(2, 1, j) + qmgcm(i, j) * emgcm(j) / 1000.
    emcm(2, 1, j) = emcm(2, 1, j) + qdscm(i, j) * edscm(j) / 1000.
    emcm(2, 1, j) = emcm(2, 1, j) + qkscm(i, j) * ekscm(j) / 1000.
    emcm(2, 1, j) = emcm(2, 1, j) + qprcm(i, j) * eprcm(j) / 1000.
    emcm(2, 1, j) = emcm(2, 1, j) + qrscm(i, j) * erscm(j) / 1000.
enddo
emcmc(11, 1, j) = emcm(1, 1, j) + emcm(2, 1, j) + emcm(3, 1, j)

! Commerical sector emissions by fuel (initially populate array)
em_comm(1, 11, jcalyr) = qmgcm(11, j) * emgcm(j)  ! Motor gasoline
em_comm(2, 11, jcalyr) = qprcm(11, j) * eprcm(j)  ! Propane
em_comm(3, 11, jcalyr) = qdscm(11, j) * edscm(j)  ! Distillate
em_comm(4, 11, jcalyr) = qrscm(11, j) * erscm(j)  ! Residual fuel
em_comm(5, 11, jcalyr) = qkscm(11, j) * ekscm(j)  ! Kerosene
em_comm(6, 11, jcalyr) = qclcm(11, j) * eclcm(j)  ! Coal
em_comm(7, 11, jcalyr) = qngcm(11, j) * engcm(j)  ! Natural gas
! Purchased electricity
do i = 1, mnumcr
    if (qelas(i, j) /= 0.) then
        em_comm(8, i, jcalyr) = qelcm(i, j) / qelas(i, j)  ! Initially just the sector share
    else
        em_comm(8, i, jcalyr) = 0.
    endif
enddo

! Commercial sector fuel consumption
fl_comm(1, 1:11, jcalyr) = qmgcm(1:11, j)  ! Motor gasoline
fl_comm(2, 1:11, jcalyr) = qprcm(1:11, j)  ! Propane
fl_comm(3, 1:11, jcalyr) = qdscm(1:11, j)  ! Distillate
fl_comm(4, 1:11, jcalyr) = qrscm(1:11, j)  ! Residual fuel
fl_comm(5, 1:11, jcalyr) = qkscm(1:11, j)  ! Kerosene
fl_comm(6, 1:11, jcalyr) = qclcm(1:11, j)  ! Coal
fl_comm(7, 1:11, jcalyr) = qngcm(1:11, j)  ! Natural gas
fl_comm(8, 1:11, jcalyr) = qelcm(1:11, j)  ! Purchased electricity

! Scale SEDS California number based on Pacific if there is no California number
if (qsmgcm(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_comm(1, 10, jcalyr) = qsmgcm(10, j) * qmgcm(9, j) / qsmgcm(9, j)
if (qslgcm(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_comm(2, 10, jcalyr) = qslgcm(10, j) * qlgcm(9, j) / qslgcm(9, j)
if (qsdscm(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_comm(3, 10, jcalyr) = qsdscm(10, j) * qdscm(9, j) / qsdscm(9, j)
if (qsrscm(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_comm(4, 10, jcalyr) = qsrscm(10, j) * qrscm(9, j) / qsrscm(9, j)
if (qskscm(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_comm(5, 10, jcalyr) = qskscm(10, j) * qkscm(9, j) / qskscm(9, j)
if (qsclcm(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_comm(6, 10, jcalyr) = qsclcm(10, j) * qclcm(9, j) / qsclcm(9, j)
if (qsngcm(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_comm(7, 10, jcalyr) = qsngcm(10, j) * qngcm(9, j) / qsngcm(9, j)
if (qselcm(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_comm(8, 10, jcalyr) = qselcm(10, j) * qelcm(9, j) / qselcm(9, j)

! Names of the 7 commercial sector fuels (plus electricity)
nm_comm(1) = 'Gasoline'
nm_comm(2) = 'Propane'
nm_comm(3) = 'Distillate'
nm_comm(4) = 'Residual'
nm_comm(5) = 'Kerosene'
nm_comm(6) = 'Coal'
nm_comm(7) = 'Natural Gas'
nm_comm(8) = 'Electricity'

!! End commercial sector carbon emissions !!

!! Begin industrial sector carbon emissions !!

! Industrial sector emissions by pollutant begin here

eminc(1:4, 1, j) = 0.
emincc(11, 1, j) = 0.

! Industrial sector pollution calculations
! Calculate covered emissions first, then uncovered, then total
i = 11  ! GSV TODO: remove this line of code? I don't think that this value of
        ! i is ever actually used for anything...

! Need to split NG into feedstock and combustion
eminc(1, 2, j) = ((qe2ngin(11, j, 1) - inqngpf(11, j)) * egfin(j) +     &
                  inqngpf(11, j) * enqngpf(j) +                         &
                  hcnstech(j, 11, 1) * (1. - pcfcp(j, 1)) * enghm(j) +  &  ! Add in hydrogen sources
                  hcnstech(j, 11, 2) * (1. - pcfcp(j, 2)) * enghm(j) +  &
                  hcnstech(j, 11, 8) * (1. - pgfcp(j, 1)) * enghm(j) +  &
                  hcnstech(j, 11, 10) * (1. - pffcp(j, 2)) * enghm(j) + &
                  qlpin(11, j) * elpin(j) +                             &  ! qlpin from ngtdm--not bifurcated
                  qnglq(11, j) * elpin(j)) / 1000.                      &  ! Treat qnglq like qlpin for starters
                  - ccs_pmm(3, mnumpr, j)  ! Refinery carbon captured and storage, natural gas

eminc(3, 2, j) = (qe2clin(11, j, 1) * eclin(j) +                       &
                  qe2mcin(11, j, 1) * emcin(j) +                       &
                  hcnstech(j, 11, 3) * (1. - pcfcp(j, 3)) * eclhm(j) + &  ! Add in hydrogen sources
                  hcnstech(j, 11, 4) * (1. - pcfcp(j, 4)) * eclhm(j) + &
                  qe2ciin(11, j, 1) * eciin(j)) / 1000.                &
                  - ccs_pmm(2, mnumpr, j)  ! Refinery carbon captured and storage, coal

eminc(2, 2, j) = (qe2mgin(11, j, 1) * emgin(j) +                                                             &
                  qe2dsin(11, j, 1) * edsin(j) +                                                             &
                  qe2ksin(11, j, 1) * eksin(j) +                                                             &
                  max(min((qe2lgin(11, j, 1) - inqlgpf(11, j)) / (qlgin(11, j) - inqlgpf(11, j)), 1.), 0.) * &
                          (qprin(11, j) - qprinpf(11, j)) * eprin(j) +                                       &
                  (qppin(11, j) - qppinpf(11, j)) * eppin(j) +                                               &
                  (qetin(11, j) - qetinpf(11, j)) * eetin(j) +                                               &
                  (qbuin(11, j) - qbuinpf(11, j)) * ebuin(j) +                                               &
                  (qisin(11, j) - qisinpf(11, j)) * eisin(j) +                                               &
                  qppinpf(11, j) * eppinpf(j) +                                                              &
                  qetinpf(11, j) * eetinpf(j) +                                                              &
                  (qprinpf(11, j) + qprolenerf(11, j)) * eprinpf(j) +                                        &
                  qbuinpf(11, j) * ebuinpf(j) +                                                              &
                  qisinpf(11, j) * eisinpf(j) +                                                              &
                  qe2rsin(11, j, 1) * ersin(j) +                                                             &
                  qe2pfin(11, j, 1) * epfin(j) +                                                             &
                  qe2pcin(11, j, 1) * epcin(j) +                                                             &
                  ! Not putting PPIN or LUIN in here as they are in OTIN
                  qe2sgin(11, j, 1) * esgin(j) +                                                             &
                  qe2otin(11, j, 1) * eotin(j)) / 1000.                                                      &
                  - ccs_pmm(4, mnumpr, j)  ! Refinery carbon captured and storage, still gas

emincc(11, 2, j) = sum(eminc(1:3, 2, j))

! Now calculate uncovered (normally 0 unless carbshr() in ind.f is < 1.0)
eminc(1, 3, j) = qe2ngin(11, j, 2) * egfin(j) / 1000.

eminc(3, 3, j) = (qe2clin(11, j, 2) * eclin(j) + &
                  qe2mcin(11, j, 2) * emcin(j) + &
                  qe2ciin(11, j, 2) * eciin(j)) / 1000.

eminc(2, 3, j) = (qe2mgin(11, j, 2) * emgin(j) +                                                                    &
                  qe2dsin(11, j, 2) * edsin(j) +                                                                    &
                  qe2ksin(11, j, 2) * eksin(j) +                                                                    &
                  (1. - max(min((qe2lgin(11, j, 2) - inqlgpf(11, j)) / (qlgin(11, j) - inqlgpf(11, j)), 1.), 0.)) * &
                          (qprin(11, j) - qprinpf(11, j)) * eprin(j) +                                              &
                  qe2rsin(11, j, 2) * ersin(j) +                                                                    &
                  qe2pfin(11, j, 2) * epfin(j) +                                                                    &
                  qe2pcin(11, j, 2) * epcin(j) +                                                                    &
                  ! Not putting PPIN or LUIN in here as they are in LGIN (PP) and OTIN (LU)
                  qe2sgin(11, j, 2) * esgin(j) +                                                                    &
                  qe2otin(11, j, 2) * eotin(j)) / 1000.

emincc(11, 3, j) = sum(eminc(1:3, 3, j))

! Now total
eminc(1, 1, j) = ((qngin(11, j) - inqngpf(11, j)) * egfin(j) +          &
                  inqngpf(11, j) * enqngpf(j) +                         &
                  hcnstech(j, 11, 1) * (1. - pcfcp(j, 1)) * enghm(j) +  &  ! Add in hydrogen sources
                  hcnstech(j, 11, 2) * (1. - pcfcp(j, 2)) * enghm(j) +  &
                  hcnstech(j, 11, 8) * (1. - pgfcp(j, 1)) * enghm(j) +  &
                  hcnstech(j, 11, 10) * (1. - pffcp(j, 2)) * enghm(j) + &
                  qlpin(11, j) * elpin(j) +                             &  ! qlpin from ngtdm--not bifurcated
                  qnglq(11, j) * elpin(j)) / 1000.                      &  ! Treat qnglq like qlpin for starters
                  - ccs_pmm(3, mnumpr, j)  ! Refinery carbon captured and storage, natural gas

eminc(3, 1, j) = (qclin(11, j) * eclin(j) +                            &
                  qmcin(11, j) * emcin(j) +                            &
                  hcnstech(j, 11, 3) * (1. - pcfcp(j, 3)) * eclhm(j) + &  ! Add in hydrogen sources
                  hcnstech(j, 11, 4) * (1. - pcfcp(j, 4)) * eclhm(j) + &
                  qciin(11, j) * eciin(j)) / 1000.                     &
                  - ccs_pmm(2, mnumpr, j)  ! Refinery carbon captured and storage, coal

eminc(2, 1, j) = (qmgin(11, j) * emgin(j) +                           &
                  qdsin(11, j) * edsin(j) +                           &
                  qksin(11, j) * eksin(j) +                           &
                  (qetin(11, j) - qetinpf(11, j)) * eetin(j) +        &
                  (qprin(11, j) - qprinpf(11, j)) * eprin(j) +        &
                  (qbuin(11, j) - qbuinpf(11, j)) * ebuin(j) +        &
                  (qisin(11, j) - qisinpf(11, j)) * eisin(j) +        &
                  (qppin(11, j) - qppinpf(11, j)) * eppin(j) +        &
                  qetinpf(11, j) * eetinpf(j) +                       &
                  (qprinpf(11, j) + qprolenerf(11, j)) * eprinpf(j) + &
                  qbuinpf(11, j) * ebuinpf(j) +                       &
                  qisinpf(11, j) * eisinpf(j) +                       &
                  qppinpf(11, j) * eppinpf(j) +                       &
                  qrsin(11, j) * ersin(j) +                           &
                  qpfin(11, j) * epfin(j) +                           &
                  qpcin(11, j) * epcin(j) +                           &
                  qsgin(11, j) * esgin(j) +                           &
                  ! Lubricants still included in qotin/eotin
                  qotin(11, j) * eotin(j)) / 1000.                    &
                  - ccs_pmm(4, mnumpr, j)  ! Refinery carbon captured and storage, still gas

emincc(11, 1, j) = sum(eminc(1:3, 1, j))

! Industrial sector emissions by pollutant end here

! Industrial sector emissions by fuel (initially populate array)
em_indy(1, 11, jcalyr) = qmgin(11, j) * emgin(j)  ! Motor gasoline
! Hydocarbon gas liquid (HGL) fuel
em_indy(2, 11, jcalyr) = (qetin(11, j) - qetinpf(11, j)) * eetin(j) + &  ! Ethane
                         (qprin(11, j) - qprinpf(11, j)) * eprin(j) + &  ! Propane
                         (qbuin(11, j) - qbuinpf(11, j)) * ebuin(j) + &  ! Butane
                         (qisin(11, j) - qisinpf(11, j)) * eisin(j) + &  ! Isobutane
                         (qppin(11, j) - qppinpf(11, j)) * eppin(j)      ! Pentanes plus
! HGL feedstock
em_indy(3, 11, jcalyr) = qetinpf(11, j) * eetinpf(j) +                       &  ! Ethane
                         (qprinpf(11, j) + qprolenerf(11, j)) * eprinpf(j) + &  ! Propane & refinery propylene
                         qbuinpf(11, j) * ebuinpf(j) +                       &  ! Butane
                         qisinpf(11, j) * eisinpf(j) +                       &  ! Isobutane
                         qppinpf(11, j) * eppinpf(j)                            ! Pentanes plus
em_indy(4, 11, jcalyr) = qdsin(11, j) * edsin(j)  ! Distillate
em_indy(5, 11, jcalyr) = qrsin(11, j) * ersin(j)  ! Residual fuel
em_indy(6, 11, jcalyr) = qksin(11, j) * eksin(j)  ! Kerosene
em_indy(7, 11, jcalyr) = qpfin(11, j) * epfin(j)  ! Petrochemical feedstocks
em_indy(8, 11, jcalyr) = qpcin(11, j) * epcin(j)  ! Petroleum coke
! Still gas
em_indy(9, 11, jcalyr) = qsgin(11, j) * esgin(j) &
                         - ccs_pmm(4, mnumpr, j) * 1000.  ! Refinery carbon captured and storage
em_indy(10, 11, jcalyr) = qotin(11, j) * eotin(j)  ! Other petroleum
! Steam coal
em_indy(11, 11, jcalyr) = qclin(11, j) * eclin(j) +                            &
                          hcnstech(j, 11, 3) * (1. - pcfcp(j, 3)) * eclhm(j) + &  ! Add in hydrogen sources
                          hcnstech(j, 11, 4) * (1. - pcfcp(j, 4)) * eclhm(j)
em_indy(12, 11, jcalyr) = qmcin(11, j) * emcin(j)  ! Metallurgical coal
! Net coal coke imports
em_indy(13, 11, jcalyr) = qciin(11, j) * eciin(j) &
                          - ccs_pmm(2, mnumpr, j) * 1000.  ! Refinery carbon captured and storage
! Natural gas fuel
em_indy(14, 11, jcalyr) = (qngin(11, j) - inqngpf(11, j)) * engin(j) +         &
                          hcnstech(j, 11, 1) * (1. - pcfcp(j, 1)) * enghm(j) + &  ! Add in hydrogen sources
                          hcnstech(j, 11, 2) * (1. - pcfcp(j, 2)) * enghm(j) + &
                          hcnstech(j, 11, 8) * (1. - pgfcp(j, 1)) * enghm(j) + &
                          hcnstech(j, 11, 10) * (1. - pffcp(j, 2)) * enghm(j)  &
                          - ccs_pmm(3, mnumpr, j) * 1000.  ! Refinery carbon captured and storage
em_indy(15, 11, jcalyr) = inqngpf(11, j) * enqngpf(j)  ! Natural gas feedstock
! Natural gas - lease & plant
em_indy(16, 11, jcalyr) = qlpin(11, j) * elpin(j) + qnglq(11, j) * elpin(j)
! Purchased electricity
do i = 1, mnumcr
    if (qelas(i, j) /= 0.) then
        em_indy(17, i, jcalyr) = qelin(i, j) / qelas(i, j)  ! Initially just the sector share
    else
        em_indy(17, i, jcalyr) = 0.
    endif
enddo

! Industrial sector fuel consumption
fl_indy(1, 1:11, jcalyr) = qmgin(1:11, j)  ! Motor gasoline
! Hydocarbon gas liquid (HGL) fuel
fl_indy(2, 1:11, jcalyr) = (qetin(1:11, j) - qetinpf(1:11, j)) + &  ! Ethane
                           (qprin(1:11, j) - qprinpf(1:11, j)) + &  ! Propane
                           (qbuin(1:11, j) - qbuinpf(1:11, j)) + &  ! Butane
                           (qisin(1:11, j) - qisinpf(1:11, j)) + &  ! Isobutane
                           (qppin(1:11, j) - qppinpf(1:11, j))      ! Pentanes plus
! HGL feedstock
fl_indy(3, 1:11, jcalyr) = qetinpf(1:11, j) +                         &  ! Ethane
                           (qprinpf(1:11, j) + qprolenerf(1:11, j)) + &  ! Propane & refinery propylene
                           qbuinpf(1:11, j) +                         &  ! Butane
                           qisinpf(1:11, j) +                         &  ! Isobutane
                           qppinpf(1:11, j)                              ! Pentanes plus
fl_indy(4, 1:11, jcalyr) = qdsin(1:11, j)  ! Distillate
fl_indy(5, 1:11, jcalyr) = qrsin(1:11, j)  ! Residual fuel
fl_indy(6, 1:11, jcalyr) = qksin(1:11, j)  ! Kerosene
fl_indy(7, 1:11, jcalyr) = qpfin(1:11, j)  ! Petrochemical feedstocks
fl_indy(8, 1:11, jcalyr) = qpcin(1:11, j)  ! Petroleum coke
fl_indy(9, 1:11, jcalyr) = qsgin(1:11, j)  ! Still gas
fl_indy(10, 1:11, jcalyr) = qotin(1:11, j)  ! Other petroleum
fl_indy(11, 1:11, jcalyr) = qclin(1:11, j)  ! Steam coal
fl_indy(12, 1:11, jcalyr) = qmcin(1:11, j)  ! Metallurgical coal
fl_indy(13, 1:11, jcalyr) = qciin(1:11, j)  ! Net coal coke imports
fl_indy(14, 1:11, jcalyr) = (qngin(1:11, j) - inqngpf(1:11, j))  ! Natural gas fuel
fl_indy(15, 1:11, jcalyr) = inqngpf(1:11, j)  ! Natural gas feedstock
fl_indy(16, 1:11, jcalyr) = qlpin(1:11, j) + qnglq(1:11, j)  ! Natural gas - lease & plant
fl_indy(17, 1:11, jcalyr) = qelin(1:11, j)  ! Purchased electricity

! Scale SEDS California number based on Pacific if there is no California number
if (qsmgin(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_indy(1, 10, jcalyr) = qsmgin(10, j) * qmgin(9, j) / qsmgin(9, j)
if ((qetin(11, j) + qprin(11, j) + qbuin(11, j) + qisin(11, j) + qppin(11, j)              &
  - (qetinpf(11, j) + qprinpf(11, j) + qbuinpf(11, j) + qisinpf(11, j) + qppinpf(11, j)))  &
  /= 0. .and. curiyr <= msedyr) &
    fl_indy(2, 10, jcalyr) = fl_indy(2, 11, jcalyr) *                                                                   &
                             ((qetin(10, j) + qprin(10, j) + qbuin(10, j) + qisin(10, j) + qppin(10, j)                 &
                             - (qetinpf(10, j) + qprinpf(10, j) + qbuinpf(10, j) + qisinpf(10, j) + qppinpf(10, j)))) / &
                             ((qetin(11, j) + qprin(11, j) + qbuin(11, j) + qisin(11, j) + qppin(11, j)                 &
                             - (qetinpf(11, j) + qprinpf(11, j) + qbuinpf(11, j) + qisinpf(11, j) + qppinpf(11, j))))
if ((qetinpf(11, j) + qprinpf(11, j) + qprolenerf(11, j) + qbuinpf(11, j) + qisinpf(11, j) + qppinpf(11, j)) &
  /= 0. .and. curiyr <= msedyr) &
    fl_indy(3, 10, jcalyr) = fl_indy(3, 11, jcalyr) *                              &
                             (qetinpf(10, j) + qprinpf(10, j) + qprolenerf(10, j)  &
                             + qbuinpf(10, j) + qisinpf(10, j) + qppinpf(10, j)) / &
                             (qetinpf(11, j) + qprinpf(11, j) + qprolenerf(11, j)  &
                             + qbuinpf(11, j) + qisinpf(11, j) + qppinpf(11, j))
if (qsdsin(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_indy(4, 10, jcalyr) = qsdsin(10, j) * qdsin(9, j) / qsdsin(9, j)
if (qsrsin(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_indy(5, 10, jcalyr) = qsrsin(10, j) * qrsin(9, j) / qsrsin(9, j)
if (qsksin(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_indy(6, 10, jcalyr) = qsksin(10, j) * qksin(9, j) / qsksin(9, j)
if (qspfin(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_indy(7, 10, jcalyr) = qspfin(10, j) * qpfin(9, j) / qspfin(9, j)
if (qspcin(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_indy(8, 10, jcalyr) = qspcin(10, j) * qpcin(9, j) / qspcin(9, j)
if (qssgin(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_indy(9, 10, jcalyr) = qssgin(10, j) * qsgin(9, j) / qssgin(9, j)
if (qsotin(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_indy(10, 10, jcalyr) = qsotin(10, j) * qotin(9, j) / qsotin(9, j)
if (qsclin(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_indy(11, 10, jcalyr) = qsclin(10, j) * qclin(9, j) / qsclin(9, j)
if (qsmcin(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_indy(12, 10, jcalyr) = qsmcin(10, j) * qmcin(9, j) / qsmcin(9, j)
if (qsciin(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_indy(13, 10, jcalyr) = qsciin(10, j) * qciin(9, j) / qsciin(9, j)
if ((qngin(11, j) - inqngpf(11, j)) /= 0. .and. curiyr <= msedyr) &
    fl_indy(14, 10, jcalyr) = fl_indy(14, 11, jcalyr) * (qngin(10, j) - inqngpf(10, j)) &
                              / (qngin(11, j) - inqngpf(11, j))
if (inqngpf(11, j) /= 0. .and. curiyr <= msedyr) &
    fl_indy(15, 10, jcalyr) = fl_indy(15, 11, jcalyr) * inqngpf(10, j) / inqngpf(11, j)
if (qslpin(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_indy(16, 10, jcalyr) = qslpin(10, j) * qlpin(9, j) / qslpin(9, j)
if (qselin(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_indy(17, 10, jcalyr) = qselin(10, j) * qelin(9, j) / qselin(9, j)

! Names of the 16 industrial sector fuels (plus electricity)
nm_indy(1) = 'Gasoline'
nm_indy(2) = 'HGL-Fuel'
nm_indy(3) = 'HGL_Feedstock'
nm_indy(4) = 'Distillate'
nm_indy(5) = 'Residual'
nm_indy(6) = 'Kerosene'
nm_indy(7) = 'Petro Feedstock'
nm_indy(8) = 'Petroleum Coke'
nm_indy(9) = 'Still Gas'
nm_indy(10) = 'Other Petroleum'
nm_indy(11) = 'Steam Coal'
nm_indy(12) = 'Met Coal'
nm_indy(13) = 'Net Coke Imports'
nm_indy(14) = 'Natural Gas-Fuel'
nm_indy(15) = 'Natural Gas-Feedstock'
nm_indy(16) = 'Natural Gas-Lease & Plant'
nm_indy(17) = 'Electricity'

!! End industrial sector carbon emissions !!

! Hydrogen sector pollution calculations
! (seemingly not yet implemented)
emhm(1:3, 1, j) = 0.

!! Begin transportation sector carbon emissions !!

! Transportation sector emissions by pollutant begin here

emtr(1:4, 1, j) = 0.
emtrc(11, 1, j) = 0.
emnt(11, 1, j) = 0.

! Ethanol is used for E85 and blending with gasoline.
! For E85 (about 74 percent ethanol, 26 percent gasoline by volume), need to account for
! the carbon in the gasoline component.  The ethanol is assumed to be a zero net carbon source
e85_gasoline_share = (trgne85 * cfrbob(j)) / (trgne85 * cfrbob(j) + ethne85 * cfetq(j))
e85_ethanol_share  = (ethne85 * cfetq(j)) / (trgne85 * cfrbob(j) + ethne85 * cfetq(j))
ethanol_tot = 365. * cfetq(j) &
              * (crnethcd(11, j) + cllethcd(11, j) + othethcd(11, j) &
              + ethimp(11, j) - ethexp(11, j)) / 1000.
ethanol_e85 = qettr(11, j) * e85_ethanol_share
ethanol_blended = ethanol_tot - ethanol_e85

do i = 1, mnumcr - 2
    emtr(1, 1, j) = emtr(1, 1, j) + qgftr(i, j) * egftr(j) / 1000.
    emtr(1, 1, j) = emtr(1, 1, j) + qgitr(i, j) * egitr(j) / 1000.

    ! Pipeline transportation is now kept in EMNT
    ! We keep it by region for completeness
    ! Don't divide by 1000 for consistency with ftab
    emnt(i, 1, j) = qgptr(i, j) * egptr(j)  ! Warning--also calculated in ngafm
    emnt(11, 1, j) = emnt(11, 1, j) + emnt(i, 1, j)
    emtr(2, 1, j) = emtr(2, 1, j) + qmgtr(i, j) * emgtr(j) / 1000.
    emtr(2, 1, j) = emtr(2, 1, j) + qjftr(i, j) * ejftr(j) / 1000.
    emtr(2, 1, j) = emtr(2, 1, j) + qdstr(i, j) * edstr(j) / 1000.
    emtr(2, 1, j) = emtr(2, 1, j) + qprtr(i, j) * eprtr(j) / 1000.
    emtr(2, 1, j) = emtr(2, 1, j) + qrltr(i, j) * erltr(j) / 1000.
    emtr(2, 1, j) = emtr(2, 1, j) + qrhtr(i, j) * erhtr(j) / 1000.
    emtr(2, 1, j) = emtr(2, 1, j) + qottr(i, j) * eottr(j) / 1000.
    emtr(3, 1, j) = emtr(3, 1, j) + qmetr(i, j) * emetr(j) / 1000.

    ! Add gasoline CO2 emissions from E85 
    emtr(2, 1, j) = emtr(2, 1, j) + qettr(i, j) * e85_gasoline_share * emgtr(j) / 1000.
enddo

! Subtract out renewable components used in gasoline blends from the motor
! gasoline carbon computed above.
! Total ethanol from refineries minus e85 = ethanol for splash blending and for
! ETBE.
! Ethanol is only reported in SEDS from 1993 on, so we only subtract it out in
! 1993 on.

! Add other sources of renewable gasoline
allrenewablegasoline = (grn2mgqty(mnumpr, j) * cfnpq +                                       &  ! Non ester renewable gasoline
                       (btlfrac(1, mnumpr, j) + cbtlfrac(2, 1, mnumpr, j)) * cfftliq(1, j) + &  ! Stream 1
                       (btlfrac(2, mnumpr, j) + cbtlfrac(2, 2, mnumpr, j)) * cfftliq(2, j) + &  ! Stream 2
                       ubavolmg(mnumpr, j) * 5.763 +                                         &  ! BPU gasoline (pyrolysys)
                       rfbiobutecd(mnumcr, j) * cfbiobute(j)) *                              &  ! Biobutanol
                       0.365 + &  ! Convert from barrels per day to barrels
                       ethanol_blended

if (j > 3) then
    emtr(2, 1, j) = emtr(2, 1, j) - (allrenewablegasoline * emgtr(j) / 1000.)  ! Remove ethanol
endif

! Subtract out biodiesel used as blending stock. Includes soy, yellow grease, white grease
biodiesel = (sum(bimqtycd(1:4, mnumcr, j)) + biodimp(mnumcr, j) - biodexp(mnumcr, j)) &
            * cfbiod(j) * 365. / 1000.  ! Converting mbcd to tril btu

! Add other zero carbon  diesel  blendstocks
allrenewable_diesel = (grd2dsqty(mnumpr, j) * cfdsq +                                       &  ! Non ester renewable diesel (NERD)
                      (btlfrac(3, mnumpr, j) + cbtlfrac(2, 3, mnumpr, j)) * cfftliq(3, j) + &  ! Stream 1
                      (btlfrac(4, mnumpr, j) + cbtlfrac(2, 4, mnumpr, j)) * cfftliq(4, j) + &  ! Stream 2
                      ubavolds(mnumpr, j) * 5.763) *                                        &  ! BPU diesel (pyrolysys)
                      0.365 + &  ! From barrels per day to barrels
                      biodiesel

emtr(2, 1, j) = emtr(2, 1, j) - (allrenewable_diesel * edstr(j)) / 1000.  ! Adjust (for EMSOL)
emtrc(11, 1, j) = emtr(1, 1, j) + emtr(2, 1, j) + emtr(3, 1, j) + emtr(4, 1, j)  ! Re-sum

! Transportation sector emissions by pollutant end here

! Transportation sector emissions by fuel (initially populate array)
em_tran(1, 11, jcalyr) = qmgtr(11, j) * emgtr(j)  ! Motor gasoline
! E85-gasoline part
em_tran(2, 11, jcalyr) = qettr(11, j) * emgtr(j) * e85_gasoline_share  ! Add mogas used in e85
! Ethanol blending adj.
em_tran(3, 11, jcalyr) = -(allrenewablegasoline * emgtr(j))  ! Sub ethanol blended in qmgtr
em_tran(4, 11, jcalyr) = qprtr(11, j) * eprtr(j)  ! Propane
em_tran(5, 11, jcalyr) = qjftr(11, j) * ejftr(j)  ! Jet fuel
em_tran(6, 11, jcalyr) = qdstr(11, j) * edstr(j)  ! Distillate
em_tran(7, 11, jcalyr) = -allrenewable_diesel * edstr(j)  ! Biomass based diesel blending adj.
em_tran(8, 11, jcalyr) = qrhtr(11, j) * erhtr(j)  ! High sulfur residual fuel
em_tran(9, 11, jcalyr) = qottr(11, j) * eottr(j)  ! Other petroleum
em_tran(10, 11, jcalyr) = qngtr(11, j) * engtr(j) + qgptr(11, j) * egptr(j)  ! Natural gas
em_tran(11, 11, jcalyr) = qmetr(11, j) * emetr(j)  ! Methanol
! Purchased electricity
do i = 1, 11
    if (qelas(i, j) /= 0.) then
        em_tran(12, i, jcalyr) = qeltr(i, j) / qelas(i, j)  ! Initially just the sector share
    else
        em_tran(12, i, jcalyr) = 0.
    endif
enddo

! Transportation sector fuel consumption
fl_tran(1, 1:11, jcalyr) = qmgtr(1:11, j)  ! Motor gasoline
! E85-gasoline part
fl_tran(2, 1:11, jcalyr) = qettr(1:11, j) * e85_gasoline_share  ! Add mogas used in e85
! Ethanol blending adj.
fl_tran(3, 1:11, jcalyr) = -(allrenewablegasoline * (qmgtr(1:11, j) / qmgtr(11, j)))  ! Sub ethanol blended in qmgtr and share to regions
fl_tran(4, 1:11, jcalyr) = qprtr(1:11, j)  ! Propane
fl_tran(5, 1:11, jcalyr) = qjftr(1:11, j)  ! Jet fuel
fl_tran(6, 1:11, jcalyr) = qdstr(1:11, j)  ! Distillate
fl_tran(7, 1:11, jcalyr) = -1. * allrenewable_diesel * (qdstr(1:11, j) / qdstr(11, j))  ! Biomass based diesel blending adj.
fl_tran(8, 1:11, jcalyr) = qrhtr(1:11, j)  ! High sulfur residual fuel
fl_tran(9, 1:11, jcalyr) = qottr(1:11, j)  ! Other petroleum
fl_tran(10, 1:11, jcalyr) = qngtr(1:11, j) + qgptr(1:11, j)  ! Natural gas
fl_tran(11, 1:11, jcalyr) = qmetr(1:11, j)  ! Methanol
fl_tran(12, 1:11, jcalyr) = qeltr(1:11, j)  ! Purchased electricity

! Scale SEDS California number based on Pacific if there is no California number
if (qsmgtr(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_tran(1, 10, jcalyr) = qsmgtr(10, j) * qmgtr(9, j) / qsmgtr(9, j)
if (qmgtr(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_tran(2, 10, jcalyr) = qettr(9, j) * fl_tran(1, 10, jcalyr) / qmgtr(9, j)
! For renewable fuels, use motor gas share in California
if (qmgtr(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_tran(3, 10, jcalyr) = fl_tran(3, 9, jcalyr) * fl_tran(1, 10, jcalyr) / qmgtr(9, j)
if (qslgtr(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_tran(4, 10, jcalyr) = qslgtr(10, j) * qlgtr(9, j) / qslgtr(9, j)
if (qsjftr(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_tran(5, 10, jcalyr) = qsjftr(10, j) * qjftr(9, j) / qsjftr(9, j)
if (qsdstr(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_tran(6, 10, jcalyr) = qsdstr(10, j) * qdstr(9, j) / qsdstr(9, j)
! For renewable fuels, use distillate share in California
if (qdstr(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_tran(7, 10, jcalyr) = fl_tran(7, 9, jcalyr) * fl_tran(6, 10, jcalyr) / qdstr(9, j)
if (qsrhtr(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_tran(8, 10, jcalyr) = qsrhtr(10, j) * qrhtr(9, j) / qsrhtr(9, j)
if (qsottr(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_tran(9, 10, jcalyr) = qsottr(10, j) * qottr(9, j) / qsottr(9, j)
if ((qsngtr(9, j) + qsgptr(9, j)) /= 0. .and. curiyr <= msedyr) &
    fl_tran(10, 10, jcalyr) = (qsngtr(10, j) + qsgptr(10, j)) * (qngtr(9, j) + qgptr(9, j)) &
                              / (qsngtr(9, j) + qsgptr(9, j))
if (qsmetr(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_tran(11, 10, jcalyr) = qsmetr(10, j) * qmetr(9, j) / qsmetr(9, j)
if (qseltr(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_tran(12, 10, jcalyr) = qseltr(10, j) * qeltr(9, j) / qseltr(9, j)

! Names of the 11 transportation sector fuels (plus electricity)
nm_tran(1) = 'Motor Gasoline'
nm_tran(2) = 'E85-Gasoline Part'
nm_tran(3) = 'Ethanol Blending Adj.'
nm_tran(4) = 'Propane'
nm_tran(5) = 'Jet Fuel'
nm_tran(6) = 'Distillate'
nm_tran(7) = 'Biomass based diesel Blending Adj.'
nm_tran(8) = 'Residual'
nm_tran(9) = 'Other Petroleum'
nm_tran(10) = 'Natural Gas'
nm_tran(11) = 'Methanol'
nm_tran(12) = 'Electricity'

!! End transportation sector carbon emissions !!

!! Begin electricity sector carbon emissions !!
! (fuel quantity * carbon emissions factor)

! Electricity sector emissions by pollutant begin here

emel(1:4, 1, j) = 0.
emelc(11, 1, j) = 0.

! Electricity emissions are adjusted for any carbon sequestration--usually
! occurs in carbon-constrained cases.
do i = 1, mnumcr - 2
    emel(2, 1, j) = emel(2, 1, j) + qdsel(i, j) * edsel(j) * (1. - xdsel(i, j)) / 1000.
    emel(2, 1, j) = emel(2, 1, j) + qrlel(i, j) * erlel(j) * (1. - xrlel(i, j)) / 1000.
    emel(2, 1, j) = emel(2, 1, j) + qrhel(i, j) * erhel(j) * (1. - xrhel(i, j)) / 1000.
    emel(2, 1, j) = emel(2, 1, j) + qpcel(i, j) * epcel(j) / 1000.
enddo

do i = 1, ndrgg
    do k = 1, nclut1
        emel(3, 1, j) = emel(3, 1, j) + qclclnr(i, j, k) * cclclnr(i, j, k) * (1. - xclclnr(i, j, k)) / 1000.
    enddo
enddo

if (curitr == 1) write(iunit1, '(//)')
write(iunit1, '(a,i4,a,f10.2)') 'Average electric power coal CO2 emission factor in ', &
                                jcalyr, ': ', 1000. * emel(3, 1, j) / qclel(11, j)

do i = 1, nngem
    emel(1, 1, j) = emel(1, 1, j) + qgfelgr(i, j) * egfelgr(j) * (1. - xqgfelgr(i, j)) / 1000.
    emel(1, 1, j) = emel(1, 1, j) + qgielgr(i, j) * egielgr(j) * (1. - xqgielgr(i, j)) / 1000.
    emel(1, 1, j) = emel(1, 1, j) + qgcelgr(i, j) * egcelgr(j) * (1. - xqgcelgr(i, j)) / 1000.
enddo

! Geothermal
egeel = 3.412 * 2.0535  ! 7.5296 from ghg pub xls file param08.xls, range EF_GEO, times 12/44.
                        ! based on btu production (generation)
geogen_bkwh = ugngenr(1, mnumnr, j) + ugngenr(2, mnumnr, j) + &
              (cgntgen(mnumnr, j, 5, 1) + cgntgen(mnumnr, j, 5, 2)) * 0.001  ! bkWh
geogen_bkwh_hist = ugngenr(1, mnumnr, base_yr) + ugngenr(2, mnumnr, base_yr) + &
                   (cgntgen(mnumnr, base_yr, 5, 1) + cgntgen(mnumnr, base_yr, 5, 2)) * 0.001  ! bkWh

! Other: geothermal and non-biogenic waste energy 
! Set up emission factor for non-biogenic waste energy using GHG history and
! bio waste-energy consumption.
emsel(j) = (emishist(4, 5, base_yr) * 1000. - geogen_bkwh_hist * egeel) / (wncmsel(base_yr, 11) * 1000.)
emel(4, 1, j) = (geogen_bkwh * egeel + 1000. * wncmsel(max(base_yr, j), 11) * emsel(j)) / 1000.
emelc(11, 1, j) = emel(1, 1, j) + emel(2, 1, j) + emel(3, 1, j) + emel(4, 1, j)

! Industrial sector emissions by pollutant end here

! Electricity sector emissions by fuel begin here (initially populate array)
! Compute carbon capture and storage numbers as we go along

ccs(1:5, :) = 0.  ! Carbon captured and stored
em_elec(1:5, 11, jcalyr) = 0.
temp_reg = 0.
do i = 1, 9
    temp_reg(i) = temp_reg(i) + qdsel(i, j) * (1. - xdsel(i, j)) * edsel(j)
    ! Distillate
    em_elec(1, 11, jcalyr) = em_elec(1, 11, jcalyr) + qdsel(i, j) * (1. - xdsel(i, j)) * edsel(j)
    ccs(1, i) = ccs(1, i) + qdsel(i, j) * edsel(j)
    ccs(1, mnumnr) = ccs(1, mnumnr) + qdsel(i, j) * edsel(j)
    ccs(1, i) = ccs(1, i) - temp_reg(i)
enddo
ccs(1, mnumnr) = ccs(1, mnumnr) - em_elec(1, 11, jcalyr)

temp_reg = 0.
do i = 1, 9
    temp_reg(i) = temp_reg(i) + qrlel(i, j) * (1. - xrlel(i, j)) * erlel(j) + &
                  qrhel(i, j) * (1. - xrhel(i, j)) * erhel(j)
    ! Residual fuel
    em_elec(2, 11, jcalyr) = em_elec(2, 11, jcalyr) + qrlel(i, j) * (1. - xrlel(i, j)) * erlel(j)
    em_elec(2, 11, jcalyr) = em_elec(2, 11, jcalyr) + qrhel(i, j) * (1. - xrhel(i, j)) * erhel(j)
    ccs(2, i) = qrlel(i, j) * erlel(j) + qrhel(i, j) * erhel(j)
    ccs(2, mnumnr) = ccs(2, mnumnr) + qrlel(i,j) * erlel(j) + qrhel(i, j) * erhel(j)
    ccs(2, i) = ccs(2, i) - temp_reg(i)
enddo
ccs(2, mnumnr) = ccs(2, mnumnr) - em_elec(2, 11, jcalyr) 

temp_reg = 0.  ! Not used in this block?
do i = 1, 9
    ! Petroleum coke
    em_elec(3, 11, jcalyr) = em_elec(3, 11, jcalyr) + qpcel(i, j) * epcel(j)
enddo

temp_reg = 0.
do i = 1, ndrgg
    do k = 1, nclut1

        ! When qclclnr() is positive and cclclnr() is zero, set cclclnr() to 25.5.
        ! This should be removed when cclclnr() has been properly populated by the coal model.
        if (qclclnr(i, j, k) > 0.00001 .and. cclclnr(i, j, k) < 0.001) then
            write(6, 2317) curirun, curiyr + 1989, curitr, i, j, k, qclclnr(i, j, k), xclclnr(i, j, k), cclclnr(i, j, k)
2317        format(1x,"EPM_COAL_CARBON_OOPS",6(":",i4),3(":",f21.6))
            cclclnr(i, j, k) = 25.5
        endif

        temp_reg(i) = temp_reg(i) + qclclnr(i, j, k) * (1. - xclclnr(i, j, k)) * cclclnr(i, j, k)
        ! Steam coal
        em_elec(4, 11, jcalyr) = em_elec(4, 11, jcalyr) + qclclnr(i, j, k) * (1. - xclclnr(i, j, k)) * cclclnr(i, j, k)
        ccs(3, i) = ccs(3, i) + qclclnr(i, j, k) * cclclnr(i, j, k)
        ccs(3, mnumnr) = ccs(3, mnumnr) + qclclnr(i, j, k) * cclclnr(i, j, k)
    enddo
    ccs(3, i) = ccs(3, i) - temp_reg(i)
enddo

! Since qclclnr is now 0 before 2009, we do this to compensate
! Though we use eclel, which is hard-wired above and not read in from EPMDATA
if (curiyr <= msedyr) then
    ccs(3, mnumnr) = 0.
    do i = 1, mnumcr
        em_elec(4, i, jcalyr) = qclel(i, j) * eclel(j)  ! Steam coal
        ccs(3, mnumnr) = ccs(3, mnumnr) + qclel(i, j) * eclel(j)
    enddo
    ccs(3, mnumnr) = em_elec(4, 11, jcalyr)
endif
ccs(3, mnumnr) = ccs(3, mnumnr) - em_elec(4, 11, jcalyr)

temp_reg = 0.
do i = 1, nngem
    temp_reg(i) = temp_reg(i) +                                        &
                  qgfelgr(i, j) * (1. - xqgfelgr(i, j)) * egfelgr(j) + &
                  qgielgr(i, j) * (1. - xqgielgr(i, j)) * egielgr(j) + &
                  qgcelgr(i, j) * (1. - xqgcelgr(i, j)) * egcelgr(j)
    ! Natural gas
    em_elec(5, 11, jcalyr) = em_elec(5, 11, jcalyr) +                             &
                             qgfelgr(i, j) * (1. - xqgfelgr(i, j)) * egfelgr(j) + &
                             qgielgr(i, j) * (1. - xqgielgr(i, j)) * egielgr(j) + &
                             qgcelgr(i, j) * (1. - xqgcelgr(i, j)) * egcelgr(j) 
    ccs(4, i) = qgfelgr(i, j) * egfelgr(j) + &
                qgielgr(i, j) * egielgr(j) + &
                qgcelgr(i, j) * egcelgr(j) 
    ccs(4, mnumnr) = ccs(4, mnumnr) +             &
                     qgfelgr(i, j) * egfelgr(j) + &
                     qgielgr(i, j) * egielgr(j) + &
                     qgcelgr(i, j) * egcelgr(j) 
    ccs(4, i) = ccs(4, i) - temp_reg(i)
enddo             
ccs(4, mnumnr) = ccs(4, mnumnr) - em_elec(5, 11, jcalyr)

! Eliminate small values due to rounding error
do k = 1, mnumnr
    do i = 1, 4
        if (abs(ccs(i, k)) < 0.05) then
            ccs(i, k) = 0.
        endif
    enddo
enddo

do i = 1, mnumnr
    ccs(5, i) = sum(ccs(1:4, i))  ! Total
enddo
ccs = ccs * 0.001

! emcarbon not previously used, but is convenient for holding this new variable
! for ftab.
emcarbon(1:5, :, j) = ccs(1:5, :)

if (ncrl == 1) then
    write(iunit1, '(1x,i4,a,5f10.2)') jcalyr, '  CCS_PMM: ', ccs_pmm(:, mnumpr, curiyr)
    write(iunit1, '(1x,i4,a,5f10.2)') jcalyr, '      CCS: ', ccs(:, mnumnr)
    write(iunit1, '(1x,i4,a,-3p,5f10.2)') jcalyr, '  em_elec: ', em_elec(1:2, 11, jcalyr), em_elec(4:5, 11, jcalyr)
endif

em_elec(6, 11, jcalyr) = 1000. * wncmsel(max(base_yr, j), 11) * emsel(j)  ! Municipal solid waste (MSW)
em_elec(7, 11, jcalyr) = geogen_bkwh * egeel  ! Geothermal
em_elec(8, 11, jcalyr) = sum(em_elec(1:7, 11, jcalyr))  ! Total electric power

! Electricity sector emissions by fuel end here (initially populate array)

! Electricity sector fuel consumption
fl_elec(1, 1:11, jcalyr) = qdsel(1:11, j)  ! Distillate
fl_elec(2, 1:11, jcalyr) = qrlel(1:11, j) + qrhel(1:11, j)  ! Residual fuel
fl_elec(3, 1:11, jcalyr) = qpcel(1:11, j)  ! Petroleum coke
fl_elec(4, 1:11, jcalyr) = qclel(1:11, j)  ! Steam coal
fl_elec(5, 1:11, jcalyr) = qngel(1:11, j)  ! Natural gas
fl_elec(6, 1:11, jcalyr) = wncmsel(max(base_yr, j), 1:11) * 1000.  ! MSW
fl_elec(7, 1:11, jcalyr) = qgeel(1:11, j)  ! Geothermal

! Scale SEDS California number based on Pacific if there is no California number
if (qsdsel(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_elec(1, 10, jcalyr) = qsdsel(10, j) * qdsel(9, j) / qsdsel(9, j)
if (qsrsel(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_elec(2, 10, jcalyr) = qsdsel(10, j) * qdsel(9, j) / qsdsel(9, j)
if (qspcel(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_elec(3, 10, jcalyr) = qspcel(10, j) * qpcel(9, j) / qspcel(9, j)
if (qsclel(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_elec(4, 10, jcalyr) = qsclel(10, j) * qclel(9, j) / qsclel(9, j)
if (qsngel(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_elec(5, 10, jcalyr) = qsngel(10, j) * qngel(9, j) / qsngel(9, j)
if (qsbmel(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_elec(6, 10, jcalyr) = (wncmsel(j, 9) * 1000.) * qsbmel(10, j) / qsbmel(9, j)
if (qsgeel(9, j) /= 0. .and. curiyr <= msedyr) &
    fl_elec(7, 10, jcalyr) = qsgeel(10, j) * qgeel(9, j) / qsgeel(9, j)

! Total electric power
do i = 1, 11
    fl_elec(8, i, jcalyr) = sum(fl_elec(1:7, i, jcalyr))
enddo

! Names of the 7 electricity sector fuels (plus the total)
nm_elec(1) = 'Distillate'
nm_elec(2) = 'Residual'
nm_elec(3) = 'Petroleum Coke'
nm_elec(4) = 'Steam Coal'
nm_elec(5) = 'Natural Gas'
nm_elec(6) = 'MSW'
nm_elec(7) = 'Geothermal'
nm_elec(8) = '  Total Electric Power'

!! End electricity sector carbon emissions !!

! Calculate regional emissions for each fuel (for each sector)
em_resd(1:iel_r-1, 1:10, jcalyr) = 0.
do i = 1, 11

    ! Electricity sector
    do ifl = 1, etot - 1
        if (fl_elec(ifl, 11, jcalyr) /= 0.) then
            em_elec(ifl, i, jcalyr) = em_elec(ifl, 11, jcalyr) * (fl_elec(ifl, i, jcalyr) / fl_elec(ifl, 11, jcalyr))
        endif
    enddo

    ! Residential sector
    do ifl = 1, iel_r - 1
        if (fl_resd(ifl, 11, jcalyr) /= 0.) then 
            em_resd(ifl, i, jcalyr) = em_resd(ifl, 11, jcalyr) * (fl_resd(ifl, i, jcalyr) / fl_resd(ifl, 11, jcalyr))
        endif
    enddo

    ! Commerical sector
    do ifl = 1, iel_c - 1
        if (fl_comm(ifl, 11, jcalyr) /= 0.) then 
            em_comm(ifl, i, jcalyr) = em_comm(ifl, 11, jcalyr) * (fl_comm(ifl, i, jcalyr) / fl_comm(ifl, 11, jcalyr))
        endif
    enddo

    ! Industrial sector
    do ifl = 1, iel_i - 1
        if (fl_indy(ifl, 11, jcalyr) /= 0.) then 
            em_indy(ifl, i, jcalyr) = em_indy(ifl, 11, jcalyr) * (fl_indy(ifl, i, jcalyr) / fl_indy(ifl, 11, jcalyr))
        endif
    enddo

    ! Transportation sector
    do ifl = 1, iel_t - 1
        if (fl_tran(ifl, 11, jcalyr) /= 0.) then 
            em_tran(ifl, i, jcalyr) = em_tran(ifl, 11, jcalyr) * (fl_tran(ifl, i, jcalyr) / fl_tran(ifl, 11, jcalyr))
        endif
    enddo

enddo

! Calculate California biofuels use and its CO2 emissions so it can adjust the
! state's covered emissions:
! Ethanol is used for E85 and blending with gasoline.
! For E85 (about 74 percent ethanol, 26 percent gasoline by volume), need to
! account for the carbon in the gasoline component. The ethanol is assumed to
! be a zero net carbon source.
! Subtract out renewable components used in gasoline blends from the motor
! gasoline emissions.
! Total ethanol from refineries minus e85 = ethanol for splash blending and for
! ETBE.
ethanol_tot = 365. * cfetq(j) * ettr_shr(curiyr) &
              * (crnethcd(9, j) + cllethcd(9, j) + othethcd(9, j) + ethimp(9, j) - ethexp(9, j)) / 1000.
ethanol_e85 = qettr(9, j) * ettr_shr(curiyr) * e85_ethanol_share
ethanol_blended = ethanol_tot - ethanol_e85

! Add other sources of renewable gasoline.
! Refinery region 7 is California.
allrenewablegasoline = (grn2mgqty(7, j) +                           &  ! Non ester renewable gasoline
                       btlfrac(1, 7, j) + btlfrac(2, 7, j) +        &  ! BTL gasoline
                       ubavolmg(7, j) +                             &  ! BPU gasoline
                       cbtlfrac(2, 1, 7, j) + cbtlfrac(2, 2, 7, j)) &  ! B part of CBTL gasoline
                       * 0.365 * cfnpq &   ! From million barrels per day to trill btu per year. Napthas
                       + ethanol_blended

! Subtract out biodiesel used as blending stock. Includes soy, yellow grease,
! white grease. Assume all renewable diesel in pacific used in California.
biodiesel = (sum(bimqtycd(1:4, 9, j)) + biodimp(9, j) - biodexp(9, j)) &
            * cfbiod(j) * 365. / 1000.  ! Converting mbcd to tril btu

! Add other zero carbon diesel blendstocks
allrenewable_diesel = (grd2dsqty(7, j) +                           &  ! Non ester renewable diesel (NERD)
                      btlfrac(3, 7, j) + btlfrac(4, 7, j) +        &  ! BTL diesel
                      ubavolds(7, j) +                             &  ! BPU diesel
                      cbtlfrac(2, 3, 7, j) + cbtlfrac(2, 4, 7, j)) &  ! B part of CBTL diesel
                      * 0.365 * cfdsq &  ! From million barrels per day to trill btu per year. Diesel
                      + biodiesel

if (curiyr > msedyr) then
    em_tran(1, 10, curcalyr) = qmgtr(10, j) * emgtr(j)
    em_tran(2, 10, curcalyr) = qettr(10, j) * emgtr(j) * e85_gasoline_share  ! Add mogas used in e85 
    em_tran(3, 10, curcalyr) = -(allrenewablegasoline * emgtr(j))  ! Sub ethanol blended in qmgtr
    em_tran(4, 10, curcalyr) = qlgtr(10, j) * eprtr(j)  ! Use qlgtr as no value for California in qprtr now
    ! CA 2014 GHG inventory query tool says intrastate jet fuel CO2 emissions
    ! were 3.554 and the total including int'l and interstate was 42.99. So
    ! 8.25% is intrastate jet fuel, but it's not what LFMM uses, so we'll pass
    ! back what they use and use it.
    em_tran(5, 10, curcalyr) = qjftr(10, j) * ejftr(j) * ab32jetcover(j)
    em_tran(6, 10, curcalyr) = qdstr(10, j) * edstr(j)
    em_tran(7, 10, curcalyr) = -allrenewable_diesel * edstr(j)
    em_tran(8, 10, curcalyr) = qrhtr(10, j) * erhtr(j)
    em_tran(9, 10, curcalyr) = qottr(10, j) * eottr(j)
    em_tran(10, 10, curcalyr) = qngtr(10, j) * engtr(j) + qgptr(10, j) * egptr(j)
    em_tran(11, 10, curcalyr) = qmetr(10, j) * emetr(j)
else  ! Need to adjust these which were based on fl_tran.
    em_tran(2, 10, curcalyr) = em_tran(2, 10, curcalyr) * e85_gasoline_share
    em_tran(3, 10, curcalyr) = -(allrenewablegasoline * emgtr(j))
    em_tran(5, 10, curcalyr) = em_tran(5, 10, curcalyr) * ab32jetcover(j)
    em_tran(7, 10, curcalyr) = -allrenewable_diesel * edstr(j)
    em_tran(8, 10, curcalyr) = qrhtr(10, j) * erhtr(j)
endif

! In history, benchmark national modeled emissions to national history.
if (j <= base_yr) then
    iy = jcalyr
    em_resd(5, 11, iy) = em_resd(5, 11, iy) + (1000. * emishist(1, 1, j) - sum(em_resd(5:5, 11, iy)))  ! ngas
    em_resd(2, 11, iy) = em_resd(2, 11, iy) + (1000. * emishist(2, 1, j) - sum(em_resd(1:3, 11, iy)))  ! oil (dist)
    em_resd(4, 11, iy) = em_resd(4, 11, iy) + (1000. * emishist(3, 1, j) - sum(em_resd(4:4, 11, iy)))  ! coal

    em_comm(7, 11, iy) = em_comm(7, 11, iy) + (1000. * emishist(1, 2, j) - sum(em_comm(7:7, 11, iy)))  ! ngas
    em_comm(3, 11, iy) = em_comm(3, 11, iy) + (1000. * emishist(2, 2, j) - sum(em_comm(1:5, 11, iy)))  ! oil (dist)
    em_comm(6, 11, iy) = em_comm(6, 11, iy) + (1000. * emishist(3, 2, j) - sum(em_comm(6:6, 11, iy)))  ! coal

    em_indy(14, 11, iy) = em_indy(14, 11, iy) + (1000. * emishist(1, 3, j) - sum(em_indy(14:16, 11, iy)))  ! ngas
    em_indy(10, 11, iy) = em_indy(10, 11, iy) + (1000. * emishist(2, 3, j) - sum(em_indy(1:10, 11, iy)))  ! oil (other pet)
    em_indy(11, 11, iy) = em_indy(11, 11, iy) + (1000. * emishist(3, 3, j) - sum(em_indy(11:13, 11, iy)))  ! coal (stm coal)

    em_tran(10, 11, iy) = em_tran(10, 11, iy) + (1000. * emishist(1, 4, j) - sum(em_tran(10:10, 11, iy)))  ! ngas
    em_tran(1, 11, iy) = em_tran(1, 11, iy) + (1000. * emishist(2, 4, j) - sum(em_tran(1:9, 11, iy)))  ! oil (mogas)

    em_elec(5, 11, iy) = em_elec(5, 11, iy) + (1000. * emishist(1, 5, j) - sum(em_elec(5:5, 11, iy)))  ! ngas
    em_elec(2, 11, iy) = em_elec(2, 11, iy) + (1000. * emishist(2, 5, j) - sum(em_elec(1:3, 11, iy)))  ! oil (resid)
    em_elec(4, 11, iy) = em_elec(4, 11, iy) + (1000. * emishist(3, 5, j) - sum(em_elec(4:4, 11, iy)))  ! coal
    em_elec(6, 11, iy) = em_elec(6, 11, iy) + (1000. * emishist(4, 5, j) - sum(em_elec(6:7, 11, iy)))  ! msw/geo

    ! Scale regional values so they add up to the new national number.
    sumc = sum(em_resd(5, 1:9, iy))
    em_resd(5, 1:9, iy) = em_resd(5, 1:9, iy) * (em_resd(5, 11, iy) / sumc)  ! ngas
    sumc = sum(em_resd(2, 1:9, iy))
    em_resd(2, 1:9, iy) = em_resd(2, 1:9, iy) * (em_resd(2, 11, iy) / sumc)  ! oil (dist)
    sumc = sum(em_resd(4, 1:9, iy))
    em_resd(4, 1:9, iy) = 0.
    if (sumc /= 0.) then
        em_resd(4, 1:9, iy) = em_resd(4, 1:9, iy) * (em_resd(4, 11, iy) / sumc)  ! coal
    endif

    sumc = sum(em_comm(7, 1:9, iy))
    em_comm(7, 1:9, iy) = em_comm(7, 1:9, iy) * (em_comm(7, 11, iy) / sumc)  ! ngas
    sumc = sum(em_comm(3, 1:9, iy))
    em_comm(3, 1:9, iy) = em_comm(3, 1:9, iy) * (em_comm(3, 11, iy) / sumc)  ! oil (dist)
    sumc = sum(em_comm(6, 1:9, iy))
    em_comm(6, 1:9, iy) = em_comm(6, 1:9, iy) * (em_comm(6, 11, iy) / sumc)  ! coal

    sumc = sum(em_indy(14, 1:9, iy))
    em_indy(14, 1:9, iy) = em_indy(14, 1:9, iy) * (em_indy(14, 11, iy) / sumc)  ! ngas
    sumc = sum(em_indy(10, 1:9, iy))
    em_indy(10, 1:9, iy) = em_indy(10, 1:9, iy) * (em_indy(10, 11, iy) / sumc)  ! oil (other pet)
    sumc = sum(em_indy(11, 1:9, iy))
    em_indy(11, 1:9, iy) = em_indy(11, 1:9, iy) * (em_indy(11, 11, iy) / sumc)  ! coal (stm coal)

    sumc = sum(em_tran(10, 1:9, iy))
    em_tran(10, 1:9, iy) = em_tran(10, 1:9, iy) * (em_tran(10, 11, iy) / sumc)  ! ngas
    sumc = sum(em_tran(1, 1:9, iy))
    em_tran(1, 1:9, iy) = em_tran(1, 1:9, iy) * (em_tran(1, 11, iy) / sumc)  ! oil (mogas)

    sumc = sum(em_elec(5,1:9,iy))
    if (sumc /= 0.) em_elec(5, 1:9, iy) = em_elec(5, 1:9, iy) * (em_elec(5, 11, iy) / sumc)  ! ngas
    sumc = sum(em_elec(2,1:9,iy))
    if (sumc /= 0.) em_elec(2, 1:9, iy) = em_elec(2, 1:9, iy) * (em_elec(2, 11, iy) / sumc)  ! oil (resid)
    sumc = sum(em_elec(4,1:9,iy))
    if (sumc /= 0.) em_elec(4, 1:9, iy) = em_elec(4, 1:9, iy) * (em_elec(4, 11, iy) / sumc)  ! coal
    sumc = sum(em_elec(6,1:9,iy))
    if (sumc /= 0.) em_elec(6, 1:9, iy) = em_elec(6, 1:9, iy) * (em_elec(6, 11, iy) / sumc)  ! msw/geo
endif

! Share electricity emissions to sectors based on sales shares.
qelas(1:11, j) = qelrs(1:11, j) + qelcm(1:11, j) + qelin(1:11, j) + qeltr(1:11, j)
do i = 1, 11
    em_elec(8, i, jcalyr) = sum(em_elec(1:7, i, jcalyr))  ! Total electric sector emissions
    if (qelas(i, j) /= 0.) then
        em_resd(6, i, jcalyr) = em_elec(8, i, jcalyr) * (qelrs(i, j) / qelas(i, j))
        em_comm(8, i, jcalyr) = em_elec(8, i, jcalyr) * (qelcm(i, j) / qelas(i, j))
        em_indy(17, i, jcalyr) = em_elec(8, i, jcalyr) * (qelin(i, j) / qelas(i, j))
        em_tran(12, i, jcalyr) = em_elec(8, i, jcalyr) * (qeltr(i, j) / qelas(i, j))
    else
        em_resd(6, i, jcalyr) = 0.
        em_comm(8, i, jcalyr) = 0.
        em_indy(17, i, jcalyr) = 0.
        em_tran(12, i, jcalyr) = 0.
    endif
enddo
em_resd(6, 11, jcalyr) = sum(em_resd(6, 1:9, jcalyr))
em_comm(8, 11, jcalyr) = sum(em_comm(8, 1:9, jcalyr))
em_indy(17, 11, jcalyr) = sum(em_indy(17, 1:9, jcalyr))
em_tran(12, 11, jcalyr) = sum(em_tran(12, 1:9, jcalyr))

! Adjust regional distribution of electricity emissions to sectors so that that
! total across regions matches the US total for each sector.
! See example in l:/main/dsa/epmreg/weighting_ex.xls
adj_resd = em_resd(iel_r, 11, jcalyr) - sum(em_resd(iel_r, 1:9, jcalyr))
adj_comm = em_comm(iel_c, 11, jcalyr) - sum(em_comm(iel_c, 1:9, jcalyr))
adj_indy = em_indy(iel_i, 11, jcalyr) - sum(em_indy(iel_i, 1:9, jcalyr))
adj_tran = em_tran(iel_t, 11, jcalyr) - sum(em_tran(iel_t, 1:9, jcalyr))
do i = 1, 9
    reg_shr = qelas(i, j) / qelas(11, j)
    em_resd(iel_r, i, jcalyr) = em_resd(iel_r, i, jcalyr) + (adj_resd * reg_shr)
    em_comm(iel_c, i, jcalyr) = em_comm(iel_c, i, jcalyr) + (adj_comm * reg_shr)
    em_indy(iel_i, i, jcalyr) = em_indy(iel_i, i, jcalyr) + (adj_indy * reg_shr)
    em_tran(iel_t, i, jcalyr) = em_tran(iel_t, i, jcalyr) + (adj_tran * reg_shr)
enddo

! Overwrite computed emissions with history.
if (j <= base_yr) then
    do i_fuel = 1, 4  ! Only emel has a nonzero i_fuel = 4 in history.
        emrs(i_fuel, 1, j) = emishist(i_fuel, 1, j)
        emcm(i_fuel, 1, j) = emishist(i_fuel, 2, j)
        eminc(i_fuel, 1, j) = emishist(i_fuel, 3, j)
        emtr(i_fuel, 1, j) = emishist(i_fuel, 4, j)
        emel(i_fuel, 1, j) = emishist(i_fuel, 5, j)
    enddo

    emtr(1, 1, j) = 0.  ! For history reporting, keep all NG use in the pipeline transportation array.
    emnt(11, 1, j) = emishist(1, 4, j) * 1000.

    emrsc(11, 1, j) = sum(emrs(1:3, 1, j))
    emcmc(11, 1, j) = sum(emcm(1:3, 1, j))
    emincc(11, 1, j) = sum(eminc(1:3, 1, j))  ! TODO ADR. is `emincc` a bug?
    emtrc(11, 1, j) = sum(emtr(1:3, 1, j))
    emelc(11, 1, j) = sum(emel(1:4, 1, j))
endif

! Work-around ngtdm overwrite of EMNT in its reporting loop.
if (j >= 2 .and. j <= base_yr + 1) then
    emnt(11, 1, j-1) = emishist(1, 4, j-1) * 1000.
endif

! Compute total_emissions(j) of CO2 for covered sectors for use in cap and
! trade policy options (via regfalsi routine).
total_emissions(j) = 0.
if (elec_flag) then  ! elec only
    total_emissions(j) = sum(emel(1:4, 1, j))                               ! elec
elseif (tran_flag) then  ! tran only
    total_emissions(j) = sum(emtr(1:2, 1, j)) + emnt(11, 1, j) / 1000.      ! tran
elseif (resd_flag) then  ! exclude resd
    total_emissions(j) = sum(emcm(1:3, 1, j)) +                          &  ! comm
                         sum(emtr(1:2, 1, j)) + emnt(11, 1, j) / 1000. + &  ! tran
                         sum(eminc(1:3, 1, j)) +                         &  ! indus
                         sum(emel(1:4, 1, j))                               ! elec
elseif (comm_flag) then  ! exclude comm
    total_emissions(j) = sum(emrs(1:3, 1, j)) +                          &  ! resd
                         sum(emtr(1:2, 1, j)) + emnt(11, 1, j) / 1000. + &  ! tran
                         sum(eminc(1:3, 1, j)) +                         &  ! indus
                         sum(emel(1:4, 1, j))                               ! elec
else                    ! everything
    total_emissions(j) = sum(emrs(1:3, 1, j)) +                          &  ! resd
                         sum(emcm(1:3, 1, j)) +                          &  ! comm
                         sum(emtr(1:2, 1, j)) + emnt(11, 1, j) / 1000. + &  ! tran
                         sum(eminc(1:3, 1, j)) +                         &  ! indus
                         sum(emel(1:4, 1, j))                               ! elec
endif

! Compute emissions from other GHGes in covered sectors, non covered sectors
! based on marginal abatement and offsets.
call oghg

! Reporting loop output
if (ncrl == 1 .and. j == lastyr) then
    iunit = file_mgr('O', 'DEMAND_CO2        ', .true.)
    write(iunit, 1) trim(scen) // '.' // trim(date)
    write(iunit, '(a)') 'National Totals'
1   format('"',a,'"')
4   format('"',a,'",,',-3p,<lastyr>(',',f10.3))

    call demand_co2(iunit, em_resd, fl_resd, nm_resd, 'Residential', iel_r, 11, mnumyr, 3, 1, 1, 1, 11)
    call demand_co2(iunit, em_comm, fl_comm, nm_comm, 'Commercial', iel_c, 11, mnumyr, 5, 1, 1, 1, 11)
    call demand_co2(iunit, em_indy, fl_indy, nm_indy, 'Industrial', iel_i, 11, mnumyr, 10, 3, 3, 1, 11)
    call demand_co2(iunit, em_tran, fl_tran, nm_tran, 'Transportation', iel_t, 11,mnumyr, 9, 0, 1, 1, 11)
    call demand_co2(iunit, em_elec, fl_elec, nm_elec, 'Electric Power', etot, 11, mnumyr, 3, 1, 1, 0, 11)

    write(iunit, '(//)')
    write(iunit, '(a/)') 'CO2 Emissions Summary'

    i = 11
    write(iunit, 4) 'Petroleum  ', ((sum(em_resd(1:3, i, iy)) +  &
                                   sum(em_comm(1:5, i, iy)) +  &
                                   sum(em_indy(1:10, i, iy)) + &
                                   sum(em_tran(1:9, i, iy)) +  &
                                   sum(em_elec(1:3, i, iy))) * 44./12., iy = 2004, min(ijumpcalyr, lastcalyr))
    write(iunit, 4) 'Coal       ', ((em_resd(4, i, iy) +        &
                                   em_comm(6, i, iy) +          &
                                   sum(em_indy(11:13, i, iy)) + &
                                   em_elec(4, i, iy)) * 44./12., iy = 2004, min(ijumpcalyr, lastcalyr))
    write(iunit, 4) 'Natural gas', ((em_resd(5, i, iy) +        &
                                   em_comm(7, i, iy) +          &
                                   sum(em_indy(14:16, i, iy)) + &
                                   em_tran(10, i, iy) +         &
                                   em_elec(5, i, iy)) * 44./12., iy = 2004, min(ijumpcalyr, lastcalyr))
    write(iunit, 4) 'Other      ', ((sum(em_elec(6:7, i, iy))) * 44./12., iy = 2004, min(ijumpcalyr, lastcalyr))
    write(iunit, 4) '  Total    ', ((sum(em_resd(1:5, i, iy)) + &
                                   sum(em_comm(1:7, i, iy)) +   &
                                   sum(em_indy(1:16, i, iy)) +  &
                                   sum(em_tran(1:11, i, iy)) +  &
                                   sum(em_elec(1:7, i, iy))) * 44./12., iy = 2004, min(ijumpcalyr, lastcalyr))
    write(iunit, 4) 'Electricity', ((em_resd(6, i, iy) + &
                                   em_comm(8, i, iy) +   &
                                   em_indy(17, i, iy) +  &
                                   em_tran(12, i, iy)) * 44./12., iy = 2004, min(ijumpcalyr, lastcalyr))

    do i = 1, 10
        write(iunit, '(/////a,i2,", ",a)') 'Region ', i, trim(regnam(i))

        call demand_co2(iunit, em_resd, fl_resd, nm_resd, 'Residential', iel_r, 11, mnumyr, 3, 1, 1, 1, i)
        call demand_co2(iunit, em_comm, fl_comm, nm_comm, 'Commercial', iel_c, 11, mnumyr, 5, 1, 1, 1, i)
        call demand_co2(iunit, em_indy, fl_indy, nm_indy, 'Industrial', iel_i, 11, mnumyr, 10, 3, 3, 1, i)
        call demand_co2(iunit, em_tran, fl_tran, nm_tran, 'Transportation', iel_t, 11, mnumyr, 9, 0, 1, 1, i)
        call demand_co2(iunit, em_elec, fl_elec, nm_elec, 'Electric Power', etot, 11, mnumyr, 3, 1, 1, 0, i)

        write(iunit, '(//)')
        write(iunit, '(a/)') 'CO2 Emissions Summary'
        write(iunit, 4) 'Petroleum  ', ((sum(em_resd(1:3, i, iy)) + &
                                       sum(em_comm(1:5, i, iy)) +   &
                                       sum(em_indy(1:10, i, iy)) +  &
                                       sum(em_tran(1:9, i, iy)) +   &
                                       sum(em_elec(1:3, i, iy))) * 44./12., iy = 2004, min(ijumpcalyr, lastcalyr))
        write(iunit, 4) 'Coal       ', ((em_resd(4, i, iy) +        &
                                       em_comm(6, i, iy) +          &
                                       sum(em_indy(11:13, i, iy)) + &
                                       em_elec(4, i, iy)) * 44./12., iy = 2004, min(ijumpcalyr, lastcalyr))
        write(iunit, 4) 'Natural gas', ((em_resd(5, i, iy) +        &
                                       em_comm(7, i, iy) +          &
                                       sum(em_indy(14:16, i, iy)) + &
                                       em_tran(10, i, iy) +         &
                                       em_elec(5, i, iy)) * 44./12., iy = 2004, min(ijumpcalyr, lastcalyr))
        write(iunit, 4) 'Other      ', ((sum(em_elec(6:7, i, iy))) * 44./12., iy = 2004, min(ijumpcalyr, lastcalyr))
        write(iunit, 4) '  Total    ', ((sum(em_resd(1:5, i, iy)) + &
                                       sum(em_comm(1:7, i, iy)) +   &
                                       sum(em_indy(1:16, i, iy)) +  &
                                       sum(em_tran(1:11, i, iy)) +  &
                                       sum(em_elec(1:7, i, iy))) * 44./12., iy = 2004, min(ijumpcalyr, lastcalyr))
        write(iunit, 4) 'Electricity', ((em_resd(6, i, iy) + &
                                       em_comm(8, i, iy) +   &
                                       em_indy(17, i, iy) +  &
                                       em_tran(12, i, iy)) * 44./12., iy = 2004, min(ijumpcalyr, lastcalyr))
    enddo

    close(iunit)
endif

return
end subroutine sum_emissions


!> Output out CO2 emissions and energy use by sector.

!> Write out a sector's worth of CO2 emissions and the corresponding energy use
!> from which it was computed.
!> 
!> @param iunit Unit number of output file.
!> @param emis Emissions array with dimensions (n, nreg, nyr).
!> @param fuel Fuels array with dimensions (n, nreg, nyr).
!> @param name Array of fuel names with length n (?).
!> @param sector Name of sector for output.
!> @param n Total number of fuels (?).
!> @param nreg Number of regions.
!> @param nyr Number of years.
!> @param noil Number of oil entries (?).
!> @param ncoal Number of coal entries (?).
!> @param ngas Number of gas entries (?).
!> @param nelec Number of electric power entries (?).
!> @param ireg Region index in arrays.

!> @see sum_emissions

subroutine demand_co2(iunit, emis, fuel, name, sector, n, nreg, nyr, noil, &
                      ncoal, ngas, nelec, ireg)

implicit none

include 'parametr'
include 'ncntrl'

integer iunit, n, nreg, nyr, noil, ncoal, ngas, nelec, ireg
real emis(n, nreg, nyr), fuel(n, nreg, nyr)
character*25 name(n)
character*(*) sector
intent(in) :: iunit, n, nreg, nyr, noil, ncoal, ngas, nelec, ireg
intent(in) :: emis, fuel
intent(in) :: name, sector

integer i, y, icoal_s, icoal_e, igas_s, igas_e
real fac, co2, btu

write(iunit, 2) trim(sector), (i, i = 2004, min(ijumpcalyr, lastcalyr))
2 format(////'"',a,' Sector","Emis Fac","Emis Fac"',<lastyr>(',',i5))
write(iunit, 3) 'CO2 Emissions (mmtCO2eq)'
3 format(/'"',a,'"')

icoal_s = noil + 1             ! first coal is after last oil
icoal_e = icoal_s + ncoal - 1  ! end coal
igas_s = noil + ncoal + 1      ! first gas is after last coal
igas_e = igas_s + ngas - 1     ! end gas

do i = 1, n
    fac = 0.
    co2 = emis(i, ireg, lastyr)
    btu = fuel(i, ireg, lastyr)
    if (btu /= 0.) fac = co2 / btu
    write(iunit, 1) trim(name(i)), fac, fac * 44./12., &
        (emis(i, ireg, y) * .001 * 44./12., y = 15, lastyr)
    if (i == noil) then
        co2 = sum(emis(1:noil, ireg, lastyr))
        btu = sum(fuel(1:noil, ireg, lastyr))
        if (btu /= 0.) fac = co2 / btu
        write(iunit, 1) '  Oil Subtotal', fac, fac * 44./12., &
            (sum(emis(1:noil, ireg, y)) * .001 * 44./12., y = 15, lastyr)
    elseif (i == icoal_e .and. ncoal > 1) then
        co2 = sum(emis(icoal_s:icoal_e, ireg, lastyr))
        btu = sum(fuel(icoal_s:icoal_e, ireg, lastyr))
        if (btu /= 0.) fac = co2 / btu
        write(iunit, 1) '  Coal Subtotal', fac, fac * 44./12., &
            (sum(emis(icoal_s:icoal_e, ireg, y)) * .001 * 44./12., y = 15, lastyr)
    elseif (i == igas_e .and. ngas > 1) then
        co2 = sum(emis(igas_s:igas_e, ireg, lastyr))
        btu = sum(fuel(igas_s:igas_e, ireg, lastyr))
        if (btu /= 0.) fac = co2 / btu
        write(iunit, 1) '  Natural Gas Subtotal', fac, fac * 44./12., &
            (sum(emis(igas_s:igas_e, ireg, y)) * .001 * 44./12., y = 15, lastyr)
    endif
1   format('"',a,'",',f10.4,',',f10.4,<lastyr>(',',f10.2))
enddo

if (nelec > 0) then
    write(iunit, 4) '  Total w/o  Elec', &
        (sum(emis(1:n-1, ireg, y)) * .001 * 44./12., y = 15, lastyr)
    write(iunit, 4) '  Total ' // trim(sector), &
        (sum(emis(1:n, ireg, y)) * .001 * 44./12., y = 15, lastyr)
endif

write(iunit, '()')
write(iunit, '()')
write(iunit, 3) 'Energy Use (trill Btu)'

do i = 1, n
    write(iunit, 4) trim(name(i)), &
        (fuel(i, ireg, y), y = 15, lastyr)
    if (i == noil) then
        write(iunit, 4) '  Oil Subtotal', &
            (sum(fuel(1:noil, ireg, y)), y = 15, lastyr)
    elseif (i == icoal_e .and. ncoal > 1) then
        write(iunit, 4) '  Coal Subtotal', &
            (sum(fuel(icoal_s:icoal_e, ireg, y)), y = 15, lastyr)
    elseif (i == igas_e .and. ngas > 1) then
        write(iunit, 4) '  Natural Gas Subtotal', &
            (sum(fuel(igas_s:igas_e, ireg, y)), y = 15, lastyr)
    endif
4   format('"',a,'",,',<lastyr>(',',f10.1))
enddo

if (nelec > 0) &
    write(iunit, 4) '  Total ' // trim(sector), &
        (sum(fuel(1:n, ireg, y)), y = 15, lastyr)

return
end subroutine demand_co2


!> Calculates the revenue effects of the emissions policy.

!> For a tax, it amounts to the revenue returned to the government. For an
!> auction, it amounts to the same thing. For a market, we need to subtract out
!> the value of the initial allocation of permits, which is done in initrev.
!> Sum revenue by sector:
!> 1. Residential
!> 2. Commercial
!> 3. Industrial
!> 4. Transportation
!> 5. Utility

!> @see epm

subroutine accntrev

implicit none

include 'parametr'
include 'emmparm'
include 'cdsparms'
include 'ncntrl'
include 'qblk'
include 'indout'
include 'uefpout'  ! UEFP output variables
include 'uefdout'  ! EFD output variables
include 'udatout'  ! UDAT output variables
include 'uecpout'  ! UECP output variables
include 'uettout'  ! UETT output variables
include 'emeblk'
include 'emoblk'
include 'emission'
include 'macout'
include 'epmcntl'
include 'pmmrpt'
include 'convfact'
include 'coalemm'
include 'control'
include 'epmbank'
include 'ghgrep'
include 'wrenew'  ! For wncmsel
include 'cogen'
include 'pmmout'
include 'pmmftab'
include 'lfmmout'

integer i, j, k, m, ij  ! Index variables
integer iunit1, bank_onyr
integer rtovalue
external rtovalue
common /epm_out/ iunit1, bank_onyr
real*4 etax, etaxendu, etaxelec, etaxtran, etaxcomm, etaxresd  ! Emissions tax in 87$'s
real*4 rev, rev_before, adjust
real bank_local(mnumyr), balance_local(mnumyr)
real*4 r
real safety_sales, pot_auction_pool, ccs_bonus_all
integer bioseqok  ! Run time option. with offsets, bioseqok = 1, bio sequestration offsets count towards goal. 0==> incentives given but don't count toward goal
real qoff
real allow_per_offset /1.0/  ! Set to 1 allowance credit per offset
real biodiesel
real egeel  ! Temporary for geothermal emissions factor
real geogen_bkwh, geogen_bkwh_hist
real e85_gasoline_share, e85_ethanol_share
real ethanol_tot, ethanol_e85, ethanol_blended, allrenewablegasoline, allrenewable_diesel
integer base_yr  ! Last year of history-overwrites (also, see hist in epm_read)
parameter(base_yr = 32)  ! 2021 is last data year when reading epmdata history 
real emishist(4, 5, base_yr)  ! Historical carb by 4 fuels, 5 sect, year
common /emishistory/ emishist
real epm_addoff
external epm_addoff

real allow_alloc(1, mnumyr) / 22 * 0.0, &
    00.000, &  ! 2012
    00.000, &  ! 2013
    00.000, &  ! 2014
    00.000, &  ! 2015
    00.000, &  ! 2016
    00.000, &  ! 2017
    00.000, &  ! 2018
    00.000, &  ! 2019
    00.000, &  ! 2020
    00.000, &  ! 2021
    00.000, &  ! 2022
    00.000, &  ! 2023
    00.000, &  ! 2024
    00.000, &  ! 2025
    00.000, &  ! 2026
    00.000, &  ! 2027
    00.000, &  ! 2028
    00.000, &  ! 2029
    00.000, &  ! 2030  ! did not have post-2031 numbers so copied 2030 to all subsequent yrs
    00.000, &  ! 2031
    00.000, &  ! 2032
    00.000, &  ! 2033
    00.000, &  ! 2034
    00.000, &  ! 2035
    00.000, &  ! 2036
    00.000, &  ! 2037
    00.000, &  ! 2038
    00.000, &  ! 2039
    00.000, &  ! 2040
    00.000, &  ! 2041
    00.000, &  ! 2042
    00.000, &  ! 2043
    00.000, &  ! 2044
    00.000, &  ! 2045
    00.000, &  ! 2046
    00.000, &  ! 2047
    00.000, &  ! 2048
    00.000, &  ! 2049
    00.000 /   ! 2050

r = 7.4  ! Real escalation rate in % for fee case with banking
r = real(rtovalue('BANKDSCR', 74)) / 10.0  ! now entered in as permille. change to percent.

j = curiyr
i = mnumcr  ! last  census region

! set etax equal to the emissions tax converted to nominal $'s
if (.not. nominal_flag) then
    etax = emtax(j) * mc_jpgdp(j)  ! convert to nominal $'s
else
    etax = emtax(j)  ! already in nominal $'s
endif

etaxelec = etax
etaxtran = etax
etaxendu = etax
etaxresd = etax
etaxcomm = etax

if (elec_flag) then
    etaxendu = 0.0  ! if elec-only, zero end-use tax copy
    etaxtran = 0.
endif
if (tran_flag) then
    etaxendu = 0.0  ! if tran-only, zero end-use tax copy
    etaxelec = 0.
endif
if (resd_flag .or. elec_flag .or. tran_flag) etaxresd = 0.0  ! if residential excluded or elec-only, zero resd tax copy
if (comm_flag .or. elec_flag .or. tran_flag) etaxcomm = 0.0  ! if commercial excluded or elec-only, zero comm tax copy

emrev(:, j) = 0.0

if (bank_flag .and. curcalyr < bank_startyr) return
if ((market_flag .or. permit_flag) .and. etax == 0.) return

! These emrev(1:5,) calculations were designed for a carbon or energy tax case.
! They do not apply to a cap and trade policy.
! emrev(1:5,) values are no longer reported in the default version of ftab table 118.
! Instead, auction revenues, allowance distribution value, and offset value
! associated with a cap and trade policy are calculated further below (emrev(6:12,).
! These results are used by the macro module and reported in ftabl table 118.
emrev(1, j) = emrev(1, j) + qgfrs(i, j) * egfrs(j) * etaxresd
emrev(1, j) = emrev(1, j) + qgirs(i, j) * egirs(j) * etaxresd
emrev(1, j) = emrev(1, j) + qclrs(i, j) * eclrs(j) * etaxresd
emrev(1, j) = emrev(1, j) + qdsrs(i, j) * edsrs(j) * etaxresd
emrev(1, j) = emrev(1, j) + qksrs(i, j) * eksrs(j) * etaxresd
emrev(1, j) = emrev(1, j) + qprrs(i, j) * eprrs(j) * etaxresd

if (dbugepm > 4) then
    write(iunit1, *) 'REV', emrev(1, j), 'QGFRS', qgfrs(i, j), &
        'ETAX', etaxresd, 'EGFRS(J)', egfrs(j)
endif  ! (dbugepm)

emrev(2, j) = emrev(2, j) + qgfcm(i, j) * egfcm(j) * etaxcomm
emrev(2, j) = emrev(2, j) + qgicm(i, j) * egicm(j) * etaxcomm
emrev(2, j) = emrev(2, j) + qclcm(i, j) * eclcm(j) * etaxcomm
emrev(2, j) = emrev(2, j) + qmgcm(i, j) * emgcm(j) * etaxcomm
emrev(2, j) = emrev(2, j) + qdscm(i, j) * edscm(j) * etaxcomm
emrev(2, j) = emrev(2, j) + qkscm(i, j) * ekscm(j) * etaxcomm
emrev(2, j) = emrev(2, j) + qprcm(i, j) * eprcm(j) * etaxcomm
emrev(2, j) = emrev(2, j) + qrlcm(i, j) * erlcm(j) * etaxcomm

emrev(3, j) = emrev(3, j) + (qngin(i, j) - inqngpf(i, j)) * engin(j) * etaxendu
emrev(3, j) = emrev(3, j) + inqngpf(i, j) * enqngpf(j) * etaxendu
emrev(3, j) = emrev(3, j) + qlpin(i, j) * elpin(j) * etaxendu
emrev(3, j) = emrev(3, j) + qnglq(i, j) * elpin(j) * etaxendu
emrev(3, j) = emrev(3, j) + qclin(i, j) * eclin(j) * etaxendu
emrev(3, j) = emrev(3, j) + qmcin(i, j) * emcin(j) * etaxendu
emrev(3, j) = emrev(3, j) + qciin(i, j) * eciin(j) * etaxendu
emrev(3, j) = emrev(3, j) + qmgin(i, j) * emgin(j) * etaxendu
emrev(3, j) = emrev(3, j) + qdsin(i, j) * edsin(j) * etaxendu
emrev(3, j) = emrev(3, j) + qksin(i, j) * eksin(j) * etaxendu
emrev(3, j) = emrev(3, j) + (qetin(i, j)-qetinpf(i, j)) * eetin(j) * etaxendu
emrev(3, j) = emrev(3, j) + (qprin(i, j)-qprinpf(i, j)) * eprin(j) * etaxendu
emrev(3, j) = emrev(3, j) + (qbuin(i, j)-qbuinpf(i, j)) * ebuin(j) * etaxendu
emrev(3, j) = emrev(3, j) + (qisin(i, j)-qisinpf(i, j)) * eisin(j) * etaxendu
emrev(3, j) = emrev(3, j) + (qppin(i, j) - qppinpf(i, j)) * eppin(j) * etaxendu
emrev(3, j) = emrev(3, j) + qetinpf(i, j) * eetinpf(j) * etaxendu
emrev(3, j) = emrev(3, j) + (qprinpf(i, j) + qprolenerf(i, j)) * eprinpf(j) * etaxendu
emrev(3, j) = emrev(3, j) + qbuinpf(i, j) * ebuinpf(j) * etaxendu
emrev(3, j) = emrev(3, j) + qisinpf(i, j) * eisinpf(j) * etaxendu
emrev(3, j) = emrev(3, j) + qppinpf(i, j) * eppinpf(j) * etaxendu
emrev(3, j) = emrev(3, j) + qrlin(i, j) * erlin(j) * etaxendu
emrev(3, j) = emrev(3, j) + qpfin(i, j) * epfin(j) * etaxendu
emrev(3, j) = emrev(3, j) + qpcin(i, j) * epcin(j) * etaxendu
emrev(3, j) = emrev(3, j) + qsgin(i, j) * esgin(j) * etaxendu

! Lubricants still contained in qotin/eotin
emrev(3, j) = emrev(3, j) + qotin(i, j) * eotin(j) * etaxendu

emrev(4, j) = emrev(4, j) + qgftr(i, j) * egftr(j) * etaxtran
emrev(4, j) = emrev(4, j) + qgitr(i, j) * egitr(j) * etaxtran
emrev(4, j) = emrev(4, j) + qgptr(i, j) * egptr(j) * etaxtran
emrev(4, j) = emrev(4, j) + qmgtr(i, j) * emgtr(j) * etaxtran
emrev(4, j) = emrev(4, j) + qdstr(i, j) * edstr(j) * etaxtran

! Take out revenue associated with ethanol and biodiesel emissions, assumed exempt
! Add revenue associated with gasoline portion of E85
e85_gasoline_share = (trgne85 * cfrbob(j)) / (trgne85 * cfrbob(j) + ethne85 * cfetq(j))
e85_ethanol_share  = (ethne85 * cfetq(j)) / (trgne85 * cfrbob(j) + ethne85 * cfetq(j))
ethanol_tot = 365. * cfetq(j) &
    * (crnethcd(11, j) + cllethcd(11, j) + othethcd(11, j) + ethimp(11, j) - ethexp(11, j)) / 1000.
ethanol_e85 =  qettr(11, j) * e85_ethanol_share
ethanol_blended = ethanol_tot - ethanol_e85

! Add other sources of renewable gasoline
allrenewablegasoline = 365. * grn2mgqty(mnumpr, j) / 1000 * cfnpq +                                    &  ! NER gasoline
                       365. * (btlfrac(1, mnumpr, j) + btlfrac(2, mnumpr, j)) / 1000 * cfnpq +         &  ! BTL gasoline
                       365. * ubavolmg(mnumpr, j) / 1000 * cfnpq +                                     &  ! BPU gasoline
                       365. * (cbtlfrac(2, 1, mnumpr, j) + cbtlfrac(2, 2, mnumpr, j)) / 1000 * cfnpq + &  ! B part of CBTL gasoline
                       ethanol_blended

! Subtract out biodiesel used as blending stock. Includes soy, yellow grease, white grease
biodiesel = (sum(bimqtycd(1:4, mnumcr, curiyr)) + biodimp(mnumcr, curiyr) - biodexp(mnumcr, curiyr)) &
    * cfbiod(j) * 365./1000.  ! converting mbcd to tril btu

! Add other zero carbon diesel blendstocks
allrenewable_diesel = 365. * grd2dsqty(mnumpr, j) / 1000 * cfdsq +                                    &  ! NERD
                      365. * (btlfrac(3, mnumpr, j) + btlfrac(4, mnumpr, j)) / 1000 * cfdsq +         &  ! BTL Diesel
                      365. * ubavolds(mnumpr, j) / 1000 * cfdsq +                                     &  ! BPU Diesel 
                      365. * (cbtlfrac(2, 3, mnumpr, j) + cbtlfrac(2, 4, mnumpr, j)) / 1000 * cfdsq + &  ! B part of CBTL Diesel
                      biodiesel

emrev(4, j) = emrev(4, j) - allrenewablegasoline * emgtr(j) * etaxtran &
              - allrenewable_diesel * edstr(j) * etaxtran
emrev(4, j) = emrev(4, j) + qettr(i, j) * e85_gasoline_share * emgtr(j) * etaxtran

emrev(4, j) = emrev(4, j) + qjftr(i, j) * ejftr(j) * etaxtran
emrev(4, j) = emrev(4, j) + qprtr(i, j) * eprtr(j) * etaxtran
emrev(4, j) = emrev(4, j) + qrltr(i, j) * erltr(j) * etaxtran
emrev(4, j) = emrev(4, j) + qrhtr(i, j) * erhtr(j) * etaxtran
emrev(4, j) = emrev(4, j) + qottr(i, j) * eottr(j) * etaxtran
emrev(4, j) = emrev(4, j) + qmetr(i, j) * emetr(j) * etaxtran

do i = 1, mnumcr - 2
    emrev(5, j) = emrev(5, j) + qdsel(i, j) * edsel(j) * (1.0 - xdsel(i, j)) * etaxelec
    emrev(5, j) = emrev(5, j) + qrlel(i, j) * erlel(j) * (1.0 - xrlel(i, j)) * etaxelec
    emrev(5, j) = emrev(5, j) + qrhel(i, j) * erhel(j) * (1.0 - xrhel(i, j)) * etaxelec
enddo

! Sum over coal/utility regions
do i = 1, ndrgg
    do k = 1, nclut1
        emrev(5, j) = emrev(5, j) + qclclnr(i, j, k) * cclclnr(i, j, k) &
                      * (1.0 - xclclnr(i, j, k)) * (1.0 - urcclsub) * etaxelec
    enddo
enddo

! Geothermal
egeel = 3.412 * 2.0536  ! 2.0536 from GHG pub xls file param04.xls, range EF_GEO.
                        ! based on btu production (generation)
geogen_bkwh = ugngenr(1, mnumnr, curiyr) + ugngenr(2, mnumnr, curiyr) + &
              (cgntgen(mnumnr,curiyr, 5, 1) + cgntgen(mnumnr, curiyr, 5, 2)) * 0.001  ! bkwh
geogen_bkwh_hist = ugngenr(1, mnumnr, base_yr) + ugngenr(2, mnumnr, base_yr) + &
                   (cgntgen(mnumnr, base_yr, 5, 1) + cgntgen(mnumnr, base_yr, 5, 2)) * 0.001  ! bkwh

! Other: geothermal and non-biogenic waste energy
! Set up emission factor for non-biogenic waste energy using GHG history and
! bio waste-energy consumption
emsel(j) = (emishist(4, 5, base_yr) * 1000. - geogen_bkwh_hist * egeel) &
           / (wncmsel(base_yr, 11) * 1000.)
emrev(5, j) = emrev(5, j) &
            + (geogen_bkwh * egeel + 1000 * wncmsel(max(base_yr, j), 11) * emsel(j)) * etaxelec

! Sum over gas/utility regions
do k = 1, nngem
    emrev(5, j) = emrev(5, j) + qgfelgr(k, j) * egfelgr(j) * (1.0 - xqgfelgr(k, j)) * etaxelec
    emrev(5, j) = emrev(5, j) + qgielgr(k, j) * egielgr(j) * (1.0 - xqgielgr(k, j)) * etaxelec
    emrev(5, j) = emrev(5, j) + qgcelgr(k, j) * egcelgr(j) * (1.0 - xqgcelgr(k, j)) * etaxelec
enddo

! We total revenue in billions of dollars, requiring division by 1000
do k = 1, 5
    emrev(k, j) = emrev(k, j) / 1000.0
enddo

if (bank_flag .or. permit_flag .or. market_flag) then
    bioseqok = rtovalue('BIOSEQOK', 1)  ! if 1, bio sequestration counts as an offsets. 0:incentive only
    qoff = epm_addoff(offset, 5, mnumyr, baseyr, j, bioseqok, allow_per_offset, 2012)

    ! Recalc potential revenue.
    ! The potential revenue collected at auction (at 100% auction) would be the number of allowances
    ! times the allowance price. This assumes no safety valve and no increase in the number allowances issued
    ! in exchange for offsets. Safety-valve sales complicate matters, as would banking with a safety sales program.
    ! Usually this section has to be customized for programs with safety-valves
    bank_local(j) = emissions_goal(j) + qoff-emsol(1, j)
    if (j == 1) then
        balance_local(j) = bank_local(j)
    else
        balance_local(j) = balance(j - 1) + bank_local(j)
    endif

    rev_before = sum(emrev(1:5, j)) + ghg_rev(1, j) * mc_jpgdp(j)

    emrev(6:8, j) = 0.  ! emrev(9, j) filled in electricity module so don't initialize it here
    emrev(10:12, j) = 0.

    ! For banking cases with allowance price maximum (safety valve) acccount for the use
    ! of previously banked allowances to reduce the need for safety value allowance purchases.
    emrev(6, j) = emlim(1, j) * etax  ! 100% auction revenue = allowances issued times price
    safety_sales = 0.
    if (emtax(j) >= (max_tax(j) - .00001)) then
        safety_sales = max(0., emissions_goal(j) + qoff - emsol(1, j))
    endif
    emrev(7, j) = safety_sales * etax  ! safety valve revenue

    if (bank_flag) then
        safety_sales = 0.

        ! If in withdrawal period, you may cover your excess emissions with bank withdrawals
        ! until you are tapped out. You are tapped out when the balance goes below zero. So,
        ! if the emissions greater than the # allowances (bank_local(j) < 0) and
        ! you are tapped out (balance(j) < 0), then you can buy safety_allowances
        if (bank_local(j) < 0 .and. balance_local(j) <= 0.) then

            ! So safety sales have to cover the remainder of the shortfall (balance_local), or the entire shortfall
            ! assuming you were short last year (bank_local). 
            safety_sales = 0. - (max(balance_local(j), bank_local(j)))
        endif
        emrev(7, j) = safety_sales * etax  ! safety valve revenue
    endif

    rev = emrev(6, j) + emrev(7, j)  ! total potential revenue, including safety sales

    ! Under s2191, the auction pool for 2012-2014 includes some allowances reserved for early auctioning.
    ! Calculation the potential pool assuming the allowances are sold in the issue year.
    pot_auction_pool = emlim(1, j) * allow_alloc(1, j) / 100.
    emrev(12, j) = pot_auction_pool * etax  ! assign auction revenue based on year allowance is created for

    ! emrev(8,) was used once to hold auction revenue given an early-auction from the allocation for the first three compliance years.
    ! no longer used.
    emrev(8, j) = emlim(1, j) * etax * (allow_alloc(1, j) / 100.)  ! etax is in nominal dollars here

    ! Calculate the value of the incremental sequestration incentive (s2191).
    emrev(10, j) = offset(4, curiyr) * etax

    ! Spending on int'l offsets
    emrev(11, j) = ghg_rev(4, j) * mc_jpgdp(j)

    ! Share out to sectors based on emissions
    adjust = 1.
    if (rev > 0 .and. rev_before /= 0.) then
        adjust = rev / rev_before
        do k = 1, 5
            emrev(k, j) = adjust * emrev(k, j)
        enddo
    endif
    ghg_rev(1, j) = adjust * ghg_rev(1, j)

! Eliminate sharing to sector--misleading and won't get used in this version
!s1766  emrev(1:5,j)=0.  ! GSV TODO: remove this comment? is it just commented code?
else
    emrev(6, j) = sum(emrev(1:5, j))
endif

return
end subroutine accntrev


!> Calculate revenue produced by holding the initial allocation of permits.

!> The total revenue is the number of permits multiplied by its value. We sum
!> this by sector.

subroutine initrev

implicit none

include 'parametr'
include 'ncntrl'
include 'emoblk'
include 'macout'
include 'emmparm'
include 'emission'
include 'epmbank'

integer i, j, k
real*4 etax, etaxendu, etaxresd, etaxcomm, etaxtran, etaxelec  ! tax value in nominal or 87 dollars

j = curiyr

! Set etax equal to the emissions tax converted to nominal $'s
if (.not. nominal_flag) then
    etax = emtax(j) * mc_jpgdp(j)  ! convert to nominal $'s
else
    etax = emtax(j)  ! already in nominal $'s
endif

etaxelec = etax
etaxtran = etax
etaxendu = etax
etaxresd = etax
etaxcomm = etax

if (elec_flag) then
    etaxendu = 0.0  ! if elec-only, zero end-use tax copy
    etaxtran = 0.0
endif
if (tran_flag) then
    etaxendu = 0.0  ! if elec-only, zero end-use tax copy
    etaxelec = 0.0
endif
if (resd_flag .or. elec_flag .or. tran_flag) etaxresd = 0.0  ! if residential excluded or elec-only, zero resd tax copy
if (comm_flag .or. elec_flag .or. tran_flag) etaxcomm = 0.0  ! if commercial  excluded or elec-only, zero comm tax copy

if (bank_flag .and. curcalyr >= bank_startyr) then
    emrev(1, j) = emrev(1, j) - init_alloc(1, j) * etaxresd
    emrev(2, j) = emrev(2, j) - init_alloc(2, j) * etaxcomm
    emrev(3, j) = emrev(3, j) - init_alloc(3, j) * etaxendu
    emrev(4, j) = emrev(4, j) - init_alloc(4, j) * etaxtran
    emrev(5, j) = emrev(5, j) - init_alloc(5, j) * etaxelec
endif

return
end subroutine initrev


!> Read input files for EPM and open EPM output file.

!> Called from routine in main.f to read contents of the EPMCNTL and EPMDATA
!> files, as well as opening the EPMOUT file for module output.

subroutine epm_read

implicit none

include 'parametr'
include 'ncntrl'
include 'emeblk'
include 'emoblk'
include 'emmparm'
include 'emission'
include 'ecpcntl'
include 'epmbank'
include 'epmcntl'
include 'macout'
include 'ab32'

integer iunit1, iunit_epmcntl, iunit_epmdata, bank_onyr
integer i, j, k, l, iy, ir, icls, ncls  ! index variables
integer file_mgr
external file_mgr
integer rtovalue
external rtovalue
integer skip_hg
integer ab32sw
logical is_new_file
character*18 filename_epmcntl, filename_epmdata, filename_epmout
integer hist  ! Number of years of emissions factor data
data hist /32/  ! Read history CO2 data through NEMS year 32 (2021)
common /epm_out/ iunit1, bank_onyr  ! Keep unit number of EPM output file in common
integer isec, ifuel, my, myr, ryr, rnd, ayr, and, ntier, rsvsw, rsvyr
character*50 dum
real xdum
integer nyrhg(mnumyr)
real mefhg(mnumyr)
real ab32yr, rsvgrw, aucgrw

filename_epmcntl = 'EPMCNTL           '  ! control flag file
filename_epmdata = 'EPMDATA           '  ! data file
filename_epmout = 'EPMOUT            '  ! output file

is_new_file = .true.

! Open output file for all time
iunit1 = file_mgr('O', filename_epmout, is_new_file)
if (iunit1 <= 0) then
    call file_err(iunit1, 'O', filename_epmout, curiyr, curitr, 'EPM       ')
endif

! Open control and data files
iunit_epmcntl = file_mgr('O', filename_epmcntl, .not. is_new_file)
if (iunit_epmcntl <= 0) then
    call file_err(iunit_epmcntl, 'O', filename_epmcntl, curiyr, curitr, 'EPM       ')
else
    call epmnext(iunit_epmcntl)
    read(iunit_epmcntl, *, err=120) dbugepm
    call epmnext(iunit_epmcntl)
    read(iunit_epmcntl, *, err=120) nominal_flag

    call epmnext(iunit_epmcntl)
    read(iunit_epmcntl, *, err=120) tax_flag, permit_flag, market_flag, offset_flag, etax_flag
    elec_flag = .false.
    tran_flag = .false.
    resd_flag = .false.
    comm_flag = .false.
    bank_flag = .false.
    read(iunit_epmcntl, *, end=20) elec_flag  ! flag for electric sector only
    read(iunit_epmcntl, *, end=20) tran_flag  ! flag for transportation sector only
20  continue
    read(iunit_epmcntl, *, end=21) resd_flag  ! flag for resd sector exclusion from
21  continue
    read(iunit_epmcntl, *, end=22) comm_flag  ! flag for comm sector exclusion from
22  continue

    ! Flag escalated fee with bank to meet cap over long period.  Used with tax_flag=.true.
    read(iunit_epmcntl, *, end=23) bank_flag
23  continue
    read(iunit_epmcntl, *, end=24) bank_priceyr  ! start year for bank, initial fee (if bank_flag) true 
24  continue
    read(iunit_epmcntl, *, end=25) bank_startyr  ! start year for bank or offsets if bank_flag or offset_flag is true
25  continue
    read(iunit_epmcntl, *, end=26) bank_endyr  ! end year for bank, initial fee (if bank_flag) true 
26  continue
    bank_end_balance = 0.
    read(iunit_epmcntl, *, end=27) bank_end_balance  ! target balance in end-year for bank 
27  continue
    bank_onyr = rtovalue('BANKONYR', 2051)  ! year between bank_startyr and bank_endyr in which the banking and price path apply.
    if (bank_onyr < bank_startyr) bank_onyr = bank_startyr  ! when bank_startyr < bank_onyr, price determined without banking initially
    if (bank_onyr > bank_endyr) bank_onyr = bank_startyr

    if (dbugepm > 0) then
        write(iunit1, *, err=120) ' Tax_Flag        :', tax_flag
        write(iunit1, *, err=120) ' Bank_Flag       :', bank_flag
        write(iunit1, *, err=120) ' Bank_PriceYr    :', bank_priceyr
        write(iunit1, *, err=120) ' Bank_StartYr    :', bank_startyr
        write(iunit1, *, err=120) ' Bank_OnYr       :', bank_onyr
        write(iunit1, *, err=120) ' Bank_EndYr      :', bank_endyr
        write(iunit1, *, err=120) ' Bank_End_Balance:', bank_end_balance
        write(iunit1, *, err=120) ' BANKSAFE Option :', rtovalue('BANKSAFE', 0)
        write(iunit1, *, err=120) ' Permit_Flag     :', permit_flag
        write(iunit1, *, err=120) ' Market_Flag     :', market_flag
        write(iunit1, *, err=120) ' OFFSET_Flag     :', offset_flag
        write(iunit1, *, err=120) ' ETax_Flag       :', etax_flag
        write(iunit1, *, err=120) ' ELEC_Flag       :', elec_flag
        write(iunit1, *, err=120) ' TRAN_Flag       :', tran_flag
        write(iunit1, *, err=120) ' RESD_Flag       :', resd_flag
        write(iunit1, *, err=120) ' COMM_Flag       :', comm_flag
    endif  ! (dbugepm)

    iunit_epmcntl = file_mgr('C', filename_epmcntl, .not. is_new_file)
    if (iunit_epmcntl < 0) &
        call file_err(iunit_epmcntl, 'C', filename_epmcntl, curiyr, curitr, 'EPM       ')
endif  ! open epmcntl successful

iunit_epmdata = file_mgr('O', filename_epmdata, .not. is_new_file)
if (iunit_epmdata > 0) then
    write(iunit1, *) ' NOW IN EPM_READ'
    call epmnext(iunit_epmdata)
    do i = 1, mnumyr
        read(iunit_epmdata, *, err=120) emtax(i), emissions_goal(i), max_tax(i), min_tax(i)
        emlim(1, i) = emissions_goal(i)  ! carbon goal -- ftab
    enddo
    call epmnext(iunit_epmdata)
    read(iunit_epmdata, 190, err=120) (cl_cdnum(i), i = 1, 9)
    call epmnext(iunit_epmdata)
    do j = 1, 5
        read(iunit_epmdata, 190, err=120) (cl_cdmap(i, j), i = 1, 9)
    enddo
    call epmnext(iunit_epmdata)
    read(iunit_epmdata, *, err=120) egfrs(hist)  ! hist is year index for GHG data pub year from which factors were obtained
    read(iunit_epmdata, *, err=120) egfcm(hist)
    read(iunit_epmdata, *, err=120) egftr(hist)
    read(iunit_epmdata, *, err=120) egfin(hist)
    read(iunit_epmdata, *, err=120) egfel(hist)
    read(iunit_epmdata, *, err=120) egirs(hist)
    read(iunit_epmdata, *, err=120) egicm(hist)
    read(iunit_epmdata, *, err=120) egitr(hist)
    read(iunit_epmdata, *, err=120) egiin(hist)
    read(iunit_epmdata, *, err=120) egiel(hist)
    read(iunit_epmdata, *, err=120) engrs(hist)
    read(iunit_epmdata, *, err=120) engcm(hist)
    read(iunit_epmdata, *, err=120) engtr(hist)
    read(iunit_epmdata, *, err=120) engin(hist)
    read(iunit_epmdata, *, err=120) engel(hist)
    enghm(hist) = engel(hist)  ! hardwire for now as requested by PKC
    read(iunit_epmdata, *, err=120) enqngpf(hist)
    read(iunit_epmdata, *, err=120) egptr(hist)
    read(iunit_epmdata, *, err=120) elpin(hist)
    read(iunit_epmdata, *, err=120) eclrs(hist)
    read(iunit_epmdata, *, err=120) eclcm(hist)
    read(iunit_epmdata, *, err=120) eclin(hist)
    eclhm(hist) = eclin(hist)  ! hardwire for now as requested by PKC
    read(iunit_epmdata, *, err=120) emcin(hist)
    read(iunit_epmdata, *, err=120) emgcm(hist)
    read(iunit_epmdata, *, err=120) emgtr(hist)
    read(iunit_epmdata, *, err=120) emgin(hist)
    read(iunit_epmdata, *, err=120) ejftr(hist)
    read(iunit_epmdata, *, err=120) edsrs(hist)
    read(iunit_epmdata, *, err=120) edscm(hist)
    read(iunit_epmdata, *, err=120) edstr(hist)
    read(iunit_epmdata, *, err=120) edsin(hist)
    read(iunit_epmdata, *, err=120) edsel(hist)
    read(iunit_epmdata, *, err=120) eksrs(hist)
    read(iunit_epmdata, *, err=120) ekscm(hist)
    read(iunit_epmdata, *, err=120) eksin(hist)
    read(iunit_epmdata, *, err=120) elgrs(hist)
    read(iunit_epmdata, *, err=120) elgcm(hist)
    read(iunit_epmdata, *, err=120) elgtr(hist)
    read(iunit_epmdata, *, err=120) elgin(hist)
    read(iunit_epmdata, *, err=120) eprrs(hist)
    read(iunit_epmdata, *, err=120) eprcm(hist)
    read(iunit_epmdata, *, err=120) eprtr(hist)
    read(iunit_epmdata, *, err=120) eetin(hist)
    read(iunit_epmdata, *, err=120) eprin(hist)
    read(iunit_epmdata, *, err=120) ebuin(hist)
    read(iunit_epmdata, *, err=120) eisin(hist)
    read(iunit_epmdata, *, err=120) eetinpf(hist)
    read(iunit_epmdata, *, err=120) eprinpf(hist)
    read(iunit_epmdata, *, err=120) ebuinpf(hist)
    read(iunit_epmdata, *, err=120) eisinpf(hist)
    read(iunit_epmdata, *, err=120) erlcm(hist)
    read(iunit_epmdata, *, err=120) erltr(hist)
    read(iunit_epmdata, *, err=120) erlin(hist)
    read(iunit_epmdata, *, err=120) erlel(hist)
    read(iunit_epmdata, *, err=120) erhtr(hist)
    read(iunit_epmdata, *, err=120) erhel(hist)
    read(iunit_epmdata, *, err=120) erscm(hist)
    read(iunit_epmdata, *, err=120) erstr(hist)
    read(iunit_epmdata, *, err=120) ersin(hist)
    read(iunit_epmdata, *, err=120) ersel(hist)
    read(iunit_epmdata, *, err=120) eottr(hist)
    read(iunit_epmdata, *, err=120) epfin(hist)
    read(iunit_epmdata, *, err=120) epcin(hist)
    read(iunit_epmdata, *, err=120) eppin(hist)
    read(iunit_epmdata, *, err=120) eppinpf(hist)
    read(iunit_epmdata, *, err=120) eluin(hist)
    read(iunit_epmdata, *, err=120) enqlgpf(hist)
    read(iunit_epmdata, *, err=120) esgin(hist)
    read(iunit_epmdata, *, err=120) eotin(hist)
    read(iunit_epmdata, *, err=120) epcel(hist)
    read(iunit_epmdata, *, err=120) emetr(hist)
    read(iunit_epmdata, *, err=120) eettr(hist)
    read(iunit_epmdata, *, err=120) emsel(hist)
    read(iunit_epmdata, *, err=120) eciin(hist)
    ebmhm(hist) = -25.05  ! hardwire for now as requested by PKC

    ! All elec utility emissions factors the same
    egfelgr(hist) = egfel(hist)
    egielgr(hist) = egiel(hist)
    egcelgr(hist) = egiel(hist)

    ! Mercury related variables
    call epmnext(iunit_epmdata)
    read(iunit_epmdata, *) dum, usw_mact, umact_yr, usw_bact, ubact_yr, usw_dsi, udsi_yr, ucap_hg
    usw_hg = 0
    call epmnext(iunit_epmdata)
    num_hg_grp = 0
    do i = 1, ndreg
        read(iunit_epmdata, *) isec, dum, hg_grp(i)
        if (hg_grp(i) > num_hg_grp) num_hg_grp = hg_grp(i)
    enddo
    call epmnext(iunit_epmdata)
    read(iunit_epmdata, *) ncls  ! number of mercury emission capacity classes
    do icls = 1, ncls
        call epmnext(iunit_epmdata)
        do my = 1, mnumyr
            read(iunit_epmdata, *) myr, hg_grams_mwh(icls, my), &
                (hg_output(icls, i, my), i = 1, 3), &
                (hg_input(icls, j, my), j = 1, 3), &
                (hg_mef(icls, k, my), k = 1, 3)
            if (hg_grams_mwh(icls,my) > 0.0) then
                do i = 1, 3
                    hg_output(icls, i, my) = hg_grams_mwh(icls, my) * 2.2046
                enddo
                hg_grams_mwh(icls, my) = 0.0
            endif
        enddo
    enddo
    call epmnext(iunit_epmdata)
    read(iunit_epmdata, *) usw_camr
    call epmnext(iunit_epmdata)
    read(iunit_epmdata, *) ncls  ! number of years with different standards
    call epmnext(iunit_epmdata)
    read(iunit_epmdata, *) isec, dum, (nyrhg(j), j = 1, ncls)
    do i = 1, ndreg
        read(iunit_epmdata, *) isec, dum, (mefhg(j), j = 1, ncls)
        do j = 1, ncls
            if (j == 1) then
                do k = 1, nyrhg(j) - (baseyr - 1) - 1
                    hg_mefnc(i, k) = 1.0
                enddo
            else
                do k = nyrhg(j - 1) - (baseyr - 1), nyrhg(j) - (baseyr - 1) - 1
                    hg_mefnc(i, k) = mefhg(j - 1)
                enddo
            endif
            if (j == ncls) then
                do k = nyrhg(j) - 1989, mnumyr
                    hg_mefnc(i, k) = mefhg(j)
                enddo
            endif
        enddo
    enddo

    call epmnext(iunit_epmdata)

    ! Check for switch to ignore mercury limits and skip structure
    skip_hg = real(rtovalue('HGSTRUCT', 0))
    do my = 1, mnumyr
        read(iunit_epmdata, *) myr, (emel_qhg(i, my), i = 1, num_hg_grp)
        if (skip_hg == 0 .or. usw_camr == 1) then
            do i = 1, num_hg_grp
                emel_qhg(i, my) = 999.9
            enddo
        endif
        emlim(4, my) = 0.0
        do i = 1, num_hg_grp
            emlim(4, my) = emlim(4, my) + emel_qhg(i, my)
        enddo
    enddo
    icls = 1
    do my = 1, ijumpyr
        if (emlim(4, my) < 100.0) usw_hg = 1
        if (hg_grams_mwh(icls, my) > 0.0) usw_hg = 1
        do i = 1, 3
            if (hg_output(icls, i, my) > 0.0) usw_hg = 1
            if (hg_input(icls, i, my) > 0.0) usw_hg = 1
            if (hg_mef(icls, i, my) > 0.0) usw_hg = 1
        enddo
    enddo
    if (usw_mact > 0) usw_hg = 1
    if (usw_camr > 0) usw_hg = 1
    call epmnext(iunit_epmdata)
    read(iunit_epmdata, *) hgdsel
    read(iunit_epmdata, *) hgrlel
    read(iunit_epmdata, *) hgrhel
    read(iunit_epmdata, *) hgngel

    ! Read emission allowance auction allocation share.
    ! (1: auction share) is the pass-through percentage for regulated electricity pricing
    call epmnext(iunit_epmdata)
    em_auction_sh(1:ijumpyr) = 0.7  ! default value in case epmdata.txt doesn't have it
    read(iunit_epmdata, *, err=981) em_auction_sh
981 continue

    ! Read California AB32 inputs
    ab32sw = rtovalue('AB32SW  ', 1)
    call epmnext(iunit_epmdata)
    read(iunit_epmdata, *) ab32yr, ryr, rnd, ntier, rsvgrw, ayr, and, aucgrw
    call epmnext(iunit_epmdata)

    ! Initialize variables to determine 1st year for containment reserves
    rsvsw = 0
    rsvyr = 0
    do my = 1, mnumyr
        read(iunit_epmdata, *) myr, ab_cap_tot(my), ab_offset_frac(my), ab_cstcont_frac(my), &
                        (ab_reserve_p(j ,my), j = 1, ntier), ab_auction_p(my)
        if (ab32sw <= 0) then

            ! If no AB32, set inputs to unlimited or 0
            ab_cap_tot(my) = 272.727
            ab_offset_frac(my) = 0.0
            ab_cstcont_frac(my) = 0.0
            do j = 1, ntier
                ab_reserve_p(j, my) = 0.0
            enddo
            ab_auction_p(my) = 0.0
        else

            ! Convert cap from mm metric tons CO2 to mm metric tons C
            ab_cap_tot(my) = ab_cap_tot(my) * 12.0 / 44.0

            ! Determine 1st year for containment reserve and accumulate total reserve allowances in 1st year
            if (ab_cstcont_frac(my) > 0.0) then
                if (rsvsw == 0 .and. rsvyr == 0) then
                    rsvsw = 1
                    rsvyr = my
                    ab_cstcont_avl(my) = ab_cap_tot(my) * ab_cstcont_frac(my)
                else
                    ab_cstcont_avl(rsvyr) = ab_cstcont_avl(rsvyr) + ab_cap_tot(my) * ab_cstcont_frac(my)
                endif
            endif
            do j = 1, ntier
                ab_reserve_p(j, my) = (ab_reserve_p(j, my) * (44.0 / 12.0) / mc_jpgdp(ab32yr - 1989)) * 0.001
                if (myr > ryr) &
                    ab_reserve_p(j, my) = ab_reserve_p(j, my) * (1.0 + rsvgrw) ** (float(min(rnd, myr) - ryr))
            enddo
            ab_auction_p(my) = (ab_auction_p(my) * (44.0 / 12.0) / mc_jpgdp(ayr - 1989)) * 0.001
            if (myr > ayr) &
                ab_auction_p(my) = ab_auction_p(my) * (1.0 + aucgrw) ** (float(min(and, myr) - ayr))
        endif
    enddo

    iunit_epmdata = file_mgr('C', filename_epmdata, .not. is_new_file)
    if (iunit_epmdata < 0) &
        call file_err(iunit_epmdata, 'C', filename_epmdata, 1, 1, 'READDATA  ')

    ! Emissions factors are the same for all years, set by sector
    do j = 1, lastyr
        egfrs(j) = egfrs(hist)
        egirs(j) = egirs(hist)
        engrs(j) = engrs(hist)
        eclrs(j) = eclrs(hist)
        edsrs(j) = edsrs(hist)
        eksrs(j) = eksrs(hist)

        egfcm(j) = egfcm(hist)
        egicm(j) = egicm(hist)
        engcm(j) = engcm(hist)
        eclcm(j) = eclcm(hist)
        emgcm(j) = emgcm(hist)
        edscm(j) = edscm(hist)
        ekscm(j) = ekscm(hist)
        elgcm(j) = elgcm(hist)
        erlcm(j) = erlcm(hist)
        erscm(j) = erscm(hist)

        egfin(j) = egfin(hist)
        egiin(j) = egiin(hist)
        engin(j) = engin(hist)
        elpin(j) = elpin(hist)
        eclin(j) = eclin(hist)
        emcin(j) = emcin(hist)
        eciin(j) = eciin(hist)
        emgin(j) = emgin(hist)
        edsin(j) = edsin(hist)
        eksin(j) = eksin(hist)
        elgin(j) = elgin(hist)
        eprrs(j) = eprrs(hist)
        eprcm(j) = eprcm(hist)
        eprtr(j) = eprtr(hist)
        eetin(j) = eetin(hist)
        eprin(j) = eprin(hist)
        ebuin(j) = ebuin(hist)
        eisin(j) = eisin(hist)
        eetinpf(j) = eetinpf(hist)
        eprinpf(j) = eprinpf(hist)
        ebuinpf(j) = ebuinpf(hist)
        eisinpf(j) = eisinpf(hist)
        erlin(j) = erlin(hist)
        ersin(j) = ersin(hist)
        enqngpf(j) = enqngpf(hist)
        enqlgpf(j) = enqlgpf(hist)
        epfin(j) = epfin(hist)
        epcin(j) = epcin(hist)
        eppin(j) = eppin(hist)
        eppinpf(j) = eppinpf(hist)
        eluin(j) = eluin(hist)
        esgin(j) = esgin(hist)
        eotin(j) = eotin(hist)

        egftr(j) = egftr(hist)
        egitr(j) = egitr(hist)
        engtr(j) = engtr(hist)
        egptr(j) = egptr(hist)
        emgtr(j) = emgtr(hist)
        ejftr(j) = ejftr(hist)
        edstr(j) = edstr(hist)
        elgtr(j) = elgtr(hist)
        erltr(j) = erltr(hist)
        erhtr(j) = erhtr(hist)
        erstr(j) = erstr(hist)
        eottr(j) = eottr(hist)
        emetr(j) = emetr(hist)
        eettr(j) = eettr(hist)

        edsel(j) = edsel(hist)
        erlel(j) = erlel(hist)
        erhel(j) = erhel(hist)
        ersel(j) = ersel(hist)
        epcel(j) = epcel(hist)
        emsel(j) = emsel(hist)
        egfelgr(j) = egfelgr(hist)
        egielgr(j) = egielgr(hist)
        egcelgr(j) = egcelgr(hist)
        egfel(j) = egfel(hist)
        egiel(j) = egiel(hist)
        engel(j) = engel(hist)

        enghm(j) = enghm(hist)
        eclhm(j) = eclhm(hist)
        ebmhm(j) = ebmhm(hist)
    enddo
    return
else
    call file_err(iunit_epmdata, 'O', filename_epmdata, 1, 1, 'READDATA  ')
    return
endif

100 format(29(9x,f7.3,3x,f9.3))
190 format(1x,11(1x,i2))
200 format(f9.3)

120 write(6, *) 'FAILED READ IN EPMDATA RESULTS INDETERMINATE'

return
end subroutine epm_read


!> Read through a file until an '@' is found in the first column.

!> Utility subroutine called by epm_read to advance in the EPMDATA file.
!> Adapted from "Fortran for Scientists & Engineers" Chapman, pg. 220, program
!> read_file.
!> 
!> @param unitnum The unit number of the file to read.

!> @see epm_read

subroutine epmnext(unitnum)

implicit none

integer unitnum
intent(in) :: unitnum

character(len=1) ch
integer io_status
character(len=200) io_message

ch = 'A'  ! Initialize ch to any value that is not '@'

do while (ch /= '@')
    
    ! Consume one line of the file, storing its first character in ch
    read(unitnum, '(a1)', iostat=io_status, iomsg=io_message) ch
    
    if (io_status /= 0) then
        
        if (io_status > 0) then
            write(6, *) 'epmnext error condition while reading file:'
        elseif (io_status < 0) then 
            write(6, *) 'epmnext found EOF or EOR while reading file:'
        endif
        
        write(6, *) 'io_message: ', trim(io_message)
        write(6, *) 'io_status = ', io_status
        
        exit
    endif
enddo

return
end subroutine epmnext


!> Print a series of error messages indicating a file access failure.

!> Used in several EPM subroutines to indicate a general failure to access a
!> file. The name of the file and its unit number will be printed along with
!> the action that was attempted. In addition, file_err prints the name of the
!> subroutine that reported the error as well as the current NEMS year and
!> iteration.
!> 
!> @param unitnum Unit number of the file accessed.
!> @param action Either 'C' for close or 'O' for open.
!> @param fname Name of file attempting to access.
!> @param cyr Current year.
!> @param ctr Current iteration.
!> @param subname Name of subroutine where error occured.

subroutine file_err(unitnum, action, fname, cyr, ctr, subname)

implicit none

integer unitnum
character*1 action
character*18 fname
integer cyr, ctr
character*10 subname
intent(in) :: unitnum, cyr, ctr
intent(in) :: action, fname, subname

write(6, *)
write(6, *) '  ****FILE ACCESS ERROR---PROGRAM HALTED'
write(6, *)
write(6, *) 'SUBROUTINE NAME : ', subname
write(6, *) 'FILENAME : ', fname, 'UNIT NUMBER = ', unitnum
write(6, *) 'CURIYR = ', cyr, 'CURITR = ', ctr
write(6, *) 'ACTION : ', action
write(6, *)
write(6, *) '  ****FILE ACCESS ERROR---'
write(6, *)

120 format(a18,a10)
130 format(a12,a18,5x,a14,i3)
140 format(a9,i3,5x,a9,i3)
150 format(a9,a1)

return
end subroutine file_err


!> Estimate starting carbon price of a series using regula falsi root finding.

!> Use a regula falsi routine to estimate the starting carbon price of an
!> escalating series that results in a carbon emissions cumulative bank balance
!> of zero. Similar to regfalsi, but run once per cycle (run) prior to first
!> year of the program, using results (brackets) from prior runs. Regula falsi
!> finds the root of a function given two points, one with positive and one
!> with negative functional value. It has linear convergence rate and is
!> guaranteed to converge if the function is continuous.
!> 
!> @param new_tax Latest carbon allowance fee or tax.
!> @param new_sum Latest carbon balance (passed as negative for consistency
!> with similar routine).

subroutine regfalsibank(new_tax, new_sum)

implicit none

include 'parametr'
include 'ncntrl'
include 'emmparm'
include 'emission'
include 'emoblk'
include 'emablk'
include 'emeblk'

! The following include stores the solution bracket data for this routine in
! the restart file. If bracket data is zero, start from scratch.
include 'epmbank'

real new_tax, new_sum
intent(inout) :: new_tax
intent(inout) :: new_sum  ! The value of new_sum is not actually passed out, so its intent *should* be intent(in).
                          ! However, it is updated in this subroutine, so the compiler won't let it have intent(in).
                          ! GSV TODO: refactor this subroutine by adding a new local variable to solve this.

character*3 yesno(0:1) /'No ', 'Yes'/
real tol /5.0/  ! tolerance on reaching long term banking goal
real epm_factor /.01/  ! the factor to branch outward to get the bracketing points
real base_tax  ! used to calculate tax in first iteration
real movedown, moveupup

! In rebracketing request logic, if last 2 newsums bigger than ctb, adjust allowance price by slope rather than brackets
real ctb /0.0/

integer i, iy
integer iunit1, bank_onyr
common /epm_out/ iunit1, bank_onyr
integer rebracket
integer rtovalue
external rtovalue

! First iteration, set upper and lower points equal to only point
iy = bank_onyr - 1989  ! NEMS year index for which calculations apply
slope = -epm_factor
rebracket = rtovalue('REBRACKT', 0)

write(iunit1, *) ' '
Write(iunit1, 205) 'Banking Bracket--Regula Falsi-Before Changes, prior iterations'
write(iunit1, 205) 'Bank ', iter, ' Low_tax*1000.', low_tax * 1000.
write(iunit1, 205) 'Bank ', iter, 'High_tax*1000.', high_tax * 1000.
write(iunit1, 205) 'Bank ', iter, '       Low_sum', low_sum
write(iunit1, 205) 'Bank ', iter, '      High_sum', high_sum
write(iunit1, 206) 'Bank ', iter, '    Bracketed?', yesno(bracket)
write(iunit1, 205) 'Bank ', iter, '         Slope', slope
write(iunit1, 205) 'Bank ', iter, '       new_sum', new_sum
write(iunit1, 205) 'Bank ', iter, ' new_tax*1000.', new_tax * 1000.

! iter -- count on how many times this routine is run
iter = iter + 1 
if (iter > 100) then
    newsum(1:10) = newsum(91:100)
    newtax(1:10) = newtax(91:100)
    newsum(11:100) = 0
    newtax(11:100) = 0
    iter = 11
    write(iunit1, *) 'Bank iter # 100 changed to 10.  New iter # is 11'
endif

newsum(iter) = new_sum  ! record prior run's result
newtax(iter) = new_tax

if (iter > 1) then
    write(iunit1, '(a)') 'iter  newtax  newsum'
    do i = 1, iter
        write(iunit1, '(i4,2f9.2)') i, newtax(i) * 1000., newsum(i)
    enddo
    write(iunit1, *) ' '
endif

! Start of rebracketing logic
if (rebracket /= 0) then
    bracket = 0
    if (curirun == 1) then
        write(iunit1, *) ' '
        write(iunit1, *) ' I see that you are requesting a rebracket.  Can do.'
    else
        if ((curirun == 2 .and. (newsum(iter) * newsum(iter - 1) < 0)) &
          .or. (curirun > 2 .and. (low_sum * high_sum < 0)) &
          .or. (curirun > 2 &
          .and. ((low_sum < 0 .and. new_sum > 0) &
          .or. (low_sum > 0 .and. new_sum < 0)))) &
            bracket = 1
    endif
endif

write(iunit1, *) ' '

! Rebracketing request logic
if (rebracket /= 0 .and. curirun == 1) then
    newsum(3:100) = 0.0
    newtax(3:100) = 0.0
    iter = 3

    ! Note that "low tax" represents high fee resulting in emissions lower than target
    if (rebracket == 9999) then  ! signal to change both low and high brackets
        write(iunit1, *) ' '
        write(iunit1, *) ' Adjusting both sides of the bracket by 50%'
        low_tax = low_tax * 1.5
        high_tax = high_tax * 0.5
        newtax(1) = high_tax
        newtax(2) = low_tax
        newsum(1) = high_sum
        newsum(2) = low_sum
        new_sum = low_sum
        new_tax = low_tax
    else
        write(iunit1, *) ' '
        newsum(1) = high_sum
        newsum(2) = low_sum
        new_sum = newsum(2)

        if (rebracket < 0) then

            ! Negative rebracket value means move lower ("high") bracket price
            write(iunit1, '(a,i4,a)') ' Adjusting lower allowance fee bracket downward by ', &
                -1 * rebracket, ' percent'
            newtax(1) = high_tax * (1.0 + real(rebracket) / 100.)
            newtax(2) = low_tax
            high_tax = newtax(1)
            low_tax = newtax(2)

        elseif (rebracket < 100) then

            ! Positive rebracket value means move higher ("low") bracket price
            write(iunit1, '(a,i4,a)') ' Adjusting upper allowance fee bracket upward by ', &
                rebracket, ' percent'
            newtax(1) = high_tax
            newtax(2) = low_tax * (1.0 + real(rebracket) / 100.)
            high_tax = newtax(1)
            low_tax = newtax(2)
        else
            movedown = real(mod(rebracket, 100))
            moveupup = real(rebracket - mod(rebracket, 100)) / 100.
            write(iunit1, '(a,i4,a)') ' Adjusting high fee up by ', int(moveupup), ' percent.'
            write(iunit1, '(a,i4,a)') ' Adjusting low fee up down ', int(movedown), ' percent.'
            newtax(1) = high_tax * (1.0 - movedown / 100.)
            newtax(2) = low_tax * (1.0 + moveupup / 100.)
            high_tax = newtax(1)
            low_tax = newtax(2)
        endif
    endif
    newsum(iter) = new_sum
    newtax(iter) = new_tax
    new_tax = (high_tax * low_sum - low_tax * high_sum) / (low_sum - high_sum)
    write(iunit1, '(a)') ' '
    write(iunit1, '(a)') 'New history:'
    write(iunit1, '(a)') 'iter  newtax  newsum'
    do i = 1, iter
        write(iunit1, '(i4,2f9.2)') i, newtax(i) * 1000., newsum(i)
    enddo
    write(iunit1, *) ' '
    goto 999
elseif (rebracket /= 0 .and. curirun >= 3 .and. bracket == 0) then

    !  Rebracketed in first cycle, checking progress and possibly adjust rebracketing
    !  There's no need to check cycle 2.  It's too early to tell
    if ((new_sum < (-1 * ctb) .and. newsum(curirun + 3 - 2) < (-1 * ctb)) &
      .or. (new_sum > ctb .and. newsum(curirun + 3 - 2) > ctb)) then
        write(iunit1, '(a)') '  I see that our initial rebracketing needs to be adjusted.'
        new_tax = new_tax - (new_tax - newtax(curirun + 3 - 2)) * &
                  new_sum / (new_sum - newsum(curirun + 3 - 2))
        if (new_tax > max_tax(iy)) new_tax = max_tax(iy)
        if (new_tax < min_tax(iy)) new_tax = newtax(iter) / 2.
        if (new_tax < min_tax(iy)) new_tax = min_tax(iy) 
        write(iunit1, '(a,f8.3,a)') ' Adjusting guess to ', new_tax * 1000., ' to attempt to bracket.'
        write(iunit1, '(a)') ' '
        goto 999
    endif
endif

! First iteration need only find one point
if (iter == 1) then
    bracket = 0

    ! Assume from under-compliance (new_sum > 0, balance(bank_endyr) < 0), given a zero or low initial price
    high_sum = new_sum
    high_tax = new_tax

    ! Set next price partway between starting and max price.
    new_tax = (new_tax * .8 + max_tax(iy) * .2)

    ! Set up initial bracket values
    low_sum = -new_sum
    low_tax = max_tax(iy)

    do i = 2, 100
        newsum(i) = 0.
        newtax(i) = 0.
    enddo

    base_tax = new_tax  ! passed as argument -- comes from epmdata or restart file?
    goto 999 ! go to end of routine
endif  ! iteration 1

! Second iteration, set low and high correctly and find new
! point for third: this starts with average of low and high
! and moves appropriately
if (iter == 2) then
    if (new_sum > high_sum) then
        low_sum = high_sum
        low_tax = high_tax
        high_sum = new_sum
        high_tax = new_tax
    else
        low_sum = new_sum
        low_tax = new_tax
    endif

    ! Check if bracketing
    if (low_sum * high_sum <= 0) then
        bracket = 1
    endif
endif  ! end of iteration two

! For iterations greater than two, we continue
! If not bracketing, we need to find two points bracketing zero
if (iter > 2) then
    if (low_sum * high_sum > 0) then

        ! If we are not bracketing there are four cases
        ! 1. Both still below zero, reset high and keep trying
        ! if carbon total still below, but tax higher, use it.
        ! This avoids certain anomalies in the function
        if (low_sum < 0 .and. new_sum < 0) then
            low_sum = new_sum
            low_tax = new_tax

        ! 2. Now bracketing; use reg falsi
        elseif (low_sum < 0 .and. new_sum > 0) then
            high_sum = new_sum
            high_tax = new_tax
            bracket = 1

        ! 3. Also bracketing; use reg falsi
        elseif (low_sum > 0 .and. new_sum < 0) then
            low_sum = new_sum
            low_tax = new_tax
            bracket = 1

        ! 4. Not bracketing both over user lesser tax value: assume monoton
        elseif (low_sum > 0 .and. new_sum > 0) then
            high_sum = new_sum
            high_tax = new_tax
        endif  ! four cases, not bracketing before
    else

        ! Bracketed, new positive value replaces old positive
        bracket = 1  ! reinforce bracket (can fall through to here from rebracketing logic)
        if (new_sum * low_sum > 0) then

            ! new_sum and low_sum both negative. new_sum becomes the
            ! NEW low_sum. Should this be conditional on bracket narrowing?
            low_sum = new_sum
            low_tax = new_tax
        else

            ! Now bracketed, new value positive, replaces old high_sum (positive)
            high_sum = new_sum
            high_tax = new_tax
        endif  ! fitting new value while bracketed
    endif  ! whether or not bracketed previously
endif  ! curitr > 2

slope = -epm_factor
if (newsum(iter) - newsum(iter - 1) /= 0.) then
    if (etax_flag) then
        slope = (100. * (newtax(iter) - newtax(iter - 1))) / &
                (newsum(iter) - newsum(iter - 1))
    else
        slope = (1000. * (newtax(iter) - newtax(iter - 1))) / &
                (newsum(iter) - newsum(iter - 1))
    endif
    if (slope >= -epm_factor / 10.) slope = -epm_factor / 10.
    if (slope <= (-10. * epm_factor)) slope = -10. * epm_factor
endif

if (bracket == 0) then
    if (etax_flag) then
        new_tax = new_tax - slope * new_sum / 100.
    else
        new_tax = new_tax - slope * new_sum / 1000.
    endif
    if (new_tax > max_tax(iy)) new_tax = max_tax(iy)
    if (new_tax < min_tax(iy)) new_tax = min_tax(iy)
else

    !  Bracketed, given two points r-f calculates new estimate
    if (abs(new_sum) > tol) then
        if (iter /= 2) then

            ! If estimate is neg or pos three times in a row, and carbon tax not changing
            ! much (less than 1%), then opposite bracket tax needs to open up a bit. To
            ! correct, move tax bracket a little on the problem side to allow some movement.
            if (newsum(iter) < -2. * tol &
              .and. newsum(iter - 1) < -2. * tol &
              .and. newsum(iter - 2) < -2. * tol &
              .and. curirun > 0 .and. curirun < numiruns &
              .and. abs(newtax(iter) - newtax(iter - 1)) / (newtax(iter - 1) + .0001) < 0.01) then
                high_tax = high_tax * .85
                if (high_tax == 0.) high_tax = newtax(iter) * .85
                if (high_tax < min_tax(iy)) high_tax = min_tax(iy)
            elseif (newsum(iter) > 2. * tol &
              .and. newsum(iter - 1) > 2. * tol &
              .and. newsum(iter-2) > 2. * tol &
              .and. curirun > 0 .and. curirun < numiruns &
              .and. abs(newtax(iter) - newtax(iter - 1)) / (newtax(iter - 1) + .0001) < 0.01) then
                low_tax = low_tax * 1.15
                if (low_tax > max_tax(iy)) low_tax = max_tax(iy)
            endif
            new_tax = (high_tax * low_sum - low_tax * high_sum) / (low_sum - high_sum)
        else
            new_tax = (high_tax + low_tax) / 2.  ! on iteration 2, do simple average
        endif
        if (new_tax > max_tax(iy)) new_tax = max_tax(iy)
        if (new_tax < min_tax(iy)) new_tax = min_tax(iy)
    endif  ! outside tolerance
endif  ! setting new value

999 continue

write(iunit1, 205) 'Banking Bracket--Regula Falsi-After Change'
write(iunit1, 205) 'Bank ', iter, ' Low_tax*1000.', low_tax * 1000.
write(iunit1, 205) 'Bank ', iter, 'High_tax*1000.', high_tax * 1000.
write(iunit1, 205) 'Bank ', iter, '       Low_sum', low_sum
write(iunit1, 205) 'Bank ', iter, '      High_sum', high_sum
write(iunit1, 206) 'Bank ', iter, '    Bracketed?', yesno(bracket)
write(iunit1, 205) 'Bank ', iter, '         Slope', slope
write(iunit1, 205) 'Bank ', iter, '       new_tax', new_tax * 1000.

205 format(a,i3,1x,a,f10.3)
206 format(a,i3,1x,a,5x,a3)

return
end subroutine regfalsibank


!> Compute emissions of other greenhouse gases as a function of current price.

!> Includes lime production and clinker process CO2 emissions from IND,
!> accounts for many types of offsets, etc.

subroutine oghg

implicit none

include 'parametr'
include 'ncntrl'
include 'epmbank'
include 'ghgrep'
include 'emoblk'

! Pass cement-related CO2 process emissions from IND to EPM for use in GHG accounting
! Used by epm.f:
real co2_clink(mnumyr, 5)  ! Clinker process CO2 emissions in thousand metric tons CO2, by year and region
real co2_lime(mnumyr, 5)  ! Lime production process CO2 emissions in thousand metric tons CO2, by year and region
common /indepm/ co2_clink, co2_lime
real noncementco2
integer iunit1, bank_onyr
common /epm_out/ iunit1, bank_onyr
real q, qoffdom, qoffint, p, qmax, phigh, plow
real allow_per_offset /1.0/  ! Set to 1 allowance credit per international offset
! 1: percentage total offset limits applied pro-rata based on formula: 2000 / (2000 + cap)
! 2: total offsets given covered emission
! 3: domestic offset limit
! 4: international after adjustment if domestic < 900 mmtCO2eq
integer icat, icl, ioff, ncount, i
integer rtovalue
external rtovalue
real offsetdom, offsetint
real bioseq
integer base_yr  ! Last year of history-overwrites (also, see hist in epm_read)
parameter(base_yr = 32)  ! 2021 is last data year when reading epmdata history
integer offseti  ! Option to calculate int'l offsets and price independently of domestic sources based on distinct limit (offsetint)
integer bioseqok  ! Run time option. with offsets, bioseqok = 1, bio sequestration offsets count towards goal. 0==> incentives given but don't count toward goal

offseti = rtovalue('OFFSETI ', 0)  ! If 1, calc int'l offsets and price independently of domestic based on offsetint limit
bioseqok = rtovalue('BIOSEQOK', 1)  ! If 1, bio sequestration counts as an offsets. 0: incentive only

if (curitr == 1 .and. curiyr == firsyr) then
    call readoffsets
endif

if (curiyr >= base_yr .and. exi == 1) then

    ! Assume non-energy CO2, other than lime and cement process emissions,
    ! grows at 1%. Take total nonenergy CO2 (which includes adjustments to
    ! exclude bunker and military fuel) and subtract base year estimated lime
    ! and cement process emissions (converted to mmtC). Grow that small
    ! residual at 1% per year.
    noncementco2 = (ghg_bl(base_yr, 6)                        &
                   - 0.001 * (12./44.) * co2_clink(base_yr, 5) &
                   - 0.001 * (12./44.) * co2_lime(base_yr, 5)  &
                   ) * 1.01 ** (curiyr - base_yr)

    ! Add in lime and cement process emissions (converted to mmtC)
    ghg_bl(curiyr, 6) = noncementco2 + (co2_lime(curiyr, 5) + co2_clink(curiyr, 5)) * 0.001 * (12./44.)
    
    if (curiyr == lastyr .and. ncrl == 1) then
        write(iunit1, *)
        write(iunit1, '(a)') 'mmtC'
        write(iunit1, '(a)') '          non-cement          lime          cement        total'
        write(iunit1, '(a)') '         nonenergy CO2      process        process   non-energy co2'
        do i = base_yr, lastyr
            
            ! Convert all to mmtC for output
            write(iunit1, '(i4,4f15.3)') i + baseyr - 1, &
                ghg_bl(i, 6) - (co2_lime(i, 5) + co2_clink(i, 5)) * 0.001 * (12./44.), &
                co2_lime(i, 5) * 0.001 * (12./44.), &
                co2_clink(i, 5) * 0.001 * (12./44.), &
                ghg_bl(i, 6)
            
        enddo
    endif
endif

ghg_abate(curiyr, :) = 0.
offset(:, curiyr) = 0
if (ncrl == 0) ghg_rev(:, curiyr) = 0.
ghg_offsetp(curiyr) = 0.

! For S2191 estimate the amount of biogenic sequestration that could result from
! the USDA incentive program funded by a 5% allocation of allowances. Any seq used in this
! program can not also be used as an offset. Since the per-ton value of this incentive is the
! value of an allowance, and the price of offsets may be less than or equal to the price of 
! allowance, assume the USDA program is fully subscribed before using bio seq as an offset.
! Subtract this quantity from the biosequestration offset supply curve when calculating offsets.
bioseq = 0.

! Sum abatement from covered sources at current price emtax(curiyr) and
! compute covered sector emissions as: Baseline - Abatement
! Compute domestic and international offsets at current price
qoffdom = 0.
qoffint = 0.

if (ghg_ncat <= 0) ghg_ncat = 0

do icat = 1, ghg_ncat
    icl = ghg_class(icat)
    if (icl == 1) then
        q = 0.

        if (curcalyr >= bank_startyr) then
            call ghg_macs(icat, emtax(curiyr), curcalyr, q)  ! with covered emissions, full allowance price is used
            ghg_abate(curiyr, icat) = min(q, ghg_bl(curiyr, icat))
        endif
        ghg_oghg(curiyr, icat) = ghg_bl(curiyr, icat) - ghg_abate(curiyr, icat)

        if ((.not. elec_flag) .and. (.not. tran_flag)) then
            total_emissions(curiyr) = total_emissions(curiyr) + ghg_oghg(curiyr, icat)
        endif

        if (curcalyr >= bank_startyr .and. ncrl == 0 .and. (icl /= 3 .or. bioseqok == 1)) then
            ghg_rev(icl, curiyr) = ghg_rev(icl, curiyr) + emtax(curiyr) * ghg_oghg(curiyr, icat)
        endif

    elseif (icl == 5) then

        ! Comparable international allowances, unlimited
        call ghg_macs(icat, emtax(curiyr), curcalyr, q)
        ghg_abate(curiyr, icat) = q
        offset(icl - 1, curiyr) = offset(icl - 1, curiyr) + ghg_abate(curiyr, icat)
    elseif (icl > 0) then

        ! A zero makes it exclude the abatement curve from affecting the baseline
        if (offseti == 0 .or. (offseti == 1 .and. icl /= 4)) then

            ! Calc int'l offsets separately, so don't include the abatement curve
            ! Sum uncovered MAC and offsets
            if (curcalyr >= bank_startyr .and. offset_flag) then
                call ghg_macs(icat, emtax(curiyr), curcalyr, q)
                if (icl /= 3 .or. (icl == 3 .and. bioseqok == 1)) then
                    qoffdom = qoffdom + q
                endif
            endif
        elseif (offseti == 1 .and. icl == 4) then
            if (curcalyr >= bank_startyr .and. offset_flag) then
                call ghg_macs(icat, allow_per_offset * emtax(curiyr), curcalyr, q)  ! set to 1.0 now
                qoffint = qoffint + q
            endif
        endif
    endif
enddo

if (curcalyr >= bank_startyr .and. offset_flag) then

    ! Find the amount of offsets available at the current price.
    ! If more than the maximum offsets allowed, find minimum price at which the maximum occurs.
    qmax = emissions_goal(curiyr)  ! normal assumption
    offsetdom = real(rtovalue('OFFSETDO', 15)) * .01  ! max domestic offset quantity (or total of domestic and intl, if offseti = 0)
    offsetint = real(rtovalue('OFFSETIN', 15)) * .01  ! max international offset quantity if offseti = 1

    ! Find domestic offset quantity
    qmax = offsetdom * emissions_goal(curiyr)
    p = emtax(curiyr)
    if (qoffdom > qmax .and. p > 0.) then

        ! Find p that results in qsum = qmax
        phigh = p
        plow = 0.
        ncount = 0

        ! Binary search
        do while (abs(qmax-qoffdom) > 0.1 .and. ncount <= 200)

            ! 0.1 is a tolerance--tenth of a ton
            ncount = ncount + 1
            p = (phigh + plow) / 2.
            qoffdom = 0.

            ! Compute emissions at trial price
            do icat = 1, ghg_ncat
                icl = ghg_class(icat)
                if (icl > 1 .and. icl < 5) then
                    if (offseti == 0 .or. (offseti == 1 .and. icl /= 4)) then  ! changed to leave int'l out when offseti = 1

                        ! Sum uncovered MAC and offsets
                        call ghg_macs(icat, p, curcalyr, q)
                        if (icl /= 3 .or. (icl == 3 .and. bioseqok == 1)) then
                            qoffdom = qoffdom + q
                        endif
                    endif
                endif
            enddo
            if (qoffdom < qmax) then
                plow = p
            elseif (qoffdom > qmax) then
                phigh = p
            else
                plow = p
                phigh = p
            endif
        enddo
    endif

    ghg_offsetp(curiyr) = p

    if (offseti == 1) then

        ! If limit on international offsets is independent of domestic limit, do int'l price separately
        ! Find international offset price subject to maximum offset
        ! Find domestic offset price
        p = emtax(curiyr)
        qmax = offsetint * emissions_goal(curiyr)  

        if (qoffint > qmax .and. p > 0.) then

            ! Find p that results in qsum = qmax
            phigh = p
            plow = 0.
            ncount = 0

            ! Binary search
            do while (abs(qmax - qoffint) > 0.1 .and. ncount <= 200)

                ! 0.1 is a tolerance--tenth of a ton
                ncount = ncount + 1
                p = (phigh + plow) / 2.
                qoffint = 0.

                ! Compute emissions at trial price
                do icat = 1, ghg_ncat
                    icl = ghg_class(icat)
                    if (icl == 4) then

                        ! Sum any international offsets
                        call ghg_macs(icat, p, curcalyr, q)
                        qoffint = qoffint + q
                    endif
                enddo
                if (qoffint < qmax) then
                    plow = p
                elseif (qoffint > qmax) then
                    phigh = p
                else
                    plow = p
                    phigh = p
                endif
            enddo
        endif
        ghg_offsetpint(curiyr) = p
    else
        ghg_offsetpint(curiyr) = ghg_offsetp(curiyr)
    endif

    ! Retally emissions from non-covered sources
    offset(1:4, curiyr) = 0
    qoffdom = 0.
    qoffint = 0.
    do icat = 1, ghg_ncat
        icl = ghg_class(icat)
        if (icl == 5) then  ! comparable international allowances, unlimited
            call ghg_macs(icat, emtax(curiyr), curcalyr, q)
            ghg_abate(curiyr, icat) = q
            offset(icl - 1, curiyr) = offset(icl - 1, curiyr) + ghg_abate(curiyr, icat)
        elseif (icl > 1) then  ! domestic and international offsets

            if (offseti == 0 .or. (offseti == 1 .and. icl /= 4)) then  ! leave int'l out if offseti = 1

                ! Sum uncovered MAC and offsets
                call ghg_macs(icat, ghg_offsetp(curiyr), curcalyr, q)
                ghg_abate(curiyr, icat) = q
                if (icl == 2) ghg_abate(curiyr, icat) = min(q ,ghg_bl(curiyr, icat))
                offset(icl - 1, curiyr) = offset(icl - 1, curiyr) + ghg_abate(curiyr, icat)
                ghg_oghg(curiyr, icat) = ghg_bl(curiyr, icat) - ghg_abate(curiyr, icat)
                if (icl /= 3 .or. (icl == 3 .and. bioseqok == 1)) then
                    qoffdom = qoffdom + ghg_abate(curiyr, icat)
                endif
            elseif (offseti == 1 .and. icl == 4) then  ! compute int'l at international offset price if separate limit on int'l offsets
                call ghg_macs(icat, ghg_offsetpint(curiyr), curcalyr, q)
                ghg_abate(curiyr, icat) = q
                offset(icl - 1, curiyr) = offset(icl - 1, curiyr) + ghg_abate(curiyr, icat)
                qoffint = qoffint + ghg_abate(curiyr, icat)
            endif
        elseif (icl == 0) then  ! not eligible for offsets and not counting abatement
            ghg_abate(curiyr, icat) = 0.
            ghg_oghg(curiyr, icat) = ghg_bl(curiyr, icat)
        endif
    enddo

    do ioff = 1, 4
        icl = min(ioff + 1, 4)
        p = ghg_offsetp(curiyr)  ! domestic offset price
        if (ioff == 3) p = ghg_offsetpint(curiyr)  ! international offset price
        if (ioff == 4) p = emtax(curiyr)  ! international allowance price same as domestic in HR2454
        if (ncrl == 0 ) then
            ghg_rev(icl, curiyr) = ghg_rev(icl, curiyr) + p * offset(ioff, curiyr)
        endif
    enddo
else
    offset(1:5, curiyr) = 0.

    ! If prior to compliance period, zero any abatement, since the offset market
    ! won't be activated until then.
    do icat = 1, ghg_ncat
        icl = ghg_class(icat)
        if (icl /= 1) then
            ghg_abate(curiyr, icat) = 0.
            ghg_oghg(curiyr, icat) = ghg_bl(curiyr, icat)
        endif
    enddo

endif

return
end subroutine oghg


!> Return the amount of abatement using Other GHG MACs.

!> Uses the Other GHG marginal abatement curves (MACs) to return the amount of
!> abatement in a given category at a given price in a given year. Assumes that
!> prices are in ascending order and interpolates between adjacent steps and
!> between adjacent intervals.
!> 
!> @param icat Index of category.
!> @param p Price given.
!> @param year The 4-digit year index.
!> @param q Returns the amount of abatement.

subroutine ghg_macs(icat, p, year, q)

implicit none

include 'parametr'
include 'ncntrl'
include 'ghgrep'

integer icat, year
real p, q
intent(in) :: icat, p, year
intent(out) :: q

integer iyl, iyh  ! year interval, 1-6
integer nsteps  ! number of steps
integer istep  ! step number
logical found  ! true when interval found
real p1, p2, q1, q2
real frac, fraciyl
real err_count(ghg_nmax)  ! error count, by max # of categories.
integer iunit1, bank_onyr
common /epm_out/ iunit1, bank_onyr

q = 0.
if (icat <= 0 .or. icat > ghg_nmax) return
if (p <= 0.) return

! Get the lower and upper curve number or index based on year, along with the
! interpolation fraction for going between the 5 year intervals.
call iyear5(year, iyl, iyh, fraciyl)

if (iyl == 0) return

nsteps = ghg_steps(icat)
if (nsteps == 0) return

! See if we max-out on the last step. If price is higher, we can quit early
if (p >= ghg_macp(nsteps, icat)) then
    
    ! fraciyl is interpolation fraction for going between 5 year intervals.
    q = ghg_macq(nsteps, iyl, icat) * fraciyl &
        + ghg_macq(nsteps, iyh, icat) * (1. - fraciyl)
    return
endif

! Find interval surrounding the input price.
istep = 1
found = .false.

if (p < ghg_macp(istep, icat)) then

    ! Price is less than first step price
    found = .true.

    ! Interpolate between curves (for q) and between 0 and first price
    q1 = 0.
    q2 = ghg_macq(istep, iyl, icat) * fraciyl &
         + ghg_macq(istep, iyh, icat) * (1. - fraciyl)
    p1 = 0.
    p2 = ghg_macp(istep, icat)
    frac = 0.
    if ((p2 - p1) /= 0.) then
        frac = (p - p1) / (p2 - p1)
    endif
    q = q1 * (1. - frac) + q2 * frac
endif

do while (.not. found .and. istep < nsteps)
    if (p >= ghg_macp(istep, icat) .and. p < ghg_macp(istep + 1, icat)) then
        found = .true.

        ! Now interpolate between curves (for q) and between price steps
        q1 = ghg_macq(istep, iyl, icat) * fraciyl &
             + ghg_macq(istep, iyh, icat) * (1. - fraciyl)
        q2 = ghg_macq(istep + 1, iyl, icat) * fraciyl &
             + ghg_macq(istep + 1, iyh, icat) * (1. - fraciyl)
        p1 = ghg_macp(istep, icat)
        p2 = ghg_macp(istep + 1, icat)
        frac = 0.
        if ((p2 - p1) /= 0.) then
            frac = (p - p1) / (p2 - p1)
        endif
        q = q1 * (1. - frac) + q2 * frac
    endif
    istep = istep + 1
enddo

if (.not. found) then
    err_count(icat) = err_count(icat) + 1.
    if (err_count(icat) < 2.) then
        write(iunit1, '(a,i2,a,i4,a,2i2)') 'Error in Abatement curve # ', icat, &
                                           ' year=', year, ' intervals=', iyl, iyh
        write(iunit1, '(a,i3)') 'Abatement curve for category ', icat
        write(iunit1, '(a,f12.5)') 'fraciyl=', fraciyl
        write(iunit1, '(a,f12.5)') ' looking for price p=', p
        do istep = 1, nsteps
            write(iunit1, '(i2,3f12.5)') istep, ghg_macp(istep, icat), &
                                         ghg_macq(istep, iyl, icat), &
                                         ghg_macq(istep,iyh,icat)
        enddo
    endif
endif

if (q < 0.) q = 0.

return
end subroutine ghg_macs


!> Return interval numbers for MACs for a given year.

!> Lookup utility called by ghg_macs to retrieve the interval numbers 1 to 5
!> for MACs for each year.
!> 
!> @param year Desired year.
!> @param iyl Gets the lower interval.
!> @param iyh Gets the higher interval.
!> @param fraciyl Gets the interpolation fraction (weight on the lower side).

!> @see ghg_macs

subroutine iyear5(year, iyl, iyh, fraciyl)

implicit none

include 'parametr'
include 'ncntrl'

integer year
integer iyl, iyh
real fraciyl
intent(in) :: year
intent(out) :: iyl, iyh
intent(out) :: fraciyl

integer lookupl(2004:(mnumyr+1989)) /1, 1, 1, 1, 1, 1, 2, &
                                     2, 2, 2, 2, 3, 3, 3, 3, 3, 4, &
                                     4, 4, 4, 4, 5, 5, 5, 5, 5, 6, &
                                     6, 6, 6, 6, 7, 7, 7, 7, 7, 8, &
                                     8, 8, 8, 8, 9, 9, 9, 9, 9, 10/

integer lookuph(2004:(mnumyr+1989)) /1, 1, 2, 2, 2, 2, 2, &
                                     3, 3, 3, 3, 3, 4, 4, 4, 4, 4, &
                                     5, 5, 5, 5, 5, 6, 6, 6, 6, 6, &
                                     7, 7, 7, 7, 7, 8, 8, 8, 8, 8, &
                                     9, 9, 9, 9, 9, 10, 10, 10, 10, 10/

real lookfracl(2004:(mnumyr+1989)) /1., 1., .8, .6, .4, .2, 1., &
                                    .8, .6, .4, .2, 1., .8, .6, .4, .2, 1., &
                                    .8, .6, .4, .2, 1., .8, .6, .4, .2, 1., &
                                    .8, .6, .4, .2, 1., .8, .6, .4, .2, 1., &
                                    .8, .6, .4, .2, 1., .8, .6, .4, .2, 1./

iyl = 0
iyh = 0
fraciyl = 1.
if (year < 2004 .or. year > ijumpcalyr) return
iyl = lookupl(year)
iyh = lookuph(year)
fraciyl = lookfracl(year)

return
end subroutine iyear5


!> Read in GHG offsets from the appropriate input worksheet.

!> Called by oghg subroutine on first iteration of first year to read in
!> greenhouse gas offsets from "offsets" worksheet in GHGOFFX file.

!> @see oghg

subroutine readoffsets

implicit none

include 'parametr'
include 'ncntrl'
include 'ghgrep'
include 'macout'  ! need for deflator mc_jpgdp

real dollarmac
logical new
character*18 fname
integer file_mgr
external file_mgr
character*16 rname
integer wkunit, sysunit
real ghgbl(8, ghg_nmax), pmax
integer i, j, nmax

new = .false.
fname = 'GHGOFFX'
wkunit = file_mgr('O', fname, new)

!  Call subroutine to read all defined ranges from worksheet "offsets"
!  This stores the ranges in a temporary data area
call readrngxlsx(wkunit, 'offsets')
close(wkunit)

! Copy each range from worksheet data area to variables
! getrngi: Copies an Integer*2 variable from the worksheet
!          data area into the variable. The variable
!          dimensions are passed as the 3rd, 4th, & 5th
!          arguments, (eg, ... 1,1,1).
!          A variable with dimesions of 1, 1, 1 is a scalar.
!          A variable with dimensions of 26, 1, 1 is a one-
!          dimensional array with 26 elements.
! getrngr: Copies a REAL variable from the worksheet
!          data area into the variable.
! getrngc: Copies a character variable
ghg_ncat = 17  ! based on spreadsheet number of CL_ ranges. last two Cats don't have Baselines
ghg_bl(:, 1:5) = 0.
ghg_bl(1:, 7:) = 0.

if (exi == 1) then
    ghg_bl(1:, 6) = 0.
endif

call getrngr('BL_CH4LAND      ', ghg_bl(1, 1), 1, mnumyr, 1)
call getrngr('BL_CH4COAL      ', ghg_bl(1, 2), 1, mnumyr, 1)
call getrngr('BL_CH4NG        ', ghg_bl(1, 3), 1, mnumyr, 1)
call getrngr('BL_CH4COMB      ', ghg_bl(1, 4), 1, mnumyr, 1)
call getrngr('BL_CH4OTH       ', ghg_bl(1, 5), 1, mnumyr, 1)
if (exi == 1) &  ! take from input restart file if industrial not on
    call getrngr('BL_NECO2        ', ghg_bl(1, 6), 1, mnumyr, 1)
call getrngr('BL_N2OAG        ', ghg_bl(1, 7), 1, mnumyr, 1)
call getrngr('BL_N2OMOB       ', ghg_bl(1, 8), 1, mnumyr, 1)
call getrngr('BL_N2OACID      ', ghg_bl(1, 9), 1, mnumyr, 1)
call getrngr('BL_N2OOTH       ', ghg_bl(1, 10), 1, mnumyr, 1)
call getrngr('BL_FGASHFC23    ', ghg_bl(1, 11), 1, mnumyr, 1)
call getrngr('BL_FGASHFC      ', ghg_bl(1, 12), 1, mnumyr, 1)
call getrngr('BL_FGASPFC      ', ghg_bl(1, 13), 1, mnumyr, 1)
call getrngr('BL_FGASSF6      ', ghg_bl(1, 14), 1, mnumyr, 1)
if (exi == 1) &
    ghg_bl(:, 6:6) = ghg_bl(:, 6:6) * 12./44.  ! convert from mmtCO2eq to mmtCe
ghg_bl(:, 1:5) = ghg_bl(:, 1:5) * 12./44.  ! convert from mmtCO2eq to mmtCe
ghg_bl(:, 7:) = ghg_bl(:, 7:) * 12./44.  ! convert from mmtCO2eq to mmtCe
call getrngi('CL_CH4LAND      ', ghg_class(1), 1, 1, 1)
call getrngi('CL_CH4COAL      ', ghg_class(2), 1, 1, 1)
call getrngi('CL_CH4NG        ', ghg_class(3), 1, 1, 1)
call getrngi('CL_CH4COMB      ', ghg_class(4), 1, 1, 1)
call getrngi('CL_CH4OTH       ', ghg_class(5), 1, 1, 1)
call getrngi('CL_NECO2        ', ghg_class(6), 1, 1, 1)
call getrngi('CL_N2OAG        ', ghg_class(7), 1, 1, 1)
call getrngi('CL_N2OMOB       ', ghg_class(8), 1, 1, 1)
call getrngi('CL_N2OACID      ', ghg_class(9), 1, 1, 1)
call getrngi('CL_N2OOTH       ', ghg_class(10), 1, 1, 1)
call getrngi('CL_FGASHFC23    ', ghg_class(11), 1, 1, 1)
call getrngi('CL_FGASHFC      ', ghg_class(12), 1, 1, 1)
call getrngi('CL_FGASPFC      ', ghg_class(13), 1, 1, 1)
call getrngi('CL_FGASSF6      ', ghg_class(14), 1, 1, 1)
call getrngi('CL_SEQUS        ', ghg_class(15), 1, 1, 1)
call getrngi('CL_INTOFF       ', ghg_class(16), 1, 1, 1)
call getrngi('CL_INTCO2       ', ghg_class(17), 1, 1, 1)
call getrngi('MAC_PRICEYR     ', mac_priceyr, 1, 1, 1)

ghg_macp(:, :) = 0.
call getrngr('MACP_CH4LAND    ', ghg_macp(1, 1), ghg_stmax, 1, 1)  ! landfill
call getrngr('MACP_CH4COAL    ', ghg_macp(1, 2), ghg_stmax, 1, 1)  ! coal mining
call getrngr('MACP_CH4NG      ', ghg_macp(1, 3), ghg_stmax, 1, 1)  ! natural gas/oil systems
call getrngr('MACP_CH4COMB    ', ghg_macp(1, 4), ghg_stmax, 1, 1)  ! combustion placeholder
call getrngr('MACP_CH4OTH     ', ghg_macp(1, 5), ghg_stmax, 1, 1)  ! agriculture
call getrngr('MACP_N2OAG      ', ghg_macp(1, 7), ghg_stmax, 1, 1)  ! agriculture
call getrngr('MACP_N2OCOMB    ', ghg_macp(1, 8), ghg_stmax, 1, 1)  ! combustion placeholder
call getrngr('MACP_N2OACID    ', ghg_macp(1, 9), ghg_stmax, 1, 1)  ! acids
call getrngr('MACP_N2OOTH     ', ghg_macp(1, 10), ghg_stmax, 1, 1)  ! other placeholder
call getrngr('MACP_FGASHFC23  ', ghg_macp(1, 11), ghg_stmax, 1, 1)
call getrngr('MACP_FGASHFC    ', ghg_macp(1, 12), ghg_stmax, 1, 1)
call getrngr('MACP_FGASPFC    ', ghg_macp(1, 13), ghg_stmax, 1, 1)
call getrngr('MACP_FGASSF6    ', ghg_macp(1, 14), ghg_stmax, 1, 1)
call getrngr('MACP_SEQUS      ', ghg_macp(1, 15), ghg_stmax, 1, 1)
call getrngr('MACP_INTOFF     ', ghg_macp(1, 16), ghg_stmax, 1, 1)  ! international offsets
call getrngr('MACP_INTCO2     ', ghg_macp(1, 17), ghg_stmax, 1, 1)  ! international allowances

! Count the steps and convert to EPM price units (1987$/kg)
ghg_steps(:) = 0
dollarmac = mc_jpgdp(mac_priceyr - 1989)
do i = 1, ghg_nmax
    pmax = 0.
    nmax = 0
    do j = 1, ghg_stmax
        ghg_macp(j, i) = (ghg_macp(j, i) / dollarmac) * .001
        if (ghg_macp(j, i) > pmax) then
            pmax = ghg_macp(j, i)
            nmax = j
        endif
    enddo
    ghg_steps(i) = nmax
enddo

! Read the quantities available at each price--specified by five 5-year intervals, 2005-2050
ghg_macq(:, :, :) = 0.
call getrngr('MACQ_CH4LAND    ', ghg_macq(1, 1, 1), ghg_stmax, 10, 1) 
call getrngr('MACQ_CH4COAL    ', ghg_macq(1, 1, 2), ghg_stmax, 10, 1)
call getrngr('MACQ_CH4NG      ', ghg_macq(1, 1, 3), ghg_stmax, 10, 1)
call getrngr('MACQ_CH4COMB    ', ghg_macq(1, 1, 4), ghg_stmax, 10, 1)
call getrngr('MACQ_CH4OTH     ', ghg_macq(1, 1, 5), ghg_stmax, 10, 1)
call getrngr('MACQ_N2OAG      ', ghg_macq(1, 1, 7), ghg_stmax, 10, 1)
call getrngr('MACQ_N2OCOMB    ', ghg_macq(1, 1, 8), ghg_stmax, 10, 1)
call getrngr('MACQ_N2OACID    ', ghg_macq(1, 1, 9), ghg_stmax, 10, 1) 
call getrngr('MACQ_N2OOTH     ', ghg_macq(1, 1, 10), ghg_stmax, 10, 1) 
call getrngr('MACQ_FGASHFC23  ', ghg_macq(1, 1, 11), ghg_stmax, 10, 1)
call getrngr('MACQ_FGASHFC    ', ghg_macq(1, 1, 12), ghg_stmax, 10, 1)
call getrngr('MACQ_FGASPFC    ', ghg_macq(1, 1, 13), ghg_stmax, 10, 1)
call getrngr('MACQ_FGASSF6    ', ghg_macq(1, 1, 14), ghg_stmax, 10, 1)
call getrngr('MACQ_SEQUS      ', ghg_macq(1, 1, 15), ghg_stmax, 10, 1)
call getrngr('MACQ_INTOFF     ', ghg_macq(1, 1, 16), ghg_stmax, 10, 1)  ! international offsets
call getrngr('MACQ_INTCO2     ', ghg_macq(1, 1, 17), ghg_stmax, 10, 1)  ! international allowances

return
end subroutine readoffsets


!> Interpolates between a 5 year variable and an annual variable.

!> Given an array of points with the values of a variable at five year
!> intervals, fill the output array with the same variable linearly intepolated
!> down to one-year intervals.
!> 
!> @param r5 Five year interval variable. One-dimensional array of length n5.
!> @param n5 Length of r5 array.
!> @param annual Annual variable with same endpoints as r5. One-dimensional
!> array of length na.
!> @param na Length of annual array. Note: na MUST be equal to 5 * n5 - 4, or
!> else epm_interp will print an error and immediately return.

subroutine epm_interp(r5, n5, annual, na)

implicit none

integer n5, na
real r5(n5)
real annual(na)
intent(in) :: n5, na
intent(in) :: r5
intent(out) :: annual

integer i, j, js, je
real increase

if (((n5 * 5) - 4) /= na) then
    write(6, *) 'bad array dimensions passed to subr interp.'
    write(6, *) 'n5, na=', n5, na
    write(6, *) 'given n5, na should be ', n5 * 5 - 4
    return
endif

do i = 1, n5 - 1
    js = 5 * (i - 1) + 1
    je = 5 * (i) + 1
    increase = (r5(i + 1) - r5(i)) / 5.
    annual(js) = r5(i)
    do j = js + 1, je
        annual(j) = annual(j - 1) + increase
    enddo
enddo

return
end subroutine epm_interp
