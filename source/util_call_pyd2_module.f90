module emm_run
    contains
    subroutine ephrts_on_off(IS_TURN_ON,IS_TURN_ON_DEBUGGING)
	    USE EPHRTS_SWTICHES
		INTEGER IS_TURN_ON,IS_TURN_ON_DEBUGGING
		
		EPHRTS=IS_TURN_ON
		if (IS_TURN_ON .eq. 0) then
            WRITE(*,*) "THE ELECTRICITY H2 SUBMODULE IS TURNED OFF, EPHRTS = ", IS_TURN_ON
		else
            WRITE(*,*) "THE ELECTRICITY H2 SUBMODULE IS TURNED ON, EPHRTS = ", IS_TURN_ON
		endif
		if (IS_TURN_ON_DEBUGGING .eq. 0) then
			TURN_ON_DEBUGGING=.FALSE.
		else
			TURN_ON_DEBUGGING=.TRUE.
		endif

		call UTIL
		
		CLOSE(10)
    end subroutine
	
    subroutine close_aimms_efd
    call AIMMS_EFD('end')
	CLOSE(10)
    end subroutine
	
    subroutine close_aimms_ecp
    call AIMMS_ECP('end')
	CLOSE(10)
    end subroutine
	
end module emm_run

module renew_run
    contains
    subroutine go_renew
    call RENEW
	CLOSE(10)
    end subroutine
end module renew_run

! module epm_run
    ! contains
    ! subroutine go_epm
        ! call EPM
    ! end subroutine

    ! subroutine read_epm
        ! call EPM_READ
    ! end subroutine

! end module epm_run

! module cvtest_run
    ! contains
    ! subroutine go_cvtest(IMODEL)
    ! INTEGER     IMODEL
    ! call CVTEST(IMODEL)
    ! CLOSE(10)
    ! end subroutine go_cvtest
! end module cvtest_run

! module relax_run
    ! contains
    ! subroutine go_relax
    ! call RELAX
    ! CLOSE(10)
    ! end subroutine go_relax
! end module relax_run
module ngpl_stat_price_run
    contains
    subroutine go_ngpl_stat_price
    CALL NGPL_STAT_PRICE
    CLOSE(10)
    end subroutine go_ngpl_stat_price

    subroutine go_ngpl_stat_price_reporting
    CALL NGPL_STAT_PRICE_REPORTING
    CLOSE(10)
    end subroutine go_ngpl_stat_price_reporting
end module ngpl_stat_price_run