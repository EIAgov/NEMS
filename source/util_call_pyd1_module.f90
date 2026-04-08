module mac_run
    contains
    subroutine go_mac
    call MAC
    CLOSE(10)
    end subroutine go_mac
end module mac_run

module cmm_run
	contains
	subroutine coal_expect_go
	call COAL_EXPECT
	CLOSE(10)
	end subroutine coal_expect_go
end module cmm_run

module rdm_run
    contains
    subroutine go_rdm
    call RESD
    CLOSE(10)
    end subroutine go_rdm
end module rdm_run

module cdm_run
    contains
    subroutine go_cdm
    call COMM
    CLOSE(10)
    end subroutine go_cdm
end module cdm_run

module ind_run
    contains
    subroutine go_ind
    call IND
    CLOSE(10)
    end subroutine go_ind
end module ind_run

module tran_run
    contains
    subroutine go_tran
    call TRAN
    CLOSE(10)
    end subroutine go_tran
end module tran_run

module world_run
    contains
    subroutine go_world
    call WORLD
    CLOSE(10)
    end subroutine go_world
end module world_run

module refine_run
    contains
    subroutine GO_NEXEC_CALL_REFINE
    call NEXEC_CALL_REFINE
    CLOSE(10)
    end subroutine GO_NEXEC_CALL_REFINE
    
    subroutine go_refine
    call REFINE
    CLOSE(10)
    end subroutine go_refine
end module refine_run



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

! ----------------------------
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
	
end module emm_run

module renew_run
    contains
    subroutine go_renew
    call RENEW
	CLOSE(10)
    end subroutine
end module renew_run
