*display NCNTRL_CURITR;
display currentYr;
display NCNTRL_CURCALYR;
display NCNTRL_CURITR_orig;

$BATINCLUDE Cre8MPS.gms "LFMM" "minimizing" "TotalCost" ;

*execute 'gamsmps.sh lfmmfixed.mps lfmmdict.txt lfmmmc6.mps'


file MPSDBG /MPSdebug.txt/;
put MPSDBG 'NCNTRL_CURCALYR: ' NCNTRL_CURCALYR /;
put MPSDBG 'NCNTRL_CURITR_orig: ' NCNTRL_CURITR_orig;













