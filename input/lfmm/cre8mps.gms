*****************************************************************************
** Create MPS file with non-meaningful column names, row names
**
** Author: Mike Cole, michael.cole@eia.gov
** Version/Date: 1.2 (2/28/2011)
**
** arguments:
**    1. GAMS model name (this automatically replaces the "%1" below)
***   2. Objective function sense: minimizing or maximizing
**    3. objective function name
**
** How to call:
**  Place the following line at the end of your GAMS code
**       $BATINCLUDE CreateMPS.gms "XYZ" "sense" "OBJ" ;
**  where XYZ is the name of the model (e.g., LFMM)
**  and "sense" is either "minimizing" or "maximizing"
**  and OBJ is the objective function row name
**
** Note that CreateMPS.gms is NOT standalone code.
**
*****************************************************************************

**Instruct GAMS to search for an optfile.
%1.optfile=1

$onecho > convert.opt
Dict %1dict.txt
FixedMPS %1fixed.mps
*Gams
$offecho

option lp=convert ;
Solve %1 %2 %3 using lp ;


