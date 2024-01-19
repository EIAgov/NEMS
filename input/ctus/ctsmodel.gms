$Title  CTS pipeline routing model
$Ontext

*s=CTStmp gdx=ctusAll.gdx --dir="./" --LPsolver="Xpress" --ForceAllOn=-1 --InNEMS=1

$set environ LO=%gams.lo% IDE=%gams.ide%
$set  LPsolver     %LPsolver%
$Offtext

* Allows a set to be empty
$onempty

set numbers / 1*2050 /;

Set  AllNode                         Plant + transship + Inj
     FromNode(AllNode)               Plant + transship
     ToNode(AllNode)                 transship + Inj
     FromTo                          Valid From-To links without diameter domain
     th                              t master plus historical
     tm(th)                          time master set
     tf(tm)                          t financing - all cash flow years
     t_emm(tf)                       t for emm output
     t(t_emm)                           time horizon of the model
     tn(t)                           time periods
     tstart(t)                       time period start
     tend(t)                         time period end
     plant(FromNode)                 capture plant
     CapPower(plant)                 Existing power plants + new power plants (EPlant+NPlant)
     source                          capture sites - units
     R_IGRP(source)                  units to be retired based on ECP solution
     E_IGRP(source)                  ECP existing coal units
     EG_IGRP(source)                 ECP existing gas units
     N_IGRP(source)                  new units built
     PS_IGRP(N_IGRP)                 Pseudo sources to be used if ForceAll is on so that every fuel region has at least one source
     Nplant(FromNode)                new power plants built
     unitplt(source,plant)           unit-plant mapping
     INJ_TERMINALS(ToNode)           Injection terminals
     Inj                             Injection (Storage) sites
     InjSal(Inj)                     Saline Injection sites
     InjEOR(Inj)                     EOR Injection sites
     InjPseudo(InjEOR)               EOR Pseudo-injection sites for NEMS integration
     INJ_TDIST(Inj,INJ_TERMINALS)    Terminal to injection site mapping
     transShip(ToNode)               transhipment points
     diameter                        pipe diameter
     link(FromNode,ToNode,diameter)  possible pipeline links
     OnePipeNodes(FromNode)          Incidence of nodes where only one outgoing pipe is allowed
     Region_EMM                      EMM regions
     INJ_EMM(Inj,REGION_EMM)         Map EOR sites to EMM regions
     DRILL_REG                       drilling regions (aggregated EMM regions)
     source_EMM(source,Region_EMM)   map All Sources to EMM Region
     SourceType                      CO2 source types
     PlantType                       Categories of CCS capture sources
     sink_att
     CTUS
;

$gdxin %dir%CTUSModIN.gdx
$Load th, tm, tf, t_emm, t, tstart, tend, AllNode, FromNode, ToNode, Plant, source, OnePipeNodes, Inj, transShip, diameter, link, unitplt, N_IGRP, PS_IGRP, Nplant
$Load InjEOR, InjPseudo, InjSal, Region_EMM, INJ_EMM, SourceType, source_EMM
$Load INJ_TERMINALS, INJ_TDIST, CTUS
$Load CapPower, PlantType, sink_att
$Load tn = t

* tp represents project years
Alias (t,tp) ;
Alias (ToNode,ToNode2);


Set     UnitType(source,SourceType)     source-type mapping ;
$Load UnitType


*Valid From-To links without diameter domain
FromTo(FromNode,ToNode) = sum(link(FromNode,ToNode,diameter),1);

Parameters

SrcCapCost(source,t)                      cost (millions of dollars) to retrofit a SOURCE
SinkCapCost(Inj,t)                        cost (millions of dollars) to build a new SINK
PipeCapCost(FromNode,ToNode,diameter,t)   cost (millions of dollars) to build pipe connecting each SOURCE to SINK n time t
CarbonOffset(t)                           Variable offset per tonne (dollars)on CO2 credited when injected
N_RY(N_IGRP)                              New plant go-live year
E_RY(E_IGRP)                              ECP existing coal unit retrofit year
EG_RY(EG_IGRP)                            ECP existing gas unit retrofit year
R_RY(R_IGRP)                              ECP existing unit retirement year

TransportCost(FromNode,ToNode,diameter,t) cost per metric-ton to ship from each SOURCE to SINK
InjectCost(Inj,t)                         cost per tonne to inject at each SINK
CaptureCost(source,t)                     cost per tonne to capture at each SOURCE
WellCapCost(Inj,t)                        cost (millions of dollars) to build a well at each SINK

CaptureMax(source,t)                      maximum (million tonnes) captured annually at SOURCE
PipeMax(diameter)                         maximum (million tonnes) shipped annually for each pipeline
WellMax(Inj)                              maximum (million tonnes) injected annually at a well for each SINK

EOR_CO2Purch(Inj,t,tp)                    CO2 purchased (million tonnes)in project year tp if project started in year t

InjectCap(Inj)                            total capacity in million tonnes at each SINK
WellDepth(Inj)                            Well Depth in Meters

RetroLT(source)                           Leadtime in time periods for Retrofit build
NewSinkLT(Inj)                            Leadtime in time periods for New Sink build
PipeLT(AllNode,AllNode,diameter)          Leadtime in time periods for Pipeline build
PV(CTUS,t)                                Present Value

LinkDist(FromNode,ToNode)                 Pipeline link distances in miles

MaxWells(InjSal)                          Max number of saline wells per site per year

MinWells(InjSal)                          Minimum number of saline wells per new injection site

SlackPenalty(t)                           Penalty for venting CO2 for power plants

SinkProps(Inj, sink_att)
;

$LOAD SrcCapCost, CaptureCost, CaptureMax, CarbonOffset, N_RY,PV
$LOAD SinkCapCost, InjectCost, InjectCap, EOR_CO2Purch, WellMax, WellCapCost, WellDepth
$LOAD PipeCapCost,  TransportCost, PipeMax, SinkProps
$load LinkDist
$gdxin

$gdxin %dir%/CTSSavR.gdx
$load R_IGRP, E_IGRP, E_RY, EG_IGRP, EG_RY, R_RY

scalar TrunkPenalty      Penalty for using transshipment nodes if ForceAll is on ;
TrunkPenalty = 0 ;

RetroLT(source) = 2;
NewSinkLT(InjSal) = 2;
PipeLT(AllNode,AllNode,diameter) = 2;

Variables CAPTURED(source,t)                 million tonnes of CO2 captured at each source in time t
          INJECTED(Inj,t)                    million tonnes of CO2 stored at each sink in time t
          PIPED(FromNode,ToNode,diameter,t)  million tonnes of CO2 piped from each source to sink in time t

          NEWSINK(InjSal,t)                   1 if  SINK is a new build in time t available after leadtime - Saline
          NEWSOURCE(source,t)                 1 if source is retrofitted in time t available after leadtime
          BLDPIPE(FromNode,ToNode,diameter,t) 1 if PIPE ij is built in time t available after leadtime

          EOR_ON(InjEOR,t)                    1 if EOR is initiated at SINK in time t                      - EOR

          WELLNUM(InjSal,t)                   number of wells added at each sink in each time t            - Saline

          TOTCaptureCost(t)        Objective function - Capture cost term
          TOTTransCost(t)          Objective function - Transportation cost term
          TOTStoreCost(t)          Objective function - Injection Storage cost term
          TOTCapitalCost(t)        Objective function - All construction costs (retro pipe sink)
          TOTOffset(t)             Objective function - Carbon Offset
          TotalCost                Total cost - objective function definition

          SinkReady(Inj,t)                       Set to 1 if sink has been built previously
          PipeReady(FromNode,ToNode,diameter,t)  Set to 1 if pipe has been built previously
          SourceReady(source,t)                  Set to 1 if source has been retrofitted previously

          CaptureSlack(source,t)         Amount of carbon a source chooses not to capture

* Infeasibility variables
          zpipe(AllNode,AllNode,diameter,t)
          zInj(Inj,t)
          zCap(source,t)
 ;

Positive Variable CAPTURED, INJECTED, PIPED;
Positive Variable zpipe, zCap, zInj ;
Positive Variable SinkReady, SourceReady, PipeReady, CaptureSlack ;
*Binary Variable BLDPIPE, EOR_ON, NEWSOURCE ;
Binary Variable EOR_ON, NEWSOURCE ;
Integer Variable BLDPIPE, NEWSINK, WELLNUM ;

parameter
  totCaptureMax(t) Total possible capture by year based on active units
  maxPipes         Maximum number of 36" pipes between any two points you could need based on max capture
  EORSafetyPrice   Safety valve price for falling short of EOR CO2 demand
;
totCaptureMax(t) =
  sum(E_IGRP$(t.val>=E_RY(E_IGRP)), CaptureMax(E_IGRP,t)) +
*Changed following line
  sum(EG_IGRP$(t.val>=EG_RY(EG_IGRP)), CaptureMax(EG_IGRP,t)) +
  sum(N_IGRP$(t.val>=N_RY(N_IGRP)), CaptureMax(N_IGRP,t)) ;

maxPipes = min(5,max(1, ceil(smax(t,totCaptureMax(t))/PipeMax('36')))) ;

EORSafetyPrice = smax((link,t),PipeCapCost(link,t)) * 2 ;
BLDPIPE.up(Link(FromNode,ToNode,diameter),t) = 1 ;
BLDPIPE.up(Link(FromNode,ToNode,'36'),t)     = maxPipes ;

*Turn off infeasiblity variables by fixing them to 0
zpipe.fx(link,t)   = 0;
*zInj.fx(InjSal,t)  = 0;
zCap.fx(source,t)  = 0;

* Constrain the number of saline wells per year per site
MaxWells(InjSal)  = ROUND(InjectCap(InjSal)/WellMax(InjSal),0) ;
WELLNUM.up(InjSal,t) = min(99,MaxWells(InjSal)) ;

* At least 4 wells must be drilled when activating a saline injection site
MinWells(InjSal) = 4 ;

* Upper Bounds
CAPTURED.up(source,t)                      $= CaptureMax(source,t);
PIPED.up(link(FromNode,ToNode,diameter),t) $= PipeMax(diameter);
PIPED.up(link(FromNode,ToNode,'36'),t)     $= PipeMax('36')*maxPipes ;
SourceReady.up(source,t) = 1.0;
PipeReady.up(link(FromNode,ToNode,diameter),t) = 1 ;
PipeReady.up(link(FromNode,ToNode,'36'),t)     = maxPipes ;

NEWSINK.up(InjSal,t) = max(1,ROUND(99*WellMax(InjSal)/SinkProps(InjSal,'SiteMax_MMT'),0)) ;

SlackPenalty(t) = CarbonOffset(t) ;

* Turn off pseudo EOR sites if not a NEMS-integrated run
EOR_ON.fx(InjPseudo,t) = 0 ;

****  Read in user-defined parameter settings
$gdxin %dir%RunDefn.gdx
Parameter  RunDefn  Settings for this run - set to -1(Off) or 1(On)in CTS_script.gms ;
$load RunDefn

*** TRUNK LINES ---- Trunk lines are disabled when set to -1
If( RunDefn('Trunks')= -1,
   BLDPIPE.fx(Link(FromNode,ToNode,diameter),t)$transShip(ToNode) = 0 ;
);

* Turn off unplanned builds when not integrated with NEMS or if ForceAll is not on
NEWSOURCE.fx(N_IGRP,t)                      = 0;

*** FORCED CAPTURE AT ALL SITES ---- When set to 1, force all sources to retrofit
$Ifi %ForceAllOn% == -1 $goto next3
* Turn on all existing power plants and turn off all other capture sources
*   NEWSOURCE.fx(source,'2012')$UnitType(source,'GEN') = 1 ;
*   NEWSOURCE.fx(N_IGRP,t)           = 0 ;

   NEWSOURCE.fx(source,t)                               = 0 ;
   NEWSOURCE.fx(PS_IGRP,tstart)$UnitType(PS_IGRP,'GEN') = 1 ;

* Turn off EOR
   EOR_ON.fx(InjEOR,t)    = 0 ;

$Ifi %ForceAllOn% == 1 $goto skipSalOGSM
* Turn off saline sites that are not under an OGSM terminal node if this is an integrated ForceAll case
   INJECTED.fx(InjSal,t)$( sum((InjPseudo,INJ_TERMINALS)$(INJ_TDIST(InjPseudo,INJ_TERMINALS) and INJ_TDIST(InjSal,INJ_TERMINALS)), 1)=0 ) = 0 ;
$label skipSalOGSM

* Force plants to capture all of their carbon
   CaptureSlack.fx(source,t)        = 0 ;

* Assume all pipes must be built in first year in ForceAll case
   BLDPIPE.fx(Link(FromNode,ToNode,diameter),t) = 0 ;
   BLDPIPE.up(Link(FromNode,ToNode,diameter),tstart) = 1 ;

* Other tightening constraints
   BLDPIPE.fx(Link(plant,ToNode,diameter),t)$(not Nplant(plant)) = 0 ;
   BLDPIPE.fx(Link(plant,ToNode,diameter),t)$(sum(PS_IGRP$(unitplt(PS_IGRP,plant) and N_RY(PS_IGRP)>0), 1)=0) = 0 ;

* Apply a per-ton penalty for any transshipment pipes used
   TrunkPenalty                     = 500 ;

$label next3

*** Fixed variables based on NEMS values if this is an integrated run
$Ifi not %InNEMS% == 1 $goto next4
   EOR_ON.fx(InjEOR,t)         = 0 ;
   EOR_ON.fx(InjPseudo,tstart) = 1 ;
   NEWSOURCE.fx(source,t)      = 0 ;

* Turn off saline and allow venting if there is no carbon price
   NEWSINK.fx(InjSal,t)$(CarbonOffset(t)=0) = 0 ;
   WELLNUM.fx(InjSal,t)$(CarbonOffset(t)=0) = 0 ;
   SlackPenalty(t) = CarbonOffset(t) ;

*** ECP RETROFITS ---- When set to 1, force retrofit according to ECP solution
   If( RunDefn('ECP_Retrofits')= 1,
      NEWSOURCE.fx(E_IGRP,t)$(t.val=E_RY(E_IGRP))    = 1 ;
      NEWSOURCE.fx(EG_IGRP,t)$(t.val=EG_RY(EG_IGRP)) = 1 ;
      BLDPIPE.fx(link(plant,ToNode,diameter),t) = 0 ;
      BLDPIPE.up(link(plant,ToNode,diameter),t)$(sum(E_IGRP$(unitplt(E_IGRP,plant) and (t.val>=E_RY(E_IGRP))), 1)>0) = 1 ;
      BLDPIPE.up(link(plant,ToNode,diameter),t)$(sum(EG_IGRP$(unitplt(EG_IGRP,plant) and (t.val>=EG_RY(EG_IGRP))), 1)>0) = 1 ;
   );

*** RETIREMENTS ---- When set to 1, don't retrofit any units slated for retirements according to ECP solution
   If( RunDefn('ECP_Retirements')= 1,
      CaptureMax(R_IGRP,t)$(t.val>=R_RY(R_IGRP)) = 0 ;
   );

*** NEW BUILDS ---- When set to 1, force new builds on in the allocated year; otherwise, ignore new builds
   If( RunDefn('ECP_NewBuilds')= 1,
     NEWSOURCE.fx(N_IGRP,t)$(t.val=N_RY(N_IGRP)) = 1;
     BLDPIPE.up(link(plant,ToNode,diameter),t)$(sum(N_IGRP$(unitplt(N_IGRP,plant) and (t.val>=N_RY(N_IGRP))), 1)>0) = 1 ;
   );
$label next4

*** ZERO LEAD TIMES ---- When set to 1, ignore lead-times
If( RunDefn('ZeroLT')= 1,
      PipeLT(AllNode,AllNode,diameter) = 0 ;
      RetroLT(source) = 0  ;
      NewSinkLT(InjSal) = 0  ;
);

*** *********************************************************************************

 Equations       SourceBalance(plant,t)               Capture Balance
                 RetrofitBalance(source,t)            Incur retrofit cost at SOURCE
                 SinkBalance(INJ_TERMINALS,t)          Storage Balance
                 NewBldSinkBalance(Inj,t)             Incur new SINK build cost
                 NewSinkOne(InjSal)                   New Sink builds can only happen a certain number of times over entire time horizon
                 MaxTotalWells(InjSal)                Max on total number of wells drilled over entire time horizon
                 SinkCap(Inj)                         Total storage at SINK cannot exceed capacity
                 SinkWellBalance(Inj,t)               Forces wells on for injection sites
                 SinkBuildsRequired(InjSal,t)         Must incure project capital costs related to the total amount injected
                 MinWellsRequired(InjSal,t)           Must build a minimum of 4 wells when preparing a new injection site
                 BuildPipeBalance(FromNode,ToNode,diameter,t)  Incur new PIPE build cost
                 OnePipeOut(FromNode)                 Only one pipe can come out of any FromNode & only happen in one period
                 OneSizePipe(FromNode,ToNode)         When allowing multiple pipes out - pick one size only
                 TransShipBalance(ToNode,t)        Balance flow through transshipment points

                 EOR_CO2_Reqt(Inj,tp)                 CO2 purchase requirements by year for EOR project

                 DefInjSalReady(InjSal,t)             Sets SinkReady variable once saline sink is built
                 DefInjEORReady(InjEOR,t)             Sets SinkReady variable once EOR sink is built
                 DefPipeReady(AllNode,AllNode,diameter,t) Sets PipeReady variable once pipe is built
                 DefSourceReady(source,t)             Sets SourceReady variable once source is retrofitted

                 OBJ                                  Objective function
                 CaptureOBJ(t)                        Objective - Variable Capture Cost
                 TransportOBJ(t)                      Objective - Variable Transportation Cost
                 StorageOBJ(t)                        Objective - Variable Injection Cost
                 CapitalCostsOBJ(t)                   Objective - Capital Retrofit - Pipe Construction - New Sink Costs
                 OffsetOBJ(t)                         Objective - Carbon Offset

;

* Objective Function

CaptureOBJ(t)..      TOTCaptureCost(t) =E= sum(source, CaptureCost(source,t) * CAPTURED(source,t));

TransportOBJ(t)..    TOTTransCost(t)   =E= sum(link(FromNode,ToNode,diameter), TransportCost(link,t)* PIPED(link,t)) ;

StorageOBJ(t)..      TOTStoreCost(t)   =E= sum(InjSal, InjectCost(InjSal,t) * INJECTED(InjSal,t) +
                                                       WellCapCost(InjSal,t) * WELLNUM(InjSal,t) ) ;

CapitalCostsOBJ(t).. TOTCapitalCost(t) =E= sum(source, SrcCapCost(source,t) * NEWSOURCE(source,t))
                                         + sum(link(FromNode,ToNode,diameter), PipeCapCost(link,t)* BLDPIPE(link,t))
                                         + sum(InjSal, SinkCapCost(InjSal,t) * NEWSINK(InjSal,t));

OffsetOBJ(t)..       TOTOffset(t)      =E= sum(Inj, CarbonOffset(t)* INJECTED(Inj,t) );

OBJ..                TotalCost         =E= sum(t, TOTCaptureCost(t)
                                                + TOTTransCost(t)
                                                + TOTStoreCost(t)
                                                + TOTCapitalCost(t)
                                                - TOTOffset(t)
                                                + sum(link(FromNode,ToNode,diameter)$transShip(ToNode), PV("Transport",t) * TrunkPenalty * PIPED(Link,t))
                                                + sum(source, SlackPenalty(t)*CaptureSlack(source,t))
                                                + 5000 * sum(link, PV("Transport",t) * zpipe(link,t))
                                                + EORSafetyPrice * sum(Inj, PV("Storage",t) * zInj(Inj,t))
                                                + 5000 * sum(source, PV("Capture",t) * zCap(source,t)) );

************* Source Constraints

SourceBalance(plant,t)..      sum(unitplt(source,plant),CAPTURED(source,t) + zCap(source,t)) =E= sum(link(plant,ToNode,diameter), PIPED(Link,t) );

* These constraints make sure that the capital cost is incurred only once, in the time period when the retrofit is initiated.
* Capture capability becomes available after stated leadtime.

* When the row sense is (=) the source must capture to it's max once retrofitted/built
* When row sense is (<=), the source may adjust the amount of CO2 captured, not exceeding its max
RetrofitBalance(source,t)..   CAPTURED(source,t) + CaptureSlack(source,t) =E= CaptureMax(source,t)* SourceReady(source,t);

DefSourceReady(source,t)..    SourceReady(source,t) =E= SourceReady(source,t-1) + NEWSOURCE(source,t-RetroLT(source));

************* Injection Site (Sink) Constraints

SinkBalance(INJ_TERMINALS,t)..    sum(link(FromNode,INJ_TERMINALS,diameter), PIPED(link,t)) =E= sum(Inj$INJ_TDIST(Inj,INJ_TERMINALS), INJECTED(Inj,t));

* These constraints make sure that the capital cost is incurred in the time period when the new storage site is initiated
* and sets the SinkReady switch on. Injection capability becomes available after stated leadtime.

DefInjSalReady(InjSal,t)..   SinkReady(InjSal,t) =E= SinkReady(InjSal,t-1) + NEWSINK(InjSal,t-NewSinkLT(InjSal));

DefInjEORReady(InjEOR,t)..   SinkReady(InjEOR,t) =E= SinkReady(InjEOR,t-1) + EOR_ON(InjEOR,t) ;

NewBldSinkBalance(InjSal,t)..   INJECTED(InjSal,t) =L= WellMax(InjSal) * sum(tn$(Ord(tn) <= Ord(t)), WELLNUM(InjSal,tn)) ;

* 99 is a Big M for the most wells that will be drilled at a sink in a year
SinkWellBalance(InjSal,t)..     WELLNUM(InjSal,t) - min(99,MaxWells(InjSal)) * SinkReady(InjSal,t) =L= 0;

MaxTotalWells(InjSal)..      sum(t, WELLNUM(InjSal,t)) - MaxWells(InjSal) =L= 0 ;

NewSinkOne(InjSal)..         sum(t, NEWSINK(InjSal,t)) =L= ROUND(InjectCap(InjSal)/SinkProps(InjSal,'SiteMax_MMT'),0) ;

SinkBuildsRequired(InjSal,t)..
  sum(tn$(Ord(tn) <= Ord(t)), INJECTED(InjSal,tn) ) - ROUND(SinkProps(InjSal,'SiteMax_MMT'),0)*sum(tn$(Ord(tn) <= Ord(t)), NEWSINK(InjSal,tn)) =L= 0 ;

MinWellsRequired(InjSal,t)..
  WELLNUM(InjSal,t) - MinWells(InjSal)*NEWSINK(InjSal,t) =G= 0 ;

* This constraint represents the total capacity for each injection site
SinkCap(InjSal)..               sum(t,INJECTED(InjSal,t)) =L= InjectCap(InjSal) ;

* Based on start year and performance level, this constraint fixes the CO2 purchases needed at the EOR site
EOR_CO2_Reqt(InjEOR,tp)..    INJECTED(InjEOR,tp) + zInj(InjEOR,tp) =E=  sum(t, EOR_CO2Purch(InjEOR,t,tp)* EOR_ON(InjEOR,t)) ;

************* Transportation Constraints

* These two constraints make sure that the capital cost is incurred in the time period
* when the pipeline link is initiated. Transfer capability becomes available after stated leadtime.

DefPipeReady(Link(FromNode,ToNode,diameter),t)..      PipeReady(Link,t) =E= PipeReady(Link,t-1) + BLDPIPE(Link,t-PipeLT(link));

BuildPipeBalance(link(FromNode,ToNode,diameter),t)..  PIPED(link,t)- zpipe(link,t) =L= PipeMax(diameter)*PipeReady(link,t);

*Only one pipe can come out of any Node in OnePipeNodes
OnePipeOut(OnePipeNodes)..       sum((link(OnePipeNodes,ToNode,diameter),t), BLDPIPE(Link,t)) =L= 1;

*Only one pipe per link
OneSizePipe(FromNode,ToNode)..   sum((link(FromNode,ToNode,diameter),t), BLDPIPE(link,t)) =L= maxPipes ;

* Flow going into a transshipment node must equal the flow leaving a transshipment node
TransShipBalance(ToNode,t)$transship(ToNode)..
   sum(Link(FromNode,ToNode,diameter),PIPED(link,t)) -
   sum(Link(FromNode(ToNode),ToNode2,diameter), PIPED(link,t))
   =E= 0;


Model CTS  / all / ;


$onecho > cplex.opt
relaxfixedinfeas 1
lpmethod 4
subalg 4
startalg 4
auxrootthreads 0
parallelmode 1
threads 1
solvefinal 0
mipstart 1
cutpass 10
mipordind 1
nodesel 3
varsel 3
polishaftertime 14400
$offecho
*probe 2
*probetime 240
*mipemphasis 1
*nodefileind = 2
*workmem = 128
*mipsearch 1
*rinsheur 150
*polishaftertime 120
*polishafterintsol 1
*polishafterepgap 0.05

$onecho > xpress.opt
algorithm barrier
reslim 900
threads 1
covercuts 2
loadmipsol 1
cutstrategy 3
mippresolve 9
preprobing 3
$offecho

cts.optfile=1;
cts.solprint=0;
*cts.dictfile=0;
cts.holdfixed=1;
* relative gap measure (percentage difference from LP presolve)
cts.optcr=0.001;
* resolution limit in seconds (10800 = 3 hours; 5400 = 1.5 hours)
cts.reslim = 1200 ;

* Allow ForceAll case to run to optimality
$Ifi %ForceAllOn% == -1 $goto next7
  cts.optcr=0.0;
$label next7

Parameters
          NEWSINKsav(InjSal,t)                   1 if  SINK is a new build in time t available after leadtime - Saline
          NEWSOURCEsav(source,t)                 1 if source is retrofitted in time t available after leadtime
          BLDPIPEsav(FromNode,ToNode,diameter,t) 1 if PIPE ij is built in time t available after leadtime
          EOR_ONsav(InjEOR,t)                    1 if EOR is initiated at SINK in time t                      - EOR
          WELLNUMsav(InjSal,t)                   number of wells added at each sink in each time t            - Saline
;

* Start from an existing integer solution if it exists
file puttemp /puttemp.lst/;
put puttemp;

execute 'test -s CTSwarmStart.gdx' ;
if(errorlevel=0,
  put_utility 'gdxin' / 'CTSwarmStart.gdx'
  execute_load NEWSINKsav, NEWSOURCEsav, BLDPIPEsav, WELLNUMsav ;
*  execute_load NEWSINKsav, NEWSOURCEsav, BLDPIPEsav, EOR_ONsav, WELLNUMsav ;

  NEWSINK.l(InjSal,t)                         = NEWSINKsav(InjSal,t) ;
  NEWSOURCE.l(source,t)                       = NEWSOURCEsav(source,t) ;
  BLDPIPE.l(link(FromNode,ToNode,diameter),t) = BLDPIPEsav(FromNode,ToNode,diameter,t) ;
*  EOR_ON.l(InjEOR,t)                          = EOR_ONsav(InjEOR,t) ;
  WELLNUM.l(InjSal,t)                         = WELLNUMsav(InjSal,t) ;

);
putclose puttemp ;

* Solve the model
option mip = %LPsolver% ;
option limrow = 0;
option limcol = 0;
option sysout = on;
Solve CTS minimizing TotalCost using mip ;

* Output integer variable solutions to a warm start file if the model terminated with an integer solution
if(cts.Modelstat=8 or cts.Modelstat=1,

  NEWSINKsav(InjSal,t)   = NEWSINK.l(InjSal,t) ;
  NEWSOURCEsav(source,t) = NEWSOURCE.l(source,t) ;
  BLDPIPEsav(link(FromNode,ToNode,diameter),t) = BLDPIPE.l(FromNode,ToNode,diameter,t) ;
  EOR_ONsav(InjEOR,t) = EOR_ON.l(InjEOR,t) ;
  WELLNUMsav(InjSal,t) = WELLNUM.l(InjSal,t) ;

  Execute_Unload "CTSwarmStart.gdx",
    NEWSINKsav, NEWSOURCEsav, BLDPIPEsav, EOR_ONsav, WELLNUMsav ;

);

*Abort$(cts.Modelstat <> 8 and cts.Modelstat <> 1) 'Model did not terminate with integer solution';
*Abort$(not(cts.Modelstat=8 or cts.Modelstat=1));

*Generate files for Matrix Analyzer
*If( RunDefn('MatrixAnalyze')= 1,

*$onecho > convert.op2
*ANALYZES
*$offecho

*option limrow = 0;
*option limcol = 0;
*option MIP=convert; CTS.optfile=2;
*option threads = 1;
*Solve CTS minimizing TotalCost using mip ;

*);


