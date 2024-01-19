*Date: 10/27/2010
*@ W. B. Rocheleau (brocheleau@onlocationinc.com)

*If the external data is to be skipped then bypass this code and load the data dircetly from CTSExtData.gdx
$If %Update_External_Data% == -1 $GoTo Skip_Import

$log '**** Begin importing external data ****'

*Local Memory SQL statements
$onecho > separatedtextcmd.txt
C=DRIVER={Microsoft Text Driver (*.txt; *.csv)};DBQ=%system.fp%;
*    Cost data and static pipe diameter data
Q1=select Component from %IDir%tCosts.txt;
O1=tCostComp.inc
Q2=select c.iGRP,c.CC from %IDir%NETLCaptureCosts.csv c, %IDir%PowerPlants.txt d where c.iGRP=d.iGRP;
O2=RetroCosts.inc
Q3=select c.iGRP,c.VarCost from %IDir%NETLCaptureCosts.csv c, %IDir%PowerPlants.txt d where c.iGRP=d.iGRP;
O3=CaptureVar.inc
Q4=select Diameter from %IDir%pipeDiameter.txt;
O4=diameterSet.inc
Q5=select Diameter,PipeMax from %IDir%pipeDiameter.txt;
O5=pipeMax.inc
Q6=select Component from %IDir%MonCosts.txt;
O6=MonCostComp.inc
*    GIS generated data
Q7=select iGRP from %IDir%PowerPlants.txt;
O7=sourceSet.inc
Q8=select Sink from %IDir%InjectionSites.txt;
O8=sinkSet.inc
Q9=select UPSite from %IDir%NewBuilds.txt;
O9=NewBuilds.inc
Q10=select distinct PLT_CODE from %IDir%PowerPlants.txt;
O10=PlantSet.inc

$offecho
$call =sql2gms @separatedtextcmd.txt

Sets
    Esource      Existing Capture Sites   /
$include sourceSet.inc
/
    Eplant       Existing Power Plants  /
$include PlantSet.inc
/
    sink        Injection Sites  /
$include sinkSet.inc
/
    transShip   Transshipment Points /
$include %IDir%TPoints.txt
/
    diameter    Pipe Diameter Options /
$include diameterSet.inc
/
    tCost_set   Rowset for Tcost /
$include tCostComp.inc
/
    MonCost_set   Rowset for MonCost /
$include MonCostComp.inc
/
    NewPlant    New Build Plant Sites /
$include NewBuilds.inc
/

*Create super sets
    AllNode           plant + transship + sink       /set.Eplant,set.NewPlant,set.transShip,set.sink/
    ToNode            transship + sink               /set.transShip,set.sink/

*Build attribute dimensions (column headers) for data import
    source_att        colset for SourceProps         /CF,SCAP,HeatRate,R_CO2/
    sink_att          colset for SinkProps           /InjectMax_MMTperYr,Capacity_MMT,Depth_m/
    tCost_att         colset for Tcost               /Component,Length,Fixed,DD,D,Constant/
    MonCost_att       colset for MonCost             /Component,FixedCost,perWell,perWellperMeter,perWellperMeterperYear/
    NewPlant_att      colset for newbuild            /iGRP,FuelRegion/
    ProgramGoal_att   colset for ProgramGoal         /FixedAdjC,VarAdjC,FixedAdjT,VarAdjT,FixedAdjS,VarAdjS,HRAdj/
;
$ondelim
Set  link(AllNode,ToNode,diameter)        Allowable pipeline links /
$include %IDir%pipeOpt.txt
/;

Parameter LinkDist(AllNode,ToNode)        PipeLine Distances (miles) /
$include %IDir%pipeDist.txt
/;

Parameter
     PipeMax(diameter)                    Pipeline Diameter Max Flow (millions tonnes) /
$include pipeMax.inc
/;

Parameter
    RetroBase(Esource)                    Capture Site Retrofit Capital Costs (million dollars) /
$include RetroCosts.inc
/;

Parameter
    CapCostVar(Esource)                   Capture Site Variable Costs $ per tonne /
$include CaptureVar.inc
/;

Table
     ProgramGoal(*,ProgramGoal_att)       Program Goal Adjustments
$include %IDir%ProgramGoal.csv

Table
    SinkProps(sink,sink_att)              Sink Properties
$include %IDir%InjectionSites.txt

Table
    NewPlantProps(NewPlant,NewPlant_att)  New Plant Properties
$include %IDir%NewBuilds.txt

Table
    SourceProps(Esource,Eplant,source_att)     Source Properties
$include %IDir%PowerPlants.txt

Table
    Tcosts(tCost_set,tCost_att)           Transportation Costs
$include %IDir%tCosts.txt

Table
    MonCosts(MonCost_set,MonCost_att)     Monitoring Costs at the Injection Site
$include %IDir%MonCosts.txt

$offdelim

Execute_unload 'CTSinput.gdx'

********************************
*If the external data has been skipped then pick up here to get the following sets and parameters declared
* and loaded from an existing CTSextData.gdx
$Label Skip_Import

*If the external data has been updated these sets and parameters have already been declared and loaded
$If %Update_External_Data% == 1 $GoTo Skip_Load

Sets
    Esource                        Existing Capture Sites
    Eplant                         Existing Power Plants
    sink                           Injection Sites
    transShip                      Transshipment Points
    diameter                       Pipe Diameter Options
    tCost_set                      Rowset for Tcost
    MonCost_set                    Rowset for MonCost
    NewPlant                       New Build Plant Sites
    AllNode                        plant + transship + sink
    ToNode                         transship + sink
    source_att                     colset for SourceProps
    sink_att                       colset for SinkProps
    tCost_att                      colset for Tcost
    MonCost_att                    colset for MonCost
    NewPlant_att                   colset for newbuild
    ProgramGoal_att                colset for ProgramGoal
    link                           Allowable pipeline links
;

$gdxin '%Idir%CTSinput.gdx'
$Load Esource,Eplant,sink,transShip,diameter,tCost_set,MonCost_set, NewPlant,AllNode, ToNode,link
$Load source_att,sink_att,tCost_att,MonCost_att,NewPlant_att,ProgramGoal_att

Parameters
   RetroBase(Esource)                     Capture Site Retrofit Capital Costs (million dollars)
   CapCostVar(Esource)                    Capture Site Variable Costs $ per tonne
   LinkDist(AllNode,ToNode)               PipeLine Distances (miles)
   PipeMax(diameter)                      Pipeline Diameter Max Flow (millions tonnes)
   ProgramGoal                            Program Goal Adjustments
   SinkProps(sink,sink_att)               Sink Properties
   NewPlantProps(NewPlant,NewPlant_att)   New Plant Properties
   SourceProps(Esource,Eplant,source_att) Source Properties
   Tcosts(tCost_set,tCost_att)            Transportation Costs
   MonCosts(MonCost_set,MonCost_att)      Monitoring Costs at the Injection Site

;

$Load RetroBase,CapCostVar,LinkDist,PipeMax,ProgramGoal,SinkProps,NewPlantProps,SourceProps,Tcosts,MonCosts
$gdxin

*If the external data has been updated these sets and parameters have already been declared and loaded
$Label Skip_Load

$log '**** Begin importing ECP data ****'
$gdxin CTSSavR.gdx

Set Year         model years
    N_IGRP       New plant build iGRP;

$load Year, N_IGRP

Parameters
*         TMPFYR(Year)                Current ECP run year
         GDP(Year)                   GDP by year
         CarbonPrice(Year)           Carbon price
         EMM_CL_CF(Esource,Year)     Capacity Factor-indicates retirements
         DiscRateC(Year)             Discount Rate - Capture
         DiscRateT(Year)             Discount Rate - Transport
         DiscRateS(Year)             Discount Rate - Storage
         N_RY(N_IGRP)                New plant Go Live Year
         N_RG(N_IGRP)                New plant EMM Fuel Region
         N_CFR(N_IGRP)               New plant Capacity Factor
         N_HRAT(N_IGRP)              New plant Heat Rate
         N_CPTY(N_IGRP)              New plant Summer Capacity
         N_PTP(N_IGRP)               New plant fuel type coal=35 gas=43

         NewPlantCap(NewPlant)       Aggregate new plant capacity
;
$Load  CarbonPrice, EMM_CL_CF, DiscRateC, DiscRateT, DiscRateS, N_RY, N_RG, N_CFR, N_HRAT,N_CPTY,N_PTP,GDP
*$Load TMPFYR
$gdxin


$log '**** Begin matching new build ECP IGRP to new build sites ****'

*Sites for new builds are created ahead of time with generic names.  When ECP passes a list of new builds the following
*code matches the new IGRP number with an unassigned new build site

*Variable DONE;
*
*Set  Nunitplt(N_IGRP,NewPlant)  New unit-plant combos to add to unitplt later;
*
*Loop(N_IGRP,
*  DONE.l = -1;
*  Loop(NewPlant,
*    if(DONE.l EQ -1 and NewPlantProps(NewPlant,'iGRP') EQ -1 and NewPlantProps(NewPlant,'FuelRegion') EQ N_RG(N_IGRP),
*       NewPlantProps(NewPlant,'iGRP') = N_IGRP.val;
*       Nunitplt(N_IGRP,NewPlant) = yes;
*       DONE.l = 0;
*    );
*  );
*);

* Add new sources and new plants to the source and plant sets
Set
    source       existing sites + new sites     /set.Esource,set.N_IGRP/
    Nplant       new plants used this run
    Nunitplt(N_IGRP,NewPlant)  New unit-plant combos to add to unitplt later
;

* Map capture units to new and existing power plants
Scalar DONE;

NewPlantCap(NewPlant) = 0 ;

* Assign unplanned builds and to new plant locations
Loop(N_IGRP$(N_CPTY(N_IGRP)>0),
  DONE = -1 ;
  Loop(NewPlant$(DONE = -1),
    If(((NewPlantProps(NewPlant,'FuelRegion') EQ N_RG(N_IGRP)) and (NewPlantCap(NewPlant) <= 500)),
       If(NewPlantProps(NewPlant,'iGRP') = -1,
          NewPlantProps(NewPlant,'iGRP') = 1 ;
       else
          NewPlantProps(NewPlant,'iGRP') = NewPlantProps(NewPlant,'iGRP') + 1 ;
       );
       Nunitplt(N_IGRP, NewPlant) = yes;
       NewPlantCap(NewPlant) = NewPlantCap(NewPlant) + N_CPTY(N_IGRP) ;
       DONE = 1 ;
    );
  );
);


Nplant(NewPlant) = yes$(NewPlantProps(NewPlant,'iGRP') ne -1);

Set plant        existing plants + new plants   /set.Eplant /;

plant(Nplant) = yes;

Set
    FromNode     plant + transship              /set.transShip/ ;

FromNode(plant) = yes;

$log '**** Matching ECP IGRP to new plant sites COMPLETE ****'

Execute_unload 'CTSdata.gdx'
