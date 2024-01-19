*Validation Checks   - see CTSDataCheck.gdx for results

********************************************************
*Input - Check that the pipe size assigned to source pipes are the best fit, w.r.t. the max flow
*Output - Check that the solution chose the best size for the trunk lines, w.r.t. the max flow
*List exceptions in BadPipeFit
* Added 10//13/2010 - VAF


$gdxin %dir%CTUSModIN.gdx
sets  t ,source,Inj,link,plant, diameter,FromNode,ToNode, N_IGRP, unitplt;
$load t ,source,Inj,link,plant, diameter,FromNode,ToNode, N_IGRP, unitplt
parameters SrcCapCost, CaptureCost,PipeCapCost,TransportCost, SinkCapCost, InjectCost,PipeMax,WellMax, InjectCap, CarbonOffset, CaptureMax;
$load SrcCapCost, CaptureCost,PipeCapCost,TransportCost,SinkCapCost,InjectCost,PipeMax,WellMax, InjectCap,CarbonOffset, CaptureMax

********************************************************
*Input - Check for missing costs
* Added 10//13/2010 - VAF

parameters ZeroCapCost   Missing Capture Costs
           ZeroPipeCost  Missing Transport Costs
           ZeroInjCost   Missing Injection Costs
           ZeroCapData   Missing Capture Bounds
           ZeroPipeData  Missing Transport Bounds
           ZeroInjData   Missing Injection Bounds;

ZeroCapCost(source,t,'Fixed Cost')$(not N_IGRP(source)) = SrcCapCost(source,t) = 0;
ZeroCapCost(source,t,'Var Cost')$(not N_IGRP(source))   = CaptureCost(source,t) = 0;
ZeroCapCost(source,t,'Offset')     = CarbonOffset(t) = 0;

ZeroPipeCost(link(FromNode,ToNode,diameter),t,'Fixed Cost') = PipeCapCost(FromNode,ToNode,diameter,t) = 0 ;
ZeroPipeCost(link(FromNode,ToNode,diameter),t,'Var Cost')   = TransportCost(FromNode,ToNode,diameter,t) = 0;

ZeroInjCost(Inj,t,'Fixed Cost') = SinkCapCost(Inj,t) = 0;
ZeroInjCost(Inj,t,'Var Cost')   = InjectCost(Inj,t) = 0;

ZeroCapData(source,'CapMax')       = sum(t,CaptureMax(source,t))  = 0;

ZeroPipeData(link(FromNode,ToNode,diameter),' ','Rate')   =  PipeMax(diameter) = 0;

ZeroInjData(Inj,'WellMax')     =  WellMax(Inj)  = 0;
ZeroInjData(Inj,'Capacity')    =  InjectCap(Inj)  = 0;

********************************************************
*Input - Check for sources with no links
* Added 10/19/2010 - VAF

set  NoSourceLinks     Plants without links ;

NoSourceLinks(plant) = NOT sum((ToNode,diameter),link(plant,ToNode,diameter)) ;

********************************************************
*Input -  Check that every entry in N_IGRP got matched to a new build site in CTSIndat
* Added 1/25/2011 - VAF

set  MissingNewBuilds     N_IGRPs that were not matched to a new build site ;

MissingNewBuilds(N_IGRP)$(not N_IGRP.val = 50000) = NOT sum(plant,unitplt(N_IGRP,plant)) ;

********************************************************
*Input - Check that every storage site Inj has a link into it
* Added 11/03/2011 - VAF

set  NoSinkLinks     Injection sites without links ;

NoSinkLinks(Inj) = NOT sum((FromNode,diameter),link(FromNode,Inj,diameter)) ;

********************************************************
Execute_Unload '%dir%CTUSDataCheck.gdx', ZeroCapCost, ZeroPipeCost, ZeroInjCost, ZeroCapData, ZeroPipeData, ZeroInjData, NoSourceLinks,
                NoSinkLinks, MissingNewBuilds;

Abort$card(NoSourceLinks) 'Missing links from some sources. Check pipe data.',NoSourceLinks;
*Abort$card(NoSinkLinks) 'Missing links from some storage sites. Check pipe data.',NoSinkLinks;
Abort$card(MissingNewBuilds) 'Not all new units mapped to a plant. Check NewPlantProps in CTSdata.gdx.',MissingNewBuilds;
