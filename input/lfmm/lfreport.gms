***** $Header: m:/default/input/RCS/lfreport.gms,v 1.163 2021/02/25 19:55:24 ESH Exp $
******** Generate NEMS variables for pass back

  if(NCNTRL_CURCALYR>2010,

  if (NCNTRL_FCRL=1,
*   Check if the file LFMM_report.gdx exists and if it does, load it
    execute 'test -s LFMM_report.gdx';

    if(errorlevel=0,
      put_utility 'gdxin' / 'LFMM_report.gdx' ;
      execute_load   repActivity, repUtilization,
        repRefWSPrice, repCenWSPrice, repRefRefTran, repRefCenTran, repRefProd,
        repRefRefTranPLL, repRefRefTranCPL, repReftoRefTranCap, repRefRefTranMode,
        repRefRefTranModeCapUtz,
        repREFCDTranCap, repRefCDTranMode, repRefCDTranModeCapUtz,
        repRefFuelUse, repRefElecPurch, repTotEnergyUse, repRFSCredits, repCO2Emissions,
        repTotalCO2, repMargCrdPrice, repCrudeUse, repCrudeRAC, repSpecProdPrice,
        repSpecProp, repEnergyBalance, repWSAltFuelPrc,
        repCrudeImports, repCrudeExports, repDomesticCrudeSup,
        repCrudeInputs,repCrudeInputsTotal,repOtherSupply,repOtherSupplyTotal,
        repNonPetOther,repNonPetOtherTotal,repProcessGain,repProcessGainTotal,
        repPetOutput,repPetOutputTotal,repPetFuelUse,repPetFuelUseTotal,
        repDiscrepency, repBalanceRep, repNGLSource, repNGLConsumption,
        nr_r_OpCost, nr_q_OpCost, nr_p_Stream, nr_q_Stream, repRFSMarkups ;
    );
  );


* ** RFS   reporting ***
* Bil credits per year
  repRFSCredits(RFSCategory,'Biodiesel',Period,RunYears) =
    sum((SupTypMode(DomRefReg,RefType,Process,ProcessMode),Stream('FBD'))$(RFSProcCategory(RFSCategory,Stream) and RFSScores(Process,Stream)),
      RFSScores(Process,Stream) * ProcessTable(Stream,Process,ProcessMode) * PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) * MbblCD_2_BGY  -
    sum( (DomRefReg,RefType,RcpMode('RCP_N2H1'),Stream('FBD'),Process)$(RFSProcCategory(RFSCategory,Stream) and RFSScores(Process,Stream)),
      RFSScores(Process,Stream) * ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,Stream,Period) ) * MbblCD_2_BGY +
    sum((Step,Process('FBD'),Stream('FBD'))$(RFSProcCategory(RFSCategory,Stream) and RFSScores(Process,Stream)),
      RFSScores(Process,Stream)* BIODIMP.l(Step,Period))* MbblCD_2_BGY ;


  repRFSCredits(RFSCategory,'Renewable Diesel',Period,RunYears) =
    sum((SupTypMode(DomRefReg,RefType,Process,ProcessMode),Stream('RDH'))$(RFSProcCategory(RFSCategory,Stream) and RFSScores(Process,Stream)),
      RFSScores(Process,Stream) * ProcessTable(Stream,Process,ProcessMode) * PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) * MbblCD_2_BGY -
    sum( (DomRefReg,RefType,SpecProd('N2H'),Stream('RDH'),Process)$(RFSProcCategory(RFSCategory,Stream) and RFSScores(Process,Stream)),
      RFSScores(Process,Stream)* ToSPECBLEND.l(DomRefReg,RefType,SpecProd,Stream,Period) ) * MbblCD_2_BGY +
    sum((Step,Process('GDT'),Stream('RDH'))$(RFSProcCategory(RFSCategory,Stream) and RFSScores(Process,Stream)),
      RFSScores(Process,Stream)* RENEWDIMP.l(Step,Period))* MbblCD_2_BGY ;


  repRFSCredits(RFSCategory,'Renewable Gasoline',Period,RunYears) =
    sum((SupTypMode(DomRefReg,RefType,Process,ProcessMode),Stream('RNH'))$(RFSProcCategory(RFSCategory,Stream) and RFSScores(Process,Stream)),
      RFSScores(Process,Stream) * ProcessTable(Stream,Process,ProcessMode) * PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) * MbblCD_2_BGY ;

  repRFSCredits(RFSCategory,'BTL - Pyrolysis',Period,RunYears) =
    sum((SupTypMode(DomRefReg,RefType,Process('BPU'),ProcessMode),Stream)$(RFSProcCategory(RFSCategory,Stream) and RFSScores(Process,Stream)),
      RFSScores(Process,Stream) * ProcessTable(Stream,Process,ProcessMode) * PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) * MbblCD_2_BGY -
    sum( (DomRefReg,RefType,SpecProd('N2H'),Stream('BPD'),Process)$(RFSProcCategory(RFSCategory,Stream) and RFSScores(Process,Stream)),
      RFSScores(Process,Stream)* ToSPECBLEND.l(DomRefReg,RefType,SpecProd,Stream,Period) ) * MbblCD_2_BGY ;

  repRFSCredits(RFSCategory,'BTL - FT',Period,RunYears) =
    sum((SupTypMode(DomRefReg,RefType,Process('BTL'),ProcessMode),Stream)$(RFSProcCategory(RFSCategory,Stream) and RFSScores(Process,Stream)),
      RFSScores(Process,Stream) * ProcessTable(Stream,Process,ProcessMode) * PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) * MbblCD_2_BGY -
    sum( (DomRefReg,RefType,SpecProd('N2H'),Stream('BDS'),Process)$(RFSProcCategory(RFSCategory,Stream) and RFSScores(Process,Stream) ),
        RFSScores(Process,Stream) * ToSPECBLEND.l(DomRefReg,RefType,SpecProd,Stream,Period) ) * MbblCD_2_BGY -
    sum( (DomRefReg,RefType,SpecProd('N2H'),Stream('BKE'),Process)$(RFSProcCategory(RFSCategory,Stream) and RFSScores(Process,Stream) ),
        RFSScores(Process,Stream) * ToSPECBLEND.l(DomRefReg,RefType,SpecProd,Stream,Period) ) * MbblCD_2_BGY ;

  repRFSCredits(RFSCategory,'CBTL',Period,RunYears) =
    sum((SupTypMode(DomRefReg,RefType,Process('CBL'),ProcessMode),Stream)$(RFSProcCategory(RFSCategory,Stream) and RFSScores(Process,Stream)),
      RFSScores(Process,Stream) * ProcessTable(Stream,Process,ProcessMode) * PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) * MbblCD_2_BGY +
    sum((SupTypMode(DomRefReg,RefType,Process('CBLCCS'),ProcessMode),Stream)$(RFSProcCategory(RFSCategory,Stream) and RFSScores(Process,Stream)),
      RFSScores(Process,Stream) * ProcessTable(Stream,Process,ProcessMode) * PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) * MbblCD_2_BGY ;


  repRFSCredits(RFSCategory,'Cellulosic Ethanol',Period,RunYears) =
    sum(EthStreamRFS('ETHCLE')$RFSProcCategory(RFSCategory,EthStreamRFS),
      sum(SupTypMode(DomRefReg,RefType,Process,ProcessMode),
        ProcessTable(EthStreamRFS,Process,ProcessMode) * PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) * MbblCD_2_BGY +
      sum((DomRefReg,PetRefType,RcpMode,Stream('DEN'))$RecipeBlending(RcpMode,'ETHCLE'),
         -1*RecipeBlending(RcpMode,Stream)*RECIPEMODE.l(DomRefReg,PetRefType,RcpMode,Period) ) * MbblCD_2_BGY ) ;

  repRFSCredits(RFSCategory,'Corn Ethanol',Period,RunYears) =
    sum(EthStreamRFS('ETHCRN')$RFSProcCategory(RFSCategory,EthStreamRFS),
      sum(SupTypMode(DomRefReg,RefType,Process,ProcessMode),
        ProcessTable(EthStreamRFS,Process,ProcessMode) * PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) * MbblCD_2_BGY +
      sum((DomRefReg,PetRefType,RcpMode,Stream('DEN'))$RecipeBlending(RcpMode,'ETHCRN'),
         -1*RecipeBlending(RcpMode,Stream)*RECIPEMODE.l(DomRefReg,PetRefType,RcpMode,Period) ) * MbblCD_2_BGY ) +
    sum(EthStreamRFS('ETHGRN')$RFSProcCategory(RFSCategory,EthStreamRFS),
      sum(SupTypMode(DomRefReg,RefType,Process,ProcessMode),
        ProcessTable(EthStreamRFS,Process,ProcessMode) * PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) * MbblCD_2_BGY +
      sum((DomRefReg,PetRefType,RcpMode,Stream('DEN'))$RecipeBlending(RcpMode,'ETHGRN'),
         -1*RecipeBlending(RcpMode,Stream)*RECIPEMODE.l(DomRefReg,PetRefType,RcpMode,Period) ) * MbblCD_2_BGY ) ;

  repRFSCredits(RFSCategory,'Biobutanol',Period,RunYears) =
    sum(EthStream('IBA')$RFSProcCategory(RFSCategory,EthStream),
      sum(SupTypMode(DomRefReg,RefType,Process,ProcessMode)$ProcessTable(EthStream,Process,ProcessMode),
        RFSScores(Process,EthStream)*ProcessTable(EthStream,Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) ) * MbblCD_2_BGY ;

* Advanced from domestic production
  repRFSCredits(RFSCategory,'Advanced Ethanol',Period,RunYears) =
    sum(EthStreamRFS('ETHAET')$RFSProcCategory(RFSCategory,EthStreamRFS),
      sum(SupTypMode(DomRefReg,RefType,Process,ProcessMode),
        ProcessTable(EthStreamRFS,Process,ProcessMode) * PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) * MbblCD_2_BGY  +
      sum((DomRefReg,PetRefType,RcpMode,Stream('DEN'))$RecipeBlending(RcpMode,'ETHAET'),
         -1*RecipeBlending(RcpMode,Stream)*RECIPEMODE.l(DomRefReg,PetRefType,RcpMode,Period) ) * MbblCD_2_BGY ) ;

* Ethanol imports
  repRFSCredits(RFSCategory,'Imported Ethanol',Period,RunYears) =
    sum((DomRefReg,EthStream)$RFSProcCategory(RFSCategory,EthStream), ETHIMP.l(DomRefReg,EthStream,Period)/(1. - 0.0249) ) * MbblCD_2_BGY ;

  repRFSCredits(RFSCategory,'Safety Valve',Period,RunYears) =
    RFSESCAPE.l(RFSCategory,Period) * MbblCD_2_BGY ;

  repRFSCredits(RFSCategory,'Safety Valve',Period('Per1'),RunYears) =
    sum((M4,MNUMYR)$(Per1_MNUMYR(Period,MNUMYR) and M4_2_RFSCat(M4,RFSCategory)), LFMMOUT_RFSSAFETY(M4,MNUMYR) ) ;


*mc6: do NOT include SAFETY in 'Category Total' for ftab reporting
  repRFSCredits(RFSCategory,'Safety Valve',Period('Per1'),RunYears) = 0 ;

  repRFSCredits(RFSCategory,'Category Total',Period,RunYears) = 0 ;
  repRFSCredits(RFSCategory,'Category Total',Period,RunYears) =
    sum(RFSFuelCredCat,
      repRFSCredits(RFSCategory,RFSFuelCredCat,Period,RunYears) ) ;

*mc6: do NOT include SAFETY in 'Category Total' for ftab reporting
  repRFSCredits(RFSCategory,'Safety Valve','Per1',RunYears) =
   (RFSMandates(RFSCategory,RunYears) - repRFSCredits(RFSCategory,'Category Total','Per1',RunYears) ) ;



*


*-----------------------------------------------------------------------------
  loop(PrcPeriod(Period),
*em4 6-8-14
*Refinery Value in 87$ per BBL

    loop((MNUMPR,MNCRUD,MNUMYR)$Per1_MNUMYR('Per1',MNUMYR),
      loop((RefReg,Crude)$(RefReg2MNUMPR(MNUMPR,RefReg) and MNCRUD_2_Crude(MNCRUD,Crude)),
        loop(t$((tMNUM(t,MNUMYR))$Runyears(t)),
          LFMMOUT_P_RFCRUDEINP(MNUMPR,MNCRUD,MNUMYR) =
           CrudeBalance.m(RefReg,Crude,Period) / GDP(t);
        ); ); );

*em4 6/8/14 initially set P_CRUDE_EXPORTS equal to Refinery Value
*em4 6/8/14 then, if exports allowed, set P_CRUDE_EXPORTS as function of dual
* on CrudeSupCurveForeign balance row - export transport cost
* Units: (87$/bbl)

    LFMMOUT_P_CRUDE_EXPORTS(M10,MNCRUD,MNXYRS)$Per1_MNXYRS('Per1',MNXYRS) =
      sum((DomRefReg,Crude,t)$(M10_2_RefReg(M10,DomRefReg) and MNCRUD_2_Crude(MNCRUD,Crude) and tMNXYRS(t,MNXYRS)),
        CrudeBalance.m(DomRefReg,Crude,Period) / GDP(t) ) ;

*Crude export prices in 87$ per BBL
    loop((M10,MNCRUD,MNXYRS)$Per1_MNXYRS('Per1',MNXYRS),
      loop((DomRefReg,Crude)$(M10_2_RefReg(M10,DomRefReg) and MNCRUD_2_Crude(MNCRUD,Crude)),
        loop(t$((tMNXYRS(t,MNXYRS))$Runyears(t)),

          trancost = npv_CrudeExportCost(DomRefReg,'Foreign',Crude,'STEP01',Period) ;
          LFMMOUT_P_CRUDE_EXPORTS(M10,MNCRUD,MNXYRS)$(trancost<500.) =
            (CrudeSupCurveForeign.m('Foreign',Crude,Period) - trancost) / GDP(t);
        ); ); );


*em4 6/8/14 initially set P_CRUDE_IMPORTS equal to Refinery Value
*em4 6/6/14 then, if imports allowed, set P_CRUDE_IMPORTS as function of dual
* on CrudeSupCurveForeign balance row + import transport cost
* Units: (87$/bbl)

    LFMMOUT_P_CRUDE_IMPORTS(M10,MNCRUD,MNXYRS)$Per1_MNXYRS('Per1',MNXYRS) =
      sum((DomRefReg,Crude,t)$(M10_2_RefReg(M10,DomRefReg) and MNCRUD_2_Crude(MNCRUD,Crude) and tMNXYRS(t,MNXYRS)),
        CrudeBalance.m(DomRefReg,Crude,Period) / GDP(t) ) ;

*Crude import prices in 87$ per BBL
    loop((M10,MNCRUD,MNXYRS)$Per1_MNXYRS('Per1',MNXYRS),
      loop((RefReg,Crude)$(M10_2_RefReg(M10,RefReg) and MNCRUD_2_Crude(MNCRUD,Crude)),
        loop(t$((tMNXYRS(t,MNXYRS))$Runyears(t)),
          trancost = npv_CrudeImportCost(RefReg,'Foreign',Crude,'STEP01',Period) ;
          trancost$(CRUDETRANS.l(RefReg,Crude,'Foreign','STEP02',Period) > 0.0)  = npv_CrudeImportCost(RefReg,'Foreign',Crude,'STEP02',Period)  ;
          LFMMOUT_P_CRUDE_IMPORTS(M10,MNCRUD,MNXYRS)$(trancost<500.) =
            (CrudeSupCurveForeign.m('Foreign',Crude,Period) + trancost) / GDP(t);
        ); ); ) ;

    loop(t$RunYears(t),
      CrudeBalance_M_BETA = max(1,(ord(t)-(2030-1990))) ;
    );

    CrudeBalance_M_LP(DomRefReg,Crude,Period) =
      CrudeBalance.m(DomRefReg,Crude,Period);

    CrudeBalance_M_DELTA(DomRefReg,Crude,Period) = 0;

    CrudeBalance_M_DELTA(DomRefReg,Crude,Period)$
      (npv_CrudeExportCost(DomRefReg,'Foreign',Crude,'STEP01',Period)<500)
      =
      CrudeSupCurveForeign.m('Foreign',Crude,Period) -
      npv_CrudeExportCost(DomRefReg,'Foreign',Crude,'STEP01',Period) -
      CrudeBalance.m(DomRefReg,Crude,Period);

    CrudeBalance_M_DELTA(DomRefReg,Crude,Period) =
      max(0,
*          CrudeBalance_M_BETA *
          CrudeBalance_M_DELTA(DomRefReg,Crude,Period));

*    CrudeBalance_M_DELTA(DomRefReg,Crude,Period)$(CrudeBalance_M_DELTA(DomRefReg,Crude,Period)>0) =
*      0.015 * CrudeBalance_M_LP(DomRefReg,Crude,Period);

    CrudeBalance_M_DELTA(DomRefReg,Crude,Period) =
      min(CrudeBalance_M_DELTA(DomRefReg,Crude,Period),
         0.1 * CrudeBalance_M_LP(DomRefReg,Crude,Period));

    CrudeBalance_M_DELTA(DomRefReg,Crude,Period) =
      CrudeBalance_M_DELTA(DomRefReg,Crude,Period) *
      CrudeBalance_M_ALPHA(DomRefReg,Crude,Period);

    CrudeBalance_M_DELTA('2_REFREG',Crude,Period) =
      CrudeBalance_M_DELTA('4_REFREG',Crude,Period);

    CrudeBalance_M_DELTA('5_REFREG',Crude,Period) =
      CrudeBalance_M_DELTA('4_REFREG',Crude,Period);


  ); /* end loop PrcPeriod(Period)*/
*-----------------------------------------------------------------------------


  loop(Per1_t_MNUMYR(Period,t,MNUMYR)$PrcPeriod(Period),

    nr_r_OpCost(RefReg,PetRefType,Process,t) = sum(ProcessMode,
      PROCMODE.l(RefReg,PetRefType,Process,ProcessMode,Period) * npv_OpVarCost(Process,ProcessMode,Period)) / GDP(t);
    nr_q_OpCost(RefReg,PetRefType,Process,t) = sum(ProcessMode, PROCMODE.l(RefReg,PetRefType,Process,ProcessMode,Period));

    nr_p_Stream(RefReg,PetRefType,Crude,t) = (CrudeBalance.m(RefReg,Crude,Period)-CrudeBalance_M_DELTA(RefReg,Crude,Period)) / GDP(t);
    nr_q_Stream(RefReg,PetRefType,Crude,t) = sum((Process,ProcessMode),
      PROCMODE.l(RefReg,PetRefType,Process,ProcessMode,Period) * ProcessTable(Crude,Process,ProcessMode));

    nr_p_Stream(RefReg,PetRefType,Utility,t) = UtilBalance.m(RefReg,PetRefType,Utility,Period) / GDP(t);
    nr_q_Stream(RefReg,PetRefType,Utility,t) = sum((Process,ProcessMode),
      PROCMODE.l(RefReg,PetRefType,Process,ProcessMode,Period) * ProcessTable(Utility,Process,ProcessMode));

    nr_p_Stream(RefReg,PetRefType,RefInputStr,t) = RefInpBalance.m(RefReg,PetRefType,RefInputStr,Period) / GDP(t);
    nr_q_Stream(RefReg,PetRefType,RefInputStr,t) =
      sum((Process,ProcessMode),
         PROCMODE.l(RefReg,PetRefType,Process,ProcessMode,Period) * ProcessTable(RefInputStr,Process,ProcessMode)) -
*      sum(RcpMode$RecipeBlending(RcpMode,RefInputStr), ToRECIPEBLEND.l(RefReg,PetRefType,RcpMode,RefInputStr,Period) ) -
      sum(GasSpecProd, ToSPECBLEND.l(RefReg,PetRefType,GasSpecProd,RefInputStr,Period)$StreamSpecProd(RefInputStr,GasSpecProd) ) -
      sum(DistSpecProd, ToSPECBLEND.l(RefReg,PetRefType,DistSpecProd,RefInputStr,Period)$StreamSpecProd(RefInputStr,DistSpecProd) ) -
      sum(ResidSpecProd, ToSPECBLEND.l(RefReg,PetRefType,ResidSpecProd,RefInputStr,Period)$StreamSpecProd(RefInputStr,ResidSpecProd) );

    nr_p_Stream(RefReg,PetRefType,BioStr,t) = sum(CoalDReg,BioBalance.m(CoalDReg,BioStr,Period)* COALDtoRefMap(CoalDreg,RefReg)) / GDP(t);
    nr_q_Stream(RefReg,PetRefType,BioStr,t) = sum((Process,ProcessMode,CoalDReg),
      PROCMODE.l(RefReg,PetRefType,Process,ProcessMode,Period) * COALDtoRefMap(CoalDreg,RefReg) * ProcessTable(BioStr,Process,ProcessMode));

    nr_p_Stream(RefReg,PetRefType,CoalStr,t) = CoalRefRegBal.m(RefReg,CoalStr,Period) / GDP(t);
    nr_q_Stream(RefReg,PetRefType,CoalStr,t) = sum((Process,ProcessMode),
      PROCMODE.l(RefReg,PetRefType,Process,ProcessMode,Period) * ProcessTable(CoalStr,Process,ProcessMode));

    nr_p_Stream(RefReg,PetRefType,NonCrudeUtil,t) = StreamBalance.m(RefReg,PetRefType,NonCrudeUtil,Period) / GDP(t);
    nr_q_Stream(RefReg,PetRefType,NonCrudeUtil,t) =
      sum((Process,ProcessMode),PROCMODE.l(RefReg,PetRefType,Process,ProcessMode,Period) * ProcessTable(NonCrudeUtil,Process,ProcessMode)) -
      sum(RcpMode, ToRECIPEBLEND.l(RefReg,PetRefType,RcpMode,NonCrudeUtil,Period)$RecipeSteam(NonCrudeUtil)) -
      sum(GasSpecProd, ToSPECBLEND.l(RefReg,PetRefType,GasSpecProd,NonCrudeUtil,Period)$StreamSpecProd(NonCrudeUtil,GasSpecProd) ) -
      sum(DistSpecProd, ToSPECBLEND.l(RefReg,PetRefType,DistSpecProd,NonCrudeUtil,Period)$StreamSpecProd(NonCrudeUtil,DistSpecProd) ) -
      sum(ResidSpecProd, ToSPECBLEND.l(RefReg,PetRefType,ResidSpecProd,NonCrudeUtil,Period)$StreamSpecProd(NonCrudeUtil,ResidSpecProd) );

    nr_p_Stream(RefReg,PetRefType,GasSpecProd,t) = GasSpecBalance.m(RefReg,PetRefType,GasSpecProd,Period) / GDP(t);
    nr_q_Stream(RefReg,PetRefType,GasSpecProd,t) =
      sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,GasSpecProd), ToSPECBLEND.l(RefReg,PetRefType,GasSpecProd,NonCrudeUtil,Period) );

    nr_p_Stream(RefReg,PetRefType,DistSpecProd,t) = DistSpecBalance.m(RefReg,PetRefType,DistSpecProd,Period) / GDP(t);
    nr_q_Stream(RefReg,PetRefType,DistSpecProd,t) =
      sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,DistSpecProd), ToSPECBLEND.l(RefReg,PetRefType,DistSpecProd,NonCrudeUtil,Period) );

    nr_p_Stream(RefReg,PetRefType,ResidSpecProd,t) = ResidSpecBalance.m(RefReg,PetRefType,ResidSpecProd,Period) / GDP(t);
    nr_q_Stream(RefReg,PetRefType,ResidSpecProd,t) =
      sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,ResidSpecProd), ToSPECBLEND.l(RefReg,PetRefType,ResidSpecProd,NonCrudeUtil,Period) );

    Gain_Detail(RefReg,RefType,Gain_Streams,t) = sum((Gain_process,ProcessMode),
       PROCMODE.l(RefReg,RefType,Gain_Process,ProcessMode,Period) * ProcessTable(Gain_Streams,Gain_Process,ProcessMode));

    Gain_ToSpec(RefReg,PetRefType,NonCrudeUtil,t) =  0.0 -
      sum(GasSpecProd, ToSPECBLEND.l(RefReg,PetRefType,GasSpecProd,NonCrudeUtil,Period)$StreamSpecProd(NonCrudeUtil,GasSpecProd) ) -
      sum(DistSpecProd, ToSPECBLEND.l(RefReg,PetRefType,DistSpecProd,NonCrudeUtil,Period)$StreamSpecProd(NonCrudeUtil,DistSpecProd) ) -
      sum(ResidSpecProd, ToSPECBLEND.l(RefReg,PetRefType,ResidSpecProd,NonCrudeUtil,Period)$StreamSpecProd(NonCrudeUtil,ResidSpecProd) );

    Gain_ToSpec(RefReg,PetRefType,GasSpecProd,t) =
      sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,GasSpecProd), ToSPECBLEND.l(RefReg,PetRefType,GasSpecProd,NonCrudeUtil,Period) );
    Gain_ToSpec(RefReg,PetRefType,DistSpecProd,t) =
      sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,DistSpecProd), ToSPECBLEND.l(RefReg,PetRefType,DistSpecProd,NonCrudeUtil,Period) );
    Gain_ToSpec(RefReg,PetRefType,ResidSpecProd,t) =
      sum(NonCrudeUtil$StreamSpecProd(NonCrudeUtil,ResidSpecProd), ToSPECBLEND.l(RefReg,PetRefType,ResidSpecProd,NonCrudeUtil,Period) );

    Crude_Detail(RefReg,RefType,Crude,t) = sum((Gain_Process,ProcessMode),
       PROCMODE.l(RefReg,RefType,Gain_Process,ProcessMode,Period) * ProcessTable(Crude,Gain_Process,ProcessMode));

    Total_Gain(Gain_Streams,Gain_Process) = sum((DomRefReg,RefType,ProcessMode),
       PROCMODE.l(DomRefReg,RefType,Gain_Process,ProcessMode,Period) * ProcessTable(Gain_Streams,Gain_Process,ProcessMode));

    PMMOUT_RFQPRCG(MNUMPR,MNUMYR) =
       sum((DomRefReg,RefType,Gain_Streams,Gain_Process,ProcessMode)$RefReg2MNUMPR(MNUMPR,DomRefReg),
       0.001 * PROCMODE.l(DomRefReg,RefType,Gain_Process,ProcessMode,Period) * ProcessTable(Gain_Streams,Gain_Process,ProcessMode));

    PMMOUT_RFQPRCG('10_MNUMPR',MNUMYR) =
       sum((DomRefReg,RefType,Gain_Streams,Gain_Process,ProcessMode),
       0.001 * PROCMODE.l(DomRefReg,RefType,Gain_Process,ProcessMode,Period) * ProcessTable(Gain_Streams,Gain_Process,ProcessMode));

    LFMMOUT_REFGAIN(MNUMPR,'1_M2',MNUMYR) =
       sum((DomRefReg,Gain_Streams,Gain_Process,ProcessMode)$RefReg2MNUMPR(MNUMPR,DomRefReg),
       PROCMODE.l(DomRefReg,'Coking',Gain_Process,ProcessMode,Period) * ProcessTable(Gain_Streams,Gain_Process,ProcessMode));

    LFMMOUT_REFGAIN(MNUMPR,'2_M2',MNUMYR) = 0;
*       sum((DomRefReg,Gain_Streams,Gain_Process,ProcessMode)$RefReg2MNUMPR(MNUMPR,DomRefReg),
*       PROCMODE.l(DomRefReg,'Cracking',Gain_Process,ProcessMode,Period) * ProcessTable(Gain_Streams,Gain_Process,ProcessMode));

    LFMMOUT_REFGAIN('10_MNUMPR','1_M2',MNUMYR) =
       sum((DomRefReg,Gain_Streams,Gain_Process,ProcessMode),
       PROCMODE.l(DomRefReg,'Coking',Gain_Process,ProcessMode,Period) * ProcessTable(Gain_Streams,Gain_Process,ProcessMode));

    LFMMOUT_REFGAIN('10_MNUMPR','2_M2',MNUMYR) = 0;
*       sum((DomRefReg,Gain_Streams,Gain_Process,ProcessMode),
*       PROCMODE.l(DomRefReg,'Cracking',Gain_Process,ProcessMode,Period) * ProcessTable(Gain_Streams,Gain_Process,ProcessMode));

    LFMMOUT_RFSCREDITS(M4,M13,MNUMYR) =
       sum((RFSCategory,RFSFuelCredCat)$(M4_2_RFSCat(M4,RFSCategory) and RFSFuelCredCat_M13(RFSFuelCredCat,M13)),
          repRFSCredits(RFSCategory,RFSFuelCredCat,Period,t) ) ;

    LFMMOUT_RFSMANDATES(M4,MNUMYR) =
       sum(RFSCategory$M4_2_RFSCat(M4,RFSCategory), RFSMandates(RFSCategory,t) ) ;


    MPBLK_PASIN(MNUMCR,MNUMYR) =
      sum(ActiveDem $crN2L(ActiveDem,MNUMCR),
        RecipeDemands.m(ActiveDem,'ASPHout',Period) / CONVFACT_CFASQ / GDP(t)) ;

    MPBLK_PASIN(MNUMCR,MNUMYR) = max(MPBLK_PASIN(MNUMCR,MNUMYR),0.0001) ;

    if(sum((ActiveDem,MNUMCR)$(crN2L(ActiveDem,MNUMCR) ), QBLK_QASIN(MNUMCR,MNUMYR))>0,
      MPBLK_PASIN('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), MPBLK_PASIN(MNUMCR,MNUMYR)*QBLK_QASIN(MNUMCR,MNUMYR) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QASIN(MNUMCR,MNUMYR) ) ;
    );

*   CONVFACT_CFPFQ(MNUMYR)$(sum((DomRefReg,RefType,RcpMode)$RecipeBlending(RcpMode,'PCFout'), RECIPEMODE.l(DomRefReg,RefType,RcpMode,Period))>0) =
*      sum((DomRefReg,RefType,RcpMode)$RecipeBlending(RcpMode,'PCFout'),
*         sum(IntStream$((RecipeInputs(IntStream) and RecipeBlending(RcpMode,IntStream))$(not RecipeSteam(IntStream))),
*            -1*RecipeBlending(RcpMode,IntStream)*RECIPEMODE.l(DomRefReg,RefType,RcpMode,Period)*StreamProp(IntStream,'END') ) ) /
*      sum((DomRefReg,RefType,RcpMode)$RecipeBlending(RcpMode,'PCFout'), RECIPEMODE.l(DomRefReg,RefType,RcpMode,Period));

    MPBLK_PPFIN(MNUMCR,MNUMYR) =
      sum(ActiveDem $crN2L(ActiveDem,MNUMCR),
        RecipeDemands.m(ActiveDem,'PCFout',Period) / CONVFACT_CFPFQ(MNUMYR) / GDP(t)) +
      EMABLK_JPFIN(MNUMYR)$(CO2PLFMM=1) ;

    MPBLK_PPFIN(MNUMCR,MNUMYR) =
      max(MPBLK_PPFIN(MNUMCR,MNUMYR),0.0001) ;

    if(sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR) , QBLK_QPFIN(MNUMCR,MNUMYR))>0,
      MPBLK_PPFIN('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), MPBLK_PPFIN(MNUMCR,MNUMYR)*QBLK_QPFIN(MNUMCR,MNUMYR) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QPFIN(MNUMCR,MNUMYR) ) ;
    );

    if(sum((DomRefReg,RefType,RcpMode)$RecipeBlending(RcpMode,'JTAout'), RECIPEMODE.l(DomRefReg,RefType,RcpMode,Period))>0,
      CONVFACT_CFJFQ(MNUMYR) = sum((DomRefReg,RefType,RcpMode)$RecipeBlending(RcpMode,'JTAout'),
          RECIPEMODE.l(DomRefReg,RefType,RcpMode,Period)*CONVFACT_CFJFK )/
        sum((DomRefReg,RefType,RcpMode)$RecipeBlending(RcpMode,'JTAout'), RECIPEMODE.l(DomRefReg,RefType,RcpMode,Period)) ;
    else
      CONVFACT_CFJFQ(MNUMYR) = CONVFACT_CFJFK ;
    );


* =======================================================
* 10-18-18 IMO price adj adders for pdstr, pjftr, pmgtr
* 10-18-18 for AEO2019 only
*
* CFDSUQ(t)  MMBtu/bbl
* CFJFQ(t)   MMBtu/bbl
* CFMGQ(t)   MMBtu/bbl
*
* adj4IMOpdstr :  adj4IMOdsuGAL *adj4IMOyrPCT(t) *42 /CFDSUQ /GDP('2018')
* adj4IMOpjftr :  adj4IMOjetGAL *adj4IMOyrPCT(t) *42 /CFJFQ  /GDP('2018')
* adj4IMOpmgtr :  adj4IMOmgGAL  *adj4IMOyrPCT(t) *42 /CFMGQ  /GDP('2018')
*
* parameters
*    adj4IMOdsuGAL = 0.16 2018$/gal
*    adj4IMOmgGAL  = 0.08 2018$/gal
*    adj4IMOjetGAL = 0.12 2018$/gal
*
*    adj4IMOyrPCT(t)
*       2020  1.00
*       2021  0.90
*       2022  0.80
*       2023  0.70
*       2024  0.40
*       2025  0.20
*
* HARDCODED, AEO2019
* 2018 $/gal
*   adj4IMOdsuGAL = 0.16 ;
*   adj4IMOmgGAL  = 0.08 ;
*   adj4IMOjetGAL = 0.12 ;

* HARDCODED, remove for AEO2020
* 2018 $/gal
    adj4IMOdsuGAL = 0.00 ;
    adj4IMOmgGAL  = 0.00 ;
    adj4IMOjetGAL = 0.00 ;

    adj4IMOyrPCT(t) = 0.0 ;
    adj4IMOyrPCT('2020') = 1.00 ;
    adj4IMOyrPCT('2021') = 0.90 ;
    adj4IMOyrPCT('2022') = 0.80 ;
    adj4IMOyrPCT('2023') = 0.70 ;
    adj4IMOyrPCT('2024') = 0.40 ;
    adj4IMOyrPCT('2025') = 0.20 ;

* =======================================================







    LFMMOUT_AB32JETCOVER(MNUMYR) = 0;
    LFMMOUT_AB32JETCOVER(MNUMYR)$(AB32_StartYr('JTAout')<=NCNTRL_CURCALYR) = AB32_CoverageFrac('JTAout');

    MPBLK_PJFTR(MNUMCR,MNUMYR) =
      sum(ActiveDem $crN2L(ActiveDem,MNUMCR),
        (RecipeDemands.m(ActiveDem,'JTAout',Period) / CONVFACT_CFJFQ(MNUMYR) / GDP(t)) +
         npv_TotalMarkups(Period,'J','T',ActiveDem)/GDP(t) ) -
         EMABLK_JJFTR(MNUMYR)$(CO2PLFMM=0) ;

* Remove AB-32 markup to get unadjusted price
    MPBLK_PJFTR('09_Pacific',MNUMYR) =
          MPBLK_PJFTR('09_Pacific',MNUMYR) -
            (AB32_CoverageFrac('JTAout')*CA_dmd_shr_JF*AB32_AllowPrice(t)*EMEBLK_EJFTR(MNUMYR)*44/12/1000.0 )$(AB32SW) ;

* =======================================================
* 10-18-18 IMO price adj adders for pdstr, pjftr, pmgtr
* 10-18-18 for AEO2019 only
* 10-18-18 covnert from 2018$/gal to 1987$/MMBtu

    MPBLK_PJFTR(MNUMCR,MNUMYR) =
      sum(ActiveDem $crN2L(ActiveDem,MNUMCR), MPBLK_PJFTR(MNUMCR,MNUMYR) +
         adj4IMOjetGAL *adj4IMOyrPCT(t) *42 /CONVFACT_CFJFQ(MNUMYR) /GDP('2018') ) ;

* 10-18-18 end for PJFTR
* 10-18-18 for AEO2019 only
* =======================================================

    MPBLK_PJFTR(MNUMCR,MNUMYR) =
      max(MPBLK_PJFTR(MNUMCR,MNUMYR),0.0001) ;

    if(sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR) , QBLK_QJFTR(MNUMCR,MNUMYR))>0,
      MPBLK_PJFTR('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), MPBLK_PJFTR(MNUMCR,MNUMYR)*QBLK_QJFTR(MNUMCR,MNUMYR) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QJFTR(MNUMCR,MNUMYR) ) ;
    );

    PMMFTAB_PJF(MNUMCR,MNUMYR) =
       MPBLK_PJFTR(MNUMCR,MNUMYR) * CONVFACT_CFJFQ(MNUMYR) ;

    LFMMOUT_AB32_JF(MNUMYR) =
            (AB32_CoverageFrac('JTAout')*CA_dmd_shr_JF*AB32_AllowPrice(t)*EMEBLK_EJFTR(MNUMYR)*44/12/1000.0 )$(AB32SW) ;

*   Set base price equal to jet fuel price for now since we are not making pure kerosene as a product at the moment
    MPBLK_PKSCM(MNUMCR,MNUMYR) = MPBLK_PJFTR(MNUMCR,MNUMYR) - EMABLK_JJFTR(MNUMYR)$(CO2PLFMM=1) + EMABLK_JKSCM(MNUMYR)$(CO2PLFMM=1) ;
    MPBLK_PKSIN(MNUMCR,MNUMYR) = MPBLK_PJFTR(MNUMCR,MNUMYR) - EMABLK_JJFTR(MNUMYR)$(CO2PLFMM=1) + EMABLK_JKSIN(MNUMYR)$(CO2PLFMM=1) ;
    MPBLK_PKSRS(MNUMCR,MNUMYR) = MPBLK_PJFTR(MNUMCR,MNUMYR) - EMABLK_JJFTR(MNUMYR)$(CO2PLFMM=1) + EMABLK_JKSRS(MNUMYR)$(CO2PLFMM=1) ;

    MPBLK_PKSAS(MNUMCR,MNUMYR)$((QBLK_QKSCM(MNUMCR,MNUMYR)+QBLK_QKSIN(MNUMCR,MNUMYR)+QBLK_QKSRS(MNUMCR,MNUMYR))>0) =
      (QBLK_QKSCM(MNUMCR,MNUMYR) * MPBLK_PKSCM(MNUMCR,MNUMYR) +
       QBLK_QKSIN(MNUMCR,MNUMYR) * MPBLK_PKSIN(MNUMCR,MNUMYR) +
       QBLK_QKSRS(MNUMCR,MNUMYR) * MPBLK_PKSRS(MNUMCR,MNUMYR)) /
      (QBLK_QKSCM(MNUMCR,MNUMYR) + QBLK_QKSIN(MNUMCR,MNUMYR) +
       QBLK_QKSRS(MNUMCR,MNUMYR)) ;

    if(sum(MNUMCR, QBLK_QKSCM(MNUMCR,MNUMYR)+QBLK_QKSIN(MNUMCR,MNUMYR)+QBLK_QKSRS(MNUMCR,MNUMYR))>0,
      MPBLK_PKSAS('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR),
          QBLK_QKSCM(MNUMCR,MNUMYR) * MPBLK_PKSCM(MNUMCR,MNUMYR) +
          QBLK_QKSIN(MNUMCR,MNUMYR) * MPBLK_PKSIN(MNUMCR,MNUMYR) +
          QBLK_QKSRS(MNUMCR,MNUMYR) * MPBLK_PKSRS(MNUMCR,MNUMYR) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR),
          QBLK_QKSCM(MNUMCR,MNUMYR) + QBLK_QKSIN(MNUMCR,MNUMYR) +
          QBLK_QKSRS(MNUMCR,MNUMYR) ) ;
    );


    MPBLK_PRLTR(MNUMCR,MNUMYR) =
      sum(ActiveDem $crN2L(ActiveDem,MNUMCR),
        (RecipeDemands.m(ActiveDem,'N6Iout',Period) / CONVFACT_CFRSQ / GDP(t)) +
         npv_TotalMarkups(Period,'L','T',ActiveDem)/GDP(t) ) +
      EMABLK_JRLTR(MNUMYR)$(CO2PLFMM=1) ;

    MPBLK_PRLTR(MNUMCR,MNUMYR) =
      max(MPBLK_PRLTR(MNUMCR,MNUMYR),0.0001) ;

    if(sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QRLTR(MNUMCR,MNUMYR))>0,
      MPBLK_PRLTR('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), MPBLK_PRLTR(MNUMCR,MNUMYR)*QBLK_QRLTR(MNUMCR,MNUMYR) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QRLTR(MNUMCR,MNUMYR) ) ;
    );

    MPBLK_PRLIN(MNUMCR,MNUMYR) =
      sum(ActiveDem $crN2L(ActiveDem,MNUMCR),
        (RecipeDemands.m(ActiveDem,'N6Iout',Period) / CONVFACT_CFRSQ / GDP(t)) +
        npv_TotalMarkups(Period,'L','I',ActiveDem)/GDP(t) ) +
      EMABLK_JRLIN(MNUMYR)$(CO2PLFMM=1) ;

    MPBLK_PRLIN(MNUMCR,MNUMYR) =
      max(MPBLK_PRLIN(MNUMCR,MNUMYR),0.0001) ;

    if(sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QRLIN(MNUMCR,MNUMYR))>0,
      MPBLK_PRLIN('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), MPBLK_PRLIN(MNUMCR,MNUMYR)*QBLK_QRLIN(MNUMCR,MNUMYR) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QRLIN(MNUMCR,MNUMYR) ) ;
    );

    MPBLK_PRLEL(MNUMCR,MNUMYR) =
      sum(ActiveDem $crN2L(ActiveDem,MNUMCR),
        (RecipeDemands.m(ActiveDem,'N6Iout',Period) / CONVFACT_CFRSQ / GDP(t)) +
         npv_TotalMarkups(Period,'L','U',ActiveDem)/GDP(t) ) +
      EMABLK_JRLEL(MNUMYR)$(CO2PLFMM=1) ;

    MPBLK_PRLEL(MNUMCR,MNUMYR) =
      max(MPBLK_PRLEL(MNUMCR,MNUMYR),0.0001) ;

    if(sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR) , QBLK_QRLEL(MNUMCR,MNUMYR))>0,
      MPBLK_PRLEL('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), MPBLK_PRLEL(MNUMCR,MNUMYR)*QBLK_QRLEL(MNUMCR,MNUMYR) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QRLEL(MNUMCR,MNUMYR) ) ;
    );

    MPBLK_PRLCM(MNUMCR,MNUMYR) = MPBLK_PRLIN(MNUMCR,MNUMYR) - EMABLK_JRLIN(MNUMYR)$(CO2PLFMM=1) + EMABLK_JRLCM(MNUMYR)$(CO2PLFMM=1) ;

    MPBLK_PRLAS(MNUMCR,MNUMYR)$((QBLK_QRLTR(MNUMCR,MNUMYR)+QBLK_QRLIN(MNUMCR,MNUMYR)+QBLK_QRLCM(MNUMCR,MNUMYR)+QBLK_QRLEL(MNUMCR,MNUMYR))>0) =
       (MPBLK_PRLTR(MNUMCR,MNUMYR)*QBLK_QRLTR(MNUMCR,MNUMYR) +
        MPBLK_PRLIN(MNUMCR,MNUMYR)*QBLK_QRLIN(MNUMCR,MNUMYR) +
        MPBLK_PRLCM(MNUMCR,MNUMYR)*QBLK_QRLCM(MNUMCR,MNUMYR) +
        MPBLK_PRLEL(MNUMCR,MNUMYR)*QBLK_QRLEL(MNUMCR,MNUMYR) ) /
       (QBLK_QRLTR(MNUMCR,MNUMYR) + QBLK_QRLIN(MNUMCR,MNUMYR) +
        QBLK_QRLCM(MNUMCR,MNUMYR) + QBLK_QRLEL(MNUMCR,MNUMYR) ) ;


* temp for AEO2022 setting PRHTR = 0.80 * PRLTR
* requires ontext/offtext applied to code right below

    MPBLK_PRHTR(MNUMCR,MNUMYR) = 0.80* MPBLK_PRLTR(MNUMCR,MNUMYR) ;

$ontext
    MPBLK_PRHTR(MNUMCR,MNUMYR) =
      sum(ActiveDem $crN2L(ActiveDem,MNUMCR),
        (RecipeDemands.m(ActiveDem,'N6Bout',Period) / CONVFACT_CFRSQ / GDP(t)) +
         npv_TotalMarkups(Period,'H','T',ActiveDem)/GDP(t) ) +
      EMABLK_JRHTR(MNUMYR)$(CO2PLFMM=1) ;

    MPBLK_PRHTR(MNUMCR,MNUMYR) =
      max(MPBLK_PRHTR(MNUMCR,MNUMYR),0.0001) ;
$offtext

    if(sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QRHTR(MNUMCR,MNUMYR))>0,
      MPBLK_PRHTR('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), MPBLK_PRHTR(MNUMCR,MNUMYR)*QBLK_QRHTR(MNUMCR,MNUMYR) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QRHTR(MNUMCR,MNUMYR) ) ;
    );

    MPBLK_PRHEL(MNUMCR,MNUMYR) =
      sum(ActiveDem $crN2L(ActiveDem,MNUMCR),
        (RecipeDemands.m(ActiveDem,'N6Bout',Period) / CONVFACT_CFRSQ / GDP(t)) +
         npv_TotalMarkups(Period,'H','U',ActiveDem)/GDP(t) ) +
      EMABLK_JRHEL(MNUMYR)$(CO2PLFMM=1) ;

    MPBLK_PRHEL(MNUMCR,MNUMYR) =
      max(MPBLK_PRHEL(MNUMCR,MNUMYR),0.0001) ;

    if(sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QRHEL(MNUMCR,MNUMYR))>0,
      MPBLK_PRHEL('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), MPBLK_PRHEL(MNUMCR,MNUMYR)*QBLK_QRHEL(MNUMCR,MNUMYR) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QRHEL(MNUMCR,MNUMYR) ) ;
    );

    MPBLK_PRHAS(MNUMCR,MNUMYR)$((QBLK_QRHTR(MNUMCR,MNUMYR)+QBLK_QRHEL(MNUMCR,MNUMYR))>0) =
      (MPBLK_PRHTR(MNUMCR,MNUMYR)*QBLK_QRHTR(MNUMCR,MNUMYR) +
       MPBLK_PRHEL(MNUMCR,MNUMYR)*QBLK_QRHEL(MNUMCR,MNUMYR) ) /
      (QBLK_QRHTR(MNUMCR,MNUMYR) + QBLK_QRHEL(MNUMCR,MNUMYR) ) ;

* Filling exports early to subtract from domestic production in calculating conversion factors for demand.  Notice the unglamorous coding
    PMMRPT_QPRDEX('01_Propane',MNUMYR) =
      sum((NGLProduct('LPGout'),ActiveDem), NGLEXPORTS.l('LPGout',ActiveDem,Period) ) / 1000 ;
    PMMRPT_QPRDEX('14_Propylene',MNUMYR) =
      sum((NGLProduct('UC3out'),ActiveDem), NGLEXPORTS.l('UC3out',ActiveDem,Period) ) / 1000 ;
    PMMRPT_QPRDEX('27_Ethane',MNUMYR) =
      sum((NGLProduct('CC2out'),ActiveDem), NGLEXPORTS.l('CC2out',ActiveDem,Period) ) / 1000 ;
    PMMRPT_QPRDEX('28_Isobutane',MNUMYR) =
      sum((NGLProduct('IC4out'),ActiveDem), NGLEXPORTS.l('IC4out',ActiveDem,Period) ) / 1000 ;
    PMMRPT_QPRDEX('15_Normal_butane',MNUMYR) =
      sum((NGLProduct('NC4out'),ActiveDem), NGLEXPORTS.l('NC4out',ActiveDem,Period) ) / 1000 ;
    PMMRPT_QPRDEX('29_Pentanes_plus',MNUMYR) =
      sum((NGLProduct('NATout'),ActiveDem), NGLEXPORTS.l('NATout',ActiveDem,Period) ) / 1000 ;
    PMMRPT_QPRDEX('02_Conventional_gas',MNUMYR) =
      sum((Stream('CFGout'),DomRefReg), EXPORTS.l('CFGout',DomRefReg,Period) ) / 1000 ;
    PMMRPT_QPRDEX('03_Reformulated_gas',MNUMYR) =
      sum((Stream('RFGout'),DomRefReg), EXPORTS.l('RFGout',DomRefReg,Period) ) / 1000 ;
    PMMRPT_QPRDEX('04_Conventional_BOB',MNUMYR) =
      sum((Stream('CBOB'),DomRefReg), EXPORTS.l('CBOB',DomRefReg,Period) ) / 1000 ;
    PMMRPT_QPRDEX('05_Reformulated_BOB',MNUMYR) =
      sum((Stream('RBOB'),DomRefReg), EXPORTS.l('RBOB',DomRefReg,Period) ) / 1000 ;
    PMMRPT_QPRDEX('06_Jet_fuel',MNUMYR) =
      sum((Stream('JTAout'),DomRefReg), EXPORTS.l('JTAout',DomRefReg,Period) ) / 1000 ;
    PMMRPT_QPRDEX('07_Heating_oil',MNUMYR) =
      sum((Stream('N2Hout'),DomRefReg), EXPORTS.l('N2Hout',DomRefReg,Period) ) / 1000 ;
    PMMRPT_QPRDEX('08_Low_sulfur_residual',MNUMYR) =
      sum((Stream('N6Iout'),DomRefReg), EXPORTS.l('N6Iout',DomRefReg,Period) ) / 1000 ;
    PMMRPT_QPRDEX('09_High_sulfur_residual',MNUMYR) =
      sum((Stream('N6Bout'),DomRefReg), EXPORTS.l('N6Bout',DomRefReg,Period) ) / 1000 ;
    PMMRPT_QPRDEX('10_Gas_oil',MNUMYR) =
      sum((Stream('GO3'),DomRefReg), EXPORTS.l('GO3',DomRefReg,Period) ) / 1000 ;
    PMMRPT_QPRDEX('11_Petrochemical_feedstoc',MNUMYR) =
      sum((Stream('PCFout'),DomRefReg), EXPORTS.l('PCFout',DomRefReg,Period) ) / 1000 ;
    PMMRPT_QPRDEX('12_Asphalt',MNUMYR) =
      sum((Stream('ASPHout'),DomRefReg), EXPORTS.l('ASPHout',DomRefReg,Period) ) / 1000 ;
    PMMRPT_QPRDEX('13_Low_sulfur_diesel',MNUMYR) =
      sum((Stream('DSLout'),DomRefReg), EXPORTS.l('DSLout',DomRefReg,Period) ) / 1000 ;
    PMMRPT_QPRDEX('16_Petroleum_coke',MNUMYR) =
      sum((DomRefReg,RefType), COPRODUCTS.l(DomRefReg,RefType,'COKdump',Period) ) / 1000 ;
    PMMRPT_QPRDEX('17_E85',MNUMYR) =
      sum((Stream('E85out'),DomRefReg), EXPORTS.l('E85out',DomRefReg,Period) ) / 1000 ;
    PMMRPT_QPRDEX('18_Aviation_gas',MNUMYR) =
      sum((Stream('AVGout'),DomRefReg), EXPORTS.l('AVGout',DomRefReg,Period) ) / 1000 ;
    PMMRPT_QPRDEX('19_Lubricants',MNUMYR) =
      sum((Stream('LUBout'),DomRefReg), EXPORTS.l('LUBout',DomRefReg,Period) ) / 1000 ;
    PMMRPT_QPRDEX('20_Atmospheric_residuum',MNUMYR) =
      sum((Stream('AR3'),DomRefReg), EXPORTS.l('AR3',DomRefReg,Period) ) / 1000 ;
    PMMRPT_QPRDEX('21_Medium_naphtha',MNUMYR) =
      sum((Stream('MN3'),DomRefReg), EXPORTS.l('MN3',DomRefReg,Period) ) / 1000 ;
    PMMRPT_QPRDEX('22_Methanol',MNUMYR) =
      sum((Stream('MET'),DomRefReg), EXPORTS.l('MET',DomRefReg,Period) ) / 1000 ;
    PMMRPT_QPRDEX('23_ex4',MNUMYR) =
      sum((DomRefReg,RefType), COPRODUCTS.l(DomRefReg,RefType,'GOPout',Period) ) / 1000 ;
    PMMRPT_QPRDEX('24_Ultralow_sulfur_diesel',MNUMYR) =
      sum((Stream('DSUout'),DomRefReg), EXPORTS.l('DSUout',DomRefReg,Period) ) / 1000 ;
    PMMRPT_QPRDEX('25_CARB_diesel',MNUMYR) =
      sum((Stream('CarbDSUout'),DomRefReg), EXPORTS.l('CarbDSUout',DomRefReg,Period) ) / 1000 ;
    PMMRPT_QPRDEX('26_CARB_motor_gasoline',MNUMYR) =
      sum((Stream('CaRBOBout'),DomRefReg), EXPORTS.l('CaRBOBout',DomRefReg,Period) ) / 1000 ;

* clumsily summing to total:
    PMMRPT_QPRDEX('30_Total',MNUMYR) =
          PMMRPT_QPRDEX('01_Propane',MNUMYR) + PMMRPT_QPRDEX('27_Ethane',MNUMYR) +
          PMMRPT_QPRDEX('14_Propylene',MNUMYR) + PMMRPT_QPRDEX('15_Normal_butane',MNUMYR) +
          PMMRPT_QPRDEX('28_Isobutane',MNUMYR) + PMMRPT_QPRDEX('29_Pentanes_plus',MNUMYR) +
          PMMRPT_QPRDEX('02_Conventional_gas',MNUMYR) + PMMRPT_QPRDEX('03_Reformulated_gas',MNUMYR) +
          PMMRPT_QPRDEX('04_Conventional_BOB',MNUMYR) + PMMRPT_QPRDEX('05_Reformulated_BOB',MNUMYR) +
          PMMRPT_QPRDEX('06_Jet_fuel',MNUMYR) + PMMRPT_QPRDEX('07_Heating_oil',MNUMYR) +
          PMMRPT_QPRDEX('08_Low_sulfur_residual',MNUMYR) + PMMRPT_QPRDEX('09_High_sulfur_residual',MNUMYR) +
          PMMRPT_QPRDEX('10_Gas_oil',MNUMYR) + PMMRPT_QPRDEX('11_Petrochemical_feedstoc',MNUMYR) +
          PMMRPT_QPRDEX('12_Asphalt',MNUMYR) + PMMRPT_QPRDEX('13_Low_sulfur_diesel',MNUMYR) +
          PMMRPT_QPRDEX('16_Petroleum_coke',MNUMYR) + PMMRPT_QPRDEX('17_E85',MNUMYR) +
          PMMRPT_QPRDEX('18_Aviation_gas',MNUMYR) + PMMRPT_QPRDEX('19_Lubricants',MNUMYR) +
          PMMRPT_QPRDEX('20_Atmospheric_residuum',MNUMYR) + PMMRPT_QPRDEX('21_Medium_naphtha',MNUMYR) +
          PMMRPT_QPRDEX('24_Ultralow_sulfur_diesel',MNUMYR) + PMMRPT_QPRDEX('25_CARB_diesel',MNUMYR) +
          PMMRPT_QPRDEX('26_CARB_motor_gasoline',MNUMYR) +
          PMMRPT_QPRDEX('22_Methanol',MNUMYR) + PMMRPT_QPRDEX('23_ex4',MNUMYR);

*   SHOULD:  calculate average denaturant (DEN) conversion factor as the model can use either LSR naphthas or NGLs.  For now, using straight NGL factor.
*   Calculate ethanol conversion factor.  Denaturant assumed as stated in preceding line, straight ethanol is 3.539 according to the AER footnote.
*   Notice that we precycle TotalCFG variable to calculate first total ethanol+denaturant, then total CBOB.
    TotalCFG = sum((DomRefReg,RefType),
                  sum(RcpMode$RecipeBlending(RcpMode,'CFGout'), sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
                  sum(RcpMode$RecipeBlending(RcpMode,'CFGout'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'CFG15out'), sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
                  sum(RcpMode$RecipeBlending(RcpMode,'CFG15out'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'RFGout'), sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
                  sum(RcpMode$RecipeBlending(RcpMode,'RFGout'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'RFG15out'), sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
                  sum(RcpMode$RecipeBlending(RcpMode,'RFG15out'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'E85out'), sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
                  sum(RcpMode$RecipeBlending(RcpMode,'E85out'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))
               ) ;

if (TotalCFG>0,
  CONVFACT_CFETQ(MNUMYR) = sum((DomRefReg,RefType),
        sum(RcpMode$RecipeBlending(RcpMode,'CFGout'), CONVFACT_CFPET * sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
        sum(RcpMode$RecipeBlending(RcpMode,'CFGout'), CONVFACT_CFPPQ * ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))+
        sum(RcpMode$RecipeBlending(RcpMode,'CFG15out'), CONVFACT_CFPET * sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
        sum(RcpMode$RecipeBlending(RcpMode,'CFG15out'), CONVFACT_CFPPQ * ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))+
        sum(RcpMode$RecipeBlending(RcpMode,'RFGout'), CONVFACT_CFPET * sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
        sum(RcpMode$RecipeBlending(RcpMode,'RFGout'), CONVFACT_CFPPQ * ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))+
        sum(RcpMode$RecipeBlending(RcpMode,'RFG15out'), CONVFACT_CFPET * sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
        sum(RcpMode$RecipeBlending(RcpMode,'RFG15out'), CONVFACT_CFPPQ * ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))+
        sum(RcpMode$RecipeBlending(RcpMode,'E85out'), CONVFACT_CFPET * sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
        sum(RcpMode$RecipeBlending(RcpMode,'E85out'), CONVFACT_CFPPQ * ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))
     ) / TotalCFG ;
);

*   CFG10+CFG15
    TotalCFG = sum((DomRefReg,RefType),
                  sum(RcpMode$RecipeBlending(RcpMode,'CFGout'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'CBOB',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'CFGout'), sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
                  sum(RcpMode$RecipeBlending(RcpMode,'CFGout'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'CFG15out'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'CBOB',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'CFG15out'), sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
                  sum(RcpMode$RecipeBlending(RcpMode,'CFG15out'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'CFGb16out'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'CBOB',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'CFGb16out'), sum(EthStream('IBA'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))
               ) ;

* CFTGQ is for conventional ("traditional" versus reformulated)
if (TotalCFG>0,
  CONVFACT_CFTGQ(MNUMYR) = sum((DomRefReg,RefType),
        sum(RcpMode$RecipeBlending(RcpMode,'CFGout'), CONVFACT_CFCBOB(MNUMYR) * ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'CBOB',Period))+
        sum(RcpMode$RecipeBlending(RcpMode,'CFGout'), CONVFACT_CFPET * sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
        sum(RcpMode$RecipeBlending(RcpMode,'CFGout'), CONVFACT_CFPPQ * ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))+
        sum(RcpMode$RecipeBlending(RcpMode,'CFG15out'), CONVFACT_CFCBOB(MNUMYR) * ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'CBOB',Period))+
        sum(RcpMode$RecipeBlending(RcpMode,'CFG15out'), CONVFACT_CFPET * sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
        sum(RcpMode$RecipeBlending(RcpMode,'CFG15out'), CONVFACT_CFPPQ * ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))+
        sum(RcpMode$RecipeBlending(RcpMode,'CFGb16out'), CONVFACT_CFCBOB(MNUMYR) * ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'CBOB',Period))+
        sum(RcpMode$RecipeBlending(RcpMode,'CFGb16out'), CONVFACT_CFBIOBUTE(MNUMYR) * sum(EthStream('IBA'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))
     ) / TotalCFG ;
else
  CONVFACT_CFTGQ(MNUMYR) = 5.07 ;
);

*   RFG10+RFG15
    TotalRFG = sum((DomRefReg,RefType),
                  sum(RcpMode$RecipeBlending(RcpMode,'RFGout'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'RBOB',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'RFGout'), sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
                  sum(RcpMode$RecipeBlending(RcpMode,'RFGout'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'RFG15out'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'RBOB',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'RFG15out'), sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
                  sum(RcpMode$RecipeBlending(RcpMode,'RFG15out'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'RFGb16out'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'RBOB',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'RFGb16out'), sum(EthStream('IBA'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))
               ) ;

if (TotalRFG>0,
  CONVFACT_CFRGQ(MNUMYR)$(TotalRFG>0) = sum((DomRefReg,RefType),
        sum(RcpMode$RecipeBlending(RcpMode,'RFGout'), CONVFACT_CFRBOB(MNUMYR) * ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'RBOB',Period))+
        sum(RcpMode$RecipeBlending(RcpMode,'RFGout'), CONVFACT_CFPET * sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
        sum(RcpMode$RecipeBlending(RcpMode,'RFGout'), CONVFACT_CFPPQ * ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))+
        sum(RcpMode$RecipeBlending(RcpMode,'RFG15out'), CONVFACT_CFRBOB(MNUMYR) * ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'RBOB',Period))+
        sum(RcpMode$RecipeBlending(RcpMode,'RFG15out'), CONVFACT_CFPET * sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
        sum(RcpMode$RecipeBlending(RcpMode,'RFG15out'), CONVFACT_CFPPQ * ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))+
        sum(RcpMode$RecipeBlending(RcpMode,'RFGb16out'), CONVFACT_CFRBOB(MNUMYR) * ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'RBOB',Period))+
        sum(RcpMode$RecipeBlending(RcpMode,'RFGb16out'), CONVFACT_CFBIOBUTE(MNUMYR) * sum(EthStream('IBA'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))
     ) / TotalRFG ;
else
  CONVFACT_CFRGQ(MNUMYR) = 5.07 ;
);

*   CaRBOB
    TotalCaRBOB = sum((DomRefReg,RefType),
                  sum(RcpMode$RecipeBlending(RcpMode,'CaRBOBout'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'CaRBOB',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'CaRBOBout'), sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
                  sum(RcpMode$RecipeBlending(RcpMode,'CaRBOBout'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))
               ) ;

if (TotalCaRBOB>0,
  CONVFACT_APICAMG('2_M2',MNUMYR)$(TotalCaRBOB>0) = sum((DomRefReg,RefType),
        sum(RcpMode$RecipeBlending(RcpMode,'CaRBOBout'), CONVFACT_CFCBQ(MNUMYR) * ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'CaRBOB',Period))+
        sum(RcpMode$RecipeBlending(RcpMode,'CaRBOBout'), CONVFACT_CFPET * sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
        sum(RcpMode$RecipeBlending(RcpMode,'CaRBOBout'), CONVFACT_CFPPQ * ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))
     ) / TotalCaRBOB ;
);

    if ((TotalCFG+TotalRFG+TotalCaRBOB)>0,
      CONVFACT_CFMGQ(MNUMYR) =
        ( (CONVFACT_CFTGQ(MNUMYR) * TotalCFG) +
          (CONVFACT_CFRGQ(MNUMYR) * TotalRFG) +
          (CONVFACT_APICAMG('2_M2',MNUMYR) * TotalCaRBOB) ) /
        (TotalCFG + TotalRFG + TotalCaRBOB) ;
    else
      CONVFACT_CFMGQ(MNUMYR) = 5.07 ;
    );





* Note: CaRBOBout only represented as E10

  LFMMOUT_MOTOR_FUEL('1_M4','1_M3',MNUMYR) = sum((DomRefReg,RefType),
                  sum(RcpMode$RecipeBlending(RcpMode,'CFGout'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'CBOB',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'RFGout'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'RBOB',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'CaRBOBout'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'CaRBOB',Period))
     );
  LFMMOUT_MOTOR_FUEL('1_M4','2_M3',MNUMYR) = sum((DomRefReg,RefType),
                  sum(RcpMode$RecipeBlending(RcpMode,'CFGout'), sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
                  sum(RcpMode$RecipeBlending(RcpMode,'RFGout'), sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
                  sum(RcpMode$RecipeBlending(RcpMode,'CaRBOBout'), sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))
     );
  LFMMOUT_MOTOR_FUEL('1_M4','3_M3',MNUMYR) = sum((DomRefReg,RefType),
                  sum(RcpMode$RecipeBlending(RcpMode,'CFGout'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'RFGout'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'CaRBOBout'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))
     );
  LFMMOUT_MOTOR_FUEL('2_M4','1_M3',MNUMYR) = sum((DomRefReg,RefType),
                  sum(RcpMode$RecipeBlending(RcpMode,'CFG15out'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'CBOB',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'RFG15out'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'RBOB',Period))
     );
  LFMMOUT_MOTOR_FUEL('2_M4','2_M3',MNUMYR) = sum((DomRefReg,RefType),
                  sum(RcpMode$RecipeBlending(RcpMode,'CFG15out'), sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
                  sum(RcpMode$RecipeBlending(RcpMode,'RFG15out'), sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))
     );
  LFMMOUT_MOTOR_FUEL('2_M4','3_M3',MNUMYR) = sum((DomRefReg,RefType),
                  sum(RcpMode$RecipeBlending(RcpMode,'CFG15out'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'RFG15out'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))
     );
  LFMMOUT_MOTOR_FUEL('3_M4','1_M3',MNUMYR) = sum((DomRefReg,RefType),
                  sum(RcpMode$RecipeBlending(RcpMode,'E85out'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'CBOB',Period))
     );
  LFMMOUT_MOTOR_FUEL('3_M4','2_M3',MNUMYR) = sum((DomRefReg,RefType),
                  sum(RcpMode$RecipeBlending(RcpMode,'E85out'), sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))
     );
  LFMMOUT_MOTOR_FUEL('3_M4','3_M3',MNUMYR) = sum((DomRefReg,RefType),
                  sum(RcpMode$RecipeBlending(RcpMode,'E85out'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))
     );
  LFMMOUT_MOTOR_FUEL('4_M4','1_M3',MNUMYR) = sum((DomRefReg,RefType),
                  sum(RcpMode$RecipeBlending(RcpMode,'CFGb16out'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'CBOB',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'RFGb16out'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'RBOB',Period))
     );
  LFMMOUT_MOTOR_FUEL('4_M4','2_M3',MNUMYR) = sum((DomRefReg,RefType),
                  sum(RcpMode$RecipeBlending(RcpMode,'CFGb16out'), sum(EthStream('IBA'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
                  sum(RcpMode$RecipeBlending(RcpMode,'RFGb16out'), sum(EthStream('IBA'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))
     );
  LFMMOUT_MOTOR_FUEL('4_M4','3_M3',MNUMYR) = 0 ;

    MPBLK_PMGTR(MNUMCR,MNUMYR) =
      sum(ActiveDem $(crN2L(ActiveDem,MNUMCR) and QMG.l(ActiveDem,Period)>0),
        MGTotal.m(ActiveDem,Period) / npv_CFMGQCD(ActiveDem,Period) / GDP(t) ) -
        EMABLK_JMGTR(MNUMYR)$(CO2PLFMM=0) ;

* =======================================================
* 10-18-18 IMO price adj adders for pdstr, pjftr, pmgtr
* 10-18-18 for AEO2019 only
* 10-18-18 covnert from 2018$/gal to 1987$/MMBtu

    MPBLK_PMGTR(MNUMCR,MNUMYR) =
      sum(ActiveDem $crN2L(ActiveDem,MNUMCR), MPBLK_PMGTR(MNUMCR,MNUMYR) +
         adj4IMOmgGAL *adj4IMOyrPCT(t) *42 /CONVFACT_CFMGQ(MNUMYR) /GDP('2018') ) ;

* 10-18-18 end for PMGTR
* 10-18-18 for AEO2019 only
* =======================================================


    MPBLK_PMGTR(MNUMCR,MNUMYR) = max(MPBLK_PMGTR(MNUMCR,MNUMYR),0.0001) ;

* Need to remove AB32 allowance price from Pacific region, but wait until price of E85 is determined
* because it uses PMGTR and needs to use what transportation sees, which is with the AB32 allowance

    if(sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QMG.l(ActiveDem,Period))>0,
      MPBLK_PMGTR('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), MPBLK_PMGTR(MNUMCR,MNUMYR)*QMG.l(ActiveDem,Period) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QMG.l(ActiveDem,Period) ) ;
    );

*** The TotalDist variables are for calculating domestic consumption conversion factors.  Thus, we
*** subtract out the distillate exports, which is assumed to be restricted to distillate from petroleum.
*** Thus (re-thus), it can be subtracted from the totals only, as petroleum-only is total minus pieces.
    TotalDistAll =
         sum((DomRefReg,RefType,NonCrudeUtil,AllDist)$StreamSpecProd(NonCrudeUtil,AllDist),(ToSPECBLEND.l(DomRefReg,RefType,AllDist,NonCrudeUtil,Period))) -
         PMMRPT_QPRDEX('07_Heating_oil',MNUMYR) -
         PMMRPT_QPRDEX('13_Low_sulfur_diesel',MNUMYR) -
         PMMRPT_QPRDEX('24_Ultralow_sulfur_diesel',MNUMYR);
    TotalDistBio =
         sum((DomRefReg,RefType,BioDist,AllDist)$StreamSpecProd(BioDist,AllDist),(ToSPECBLEND.l(DomRefReg,RefType,AllDist,BioDist,Period)));
    TotalKeroBio =
         sum((DomRefReg,RefType,BioKero,AllDist)$StreamSpecProd(BioKero,AllDist),(ToSPECBLEND.l(DomRefReg,RefType,AllDist,BioKero,Period)));
    TotalDistPet = TotalDistAll - TotalDistBio - TotalKeroBio;

   TotalDistN2H  =
         sum((DomRefReg,RefType,NonCrudeUtil)$StreamSpecProd(NonCrudeUtil,'N2H'),(ToSPECBLEND.l(DomRefReg,RefType,'N2H',NonCrudeUtil,Period))) -
         PMMRPT_QPRDEX('07_Heating_oil',MNUMYR);
   TotalDistDSL  =
         sum((DomRefReg,RefType,NonCrudeUtil)$StreamSpecProd(NonCrudeUtil,'DSL'),(ToSPECBLEND.l(DomRefReg,RefType,'DSL',NonCrudeUtil,Period))) -
         PMMRPT_QPRDEX('13_Low_sulfur_diesel',MNUMYR);
   TotalDistDSU  =
         sum((DomRefReg,RefType,NonCrudeUtil)$StreamSpecProd(NonCrudeUtil,'DSU'),(ToSPECBLEND.l(DomRefReg,RefType,'DSU',NonCrudeUtil,Period))) -
         PMMRPT_QPRDEX('24_Ultralow_sulfur_diesel',MNUMYR);
   TotalDistCarb =
         sum((DomRefReg,RefType,NonCrudeUtil)$StreamSpecProd(NonCrudeUtil,'CarbDSU'),(ToSPECBLEND.l(DomRefReg,RefType,'CarbDSU',NonCrudeUtil,Period)));
   TotalDistNonN2H  =
         sum((DomRefReg,RefType,BioDist)$StreamSpecProd(BioDist,'N2H'),(ToSPECBLEND.l(DomRefReg,RefType,'N2H',BioDist,Period)));
   TotalDistNonDSL  =
         sum((DomRefReg,RefType,BioDist)$StreamSpecProd(BioDist,'DSL'),(ToSPECBLEND.l(DomRefReg,RefType,'DSL',BioDist,Period)));
   TotalDistNonDSU  =
         sum((DomRefReg,RefType,BioDist)$StreamSpecProd(BioDist,'DSU'),(ToSPECBLEND.l(DomRefReg,RefType,'DSU',BioDist,Period)));
   TotalDistNonCarb =
         sum((DomRefReg,RefType,BioDist)$StreamSpecProd(BioDist,'CarbDSU'),(ToSPECBLEND.l(DomRefReg,RefType,'CarbDSU',BioDist,Period)));
   TotalDistBKEN2H  =
         sum((DomRefReg,RefType,BioKero)$StreamSpecProd(BioKero,'N2H'),(ToSPECBLEND.l(DomRefReg,RefType,'N2H',BioKero,Period)));
   TotalDistBKEDSL  =
         sum((DomRefReg,RefType,BioKero)$StreamSpecProd(BioKero,'DSL'),(ToSPECBLEND.l(DomRefReg,RefType,'DSL',BioKero,Period)));
   TotalDistBKEDSU  =
         sum((DomRefReg,RefType,BioKero)$StreamSpecProd(BioKero,'DSU'),(ToSPECBLEND.l(DomRefReg,RefType,'DSU',BioKero,Period)));
   TotalDistBKECarb =
         sum((DomRefReg,RefType,BioKero)$StreamSpecProd(BioKero,'CarbDSU'),(ToSPECBLEND.l(DomRefReg,RefType,'CarbDSU',BioKero,Period)));
   TotalDistPetN2H  = TotalDistN2H  - TotalDistNonN2H  - TotalDistBKEN2H  ;
   TotalDistPetDSL  = TotalDistDSL  - TotalDistNonDSL  - TotalDistBKEDSL  ;
   TotalDistPetDSU  = TotalDistDSU  - TotalDistNonDSU  - TotalDistBKEDSU  ;
   TotalDistPetCarb = TotalDistCarb - TotalDistNonCarb - TotalDistBKECarb ;

    CONVFACT_CFDSQT(MNUMYR) = (TotalDistPetN2H * CONVFACT_CFDSQ +
                               TotalDistPetDSL * CONVFACT_CFDSLQ(MNUMYR) +
                               TotalDistPetDSU * CONVFACT_CFDSUQ(MNUMYR) +
                               TotalDistPetCarb* CONVFACT_CFDSCQ(MNUMYR) +
                               TotalDistBio * CONVFACT_CFBIOD(MNUMYR) +
                               TotalKeroBio * CONVFACT_CFNPQ) / TotalDistAll;

    CONVFACT_CFDSEL(MNUMYR) = CONVFACT_CFDSQT(MNUMYR) ;
    CONVFACT_CFDSRS(MNUMYR) = CONVFACT_CFDSQT(MNUMYR) ;
    CONVFACT_CFDSCM(MNUMYR) = CONVFACT_CFDSQT(MNUMYR) ;
    CONVFACT_CFDSIN(MNUMYR) = CONVFACT_CFDSQT(MNUMYR) ;
    CONVFACT_CFDSTR(MNUMYR) = CONVFACT_CFDSQT(MNUMYR) ;
    QONROAD_CFDSTRHWY(MNUMYR) = CONVFACT_CFDSQT(MNUMYR) ;

    PMMFTAB_PDS(MNUMCR,MNUMYR) = sum(ActiveDem $crN2L(ActiveDem,MNUMCR),
        RecipeDemands.m(ActiveDem,'N2Hout',Period) / GDP(t) ) ;

* Remove AB-32 markup to get unadjusted price       Note that I am using N2Hout for the CoverageFrac
    PMMFTAB_PDS('09_Pacific',MNUMYR) =
          PMMFTAB_PDS('09_Pacific',MNUMYR) -
            (AB32_CoverageFrac('N2Hout')*CA_dmd_shr_DS*AB32_AllowPrice(t)*EMEBLK_EDSTR(MNUMYR)*CONVFACT_CFDSRS(MNUMYR)*44/12/1000.0 )$(AB32SW) ;

    PMMFTAB_PDS(MNUMCR,MNUMYR) = max(PMMFTAB_PDS(MNUMCR,MNUMYR),0.0001) ;

    if(sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), RECIPESUPPLY.l(ActiveDem,'N2Hout',Period))>0,
      PMMFTAB_PDS('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), PMMFTAB_PDS(MNUMCR,MNUMYR)*RECIPESUPPLY.l(ActiveDem,'N2Hout',Period) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), RECIPESUPPLY.l(ActiveDem,'N2Hout',Period) ) ;
    );

    PMMFTAB_PDSL(MNUMCR,MNUMYR) =  sum(ActiveDem $crN2L(ActiveDem,MNUMCR),
        RecipeDemands.m(ActiveDem,'DSLout',Period) / GDP(t)) ;

* Remove AB-32 markup to get unadjusted price
    PMMFTAB_PDSL('09_Pacific',MNUMYR) =
          PMMFTAB_PDSL('09_Pacific',MNUMYR) -
            (AB32_CoverageFrac('DSLout')*CA_dmd_shr_DS*AB32_AllowPrice(t)*EMEBLK_EDSTR(MNUMYR)*CONVFACT_CFDSLQ(MNUMYR)*44/12/1000.0 )$(AB32SW) ;

    PMMFTAB_PDSL(MNUMCR,MNUMYR) =  max(PMMFTAB_PDSL(MNUMCR,MNUMYR),0.0001) ;

    if(sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), RECIPESUPPLY.l(ActiveDem,'DSLout',Period))>0,
      PMMFTAB_PDSL('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), PMMFTAB_PDSL(MNUMCR,MNUMYR)*RECIPESUPPLY.l(ActiveDem,'DSLout',Period) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), RECIPESUPPLY.l(ActiveDem,'DSLout',Period) ) ;
    );

    PMMFTAB_PDSU(MNUMCR,MNUMYR) = sum(ActiveDem $crN2L(ActiveDem,MNUMCR),
        RecipeDemands.m(ActiveDem,'DSUout',Period) / GDP(t)) ;

* Remove AB-32 markup to get unadjusted price
    PMMFTAB_PDSU('09_Pacific',MNUMYR) =
          PMMFTAB_PDSU('09_Pacific',MNUMYR) -
            (AB32_CoverageFrac('DSUout')*CA_dmd_shr_DS*AB32_AllowPrice(t)*EMEBLK_EDSTR(MNUMYR)*CONVFACT_CFDSUQ(MNUMYR)*44/12/1000.0 )$(AB32SW) ;

    PMMFTAB_PDSU(MNUMCR,MNUMYR) = max(PMMFTAB_PDSU(MNUMCR,MNUMYR),0.0001) ;

    if(sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), RECIPESUPPLY.l(ActiveDem,'DSUout',Period))>0,
      PMMFTAB_PDSU('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), PMMFTAB_PDSU(MNUMCR,MNUMYR)*RECIPESUPPLY.l(ActiveDem,'DSUout',Period) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), RECIPESUPPLY.l(ActiveDem,'DSUout',Period) ) ;
    );

    PMMFTAB_DSCSHR(MNUMCR,MNUMYR) =
      sum(ActiveDem$crN2L(ActiveDem,MNUMCR), CarbDSUShare('CarbDSUout',ActiveDem) ) ;
    PMMFTAB_DSCSHR('11_United_States',MNUMYR)$(QBLK_QDSTR('11_United_States',MNUMYR)>0) =
      sum(MNUMCR$(ord(MNUMCR)<10), PMMFTAB_DSCSHR(MNUMCR,MNUMYR)* QBLK_QDSTR(MNUMCR,MNUMYR) ) /
      QBLK_QDSTR('11_United_States',MNUMYR) ;

    PMMFTAB_PDSCRB(MNUMCR,MNUMYR) = sum(ActiveDem $crN2L(ActiveDem,MNUMCR),
        RecipeDemands.m(ActiveDem,'CarbDSUout',Period) / GDP(t)) ;

* Remove AB-32 markup to get unadjusted price
    PMMFTAB_PDSCRB('09_Pacific',MNUMYR) =
          PMMFTAB_PDSCRB('09_Pacific',MNUMYR) -
            (AB32_CoverageFrac('CarbDSUout')*CA_dmd_shr_DS*AB32_AllowPrice(t)*EMEBLK_EDSTR(MNUMYR)*CONVFACT_CFDSCQ(MNUMYR)*44/12/1000.0 )$(AB32SW) ;

    LFMMOUT_AB32_DS(MNUMYR) =
            (AB32_CoverageFrac('CarbDSUout')*CA_dmd_shr_DS*AB32_AllowPrice(t)*EMEBLK_EDSTR(MNUMYR)*44/12/1000.0 )$(AB32SW) ;

* use distillate for kerosene since it is not modeled in LFMM
    LFMMOUT_AB32_KS(MNUMYR) =
            (AB32_CoverageFrac('N2Hout')*CA_dmd_shr_DS*AB32_AllowPrice(t)*EMEBLK_EDSTR(MNUMYR)*44/12/1000.0 )$(AB32SW) ;

    PMMFTAB_PDSCRB(MNUMCR,MNUMYR) = max(PMMFTAB_PDSCRB(MNUMCR,MNUMYR),0.0001) ;

    if(sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), RECIPESUPPLY.l(ActiveDem,'CarbDSUout',Period))>0,
      PMMFTAB_PDSCRB('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), PMMFTAB_PDSCRB(MNUMCR,MNUMYR)*RECIPESUPPLY.l(ActiveDem,'CarbDSUout',Period) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), RECIPESUPPLY.l(ActiveDem,'CarbDSUout',Period) ) ;
    );

*   On-road distillate price, transportation sector
    PONROAD_PDSTRHWY(MNUMCR,MNUMYR) =
       sum(ActiveDem$crN2L(ActiveDem,MNUMCR),
          PMMFTAB_PDS(MNUMCR,MNUMYR)*TRHWYPCT_N2H(t) / CONVFACT_CFDSRS(MNUMYR) +
          PMMFTAB_PDSL(MNUMCR,MNUMYR)*TRHWYPCT_DSL(t) / CONVFACT_CFDSLQ(MNUMYR) +
          PMMFTAB_PDSU(MNUMCR,MNUMYR)*TRHWYPCT_DSU(t)*(1.0-CarbDSUShare('CarbDSUout',ActiveDem)) / CONVFACT_CFDSUQ(MNUMYR) +
          PMMFTAB_PDSCRB(MNUMCR,MNUMYR)*TRHWYPCT_DSU(t)*CarbDSUShare('CarbDSUout',ActiveDem) / CONVFACT_CFDSCQ(MNUMYR) ) +
       sum(ActiveDem$crN2L(ActiveDem,MNUMCR),
          npv_TotalMarkups(Period,'D','T',ActiveDem)/GDP(t) ) -
       EMABLK_JDSTR(MNUMYR)$(CO2PLFMM=0) ;

* =======================================================
* 10-18-18 IMO price adj adders for pdstr, pjftr, pmgtr
* 10-18-18 for AEO2019 only
* 10-18-18 covnert from 2018$/gal to 1987$/MMBtu

    PONROAD_PDSTRHWY(MNUMCR,MNUMYR) =
      sum(ActiveDem $crN2L(ActiveDem,MNUMCR), PONROAD_PDSTRHWY(MNUMCR,MNUMYR) +
         adj4IMOdsuGAL *adj4IMOyrPCT(t) *42 /CONVFACT_CFDSUQ(MNUMYR) /GDP('2018') ) ;

* 10-18-18 end for PDSTRHWY
* 10-18-18 for AEO2019 only
* =======================================================


    if(sum(MNUMCR, QONROAD_QDSTRHWY(MNUMCR,MNUMYR))>0,
      PONROAD_PDSTRHWY('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), PONROAD_PDSTRHWY(MNUMCR,MNUMYR)*QONROAD_QDSTRHWY(MNUMCR,MNUMYR) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QONROAD_QDSTRHWY(MNUMCR,MNUMYR) ) ;
    );

    MPBLK_PDSTR(MNUMCR,MNUMYR) =
       sum(ActiveDem $crN2L(ActiveDem,MNUMCR),
          PMMFTAB_PDS(MNUMCR,MNUMYR)*TRPCT_N2H(t) / CONVFACT_CFDSRS(MNUMYR) +
          PMMFTAB_PDSL(MNUMCR,MNUMYR)*TRPCT_DSL(t) / CONVFACT_CFDSLQ(MNUMYR) +
          PMMFTAB_PDSU(MNUMCR,MNUMYR)*TRPCT_DSU(t)*(1.0-CarbDSUShare('CarbDSUout',ActiveDem)) / CONVFACT_CFDSUQ(MNUMYR) +
          PMMFTAB_PDSCRB(MNUMCR,MNUMYR)*TRPCT_DSU(t)*CarbDSUShare('CarbDSUout',ActiveDem) / CONVFACT_CFDSCQ(MNUMYR) +
          npv_TotalMarkups(Period,'D','T',ActiveDem)/GDP(t) ) -
       EMABLK_JDSTR(MNUMYR)$(CO2PLFMM=0) ;

* =======================================================
* 10-18-18 IMO price adj adders for pdstr, pjftr, pmgtr
* 10-18-18 for AEO2019 only
* 10-18-18 covnert from 2018$/gal to 1987$/MMBtu

    MPBLK_PDSTR(MNUMCR,MNUMYR) =
      sum(ActiveDem $crN2L(ActiveDem,MNUMCR), MPBLK_PDSTR(MNUMCR,MNUMYR) +
         adj4IMOdsuGAL *adj4IMOyrPCT(t) *42 /CONVFACT_CFDSUQ(MNUMYR) /GDP('2018') ) ;

* 10-18-18 end for PDSTR
* 10-18-18 for AEO2019 only
* =======================================================


    if(sum(MNUMCR, QBLK_QDSTR(MNUMCR,MNUMYR))>0,
      MPBLK_PDSTR('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QDSTR(MNUMCR,MNUMYR) * MPBLK_PDSTR(MNUMCR,MNUMYR) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QDSTR(MNUMCR,MNUMYR) ) ;
    );

    MPBLK_PDSCM(MNUMCR,MNUMYR) =
         sum(ActiveDem $crN2L(ActiveDem,MNUMCR),
             PMMFTAB_PDS(MNUMCR,MNUMYR)*CMPCT_N2H(t) / CONVFACT_CFDSRS(MNUMYR) +
             PMMFTAB_PDSL(MNUMCR,MNUMYR)*CMPCT_DSL(t) / CONVFACT_CFDSLQ(MNUMYR) +
             PMMFTAB_PDSU(MNUMCR,MNUMYR)*CMPCT_DSU(t) / CONVFACT_CFDSUQ(MNUMYR) ) +
         sum(ActiveDem$crN2L(ActiveDem,MNUMCR), npv_TotalMarkups(Period,'D','C',ActiveDem)/GDP(t) ) -
         EMABLK_JDSTR(MNUMYR) + EMABLK_JDSCM(MNUMYR)$(CO2PLFMM=1) ;

    if(sum(MNUMCR, QBLK_QDSCM(MNUMCR,MNUMYR))>0,
      MPBLK_PDSCM('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QDSCM(MNUMCR,MNUMYR) * MPBLK_PDSCM(MNUMCR,MNUMYR) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QDSCM(MNUMCR,MNUMYR) ) ;
    );

    MPBLK_PDSIN(MNUMCR,MNUMYR) =
         sum(ActiveDem $crN2L(ActiveDem,MNUMCR),
            PMMFTAB_PDS(MNUMCR,MNUMYR)*INPCT_N2H(t) / CONVFACT_CFDSRS(MNUMYR) +
            PMMFTAB_PDSL(MNUMCR,MNUMYR)*INPCT_DSL(t) / CONVFACT_CFDSLQ(MNUMYR) +
            PMMFTAB_PDSU(MNUMCR,MNUMYR)*INPCT_DSU(t) / CONVFACT_CFDSUQ(MNUMYR) ) +
         sum(ActiveDem$crN2L(ActiveDem,MNUMCR), npv_TotalMarkups(Period,'D','I',ActiveDem)/GDP(t) ) -
         EMABLK_JDSTR(MNUMYR) + EMABLK_JDSIN(MNUMYR)$(CO2PLFMM=1) ;

    if(sum(MNUMCR, QBLK_QDSIN(MNUMCR,MNUMYR))>0,
      MPBLK_PDSIN('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QDSIN(MNUMCR,MNUMYR) * MPBLK_PDSIN(MNUMCR,MNUMYR) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QDSIN(MNUMCR,MNUMYR) ) ;
    );

*   DONE 3/29/17: ADD IN DSU DEMANDS FOR RESIDENTIAL HEATING OIL!!!

    MPBLK_PDSRS(MNUMCR,MNUMYR) =
           sum(ActiveDem $crN2L(ActiveDem,MNUMCR),
           ((PMMFTAB_PDS(MNUMCR,MNUMYR)  * RECIPESUPPLY.l(ActiveDem,'N2Hout',Period)) +
            (PMMFTAB_PDSU(MNUMCR,MNUMYR) * ULSD_N2H_CD(ActiveDem,t)/365) )  /
           ( RECIPESUPPLY.l(ActiveDem,'N2Hout',Period) + ULSD_N2H_CD(ActiveDem,t)/365 )) / CONVFACT_CFDSRS(MNUMYR) +
         sum(ActiveDem$crN2L(ActiveDem,MNUMCR), npv_TotalMarkups(Period,'D','R',ActiveDem)/GDP(t) ) -
         EMABLK_JDSTR(MNUMYR) + EMABLK_JDSRS(MNUMYR)$(CO2PLFMM=1) ;

*   MPBLK_PDSRS(MNUMCR,MNUMYR) =
*         PMMFTAB_PDS(MNUMCR,MNUMYR) / CONVFACT_CFDSRS(MNUMYR) +
*         sum(ActiveDem$crN2L(ActiveDem,MNUMCR), npv_TotalMarkups(Period,'D','R',ActiveDem)/GDP(t) ) -
*         EMABLK_JDSTR(MNUMYR) + EMABLK_JDSRS(MNUMYR)$(CO2PLFMM=1) ;

    if(sum(MNUMCR, QBLK_QDSRS(MNUMCR,MNUMYR))>0,
      MPBLK_PDSRS('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QDSRS(MNUMCR,MNUMYR) * MPBLK_PDSRS(MNUMCR,MNUMYR) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QDSRS(MNUMCR,MNUMYR) ) ;
    );

    MPBLK_PDSEL(MNUMCR,MNUMYR) =
          PMMFTAB_PDS(MNUMCR,MNUMYR) / CONVFACT_CFDSRS(MNUMYR) +
          sum(ActiveDem$crN2L(ActiveDem,MNUMCR), npv_TotalMarkups(Period,'D','U',ActiveDem)/GDP(t) ) -
          EMABLK_JDSTR(MNUMYR) + EMABLK_JDSEL(MNUMYR)$(CO2PLFMM=1) ;

    MPBLK_PDSEL(MNUMCR,MNUMYR) = max(MPBLK_PDSEL(MNUMCR,MNUMYR),0.0001) ;

    if(sum(MNUMCR, QBLK_QDSEL(MNUMCR,MNUMYR))>0,
      MPBLK_PDSEL('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QDSEL(MNUMCR,MNUMYR) * MPBLK_PDSEL(MNUMCR,MNUMYR) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QDSEL(MNUMCR,MNUMYR) ) ;
    );

    MPBLK_PDSAS(MNUMCR,MNUMYR)$((QBLK_QDSRS(MNUMCR,MNUMYR)+QBLK_QDSCM(MNUMCR,MNUMYR)+QBLK_QDSTR(MNUMCR,MNUMYR)+ QBLK_QDSIN(MNUMCR,MNUMYR)+QBLK_QDSEL(MNUMCR,MNUMYR))>0) =
        (QBLK_QDSRS(MNUMCR,MNUMYR) * MPBLK_PDSRS(MNUMCR,MNUMYR) +
         QBLK_QDSCM(MNUMCR,MNUMYR) * MPBLK_PDSCM(MNUMCR,MNUMYR) +
         QBLK_QDSTR(MNUMCR,MNUMYR) * MPBLK_PDSTR(MNUMCR,MNUMYR) +
         QBLK_QDSIN(MNUMCR,MNUMYR) * MPBLK_PDSIN(MNUMCR,MNUMYR) +
         QBLK_QDSEL(MNUMCR,MNUMYR) * MPBLK_PDSEL(MNUMCR,MNUMYR)) /
        (QBLK_QDSRS(MNUMCR,MNUMYR) + QBLK_QDSCM(MNUMCR,MNUMYR) +
         QBLK_QDSTR(MNUMCR,MNUMYR) + QBLK_QDSIN(MNUMCR,MNUMYR) +
         QBLK_QDSEL(MNUMCR,MNUMYR)) ;

    if(sum(MNUMCR, QBLK_QDSRS(MNUMCR,MNUMYR)+QBLK_QDSCM(MNUMCR,MNUMYR)+QBLK_QDSTR(MNUMCR,MNUMYR)+ QBLK_QDSIN(MNUMCR,MNUMYR)+QBLK_QDSEL(MNUMCR,MNUMYR))>0,
      MPBLK_PDSAS('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR),
          QBLK_QDSRS(MNUMCR,MNUMYR) * MPBLK_PDSRS(MNUMCR,MNUMYR) +
          QBLK_QDSCM(MNUMCR,MNUMYR) * MPBLK_PDSCM(MNUMCR,MNUMYR) +
          QBLK_QDSTR(MNUMCR,MNUMYR) * MPBLK_PDSTR(MNUMCR,MNUMYR) +
          QBLK_QDSIN(MNUMCR,MNUMYR) * MPBLK_PDSIN(MNUMCR,MNUMYR) +
          QBLK_QDSEL(MNUMCR,MNUMYR) * MPBLK_PDSEL(MNUMCR,MNUMYR) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR),
          QBLK_QDSRS(MNUMCR,MNUMYR) + QBLK_QDSCM(MNUMCR,MNUMYR) +
          QBLK_QDSTR(MNUMCR,MNUMYR) + QBLK_QDSIN(MNUMCR,MNUMYR) +
          QBLK_QDSEL(MNUMCR,MNUMYR) ) ;
    );


    MPBLK_POTIN(MNUMCR,MNUMYR)=  sum(ActiveDem $crN2L(ActiveDem,MNUMCR),
        RecipeDemands.m(ActiveDem,'LUBout',Period) / CONVFACT_CFLUQ / GDP(t)) +
        EMABLK_JOTIN(MNUMYR)$(CO2PLFMM=1) ;

    MPBLK_POTIN(MNUMCR,MNUMYR) = max(MPBLK_POTIN(MNUMCR,MNUMYR),0.0001) ;

    if(sum(MNUMCR, QBLK_QOTIN(MNUMCR,MNUMYR))>0,
      MPBLK_POTIN('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QOTIN(MNUMCR,MNUMYR) * MPBLK_POTIN(MNUMCR,MNUMYR) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QOTIN(MNUMCR,MNUMYR) ) ;
    );

    PMORE_PLUIN(MNUMCR,MNUMYR)= MPBLK_POTIN(MNUMCR,MNUMYR) - EMABLK_JOTIN(MNUMYR)$(CO2PLFMM=1) + EMABLK_JLUIN(MNUMYR)$(CO2PLFMM=1) ;


    MPBLK_POTTR(MNUMCR,MNUMYR) = sum(ActiveDem $crN2L(ActiveDem,MNUMCR),
        RecipeDemands.m(ActiveDem,'AVGout',Period) / CONVFACT_CFAVQ / GDP(t)) +
        EMABLK_JOTTR(MNUMYR)$(CO2PLFMM=1) ;

    MPBLK_POTTR(MNUMCR,MNUMYR) = max(MPBLK_POTTR(MNUMCR,MNUMYR),0.0001) ;

    if(sum(MNUMCR, QBLK_QOTTR(MNUMCR,MNUMYR))>0,
      MPBLK_POTTR('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QOTTR(MNUMCR,MNUMYR) * MPBLK_POTTR(MNUMCR,MNUMYR) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QOTTR(MNUMCR,MNUMYR) ) ;
    );

    MPBLK_POTAS(MNUMCR,MNUMYR)$((QBLK_QOTIN(MNUMCR,MNUMYR) + QBLK_QOTTR(MNUMCR,MNUMYR))>0) =
        (QBLK_QOTIN(MNUMCR,MNUMYR) * MPBLK_POTIN(MNUMCR,MNUMYR) +
         QBLK_QOTTR(MNUMCR,MNUMYR) * MPBLK_POTTR(MNUMCR,MNUMYR) ) /
        (QBLK_QOTIN(MNUMCR,MNUMYR) + QBLK_QOTTR(MNUMCR,MNUMYR)) ;


    if(sum(MNUMCR, QBLK_QOTIN(MNUMCR,MNUMYR) + QBLK_QOTTR(MNUMCR,MNUMYR))>0,
      MPBLK_POTAS('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR),
          QBLK_QOTIN(MNUMCR,MNUMYR) * MPBLK_POTIN(MNUMCR,MNUMYR) +
          QBLK_QOTTR(MNUMCR,MNUMYR) * MPBLK_POTTR(MNUMCR,MNUMYR) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR),
          QBLK_QOTIN(MNUMCR,MNUMYR) + QBLK_QOTTR(MNUMCR,MNUMYR) ) ;
    );

*   E85

    PMMRPT_RFETHE85(MNUMPR,MNUMYR) =
(sum((DomRefReg,RefType)$RefReg2MNUMPR(MNUMPR,DomRefReg),
sum(RcpMode$RecipeBlending(RcpMode,'E85out'), sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))  +
sum(RcpMode$RecipeBlending(RcpMode,'E85out'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))    ))/1000.;

    PMMRPT_RFETHE85('10_MNUMPR',MNUMYR) =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMRPT_RFETHE85(MNUMPR,MNUMYR) ) ;

* Set E85 fuel availibility to the level at the chosen E85 demand step

IF ( NCNTRL_CURCALYR > E85availLYR,

    Loop ((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR),
       IF ( (QE85.l(ActiveDem,Period) <= 0.0),
          TRANREP_E85AVAIL(MNUMCR,MNUMYR) = TRANREP_E85AVAIL(MNUMCR,MNUMYR-1);
       );
    );

    Loop ((E85_Steps,ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR),
       IF (E85STP.l(E85_Steps,ActiveDem,Period) > (0.000001 + E85STP.lo(E85_Steps,ActiveDem,Period)),
          Max_E85_STP(ActiveDem,Period) = ord(E85_Steps);
          TRANREP_E85AVAIL(MNUMCR,MNUMYR) = max(A_E85STP(E85_Steps,ActiveDem,Period),TRANREP_E85AVAIL(MNUMCR,MNUMYR-1)) ;
       );
    );
);

TempSum = 0 ;
    Loop ((E85_Steps,ActiveDem),
          TempSum = TempSum +
          E85STP.l(E85_Steps,ActiveDem,Period)*(P_E85_IC_t(E85_Steps,ActiveDem,t)+P_E85_SC_t(E85_Steps,ActiveDem,t)) * CONVFACT_CFE85Q(MNUMYR) ;
    );


    PMMFTAB_E85ICCREDIT(MNUMYR) =
      TempSum /
        (npv_PetMGFrac_noETH(Period)*sum(ActiveDem,
             0.90*QMG10.l(ActiveDem,Period) +
             0.85*QMG15.l(ActiveDem,Period) +
             0.84*QMGb16.l(ActiveDem,Period) +
             abs(RecipeBlending('RCP_E85a','CBOB'))*QE85.l(ActiveDem,Period) ) +
          npv_PetDSFrac(Period)*sum(ActiveDem, CDSUPPLY.l(ActiveDem,'DSUout',Period) + CDSUPPLY.l(ActiveDem,'CarbDSUout',Period) ) );


*   Calculate PETTR required to achieve LFMM MG / E85 Shares
    T_QMGTR(MNUMCR,MNUMYR) =  sum(CenDiv $crN2L(CenDiv,MNUMCR),
      (QMG10.l(CenDiv,Period) + QMG15.l(CenDiv,Period) + QMGb16.l(CenDiv,Period)) * npv_CFMGQCD(CenDiv,Period) );

    T_QETTR_SHR(MNUMCR,MNUMYR) = sum(CenDiv $crN2L(CenDiv,MNUMCR),
      (npv_BTUDemand(CenDiv,Period) - T_QMGTR(MNUMCR,MNUMYR)) / (QFFV(CenDiv,Period)/ 0.365 ));
    T_QETTR_SHR(MNUMCR,MNUMYR) = MIN(T_QETTR_SHR(MNUMCR,MNUMYR) , 0.9999);
    T_QETTR_SHR(MNUMCR,MNUMYR) = MAX(T_QETTR_SHR(MNUMCR,MNUMYR) , 0.0005);



    Loop ((MNUMCR,CenDiv)$crN2L(CenDiv,MNUMCR),
       IF (NCNTRL_CURCALYR > HISTLYR,
         tempPMGTR(CenDiv) =  MPBLK_PMGTR(MNUMCR,MNUMYR) +  PMGTRadjSTEO(CenDiv);
       ELSE
         tempPMGTR(CenDiv) = MPBLK_PMGTR(MNUMCR,MNUMYR);
       );
       IF (NCNTRL_CURCALYR > (STEOLYR + STEO_NumPhaseoutYrs),
         PMGTRadjSTEO(CenDiv)= 0;
       ELSE
         PMGTRadjSTEO(CenDiv)= PMGTRadjSTEO(CenDiv) ;
       );
       GASPRR = EXP( (tempPMGTR(CenDiv) + EMABLK_JMGTR(MNUMYR)$(CO2PLFMM=0)) * TRANREP_FCLOGIT4 * 0.125 );
       Upper_PRC = (tempPMGTR(CenDiv) + EMABLK_JMGTR(MNUMYR)$(CO2PLFMM=0)) * 1.5;
       ALTPRR = EXP(sum(MNUMC2$MNUMC2_2_MNUMCR(MNUMC2,MNUMCR),TRANREP_FCLOGIT0(MNUMC2)) +  0.125 * Upper_PRC * TRANREP_FCLOGIT1 - (TRANREP_FCLOGIT2 * EXP(TRANREP_E85AVAIL(MNUMCR,MNUMYR) * TRANREP_FCLOGIT3)));
       Lower_SHR = ALTPRR / (GASPRR + ALTPRR);
       Lower_PRC = (tempPMGTR(CenDiv) + EMABLK_JMGTR(MNUMYR)$(CO2PLFMM=0)) * 0.01;
       ALTPRR = EXP(sum(MNUMC2$MNUMC2_2_MNUMCR(MNUMC2,MNUMCR),TRANREP_FCLOGIT0(MNUMC2)) +  0.125 * Lower_PRC * TRANREP_FCLOGIT1 - (TRANREP_FCLOGIT2 * EXP(TRANREP_E85AVAIL(MNUMCR,MNUMYR) * TRANREP_FCLOGIT3)));
       Upper_SHR = ALTPRR / (GASPRR + ALTPRR);
          I_Trial = 0;
          Test_Trial = 0;
          Max_Trial = 100;
       IF (T_QETTR_SHR(MNUMCR,MNUMYR) < Lower_SHR,
          MPBLK_PETTR(MNUMCR,MNUMYR) = Upper_PRC - EMABLK_JETTR(MNUMYR)$(CO2PLFMM=0);
       ELSEIF (T_QETTR_SHR(MNUMCR,MNUMYR) > Upper_SHR),
          MPBLK_PETTR(MNUMCR,MNUMYR) = Lower_PRC - EMABLK_JETTR(MNUMYR)$(CO2PLFMM=0);
       ELSE
          WHILE ((I_Trial <= Max_Trial) and (Test_Trial = 0),
             I_Trial = I_Trial + 1;
             Trial_PRC = (Upper_PRC + Lower_PRC) / 2.0;
             ALTPRR = EXP(sum(MNUMC2$MNUMC2_2_MNUMCR(MNUMC2,MNUMCR),TRANREP_FCLOGIT0(MNUMC2)) +  0.125 * Trial_PRC * TRANREP_FCLOGIT1 - (TRANREP_FCLOGIT2 * EXP(TRANREP_E85AVAIL(MNUMCR,MNUMYR) * TRANREP_FCLOGIT3)));
             Trial_SHR = ALTPRR / (GASPRR + ALTPRR);
             IF ((Trial_SHR >= T_QETTR_SHR(MNUMCR,MNUMYR) - 0.0001) and (Trial_SHR <= (T_QETTR_SHR(MNUMCR,MNUMYR) + 0.0001)),
                Test_Trial = 1;
             ELSEIF (Trial_SHR > T_QETTR_SHR(MNUMCR,MNUMYR)),
                Lower_PRC = Trial_PRC;
             ELSE
                Upper_PRC = Trial_PRC;
             );
          );
          MPBLK_PETTR(MNUMCR,MNUMYR) = Trial_PRC - EMABLK_JETTR(MNUMYR)$(CO2PLFMM=0);
       );
    );


  MG_Share_US = sum((DomRefReg,RefType),
                 (sum(RcpMode$RecipeBlending(RcpMode,'CFGout'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'CBOB',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'CFG15out'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'CBOB',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'CFGb16out'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'CBOB',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'RFGout'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'RBOB',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'RFG15out'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'RBOB',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'RFGb16out'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'RBOB',Period))+
                  sum(RcpMode$RecipeBlending(RcpMode,'CaRBOBout'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'CaRBOB',Period)))) /
                  sum(ActiveDem, QMG.l(ActiveDem,Period) ) ;
*                       sum((DomRefReg,RefType),
*                (sum(RcpMode$RecipeBlending(RcpMode,'CFGout'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'CBOB',Period))+
*                 sum(RcpMode$RecipeBlending(RcpMode,'RFGout'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'RBOB',Period))+
*                 sum(RcpMode$RecipeBlending(RcpMode,'CaRBOBout'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'CaRBOB',Period))+
*                 sum(RcpMode$RecipeBlending(RcpMode,'CFGout'), sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
*                 sum(RcpMode$RecipeBlending(RcpMode,'RFGout'), sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
*                 sum(RcpMode$RecipeBlending(RcpMode,'CaRBOBout'), sum(EthStream, ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period)))+
*                 sum(RcpMode$RecipeBlending(RcpMode,'CFGout'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))+
*                 sum(RcpMode$RecipeBlending(RcpMode,'RFGout'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period))+
*                 sum(RcpMode$RecipeBlending(RcpMode,'CaRBOBout'), ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,'DEN',Period)))
*    );

*  Do this AFTER the E85 pricing algorithm (which means we need to subtract from E85 price, too)!
* Remove AB-32 markup to get unadjusted price     Note that here I am using CFGout for the CoverageFrac
    MPBLK_PMGTR('09_Pacific',MNUMYR) =
          MPBLK_PMGTR('09_Pacific',MNUMYR) -
            (AB32_CoverageFrac('CFGout')*CA_dmd_shr_MG*AB32_AllowPrice(t)*EMEBLK_EMGTR(MNUMYR)*MG_Share_US*44/12/1000.0 )$(AB32SW) ;

    MPBLK_PETTR('09_Pacific',MNUMYR) =
          MPBLK_PETTR('09_Pacific',MNUMYR) -
            (AB32_CoverageFrac('E85out')*CA_dmd_shr_E85*AB32_AllowPrice(t)*EMEBLK_EMGTR(MNUMYR)*MG_BTUShare_E85(t)*44/12/1000.0 )$(AB32SW) ;

    MPBLK_PMGTR(MNUMCR,MNUMYR) = max(MPBLK_PMGTR(MNUMCR,MNUMYR),0.0001) ;
    MPBLK_PETTR(MNUMCR,MNUMYR) = max(MPBLK_PETTR(MNUMCR,MNUMYR),0.0001) ;

    if(sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QMG.l(ActiveDem,Period))>0,
      MPBLK_PMGTR('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), MPBLK_PMGTR(MNUMCR,MNUMYR)*QMG.l(ActiveDem,Period) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QMG.l(ActiveDem,Period) ) ;
    );

    if(sum((MNUMCR,ActiveDem)$crN2L(ActiveDem,MNUMCR), QE85.l(ActiveDem,Period))>0,
      MPBLK_PETTR('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QE85.l(ActiveDem,Period) * MPBLK_PETTR(MNUMCR,MNUMYR) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QE85.l(ActiveDem,Period) ) ;
    );

    LFMMOUT_AB32_MG(MNUMYR) =
            (AB32_CoverageFrac('CFGout')*CA_dmd_shr_MG*AB32_AllowPrice(t)*EMEBLK_EMGTR(MNUMYR)*MG_Share_US*44/12/1000.0 )$(AB32SW) ;

    LFMMOUT_AB32_ET(MNUMYR) =
            (AB32_CoverageFrac('E85out')*CA_dmd_shr_E85*AB32_AllowPrice(t)*EMEBLK_EMGTR(MNUMYR)*MG_BTUShare_E85(t)*44/12/1000.0 )$(AB32SW) ;

* Re-adjust other sector prices motor gasoline
    MPBLK_PMGCM(MNUMCR,MNUMYR) = MPBLK_PMGTR(MNUMCR,MNUMYR) - EMABLK_JMGTR(MNUMYR)$(CO2PLFMM=1) + EMABLK_JMGCM(MNUMYR)$(CO2PLFMM=1) ;
    MPBLK_PMGIN(MNUMCR,MNUMYR) = MPBLK_PMGTR(MNUMCR,MNUMYR) - EMABLK_JMGTR(MNUMYR)$(CO2PLFMM=1) + EMABLK_JMGIN(MNUMYR)$(CO2PLFMM=1) ;

    MPBLK_PMGAS(MNUMCR,MNUMYR)$((QBLK_QMGTR(MNUMCR,MNUMYR)+QBLK_QMGCM(MNUMCR,MNUMYR)+QBLK_QMGIN(MNUMCR,MNUMYR))>0) =
      (QBLK_QMGTR(MNUMCR,MNUMYR) * MPBLK_PMGTR(MNUMCR,MNUMYR) +
       QBLK_QMGCM(MNUMCR,MNUMYR) * MPBLK_PMGCM(MNUMCR,MNUMYR) +
       QBLK_QMGIN(MNUMCR,MNUMYR) * MPBLK_PMGIN(MNUMCR,MNUMYR)) /
      (QBLK_QMGTR(MNUMCR,MNUMYR) + QBLK_QMGCM(MNUMCR,MNUMYR) +
       QBLK_QMGIN(MNUMCR,MNUMYR)) ;

    if(sum(MNUMCR, QBLK_QMGTR(MNUMCR,MNUMYR)+QBLK_QMGCM(MNUMCR,MNUMYR)+QBLK_QMGIN(MNUMCR,MNUMYR))>0,
      MPBLK_PMGAS('11_United_States',MNUMYR) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR),
          QBLK_QMGTR(MNUMCR,MNUMYR) * MPBLK_PMGTR(MNUMCR,MNUMYR) +
          QBLK_QMGCM(MNUMCR,MNUMYR) * MPBLK_PMGCM(MNUMCR,MNUMYR) +
          QBLK_QMGIN(MNUMCR,MNUMYR) * MPBLK_PMGIN(MNUMCR,MNUMYR) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR),
          QBLK_QMGTR(MNUMCR,MNUMYR) + QBLK_QMGCM(MNUMCR,MNUMYR) +
          QBLK_QMGIN(MNUMCR,MNUMYR) ) ;
    );

*   Purchased Electricity,  Refinery
    QBLK_QELRF(MNUMCR,MNUMYR)  =
       sum((DomRefReg,RefType,Utility('KWH'),ActiveDem)$(not EthRefType(RefType) and crN2L(ActiveDem,MNUMCR)),
             RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem) * UTILPURCH.l(DomRefReg,RefType,Utility,Period) )*CONVFACT_CFELQ*365/1000000000.0 ;

    QBLK_QELRF('11_United_States',MNUMYR)   =
       sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QELRF(MNUMCR,MNUMYR) ) ;

*   Electricity consumption, Ethanol plants
    WRENEW_QELETH(MNUMYR,MNUMCR)   =
       sum((DomRefReg,EthRefType,EthanolProcess,ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
         RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem) * abs(ProcessTable('KWH',EthanolProcess,ProcessMode))*PROCMODE.l(DomRefReg,EthRefType,EthanolProcess,ProcessMode,Period) )
         * CONVFACT_CFELQ * 365 / 1000000000.0 ;

    WRENEW_QELETH(MNUMYR,'11_United_States') =
       sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), WRENEW_QELETH(MNUMYR,MNUMCR) ) ;


*   TEMP, Hardcoded (em4), re-allocate NG from RefReg 2 into CDs 4 & 6 to better match history

    TempAllocate(RefReg,CenDiv) = RefReg_to_CenDiv_ACU_Frac(RefReg,CenDiv);
    TempAllocate('2_REFREG','CenDiv4') = 0.95 *(RefReg_to_CenDiv_ACU_Frac('2_REFREG','CenDiv4') +
                                                RefReg_to_CenDiv_ACU_Frac('2_REFREG','CenDiv6'));
    TempAllocate('2_REFREG','CenDiv6') = 0.05 *(RefReg_to_CenDiv_ACU_Frac('2_REFREG','CenDiv4') +
                                                RefReg_to_CenDiv_ACU_Frac('2_REFREG','CenDiv6'));



*   Natural Gas, Refinery Fuel Use
    QBLK_QNGRF(MNUMCR,MNUMYR)   =
      sum((DomRefReg,RefType,Process('CGN'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
***     RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable('NGS',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        TempAllocate(DomRefReg,ActiveDem)*abs(ProcessTable('NGS',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        * 365 * CONVFACT_CFRSQ / 1000 ) +
      sum((DomRefReg,RefType,Process('FUM'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
***     RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable('NGS',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        TempAllocate(DomRefReg,ActiveDem)*abs(ProcessTable('NGS',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        * 365 * CONVFACT_CFRSQ / 1000 ) ;

    QBLK_QNGRF('11_United_States',MNUMYR)   =
       sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QNGRF(MNUMCR,MNUMYR) ) ;

*   Natural gas total, Ethanol plants
    WRENEW_QNGETH(MNUMYR,MNUMCR)   =
       sum((DomRefReg,RefType,Utility('NGS'),ActiveDem)$(EthRefType(RefType) and crN2L(ActiveDem,MNUMCR)),
***          RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem) * UTILPURCH.l(DomRefReg,RefType,Utility,Period) )
***          *CONVFACT_CFRSQ*365/1000.0 ;
             TempAllocate(DomRefReg,ActiveDem) * UTILPURCH.l(DomRefReg,RefType,Utility,Period) )
             * CONVFACT_CFRSQ*365/1000.0 ;

    WRENEW_QNGETH(MNUMYR,'11_United_States')   =
       sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), WRENEW_QNGETH(MNUMYR,MNUMCR) ) ;

*   Quantity of methanol purchased by refineries
    PMMOUT_QMERF(MNUMCR,MNUMYR)    =
        sum((DomRefReg,RefInputStr('MET'),Step,ActiveDem)$crN2L(ActiveDem,MNUMCR),
***          RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem) * REFPURCH.l(DomRefReg,RefInputStr,Step,Period))
***          *CONVFACT_CFMEQT*365/1000.0 ;
             TempAllocate(DomRefReg,ActiveDem) * REFPURCH.l(DomRefReg,RefInputStr,Step,Period) )
             * CONVFACT_CFMEQT*365/1000.0 ;

    PMMOUT_QMERF('11_United_States',MNUMYR)   =
       sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), PMMOUT_QMERF(MNUMCR,MNUMYR) ) ;

*   Coal total, Ethanol plants
    WRENEW_QCLETH(MNUMYR,MNUMCR)    =
       sum((DomRefReg,CoalStr,EthRefType,Process('SEW'),ProcessMode,ActiveDem)$(ProcessTable(CoalStr,Process,ProcessMode) and crN2L(ActiveDem,MNUMCR)),
         RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem) * abs(ProcessTable(CoalStr,Process,ProcessMode))*PROCMODE.l(DomRefReg,EthRefType,Process,ProcessMode,Period) ) * 365 / 1000.0 ;

    WRENEW_QCLETH(MNUMYR,'11_United_States')   =
       sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), WRENEW_QCLETH(MNUMYR,MNUMCR) ) ;

*   Quantity of coal for CTL
    PMMOUT_QCLRFPD(MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('CTL'),ProcessMode,CoalStr)$RefReg2MNUMPR(MNUMPR,DomRefReg),
             abs(ProcessTable(CoalStr,Process,ProcessMode))* PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)) * 365/1000 +
      sum((DomRefReg,RefType,Process('CBL'),ProcessMode,CoalStr)$RefReg2MNUMPR(MNUMPR,DomRefReg),
             abs(ProcessTable(CoalStr,Process,ProcessMode))* PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)) * 365/1000 +
      sum((DomRefReg,RefType,Process('CTLCCS'),ProcessMode,CoalStr)$RefReg2MNUMPR(MNUMPR,DomRefReg),
             abs(ProcessTable(CoalStr,Process,ProcessMode))* PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)) * 365/1000 +
      sum((DomRefReg,RefType,Process('CBLCCS'),ProcessMode,CoalStr)$RefReg2MNUMPR(MNUMPR,DomRefReg),
             abs(ProcessTable(CoalStr,Process,ProcessMode))* PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)) * 365/1000 ;

    PMMOUT_QCLRFPD('10_MNUMPR',MNUMYR)  =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMOUT_QCLRFPD(MNUMPR,MNUMYR) ) ;

*   Quantity of natural gas for GTL
    LFMMOUT_QNGRFPD(MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('GTL'),ProcessMode)$RefReg2MNUMPR(MNUMPR,DomRefReg),
             abs(ProcessTable('NGS',Process,ProcessMode))* PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)) * CONVFACT_CFRSQ * 365/1000 ;

    LFMMOUT_QNGRFPD('10_MNUMPR',MNUMYR)  =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_QNGRFPD(MNUMPR,MNUMYR) ) ;

    QBLK_QRLRF(MNUMCR,MNUMYR) = 0.0 ;

* store old catalytic coke number.  this is what gets subtracted from QPCIN later on
    TEMP_QCCRF(ActiveDem) = sum(M11$CenDiv_2_M11(ActiveDem,M11),INDREP_QCCRF(M11,MNUMYR)) ;

* Dump cat coke into QPCRF for now, coker coke cannot be burned for fuel in the LFMM
    INDREP_QCCRF(M11,MNUMYR) =
      sum((DomRefReg,RefType,Process('RGN'),ProcessMode,ActiveDem)$CenDiv_2_M11(ActiveDem,M11),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable('CATC',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) )
        * 365 * CONVFACT_CFCCQ(MNUMYR) / 1000 ;

    INDREP_QCCRF('11_M11',MNUMYR)   =
      sum((ActiveDem,M11)$CenDiv_2_M11(ActiveDem,M11), INDREP_QCCRF(M11,MNUMYR) ) ;

    QBLK_QPCRF(MNUMCR,MNUMYR) = 0 ;

    QBLK_QSGRF(MNUMCR,MNUMYR) =
      sum((DomRefReg,RefType,Process('CGN'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable('PGS',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) )
        * 365 * CONVFACT_CFRSQ / 1000 +
      sum((DomRefReg,RefType,Process('FUM'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable('PGS',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) )
        * 365 * CONVFACT_CFRSQ / 1000 ;

    QBLK_QSGRF('11_United_States',MNUMYR)   =
      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QSGRF(MNUMCR,MNUMYR) ) ;

    QBLK_QLGRF(MNUMCR,MNUMYR) =
      sum((DomRefReg,RefType,Process('FUM'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable('CC3',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        * 365 * CONVFACT_CFPRQ / 1000 +
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable('UC3',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        * 365 * CONVFACT_CFPRQ / 1000  +
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable('IC4',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        * 365 * CONVFACT_CFIBQ / 1000 +
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable('NC4',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        * 365 * CONVFACT_CFBUQ / 1000 ) ;

    QBLK_QLGRF('11_United_States',MNUMYR)   =
      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QLGRF(MNUMCR,MNUMYR) ) ;

    QMORE_QPRRF(MNUMCR,MNUMYR) =
      sum((DomRefReg,RefType,Process('FUM'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable('CC3',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        * 365 * CONVFACT_CFPRQ / 1000 ) ;

    QMORE_QPRRF('11_United_States',MNUMYR)   =
      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QMORE_QPRRF(MNUMCR,MNUMYR) ) ;

    QMORE_QPYRF(MNUMCR,MNUMYR) =
      sum((DomRefReg,RefType,Process('FUM'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable('UC3',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        * 365 * CONVFACT_CFPRQ / 1000 ) ;

    QMORE_QPYRF('11_United_States',MNUMYR)   =
      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QMORE_QPYRF(MNUMCR,MNUMYR) ) ;

    QMORE_QBURF(MNUMCR,MNUMYR) =
      sum((DomRefReg,RefType,Process('FUM'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable('NC4',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        * 365 * CONVFACT_CFBUQ / 1000 ) ;

    QMORE_QBURF('11_United_States',MNUMYR)   =
      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QMORE_QBURF(MNUMCR,MNUMYR) ) ;

    QMORE_QISRF(MNUMCR,MNUMYR) =
      sum((DomRefReg,RefType,Process('FUM'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable('IC4',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        * 365 * CONVFACT_CFIBQ / 1000 ) ;

    QMORE_QISRF('11_United_States',MNUMYR)   =
      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QMORE_QISRF(MNUMCR,MNUMYR) ) ;

    QBLK_QOTRF(MNUMCR,MNUMYR) =
      sum((DomRefReg,RefType,Process('FUM'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable('H2U',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) )
        * 365 * CONVFACT_CFRSQ / 1000 ;

    QBLK_QOTRF('11_United_States',MNUMYR)   =
      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QOTRF(MNUMCR,MNUMYR) ) ;

*   Quantity of CTL liquid produced by type   Mbbl/cd
    PMMOUT_CTLFRAC(M4,MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('CTL'),ProcessMode)$RefReg2MNUMPR(MNUMPR,DomRefReg),
         sum(CTLLiq$M4_2_CTL(M4,CTLLiq),
            ProcessTable(CTLLiq,Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) ) +
      sum((DomRefReg,RefType,Process('CTLCCS'),ProcessMode)$RefReg2MNUMPR(MNUMPR,DomRefReg),
         sum(CTLLiq$M4_2_CTL(M4,CTLLiq),
            ProcessTable(CTLLiq,Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) );

    PMMOUT_CTLFRAC(M4,'10_MNUMPR',MNUMYR)   =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMOUT_CTLFRAC(M4,MNUMPR,MNUMYR) ) ;

*   Quantity of BTL liquid produced by type  Mbbl/cd
    PMMOUT_BTLFRAC(M4,MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('BTL'),ProcessMode)$RefReg2MNUMPR(MNUMPR,DomRefReg),
         sum(BTLLiq$M4_2_BTL(M4,BTLLiq),
            ProcessTable(BTLLiq,Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) );

    PMMOUT_BTLFRAC(M4,'10_MNUMPR',MNUMYR)  =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMOUT_BTLFRAC(M4,MNUMPR,MNUMYR) ) ;

*   Liquids produced from coal/biomass combo plant (1 if by coal, 2 if by biomass)    Mbbl/cd
    PMMOUT_CBTLFRAC('1_M2',M4,MNUMPR,MNUMYR)  =
      (8.0/11.0)*(sum((DomRefReg,RefType,Process('CBL'),ProcessMode)$RefReg2MNUMPR(MNUMPR,DomRefReg),
         sum(CBTLLiq$M4_2_CBTL(M4,CBTLLiq),
            ProcessTable(CBTLLiq,Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) ) ) +
     (8.0/11.0)*(sum((DomRefReg,RefType,Process('CBLCCS'),ProcessMode)$RefReg2MNUMPR(MNUMPR,DomRefReg),
         sum(CBTLLiq$M4_2_CBTL(M4,CBTLLiq),
            ProcessTable(CBTLLiq,Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) ) );
    PMMOUT_CBTLFRAC('2_M2',M4,MNUMPR,MNUMYR) =
      (3.0/11.0)*(sum((DomRefReg,RefType,Process('CBL'),ProcessMode)$RefReg2MNUMPR(MNUMPR,DomRefReg),
         sum(CBTLLiq$M4_2_CBTL(M4,CBTLLiq),
            ProcessTable(CBTLLiq,Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) ) ) +
      (3.0/11.0)*(sum((DomRefReg,RefType,Process('CBLCCS'),ProcessMode)$RefReg2MNUMPR(MNUMPR,DomRefReg),
         sum(CBTLLiq$M4_2_CBTL(M4,CBTLLiq),
            ProcessTable(CBTLLiq,Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) ) );

    PMMOUT_CBTLFRAC(M2,M4,'10_MNUMPR',MNUMYR)   =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMOUT_CBTLFRAC(M2,M4,MNUMPR,MNUMYR) ) ;

*   Quantity of GTL liquid produced by type   Mbbl/cd
    PMMOUT_GTLFRAC(M4,MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('GTL'),ProcessMode)$RefReg2MNUMPR(MNUMPR,DomRefReg),
      sum(GTLLiq$M4_2_GTL(M4,GTLLiq),
        ProcessTable(GTLLiq,Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) );

    PMMOUT_GTLFRAC(M4,'10_MNUMPR',MNUMYR) =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMOUT_GTLFRAC(M4,MNUMPR,MNUMYR) ) ;

*   Quantity of biomass for BTL, CBTL, BPU (national only) (trills/yr)
    PMMOUT_QBMRFBTL(MNUMCR,MNUMYR)  =
      sum((DomRefReg,RefType,Process('BTL'),ProcessMode,BioStr,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable(BioStr,Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period))*365/1000  +
      sum((DomRefReg,RefType,Process('CBL'),ProcessMode,BioStr,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable(BioStr,Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period))*365/1000  +
      sum((DomRefReg,RefType,Process('CBLCCS'),ProcessMode,BioStr,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable(BioStr,Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period))*365/1000  +
      sum((DomRefReg,RefType,Process('BPU'),ProcessMode,BioStr,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable(BioStr,Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period))*365/1000 ;

    PMMOUT_QBMRFBTL('11_United_States',MNUMYR)  =
      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), PMMOUT_QBMRFBTL(MNUMCR,MNUMYR) ) ;

* Conversion factors for Fischer-Tropsch liquids by process
    TotalCBTL = sum((MNUMPR,M4,CBTLLiq)$M4_2_CBTL(M4,CBTLLiq), PMMOUT_CBTLFRAC('1_M2',M4,MNUMPR,MNUMYR)) ;
    if (TotalCBTL>0,
      CONVFACT_CFCBTLLIQ('1_M3',MNUMYR) =
       (sum((MNUMPR,CBTLLiq)$M4_2_CBTL('1_M4',CBTLLiq), PMMOUT_CBTLFRAC('1_M2','1_M4',MNUMPR,MNUMYR)*CONVFACT_CFFTLIQ('1_M4',MNUMYR) ) +
        sum((MNUMPR,CBTLLiq)$M4_2_CBTL('2_M4',CBTLLiq), PMMOUT_CBTLFRAC('1_M2','2_M4',MNUMPR,MNUMYR)*CONVFACT_CFFTLIQ('2_M4',MNUMYR) ) +
        sum((MNUMPR,CBTLLiq)$M4_2_CBTL('3_M4',CBTLLiq), PMMOUT_CBTLFRAC('1_M2','3_M4',MNUMPR,MNUMYR)*CONVFACT_CFFTLIQ('3_M4',MNUMYR) ) +
        sum((MNUMPR,CBTLLiq)$M4_2_CBTL('4_M4',CBTLLiq), PMMOUT_CBTLFRAC('1_M2','4_M4',MNUMPR,MNUMYR)*CONVFACT_CFFTLIQ('4_M4',MNUMYR) ) )
          / TotalCBTL ;
    else
      CONVFACT_CFCBTLLIQ('1_M3',MNUMYR) = CONVFACT_CFDSQ ;
    );
    TotalCBTL = sum((MNUMPR,M4,CBTLLiq)$M4_2_CBTL(M4,CBTLLiq), PMMOUT_CBTLFRAC('2_M2',M4,MNUMPR,MNUMYR)) ;
    if (TotalCBTL>0,
      CONVFACT_CFCBTLLIQ('2_M3',MNUMYR) =
       (sum((MNUMPR,CBTLLiq)$M4_2_CBTL('1_M4',CBTLLiq), PMMOUT_CBTLFRAC('2_M2','1_M4',MNUMPR,MNUMYR)*CONVFACT_CFFTLIQ('1_M4',MNUMYR) ) +
        sum((MNUMPR,CBTLLiq)$M4_2_CBTL('2_M4',CBTLLiq), PMMOUT_CBTLFRAC('2_M2','2_M4',MNUMPR,MNUMYR)*CONVFACT_CFFTLIQ('2_M4',MNUMYR) ) +
        sum((MNUMPR,CBTLLiq)$M4_2_CBTL('3_M4',CBTLLiq), PMMOUT_CBTLFRAC('2_M2','3_M4',MNUMPR,MNUMYR)*CONVFACT_CFFTLIQ('3_M4',MNUMYR) ) +
        sum((MNUMPR,CBTLLiq)$M4_2_CBTL('4_M4',CBTLLiq), PMMOUT_CBTLFRAC('2_M2','4_M4',MNUMPR,MNUMYR)*CONVFACT_CFFTLIQ('4_M4',MNUMYR) ) )
          / TotalCBTL ;
    else
      CONVFACT_CFCBTLLIQ('2_M3',MNUMYR) = CONVFACT_CFDSQ ;
    );
    TotalCBTL = sum((MNUMPR,M2,M4,CBTLLiq)$M4_2_CBTL(M4,CBTLLiq), PMMOUT_CBTLFRAC(M2,M4,MNUMPR,MNUMYR)) ;
    if (TotalCBTL>0,
      CONVFACT_CFCBTLLIQ('3_M3',MNUMYR) =
       (sum((MNUMPR,M2,CBTLLiq)$M4_2_CBTL('1_M4',CBTLLiq), PMMOUT_CBTLFRAC(M2,'1_M4',MNUMPR,MNUMYR)*CONVFACT_CFFTLIQ('1_M4',MNUMYR) ) +
        sum((MNUMPR,M2,CBTLLiq)$M4_2_CBTL('2_M4',CBTLLiq), PMMOUT_CBTLFRAC(M2,'2_M4',MNUMPR,MNUMYR)*CONVFACT_CFFTLIQ('2_M4',MNUMYR) ) +
        sum((MNUMPR,M2,CBTLLiq)$M4_2_CBTL('3_M4',CBTLLiq), PMMOUT_CBTLFRAC(M2,'3_M4',MNUMPR,MNUMYR)*CONVFACT_CFFTLIQ('3_M4',MNUMYR) ) +
        sum((MNUMPR,M2,CBTLLiq)$M4_2_CBTL('4_M4',CBTLLiq), PMMOUT_CBTLFRAC(M2,'4_M4',MNUMPR,MNUMYR)*CONVFACT_CFFTLIQ('4_M4',MNUMYR) ) )
          / TotalCBTL ;
    else
      CONVFACT_CFCBTLLIQ('3_M3',MNUMYR) = CONVFACT_CFDSQ ;
    );

    TotalGTL = sum((MNUMPR,M4,GTLLiq)$M4_2_GTL(M4,GTLLiq), PMMOUT_GTLFRAC(M4,MNUMPR,MNUMYR) ) ;
    if (TotalGTL>0,
      CONVFACT_CFGTLLIQ(MNUMYR) =
       (sum((MNUMPR,GTLLiq)$M4_2_GTL('1_M4',GTLLiq), PMMOUT_GTLFRAC('1_M4',MNUMPR,MNUMYR)*CONVFACT_CFFTLIQ('1_M4',MNUMYR) ) +
        sum((MNUMPR,GTLLiq)$M4_2_GTL('2_M4',GTLLiq), PMMOUT_GTLFRAC('2_M4',MNUMPR,MNUMYR)*CONVFACT_CFFTLIQ('2_M4',MNUMYR) ) +
        sum((MNUMPR,GTLLiq)$M4_2_GTL('3_M4',GTLLiq), PMMOUT_GTLFRAC('3_M4',MNUMPR,MNUMYR)*CONVFACT_CFFTLIQ('3_M4',MNUMYR) ) +
        sum((MNUMPR,GTLLiq)$M4_2_GTL('4_M4',GTLLiq), PMMOUT_GTLFRAC('4_M4',MNUMPR,MNUMYR)*CONVFACT_CFFTLIQ('4_M4',MNUMYR) ) )
          / TotalGTL ;
    else
      CONVFACT_CFGTLLIQ(MNUMYR) = CONVFACT_CFDSQ ;
    );

    TotalCTL = sum((MNUMPR,M4,CTLLiq)$M4_2_CTL(M4,CTLLiq), PMMOUT_CTLFRAC(M4,MNUMPR,MNUMYR) ) ;
    if (TotalCTL>0,
      CONVFACT_CFCTLLIQ(MNUMYR) =
       (sum((MNUMPR,CTLLiq)$M4_2_CTL('1_M4',CTLLiq), PMMOUT_CTLFRAC('1_M4',MNUMPR,MNUMYR)*CONVFACT_CFFTLIQ('1_M4',MNUMYR) ) +
        sum((MNUMPR,CTLLiq)$M4_2_CTL('2_M4',CTLLiq), PMMOUT_CTLFRAC('2_M4',MNUMPR,MNUMYR)*CONVFACT_CFFTLIQ('2_M4',MNUMYR) ) +
        sum((MNUMPR,CTLLiq)$M4_2_CTL('3_M4',CTLLiq), PMMOUT_CTLFRAC('3_M4',MNUMPR,MNUMYR)*CONVFACT_CFFTLIQ('3_M4',MNUMYR) ) +
        sum((MNUMPR,CTLLiq)$M4_2_CTL('4_M4',CTLLiq), PMMOUT_CTLFRAC('4_M4',MNUMPR,MNUMYR)*CONVFACT_CFFTLIQ('4_M4',MNUMYR) ) )
          / TotalCTL ;
    else
      CONVFACT_CFCTLLIQ(MNUMYR) = CONVFACT_CFDSQ ;
    );

    TotalBTL = sum((MNUMPR,M4,BTLLiq)$M4_2_BTL(M4,BTLLiq), PMMOUT_BTLFRAC(M4,MNUMPR,MNUMYR) ) ;
    if (TotalBTL>0,
      CONVFACT_CFBTLLIQ(MNUMYR) =
       (sum((MNUMPR,BTLLiq)$M4_2_BTL('1_M4',BTLLiq), PMMOUT_BTLFRAC('1_M4',MNUMPR,MNUMYR)*CONVFACT_CFFTLIQ('1_M4',MNUMYR) ) +
        sum((MNUMPR,BTLLiq)$M4_2_BTL('2_M4',BTLLiq), PMMOUT_BTLFRAC('2_M4',MNUMPR,MNUMYR)*CONVFACT_CFFTLIQ('2_M4',MNUMYR) ) +
        sum((MNUMPR,BTLLiq)$M4_2_BTL('3_M4',BTLLiq), PMMOUT_BTLFRAC('3_M4',MNUMPR,MNUMYR)*CONVFACT_CFFTLIQ('3_M4',MNUMYR) ) +
        sum((MNUMPR,BTLLiq)$M4_2_BTL('4_M4',BTLLiq), PMMOUT_BTLFRAC('4_M4',MNUMPR,MNUMYR)*CONVFACT_CFFTLIQ('4_M4',MNUMYR) ) )
          / TotalBTL ;
    else
      CONVFACT_CFBTLLIQ(MNUMYR) = CONVFACT_CFDSQ ;
    );

*   Volume of pyrolysis liquids in M bbl per cd
    PMMOUT_UBAVOL(MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('BPU'),ProcessMode)$RefReg2MNUMPR(MNUMPR,DomRefReg),
         (ProcessTable('BPN',Process,ProcessMode)+ProcessTable('BPD',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) );

    PMMOUT_UBAVOL('10_MNUMPR',MNUMYR)   =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMOUT_UBAVOL(MNUMPR,MNUMYR) ) ;

*   Refinery biomass fuel consumption (trills/yr)
*   = (biomass to BTL+CBL+UBA) - (liquids from BTL) - (liquids from biomass part of CBTL) - (liquids from UBA)
    QBLK_QBMRF(MNUMCR,MNUMYR) =
      PMMOUT_QBMRFBTL(MNUMCR,MNUMYR) -
      sum((M4,MNUMPR,DomRefReg,ActiveDem)$(RefReg2MNUMPR(MNUMPR,DomRefReg) and (ord(MNUMPR)<9) and crN2L(ActiveDem,MNUMCR)),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*PMMOUT_BTLFRAC(M4,MNUMPR,MNUMYR) )*CONVFACT_CFBTLLIQ(MNUMYR)*365/1000 -
      sum((M4,MNUMPR,DomRefReg,ActiveDem)$(RefReg2MNUMPR(MNUMPR,DomRefReg) and (ord(MNUMPR)<9) and crN2L(ActiveDem,MNUMCR)),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*PMMOUT_CBTLFRAC('2_M2',M4,MNUMPR,MNUMYR) )*CONVFACT_CFBTLLIQ(MNUMYR)*365/1000 -
      sum((MNUMPR,DomRefReg,ActiveDem)$(RefReg2MNUMPR(MNUMPR,DomRefReg) and (ord(MNUMPR)<9) and crN2L(ActiveDem,MNUMCR)),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*PMMOUT_UBAVOL(MNUMPR,MNUMYR) )* CONVFACT_CFUBAQ(MNUMYR) *365/1000 ;

    QBLK_QBMRF('11_United_States',MNUMYR)   =
      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QBMRF(MNUMCR,MNUMYR) ) ;

*   World crude oil demand (PMM results)
    PMMOUT_GLBCRDDMD(MNUMYR) =
      sum(Step, CRUDETOTAL.l(Step,Period)) ;

*   Renewable diesel to distillate
    LFMMOUT_GRD2DSQTY(MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('GDT'),ProcessMode)$RefReg2MNUMPR(MNUMPR,DomRefReg),
         ProcessTable('RDH',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) )+
      sum((DomRefReg,RefType,Process('SAF'),ProcessMode)$RefReg2MNUMPR(MNUMPR,DomRefReg),
         ProcessTable('RDH',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) );

    LFMMOUT_GRD2DSQTY('10_MNUMPR',MNUMYR) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_GRD2DSQTY(MNUMPR,MNUMYR) ) ;

*   Renewable diesel to naphtha
    LFMMOUT_GRN2MGQTY(MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('GDT'),ProcessMode)$RefReg2MNUMPR(MNUMPR,DomRefReg),
         ProcessTable('RNH',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) );

    LFMMOUT_GRN2MGQTY('10_MNUMPR',MNUMYR)  =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_GRN2MGQTY(MNUMPR,MNUMYR) ) ;

*   Renewable SAF to petroleum jet
    LFMMOUT_SAF2JTQTY(MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('SAF'),ProcessMode)$RefReg2MNUMPR(MNUMPR,DomRefReg),
         ProcessTable('RJH',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) );

    LFMMOUT_SAF2JTQTY('10_MNUMPR',MNUMYR) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_SAF2JTQTY(MNUMPR,MNUMYR) ) ;

*   * Biodiesel production
*   crop oils
    LFMMOUT_BIMQTY('1_M4',MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('FBD'),ProcessMode('FBD_FCO'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
         ProcessTable('FBD',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) );

    LFMMOUT_BIMQTY('1_M4','10_MNUMPR',MNUMYR)   =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_BIMQTY('1_M4',MNUMPR,MNUMYR) ) ;

*   yellow grease
    LFMMOUT_BIMQTY('2_M4',MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('FBD'),ProcessMode('FBD_YGR'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
         ProcessTable('FBD',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) );

    LFMMOUT_BIMQTY('2_M4','10_MNUMPR',MNUMYR)   =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_BIMQTY('2_M4',MNUMPR,MNUMYR) ) ;

*   white grease
    LFMMOUT_BIMQTY('3_M4',MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('FBD'),ProcessMode('FBD_WGR'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
         ProcessTable('FBD',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) );

    LFMMOUT_BIMQTY('3_M4','10_MNUMPR',MNUMYR)   =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_BIMQTY('3_M4',MNUMPR,MNUMYR) ) ;

*   palm oil imports
    LFMMOUT_BIMQTY('4_M4',MNUMPR,MNUMYR) = 0 ;

*   Fill in federal fuel tax values in NOMINAL/mmbtu
    PMMRPT_MUFTAX(MNUMYR,'02_M15') = FedFuelTax(t,'M') ;
    PMMRPT_MUFTAX(MNUMYR,'03_M15') = FedFuelTax(t,'J') ;
    PMMRPT_MUFTAX(MNUMYR,'10_M15') = FedFuelTax(t,'D') ;
    PMMRPT_MUFTAX(MNUMYR,'14_M15') = FedFuelTax(t,'E') ;

    PMMFTAB_DSMUTR(MNUMCR,MNUMYR,'1_M2') =
      sum(ActiveDem$crN2L(ActiveDem,MNUMCR), ProductMarkups(t,'D','T',ActiveDem) ) ;
    PMMFTAB_DSMUTR(MNUMCR,MNUMYR,'2_M2') =
      sum(ActiveDem$crN2L(ActiveDem,MNUMCR), StateFuelTax(t,'D','T',ActiveDem) ) ;

    PMMFTAB_DSMUTR('11_United_States',MNUMYR,M2)$(sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR),QONROAD_QDSTRHWY(MNUMCR,MNUMYR))>0) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), PMMFTAB_DSMUTR(MNUMCR,MNUMYR,M2)*QONROAD_QDSTRHWY(MNUMCR,MNUMYR) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QONROAD_QDSTRHWY(MNUMCR,MNUMYR) ) ;

    PMMFTAB_DSMURS(MNUMCR,MNUMYR,'1_M2') =
      sum(ActiveDem$crN2L(ActiveDem,MNUMCR), ProductMarkups(t,'D','R',ActiveDem) ) ;
    PMMFTAB_DSMURS(MNUMCR,MNUMYR,'2_M2') =
      sum(ActiveDem$crN2L(ActiveDem,MNUMCR), StateFuelTax(t,'D','R',ActiveDem) ) ;

    PMMFTAB_DSMURS('11_United_States',MNUMYR,'1_M2')$(sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR),QBLK_QDSRS(MNUMCR,MNUMYR))>0) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), PMMFTAB_DSMURS(MNUMCR,MNUMYR,'1_M2')*QBLK_QDSRS(MNUMCR,MNUMYR) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QDSRS(MNUMCR,MNUMYR) ) ;

    PMMFTAB_MGMUTR(MNUMCR,MNUMYR,'1_M2') =
      sum(ActiveDem$crN2L(ActiveDem,MNUMCR), ProductMarkups(t,'M','T',ActiveDem) ) ;
    PMMFTAB_MGMUTR(MNUMCR,MNUMYR,'2_M2') =
      sum(ActiveDem$crN2L(ActiveDem,MNUMCR), StateFuelTax(t,'M','T',ActiveDem) ) ;

    PMMFTAB_MGMUTR('11_United_States',MNUMYR,M2)$(sum(ActiveDem, QMG.l(ActiveDem,Period))>0) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), PMMFTAB_MGMUTR(MNUMCR,MNUMYR,M2)*QMG.l(ActiveDem,Period) ) /
        sum(ActiveDem, QMG.l(ActiveDem,Period) ) ;

    PMMFTAB_JFMUTR(MNUMCR,MNUMYR,'1_M2') =
      sum(ActiveDem$crN2L(ActiveDem,MNUMCR), ProductMarkups(t,'J','T',ActiveDem) ) ;
    PMMFTAB_JFMUTR(MNUMCR,MNUMYR,'2_M2') =
      sum(ActiveDem$crN2L(ActiveDem,MNUMCR), StateFuelTax(t,'J','T',ActiveDem) ) ;

    PMMFTAB_JFMUTR('11_United_States',MNUMYR,M2)$(sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR),QBLK_QJFTR(MNUMCR,MNUMYR))>0) =
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), PMMFTAB_JFMUTR(MNUMCR,MNUMYR,M2)*QBLK_QJFTR(MNUMCR,MNUMYR) ) /
        sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QJFTR(MNUMCR,MNUMYR) ) ;

    PMMFTAB_CBIODUAL(MNUMYR) = 0 ;

*   ** Set NATIONAL ONLY quatity for census-level variable from NEMS
    PMMRPT_BIMQTYCD(M4,'11_United_States',MNUMYR)  =
      LFMMOUT_BIMQTY(M4,'10_MNUMPR',MNUMYR) ;

*   Total ethanol production and imports by PADD
*   3/21/13 em4: include 4.5% denaturant (hardcoded)
*   8/28/13 em4: change 4.5% to 2.49% denaturant to match RFS allowance (hardcoded)
    LFMMOUT_ETHTOT(MNUMPR,MNUMYR) =

      sum(DomRefReg$RefReg2MNUMPR(MNUMPR,DomRefReg),

       ( sum((PetRefType,RcpMode,EthStream)$((not SameAs(EthStream,'IBA')) and (not SameAs(EthStream,'IBApre'))),
           ToRECIPEBLEND.l(DomRefReg,PetRefType,RcpMode,EthStream,Period) ) +
         sum((PetRefType,RcpMode),
           ToRECIPEBLEND.l(DomRefReg,PetRefType,RcpMode,'DEN',Period) )
       )
      ) ;

    LFMMOUT_ETHTOT('10_MNUMPR',MNUMYR)  =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_ETHTOT(MNUMPR,MNUMYR) ) ;

*   Biobutanol stock change
    LFMMOUT_BIOBUTESTK(MNUMYR) = 0 ;

*   Biobutanol production by refining region
    LFMMOUT_RFBIOBUTERR(MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('IBA'),ProcessMode)$(RefReg2MNUMPR(MNUMPR,DomRefReg) and ProcessTable('IBA',Process,ProcessMode)),
         ProcessTable('IBA',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) );

    LFMMOUT_RFBIOBUTERR('10_MNUMPR',MNUMYR)  =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFBIOBUTERR(MNUMPR,MNUMYR) ) ;

*   Biobutanol production by census (must be shared out since real production is by refining region)
    LFMMOUT_RFBIOBUTECD(MNUMCR,MNUMYR) =
      sum((MNUMPR,DomRefReg,ActiveDem)$(RefReg2MNUMPR(MNUMPR,DomRefReg) and (ord(MNUMPR)<9) and crN2L(ActiveDem,MNUMCR)),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*LFMMOUT_RFBIOBUTERR(MNUMPR,MNUMYR) ) ;

    LFMMOUT_RFBIOBUTECD('11_United_States',MNUMYR)   =
      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), LFMMOUT_RFBIOBUTECD(MNUMCR,MNUMYR) ) ;

*   Consumption of biobutanol by Census division in M bbl per day
    LFMMOUT_QBIOBUTE(MNUMCR,MNUMYR) =
      0.16*sum(ActiveDem$crN2L(ActiveDem,MNUMCR),
        CDSUPPLY.l(ActiveDem,'CFGb16out',Period) + CDSUPPLY.l(ActiveDem,'RFGb16out',Period) ) ;

    LFMMOUT_QBIOBUTE('11_United_States',MNUMYR)   =
      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), LFMMOUT_QBIOBUTE(MNUMCR,MNUMYR) ) ;

*   No biobutanol imports/exports at the moment
    LFMMOUT_BIOBUTEIMP(MNUMYR) = 0 ;
    LFMMOUT_BIOBUTEEXP(MNUMYR) = 0 ;

    LFMMOUT_BIOBUTEPRICE(MNUMYR) = 0 ;
    LFMMOUT_BIOBUTEPRICE(MNUMYR)$(sum((DomRefReg,RefType,RcpMode,EthStream('IBA'))$RecipeBlending(RcpMode,EthStream),ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period))>0) =
      sum((DomRefReg,RefType,RcpMode,EthStream('IBA'))$RecipeBlending(RcpMode,EthStream),
        EthBalance.m(DomRefReg,EthStream,Period)*ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period) ) /
      sum((DomRefReg,RefType,RcpMode,EthStream('IBA'))$RecipeBlending(RcpMode,EthStream),
        ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period) ) / GDP(t) ;

*   Set LPG prices with regression equation prices

*   Pentanes plus price
    PMORE_PPPIN(MNUMCR,MNUMYR)             = npv_CoproductPrice('BTXout','1_REFREG',Period) / CONVFACT_CFPPQ / GDP(t) +
                                             EMABLK_JPPIN(MNUMYR)$(CO2PLFMM=1) ;
    PMORE_PPPIN('11_United_States',MNUMYR) = PMORE_PPPIN('01_New_England',MNUMYR) ;

    PMORE_PPPINPF(MNUMCR,MNUMYR)           = PMORE_PPPIN(MNUMCR,MNUMYR) - EMABLK_JPPIN(MNUMYR)$(CO2PLFMM=1) + EMABLK_JPPINPF(MNUMYR)$(CO2PLFMM=1) ;

*-------------------------
* WARNING: we should modify StreamProp so that it includes data for CC2

*R NOTE: change to calc ethane prices by sector in 87$/mmbtu

*R Ethane prices are now set once per cycle (before macro is called)
*R outside of the LFMM using DLM (dynamic linear model) and Bayesian priors methodology
*R via method by Janice Lent & Peter Gross (08/2014)

*R
*R maybe keep the following US totals ????

*R  Industrial ethane feedstock
*R  PMORE_PETINPF(MNUMCR,MNUMYR)           = PMORE_PETIN(MNUMCR,MNUMYR) +
*R                                           EMABLK_JETINPF(MNUMYR)$(CO2PLFMM=1) ;
*R  PMORE_PETINPF('11_United_States',MNUMYR) = PMORE_PETINPF('01_New_England',MNUMYR);
*R
*R  Industrial ethane
*R  PMORE_PETIN(MNUMCR,MNUMYR)             = PMORE_PETIN(MNUMCR,MNUMYR) +
*R                                           EMABLK_JETIN(MNUMYR)$(CO2PLFMM=1) ;
*R  PMORE_PETIN('11_United_States',MNUMYR) = PMORE_PETIN('01_New_England',MNUMYR);


*-------------------------


*   Industrial butane
    PMORE_PBUIN(MNUMCR,MNUMYR)             = npv_CoproductPrice('NC4out','1_REFREG',Period) / CONVFACT_CFBUQ / GDP(t) +
                                             EMABLK_JBUIN(MNUMYR)$(CO2PLFMM=1) ;
    PMORE_PBUIN('11_United_States',MNUMYR) = PMORE_PBUIN('01_New_England',MNUMYR) ;

*   Industrial butane feedstock
    PMORE_PBUINPF(MNUMCR,MNUMYR)           = PMORE_PBUIN(MNUMCR,MNUMYR) - EMABLK_JBUIN(MNUMYR)$(CO2PLFMM=1) + EMABLK_JBUINPF(MNUMYR)$(CO2PLFMM=1) ;

*   Industrial isobutane
    PMORE_PISIN(MNUMCR,MNUMYR)             = npv_CoproductPrice('IC4out','1_REFREG',Period) / CONVFACT_CFIBQ / GDP(t) +
                                             EMABLK_JISIN(MNUMYR)$(CO2PLFMM=1) ;
    PMORE_PISIN('11_United_States',MNUMYR) = PMORE_PISIN('01_New_England',MNUMYR) ;

    PMORE_PISINPF(MNUMCR,MNUMYR)           = PMORE_PISIN(MNUMCR,MNUMYR) - EMABLK_JISIN(MNUMYR)$(CO2PLFMM=1) + EMABLK_JISINPF(MNUMYR)$(CO2PLFMM=1) ;

*   Propylene
    PMORE_PPROLENERF(MNUMCR,MNUMYR)             = npv_CoproductPrice('UC3out','1_REFREG',Period) / CONVFACT_CFPRQ / GDP(t)  ;
    PMORE_PPROLENERF('11_United_States',MNUMYR) = PMORE_PPROLENERF('01_New_England',MNUMYR) ;

    PMMRPT_CLLETHCD('11_United_States',MNUMYR) =
      sum((DomRefReg,EthRefType,Process('CLE'),ProcessMode)$ProcessTable('ETHCLE',Process,ProcessMode),
         ProcessTable('ETHCLE',Process,ProcessMode)*PROCMODE.l(DomRefReg,EthRefType,Process,ProcessMode,Period) ) +
      sum((DomRefReg,PetRefType,RcpMode,Stream('DEN'))$RecipeBlending(RcpMode,'ETHCLE'),
         -1*RecipeBlending(RcpMode,Stream)*RECIPEMODE.l(DomRefReg,PetRefType,RcpMode,Period) ) ;

    PMMRPT_CRNETHCD('11_United_States',MNUMYR) =
      sum((DomRefReg,EthRefType,CornProcess,ProcessMode)$ProcessTable('ETHCRN',CornProcess,ProcessMode),
         ProcessTable('ETHCRN',CornProcess,ProcessMode)*PROCMODE.l(DomRefReg,EthRefType,CornProcess,ProcessMode,Period) ) +
      sum((DomRefReg,EthRefType,CornProcess,ProcessMode)$ProcessTable('ETHCRNexp',CornProcess,ProcessMode),
         ProcessTable('ETHCRNexp',CornProcess,ProcessMode)*PROCMODE.l(DomRefReg,EthRefType,CornProcess,ProcessMode,Period) ) +
      sum((DomRefReg,PetRefType,RcpMode,Stream('DEN'))$RecipeBlending(RcpMode,'ETHCRN'),
         -1*RecipeBlending(RcpMode,Stream)*RECIPEMODE.l(DomRefReg,PetRefType,RcpMode,Period) ) +
      sum((DomRefReg,PetRefType,RcpMode,Stream('DEN'))$RecipeBlending(RcpMode,'ETHCRNexp'),
         -1*RecipeBlending(RcpMode,Stream)*RECIPEMODE.l(DomRefReg,PetRefType,RcpMode,Period) ) ;

    PMMRPT_ETHIMP('11_United_States',MNUMYR) = sum((DomRefReg,EthStream), ETHIMP.l(DomRefReg,EthStream,Period) );

    PMMRPT_ETHEXP('11_United_States',MNUMYR) =
      sum((DomRefReg,EthStream), ETHEXP.l(DomRefReg,EthStream,Period) ) ;
*     sum((DomRefReg,EthStream), ETHEXP.l(DomRefReg,EthStream,Period) ) +
*     sum((DomRefReg,EthStream), ETHEXP_2.l(DomRefReg,EthStream,Period) );

    PMMRPT_OTHETHCD('11_United_States',MNUMYR) =
      sum((DomRefReg,EthRefType,Process('NCE'),ProcessMode)$ProcessTable('ETHGRN',Process,ProcessMode),
         ProcessTable('ETHGRN',Process,ProcessMode)*PROCMODE.l(DomRefReg,EthRefType,Process,ProcessMode,Period) ) +
      sum((DomRefReg,EthRefType,Process('AET'),ProcessMode)$ProcessTable('ETHAET',Process,ProcessMode),
         ProcessTable('ETHAET',Process,ProcessMode)*PROCMODE.l(DomRefReg,EthRefType,Process,ProcessMode,Period) ) +
      sum((DomRefReg,PetRefType,RcpMode,Stream('DEN'))$RecipeBlending(RcpMode,'ETHGRN'),
         -1*RecipeBlending(RcpMode,Stream)*RECIPEMODE.l(DomRefReg,PetRefType,RcpMode,Period) ) +
      sum((DomRefReg,PetRefType,RcpMode,Stream('DEN'))$RecipeBlending(RcpMode,'ETHAET'),
         -1*RecipeBlending(RcpMode,Stream)*RECIPEMODE.l(DomRefReg,PetRefType,RcpMode,Period) ) ;


* aeo2015, to match STEO, need to add capacity for ACU and CSU (splitter)
* (MMbbl/cd)
    PMMRPT_RFDSTCAP(MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType)$RefReg2MNUMPR(MNUMPR,DomRefReg),(AvailCap(DomRefReg,RefType,'ACU')  +
                                                               AvailCap(DomRefReg,RefType,'CSU')) )/1000 ;

    PMMRPT_RFDSTCAP('10_MNUMPR',MNUMYR) =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMRPT_RFDSTCAP(MNUMPR,MNUMYR) ) ;

* aeo2015, to match STEO, need to include UFO, as well as crude flow through ACU and CSU
*
    PMMRPT_RFDSTUTL(MNUMPR,MNUMYR)$(sum( (DomRefReg,RefType)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        (AvailCap(DomRefReg,RefType,'ACU')+ AvailCap(DomRefReg,RefType,'CSU')) ) >0 ) =

      100* sum(DomRefReg$RefReg2MNUMPR(MNUMPR,DomRefReg),
        ( sum(UFOImpStr, 0.333*IMPORTS.l(UFOImpStr,DomRefReg,Period) ) +
          sum(RefType, ( StreamFactors('ACU')* OPERATECAP.l(DomRefReg,RefType,'ACU',Period) +
                         StreamFactors('CSU')* OPERATECAP.l(DomRefReg,RefType,'CSU',Period) )) )) /
      sum((DomRefReg,RefType)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        (AvailCap(DomRefReg,RefType,'ACU')+
         AvailCap(DomRefReg,RefType,'CSU')) ) ;

*=    100*sum((DomRefReg,RefType)$RefReg2MNUMPR(MNUMPR,DomRefReg),
*=      (StreamFactors('ACU')* OPERATECAP.l(DomRefReg,RefType,'ACU',Period) +
*=       StreamFactors('CSU')* OPERATECAP.l(DomRefReg,RefType,'CSU',Period) ) /
*=    sum((DomRefReg,RefType)$RefReg2MNUMPR(MNUMPR,DomRefReg),
*=      (AvailCap(DomRefReg,RefType,'ACU')+
*=       AvailCap(DomRefReg,RefType,'CSU')) ) ;

*     100* sum(DomRefReg$RefReg2MNUMPR(MNUMPR,DomRefReg),
*       ( sum(UFOImpStr, 0.333*IMPORTS.l(UFOImpStr,DomRefReg,Period) ) +
*         sum(RefType, ( StreamFactors('ACU')* OPERATECAP.l(DomRefReg,RefType,'ACU',Period) +
*                        StreamFactors('CSU')* OPERATECAP.l(DomRefReg,RefType,'CSU',Period) )) )) /
*     sum((DomRefReg,RefType)$RefReg2MNUMPR(MNUMPR,DomRefReg),
*       (AvailCap(DomRefReg,RefType,'ACU')+
*        AvailCap(DomRefReg,RefType,'CSU')) ) ;


    PMMRPT_RFDSTUTL('10_MNUMPR',MNUMYR) =

      100* sum((MNUMPR,DomRefReg)$(RefReg2MNUMPR(MNUMPR,DomRefReg) and ord(MNUMPR)<9),
        ( sum(UFOImpStr, 0.333*IMPORTS.l(UFOImpStr,DomRefReg,Period) ) +
          sum(RefType, ( StreamFactors('ACU')* OPERATECAP.l(DomRefReg,RefType,'ACU',Period) +
                         StreamFactors('CSU')* OPERATECAP.l(DomRefReg,RefType,'CSU',Period) )) )) /
      sum((MNUMPR,DomRefReg,RefType)$(RefReg2MNUMPR(MNUMPR,DomRefReg) and ord(MNUMPR)<9),
        (AvailCap(DomRefReg,RefType,'ACU')+
         AvailCap(DomRefReg,RefType,'CSU')) ) ;

*=    100*sum((MNUMPR,DomRefReg,RefType)$(RefReg2MNUMPR(MNUMPR,DomRefReg) and ord(MNUMPR)<9),
*=      (StreamFactors('ACU')* OPERATECAP.l(DomRefReg,RefType,'ACU',Period) +
*=       StreamFactors('CSU')* OPERATECAP.l(DomRefReg,RefType,'CSU',Period) ) /
*=    sum((MNUMPR,DomRefReg,RefType)$(RefReg2MNUMPR(MNUMPR,DomRefReg) and ord(MNUMPR)<9),
*=      (AvailCap(DomRefReg,RefType,'ACU')+
*=       AvailCap(DomRefReg,RefType,'CSU')) ) ;

    PMMRPT_RFIMCR(MNUMPR,MNUMYR) =
      sum((DomRefReg,Crude,Source,Step)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CRUDETRANS.l(DomRefReg,Crude,Source,Step,Period) )/1000 -
      sum((DomRefReg,Crude,Source,Step)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CRUDEEXPORT.l(DomRefReg,Crude,Source,Step,Period) )/1000 ;

    PMMRPT_RFIMCR('10_MNUMPR',MNUMYR) = sum(MNUMPR$(ord(MNUMPR)<9), PMMRPT_RFIMCR(MNUMPR,MNUMYR) ) ;

    PMMRPT_RFMETM85(MNUMPR,MNUMYR) = 0 ;

*    PMMRPT_RFQARO(MNUMCR,MNUMYR) =
*      sum(ActiveDem$crN2L(ActiveDem,MNUMCR), RECIPESUPPLY.l(ActiveDem,'ASPHout',Period) ) / 1000 ;

    PMMRPT_RFQARO(MNUMCR,MNUMYR) =
      QBLK_QASIN(MNUMCR,MNUMYR) / CONVFACT_CFASQ / 365 ;

    PMMRPT_RFQARO('11_United_States',MNUMYR) =
      sum(MNUMCR$(ord(MNUMCR)<10), PMMRPT_RFQARO(MNUMCR,MNUMYR) ) ;

    PMMRPT_RFQDS(MNUMCR,MNUMYR) =
      sum(ActiveDem$crN2L(ActiveDem,MNUMCR), RECIPESUPPLY.l(ActiveDem,'CarbDSUout',Period) )/1000 +
      sum(ActiveDem$crN2L(ActiveDem,MNUMCR), RECIPESUPPLY.l(ActiveDem,'DSUout',Period) )/1000 +
      sum(ActiveDem$crN2L(ActiveDem,MNUMCR), RECIPESUPPLY.l(ActiveDem,'DSLout',Period) )/1000 +
      sum(ActiveDem$crN2L(ActiveDem,MNUMCR), RECIPESUPPLY.l(ActiveDem,'N2Hout',Period) )/1000  ;

    PMMRPT_RFQDS('11_United_States',MNUMYR) =
      sum(ActiveDem, RECIPESUPPLY.l(ActiveDem,'CarbDSUout',Period) )/1000 +
      sum(ActiveDem, RECIPESUPPLY.l(ActiveDem,'DSUout',Period) )/1000 +
      sum(ActiveDem, RECIPESUPPLY.l(ActiveDem,'DSLout',Period) )/1000 +
      sum(ActiveDem, RECIPESUPPLY.l(ActiveDem,'N2Hout',Period) )/1000  ;

*    PMMRPT_RFQJF(MNUMCR,MNUMYR) =
*      sum(ActiveDem$crN2L(ActiveDem,MNUMCR), RECIPESUPPLY.l(ActiveDem,'JTAout',Period) )/1000 ;

    PMMRPT_RFQJF(MNUMCR,MNUMYR) =
      (QBLK_QJFTR(MNUMCR,MNUMYR) +
       QBLK_QKSAS(MNUMCR,MNUMYR) ) / CONVFACT_CFJFQ(MNUMYR) / 365 ;

    PMMRPT_RFQJF('11_United_States',MNUMYR) =
      sum(MNUMCR$(ord(MNUMCR)<10), PMMRPT_RFQJF(MNUMCR,MNUMYR) ) ;

*   The LFMM does not have an individual kersosene product at the moment!
    PMMRPT_RFQKS(MNUMCR,MNUMYR) = 0 ;

    PMMRPT_RFQLG('11_United_States',MNUMYR) =
     (sum(ActiveDem, NGLDemands(ActiveDem,'CC2out',t)) +
      sum(ActiveDem, NGLDemands(ActiveDem,'LPGout',t)) +
      sum(ActiveDem, NGLDemands(ActiveDem,'NC4out',t)) +
      sum(ActiveDem, NGLDemands(ActiveDem,'IC4out',t)) +
      sum(ActiveDem, NGLDemands(ActiveDem,'NATout',t)) +
      sum(ActiveDem, NGLDemands(ActiveDem,'UC3out',t)))/1000 +
      sum((DomRefReg,RefType,Process('FUM'),ProcessMode),
        abs(ProcessTable('CC3',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) +
        abs(ProcessTable('UC3',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) +
        abs(ProcessTable('IC4',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) +
        abs(ProcessTable('NC4',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period))/1000;

    CONVFACT_CFLGQ(MNUMYR)$(PMMRPT_RFQLG('11_United_States',MNUMYR)>0) =
       (QMORE_QPRRS('11_United_States',MNUMYR) +
        QMORE_QPRCM('11_United_States',MNUMYR) +
        QMORE_QPRTR('11_United_States',MNUMYR) +
        QMORE_QETIN('11_United_States',MNUMYR) +
        QMORE_QPRIN('11_United_States',MNUMYR) +
        QMORE_QBUIN('11_United_States',MNUMYR) +
        QMORE_QISIN('11_United_States',MNUMYR) +
        QMORE_QPPIN('11_United_States',MNUMYR) +
        QMORE_QPROLENERF('11_United_States',MNUMYR)) / (PMMRPT_RFQLG('11_United_States',MNUMYR) * 365);

*    PMMRPT_RFQMG(MNUMCR,MNUMYR) =
*      sum(ActiveDem$crN2L(ActiveDem,MNUMCR), QE85.l(ActiveDem,Period) )/1000 * (0.0-RecipeBlending('RCP_E85a','CBOB')) +
*      sum(ActiveDem$crN2L(ActiveDem,MNUMCR), QMG.l(ActiveDem,Period) )/1000 ;

    PMMOUT_TRGNE85 = -RecipeBlending('RCP_E85a','CBOB') ;

    PMMOUT_ETHNE85 = 1 + RecipeBlending('RCP_E85a','CBOB') ;

    PMMRPT_RFQMG(MNUMCR,MNUMYR) =
       QBLK_QMGAS(MNUMCR,MNUMYR) / CONVFACT_CFMGQ(MNUMYR) / 365 +
       QBLK_QETTR(MNUMCR,MNUMYR) / CONVFACT_CFE85Q(MNUMYR) / 365;

    PMMRPT_RFQMG('11_United_States',MNUMYR) =
      sum(MNUMCR$(ord(MNUMCR)<10), PMMRPT_RFQMG(MNUMCR,MNUMYR) ) ;

    PMMRPT_RFQOTH(MNUMCR,MNUMYR) =
     (QBLK_QOTIN(MNUMCR,MNUMYR)-QMORE_QLUIN(MNUMCR,MNUMYR)) / 5.8 / 365.0 +
      QMORE_QLUIN(MNUMCR,MNUMYR) / CONVFACT_CFLUQ / 365.0 +
      TRANREP_QAGTR(MNUMCR,MNUMYR) / CONVFACT_CFAVQ / 365.0 +
      TRANREP_QLUTR(MNUMCR,MNUMYR) / CONVFACT_CFLUQ / 365.0 ;

    PMMRPT_RFQOTH('11_United_States',MNUMYR) =
      sum(MNUMCR$(ord(MNUMCR)<10), PMMRPT_RFQOTH(MNUMCR,MNUMYR) ) ;

*    PMMRPT_RFQPCK(MNUMCR,MNUMYR) =
*      sum(ActiveDem, RECIPESUPPLY.l(ActiveDem,'COKout',Period) ) / 1000 ;

    PMMRPT_RFQPCK(MNUMCR,MNUMYR) = ( QBLK_QPCAS(MNUMCR,MNUMYR) -
       sum(ActiveDem$crN2L(ActiveDem,MNUMCR), TEMP_QCCRF(ActiveDem)) ) / CONVFACT_CFPCQ / 365 +
       sum(ActiveDem$crN2L(ActiveDem,MNUMCR),
       sum(M11$CenDiv_2_M11(ActiveDem,M11), INDREP_QCCRF(M11,MNUMYR))) / CONVFACT_CFCCQ(MNUMYR) / 365;
*     sum((DomRefReg,RefType,Process('RGN'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
*       RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable('CATC',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) / 1000 ;

    PMMRPT_RFQPCK('11_United_States',MNUMYR) =
      sum(MNUMCR$(ord(MNUMCR)<10), PMMRPT_RFQPCK(MNUMCR,MNUMYR) ) ;

*    PMMRPT_RFQPF(MNUMCR,MNUMYR) =
*      sum(ActiveDem$crN2L(ActiveDem,MNUMCR), RECIPESUPPLY.l(ActiveDem,'PCFout',Period) )/1000 ;

    PMMRPT_RFQPF(MNUMCR,MNUMYR) =
      QBLK_QPFIN(MNUMCR,MNUMYR) / CONVFACT_CFPFQ(MNUMYR) / 365 ;

    PMMRPT_RFQPF('11_United_States',MNUMYR) =
      sum(MNUMCR$(ord(MNUMCR)<10), PMMRPT_RFQPF(MNUMCR,MNUMYR) ) ;

*    PMMRPT_RFQRH(MNUMCR,MNUMYR) =
*      sum(ActiveDem$crN2L(ActiveDem,MNUMCR), RECIPESUPPLY.l(ActiveDem,'N6Bout',Period) )/1000 ;

    PMMRPT_RFQRH(MNUMCR,MNUMYR) =
      QBLK_QRHAS(MNUMCR,MNUMYR) / CONVFACT_CFRSQ / 365 ;

    PMMRPT_RFQRH('11_United_States',MNUMYR) =
      sum(MNUMCR$(ord(MNUMCR)<10), PMMRPT_RFQRH(MNUMCR,MNUMYR) ) ;

*    PMMRPT_RFQRL(MNUMCR,MNUMYR) =
*      sum(ActiveDem$crN2L(ActiveDem,MNUMCR), RECIPESUPPLY.l(ActiveDem,'N6Iout',Period) )/1000 ;

    PMMRPT_RFQRL(MNUMCR,MNUMYR) =
      QBLK_QRLAS(MNUMCR,MNUMYR) / CONVFACT_CFRSQ / 365 ;

    PMMRPT_RFQRL('11_United_States',MNUMYR) =
      sum(MNUMCR$(ord(MNUMCR)<10), PMMRPT_RFQRL(MNUMCR,MNUMYR) ) ;

* using QSGRF instead of QSGIN because QSGIN gets processed through industrial, so
* value in there is still gas from the previous iteration, while QSGRF was filled above
    PMMRPT_RFQSTG(MNUMCR,MNUMYR) =
      QBLK_QSGRF(MNUMCR,MNUMYR) / CONVFACT_CFSGQ / 365.0 ;

    PMMRPT_RFQSTG('11_United_States',MNUMYR) =
      sum(MNUMCR$(ord(MNUMCR)<10), PMMRPT_RFQSTG(MNUMCR,MNUMYR) ) ;

    PMMRPT_TDIESEL(MNUMCR,MNUMYR) =
      sum(ActiveDem$crN2L(ActiveDem,MNUMCR), RECIPESUPPLY.l(ActiveDem,'CarbDSUout',Period) ) +
      sum(ActiveDem$crN2L(ActiveDem,MNUMCR), RECIPESUPPLY.l(ActiveDem,'DSUout',Period) ) +
      sum(ActiveDem$crN2L(ActiveDem,MNUMCR), RECIPESUPPLY.l(ActiveDem,'DSLout',Period) ) -
      sum( (ActiveDem,tt)$(crN2L(ActiveDem,MNUMCR) and ord(tt)<=ord(t) and ULSD_N2H_CD(ActiveDem,tt)>0),
          ULSD_N2H_CD(ActiveDem,tt) / 365.0 ) ;

    PMMRPT_TDIESEL('11_United_States',MNUMYR) =
      sum( MNUMCR$(ord(MNUMCR)<10), PMMRPT_TDIESEL(MNUMCR,MNUMYR) ) ;

*  *  *  loop(Per1_t_MNUMYR(Period,t,MNUMYR)$PrcPeriod(Period),

*  *    PMMRPT_TDIESEL('11_United_States',MNUMYR) =
*  *      sum(ActiveDem, RECIPESUPPLY.l(ActiveDem,'CarbDSUout',Period) ) +
*  *      sum(ActiveDem, RECIPESUPPLY.l(ActiveDem,'DSUout',Period) ) +
*  *      sum(ActiveDem, RECIPESUPPLY.l(ActiveDem,'DSLout',Period) ) ;

*   UBA fractions come from RFSScores tab in Policy Inputs.xls
    PMMFTAB_UBAVOLMG(MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('BPU'),ProcessMode)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        ProcessTable('BPN',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) );

    PMMFTAB_UBAVOLMG('10_MNUMPR',MNUMYR) =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMFTAB_UBAVOLMG(MNUMPR,MNUMYR) ) ;

    PMMFTAB_UBAVOLDS(MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('BPU'),ProcessMode)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        ProcessTable('BPD',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) );

    PMMFTAB_UBAVOLDS('10_MNUMPR',MNUMYR) =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMFTAB_UBAVOLDS(MNUMPR,MNUMYR) ) ;

    PMMRPT_BIODCONCD('1_M2','11_United_States',MNUMYR) =
      LFMMOUT_BIMQTY('1_M4','10_MNUMPR',MNUMYR) ;

    PMMRPT_BIODCONCD('2_M2','11_United_States',MNUMYR) =
      LFMMOUT_BIMQTY('2_M4','10_MNUMPR',MNUMYR) +
      LFMMOUT_BIMQTY('3_M4','10_MNUMPR',MNUMYR) ;

    PMMRPT_BIODPRICE('11_United_States',MNUMYR) = 0 ;

    PMMRPT_BIODPRICE('11_United_States',MNUMYR)$(sum((DomRefReg,RefType,Process('FBD'),ProcessMode), ProcessTable('FBD',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period))>0) =
      sum((DomRefReg,RefType,Process('FBD'),ProcessMode),
         StreamBalance.m(DomRefReg,RefType,'FBD',Period)*ProcessTable('FBD',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) /
      sum((DomRefReg,RefType,Process('FBD'),ProcessMode),
         ProcessTable('FBD',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) / GDP(t) ;

    if(sum((DomRefReg,EthStream), ETHIMP.l(DomRefReg,EthStream,Period) )>0,
      PMMFTAB_CELLIMPFRAC('11_United_States',MNUMYR) =
        sum(DomRefReg, ETHIMP.l(DomRefReg,'ETHCLE',Period) ) /
        sum((DomRefReg,EthStream), ETHIMP.l(DomRefReg,EthStream,Period) ) ;
    else
      PMMFTAB_CELLIMPFRAC('11_United_States',MNUMYR) = 0.0 ;
    ) ;

*   ** total ethanol consumed by CD for E10, E15, and E85
    PMMRPT_ETHTOTCD(MNUMCR,MNUMYR) =
        sum(ActiveDem$crN2L(ActiveDem,MNUMCR),(PMMOUT_ETHNE85* QE85.l(ActiveDem,Period)
                                             +          0.15 *QMG15.l(ActiveDem,Period)
                                             +          0.10 *QMG10.l(ActiveDem,Period))) ;

    PMMRPT_ETHTOTCD('11_United_States',MNUMYR)   =
        sum(ActiveDem,                      (  PMMOUT_ETHNE85* QE85.l(ActiveDem,Period)
                                             +          0.15 *QMG15.l(ActiveDem,Period)
                                             +          0.10 *QMG10.l(ActiveDem,Period))) ;

*  ** total ethanol consumed by CD via E85
    PMMRPT_ETHE85CD(MNUMCR,MNUMYR) =
        sum(ActiveDem$crN2L(ActiveDem,MNUMCR), PMMOUT_ETHNE85*QE85.l(ActiveDem,Period)) ;

    PMMRPT_ETHE85CD('11_United_States',MNUMYR) =
      sum(ActiveDem, PMMOUT_ETHNE85*QE85.l(ActiveDem,Period) ) ;

    PMMFTAB_GRNCAPCD('11_United_States',MNUMYR) =
      sum((DomRefReg,RefType), AvailCap(DomRefReg,RefType,'NCE') );

    PMMFTAB_CLLCAPCD('11_United_States',MNUMYR) =
      sum((DomRefReg,RefType), AvailCap(DomRefReg,RefType,'CLE') );

    PMMFTAB_CRNCAPCD('11_United_States',MNUMYR) =
      sum((DomRefReg,RefType,CornProcess), AvailCap(DomRefReg,RefType,CornProcess) );

    PMMFTAB_ADVCAPCD('11_United_States',MNUMYR) =
      sum((DomRefReg,RefType), AvailCap(DomRefReg,RefType,'AET') );

    LFMMOUT_GRAINCD('11_United_States',MNUMYR) =
      sum((DomRefReg,RefType,EthanolProcess,ProcessMode)$ProcessTable('ETHAET',EthanolProcess,ProcessMode),
        abs(ProcessTable('GRN',EthanolProcess,ProcessMode)) * PROCMODE.l(DomRefReg,RefType,EthanolProcess,ProcessMode,Period) ) * 365 / 1000;

    LFMMOUT_CORNCD('1_Ethanol','11_United_States',MNUMYR) =
      sum((DomRefReg,RefType,CornProcess,ProcessMode)$ProcessTable('ETHCRN',CornProcess,ProcessMode),
        abs(ProcessTable('CRN',CornProcess,ProcessMode)) * PROCMODE.l(DomRefReg,RefType,CornProcess,ProcessMode,Period) ) * 365 / 1000  +
      sum((DomRefReg,RefType,CornProcess,ProcessMode)$ProcessTable('ETHCRNexp',CornProcess,ProcessMode),
        abs(ProcessTable('CRN',CornProcess,ProcessMode)) * PROCMODE.l(DomRefReg,RefType,CornProcess,ProcessMode,Period) ) * 365 / 1000 ;

    LFMMOUT_CORNCD('2_Biobutanol','11_United_States',MNUMYR) =
      sum((DomRefReg,RefType,CornProcess,ProcessMode)$(ProcessTable('IBApre',CornProcess,ProcessMode)>0),
        abs(ProcessTable('CRN',CornProcess,ProcessMode)) * PROCMODE.l(DomRefReg,RefType,CornProcess,ProcessMode,Period) ) * 365 / 1000 ;

    LFMMOUT_CORNCD('3_Total','11_United_States',MNUMYR) =
      LFMMOUT_CORNCD('1_Ethanol','11_United_States',MNUMYR) +
      LFMMOUT_CORNCD('2_Biobutanol','11_United_States',MNUMYR) ;

*   Filled with RefReg level variables at the moment!
    PMMOUT_CRNPRICE(MNUMCR,MNUMYR) =
      sum(DomRefReg$(ord(DomRefReg)=ord(MNUMCR)),
        min(npv_RefInpPrc(DomRefReg,'CRN','STEP14',Period),RefPurchBal.m(DomRefReg,'CRN',Period))) / agGDP87(t) ;

*    PMMOUT_CRNPRICE(MNUMCR,MNUMYR)$((ord(MNUMCR)<2) or (ord(MNUMCR)>3)) =
*      sum(DomRefReg$(ord(DomRefReg)=ord(MNUMCR)),
*        npv_RefInpPrc(DomRefReg,'CRN','STEP05',Period) ) / agGDP87(t) ;

    PMMOUT_CRNPRICE('11_United_States',MNUMYR)$((sum((DomRefReg,Step), REFPURCH.l(DomRefReg,'CRN',Step,Period))>0)) =
      sum((DomRefReg,MNUMCR)$(ord(DomRefReg)=ord(MNUMCR)),
        PMMOUT_CRNPRICE(MNUMCR,MNUMYR)*sum(Step, REFPURCH.l(DomRefReg,'CRN',Step,Period)) ) /
      sum((DomRefReg,Step), REFPURCH.l(DomRefReg,'CRN',Step,Period) ) ;

    PMMOUT_CRNPRICE('11_United_States',MNUMYR)$((sum((DomRefReg,Step), REFPURCH.l(DomRefReg,'CRN',Step,Period))=0)) = 0 ;

    PMMRPT_GRNETHCD('11_United_States',MNUMYR) =
      sum((DomRefReg,EthRefType,Process('NCE'),ProcessMode)$ProcessTable('ETHGRN',Process,ProcessMode),
        ProcessTable('ETHGRN',Process,ProcessMode)*PROCMODE.l(DomRefReg,EthRefType,Process,ProcessMode,Period) ) +
      sum((DomRefReg,PetRefType,RcpMode,Stream('DEN'))$RecipeBlending(RcpMode,'ETHGRN'),
         -1*RecipeBlending(RcpMode,Stream)*RECIPEMODE.l(DomRefReg,PetRefType,RcpMode,Period) ) ;

    PMMFTAB_MINREN(MNUMYR) =
      npv_RFSMandates('Total',Period) ;

*   Refinery fuel use (trills/yr)
*   Initialize
    COGEN_CGREFQ(MNUMCR,MNUMYR,NUMCGF) = 0.0 ;
*** Leave CGREFQ from NG or other gas at zero for now until we figure out how to deal with the fact that
*** our cogen from gas streams is all mixed together into FUM stream

* $ontext
*   Natural gas
    COGEN_CGREFQ(MNUMCR,MNUMYR,'03_NUMCGF') =
      sum((DomRefReg,RefType,Process('CGN'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
***     RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable('NGS',Process,ProcessMode))*
        TempAllocate(DomRefReg,ActiveDem)*abs(ProcessTable('NGS',Process,ProcessMode))*
        PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        * 365 * CONVFACT_CFRSQ / 1000 ) ;

    COGEN_CGREFQ('11_United_States',MNUMYR,'03_NUMCGF') =
      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), COGEN_CGREFQ(MNUMCR,MNUMYR,'03_NUMCGF') ) ;

*   Other Gas
    COGEN_CGREFQ(MNUMCR,MNUMYR,'09_NUMCGF') =
      sum((DomRefReg,RefType,Process('CGN'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable('PGS',Process,ProcessMode))*
        PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) )
        * 365 * CONVFACT_CFRSQ / 1000 ;

    COGEN_CGREFQ('11_United_States',MNUMYR,'09_NUMCGF') =
      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), COGEN_CGREFQ(MNUMCR,MNUMYR,'09_NUMCGF') ) ;

* $offtext

*   biomass
*   = (biomass to BTL+CBL+UBA) - (liquids from BTL) - (liquids from biomass part of CBTL) - (liquids from UBA)
    COGEN_CGREFQ(MNUMCR,MNUMYR,'07_NUMCGF') =
      PMMOUT_QBMRFBTL(MNUMCR,MNUMYR) -
      sum((M4,MNUMPR,DomRefReg,ActiveDem)$(RefReg2MNUMPR(MNUMPR,DomRefReg) and (ord(MNUMPR)<9) and crN2L(ActiveDem,MNUMCR)),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*PMMOUT_BTLFRAC(M4,MNUMPR,MNUMYR) )*CONVFACT_CFBTLLIQ(MNUMYR)*365/1000 -
      sum((M4,MNUMPR,DomRefReg,ActiveDem)$(RefReg2MNUMPR(MNUMPR,DomRefReg) and (ord(MNUMPR)<9) and crN2L(ActiveDem,MNUMCR)),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*PMMOUT_CBTLFRAC('2_M2',M4,MNUMPR,MNUMYR) )*CONVFACT_CFBTLLIQ(MNUMYR)*365/1000 -
      sum((MNUMPR,DomRefReg,ActiveDem)$(RefReg2MNUMPR(MNUMPR,DomRefReg) and (ord(MNUMPR)<9) and crN2L(ActiveDem,MNUMCR)),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*PMMOUT_UBAVOL(MNUMPR,MNUMYR) )*CONVFACT_CFUBAQ(MNUMYR)*365/1000 ;

*   coal
    COGEN_CGREFQ(MNUMCR,MNUMYR,'01_NUMCGF') =
      sum((DomRefReg,RefType,Process('CTL'),ProcessMode,CoalStr,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable(CoalStr,Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period))*365/1000  +
      sum((DomRefReg,RefType,Process('CBL'),ProcessMode,CoalStr,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable(CoalStr,Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period))*365/1000 +
      sum((DomRefReg,RefType,Process('CTLCCS'),ProcessMode,CoalStr,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable(CoalStr,Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period))*365/1000 +
      sum((DomRefReg,RefType,Process('CBLCCS'),ProcessMode,CoalStr,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*abs(ProcessTable(CoalStr,Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period))*365/1000 -
      sum((M4,MNUMPR,DomRefReg,ActiveDem)$(RefReg2MNUMPR(MNUMPR,DomRefReg) and (ord(MNUMPR)<9) and crN2L(ActiveDem,MNUMCR)),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*PMMOUT_CTLFRAC(M4,MNUMPR,MNUMYR) )*CONVFACT_CFCTLLIQ(MNUMYR)*365/1000 -
      sum((M4,MNUMPR,DomRefReg,ActiveDem)$(RefReg2MNUMPR(MNUMPR,DomRefReg) and (ord(MNUMPR)<9) and crN2L(ActiveDem,MNUMCR)),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*PMMOUT_CBTLFRAC('1_M2',M4,MNUMPR,MNUMYR) )*CONVFACT_CFBTLLIQ(MNUMYR)*365/1000 ;

*   National totals
    COGEN_CGREFQ('11_United_States',MNUMYR,NUMCGF) =
      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), COGEN_CGREFQ(MNUMCR,MNUMYR,NUMCGF) ) ;

***   Refinery CHP Generation (MM kwh/yr) ***
    COGEN_CGREFGEN(MNUMCR,MNUMYR,NUMCGF,M2) = 0.0 ;

* generation - GWh
*   Natural Gas - Grid
    COGEN_CGREFGEN(MNUMCR,MNUMYR,'03_NUMCGF','1_M2') =
      sum((DomRefReg,RefType,Process('CGN'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
***     RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*
        TempAllocate(DomRefReg,ActiveDem)*
        abs(ProcessTable('KWGrid',Process,ProcessMode))*
        abs(ProcessTable('NGS',Process,ProcessMode))*
        PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        * 365 / 1000 ) ;
*   Other Gas - Grid
    COGEN_CGREFGEN(MNUMCR,MNUMYR,'09_NUMCGF','1_M2') =
      sum((DomRefReg,RefType,Process('CGN'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*
        abs(ProcessTable('KWGrid',Process,ProcessMode))*
        abs(ProcessTable('PGS',Process,ProcessMode))*
        PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        * 365 / 1000 ) +
      sum((DomRefReg,RefType,Process('KWG'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*
        abs(ProcessTable('KWGrid',Process,ProcessMode))*
        PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        * 365 / 1000 ) ;

*   Natural Gas - Own use
    COGEN_CGREFGEN(MNUMCR,MNUMYR,'03_NUMCGF','2_M2') =
      sum((DomRefReg,RefType,Process('CGN'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
***     RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*
        TempAllocate(DomRefReg,ActiveDem)*
        abs(ProcessTable('KWH',Process,ProcessMode))*
        abs(ProcessTable('NGS',Process,ProcessMode))*
        PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        * 365 / 1000 ) ;
*   Other Gas - Own use
    COGEN_CGREFGEN(MNUMCR,MNUMYR,'09_NUMCGF','2_M2') =
      sum((DomRefReg,RefType,Process('CGN'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*
        abs(ProcessTable('KWH',Process,ProcessMode))*
        abs(ProcessTable('PGS',Process,ProcessMode))*
        PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        * 365 / 1000 ) +
      sum((DomRefReg,RefType,Process('KWG'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*
        abs(ProcessTable('KWH',Process,ProcessMode))*
        PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        * 365 / 1000 ) ;

*   Coal - Grid (input data assumes 300 MW to grid, 545 MW own)
    COGEN_CGREFGEN(MNUMCR,MNUMYR,'01_NUMCGF','1_M2') =
      sum((DomRefReg,RefType,Process('CTL'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*
        abs(ProcessTable('KWGrid',Process,ProcessMode))*
        PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        * 365 / 1000 ) +
      sum((DomRefReg,RefType,Process('CTLCCS'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*
        abs(ProcessTable('KWGrid',Process,ProcessMode))*
        PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        * 365 / 1000 ) +
*       #### hardcoded 8.0/11.0
        (8.0/11.0)*sum((DomRefReg,RefType,Process('CBL'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*
        abs(ProcessTable('KWGrid',Process,ProcessMode))*
        PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        * 365 / 1000 ) +
        (8.0/11.0)*sum((DomRefReg,RefType,Process('CBLCCS'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*
        abs(ProcessTable('KWGrid',Process,ProcessMode))*
        PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        * 365 / 1000 ) ;

*   Coal - Own Use (input data assumes 300 MW to grid, 545 MW own)
    COGEN_CGREFGEN(MNUMCR,MNUMYR,'01_NUMCGF','2_M2') =
    COGEN_CGREFGEN(MNUMCR,MNUMYR,'01_NUMCGF','1_M2') *545 /300 ;

*   Biomass - Grid
    COGEN_CGREFGEN(MNUMCR,MNUMYR,'07_NUMCGF','1_M2') =
      sum((DomRefReg,RefType,Process('BTL'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*
        abs(ProcessTable('KWGrid',Process,ProcessMode))*
        PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        * 365 / 1000 ) +
*       #### hardcoded 3.0/11.0
        (3.0/11.0)*sum((DomRefReg,RefType,Process('CBL'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*
        abs(ProcessTable('KWGrid',Process,ProcessMode))*
        PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        * 365 / 1000 ) +
        (3.0/11.0)*sum((DomRefReg,RefType,Process('CBLCCS'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*
        abs(ProcessTable('KWGrid',Process,ProcessMode))*
        PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        * 365 / 1000 ) ;

*   Biomass - Own Use (NEED TO DEFINE OWN USE RATIO FOR BTL -- TEMP, use CBTL ratio)
    COGEN_CGREFGEN(MNUMCR,MNUMYR,'07_NUMCGF','2_M2') =
    COGEN_CGREFGEN(MNUMCR,MNUMYR,'07_NUMCGF','1_M2') *545 /300 ;

*   National totals
    COGEN_CGREFGEN('11_United_States',MNUMYR,NUMCGF,M2) =
      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), COGEN_CGREFGEN(MNUMCR,MNUMYR,NUMCGF,M2) ) ;


*   Refinery cogen capacity (MW)

* =======
* ======= BELOW: if CGN cannot build, then keep CGREFCAP constant at historical/STEO year levels ======
* =======
  if( (FirstBldYearMatrix('CGN','1_RefReg') >= 2100) and
      (NCNTRL_CURCALYR > STEOLYR),
    Loop( MNUMYR_STEOLYR,
      COGEN_CGREFCAP(MNUMCR,MNUMYR,NUMCGF)   = COGEN_CGREFCAP(MNUMCR,MNUMYR_STEOLYR,NUMCGF) ;
    );

  else


   COGEN_CGREFCAP(MNUMCR,MNUMYR,NUMCGF)   = 0.0 ;
*******TEMP
*******NEW 8-1-12
*******
*******TEMP FOR NOW:  and use generation results converted to GW capacity

*  Natural Gas - capacity - GW
   COGEN_CGREFCAP(MNUMCR,MNUMYR,'03_NUMCGF') =
      sum((DomRefReg,RefType,Process('CGN'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
***     RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*
        TempAllocate(DomRefReg,ActiveDem)*
        abs(ProcessTable('KWGrid',Process,ProcessMode))*
        abs(ProcessTable('NGS',Process,ProcessMode))*
        PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        / 24 / StreamFactors('CGN') ) +
      sum((DomRefReg,RefType,Process('CGN'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
***     RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*
        TempAllocate(DomRefReg,ActiveDem)*
        abs(ProcessTable('KWH',Process,ProcessMode))*
        abs(ProcessTable('NGS',Process,ProcessMode))*
        PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        / 24 / StreamFactors('CGN') ) ;

*  Other Gas - capacity - GW
   COGEN_CGREFCAP(MNUMCR,MNUMYR,'09_NUMCGF') =
      sum((DomRefReg,RefType,Process('CGN'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*
        abs(ProcessTable('KWGrid',Process,ProcessMode))*
        abs(ProcessTable('PGS',Process,ProcessMode))*
        PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        / 24 / StreamFactors('CGN') ) +
      sum((DomRefReg,RefType,Process('KWG'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*
        abs(ProcessTable('KWGrid',Process,ProcessMode))*
        PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        / 24 / StreamFactors('KWG') ) +
      sum((DomRefReg,RefType,Process('CGN'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*
        abs(ProcessTable('KWH',Process,ProcessMode))*
        abs(ProcessTable('PGS',Process,ProcessMode))*
        PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        / 24 / StreamFactors('CGN') ) +
      sum((DomRefReg,RefType,Process('KWG'),ProcessMode,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*
        abs(ProcessTable('KWH',Process,ProcessMode))*
        PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period)
        / 24 / StreamFactors('KWG') ) ;


*####


*####
*** COGEN CAPACITY ACCOUNTING
*  Cannot loop over all procmode because capacity is not separated by procmode;
*  TEMP Therefore, select CTL_CDS as representative for all procmode
*  TEMP Therefore, select CBL_CBJE as representative for all procmode
*  TEMP Therefore, select BTL_JTE as representative for all procmode
*  TEMP Therefore, select CLE_ECP as representative for all procmode
*  TEMP Therefore, select AET_DSA as representative for all procmode
*Note: if using ExistingCap and/or AvailCap, then DO NOT divide by StreamFactors

*  Coal - Total
*  -first calc to grid (KWGrid)
*== 10/21/14 em4, convert to nameplate cap for aeo2015
   COGEN_CGREFCAP(MNUMCR,MNUMYR,'01_NUMCGF') =
     sum((DomRefReg,RefType,Process('CTL'),ActiveDem)$crN2L(ActiveDem,MNUMCR),
       RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem) * AvailCap(DomRefReg,RefType,Process)/0.85 *
       ProcessTable('KWGrid',Process,'CTL_CDS') / 24 ) +
*Note: ProcessTable('KWGrid',Process,'CTL_CDS') / StreamFactors(Process) / 24 ) +
     sum((DomRefReg,RefType,Process('CTLCCS'),ActiveDem)$crN2L(ActiveDem,MNUMCR),
       RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem) * AvailCap(DomRefReg,RefType,Process)/0.85 *
       ProcessTable('KWGrid',Process,'CTLCCS_CDSCCS') / 24 ) +
*      #### hardcoded 8.0/11.0
     (8.0/11.0)*sum((DomRefReg,RefType,Process('CBL'),ActiveDem)$crN2L(ActiveDem,MNUMCR),
       RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem) * AvailCap(DomRefReg,RefType,Process)/0.85 *
       ProcessTable('KWGrid',Process,'CBL_CBJE') / 24 ) +
*Note: ProcessTable('KWGrid',Process,'CBL_CBJE') / StreamFactors(Process) / 24 ) ;
     (8.0/11.0)*sum((DomRefReg,RefType,Process('CBLCCS'),ActiveDem)$crN2L(ActiveDem,MNUMCR),
       RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem) * AvailCap(DomRefReg,RefType,Process)/0.85 *
       ProcessTable('KWGrid',Process,'CBLCCS_CBJECCS') / 24 ) ;
*  -Second convert grid to total using ratio total/grid of 845/300
   COGEN_CGREFCAP(MNUMCR,MNUMYR,'01_NUMCGF') =
   COGEN_CGREFCAP(MNUMCR,MNUMYR,'01_NUMCGF') * 845/300 ;

*  Biomass - Total
*  -first calc to grid (KWGrid)
   COGEN_CGREFCAP(MNUMCR,MNUMYR,'07_NUMCGF') =
     sum((DomRefReg,RefType,Process('BTL'),ActiveDem)$crN2L(ActiveDem,MNUMCR),
       RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem) * AvailCap(DomRefReg,RefType,Process)/0.85 *
       ProcessTable('KWGrid',Process,'BTL_JTE') / 24 ) +
*Note: ProcessTable('KWGrid',Process,'BTL_JTE') / StreamFactors(Process) / 24 ) +
*      #### hardcoded 3.0/11.0
     (3.0/11.0)*sum((DomRefReg,RefType,Process('CBL'),ActiveDem)$crN2L(ActiveDem,MNUMCR),
       RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem) * AvailCap(DomRefReg,RefType,Process)/0.85 *
       ProcessTable('KWGrid',Process,'CBL_CBJE') / 24 ) +
*Note: ProcessTable('KWGrid',Process,'CBL_CBJE') / StreamFactors(Process) / 24 ) +
     (3.0/11.0)*sum((DomRefReg,RefType,Process('CBLCCS'),ActiveDem)$crN2L(ActiveDem,MNUMCR),
       RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem) * AvailCap(DomRefReg,RefType,Process)/0.85 *
       ProcessTable('KWGrid',Process,'CBLCCS_CBJECCS') / 24 ) +
*==AET max utilization is 100%
     sum((DomRefReg,RefType,Process('AET'),ActiveDem)$crN2L(ActiveDem,MNUMCR),
       RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem) * AvailCap(DomRefReg,RefType,Process)/1.00 *
       ProcessTable('KWGrid',Process,'AET_DSA') / 24 ) +
*Note: ProcessTable('KWGrid',Process,'AET_DSA') / StreamFactors(Process) / 24 ) +
     sum((DomRefReg,RefType,Process('CLE'),ActiveDem)$crN2L(ActiveDem,MNUMCR),
       RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem) * AvailCap(DomRefReg,RefType,Process)/0.85 *
       ProcessTable('KWGrid',Process,'CLE_ECP') / 24 ) ;
*Note: ProcessTable('KWGrid',Process,'CLE_ECP') / StreamFactors(Process) / 24 ) ;
*  -Second convert grid to total using ratio total:grid 845/300
   COGEN_CGREFCAP(MNUMCR,MNUMYR,'07_NUMCGF') =
   COGEN_CGREFCAP(MNUMCR,MNUMYR,'07_NUMCGF') * 845/300 ;

*   National totals
    COGEN_CGREFCAP('11_United_States',MNUMYR,NUMCGF) =
      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), COGEN_CGREFCAP(MNUMCR,MNUMYR,NUMCGF) ) ;


   );
* endif
* =======
* ======= ABOVE: if CGN cannot build, then keep CGREFCAP constant at historical year levels ======
* =======


*   Crude oil refinery and condensate splitter inputs by refinery region and type in M bbl per day
    LFMMOUT_RFCRUDEINP(MNUMPR,MNCRUD,MNUMYR) =
      sum((Crude,PetRefType,Process('ACU'),ProcessMode,DomRefReg)$(RefReg2MNUMPR(MNUMPR,DomRefReg) and MNCRUD_2_Crude(MNCRUD,Crude)),
        abs(ProcessTable(Crude,Process,ProcessMode)) * PROCMODE.l(DomRefReg,PetRefType,Process,ProcessMode,Period) ) +
      sum((Crude,PetRefType,Process('CSU'),ProcessMode,DomRefReg)$(RefReg2MNUMPR(MNUMPR,DomRefReg) and MNCRUD_2_Crude(MNCRUD,Crude)),
        abs(ProcessTable(Crude,Process,ProcessMode)) * PROCMODE.l(DomRefReg,PetRefType,Process,ProcessMode,Period) ) ;
    LFMMOUT_RFCRUDEINP('10_MNUMPR',MNCRUD,MNUMYR) =
      sum( (MNUMPR,DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg), LFMMOUT_RFCRUDEINP(MNUMPR,MNCRUD,MNUMYR) ) ;

*   Other refinery inputs by refinery region in M bbl per day (unfinished oil imports)
    LFMMOUT_RFOTHERINP(MNUMPR,MNUMYR) =
      sum((UFOImpStr,DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(UFOImpStr,DomRefReg,Period) ) ;

    PMMRPT_RFIPQCLL(MNUMPR,MNUMYR,'2_M2') =
      sum((DomRefReg,Source,Step)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CRUDETRANS.l(DomRefReg,'L_Sweet',Source,Step,Period) );

    PMMRPT_RFIPQCLL('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMRPT_RFIPQCLL(MNUMPR,MNUMYR,'2_M2') ) ;

    PMMRPT_RFIPQCLL(MNUMPR,MNUMYR,'1_M2') =
      sum((DomRefReg,Crude('L_Sweet'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
          (CrudeBalance.m(DomRefReg,Crude,Period)-CrudeBalance_M_DELTA(DomRefReg,Crude,Period)) / GDP(t) ) ;

    PMMRPT_RFIPQCLL('10_MNUMPR',MNUMYR,'1_M2')$( PMMRPT_RFIPQCLL('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMRPT_RFIPQCLL(MNUMPR,MNUMYR,'1_M2')*PMMRPT_RFIPQCLL(MNUMPR,MNUMYR,'2_M2') ) /
      sum(MNUMPR$(ord(MNUMPR)<9), PMMRPT_RFIPQCLL(MNUMPR,MNUMYR,'2_M2') );

    PMMRPT_RFIPQCLL('10_MNUMPR',MNUMYR,'1_M2')$( PMMRPT_RFIPQCLL('10_MNUMPR',MNUMYR,'2_M2')=0) = 0.0001 ;

    PMMRPT_RFPQIPRDT(MNUMPR,MNUMYR,'2_M2') =
     (sum((ProdImpStr,DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(ProdImpStr,DomRefReg,Period) )  +
      sum((DomRefReg,TranMode,ProdImpStr)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,ProdImpStr,Period) ) +
      sum((DomRefReg,TranMode,Stream('LPGout'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,Stream,Period) ) +
      sum((NGLProduct,DomRefReg,ActiveDem)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CenDiv_to_RefReg_ACU_Frac(DomRefReg,ActiveDem)*NGLIMPORTS.l(NGLProduct,ActiveDem,Period) ) ) / 1000 ;

    PMMRPT_RFPQIPRDT('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMRPT_RFPQIPRDT(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQMG(MNUMPR,MNUMYR,'2_M2') =
      sum((ProdImpStr('CFGout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(ProdImpStr,DomRefReg,Period) )  +
      sum((DomRefReg,TranMode,ProdImpStr('CFGout'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,ProdImpStr,Period) ) ;

    LFMMOUT_RFIPQMG('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQMG(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQMG(MNUMPR,MNUMYR,'1_M2') =
      sum((ProdImpStr('CFGout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CombineSupply.m(DomRefReg,ProdImpStr,Period)/GDP(t) ) ;

    LFMMOUT_RFIPQMG('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQMG('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQMG(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQMG(MNUMPR,MNUMYR,'2_M2') ) /
        LFMMOUT_RFIPQMG('10_MNUMPR',MNUMYR,'2_M2') ;
    LFMMOUT_RFIPQMG(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQMG(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    LFMMOUT_RFIPQCBOB(MNUMPR,MNUMYR,'2_M2') =
      sum(DomRefReg$RefReg2MNUMPR(MNUMPR,DomRefReg), IMPORTS.l('CBOB',DomRefReg,Period) ) +
      sum((DomRefReg,TranMode,GasSpecProd('CBOB'))$(REFtoREFTranCost('9_REFREG',DomRefReg,TranMode,GasSpecProd) and RefReg2MNUMPR(MNUMPR,DomRefReg)),
        RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,GasSpecProd,Period) ) ;

    LFMMOUT_RFIPQCBOB('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQCBOB(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQCBOB(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQCBOB(MNUMPR,MNUMYR,'2_M2')>0) =
     (sum(DomRefReg$RefReg2MNUMPR(MNUMPR,DomRefReg),
        BOBBalance.m(DomRefReg,'CBOB',Period)*IMPORTS.l('CBOB',DomRefReg,Period)/GDP(t) ) +
      sum((DomRefReg,TranMode,GasSpecProd('CBOB'))$(REFtoREFTranCost('9_REFREG',DomRefReg,TranMode,GasSpecProd) and RefReg2MNUMPR(MNUMPR,DomRefReg)),
        BOBBalance.m(DomRefReg,'CBOB',Period)*RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,GasSpecProd,Period)/GDP(t) ) ) /
        LFMMOUT_RFIPQCBOB(MNUMPR,MNUMYR,'2_M2') ;

    LFMMOUT_RFIPQCBOB('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQCBOB('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQCBOB(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQCBOB(MNUMPR,MNUMYR,'2_M2') ) /
        LFMMOUT_RFIPQCBOB('10_MNUMPR',MNUMYR,'2_M2') ;
        LFMMOUT_RFIPQCBOB(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQCBOB(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    LFMMOUT_RFIPQRG(MNUMPR,MNUMYR,'2_M2') =
      sum((ProdImpStr('RFGout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(ProdImpStr,DomRefReg,Period) )  +
      sum((DomRefReg,TranMode,ProdImpStr('RFGout'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,ProdImpStr,Period) ) ;

    LFMMOUT_RFIPQRG('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQRG(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQRG(MNUMPR,MNUMYR,'1_M2') =
      sum((ProdImpStr('RFGout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CombineSupply.m(DomRefReg,ProdImpStr,Period)/GDP(t) ) ;

    LFMMOUT_RFIPQRG('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQRG('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQRG(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQRG(MNUMPR,MNUMYR,'2_M2') ) /
        LFMMOUT_RFIPQRG('10_MNUMPR',MNUMYR,'2_M2') ;
        LFMMOUT_RFIPQRG(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQRG(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    LFMMOUT_RFIPQRBOB(MNUMPR,MNUMYR,'2_M2') =
      sum(DomRefReg$RefReg2MNUMPR(MNUMPR,DomRefReg), IMPORTS.l('RBOB',DomRefReg,Period) ) +
      sum((DomRefReg,TranMode,GasSpecProd('RBOB'))$(REFtoREFTranCost('9_REFREG',DomRefReg,TranMode,GasSpecProd) and RefReg2MNUMPR(MNUMPR,DomRefReg)),
        RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,GasSpecProd,Period) ) ;

    LFMMOUT_RFIPQRBOB('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQRBOB(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQRBOB(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQRBOB(MNUMPR,MNUMYR,'2_M2')>0) =
     (sum(DomRefReg$RefReg2MNUMPR(MNUMPR,DomRefReg),
        BOBBalance.m(DomRefReg,'RBOB',Period)*IMPORTS.l('RBOB',DomRefReg,Period)/GDP(t) ) +
      sum((DomRefReg,TranMode,GasSpecProd('RBOB'))$(REFtoREFTranCost('9_REFREG',DomRefReg,TranMode,GasSpecProd) and RefReg2MNUMPR(MNUMPR,DomRefReg)),
        BOBBalance.m(DomRefReg,'RBOB',Period)*RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,GasSpecProd,Period)/GDP(t) ) ) /
      LFMMOUT_RFIPQRBOB(MNUMPR,MNUMYR,'2_M2') ;

    LFMMOUT_RFIPQRBOB('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQRBOB('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQRBOB(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQRBOB(MNUMPR,MNUMYR,'2_M2') ) /
        LFMMOUT_RFIPQRBOB('10_MNUMPR',MNUMYR,'2_M2') ;
        LFMMOUT_RFIPQRBOB(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQRBOB(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    LFMMOUT_RFIPQPR(MNUMPR,MNUMYR,'2_M2') =
      sum((NGLProduct('LPGout'),DomRefReg,ActiveDem)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CenDiv_to_RefReg_ACU_Frac(DomRefReg,ActiveDem)*NGLIMPORTS.l(NGLProduct,ActiveDem,Period) ) +
      sum((DomRefReg,TranMode,Stream('LPGout'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,Stream,Period) ) ;

    LFMMOUT_RFIPQPR('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQPR(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQPR(MNUMPR,MNUMYR,'1_M2') = 0.0;
    LFMMOUT_RFIPQPR(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQPR(MNUMPR,MNUMYR,'2_M2')>0) =
      sum((NGLProduct('LPGout'),DomRefReg,ActiveDem)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CenDiv_to_RefReg_ACU_Frac(DomRefReg,ActiveDem)*NGLIMPORTS.l(NGLProduct,ActiveDem,Period)*NGLEndBal.m(ActiveDem,NGLProduct,Period) )
        /GDP(t)/LFMMOUT_RFIPQPR(MNUMPR,MNUMYR,'2_M2') ;

    LFMMOUT_RFIPQPR('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQPR('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQPR(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQPR(MNUMPR,MNUMYR,'2_M2') ) /
        LFMMOUT_RFIPQPR('10_MNUMPR',MNUMYR,'2_M2') ;
        LFMMOUT_RFIPQPR(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQPR(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    LFMMOUT_RFIPQPY(MNUMPR,MNUMYR,'2_M2') =
      sum((NGLProduct('UC3out'),DomRefReg,ActiveDem)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CenDiv_to_RefReg_ACU_Frac(DomRefReg,ActiveDem)*NGLIMPORTS.l(NGLProduct,ActiveDem,Period) ) ;

    LFMMOUT_RFIPQPY('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQPY(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQPY(MNUMPR,MNUMYR,'1_M2') = 0.0;
    LFMMOUT_RFIPQPY(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQPY(MNUMPR,MNUMYR,'2_M2')>0) =
      sum((NGLProduct('UC3out'),DomRefReg,ActiveDem)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CenDiv_to_RefReg_ACU_Frac(DomRefReg,ActiveDem)*NGLIMPORTS.l(NGLProduct,ActiveDem,Period)*NGLEndBal.m(ActiveDem,NGLProduct,Period) )
        /GDP(t)/LFMMOUT_RFIPQPY(MNUMPR,MNUMYR,'2_M2') ;

    LFMMOUT_RFIPQPY('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQPY('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQPY(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQPY(MNUMPR,MNUMYR,'2_M2') ) /
        LFMMOUT_RFIPQPY('10_MNUMPR',MNUMYR,'2_M2') ;
        LFMMOUT_RFIPQPY(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQPY(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    LFMMOUT_RFIPQET(MNUMPR,MNUMYR,'2_M2') =
      sum((NGLProduct('CC2out'),DomRefReg,ActiveDem)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CenDiv_to_RefReg_ACU_Frac(DomRefReg,ActiveDem)*NGLIMPORTS.l(NGLProduct,ActiveDem,Period) ) ;

    LFMMOUT_RFIPQET('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQET(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQET(MNUMPR,MNUMYR,'1_M2') = 0.0;
    LFMMOUT_RFIPQET(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQET(MNUMPR,MNUMYR,'2_M2')>0) =
      sum((NGLProduct('CC2out'),DomRefReg,ActiveDem)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CenDiv_to_RefReg_ACU_Frac(DomRefReg,ActiveDem)*NGLIMPORTS.l(NGLProduct,ActiveDem,Period)*NGLEndBal.m(ActiveDem,NGLProduct,Period) )
        /GDP(t)/LFMMOUT_RFIPQET(MNUMPR,MNUMYR,'2_M2') ;

    LFMMOUT_RFIPQET('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQET('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQET(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQET(MNUMPR,MNUMYR,'2_M2') ) /
        LFMMOUT_RFIPQET('10_MNUMPR',MNUMYR,'2_M2') ;
        LFMMOUT_RFIPQET(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQET(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    LFMMOUT_RFIPQBU(MNUMPR,MNUMYR,'2_M2') =
      sum((NGLProduct('NC4out'),DomRefReg,ActiveDem)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CenDiv_to_RefReg_ACU_Frac(DomRefReg,ActiveDem)*NGLIMPORTS.l(NGLProduct,ActiveDem,Period) ) ;

    LFMMOUT_RFIPQBU('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQBU(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQBU(MNUMPR,MNUMYR,'1_M2') = 0.0;
    LFMMOUT_RFIPQBU(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQBU(MNUMPR,MNUMYR,'2_M2')>0) =
      sum((NGLProduct('NC4out'),DomRefReg,ActiveDem)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CenDiv_to_RefReg_ACU_Frac(DomRefReg,ActiveDem)*NGLIMPORTS.l(NGLProduct,ActiveDem,Period)*NGLEndBal.m(ActiveDem,NGLProduct,Period) )
        /GDP(t)/LFMMOUT_RFIPQBU(MNUMPR,MNUMYR,'2_M2') ;

    LFMMOUT_RFIPQBU('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQBU('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQBU(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQBU(MNUMPR,MNUMYR,'2_M2') ) /
        LFMMOUT_RFIPQBU('10_MNUMPR',MNUMYR,'2_M2') ;
        LFMMOUT_RFIPQBU(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQBU(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    LFMMOUT_RFIPQIS(MNUMPR,MNUMYR,'2_M2') =
      sum((NGLProduct('IC4out'),DomRefReg,ActiveDem)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CenDiv_to_RefReg_ACU_Frac(DomRefReg,ActiveDem)*NGLIMPORTS.l(NGLProduct,ActiveDem,Period) ) ;

    LFMMOUT_RFIPQIS('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQIS(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQIS(MNUMPR,MNUMYR,'1_M2') = 0.0;
    LFMMOUT_RFIPQIS(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQIS(MNUMPR,MNUMYR,'2_M2')>0) =
      sum((NGLProduct('IC4out'),DomRefReg,ActiveDem)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CenDiv_to_RefReg_ACU_Frac(DomRefReg,ActiveDem)*NGLIMPORTS.l(NGLProduct,ActiveDem,Period)*NGLEndBal.m(ActiveDem,NGLProduct,Period) )
        /GDP(t)/LFMMOUT_RFIPQIS(MNUMPR,MNUMYR,'2_M2') ;

    LFMMOUT_RFIPQIS('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQIS('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQIS(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQIS(MNUMPR,MNUMYR,'2_M2') ) /
        LFMMOUT_RFIPQIS('10_MNUMPR',MNUMYR,'2_M2') ;
        LFMMOUT_RFIPQIS(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQIS(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    LFMMOUT_RFIPQPP(MNUMPR,MNUMYR,'2_M2') =
      sum((NGLProduct('NATout'),DomRefReg,ActiveDem)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CenDiv_to_RefReg_ACU_Frac(DomRefReg,ActiveDem)*NGLIMPORTS.l(NGLProduct,ActiveDem,Period) ) ;

    LFMMOUT_RFIPQPP('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQPP(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQPP(MNUMPR,MNUMYR,'1_M2') = 0.0;
    LFMMOUT_RFIPQPP(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQPP(MNUMPR,MNUMYR,'2_M2')>0) =
      sum((NGLProduct('NATout'),DomRefReg,ActiveDem)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CenDiv_to_RefReg_ACU_Frac(DomRefReg,ActiveDem)*NGLIMPORTS.l(NGLProduct,ActiveDem,Period)*NGLEndBal.m(ActiveDem,NGLProduct,Period) )
        /GDP(t)/LFMMOUT_RFIPQPP(MNUMPR,MNUMYR,'2_M2') ;

    LFMMOUT_RFIPQPP('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQPP('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQPP(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQPP(MNUMPR,MNUMYR,'2_M2') ) /
        LFMMOUT_RFIPQPP('10_MNUMPR',MNUMYR,'2_M2') ;
        LFMMOUT_RFIPQPP(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQPP(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;


    LFMMOUT_RFIPQJF(MNUMPR,MNUMYR,'2_M2') =
      sum((ProdImpStr('JTAout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(ProdImpStr,DomRefReg,Period) )  +
      sum((DomRefReg,TranMode,ProdImpStr('JTAout'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,ProdImpStr,Period) ) ;

    LFMMOUT_RFIPQJF('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQJF(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQJF(MNUMPR,MNUMYR,'1_M2') =
      sum((ProdImpStr('JTAout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CombineSupply.m(DomRefReg,ProdImpStr,Period)/GDP(t) ) ;

    LFMMOUT_RFIPQJF('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQJF('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQJF(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQJF(MNUMPR,MNUMYR,'2_M2') ) /
        LFMMOUT_RFIPQJF('10_MNUMPR',MNUMYR,'2_M2') ;
    LFMMOUT_RFIPQJF(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQJF(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    LFMMOUT_RFIPQDS(MNUMPR,MNUMYR,'2_M2') =
      sum((ProdImpStr('N2Hout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(ProdImpStr,DomRefReg,Period) )  +
      sum((DomRefReg,TranMode,ProdImpStr('N2Hout'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,ProdImpStr,Period) ) ;

    LFMMOUT_RFIPQDS('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQDS(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQDS(MNUMPR,MNUMYR,'1_M2') =
      sum((ProdImpStr('N2Hout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CombineSupply.m(DomRefReg,ProdImpStr,Period)/GDP(t) ) ;

    LFMMOUT_RFIPQDS('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQDS('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQDS(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQDS(MNUMPR,MNUMYR,'2_M2') ) /
        LFMMOUT_RFIPQDS('10_MNUMPR',MNUMYR,'2_M2') ;
    LFMMOUT_RFIPQDS(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQDS(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    LFMMOUT_RFIPQDL(MNUMPR,MNUMYR,'2_M2') =
      sum((ProdImpStr('DSLout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(ProdImpStr,DomRefReg,Period) )  +
      sum((DomRefReg,TranMode,ProdImpStr('DSLout'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,ProdImpStr,Period) ) ;

    LFMMOUT_RFIPQDL('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQDL(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQDL(MNUMPR,MNUMYR,'1_M2') =
      sum((ProdImpStr('DSLout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CombineSupply.m(DomRefReg,ProdImpStr,Period)/GDP(t) ) ;

    LFMMOUT_RFIPQDL('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQDL('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQDL(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQDL(MNUMPR,MNUMYR,'2_M2') ) /
        LFMMOUT_RFIPQDL('10_MNUMPR',MNUMYR,'2_M2') ;
    LFMMOUT_RFIPQDL(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQDL(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    LFMMOUT_RFIPQDU(MNUMPR,MNUMYR,'2_M2') =
      sum((ProdImpStr('DSUout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(ProdImpStr,DomRefReg,Period) )  +
      sum((DomRefReg,TranMode,ProdImpStr('DSUout'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,ProdImpStr,Period) ) ;

    LFMMOUT_RFIPQDU('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQDU(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQDU(MNUMPR,MNUMYR,'1_M2') =
      sum((ProdImpStr('DSUout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CombineSupply.m(DomRefReg,ProdImpStr,Period)/GDP(t) ) ;

    LFMMOUT_RFIPQDU('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQDU('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQDU(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQDU(MNUMPR,MNUMYR,'2_M2') ) /
        LFMMOUT_RFIPQDU('10_MNUMPR',MNUMYR,'2_M2') ;
    LFMMOUT_RFIPQDU(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQDU(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    LFMMOUT_RFIPQRL(MNUMPR,MNUMYR,'2_M2') =
      sum((ProdImpStr('N6Iout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(ProdImpStr,DomRefReg,Period) )  +
      sum((DomRefReg,TranMode,ProdImpStr('N6Iout'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,ProdImpStr,Period) ) ;

    LFMMOUT_RFIPQRL('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQRL(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQRL(MNUMPR,MNUMYR,'1_M2') =
      sum((ProdImpStr('N6Iout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CombineSupply.m(DomRefReg,ProdImpStr,Period)/GDP(t) ) ;

    LFMMOUT_RFIPQRL('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQRL('10_MNUMPR',MNUMYR,'2_M2')>0) =
       sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQRL(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQRL(MNUMPR,MNUMYR,'2_M2') ) /
         LFMMOUT_RFIPQRL('10_MNUMPR',MNUMYR,'2_M2') ;
    LFMMOUT_RFIPQRL(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQRL(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    LFMMOUT_RFIPQRH(MNUMPR,MNUMYR,'2_M2') =
      sum((ProdImpStr('N6Bout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(ProdImpStr,DomRefReg,Period) )  +
      sum((DomRefReg,TranMode,ProdImpStr('N6Bout'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,ProdImpStr,Period) ) ;

    LFMMOUT_RFIPQRH('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQRH(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQRH(MNUMPR,MNUMYR,'1_M2') =
      sum((ProdImpStr('N6Bout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CombineSupply.m(DomRefReg,ProdImpStr,Period)/GDP(t) ) ;

    LFMMOUT_RFIPQRH('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQRH('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQRH(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQRH(MNUMPR,MNUMYR,'2_M2') ) /
        LFMMOUT_RFIPQRH('10_MNUMPR',MNUMYR,'2_M2') ;
    LFMMOUT_RFIPQRH(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQRH(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    LFMMOUT_RFIPQPF(MNUMPR,MNUMYR,'2_M2') =
      sum((ProdImpStr('PCFout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(ProdImpStr,DomRefReg,Period) )  +
      sum((DomRefReg,TranMode,ProdImpStr('PCFout'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,ProdImpStr,Period) ) ;

    LFMMOUT_RFIPQPF('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQPF(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQPF(MNUMPR,MNUMYR,'1_M2') =
      sum((ProdImpStr('PCFout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CombineSupply.m(DomRefReg,ProdImpStr,Period)/GDP(t) ) ;

    LFMMOUT_RFIPQPF('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQPF('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQPF(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQPF(MNUMPR,MNUMYR,'2_M2') ) /
        LFMMOUT_RFIPQPF('10_MNUMPR',MNUMYR,'2_M2') ;
    LFMMOUT_RFIPQPF(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQPF(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    LFMMOUT_RFIPQMN3(MNUMPR,MNUMYR,'2_M2') =
      sum((UFOImpStr('MN3'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(UFOImpStr,DomRefReg,Period) )  +
      sum((DomRefReg,TranMode,UFOImpStr('MN3'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,UFOImpStr,Period) ) ;

    LFMMOUT_RFIPQMN3('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQMN3(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQMN3(MNUMPR,MNUMYR,'1_M2')$((sum((UFOImpStr('MN3'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
                                           IMPORTS.l(UFOImpStr,DomRefReg,Period) ) )>0) =
     (sum((UFOImpStr('MN3'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        StreamBalance.m(DomRefReg,'COKING',UFOImpStr,Period) * IMPORTS.l(UFOImpStr,DomRefReg,Period)/GDP(t) ) ) /
     (sum((UFOImpStr('MN3'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(UFOImpStr,DomRefReg,Period) ) ) ;

    LFMMOUT_RFIPQMN3('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQMN3('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQMN3(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQMN3(MNUMPR,MNUMYR,'2_M2') ) /
        LFMMOUT_RFIPQMN3('10_MNUMPR',MNUMYR,'2_M2') ;
    LFMMOUT_RFIPQMN3(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQMN3(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    LFMMOUT_RFIPQGO3(MNUMPR,MNUMYR,'2_M2') =
      sum((UFOImpStr('GO3'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(UFOImpStr,DomRefReg,Period) )  +
      sum((DomRefReg,TranMode,UFOImpStr('GO3'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,UFOImpStr,Period) ) ;

    LFMMOUT_RFIPQGO3('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQGO3(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQGO3(MNUMPR,MNUMYR,'1_M2')$((sum((UFOImpStr('GO3'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
                                           IMPORTS.l(UFOImpStr,DomRefReg,Period) ) )>0) =
     (sum((UFOImpStr('GO3'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        StreamBalance.m(DomRefReg,'COKING',UFOImpStr,Period) * IMPORTS.l(UFOImpStr,DomRefReg,Period)/GDP(t) ) ) /
     (sum((UFOImpStr('GO3'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(UFOImpStr,DomRefReg,Period) ) ) ;

    LFMMOUT_RFIPQGO3('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQGO3('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQGO3(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQGO3(MNUMPR,MNUMYR,'2_M2') ) /
        LFMMOUT_RFIPQGO3('10_MNUMPR',MNUMYR,'2_M2') ;
    LFMMOUT_RFIPQGO3(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQGO3(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    LFMMOUT_RFIPQAR3(MNUMPR,MNUMYR,'2_M2') =
      sum((UFOImpStr('AR3'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(UFOImpStr,DomRefReg,Period) )  +
      sum((DomRefReg,TranMode,UFOImpStr('AR3'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,UFOImpStr,Period) ) ;

    LFMMOUT_RFIPQAR3('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQAR3(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQAR3(MNUMPR,MNUMYR,'1_M2')$((sum((UFOImpStr('AR3'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
                                           IMPORTS.l(UFOImpStr,DomRefReg,Period) ) )>0) =
     (sum((UFOImpStr('AR3'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        StreamBalance.m(DomRefReg,'COKING',UFOImpStr,Period) * IMPORTS.l(UFOImpStr,DomRefReg,Period)/GDP(t) ) ) /
     (sum((UFOImpStr('AR3'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(UFOImpStr,DomRefReg,Period) ) ) ;

    LFMMOUT_RFIPQAR3('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQAR3('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQAR3(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQAR3(MNUMPR,MNUMYR,'2_M2') ) /
        LFMMOUT_RFIPQAR3('10_MNUMPR',MNUMYR,'2_M2') ;
    LFMMOUT_RFIPQAR3(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQAR3(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    LFMMOUT_RFIPQAG(MNUMPR,MNUMYR,'2_M2') =
      sum((ProdImpStr('AVGout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(ProdImpStr,DomRefReg,Period) )  +
      sum((DomRefReg,TranMode,ProdImpStr('AVGout'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,ProdImpStr,Period) ) ;

    LFMMOUT_RFIPQAG('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQAG(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQAG(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQAG(MNUMPR,MNUMYR,'2_M2')>0) =
      (sum((ProdImpStr('AVGout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CombineSupply.m(DomRefReg,ProdImpStr,Period) * IMPORTS.l(ProdImpStr,DomRefReg,Period)/GDP(t) ) ) /
        LFMMOUT_RFIPQAG(MNUMPR,MNUMYR,'2_M2') ;

    LFMMOUT_RFIPQAG('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQAG('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQAG(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQAG(MNUMPR,MNUMYR,'2_M2') ) /
        LFMMOUT_RFIPQAG('10_MNUMPR',MNUMYR,'2_M2') ;
    LFMMOUT_RFIPQAG(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQAG(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    LFMMOUT_RFIPQAS(MNUMPR,MNUMYR,'2_M2') =
      sum((ProdImpStr('ASPHout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(ProdImpStr,DomRefReg,Period) )  +
      sum((DomRefReg,TranMode,ProdImpStr('ASPHout'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,ProdImpStr,Period) ) ;

    LFMMOUT_RFIPQAS('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQAS(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQAS(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQAS(MNUMPR,MNUMYR,'2_M2')>0) =
      (sum((ProdImpStr('ASPHout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CombineSupply.m(DomRefReg,ProdImpStr,Period) * IMPORTS.l(ProdImpStr,DomRefReg,Period)/GDP(t) ) ) /
        LFMMOUT_RFIPQAS(MNUMPR,MNUMYR,'2_M2') ;

    LFMMOUT_RFIPQAS('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQAS('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQAS(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQAS(MNUMPR,MNUMYR,'2_M2') ) /
        LFMMOUT_RFIPQAS('10_MNUMPR',MNUMYR,'2_M2') ;
    LFMMOUT_RFIPQAS(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQAS(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    LFMMOUT_RFIPQLU(MNUMPR,MNUMYR,'2_M2') =
      sum((ProdImpStr('LUBout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(ProdImpStr,DomRefReg,Period) )  +
      sum((DomRefReg,TranMode,ProdImpStr('LUBout'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,ProdImpStr,Period) ) ;

    LFMMOUT_RFIPQLU('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQLU(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQLU(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQLU(MNUMPR,MNUMYR,'2_M2')>0) =
      (sum((ProdImpStr('LUBout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CombineSupply.m(DomRefReg,ProdImpStr,Period) * IMPORTS.l(ProdImpStr,DomRefReg,Period)/GDP(t) ) ) /
        LFMMOUT_RFIPQLU(MNUMPR,MNUMYR,'2_M2') ;

    LFMMOUT_RFIPQLU('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQLU('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQLU(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQLU(MNUMPR,MNUMYR,'2_M2') ) /
        LFMMOUT_RFIPQLU('10_MNUMPR',MNUMYR,'2_M2') ;
    LFMMOUT_RFIPQLU(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQLU(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    LFMMOUT_RFIPQPC(MNUMPR,MNUMYR,'2_M2') =
      sum((ProdImpStr('COKout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(ProdImpStr,DomRefReg,Period) )  +
      sum((DomRefReg,TranMode,ProdImpStr('COKout'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,ProdImpStr,Period) ) ;

    LFMMOUT_RFIPQPC('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQPC(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQPC(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQPC(MNUMPR,MNUMYR,'2_M2')>0) =
      (sum((ProdImpStr('COKout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CombineSupply.m(DomRefReg,ProdImpStr,Period) * IMPORTS.l(ProdImpStr,DomRefReg,Period)/GDP(t) ) ) /
        LFMMOUT_RFIPQPC(MNUMPR,MNUMYR,'2_M2') ;

    LFMMOUT_RFIPQPC('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQPC('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQPC(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQPC(MNUMPR,MNUMYR,'2_M2') ) /
        LFMMOUT_RFIPQPC('10_MNUMPR',MNUMYR,'2_M2') ;
    LFMMOUT_RFIPQPC(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQPC(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    LFMMOUT_RFIPQCG(MNUMPR,MNUMYR,'2_M2') =
      sum((ProdImpStr('CaRBOBout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(ProdImpStr,DomRefReg,Period) )  +
      sum((DomRefReg,TranMode,ProdImpStr('CaRBOBout'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,ProdImpStr,Period) ) ;

    LFMMOUT_RFIPQCG('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQCG(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQCG(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQCG(MNUMPR,MNUMYR,'2_M2')>0) =
      sum((ProdImpStr('CaRBOBout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CombineSupply.m(DomRefReg,ProdImpStr,Period)/GDP(t) ) ;

    LFMMOUT_RFIPQCG('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQCG('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQCG(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQCG(MNUMPR,MNUMYR,'2_M2') ) /
      LFMMOUT_RFIPQCG('10_MNUMPR',MNUMYR,'2_M2') ;
    LFMMOUT_RFIPQCG(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQCG(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    LFMMOUT_RFIPQCD(MNUMPR,MNUMYR,'2_M2') =
      sum((ProdImpStr('CarbDSUout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(ProdImpStr,DomRefReg,Period) )  +
      sum((DomRefReg,TranMode,ProdImpStr('CarbDSUout'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,ProdImpStr,Period) ) ;

    LFMMOUT_RFIPQCD('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQCD(MNUMPR,MNUMYR,'2_M2') ) ;

    LFMMOUT_RFIPQCD(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQCD(MNUMPR,MNUMYR,'2_M2')>0) =
      sum((ProdImpStr('CarbDSUout'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CombineSupply.m(DomRefReg,ProdImpStr,Period)/GDP(t) ) ;

    LFMMOUT_RFIPQCD('10_MNUMPR',MNUMYR,'1_M2')$(LFMMOUT_RFIPQCD('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RFIPQCD(MNUMPR,MNUMYR,'1_M2')*LFMMOUT_RFIPQCD(MNUMPR,MNUMYR,'2_M2') ) /
      LFMMOUT_RFIPQCD('10_MNUMPR',MNUMYR,'2_M2') ;
    LFMMOUT_RFIPQCD(MNUMPR,MNUMYR,'1_M2')$(LFMMOUT_RFIPQCD(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    PMMRPT_RFPQUFC(MNUMPR,MNUMYR,'2_M2') =
      sum((UFOImpStr,DomRefReg)$(RefReg2MNUMPR(MNUMPR,DomRefReg)),
        IMPORTS.l(UFOImpStr,DomRefReg,Period) ) /1000 ;

    PMMRPT_RFPQUFC('10_MNUMPR',MNUMYR,'2_M2') =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMRPT_RFPQUFC(MNUMPR,MNUMYR,'2_M2') ) ;

    PMMRPT_RFPQUFC(MNUMPR,MNUMYR,'1_M2')$((sum((UFOImpStr('AR3'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
                                           IMPORTS.l(UFOImpStr,DomRefReg,Period) ) +
                                         sum((UFOImpStr('GO3'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
                                           IMPORTS.l(UFOImpStr,DomRefReg,Period) ) +
                                         sum((UFOImpStr('MN3'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
                                           IMPORTS.l(UFOImpStr,DomRefReg,Period) ) )>0) =
      (sum((UFOImpStr('AR3'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        StreamBalance.m(DomRefReg,'COKING',UFOImpStr,Period) * IMPORTS.l(UFOImpStr,DomRefReg,Period)/GDP(t) ) +
      sum((UFOImpStr('GO3'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        StreamBalance.m(DomRefReg,'COKING',UFOImpStr,Period) * IMPORTS.l(UFOImpStr,DomRefReg,Period)/GDP(t) ) +
      sum((UFOImpStr('MN3'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        StreamBalance.m(DomRefReg,'COKING',UFOImpStr,Period) * IMPORTS.l(UFOImpStr,DomRefReg,Period)/GDP(t) ) ) /
      (sum((UFOImpStr('AR3'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(UFOImpStr,DomRefReg,Period) ) +
      sum((UFOImpStr('GO3'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(UFOImpStr,DomRefReg,Period) ) +
      sum((UFOImpStr('MN3'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l(UFOImpStr,DomRefReg,Period) ) ) ;

    PMMRPT_RFPQUFC('10_MNUMPR',MNUMYR,'1_M2')$(PMMRPT_RFPQUFC('10_MNUMPR',MNUMYR,'2_M2')>0) =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMRPT_RFPQUFC(MNUMPR,MNUMYR,'1_M2')*PMMRPT_RFPQUFC(MNUMPR,MNUMYR,'2_M2') ) /
        PMMRPT_RFPQUFC('10_MNUMPR',MNUMYR,'2_M2') ;
    PMMRPT_RFPQUFC(MNUMPR,MNUMYR,'1_M2')$(PMMRPT_RFPQUFC(MNUMPR,MNUMYR,'2_M2')=0) = 0.0 ;

    PMMRPT_RFMTBI(MNUMPR,MNUMYR) = 0 ;

    PMMRPT_RFMETI(MNUMPR,MNUMYR) =
      sum(DomRefReg$RefReg2MNUMPR(MNUMPR,DomRefReg),
        IMPORTS.l('MET',DomRefReg,Period) ) /1000 ;

    PMMRPT_RFMETI('10_MNUMPR',MNUMYR) =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMRPT_RFMETI(MNUMPR,MNUMYR) ) ;

    PMMRPT_RFQEXCRD(MNUMPR,MNUMYR) =
      sum((DomRefReg,Crude,Source,Step)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CRUDEEXPORT.l(DomRefReg,Crude,Source,Step,Period) ) ;

    PMMRPT_RFQEXCRD('10_MNUMPR',MNUMYR) =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMRPT_RFQEXCRD(MNUMPR,MNUMYR) ) ;

    PMMOUT_RFSPRFR(MNUMYR) =
      sum((DomRefReg,Crude),
        npv_SPR_Withdraw(DomRefReg,Crude,Period)) / 1000 ;

    PMMRPT_RFCRDOTH(MNUMPR,MNUMYR) =
      sum((DomRefReg,Crude)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        npv_SPR_Withdraw(DomRefReg,Crude,Period) ) / 1000 ;

    PMMRPT_RFCRDOTH('10_MNUMPR',MNUMYR) =
       sum((MNUMPR,DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
         PMMRPT_RFCRDOTH(MNUMPR,MNUMYR) ) ;

    PMMFTAB_PALBOB(MNUMCR,MNUMYR)             = 0.0;
    PMMFTAB_PALBOB('11_United_States',MNUMYR) =
      sum((DomRefReg,RefType,GasSpecProd,RcpMode)$RecipeBlending(RcpMode,GasSpecProd),
        ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,GasSpecProd,Period)*GasSpecBalance.m(DomRefReg,RefType,GasSpecProd,Period) ) /
      sum((DomRefReg,RefType,GasSpecProd,RcpMode)$RecipeBlending(RcpMode,GasSpecProd),
        ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,GasSpecProd,Period) ) / GDP(t);

    PMMFTAB_PALBOB('11_United_States',MNUMYR) =
      max(PMMFTAB_PALBOB('11_United_States',MNUMYR),0.0001) ;

*   Wholesale gasoline price in 87$/bbl
    PMMFTAB_PALMG(MNUMCR,MNUMYR)             = 0.0 ;
    PMMFTAB_PALMG('11_United_States',MNUMYR) =
      PMMFTAB_PALBOB('11_United_States',MNUMYR) ;

    PMMFTAB_WS_RBOB(MNUMCR,MNUMYR)             = 0.0 ;

    PMMFTAB_WS_RBOB(MNUMCR,MNUMYR)=
      sum((DomRefReg,GasSpecProd('RBOB'),ActiveDem)$crN2L(ActiveDem,MNUMCR),
        BOBBalance.m(DomRefReg,GasSpecProd,Period)* CenDiv_to_RefReg_ACU_Frac(DomRefReg,ActiveDem)) / GDP(t);

    PMMFTAB_WS_RBOB('11_United_States',MNUMYR) =
      sum((DomRefReg,RefType,GasSpecProd('RBOB'),RcpMode)$RecipeBlending(RcpMode,GasSpecProd),
        ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,GasSpecProd,Period)*BOBBalance.m(DomRefReg,GasSpecProd,Period) ) /
      sum((DomRefReg,RefType,GasSpecProd('RBOB'),RcpMode)$RecipeBlending(RcpMode,GasSpecProd),
        ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,GasSpecProd,Period) ) / GDP(t);

*   Only has crude and imports from non-Canada/Caribbean, need to add imports from 9_RefReg.
*   Also, "fixed" imports are currently free in the objective function so we have to decide what price to use here.
*   no DELTA adjustment here
    PMMFTAB_RFIMPEXPEND(MNUMYR) =
      (sum((DomRefReg,Crude,Source,Step),
        CrudeBalance.m(DomRefReg,Crude,Period)*CRUDETRANS.l(DomRefReg,Crude,Source,Step,Period) ) +
       sum((ImpStr,INTREG,Step), npv_ImportPrice(ImpStr,INTREG,Step,Period)*PRODIMP.l(ImpStr,INTREG,Step,Period) )
      ) * 365 / 1000000 / GDP(t) ;

    PMMRPT_RFQICRD(MNUMPR,MNUMYR) =
      sum((DomRefReg,Crude,Source,Step)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CRUDETRANS.l(DomRefReg,Crude,Source,Step,Period) ) / 1000 ;

    PMMRPT_RFQICRD('10_MNUMPR',MNUMYR) =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMRPT_RFQICRD(MNUMPR,MNUMYR) ) ;

    PMMRPT_BLDIMP(MNUMPR,MNUMYR) =
     (sum(DomRefReg$RefReg2MNUMPR(MNUMPR,DomRefReg), IMPORTS.l('CBOB',DomRefReg,Period) ) +
      sum(DomRefReg$RefReg2MNUMPR(MNUMPR,DomRefReg), IMPORTS.l('RBOB',DomRefReg,Period) ) +
      sum((DomRefReg,TranMode,GasSpecProd)$(REFtoREFTranCost('9_REFREG',DomRefReg,TranMode,GasSpecProd) and RefReg2MNUMPR(MNUMPR,DomRefReg)),
        RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,GasSpecProd,Period) ) ) / 1000 ;

    PMMRPT_BLDIMP('10_MNUMPR',MNUMYR) =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMRPT_BLDIMP(MNUMPR,MNUMYR) ) ;

    PMMRPT_RFQEXPRDT(MNUMPR,MNUMYR) =
      sum((ImpStr,DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        EXPORTS.l(ImpStr,DomRefReg,Period) ) / 1000 +
      sum((NGLProduct,DomRefReg,ActiveDem)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        CenDiv_to_RefReg_ACU_Frac(DomRefReg,ActiveDem)*NGLEXPORTS.l(NGLProduct,ActiveDem,Period) ) / 1000 +
      sum((DomRefReg,RefType)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        COPRODUCTS.l(DomRefReg,RefType,'COKdump',Period) ) / 1000 +
      sum((DomRefReg,RefType)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        COPRODUCTS.l(DomRefReg,RefType,'GOPout',Period) ) / 1000 ;


    PMMRPT_RFQEXPRDT('10_MNUMPR',MNUMYR) =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMRPT_RFQEXPRDT(MNUMPR,MNUMYR) ) ;

    PMMRPT_RFIMTP(MNUMPR,MNUMYR) =
      PMMRPT_RFPQIPRDT(MNUMPR,MNUMYR,'2_M2') -
      PMMRPT_RFQEXPRDT(MNUMPR,MNUMYR) +
      PMMRPT_BLDIMP(MNUMPR,MNUMYR) +
      PMMRPT_RFPQUFC(MNUMPR,MNUMYR,'2_M2') +
      PMMRPT_RFMTBI(MNUMPR,MNUMYR) ;

    PMMRPT_RFIMTP('10_MNUMPR',MNUMYR) =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMRPT_RFIMTP(MNUMPR,MNUMYR) ) ;

*   Seed oil to GDT unit (MMbbl / day)
    PMMFTAB_SBO2GDTPD(MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('GDT'),ProcessMode('GDT_FCO'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
         abs(ProcessTable('FCO',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) / 1000 ;

    PMMFTAB_SBO2GDTPD('10_MNUMPR',MNUMYR)   =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMFTAB_SBO2GDTPD(MNUMPR,MNUMYR) ) ;

*   8/15/22 esh
*   Seed oil to SAF unit (MMbbl / day)
    PMMFTAB_SBO2SAFPD(MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('SAF'),ProcessMode('GDT_FCO'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
         abs(ProcessTable('FCO',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) / 1000 +
      sum((DomRefReg,RefType,Process('SAF'),ProcessMode('SAF_FCO'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
         abs(ProcessTable('FCO',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) / 1000;

    PMMFTAB_SBO2SAFPD('10_MNUMPR',MNUMYR)   =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMFTAB_SBO2SAFPD(MNUMPR,MNUMYR) ) ;

*   Green diesel from seed oil
    LFMMOUT_SBOQGD(MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('GDT'),ProcessMode('GDT_FCO'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        ProcessTable('RNH',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) +
        ProcessTable('RDH',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) +
      sum((DomRefReg,RefType,Process('SAF'),ProcessMode('GDT_FCO'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        ProcessTable('RDH',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) )  ;

    LFMMOUT_SBOQGD('10_MNUMPR',MNUMYR)   =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_SBOQGD(MNUMPR,MNUMYR) ) ;

* 8/15/22 esh
* HEFA-SPK renewable jet from seed oil
    LFMMOUT_SBOQRJH(MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('SAF'),ProcessMode('SAF_FCO'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        ProcessTable('RJH',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) )  ;

    LFMMOUT_SBOQRJH('10_MNUMPR',MNUMYR)   =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_SBOQRJH(MNUMPR,MNUMYR) ) ;

*   Seed oil to GDT
*   8/15/22 esh: seed oil used to produce renewable diesel
    PMMFTAB_SBOQTYCD('11_United_States',MNUMYR) =
      sum((DomRefReg,RefType,Process('GDT'),ProcessMode('GDT_FCO')),
         abs(ProcessTable('FCO',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) +
      sum((DomRefReg,RefType,Process('SAF'),ProcessMode('SAF_FCO')),
         abs(ProcessTable('FCO',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) ;

*   Yellow grease to GDT unit (MMbbl / day)
    PMMFTAB_YGR2GDTPD(MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('GDT'),ProcessMode('GDT_YGR'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
         abs(ProcessTable('YGR',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) / 1000  ;

    PMMFTAB_YGR2GDTPD('10_MNUMPR',MNUMYR)   =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMFTAB_YGR2GDTPD(MNUMPR,MNUMYR) ) ;

*   Green liquids from yellow grease
    LFMMOUT_YGRQGD(MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('GDT'),ProcessMode('GDT_YGR'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
         ProcessTable('RNH',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) +
         ProcessTable('RDH',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) +
      sum((DomRefReg,RefType,Process('SAF'),ProcessMode('GDT_YGR'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        ProcessTable('RDH',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) )  ;

    LFMMOUT_YGRQGD('10_MNUMPR',MNUMYR)   =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_YGRQGD(MNUMPR,MNUMYR) ) ;

*   White grease to GDT unit (MMbbl / day)
    PMMFTAB_WGR2GDTPD(MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('GDT'),ProcessMode('GDT_WGR'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
         abs(ProcessTable('WGR',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) / 1000 ;

    PMMFTAB_WGR2GDTPD('10_MNUMPR',MNUMYR)   =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMFTAB_WGR2GDTPD(MNUMPR,MNUMYR) ) ;

*   White grease to SAF unit (MMbbl / day)
    PMMFTAB_WGR2SAFPD(MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('SAF'),ProcessMode('GDT_WGR'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
         abs(ProcessTable('WGR',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) / 1000 +
       sum((DomRefReg,RefType,Process('SAF'),ProcessMode('SAF_WGR'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
         abs(ProcessTable('WGR',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) / 1000 ;

    PMMFTAB_WGR2SAFPD('10_MNUMPR',MNUMYR)   =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMFTAB_WGR2SAFPD(MNUMPR,MNUMYR) ) ;

*   Green liquids from white grease
    LFMMOUT_WGRQGD(MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('GDT'),ProcessMode('GDT_WGR'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
         ProcessTable('RNH',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) +
         ProcessTable('RDH',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) +
      sum((DomRefReg,RefType,Process('SAF'),ProcessMode('GDT_WGR'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        ProcessTable('RDH',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) )  ;

    LFMMOUT_WGRQGD('10_MNUMPR',MNUMYR)   =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_WGRQGD(MNUMPR,MNUMYR) ) ;

*   HEFA-SPK renewable jet from white grease
    LFMMOUT_WGRQRJH(MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('SAF'),ProcessMode('SAF_WGR'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        ProcessTable('RJH',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) )  ;

    LFMMOUT_WGRQRJH('10_MNUMPR',MNUMYR)   =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_WGRQRJH(MNUMPR,MNUMYR) ) ;

*   Green diesel from white grease
    LFMMOUT_WGRQRDH(MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('GDT'),ProcessMode('GDT_WGR'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
         ProcessTable('RDH',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) +
      sum((DomRefReg,RefType,Process('SAF'),ProcessMode('GDT_WGR'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        ProcessTable('RDH',Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) )  ;

    LFMMOUT_WGRQRDH('10_MNUMPR',MNUMYR)   =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_WGRQRDH(MNUMPR,MNUMYR) ) ;

    PMMFTAB_SBO_PRICE('11_United_States',MNUMYR)$(sum(DomRefReg, sum(step,REFPURCH.l(DomRefReg,'FCO',Step,Period)) )>0) =
      sum(DomRefReg, sum(step,REFPURCH.l(DomRefReg,'FCO',Step,Period))*RefPurchBal.m(DomRefReg,'FCO',Period) ) /
      sum(DomRefReg, sum(step,REFPURCH.l(DomRefReg,'FCO',Step,Period)) ) / agGDP87(t);

*    PMMFTAB_YGR_PRICE('11_United_States',MNUMYR)$(sum(DomRefReg, sum(step,REFPURCH.l(DomRefReg,'YGR',Step,Period)) )>0) =
*      sum(DomRefReg, sum(step,REFPURCH.l(DomRefReg,'YGR',Step,Period))*RefPurchBal.m(DomRefReg,'YGR',Period) ) /
*      sum(DomRefReg, sum(step,REFPURCH.l(DomRefReg,'YGR',Step,Period)) ) / GDP(t);

    PMMFTAB_WGR_PRICE('11_United_States',MNUMYR)$(sum(DomRefReg, sum(step,REFPURCH.l(DomRefReg,'WGR',Step,Period)) )>0) =
      sum(DomRefReg, sum(step,REFPURCH.l(DomRefReg,'WGR',Step,Period))*RefPurchBal.m(DomRefReg,'WGR',Period) ) /
      sum(DomRefReg, sum(step,REFPURCH.l(DomRefReg,'WGR',Step,Period)) ) / agGDP87(t);

    PMMFTAB_YGR_PRICE('11_United_States',MNUMYR) =  PMMFTAB_WGR_PRICE('11_United_States',MNUMYR);

*   There are currently no palm oil imports in the LFMM
    PMMFTAB_PLMQTYCD(MNUMCR,MNUMYR) = 0 ;


    PMMRPT_BIODIMP(MNUMCR,MNUMYR)             = 0.0 ;
    PMMRPT_BIODIMP('11_United_States',MNUMYR) =
      sum((Step), BIODIMP.l(Step,Period) ) ;

    LFMMOUT_BIODIMPPD(MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        BIODIMPref.l(DomRefReg,RefType,Period) ) ;

    LFMMOUT_BIODIMPPD('10_MNUMPR',MNUMYR) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_BIODIMPPD(MNUMPR,MNUMYR) ) ;

* 2/18/20: adding LFMMOUT_RenewDIMP and LFMMOUT_RenewDImpPD

    LFMMOUT_RenewDIMP(MNUMCR,MNUMYR)             = 0.0 ;
    LFMMOUT_RenewDIMP('11_United_States',MNUMYR) =
      sum((Step), RENEWDIMP.l(Step,Period) ) ;

    LFMMOUT_RenewDImpPD(MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType)$RefReg2MNUMPR(MNUMPR,DomRefReg),
        RENEWDIMPref.l(DomRefReg,RefType,Period) ) ;

    LFMMOUT_RenewDImpPD('10_MNUMPR',MNUMYR) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_RenewDImpPD(MNUMPR,MNUMYR) ) ;

* TEMP TEMP TEMP - combine renewD and bioD imports into BIODIMP for FTAB reporting - TEMP TEMP TEMP
*    PMMRPT_BIODIMP('11_United_States',MNUMYR) =
*    PMMRPT_BIODIMP('11_United_States',MNUMYR) + LFMMOUT_RenewDIMP('11_United_States',MNUMYR) ;

* this eq includes US total, so don't need to do it separately
*    LFMMOUT_BIODIMPPD(MNUMPR,MNUMYR) =
*    LFMMOUT_BIODIMPPD(MNUMPR,MNUMYR) + LFMMOUT_RenewDImpPD(MNUMPR,MNUMYR) ;

*   There are currently no biodiesel exports in the LFMM
    PMMRPT_BIODEXP(MNUMCR,MNUMYR) = 0 ;

    PMMFTAB_RFENVFX(MNUMCR,MNUMYR,M20) = 0 ;
    PMMFTAB_RFENVFX(MNUMCR,MNUMYR,'02_M20') =
      sum(ActiveDem$crN2L(ActiveDem,MNUMCR),  EnvMarkups(ActiveDem,'M') ) ;
    PMMFTAB_RFENVFX(MNUMCR,MNUMYR,'06_M20') =
      sum(ActiveDem$crN2L(ActiveDem,MNUMCR),  EnvMarkups(ActiveDem,'J') ) ;
    PMMFTAB_RFENVFX(MNUMCR,MNUMYR,'07_M20') =
      sum(ActiveDem$crN2L(ActiveDem,MNUMCR),  EnvMarkups(ActiveDem,'D') ) ;
    PMMFTAB_RFENVFX(MNUMCR,MNUMYR,'13_M20') =
      sum(ActiveDem$crN2L(ActiveDem,MNUMCR),  EnvMarkups(ActiveDem,'D') ) ;

*   Weighted-average marginal price in 87$ per bbl
    PMMRPT_PETHM('11_United_States',MNUMYR) =
      sum((DomRefReg,RefType,RcpMode,EthStream)$RecipeBlending(RcpMode,EthStream),
        EthBalance.m(DomRefReg,EthStream,Period)*ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period) ) /
      sum((DomRefReg,RefType,RcpMode,EthStream)$RecipeBlending(RcpMode,EthStream),
        ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period) ) / GDP(t) ;

    PMMRPT_PETHANOL('11_United_States',MNUMYR) =
      sum((DomRefReg,RefType,RcpMode,EthStream)$RecipeBlending(RcpMode,EthStream),
        RecipeBalance.m(DomRefReg,RefType,RcpMode,EthStream,Period)*ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period) ) /
      sum((DomRefReg,RefType,RcpMode,EthStream)$RecipeBlending(RcpMode,EthStream),
        ToRECIPEBLEND.l(DomRefReg,RefType,RcpMode,EthStream,Period) ) / GDP(t) ;

    CONVFACT_APICRDIMP('1_M2',MNUMYR) = 36.1746;
    CONVFACT_APICRDIMP('1_M2',MNUMYR)$(sum((DomRefReg,Crude,Source,step), CRUDETRANS.l(DomRefReg,Crude,Source,Step,Period)) > 0) =
      sum((DomRefReg,Crude), StreamProp(Crude,'API') * sum((Source,step), CRUDETRANS.l(DomRefReg,Crude,Source,Step,Period) ) ) /
      sum((DomRefReg,Crude,Source,step), CRUDETRANS.l(DomRefReg,Crude,Source,Step,Period) ) ;

    CONVFACT_APICRDEXP('1_M2',MNUMYR) = 36.1746;
    CONVFACT_APICRDEXP('1_M2',MNUMYR)$(sum((DomRefReg,Crude,Source,step), CRUDEEXPORT.l(DomRefReg,Crude,Source,Step,Period)) > 0) =
      sum((DomRefReg,Crude), StreamProp(Crude,'API') * sum((Source,step), CRUDEEXPORT.l(DomRefReg,Crude,Source,Step,Period) ) ) /
      sum((DomRefReg,Crude,Source,step), CRUDEEXPORT.l(DomRefReg,Crude,Source,Step,Period) ) ;

    CONVFACT_CFCRDLTSWT(MNUMYR) = StreamProp('L_Sweet','END') ;
    CONVFACT_CFCRDLTSOUR(MNUMYR) = StreamProp('L_Sour','END') ;
    CONVFACT_CFCRDMD2SOUR(MNUMYR) = StreamProp('M_Msour','END') ;
    CONVFACT_CFCRDMDSOUR(MNUMYR) = StreamProp('M_Sour','END') ;
    CONVFACT_CFCRDHVSWT(MNUMYR) = StreamProp('H_Sweet','END') ;
    CONVFACT_CFCRDHVSOUR(MNUMYR) = StreamProp('H_Sour','END') ;
    CONVFACT_CFCRDCA(MNUMYR) = StreamProp('Calif','END') ;
    CONVFACT_CFCRDSYN(MNUMYR) = StreamProp('Syncrude','END') ;
    CONVFACT_CFCRDDILBIT(MNUMYR) = StreamProp('Dilbit','END') ;
    CONVFACT_CFCRDLT2SWT(MNUMYR) = StreamProp('UL_Sweet','END') ;
    CONVFACT_CFCRDLSCOND(MNUMYR) = StreamProp('Condensate','END') ;
    CONVFACT_APILTSW('1_M2',MNUMYR) = StreamProp('L_Sweet','API') ;
    CONVFACT_APILTSO('1_M2',MNUMYR) = StreamProp('L_Sour','API') ;
    CONVFACT_APIMMSO('1_M2',MNUMYR) = StreamProp('M_Msour','API') ;
    CONVFACT_APIMDSO('1_M2',MNUMYR) = StreamProp('M_Sour','API') ;
    CONVFACT_APIHVSW('1_M2',MNUMYR) = StreamProp('H_Sweet','API') ;
    CONVFACT_APIHVSO('1_M2',MNUMYR) = StreamProp('H_Sour','API') ;
    CONVFACT_APICA('1_M2',MNUMYR) = StreamProp('Calif','API') ;
    CONVFACT_APISYN('1_M2',MNUMYR) = StreamProp('Syncrude','API') ;
    CONVFACT_APIDIL('1_M2',MNUMYR) = StreamProp('Dilbit','API') ;
    CONVFACT_APILLSW('1_M2',MNUMYR) = StreamProp('UL_Sweet','API') ;
    CONVFACT_API50PL('1_M2',MNUMYR) = StreamProp('Condensate','API') ;

    CONVFACT_APICRDDOM('1_M2',MNUMYR) =
      sum((DomRefReg,Crude), StreamProp(Crude,'API') * npv_DomesticCrudeSup(DomRefReg,Crude,Period) ) /
      sum((DomRefReg,Crude), npv_DomesticCrudeSup(DomRefReg,Crude,Period) ) ;

* Net NGL totals by NGL stream in M bbl per day
    PMMOUT_RFQNGPL(MNUMPR,MNUMYR,'1_M6') =
       sum((NGLInputStr('CC2ngl'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
         npv_NGLBounds(NGLInputStr,DomRefReg,Period) ) ;
*+      sum((DomRefReg,RefType,RcpMode('RCP_CC2'),Stream('CC2ngl')),
*         abs(RecipeBlending(RcpMode,Stream))*RECIPEMODE.l(DomRefReg,RefType,RcpMode,Period) ) ;

    PMMOUT_RFQNGPL('10_MNUMPR',MNUMYR,'1_M6') =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMOUT_RFQNGPL(MNUMPR,MNUMYR,'1_M6') ) ;

    PMMOUT_RFQNGPL(MNUMPR,MNUMYR,'2_M6') =
       sum((NGLInputStr('CC3ngl'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
         npv_NGLBounds(NGLInputStr,DomRefReg,Period) ) ;
*+       sum((DomRefReg,RefType,RcpMode('RCP_CC3a'),Stream('CC3')),
*         abs(RecipeBlending(RcpMode,Stream))*RECIPEMODE.l(DomRefReg,RefType,RcpMode,Period) ) ;

    PMMOUT_RFQNGPL('10_MNUMPR',MNUMYR,'2_M6') =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMOUT_RFQNGPL(MNUMPR,MNUMYR,'2_M6') ) ;

    PMMOUT_RFQNGPL(MNUMPR,MNUMYR,'3_M6') =
       sum((NGLInputStr('NC4ngl'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
         npv_NGLBounds(NGLInputStr,DomRefReg,Period) ) ;
*+       sum((DomRefReg,RefType,RcpMode('RCP_NC4a'),Stream('NC4')),
*         abs(RecipeBlending(RcpMode,Stream))*RECIPEMODE.l(DomRefReg,RefType,RcpMode,Period) ) ;

    PMMOUT_RFQNGPL('10_MNUMPR',MNUMYR,'3_M6') =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMOUT_RFQNGPL(MNUMPR,MNUMYR,'3_M6') ) ;

    PMMOUT_RFQNGPL(MNUMPR,MNUMYR,'4_M6') =
       sum((NGLInputStr('IC4ngl'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
         npv_NGLBounds(NGLInputStr,DomRefReg,Period) ) ;
*+       sum((DomRefReg,RefType,RcpMode('RCP_IC4a'),Stream('IC4')),
*         abs(RecipeBlending(RcpMode,Stream))*RECIPEMODE.l(DomRefReg,RefType,RcpMode,Period) ) ;

    PMMOUT_RFQNGPL('10_MNUMPR',MNUMYR,'4_M6') =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMOUT_RFQNGPL(MNUMPR,MNUMYR,'4_M6') ) ;

    PMMOUT_RFQNGPL(MNUMPR,MNUMYR,'5_M6') =
       sum((NGLInputStr('NATngl'),DomRefReg)$RefReg2MNUMPR(MNUMPR,DomRefReg),
          npv_NGLBounds(NGLInputStr,DomRefReg,Period) ) ;

    PMMOUT_RFQNGPL('10_MNUMPR',MNUMYR,'5_M6') =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMOUT_RFQNGPL(MNUMPR,MNUMYR,'5_M6') ) ;

    PMMOUT_RFQNGPL(MNUMPR,MNUMYR,'6_M6') =
      sum(M6$(ord(M6)<6), PMMOUT_RFQNGPL(MNUMPR,MNUMYR,M6) ) ;

*   MM bbl/cd
*=  PMMOUT_RFPQNGL('10_MNUMPR',MNUMYR,'6_M6','2_M2') =
*=    PMMOUT_RFQNGPL('10_MNUMPR',MNUMYR,'6_M6') /1000 ;
    PMMOUT_RFPQNGL(MNUMPR,MNUMYR,M6,'2_M2') =
      PMMOUT_RFQNGPL(MNUMPR,MNUMYR,M6) /1000 ;
*====      sum(OGDIST, OGSMOUT_OGNGPLPRD(OGDIST,MNUMYR) ) ;

    if (PMMOUT_RFQNGPL('10_MNUMPR',MNUMYR,'6_M6')>0,
           CONVFACT_CFNGL(MNUMYR) =
          (sum((NGLInputStr('CC2ngl'),DomRefReg), CONVFACT_CFEEQ * npv_NGLBounds(NGLInputStr,DomRefReg,Period) ) +
           sum((NGLInputStr('CC3ngl'),DomRefReg), CONVFACT_CFPRQ * npv_NGLBounds(NGLInputStr,DomRefReg,Period) ) +
           sum((NGLInputStr('NC4ngl'),DomRefReg), CONVFACT_CFBUQ * npv_NGLBounds(NGLInputStr,DomRefReg,Period) ) +
           sum((NGLInputStr('IC4ngl'),DomRefReg), CONVFACT_CFIBQ * npv_NGLBounds(NGLInputStr,DomRefReg,Period) ) +
           sum((NGLInputStr('NATngl'),DomRefReg), CONVFACT_CFPPQ * npv_NGLBounds(NGLInputStr,DomRefReg,Period) ) )
      /  PMMOUT_RFQNGPL('10_MNUMPR',MNUMYR,'6_M6') ;
    else
        CONVFACT_CFNGL(MNUMYR) = 3.6 ;
    );


******* NG used to make H2 at refinery, MM bfoe/cd, by LFMM region

* MM bfoe/cd, by LFMM region

    PMMFTAB_RFHCXH2IN(MNUMPR,MNUMYR) =
      sum((DomRefReg,RefType,Process('H2P'),ProcessMode('H2P_NGS'))$RefReg2MNUMPR(MNUMPR,DomRefReg),
        abs(ProcessTable('NGS',Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) / 1000 ;

    PMMFTAB_RFHCXH2IN('10_MNUMPR',MNUMYR) =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMFTAB_RFHCXH2IN(MNUMPR,MNUMYR) ) ;


* trill BTU/yr, by Census Div

    PMMFTAB_RFQNGPF(MNUMCR,MNUMYR) =
      sum((MNUMPR,DomRefReg,ActiveDem)$(crN2L(ActiveDem,MNUMCR) and RefReg2MNUMPR(MNUMPR,DomRefReg) and (ord(MNUMPR)<9)),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem) * PMMFTAB_RFHCXH2IN(MNUMPR,MNUMYR)) * CONVFACT_CFRSQ * 365.;

    PMMFTAB_RFQNGPF('11_United_States',MNUMYR) =
      PMMFTAB_RFHCXH2IN('10_MNUMPR',MNUMYR)* CONVFACT_CFRSQ * 365.;

* -- add to QNGRF, trill BTU/yr, by Census Div
    QBLK_QNGRF(MNUMCR,MNUMYR) = QBLK_QNGRF(MNUMCR,MNUMYR) + PMMFTAB_RFQNGPF(MNUMCR,MNUMYR) ;

* TEMP TEMP TEMP TEMP
* for accounting, now add hydrogen purch from IND ( HYDROGEN2REF, MMbfoe/yr ) to NG,H2 to refinery (RFHCXH2IN, MMbfoe/cd)
* ASSUMED to go into RefReg 4 only

    PMMFTAB_RFHCXH2IN('04_MNUMPR',MNUMYR) = PMMFTAB_RFHCXH2IN('04_MNUMPR',MNUMYR) +
                                            INDOUT_HYDROGEN2REF(MNUMYR)/365. ;

    PMMFTAB_RFHCXH2IN('10_MNUMPR',MNUMYR) =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMFTAB_RFHCXH2IN(MNUMPR,MNUMYR) ) ;
* TEMP TEMP TEMP TEMP

*******

    LFMMOUT_RFCRUDEWHP(MNUMPR,MNCRUD,MNUMYR) =
      sum((DomRefReg,Crude)$(RefReg2MNUMPR(MNUMPR,DomRefReg) and MNCRUD_2_Crude(MNCRUD,Crude)),
        (CrudeBalance.m(DomRefReg,Crude,Period)-CrudeBalance_M_DELTA(DomRefReg,Crude,Period)) ) / GDP(t) ;



* mc6, 2-13-2016
* Only adjust the rent if it's positive to begin with. Use a random domestic RefReg, Crude as a guide
*   if(CrudeWHP_rent('2_REFREG','L_Sweet','2015')>0,
    if(LFMMOUT_lfREFRENT >0,

      if(NCNTRL_CURCALYR>2020,

* 87$/bbl
*
        CrudeWHP_rent(DomRefReg,Crude,t) =
        (15.0/GDP('2013')) -
        (
          ((CrudeSupCurveForeign.m('Foreign','L_Sweet',Period))-CrudeBalance.m('2_REFREG','L_Sweet',Period))/GDP(t)
        );

* 87$/bbl
*

*===  ALLOW negative rent for aeo2015, highresource
*===  CrudeWHP_rent(DomRefReg,Crude,t)$(CrudeWHP_rent(DomRefReg,Crude,t)<0) = 0.0001;
        CrudeWHP_rent(DomRefReg,Crude,t) = min(TargetRENT/GDP('2013'),CrudeWHP_rent(DomRefReg,Crude,t));
        CrudeWHP_rent(DomRefReg,'Condensate',t) = min(TargetRENTcond/GDP('2013'),CrudeWHP_rent(DomRefReg,'Condensate',t));

      );
    );

    LFMMOUT_RFCRUDEWHP(MNUMPR,MNCRUD,MNUMYR) =
      sum((DomRefReg,Crude)$(RefReg2MNUMPR(MNUMPR,DomRefReg) and MNCRUD_2_Crude(MNCRUD,Crude)),
         (LFMMOUT_RFCRUDEWHP(MNUMPR,MNCRUD,MNUMYR) - CrudeWHP_rent(DomRefReg,Crude,t)) ) ;



* AB-32 refinery emissions in MM tonnes Ceq per year
    AB32_AB_COVD_EM_REF(MNUMYR)                          = 0 ;
    AB32_AB_COVD_EM_REF(MNUMYR)$(AB32SW and t.val>=2013) =
      sum((Stream('CO2'),Process,ProcessMode,RefType)$(ProcessTable(Stream,Process,ProcessMode) and SupTypMode('7_RefReg',RefType,Process,ProcessMode) and not BioProcess(Process)),
        ProcessTable(Stream,Process,ProcessMode)* PROCMODE.l('7_RefReg',RefType,Process,ProcessMode,Period)) *
      365 / 3.67 / 1000 ;

* RFS waiver prices in 87$ per barrel
    LFMMOUT_RFS_WAIVER(M4,MNUMYR) =
      sum(RFSCategory$M4_2_RFSCat(M4,RFSCategory) , RFSWaiverPrice(RFSCategory,t) ) ;

* RFS contribution to diesel price in 87$ per BBL
    LFMMOUT_RFSDSTR(MNUMCR,MNUMYR) =
      sum(RFSCategory, RFSFraction(RFSCategory,Period) * npv_PetDSFrac(Period) * RFSConstraintsRQM.m(RFSCategory,Period) ) / GDP(t) ;

* RFS contribution to motor gasoline price in 87$ per BBL
    LFMMOUT_RFSMGTR(MNUMCR,MNUMYR) =
     (sum((RFSCategory,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        0.9*(QMG10.l(ActiveDem,Period)/QMG.l(ActiveDem,Period))*RFSFraction(RFSCategory,Period) * npv_PetMGFrac_noETH(Period) * RFSConstraintsRQM.m(RFSCategory,Period) ) +
      sum((RFSCategory,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        0.85*(QMG15.l(ActiveDem,Period)/QMG.l(ActiveDem,Period))*RFSFraction(RFSCategory,Period) * npv_PetMGFrac_noETH(Period) * RFSConstraintsRQM.m(RFSCategory,Period) ) +
      sum((RFSCategory,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        0.84*(QMGb16.l(ActiveDem,Period)/QMG.l(ActiveDem,Period))*RFSFraction(RFSCategory,Period) * npv_PetMGFrac_noETH(Period) * RFSConstraintsRQM.m(RFSCategory,Period) ) ) / GDP(t) ;

    LFMMOUT_RFSMGTR('11_United_States',MNUMYR) =
      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR),QMG.l(ActiveDem,Period)*LFMMOUT_RFSMGTR(MNUMCR,MNUMYR) ) /
      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR),QMG.l(ActiveDem,Period) ) ;

* RFS contribution to RBOB price in 87$ per BBL
* zero for RBOB, since this actually is a marginal production cost at the petroleum refinery
    LFMMOUT_RFSRBOB(MNUMCR,MNUMYR) = 0 ;

* RFS contribution to jet fuel price in 87$ per BBL
    LFMMOUT_RFSJFTR(MNUMCR,MNUMYR) = 0 ;

* RFS contribution to heating oil price in 87$ per BBL
    LFMMOUT_RFSDSRS(MNUMCR,MNUMYR) = 0 ;

* Refinery Capacity in thousand barrels per day
    LFMMOUT_REF_CAP(M57,MNUMPR,MNUMYR) =
      sum((RefReg,RefType,Process)$(M57_2_Process(M57,Process) and RefReg2MNUMPR(MNUMPR,RefReg)),
        AvailCap(RefReg,RefType,Process) ) ;

    LFMMOUT_REF_CAP(M57,'10_MNUMPR',MNUMYR) =
      sum(MNUMPR$(ord(MNUMPR)<9), LFMMOUT_REF_CAP(M57,MNUMPR,MNUMYR) ) ;

* Refinery Process Utilization
    LFMMOUT_REF_UTL(M57,MNUMPR,MNUMYR) = 0;
    LFMMOUT_REF_UTL(M57,MNUMPR,MNUMYR)$(sum((RefReg,RefType,Process)$(M57_2_Process(M57,Process) and RefReg2MNUMPR(MNUMPR,RefReg)), AvailCap(RefReg,RefType,Process) )>0) =
      sum((RefReg,RefType,Process)$(M57_2_Process(M57,Process) and RefReg2MNUMPR(MNUMPR,RefReg)),
         StreamFactors(Process)*OPERATECAP.l(RefReg,RefType,Process,Period) ) /
      sum((RefReg,RefType,Process)$(M57_2_Process(M57,Process) and RefReg2MNUMPR(MNUMPR,RefReg)),
         AvailCap(RefReg,RefType,Process) ) ;

    LFMMOUT_REF_UTL(M57,'10_MNUMPR',MNUMYR)$(sum((MNUMPR,RefReg,RefType,Process)$(M57_2_Process(M57,Process) and RefReg2MNUMPR(MNUMPR,RefReg) and (ord(MNUMPR)<9)), AvailCap(RefReg,RefType,Process) )>0) =
      sum((MNUMPR,RefReg,RefType,Process)$(M57_2_Process(M57,Process) and RefReg2MNUMPR(MNUMPR,RefReg) and (ord(MNUMPR)<9)),
         StreamFactors(Process)* OPERATECAP.l(RefReg,RefType,Process,Period) ) /
      sum((MNUMPR,RefReg,RefType,Process)$(M57_2_Process(M57,Process) and RefReg2MNUMPR(MNUMPR,RefReg) and (ord(MNUMPR)<9)),
         AvailCap(RefReg,RefType,Process) ) ;


* HARDCODED, updated 7-11-19
* benchmark US level coker (3_M57) util for historical years 2010 - 2018
*   2015 added for aeo2016
*   2016 added for aeo2017
*   2017,2018 added for aeo2020
*   CokerFlowUtilzCalc-PET_PNP_DWNS_DC_NUS_MBBLPD_A.xlsx
*
    LFMMOUT_REF_UTL('03_M57','10_MNUMPR','2010_MNUMYR') =  0.836 ;
    LFMMOUT_REF_UTL('03_M57','10_MNUMPR','2011_MNUMYR') =  0.874 ;
    LFMMOUT_REF_UTL('03_M57','10_MNUMPR','2012_MNUMYR') =  0.871 ;
    LFMMOUT_REF_UTL('03_M57','10_MNUMPR','2013_MNUMYR') =  0.887 ;
    LFMMOUT_REF_UTL('03_M57','10_MNUMPR','2014_MNUMYR') =  0.870 ;
    LFMMOUT_REF_UTL('03_M57','10_MNUMPR','2015_MNUMYR') =  0.876 ;
    LFMMOUT_REF_UTL('03_M57','10_MNUMPR','2016_MNUMYR') =  0.904 ;
    LFMMOUT_REF_UTL('03_M57','10_MNUMPR','2017_MNUMYR') =  0.885 ;
    LFMMOUT_REF_UTL('03_M57','10_MNUMPR','2018_MNUMYR') =  0.890 ;


* Zero out old PMM variables
*== PMMRPT_RFDSTCAP(MNUMPR,MNUMYR) = 0 ;
*== PMMRPT_RFDSTUTL(MNUMPR,MNUMYR) = 0 ;

* All potential CO2 not captured by xTLs in MM tonnes CO2 per year

    PMMOUT_XTL_CO2AVAIL(MNUMPR,MNUMYR) =
      sum((RefReg,RefType)$RefReg2MNUMPR(MNUMPR,RefReg),
        0.85*sum((Process('CTL'),ProcessMode), PROCMODE.l(RefReg,RefType,Process,ProcessMode,Period)*
                  ProcessTable('CO2',Process,ProcessMode)) +
             sum((Process('CTLCCS'),ProcessMode), PROCMODE.l(RefReg,RefType,Process,ProcessMode,Period)*
                  ProcessTable('CO2CCS',Process,ProcessMode)) +
        0.85*sum((Process('CBL'),ProcessMode), PROCMODE.l(RefReg,RefType,Process,ProcessMode,Period)*
                  ProcessTable('CO2',Process,ProcessMode)) +
             sum((Process('CBLCCS'),ProcessMode), PROCMODE.l(RefReg,RefType,Process,ProcessMode,Period)*
                  ProcessTable('CO2CCS',Process,ProcessMode)) ) * 365 / 1000 ;

    PMMOUT_XTL_CO2AVAIL('10_MNUMPR',MNUMYR) =
      sum(MNUMPR$(ord(MNUMPR)<9), PMMOUT_XTL_CO2AVAIL(MNUMPR,MNUMYR) ) ;

* CO2 captured by xTLs in MM tonnes C per year
    EMISSION_CCS_PMM('2_M5',MNUMPR,MNUMYR) =
      sum((RefReg,RefType)$RefReg2MNUMPR(MNUMPR,RefReg),
        sum((Process('CTLCCS'),ProcessMode), PROCMODE.l(RefReg,RefType,Process,ProcessMode,Period)*ProcessTable('CO2CCS',Process,ProcessMode) ) +
        (8.04/11.37)*sum((Process('CBLCCS'),ProcessMode), PROCMODE.l(RefReg,RefType,Process,ProcessMode,Period)*ProcessTable('CO2CCS',Process,ProcessMode) ) ) * (12/44) * 365 / 1000 ;

    EMISSION_CCS_PMM('1_M5',MNUMPR,MNUMYR) =
      sum((RefReg,RefType)$RefReg2MNUMPR(MNUMPR,RefReg),
        (3.33/11.37)*sum((Process('CBLCCS'),ProcessMode), PROCMODE.l(RefReg,RefType,Process,ProcessMode,Period)*ProcessTable('CO2CCS',Process,ProcessMode) ) ) * (12/44) * 365 / 1000 ;

    EMISSION_CCS_PMM(M5,'10_MNUMPR',MNUMYR) =
      sum(MNUMPR$(ord(MNUMPR)<9), EMISSION_CCS_PMM(M5,MNUMPR,MNUMYR) ) ;

    OGSMOUT_OGCO2QLF(M8,MNUMYR) =
      sum((FuelRegion,OGCO2Reg)$OGCO2Reg_2_M8(OGCO2Reg,M8),
        CO2toEOR.l(FuelRegion,OGCO2Reg,Period) ) * 18 * 365 ;

    OGSMOUT_OGCO2QLF('8_M8',MNUMYR) =
      sum(M8$(ord(M8)<8), OGSMOUT_OGCO2QLF(M8,MNUMYR) ) ;

  );    /* end of Per1_t_MNUMYR_loop */

  loop(PrcPeriod(Period),
    COALDtoREFtot(DomRefReg,BioStr) = sum(CoalDReg,BIOXFER.l(CoalDReg,DomRefReg,BioStr,Period));

    COALDtoREFmap2(CoalDReg,DomRefReg,BioStr)$(COALDtoREFtot(DomRefReg,BioStr)>0) =
      BIOXFER.l(CoalDReg,DomRefReg,BioStr,Period)/COALDtoREFtot(DomRefReg,BioStr);

*   BTL (including pyrolysis) Production Demand for Biomass used from the supply curves IN TRILLS
    WRENEW_QBMBTCL(MNMFS1,NDRGN1,MNMYRF)$Per1_MNMYRF(Period,MNMYRF) =

      sum((CoalDReg,BioStr)$(Bio2MNMFS1(BioStr,MNMFS1) and CoalDReg2NDRGN1(CoalDReg,NDRGN1)),
      sum((DomRefReg,RefType,Process('BTL'),ProcessMode),
        COALDtoREFmap2(CoalDReg,DomRefReg,BioStr)*
        abs(ProcessTable(BioStr,Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period))*365/1000 ) +

      sum((CoalDReg,BioStr)$(Bio2MNMFS1(BioStr,MNMFS1) and CoalDReg2NDRGN1(CoalDReg,NDRGN1)),
      sum((DomRefReg,RefType,Process('CBL'),ProcessMode),
        COALDtoREFmap2(CoalDReg,DomRefReg,BioStr)*
        abs(ProcessTable(BioStr,Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period))*365/1000 ) +

      sum((CoalDReg,BioStr)$(Bio2MNMFS1(BioStr,MNMFS1) and CoalDReg2NDRGN1(CoalDReg,NDRGN1)),
      sum((DomRefReg,RefType,Process('CBLCCS'),ProcessMode),
        COALDtoREFmap2(CoalDReg,DomRefReg,BioStr)*
        abs(ProcessTable(BioStr,Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period))*365/1000 ) +

      sum((CoalDReg,BioStr)$(Bio2MNMFS1(BioStr,MNMFS1) and CoalDReg2NDRGN1(CoalDReg,NDRGN1)),
      sum((DomRefReg,RefType,Process('BPU'),ProcessMode),
        COALDtoREFmap2(CoalDReg,DomRefReg,BioStr)*
        abs(ProcessTable(BioStr,Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period))*365/1000 );


    WRENEW_QBMBTCL(MNMFS1,'01_NDRGN1',MNMYRF)$Per1_MNMYRF(Period,MNMYRF) =
      sum((CoalDReg,NDRGN1)$(CoalDReg2NDRGN1(CoalDReg,NDRGN1) and ord(NDRGN1)>1), WRENEW_QBMBTCL(MNMFS1,NDRGN1,MNMYRF) ) ;

    WRENEW_QBMBTCL('1_MNMFS1',NDRGN1,MNMYRF)$Per1_MNMYRF(Period,MNMYRF) =
      sum(MNMFS1$(ord(MNMFS1) > 1), WRENEW_QBMBTCL(MNMFS1,NDRGN1,MNMYRF) ) ;

*   Ethanol Production Demand for Biomass used from the supply curves
    WRENEW_QBMETCL(MNMFS1,NDRGN1,MNMYRF)$Per1_MNMYRF(Period,MNMYRF)  =
      sum((CoalDReg,BioStr)$(Bio2MNMFS1(BioStr,MNMFS1) and CoalDReg2NDRGN1(CoalDReg,NDRGN1)),
      sum((DomRefReg,RefType,Process('CLE'),ProcessMode),
        COALDtoREFmap2(CoalDReg,DomRefReg,BioStr)*
        abs(ProcessTable(BioStr,Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period))*365/1000 ) ;

    WRENEW_QBMETCL(MNMFS1,'01_NDRGN1',MNMYRF)$Per1_MNMYRF(Period,MNMYRF) =
      sum((CoalDReg,NDRGN1)$(CoalDReg2NDRGN1(CoalDReg,NDRGN1) and ord(NDRGN1)>1), WRENEW_QBMETCL(MNMFS1,NDRGN1,MNMYRF) ) ;

    WRENEW_QBMETCL('1_MNMFS1',NDRGN1,MNMYRF)$Per1_MNMYRF(Period,MNMYRF) =
      sum(MNMFS1$(ord(MNMFS1) > 1), WRENEW_QBMETCL(MNMFS1,NDRGN1,MNMYRF) ) ;

*   * Crude imports in M BBL per day
    LFMMOUT_Q_CRUDE_IMPORTA(M10,MNCRUD,MNXYRS)$Per1_MNXYRS('Per1',MNXYRS) =
      sum((RefReg,Crude,Source,Step)$(M10_2_RefReg(M10,RefReg) and MNCRUD_2_Crude(MNCRUD,Crude)),
      CRUDETRANS.l(RefReg,Crude,Source,Step,PrcPeriod) );
    LFMMOUT_Q_CRUDE_IMPORTA('10_M10',MNCRUD,MNXYRS)$Per1_MNXYRS('Per1',MNXYRS) =
      sum(DomM10, LFMMOUT_Q_CRUDE_IMPORTA(DomM10,MNCRUD,MNXYRS) );

    QIN(M10,MNCRUD) = sum(MNXYRS$Per1_MNXYRS('Per1',MNXYRS),LFMMOUT_Q_CRUDE_IMPORTS(M10,MNCRUD,MNXYRS));
    QIN_TOT(M10) = sum(MNCRUD,QIN(M10,MNCRUD));
    SIN(M10,MNCRUD) = 0.0;
    SIN(M10,MNCRUD)$QIN_TOT(M10) = QIN(M10,MNCRUD) / QIN_TOT(M10);
    QOUT(M10,MNCRUD) = sum((RefReg,Crude,Source,Step)$(M10_2_RefReg(M10,RefReg) and MNCRUD_2_Crude(MNCRUD,Crude)),
      CRUDETRANS.l(RefReg,Crude,Source,Step,PrcPeriod) );
    QOUT('10_M10',MNCRUD) = sum(DomM10,QOUT(DomM10,MNCRUD));
    QOUT_TOT(M10) = sum(MNCRUD,QOUT(M10,MNCRUD));
    SOUT(M10,MNCRUD) = 0.0;
    SOUT(M10,MNCRUD)$QOUT_TOT(M10) = QOUT(M10,MNCRUD) / QOUT_TOT(M10);

    STMP(M10,MNCRUD) = sum((Crude,t,MNXYRS)$(MNCRUD_2_Crude(MNCRUD,Crude) and Per1_t_MNXYRS('Per1',t,MNXYRS) and Per1_MNXYRS('Per1',MNXYRS)),
      MAX( MIN(SOUT(M10,MNCRUD) , CrudeImportShares(Crude,t-1) + 0.005) , CrudeImportShares(Crude,t-1) - 0.005));
    STMP_TOT(M10) = sum(MNCRUD,STMP(M10,MNCRUD));
    SAVG(M10,MNCRUD) = 0.0;
    SAVG(M10,MNCRUD)$STMP_TOT(M10) = STMP(M10,MNCRUD) / STMP_TOT(M10);

    Loop(MNXYRS$Per1_MNXYRS('Per1',MNXYRS),
      CrudeImportShares(Crude,t)$Per1_t_MNXYRS('Per1',t,MNXYRS) = sum(MNCRUD$MNCRUD_2_Crude(MNCRUD,Crude),SAVG('10_M10',MNCRUD));
    );

    LFMMOUT_Q_CRUDE_IMPORTS(M10,MNCRUD,MNXYRS)$Per1_MNXYRS('Per1',MNXYRS) =
      sum((Crude,t)$(MNCRUD_2_Crude(MNCRUD,Crude) and Per1_t_MNXYRS('Per1',t,MNXYRS)),
        QOUT_TOT(M10) * CrudeImportShares(Crude,t)) ;

    LFMMOUT_Q_CRUDE_IMPORTS('09_M10',MNCRUD,MNXYRS)$Per1_MNXYRS('Per1',MNXYRS) =
      LFMMOUT_Q_CRUDE_IMPORTA('09_M10',MNCRUD,MNXYRS);

    LFMMOUT_Q_CRUDE_IMPORTS('10_M10',MNCRUD,MNXYRS)$Per1_MNXYRS('Per1',MNXYRS) =
      sum(DomM10, LFMMOUT_Q_CRUDE_IMPORTS(DomM10,MNCRUD,MNXYRS) );
*   LFMMOUT_Q_CRUDE_IMPORTS('10_M10',MNCRUD,MNXYRS)$Per1_MNXYRS('Per1',MNXYRS) =
*     sum(M10$(ord(M10)<card(M10)), LFMMOUT_Q_CRUDE_IMPORTS(M10,MNCRUD,MNXYRS) );

    LFMMOUT_Q_CRUDE_EXPORTS(M10,MNCRUD,MNXYRS)$Per1_MNXYRS('Per1',MNXYRS) =
      sum((DomRefReg,Crude,Source,Step)$(M10_2_RefReg(M10,DomRefReg) and MNCRUD_2_Crude(MNCRUD,Crude)),
        CRUDEEXPORT.l(DomRefReg,Crude,Source,Step,Period) ) ;

    LFMMOUT_Q_CRUDE_EXPORTS('10_M10',MNCRUD,MNXYRS)$Per1_MNXYRS('Per1',MNXYRS) =
      sum(DomM10, LFMMOUT_Q_CRUDE_EXPORTS(DomM10,MNCRUD,MNXYRS) );
*   LFMMOUT_Q_CRUDE_EXPORTS('10_M10',MNCRUD,MNXYRS)$Per1_MNXYRS('Per1',MNXYRS) =
*     sum(M10$(ord(M10)<card(M10)), LFMMOUT_Q_CRUDE_EXPORTS(M10,MNCRUD,MNXYRS) );


  );             /* END  loop(PrcPeriod(Period) */

    loop((t,MNUMYR)$((tMNUM(t,MNUMYR))$Runyears(t)),
       INTOUT_BRENT_PRICE(MNUMYR) = (CrudeSupCurveForeign.m('Foreign','L_Sweet','Per1')) / GDP(t);
    );


* Calculate the world oil price
  WEIGHTEDPRICE(MNCRUD) = 0.0 ;
  loop((MNUMYR,MNXYRS)$(Per1_MNXYRS('Per1',MNXYRS) and Per1_MNUMYR('Per1',MNUMYR)),
     loop(MNCRUD,

        WEIGHTEDPRICE(MNCRUD) = sum(M10$(ord(M10)<9),
           LFMMOUT_P_CRUDE_IMPORTS(M10,MNCRUD,MNXYRS) * LFMMOUT_Q_CRUDE_IMPORTA(M10,MNCRUD,MNXYRS) ) ;

        LFMMOUT_P_CRUDE_IMPORTS('10_M10',MNCRUD,MNXYRS) = 0 ;
        LFMMOUT_P_CRUDE_IMPORTS('10_M10',MNCRUD,MNXYRS)$(LFMMOUT_Q_CRUDE_IMPORTA('10_M10',MNCRUD,MNXYRS)>0) =
           WEIGHTEDPRICE(MNCRUD) / LFMMOUT_Q_CRUDE_IMPORTA('10_M10',MNCRUD,MNXYRS) ;

     );

     INTOUT_IT_WOP(MNUMYR,'1_M2') = INTOUT_BRENT_PRICE(MNUMYR) ;
     INTOUT_IT_WOP(MNUMYR,'1_M2')$(sum(MNCRUD, LFMMOUT_Q_CRUDE_IMPORTA('10_M10',MNCRUD,MNXYRS))>0) =
        sum(MNCRUD, WEIGHTEDPRICE(MNCRUD)) / sum(MNCRUD, LFMMOUT_Q_CRUDE_IMPORTA('10_M10',MNCRUD,MNXYRS)) ;

  );

  WEIGHTEDPRICE(MNCRUD) = 0.0 ;
  loop((MNUMYR,MNXYRS)$(Per1_MNXYRS('Per1',MNXYRS) and Per1_MNUMYR('Per1',MNUMYR)),
     loop(MNCRUD,

        WEIGHTEDPRICE(MNCRUD) = sum(M10$(ord(M10)<9),
           LFMMOUT_P_CRUDE_EXPORTS(M10,MNCRUD,MNXYRS) * LFMMOUT_Q_CRUDE_EXPORTS(M10,MNCRUD,MNXYRS) ) ;

        LFMMOUT_P_CRUDE_EXPORTS('10_M10',MNCRUD,MNXYRS) = 0 ;
        LFMMOUT_P_CRUDE_EXPORTS('10_M10',MNCRUD,MNXYRS)$(LFMMOUT_Q_CRUDE_EXPORTS('10_M10',MNCRUD,MNXYRS)>0) =
           WEIGHTEDPRICE(MNCRUD) / LFMMOUT_Q_CRUDE_EXPORTS('10_M10',MNCRUD,MNXYRS) ;

* cannot export Calif, Dilbit, Syncrude
        LFMMOUT_P_CRUDE_EXPORTS(M10,MNCRUD,MNXYRS)$MNCRUD_2_Crude(MNCRUD,'Calif') = 0 ;
        LFMMOUT_P_CRUDE_EXPORTS(M10,MNCRUD,MNXYRS)$MNCRUD_2_Crude(MNCRUD,'Dilbit') = 0 ;
        LFMMOUT_P_CRUDE_EXPORTS(M10,MNCRUD,MNXYRS)$MNCRUD_2_Crude(MNCRUD,'Syncrude') = 0 ;
     );
  );


*R NOTE: change to calc of LPG prices by sector in 87$/mmbtu

*R LPG/propane prices are now set once per cycle (before macro is called)
*R outside of the LFMM using DLM (dynamic linear model) and Bayesian priors methodology
*R via method by Janice Lent & Peter Gross (08/2014)


*Rloop(Per1_t_MNUMYR(Period,t,MNUMYR)$PrcPeriod(Period),
*R
*R
*R maybe keep the following US totals ????
*R  if(sum(MNUMCR, QBLK_QLGRS(MNUMCR,MNUMYR))>0,
*R    MPBLK_PLGRS('11_United_States',MNUMYR) =
*R      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QLGRS(MNUMCR,MNUMYR) * MPBLK_PLGRS(MNUMCR,MNUMYR) ) /
*R      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QLGRS(MNUMCR,MNUMYR) ) ;
*R  );
*R  if(sum(MNUMCR, QBLK_QLGCM(MNUMCR,MNUMYR))>0,
*R    MPBLK_PLGCM('11_United_States',MNUMYR) =
*R      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QLGCM(MNUMCR,MNUMYR) * MPBLK_PLGCM(MNUMCR,MNUMYR) ) /
*R      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QLGCM(MNUMCR,MNUMYR) ) ;
*R  );
*R  if(sum(MNUMCR, QBLK_QLGIN(MNUMCR,MNUMYR))>0,
*R    MPBLK_PLGIN('11_United_States',MNUMYR) =
*R      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QLGIN(MNUMCR,MNUMYR) * MPBLK_PLGIN(MNUMCR,MNUMYR) ) /
*R      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QLGIN(MNUMCR,MNUMYR) ) ;
*R  );
*R  if(sum(MNUMCR, QMORE_QPRINPF(MNUMCR,MNUMYR))>0,
*R    PMORE_PLGINPF('11_United_States',MNUMYR) =
*R      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QMORE_QPRINPF(MNUMCR,MNUMYR) * PMORE_PLGINPF(MNUMCR,MNUMYR) ) /
*R      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QMORE_QPRINPF(MNUMCR,MNUMYR) ) ;
*R  );
*R  if(sum(MNUMCR, QBLK_QLGTR(MNUMCR,MNUMYR))>0,
*R    MPBLK_PLGTR('11_United_States',MNUMYR) =
*R      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QLGTR(MNUMCR,MNUMYR) * MPBLK_PLGTR(MNUMCR,MNUMYR) ) /
*R      sum((ActiveDem,MNUMCR)$crN2L(ActiveDem,MNUMCR), QBLK_QLGTR(MNUMCR,MNUMYR) ) ;
*R  );
*R
*R
*R maybe keep the following carbon price adjustments ????
*R    PMORE_PPRRS(MNUMCR,MNUMYR)   = MPBLK_PLGRS(MNUMCR,MNUMYR) + EMABLK_JPRRS(MNUMYR)$(CO2PLFMM=1) ;
*R    PMORE_PPRCM(MNUMCR,MNUMYR)   = MPBLK_PLGCM(MNUMCR,MNUMYR) + EMABLK_JPRCM(MNUMYR)$(CO2PLFMM=1) ;
*R    PMORE_PPRIN(MNUMCR,MNUMYR)   = MPBLK_PLGIN(MNUMCR,MNUMYR) + EMABLK_JPRIN(MNUMYR)$(CO2PLFMM=1) ;
*R    PMORE_PPRTR(MNUMCR,MNUMYR)   = MPBLK_PLGTR(MNUMCR,MNUMYR) + EMABLK_JPRTR(MNUMYR)$(CO2PLFMM=1) ;
*R    PMORE_PPRINPF(MNUMCR,MNUMYR) = PMORE_PLGINPF(MNUMCR,MNUMYR) + EMABLK_JPRINPF(MNUMYR)$(CO2PLFMM=1) ;

*R    MPBLK_PLGRS(MNUMCR,MNUMYR)   = MPBLK_PLGRS(MNUMCR,MNUMYR) + EMABLK_JLGRS(MNUMYR)$(CO2PLFMM=1) ;
*R    MPBLK_PLGCM(MNUMCR,MNUMYR)   = MPBLK_PLGCM(MNUMCR,MNUMYR) + EMABLK_JLGCM(MNUMYR)$(CO2PLFMM=1) ;
*R    MPBLK_PLGIN(MNUMCR,MNUMYR)   = MPBLK_PLGIN(MNUMCR,MNUMYR) + EMABLK_JLGIN(MNUMYR)$(CO2PLFMM=1) ;
*R    PMORE_PLGINPF(MNUMCR,MNUMYR) = PMORE_PLGINPF(MNUMCR,MNUMYR) + EMABLK_JNQLGPF(MNUMYR)$(CO2PLFMM=1) ;
*R    MPBLK_PLGTR(MNUMCR,MNUMYR)   = MPBLK_PLGTR(MNUMCR,MNUMYR) + EMABLK_JLGTR(MNUMYR)$(CO2PLFMM=1) ;

*R
*R maybe keep the following US totals ????
*R  MPBLK_PLGAS(MNUMCR,MNUMYR)$((QBLK_QLGRS(MNUMCR,MNUMYR)+QBLK_QLGCM(MNUMCR,MNUMYR)+QBLK_QLGIN(MNUMCR,MNUMYR)+QBLK_QLGTR(MNUMCR,MNUMYR))>0) =
*R     (MPBLK_PLGRS(MNUMCR,MNUMYR)*QBLK_QLGRS(MNUMCR,MNUMYR) +
*R      MPBLK_PLGCM(MNUMCR,MNUMYR)*QBLK_QLGCM(MNUMCR,MNUMYR) +
*R      MPBLK_PLGIN(MNUMCR,MNUMYR)*QBLK_QLGIN(MNUMCR,MNUMYR) +
*R      MPBLK_PLGTR(MNUMCR,MNUMYR)*QBLK_QLGTR(MNUMCR,MNUMYR)) /
*R      (QBLK_QLGRS(MNUMCR,MNUMYR) + QBLK_QLGCM(MNUMCR,MNUMYR) +
*R       QBLK_QLGIN(MNUMCR,MNUMYR) + QBLK_QLGTR(MNUMCR,MNUMYR)) ;
*R
*R);


$ontext
* PENDING ITEMS..

* This is an input price since the LFMM doesn't produce methanol
  MPBLK_PMETR(MNUMCR,MNUMYR)$Per1_MNUMYR('Per1',MNUMYR)   =


$offtext

);

** end of initial NCNTRL_CURCALYR>2010 IF statement

* Only do these things if NCNTRL_FCRL = 1
if (NCNTRL_FCRL=1,

*loop((LCFS_Years(t),MNXYRS)$(tMNXYRS(t,MNXYRS) and t.val=NCNTRL_CURCALYR+1),
if ((LCFSOPT > 0),
  loop((t,MNXYRS)$(tMNXYRS(t,MNXYRS) and t.val=NCNTRL_CURCALYR+1),

** Covert LCFS reporting to CO2 from C
** esh 1/15/21:
** need to convert values so multipling by 44/12 or 12/44 to get into tonne CO2
*** used to be converted in ftab, now converting here so gdx files have correct values/units
*87$ per tonne CO2
     LFMMOUT_LCFS_Offset_Prc(LCFS_C,MNXYRS) = (12/44)*sum((Period(BldPeriod),LCFS_PetCategory)$LCFS_M2(LCFS_PetCategory,LCFS_C),
        LCFS_Petroleum.m(LCFS_PetCategory,Period) / GDP(t) / Discount(rate(t)));
*1000 tonne CO2 per trill BTU
     LFMMOUT_LCFS_Baseline(LCFS_C,MNXYRS) = (44/12)*sum((Period(BldPeriod),LCFS_PetCategory,LCFS_RefReg)$LCFS_M2(LCFS_PetCategory,LCFS_C),
        LCFS_Target(LCFS_PetCategory,LCFS_RefReg,t));
*MM tonne CO2 per yr
     LFMMOUT_LCFS_Carb_Offset(LCFS_C,MNXYRS) = (44/12)*sum((Period(BldPeriod),LCFS_PetCategory)$LCFS_M2(LCFS_PetCategory,LCFS_C),
        LCFS_Pet_to_Bio.l(LCFS_PetCategory,Period))*365/1000 ;
*MM tonne CO2 per yr
     LFMMOUT_LCFS_Waiver(LCFS_C,MNXYRS) = (44/12)*sum((Period(BldPeriod),LCFS_PetCategory)$LCFS_M2(LCFS_PetCategory,LCFS_C),
        LCFSSafety.l(LCFS_PetCategory,Period))*365/1000 ;
     LFMMOUT_LCFS_PetTotalVolume(LCFS_C,MNXYRS) = (44/12)*sum((Period(BldPeriod),LCFS_PetCategory,LCFS_RefReg,RefType,LCFS_PetStreams)
       $(LCFS_M2(LCFS_PetCategory,LCFS_C) and LCFS_PetCategory_PetStreams(LCFS_PetCategory,LCFS_PetStreams)),
       RecipetoProd.l(LCFS_RefReg,RefType,LCFS_PetStreams,Period));
*Trill BTU per year
     LFMMOUT_LCFS_PeToTrills(LCFS_C,MNXYRS) = sum((Period(BldPeriod),LCFS_PetCategory)
       $LCFS_M2(LCFS_PetCategory,LCFS_C),

       sum((LCFS_RefReg,RefType,LCFS_PetStreams)$LCFS_PetCategory_PetStreams(LCFS_PetCategory,LCFS_PetStreams),
          (RecipetoProd.l(LCFS_RefReg,RefType,LCFS_PetStreams,Period) * LCFS_Pet_Energy_Density(LCFS_PetStreams,t) * 365.0 * 0.001))+

       sum(LCFS_Vehicle_Types,LCFS_AltVehicles.l(LCFS_PetCategory,LCFS_Vehicle_Types,Period)) +

       sum((RefRegA,LCFS_RefReg,TranMode,LCFS_PetStreams)$LCFS_PetCategory_PetStreams(LCFS_PetCategory,LCFS_PetStreams),
          (RefRefTRAN.l(RefRegA,LCFS_RefReg,TranMode,LCFS_PetStreams,Period) *
           LCFS_Pet_Energy_Density(LCFS_PetStreams,t) * 365.0 * 0.001)) );
*1000 tonne CO2 per trill BTU
     LFMMOUT_LCFS_Actual(LCFS_C,MNXYRS)$LFMMOUT_LCFS_PeToTrills(LCFS_C,MNXYRS) =
       (LFMMOUT_LCFS_Baseline(LCFS_C,MNXYRS) * LFMMOUT_LCFS_PeToTrills(LCFS_C,MNXYRS) + LFMMOUT_LCFS_Waiver(LCFS_C,MNXYRS) *1000. ) /
       LFMMOUT_LCFS_PeToTrills(LCFS_C,MNXYRS);
  );
);

*loop((CFP_Years(t),MNXYRS)$(tMNXYRS(t,MNXYRS) and t.val=NCNTRL_CURCALYR+1),
if ((LCFSOPT > 0),
  loop((t,MNXYRS)$(tMNXYRS(t,MNXYRS) and t.val=NCNTRL_CURCALYR+1),

** Covert CFP reporting to CO2 from C
** esh 1/15/21:
** need to convert values so multipling by 44/12 or 12/44 to get into tonne CO2
*** used to be converted in ftab, now converting here so gdx files have correct values/units
*87$ per tonne CO2
     LFMMOUT_CFP_Offset_Prc(LCFS_C,MNXYRS) = (12/44)*sum((Period(BldPeriod),CFP_PetCategory)$CFP_M2(CFP_PetCategory,LCFS_C),
        CFP_Petroleum.m(CFP_PetCategory,Period) / GDP(t) / Discount(rate(t)));
*1000 tonne CO2 per trill BTU
     LFMMOUT_CFP_Baseline(LCFS_C,MNXYRS) = (44/12)*sum((Period(BldPeriod),CFP_PetCategory,CFP_RefReg)$CFP_M2(CFP_PetCategory,LCFS_C),
        CFP_Target(CFP_PetCategory,CFP_RefReg,t));
*MM tonne CO2 per yr
     LFMMOUT_CFP_Carb_Offset(LCFS_C,MNXYRS) = (44/12)*sum((Period(BldPeriod),CFP_PetCategory)$CFP_M2(CFP_PetCategory,LCFS_C),
        CFP_Pet_to_Bio.l(CFP_PetCategory,Period))*365/1000 ;
*MM tonne CO2 per yr
     LFMMOUT_CFP_Waiver(LCFS_C,MNXYRS) = (44/12)*sum((Period(BldPeriod),CFP_PetCategory)$CFP_M2(CFP_PetCategory,LCFS_C),
        CFPSafety.l(CFP_PetCategory,Period))*365/1000 ;
     LFMMOUT_CFP_PetTotalVolume(LCFS_C,MNXYRS) = (44/12)*sum((Period(BldPeriod),CFP_PetCategory,CFP_RefReg,RefType,CFP_PetStreams)
       $(CFP_M2(CFP_PetCategory,LCFS_C) and CFP_PetCategory_PetStreams(CFP_PetCategory,CFP_PetStreams)),
       RecipetoProd.l(CFP_RefReg,RefType,CFP_PetStreams,Period));
*Trill BTU per year
     LFMMOUT_CFP_PeToTrills(LCFS_C,MNXYRS) = sum((Period(BldPeriod),CFP_PetCategory)
       $CFP_M2(CFP_PetCategory,LCFS_C),

       sum((CFP_RefReg,RefType,CFP_PetStreams)$CFP_PetCategory_PetStreams(CFP_PetCategory,CFP_PetStreams),
          (RecipetoProd.l(CFP_RefReg,RefType,CFP_PetStreams,Period) * CFP_Pet_Energy_Density(CFP_PetStreams,t) * 365.0 * 0.001))+

       sum(CFP_Vehicle_Types,CFP_AltVehicles.l(CFP_PetCategory,CFP_Vehicle_Types,Period)) +

       sum((RefRegA,CFP_RefReg,TranMode,CFP_PetStreams)$CFP_PetCategory_PetStreams(CFP_PetCategory,CFP_PetStreams),
          (RefRefTRAN.l(RefRegA,CFP_RefReg,TranMode,CFP_PetStreams,Period) *
           CFP_Pet_Energy_Density(CFP_PetStreams,t) * 365.0 * 0.001)) );
*1000 tonne CO2 per trill BTU
     LFMMOUT_CFP_Actual(LCFS_C,MNXYRS)$LFMMOUT_CFP_PeToTrills(LCFS_C,MNXYRS) =
       (LFMMOUT_CFP_Baseline(LCFS_C,MNXYRS) * LFMMOUT_CFP_PeToTrills(LCFS_C,MNXYRS) + LFMMOUT_CFP_Waiver(LCFS_C,MNXYRS) *1000. ) /
       LFMMOUT_CFP_PeToTrills(LCFS_C,MNXYRS);
  );
);

loop((t,MNUMYR)$(tMNUM(t,MNUMYR) and t.val=NCNTRL_CURCALYR+1),

* Set to zero until we fill in properly
   PMMFTAB_E85ICCREDIT(MNUMYR) = 0 ;

   LFMMOUT_RFSCREDPRC(M4,MNUMYR) =
     sum((RFSCategory,Period(BldPeriod))$M4_2_RFSCat(M4,RFSCategory), RFSConstraintsPRD.m(RFSCategory,Period) )
      / GDP(t) / Discount(rate(t));

   LFMMOUT_RFSSAFETY(M4,MNUMYR) =
     sum((RFSCategory,Period(BldPeriod))$M4_2_RFSCat(M4,RFSCategory), RFSESCAPE.l(RFSCategory,Period) ) * 365 * 42 / 1000000 ;

   LFMMOUT_RFSACTUAL(M4,MNUMYR) =
     sum((RFSCategory,Period(BldPeriod))$M4_2_RFSCat(M4,RFSCategory), RFS_PRD_to_RQM.l(RFSCategory,Period) ) ;

);

* Set current year CO2 prices using the Period 2 marginal prices since we want them to include the capital component
loop((t,tt,MNUMYR)$(t.val=NCNTRL_CURCALYR+1 and tt.val=NCNTRL_CURCALYR and tMNUM(tt,MNUMYR)),

* Set the CTL CO2 price to just below the marginal CO2 price
    OGSMOUT_OGCO2PLF(M8,MNUMYR) =
      sum((OGCO2Reg,BldPeriod)$OGCO2Reg_2_M8(OGCO2Reg,M8),
        0.90 * OGSM_CO2Demand.m(OGCO2Reg,BldPeriod) ) * GDP('2008') / 18 / GDP(t) / Discount(Rate(t)) ;

    OGSMOUT_OGCO2PLF('8_M8',MNUMYR) =
      sum((OGCO2Reg,M8,BldPeriod)$OGCO2Reg_2_M8(OGCO2Reg,M8),
        OGSMOUT_OGCO2PLF(M8,MNUMYR)*npv_OGSMCO2Dem(OGCO2Reg,BldPeriod) ) /
      sum((OGCO2Reg,M8,BldPeriod)$OGCO2Reg_2_M8(OGCO2Reg,M8),
        npv_OGSMCO2Dem(OGCO2Reg,BldPeriod) ) ;

);

*** Populate CCS data for CTS model ***
plant_counter = COALEMM_N_PLTS + 1 ;
loop((DomRefReg,Period(BldPeriod),FuelRegion)$RefReg_2_FuelRegion(DomRefReg,FuelRegion),

  if(sum((PetRefType,BldStep), BUILDS.l(DomRefReg,PetRefType,'CTLCCS',BldStep,Period))>0,
    COALEMM_N_RG(M1000)$(ord(M1000)=plant_counter)   = FuelRegion.val ;
    COALEMM_N_RY(M1000)$(ord(M1000)=plant_counter)   = NCNTRL_CURCALYR+1 ;
    COALEMM_N_IGRP(M1000)$(ord(M1000)=plant_counter) = 20000 + plant_counter ;
    COALEMM_N_PLTS                                   = plant_counter ;
    COALEMM_N_CFR(M1000)$(ord(M1000)=plant_counter)  = StreamFactors('CTLCCS') ;
    COALEMM_N_HRAT(M1000)$(ord(M1000)=plant_counter) = abs(ProcessTableCrude('COA','all','CTLCCS','CTLCCS_CDSCCS')) * 1000000 /
                                                         abs(ProcessTableCrude('KWGrid','all','CTLCCS','CTLCCS_CDSCCS')) ;
    COALEMM_N_CPTY(M1000)$(ord(M1000)=plant_counter) = sum((PetRefType,BldStep), BUILDS.l(DomRefReg,PetRefType,'CTLCCS',BldStep,Period)) *
                                                         abs(ProcessTableCrude('KWGrid','all','CTLCCS','CTLCCS_CDSCCS')) * 365 / 8760 ;
    COALEMM_N_PTP(M1000)$(ord(M1000)=plant_counter)  = 60 ;

    plant_counter = plant_counter + 1 ;
  );

  if(sum((PetRefType,BldStep), BUILDS.l(DomRefReg,PetRefType,'CBLCCS',BldStep,Period))>0,
    COALEMM_N_RG(M1000)$(ord(M1000)=plant_counter)   = FuelRegion.val ;
    COALEMM_N_RY(M1000)$(ord(M1000)=plant_counter)   = NCNTRL_CURCALYR+1 ;
    COALEMM_N_IGRP(M1000)$(ord(M1000)=plant_counter) = 20000 + plant_counter ;
    COALEMM_N_PLTS                                   = plant_counter ;
    COALEMM_N_CFR(M1000)$(ord(M1000)=plant_counter)  = StreamFactors('CBLCCS') ;
    COALEMM_N_HRAT(M1000)$(ord(M1000)=plant_counter) = abs(ProcessTableCrude('COA','all','CBLCCS','CBLCCS_CBJECCS')+ProcessTableCrude('ECR','all','CBLCCS','CBLCCS_CBJECCS')) * 1000000 /
                                                         abs(ProcessTableCrude('KWGrid','all','CBLCCS','CBLCCS_CBJECCS')) ;
    COALEMM_N_CPTY(M1000)$(ord(M1000)=plant_counter) = sum((PetRefType,BldStep), BUILDS.l(DomRefReg,PetRefType,'CBLCCS',BldStep,Period)) *
                                                         abs(ProcessTableCrude('KWGrid','all','CBLCCS','CBLCCS_CBJECCS')) * 365 / 8760 ;
    COALEMM_N_PTP(M1000)$(ord(M1000)=plant_counter)  = 61 ;

    plant_counter = plant_counter + 1 ;
  );

);

);
* end of NCNTRL_FCRL=1 IF

* LFMM REPORT GENERATION  -  All prices in ReportYr dollars
if((NCNTRL_FCRL=1) and (NCNTRL_CURCALYR>2010) and (LFMMReporting=1),

repUtilization(Process,RefReg,RefType,Period,RunYears) = 0 ;
repUtilization(Process,RefReg,RefType,Period,RunYears)$((AvailCap(RefReg,RefType,Process)+sum(BldStep,BUILDS.l(RefReg,RefType,Process,BldStep,Period)))>0)
   = ROUND((OPERATECAP.l(RefReg,RefType,Process,Period)+sum(BldStep,BUILDS.l(RefReg,RefType,Process,BldStep,Period))) /
     (AvailCap(RefReg,RefType,Process)+sum(BldStep,BUILDS.l(RefReg,RefType,Process,BldStep,Period))),3) ;

repActivity(RefReg,RefType,Stream,Process,ProcessMode,Period,RunYears) = 0 ;
repActivity(RefReg,RefType,Stream,Process,ProcessMode,Period,RunYears)
   = PROCMODE.l(RefReg,RefType,Process,ProcessMode,Period) * ProcessTable(Stream,Process,ProcessMode);

* ReportYr$/BBL
repRefWSPrice(RefReg,RefType,RecipeProd,Period,RunYears) = 0 ;
repRefWSPrice(RefReg,RefType,RecipeProd,Period,RunYears) =
   CurrentDeflator * RecipeTransfer.m(RefReg,RefType,RecipeProd,Period) ;

* ReportYr$/BBL
repCenWSPrice(ActiveDem,EndProduct,Period,RunYears) = 0 ;
repCenWSPrice(ActiveDem,RecipeEnd,Period,RunYears) =
   CurrentDeflator * RecipeDemands.m(ActiveDem,RecipeEnd,Period) ;
repCenWSPrice(ActiveDem,'CFGout',Period,RunYears) =
   CurrentDeflator * CFGBalance.m(ActiveDem,Period) ;
repCenWSPrice(ActiveDem,'CFG15out',Period,RunYears) =
   CurrentDeflator * CFG15Balance.m(ActiveDem,Period) ;
repCenWSPrice(ActiveDem,'CFGb16out',Period,RunYears) =
   CurrentDeflator * CFGb16Balance.m(ActiveDem,Period) ;
repCenWSPrice(ActiveDem,'RFGout',Period,RunYears) =
   CurrentDeflator * RFGBalance.m(ActiveDem,Period) ;
repCenWSPrice(ActiveDem,'RFG15out',Period,RunYears) =
   CurrentDeflator * RFG15Balance.m(ActiveDem,Period) ;
repCenWSPrice(ActiveDem,'RFGb16out',Period,RunYears) =
   CurrentDeflator * RFGb16Balance.m(ActiveDem,Period) ;
repCenWSPrice(ActiveDem,'E85out',Period,RunYears) =
   CurrentDeflator * E85Balance.m(ActiveDem,Period) ;

* ReportYr$/BBL
repSpecProdPrice(RefReg,RefType,SpecProd,Period,RunYears) = 0 ;
repSpecProdPrice(RefReg,RefType,GasSpecProd,Period,RunYears) =
   CurrentDeflator * GasSpecBalance.m(RefReg,RefType,GasSpecProd,Period) ;
repSpecProdPrice(RefReg,RefType,DistSpecProd,Period,RunYears) =
   CurrentDeflator * DistSpecBalance.m(RefReg,RefType,DistSpecProd,Period) ;
repSpecProdPrice(RefReg,RefType,ResidSpecProd,Period,RunYears) =
   CurrentDeflator * ResidSpecBalance.m(RefReg,RefType,ResidSpecProd,Period) ;

* M BBL per day - sum all tranmode
repRefRefTran(RefReg,RefRegA,Stream,Period,RunYears) = 0 ;
repRefRefTran(RefReg,RefRegA,Stream,Period,RunYears) =
   sum(TranMode, RefRefTRAN.l(RefReg,RefRegA,TranMode,Stream,Period) ) ;

* M BBL per day - PLL
repRefRefTranPLL(RefReg,RefRegA,Stream,Period,RunYears) = 0 ;
repRefRefTranPLL(RefReg,RefRegA,Stream,Period,RunYears) =
   RefRefTRAN.l(RefReg,RefRegA,'tmPLL',Stream,Period) ;

* M BBL per day - CPL
repRefRefTranCPL(RefReg,RefRegA,Stream,Period,RunYears) = 0 ;
repRefRefTranCPL(RefReg,RefRegA,Stream,Period,RunYears) =
   RefRefTRAN.l(RefReg,RefRegA,'tmCPL',Stream,Period) ;

* M BBL per day - TranMode capacity
repReftoRefTranCap(RefReg,RefRegA,TranMode,RunYears) = 0 ;
repReftoRefTranCap(RefReg,RefRegA,TranMode,RunYears) =
   npv_ReftoRefTranCap(RefReg,RefRegA,TranMode,'Per1') ;
* need to convert DWT to bbl for barge and tanker capacity (0.002 DWT/bbl)
repReftoRefTranCap(RefReg,RefRegA,TranMode(MarineMode),RunYears) =
   repReftoRefTranCap(RefReg,RefRegA,TranMode,RunYears) / 0.002 ;

* M BBL per day - TranMode movements
repRefRefTranMode(RefReg,RefRegA,TranMode,RunYears) = 0 ;
repRefRefTranMode(RefReg,RefRegA,TranMode,RunYears) =
   sum(Stream, RefRefTRAN.l(RefReg,RefRegA,TranMode,Stream,'Per1') ) ;

* M BBL per day - TranMode movements, capacity, utilization
repRefRefTranModeCapUtz(M3,RefReg,RefRegA,TranMode,RunYears) = 0 ;
repRefRefTranModeCapUtz('1_M3',RefReg,RefRegA,TranMode,RunYears) =
   sum(Stream, RefRefTRAN.l(RefReg,RefRegA,TranMode,Stream,'Per1') ) ;
repRefRefTranModeCapUtz('2_M3',RefReg,RefRegA,TranMode,RunYears) =
   repReftoRefTranCap(RefReg,RefRegA,TranMode,RunYears) ;
*  npv_ReftoRefTranCap(RefReg,RefRegA,TranMode,'Per1') ;
repRefRefTranModeCapUtz('3_M3',RefReg,RefRegA,TranMode,RunYears)
   $(repRefRefTranModeCapUtz('2_M3',RefReg,RefRegA,TranMode,RunYears) > 0) =
   repRefRefTranModeCapUtz('1_M3',RefReg,RefRegA,TranMode,RunYears) /
   repRefRefTranModeCapUtz('2_M3',RefReg,RefRegA,TranMode,RunYears) ;



* M BBL per day - NonMarineMode capacity
repRefCDTranCap(RefReg,ActiveDem,NonMarineMode,RunYears) = 0 ;
repRefCDTranCap(RefReg,ActiveDem,NonMarineMode,RunYears) =
   ReftoCDTranCap(RefReg,ActiveDem,NonMarineMode) ;

* M BBL per day - NonMarineMode movements
repRefCDTRANmode(RefReg,ActiveDem,NonMarineMode,RunYears) = 0 ;
repRefCDTRANmode(RefReg,ActiveDem,NonMarineMode,RunYears) =
   sum(RecipeProd, RefCenTRAN.l(RefReg,ActiveDem,NonMarineMode,RecipeProd,'Per1')) ;

* M BBL per day - TranMode movements, capacity, utilization
repRefCDTranModeCapUtz(M3,RefReg,CenDiv,NonMarineMode,RunYears) = 0 ;
repRefCDTranModeCapUtz('1_M3',RefReg,ActiveDem,NonMarineMode,RunYears) =
   repRefCDTRANmode(RefReg,ActiveDem,NonMarineMode,RunYears) ;
repRefCDTranModeCapUtz('2_M3',RefReg,ActiveDem,NonMarineMode,RunYears) =
   repRefCDTranCap(RefReg,ActiveDem,NonMarineMode,RunYears) ;
repRefCDTranModeCapUtz('3_M3',RefReg,ActiveDem,NonMarineMode,RunYears)
   $(repRefCDTranModeCapUtz('2_M3',RefReg,ActiveDem,NonMarineMode,RunYears) > 0) =
   repRefCDTranModeCapUtz('1_M3',RefReg,ActiveDem,NonMarineMode,RunYears) /
   repRefCDTranModeCapUtz('2_M3',RefReg,ActiveDem,NonMarineMode,RunYears) ;



repRefCenTran(RefReg,CenDiv,RecipeProd,Period,RunYears) = 0 ;
repRefCenTran(RefReg,CenDiv,RecipeProd,Period,RunYears) =
   sum(TranMode, RefCenTRAN.l(RefReg,CenDiv,TranMode,RecipeProd,Period) ) ;

repRefProd(RefReg,RefType,RecipeProd,Period,RunYears) = 0 ;
repRefProd(RefReg,RefType,RecipeProd,Period,RunYears) =
   RECIPETOPROD.l(RefReg,RefType,RecipeProd,Period) ;

* Trills/yr
repRefFuelUse(RefReg,RefType,Period,RunYears) = 0 ;
repRefFuelUse(RefReg,RefType,Period,RunYears) =
   sum(ProcessMode, ProcessTable('FUL','FUM',ProcessMode)*PROCMODE.l(RefReg,RefType,'FUM',ProcessMode,Period))
     * CONVFACT_CFRSQ * 365 / 1000.0 ;

* Trills/yr
repRefElecPurch(RefReg,RefType,Period,RunYears) = 0 ;
repRefElecPurch(RefReg,RefType,Period,RunYears) =
    sum(Step, UTILPURCH.l(RefReg,RefType,'KWH',Period)) * 365 * 3.414 / 1000000.0 ;
if(sum((RefReg,RefType,Period),repRefElecPurch(RefReg,RefType,Period,RunYears))=0,
    repRefElecPurch('9_RefReg','COKING','Per1',RunYears) = -9999 ;
);

* Trills/yr
repTotEnergyUse(RefReg,RefType,Period,RunYears) = 0 ;
repTotEnergyUse(RefReg,RefType,Period,RunYears) =
    repRefFuelUse(RefReg,RefType,Period,RunYears) +
    repRefElecPurch(RefReg,RefType,Period,RunYears) ;

repCO2Emissions(RefReg,RefType,Period,RunYears) = 0 ;
repCO2Emissions(RefReg,RefType,Period,RunYears) =
    sum((Process,ProcessMode,CO2Stream), PROCMODE.l(RefReg,RefType,Process,ProcessMode,Period)*CO2EmFactors(CO2Stream,Process,ProcessMode) ) ;

repTotalCO2(Period,RunYears) = 0 ;
repTotalCO2(Period,RunYears) =
    sum((RefReg,RefType), repCO2Emissions(RefReg,RefType,Period,RunYears) ) ;

repCrudeUse(RefReg,PetRefType,Crude,Period,RunYears) = 0 ;
repCrudeUse(RefReg,PetRefType,Crude,Period,RunYears) =
    -1 * sum((Process,ProcessMode)$ProcessTable(Crude,Process,ProcessMode),
       ProcessTable(Crude,Process,ProcessMode)* PROCMODE.l(RefReg,PetRefType,Process,ProcessMode,Period) ) ;

repMargCrdPrice(RefReg,Crude,Period,RunYears) = EPS ;
repMargCrdPrice(RefReg,Crude,Period,RunYears)$(sum(PetRefType, repCrudeUse(RefReg,PetRefType,Crude,Period,RunYears))>0) =
    CurrentDeflator *
    sum(PetRefType, repCrudeUse(RefReg,PetRefType,Crude,Period,RunYears) *
    (CrudeBalance.m(RefReg,Crude,Period)-CrudeBalance_M_DELTA(RefReg,Crude,Period)) ) /
    sum(PetRefType, repCrudeUse(RefReg,PetRefType,Crude,Period,RunYears) ) ;

repCrudeRAC(RefReg,RefType,Period,RunYears) = 0 ;
repCrudeRAC(RefReg,RefType,Period,RunYears)$(sum(Crude, repCrudeUse(RefReg,RefType,Crude,Period,RunYears))>0) =
   sum(Crude, repMargCrdPrice(RefReg,Crude,Period,RunYears)*repCrudeUse(RefReg,RefType,Crude,Period,RunYears) ) /
   sum(Crude, repCrudeUse(RefReg,RefType,Crude,Period,RunYears) ) ;
repCrudeRAC(RefReg,RefType,Period,RunYears)$(sum(Crude, repCrudeUse(RefReg,RefType,Crude,Period,RunYears))=0) = 0 ;

repRACMargins(RefReg,RefType,SpecProd,Period,RunYears) = 0 ;
repRACMargins(RefReg,RefType,SpecProd,Period,RunYears) =
   repSpecProdPrice(RefReg,RefType,SpecProd,Period,RunYears) -
   repCrudeRAC(RefReg,RefType,Period,RunYears) ;

repWTIMargins(RefReg,RefType,SpecProd,Period,RunYears) = 0 ;
repWTIMargins(RefReg,RefType,SpecProd,Period,RunYears) =
   repSpecProdPrice(RefReg,RefType,SpecProd,Period,RunYears) -
   repMargCrdPrice(RefReg,'L_Sweet',Period,RunYears) ;

repMassBalanceIn(Process,ProcessMode) = 0 ;
repMassBalanceIn(Process,ProcessMode) =
   sum(IntStream$(not NoMat(IntStream) and ProcessTable(IntStream,Process,ProcessMode)<0),
     ProcessTable(IntStream,Process,ProcessMode) * 141.5/(StreamProp(IntStream,'API')+131.5) );
repMassBalanceOut(Process,ProcessMode) = 0 ;
repMassBalanceOut(Process,ProcessMode) =
   sum(IntStream$(not NoMat(IntStream) and ProcessTable(IntStream,Process,ProcessMode)>0),
     ProcessTable(IntStream,Process,ProcessMode) * 141.5/(StreamProp(IntStream,'API')+131.5) );
repMassBalance(Process,ProcessMode) = 0 ;
repMassBalance(Process,ProcessMode) =
   repMassBalanceIn(Process,ProcessMode) + repMassBalanceOut(Process,ProcessMode);

* Calculate the specification properties of spec-blended products
repSpecProp(RefReg,RefType,SpecProd,GasProp,Period,RunYears) = 0 ;
repSpecProp(RefReg,RefType,GasSpecProd,GasProp,Period,RunYears)$(not sum(NonCrudeUtil,ToSPECBLEND.l(RefReg,RefType,GasSpecProd,NonCrudeUtil,Period))=0) =
    (sum(NonCrudeUtil,(141.5/(131.5+StreamProp(NonCrudeUtil,'API')))*StreamProp(NonCrudeUtil,GasProp)* ToSPECBLEND.l(RefReg,RefType,GasSpecProd,NonCrudeUtil,Period) ) /
     sum(NonCrudeUtil,ToSPECBLEND.l(RefReg,RefType,GasSpecProd,NonCrudeUtil,Period)))$GasSulfurSpec(GasProp) +
    (sum(NonCrudeUtil,StreamProp(NonCrudeUtil,GasProp)* ToSPECBLEND.l(RefReg,RefType,GasSpecProd,NonCrudeUtil,Period) ) /
     sum(NonCrudeUtil,ToSPECBLEND.l(RefReg,RefType,GasSpecProd,NonCrudeUtil,Period)))$(not GasSulfurSpec(GasProp));
repSpecProp(RefReg,RefType,DistSpecProd,DistProp,Period,RunYears)$(not sum(NonCrudeUtil,ToSPECBLEND.l(RefReg,RefType,DistSpecProd,NonCrudeUtil,Period))=0) =
    (sum(NonCrudeUtil,(141.5/(131.5+StreamProp(NonCrudeUtil,'API')))*StreamProp(NonCrudeUtil,DistProp)* ToSPECBLEND.l(RefReg,RefType,DistSpecProd,NonCrudeUtil,Period) ) /
     sum(NonCrudeUtil,ToSPECBLEND.l(RefReg,RefType,DistSpecProd,NonCrudeUtil,Period)))$DistSulfurSpec(DistProp) +
    (sum(NonCrudeUtil,StreamProp(NonCrudeUtil,DistProp)* ToSPECBLEND.l(RefReg,RefType,DistSpecProd,NonCrudeUtil,Period) ) /
     sum(NonCrudeUtil,ToSPECBLEND.l(RefReg,RefType,DistSpecProd,NonCrudeUtil,Period)))$(not DistSulfurSpec(DistProp));
repSpecProp(RefReg,RefType,ResidSpecProd,ResidProp,Period,RunYears)$(not sum(NonCrudeUtil,ToSPECBLEND.l(RefReg,RefType,ResidSpecProd,NonCrudeUtil,Period))=0) =
    (sum(NonCrudeUtil,(141.5/(131.5+StreamProp(NonCrudeUtil,'API')))*StreamProp(NonCrudeUtil,ResidProp)* ToSPECBLEND.l(RefReg,RefType,ResidSpecProd,NonCrudeUtil,Period) ) /
     sum(NonCrudeUtil,ToSPECBLEND.l(RefReg,RefType,ResidSpecProd,NonCrudeUtil,Period)))$ResidSulfurSpec(ResidProp) +
    (sum(NonCrudeUtil,StreamProp(NonCrudeUtil,ResidProp)* ToSPECBLEND.l(RefReg,RefType,ResidSpecProd,NonCrudeUtil,Period) ) /
     sum(NonCrudeUtil,ToSPECBLEND.l(RefReg,RefType,ResidSpecProd,NonCrudeUtil,Period)))$(not ResidSulfurSpec(ResidProp));

repEnergyBalance(Process,ProcessMode) = 0 ;
repEnergyBalance(Process,ProcessMode) =
   sum(IntStream$(not NoMat(IntStream)),
     ProcessTable(IntStream,Process,ProcessMode) * StreamProp(IntStream,'END') );

* Marginal prices in 87$ per bbl
repWSAltFuelPrc(RefReg,Stream,Period,RunYears) = 0 ;
*em4?? repWSAltFuelPrc(RefReg,EthStream,Period,RunYears)$(ord(Period)<3) =
repWSAltFuelPrc(RefReg,EthStream,Period,RunYears)$(ord(Period)<3 and ActivePeriod(Period)) =
   CurrentDeflator *
    EthBalance.m(RefReg,EthStream,Period) /
    sum(t$(ModelYears(t) and tPer(t,Period)), Discount(rate(t))) ;

*em4?? repWSAltFuelPrc(RefReg,AltStream,Period,RunYears)$((ord(Period)<3) and (sum((PetRefType,Process,ProcessMode)$(ProcessTable(AltStream,Process,ProcessMode)>0),
repWSAltFuelPrc(RefReg,AltStream,Period,RunYears)$((ord(Period)<3) and ActivePeriod(Period) and (sum((PetRefType,Process,ProcessMode)$(ProcessTable(AltStream,Process,ProcessMode)>0),
    ProcessTable(AltStream,Process,ProcessMode) * PROCMODE.l(RefReg,PetRefType,Process,ProcessMode,Period))>0)) =
 CurrentDeflator *
 (sum((PetRefType,Process,ProcessMode)$(ProcessTable(AltStream,Process,ProcessMode)>0),
    ProcessTable(AltStream,Process,ProcessMode) * PROCMODE.l(RefReg,PetRefType,Process,ProcessMode,Period) * StreamBalance.m(RefReg,PetRefType,AltStream,Period) ) /
  sum((PetRefType,Process,ProcessMode)$(ProcessTable(AltStream,Process,ProcessMode)>0),
    ProcessTable(AltStream,Process,ProcessMode) * PROCMODE.l(RefReg,PetRefType,Process,ProcessMode,Period) ) ) /
  sum(t$(ModelYears(t) and tPer(t,Period)), Discount(rate(t))) ;
*em4?? repWSAltFuelPrc(RefReg,AltStream,Period,RunYears)$((ord(Period)<3) and (sum((PetRefType,Process,ProcessMode)$(ProcessTable(AltStream,Process,ProcessMode)>0),
repWSAltFuelPrc(RefReg,AltStream,Period,RunYears)$((ord(Period)<3) and ActivePeriod(Period) and (sum((PetRefType,Process,ProcessMode)$(ProcessTable(AltStream,Process,ProcessMode)>0),
    ProcessTable(AltStream,Process,ProcessMode) * PROCMODE.l(RefReg,PetRefType,Process,ProcessMode,Period))=0)) = 0.0;

repProcModeVolBal(Process,ProcessMode) = 0 ;
repProcModeVolBal(Process,ProcessMode) =
   sum(IntStream$(not NoMat(IntStream)),
     ProcessTable(IntStream,Process,ProcessMode) ) ;

repProcessGain(DomRefReg,Gain_Process,Period,RunYears) = 0 ;
repProcessGain(DomRefReg,Gain_Process,Period,RunYears) =
  sum((RefType,Gain_Streams,ProcessMode),
     PROCMODE.l(DomRefReg,RefType,Gain_Process,ProcessMode,Period) * ProcessTable(Gain_Streams,Gain_Process,ProcessMode)) ;

repPetOutput(DomRefReg,Stream,Period,RunYears) = 0 ;
repPetOutput(DomRefReg,RecipeProd,Period,RunYears) =
  TOTPROD.l(DomRefReg,RecipeProd,Period) ;
repPetOutput(DomRefReg,CoProduct_Bal_Str,Period,RunYears) =
  sum(RefType, COPRODUCTS.l(DomRefReg,RefType,CoProduct_Bal_Str,Period) ) ;

repPetFuelUse(DomRefReg,FuelUse_Streams,Period,RunYears) = 0 ;
repPetFuelUse(DomRefReg,FuelUse_Streams,Period,RunYears) =
  sum((RefType,FuelUse_Process,ProcessMode),
     abs(ProcessTable(FuelUse_Streams,FuelUse_Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,FuelUse_Process,ProcessMode,Period) ) ;


repDomesticCrudeSup(DomRefReg,Crude,Period,RunYears) = 0 ;
repDomesticCrudeSup(DomRefReg,Crude,Period,RunYears) =
   npv_DomesticCrudeSup(DomRefReg,Crude,Period) ;

repCrudeImports(DomRefReg,Crude,Period,RunYears) = 0 ;
repCrudeImports(DomRefReg,Crude,Period,RunYears) =
   sum((Source,Step), CRUDETRANS.l(DomRefReg,Crude,Source,Step,Period) ) ;

repCrudeExports(DomRefReg,Crude,Period,RunYears) = 0 ;
repCrudeExports(DomRefReg,Crude,Period,RunYears) =
   sum((Source,Step), CRUDEEXPORT.l(DomRefReg,Crude,Source,Step,Period) ) ;


repCrudeInputs(DomRefReg,Crude,Period,RunYears) = 0 ;
repCrudeInputs(DomRefReg,Crude,Period,RunYears) =
   abs(sum((PetRefType,Process,ProcessMode)$ProcessTable(Crude,Process,ProcessMode),
     ProcessTable(Crude,Process,ProcessMode)* PROCMODE.l(DomRefReg,PetRefType,Process,ProcessMode,Period)$SupTypMode(DomRefReg,PetRefType,Process,ProcessMode) ) ) ;

repOtherSupply(DomRefReg,Stream,Period,RunYears) = 0 ;
repOtherSupply(DomRefReg,Stream,Period,RunYears) =
  IMPORTS.l(Stream,DomRefReg,Period) +
  sum(TranMode, RefRefTRAN.l('9_REFREG',DomRefReg,TranMode,Stream,Period) ) -
  EXPORTS.l(Stream,DomRefReg,Period) ;

repOtherSupply(DomRefReg,Purchase_Streams,Period,RunYears) =
  sum(Step, REFPURCH.l(DomRefReg,Purchase_Streams,Step,Period) ) ;

repOtherSupply(DomRefReg,Utility('NGS'),Period,RunYears) =
  sum((RefType,Process('H2P'),ProcessMode)$ProcessTable(Utility,Process,ProcessMode),
     abs(ProcessTable(Utility,Process,ProcessMode))*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) ;

repNonPetOther(DomRefReg,Stream,Period,RunYears) = 0 ;
repNonPetOther(DomRefReg,AltStream,Period,RunYears) =
  sum((RefType,AltFuel_Process,ProcessMode)$(ProcessTable(AltStream,AltFuel_Process,ProcessMode)>0),
     ProcessTable(AltStream,AltFuel_Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,AltFuel_Process,ProcessMode,Period) ) ;

repNonPetOther(DomRefReg,EthStream,Period,RunYears) =
  sum((RefType,Process,ProcessMode)$ProcessTable(EthStream,Process,ProcessMode),
     ProcessTable(EthStream,Process,ProcessMode)*PROCMODE.l(DomRefReg,RefType,Process,ProcessMode,Period) ) +
  ETHIMP.l(DomRefReg,EthStream,Period) -
  ETHEXP.l(DomRefReg,EthStream,Period) ;

repNonPetOther(DomRefReg,BioDieselStr,Period,RunYears) =
  sum(Step, BIODIMP.l(Step,Period) ) ;

repNonPetOther(DomRefReg,RenewDieselStr,Period,RunYears) =
  sum(Step, RENEWDIMP.l(Step,Period) ) ;

repCrudeInputsTotal(Period,RunYears) = 0 ;
repCrudeInputsTotal(Period,RunYears) =
  sum((DomRefReg,Crude),
    repCrudeInputs(DomRefReg,Crude,Period,RunYears) ) / 1000 ;

repOtherSupplyTotal(Period,RunYears) = 0 ;
repOtherSupplyTotal(Period,RunYears) =
  sum((DomRefReg,Stream),
    repOtherSupply(DomRefReg,Stream,Period,RunYears) ) / 1000 ;

repNonPetOtherTotal(Period,RunYears) = 0 ;
repNonPetOtherTotal(Period,RunYears) =
  sum((DomRefReg,Stream),
    repNonPetOther(DomRefReg,Stream,Period,RunYears) ) / 1000 ;

repProcessGainTotal(Period,RunYears) = 0 ;
repProcessGainTotal(Period,RunYears) =
  sum((DomRefReg,Gain_Process),
    repProcessGain(DomRefReg,Gain_Process,Period,RunYears) ) / 1000 ;

repPetOutputTotal(Period,RunYears) = 0 ;
repPetOutputTotal(Period,RunYears) =
  sum((DomRefReg,Stream),
    repPetOutput(DomRefReg,Stream,Period,RunYears) ) / 1000 ;

repPetFuelUseTotal(Period,RunYears) = 0 ;
repPetFuelUseTotal(Period,RunYears) =
  sum((DomRefReg,FuelUse_Streams),
    repPetFuelUse(DomRefReg,FuelUse_Streams,Period,RunYears) ) / 1000 ;

repDiscrepency(Period,RunYears) = 0 ;
repDiscrepency(Period,RunYears) =
  repCrudeInputsTotal(Period,RunYears) +
  repOtherSupplyTotal(Period,RunYears) +
  repNonPetOtherTotal(Period,RunYears) +
  repProcessGainTotal(Period,RunYears) -
  repPetOutputTotal(Period,RunYears) -
  repPetFuelUseTotal(Period,RunYears) ;

repBalanceRep(BalanceRepLabel,Period,RunYears) = 0 ;
repBalanceRep('Crude Inputs',Period,RunYears) =
  repCrudeInputsTotal(Period,RunYears) ;
repBalanceRep('Other Supply',Period,RunYears) =
  repOtherSupplyTotal(Period,RunYears) ;
repBalanceRep('Other Non-Pet',Period,RunYears) =
  repNonPetOtherTotal(Period,RunYears) ;
repBalanceRep('Refinery Gain',Period,RunYears) =
  repProcessGainTotal(Period,RunYears) ;
repBalanceRep('Consumption',Period,RunYears) =
  repPetOutputTotal(Period,RunYears) ;
repBalanceRep('Fuel Use',Period,RunYears) =
  repPetFuelUseTotal(Period,RunYears) ;
repBalanceRep('Discrepency',Period,RunYears) =
  repDiscrepency(Period,RunYears) ;

repNGLSource('Gas Plant',Period,RunYears) =
  sum((NGLInputStr,DomRefReg), npv_NGLBounds(NGLInputStr,DomRefReg,Period) ) / 1000 ;

repNGLSource('Refinery',Period,RunYears) =
* (sum((DomRefReg,RefType,RcpMode('RCP_PGSa'),Stream('PGS')),
*    abs(RecipeBlending(RcpMode,Stream))*RECIPEMODE.l(DomRefReg,RefType,RcpMode,Period) ) +
  (sum((DomRefReg,RefType,RcpMode('RCP_CC3a'),Stream('CC3')),
    abs(RecipeBlending(RcpMode,Stream))*RECIPEMODE.l(DomRefReg,RefType,RcpMode,Period) ) +
  sum((DomRefReg,RefType,RcpMode('RCP_NC4a'),Stream('NC4')),
    abs(RecipeBlending(RcpMode,Stream))*RECIPEMODE.l(DomRefReg,RefType,RcpMode,Period) ) +
  sum((DomRefReg,RefType,RcpMode('RCP_IC4a'),Stream('IC4')),
    abs(RecipeBlending(RcpMode,Stream))*RECIPEMODE.l(DomRefReg,RefType,RcpMode,Period) ) +
  sum((DomRefReg,RefType,RcpMode,Stream('UC3')),
    abs(RecipeBlending(RcpMode,Stream))*RECIPEMODE.l(DomRefReg,RefType,RcpMode,Period) ) ) / 1000;

repNGLSource('Imports',Period,RunYears) =
  sum((NGLProduct,ActiveDem), NGLIMPORTS.l(NGLProduct,ActiveDem,Period) ) / 1000 ;

repNGLConsumption('Exports',Period,RunYears) =
  sum((NGLProduct,ActiveDem), NGLEXPORTS.l(NGLProduct,ActiveDem,Period) ) / 1000 ;

repNGLConsumption('Industrial',Period,RunYears) =
  sum((ActiveDem,NGLProduct), npv_NGLDemands(ActiveDem,NGLProduct,Period) ) / 1000 ;

repNGLConsumption('Refinery Use',Period,RunYears) =
  sum((DomRefReg,RefType,Process('TRS')),
    PROCMODE.l(DomRefReg,RefType,Process,'TRS_NATngl',Period) +
    PROCMODE.l(DomRefReg,RefType,Process,'TRS_IC4ngl',Period) ) / 1000 ;

repRFSMarkups(RFSCategory,'DSUout',Period,RunYears) =
  CurrentDeflator * RFSFraction(RFSCategory,Period) * npv_PetDSFrac(Period) * RFSConstraintsRQM.m(RFSCategory,Period) ;

repRFSMarkups(RFSCategory,'CFGout',Period,RunYears) =
  CurrentDeflator * RFSFraction(RFSCategory,Period) * (0.90*npv_PetMGFrac_noETH(Period)) * RFSConstraintsRQM.m(RFSCategory,Period) ;

repRFSMarkups(RFSCategory,'CFG15out',Period,RunYears) =
  CurrentDeflator * RFSFraction(RFSCategory,Period) * (0.85*npv_PetMGFrac_noETH(Period)) * RFSConstraintsRQM.m(RFSCategory,Period) ;

repRFSMarkups(RFSCategory,'CFGb16out',Period,RunYears) =
  CurrentDeflator * RFSFraction(RFSCategory,Period) * (0.84*npv_PetMGFrac_noETH(Period)) * RFSConstraintsRQM.m(RFSCategory,Period) ;

repRFSMarkups(RFSCategory,'E85out',Period,RunYears) =
  CurrentDeflator * RFSFraction(RFSCategory,Period) * (abs(RecipeBlending('RCP_E85a','CBOB'))*npv_PetMGFrac_noETH(Period)) * RFSConstraintsRQM.m(RFSCategory,Period) ;

$ontext

BalanceCheck(stream,process,ProcessMode,'Inputs')$(not NoMat(stream))  = ROUND(ProcessTable(stream,process,ProcessMode),3);
BalanceCheck('Total',process,ProcessMode,'Inputs') = ROUND(sum(stream$(NOT NoMat(stream)),ProcessTable(stream,process,ProcessMode)),3);

BalanceCheck(stream,process,ProcessMode,'Activity')$(not NoMat(stream))= ROUND(ProcessTable(stream,process,ProcessMode)* sum((RefReg,RefType,Period),PROCMODE.l(RefReg,RefType,Process,ProcessMode,Period)),3);
BalanceCheck('Total',process,ProcessMode,'Activity') = ROUND(sum(stream$(not NoMat(stream)),ProcessTable(stream,process,ProcessMode)* sum((RefReg,RefType,Period),PROCMODE.l(RefReg,RefType,Process,ProcessMode,Period))),3);

$offtext

);   /* end 1st LOOP over FCRL=1, CURCALYR>2010, LFMMReporting=1 */


* Check for non-numeric values before passing back to NEMS
*  All NEMS reports are grouped by similar dimensions and checked for any non-numeric values.  If found, the entry 1 is made
*  See LFshell.gms for macro defninition
Loop(M1000$(ord(M1000)<COALEMM_N_PLTS),
  CheckValue(COALEMM_N_RG(M1000));
  CheckValue(COALEMM_N_RY(M1000));
  CheckValue(COALEMM_N_IGRP(M1000));
  CheckValue(COALEMM_N_CFR(M1000));
  CheckValue(COALEMM_N_HRAT(M1000));
  CheckValue(COALEMM_N_CPTY(M1000));
  CheckValue(COALEMM_N_PTP(M1000));
);

Loop(MNUMYR,
  CheckValue(CONVFACT_CFBTLLIQ(MNUMYR));
  CheckValue(CONVFACT_CFCRDLTSWT(MNUMYR));
  CheckValue(CONVFACT_CFCRDLTSOUR(MNUMYR));
  CheckValue(CONVFACT_CFCRDMD2SOUR(MNUMYR));
  CheckValue(CONVFACT_CFCRDMDSOUR(MNUMYR));
  CheckValue(CONVFACT_CFCRDHVSWT(MNUMYR));
  CheckValue(CONVFACT_CFCRDHVSOUR(MNUMYR));
  CheckValue(CONVFACT_CFCRDCA(MNUMYR));
  CheckValue(CONVFACT_CFCRDSYN(MNUMYR));
  CheckValue(CONVFACT_CFCRDDILBIT(MNUMYR));
  CheckValue(CONVFACT_CFCRDLT2SWT(MNUMYR));
  CheckValue(CONVFACT_CFCRDLSCOND(MNUMYR));
  CheckValue(CONVFACT_CFCTLLIQ(MNUMYR));
  CheckValue(CONVFACT_CFIMUO(MNUMYR));
  CheckValue(CONVFACT_CFNGL(MNUMYR));
  CheckValue(CONVFACT_CFDSRS(MNUMYR));
  CheckValue(CONVFACT_CFDSCM(MNUMYR));
  CheckValue(CONVFACT_CFDSIN(MNUMYR));
  CheckValue(CONVFACT_CFDSTR(MNUMYR));
  CheckValue(CONVFACT_CFDSEL(MNUMYR));
  CheckValue(CONVFACT_CFDSQT(MNUMYR));
  CheckValue(CONVFACT_CFE85Q(MNUMYR));
  CheckValue(CONVFACT_CFJFQ(MNUMYR));
  CheckValue(CONVFACT_CFLGQ(MNUMYR));
  CheckValue(CONVFACT_CFMGQ(MNUMYR));
  CheckValue(CONVFACT_CFPFQ(MNUMYR));
  CheckValue(CONVFACT_CFRGQ(MNUMYR));
  CheckValue(CONVFACT_CFTGQ(MNUMYR));

  CheckValue(PMMFTAB_CBIODUAL(MNUMYR));
  CheckValue(PMMFTAB_E85ICCREDIT(MNUMYR));
  CheckValue(PMMFTAB_MINREN(MNUMYR));
  CheckValue(PMMFTAB_RFIMPEXPEND(MNUMYR));

  CheckValue(PMMOUT_GLBCRDDMD(MNUMYR));

  CheckValue(QONROAD_CFDSTRHWY(MNUMYR));

  CheckValue(AB32_AB_COVD_EM_REF(MNUMYR));

  CheckValue(LFMMOUT_AB32JETCOVER(MNUMYR));

  CheckValue(LFMMOUT_BIOBUTESTK(MNUMYR));
  CheckValue(LFMMOUT_BIOBUTEIMP(MNUMYR));
  CheckValue(LFMMOUT_BIOBUTEEXP(MNUMYR));
  CheckValue(LFMMOUT_BIOBUTEPRICE(MNUMYR));

  CheckValue(INTOUT_BRENT_PRICE(MNUMYR));

  CheckValue(PMMOUT_RFSPRFR(MNUMYR));

  Loop(M11,
    CheckValue(INDREP_QCCRF(M11,MNUMYR));
  );

  Loop(M3,
   Loop(M4,
     CheckValue(LFMMOUT_MOTOR_FUEL(M4,M3,MNUMYR));
     CheckValue(LFMMOUT_DIST_FUEL(M3,M4,MNUMYR));
   );
  );

  Loop(M8,
    CheckValue(OGSMOUT_OGCO2QLF(M8,MNUMYR));
    CheckValue(OGSMOUT_OGCO2PLF(M8,MNUMYR));
  );

  Loop(M2,
  CheckValue(INTOUT_IT_WOP(MNUMYR,M2));
  CheckValue(CONVFACT_APICAMG(M2,MNUMYR));
  CheckValue(CONVFACT_APILTSW(M2,MNUMYR));
  CheckValue(CONVFACT_APILTSO(M2,MNUMYR));
  CheckValue(CONVFACT_APIMMSO(M2,MNUMYR));
  CheckValue(CONVFACT_APIMDSO(M2,MNUMYR));
  CheckValue(CONVFACT_APIHVSW(M2,MNUMYR));
  CheckValue(CONVFACT_APIHVSO(M2,MNUMYR));
  CheckValue(CONVFACT_APICA(M2,MNUMYR));
  CheckValue(CONVFACT_APISYN(M2,MNUMYR));
  CheckValue(CONVFACT_APIDIL(M2,MNUMYR));
  CheckValue(CONVFACT_APILLSW(M2,MNUMYR));
  CheckValue(CONVFACT_API50PL(M2,MNUMYR));
  CheckValue(CONVFACT_APICRDDOM(M2,MNUMYR));
  CheckValue(CONVFACT_APICRDIMP(M2,MNUMYR));
  CheckValue(CONVFACT_APICRDEXP(M2,MNUMYR));
  );

  Loop(M4,
   Loop(M13,
     CheckValue(LFMMOUT_RFSCREDITS(M4,M13,MNUMYR));
   );
   CheckValue(LFMMOUT_RFSCREDPRC(M4,MNUMYR));
   CheckValue(LFMMOUT_RFSMANDATES(M4,MNUMYR));
   CheckValue(LFMMOUT_RFSSAFETY(M4,MNUMYR));
   CheckValue(LFMMOUT_RFSACTUAL(M4,MNUMYR));
   CheckValue(LFMMOUT_RFS_WAIVER(M4,MNUMYR));
  );
  Loop(M3,
  CheckValue(CONVFACT_CFCBTLLIQ(M3,MNUMYR));
  );

  Loop(MNUMPR,
   CheckValue(LFMMOUT_BIODEXPPD(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_BIODIMPPD(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_RenewDImpPD(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_ETHTOT(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_GRD2DSQTY(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_GRN2MGQTY(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_SAF2JTQTY(MNUMPR,MNUMYR));

   CheckValue(LFMMOUT_REFPRODET(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFPRODPR(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFPRODBU(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFPRODIS(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFPRODPP(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFPRODPY(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFPRODOO(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFINPET(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFINPPR(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFINPBU(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFINPIS(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFINPPP(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFINPPY(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFINPOO(MNUMPR,MNUMYR));

   CheckValue(PMMFTAB_SBO2GDTPD(MNUMPR,MNUMYR));
   CheckValue(PMMFTAB_UBAVOLDS(MNUMPR,MNUMYR));
   CheckValue(PMMFTAB_UBAVOLMG(MNUMPR,MNUMYR));
   CheckValue(PMMFTAB_WGR2GDTPD(MNUMPR,MNUMYR));
   CheckValue(PMMFTAB_YGR2GDTPD(MNUMPR,MNUMYR));

   CheckValue(PMMFTAB_SBO2SAFPD(MNUMPR,MNUMYR));
   CheckValue(PMMFTAB_WGR2SAFPD(MNUMPR,MNUMYR));

   CheckValue(PMMOUT_QCLRFPD(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_QNGRFPD(MNUMPR,MNUMYR));
   CheckValue(PMMOUT_RFQPRCG(MNUMPR,MNUMYR));
   CheckValue(PMMOUT_UBAVOL(MNUMPR,MNUMYR));

   CheckValue(PMMRPT_BLDIMP(MNUMPR,MNUMYR));
   CheckValue(PMMRPT_RFDSTCAP(MNUMPR,MNUMYR));
   CheckValue(PMMRPT_RFDSTUTL(MNUMPR,MNUMYR));
   CheckValue(PMMRPT_RFIMCR(MNUMPR,MNUMYR));
   CheckValue(PMMRPT_RFIMTP(MNUMPR,MNUMYR));
   CheckValue(PMMRPT_RFMETM85(MNUMPR,MNUMYR));
   CheckValue(PMMRPT_RFMTBI(MNUMPR,MNUMYR));
   CheckValue(PMMRPT_RFQEXPRDT(MNUMPR,MNUMYR));
   CheckValue(PMMRPT_RFQICRD(MNUMPR,MNUMYR));
   CheckValue(PMMRPT_RFMETI(MNUMPR,MNUMYR));
   CheckValue(PMMRPT_RFQEXCRD(MNUMPR,MNUMYR));
   CheckValue(PMMFTAB_RFHCXH2IN(MNUMPR,MNUMYR));
   CheckValue(PMMRPT_RFETHE85(MNUMPR,MNUMYR));
   CheckValue(PMMOUT_XTL_CO2AVAIL(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_RFOTHERINP(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_RFBIOBUTERR(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFPRODET(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFPRODPR(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFPRODBU(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFPRODIS(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFPRODPP(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFPRODPY(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFPRODOO(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFINPET(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFINPPR(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFINPBU(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFINPIS(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFINPPP(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFINPPY(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_REFINPOO(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_SBOQGD(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_WGRQGD(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_YGRQGD(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_SBOQRJH(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_WGRQRJH(MNUMPR,MNUMYR));
   CheckValue(LFMMOUT_WGRQRDH(MNUMPR,MNUMYR));
   CheckValue(PMMRPT_RFCRDOTH(MNUMPR,MNUMYR));

   Loop(MNCRUD,
     CheckValue(LFMMOUT_RFCRUDEWHP(MNUMPR,MNCRUD,MNUMYR));
     CheckValue(LFMMOUT_RFCRUDEINP(MNUMPR,MNCRUD,MNUMYR));
     CheckValue(LFMMOUT_P_RFCRUDEINP(MNUMPR,MNCRUD,MNUMYR));
   );

   Loop(M5,
     CheckValue(EMISSION_CCS_PMM(M5,MNUMPR,MNUMYR));
   );

   Loop(M6,
     CheckValue(PMMOUT_RFQNGPL(MNUMPR,MNUMYR,M6));
   );

   Loop(M57,
     CheckValue(LFMMOUT_REF_CAP(M57,MNUMPR,MNUMYR));
     CheckValue(LFMMOUT_REF_UTL(M57,MNUMPR,MNUMYR));
   );

  Loop(M4,
     CheckValue(PMMOUT_BTLFRAC(M4,MNUMPR,MNUMYR));
     CheckValue(PMMOUT_CTLFRAC(M4,MNUMPR,MNUMYR));
     CheckValue(PMMOUT_GTLFRAC(M4,MNUMPR,MNUMYR));

     CheckValue(LFMMOUT_BIMQTY(M4,MNUMPR,MNUMYR));
     Loop(M2,
       CheckValue(PMMOUT_CBTLFRAC(M2,M4,MNUMPR,MNUMYR));
     );
  );  /* End M4_loop */
  Loop(M2,
   CheckValue(PMMRPT_RFPQIPRDT(MNUMPR,MNUMYR,M2));
   CheckValue(PMMRPT_RFPQUFC(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQMG(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQCBOB(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQRG(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQRBOB(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQPR(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQPY(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQET(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQBU(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQIS(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQPP(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQAR3(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQGO3(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQMN3(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQJF(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQDS(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQDL(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQDU(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQRL(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQRH(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQPF(MNUMPR,MNUMYR,M2));
   CheckValue(PMMRPT_RFIPQCLL(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQCD(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQCG(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQAG(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQAS(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQLU(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_RFIPQPC(MNUMPR,MNUMYR,M2));
   CheckValue(LFMMOUT_REFGAIN(MNUMPR,M2,MNUMYR));

   Loop(M6,
     CheckValue(PMMOUT_RFPQNGL(MNUMPR,MNUMYR,M6,M2));
   );

  );
); /* End MNUMPR_loop */

  Loop(M15,
     CheckValue(PMMRPT_MUFTAX(MNUMYR,M15));
  );

   Loop(PRDEXP,
     CheckValue(PMMRPT_QPRDEX(PRDEXP,MNUMYR));
   );

  Loop(MNUMCR,
   CheckValue(MPBLK_PASIN(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PDSAS(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PDSCM(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PDSEL(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PDSIN(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PDSRS(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PDSTR(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PETTR(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PJFTR(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PKSAS(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PKSCM(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PKSIN(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PKSRS(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PLGAS(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PLGCM(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PLGIN(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PLGRS(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PLGTR(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PMETR(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PMGAS(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PMGCM(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PMGIN(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PMGTR(MNUMCR,MNUMYR));
   CheckValue(MPBLK_POTAS(MNUMCR,MNUMYR));
   CheckValue(MPBLK_POTIN(MNUMCR,MNUMYR));
   CheckValue(MPBLK_POTTR(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PPFIN(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PRHAS(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PRHEL(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PRHTR(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PRLAS(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PRLCM(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PRLEL(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PRLIN(MNUMCR,MNUMYR));
   CheckValue(MPBLK_PRLTR(MNUMCR,MNUMYR));

   CheckValue(PMMFTAB_RFQNGPF(MNUMCR,MNUMYR));
   CheckValue(PMMFTAB_ADVCAPCD(MNUMCR,MNUMYR));
   CheckValue(PMMFTAB_CELLIMPFRAC(MNUMCR,MNUMYR));
   CheckValue(PMMFTAB_CLLCAPCD(MNUMCR,MNUMYR));
   CheckValue(PMMFTAB_CRNCAPCD(MNUMCR,MNUMYR));
   CheckValue(PMMFTAB_GRNCAPCD(MNUMCR,MNUMYR));
   CheckValue(LFMMOUT_GRAINCD(MNUMCR,MNUMYR));
   CheckValue(PMMFTAB_PALBOB(MNUMCR,MNUMYR));
   CheckValue(PMMFTAB_PALMG(MNUMCR,MNUMYR));
   CheckValue(PMMFTAB_WS_RBOB(MNUMCR,MNUMYR));
   CheckValue(PMMFTAB_PLMQTYCD(MNUMCR,MNUMYR));
   CheckValue(PMMFTAB_SBO_PRICE(MNUMCR,MNUMYR));
   CheckValue(PMMFTAB_SBOQTYCD(MNUMCR,MNUMYR));
   CheckValue(PMMFTAB_YGR_PRICE(MNUMCR,MNUMYR));
   CheckValue(PMMFTAB_PDSU(MNUMCR,MNUMYR));
   CheckValue(PMMFTAB_PDSL(MNUMCR,MNUMYR));
   CheckValue(PMMFTAB_PDSCRB(MNUMCR,MNUMYR));
   CheckValue(PMMFTAB_DSCSHR(MNUMCR,MNUMYR));
   CheckValue(PMMFTAB_PJF(MNUMCR,MNUMYR));
   CheckValue(PMMFTAB_PDS(MNUMCR,MNUMYR));

   CheckValue(PMMOUT_CRNPRICE(MNUMCR,MNUMYR));
   CheckValue(PMMOUT_QBMRFBTL(MNUMCR,MNUMYR));
   CheckValue(PMMOUT_QMERF(MNUMCR,MNUMYR));

   CheckValue(PMMRPT_BIODEXP(MNUMCR,MNUMYR));
   CheckValue(LFMMOUT_RenewDIMP(MNUMCR,MNUMYR));
   CheckValue(PMMRPT_BIODIMP(MNUMCR,MNUMYR));
   CheckValue(PMMRPT_BIODPRICE(MNUMCR,MNUMYR));
   CheckValue(PMMRPT_CLLETHCD(MNUMCR,MNUMYR));
   Loop(CORNTO,
      CheckValue(LFMMOUT_CORNCD(CORNTO,MNUMCR,MNUMYR));
   );
   CheckValue(PMMRPT_CRNETHCD(MNUMCR,MNUMYR));
   CheckValue(PMMRPT_ETHE85CD(MNUMCR,MNUMYR));
   CheckValue(PMMRPT_ETHEXP(MNUMCR,MNUMYR));
   CheckValue(PMMRPT_ETHIMP(MNUMCR,MNUMYR));
   CheckValue(PMMRPT_ETHTOTCD(MNUMCR,MNUMYR));
   CheckValue(PMMRPT_GRNETHCD(MNUMCR,MNUMYR));
   CheckValue(PMMRPT_OTHETHCD(MNUMCR,MNUMYR));
   CheckValue(PMMRPT_PETHANOL(MNUMCR,MNUMYR));
   CheckValue(PMMRPT_PETHM(MNUMCR,MNUMYR));
   CheckValue(PMMRPT_RFQARO(MNUMCR,MNUMYR));
   CheckValue(PMMRPT_RFQDS(MNUMCR,MNUMYR));
   CheckValue(PMMRPT_RFQJF(MNUMCR,MNUMYR));
   CheckValue(PMMRPT_RFQKS(MNUMCR,MNUMYR));
   CheckValue(PMMRPT_RFQLG(MNUMCR,MNUMYR));
   CheckValue(PMMRPT_RFQMG(MNUMCR,MNUMYR));
   CheckValue(PMMRPT_RFQOTH(MNUMCR,MNUMYR));
   CheckValue(PMMRPT_RFQPCK(MNUMCR,MNUMYR));
   CheckValue(PMMRPT_RFQPF(MNUMCR,MNUMYR));
   CheckValue(PMMRPT_RFQRH(MNUMCR,MNUMYR));
   CheckValue(PMMRPT_RFQRL(MNUMCR,MNUMYR));
   CheckValue(PMMRPT_RFQSTG(MNUMCR,MNUMYR));
   CheckValue(PMMRPT_TDIESEL(MNUMCR,MNUMYR));

   CheckValue(PMORE_PBUIN(MNUMCR,MNUMYR));
   CheckValue(PMORE_PBUINPF(MNUMCR,MNUMYR));
   CheckValue(PMORE_PETIN(MNUMCR,MNUMYR));
   CheckValue(PMORE_PETINPF(MNUMCR,MNUMYR));
   CheckValue(PMORE_PISIN(MNUMCR,MNUMYR));
   CheckValue(PMORE_PISINPF(MNUMCR,MNUMYR));
   CheckValue(PMORE_PLGINPF(MNUMCR,MNUMYR));
   CheckValue(PMORE_PLUIN(MNUMCR,MNUMYR));
   CheckValue(PMORE_PPPIN(MNUMCR,MNUMYR));
   CheckValue(PMORE_PPPINPF(MNUMCR,MNUMYR));
   CheckValue(PMORE_PPRCM(MNUMCR,MNUMYR));
   CheckValue(PMORE_PPRIN(MNUMCR,MNUMYR));
   CheckValue(PMORE_PPRINPF(MNUMCR,MNUMYR));
   CheckValue(PMORE_PPROLENERF(MNUMCR,MNUMYR));
   CheckValue(PMORE_PPRRS(MNUMCR,MNUMYR));
   CheckValue(PMORE_PPRTR(MNUMCR,MNUMYR));
   CheckValue(PMORE_PSULFURIN(MNUMCR,MNUMYR));

   CheckValue(PONROAD_PDSTRHWY(MNUMCR,MNUMYR));

   CheckValue(QBLK_QCLRF(MNUMCR,MNUMYR));
   CheckValue(QBLK_QELRF(MNUMCR,MNUMYR));
   CheckValue(QBLK_QLGRF(MNUMCR,MNUMYR));
   CheckValue(QMORE_QPRRF(MNUMCR,MNUMYR));
   CheckValue(QMORE_QPYRF(MNUMCR,MNUMYR));
   CheckValue(QMORE_QBURF(MNUMCR,MNUMYR));
   CheckValue(QMORE_QISRF(MNUMCR,MNUMYR));
   CheckValue(QBLK_QNGRF(MNUMCR,MNUMYR));
   CheckValue(QBLK_QOTRF(MNUMCR,MNUMYR));
   CheckValue(QBLK_QPCRF(MNUMCR,MNUMYR));
   CheckValue(QBLK_QRLRF(MNUMCR,MNUMYR));
   CheckValue(QBLK_QSGRF(MNUMCR,MNUMYR));
   CheckValue(QBLK_QBMRF(MNUMCR,MNUMYR));

   CheckValue(QONROAD_QDSTRHWY(MNUMCR,MNUMYR));

   CheckValue(TRANREP_E85AVAIL(MNUMCR,MNUMYR));

   CheckValue(WRENEW_QCLETH(MNUMYR,MNUMCR));
   CheckValue(WRENEW_QELETH(MNUMYR,MNUMCR));
   CheckValue(WRENEW_QNGETH(MNUMYR,MNUMCR));

   CheckValue(LFMMOUT_RFSDSTR(MNUMCR,MNUMYR));
   CheckValue(LFMMOUT_RFSMGTR(MNUMCR,MNUMYR));
   CheckValue(LFMMOUT_RFSRBOB(MNUMCR,MNUMYR));
   CheckValue(LFMMOUT_RFSJFTR(MNUMCR,MNUMYR));
   CheckValue(LFMMOUT_RFSDSRS(MNUMCR,MNUMYR));
   CheckValue(LFMMOUT_RFBIOBUTECD(MNUMCR,MNUMYR));
   CheckValue(LFMMOUT_QBIOBUTE(MNUMCR,MNUMYR));

   Loop(M2,
     CheckValue(PMMRPT_BIODCONCD(M2,MNUMCR,MNUMYR));
     CheckValue(PMMFTAB_DSMUTR(MNUMCR,MNUMYR,M2));
     CheckValue(PMMFTAB_DSMURS(MNUMCR,MNUMYR,M2));
     CheckValue(PMMFTAB_MGMUTR(MNUMCR,MNUMYR,M2));
     CheckValue(PMMFTAB_JFMUTR(MNUMCR,MNUMYR,M2));
   );

   Loop(NUMCGF,
     CheckValue(COGEN_CGREFCAP(MNUMCR,MNUMYR,NUMCGF));
     CheckValue(COGEN_CGREFQ(MNUMCR,MNUMYR,NUMCGF));
     Loop(M2,
       CheckValue(COGEN_CGREFGEN(MNUMCR,MNUMYR,NUMCGF,M2));
     );
   );

   Loop(M4,
     CheckValue(PMMRPT_BIMQTYCD(M4,MNUMCR,MNUMYR));
   );

   Loop(M20,
     CheckValue(PMMFTAB_RFENVFX(MNUMCR,MNUMYR,M20));
   );

  ); /* End_of MNUMCR_loop */
);/* End_of MNUMYR_loop */

Loop((MNMFS1,NDRGN1,MNMYRF),
   CheckValue(WRENEW_QBMBTCL(MNMFS1,NDRGN1,MNMYRF));
   CheckValue(WRENEW_QBMETCL(MNMFS1,NDRGN1,MNMYRF));
);
Loop(MNXYRS,
   Loop((M10,MNCRUD),
      CheckValue(LFMMOUT_P_CRUDE_IMPORTS(M10,MNCRUD,MNXYRS));
      CheckValue(LFMMOUT_Q_CRUDE_IMPORTS(M10,MNCRUD,MNXYRS));
      CheckValue(LFMMOUT_Q_CRUDE_IMPORTA(M10,MNCRUD,MNXYRS));
      CheckValue(LFMMOUT_P_CRUDE_EXPORTS(M10,MNCRUD,MNXYRS));
      CheckValue(LFMMOUT_Q_CRUDE_EXPORTS(M10,MNCRUD,MNXYRS));
   );
   Loop((MNUMPR,MNCRUD),
      CheckValue(LFMMOUT_P_CRUDE_TO_CAN(MNUMPR,MNCRUD,MNXYRS));
      CheckValue(LFMMOUT_Q_CRUDE_TO_CAN(MNUMPR,MNCRUD,MNXYRS));
   );
   Loop(LCFS_C,
      CheckValue(LFMMOUT_LCFS_Baseline(LCFS_C,MNXYRS));
      CheckValue(LFMMOUT_LCFS_Actual(LCFS_C,MNXYRS));
      CheckValue(LFMMOUT_LCFS_Waiver(LCFS_C,MNXYRS));
      CheckValue(LFMMOUT_LCFS_Offset_Prc(LCFS_C,MNXYRS));
      CheckValue(LFMMOUT_LCFS_Carb_Offset(LCFS_C,MNXYRS));
      CheckValue(LFMMOUT_LCFS_PeToTrills(LCFS_C,MNXYRS));
   );
   Loop(LCFS_C,
      CheckValue(LFMMOUT_CFP_Baseline(LCFS_C,MNXYRS));
      CheckValue(LFMMOUT_CFP_Actual(LCFS_C,MNXYRS));
      CheckValue(LFMMOUT_CFP_Waiver(LCFS_C,MNXYRS));
      CheckValue(LFMMOUT_CFP_Offset_Prc(LCFS_C,MNXYRS));
      CheckValue(LFMMOUT_CFP_Carb_Offset(LCFS_C,MNXYRS));
      CheckValue(LFMMOUT_CFP_PeToTrills(LCFS_C,MNXYRS));
   );

);

*LFMM_mac.gdx reporting parameters
*-------------------------------------------------------------------------------------------------------------------
If(NCNTRL_CURCALYR>2010 and NCNTRL_FCRL=1,
repStreamFlows(DomRefReg,Stream,StreamNames,StreamType,Industry,t) = 0 ;

repProcesses(DomRefReg,BioProcess,'Biofuels','Nameplate',RunYears) =
    CapExpSize(BioProcess) ;
repProcesses(DomRefReg,BioProcess,'Biofuels','ProductionCapacity',RunYears) =
    sum(RefType,AvailCap(DomRefReg,RefType,BioProcess)) ;
repProcesses(DomRefReg,BioProcess,'Biofuels','CapacityFactor',RunYears)$(sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period)) > 0) =
    sum((RefType,ProcessMode,Period(PrcPeriod)),
    (CapFactor(BioProcess,ProcessMode)*PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period))$ProcProcMode(BioProcess,ProcessMode)) /
    sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period)$ProcProcMode(BioProcess,ProcessMode));
repProcesses(DomRefReg,BioProcess,'Biofuels','Production',RunYears) =
    sum((Period(PrcPeriod),ProcessMode),
        PROCMODE.l(DomRefReg,'Coking',BioProcess,ProcessMode,Period)$ProcProcMode(BioProcess,ProcessMode)) / 1000;
repProcesses(DomRefReg,BioProcess,'Biofuels','BuildYears',RunYears) =
    InvFactors('BldYrs','NonPet') ;
repProcesses(DomRefReg,BioProcess,'Biofuels','EquipmentCost',RunYears) =
    1.714 * AdjISBL(BioProcess,DomRefReg) ;
repProcesses(DomRefReg,BioProcess,'Biofuels','FieldCost',RunYears) =
    1.714 * TotalFieldCost(BioProcess,DomRefReg) ;
repProcesses(DomRefReg,BioProcess,'Biofuels','BaseOvernight',RunYears) =
    1.714 * TotalProjInv(BioProcess,DomRefReg) / (1 + InvFactors('PCTCNTG','NonPet')) ;
repProcesses(DomRefReg,BioProcess,'Biofuels','OvernightCapex',RunYears) =
    1.714 * TotalProjInv(BioProcess,DomRefReg) ;
repProcesses(DomRefReg,BioProcess,'Biofuels','FixedCapitalInvestment',RunYears) =
    1.714 * FixedCapInv(BioProcess,DomRefReg) ;
repProcesses(DomRefReg,BioProcess,'Biofuels','TotalProjectCost',RunYears) =
    1.714 * TPIStartYr(BioProcess,DomRefReg) ;
repProcesses(DomRefReg,BioProcess,'Biofuels','ProjectInvestment',RunYears) =
    1.714 * PVprjInv(BioProcess,DomRefReg) ;
repProcesses(DomRefReg,BioProcess,'Biofuels','AfterTaxCreditAnnuity',RunYears) =
    1.714 * Caprec(BioProcess,DomRefReg) ;
repProcesses(DomRefReg,BioProcess,'Biofuels','BuildCost',RunYears) =
    Currentdeflator * sum(Period(PrcPeriod),npv_BuildCost(BioProcess,DomRefReg,'Step01',Period));
repProcesses(DomRefReg,BioProcess,'Biofuels','FixedOpCost',RunYears)=
    Currentdeflator * sum(Period(PrcPeriod),npv_FXOCCost(BioProcess,DomRefReg,Period));
repProcesses(DomRefReg,BioProcess,'Biofuels','FeedstockCost',RunYears)$(sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period)) > 0) =
    Currentdeflator * sum((RefType,BioStr,ProcessMode,Period(PrcPeriod)),
    (BioRefRegBal.m(DomRefReg,BioStr,Period)*PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period))$(ProcProcMode(BioProcess,ProcessMode) And ProcessTable(BioStr,BioProcess,ProcessMode)))/
    sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period)$ProcProcMode(BioProcess,ProcessMode)) ;
repProcesses(DomRefReg,BioProcess,'Biofuels','ElectricityUse',RunYears)$(sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period)) > 0) =
    sum((RefType,ProcessMode,Period(PrcPeriod)),
    (ProcessTable('KWH',BioProcess,ProcessMode)*PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period))$ProcProcMode(BioProcess,ProcessMode)) /
    sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period)$ProcProcMode(BioProcess,ProcessMode)) ;
repProcesses(DomRefReg,BioProcess,'Biofuels','ElectricitySales',RunYears)$(sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period)) > 0) =
    sum((RefType,ProcessMode,Period(PrcPeriod)),
    (ProcessTable('KWGrid',BioProcess,ProcessMode)*PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period))$ProcProcMode(BioProcess,ProcessMode)) /
    sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period)$ProcProcMode(BioProcess,ProcessMode)) ;
repProcesses(DomRefReg,BioProcess,'Biofuels','VariableOpCost',RunYears)$(sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period)) > 0)=
    Currentdeflator * sum((RefType,ProcessMode,Period(PrcPeriod)),
    (npv_OpVarCost(BioProcess,ProcessMode,Period)*PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period))$ProcProcMode(BioProcess,ProcessMode)) /
    sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period)$ProcProcMode(BioProcess,ProcessMode)) ;
repProcesses(DomRefReg,BioProcess,'Biofuels','FeedUse',RunYears)$(sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period)) > 0) =
    sum((RefType,ProcessMode,Feedstocks,Period(PrcPeriod)),
    (ProcessTable(Feedstocks,BioProcess,ProcessMode)*PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period))$ProcProcMode(BioProcess,ProcessMode)) /
    sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period)$ProcProcMode(BioProcess,ProcessMode)) ;
repProcesses(DomRefReg,BioProcess,'Biofuels','FuelYield',RunYears)$(sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period)) > 0) =
    sum((RefType,ProcessMode,FuelStreams,Period(PrcPeriod)),
    (ProcessTable(FuelStreams,BioProcess,ProcessMode)*PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period))$ProcProcMode(BioProcess,ProcessMode)) /
    sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period)$ProcProcMode(BioProcess,ProcessMode)) ;
repProcesses(DomRefReg,BioProcess,'Biofuels','CoproductYield',RunYears)$(sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period)) > 0) =
    sum((RefType,ProcessMode,NonfuelStreams,Period(PrcPeriod)),
    (ProcessTable(NonfuelStreams,BioProcess,ProcessMode)*PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period))$ProcProcMode(BioProcess,ProcessMode)) /
    sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period)$ProcProcMode(BioProcess,ProcessMode)) ;

repProcesses(DomRefReg,NPFProcess,'NonPetroleumFossil','Nameplate',RunYears) =
    CapExpSize(NPFProcess) ;
repProcesses(DomRefReg,NPFProcess,'NonPetroleumFossil','ProductionCapacity',RunYears) =
    sum(RefType,AvailCap(DomRefReg,RefType,NPFProcess)) ;
repProcesses(DomRefReg,NPFProcess,'NonPetroleumFossil','CapacityFactor',RunYears)$(sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,NPFProcess,ProcessMode,Period)) > 0) =
    sum((ProcessMode,Period(PrcPeriod)),
    (CapFactor(NPFProcess,ProcessMode)*PROCMODE.l(DomRefReg,'Coking',NPFProcess,ProcessMode,Period))$ProcProcMode(NPFProcess,ProcessMode)) /
    sum((ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,'Coking',NPFProcess,ProcessMode,Period)$ProcProcMode(NPFProcess,ProcessMode));
repProcesses(DomRefReg,NPFProcess,'NonPetroleumFossil','Production',RunYears) =
    sum((Period(PrcPeriod),ProcessMode),
        PROCMODE.l(DomRefReg,'Coking',NPFProcess,ProcessMode,Period)$ProcProcMode(NPFProcess,ProcessMode)) / 1000;
repProcesses(DomRefReg,NPFProcess,'NonPetroleumFossil','BuildYears',RunYears) =
    InvFactors('BldYrs','NonPet') ;
repProcesses(DomRefReg,NPFProcess,'NonPetroleumFossil','EquipmentCost',RunYears) =
    1.714 * AdjISBL(NPFProcess,DomRefReg) ;
repProcesses(DomRefReg,NPFProcess,'NonPetroleumFossil','FieldCost',RunYears) =
    1.714 * TotalFieldCost(NPFProcess,DomRefReg) ;
repProcesses(DomRefReg,NPFProcess,'NonPetroleumFossil','OvernightCapex',RunYears) =
    1.714 * TotalProjInv(NPFProcess,DomRefReg) ;
repProcesses(DomRefReg,NPFProcess,'NonPetroleumFossil','FixedCapitalInvestment',RunYears) =
    1.714 * FixedCapInv(NPFProcess,DomRefReg) ;
repProcesses(DomRefReg,NPFProcess,'NonPetroleumFossil','TotalProjectCost',RunYears) =
    1.714 * TPIStartYr(NPFProcess,DomRefReg) ;
repProcesses(DomRefReg,NPFProcess,'NonPetroleumFossil','ProjectInvestment',RunYears) =
    1.714 * PVprjInv(NPFProcess,DomRefReg) ;
repProcesses(DomRefReg,NPFProcess,'NonPetroleumFossil','AfterTaxCreditAnnuity',RunYears) =
    1.714 * Caprec(NPFProcess,DomRefReg) ;
repProcesses(DomRefReg,NPFProcess,'NonPetroleumFossil','BuildCost',RunYears) =
    Currentdeflator * sum(Period(PrcPeriod),npv_BuildCost(NPFProcess,DomRefReg,'Step01',Period));
repProcesses(DomRefReg,NPFProcess,'NonPetroleumFossil','FixedOpCost',RunYears)=
    Currentdeflator * sum(Period(PrcPeriod),npv_FXOCCost(NPFProcess,DomRefReg,Period));
repProcesses(DomRefReg,NPFProcess,'NonPetroleumFossil','FeedstockCost',RunYears)$(sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,NPFProcess,ProcessMode,Period)) > 0) =
    Currentdeflator * sum((RefType,BioStr,ProcessMode,Period(PrcPeriod)),
    (BioRefRegBal.m(DomRefReg,BioStr,Period)*PROCMODE.l(DomRefReg,RefType,NPFProcess,ProcessMode,Period))$(ProcProcMode(NPFProcess,ProcessMode) And ProcessTable(BioStr,NPFProcess,ProcessMode)))/
    sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,NPFProcess,ProcessMode,Period)$ProcProcMode(NPFProcess,ProcessMode)) ;
repProcesses(DomRefReg,NPFProcess,'NonPetroleumFossil','ElectricityUse',RunYears)$(sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,NPFProcess,ProcessMode,Period)) > 0) =
    sum((RefType,ProcessMode,Period(PrcPeriod)),
    (ProcessTable('KWH',NPFProcess,ProcessMode)*PROCMODE.l(DomRefReg,RefType,NPFProcess,ProcessMode,Period))$ProcProcMode(NPFProcess,ProcessMode)) /
    sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,NPFProcess,ProcessMode,Period)$ProcProcMode(NPFProcess,ProcessMode)) ;
repProcesses(DomRefReg,NPFProcess,'NonPetroleumFossil','ElectricitySales',RunYears)$(sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,NPFProcess,ProcessMode,Period)) > 0) =
    sum((RefType,ProcessMode,Period(PrcPeriod)),
    (ProcessTable('KWGrid',NPFProcess,ProcessMode)*PROCMODE.l(DomRefReg,RefType,NPFProcess,ProcessMode,Period))$ProcProcMode(NPFProcess,ProcessMode)) /
    sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,NPFProcess,ProcessMode,Period)$ProcProcMode(NPFProcess,ProcessMode)) ;
repProcesses(DomRefReg,NPFProcess,'NonPetroleumFossil','VariableOpCost',RunYears)$(sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,NPFProcess,ProcessMode,Period)) > 0)=
    Currentdeflator * sum((RefType,ProcessMode,Period(PrcPeriod)),
    (npv_OpVarCost(NPFProcess,ProcessMode,Period)*PROCMODE.l(DomRefReg,RefType,NPFProcess,ProcessMode,Period))$ProcProcMode(NPFProcess,ProcessMode)) /
    sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,NPFProcess,ProcessMode,Period)$ProcProcMode(NPFProcess,ProcessMode)) ;
repProcesses(DomRefReg,NPFProcess,'NonPetroleumFossil','FeedUse',RunYears)$(sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,NPFProcess,ProcessMode,Period)) > 0) =
    sum((RefType,ProcessMode,Feedstocks,Period(PrcPeriod)),
    (ProcessTable(Feedstocks,NPFProcess,ProcessMode)*PROCMODE.l(DomRefReg,RefType,NPFProcess,ProcessMode,Period))$ProcProcMode(NPFProcess,ProcessMode)) /
    sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,NPFProcess,ProcessMode,Period)$ProcProcMode(NPFProcess,ProcessMode)) ;
repProcesses(DomRefReg,NPFProcess,'NonPetroleumFossil','FuelYield',RunYears)$(sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,NPFProcess,ProcessMode,Period)) > 0) =
    sum((RefType,ProcessMode,FuelStreams,Period(PrcPeriod)),
    (ProcessTable(FuelStreams,NPFProcess,ProcessMode)*PROCMODE.l(DomRefReg,RefType,NPFProcess,ProcessMode,Period))$ProcProcMode(NPFProcess,ProcessMode)) /
    sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,NPFProcess,ProcessMode,Period)$ProcProcMode(NPFProcess,ProcessMode)) ;
repProcesses(DomRefReg,NPFProcess,'NonPetroleumFossil','CoproductYield',RunYears)$(sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,NPFProcess,ProcessMode,Period)) > 0) =
    sum((RefType,ProcessMode,NonfuelStreams,Period(PrcPeriod)),
    (ProcessTable(NonfuelStreams,NPFProcess,ProcessMode)*PROCMODE.l(DomRefReg,RefType,NPFProcess,ProcessMode,Period))$ProcProcMode(NPFProcess,ProcessMode)) /
    sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,NPFProcess,ProcessMode,Period)$ProcProcMode(NPFProcess,ProcessMode)) ;

repProcesses(DomRefReg,PetroleumProcess,'Petroleum','Nameplate',RunYears) =
    CapExpSize(PetroleumProcess) ;
repProcesses(DomRefReg,PetroleumProcess,'Petroleum','ProductionCapacity',RunYears) =
    sum(RefType,AvailCap(DomRefReg,RefType,PetroleumProcess)) ;
repProcesses(DomRefReg,PetroleumProcess,'Petroleum','CapacityFactor',RunYears)$(sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,PetroleumProcess,ProcessMode,Period)) > 0) =
    sum((ProcessMode,Period(PrcPeriod)),
    (CapFactor(PetroleumProcess,ProcessMode)*PROCMODE.l(DomRefReg,'Coking',PetroleumProcess,ProcessMode,Period))$ProcProcMode(PetroleumProcess,ProcessMode)) /
    sum((ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,'Coking',PetroleumProcess,ProcessMode,Period)$ProcProcMode(PetroleumProcess,ProcessMode));
repProcesses(DomRefReg,PetroleumProcess,'Petroleum','Production',RunYears) =
    sum((Period(PrcPeriod),ProcessMode),
        PROCMODE.l(DomRefReg,'Coking',PetroleumProcess,ProcessMode,Period)$ProcProcMode(PetroleumProcess,ProcessMode)) / 1000;
repProcesses(DomRefReg,PetroleumProcess,'Petroleum','BuildYears',RunYears) =
    InvFactors('BldYrs','AvgRiskPet') ;
repProcesses(DomRefReg,PetroleumProcess,'Petroleum','EquipmentCost',RunYears) =
    1.714 * AdjISBL(PetroleumProcess,DomRefReg) ;
repProcesses(DomRefReg,PetroleumProcess,'Petroleum','FieldCost',RunYears) =
    1.714 * TotalFieldCost(PetroleumProcess,DomRefReg) ;
repProcesses(DomRefReg,PetroleumProcess,'Petroleum','OvernightCapex',RunYears) =
    1.714 * TotalProjInv(PetroleumProcess,DomRefReg) ;
repProcesses(DomRefReg,PetroleumProcess,'Petroleum','FixedCapitalInvestment',RunYears) =
    1.714 * FixedCapInv(PetroleumProcess,DomRefReg) ;
repProcesses(DomRefReg,PetroleumProcess,'Petroleum','TotalProjectCost',RunYears) =
    1.714 * TPIStartYr(PetroleumProcess,DomRefReg) ;
repProcesses(DomRefReg,PetroleumProcess,'Petroleum','ProjectInvestment',RunYears) =
    1.714 * PVprjInv(PetroleumProcess,DomRefReg) ;
repProcesses(DomRefReg,PetroleumProcess,'Petroleum','AfterTaxCreditAnnuity',RunYears) =
    1.714 * Caprec(PetroleumProcess,DomRefReg) ;
repProcesses(DomRefReg,PetroleumProcess,'Petroleum','BuildCost',RunYears) =
    Currentdeflator * sum(Period(PrcPeriod),npv_BuildCost(PetroleumProcess,DomRefReg,'Step01',Period));
repProcesses(DomRefReg,PetroleumProcess,'Petroleum','FixedOpCost',RunYears)=
    Currentdeflator * sum(Period(PrcPeriod),npv_FXOCCost(PetroleumProcess,DomRefReg,Period));
repProcesses(DomRefReg,PetroleumProcess,'Petroleum','ElectricityUse',RunYears)$(sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,PetroleumProcess,ProcessMode,Period)) > 0) =
    sum((RefType,ProcessMode,Period(PrcPeriod)),
    (ProcessTable('KWH',PetroleumProcess,ProcessMode)*PROCMODE.l(DomRefReg,RefType,PetroleumProcess,ProcessMode,Period))$ProcProcMode(PetroleumProcess,ProcessMode)) /
    sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,PetroleumProcess,ProcessMode,Period)$ProcProcMode(PetroleumProcess,ProcessMode)) ;
repProcesses(DomRefReg,PetroleumProcess,'Petroleum','ElectricitySales',RunYears)$(sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,PetroleumProcess,ProcessMode,Period)) > 0) =
    sum((RefType,ProcessMode,Period(PrcPeriod)),
    (ProcessTable('KWGrid',PetroleumProcess,ProcessMode)*PROCMODE.l(DomRefReg,RefType,PetroleumProcess,ProcessMode,Period))$ProcProcMode(PetroleumProcess,ProcessMode)) /
    sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,PetroleumProcess,ProcessMode,Period)$ProcProcMode(PetroleumProcess,ProcessMode)) ;
repProcesses(DomRefReg,PetroleumProcess,'Petroleum','VariableOpCost',RunYears)$(sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,PetroleumProcess,ProcessMode,Period)) > 0)=
    Currentdeflator * sum((RefType,ProcessMode,Period(PrcPeriod)),
    (npv_OpVarCost(PetroleumProcess,ProcessMode,Period)*PROCMODE.l(DomRefReg,RefType,PetroleumProcess,ProcessMode,Period))$ProcProcMode(PetroleumProcess,ProcessMode)) /
    sum((RefType,ProcessMode,Period(PrcPeriod)),PROCMODE.l(DomRefReg,RefType,PetroleumProcess,ProcessMode,Period)$ProcProcMode(PetroleumProcess,ProcessMode)) ;

repStreamProperties(Stream,StreamNames,StreamType,Industry,Property) = 0 ;

repStreamProperties(Crude,'Crude','Feedstocks','Petroleum',Property) = StreamProp(Crude,Property) ;
repStreamProperties(UnfinishedOils,'UFOImports','Feedstocks','Petroleum',Property) = StreamProp(UnfinishedOils,Property) ;
repStreamProperties(NGLInputStr,'GasLiquids','Feedstocks','NonPetroleumFossil',Property) = StreamProp(NGLInputStr,Property) ;
repStreamProperties(NGLProduct,'GasLiquids','Feedstocks','NonPetroleumFossil',Property) = StreamProp(NGLProduct,Property) ;
repStreamProperties(Coal,'Coal','Feedstocks','NonPetroleumFossil',Property) = StreamProp(Coal,Property) ;
repStreamProperties(Gas,'Gas','Feedstocks','NonPetroleumFossil',Property) = StreamProp(Gas,Property) ;
repStreamProperties(GrainCrops,'Grains','Feedstocks','Biofuels',Property) = StreamProp(GrainCrops,Property) ;
repStreamProperties(BioStr,'Cellulose','Feedstocks','Biofuels',Property) = StreamProp(BioStr,Property) ;
repStreamProperties(RenewableOils,'Oils','Feedstocks','Biofuels',Property) = StreamProp(RenewableOils,Property) ;

repStreamProperties(BioNaphtha,'Naphtha','Intermediates','Biofuels',Property) = StreamProp(BioNaphtha,Property) ;
repStreamProperties(BioDistillate,'Distillate','Intermediates','Biofuels',Property) = StreamProp(BioDistillate,Property) ;
repStreamProperties(NPFNaphtha,'Naphtha','Intermediates','NonPetroleumFossil',Property) = StreamProp(NPFNaphtha,Property) ;
repStreamProperties(NPFDistillate,'Distillate','Intermediates','NonPetroleumFossil',Property) = StreamProp(NPFDistillate,Property) ;
repStreamProperties(Alkylate,'Alkylate','Intermediates','Petroleum',Property) = StreamProp(Alkylate,Property) ;
repStreamProperties(ReformatePet,'Reformate','Intermediates','Petroleum',Property) = StreamProp(ReformatePet,Property) ;
repStreamProperties(NaphthaLightPet,'NaphthaLight','Intermediates','Petroleum',Property) = StreamProp(NaphthaLightPet,Property) ;
repStreamProperties(NaphthaMedium,'NaphthaMedium','Intermediates','Petroleum',Property) = StreamProp(NaphthaMedium,Property) ;
repStreamProperties(NaphthaHeavyPet,'NaphthaHeavy','Intermediates','Petroleum',Property) = StreamProp(NaphthaHeavyPet,Property) ;
repStreamProperties(GasolineCat,'CatGasoline','Intermediates','Petroleum',Property) = StreamProp(GasolineCat,Property) ;
repStreamProperties(KerosenePet,'Kerosene','Intermediates','Petroleum',Property) = StreamProp(KerosenePet,Property) ;
repStreamProperties(DieselPet,'Diesel','Intermediates','Petroleum',Property) = StreamProp(DieselPet,Property) ;
repStreamProperties(DieselHeavy,'DieselHeavy','Intermediates','Petroleum',Property) = StreamProp(DieselHeavy,Property) ;
repStreamProperties(LightCycleOil,'LightCycleOil','Intermediates','Petroleum',Property) = StreamProp(LightCycleOil,Property) ;
repStreamProperties(GasOilLight,'GasOilLight','Intermediates','Petroleum',Property) = StreamProp(GasOilLight,Property) ;
repStreamProperties(GasOil,'GasOil','Intermediates','Petroleum',Property) = StreamProp(GasOil,Property) ;
repStreamProperties(GasOilHeavy,'GasOilHeavy','Intermediates','Petroleum',Property) = StreamProp(GasOilHeavy,Property) ;
repStreamProperties(ResidualOil,'Resid','Intermediates','Petroleum',Property) = StreamProp(ResidualOil,Property) ;
repStreamProperties(ChemIntermediates,'Chemicals','Products','Blending',Property) = StreamProp(ChemIntermediates,Property) ;

repStreamProperties(Chemicals,'Chemicals','Products','Blending',Property) = StreamProp(Chemicals,Property) ;

repStreamProperties(PetNaphtha,'CBOB','Products','Petroleum',Property) = StreamProp(PetNaphtha,Property) ;
repStreamProperties(PetNaphtha,'RBOB','Products','Petroleum',Property) = StreamProp(PetNaphtha,Property) ;
repStreamProperties(PetNaphtha,'CaRBOB','Products','Petroleum',Property) = StreamProp(PetNaphtha,Property) ;
repStreamProperties(PetDistillate,'ULSD','Products','Petroleum',Property) = StreamProp(PetDistillate,Property) ;
repStreamProperties(PetDistillate,'LowSulfurDiesel','Products','Petroleum',Property) = StreamProp(PetDistillate,Property) ;
repStreamProperties(PetDistillate,'CarbULSD','Products','Petroleum',Property) = StreamProp(PetDistillate,Property) ;
repStreamProperties(PetDistillate,'HeatingOil','Products','Petroleum',Property) = StreamProp(PetDistillate,Property) ;
repStreamProperties(PetDistillate,'JetFuel','Products','Petroleum',Property) = StreamProp(PetDistillate,Property) ;
repStreamProperties(PetResid,'Resid','Products','Petroleum',Property) = StreamProp(PetResid,Property) ;
repStreamProperties(Solids,'Chemicals','Products','Petroleum',Property) = StreamProp(Solids,Property) ;

repStreamProperties(NPFNaphtha,'CBOB','Products','NonPetroleumFossil',Property) = StreamProp(NPFNaphtha,Property) ;
repStreamProperties(NPFNaphtha,'RBOB','Products','NonPetroleumFossil',Property) = StreamProp(NPFNaphtha,Property) ;
repStreamProperties(NPFNaphtha,'CaRBOB','Products','NonPetroleumFossil',Property) = StreamProp(NPFNaphtha,Property) ;
repStreamProperties(NPFDistillate,'ULSD','Products','NonPetroleumFossil',Property) = StreamProp(NPFDistillate,Property) ;
repStreamProperties(NPFDistillate,'LowSulfurDiesel','Products','NonPetroleumFossil',Property) = StreamProp(NPFDistillate,Property) ;
repStreamProperties(NPFDistillate,'CarbULSD','Products','NonPetroleumFossil',Property) = StreamProp(NPFDistillate,Property) ;
repStreamProperties(NPFDistillate,'HeatingOil','Products','NonPetroleumFossil',Property) = StreamProp(NPFDistillate,Property) ;
repStreamProperties(NPFDistillate,'JetFuel','Products','NonPetroleumFossil',Property) = StreamProp(NPFDistillate,Property) ;

repStreamProperties(BioNaphtha,'CBOB','Products','Biofuels',Property) = StreamProp(BioNaphtha,Property) ;
repStreamProperties(BioNaphtha,'RBOB','Products','Biofuels',Property) = StreamProp(BioNaphtha,Property) ;
repStreamProperties(BioNaphtha,'CaRBOB','Products','Biofuels',Property) = StreamProp(BioNaphtha,Property) ;
repStreamProperties(BioDistillate,'ULSD','Products','Biofuels',Property) = StreamProp(BioDistillate,Property) ;
repStreamProperties(BioDistillate,'LowSulfurDiesel','Products','Biofuels',Property) = StreamProp(BioDistillate,Property) ;
repStreamProperties(BioDistillate,'CarbULSD','Products','Biofuels',Property) = StreamProp(BioDistillate,Property) ;
repStreamProperties(BioDistillate,'HeatingOil','Products','Biofuels',Property) = StreamProp(BioDistillate,Property) ;
repStreamProperties(BioDistillate,'JetFuel','Products','Biofuels',Property) = StreamProp(BioDistillate,Property) ;
repStreamProperties(EthStream,'Ethanol','Products','Biofuels',Property) = StreamProp(EthStream,Property) ;
repStreamProperties(Biosolids,'Chemicals','Products','Biofuels',Property) = StreamProp(Biosolids,Property) ;
repStreamProperties('GLYout','Chemicals','Products','Biofuels',Property) = StreamProp('GLYout',Property);

repStreamProperties('CBOB','Gasoline','Products','Blending',Property) = StreamProp('CBOB',Property) ;
repStreamProperties('RBOB','Gasoline','Products','Blending',Property) = StreamProp('RBOB',Property) ;
repStreamProperties('CaRBOB','Gasoline','Products','Blending',Property) = StreamProp('CaRBOB',Property) ;

repStreamProperties(Gasoline,'Gasoline','EndProducts','Blending',Property) = StreamProp(Gasoline,Property) ;
repStreamProperties(Distprod,'Distillate','EndProducts','Blending',Property) = StreamProp(Distprod,Property) ;
repStreamProperties(ResidFuel,'ResidFuel','EndProducts','Blending',Property) = StreamProp(ResidFuel,Property) ;

*Feedstock prices
repStreamPrices(RefReg,Crude,'Crude','Feedstocks','Petroleum',RunYears) =
    CurrentDeflator * sum(Period(PrcPeriod),(CrudeBalance.m(RefReg,Crude,Period)-CrudeBalance_M_DELTA(RefReg,Crude,Period))) ;
repStreamPrices(DomRefReg,UnfinishedOils,'UFOImports','Feedstocks','Petroleum',RunYears) =
    CurrentDeflator * sum(Period(PrcPeriod),StreamBalance.m(DomRefReg,'Coking',UnfinishedOils,Period)) ;
repStreamPrices(RefReg,'NGS','Gas','Feedstocks','Petroleum',RunYears) =
    CurrentDeflator * sum(Period(PrcPeriod),UtilBalance.m(RefReg,'Coking','NGS',Period)) / 6.287 * GasHeatContent('NGS') ;
repStreamPrices(RefReg,'KWH','Electricity','Feedstocks','Petroleum',RunYears) =
    CurrentDeflator * sum(Period(PrcPeriod),UtilBalance.m(RefReg,'Coking','KWH',Period)) ;
repStreamPrices(RefReg,RefInputStr,'Grains','Feedstocks','Biofuels',RunYears)$(GrainCrops(RefInputStr)) =
    CurrentDeflator * sum(Period(PrcPeriod),RefInpBalance.m(RefReg,'ETH_Ref',RefInputStr,Period)$GrainCrops(RefInputStr));
repStreamPrices(RefReg,BioStr,'Cellulose','Feedstocks','Biofuels',RunYears) =
    CurrentDeflator * sum(Period(PrcPeriod),BioRefRegBal.m(RefReg,BioStr,Period)) *
    repStreamProperties(BioStr,'Cellulose','Feedstocks','Biofuels','END') ;
repStreamPrices(RefReg,RefInputStr,'Oils','Feedstocks','Biofuels',RunYears)$(RenewableOils(RefInputStr)) =
    CurrentDeflator * sum(Period(PrcPeriod),RefInpBalance.m(RefReg,'Coking',RefInputStr,Period)$RenewableOils(RefInputStr));
repStreamPrices(RefReg,CoalStr,'Coal','Feedstocks','NonPetroleumFossil',RunYears) =
    CurrentDeflator * sum(Period(PrcPeriod),CoalRefRegBal.m(RefReg,CoalStr,Period)) *
    CoalHeatContent(CoalStr)*2000/1000000 ;
repStreamPrices(RefReg,'NGS','Gas','Feedstocks','NonPetroleumFossil',RunYears) =
    CurrentDeflator * sum(Period(PrcPeriod),UtilBalance.m(RefReg,'Coking','NGS',Period)) / 6.287 * GasHeatContent('NGS') / 1000 ;
repStreamPrices(DomRefReg,NGLInputStr,'DomesticNGPL','Feedstocks','NonPetroleumFossil',RunYears) =
    CurrentDeflator * sum(Period(PrcPeriod),RefInpBalance.m(DomRefReg,'Coking',NGLInputStr,Period)) ;

*Intermediate stream values
repStreamPrices(DomRefReg,EthStream,'Ethanol','Intermediates','BioFuels',RunYears) =
    CurrentDeflator * sum(Period(PrcPeriod),EthBalance.m(DomRefReg,EthStream,Period)) / 42 ;
repStreamPrices(DomRefReg,BioNaphtha,'Naphtha','Intermediates','Biofuels',RunYears) =
    CurrentDeflator * sum(Period(PrcPeriod),StreamBalance.m(DomRefReg,'Coking',BioNaphtha,Period)) / 42 ;
repStreamPrices(DomRefReg,BioDistillate,'Distillate','Intermediates','Biofuels',RunYears) =
    CurrentDeflator * sum(Period(PrcPeriod),StreamBalance.m(DomRefReg,'Coking',BioDistillate,Period)) / 42 ;
repStreamPrices(DomRefReg,NonFuelStreams,'NonFuelStreams','Intermediates','Biofuels',RunYears) =
    CurrentDeflator * sum(Period(PrcPeriod),StreamBalance.m(DomRefReg,'Coking',NonFuelStreams,Period)) ;
repStreamPrices(DomRefReg,NPFNaphtha,'Naphtha','Intermediates','NonPetroleumFossil',RunYears) =
    CurrentDeflator * sum(Period(PrcPeriod),StreamBalance.m(DomRefReg,'Coking',NPFNaphtha,Period)) / 42 ;
repStreamPrices(DomRefReg,NPFDistillate,'Distillate','Intermediates','NonPetroleumFossil',RunYears) =
    CurrentDeflator * sum(Period(PrcPeriod),StreamBalance.m(DomRefReg,'Coking',NPFDistillate,Period)) / 42 ;

*Product Prices
repStreamPrices(DomRefReg,GasSpecProd,'Gasoline','Products','Blending',RunYears) =
    CurrentDeflator * sum(Period(PrcPeriod),GasSpecBalance.m(DomRefReg,'Coking',GasSpecProd,Period)) / 42 ;
repStreamPrices(DomRefReg,DistSpecProd,'Distillate','Products','Blending',RunYears) =
    CurrentDeflator * sum(Period(PrcPeriod),DistSpecBalance.m(DomRefReg,'Coking',DistSpecProd,Period)) / 42 ;
repStreamPrices(DomRefReg,RecipeProd,'Chemicals','Products','Blending',RunYears) =
    CurrentDeflator * sum(Period(PrcPeriod),RecipeTransfer.m(DomRefReg,'Coking',RecipeProd,Period)) / 42 ;
repStreamPrices(DomRefReg,Coproduct,'NonFuelStreams','Products','Blending',RunYears) =
    CurrentDeflator * sum(Period(PrcPeriod),RecipeBPTransfer.m(DomRefReg,'Eth_Ref',Coproduct,Period)) ;

*End Product Prices
repStreamPrices(DomRefReg,GasProd,'Gasoline','EndProducts','Blending',RunYears) =
    CurrentDeflator * sum(Period(PrcPeriod),CombineSupply.m(DomRefReg,GasProd,Period)) / 42 ;
repStreamPrices(DomRefReg,DistProd,'Distillate','EndProducts','Blending',RunYears) =
    CurrentDeflator * sum(Period(PrcPeriod),CombineSupply.m(DomRefReg,DistProd,Period)) / 42 ;
repStreamPrices(DomRefReg,ResidFuel,'ResidFuel','EndProducts','Blending',RunYears) =
    CurrentDeflator * sum(Period(PrcPeriod),CombineSupply.m(DomRefReg,ResidFuel,Period)) / 42 ;

*Feedstock consumption
repStreamFlows(DomRefReg,Crude,'DomesticCrude','Feedstocks','Petroleum',RunYears) =
    sum(Period(PrcPeriod),
      npv_DomesticCrudeSup(DomRefReg,Crude,Period) ) / 1000 ;

repStreamFlows(DomRefReg,Crude,'CrudeImports','Feedstocks','Petroleum',RunYears) =
    sum((Source,Step,Period(PrcPeriod)),
        CRUDETRANS.l(DomRefReg,Crude,Source,Step,Period)) / 1000;

repStreamFlows(DomRefReg,UnfinishedOils,'UFOImports','Feedstocks','Petroleum',RunYears) =
    sum(Period(PrcPeriod),
        (IMPORTS.l(UnfinishedOils,DomRefReg,Period) - EXPORTS.l(UnfinishedOils,DomRefReg,Period)))/ 1000;

repStreamFlows(DomRefReg,Gas,'GasFeed','Feedstocks','Petroleum',RunYears) =
    -sum((RefType,Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(Gas,'H2P',ProcessMode) * PROCMODE.l(DomRefReg,RefType,'H2P',ProcessMode,Period) *
    6.287 / GasHeatContent(Gas));

repStreamFlows(DomRefReg,Gas,'GasFuel','Feedstocks','Petroleum',RunYears) =
    -sum((RefType,Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(Gas,'FUM',ProcessMode) * PROCMODE.l(DomRefReg,RefType,'FUM',ProcessMode,Period) *
    6.287 / GasHeatContent(Gas));

repStreamFlows(DomRefReg,'KWH','Electricity','Feedstocks','Petroleum',RunYears) =
    -sum((PetRefType,Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable('KWH',Process,ProcessMode) * PROCMODE.l(DomRefReg,PetRefType,Process,ProcessMode,Period)) ;

repStreamFlows(DomRefReg,GrainCrops,'Grains','Feedstocks','Biofuels',RunYears) =
    -sum((RefType,LCFS_BioProcess,LCFS_BioMode,Period(PrcPeriod)),
    ProcessTable(GrainCrops,LCFS_BioProcess,LCFS_BioMode) * PROCMODE.l(DomRefReg,RefType,LCFS_BioProcess,LCFS_BioMode,Period))/1000;

repStreamFlows(DomRefReg,BioStr,'Cellulose','Feedstocks','Biofuels',RunYears) =
    -sum((RefType,LCFS_BioProcess,LCFS_BioMode,Period(PrcPeriod)),
    ProcessTable(BioStr,LCFS_BioProcess,LCFS_BioMode) * PROCMODE.l(DomRefReg,RefType,LCFS_BioProcess,LCFS_BioMode,Period)) /
    repStreamProperties(BioStr,'Cellulose','Feedstocks','Biofuels','END') / 1000 ;

repStreamFlows(DomRefReg,RenewableOils,'Oils','Feedstocks','Biofuels',RunYears) =
    -sum((RefType,LCFS_BioProcess,LCFS_BioMode,Period(PrcPeriod)),
    ProcessTable(RenewableOils,LCFS_BioProcess,LCFS_BioMode) * PROCMODE.l(DomRefReg,RefType,LCFS_BioProcess,LCFS_BioMode,Period) /
    1000)*0.8 *62.4/7.48/2000*42;

repStreamFlows(DomRefReg,GrainCrops,'Grains','Feedstocks','Biofuels',RunYears) =
    -sum((RefType,CFP_BioProcess,CFP_BioMode,Period(PrcPeriod)),
    ProcessTable(GrainCrops,CFP_BioProcess,CFP_BioMode) * PROCMODE.l(DomRefReg,RefType,CFP_BioProcess,CFP_BioMode,Period))/1000;

repStreamFlows(DomRefReg,BioStr,'Cellulose','Feedstocks','Biofuels',RunYears) =
    -sum((RefType,CFP_BioProcess,CFP_BioMode,Period(PrcPeriod)),
    ProcessTable(BioStr,CFP_BioProcess,CFP_BioMode) * PROCMODE.l(DomRefReg,RefType,CFP_BioProcess,CFP_BioMode,Period)) /
    repStreamProperties(BioStr,'Cellulose','Feedstocks','Biofuels','END') / 1000 ;

repStreamFlows(DomRefReg,RenewableOils,'Oils','Feedstocks','Biofuels',RunYears) =
    -sum((RefType,CFP_BioProcess,CFP_BioMode,Period(PrcPeriod)),
    ProcessTable(RenewableOils,CFP_BioProcess,CFP_BioMode) * PROCMODE.l(DomRefReg,RefType,CFP_BioProcess,CFP_BioMode,Period) /
    1000)*0.8 *62.4/7.48/2000*42;

repStreamFlows(DomRefReg,'KWH','Electricity','Feedstocks','Biofuels',RunYears) =
    -sum((RefType,BioProcess,ProcessMode,Period(PrcPeriod))$(ProcProcMode(BioProcess,ProcessMode)),
    (ProcessTable('KWH',BioProcess,ProcessMode) * PROCMODE.l(DomRefReg,RefType,BioProcess,ProcessMode,Period))$ProcProcMode(BioProcess,ProcessMode));

* Coal (for CTL)
repStreamFlows(DomRefReg,CoalStr,'Coal','Feedstocks','NonPetroleumFossil',RunYears) =
    -sum((LCFS_BioMode,Period(PrcPeriod))$(ProcProcMode('CTL',LCFS_BioMode)),
    ProcessTable(CoalStr,'CTL',LCFS_BioMode) * PROCMODE.l(DomRefReg,'Coking','CTL',LCFS_BioMode,Period)$ProcProcMode('CTL',LCFS_BioMode) /
    (CoalHeatContent(CoalStr)*2000/1000000) / 1000);

* Coal (cont'd for CTLCCS)
repStreamFlows(DomRefReg,CoalStr,'Coal','Feedstocks','NonPetroleumFossil',RunYears) =
repStreamFlows(DomRefReg,CoalStr,'Coal','Feedstocks','NonPetroleumFossil',RunYears)
    -sum((LCFS_BioMode,Period(PrcPeriod))$(ProcProcMode('CTLCCS',LCFS_BioMode)),
    ProcessTable(CoalStr,'CTLCCS',LCFS_BioMode) * PROCMODE.l(DomRefReg,'Coking','CTLCCS',LCFS_BioMode,Period)$ProcProcMode('CTLCCS',LCFS_BioMode) /
    (CoalHeatContent(CoalStr)*2000/1000000) / 1000);

* Coal (cont'd for CBL)
repStreamFlows(DomRefReg,CoalStr,'Coal','Feedstocks','NonPetroleumFossil',RunYears) =
repStreamFlows(DomRefReg,CoalStr,'Coal','Feedstocks','NonPetroleumFossil',RunYears)
    -sum((LCFS_BioMode,Period(PrcPeriod))$(ProcProcMode('CBL',LCFS_BioMode)),
    ProcessTable(CoalStr,'CBL',LCFS_BioMode) * PROCMODE.l(DomRefReg,'Coking','CBL',LCFS_BioMode,Period)$ProcProcMode('CBL',LCFS_BioMode) /
    (CoalHeatContent(CoalStr)*2000/1000000) / 1000);

* Coal (cont'd for CBLCCS)
repStreamFlows(DomRefReg,CoalStr,'Coal','Feedstocks','NonPetroleumFossil',RunYears) =
repStreamFlows(DomRefReg,CoalStr,'Coal','Feedstocks','NonPetroleumFossil',RunYears)
    -sum((LCFS_BioMode,Period(PrcPeriod))$(ProcProcMode('CBLCCS',LCFS_BioMode)),
    ProcessTable(CoalStr,'CBLCCS',LCFS_BioMode) * PROCMODE.l(DomRefReg,'Coking','CBLCCS',LCFS_BioMode,Period)$ProcProcMode('CBLCCS',LCFS_BioMode) /
    (CoalHeatContent(CoalStr)*2000/1000000) / 1000);

repStreamFlows(DomRefReg,Gas,'GasFeed','Feedstocks','NonPetroleumFossil',RunYears) =
    -sum((LCFS_BioMode,Period(PrcPeriod))$ProcProcMode('GTL',LCFS_BioMode),
    ProcessTable(Gas,'GTL',LCFS_BioMode) * PROCMODE.l(DomRefReg,'Coking','GTL',LCFS_BioMode,Period)$ProcProcMode('GTL',LCFS_BioMode) * 6.287 /
    GasHeatContent(Gas)) ;

*Petroleum Intermediates
repStreamFlows(DomRefReg,LightGases,'LightGases','Intermediates','Petroleum',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(LightGases,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;

repStreamFlows(DomRefReg,NaphthaLightPet,'NaphthaLight','Intermediates','Petroleum',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(NaphthaLightPet,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;

repStreamFlows(DomRefReg,Alkylate,'Alkylate','Intermediates','Petroleum',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(Alkylate,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;

repStreamFlows(DomRefReg,ReformatePet,'Reformate','Intermediates','Petroleum',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(ReformatePet,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;

repStreamFlows(DomRefReg,NaphthaMedium,'NaphthaMedium','Intermediates','Petroleum',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(NaphthaMedium,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;

repStreamFlows(DomRefReg,NaphthaHeavyPet,'NaphthaHeavy','Intermediates','Petroleum',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(NaphthaHeavyPet,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;

repStreamFlows(DomRefReg,GasolineCat,'CatGasoline','Intermediates','Petroleum',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(GasolineCat,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;

repStreamFlows(DomRefReg,IsomeratePet,'Isomerate','Intermediates','Petroleum',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(IsomeratePet,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;

repStreamFlows(DomRefReg,GasolinePoly,'PolyGasoline','Intermediates','Petroleum',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(GasolinePoly,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;

repStreamFlows(DomRefReg,KerosenePet,'Kerosene','Intermediates','Petroleum',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(KerosenePet,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;

repStreamFlows(DomRefReg,DieselPet,'Diesel','Intermediates','Petroleum',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(DieselPet,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;

repStreamFlows(DomRefReg,DieselHeavy,'DieselHeavy','Intermediates','Petroleum',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(DieselHeavy,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;

repStreamFlows(DomRefReg,LightCycleOil,'LightCycleOil','Intermediates','Petroleum',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(LightCycleOil,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;

repStreamFlows(DomRefReg,GasOilLight,'GasOilLight','Intermediates','Petroleum',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(GasOilLight,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;

repStreamFlows(DomRefReg,GasOil,'GasOil','Intermediates','Petroleum',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(GasOil,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;

repStreamFlows(DomRefReg,GasOilHeavy,'GasOilHeavy','Intermediates','Petroleum',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(GasOilHeavy,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;

repStreamFlows(DomRefReg,ResidualOil,'Resid','Intermediates','Petroleum',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(ResidualOil,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;

*Nonpetroleum fossil intermediate streams
repStreamFlows(DomRefReg,NaphthaLightNPF,'NaphthaLight','Intermediates','NonPetroleumFossil',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(NaphthaLightNPF,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;
repStreamFlows(DomRefReg,NaphthaHeavyNPF,'NaphthaHeavy','Intermediates','NonPetroleumFossil',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(NaphthaHeavyNPF,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;
repStreamFlows(DomRefReg,ReformateNPF,'Reformate','Intermediates','NonPetroleumFossil',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(ReformateNPF,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;
repStreamFlows(DomRefReg,IsomerateNPF,'Isomerate','Intermediates','NonPetroleumFossil',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(IsomerateNPF,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;
repStreamFlows(DomRefReg,NaphthaHeavyBiofuels,'NaphthaHeavy','Intermediates','Biofuels',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(NaphthaHeavyBiofuels,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;
repStreamFlows(DomRefReg,ReformateBiofuels,'Reformate','Intermediates','Biofuels',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(ReformateBiofuels,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;
repStreamFlows(DomRefReg,IsomerateBiofuels,'Isomerate','Intermediates','Biofuels',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(IsomerateBiofuels,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;
repStreamFlows(DomRefReg,KeroseneNPF,'Kerosene','Intermediates','NonPetroleumFossil',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(KeroseneNPF,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;
repStreamFlows(DomRefReg,DieselNPF,'Diesel','Intermediates','NonPetroleumFossil',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(DieselNPF,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;

*Biofuels Intermediate Streams
repStreamFlows(DomRefReg,NaphthaLightBiofuels,'NaphthaLight','Intermediates','Biofuels',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(NaphthaLightBiofuels,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;
repStreamFlows(DomRefReg,KeroseneBiofuels,'Kerosene','Intermediates','Biofuels',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(KeroseneBiofuels,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;
repStreamFlows(DomRefReg,DieselBiofuels,'Diesel','Intermediates','Biofuels',RunYears) =
    sum((Process,ProcessMode,Period(PrcPeriod)),
    ProcessTable(DieselBiofuels,Process,ProcessMode) * PROCMODE.l(DomRefReg,'Coking',Process,ProcessMode,Period)) / 1000 ;
repStreamFlows(DomRefReg,EthStream,'Ethanol','Intermediates','Biofuels',RunYears) =
    sum((RefType,LCFS_BioProcess,LCFS_BioMode,Period(PrcPeriod)),
        ProcessTable(EthStream,LCFS_BioProcess,LCFS_BioMode) * PROCMODE.l(DomRefReg,RefType,LCFS_BioProcess,LCFS_BioMode,Period));
repStreamFlows(DomRefReg,BioNaphtha,'Naphtha','Intermediates','Biofuels',RunYears) =
    sum((RefType,LCFS_BioProcess,LCFS_BioMode,Period(PrcPeriod)),
    ProcessTable(BioNaphtha,LCFS_BioProcess,LCFS_BioMode) * PROCMODE.l(DomRefReg,RefType,LCFS_BioProcess,LCFS_BioMode,Period));
repStreamFlows(DomRefReg,BioDistillate,'Distillate','Intermediates','Biofuels',RunYears) =
    sum((RefType,LCFS_BioProcess,LCFS_BioMode,Period(PrcPeriod)),
    ProcessTable(BioDistillate,LCFS_BioProcess,LCFS_BioMode) * PROCMODE.l(DomRefReg,RefType,LCFS_BioProcess,LCFS_BioMode,Period));
repStreamFlows(DomRefReg,NPFNaphtha,'Naphtha','Intermediates','NonPetroleumFossil',RunYears) =
    sum((RefType,LCFS_BioProcess,LCFS_BioMode,Period(PrcPeriod)),
    ProcessTable(NPFNaphtha,LCFS_BioProcess,LCFS_BioMode) * PROCMODE.l(DomRefReg,RefType,LCFS_BioProcess,LCFS_BioMode,Period));
repStreamFlows(DomRefReg,NPFDistillate,'Distillate','Intermediates','NonPetroleumFossil',RunYears) =
    sum((RefType,LCFS_BioProcess,LCFS_BioMode,Period(PrcPeriod)),
    ProcessTable(NPFDistillate,LCFS_BioProcess,LCFS_BioMode) * PROCMODE.l(DomRefReg,RefType,LCFS_BioProcess,LCFS_BioMode,Period));
repStreamFlows(DomRefReg,EthStream,'Ethanol','Intermediates','Biofuels',RunYears) =
    sum((RefType,CFP_BioProcess,CFP_BioMode,Period(PrcPeriod)),
        ProcessTable(EthStream,CFP_BioProcess,CFP_BioMode) * PROCMODE.l(DomRefReg,RefType,CFP_BioProcess,CFP_BioMode,Period));
repStreamFlows(DomRefReg,BioNaphtha,'Naphtha','Intermediates','Biofuels',RunYears) =
    sum((RefType,CFP_BioProcess,CFP_BioMode,Period(PrcPeriod)),
    ProcessTable(BioNaphtha,CFP_BioProcess,CFP_BioMode) * PROCMODE.l(DomRefReg,RefType,CFP_BioProcess,CFP_BioMode,Period));
repStreamFlows(DomRefReg,BioDistillate,'Distillate','Intermediates','Biofuels',RunYears) =
    sum((RefType,CFP_BioProcess,CFP_BioMode,Period(PrcPeriod)),
    ProcessTable(BioDistillate,CFP_BioProcess,CFP_BioMode) * PROCMODE.l(DomRefReg,RefType,CFP_BioProcess,CFP_BioMode,Period));
repStreamFlows(DomRefReg,NPFNaphtha,'Naphtha','Intermediates','NonPetroleumFossil',RunYears) =
    sum((RefType,CFP_BioProcess,CFP_BioMode,Period(PrcPeriod)),
    ProcessTable(NPFNaphtha,CFP_BioProcess,CFP_BioMode) * PROCMODE.l(DomRefReg,RefType,CFP_BioProcess,CFP_BioMode,Period));
repStreamFlows(DomRefReg,NPFDistillate,'Distillate','Intermediates','NonPetroleumFossil',RunYears) =
    sum((RefType,CFP_BioProcess,CFP_BioMode,Period(PrcPeriod)),
    ProcessTable(NPFDistillate,CFP_BioProcess,CFP_BioMode) * PROCMODE.l(DomRefReg,RefType,CFP_BioProcess,CFP_BioMode,Period));

*Petroleum Product Streams
repStreamFlows(DomRefReg,Gasoline,'GasolineImports','Products','Petroleum',RunYears) =
    sum(Period(PrcPeriod),
        IMPORTS.l(Gasoline,DomRefReg,Period)) / 1000 +
    sum((TranMode,Period(PrcPeriod)),
        RefRefTRAN.l('9_RefReg',DomRefReg,TranMode,Gasoline,Period)) / 1000;
repStreamFlows(DomRefReg,Gasoline,'GasolineExports','Products','Petroleum',RunYears) =
    sum(Period(PrcPeriod),
        EXPORTS.l(Gasoline,DomRefReg,Period)) / 1000 ;
repStreamFlows(DomRefReg,CRBOB,'BOBImports','Products','Petroleum',RunYears) =
    sum(Period(PrcPeriod),
        IMPORTS.l(CRBOB,DomRefReg,Period)) / 1000 +
    sum((TranMode,Period(PrcPeriod)),
        RefRefTRAN.l('9_RefReg',DomRefReg,TranMode,CRBOB,Period)) / 1000 ;
repStreamFlows(DomRefReg,CRBOB,'BOBExports','Products','Petroleum',RunYears) =
    sum(Period(PrcPeriod),EXPORTS.l(CRBOB,DomRefReg,Period))/1000;
repStreamFlows(DomRefReg,DistProd,'DistillateImports','Products','Petroleum',RunYears) =
    sum(Period(PrcPeriod),
        IMPORTS.l(DistProd,DomRefReg,Period))/ 1000 +
    sum((TranMode,Period(PrcPeriod)),
        RefRefTRAN.l('9_RefReg',DomRefReg,TranMode,DistProd,Period)) / 1000;
repStreamFlows(DomRefReg,DistProd,'DistillateExports','Products','Petroleum',RunYears) =
    sum(Period(PrcPeriod),EXPORTS.l(DistProd,DomRefReg,Period))/1000 ;
repStreamFlows(DomRefReg,ResidFuel,'ResidImports','Products','Petroleum',RunYears) =
    sum(Period(PrcPeriod),
        IMPORTS.l(ResidFuel, DomRefReg,Period))/ 1000 +
    sum((TranMode,Period(PrcPeriod)),
        RefRefTRAN.l('9_RefReg',DomRefReg,TranMode,ResidFuel,Period)) / 1000;
repStreamFlows(DomRefReg,ResidFuel,'ResidExports','Products','Petroleum',RunYears) =
    sum(Period(PrcPeriod),
        EXPORTS.l(ResidFuel,DomRefReg,Period))/ 1000 ;
repStreamFlows(DomRefReg,PetNaphtha,'CBOB','Products','Petroleum',RunYears) =
    sum((RefType,Period(PrcPeriod)),
    ToSpecBlend.l(DomRefReg,RefType,'CBOB',PetNaphtha,Period)) / 1000 ;
repStreamFlows(DomRefReg,PetNaphtha,'RBOB','Products','Petroleum',RunYears) =
    sum((RefType,Period(PrcPeriod)),
    ToSpecBlend.l(DomRefReg,RefType,'RBOB',PetNaphtha,Period)) / 1000 ;
repStreamFlows(DomRefReg,PetNaphtha,'CaRBOB','Products','Petroleum',RunYears) =
    sum((RefType,Period(PrcPeriod)),
    ToSpecBlend.l(DomRefReg,RefType,'CaRBOB',PetNaphtha,Period)) / 1000 ;
repStreamFlows(DomRefReg,PetDistillate,'ULSD','Products','Petroleum',RunYears) =
    sum((RefType,Period(PrcPeriod)),ToSpecBlend.l(DomRefReg,RefType,'DSU',PetDistillate,Period)) / 1000;
repStreamFlows(DomRefReg,PetDistillate,'LowSulfurDiesel','Products','Petroleum',RunYears) =
    sum((RefType,Period(PrcPeriod)),ToSpecBlend.l(DomRefReg,RefType,'DSL',PetDistillate,Period)) / 1000;
repStreamFlows(DomRefReg,PetDistillate,'CARBULSD','Products','Petroleum',RunYears) =
    sum((RefType,Period(PrcPeriod)),ToSpecBlend.l(DomRefReg,RefType,'CarbDSU',PetDistillate,Period)) / 1000;
repStreamFlows(DomRefReg,PetDistillate,'HeatingOil','Products','Petroleum',RunYears) =
    sum((RefType,Period(PrcPeriod)),ToSpecBlend.l(DomRefReg,RefType,'N2H',PetDistillate,Period)) / 1000;
repStreamFlows(DomRefReg,PetDistillate,'JetFuel','Products','Petroleum',RunYears) =
    sum((RefType,Period(PrcPeriod)),ToSpecBlend.l(DomRefReg,RefType,'JTA',PetDistillate,Period)) / 1000;
repStreamFlows(DomRefReg,RefinedProduct,'Chemicals','Products','Petroleum',RunYears) =
    sum((RefType,Period(PrcPeriod),PetChemBlends),
        RECIPEMODE.l(DomRefReg,RefType,PetChemBlends,Period) * RecipeBlending(PetChemBlends,RefinedProduct)) / 1000 ;

*Nonpetroleum fossil product streams

*   MM bbl/cd
repStreamFlows(DomRefReg,NGLInputStr,'DomesticNGPL','Feedstocks','NonPetroleumFossil',RunYears) =
    sum(Period(PrcPeriod), npv_NGLBounds(NGLInputStr,DomRefReg,Period) ) /1000 ;


repStreamFlows(DomRefReg,NGLProduct,'NGPLImports','Feedstocks','NonPetroleumFossil',RunYears) =
    sum((Period(PrcPeriod),CenDiv),
        NGLIMPORTS.l(NGLProduct,CenDiv,Period) * CDToRefMap(DomRefReg,CenDiv)) / 1000;
repStreamFlows(DomRefReg,NGLProduct,'NGLExports','Feedstocks','NonPetroleumFossil',RunYears) =
    sum((Period(PrcPeriod),CenDiv),
        NGLEXPORTS.l(NGLProduct,CenDiv,Period) * CDToRefMap(DomRefReg,CenDiv)) / 1000;
repStreamFlows(DomRefReg,NPFNaphtha,'CBOB','Products','NonPetroleumFossil',RunYears) =
    sum((RefType,Period(PrcPeriod)),
    ToSpecBlend.l(DomRefReg,RefType,'CBOB',NPFNaphtha,Period)) / 1000 ;
repStreamFlows(DomRefReg,NPFNaphtha,'RBOB','Products','NonPetroleumFossil',RunYears) =
    sum((RefType,Period(PrcPeriod)),
    ToSpecBlend.l(DomRefReg,RefType,'RBOB',NPFNaphtha,Period)) / 1000 ;
repStreamFlows(DomRefReg,NPFNaphtha,'CaRBOB','Products','NonPetroleumFossil',RunYears) =
    sum((RefType,Period(PrcPeriod)),
    ToSpecBlend.l(DomRefReg,RefType,'CaRBOB',NPFNaphtha,Period)) / 1000 ;
repStreamFlows(DomRefReg,NPFDistillate,'ULSD','Products','NonPetroleumFossil',RunYears) =
    sum((RefType,Period(PrcPeriod)),ToSpecBlend.l(DomRefReg,RefType,'DSU',NPFDistillate,Period)) / 1000;
repStreamFlows(DomRefReg,NPFDistillate,'LowSulfurDiesel','Products','NonPetroleumFossil',RunYears) =
    sum((RefType,Period(PrcPeriod)),ToSpecBlend.l(DomRefReg,RefType,'DSL',NPFDistillate,Period)) / 1000;
repStreamFlows(DomRefReg,NPFDistillate,'CARBULSD','Products','NonPetroleumFossil',RunYears) =
    sum((RefType,Period(PrcPeriod)),ToSpecBlend.l(DomRefReg,RefType,'CarbDSU',NPFDistillate,Period)) / 1000;
repStreamFlows(DomRefReg,NPFDistillate,'HeatingOil','Products','NonPetroleumFossil',RunYears) =
    sum((RefType,Period(PrcPeriod)),ToSpecBlend.l(DomRefReg,RefType,'N2H',NPFDistillate,Period)) / 1000;
repStreamFlows(DomRefReg,NPFDistillate,'JetFuel','Products','NonPetroleumFossil',RunYears) =
    sum((RefType,Period(PrcPeriod)),ToSpecBlend.l(DomRefReg,RefType,'JTA',NPFDistillate,Period)) / 1000;
repStreamFlows(DomRefReg,RefinedProduct,'Chemicals','Products','NonPetroleumFossil',RunYears) =
    sum((RefType,Period(PrcPeriod),NPFChemBlends),
        RECIPEMODE.l(DomRefReg,RefType,NPFChemBlends,Period) * RecipeBlending(NPFChemBlends,RefinedProduct)) / 1000 ;

*Biofuel product streams
repStreamFlows(DomRefReg,EthStream,'EthanolImports','Products','Biofuels',RunYears) =
    sum(Period(PrcPeriod),ETHIMP.l(DomRefReg,EthStream,Period)) / 1000;
repStreamFlows(DomRefReg,EthStream,'EthanolExports','Products','Biofuels',RunYears) =
    sum(Period(PrcPeriod),ETHEXP.l(DomRefReg,EthStream,Period)) / 1000 ;
repStreamFlows(DomRefReg,Chemicals,'ChemicalImports','Products','Blending',RunYears) =
    sum(Period(PrcPeriod),IMPORTS.l(Chemicals,DomRefReg,Period)) / 1000 +
    sum((TranMode,Period(PrcPeriod)),
        RefRefTRAN.l('9_RefReg',DomRefReg,TranMode,Chemicals,Period)) / 1000;
repStreamFlows(DomRefReg,Chemicals,'ChemicalExports','Products','Blending',RunYears) =
    sum(Period(PrcPeriod),EXPORTS.l(Chemicals,DomRefReg,Period)) / 1000;
repStreamFlows(DomRefReg,BioNaphtha,'CBOB','Products','Biofuels',RunYears) =
    sum((RefType,Period(PrcPeriod)),
    ToSpecBlend.l(DomRefReg,RefType,'CBOB',BioNaphtha,Period)) / 1000 ;
repStreamFlows(DomRefReg,BioNaphtha,'RBOB','Products','Biofuels',RunYears) =
    sum((RefType,Period(PrcPeriod)),
    ToSpecBlend.l(DomRefReg,RefType,'RBOB',BioNaphtha,Period)) / 1000 ;
repStreamFlows(DomRefReg,BioNaphtha,'CaRBOB','Products','Biofuels',RunYears) =
    sum((RefType,Period(PrcPeriod)),
    ToSpecBlend.l(DomRefReg,RefType,'CaRBOB',BioNaphtha,Period)) / 1000 ;
repStreamFlows(DomRefReg,BioDistillate,'ULSD','Products','Biofuels',RunYears) =
    sum((RefType,Period(PrcPeriod)),ToSpecBlend.l(DomRefReg,RefType,'DSU',BioDistillate,Period)) / 1000 +

    sum((RefType,Period(PrcPeriod)),
         ToRecipeBlend.l(DomRefReg,RefType,'RCP_DSU2',BioDistillate,Period)) / 1000;
*remove  ToRecipeBlend.l(DomRefReg,RefType,'RCP_DSU2',BioDistillate,Period)$BioDistillate(RenewDiesel)) / 1000;

repStreamFlows(DomRefReg,BioDistillate,'LowSulfurDiesel','Products','Biofuels',RunYears) =
    sum((RefType,Period(PrcPeriod)),ToSpecBlend.l(DomRefReg,RefType,'DSL',BioDistillate,Period)) / 1000;
repStreamFlows(DomRefReg,BioDistillate,'CARBULSD','Products','Biofuels',RunYears) =
    sum((RefType,Period(PrcPeriod)),ToSpecBlend.l(DomRefReg,RefType,'CarbDSU',BioDistillate,Period)) / 1000 +

    sum((RefType,stream(RenewDieselStr),Period(PrcPeriod)),
         ToRecipeBlend.l(DomRefReg,RefType,'RCP_carbDSU2',BioDistillate,Period)) / 1000;
*remove  ToRecipeBlend.l(DomRefReg,RefType,'RCP_carbDSU2',BioDistillate,Period)$BioDistillate(RenewDiesel)) / 1000;

repStreamFlows(DomRefReg,BioDistillate,'HeatingOil','Products','Biofuels',RunYears) =
    sum((RefType,Period(PrcPeriod)),ToSpecBlend.l(DomRefReg,RefType,'N2H',BioDistillate,Period)) / 1000;
repStreamFlows(DomRefReg,BioDistillate,'JetFuel','Products','Biofuels',RunYears) =
    sum((RefType,Period(PrcPeriod)),ToSpecBlend.l(DomRefReg,RefType,'JTA',BioDistillate,Period)) / 1000;
repStreamFlows(DomRefReg,RefinedProduct,'Chemicals','Products','Biofuels',RunYears) =
    sum((RefType,Period(PrcPeriod),BiochemBlends),
        RECIPEMODE.l(DomRefReg,RefType,BioChemBlends,Period) * RecipeBlending(BioChemBlends,RefinedProduct)) / 1000 ;

*End Product Streams
repStreamFlows(DomRefReg,EthStream,'E10CFGEthanol','EndProducts','Biofuels',RunYears) =
    sum((RefType,Period(PrcPeriod),E10CFGBlends),
    ToRecipeBlend.l(DomRefReg,RefType,E10CFGBlends,EthStream,Period)) / 1000;
repStreamFlows(DomRefReg,EthStream,'E10RFGEthanol','EndProducts','Biofuels',RunYears) =
    sum((RefType,Period(PrcPeriod),E10RFGBlends),
    ToRecipeBlend.l(DomRefReg,RefType,E10RFGBlends,EthStream,Period)) / 1000;
repStreamFlows(DomRefReg,EthStream,'E10CarbRFGEthanol','EndProducts','Biofuels',RunYears) =
    sum((RefType,Period(PrcPeriod),E10CarbRFGBlends),
    ToRecipeBlend.l(DomRefReg,RefType,E10CarbRFGBlends,EthStream,Period)) / 1000;
repStreamFlows(DomRefReg,EthStream,'E15CFGEthanol','EndProducts','Biofuels',RunYears) =
    sum((RefType,Period(PrcPeriod),E15CFGBlends),
    ToRecipeBlend.l(DomRefReg,RefType,E15CFGBlends,EthStream,Period)) / 1000;
repStreamFlows(DomRefReg,EthStream,'E15RFGEthanol','EndProducts','Biofuels',RunYears) =
    sum((RefType,Period(PrcPeriod),E15RFGBlends),
    ToRecipeBlend.l(DomRefReg,RefType,E15RFGBlends,EthStream,Period)) / 1000;
repStreamFlows(DomRefReg,EthStream,'E85Ethanol','EndProducts','Biofuels',RunYears) =
    sum((RefType,Period(PrcPeriod),E85Blends),
    ToRecipeBlend.l(DomRefReg,RefType,E85Blends,EthStream,Period)) / 1000;
repStreamFlows(DomRefReg,BioDistillate,'B20Dist','EndProducts','Biofuels',RunYears) =
    sum((RefType,Period(PrcPeriod),B20Blends),ToRecipeBlend.l(DomRefReg,RefType,B20Blends,BioDistillate,Period)) / 1000;
repStreamFlows(DomRefReg,BiodieselStr,'B20DSU','EndProducts','Biofuels',RunYears) =
    sum((RefType,Period(PrcPeriod)),ToRecipeBlend.l(DomRefReg,RefType,'RCP_DSU1',BiodieselStr,Period)) / 1000;
repStreamFlows(DomRefReg,BioDistillate,'B20DSL','EndProducts','Biofuels',RunYears) =
    sum((RefType,Period(PrcPeriod)),ToRecipeBlend.l(DomRefReg,RefType,'RCP_DSL1',BioDistillate,Period)) / 1000;
repStreamFlows(DomRefReg,BioDistillate,'B20CarbDSU','EndProducts','Biofuels',RunYears) =
    sum((RefType,Period(PrcPeriod)),ToRecipeBlend.l(DomRefReg,RefType,'RCP_CarbDSU1',BioDistillate,Period)) / 1000;
repStreamFlows(DomRefReg,BioDistillate,'B20N2H','EndProducts','Biofuels',RunYears) =
    sum((RefType,Period(PrcPeriod)),ToRecipeBlend.l(DomRefReg,RefType,'RCP_N2H1',BioDistillate,Period)) / 1000;
repStreamFlows(DomRefReg,GasSpecProd,'E10BOB','EndProducts','Blending',RunYears) =
    sum((RefType,Period(PrcPeriod),E10Blends),ToRecipeBlend.l(DomRefReg,RefType,E10Blends,GasSpecProd,Period)) / 1000 ;
repStreamFlows(DomRefReg,GasSpecProd,'E15BOB','EndProducts','Blending',RunYears) =
    sum((RefType,Period(PrcPeriod),E15Blends),ToRecipeBlend.l(DomRefReg,RefType,E15Blends,GasSpecProd,Period)) / 1000 ;
repStreamFlows(DomRefReg,GasSpecProd,'E85BOB','EndProducts','Blending',RunYears) =
    sum((RefType,Period(PrcPeriod),E85Blends),ToRecipeBlend.l(DomRefReg,RefType,E85Blends,GasSpecProd,Period)) / 1000 ;
repStreamFlows(DomRefReg,DistSpecProd,'NonFAMEDist','EndProducts','Blending',RunYears) =
    sum((RefType,Period(PrcPeriod),NonFAMEBlends),ToRecipeBlend.l(DomRefReg,RefType,NonFAMEBlends,DistSpecProd,Period)) / 1000 ;
repStreamFlows(DomRefReg,DistSpecProd,'B20Dist','EndProducts','Blending',RunYears) =
    sum((RefType,Period(PrcPeriod),B20Blends),ToRecipeBlend.l(DomRefReg,RefType,B20Blends,DistSpecProd,Period)) / 1000 ;
repStreamFlows(DomRefReg,GasProd,'Gasoline','EndProducts','Blending',RunYears) =
    sum(Period(PrcPeriod),TotProd.l(DomRefReg,GasProd,Period)) / 1000 ;
repStreamFlows(DomRefReg,DistProd,'Distillate','EndProducts','Blending',RunYears) =
    sum(Period(PrcPeriod),TotProd.l(DomRefReg,DistProd,Period)) / 1000 ;
repStreamFlows(DomRefReg,ResidFuel,'ResidFuel','EndProducts','Blending',RunYears) =
    sum(Period(PrcPeriod),TotProd.l(DomRefReg,ResidFuel,Period)) / 1000;
repStreamFlows(DomRefReg,RecipeProd,'Chemicals','EndProducts','Blending',RunYears) =
    sum(Period(PrcPeriod),TotProd.l(DomRefReg,RecipeProd,Period)) / 1000 ;

*--------------------------------------------------------------------------------------------------------
*End LFMM_mac.gdx reporting parameters

*FTAB Table 21 global variable fill
*----------------------------------------------------------------------------------------------------------

*em4?? If(NCNTRL_CURCALYR>2010,

loop((t,MNUMYR)$(tMNUM(t,MNUMYR) And CurrentYr(t)),

*Volume units
LFMMOUT_FEEDSTOCKS('1_M4','01_M10',MNUMYR) =
    sum((DomRefReg,Crude),repStreamFlows(DomRefReg,Crude,'DomesticCrude','Feedstocks','Petroleum',t) *
        TMNUM(t,MNUMYR)) ;
LFMMOUT_FEEDSTOCKS('1_M4','02_M10',MNUMYR) =
    sum((DomRefReg,Crude,Industry),repStreamFlows(DomRefReg,Crude,'CrudeImports','Feedstocks',Industry,t) *
        TMNUM(t,MNUMYR)) ;
LFMMOUT_FEEDSTOCKS('1_M4','03_M10',MNUMYR) =
    sum((DomRefReg,UnfinishedOils,Industry),repStreamFlows(DomRefReg,UnfinishedOils,'UFOImports','Feedstocks',Industry,t) *
        TMNUM(t,MNUMYR)) ;
LFMMOUT_FEEDSTOCKS('1_M4','04_M10',MNUMYR) =
    sum((DomRefReg,NGLInputStr,Industry),repStreamFlows(DomRefReg,NGLInputStr,'DomesticNGPL','Feedstocks',Industry,t) *
        TMNUM(t,MNUMYR)) ;

* note:  NGL imports already reported as product imports, so set to 0 here
LFMMOUT_FEEDSTOCKS('1_M4','05_M10',MNUMYR) = 0 ;
LFMMOUT_FEEDSTOCKS('1_M4','06_M10',MNUMYR) =
    sum((DomRefReg,Gas,Industry),(repStreamFlows(DomRefReg,Gas,'GasFeed','Feedstocks',Industry,t)) *
        TMNUM(t,MNUMYR)) / 1000 * 365 / 50 ;
LFMMOUT_FEEDSTOCKS('1_M4','07_M10',MNUMYR) =
    sum((DomRefReg,Coal,Industry),repStreamFlows(DomRefReg,Coal,'Coal','Feedstocks',Industry,t) *
        TMNUM(t,MNUMYR)) * 2000 / (0.3 * 62.4) * 365 / 1000 ;
LFMMOUT_FEEDSTOCKS('1_M4','08_M10',MNUMYR) =
    sum((DomRefReg,GrainCrops,Industry),repStreamFlows(DomRefReg,GrainCrops,'Grains','Feedstocks',Industry,t) *
        TMNUM(t,MNUMYR)) / 0.803563913 * 365 / 1000 ;
LFMMOUT_FEEDSTOCKS('1_M4','09_M10',MNUMYR) =
    sum((DomRefReg,BioStr,Industry),repStreamFlows(DomRefReg,BioStr,'Cellulose','Feedstocks',Industry,t) /
        (repStreamProperties(BioStr,'Cellulose','Feedstocks','Biofuels','SPG') * 62.4 / 2000) *
        TMNUM(t,MNUMYR)) * 365 / 1000 ;
LFMMOUT_FEEDSTOCKS('1_M4','10_M10',MNUMYR) =
    sum((DomRefReg,RenewableOils,Industry),repStreamFlows(DomRefReg,RenewableOils,'Oils','Feedstocks',Industry,t) *
        TMNUM(t,MNUMYR)) * 2000 / 7.42 / 7.48 * 365 / 1000 ;


LFMMOUT_INTERMEDIATE('1_M4','01_M13',MNUMYR) =
    sum((DomRefReg,IntStream),(repStreamFlows(DomRefReg,IntStream,'CBOB','Products','Petroleum',t) +
        repStreamFlows(DomRefReg,IntStream,'RBOB','Products','Petroleum',t) +
        repStreamFlows(DomRefReg,IntStream,'CaRBOB','Products','Petroleum',t)) * TMNUM(t,MNUMYR)) ;
LFMMOUT_INTERMEDIATE('1_M4','02_M13',MNUMYR) =
    sum((DomRefReg,IntStream),(repStreamFlows(DomRefReg,IntStream,'CBOB','Products','NonPetroleumFossil',t) +
        repStreamFlows(DomRefReg,IntStream,'RBOB','Products','NonPetroleumFossil',t) +
        repStreamFlows(DomRefReg,IntStream,'CaRBOB','Products','NonPetroleumFossil',t)) * TMNUM(t,MNUMYR)) ;
LFMMOUT_INTERMEDIATE('1_M4','03_M13',MNUMYR) =
    sum((DomRefReg,IntStream),(repStreamFlows(DomRefReg,IntStream,'CBOB','Products','Biofuels',t) +
        repStreamFlows(DomRefReg,IntStream,'RBOB','Products','Biofuels',t) +
        repStreamFlows(DomRefReg,IntStream,'CaRBOB','Products','Biofuels',t)) * TMNUM(t,MNUMYR)) ;
LFMMOUT_INTERMEDIATE('1_M4','04_M13',MNUMYR) =
    sum((DomRefReg,EthStream),repStreamFlows(DomRefReg,EthStream,'Ethanol','Intermediates','Biofuels',t) / 1000 *
    TMNUM(t,MNUMYR)) * 1.0249 ;
LFMMOUT_INTERMEDIATE('1_M4','05_M13',MNUMYR) =
    sum((DomRefReg,IntStream),(repStreamFlows(DomRefReg,IntStream,'ULSD','Products','Petroleum',t) +
        repStreamFlows(DomRefReg,IntStream,'LowSulfurDiesel','Products','Petroleum',t) +
        repStreamFlows(DomRefReg,IntStream,'CarbULSD','Products','Petroleum',t) +
        repStreamFlows(DomRefReg,IntStream,'JetFuel','Products','Petroleum',t) +
        repStreamFlows(DomRefReg,IntStream,'HeatingOil','Products','Petroleum',t)) * TMNUM(t,MNUMYR)) ;
LFMMOUT_INTERMEDIATE('1_M4','06_M13',MNUMYR) =
    sum((DomRefReg,IntStream),(repStreamFlows(DomRefReg,IntStream,'ULSD','Products','NonPetroleumFossil',t) +
        repStreamFlows(DomRefReg,IntStream,'LowSulfurDiesel','Products','NonPetroleumFossil',t) +
        repStreamFlows(DomRefReg,IntStream,'CarbULSD','Products','NonPetroleumFossil',t) +
        repStreamFlows(DomRefReg,IntStream,'JetFuel','Products','NonPetroleumFossil',t) +
        repStreamFlows(DomRefReg,IntStream,'HeatingOil','Products','NonPetroleumFossil',t)) * TMNUM(t,MNUMYR)) ;
LFMMOUT_INTERMEDIATE('1_M4','07_M13',MNUMYR) =
    sum((DomRefReg,IntStream),(repStreamFlows(DomRefReg,IntStream,'ULSD','Products','Biofuels',t) +
        repStreamFlows(DomRefReg,IntStream,'LowSulfurDiesel','Products','Biofuels',t) +
        repStreamFlows(DomRefReg,IntStream,'CarbULSD','Products','Biofuels',t) +
        repStreamFlows(DomRefReg,IntStream,'JetFuel','Products','Biofuels',t) +
        repStreamFlows(DomRefReg,IntStream,'HeatingOil','Products','Biofuels',t)) * TMNUM(t,MNUMYR)) ;
LFMMOUT_INTERMEDIATE('1_M4','08_M13',MNUMYR) =
    sum((DomRefReg,IntStream),repStreamFlows(DomRefReg,IntStream,'Resid','Intermediates','Petroleum',t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_INTERMEDIATE('1_M4','09_M13',MNUMYR) =
    sum((DomRefReg,LiquidChem),repStreamFlows(DomRefReg,LiquidChem,'Chemicals','Products','Petroleum',t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_INTERMEDIATE('1_M4','10_M13',MNUMYR) =
    sum((DomRefReg,LiquidChem),repStreamFlows(DomRefReg,LiquidChem,'Chemicals','Products','NonPetroleumFossil',t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_INTERMEDIATE('1_M4','11_M13',MNUMYR) =
    sum((DomRefReg,LiquidChem),repStreamFlows(DomRefReg,LiquidChem,'Chemicals','Products','Biofuels',t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_INTERMEDIATE('1_M4','12_M13',MNUMYR) =
    sum((DomRefReg,Solids),repStreamFlows(DomRefReg,Solids,'Chemicals','Products','Petroleum',t) *
    TMNUM(t,MNUMYR))* MMBPD_TO_BCF ;
LFMMOUT_INTERMEDIATE('1_M4','13_M13',MNUMYR) =
    sum((DomRefReg,BioSolids),(repStreamFlows(DomRefReg,BioSolids,'Chemicals','Products','Biofuels',t)) *
    TMNUM(t,MNUMYR)) / (35/2000) * 365 / 1000 ;


*  1000 bbl/cd
LFMMOUT_RFPRDDIESEL(MNUMCR,MNUMYR) = 1000. *
    sum((DomRefReg,IntStream,Industry,ActiveDem)$crN2L(ActiveDem,MNUMCR),
        RefReg_to_CenDiv_ACU_Frac(DomRefReg,ActiveDem)*(
       (repStreamFlows(DomRefReg,IntStream,'ULSD','Products',Industry,t) +
        repStreamFlows(DomRefReg,IntStream,'LowSulfurDiesel','Products',Industry,t) +
        repStreamFlows(DomRefReg,IntStream,'CarbULSD','Products',Industry,t)) * TMNUM(t,MNUMYR))) ;

LFMMOUT_RFPRDDIESEL('11_United_States',MNUMYR) =
  sum(MNUMCR$(ord(MNUMCR)<10), LFMMOUT_RFPRDDIESEL(MNUMCR,MNUMYR) ) ;



LFMMOUT_REFINE_PROD('1_M4','01_M11',MNUMYR) =
    LFMMOUT_INTERMEDIATE('1_M4','01_M13',MNUMYR) +
    LFMMOUT_INTERMEDIATE('1_M4','02_M13',MNUMYR) +
    LFMMOUT_INTERMEDIATE('1_M4','03_M13',MNUMYR) ;
LFMMOUT_REFINE_PROD('1_M4','02_M11',MNUMYR) =
    LFMMOUT_INTERMEDIATE('1_M4','04_M13',MNUMYR) ;
LFMMOUT_REFINE_PROD('1_M4','03_M11',MNUMYR) =
    sum((DomRefReg,IntStream,Industry),repStreamFlows(DomRefReg,IntStream,'JetFuel','Products',Industry,t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_REFINE_PROD('1_M4','04_M11',MNUMYR) =
    sum((DomRefReg,IntStream,Industry),
       (repStreamFlows(DomRefReg,IntStream,'ULSD','Products',Industry,t) +
        repStreamFlows(DomRefReg,IntStream,'LowSulfurDiesel','Products',Industry,t) +
        repStreamFlows(DomRefReg,IntStream,'CarbULSD','Products',Industry,t)) * TMNUM(t,MNUMYR)) ;
LFMMOUT_REFINE_PROD('1_M4','05_M11',MNUMYR) =
    sum((DomRefReg,IntStream,Industry),repStreamFlows(DomRefReg,IntStream,'HeatingOil','Products',Industry,t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_REFINE_PROD('1_M4','06_M11',MNUMYR) =
    LFMMOUT_INTERMEDIATE('1_M4','08_M13',MNUMYR) ;
LFMMOUT_REFINE_PROD('1_M4','07_M11',MNUMYR) =
    sum((DomRefReg,LiquefiedGases,Industry),repStreamFlows(DomRefReg,LiquefiedGases,'Chemicals','Products',Industry,t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_REFINE_PROD('1_M4','08_M11',MNUMYR) =
    sum((DomRefReg,RTLiquids,Industry),repStreamFlows(DomRefReg,RTLiquids,'Chemicals','Products',Industry,t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_REFINE_PROD('1_M4','09_M11',MNUMYR) =
    LFMMOUT_INTERMEDIATE('1_M4','12_M13',MNUMYR) ;
LFMMOUT_REFINE_PROD('1_M4','10_M11',MNUMYR) =
    LFMMOUT_INTERMEDIATE('1_M4','13_M13',MNUMYR) ;


LFMMOUT_GROSS_IMPORT('1_M4','01_M11',MNUMYR) =
    sum((DomRefReg,CRBOB),repStreamFlows(DomRefReg,CRBOB,'BOBImports','Products','Petroleum',t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_GROSS_IMPORT('1_M4','02_M11',MNUMYR) =
    sum((DomRefReg,EthStream),repStreamFlows(DomRefReg,EthStream,'EthanolImports','Products','Biofuels',t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_GROSS_IMPORT('1_M4','03_M11',MNUMYR) =
    sum((DomRefReg,Gasoline,Industry),repStreamFlows(DomRefReg,Gasoline,'GasolineImports','Products',Industry,t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_GROSS_IMPORT('1_M4','04_M11',MNUMYR) =
    sum((DomRefReg,Industry),repStreamFlows(DomRefReg,'JTAout','DistillateImports','Products',Industry,t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_GROSS_IMPORT('1_M4','05_M11',MNUMYR) =
    sum((DomRefReg,Diesel,Industry),repStreamFlows(DomRefReg,Diesel,'DistillateImports','Products',Industry,t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_GROSS_IMPORT('1_M4','06_M11',MNUMYR) =
    sum((DomRefReg,Industry),repStreamFlows(DomRefReg,'N2Hout','DistillateImports','Products',Industry,t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_GROSS_IMPORT('1_M4','07_M11',MNUMYR) =
    sum((DomRefReg,ResidFuel),repStreamFlows(DomRefReg,ResidFuel,'ResidImports','Products','Petroleum',t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_GROSS_IMPORT('1_M4','08_M11',MNUMYR) =
    sum((DomRefReg,NGLProduct,Industry),repStreamFlows(DomRefReg,NGLProduct,'NGPLImports','Feedstocks',Industry,t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_GROSS_IMPORT('1_M4','09_M11',MNUMYR) =
    sum((DomRefReg,RTLiquids),repStreamFlows(DomRefReg,RTLiquids,'ChemicalImports','Products','Blending',t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_GROSS_IMPORT('1_M4','10_M11',MNUMYR) =
    sum((DomRefReg,Solids),repStreamFlows(DomRefReg,Solids,'ChemicalImports','Products','Blending',t) *
    TMNUM(t,MNUMYR))* MMBPD_TO_BCF ;


LFMMOUT_GROSS_EXPORT('1_M4','01_M12',MNUMYR) =
    sum((DomRefReg,CRBOB),repStreamFlows(DomRefReg,CRBOB,'BOBExports','Products','Petroleum',t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_GROSS_Export('1_M4','02_M12',MNUMYR) =
    sum((DomRefReg,EthStream),repStreamFlows(DomRefReg,EthStream,'EthanolExports','Products','Biofuels',t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_GROSS_Export('1_M4','03_M12',MNUMYR) =
    sum((DomRefReg,Gasoline,Industry),repStreamFlows(DomRefReg,Gasoline,'GasolineExports','Products',Industry,t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_GROSS_Export('1_M4','04_M12',MNUMYR) =
    sum((DomRefReg,Industry),repStreamFlows(DomRefReg,'JTAout','DistillateExports','Products',Industry,t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_GROSS_Export('1_M4','05_M12',MNUMYR) =
    sum((DomRefReg,Diesel,Industry),repStreamFlows(DomRefReg,Diesel,'DistillateExports','Products',Industry,t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_GROSS_Export('1_M4','06_M12',MNUMYR) =
    sum((DomRefReg,Industry),repStreamFlows(DomRefReg,'N2Hout','DistillateExports','Products',Industry,t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_GROSS_Export('1_M4','07_M12',MNUMYR) =
    sum((DomRefReg,ResidFuel),repStreamFlows(DomRefReg,ResidFuel,'ResidExports','Products','Petroleum',t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_GROSS_Export('1_M4','08_M12',MNUMYR) =
    sum((DomRefReg,NGLProduct,Industry),repStreamFlows(DomRefReg,NGLProduct,'NGLExports','Feedstocks',Industry,t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_GROSS_Export('1_M4','09_M12',MNUMYR) =
    sum((DomRefReg,RTLiquids),repStreamFlows(DomRefReg,RTLiquids,'ChemicalExports','Products','Blending',t) *
    TMNUM(t,MNUMYR)) ;
LFMMOUT_GROSS_Export('1_M4','10_M12',MNUMYR) =
    sum((DomRefReg,Solids),repStreamFlows(DomRefReg,Solids,'ChemicalExports','Products','Blending',t) *
    TMNUM(t,MNUMYR))* MMBPD_TO_BCF ;


LFMMOUT_DOM_CONSUME('1_M4','01_M12',MNUMYR) =
    sum(DomRefReg,repStreamFlows(DomRefReg,'LPGout','Chemicals','EndProducts','Blending',t) *
        TMNUM(t,MNUMYR)) ;
LFMMOUT_DOM_CONSUME('1_M4','02_M12',MNUMYR) =
    sum((DomRefReg,E10,Industry),repStreamFlows(DomRefReg,E10,'Gasoline','EndProducts',Industry,t) *
        TMNUM(t,MNUMYR)) ;
LFMMOUT_DOM_CONSUME('1_M4','03_M12',MNUMYR) =
    sum((DomRefReg,E15,Industry),repStreamFlows(DomRefReg,E15,'Gasoline','EndProducts',Industry,t) *
        TMNUM(t,MNUMYR)) ;
LFMMOUT_DOM_CONSUME('1_M4','04_M12',MNUMYR) =
    sum((DomRefReg,Industry),repStreamFlows(DomRefReg,'E85out','Gasoline','EndProducts',Industry,t) *
        TMNUM(t,MNUMYR)) ;
LFMMOUT_DOM_CONSUME('1_M4','05_M12',MNUMYR) =
    sum((DomRefReg,Industry),repStreamFlows(DomRefReg,'JTAout','Distillate','EndProducts',Industry,t) *
        TMNUM(t,MNUMYR)) ;
LFMMOUT_DOM_CONSUME('1_M4','06_M12',MNUMYR) =
    sum((DomRefReg,Diesel,Industry),repStreamFlows(DomRefReg,Diesel,'Distillate','EndProducts',Industry,t) *
        TMNUM(t,MNUMYR)) ;
LFMMOUT_DOM_CONSUME('1_M4','07_M12',MNUMYR) =
    sum((DomRefReg,Industry),repStreamFlows(DomRefReg,'N2Hout','Distillate','EndProducts',Industry,t) *
        TMNUM(t,MNUMYR)) ;
LFMMOUT_DOM_CONSUME('1_M4','08_M12',MNUMYR) =
    sum((DomRefReg,ResidFuel, Industry),repStreamFlows(DomRefReg,ResidFuel,'ResidFuel','EndProducts',Industry,t) *
        TMNUM(t,MNUMYR)) ;
LFMMOUT_DOM_CONSUME('1_M4','09_M12',MNUMYR) =
    (sum((DomRefReg,LiquidChem),repStreamFlows(DomRefReg,LiquidChem,'Chemicals','EndProducts','Blending',t)* TMNUM(t,MNUMYR)) -
    sum(DomRefReg,repStreamFlows(DomRefReg,'LPGout','Chemicals','EndProducts','Blending',t)* TMNUM(t,MNUMYR))) ;
LFMMOUT_DOM_CONSUME('1_M4','10_M12',MNUMYR) =
    sum((DomRefReg,Solids),repStreamFlows(DomRefReg,Solids,'Chemicals','EndProducts','Blending',t) *
        TMNUM(t,MNUMYR))* MMBPD_TO_BCF -
    sum((DomRefReg,Solids),repStreamFlows(DomRefReg,'COKout','Chemicals','EndProducts','Blending',t) *
        TMNUM(t,MNUMYR))* MMBPD_TO_BCF -
    sum((DomRefReg,Solids),repStreamFlows(DomRefReg,'COKdump','Chemicals','EndProducts','Blending',t) *
        TMNUM(t,MNUMYR))* MMBPD_TO_BCF ;
LFMMOUT_DOM_CONSUME('1_M4','11_M12',MNUMYR) =
    sum(DomRefReg,(repStreamFlows(DomRefReg,'DDGout','Chemicals','Products','Biofuels',t) +
        repStreamFlows(DomRefReg,'DGSout','Chemicals','Products','Biofuels',t)) * TMNUM(t,MNUMYR)) / (35/2000) * 365 / 1000 ;
LFMMOUT_DOM_CONSUME('1_M4','12_M12',MNUMYR) =
    sum((DomRefReg,Solids),repStreamFlows(DomRefReg,'COKout','Chemicals','EndProducts','Blending',t) *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BCF -
    sum((DomRefReg,Solids),repStreamFlows(DomRefReg,'COKdump','Chemicals','EndProducts','Blending',t) *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BCF;

*Value units (billion US dollars)
LFMMOUT_FEEDSTOCKS('2_M4','01_M10',MNUMYR) =
    sum((DomRefReg,Crude),repStreamFlows(DomRefReg,Crude,'DomesticCrude','Feedstocks','Petroleum',t) *
        repStreamPrices(DomRefReg,Crude,'Crude','Feedstocks','Petroleum',t) * TMNUM(t,MNUMYR)) * 365 / 1000 / GDP('2010') ;
LFMMOUT_FEEDSTOCKS('2_M4','02_M10',MNUMYR) =
    sum((DomRefReg,Crude,Industry),repStreamFlows(DomRefReg,Crude,'CrudeImports','Feedstocks',Industry,t) *
        repStreamPrices(DomRefReg,Crude,'Crude','Feedstocks',Industry,t) * TMNUM(t,MNUMYR)) * 365 / 1000 / GDP('2010') ;
LFMMOUT_FEEDSTOCKS('2_M4','03_M10',MNUMYR) =
    sum((DomRefReg,UnfinishedOils,Industry),repStreamFlows(DomRefReg,UnfinishedOils,'UFOImports','Feedstocks',Industry,t) *
        repStreamPrices(DomRefReg,UnfinishedOils,'UFOImports','Feedstocks',Industry,t) * TMNUM(t,MNUMYR)) * 365 / 1000 / GDP('2010') ;
LFMMOUT_FEEDSTOCKS('2_M4','04_M10',MNUMYR) =
    sum((DomRefReg,NGLInputStr,Industry),repStreamFlows(DomRefReg,NGLInputStr,'DomesticNGPL','Feedstocks',Industry,t) *
        repStreamPrices(DomRefReg,NGLInputStr,'DomesticNGPL','Feedstocks',Industry,t) * TMNUM(t,MNUMYR)) * 365 / 1000 / GDP('2010') ;

* note:  repStreamPrices for *,*,'Chemicals','Products','Blending' was defined as $/gal, so convert back to $/bbl
* note:  NGL imports already reported as product imports, so set to 0 here
LFMMOUT_FEEDSTOCKS('2_M4','05_M10',MNUMYR) = 0 ;
LFMMOUT_FEEDSTOCKS('2_M4','06_M10',MNUMYR) =
    sum((DomRefReg,Gas,Industry),(repStreamFlows(DomRefReg,Gas,'GasFeed','Feedstocks',Industry,t) +
        repStreamFlows(DomRefReg,Gas,'Gas','Feedstocks',Industry,t)) *
        repStreamPrices(DomRefReg,Gas,'Gas','Feedstocks',Industry,t) * TMNUM(t,MNUMYR)) * 365 / 1000000 / GDP('2010') ;
LFMMOUT_FEEDSTOCKS('2_M4','07_M10',MNUMYR) =
    sum((DomRefReg,Coal,Industry),repStreamFlows(DomRefReg,Coal,'Coal','Feedstocks',Industry,t) *
        repStreamPrices(DomRefReg,Coal,'Coal','Feedstocks',Industry,t) * TMNUM(t,MNUMYR)) * 365 / 1000 / GDP('2010') ;
LFMMOUT_FEEDSTOCKS('2_M4','08_M10',MNUMYR) =
    sum((DomRefReg,GrainCrops,Industry),repStreamFlows(DomRefReg,GrainCrops,'Grains','Feedstocks',Industry,t) *
        repStreamPrices(DomRefReg,GrainCrops,'Grains','Feedstocks',Industry,t) * TMNUM(t,MNUMYR)) * 365 / 1000 / GDP('2010') ;
LFMMOUT_FEEDSTOCKS('2_M4','09_M10',MNUMYR) =
    sum((DomRefReg,BioStr,Industry),repStreamFlows(DomRefReg,BioStr,'Cellulose','Feedstocks',Industry,t) *
        repStreamPrices(DomRefReg,BioStr,'Cellulose','Feedstocks',Industry,t) * TMNUM(t,MNUMYR)) * 365 / 1000 / GDP('2010') ;
LFMMOUT_FEEDSTOCKS('2_M4','10_M10',MNUMYR) =
    sum((DomRefReg,RenewableOils,Industry),repStreamFlows(DomRefReg,RenewableOils,'Oils','Feedstocks',Industry,t) *
        repStreamPrices(DomRefReg,RenewableOils,'Oils','Feedstocks',Industry,t) * TMNUM(t,MNUMYR)) * 365 / 1000 / GDP('2010') ;

LFMMOUT_INTERMEDIATE('2_M4','01_M13',MNUMYR) =
    sum((DomRefReg,IntStream),
        (repStreamFlows(DomRefReg,IntStream,'CBOB','Products','Petroleum',t) * repStreamPrices(DomRefReg,'CBOB','Gasoline','Products','Blending',t) +
        repStreamFlows(DomRefReg,IntStream,'RBOB','Products','Petroleum',t) * repStreamPrices(DomRefReg,'RBOB','Gasoline','Products','Blending',t) +
        repStreamFlows(DomRefReg,IntStream,'CaRBOB','Products','Petroleum',t) * repStreamPrices('7_RefReg','CaRBOB','Gasoline','Products','Blending',t)) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_INTERMEDIATE('2_M4','02_M13',MNUMYR) =
    sum((DomRefReg,IntStream),
        (repStreamFlows(DomRefReg,IntStream,'CBOB','Products','NonPetroleumFossil',t) * repStreamPrices(DomRefReg,'CBOB','Gasoline','Products','Blending',t) +
        repStreamFlows(DomRefReg,IntStream,'RBOB','Products','NonPetroleumFossil',t) * repStreamPrices(DomRefReg,'RBOB','Gasoline','Products','Blending',t) +
        repStreamFlows(DomRefReg,IntStream,'CaRBOB','Products','NonPetroleumFossil',t) * repStreamPrices('7_RefReg','CaRBOB','Gasoline','Products','Blending',t)) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_INTERMEDIATE('2_M4','03_M13',MNUMYR) =
    sum((DomRefReg,IntStream),
        repStreamFlows(DomRefReg,IntStream,'CBOB','Products','Biofuels',t) * repStreamPrices(DomRefReg,'CBOB','Gasoline','Products','Blending',t) +
        repStreamFlows(DomRefReg,IntStream,'RBOB','Products','Biofuels',t) * repStreamPrices(DomRefReg,'RBOB','Gasoline','Products','Blending',t) +
        repStreamFlows(DomRefReg,IntStream,'CaRBOB','Products','Biofuels',t) * repStreamPrices('7_RefReg','CaRBOB','Gasoline','Products','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_INTERMEDIATE('2_M4','04_M13',MNUMYR) =
    sum((DomRefReg,EthStream),
        repStreamFlows(DomRefReg,EthStream,'Ethanol','Intermediates','Biofuels',t) * repStreamPrices(DomRefReg,EthStream,'Ethanol','Intermediates','Biofuels',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000000 / GDP('2010') ;
LFMMOUT_INTERMEDIATE('2_M4','05_M13',MNUMYR) =
    sum((DomRefReg,IntStream),
        repStreamFlows(DomRefReg,IntStream,'ULSD','Products','Petroleum',t) * repStreamPrices(DomRefReg,'DSU','Distillate','Products','Blending',t) +
        repStreamFlows(DomRefReg,IntStream,'LowSulfurDiesel','Products','Petroleum',t) * repStreamPrices(DomRefReg,'DSL','Distillate','Products','Blending',t) +
        repStreamFlows(DomRefReg,IntStream,'CarbULSD','Products','Petroleum',t) * repStreamPrices(DomRefReg,'CarbDSU','Distillate','Products','Blending',t) +
        repStreamFlows(DomRefReg,IntStream,'JetFuel','Products','Petroleum',t) * repStreamPrices(DomRefReg,'JTA','Distillate','Products','Blending',t) +
        repStreamFlows(DomRefReg,IntStream,'HeatingOil','Products','Petroleum',t) * repStreamPrices(DomRefReg,'N2H','Distillate','Products','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_INTERMEDIATE('2_M4','06_M13',MNUMYR) =
    sum((DomRefReg,IntStream),
        repStreamFlows(DomRefReg,IntStream,'ULSD','Products','NonPetroleumFossil',t) * repStreamPrices(DomRefReg,'DSU','Distillate','Products','Blending',t) +
        repStreamFlows(DomRefReg,IntStream,'LowSulfurDiesel','Products','NonPetroleumFossil',t) * repStreamPrices(DomRefReg,'DSL','Distillate','Products','Blending',t) +
        repStreamFlows(DomRefReg,IntStream,'CarbULSD','Products','NonPetroleumFossil',t) * repStreamPrices(DomRefReg,'CarbDSU','Distillate','Products','Blending',t) +
        repStreamFlows(DomRefReg,IntStream,'JetFuel','Products','NonPetroleumFossil',t) * repStreamPrices(DomRefReg,'JTA','Distillate','Products','Blending',t)+
        repStreamFlows(DomRefReg,IntStream,'HeatingOil','Products','NonPetroleumFossil',t) * repStreamPrices(DomRefReg,'N2H','Distillate','Products','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_INTERMEDIATE('2_M4','07_M13',MNUMYR) =
    sum((DomRefReg,IntStream),
        repStreamFlows(DomRefReg,IntStream,'ULSD','Products','Biofuels',t) * repStreamPrices(DomRefReg,'DSU','Distillate','Products','Blending',t) +
        repStreamFlows(DomRefReg,IntStream,'LowSulfurDiesel','Products','Biofuels',t) * repStreamPrices(DomRefReg,'DSL','Distillate','Products','Blending',t) +
        repStreamFlows(DomRefReg,IntStream,'CarbULSD','Products','Biofuels',t) * repStreamPrices(DomRefReg,'CarbDSU','Distillate','Products','Blending',t) +
        repStreamFlows(DomRefReg,IntStream,'JetFuel','Products','Biofuels',t) * repStreamPrices(DomRefReg,'JTA','Distillate','Products','Blending',t)+
        repStreamFlows(DomRefReg,IntStream,'HeatingOil','Products','Biofuels',t) * repStreamPrices(DomRefReg,'N2H','Distillate','Products','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_INTERMEDIATE('2_M4','08_M13',MNUMYR) =
    sum((DomRefReg,Stream),
        repStreamFlows(DomRefReg,Stream,'ResidFuel','EndProducts','Blending',t) * repStreamPrices(DomRefReg,Stream,'ResidFuel','EndProducts','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_INTERMEDIATE('2_M4','09_M13',MNUMYR) =
    sum((DomRefReg,LiquidChem),
        repStreamFlows(DomRefReg,LiquidChem,'Chemicals','Products','Petroleum',t) * repStreamPrices(DomRefReg,LiquidChem,'Chemicals','Products','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_INTERMEDIATE('2_M4','10_M13',MNUMYR) =
    sum((DomRefReg,LiquidChem),
        repStreamFlows(DomRefReg,LiquidChem,'Chemicals','Products','NonPetroleumFossil',t) * repStreamPrices(DomRefReg,LiquidChem,'Chemicals','Products','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_INTERMEDIATE('2_M4','11_M13',MNUMYR) =
    sum((DomRefReg,LiquidChem),
        repStreamFlows(DomRefReg,LiquidChem,'Chemicals','Products','Biofuels',t) * repStreamPrices(DomRefReg,LiquidChem,'Chemicals','Products','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_INTERMEDIATE('2_M4','12_M13',MNUMYR) =
    sum((DomRefReg,Solids),
        repStreamFlows(DomRefReg,Solids,'Chemicals','Products','Petroleum',t) * repStreamPrices(DomRefReg,Solids,'Chemicals','Products','Blending',t) *
        TMNUM(t,MNUMYR)) * 365 / 1000 / GDP('2010') ;
LFMMOUT_INTERMEDIATE('2_M4','13_M13',MNUMYR) =
    sum(DomRefReg,
        repStreamFlows(DomRefReg,'DDGout','Chemicals','Products','Biofuels',t) * repStreamPrices(DomRefReg,'DDGout','NonFuelStreams','Products','Blending',t) *
        TMNUM(t,MNUMYR)) / (35/2000) * 365 / 1000 / GDP('2010') ;


LFMMOUT_REFINE_PROD('2_M4','01_M11',MNUMYR) =
    LFMMOUT_INTERMEDIATE('2_M4','01_M13',MNUMYR) +
    LFMMOUT_INTERMEDIATE('2_M4','02_M13',MNUMYR) +
    LFMMOUT_INTERMEDIATE('2_M4','03_M13',MNUMYR) ;
LFMMOUT_REFINE_PROD('2_M4','02_M11',MNUMYR) =
    LFMMOUT_INTERMEDIATE('2_M4','04_M13',MNUMYR) * 1.0249 ;
LFMMOUT_REFINE_PROD('2_M4','03_M11',MNUMYR) =
    sum((DomRefReg,IntStream,Industry),
        repStreamFlows(DomRefReg,IntStream,'JetFuel','Products',Industry,t) * repStreamPrices(DomRefReg,'JTA','Distillate','Products','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_REFINE_PROD('2_M4','04_M11',MNUMYR) =
    sum((DomRefReg,IntStream,Industry),
        repStreamFlows(DomRefReg,IntStream,'ULSD','Products',Industry,t) * repStreamPrices(DomRefReg,'DSU','Distillate','Products','Blending',t) +
        repStreamFlows(DomRefReg,IntStream,'LowSulfurDiesel','Products',Industry,t) * repStreamPrices(DomRefReg,'DSL','Distillate','Products','Blending',t) +
        repStreamFlows(DomRefReg,IntStream,'CarbULSD','Products',Industry,t) * repStreamPrices(DomRefReg,'CarbDSU','Distillate','Products','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_REFINE_PROD('2_M4','05_M11',MNUMYR) =
    sum((DomRefReg,IntStream,Industry),
        repStreamFlows(DomRefReg,IntStream,'HeatingOil','Products',Industry,t) * repStreamPrices(DomRefReg,'N2H','Distillate','Products','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_REFINE_PROD('2_M4','06_M11',MNUMYR) =
    LFMMOUT_INTERMEDIATE('2_M4','08_M13',MNUMYR) ;
LFMMOUT_REFINE_PROD('2_M4','07_M11',MNUMYR) =
    sum((DomRefReg,LiquefiedGases,Industry),
        repStreamFlows(DomRefReg,LiquefiedGases,'Chemicals','Products',Industry,t) * repStreamPrices(DomRefReg,LiquefiedGases,'Chemicals','Products','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_REFINE_PROD('2_M4','08_M11',MNUMYR) =
    sum((DomRefReg,RTLiquids,Industry),
        repStreamFlows(DomRefReg,RTLiquids,'Chemicals','Products',Industry,t) * repStreamPrices(DomRefReg,RTLiquids,'Chemicals','Products','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_REFINE_PROD('2_M4','09_M11',MNUMYR) =
    LFMMOUT_INTERMEDIATE('2_M4','12_M13',MNUMYR) ;
LFMMOUT_REFINE_PROD('2_M4','10_M11',MNUMYR) =
    LFMMOUT_INTERMEDIATE('2_M4','13_M13',MNUMYR) ;


LFMMOUT_GROSS_IMPORT('2_M4','01_M11',MNUMYR) =
    sum((DomRefReg,CRBOB),
       (repStreamFlows (DomRefReg,CRBOB,'BOBImports','Products','Petroleum',t) *
        repStreamPrices(DomRefReg,CRBOB,'Gasoline','Products','Blending',t)) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_GROSS_IMPORT('2_M4','02_M11',MNUMYR) =
    sum((DomRefReg,EthStream),
        repStreamFlows(DomRefReg,EthStream,'EthanolImports','Products','Biofuels',t) * repStreamPrices(DomRefReg,'ETHAETbrz','Ethanol','Intermediates','Biofuels',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_GROSS_IMPORT('2_M4','03_M11',MNUMYR) =
    sum((DomRefReg,Gasoline,Industry),
        repStreamFlows(DomRefReg,Gasoline,'GasolineImports','Products',Industry,t) * repStreamPrices(DomRefReg,'CFGout','Gasoline','EndProducts','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_GROSS_IMPORT('2_M4','04_M11',MNUMYR) =
    sum((DomRefReg,Industry),
        repStreamFlows(DomRefReg,'JTAout','DistillateImports','Products',Industry,t) * repStreamPrices(DomRefReg,'JTAout','Distillate','EndProducts','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_GROSS_IMPORT('2_M4','05_M11',MNUMYR) =
    sum((DomRefReg,Diesel,Industry),
        repStreamFlows(DomRefReg,Diesel,'DistillateImports','Products',Industry,t) *
            (repStreamPrices(DomRefReg,'DSUout','Distillate','EndProducts','Blending',t)  +
             repStreamPrices(DomRefReg,'CarbDSUout','Distillate','EndProducts','Blending',t)) / 2 *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_GROSS_IMPORT('2_M4','06_M11',MNUMYR) =
    sum((DomRefReg,Industry),
        repStreamFlows(DomRefReg,'N2Hout','DistillateImports','Products',Industry,t) * repStreamPrices(DomRefReg,'N2Hout','Distillate','EndProducts','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_GROSS_IMPORT('2_M4','07_M11',MNUMYR) =
    sum((DomRefReg,ResidFuel),
        repStreamFlows(DomRefReg,ResidFuel,'ResidImports','Products','Petroleum',t) *
            (repStreamPrices(DomRefReg,'N6Bout','ResidFuel','EndProducts','Blending',t) +
             repStreamPrices(DomRefReg,'N6Iout','ResidFuel','EndProducts','Blending',t)) / 2 *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;

* note: Since NGL imports reported here, NGL import feedstock set to zero above
LFMMOUT_GROSS_IMPORT('2_M4','08_M11',MNUMYR) =
    sum((DomRefReg,NGLProduct,Industry),
        repStreamFlows (DomRefReg,NGLProduct,'NGPLImports','Feedstocks',Industry,t) *
        repStreamPrices(DomRefReg,NGLProduct,'Chemicals','Products','Blending',t) *
        tMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;

LFMMOUT_GROSS_IMPORT('2_M4','09_M11',MNUMYR) =
    sum((DomRefReg,RTLiquids),
        repStreamFlows(DomRefReg,RTLiquids,'ChemicalImports','Products','Blending',t) * repStreamPrices(DomRefReg,RTLiquids,'Chemicals','Products','Blending',t) *
        tMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_GROSS_IMPORT('2_M4','10_M11',MNUMYR) =
    sum((DomRefReg,Solids),
        repStreamFlows(DomRefReg,Solids,'ChemicalImports','Products','Blending',t) * repStreamPrices(DomRefReg,Solids,'Chemicals','Products','Blending',t) *
        tMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;


* repStreamPrices = 2010$/gal
* repStreamFlows  = 1000 bbl/cd
* GROSS_Export    = billion RptYr dollars (2012 for aeo2014)

LFMMOUT_GROSS_Export('2_M4','01_M12',MNUMYR) =
    sum((DomRefReg,CRBOB),
       (repStreamFlows (DomRefReg,CRBOB,'BOBExports','Products','Petroleum',t) *
        repStreamPrices(DomRefReg,CRBOB,'Gasoline','Products','Blending',t)) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;

LFMMOUT_GROSS_Export('2_M4','02_M12',MNUMYR) =
    sum((DomRefReg,EthStream),
        repStreamFlows(DomRefReg,EthStream,'EthanolExports','Products','Biofuels',t) * repStreamPrices(DomRefReg,'ETHAETbrz','Ethanol','Intermediates','Biofuels',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_GROSS_Export('2_M4','03_M12',MNUMYR) =
    sum((DomRefReg,Gasoline,Industry),
        repStreamFlows(DomRefReg,Gasoline,'GasolineExports','Products',Industry,t) * repStreamPrices(DomRefReg,'CFGout','Gasoline','EndProducts','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_GROSS_Export('2_M4','04_M12',MNUMYR) =
    sum((DomRefReg,Industry),
        repStreamFlows(DomRefReg,'JTAout','DistillateExports','Products',Industry,t) * repStreamPrices(DomRefReg,'JTAout','Distillate','EndProducts','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_GROSS_Export('2_M4','05_M12',MNUMYR) =
    sum((DomRefReg,Diesel,Industry),
        repStreamFlows(DomRefReg,Diesel,'DistillateExports','Products',Industry,t) *
            (repStreamPrices(DomRefReg,'DSUout','Distillate','EndProducts','Blending',t)  +
             repStreamPrices(DomRefReg,'CarbDSUout','Distillate','EndProducts','Blending',t)) / 2 *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_GROSS_Export('2_M4','06_M12',MNUMYR) =
    sum((DomRefReg,Industry),
        repStreamFlows(DomRefReg,'N2Hout','DistillateExports','Products',Industry,t) * repStreamPrices(DomRefReg,'N2Hout','Distillate','EndProducts','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_GROSS_Export('2_M4','07_M12',MNUMYR) =
    sum((DomRefReg,ResidFuel),
        repStreamFlows(DomRefReg,ResidFuel,'ResidExports','Products','Petroleum',t) *
            (repStreamPrices(DomRefReg,'N6Bout','ResidFuel','EndProducts','Blending',t) +
             repStreamPrices(DomRefReg,'N6Iout','ResidFuel','EndProducts','Blending',t)) / 2 *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_GROSS_Export('2_M4','08_M12',MNUMYR) =
    sum((DomRefReg,NGLProduct,Industry),
        repStreamFlows(DomRefReg,NGLProduct,'NGLExports','Feedstocks',Industry,t) *
        repStreamPrices(DomRefReg,NGLProduct,'Chemicals','Products','Blending',t) *
        tMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;

LFMMOUT_GROSS_Export('2_M4','09_M12',MNUMYR) =
    sum((DomRefReg,RTLiquids),
        repStreamFlows(DomRefReg,RTLiquids,'ChemicalExports','Products','Blending',t) * repStreamPrices(DomRefReg,RTLiquids,'Chemicals','Products','Blending',t) *
        tMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_GROSS_Export('2_M4','10_M12',MNUMYR) =
    sum((DomRefReg,Solids),
        repStreamFlows(DomRefReg,Solids,'ChemicalExports','Products','Blending',t) * repStreamPrices(DomRefReg,Solids,'Chemicals','Products','Blending',t) *
        tMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;

LFMMOUT_DOM_CONSUME('2_M4','01_M12',MNUMYR) =
    sum(DomRefReg,
        repStreamFlows(DomRefReg,'LPGout','Chemicals','EndProducts','Blending',t) * repStreamPrices(DomRefReg,'LPGout','Chemicals','Products','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_DOM_CONSUME('2_M4','02_M12',MNUMYR) =
    sum((DomRefReg,E10,Industry),repStreamFlows(DomRefReg,E10,'Gasoline','EndProducts',Industry,t) *
        (repStreamPrices(DomRefReg,'CFGout','Gasoline','EndProducts','Blending',t) +
        repStreamPrices(DomRefReg,'RFGout','Gasoline','EndProducts','Blending',t)  +
        repStreamPrices(DomRefReg,'CaRBOBout','Gasoline','EndProducts','Blending',t)) / 3 *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_DOM_CONSUME('2_M4','03_M12',MNUMYR) =
    sum((DomRefReg,E15,Industry),repStreamFlows(DomRefReg,E15,'Gasoline','EndProducts',Industry,t) *
        (repStreamPrices(DomRefReg,'CFG15out','Gasoline','EndProducts','Blending',t) +
        repStreamPrices(DomRefReg,'RFG15out','Gasoline','EndProducts','Blending',t)) / 2 *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_DOM_CONSUME('2_M4','04_M12',MNUMYR) =
    sum((DomRefReg,Industry),repStreamFlows(DomRefReg,'E85out','Gasoline','EndProducts',Industry,t) * repStreamPrices(DomRefReg,'E85out','Gasoline','EndProducts','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_DOM_CONSUME('2_M4','05_M12',MNUMYR) =
    sum((DomRefReg,Industry),repStreamFlows(DomRefReg,'JTAout','Distillate','EndProducts',Industry,t) * repStreamPrices(DomRefReg,'JTAout','Distillate','EndProducts','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010');
LFMMOUT_DOM_CONSUME('2_M4','06_M12',MNUMYR) =
    sum((DomRefReg,Diesel,Industry),
        repStreamFlows(DomRefReg,Diesel,'Distillate','EndProducts',Industry,t) *
            (repStreamPrices(DomRefReg,'DSUout','Distillate','EndProducts','Blending',t) +
            repStreamPrices(DomRefReg,'DSLout','Distillate','EndProducts','Blending',t)  +
            repStreamPrices(DomRefReg,'CarbDSUout','Distillate','EndProducts','Blending',t)) / 3 *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_DOM_CONSUME('2_M4','07_M12',MNUMYR) =
    sum((DomRefReg,Industry),repStreamFlows(DomRefReg,'N2Hout','Distillate','EndProducts',Industry,t) * repStreamPrices(DomRefReg,'N2Hout','Distillate','EndProducts','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010');
LFMMOUT_DOM_CONSUME('2_M4','08_M12',MNUMYR) =
    sum((DomRefReg,ResidFuel, Industry),repStreamFlows(DomRefReg,ResidFuel,'ResidFuel','EndProducts',Industry,t) *
        (repStreamPrices(DomRefReg,'N6Bout','ResidFuel','EndProducts','Blending',t) +
        repStreamPrices(DomRefReg,'N6Iout','ResidFuel','EndProducts','Blending',t)) / 2 *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_DOM_CONSUME('2_M4','09_M12',MNUMYR) =
    sum((DomRefReg,LiquidChem),
        repStreamFlows(DomRefReg,LiquidChem,'Chemicals','EndProducts','Blending',t) * repStreamPrices(DomRefReg,LiquidChem,'Chemicals','Products','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') -
    sum(DomRefReg,
        repStreamFlows(DomRefReg,'LPGout','Chemicals','EndProducts','Blending',t) * repStreamPrices(DomRefReg,'LPGout','Chemicals','Products','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_DOM_CONSUME('2_M4','10_M12',MNUMYR) =
    sum((DomRefReg,Solids),repStreamFlows(DomRefReg,Solids,'Chemicals','EndProducts','Blending',t) * repStreamPrices(DomRefReg,Solids,'Chemicals','Products','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') -
    sum(DomRefReg,repStreamFlows(DomRefReg,'COKout','Chemicals','EndProducts','Blending',t) * repStreamPrices(DomRefReg,'COKout','Chemicals','EndProducts','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') -
    sum(DomRefReg,repStreamFlows(DomRefReg,'COKdump','Chemicals','EndProducts','Blending',t) * repStreamPrices(DomRefReg,'COKdump','Chemicals','EndProducts','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;
LFMMOUT_DOM_CONSUME('2_M4','11_M12',MNUMYR) =
    sum(DomRefReg,repStreamFlows(DomRefReg,'DDGout','NonfuelStreams','Products','Biofuels',t) * repStreamPrices(DomRefReg,'DDGout','NonFuelStreams','Products','Biofuels',t) *
        TMNUM(t,MNUMYR)) / (35/2000) * 365 / 1000 / GDP('2010') +
    sum(DomRefReg,repStreamFlows(DomRefReg,'DGSout','NonfuelStreams','Products','Biofuels',t) * repStreamPrices(DomRefReg,'DGSout','NonFuelStreams','Products','Biofuels',t) *
        TMNUM(t,MNUMYR)) / (35/2000) * 365 / 1000 / GDP('2010') ;
LFMMOUT_DOM_CONSUME('2_M4','12_M12',MNUMYR) =
    sum(DomRefReg,repStreamFlows(DomRefReg,'COKout','Chemicals','EndProducts','Blending',t) * repStreamPrices(DomRefReg,'COKout','Chemicals','Products','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 /GDP('2010') +
    sum(DomRefReg,repStreamFlows(DomRefReg,'COKdump','Chemicals','EndProducts','Blending',t) * repStreamPrices(DomRefReg,'COKdump','Chemicals','Products','Blending',t) *
        TMNUM(t,MNUMYR)) * 42 * 365 / 1000 / GDP('2010') ;


*Mass units (billion short tons)
LFMMOUT_FEEDSTOCKS('3_M4','01_M10',MNUMYR) =
    sum((DomRefReg,Crude),
        repStreamFlows(DomRefReg,Crude,'DomesticCrude','Feedstocks','Petroleum',t) * repStreamProperties(Crude,'Crude','Feedstocks','Petroleum','SPG') *
        tMNUM(t,MNUMYR)) * MMBPD_TO_BTONS;
LFMMOUT_FEEDSTOCKS('3_M4','02_M10',MNUMYR) =
    sum((DomRefReg,Crude),
        repStreamFlows(DomRefReg,Crude,'CrudeImports','Feedstocks','Petroleum',t) * repStreamProperties(Crude,'Crude','Feedstocks','Petroleum','SPG') *
        tMNUM(t,MNUMYR)) * MMBPD_TO_BTONS;
LFMMOUT_FEEDSTOCKS('3_M4','03_M10',MNUMYR) =
    sum((DomRefReg,UnfinishedOils),
        repStreamFlows(DomRefReg,UnfinishedOils,'UFOImports','Feedstocks','Petroleum',t) * repStreamProperties(UnfinishedOils,'UFOImports','Feedstocks','Petroleum','SPG') *
        tMNUM(t,MNUMYR)) * MMBPD_TO_BTONS;
LFMMOUT_FEEDSTOCKS('3_M4','04_M10',MNUMYR) =
    sum((DomRefReg,NGLInputStr),
        repStreamFlows(DomRefReg,NGLInputStr,'DomesticNGPL','Feedstocks','NonPetroleumFossil',t) * repStreamProperties(NGLInputStr,'GasLiquids','Feedstocks','NonPetroleumFossil','SPG') *
        tMNUM(t,MNUMYR)) * MMBPD_TO_BTONS;
LFMMOUT_FEEDSTOCKS('3_M4','05_M10',MNUMYR) =
    sum((DomRefReg,NGLProduct),
        repStreamFlows(DomRefReg,NGLProduct,'NGPLImports','Feedstocks','NonPetroleumFossil',t) * repStreamProperties(NGLProduct,'GasLiquids','Feedstocks','NonPetroleumFossil','SPG') *
        tMNUM(t,MNUMYR)) * MMBPD_TO_BTONS;
LFMMOUT_FEEDSTOCKS('3_M4','06_M10',MNUMYR) =
    sum((DomRefReg,Gas),
        repStreamFlows(DomRefReg,Gas,'GasFeed','Feedstocks','NonPetroleumFossil',t) * repStreamProperties(Gas,'Gas','Feedstocks','NonPetroleumFossil','SPG') *
        tMNUM(t,MNUMYR)) * MMBPD_TO_BTONS;
LFMMOUT_FEEDSTOCKS('3_M4','07_M10',MNUMYR) =
    sum((DomRefReg,CoalStr),
        repStreamFlows(DomRefReg,CoalStr,'Coal','Feedstocks','NonPetroleumFossil',t) * repStreamProperties(CoalStr,'Coal','Feedstocks','NonPetroleumFossil','SPG') *
        tMNUM(t,MNUMYR)) * MMBPD_TO_BTONS;
LFMMOUT_FEEDSTOCKS('3_M4','08_M10',MNUMYR) =
    sum((DomRefReg,GrainCrops),
        repStreamFlows(DomRefReg,GrainCrops,'Grains','Feedstocks','Biofuels',t) * repStreamProperties(GrainCrops,'Grains','Feedstocks','Biofuels','SPG') *
        tMNUM(t,MNUMYR)) / 0.803563913 * 62.4 / 2000 * 365 / 1000 ;
LFMMOUT_FEEDSTOCKS('3_M4','09_M10',MNUMYR) =
    sum((DomRefReg,BioStr),
        repStreamFlows(DomRefReg,BioStr,'Cellulose','Feedstocks','Biofuels',t) *
        tMNUM(t,MNUMYR)) * 365 / 1000 ;
LFMMOUT_FEEDSTOCKS('3_M4','10_M10',MNUMYR) =
    sum((DomRefReg,RenewableOils),
        repStreamFlows(DomRefReg,RenewableOils,'Oils','Feedstocks','Biofuels',t) * repStreamProperties(RenewableOils,'Oils','Feedstocks','Biofuels','SPG') *
        tMNUM(t,MNUMYR)) * MMBPD_TO_BTONS;

LFMMOUT_INTERMEDIATE('3_M4','01_M13',MNUMYR) =
    sum((DomRefReg,PetNaphtha),
        (repStreamFlows(DomRefReg,PetNaphtha,'CBOB','Products','Petroleum',t) * repStreamProperties(PetNaphtha,'CBOB','Products','Petroleum','SPG') +
        repStreamFlows(DomRefReg,PetNaphtha,'RBOB','Products','Petroleum',t) * repStreamProperties(PetNaphtha,'RBOB','Products','Petroleum','SPG') +
        repStreamFlows(DomRefReg,PetNaphtha,'CaRBOB','Products','Petroleum',t) * repStreamProperties(PetNaphtha,'CaRBOB','Products','Petroleum','SPG')) *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_INTERMEDIATE('3_M4','02_M13',MNUMYR) =
    sum((DomRefReg,NPFNaphtha),
        (repStreamFlows(DomRefReg,NPFNaphtha,'CBOB','Products','NonPetroleumFossil',t) * repStreamProperties(NPFNaphtha,'CBOB','Products','NonPetroleumFossil','SPG') +
        repStreamFlows(DomRefReg,NPFNaphtha,'RBOB','Products','NonPetroleumFossil',t) * repStreamProperties(NPFNaphtha,'RBOB','Products','NonPetroleumFossil','SPG') +
        repStreamFlows(DomRefReg,NPFNaphtha,'CaRBOB','Products','NonPetroleumFossil',t) * repStreamProperties(NPFNaphtha,'CaRBOB','Products','NonPetroleumFossil','SPG')) *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_INTERMEDIATE('3_M4','03_M13',MNUMYR) =
    sum((DomRefReg,BioNaphtha),
        (repStreamFlows(DomRefReg,BioNaphtha,'CBOB','Products','Biofuels',t) * repStreamProperties(BioNaphtha,'CBOB','Products','Biofuels','SPG') +
        repStreamFlows(DomRefReg,BioNaphtha,'RBOB','Products','Biofuels',t) * repStreamProperties(BioNaphtha,'RBOB','Products','Biofuels','SPG') +
        repStreamFlows(DomRefReg,BioNaphtha,'CaRBOB','Products','Biofuels',t) * repStreamProperties(BioNaphtha,'CaRBOB','Products','Biofuels','SPG')) *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_INTERMEDIATE('3_M4','04_M13',MNUMYR) =
    sum((DomRefReg,EthStream),
        repStreamFlows(DomRefReg,EthStream,'Ethanol','Intermediates','Biofuels',t) * repStreamProperties(EthStream,'Ethanol','Products','Biofuels','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS / 1000 ;
LFMMOUT_INTERMEDIATE('3_M4','05_M13',MNUMYR) =
    sum((DomRefReg,PetDistillate),
        (repStreamFlows(DomRefReg,PetDistillate,'ULSD','Products','Petroleum',t) * repStreamProperties(PetDistillate,'ULSD','Products','Petroleum','SPG') +
        repStreamFlows(DomRefReg,PetDistillate,'LowSulfurDiesel','Products','Petroleum',t) * repStreamProperties(PetDistillate,'LowSulfurDiesel','Products','Petroleum','SPG') +
        repStreamFlows(DomRefReg,PetDistillate,'CarbULSD','Products','Petroleum',t) * repStreamProperties(PetDistillate,'CarbULSD','Products','Petroleum','SPG') +
        repStreamFlows(DomRefReg,PetDistillate,'HeatingOil','Products','Petroleum',t) * repStreamProperties(PetDistillate,'HeatingOil','Products','Petroleum','SPG') +
        repStreamFlows(DomRefReg,PetDistillate,'JetFuel','Products','Petroleum',t) * repStreamProperties(PetDistillate,'JetFuel','Products','Petroleum','SPG')) *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_INTERMEDIATE('3_M4','06_M13',MNUMYR) =
    sum((DomRefReg,NPFDistillate),
        (repStreamFlows(DomRefReg,NPFDistillate,'ULSD','Products','NonPetroleumFossil',t) * repStreamProperties(NPFDistillate,'ULSD','Products','NonPetroleumFossil','SPG') +
        repStreamFlows(DomRefReg,NPFDistillate,'LowSulfurDiesel','Products','NonPetroleumFossil',t) * repStreamProperties(NPFDistillate,'LowSulfurDiesel','Products','NonPetroleumFossil','SPG') +
        repStreamFlows(DomRefReg,NPFDistillate,'CarbULSD','Products','NonPetroleumFossil',t) * repStreamProperties(NPFDistillate,'CarbULSD','Products','NonPetroleumFossil','SPG') +
        repStreamFlows(DomRefReg,NPFDistillate,'HeatingOil','Products','NonPetroleumFossil',t) * repStreamProperties(NPFDistillate,'HeatingOil','Products','NonPetroleumFossil','SPG') +
        repStreamFlows(DomRefReg,NPFDistillate,'JetFuel','Products','NonPetroleumFossil',t) * repStreamProperties(NPFDistillate,'JetFuel','Products','NonPetroleumFossil','SPG')) *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_INTERMEDIATE('3_M4','07_M13',MNUMYR) =
    sum((DomRefReg,BioDistillate),
        (repStreamFlows(DomRefReg,BioDistillate,'ULSD','Products','Biofuels',t) * repStreamProperties(BioDistillate,'ULSD','Products','Biofuels','SPG') +
        repStreamFlows(DomRefReg,BioDistillate,'LowSulfurDiesel','Products','Biofuels',t) * repStreamProperties(BioDistillate,'LowSulfurDiesel','Products','Biofuels','SPG') +
        repStreamFlows(DomRefReg,BioDistillate,'CarbULSD','Products','Biofuels',t) * repStreamProperties(BioDistillate,'CarbULSD','Products','Biofuels','SPG') +
        repStreamFlows(DomRefReg,BioDistillate,'HeatingOil','Products','Biofuels',t) * repStreamProperties(BioDistillate,'HeatingOil','Products','Biofuels','SPG') +
        repStreamFlows(DomRefReg,BioDistillate,'JetFuel','Products','Biofuels',t) * repStreamProperties(BioDistillate,'JetFuel','Products','Biofuels','SPG')) *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_INTERMEDIATE('3_M4','08_M13',MNUMYR) =
    sum((DomRefReg,PetResid),
        repStreamFlows(DomRefReg,PetResid,'Resid','Intermediates','Petroleum',t) * repStreamProperties(PetResid,'Resid','Products','Petroleum','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_INTERMEDIATE('3_M4','09_M13',MNUMYR) =
    sum((DomRefReg,LiquidChem),
        repStreamFlows(DomRefReg,LiquidChem,'Chemicals','Products','Petroleum',t) * repStreamProperties(LiquidChem,'Chemicals','Products','Blending','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_INTERMEDIATE('3_M4','10_M13',MNUMYR) =
    sum((DomRefReg,LiquidChem),
        repStreamFlows(DomRefReg,LiquidChem,'Chemicals','Products','NonPetroleumFossil',t) * repStreamProperties(LiquidChem,'Chemicals','Products','Blending','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_INTERMEDIATE('3_M4','11_M13',MNUMYR) =
    sum((DomRefReg,LiquidChem),
        repStreamFlows(DomRefReg,'GLYout','Chemicals','Products','Biofuels',t) * repStreamProperties('GLYout','Chemicals','Products','Blending','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_INTERMEDIATE('3_M4','12_M13',MNUMYR) =
    sum((DomRefReg,Solids),
        repStreamFlows(DomRefReg,Solids,'Chemicals','Products','Petroleum',t) * repStreamProperties(Solids,'Chemicals','Products','Petroleum','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_INTERMEDIATE('3_M4','13_M13',MNUMYR) =
    sum((DomRefReg,Biosolids),
        repStreamFlows(DomRefReg,Biosolids,'Chemicals','Products','Biofuels',t) * repStreamProperties(Biosolids,'Chemicals','Products','Biofuels','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;


LFMMOUT_REFINE_PROD('3_M4','01_M11',MNUMYR) =
    LFMMOUT_INTERMEDIATE('3_M4','01_M13',MNUMYR) +
    LFMMOUT_INTERMEDIATE('3_M4','02_M13',MNUMYR) +
    LFMMOUT_INTERMEDIATE('3_M4','03_M13',MNUMYR) ;
LFMMOUT_REFINE_PROD('3_M4','02_M11',MNUMYR) =
    LFMMOUT_INTERMEDIATE('3_M4','04_M13',MNUMYR) * 1.0249 ;
LFMMOUT_REFINE_PROD('3_M4','03_M11',MNUMYR) =
    sum((DomRefReg,IntStream,Industry),
        repStreamFlows(DomRefReg,IntStream,'JetFuel','Products',Industry,t) * repStreamProperties(IntStream,'JetFuel','Products','Petroleum','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_REFINE_PROD('3_M4','04_M11',MNUMYR) =
    sum((DomRefReg,IntStream,Industry),
        repStreamFlows(DomRefReg,IntStream,'ULSD','Products',Industry,t) * repStreamProperties(IntStream,'ULSD','Products','Petroleum','SPG') +
        repStreamFlows(DomRefReg,IntStream,'LowSulfurDiesel','Products',Industry,t) * repStreamProperties(IntStream,'LowSulfurDiesel','Products','Petroleum','SPG') +
        repStreamFlows(DomRefReg,IntStream,'CarbULSD','Products',Industry,t) * repStreamProperties(IntStream,'CarbULSD','Products','Petroleum','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_REFINE_PROD('3_M4','05_M11',MNUMYR) =
    sum((DomRefReg,IntStream,Industry),
        repStreamFlows(DomRefReg,IntStream,'HeatingOil','Products',Industry,t) * repStreamProperties(IntStream,'HeatingOil','Products','Petroleum','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_REFINE_PROD('3_M4','06_M11',MNUMYR) =
    LFMMOUT_INTERMEDIATE('3_M4','08_M13',MNUMYR) ;
LFMMOUT_REFINE_PROD('3_M4','07_M11',MNUMYR) =
    sum((DomRefReg,LiquefiedGases,Industry),
        repStreamFlows(DomRefReg,LiquefiedGases,'Chemicals','Products',Industry,t) * repStreamProperties(LiquefiedGases,'Chemicals','Products','Blending','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_REFINE_PROD('3_M4','08_M11',MNUMYR) =
    sum((DomRefReg,RTLiquids,Industry),
        repStreamFlows(DomRefReg,RTLiquids,'Chemicals','Products',Industry,t) * repStreamProperties(RTLiquids,'Chemicals','Products','Petroleum','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_REFINE_PROD('3_M4','09_M11',MNUMYR) =
    LFMMOUT_INTERMEDIATE('3_M4','12_M13',MNUMYR) ;
LFMMOUT_REFINE_PROD('3_M4','10_M11',MNUMYR) =
    LFMMOUT_INTERMEDIATE('3_M4','13_M13',MNUMYR) ;


LFMMOUT_GROSS_IMPORT('3_M4','01_M11',MNUMYR) =
    sum((DomRefReg,CRBOB),
        repStreamFlows(DomRefReg,CRBOB,'BOBImports','Products','Petroleum',t) *
            (0.9 * repStreamProperties('CBOB','Gasoline','Products','Blending','SPG')  +
             0.1 * repStreamProperties('RBOB','Gasoline','Products','Blending','SPG')) *
             TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_GROSS_IMPORT('3_M4','02_M11',MNUMYR) =
    sum(DomRefReg,
        repStreamFlows(DomRefReg,'ETHAETbrz','EthanolImports','Products','Biofuels',t) * repStreamProperties('ETHAETbrz','Ethanol','Products','Biofuels','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_GROSS_IMPORT('3_M4','03_M11',MNUMYR) =
    sum((DomRefReg,Gasoline,Industry),
        repStreamFlows(DomRefReg,Gasoline,'GasolineImports','Products',Industry,t) * repStreamProperties(Gasoline,'Gasoline','EndProducts','Blending','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_GROSS_IMPORT('3_M4','04_M11',MNUMYR) =
    sum((DomRefReg,Industry),
        repStreamFlows(DomRefReg,'JTAout','DistillateImports','Products',Industry,t) * repStreamProperties('JTAout','Distillate','EndProducts','Blending','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_GROSS_IMPORT('3_M4','05_M11',MNUMYR) =
    sum((DomRefReg,Diesel,Industry),
        repStreamFlows(DomRefReg,Diesel,'DistillateImports','Products',Industry,t) * repStreamProperties(Diesel,'Distillate','EndProducts','Blending','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_GROSS_IMPORT('3_M4','06_M11',MNUMYR) =
    sum((DomRefReg,Industry),
        repStreamFlows(DomRefReg,'N2Hout','DistillateImports','Products',Industry,t) * repStreamProperties('N2Hout','Distillate','EndProducts','Blending','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_GROSS_IMPORT('3_M4','07_M11',MNUMYR) =
    sum((DomRefReg,ResidFuel),
        repStreamFlows(DomRefReg,ResidFuel,'ResidImports','Products','Petroleum',t) * repStreamProperties(ResidFuel,'ResidFuel','EndProducts','Blending','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_GROSS_IMPORT('3_M4','08_M11',MNUMYR) =
    sum((DomRefReg,LiquefiedGases),
        repStreamFlows(DomRefReg,LiquefiedGases,'ChemicalImports','Products','Blending',t) * repStreamProperties(LiquefiedGases,'Chemicals','Products','Blending','SPG') *
        tMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_GROSS_IMPORT('3_M4','09_M11',MNUMYR) =
    sum((DomRefReg,RTLiquids),
        repStreamFlows(DomRefReg,RTLiquids,'ChemicalImports','Products','Blending',t) * repStreamProperties(RTLiquids,'Chemicals','Products','Blending','SPG') *
        tMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_GROSS_IMPORT('3_M4','10_M11',MNUMYR) =
    sum((DomRefReg,Solids),
        repStreamFlows(DomRefReg,Solids,'ChemicalImports','Products','Blending',t) * repStreamProperties(Solids,'Chemicals','Products','Blending','SPG') *
        tMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;

LFMMOUT_GROSS_Export('3_M4','01_M12',MNUMYR) =
    sum((DomRefReg,CRBOB),
        repStreamFlows(DomRefReg,CRBOB,'BOBExports','Products','Petroleum',t) *
            (0.9 * repStreamProperties('CBOB','Gasoline','Products','Blending','SPG')  +
             0.1 * repStreamProperties('RBOB','Gasoline','Products','Blending','SPG')) *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_GROSS_Export('3_M4','02_M12',MNUMYR) =
    sum(DomRefReg,
        repStreamFlows(DomRefReg,'ETHCRNexp','EthanolExports','Products','Biofuels',t) * repStreamProperties('ETHCRNexp','Ethanol','Intermediates','Biofuels','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_GROSS_Export('3_M4','03_M12',MNUMYR) =
    sum((DomRefReg,Gasoline,Industry),
        repStreamFlows(DomRefReg,Gasoline,'GasolineExports','Products',Industry,t) * repStreamProperties('CFGout','Gasoline','EndProducts','Blending','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_GROSS_Export('3_M4','04_M12',MNUMYR) =
    sum((DomRefReg,Industry),
        repStreamFlows(DomRefReg,'JTAout','DistillateExports','Products',Industry,t) * repStreamProperties('JTAout','Distillate','EndProducts','Blending','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_GROSS_Export('3_M4','05_M12',MNUMYR) =
    sum((DomRefReg,Diesel,Industry),
        repStreamFlows(DomRefReg,Diesel,'DistillateExports','Products',Industry,t) *
            (repStreamProperties('DSUout','Distillate','EndProducts','Blending','SPG')  +
             repStreamProperties('DSLout','Distillate','EndProducts','Blending','SPG')  +
             repStreamProperties('CarbDSUout','Distillate','EndProducts','Blending','SPG')) / 3 *
        TMNUM(t,MNUMYR))  * MMBPD_TO_BTONS ;
LFMMOUT_GROSS_Export('3_M4','06_M12',MNUMYR) =
    sum((DomRefReg,Industry),
        repStreamFlows(DomRefReg,'N2Hout','DistillateExports','Products',Industry,t) * repStreamProperties('N2Hout','Distillate','EndProducts','Blending','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_GROSS_Export('3_M4','07_M12',MNUMYR) =
    sum((DomRefReg,ResidFuel),
        repStreamFlows(DomRefReg,ResidFuel,'ResidExports','Products','Petroleum',t) *
            (repStreamProperties('N6Bout','ResidFuel','EndProducts','Blending','SPG') +
             repStreamProperties('N6Iout','ResidFuel','EndProducts','Blending','SPG')) / 2 *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_GROSS_Export('3_M4','08_M12',MNUMYR) =
    sum((DomRefReg,LiquefiedGases),
        repStreamFlows(DomRefReg,LiquefiedGases,'ChemicalExports','Products','Blending',t) * repStreamProperties(LiquefiedGases,'Chemicals','Products','Blending','SPG') *
        tMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_GROSS_Export('3_M4','09_M12',MNUMYR) =
    sum((DomRefReg,RTLiquids),
        repStreamFlows(DomRefReg,RTLiquids,'ChemicalExports','Products','Blending',t) * repStreamProperties(RTLiquids,'Chemicals','Products','Blending','SPG') *
        tMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_GROSS_Export('3_M4','10_M12',MNUMYR) =
    sum((DomRefReg,Solids),
        repStreamFlows(DomRefReg,Solids,'ChemicalExports','Products','Blending',t) * repStreamProperties(Solids,'Chemicals','Products','Blending','SPG') *
        tMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;

LFMMOUT_DOM_CONSUME('3_M4','01_M12',MNUMYR) =
    sum(DomRefReg,
        repStreamFlows(DomRefReg,'LPGout','Chemicals','EndProducts','Blending',t) * repStreamProperties('LPGout','Chemicals','Products','Blending','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_DOM_CONSUME('3_M4','02_M12',MNUMYR) =
    sum((DomRefReg,E10,Industry),repStreamFlows(DomRefReg,E10,'Gasoline','EndProducts',Industry,t) *
        (repStreamProperties('CFGout','Gasoline','EndProducts','Blending','SPG') +
        repStreamProperties('RFGout','Gasoline','EndProducts','Blending','SPG')  +
        repStreamProperties('CaRBOBout','Gasoline','EndProducts','Blending','SPG')) / 3 *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_DOM_CONSUME('3_M4','03_M12',MNUMYR) =
    sum((DomRefReg,E15,Industry),repStreamFlows(DomRefReg,E15,'Gasoline','EndProducts',Industry,t) *
        (repStreamProperties('CFG15out','Gasoline','EndProducts','Blending','SPG') +
        repStreamProperties('RFG15out','Gasoline','EndProducts','Blending','SPG')) / 2 *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_DOM_CONSUME('3_M4','04_M12',MNUMYR) =
    sum((DomRefReg,Industry),repStreamFlows(DomRefReg,'E85out','Gasoline','EndProducts',Industry,t) * repStreamProperties('E85out','Gasoline','EndProducts','Blending','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_DOM_CONSUME('3_M4','05_M12',MNUMYR) =
    sum((DomRefReg,Industry),repStreamFlows(DomRefReg,'JTAout','Distillate','EndProducts',Industry,t) * repStreamProperties('JTAout','Distillate','EndProducts','Blending','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_DOM_CONSUME('3_M4','06_M12',MNUMYR) =
    sum((DomRefReg,Diesel,Industry),
        repStreamFlows(DomRefReg,Diesel,'Distillate','EndProducts',Industry,t) *
            (repStreamProperties('DSUout','Distillate','EndProducts','Blending','SPG') +
            repStreamProperties('DSLout','Distillate','EndProducts','Blending','SPG')  +
            repStreamProperties('CarbDSUout','Distillate','EndProducts','Blending','SPG')) / 3 *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_DOM_CONSUME('3_M4','07_M12',MNUMYR) =
    sum((DomRefReg,Industry),repStreamFlows(DomRefReg,'N2Hout','Distillate','EndProducts',Industry,t) * repStreamProperties('N2Hout','Distillate','EndProducts','Blending','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_DOM_CONSUME('3_M4','08_M12',MNUMYR) =
    sum((DomRefReg,ResidFuel, Industry),repStreamFlows(DomRefReg,ResidFuel,'ResidFuel','EndProducts',Industry,t) *
        (repStreamProperties('N6Bout','ResidFuel','EndProducts','Blending','SPG') +
        repStreamProperties('N6Iout','ResidFuel','EndProducts','Blending','SPG')) / 2 *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_DOM_CONSUME('3_M4','09_M12',MNUMYR) =
    sum((DomRefReg,LiquidChem),
        repStreamFlows(DomRefReg,LiquidChem,'Chemicals','EndProducts','Blending',t) * repStreamProperties(LiquidChem,'Chemicals','Products','Blending','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS -
    sum(DomRefReg,
        repStreamFlows(DomRefReg,'LPGout','Chemicals','EndProducts','Blending',t) * repStreamProperties('LPGout','Chemicals','Products','Blending','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_DOM_CONSUME('3_M4','10_M12',MNUMYR) =
    sum((DomRefReg,Solids),repStreamFlows(DomRefReg,Solids,'Chemicals','EndProducts','Blending',t) * repStreamProperties(Solids,'Chemicals','Products','Blending','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS -
    sum(DomRefReg,repStreamFlows(DomRefReg,'COKout','Chemicals','EndProducts','Blending',t) * repStreamProperties('COKout','Chemicals','EndProducts','Blending','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS -
    sum(DomRefReg,repStreamFlows(DomRefReg,'COKdump','Chemicals','EndProducts','Blending',t) * repStreamProperties('COKdump','Chemicals','EndProducts','Blending','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;
LFMMOUT_DOM_CONSUME('3_M4','11_M12',MNUMYR) =
    sum(DomRefReg,repStreamFlows(DomRefReg,'DDGout','NonfuelStreams','Products','Biofuels',t) * repStreamProperties('DDGout','NonFuelStreams','Products','Biofuels','SPG') *
        TMNUM(t,MNUMYR)) / (35/2000) * 365 / 1000 +
    sum(DomRefReg,repStreamFlows(DomRefReg,'DGSout','NonfuelStreams','Products','Biofuels',t) * repStreamProperties('DGSout','NonFuelStreams','Products','Biofuels','SPG') *
        TMNUM(t,MNUMYR)) / (35/2000) * 365 / 1000 ;
LFMMOUT_DOM_CONSUME('3_M4','12_M12',MNUMYR) =
    sum(DomRefReg,repStreamFlows(DomRefReg,'COKout','Chemicals','EndProducts','Blending',t) * repStreamProperties('COKout','Chemicals','Products','Blending','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS +
    sum(DomRefReg,repStreamFlows(DomRefReg,'COKdump','Chemicals','EndProducts','Blending',t) * repStreamProperties('COKdump','Chemicals','Products','Blending','SPG') *
        TMNUM(t,MNUMYR)) * MMBPD_TO_BTONS ;

*Energy units (quadrillion btu)
LFMMOUT_FEEDSTOCKS('4_M4','01_M10',MNUMYR) =
    sum((DomRefReg,Crude),
        repStreamFlows(DomRefReg,Crude,'DomesticCrude','Feedstocks','Petroleum',t) * repStreamProperties(Crude,'Crude','Feedstocks','Petroleum','END') *
        tMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_FEEDSTOCKS('4_M4','02_M10',MNUMYR) =
    sum((DomRefReg,Crude),
        repStreamFlows(DomRefReg,Crude,'CrudeImports','Feedstocks','Petroleum',t) * repStreamProperties(Crude,'Crude','Feedstocks','Petroleum','END') *
        tMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_FEEDSTOCKS('4_M4','03_M10',MNUMYR) =
    sum((DomRefReg,UnfinishedOils),
        repStreamFlows(DomRefReg,UnfinishedOils,'UFOImports','Feedstocks','Petroleum',t) * repStreamProperties(UnfinishedOils,'UFOImports','Feedstocks','Petroleum','END') *
        tMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_FEEDSTOCKS('4_M4','04_M10',MNUMYR) =
    sum((DomRefReg,NGLInputStr),
        repStreamFlows(DomRefReg,NGLInputStr,'DomesticNGPL','Feedstocks','NonPetroleumFossil',t) * repStreamProperties(NGLInputStr,'GasLiquids','Feedstocks','NonPetroleumFossil','END') *
        tMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_FEEDSTOCKS('4_M4','05_M10',MNUMYR) =
    sum((DomRefReg,NGLProduct),
        repStreamFlows(DomRefReg,NGLProduct,'NGPLImports','Feedstocks','NonPetroleumFossil',t) * repStreamProperties(NGLProduct,'GasLiquids','Feedstocks','NonPetroleumFossil','END') *
        tMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_FEEDSTOCKS('4_M4','06_M10',MNUMYR) =
    sum((DomRefReg,Gas),
        repStreamFlows(DomRefReg,Gas,'GasFeed','Feedstocks','NonPetroleumFossil',t) * repStreamProperties(Gas,'Gas','Feedstocks','NonPetroleumFossil','END') *
        tMNUM(t,MNUMYR)) / 1000 ;
LFMMOUT_FEEDSTOCKS('4_M4','07_M10',MNUMYR) =
    sum((DomRefReg,CoalStr),
        repStreamFlows(DomRefReg,CoalStr,'Coal','Feedstocks','NonPetroleumFossil',t) * repStreamProperties(CoalStr,'Coal','Feedstocks','NonPetroleumFossil','END') *
        tMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_FEEDSTOCKS('4_M4','08_M10',MNUMYR) =
    sum((DomRefReg,GrainCrops),
        repStreamFlows(DomRefReg,GrainCrops,'Grains','Feedstocks','Biofuels',t) * repStreamProperties(GrainCrops,'Grains','Feedstocks','Biofuels','END') *
        tMNUM(t,MNUMYR)) / 1000 ;
LFMMOUT_FEEDSTOCKS('4_M4','09_M10',MNUMYR) =
    sum((DomRefReg,BioStr),
        repStreamFlows(DomRefReg,BioStr,'Cellulose','Feedstocks','Biofuels',t) *
        tMNUM(t,MNUMYR)) / 1000000 ;
LFMMOUT_FEEDSTOCKS('4_M4','10_M10',MNUMYR) =
    sum((DomRefReg,RenewableOils),
        repStreamFlows(DomRefReg,RenewableOils,'Oils','Feedstocks','Biofuels',t) * repStreamProperties(RenewableOils,'Oils','Feedstocks','Biofuels','END') *
        tMNUM(t,MNUMYR)) / 1000 ;

LFMMOUT_INTERMEDIATE('4_M4','01_M13',MNUMYR) =
    sum((DomRefReg,PetNaphtha),
        (repStreamFlows(DomRefReg,PetNaphtha,'CBOB','Products','Petroleum',t) * repStreamProperties(PetNaphtha,'CBOB','Products','Petroleum','END') +
         repStreamFlows(DomRefReg,PetNaphtha,'RBOB','Products','Petroleum',t) * repStreamProperties(PetNaphtha,'RBOB','Products','Petroleum','END') +
         repStreamFlows(DomRefReg,PetNaphtha,'CarBOB','Products','Petroleum',t) * repStreamProperties(PetNaphtha,'CarBOB','Products','Petroleum','END')) *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_INTERMEDIATE('4_M4','02_M13',MNUMYR) =
    sum((DomRefReg,NPFNaphtha),
        (repStreamFlows(DomRefReg,NPFNaphtha,'CBOB','Products','NonPetroleumFossil',t) * repStreamProperties(NPFNaphtha,'CBOB','Products','NonPetroleumFossil','END') +
         repStreamFlows(DomRefReg,NPFNaphtha,'RBOB','Products','NonPetroleumFossil',t) * repStreamProperties(NPFNaphtha,'RBOB','Products','NonPetroleumFossil','END') +
         repStreamFlows(DomRefReg,NPFNaphtha,'CaRBOB','Products','NonPetroleumFossil',t) * repStreamProperties(NPFNaphtha,'CaRBOB','Products','NonPetroleumFossil','END')) *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_INTERMEDIATE('4_M4','03_M13',MNUMYR) =
    sum((DomRefReg,BioNaphtha),
        (repStreamFlows(DomRefReg,BioNaphtha,'CBOB','Products','Biofuels',t) * repStreamProperties(BioNaphtha,'CBOB','Products','Biofuels','END') +
        repStreamFlows(DomRefReg,BioNaphtha,'RBOB','Products','Biofuels',t) * repStreamProperties(BioNaphtha,'RBOB','Products','Biofuels','END') +
        repStreamFlows(DomRefReg,BioNaphtha,'CaRBOB','Products','Biofuels',t) * repStreamProperties(BioNaphtha,'CaRBOB','Products','Biofuels','END')) *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_INTERMEDIATE('4_M4','04_M13',MNUMYR) =
    sum((DomRefReg,EthStream),
        repStreamFlows(DomRefReg,EthStream,'Ethanol','Intermediates','Biofuels',t) * repStreamProperties(EthStream,'Ethanol','Products','Biofuels','END') *
        TMNUM(t,MNUMYR)) / 1000000 * 365  ;
LFMMOUT_INTERMEDIATE('4_M4','05_M13',MNUMYR) =
    sum((DomRefReg,PetDistillate),
        (repStreamFlows(DomRefReg,PetDistillate,'ULSD','Products','Petroleum',t) * repStreamProperties(PetDistillate,'ULSD','Products','Petroleum','END') +
        repStreamFlows(DomRefReg,PetDistillate,'LowSulfurDiesel','Products','Petroleum',t) * repStreamProperties(PetDistillate,'LowSulfurDiesel','Products','Petroleum','END') +
        repStreamFlows(DomRefReg,PetDistillate,'CarbULSD','Products','Petroleum',t) * repStreamProperties(PetDistillate,'CarbULSD','Products','Petroleum','END') +
        repStreamFlows(DomRefReg,PetDistillate,'HeatingOil','Products','Petroleum',t) * repStreamProperties(PetDistillate,'HeatingOil','Products','Petroleum','END') +
        repStreamFlows(DomRefReg,PetDistillate,'JetFuel','Products','Petroleum',t) * repStreamProperties(PetDistillate,'JetFuel','Products','Petroleum','END')) *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_INTERMEDIATE('4_M4','06_M13',MNUMYR) =
    sum((DomRefReg,NPFDistillate),
        (repStreamFlows(DomRefReg,NPFDistillate,'ULSD','Products','NonPetroleumFossil',t) * repStreamProperties(NPFDistillate,'ULSD','Products','NonPetroleumFossil','END') +
        repStreamFlows(DomRefReg,NPFDistillate,'LowSulfurDiesel','Products','NonPetroleumFossil',t) * repStreamProperties(NPFDistillate,'LowSulfurDiesel','Products','NonPetroleumFossil','END') +
        repStreamFlows(DomRefReg,NPFDistillate,'CarbULSD','Products','NonPetroleumFossil',t) * repStreamProperties(NPFDistillate,'CarbULSD','Products','NonPetroleumFossil','END') +
        repStreamFlows(DomRefReg,NPFDistillate,'HeatingOil','Products','NonPetroleumFossil',t) * repStreamProperties(NPFDistillate,'HeatingOil','Products','NonPetroleumFossil','END') +
        repStreamFlows(DomRefReg,NPFDistillate,'JetFuel','Products','NonPetroleumFossil',t) * repStreamProperties(NPFDistillate,'JetFuel','Products','NonPetroleumFossil','END')) *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_INTERMEDIATE('4_M4','07_M13',MNUMYR) =
    sum((DomRefReg,BioDistillate),
        (repStreamFlows(DomRefReg,BioDistillate,'ULSD','Products','Biofuels',t) * repStreamProperties(BioDistillate,'ULSD','Products','Biofuels','END') +
        repStreamFlows(DomRefReg,BioDistillate,'LowSulfurDiesel','Products','Biofuels',t) * repStreamProperties(BioDistillate,'LowSulfurDiesel','Products','Biofuels','END') +
        repStreamFlows(DomRefReg,BioDistillate,'CarbULSD','Products','Biofuels',t) * repStreamProperties(BioDistillate,'CarbULSD','Products','Biofuels','END') +
        repStreamFlows(DomRefReg,BioDistillate,'HeatingOil','Products','Biofuels',t) * repStreamProperties(BioDistillate,'HeatingOil','Products','Biofuels','END') +
        repStreamFlows(DomRefReg,BioDistillate,'JetFuel','Products','Biofuels',t) * repStreamProperties(BioDistillate,'JetFuel','Products','Biofuels','END')) *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_INTERMEDIATE('4_M4','08_M13',MNUMYR) =
    sum((DomRefReg,PetResid),
        repStreamFlows(DomRefReg,PetResid,'Resid','Intermediates','Petroleum',t) * repStreamProperties(PetResid,'Resid','Products','Petroleum','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_INTERMEDIATE('4_M4','09_M13',MNUMYR) =
    sum((DomRefReg,LiquidChem),
        repStreamFlows(DomRefReg,LiquidChem,'Chemicals','Products','Petroleum',t) * repStreamProperties(LiquidChem,'Chemicals','Products','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_INTERMEDIATE('4_M4','10_M13',MNUMYR) =
    sum((DomRefReg,LiquidChem),
        repStreamFlows(DomRefReg,LiquidChem,'Chemicals','Products','NonPetroleumFossil',t) * repStreamProperties(LiquidChem,'Chemicals','Products','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_INTERMEDIATE('4_M4','11_M13',MNUMYR) =
    sum((DomRefReg,LiquidChem),
        repStreamFlows(DomRefReg,'GLYout','Chemicals','Products','Biofuels',t) * repStreamProperties('GLYout','Chemicals','Products','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_INTERMEDIATE('4_M4','12_M13',MNUMYR) =
    sum((DomRefReg,Solids),
        repStreamFlows(DomRefReg,Solids,'Chemicals','Products','Petroleum',t) * repStreamProperties(Solids,'Chemicals','Products','Petroleum','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_INTERMEDIATE('4_M4','13_M13',MNUMYR) =
    sum((DomRefReg,Biosolids),
        repStreamFlows(DomRefReg,Biosolids,'Chemicals','Products','Biofuels',t) * repStreamProperties(Biosolids,'Chemicals','Products','Biofuels','END') *
        TMNUM(t,MNUMYR)) / 1000 ;


LFMMOUT_REFINE_PROD('4_M4','01_M11',MNUMYR) =
    LFMMOUT_INTERMEDIATE('4_M4','01_M13',MNUMYR) +
    LFMMOUT_INTERMEDIATE('4_M4','02_M13',MNUMYR) +
    LFMMOUT_INTERMEDIATE('4_M4','03_M13',MNUMYR) ;
LFMMOUT_REFINE_PROD('4_M4','02_M11',MNUMYR) =
    LFMMOUT_INTERMEDIATE('4_M4','04_M13',MNUMYR) * 1.0249 ;
LFMMOUT_REFINE_PROD('4_M4','03_M11',MNUMYR) =
    sum((DomRefReg,IntStream,Industry),
        repStreamFlows(DomRefReg,IntStream,'JetFuel','Products',Industry,t) * repStreamProperties(IntStream,'JetFuel','Products','Petroleum','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_REFINE_PROD('4_M4','04_M11',MNUMYR) =
    sum((DomRefReg,IntStream,Industry),
        repStreamFlows(DomRefReg,IntStream,'ULSD','Products',Industry,t) * repStreamProperties(IntStream,'ULSD','Products','Petroleum','END') +
        repStreamFlows(DomRefReg,IntStream,'LowSulfurDiesel','Products',Industry,t) * repStreamProperties(IntStream,'LowSulfurDiesel','Products','Petroleum','END') +
        repStreamFlows(DomRefReg,IntStream,'CarbULSD','Products',Industry,t) * repStreamProperties(IntStream,'CarbULSD','Products','Petroleum','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_REFINE_PROD('4_M4','05_M11',MNUMYR) =
    sum((DomRefReg,IntStream,Industry),
        repStreamFlows(DomRefReg,IntStream,'HeatingOil','Products',Industry,t) * repStreamProperties(IntStream,'HeatingOil','Products','Petroleum','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_REFINE_PROD('4_M4','06_M11',MNUMYR) =
    LFMMOUT_INTERMEDIATE('4_M4','08_M13',MNUMYR) ;
LFMMOUT_REFINE_PROD('4_M4','07_M11',MNUMYR) =
    sum((DomRefReg,LiquefiedGases,Industry),
        repStreamFlows(DomRefReg,LiquefiedGases,'Chemicals','Products',Industry,t) * repStreamProperties(LiquefiedGases,'Chemicals','Products','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_REFINE_PROD('4_M4','08_M11',MNUMYR) =
    sum((DomRefReg,RTLiquids,Industry),
        repStreamFlows(DomRefReg,RTLiquids,'Chemicals','Products',Industry,t) * repStreamProperties(RTLiquids,'Chemicals','Products','Petroleum','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_REFINE_PROD('4_M4','09_M11',MNUMYR) =
    LFMMOUT_INTERMEDIATE('4_M4','12_M13',MNUMYR) ;
LFMMOUT_REFINE_PROD('4_M4','10_M11',MNUMYR) =
    LFMMOUT_INTERMEDIATE('4_M4','13_M13',MNUMYR) ;


LFMMOUT_GROSS_IMPORT('4_M4','01_M11',MNUMYR) =
    sum((DomRefReg,CRBOB),
        repStreamFlows(DomRefReg,CRBOB,'BOBImports','Products','Petroleum',t) *
            (0.9 * repStreamProperties('CBOB','Gasoline','Products','Blending','END')  +
             0.1 * repStreamProperties('RBOB','Gasoline','Products','Blending','END')) *
             TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_GROSS_IMPORT('4_M4','02_M11',MNUMYR) =
    sum(DomRefReg,
        repStreamFlows(DomRefReg,'ETHAETbrz','EthanolImports','Products','Biofuels',t) * repStreamProperties('ETHAETbrz','Ethanol','Products','Biofuels','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_GROSS_IMPORT('4_M4','03_M11',MNUMYR) =
    sum((DomRefReg,Gasoline,Industry),
        repStreamFlows(DomRefReg,Gasoline,'GasolineImports','Products',Industry,t) * repStreamProperties(Gasoline,'Gasoline','EndProducts','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_GROSS_IMPORT('4_M4','04_M11',MNUMYR) =
    sum((DomRefReg,Industry),
        repStreamFlows(DomRefReg,'JTAout','DistillateImports','Products',Industry,t) * repStreamProperties('JTAout','Distillate','EndProducts','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_GROSS_IMPORT('4_M4','05_M11',MNUMYR) =
    sum((DomRefReg,Diesel,Industry),
        repStreamFlows(DomRefReg,Diesel,'DistillateImports','Products',Industry,t) * repStreamProperties(Diesel,'Distillate','EndProducts','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_GROSS_IMPORT('4_M4','06_M11',MNUMYR) =
    sum((DomRefReg,Industry),
        repStreamFlows(DomRefReg,'N2Hout','DistillateImports','Products',Industry,t) * repStreamProperties('N2Hout','Distillate','EndProducts','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_GROSS_IMPORT('4_M4','07_M11',MNUMYR) =
    sum((DomRefReg,ResidFuel),
        repStreamFlows(DomRefReg,ResidFuel,'ResidImports','Products','Petroleum',t) * repStreamProperties(ResidFuel,'ResidFuel','EndProducts','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_GROSS_IMPORT('4_M4','08_M11',MNUMYR) =
    sum((DomRefReg,LiquefiedGases),
        repStreamFlows(DomRefReg,LiquefiedGases,'ChemicalImports','Products','Blending',t) * repStreamProperties(LiquefiedGases,'Chemicals','Products','Blending','END') *
        tMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_GROSS_IMPORT('4_M4','09_M11',MNUMYR) =
    sum((DomRefReg,RTLiquids),
        repStreamFlows(DomRefReg,RTLiquids,'ChemicalImports','Products','Blending',t) * repStreamProperties(RTLiquids,'Chemicals','Products','Blending','END') *
        tMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_GROSS_IMPORT('4_M4','10_M11',MNUMYR) =
    sum((DomRefReg,Solids),
        repStreamFlows(DomRefReg,Solids,'ChemicalImports','Products','Blending',t) * repStreamProperties(Solids,'Chemicals','Products','Blending','END') *
        tMNUM(t,MNUMYR)) / 1000 * 365 ;

LFMMOUT_GROSS_Export('4_M4','01_M12',MNUMYR) =
    sum((DomRefReg,CRBOB),
        repStreamFlows(DomRefReg,CRBOB,'BOBExports','Products','Petroleum',t) *
            (0.9 * repStreamProperties('CBOB','Gasoline','Products','Blending','END')  +
             0.1 * repStreamProperties('RBOB','Gasoline','Products','Blending','END')) *
            TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_GROSS_Export('4_M4','02_M12',MNUMYR) =
    sum((DomRefReg,EthStream),
        repStreamFlows(DomRefReg,EthStream,'EthanolExports','Products','Biofuels',t) * repStreamProperties('ETHAETbrz','Ethanol','Intermediates','Biofuels','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_GROSS_Export('4_M4','03_M12',MNUMYR) =
    sum((DomRefReg,Gasoline,Industry),
        repStreamFlows(DomRefReg,Gasoline,'GasolineExports','Products',Industry,t) * repStreamProperties('CFGout','Gasoline','EndProducts','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_GROSS_Export('4_M4','04_M12',MNUMYR) =
    sum((DomRefReg,Industry),
        repStreamFlows(DomRefReg,'JTAout','DistillateExports','Products',Industry,t) * repStreamProperties('JTAout','Distillate','EndProducts','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_GROSS_Export('4_M4','05_M12',MNUMYR) =
    sum((DomRefReg,Diesel,Industry),
        repStreamFlows(DomRefReg,Diesel,'DistillateExports','Products',Industry,t) *
            (repStreamProperties('DSUout','Distillate','EndProducts','Blending','END')  +
             repStreamProperties('DSLout','Distillate','EndProducts','Blending','END')  +
             repStreamProperties('CarbDSUout','Distillate','EndProducts','Blending','END')) / 3 *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_GROSS_Export('4_M4','06_M12',MNUMYR) =
    sum((DomRefReg,Industry),
        repStreamFlows(DomRefReg,'N2Hout','DistillateExports','Products',Industry,t) * repStreamProperties('N2Hout','Distillate','EndProducts','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_GROSS_Export('4_M4','07_M12',MNUMYR) =
    sum((DomRefReg,ResidFuel),
        repStreamFlows(DomRefReg,ResidFuel,'ResidExports','Products','Petroleum',t) *
            (repStreamProperties('N6Bout','ResidFuel','EndProducts','Blending','END') +
             repStreamProperties('N6Iout','ResidFuel','EndProducts','Blending','END')) / 2 *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_GROSS_Export('4_M4','08_M12',MNUMYR) =
    sum((DomRefReg,LiquefiedGases),
        repStreamFlows(DomRefReg,LiquefiedGases,'ChemicalExports','Products','Blending',t) * repStreamProperties(LiquefiedGases,'Chemicals','Products','Blending','END') *
        tMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_GROSS_Export('4_M4','09_M12',MNUMYR) =
    sum((DomRefReg,RTLiquids),
        repStreamFlows(DomRefReg,RTLiquids,'ChemicalExports','Products','Blending',t) * repStreamProperties(RTLiquids,'Chemicals','Products','Blending','END') *
        tMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_GROSS_Export('4_M4','10_M12',MNUMYR) =
    sum((DomRefReg,Solids),
        repStreamFlows(DomRefReg,Solids,'ChemicalExports','Products','Blending',t) * repStreamProperties(Solids,'Chemicals','Products','Blending','END') *
        tMNUM(t,MNUMYR)) / 1000 * 365 ;

LFMMOUT_DOM_CONSUME('4_M4','01_M12',MNUMYR) =
    sum(DomRefReg,
        repStreamFlows(DomRefReg,'LPGout','Chemicals','EndProducts','Blending',t) * repStreamProperties('LPGout','Chemicals','Products','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_DOM_CONSUME('4_M4','02_M12',MNUMYR) =
    sum((DomRefReg,E10,Industry),repStreamFlows(DomRefReg,E10,'Gasoline','EndProducts',Industry,t) *
        repStreamProperties(E10,'Gasoline','EndProducts','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_DOM_CONSUME('4_M4','03_M12',MNUMYR) =
    sum((DomRefReg,E15,Industry),repStreamFlows(DomRefReg,E15,'Gasoline','EndProducts',Industry,t) *
        repStreamProperties(E15,'Gasoline','EndProducts','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_DOM_CONSUME('4_M4','04_M12',MNUMYR) =
    sum((DomRefReg,Industry),repStreamFlows(DomRefReg,'E85out','Gasoline','EndProducts',Industry,t) * repStreamProperties('E85out','Gasoline','EndProducts','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_DOM_CONSUME('4_M4','05_M12',MNUMYR) =
    sum((DomRefReg,Industry),repStreamFlows(DomRefReg,'JTAout','Distillate','EndProducts',Industry,t) * repStreamProperties('JTAout','Distillate','EndProducts','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_DOM_CONSUME('4_M4','06_M12',MNUMYR) =
    sum((DomRefReg,Diesel,Industry),
        repStreamFlows(DomRefReg,Diesel,'Distillate','EndProducts',Industry,t) *
        repStreamProperties(Diesel,'Distillate','EndProducts','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_DOM_CONSUME('4_M4','07_M12',MNUMYR) =
    sum((DomRefReg,Industry),repStreamFlows(DomRefReg,'N2Hout','Distillate','EndProducts',Industry,t) * repStreamProperties('N2Hout','Distillate','EndProducts','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_DOM_CONSUME('4_M4','08_M12',MNUMYR) =
    sum((DomRefReg,ResidFuel, Industry),repStreamFlows(DomRefReg,ResidFuel,'ResidFuel','EndProducts',Industry,t) *
        (repStreamProperties('N6Bout','ResidFuel','EndProducts','Blending','END') +
        repStreamProperties('N6Iout','ResidFuel','EndProducts','Blending','END')) / 2 *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_DOM_CONSUME('4_M4','09_M12',MNUMYR) =
    sum((DomRefReg,LiquidChem),
        repStreamFlows(DomRefReg,LiquidChem,'Chemicals','EndProducts','Blending',t) * repStreamProperties(LiquidChem,'Chemicals','Products','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 -
    sum(DomRefReg,
        repStreamFlows(DomRefReg,'LPGout','Chemicals','EndProducts','Blending',t) * repStreamProperties('LPGout','Chemicals','Products','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_DOM_CONSUME('4_M4','10_M12',MNUMYR) =
    sum((DomRefReg,Solids),repStreamFlows(DomRefReg,Solids,'Chemicals','EndProducts','Blending',t) * repStreamProperties(Solids,'Chemicals','Products','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 -
    sum(DomRefReg,repStreamFlows(DomRefReg,'COKout','Chemicals','EndProducts','Blending',t) * repStreamProperties('COKout','Chemicals','EndProducts','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 -
    sum(DomRefReg,repStreamFlows(DomRefReg,'COKdump','Chemicals','EndProducts','Blending',t) * repStreamProperties('COKdump','Chemicals','EndProducts','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_DOM_CONSUME('4_M4','11_M12',MNUMYR) =
    sum(DomRefReg,repStreamFlows(DomRefReg,'DDGout','NonfuelStreams','Products','Biofuels',t) * repStreamProperties('DDGout','NonFuelStreams','Products','Biofuels','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 +
    sum(DomRefReg,repStreamFlows(DomRefReg,'DGSout','NonfuelStreams','Products','Biofuels',t) * repStreamProperties('DGSout','NonFuelStreams','Products','Biofuels','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;
LFMMOUT_DOM_CONSUME('4_M4','12_M12',MNUMYR) =
    sum(DomRefReg,repStreamFlows(DomRefReg,'COKout','Chemicals','EndProducts','Blending',t) * repStreamProperties('COKout','Chemicals','Products','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 +
    sum(DomRefReg,repStreamFlows(DomRefReg,'COKdump','Chemicals','EndProducts','Blending',t) * repStreamProperties('COKdump','Chemicals','Products','Blending','END') *
        TMNUM(t,MNUMYR)) / 1000 * 365 ;

  );

**************************** REFINERY PROFITABILITY REPORT ****************************

* Period 1
loop(Per1_t_MNUMYR(Period,t,MNUMYR)$PrcPeriod(Period),

  ProductPrices(GasSpecProd,RefReg,Period)$(sum(RefType,sum(RcpMode$RecipeBlending(RcpMode,GasSpecProd),ToRECIPEBLEND.l(RefReg,RefType,RcpMode,GasSpecProd,Period)) + ExpFromSPECBLEND.l(RefReg,RefType,GasSpecProd,Period))>0) =
     sum(RefType,
       (sum(RcpMode$RecipeBlending(RcpMode,GasSpecProd),ToRECIPEBLEND.l(RefReg,RefType,RcpMode,GasSpecProd,Period)) +
        ExpFromSPECBLEND.l(RefReg,RefType,GasSpecProd,Period)) * GasSpecBalance.m(RefReg,RefType,GasSpecProd,Period) ) /
     sum(RefType,
       sum(RcpMode$RecipeBlending(RcpMode,GasSpecProd),ToRECIPEBLEND.l(RefReg,RefType,RcpMode,GasSpecProd,Period)) +
       ExpFromSPECBLEND.l(RefReg,RefType,GasSpecProd,Period) ) / GDP(t);

  ProductPrices(DistSpecProd,RefReg,Period)$(sum((RefType,RcpMode)$RecipeBlending(RcpMode,DistSpecProd),ToRECIPEBLEND.l(RefReg,RefType,RcpMode,DistSpecProd,Period))>0) =
     sum((RefType,RcpMode)$RecipeBlending(RcpMode,DistSpecProd),
       ToRECIPEBLEND.l(RefReg,RefType,RcpMode,DistSpecProd,Period)*DistSpecBalance.m(RefReg,RefType,DistSpecProd,Period) ) /
     sum((RefType,RcpMode)$RecipeBlending(RcpMode,DistSpecProd),
       ToRECIPEBLEND.l(RefReg,RefType,RcpMode,DistSpecProd,Period) ) / GDP(t);

  ProductPrices(ResidSpecProd,RefReg,Period)$(sum((RefType,RcpMode)$RecipeBlending(RcpMode,ResidSpecProd),ToRECIPEBLEND.l(RefReg,RefType,RcpMode,ResidSpecProd,Period))>0) =
     sum((RefType,RcpMode)$RecipeBlending(RcpMode,ResidSpecProd),
       ToRECIPEBLEND.l(RefReg,RefType,RcpMode,ResidSpecProd,Period)*ResidSpecBalance.m(RefReg,RefType,ResidSpecProd,Period) ) /
     sum((RefType,RcpMode)$RecipeBlending(RcpMode,ResidSpecProd),
       ToRECIPEBLEND.l(RefReg,RefType,RcpMode,ResidSpecProd,Period) ) / GDP(t);

  ProductPrices(Stream,RefReg,Period)$((sum((RefType,Process,ProcessMode)$(ProcessTable(Stream,Process,ProcessMode)>0),ProcessTable(Stream,Process,ProcessMode)*PROCMODE.l(RefReg,RefType,Process,ProcessMode,Period))>0) and (AltFuelStream(Stream) or ImpStr(Stream))) =
     sum((RefType,Process,ProcessMode)$(ProcessTable(Stream,Process,ProcessMode)>0),
       ProcessTable(Stream,Process,ProcessMode)*PROCMODE.l(RefReg,RefType,Process,ProcessMode,Period)*StreamBalance.m(RefReg,RefType,Stream,Period)) /
     sum((RefType,Process,ProcessMode)$(ProcessTable(Stream,Process,ProcessMode)>0),
       ProcessTable(Stream,Process,ProcessMode)*PROCMODE.l(RefReg,RefType,Process,ProcessMode,Period)) / GDP(t) / Discount(rate(t));

  ProductPrices(Crude,RefReg,Period)$(sum((PetRefType,Process,ProcessMode)$ProcessTable(Crude,Process,ProcessMode),-1*ProcessTable(Crude,Process,ProcessMode)*PROCMODE.l(RefReg,PetRefType,Process,ProcessMode,Period))>0) =
    sum((PetRefType,Process,ProcessMode)$ProcessTable(Crude,Process,ProcessMode),
      ProcessTable(Crude,Process,ProcessMode)*PROCMODE.l(RefReg,PetRefType,Process,ProcessMode,Period)*
      (CrudeBalance.m(RefReg,Crude,Period)-CrudeBalance_M_DELTA(RefReg,Crude,Period))) /
    sum((PetRefType,Process,ProcessMode)$ProcessTable(Crude,Process,ProcessMode),
      ProcessTable(Crude,Process,ProcessMode)*PROCMODE.l(RefReg,PetRefType,Process,ProcessMode,Period)) / GDP(t) ;

  ProductPrices(RecipeProd,RefReg,Period)$(sum(RefType,RECIPETOPROD.l(RefReg,RefType,RecipeProd,Period))>0) =
     sum(RefType,RECIPETOPROD.l(RefReg,RefType,RecipeProd,Period)*RecipeTransfer.m(RefReg,RefType,RecipeProd,Period) ) /
     sum(RefType,RECIPETOPROD.l(RefReg,RefType,RecipeProd,Period)) / GDP(t);


  ProductQuantities(GasSpecProd,RefReg,Period) =
     sum((RefType,NonCrudeUtil)$StreamSpecProd(NonCrudeUtil,GasSpecProd),
       ToSPECBLEND.l(RefReg,RefType,GasSpecProd,NonCrudeUtil,Period) ) ;

  ProductQuantities(DistSpecProd,RefReg,Period) =
     sum((RefType,NonCrudeUtil)$StreamSpecProd(NonCrudeUtil,DistSpecProd),
       ToSPECBLEND.l(RefReg,RefType,DistSpecProd,NonCrudeUtil,Period) ) ;

  ProductQuantities(ResidSpecProd,RefReg,Period) =
     sum((RefType,NonCrudeUtil)$StreamSpecProd(NonCrudeUtil,ResidSpecProd),
       ToSPECBLEND.l(RefReg,RefType,ResidSpecProd,NonCrudeUtil,Period) ) ;

  ProductQuantities(AltFuelStream,RefReg,Period) =
     sum((RefType,Process,ProcessMode)$(ProcessTable(AltFuelStream,Process,ProcessMode)>0),
       ProcessTable(AltFuelStream,Process,ProcessMode)*PROCMODE.l(RefReg,RefType,Process,ProcessMode,Period)) ;

  ProductQuantities(Crude,RefReg,Period) =
     sum((PetRefType,Process,ProcessMode)$ProcessTable(Crude,Process,ProcessMode),
       -1*ProcessTable(Crude,Process,ProcessMode)*PROCMODE.l(RefReg,PetRefType,Process,ProcessMode,Period)) ;

  ProductQuantities('ASPHout',RefReg,Period) =
     sum(RefType,RECIPETOPROD.l(RefReg,RefType,'ASPHout',Period)) ;

  ProductQuantities('LUBout',RefReg,Period) =
     sum(RefType,RECIPETOPROD.l(RefReg,RefType,'LUBout',Period)) ;

  ProductQuantities('PCFout',RefReg,Period) =
     sum(RefType,RECIPETOPROD.l(RefReg,RefType,'PCFout',Period)) ;

  ProductQuantities('AVGout',RefReg,Period) =
     sum(RefType,RECIPETOPROD.l(RefReg,RefType,'AVGout',Period)) ;

  ProductQuantities('COKout',RefReg,Period) =
     sum(RefType,RECIPETOPROD.l(RefReg,RefType,'COKout',Period)) ;

  ProductQuantities(CoProductSales('COKdump'),RefReg,Period) =
     sum(RefType,COPRODUCTS.l(RefReg,RefType,CoProductSales,Period)) ;

  ProductQuantities(CoProductSales('GOPout'),RefReg,Period) =
     sum(RefType,COPRODUCTS.l(RefReg,RefType,CoProductSales,Period)) ;

  TotalCrude(RefReg,Period) =
    sum(Crude, ProductQuantities(Crude,RefReg,Period)) ;

  ProfitPerBBL(MNUMPR,Period)$((ord(MNUMPR)<10) and (sum((Crude,RefReg)$(ProductQuantities(Crude,RefReg,Period) and RefReg2MNUMPR(MNUMPR,RefReg)),ProductQuantities(Crude,RefReg,Period))>0)) =
    (
    sum((SpecProd,RefReg)$(ProductQuantities(SpecProd,RefReg,Period) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(SpecProd,RefReg,Period)*ProductQuantities(SpecProd,RefReg,Period) ) +
    sum((RecipeProd,RefReg)$(ProductQuantities(RecipeProd,RefReg,Period) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(RecipeProd,RefReg,Period)*ProductQuantities(RecipeProd,RefReg,Period) ) +
    sum((CoProductSales,RefReg)$(ProductQuantities(CoProductSales,RefReg,Period) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(CoProductSales,RefReg,Period)*ProductQuantities(CoProductSales,RefReg,Period) ) +
    sum((ImpStr('ASPHout'),RefReg)$((EXPORTS.l(ImpStr,RefReg,Period)>0) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(ImpStr,RefReg,Period)*EXPORTS.l(ImpStr,RefReg,Period) ) +
    sum((ImpStr('AVGout'),RefReg)$((EXPORTS.l(ImpStr,RefReg,Period)>0) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(ImpStr,RefReg,Period)*EXPORTS.l(ImpStr,RefReg,Period) ) +
    sum((ImpStr('LUBout'),RefReg)$((EXPORTS.l(ImpStr,RefReg,Period)>0) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(ImpStr,RefReg,Period)*EXPORTS.l(ImpStr,RefReg,Period) ) +
    sum((ImpStr('PCFout'),RefReg)$((EXPORTS.l(ImpStr,RefReg,Period)>0) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(ImpStr,RefReg,Period)*EXPORTS.l(ImpStr,RefReg,Period) ) +
    sum((ImpStr('MN3'),RefReg)$((EXPORTS.l(ImpStr,RefReg,Period)>0) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(ImpStr,RefReg,Period)*EXPORTS.l(ImpStr,RefReg,Period) ) +
    sum((ImpStr('GO3'),RefReg)$((EXPORTS.l(ImpStr,RefReg,Period)>0) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(ImpStr,RefReg,Period)*EXPORTS.l(ImpStr,RefReg,Period) ) +
    sum((ImpStr('AR3'),RefReg)$((EXPORTS.l(ImpStr,RefReg,Period)>0) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(ImpStr,RefReg,Period)*EXPORTS.l(ImpStr,RefReg,Period) ) -
    sum((ImpStr('MN3'),RefReg)$((IMPORTS.l(ImpStr,RefReg,Period)>0) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(ImpStr,RefReg,Period)*IMPORTS.l(ImpStr,RefReg,Period) ) -
    sum((ImpStr('GO3'),RefReg)$((IMPORTS.l(ImpStr,RefReg,Period)>0) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(ImpStr,RefReg,Period)*IMPORTS.l(ImpStr,RefReg,Period) ) -
    sum((ImpStr('AR3'),RefReg)$((IMPORTS.l(ImpStr,RefReg,Period)>0) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(ImpStr,RefReg,Period)*IMPORTS.l(ImpStr,RefReg,Period) ) -
    sum((AltFuelStream,RefReg)$(ProductQuantities(AltFuelStream,RefReg,Period) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(AltFuelStream,RefReg,Period)*ProductQuantities(AltFuelStream,RefReg,Period) ) -
    sum((Crude,RefReg)$(ProductQuantities(Crude,RefReg,Period) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(Crude,RefReg,Period)*ProductQuantities(Crude,RefReg,Period) )
    ) /
    sum((Crude,RefReg)$(ProductQuantities(Crude,RefReg,Period) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductQuantities(Crude,RefReg,Period)) ;

  ProfitPerBBL('10_MNUMPR',Period)$(sum((Crude,DomRefReg)$ProductQuantities(Crude,DomRefReg,Period),ProductQuantities(Crude,DomRefReg,Period))>0) =
    (
    sum((SpecProd,DomRefReg)$ProductQuantities(SpecProd,DomRefReg,Period),
      ProductPrices(SpecProd,DomRefReg,Period)*ProductQuantities(SpecProd,DomRefReg,Period) ) +
    sum((RecipeProd,DomRefReg)$ProductQuantities(RecipeProd,DomRefReg,Period),
      ProductPrices(RecipeProd,DomRefReg,Period)*ProductQuantities(RecipeProd,DomRefReg,Period) ) +
    sum((CoProductSales,DomRefReg)$ProductQuantities(CoProductSales,DomRefReg,Period),
      ProductPrices(CoProductSales,DomRefReg,Period)*ProductQuantities(CoProductSales,DomRefReg,Period) ) +
    sum((ImpStr('ASPHout'),DomRefReg)$(EXPORTS.l(ImpStr,DomRefReg,Period)>0),
      ProductPrices(ImpStr,DomRefReg,Period)*EXPORTS.l(ImpStr,DomRefReg,Period) ) +
    sum((ImpStr('AVGout'),DomRefReg)$(EXPORTS.l(ImpStr,DomRefReg,Period)>0),
      ProductPrices(ImpStr,DomRefReg,Period)*EXPORTS.l(ImpStr,DomRefReg,Period) ) +
    sum((ImpStr('LUBout'),DomRefReg)$(EXPORTS.l(ImpStr,DomRefReg,Period)>0),
      ProductPrices(ImpStr,DomRefReg,Period)*EXPORTS.l(ImpStr,DomRefReg,Period) ) +
    sum((ImpStr('PCFout'),DomRefReg)$(EXPORTS.l(ImpStr,DomRefReg,Period)>0),
      ProductPrices(ImpStr,DomRefReg,Period)*EXPORTS.l(ImpStr,DomRefReg,Period) ) +
    sum((ImpStr('MN3'),DomRefReg)$(EXPORTS.l(ImpStr,DomRefReg,Period)>0),
      ProductPrices(ImpStr,DomRefReg,Period)*EXPORTS.l(ImpStr,DomRefReg,Period) ) +
    sum((ImpStr('GO3'),DomRefReg)$(EXPORTS.l(ImpStr,DomRefReg,Period)>0),
      ProductPrices(ImpStr,DomRefReg,Period)*EXPORTS.l(ImpStr,DomRefReg,Period) ) +
    sum((ImpStr('AR3'),DomRefReg)$(EXPORTS.l(ImpStr,DomRefReg,Period)>0),
      ProductPrices(ImpStr,DomRefReg,Period)*EXPORTS.l(ImpStr,DomRefReg,Period) ) -
    sum((ImpStr('MN3'),DomRefReg)$(IMPORTS.l(ImpStr,DomRefReg,Period)>0),
      ProductPrices(ImpStr,DomRefReg,Period)*IMPORTS.l(ImpStr,DomRefReg,Period) ) -
    sum((ImpStr('GO3'),DomRefReg)$(IMPORTS.l(ImpStr,DomRefReg,Period)>0),
      ProductPrices(ImpStr,DomRefReg,Period)*IMPORTS.l(ImpStr,DomRefReg,Period) ) -
    sum((ImpStr('AR3'),DomRefReg)$(IMPORTS.l(ImpStr,DomRefReg,Period)>0),
      ProductPrices(ImpStr,DomRefReg,Period)*IMPORTS.l(ImpStr,DomRefReg,Period) ) -
    sum((AltFuelStream,DomRefReg)$ProductQuantities(AltFuelStream,DomRefReg,Period),
      ProductPrices(AltFuelStream,DomRefReg,Period)*ProductQuantities(AltFuelStream,DomRefReg,Period) ) -
    sum((Crude,DomRefReg)$ProductQuantities(Crude,DomRefReg,Period),
      ProductPrices(Crude,DomRefReg,Period)*ProductQuantities(Crude,DomRefReg,Period) )
    ) /
    sum((Crude,DomRefReg)$ProductQuantities(Crude,DomRefReg,Period),
      ProductQuantities(Crude,DomRefReg,Period)) ;

* Write the 1st period value to the restart file
  LFMMOUT_PROFIT_BBL(MNUMPR,MNUMYR) = ProfitPerBBL(MNUMPR,Period) ;

);

* Period 2
loop((t,MNUMYR,Period)$(t.val=NCNTRL_CURCALYR+1 and tMNUM(t,MNUMYR) and BldPeriod(Period)),

  ProductPrices(GasSpecProd,RefReg,Period)$(sum(RefType,sum(RcpMode$RecipeBlending(RcpMode,GasSpecProd),ToRECIPEBLEND.l(RefReg,RefType,RcpMode,GasSpecProd,Period)) + ExpFromSPECBLEND.l(RefReg,RefType,GasSpecProd,Period))>0) =
     sum(RefType,
       (sum(RcpMode$RecipeBlending(RcpMode,GasSpecProd),ToRECIPEBLEND.l(RefReg,RefType,RcpMode,GasSpecProd,Period)) +
        ExpFromSPECBLEND.l(RefReg,RefType,GasSpecProd,Period)) * GasSpecBalance.m(RefReg,RefType,GasSpecProd,Period) ) /
     sum(RefType,
       sum(RcpMode$RecipeBlending(RcpMode,GasSpecProd),ToRECIPEBLEND.l(RefReg,RefType,RcpMode,GasSpecProd,Period)) +
       ExpFromSPECBLEND.l(RefReg,RefType,GasSpecProd,Period) ) / GDP(t) / Discount(rate(t)) ;

  ProductPrices(DistSpecProd,RefReg,Period)$(sum((RefType,RcpMode)$RecipeBlending(RcpMode,DistSpecProd),ToRECIPEBLEND.l(RefReg,RefType,RcpMode,DistSpecProd,Period))>0) =
     sum((RefType,RcpMode)$RecipeBlending(RcpMode,DistSpecProd),
       ToRECIPEBLEND.l(RefReg,RefType,RcpMode,DistSpecProd,Period)*DistSpecBalance.m(RefReg,RefType,DistSpecProd,Period) ) /
     sum((RefType,RcpMode)$RecipeBlending(RcpMode,DistSpecProd),
       ToRECIPEBLEND.l(RefReg,RefType,RcpMode,DistSpecProd,Period) ) / GDP(t) / Discount(rate(t));

  ProductPrices(ResidSpecProd,RefReg,Period)$(sum((RefType,RcpMode)$RecipeBlending(RcpMode,ResidSpecProd),ToRECIPEBLEND.l(RefReg,RefType,RcpMode,ResidSpecProd,Period))>0) =
     sum((RefType,RcpMode)$RecipeBlending(RcpMode,ResidSpecProd),
       ToRECIPEBLEND.l(RefReg,RefType,RcpMode,ResidSpecProd,Period)*ResidSpecBalance.m(RefReg,RefType,ResidSpecProd,Period) ) /
     sum((RefType,RcpMode)$RecipeBlending(RcpMode,ResidSpecProd),
       ToRECIPEBLEND.l(RefReg,RefType,RcpMode,ResidSpecProd,Period) ) / GDP(t) / Discount(rate(t));

  ProductPrices(Stream,RefReg,Period)$((sum((RefType,Process,ProcessMode)$(ProcessTable(Stream,Process,ProcessMode)>0),ProcessTable(Stream,Process,ProcessMode)*PROCMODE.l(RefReg,RefType,Process,ProcessMode,Period))>0) and (AltFuelStream(Stream) or ImpStr(Stream))) =
     sum((RefType,Process,ProcessMode)$(ProcessTable(Stream,Process,ProcessMode)>0),
       ProcessTable(Stream,Process,ProcessMode)*PROCMODE.l(RefReg,RefType,Process,ProcessMode,Period)*StreamBalance.m(RefReg,RefType,Stream,Period)) /
     sum((RefType,Process,ProcessMode)$(ProcessTable(Stream,Process,ProcessMode)>0),
       ProcessTable(Stream,Process,ProcessMode)*PROCMODE.l(RefReg,RefType,Process,ProcessMode,Period)) / GDP(t) / Discount(rate(t));

  ProductPrices(Crude,RefReg,Period)$(sum((PetRefType,Process,ProcessMode)$ProcessTable(Crude,Process,ProcessMode),-1*ProcessTable(Crude,Process,ProcessMode)*PROCMODE.l(RefReg,PetRefType,Process,ProcessMode,Period))>0) =
     sum((PetRefType,Process,ProcessMode)$ProcessTable(Crude,Process,ProcessMode),
       ProcessTable(Crude,Process,ProcessMode)*PROCMODE.l(RefReg,PetRefType,Process,ProcessMode,Period)*
       (CrudeBalance.m(RefReg,Crude,Period)-CrudeBalance_M_DELTA(RefReg,Crude,Period))) /
     sum((PetRefType,Process,ProcessMode)$ProcessTable(Crude,Process,ProcessMode),
       ProcessTable(Crude,Process,ProcessMode)*PROCMODE.l(RefReg,PetRefType,Process,ProcessMode,Period)) / GDP(t) / Discount(rate(t)) ;

  ProductPrices(RecipeProd,RefReg,Period)$(sum(RefType,RECIPETOPROD.l(RefReg,RefType,RecipeProd,Period))>0) =
     sum(RefType,RECIPETOPROD.l(RefReg,RefType,RecipeProd,Period)*RecipeTransfer.m(RefReg,RefType,RecipeProd,Period) ) /
     sum(RefType,RECIPETOPROD.l(RefReg,RefType,RecipeProd,Period)) / GDP(t) / Discount(rate(t));

  ProductPrices(CoProductSales,RefReg,Period)$(sum(RefType,COPRODUCTS.l(RefReg,RefType,CoProductSales,Period))>0) =
     sum(RefType,COPRODUCTS.l(RefReg,RefType,CoProductSales,Period)*RecipeBPTransfer.m(RefReg,RefType,CoProductSales,Period) ) /
     sum(RefType,COPRODUCTS.l(RefReg,RefType,CoProductSales,Period)) / GDP(t) / Discount(rate(t));


  ProductQuantities(GasSpecProd,RefReg,Period) =
     sum((RefType,NonCrudeUtil)$StreamSpecProd(NonCrudeUtil,GasSpecProd),
       ToSPECBLEND.l(RefReg,RefType,GasSpecProd,NonCrudeUtil,Period) ) ;

  ProductQuantities(DistSpecProd,RefReg,Period) =
     sum((RefType,NonCrudeUtil)$StreamSpecProd(NonCrudeUtil,DistSpecProd),
       ToSPECBLEND.l(RefReg,RefType,DistSpecProd,NonCrudeUtil,Period) ) ;

  ProductQuantities(ResidSpecProd,RefReg,Period) =
     sum((RefType,NonCrudeUtil)$StreamSpecProd(NonCrudeUtil,ResidSpecProd),
       ToSPECBLEND.l(RefReg,RefType,ResidSpecProd,NonCrudeUtil,Period) ) ;

  ProductQuantities(AltFuelStream,RefReg,Period) =
     sum((RefType,Process,ProcessMode)$(ProcessTable(AltFuelStream,Process,ProcessMode)>0),
       ProcessTable(AltFuelStream,Process,ProcessMode)*PROCMODE.l(RefReg,RefType,Process,ProcessMode,Period)) ;

  ProductQuantities(Crude,RefReg,Period) =
     sum((PetRefType,Process,ProcessMode)$ProcessTable(Crude,Process,ProcessMode),
       -1*ProcessTable(Crude,Process,ProcessMode)*PROCMODE.l(RefReg,PetRefType,Process,ProcessMode,Period)) ;

  ProductQuantities('ASPHout',RefReg,Period) =
     sum(RefType,RECIPETOPROD.l(RefReg,RefType,'ASPHout',Period)) ;

  ProductQuantities('LUBout',RefReg,Period) =
     sum(RefType,RECIPETOPROD.l(RefReg,RefType,'LUBout',Period)) ;

  ProductQuantities('PCFout',RefReg,Period) =
     sum(RefType,RECIPETOPROD.l(RefReg,RefType,'PCFout',Period)) ;

  ProductQuantities('AVGout',RefReg,Period) =
     sum(RefType,RECIPETOPROD.l(RefReg,RefType,'AVGout',Period)) ;

  ProductQuantities('COKout',RefReg,Period) =
     sum(RefType,RECIPETOPROD.l(RefReg,RefType,'COKout',Period)) ;

  ProductQuantities(CoProductSales('COKdump'),RefReg,Period) =
     sum(RefType,COPRODUCTS.l(RefReg,RefType,CoProductSales,Period)) ;

  ProductQuantities(CoProductSales('GOPout'),RefReg,Period) =
     sum(RefType,COPRODUCTS.l(RefReg,RefType,CoProductSales,Period)) ;

  TotalCrude(RefReg,Period) =
    sum(Crude, ProductQuantities(Crude,RefReg,Period)) ;

  ProfitPerBBL(MNUMPR,Period)$((ord(MNUMPR)<10) and (sum((Crude,RefReg)$(ProductQuantities(Crude,RefReg,Period) and RefReg2MNUMPR(MNUMPR,RefReg)),ProductQuantities(Crude,RefReg,Period))>0) ) =
    (
    sum((SpecProd,RefReg)$(ProductQuantities(SpecProd,RefReg,Period) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(SpecProd,RefReg,Period)*ProductQuantities(SpecProd,RefReg,Period) ) +
    sum((RecipeProd,RefReg)$(ProductQuantities(RecipeProd,RefReg,Period) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(RecipeProd,RefReg,Period)*ProductQuantities(RecipeProd,RefReg,Period) ) +
    sum((CoProductSales,RefReg)$(ProductQuantities(CoProductSales,RefReg,Period) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(CoProductSales,RefReg,Period)*ProductQuantities(CoProductSales,RefReg,Period) ) +
    sum((ImpStr('ASPHout'),RefReg)$((EXPORTS.l(ImpStr,RefReg,Period)>0) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(ImpStr,RefReg,Period)*EXPORTS.l(ImpStr,RefReg,Period) ) +
    sum((ImpStr('AVGout'),RefReg)$((EXPORTS.l(ImpStr,RefReg,Period)>0) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(ImpStr,RefReg,Period)*EXPORTS.l(ImpStr,RefReg,Period) ) +
    sum((ImpStr('LUBout'),RefReg)$((EXPORTS.l(ImpStr,RefReg,Period)>0) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(ImpStr,RefReg,Period)*EXPORTS.l(ImpStr,RefReg,Period) ) +
    sum((ImpStr('PCFout'),RefReg)$((EXPORTS.l(ImpStr,RefReg,Period)>0) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(ImpStr,RefReg,Period)*EXPORTS.l(ImpStr,RefReg,Period) ) +
    sum((ImpStr('MN3'),RefReg)$((EXPORTS.l(ImpStr,RefReg,Period)>0) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(ImpStr,RefReg,Period)*EXPORTS.l(ImpStr,RefReg,Period) ) +
    sum((ImpStr('GO3'),RefReg)$((EXPORTS.l(ImpStr,RefReg,Period)>0) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(ImpStr,RefReg,Period)*EXPORTS.l(ImpStr,RefReg,Period) ) +
    sum((ImpStr('AR3'),RefReg)$((EXPORTS.l(ImpStr,RefReg,Period)>0) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(ImpStr,RefReg,Period)*EXPORTS.l(ImpStr,RefReg,Period) ) -
    sum((ImpStr('MN3'),RefReg)$((IMPORTS.l(ImpStr,RefReg,Period)>0) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(ImpStr,RefReg,Period)*IMPORTS.l(ImpStr,RefReg,Period) ) -
    sum((ImpStr('GO3'),RefReg)$((IMPORTS.l(ImpStr,RefReg,Period)>0) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(ImpStr,RefReg,Period)*IMPORTS.l(ImpStr,RefReg,Period) ) -
    sum((ImpStr('AR3'),RefReg)$((IMPORTS.l(ImpStr,RefReg,Period)>0) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(ImpStr,RefReg,Period)*IMPORTS.l(ImpStr,RefReg,Period) ) -
    sum((AltFuelStream,RefReg)$(ProductQuantities(AltFuelStream,RefReg,Period) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(AltFuelStream,RefReg,Period)*ProductQuantities(AltFuelStream,RefReg,Period) ) -
    sum((Crude,RefReg)$(ProductQuantities(Crude,RefReg,Period) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductPrices(Crude,RefReg,Period)*ProductQuantities(Crude,RefReg,Period) )
    ) /
    sum((Crude,RefReg)$(ProductQuantities(Crude,RefReg,Period) and RefReg2MNUMPR(MNUMPR,RefReg)),
      ProductQuantities(Crude,RefReg,Period)) ;

  ProfitPerBBL('10_MNUMPR',Period)$(sum((Crude,DomRefReg)$ProductQuantities(Crude,DomRefReg,Period),ProductQuantities(Crude,DomRefReg,Period))>0) =
    (
    sum((SpecProd,DomRefReg)$ProductQuantities(SpecProd,DomRefReg,Period),
      ProductPrices(SpecProd,DomRefReg,Period)*ProductQuantities(SpecProd,DomRefReg,Period) ) +
    sum((RecipeProd,DomRefReg)$ProductQuantities(RecipeProd,DomRefReg,Period),
      ProductPrices(RecipeProd,DomRefReg,Period)*ProductQuantities(RecipeProd,DomRefReg,Period) ) +
    sum((CoProductSales,DomRefReg)$ProductQuantities(CoProductSales,DomRefReg,Period),
      ProductPrices(CoProductSales,DomRefReg,Period)*ProductQuantities(CoProductSales,DomRefReg,Period) ) +
    sum((ImpStr('ASPHout'),DomRefReg)$(EXPORTS.l(ImpStr,DomRefReg,Period)>0),
      ProductPrices(ImpStr,DomRefReg,Period)*EXPORTS.l(ImpStr,DomRefReg,Period) ) +
    sum((ImpStr('AVGout'),DomRefReg)$(EXPORTS.l(ImpStr,DomRefReg,Period)>0),
      ProductPrices(ImpStr,DomRefReg,Period)*EXPORTS.l(ImpStr,DomRefReg,Period) ) +
    sum((ImpStr('LUBout'),DomRefReg)$(EXPORTS.l(ImpStr,DomRefReg,Period)>0),
      ProductPrices(ImpStr,DomRefReg,Period)*EXPORTS.l(ImpStr,DomRefReg,Period) ) +
    sum((ImpStr('PCFout'),DomRefReg)$(EXPORTS.l(ImpStr,DomRefReg,Period)>0),
      ProductPrices(ImpStr,DomRefReg,Period)*EXPORTS.l(ImpStr,DomRefReg,Period) ) +
    sum((ImpStr('MN3'),DomRefReg)$(EXPORTS.l(ImpStr,DomRefReg,Period)>0),
      ProductPrices(ImpStr,DomRefReg,Period)*EXPORTS.l(ImpStr,DomRefReg,Period) ) +
    sum((ImpStr('GO3'),DomRefReg)$(EXPORTS.l(ImpStr,DomRefReg,Period)>0),
      ProductPrices(ImpStr,DomRefReg,Period)*EXPORTS.l(ImpStr,DomRefReg,Period) ) +
    sum((ImpStr('AR3'),DomRefReg)$(EXPORTS.l(ImpStr,DomRefReg,Period)>0),
      ProductPrices(ImpStr,DomRefReg,Period)*EXPORTS.l(ImpStr,DomRefReg,Period) ) -
    sum((ImpStr('MN3'),DomRefReg)$(IMPORTS.l(ImpStr,DomRefReg,Period)>0),
      ProductPrices(ImpStr,DomRefReg,Period)*IMPORTS.l(ImpStr,DomRefReg,Period) ) -
    sum((ImpStr('GO3'),DomRefReg)$(IMPORTS.l(ImpStr,DomRefReg,Period)>0),
      ProductPrices(ImpStr,DomRefReg,Period)*IMPORTS.l(ImpStr,DomRefReg,Period) ) -
    sum((ImpStr('AR3'),DomRefReg)$(IMPORTS.l(ImpStr,DomRefReg,Period)>0),
      ProductPrices(ImpStr,DomRefReg,Period)*IMPORTS.l(ImpStr,DomRefReg,Period) ) -
    sum((AltFuelStream,DomRefReg)$ProductQuantities(AltFuelStream,DomRefReg,Period),
      ProductPrices(AltFuelStream,DomRefReg,Period)*ProductQuantities(AltFuelStream,DomRefReg,Period) ) -
    sum((Crude,DomRefReg)$ProductQuantities(Crude,DomRefReg,Period),
      ProductPrices(Crude,DomRefReg,Period)*ProductQuantities(Crude,DomRefReg,Period) )
    ) /
    sum((Crude,DomRefReg)$ProductQuantities(Crude,DomRefReg,Period),
      ProductQuantities(Crude,DomRefReg,Period)) ;

);


***************************************************************************************

);  /* END  If(NCNTRL_CURCALYR>2010 and NCNTRL_FCRL=1, */
*----------------------------------------------------------------------------------------------------------
*End FTAB Table 21 Global variable fill

Loop(MNUMYR,
  Loop(MNUMPR,
    CheckValue(LFMMOUT_PROFIT_BBL(MNUMPR,MNUMYR));
  );
  Loop(MNUMCR,
   Loop(M4,
    Loop(M10,
     CheckValue(LFMMOUT_FEEDSTOCKS(M4,M10,MNUMYR));
    );
    Loop(M11,
     CheckValue(LFMMOUT_REFINE_PROD(M4,M11,MNUMYR));
     CheckValue(LFMMOUT_GROSS_IMPORT(M4,M11,MNUMYR));
    );
    Loop(M12,
     CheckValue(LFMMOUT_GROSS_EXPORT(M4,M12,MNUMYR));
     CheckValue(LFMMOUT_DOM_CONSUME(M4,M12,MNUMYR));
    );
    Loop(M13,
     CheckValue(LFMMOUT_INTERMEDIATE(M4,M13,MNUMYR));
    );
   );
  ); /* End_of MNUMCR_loop */
);/* End_of MNUMYR_loop */



if((LFMMReporting=1),
Execute_Unload 'LFMM_to_NEMS.gdx',
   MNUMCR, MNUMYR, M2, M4, M5, M6, M10, PRDEXP, MNUMPR, MNUMY3, MNMFS1, NDRGN1, MNMYRF, LCFS_C,
   MPBLK_PDSAS, MPBLK_PDSCM, MPBLK_PDSEL, MPBLK_PDSIN, MPBLK_PDSRS, MPBLK_PDSTR, MPBLK_PASIN, MPBLK_PETTR,
   MPBLK_PKSAS, MPBLK_PKSCM, MPBLK_PKSIN, MPBLK_PKSRS, MPBLK_PLGAS, MPBLK_PJFTR, MPBLK_PLGCM, MPBLK_PLGIN,
   MPBLK_PLGRS, MPBLK_PLGTR, PMORE_PLGINPF, MPBLK_PMETR,
   MPBLK_PMGAS, MPBLK_PMGCM, MPBLK_PMGIN, MPBLK_PMGTR,
   MPBLK_POTAS, MPBLK_POTIN, MPBLK_POTTR, MPBLK_PPFIN,
   MPBLK_PRHAS, MPBLK_PRHEL, MPBLK_PRHTR, MPBLK_PRLAS, MPBLK_PRLCM, MPBLK_PRLEL, MPBLK_PRLIN, MPBLK_PRLTR,
   QBLK_QBMRF, QBLK_QCLRF, QBLK_QELRF, QBLK_QNGRF, QBLK_QRLRF, QBLK_QLGRF, QBLK_QPCRF, QBLK_QSGRF, QBLK_QOTRF,
   QMORE_QPRRF, QMORE_QBURF, QMORE_QISRF, QMORE_QPYRF,
   WRENEW_QBMETCL, WRENEW_QBMBTCL,
   WRENEW_QCLETH, WRENEW_QELETH, WRENEW_QNGETH, PMMOUT_QBMRFBTL, PMMOUT_QCLRFPD, PMMOUT_QMERF,
   COGEN_CGREFGEN, COGEN_CGREFQ, COGEN_CGREFCAP,
   CONVFACT_CFPFQ, CONVFACT_CFTGQ, CONVFACT_CFJFQ, CONVFACT_CFLGQ,
   CONVFACT_CFRGQ, CONVFACT_CFMGQ, CONVFACT_CFDSRS, CONVFACT_CFDSQT,
   CONVFACT_CFDSTR, CONVFACT_CFDSIN, CONVFACT_CFDSCM, CONVFACT_CFDSEL, CONVFACT_CFLUQ,
   CONVFACT_CFE85Q, CONVFACT_CFCTLLIQ, CONVFACT_CFBTLLIQ, CONVFACT_CFCBTLLIQ,
   CONVFACT_CFIMUO, CONVFACT_CFNGL,
   CONVFACT_CFCRDLTSWT, CONVFACT_CFCRDLTSOUR, CONVFACT_CFCRDMD2SOUR, CONVFACT_CFCRDMDSOUR,
   CONVFACT_CFCRDHVSWT, CONVFACT_CFCRDHVSOUR, CONVFACT_CFCRDCA, CONVFACT_CFCRDSYN,
   CONVFACT_CFCRDDILBIT, CONVFACT_CFCRDLT2SWT, CONVFACT_CFCRDLSCOND,
   PMMFTAB_DSMUTR, PMMFTAB_DSMURS, PMMFTAB_MGMUTR, PMMFTAB_JFMUTR, PMMFTAB_CBIODUAL,
   PMMFTAB_UBAVOLMG, PMMFTAB_UBAVOLDS, PMMFTAB_YGR2GDTPD, PMMFTAB_CELLIMPFRAC, PMMFTAB_MINREN,
   PMMFTAB_PDSU, PMMFTAB_PDSL, PMMFTAB_PDSCRB, PMMFTAB_DSCSHR, PMMFTAB_PJF, PMMFTAB_PDS,
   PMMFTAB_GRNCAPCD, PMMFTAB_CLLCAPCD, PMMFTAB_CRNCAPCD, PMMFTAB_ADVCAPCD, LFMMOUT_GRAINCD,
   PMMFTAB_E85ICCREDIT, PMMFTAB_PALBOB, PMMFTAB_PALMG, PMMFTAB_WS_RBOB, PMMFTAB_RFENVFX,
   PMMFTAB_RFIMPEXPEND, PMMRPT_RFQICRD, PMMRPT_BLDIMP, PMMFTAB_PLMQTYCD,
   PMMFTAB_RFHCXH2IN, PMMFTAB_RFQNGPF,
   PMMFTAB_SBO2GDTPD, PMMFTAB_SBOQTYCD, PMMFTAB_WGR2GDTPD, PMMFTAB_YGR_PRICE, PMMFTAB_SBO_PRICE,
   PMMFTAB_SBO2SAFPD, PMMFTAB_WGR2SAFPD,
   TRANREP_E85AVAIL,
   PMMRPT_MUFTAX, PMMRPT_BIMQTYCD, PMMRPT_ETHTOTCD, PMMRPT_RFCRDOTH, PMMRPT_ETHE85CD,
   PMMRPT_CLLETHCD, PMMRPT_CRNETHCD, PMMRPT_ETHIMP, PMMRPT_ETHEXP, PMMRPT_OTHETHCD, PMMRPT_RFDSTCAP,
   PMMRPT_RFDSTUTL, PMMRPT_RFIMCR, PMMRPT_RFIMTP, PMMRPT_RFMETM85, PMMRPT_RFQARO, PMMRPT_RFMETI,
   PMMRPT_RFQDS, PMMRPT_RFQJF, PMMRPT_RFQKS, PMMRPT_RFQLG, PMMRPT_RFQMG, PMMRPT_RFQOTH,
   PMMRPT_RFQPCK, PMMRPT_RFQPF, PMMRPT_RFQRH, PMMRPT_RFQRL, PMMRPT_RFQSTG, PMMRPT_TDIESEL,
   PMMRPT_BIODCONCD, PMMRPT_BIODPRICE, PMMRPT_GRNETHCD, PMMRPT_RFIPQCLL,
   PMMRPT_RFMTBI, PMMRPT_RFPQIPRDT, PMMRPT_RFPQUFC, PMMRPT_RFQEXCRD, PMMRPT_RFETHE85,
   PMMRPT_BIODIMP, PMMRPT_BIODEXP, PMMRPT_RFQEXPRDT, PMMRPT_PETHM, PMMRPT_PETHANOL, PMMRPT_QPRDEX,
   INDREP_QCCRF, QONROAD_QDSTRHWY, QONROAD_CFDSTRHWY, PONROAD_PDSTRHWY,
   PMMOUT_TRGNE85, PMMOUT_ETHNE85, PMMOUT_RFSPRFR, PMMOUT_RFQNGPL, PMMOUT_UBAVOL,
   PMMOUT_BTLFRAC, PMMOUT_CTLFRAC, PMMOUT_CBTLFRAC, PMMOUT_GLBCRDDMD, PMMOUT_CRNPRICE,
   PMMOUT_RFPQNGL, PMMOUT_GTLFRAC,
   LFMMOUT_RFIPQMG, LFMMOUT_RFIPQCBOB, LFMMOUT_RFIPQRG, LFMMOUT_RFIPQRBOB, LFMMOUT_RFIPQJF,
   LFMMOUT_RFIPQPR, LFMMOUT_RFIPQPY, LFMMOUT_RFIPQET, LFMMOUT_RFIPQBU, LFMMOUT_RFIPQIS, LFMMOUT_RFIPQPP,
   LFMMOUT_RFIPQAR3, LFMMOUT_RFIPQGO3, LFMMOUT_RFIPQMN3,
   LFMMOUT_RFIPQDS,LFMMOUT_RFIPQDL,LFMMOUT_RFIPQDU,LFMMOUT_RFIPQRL,LFMMOUT_RFIPQRH,LFMMOUT_RFIPQPF,
   LFMMOUT_RFPRDDIESEL, LFMMOUT_CORNCD,
   LFMMOUT_Q_CRUDE_IMPORTA, LFMMOUT_Q_CRUDE_IMPORTS, LFMMOUT_P_CRUDE_IMPORTS,
   LFMMOUT_P_CRUDE_EXPORTS, LFMMOUT_Q_CRUDE_EXPORTS, LFMMOUT_P_CRUDE_TO_CAN, LFMMOUT_Q_CRUDE_TO_CAN,
   LFMMOUT_GRD2DSQTY, LFMMOUT_GRN2MGQTY, LFMMOUT_ETHTOT, LFMMOUT_BIMQTY,
   LFMMOUT_SBOQGD, LFMMOUT_YGRQGD, LFMMOUT_WGRQGD,
   LFMMOUT_SAF2JTQTY, LFMMOUT_SBOQRJH, LFMMOUT_WGRQRJH, LFMMOUT_WGRQRDH
   LFMMOUT_BIODIMPPD, LFMMOUT_RenewDIMP, LFMMOUT_RenewDImpPD, LFMMOUT_BIODEXPPD,
   LFMMOUT_RFSCREDPRC, LFMMOUT_RFSSAFETY, LFMMOUT_RFSACTUAL, LFMMOUT_RFSMANDATES, LFMMOUT_RFSCREDITS,
   LFMMOUT_RFIPQCG, LFMMOUT_RFIPQAG, LFMMOUT_RFIPQAS, LFMMOUT_RFIPQLU, LFMMOUT_RFIPQCD, LFMMOUT_RFIPQPC,
   LFMMOUT_MOTOR_FUEL, LFMMOUT_DIST_FUEL, LFMMOUT_QNGRFPD,
   LFMMOUT_FEEDSTOCKS, LFMMOUT_INTERMEDIATE, LFMMOUT_REFINE_PROD, LFMMOUT_GROSS_IMPORT,
   LFMMOUT_GROSS_EXPORT, LFMMOUT_DOM_CONSUME,
   LFMMOUT_LCFS_Offset_Prc, LFMMOUT_LCFS_Baseline, LFMMOUT_LCFS_Carb_Offset,
   LFMMOUT_LCFS_Waiver, LFMMOUT_LCFS_Actual, LFMMOUT_LCFS_PeToTrills,
   LFMMOUT_CFP_Offset_Prc, LFMMOUT_CFP_Baseline, LFMMOUT_CFP_Carb_Offset,
   LFMMOUT_CFP_Waiver, LFMMOUT_CFP_Actual, LFMMOUT_CFP_PeToTrills,
   LFMMOUT_REFGAIN, LFMMOUT_LFMMCODE, PMMOUT_RFQPRCG, AB32_AB_COVD_EM_REF,
   LFMMOUT_RFS_WAIVER, LFMMOUT_RFSDSTR, LFMMOUT_RFSMGTR, LFMMOUT_RFSRBOB, LFMMOUT_RFSJFTR,
   LFMMOUT_RFSDSRS, LFMMOUT_REF_CAP, LFMMOUT_REF_UTL,
   LFMMOUT_RFCRUDEINP, LFMMOUT_P_RFCRUDEINP,
   LFMMOUT_RFOTHERINP, LFMMOUT_BIOBUTESTK, LFMMOUT_RFBIOBUTECD, LFMMOUT_RFBIOBUTERR,
   LFMMOUT_QBIOBUTE, LFMMOUT_BIOBUTEIMP, LFMMOUT_BIOBUTEEXP, LFMMOUT_BIOBUTEPRICE,
   LFMMOUT_REFPRODET, LFMMOUT_REFPRODPR, LFMMOUT_REFPRODBU, LFMMOUT_REFPRODIS,
   LFMMOUT_REFPRODPP, LFMMOUT_REFPRODPY, LFMMOUT_REFPRODOO,
   LFMMOUT_REFINPET, LFMMOUT_REFINPPR, LFMMOUT_REFINPBU, LFMMOUT_REFINPIS,
   LFMMOUT_REFINPPP, LFMMOUT_REFINPPY, LFMMOUT_REFINPOO,
   LFMMOUT_AB32JETCOVER,
   LFMMOUT_AB32_DS, LFMMOUT_AB32_KS, LFMMOUT_AB32_PR, LFMMOUT_AB32_MG, LFMMOUT_AB32_ET, LFMMOUT_AB32_JF,
   INTOUT_IT_WOP, INTOUT_BRENT_PRICE,
   PMORE_PPRRS, PMORE_PPRCM, PMORE_PPPIN, PMORE_PPPINPF, PMORE_PETIN, PMORE_PBUIN,
   PMORE_PPRIN, PMORE_PISIN, PMORE_PISINPF, PMORE_PLUIN, PMORE_PPRTR, PMORE_PPROLENERF,
   PMORE_PSULFURIN, PMORE_PETINPF, PMORE_PBUINPF, PMORE_PPRINPF,
   COALEMM_N_RG,COALEMM_N_RY,COALEMM_N_IGRP,COALEMM_N_PLTS,COALEMM_N_CFR,
   COALEMM_N_HRAT,COALEMM_N_CPTY,COALEMM_N_PTP, PMMOUT_XTL_CO2AVAIL, EMISSION_CCS_PMM,
   OGSMOUT_OGCO2QLF, OGSMOUT_OGCO2PLF, LFMMOUT_PROFIT_BBL, LFMMOUT_RFCRUDEWHP,
   CONVFACT_APICAMG,
   CONVFACT_APILTSW, CONVFACT_APILTSO, CONVFACT_APIMMSO, CONVFACT_APIMDSO,
   CONVFACT_APIHVSW, CONVFACT_APIHVSO, CONVFACT_APICA,   CONVFACT_APISYN,
   CONVFACT_APIDIL,  CONVFACT_APILLSW, CONVFACT_API50PL,
   CONVFACT_APICRDDOM, CONVFACT_APICRDIMP, CONVFACT_APICRDEXP;

   else

*  send back only the return code if anything other than optimal solution:
Execute_Unload 'LFMM_to_NEMS.gdx', LFMMOUT_LFMMCODE ;

);
* end of "if((LFMMReporting=1)," (sending values back to NEMS only if optimal solution)
********************************************************************************




if((NCNTRL_FCRL=1) and (NCNTRL_CURCALYR>2010) and (LFMMReporting=1),

* Currently, all of the "rep" stuff is automatically dumped in to lfmm_p.gdx
* Keep this code, though, in case lfmm_p.gdx changes in the future
   execute_unload 'LFMM_report.gdx',
   repActivity, repUtilization,
   repRefWSPrice, repCenWSPrice, repRefRefTran, repRefCenTran, repRefProd,
   repRefRefTranPLL, repRefRefTranCPL, repReftoRefTranCap, repRefRefTranMode,
   repRefRefTranModeCapUtz,
   repREFCDTranCap, repRefCDTranMode, repRefCDTranModeCapUtz,
   repRefFuelUse, repRefElecPurch, repTotEnergyUse, repRFSCredits, repCO2Emissions,
   repTotalCO2, repMargCrdPrice, repCrudeUse, repCrudeRAC, repSpecProdPrice,
   repRACMargins, repWTIMargins, repMassBalance, repMassBalanceIn, repMassBalanceOut,
   repSpecProp, repEnergyBalance, repWSAltFuelPrc,
   repCrudeImports, repCrudeExports, repDomesticCrudeSup,
   repCrudeInputs,repCrudeInputsTotal,repOtherSupply,repOtherSupplyTotal,
   repNonPetOther,repNonPetOtherTotal,repProcessGain,repProcessGainTotal,
   repPetOutput,repPetOutputTotal,repPetFuelUse,repPetFuelUseTotal,
   repDiscrepency, repBalanceRep, repNGLSource, repNGLConsumption,
   nr_r_OpCost, nr_q_OpCost, nr_p_Stream, nr_q_Stream, repRFSMarkups ;


*** Should set this up so that it only does this in the last iteration of the final year!!!
*  Create the Access reporting database
  If(CreateAccessDB = 1,
*   Execute '=gdx2access LFMM_report.gdx @nemsdefault.ini';
*  Execute VB scripts for relabeling
   Execute '=cscript access.vbs';
   Execute '=cscript access2.vbs';
   Execute '=cscript access3.vbs';
   put_utility 'shell' / 'move /Y LFMM_report.mdb LFMM_report'RunYears.te(RunYears)'.mdb' ;
  );

* Clean up and compress output (note: call to rar.exe may need to be modified to a user's system)
*put_utility 'shell' / 'c:\progra~1\winrar\rar.exe m debug'RunYears.te(RunYears)'.rar *'RunYears.te(RunYears)'.txt' ;
*put_utility 'shell' / 'c:\progra~1\winrar\rar.exe m debug'RunYears.te(RunYears)'.rar *report'RunYears.te(RunYears)'.gdx' ;

);   /* end 2nd LOOP over FCRL=1, CURCALYR>2010, LFMMReporting=1 */

