

*------------------------------------------------------------------------------------------------------------
*------------------------------------------------------------------------------------------------------------
* for lfreport.gms

set DomM10(M10)                   Domestic refining regions NEMS indexes
                                               / 01_M10
                                                 02_M10
                                                 03_M10
                                                 04_M10
                                                 05_M10
                                                 06_M10
                                                 07_M10
                                                 08_M10  /
;

set
Gain_Streams(stream)       Set of all streams referenced in gain calculation
                           /Calif, Dilbit, H_Sour, H_Sweet, L_Sour, L_Sweet, M_Msour, M_Sour, Syncrude, UL_Sweet, Condensate
                            BDS, BKE, BNH, BNL
                            CBDS, CBKE, CBNH, CBNL
                            CDS, CKE, CNH, CNL
                            GDS, GKE, GNH, GNL
                            FCO, WGR, YGR
                            GO1, GO2, GO3, GO4, GO5, GO6, GO7, GO8, GO9
                            IC4, IC6
                            MN1, MN2, MN3, MN4, MN5, MN6, MN7, MN8, MN9
                            NGS, BPN, BPD
                            AL5, ALB, ALP
                            C5F, C5PH, C5PL, CC3S, CC3U
                            CGO1H, CGO1V, CGO3H, CGO3V, CGO4H, CGO4V, CGO6H
                            CGO6V, CGO7H, CGO7V, CGO9H, CGO9V, CGOH, CGOL
                            CHDSH, CHDSHD, CHDSHDA, CHDSL, CHDSLD, CHDSLDA, CHDSM, CHDSMD, CHDSMDA
                            CLDSH, CLDSHD, CLDSL, CLDSLD, CLDSM, CLDSMD, CLOH, CLOL
                            CNHH, CNHHD, CNHL, CNHLD,
                            CNLD, CNLH, CNLL, CNMD, CNMDB, CNMDT, CNMDTS, CNMH, CNML
                            COK, CPG
                            DAOH, DAOHH, DAOL, DAOLH, DEN
                            DFCD75, DFCD7505, DFCD7550, DFCD75M, DFCD75M05, DFCD75M50
                            DFCD95, DFCD9505, DFCD9550, DFCD95M, DFCD95M05, DFCD95M50
                            DFCH75, DFCH7505, DFCH7550, DFCH75M, DFCH75M05, DFCH75M50
                            DFCH95, DFCH9505, DFCH9550, DFCH95M, DFCH95M05, DFCH95M50
                            DFCL75, DFCL7505, DFCL7550, DFCL75M, DFCL75M05, DFCL75M50
                            DFCL95, DFCL9505, DFCL9550, DFCL95M, DFCL95M05, DFCL95M50
                            DS1, DS1D, DS1DA
                            DS2, DS2D, DS2DA
                            DS3, DS3D, DS3DA
                            DS4, DS4D, DS4DA
                            DS5, DS5D, DS5DA
                            DS6, DS6D, DS6DA
                            DS7, DS7D, DS7DA
                            DS8, DS8D, DS8DA
                            DS9, DS9D, DS9DA
                            DSNDH, DSNDL
                            FCD75, FCD7505, FCD7550, FCD75M, FCD75M05, FCD75M50
                            FCD95, FCD9505, FCD9550, FCD95M, FCD95M05, FCD95M50
                            FCH75, FCH7505, FCH7550, FCH75M, FCH75M05, FCH75M50
                            FCH95, FCH9505, FCH9550, FCH95M, FCH95M05, FCH95M50
                            FCL75, FCL7505, FCL7550, FCL75M, FCL75M05, FCL75M50
                            FCL95, FCL9505, FCL9550, FCL95M, FCL95M05, FCL95M50
                            GO1H, GO2H, GO3H, GO4H, GO5H, GO6H, GO7H, GO8H, GO9H
                            H2, H2U
                            HCBOTH, HCBOTL, HCDSTH, HCDSTL, HCHND, HCHNH, HCHNL, HCKERH, HCKERL
                            HCLNH, HCLNHS, HCLNL, HCLNLS, HCMND, HCMNH, HCMNL, HCND, HCNH, HCNL
                            HDS1, HDS1D, HDS1DA
                            HDS2, HDS2D, HDS2DA
                            HDS3, HDS3D, HDS3DA
                            HDS4, HDS4D, HDS4DA
                            HDS5, HDS5D, HDS5DA
                            HDS6, HDS6D, HDS6DA
                            HDS7, HDS7D, HDS7DA
                            HDS8, HDS8D, HDS8DA
                            HDS9, HDS9D, HDS9DA
                            HG1, HG2, HG3, HG4, HG5, HG6, HG7, HG8, HG9
                            HN1, HN1D
                            HN2, HN2D
                            HN3, HN3D
                            HN4, HN4D
                            HN5, HN5D
                            HN6, HN6D
                            HN7, HN7D
                            HN8, HN8D
                            HN9, HN9D
                            IC4C, IC4S, IC4U, IC4ngl
                            IC6H, IC6HT, IC6T
                            ISOP, ISOB, ISON, ISOT, ISOTB, ISOTN
                            KR1, KR1D
                            KR2, KR2D
                            KR3, KR3D
                            KR4, KR4D
                            KR5, KR5D
                            KR6, KR6D
                            KR7, KR7D
                            KR8, KR8D
                            KR9, KR9D
                            LCOH, LCOHD, LCOHDA
                            LCOL, LCOLD, LCOLDA
                            LCOM, LCOMD, LCOMDA
                            LG1, LG2, LG3, LG4, LG5, LG6, LG7, LG8, LG9
                            LN1, LN2, LN3, LN4, LN5, LN6, LN7, LN8, LN9
                            LNDHH, LNDHHB, LNDHHT, LNDHL, LNDHLB, LNDHLT, LNDHTS
                            LNDLH, LNDLHB, LNDLHT, LNDLL, LNDLLB, LNDLLT, LNDLTS
                            LUB, LUBX
                            MET, MNAP, MNDHH, MNDHL, MNDLH, MNDLL
                            NAPDA, NC4, NC4C, NC4S, NC4U, NATngl, NATD
                            PGSS, PGSU
                            R100, R105, R90, R95, R95N, R95B
                            RAF100, RAF90, RAF95, RDH, RNH, , RJH, FBD
                            RS100, RS105, RS90, RS95
                            SN1, SN2, SN3, SN4, SN5, SN6, SN7, SN8, SN9
                            SNDH, SNDL
                            TARH, TARL
                            UC3, UC3U, UC4, UC4C, UC4U
                            V100, V90, V95, V95N, V95B
                            VR1, VR2, VR3, VR4, VR5, VR6, VR7, VR8, VR9
                            VS100, VS90, VS95
                            VSW1, VSW2, VSW3, VSW4, VSW5, VSW6, VSW7, VSW8, VSW9
                            W100, W90, W95, W95N, W95B, WS100, WS90, WS95
                            BNZ, TOL, XYL
                            CBOB, CarbDSU, CaRBOB, CATC, CC3, DSL, DSU, JTA, N2H, N6B, N6I, PGS, RBOB
                            AR1, AR2, AR3, AR4, AR5, AR6, AR7, AR8, AR9, AR10, AR11
                            ASPHout, AVGout, BTXout, COKout, LUBout, PCFout, SULout
                            LTE1, LTE2, LTE3, LTE4, LTE5, LTE6, LTE7, LTE8, LTE9, LTE10, LTE11  /

Gain_Process(Process)       Processes to be included in Gain Calculation
                            / ACU, ALK, ARP, BSA, C4I, CPL, CSU, DC4, DC5, DDA, DDS, FCC
                              FDS, FGS, GDS, HCD, KRD, LNS, LUB, LTE
                              NDS, PHI, RCR, RSR, SDA, SGP, TRI, TRS, UGP, VCU /
;


set
  CO2Stream(Stream)           CO2 stream                               / CO2 /
  RecipeSteam(Stream)         Steam input stream for recipe blending   / STM /
;



set RFSFuelCredCat        Reporting categories for RFS credit reporting
                      / 'Corn Ethanol'
                        'Advanced Ethanol'
                        'Cellulosic Ethanol'
                        'Imported Ethanol'
                        'Biodiesel'
                        'Renewable Diesel'
                        'Renewable Gasoline'
                        'BTL - FT'
                        'BTL - Pyrolysis'
                        'CBTL'
                        'Safety Valve'
                        'Biobutanol'
                        'Category Total' /
;

set RFSFuelCredCat_M13(RFSFuelCredCat,M13)    Map RFS fuel reporting categories to M13 index for NEMS
                      / 'Corn Ethanol'.01_M13
                        'Advanced Ethanol'.02_M13
                        'Cellulosic Ethanol'.03_M13
                        'Imported Ethanol'.04_M13
                        'Biodiesel'.05_M13
                        'Renewable Diesel'.06_M13
                        'Renewable Gasoline'.07_M13
                        'BTL - FT'.08_M13
                        'BTL - Pyrolysis'.09_M13
                        'CBTL'.10_M13
                        'Safety Valve'.11_M13
                        'Biobutanol'.12_M13
                        'Category Total'.13_M13 /
;


set
CTLLiq(IntStream)           CTL output streams / CNL 'Coal Based FT Light Naphtha'
                                                 CNH 'Coal Based FT Heavy Naphtha'
                                                 CKE 'Coal Based FT Kerosene'
                                                 CDS 'Coal Based FT Diesel'  /

CBTLLiq(IntStream)          CBTL output streams /CBNL 'Coal and Biomass Based FT Light Naphtha'
                                                 CBNH 'Coal and Biomass Based FT Heavy Naphtha'
                                                 CBKE 'Coal and Biomass Based FT Kerosene'
                                                 CBDS 'Coal and Biomass Based FT Diesel'  /

BTLLiq(IntStream)           BTL output streams / BNL 'Biomass Based FT Light Naphtha'
                                                 BNH 'Biomass Based FT Heavy Naphtha'
                                                 BKE 'Biomass Based FT Kerosene'
                                                 BDS 'Biomass Based FT Diesel'  /

GTLLiq(IntStream)           GTL output streams / GNL 'NG Based FT Light Naphtha'
                                                 GNH 'NG Based FT Heavy Naphtha'
                                                 GKE 'NG Based FT Kerosene'
                                                 GDS 'NG Based FT Diesel'  /
;


Set

ImpStr(Stream)            Set of finished product import streams
                                              / ASPHout
                                                AVGout
                                                CaRBOBout
                                                CarbDSUout
                                                CFGout
                                                DSLout
                                                DSUout
                                                JTAout
                                                LUBout
                                                N2Hout
                                                N6Bout
                                                N6Iout
                                                PCFout
                                                RFGout
                                                CBOB
                                                RBOB
                                                MET
                                                AR3
                                                GO3
                                                MN3  /

ProdImpStr(Stream)        Set of finished product import streams
                                              / ASPHout
                                                AVGout
                                                CaRBOBout
                                                CarbDSUout
                                                CFGout
                                                COKout
                                                DSLout
                                                DSUout
                                                JTAout
                                                LUBout
                                                N2Hout
                                                N6Bout
                                                N6Iout
                                                PCFout
                                                RFGout  /

*BlendImpStr(Stream)       Set of blending import streams
*                                              / CBOB
*                                                RBOB /

UFOImpStr(Stream)         Set of unfinished oil import streams
                                              / AR3
                                                GO3
                                                MN3  /
;

set AltStream(Stream)             Alternative fuel streams except ethanols
                                 / BDS, BKE, BNH, BNL
                                   CDS, CKE, CNH, CNL
                                   GDS, GKE, GNH, GNL
                                   BPN, BPD, FBD, RNH, RDH, RJH, IBA  /
;



set

M4_2_CTL(M4,CTLLiq)         Map generic header to CTL liquid streams
                                               / 1_M4 . CNL
                                                 2_M4 . CNH
                                                 3_M4 . CKE
                                                 4_M4 . CDS    /

M4_2_CBTL(M4,CBTLLiq)         Map generic header to BTL liquid streams
                                               / 1_M4 . CBNL
                                                 2_M4 . CBNH
                                                 3_M4 . CBKE
                                                 4_M4 . CBDS   /

M4_2_BTL(M4,BTLLiq)         Map generic header to BTL liquid streams
                                               / 1_M4 . BNL
                                                 2_M4 . BNH
                                                 3_M4 . BKE
                                                 4_M4 . BDS   /

M4_2_GTL(M4,GTLLiq)         Map generic header to BTL liquid streams
                                               / 1_M4 . GNL
                                                 2_M4 . GNH
                                                 3_M4 . GKE
                                                 4_M4 . GDS   /




Parameters
     Gain_Detail(RefReg,RefType,Stream,t)                     Gain - Loss by Stream
     Gain_ToSpec(RefReg,RefType,Stream,t)                     Gain - Products into SpecBlending and Products out
     Crude_Detail(RefReg,RefType,Stream,t)                     Gain - Loss by Stream
     Total_Gain(Gain_Streams,Gain_Process)                    Total Gain - Loss by Process and Stream

*    Parameters needed to display net revenues for refineries
     nr_r_OpCost(RefReg,RefType,Process,t)                    Variable Operating Costs
     nr_q_OpCost(RefReg,RefType,Process,t)                    Activity of Processes
     nr_p_Stream(RefReg,RefType,Stream,t)                     Dual Value of Stream Balance Rows
     nr_q_Stream(RefReg,RefType,Stream,t)                     Net Flow on Stream Balances
     nr_p_Process(RefReg,RefType,Process,t)                   Dual Value of Capacity Balance Rows
     nr_q_Process(RefReg,RefType,Process,t)                   Net Activity of Processes
;





*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* Table 21 items

Sets
  StreamType         Set of All Stream Types
   / Feedstocks, Intermediates, Products, EndProducts  /


  Industry           Set of All Industries
    / Petroleum, Biofuels, NonPetroleumFossil, Blending   /

  StreamNames        Set of all Stream Names
    /
    Crude
    DomesticCrude
    CrudeImports
    UFOImports
    GasolineImports
    GasolineExports
    BOBImports
    BOBExports
    IndustrialFeedNetImports
    DistillateImports
    DistillateExports
    ResidImports
    ResidExports
    ChemicalImports
    ChemicalExports
    Grains
    Cellulose
    Oils
    Methanol
    Electricity
    EthanolImports
    EthanolExports
    BiobasedDieselImports
    Coal
    Gas
    GasFeed
    GasFuel
    GasLiquids
    DomesticNGPL
    NGPLImports
    NGLExports
    LightGases
    Alkylate
    Reformate
    NaphthaLight
    NaphthaMedium
    NaphthaHeavy
    CatGasoline
    Isomerate
    PolyGasoline
    Naphtha
    Kerosene
    Diesel
    DieselHeavy
    LightCycleOil
    GasOilLight
    GasOil
    GasOilHeavy
    Resid
    Ethanol
    Distillate
    Chemicals
    NonFuelStreams
    Propane
    Gasoline
    CBOB
    RBOB
    CaRBOB
    ULSD
    LowSulfurDiesel
    CarbULSD
    HeatingOil
    JetFuel
    E10CFGEthanol
    E10RFGEthanol
    E15CFGEthanol
    E15RFGEthanol
    E10CarbRFGEthanol
    E85Ethanol
    B20DSU
    B20DSL
    B20CarbDSU
    B20N2H
    E10BOB
    E15BOB
    E85BOB
    NonFAMEDist
    B20Dist
    ResidFuel /

  CostType  Set of process dependent costs and levels
   /  FuelYield
      FeedUse
      CoproductYield
      Nameplate
      ProductionCapacity
      Production
      CapacityFactor
      BuildYears
      EquipmentCost
      FieldCost
      FixedCapitalInvestment
      BaseOvernight
      OvernightCapex
      TotalProjectCost
      ProjectInvestment
      AfterTaxCreditAnnuity
      BuildCost
      FeedstockCost
      FixedCapitalCost
      FIxedLaborCost
      FixedOpCost
      VariableOpCost
      ElectricityUse
      ElectricitySales    /
;



* Note: GasProd is identical to EndProductGas ????
set GasProd(RecipeProd)         Gasoline final products
 /   CFGout
     CFG15out
     RFGout
     RFG15out
     E85out
     CaRBOBout /
;

set DistProd(RecipeProd)        Distillate final products
 /   DSLout     'Low Sulfur Diesel'
     CarbDSUout 'California diesel'
     N2Hout     '#2 Heating oil'
     JTAout     'Jet Fuel'
     DSUout     'Ultra-low sulfur diesel' /
;

set Feedstocks(IntStream)       Feedstocks
  /   CRN
      GRN
      AGR
      ECR
      NFR
      UWW
      FCO
      WGR
      YGR
      COA
      NGS /
;

set FuelStreams(IntStream)      Liquid fuel streams produced by conversion units
  /   ETHCRN
      ETHCRNexp
      ETHGRN
      ETHAET
      ETHCLE
      BNL
      BNH
      BKE
      BDS
      BPN
      BPD
      CBNL
      CBNH
      CBKE
      CBDS
      CNL
      CNH
      CKE
      CDS
      GNL
      GNH
      GKE
      GDS
      IBA
      ETHCCS/
;

set NonFuelStreams(IntStream)   Streams produced by conversion units whose destination is other industries
  /  DDG
     GLY
     BCH
     BCK
     GCK
     SLG
     /
;


set RefinedProduct(Stream)      All products be they recipe or coproduct
  / set.RecipeProd, set.Coproduct /

set Chemicals(RefinedProduct)   Chemical products
 /  CC2out   'Ethane for sale to other industrial facilities'
    LPGout   'Propane for sale to other industrial facilities'
    UC3out   'Propylene for sale to other industrial facilities'
    GLYout   'Glycerine from biodiesel plants'
    NC4out   'n-butane for sale to other industrial facilities'
    IC4out   'isobutane for sale to other industrial facilities'
    NATout   'pentanes plus for sale to other industrial facilities'
    PCFout   'naphtha for chemical feedstock use'
    LUBout   'Base oils for sale to lube plants'
    ASPHout  'Petroleum based asphalt'
    COKout   'Petroleum coke'
    DDGout   'Dried distillers grains'
    DGSout   'Wet distillers grains' /
;


set LiquidChem(Chemicals)       Liquid chemical products
  /  CC2out   'Ethane for sale to other industrial facilities'
     LPGout   'Propane for sale to other industrial facilities'
     UC3out   'Propylene for sale to other industrial facilities'
     GLYout   'Glycerine from biodiesel plants'
     NC4out   'n-butane for sale to other industrial facilities'
     IC4out   'isobutane for sale to other industrial facilities'
     NATout   'pentanes plus for sale to other industrial facilities'
     PCFout   'naphtha for chemical feedstock use'
     LUBout   'Base oils for sale to lube plants'  /
;

set LiquefiedGases(LiquidChem) Light products which can be liqufied by increased pressure or temperature in industrial settings
  /  CC2out
     LPGout
     UC3out
     NC4out
     IC4out /

set RTLiquids(LiquidChem)       Chemicals that are liquid at room temperature
  / PCFout
    LUBout /

set Solids(Chemicals)           Solid petroleum products
  /  ASPHout  'Petroleum based asphalt'
     COKout   'Petroleum coke' /
;

set BioSolids(Chemicals)        Solid bio-based coproducts
  / DDGout
    DGSout /

sets
  BalanceRepLabel                  Labels for refinery balance report
    /'Crude Inputs'
     'Other Supply'
     'Other Non-Pet'
     'Refinery Gain'
     'Consumption'
     'Fuel Use'
     'Discrepency'  /

   NGLInputLabels                   Labels for NGL sources
    /'Refinery'
     'Gas Plant'
     'Imports' /

   NGLOutputLabels                  Labels for NGL outputs from LFMM
    /'Exports'
     'Industrial'
     'Refinery Use' /
;


sets
  Distillate(Stream)          Distillate intermediate products
  /  DSU      'EPA ultra-low sulfur diesel'
     DSL      'EPA low sulfur diesel'
     CarbDSU  'California ultra-low sulfur diesel'
     N2H      'EPA heating oil'
     JTA      'Spec Jet fuel'
     CDS      'Coal based diesel'
     CBDS     'Coal/biomass based diesel'
     GDS      'Natural gas based diesel'
     FBD      'FAME biodiesel'
     BDS      'Cellulosic diesel'
     RDH      'Hydrotreated renewable distillate'
     RJH      'HEFA-SPK renewable jet' /

  ResidFuel(RecipeProd)       #6 Heating Oils
    / N6Iout    'Intermediate sulfur'
      N6Bout    'High Sulfur' /

  IndustrialFeeds(Stream)     Petrochemical Feedstock streams
  /  CC2out 'Ethane'
     NC4out 'n-butane'
     NATout 'Natural gasoline'
     PCFout 'Naphtha for petrochemical use'
     LUBout 'Lubricants' /

  GrainCrops(IntStream)       Starch feedstocks
  /  CRN 'Corn'
     GRN 'Other grain based starches' /

  RenewableOils(IntStream)    Biobased diesel feedstocks
  /  FCO 'Virgin seed oils (e.g. soy, corn, and canola)'
     YGR 'High Free Fatty acid waste grease (e.g. Yellow Grease)'
     WGR 'Low Free Fatty acid waste grease  (e.g. Tallow)' /

  Coal(IntStream)             Coal and feedstocks
  /  COA 'Coal' /

  Gas(IntStream)              Dry natural gas feedstock
  / NGS 'Dry Natural Gas' /

  ChemIntermediates(IntStream) Chemical Intermediate streams
  / CC3 'Propane'
    NC4 'n-butane'
    IC4 'isobutane'
    NATngl /

  BioProcess(Process)         All biofuel process units
   / EDH 'Corn ethanol high yield'
     EDM 'Corn ethanol low yield'
     NCE 'Non-corn non-advanced ethanol'
     AET 'Non-corn advanced ethanol'
     FBD 'FAME Biodiesel'
     GDT 'Green diesel hydrotreater'
     SAF 'Sustainable aviation fuel'
     CLE 'Cellulosic ethanol'
     BTL 'Cellulosic distillate'
     BPU 'Cellulosic gasoline'
     IBA 'Biobutanol Pseudo-Unit'
     ETHCCS 'Ethanol Retrofit Pseudo-Unit'  /

  NPFProcess(Process)         CTL and GTL Processes
   /  CTL 'Coal to Liquids process'
      CBL 'Coal/biomass to liquids'
      GTL 'Gas to liquids process' /

  BiofuelImports(IntStream)   Biofuel streams that can be imported
  /  ETHAET 'Sugarcane ethanol'
     FBD    'Fatty acid methyl ester biodiesel' /

  BioNaphtha(IntStream)       Biobased gasoline and chemical blendstocks
  /  BNH   'Biomass Based FT Heavy Naphtha'
     BPN   'Gasoline from light hydrocarbon based cellulosic biomass'
     ISOB  'C5/C6 Isomerate'
     ISOTB 'C5/C6 total recycle isomerate'
     R95B  'R Reformate 95 octane'
     V95B  'V Reformate 95 octane'
     W95B  'W Reformate 95 octane'
     RNH   'Gasoline from renewable oils'
     BNL   'Biomass based LPG/Petrochem feed' /

  BioDistillate(IntStream)    Biobased distillate streams
  /  FBD  'Fatty Acid Methyl Ester biodiesel'
     RDH  'Diesel from renewable oils'
     RJH  'HEFA-SPK renewable jet'
     BPD
     BDS  'Biomass based Diesel'
     BKE  'Biomass based Kerojet' /

  NPFNaphtha(IntStream)      Non-petroleum fossil gasoline and chemical blendstocks
  /  CNH   'Coal based heavy naphtha'
     CBNH  'Coal/biomass based heavy naphtha'
     GNH   'Natural gas based heavy naphtha'
     ISON
     ISOTN
     R95N
     V95N
     W95N
     CNL   'Coalbased LPG/petrochem feed'
     CBNL  'Coal/biomass based LPG/petrochem feed'
     GNL   'Natural gas based LPG/petrochem feed' /

  NPFDistillate(IntStream)        Non-petroleum fossil distillate streams
  /   CDS  'Coal based diesel'
      GDS  'Natural gas based diesel'
      CBDS
      CKE  'Coal based kerojet'
      GKE  'Natural gas based kerojet'
      CBKE   /
;

set BioDieselStr(Stream)          Biodiesel stream
  / FBD /
;


Set E10(RecipeProd)               10% Ethanol blended gasoline streams
  / CFGout, RFGout, CaRBOBout /

Set E15(RecipeProd)               15% Ethanol blended gasoline streams
 / CFG15out, RFG15out /

Set Diesel(RecipeProd)            #2 Diesel blended products (including biodiesel and renewable diesel)
 / DSUout, DSLout, CarbDSUout /

set
  CRBOB(GasSpecProd)
 /CBOB,RBOB/
;



*alias(rmBlendGas, GasolineBlends);
set GasolineBlends(RCPMode)     Gasoline blends
 /RCP_CFG10a  'Conventional E10 from corn'
  RCP_CFG10b
  RCP_CFG10c  'Conventional E10 from cellulose'
  RCP_CFG10d  'Conventional E10 from non-corn advanced starch'
  RCP_CFG10e  'Conventional E10 from cellulose'
  RCP_CFG10f  'Conventional E10 from brazilian advanced'
  RCP_CFGb16  'Conventional Biobutanol 16%'
  RCP_RFG10a  'Reformulated E10 from corn'
  RCP_RFG10b
  RCP_RFG10c  'Reformulated E10 from cellulose'
  RCP_RFG10d  'Reformulated E10 from non-corn advanced starch'
  RCP_RFG10e
  RCP_RFG10f
  RCP_RFGb16  'Reformulated Biobutanol 16%'
  RCP_CFG15a  'Conventional E15 from corn'
  RCP_CFG15b  'Conventional E15 from cellulose'
  RCP_CFG15c  'Conventional E15 from cellulose'
  RCP_CFG15d  'Conventional E15 from non-corn advanced starch'
  RCP_CFG15e
  RCP_CFG15f  'Conventional E15 from non-corn advanced starch'
  RCP_RFG15a  'Reformulated E15 from corn'
  RCP_RFG15b
  RCP_RFG15c  'Reformulated E15 from cellulose'
  RCP_RFG15d  'Reformulated E15 from non-corn advanced starch'
  RCP_RFG15e
  RCP_RFG15f  'Reformulated from brazilian advanced'
  RCP_E85a    'E85 from corn'
  RCP_E85b
  RCP_E85c    'E85 from cellulose'
  RCP_E85d    'E85 from non-corn advanced starch'
  RCP_E85e
  RCP_E85f    'E85 from brazilian advanced'
  RCP_CaRBOBa 'California reformulated from corn'
  RCP_CaRBOBb
  RCP_CaRBOBc 'California reformulated from cellulose'
  RCP_CaRBOBd 'California reformulated from non-corn advanced starch'
  RCP_CaRBOBe
  RCP_CaRBOBf 'California reformulated from brazilian advanced' /
;

set
  E10Blends(GasolineBlends)   E10 blends
    /RCP_CFG10a  'Conventional E10 from corn'
     RCP_CFG10b  'Conventional E10 from US advanced'
     RCP_CFG10c  'Conventional E10 from cellulose'
     RCP_CFG10d  'Conventional E10 from non-corn advanced starch'
     RCP_CFG10e  'Conventional E10 for export from corn'
     RCP_CFG10f  'Conventional E10 from brazilian advanced'
     RCP_CFGb16  'Conventional Biobutanol 16%'
     RCP_RFG10a  'Reformulated E10 from corn'
     RCP_RFG10b  'Reformulated E10 from US advanced'
     RCP_RFG10c  'Reformulated E10 from cellulose'
     RCP_RFG10d  'Reformulated E10 from non-corn advanced starch'
     RCP_RFG10e  'Reformulated E10 for export from corn'
     RCP_RFG10f  'Reformulated E10 from brazilian advanced'
     RCP_RFGb16  'Reformulated Biobutanol 16%'
     RCP_CaRBOBa 'California reformulated from corn'
     RCP_CaRBOBb
     RCP_CaRBOBc 'California reformulated from cellulose'
     RCP_CaRBOBd 'California reformulated from non-corn advanced starch'
     RCP_CaRBOBe
     RCP_CaRBOBf 'California reformulated from brazilian advanced' /

  E15Blends(GasolineBlends)   E15 blends
    /RCP_CFG15a  'Conventional E15 from corn'
    RCP_CFG15b
    RCP_CFG15c  'Conventional E15 from cellulose'
    RCP_CFG15d  'Conventional E15 from non-corn advanced starch'
    RCP_CFG15e  'Conventional E15 for export from corn'
    RCP_CFG15f  'Conventional E15 from brazilian advanced'
    RCP_RFG15a  'Reformulated E15 from corn'
    RCP_RFG15b
    RCP_RFG15c  'Reformulated E15 from cellulose'
    RCP_RFG15d  'Reformulated E15 from non-corn advanced starch'
    RCP_RFG15e
    RCP_RFG15f  /
;


*set E10CFGBlends; alias(rmBlendGasCFG10,E10CFGBlends);
*set E10RFGBlends; alias(rmBlendGasRFG10,E10RFGBlends);
*set E10CarbRFGBlends; alias(rmBlendGasCarBOB10,E10CarbRFGBlends);


set E10CFGBlends(E10Blends)   10% ethanol blends into conventional gasoline
  /  RCP_CFG10a  'Conventional E10 from corn'
     RCP_CFG10b
     RCP_CFG10c  'Conventional E10 from cellulose'
     RCP_CFG10d  'Conventional E10 from non-corn advanced starch'
     RCP_CFG10e  'Conventional E10 from cellulose'
     RCP_CFG10f  'Conventional E10 from brazilian advanced'
     RCP_CFGb16  'Conventional Biobutanol 16%' /
;

set E10RFGBlends(E10Blends)   10% ethanol blends into reformulated gasoline
  /  RCP_RFG10a
     RCP_RFG10b
     RCP_RFG10c  'Reformulated E10 from cellulose'
     RCP_RFG10d  'Reformulated E10 from non-corn advanced starch'
     RCP_RFG10e
     RCP_RFG10f
     RCP_RFGb16  'Reformulated Biobutanol 16%' /
;


set E10CarbRFGBlends(E10Blends)   10% ethanol blends into conventional gasoline
  / RCP_CaRBOBa 'California reformulated from corn'
    RCP_CaRBOBb
    RCP_CaRBOBc 'California reformulated from cellulose'
    RCP_CaRBOBd 'California reformulated from non-corn advanced starch'
    RCP_CaRBOBe
    RCP_CaRBOBf 'California reformulated from brazilian advanced' /
;


set E15CFGBlends(GasolineBlends)   15% ethanol-motor gasoline blends
  /  RCP_CFG15a  'Conventional E15 from corn'
     RCP_CFG15b  'Conventional E15 from cellulose'
     RCP_CFG15c  'Conventional E15 from cellulose'
     RCP_CFG15d  'Conventional E15 from non-corn advanced starch'
     RCP_CFG15e
     RCP_CFG15f  'Conventional E15 from non-corn advanced starch' /
;


set E15RFGBlends(E15Blends)   15% ethanol-motor gasoline blends
  /  RCP_RFG15a  'Reformulated E15 from corn'
     RCP_RFG15b
     RCP_RFG15c  'Reformulated E15 from cellulose'
     RCP_RFG15d  'Reformulated E15 from non-corn advanced starch'
     RCP_RFG15e
     RCP_RFG15f  'Reformulated from brazilian advanced' /
;


set E85Blends(GasolineBlends)   Ethanol motor gasoline blends whose ethanol content averages 74% over the course of a year
  /  RCP_E85a    'E85 from corn'
     RCP_E85b
     RCP_E85c    'E85 from cellulose'
     RCP_E85d    'E85 from non-corn advanced starch'
     RCP_E85e
     RCP_E85f    'E85 from brazilian advanced' /
;


set DistBlends(RCPMode)         Distillate blends
  /RCP_CARBDSU0 'California ultralow sulfur petroleum diesel'
   RCP_CARBDSU1 'California ultralow sulfur B20'
   RCP_CARBDSU2 'California ultralow sulfur renewable diesel RDH'
   RCP_DSL0     'Low sulfur petroleum diesel'
   RCP_DSL1     'Low sulfur B20'
   RCP_DSU0     'Ultralow sulfur petroleum diesel'
   RCP_DSU1     'Ultralow sulfur B20'
   RCP_DSU2     'Ultralow sulfur renewable diesel RDH'
   RCP_JTA0     'Petroleum jet fuel'
   RCP_JTA1     'SAF 25% blend with petroleum jet fuel'
   RCP_JTA2     'SAF 10% blend with petroleum jet fuel'
   RCP_JTA3     'SAF 1% blend with petroleum jet fuel'
   RCP_N2H0     '#2 Petroleum petroleum heating oil'
   RCP_N2H1     '#2 Heating oil B20'
   RCP_N6I0
   RCP_N6B0  /
;


set NonFAMEBlends(DistBlends)   Distillate blends with no FAME biodiesel
  / RCP_CARBDSU0 'California ultralow sulfur petroleum diesel'
    RCP_DSL0     'Low sulfur petroleum diesel'
    RCP_DSU0     'Ultralow sulfur petroleum diesel'
    RCP_JTA0     'Petroleum jet fuel'
    RCP_JTA1     'SAF 25% blend with petroleum jet fuel'
    RCP_JTA2     'SAF 10% blend with petroleum jet fuel'
    RCP_JTA3     'SAF 1% blend with petroleum jet fuel'
    RCP_N2H0     '#2 Petroleum petroleum heating oil' /
;


set B20Blends(DistBlends)       Distillate blends with 20% FAME biodiesel
  / RCP_CARBDSU1 'California ultralow sulfur B20'
    RCP_DSL1     'Low sulfur B20'
    RCP_DSU1     'Ultralow sulfur B20'
    RCP_N2H1     '#2 Heating oil B20' /
;

set RDHBlends(DistBlends)       Distillate blends only with renewable diesel RDH
  / RCP_CARBDSU2 'California ultralow sulfur renewable diesel RDH'
    RCP_DSU2     'Ultralow sulfur renewable diesel RDH' /
;

set
  PetChemBlends(RcpMode)      Petrochemical blending modes                                            /
* RCP_PGSa  'Methane/still gas'
   RCP_CC3a  'Propane'
   RCP_UC3   'Propylene'
   RCP_NC4a  'n-butane'
   RCP_IC4a  'isobutane'
   RCP_C5P0  'C5 paraffins'
   RCP_C5P1  'C5 paraffins'
   RCP_C5P2  'C5 unsaturated mix'
   RCP_BNZ   'Benzene'
   RCP_TOL   'Toluene'
   RCP_XYL   'Xylene'
   RCP_SPN0  'Special naphtha'
   RCP_LUB   'Base oils for lubricants'
   RCP_GOP   'Gas oil as lube for air compressors'
   RCP_COKa  'Petroleum coke for sales'
   RCP_COKb  'Petroleum coke for disposal'
   RCP_SUL   'Elemental Sulfur for sales'
   RCP_ASPH0 'Asphalt from vacuum residue a'
   RCP_ASPH1 'Asphalt from vacuum residue b'
   RCP_ASPH2 'Asphalt from vacuum residue c'
   RCP_ASPH3 'Asphalt from low sulfur SDA tar'
   RCP_ASPH4 'Asphalt from high sulfur SDA tar' /

NPFChemBlends(RcpMode)      NonPetroleum fossil chemical blending modes
  / RCP_CC2   'Ethane'
    RCP_CC3b  'Propane'
    RCP_NC4b  'n-butane'
    RCP_IC4b  'isobutane'
    RCP_NAT   'Natural gasoline'
    RCP_SLG0  'Vitreous slag for sale' /

BioChemBlends(RcpMode)      Biochemical blending modes
  / RCP_DDG0  'Dried distillers grains'
    RCP_GLY0  'Glycerin from biodiesel plants'
    RCP_BCK0  'Biobased coke for sale/disposal'
    RCP_BCH0  'Biobased soil ammendment for sale/disposal' /

UnfinishedOils(Stream)      Unfinished oil streams for import and export
  / AR3       'Atmospheric Residuum'
    GO3       'Gas Oil'
    MN3       'Medium Naphtha' /


Gasoline(Stream)            Gasoline and blending component streams
  / CFGout    'Conventional gasoline'
    CaRBOBout 'California gasoline'
    RFGout    'Reformulated gasoline'
    CFG15out  'Conventional E15'
    RFG15out  'Reformulated E15'
    E85out    'E85' /
;

*CoProduct_Bal_Str(CoProduct) Co-product streams used for refinery balance calculation
*                            / COKout, GOPout, IC4out, LPGout, NC4out, PGSout, UC3out /

set
  CoProduct_Bal_Str(CoProduct) Co-product streams used for refinery balance calculation
    / GOPout, COKdump /
;


Parameters
   repMassBalance(Process,ProcessMode)                              Mass balance check for each ProcessMode
   repMassBalanceIn(Process,ProcessMode)                            Mass inputs for each ProcessMode
   repMassBalanceOut(Process,ProcessMode)                           Mass outputs for each ProcessMode

   repProcModeVolBal(Process,ProcessMode)                           Volume gain by process and mode
   repPetOutput(RefReg,Stream,Period,t)                             Refinery output streams used for balance calculation (M bbl per day)
   repPetFuelUse(RefReg,Stream,Period,t)                            Refinery fuel-use streams used for balance calculation (M bbl per day)
   repProcessGain(RefReg,Process,Period,t)                          Refinery process gain used for balance calculation (M bbl per day)
   repOtherSupply(RefReg,Stream,Period,t)                           Refinery other petroleum input streams used for balance calculation (M bbl per day)
   repNonPetOther(RefReg,Stream,Period,t)                           Refinery other non-petroleum input streams used for balance calculation (M bbl per day)
   repCrudeInputs(RefReg,Crude,Period,t)                            Refinery crude input streams used for balance calculation (M bbl per day)

   repCrudeInputsTotal(Period,t)                                    National crude use total (MM bbl per day)
   repOtherSupplyTotal(Period,t)                                    National other supply total (MM bbl per day)
   repNonPetOtherTotal(Period,t)                                    National non-petroleum supply total (MM bbl per day)
   repProcessGainTotal(Period,t)                                    National refinery gain total (MM bbl per day)
   repPetOutputTotal(Period,t)                                      National output total (MM bbl per day)
   repPetFuelUseTotal(Period,t)                                     National fuel use total (MM bbl per day)
   repBalanceRep(BalanceRepLabel,Period,t)                          Aggregated refinery balance report (MM bbl per day)
   repNGLSource(NGLInputLabels,Period,t)
   repNGLConsumption(NGLOutputLabels,Period,t)

   repDiscrepency(Period,t)                                         Refinery discrepency (supply - consumption)
   repUtilization(Process,RefReg,RefType,Period,t)                  Process Utilization Report [0-1]
   repActivity(RefReg,RefType,Stream,Process,ProcessMode,Period,t)  Activity Report
   repCO2Emissions(RefReg,RefType,Period,t)                         Refinery CO2 emissions by supply region and refinery type (M tons per day)
   repTotalCO2(Period,t)                                            National C02 emissions (M tons per day)
   repEnergyDensity(RefReg,RefType,Stream,Period,t)                 Energy density - weighted average of output streams (BTUs)
   repRFSCredits(RFSCategory,RFSFuelCredCat,Period,t)               RFS credits by category and biofuel in billion credits per year
   repRefWSPrice(RefReg,RefType,RecipeProd,Period,t)                Wholesale RefReg-level product prices in ReportYr dollars per BBL
   repCenWSPrice(CenDiv,EndProduct,Period,t)                        Wholesale CenDiv-level product prices in ReportYr dollars per BBL
   repSpecProdPrice(RefReg,RefType,SpecProd,Period,t)               Wholesale RefReg-level spec-blended product prices in ReportYr dollars per BBL
   repRefRefTran(RefReg,RefRegA,Stream,Period,t)                    RefReg-RefReg transfers in M BBL per day
   repRefCenTran(RefReg,CenDiv,RecipeProd,Period,t)                 RefReg-CenDiv transfers in M BBL per day
   repRefProd(RefReg,RefType,RecipeProd,Period,t)                   Production levels in M BBL per day
   repRefH2Use(RefReg,RefType,Period,t)                             Refinery hydrogen purchases in TBtu
   repRefFuelUse(RefReg,RefType,Period,t)                           Refinery fuel use (trills per year)
   repRefElecPurch(RefReg,RefType,Period,t)                         Refinery electricity purchases (trills per year)
   repTotEnergyUse(RefReg,RefType,Period,t)                         Total refinery energy use (trills per year)
   repMargCrdPrice(RefReg,Crude,Period,t)                           Crude prices in ReportYr dollars per BBL
   repCrudeUse(RefReg,RefType,Crude,Period,t)                       Crude use in M BBL per day
   repCrudeRAC(RefReg,RefType,Period,t)                             Crude RAC price in ReportYr dollars per BBL
   repRACMargins(RefReg,RefType,SpecProd,Period,t)                  Spec-product RAC margins in ReportYr dollars per BBL
   repWTIMargins(RefReg,RefType,SpecProd,Period,t)                  Spec-product WTI margins in ReportYr dollars per BBL
   repSpecProp(RefReg,RefType,SpecProd,Property,Period,t)           Specification product properties
   repEnergyBalance(Process,ProcessMode)                            Energy balance check for each ProcessMode (MMBTU)  (does not work for alt fuels yet!)
   repWSAltFuelPrc(RefReg,Stream,Period,t)                          Alternative fuel stream marginal prices in ReportYr dollars per BBL
   repRFSMarkups(RFSCategory,EndProduct,Period,t)                   RFS markups by category by fuel in ReportYr dollars per BBL
;


parameters
   MMBPD_TO_BCF
   MMBPD_TO_BTONS
   repStreamFlows(DomRefReg,Stream,StreamNames,StreamType,Industry,t) FTAB Table 21: Liquid fuels consumption by sector and source
   repProcesses(RefReg,Process,Industry,CostType,t)                Costs ascribed to the objective function for a unit of activity on a process and mode
   repStreamPrices(RefReg,Stream,StreamNames,StreamType,Industry,t) Value ascribed to each stream by the LP (Balance row shadow prices)
   repStreamProperties(Stream,StreamNames,StreamType,Industry,Property)
;

MMBPD_TO_BCF = 365 * 42 / 7.48 / 1000 ;
MMBPD_TO_BTONS = 62.4 / 7.48 * 42 / 2000 * 365 / 1000 ;

Parameters
   TotalUFO                                                         Total national volume of unfinished oils
   TotalCFG                                                         Total national volume of CFG10+CFG15+CFGb16
   TotalRFG                                                         Total national volume of RFG10+RFG15+RFGb16
   TotalCaRBOB                                                      Total national volume of CaRBOB
   TotalN2H                                                         Total national volume of N2H
   TotalDSL                                                         Total national volume of DSL
   TotalDSU                                                         Total national volume of DSU
   TotalCarbDSU                                                     Total national volume of CarbDSU
   TotalE85                                                         Total national volume of E85
   TotalCTL                                                         Total national volume of CTL
   TotalBTL                                                         Total national volume of BTL

   TempSum
;


Parameters
  BiomassHeatContent(BioStr)    Higher Heating Value of various types of biomass
    / UWW 8600
      ECR 7700
      AGR 8600
      NFR 8600  /
  CoalHeatContent(CoalStr)      Higher Heating Value of Coal
    / COA 8000 /
  GasHeatContent(Gas)        Higher Heating Value of Natural Gas
    / NGS 1000  /
;


Parameters
     Gain_Detail(RefReg,RefType,Stream,t)                     Gain - Loss by Stream
     Gain_ToSpec(RefReg,RefType,Stream,t)                     Gain - Products into SpecBlending and Products out
     Crude_Detail(RefReg,RefType,Stream,t)                    Gain - Loss by Stream
     Total_Gain(Gain_Streams,Gain_Process)                    Total Gain - Loss by Process and Stream

*    Parameters needed to display net revenues for refineries
     nr_r_OpCost(RefReg,RefType,Process,t)                    Variable Operating Costs
     nr_q_OpCost(RefReg,RefType,Process,t)                    Activity of Processes
     nr_p_Stream(RefReg,RefType,Stream,t)                     Dual Value of Stream Balance Rows
     nr_q_Stream(RefReg,RefType,Stream,t)                     Net Flow on Stream Balances
     nr_p_Process(RefReg,RefType,Process,t)                   Dual Value of Capacity Balance Rows
     nr_q_Process(RefReg,RefType,Process,t)                   Net Activity of Processes
;



* Sets defining streams (originally just for reporting)
* These subsets are also used to build-up other sets prior to creation of lfminset.gdx via lfinput.gms,
* but that is separate from a NEMS run and lfprep.gms

Sets
   LightGases(IntStream)
   NaphthaLightPet(IntStream)
   NaphthaLightNPF(IntStream)
   NaphthaLightBiofuels(IntStream)
   NaphthaMedium(IntStream)
   NaphthaHeavyPet(IntStream)
   ReformatePet(IntStream)
   Alkylate(IntStream)
   GasolineCat(IntStream)
   IsomeratePet(IntStream)
   GasolinePoly(IntStream)
   NaphthaHeavyNPF(IntStream)
   NaphthaHeavyBiofuels(IntStream)
   ReformateNPF(IntStream)
   ReformateBiofuels(IntStream)
   IsomerateNPF(IntStream)
   IsomerateBiofuels(IntStream)
   KerosenePet(IntStream)
   DieselPet(IntStream)
   DieselHeavy(IntStream)
   LightCycleOil(IntStream)
   KeroseneNPF(IntStream)
   KeroseneBiofuels(IntStream)
   DieselNPF(IntStream)
   DieselBiofuels(IntStream)
   GasOilLight(IntStream)
   GasOil(IntStream)
   GasOilHeavy(IntStream)
   ResidualOil(IntStream)
   PetNaphtha(IntStream)
   PetDistillate(IntStream)
   PetResid(IntStream)
;

Sets
  AltFuelStream(Stream)
;

$ontext
$gdxin %DirPath%\input\%INPUT_FILE_SET%
$loaddc LightGases, NaphthaLightPet, NaphthaLightNPF, NaphthaLightBiofuels,
$loaddc NaphthaMedium, NaphthaHeavyPet, ReformatePet, Alkylate, GasolineCat, IsomeratePet, GasolinePoly,
$loaddc NaphthaHeavyNPF, NaphthaHeavyBiofuels, ReformateNPF, ReformateBiofuels, IsomerateNPF, IsomerateBiofuels,
$loaddc KerosenePet, DieselPet, DieselHeavy, LightCycleOil,
$loaddc KeroseneNPF, KeroseneBiofuels, DieselNPF, DieselBiofuels,
$loaddc GasOilLight, GasOil, GasOilHeavy, ResidualOil,
$loaddc PetNaphtha, PetDistillate, PetResid, AltFuelStream
$gdxin
$offtext

* End of Table 21 items
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------

