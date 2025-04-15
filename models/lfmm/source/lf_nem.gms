***** $Header: m:/default/input/RCS/lf_nem.gms,v 1.80 2020/09/11 15:34:48 EM4 Exp $
* Load data structures from NEMS needed for input to LFMM and output back to NEMS

$if not set NEM_TO_LFMM $set NEM_TO_LFMM 'NEM_TO_LFMM1.gdx'

Sets
     CRSTEP                           curve steps - crude oil
     CISTEP                           curve steps - incremental crude oil
     MCSTEP                           curve steps - Caribbean and Maritime Canada
     INSTEP                           curve steps - International product import-export steps

     MNPROD                           products
     MNCRUD                           crude oil types
     OGDIST                           natural gas plant liquid regions
     REFREG                           NEMS Refining supply regions
     MSEDYR                           SEDS years
     MNUMCR                           NEMS census regions
     MNUMC2                           NEMS census regions minus 2
     MNUMPR                           NEMS supply regions
     NUMCGF                           NEMS cogeneration fuels
     MX_NCL                           NEMS coal supply regions - Domestic
     MX_NCI                           NEMS coal supply regions - Domestic and International
     NDRGN1                           NEMS coal demand regions - position 1 is national total
     NDREGN                           NEMS coal demand region
     MX_SO2                           Maximum Number of SO2 compliance regions
     NUTSEC                           Number of electric power demand sectors
     MX_RNK                           Maximum Number of Coal Ranks
     CORNTO                           Uses for corn - ethanol and biobutanol and total
     ECPCAP                           ECP Capacity Types
     MNUMYR                           NEMS years
     MNMYRF                           NEMS years
     MNUMY3                           NEMS years
     MNXYRS                           NEMS years
     ECPFPH                           Expectation Years
     MNUMFS                           Biomass types
     MNMFS1                           Biomass types and total(1_MNMFS1)
     WDCRVS                           Supply Curve steps
     PRDEXP                           Maximum number of exported products
     MAXNF2                           Maximum number of EMM fuel regions
     MAXNFR                           Maximum number of EMM fuel regions
     S1                               Number of Foreign Non_US Crude Demand Steps / 1_S1 /
     M1                               Number of Foreign Crude Regions / 1_M1 /
     M2                               Generic column headers  / 1_M2 * 2_M2 /
     M3                               Generic column headers  / 1_M3 * 3_M3 /
     M4                               Generic column headers  / 1_M4 * 4_M4 /
     M5                               Generic column headers  / 1_M5 * 5_M5 /
     M6                               Generic column headers  / 1_M6 * 6_M6 /
     M8                               Generic column headers  / 1_M8 * 8_M8 /
     M10                              supply steps            / 01_M10 * 10_M10 /
     M11                              supply steps            / 01_M11 * 11_M11 /
     M12                              supply steps            / 01_M12 * 12_M12 /
     M13                              Generic column header   / 01_M13 * 13_M13 /
     M15                              Generic column headers  / 01_M15 * 15_M15 /
     M20                              Generic column headers  / 01_M20 * 20_M20 /
     M34                              Generic column headers  / 01_M34 * 34_M34 /
     M57                              Generic column headers  / 01_M57 * 57_M57 /
     M102                             Generic column headers  / 001 * 102 /
     M1000                            Generic column headers
     LCFS_C                           Number of LCFS Categories / 1_LCFS_C * 2_LCFS_C /


     INTREG                           Number of international product import-export regions
;
Scalars
     NCNTRL_CURCALYR             NEMS current run year
     NCNTRL_CURITR               NEMS current iteration
     NCNTRL_CURITR_orig          *TEMP* store actual NEMS iteration
     NCNTRL_FCRL                 NEMS final convergence and reporting loop flag
     NCNTRL_NCRL                 NEMS reporting only flag
     CYCLEINFO_CURIRUN           NEMS current cycle
     AEOLSTYR                    User Defined End of NEMS Horizon  /2050/
     CONVFACT_CFASQ     BTUBBL   Asphalt  and Road Oil
     CONVFACT_CFAVQ     BTUBBL   Aviation Gasoline
     CONVFACT_CFBUQ     BTUBBL   Butane
     CONVFACT_CFCELL    BTUBUS   Switchgrass (cellulosic ethanol) Btu per bushel
     CONVFACT_CFCORN    BTUBUS   Corn Btu per bushel
     CONVFACT_CFDSQ     BTUBBL   Distillate
     CONVFACT_CFEEQ     BTUBBL   Ethane
     CONVFACT_CFELQ     BTUKWH   Electricity
     CONVFACT_CFIBQ     BTUBBL   Isobutane
     CONVFACT_CFJFK     BTUBBL   Jet Fuel - Kerosene
     CONVFACT_CFJFN     BTUBBL   Jet Fuel - Naphtha
     CONVFACT_CFJOULE   Btu per million joule
     CONVFACT_CFKSQ     BTUBBL   Kerosene.
     CONVFACT_CFLUQ     BTUBBL   Lubricants
     CONVFACT_CFMEQT    BTUBBL   Methanol
     CONVFACT_CFMSQ     BTUBBL   Miscellaneous petroleum products
     CONVFACT_CFNPQ     BTUBBL   Naphthas - special or otherwise
     CONVFACT_CFOGQ     BTUBBL   Oxygenated Motor Gasoline
     CONVFACT_CFPCQ     BTUBBL   Petroleum Coke
     CONVFACT_CFPPQ     BTUBBL   Pentanes Plus
     CONVFACT_CFPRQ     BTUBBL   Propane
     CONVFACT_CFRSQ     BTUBBL   Residual Fuel
     CONVFACT_CFSGQ     BTUBBL   Still Gas
     CONVFACT_CFUSQ     BTUBBL   Plant condensate and unfractionated stream
     CONVFACT_CFWXQ     BTUBBL   Waxes

     PMMOUT_ETHNE85              Fraction ethanol makes up of E85
     PMMOUT_TRGNE85              Fraction traditional gasoline makes up of E85

;

Parameters
** NEMS product demand variables in TRILLS
     QBLK_QASIN(MNUMCR,MNUMYR)        Asphalt and road oil demand
     QBLK_QBMRF(MNUMCR,MNUMYR)        Biomass - Refinery Use
     QBLK_QCLRF(MNUMCR,MNUMYR)        Purchased Coal - Refinery  Tril Btu per Yr
     QBLK_QDSCM(MNUMCR,MNUMYR)        Distillate - Commercial
     QBLK_QDSEL(MNUMCR,MNUMYR)        Distillate - Electricity
     QBLK_QDSIN(MNUMCR,MNUMYR)        Distillate - Industrial
     QBLK_QDSRS(MNUMCR,MNUMYR)        Distillate - Residential
     QBLK_QDSTR(MNUMCR,MNUMYR)        Distillate - Transportation
     QBLK_QELRF(MNUMCR,MNUMYR)        Purchased Electricity - Refinery Tril Btu per Yr
     QBLK_QETTR(MNUMCR,MNUMYR)        E85 - Transportation
     QBLK_QJFTR(MNUMCR,MNUMYR)        Jet Fuel - Transportation
     QBLK_QKSAS(MNUMCR,MNUMYR)        Kerosene - All Sectors
     QBLK_QKSRS(MNUMCR,MNUMYR)        Kerosene - Residential
     QBLK_QKSCM(MNUMCR,MNUMYR)        Kerosene - Commercial
     QBLK_QKSIN(MNUMCR,MNUMYR)        Kerosene - Industrial
     QBLK_QLGAS(MNUMCR,MNUMYR)        LPG - All Sectors
     QBLK_QLGCM(MNUMCR,MNUMYR)        LPG - Commercial
     QBLK_QLGIN(MNUMCR,MNUMYR)        LPG - Industrial
     QBLK_QLGRF(MNUMCR,MNUMYR)        LPG - Refinery Use
     QMORE_QPRRF(MNUMCR,MNUMYR)       Propane - Refinery Use
     QMORE_QPYRF(MNUMCR,MNUMYR)       Propylene - Refinery Use
     QMORE_QBURF(MNUMCR,MNUMYR)       Butane - Refinery Use
     QMORE_QISRF(MNUMCR,MNUMYR)       Isobutane - Refinery Use
     QBLK_QLGRS(MNUMCR,MNUMYR)        LPG - Residential
     QBLK_QLGTR(MNUMCR,MNUMYR)        LPG - Transportation
     QBLK_QNGTR(MNUMCR,MNUMYR)        CNG - Transportation
     QBLK_QMGAS(MNUMCR,MNUMYR)        Gasoline - All Sectors
     QBLK_QMGCM(MNUMCR,MNUMYR)        Gasoline - Commercial
     QBLK_QMGIN(MNUMCR,MNUMYR)        Gasoline - Industrial
     QBLK_QMGTR(MNUMCR,MNUMYR)        Gasoline - Transportation
     QBLK_QNGRF(MNUMCR,MNUMYR)        Purchased Natural Gas - Refinery Tril Btu per Yr
     QBLK_QOTAS(MNUMCR,MNUMYR)        Other Petroleum - All Sectors (AVG+LUB)
     QBLK_QOTIN(MNUMCR,MNUMYR)        Other Petroleum - Lubes
     QBLK_QOTRF(MNUMCR,MNUMYR)        Other Petroleum - Refinery Use
     QBLK_QOTTR(MNUMCR,MNUMYR)        Other Petroleum - Aviation Gasoline
     QBLK_QPCAS(MNUMCR,MNUMYR)        Petroleum Coke - All Sectors
     QBLK_QPCRF(MNUMCR,MNUMYR)        Petroleum Coke - Refinery Use
     QBLK_QPFIN(MNUMCR,MNUMYR)        Petrochemical Feedstocks - Industrial
     QBLK_QRHAS(MNUMCR,MNUMYR)        High-sulfur Residual Fuel - All Sectors
     QBLK_QRHEL(MNUMCR,MNUMYR)        High-sulfur Residual Fuel - Electricity
     QBLK_QRHTR(MNUMCR,MNUMYR)        High-sulfur Residual Fuel - Transportation
     QBLK_QRLAS(MNUMCR,MNUMYR)        Low-sulfur Residual Fuel - All Sectors
     QBLK_QRLCM(MNUMCR,MNUMYR)        Low-sulfur Residual Fuel - Commercial
     QBLK_QRLEL(MNUMCR,MNUMYR)        Low-sulfur Residual Fuel - Electricity
     QBLK_QRLIN(MNUMCR,MNUMYR)        Low-sulfur Residual Fuel - Industrial
     QBLK_QRLRF(MNUMCR,MNUMYR)        Low-sulfur Residual Fuel - Refinery Use
     QBLK_QRLTR(MNUMCR,MNUMYR)        Low-sulfur Residual Fuel - Transportation
     QBLK_QSGIN(MNUMCR,MNUMYR)        Still Gas - Industrial
     QBLK_QSGRF(MNUMCR,MNUMYR)        Still Gas - Refinery Use

     QMORE_QETIN(MNUMCR,MNUMYR)       Industrial total ethane
     QMORE_QETINPF(MNUMCR,MNUMYR)     Industrial ethane feedstock
     QMORE_QPRCM(MNUMCR,MNUMYR)       Commercial propane
     QMORE_QPRIN(MNUMCR,MNUMYR)       Industrial total propane
     QMORE_QPRINPF(MNUMCR,MNUMYR)     Industrial propane feedstock
     QMORE_QPRRS(MNUMCR,MNUMYR)       Residential propane
     QMORE_QPRTR(MNUMCR,MNUMYR)       Transportation propane
     QMORE_QISIN(MNUMCR,MNUMYR)       Industrial total isobutane
     QMORE_QISINPF(MNUMCR,MNUMYR)     Industrial isobutane feedstock
     QMORE_QBUIN(MNUMCR,MNUMYR)       Industrial total normal butane
     QMORE_QBUINPF(MNUMCR,MNUMYR)     Industrial n-butane feedstock
     QMORE_QPPIN(MNUMCR,MNUMYR)       Industrial total natural gasoline
     QMORE_QPPINPF(MNUMCR,MNUMYR)     Industrial pentanes plus feedstock
     QMORE_QPROLENERF(MNUMCR,MNUMYR)  Industrial propylene feedstock
     QMORE_QLUIN(MNUMCR,MNUMYR)       Industrial lubricants

** NEMS SEDS historical data in TRILLS per year
     QSBLK_QSDSRS(MNUMCR,MNUMYR)
     QSBLK_QSLGRS(MNUMCR,MNUMYR)
     QSBLK_QSKSRS(MNUMCR,MNUMYR)
     QSBLK_QSDSCM(MNUMCR,MNUMYR)
     QSBLK_QSLGCM(MNUMCR,MNUMYR)
     QSBLK_QSMGCM(MNUMCR,MNUMYR)
     QSBLK_QSKSCM(MNUMCR,MNUMYR)
     QSBLK_QSLGTR(MNUMCR,MNUMYR)
     QSBLK_QSMGTR(MNUMCR,MNUMYR)
     QSBLK_QSETTR(MNUMCR,MNUMYR)
     QSBLK_QSDSTR(MNUMCR,MNUMYR)
     QSBLK_QSOTTR(MNUMCR,MNUMYR)
     QSBLK_QSJFTR(MNUMCR,MNUMYR)
     QSBLK_QSDSIN(MNUMCR,MNUMYR)
     QSBLK_QSKSIN(MNUMCR,MNUMYR)
     QSBLK_QSLGIN(MNUMCR,MNUMYR)
     QSBLK_QSPFIN(MNUMCR,MNUMYR)
     QSBLK_QSASIN(MNUMCR,MNUMYR)
     QSBLK_QSOTIN(MNUMCR,MNUMYR)
     QSBLK_QSSGIN(MNUMCR,MNUMYR)
     QSBLK_QSMGIN(MNUMCR,MNUMYR)

     EMISSION_EMETAX(M15,MNUMYR)      Excise (Consumption) Tax by Fuel
     EMISSION_EMBTAX(M15,MNUMYR)      Btu Tax by Fuel

     EMEBLK_EMGTR(MNUMYR)
     EMEBLK_EJFTR(MNUMYR)
     EMEBLK_EDSTR(MNUMYR)
     EMEBLK_ELGTR(MNUMYR)
     EMEBLK_ERSTR(MNUMYR)
     EMEBLK_EOTTR(MNUMYR)
     EMEBLK_EPFIN(MNUMYR)
     EMEBLK_ENQLGPF(MNUMYR)
     EMEBLK_ESGIN(MNUMYR)
     EMEBLK_EOTIN(MNUMYR)
     EMEBLK_EETTR(MNUMYR)
     EMEBLK_EAGTR(MNUMYR)
     EMEBLK_ENGIN(MNUMYR)

     INDOUT_INQLGHP(MNUMCR,MNUMYR)    Industrial LPG heat and power feedstock

     INDREP_QCCRF(M11,MNUMYR)         Refinery cat coke consumption

** NEMS EXPECTED product demand variables in TRILLS
     MXQBLK_XQDSTR(MNUMCR,MNXYRS)        Distillate - Transportation
     MXQBLK_XQDSIN(MNUMCR,MNXYRS)        Distillate - Industrial
     MXQBLK_XQDSCM(MNUMCR,MNXYRS)        Distillate - Commercial
     MXQBLK_XQDSRS(MNUMCR,MNXYRS)        Distillate - Residential
     MXQBLK_XQDSEL(MNUMCR,MNXYRS)        Distillate - Electricity
     MXQBLK_XQMGAS(MNUMCR,MNXYRS)        Gasoline - All Sectors
     MXQBLK_XQJFTR(MNUMCR,MNXYRS)        Jet Fuel - Transportation
     MXQBLK_XQKSAS(MNUMCR,MNXYRS)        Kerosene - All Sectors
     MXQBLK_XQLGAS(MNUMCR,MNXYRS)        LPG - All Sectors
     MXQBLK_XQLGRF(MNUMCR,MNXYRS)        LPG - Refinery Use
     MXQBLK_XQLGTR(MNUMCR,MNXYRS)        LPG - Transportation Use
     MXQBLK_XQLUIN(MNUMCR,MNXYRS)        Lubricants - Industrial
     MXQBLK_XQOTIN(MNUMCR,MNXYRS)        Other Petroleum - Lubes
     MXQBLK_XQNGTR(MNUMCR,MNXYRS)        CNG - Transportation Use
     MXQBLK_XQRLAS(MNUMCR,MNXYRS)        Low-sulfur Residual Fuel - All Sectors
     MXQBLK_XQRLEL(MNUMCR,MNXYRS)        Low-sulfur Residual Fuel - Electricity
     MXQBLK_XQRLRF(MNUMCR,MNXYRS)        Low-sulfur Residual Fuel - Refinery Use
     MXQBLK_XQRHAS(MNUMCR,MNXYRS)        High-sulfur Residual Fuel - All Sectors
     MXQBLK_XQRHEL(MNUMCR,MNXYRS)        High-sulfur Residual Fuel - Electricity
     MXQBLK_XQASIN(MNUMCR,MNXYRS)        Asphalt and road oil demand
     MXQBLK_XQSGIN(MNUMCR,MNXYRS)        Still Gas - Industrial
     MXQBLK_XQSGRF(MNUMCR,MNXYRS)        Still Gas - Refinery Use
     MXQBLK_XQPFIN(MNUMCR,MNXYRS)        Petrochemical Feedstocks - Industrial
     MXQBLK_XQPCAS(MNUMCR,MNXYRS)        Petroleum Coke - All Sectors
     MXQBLK_XQPCRF(MNUMCR,MNXYRS)        Petroleum Coke - Refinery Use
     MXQBLK_XQOTAS(MNUMCR,MNXYRS)        Other Petroleum - All Sectors (AVG+LUB)
     MXQBLK_XQOTRF(MNUMCR,MNXYRS)        Other Petroleum - Refinery Use
     MXQBLK_XQETTR(MNUMCR,MNXYRS)        E85 - Transportation

     MXPBLK_XPDSTR(MNUMCR,MNXYRS)        Distillate - Transportation
     MXPBLK_XPELIN(MNUMCR,MNXYRS)        Electricity - Industrial
     MXPBLK_XPGIIN(MNUMCR,MNXYRS)        Natural Gas - Industrial
     MXPBLK_XPALMG(MNUMCR,MNXYRS)        Gasoline - Wholesale price

     EMABLK_JMGTR(MNUMYR)                Carbon Tax portion of Motor Gasoline price in 87$ per mmbtu
     EMABLK_JMGIN(MNUMYR)                Carbon Tax portion of Motor Gasoline price in 87$ per mmbtu
     EMABLK_JMGCM(MNUMYR)                Carbon Tax portion of Motor Gasoline price in 87$ per mmbtu
     EMABLK_JJFTR(MNUMYR)                Carbon Tax portion of Jet Fuel price in 87$ per mmbtu
     EMABLK_JDSTR(MNUMYR)                Carbon Tax portion of Diesel price in 87$ per mmbtu
     EMABLK_JDSRS(MNUMYR)                Carbon Tax portion of Diesel price in 87$ per mmbtu
     EMABLK_JDSCM(MNUMYR)                Carbon Tax portion of Diesel price in 87$ per mmbtu
     EMABLK_JDSIN(MNUMYR)                Carbon Tax portion of Diesel price in 87$ per mmbtu
     EMABLK_JDSEL(MNUMYR)                Carbon Tax portion of Diesel price in 87$ per mmbtu
     EMABLK_JKSRS(MNUMYR)                Carbon Tax portion of Kerosene price in 87$ per mmbtu
     EMABLK_JKSCM(MNUMYR)                Carbon Tax portion of Kerosene price in 87$ per mmbtu
     EMABLK_JKSIN(MNUMYR)                Carbon Tax portion of Kerosene price in 87$ per mmbtu
     EMABLK_JLGRS(MNUMYR)                Carbon Tax portion of LPG price in 87$ per mmbtu
     EMABLK_JLGCM(MNUMYR)                Carbon Tax portion of LPG price in 87$ per mmbtu
     EMABLK_JLGTR(MNUMYR)                Carbon Tax portion of LPG price in 87$ per mmbtu
     EMABLK_JLGIN(MNUMYR)                Carbon Tax portion of LPG price in 87$ per mmbtu
     EMABLK_JNQLGPF(MNUMYR)              Carbon Tax portion of LPG price in 87$ per mmbtu
     EMABLK_JPRRS(MNUMYR)                Carbon Tax portion of Propane price in 87$ per mmbtu
     EMABLK_JPRCM(MNUMYR)                Carbon Tax portion of Propane price in 87$ per mmbtu
     EMABLK_JPRTR(MNUMYR)                Carbon Tax portion of Propane price in 87$ per mmbtu
     EMABLK_JETIN(MNUMYR)                Carbon Tax portion of Ethane price in 87$ per mmbtu
     EMABLK_JPRIN(MNUMYR)                Carbon Tax portion of Propane price in 87$ per mmbtu
     EMABLK_JBUIN(MNUMYR)                Carbon Tax portion of Butane price in 87$ per mmbtu
     EMABLK_JISIN(MNUMYR)                Carbon Tax portion of Isobutane price in 87$ per mmbtu
     EMABLK_JETINPF(MNUMYR)              Carbon Tax portion of Ethane price in 87$ per mmbtu
     EMABLK_JPRINPF(MNUMYR)              Carbon Tax portion of Propane price in 87$ per mmbtu
     EMABLK_JBUINPF(MNUMYR)              Carbon Tax portion of Butane price in 87$ per mmbtu
     EMABLK_JISINPF(MNUMYR)              Carbon Tax portion of Isobutane price in 87$ per mmbtu
     EMABLK_JRLCM(MNUMYR)                Carbon Tax portion of Residual Fuel - Low Sulfur price in 87$ per mmbtu
     EMABLK_JRLTR(MNUMYR)                Carbon Tax portion of Residual Fuel - Low Sulfur price in 87$ per mmbtu
     EMABLK_JRLIN(MNUMYR)                Carbon Tax portion of Residual Fuel - Low Sulfur price in 87$ per mmbtu
     EMABLK_JRLEL(MNUMYR)                Carbon Tax portion of Residual Fuel - Low Sulfur price in 87$ per mmbtu
     EMABLK_JRHTR(MNUMYR)                Carbon Tax portion of Residual Fuel - High Sulfur price in 87$ per mmbtu
     EMABLK_JRHEL(MNUMYR)                Carbon Tax portion of Residual Fuel - High Sulfur price in 87$ per mmbtu
     EMABLK_JRSCM(MNUMYR)                Carbon Tax portion of Residual Fuel price in 87$ per mmbtu
     EMABLK_JRSTR(MNUMYR)                Carbon Tax portion of Residual Fuel price in 87$ per mmbtu
     EMABLK_JRSIN(MNUMYR)                Carbon Tax portion of Residual Fuel price in 87$ per mmbtu
     EMABLK_JRSEL(MNUMYR)                Carbon Tax portion of Residual Fuel price in 87$ per mmbtu
     EMABLK_JPFIN(MNUMYR)                Carbon Tax portion of Petrochemical Feedstock price in 87$ per mmbtu
     EMABLK_JPCIN(MNUMYR)                Carbon Tax portion of Petroleum Coke price in 87$ per mmbtu
     EMABLK_JPPIN(MNUMYR)                Carbon Tax portion of Pentanes Plus price in 87$ per mmbtu
     EMABLK_JPPINPF(MNUMYR)              Carbon Tax portion of Pentanes Plus price in 87$ per mmbtu
     EMABLK_JLUIN(MNUMYR)                Carbon Tax portion of Lubricant price in 87$ per mmbtu
     EMABLK_JOTIN(MNUMYR)                Carbon Tax portion of Other Petroleum price in 87$ per mmbtu
     EMABLK_JOTTR(MNUMYR)                Carbon Tax portion of Other Petroleum price in 87$ per mmbtu
     EMABLK_JMETR(MNUMYR)                Carbon Tax portion of Methanol price in 87$ per mmbtu
     EMABLK_JETTR(MNUMYR)                Carbon Tax portion of E85 price in 87$ per mmbtu
     EMABLK_JCLCLNR(MNUMYR,NUTSEC)       Carbon Content of Coal to an IGCC Plant

** NEMS Corn and Soy/seed oil supply curves price and production from polysys
     WRENEW_CRNSUP_TOT_Q(M10,NDRGN1,MNMYRF)      Polysys Total Corn Production        MMBushels
     WRENEW_CRNSUP_ETH_Q(M10,NDRGN1,MNMYRF)      Polysys Corn Supply Used for Ethanol MMBushels
     WRENEW_CRNSUP_P(M10,NDRGN1,MNMYRF)          Polysys Corn Price                   1987$ per Bushel
     WRENEW_SOYOILSUP_TOT_Q(M10,NDRGN1,MNMYRF)   Polysys Total Soybean Oil Production MMLBS
     WRENEW_SOYOILSUP_BIOD_Q(M10,NDRGN1,MNMYRF)  Polysys Soybean Oil Supply used for Biodiesel MMLBS
     WRENEW_SOYOILSUP_P(M10,NDRGN1,MNMYRF)       Polysys Soybean Oil Price            1987$ per LB
     WRENEW_SOYSUP_TOT_Q(M10,NDRGN1,MNMYRF)        Polysys Total Soybean Production     MMBushels
     WRENEW_SOYSUP_P(M10,NDRGN1,MNMYRF)          Polysys Soybean Price                1987$ per Bushel

** NEMS BioMass supply curves and demand from other modules
     WRENEW_WDSUP_Q_AG(WDCRVS,NDREGN,MNMYRF)     Supply at each step of biomass curve  - AG waste         Tril Btu per Yr
     WRENEW_WDSUP_Q_EC(WDCRVS,NDREGN,MNMYRF)     Supply at each step of biomass curve  - Energy crops     Tril Btu per Yr
     WRENEW_WDSUP_Q_FR(WDCRVS,NDREGN,MNMYRF)     Supply at each step of biomass curve  - Forestry waste   Tril Btu per Yr
     WRENEW_WDSUP_Q_UM(WDCRVS,NDREGN,MNMYRF)     Supply at each step of biomass curve  - Urban wood waste Tril Btu per Yr

     WRENEW_WDSUP_P_AG(WDCRVS,NDREGN,MNMYRF)     Price at each step of biomass curve  - AG waste          87$ per MMbtu
     WRENEW_WDSUP_P_EC(WDCRVS,NDREGN,MNMYRF)     Price at each step of biomass curve  - Energy crops      87$ per MMbtu
     WRENEW_WDSUP_P_FR(WDCRVS,NDREGN,MNMYRF)     Price at each step of biomass curve  - Forestry waste    87$ per MMbtu
     WRENEW_WDSUP_P_UM(WDCRVS,NDREGN,MNMYRF)     Price at each step of biomass curve  - Urban wood waste  87$ per MMbtu

     WRENEW_MP_BM_BT(MNUMFS)                     Identifies which supply types available to BTL production
     WRENEW_MP_BM_CM(MNUMFS)                     Identifies which supply types available to the Commercial Sector
     WRENEW_MP_BM_ET(MNUMFS)                     Identifies which supply types available to Ethanol production
     WRENEW_MP_BM_H2(MNUMFS)                     Identifies which supply types available to Hydrogen production
     WRENEW_MP_BM_IN(MNUMFS)                     Identifies which supply types available to the Industrial Sector
     WRENEW_MP_BM_PW(MNUMFS)                     Identifies which supply types available to the Power Sector
     WRENEW_MP_BM_RS(MNUMFS)                     Identifies which supply types available to Residential Sector

     WRENEW_QBMPWCL(MNMFS1,NDRGN1,MNMYRF)        Biomass demand at each step - Power  - Tril Btu per Yr
     WRENEW_QBMRSCL(MNMFS1,NDRGN1,MNMYRF)        Biomass demand at each step - Residential  - Tril Btu per Yr
     WRENEW_QBMINCL(MNMFS1,NDRGN1,MNMYRF)        Biomass demand at each step - Industrial  - Tril Btu per Yr
     WRENEW_QBMH2CL(MNMFS1,NDRGN1,MNMYRF)        Biomass demand at each step - Hydrogen  - Tril Btu per Yr

     WRENEW_QBMETCL(MNMFS1,NDRGN1,MNMYRF)        Ethanol Production Demand for Biomass used from the supply curves    Tril Btu per Yr
     WRENEW_QBMBTCL(MNMFS1,NDRGN1,MNMYRF)        BTL Production Demand for Biomass used from the supply curves    Tril Btu per Yr
     WRENEW_QBMET(MNUMCR,MNUMYR,M6)              Biomass Ethanol quantity   Mbbl per cd
     WRENEW_QCLETH(MNUMYR,MNUMCR)                Purchased Coal - Ethanol plants Tril Btu per Yr
     WRENEW_QELETH(MNUMYR,MNUMCR)                Purchased Electricity - Ethanol plants Tril Btu per Yr
     WRENEW_QNGETH(MNUMYR,MNUMCR)                Purchased Natural Gas - Ethanol plants Tril Btu per Yr

** NEMS Coal supply curves
     USO2GRP_XCL_QECP(MX_NCL,ECPFPH,MNUMYR)

     USO2GRP_XCL_PECP(MX_NCL,M11,ECPFPH,MNUMYR)

     USO2GRP_CTL_CDSL1(NDREGN,REFREG)
     USO2GRP_CTL_CLDR(NDREGN)
     USO2GRP_CTL_OTHER(MX_NCL,MNUMYR)
     USO2GRP_CTL_TRATE(MX_NCL,NDREGN)
     USO2GRP_CTL_TYPE(MX_NCL)

     USO2GRP_EFD_RANK(MX_NCI)

     USO2GRP_XCL_BTU(MX_NCI)
     USO2GRP_XCL_CAR(MX_NCI)
     USO2GRP_XCL_HG(MX_NCI)
     USO2GRP_XCL_MX_PCAP(MX_NCL)
     USO2GRP_XCL_PCAP(MX_NCL,MNUMYR)
     USO2GRP_XCL_SO2(MX_NCI)
     USO2GRP_XCL_STEPS(M11)

     USO2GRP_XCL_1TESC(MX_NCL,ECPFPH,MNUMYR,NDREGN)
     USO2GRP_XCL_2TESC(MX_NCL,ECPFPH,MNUMYR,NDREGN)

     EMISSION_NUM_SO2_GRP
     EMISSION_SO2_SHR_BY_CLRG(NDREGN,MX_SO2)
     EMISSION_EMELPSO2(MNUMYR,MX_SO2)
     EMISSION_EMEL_PHG(NDREGN,MNUMYR)
     EMISSION_CCS_PMM(M5,MNUMPR,MNUMYR)             Carbon capture and storage from LFMM
     EMISSION_EXTRARISK(MNUMYR)                     Extra risk premium possibly due to carbon

     COALEMM_RCLIGNR(NDREGN,MNUMYR)
     COALEMM_RCLCLNR(NDREGN,MNUMYR,NUTSEC)
     COALEMM_PLNT_EMF(ECPCAP,MX_RNK)

     COALEMM_N_RG(M1000)                    Region (NEMS or Fuel region?) of new build in ECP
     COALEMM_N_RY(M1000)                    Year plant built in ECP
     COALEMM_N_IGRP(M1000)                  W_IGRP index for plant bult in ECP
     COALEMM_N_PLTS                         Number of new builds from ECP
     COALEMM_N_CFR(M1000)                   Capacity Factor of new CCS build
     COALEMM_N_HRAT(M1000)                  Heatrate of new CCS build
     COALEMM_N_CPTY(M1000)                  Capacity  of new CCS build
     COALEMM_N_PTP(M1000)                   Plant type of new CCS build

     EMABLK_JCLIGNR(MNUMYR)

* NEMS conversion factors
     CONVFACT_CFAR3(MNUMYR)       BTUBBL                      Atmospheric residuum (unfinished oil stream)
     CONVFACT_CFBIOD(MNUMYR)      BTUBBL                      Biodiesel
     CONVFACT_CFBMQ(MNUMYR)       BTUTON                      Biomass (cellulose) energy content
     CONVFACT_CFBTLLIQ(MNUMYR)    BTUBBL                      Liquids from biomass
     CONVFACT_CFCBOB(MNUMYR)      BTUBBL                      Conventional gasoline BOB
     CONVFACT_CFCBQ(MNUMYR)       BTUBBL                      Ca Air Resource Board BOB
     CONVFACT_CFCBTLLIQ(M3,MNUMYR) BTUBBL                     Liquids from coal-biomass combo (1=coal 2=biomass 3=total)
     CONVFACT_CFCLSN(MNUMYR)      BTUTON                      Coal to liquids
     CONVFACT_CFCNGQ(MNUMYR)      BTUCF                       Compressed natural gas
     CONVFACT_CFCRDDOM(MNUMYR)    BTUBBL                      Domestic crude production
     CONVFACT_CFCRDIMP(MNUMYR)    BTUBBL                      Crude oil imports
     CONVFACT_CFCRDEXP(MNUMYR)    BTUBBL                      Crude oil exports
     CONVFACT_CFCRDLTSWT(MNUMYR)  BTUBBL                      Light sweet crude oil
     CONVFACT_CFCRDLTSOUR(MNUMYR) BTUBBL                      Light sour crude oil
     CONVFACT_CFCRDMD2SOUR(MNUMYR) BTUBBL                     Medium medium sour crude oil
     CONVFACT_CFCRDMDSOUR(MNUMYR) BTUBBL                      Medium sour crude oil
     CONVFACT_CFCRDHVSWT(MNUMYR)  BTUBBL                      Heavy sweet crude oil
     CONVFACT_CFCRDHVSOUR(MNUMYR) BTUBBL                      Heavy sour crude oil
     CONVFACT_CFCRDCA(MNUMYR)     BTUBBL                      California crude oil
     CONVFACT_CFCRDSYN(MNUMYR)    BTUBBL                      Syncrude
     CONVFACT_CFCRDDILBIT(MNUMYR) BTUBBL                      Dilbit
     CONVFACT_CFCRDLT2SWT(MNUMYR) BTUBBL                      Light light sweet crude oil
     CONVFACT_CFCRDLSCOND(MNUMYR) BTUBBL                      Lease condensate
     CONVFACT_CFCTLLIQ(MNUMYR)    BTUBBL                      Liquids from coal
     CONVFACT_CFDSCM(MNUMYR)      BTUBBL                      Commercial distillate
     CONVFACT_CFDSEL(MNUMYR)      BTUBBL                      Electric power distillate
     CONVFACT_CFDSIN(MNUMYR)      BTUBBL                      Industrial distillate (diesel) includes ulsd
     CONVFACT_CFDSLQ(MNUMYR)      BTUBBL                      Low sulfur diesel
     CONVFACT_CFDSQT(MNUMYR)      BTUBBL                      Total distillate
     CONVFACT_CFDSRS(MNUMYR)      BTUBBL                      Residential distillate
     CONVFACT_CFDSTR(MNUMYR)      BTUBBL                      Transportation distillate (diesel) includes ulsd
     CONVFACT_CFDSUQ(MNUMYR)      BTUBBL                      Ultra low sulfur diesel
     CONVFACT_CFDSCQ(MNUMYR)      BTUBBL                      Ca Air Resource Board diesel
     CONVFACT_CFE85Q(MNUMYR)      BTUBBL                      E85
     CONVFACT_CFETQ(MNUMYR)       BTUBBL                      Ethanol
     CONVFACT_CFEXPRD(MNUMYR)     BTUBBL                      Product Exports
     CONVFACT_CFFTLIQ(M4,MNUMYR)  BTUBBL                      Fischer-Tropsch output liquid streams
     CONVFACT_CFGO3(MNUMYR)       BTUBBL                      Gas oil (unfinished oil stream)
     CONVFACT_CFGOP(MNUMYR)       BTUBBL                      Gas oil exports (average of GO1 and GO4)
     CONVFACT_CFGRAIN(MNUMYR)     BTUBUS                      Grain Btu per bushel
     CONVFACT_CFGTLLIQ(MNUMYR)    BTUBBL                      Liquids from natural gas
     CONVFACT_CFIMPRD(MNUMYR)     BTUBBL                      Product Imports
     CONVFACT_CFIMUO(MNUMYR)      BTUBBL                      Unfinished oil imports - weighted average
     CONVFACT_CFJFQ(MNUMYR)       BTUBBL                      Jet Fuel
     CONVFACT_CFLGQ(MNUMYR)       BTUBBL                      Liquefied Petroleum Gas
     CONVFACT_CFM85Q(MNUMYR)      BTUBBL                      M85
     CONVFACT_CFMGQ(MNUMYR)       BTUBBL                      Motor Gasoline
     CONVFACT_CFMN3(MNUMYR)       BTUBBL                      Medium naphtha (unfinished oil stream)
     CONVFACT_CFNGC(MNUMYR)       BTUCF                       Natural Gas - Consumption and Production
     CONVFACT_CFNGCL(MNUMYR)      BTUCF                       Natural gas (synthetic) from coal
     CONVFACT_CFNGE(MNUMYR)       BTUCF                       Natural Gas - Exports
     CONVFACT_CFNGI(MNUMYR)       BTUCF                       Natural Gas - Imports
     CONVFACT_CFNGL(MNUMYR)       BTUBBL                      Natural Gas Liquids
     CONVFACT_CFNGN(MNUMYR)       BTUCF                       Natural Gas - Nonutility consumption
     CONVFACT_CFNGU(MNUMYR)       BTUCF                       Natural Gas - Utility consumption
     CONVFACT_CFOTQ(MNUMYR)       BTUBBL                      Other Petroleum
     CONVFACT_CFPFQ(MNUMYR)       BTUBBL                      Petrochemical Feedstocks
     CONVFACT_CFCCQ(MNUMYR)       BTUBBL                      Catalytic petroleum Coke
     CONVFACT_CFRBOB(MNUMYR)      BTUBBL                      Reformulated gasoline BOB
     CONVFACT_CFRGQ(MNUMYR)       BTUBBL                      Motor Gasoline (Reformulated)
     CONVFACT_CFTGQ(MNUMYR)       BTUBBL                      Motor Gasoline (no Oxygenates)
     CONVFACT_CFTPQ(MNUMYR)       BTUBBL                      Total Petroleum Consumption
     CONVFACT_CFUBAQ(MNUMYR)      BTUBBL                      Pyrolysis oils
     CONVFACT_CFVEGGIE(MNUMYR)    BTUBBL                      Convert biodiesel to vegetable supply
     CONVFACT_CFBIOBUTE(MNUMYR)   BTUBBL                      Isobutanol
     CONVFACT_CFPET               BTUBBL                      Undenatured ethanol
     CONVFACT_APICAMG(M2,MNUMYR)     California motor gasoline
     CONVFACT_APILTSW(M2,MNUMYR)     Light sweet crude oil
     CONVFACT_APILTSO(M2,MNUMYR)     Light sour crude oil
     CONVFACT_APIMMSO(M2,MNUMYR)     Medium medium sour crude oil
     CONVFACT_APIMDSO(M2,MNUMYR)     Medium sour crude oil
     CONVFACT_APIHVSW(M2,MNUMYR)     Heavy sweet crude oil
     CONVFACT_APIHVSO(M2,MNUMYR)     Heavy sour crude oil
     CONVFACT_APICA(M2,MNUMYR)       California crude oil
     CONVFACT_APISYN(M2,MNUMYR)      Syncrude
     CONVFACT_APIDIL(M2,MNUMYR)      Dilbit
     CONVFACT_APILLSW(M2,MNUMYR)     Light light sweet crude oil
     CONVFACT_API50PL(M2,MNUMYR)     API 50+ crude oil
     CONVFACT_APICRDDOM(M2,MNUMYR)   API domestic crude oil production
     CONVFACT_APICRDIMP(M2,MNUMYR)   API crude oil imports
     CONVFACT_APICRDEXP(M2,MNUMYR)   API crude oil exports

     MACOUT_MC_JPGDP(MNUMY3)
     MCDETAIL_MC_DETAIL(M102,MNUMYR)  holds Polysys ag deflator
     MACOUT_MC_NP(MNUMCR,MNUMYR)
     MACOUT_MC_RMCORPBAA(MNUMYR)
     MACOUT_MC_RMTCM10Y(MNUMYR)

     MPBLK_PASIN(MNUMCR,MNUMYR)       Asphalt- Road Oil- Industrial  $87 per MMBtu
     MPBLK_PDSAS(MNUMCR,MNUMYR)       Distillate - All Sectors
     MPBLK_PDSCM(MNUMCR,MNUMYR)       Distillate - Commercial
     MPBLK_PDSEL(MNUMCR,MNUMYR)       Distillate - Electricity
     MPBLK_PDSIN(MNUMCR,MNUMYR)       Distillate - Industrial
     MPBLK_PDSRS(MNUMCR,MNUMYR)       Distillate - Residential
     MPBLK_PDSTR(MNUMCR,MNUMYR)       Distillate - Transportation
     MPBLK_PELIN(MNUMCR,MNUMYR)       Electricity - Industrial
     MPBLK_PETTR(MNUMCR,MNUMYR)       Ethanol - Transportation (sold as E85)
     MPBLK_PJFTR(MNUMCR,MNUMYR)       Jet Fuel - Transportation  $87 per MMBtu
     MPBLK_PKSAS(MNUMCR,MNUMYR)       Kerosene - All Sectors
     MPBLK_PKSCM(MNUMCR,MNUMYR)       Kerosene - Commercial
     MPBLK_PKSIN(MNUMCR,MNUMYR)       Kerosene - Industrial
     MPBLK_PKSRS(MNUMCR,MNUMYR)       Kerosene - Residential
     MPBLK_PLGAS(MNUMCR,MNUMYR)       Liquid Petroleum Gases - All Sectors
     MPBLK_PLGCM(MNUMCR,MNUMYR)       Liquid Petroleum Gases - Commercial
     MPBLK_PLGIN(MNUMCR,MNUMYR)       Liquid Petroleum Gases - Industrial
     MPBLK_PLGRS(MNUMCR,MNUMYR)       Liquid Petroleum Gases - Residential
     MPBLK_PLGTR(MNUMCR,MNUMYR)       Liquid Petroleum Gases - Transportation
     MPBLK_PMETR(MNUMCR,MNUMYR)       Methanol Transportation
     MPBLK_PMGAS(MNUMCR,MNUMYR)       Motor Gasoline - All Sectors
     MPBLK_PMGCM(MNUMCR,MNUMYR)       Motor Gasoline - Commercial
     MPBLK_PMGIN(MNUMCR,MNUMYR)       Motor Gasoline - Industrial
     MPBLK_PMGTR(MNUMCR,MNUMYR)       Motor Gasoline - Transportation
     MPBLK_PNGIN(MNUMCR,MNUMYR)       Natural Gas - Industrial
     MPBLK_POTAS(MNUMCR,MNUMYR)       Other Petroleum - All Sectors
     MPBLK_POTIN(MNUMCR,MNUMYR)       Other Petroleum - Industrial
     MPBLK_POTTR(MNUMCR,MNUMYR)       Other Petroleum - Transportation
     MPBLK_PPFIN(MNUMCR,MNUMYR)       Petrochemical - Feedstock Industrial
     MPBLK_PRHAS(MNUMCR,MNUMYR)       Residential Fuel High Sulfur - All Sectors
     MPBLK_PRHEL(MNUMCR,MNUMYR)       Residential Fuel High Sulfur - Electricity
     MPBLK_PRHTR(MNUMCR,MNUMYR)       Residential Fuel High Sulfur - Transportation
     MPBLK_PRLAS(MNUMCR,MNUMYR)       Residential Fuel Low Sulfur - All Sectors
     MPBLK_PRLCM(MNUMCR,MNUMYR)       Residential Fuel Low Sulfur - Commercial
     MPBLK_PRLEL(MNUMCR,MNUMYR)       Residential Fuel Low Sulfur - Electricity
     MPBLK_PRLIN(MNUMCR,MNUMYR)       Residential Fuel Low Sulfur - Industrial
     MPBLK_PRLTR(MNUMCR,MNUMYR)       Residential Fuel Low Sulfur - Transportation

     PMORE_PBUIN(MNUMCR,MNUMYR)       Butane - Industrial
     PMORE_PBUINPF(MNUMCR,MNUMYR)     Butane feedstock - Industrial
     PMORE_PETIN(MNUMCR,MNUMYR)       Ethane - Industrial
     PMORE_PETINPF(MNUMCR,MNUMYR)     Ethane feedstock - Industrial
     PMORE_PISIN(MNUMCR,MNUMYR)       Isobutane - Industrial
     PMORE_PISINPF(MNUMCR,MNUMYR)     Isobutane - Feedstock Industrial
     PMORE_PLGINPF(MNUMCR,MNUMYR)     Liquid Petroleum Gases - Feedstock Industrial
     PMORE_PLUIN(MNUMCR,MNUMYR)       Lubricants - Industrial
     PMORE_PPPIN(MNUMCR,MNUMYR)       Pentanes plus - Industrial
     PMORE_PPPINPF(MNUMCR,MNUMYR)     Pentanes plus - Feedstock Industrial
     PMORE_PPRCM(MNUMCR,MNUMYR)       Propane - Commercial
     PMORE_PPRIN(MNUMCR,MNUMYR)       Propane - Industrial
     PMORE_PPRINPF(MNUMCR,MNUMYR)     Propane feedstock - Industrial
     PMORE_PPROLENERF(MNUMCR,MNUMYR)  Propylene - Refinery production
     PMORE_PPRRS(MNUMCR,MNUMYR)       Propane - Residential
     PMORE_PPRTR(MNUMCR,MNUMYR)       Propane - Transportation
     PMORE_PSULFURIN(MNUMCR,MNUMYR)   Sulfur - Industrial
     PMORE_PPCIN(MNUMCR,MNUMYR)       Petroleum coke - Industrial

     PONROAD_PDSTRHWY(MNUMCR,MNUMYR)       On-road distillate price transportation sector  $87 per MMBtu
     QONROAD_QDSTRHWY(MNUMCR,MNUMYR)       On-road distillate quantity transportation sector Tril Btu per Yr
     QONROAD_CFDSTRHWY(MNUMYR)             On-road distillate conversion factor trans. sector  MMBtu per bbl

     UEFPOUT_PELBS(MNUMCR,MNUMYR)          Wholesale electricity price to grid in 87$ per MMBTU

     LFMMOUT_Q_CRUDE_IMPORTS(M10,MNCRUD,MNXYRS)        Target Crude Imports to World Model
     LFMMOUT_Q_CRUDE_IMPORTA(M10,MNCRUD,MNXYRS)        Actual Crude Imports from LFMM

     LFMMOUT_Q_CRUDE_EXPORTS(M10,MNCRUD,MNXYRS)        Crude Export Quantities to World Model
     LFMMOUT_P_CRUDE_EXPORTS(M10,MNCRUD,MNXYRS)        Crude Export Prices to World Model
     LFMMOUT_P_CRUDE_TO_CAN(MNUMPR,MNCRUD,MNXYRS)      Price of crude oil exported to Canada in 87$ per bbl
     LFMMOUT_Q_CRUDE_TO_CAN(MNUMPR,MNCRUD,MNXYRS)      Amount of crude oil exported to Canada in M bbl per day

* Parameters for crude import level smoothing
     QIN(M10,MNCRUD)
     QIN_TOT(M10)
     SIN(M10,MNCRUD)
     QOUT(M10,MNCRUD)
     QOUT_TOT(M10)
     SOUT(M10,MNCRUD)
     SAVG(M10,MNCRUD)
     STMP(M10,MNCRUD)
     STMP_TOT(M10)
     SMOOTH_FACTOR / 0.3 /

     LFMMOUT_P_CRUDE_IMPORTS(M10,MNCRUD,MNXYRS)
     LFMMOUT_GRD2DSQTY(MNUMPR,MNUMYR)       Renewable diesel to distillate Mbbl per day by PADD level
     LFMMOUT_GRN2MGQTY(MNUMPR,MNUMYR)       Renewable diesel to gasoline Mbbl per day by PADD level
     LFMMOUT_SAF2JTQTY(MNUMPR,MNUMYR)       HEFA-SPK renewable jet to jet Mbbl per day by PADD level
     LFMMOUT_ETHTOT(MNUMPR,MNUMYR)          Total ethanol production by PADD Mbbl per day
     LFMMOUT_BIMQTY(M4,MNUMPR,MNUMYR)       Biodiesel production by type and PADD Mbbl per day
     LFMMOUT_RENEWDIMPPD(MNUMPR,MNUMYR)     Renewable diesel imports by PADD in MBBL per day
     LFMMOUT_BIODIMPPD(MNUMPR,MNUMYR)       Biodiesel imports by PADD in MBBL per day
     LFMMOUT_BIODEXPPD(MNUMPR,MNUMYR)       Biodiesel exports by PADD
     LFMMOUT_RFSCREDPRC(M4,MNUMYR)          RFS credit price by constraint category in 87$ per BBL
     LFMMOUT_RFSCREDITS(M4,M13,MNUMYR)      RFS credits by category and biofuel in billion gallons per year
     LFMMOUT_RFSSAFETY(M4,MNUMYR)           RFS safety valve usage in in billion gallons per year
     LFMMOUT_RFSACTUAL(M4,MNUMYR)           Actual RFS production levels from prior year Period 2 in M BBL per day
     LFMMOUT_RFSMANDATES(M4,MNUMYR)         RFS mandates by category and biofuel in billion gallons per year

     LFMMOUT_LCFS_Baseline(LCFS_C,MNXYRS)       LCFS Baseline Values Mmton_CO2 per Trill
     LFMMOUT_LCFS_Actual(LCFS_C,MNXYRS)         LCFS Actual - Achieved Values Mmton_CO2 per Trill
     LFMMOUT_LCFS_Waiver(LCFS_C,MNXYRS)         LCFS Waivers Purchased Mmton CO2
     LFMMOUT_LCFS_Offset_Prc(LCFS_C,MNXYRS)     LCFS Offset Price 87$ per mton CO2
     LFMMOUT_LCFS_Carb_Offset(LCFS_C,MNXYRS)    LCFS Carbon Offset Mmton CO2

     LFMMOUT_LCFS_PeToTrills(LCFS_C,MNXYRS)     LCFS Temp Variables Total Btus of Petroleum Fuels
     LFMMOUT_LCFS_PetTotalVolume(LCFS_C,MNXYRS) LCFS Temp Variables Total Volume of Petroleum Fuels

     LFMMOUT_CFP_Baseline(LCFS_C,MNXYRS)       CFP Baseline Values Mmton_CO2 per Trill
     LFMMOUT_CFP_Actual(LCFS_C,MNXYRS)         CFP Actual - Achieved Values Mmton_CO2 per Trill
     LFMMOUT_CFP_Waiver(LCFS_C,MNXYRS)         CFP Waivers Purchased Mmton CO2
     LFMMOUT_CFP_Offset_Prc(LCFS_C,MNXYRS)     CFP Offset Price 87$ per mton CO2
     LFMMOUT_CFP_Carb_Offset(LCFS_C,MNXYRS)    CFP Carbon Offset Mmton CO2

     LFMMOUT_CFP_PeToTrills(LCFS_C,MNXYRS)     CFP Temp Variables Total Btus of Petroleum Fuels
     LFMMOUT_CFP_PetTotalVolume(LCFS_C,MNXYRS) CFP Temp Variables Total Volume of Petroleum Fuels

     LFMMOUT_WACFS_Baseline(LCFS_C,MNXYRS)       WACFS Baseline Values Mmton_CO2 per Trill
     LFMMOUT_WACFS_Actual(LCFS_C,MNXYRS)         WACFS Actual - Achieved Values Mmton_CO2 per Trill
     LFMMOUT_WACFS_Waiver(LCFS_C,MNXYRS)         WACFS Waivers Purchased Mmton CO2
     LFMMOUT_WACFS_Offset_Prc(LCFS_C,MNXYRS)     WACFS Offset Price 87$ per mton CO2
     LFMMOUT_WCFS_Carb_Offset(LCFS_C,MNXYRS)    WACFS Carbon Offset Mmton CO2

     LFMMOUT_WACFS_PeToTrills(LCFS_C,MNXYRS)     WACFS Temp Variables Total Btus of Petroleum Fuels
     LFMMOUT_WACFS_PetTotalVolume(LCFS_C,MNXYRS) WACFS Temp Variables Total Volume of Petroleum Fuels

LFMMOUT_REFGAIN(MNUMPR,M2,MNUMYR)          Refinery Gain by Refinery Region and Refinery Type

     LFMMOUT_MOTOR_FUEL(M4,M3,MNUMYR)           Motor gasoline breakout
     LFMMOUT_DIST_FUEL(M3,M4,MNUMYR)            Distillate breakout
     LFMMOUT_QNGRFPD(MNUMPR,MNUMYR)             Natural gas for GTL  Tril Btu per Yr
* for ftab table 21
     LFMMOUT_FEEDSTOCKS(M4,M10,MNUMYR)          Feedstock section
     LFMMOUT_INTERMEDIATE(M4,M13,MNUMYR)        Intermediate stream section
     LFMMOUT_REFINE_PROD(M4,M11,MNUMYR)         Refined products section
     LFMMOUT_GROSS_IMPORT(M4,M11,MNUMYR)        Gross import section
     LFMMOUT_GROSS_EXPORT(M4,M12,MNUMYR)        Gross export section
     LFMMOUT_DOM_CONSUME(M4,M12,MNUMYR)         Domestic consumption section

     LFMMOUT_AB32JETCOVER(MNUMYR)               Portion of CA jet fuel covered under AB32

     LFMMOUT_AB32_DS(MNUMYR)                    Distillate AB32 tax adder from LP
     LFMMOUT_AB32_KS(MNUMYR)                    Kerosene AB32 tax adder from LP
     LFMMOUT_AB32_PR(MNUMYR)                    Propane AB32 tax adder from LP
     LFMMOUT_AB32_MG(MNUMYR)                    Motor gas AB32 tax adder from LP
     LFMMOUT_AB32_ET(MNUMYR)                    E85 AB32 tax adder from LP
     LFMMOUT_AB32_JF(MNUMYR)                    Jet fuel AB32 tax adder from LP

     LFMMOUT_LFMMCODE
     LFMMOUT_LFEXPLEASE                        Switch allowing exports of lease condensate
     LFMMOUT_LFREFRENT                           Switch to activate crude oil export logic to simulate rent for unused refinery capacity

     LFMMOUT_RFS_WAIVER(M4,MNUMYR)             RFS waiver price by constraint category in 87$ per BBL
     LFMMOUT_RFSDSTR(MNUMCR,MNUMYR)            RFS contribution to diesel price in 87$ per BBL
     LFMMOUT_RFSMGTR(MNUMCR,MNUMYR)            RFS contribution to motor gasoline price in 87$ per BBL
     LFMMOUT_RFSRBOB(MNUMCR,MNUMYR)            RFS contribution to RBOB price in 87$ per BBL
     LFMMOUT_RFSJFTR(MNUMCR,MNUMYR)            RFS contribution to jet fuel price in 87$ per BBL
     LFMMOUT_RFSDSRS(MNUMCR,MNUMYR)            RFS contribution to heating oil price in 87$ per BBL
     LFMMOUT_REF_CAP(M57,MNUMPR,MNUMYR)        Refinery Capacity in thousand barrels per day
     LFMMOUT_REF_UTL(M57,MNUMPR,MNUMYR)        Refinery Utilization

     LFMMOUT_PROFIT_BBL(MNUMPR,MNUMYR)         Refinery Marginal Profit per BBL of Crude Run in 87$ per BBL

     LFMMOUT_RFCRUDEWHP(MNUMPR,MNCRUD,MNUMYR)  Crude oil wellhead price by refinery region and type in 87$ per BBL

     LFMMOUT_RFCRUDEINP(MNUMPR,MNCRUD,MNUMYR)     Crude oil refinery inputs by refinery region and type in M bbl per day
     LFMMOUT_P_RFCRUDEINP(MNUMPR,MNCRUD,MNUMYR)   Crude oil refinery price by refinery region and type in 87$ per BBL
     LFMMOUT_RFOTHERINP(MNUMPR,MNUMYR)            Other refinery inputs by refinery region and type in M bbl per day
     LFMMOUT_BIOBUTESTK(MNUMYR)                   Biobutanol stock withdrawal (+) or addition (-) in M bbl per day
     LFMMOUT_RFBIOBUTECD(MNUMCR,MNUMYR)           Production of biobutanol by Census division in M bbl per day
     LFMMOUT_RFBIOBUTERR(MNUMPR,MNUMYR)           Production of biobutanol by refinery region in M bbl per day
     LFMMOUT_QBIOBUTE(MNUMCR,MNUMYR)              Consumption of biobutanol by Census division in M bbl per day
     LFMMOUT_BIOBUTEIMP(MNUMYR)                   Imports of biobutanol in M bbl per day
     LFMMOUT_BIOBUTEEXP(MNUMYR)                   Exports of biobutanol in M bbl per day
     LFMMOUT_BIOBUTEPRICE(MNUMYR)                 Biobutanol price in 87$ per bbl
     LFMMOUT_CORNCD(CORNTO,MNUMCR,MNUMYR)         Corn Consumed in Production of Ethanol in MMBTU

     LFMMOUT_REFPRODET(MNUMPR,MNUMYR)             Refinery production of ethane
     LFMMOUT_REFPRODPR(MNUMPR,MNUMYR)             Refinery production of propane
     LFMMOUT_REFPRODBU(MNUMPR,MNUMYR)             Refinery production of butane
     LFMMOUT_REFPRODIS(MNUMPR,MNUMYR)             Refinery production of isobutane
     LFMMOUT_REFPRODPP(MNUMPR,MNUMYR)             Refinery production of natural gasoline
     LFMMOUT_REFPRODPY(MNUMPR,MNUMYR)             Refinery production of propylene
     LFMMOUT_REFPRODOO(MNUMPR,MNUMYR)             Refinery production of other olefins
     LFMMOUT_REFINPET(MNUMPR,MNUMYR)              Refinery inputs of ethane
     LFMMOUT_REFINPPR(MNUMPR,MNUMYR)              Refinery inputs of propane
     LFMMOUT_REFINPBU(MNUMPR,MNUMYR)              Refinery inputs of butane
     LFMMOUT_REFINPIS(MNUMPR,MNUMYR)              Refinery inputs of isobutane
     LFMMOUT_REFINPPP(MNUMPR,MNUMYR)              Refinery inputs of natural gasoline
     LFMMOUT_REFINPPY(MNUMPR,MNUMYR)              Refinery inputs of propylene
     LFMMOUT_REFINPOO(MNUMPR,MNUMYR)              Refinery inputs of other olefins

* INTOUT variables
     INTOUT_BRENT_PRICE(MNUMYR)                             Brent spot price
     INTOUT_START_PRICE(MNUMYR)                             World oil price as specified in the memo
     INTOUT_P_Total_Crude(CRSTEP,MNXYRS)                    World crude-like liquids supply curve prices
     INTOUT_Q_Total_Crude(CRSTEP,MNXYRS)                    World crude-like liquids supply curve quantities
     INTOUT_P_Foreign_Crude(MNCRUD,M1,CISTEP,MNXYRS)        Incremental crude supply curve prices
     INTOUT_Q_Foreign_Crude(MNCRUD,M1,CISTEP,MNXYRS)        Incremental crude supply curve quantities
     INTOUT_P_Non_US_Demand(MNCRUD,M1,S1,MNXYRS)            NonUS demand curve prices
     INTOUT_Q_Non_US_Demand(MNCRUD,M1,S1,MNXYRS)            NonUS demand curve quantities
     INTOUT_P_C_MC_Demand(MCSTEP,MNXYRS,MNPROD)             Caribbean and Maritime Canada demand curve prices
     INTOUT_Q_C_MC_Demand(MCSTEP,MNXYRS,MNPROD)             Caribbean and Maritime Canada demand curve quantities
     INTOUT_Product_Import_P(MNPROD,INTREG,INSTEP,MNXYRS)   Prices for product import supply steps
     INTOUT_Product_Import_Q(MNPROD,INTREG,INSTEP,MNXYRS)   Quantites for product import supply steps
     INTOUT_Product_Export_P(MNPROD,INTREG,INSTEP,MNXYRS)   Prices for product export supply steps
     INTOUT_Product_Export_Q(MNPROD,INTREG,INSTEP,MNXYRS)   Quantities for product export supply steps

* World oil prices
     INTOUT_IT_WOP(MNUMYR,M2)                               World oil prices in 87$ per bbl
     MXPBLK_XIT_WOP(MNXYRS,M2)                              World oil prices in 87$ per bbl and 87$ per MMBTU

*OGSM variables
     OGSMOUT_OGCRDHEAT(MNCRUD,MNUMYR)                       Heat rates by crude oil type
     OGSMOUT_OGNGPLPRD(OGDIST,MNUMYR)                       Natural Gas Plant Liquids
     OGSMOUT_OGNGPLET(OGDIST,MNUMYR)                        NGPL Ethane production
     OGSMOUT_OGNGPLPR(OGDIST,MNUMYR)                        NGPL Propane production
     OGSMOUT_OGNGPLBU(OGDIST,MNUMYR)                        NGPL Butane production
     OGSMOUT_OGNGPLIS(OGDIST,MNUMYR)                        NGPL Isobutane production
     OGSMOUT_OGNGPLPP(OGDIST,MNUMYR)                        NGPL Pentanes plus production
     OGSMOUT_OGCRUDEREF(MNUMPR,MNCRUD,MNUMYR)               Crude oil production by refinery region and crude type
     OGSMOUT_OGCO2TAR(M8,M8)                                CO2 transport price from OGSM ($ per mmcf)
     OGSMOUT_OGCO2AVL(M8,M13,MNUMYR)                        CO2 available (mmcf)
     OGSMOUT_OGCO2PRC(M8,M13,MNUMYR)                        CO2 price ($ per mmcf)
     OGSMOUT_OGCO2PUR2(M8,M13,MNUMYR)                       CO2 purchased at the EOR sites (mmcf)
     OGSMOUT_OGCO2QLF(M8,MNUMYR)                            CO2 quantity from LFMM (mmcf)
     OGSMOUT_OGCO2PLF(M8,MNUMYR)                            CO2 price from LFMM ($ per mmcf)

* UECPOUT variables
     UECPOUT_TnS_Costs(MAXNF2,MNUMYR)                       Unit Co2 trsnport and storage costs in each fuel region for non-EOR CO2
     UECPOUT_FR_OR_TRANCOST(MAXNFR,M8,MNUMYR)               Unit Co2 trsnport costs from each fuel region to each OGSM region
     UECPOUT_MUST_STORE(MAXNFR,MNMYRF)                      Flag indicating CO2 regulations 0=> No Regulation 1=> Some kind of CO2 regulation

* TRANREP variables - NEMS Flex-Fuel vehicle demands and expectation
     TRANREP_E10SHARE(MNUMCR,MNUMYR)                        E10-only motor gasoline demand as percent of total motor gasoline demand
     TRANREP_E85AVAIL(MNUMCR,MNUMYR)                        E85 Availability - Percent Stations with E85 Capability
     TRANREP_QAGTR(MNUMCR,MNUMYR)                           Transportation demand (aviation gasoline)
     TRANREP_QFFV(MNUMCR,MNUMYR)                            Total flex-fuel vehicle demand in trills
     TRANREP_QLUTR(MNUMCR,MNUMYR)                           Transportation demand (lubricants)
     TRANREP_XQAGTR(MNUMCR,MNXYRS)                          Expected transportation demand (aviation gasoline)
     TRANREP_XQFFV(MNUMCR,MNXYRS)                           EXPECTED total flex-fuel vehicle demand in trills
     TRANREP_XQLUTR(MNUMCR,MNXYRS)                          Expected transportation demand (lubricants)
     TRANREP_TRQLDV(M8,MNUMCR,MNUMYR)                       Electric Vehicle Demand
     TRANREP_XTRQLDV(M8,MNUMCR,MNXYRS)                      Expected Electric Vehicle Demand
     TRANREP_FCLOGIT0(MNUMC2)
     TRANREP_FCLOGIT1
     TRANREP_FCLOGIT2
     TRANREP_FCLOGIT3
     TRANREP_FCLOGIT4

*** COGEN tables
     COGEN_CGREFCAP(MNUMCR,MNUMYR,NUMCGF)         MWATT     Combined heat and power fuel consumption - refinery
     COGEN_CGREFQ(MNUMCR,MNUMYR,NUMCGF)           tBTU      Combined heat and power fuel capacity - refinery
     COGEN_CGREFGEN(MNUMCR,MNUMYR,NUMCGF,M2)      GWh       Combined heat and power generation - refinery

*    NUMCGF FUELS:
*            1=Coal            2=Petroleum      3=Natural Gas     4=Hydropower
*            5=Geothermal      6=MSW            7=Biomass         8=Solar Photovoltaic
*            9=Other Gaseous  10=Other         11=Wind           12=Solar Thermal


*** PMMFTAB tables
     PMMFTAB_ADVCAPCD(MNUMCR,MNUMYR)          Advanced ethanol plant capacity
     PMMFTAB_BANKCRED(MNUMYR)                 Banked credits
     PMMFTAB_BANKUSED(MNUMYR)                 Banked credits used
     PMMFTAB_CBIODUAL(MNUMYR)                 Dual price from the biofuels offset row (CTRNBIO)
     PMMFTAB_CELLIMPFRAC(MNUMCR,MNUMYR)       Fraction of ethanol imports that is cellulosic
     PMMFTAB_CLLCAPCD(MNUMCR,MNUMYR)          Cellulosic ethanol plant capacity
     PMMFTAB_CONEFF(MNUMYR)                   gallon ethanol per short ton cellulose
     PMMFTAB_CRNCAPCD(MNUMCR,MNUMYR)          Corn ethanol plant capacity MBCD
     PMMFTAB_DSCSHR(MNUMCR,MNUMYR)            CARB diesel share of total ultra low diesel
     PMMFTAB_DSMURS(MNUMCR,MNUMYR,M2)         1 = DISTRIBUTION MARKUP.. DS FUEL.. RESIDENTIAL Price 87$BTU
     PMMFTAB_DSMUTR(MNUMCR,MNUMYR,M2)         PRICE  87$BTU 2 = TAX MARKUP..DS FUEL.. TRANSPORT
     PMMFTAB_E85ICCREDIT(MNUMYR)              E85 infrastructure cost    87$BBL
     PMMFTAB_FUEL_CARB(M34,MNUMYR)            Factor relative to pure petroleum by fuel
     PMMFTAB_GRNCAPCD(MNUMCR,MNUMYR)          Grain ethanol plant capacity
     LFMMOUT_GRAINCD(MNUMCR,MNUMYR)             Grain (non-corn) used to make ethanol
     PMMFTAB_JFMUTR(MNUMCR,MNUMYR,M2)         JET FUEL TRANSPORTATION 87$BTU
     PMMFTAB_LCFSSAFE(MNUMYR)                 Safety valve for the Biofuels constraint in 1000 tons Carbon
*     PMMFTAB_CFPSAFE(MNUMYR)                  Safety valve for the Biofuels constraint in 1000 tons Carbon
     PMMFTAB_MGMUTR(MNUMCR,MNUMYR,M2)         REFORMULATED - MARKUP - MOTOR GASOLINE  TRANS  87$BTU
     PMMFTAB_MINREN(MNUMYR)                   Minimum renewable (including biodiesel CTL) in gasoline and diesel
     PMMFTAB_PALBOB(MNUMCR,MNUMYR)            Wholesale BOB price   87$BBL
     PMMFTAB_PALMG(MNUMCR,MNUMYR)             ALL COMBINED (UNLEAD(REG+PREM) + REFORMULATED87$BBL
     PMMFTAB_PDS(MNUMCR,MNUMYR)               87$BBL  PRICE DISTILLATE FUEL OIL --> PD   87$BBL
     PMMFTAB_PDSCRB(MNUMCR,MNUMYR)            87$BBL  CARB diesel price
     PMMFTAB_PDSU(MNUMCR,MNUMYR)              87$BBL  Distillate Subcategories
     PMMFTAB_PDSL(MNUMCR,MNUMYR)              87$BBL  Distillate Subcategories
     PMMFTAB_PJF(MNUMCR,MNUMYR)               87$BBL  PRICE JET FUEL
     PMMFTAB_PLMQTYCD(MNUMCR,MNUMYR)          MBCD   New palm oil quantity
     PMMFTAB_RFENVFX(MNUMCR,MNUMYR,M20)       87$BTU  ENVIRONMENTAL FIXED COST - 20 PRODUCTS
     PMMFTAB_RFIMPEXPEND(MNUMYR)              b87$    Expenditures on imports
     PMMFTAB_RFQNGPF(MNUMCR,MNUMYR)           MMBD    TOTAL SECT.PRDS
     PMMFTAB_RFQSGPF(MNUMCR,MNUMYR)           tBTU    Consumption of still gas feedstocks
     PMMFTAB_SBO_PRICE(MNUMCR,MNUMYR)         87$BBL  Soybean oil price
     PMMFTAB_SBO2GDTPD(MNUMPR,MNUMYR)         MMBD    Seed oil to GDT unit
     PMMFTAB_SBO2SAFPD(MNUMPR,MNUMYR)         MMBD    Seed oil to SAF unit
     PMMFTAB_SBOQTYCD(MNUMCR,MNUMYR)          MBCD    New seed oil quantity used to produce renewable diesel
     PMMFTAB_UBAVOLDS(MNUMPR,MNUMYR)          MBCD    Pyrolysis liquid blended into diesel
     PMMFTAB_UBAVOLMG(MNUMPR,MNUMYR)          MBCD    Pyrolysis liquid blended into mogas
     PMMFTAB_WGR2GDTPD(MNUMPR,MNUMYR)         MMBD    White grease to GDT unit
     PMMFTAB_WGR2SAFPD(MNUMPR,MNUMYR)         MMBD    White grease to SAF unit
     PMMFTAB_WS_RBOB(MNUMCR,MNUMYR)           87$BBL  Wholesale RBOB price
     PMMFTAB_YGR_PRICE(MNUMCR,MNUMYR)         87$BBL  Yellow grease price
     PMMFTAB_WGR_PRICE(MNUMCR,MNUMYR)         87$BBL  White grease price
     PMMFTAB_YGR2GDTPD(MNUMPR,MNUMYR)         MMBD    Yellow grease to GDT unit
     PMMFTAB_RFHCXH2IN(MNUMPR,MNUMYR)         MMBD  Hydrogen from natural gas input to refinery

*** PMMRPT tables
     PMMRPT_BIMQTYCD(M4,MNUMCR,MNUMYR)        MBCD    Biodiesel production by type and census
     PMMRPT_BIODCONCD(M2,MNUMCR,MNUMYR)       MBCD    Biodiesel consumption by virgin or non
     PMMRPT_BIODEXP(MNUMCR,MNUMYR)            MBCD    Biodiesel exports by census
     PMMRPT_BIODIMP(MNUMCR,MNUMYR)            MBCD    Biodiesel imports by census
     PMMRPT_BIODPRICE(MNUMCR,MNUMYR)          87$BBL  Biodiesel price
     LFMMOUT_RENEWDIMP(MNUMCR,MNUMYR)         MBCD    Renewable diesel imports by census
     PMMRPT_BLDIMP(MNUMPR,MNUMYR)             MMBD    BLENDING IMPORTS
     PMMRPT_CLLETHCD(MNUMCR,MNUMYR)           MBCD    ETHANOL PRODUCED FROM CELLULOSE
     PMMRPT_CRNETHCD(MNUMCR,MNUMYR)           MBCD    ETHANOL PRODUCED FROM CORN
     PMMRPT_ETHE85CD(MNUMCR,MNUMYR)           MMBD    Ethanol for E85 at Census division level
     PMMRPT_ETHEXP(MNUMCR,MNUMYR)             MBCD    Ethanol exports
     PMMRPT_ETHIMP(MNUMCR,MNUMYR)             MBCD    Ethanol imports
     PMMRPT_ETHTOTCD(MNUMCR,MNUMYR)           MMBD    Total ethanol production by census
     PMMRPT_GRNETHCD(MNUMCR,MNUMYR)           MBCD    Non-corn non-advanced ethanol produced from grain
     PMMRPT_MUFTAX(MNUMYR,M15)                87$BTU  FEDERAL TAXES - 15 PRODUCTS
     PMMRPT_OTHETHCD(MNUMCR,MNUMYR)           MBCD    Ethanol produced from other feedstock
     PMMRPT_PETHANOL(MNUMCR,MNUMYR)           87$BBL  Ethanol blending value
     PMMRPT_PETHM(MNUMCR,MNUMYR)              87$BBL  Marginal ethanol price
     PMMRPT_QPRDEX(PRDEXP,MNUMYR)             MBCD    Exports by product
     PMMRPT_RFCRDOTH(MNUMPR,MNUMYR)           MBCD    OTHER CRUDE INPUTS BY PADD & YR
     PMMRPT_RFDSTCAP(MNUMPR,MNUMYR)           MMBD    REFINERY DISTILLATION CAPACITY
     PMMRPT_RFDSTUTL(MNUMPR,MNUMYR)           MMBD    CAPACITY UTILIZATION RATE
     PMMRPT_RFETHE85(MNUMPR,MNUMYR)           MMBD    QUANTITY E85
     PMMRPT_RFIMCR(MNUMPR,MNUMYR)             MMBD    1 Crude net imports
     PMMRPT_RFIMTP(MNUMPR,MNUMYR)             MMBD    2 Total prod net imports
     PMMRPT_RFIPQCLL(MNUMPR,MNUMYR,M2)        BBL$$$  IMPORT CRUDE-LO SULFUR LT(P . Q)
     LFMMOUT_RFIPQMG(MNUMPR,MNUMYR,M2)        BBL$$$  IMPORTS CONV MOTOR GASOLINE (P.Q)
     LFMMOUT_RFIPQCBOB(MNUMPR,MNUMYR,M2)      BBL$$$  IMPORTS CBOB (P.Q)
     LFMMOUT_RFIPQRG(MNUMPR,MNUMYR,M2)        BBL$$$  IMPORTS REFORMULATED MOTOR GASOLINE (P.Q)
     LFMMOUT_RFIPQRBOB(MNUMPR,MNUMYR,M2)      BBL$$$  IMPORTS RBOB (P.Q)
     LFMMOUT_RFIPQPR(MNUMPR,MNUMYR,M2)        BBL$$$  IMPORTS propane (P.Q)
     LFMMOUT_RFIPQPY(MNUMPR,MNUMYR,M2)        BBL$$$  IMPORTS propylene (P.Q)
     LFMMOUT_RFIPQET(MNUMPR,MNUMYR,M2)        BBL$$$  IMPORTS ethane (P.Q)
     LFMMOUT_RFIPQBU(MNUMPR,MNUMYR,M2)        BBL$$$  IMPORTS butane (P.Q)
     LFMMOUT_RFIPQIS(MNUMPR,MNUMYR,M2)        BBL$$$  IMPORTS isobutane (P.Q)
     LFMMOUT_RFIPQPP(MNUMPR,MNUMYR,M2)        BBL$$$  IMPORTS pentanes plus (P.Q)
     LFMMOUT_RFIPQJF(MNUMPR,MNUMYR,M2)        BBL$$$  IMPORTS JET FUEL (P.Q)
     LFMMOUT_RFIPQDS(MNUMPR,MNUMYR,M2)        BBL$$$  IMPORTS DISTILLATE FUEL OIL (P.Q)
     LFMMOUT_RFIPQDL(MNUMPR,MNUMYR,M2)        BBL$$$  IMPORTS LOW SULFUR DIESEL (P.Q)
     LFMMOUT_RFIPQDU(MNUMPR,MNUMYR,M2)        BBL$$$  IMPORTS ULTRA LOW SULFUR DIESEL (P.Q)
     LFMMOUT_RFIPQRL(MNUMPR,MNUMYR,M2)        BBL$$$  IMPORTS LOW SULFUR RESIDUAL FUEL (P.Q)
     LFMMOUT_RFIPQRH(MNUMPR,MNUMYR,M2)        BBL$$$  IMPORTS HIGH SULFUR RESIDUAL FUEL (P.Q)
     LFMMOUT_RFIPQPC(MNUMPR,MNUMYR,M2)        BBL$$$  IMPORTS petroleum coke (P.Q)
     LFMMOUT_RFIPQPF(MNUMPR,MNUMYR,M2)        BBL$$$  IMPORTS PETROCHEMICAL FEEDSTOCKS (P.Q)
     LFMMOUT_RFIPQCG(MNUMPR,MNUMYR,M2)        BBL$$$  IMPORTS CarBOB (P.Q)
     LFMMOUT_RFIPQCD(MNUMPR,MNUMYR,M2)        BBL$$$  IMPORTS Carb diesel (P.Q)
     LFMMOUT_RFIPQAG(MNUMPR,MNUMYR,M2)        BBL$$$  IMPORTS Aviation Gasoline (P.Q)
     LFMMOUT_RFIPQAS(MNUMPR,MNUMYR,M2)        BBL$$$  IMPORTS Asphalt (P.Q)
     LFMMOUT_RFIPQLU(MNUMPR,MNUMYR,M2)        BBL$$$  IMPORTS Lubricants (P.Q)
     LFMMOUT_RFIPQMN3(MNUMPR,MNUMYR,M2)       BBL$$$  IMPORTS medium naphtha (P.Q)
     LFMMOUT_RFIPQGO3(MNUMPR,MNUMYR,M2)       BBL$$$  IMPORTS gas oil (P.Q)
     LFMMOUT_RFIPQAR3(MNUMPR,MNUMYR,M2)       BBL$$$  IMPORTS atmospheric residuum (P.Q)
     PMMRPT_RFMETM85(MNUMPR,MNUMYR)           MMBD    QUANT M85
     PMMRPT_RFMTBI(MNUMPR,MNUMYR)             MMBD    IMPORTED MTBE
     PMMRPT_RFMETI(MNUMPR,MNUMYR)             MMBD    IMPORTED METHANOL
     PMMRPT_RFPQIPRDT(MNUMPR,MNUMYR,M2)       MMBD    TOTAL PRODUCT IMPORTED
     PMMRPT_RFPQUFC(MNUMPR,MNUMYR,M2)         MMBD    TOTAL IMPORTS OF UNFINISHED REF MOD OUTPUT COMM
     PMMRPT_RFQARO(MNUMCR,MNUMYR)             MMBD    QUANTITY OF ASPHALT AND ROAD OIL
     PMMRPT_RFQDS(MNUMCR,MNUMYR)              MMBD    DISTILLATE FUEL OIL   --> QDSAS (MNUMCR . MNUMYR)
     PMMRPT_RFQEXCRD(MNUMPR,MNUMYR)           MMBD    CRUDE EXPORTED
     PMMRPT_RFQEXPRDT(MNUMPR,MNUMYR)          MMBD    PRODUCT EXPORTS
     PMMRPT_RFQICRD(MNUMPR,MNUMYR)            MMBD    IMPORTED TOTAL CRUDE MMBBLD per PADD
     PMMRPT_RFQJF(MNUMCR,MNUMYR)              MMBD    JET FUEL --> QJFTR(MNUMCR . MNUMYR)
     PMMRPT_RFQKS(MNUMCR,MNUMYR)              MMBD    KEROSENE --> QKSAS(MNUMCR . MNUMYR)
     PMMRPT_RFQLG(MNUMCR,MNUMYR)              MMBD    LPG --> QLGAS(MNUMCR . MNUMYR)
     PMMRPT_RFQMG(MNUMCR,MNUMYR)              MMBD    REFORMULATED
     PMMRPT_RFQOTH(MNUMCR,MNUMYR)             MMBD    OTHER --> QOTAS (MNUMCR . MNUMYR)
     PMMRPT_RFQPCK(MNUMCR,MNUMYR)             MMBD    QUANTITY OF PETROLEUM COKE
     PMMRPT_RFQPF(MNUMCR,MNUMYR)              MMBD    PETROCHEMICAL FEED STOCKS --> QPFIN (CR.YR)
     PMMRPT_RFQRH(MNUMCR,MNUMYR)              MMBD    RESIDUAL FUEL OIL HIGH SULFUR --> QRHAS(CR.YR)
     PMMRPT_RFQRL(MNUMCR,MNUMYR)              MMBD    RESIDUAL FUEL OIL LOW SULFUR --> QRLAS (CR.YR)
     PMMRPT_RFQSTG(MNUMCR,MNUMYR)             MMBD    QUANTITY OF STILL GAS
     PMMRPT_TDIESEL(MNUMCR,MNUMYR)            MBCD    Total diesel consumption
     LFMMOUT_RFPRDDIESEL(MNUMCR,MNUMYR)       MBCD    Diesel refinery production
     LFMMOUT_SBOQGD(MNUMPR,MNUMYR)            MBCD    Green nap or dist from seed oil
     LFMMOUT_SBOQRJH(MNUMPR,MNUMYR)           MBCD    HEFA-SPK renewable jet from seed oil
     LFMMOUT_WGRQRJH(MNUMPR,MNUMYR)           MBCD    HEFA-SPK renewable jet from white grease
     LFMMOUT_WGRQGD(MNUMPR,MNUMYR)            MBCD    Green nap or dist from white grease
     LFMMOUT_WGRQRDH(MNUMPR,MNUMYR)           MBCD    green diesel (RDH) from white grease
     LFMMOUT_YGRQGD(MNUMPR,MNUMYR)            MBCD    Green nap or dist from yellow grease


*** PMMOUT tables
     PMMOUT_BTLFRAC(M4,MNUMPR,MNUMYR)         BTL liquid produced by type Mbbl per cd
     PMMOUT_CBTLFRAC(M2,M4,MNUMPR,MNUMYR)     Liquids produced from coal or biomass combo plant (1 if by coal 2 if by biomass)  MMbbl per cd
     PMMOUT_CRNPRICE(MNUMCR,MNUMYR)           87$BU   Price of corn
     PMMOUT_CTLFRAC(M4,MNUMPR,MNUMYR)         CTL liquid produced by type Mbbl per cd
     PMMOUT_GLBCRDDMD(MNUMYR)                 World crude oil demand (PMM results)  MMbbl per cd
     PMMOUT_GTLFRAC(M4,MNUMPR,MNUMYR)         MMBD    Liquids from gas by type
     PMMOUT_PRDSTKWDR(MNUMPR,MNUMYR)          MMBD    Petroleum product stock withdrawal
     PMMOUT_QBMRFBTL(MNUMCR,MNUMYR)           Biomass for BTL   Tril Btu per Yr
     PMMOUT_QCLRFPD(MNUMPR,MNUMYR)            Coal for CTL  Tril Btu per Yr
     PMMOUT_QMERF(MNUMCR,MNUMYR)              Methanol purchased by refineries Tril Btu per Yr
     PMMOUT_RFQNGPL(MNUMPR,MNUMYR,M6)         Quantity of streams from natural gas processing plants in M bbl per day
     PMMOUT_RFPQNGL(MNUMPR,MNUMYR,M6,M2)      PRICE-QUANITY OF NGL BY PADD FOR 6 NGL'S
     PMMOUT_RFQPRCG(MNUMPR,MNUMYR)            MMBD    Refinery processing gain
     PMMOUT_RFSPRIM(MNUMYR)                   MMBD    SPR IMPORTS
     PMMOUT_UBAVOL(MNUMPR,MNUMYR)             Pyrolysis liquid produced  Mbbl per cd
     PMMOUT_XTL_CO2AVAIL(MNUMPR,MNUMYR)       CO2 fr xTL (less 15% to atm) avail for sequestration in MM tonne per yr
     PMMOUT_RFSPRFR(MNUMYR)                   SPR withdrawals in MM bbl per day

     NGTDMREP_OGHHPRNG(MNUMYR)                Price of natural gas at Henry Hub in 87$ per MCF

*** AB32 tables
     AB32_AB_ALLOW_P(MNUMYR)                  Allowance price 87$ per kg Ceq
     AB32_AB_COVD_EM_REF(MNUMYR)              Covered emissions - refining in MM tonnes Ceq per year

*** HMM refinery H2
     HMMBLK_PH2RF(MNUMCR,MNUMYR)              Price of H2 to refineries by cd in 87$ per BFOE
     QMORE_QH2RF(MNUMCR,MNUMYR)                Market H2 consumption by census division Tril Btu per Yr

*** CCATS data outputs from LFMM
     CCATSDAT_SUP_ETH_45Q(MNUMCR,MNUMYR)       CO2 volumes from ethanol production that are 45Q eligible
     CCATSDAT_SUP_ETH_NTC(MNUMCR,MNUMYR)       CO2 volumes from ethanol production no tax credit
     CCATSDAT_CST_ETH_INV(MNUMCR,MNUMYR)       Investment cost for carbon capture from ethanol production
     CCATSDAT_CST_ETH_OM(MNUMCR,MNUMYR)        O&M cost for carbon capture from ethanol production

*** CCATS data inputs to LFMM
     TCS45Q_CCS_EOR_45Q(MNUMYR)                45Q tax credit for enhanced oil recovery
     TCS45Q_CCS_SALINE_45Q(MNUMYR)             45Q tax credit for saline injection
*     TCS45Q_I_45Q_LYR_NEW(1)                   End year of tax code section 45Q subsidy for new builds

     CCATSDAT_CO2_PRC_DIS_45Q(MNUMCR,MNUMYR)   CO2 price output after optimization (45Q eligible CO2) Census Division
     CCATSDAT_CO2_PRC_DIS_NTC(MNUMCR,MNUMYR)   CO2 price output after optimization (no tax credit) Census Division


;

$gdxin %NEM_TO_LFMM%

*Sets
$Load NCNTRL_CURCALYR,NCNTRL_CURITR,NCNTRL_FCRL,NCNTRL_NCRL,CYCLEINFO_CURIRUN,MNUMCR,MNUMC2,MNUMYR,NUMCGF
$Load M1000, CORNTO

$Load MNUMFS,MNUMPR,MNMFS1,NDRGN1,MNMYRF,MNUMY3,WDCRVS,ECPCAP,MAXNF2,MAXNFR
$Load RefReg, NDREGN,MX_NCL,ECPFPH,MNXYRS,MX_NCI,MX_SO2, MX_RNK, NUTSEC
$Load CRSTEP,CISTEP,MCSTEP,MNPROD,MNCRUD,OGDIST, PRDEXP
$Load INSTEP, INTREG

$Load COALEMM_RCLCLNR,COALEMM_PLNT_EMF,
$Load COALEMM_N_RG,COALEMM_N_RY,COALEMM_N_IGRP,COALEMM_N_PLTS,COALEMM_N_CFR
$Load COALEMM_N_HRAT,COALEMM_N_CPTY,COALEMM_N_PTP

$Load EMISSION_NUM_SO2_GRP,EMISSION_SO2_SHR_BY_CLRG,EMISSION_CCS_PMM,EMABLK_JCLCLNR,EMISSION_EXTRARISK

$Load COGEN_CGREFCAP, COGEN_CGREFQ, COGEN_CGREFGEN

$Load CONVFACT_CFASQ,CONVFACT_CFBUQ,CONVFACT_CFIBQ,CONVFACT_CFJFK,CONVFACT_CFJFN, CONVFACT_CFKSQ, CONVFACT_CFETQ
$Load CONVFACT_CFBMQ,CONVFACT_CFBIOD,CONVFACT_CFVEGGIE,CONVFACT_CFCNGQ,CONVFACT_CFIMUO,CONVFACT_CFNGCL,CONVFACT_CFGTLLIQ
$Load CONVFACT_CFBTLLIQ, CONVFACT_CFCBTLLIQ, CONVFACT_CFGRAIN, CONVFACT_CFBIOBUTE
$Load CONVFACT_CFCLSN,CONVFACT_CFDSRS,CONVFACT_CFDSCM,CONVFACT_CFDSTR,CONVFACT_CFDSIN,CONVFACT_CFDSEL,CONVFACT_CFDSQT
$Load CONVFACT_CFDSUQ, CONVFACT_CFDSLQ, CONVFACT_CFJFQ, CONVFACT_CFDSQ,CONVFACT_CFMEQT,CONVFACT_CFCORN,CONVFACT_CFCELL
$Load CONVFACT_CFEEQ,CONVFACT_CFNPQ,CONVFACT_CFAVQ,CONVFACT_CFLUQ,CONVFACT_CFWXQ,CONVFACT_CFMSQ ,CONVFACT_CFUSQ,CONVFACT_CFJOULE
$Load CONVFACT_CFELQ,CONVFACT_CFOGQ,CONVFACT_CFPPQ,CONVFACT_CFPCQ,CONVFACT_CFPRQ,CONVFACT_CFRSQ,CONVFACT_CFSGQ
$Load CONVFACT_CFEXPRD,CONVFACT_CFCRDDOM,CONVFACT_CFCRDIMP,CONVFACT_CFCRDEXP,CONVFACT_CFNGL,CONVFACT_CFE85Q,CONVFACT_CFM85Q,CONVFACT_CFCTLLIQ
$Load CONVFACT_CFCRDLTSWT, CONVFACT_CFCRDLTSOUR, CONVFACT_CFCRDMD2SOUR, CONVFACT_CFCRDMDSOUR
$Load CONVFACT_CFCRDHVSWT, CONVFACT_CFCRDHVSOUR
$Load CONVFACT_CFCRDCA, CONVFACT_CFCRDSYN, CONVFACT_CFCRDDILBIT, CONVFACT_CFCRDLT2SWT
$Load CONVFACT_CFCRDLSCOND
$Load CONVFACT_CFLGQ,CONVFACT_CFMGQ,CONVFACT_CFOTQ,CONVFACT_CFTPQ,CONVFACT_CFPFQ,CONVFACT_CFNGU,CONVFACT_CFNGN
$Load CONVFACT_CFNGC,CONVFACT_CFNGI,CONVFACT_CFNGE,CONVFACT_CFTGQ,CONVFACT_CFRGQ,CONVFACT_CFIMPRD
$Load CONVFACT_CFAR3, CONVFACT_CFGO3, CONVFACT_CFGOP, CONVFACT_CFMN3, CONVFACT_CFUBAQ, CONVFACT_CFCCQ
$Load CONVFACT_CFCBQ, CONVFACT_CFDSCQ, CONVFACT_CFCBOB, CONVFACT_CFRBOB, CONVFACT_CFFTLIQ
$Load CONVFACT_CFPET

$Load INTOUT_BRENT_PRICE, INTOUT_START_PRICE, INTOUT_IT_WOP, MXPBLK_XIT_WOP
$Load INTOUT_P_C_MC_Demand,INTOUT_Q_C_MC_Demand
$Load INTOUT_P_Total_Crude,INTOUT_P_Foreign_Crude,INTOUT_Q_Foreign_Crude,INTOUT_P_Non_US_Demand,INTOUT_Q_Non_US_Demand,INTOUT_Q_Total_Crude
$Load INTOUT_Product_Import_P,INTOUT_Product_Import_Q,INTOUT_Product_Export_P,INTOUT_Product_Export_Q


$Load LFMMOUT_BIODEXPPD, LFMMOUT_RFSCREDPRC, LFMMOUT_RFSCREDITS, LFMMOUT_RFSSAFETY, LFMMOUT_RFSMANDATES
$Load LFMMOUT_RFSACTUAL
$Load LFMMOUT_GRN2MGQTY, LFMMOUT_ETHTOT, LFMMOUT_BIMQTY, LFMMOUT_BIODIMPPD, LFMMOUT_RENEWDIMPPD
$Load LFMMOUT_Q_CRUDE_IMPORTS, LFMMOUT_P_CRUDE_IMPORTS, LFMMOUT_GRD2DSQTY, LFMMOUT_SAF2JTQTY
$Load LFMMOUT_Q_CRUDE_EXPORTS, LFMMOUT_P_CRUDE_EXPORTS, LFMMOUT_Q_CRUDE_TO_CAN, LFMMOUT_P_CRUDE_TO_CAN
$Load LFMMOUT_Q_CRUDE_IMPORTA
$Load LFMMOUT_LCFS_Baseline, LFMMOUT_LCFS_Actual, LFMMOUT_LCFS_Waiver, LFMMOUT_LCFS_Offset_Prc
$Load LFMMOUT_LCFS_PeToTrills, LFMMOUT_LCFS_Carb_Offset
$Load LFMMOUT_CFP_Baseline, LFMMOUT_CFP_Actual, LFMMOUT_CFP_Waiver, LFMMOUT_CFP_Offset_Prc
$Load LFMMOUT_CFP_PeToTrills, LFMMOUT_CFP_Carb_Offset
$Load LFMMOUT_WACFS_Baseline, LFMMOUT_WACFS_Actual, LFMMOUT_WACFS_Waiver, LFMMOUT_WACFS_Offset_Prc
$Load LFMMOUT_WACFS_PeToTrills, LFMMOUT_WCFS_Carb_Offset
$Load LFMMOUT_REFGAIN, LFMMOUT_QNGRFPD
$Load LFMMOUT_RFIPQCG, LFMMOUT_RFIPQAG, LFMMOUT_RFIPQAS, LFMMOUT_RFIPQLU, LFMMOUT_RFIPQCD
$Load LFMMOUT_MOTOR_FUEL, LFMMOUT_DIST_FUEL, LFMMOUT_FEEDSTOCKS, LFMMOUT_INTERMEDIATE
$Load LFMMOUT_REFINE_PROD, LFMMOUT_GROSS_IMPORT, LFMMOUT_GROSS_EXPORT, LFMMOUT_DOM_CONSUME
$Load LFMMOUT_AB32JETCOVER
$Load LFMMOUT_AB32_DS, LFMMOUT_AB32_KS, LFMMOUT_AB32_PR, LFMMOUT_AB32_MG, LFMMOUT_AB32_ET, LFMMOUT_AB32_JF

$Load LFMMOUT_LFMMCODE, LFMMOUT_LFEXPLEASE, LFMMOUT_LFREFRENT

$Load LFMMOUT_RFS_WAIVER, LFMMOUT_RFSDSTR, LFMMOUT_RFSMGTR, LFMMOUT_RFSRBOB
*$Load LFMMOUT_RFSJFTR, LFMMOUT_RFSDSRS
$Load LFMMOUT_REF_CAP, LFMMOUT_REF_UTL
$Load LFMMOUT_PROFIT_BBL, LFMMOUT_RFCRUDEWHP

$Load LFMMOUT_P_RFCRUDEINP
$Load LFMMOUT_RFCRUDEINP, LFMMOUT_RFOTHERINP, LFMMOUT_BIOBUTESTK, LFMMOUT_RFBIOBUTECD, LFMMOUT_RFBIOBUTERR
$Load LFMMOUT_QBIOBUTE, LFMMOUT_BIOBUTEIMP, LFMMOUT_BIOBUTEEXP, LFMMOUT_BIOBUTEPRICE
$Load LFMMOUT_REFPRODET, LFMMOUT_REFPRODPR, LFMMOUT_REFPRODBU, LFMMOUT_REFPRODIS
$Load LFMMOUT_REFPRODPP, LFMMOUT_REFPRODPY, LFMMOUT_REFPRODOO
$Load LFMMOUT_REFINPET, LFMMOUT_REFINPPR, LFMMOUT_REFINPBU, LFMMOUT_REFINPIS
$Load LFMMOUT_REFINPPP, LFMMOUT_REFINPPY, LFMMOUT_REFINPOO

$Load MACOUT_MC_JPGDP, MCDETAIL_MC_DETAIL, MACOUT_MC_RMCORPBAA, MACOUT_MC_RMTCM10Y, MACOUT_MC_NP

$Load MPBLK_PDSAS, MPBLK_PDSCM, MPBLK_PDSEL, MPBLK_PDSIN, MPBLK_PDSRS, MPBLK_PDSTR,MPBLK_PASIN
$Load MPBLK_PETTR, MPBLK_PKSAS, MPBLK_PKSCM, MPBLK_PKSIN, MPBLK_PKSRS, MPBLK_PLGAS,MPBLK_PJFTR
$Load MPBLK_PLGCM, MPBLK_PLGIN, MPBLK_PLGRS, MPBLK_PLGTR, MPBLK_PMETR
$Load MPBLK_PMGAS, MPBLK_PMGCM, MPBLK_PMGIN, MPBLK_PMGTR, MPBLK_POTAS, MPBLK_POTIN,MPBLK_POTTR
$Load MPBLK_PPFIN, MPBLK_PRHAS, MPBLK_PRHEL, MPBLK_PRHTR, MPBLK_PRLAS, MPBLK_PRLCM, MPBLK_PRLEL
$Load MPBLK_PRLIN, MPBLK_PRLTR, MPBLK_PELIN, MPBLK_PNGIN

$Load MXPBLK_XPELIN, MXPBLK_XPGIIN, MXPBLK_XPALMG, MXPBLK_XPDSTR

$Load MXQBLK_XQDSTR, MXQBLK_XQDSIN, MXQBLK_XQDSCM, MXQBLK_XQDSRS, MXQBLK_XQDSEL, MXQBLK_XQMGAS
$Load MXQBLK_XQJFTR, MXQBLK_XQKSAS, MXQBLK_XQLGAS, MXQBLK_XQLGRF, MXQBLK_XQRLAS, MXQBLK_XQRLEL
$Load MXQBLK_XQPFIN, MXQBLK_XQPCAS, MXQBLK_XQPCRF, MXQBLK_XQOTAS, MXQBLK_XQOTRF, MXQBLK_XQETTR
$Load MXQBLK_XQRLRF, MXQBLK_XQRHAS, MXQBLK_XQRHEL, MXQBLK_XQASIN, MXQBLK_XQSGIN, MXQBLK_XQSGRF
$Load MXQBLK_XQLGTR, MXQBLK_XQNGTR, MXQBLK_XQLUIN, MXQBLK_XQOTIN

$Load OGSMOUT_OGCRDHEAT,OGSMOUT_OGNGPLPRD,OGSMOUT_OGCRUDEREF
$Load OGSMOUT_OGNGPLET, OGSMOUT_OGNGPLPR, OGSMOUT_OGNGPLBU, OGSMOUT_OGNGPLIS, OGSMOUT_OGNGPLPP
$Load OGSMOUT_OGCO2TAR,OGSMOUT_OGCO2AVL,OGSMOUT_OGCO2PRC,OGSMOUT_OGCO2PUR2
$Load OGSMOUT_OGCO2QLF,OGSMOUT_OGCO2PLF

$Load UECPOUT_TnS_Costs, UECPOUT_FR_OR_TRANCOST
$Load UECPOUT_MUST_STORE

$Load PMMFTAB_ADVCAPCD,PMMFTAB_BANKCRED,PMMFTAB_BANKUSED
$Load PMMFTAB_CBIODUAL,PMMFTAB_CELLIMPFRAC,PMMFTAB_CLLCAPCD,PMMFTAB_CONEFF
$Load PMMFTAB_CRNCAPCD,PMMFTAB_DSCSHR,PMMFTAB_DSMURS
$Load PMMFTAB_DSMUTR,PMMFTAB_E85ICCREDIT,PMMFTAB_FUEL_CARB
$Load PMMFTAB_GRNCAPCD,LFMMOUT_GRAINCD,PMMFTAB_JFMUTR,PMMFTAB_LCFSSAFE
*$Load PMMFTAB_CFPSAFE
$Load PMMFTAB_MGMUTR,PMMFTAB_MINREN,PMMFTAB_PALBOB,PMMFTAB_PALMG
$Load PMMFTAB_PDS,PMMFTAB_PDSCRB,PMMFTAB_PDSU,PMMFTAB_PDSL
$Load PMMFTAB_PJF,PMMFTAB_PLMQTYCD,PMMFTAB_RFENVFX,PMMFTAB_RFIMPEXPEND,PMMFTAB_RFQNGPF
$Load PMMFTAB_RFQSGPF,PMMFTAB_SBO_PRICE,PMMFTAB_SBO2GDTPD,PMMFTAB_SBO2SAFPD, PMMFTAB_SBOQTYCD,PMMFTAB_UBAVOLDS
$Load PMMFTAB_UBAVOLMG,PMMFTAB_WGR2GDTPD,PMMFTAB_WGR2SAFPD, PMMFTAB_WS_RBOB,PMMFTAB_YGR_PRICE,PMMFTAB_WGR_PRICE,PMMFTAB_YGR2GDTPD
$Load PMMFTAB_RFHCXH2IN

$Load PMMOUT_CRNPRICE,PMMOUT_ETHNE85,PMMOUT_GTLFRAC,PMMOUT_PRDSTKWDR
$Load PMMOUT_QBMRFBTL,PMMOUT_BTLFRAC, PMMOUT_CTLFRAC, PMMOUT_CBTLFRAC, PMMOUT_UBAVOL
$Load PMMOUT_QCLRFPD, PMMOUT_QMERF, PMMOUT_GLBCRDDMD, PMMOUT_RFSPRFR
$Load PMMOUT_RFQNGPL,PMMOUT_RFPQNGL,PMMOUT_RFQPRCG,PMMOUT_RFSPRIM,PMMOUT_TRGNE85,PMMOUT_XTL_CO2AVAIL
$Load PMMRPT_BIMQTYCD,PMMRPT_BIODCONCD,PMMRPT_BIODEXP,PMMRPT_BIODIMP,PMMRPT_BIODPRICE,LFMMOUT_RENEWDIMP
$Load PMMRPT_BLDIMP,PMMRPT_CLLETHCD
$Load LFMMOUT_CORNCD,PMMRPT_CRNETHCD,PMMRPT_ETHE85CD,PMMRPT_ETHEXP
$Load PMMRPT_ETHIMP,PMMRPT_ETHTOTCD,PMMRPT_GRNETHCD,PMMRPT_MUFTAX,PMMRPT_OTHETHCD,PMMRPT_PETHANOL,PMMRPT_PETHM
$Load PMMRPT_QPRDEX
$Load PMMRPT_RFCRDOTH,PMMRPT_RFDSTCAP,PMMRPT_RFDSTUTL,PMMRPT_RFETHE85,PMMRPT_RFIMCR,PMMRPT_RFIMTP,PMMRPT_RFIPQCLL
$Load PMMRPT_RFMETM85,PMMRPT_RFMTBI,PMMRPT_RFMETI,PMMRPT_RFPQIPRDT,PMMRPT_RFPQUFC,PMMRPT_RFQARO,PMMRPT_RFQDS,PMMRPT_RFQEXCRD
$Load PMMRPT_RFQEXPRDT,PMMRPT_RFQICRD,PMMRPT_RFQJF,PMMRPT_RFQKS,PMMRPT_RFQLG,PMMRPT_RFQMG,PMMRPT_RFQOTH,PMMRPT_RFQPCK
$Load PMMRPT_RFQPF,PMMRPT_RFQRH,PMMRPT_RFQRL,PMMRPT_RFQSTG,PMMRPT_TDIESEL,LFMMOUT_SBOQGD,LFMMOUT_WGRQGD,LFMMOUT_YGRQGD
$Load LFMMOUT_SBOQRJH, LFMMOUT_WGRQRJH, LFMMOUT_WGRQRDH
$Load LFMMOUT_RFIPQMG,LFMMOUT_RFIPQRG,LFMMOUT_RFIPQJF,LFMMOUT_RFIPQPF
$Load LFMMOUT_RFIPQDS,LFMMOUT_RFIPQDL,LFMMOUT_RFIPQDU,LFMMOUT_RFIPQRL,LFMMOUT_RFIPQRH,LFMMOUT_RFIPQPC
$Load LFMMOUT_RFIPQPR,LFMMOUT_RFIPQPY,LFMMOUT_RFIPQET,LFMMOUT_RFIPQBU,LFMMOUT_RFIPQIS,LFMMOUT_RFIPQPP
$Load LFMMOUT_RFIPQCBOB,LFMMOUT_RFIPQRBOB,LFMMOUT_RFIPQMN3,LFMMOUT_RFIPQGO3,LFMMOUT_RFIPQAR3
$Load LFMMOUT_RFPRDDIESEL

$Load PMORE_PBUIN, PMORE_PPRIN, PMORE_PISIN, PMORE_PISINPF, PMORE_PLUIN, PMORE_PPRTR
$Load PMORE_PLGINPF, PMORE_PPRRS, PMORE_PPRCM, PMORE_PPPIN, PMORE_PPPINPF, PMORE_PETIN, PMORE_PPCIN
$Load PMORE_PPROLENERF, PMORE_PSULFURIN, PMORE_PETINPF, PMORE_PBUINPF, PMORE_PPRINPF

$Load PONROAD_PDSTRHWY, QONROAD_QDSTRHWY, QONROAD_CFDSTRHWY, UEFPOUT_PELBS

$Load QBLK_QCLRF, QBLK_QELRF, QBLK_QNGRF, QBLK_QOTRF, QBLK_QETTR
$Load QBLK_QDSTR, QBLK_QDSIN, QBLK_QDSCM, QBLK_QDSRS, QBLK_QDSEL
$Load QBLK_QJFTR, QBLK_QKSAS, QBLK_QKSRS, QBLK_QKSCM, QBLK_QKSIN, QBLK_QLGAS, QBLK_QLGRF
$Load QBLK_QLGRS, QBLK_QLGCM, QBLK_QLGIN, QBLK_QLGTR
$Load QBLK_QNGTR
$Load QBLK_QMGAS, QBLK_QMGTR, QBLK_QMGIN, QBLK_QMGCM
$Load QBLK_QPFIN, QBLK_QPCAS, QBLK_QPCRF, QBLK_QOTAS, QBLK_QOTIN, QBLK_QOTTR
$Load QBLK_QRHAS, QBLK_QRHEL, QBLK_QRHTR
$Load QBLK_QRLAS, QBLK_QRLEL, QBLK_QRLIN, QBLK_QRLTR, QBLK_QRLCM
$Load QBLK_QRLRF, QBLK_QASIN, QBLK_QSGIN, QBLK_QSGRF, QBLK_QBMRF

$Load QMORE_QPRRS, QMORE_QPRCM, QMORE_QPRTR
$Load QMORE_QETIN, QMORE_QPRIN, QMORE_QISIN, QMORE_QBUIN
$Load QMORE_QETINPF, QMORE_QPRINPF, QMORE_QISINPF, QMORE_QBUINPF
$Load QMORE_QPRRF, QMORE_QISRF, QMORE_QBURF, QMORE_QPYRF
$Load QMORE_QPPINPF, QMORE_QPROLENERF, QMORE_QLUIN, QMORE_QPPIN

$Load INDOUT_INQLGHP

$Load INDREP_QCCRF

$Load TRANREP_FCLOGIT0, TRANREP_FCLOGIT1, TRANREP_FCLOGIT2, TRANREP_FCLOGIT3, TRANREP_FCLOGIT4
$Load TRANREP_QAGTR,TRANREP_QLUTR,TRANREP_XQAGTR,TRANREP_XQLUTR
$Load TRANREP_QFFV, TRANREP_XQFFV, TRANREP_E85AVAIL, TRANREP_E10SHARE
$Load TRANREP_TRQLDV, TRANREP_XTRQLDV

$Load EMABLK_JMGTR, EMABLK_JMGIN, EMABLK_JMGCM, EMABLK_JJFTR
$Load EMABLK_JDSTR, EMABLK_JDSRS, EMABLK_JDSCM, EMABLK_JDSIN
$Load EMABLK_JDSEL, EMABLK_JKSRS, EMABLK_JKSCM, EMABLK_JKSIN
$Load EMABLK_JLGRS, EMABLK_JLGCM, EMABLK_JLGTR, EMABLK_JLGIN
$Load EMABLK_JNQLGPF, EMABLK_JPRRS, EMABLK_JPRCM, EMABLK_JPRTR
$Load EMABLK_JETIN, EMABLK_JPRIN, EMABLK_JBUIN, EMABLK_JISIN
$Load EMABLK_JETINPF, EMABLK_JPRINPF, EMABLK_JBUINPF, EMABLK_JISINPF
$Load EMABLK_JRLCM, EMABLK_JRLTR, EMABLK_JRLIN, EMABLK_JRLEL
$Load EMABLK_JRHTR, EMABLK_JRHEL, EMABLK_JRSCM, EMABLK_JRSTR
$Load EMABLK_JRSIN, EMABLK_JRSEL, EMABLK_JPFIN, EMABLK_JPCIN
$Load EMABLK_JPPIN, EMABLK_JPPINPF, EMABLK_JLUIN, EMABLK_JOTIN
$Load EMABLK_JOTTR, EMABLK_JMETR, EMABLK_JETTR

$Load USO2GRP_XCL_1TESC,USO2GRP_XCL_2TESC

$Load USO2GRP_XCL_BTU,USO2GRP_XCL_CAR,USO2GRP_XCL_HG,USO2GRP_XCL_SO2,EMISSION_EMELPSO2,EMISSION_EMEL_PHG,USO2GRP_EFD_RANK,USO2GRP_XCL_STEPS
$Load USO2GRP_XCL_PCAP,USO2GRP_XCL_MX_PCAP,USO2GRP_CTL_OTHER,USO2GRP_CTL_TRATE,USO2GRP_CTL_CLDR,USO2GRP_CTL_CDSL1,USO2GRP_CTL_TYPE
$Load USO2GRP_XCL_QECP, USO2GRP_XCL_PECP

$Load WRENEW_MP_BM_BT, WRENEW_MP_BM_CM, WRENEW_MP_BM_ET, WRENEW_MP_BM_H2, WRENEW_MP_BM_IN, WRENEW_MP_BM_PW, WRENEW_MP_BM_RS
$Load WRENEW_QBMPWCL,WRENEW_QBMRSCL,WRENEW_QBMINCL,WRENEW_QBMH2CL
$Load WRENEW_QCLETH, WRENEW_QELETH, WRENEW_QNGETH,WRENEW_QBMET, WRENEW_QBMETCL, WRENEW_QBMBTCL
$Load WRENEW_WDSUP_P_AG,WRENEW_WDSUP_P_EC,WRENEW_WDSUP_P_FR,WRENEW_WDSUP_P_UM
$Load WRENEW_WDSUP_Q_AG,WRENEW_WDSUP_Q_EC,WRENEW_WDSUP_Q_FR,WRENEW_WDSUP_Q_UM

$Load WRENEW_CRNSUP_TOT_Q, WRENEW_CRNSUP_ETH_Q, WRENEW_CRNSUP_P
$Load WRENEW_SOYOILSUP_TOT_Q, WRENEW_SOYOILSUP_BIOD_Q, WRENEW_SOYOILSUP_P
$Load WRENEW_SOYSUP_TOT_Q, WRENEW_SOYSUP_P

$Load AB32_AB_ALLOW_P, AB32_AB_COVD_EM_REF

$Load QSBLK_QSDSRS,QSBLK_QSLGRS,QSBLK_QSKSRS,QSBLK_QSDSCM,QSBLK_QSLGCM,QSBLK_QSMGCM
$Load QSBLK_QSKSCM,QSBLK_QSLGTR,QSBLK_QSMGTR,QSBLK_QSETTR,QSBLK_QSDSTR,QSBLK_QSOTTR
$Load QSBLK_QSDSIN,QSBLK_QSKSIN,QSBLK_QSLGIN,QSBLK_QSPFIN,QSBLK_QSASIN,QSBLK_QSOTIN
$Load QSBLK_QSSGIN,QSBLK_QSMGIN,QSBLK_QSJFTR

$Load EMISSION_EMETAX,EMISSION_EMBTAX

$Load EMEBLK_EMGTR,EMEBLK_EJFTR,EMEBLK_EDSTR,EMEBLK_ELGTR,EMEBLK_ERSTR,EMEBLK_EOTTR
$Load EMEBLK_EPFIN,EMEBLK_ENQLGPF,EMEBLK_ESGIN,EMEBLK_EOTIN,EMEBLK_EETTR,EMEBLK_EAGTR,EMEBLK_ENGIN

$Load CONVFACT_APICAMG
$Load CONVFACT_APILTSW, CONVFACT_APILTSO, CONVFACT_APIMMSO, CONVFACT_APIMDSO
$Load CONVFACT_APIHVSW, CONVFACT_APIHVSO, CONVFACT_APICA,   CONVFACT_APISYN
$Load CONVFACT_APIDIL,  CONVFACT_APILLSW, CONVFACT_API50PL
$Load CONVFACT_APICRDDOM, CONVFACT_APICRDIMP, CONVFACT_APICRDEXP

$Load NGTDMREP_OGHHPRNG

$Load HMMBLK_PH2RF, QMORE_QH2RF

$Load CCATSDAT_SUP_ETH_45Q, CCATSDAT_SUP_ETH_NTC, CCATSDAT_CST_ETH_INV, CCATSDAT_CST_ETH_OM
$Load TCS45Q_CCS_EOR_45Q, TCS45Q_CCS_SALINE_45Q
$Load CCATSDAT_CO2_PRC_DIS_45Q, CCATSDAT_CO2_PRC_DIS_NTC

$gdxin


*********************************************************************
*** The following section does data manipulation on the
*** corn and soy supply curve data imported from NEMS/WRENEW
*** - fill in data for years 2011-2013 using 2014 data (HARDCODED)

   WRENEW_CRNSUP_TOT_Q(M10,NDRGN1,'2011_MNMYRF')     = WRENEW_CRNSUP_TOT_Q(M10,NDRGN1,'2014_MNMYRF') ;
   WRENEW_CRNSUP_TOT_Q(M10,NDRGN1,'2012_MNMYRF')     = WRENEW_CRNSUP_TOT_Q(M10,NDRGN1,'2014_MNMYRF') ;
   WRENEW_CRNSUP_TOT_Q(M10,NDRGN1,'2013_MNMYRF')     = WRENEW_CRNSUP_TOT_Q(M10,NDRGN1,'2014_MNMYRF') ;

   WRENEW_CRNSUP_ETH_Q(M10,NDRGN1,'2011_MNMYRF')     = WRENEW_CRNSUP_ETH_Q(M10,NDRGN1,'2014_MNMYRF') ;
   WRENEW_CRNSUP_ETH_Q(M10,NDRGN1,'2012_MNMYRF')     = WRENEW_CRNSUP_ETH_Q(M10,NDRGN1,'2014_MNMYRF') ;
   WRENEW_CRNSUP_ETH_Q(M10,NDRGN1,'2013_MNMYRF')     = WRENEW_CRNSUP_ETH_Q(M10,NDRGN1,'2014_MNMYRF') ;

   WRENEW_CRNSUP_P(M10,NDRGN1,'2011_MNMYRF')         = WRENEW_CRNSUP_P(M10,NDRGN1,'2014_MNMYRF') ;
   WRENEW_CRNSUP_P(M10,NDRGN1,'2012_MNMYRF')         = WRENEW_CRNSUP_P(M10,NDRGN1,'2014_MNMYRF') ;
   WRENEW_CRNSUP_P(M10,NDRGN1,'2013_MNMYRF')         = WRENEW_CRNSUP_P(M10,NDRGN1,'2014_MNMYRF') ;

   WRENEW_SOYOILSUP_TOT_Q(M10,NDRGN1,'2011_MNMYRF')  = WRENEW_SOYOILSUP_TOT_Q(M10,NDRGN1,'2014_MNMYRF') ;
   WRENEW_SOYOILSUP_TOT_Q(M10,NDRGN1,'2012_MNMYRF')  = WRENEW_SOYOILSUP_TOT_Q(M10,NDRGN1,'2014_MNMYRF') ;
   WRENEW_SOYOILSUP_TOT_Q(M10,NDRGN1,'2013_MNMYRF')  = WRENEW_SOYOILSUP_TOT_Q(M10,NDRGN1,'2014_MNMYRF') ;

   WRENEW_SOYOILSUP_BIOD_Q(M10,NDRGN1,'2011_MNMYRF') = WRENEW_SOYOILSUP_BIOD_Q(M10,NDRGN1,'2014_MNMYRF') ;
   WRENEW_SOYOILSUP_BIOD_Q(M10,NDRGN1,'2012_MNMYRF') = WRENEW_SOYOILSUP_BIOD_Q(M10,NDRGN1,'2014_MNMYRF') ;
   WRENEW_SOYOILSUP_BIOD_Q(M10,NDRGN1,'2013_MNMYRF') = WRENEW_SOYOILSUP_BIOD_Q(M10,NDRGN1,'2014_MNMYRF') ;

   WRENEW_SOYOILSUP_P(M10,NDRGN1,'2011_MNMYRF')      = WRENEW_SOYOILSUP_P(M10,NDRGN1,'2014_MNMYRF') ;
   WRENEW_SOYOILSUP_P(M10,NDRGN1,'2012_MNMYRF')      = WRENEW_SOYOILSUP_P(M10,NDRGN1,'2014_MNMYRF') ;
   WRENEW_SOYOILSUP_P(M10,NDRGN1,'2013_MNMYRF')      = WRENEW_SOYOILSUP_P(M10,NDRGN1,'2014_MNMYRF') ;

   WRENEW_SOYSUP_TOT_Q(M10,NDRGN1,'2011_MNMYRF')     = WRENEW_SOYSUP_TOT_Q(M10,NDRGN1,'2014_MNMYRF') ;
   WRENEW_SOYSUP_TOT_Q(M10,NDRGN1,'2012_MNMYRF')     = WRENEW_SOYSUP_TOT_Q(M10,NDRGN1,'2014_MNMYRF') ;
   WRENEW_SOYSUP_TOT_Q(M10,NDRGN1,'2013_MNMYRF')     = WRENEW_SOYSUP_TOT_Q(M10,NDRGN1,'2014_MNMYRF') ;

   WRENEW_SOYSUP_P(M10,NDRGN1,'2011_MNMYRF')         = WRENEW_SOYSUP_P(M10,NDRGN1,'2014_MNMYRF') ;
   WRENEW_SOYSUP_P(M10,NDRGN1,'2012_MNMYRF')         = WRENEW_SOYSUP_P(M10,NDRGN1,'2014_MNMYRF') ;
   WRENEW_SOYSUP_P(M10,NDRGN1,'2013_MNMYRF')         = WRENEW_SOYSUP_P(M10,NDRGN1,'2014_MNMYRF') ;


*********************************************************************
*** The following section does data manipulation on the data imported from NEMS
*      Condense Biomass supply curves, prices, and other module demand into one table each
*      MNUMFS translations UM-1 FR-2 AG-3 EC-4

Set
MKT                        Biomass Markets               / PW, RS, CM, IN, H2, ET, BT /
Other_MKT(MKT)             Non-LFMM Biomass Markets      / PW, RS, CM, IN, H2 /
;

Parameters
WRENEW_AVL_SUP(MNUMFS,MKT)                              Available Supply Types by Biomass Market
WRENEW_WDSUP_Q(NDREGN,MNUMFS,WDCRVS,MNMYRF)             combined WRENEW_WDSUP_Q* tables
WRENEW_WDSUP_P(NDREGN,MNUMFS,WDCRVS,MNMYRF)             combined WRENEW_WDSUP_P* tables
WRENEW_QBM_CL(MNUMFS,NDREGN,MNMYRF,Other_MKT)           combined WRENEW_QBM*CL tables
WDSUP_Inc(NDREGN,MNUMFS,WDCRVS,MNMYRF)                  incremental supply step
;

* Create WRENEW_AVL_SUP from NEMS Variables
WRENEW_AVL_SUP(MNUMFS,'BT') = WRENEW_MP_BM_BT(MNUMFS);
WRENEW_AVL_SUP(MNUMFS,'ET') = WRENEW_MP_BM_ET(MNUMFS);
WRENEW_AVL_SUP(MNUMFS,'PW') = WRENEW_MP_BM_PW(MNUMFS);
WRENEW_AVL_SUP(MNUMFS,'H2') = WRENEW_MP_BM_H2(MNUMFS);
WRENEW_AVL_SUP(MNUMFS,'RS') = WRENEW_MP_BM_RS(MNUMFS);
WRENEW_AVL_SUP(MNUMFS,'CM') = WRENEW_MP_BM_CM(MNUMFS);
WRENEW_AVL_SUP(MNUMFS,'IN') = WRENEW_MP_BM_IN(MNUMFS);

* Picking out the one RCLCLNR being used:
COALEMM_RCLIGNR(NDREGN,MNUMYR) = COALEMM_RCLCLNR(NDREGN,MNUMYR,'35_NUTSEC');
EMABLK_JCLIGNR(MNUMYR) = EMABLK_JCLCLNR(MNUMYR,'35_NUTSEC');

* Biomass Supply Quantities
WRENEW_WDSUP_Q(NDREGN,'1_MNUMFS',WDCRVS,MNMYRF) = WRENEW_WDSUP_Q_UM(WDCRVS,NDREGN,MNMYRF);            /* UWW */
WRENEW_WDSUP_Q(NDREGN,'2_MNUMFS',WDCRVS,MNMYRF) = WRENEW_WDSUP_Q_FR(WDCRVS,NDREGN,MNMYRF);            /* FR is Fed forestry residue FRR */
WRENEW_WDSUP_Q(NDREGN,'3_MNUMFS',WDCRVS,MNMYRF) = WRENEW_WDSUP_Q_AG(WDCRVS,NDREGN,MNMYRF);            /* AGR includes energy crops */
WRENEW_WDSUP_Q(NDREGN,'4_MNUMFS',WDCRVS,MNMYRF) = WRENEW_WDSUP_Q_EC(WDCRVS,NDREGN,MNMYRF);            /* EC is actually NFR */

* NEMS data coming in as a cummulative amount, needs to be converted to an incremental amount (billion BTUs)
WDSUP_Inc(NDREGN,MNUMFS,WDCRVS,MNMYRF) = max(0,WRENEW_WDSUP_Q(NDREGN,MNUMFS,WDCRVS,MNMYRF) - WRENEW_WDSUP_Q(NDREGN,MNUMFS,WDCRVS-1,MNMYRF));

* Biomass Supply Prices
WRENEW_WDSUP_P(NDREGN,'1_MNUMFS',WDCRVS,MNMYRF) = WRENEW_WDSUP_P_UM(WDCRVS,NDREGN,MNMYRF);            /* UWW */
WRENEW_WDSUP_P(NDREGN,'2_MNUMFS',WDCRVS,MNMYRF) = WRENEW_WDSUP_P_FR(WDCRVS,NDREGN,MNMYRF);            /* FR is Fed forestry residue FRR */
WRENEW_WDSUP_P(NDREGN,'3_MNUMFS',WDCRVS,MNMYRF) = WRENEW_WDSUP_P_AG(WDCRVS,NDREGN,MNMYRF);            /* AGR includes energy crops */
WRENEW_WDSUP_P(NDREGN,'4_MNUMFS',WDCRVS,MNMYRF) = WRENEW_WDSUP_P_EC(WDCRVS,NDREGN,MNMYRF);            /* EC is actually NFR */

* NEMS data coming in as a cummulative amount, needs to be converted to an incremental amount (billion BTUs)
WDSUP_Inc(NDREGN,MNUMFS,WDCRVS,MNMYRF) = max(0,WRENEW_WDSUP_Q(NDREGN,MNUMFS,WDCRVS,MNMYRF) - WRENEW_WDSUP_Q(NDREGN,MNUMFS,WDCRVS-1,MNMYRF));

* Biomass Demand from other Modules
WRENEW_QBM_CL(MNUMFS,NDREGN,MNMYRF,'PW')  = sum((NDRGN1,MNMFS1),WRENEW_QBMPWCL(MNMFS1,NDRGN1,MNMYRF)
                                            $((ord(NDREGN)=ord(NDRGN1)-1) and (ord(MNUMFS)=ord(MNMFS1)-1))) ;
WRENEW_QBM_CL(MNUMFS,NDREGN,MNMYRF,'RS')  = sum((NDRGN1,MNMFS1),WRENEW_QBMRSCL(MNMFS1,NDRGN1,MNMYRF)
                                            $((ord(NDREGN)=ord(NDRGN1)-1) and (ord(MNUMFS)=ord(MNMFS1)-1))) ;
WRENEW_QBM_CL(MNUMFS,NDREGN,MNMYRF,'IN')  = sum((NDRGN1,MNMFS1),WRENEW_QBMINCL(MNMFS1,NDRGN1,MNMYRF)
                                            $((ord(NDREGN)=ord(NDRGN1)-1) and (ord(MNUMFS)=ord(MNMFS1)-1))) ;
WRENEW_QBM_CL(MNUMFS,NDREGN,MNMYRF,'H2')  = sum((NDRGN1,MNMFS1),WRENEW_QBMH2CL(MNMFS1,NDRGN1,MNMYRF)
                                            $((ord(NDREGN)=ord(NDRGN1)-1) and (ord(MNUMFS)=ord(MNMFS1)-1))) ;
