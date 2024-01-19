'# ==========================================
SUBROUTINE io_regional
'# ==========================================


copy t300522\mc_rev*_1 io_regional\
copy t300522\mc_emp*_1 emp_regional\
copy t300522\eg*_1 emp_regional\
copy t300522\eea_1 emp_regional\
copy t300522\eeap_1 emp_regional\
copy t300522\emf_1 emp_regional\

pageselect io_regional


'Initialize national IO aggregates
'Sum of All Chemicals; NAICS 325 (Billions of Fixed 2012 Dollars)        
genr naics325 = 0
'Sum of All Petroleum; NAICS 324 (Billions of Fixed 2012 Dollars)
genr naics324 = 0
'Sum of All Stone, Clay, Glass and Cement; NAICS 327 (Billions of Fixed 2012 Dollars)
genr naics327 = 0
'Sum of All Primary Metals; NAICS 331 (Billions of Fixed 2012 Dollars)
genr naics331 = 0
'Total Manufacturing Output; NAICS 31-33 (Billions of Fixed 2012 Dollars)
genr naics31t33 = 0
'Total Industrial Output; NAICS 11, 21, 23, 31-33 (Billions of Fixed 2012 Dollars)
genr naics11a21a23a31t33 = 0
'Total Non-Industrial/Service Gross Output; NAICS 22, 42-92 (Billions of Fixed 2012 Dollars)
genr naics22a42t92 = 0
'Total Gross Output; NAICS 11-92 (Billions of Fixed 2012 Dollars)
genr naics11t92 = 0


'Compute regional IO 
for %r enc esc matl mtn neng pac satl wnc wsc

'Manufacturing
'        genr revind1_{%r}  = r3112r_{%r}+r3115r_{%r}+r3116a7r_{%r}+r311or_{%r}
'Food Products (Billions of Fixed 2012 Dollars)
        genr revind1_{%r}  = mc_revind2_1*{%r}_3112_outsh + mc_revind3_1*{%r}_3115_outsh _
                                       + mc_revind4_1*{%r}_3116a7_outsh + mc_revind5_1*{%r}_311o_outsh
'        genr revind2_{%r}  = r3112r_{%r}
'Grain and Oil Seed Milling (Billions of Fixed 2012 Dollars)
        genr revind2_{%r} = mc_revind2_1*{%r}_3112_outsh
'        genr revind3_{%r}  = r3115r_{%r}
'Dairy Products (Billions of Fixed 2012 Dollars)
        genr revind3_{%r} = mc_revind3_1*{%r}_3115_outsh
'        genr revind4_{%r}  = r3116a7r_{%r}
'Animal Slaughter and Seafood Products (Billions of Fixed 2012 Dollars)
        genr revind4_{%r} = mc_revind4_1*{%r}_3116a7_outsh
'        genr revind5_{%r}  = r311or_{%r}
'Other Food Products (Billions of Fixed 2012 Dollars)
        genr revind5_{%r} = mc_revind5_1*{%r}_311o_outsh
'        genr revind6_{%r}  = r312r_{%r}
'Beverage and Tobacco Products (Billions of Fixed 2012 Dollars)
        genr revind6_{%r} = mc_revind6_1*{%r}_312_outsh
'        genr revind7_{%r}  = r313t316r_{%r}
'Textiles, Apparel, and Leather (Billions of Fixed 2012 Dollars)
        genr revind7_{%r} = mc_revind7_1*{%r}_313t316_outsh
'        genr revind8_{%r}  = r321r_{%r}
'Wood Products (Billions of Fixed 2012 Dollars)
        genr revind8_{%r} = mc_revind8_1*{%r}_321_outsh
'        genr revind9_{%r}  = r337r_{%r}
'Furniture and Related Products (Billions of Fixed 2012 Dollars)
        genr revind9_{%r} = mc_revind9_1*{%r}_337_outsh
'        genr revind10_{%r} = r3221r_{%r}+r32221r_{%r}+r322or_{%r}
'Paper Products (Billions of Fixed 2012 Dollars)
        genr revind10_{%r} = mc_revind11_1*{%r}_3221_outsh + mc_revind12_1*{%r}_32221_outsh _
                           + mc_revind13_1*{%r}_322o_outsh
'        genr revind11_{%r} = r3221r_{%r}
'Pulp and Paper Mills (Billions of Fixed 2012 Dollars)
        genr revind11_{%r} = mc_revind11_1*{%r}_3221_outsh
'        genr revind12_{%r} = r32221r_{%r}
'Paperboard Containers (Billions of Fixed 2012 Dollars)
        genr revind12_{%r} = mc_revind12_1*{%r}_32221_outsh
'        genr revind13_{%r} = r322or_{%r}
'Other Paper Products (Billions of Fixed 2012 Dollars)
        genr revind13_{%r} = mc_revind13_1*{%r}_322o_outsh
'        genr revind14_{%r} = r323r_{%r}
'Printing (Billions of Fixed 2012 Dollars)
        genr revind14_{%r} = mc_revind14_1*{%r}_323_outsh
'        genr revind15_{%r} = r32512t8r_{%r}
'Basic Inorganic Chemicals (Billions of Fixed 2012 Dollars)
        genr revind15_{%r} = mc_revind15_1*{%r}_32512t8_outsh
'        genr revind16_{%r} = r32511a9or_{%r}+r325193r_{%r}
'        genr revind16_{%r} = mc_revind16_1*({%r}_32511a9o_outsh+{%r}_325193_outsh)
'Basic Organic Chemicals (Billions of Fixed 2012 Dollars)
        genr revind16_{%r} = mc_revind16_1*{%r}_32511a9_outsh
'        genr revind17_{%r} = r325193r_{%r}
'Ethanol (Billions of Fixed 2012 Dollars)
        genr revind17_{%r} = mc_revind17_1*{%r}_325193_outsh
'        genr revind18_{%r} = r3252r_{%r}
'Resins and Synthetics (Billions of Fixed 2012 Dollars)
        genr revind18_{%r} = mc_revind18_1*{%r}_3252_outsh
'        genr revind19_{%r} = r3253r_{%r}
'Agricultural Chemicals (Billions of Fixed 2012 Dollars)
        genr revind19_{%r} = mc_revind19_1*{%r}_3253_outsh
'        genr revind20_{%r} = r3254r_{%r}+r3255r_{%r}+r3256r_{%r}+r325or_{%r}
'Other Chemical Products Subtotal (Billions of Fixed 2012 Dollars)
        genr revind20_{%r} = mc_revind21_1*{%r}_3254_outsh + mc_revind22_1*{%r}_3255_outsh _
                           + mc_revind23_1*{%r}_3256_outsh + mc_revind24_1*{%r}_325o_outsh
'        genr revind21_{%r} = r3254r_{%r}
'Pharma Products (Billions of Fixed 2012 Dollars)
        genr revind21_{%r} = mc_revind21_1*{%r}_3254_outsh
'        genr revind22_{%r} = r3255r_{%r}
'Paint Products (Billions of Fixed 2012 Dollars)
        genr revind22_{%r} = mc_revind22_1*{%r}_3255_outsh
'        genr revind23_{%r} = r3256r_{%r}
'Soaps and Cleaning Products (Billions of Fixed 2012 Dollars)
        genr revind23_{%r} = mc_revind23_1*{%r}_3256_outsh
'        genr revind24_{%r} = r325or_{%r}
'Other Chemical Products (Billions of Fixed 2012 Dollars)
        genr revind24_{%r} = mc_revind24_1*{%r}_325o_outsh
'        genr revind25_{%r} = r32411r_{%r}
'Petroleum Refining (Billions of Fixed 2012 Dollars)
        genr revind25_{%r} = mc_revind25_1*{%r}_32411_outsh
'        genr revind26_{%r} = r324or_{%r}
'Other Petroleum and Coal Products (Billions of Fixed 2012 Dollars)
        genr revind26_{%r} = mc_revind26_1*{%r}_324o_outsh
'        genr revind27_{%r} = r326r_{%r}
'Plastics and Rubber Products (Billions of Fixed 2012 Dollars)
        genr revind27_{%r} = mc_revind27_1*{%r}_326_outsh
'x        genr revind28_{%r} = r3272or_{%r}+r327211r_{%r}
'        genr revind28_{%r} = mc_revind28_1*({%r}_3272o_outsh+{%r}_327211_outsh)
'Glass and Glass Products (Billions of Fixed 2012 Dollars)
        genr revind28_{%r} = mc_revind28_1*{%r}_3272_outsh
'        genr revind29_{%r} = r327211r_{%r}
'Flat Glass (Billions of Fixed 2012 Dollars)
        genr revind29_{%r} = mc_revind29_1*{%r}_327211_outsh
'        genr revind30_{%r} = r32731r_{%r}
'Cement Manufacturing (Billions of Fixed 2012 Dollars)
        genr revind30_{%r} = mc_revind30_1*{%r}_32731_outsh
'        genr revind31_{%r} = r3274r_{%r}
'Lime Manufacturing (Billions of Fixed 2012 Dollars)
        genr revind31_{%r} = mc_revind31_1*{%r}_3274_outsh
'x        genr revind32_{%r} = r327or_{%r}+r3273or_{%r}
'        genr revind32_{%r} = mc_revind32_1*({%r}_327o_outsh+{%r}_3273o_outsh)
'Other Nonmetallic Mineral Products (Billions of Fixed 2012 Dollars)
        genr revind32_{%r} = mc_revind32_1*{%r}_327oth_outsh
'        genr revind33_{%r} = r3311a2r_{%r}
'Iron and Steel Products (Billions of Fixed 2012 Dollars)
        genr revind33_{%r} = mc_revind33_1*{%r}_3311a2_outsh
'        genr revind34_{%r} = r3313r_{%r}
'Alumina and Aluminum Products (Billions of Fixed 2012 Dollars)
        genr revind34_{%r} = mc_revind34_1*{%r}_3313_outsh
'x        genr revind35_{%r} = r33151r_{%r}+r3314a5x1r_{%r}
'        genr revind35_{%r} = mc_revind35_1*({%r}_3314a5x1_outsh+{%r}_33151_outsh)
'Other Primary Metals (Billions of Fixed 2012 Dollars)
        genr revind35_{%r} = mc_revind35_1*{%r}_3314a5_outsh
'        genr revind36_{%r} = r332r_{%r}
'Fabricated Metal Products (Billions of Fixed 2012 Dollars)
        genr revind36_{%r} = mc_revind36_1*{%r}_332_outsh
'        genr revind37_{%r} = r333r_{%r}
'Machinery (Billions of Fixed 2012 Dollars)
        genr revind37_{%r} = mc_revind37_1*{%r}_333_outsh
'x        genr revind38_{%r} = r3341_{%r}+r33411a_{%r}+r334a5or_{%r}+r334511r_{%r}+r3345x11r_{%r}
'        genr revind38_{%r} = mc_revind38_1*({%r}_3341_outsh+{%r}_33411a_outsh+{%r}_334a5o_outsh _
'                           +{%r}_334511_outsh+{%r}_3345x11_outsh)
'Computers and Electronic Products (Billions of Fixed 2012 Dollars)
        genr revind38_{%r} = mc_revind38_1*{%r}_334_outsh
'        genr revind39_{%r} = r336r_{%r}
'Transportation Equipment (Billions of Fixed 2012 Dollars)
        genr revind39_{%r} = mc_revind39_1*{%r}_336_outsh
'        genr revind40_{%r} = r335r_{%r}
'Appliance and Electrical Equipment (Billions of Fixed 2012 Dollars)
        genr revind40_{%r} = mc_revind40_1*{%r}_335_outsh
'        genr revind41_{%r} = r339r_{%r}
'Miscellaneous Manufacturing (Billions of Fixed 2012 Dollars)
        genr revind41_{%r} = mc_revind41_1*{%r}_339_outsh

'Nonmanufacturing
'        genr revind42_{%r} = r111r_{%r}
'Crop Production (Billions of Fixed 2012 Dollars)
        genr revind42_{%r} = mc_revind42_1*{%r}_111_outsh
'        genr revind43_{%r} = r112r_{%r}
'Animal Production (Billions of Fixed 2012 Dollars)
        genr revind43_{%r} = mc_revind43_1*{%r}_112_outsh
'        genr revind44_{%r} = r113r_{%r}+r11or_{%r}
'        genr revind44_{%r} = mc_revind44_1*({%r}_113_outsh+{%r}_11o_outsh)
'Other Agriculture (Billions of Fixed 2012 Dollars)
        genr revind44_{%r} = mc_revind44_1*{%r}_113ao_outsh
'        genr revind45_{%r} = r2121r_{%r}
'Coal Mining (Billions of Fixed 2012 Dollars)
        genr revind45_{%r} = mc_revind45_1*{%r}_2121_outsh
'        genr revind46_{%r} = r211r_{%r}+r213r_{%r}
'        genr revind46_{%r} = mc_revind46_1*({%r}_211_outsh+{%r}_213_outsh)
'Oil and Gas Extraction and Support Activities (Billions of Fixed 2012 Dollars)
        genr revind46_{%r} = mc_revind46_1*{%r}_211a3_outsh
'        genr revind47_{%r} = r2122r_{%r}+r2123r_{%r}
'        genr revind47_{%r} = mc_revind47_1*({%r}_2122_outsh+{%r}_2123_outsh)
'Other Mining and Quarrying (Billions of Fixed 2012 Dollars)
        genr revind47_{%r} = mc_revind47_1*{%r}_2122a3_outsh  
'        genr revind48_{%r} = r23r_{%r}
'Construction (Billions of Fixed 2012 Dollars)
        genr revind48_{%r} = mc_revind48_1*{%r}_23_outsh
        
'Services
'        genr revser1_{%r}  = r48a9r_{%r}
'Transportation and Warehousing (Billions of Fixed 2012 Dollars)
        genr revser1_{%r} = mc_revser1_1*{%r}_48a9_outsh
'x        genr revser2_{%r}  = r515r_{%r}+r517r_{%r}
'        genr revser2_{%r} = mc_revser2_1*({%r}_515_outsh+{%r}_517_outsh)
'Broadcasting and Telecommunications (Billions of Fixed 2012 Dollars)
        genr revser2_{%r} = mc_revser2_1*{%r}_515a7_outsh
'        genr revser3_{%r}  = r2211r_{%r}
'Electric Power Generation and Distribution (Billions of Fixed 2012 Dollars)
        genr revser3_{%r} = mc_revser3_1*{%r}_2211_outsh
'        genr revser4_{%r}  = r2212r_{%r}
'Natural Gas Distribution (Billions of Fixed 2012 Dollars)
        genr revser4_{%r} = mc_revser4_1*{%r}_2212_outsh
'        genr revser5_{%r}  = r2213r_{%r}
'Water, Sewage, and Related System (Billions of Fixed 2012 Dollars)
        genr revser5_{%r} = mc_revser5_1*{%r}_2213_outsh
'        genr revser6_{%r}  = r42r_{%r}
'Wholesale Trade (Billions of Fixed 2012 Dollars)
        genr revser6_{%r} = mc_revser6_1*{%r}_42_outsh
'        genr revser7_{%r}  = r44a5r_{%r}
'Retail Trade (Billions of Fixed 2012 Dollars)
        genr revser7_{%r} = mc_revser7_1*{%r}_44a5_outsh
'        genr revser8_{%r}  = r52r_{%r}+r53r_{%r}
'        genr revser8_{%r} = mc_revser8_1*({%r}_52_outsh+{%r}_53_outsh)
'Finance, Insurance, and Real Estate (Billions of Fixed 2012 Dollars)
        genr revser8_{%r} = mc_revser8_1*{%r}_52a3_outsh
'        genr revser9_{%r}  = r5111r_{%r}+rSERVr_{%r}
'        genr revser9_{%r} = mc_revser9_1*({%r}_5111_outsh+{%r}_serv_outsh)
'Other Services (Billions of Fixed 2012 Dollars)
        genr revser9_{%r} = mc_revser9_1*{%r}_5111aserv_outsh
'x        genr revser10_{%r} = r921r_{%r}+r922a3r_{%r}
'        genr revser10_{%r} = mc_revser10_1*({%r}_921_outsh+{%r}_922a3_outsh)
'Public Administration (Billions of Fixed 2012 Dollars)
        genr revser10_{%r} = mc_revser10_1*{%r}_921a922a3_outsh
        
'Sum of All Chemicals; NAICS 325 (Billions of Fixed 2012 Dollars)        
        genr naics325_{%r} = revind15_{%r}+revind16_{%r}+revind18_{%r} _
                           + revind19_{%r}+revind20_{%r}
        naics325 = naics325+naics325_{%r}
'Sum of All Petroleum; NAICS 324 (Billions of Fixed 2012 Dollars)
        genr naics324_{%r} = revind25_{%r}+revind26_{%r}
        naics324 = naics324+naics324_{%r}
'Sum of All Stone, Clay, Glass and Cement; NAICS 327 (Billions of Fixed 2012 Dollars)
        genr naics327_{%r} = revind28_{%r}+revind30_{%r}+revind31_{%r}+revind32_{%r}
        naics327 = naics327+naics327_{%r}
'Sum of All Primary Metals; NAICS 331 (Billions of Fixed 2012 Dollars)
        genr naics331_{%r} = revind33_{%r}+revind34_{%r}+revind35_{%r}
        naics331 = naics331+naics331_{%r}
'Total Manufacturing Output; NAICS 31-33 (Billions of Fixed 2012 Dollars)
        genr naics31t33_{%r} = revind1_{%r} _
                             +revind6_{%r}+revind7_{%r}+revind8_{%r}+revind9_{%r}+revind10_{%r} _
                             +revind14_{%r}+revind15_{%r}+revind16_{%r} _
                             +revind18_{%r}+revind19_{%r}+revind20_{%r} _
                             +revind25_{%r}+revind26_{%r}+revind27_{%r}+revind28_{%r} _
                             +revind30_{%r}+revind31_{%r}+revind32_{%r}+revind33_{%r}+revind34_{%r} _
                             +revind35_{%r}+revind36_{%r}+revind37_{%r}+revind38_{%r}+revind39_{%r} _
                             +revind40_{%r}+revind41_{%r}
        naics31t33 = naics31t33+naics31t33_{%r}
'Total Industrial Output; NAICS 11, 21, 23, 31-33 (Billions of Fixed 2012 Dollars)
        genr naics11a21a23a31t33_{%r} = revind42_{%r}+revind43_{%r}+revind44_{%r}+revind45_{%r} _
                                      + revind46_{%r}+revind47_{%r}+revind48_{%r} _
                                      + naics31t33_{%r}
        naics11a21a23a31t33 = naics11a21a23a31t33+naics11a21a23a31t33_{%r}
'Total Non-Industrial/Service Gross Output; NAICS 22, 42-92 (Billions of Fixed 2012 Dollars)
        genr naics22a42t92_{%r} = revser1_{%r}+revser2_{%r}+revser3_{%r}+revser4_{%r}+revser5_{%r} _
                                + revser6_{%r}+revser7_{%r}+revser8_{%r}+revser9_{%r}+revser10_{%r}
        naics22a42t92 = naics22a42t92+naics22a42t92_{%r}
'Total Gross Output; NAICS 11-92 (Billions of Fixed 2012 Dollars)
        genr naics11t92_{%r} = naics11a21a23a31t33_{%r}+naics22a42t92_{%r}
        naics11t92 = naics11t92+naics11t92_{%r}

next

'Create group gregio containing US Regional Industrial Shipments projection.
group gregio
for %r neng matl enc wnc satl esc wsc mtn pac
   for !x=1 to 48
      gregio.add revind{!x}_{%r}
   next
next

store(d={%wfpathpwd}eviewsdb) {%wfpathpwd}eviewsdb::gregio


'# ==========================================
ENDSUB
'# ==========================================


'# ==========================================
SUBROUTINE emp_regional
'# ==========================================

pageselect emp_regional


'initialize national aggregates
'manufacturing
genr naics31t33 = 0

'nonmanufacturing
genr naics11a21a23 = 0

'services
genr naics22a42t92 = 0

'nonagricultural
genr naics21t23a31t33a42t92 = 0


'Compute regional EMP 
for %r enc esc matl mtn neng pac satl wnc wsc
'manufacturing
        genr empind1_{%r}= mc_empind1_1*{%r}_311_empsh
        genr empind2_{%r}= mc_empind2_1*{%r}_312_empsh
        genr empind3_{%r}= mc_empind3_1*{%r}_313t316_empsh
        genr empind4_{%r}= mc_empind4_1*{%r}_321_empsh
        genr empind5_{%r}= mc_empind5_1*{%r}_337_empsh
        genr empind6_{%r}= mc_empind6_1*{%r}_322_empsh
        genr empind7_{%r}= mc_empind7_1*{%r}_323_empsh
        genr empind8_{%r}= mc_empind8_1*{%r}_3251t3_empsh
        genr empind9_{%r}= mc_empind9_1*{%r}_3254t9_empsh
        genr empind10_{%r}= mc_empind10_1*{%r}_324_empsh
        genr empind11_{%r}= mc_empind11_1*{%r}_326_empsh
        genr empind12_{%r}= mc_empind12_1*{%r}_327_empsh
        genr empind13_{%r}= mc_empind13_1*{%r}_331_empsh
        genr empind14_{%r}= mc_empind14_1*{%r}_332_empsh
        genr empind15_{%r}= mc_empind15_1*{%r}_333_empsh
        genr empind16_{%r}= mc_empind16_1*{%r}_334_empsh
        genr empind17_{%r}= mc_empind17_1*{%r}_336_empsh
        genr empind18_{%r}= mc_empind18_1*{%r}_335_empsh
        genr empind19_{%r}= mc_empind19_1*{%r}_339_empsh
'nonmanufacturing
        genr empind20_{%r}= mc_empind20_1*{%r}_111_empsh
        genr empind21_{%r}= mc_empind21_1*{%r}_11o_empsh
        genr empind22_{%r}= mc_empind22_1*{%r}_2121_empsh
'        genr empind23_{%r}= mc_empind23_1*({%r}_211_empsh+{%r}_213_empsh)
        genr empind23_{%r}= mc_empind23_1*{%r}_211a3_empsh
        genr empind24_{%r}= mc_empind24_1*{%r}_2122a3_empsh
        genr empind25_{%r}= mc_empind25_1*{%r}_236_empsh
        genr empind26_{%r}= mc_empind26_1*{%r}_237_empsh
        genr empind27_{%r}= mc_empind27_1*{%r}_238_empsh
'services
        genr empser1_{%r}= mc_empser1_1*{%r}_2211_empsh
        genr empser2_{%r}= mc_empser2_1*{%r}_2212_empsh
        genr empser3_{%r}= mc_empser3_1*{%r}_2213_empsh
        genr empser4_{%r}= mc_empser4_1*{%r}_42_empsh
        genr empser5_{%r}= mc_empser5_1*{%r}_44a5_empsh
        genr empser6_{%r}= mc_empser6_1*{%r}_48a9_empsh
        genr empser7_{%r}= mc_empser7_1*{%r}_511_empsh
        genr empser8_{%r}= mc_empser8_1*{%r}_515_empsh
        genr empser9_{%r}= mc_empser9_1*{%r}_517_empsh
        genr empser10_{%r}= mc_empser10_1*{%r}_52_empsh
        genr empser11_{%r}= mc_empser11_1*{%r}_53_empsh
        genr empser12_{%r}= mc_empser12_1*{%r}_serv_empsh


'manufacturing
        genr naics31t33_{%r} = 0
        for !i = 1 to 19
           naics31t33_{%r} = naics31t33_{%r}+empind{!i}_{%r}
           naics31t33 = naics31t33+empind{!i}_{%r}
        next

'nonmanufacturing
        genr naics11a21a23_{%r} = 0
        for !i = 20 to 27
           naics11a21a23_{%r} = naics11a21a23_{%r}+empind{!i}_{%r}
           naics11a21a23 = naics11a21a23+empind{!i}_{%r}
        next

'services
        genr naics22a42t92_{%r} = 0
        for !i = 1 to 12
           naics22a42t92_{%r} = naics22a42t92_{%r}+empser{!i}_{%r}
           naics22a42t92 = naics22a42t92+empser{!i}_{%r}
        next

'nonagricultural
        genr naics21t23a31t33a42t92_{%r} = naics31t33_{%r}+naics11a21a23_{%r}+naics22a42t92_{%r}-empind20_{%r}-empind21_{%r}
        naics21t23a31t33a42t92 = naics21t23a31t33a42t92+naics21t23a31t33a42t92_{%r}


next


group gregemp
for %r neng matl enc wnc satl esc wsc mtn pac
   for !x=1 to 27
      gregemp.add empind{!x}_{%r}
   next
   for !x=1 to 12
      gregemp.add empser{!x}_{%r}
   next
next

store(d={%wfpathpwd}eviewsdb) {%wfpathpwd}eviewsdb::gregemp



'# ==========================================
ENDSUB
'# ==========================================


'# ==========================================
SUBROUTINE GIXTABS
'# ==========================================


' giswitch is nolonger used and is not active!

'gi baseline
if giswitch = 0 then
  %giscen = "_0"

endif

'gi pessimistic scenario
if giswitch = 1 then
  %giscen = "_pes"

endif

'gi optimistic scenario
if giswitch = 2 then
  %giscen = "_opt"

endif

'gi cyclical scenario
if giswitch = 3 then
  %giscen = "_cyc"

endif

'name of gi workfile page
%name = @pagename
'sample of gi workfile page
%xtabsmpl  = @pagesmpl
'create a new annual page for gi forecast
pagecreate(page=xtab_page) a 1990 2050
'return to gi workfile page
pageselect {%name}

'create a group holding gi forecast
group xtab_group bopcrntac     cd          cdfhe        cdfher        cdmv         cdmvr       cdo         _
cdor             cdr           cdrec       cdrecr       cgoods        cgoodsr      ckf         ckfadjcorp  _
ckfcorp          ckfcorpbk     cn          cncs         cncsr         cnefao       cnefaor     cnegao      _
cnegaor          cnf           cnfr        cnoo         cnoor         cnopmp       cnopmpr     cnotob      _
cnotobr          cnr           cons        consr        cpi           cpie         cpixfae     csv         _
csvac            csvacr        csvf        csvfin       csvfinr       csvfr        csvh        csvhc       _
csvhcr           csvhh         csvhhr      csvhr        csvins        csvinsr      csvnpish    csvnpishr   _
csvo             csvor         csvr        csvrec       csvrecr       csvts        csvtsr      csvu        _
csvur            eea           emf         g            gdp           gdpfer       gdpr        gf          _
gfml             gfmlr         gfo         gfor         gfr           gnp          gr          gsl         _
gslr             huesold       husps       i            ifnre         ifnree       ifnreeind   ifnreeindr  _
ifnreeip         ifnreeipccr   ifnreeipctr ifnreeipr    ifnreeo       ifnreeor     ifnreer     ifnreetac   _
ifnreetacr       ifnreetlv     ifnreetlvr  ifnreeto     ifnreetor     ifnreetr     ifnrer      ifnres      _
ifnresc          ifnrescr      ifnresmfg   ifnresmfgr   ifnresmi      ifnresmir    ifnreso     ifnresor    _
ifnresp          ifnrespc      ifnrespcr   ifnrespp     ifnresppr     ifnrespr     ifnresr     ifre        _
ifree            ifreer        ifrer       ifres        ifresr        ii           iicmiu      iicmiur     _
iif              iifr          iim         iimr         iinf          iinfr        iir         iirt        _
iirtr            iiw           iiwr        intnetbus    ipsb50001     ir           ivacorp     jcsmich     _
jeciwssp         jexchmtp      jpgdp       jqpcmhnf     m             mfy          mg          mgr         _
mr               msvtot        msvtotr     netcfiva     netsavgfunify nlfc         nnp         poilwti     _
rmcorppuaa       rmff          rmmtg30con  rmtb3m       rmtcm10y      ruc          savprate    sfdprodr    _
sp500            sublsurpgf    sublsurpgsl suva         suvtl         trfbus       txcorp      txcorpgf    _
txcorpgsl        txim          utlb00004   wpi          wpi05         wpi10        wpiind_05   wpisop3000  _
x                xfy           xg          xgr          xr            xsvtot       xsvtotr     yn          _
yp               ypcomp        ypd         ypdr         yppropadjf    yppropadjnf  yprentadj   za          _
zadiv            zar           zare        zb           zbecon        zbivarw

'copy group members from gi workfile page to the new annual page dropping the extension
for !i = 1 to xtab_group.@count
   %xtab_name = xtab_group.@seriesname(!i)
   copy {%xtab_name}{%giscen} xtab_page\{%xtab_name}
next

'copy remaining series to the new annual page
for %xtab_name netcaptrf stat txcorprw
   copy {%xtab_name} xtab_page\{%xtab_name}
next

'select the new annual page
pageselect xtab_page

'group all series on the new annual page
group xtab_group bopcrntac     cd          cdfhe        cdfher        cdmv         cdmvr       cdo         _
cdor             cdr           cdrec       cdrecr       cgoods        cgoodsr      ckf         ckfadjcorp  _
ckfcorp          ckfcorpbk     cn          cncs         cncsr         cnefao       cnefaor     cnegao      _
cnegaor          cnf           cnfr        cnoo         cnoor         cnopmp       cnopmpr     cnotob      _
cnotobr          cnr           cons        consr        cpi           cpie         cpixfae     csv         _
csvac            csvacr        csvf        csvfin       csvfinr       csvfr        csvh        csvhc       _
csvhcr           csvhh         csvhhr      csvhr        csvins        csvinsr      csvnpish    csvnpishr   _
csvo             csvor         csvr        csvrec       csvrecr       csvts        csvtsr      csvu        _
csvur            eea           emf         g            gdp           gdpfer       gdpr        gf          _
gfml             gfmlr         gfo         gfor         gfr           gnp          gr          gsl         _
gslr             huesold       husps       i            ifnre         ifnree       ifnreeind   ifnreeindr  _
ifnreeip         ifnreeipccr   ifnreeipctr ifnreeipr    ifnreeo       ifnreeor     ifnreer     ifnreetac   _
ifnreetacr       ifnreetlv     ifnreetlvr  ifnreeto     ifnreetor     ifnreetr     ifnrer      ifnres      _
ifnresc          ifnrescr      ifnresmfg   ifnresmfgr   ifnresmi      ifnresmir    ifnreso     ifnresor    _
ifnresp          ifnrespc      ifnrespcr   ifnrespp     ifnresppr     ifnrespr     ifnresr     ifre        _
ifree            ifreer        ifrer       ifres        ifresr        ii           iicmiu      iicmiur     _
iif              iifr          iim         iimr         iinf          iinfr        iir         iirt        _
iirtr            iiw           iiwr        intnetbus    ipsb50001     ir           ivacorp     jcsmich     _
jeciwssp         jexchmtp      jpgdp       jqpcmhnf     m             mfy          mg          mgr         _
mr               msvtot        msvtotr     netcfiva     netsavgfunify nlfc         nnp         poilwti     _
rmcorppuaa       rmff          rmmtg30con  rmtb3m       rmtcm10y      ruc          savprate    sfdprodr    _
sp500            sublsurpgf    sublsurpgsl suva         suvtl         trfbus       txcorp      txcorpgf    _
txcorpgsl        txim          utlb00004   wpi          wpi05         wpi10        wpiind_05   wpisop3000  _
x                xfy           xg          xgr          xr            xsvtot       xsvtotr     yn          _
yp               ypcomp        ypd         ypdr         yppropadjf    yppropadjnf  yprentadj   za          _
zadiv            zar           zare        zb           zbecon        zbivarw


'# ==========================================
ENDSUB
'# ==========================================


'# ==========================================
SUBROUTINE TRAN
'# ==========================================


'**********************************************************************
'
' Model of Vehicle Classes
'
'**********************************************************************


'comments
TranC1.label(d) Unit Sales of Class 1 Light Trucks, 0 to 6000 lbs.
TranC1.label(s) Wards Communication.
TranC1.label(u) Thousands of Vehicles.
TranC2.label(d) Unit Sales of Class 2 Light Trucks, 6001 to 10,000 lbs.
TranC2.label(s) Wards Communication.
TranC2.label(u) Thousands of Vehicles.
TranC3.label(d) Unit Sales of Class 3 Light Trucks, 10,000 to 14,000 lbs.
TranC3.label(s) Wards Communication.
TranC3.label(u) Thousands of Vehicles.
TranLt.label(d) Unit Sales of Classes 1, 2 and 3 Light Trucks, 0 to 14,000 lbs.
TranLt.label(s) Sum of original annual data for history and suvtl for forecast.
TranLt.label(u) Thousands of Vehicles.


'import high/low factor for light trucks
smpl @all
genr TranC1x  = TranC1
genr TranC2x  = TranC2
genr TranC3x  = TranC3
genr TranLtx  = TranC1x+TranC2x+TranC3x

'specify and estimate equations
smpl 1967q1 2020q4
equation TranC1eqn.ls (TranC1X/suvtl) C  (((cdmvn+ifnreetlvadj-ifmvpuna)/plvavg) + (.001*suvgov/suvtl))
equation TranC2eqn.ls (TranC2x/suvtl) C D(((cdmvn+ifnreetlvadj-ifmvpuna)/plvavg) + (.001*suvgov/suvtl))
equation TranC3eqn.ls (TranC3x/suvtl) C D(((cdmvn+ifnreetlvadj-ifmvpuna)/plvavg) + (.001*suvgov/suvtl)) time

smpl @all

'add TranClass model equations to m_us2021a model
m_us2021a.append :TranC1eqn
m_us2021a.append :TranC2eqn
m_us2021a.append :TranC3eqn
m_us2021a.append @identity TranLtx = TranC1x+TranC2x+TranC3x
m_us2021a.append @identity TranC1  = suvtl*TranC1x/TranLtx
m_us2021a.append @identity TranC2  = suvtl*TranC2x/TranLtx
m_us2021a.append @identity TranC3  = suvtl*TranC3x/TranLtx
m_us2021a.append @identity TranLt  = TranC1+TranC2+TranC3


'**********************************************************************
' END OF SUBROUTINE TRAN
'**********************************************************************


'# ==========================================
ENDSUB
'# ==========================================


'# ==========================================
SUBROUTINE REGWPIPT1
'# ==========================================


'**********************************************************************
'
' Model of Regional Producer Price Index - Part 1
'
'**********************************************************************

'Producer price index - coal
 genr wpi051_neng = wpi051_0
 genr wpi051_matl = wpi051_0
 genr wpi051_enc  = wpi051_0
 genr wpi051_wnc  = wpi051_0
 genr wpi051_satl = wpi051_0
 genr wpi051_esc  = wpi051_0
 genr wpi051_wsc  = wpi051_0
 genr wpi051_mtn  = wpi051_0
 genr wpi051_pac  = wpi051_0
'Producer price index - gas fuels
 genr wpi053_neng = wpi053_0
 genr wpi053_matl = wpi053_0
 genr wpi053_enc  = wpi053_0
 genr wpi053_wnc  = wpi053_0
 genr wpi053_satl = wpi053_0
 genr wpi053_esc  = wpi053_0
 genr wpi053_wsc  = wpi053_0
 genr wpi053_mtn  = wpi053_0
 genr wpi053_pac  = wpi053_0
'Producer price index - electric power
 genr wpi054_neng = wpi054_0
 genr wpi054_matl = wpi054_0
 genr wpi054_enc  = wpi054_0
 genr wpi054_wnc  = wpi054_0
 genr wpi054_satl = wpi054_0
 genr wpi054_esc  = wpi054_0
 genr wpi054_wsc  = wpi054_0
 genr wpi054_mtn  = wpi054_0
 genr wpi054_pac  = wpi054_0
'Producer price index - utility natural gas
 genr wpi055_neng = wpi055_0
 genr wpi055_matl = wpi055_0
 genr wpi055_enc  = wpi055_0
 genr wpi055_wnc  = wpi055_0
 genr wpi055_satl = wpi055_0
 genr wpi055_esc  = wpi055_0
 genr wpi055_wsc  = wpi055_0
 genr wpi055_mtn  = wpi055_0
 genr wpi055_pac  = wpi055_0
'Producer price index - refined petroleum products
 genr wpi057_neng = wpi057_0
 genr wpi057_matl = wpi057_0
 genr wpi057_enc  = wpi057_0
 genr wpi057_wnc  = wpi057_0
 genr wpi057_satl = wpi057_0
 genr wpi057_esc  = wpi057_0
 genr wpi057_wsc  = wpi057_0
 genr wpi057_mtn  = wpi057_0
 genr wpi057_pac  = wpi057_0
'Producer price index - residual petroleum fuels
 genr wpi0574_neng = wpi0574_0
 genr wpi0574_matl = wpi0574_0
 genr wpi0574_enc  = wpi0574_0
 genr wpi0574_wnc  = wpi0574_0
 genr wpi0574_satl = wpi0574_0
 genr wpi0574_esc  = wpi0574_0
 genr wpi0574_wsc  = wpi0574_0
 genr wpi0574_mtn  = wpi0574_0
 genr wpi0574_pac  = wpi0574_0
'Producer price index - fuels, related products and power
 genr wpi05_neng = wpi05_0
 genr wpi05_matl = wpi05_0
 genr wpi05_enc  = wpi05_0
 genr wpi05_wnc  = wpi05_0
 genr wpi05_satl = wpi05_0
 genr wpi05_esc  = wpi05_0
 genr wpi05_wsc  = wpi05_0
 genr wpi05_mtn  = wpi05_0
 genr wpi05_pac  = wpi05_0


'**********************************************************************
' END OF SUBROUTINE REGWPIPT1
'**********************************************************************


'# ==========================================
ENDSUB
'# ==========================================


'# ==========================================
SUBROUTINE REGWPIPT2
'# ==========================================


'**********************************************************************
'
' Model of Regional Producer Price Index - Part 2
'
'**********************************************************************

'Producer price index - coal
 genr wpi051_neng = @mean(wpi051_neng,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi051_neng_r_0
 genr wpi051_matl = @mean(wpi051_matl,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi051_matl_r_0
 genr wpi051_enc  = @mean(wpi051_enc,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi051_enc_r_0
 genr wpi051_wnc  = @mean(wpi051_wnc,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi051_wnc_r_0
 genr wpi051_satl = @mean(wpi051_satl,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi051_satl_r_0
 genr wpi051_esc  = @mean(wpi051_esc,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi051_esc_r_0
 genr wpi051_wsc  = @mean(wpi051_wsc,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi051_wsc_r_0
 genr wpi051_mtn  = @mean(wpi051_mtn,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi051_mtn_r_0
 genr wpi051_pac  = @mean(wpi051_pac,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi051_pac_r_0
'Producer price index - gas fuels
 genr wpi053_neng = @mean(wpi053_neng,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi053_neng_r_0
 genr wpi053_matl = @mean(wpi053_matl,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi053_matl_r_0
 genr wpi053_enc  = @mean(wpi053_enc,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi053_enc_r_0
 genr wpi053_wnc  = @mean(wpi053_wnc,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi053_wnc_r_0
 genr wpi053_satl = @mean(wpi053_satl,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi053_satl_r_0
 genr wpi053_esc  = @mean(wpi053_esc,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi053_esc_r_0
 genr wpi053_wsc  = @mean(wpi053_wsc,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi053_wsc_r_0
 genr wpi053_mtn  = @mean(wpi053_mtn,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi053_mtn_r_0
 genr wpi053_pac  = @mean(wpi053_pac,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi053_pac_r_0
'Producer price index - electric power
 genr wpi054_neng = @mean(wpi054_neng,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi054_neng_r_0
 genr wpi054_matl = @mean(wpi054_matl,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi054_matl_r_0
 genr wpi054_enc  = @mean(wpi054_enc,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi054_enc_r_0
 genr wpi054_wnc  = @mean(wpi054_wnc,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi054_wnc_r_0
 genr wpi054_satl = @mean(wpi054_satl,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi054_satl_r_0
 genr wpi054_esc  = @mean(wpi054_esc,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi054_esc_r_0
 genr wpi054_wsc  = @mean(wpi054_wsc,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi054_wsc_r_0
 genr wpi054_mtn  = @mean(wpi054_mtn,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi054_mtn_r_0
 genr wpi054_pac  = @mean(wpi054_pac,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi054_pac_r_0
'Producer price index - utility natural gas
 genr wpi055_neng = @mean(wpi055_neng,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi055_neng_r_0
 genr wpi055_matl = @mean(wpi055_matl,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi055_matl_r_0
 genr wpi055_enc  = @mean(wpi055_enc,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi055_enc_r_0
 genr wpi055_wnc  = @mean(wpi055_wnc,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi055_wnc_r_0
 genr wpi055_satl = @mean(wpi055_satl,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi055_satl_r_0
 genr wpi055_esc  = @mean(wpi055_esc,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi055_esc_r_0
 genr wpi055_wsc  = @mean(wpi055_wsc,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi055_wsc_r_0
 genr wpi055_mtn  = @mean(wpi055_mtn,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi055_mtn_r_0
 genr wpi055_pac  = @mean(wpi055_pac,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi055_pac_r_0
'Producer price index - refined petroleum products
 genr wpi057_neng = @mean(wpi057_neng,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi057_neng_r_0
 genr wpi057_matl = @mean(wpi057_matl,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi057_matl_r_0
 genr wpi057_enc  = @mean(wpi057_enc,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi057_enc_r_0
 genr wpi057_wnc  = @mean(wpi057_wnc,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi057_wnc_r_0
 genr wpi057_satl = @mean(wpi057_satl,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi057_satl_r_0
 genr wpi057_esc  = @mean(wpi057_esc,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi057_esc_r_0
 genr wpi057_wsc  = @mean(wpi057_wsc,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi057_wsc_r_0
 genr wpi057_mtn  = @mean(wpi057_mtn,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi057_mtn_r_0
 genr wpi057_pac  = @mean(wpi057_pac,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi057_pac_r_0
'Producer price index - residual petroleum fuels
 genr wpi0574_neng = @mean(wpi0574_neng,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi0574_neng_r_0
 genr wpi0574_matl = @mean(wpi0574_matl,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi0574_matl_r_0
 genr wpi0574_enc  = @mean(wpi0574_enc,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi0574_enc_r_0
 genr wpi0574_wnc  = @mean(wpi0574_wnc,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi0574_wnc_r_0
 genr wpi0574_satl = @mean(wpi0574_satl,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi0574_satl_r_0
 genr wpi0574_esc  = @mean(wpi0574_esc,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi0574_esc_r_0
 genr wpi0574_wsc  = @mean(wpi0574_wsc,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi0574_wsc_r_0
 genr wpi0574_mtn  = @mean(wpi0574_mtn,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi0574_mtn_r_0
 genr wpi0574_pac  = @mean(wpi0574_pac,"2020:1 2020:4")*{%wfpathpwd}eviewsdb::wpi0574_pac_r_0

'Producer price index - fuels, related products and power
 m_us2021a.append @identity wpi05_neng = wpi05_neng(-1) _
                                       * ((0.0355*wpi051_neng+0.1042*wpi053_neng+0.3011*wpi054_neng _
                                          +0.1590*wpi055_neng+0.0950*wpi0561+0.3053*wpi057_neng)    _
                                       /  (0.0355*wpi051_neng(-1)+0.1042*wpi053_neng(-1)            _
                                          +0.3011*wpi054_neng(-1)+0.1590*wpi055_neng(-1)            _
                                          +0.0950*wpi0561(-1)+0.3053*wpi057_neng(-1)))
 m_us2021a.append @identity wpi05_matl = wpi05_matl(-1) _
                                       * ((0.0355*wpi051_matl+0.1042*wpi053_matl+0.3011*wpi054_matl _
                                          +0.1590*wpi055_matl+0.0950*wpi0561+0.3053*wpi057_matl)    _
                                       /  (0.0355*wpi051_matl(-1)+0.1042*wpi053_matl(-1)            _
                                          +0.3011*wpi054_matl(-1)+0.1590*wpi055_matl(-1)            _
                                          +0.0950*wpi0561(-1)+0.3053*wpi057_matl(-1)))
 m_us2021a.append @identity wpi05_enc  = wpi05_enc(-1) _
                                       * ((0.0355*wpi051_enc+0.1042*wpi053_enc+0.3011*wpi054_enc _
                                          +0.1590*wpi055_enc+0.0950*wpi0561+0.3053*wpi057_enc)   _
                                       /  (0.0355*wpi051_enc(-1)+0.1042*wpi053_enc(-1)           _
                                          +0.3011*wpi054_enc(-1)+0.1590*wpi055_enc(-1)           _
                                          +0.0950*wpi0561(-1)+0.3053*wpi057_enc(-1)))
 m_us2021a.append @identity wpi05_wnc  = wpi05_wnc(-1) _
                                       * ((0.0355*wpi051_wnc+0.1042*wpi053_wnc+0.3011*wpi054_wnc _
                                          +0.1590*wpi055_wnc+0.0950*wpi0561+0.3053*wpi057_wnc)   _
                                       /  (0.0355*wpi051_wnc(-1)+0.1042*wpi053_wnc(-1)           _
                                          +0.3011*wpi054_wnc(-1)+0.1590*wpi055_wnc(-1)           _
                                          +0.0950*wpi0561(-1)+0.3053*wpi057_wnc(-1)))
 m_us2021a.append @identity wpi05_satl = wpi05_satl(-1) _
                                       * ((0.0355*wpi051_satl+0.1042*wpi053_satl+0.3011*wpi054_satl _
                                          +0.1590*wpi055_satl+0.0950*wpi0561+0.3053*wpi057_satl)    _
                                       /  (0.0355*wpi051_satl(-1)+0.1042*wpi053_satl(-1)            _
                                          +0.3011*wpi054_satl(-1)+0.1590*wpi055_satl(-1)            _
                                          +0.0950*wpi0561(-1)+0.3053*wpi057_satl(-1)))
 m_us2021a.append @identity wpi05_esc  = wpi05_esc(-1) _
                                       * ((0.0355*wpi051_esc+0.1042*wpi053_esc+0.3011*wpi054_esc _
                                          +0.1590*wpi055_esc+0.0950*wpi0561+0.3053*wpi057_esc)   _
                                       /  (0.0355*wpi051_esc(-1)+0.1042*wpi053_esc(-1)           _
                                          +0.3011*wpi054_esc(-1)+0.1590*wpi055_esc(-1)           _
                                          +0.0950*wpi0561(-1)+0.3053*wpi057_esc(-1)))
 m_us2021a.append @identity wpi05_wsc  = wpi05_wsc(-1) _
                                       * ((0.0355*wpi051_wsc+0.1042*wpi053_wsc+0.3011*wpi054_wsc _
                                          +0.1590*wpi055_wsc+0.0950*wpi0561+0.3053*wpi057_wsc)   _
                                       /  (0.0355*wpi051_wsc(-1)+0.1042*wpi053_wsc(-1)           _
                                          +0.3011*wpi054_wsc(-1)+0.1590*wpi055_wsc(-1)           _
                                          +0.0950*wpi0561(-1)+0.3053*wpi057_wsc(-1)))
 m_us2021a.append @identity wpi05_mtn  = wpi05_mtn(-1) _
                                       * ((0.0355*wpi051_mtn+0.1042*wpi053_mtn+0.3011*wpi054_mtn _
                                          +0.1590*wpi055_mtn+0.0950*wpi0561+0.3053*wpi057_mtn)   _
                                       /  (0.0355*wpi051_mtn(-1)+0.1042*wpi053_mtn(-1)           _
                                          +0.3011*wpi054_mtn(-1)+0.1590*wpi055_mtn(-1)           _
                                          +0.0950*wpi0561(-1)+0.3053*wpi057_mtn(-1)))
 m_us2021a.append @identity wpi05_pac  = wpi05_pac(-1) _
                                       * ((0.0355*wpi051_pac+0.1042*wpi053_pac+0.3011*wpi054_pac _
                                          +0.1590*wpi055_pac+0.0950*wpi0561+0.3053*wpi057_pac)   _
                                       /  (0.0355*wpi051_pac(-1)+0.1042*wpi053_pac(-1)           _
                                          +0.3011*wpi054_pac(-1)+0.1590*wpi055_pac(-1)           _
                                          +0.0950*wpi0561(-1)+0.3053*wpi057_pac(-1)))


'**********************************************************************
' END OF SUBROUTINE REGWPIPT2
'**********************************************************************


'# ==========================================
ENDSUB
'# ==========================================


'# ==========================================
SUBROUTINE COMMFLR
'# ==========================================


'**********************************************************************
'
' Model of Commercial Floorspace Stocks
'
' This commercial floorspace model contains 306 equations of which 117
' (thirteen commercial floorspace types in each of nine Census
' Divisions) are estimated using historical data reaching back into
' the seventies.  Of the remaining 189 equations, 117 are stock
' identities that accumulate flows; 9 are sum of flows by region, 9 are
' sum of removals by region and 9 are sum of stocks by region. Of the
' remaining 45, 13 are national sums of flows by floorspace type,
' 13 are national sums of removals by floorspace type and 13 are national
' sums stocks by floorspace type. Of the remaining 6, 3 are national
' sums of regional flows, removals and stocks and national sums of
' floortype by flow, removal and stocks.
'
' The model forecasts flows of thirteen floorspace types in each of the
' nine Census Divisions.  The units of the forecast are thousand square
' feet of commercial floorspace.  The model frequency is quarterly which
' is higher than the frequency of NEMS.  NEMS is an annual model.
'
' Aug. 2006
'
'**********************************************************************


pageselect comfloor

smpl @all

' document the data objects
  ENC_AMUSE_DLBSD.label(d) Commercial Floorspace Stock: Amusement, East North Central Census Division.
  ESC_AMUSE_DLBSD.label(d) Commercial Floorspace Stock: Amusement, East South Central Census Division.
  MA_AMUSE_DLBSD.label(d) Commercial Floorspace Stock: Amusement, Mid-Atlantic Census Division.
  MTN_AMUSE_DLBSD.label(d) Commercial Floorspace Stock: Amusement, Mountain Census Division.
  NENG_AMUSE_DLBSD.label(d) Commercial Floorspace Stock: Amusement, New England Census Division.
  PAC_AMUSE_DLBSD.label(d) Commercial Floorspace Stock: Amusement, Pacific Census Division.
  SATL_AMUSE_DLBSD.label(d) Commercial Floorspace Stock: Amusement, South Atlantic Census Division.
  TOTAL_AMUSE_DLBSD.label(d) Commercial Floorspace Stock: Amusement, US.
  WNC_AMUSE_DLBSD.label(d) Commercial Floorspace Stock: Amusement, West North Central Census Division.
  WSC_AMUSE_DLBSD.label(d) Commercial Floorspace Stock: Amusement, West South Central Census Division.
  ENC_AUTO_DLBSD.label(d) Commercial Floorspace Stock: Automotive service and parking garages, East North Central Census Division.
  ESC_AUTO_DLBSD.label(d) Commercial Floorspace Stock: Automotive service and parking garages, East South Central Census Division.
  MA_AUTO_DLBSD.label(d) Commercial Floorspace Stock: Automotive service and parking garages, Mid-Atlantic Census Division.
  MTN_AUTO_DLBSD.label(d) Commercial Floorspace Stock: Automotive service and parking garages, Mountain Census Division.
  NENG_AUTO_DLBSD.label(d) Commercial Floorspace Stock: Automotive service and parking garages, New England Census Division.
  PAC_AUTO_DLBSD.label(d) Commercial Floorspace Stock: Automotive service and parking garages, Pacific Census Division.
  SATL_AUTO_DLBSD.label(d) Commercial Floorspace Stock: Automotive service and parking garages, South Atlantic Census Division.
  TOTAL_AUTO_DLBSD.label(d) Commercial Floorspace Stock: Automotive service and parking garages, US.
  WNC_AUTO_DLBSD.label(d) Commercial Floorspace Stock: Automotive service and parking garages, West North Central Census Division.
  WSC_AUTO_DLBSD.label(d) Commercial Floorspace Stock: Automotive service and parking garages, West South Central Census Division.
  ENC_DORM_DLBSD.label(d) Commercial Floorspace Stock: Educational and federally-owned (primarily military), East North Central Census Division.
  ESC_DORM_DLBSD.label(d) Commercial Floorspace Stock: Educational and federally-owned (primarily military), East South Central Census Division.
  MA_DORM_DLBSD.label(d) Commercial Floorspace Stock: Educational and federally-owned (primarily military), Mid-Atlantic Census Division.
  MTN_DORM_DLBSD.label(d) Commercial Floorspace Stock: Educational and federally-owned (primarily military), Mountain Census Division.
  NENG_DORM_DLBSD.label(d) Commercial Floorspace Stock: Educational and federally-owned (primarily military), New England Census Division.
  PAC_DORM_DLBSD.label(d) Commercial Floorspace Stock: Educational and federally-owned (primarily military), Pacific Census Division.
  SATL_DORM_DLBSD.label(d) Commercial Floorspace Stock: Educational and federally-owned (primarily military), South Atlantic Census Division.
  TOTAL_DORM_DLBSD.label(d) Commercial Floorspace Stock: Educational and federally-owned (primarily military), US.
  WNC_DORM_DLBSD.label(d) Commercial Floorspace Stock: Educational and federally-owned (primarily military), West North Central Census Division.
  WSC_DORM_DLBSD.label(d) Commercial Floorspace Stock: Educational and federally-owned (primarily military), West South Central Census Division.
  ENC_EDUC_DLBSD.label(d) Commercial Floorspace Stock: Primary, secondary and higher education, East North Central Census Division.
  ESC_EDUC_DLBSD.label(d) Commercial Floorspace Stock: Primary, secondary and higher education, East South Central Census Division.
  MA_EDUC_DLBSD.label(d) Commercial Floorspace Stock: Primary, secondary and higher education, Mid-Atlantic Census Division.
  MTN_EDUC_DLBSD.label(d) Commercial Floorspace Stock: Primary, secondary and higher education, Mountain Census Division.
  NENG_EDUC_DLBSD.label(d) Commercial Floorspace Stock: Primary, secondary and higher education, New England Census Division.
  PAC_EDUC_DLBSD.label(d) Commercial Floorspace Stock: Primary, secondary and higher education, Pacific Census Division.
  SATL_EDUC_DLBSD.label(d) Commercial Floorspace Stock: Primary, secondary and higher education, South Atlantic Census Division.
  TOTAL_EDUC_DLBSD.label(d) Commercial Floorspace Stock: Primary, secondary and higher education, US.
  WNC_EDUC_DLBSD.label(d) Commercial Floorspace Stock: Primary, secondary and higher education, West North Central Census Division.
  WSC_EDUC_DLBSD.label(d) Commercial Floorspace Stock: Primary, secondary and higher education, West South Central Census Division.
  ENC_HEALTH_DLBSD.label(d) Commercial Floorspace Stock: Hospitals and nursing homes, East North Central Census Division.
  ESC_HEALTH_DLBSD.label(d) Commercial Floorspace Stock: Hospitals and nursing homes, East South Central Census Division.
  MA_HEALTH_DLBSD.label(d) Commercial Floorspace Stock: Hospitals and nursing homes, Mid-Atlantic Census Division.
  MTN_HEALTH_DLBSD.label(d) Commercial Floorspace Stock: Hospitals and nursing homes, Mountain Census Division.
  NENG_HEALTH_DLBSD.label(d) Commercial Floorspace Stock: Hospitals and nursing homes, New England Census Division.
  PAC_HEALTH_DLBSD.label(d) Commercial Floorspace Stock: Hospitals and nursing homes, Pacific Census Division.
  SATL_HEALTH_DLBSD.label(d) Commercial Floorspace Stock: Hospitals and nursing homes, South Atlantic Census Division.
  TOTAL_HEALTH_DLBSD.label(d) Commercial Floorspace Stock: Hospitals and nursing homes, US.
  WNC_HEALTH_DLBSD.label(d) Commercial Floorspace Stock: Hospitals and nursing homes, West North Central Census Division.
  WSC_HEALTH_DLBSD.label(d) Commercial Floorspace Stock: Hospitals and nursing homes, West South Central Census Division.
  ENC_HOTEL_DLBSD.label(d) Commercial Floorspace Stock: Hotels and motels, East North Central Census Division.
  ESC_HOTEL_DLBSD.label(d) Commercial Floorspace Stock: Hotels and motels, East South Central Census Division.
  MA_HOTEL_DLBSD.label(d) Commercial Floorspace Stock: Hotels and motels, Mid-Atlantic Census Division.
  MTN_HOTEL_DLBSD.label(d) Commercial Floorspace Stock: Hotels and motels, Mountain Census Division.
  NENG_HOTEL_DLBSD.label(d) Commercial Floorspace Stock: Hotels and motels, New England Census Division.
  PAC_HOTEL_DLBSD.label(d) Commercial Floorspace Stock: Hotels and motels, Pacific Census Division.
  SATL_HOTEL_DLBSD.label(d) Commercial Floorspace Stock: Hotels and motels, South Atlantic Census Division.
  TOTAL_HOTEL_DLBSD.label(d) Commercial Floorspace Stock: Hotels and motels, US.
  WNC_HOTEL_DLBSD.label(d) Commercial Floorspace Stock: Hotels and motels, West North Central Census Division.
  WSC_HOTEL_DLBSD.label(d) Commercial Floorspace Stock: Hotels and motels, West South Central Census Division.
  ENC_MFG_DLBSD.label(d) Commercial Floorspace Stock: Manufacturing, East North Central Census Division.
  ESC_MFG_DLBSD.label(d) Commercial Floorspace Stock: Manufacturing, East South Central Census Division.
  MA_MFG_DLBSD.label(d) Commercial Floorspace Stock: Manufacturing, Mid-Atlantic Census Division.
  MTN_MFG_DLBSD.label(d) Commercial Floorspace Stock: Manufacturing, Mountain Census Division.
  NENG_MFG_DLBSD.label(d) Commercial Floorspace Stock: Manufacturing, New England Census Division.
  PAC_MFG_DLBSD.label(d) Commercial Floorspace Stock: Manufacturing, Pacific Census Division.
  SATL_MFG_DLBSD.label(d) Commercial Floorspace Stock: Manufacturing, South Atlantic Census Division.
  TOTAL_MFG_DLBSD.label(d) Commercial Floorspace Stock: Manufacturing, US.
  WNC_MFG_DLBSD.label(d) Commercial Floorspace Stock: Manufacturing, West North Central Census Division.
  WSC_MFG_DLBSD.label(d) Commercial Floorspace Stock: Manufacturing, West South Central Census Division.
  ENC_MISCNR_DLBSD.label(d) Commercial Floorspace Stock: Transportation related and all other nec, East North Central Census Division.
  ESC_MISCNR_DLBSD.label(d) Commercial Floorspace Stock: Transportation related and all other nec, East South Central Census Division.
  MA_MISCNR_DLBSD.label(d) Commercial Floorspace Stock: Transportation related and all other nec, Mid-Atlantic Census Division.
  MTN_MISCNR_DLBSD.label(d) Commercial Floorspace Stock: Transportation related and all other nec, Mountain Census Division.
  NENG_MISCNR_DLBSD.label(d) Commercial Floorspace Stock: Transportation related and all other nec, New England Census Division.
  PAC_MISCNR_DLBSD.label(d) Commercial Floorspace Stock: Transportation related and all other nec, Pacific Census Division.
  SATL_MISCNR_DLBSD.label(d) Commercial Floorspace Stock: Transportation related and all other nec, South Atlantic Census Division.
  TOTAL_MISCNR_DLBSD.label(d) Commercial Floorspace Stock: Transportation related and all other nec, US.
  WNC_MISCNR_DLBSD.label(d) Commercial Floorspace Stock: Transportation related and all other nec, West North Central Census Division.
  WSC_MISCNR_DLBSD.label(d) Commercial Floorspace Stock: Transportation related and all other nec, West South Central Census Division.
  ENC_OFFICE_DLBSD.label(d) Commercial Floorspace Stock: Private, federal and state and local offices, East North Central Census Division.
  ESC_OFFICE_DLBSD.label(d) Commercial Floorspace Stock: Private, federal and state and local offices, East South Central Census Division.
  MA_OFFICE_DLBSD.label(d) Commercial Floorspace Stock: Private, federal and state and local offices, Mid-Atlantic Census Division.
  MTN_OFFICE_DLBSD.label(d) Commercial Floorspace Stock: Private, federal and state and local offices, Mountain Census Division.
  NENG_OFFICE_DLBSD.label(d) Commercial Floorspace Stock: Private, federal and state and local offices, New England Census Division.
  PAC_OFFICE_DLBSD.label(d) Commercial Floorspace Stock: Private, federal and state and local offices, Pacific Census Division.
  SATL_OFFICE_DLBSD.label(d) Commercial Floorspace Stock: Private, federal and state and local offices, South Atlantic Census Division.
  TOTAL_OFFICE_DLBSD.label(d) Commercial Floorspace Stock: Private, federal and state and local offices, US.
  WNC_OFFICE_DLBSD.label(d) Commercial Floorspace Stock: Private, federal and state and local offices, West North Central Census Division.
  WSC_OFFICE_DLBSD.label(d) Commercial Floorspace Stock: Private, federal and state and local offices, West South Central Census Division.
  ENC_PUB_DLBSD.label(d) Commercial Floorspace Stock: Federal and state and local, East North Central Census Division.
  ESC_PUB_DLBSD.label(d) Commercial Floorspace Stock: Federal and state and local, East South Central Census Division.
  MA_PUB_DLBSD.label(d) Commercial Floorspace Stock: Federal and state and local, Mid-Atlantic Census Division.
  MTN_PUB_DLBSD.label(d) Commercial Floorspace Stock: Federal and state and local, Mountain Census Division.
  NENG_PUB_DLBSD.label(d) Commercial Floorspace Stock: Federal and state and local, New England Census Division.
  PAC_PUB_DLBSD.label(d) Commercial Floorspace Stock: Federal and state and local, Pacific Census Division.
  SATL_PUB_DLBSD.label(d) Commercial Floorspace Stock: Federal and state and local, South Atlantic Census Division.
  TOTAL_PUB_DLBSD.label(d) Commercial Floorspace Stock: Federal and state and local, US.
  WNC_PUB_DLBSD.label(d) Commercial Floorspace Stock: Federal and state and local, West North Central Census Division.
  WSC_PUB_DLBSD.label(d) Commercial Floorspace Stock: Federal and state and local, West South Central Census Division.
  ENC_REL_DLBSD.label(d) Commercial Floorspace Stock: Religious, East North Central Census Division.
  ESC_REL_DLBSD.label(d) Commercial Floorspace Stock: Religious, East South Central Census Division.
  MA_REL_DLBSD.label(d) Commercial Floorspace Stock: Religious, Mid-Atlantic Census Division.
  MTN_REL_DLBSD.label(d) Commercial Floorspace Stock: Religious, Mountain Census Division.
  NENG_REL_DLBSD.label(d) Commercial Floorspace Stock: Religious, New England Census Division.
  PAC_REL_DLBSD.label(d) Commercial Floorspace Stock: Religious, Pacific Census Division.
  SATL_REL_DLBSD.label(d) Commercial Floorspace Stock: Religious, South Atlantic Census Division.
  TOTAL_REL_DLBSD.label(d) Commercial Floorspace Stock: Religious, US.
  WNC_REL_DLBSD.label(d) Commercial Floorspace Stock: Religious, West North Central Census Division.
  WSC_REL_DLBSD.label(d) Commercial Floorspace Stock: Religious, West South Central Census Division.
  ENC_STORES_DLBSD.label(d) Commercial Floorspace Stock: Stores and restaurants, East North Central Census Division.
  ESC_STORES_DLBSD.label(d) Commercial Floorspace Stock: Stores and restaurants, East South Central Census Division.
  MA_STORES_DLBSD.label(d) Commercial Floorspace Stock: Stores and restaurants, Mid-Atlantic Census Division.
  MTN_STORES_DLBSD.label(d) Commercial Floorspace Stock: Stores and restaurants, Mountain Census Division.
  NENG_STORES_DLBSD.label(d) Commercial Floorspace Stock: Stores and restaurants, New England Census Division.
  PAC_STORES_DLBSD.label(d) Commercial Floorspace Stock: Stores and restaurants, Pacific Census Division.
  SATL_STORES_DLBSD.label(d) Commercial Floorspace Stock: Stores and restaurants, South Atlantic Census Division.
  TOTAL_STORES_DLBSD.label(d) Commercial Floorspace Stock: Stores and restaurants, US.
  WNC_STORES_DLBSD.label(d) Commercial Floorspace Stock: Stores and restaurants, West North Central Census Division.
  WSC_STORES_DLBSD.label(d) Commercial Floorspace Stock: Stores and restaurants, West South Central Census Division.
  ENC_WARE_DLBSD.label(d) Commercial Floorspace Stock: Manufacturing and wholesale trade, public and federally-owned warehouses, East North Central Census Division.
  ESC_WARE_DLBSD.label(d) Commercial Floorspace Stock: Manufacturing and wholesale trade, public and federally-owned warehouses, East South Central Census Division.
  MA_WARE_DLBSD.label(d) Commercial Floorspace Stock: Manufacturing and wholesale trade, public and federally-owned warehouses, Mid-Atlantic Census Division.
  MTN_WARE_DLBSD.label(d) Commercial Floorspace Stock: Manufacturing and wholesale trade, public and federally-owned warehouses, Mountain Census Division.
  NENG_WARE_DLBSD.label(d) Commercial Floorspace Stock: Manufacturing and wholesale trade, public and federally-owned warehouses, New England Census Division.
  PAC_WARE_DLBSD.label(d) Commercial Floorspace Stock: Manufacturing and wholesale trade, public and federally-owned warehouses, Pacific Census Division.
  SATL_WARE_DLBSD.label(d) Commercial Floorspace Stock: Manufacturing and wholesale trade, public and federally-owned warehouses, South Atlantic Census Division.
  TOTAL_WARE_DLBSD.label(d) Commercial Floorspace Stock: Manufacturing and wholesale trade, public and federally-owned warehouses, US.
  WNC_WARE_DLBSD.label(d) Commercial Floorspace Stock: Manufacturing and wholesale trade, public and federally-owned warehouses, West North Central Census Division.
  WSC_WARE_DLBSD.label(d) Commercial Floorspace Stock: Manufacturing and wholesale trade, public and federally-owned warehouses, West South Central Census Division.


  for %cd ENC ESC MA MTN NENG PAC SATL WNC WSC

     for %cft AMUSE AUTO DORM EDUC HEALTH HOTEL MFG MISCNR OFFICE PUB REL STORES TOTAL WARE
        if %cft = "TOTAL" then
          {%cd}_{%cft}_DLBSD.label(s) Constructed.

        else
          {%cd}_{%cft}_DLBSD.label(s) F.W. Dodge, Building Stock Database.

        endif

          {%cd}_{%cft}_DLBSD.label(u) Thousand square feet.

     next

  next


  %startfcst = "2023"
  copy Untitled\ogtechmode ogtechmode
  copy Untitled\wwopmode wwopmode
  copy Untitled\macmode macmode

  smpl @all

  genr consr       = {%wfpathpwd}eviewsdb::consr_1
  genr gdpr        = {%wfpathpwd}eviewsdb::gdpr_1
  genr ifnrer      = {%wfpathpwd}eviewsdb::ifnrer_1
  genr jqpcmhfe    = {%wfpathpwd}eviewsdb::jqpcmhfe_1
  genr np16a       = {%wfpathpwd}eviewsdb::np16a
  genr rmcorpaaa   = {%wfpathpwd}eviewsdb::rmcorpaaa_1
  genr wpisop3500  = {%wfpathpwd}eviewsdb::wpisop3500_1
  genr ypdr        = {%wfpathpwd}eviewsdb::ypdr_1

  if (macmode <> 2 or ogtechmode <> 1 or wwopmode <> 2) then
  
    smpl 2040 2050
    wpisop3500 = wpisop3500(-1)*(1+@elem(@pch(wpisop3500),2039))
    smpl @all    
    
  endif

  genr rlrmcorpaaa = rmcorpaaa-100*((wpisop3500(-1)/wpisop3500(-4))^(1/3)-1) + 0.10

  if (wwopmode <> 2) then
    
    genr rlrmcorpaaa = rmcorpaaa-100*((wpisop3500(-1)/wpisop3500(-4))^(1/3)-1) + 0.50
  
  endif
  
  smpl {%startfcst} {%mamlastyr}
  rlrmcorpaaa = rlrmcorpaaa*(rlrmcorpaaa>0)+((rlrmcorpaaa<=0)*rlrmcorpaaa(-1))
  
  'set forecast sample
  smpl {%startfcst} {%mamlastyr}

  model04.scenario(n) "Scenario 1"

'  if (ogtechmode = 23 or ogtechmode = 30 or wwopmode = 3) then
'  
'    model04.exclude(m) ESC_AMUSE_DLBSD MTN_HOTEL_DLBSD MTN_PUB_DLBSD NENG_PUB_DLBSD _
'    PAC_DORM_DLBSD PAC_PUB_DLBSD SATL_EDUC_DLBSD SATL_MISCNR_DLBSD SATL_PUB_DLBSD _
'    WNC_PUB_DLBSD WSC_DORM_DLBSD WSC_PUB_DLBSD
'    
'  endif

  show model04
  model04.solve


'Diagnostic graphs
'  smpl @all
'  ' graphs of flows and stocks
'  for %cenreg ENC ESC MA MTN NENG PAC SATL WNC WSC
'     for %cftype AMUSE AUTO DORM EDUC HEALTH HOTEL MFG MISCNR OFFICE PUB REL STORES TOTAL WARE _
'                 AMUSE_REL HOTEL_DORM PUB_MISCNR
'
'        group {%cenreg}_{%cftype}_igstk
'        if %cftype = "TOTAL" then
'          if %cenreg = "MA" then
'
'            {%cenreg}_{%cftype}_igstk.add {%cenreg}_{%cftype}_ftrend {%cenreg}_{%cftype}_bsdx {%cenreg}_{%cftype}_bsdx_1 sum_stk_matl_1
'
'          else
'
'            {%cenreg}_{%cftype}_igstk.add {%cenreg}_{%cftype}_ftrend {%cenreg}_{%cftype}_bsdx {%cenreg}_{%cftype}_bsdx_1 sum_stk_{%cenreg}_1
'ru
'          endif
'
'        else
'
'          if %cenreg = "MA" then
'
'            {%cenreg}_{%cftype}_igstk.add {%cenreg}_{%cftype}_ftrend {%cenreg}_{%cftype}_bsdx {%cenreg}_{%cftype}_bsdx_1 {%cftype}_stk_matl_1
'
'          else
'
'            {%cenreg}_{%cftype}_igstk.add {%cenreg}_{%cftype}_ftrend {%cenreg}_{%cftype}_bsdx {%cenreg}_{%cftype}_bsdx_1 {%cftype}_stk_{%cenreg}_1
'
'          endif
'
'        endif
'
'        freeze({%cenreg}_{%cftype}_gstk) {%cenreg}_{%cftype}_igstk.line
'        {%cenreg}_{%cftype}_gstk.draw(line, bottom, color(orange)) {%startfcst}
'
'        delete {%cenreg}_{%cftype}_igstk
'
'     next
'
'  next
'
'
'  ' graphs of flows and stocks
'  for %cenreg TOTAL
'     for %cftype AMUSE AUTO DORM EDUC HEALTH HOTEL MFG MISCNR OFFICE PUB REL STORES TOTAL WARE _
'                 AMUSE_REL HOTEL_DORM PUB_MISCNR
'
'        group {%cenreg}_{%cftype}_igstk
'        if %cftype = "TOTAL" then
'
'          {%cenreg}_{%cftype}_igstk.add {%cenreg}_{%cftype}_ftrend {%cenreg}_{%cftype}_bsdx {%cenreg}_{%cftype}_bsdx_1 sum_stk_sum_1
'
'        else
'
'          {%cenreg}_{%cftype}_igstk.add {%cenreg}_{%cftype}_ftrend {%cenreg}_{%cftype}_bsdx {%cenreg}_{%cftype}_bsdx_1 {%cftype}_stk_sum_1
'
'        endif
'
'        freeze({%cenreg}_{%cftype}_gstk) {%cenreg}_{%cftype}_igstk.line
'        {%cenreg}_{%cftype}_gstk.draw(line, bottom, color(orange)) {%startfcst}
'
'        delete {%cenreg}_{%cftype}_igstk
'
'     next
'
'  next


'Create group commflr containing solution and base.
 group gcommflr
      for %cd NENG MA ENC WNC SATL ESC WSC MTN PAC 
         for %cft AMUSE_REL EDUC HEALTH HOTEL_DORM OFFICE AUTO STORES WARE PUB_MISCNR
            gcommflr.add {%cd}_{%cft}_dlbsd_1

         next

      next


'Store quarterly data in the workfile to database eviewsdb.
 store(d={%wfpathpwd}eviewsdb) {%wfpathpwd}eviewsdb::gcommflr


'**********************************************************************
' END OF SUBROUTINE COMMFLR
'**********************************************************************


'# ==========================================
ENDSUB
'# ==========================================



'# ==========================================
SUBROUTINE quarterly
'# ==========================================

pageselect quarterly


' DATES
%start_q 	= "1970q1" 'first quarter of data
%end_q 		= "2050q4" 'last quarter of the projection period 
%start_fcst_q 	= "2023q1" 'first quarter of the projection period
%end_q 		= "2050q4" 'last quarter of the projection period
%end_data_q 	= "2022q4" 'last quarter before start of projection period


' IHS variables
smpl %start_q %end_q
genr rmff = {%wfpathpwd}eviewsdb::rmff_1
genr ruc = {%wfpathpwd}eviewsdb::ruc_1 
genr rmcorpaaa = {%wfpathpwd}eviewsdb::rmcorpaaa_1
genr np16a = {%wfpathpwd}eviewsdb::np16a
genr dlog_np16a = log(np16a) - log(np16a(-4))
for %var gdpr ypdr consr ifnrer cpi
        genr {%var} = {%wfpathpwd}eviewsdb::{%var}_1
	genr dlog_{%var} = log({%var}) - log({%var}(-4)) ' 4 quarter growth
next


'forecast
smpl %start_fcst_q %end_q
group rhs_vars dlog_np16a dlog_gdpr dlog_ypdr dlog_consr dlog_ifnrer rmcorpaaa ruc dlog_cpi
_varmod3.scenario "Scenario 1" 
_varmod3.exclude rhs_vars 
_varmod3.override rhs_vars
solve _varmod3


'US comfloor 4q growth rates
'genr us = us_raw/@elem(us_raw, %end_data_q)*100
smpl %start_q %end_data_q
genr total_us_fcst = us
smpl %start_fcst_q %end_q
genr total_us_fcst = total_us_fcst(-4)*(1 + dlog_us_1) 'apply growth rates from forecast


'subroutine for calculating disaggregated comfloor forecasts
'after US aggregate has been forecasted
'based on long-run assumptions specified in longrun_shares.prg


'----- POST-ESTIMATION: QUARTERLY -----'
'pageselect quarterly

' DISAGGREGATED FORECASTS
for %cd neng matl enc wnc satl esc wsc mtn pac
	for %cft amuse dorm pub health hotel mfg miscnr office auto rel educ stores ware
		smpl %start_q %end_data_q
		genr {%cd}_{%cft}_fcst = {%cd}_{%cft} 'historical data
		smpl %start_fcst_q %end_q
		genr {%cd}_{%cft}_fcst = total_us_fcst * {%cd}_{%cft}_int 'apply forecasted shares by region and type
	next
next

' COMBINED BUILDING TYPES
smpl %start_q %end_q
for %cd neng matl enc wnc satl esc wsc mtn pac 
	genr {%cd}_amuse_rel_fcst = {%cd}_amuse_fcst + {%cd}_rel_fcst
	genr {%cd}_hotel_dorm_fcst = {%cd}_hotel_fcst + {%cd}_dorm_fcst
	genr {%cd}_pub_miscnr_fcst = {%cd}_pub_fcst + {%cd}_miscnr_fcst
next

' AGGREGATE BY REGION
for %cd neng matl enc wnc satl esc wsc mtn pac 
	smpl %start_q %end_q
	genr total_{%cd}_fcst = 0
	for %cft amuse dorm pub health hotel mfg miscnr office auto rel educ stores ware 
		genr total_{%cd}_fcst = total_{%cd}_fcst + {%cd}_{%cft}_fcst 'levels
	next
	'exclude auto and mfg: not included in comfloor module
	genr totalx_{%cd}_fcst = total_{%cd}_fcst - {%cd}_auto_fcst - {%cd}_mfg_fcst 'levels
next

' AGGREGATE BY BUILDING TYPE
for %cft amuse dorm pub health hotel mfg miscnr office auto rel educ stores ware amuse_rel hotel_dorm pub_miscnr
	smpl %start_q %end_q
	genr total_{%cft}_fcst = 0
	for %cd neng matl enc wnc satl esc wsc mtn pac 
		genr total_{%cft}_fcst = total_{%cft}_fcst + {%cd}_{%cft}_fcst 'levels
	next
next

'exclude auto and mfg: not included in comfloor module
genr totalx_us_fcst = total_us_fcst - total_auto_fcst - total_mfg_fcst 'levels

'verify calculation
genr total_us_fcst_verify = 0
for %cd neng matl enc wnc satl esc wsc mtn pac
	genr total_us_fcst_verify = total_us_fcst_verify + total_{%cd}_fcst
next


'----- POST-ESTIMATION: ANNUAL -----'
'move quarterly variables from quarterly page to annual page
'calculate growth rates after transfer
pageselect annual

for %cd neng matl enc wnc satl esc wsc mtn pac 
     for %cft amuse dorm pub health hotel mfg miscnr office auto rel educ stores ware amuse_rel hotel_dorm pub_miscnr
		copy quarterly\{%cd}_{%cft}_fcst * 'levels
		genr dlog_{%cd}_{%cft}_fcst = log({%cd}_{%cft}_fcst) - log({%cd}_{%cft}_fcst(-1)) 'growth rates
	next
next

for %cd neng matl enc wnc satl esc wsc mtn pac 
	copy quarterly\total_{%cd}_fcst * 
	genr dlog_total_{%cd}_fcst = log(total_{%cd}_fcst) - log(total_{%cd}_fcst(-1)) 'growth rates
next

for %cft amuse dorm pub health hotel mfg miscnr office auto rel educ stores ware amuse_rel hotel_dorm pub_miscnr
	copy quarterly\total_{%cft}_fcst * 
	genr dlog_total_{%cft}_fcst = log(total_{%cft}_fcst) - log(total_{%cft}_fcst(-1)) 'growth rates
next

copy quarterly\total_us_fcst *
genr dlog_total_us_fcst = log(total_us_fcst) - log(total_us_fcst(-1)) 'growth rates
copy quarterly\totalx_us_fcst *
genr dlog_totalx_us_fcst = log(totalx_us_fcst) - log(totalx_us_fcst(-1)) 'growth rates

' calculate ANNUAL shares of US aggregate comfloor
smpl @all 
for %cd neng matl enc wnc satl esc wsc mtn pac
	genr {%cd}_sh = total_{%cd}_fcst/total_us_fcst
next
for %cft amuse dorm pub health hotel mfg miscnr office auto rel educ stores ware
	genr {%cft}_sh = total_{%cft}_fcst/total_us_fcst
next

'write out projection to global data structure
  smpl @all
    for %cd NENG MATL ENC WNC SATL ESC WSC MTN PAC 
       for %cft AMUSE_REL EDUC HEALTH HOTEL_DORM OFFICE AUTO STORES WARE PUB_MISCNR
          genr {%cd}_{%cft}_dlbsd_1 = dlog_{%cd}_{%cft}_fcst
            
       next

    next

  
  group gcommflr
    for %cd NENG MATL ENC WNC SATL ESC WSC MTN PAC 
       for %cft AMUSE_REL EDUC HEALTH HOTEL_DORM OFFICE AUTO STORES WARE PUB_MISCNR
          gcommflr.add {%cd}_{%cft}_dlbsd_1
            
       next

    next
 
  store(d={%wfpathpwd}eviewsdb) {%wfpathpwd}eviewsdb::gcommflr


'# ==========================================
ENDSUB
'# ==========================================



'# ==========================================
SUBROUTINE REGIONAL
'# ==========================================


%mamlastyr = @str(mamlastyr)

smpl @all

genr cpi                   = {%wfpathpwd}eviewsdb::cpi_0
genr cpi_ave               = 0.
genr cpi_ave_adj           = 0.

genr gdpr                  = {%wfpathpwd}eviewsdb::gdpr_0
genr gspr_sum              = 0.
genr gspr_sum_adj          = 0.

genr jeciwsp               = {%wfpathpwd}eviewsdb::jeciwsp_0

genr jpc                   = {%wfpathpwd}eviewsdb::jpc_0

genr mhrsnfp               = {%wfpathpwd}eviewsdb::mhrsnfp_0

genr np                    = {%wfpathpwd}eviewsdb::np
genr np_sum                = 0.
genr np_sum_adj            = 0.

genr np16a                 = {%wfpathpwd}eviewsdb::np16a
genr np16a_sum             = 0.
genr np16a_sum_adj         = 0.

genr rwm_ave               = 0.
genr rwnm_ave              = 0.
             
genr yp                    = {%wfpathpwd}eviewsdb::yp_0
genr yp_sum_adj            = 0.

genr ypcompwsd             = {%wfpathpwd}eviewsdb::ypcompwsd_0
genr ypcompwsdg            = {%wfpathpwd}eviewsdb::ypcompwsdg_0
genr ypcompwsdp            = {%wfpathpwd}eviewsdb::ypcompwsdp_0
genr ypcompwsd_sum_adj     = 0.
genr ypcompwsdg_sum_adj    = 0.
genr ypcompwsdp_sum_adj    = 0.

genr ypoth                 = {%wfpathpwd}eviewsdb::yp_0-{%wfpathpwd}eviewsdb::ypcompwsd_0
genr ypoth_sum_adj         = 0.

genr ypdr                  = {%wfpathpwd}eviewsdb::ypdr_0
genr ypdr_sum_adj          = 0.


for %reg enc esc matl mtn neng pac satl wnc wsc

 cpi_ave                   = cpi_ave+cpi_{%reg}
 genr cpi_{%reg}_adj       = cpi_{%reg}
 genr cpiz_{%reg}          = cpi_{%reg}/cpi

 np_sum                    = np_sum+np_{%reg}
 genr np_{%reg}_adj        = np_{%reg}

 np16a_sum                 = np16a_sum+np16a_{%reg}
 genr np16a_{%reg}_adj     = np16a_{%reg}

 gspr_sum                  = gspr_sum+gspr_{%reg}
 genr gspr_{%reg}_adj      = gspr_{%reg}

 rwm_ave                   = rwm_ave+rwm_{%reg}

 rwnm_ave                  = rwnm_ave+rwnm_{%reg}

 genr yp_{%reg}_adj        = yp_{%reg}

 genr ypcompwsdg_{%reg}_adj = ypcompwsdg_{%reg}
 genr ypcompwsdp_{%reg}_adj = ypcompwsdp_{%reg}
 genr ypcompwsd_{%reg}_adj  = ypcompwsd_{%reg}

 genr ypoth_{%reg}_adj     = ypoth_{%reg}

 genr ypdr_{%reg}_adj      = ypdr_{%reg}
 genr ypdrznp_{%reg}       = ypdr_{%reg}/np_{%reg}

next

cpi_ave                    = cpi_ave/9.
rwm_ave                    = rwm_ave/9.
rwnm_ave                   = rwnm_ave/9.

for %reg enc esc matl mtn neng pac satl wnc wsc

 cpi_{%reg}_adj       = cpi_{%reg}_adj*cpi/cpi_ave
 cpi_ave_adj          = cpi_ave_adj+cpi_{%reg}_adj

 np_{%reg}_adj        = np_{%reg}_adj*np/np_sum
 np_sum_adj           = np_sum_adj+np_{%reg}_adj

 np16a_{%reg}_adj     = np16a_{%reg}_adj*np16a/np16a_sum
 np16a_sum_adj        = np16a_sum_adj+np16a_{%reg}_adj

 gspr_{%reg}_adj      = gspr_{%reg}_adj*gdpr/gspr_sum
 gspr_sum_adj         = gspr_sum_adj+gspr_{%reg}_adj
 genr gsprznp_{%reg}  = gspr_{%reg}_adj/np_{%reg}_adj

 yp_{%reg}_adj        = yp_{%reg}_adj*yp/yp_sum
 yp_sum_adj           = yp_sum_adj+yp_{%reg}_adj

 ypcompwsdg_{%reg}_adj = ypcompwsdg_{%reg}_adj*ypcompwsdg/ypcompwsdg_sum
 ypcompwsdg_sum_adj    = ypcompwsdg_sum_adj+ypcompwsdg_{%reg}_adj

 ypcompwsdp_{%reg}_adj = ypcompwsdp_{%reg}_adj*ypcompwsdp/ypcompwsdp_sum
 ypcompwsdp_sum_adj    = ypcompwsdp_sum_adj+ypcompwsdp_{%reg}_adj

 ypcompwsd_{%reg}_adj = ypcompwsd_{%reg}_adj*ypcompwsd/ypcompwsd_sum
 ypcompwsd_sum_adj    = ypcompwsd_sum_adj+ypcompwsd_{%reg}_adj

 ypoth_{%reg}_adj     = ypoth_{%reg}_adj*ypoth/ypoth_sum
 ypoth_sum_adj        = ypoth_sum_adj+ypoth_{%reg}_adj

 ypdr_{%reg}_adj      = ypdr_{%reg}_adj*ypdr/ypdr_sum
 ypdr_sum_adj         = ypdr_sum_adj+ypdr_{%reg}_adj

next

cpi_ave_adj = cpi_ave_adj/9.


' import US model baseline and exogenous variables
genr edrsre      = {%wfpathpwd}eviewsdb::edrsre
genr invr        = {%wfpathpwd}eviewsdb::invr_0
genr rmmtg30con  = {%wfpathpwd}eviewsdb::rmmtg30con_0
genr rtxpmarg    = {%wfpathpwd}eviewsdb::rtxpmarg_0
genr time        = {%wfpathpwd}eviewsdb::time
genr ypd         = {%wfpathpwd}eviewsdb::ypd_0

' construct tax variables
genr tax         = yp-ypd
genr taxrate     = tax/yp


smpl @all
for %reg enc esc matl mtn neng pac satl wnc wsc

 genr jpc_rel_{%reg}        = ypd_{%reg}/ypdr_{%reg}/jpc

' compute regional tax and taxrate
 genr tax_{%reg}            = yp_{%reg}-ypd_{%reg}
 genr taxrate_rel_{%reg}    = (tax_{%reg}/yp_{%reg})/taxrate

next


' construct more variables
genr atr                    = 1.-{%wfpathpwd}eviewsdb::rtxpmarg_0
genr xrzn                   = {%wfpathpwd}eviewsdb::xr_0/np
genr gdprzn                 = gdpr/np
genr gfsrzn                 = ({%wfpathpwd}eviewsdb::gfr_0+{%wfpathpwd}eviewsdb::gslr_0)/np

genr mrzn                   = {%wfpathpwd}eviewsdb::mr_0/np
genr pcjpgdp                = 100*(({%wfpathpwd}eviewsdb::jpgdp_0 _
                            / {%wfpathpwd}eviewsdb::jpgdp_0(-1))^4-1)
genr jpmz                   = {%wfpathpwd}eviewsdb::jpm_0/{%wfpathpwd}eviewsdb::jpgdp_0
genr vfsrzn                 = ({%wfpathpwd}eviewsdb::yptrfgf_0+{%wfpathpwd}eviewsdb::yptrfgsl_0) _
                            / ({%wfpathpwd}eviewsdb::jpc_0*np)


'estimate regional gspr equations
smpl 1959:1 2020:4
for %cd ENC ESC MATL MTN NENG PAC SATL WNC WSC 
  EQUATION GSPR_{%CD}_EQ.LS LOG(GSPRZNP_{%CD}/GDPRZN) LOG(GSPRZNP_{%CD}(-1)/GDPRZN(-1))
next

'over write some regional gspr equations
EQUATION GSPR_ESC_EQ.LS LOG(GSPRZNP_ESC/GDPRZN) LOG(GSPRZNP_ESC(-1)/GDPRZN(-1)) _
                           @MOVAV(LOG(GSPRZNP_ESC(-1)/GDPRZN(-1)),3)
EQUATION GSPR_PAC_EQ.LS LOG(GSPRZNP_PAC/GDPRZN) LOG(GSPRZNP_PAC(-1)/GDPRZN(-1)) _
                           @MOVAV(LOG(GSPRZNP_PAC(-1)/GDPRZN(-1)),3)

'estimate equations for annual average pay in manufacturing and non-manufacturing industries
for %cd ENC ESC MATL MTN NENG PAC SATL WNC WSC 
  EQUATION RWM_{%CD}_EQ.LS   DLOG(RWM_{%CD})   DLOG(jeciwsp)
  EQUATION RWNM_{%CD}_EQ.LS  DLOG(RWNM_{%CD})  DLOG(jeciwsp)
next

smpl @all

' build GSPR model
model m_gspr

for %reg enc esc matl mtn neng pac satl wnc wsc

 m_gspr.append :gspr_{%reg}_eq
 m_gspr.append @identity gspr_{%reg} = gsprznp_{%reg}*np_{%reg}_adj

 m_gspr.append @identity gspr_{%reg}_adj = gdpr*gspr_{%reg}/gspr_sum

next

m_gspr.append @identity gdprzn = gdpr/np

m_gspr.append @identity gspr_sum = gspr_enc+gspr_esc+gspr_matl+gspr_mtn _
                                 + gspr_neng+gspr_pac+gspr_satl+gspr_wnc _
                                 + gspr_wsc

m_gspr.append @identity gspr_sum_adj = gspr_enc_adj+gspr_esc_adj+gspr_matl_adj+gspr_mtn_adj _
                                     + gspr_neng_adj+gspr_pac_adj+gspr_satl_adj+gspr_wnc_adj _
                                     + gspr_wsc_adj


' sim the GSPR model
smpl 2023:1 {%mamlastyr}
m_gspr.scenario "baseline"
m_gspr.solve

genr gdpr = {%wfpathpwd}eviewsdb::gdpr_1
genr invr = {%wfpathpwd}eviewsdb::invr_1

m_gspr.scenario "scenario 1"
m_gspr.solve


' build regional model
model m_regional

for %reg enc esc matl mtn neng pac satl wnc wsc


 m_regional.append @identity ypdr_{%reg}_adj       = ypdr*ypdr_{%reg}/ypdr_sum
 m_regional.append @identity ypdrznp_{%reg}        = ypdr_{%reg}/np_{%reg}

 m_regional.append @identity yp_{%reg}_adj         = yp*yp_{%reg}/yp_sum

 m_regional.append @identity ypd_{%reg}_adj        = ypd*ypd_{%reg}/ypd_sum

 m_regional.append @identity ypcompwsdg_{%reg}_adj = ypcompwsdg*ypcompwsdg_{%reg}/ypcompwsdg_sum

 m_regional.append @identity ypcompwsdp_{%reg}_adj = ypcompwsdp*ypcompwsdp_{%reg}/ypcompwsdp_sum

 m_regional.append @identity ypcompwsd_{%reg}_adj  = ypcompwsd*ypcompwsd_{%reg}/ypcompwsd_sum

 m_regional.append @identity ypoth_{%reg}_adj      = yp_{%reg}_adj-ypcompwsd_{%reg}_adj

 m_regional.append :rwm_{%reg}_eq
 m_regional.append :rwnm_{%reg}_eq

 m_regional.append @identity cpi_{%reg}           = cpiz_{%reg}*cpi
 m_regional.append @identity cpi_{%reg}_adj       = cpi*cpi_{%reg}/cpi_ave


next

m_regional.append @identity tax               = yp-ypd
m_regional.append @identity taxrate           = tax/yp

m_regional.append @identity ypdr_sum_adj      = ypdr_enc_adj+ypdr_esc_adj+ypdr_matl_adj+ypdr_mtn_adj _
                                              + ypdr_neng_adj+ypdr_pac_adj+ypdr_satl_adj+ypdr_wnc_adj _
                                              + ypdr_wsc_adj

m_regional.append @identity ypd_sum_adj       = ypd_enc_adj +ypd_esc_adj+ypd_matl_adj+ypd_mtn_adj _
                                              + ypd_neng_adj+ypd_pac_adj+ypd_satl_adj+ypd_wnc_adj _
                                              + ypd_wsc_adj

m_regional.append @identity yp_sum_adj        = yp_enc_adj+yp_esc_adj+yp_matl_adj+yp_mtn_adj+yp_neng_adj _
                                              + yp_pac_adj+yp_satl_adj+yp_wnc_adj+yp_wsc_adj

m_regional.append @identity ypcompwsd_sum_adj = ypcompwsd_enc_adj+ypcompwsd_esc_adj+ypcompwsd_matl_adj+ypcompwsd_mtn_adj _
                                              + ypcompwsd_neng_adj+ypcompwsd_pac_adj+ypcompwsd_satl_adj+ypcompwsd_wnc_adj _
                                              + ypcompwsd_wsc_adj

m_regional.append @identity ypcompwsdg_sum_adj = ypcompwsdg_enc_adj+ypcompwsdg_esc_adj+ypcompwsdg_matl_adj _
                                               + ypcompwsdg_mtn_adj+ypcompwsdg_neng_adj+ypcompwsdg_pac_adj _
                                               + ypcompwsdg_satl_adj+ypcompwsdg_wnc_adj+ypcompwsdg_wsc_adj

m_regional.append @identity ypcompwsdp_sum_adj = ypcompwsdp_enc_adj+ypcompwsdp_esc_adj+ypcompwsdp_matl_adj _
                                               + ypcompwsdp_mtn_adj+ypcompwsdp_neng_adj+ypcompwsdp_pac_adj _
                                               + ypcompwsdp_satl_adj+ypcompwsdp_wnc_adj+ypcompwsdp_wsc_adj

m_regional.append @identity ypoth_sum_adj     = ypoth_enc_adj+ypoth_esc_adj+ypoth_matl_adj+ypoth_mtn_adj+ypoth_neng_adj _
                                              + ypoth_pac_adj+ypoth_satl_adj+ypoth_wnc_adj+ypoth_wsc_adj

m_regional.append @identity rwm_ave           = (rwm_enc+rwm_esc+rwm_matl+rwm_mtn _
                                              +  rwm_neng+rwm_pac+rwm_satl+rwm_wnc _
                                              +  rwm_wsc)/9.

m_regional.append @identity rwnm_ave          = (rwnm_enc+rwnm_esc+rwnm_matl+rwnm_mtn _
                                              +  rwnm_neng+rwnm_pac+rwnm_satl+rwnm_wnc _
                                              +  rwnm_wsc)/9.

m_regional.append @identity cpi_ave           = (cpi_enc+cpi_esc+cpi_matl+cpi_mtn _
                                              +  cpi_neng+cpi_pac+cpi_satl+cpi_wnc _
                                              +  cpi_wsc)/9.

m_regional.append @identity cpi_ave_adj       = (cpi_enc_adj+cpi_esc_adj+cpi_matl_adj+cpi_mtn_adj _
                                              +  cpi_neng_adj+cpi_pac_adj+cpi_satl_adj+cpi_wnc_adj _
                                              +  cpi_wsc_adj)/9.


' sim regional model
smpl 2023:1 {%mamlastyr}
m_regional.scenario "baseline"
m_regional.solve

smpl 2023:1 {%mamlastyr}
genr cpi        = {%wfpathpwd}eviewsdb::cpi_1
genr jpc        = {%wfpathpwd}eviewsdb::jpc_1
genr jeciwsp    = {%wfpathpwd}eviewsdb::jeciwsp_1
genr mhrsnfp    = {%wfpathpwd}eviewsdb::mhrsnfp_1
genr rmmtg30con = {%wfpathpwd}eviewsdb::rmmtg30con_1
genr rtxpmarg   = {%wfpathpwd}eviewsdb::rtxpmarg_1
genr yp         = {%wfpathpwd}eviewsdb::yp_1
genr ypcompwsd  = {%wfpathpwd}eviewsdb::ypcompwsd_1
genr ypcompwsdg = {%wfpathpwd}eviewsdb::ypcompwsdg_1
genr ypcompwsdp = {%wfpathpwd}eviewsdb::ypcompwsdp_1
genr ypoth      = {%wfpathpwd}eviewsdb::yp_1-{%wfpathpwd}eviewsdb::ypcompwsd_1
genr ypd        = {%wfpathpwd}eviewsdb::ypd_1
genr ypdr       = {%wfpathpwd}eviewsdb::ypdr_1

m_regional.scenario "scenario 1"
m_regional.solve

smpl 2023:1 {%mamlastyr}
m_regional.scenario(n) "scenario 2"
m_regional.solve


' Overwrite housing start forecast with shares supplied by J. Cymbalsky.
for %hous husmfg husps1 husps2a

   smpl @all

   genr {%hous}_sum_0 = 0
   genr {%hous}_sum_1 = 0

   genr {%hous}_0 = {%wfpathpwd}eviewsdb::{%hous}_0
   genr {%hous}_1 = {%wfpathpwd}eviewsdb::{%hous}_1


   smpl 1959:1 2020:4

   for %cd ENC ESC MATL MTN NENG PAC SATL WNC WSC

      genr {%hous}_{%cd}_0 = {%hous}_{%cd}_0/1000.
      genr {%hous}_{%cd}_1 = {%hous}_{%cd}_0

      {%hous}_sum_0 = {%hous}_sum_0 + {%hous}_{%cd}_0
      {%hous}_sum_1 = {%hous}_sum_1 + {%hous}_{%cd}_1

   next


   for %cd ENC ESC MATL MTN NENG PAC SATL WNC WSC

      {%hous}_{%cd}_0 = {%hous}_{%cd}_0*{%hous}_0/{%hous}_sum_0
      {%hous}_{%cd}_1 = {%hous}_{%cd}_1*{%hous}_1/{%hous}_sum_1

   next


   {%hous}_sum_0 = 0
   {%hous}_sum_1 = 0

   for %cd ENC ESC MATL MTN NENG PAC SATL WNC WSC

      {%hous}_sum_0 = {%hous}_sum_0 + {%hous}_{%cd}_0
      {%hous}_sum_1 = {%hous}_sum_1 + {%hous}_{%cd}_1

   next

' Moved the sample from 2015:1 to 2009:1 at request of O. Comstock.
'   smpl 2015:1 {%mamlastyr}
   smpl 2009:1 {%mamlastyr}


   for %cd ENC ESC MATL MTN NENG PAC SATL WNC WSC

      {%hous}_{%cd}_0 = {%hous}_0*{%wfpathpwd}eviewsdb::js_{%hous}_{%cd}
      {%hous}_{%cd}_1 = {%hous}_1*{%wfpathpwd}eviewsdb::js_{%hous}_{%cd}

      {%hous}_sum_0 = {%hous}_sum_0 + {%hous}_{%cd}_0 
      {%hous}_sum_1 = {%hous}_sum_1 + {%hous}_{%cd}_1 

   next

next


' Compute housing stocks forecast based upon starts whose shares are supplied by J. Cymbalsky.

smpl @all


genr khumfg_0      = {%wfpathpwd}eviewsdb::khumfg_0
genr khups1_0      = {%wfpathpwd}eviewsdb::khups1_0
genr khups2a_0     = {%wfpathpwd}eviewsdb::khups2a_0

genr khumfg_1      = {%wfpathpwd}eviewsdb::khumfg_1
genr khups1_1      = {%wfpathpwd}eviewsdb::khups1_1
genr khups2a_1     = {%wfpathpwd}eviewsdb::khups2a_1


genr khups1_sum_0  = 0
genr khups2a_sum_0 = 0
genr khumfg_sum_0  = 0

genr khups1_sum_1  = 0
genr khups2a_sum_1 = 0
genr khumfg_sum_1  = 0


for %cd ENC ESC MATL MTN NENG PAC SATL WNC WSC

   genr khups1_{%cd}_1  = khups1_{%cd}_0
   genr khups2a_{%cd}_1 = khups2a_{%cd}_0
   genr khumfg_{%cd}_1  = khumfg_{%cd}_0

next

smpl 2023:1 {%mamlastyr}

for %cd ENC ESC MATL MTN NENG PAC SATL WNC WSC

   khups1_{%cd}_0  = husps1_{%cd}_0/4 + ((1.-edrsre/100)^0.25)*khups1_{%cd}_0(-1)
   khups2a_{%cd}_0 = husps2a_{%cd}_0/4+ ((1.-edrsre/100)^0.25)*khups2a_{%cd}_0(-1)
   khumfg_{%cd}_0  = husmfg_{%cd}_0/4 + ((1.-edrsre/100)^0.25)*khumfg_{%cd}_0(-1)

   khups1_{%cd}_1  = husps1_{%cd}_1/4 + ((1.-edrsre/100)^0.25)*khups1_{%cd}_1(-1)
   khups2a_{%cd}_1 = husps2a_{%cd}_1/4+ ((1.-edrsre/100)^0.25)*khups2a_{%cd}_1(-1)
   khumfg_{%cd}_1  = husmfg_{%cd}_1/4 + ((1.-edrsre/100)^0.25)*khumfg_{%cd}_1(-1)
   
next


smpl @all

for %cd ENC ESC MATL MTN NENG PAC SATL WNC WSC
   khups1_sum_0  = khups1_sum_0  + khups1_{%cd}_0
   khups2a_sum_0 = khups2a_sum_0 + khups2a_{%cd}_0
   khumfg_sum_0  = khumfg_sum_0  + khumfg_{%cd}_0

   khups1_sum_1  = khups1_sum_1  + khups1_{%cd}_1
   khups2a_sum_1 = khups2a_sum_1 + khups2a_{%cd}_1
   khumfg_sum_1  = khumfg_sum_1  + khumfg_{%cd}_1

next


for %cd ENC ESC MATL MTN NENG PAC SATL WNC WSC

   khups1_{%cd}_0  = khups1_{%cd}_0 *khups1_0/khups1_sum_0
   khups2a_{%cd}_0 = khups2a_{%cd}_0*khups2a_0/khups2a_sum_0
   khumfg_{%cd}_0  = khumfg_{%cd}_0 *khumfg_0/khumfg_sum_0

   khups1_{%cd}_1  = khups1_{%cd}_1 *khups1_1/khups1_sum_1
   khups2a_{%cd}_1 = khups2a_{%cd}_1*khups2a_1/khups2a_sum_1
   khumfg_{%cd}_1  = khumfg_{%cd}_1 *khumfg_1/khumfg_sum_1

next

smpl @all


genr khups1_sum_0  = 0
genr khups2a_sum_0 = 0
genr khumfg_sum_0  = 0

genr khups1_sum_1  = 0
genr khups2a_sum_1 = 0
genr khumfg_sum_1  = 0


for %cd ENC ESC MATL MTN NENG PAC SATL WNC WSC
   khups1_sum_0  = khups1_sum_0  + khups1_{%cd}_0
   khups2a_sum_0 = khups2a_sum_0 + khups2a_{%cd}_0
   khumfg_sum_0  = khumfg_sum_0  + khumfg_{%cd}_0

   khups1_sum_1  = khups1_sum_1  + khups1_{%cd}_1
   khups2a_sum_1 = khups2a_sum_1 + khups2a_{%cd}_1
   khumfg_sum_1  = khumfg_sum_1  + khumfg_{%cd}_1

next


for %scen 1 0
    genr cpi_{%scen}       = {%wfpathpwd}eviewsdb::cpi_{%scen}
    genr ypdr_{%scen}      = {%wfpathpwd}eviewsdb::ypdr_{%scen}
    genr ypcompwsd_{%scen} = {%wfpathpwd}eviewsdb::ypcompwsd_{%scen}
    genr yp_{%scen}        = {%wfpathpwd}eviewsdb::yp_{%scen}
    genr husmfg_{%scen}    = {%wfpathpwd}eviewsdb::husmfg_{%scen}
    genr husps1_{%scen}    = {%wfpathpwd}eviewsdb::husps1_{%scen}
    genr husps2a_{%scen}   = {%wfpathpwd}eviewsdb::husps2a_{%scen}
    genr khumfg_{%scen}    = {%wfpathpwd}eviewsdb::khumfg_{%scen}
    genr khups1_{%scen}    = {%wfpathpwd}eviewsdb::khups1_{%scen}
    genr khups2a_{%scen}   = {%wfpathpwd}eviewsdb::khups2a_{%scen}

next


for %x khumfg khups1 khups2a

  delete {%x}_0
  delete {%x}_1
  
  copy {%x}_sum_0 {%x}_0
  copy {%x}_sum_1 {%x}_1

next


genr np                    = {%wfpathpwd}eviewsdb::np
genr np16a                 = {%wfpathpwd}eviewsdb::np16a

' Create group regional containing solution, base and shares.
 group gregmac
   for %cd neng matl enc wnc satl esc wsc mtn pac 

      gregmac.add gspr_{%cd}_adj_0
      gregmac.add gspr_{%cd}_adj_1

      gregmac.add cpi_{%cd}_adj_1
      gregmac.add ypdr_{%cd}_adj_1
      gregmac.add ypcompwsd_{%cd}_adj_1
      gregmac.add yp_{%cd}_adj_1
      gregmac.add husmfg_{%cd}_1
      gregmac.add husps1_{%cd}_1
      gregmac.add husps2a_{%cd}_1
      gregmac.add khumfg_{%cd}_1
      gregmac.add khups1_{%cd}_1
      gregmac.add khups2a_{%cd}_1
      gregmac.add np_{%cd}_adj
      gregmac.add np16a_{%cd}_adj
      gregmac.add rwm_{%cd}_1
      gregmac.add rwnm_{%cd}_1

   next

' Create group regional containing solution, base and shares.
 group gregmac2
   for %scen 1 0

      for %cd neng matl enc wnc satl esc wsc mtn pac
         gregmac2.add cpi_{%cd}_adj_{%scen}
         gregmac2.add ypdr_{%cd}_adj_{%scen}
         gregmac2.add ypcompwsd_{%cd}_adj_{%scen}
         gregmac2.add yp_{%cd}_adj_{%scen}
         gregmac2.add husmfg_{%cd}_{%scen}
         gregmac2.add husps1_{%cd}_{%scen}
         gregmac2.add husps2a_{%cd}_{%scen}
         gregmac2.add khumfg_{%cd}_{%scen}
         gregmac2.add khups1_{%cd}_{%scen}
         gregmac2.add khups2a_{%cd}_{%scen}
         gregmac2.add np_{%cd}
         gregmac2.add np16a_{%cd}
         gregmac2.add rwm_{%cd}_{%scen}
         gregmac2.add rwnm_{%cd}_{%scen}

      next

      gregmac2.add cpi_{%scen}
      gregmac2.add ypdr_{%scen}
      gregmac2.add ypcompwsd_{%scen}
      gregmac2.add yp_{%scen}
      gregmac2.add husmfg_{%scen}
      gregmac2.add husps1_{%scen}
      gregmac2.add husps2a_{%scen}
      gregmac2.add khumfg_{%scen}
      gregmac2.add khups1_{%scen}
      gregmac2.add khups2a_{%scen}
      gregmac2.add np
      gregmac2.add np16a

      gregmac2.add rwm_ave_{%scen}
      gregmac2.add rwnm_ave_{%scen}

   next

 group gregmac3
   for %scen 0 1

      for %cd neng matl enc wnc satl esc wsc mtn pac 

         gregmac3.add gspr_{%cd}_adj_{%scen}
         gregmac3.add ypdr_{%cd}_adj_{%scen}

      next

   next


'Store quarterly data in the workfile to database eviewsdb.
 store(d={%wfpathpwd}eviewsdb) {%wfpathpwd}eviewsdb::gregmac
 store(d={%wfpathpwd}eviewsdb) {%wfpathpwd}eviewsdb::gregmac2
 store(d={%wfpathpwd}eviewsdb) {%wfpathpwd}eviewsdb::gregmac3


'**********************************************************************
' END OF SUBROUTINE REGIONAL
'**********************************************************************


'# ==========================================
ENDSUB 
'# ==========================================
