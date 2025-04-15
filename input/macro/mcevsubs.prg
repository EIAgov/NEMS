'# ==========================================
SUBROUTINE sub_mac111d
'# ==========================================


'reference and side cases baseline investment except low and high macro
if (macmode = 2)then


'Annual Expense and Capital Commitment, Electric Sector, Total Resource Costs
'Installed Capacity
'Billions of 2009 Dollars
'Source: No ABCPP Reference Case
'        //nem1/Q/output/aeo2017/ref_no_abcpp/d111616a
genr elsec_instcap_b = 0
elsec_instcap_b.fill(o=1997)      0.000000,  0.000000,  0.000000,  0.000000,  0.000000, _
 0.000000,  0.000000,  0.000000,  0.000000,  0.000000,  0.000000,  0.000000,  0.000000, _
 0.000000,  0.000000,  0.000000,  0.000000,  0.000000,  0.000000, 66.502930, 40.055000, _
14.203466, 26.520622, 52.263237, 32.591045, 20.189800,  3.285453,  5.127619,  6.054205, _
 7.985078,  6.915232, 15.077737, 13.766582, 14.227000, 14.951482, 10.307487, 12.026816, _
14.961078, 13.517105, 15.909302, 13.055498, 10.309064, 10.287686, 10.296397, 18.595566, _
16.973913, 14.672043, 15.895393, 17.781700, 18.740870, 19.837994, 19.558395, 21.337004, _
24.309908


'Annual Expense and Capital Commitment, Electric Sector, Total Resource Costs
'Transmission
'Billions of 2009 Dollars
'Source: No ABCPP Reference Case
'        //nem1/Q/output/aeo2017/ref_no_abcpp/d111616a
genr elsec_tran_b = 0
elsec_tran_b.fill(o=1997)     0.000000, 0.000000, 0.000000, 0.000000, 0.000000, _
0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, _
0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 3.184161, 2.620721, _
0.906890, 0.738230, 2.150491, 1.997307, 1.341384, 0.207604, 0.368992, 0.442114, _
0.603295, 0.486329, 0.968609, 0.932988, 0.971197, 1.011904, 0.702819, 0.846205, _
1.131122, 0.971061, 1.140380, 0.955463, 0.825312, 0.802407, 0.838486, 1.381992, _
1.311816, 1.114227, 1.192271, 1.430821, 1.537479, 1.541986, 1.579782, 1.748815, _
2.161597


'Annual Expense and Capital Commitment, Electric Sector, Total Resource Costs
'Retrofits
'Billions of 2009 Dollars
'Source: No ABCPP Reference Case
'        //nem1/Q/output/aeo2017/ref_no_abcpp/d111616a
genr elsec_retro_b = 0
elsec_retro_b.fill(o=1997)    0.000000, 0.000000, 0.000000, 0.000000, 0.000000, _
0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, _
0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 2.788217, 0.384887, _
0.519337, 1.131820, 1.276367, 0.026212, 0.031933, 0.200608, 0.000000, 0.000000, _
0.000000, 0.020358, 0.206805, 0.665648, 0.407531, 0.035087, 0.070758, 0.000000, _
0.099342, 0.000000, 0.000000, 0.043804, 0.000000, 0.145086, 0.000000, 0.000000, _
0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.091429, _
0.000000


endif


'low macro investment baseline
if (macmode = 4)then


'Annual Expense and Capital Commitment, Electric Sector, Total Resource Costs
'Installed Capacity
'Billions of 2009 Dollars
'Source: No CPP Reference Case
'        //nem1/Q/output/aeo2017/lm_no_abcpp/d111816a
genr elsec_instcap_b = 0
elsec_instcap_b.fill(o=1997)      0.000000,  0.000000,  0.000000,  0.000000,  0.000000, _
 0.000000,  0.000000,  0.000000,  0.000000,  0.000000,  0.000000,  0.000000,  0.000000, _
 0.000000,  0.000000,  0.000000,  0.000000,  0.000000,  0.000000, 66.502930, 40.106995, _
13.792732, 29.788864, 54.189587, 33.170811,  8.969301,  2.535049,  3.676453,  3.944578, _
 4.555389,  4.271817,  9.060807,  8.323685,  8.714393,  8.688277,  9.655673, 10.629487, _
14.412014, 10.167546, 13.340382,  9.654643,  9.319817,  7.433548,  6.554801, 10.561786, _
11.702043,  8.530060,  9.742535, 11.233026, 11.987369, 12.438950,  9.350551,  9.409238, _
11.145323


'Annual Expense and Capital Commitment, Electric Sector, Total Resource Costs
'Transmission
'Billions of 2009 Dollars
'Source: No CPP Reference Case
'        //nem1/Q/output/aeo2017/lm_no_abcpp/d111816a
genr elsec_tran_b = 0
elsec_tran_b.fill(o=1997)     0.000000, 0.000000, 0.000000, 0.000000, 0.000000, _
0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, _
0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 3.184161, 2.620072, _
0.880459, 0.851667, 2.190274, 2.007892, 0.630809, 0.145502, 0.255019, 0.297919, _
0.354603, 0.285814, 0.610350, 0.545654, 0.618244, 0.566217, 0.694687, 0.787148, _
1.047419, 0.762114, 1.008985, 0.753100, 0.699641, 0.628458, 0.563181, 0.763220, _
0.853651, 0.689322, 0.795411, 0.903932, 1.005450, 1.052581, 0.789706, 0.882186, _
0.951635


'Annual Expense and Capital Commitment, Electric Sector, Total Resource Costs
'Retrofits
'Billions of 2009 Dollars
'Source: No CPP Reference Case
'        //nem1/Q/output/aeo2017/lm_no_abcpp/d111816a
genr elsec_retro_b = 0
elsec_retro_b.fill(o=1997)    0.000000, 0.000000, 0.000000, 0.000000, 0.000000, _
0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, _
0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 2.788217, 0.384887, _
0.710558, 1.105558, 0.044084, 0.000000, 0.000000, 0.178723, 0.000000, 0.000000, _
0.036609, 0.008259, 0.000000, 0.072477, 0.000000, 0.000000, 0.000000, 0.000000, _
0.000000, 0.061708, 0.047217, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, _
0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, _
0.000000


endif


'high macro investment baseline
if (macmode = 5)then


'Annual Expense and Capital Commitment, Electric Sector, Total Resource Costs
'Installed Capacity
'Billions of 2009 Dollars
'Source: No CPP Reference Case
'        //nem1/Q/output/aeo2017/hm_no_abcpp/d111816a
genr elsec_instcap_b = 0
elsec_instcap_b.fill(o=1997)      0.000000,  0.000000,  0.000000,  0.000000,  0.000000, _
 0.000000,  0.000000,  0.000000,  0.000000,  0.000000,  0.000000,  0.000000,  0.000000, _
 0.000000,  0.000000,  0.000000,  0.000000,  0.000000,  0.000000, 66.502930, 39.984417, _
14.070284, 27.170923, 52.746704, 32.684429, 28.532202,  5.062190,  5.179525,  7.718199, _
12.864685, 15.480722, 18.686621, 20.481327, 22.853148, 11.810833, 12.828535, 14.035525, _
23.610512, 13.223314, 19.237085, 25.331446, 25.879190, 21.905247, 23.439098, 26.627699, _
17.713999, 17.419289, 21.420237, 29.637550, 24.785795, 29.137861, 22.352791, 23.204943, _
17.523144


'Annual Expense and Capital Commitment, Electric Sector, Total Resource Costs
'Transmission
'Billions of 2009 Dollars
'Source: No CPP Reference Case
'        //nem1/Q/output/aeo2017/hm_no_abcpp/d111816a
genr elsec_tran_b = 0
elsec_tran_b.fill(o=1997)     0.000000, 0.000000, 0.000000, 0.000000, 0.000000, _
0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, _
0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 3.184161, 2.614362, _
0.895928, 0.776642, 2.176778, 2.025618, 1.764665, 0.330457, 0.401614, 0.534561, _
0.922066, 0.965095, 1.262528, 1.298888, 1.477757, 0.881228, 0.832817, 0.995845, _
1.684204, 1.022674, 1.325638, 1.741969, 1.857022, 1.625608, 1.602535, 1.967241, _
1.429716, 1.423734, 1.718901, 2.238693, 1.957875, 2.430469, 2.100100, 1.830780, _
1.452288


'Annual Expense and Capital Commitment, Electric Sector, Total Resource Costs
'Retrofits
'Billions of 2009 Dollars
'Source: No CPP Reference Case
'        //nem1/Q/output/aeo2017/hm_no_abcpp/d111816a
genr elsec_retro_b = 0
elsec_retro_b.fill(o=1997)    0.000000, 0.000000, 0.000000, 0.000000, 0.000000, _
0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, _
0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 2.788217, 0.384887, _
0.754495, 1.119881, 0.985467, 0.049630, 0.044029, 0.178723, 0.008469, 0.036819, _
0.000000, 0.714409, 0.617255, 0.000000, 0.101329, 0.000000, 0.000000, 0.034995, _
0.000000, 0.000000, 0.097762, 0.010425, 0.000000, 0.000000, 0.000000, 0.000000, _
0.000000, 0.069716, 0.000000, 0.000000, 0.093017, 0.000000, 0.028163, 0.000000, _
0.037853


endif


'# ==========================================
ENDSUB
'# ==========================================


'# ==========================================
SUBROUTINE io_regional
'# ==========================================


copy {%scenpage}\mc_rev*_1 io_regional\
copy {%scenpage}\mc_emp*_1 emp_regional\
copy {%scenpage}\eg*_1 emp_regional\
copy {%scenpage}\eea_1 emp_regional\
copy {%scenpage}\eeap_1 emp_regional\
copy {%scenpage}\emf_1 emp_regional\

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
'Food Products (Billions of Fixed 2012 Dollars)
        genr revind1_{%r}  = mc_revind2_1*{%r}_3112_outsh + mc_revind3_1*{%r}_3115_outsh _
                                       + mc_revind4_1*{%r}_3116a7_outsh + mc_revind5_1*{%r}_311o_outsh
'Grain and Oil Seed Milling (Billions of Fixed 2012 Dollars)
        genr revind2_{%r} = mc_revind2_1*{%r}_3112_outsh
'Dairy Products (Billions of Fixed 2012 Dollars)
        genr revind3_{%r} = mc_revind3_1*{%r}_3115_outsh
'Meat, Poultry and Seafood Product Processing (Billions of Fixed 2012 Dollars)
        genr revind4_{%r} = mc_revind4_1*{%r}_3116a7_outsh
'Other Food Products (Billions of Fixed 2012 Dollars)
        genr revind5_{%r} = mc_revind5_1*{%r}_311o_outsh
'Beverage and Tobacco Products (Billions of Fixed 2012 Dollars)
        genr revind6_{%r} = mc_revind6_1*{%r}_312_outsh
'Textiles, Apparel, and Leather (Billions of Fixed 2012 Dollars)
        genr revind7_{%r} = mc_revind7_1*{%r}_313t316_outsh
'Wood Products (Billions of Fixed 2012 Dollars)
        genr revind8_{%r} = mc_revind8_1*{%r}_321_outsh
'Furniture and Related Products (Billions of Fixed 2012 Dollars)
        genr revind9_{%r} = mc_revind9_1*{%r}_337_outsh
'Paper Products (Billions of Fixed 2012 Dollars)
        genr revind10_{%r} = mc_revind11_1*{%r}_3221_outsh + mc_revind12_1*{%r}_32221_outsh _
                           + mc_revind13_1*{%r}_322o_outsh
'Pulp and Paper Mills (Billions of Fixed 2012 Dollars)
        genr revind11_{%r} = mc_revind11_1*{%r}_3221_outsh
'Paperboard Containers (Billions of Fixed 2012 Dollars)
        genr revind12_{%r} = mc_revind12_1*{%r}_32221_outsh
'Other Paper Products (Billions of Fixed 2012 Dollars)
        genr revind13_{%r} = mc_revind13_1*{%r}_322o_outsh
'Printing (Billions of Fixed 2012 Dollars)
        genr revind14_{%r} = mc_revind14_1*{%r}_323_outsh
'Basic Inorganic Chemicals (Billions of Fixed 2012 Dollars)
        genr revind15_{%r} = mc_revind15_1*{%r}_32512t8_outsh
'Basic Organic Chemicals (Billions of Fixed 2012 Dollars)
        genr revind16_{%r} = mc_revind16_1*{%r}_32511a9_outsh
'Ethanol (Billions of Fixed 2012 Dollars)
        genr revind17_{%r} = mc_revind17_1*{%r}_325193_outsh
'Resins and Synthetics (Billions of Fixed 2012 Dollars)
        genr revind18_{%r} = mc_revind18_1*{%r}_3252_outsh
'Agricultural Chemicals (Billions of Fixed 2012 Dollars)
        genr revind19_{%r} = mc_revind19_1*{%r}_3253_outsh
'Other Chemical Products Subtotal (Billions of Fixed 2012 Dollars)
        genr revind20_{%r} = mc_revind21_1*{%r}_3254_outsh + mc_revind22_1*{%r}_3255_outsh _
                           + mc_revind23_1*{%r}_3256_outsh + mc_revind24_1*{%r}_325o_outsh
'Pharma Products (Billions of Fixed 2012 Dollars)
        genr revind21_{%r} = mc_revind21_1*{%r}_3254_outsh
'Paint Products (Billions of Fixed 2012 Dollars)
        genr revind22_{%r} = mc_revind22_1*{%r}_3255_outsh
'Soaps and Cleaning Products (Billions of Fixed 2012 Dollars)
        genr revind23_{%r} = mc_revind23_1*{%r}_3256_outsh
'Other Chemical Products (Billions of Fixed 2012 Dollars)
        genr revind24_{%r} = mc_revind24_1*{%r}_325o_outsh
'Petroleum Refining (Billions of Fixed 2012 Dollars)
        genr revind25_{%r} = mc_revind25_1*{%r}_32411_outsh
'Other Petroleum and Coal Products (Billions of Fixed 2012 Dollars)
        genr revind26_{%r} = mc_revind26_1*{%r}_324o_outsh
'Plastics and Rubber Products (Billions of Fixed 2012 Dollars)
        genr revind27_{%r} = mc_revind27_1*{%r}_326_outsh
'Glass and Glass Products (Billions of Fixed 2012 Dollars)
        genr revind28_{%r} = mc_revind28_1*{%r}_3272_outsh
'Flat Glass (Billions of Fixed 2012 Dollars)
        genr revind29_{%r} = mc_revind29_1*{%r}_327211_outsh
'Cement Manufacturing (Billions of Fixed 2012 Dollars)
        genr revind30_{%r} = mc_revind30_1*{%r}_32731_outsh
'Lime Manufacturing (Billions of Fixed 2012 Dollars)
        genr revind31_{%r} = mc_revind31_1*{%r}_3274_outsh
'Other Nonmetallic Mineral Products (Billions of Fixed 2012 Dollars)
        genr revind32_{%r} = mc_revind32_1*{%r}_327oth_outsh
'Iron and Steel Products (Billions of Fixed 2012 Dollars)
        genr revind33_{%r} = mc_revind33_1*{%r}_3311a2_outsh
'Alumina and Aluminum Products (Billions of Fixed 2012 Dollars)
        genr revind34_{%r} = mc_revind34_1*{%r}_3313_outsh
'Other Primary Metals (Billions of Fixed 2012 Dollars)
        genr revind35_{%r} = mc_revind35_1*{%r}_3314a5_outsh
'Fabricated Metal Products (Billions of Fixed 2012 Dollars)
        genr revind36_{%r} = mc_revind36_1*{%r}_332_outsh
'Machinery (Billions of Fixed 2012 Dollars)
        genr revind37_{%r} = mc_revind37_1*{%r}_333_outsh
'Computers and Electronic Products (Billions of Fixed 2012 Dollars)
        genr revind38_{%r} = mc_revind38_1*{%r}_334_outsh
'Transportation Equipment (Billions of Fixed 2012 Dollars)
        genr revind39_{%r} = mc_revind39_1*{%r}_336_outsh
'Appliance and Electrical Equipment (Billions of Fixed 2012 Dollars)
        genr revind40_{%r} = mc_revind40_1*{%r}_335_outsh
'Miscellaneous Manufacturing (Billions of Fixed 2012 Dollars)
        genr revind41_{%r} = mc_revind41_1*{%r}_339_outsh
'Petrochemical Manufacturing (Billions of Chained 2012 U.S. Dollars)
        genr revind49_{%r} = mc_revind49_1*{%r}_32511_outsh
'All Other Basic Organic Chemical Manufacturing (Billions of Chained 2012 U.S. Dollars)
        genr revind50_{%r} = mc_revind50_1*{%r}_32519_outsh
'Industrial Gas Manufacturing (Billions of Chained 2012 U.S. Dollars)
        genr revind51_{%r} = mc_revind51_1*{%r}_32512_outsh
'Synthetic Dye and Pigment and Other Manufacturing (Billions of Chained 2012 U.S. Dollars)
        genr revind52_{%r} = mc_revind52_1*{%r}_32513A8_outsh
'Container Glass (Billions of Chained 2012 U.S. Dollars)
        genr revind53_{%r} = mc_revind53_1*{%r}_327213_outsh
'Glass & Glass Product Manufacturing Excl. Flat and Container Glass (Billions of Chained 2012 U.S. Dollars)
        genr revind54_{%r} = mc_revind54_1*{%r}_327212A5_outsh
'Lime Manufacturing (Billions of Chained 2012 U.S. Dollars)
        genr revind55_{%r} = mc_revind55_1*{%r}_32741_outsh


'Nonmanufacturing
'Crop Production (Billions of Fixed 2012 Dollars)
        genr revind42_{%r} = mc_revind42_1*{%r}_111_outsh
'Animal Production (Billions of Fixed 2012 Dollars)
        genr revind43_{%r} = mc_revind43_1*{%r}_112_outsh
'Other Agriculture (Billions of Fixed 2012 Dollars)
        genr revind44_{%r} = mc_revind44_1*{%r}_113ao_outsh
'Coal Mining (Billions of Fixed 2012 Dollars)
        genr revind45_{%r} = mc_revind45_1*{%r}_2121_outsh
'Oil and Gas Extraction and Support Activities (Billions of Fixed 2012 Dollars)
        genr revind46_{%r} = mc_revind46_1*{%r}_211a3_outsh
'Other Mining and Quarrying (Billions of Fixed 2012 Dollars)
        genr revind47_{%r} = mc_revind47_1*{%r}_2122a3_outsh  
'Construction (Billions of Fixed 2012 Dollars)
        genr revind48_{%r} = mc_revind48_1*{%r}_23_outsh


'Services
'Transportation and Warehousing (Billions of Fixed 2012 Dollars)
        genr revser1_{%r} = mc_revser1_1*{%r}_48a9_outsh
'Broadcasting and Telecommunications (Billions of Fixed 2012 Dollars)
        genr revser2_{%r} = mc_revser2_1*{%r}_515a7_outsh
'Electric Power Generation and Distribution (Billions of Fixed 2012 Dollars)
        genr revser3_{%r} = mc_revser3_1*{%r}_2211_outsh
'Natural Gas Distribution (Billions of Fixed 2012 Dollars)
        genr revser4_{%r} = mc_revser4_1*{%r}_2212_outsh
'Water, Sewage, and Related System (Billions of Fixed 2012 Dollars)
        genr revser5_{%r} = mc_revser5_1*{%r}_2213_outsh
'Wholesale Trade (Billions of Fixed 2012 Dollars)
        genr revser6_{%r} = mc_revser6_1*{%r}_42_outsh
'Retail Trade (Billions of Fixed 2012 Dollars)
        genr revser7_{%r} = mc_revser7_1*{%r}_44a5_outsh
'Finance, Insurance, and Real Estate (Billions of Fixed 2012 Dollars)
        genr revser8_{%r} = mc_revser8_1*{%r}_52a3_outsh
'Other Services (Billions of Fixed 2012 Dollars)
        genr revser9_{%r} = mc_revser9_1*{%r}_5111aserv_outsh
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
   for !x=1 to 55
      gregio.add revind{!x}_{%r}
   next
next

store(d={%wfpathpwd}eviewsdb) {%wfpathpwd}eviewsdb::gregio

delete gregio


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



'US baseline
if giswitch = 0 then
  %giscen = "_0"

endif

'name of US workfile page
%name = @pagename
'sample of US workfile page
%xtabsmpl  = @pagesmpl
'create a new annual page for US forecast
pagecreate(page=xtab_page) a 1990 2050
'return to US workfile page
pageselect {%name}

'create a group holding US forecast
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

'copy group members from US workfile page to the new annual page dropping the extension
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
genr TranLtx  = TranLt


'specify and estimate equations
smpl 1967q1 %growth_yr
equation TranC1eqn.ls (TranC1X/suvtl) C  (((cdmvn+ifnreetlvadj-ifmvpuna)/plvavg) + (.001*suvgov/suvtl))
equation TranC2eqn.ls (TranC2x/suvtl) C D(((cdmvn+ifnreetlvadj-ifmvpuna)/plvavg) + (.001*suvgov/suvtl))
equation TranC3eqn.ls (TranC3x/suvtl) C D(((cdmvn+ifnreetlvadj-ifmvpuna)/plvavg) + (.001*suvgov/suvtl)) time

smpl @all

'add TranClass model equations to m_us2023a model
m_us2023a.append :TranC1eqn
m_us2023a.append :TranC2eqn
m_us2023a.append :TranC3eqn
m_us2023a.addassign(v) TranC1x TranC2x TranC3x
m_us2023a.append @identity TranLtx = TranC1x+TranC2x+TranC3x
m_us2023a.append @identity TranC1  = suvtl*TranC1x/TranLtx
m_us2023a.append @identity TranC2  = suvtl*TranC2x/TranLtx
m_us2023a.append @identity TranC3  = suvtl*TranC3x/TranLtx
m_us2023a.append @identity TranLt  = TranC1+TranC2+TranC3


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
for %cd neng matl enc wnc satl esc wsc mtn pac
  genr wpi051_{%cd} = wpi051_0
next


'Producer price index - gas fuels
for %cd neng matl enc wnc satl esc wsc mtn pac
  genr wpi053_{%cd} = wpi053_0
next


'Producer price index - electric power
for %cd neng matl enc wnc satl esc wsc mtn pac
  genr wpi054_{%cd} = wpi054_0
next


'Producer price index - utility natural gas
for %cd neng matl enc wnc satl esc wsc mtn pac
  genr wpi055_{%cd} = wpi055_0
next


'Producer price index - refined petroleum products
for %cd neng matl enc wnc satl esc wsc mtn pac
  genr wpi057_{%cd} = wpi057_0
next


'Producer price index - residual petroleum fuels
for %cd neng matl enc wnc satl esc wsc mtn pac
  genr wpi0574_{%cd} = wpi0574_0
next


'Producer price index - fuels, related products and power
for %cd neng matl enc wnc satl esc wsc mtn pac
  genr wpi05_{%cd} = wpi05_0
next



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
for %cd neng matl enc wnc satl esc wsc mtn pac
 genr wpi051_{%cd} = @mean(wpi051_{%cd},%growth_meanyr)*{%wfpathpwd}eviewsdb::wpi051_{%cd}_r_0
next


'Producer price index - gas fuels
 for %cd neng matl enc wnc satl esc wsc mtn pac
  genr wpi053_{%cd} = @mean(wpi053_{%cd},%growth_meanyr)*{%wfpathpwd}eviewsdb::wpi053_{%cd}_r_0
 next

 
'Producer price index - electric power
for %cd neng matl enc wnc satl esc wsc mtn pac
 genr wpi054_{%cd} = @mean(wpi054_{%cd},%growth_meanyr)*{%wfpathpwd}eviewsdb::wpi054_{%cd}_r_0
next


'Producer price index - utility natural gas
for %cd neng matl enc wnc satl esc wsc mtn pac
 genr wpi055_{%cd} = @mean(wpi055_{%cd},%growth_meanyr)*{%wfpathpwd}eviewsdb::wpi055_{%cd}_r_0
next


'Producer price index - refined petroleum products
for %cd neng matl enc wnc satl esc wsc mtn pac
 genr wpi057_{%cd} = @mean(wpi057_{%cd},%growth_meanyr)*{%wfpathpwd}eviewsdb::wpi057_{%cd}_r_0
next


'Producer price index - residual petroleum fuels
for %cd neng matl enc wnc satl esc wsc mtn pac
 genr wpi0574_{%cd} = @mean(wpi0574_{%cd},%growth_meanyr)*{%wfpathpwd}eviewsdb::wpi0574_{%cd}_r_0
next


'Producer price index - fuels, related products and power
for %cd neng matl enc wnc satl esc wsc mtn pac
  m_us2023a.append @identity wpi05_{%cd} = wpi05_{%cd}(-1) _
                                         * ((0.0355*wpi051_{%cd}+0.1042*wpi053_{%cd}+0.3011*wpi054_{%cd} _
                                            +0.1590*wpi055_{%cd}+0.0950*wpi0561+0.3053*wpi057_{%cd})     _
                                         /  (0.0355*wpi051_{%cd}(-1)+0.1042*wpi053_{%cd}(-1)             _
                                            +0.3011*wpi054_{%cd}(-1)+0.1590*wpi055_{%cd}(-1)             _
                                            +0.0950*wpi0561(-1)+0.3053*wpi057_{%cd}(-1)))
next


'**********************************************************************
' END OF SUBROUTINE REGWPIPT2
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
%start_fcst_q 	= "2025q1" 'first quarter of the projection period
%end_data_q 	= "2024q4" 'last quarter before start of projection period


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
genr us_index = us_raw/@elem(us_raw, %end_data_q)*100
genr total_us_fcst = us_index

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
                genr {%cd}_{%cft}_fcst = {%cd}_{%cft}_index 'historical data
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
        'smpl 1970q1 2050q4
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

genr np                    = {%wfpathpwd}eviewsdb::np
genr np_sum                = 0.
genr np_sum_adj            = 0.

genr np16a                 = {%wfpathpwd}eviewsdb::np16a
genr np16a_sum             = 0.
genr np16a_sum_adj         = 0.

genr yp                    = {%wfpathpwd}eviewsdb::yp_0
genr yp_sum_adj            = 0.

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

 genr yp_{%reg}_adj        = yp_{%reg}

 genr ypdr_{%reg}_adj      = ypdr_{%reg}

next

cpi_ave                    = cpi_ave/9.

for %reg enc esc matl mtn neng pac satl wnc wsc

 cpi_{%reg}_adj       = cpi_{%reg}_adj*cpi/cpi_ave
 cpi_ave_adj          = cpi_ave_adj+cpi_{%reg}_adj

 np_{%reg}_adj        = np_{%reg}_adj*np/np_sum
 np_sum_adj           = np_sum_adj+np_{%reg}_adj

 np16a_{%reg}_adj     = np16a_{%reg}_adj*np16a/np16a_sum
 np16a_sum_adj        = np16a_sum_adj+np16a_{%reg}_adj

 yp_{%reg}_adj        = yp_{%reg}_adj*yp/yp_sum
 yp_sum_adj           = yp_sum_adj+yp_{%reg}_adj

 ypdr_{%reg}_adj      = ypdr_{%reg}_adj*ypdr/ypdr_sum
 ypdr_sum_adj         = ypdr_sum_adj+ypdr_{%reg}_adj

next

cpi_ave_adj = cpi_ave_adj/9.


' import US model baseline and exogenous variables
genr edrsre      = {%wfpathpwd}eviewsdb::edrsre


smpl @all

' build regional model
model m_regional

for %reg enc esc matl mtn neng pac satl wnc wsc


 m_regional.append @identity ypdr_{%reg}_adj       = ypdr*ypdr_{%reg}/ypdr_sum

 m_regional.append @identity yp_{%reg}_adj         = yp*yp_{%reg}/yp_sum

 m_regional.append @identity cpi_{%reg}           = cpiz_{%reg}*cpi
 m_regional.append @identity cpi_{%reg}_adj       = cpi*cpi_{%reg}/cpi_ave


next

m_regional.append @identity ypdr_sum_adj      = ypdr_enc_adj+ypdr_esc_adj+ypdr_matl_adj+ypdr_mtn_adj _
                                              + ypdr_neng_adj+ypdr_pac_adj+ypdr_satl_adj+ypdr_wnc_adj _
                                              + ypdr_wsc_adj

m_regional.append @identity yp_sum_adj        = yp_enc_adj+yp_esc_adj+yp_matl_adj+yp_mtn_adj+yp_neng_adj _
                                              + yp_pac_adj+yp_satl_adj+yp_wnc_adj+yp_wsc_adj

m_regional.append @identity cpi_ave           = (cpi_enc+cpi_esc+cpi_matl+cpi_mtn _
                                              +  cpi_neng+cpi_pac+cpi_satl+cpi_wnc _
                                              +  cpi_wsc)/9.

m_regional.append @identity cpi_ave_adj       = (cpi_enc_adj+cpi_esc_adj+cpi_matl_adj+cpi_mtn_adj _
                                              +  cpi_neng_adj+cpi_pac_adj+cpi_satl_adj+cpi_wnc_adj _
                                              +  cpi_wsc_adj)/9.


' sim regional model
smpl 2025:1 {%mamlastyr}
m_regional.scenario "baseline"
m_regional.solve

smpl 2025:1 {%mamlastyr}
genr cpi        = {%wfpathpwd}eviewsdb::cpi_1
genr yp         = {%wfpathpwd}eviewsdb::yp_1
genr ypdr       = {%wfpathpwd}eviewsdb::ypdr_1

m_regional.scenario "scenario 1"
m_regional.solve

smpl 2025:1 {%mamlastyr}
m_regional.scenario(n) "scenario 2"
m_regional.solve


' Overwrite housing start/shipment projections with shares developed from U.S. Census Bureau Survey of Construction data; developed in coordination with RDM modelers
for %hous husmfg husps1 husps2a

   smpl @all

   genr {%hous}_sum_0 = 0
   genr {%hous}_sum_1 = 0

   genr {%hous}_0 = {%wfpathpwd}eviewsdb::{%hous}_0
   genr {%hous}_1 = {%wfpathpwd}eviewsdb::{%hous}_1


   smpl 1959:1 %growth_yr

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

' 2020 refers to 2020 RECS base year, the first year RDM runs  !ResShares
   smpl 2020:1 {%mamlastyr}


   for %cd ENC ESC MATL MTN NENG PAC SATL WNC WSC

      {%hous}_{%cd}_0 = {%hous}_0*{%wfpathpwd}eviewsdb::js_{%hous}_{%cd}
      {%hous}_{%cd}_1 = {%hous}_1*{%wfpathpwd}eviewsdb::js_{%hous}_{%cd}

      {%hous}_sum_0 = {%hous}_sum_0 + {%hous}_{%cd}_0 
      {%hous}_sum_1 = {%hous}_sum_1 + {%hous}_{%cd}_1 

   next

next


' Compute projected housing stocks based on start/shipment shares developed from U.S. Census Bureau data; developed in coordination with RDM modelers

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

smpl 2025:1 {%mamlastyr}

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
 group gregmac2
   for %scen 1

      for %cd neng matl enc wnc satl esc wsc mtn pac
         gregmac2.add cpi_{%cd}_adj_{%scen}
         gregmac2.add ypdr_{%cd}_adj_{%scen}
         gregmac2.add yp_{%cd}_adj_{%scen}
         gregmac2.add husmfg_{%cd}_{%scen}
         gregmac2.add husps1_{%cd}_{%scen}
         gregmac2.add husps2a_{%cd}_{%scen}
         gregmac2.add khumfg_{%cd}_{%scen}
         gregmac2.add khups1_{%cd}_{%scen}
         gregmac2.add khups2a_{%cd}_{%scen}
         gregmac2.add np_{%cd}_adj
         gregmac2.add np16a_{%cd}_adj

      next

      gregmac2.add cpi_{%scen}
      gregmac2.add ypdr_{%scen}
      gregmac2.add yp_{%scen}
      gregmac2.add husmfg_{%scen}
      gregmac2.add husps1_{%scen}
      gregmac2.add husps2a_{%scen}
      gregmac2.add khumfg_{%scen}
      gregmac2.add khups1_{%scen}
      gregmac2.add khups2a_{%scen}
      gregmac2.add np
      gregmac2.add np16a

   next


'Store quarterly data in the workfile to database eviewsdb.
 store(d={%wfpathpwd}eviewsdb) {%wfpathpwd}eviewsdb::gregmac2


'**********************************************************************
' END OF SUBROUTINE REGIONAL
'**********************************************************************


'# ==========================================
ENDSUB
'# ==========================================


