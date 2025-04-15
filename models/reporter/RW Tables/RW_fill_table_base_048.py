
# -*- coding: utf-8 -*-
"""
Created on Mon Jan 3 08:30:06 2024

@author: SZO
"""

def fill_table_base_048(dfd, table_spec, table_id):
    """Fill table for Light-Duty Vehicle Sales by Technology Type.
   
    The function returns a dict in which each key is an integer
    references a table row, and each value is a dataframe indexed
    by region number. The integer keys are the same as "irow" in
    the old layin.xls / ftab.f.
 
    Parameters
    ----------
    dfd : dict of restart variables
        key = variable name
        value = pandas series of variable values
 
    Returns
    -------
    dict
        dict of dataframes, with integers as keys. The dict values
        are dataframes indexed by region number.
    
    """
    
    z = {}

    # TST
    # 1
    # Light-Duty Vehicle Sales by Technology Type-	
    # (thousands)---------------------------------	
    #  Technology Type----------------------------	
    # --------------------------------------------	
    # New Car Sales 1/----------------------------	
    #  Conventional Cars--------------------------	
    #    Gasoline ICE Vehicles--------------------	T48(1,IR,IY,IS)=TRLDSALC(1,IR,IY)*1000.
    #    TDI Diesel ICE---------------------------	T48(2,IR,IY,IS)=TRLDSALC(2,IR,IY)*1000.
    #      Total Conventional Cars----------------	T48(3,IR,IY,IS)=T48(1,IR,IY,IS)+T48(2,IR,IY,IS)
    # --------------------------------------------	
    #  Alternative-Fuel Cars----------------------	
    #    Ethanol-Flex Fuel ICE--------------------	T48(5,IR,IY,IS)=TRLDSALC(3,IR,IY)*1000.
    #    100-Mile Electric Vehicle----------------	T48(6,IR,IY,IS)=TRLDSALC(4,IR,IY)*1000.
    #    200-Mile Electric Vehicle----------------	T48(7,IR,IY,IS)=TRLDSALC(7,IR,IY)*1000.
    #    300-Mile Electric Vehicle----------------	T48(15,IR,IY,IS)=TRLDSALC(15,IR,IY)*1000.
    #    Plug-in 20 Gasoline Hybrid---------------	T48(8,IR,IY,IS)=TRLDSALC(5,IR,IY)*1000.
    #    Plug-in 50 Gasoline Hybrid---------------	T48(4,IR,IY,IS)=TRLDSALC(6,IR,IY)*1000.
    #    Electric-Diesel Hybrid-------------------	T48(9,IR,IY,IS)=TRLDSALC(8,IR,IY)*1000.
    #    Electric-Gasoline Hybrid-----------------	T48(10,IR,IY,IS)=TRLDSALC(16,IR,IY)*1000.
    #    Natural Gas ICE--------------------------	T48(11,IR,IY,IS)=TRLDSALC(11,IR,IY)*1000.
    #    Natural Gas Bi-fuel----------------------	T48(12,IR,IY,IS)=TRLDSALC(9,IR,IY)*1000.
    #    Propane ICE------------------------------	T48(13,IR,IY,IS)=TRLDSALC(12,IR,IY)*1000.
    #    Propane Bi-fuel--------------------------	T48(14,IR,IY,IS)=TRLDSALC(10,IR,IY)*1000.
    #    Fuel Cell Methanol-----------------------	T48(16,IR,IY,IS)=TRLDSALC(13,IR,IY)*1000.
    #    Fuel Cell Hydrogen-----------------------	T48(17,IR,IY,IS)=TRLDSALC(14,IR,IY)*1000.
    #      Total Alternative Cars-----------------	T48(18,IR,IY,IS)=FSUM(T48(4,IR,IY,IS),14)
    # --------------------------------------------	
    # Percent Alternative Car Sales---------------	T48(19,IR,IY,IS)=T48(18,IR,IY,IS)/T48(20,IR,IY,IS)*100.
    # Total New Car Sales-------------------------	T48(20,IR,IY,IS)=T48(3,IR,IY,IS)+T48(18,IR,IY,IS)
    # --------------------------------------------	
    # New Light Truck Sales 2/--------------------	
    #  Conventional Light Trucks------------------	
    #    Gasoline ICE Vehicles--------------------	T48(21,IR,IY,IS)=TRLDSALT(1,IR,IY)*1000.
    #    TDI Diesel ICE---------------------------	T48(22,IR,IY,IS)=TRLDSALT(2,IR,IY)*1000.
    #      Total Conventional Light Trucks--------	T48(23,IR,IY,IS)=T48(21,IR,IY,IS)+T48(22,IR,IY,IS)
    # --------------------------------------------	
    #  Alternative-Fuel Light Trucks--------------	
    #    Ethanol-Flex Fuel ICE--------------------	T48(25,IR,IY,IS)=TRLDSALT(3,IR,IY)*1000.
    #    100-Mile Electric Vehicle----------------	T48(26,IR,IY,IS)=TRLDSALT(4,IR,IY)*1000.
    #    200-Mile Electric Vehicle----------------	T48(27,IR,IY,IS)=TRLDSALT(7,IR,IY)*1000.
    #    300-Mile Electric Vehicle----------------	T48(35,IR,IY,IS)=TRLDSALT(15,IR,IY)*1000.
    #    Plug-in 20 Gasoline Hybrid---------------	T48(28,IR,IY,IS)=TRLDSALT(5,IR,IY)*1000.
    #    Plug-in 50 Gasoline Hybrid---------------	T48(24,IR,IY,IS)=TRLDSALT(6,IR,IY)*1000.
    #    Electric-Diesel Hybrid-------------------	T48(29,IR,IY,IS)=TRLDSALT(8,IR,IY)*1000.
    #    Electric-Gasoline Hybrid-----------------	T48(30,IR,IY,IS)=TRLDSALT(16,IR,IY)*1000.
    #    Natural Gas ICE--------------------------	T48(31,IR,IY,IS)=TRLDSALT(11,IR,IY)*1000.
    #    Natural Gas Bi-fuel----------------------	T48(32,IR,IY,IS)=TRLDSALT(9,IR,IY)*1000.
    #    Propane ICE------------------------------	T48(33,IR,IY,IS)=TRLDSALT(12,IR,IY)*1000.
    #    Propane Bi-fuel--------------------------	T48(34,IR,IY,IS)=TRLDSALT(10,IR,IY)*1000.
    #    Fuel Cell Methanol-----------------------	T48(36,IR,IY,IS)=TRLDSALT(13,IR,IY)*1000.
    #    Fuel Cell Hydrogen-----------------------	T48(37,IR,IY,IS)=TRLDSALT(14,IR,IY)*1000.
    #      Total Alternative Light Trucks---------	T48(38,IR,IY,IS)=FSUM(T48(24,IR,IY,IS),14)
    # --------------------------------------------	
    # --------------------------------------------	
    # Percent Alternative Light Truck Sales-------	T48(39,IR,IY,IS)=T48(38,IR,IY,IS)/T48(40,IR,IY,IS)*100.
    # Total New Light Truck Sales-----------------	T48(40,IR,IY,IS)=(T48(23,IR,IY,IS)+T48(38,IR,IY,IS))
    # --------------------------------------------	
    # Percent Total Alternative Sales-------------	T48(41,IR,IY,IS)=LEGALTSAL(1,IR,IY)*100.
    # EPACT Legislative  Alternative Sales--------	T48(42,IR,IY,IS)=LEGALTSAL(2,IR,IY)*1000.
    # ZEVP Legislative Alternative Sales----------	T48(43,IR,IY,IS)=LEGALTSAL(3,IR,IY)*1000.
    # --------------------------------------------	
    # Total Sales, Cars and Light Trucks----------	
    #    Conventional Gasoline--------------------	T48(51,IR,IY,IS)=(TRLDSALC(1,IR,IY)+TRLDSALT(1,IR,IY))*1000.
    #    TDI Diesel-------------------------------	T48(45,IR,IY,IS)=(TRLDSALC(2,IR,IY)+TRLDSALT(2,IR,IY))*1000.
    #    Flex-Fuel--------------------------------	T48(47,IR,IY,IS)=(TRLDSALC(3,IR,IY)+TRLDSALT(3,IR,IY))*1000.
    #    Ethanol----------------------------------	T48(51,IR,IY,IS)=(TRLDSALC(1,IR,IY)+TRLDSALT(1,IR,IY))*1000.
    #    Electric---------------------------------	T48(49,IR,IY,IS)=(TRLDSALC(7,IR,IY)+TRLDSALT(7,IR,IY)+TRLDSALC(4,IR,IY)+TRLDSALT(4,IR,IY)+TRLDSALC(15,IR,IY)+TRLDSALT(15,IR,IY))*1000.
    #    Plug-in Electric Hybrid------------------	T48(53,IR,IY,IS)=(TRLDSALC(5,IR,IY)+TRLDSALT(5,IR,IY)+TRLDSALC(6,IR,IY)+TRLDSALT(6,IR,IY))*1000.
    #    Electric Hybrid--------------------------	T48(46,IR,IY,IS)=(TRLDSALC(8,IR,IY)+TRLDSALT(8,IR,IY)+TRLDSALC(16,IR,IY)+TRLDSALT(16,IR,IY))*1000.
    #    Gaseous (Propane and Natural Gas)--------	T48(48,IR,IY,IS)=(TRLDSALC(9,IR,IY)+TRLDSALT(9,IR,IY)+TRLDSALC(10,IR,IY)+TRLDSALT(10,IR,IY)+TRLDSALC(11,IR,IY)+TRLDSALT(11,IR,IY)+TRLDSALC(12,IR,IY)+TRLDSALT(12,IR,IY))*1000.
    #    Fuel Cell--------------------------------	T48(50,IR,IY,IS)=(TRLDSALC(13,IR,IY)+TRLDSALT(13,IR,IY)+TRLDSALC(14,IR,IY)+TRLDSALT(14,IR,IY))*1000.
    # Total Vehicles Sales------------------------	T48(44,IR,IY,IS)=T48(20,IR,IY,IS)+T48(40,IR,IY,IS)
    # --------------------------------------------	
    # Sales of Microhybrids (engine off at idle)--	
    #    Conventional Gasoline Microhybrids-------	T48(54,IR,IY,IS)=(TRMICROS(1,1,IR,IY)+TRMICROS(2,1,IR,IY))*1000.
    #    TDI Diesel Microhybrids------------------	T48(55,IR,IY,IS)=(TRMICROS(1,2,IR,IY)+TRMICROS(2,2,IR,IY))*1000.
    #    Flex-Fuel--------------------------------	T48(59,IR,IY,IS)=(TRMICROS(1,3,IR,IY)+TRMICROS(2,3,IR,IY))*1000.
    #    Ethanol----------------------------------	T48(55,IR,IY,IS)=(TRMICROS(1,2,IR,IY)+TRMICROS(2,2,IR,IY))*1000.
    #    Electric---------------------------------	T48(61,IR,IY,IS)=(TRMICROS(1,7,IR,IY)+TRMICROS(2,7,IR,IY)+TRMICROS(1,4,IR,IY)+TRMICROS(2,4,IR,IY)+TRMICROS(1,15,IR,IY)+TRMICROS(2,15,IR,IY))*1000.
    #    Plug-in Electric Hybrid------------------	T48(58,IR,IY,IS)=(TRMICROS(1,5,IR,IY)+TRMICROS(2,5,IR,IY)+TRMICROS(1,6,IR,IY)+TRMICROS(2,6,IR,IY))*1000.
    #    Electric Hybrid--------------------------	T48(57,IR,IY,IS)=(TRMICROS(1,8,IR,IY)+TRMICROS(2,8,IR,IY)+TRMICROS(1,16,IR,IY)+TRMICROS(2,16,IR,IY))*1000.
    #    Gaseous----------------------------------	T48(60,IR,IY,IS)=(TRMICROS(1,9,IR,IY)+TRMICROS(2,9,IR,IY)+TRMICROS(1,10,IR,IY)+TRMICROS(2,10,IR,IY)+TRMICROS(1,11,IR,IY)+TRMICROS(2,11,IR,IY)+TRMICROS(1,12,IR,IY)+TRMICROS(2,12,IR,IY))*1000.
    #    Fuel Cell--------------------------------	T48(62,IR,IY,IS)=(TRMICROS(1,13,IR,IY)+TRMICROS(2,13,IR,IY)+TRMICROS(1,14,IR,IY)+TRMICROS(2,14,IR,IY))*1000.
    #       Total Microhybrids--------------------	T48(63,IR,IY,IS)=FSUM(T48(54,IR,IY,IS),9)
    # --------------------------------------------	
    # Total Alternative-Fueled Vehicle Sales------	T48(64,IR,IY,IS)=FSUM(T48(45,IR,IY,IS),9)+T48(54,IR,IY,IS)-T48(51,IR,IY,IS)
    # --------------------------------------------	
    # Credit Bank---------------------------------	
    #    Advanced Technology Partial Zero Emission	T48(65,IR,IY,IS)=ZEV_CREDIT_BANK(IR,1,IY)
    #    Transitional Zero Emission---------------	T48(66,IR,IY,IS)=ZEV_CREDIT_BANK(IR,2,IY)
    #    Zero Emission----------------------------	T48(67,IR,IY,IS)=ZEV_CREDIT_BANK(IR,3,IY)

# New Car Sales 1/
#  Conventional Cars-----------------

    # T48(1,IR,IY,IS)=TRLDSALC(1,IR,IY)*1000.
    z[1] = dfd['TRLDSALC'].loc[1] * 1000.

    # T48(2,IR,IY,IS)=TRLDSALC(2,IR,IY)*1000.
    z[2] = dfd['TRLDSALC'].loc[2] * 1000.

    # T48(3,IR,IY,IS)=T48(1,IR,IY,IS)+T48(2,IR,IY,IS)
    z[3] = z[1] + z[2]


#  Alternative-Fuel Cars----------------
    # T48(5,IR,IY,IS)=TRLDSALC(3,IR,IY)*1000.
    z[5] = dfd['TRLDSALC'].loc[3] * 1000.

    # T48(6,IR,IY,IS)=TRLDSALC(4,IR,IY)*1000.
    z[6] = dfd['TRLDSALC'].loc[4] * 1000.

    # T48(7,IR,IY,IS)=TRLDSALC(7,IR,IY)*1000.
    z[7] = dfd['TRLDSALC'].loc[7] * 1000.

    # T48(15,IR,IY,IS)=TRLDSALC(15,IR,IY)*1000.
    z[15] = dfd['TRLDSALC'].loc[15] * 1000.

    # T48(8,IR,IY,IS)=TRLDSALC(5,IR,IY)*1000.
    z[8] = dfd['TRLDSALC'].loc[5] * 1000.

    # T48(4,IR,IY,IS)=TRLDSALC(6,IR,IY)*1000.
    z[4] = dfd['TRLDSALC'].loc[6] * 1000.

    # T48(9,IR,IY,IS)=TRLDSALC(8,IR,IY)*1000.
    z[9] = dfd['TRLDSALC'].loc[8] * 1000.

    # T48(10,IR,IY,IS)=TRLDSALC(16,IR,IY)*1000.
    z[10] = dfd['TRLDSALC'].loc[16] * 1000.

    # T48(11,IR,IY,IS)=TRLDSALC(11,IR,IY)*1000.
    z[11] = dfd['TRLDSALC'].loc[11] * 1000.

    # T48(12,IR,IY,IS)=TRLDSALC(9,IR,IY)*1000.
    z[12] = dfd['TRLDSALC'].loc[9] * 1000.

    # T48(13,IR,IY,IS)=TRLDSALC(12,IR,IY)*1000.
    z[13] = dfd['TRLDSALC'].loc[12] * 1000.

    # T48(14,IR,IY,IS)=TRLDSALC(10,IR,IY)*1000.
    z[14] = dfd['TRLDSALC'].loc[10] * 1000.

    # T48(16,IR,IY,IS)=TRLDSALC(13,IR,IY)*1000.
    z[16] = dfd['TRLDSALC'].loc[13] * 1000.

    # T48(17,IR,IY,IS)=TRLDSALC(14,IR,IY)*1000.
    z[17] = dfd['TRLDSALC'].loc[14] * 1000.

    # T48(18,IR,IY,IS)=FSUM(T48(4,IR,IY,IS),14)
    z[18] = z[4] + z[5] + z[6] + z[7] + z[8] + z[9] + z[10] + z[11] + z[12] + z[13] + z[14] + z[15] + z[16] + z[17]

# ----------------    
    # T48(20,IR,IY,IS)=T48(3,IR,IY,IS)+T48(18,IR,IY,IS)
    z[20] = z[3] + z[18]

    # T48(19,IR,IY,IS)=T48(18,IR,IY,IS)/T48(20,IR,IY,IS)*100.
    z[19] = z[18] / z[20] * 100.


    # # T48(20,IR,IY,IS)=T48(3,IR,IY,IS)+T48(18,IR,IY,IS)
    # z[20] = z[3] + z[18]


# New Light Truck Sales 2/--------------
    # T48(21,IR,IY,IS)=TRLDSALT(1,IR,IY)*1000.
    z[21] = dfd['TRLDSALT'].loc[1] * 1000.

    # T48(22,IR,IY,IS)=TRLDSALT(2,IR,IY)*1000.
    z[22] = dfd['TRLDSALT'].loc[2] * 1000.

    # T48(23,IR,IY,IS)=T48(21,IR,IY,IS)+T48(22,IR,IY,IS)
    z[23] =  z[21] + z[22]


#  Alternative-Fuel Light Trucks----------------
    # T48(25,IR,IY,IS)=TRLDSALT(3,IR,IY)*1000.
    z[25] = dfd['TRLDSALT'].loc[3] * 1000.

    # T48(26,IR,IY,IS)=TRLDSALT(4,IR,IY)*1000.
    z[26] = dfd['TRLDSALT'].loc[4] * 1000.

    # T48(27,IR,IY,IS)=TRLDSALT(7,IR,IY)*1000.
    z[27] = dfd['TRLDSALT'].loc[7] * 1000.

    # T48(35,IR,IY,IS)=TRLDSALT(15,IR,IY)*1000.
    z[35] = dfd['TRLDSALT'].loc[15] * 1000.

    # T48(28,IR,IY,IS)=TRLDSALT(5,IR,IY)*1000.
    z[28] = dfd['TRLDSALT'].loc[5] * 1000.

    # T48(24,IR,IY,IS)=TRLDSALT(6,IR,IY)*1000.
    z[24] = dfd['TRLDSALT'].loc[6] * 1000.

    # T48(29,IR,IY,IS)=TRLDSALT(8,IR,IY)*1000.
    z[29] = dfd['TRLDSALT'].loc[8] * 1000.

    # T48(30,IR,IY,IS)=TRLDSALT(16,IR,IY)*1000.
    z[30] = dfd['TRLDSALT'].loc[16] * 1000.

    # T48(31,IR,IY,IS)=TRLDSALT(11,IR,IY)*1000.
    z[31] = dfd['TRLDSALT'].loc[11] * 1000.

    # T48(32,IR,IY,IS)=TRLDSALT(9,IR,IY)*1000.
    z[32] = dfd['TRLDSALT'].loc[9] * 1000.

    # T48(33,IR,IY,IS)=TRLDSALT(12,IR,IY)*1000.
    z[33] = dfd['TRLDSALT'].loc[12] * 1000.

    # T48(34,IR,IY,IS)=TRLDSALT(10,IR,IY)*1000.
    z[34] = dfd['TRLDSALT'].loc[10] * 1000.

    # T48(36,IR,IY,IS)=TRLDSALT(13,IR,IY)*1000.
    z[36] = dfd['TRLDSALT'].loc[13] * 1000.

    # T48(37,IR,IY,IS)=TRLDSALT(14,IR,IY)*1000.
    z[37] = dfd['TRLDSALT'].loc[14] * 1000.

    # T48(38,IR,IY,IS)=FSUM(T48(24,IR,IY,IS),14)
    z[38] = z[24] + z[25] + z[26] + z[27] + z[28] + z[29] + z[30] + z[31] + z[32] + z[33] + z[34] + z[35] + z[36] + z[37]

# -----------------   
    # T48(40,IR,IY,IS)=(T48(23,IR,IY,IS)+T48(38,IR,IY,IS))
    z[40] = z[23] + z[38]

    # T48(39,IR,IY,IS)=T48(38,IR,IY,IS)/T48(40,IR,IY,IS)*100.
    z[39] = z[38] / z[40] * 100.


    # # T48(40,IR,IY,IS)=(T48(23,IR,IY,IS)+T48(38,IR,IY,IS))
    # z[40] = z[23] + z[38]

# ---------------
    # T48(41,IR,IY,IS)=LEGALTSAL(1,IR,IY)*100.
    z[41] = dfd['LEGALTSAL'].loc[1] * 100.

    # T48(42,IR,IY,IS)=LEGALTSAL(2,IR,IY)*1000.
    z[42] = dfd['LEGALTSAL'].loc[2] * 1000.

    # T48(43,IR,IY,IS)=LEGALTSAL(3,IR,IY)*1000.
    z[43] = dfd['LEGALTSAL'].loc[3] * 1000.


# Total Sales, Cars and Light Trucks--------------
    # T48(51,IR,IY,IS)=(TRLDSALC(1,IR,IY)+TRLDSALT(1,IR,IY))*1000.
    #   Conventional Gasoline
    z[51] = (dfd['TRLDSALC'].loc[1] + dfd['TRLDSALT'].loc[1] )* 1000.

    # T48(45,IR,IY,IS)=(TRLDSALC(2,IR,IY)+TRLDSALT(2,IR,IY))*1000.
    #   TDI Diesel
    z[45] = (dfd['TRLDSALC'].loc[2] + dfd['TRLDSALT'].loc[2]) * 1000.

    # T48(47,IR,IY,IS)=(TRLDSALC(3,IR,IY)+TRLDSALT(3,IR,IY))*1000.
    #   Flex-Fuel
    z[47] = (dfd['TRLDSALC'].loc[3] + dfd['TRLDSALT'].loc[3]) * 1000.

   
    # Ethanol (not reported)
    z[52] = z[47] *0.0 

    # T48(49,IR,IY,IS)=(TRLDSALC(7,IR,IY)+TRLDSALT(7,IR,IY)+TRLDSALC(4,IR,IY)
    # +TRLDSALT(4,IR,IY)+TRLDSALC(15,IR,IY)+TRLDSALT(15,IR,IY))*1000.
    #   Electric
    z[49] = (dfd['TRLDSALC'].loc[7] + dfd['TRLDSALT'].loc[7] + dfd['TRLDSALC'].loc[4] 
            + dfd['TRLDSALT'].loc[4] + dfd['TRLDSALC'].loc[15] + dfd['TRLDSALT'].loc[15]) * 1000.

    # T48(53,IR,IY,IS)=(TRLDSALC(5,IR,IY)+TRLDSALT(5,IR,IY)+TRLDSALC(6,IR,IY)+TRLDSALT(6,IR,IY))*1000.
    #   Plug-in Electric Hybrid
    z[53] = (dfd['TRLDSALC'].loc[5] + dfd['TRLDSALT'].loc[5] + dfd['TRLDSALC'].loc[6] + dfd['TRLDSALT'].loc[6]) * 1000.

    # T48(46,IR,IY,IS)=(TRLDSALC(8,IR,IY)+TRLDSALT(8,IR,IY)+TRLDSALC(16,IR,IY)+TRLDSALT(16,IR,IY))*1000.
    #   Electric Hybrid
    z[46] = (dfd['TRLDSALC'].loc[8] + dfd['TRLDSALT'].loc[8] + dfd['TRLDSALC'].loc[16] + dfd['TRLDSALT'].loc[16]) * 1000.

    # T48(48,IR,IY,IS)=(TRLDSALC(9,IR,IY)+TRLDSALT(9,IR,IY)+TRLDSALC(10,IR,IY)+TRLDSALT(10,IR,IY)
    # +TRLDSALC(11,IR,IY)+TRLDSALT(11,IR,IY)+TRLDSALC(12,IR,IY)+TRLDSALT(12,IR,IY))*1000.
    #Gaseous
    z[48] = (dfd['TRLDSALC'].loc[9] + dfd['TRLDSALT'].loc[9] + dfd['TRLDSALC'].loc[10] + dfd['TRLDSALT'].loc[10] 
            + dfd['TRLDSALC'].loc[11] + dfd['TRLDSALT'].loc[11] + dfd['TRLDSALC'].loc[12] + dfd['TRLDSALT'].loc[12]) * 1000.

    # T48(50,IR,IY,IS)=(TRLDSALC(13,IR,IY)+TRLDSALT(13,IR,IY)+TRLDSALC(14,IR,IY)+TRLDSALT(14,IR,IY))*1000.
    #Fuel Celll
    z[50] = (dfd['TRLDSALC'].loc[13] + dfd['TRLDSALT'].loc[13] + dfd['TRLDSALC'].loc[14] + dfd['TRLDSALT'].loc[14]) * 1000.

    # T48(44,IR,IY,IS)=T48(20,IR,IY,IS)+T48(40,IR,IY,IS)
    #Total Vehicles Sales
    z[44] = z[20] + z[40]


# Sales of Microhybrids (engine off at idle)------------------
    #Conventional Gasoline Microhybrids
    # T48(54,IR,IY,IS)=(TRMICROS(1,1,IR,IY)+TRMICROS(2,1,IR,IY))*1000.
    z[54] = (dfd['TRMICROS'].loc[(1,1), :] + dfd['TRMICROS'].loc[(2,1), :]) * 1000.

    # T48(55,IR,IY,IS)=(TRMICROS(1,2,IR,IY)+TRMICROS(2,2,IR,IY))*1000.
    # TDI Diesel Microhybrids
    z[55] = (dfd['TRMICROS'].loc[(1,2), :] + dfd['TRMICROS'].loc[(2,2), :]) * 1000.

    # T48(59,IR,IY,IS)=(TRMICROS(1,3,IR,IY)+TRMICROS(2,3,IR,IY))*1000.
    #    Flex-Fuel
    z[59] = (dfd['TRMICROS'].loc[(1,3), :] + dfd['TRMICROS'].loc[(2,3), :]) * 1000.

    #Ethanol (not reported)
    z[56] = z[59] * 0.0 

    # T48(61,IR,IY,IS)=(TRMICROS(1,7,IR,IY)+TRMICROS(2,7,IR,IY)+TRMICROS(1,4,IR,IY)
    # +TRMICROS(2,4,IR,IY)+TRMICROS(1,15,IR,IY)+TRMICROS(2,15,IR,IY))*1000.
    #   Electric
    z[61] = (dfd['TRMICROS'].loc[(1,7), :] + dfd['TRMICROS'].loc[(2,7), :] + dfd['TRMICROS'].loc[1,4] 
            + dfd['TRMICROS'].loc[(2,4), :] + dfd['TRMICROS'].loc[(1,15), :] + dfd['TRMICROS'].loc[(2,15), :]) * 1000.

    # T48(58,IR,IY,IS)=(TRMICROS(1,5,IR,IY)+TRMICROS(2,5,IR,IY)+TRMICROS(1,6,IR,IY)+TRMICROS(2,6,IR,IY))*1000.
    #   Plug-in Electric Hybrid
    z[58] = (dfd['TRMICROS'].loc[(1,5), :] + dfd['TRMICROS'].loc[(2,5), :] 
            + dfd['TRMICROS'].loc[(1,6), :] + dfd['TRMICROS'].loc[(2,6), :]) * 1000.

    # T48(57,IR,IY,IS)=(TRMICROS(1,8,IR,IY)+TRMICROS(2,8,IR,IY)+TRMICROS(1,16,IR,IY)+TRMICROS(2,16,IR,IY))*1000.
    #   Electric Hybrid
    z[57] = (dfd['TRMICROS'].loc[(1,8), :] + dfd['TRMICROS'].loc[(2,8), :] 
            + dfd['TRMICROS'].loc[(1,16), :] + dfd['TRMICROS'].loc[(2,16), :]) * 1000.

    # T48(60,IR,IY,IS)=(TRMICROS(1,9,IR,IY)+TRMICROS(2,9,IR,IY)+TRMICROS(1,10,IR,IY)+TRMICROS(2,10,IR,IY)
    # +TRMICROS(1,11,IR,IY)+TRMICROS(2,11,IR,IY)+TRMICROS(1,12,IR,IY)+TRMICROS(2,12,IR,IY))*1000.
    #Gaseous
    z[60] = (dfd['TRMICROS'].loc[(1,9), :] + dfd['TRMICROS'].loc[(2,9), :] + dfd['TRMICROS'].loc[(1,10), :] 
            + dfd['TRMICROS'].loc[(2,10), :] + dfd['TRMICROS'].loc[(1,11), :] + dfd['TRMICROS'].loc[(2,11), :] 
            + dfd['TRMICROS'].loc[(1,12), :] + dfd['TRMICROS'].loc[(2,12), :]) * 1000.

    # T48(62,IR,IY,IS)=(TRMICROS(1,13,IR,IY)+TRMICROS(2,13,IR,IY)+TRMICROS(1,14,IR,IY)+TRMICROS(2,14,IR,IY))*1000.
    #   Gaseous
    z[62] = (dfd['TRMICROS'].loc[(1,13), :] + dfd['TRMICROS'].loc[(2,13), :] 
            + dfd['TRMICROS'].loc[(1,14), :] + dfd['TRMICROS'].loc[(2,14), :]) * 1000.

    # T48(63,IR,IY,IS)=FSUM(T48(54,IR,IY,IS),9)
    #      Total Microhybrids
    z[63] = z[54] + z[55] + z[57] + z[58] + z[59] + z[60] + z[61] + z[62] +z[56]


# Total Alternative-Fueled Vehicle Sales------------
    # T48(64,IR,IY,IS)=FSUM(T48(45,IR,IY,IS),9)+T48(54,IR,IY,IS)-T48(51,IR,IY,IS)
 
    z[64] = z[45] + z[46] + z[47] + z[48] + z[49] + z[50] + z[52] + z[53]


    # Credit Bank---------------------
    # T48(65,IR,IY,IS)=ZEV_CREDIT_BANK(IR,1,IY)
    # MNUMC2	MAXZEV 9 3
    # '''Below variables have only 9 regions, make padding by filling
    #  region 10 with 0
    #  region 11 with sum of region 1 to 9'''
    # TODO: check should be sum or average?
    # 
    df = dfd['ZEV_CREDIT_BANK'].loc[(slice(None), 1), :]
    df = df.reset_index(level=1, drop=True)
    df.loc[10] = 0
    df.loc[11] = df.sum()
    z[65] = df #* 1000.


    # T48(66,IR,IY,IS)=ZEV_CREDIT_BANK(IR,2,IY)
    df = dfd['ZEV_CREDIT_BANK'].loc[(slice(None), 2), :]
    df = df.reset_index(level=1, drop=True)
    df.loc[10] = 0
    df.loc[11] = df.sum()
    z[66] = df #* 1000.

    # T48(67,IR,IY,IS)=ZEV_CREDIT_BANK(IR,3,IY)
    df = dfd['ZEV_CREDIT_BANK'].loc[(slice(None), 3), :]
    df = df.reset_index(level=1, drop=True)
    df.loc[10] = 0
    df.loc[11] = df.sum()
    z[67] = df #* 1000.


    return z













