# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_075(dfd, table_spec, table_id):
    """Fill table for Average Technology Cost for Light-Duty Vehicles

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

    def calc_avg_cost(M3, TRTECH):
        # TODO: Remove Hard-coded years
        df = (
            dfd["AVGCOST"].loc[M3].loc[TRTECH]
            / dfd["MC_JPGDP"].loc[table_spec["first_year"]]
            * dfd["MC_JPGDP"].loc[table_spec["growth_rate_start"]]
        )
        # Convert the DataFrame into a dictionary with index as keys and rows as DataFrames
        df_d = {idx: df.loc[[idx]] for idx in df.index}
        return df_d

    i = 1
    j = 86
    TRTECH = range(i, j + 1)

    # # Car [1, 1:86]
    M3 = 1
    z_car = calc_avg_cost(M3, TRTECH)

    # # Light Truck [2, 1:86]
    M3 = 2
    z_lt = calc_avg_cost(M3, TRTECH)
    # Update dictionary with each key increased by 86
    z_lt = {key + 86: value for key, value in z_lt.items()}

    # #  Light-Duty Vehicle Total [3, 1:86]
    M3 = 3
    z_ldv = calc_avg_cost(M3, TRTECH)
    # Update dictionary with each key increased by 2*86
    z_ldv = {key + 2 * 86: value for key, value in z_ldv.items()}

    # Combine Car, Light Truck, and Light-Duty Vehicle
    z.update(z_car)
    z.update(z_lt)
    z.update(z_ldv)

    ## need to put AVGCOST in the preprocessor ###

    #   Average Technology Cost for Light-Duty Vehicles
    #   (2000 dollars)
    #    Technology
    #
    #   Car
    #      Unit Body Construction                                   T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[1] =

    #      Mass Reduction I:  1.5%                                  T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[2] =

    #      Mass Reduction II:  3.5%                                 T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[3] =

    #      Mass Reduction III:  10%                                 T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[4] =

    #      Mass Reduction IV:  15%                                  T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[5] =

    #      Mass Reduction V:  20%                                   T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[6] =

    #      Aero I-10% Coefficient of Drag Reduction                 T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[7] =

    #      Aero II-20% Coefficient of Drag Reduction                T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[8] =

    #      6 Speed Manual                                           T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[9] =

    #      Aggressive Shift Logic I                                 T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[10] =

    #      Aggressive Shift Logic II                                T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[11] =

    #      Early Torque Converter Lockup                            T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[12] =

    #      High Efficiency Gearbox                                  T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[13] =

    #      5 Speed Automatic                                        T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[14] =

    #      6 Speed Automatic                                        T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[15] =

    #      7 Speed Automatic                                        T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[16] =

    #      8 Speed Automatic                                        T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[17] =

    #      Dual Clutch Automated Manual                             T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[18] =

    #      Continuously Variable Transmission                       T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[19] =

    #      Low Friction Lubricants                                  T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[20] =

    #      Engine Friction Reduction I-4 Cylinder                   T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[21] =

    #      Engine Friction Reduction I-6 Cylinder                   T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[22] =

    #      Engine Friction Reduction I-8 Cylinder                   T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[23] =

    #      Engine Friction Reduction II-4 Cylinder                  T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[24] =

    #      Engine Friction Reduction II-6 Cylinder                  T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[25] =

    #      Engine Friction Reduction II-8 Cylinder                  T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[26] =

    #      Cylinder Deactivation-6 Cylinder                         T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[27] =

    #      Cylinder Deactivation-8 Cylinder                         T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[28] =

    #      VVT I-OHV Intake Cam Phasing-6 Cylinder                  T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[29] =

    #      VVT I-OHV Intake Cam Phasing-8 Cylinder                  T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[30] =

    #      VVT I-OHC Intake Cam Phasing-4 Cylinder                  T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[31] =

    #      VVT I-OHC Intake Cam Phasing-6 Cylinder                  T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[32] =

    #      VVT I-OHC Intake Cam Phasing-8 Cylinder                  T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[33] =

    #      VVT II-OHV Coupled Cam Phasing-6 Cylinder                T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[34] =

    #      VVT II-OHV Coupled Cam Phasing-8 Cylinder                T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[35] =

    #      VVT II-OHC Coupled Cam Phasing-4 Cylinder                T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[36] =

    #      VVT II-OHC Coupled Cam Phasing-6 Cylinder                T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[37] =

    #      VVT II-OHC Coupled Cam Phasing-8 Cylinder                T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[38] =

    #      VVT III-OHV Dual Cam Phasing-6 Cylinder                  T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[39] =

    #      VVT III-OHV Dual Cam Phasing-8 Cylinder                  T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[40] =

    #      VVT III-OHC Dual Cam Phasing-4 Cylinder                  T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[41] =

    #      VVT III-OHC Dual Cam Phasing-6 Cylinder                  T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[42] =

    #      VVT III-OHC Dual Cam Phasing-8 Cylinder                  T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[43] =

    #      VVL I-OHV Discrete-6 Cylinder                            T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[44] =

    #      VVL I-OHV Discrete-8 Cylinder                            T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[45] =

    #      VVL I-OHC Discrete-4 Cylinder                            T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[46] =

    #      VVL I-OHC Discrete-6 Cylinder                            T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[47] =

    #      VVL I-OHC Discrete-8 Cylinder                            T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[48] =

    #      VVL II-OHV Continuous-6 Cylinder                         T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[49] =

    #      VVL II-OHV Continuous-8 Cylinder                         T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[50] =

    #      VVL II-OHC Continuous-4 Cylinder                         T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[51] =

    #      VVL II-OHC Continuous-6 Cylinder                         T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[52] =

    #      VVL II-OHC Continuous-8 Cylinder                         T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[53] =

    #      Stoichiometric GDI-4 Cylinder                            T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[54] =

    #      Stoichiometric GDI-6 Cylinder                            T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[55] =

    #      Stoichiometric GDI-8 Cylinder                            T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[56] =

    #      OHV to DOHC TBDS I-I4 (from V6), VVT, VVL, SGDI          T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[57] =

    #      OHV to DOHC TBDS I-V6 (from V8), VVT, VVL, SGDI          T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[58] =

    #      SOHC to DOHC TBDS I-I4 (from V6), VVT, VVL, SGDI         T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[59] =

    #      SOHC to DOHC TBDS I-V6 (from V8), VVT, VVL, SGDI         T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[60] =

    #      DOHC TBDS I-I3 (from I4), VVT, VVL, SGDI                 T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[61] =

    #      DOHC TBDS I-I4 (from V6), VVT, VVL, SGDI                 T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[62] =

    #      DOHC TBDS I-V6 (from V8), VVT, VVL, SGDI                 T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[63] =

    #      OHV to DOHC TBDS II-I4 (from V6), VVT, VVL, SGDI         T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[64] =

    #      OHV to DOHC TBDS II-V6 (from V8), VVT, VVL, SGDI         T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[65] =

    #      SOHC to DOHC TBDS II-I4 (from V6), VVT, VVL, SGDI        T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[66] =

    #      SOHC to DOHC TBDS II-V6 (from V8), VVT, VVL, SGDI        T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[67] =

    #      DOHC TBDS II-I3 (from I4), VVT, VVL, SGDI                T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[68] =

    #      DOHC TBDS II-I4 (from V6), VVT, VVL, SGDI                T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[69] =

    #      DOHC TBDS II-V6 (from V8), VVT, VVL, SGDI                T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[70] =

    #      OHV to DOHC TBDS III-I4 (from V6), VVT, VVL, SGDI, EGR   T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[71] =

    #      OHV to DOHC TBDS III-I4 (from V8), VVT, VVL, SGDI, EGR   T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[72] =

    #      SOHC to DOHC TBDS III-I4 (from V6), VVT, VVL, SGDI, EGR  T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[73] =

    #      SOHC to DOHC TBDS III-I4 (from V8), VVT, VVL, SGDI, EGR  T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[74] =

    #      DOHC TBDS III-I3 (from I4), VVT, VVL, SGDI, EGR          T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[75] =

    #      DOHC TBDS III-I4 (from V6), VVT, VVL, SGDI, EGR          T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[76] =

    #      DOHC TBDS III-I4 (from V8), VVT, VVL, SGDI, EGR          T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[77] =

    #      Electric Power Steering                                  T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[78] =

    #      Improved Accessories I                                   T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[79] =

    #      12V Micro Hybrid with EPS and IACC                       T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[80] =

    #      Improved Accessories II                                  T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[81] =

    #      ISG Mild Hybrid with EPS and IACC                        T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[82] =

    #      Tires I-10% Coefficient of Rolling Resistance Reduction  T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[83] =

    #      Tires II-20% Coefficient of Rolling Resistance Reduction T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[84] =

    #      Low Drag Brakes                                          T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[85] =

    #      Secondary Axle Disconnect                                T75(1:86,IY,IS)=AVGCOST(1,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[86] =

    #
    #   Light Truck
    #      Unit Body Construction                                   T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[87] =

    #      Mass Reduction I:  1.5%                                  T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[88] =

    #      Mass Reduction II:  7.5%                                 T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[89] =

    #      Mass Reduction III:  10%                                 T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[90] =

    #      Mass Reduction IV:  15%                                  T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[91] =

    #      Mass Reduction V:  20%                                   T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[92] =

    #      Aero I-10% Coefficient of Drag Reduction                 T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[93] =

    #      Aero II-20% Coefficient of Drag Reduction                T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[94] =

    #      6 Speed Manual                                           T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[95] =

    #      Aggressive Shift Logic I                                 T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[96] =

    #      Aggressive Shift Logic II                                T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[97] =

    #      Early Torque Converter Lockup                            T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[98] =

    #      High Efficiency Gearbox                                  T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[99] =

    #      5 Speed Automatic                                        T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[100] =

    #      6 Speed Automatic                                        T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[101] =

    #      7 Speed Automatic                                        T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[102] =

    #      8 Speed Automatic                                        T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[103] =

    #      Dual Clutch Automated Manual                             T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[104] =

    #      Continuously Variable Transmission                       T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[105] =

    #      Low Friction Lubricants                                  T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[106] =

    #      Engine Friction Reduction I-4 Cylinder                   T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[107] =

    #      Engine Friction Reduction I-6 Cylinder                   T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[108] =

    #      Engine Friction Reduction I-8 Cylinder                   T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[109] =

    #      Engine Friction Reduction II-4 Cylinder                  T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[110] =

    #      Engine Friction Reduction II-6 Cylinder                  T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[111] =

    #      Engine Friction Reduction II-8 Cylinder                  T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[112] =

    #      Cylinder Deactivation-6 Cylinder                         T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[113] =

    #      Cylinder Deactivation-8 Cylinder                         T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[114] =

    #      VVT I-OHV Intake Cam Phasing-6 Cylinder                  T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[115] =

    #      VVT I-OHV Intake Cam Phasing-8 Cylinder                  T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[116] =

    #      VVT I-OHC Intake Cam Phasing-4 Cylinder                  T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[117] =

    #      VVT I-OHC Intake Cam Phasing-6 Cylinder                  T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[118] =

    #      VVT I-OHC Intake Cam Phasing-8 Cylinder                  T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[119] =

    #      VVT II-OHV Coupled Cam Phasing-6 Cylinder                T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[120] =

    #      VVT II-OHV Coupled Cam Phasing-8 Cylinder                T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[121] =

    #      VVT II-OHC Coupled Cam Phasing-4 Cylinder                T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[122] =

    #      VVT II-OHC Coupled Cam Phasing-6 Cylinder                T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[123] =

    #      VVT II-OHC Coupled Cam Phasing-8 Cylinder                T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[124] =

    #      VVT III-OHV Dual Cam Phasing-6 Cylinder                  T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[125] =

    #      VVT III-OHV Dual Cam Phasing-8 Cylinder                  T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[126] =

    #      VVT III-OHC Dual Cam Phasing-4 Cylinder                  T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[127] =

    #      VVT III-OHC Dual Cam Phasing-6 Cylinder                  T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[128] =

    #      VVT III-OHC Dual Cam Phasing-8 Cylinder                  T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[129] =

    #      VVL I-OHV Discrete-6 Cylinder                            T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[130] =

    #      VVL I-OHV Discrete-8 Cylinder                            T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[131] =

    #      VVL I-OHC Discrete-4 Cylinder                            T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[132] =

    #      VVL I-OHC Discrete-6 Cylinder                            T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[133] =

    #      VVL I-OHC Discrete-8 Cylinder                            T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[134] =

    #      VVL II-OHV Continuous-6 Cylinder                         T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[135] =

    #      VVL II-OHV Continuous-8 Cylinder                         T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[136] =

    #      VVL II-OHC Continuous-4 Cylinder                         T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[137] =

    #      VVL II-OHC Continuous-6 Cylinder                         T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[138] =

    #      VVL II-OHC Continuous-8 Cylinder                         T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[139] =

    #      Stoichiometric GDI-4 Cylinder                            T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[140] =

    #      Stoichiometric GDI-6 Cylinder                            T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[141] =

    #      Stoichiometric GDI-8 Cylinder                            T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[142] =

    #      OHV to DOHC TBDS I-I4 (from V6), VVT, VVL, SGDI          T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[143] =

    #      OHV to DOHC TBDS I-V6 (from V8), VVT, VVL, SGDI          T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[144] =

    #      SOHC to DOHC TBDS I-I4 (from V6), VVT, VVL, SGDI         T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[145] =

    #      SOHC to DOHC TBDS I-V6 (from V8), VVT, VVL, SGDI         T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[146] =

    #      DOHC TBDS I-I3 (from I4), VVT, VVL, SGDI                 T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[147] =

    #      DOHC TBDS I-I4 (from V6), VVT, VVL, SGDI                 T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[148] =

    #      DOHC TBDS I-V6 (from V8), VVT, VVL, SGDI                 T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[149] =

    #      OHV to DOHC TBDS II-I4 (from V6), VVT, VVL, SGDI         T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[150] =

    #      OHV to DOHC TBDS II-V6 (from V8), VVT, VVL, SGDI         T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[151] =

    #      SOHC to DOHC TBDS II-I4 (from V6), VVT, VVL, SGDI        T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[152] =

    #      SOHC to DOHC TBDS II-V6 (from V8), VVT, VVL, SGDI        T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[153] =

    #      DOHC TBDS II-I3 (from I4), VVT, VVL, SGDI                T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[154] =

    #      DOHC TBDS II-I4 (from V6), VVT, VVL, SGDI                T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[155] =

    #      DOHC TBDS II-V6 (from V8), VVT, VVL, SGDI                T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[156] =

    #      OHV to DOHC TBDS III-I4 (from V6), VVT, VVL, SGDI, EGR   T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[157] =

    #      OHV to DOHC TBDS III-I4 (from V8), VVT, VVL, SGDI, EGR   T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[158] =

    #      SOHC to DOHC TBDS III-I4 (from V6), VVT, VVL, SGDI, EGR  T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[159] =

    #      SOHC to DOHC TBDS III-I4 (from V8), VVT, VVL, SGDI, EGR  T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[160] =

    #      DOHC TBDS III-I3 (from I4), VVT, VVL, SGDI, EGR          T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[161] =

    #      DOHC TBDS III-I4 (from V6), VVT, VVL, SGDI, EGR          T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[162] =

    #      DOHC TBDS III-I4 (from V8), VVT, VVL, SGDI, EGR          T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[163] =

    #      Electric Power Steering                                  T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[164] =

    #      Improved Accessories I                                   T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[165] =

    #      12V Micro Hybrid with EPS and IACC                       T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[166] =

    #      Improved Accessories II                                  T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[167] =

    #      ISG Mild Hybrid with EPS and IACC                        T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[168] =

    #      Tires I-10% Coefficient of Rolling Resistance Reduction  T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[169] =

    #      Tires II-20% Coefficient of Rolling Resistance Reduction T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[170] =

    #      Low Drag Brakes                                          T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[171] =

    #      Secondary Axle Disconnect                                T75(87:172,IY,IS)=AVGCOST(2,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[172] =

    #
    #   Light-Duty Vehicle Total
    #      Unit Body Construction                                   T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[173] =

    #      Mass Reduction I:  1.5%                                  T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[174] =

    #      Mass Reduction II                                        T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[175] =

    #      Mass Reduction III:  10%                                 T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[176] =

    #      Mass Reduction IV:  15%                                  T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[177] =

    #      Mass Reduction V:  20%                                   T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[178] =

    #      Aero I-10% Coefficient of Drag Reduction                 T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[179] =

    #      Aero II-20% Coefficient of Drag Reduction                T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[180] =

    #      6 Speed Manual                                           T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[181] =

    #      Aggressive Shift Logic I                                 T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[182] =

    #      Aggressive Shift Logic II                                T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[183] =

    #      Early Torque Converter Lockup                            T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[184] =

    #      High Efficiency Gearbox                                  T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[185] =

    #      5 Speed Automatic                                        T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[186] =

    #      6 Speed Automatic                                        T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[187] =

    #      7 Speed Automatic                                        T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[188] =

    #      8 Speed Automatic                                        T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[189] =

    #      Dual Clutch Automated Manual                             T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[190] =

    #      Continuously Variable Transmission                       T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[191] =

    #      Low Friction Lubricants                                  T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[192] =

    #      Engine Friction Reduction I-4 Cylinder                   T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[193] =

    #      Engine Friction Reduction I-6 Cylinder                   T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[194] =

    #      Engine Friction Reduction I-8 Cylinder                   T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[195] =

    #      Engine Friction Reduction II-4 Cylinder                  T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[196] =

    #      Engine Friction Reduction II-6 Cylinder                  T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[197] =

    #      Engine Friction Reduction II-8 Cylinder                  T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[198] =

    #      Cylinder Deactivation-6 Cylinder                         T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[199] =

    #      Cylinder Deactivation-8 Cylinder                         T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[200] =

    #      VVT I-OHV Intake Cam Phasing-6 Cylinder                  T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[201] =

    #      VVT I-OHV Intake Cam Phasing-8 Cylinder                  T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[202] =

    #      VVT I-OHC Intake Cam Phasing-4 Cylinder                  T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[203] =

    #      VVT I-OHC Intake Cam Phasing-6 Cylinder                  T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[204] =

    #      VVT I-OHC Intake Cam Phasing-8 Cylinder                  T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[205] =

    #      VVT II-OHV Coupled Cam Phasing-6 Cylinder                T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[206] =

    #      VVT II-OHV Coupled Cam Phasing-8 Cylinder                T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[207] =

    #      VVT II-OHC Coupled Cam Phasing-4 Cylinder                T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[208] =

    #      VVT II-OHC Coupled Cam Phasing-6 Cylinder                T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[209] =

    #      VVT II-OHC Coupled Cam Phasing-8 Cylinder                T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[210] =

    #      VVT III-OHV Dual Cam Phasing-6 Cylinder                  T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[211] =

    #      VVT III-OHV Dual Cam Phasing-8 Cylinder                  T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[212] =

    #      VVT III-OHC Dual Cam Phasing-4 Cylinder                  T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[213] =

    #      VVT III-OHC Dual Cam Phasing-6 Cylinder                  T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[214] =

    #      VVT III-OHC Dual Cam Phasing-8 Cylinder                  T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[215] =

    #      VVL I-OHV Discrete-6 Cylinder                            T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[216] =

    #      VVL I-OHV Discrete-8 Cylinder                            T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[217] =

    #      VVL I-OHC Discrete-4 Cylinder                            T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[218] =

    #      VVL I-OHC Discrete-6 Cylinder                            T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[219] =

    #      VVL I-OHC Discrete-8 Cylinder                            T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[220] =

    #      VVL II-OHV Continuous-6 Cylinder                         T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[221] =

    #      VVL II-OHV Continuous-8 Cylinder                         T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[222] =

    #      VVL II-OHC Continuous-4 Cylinder                         T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[223] =

    #      VVL II-OHC Continuous-6 Cylinder                         T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[224] =

    #      VVL II-OHC Continuous-8 Cylinder                         T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[225] =

    #      Stoichiometric GDI-4 Cylinder                            T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[226] =

    #      Stoichiometric GDI-6 Cylinder                            T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[227] =

    #      Stoichiometric GDI-8 Cylinder                            T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[228] =

    #      OHV to DOHC TBDS I-I4 (from V6), VVT, VVL, SGDI          T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[229] =

    #      OHV to DOHC TBDS I-V6 (from V8), VVT, VVL, SGDI          T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[230] =

    #      SOHC to DOHC TBDS I-I4 (from V6), VVT, VVL, SGDI         T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[231] =

    #      SOHC to DOHC TBDS I-V6 (from V8), VVT, VVL, SGDI         T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[232] =

    #      DOHC TBDS I-I3 (from I4), VVT, VVL, SGDI                 T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[233] =

    #      DOHC TBDS I-I4 (from V6), VVT, VVL, SGDI                 T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[234] =

    #      DOHC TBDS I-V6 (from V8), VVT, VVL, SGDI                 T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[235] =

    #      OHV to DOHC TBDS II-I4 (from V6), VVT, VVL, SGDI         T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[236] =

    #      OHV to DOHC TBDS II-V6 (from V8), VVT, VVL, SGDI         T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[237] =

    #      SOHC to DOHC TBDS II-I4 (from V6), VVT, VVL, SGDI        T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[238] =

    #      SOHC to DOHC TBDS II-V6 (from V8), VVT, VVL, SGDI        T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[239] =

    #      DOHC TBDS II-I3 (from I4), VVT, VVL, SGDI                T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[240] =

    #      DOHC TBDS II-I4 (from V6), VVT, VVL, SGDI                T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[241] =

    #      DOHC TBDS II-V6 (from V8), VVT, VVL, SGDI                T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[242] =

    #      OHV to DOHC TBDS III-I4 (from V6), VVT, VVL, SGDI, EGR   T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[243] =

    #      OHV to DOHC TBDS III-I4 (from V8), VVT, VVL, SGDI, EGR   T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[244] =

    #      SOHC to DOHC TBDS III-I4 (from V6), VVT, VVL, SGDI, EGR  T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[245] =

    #      SOHC to DOHC TBDS III-I4 (from V8), VVT, VVL, SGDI, EGR  T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[246] =

    #      DOHC TBDS III-I3 (from I4), VVT, VVL, SGDI, EGR          T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[247] =

    #      DOHC TBDS III-I4 (from V6), VVT, VVL, SGDI, EGR          T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[248] =

    #      DOHC TBDS III-I4 (from V8), VVT, VVL, SGDI, EGR          T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[249] =

    #      Electric Power Steering                                  T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[250] =

    #      Improved Accessories I                                   T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[251] =

    #      12V Micro Hybrid with EPS and IACC                       T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[252] =

    #      Improved Accessories II                                  T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[253] =

    #      ISG Mild Hybrid with EPS and IACC                        T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[254] =

    #      Tires I-10% Coefficient of Rolling Resistance Reduction  T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[255] =

    #      Tires II-20% Coefficient of Rolling Resistance Reduction T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[256] =

    #      Low Drag Brakes                                          T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[257] =

    #      Secondary Axle Disconnect                                T75(173:258,IY,IS)=AVGCOST(3,1:86,IY+BASEYR-1)/MC_JPGDP(1)*MC_JPGDP(11)
    # z[258] =

    return z
