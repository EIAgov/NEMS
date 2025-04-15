# -*- coding: utf-8 -*-
"""
Created on Feb 06 2024

@author: Claire Su
"""
import os
import math


try:
    import mnfactorx_parser
except:
    from main import mnfactorx_parser

def main(user, pyfiler):
    """The main entry point to execute this conversion factor module. Load the mnfactorx.xlsx file,
    and write the data into the restart file.

    Parameters
    ----------

    user : SimpleNamespace
        user class object, containing dict with NEMS runtime information
    pyfiler : module
        pyfiler fortran module

    Returns
    -------
    None.

    """

    df=mnfactorx_parser.main(user, pyfiler)
    pyfiler = write_to_restart(user, pyfiler, df)

def write_to_restart(user, pyfiler, df):
    """Use the mnfactor dataframe data, compute and populate the values of Pyfiler convfact variables.

    Parameters
    ----------

    user : SimpleNamespace
        user class object, containing dict with NEMS runtime information
    pyfiler : module
        pyfiler fortran module
    df : DataFrame
        the dataframe parsed from the mnfactor file

    Returns
    -------
    module
        pyfiler fortran module

    """
    MNUMYR=pyfiler.utils.mnumyr #60    
    HISTYR=int(get_value(df,'HISTYR',True)) #30=2019-1990+1

    po = pyfiler.convfact
    po.histyr=HISTYR

    po.cfdslq   = get_value(df,'CFDSLQ')
    po.cfdsuq   = get_value(df,'CFDSUQ')
    po.cftgq    = get_value(df,'CFTGQ')
    po.cfrgq    = get_value(df,'CFRGQ')

    po.cfe85q   = get_value(df,'CFE85Q',partial_list = po.cfe85q.tolist())
    po.cfm85q   = get_value(df,'CFM85Q')
    po.cfbmq    = get_value(df,'CFBMQ')

    po.cfjoule  = get_value(df,'CFJOULE',True)
    po.cfcorn   = get_value(df,'CFCORN', True)
    po.cfcell   = get_value(df,'CFCELL', True)

    #! Setting CFASQ after using it as the placeholder for scalars above.
    po.cfasq    = get_value(df,'CFASQ',True) #6.636
    po.cfbuq    = get_value(df,'CFBUQ',True)
    po.cfdsq    = get_value(df,'CFDSQ',True)

    # We have dumped the value result into the mnfactor_processed.csv CFDSCQ row. No need to convert anymore. But keep the comments as info.
    #! Carb rules phase in as follows:
    #!      low sulfur July 1993-July 2006: CFDSCQ(II) = CFDSLQ(II)
    #!      start in July 1993: CFDSCQ(II) = CFDSQ
    #!      ultra low sulfur after July 2006: CFDSCQ(II) = CFDSUQ(II)
    po.cfdscq   = get_value(df,'CFDSCQ')
    
    po.cfeeq    = get_value(df,'CFEEQ',True)
    po.cfibq    = get_value(df,'CFIBQ',True)
    po.cfjfk    = get_value(df,'CFJFK',True)
    po.cfjfn    = get_value(df,'CFJFN',True)
    po.cfksq    = get_value(df,'CFKSQ',True)
    po.cfppq    = get_value(df,'CFPPQ',True)
    po.cfpcq    = get_value(df,'CFPCQ',True)
    po.cfccq    = get_value(df,'CFCCQ')  
    po.cfprq    = get_value(df,'CFPRQ',True)
    po.cfrsq    = get_value(df,'CFRSQ',True)
    po.cfsgq    = get_value(df,'CFSGQ',True)
    po.cfavq    = get_value(df,'CFAVQ',True)
    po.cfluq    = get_value(df,'CFLUQ',True)
    po.cfnpq    = get_value(df,'CFNPQ',True)
    po.cfusq    = get_value(df,'CFUSQ',True)
    po.cfwxq    = get_value(df,'CFWXQ',True)
    po.cfmsq    = get_value(df,'CFMSQ',True)
    po.cfmeqt   = get_value(df,'CFMEQT',True)
    po.cfpet    = get_value(df,'CFPET',True)
    po.cfogq    = get_value(df,'CFOGQ',True)
    po.cflgq    = get_value(df,'CFLGQ',partial_list = po.cflgq.tolist())

    po.cfcbob   = get_value(df,'CFCBOB')
    po.cfrbob   = get_value(df,'CFRBOB')
    po.cfcbq    = get_value(df,'CFCBQ')
    po.cfmgq    = get_value(df,'CFMGQ',partial_list = po.cfmgq.tolist())
    po.cfar3    = get_value(df,'CFAR3')
    po.cfmn3    = get_value(df,'CFMN3')
    po.cfgo3    = get_value(df,'CFGO3')
    po.cfotq    = get_value(df,'CFOTQ')
    po.cfpfq    = get_value(df,'CFPFQ',partial_list = po.cfpfq.tolist())  # this is repetting but main.f has the code. Leave it for now.
    po.cfdsrs   = get_value(df,'CFDSRS',partial_list = po.cfdsrs.tolist())    
    po.cfdscm   = get_value(df,'CFDSCM',partial_list = po.cfdscm.tolist())
    
    po.cfdsin   = get_value(df,'CFDSIN',partial_list = po.cfdsin.tolist())
    po.cfdstr   = get_value(df,'CFDSTR',partial_list = po.cfdstr.tolist())
    po.cfdsel   = get_value(df,'CFDSEL',partial_list = po.cfdsel.tolist())
    po.cfdsqt   = get_value(df,'CFDSQT',partial_list = po.cfdsqt.tolist())
    po.cfngu    = get_value(df,'CFNGU',partial_list = po.cfngu.tolist())
    po.cfngn    = get_value(df,'CFNGN',partial_list = po.cfngn.tolist())
    po.cfngp    = get_value(df,'CFNGP',partial_list = po.cfngp.tolist())
    po.cfngc    = get_value(df,'CFNGC',partial_list = po.cfngc.tolist())
    po.cfngi    = get_value(df,'CFNGI',partial_list = po.cfngi.tolist())
    po.cfnge    = get_value(df,'CFNGE',partial_list = po.cfnge.tolist())
    po.cfcngq   = get_value(df,'CFCNGQ')
    po.cfngcl   = get_value(df,'CFNGCL')    # this is a constant array
    
    po.cfimprd  = get_value(df,'CFIMPRD',partial_list = po.cfimprd.tolist())
    po.cfexprd  = get_value(df,'CFEXPRD',partial_list = po.cfexprd.tolist())
    po.cftpq    = get_value(df,'CFTPQ',partial_list = po.cftpq.tolist())
    po.cfetq    = get_value(df,'CFETQ',partial_list = po.cfetq.tolist())    #3.563
    # cfbiobute is constant. has been populated through 2050
    po.cfbiobute= get_value(df,'CFBIOBUTE')
    '''
    # CFETQ is not constant. Even though CFETQ was code as constant in main.f, the mnfactorx.xlsx gives different value for each year.
    # CFBIOBUTE is constant.
        RNAME = 'CFETQ'
        CALL GETRNGR(RNAME,CFETQ,HISTYR,1,1)    
        RNAME = 'CFBIOBUTE'
        CALL GETRNGR(RNAME,CFBIOBUTE,HISTYR,1,1)
        DO II = HISTYR+1,LASTYR
          CFETQ(II)=CFETQ(HISTYR)
          CFBIOBUTE(II)=CFBIOBUTE(HISTYR)
        ENDDO
    '''

    #! have not created CFDLTSWT, CFFLTSWT variables yet, but reading in to test read
    # no need CFDLTSWT, CFFLTSWT! (Take them out from code. CFDLTSWT, CFFLTSWT are not used in NEMS program, but for test purpose in main.f)
    po.cfcrdexp =  get_value(df,'CFCRDEXP',partial_list = po.cfcrdexp.tolist()) # The after HISTYR part data will be cmputed in docvfacts()

    po.cfcrdltswt= get_value(df,'CFCRDLTSWT')
    po.cfcrdltsour=get_value(df,'CFCRDLTSOUR')
    po.cfcrdmd2sour=get_value(df,'CFCRDMD2SOUR')
    po.cfcrdmdsour=get_value(df,'CFCRDMDSOUR')
    po.cfcrdhvswt=get_value(df,'CFCRDHVSWT')
    po.cfcrdhvsour=get_value(df,'CFCRDHVSOUR')
    po.cfcrdca=get_value(df,'CFCRDCA')
    po.cfcrdsyn=get_value(df,'CFCRDSYN')
    po.cfcrddilbit=get_value(df,'CFCRDDILBIT')
    po.cfcrdlt2swt=get_value(df,'CFCRDLT2SWT')
    po.cfcrdlscond=get_value(df,'CFCRDLSCOND')
    po.cfcrddom=get_value(df,'CFCRDDOM',partial_list = po.cfcrddom.tolist())
    po.cfcrdimp=get_value(df,'CFCRDIMP',partial_list = po.cfcrdimp.tolist())
    po.cfcrdexp=get_value(df,'CFCRDEXP',partial_list = po.cfcrdexp.tolist())    

    #!  for CFVEGGIE and CFBIOD, carry values throughout forecast period
    po.cfbiod=get_value(df,'CFBIOD')
    po.cfveggie=get_value(df,'CFVEGGIE')
    po.cfetfac=get_value(df,'CFETFAC',partial_list = po.cfetfac.tolist())
    po.cfelq=get_value(df,'CFELQ',True)

    # CFNPQ, CFJFN, etc. has been read from the .xlsx df and assigned into pyfiler. Now, ready to calculate CFFTLIQ:
    po.cfftliq[0,:]=po.cfnpq * 0.92 #CFFTLIQ(1,II)=CFNPQ * 0.92 or use fall-back value=CFNPQ
    po.cfftliq[1,:]=po.cfjfn * 0.92 #CFFTLIQ(2,II)=CFJFN * 0.92 or use fall-back value=CFNPQ
    po.cfftliq[2,:]=po.cfjfk * 0.92 #CFFTLIQ(3,II)=CFJFK * 0.92 or use fall-back value=CFKSQ
    po.cfftliq[3,:]=po.cfbiod       #!  CFBIOD is about 8% less than distillate (DO II = 1,LASTYR: CFFTLIQ(4,II)=CFBIOD(II) or use fall_back value=CFDSQ)
    
    #!  overriding what was read in for these (input file has values extracted from LFMM, these are as below):
    # in the input .xlsx, CFAR3 has the fomula "=CFRSQ" alread, so we just need to directly read the values in
    po.cfar3=get_value(df,'CFAR3')  #!  set atmospheric residuum to residual fuel oil (CFRSQ, 6.287 at 1990) or fall-back value
    po.cfmn3=get_value(df,'CFMN3')  #!  set medium naphtha to naphtha jet fuel (CFJFN) or fall-back value
    po.cfgo3=get_value(df,'CFGO3')  #!  set gas oil to straight distillate (CFDSQ) or fall-back value
    po.cfgop=get_value(df,'CFGOP')  #!  set gas oil to straight distillate (CFDSQ, 5.825 at 1990) or fall-back value

    # use WEIGHTS: WEIGHTLPG1, WEIGHTLPG2, etc. to compute CFNGL etc.
    pyfiler=calc_weights(pyfiler,df,HISTYR)
  
    # API: (API only has 3 elements - API_IN,40.0,135.8,0.6)
    temp=get_value(df,'API_IN')
    po.api_in=temp[:3]
    # CRUDEQ only has 20(=5x4) elements
    temp=get_value(df,'CRUDEQ')
    po.crudeq=temp[:20]

    po.apiltsw[1,:]=po.cfcrdltswt
    po.apiltso[1,:]=po.cfcrdltsour
    po.apimmso[1,:]=po.cfcrdmd2sour
    po.apimdso[1,:]=po.cfcrdmdsour
    po.apihvsw[1,:]=po.cfcrdhvswt
    po.apihvso[1,:]=po.cfcrdhvsour
    po.apica[1,:]=po.cfcrdca
    po.apisyn[1,:]=po.cfcrdsyn
    po.apidil[1,:]=po.cfcrddilbit
    po.apillsw[1,:]=po.cfcrdlt2swt
    po.api50pl[1,:]=po.cfcrdlscond

    pyfiler = check_if_need_customize(user,pyfiler,HISTYR)

    #! Use fall-back values for these if 0. BUT, no need this computation anymore,
    # since all fall-back value has been populated in the mnfactor_processed.csv file.
	#Line  5878:          IF (APICAMG(2,II) .EQ. 0.0) APICAMG(2,II) = CFRGQ(II)
    # However, we do need to assign APICAMG(2) and CFUBAQ values since they are not set anywhere above
    for i in range(MNUMYR):
        if po.cfubaq[i]==0: po.cfubaq[i]=po.cfdsq
        if po.apicamg[1,i]==0: po.apicamg[1,i]=po.cfrgq[HISTYR-1]

    return pyfiler

def calc_weights(pyfiler,df,HISTYR):
    """Retrieve WEIGHTLPG1, WEIGHTPETF etc. weights data from mnfactor dataframe, to calculate and popluate CFNGL, CFLGQ, etc.
    conversion factor variables. 

    Parameters
    ----------

    pyfiler : module
        pyfiler fortran module
    df : DataFrame
        the dataframe parsed from the mnfactor file
    HISTYR : int
        integer year 34 is the historical year 2023 calculated to the base year (BASEYR) 1990.

    Returns
    -------
    None.

    """
    po = pyfiler.convfact

    # calculate CFNGL by WEIGHTLPG1: 
    w1=get_value_default_zero(df,'WEIGHTLPG1(1)')
    w2=get_value_default_zero(df,'WEIGHTLPG1(2)')
    w3=get_value_default_zero(df,'WEIGHTLPG1(3)')
    w4=get_value_default_zero(df,'WEIGHTLPG1(4)')
    w5=get_value_default_zero(df,'WEIGHTLPG1(5)')
    for i in range(0,HISTYR):
        w=w1[i]+w2[i]+w3[i]+w4[i]+w5[i]
        if w!=0:
            po.cfngl[i]= (w5[i]*po.cfppq+w1[i]*po.cfeeq+w2[i]*po.cfprq+w3[i]*po.cfbuq+w4[i]*po.cfibq) / w

    # calculate CFLGQ by WEIGHTLPG2: 
    w1=get_value_default_zero(df,'WEIGHTLPG2(1)')
    w2=get_value_default_zero(df,'WEIGHTLPG2(2)')
    w3=get_value_default_zero(df,'WEIGHTLPG2(3)')
    w4=get_value_default_zero(df,'WEIGHTLPG2(4)')
    for i in range(0,HISTYR):
        w=w1[i]+w2[i]+w3[i]+w4[i]
        if w!=0:
            po.cflgq[i]= (w1[i]*po.cfeeq+w2[i]*po.cfprq+w3[i]*po.cfbuq+w4[i]*po.cfibq) /w

    # calculate CFPFQ by WEIGHTPETF:
    w1=get_value_default_zero(df,'WEIGHTPETF(1)')
    w2=get_value_default_zero(df,'WEIGHTPETF(2)')
    for i in range(0,HISTYR):
        w=w1[i]+w2[i]
        if w!=0:
            po.cfpfq[i]= (w1[i]*po.cfnpq+w2[i]*po.cfdsq) /w

    # calculate CFJFQ by WEIGHTJETF: 
    w1=get_value_default_zero(df,'WEIGHTJETF(1)')
    w2=get_value_default_zero(df,'WEIGHTJETF(2)')
    for i in range(0,HISTYR):
        w=w1[i]+w2[i]
        if w!=0:
            po.cfjfq[i]= (w1[i]*po.cfjfn+w2[i]*po.cfjfk) /w

    # calculate CFIMUO by WEIGHTUOIL: 
    w1=get_value_default_zero(df,'WEIGHTUOIL(1)')
    w2=get_value_default_zero(df,'WEIGHTUOIL(2)')
    w3=get_value_default_zero(df,'WEIGHTUOIL(3)')
    w4=get_value_default_zero(df,'WEIGHTUOIL(4)')
    for i in range(0,HISTYR):
        w=w1[i]+w2[i]+w3[i]+w4[i]
        if w!=0:
            po.cfimuo[i]= (w1[i]*po.cfnpq+w2[i]*po.cfksq+w3[i]*po.cfrsq+w4[i]*po.cfrsq) /w

    return pyfiler

def check_if_need_customize(user,pyfiler,HISTYR):
    """Exam if LFMM and/or NGMM is turned on, to decide whether using the last historical year for the projections.
    by calling customize_lfmm_calculation() and/or customize_ngmm_calculation().

    Parameters
    ----------
    user : SimpleNamespace
        a user class. It contains a user scedes dictionary and other settings
    
    pyfiler : module
        pyfiler fortran module
    
    HISTYR : int
        integer year 34 is the historical year 2023 calculated to the base year (BASEYR) 1990.

    Returns
    -------
    module
        pyfiler fortran module
        
    """
    exo=int(user.SCEDES['EXO'])
    exg=int(user.SCEDES['EXG'])
    if pyfiler.ncntrl.exo == 1 or exo == 1:
        pyfiler=customize_lfmm_calculation(pyfiler,HISTYR)
    if pyfiler.ncntrl.exg == 1 or exg == 1:
        pyfiler=customize_ngmm_calculation(pyfiler,HISTYR)
    return pyfiler

def customize_lfmm_calculation(pyfiler,HISTYR):
    """When EXO=1, check_if_need_customize() calls this method to customize Pyfiler convfact variables.
    Put the last historical value in all of the forecast years, except for stuff LFMM calculates (like CFIMPRD, CFEXPRD, CFMGQ)
    This uses the last historical year for the projections. If we didn't do this, the value in the projections would be an old 
    number (providing history changes).

    Parameters
    ----------
    pyfiler : module
        pyfiler fortran module
    
    HISTYR : int
        integer year 34 is the historical year 2023 calculated to the base year (BASEYR) 1990.

    Returns
    -------
    module
        pyfiler fortran module
    
    """
    po=pyfiler.convfact
    mnumyr=pyfiler.utils.mnumyr
    po.cfpfq[-(mnumyr-HISTYR):] =[po.cfpfq[HISTYR-1] for i in po.cfpfq[-(mnumyr-HISTYR):]]
    po.cfjfq[-(mnumyr-HISTYR):] =[po.cfjfq[HISTYR-1] for i in po.cfjfq[-(mnumyr-HISTYR):]]
    po.cfcbq[-(mnumyr-HISTYR):] =[po.cfcbq[HISTYR-1] for i in po.cfcbq[-(mnumyr-HISTYR):]]
    po.cfcbob[-(mnumyr-HISTYR):]=[po.cfcbob[HISTYR-1] for i in po.cfcbob[-(mnumyr-HISTYR):]]
    po.cfrbob[-(mnumyr-HISTYR):]=[po.cfcbob[HISTYR-1] for i in po.cfrbob[-(mnumyr-HISTYR):]]
    po.cfdslq[-(mnumyr-HISTYR):]=[po.cfdslq[HISTYR-1] for i in po.cfdslq[-(mnumyr-HISTYR):]]
    po.cfdsuq[-(mnumyr-HISTYR):]=[po.cfdsuq[HISTYR-1] for i in po.cfdsuq[-(mnumyr-HISTYR):]]
    po.cfdscq[-(mnumyr-HISTYR):]=[po.cfdscq[HISTYR-1] for i in po.cfdscq[-(mnumyr-HISTYR):]]
    po.cfe85q[-(mnumyr-HISTYR):]=[po.cfe85q[HISTYR-1] for i in po.cfe85q[-(mnumyr-HISTYR):]]
    return pyfiler

def customize_ngmm_calculation(pyfiler,HISTYR):
    """When EXG=1, check_if_need_customize() calls this method to customize Pyfiler convfact variables.

    Parameters
    ----------
    pyfiler : module
        pyfiler fortran module
    
    HISTYR : int
        integer year 34 is the historical year 2023 calculated to the base year (BASEYR) 1990.

    Returns
    -------
    module
        pyfiler fortran module
    """    
    po=pyfiler.convfact
    mnumyr=pyfiler.utils.mnumyr
    po.cfngu[-(mnumyr-HISTYR):] =[po.cfngu[HISTYR-1] for i in po.cfngu[-(mnumyr-HISTYR):]]
    po.cfngn[-(mnumyr-HISTYR):] =[po.cfngn[HISTYR-1] for i in po.cfngn[-(mnumyr-HISTYR):]]
    po.cfngc[-(mnumyr-HISTYR):] =[po.cfngc[HISTYR-1] for i in po.cfngc[-(mnumyr-HISTYR):]]
    po.cfngp[-(mnumyr-HISTYR):] =[po.cfngp[HISTYR-1] for i in po.cfngp[-(mnumyr-HISTYR):]]
    po.cfngi[-(mnumyr-HISTYR):] =[po.cfngi[HISTYR-1] for i in po.cfngi[-(mnumyr-HISTYR):]]
    po.cfnge[-(mnumyr-HISTYR):] =[po.cfnge[HISTYR-1] for i in po.cfnge[-(mnumyr-HISTYR):]]
    po.cfcngq[-(mnumyr-HISTYR):]=[po.cfcngq[HISTYR-1] for i in po.cfcngq[-(mnumyr-HISTYR):]]
    po.cfngcl[-(mnumyr-HISTYR):]=[po.cfngcl[HISTYR-1] for i in po.cfngcl[-(mnumyr-HISTYR):]]
    return pyfiler

def get_value_default_zero(df,key):
    """A helper method based on get_value(), to default the looked-up value NaN to 0.0.
    The value can be a single data or a float type array.

    Parameters
    ----------
    df : DataFrame
        the dataframe parsed from the mnfactor file
    
    key : string
        the lookup key in mnfactor dataframe

    Returns
    -------
    list
        the list of lookup value

    """
    data=get_value(df,key)
    data=[0.0 if math.isnan(x) else x for x in data]
    return data

def get_value(df, key, is_single_value = False, year = 1990, partial_list = []):
    """A helper method to look up mnfactor dataframe and return a single float value or an array.

    Parameters
    ----------
    df : DataFrame
        the dataframe parsed from the mnfactor file

    key : str
        the lookup key in mnfactor dataframe
    
    is_single_value : bool, optional
        look up a single value or a data array. Default is an array.
    
    year : int
        specific year column to look up. Default is 1990

    partial_list : list, optional
        list if data not populated through 2025

    Returns
    -------
    list
        list of mnfactor values corresponding to the variable

    """    
    lookup_column_header='API'
    get_row=df.loc[df[lookup_column_header] == key]

    # you shall never hit this senario. You shall always have looked-up data saved in $NEMS\input\mnfactorx_processed.csv
    if get_row.empty:
        print(f"no corresponding data found for {key} in the conversion factor table. Exit the program now")
        os.sys.exit()

    if is_single_value:
        single_value=get_row[year].iloc[0]
        return single_value
    
    elif not partial_list:  #Check if partial_list is empty
        ls=get_row.values.flatten().tolist()[1:]
        return ls
    
    else:   # return a list of year values
        ls=get_row.values.flatten().tolist()[1:]
        if any(x != x for x in ls): #Check if the list contains nan
            for idy, y in enumerate(ls):
                if y != y:  #Iterate and check if element is nan
                    ls[idy] = partial_list[idy] #Replaces the nan with values from partial_list
        
        return ls


if __name__ == '__main__':
    print("Please load from the main program script. Exit now...")
    os.sys.exit()