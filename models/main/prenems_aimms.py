import os
import pandas as pd
#from main import parse_dict as pdw
import parse_dict as pdw


def fill_aimms_user_items(user, pyfiler):

    pyfiler_dict_df = pdw.ParseDict('dict.txt')[0].set_index(['Common Block Name', 'Fortran Variable Name'])

    try:
        user.CMMCONF = {} 
        user.CMMCONF["putvar"] = ""
        write_runval_cmm(user.SCEDES)
        user.CMMCONF["putvar"] = parse_putget_file("coal/input/coalputvars.txt", pyfiler_dict_df, pyfiler)
        user.CMMCONF["base_year"] = int(user.SCEDES['CLBASEYR'])
    except:
        pass

    try:
        user.HMMCONF = {}
        user.HMMCONF["putvar"] = ""
        write_runval_hmm(user.SCEDES)
        user.HMMCONF["putvar"] = parse_putget_file("hmm/input/h2putvars.txt", pyfiler_dict_df, pyfiler)

        with open("hmm/input/h2config.txt", "r") as f:
            temp = f.readlines()
        i = [j for j in temp if j.startswith("FirstModelYear")][0]
        user.HMMCONF["base_year"] = int(i.split("=")[1].replace(";","").replace("'","").strip())
        #for i in temp:
        #    if i.startswith("FirstModelYear"):
        #        user.HMMCONF["base_year"] = int(i.split("=")[1].replace(";","").replace("'","").strip())

    except:
        pass

    try:
        user.NGMMCONF = {}
        user.NGMMCONF["putvar"] = ""
        with open('ngas/input/ngparquetswitch.txt', 'w') as f:
            f.write(f'ParquetReadSwitch := 0; !Switch to read restart file data using Parquet method\n')
            f.write(f'ParquetWriteSwitch := 0; !Switch to write restart file data using Parquet method\n')

        write_runval_ng(user.SCEDES)
        user.NGMMCONF["putvar"] = parse_putget_file("ngas/input/ngputvars.txt",  pyfiler_dict_df, pyfiler)

        with open("ngas/data/nginitialize.txt", "r") as f:
            temp = f.readlines()
        i = [j for j in temp if j.startswith("FirstModelYear")][0]
        user.NGMMCONF["base_year"] = int(i.split("=")[1].replace(";","").replace("'","").strip())

        #for i in temp:
        #    if i.startswith("FirstModelYear"):
        #        user.NGMMCONF["base_year"] = int(i.split("=")[1].replace(";","").replace("'","").strip())
    except:
        pass

    return user


def parse_putget_file(f_in, pyfiler_dict, pyfiler):
    """
        pyfiler_dict should be:
            PyFiler.PyFilerWrapper.ParseDict('dict.txt')[0] \
                  .set_index(['Common Block Name', 'Fortran Variable Name'])
    """
    varlistfile = f_in
    z = pyfiler_dict

    df = pd.read_csv(varlistfile, header=None) \
           .rename(columns={0:'common_block', 1:'variable'})
    for i in df.columns:
        df[i] = df[i].apply(lambda x: x.split('=')[1].strip())

    df = df[~df.duplicated()]
	
    df_all = df[df["variable"].str.upper() == "ALL"]
    df = df[df["variable"].str.upper() != "ALL"]
    L = []
    for i in df_all["common_block"].unique():
        j = eval(f"pyfiler.{i.lower()}.__dict__.keys()")
        z = [k.upper() for k in j if not k.startswith("_")]
        d = {"common_block":[i], "variable": z}
        temp = pd.DataFrame(data = d)
        temp["common_block"] = i
        L.append(temp.copy())
    df = pd.concat([df] + L)

    df['Common Block Name'] = df['common_block'].str.upper()
    df['Fortran Variable Name'] = df['variable'].str.upper()
    df['my_name'] = df['common_block'] + '.' + df['variable']
    df = df.set_index(['Common Block Name', 'Fortran Variable Name'])
    
    my_dim = {}
    for i in df.index:
        temp = z.loc[i, 'Dimensions Parameters'].split(',')
        temp2 = []
        for j in temp:
            if str(j).isdigit():
                temp2.append(f"M{j}")
            else:
                temp2.append(j)
        try:               
            my_dim[df.loc[i, 'my_name']] = temp2
        except:
            my_dim[df.loc[i, 'my_name'].iloc[0]] = temp2
    
    return my_dim


# once per NEMS run
def write_runval_ng(scedes): 

    scedes_params = [ \
        "STEOBM", "STEONG", "STSCALNG", 
        "KEEPOPEN", "NGASSUMPTIONSN", "NGMARKUPSN", "NGLNGEXPN",   
        "NGMEXICON", "NGSPOTPRCN", "NGCAPACITYN", "NGCANADAN",      
        "NGEIAN", "NGSETMAPN", "NGVARTARN", "NGSTEOFACTINN", 
        "NGTEXASN", "NGTXCAPAN", ]

    if os.getcwd().endswith('ngas'):  # TODO: correct?
        f_out = "ng_runval.txt"
    else:
        f_out = os.path.join('ngas', 'ng_runval.txt')

    with open(f_out, 'w', encoding='utf-8') as f:
        for i in scedes_params:
            if scedes[i].isnumeric():
                f.write(f'{i} := {scedes[i]};\n')
            else:
                f.write(f'{i} := "{scedes[i]}";\n')

        # TODO: why aren't these in the scedes file ?
        f.write('x_LNG_Gamma2 := 55.0;\n')  # not in scedes
        f.write('x_LNG_Gamma1 := -42.5;')  # not in scedes


# once per NEMS run
def write_runval_cmm(scedes): 

    scedes_params = [ \
        "CLUSEXPORTICMMN",  
        "ICMMCOMMODITYMAPN", 
        "CLOCEANDISTN", 
        "CLAGGEXPORTMAPN", 
        "CLEXPORTLIMITSN", 
        "CLIMPORTLIMITSN", 
        "CLINTLDEMANDN", 
        "CLINTLQUALITYN",  
        "CLINTLSUPPLYN", 
        "CLDISTANCEN", 
        "CLFEASIBLEROUTN", 
        "CLFREIGHTN", 
        "CLRATESELECN", 
        "CLRATESNONELECN", 
        "CLTONRAILMILEN", 
        "CLTONSPERCARN", 
        "CLINTLUSEXPORTN", 
        "CLFLAGSN", 
        "CLBYPRDN", 
        "CLUSIMPN", 
        "CLHMISCN", 
        "CLHDISPN", 
        "CLHSCPRN", 
        "CLHWRLDN", 
        "CLHEWQN", 
        "CLRCAFN", 
        "CLCPSRN", 
        "CLCDSRN", 
        "CLIMSURN", 
        "CLCPSYRN", 
        "CLPRFILN", 
        "CLGNPLTN",] 

    if os.getcwd().endswith('coal'):  # TODO: correct?
        f_out = "cl_runval.txt"
    else:
        f_out = os.path.join('coal', 'cl_runval.txt')

    with open(f_out, 'w', encoding='utf-8') as f:
        for i in scedes_params:
            if scedes[i].isnumeric():
                f.write(f'{i} := {scedes[i]};\n')
            else:
                f.write(f'{i} := "{scedes[i]}";\n')


# once per NEMS run
def write_runval_hmm(scedes): 

    scedes_params = ["KEEPOPEN","ZTCCOSTM", "TRANEFF"]

    if os.getcwd().endswith('hmm'):  # TODO: correct?
        f_out = "h2_runval.txt"
    else:
        f_out = os.path.join('hmm', 'h2_runval.txt')

    with open(f_out, 'w', encoding='utf-8') as f:
        for i in scedes_params:
            if scedes[i].isnumeric():
                f.write(f'{i} := {scedes[i]};\n')
            else:
                f.write(f'{i} := "{scedes[i]}";\n')

