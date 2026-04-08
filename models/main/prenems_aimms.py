import copy
import os
import pandas as pd
#from main import parse_dict as pdw
import parse_dict as pdw


def fill_aimms_user_items(user, pyfiler):
    """Update NEMS user object to include info for CMM, HMM, and NGMM.
    Write "runval" input files for CMM, HMM, and NGMM,

    Parameters
    ----------
    user : SimpleNamespace
        NEMS user object
    pyfiler : module
        NEMS pyfiler object

    Returns
    -------
    SimpleNamespace
        updated NEMS user object
    """
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
        write_parquet_mappings("HMM", "main/aimms_endpoint/Mappings", user)
        with open("hmm/input/h2config.txt", "r") as f:
            temp = f.readlines()
        i = [j for j in temp if j.startswith("nh::FirstModelYear")][0]
        user.HMMCONF["base_year"] = int(i.split("=")[1].replace(";","").replace("'","").strip())

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
        i = [j for j in temp if j.startswith("nn::FirstModelYear")][0]
        user.NGMMCONF["base_year"] = int(i.split("=")[1].replace(";","").replace("'","").strip())

    except:
        pass

    return user


def write_parquet_mappings(module, path, user):
    """Writes the parquet mapping file so AIMMS can read in parquet files.
    Parameters
    ----------
    path: str
        path to AIMMS_frame folder mapping file
        e.g. main/aimms_frame/Mappings
    module: str
        HMM, ngas, or coal

    user : SimpleNamespace
        NEMS user object

    Returns
    -------
    None
    """
    my_prefix = {"coal": "",
                 "hmm": "nh::",
                 "ngas": "nn::"}

#TODO: Generalize this for the rest of the AIMMS models. Currently just hardcoded to HMM
    dest_file = module +"ParquetMapping.xml"
    with open(os.path.join(path,dest_file), "w") as file:
        file.write("<AimmsParquetMapping>")
        for key, value in user.HMMCONF["putvar"].items():
            TableMappingName = key.replace(".", "_")

            ColumnMappingValues = value
            ColumnBindingValues = [my_prefix["hmm"] + item for item in ColumnMappingValues]
            ColumnBindingValues = ",".join(ColumnBindingValues)

            file.write(f'\n   <TableMapping name="{TableMappingName}">')
            file.write(f'\n     <RowMapping name="{TableMappingName}">')

            for column in ColumnMappingValues:
                file.write(f'\n         <ColumnMapping name="{column}" binds-to="{my_prefix["hmm"]}{column}"/>')

            file.write(f'\n         <ColumnMapping name="{TableMappingName}" maps-to="{my_prefix["hmm"]}{TableMappingName}({ColumnBindingValues})"/>')
            file.write("\n     </RowMapping>")
            file.write("\n  </TableMapping>")
        file.write("\n</AimmsParquetMapping>")
    file.close()



def parse_putget_file(f_in, pyfiler_dict, pyfiler):
    """Parse CMM/HMM/NGMM 'putget' file.

    Parameters
    ----------
    f_in: string
        filename
    pyfiler_dict : dict
        NEMS user object
    pyfiler : module
        NEMS pyfiler module (not used)

    Returns
    -------
    dict
        dictionary of variable dimensions
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
    """Write runval input file for NGMM, mostly based on scedes values.

    Parameters
    ----------
    scedes: dict
        NEMS scedes

    Returns
    -------
    None
    """
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
    """Write runval input file for CMM, based on scedes values.

    Parameters
    ----------
    scedes: dict
        NEMS scedes

    Returns
    -------
    None
    """
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
    """Write runval input file for HMM, based on scedes values.

    Parameters
    ----------
    scedes: dict
        NEMS scedes

    Returns
    -------
    None
    """
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