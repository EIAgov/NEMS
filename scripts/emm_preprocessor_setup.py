import os
import sys
import shutil
import subprocess
import logging
logger = logging.getLogger(__name__)


def prepplt(outdir, scenario, pwd,datekey, emmprepro):
    sys.path.append(pwd)
    from  models.main.parse_scedes import parse_scedes_all
    os.environ['NEMS']=pwd
    username=os.getenv('username')
    outdir=outdir+username
    # We need to know the "launched from" directory
    OLDDIR=pwd+r"/scedes"
    sourcedir=pwd+r"/source"
    
    #set up some directories
    if os.path.isdir(os.path.join(outdir,scenario)):
        pass
    else:
        os.mkdir(outdir + "/" + scenario)

    if os.path.isdir(os.path.join(outdir,scenario,datekey)):
        pass
    else:
        os.mkdir(outdir + "/" +scenario + "/" + datekey)
    
    if os.path.isdir(os.path.join(outdir,scenario,datekey,'emm_db')):
        pass
    else:
        os.mkdir(outdir + "/" + scenario + "/" + datekey + "/" + 'emm_db')

    #get a whole bunch of files
    os.chdir(os.path.join(outdir,scenario,datekey))
    logging.basicConfig(filename=emmprepro+'.log', level=logging.INFO)
    logger.info('Started')
    logger.info('Outdir:{0}'.format(outdir))
    logger.info('Scenario:{0}'.format(scenario))
    logger.info('Launch directory:{0}'.format(pwd))
    logger.info('Date Key: {0}'.format(datekey))
    logger.info('preprocessor:  making directories')
    logger.info('preprocessor:  copying files')
    shutil.copy(os.path.join(OLDDIR,("FILELIST"+"."+scenario+"."+datekey)), "FILELIST")
    shutil.copy(os.path.join(OLDDIR,("scedes.all"+"."+scenario+"."+datekey)), "scedes.all")
    copy_list=[]

    #parse the filelist adn replace with the launch directory
    with open('filelist_temp',"w") as f2:
        with open('FILELIST') as f:
            for line in f:
                line=line.replace("$NEMS",pwd)
                f2.write(line)
                a=line.split(" ")
                #logger.info(a)
                try:
                    copy_list.append(a[2])
                except IndexError:
                    logger.info('Not Parsing:{0}'.format(a))
    #logger.info(copy_list)
    os.replace('filelist_temp','FILELIST')
    os.mkdir(outdir+"/"+scenario+"/"+datekey+"/input")
    os.mkdir(outdir+"/"+scenario+"/"+datekey+"/input/emm")

    #just copy everything in the emm directory
    s = pwd+'/input/emm'
    t = outdir+"/"+scenario+"/"+datekey+"/input/emm"
    files = os.listdir(s)
    for fname in files:
        try:
            shutil.copy(os.path.join(s, fname), t)
        except PermissionError:
            pass
        
    scedes_dict=parse_scedes_all("scedes.all")
        
    #Move the emm_db
    s = scedes_dict['EMM_DBN'].replace("$NEMS",pwd)
    t = outdir+"/"+scenario+"/"+datekey+"/emm_db"
    files = os.listdir(s)
    for fname in files:
        shutil.copy(os.path.join(s, fname), t)

    #move other stuff that may be helpful
    for i in copy_list:
        try:
            if i[1]!=":" :
                continue
        except IndexError:
            logger.info(i)
        fileneeded=i.split("/")[-1]
        logger.info(fileneeded)
        try:
            shutil.copy(i,fileneeded )
        except FileNotFoundError:
            logger.info(i)
        except PermissionError:
            logger.info(i)
    
    #compile the code
    rr=pwd+"/includes"
    logger.info('preprocessor:  compiling files')
    flags='/free /traceback /names:lowercase /assume:underscore /include:'+rr+' /check:bounds /Qzero /Qsave /debug:full /fpconstant /assume:byterecl /nolist /static /heap-arrays0'
    prgms=['FSQLITEN','FILEMGRN','FILERN','UDATN','UDAFN','ULDSMN','CIO4WK1N','NEMSWK1N','FWK1ION','UREADN','UESQLN','DUMMYPPN']
    prgm_txt=''
    #change the file based on the option chosen
    match emmprepro:
        case "prepplt":
            prgms=prgms+['PREPPLTN']
    
        case "prepett":
            prgms=prgms+['PREPETTN']

    for i in prgms:
        prgm_txt=prgm_txt+scedes_dict[i]+" "
    prgm_txt=prgm_txt.replace("$NEMS",pwd)+" "+ sourcedir+"/gdxf9glu.o " + " "+ sourcedir + "/gdxf9def.f" +" "+ sourcedir + "/libfsqlite.lib legacy_stdio_definitions.lib "
    scpt='ifort /exe:'+emmprepro+'.exe ' + sourcedir + "/util_tools_prepro.f90 "+ prgm_txt + flags

    logger.info(scpt)

    result=subprocess.run(scpt, capture_output=True, text=True)
    logger.info(result.stdout)
    logger.info(result.stderr)
    logger.info("preprocessor: Submitting{0}".format(emmprepro))         

    #parse the filelist adn replace with the output directory
    with open('filelist_temp',"w") as f2:
        with open('FILELIST') as f:
            for line in f:
                try:
                    key=line.split(' ')[1]
                    if key in ['ETTIN','ETTDEM','PLNTDAF']:
                        line=line.replace(pwd+"/input/emm",outdir + "/" + scenario + "/" + datekey)
                    else:
                        line=line.replace(pwd,outdir + "/" + scenario + "/" + datekey)
                except IndexError:
                    pass
                f2.write(line)
                a=line.split(" ")
                #logger.info(a)
                try:
                    copy_list.append(a[2])
                except IndexError:
                    logger.info('Not Parsing:{0}'.format(a))
    os.replace('filelist_temp','FILELIST')

    os.chdir(outdir+"/"+scenario+"/"+datekey)
    result=subprocess.run(emmprepro+".exe", capture_output=True, text=True)

    logger.info(result.stdout)
    logger.info(result.stderr)

if __name__ == "__main__":
    outdir=r'L:/main/'
    scenario=r'prepett'
    pwd=r'L:/main/ark/NEMS_base'
    emmprepro='prepett'
    datekey='d042925b'

    prepplt(outdir, scenario, pwd, datekey,emmprepro)


