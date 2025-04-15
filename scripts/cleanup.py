"""
Cleanup output folder by deleting or compressing various files.
"""
from datetime import datetime
import glob
import gzip
import os.path
import shutil
import sys
import tarfile
#import zipfile as zf

LOGFILE = "nohup.out"
MODULE_NAME = "cleanup.py"

DELETE_ME = {}
DELETE_ME["emm"] = ["emm_*.daf",]

COMPRESS_ME_NOT = ["dict.out", "intercvout.txt", "nemsvardf.csv"]
COMPRESS_ME_NOT = [i.lower() for i in COMPRESS_ME_NOT[:]]

COMPRESS_ME = {}

COMPRESS_ME["emm"] = ["emm_*",
                      "edb*.txt"]

COMPRESS_ME["ecp"] = ["ecp/*.txt",
                      "ecp/*.mps",
                      "ecp/log/*.lis",
                      "ecp/ecp_*/*.txt",
                      "ecp/ecp_*/*.mps",
                      "ecp/ecp_*/log/*.lis",
                      ]

COMPRESS_ME['efd'] = ["efd/*.txt",
                      "efd/*.mps",
                      "efd/log/*.lis",
                      ]


COMPRESS_ME['input'] = ["input/layin.xls",
                        "input/ps*.unf", 
                        ]

COMPRESS_ME["misc"] = ["*.20", "*.daf", "*.gdx", "*dbg.txt", "*dbug.txt", 
                       "*out.txt", "comfloor.xls", "idm*.csv", "mchighlo.xls", "tdm*.txt",
                       "ldsmrpt.txt", "NEMSVardf.csv", "resdeqp.txt", "RFM_WDUMP.*", 
                       "uefp.txt", "hmm/debug", "hsm/debug", "*.mdb", "*.mps",
                       "hmm/fromAIMMS/*.txt", "hmm/toAIMMS/*.txt",
                       "ngas/fromAIMMS/*.txt", "ngas/toAIMMS/*.txt",
                       "rest/toAIMMS/*.txt",
                       "RAN_log.log"]

my_folders = ["coal/fromAIMMS", "coal/toAIMMS"] 

compress_subfolder = ["rest/fromAIMMS"]


def log_it(n, s):
    """Append a formatted message to LOGFILE (nohup.out).
    
    LOGGFILE is a module-level hardcoded parameter.

    sys.stdout.flush() and sys.stederr.flush() ensure that
    nothing is left in the buffer.

    Parameters
    ----------
    n : integer
        cycle number
    s : string
        message to write in logfile
    """
    with open(LOGFILE, "a", encoding="utf-8") as f:
        f.write(f"{datetime.now()} :: cycle {n} :: {MODULE_NAME} :: {s}\n")
        sys.stdout.flush()
        sys.stderr.flush()


def cleanup_gz_folder(n, folder_path):
    """
    Attempt to compress a given folder into a .tar.gz file
    If compression is successful, delete the original
    folder.

    Parameters
    ----------
    folder_pathn : list
        list of strings of folder names
    """
    for i in folder_path:
        if os.path.isdir(i):
            try:
                log_it(n, f"Zipping folder: {i}")
                # Attempt to zip folder
                with tarfile.open(f"{i}.tar.gz", "w:gz") as tar:
                    tar.add(i, arcname=os.path.basename(i))

                log_it(n, f"Done Zipping folder: {i}")
                # Delete folder after successfully zipping
                shutil.rmtree(i)

            except:
                print(f"cannot compress folder {i}")

        else:
            log_it(n, f"Does No Exist Folder: {i}")


def list_folders(path=['.']):
    """List all folders in the specified directory"""
    folders = []
    for i in path:
        try:
            for entry in os.scandir(i):
                if entry.is_dir():
                    folders.append(f'{i}/{entry.name}')
        except:
            pass
    return folders


def cleanup(n, my_dir):
    """Reanme, compress, and/or delete folders and files.

    Parameters
    ----------
    n : int
        NEMS cycle number
    my_dir : string
        run folder
    """
    log_it(n, "move .exe files to .xxx and (try to) delete .exe files")
    for subdir in ["", "p1", "p2", "p3"]:
        z = glob.glob(os.path.join(my_dir, subdir, "*.exe"))
        for f in z:
            try:
                shutil.move(f, f.replace(".exe", ".xxx"))
            except:
                log_it(n, f"unable to move {f} to {f.replace('.exe', '.xxx')}")
        for f in z:
            if os.path.exists(f.replace(".exe", ".xxx")):
                try:
                    os.remove(f)
                except:
                    log_it(n, f"unable to delete: {f}")

    # delete specified files
    log_it(n, "delete specified files")
    files_to_delete = list(set([val for vals in DELETE_ME.values() for val in vals]))
    temp = [f"{i}/{j}" for i in ["p1", "p2", "p3"] for j in files_to_delete]
    files_to_delete += temp

    for f in files_to_delete:
        z = glob.glob(os.path.join(my_dir, f))
        z = [i for i in z[:] if os.path.isfile(i)]
        for f2 in z:
            try:
                os.remove(f2) 
            except:
                pass

    # compress specified files/folders
    MIN_COMPRESS_SIZE = 1000000  # minimum size (in bytes) of files to compress
    log_it(n, "compress specified files")
    temp = [f for i in COMPRESS_ME.values() for f in i]
    temp_p = [f"{i}/{j}" for i in ["p1", "p2", "p3"] for j in temp]
    temp2 = [glob.glob(i) for i in temp+temp_p if glob.glob(i)]
    temp3 = [f for i in temp2 for f in i]
    temp4 = list(set(temp3))
    temp5 = [i for i in temp4 if not i.endswith(".gz")]
    temp6 = [i for i in temp5 if os.path.getsize(i) > MIN_COMPRESS_SIZE]
    files_to_compress = [i for i in temp6 if i.split("\\")[-1].lower() not in COMPRESS_ME_NOT]

    compressed = []
    for f2 in files_to_compress:
        with open(f2, "rb") as f_in:
            try:
                with gzip.open(f"{f2}.gz", compresslevel=6, mode="wb") as f_out:
                    shutil.copyfileobj(f_in, f_out)
                    compressed.append(f2)
            except:
                log_it(n, f"unable to compress: {f2}")
        if os.path.exists(f"{f2}.gz"):
            try:
                os.remove(f2)
            except:
                log_it(n, f"unable to remove un-compressed version of {f2}.gz: {f2}")

    log_it(n, "done with file cleanup")

    ## compress folders given the folder name
    folders_to_compress = my_folders.copy()
    temp = [f"{i}/{j}" for i in ["p1", "p2", "p3"] for j in folders_to_compress]
    folders_to_compress += temp
    cleanup_gz_folder(n, folders_to_compress)
    log_it(n, "done with folder cleanup")
    
    ## compress sub-folders given the folder name
    subfolders_to_compress = compress_subfolder.copy()
    temp = [f"{i}/{j}" for i in ["p1", "p2", "p3"] for j in subfolders_to_compress]
    subfolders_to_compress += temp
    subfolders_to_compress = list_folders(subfolders_to_compress)
    cleanup_gz_folder(n, subfolders_to_compress)
    log_it(n, "done with sub-folder cleanup")


if __name__ == "__main__":
    x = r"\\nemsfs\r\output\mc6\ref2025_solo\d011025an"
    os.chdir(x)
    cleanup(-1, x)
