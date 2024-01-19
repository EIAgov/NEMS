# Simple script to Unzip archives from www.pnezilla.net/tutorials/python/scripting

import sys
import os
from zipfile import ZipFile, ZIP_DEFLATED

def unzip( path ):
    # Create a ZipFile Object Instance
    archive = ZipFile(path, "r", ZIP_DEFLATED)
    names = archive.namelist()
    for name in names:
        temp = open(name, "wb") # create the file
        data = archive.read(name) #read the binary data
        temp.write(data)
        temp.close()
    archive.close()
    return "\""+path+"\" was unzipped successfully."
    
instructions = "This script unzips zipfiles:"+\
               "e.g.:  python unzipit.py myfiles.zip"

if __name__=="__main__":
        msg = unzip(sys.argv[1])
