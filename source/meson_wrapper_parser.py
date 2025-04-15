import os, shutil, pathlib
def main():
    """Invoke process_wrapper() to parse each wratpper file, save the content in desired format, and print a message to indicate the entire process succeeded.
    """
    process_wrapper('pyfiler1-f2pywrappers.f', 'pyfiler1-f2pywrappers.f-org')
    process_wrapper('pyfiler2-f2pywrappers.f', 'pyfiler2-f2pywrappers.f-org')    
    print("Done with wrapper process!")

def process_wrapper(file, old):
    """Check the existence of a wrapper file, and call read_write() to continue the wrapper prasing.
    If the wrapper file does not exist, return a file not exist message and exit the program.

    Args:
        file (_str_): the path of the wrapper file
        old (_str_): the path of the origin wrapper file
    """
    if not os.path.isfile(file):
        print(f"the {file} does not exist. Exit now.")
        os.sys.exit()

    if os.path.exists(old): pathlib.Path.unlink(old)
    shutil.move(file, old)
    read_write(old, file)

def read_write(read_file, written_file):
    """Read in the wrapper file, remove newline breaks and replace 'C     ' with '!     '. Write and save the cleaned content.

    Args:
        read_file (_str_): the path of the read-in file
        written_file (_str_): the path of the written file
    """
    replacement = '\n     &'
    with open(read_file,'r') as f:
        data=f.read().replace(replacement,'')
        data=data.replace('C     ',r'!     ')

    with open(written_file,'w') as f:
        f.write(data)

if __name__ == "__main__":
   main()