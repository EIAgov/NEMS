import os, shutil, pathlib

def main():
    """Invoke process_module_c() to cut each module.c file, save the content in desired format, and print a message to indicate the entire process succeeded.
    """    
    process_module_c('pyfiler1module.c', 'pyfiler1module.c-org')
    process_module_c('pyfiler2module.c', 'pyfiler2module.c-org')
    print("Done with module.c cut process!")

def process_module_c(file, old):
    """Check the existence of a module.c file, and call read_write() to continue the module.c prasing.
    If the module.c file does not exist, return a file not exist message and exit the program.

    Args:
        file (_str_): the path of the module.c file
        old (_str_): the path of the origin module.c file
    """
    if not os.path.isfile(file):
        print("the pyfilermodule.c does not exist. Exit now.")
        os.sys.exit()

    if os.path.exists(old):
        pathlib.Path.unlink(old)

    shutil.move(file, old)
    read_write(old, file, 16000)
   
def read_write(read_file, written_file, jump):  
    """Read in the module.c file and search for the '"COMMON blocks:' area and calls cut_block() to cut the area in desired format 
    and save back to the written file.

    Args:
        read_file (_str_): the path of the read-in file
        written_file (_str_): the path of the written file
        jump (_int_): the number of the chars to be cut each time. For example, 16000
    """
    lines = open(read_file).readlines()

    special='"COMMON blocks:'
    with open(written_file,'w+') as f:
        for line in lines:
            if special[1:] in line[1:15]:
                print("---yes! cut---")
                # before parse and cut, truncate the head and tail
                line=line[1:-1]
                cut_list=cut_block(line,jump)
                for c in cut_list:
                    f.write(f'{c}\n')
            else:
                f.write(line)

def cut_block(entire_str, jump):
    """

    Args:
        entire_str (_str_): the string block need to be properly cut
        jump (_int_): the number of the chars to be cut each time. For example, 16000

    Returns:
        _list_: the list of the string with the desired format
    """
    end_pos=len(entire_str)
    # decide the iterration times
    iter=(end_pos//jump)+1
    start=0
    end=jump

    result_list=[]
    seperators=['),',',']
    shall_continue=True
    while shall_continue:
        s=""
        is_seperator_switched = False
        if end < (end_pos):
            # now slice and find the last "),"
            tmp=entire_str[start:end]
            seperator= seperators[1] if is_seperator_switched else seperators[0]
            last_comma_index=start+tmp.rfind(seperator)+len(seperator)

            if tmp.rfind(seperator)== -1:
                # ths first kind of seperators is not found. set to use the 2nd kind of seperators
                is_seperator_switched = True
                seperator= seperators[1] if is_seperator_switched else seperators[0]
                last_comma_index=start+tmp.rfind(seperator)+len(seperator)

            # reset the end index. Cut the str up to and include the seperator
            end=last_comma_index
            s=entire_str[start:end]

            # embed the cut in double quotes and add a line break
            s=f'"{s}"'
            result_list.append(s)

            start=end
            end=start+jump
        else:
            end=end_pos
            # now print the last part. No need to append double quote in the end
            s=entire_str[start:end]
            s=f'"{s}'
            result_list.append(s)
            shall_continue=False

    return result_list

if __name__ == "__main__":
   main()