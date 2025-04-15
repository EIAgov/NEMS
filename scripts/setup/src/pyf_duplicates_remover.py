import os, shutil, pathlib, copy
from os.path import dirname

def main():
    """The main flow controller for this pyf duplicates remover automation.
    Define the file path of pyfiler1.pyf and pyfiler1.pyf and call process_pyf() on both pyfiler1.pyf and pyfiler2, to remove their duplicates.
    Print a message to indicate the succeed when the removal is done without exception.
    """
    NEMS_path=dirname(dirname(dirname(dirname(os.path.realpath(__file__)))))
    file_path=os.path.join(NEMS_path,'source')
    f1=os.path.join(file_path,'pyfiler1.pyf')
    f2=os.path.join(file_path,'pyfiler2.pyf')
    line_number_module_utils_starts, line_number_module_other_starts, line_number_for_end_module_other=locate_baseline_and_scan_lists(f1)
    
    # now check and process the pyfiler1.pyf and pyfiler2.pyf
    process_pyf(f1, line_number_module_utils_starts, line_number_module_other_starts, line_number_for_end_module_other)
    process_pyf(f2, line_number_module_utils_starts, line_number_module_other_starts, line_number_for_end_module_other)    

    print("done with pyf duplicates removal!")

def locate_baseline_and_scan_lists(file):
    """In the given file, locate the module utils and the module other, and return the line numbers that the module utils starts, the module other starts and ends.

    Args:
        file (_str_): the file path where the .pyf file locates

    Returns:
        line_number_module_utils_starts (_int_): the line number where the module utils starts in the pyf file.
        line_number_module_others_starts (_int_): the line number where the module other starts in the pyf file.
        line_number_for_end_module_other (_int_): the line number where the module other ends in the pyf file.
    """
    lines = open(file).readlines()
    lines = [line.strip() for line in lines]

    s1='module utils ! in :'
    s2='module other ! in :'
    s3='end module other'
    len_s1=len(s1)
    len_s2=len(s2)
    len_s3=len(s3)
    line_number_module_utils_starts = -1    #6
    line_number_module_other_starts = -1    #5447
    line_number_for_end_module_other = -1   #6152

    for idx,line in enumerate(lines):
        if line_number_module_utils_starts== -1 and line[:len_s1]==s1:
            line_number_module_utils_starts=idx+1
            continue
        elif line_number_module_other_starts==-1 and line[:len_s2]==s2:
            line_number_module_other_starts=idx+1
            continue
        elif line_number_for_end_module_other==-1 and line[:len_s3]==s3:
            line_number_for_end_module_other=idx+1
            continue
        if line_number_module_utils_starts>-1 and line_number_module_other_starts>-1 and line_number_for_end_module_other>-1:
            break
    
    return line_number_module_utils_starts, line_number_module_other_starts, line_number_for_end_module_other

def process_pyf(file, line_number_module_utils_starts, line_number_module_other_starts, line_number_for_end_module_other):
    """The major process pyf file method. Read in a pyf file, process the file, and produce the result with the same filename.
    The work is done with several steps:
    step 1) swap the filename with appending '-org' ('-org' stands for 'the origin').
    step 2) split the file content into 5 lists - the module utils list, the module other list, the need-to-scan area list,
      the temporary result list including the header part of the pyf until the module utils starts,
      and the ending lines list which is the footer part of the pyf after the need-to-scan area ends.
    step 3) do feature engineering in the module utils list and the module other list, to discard irrelevant subroutines etc. lines.
    step 4) call get_duplicates_checklist() to get a duplicates checklist as the scan standard.
    step 5) scan through the duplicates, parse and compose the temporary result list.
    step 6) assemble the result list with the footer ending lines list. Write out the result back to the pyf file.

    Args:
        file (_str_): the file path where the .pyf file locates
        line_number_module_utils_starts (_int_): the line number where the module utils starts in the pyf file.
        line_number_module_others_starts (_int_): the line number where the module other starts in the pyf file.
        line_number_for_end_module_other (_int_): the line number where the module other ends in the pyf file.
    """
    old,dst=swap_filename(file)

    result_list, lines_need_scan, ending_lines, module_utils_lines, module_other_lines=split_lists(old, line_number_module_utils_starts, line_number_module_other_starts, line_number_for_end_module_other)

    #exclude subroutine lines
    module_utils_lines=feature_engineering_lines(module_utils_lines)
    module_other_lines=feature_engineering_lines(module_other_lines)    
    duplicates_checklist=get_duplicates_checklist(module_utils_lines,module_other_lines)
    result_list=parse_and_compose(result_list, lines_need_scan, duplicates_checklist)

    with open(dst,'w+') as f:        
        f.write(''.join(item for item in result_list))
        f.write(''.join(item for item in ending_lines))

def feature_engineering_lines(ls):
    """This method is to discard irrelevant subroutine lines in the module utils and module other lists, since subroutine codes can be excluded on scan.

    Args:
        ls (_list_): the list needs to be feature engineering. It can be either module utils list or module other list

    Returns:
        ls (_list_): the cleaned list
    """
    # exclude subroutine lines
    pattern='subroutine'
    pattern_found_idx=-1
    for idx, e in enumerate(ls):
        t = e.strip()
        if t.startswith(pattern):
            pattern_found_idx=idx
            break
    if pattern_found_idx != -1:
        return ls[:idx]
    else:
        return ls
    
def swap_filename(dst):
    """Check the existance of the given .pyf file. If it does not exist, print the file not exist message and exit.
    Otherwise, append '-org' ('-org' stands for 'the origin') on the filename.

    Args:
        dst (_str_): the file path where the input .pyf file locates

    Returns:
        old (_str_): the file which the input .pyf file was renamed.
        dst (_str_): the file which will be cleaned without duplicates later.
    """
    old=dst+'-org'

    if not os.path.isfile(dst):
        print(f"the {dst} does not exist. Exit now.")
        os.sys.exit()

    if os.path.exists(old): pathlib.Path.unlink(old)
    shutil.move(dst,old)

    return old,dst

def parse_and_compose(result_list, lines_need_scan, duplicates_checklist):
    """This method scans through the need-scanned-lines list, parse and skip the certain patterns
    (the lines starts with ['module','subroutine','end module','end subroutine','end interface','python module']),
    remove duplicates, and compose the result list.

    Args:
        result_list (_list_): the result list was cutted from split_list().
        lines_need_scan (_list_): the list required to scan through
        duplicates_checklist (_list_): the baseline list to use as the standard to scan through the need-scanned-lines list

    Returns:
        result_list (_list_): the cleaned, no duplicates result list. This result list be used later in process_pyf(), to dump the content back to the .pyf file.
    """
    skip_scan_pattern=['module','subroutine','end module','end subroutine','end interface','python module']
    for line in lines_need_scan:
        line_trimmed = line.strip()
        shall_skip_flag=is_skipped(skip_scan_pattern,line_trimmed)
        if shall_skip_flag:
            result_list.append(line)
        else:
            # start to check if the line is duplicated with the lines in the module utils
            if line in duplicates_checklist:
                continue
            else:
                result_list.append(line)

    return result_list

def get_duplicates_checklist(module_utils_lines,module_other_lines):
    """Compare the module other list with the module utils list, remove the duplicates, 
    and generate a distinct line list as the duplicates checklist.

    Args:
        module_utils_lines (_list_): the line list of the module utils
        module_other_lines (_list_): the line list of the module other

    Returns:
        duplicates_checklist (_list_): the baseline list to use as the standard to scan through the need-scanned-lines list in parse_and_compose().
    """
    duplicates_checklist = copy.deepcopy(module_utils_lines)
    for item in module_other_lines:
        if item in module_utils_lines:
            continue
        else:
            duplicates_checklist.append(item)
    return duplicates_checklist

def split_lists(file, line_number_module_utils_starts, line_number_module_other_starts, line_number_for_end_module_other):
    """Read in the file content and divide it into 5 lists: the potential result list, the list of the need-to-scan/examine lines,
    the footer part after the need-to-scan lines, the line list of the module utils, the line list of the module other.

    Args:
        file (_str_): the file path where the .pyf file locates
        line_number_module_utils_starts (_int_): the line number where the module utils starts in the pyf file.
        line_number_module_others_starts (_int_): the line number where the module other starts in the pyf file.
        line_number_for_end_module_other (_int_): the line number where the module other ends in the pyf file.

    Returns:
        result_list (_list_): the result list for the final .pyf file later.
        lines_need_scan (_list_): the list required to scan through.
        ending_lines (_list_): the footer part of the pyf after the end of the need-to-scan lines area.
        lines[line_number_module_utils_starts:line_number_module_other_starts-2] (_list_): the module utils lines list.
        lines[line_number_module_other_starts: line_number_for_end_module_other-1] (_list_): the module other lines list.
    """
    lines = open(file).readlines()
    # now divide the line list to 3 parts: the top part till end of module other, the lines need to be scanned, and the ending after the scanning lines to EOF.
    result_list = lines[:line_number_for_end_module_other]
    lines_need_scan = lines[line_number_for_end_module_other:-4]
    ending_lines = lines[-4:]
    return result_list, lines_need_scan, ending_lines, lines[line_number_module_utils_starts:line_number_module_other_starts-2], lines[line_number_module_other_starts: line_number_for_end_module_other-1]

def is_skipped(skip_scan_pattern,checked_line):
    """The method to search the skip-scan pattern on a line. Return True if the line can be skipped.
    Or, return False if the line can not be skipped.

    Args:
        skip_scan_pattern (_list_): the pattern list to indicate the content line can be be skipped
        checked_line (_str_): the line string need to be examinated and checked

    Returns:
        shall_skip_flag (_bool_): True or False to tell if the line can be skipped.
    """
    shall_skip_flag=False
    for e in skip_scan_pattern:
        if checked_line[:len(e)] == e:
            shall_skip_flag=True
            break
    return shall_skip_flag

if __name__ == "__main__":
   main()