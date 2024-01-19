#!/usr/bin/env python3
import os
import subprocess
from enum import Enum
from dataclasses import dataclass

class TfilerFiles:

    def __init__(self):
        self.dictionary = f"{os.environ['NEMS']}/input/dict.txt"
        self.restarti = RestartFile(f"{os.environ['NEMS']}/input/restart.unf", RestartFormat.UNFORMATTED)
        self.varlist = f"{os.environ['NEMS']}/input/varlist.txt"
        self.restarto = RestartFile(f"{os.path.join(os.getcwd(), 'restart.txt')}", RestartFormat.FORMATTED)

    def to_file(self):
        with open("tfiler.files", "w") as f:
            f.write(self.to_string())

    def to_string(self):
        return "\n".join([
            f"Dictionary {self.dictionary}",
            f"Restarti {self.restarti.filepath}",
            f"In Format {self.restarti.format}",
            f"Varlist {self.varlist}",
            f"Restarto {self.restarto.filepath}",
            f"Out Format {self.restarto.format}"
        ])


    def from_file(self):
        with open("tfiler.files", "r") as f:
            self.dictionary = f.readline().split()[1]
            path = f.readline().split()[1]
            formatvalue = int(f.readline().split()[2])
            self.restarti = RestartFile(path, RestartFormat(formatvalue))
            self.varlist = f.readline().split()[1]
            path = f.readline().split()[1]
            formatvalue = int(f.readline().split()[2])
            self.restarto = RestartFile(path, RestartFormat(formatvalue))

      
class RestartFormat(Enum):
    FORMATTED = 0
    UNFORMATTED = 1
    WK1 = 3
    CSV = 4
    GDX = 5
    AIMMS = 7

    def __str__(self):
        if self.value == 0:
            return "0 Formatted"
        elif self.value == 1:
            return "1 Unformatted"
        else: 
            return str(self.value)

class RestartFile:

    def __init__(self, filepath="", format=RestartFormat.UNFORMATTED):
        self._filepath=filepath
        self.format = format

    @property
    def filepath(self):
        return self._filepath

    #Sets the filepath of the restart file, and changes RestartFormat to match
    @filepath.setter
    def filepath(self, path):
        self._filepath = path
        
        if path.endswith(".txt"):
            self.format= RestartFormat.FORMATTED
            try: 
                with open(path) as f:
                    first_line = f.readline()
                if first_line.strip().upper() == "COMPOSITE TABLE":
                    self.format = RestartFormat.AIMMS
            except:
                print("Unable to open restart file")

        elif path.endswith(".csv"):
            self.format = RestartFormat.CSV
        elif path.endswith(".unf"):
            self.format = RestartFormat.UNFORMATTED
        elif path.endswith(".gdx"):
            self.format = RestartFormat.GDX
        elif path.endswith(".wk1"):
            self.format= RestartFormat.WK1
        else:
            self.format = RestartFormat.UNFORMATTED

    
    #Give as input a RestartFormat object and the filename will be appropriately set to match
    def from_format(self,format):
        if format == RestartFormat.UNFORMATTED:
            path = "restart.unf"
        elif format == RestartFormat.FORMATTED:
            path = "restart.txt"
        elif format == RestartFormat.CSV:
            path = "restart.csv"
        elif format == RestartFormat.GDX:
            path = "restart.gdx"
        elif format == RestartFormat.WK1:
            path = "restart.wk1"
        elif format == RestartFormat.AIMMS:
            path = "restart.txt"
        else:
            path = "restart.txt"

        self.format = format
        self._filepath = os.path.join(os.getcwd(), path)

def clearConsole():
    command = 'clear'
    if os.name in ('nt', 'dos'):
        command = 'cls'
    os.system(command)

def main_menu():
    clearConsole()
    print("TFILER Setup Menu\n")
    print("\t1> Edit Tfiler.files\n\t2> Begin TFILER\n\t3> Change source code directory\n\t4>Quit Program")
    choice = eval(input("Enter Selected Option: "))

    if choice == 1:
        files_menu()
    elif choice == 2:
        run_tfiler()
    elif choice == 3:
        print(f"The current location of your NEMS git repository is set to: {os.environ['NEMS']}")
        print("The recommended way to change this is by running \". ./scripts/commands.sh\" from the NEMS source directory")
    else:
        quit()

#Menu driven program to edit tfiler.files 
def files_menu():
    clearConsole()
    print("Editing Tfiler.files\n")
    tfiler = TfilerFiles()
    
    if os.path.isfile("tfiler.files"):
        print("Found an existing tfiler.files in this directory.")
        try: 
            tfiler.from_file()
        except: 
            print("Error opening existing tfiler.files. We will just make a new one.")
    
    print(f"The contents of the existing tfiler.files:\n{tfiler.to_string()}\n")
    while True:
        print("Which of these options would you like to change?")
        print("\t1> Dictionary File\n\t2> Input Restart File\n\t3> Varlist File\n\t4> Output Restart Format\n\t5> Go back to main menu. ")
        choice = eval(input("Enter Selected Option: "))

        if choice == 1:
            path = input("Enter Dictionary Filepath: ")
            tfiler.dictionary = path
        elif choice == 2:
            path = input("Enter Input Restart Filepath: ")
            restart = RestartFile()
            restart.filepath = path
            tfiler.restarti = restart
        elif choice == 3:
            path = input("Enter Varlist Filepath: ")
            tfiler.varlist = path
        elif choice == 4:
            print("Specify the format for the output restart file:")
            print("\t0> Formatted (text)\n\t1> Unformattted (binary)\n\t2> not available yet\n\t3> PC WK1 format and thus unopenable\n\t4> Comma delimited (csv) format\n\t5> GAMS GDX format\n\t6> not used\n\t7> AIMMS Composite Table format")
            formatvalue = eval(input("Enter Selected Option: "))
            
            restart = RestartFile()
            restart.from_format(RestartFormat(formatvalue))
            tfiler.restarto = restart
        else:
            main_menu()
            break

        tfiler.to_file()

#compile and run the tfiler fortran program
def run_tfiler():
    subprocess.run(f"make -c {os.environ['NEMS']}/source tfiler.exe", shell=True, check=False)
    subprocess.run(f"cp {os.environ['NEMS']}/source/tfiler.exe .", shell=True, check=False)
    subprocess.run(f"tfiler.exe", shell=True, check=False)

if __name__ == "__main__":
    main_menu()