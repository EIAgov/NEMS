#!C:\Python311\python.exe
#meant to merge the files scedes.all and varkeys.files

import sys

d = {}

def read_file(file):
    with open(file) as f:
        for line in f:
            try:
                (key, val) = line.split("=")
            except ValueError:
                continue
            d[key] = val

def to_file(filename):
    with open(filename, "a") as f:
        for key, value in d.items():
            f.write(f"{key}={value}")


for file in sys.argv[1:-1]:
    read_file(file)

to_file(sys.argv[-1])