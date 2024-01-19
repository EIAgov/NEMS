#!C:\Python311\python.exe
"""
This script is used by the makefile to find .d files and add them to the main makefile
"""
import os, fnmatch

d_dir = "./build"
sub_makefile_dir = "./.makefiles"
include_file = "./build/include_d.mk"

def get_correct_makefile_paths(dep_files, sub_makefiles):
	paths = []

	for i in sub_makefiles:
		paths.append(os.path.join(sub_makefile_dir, i))

	for d in dep_files:
		target = d.split(".")[0]
		if not target in [i.split(".")[0] for i in sub_makefiles]:
			paths.append(os.path.join(d_dir, d))

	return paths

def main():
	#find all makefiles in the sub_makefile_dir
	sub_makefiles = []
	if os.path.isdir(sub_makefile_dir):
		sub_makefiles = fnmatch.filter(os.listdir(sub_makefile_dir), '*.mk')

	#find list of all .d files
	if os.path.isdir(d_dir):
		dep_files = fnmatch.filter(os.listdir(d_dir), '*.d')
	else:
		os.mkdir(d_dir)
		dep_files = []

	paths = get_correct_makefile_paths(dep_files, sub_makefiles)
	
	#write them out to a file
	with open(include_file, "w") as f:
		f.write("#DO NOT EDIT OR DELETE THIS FILE\n")
		f.write("#FILE IS CREATED AUTOMATICALLY BY MAKEFILE\n")
		for p in paths:
			f.write(f"include {p}\n")
	
	
if __name__ == "__main__":
	#todo: get output file path as arg, get .d filepath as arg
	main()
