#!C:\Python311\python.exe

"""
Takes an existing ifort gen-dep file and converts it to a makefile
Used within the makefile to allow functionality similar to GCC -MMD flag
"""

import re
import sys
	
#some header files will be intel, and system headers located in C:/Program Files, ... etc
#add any file location that contains these files here	
excludes = ("C:/PROGRA~2","C:/Program", "(x86)","Files")

class Rule:
	def __init__(self, target="", depends="", recipe=""):
		self.target = target
		self.depends = depends
		self.recipe = recipe
		self.print_cmd = f"@echo \"  > {target}\""
		
	def __str__(self):
		return f"{self.target} : {' '.join(self.depends)}\n\t{self.print_cmd}\n\t@{self.recipe}\n\n"
		
class Makefile:

	header = "# DO NOT DIRECTLY EDIT THIS FILE\n# FILE IS CREATED AUTOMATICALLY BY MAKEFILE\n"
	
	def __init__(self, rules=None):
		self.rules = rules

	def to_file(self, filepath):
		with open(filepath, "w") as f:
			f.write(self.header)
			for rule in self.rules:
				f.write(f"{str(rule)}")
			
class RuleTokenizer:
	
	def __init__(self, gen_dep_filepath):
		with open(gen_dep_filepath, "r", errors="ignore") as f:
			self.gen_dep = f.read()
		
	def get_rules(self):
		toks = self.gen_dep.split("\n\n")
		
		rules = []
		for t in toks:
			# For including modules in the .d file uncomment below
			match = re.search(r"(\w+.obj|\w+.mod) : ([\s\S]*)", t)
			if match: 
				target = match.group(1)
				depends = match.group(2)
				depends = self.clean_depends(depends)
				rules.append(Rule(target=target, depends=depends))
			
		return rules
		
	def clean_depends(self, depends):
		# replace backslash as they cause headaches with regex
		depends = depends.replace('\\', '/')
		depends = depends.replace(' /', '')
		depends = depends.replace('/\n', '')
		depends = depends.replace('\n', '')
		
		# remove duplicates from depends
		depends = list(set(depends.split(" ")))

		# remove all paths in depends that begin with a string in excludes
		depends = [x for x in depends if not x.startswith(excludes)]
				
		return depends
		
def main(filepath, recipe):
	tok = RuleTokenizer(filepath)
	rules = tok.get_rules()
	for rule in rules:
		rule.recipe = recipe
	
	makefile = Makefile(rules=rules)
	makefile.to_file(filepath)
	
	
if __name__ == "__main__":
	main(filepath=sys.argv[1], recipe=' '.join(sys.argv[2:]))
