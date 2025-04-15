import os
from Controller import ReportFormatter
from DataModel.Model import Model

def main():
	#pass in the directory name as input
	print('You are running the NEMS output validation script.')

	# grab the info of path of output\validator folder and the output root 
	validator_dir = os.path.dirname(os.path.abspath(__file__))
	output_root_dir = os.path.dirname(validator_dir)
	print(f"Running from {output_root_dir}")

	run_validate(validator_dir, output_root_dir)

def run_validate(validator_dir, output_root_dir):
	import pytest

	os.chdir(validator_dir)
	# validator_report.xml shall exist in the same folder of this validate.py file
	xml_report_path = os.path.join((validator_dir), 'validator_report.xml')
	print(xml_report_path)

	model = Model.getInstance()
	model.load_files(output_root_dir)

	# Tests are run here
	pytest.main(["-o", "junit_family=xunit1", "-v", f"--junitxml={xml_report_path}"])

	# Report is formatted here
	ReportFormatter.main()

if __name__ == '__main__':
	main()