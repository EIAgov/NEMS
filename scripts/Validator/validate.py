from Controller import ReportFormatter
import os
import sys

from DataModel.Model import Model

if __name__ == '__main__':
	#pass in the directory name as input
	print('You are running the NEMS output validation script.')
	print(f"Running in {os.getcwd()}")
	print(f'sys.argv= {sys.argv[1]}')
	import pytest

	xml_report_path = os.path.join(os.getcwd(), 'validator_report.xml')
	#sys.exit()

	model = Model.getInstance()
	model.load_files(sys.argv[1])

	# Tests are run here
	pytest.main(["-o", "junit_family=xunit1", "-v", f"--junitxml={xml_report_path}"])

	# Report is formatted here
	ReportFormatter.main()