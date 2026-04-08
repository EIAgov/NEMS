<#
.SYNOPSIS
    Performs the Sphinx build process for NEMS documentation.

.DESCRIPTION
    This script builds the Sphinx documentation build for the NEMS project.

    When run the script will run with the environment specified in the init_configs.csv
    pyver key. This can be customized with the -CSV_FILE and -key parameters.
    
    The script will cleanup previous builds then execute several Sphinx commands
    to build the NEMS project documentation.

.PARAMETER key
    Specifies the key to look up in the configuration CSV file (`CSV_FILE`).
    This is expected to be the name of the key in the `CSV_FILE`.

    Default Value: 'pyver'

.PARAMETER CSV_FILE
    Specifies the path to the CSV configuration file that contains mappings
    for various NEMS options. A key,value mapping is expected.

    Default Value: "../scripts/setup/input/init_configs.csv"

.EXAMPLE
    .\build_sphinx.ps1
    Description: Runs the Sphinx build using the default 'pyver' key and CSV file.

.EXAMPLE
    .\build_sphinx.ps1 -key "pyver" -CSV_FILE "$NEMS/src/setup/input/init_configs.csv"
    Description: Runs the Sphinx build using a custom key "dev_env" and a specified configuration file.

.NOTES
    Author: Greg Miller <Gregory.Miller@eia.gov>
    Version: 1.0.1
    Date: Thursday, December, 18, 2025
    This script is intended for development and debugging purposes. It is not
    invoked during standard NEMS runtime.
    
    Requires PowerShell 5.0 or later.
    
    This script should be run from within the $NEMS/docs folder.
#>
param (
    [string]$key = "pyver",
    [string]$CSV_FILE = "../scripts/setup/input/init_configs.csv"
)

if (Test-Path ./build) {
    Write-Warning "Removing previous build."
    Remove-Item ./build -Force -Recurse
}

# Parse Python environment from init_configs file
if (-Not (Test-Path $CSV_FILE -PathType Leaf)) {
    Write-Error "CSV file could not be found: $CSV_FILE"
    exit 1
}

$csv_data = Import-Csv -Path $CSV_FILE
$pyenv_line = $csv_data | Where-Object { $_.Key -eq $key }
if ($pyenv_line) {
    $pyenv = $pyenv_line.Value
    . $pyenv\scripts\activate.ps1
} else {
    Write-Error "Could not find $key in CSV file."
    exit 1
}

Write-Host "Beginning sphinx build process..."
sphinx-apidoc -f -o source/generated/reporter ../models/reporter
sphinx-apidoc -f -o source/generated/converge ../models/converge
sphinx-apidoc -f -o source/generated/source ../source
sphinx-apidoc -f -o source/generated/main ../models/main
sphinx-apidoc -f -o source/generated/ngpl ../models/ngpl
sphinx-apidoc -f -o source/generated/epm ../models/epm
sphinx-apidoc -f -o source/generated/setup ../scripts/setup/src
sphinx-apidoc -f -o source/generated/ccats ../models/ccats
sphinx-build -b html source/ build/html > sphinx_log.txt 2>&1