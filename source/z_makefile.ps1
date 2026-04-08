<#
.SYNOPSIS
    Builds object files (.obj) for NEMS components using Meson, without running NEMS.

.DESCRIPTION
    This script is provided to troubleshoot and debug the NEMS build process
    outside of the standard NEMS application runtime.

    It does not initiate a NEMS run, it will only perform the necessary steps
    to build the NEMS application and then exit.

.PARAMETER key
    Specifies the key to look up in the configuration CSV file (`CSV_FILE`).
    This is expected to be the name of the key in the `CSV_FILE`.

    Default Value: 'pyver'

.PARAMETER CSV_FILE
    Specifies the path to the CSV configuration file that contains mappings
    for various NEMS options. A key,value mapping is expected.

    Default Value: "../scripts/setup/input/init_configs.csv"

.EXAMPLE
    .\z_makefile.ps1
    Description: Initiates the Meson build for object files using default parameters
                 ('pyver' key and default CSV file).

.EXAMPLE
    .\z_makefile.ps1 -key "pyver" -CSV_FILE "$NEMS/src/setup/input/init_configs.csv"
    Description: Executes the build process using a custom Python environment key
                 and an alternative configuration CSV file.

.NOTES
    Author: Greg Miller <Gregory.Miller@eia.gov>
    Version: 1.0.1
    Date: Thursday, December, 18, 2025
    This script is intended for development and debugging purposes. It is not
    invoked during standard NEMS runtime.
    
    Requires PowerShell 5.0 or later.

    This script must be run from within the $NEMS/source/ folder.
#>
param (
    [string]$key = "pyver",
    [string]$CSV_FILE = "../scripts/setup/input/init_configs.csv"
)

# Intel oneAPI does not support PowerShell
# This code imports the oneAPI environment set by setvars.bat
Write-Debug "Configuring Intel oneAPI Environment in PowerShell."
$tempBatPath = Join-Path $env:TEMP "dump_env.bat"
$tempEnvPath = Join-Path $env:TEMP "env_vars.txt"
Write-Debug "Creating temporary bat file to export oneAPI env."
@"
@echo off
call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat"
set > "$tempEnvPath"
exit 0
"@ | Out-File -FilePath $tempBatPath -Encoding ASCII
Invoke-Expression "& cmd.exe /c `"$tempBatPath`""
if (Test-Path $tempEnvPath) {
    Get-Content $tempEnvPath | ForEach-Object {
        if ($_ -match "^([^=]+)=(.*)$") {
            $varName = $matches[1]
            $varValue = $matches[2]
            Write-Debug "Updating environment with oneAPI variables."
            Set-Item Env:\$varName $varValue
        }
    }
    Remove-Item $tempEnvPath
} else {
    Write-Error "Failed to export oneAPI environment to PowerShell."
    Remove-Item $tempBatPath
    exit 1
}
Remove-Item $tempBatPath

# Parse Python environment from init_configs file
$CSV_FILE = "../scripts/setup/input/init_configs.csv"
if (-Not (Test-Path $CSV_FILE -PathType Leaf)) {
    Write-Error "CSV file could not be found: $CSV_FILE"
    exit 1
}

# Load python environment from init_configs.csv file
$csv_data = Import-Csv -Path $CSV_FILE
$pyenv_line = $csv_data | Where-Object { $_.Key -eq "pyver" }
if ($pyenv_line) {
    $pyenv = $pyenv_line.Value
    . $pyenv\scripts\activate.ps1
} else {
    Write-Error "Could not find $key in CSV file."
    exit 1
}

# Perform meson build
meson setup builddir
meson.exe compile -C builddir
Read-Host -Prompt "Press Enter to continue..."