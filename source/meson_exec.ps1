<#
.SYNOPSIS
    Manages the build process for NEMS Python modules and related components.

.DESCRIPTION
    This script facilitates the build process of the NEMS project using the
    Meson build system to compile NEMS' Fortran components.

.PARAMETER key
    Specifies the key to look up in the configuration CSV file (`CSV_FILE`).
    This is expected to be the name of the key in the `CSV_FILE`.

    Default Value: 'pyver'

.PARAMETER CSV_FILE
    Specifies the path to the CSV configuration file that contains mappings
    for various NEMS options. A key,value mapping is expected.

    Default Value: "../scripts/setup/input/init_configs.csv"

.EXAMPLE
    .\meson_exec.ps1
    Description: Executes the build process using default parameters, activating the 'pyver'
                 Python environment and using the default config file.

.EXAMPLE
    .\meson_exec.ps1 -key "pyver" -CSV_FILE "$NEMS/src/setup/input/init_configs.csv"
    Description: Starts the build process using a custom Python environment key
                 and a specified configuration CSV file path.

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

# Preserve original dir location similar to original script
Push-Location $PSScriptRoot 

# Parse Python environment from init_configs file
if (-Not (Test-Path $CSV_FILE -PathType Leaf)) {
    Write-Error "CSV file could not be found: $CSV_FILE"
    exit 1
}

# Test for ifort and fail if not found.
# TODO init_configs value or other method to detect ifx vs ifort? Hardcode ifx in future?
try { ifort --help } 
catch {
    Write-Error "Failed to access ifort. Try running meson_exec.bat"
    exit 1
}

# Load python environment from init_configs.csv file
$csv_data = Import-Csv -Path $CSV_FILE
$pyenv_line = $csv_data | Where-Object { $_.Key -eq $key }
if ($pyenv_line) {
    $pyenv = $pyenv_line.Value
    . $pyenv\scripts\activate.ps1
} else {
    Write-Error "Could not find $key in CSV file."
    exit 1
}

# Activate Python environment
. $pyenv\scripts\activate.ps1

# Perform build process
Write-Host "building pyf"
python PyFilerf2py_build.py
python -m numpy.f2py pyfiler1.pyf

Write-Host "Parsing wrapper and module.c"
python meson_wrapper_parser.py
python meson_module_c_parser.py

# Sleep 5 to remain consistent with original script behavior
Start-Sleep -Seconds 5

Write-Host "Performing \`meson build\` command to generate pyd file."
Write-Host "Cleaning old builddir."
Remove-Item -LiteralPath "builddir" -Recurse -Force -ErrorAction -SilentlyContinue

meson setup builddir
meson compile -C builddir

Write-Host "pyd build complete."
Read-Host "Press Enter/Return to exit."

Pop-Location