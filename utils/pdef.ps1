$ParameterName = $args -join ' '
$normalizedParameterName = $ParameterName -replace '\$','.'
$nemsIncludesPath = Join-Path $env:NEMS "includes"

Get-ChildItem -Path $nemsIncludesPath -Recurse -File -Exclude *.mod |
    Select-String -Pattern "parameter\s*${normalizedParameterName}\s*=" -CaseSensitive -SimpleMatch |
    Where-Object { $_.Line -match "parameter\s*${normalizedParameterName}\s*=" } |
    Select-Object -ExpandProperty Line