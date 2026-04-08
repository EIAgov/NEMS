$uc = $args[0].ToUpper()
$lc = $args[0].ToLower()
$mc = ($uc[0])+($lc.substring(1))

$nemspath = $env:NEMS + "\includes\*"
$exclude = "*.mod"

$regex = "\s$([regex]::Escape($arg))[\s\t(]|\s$([regex]::Escape($arg))$|(?i)parameter.*$([regex]::Escape($arg))"

$initial_results = Get-ChildItem -Path $nemspath -Recurse -File -Exclude $exclude -ErrorAction SilentlyContinue | 
    Select-String -Pattern $regex -CaseSensitive:$false | 
    Select-Object -ExpandProperty Line

# strip comments
$commentRemovePattern1 = "!\s.*$([regex]::Escape($lc)).*$"
$commentRemovePattern2 = "!\s.*$([regex]::Escape($uc)).*$"
$commentRemovePattern3 = "!\s.*$([regex]::Escape($mc)).*$"

$filteredResults = @()
foreach ($line in $initial_results) {
    $tempLine = $line -replace $commentRemovePattern1, ""
    $tempLine = $tempLine -replace $commentRemovePattern2, ""
    $tempLine = $tempLine -replace $commentRemovePattern3, ""
    $filteredResults += $tempLine
}

$regex2 = "\s$([regex]::Escape($arg))[\s\t(,)]|\s$([regex]::Escape($arg))$"
$filtered_results | Where-Object { $_ -match $regex2 }