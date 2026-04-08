$argPatterns = New-Object System.Collections.Generic.List[string]
for ($i = 0; $i -lt $args.Length; $i++) {
    $currentArg = [regex]::Escape($args[$i])
    $argPatterns.Add("^\s*include\s*\." + $currentArg)
}

if ($argPatterns.Count -gt 0) {
    $searchRegex = ($argPatterns | Join-String -Separator "|")
} else {
    Write-Error "No arguments provided for search."
    exit 1
}

$nemsSourcePath = $env:NEMS + "\source\*.f"
Write-Host "Equivalent command: Select-String -Path '$nemsSourcePath' -Pattern '$searchRegex' -CaseSensitive:`$false -List"
Select-String -Path $nemsSourcePath -Pattern $searchRegex -CaseSensitive:$false -List |
    Select-Object -ExpandProperty Path -Unique