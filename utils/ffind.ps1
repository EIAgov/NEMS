if ($args.Count -gt 0) {
    $file = $args[0]
    Write-Host "Files with the string $file"
} else {
    Write-Host -NoNewline "Enter all or part of the file name to search for: "
    $file = Read-Host
    Write-Host "Files with the string $file"
}

Write-Host "find command is:  Get-ChildItem -Recurse | Where-Object { $_.Name -like ""*$file*"" }"
Get-ChildItem -Recurse | Where-Object { $_.Name -like "*$file*" }