param (
    [string]$MSBuildPath,
    [string]$Project,
    [string]$Platform,
    [string]$Config,
    [string]$Log
)

Write-Host "[PowerShell] Launching MSBuild..."
Start-Process -FilePath $MSBuildPath `
    -ArgumentList "`"$Project`"","/t:Build","/p:Config=$Config","/p:Platform=$Platform" `
    -RedirectStandardOutput $Log `
    -NoNewWindow -Wait

Write-Host "[PowerShell] Streaming logs from $Log..."
Get-Content $Log -Tail 10 -Wait
