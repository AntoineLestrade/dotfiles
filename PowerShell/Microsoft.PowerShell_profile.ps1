Set-PSReadLineKeyHandler -Key Tab -Function MenuComplete
oh-my-posh init pwsh --config $PROFILE/../theme.omp.json | Invoke-Expression
$env:POSH_GIT_ENABLED = $true
