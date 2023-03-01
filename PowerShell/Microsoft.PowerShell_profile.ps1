Set-PSReadLineKeyHandler -Key Tab -Function MenuComplete

Set-PSReadLineKeyHandler -Chord Ctrl+d -Function DeleteCharOrExit

function exa-full {exa  -alhb --git --icons $args}
New-Alias ll exa-full

oh-my-posh init pwsh --config $PROFILE/../theme.omp.json | Invoke-Expression
$env:POSH_GIT_ENABLED = $true
