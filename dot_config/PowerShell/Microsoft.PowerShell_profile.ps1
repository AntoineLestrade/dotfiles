Set-PSReadLineKeyHandler -Key Tab -Function MenuComplete

Set-PSReadLineKeyHandler -Chord Ctrl+d -Function DeleteCharOrExit

function exa-full {exa  -alhb --git --icons $args}
New-Alias ll exa-full

#Invoke-Expression (&starship init powershell)
$THEME_PATH = Join-Path -Path $script:MyInvocation.MyCommand.Path -ChildPath '\..\theme.omp.json'
oh-my-posh init pwsh --config $THEME_PATH | Invoke-Expression
$env:POSH_GIT_ENABLED = $true
