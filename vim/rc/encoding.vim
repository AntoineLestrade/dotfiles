set encoding=utf-8

if g:is_windows
    set termencoding=latin1
else
    set termencoding=
endif

"if !has('gui_running')
"    set termencoding= " same as 'encoding'
"elseif g:is_windows
"    set termencoding=cp932
"endif
