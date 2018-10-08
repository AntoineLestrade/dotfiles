let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wg = 0
function! s:syntastic() " {{{
    SyntasticCheck
    call lightline#update()
endfunction " }}}
augroup AutoSyntastic " {{{
    autocmd!
    autocmd BufWritePost *.c,*.cpp,*.perl,*.py,*rs,vimrc,*.vim call s:syntastic()
augroup END " }}}
" vim: set foldmethod=marker et sts=4 sw=4:
" vim:ft=vim fileformat=unix:
