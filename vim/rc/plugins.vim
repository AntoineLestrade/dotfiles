" Unite {{{
if neobundle#tap('unite.vim') " {{{
    nnoremap [unite] <Nop>
    xnoremap [unite] <Nop>
    nmap ;u [unite]
    xmap ;u [unite]

    " TEST?
    nnoremap [Window] <Nop>
    nmap s [Window]

    " sources
    " File List
    " nnoremap <silent> ;f :<C-u>UniteWithBufferDir -silent -buffer-name=files_rec/async file<CR>
    nnoremap <silent> ;o :<C-u>Unite outline -no-start-insert -resume<CR>
    nnoremap <silent> ;t :<C-u>UniteWithCursorWord -buffer-name=tag tag tag/include<CR>
    xnoremap <silent> ;r d:<C-u>Unite -buffer-name=register -default-action=append register history/yank<CR>
    nnoremap <silent> ;r :<C-u>Unite -buffer-name=register -default-action=append register history/yank<CR>
    nnoremap <silent> <C-k> :<C-u>Unite change jump<CR>
    nnoremap <silent> ;g :<C-u>Unite grep -buffer-name=grep`tabpagenr()` -auto-preview -no-split -no-empty -resume<CR>

nnoremap <silent> [Window]s
\ :<C-u>Unite -buffer-name=files -no-split -multi-line -unique -silent
\ jump_point file_point file_mru
\ file_rec/git buffer_tab:- file file/new<CR>

    " buffers
    "nnoremap <expr><silent> ;b ":\<C-u>Unite -buffer-name
    "
    " Execute help
    nnoremap <F1> :<C-u>Unite -buffer-name=help help<CR>
    nnoremap g<F1> :<C-u>UniteWithCursorWord help<CR>

    function! neobundle#tapped.hooks.on_source(bundle) " {{{
        call unite#custom#profile('default', 'context', { 'prompt': '>> ?' })
        call unite#custom#profile('action', 'context', {
        \ 'start_insert': 1,
        \ })

        if executable('ag')
            let g:unite_source_grep_command = 'ag'
            let g:unite_source_grep_default_opts =
                        \ '-i --line-numbers --nocolor --nogroup --hidden --ignore ' .
                        \ '''.hg'' --ignore ''.svn'' --ignore ''.git'' --ignore ''.bzr'''
            let g:unite_source_grep_recursive_opt = ''
        elseif executable('pt')
            let g:unite_source_grep_command = 'pt'
            let g:unite_source_grep_default_opts = '--nogroup -nocolor'
            let g:unite_source_grep_recursive_opt = ''
        endif

        let g:unite_source_rec_max_cache_files = -1
    endfunction " }}}
    call neobundle#untap()
endif " }}}
" }}}
" Snippets / Completion {{{
if neobundle#tap('neocomplete.vim') " {{{
    let g:neocomplete#enable_at_startup = 1
    "function! neobundle#tapped.hooks.on_source(bundle) " {{{
 "       let g:neocomplete#disable_auto_complete = 0
 "       let g:neocomplete#enable_insert_char_pre = 0

 "       let g:neocomplete#enable_smart_case = 1
 "       let g:neocomplete#enable_camel_case = 1

 "       let g:neocomplete#enable_fuzzy_completion = 1

 "       let g:neocomplete#sources#syntax#min_keyword_length = 3
 "       let g:neocomplete#auto_completion_start_length = 2
 "       let g:neocomplete#manual_completion_start_length = 0
 "       let g:neocomplete#min_keyword_length = 3

 "       let g:neocomplete#enable_complete_select = 1
 "       try
 "           let completeopt_save = &completeopt
 "           set completeopt+=noinsert,noselect
 "       catch
 "           let g:neocomplete#enable_complete_select = 0
 "       finally
 "           let &completeopt = completeopt_save
 "       endtry
 "       let g:neocomplete#enable_auto_select = 1
 "       let g:neocomplete#enable_refresh_always = 1
 "       let g:neocomplete#enable_cursor_hold_i = 0

 "       let g:neocomplete#enable_auto_delimiter = 1

 "       let g:neocomplete#max_list = 100
 "       let g:neocomplete#force_overwrite_completefunc = 1
 "       if !exists('g:neocomplete#sources#omni#input_patterns')
 "           let g:neocomplete#sources#omni#input_patterns = {}
 "       endif

 "       if !exists('g:neocomplete#sources')
 "           let g:neocomplete#sources = {}
 "       endif
        "let g:neocomplete#sources.rust = ['buffer', 'omni']
        " if patter matches, local omnifunc will be called
if !exists('g:neocomplete#sources#omni#input_patterns')
    let g:neocomplete#sources#omni#input_patterns = {}
endif
let g:neocomplete#sources#omni#input_patterns.rust =
    \ '[^.[:digit:] *\t]\%(\.\|\::\)\%(\h\w*\)\?'

 "   endfunction " }}}
    call neobundle#untap()
endif " }}}
" }}}
" Development {{{
" ## SQL {{{
if neobundle#tap('sqlserver.vim') " {{{
    let g:sql_type_default = "sqlserver"
    call neobundle#untap()
endif " }}}
" ## }}}
if neobundle#tap('racer') " {{{
    let g:racer_cmd = "racer.exe"
    let $RUST_SRC_PATH="D:/ProgramFiles/Rust_src/src"
    call neobundle#untap()
endif " }}}
" }}}
" ## Syntastic {{{
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wg = 0
let g:syntastic_vimlint_options = { 'EVL103': 1 }
" ## }}}
" }}}
" vim: set foldmethod=marker et sts=4 sw=4:
