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
    nnoremap <silent> [unite]f :<C-u>UniteWithBufferDir -silent -buffer-name=files_rec/async file<CR>
"    nnoremap <expr><silent> [unite]f :<C-u>Unite -bugger-name=files 
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
    endfunction " }}}
    call neobundle#untap()
endif " }}}
" }}}
" Snippets / Completion {{{
if neobundle#tap('neocomplete.vim') " {{{
    let g:neocomplete#enable_at_startup = 1
    function! neobundle#tapped.hooks.on_source(bundle) " {{{
        let g:neocomplete#disable_auto_complete = 0
        let g:neocomplete#enable_insert_char_pre = 0

        let g:neocomplete#enable_smart_case = 1
        let g:neocomplete#enable_camel_case = 1

        let g:neocomplete#enable_fuzzy_completion = 1

        let g:neocomplete#sources#syntax#min_keyword_length = 3
        let g:neocomplete#auto_completion_start_length = 2
        let g:neocomplete#manual_completion_start_length = 0
        let g:neocomplete#min_keyword_length = 3

        let g:neocomplete#enable_complete_select = 1
        try
            let completeopt_save = &completeopt
            set completeopt+=noinsert,noselect
        catch
            let g:neocomplete#enable_complete_select = 0
        finally
            let &completeopt = completeopt_save
        endtry
        let g:neocomplete#enable_auto_select = 1
        let g:neocomplete#enable_refresh_always = 1
        let g:neocomplete#enable_cursor_hold_i = 0

        let g:neocomplete#enable_auto_delimiter = 1

        let g:neocomplete#max_list = 100
        let g:neocomplete#force_overwrite_completefunc = 1
        if !exists('g:neocomplete#sources#omni#input_patterns')
            let g:neocomplete#sources#omni#input_patterns = {}
        endif

    endfunction " }}}
endif " }}}
" }}}
" Development {{{
" ## SQL {{{
if neobundle#tap('sqlserver.vim') " {{{
    let g:sql_type_default = "sqlserver"
    call neobundle#untap()
endif " }}}
" ## }}}
" }}}
" UI {{{
if neobundle#tap('lightline.vim') " {{{
    set laststatus=2
    set noshowmode
    " let g:lightline = {... {{{
    let g:lightline = {
    \   'colorscheme': 'jellybeans',
    \   'active': {
    \       'left': [
    \           ['mode', 'paste'],
    \           ['git_changes', 'readonly', 'fugitive'],
    \           ['ctrlpmark', 'filename']
    \       ],
    \       'right': [
    \           ['lineinfo'],
    \           ['percent'],
    \           ['fileformat', 'fileencoding', 'filetype', '#warningmsg#', 'syntastic', '*']
    \       ]
    \   },
    \   'component': {
    \       'paste': '%{&paste?"!":""}'
    \   },
    \   'component_function': {
    \     'mode'         : 'MyMode',
    \     'git_changes'  : 'MyGitGutterStatus',
    \     'fugitive'     : 'MyFugitive',
    \     'filename'     : 'MyFilename',
    \     'readonly'     : 'MyReadonly',
    \     'ctrlpmark'    : 'CtrlPMark',
    \     'bufferline'   : 'MyBufferline',
    \     'fileformat'   : 'MyFileformat',
    \     'fileencoding' : 'MyFileencoding',
    \     'filetype'     : 'MyFiletype'
    \   },
    \   'component_expand': {
    \       'syntastic': 'SyntasticStatuslineFlag',
    \   },
    \   'component_type': {
    \       'syntastic': 'middle',
    \   },
    \   'subseparator': {
    \       'left': '|', 'right': '|'
    \   }
    \ }
    " }}}
    " let g:lighline.mode_map = {... {{{
    let g:lightline.mode_map = {
        \ 'n'      : ' N ',
        \ 'i'      : ' I ',
        \ 'R'      : ' R ',
        \ 'v'      : ' V ',
        \ 'V'      : 'V-L',
        \ 'c'      : ' C ',
        \ "\<C-v>" : 'V-B',
        \ 's'      : ' S ',
        \ 'S'      : 'S-L',
        \ "\<C-s>" : 'S-B',
        \ '?'      : '      ' } " }}}
function! MyModified() " {{{
    return &ft =~ 'help' ? '' : &modified ? '+' : &modifiable ? '' : '-'
endfunction " }}}
function! MyFilename() " {{{
    let fname = expand('%:t')
    let mod = MyModified()
    return fname == 'ControlP' ? g:lightline.ctrlp_item :
                \ fname == '__Tagbar__' ? g:lightline.fname :
                \ fname =~ '__Gundo\|NERD_tree' ? '' :
                \ &ft == 'vimfiler' ? vimfiler#get_status_string() :
                \ &ft == 'unite' ? unite#get_status_string() :
                \ &ft == 'vimshell' ? vimshell#get_status_string() :
                \ ('') .
                \ ('' != fname ? fname : '[No Name]') .
                \ ('' != mod ? ' ' . mod : '')
endfunction " }}}
function! MyMode() " {{{
    let fname = expand('%:t')
    return fname == '__Tagbar__' ? 'Tagbar' :
                \ fname == 'ControlP' ? 'CtrlP' :
                \ winwidth('.') > 60 ? lightline#mode() : ''
endfunction " }}}

function! MyGitGutterStatus() " {{{
    if !exists('GitGutterGetHunxSummary')
        return ''
    endif
    let hunks = GitGutterGetHunkSummary()
    let symbols = ['+', ' ~', ' -']
    if empty(hunks) || !exists('*fugitive#head') || !strlen(fugitive#head())
        return ''
    else
        let res = ''
        for i in [0, 1, 2]
            let res .= symbols[i] . hunks[i]
        endfor
        return res
    endif
endfunction " }}}

function! MyFugitive() " {{{
    try
        if expand('%:t') !~? 'Tagbar' && exists('*fugitive#head')
            "let mark = '± '
            let _ = fugitive#head()
            "return strlen(_) ? mark._ : ''
            return _
        endif
    catch
    endtry
    return ''
endfunction " }}}

function! MyReadonly() " {{{
    return &ft !~? 'help' && &readonly ? '≠' : '' " or ⭤
endfunction " }}}

function! CtrlPMark() " {{{
    if expand('%:t') =~ 'ControlP'
        call lightline#link('iR'[g:lightline.ctrlp_regex])
        return lightline#concatenate([g:lightline.ctrlp_prev, g:lightline.ctrlp_item
                    \ , g:lightline.ctrlp_next], 0)
    else
        return ''
    endif
endfunction " }}}

function! MyFileformat() " {{{
    return winwidth(0) > 90 ? &fileformat : ''
endfunction " }}}

function! MyFiletype() " {{{
    return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype : 'no ft') : ''
endfunction " }}}

function! MyFileencoding() " {{{
    if winwidth(0) < 80
        return ''
    endif
    let baseenc = strlen(&fenc) ? &fenc : &enc
    if (&bomb)
        let baseenc = baseenc . ' BOM'
    endif
    return baseenc
endfunction " }}}
augroup AutoSyntastic " {{{
    autocmd!
    autocmd BufWritePost *.c,*.cpp,*.perl,*.py,*rs call s:syntastic()
augroup END " }}}
function! s:syntastic() " {{{
    SyntasticCheck
    call lightline#update()
endfunction " }}}

    call neobundle#untap()
endif " }}}
" }}}
" vim: set foldmethod=marker et sts=4 sw=4:
