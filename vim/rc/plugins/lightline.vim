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
