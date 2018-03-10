
function! s:get_rc_script(relative_path)
    return expand(g:rc_dir . '/rc/' . a:relative_path)
endfunction
function! plugins#init()
    let l:dein_path = $CACHE.'/dein_plugins/repos/github.com/Shougo/dein.vim'
    if !filereadable(expand(l:dein_path.'/README.md'))
        " NOTE: git creates also subdirectories if not exists
        silent execute '!git clone https://github.com/Shougo/dein.vim ' . expand(l:dein_path)
    endif
    execute 'set rtp+='.l:dein_path
    if dein#load_state($CACHE.'/dein_plugins')
        call dein#begin($CACHE.'/dein_plugins')
        call dein#add(l:dein_path)

        call dein#add('will133/vim-dirdiff')
        call dein#add('mattn/emmet-vim')
        " ## Completion {{{
        if g:enable_completion
            if g:is_nvim
                call dein#add('Shougo/deoplete.nvim', { 'lazy': 1, 'on_event': 'InsertEnter', 'hook_source': 'source '.s:get_rc_script('plugins/deoplete.vim') })
            elseif has('lua')
                call dein#add('Shougo/neocomplete.vim', { 'lazy': 1, 'on_event': 'InsertEnter', 'hook_source': 'source '.s:get_rc_script('plugins/neocomplete.vim') })
            endif
            call dein#add('Shougo/neco-vim') " Vim source for neocomplete
        endif
        " ## }}}
        " ## Development {{{
        " #### Git {{{
        call dein#add('tpope/vim-fugitive')
        "call dein#add('rhysd/committia.vim')
        call dein#add('jreybert/vimagit')
        call dein#add('airblade/vim-gitgutter')

        call dein#add('gregsexton/gitv', { 'on': 'Gitv' })
        " #### }}}
        " #### Powershell {{{
        call dein#add('PProvost/vim-ps1')
        " #### }}}
        " #### Rust {{{
        call dein#add('rust-lang/rust.vim', { 'lazy': 1, 'on_ft': 'rust'}) " {{{
        let g:rust_fold = 1
        let g:ftplugin_rust_source_path = 'D:/ProgramFiles/Rust_src/src'
        " }}}
        " ### Rust }}}
        " #### SQL {{{
        call dein#add('vim-scripts/sqlserver.vim', { 'lazy': 1, 'on_ft': 'sql' }) " {{{
        let g:sql_type_default = "sqlserver" " }}}
        " #### }}}
        " #### Syntax checking {{{
        call dein#add('scrooloose/syntastic')
        call dein#add('syngan/vim-vimlint', { 'depends': 'ynkdir/vim-vimlparser' })
        " #### }}}
        " #### TOML {{{
        call dein#add('cespare/vim-toml', { 'lazy': 1, 'on_ft': 'toml' })
        " #### }}}
        " ## }}}

        " ## User interface {{{
        call dein#add('w0ng/vim-hybrid')
        call dein#add('chriskempson/base16-vim')
        call dein#add('altercation/vim-colors-solarized')
        call dein#add('itchyny/lightline.vim', { 'hook_source': 'source '. s:get_rc_script('plugins/lightline.vim') }) " {{{
        " lightline.vim }}}
        if has('signs')
            call dein#add('mattesgroeger/vim-bookmarks')
        endif
        " ## }}}
        " ## pandoc {{{
        call dein#add('vim-pandoc/vim-pandoc-syntax')
        call dein#add('vim-pandoc/vim-pandoc')
        " ##}}}
        if has('python3')
            "call dein#add('Shougo/denite.nvim', { 'lazy': 1, 'on_cmd': 'Denite', 'hook_source': 'source '. s:get_rc_script('plugins/denite.vim') })
            if g:is_nvim
                call dein#add('Shougo/deoplete.nvim')
            else
                call dein#add('Shougo/denite.nvim', { 'hook_source': 'source '. s:get_rc_script('plugins/denite.vim') })
            endif
        endif

        call dein#end()
        call dein#save_state()

    endif
endfunction
