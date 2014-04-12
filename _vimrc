
           

"" NeoBundle {{{
set runtimepath+=~/vimfiles/bundle/neobundle.vim/
call neobundle#rc(expand('~/vimfiles/bundle'))

NeoBundleFetch 'Shougo/noebundle.vim'

NeoBundle 'Lokaltog/powerline'
NeoBundle 'mattn/emmet-vim'
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'dterei/VimBookmarking'
NeoBundle 'tpope/vim-dispatch'
NeoBundle 'majutsushi/tagbar'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'PProvost/vim-ps1'
NeoBundle 'vim-scripts/Conque-Shell'
NeoBundle 'genoma/vim-less'
NeoBundle 'vim-scripts/DirDiff.vim'

"}}}

colorscheme slate
"set guifont=Source\ Code\ Pro\ for\ Powerline\ 11
set guifont=DejaVu_Sans_Mono_for_Powerline
"" global options {{{
noremap <A-n> :bnext<CR>
noremap <A-p> :bprevious<CR>


source $VIMRUNTIME/mswin.vim
behave mswin

set guioptions-=m
set guioptions-=T
set noswapfile

map Q gq

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif


if has("autocmd")
  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!
endif


"" from sensible.vim {{{

" sensible.vim - Defaults everyone can agree on
" Maintainer:   Tim Pope <http://tpo.pe/>
" Version:      1.0

if exists('g:loaded_sensible') || &compatible
  finish
else
  let g:loaded_sensible = 1
endif

if has('autocmd')
  filetype plugin indent on
endif
if has('syntax') && !exists('g:syntax_on')
  syntax enable
endif

" Use :help 'option' to see the documentation for the given option.

set autoindent
set backspace=indent,eol,start
set complete-=i
set smarttab

set nrformats-=octal
set shiftround

set ttimeout
set ttimeoutlen=100

set incsearch
" Use <C-L> to clear the highlighting of :set hlsearch.
if maparg('<C-L>', 'n') ==# ''
  nnoremap <silent> <C-L> :nohlsearch<CR><C-L>
endif

set laststatus=2
set ruler
set showcmd
set wildmenu

if !&scrolloff
  set scrolloff=1
endif
if !&sidescrolloff
  set sidescrolloff=5
endif
set display+=lastline

if &encoding ==# 'latin1' && has('gui_running')
  set encoding=utf-8
endif

if &listchars ==# 'eol:$'
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
  if !has('win32') && (&termencoding ==# 'utf-8' || &encoding ==# 'utf-8')
    let &listchars = "tab:\u21e5 ,trail:\u2423,extends:\u21c9,precedes:\u21c7,nbsp:\u00b7"
  endif
endif

if &shell =~# 'fish$'
  set shell=/bin/bash
endif

set autoread
"set fileformats+=mac

if &history < 1000
  set history=1000
endif
if &tabpagemax < 50
  set tabpagemax=50
endif
if !empty(&viminfo)
  set viminfo^=!
endif

" Allow color schemes to do bright colors without forcing bold.
if &t_Co == 8 && $TERM !~# '^linux'
  set t_Co=16
endif

" Load matchit.vim, but only if the user hasn't installed a newer version.
if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
  runtime! macros/matchit.vim
endif

inoremap <C-U> <C-G>u<C-U>

""}}}
""}}}



"" NERDTree {{{
"close vim if nerdtree is the unique opened buffer
autocmd bufenter * if (winnr("$") == 1 && exists ("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

let g:NERDTreeWinPos="right"

map <F9> :NERDTreeToggle<CR>
""}}}


"" Taglist {{{
let Tlist_Ctags_Cmd="C:/NoInstall_Programs/ctags58/ctags.exe"
map <F8> :TlistToggle<CR>
""}}}

"" Tagbar {{{
let g:tagbar_ctags_bin="C:/NoInstall_Programs/ctags58/ctags.exe"
nmap <F7> :TagbarToggle<CR>
""}}}

"" PoshComplete {{{
autocmd FileType ps1 :setl omnifunc=poshcomplete#CompleteCommand
""}}}


if 0
"" OmniSharp {{{
"  " OmniSharp won't work without this setting
"  filetype plugin on
"  
"  "This is the default value, setting it isn't actually necessary
"  let g:OmniSharp_host = "http://localhost:2000"
"  
"  "Showmatch significantly slows down omnicomplete
"  "when the first match contains parentheses.
"  set noshowmatch
"  "Set autocomplete function to OmniSharp (if not using YouCompleteMe completion plugin)
"  autocmd FileType cs setlocal omnifunc=OmniSharp#Complete
"  
"  "Super tab settings
"  "let g:SuperTabDefaultCompletionType = 'context'
"  "let g:SuperTabContextDefaultCompletionType = "<c-x><c-o>"
"  "let g:SuperTabDefaultCompletionTypeDiscovery = ["&omnifunc:<c-x><c-o>","&completefunc:<c-x><c-n>"]
"  "let g:SuperTabClosePreviewOnPopupClose = 1
"  
"  "don't autoselect first item in omnicomplete, show if only one item (for preview)
"  "remove preview if you don't want to see any documentation whatsoever.
"  set completeopt=longest,menuone,preview
"  " Fetch full documentation during omnicomplete requests. 
"  " There is a performance penalty with this (especially on Mono)
"  " By default, only Type/Method signatures are fetched. Full documentation can still be fetched when
"  " you need it with the :OmniSharpDocumentation command.
"  " let g:omnicomplete_fetch_documentation=1
"  
"  "Move the preview window (code documentation) to the bottom of the screen, so it doesn't move the code!
"  "You might also want to look at the echodoc plugin
"  set splitbelow
"  
"  nnoremap <F5> :wa!<cr>:OmniSharpBuild<cr>
"  " Builds can run asynchronously with vim-dispatch installed
"  "nnoremap <F5> :wa!<cr>:OmniSharpBuildAsync<cr>
"  
"  "The following commands are contextual, based on the current cursor position.
"  
"  nnoremap <F12> :OmniSharpGotoDefinition<cr>
"  nnoremap gd :OmniSharpGotoDefinition<cr>
"  nnoremap <leader>fi :OmniSharpFindImplementations<cr>
"  nnoremap <leader>ft :OmniSharpFindType<cr>
"  nnoremap <leader>fs :OmniSharpFindSymbol<cr>
"  nnoremap <leader>fu :OmniSharpFindUsages<cr>
"  nnoremap <leader>fm :OmniSharpFindMembersInBuffer<cr>
"  nnoremap <leader>tt :OmniSharpTypeLookup<cr>
"  nnoremap <leader>dc :OmniSharpDocumentation<cr>
"  "show type information automatically when the cursor stops moving
"  autocmd CursorHold *.cs call OmniSharp#TypeLookupWithoutDocumentation()
"  set updatetime=500
"  set cmdheight=2
"  "I find contextual code actions so useful that I have it mapped to the spacebar
"  nnoremap <space> :OmniSharpGetCodeActions<cr>
"  
"  " rename with dialog
"  nnoremap <leader>nm :OmniSharpRename<cr>
"  nnoremap <F2> :OmniSharpRename<cr>      
"  " rename without dialog - with cursor on the symbol to rename... ':Rename newname'
"  command! -nargs=1 Rename :call OmniSharp#RenameTo("<args>")
"  
"  " Force OmniSharp to reload the solution. Useful when switching branches etc.
"  nnoremap <leader>rl :OmniSharpReloadSolution<cr>
"  nnoremap <leader>cf :OmniSharpCodeFormat<cr>
"  " Load the current .cs file to the nearest project
"  nnoremap <leader>tp :OmniSharpAddToProject<cr>
"  " Automatically add new cs files to the nearest project on save
"  autocmd BufWritePost *.cs call OmniSharp#AddToProject()
"  " (Experimental - uses vim-dispatch or vimproc plugin) - Start the omnisharp server for the current solution
"  nnoremap <leader>ss :OmniSharpStartServer<cr>
"  nnoremap <leader>sp :OmniSharpStopServer<cr>
"  
"  " Add syntax highlighting for types and interfaces
"  nnoremap <leader>th :OmniSharpHighlightTypes<cr>
"  "Don't ask to save when changing buffers (i.e. when jumping to a type definition)
"  set hidden
"  ""}}}OmniSharp
endif

"" VimOrganizer {{{

" This is an example vimrc that should work for testing purposes.
" Integrate the VimOrganizer specific sections into your own
" vimrc if you wish to use VimOrganizer on a regular basis. . .

"===================================================================
" THE NECESSARY STUFF
" The three lines below are necessary for VimOrganizer to work right
" ==================================================================
let g:ft_ignore_pat = '\.org'
filetype plugin indent on
" and then put these lines in vimrc somewhere after the line above
au! BufRead,BufWrite,BufWritePost,BufNewFile *.org 
au BufEnter *.org            call org#SetOrgFileType()
" let g:org_capture_file = '~/org_files/mycaptures.org'
command! OrgCapture :call org#CaptureBuffer()
command! OrgCaptureFile :call org#OpenCaptureFile()
syntax on

"==============================================================
" THE UNNECESSARY STUFF
"==============================================================
"  Everything below here is a customization.  None are needed.
"==============================================================

" The variables below are used to define the default Todo list and
" default Tag list.  Both of these can also be defined 
" on a document-specific basis by config lines in a file.
" See :h vimorg-todo-metadata and/or :h vimorg-tag-metadata
" 'TODO | DONE' is the default, so not really necessary to define it at all
let g:org_todo_setup='TODO | DONE'
" OR, e.g.,:
"let g:org_todo_setup='TODO NEXT STARTED | DONE CANCELED'

" include a tags setup string if you want:
let g:org_tags_alist='{@home(h) @work(w) @tennisclub(t)} {easy(e) hard(d)} {computer(c) phone(p)}'
"
" g:org_agenda_dirs specify directories that, along with 
" their subtrees, are searched for list of .org files when
" accessing EditAgendaFiles().  Specify your own here, otherwise
" default will be for g:org_agenda_dirs to hold single
" directory which is directory of the first .org file opened
" in current Vim instance:
" Below is line I use in my Windows install:
" NOTE:  case sensitive even on windows.
let g:org_agenda_select_dirs=["~/desktop/org_files"]
let g:org_agenda_files = split(glob("~/desktop/org_files/org-mod*.org"),"\n")

" ----------------------
" Emacs setup
" ----------------------
" To use Emacs you will need to define the client.  On
" Linux/OSX this is typically simple, just:
"let g:org_command_for_emacsclient = 'emacsclient'
"
" On Windows it is more complicated, and probably involves creating
" a 'soft link' to the emacsclient executable (which is 'emacsclientw')
" See :h vimorg-emacs-setup
"let g:org_command_for_emacsclient = 'c:\users\herbert\emacsclientw.exe'

" ----------------------
" Custom Agenda Searches
" ----------------------
" The assignment to g:org_custom_searches below defines searches that a
" a user can then easily access from the Org menu or the Agenda Dashboard.
" (Still need to add help on how to define them, assignment below
" is hopefully illustrative for now. . . . )
let g:org_custom_searches = [
            \  { 'name':"Next week's agenda", 'type':'agenda',
            \    'agenda_date':'+1w', 'agenda_duration':'w' }
            \, { 'name':"Next week's TODOS", 'type':'agenda',
            \    'agenda_date':'+1w', 'agenda_duration':'w',
            \    'spec':'+UNFINISHED_TODOS' }
            \, { 'name':'Home tags', 'type':'heading_list', 'spec':'+HOME' }
            \, { 'name':'Home tags', 'type':'sparse_tree', 'spec':'+HOME' }
            \  ]

" --------------------------------
" Custom colors
" --------------------------------"
" OrgCustomColors() allows a user to set highlighting for particular items
function! OrgCustomColors()
    " Various text item 'highlightings' below
    " are the defaults.  Uncomment and change a line if you
    " want different highlighting for the element.
    "
    " Below are defaults for any TODOS you define.  TODOS that
    " come before the | in a definition will use  'NOTDONETODO'
    " and those that come after are DONETODO
    "hi! DONETODO guifg=green ctermfg=green
    "hi! NOTDONETODO guifg=red ctermfg=lightred

    " Heading level highlighting is done in pairs, one for the
    " heading when unfolded and one for folded.  Default is to make
    " them the same except for the folded version being bold:
    " assign OL1 pair for level 1, OL2 pair for level 2, etc.
    "hi! OL1 guifg=somecolor guibg=somecolor 
    "hi! OL1Folded guifg=somecolor guibg=somecolor gui=bold


    " Tags are lines below headings that have :colon:separated:tags:
    "hi! Org_Tag guifg=lightgreen ctermfg=blue

    " Lines that begin with '#+' in column 0 are config lines
    "hi! Org_Config_Line guifg=darkgray ctermfg=magenta

    " Drawers are :PROPERTIES: and :LOGBOOK: lines and their associated
    " :END: lines
    "hi! Org_Drawer guifg=pink ctermfg=magenta
    "hi! Org_Drawer_Folded guifg=pink ctermfg=magenta gui=bold cterm=bold

    " This applies to value names in :PROPERTIES: blocks
    "hi! Org_Property_Value guifg=pink ctermfg=magenta

    " Three lines below apply to different kinds of blocks
    "hi! Org_Block guifg=#555555 ctermfg=magenta
    "hi! Org_Src_Block guifg=#555555 ctermfg=magenta
    "hi! Org_Table guifg=#888888 guibg=#333333 ctermfg=magenta

    " Dates are date specs between angle brackets (<>) or square brackets ([])
    "hi! Org_Date guifg=magenta ctermfg=magenta gui=underline cterm=underline

    " Org_Star is used to "hide" initial asterisks in a heading
    "hi! Org_Star guifg=#444444 ctermfg=darkgray

    "hi! Props guifg=#ffa0a0 ctermfg=gray

    " Bold, italics, underline, and code are highlights applied
    " to character formatting
    "hi! Org_Code guifg=darkgray gui=bold ctermfg=14
    "hi! Org_Itals gui=italic guifg=#aaaaaa ctermfg=lightgray
    "hi! Org_Bold gui=bold guifg=#aaaaaa ctermfg=lightgray
    "hi! Org_Underline gui=underline guifg=#aaaaaa ctermfg=lightgray
    "hi! Org_Lnumber guifg=#999999 ctermfg=gray

    " These lines apply to links: [[link]], and [[link][link desc]]
    "if has("conceal")
    "    hi! default linkends guifg=blue ctermfg=blue
    "endif
    "hi! Org_Full_Link guifg=cyan gui=underline ctermfg=lightblue cterm=underline
    "hi! Org_Half_Link guifg=cyan gui=underline ctermfg=lightblue cterm=underline

    " Applies to the Heading line that can be displayed in column view
    "highlight OrgColumnHeadings guibg=#444444 guifg=#aaaaaa gui=underline

    " Use g:org_todo_custom_highlights to set up highlighting for individual
    " TODO items.  Without this all todos that designate an unfinished state
    " will be highlighted using NOTDONETODO highlight (see above) 
    " and all todos that designate a finished state will be highlighted using
    " the DONETODO highlight (see above).
    let g:org_todo_custom_highlights = 
               \     { 'NEXT': { 'guifg':'#888888', 'guibg':'#222222',
               \              'ctermfg':'gray', 'ctermbg':'darkgray'},
               \      'WAITING': { 'guifg':'#aa3388', 
               \                 'ctermfg':'red' } }

endfunction

" Below are two examples of Org-mode "hook" functions
" These present opportunities for end-user customization
" of how VimOrganizer works.  For more info see the 
" documentation for hooks in Emacs' Org-mode documentation:
" http://orgmode.org/worg/org-configs/org-hooks.php#sec-1_40
"
" These two hooks are currently the only ones enabled in 
" the VimOrganizer codebase, but they are easy to add so if
" there's a particular hook you want go ahead and request it
" or look for where these hooks are implemented in 
" /ftplugin/org.vim and use them as example for placing your
" own hooks in VimOrganizer:
function! Org_property_changed_functions(line,key, val)
        "call confirm("prop changed: ".a:line."--key:".a:key." val:".a:val)
endfunction
function! Org_after_todo_state_change_hook(line,state1, state2)
        "call confirm("changed: ".a:line."--key:".a:state1." val:".a:state2)
        "call OrgConfirmDrawer("LOGBOOK")
        "let str = ": - State: " . org#Pad(a:state2,10) . "   from: " . Pad(a:state1,10) .
        "            \ '    [' . org#Timestamp() . ']'
        "call append(line("."), repeat(' ',len(matchstr(getline(line(".")),'^\s*'))) . str)
endfunction


""}}}


"" Powerline {{{
set rtp+=~/vimfiles/bundle/powerline/powerline/bindings/vim

"" }}}
" vim:set ft=vim et sw=2 fdm=marker:
"


