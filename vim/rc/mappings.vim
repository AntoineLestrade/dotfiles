nnoremap ^ :
let g:mapleader=','

imap jk <Esc>
omap jk <Esc>
cmap jk <Esc>

imap <C-h> <Left>
imap <C-j> <Down>
imap <C-k> <Up>
imap <C-l> <Right>

" Move around windows {{{
nmap <A-h> <C-w>h
imap <A-h> <C-o><C-w>h
nmap <A-j> <C-w>j
imap <A-j> <C-o><C-w>j
nmap <A-k> <C-w>k
imap <A-k> <C-o><C-w>k
nmap <A-l> <C-w>l
imap <A-l> <C-o><C-w>l
" }}}
" Move windows {{{
nmap <C-A-h> <C-w>H
imap <C-A-h> <C-o><C-w>H
nmap <C-A-j> <C-w>J
imap <C-A-j> <C-o><C-w>J
nmap <C-A-k> <C-w>K
imap <C-A-k> <C-o><C-w>K
nmap <C-A-l> <C-w>L
imap <C-A-l> <C-o><C-w>L
" }}}

" ## Folding {{{
nmap <leader>f0 :set foldlevel=0<CR>
nmap <leader>f1 :set foldlevel=1<CR>
nmap <leader>f2 :set foldlevel=2<CR>
nmap <leader>f3 :set foldlevel=3<CR>
nmap <leader>f4 :set foldlevel=4<CR>
nmap <leader>f5 :set foldlevel=5<CR>
nmap <leader>f6 :set foldlevel=6<CR>
nmap <leader>f7 :set foldlevel=7<CR>
nmap <leader>f8 :set foldlevel=8<CR>
nmap <leader>f9 :set foldlevel=9<CR>
" ## }}}

" ## Visual shifting {{
vnoremap < <gv
vnoremap > >gv
" }}

" Save {{{
noremap <C-s> :update<CR>
vnoremap <C-s> <C-c>:update<CR>
inoremap <C-s> <C-o>:update<CR>
" }}}

"vim: set foldmethod=marker et sts=4 sw=4:
