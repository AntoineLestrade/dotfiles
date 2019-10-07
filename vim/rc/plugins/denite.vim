if executable('rg')
    call denite#custom#var('file_rec', 'command', ['rg', '--files', '--glob', '!.git' ])
    call denite#custom#var('grep', 'command', 'rg')
    call denite#custom#var('grep', 'recursive_opts', [])
    call denite#custom#var('grep', 'final_opts', [])
    call denite#custom#var('grep', 'separator', ['--'])
    call denite#custom#var('grep', 'default_opts', ['--vimgrep', '--no-heading'])
endif


call denite#custom#option('default', 'prompt', '>')
call denite#custom#option('default', 'short_source_name', v:true)
"nnoremap <silent> ;r :<C-u>Denite -buffer-name=register register neoyank<CR>
"xnoremap <silent> ;r :<C-u>Denite -buffer-name=register register neoyank<CR>
nnoremap <silent> <Leader>/ :<C-u>Denite -buffer-name=search line<CR>


autocmd FileType denite call s:denite_settings()
function! s:denite_settings()
  nnoremap <silent><buffer><expr> <CR>
  \ denite#do_map('do_action')
  nnoremap <silent><buffer><expr> d
  \ denite#do_map('do_action', 'delete')
  nnoremap <silent><buffer><expr> p
  \ denite#do_map('do_action', 'preview')
  nnoremap <silent><buffer><expr> q
  \ denite#do_map('quit')
  nnoremap <silent><buffer><expr> i
  \ denite#do_map('open_filter_buffer')
  nnoremap <silent><buffer><expr> <Space>
  \ denite#do_map('toggle_select').'j'
endfunction
