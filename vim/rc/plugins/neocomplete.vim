
    let g:neocomplete#enable_at_startup = 1
 "  let g:neocomplete#disable_auto_complete = 0
 "  let g:neocomplete#enable_insert_char_pre = 0

 "  let g:neocomplete#enable_smart_case = 1
 "  let g:neocomplete#enable_camel_case = 1

 "  let g:neocomplete#enable_fuzzy_completion = 1

 "  let g:neocomplete#sources#syntax#min_keyword_length = 3
 "  let g:neocomplete#auto_completion_start_length = 2
 "  let g:neocomplete#manual_completion_start_length = 0
 "  let g:neocomplete#min_keyword_length = 3

 "  let g:neocomplete#enable_complete_select = 1
 "  try
 "      let completeopt_save = &completeopt
 "      set completeopt+=noinsert,noselect
 "  catch
 "      let g:neocomplete#enable_complete_select = 0
 "  finally
 "      let &completeopt = completeopt_save
 "  endtry
 "  let g:neocomplete#enable_auto_select = 1
 "  let g:neocomplete#enable_refresh_always = 1
 "  let g:neocomplete#enable_cursor_hold_i = 0

 "  let g:neocomplete#enable_auto_delimiter = 1

 "  let g:neocomplete#max_list = 100
 "  let g:neocomplete#force_overwrite_completefunc = 1
 "  if !exists('g:neocomplete#sources#omni#input_patterns')
 "      let g:neocomplete#sources#omni#input_patterns = {}
 "  endif

 "  if !exists('g:neocomplete#sources')
 "      let g:neocomplete#sources = {}
 "  endif
   "let g:neocomplete#sources.rust = ['buffer', 'omni']
  " if patter matches, local omnifunc will be called
    if !exists('g:neocomplete#sources#omni#input_patterns')
        let g:neocomplete#sources#omni#input_patterns = {}
    endif
    let g:neocomplete#sources#omni#input_patterns.rust = 
        \ '[^.[:digit:] *\t]\%(\.\|\::\)\%(\h\w*\)\?'
