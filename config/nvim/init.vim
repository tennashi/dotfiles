" plugins
call plug#begin('~/.local/share/nvim/plugged')
Plug 'junegunn/vim-plug'

"" Colorscheme
Plug 'arcticicestudio/nord-vim', {'do': 'cp colors/* ~/.config/nvim/colors/'}

"" Go
"Plug 'fatih/vim-go', {'do': ':GoInstallBinaries' }

"" Haskell
Plug 'dag/vim2hs'

"" Filer
Plug 'Shougo/defx.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'kristijanhusak/defx-git'
Plug 'kristijanhusak/defx-icons'

"" Markdown
Plug 'previm/previm'

"" Git
Plug 'lambdalisue/gina.vim'
"Plug 'airblade/vim-gitgutter'
Plug 'itchyny/vim-gitbranch'

"" TypeScript
"Plug 'ryanolsonx/vim-lsp-typescript'
"Plug 'leafgarland/typescript-vim'

"" Auto complete
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'lighttiger2505/deoplete-vim-lsp'
"Plug 'prabirshrestha/asyncomplete.vim'
"Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'

"Plug 'cohama/lexima.vim'
"Plug 'w0rp/ale'
Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins' }
"Plug 'Shougo/deol.nvim'
Plug 'luochen1990/rainbow'
Plug 'itchyny/lightline.vim'
Plug 'ryanoasis/vim-devicons'
"Plug 'liuchengxu/vista.vim'
call plug#end()

set autoindent
set autoread
set autowriteall
set clipboard+=unnamedplus
set expandtab
set gdefault
set hidden
set listchars="tab:>-,trail:-,eol:$"
set mouse=a
set number
set relativenumber
set shiftwidth=2
set showtabline=2
set smartindent
set smarttab
set tabstop=2
set termguicolors
set updatetime=100
let mapleader = "\<Space>"

noremap <C-n> :cnext<CR>
noremap <C-m> :cprevious<CR>
nnoremap <leader>a :cclose<CR>

" deoplete
let g:deoplete#enable_at_startup = 1

" vim-lsp
let g:lsp_diagnostics_enabled = 1
let g:lsp_auto_enable = 1
let g:lsp_preview_keep_focus = 1
let g:lsp_insert_text_enabled = 1
let g:lsp_text_edit_enabled = 1
let g:lsp_signs_enabled = 1
let g:lsp_virtual_text_enabled = 1
let g:lsp_use_event_queue = 1
let g:lsp_highlight_references_enabled = 1
let g:lsp_diagnostics_echo_cursor = 1
let g:lsp_signs_error = {'text': 'x'}
let g:lsp_signs_warning = {'text': '!'}
let g:lsp_virtual_text_enabled = 1
let g:lsp_log_verbose = 1
let g:lsp_log_file = expand('~/vim-lsp.log')

" ale
let g:ale_fix_on_save = 1
let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'typescript': ['tslint'],
\}
let g:ale_linters_explicit = 1
let g:ale_linters = {
\   'typescript': ['tslint'],
\}

" Colorscheme
colorscheme nord
let g:nord_cursor_line_number_background = 1

" rainbow
let g:rainbow_active = 1

" previm
let g:previm_open_cmd = "firefox"

" vim-devicons
let g:webdevicons_enable_denite=1
function! DeviconFiletype()
  return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype . ' ' . WebDevIconsGetFileTypeSymbol() : 'no ft') : ''
endfunction
function! DeviconFileformat()
  return winwidth(0) > 70 ? (&fileformat . ' ' . WebDevIconsGetFileFormatSymbol()) : ''
endfunction

" TypeScript
if executable('typescript-language-server')
  augroup LspTS
    au!
    au User lsp_setup call lsp#register_server({
        \ 'name': 'typescript-language-server',
        \ 'cmd': {server_info->['typescript-language-server', '--stdio']},
        \ 'root_uri':{server_info->lsp#utils#path_to_uri(lsp#utils#find_nearest_parent_file_directory(lsp#utils#get_buffer_path(), 'tsconfig.json'))},
        \ 'whitelist': ['typescript', 'typescript.tsx'],
        \ })
  augroup END
endif

" Go
function! s:build_go_files()
  let l:file = expand('%')
  if l:file =~# '^\f\+_test\.go$'
    call go#test#Test(0, 1)
  elseif l:file =~# '^\f\+\.go$'
    call go#cmd#Build(0)
  endif
endfunction
let g:go_fmt_command = "goimports"
let g:go_decls_includes = "func,type"
let g:go_auto_type_info = 1
let g:go_auto_sameids = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_operators = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_build_constraints = 1
let g:go_def_mapping_enabled = 0
let g:go_doc_keywordprg_enabled = 0

augroup Go
  au!
  au FileType go noremap <buffer><leader>b :<C-u>call <SID>build_go_files()<CR>
  au FileType go nmap <buffer><leader>r <Plug>(go-run)
  au FileType go nmap <buffer><leader>t <Plug>(go-test)
  au FileType go nmap <buffer><leader>c <Plug>(go-coverage-toggle)
  au FileType go nmap <buffer><leader>i <Plug>(go-info)
  au FileType go nmap <buffer><leader>s <Plug>(go-def-split)
  au FileType go nmap <buffer><leader>v <Plug>(go-def-vertical)
  au FileType go nmap <buffer><leader>d <Plug>(go-def)
  au Filetype go command! -bang -buffer A call go#alternate#Switch(<bang>0, 'edit')
  au Filetype go command! -bang -buffer AV call go#alternate#Switch(<bang>0, 'vsplit')
  au Filetype go command! -bang -buffer AS call go#alternate#Switch(<bang>0, 'split')
  au Filetype go command! -bang -buffer AT call go#alternate#Switch(<bang>0, 'tabe')
  au Filetype go setlocal noexpandtab tabstop=4 shiftwidth=4
augroup END

if executable('gopls')
  augroup LspGo
    au!
    au User lsp_setup call lsp#register_server({
        \ 'name': 'gopls',
        \ 'cmd': {server_info->['gopls', '-mode', 'stdio']},
        \ 'whitelist': ['go'],
        \ })
  augroup END
endif

" EFM langserver
if executable('efm-langserver')
  augroup LspEFM
    au!
    au User lsp_setup call lsp#register_server({
        \ 'name': 'efm-langserver',
        \ 'cmd': {server_info->['efm-langserver', '-c=/home/yota/.config/efm-langserver/config.yaml']},
        \ 'whitelist': ['vim'],
        \ })
  augroup END
endif


" Haskell
let g:haskell_conceal = 0

" terminal
tnoremap <silent> <C-t> <C-\><C-n>
augroup Term
  au!
  au VimEnter * if @% == '' && s:get_buf_byte() == 0 | call Term()
  au TermOpen * setlocal nonumber norelativenumber
  au TermOpen * normal i
  function! s:get_buf_byte()
    let byte = line2byte(line('$') + 1)
    if byte == -1
      return 0
    else
      return byte - 1
    endif
  endfunction
augroup END

function! Term()
  setlocal nonumber
  setlocal norelativenumber
  call termopen(&shell, {'on_exit': 'OnExit'})
  normal i
endfunction

function! CloseLastTerm()
  if len(filter(range(1, bufnr('$')), 'buflisted(v:val)')) == 1
    :q
  endif
endfunction

function! OnExit(job_id, code, event)
  if a:code == 0
    call CloseLastTerm()
  endif
endfunction

" buffer
augroup Buffer
	au!
  au BufLeave * call s:buffer_name()

  function! s:buffer_name()
    if exists('b:term_title') && exists('b:terminal_job_pid')
      execute ":file term" . b:terminal_job_pid . "/" . b:term_title
    endif
  endfunction
augroup END

function! CloseBuf()
  if len(filter(range(1, bufnr('$')), 'buflisted(v:val)')) == 1
    :q
  else
    :bp|bd #
  endif
endfunction

cnoreabbrev q call CloseBuf()


" lightline
let g:lightline = {
      \   'colorscheme': 'nord',
      \   'active' : {
      \     'left' : [ [ 'mode', 'paste' ], [ 'gitbranch', 'filename', 'modified', 'readonly' ] ]
      \   },
      \   'component_function': {
      \     'filetype': 'DeviconFiletype',
      \     'fileformat': 'DeviconFileformat',
      \     'gitbranch': 'gitbranch#name'
      \   }
      \ }
let g:lightline.tab = {
      \ 'active': [ 'tabnum', 'filename', 'modified' ],
      \ 'inactive': [ 'tabnum', 'filename', 'modified' ]
      \ }

" denite
call denite#custom#var('file/rec', 'command',
      \ ['ag', '--follow', '--nocolor', '--nogroup', '-g', ''])
call denite#custom#map(
      \ 'insert',
      \ '<C-n>',
      \ '<denite:move_to_next_line>',
      \ 'noremap'
      \)
call denite#custom#map(
      \ 'insert',
      \ '<C-p>',
      \ '<denite:move_to_previous_line>',
      \ 'noremap'
      \)

nnoremap <silent> <C-p>f :<C-u>Denite file<CR>
nnoremap <silent> <C-p>j :<C-u>Denite buffer file/rec<CR>
nnoremap <silent> <C-p>b :<C-u>Denite buffer<CR>
nnoremap <silent> <C-p>r :<C-u>Denite register<CR>

" deol
nnoremap <silent> <C-Space> :<C-u>Deol -split=holizontal<CR><Paste>

      \
" defx
nnoremap <silent> <C-l> :<C-u>Defx -auto-cd -split=vertical -winwidth=40 -direction=topleft -listed -columns=git:icons:filename:type<CR>
augroup Defx
  au!
  autocmd FileType defx call s:defx_my_settings()
  function! s:defx_my_settings() abort
  " Define mappings
    nnoremap <silent><buffer><expr> <CR>
      \ defx#is_directory() ? defx#do_action('open_or_close_tree') : defx#do_action('multi', ['drop', 'quit'])
    nnoremap <silent><buffer><expr> c
      \ defx#do_action('copy')
    nnoremap <silent><buffer><expr> m
      \ defx#do_action('move')
    nnoremap <silent><buffer><expr> p
      \ defx#do_action('paste')
    nnoremap <silent><buffer><expr> l
      \ defx#do_action('drop')
    nnoremap <silent><buffer><expr> E
      \ defx#do_action('drop', 'split')
    nnoremap <silent><buffer><expr> P
      \ defx#do_action('drop', 'pedit')
    nnoremap <silent><buffer><expr>
      \ defx#do_action('new_file')
    nnoremap <silent><buffer><expr> Nd
      \ defx#do_action('new_directory')
    nnoremap <silent><buffer><expr> d
      \ defx#do_action('rename')
    nnoremap <silent><buffer><expr> x
      \ defx#do_action('execute_system')
    nnoremap <silent><buffer><expr> yy
      \ defx#do_action('yank_path')
    nnoremap <silent><buffer><expr> .
      \ defx#do_action('toggle_ignored_files')
    nnoremap <silent><buffer><expr> h
      \ defx#do_action('cd', ['..'])
    nnoremap <silent><buffer><expr> ~
      \ defx#do_action('cd')
    nnoremap <silent><buffer><expr> q
      \ defx#do_action('quit')
    nnoremap <silent><buffer><expr> <Space>
      \ defx#do_action('toggle_select') . 'j'
    nnoremap <silent><buffer><expr> *
      \ defx#do_action('toggle_select_all')
    nnoremap <silent><buffer><expr> j
      \ line('.') == line('$') ? 'gg' : 'j'
    nnoremap <silent><buffer><expr> k
      \ line('.') == 1 ? 'G' : 'k'
    nnoremap <silent><buffer><expr> <C-l>
      \ defx#do_action('redraw')
    nnoremap <silent><buffer><expr> <C-g>
      \ defx#do_action('print')
    nnoremap <silent><buffer><expr> cd
      \ defx#do_action('change_vim_cwd')
  endfunction
augroup END

" gina
nnoremap <silent> <leader>gs :<C-u>Gina status<CR>
nnoremap <silent> <leader>gc :<C-u>Gina commit<CR>
nnoremap <silent> <leader>gb :<C-u>Gina branch<CR>
nnoremap <silent> <leader>gB :<C-u>Gina browse<CR>
nnoremap <silent> <leader>g<C-b> :<C-u>Gina brame<CR>
nnoremap <silent> <leader>gd :<C-u>Gina diff<CR>
nnoremap <silent> <leader>gl :<C-u>Gina log<CR>
nnoremap <silent> <leader>gp :<C-u>Gina pull<CR>
nnoremap <silent> <leader>gP :<C-u>Gina push<CR>
