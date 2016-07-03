filetype off
filetype plugin indent off

"" 表示設定
set ruler
set title
set cmdheight=2
set laststatus=2
let g:tex_conceal=''
syntax on
set tabstop=2
set smartindent
set autoindent
set expandtab
set number
set showmatch
set smarttab
set cursorline
set scrolloff=5
set list
set listchars=tab:>-,extends:<
if exists('&ambiwidth')
    set ambiwidth=double
endif

"" 検索設定
set ignorecase
set smartcase
set wrapscan
set incsearch

"" 基本設定
set backupdir=$HOME/vimbackup
set directory=$HOME/vimbackup
set autoread
set backspace=indent,eol,start
set showcmd
set notitle
set clipboard=unnamedplus

"" 編集設定
set expandtab
set shiftwidth=2

"" key mapping
nnoremap <C-l> gt
nnoremap <C-h> gT

"" dein
let s:dein_dir = expand('~/.cache/dein')
let s:dein_repo_dir = s:dein_dir . '/repos/github.com/Shougo/dein.vim'

if &runtimepath !~# '/dein.vim'
    if !isdirectory(s:dein_repo_dir)
        execute '!git clone https://github.com/Shougo/dein.vim' s:dein_repo_dir
    endif
    execute 'set runtimepath^=' . fnamemodify(s:dein_repo_dir, ':p')
endif

if dein#load_state(s:dein_dir)
    call dein#begin(s:dein_dir)

    let g:rc_dir = expand('~/.vim/rc')
    let s:toml = g:rc_dir . '/dein.toml'
    let s:lazy_toml = g:rc_dir . '/dein_lazy.toml'

    call dein#load_toml(s:toml, {'lazy': 0})
    call dein#load_toml(s:lazy_toml, {'lazy': 1})

    call dein#end()
    call dein#save_state()
endif

if dein#check_install()
    call dein#install()
endif

"" neocomplete
let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_smart_case = 1
let g:neocomplete#sources#syntax#min_keyword_length = 3
let g:neocomplete#sources#dictionary#dictionaries = {
  \ 'default' : '',
  \ 'vimshell' : $HOME.'/.vimshell_hist',
  \ 'scheme' : $HOME.'/.gosh_completions'
    \}
if !exists('g:neocomplete#keyword_patterns')
    let g:neocomplete#keyword_patterns = {}
endif
let g:neocomplete#keyword_patterns['default'] = '\h\w*'
let g:neocomplete#keyword_patterns['gosh-repl'] = "[[:alpha:]+*/@$_=.!?-][[:alnum:]+*/@$_:=.!?-]*"

" <TAB>: completion.
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<S-TAB>"
" Plugin key-mappings.
imap <C-k> <Plug>(neosnippet_expand_or_jump)
smap <C-k> <Plug>(neosnippet_expand_or_jump)
"SuperTab like snippets behavior.
imap <expr><TAB> pumvisible() ? "\<C-n>" : neosnippet#jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
smap <expr><TAB> neosnippet#jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
"For snippet_complete marker.
if has('conceal')
    set conceallevel=2 concealcursor=i
endif


" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
" Enable heavy omni completion.
if !exists('g:neocomplete#sources#omni#input_patterns')
  let g:neocomplete#sources#omni#input_patterns = {}
endif

"" ColorScheme
"set t_Co=256
set background=dark
let g:solarized_termcolors = 256
"autocmd ColorScheme * highlight Normal ctermbg=none
"autocmd ColorScheme * highlight LineNr ctermfg=243 guifg=#aaaaaa
"autocmd ColorScheme * highlight Visual ctermfg=245
"colorscheme hybrid
colorscheme solarized
"colorscheme darkblue

" lightline の色設定
let g:lightline = {
            \ 'colorscheme': 'solarized_dark'
            \ }

if has('unix') && !has('gui_running')
  " ESC後にすぐ反映されない対策
  inoremap <silent> <ESC> <ESC>
endif

filetype plugin indent on
