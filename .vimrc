filetype off
filetype plugin indent off

"" 表示設定
set ruler
set title
set cmdheight=2
set laststatus=2
syntax on
set tabstop=4
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
set shiftwidth=4

"" key mapping
nnoremap <C-l> gt
nnoremap <C-h> gT
"" Neobundle
if has('vim_starting')
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#begin(expand('~/.vim/bundle/'))

" Plugins
NeoBundleFetch 'Shougo/neobundle.vim'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/neocomplete.vim'
NeoBundle 'Shougo/neosnippet.vim'
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'Shougo/vimproc.vim'
NeoBundle 'Shougo/neomru.vim'
NeoBundle 'ujihisa/unite-colorscheme'
NeoBundle 'bronson/vim-trailing-whitespace'
NeoBundle 'itchyny/lightline.vim'

" Programing
"" scheme
NeoBundle 'aharisu/vim_goshrepl'
NeoBundle 'aharisu/vim-gdev'
"" haskell
NeoBundle 'kana/vim-filetype-haskell'
NeoBundle 'ujihisa/neco-ghc'
NeoBundle 'dag/vim2hs'
"" scala
NeoBundle 'derekwyatt/vim-scala'

" Color Schemes
NeoBundle 'w0ng/vim-hybrid'
call neobundle#end()

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
set t_Co=256
autocmd ColorScheme * highlight Normal ctermbg=none
autocmd ColorScheme * highlight LineNr ctermfg=243 guifg=#aaaaaa
autocmd ColorScheme * highlight Visual ctermfg=245
colorscheme hybrid

if has('unix') && !has('gui_running')
  " ESC後にすぐ反映されない対策
  inoremap <silent> <ESC> <ESC>
endif

filetype plugin indent on
