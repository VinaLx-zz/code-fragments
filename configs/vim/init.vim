""" ------- VUNDLE SETUP BEGIN -------

set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

" General
Plugin 'scrooloose/nerdtree'
Plugin 'easymotion/vim-easymotion'
Plugin 'tpope/vim-surround'
Plugin 'jiangmiao/auto-pairs'
Plugin 'scrooloose/nerdcommenter'
Plugin 'bling/vim-airline'
Plugin 'tpope/vim-repeat'
Plugin 'ap/vim-css-color'

" Integrations

"   linter
Plugin 'scrooloose/syntastic'
" Plugin 'w0rp/ale'

"   git
Plugin 'tpope/vim-fugitive'
Plugin 'airblade/vim-gitgutter'

" Completions
Plugin 'Shougo/deoplete.nvim'
Plugin 'SirVer/ultisnips'
Plugin 'Shougo/neco-vim'
Plugin 'Shougo/neoinclude.vim'
Plugin 'zchee/deoplete-clang'
Plugin 'zchee/deoplete-jedi'
Plugin 'eagletmt/neco-ghc'

" Languages Support

"   c++
Plugin 'rhysd/vim-clang-format'
Plugin 'octol/vim-cpp-enhanced-highlight'
"   ispc
Plugin 'jez/vim-ispc'
"   agda
Plugin 'derekelkins/agda-vim'
"   markdown
Plugin 'tpope/vim-markdown'
"   json
Plugin 'elzr/vim-json'
"   haskell
Plugin 'neovimhaskell/haskell-vim'
Plugin 'eagletmt/ghcmod-vim'
Plugin 'Shougo/vimproc'

" colorscheme
Plugin 'flazz/vim-colorschemes'
Plugin 'kien/rainbow_parentheses.vim'

call vundle#end()            " required
filetype plugin indent on    " required

""" ------- VUNDLE SETUP END -------

""" ------- GENERAL CONFIGURE BEGIN -------

syntax enable
syntax on
filetype on

colorscheme atom

set updatetime=300

""" Line Numbers
set number numberwidth=5

""" Scrolling
set scrolloff=5

""" Cursor
set ruler
set cursorline cursorcolumn
set colorcolumn=81
set guicursor+=a:blinkon0

" about tabs and indents
set tabstop=4 shiftwidth=4 shiftround softtabstop=4 expandtab cindent
set backspace=2
filetype indent on

""" Search hightlight
set hlsearch incsearch
nnoremap <silent> <leader>nh :nohlsearch<CR>

""" Folding
set foldmethod=syntax
set nofoldenable

""" Leaders
let mapleader = "-"
let maplocalleader = "\\"

""" Reloading init.vim
nnoremap <leader>rl :source $MYVIMRC<CR>
nnoremap <leader>rc :vspl $MYVIMRC<CR>

""" Window
nnoremap J <C-W>j
nnoremap K <C-W>k
nnoremap L <C-W>l
nnoremap H <C-W>h
nnoremap <M-J> <C-W>J
nnoremap <M-K> <C-W>K
nnoremap <M-L> <C-W>L
nnoremap <M-H> <C-W>H


""" Quit Tab
nnoremap Q :q<CR>
""" Write
nnoremap W :update<CR>

""" Terminal
nnoremap <C-`> :vspl term://zsh<CR><C-W>L
tnoremap <M-c> <C-\><C-N>

""" ------- GENERAL CONFIGURE END -------

""" ------- LANGUAGE SPECIFIC BEGIN -------

" cpp, c++
autocmd FileType cpp set mps+=<:>

" haskell

let g:hs_highlight_boolean = 1
let g:hs_highlight_types = 1
let g:hs_highlight_more_types = 1
let g:hs_highlight_debug = 1

autocmd FileType haskell nnoremap <leader>cf :%!stylish-haskell<CR>

""" ------- LANGUAGE SPECIFIC END -------

""" ------- PLUGINS CONFIGURE BEGIN -------

" nerdtree

nnoremap <leader>nt :NERDTree<CR>

" nerdcommenter

let g:NERDSpaceDelims = 1
let g:NERDTrimTrailingWhitespace = 1

let g:NERDBlockComIgnoreEmpty = 0
let g:NERDCommentEmptyLines = 1

let g:NERDCustomDelimiters = {
            \ 'cpp': {'left': '//', 'leftAlt': '/*', 'rightAlt': '*/'},
            \ 'c': {'left': '//', 'leftAlt': '/*', 'rightAlt' : '*/'},
            \ 'agda': {'left': '--', 'leftAlt': '{-', 'rightAlt': '-}'}
            \ }

" airline

let g:airline_extensions = ["branch", "syntastic"]
let g:airline#extensions#syntastic#enabled = 1
let g:airline#extensions#branch#enabled = 1
let g:airline_section_c = "%f"
let g:airline_section_z = "ln:%-3l col:%-2c"

" auto-pairs

let g:AutoPairsMultilineClose = 0

" easymotion

map <space> <Plug>(easymotion-prefix)

" syntastic

" let g:syntastic_always_populate_loc_list = 1
" let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_debug = 0

" syntastic-python
let g:syntastic_python_checkers = ["pep8", "python", "flake8"]
let g:syntastic_python_python_exec = "/usr/local/bin/python3"

" syntastic-cpp
let g:syntastic_cpp_checkers = ["gcc"]
let g:syntastic_cpp_gcc_compiler = "clang++"
let g:syntastic_cpp_check_header = 1
let g:syntastic_cpp_no_include_search = 1
let g:syntastic_cpp_config_file = ".include"
let g:syntastic_cpp_compiler_options = "-std=c++1z -Wall"

" syntastic-idris
let g:syntastic_idris_checkers = ["idris"]

let g:syntastic_enable_racket_racket_checker = 1

" ale



" clang-format

let g:clang_format#detect_style_file = 1
autocmd FileType c,cpp,java nnoremap <leader>cf :ClangFormat<CR>

" deoplete
inoremap <expr> <Tab> pumvisible() ? "\<C-N>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-P>" : "\<Tab>"

let g:deoplete#enable_at_startup = 1
call deoplete#custom#option({
    \ 'camel_case': v:true,
    \ 'max_list': 50,
    \ 'refresh_always': v:true,
    \ 'auto_complete_delay': 100
    \ })

" deoplete-clang
let g:deoplete#sources#clang#libclang_path =
            \ "/usr/local/Cellar/llvm/6.0.1/lib/libclang.dylib"
let g:deoplete#sources#clang#clang_header =
            \ "/usr/local/Cellar/llvm/6.0.1/lib/clang"

let g:deoplete#sources#clang#std = {'c': 'c11', 'cpp': 'cpp1z'}

" deoplete-jedi

let g:deoplete#sources#jedi#python_path = '/usr/local/bin/python3'

" c++ enhance higlight
let g:cpp_class_scope_highlight = 1
let g:cpp_member_variable_highlight = 1
let g:cpp_class_decl_highlight = 1
let g:c_operator_hightlight = 1

" fugitive

" git-gutter

" ultisnips

let g:UltiSnipsSnippetDirectories=[$HOME.'/.config/nvim/snippets']
let g:UltiSnipsExpandTrigger = "<C-z>"
let g:UltiSnipsEditSplit = "vertical"
let g:UltiSnipsUsePythonVersion = 3
let g:UltiSnipsEnableSnipMate = 0

" haskell-vim

let g:haskell_indent_if = 3
let g:haskell_indent_case = 4
let g:haskell_indent_let = 4
let g:haskell_indent_where = 6
let g:haskell_indent_do = 3
let g:haskell_indent_in = 1
let g:haskell_indent_guard = 4

" ghc-mod

function! s:mapGHCMod()
    nnoremap <localleader>rl :GhcModCheck<CR>
    nnoremap <localleader>t :GhcModType<CR>
    nnoremap <localleader>cl :GhcModTypeClear<CR>
    nnoremap <localleader>lt :GhcModLint<CR>
    nnoremap <localleader>c :GhcModSplitFunCase<CR>
endfunction

augroup GhcCommand
    autocmd!
    autocmd filetype haskell call s:mapGHCMod()
    autocmd filetype haskell
                \ autocmd BufWritePost <buffer> :GhcModCheckAsync
augroup end

""" ------- PLUGINS CONFIGURE END -------
