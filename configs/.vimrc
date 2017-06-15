syntax enable
syntax on
filetype on

colorscheme elflord

" about line number
set number numberwidth=5
set relativenumber

" about cursor
set ruler cursorline cursorcolumn gcr=a:block-blinkon0

" about tabs and indents
set tabstop=4 shiftwidth=4 shiftround softtabstop=4 expandtab cindent
set backspace=2
filetype indent on

" highlight search result
set hlsearch incsearch

" shell
" set shell=/bin/bash

" fold
" set foldmethod=syntax

""" mappings

let mapleader = "-"

nnoremap <leader>rl :source $MYVIMRC<CR>
nnoremap <leader>wq :wq<CR>
nnoremap <silent> <leader>nh :nohlsearch<CR>

" CTRL-s save
inoremap <c-s> <c-c>:update<CR>
nnoremap <c-s> :update<CR>

" CTRL-A select all
inoremap <c-a> <c-c>ggVG
nnoremap <c-a> ggVG

nnoremap <left> <nop>
nnoremap <right> <nop>
nnoremap <up> <nop>
nnoremap <down> <nop>

nnoremap Q :q<CR> 
nnoremap W :update<CR>


" move in splits

nnoremap J <C-W>j
nnoremap K <C-W>k
nnoremap L <C-W>l
nnoremap H <C-W>h

" map inside braces

onoremap 9 i(
onoremap ( a(
onoremap [ i[
onoremap { i{
onoremap , i<
onoremap < a<

""" mappings end


""" vundle setup 

set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" To ignore plugin indent changes, instead use:

"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal

" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line
" let Vundle manage Vundle, required

Plugin 'VundleVim/Vundle.vim'
Plugin 'altercation/vim-colors-solarized'
Plugin 'scrooloose/nerdtree'
Plugin 'Valloric/YouCompleteMe'
Plugin 'easymotion/vim-easymotion'
Plugin 'tpope/vim-surround'
Plugin 'jiangmiao/auto-pairs'
Plugin 'kien/ctrlp.vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/syntastic'
Plugin 'rhysd/vim-clang-format'
Plugin 'majutsushi/tagbar'
Plugin 'octol/vim-cpp-enhanced-highlight'
Plugin 'bling/vim-airline'
Plugin 'tpope/vim-markdown'
Plugin 'elzr/vim-json'
Plugin 'davidhalter/jedi-vim'
Plugin 'shougo/vimproc.vim'
Plugin 'neovimhaskell/haskell-vim'
Plugin 'eagletmt/ghcmod-vim'
Plugin 'shougo/neocomplete.vim'
Plugin 'eagletmt/neco-ghc'
Plugin 'fatih/vim-go'
Plugin 'derekwyatt/vim-scala'
Plugin 'tomasr/molokai'
Plugin 'mattn/emmet-vim'
Plugin 'pangloss/vim-javascript'
Plugin 'wlangstroth/vim-racket'
Plugin 'kien/rainbow_parentheses.vim'
Plugin 'tpope/vim-repeat'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

""" vundle setup end

""" YouCompleteMe 

let g:ycm_confirm_extra_conf = 0
let g:ycm_complete_in_comments = 1
let g:ycm_min_num_of_chars_for_completion = 1
let g:ycm_filetype_blacklist = { 'haskell': 1, 'racket': 1, 'vim': 1 }
nnoremap <leader>dk :YcmCompleter GoToDeclaration<CR>
nnoremap <leader>df :YcmCompleter GoToDefinition<CR>

""" airline

let g:airline#extensions#tabline#enabled = 0
let g:airline#extensions#syntastic#enabled = 1
let g:airline_section_b = '%{getcwd()}'
let g:airline_section_c = '%t'

""" airline end

""" vim-markdown

let g:markdown_fenced_languages = ["html", "python", "cpp", "c", "java", "bash=sh"]
let g:markdown_syntax_conceal = 0

""" syntastic

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}

set statusline+=%*

" let g:syntastic_always_populate_loc_list = 1
" let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:Syntastic_python_checkers = ["flake8", "python"]
let g:Syntastic_python_python_exec = ["python3"]

let g:Syntastic_cpp_checkers = ["gcc"]
let g:Syntastic_cpp_gcc_compiler = "clang++"
let g:syntastic_cpp_compiler_options = "-std=c++14 --fsyntax-only -Wall"

let g:syntastic_enable_racket_racket_checker = 1

""" nerdtree

nnoremap <leader>nt :NERDTree<CR>

""" NerdCommenter

let g:NERDSpaceDelims = 1
let g:NERDTrimTrailingWhitespace = 1

let g:NERDBlockComIgnoreEmpty = 0
let g:NERDCommentEmptyLines = 1

let g:NERDCustomDelimiters = {
            \ 'cpp': {'left': '//', 'leftAlt': '/*', 'rightAlt': '*/'}, 
            \ 'c': {'left': '//', 'leftAlt': '/*', 'rightAlt' : '*/'}
            \ }

""" ClangFormat

let g:clang_format#style_options = {
            \ "BasedOnStyle": "Google",
            \ "IndentWidth": 4,
            \ "AllowShortFunctionsOnASingleLine": "Empty",
            \ "AllowShortBlocksOnASingleLine": "false",
            \ "AllowShortLoopsOnASingleLine": "false",
            \ "AllowShortIfStatementsOnASingleLine": "false",
            \ "AccessModifierOffset": -2,
            \ "AlignAfterOpenBracket": "AlwaysBreak",
            \ }


"let g:clang_format#detect_style_file = 1

""" language specific snippets

iabbrev mian main

augroup CustomSnippet
    autocmd!
    autocmd FileType c,cpp 
                \ nnoremap <buffer> <leader>main 
                \ iint main(int argc, char **argv) {<CR><CR>}<CR><up><up><tab>
    autocmd FileType c,cpp 
                \ nnoremap <buffer> <leader>ins
                \ O#include <><left>
    autocmd FileType c,cpp
                \ nnoremap <buffer> <leader>inc
            \ O#include ""<left>
augroup end

augroup AutoPep8
    autocmd!
    autocmd FileType python nnoremap <leader>cf :%!autopep8 -a -a -<CR>

augroup EditConvenience
    autocmd!
    autocmd FileType c,cpp,java inoremap <C-e> <end>;
    autocmd FileType c,cpp,java inoremap <C-b> <end><space>{}<left>
    autocmd FileType python inoremap <C-b> <end>:<CR>
    autocmd FileType python inoremap <C-e> <end>
augroup end

augroup FormatCommand
    autocmd!
    autocmd FileType c,cpp,java nnoremap <leader>cf :ClangFormat<CR>
    autocmd FileType haskell nnoremap <leader>cf :%!stylish-haskell<CR>
augroup end

""" emmet

let g:user_emmet_leader_key='<C-x>'

""" auto-pairs

let g:AutoPairsMultilineClose = 0

""" easymotion

map <space> <Plug>(easymotion-prefix)

""" vim-go

let g:go_term_mode = "split"

""" tagbar

nnoremap <leader>tb :Tagbar<CR>
let g:tagbar_width = 30

""" ghc-mod

augroup GhcCommand
    autocmd filetype haskell nnoremap <leader>ck :GhcModCheck<CR>
    autocmd filetype haskell nnoremap <leader>tp :GhcModType<CR>
    autocmd filetype haskell nnoremap <leader>cl :GhcModTypeClear<CR>
    autocmd filetype haskell nnoremap <leader>lt :GhcModLint<CR>
    autocmd filetype haskell nnoremap <leader>spl :GhcModSplitFunCase<CR>
augroup end

""" haskell-vim

let g:haskell_indent_if = 3
let g:haskell_indent_case = 4
let g:haskell_indent_let = 4
let g:haskell_indent_where = 6
let g:haskell_indent_do = 3
let g:haskell_indent_in = 1
let g:haskell_indent_guard = 4

""" neo-ghc

let g:haskellmode_completion_ghc = 0
let g:necoghc_enable_detailed_browse = 1
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

""" neocomplete
autocmd filetype haskell,racket,vim :NeoCompleteEnable
let g:neocomplete#enable_refresh_always = 1
let g:neocomplete#enable_camel_case = 1
let g:neocomplete#auto_complete_delay = 40
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

""" racket-vim

augroup Lisp
    autocmd!
    autocmd filetype racket :RainbowParenthesesActivate
    autocmd filetype racket :RainbowParenthesesLoadRound
    autocmd filetype racket :RainbowParenthesesLoadSquare
    autocmd filetype racket :RainbowParenthesesLoadBraces
    " disable racket plugin
    autocmd filetype racket nnoremap K <C-W>k 

    " same as scheme
    autocmd filetype racket let g:neocomplete#keyword_patterns.racket =
                \ '[[:alpha:]!$%&*+/:<=>?@\^_~\-][[:alnum:]!$%&*./:<=>?@\^_~\-]*'
    autocmd filetype racket exec 'iabbrev <buffer> lambda (λ'
    autocmd filetype racket let b:AutoPairs = {'(': ')', '[': ']', '{': '}','"': '"'}
augroup end
