let UseVimplug=1

let PluginsInstalled=0
if UseVimplug == 1
    " Bootstrap plugin manager
    let vplug=expand('~/.vim/autoload/plug.vim')
    if !filereadable(vplug)
        echo "Installing vim-plug.."
        echo ""
        silent !mkdir -p ~/.vim/autoload
        silent !git clone --depth=1 https://github.com/junegunn/vim-plug ~/.vim/autoload
    endif
    " Detect if plugins have been installed
    let PluginsInstalled=1
    let PluginsDir=expand('~/.vim/plugged')
    if !isdirectory(PluginsDir)
        echo "Installing plugins.."
        let PluginsInstalled=0
    endif
endif

if UseVimplug == 1
    call plug#begin('~/.vim/plugged')

    " My Plugins here:
    Plug 'kien/rainbow_parentheses.vim'
    Plug 'tpope/vim-fugitive'
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-repeat'
    Plug 'gregsexton/gitv'
    Plug 'mhinz/vim-signify'
    Plug 'scrooloose/syntastic'
    Plug 'scrooloose/nerdtree'
    Plug 'kien/ctrlp.vim'
    Plug 'ervandew/supertab'
    Plug 'mattn/webapi-vim'
    Plug 'mattn/gist-vim'
    Plug 'terryma/vim-multiple-cursors'
    " Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'

    Plug 'vim-scripts/paredit.vim', { 'for': 'clojure' }
    Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
    Plug 'tpope/vim-classpath', { 'for': 'clojure' }
    Plug 'tpope/vim-salve', { 'for': 'clojure' }
    Plug 'guns/vim-clojure-static', { 'for': 'clojure' }
    Plug 'guns/vim-clojure-highlight', { 'for': 'clojure' }
    Plug 'vim-scripts/verilog_systemverilog.vim'
    Plug 'fatih/vim-go'
    Plug 'vim-scripts/groovy.vim'
    Plug 'tfnico/vim-gradle'
    Plug 'mileszs/ack.vim' 
    Plug 'vim-ruby/vim-ruby'
    Plug 'tpope/vim-rails'
    Plug 'tpope/vim-bundler'
    Plug 'tpope/vim-endwise'
    Plug 'mileszs/apidock.vim'
    Plug 'voxpupuli/vim-puppet'
  
    Plug 'bling/vim-airline'
    Plug 'flazz/vim-colorschemes'

    call plug#end()
endif

if UseVimplug ==1 && PluginsInstalled == 0
    :PlugInstall
    let PluginsInstalled=1
endif

" Done bootstrapping plugin manager

" Global settings (regardless of plugins)
filetype plugin indent on

syntax on
set hlsearch

" Custom Leader bindings
let mapleader=","
let maplocalleader="\\"

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

" numbers
set number
set numberwidth=4

set nobackup
set history=50      " keep 50 lines of command line history
set ruler           " show the cursor position all the time
set showcmd         " display incomplete commands
set incsearch       " do incremental searching

set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab
set smartindent

set statusline=%F%m%r%h%w\ 
set statusline+=%=
set statusline+=\ [line\ %l\/%L]
set laststatus=2

" case only matters with mixed case expressions
set ignorecase
set smartcase

set nolist
set listchars=eol:¬,extends:»,tab:▸\ ,trail:›

set mouse=a

" Plugin settings
if UseVimplug == 1 && PluginsInstalled == 1
    " Add git status to statusline
    set statusline=%F%m%r%h%w\ 
    set statusline+=%=
    set statusline+=%{fugitive#statusline()}
    set statusline+=\ [line\ %l\/%L]
    set laststatus=2

    " vim-go settings
    au Filetype go nnoremap <leader>v :vsp <CR>:exe "GoDef" <CR>
    au Filetype go nnoremap <leader>s :sp <CR>:exe "GoDef"<CR>
    au Filetype go nnoremap <leader>t :tab split <CR>:exe "GoDef"<CR>

    " Rainbow Parentheses settings
    au VimEnter * RainbowParenthesesToggle
    au Syntax * RainbowParenthesesLoadRound
    au Syntax * RainbowParenthesesLoadSquare
    au Syntax * RainbowParenthesesLoadBraces
    let g:rbpt_max = 8
    " NERDTree
    map <Leader>n :NERDTreeToggle<CR>
    " Fugitive
    map <Leader>gs :Gstatus<CR>
    map <Leader>gd :Gdiff<CR>
    map <Leader>gc :Gcommit<CR>
    map <Leader>gb :Gblame<CR>
    map <Leader>gl :Glog<CR>
    map <Leader>gp :Git push<CR>
    " Colorscheme stuff
    set background=dark
    colorscheme Chasing_Logic

    " Make syntastic shut up about asm files
    let g:loaded_syntastic_asm_gcc_checker = 1

    " Set up ruby_path to use system ruby, so that it doesn't use jruby, which
    " kills startup time
    let g:ruby_path='/usr/bin/ruby'

    " This macro increments every number on a line by one. Very useful for
    " deploy templates
    let @i='yypV:s/\d\+/\=(submatch(0)+1)/ga'
endif
