call pathogen#runtime_append_all_bundles()
call pathogen#infect()
call pathogen#helptags()

let mapleader = ","

filetype plugin indent on   " Automatically detect file types.
syntax on                   " syntax highlighting
set mouse=a                 " automatically enable mouse usage
set mousehide               " hide the mouse cursor while typing
scriptencoding utf-8

set shortmess+=filmnrxoOtT      " abbrev. of messages (avoids 'hit enter')
set virtualedit=onemore         " allow for cursor beyond last character
set history=100                 " Store a ton of history (default is 20)
" set spell                       " spell checking on
set hidden                      " allow buffer switching without saving

set showmode                    " display the current mode

set laststatus=2

" Broken down into easily includeable segments
set statusline=%<%f\    " Filename
set statusline+=%w%h%m%r " Options
" set statusline+=%{fugitive#statusline()} "  Git Hotness
set statusline+=\ [%{&ff}/%Y]            " filetype
" set statusline+=\ [%{getcwd()}]          " current dir
set statusline+=%=%-8.(%3{col('.')-1},%l%)\ %3p%%  " Right aligned file nav info

set backspace=indent,eol,start  " backspace for dummies
set linespace=0                 " No extra spaces between rows
set number                      " Line numbers on
set showmatch                   " show matching brackets/parenthesis
set incsearch                   " find as you type search
set hlsearch                    " highlight search terms
set winminheight=0              " windows can be 0 line high
set ignorecase                  " case insensitive search
set smartcase                   " case sensitive when uc present
set wildmenu                    " show list instead of just completing
set wildmode=list:longest,full  " command <Tab> completion, list matches, then longest common part, then all.
set whichwrap=b,s,h,l,<,>,[,]   " backspace and cursor keys wrap to
set scrolljump=1                " lines to scroll when cursor leaves screen
set scrolloff=0                 " minimum lines to keep above and below cursor
" set foldenable                  " auto fold code

set autoindent                  " indent at the same level of the previous line
set shiftwidth=4                " use indents of 4 spaces
set expandtab                   " tabs are spaces, not tabs
set tabstop=4                   " an indentation every four columns
set softtabstop=4               " let backspace delete indent
"set matchpairs+=<:>                " match, to be used with %
"set comments=sl:/*,mb:*,elx:*/  " auto format comment blocks

" Yank from the cursor to the end of the line, to be consistent with C and D.
nnoremap Y y$

" visual shifting (does not exit Visual mode)
vnoremap < <gv
vnoremap > >gv

" For when you forget to sudo.. Really Write the file.
cmap w!! w !sudo tee % >/dev/null

" Fugitive {
   nnoremap <silent> <leader>gs :Gstatus<CR>
   nnoremap <silent> <leader>gd :Gdiff<CR>
   nnoremap <silent> <leader>gc :Gcommit<CR>
   nnoremap <silent> <leader>gb :Gblame<CR>
   nnoremap <silent> <leader>gl :Glog<CR>
   nnoremap <silent> <leader>gp :Git push<CR>
"}

"colorscheme bclear

noremap t j
noremap n k
noremap s l

noremap K T
noremap k t

noremap N n
noremap T N

" Window management etc (inspired from emacs)
" noremap <tab> <C-w><C-w>
noremap gn <C-w><C-w>
noremap g2 <C-w>s
noremap g3 <C-w>v
noremap g0 <C-w>c
noremap g1 <C-w>o
noremap g= <C-w>=

noremap go :CtrlP<cr>
noremap jb :BufExplorer<cr>j
noremap j/ :noh<cr>

" Always source vimrc when saving it
" augroup source_vimrc
"     au!
"     au BufWritePost *.vimrc :source $MYVIMRC
" augroup END

" Strip whitespace
function! StripTrailingWhitespace()
" Preparation: save last search, and cursor position.
    let _s=@/
    let l = line(".")
    let c = col(".")
" Do the business:
    %s/\s\+$//e
" Clean up: restore previous search history, and cursor position
    let @/=_s
    call cursor(l, c)
endfunction

" Always strip trailing whitespace
augroup my-strip
    au!
    au BufWritePre * call StripTrailingWhitespace()
augroup END

augroup my-coffee
    au!
    au BufWritePost *.coffee !coffee -c <afile>
augroup END

augroup my-sass
    au!
    au BufWritePost *.sass !sass <afile> <afile>:r.css
augroup END
