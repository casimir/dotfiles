let mapleader = "j"

call plug#begin('~/.vim/plugged')
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'godlygeek/csapprox'

Plug 'bronson/vim-visual-star-search'
Plug 'vim-scripts/closetag.vim'

Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!

" git
Plug 'tpope/vim-fugitive'
Plug 'junegunn/gv.vim'
" Plug 'airblade/vim-gitgutter'

" Plug 'tpope/vim-rsi'            " readline
" Plug 'junegunn/vim-peekaboo'    " registers " @
" Plug 'junegunn/vim-pseudocl'    " dependency
" Plug 'junegunn/vim-fnr'         " <leader>R
Plug 'junegunn/vim-easy-align'  " gaip=
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)
Plug 'godlygeek/tabular'        " Tabularize

Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'    " cs"' yss} VS<p>
" Plug 'tpope/vim-speeddating' " c-a c-x for dates
" Plug 'tpope/vim-abolish'     " crs crm crc cru :%Subvert/child{,ren}/adult{,s}/g
Plug 'tpope/vim-commentary'  " gc gcs gcgc
" Plug 'mbbill/undotree'
Plug 'AndrewRadev/splitjoin.vim' " gS gJ

" " Is this the plugin that messes up insert mode?
" Plug 'Yggdroot/indentLine'
" autocmd! User indentLine doautocmd indentLine Syntax

Plug 'scrooloose/nerdtree'
augroup nerd_loader
  autocmd!
  autocmd VimEnter * silent! autocmd! FileExplorer
  autocmd BufEnter,BufNew *
        \  if isdirectory(expand('<amatch>'))
        \|   call plug#load('nerdtree')
        \|   execute 'autocmd! nerd_loader'
        \| endif
augroup END

" languages
Plug 'groenewege/vim-less'
Plug 'kchmck/vim-coffee-script'
Plug 'pangloss/vim-javascript'
Plug 'tpope/vim-markdown'

Plug 'klen/python-mode'
let g:pymode_rope_completion = 0
let g:pymode_rope_complete_on_dot = 0
let g:pymode_rope_lookup_project = 0
let g:pymode_lint_ignore = "C901,C0111,F0401"
let g:pymode_lint_checkers = ['pylint']
Plug 'davidhalter/jedi-vim'
Plug 'Valloric/YouCompleteMe'
Plug 'majutsushi/tagbar'

augroup mypython
au BufRead *py set ts=4
augroup END


Plug 'metakirby5/codi.vim'
call plug#end()

filetype plugin on  " Automatically detect file types.
filetype indent off " Just use autoindent
syntax on           " syntax highlighting
set mouse=a         " automatically enable mouse usage
set mousehide       " hide the mouse cursor while typing
scriptencoding utf-8

hi Vertsplit ctermfg=white ctermbg=black
" set enc=utf-8
set fillchars=vert:\│

set clipboard=unnamed      " share clipboard with x

set shortmess+=filmnrxoOtT " abbrev. of messages (avoids 'hit enter')
set virtualedit=onemore    " allow for cursor beyond last character
set history=100            " Store a ton of history (default is 20)
" set spell                " spell checking on
set hidden                 " allow buffer switching without saving
set showmode               " display the current mode

set laststatus=2

" Broken down into easily includeable segments
set statusline=%<%f\    " Filename
set statusline+=%w%h%m%r " Options
" set statusline+=%{fugitive#statusline()} "  Git Hotness
" set statusline+=\ [%{&ff}/%Y]            " filetype
set statusline+=\ %y            " filetype
" set statusline+=\ [%{getcwd()}]          " current dir
set statusline+=%=%-8.(%3{col('.')-1},%l%)\ %3p%%  " Right aligned file nav info

set backspace=indent,eol,start  " backspace for dummies
set linespace=0                 " No extra spaces between rows
set number                      " Line numbers on
set showmatch                   " show matching brackets/parenthesis
set incsearch                   " find as you type search
set hlsearch                    " highlight search terms
set winminheight=0              " windows can be 0 line high
set cmdheight=1                 " command height only one line!
set ignorecase                  " case insensitive search
set smartcase                   " case sensitive when uc present
set wildmenu                    " show list instead of just completing
set wildmode=full               " command <Tab> completion, list matches, then longest common part, then all.
set nowrap                      " don't wrap
set scrolljump=1                " lines to scroll when cursor leaves screen
set scrolloff=0                 " minimum lines to keep above and below cursor
set sidescroll=1
set nofoldenable                " no code folding

set autoindent                  " indent at the same level of the previous line
set shiftwidth=4                " use indents of 4 spaces
set expandtab                   " tabs are spaces, not tabs
set tabstop=4                   " an indentation every four columns
set softtabstop=4               " let backspace delete indent
set matchpairs+=<:>             " match, to be used with %
set matchpairs+=":"             " match, to be used with %
"set comments=sl:/*,mb:*,elx:*/  " auto format comment blocks

" Yank from the cursor to the end of the line, to be consistent with C and D.
nnoremap Y y$

function! Wrap()
    setl wrap
    setl linebreak
    noremap <buffer> t gj
    noremap <buffer> n gk
    noremap <buffer> $ g$
    noremap <buffer> 0 g0
    noremap <buffer> ^ g^
endfunction

" For when you forget to sudo.. Really Write the file.

function! W()
    write !sudo tee % >/dev/null
endfunction

noremap t j
noremap n k
noremap s l

noremap K T
noremap k t

noremap N n
noremap T N

" Window management etc (inspired from emacs)
nnoremap <tab> <c-w>w
nnoremap <S-tab> <c-w>W
noremap gn <C-w><C-w>
noremap g2 <C-w>s
noremap g3 <C-w>v
noremap g0 <C-w>c
noremap g1 <C-w>o
noremap g= <C-w>=

noremap gy :Goyo<cr>

" clear highlight
noremap <silent> <leader>/ :noh<cr>

noremap ,s :update<cr>
noremap ,q :bd<cr>
noremap ,n :bn<cr>
noremap ,p :bp<cr>
noremap ,l :so %<cr>:PlugInstall<cr>

nnoremap <c-n> :move-2<cr>
nnoremap <c-t> :move+<cr>
nnoremap <c-h> <<
nnoremap <c-s> >>
inoremap <c-h> <esc><<i
inoremap <c-s> <esc>>>i
xnoremap <c-n> :move-2<cr>gv
xnoremap <c-t> :move'>+<cr>gv
xnoremap <c-h> <gv
xnoremap <c-s> >gv
xnoremap < <gv
xnoremap > >gv

noremap : ;
noremap ; :

noremap ,r :%s

" ----------------------------------------------------------------------------
" ?ii / ?ai | indent-object
" ?io       | strictly-indent-object
" ----------------------------------------------------------------------------
function! s:indent_len(str)
  return type(a:str) == 1 ? len(matchstr(a:str, '^\s*')) : 0
endfunction

function! s:indent_object(op, skip_blank, b, e, bd, ed)
  let i = min([s:indent_len(getline(a:b)), s:indent_len(getline(a:e))])
  let x = line('$')
  let d = [a:b, a:e]

  if i == 0 && empty(getline(a:b)) && empty(getline(a:e))
    let [b, e] = [a:b, a:e]
    while b > 0 && e <= line('$')
      let b -= 1
      let e += 1
      let i = min(filter(map([b, e], 's:indent_len(getline(v:val))'), 'v:val != 0'))
      if i > 0
        break
      endif
    endwhile
  endif

  for triple in [[0, 'd[o] > 1', -1], [1, 'd[o] < x', +1]]
    let [o, ev, df] = triple

    while eval(ev)
      let line = getline(d[o] + df)
      let idt = s:indent_len(line)

      if eval('idt '.a:op.' i') && (a:skip_blank || !empty(line)) || (a:skip_blank && empty(line))
        let d[o] += df
      else | break | end
    endwhile
  endfor
  execute printf('normal! %dGV%dG', max([1, d[0] + a:bd]), min([x, d[1] + a:ed]))
endfunction
xnoremap <silent> ii :<c-u>call <SID>indent_object('>=', 1, line("'<"), line("'>"), 0, 0)<cr>
onoremap <silent> ii :<c-u>call <SID>indent_object('>=', 1, line('.'), line('.'), 0, 0)<cr>
xnoremap <silent> ai :<c-u>call <SID>indent_object('>=', 1, line("'<"), line("'>"), -1, 1)<cr>
onoremap <silent> ai :<c-u>call <SID>indent_object('>=', 1, line('.'), line('.'), -1, 1)<cr>
xnoremap <silent> io :<c-u>call <SID>indent_object('==', 0, line("'<"), line("'>"), 0, 0)<cr>
onoremap <silent> io :<c-u>call <SID>indent_object('==', 0, line('.'), line('.'), 0, 0)<cr>


" ----------------------------------------------------------------------------
" <Leader>I/A | Prepend/Append to all adjacent lines with same indentation
" ----------------------------------------------------------------------------
nmap <silent> ,I ^vii<C-V>I
nmap <silent> ,A ^vii<C-V>$A
nmap <silent> ,i ^vio<C-V>I
nmap <silent> ,a ^vio<C-V>$A

" " CloseTag in html, xhtml, xml and others
" augroup myCloseTag
"     au!
"     au FileType html,htmldjango,jinjahtml,eruby,mako let b:closetag_html_style=1
"     au FileType html,xhtml,xml,htmldjango,jinjahtml,eruby,mako source ~/.vim/bundle/closetag/plugin/closetag.vim
" augroup END
"
" " Tagbar
" " let g:tagbar_usearrows = 1
"
" " tagbar
" " nnoremap <leader>t :TagbarToggle<CR>

" [Buffers] Jump to the existing window if possible
let g:fzf_buffers_jump = 1

" [Tags] Command to generate tags file
let g:fzf_tags_command = 'ctags -R'

nnoremap <silent> <Leader>h :Help<cr>
nnoremap <silent> <Leader>l :Lines<cr>
nnoremap <silent> <Leader>f :Files<cr>
nnoremap <silent> <Leader>g :GFiles<cr>
nnoremap <silent> <Leader>s :GFiles?<cr>
nnoremap <silent> <Leader>b :Buffers<cr>
nnoremap <silent> <Leader>t :Tags<cr>
nnoremap <silent> <Leader>m :History<cr>

" Insert mode completion
imap <c-c><c-c> <plug>(fzf-complete-word)
imap <c-c><c-p> <plug>(fzf-complete-path)
imap <c-c><c-f> <plug>(fzf-complete-file-ag)
imap <c-c><c-l> <plug>(fzf-complete-line)

colorscheme danr

let g:EasyMotion_leader_key = ','

" TAGS
" goto tag
noremap <leader>t <C-]>
" pop tag stack
noremap <leader>p <C-T>
" goto tag, but in split window
noremap <leader>s <C-w>i

if exists(":UndotreeToggle")
    nmap <Leader>u :UndotreeToggle<cr>
endif

" " haskell
" let g:ghc="/usr/bin/ghc"
" let g:haddock_browser="/usr/bin/firefox"
" "let g:haddock_browser_callformat = '%s file://%s '.printf(&shellredir,'/dev/null').' &'
" let g:haddock_docdir="/home/dan/.cabal/share/doc"
" let g:haddock_indexfiledir="~/.vim/"
" let g:wget="/usr/bin/wget/"
" let g:haddock_browser="/usr/bin/firefox"

" function! s:add_package_ghc()
"   if !exists('b:ghcmod_ghc_options')
"     let b:ghcmod_ghc_options = []
"   endif
"   call add(b:ghcmod_ghc_options, '-package ghc')
"   if !exists('b:syntastic_haskell_checker_args')
"     let b:syntastic_haskell_checker_args = []
"   endif
"   call add(b:syntastic_haskell_checker_args, '-package ghc')
"   let g:syntastic_haskell_checker_args = '-package ghc'
"   let b:ghc_staticoptions = '-package ghc'
" endfunction
"
" " haskellmode
" augroup myHaskell
"     au!
"     au BufEnter *.hs compiler ghc
"     au BufEnter *.hs set cmdheight=1
"     au FileType haskell let b:ghc_staticoptions = '-w'
"     au BufRead,BufNewFile ~/code/tfp1/* call s:add_package_ghc()
"     au BufRead,BufNewFile ~/code/hipspec/* call s:add_package_ghc()
" "    au BufWritePost ~/code/hipspec/*.hs GhcModCheckAsync
" "    au BufWritePost ~/code/LL/*.hs GhcModCheckAsync
" "    au BufWritePost ~/code/LL/*/*.hs GhcModCheckAsync
" augroup END
"
" " literate haskell doesn't really work, fall back to haskell
" augroup myLiterateHaskell
"     au!
" "    au BufEnter *.lhs setf haskell
"     au BufEnter *.lhs setf tex
"     au BufEnter *.lhs set conceallevel=0
"     au BufEnter *.lhs compiler ghc
"     au BufEnter *.lhs set cmdheight=1
" "    au BufWritePost *.lhs GhcModCheckAsync
" augroup END
"

" from bronson/vim-trailing-whitespace
function! s:FixWhitespace(line1,line2)
    let l:save_cursor = getpos(".")
    silent! execute ':' . a:line1 . ',' . a:line2 . 's/\\\@<!\s\+$//'
    call setpos('.', l:save_cursor)
endfunction

" Run :FixWhitespace to remove end of line white space
command! -range=% FixWhitespace call <SID>FixWhitespace(<line1>,<line2>)

augroup vimrc
    au!
    au BufEnter *.ll     setf llvm
    au BufEnter *.smt setf lisp
    au BufEnter *.dfy setf dafny
    au BufEnter *.w setf waldmeister
    au BufEnter *.json set ft=json

    " Always strip trailing whitespace
    au BufWritePre * FixWhitespace

    " Set sw/ts to 2 in html
    au FileType html,xhtml,xml,htmldjango,jinjahtml,eruby,mako setlocal ts=2
    au FileType html,xhtml,xml,htmldjango,jinjahtml,eruby,mako setlocal sw=2
augroup END

set hidden
