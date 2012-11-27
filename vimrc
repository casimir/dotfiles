set nocp
set showmode
set showcmd
set showmatch
set tabstop=4
set shiftwidth=4
set autoindent
set expandtab
set number
set nowrap
set hlsearch
set incsearch
syntax on

" Allow switching buffer when buffer is modified
set hidden

set wildmode=longest,list,full
set wildmenu

" n: normal
" o: operator-pending
" v: visual
" i: insert
" c: commmand line

noremap t j
noremap n k
noremap s l

noremap K S
noremap k s
noremap N n
noremap T N
 
" Window management etc (inspired from emacs)
noremap <tab> <C-w><C-w>
noremap gn <C-w><C-w>
noremap g2 <C-w>s
noremap g3 <C-w>v
noremap g0 <C-w>c
noremap g1 <C-w>o

"Bind the BufSel() function to a user-command
"look at http://vim.wikia.com/wiki/Easier_buffer_switching

" Always source vimrc when saving it
augroup source_vimrc 
    au! 
    au BufWritePost *.vimrc :source $MYVIMRC
augroup END

" always show status
set laststatus=2
set statusline=%y\ %f%m%r\ (%02v,%03l)%=%p%%\  

