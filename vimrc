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
syntax on

noremap t j
noremap n k
noremap s l

noremap K S
noremap k s
noremap N n
noremap T N

set statusline+=[L:\ %l/%L]\ [C:\ %v]\ [%p%%]

ab cdata <![CDATA[ ]]><ESC>4hi

