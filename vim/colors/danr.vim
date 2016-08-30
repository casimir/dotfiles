" vim: sw=4
" Vim color file

set background=light
hi clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name = "danr"

" Vim >= 7.0 specific colors
if version >= 700
    hi CursorLine            guibg=#e8f2ff
    hi CursorColumn          guibg=#e8f2ff
    hi MatchParen   guifg=White   guibg=#4088d0   gui=NONE
    hi Pmenu    guifg=White  guibg=#222222
    hi PmenuSel guifg=White  guibg=#4088d0
    hi WildMenu guifg=White  guibg=#4088d0
endif

" General colors
hi Cursor     guifg=White    guibg=Black
hi lCursor    guifg=White    guibg=Black
hi Normal     guifg=Black    guibg=White
hi NonText    guifg=gray40   guibg=White
hi LineNR     guifg=#cc7a00  guibg=White
hi SpecialKey guifg=#ff00ff
hi Title      guifg=Black                  gui=NONE
hi Visual     guibg=gray95
hi Search     guifg=White  guibg=#4088d0
" hi Search     guifg=Black  guibg=#cc7a00

" Syntax highlighting
hi Constant   guifg=#aa0000
hi Number     guifg=#aa0000
hi String     guifg=#aa0000
hi Function   guifg=#aa0000
hi hsSpecialChar guifg=#aa0000 gui=bold
hi Directory  guifg=#008000 gui=NONE

" pragmas
hi PreProc    guifg=#aa0000 gui=NONE

" deriving, module, let, in, where, do, =, ::, =>
hi Structure guifg=#008000 gui=NONE
" data, type
hi Typedef guifg=#008000 gui=NONE

hi Type guifg=#008000 gui=NONE
" type signatures
hi hsType guifg=black gui=NONE
hi hsIdentifier guifg=black gui=NONE

" import
hi Statement guifg=#aa00aa gui=NONE
" qualified safe as hiding default family
hi Keyword guifg=#aa00aa gui=NONE

" () [] { , } |
" hi Special guifg=#cc7a00 gui=NONE
hi Special guifg=#888888 gui=NONE

" case, of, if, then, else
hi Conditional guifg=#800000 gui=NONE
hi hsExprKeyword guifg=#800000 gui=NONE

hi Operator guifg=#aa00aa gui=NONE

hi Error guifg=white guibg=#aa0000

" comments
hi Comment guifg=#2048a0 gui=NONE
hi Todo    guifg=#4088d0 guibg=#ffffff gui=bold

hi Conceal    guifg=#000000 guibg=#ffffff

hi DiffAdd     guifg=#008000 guibg=#ffffff gui=bold
hi DiffChange  guifg=#aa00aa guibg=#ffffff gui=bold
hi DiffDelete  guifg=#800000 guibg=#ffffff gui=bold
hi DiffText                  guibg=#ffffff gui=bold
hi SignColumn                guibg=#ffffff

hi SyntasticErrorSign         guifg=#800000 guibg=#ffffff gui=bold
hi SyntasticStyleWarningSign  guifg=#cc7a00 guibg=#ffffff gui=bold

hi FoldColumn guifg=DarkBlue guibg=gray95
hi Folded     guifg=red      guibg=#ffffff

