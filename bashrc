
# Check for an interactive session
[ -z "$PS1" ] && return

if [ -f /etc/bash_completion ]; then
 . /etc/bash_completion
fi

export GIT_EDITOR='vim'
export PATH=$HOME/scripts:$HOME/bin:$PATH
export PATH=$HOME/.cabal/bin:$PATH
export PATH=$HOME/.gem/ruby/1.9.1/bin:$PATH

export PS1='[$(date +%H:%M)|\w] \n\$ '

stty stop undef
stty start undef

shopt -s globstar

alias ..='cd ..'
alias .='ls --color=auto'
alias l='ls --color=auto'
alias ls='ls --color=auto'
alias sl='ls --color=auto'
alias s='ls --color=auto'
alias dir='ls --color=auto'

export GHC_INCLUDE_DIRS='.'

alias ghc='ghc -i$GHC_INCLUDE_DIRS'
alias ghci='ghci -i$GHC_INCLUDE_DIRS'
alias runghc='runghc -i$GHC_INCLUDE_DIRS'

alias cb='cabal build'
alias ci='cabal install'
alias cid='cabal install --disable-executable-profiling --disable-library-profiling'
alias cie='cabal install --enable-executable-profiling --enable-library-profiling'
alias cu='cabal update'

alias ga='git add'
alias gam='git commit --amend'
alias gb='git branch'
alias gc='git commit'
alias gcp='git cherry-pick'
alias gd='git diff --color'
alias gds='git diff --staged --color'
alias gg='git grep'
alias gl='git log'
alias gls='git log --pretty=oneline --abbrev-commit'
alias gm='git merge'
alias gn='git grep -n'
alias go='git checkout'
alias gp='git push'
alias gpl='git pull'
alias gr='git rebase'
alias gs='git status'
alias gsa='git submodule add'
alias gsi='git submodule update --init'
alias gsm='git submodule'
alias gsu='git submodule update'
alias gsw='git show --color'
alias gw='git diff --color-words --color'
alias gws='git diff --color-words --staged --color'

alias epf='eproof -tAuto -xAuto --tptp3-format'
alias epr='eprover -tAuto -xAuto --tptp3-format'
alias spa='SPASS -Auto -TPTP -PGiven=0 -PProblem=0 -DocProof=0 -PStatistic=0'
alias vam='vampire_lin32 -mode casc'
