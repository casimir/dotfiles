
# Check for an interactive session
[ -z "$PS1" ] && return

if [ -f /etc/bash_completion ]; then
 . /etc/bash_completion
fi

stty stop undef
stty start undef

shopt -s globstar

alias ..='cd ..'
alias .='ls --color=auto'
alias l='ls --color=auto'
alias ls='ls --color=auto'
alias s='ls --color=auto'
alias dir='ls --color=auto'
alias ghc='ghc -i$GHC_INCLUDE_DIRS'
alias ghci='ghci -i$GHC_INCLUDE_DIRS'
alias runghc='runghc -i$GHC_INCLUDE_DIRS'

alias ci='cabal install'
alias cb='cabal build'
alias cu='cabal update'

alias ga='git add'
alias gam='git commit --amend'
alias gb='git branch'
alias gc='git commit'
alias gcp='git cherry-pick'
alias gd='git diff --color'
alias gw='git diff --color-words --color'
alias gds='git diff --staged --color'
alias gws='git diff --color-words --staged --color'
alias gg='git grep'
alias gl='git log'
alias gm='git merge'
alias gn='git grep -n'
alias go='git checkout'
alias gp='git push'
alias gpl='git pull'
alias gr='git rebase'
alias gs='git status'
alias gsl='git log --pretty=oneline --abbrev-commit'
alias gsm='git submodule'
alias gsu='git submodule update'
alias gsi='git submodule update --init'

alias cid='cabal install --disable-executable-profiling --disable-library-profiling'
alias cie='cabal install --enable-executable-profiling --enable-library-profiling'

alias epr='eprover -tAuto -xAuto --tptp3-format'
alias spa='SPASS -Auto -TPTP -PGiven=0 -PProblem=0 -DocProof=0 -PStatistic=0'
alias vam='vampire_lin32 -mode casc'
alias epf='eproof -tAuto -xAuto --tptp3-format'
alias hipghcdirs='export GHC_INCLUDE_DIRS=\'/home/dan/hip/src/:.''

# export PYTHONPATH=/usr/lib/python3.2/site-packages/:$PYTHONPATH
export GHC_INCLUDE_DIRS='.'
export GIT_EDITOR='emacsclient'
export PATH=/home/dan/scripts:/home/dan/bin:/home/dan/.cabal/bin:/opt/cuda-toolkit/bin:$PATH
# PS1='[\u@\h \W]\$ '
# PS1='[\w]\$ '
# export PS1='[$(date +%H:%M)|\w|$(__git_ps1 "%s")] \n\$ '
export PS1='[$(date +%H:%M)|\w] \n\$ '
