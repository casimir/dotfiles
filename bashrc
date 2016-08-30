
# Check for an interactive session
[ -z "$PS1" ] && return

if [ -f /etc/bash_completion ]; then
 . /etc/bash_completion
fi

#source $HOME/.nix-profile/etc/profile.d/nix.sh

export HISTCONTROL=ignorespace:ignoreboth
export HISTFILESIZE=40000000
export HISTSIZE=10000
export PROMPT_COMMAND="history -a"
shopt -s histappend

export XDG_CONFIG_HOME=$HOME/config
export GIT_EDITOR='vim'
export PATH=/home/dan/build/pakcs-1.11.1/bin:$PATH
export PATH=$HOME/scripts:$HOME/bin:$HOME/bin/provers:$HOME/code/provers:$PATH
export PATH=$HOME/.cabal/bin:$PATH
export PATH=$HOME/code/jbofihe:$PATH
export PATH=$HOME/.gem/ruby/*/bin:$PATH

export PS1='[$(date +%H:%M)|\w] \n\$ '

alias hipspec='emna -v=2'
alias hip='emna --explore=off'

alias dafny='mono ~/code/dafny/Binaries/Dafny.exe'

# if [ ! "$TERM" == "rxvt-unicode-256color" ]; then
#     setterm -blength 0
# fi

stty stop undef
stty start undef

shopt -s globstar
shopt -s autocd

alias today='date +"%Y-%m-%d"'
alias now='date +"%Y-%m-%d-%H%M"'

alias ..='cd ..'
alias .='ls --color=auto'
alias l='ls --color=auto'
alias ls='ls --color=auto'
alias sl='ls --color=auto'
alias s='ls --color=auto'
alias dir='ls --color=auto'

alias ai='sudo apt-get install'
alias as='sudo apt-cache search'

export GHC_INCLUDE_DIRS='.'

alias ghc='ghc -i$GHC_INCLUDE_DIRS'
alias ghci='ghci -i$GHC_INCLUDE_DIRS'
alias runghc='runghc -i$GHC_INCLUDE_DIRS'

alias cb='cabal build'
alias ci='cabal install'
alias cif='cabal install --force-reinstalls'
alias cid='cabal install --disable-executable-profiling --disable-library-profiling'
alias cie='cabal install --enable-executable-profiling --enable-library-profiling'
alias cu='cabal update'
alias cc='cabal clean'
alias ch='cabal haddock --hyperlink-source'
alias co='cabal configure'
alias cs='cabal sandbox init'

alias ga='git add'
alias gam='git commit --amend'
alias gb='git branch'
alias gc='git commit'
alias gcdr='git commit --author="danmo rozgu <danmo.rozgu@gmail.com>"'
alias gcl='git clone'
alias gcp='git cherry-pick'
alias gd='git diff --color'
alias gds='git diff --staged --color'
alias gg='git grep --color'
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

# alias st='svn status'
# alias sc='svn commit'
# alias sr='svn revert'
# alias sa='svn add'
# alias so='svn checkout'
# alias su='svn update'

alias epf='eproof -tAuto -xAuto --tptp3-format'
alias epr='eprover -tAuto -xAuto --tptp3-format'
alias spa='SPASS -Auto -TPTP -PGiven=0 -PProblem=0 -DocProof=0 -PStatistic=0'
alias vam='vampire_lin32 -mode casc'

alias xo='xclip -o'

alias ski='kibr lookup'
alias sis='kibr search'

alias sd='pwd | xclip -selection secondary'
alias ld='cd $(xclip -o -selection secondary)'

alias jd='jbo define'
alias jf='jbo filter'

js() {
    jbo filter $1 | xargs -0 jbo define
}

jl() {
    jbo filter $1 | xargs -0 jbo define | less
}

terminal() {
    urxvt -fn "xft:dejavu sans mono-$1:autohint=true" +sb
}

rlp() {
#        set -o LOCAL_OPTIONS -o ERR_RETURN
        file=$1
        shift
        cat "$file" | ssh danr@remote12.chalmers.se lp -d cse-ed-5473-laser1 $* -
                                                                #5102b-laser1
                                                                #5102b-color1
}

rlp2() {
#        set -o LOCAL_OPTIONS -o ERR_RETURN
        file=$1
        shift
        cat "$file" | ssh danr@remote12.chalmers.se lp -d cse-ed-5102b-color1 $* -
                                                                #5102b-laser1
                                                                #5102b-color1
}


export PYTHONPATH=$HOME/code/jbokorp/corpus_import/annotate/python:$PYTHONPATH
export SB_MODELS=$HOME/code/jbokorp/corpus_import/annotate/models
export CWB_DATADIR=$HOME/corpora/data
export CORPUS_REGISTRY=$HOME/corpora/registry


PERL_MB_OPT="--install_base \"/home/dan/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/dan/perl5"; export PERL_MM_OPT;

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
export FZF_DEFAULT_COMMAND="find * -name .git -prune -o -type f -print | grep '\.\(dyn_hi\|hi\|o\|dyn_o\)$' -v"
export FZF_DEFAULT_OPTS="-m --color=light -x"
