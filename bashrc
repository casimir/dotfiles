
# Check for an interactive session
[ -z "$PS1" ] && return

if [ -f /etc/bash_completion ]; then
 . /etc/bash_completion
fi

export SB='kark burk bark koala'

RED='\033[0;31m'
NC='\033[0m' # No Color

onall() {
  for i in $SB; do
   echo -e "$RED$i$NC"
   ssh -A -T $i $@
  done
}

onpar() {
  for i in $SB; do echo $i; done |
  xargs -P 10 -I '{}' ssh -A -T {} "$@ |& while read line; do echo -e \"$RED\$(printf %7s {})$NC\" \"\$line\"; done"
}

export HISTCONTROL=ignorespace:ignoreboth
export HISTFILESIZE=4000000000
export HISTSIZE=1000000
HISTCONTROL=ignoredups:erasedups
shopt -s histappend
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

export XDG_CONFIG_HOME=$HOME/config
export GIT_EDITOR='kak'
export EDITOR='kak'
export PATH=$HOME/scripts:$HOME/bin:$HOME/.cabal/bin:$HOME/.local/bin:$PATH

export PS1='[$(date +%H:%M)|\w] \n\$ '

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

alias g='git commit -m'
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

alias xo='xclip -o'

alias sd='pwd | xclip -selection secondary'
alias ld='cd $(xclip -o -selection secondary)'
alias mt='mv -t "$(xclip -o -selection secondary)"; clear; ls'
alias wl='watch -n 0.1 -t -c ls --color=always'
alias wg='watch -n 0.1 -t -c git status -s'

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
export FZF_DEFAULT_COMMAND="rg --files"
export FZF_DEFAULT_OPTS="--height '60%' --reverse -m -e --color=16"

(cd $HOME
[ ! -f dircolors.ansi-dark ] && wget https://raw.githubusercontent.com/seebi/dircolors-solarized/master/dircolors.ansi-dark
eval $(dircolors dircolors.ansi-dark)
)

alias hl='highlight -O ansi'
alias lr='less -R'

# http://unix.stackexchange.com/questions/55203/bash-autocomplete-first-list-files-then-cycle-through-them
# http://unix.stackexchange.com/questions/205489/bash-ignore-case-but-disallow-autocomplete-if-ambiguous
bind 'set completion-ignore-case on'
bind 'TAB:menu-complete'
bind 'set show-all-if-ambiguous on'
bind 'set menu-complete-display-prefix on'
