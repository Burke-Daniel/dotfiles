source "$XDG_CONFIG_HOME/zsh/aliases"
autoload -Uz compinit; compinit

_comp_options+=(globdots)
source ~/dotfiles/zsh/external/completion.zsh
fpath=($ZDOTDIR/external ~/bin/zsh/completions/src $fpath)

source ~/dotfiles/zsh/external/prompt_purification_setup
autoload -Uz prompt_purification_setup; prompt_purification_setup

setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS
setopt PUSHD_SILENT

alias d='dirs -v'
for index ({1..9}) alias "$index"="cd +${index}"; unset index

bindkey -v
export KEYTIMEOUT=1

source ~/dotfiles/zsh/external/cursor_mode
autoload -Uz cursor_mode; cursor_mode

zmodload zsh/complist
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

source ~/dotfiles/zsh/external/bd.zsh
source $DOTFILES/zsh/scripts.sh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

bindkey -r '^l'
bindkey -r '^g'
bindkey -s '^g' 'clear\n'

source /home/burke/bin/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

