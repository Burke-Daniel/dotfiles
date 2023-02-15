# Completion
autoload -U compinit; compinit
_comp_options+=(globdots)

# Prompt
autoload -Uz vcs_info
precmd() { vcs_info }
zstyle ':vcs_info:git:*' formats '%b '
setopt PROMPT_SUBST
PROMPT='%F{green}%n@%m%f %F{blue}%~%f %F{red}${vcs_info_msg_0_}%f$ '

# Config management
alias config='/usr/bin/git --git-dir=/home/dburke/.cfg/ --work-tree=/home/dburke'

# Syntax highlighting
source /home/dburke/.bin/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

