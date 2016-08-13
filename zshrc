# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git, noreallyjustfuckingstopalready)

# User configuration

export PATH="$HOME/.opam/system/bin:/bin:/usr/local/bin:$HOME/bin:/opt/local/bin:/opt/local/sbin:$HOME/dev/adt-bundle-mac/sdk/platform-tools:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/usr/local/sbin:$HOME/node_modules/.bin"
# export MANPATH="/usr/local/man:$MANPATH"

# island of misfit paths
# $HOME/.rvm/gems/ruby-2.2.1/bin:
# $HOME/.rvm/gems/ruby-2.2.1@global/bin:
# $HOME/.rvm/rubies/ruby-2.2.1/bin:
# $HOME/.rvm/bin:

# ZSH_THEME=flazz
source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Vars
export NOTES_DIR="$HOME/notes"

alias grep='grep --color=auto'
alias b='bundle exec'
# run node in ES6 mode
alias node='node --harmony'
alias curl-json='curl -v -H "Accept: application/json"'


# completions
# eval "$(/usr/local/bin/gulp --completion=zsh)"

# omz placation
# export PATH="$HOME/.rvm/bin:$PATH" # Add RVM to PATH for scripting
# [[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" 


# functions

# Change 'regex' to '[r]egex' in grep to prevent grep from showing up in its own results when doing a `ps ... | grep ...`
grepp() {
        first_char=$(echo $1 | cut -c -1);
        left_bracket='[';
        right_bracket=']';
        first=$left_bracket$first_char$right_bracket;
        rest=$(echo $1 | cut -c 2-);
        args=$first$rest;
        grep $args
}

# Upload image to Imgur and return its URL. Get API key at http://imgur.com/register/api_anon
# imgur() { curl -F "image=@$1" -F "key=ANONYMOUS_IMGUR_API_KEY" https://api.imgur.com/2/upload | egrep -o "<original>.+?</original>" | egrep -o "http://imgur\.com/[^<]+" | sed "s/imgur.com/i.imgur.com/" | tee >(pbcopy); }

# IP addresses I don't want to memorize
export AWS_IRC=52.7.174.243
export AWS_ROID_MINER=52.2.198.173
export USER_AWS_IRC=ubuntu@$AWS_IRC   # I should probably just make 'logan' on that system
export USER_AWS_ROID_MINER=ubuntu@$AWS_ROID_MINER

# thefuck - `fuck` after a failed command and it will try to do the right thing
eval $(thefuck --alias)

# nvm
export NVM_DIR="$HOME/.nvm"
 . "$(brew --prefix nvm)/nvm.sh"

# vim mode for zsh
bindkey -v

function pwd_prompt() {
  echo "${PWD/$HOME/~}"
}

function path_color_prompt() {
  echo "%{$fg[cyan]%}"
}

function prompt_char() {
  echo "☃ "
}

################################################################################
# BEGIN VIM PROMPT SECTION
################################################################################

EDIT_MODE_PROMPT="%{$fg[green]%}✎ "
COMMAND_MODE_PROMPT="%{$fg[yellow]%}© "
VIM_MODE_PROMPT=$EDIT_MODE_PROMPT
function mode_change_prompt() {
  case $KEYMAP in
    vicmd) VIM_MODE_PROMPT=$COMMAND_MODE_PROMPT;; # command mode
    viins|main) VIM_MODE_PROMPT=$EDIT_MODE_PROMPT;; # insert mode
    *) VIM_MODE_PROMPT=$EDIT_MODE_PROMPT;; # insert mode
  esac
  set_prompt
  zle reset-prompt
}

function zle-line-init zle-keymap-select {
  mode_change_prompt
}


zle -N zle-line-init
zle -N zle-keymap-select

function vim_mode_prompt() {
  echo $VIM_MODE_PROMPT
}

# 10ms for key sequences - especially for vim mode switching
KEYTIMEOUT=1

################################################################################
# END VIM PROMPT SECTION
################################################################################

ZSH_THEME_GIT_PROMPT_PREFIX=" on %{$fg[magenta]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[green]%}!"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[green]%}?"
ZSH_THEME_GIT_PROMPT_CLEAN=""

function timestamp_prompt() {
  
}

EXIT_STATUS_VAR='$?'

function set_prompt() {
  PROMPT='$(path_color_prompt)$(pwd_prompt)$(git_prompt_info) $(echo $?) $(timestamp_prompt)
$(vim_mode_prompt)$reset_color'
}


set_prompt
