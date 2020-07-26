# -*- mode: sh -*-

# prevent emacs tramp from barfing on startup
[[ "$TERM" == "dumb" ]] &&
  echo "[ZSHRC] Setting as dumb terminal (likely for Tramp)..." &&
  unsetopt zle &&
  unsetopt prompt_cr &&
  unsetopt prompt_subst &&
  # unfunction precmd &&
  # unfunction preexec &&
  PS1='$ ' &&
  # gls respects the --dired flag, which dired + tramp uses.
  alias ls='gls' &&
  return

setopt prompt_subst

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.

# TODO: add osx conditionally
plugins=(
    brew
    command-not-found
    git
    node
    noreallyjustfuckingstopalready
    npm
    osx
    rsync
    yarn
    zsh-git-prompt
)

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

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# island of misfit paths
# $HOME/.rvm/gems/ruby-2.2.1/bin:
# $HOME/.rvm/gems/ruby-2.2.1@global/bin:
# $HOME/.rvm/rubies/ruby-2.2.1/bin:
# $HOME/.rvm/bin:

# ZSH_THEME=flazz
source $ZSH/oh-my-zsh.sh

source $ZSH/plugins/osx/osx.plugin.zsh

# Yanking args is amazing. In command mode use _. I think using a number will
# grab a specific one - like 4_ to grab the 4th.
vi-yank-arg() {
  NUMERIC=1 zle .vi-add-next
  zle .insert-last-word
}
zle -N vi-yank-arg
bindkey -M vicmd _ vi-yank-arg

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

# AUTO_CD means if you type a directory name, it's the same as doing `pushd
# dirname`. I guess it's nice for some but odd and doesn't sit well with me in
# terms of least surprising operations.
unsetopt AUTO_CD

# This is the problem.
# source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Vars
export NOTES_DIR="$HOME/notes"

alias ls='ls -aG'
alias grep='grep --color=auto'
alias b='bundle exec'
# run node in ES6 mode
alias node='node --harmony'
alias curl-json='curl -v -H "Accept: application/json"'

# git
alias gs="git status"
alias gfp="git push --force-with-lease"
alias grc="git rebase --continue"
alias gro="git restore --ours"
alias grt="git restore --theirs"

# yarn flow management because it happens a lot.
alias yf="yarn flow"
alias yfs="yarn flow stop"
alias yfr="yfs && yf"

# completions

# omz placation
# export PATH="$HOME/.rvm/bin:$PATH" # Add RVM to PATH for scripting
# [[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" 


# functions

# Change 'regex' to '[r]egex' in grep to prevent grep from showing up in its own
# results when doing a `ps ... | grep ...`
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

# oh java
# JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.8.0_102.jdk/Contents/Home"
# JAVA_HOME="/Library/Java/Home"
export JAVA_HOME=$(/usr/libexec/java_home)

# thefuck - `fuck` after a failed command and it will try to do the right thing
eval $(thefuck --alias)

# When we're running ansi-term from emacs, we don't want the evil-mode bindings
# and zsh's vim bindings stumbling over each other. See
# https://github.com/syl20bnr/spacemacs/issues/7140#issuecomment-252036519 for
# more config examples.
if [ $EMACS ]; then
  bindkey -e
else
  # vim mode for zsh
  bindkey -v
  # Bundles that break emacs/term.el
  # Syntax highlighting bundle.
  plugins=$plugins"zsh-users/zsh-syntax-highlighting"
fi

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
  if [ $EMACS ]; then
    VIM_MODE_PROMPT="$"
  else
    case $KEYMAP in
      vicmd) VIM_MODE_PROMPT=$COMMAND_MODE_PROMPT;; # command mode
      viins|main) VIM_MODE_PROMPT=$EDIT_MODE_PROMPT;; # insert mode
      *) VIM_MODE_PROMPT=$EDIT_MODE_PROMPT;; # insert mode
    esac
  fi
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

# This should make the prompt calculation really zippy. Haskell functionality is
# borken though, and always thinks that we are MERGING...
# GIT_PROMPT_EXECUTABLE="haskell"
ZSH_THEME_GIT_PROMPT_PREFIX=" on %{$fg[magenta]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[green]%}!%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[green]%}?%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="✓%{$reset_color%}"

function timestamp_prompt() {
  echo "%{$fg[blue]%}\
[$reset_color\
%D{$fg[green]%H$fg[yellow]:$fg[green]%M$fg[yellow]:$fg[green]%S}\
%{$fg[blue]%}]\
$reset_color"
}

function exit_status_prompt() {
   echo "%?"
}

function host_prompt() {
  echo "%{$fg[yellow]%}%n@%M$reset_color"
}

autoload -U add-zsh-hook

function set_prompt() {
  # Line break intentional. Sometimes the last line of stdout would be cut off
  # without it.
  # This was using PROMPT before. Why?
#   PS1='$(path_color_prompt)$(pwd_prompt)$(git_super_status) $(host_prompt) $(exit_status_prompt) $(timestamp_prompt) $(timer_prompt)
# $(vim_mode_prompt)'
  PS1='$(vim_mode_prompt)'
}

# Print an empty line before the prompt. The prompt is still jiggly with long
# prompts but at least it's working now.
# function precmd() { print "" }

# We can just print before we print the prompt. Otherwise zsh does a weird thing
# where it deletes the prior line when you do certain interactions (such as
# pressing escape).
#
# See: https://superuser.com/a/395784
function print_prompt() {
  print -rP '$(path_color_prompt)$(pwd_prompt)$(git_super_status) $(host_prompt) $(exit_status_prompt) $(timestamp_prompt) $(timer_prompt)'
}

function fix_long_git_branch() {
  # Intentionally using unicode to save space.
  (( ${#GIT_BRANCH} > 14)) && GIT_BRANCH="${GIT_BRANCH:0:13}…"
}

add-zsh-hook precmd fix_long_git_branch
add-zsh-hook precmd print_prompt

# zsh redefined function
function preexec() {
  timer=${timer:-$SECONDS}
}

# timer stuff found here
# https://coderwall.com/p/kmchbw/zsh-display-commands-runtime-in-prompt
# zsh hook command
function precmd() {
  if [ $timer ]; then
    timer_show=$(($SECONDS - $timer))
    unset timer
  fi
  # Print an empty line before the prompt. The prompt is still jiggly with long
  # prompts but at least it's working now.
  print ""
}

function timer_prompt() {
  if [ $timer_show ]; then
    echo $timer_show"s"
  fi
}

# cause the prompt to repaint so we can see the current time
# TMOUT=1
# TRAPALRM() {
#   if [ "$WIDGET" != "complete-word" ]; then
#     zle reset-prompt
#   fi
# }


set_prompt

export PATH="$HOME/.opam/system/bin:\
/bin:/usr/local/bin:$HOME/bin:\
$HOME/dev/dotfiles-private/bin:\
/opt/local/bin:\
/opt/local/sbin:\
$HOME/dev/adt-bundle-mac/sdk/platform-tools:\
$HOME/.doom-emacs.d/bin:\
/Library/TeX/texbin:\
/usr/local/bin:\
/usr/bin:\
/bin:\
/usr/sbin:\
/sbin:\
/opt/X11/bin:\
/usr/local/sbin:\
$HOME/node_modules/.bin\
"

# Puppet installs using its own pattern.
# See https://puppet.com/docs/pdk/1.x/pdk_troubleshooting.html#pdk-not-in-zshell-path-on-mac-os-x
# This is supposedly needed for zsh, but also mangles the PATH such that rbenv
# no longer works. I assume it also means other shim-installing things won't
# work. Let's run this before the _all_ of the shim installers so it doesn't
# interfere.
eval $(/usr/libexec/path_helper -s)


export NOTES_DIR=~/notes

# Should take care of some prompt issues.
export LC_ALL=en_US.UTF-8

# add Go support
export GOPATH=$HOME/golang
export GOROOT=/usr/local/opt/go/libexec
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:$GOROOT/bin
# This really should be included in the homebrew jenv caveats list:
# https://github.com/gcuisinier/jenv/wiki/Trouble-Shooting
export JENV_ROOT=/usr/local/opt/jenv

export PATH="$JENV_ROOT:$PATH"
eval "$(jenv init -)"

# rbenv for Ruby
eval "$(rbenv init -)"

# Bootstrap Nix.
. ~/.nix-profile/etc/profile.d/nix.sh

# nodenv requires a quick init
eval "$(nodenv init -)"

# cargo also requires an init
source $HOME/.cargo/env

# nodenv's build-update-defs plugin needs an environment variable set.
export NODE_BUILD_DEFINITIONS=$(brew --prefix node-build-update-defs)/share/node-build

# Get ocaml's package manager on the PATH and other ocaml config.
eval $(opam config env)

export PATH="/usr/local/opt/gnupg@2.1/bin:$PATH"

# GNU commands are generally way better.
export PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"

# This allows commands to be excluded from history if they are prefixed with a
# space.
export HISTCONTROL=ignorespace

# Pull in private stuff.
source ~/dev/dotfiles-private/zshrc-private
