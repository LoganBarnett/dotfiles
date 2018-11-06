
export PATH="$HOME/.opam/system/bin:/bin:/usr/local/bin:$HOME/bin:/opt/local/bin:/opt/local/sbin:$HOME/dev/adt-bundle-mac/sdk/platform-tools:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/usr/local/sbin:$HOME/node_modules/.bin"

export NOTES_DIR=~/Dropbox/notes

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

# This allows commands to be excluded from history if they are prefixed with a
# space.
export HISTCONTROL=ignorespace
