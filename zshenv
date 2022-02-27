# -*- mode: sh -*-

# export GEM_PATH="~/.gem/ruby/2.6.0:$GEM_PATH"
export PATH="$PATH:/Library/TeX/texbin/"

# Force git to use ssh with IPv4. When tethered over my phone, everything is
# IPv6. While dig, nslookup, and ping all agree on the IP used by a source like
# github.com, this doesn't seem to be the case with ssh. Switching to IPv4 fixes
# this issue. It pains me that the fix for IPv6 related issues are generally
# "stop using IPv6", but I know of no other course to pursue here.
#
# One thread I can pull on is to attempt to use a recent version of ssh rather
# than the one bundled on macOS, which is probably tailing behind. While the ssh
# program has been a stable workhorse for as long as I can remember, it is not
# immune to bugs.
export GIT_SSH_COMMAND="ssh -4"
. "$HOME/.cargo/env"
