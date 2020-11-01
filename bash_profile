# As charming as the nix installer seems to be, it neglects to mention you must
# add nix to your PATH. Perhaps that's obvious, but the path to use is not
# obvious. Taken from
# https://github.com/NixOS/nix/issues/1727#issuecomment-360355330
#
# Nix will flip out if it sees itself in the PATH, but it needs to be there.
# Chop up the string to defeat its detection. This allows us to do the install
# without having to remove it during a reinstallation and then re-add it.
export PATH="\
/nix/var/nix/profiles/default/bin:\
"$HOME/.nix""-profile/bin:\
${PATH}\
"


# oh java
JAVA_HOME="/Library/Java/Home"

export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$PATH:/Library/TeX/texbin/"

source ~/dev/dotfiles-private/bashrc-private
