# As charming as the nix installer seems to be, it neglects to mention you must
# add nix to your PATH. Perhaps that's obvious, but the path to use is not
# obvious. Taken from
# https://github.com/NixOS/nix/issues/1727#issuecomment-360355330
export PATH="\
/nix/var/nix/profiles/default/bin:\
$HOME/.nix-profile/bin:\
${PATH}\
"


# oh java
JAVA_HOME="/Library/Java/Home"

export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$PATH:/Library/TeX/texbin/"

source ~/dev/dotfiles-private/bashrc-private
