# I added percol as an overlay because 1: it's kind of useful and 2: it's fairly
# simple as far as Python packages go. It uses setup.py. I managed to find a
# decent working sample in this location:
# https://github.com/NixOS/nixpkgs/blob/8284fc30c84ea47e63209d1a892aca1dfcd6bdf3/pkgs/applications/networking/instant-messengers/salut-a-toi/default.nix
# And a more complicated one here:
# https://github.com/NixOS/nixpkgs/blob/8284fc30c84ea47e63209d1a892aca1dfcd6bdf3/pkgs/servers/tautulli/default.nix
#
# This is a working Python application constructed in an overlay. In the
# consuming nix file it is simply presented as "pkgs.percol". I might like to
# keep this around as an example.
final: prev: {
  percol = prev.callPackage ../derivations/percol.nix {};
}
