# TODO: Remove this file, but make sure it doesn't have anything helpful in it
# first.
#
# I want to call this defaults.nix, but the ergonomics of Agenix demand a
# "secrets.nix" name.
#
# https://github.com/ryantm/agenix?tab=readme-ov-file has a tutorial on setting
# up Agenix.  The secrets created here are considered to be reasonably secure
# against offline, brute force attacks.  Otherwise we'd have trouble with
# communicating securely over SSH, but having some harder evidence (like entropy
# levels, and how much compute it would take to break) would be preferable than
# my supposition.
#
# https://security.stackexchange.com/a/257678 has a good summary of various
# keys, and a sorted preferential list of ciphers for the laymen that just wants
# to get the best-in-slot cipher and move on.  At time of writing (2024-03-31),
# that cipher is ed25519, which `ssh-keygen` (from Nix) defaults to.
let
  # How to make a new key, as Nix-y as possible:
  # 1. LOL make it as a secret!
  lithium = "";
  scandium = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN3jJteck5yCfIm0iA4qKSIVx9zh6qhCuAt5cV1Ysib+";
  systems = [
    lithium
    scandium
  ];
  deployers = [
    lithium
    scandium
  ];
in {
  # lithium-age-ssh-private = {
  #   # name = "lithium-age-ssh-private.age";
  #   rekeyFile = ./lithium-age-ssh-private.age;
  #   # publicKeys = deployers;
  # };
  # "civitai-token.age" = {
  #   name = "civitai-token.age";
  #   rekeyFile = ./civitai-token.age;
  #   # publicKeys = [ lithium ];
  # };
  # How to make a new secret (substitute $secret as needed):
  # 1. Create an attribute here with `"$secret.age".publicKeys = [];`.
  # 2. Populate the `publicKeys` list with the public keys for the systems where
  #    the secret is to be used.
  # 3. `pushd ~/dev/dotfiles/nix/secrets` - yes you have to be in this directory
  #     per: https://github.com/ryantm/agenix/issues/149
  # 4. Run `RULES=$PWD/default.nix agenix -e $secret.age`.
  # 5. Paste the secret.  You might have to use `:set paste` in vim, but since
  #    it's text you should be fine without it.
  # 6. <find out how to use the secret>
}
