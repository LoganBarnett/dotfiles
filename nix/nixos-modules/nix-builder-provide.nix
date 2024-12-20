################################################################################
# Make this host a builder for building Nix derivations.
################################################################################
{ ... }: let
  builder-user = "builder";
in {
  nix.settings = {
    trusted-users = [ builder-user ];
    # secret-key-files = [];
  };
  users.users = {
    # https://nixos.wiki/wiki/Distributed_build mentions using `useradd -m -L
    # builder` but -L isn't documented anywhere, which makes it hard to find
    # anything about this feature.  I can't find mention of it in this users
    # attrset either.
    "${builder-user}" = {
      isNormalUser = true;
      # Create a home so we can store SSH key info privately.
      createHome = true;
      openssh = {
        # authorizedKeysInHomeDir = true;
        authorizedKeys.keys = [
          (builtins.readFile ../secrets/builder-key.pub)
          # (builtins.readFile /etc/nix/builder_ed25519.pub)
        ];
      };
    };
  };
}
