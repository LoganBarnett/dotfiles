################################################################################
# Make this host a builder for building Nix derivations.
################################################################################
{ ... }: {
  nix.settings = {
    trusted-users = [
      "nixremote"
    ];
    # secret-key-files = [];
  };
  users.users = {
    # https://nixos.wiki/wiki/Distributed_build mentions using `useradd -m -L
    # nixremote` but -L isn't documented anywhere, which makes it hard to find
    # anything about this feature.  I can't find mention of it in this users
    # attrset either.
    nixremote = {
      isNormalUser = true;
      # Create a home so we can store SSH key info privately.
      createHome = true;
      openssh.authorizedKeys.keys = [
        (builtins.readFile ../secrets/builder-key.pub)
      ];
    };
  };
}
