# User must be explicitly set per
# https://github.com/nix-community/home-manager/issues/4026
{ pkgs, ... }: {
  imports = [
    ./home.nix
  ];
  # This is one half of the glue between home-manager and nix-darwin.  The other
  # is including the home-manager module, as an import to the
  # `darwinConfiguration`.
  home-manager.users.logan = import ../home.nix;
  # TODO: Make this configurable such that my client machines require
  # passwords but service machines do not.
  security.${
    if !pkgs.stdenv.isDarwin then "sudo.wheelNeedsPassword" else null
  } = true;
  # Left as reference if you want fine-grained settings:
  # security.sudo.extraRules = [
  #   {
  #     users = [ "logan" ];
  #     options = [ "NOPASSWD" ];
  #   }
  # ];
  # Must be explicitly set per
  # https://github.com/nix-community/home-manager/issues/4026
  users.users = {
    logan.barnett = {
      openssh.authorizedKeys.keys = [
        # TODO: Fix
        "ssh-rsa deadc0de"
      ];
      home = "/Users/logan.barnett";
    }
      # nix-darwin isn't in perfect harmony with NixOS in terms of schema.
      # Conditionally add these if we're not on Darwin.
    // (if !pkgs.stdenv.isDarwin then {
      # Allow this user to sudo.  It won't stick because Reasons.
      extraGroups = [ "wheel" ];
      # TODO: You can set an initial password for your user.
      # If you do, you can skip setting a root password by passing
      # '--no-root-passwd' to nixos-install.
      # Be sure to change it (using passwd) after rebooting!
      initialPassword = "correcthorsebatterystaple";
      isNormalUser = true;
    } else {});
  };
}
