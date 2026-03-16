{ pkgs, config, ... }: {
  imports = [
  ];
  # This is one half of the glue between home-manager and nix-darwin.  The other
  # is including the home-manager module, as an import to the
  # `darwinConfiguration`.
  # It has to be done like this (and not via `imports`, or the wrong
  # `config.lib` will be provided - it won't be home-manager's.  This causes
  # things like symlinking to break the build.
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
    logan = {
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQOx2dxH8oP1406bie6eO3HB6fin4NY01laNiWRqcNsrRl6/M6e80wiTnG9u0Walb3JXegyqrHKIlFgvcrn2Tg/y944akJ/XqrcLPn3vwTcCV6XGI/1hPdcN0V156pbbnTS/T9y9btO+QJvELOjT4dET6HixBeBpGhLM95cirOrJjT2C6VVBYTGdAu3eKwCeDsjQtfKOHp9Huv0c1i57Fb13iTU1u0+L2o+LMYpS8YNbcBOgzx9FyyjvA/KuEVcyt2raVpbJv6nOP9ynz7a1Ja3Y2tgQwC6XCMpgKYHDYxaJhJbWjv9cxwq4zSzBr8yrlDKooqvpp9fTdOBAWF4R2MI2wb01yaaTlqPDcATBl5+Xu+SvxYf9wBt6wFIbv0baf1WtDDE7u9d2K/MJhShK9p45AQPTbmoYw7fzeMQOLdZNdZdXIOHWd17IJi2T+WnnO9hL1x+M5uZUlFlk0jGu0NP/YmHuWjGxxL7AIO1hH2q7ZHq7tzM+8sV6tjfGePwALFXSBBSGn2czgtfKzEVRFHBQajPco0g9zFWvi5ZfmU4QAkWOrQQFLEYK4IE0e1gR9Dsnqdm5tiYkCdVlapbG9jWdIBAgOCMj2bBXn+YObCrbVHW4wNo5OR6nec+b6miCuG23ue/o5j2L64kE16n1+hGx/Bbm0Adif4vw8zXVhAmxvQ== logan@scandium"
      ];
      home = "/Users/logan";
    }
      # nix-darwin isn't in perfect harmony with NixOS in terms of schema.
      # Conditionally add these if we're not on Darwin.
    // (if !pkgs.stdenv.isDarwin then {
      # Allow this user to sudo.
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
