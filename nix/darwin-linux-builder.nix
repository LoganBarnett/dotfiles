{
  # cross-architecture-test-pkgs,
  # linux-builder-pkgs,
  # config,
  # lib,
  # pkgs,
  nixpkgs,
  lib,
  ...
}: let
  cross-architecture-test-pkgs = import nixpkgs {
    system = "x86_64-linux";
  };
  linux-builder-pkgs = import nixpkgs {
    system = "aarch64-linux";
  };
in {
  boot.binfmt.emulatedSystems = [ "x86_64-linux" ];
  environment.systemPackages = [
    linux-builder-pkgs.file
    cross-architecture-test-pkgs.hello
  ];
  # virtualisation.host.pkgs = lib.mkForce linux-builder-pkgs;
  # modules = [
  #   {
  #     virtualisation.host.pkgs = linux-builder-pkgs;
  #   }
  #   ./logan.nix
  #   ({ pkgs, config, ... }: {
  #     # cross compile to armv7l-hf-multiplatform
  #     # boot.kernelPackages = pkgs.linuxPackagesFor (pkgs.callPackage ./kernel.nix { });
  #     # nixpkgs.overlays = [ self.overlays.default ];
  #     # system.stateVersion = "23.05";
  #   })
  # ];
  nixpkgs.buildPlatform = { system = "aarch64-linux"; };
  # nixpkgs.hostPlatform = {
  #   system = "x86_64-linux";
  #   # I am not sure what this shold be in my case.
  #   # config = "armv7l-unknown-linux-gnueabihf";
  # };
  users.users = {
    logan = {
      # TODO: You can set an initial password for your user.
      # If you do, you can skip setting a root password by passing
      # '--no-root-passwd' to nixos-install.
      # Be sure to change it (using passwd) after rebooting!
      initialPassword = "correcthorsebatterystaple";
      isNormalUser = true;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQOx2dxH8oP1406bie6eO3HB6fin4NY01laNiWRqcNsrRl6/M6e80wiTnG9u0Walb3JXegyqrHKIlFgvcrn2Tg/y944akJ/XqrcLPn3vwTcCV6XGI/1hPdcN0V156pbbnTS/T9y9btO+QJvELOjT4dET6HixBeBpGhLM95cirOrJjT2C6VVBYTGdAu3eKwCeDsjQtfKOHp9Huv0c1i57Fb13iTU1u0+L2o+LMYpS8YNbcBOgzx9FyyjvA/KuEVcyt2raVpbJv6nOP9ynz7a1Ja3Y2tgQwC6XCMpgKYHDYxaJhJbWjv9cxwq4zSzBr8yrlDKooqvpp9fTdOBAWF4R2MI2wb01yaaTlqPDcATBl5+Xu+SvxYf9wBt6wFIbv0baf1WtDDE7u9d2K/MJhShK9p45AQPTbmoYw7fzeMQOLdZNdZdXIOHWd17IJi2T+WnnO9hL1x+M5uZUlFlk0jGu0NP/YmHuWjGxxL7AIO1hH2q7ZHq7tzM+8sV6tjfGePwALFXSBBSGn2czgtfKzEVRFHBQajPco0g9zFWvi5ZfmU4QAkWOrQQFLEYK4IE0e1gR9Dsnqdm5tiYkCdVlapbG9jWdIBAgOCMj2bBXn+YObCrbVHW4wNo5OR6nec+b6miCuG23ue/o5j2L64kE16n1+hGx/Bbm0Adif4vw8zXVhAmxvQ== logan@scandium"
      ];
      extraGroups = [
        # Allow this user to sudo.
        "wheel"
        "@admin"
      ];
    };
  };
  nix.settings = {
    trusted-users = [ "builder" "logan" "@admin" ];
    # "i686-linux"
    # extra-platforms = [ "aarch64-linux" "x86_64-linux" ];
    # system = "aarch64-linux";
  };
}
