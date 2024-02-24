##
# NixOS can be bootstraped in various ways but it requires running on  the
# target architecture and OS.  Both of these can be emulated but not presently
# on macOS.  So instead we fire up a VM that's running Nix.  The nix-darwin
# module provides a handy way to stand all of that up.  This is the
# configuration that goes into the `config' attribute for nix-darwin's
# linux-builder.  This VM is stateful, and requires both some bootstrapping and
# some manual invocation to operate.  Once up, it should present itself as a
# "builder" (a Nix entity) for the local Nix installation.
#
# To force changes to take effect:
# sudo launchctl kickstart -k system/org.nixos.linux-builder
#
# To get a fresh image:
# 1. Set linux-builder.enabled (in ./darwin.nix) to false.
# 2. Run `nix-darwin-switch` (my personal shortcut for nix-darwin).
# 3. Run `nix run nixpkgs#darwin.linux-builder'.
# 4. Verify the VM starts up correctly.
# 5. Run `shutdown now` in the VM.
# 6. Set linux-builder.enabled (in ./darwin.nix) to true.
# 7. Run `nix-darwin-switch` (my personal shortcut for nix-darwin).
# 8. Observe the new instance running.
#
# *Getting to run as a builder*
#
# I'm still working on this.  I can see the builder with `nix store ping --store
# ssh-ng://linux-builder`, but it does not trust my host.
{
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
  boot.binfmt.emulatedSystems = [ "i686-linux" "x86_64-linux" ];
  environment.systemPackages = [
    linux-builder-pkgs.file
    cross-architecture-test-pkgs.hello
  ];
  nixpkgs.buildPlatform = { system = "aarch64-linux"; };
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
      ];
    };
  };
  nix.settings = {
    extra-platforms = [ "aarch64-linux" "i686-linux" "x86_64-linux" ];
  };
}
