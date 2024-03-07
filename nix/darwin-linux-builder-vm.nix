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
  linux-builder-pkgs = import nixpkgs {
    system = "aarch64-linux";
    crossSystem = {
      # config = "x86_64-linux";
      config = "x86_64-unknown-linux-gnu";
      # config = {
      #   hostPlatform = "aarch64-unknown-linux-gnu";
      #   buildPlatform = "x86_64-unknown-linux-gnu";
      #   targetPlatform = "x86_64-unknown-linux-gnu";
      # };
      hostPlatform = "aarch64-unknown-linux-gnu";
      buildPlatform = "x86_64-unknown-linux-gnu";
      targetPlatform = "x86_64-unknown-linux-gnu";
      # stdenv = linux-builder-pkgs.stdenv.override {
      #   hostPlatform = "aarch64-unknown-linux-gnu";
      #   buildPlatform = "x86_64-unknown-linux-gnu";
      #   targetPlatform = "x86_64-unknown-linux-gnu";
      # };
    };

    # hostPlatform = "aarch64-linux";
    # buildPlatform = "x86_64-linux";
    # targetPlatform = "x86_64-linux";
  };
  # pkgs = import <nixpkgs> { };
  # pkgsCross = import <nixpkgs> {
  #   crossSystem = { config = "x86_64-unknown-linux-gnu"; };
  # };
  # localSystem = pkgs.lib.systems.elaborate builtins.currentSystem;
  # crossSystem = pkgs.lib.systems.elaborate {
  #   config = "aarch64-unknown-linux-gnu";
  # };
  # crGcc10 = pkgs.wrapCCWith {
  #   cc = pkgs.gcc10.cc.override {
  #     libcCross = pkgsCross.libcCross;
  #     stdenv = pkgs.stdenv.override {
  #       hostPlatform = localSystem;
  #       buildPlatform = localSystem;
  #       targetPlatform = crossSystem;
  #     };
  #   };
  # };
  cross-architecture-test-pkgs = import nixpkgs {
    system = "x86_64-linux";
    crossSystem = {
      config = "x86_64-unknown-linux-gnu";
      hostPlatform = "aarch64-unknown-linux-gnu";
      buildPlatform = "x86_64-unknown-linux-gnu";
      targetPlatform = "x86_64-unknown-linux-gnu";
    };
    # stdenv = linux-builder-pkgs.stdenv.override {
    #   hostPlatform = "aarch64-unknown-linux-gnu";
    #   buildPlatform = "x86_64-unknown-linux-gnu";
    #   targetPlatform = "x86_64-unknown-linux-gnu";
    # };
  };
  # cross-architecture-test-pkgs = import nixpkgs {
  #   system = "aarch64-linux";
  #   crossSystem = {
  #     # config = "x86_64-linux";
  #     config = "x86_64-unknown-linux-gnu";
  #   };
  # };
in {
  boot.binfmt.emulatedSystems = [ "i686-linux" "x86_64-linux" ];
  environment.systemPackages = [
    # Gives us a utility for inspecting binary meta-data.
    linux-builder-pkgs.file
    linux-builder-pkgs.gdb
    linux-builder-pkgs.gnu-config
    # A test to show we can run x86_64-linux programs on an aarch64-linux
    # system.
    linux-builder-pkgs.pkgsCross.gnu64.hello
    # This provides us readelf, which is useful for getting extended information
    # from ELF binaries.
    linux-builder-pkgs.binutils-unwrapped
    # cross-architecture-test-pkgs.hello
  ];
  nixpkgs = {
    buildPlatform = { system = "aarch64-linux"; };
  };
  # Shows us what package an executable resides in when attempting to run an
  # missing command.  This is enabled by default but doesn't seem to work.  See
  # https://discourse.nixos.org/t/command-not-found-unable-to-open-database/3807/4
  # for some troubleshooting ideas but might not work since this is a Flake.  I
  # will use nix-index instead, and nix-index is mutually exclusive to
  # command-not-found, so I'm disabling command-not-found.
  programs.command-not-found.enable = false;
  # Of course, this doesn't work because we don't have a nix-channel.
  programs.nix-index = {
    enable = true;
    enableBashIntegration = true;
  };
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
