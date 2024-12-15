##
# NixOS can be bootstraped in various ways but it requires running on the target
# architecture and OS.  Both of these can be emulated but not presently on
# macOS.  So instead we fire up a VM that's running Nix.  The nix-darwin module
# provides a handy way to stand all of that up.  This is the configuration that
# goes into the `config' attribute for nix-darwin's linux-builder.  This VM is
# stateful, and requires both some bootstrapping and some manual invocation to
# operate.  Once up, it should present itself as a "builder" (a Nix entity) for
# the local Nix installation.
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
  flake-inputs,
  lib,
  nixpkgs,
  # pkgs,
  ...
}: let
  system = "aarch64-linux";
  # linux-builder-pkgs = import nixpkgs {
  #   system = "aarch64-linux";
  #   # This seems to cause the build to be x86_64 which I don't think we want, or
  #   # at least don't want as this primary packages for this system.  It needs to
  #   # be broken out into a separate import nixpkgs expression.
  #   # crossSystem = {
  #   #   # config = "x86_64-linux";
  #   #   config = "x86_64-unknown-linux-gnu";
  #   #   # config = {
  #   #   #   hostPlatform = "aarch64-unknown-linux-gnu";
  #   #   #   buildPlatform = "x86_64-unknown-linux-gnu";
  #   #   #   targetPlatform = "x86_64-unknown-linux-gnu";
  #   #   # };
  #   #   hostPlatform = "aarch64-unknown-linux-gnu";
  #   #   buildPlatform = "x86_64-unknown-linux-gnu";
  #   #   targetPlatform = "x86_64-unknown-linux-gnu";
  #   #   # stdenv = linux-builder-pkgs.stdenv.override {
  #   #   #   hostPlatform = "aarch64-unknown-linux-gnu";
  #   #   #   buildPlatform = "x86_64-unknown-linux-gnu";
  #   #   #   targetPlatform = "x86_64-unknown-linux-gnu";
  #   #   # };
  #   # };

  #   # hostPlatform = "aarch64-linux";
  #   # buildPlatform = "x86_64-linux";
  #   # targetPlatform = "x86_64-linux";
  # };
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
  # cross-architecture-test-pkgs = import nixpkgs {
  #   system = "x86_64-linux";
  #   # crossSystem = {
  #   #   config = "x86_64-unknown-linux-gnu";
  #   #   hostPlatform = "aarch64-unknown-linux-gnu";
  #   #   buildPlatform = "x86_64-unknown-linux-gnu";
  #   #   targetPlatform = "x86_64-unknown-linux-gnu";
  #   # };
  #   # stdenv = linux-builder-pkgs.stdenv.override {
  #   #   hostPlatform = "aarch64-unknown-linux-gnu";
  #   #   buildPlatform = "x86_64-unknown-linux-gnu";
  #   #   targetPlatform = "x86_64-unknown-linux-gnu";
  #   # };
  # };
  # cross-architecture-test-pkgs = import nixpkgs {
  #   system = "aarch64-linux";
  #   crossSystem = {
  #     # config = "x86_64-linux";
  #     config = "x86_64-unknown-linux-gnu";
  #   };
  # };
in {
  imports = [
    # ./users/logan-server.nix
    # (import ./nixos-modules/user-can-admin.nix {
    #   inherit flake-inputs;
    #   inherit system;
    # })
  ];
  # boot.binfmt.emulatedSystems = [
  #   # "i686-linux"
  #   # "x86_64-linux"
  #   # To get the Raspberry Pi building from linux-builder.  Only two of them
  #   # work here.
  #   # "armv7a-darwin"
  #   # "armv5tel-linux"
  #   "armv6l-linux"
  #   # "armv7a-linux"
  #   "armv7l-linux"
  #   # "armv6l-netbsd"
  #   # "armv7a-netbsd"
  #   # "armv7l-netbsd"
  #   # "arm-none"
  #   # "armv6l-none"
  # ];
  # environment.systemPackages = [
  #   pkgs.lsof
  #   # Allow us to do a "bootstrapped" remote deploy - see bin/remote-deploy for
  #   # details on why this is needed.
  #   linux-builder-pkgs.git
  #   # Gives us a utility for inspecting binary meta-data.
  #   linux-builder-pkgs.file
  #   linux-builder-pkgs.gdb
  #   linux-builder-pkgs.gnu-config
  #   # A test to show we can run x86_64-linux programs on an aarch64-linux
  #   # system.
  #   linux-builder-pkgs.pkgsCross.gnu64.hello
  #   # This provides us readelf, which is useful for getting extended information
  #   # from ELF binaries.
  #   linux-builder-pkgs.binutils-unwrapped
  #   # Ensure I can remote-deploy to this host.
  #   linux-builder-pkgs.rsync
  #   # cross-architecture-test-pkgs.hello
  # ];
  # nixpkgs = {
  #   buildPlatform = { system = "aarch64-linux"; };
  # };
  # Shows us what package an executable resides in when attempting to run an
  # missing command.  This is enabled by default but doesn't seem to work.  See
  # https://discourse.nixos.org/t/command-not-found-unable-to-open-database/3807/4
  # for some troubleshooting ideas but might not work since this is a Flake.  I
  # will use nix-index instead, and nix-index is mutually exclusive to
  # command-not-found, so I'm disabling command-not-found.
  # programs.command-not-found.enable = false;
  # Of course, this doesn't work because we don't have a nix-channel.
  # programs.nix-index = {
  #   enable = true;
  #   enableBashIntegration = true;
  # };
  # nix.settings = {
  #   extra-platforms = [
  #     # This might show up or not show up in an error list, helping us
  #     # identify what list is being shown.
  #     "this-is-darwin-linux-builders-list"
  #     "aarch64-unknown-linux-gnu"
  #     "aarch64-linux"
  #     "i686-linux"
  #     "x86_64-linux"
  #     "armv5tel-linux"
  #     "armv6l-linux"
  #     "armv7a-linux"
  #     "armv7l-linux"
  #     "armv6l-netbsd"
  #     "armv7a-netbsd"
  #     "armv7l-netbsd"
  #     "arm-none"
  #     "armv6l-none"
  #   ];
  #   # extra-platforms = [ "aarch64-linux" ];
  # };

  services.openssh.enable = true;
  # This keeps breaking builds.  Just disable it.
  systemd.oomd.enable = false;
  # See https://nixos.org/manual/nixpkgs/stable/#sec-darwin-builder for
  # information about configuration values here.
  virtualisation = {
    # Sizes here are in MB.
    darwin-builder = {
      # There might be disk space issues at play with the whole "vmlinux" BPF
      # issue seen here:
      # https://discourse.nixos.org/t/cannot-build-arm-linux-kernel-on-an-actual-arm-device/54218
      diskSize = lib.mkForce (100 * 1024);
      # Defaults to 3 GB.  Maybe with 8 we can build the Linux kernel...
      memorySize = lib.mkForce (8 * 1024);
    };
    cores = 6;
  };
}
