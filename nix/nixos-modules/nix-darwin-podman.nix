################################################################################
# Provide a podman service for nix-darwin.
#
# Largely this is taken from here: https://rossabaker.com/configs/podman/
################################################################################
{ config, lib, pkgs, ... }: let
  # Instead of trying to do conditional imports and the like (we don't want to
  # break NixOS, which has a working podman configuration), just add a prefix
  # that better states our intent.
  cfg = config.nix-darwin-custom.services.podman;
  # Work around https://github.com/containers/podman/issues/17026
  # by downgrading to qemu-8.1.3.
  inherit (import (pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "4db6d0ab3a62ea7149386a40eb23d1bd4f508e6e";
    sha256 = "sha256-kyw7744auSe+BdkLwFGyGbOHqxdE3p2hO6cw7KRLflw=";
  }) { inherit (pkgs) system; }) qemu;
in {

  options.nix-darwin-custom.services.podman = {
    enabled = lib.mkEnableOption "Podman" // {
      default = false;
    };
  };

  config = lib.mkIf cfg.enabled {
    environment.systemPackages = [
      pkgs.podman
      qemu
      # Why is xz needed?
      pkgs.xz
    ];
    # https://github.com/containers/podman/issues/17026
    environment.pathsToLink = [ "/share/qemu" ];

    # https://github.com/LnL7/nix-darwin/issues/432#issuecomment-1024951660
    environment.etc."containers/containers.conf.d/99-gvproxy-path.conf".text = ''
      [engine]
      helper_binaries_dir = ["${pkgs.gvproxy}/bin"]
    '';
    # Fix issue where Podman can't resolve any container hosts.
    environment.etc."containers/registries.conf".text = ''
      [registries.search]
      registries = ['docker.io', 'registry.fedoraproject.org', 'quay.io', 'registry.access.redhat.com', 'registry.centos.org']

      [registries.insecure]
      registries = []

      [registries.block]
      registries = []
    '';
  };

}
