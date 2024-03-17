################################################################################
# Gives us a ComfyUI server.  See https://github.com/comfyanonymous/ComfyUI for
# specifics on the server.
#
# This leverages work from https://github.com/NixOS/nixpkgs/pull/268378 by
# @fazo96.  As such it will be using my fork of fazo96's fork.
################################################################################
{ lib, pkgs, ... }: {
  # The firewall is enabled by default, per:
  # https://nixos.org/manual/nixos/unstable/index.html#sec-firewall
  networking.firewall.allowedTCPPortRanges = [
    # Default ComfyUI port.
    { from = 8188; to = 8188; }
  ];
  services.comfyui = {
    enable = true;
    dataPath = "/var/lib/comfyui";
    # Leaving this as the default seems to be broken with this error:
    # error: attribute 'cudaSupport' missing
    package = pkgs.comfyui-cuda;
  };
}
