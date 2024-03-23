################################################################################
# Gives us a ComfyUI server.  See https://github.com/comfyanonymous/ComfyUI for
# specifics on the server.
#
# This leverages work from https://github.com/NixOS/nixpkgs/pull/268378 by
# @fazo96.  As such it will be using my fork of fazo96's fork.
################################################################################
{ lib, pkgs, ... }: let
  # Default ComfyUI port.
  port = 8188;
  fetchModel = pkgs.callPackage ../hacks/comfyui/fetch-model.nix {};
  # fetchModel = import ../hacks/comfyui/fetch-model.nix;
in {
  # We need to override the original if we want to provide our own for rapid
  # iteration.  First we disable the original via `disabledModules`, and then
  # inject our own version via `imports`.
  disabledModules = [ "services/web-apps/comfyui.nix" ];
  imports = [ ../hacks/comfyui-services-web-apps/comfyui.nix ];
  # The firewall is enabled by default, per:
  # https://nixos.org/manual/nixos/unstable/index.html#sec-firewall
  networking.firewall.allowedTCPPorts = [ port ];
  networking.firewall.allowedUDPPorts = [ port ];
  nixpkgs.overlays = [
    (final: prev: {
      # See https://github.com/NixOS/nixpkgs/issues/280621 for the issue report
      # and the commit that introduced the issue.
      astc-encoder = prev.asct-encoder.overrideAttrs (prev-pkg: rec {
        version = "4.6.1";
        src = prev.fetchFromGitHub {
          owner = "ARM-software";
          repo = "astc-encoder";
          rev = version;
          sha256 = "sha256-7/GBzqgXh8sU3Pl30eH9Mi24PPIqayQqNuhkMnn5Lq0=";
        };
      });
      opencv = prev.opencv.overrideAttrs (prev-pkg: {
        cmakeFlags = [
          ''-DPYTHON_LIBRARY=${prev.python3}/lib''
          ''-DPYTHON_INCLUDE_DIR=${prev.python3}/include''
          ''-DPYTHON_EXECUTABLE=${prev.python3}/bin/python3''
        ] ++ prev-pkg.cmakeFlags;
      });
      pythonPackagesExtensions = [(py-final: py-prev: {
        torch = py-final.torch-bin;
      })];
    })
  ];
  services.comfyui = {
    enable = true;
    extraArgs =
      # Listen on 0.0.0.0, otherwise it's localhost connections only.
      "--listen"
    ;
    dataPath = "/var/lib/comfyui";
    # Leaving this as the default seems to be broken with this error:
    # error: attribute 'cudaSupport' missing
    # package = pkgs.comfyui-cpu;
    package = pkgs.callPackage ../hacks/comfyui/package.nix {};
    models = {
      checkpoints = [
        (fetchModel {
          name = "pony-xl-v6";
          url = "https://civitai.com/api/download/models/290640?type=Model&format=SafeTensor&size=pruned&fp=fp16";
          sha256 = "1cxh5450k3y9mkrf9dby7hbaydj3ymjwq5fvzsrqk6j3xkc2zav7";
        })
      ];
    };
    # package = pkgs.comfyui;
  };
}

# Next up, try this: eb9b60a from
# https://github.com/r-ryantm/nixpkgs/commit/eb9b60a2de7e9f406efed12f68c2024d5955b35e
# https://github.com/NixOS/nixpkgs/pull/293555/files
# Original invocation:
# nix build --impure -L nixpkgs/383e4b2b#cudaPackages.nccl
# Adapted invocation:
# nix build --impure -L nixpkgs/eb9b60a#cudaPackages.nccl
