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
          # It's critical that the extension is present, or comfyui won't find
          # the file.
          name = "pony-xl-v6.safetensors";
          url = "https://civitai.com/api/download/models/290640?type=Model&format=SafeTensor&size=pruned&fp=fp16";
          sha256 = "1cxh5450k3y9mkrf9dby7hbaydj3ymjwq5fvzsrqk6j3xkc2zav7";
        })
      ];
      clip = [];
      clip_vision = [];
      configs = [];
      controlnet = [];
      embeddings = [];
      loras = [
        # https://civitai.com/models/264290?modelVersionId=398292
        (fetchModel {
          name = "ponx-xl-v6-artist-styles.safetensors";
          url = "https://civitai.com/api/download/models/398292?type=Model&format=SafeTensor";
          sha256 = "01m4zq2i1hyzvx95nq2v3n18b2m98iz0ryizdkyc1y42f1rwd0kx";
        })
        # https://civitai.com/models/200255/hands-xl-sd-15?modelVersionId=254267
        # Requires an auth token.
        # (fetchModel {
        #   name = "hands-sdxl.safetensors";
        #   url = "https://civitai.com/api/download/models/254267?type=Model&format=SafeTensor";
        #   sha256 = "00f65fia7g0ammwjw2vw1yhijw5kd2c54ksv3d64mgw6inplamr3";
        # })
      ];
      upscale_modules = [];
      vae = [
        (fetchModel {
          name = "sdxl_vae.safetensors";
          url = "https://civitai.com/api/download/models/290640?type=VAE";
          sha256 = "1qf65fia7g0ammwjw2vw1yhijw5kd2c54ksv3d64mgw6inplamr3";
        })
      ];
      vae_approx = [];
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
