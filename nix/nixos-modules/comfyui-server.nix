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
  networking.firewall.allowedTCPPorts = [
    # Default ComfyUI port.
    8188
  ];
  networking.firewall.allowedUDPPorts = [
    # Default ComfyUI port.
    8188
  ];
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
          # ''-DPYTHON_LIBRARY=$(python -c "import sysconfig; print(sysconfig.get_path('include'))")''
          ''-DPYTHON_LIBRARY=${prev.python3}/lib''
          ''-DPYTHON_INCLUDE_DIR=${prev.python3}/include''
          ''-DPYTHON_EXECUTABLE=${prev.python3}/bin/python3''
          # ''-DPYTHON_INCLUDE_DIR=$(python -c "import sysconfig; print(sysconfig.get_config_var('LIBDIR'))")''
        ] ++ prev-pkg.cmakeFlags;
      });
      # python3 = prev.python3.override {
      #   packageOverrides = (pf: pp: {
      #     torch = pp.pytorch-bin;
      #   });
      # };
      # Prefer this version:
      pythonPackagesExtensions = [(py-final: py-prev: {
        torch = py-final.torch-bin;
      })];
    })
  ];
  services.comfyui = {
    enable = true;
    dataPath = "/var/lib/comfyui";
    # Leaving this as the default seems to be broken with this error:
    # error: attribute 'cudaSupport' missing
    # package = pkgs.comfyui-cpu;
    package = pkgs.callPackage ../hacks/comfyui/package.nix {};

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

    #   pythonEnv = (pkgs.python311.withPackages (ps: with ps; [
    #     ps.pytorch-bin
    #     # (
    #     #   if gpuBackend == "cuda"
    #     #   then torchWithCuda
    #     #   else if gpuBackend == "rocm"
    #     #   then torchWithRocm
    #     #   else torch
    #     # )
    #     # torchsde
    #     # torchvision
    #     # torchaudio
    #     transformers
    #     safetensors
    #     accelerate
    #     aiohttp
    #     einops
    #     pyyaml
    #     pillow
    #     scipy
    #     psutil
    #     tqdm
    #   ] # ++ (builtins.concatMap (node: node.dependencies) customNodes)));
    #   ));
    # });
