################################################################################
# Gives us a ComfyUI server.  See https://github.com/comfyanonymous/ComfyUI for
# specifics on the server.
#
# This leverages work from https://github.com/NixOS/nixpkgs/pull/268378 by
# @fazo96.  As such it will be using my fork of fazo96's fork.
################################################################################
{ host-id }: { config, lib, pkgs, ... }: let
  # Default ComfyUI port.
  port = 8188;
  fetchModel = pkgs.callPackage ../hacks/comfyui/fetch-model.nix {};
  # fetchModel = import ../hacks/comfyui/fetch-model.nix;
in {
  age.secrets.civitai-token = {
    rekeyFile = (builtins.trace "civitai-token.age path" (lib.debug.traceVal ../secrets/rekeyed/${host-id}/civitai-token.age));
  };
  # age.rekey.secrets.civitai-token = {
  #   rekeyFile = (builtins.trace "civitai-token.age path" (lib.debug.traceVal ../secrets/rekeyed/${host-id}/civitai-token.age));
  # };
  # We need to override the original if we want to provide our own for rapid
  # iteration.  First we disable the original via `disabledModules`, and then
  # inject our own version via `imports`.
  disabledModules = [ "services/web-apps/comfyui.nix" ];
  imports = [ ../hacks/comfyui-services-web-apps/comfyui.nix ];
  environment.systemPackages = [
    # It's useful to be able to watch a network graph when downloading large
    # models.  Otherwise I just stare at a blank terminal.  These packages
    # (iftop and speedometer) provide means to watch the bits fly over the wire.
    # It won't give me a sense of progress though.  I'm not sure how to do that
    # besides use --verbose on the switch command (and even that seems iffy).
    pkgs.iftop
    # Uh wait, I thought I saw this somewhere.  Where did it go?
    # pkgs.speedometer
    # I've only been able to find this:
    # https://manpages.ubuntu.com/manpages/jammy/man1/speedometer.1.html
    # Here it is?
    # https://github.com/wardi/speedometer
    # Also: https://excess.org/speedometer/
  ];
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
    # Listen on 0.0.0.0, otherwise it's localhost connections only.
    listen = "0.0.0.0";
    extraArgs = {
    };
    dataPath = "/var/lib/comfyui";
    # Leaving this as the default seems to be broken with this error:
    # error: attribute 'cudaSupport' missing
    # package = pkgs.comfyui-cpu;
    package = pkgs.callPackage ../hacks/comfyui/package.nix {};
    models = {
      checkpoints = {
        # Pony generates some really high quality images - they tend to be more
        # based on a digital painting style but can do other things as well.
        # This makes it an excellent model for generating characters.
        # WARNING:  Pony is capable of generating some _very_ NSFW
        # images.  You should be able to use the negative prompt "nsfw" and
        # perhaps others to avoid this.
        pony-xl-v6 = (fetchModel {
          # It's critical that the extension is present, or comfyui won't find
          # the file.
          format = "safetensors";
          url = "https://civitai.com/api/download/models/290640?type=Model&format=SafeTensor&size=pruned&fp=fp16";
          sha256 = "1cxh5450k3y9mkrf9dby7hbaydj3ymjwq5fvzsrqk6j3xkc2zav7";
        });
      };
      clip = {};
      clip_vision = {};
      configs = {
        # https://huggingface.co/lllyasviel/ControlNet-v1-1
        # See also the accompanying file in `controlnet`.
        controlnet-v1_1_fe-sd15-tile = (fetchModel {
          format = "yaml";
          url = "https://huggingface.co/lllyasviel/ControlNet-v1-1/raw/main/control_v11f1e_sd15_tile.yaml";
          sha256 = "sha256-OeEzjEFDYYrbF2BPlsOj90DBq10VV9cbBE8DB6CmrbQ=";
        });
      };
      controlnet = {
        # https://huggingface.co/lllyasviel/ControlNet-v1-1
        # See also the accompanying file in `configs`.
        controlnet-v1_1_f1e-sd15-tile = (fetchModel {
          format = "pth";
          url = "https://huggingface.co/lllyasviel/ControlNet-v1-1/blob/main/control_v11f1e_sd15_tile.pth";
          sha256 = "11qndcrfz5jrjghn6v9is813igfd8310knl1l9rwxbf8lvwjncbc";
        });
      };
      embeddings = {};
      loras = {
        # https://civitai.com/models/264290?modelVersionId=398292
        ponx-xl-v6-artist-styles = (fetchModel {
          format = "safetensors";
          url = "https://civitai.com/api/download/models/398292?type=Model&format=SafeTensor";
          sha256 = "01m4zq2i1hyzvx95nq2v3n18b2m98iz0ryizdkyc1y42f1rwd0kx";
        });
        # https://civitai.com/models/200255/hands-xl-sd-15?modelVersionId=254267
        # Versions are not posted, so just use the "Updated:" date.
        hands-sdxl-v20240305 = (fetchModel {
          bearer = lib.fileContents config.age.secrets.civitai-token.path;
          format = "safetensors";
          url = "https://civitai.com/api/download/models/254267?type=Model&format=SafeTensor";
          sha256 = "sha256-a/NpZNiVK09Kdzs/pl0yADCF57BdCVuugYJd+g8Q9Kk=";
        });
        # https://civitai.com/models/200251/feet?modelVersionId=225347
        # Versions are not posted, so just use the "Updated:" date.
        # This is posted by the same author as the hands lora, and releases seem
        # to go out together.
        # feet-sdxl-v20240305 = (fetchModel {
        #   bearer = lib.fileContents config.age.secrets.civitai-token.path;
        #   format = "safetensors";
        #   url = "https://civitai.com/api/download/models/225347?type=Model&format=SafeTensor";
        #   sha256 = "";
        # });
      };
      upscale_modules = {};
      vae = {
        sdxl_vae = (fetchModel {
          format = "safetensors";
          url = "https://civitai.com/api/download/models/290640?type=VAE";
          sha256 = "1qf65fia7g0ammwjw2vw1yhijw5kd2c54ksv3d64mgw6inplamr3";
        });
      };
      vae_approx = {};
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
