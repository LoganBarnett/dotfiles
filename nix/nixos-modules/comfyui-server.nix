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
  custom-nodes = (pkgs.callPackage ../hacks/comfyui/custom-nodes.nix {});
  mkComfyUICustomNodes = custom-nodes.mkComfyUICustomNodes;
  # fetchModel = import ../hacks/comfyui/fetch-model.nix;
in {
  # We don't actually need this file, but it's kept for reference.
  age.secrets.civitai-token = {
    rekeyFile = ../secrets/civitai-token.age;
  };
  # There's problems with feeding in secrets at build time, so we've written a
  # file that is essentially "Authorization: Bearer <token>".  Curl can use a
  # file as a header listing if the argument starts with @ to denote a file path
  # instead of a header value.
  age.secrets.civitai-bearer-token-header = {
    rekeyFile = ../secrets/civitai-bearer-token-header.age;
  };
  # We need to override the original if we want to provide our own for rapid
  # iteration.  First we disable the original via `disabledModules`, and then
  # inject our own version via `imports`.
  disabledModules = [ "services/web-apps/comfyui.nix" ];
  imports = [
    ../hacks/comfyui-services-web-apps/comfyui.nix
  ];
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
    customNodes = [
      (mkComfyUICustomNodes {
        pname = "ultimate-sd-upscale";
        version = "unstable-2023-08-16";
        src = pkgs.fetchFromGitHub {
          owner = "ssitu";
          repo = "ComfyUI_UltimateSDUpscale";
          rev = "b303386bd363df16ad6706a13b3b47a1c2a1ea49";
          sha256 = "sha256-kcvhafXzwZ817y+8LKzOkGR3Y3QBB7Nupefya6s/HF4=";
          fetchSubmodules = true;
        };
      })
      custom-nodes.comfyui-browser
      custom-nodes.comfyui-profiler
      custom-nodes.comfyui-crystools
      custom-nodes.comfyui-custom-scripts
    ];
    preview-method = "auto";
    extraArgs = {
    };
    dataPath = "/var/lib/comfyui";
    # Leaving this as the default seems to be broken with this error:
    # error: attribute 'cudaSupport' missing
    # package = pkgs.comfyui-cpu;
    package = pkgs.callPackage ../hacks/comfyui/package.nix {};
    models = let
      bearer = builtins.readFile config.age.secrets.civitai-token.path;
    in {
      checkpoints = {
        # https://civitai.com/models/288584/autismmix-sdxl
        autism-mix-sdxl = (fetchModel {
          inherit bearer;
          url = "https://civitai.com/api/download/models/324619";
          format = "safetensors";
          sha256 = "sha256-ghqlU3+N2v2/ljgnVRhlwxxbv6savnkly18AbI9x5IU=";
        });
        # A high quality checkpoint but beware it also does nsfw very
        # easily.
        # https://civitai.com/models/147720/colossus-project-xl-sfwandnsfw
        # Some notes on usage, from the description:
        # Be aware that some samplers aren't working. Don't use following samplers:
        # DPM++ 2M Karras, DPM++ 2M,DPM++ 2M, DPM++ 2M SDE, DPM++ 2M SDE Heun,
        # Euler, LMS, LMS Karras, Heun, 3M SDE Karras, DPM fast, DPM2 Karras,
        # Restart, PLMS, DDIM, Uni PC, LCM, LCM Karras.
        # Recommended sampler:
        # In my tests DPM 2 a and DPM++ 2S a worked really good for fine
        # details. You can also use the Karras versions of these samplers. Also
        # DPM++ SDE, DPM++ SDE Karras, Euler a, Euler a Turbo, DDPM, DDPM
        # Karras, DPM++ 2M Turbo, DPM++ 2M SDE Heun Exponential worked great in
        # my tests.
        #
        # Keep the CFG around 2-4.
        colossus-xl-v6 = (fetchModel {
          url = "https://civitai.com/api/download/models/355884";
          format = "safetensors";
          sha256 = "sha256-ZymMt9jS1Z698wujJGxEMQZeyt0E97qaOtLfDdWjhuc=";
        });
        # https://civitai.com/models/112902/dreamshaper-xl
        # Preferred settings:
        # CFG = 2
        # 4-8 sampling steps.
        # Sampler: DPM SDE Kerras (not 2M).
        # ComfyUI workflow for upscaling: https://pastebin.com/79XN01xs
        dreamshaper-xl-fp16 = (fetchModel {
          url = "https://civitai.com/api/download/models/351306";
          format = "safetensors";
          sha256 = "sha256-RJazbUi/18/k5dvONIXbVnvO+ivvcjjSkNvUVhISUIM=";
        });
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
        # https://github.com/lllyasviel/ControlNet-v1-1-nightly
        # See also the accompanying file in `controlnet`.
        controlnet-v1_1_fe-sd15-tile = (fetchModel {
          format = "yaml";
          url = "https://huggingface.co/lllyasviel/ControlNet-v1-1/raw/main/control_v11f1e_sd15_tile.yaml";
          sha256 = "sha256-OeEzjEFDYYrbF2BPlsOj90DBq10VV9cbBE8DB6CmrbQ=";
        });
      };
      controlnet = {
        # https://huggingface.co/TTPlanet/TTPLanet_SDXL_Controlnet_Tile_Realistic_V1
        ttplanet-sdxl-controlnet-tile-realistic-32-v1 = (fetchModel {
          format = "safetensors";
          url = "https://huggingface.co/TTPlanet/TTPLanet_SDXL_Controlnet_Tile_Realistic_V1/resolve/main/TTPLANET_Controlnet_Tile_realistic_v1_fp32.safetensors?download=true";
          sha256 = "sha256-Xj+FHFZvH5ApauL9IRaKR9aIIcK0xTO66SWdx/pqsdI=";
        });
        ttplanet-sdxl-controlnet-tile-realistic-16-v1 = (fetchModel {
          format = "safetensors";
          url = "https://huggingface.co/TTPlanet/TTPLanet_SDXL_Controlnet_Tile_Realistic_V1/resolve/main/TTPLANET_Controlnet_Tile_realistic_v1_fp16.safetensors?download=true";
          sha256 = "sha256-+ipfL+yBSBnINUA8d4viwkN9FHkxkhMEVp/M7CtFFzw=";
        });
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
        # Helps with eyes.
        # https://civitai.com/models/118427/perfect-eyes-xl?modelVersionId=128461
        perfect-eyes-xl = (fetchModel {
          format = "safetensors";
          url = "https://civitai.com/api/download/models/128461?type=Model&format=SafeTensor";
          sha256 = "sha256-8kg2TPCsx6ALxLUUW0TA378Q5x6bDvtrd/CVauryQRw=";
        });
        # Helps with indicating various styles in PonyXL, such as oil,
        # realistic, digital art, and combinations thereof.
        # https://civitai.com/models/264290?modelVersionId=398292
        ponyx-xl-v6-non-artist-styles = (fetchModel {
          format = "safetensors";
          url = "https://civitai.com/api/download/models/398292?type=Model&format=SafeTensor";
          sha256 = "01m4zq2i1hyzvx95nq2v3n18b2m98iz0ryizdkyc1y42f1rwd0kx";
        });
        # https://civitai.com/models/200255/hands-xl-sd-15?modelVersionId=254267
        # Versions are not posted, so just use the "Updated:" date.
        hands-sdxl-v20240305 = (fetchModel {
          inherit bearer;
          format = "safetensors";
          url = "https://civitai.com/api/download/models/254267?type=Model&format=SafeTensor";
          sha256 = "sha256-a/NpZNiVK09Kdzs/pl0yADCF57BdCVuugYJd+g8Q9Kk=";
        });
        # https://civitai.com/models/200251/feet?modelVersionId=225347
        # Versions are not posted, so just use the "Updated:" date.
        # This is posted by the same author as the hands lora, and releases seem
        # to go out together.
        feet-sdxl-v20240305 = (fetchModel {
          inherit bearer;
          # bearerFile = config.age.secrets.civitai-bearer-token-header.path;
          format = "safetensors";
          url = "https://civitai.com/api/download/models/225347?type=Model&format=SafeTensor";
          sha256 = "sha256-5OuEVEBiNYj+ja7BpBGwf+8uCnlQg6+xvAjt45RueNI=";
        });
      };
      # Upscaler comparisons can be found here:
      # https://civitai.com/articles/636/sd-upscalers-comparison
      upscale_models = {
        # https://openmodeldb.info/models/4x-realesrgan-x4plus
        # https://github.com/xinntao/Real-ESRGAN
        real-esrgan-4xplus = (fetchModel {
          url = "https://github.com/xinntao/Real-ESRGAN/releases/download/v0.1.0/RealESRGAN_x4plus.pth";
          format = "pth";
          sha256 = "sha256-T6DTiQX3WsButJp5UbQmZwAhvjAYJl/RkdISXfnWgvE=";
        });
        # Doesn't work at all - unsupported model.  Must be older SD version
        # only.
        stable-diffusion-4x-upscaler = (fetchModel {
          url = "https://huggingface.co/stabilityai/stable-diffusion-x4-upscaler/resolve/main/x4-upscaler-ema.safetensors?download=true";
          format = "safetensors";
          sha256 = "35c01d6160bdfe6644b0aee52ac2667da2f40a33a5d1ef12bbd011d059057bc6";
        });
        # Samael1976 reposted this to civitai.com - the alternative is to
        # download it from mega.nz, which I do not believe is friendly to
        # headless activity such as this.  The original model is listed here:
        # https://openmodeldb.info/models/4x-UltraSharp
        kim2091-4k-ultrasharp = (fetchModel {
          format = "pth";
          url = "https://huggingface.co/Kim2091/UltraSharp/blob/main/4x-UltraSharp.pth";
          # sha256 = "sha256-pYEiMfyTa0KvCKXtunhBlUldMD1bMkjCRInvDEAh/gE=";
          # sha256 = "sha256-JZDtqx4ZEUbexkONl9z2BV8viMFmRnalj6NfoDeYNgU=";
          sha256 = "sha256-weKbF1v8Yi1+8r/eagg6FWJ5QFdFmmTLXZlJFQdFiIU=";
          # SHA256 reported by civitai.com.  Unsure how these relate.  It is not
          # base64 encoded.
          # "A5812231FC936B42AF08A5EDBA784195495D303D5B3248C24489EF0C4021FE01"
        });
      };
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
