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
  # fetchModel = pkgs.callPackage ../hacks/comfyui/fetch-model.nix {};
  # custom-nodes = (pkgs.callPackage ../hacks/comfyui/custom-nodes.nix {});
  # mkComfyUICustomNodes = custom-nodes.mkComfyUICustomNodes;
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
    # path = "/nix/var/secrets/civitai-bearer-token-header";
    path = "/etc/civitai-bearer-token-header";
    symlink = false;
    group = "nixbld";
    mode = "0444";
  };
  # We need to override the original if we want to provide our own for rapid
  # iteration.  First we disable the original via `disabledModules`, and then
  # inject our own version via `imports`.
  # disabledModules = [ "services/web-apps/comfyui.nix" ];
  # imports = [
  #   ../hacks/comfyui-services-web-apps/comfyui.nix
  # ];
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
    multi-user = true;
    customNodes = with pkgs.comfyui-custom-nodes; [
      comfyui-browser
      # Disabled due to https://github.com/tzwm/comfyui-profiler/issues/2
      # comfyui-profiler
      comfyui-crystools
      comfyui-custom-scripts
      images-grid-comfy-plugin
      ultimate-sd-upscale
    ];
    preview-method = "auto";
    extraArgs = {
    };
    dataPath = "/var/lib/comfyui";
    # Leaving this as the default seems to be broken with this error:
    # error: attribute 'cudaSupport' missing
    # package = pkgs.comfyui-cpu;
    # package = pkgs.callPackage ../hacks/comfyui/package.nix {};
    package = pkgs.comfyui-cuda;
    models = let
      bearer = builtins.readFile config.age.secrets.civitai-token.path;
    in {
      checkpoints = {
        # https://civitai.com/models/288584/autismmix-sdxl
        autism-mix-sdxl = {
          format = "safetensors";
          path = {
            inherit bearer;
            url = "https://civitai.com/api/download/models/324619";
            sha256 = "sha256-ghqlU3+N2v2/ljgnVRhlwxxbv6savnkly18AbI9x5IU=";
          };
        };
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
        colossus-xl-v6 = {
          format = "safetensors";
          path = {
            url = "https://civitai.com/api/download/models/355884";
            sha256 = "sha256-ZymMt9jS1Z698wujJGxEMQZeyt0E97qaOtLfDdWjhuc=";
          };
        };
        # https://civitai.com/models/112902/dreamshaper-xl
        # Preferred settings:
        # CFG = 2
        # 4-8 sampling steps.
        # Sampler: DPM SDE Kerras (not 2M).
        # ComfyUI workflow for upscaling: https://pastebin.com/79XN01xs
        dreamshaper-xl-fp16 = {
          format = "safetensors";
          path = {
            url = "https://civitai.com/api/download/models/351306";
            sha256 = "sha256-RJazbUi/18/k5dvONIXbVnvO+ivvcjjSkNvUVhISUIM=";
          };
        };
        # Pony generates some really high quality images - they tend to be more
        # based on a digital painting style but can do other things as well.
        # This makes it an excellent model for generating characters.
        # WARNING:  Pony is capable of generating some _very_ NSFW
        # images.  You should be able to use the negative prompt "nsfw" and
        # perhaps others to avoid this.
        pony-xl-v6 = {
          format = "safetensors";
          path = {
            url = "https://civitai.com/api/download/models/290640?type=Model&format=SafeTensor&size=pruned&fp=fp16";
            sha256 = "1cxh5450k3y9mkrf9dby7hbaydj3ymjwq5fvzsrqk6j3xkc2zav7";
          };
        };
        # Allow for video from images.  See
        # https://comfyanonymous.github.io/ComfyUI_examples/video/ for the
        # official ComfyUI documentation.
        stable-video-diffusion-img2vid-xt = {
          format = "safetensors";
          path = {
            url = "https://huggingface.co/stabilityai/stable-video-diffusion-img2vid-xt/resolve/main/svd_xt.safetensors?download=true";
            sha256 = "b2652c23d64a1da5f14d55011b9b6dce55f2e72e395719f1cd1f8a079b00a451";
          };
        };
      };
      clip = {};
      clip_vision = {};
      configs = {
        # https://huggingface.co/lllyasviel/ControlNet-v1-1
        # https://github.com/lllyasviel/ControlNet-v1-1-nightly
        # See also the accompanying file in `controlnet`.
        controlnet-v1_1_fe-sd15-tile-yaml = {
          format = "yaml";
          path = {
            url = "https://huggingface.co/lllyasviel/ControlNet-v1-1/raw/69fc48b9cbd98661f6d0288dc59b59a5ccb32a6b/control_v11f1e_sd15_tile.yaml";
            sha256 = "sha256-OeEzjEFDYYrbF2BPlsOj90DBq10VV9cbBE8DB6CmrbQ=";
          };
        };
      };
      controlnet = {
        # https://huggingface.co/TTPlanet/TTPLanet_SDXL_Controlnet_Tile_Realistic_V1
        ttplanet-sdxl-controlnet-tile-realistic-32-v1 = {
          format = "safetensors";
          path = {
            url = "https://huggingface.co/TTPlanet/TTPLanet_SDXL_Controlnet_Tile_Realistic/raw/83fbced5f508ee3c78288f1db3dd93306a760d4d/TTPLANET_Controlnet_Tile_realistic_v1_fp32.safetensors";
            sha256 = "f33012cbac5839885f143aa2accb2e403414c7dad119364bbe18de37e4a65f67";
          };
        };
        ttplanet-sdxl-controlnet-tile-realistic-16-v1 = {
          format = "safetensors";
          path = {
            url = "https://huggingface.co/TTPlanet/TTPLanet_SDXL_Controlnet_Tile_Realistic_V1/resolve/main/TTPLANET_Controlnet_Tile_realistic_v1_fp16.safetensors?download=true";
            sha256 = "sha256-+ipfL+yBSBnINUA8d4viwkN9FHkxkhMEVp/M7CtFFzw=";
          };
        };
        # https://huggingface.co/lllyasviel/ControlNet-v1-1
        # See also the accompanying file in `configs`.
        controlnet-v1_1_f1e-sd15-tile-pth = {
          format = "pth";
          path = {
            url = "https://huggingface.co/lllyasviel/ControlNet-v1-1/raw/69fc48b9cbd98661f6d0288dc59b59a5ccb32a6b/control_v11f1e_sd15_tile.pth";
            sha256 = "sha256-hpBH/B2u4eXcHvhqxOrjX52Nxh6cQyltx/z8XoEjcds=";
          };
        };
      };
      embeddings = {};
      loras = {
        # Helps with eyes.
        # https://civitai.com/models/118427/perfect-eyes-xl?modelVersionId=128461
        perfect-eyes-xl = {
          format = "safetensors";
          path = {
            url = "https://civitai.com/api/download/models/128461?type=Model&format=SafeTensor";
            sha256 = "sha256-8kg2TPCsx6ALxLUUW0TA378Q5x6bDvtrd/CVauryQRw=";
          };
        };
        # Helps with indicating various styles in PonyXL, such as oil,
        # realistic, digital art, and combinations thereof.
        # https://civitai.com/models/264290?modelVersionId=398292
        ponyx-xl-v6-non-artist-styles = {
          format = "safetensors";
          path = {
            url = "https://civitai.com/api/download/models/398292?type=Model&format=SafeTensor";
            sha256 = "01m4zq2i1hyzvx95nq2v3n18b2m98iz0ryizdkyc1y42f1rwd0kx";
          };
        };
        # https://civitai.com/models/200255/hands-xl-sd-15?modelVersionId=254267
        # Versions are not posted, so just use the "Updated:" date.
        hands-sdxl-v20240305 = {
          format = "safetensors";
          path = {
            inherit bearer;
            url = "https://civitai.com/api/download/models/254267?type=Model&format=SafeTensor";
            sha256 = "sha256-a/NpZNiVK09Kdzs/pl0yADCF57BdCVuugYJd+g8Q9Kk=";
          };
        };
        ralph-breaks-internet-disney-princesses = {
          format = "safetensors";
          path = {
            url = "https://civitai.com/api/download/models/244808?type=Model&format=SafeTensor";
            sha256 = "sha256-gKpnkTrryJoBvhkH5iEi8zn9/ucMFxq3upZ8Xl/PJ+o=";
          };
        };
        # https://civitai.com/models/200251/feet?modelVersionId=225347
        # Versions are not posted, so just use the "Updated:" date.
        # This is posted by the same author as the hands lora, and releases seem
        # to go out together.
        feet-sdxl-v20240305 = {
          format = "safetensors";
          path = {
            inherit bearer;
            # bearerFile = config.age.secrets.civitai-bearer-token-header.path;
            url = "https://civitai.com/api/download/models/225347?type=Model&format=SafeTensor";
            sha256 = "sha256-5OuEVEBiNYj+ja7BpBGwf+8uCnlQg6+xvAjt45RueNI=";
          };
        };
      };
      # Upscaler comparisons can be found here:
      # https://civitai.com/articles/636/sd-upscalers-comparison
      upscale_models = {
        # https://openmodeldb.info/models/4x-realesrgan-x4plus
        # https://github.com/xinntao/Real-ESRGAN
        real-esrgan-4xplus = {
          format = "pth";
          path = {
            url = "https://github.com/xinntao/Real-ESRGAN/releases/download/v0.1.0/RealESRGAN_x4plus.pth";
            sha256 = "sha256-T6DTiQX3WsButJp5UbQmZwAhvjAYJl/RkdISXfnWgvE=";
          };
        };
        # Doesn't work at all - unsupported model.  Must be older SD version
        # only.
        stable-diffusion-4x-upscaler = {
          format = "safetensors";
          path = {
            url = "https://huggingface.co/stabilityai/stable-diffusion-x4-upscaler/resolve/main/x4-upscaler-ema.safetensors?download=true";
            sha256 = "35c01d6160bdfe6644b0aee52ac2667da2f40a33a5d1ef12bbd011d059057bc6";
          };
        };
        # Samael1976 reposted this to civitai.com - the alternative is to
        # download it from mega.nz, which I do not believe is friendly to
        # headless activity such as this.  The original model is listed here:
        # https://openmodeldb.info/models/4x-UltraSharp
        kim2091-4k-ultrasharp = {
          format = "pth";
          path = {
            url = "https://huggingface.co/Kim2091/UltraSharp/raw/main/4x-UltraSharp.pth";
            sha256 = "sha256-1UUMW6f2bilb9iROW6olbMcUnwOImHAgeEQaVDs6jW4=";
          };
        };
      };
      vae = {
        sdxl_vae = {
          format = "safetensors";
          path = {
            url = "https://civitai.com/api/download/models/290640?type=VAE";
            sha256 = "1qf65fia7g0ammwjw2vw1yhijw5kd2c54ksv3d64mgw6inplamr3";
          };
        };
      };
      vae_approx = {};
    };
  };
}

# Next up, try this: eb9b60a from
# https://github.com/r-ryantm/nixpkgs/commit/eb9b60a2de7e9f406efed12f68c2024d5955b35e
# https://github.com/NixOS/nixpkgs/pull/293555/files
# Original invocation:
# nix build --impure -L nixpkgs/383e4b2b#cudaPackages.nccl
# Adapted invocation:
# nix build --impure -L nixpkgs/eb9b60a#cudaPackages.nccl
