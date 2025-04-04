################################################################################
# Gives us a ComfyUI server.  See https://github.com/comfyanonymous/ComfyUI for
# specifics on the server.
#
# This leverages work from https://github.com/NixOS/nixpkgs/pull/268378 by
# @fazo96.  As such it will be using my fork of fazo96's fork.
################################################################################
{ host-id, port }: { config, lib, pkgs, ... }: let
  # Set this to true to disable anything that needs build time secrets, so
  # agenix-rekey can actually lay down those files.  Run a switch to execute.
  # Then set it back to true and run a switch again.
  key-catch-22 = false;
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
    path = "/etc/civitai-bearer-token-header";
    symlink = false;
    group = "nixbld";
    mode = "0444";
  };
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
  # networking.firewall.allowedTCPPorts = [ port ];
  # networking.firewall.allowedUDPPorts = [ port ];
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
        # Some of this is required to use torch-bin, but I couldn't get it to
        # work.  That said, I didn't build it while I had a _working_ Cuda
        # configuration - the library and kernel module didn't match.  I have
        # only recently learned this can cause cryptic build issues.  Simply
        # switching out `torch` with `torch-bin` (and the `pytorch` alias)
        # doesn't seem to be enough, as it will complain about a lack of
        # `cudaCapabilities` attribute later.  I don't know where this attribute
        # comes from and haven't had luck finding it.
        # transformers = py-prev.transformers.override { torch = py-final.pytorch-bin; };
        # safetensors = py-prev.safetensors.override { torch = py-final.pytorch-bin; };
        # accelerate = py-prev.accelerate.override { torch = py-final.pytorch-bin; };
        # openai-triton = py-final.openai-triton-cuda;
        # torch = py-final.torch-bin;
        # pytorch = py-final.pytorch-bin;
        # torchaudio = py-final.torchaudio-bin;
      })];
    })
  ];
  services.comfyui = {
    enable = true;
    multi-user = true;
    customNodes = with pkgs.comfyui-custom-nodes; [
      # Manages workflows in comfyui such that they can be version controlled
      # easily.
      # https://github.com/talesofai/comfyui-browser
      #
      # This uses a fork that allows for configurable directories and debugging
      # the XYZ Plot node.
      (pkgs.comfyui-custom-nodes.mkComfyUICustomNodes {
        pname = "comfyui-browser";
        version = "unstable-fork-2024-04-21";
        src = pkgs.fetchFromGitHub {
          owner = "LoganBarnett";
          repo = "comfyui-browser";
          rev = "1d8ce54d06081c7477895de284e6c0bfd604c906";
          hash = "sha256-X9pJtkeRO+RQiwHlg4bVKQYY2XvdLREm3J6Af1x97vs=";
        };
        patches = [
          # ../hacks/comfyui/comfyui-browser-debug-xyz-plot.patch
        ];
        installPhase = let
          # Patches don't apply to $src, and as with many scripting languages
          # that don't have a build output per se, we just want the script
          # source itself placed into $out.  So just copy everything into $out
          # instead of from $src so we can make sure we get everything in the
          # future, and we use the patched versions.
          install = ''
            shopt -s dotglob
            shopt -s extglob
            cp -r ./!($out|$src) $out/
          '';
          in ''
          mkdir -p $out/
          ${install}
          cp ${pkgs.writeText "config.json" (builtins.toJSON {
            collections = "/var/lib/comfyui/comfyui-browser-collections";
            download_logs = "/var/lib/comfyui/comfyui-browser-download-logs";
            outputs = "/var/lib/comfyui/output";
            sources = "/var/lib/comfyui/comfyui-browser-sources";
          })} $out/config.json
        '';
      })
      # comfyui-browser
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
    # package = pkgs.comfyui-cuda;
    models = if key-catch-22 then
      {}
    else
    let
      bearer = builtins.readFile config.age.secrets.civitai-token.path;
      fetch-civitai = { ... }@args: pkgs.fetchurl ({
        curlOptsList = [
          "--header" "Authorization: Bearer ${bearer}"
          "--location"
        ];
      } // args);
    in {
      checkpoints = {
        # https://civitai.com/models/288584/autismmix-sdxl
        "autism-mix-sdxl.safetensors" = fetch-civitai {
          url = "https://civitai.com/api/download/models/324619";
          sha256 = "sha256-ghqlU3+N2v2/ljgnVRhlwxxbv6savnkly18AbI9x5IU=";
        };
        # A high quality checkpoint but beware it also does nsfw very
        # easily.
        # https://civitai.com/models/147720/colossus-project-xl-sfwandnsfw
        # Some notes on usage, from the description:
        # Be aware that some samplers aren't working. Don't use following
        # samplers:
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
        "colossus-xl-v6.safetensors" = fetch-civitai {
          url = "https://civitai.com/api/download/models/355884";
          sha256 = "sha256-ZymMt9jS1Z698wujJGxEMQZeyt0E97qaOtLfDdWjhuc=";
        };
        # https://civitai.com/models/112902/dreamshaper-xl
        # Preferred settings:
        # CFG = 2
        # 4-8 sampling steps.
        # Sampler: DPM SDE Kerras (not 2M).
        # ComfyUI workflow for upscaling: https://pastebin.com/79XN01xs
        "dreamshaper-xl-fp16.safetensors" = fetch-civitai {
          url = "https://civitai.com/api/download/models/351306";
          sha256 = "sha256-RJazbUi/18/k5dvONIXbVnvO+ivvcjjSkNvUVhISUIM=";
        };
        # Pony generates some really high quality images - they tend to be more
        # based on a digital painting style but can do other things as well.
        # This makes it an excellent model for generating characters.
        # WARNING:  Pony is capable of generating some _very_ NSFW
        # images.  You should be able to use the negative prompt "nsfw" and
        # perhaps others to avoid this.
        "pony-xl-v6.safetensors" = fetch-civitai {
          url = "https://civitai.com/api/download/models/290640?type=Model&format=SafeTensor&size=pruned&fp=fp16";
          sha256 = "1cxh5450k3y9mkrf9dby7hbaydj3ymjwq5fvzsrqk6j3xkc2zav7";
        };
        # Allow for video from images.  See
        # https://comfyanonymous.github.io/ComfyUI_examples/video/ for the
        # official ComfyUI documentation.
        "stable-video-diffusion-img2vid-xt.safetensors" = pkgs.fetchurl {
          url = "https://huggingface.co/stabilityai/stable-video-diffusion-img2vid-xt/resolve/f4aeeadd844420611e9f9d16332d440621841550/svd_xt.safetensors?download=true";
          sha256 = "b2652c23d64a1da5f14d55011b9b6dce55f2e72e395719f1cd1f8a079b00a451";
        };
      };
      clip = {};
      clip_vision = {};
      configs = {
        # https://huggingface.co/lllyasviel/ControlNet-v1-1
        # https://github.com/lllyasviel/ControlNet-v1-1-nightly
        # See also the accompanying file in `controlnet`.
        "controlnet-v1_1_fe-sd15-tile.yaml" = pkgs.fetchurl {
          url = "https://huggingface.co/lllyasviel/ControlNet-v1-1/resolve/69fc48b9cbd98661f6d0288dc59b59a5ccb32a6b/control_v11f1e_sd15_tile.yaml?download=true";
          sha256 = "sha256-OeEzjEFDYYrbF2BPlsOj90DBq10VV9cbBE8DB6CmrbQ=";
        };
      };
      controlnet = {
        # https://huggingface.co/TTPlanet/TTPLanet_SDXL_Controlnet_Tile_Realistic_V1
        "ttplanet-sdxl-controlnet-tile-realistic-32-v1.safetensors" = pkgs.fetchurl {
          url = "https://huggingface.co/TTPlanet/TTPLanet_SDXL_Controlnet_Tile_Realistic/resolve/83fbced5f508ee3c78288f1db3dd93306a760d4d/TTPLANET_Controlnet_Tile_realistic_v1_fp32.safetensors?download=true";
          sha256 = "f33012cbac5839885f143aa2accb2e403414c7dad119364bbe18de37e4a65f67";
        };
        "ttplanet-sdxl-controlnet-tile-realistic-16-v1.safetensors" = pkgs.fetchurl {
          url = "https://huggingface.co/TTPlanet/TTPLanet_SDXL_Controlnet_Tile_Realistic/resolve/83fbced5f508ee3c78288f1db3dd93306a760d4d/TTPLANET_Controlnet_Tile_realistic_v1_fp16.safetensors?download=true";
          sha256 = "sha256-+ipfL+yBSBnINUA8d4viwkN9FHkxkhMEVp/M7CtFFzw=";
        };
        # https://huggingface.co/lllyasviel/ControlNet-v1-1
        # See also the accompanying file in `configs`.
        "controlnet-v1_1_f1e-sd15-tile.pth" = pkgs.fetchurl {
          url = "https://huggingface.co/lllyasviel/ControlNet-v1-1/resolve/69fc48b9cbd98661f6d0288dc59b59a5ccb32a6b/control_v11f1e_sd15_tile.pth?download=true";
          hash = "sha256-iqabjTkecsL87WplAmgTfcDtWUyv6KLA+LmUeZohl5s=";
        };
      };
      embeddings = {};
      loras = {
        # Helps with eyes.
        # https://civitai.com/models/118427/perfect-eyes-xl?modelVersionId=128461
        "perfect-eyes-xl.safetensors" = fetch-civitai {
          url = "https://civitai.com/api/download/models/128461?type=Model&format=SafeTensor";
          sha256 = "sha256-8kg2TPCsx6ALxLUUW0TA378Q5x6bDvtrd/CVauryQRw=";
        };
        # Helps with indicating various styles in PonyXL, such as oil,
        # realistic, digital art, and combinations thereof.
        # https://civitai.com/models/264290?modelVersionId=398292
        "ponyx-xl-v6-non-artist-styles.safetensors" = fetch-civitai {
          url = "https://civitai.com/api/download/models/398292?type=Model&format=SafeTensor";
          sha256 = "01m4zq2i1hyzvx95nq2v3n18b2m98iz0ryizdkyc1y42f1rwd0kx";
        };
        # https://civitai.com/models/200255/hands-xl-sd-15?modelVersionId=254267
        # Versions are not posted, so just use the "Updated:" date.
        "hands-sdxl-v20240305.safetensors" = fetch-civitai {
          url = "https://civitai.com/api/download/models/254267?type=Model&format=SafeTensor";
          sha256 = "sha256-a/NpZNiVK09Kdzs/pl0yADCF57BdCVuugYJd+g8Q9Kk=";
        };
        "ralph-breaks-internet-disney-princesses.safetensors" = fetch-civitai {
          url = "https://civitai.com/api/download/models/244808?type=Model&format=SafeTensor";
          sha256 = "sha256-gKpnkTrryJoBvhkH5iEi8zn9/ucMFxq3upZ8Xl/PJ+o=";
        };
        # https://civitai.com/models/200251/feet?modelVersionId=225347
        # Versions are not posted, so just use the "Updated:" date.
        # This is posted by the same author as the hands lora, and releases seem
        # to go out together.
        "feet-sdxl-v20240305.safetensors" = fetch-civitai {
          url = "https://civitai.com/api/download/models/225347?type=Model&format=SafeTensor";
          sha256 = "sha256-5OuEVEBiNYj+ja7BpBGwf+8uCnlQg6+xvAjt45RueNI=";
        };
      };
      # Upscaler comparisons can be found here:
      # https://civitai.com/articles/636/sd-upscalers-comparison
      upscale_models = {
        # https://openmodeldb.info/models/4x-realesrgan-x4plus
        # https://github.com/xinntao/Real-ESRGAN
        "real-esrgan-4xplus.pth" = pkgs.fetchurl {
          url = "https://github.com/xinntao/Real-ESRGAN/releases/download/v0.1.0/RealESRGAN_x4plus.pth";
          sha256 = "sha256-T6DTiQX3WsButJp5UbQmZwAhvjAYJl/RkdISXfnWgvE=";
        };
        # Doesn't work at all - unsupported model.  Must be older SD version
        # only.
        "stable-diffusion-4x-upscaler.safetensors" = pkgs.fetchurl {
          url = "https://huggingface.co/stabilityai/stable-diffusion-x4-upscaler/resolve/a44206cef52cdd20c6a32fea6257860474f0be43/x4-upscaler-ema.safetensors?download=true";
          sha256 = "35c01d6160bdfe6644b0aee52ac2667da2f40a33a5d1ef12bbd011d059057bc6";
        };
        # Samael1976 reposted this to civitai.com - the alternative is to
        # download it from mega.nz, which I do not believe is friendly to
        # headless activity such as this.  The original model is listed here:
        # https://openmodeldb.info/models/4x-UltraSharp
        "kim2091-4k-ultrasharp.pth" = pkgs.fetchurl {
          url = "https://huggingface.co/Kim2091/UltraSharp/resolve/b32be1b8c6f81d1f6ef8ce11f016aad3a139d1e0/4x-UltraSharp.pth";
          sha256 = "sha256-1UUMW6f2bilb9iROW6olbMcUnwOImHAgeEQaVDs6jW4=";
        };
      };
      vae = {
        "sdxl_vae.safetensors" = fetch-civitai {
          url = "https://civitai.com/api/download/models/290640?type=VAE";
          sha256 = "1qf65fia7g0ammwjw2vw1yhijw5kd2c54ksv3d64mgw6inplamr3";
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
