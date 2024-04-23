# TODO: Document how to override this data (let alone that it exists).  Show how
# to specify the workflow directory ala:
# https://github.com/pythongosssss/ComfyUI-Custom-Scripts/blob/main/pysssss.example.json
{ comfyui-custom-scripts-autocomplete-text ? (builtins.fetchurl {
  url = "https://gist.githubusercontent.com/pythongosssss/1d3efa6050356a08cea975183088159a/raw/a18fb2f94f9156cf4476b0c24a09544d6c0baec6/danbooru-tags.txt";
  sha256 = "15xmm538v0mjshncglpbkw2xdl4cs6y0faz94vfba70qq87plz4p";
})
, comfyui-custom-scripts-data ? {
  name = "CustomScripts";
  logging = false;
}
, fetchFromGitHub
, lib
, writeTextFile
, pkgs
, stdenv
}:

let
  mkComfyUICustomNodes = args: stdenv.mkDerivation ({
    installPhase = ''
      runHook preInstall
      mkdir -p $out/
      cp -r $src/* $out/
      runHook postInstall
    '';

    passthru.dependencies = [];
  } // args);
in
# TODO: These should have a meta field for better searching.
{
  inherit mkComfyUICustomNodes;

  # Handle upscaling of smaller images into larger ones.  This is helpful to go
  # from a prototyped image to a highly detailed, high resolution version.
  ultimate-sd-upscale = mkComfyUICustomNodes {
    pname = "ultimate-sd-upscale";
    version = "unstable-2024-03-30";

    src = fetchFromGitHub {
      owner = "ssitu";
      repo = "ComfyUI_UltimateSDUpscale";
      rev = "b303386bd363df16ad6706a13b3b47a1c2a1ea49";
      hash = "sha256-kcvhafXzwZ817y+8LKzOkGR3Y3QBB7Nupefya6s/HF4=";
      fetchSubmodules = true;
    };
  };

  # Manages workflows in comfyui such that they can be version controlled
  # easily.
  # https://github.com/talesofai/comfyui-browser
  #
  # This uses a fork that allows for configurable directories.
  comfyui-browser = mkComfyUICustomNodes {
    pname = "comfyui-browser";
    version = "unstable-fork-2024-04-21";
    src = fetchFromGitHub {
      owner = "LoganBarnett";
      repo = "comfyui-browser";
      rev = "506cf662e390ba736481faf1e080ed94eb85dc2c";
      hash = "sha256-Ja+NWEXTMxYUG/NKuYfj6LqbjWAeMoU7FuLqdLKMb8M=";
    };
    installPhase = ''
      mkdir -p $out/
      cp -r $src/* $out/
      cp ${pkgs.writeText "config.json" (builtins.toJSON {
        collections = "/var/lib/comfyui/comfyui-browser-collections";
        download_logs = "/var/lib/comfyui/comfyui-browser-download-logs";
        outputs = "/var/lib/comfyui/output";
        sources = "/var/lib/comfyui/comfyui-browser-sources";
      })} $out/config.json
    '';
  };

  # Show the time spent in various nodes of a workflow.
  comfyui-profiler = mkComfyUICustomNodes {
    pname = "comfyui-profiler";
    version = "unstable-2024-01-11";
    src = fetchFromGitHub {
      owner = "tzwm";
      repo = "comfyui-profiler";
      rev = "942dfe484c481f7cdab8806fa278b3df371711bf";
      hash = "sha256-J0iTRycneGYF6RGJyZ/mVtEge1dxakwny0yfG1ceOd8=";
    };
  };

  # https://github.com/crystian/ComfyUI-Crystools
  # Various tools/nodes:
  # 1. Resources monitor (CUDA GPU usage, CPU usage, memory, etc).
  #   a. CUDA only.
  # 2. Progress monitor.
  # 3. Compare images.
  # 4. Compare workflow JSON documents.
  # 5. Debug values.
  # 6. Pipes - A means of condensing multiple inputs or outputs together into a
  #    single output or input (respectively).
  # 7. Better nodes for:
  #   a. Saving images.
  #   b. Loading images.
  #   c. See "hidden" data(?).
  # 8. New primitives (list), and possibly better/different replacements for
  #    original primitives.
  # 9. Switch - turn on or off functionality based on a boolean primitive.
  # 9. More™!
  #
  comfyui-crystools = mkComfyUICustomNodes (let
    version = "1.12.0";
  in {
    pname = "comfyui-cystools";
    inherit version;
    src = fetchFromGitHub {
      owner = "crystian";
      repo = "ComfyUI-Crystools";
      rev = version;
      hash = "sha256-ZzbMgFeV5rrRU76r/wKnbhujoqE7UDOSaLgQZhguXuY=";
    };
    # TODO: Can we read from one of the Python library tools or build the list
    # from the project metadata?
    passthru.dependencies = with pkgs.python3Packages; [
      deepdiff
      py-cpuinfo
      pynvml
    ];
  });

  # https://github.com/pythongosssss/ComfyUI-Custom-Scripts
  # Various tools/nodes:
  # 1. Autocomplete of keywords, showing keyword count in the model.
  # 2. Auto-arrange graph.
  # 3. Always snap to grid.
  # 4. Loaders that show preview images, have example prompts, and are cataloged
  #    under folders.
  # 5. Checkpoint/LoRA metadata viewing.
  # 6. Image constraints (I assume for preview).
  # 7. Favicon for comfyui.
  # 8. Image feed showing images of the current session.
  # 9. Advanced KSampler denoise "helper" - asks for steps?
  # 10. Lock nodes and groups (groups doesn't have this in stock comfyui) to
  #     prevent moving.
  # 11. Math/eval expressions as a node.
  # 12. Node finder.
  # 13. Preset text - save and reuse text.
  # 14. Play sound node - great for notification of completion!
  # 15. Repeaters.
  # 16. Show text (can be good for loading images and getting the prompt text
  #     out).
  # 17. Show image on menu.
  # 18. String (replace) function - Substitution via regular expression or exact
  #     match.
  # 19. Save and load workflows (already in stock?).
  # 20. 90º reroutes...?
  # 21. Link render mode - linear, spline, straight.
  #
  comfyui-custom-scripts = mkComfyUICustomNodes (let
    pysssss-config = (writeTextFile {
      name = "pysssss.json";
      text = (lib.generators.toJSON {} comfyui-custom-scripts-data);
    });
  in {
    pname = "comfyui-custom-scripts";
    version = "unstable-2024-04-07";
    src = fetchFromGitHub {
      owner = "pythongosssss";
      repo = "ComfyUI-Custom-Scripts";
      rev = "3f2c021e50be2fed3c9d1552ee8dcaae06ad1fe5";
      hash = "sha256-Kc0zqPyZESdIQ+umepLkQ/GyUq6fev0c/Z7yyTew5dY=";
    };
    installPhase = ''
      runHook preInstall
      mkdir -p $out/
      cp -r $src/* $out/
      cp ${pysssss-config} $out/pysssss.json
      mkdir -p $out/user
      chmod +w $out/user
      cp ${comfyui-custom-scripts-autocomplete-text} $out/user/autocomplete.txt
      chmod -w $out/user
      # Copy the patched version separately.  See
      # https://discourse.nixos.org/t/solved-how-to-apply-a-patch-in-a-flake/27227/4
      # for reference.  Perhaps a better reference exists?
      # But this doesn't work for reasons I can't understand.  I get permission
      # denied.
      # cp pysssss.py $out/
      # It seems that I need to grant myself write permissions first.  Is any of
      # this documented anywhere?
      chmod -R +w $out
      cp pysssss.py $out/
      cp __init__.py $out/
      # Put it back I guess?
      chmod -R -w $out/
      runHook postInstall
    '';
    # TODO: Determine if this is actually necessary.
    patches = [
      ./comfyui-custom-scripts-remove-js-install-step.patch
    ];
  });

  # https://github.com/LEv145/images-grid-comfy-plugin
  images-grid-comfy-plugin = mkComfyUICustomNodes (let
    version = "2.6";
  in {
    pname = "images-grid-comfy-plugin";
    inherit version;
    src = fetchFromGitHub {
      owner = "LEv145";
      repo = "images-grid-comfy-plugin";
      # Space character is deliberate.
      rev = "refs/tags/${version}";
      hash = "sha256-YG08pF6Z44y/gcS9MrCD/X6KqG99ig+VKLfZOd49w9s=";
    };
  });

  # More to add:
  # https://github.com/pythongosssss/ComfyUI-WD14-Tagger - Reverse image
  # inference - generate keywords (or a prompt of sorts) from an image.
}
