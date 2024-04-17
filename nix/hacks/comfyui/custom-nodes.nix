{
  fetchFromGitHub
, stdenv
, lib
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
  # This saves things here:
  # <comfyui-dir>/custom_nodes/comfyui-browser/collections
  # Perhaps there is something we can do to help Nix manage this further?  Or a
  # setting that can "unmanage" it.
  #
  # TODO: This requires the <comfyui-browser>/collections dir to exist.  Can we
  # override it and place it elsewhere in the comfyui directory?  Even if we
  # created it as part of this package, it would be read-only and thus useless.
  comfyui-browser = mkComfyUICustomNodes {
    pname = "comfyui-browser";
    version = "unstable-2024-04-07";
    src = fetchFromGitHub {
      owner = "talesofai";
      repo = "comfyui-browser";
      rev = "0a39d125cbf182154d940e7f4f930ac3c0bf3e2e";
      hash = "sha256-096x+n9TKRzG+sfbZJG6stMEmk7KFxZsGCU0TLlw+6s=";
    };
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
  # TODO: This requires a "pysssss.json" to be laid down, but this necessitates
  # writing to the nix store and that doesn't work.  The directory does have an
  # example.  How can we specify this?
  comfyui-custom-scripts = mkComfyUICustomNodes {
    pname = "comfyui-custom-scripts";
    version = "unstable-2024-04-07";
    src = fetchFromGitHub {
      owner = "pythongosssss";
      repo = "ComfyUI-Custom-Scripts";
      rev = "3f2c021e50be2fed3c9d1552ee8dcaae06ad1fe5";
      hash = "sha256-Kc0zqPyZESdIQ+umepLkQ/GyUq6fev0c/Z7yyTew5dY=";
    };
  };

  # More to add:
  # https://github.com/pythongosssss/ComfyUI-WD14-Tagger - Reverse image
  # inference - generate keywords (or a prompt of sorts) from an image.
}
