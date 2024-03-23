{ config, lib, options, pkgs, ...}:

with lib;

let
  cfg = config.services.comfyui;
  defaultUser = "comfyui";
  defaultGroup = defaultUser;
  mkComfyUIPackage = cfg: cfg.package.override {
    modelsPath = "${cfg.dataPath}/models";
    inputPath = "${cfg.dataPath}/input";
    outputPath = "${cfg.dataPath}/output";
    customNodes = cfg.customNodes;
    checkpoints = cfg.models.checkpoints;
    loras = cfg.models.loras;
    vae = cfg.models.vae;
  };
in
{
  options = {
    services.comfyui = {
      enable = mkEnableOption
        (mdDoc "The most powerful and modular stable diffusion GUI with a graph/nodes interface.");

      dataPath = mkOption {
        type = types.str;
        default = "/var/lib/comfyui";
        description = mdDoc "path to the folders which stores models, custom nodes, input and output files";
      };

      cudaSupport = mkOption {
        type = types.bool;
        default = false;
        description = mdDoc "Whether or not to enable CUDA for NVidia GPU acceleration.";
        defaultText = literalExpression "false";
        example = literalExpression "true";
      };

      rocmSupport = mkOption {
        type = types.bool;
        default = false;
        description = mdDoc "Whether or not to enable ROCM for ATi GPU acceleration.";
        defaultText = literalExpression "false";
        example = literalExpression "true";
      };

      package = mkOption {
        type = types.package;
        default = (
          if config.cudaSupport
          then pkgs.comfyui-cuda
          else if config.rocmSupport
          then pkgs.comfyui-rocm
          else pkgs.comfyui
        );
        defaultText = literalExpression "pkgs.comfyui";
        example = literalExpression "pkgs.comfyui-rocm";
        description = mdDoc "ComfyUI base package to use";
      };

      user = mkOption {
        type = types.str;
        default = defaultUser;
        example = "yourUser";
        description = mdDoc ''
          The user to run ComfyUI as.
          By default, a user named `${defaultUser}` will be created whose home
          directory will contain input, output, custom nodes and models.
        '';
      };

      group = mkOption {
        type = types.str;
        default = defaultGroup;
        example = "yourGroup";
        description = mdDoc ''
          The group to run ComfyUI as.
          By default, a group named `${defaultUser}` will be created.
        '';
      };

      useCPU = mkOption {
        type = types.bool;
        default = false;
        description = mdDoc ''
          Uses the CPU for everything. Very slow, but needed if there is no hardware acceleration.
        '';
      };

      port = mkOption {
        type = types.int;
        default = 8188;
        description = mdDoc "Set the listen port for the Web UI and API.";
      };

      customNodes = mkOption {
        type = types.listOf types.package;
        default = [];
        description = mdDoc "custom nodes to add to the ComfyUI setup. Expects a list of packages from pkgs.comfyui-custom-nodes";
      };

      # Argument dump:
      #  usage: comfyui [-h] [--listen [IP]] [--port PORT]
      #                 [--enable-cors-header [ORIGIN]]
      #                 [--max-upload-size MAX_UPLOAD_SIZE]
      #                 [--extra-model-paths-config PATH [PATH ...]]
      #                 [--output-directory OUTPUT_DIRECTORY]
      #                 [--temp-directory TEMP_DIRECTORY]
      #                 [--input-directory INPUT_DIRECTORY] [--auto-launch]
      #                 [--disable-auto-launch] [--cuda-device DEVICE_ID]
      #                 [--cuda-malloc | --disable-cuda-malloc]
      #                 [--dont-upcast-attention] [--force-fp32 | --force-fp16]
      #                 [--bf16-unet | --fp16-unet | --fp8_e4m3fn-unet | --fp8_e5m2-unet]
      #                 [--fp16-vae | --fp32-vae | --bf16-vae] [--cpu-vae]
      #                 [--fp8_e4m3fn-text-enc | --fp8_e5m2-text-enc | --fp16-text-enc | --fp32-text-enc]
      #                 [--directml [DIRECTML_DEVICE]] [--disable-ipex-optimize]
      #                 [--preview-method [none,auto,latent2rgb,taesd]]
      #                 [--use-split-cross-attention | --use-quad-cross-attention | --use-pytorch-cross-attention]
      #                 [--disable-xformers]
      #                 [--gpu-only | --highvram | --normalvram | --lowvram | --novram | --cpu]
      #                 [--disable-smart-memory] [--deterministic]
      #                 [--dont-print-server] [--quick-test-for-ci]
      #                 [--windows-standalone-build] [--disable-metadata]
      #                 [--multi-user] [--verbose]
      extraArgs = mkOption {
        type = types.str;
        default = "";
        example = "--preview-method auto";
        description = mdDoc ''
          Additional arguments to be passed to comfyui
        '';
      };

      # TODO: Validate that the name of the package has extensions expected for
      # file depending on where it's going.  Without the extension, comfyui
      # won't find the file (or know how to treat the file), and a rename will
      # have to be done, potentially triggering a very expensive re-download.
      models = mkOption {
        # TODO: See if we can make this tighter.
        # type = types.attrsOf {
        # type = types.lazyAttrsOf {
        # type = types.attrsOf (types.submodule ({ config, ... }: {
        #   options = {
        #     checkpoints = mkOption {
        #       type = types.listOf types.package;
        #     };
        #   };
        # }));
        default = {
          checkpoints = [];
          clip = [];
          clip_vision = [];
          configs = [];
          controlnet = [];
          embeddings = [];
          loras = [];
          upscale_modules = [];
          vae = [];
          vae_approx = [];
        };
      };
    };
  };

  config = mkIf cfg.enable {
    users.users = mkIf (cfg.user == defaultUser) {
      ${defaultUser} =
        { group = cfg.group;
          home  = cfg.dataPath;
          createHome = true;
          description = "ComfyUI daemon user";
          isSystemUser = true;
        };
    };

    users.groups = mkIf (cfg.group == defaultGroup) {
      ${defaultGroup} = {};
    };

    systemd.services.comfyui = {
      description = "ComfyUI Service";
      wantedBy = [ "multi-user.target" ];
      environment = {
        DATA = cfg.dataPath;
      };

      preStart = let
  linkModels = name: models: path:
    # symlinkJoin doesn't work because the models aren't directories, and
    # symlinkJoin requires directories only.  No advice has been given on how to
    # nest/wrap to overcome this.
    # symlinkJoin {
    #   name = "comfyui-models-${name}";
    #   meta.mainProgram = "comfyui";
    #   paths = models;
    # }
    # lib.strings.concatMapStrings (model: "ln -s ${model} $out${config.comfyui.${name}};") models
    # lib.strings.concatMapStrings (model: "ln -s ${lib.debug.traceVal model} $out${config.comfyui.${name}};") models
    lib.strings.concatMapStrings (model:
      "mkdir -p ${path}/${name} ; ln -snf ${model}  ${path}/${name}/ ;"
    ) models
      ;
      in ''
        mkdir -p $DATA/input
        mkdir -p $DATA/output
        mkdir -p $DATA/custom_nodes
        mkdir -p $DATA/models
        ln -snf ${cfg.package}/extra_model_paths.yaml $DATA/extra_model_paths.yaml
        ${linkModels "checkpoints" cfg.models.checkpoints "${cfg.dataPath}/models"}
        ${linkModels "clip" cfg.models.clip "${cfg.dataPath}/models"}
        ${linkModels "clip_vision" cfg.models.clip_vision "${cfg.dataPath}/models"}
        ${linkModels "configs" cfg.models.configs "${cfg.dataPath}/models"}
        ${linkModels "controlnet" cfg.models.controlnet "${cfg.dataPath}/models"}
        ${linkModels "embeddings" cfg.models.embeddings "${cfg.dataPath}/models"}
        ${linkModels "loras" cfg.models.loras "${cfg.dataPath}/models"}
        ${linkModels "vae" cfg.models.vae "${cfg.dataPath}/models"}
        ${linkModels "vae_approx" cfg.models.vae_approx "${cfg.dataPath}/models"}
        ${linkModels "upscale_modules" cfg.models.upscale_modules "${cfg.dataPath}/models"}
      '';

      serviceConfig = let
        # This should be hoisted higher and applied elsewhere.
        name = "comfyui";
      in {
        User = cfg.user;
        Group = cfg.group;
        # These directories must be relative to /var/lib.  Absolute paths are
        # greeted with:
        #  <path-to-unit>: StateDirectory= path is absolute, ignoring: <abs-path>
        RuntimeDirectory = [ name ];
        StateDirectory = [ name ];
        WorkingDirectory = "/run/${name}";
        ExecStart = let
          args = cli.toGNUCommandLine {} {
            cpu = cfg.useCPU;
            port = cfg.port;
          };
        in ''
          ${mkComfyUIPackage cfg}/bin/comfyui ${toString args} ${cfg.extraArgs}
        '';
        # TODO: Figure out what to do with dataPath, since it isn't used here
        # anymore.
        # StateDirectory = cfg.dataPath;
        # comfyui is prone to crashing on long slow workloads.
        Restart = "always";
        # Prevent it from restarting _too_ much though.  Stop if three times a
        # minute.  This might need a better default from someone with better
        # sysadmin chops.
        StartLimitIntervalSec = "1m";
        StartLimitBurst = 3;
      };
    };
  };
}
