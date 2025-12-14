# For using AMD GPUs for graphics, gaming, or compute.
# See https://wiki.nixos.org/wiki/AMD_GPU for a guide.
# Unfortunately the guide doesn't speak much as to _when_ you'd want to turn on
# these various settings, so for which applications these settings are needed is
# less understood.  That's an exercise left the reader.
{ lib, options, pkgs, ... }: {
  boot.initrd.kernelModules = [ "amdgpu" ];
  # Some programs hard-code the path to HIP.
  # systemd.tmpfiles.rules = ["L+ /opt/rocm/hip - - - - ${pkgs.rocmPackages.clr}"];
  systemd.tmpfiles.rules = let
    rocmEnv = pkgs.symlinkJoin {
      name = "rocm-combined";
      paths = [
        pkgs.rocmPackages.rocblas
        pkgs.rocmPackages.hipblas
        pkgs.rocmPackages.clr
      ];
    };
  in [
    "L+    /opt/rocm   -    -    -     -    ${rocmEnv}"
  ];
  environment.systemPackages = [
    # More diagnostics.
    pkgs.clinfo
    # Shows information about the GPU.  Equivalent to nvidia-smi.
    pkgs.rocmPackages.rocm-smi
    # More diagnostics.
    pkgs.rocmPackages.rocminfo
    # This provides the vulkaninfo tool, which can be used to diagnose issues
    # with the drivers.
    pkgs.vulkan-tools
  ];
  environment.variables = {
    # Force radv.  Why?
    # AMD_VULKAN_ICD = "RADV";
    # Or this for some reason.  Why?
    VK_ICD_FILENAMES =
      "/run/opengl-driver/share/vulkan/icd.d/radeon_icd.x86_64.json";
  };
  hardware.amdgpu = {
    opencl.enable = true;
    initrd.enable = true;
  };
  # Vulkan is already enabled by default (using Mesa RADV) on 64 bit
  # applications.
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
    extraPackages = [
      pkgs.rocmPackages.clr.icd
    ];
    # For 32 bit applications.
    extraPackages32 = [];
  };
  imports = [
    (lib.mkIf (builtins.hasAttr "comfyui" options.services) {
      # This is kind of magical.  See
      # https://nix.dev/manual/nix/2.17/language/values.html?highlight=coerced#attribute-set
      # but basically if the attribute name evaluates to null then the attribute
      # won't exist.  Without this hack, we get `The option `services.comfyui'
      # does not exist.`.  This is a special case and one cannot use null as a
      # key name.
      services.${
        if (builtins.hasAttr "comfyui" options.services)
        then "comfyui"
        else null
      } = {
        package = pkgs ? comfyui-rocm;
        rocmSupport = true;
      };
    })
  ];
  nixpkgs.config.enableRocm = true;
  nixpkgs.config.rocmSupport = true;
  nixpkgs.overlays = [
    (final: prev: {
      # https://github.com/NixOS/nixpkgs/issues/268736 suggests we can just use
      # `torchWithRocm` to make this work, since `torch` is marked as broken
      # when evaluated with `rocmSupport = true`.  I haven't been able to get
      # this to work yet though.
      pythonPackagesExtensions = [(py-final: py-prev: {
        transformers = py-prev.transformers.override {
          torch = py-prev.torchWithRocm;
        };
        safetensors = py-prev.safetensors.override {
          torch = py-prev.torchWithRocm;
        };
        accelerate = py-prev.accelerate.override {
          torch = py-prev.torchWithRocm;
        };
        torchaudio = py-prev.torchaudio.override {
          torch = py-prev.torchWithRocm;
        };
      })];
      # python3Packages = prev.python3Packages.override (py-final: py-prev: {
      #   torch = py-prev.torchWithRocm;
      # });
      # python3 = prev.python3.override {
      #   packageOverrides = (py-final: py-prev: {
      #     torch = py-prev.torchWithRocm;
      #   });
      # };
      # python311 = prev.python311.override {
      #   packageOverrides = (py-final: py-prev: {
      #     torch = py-prev.torchWithRocm;
      #   });
      # };
    })
  ];
  services.xserver.enable = true;
  # This says the wiki is wrong:
  # https://discourse.nixos.org/t/amd-rx-7700-xt-not-being-detected-properly/33683/2
  # Do not use "amdgpu" here, use "modesetting".  Why?
  services.xserver.videoDrivers = [ "amdgpu" "modesetting" ];
}
