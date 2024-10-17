################################################################################
# Installs necesssary NVidia drivers.  Can be used for games or CUDA
# computation.  Both will be supported here.
#
# This seems to be the most up to date version of anything resembling a cohesive
# document for CUDA + Torch + Nix:
# https://discourse.nixos.org/t/on-nixpkgs-and-the-ai-follow-up-to-2023-nix-developer-dialogues/37087#binary-cache-3
# Not all of the configuration here comes from that, but I am starting to
# integrate it where I can.
################################################################################
{
  # Bus ID of the NVIDIA GPU. You can find it using lspci, either under 3D or
  # VGA.  Examples in the wild will show "PCI:1:0:0" but that's not the actual
  # output of lspci.  `sudo lshw display` will show a terse and closer ID to
  # what we need.
  bus-id,
  # Use https://en.wikipedia.org/wiki/Pascal_(microarchitecture) and
  # https://en.wikipedia.org/wiki/CUDA#GPUs_supported to determine the values
  # for this.
  cudaCapabilities,
  flake-inputs,
}: { lib, pkgs, ... }: let
  linux-packages = pkgs.linuxPackages_latest;
in {
  allowUnfreePackagePredicates = [
    # Zounds!
    (pkg: builtins.elem (lib.getName pkg) [
      "cuda_cccl"
      "cuda_cudart"
      "cuda_cuobjdump"
      "cuda_cupti"
      "cuda_cuxxfilt"
      "cuda_gdb"
      "cuda-merged"
      "cuda_nvcc"
      "cuda_nvdisasm"
      "cuda_nvprune"
      "cuda_nvml_dev"
      "cuda_nvrtc"
      "cuda_nvtx"
      "cuda_profiler_api"
      "cuda_sanitizer_api"
      "cudnn"
      "libcublas"
      "libcufft"
      "libcurand"
      "libcusolver"
      "libcusparse"
      "libnpp"
      "libnvjitlink"
      "nvidia-settings"
      "nvidia-x11"
    ])
  ];
  # See https://nixos.wiki/wiki/Linux_kernel for values and options.
  boot.kernelPackages = linux-packages;
  boot.kernelModules = [
    "nvidia_uvm"
  ];
  # The open source module doesn't support my card.  See
  # https://github.com/NVIDIA/open-gpu-kernel-modules for supported cards.  For
  # some reason, `lshw -c display` may show `nouveau` for the display driver,
  # even though I have `hardware.nvidia.open = false`.  That said, this can
  # happen because a reboot wasn't done.  See
  # https://github.com/NixOS/nixpkgs/issues/16711 someone who had the same
  # problem on a much older NixOS.
  boot.blacklistedKernelModules = ["nouveau"];
  environment.systemPackages = [
    # Show us library paths used so we can see where errant libcuda
    # installations may be.  It's not in by this package though.
    # pkgs.ldd
    # Allow us to get the PCI Bus ID for the graphics card.  This will render a
    # litle differently than lspci, and nvidiaBusId demands a specific format
    # that's closer to what lshw puts out.  There might be a flag that would fix
    # it, but I've yet to find it.
    pkgs.lshw
    # The wiki is also wrong about this package - it doesn't exist.
    # pkgs.nvidia-persistenced
    # pkgs.nvidia-settings
    # This will also bring in `nvidia-smi` which can be helpful for seeing if
    # the kernel driver and other things are compatible.  But beware mismatches
    # happening from lack of restarts per:
    # https://github.com/NixOS/nixpkgs/issues/255070
    # Actually this doesn't bring in nvidia-smi and it can cause version
    # conflicts between the kernel driver and the library!
    # linux-packages.nvidia_x11
    # Includes `lspci`.  `lspci` gets more information about the PCI bus.
    # Useful for debugging issues with the driver not picking up the hardware.
    pkgs.pciutils
    # saxpy can is the best "doctor" tool for version mismatches.  Example
    # output of a version mismatch:
    # [logan@lithium:~]$ saxpy
    # Start
    # Runtime version: 12020
    # Driver version: 12050
    # Host memory initialized, copying to the device
    # Scheduled a cudaMemcpy, calling the kernel
    # Scheduled a kernel call
    # Max error: 0.000000
    pkgs.cudaPackages.saxpy
    # If encountering "RuntimeError: No CUDA GPUs are available", use this to
    # debug:
    # LD_DEBUG=libs python -c "import torch ; torch.cuda.is_available()"
    # Thus we need Python.
    # pkgs.python3.withPackages (ps: [
    #   ps.torch-bin
    # ]).env
    # Actually that doesn't work.  You have to enter a shell manually:
    # nix-shell -p 'python3.withPackages (ps: [ps.torch-bin])'
    # That doesn't work, but this looks promising and is quick:
    # [logan@lithium:~]$ nvidia-smi
    # NVIDIA-SMI has failed because it couldn't communicate with the NVIDIA
    # driver. Make sure that the latest NVIDIA driver is installed and running.
    # Use `modinfo nvidia | grep -i version` to determine what the kernel
    # version is.  `ls /run/opengl-driver/lib/libnvidia-nvvm.so.*` will show
    # what version the libraries are.  There are a lot of other files in that
    # directory with the version number, in case this vanishes in a future
    # configuration.
  ];
  # I'd like to better understand what these are doing.
  imports = [
    flake-inputs.nixos-hardware.nixosModules.common-pc
    flake-inputs.nixos-hardware.nixosModules.common-pc-ssd
    flake-inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
    flake-inputs.nixos-hardware.nixosModules.common-gpu-nvidia-nonprime
    # Gives us allowUnfreePackagePredicates.
    ./unfree-predicates.nix
  ];
  hardware.graphics = {
    enable = true;
  };
  hardware.graphics.package = linux-packages.nvidiaPackages.beta;
  # Most of this can be found here:
  # https://nixos.wiki/wiki/Nvidia#Nvidia_PRIME
  hardware.nvidia = {
    package = linux-packages.nvidiaPackages.beta;
    # Actually this expression is broken.  The wiki is wrong?  Error:
    # error: attribute 'boot' missing
    # package = config.boot.kernelPackages.nvidiaPackages.production;
    # Modesetting is required.
    # Enable to fix screen tearing
    modesetting.enable = true;
    # Use the NVidia open source kernel module (not to be confused with the
    # independent third-party "nouveau" open source driver).
    # Support is limited to the Turing and later architectures. Full list of
    # supported GPUs is at:
    # https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus
    # Only available from driver 515.43.04+
    # Currently alpha-quality/buggy, so false is currently the recommended
    # setting.
    open = false;
    # Enable the Nvidia settings menu,
    # accessible via `nvidia-settings`.
    nvidiaSettings = true;
    # Prime is setup with offload
    # Taken from: https://wiki.archlinux.org/title/Hybrid_graphics
    # Hybrid-graphics is a concept involving two graphics cards on same
    # computer. Laptop manufacturers have developed technologies involving
    # two graphic cards with different abilities and power consumption on a
    # single computer.
    # prime.offload.enable = true;
    # Bus ID of the NVIDIA GPU. You can find it using lspci, either under 3D
    # or VGA.  Examples in the wild will show "PCI:1:0:0" but that's not the
    # actual output of lspci.  `sudo lshw display` will show a terse and closer
    # ID to what we need.
    prime.nvidiaBusId = bus-id;
  };
  # One can arrive at compiler errors like this:
  # ...due to signal 11 (invalid memory reference)
  #
  # This indicates a lack of configuration on this NixOS module.  See
  # https://github.com/NixOS/nixpkgs/issues/248242 for the specifics that need
  # to be added.
  #
  # It could also mean the drivers don't match the libraries.  `nvidia-smi` can
  # help determine if that's the case (a failure meaning it is a mismatch).
  nixpkgs.config = {
    # TODO: Use this to bless specific packages.
    # nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    #   "roon-server"
    #   "vscode"
    # ];
    # allowUnfree = true;
    nvidia.acceptLicense = true;
    # cudaCapabilities = [ "8.6" ];
    # Use https://en.wikipedia.org/wiki/Pascal_(microarchitecture) and
    # https://en.wikipedia.org/wiki/CUDA#GPUs_supported to determine the values
    # for this.
    inherit cudaCapabilities;
    # Compilation problems from this can arise from driver issues.  Run tools
    # like nvidia-smi to figure out what's wrong, fix, and then enable this
    # again.
    cudaSupport = true;
    extra-substituters = [
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
  services.comfyui.cudaSupport = true;
  services.comfyui.package = pkgs.comfyui-cuda;
  services.xserver.videoDrivers = [ "nvidia" ];
}
