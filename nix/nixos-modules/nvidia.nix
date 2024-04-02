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
{ flake-inputs, pkgs, ... }: let
  linux-packages = pkgs.linuxPackages_latest;
in {
  # See https://nixos.wiki/wiki/Linux_kernel for values and options.
  boot.kernelPackages = linux-packages;
  boot.kernelModules = [
    "nvidia_uvm"
  ];
  # The open source module doesn't support my card.  See
  # https://github.com/NVIDIA/open-gpu-kernel-modules for supported cards.  For
  # some reason, `lshw -c display` shows `nouveau` for the display driver, even
  # though I have `hardware.nvidia.open = false`.  That said, this can happen
  # because a reboot wasn't done.  See
  # https://github.com/NixOS/nixpkgs/issues/16711 someone who had the same
  # problem on a much older NixOS.
  boot.blacklistedKernelModules = ["nouveau"];
  environment.systemPackages = [
    # Allow us to get the PCI Bus ID for the graphics card.  This will render a
    # litle differently than lspci, and nvidiaBusId demands a specific format
    # that's closer to what lshw puts out.  There might be a flag that would fix
    # it, but I've yet to find it.
    pkgs.lshw
    # The wiki is also wrong about this package - it doesn't exist.
    # pkgs.nvidia-persistenced
    # pkgs.nvidia-settings
    linux-packages.nvidia_x11
    # Includes `lspci`.  `lspci` gets more information about the PCI bus.
    # Useful for debugging issues with the driver not picking up the hardware.
    pkgs.pciutils
    # Somehow this executable is available to some folks for diagnostic
    # purposes, but this package apparently doesn't exist.
    # pkgs.nvidia-smi
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
  ];
  # I'd like to better understand what these are doing.
  imports = [
    flake-inputs.nixos-hardware.nixosModules.common-pc
    flake-inputs.nixos-hardware.nixosModules.common-pc-ssd
    flake-inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
    flake-inputs.nixos-hardware.nixosModules.common-gpu-nvidia-nonprime
  ];
  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.driSupport32Bit = true;
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
    # Currently alpha-quality/buggy, so false is currently the recommended setting.
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
    # actual output of lspci.
    prime.nvidiaBusId = "PCI:1:0:0";
  };
  # One can arrive at compiler errors like this:
  # ...due to signal 11 (invalid memory reference)
  #
  # This indicates a lack of configuration on this NixOS module.  See
  # https://github.com/NixOS/nixpkgs/issues/248242 for the specifics that need
  # to be added.
  nixpkgs.config = {
    # TODO: Use this to bless specific packages.
    # nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    #   "roon-server"
    #   "vscode"
    # ];
    allowUnfree = true;
    nvidia.acceptLicense = true;
    # cudaCapabilities = [ "8.6" ];
    # According to https://en.wikipedia.org/wiki/Pascal_(microarchitecture) this
    # only goes to 6.0, and this can cause build problems with magma/ptxas
    # later.  This (https://en.wikipedia.org/wiki/CUDA#GPUs_supported) shows we
    # can go to ... 8.0 or 6.x?  I'm not sure how to read this chart.  The
    # actual name of the chip (from the product level that I understand it - a
    # GTX 1060 6GB) says 6.1.
    cudaCapabilities = [ "6.0" ];
    # Disable until I can figure out what's causing the compilation problems.  I
    # currently get:
    # error: builder for '/nix/store/4q3mh591j2dn6cb817bif8l9z0jyv1bh-magma-2.7.2.drv' failed with exit code 1;
    #  last 10 log lines:
    #  > nvcc warning : incompatible redefinition for option 'compiler-bindir', the last value of this option was used
    #  > nvcc error   : 'ptxas' died due to signal 11 (Invalid memory reference)
    #  > nvcc error   : 'ptxas' core dumped
    #  > [1286/3430] Building CUDA object CMakeFiles/magma.dir/magmablas/dgeqr2_batched_fused_reg_medium.cu.o
    #  > nvcc warning : incompatible redefinition for option 'compiler-bindir', the last value of this option was used
    #  > [1287/3430] Building CUDA object CMakeFiles/magma.dir/magmablas/cgeqr2_batched_fused_reg_medium.cu.o
    #  > nvcc warning : incompatible redefinition for option 'compiler-bindir', the last value of this option was used
    #  > [1288/3430] Building CUDA object CMakeFiles/magma.dir/magmablas/dgeqr2_batched_fused_reg_tall.cu.o
    #  > nvcc warning : incompatible redefinition for option 'compiler-bindir', the last value of this option was used
    #  > ninja: build stopped: subcommand failed.
    #  For full logs, run 'nix log /nix/store/4q3mh591j2dn6cb817bif8l9z0jyv1bh-magma-2.7.2.drv'.
    #
    # The full log is written to ./nvidia-cuda-magma-opencv-fail.log.
    # cudaSupport = false;
    cudaSupport = true;
    extra-substituters = [
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
  services.xserver.videoDrivers = [ "nvidia" ];
}
