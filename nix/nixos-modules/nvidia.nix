################################################################################
# Installs necesssary NVidia drivers.  Can be used for games or CUDA
# computation.  Both will be supported here.
################################################################################
{ nixos-hardware, pkgs, ... }: let
  linux-packages = pkgs.linuxPackages_latest;
in {
  # See https://nixos.wiki/wiki/Linux_kernel for values and options.
  boot.kernelPackages = linux-packages;
  boot.kernelModules = [
    "nvidia_uvm"
  ];
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
    # Somehow this executable is available to some folks for diagnostic
    # purposes, but this package apparently doesn't exist.
    # pkgs.nvidia-smi
  ];
  # I'd like to better understand what these are doing.
  imports = [
    nixos-hardware.nixosModules.common-pc
    nixos-hardware.nixosModules.common-pc-ssd
    nixos-hardware.nixosModules.common-cpu-amd-pstate
    nixos-hardware.nixosModules.common-gpu-nvidia-nonprime
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
  # https://github.com/NixOS/nixpkgs/issues/248242 for the specifics that need to
  # be added.
  nixpkgs.config = {
    # TODO: Use this to bless specific packages.
    # nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    #   "roon-server"
    #   "vscode"
    # ];
    allowUnfree = true;
    nvidia.acceptLicense = true;
    cudaCapabilities = [ "8.6" ];
    cudaSupport = true;
  };
  services.xserver.videoDrivers = [ "nvidia" ];
}
