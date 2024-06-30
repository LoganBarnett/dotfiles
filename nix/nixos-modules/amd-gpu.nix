# For using AMD GPUs for either gaming or compute.
# See https://nixos.wiki/wiki/AMD_GPU for a guide.
# Unfortunately the guide doesn't speak much as to _when_ you'd want to turn on
# these various settings.  That's an exercise left the reader.
{ pkgs, ... }: {
  boot.initrd.kernelModules = [ "amdgpu" ];
  environment.systemPackages = [
    # This provides the vulkaninfo tool, which can be used to diagnose issues
    # with the drivers.
    pkgs.vulkan-tools
  ];
  # This is implied that we should turn this on via:
  # https://nixos.org/manual/nixos/unstable/index.html#sec-gpu-accel-vulkan
  # Looks like it's wrong though.  This doesn't exist.  Could be that I'm on
  # master and not unstable.
  # hardware.graphics.enable = true;
  # Vulkan is already enabled by default (using Mesa RADV) on 64 bit
  # applications.
  hardware.opengl.driSupport = true; # This is already enabled by default.
  hardware.opengl.driSupport32Bit = true; # For 32 bit applications.
  # The AMDVLK drivers can be used in addition to the Mesa RADV drivers. The
  # program will choose which one to use.
  hardware.opengl.extraPackages = [
    pkgs.amdvlk
  ];
  # For 32 bit applications.
  hardware.opengl.extraPackages32 = [
    pkgs.driversi686Linux.amdvlk
  ];
  services.xserver.enable = true;
  services.xserver.videoDrivers = [ "amdgpu" ];
}
