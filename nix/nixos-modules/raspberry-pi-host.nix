################################################################################
# This is some boilerplate for Raspberry Pi hosts.
#
# We can make some assumptions about building Raspberry Pis since we can create
# the entire bootable image from scratch, and we don't need to deal with UEFI
# nonsense.
################################################################################
{ flake-inputs, host-id, lib, ... }: {
  imports = [
  ];
  # Disable swap devices for these.  They have little SD cards with abysmal
  # write speeds.  Perhaps if we have a permanently connected disk with some
  # speed to it, we can change this up.  We can also destroy the SD cards
  # quickly with a swap device.
  swapDevices = lib.mkForce [];
  # By default, Raspberry Pis are disallowed from doing their own builds.  We
  # have a single Raspberry Pi build host - a host with sufficient memory and no
  # other services on it.  This prevents any local builds from occurring.
  nix.settings.max-jobs = 0;
}
