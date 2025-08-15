################################################################################
# This defines the entirety of the configuration for the cobalt host.
#
# The word "cobalt" comes from the German "Kobold", which is German for
# "goblin".  Cobalt typically contained toxic arsenic and wasn't useful when
# smelted, so minors thought there were mischievous spirits that tainted the
# mineral.  It is one of three ferromagnetic metals at room temperature (along
# with iron and nickel).  It's a key part of vitamin B12.  Cobalt provides a
# stark blue color that was used in ceramics.  It keeps its strength at high
# temperatures, and is also used in battery cathodes.
#
# The cobalt host is our Raspberry Pi build server.  This is something the
# ecosystem desperately needs.  Many Pis are relatively low on memory and so
# have trouble with larger builds before OOM-Killer gets to them.  This
# particular Pi has 64GB of RAM and thus is a suitable build host.  That will be
# its only role, other than perhaps providing Nix build caching via external
# disks (which should take no memory).
################################################################################
{ flake-inputs, lib, nixpkgs, system, ... }: {
  imports = [
    ../nixos-modules/raspberry-pi-5.nix
    ({ config, lib, pkgs, ... }: {
      nixpkgs.hostPlatform = system;
      # Just a guessed value.  This was crushed by
      # `../nixos-modules/raspberry-pi-host.nix`.
      nix.settings.max-jobs = lib.mkForce 4;
    })
    ../nixos-modules/nix-builder-provide.nix
    # Add some extra ARM build architectures in case we need them.
    ../nixos-modules/raspberry-pi-builder.nix
    ../nixos-modules/server-host.nix
  ];
}
