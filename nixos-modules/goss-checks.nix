################################################################################
# Site-specific goss health checks contributed to services.goss.checks.
#
# These checks are active on any host that imports this file and has "goss"
# listed in its facts.network.hosts monitors, which enables the goss service
# via nixos-modules/goss.nix.
################################################################################
{ ... }:
{
  # Verify the Nix store is mounted read-only.  A writable /nix/store allows
  # any root process to silently corrupt store paths; the ro bind mount is a
  # system integrity guarantee that must never be relaxed.
  services.goss.checks.mount."/nix/store" = {
    exists = true;
    opts = [ "ro" ];
  };
}
