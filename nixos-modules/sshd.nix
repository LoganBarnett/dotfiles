################################################################################
# Enables sshd so this server will accept SSH connections.  Includes some
# typical security measures.
#
# `PasswordAuthentication` and `PermitRootLogin` can be overridden with
# `lib.mkForce true` or an appropriately weighted `lib.mkOverride`.  This is to
# allow installers to leverage this module as well.
################################################################################
{ lib, ... }: {
  # This setups a SSH server.
  services.openssh = {
    enable = true;
    settings = {
      # Forbid root login through SSH.  This gets turned to "yes" for
      # installers, which is a heavy assumption made by all of the installation
      # utilities.
      PermitRootLogin = lib.mkDefault "no";
      # Use keys only. Remove if you want to SSH using password (not
      # recommended).  Use mkDefault so it can be overridden by the installer to
      # help bootstrap the system.
      PasswordAuthentication = lib.mkDefault false;
    };
  };
}
