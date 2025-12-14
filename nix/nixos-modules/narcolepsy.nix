################################################################################
# Prevent this host from going to sleep / suspending for any reason.
#
# Sometimes, you just have to be really insistent about this, I guess.  When
# enabling any sort of desktop functionality, this finds its way in.  This
# should be part of any server host.
#
# This will include services that potentially aren't in use.  We just really
# want to cover all of our bases - now and in the future.
################################################################################
{ lib, options, pkgs, ... }: let
  inherit (lib) hasAttrByPath mkIf mkMerge optional setAttrByPath;
  inherit (lib.versions) version;
  inherit (lib.strings) versionAtLeast;
  # TODO: Move these to shared functions.
  nixpkgsVersion =
    if lib ? version then lib.version
    else if pkgs ? lib && pkgs.lib ? version then pkgs.lib.version
    else if pkgs ? version then pkgs.version
    else throw "Cannot determine nixpkgs version";
in {
  config = mkMerge [
    (if versionAtLeast (lib.traceVal nixpkgsVersion) "25.11"
      # TODO: I lost the real path for the new way of doing it, and I don't have
      # a host which warns about this on hand.
     then { services.displayManager.gdm.autoSuspend = false; }
     else { services.xserver.displayManager.gdm.autoSuspend = false; }
    )
    {
      # Disable the GNOME3/GDM auto-suspend feature that cannot be disabled in
      # GUI!  Without this, if no user is logged in, the machine will power down
      # after 20 minutes.
      systemd.targets.sleep.enable = false;
      systemd.targets.suspend.enable = false;
      systemd.targets.hibernate.enable = false;
      systemd.targets.hybrid-sleep.enable = false;
      # A tip from @ryantm:
      # https://discourse.nixos.org/t/why-is-my-new-nixos-install-suspending/19500/2
      # This has some troubleshooting and potential extra context on the "bug":
      # https://github.com/NixOS/nixpkgs/issues/100390
      security.polkit.extraConfig = ''
        polkit.addRule(function(action, subject) {
          if (
            action.id == "org.freedesktop.login1.suspend" ||
            action.id == "org.freedesktop.login1.suspend-multiple-sessions" ||
            action.id == "org.freedesktop.login1.hibernate" ||
           action.id == "org.freedesktop.login1.hibernate-multiple-sessions"
          ) {
            return polkit.Result.NO;
          }
        });
      '';
      # This has more suggestions to build upon the polkit configuration above
      # (it's the same thread):
      # https://discourse.nixos.org/t/why-is-my-new-nixos-install-suspending/19500/6
      # This is kept for reference though, because it implies the use of home
      # manager and requires you log into a GUI session for it to take effect.
      # programs.dconf.profiles.gdm.databases = lib.optionals (!cfg.gdm.autoSuspend) [{
      # settings."org/gnome/settings-daemon/plugins/power" = {
      #   sleep-inactive-ac-type = "nothing";
      #   sleep-inactive-battery-type = "nothing";
      #   sleep-inactive-ac-timeout = lib.gvariant.mkInt32 0;
      #   sleep-inactive-battery-timeout = lib.gvariant.mkInt32 0;
      # };
    }
  ];
}
