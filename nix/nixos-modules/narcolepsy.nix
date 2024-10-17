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
{ ... }: {
  # Disable the GNOME3/GDM auto-suspend feature that cannot be disabled in GUI!
  # Without this, if no user is logged in, the machine will power down after 20
  # minutes.
  systemd.targets.sleep.enable = false;
  systemd.targets.suspend.enable = false;
  systemd.targets.hibernate.enable = false;
  systemd.targets.hybrid-sleep.enable = false;
  # Don't suspend the machine from idleness.
  services.xserver.displayManager.gdm.autoSuspend = false;
  # A tip from @ryantm:
  # https://discourse.nixos.org/t/why-is-my-new-nixos-install-suspending/19500/2
  # This has some troubleshooting and potential extra context on the "bug":
  # https://github.com/NixOS/nixpkgs/issues/100390
  security.polkit.extraConfig = ''
    polkit.addRule(function(action, subject) {
        if (action.id == "org.freedesktop.login1.suspend" ||
            action.id == "org.freedesktop.login1.suspend-multiple-sessions" ||
            action.id == "org.freedesktop.login1.hibernate" ||
            action.id == "org.freedesktop.login1.hibernate-multiple-sessions")
        {
            return polkit.Result.NO;
        }
    });
  '';
  # This has more suggestions to build upon the polkit configuration above (it's
  # the same thread):
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
