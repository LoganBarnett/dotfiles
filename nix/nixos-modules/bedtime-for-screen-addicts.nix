################################################################################
# Set a bed time for gamer kids.  Boot them out of the x-session at a chosen
# time.
################################################################################
{ config, facts, lib, pkgs, ... }: let
  # Only include users on this system.
  users = lib.lists.filter
    (user: (builtins.elem user (builtins.attrNames config.users.users)))
    facts.network.groups.screen-addicts.members
  ;
  users-string = lib.strings.concatStringsSep " " users;
in {
  systemd.timers.bedtime-logout = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      Persistent = true;
      # This expression doesn't work, partly because of the PT but the rest is
      # untested as well.  We should try this out at some point.
      # OnCalendar = "Mon..Fri 20:00 PT, Sat..Sun 21:00 PT";
      OnCalendar = "*-*-* 20:00";
    };
  };
  systemd.services.bedtime-logout = {
    enable = true;
    serviceConfig = {
      ExecStart = let
        script = pkgs.writeShellApplication {
          name = "user-logout-and-lockout";
          text = builtins.readFile ../scripts/user-logout-and-lockout.sh;
          runtimeInputs = [
            # For `usermod`.
            pkgs.shadow
            # For pkill.
            pkgs.procps
          ];
        };
      in ''
        ${script}/bin/user-logout-and-lockout ${users-string}
      '';
      # This needs to kill other users' processes, which is basically just a
      # root thing.
      User = "root";
      Type = "oneshot";
    };
  };
  systemd.timers.daytime-enable = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      Persistent = true;
      OnCalendar = "*-*-* 07:00";
    };
  };
  systemd.services.daytime-enable = {
    enable = true;
    serviceConfig = {
      ExecStart = ''
        users=(${users-string})
        for user in "''${users[@]}"; do
          ${pkgs.shadow}/bin/usermod --expiredate -1 $user
        done
      '';
      # This needs to kill other users' processes, which is basically just a
      # root thing.
      User = "root";
      Type = "oneshot";
    };
  };
}
