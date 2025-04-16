################################################################################
# Set a bed time for gamer kids.  Boot them out of the x-session at a chosen
# time.
################################################################################
{ pkgs, facts, lib, ... }: let
  users = facts.network.groups.screen-addicts.members;
  users-string = lib.strings.concatStringsSep " " users;
in {
  systemd.timers.bedtime-logout = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "Mon..Fri 20:00 PT, Sat..Sun 21:00 PT";
    };
  };
  systemd.services.bedtime-logout = {
    enable = true;
    serviceConfig = {
      ExecStart = let
        script = pkgs.writeShellApplication {
          name = "user-logout-and-lockout";
          text = builtins.readFile ../scripts/user-logout-and-lockout.sh;
        };
      in ''
        ${script}/bin/user-logout-and-lockout.sh ${users-string}
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
      OnCalendar = "Mon..Fri 07:00 PT, Sat..Sun 07:00 PT";
    };
  };
  systemd.services.daytime-enable = {
    enable = true;
    serviceConfig = {
      ExecStart = ''
        users=(${users-string})
        for user in "''${users[@]}"; do
          usermod --expiredate -1 $user
        done
      '';
      # This needs to kill other users' processes, which is basically just a
      # root thing.
      User = "root";
      Type = "oneshot";
    };
  };
}
