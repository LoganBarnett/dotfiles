################################################################################
# A means of scheduling the lock-out of users (from terminal and X-Session).
# Primarily motivated to help fight screen addiction.  This allows tuning, for a
# given user, a usage schedule per day.
################################################################################
{ lib, pkgs, config, ... }:
let
  inherit (lib) mkEnableOption mkOption mkIf types mapAttrsToList filterAttrs
  concatLists;

  weekdays = [
    { key = "mon"; name = "Mon"; }
    { key = "tue"; name = "Tue"; }
    { key = "wed"; name = "Wed"; }
    { key = "thu"; name = "Thu"; }
    { key = "fri"; name = "Fri"; }
    { key = "sat"; name = "Sat"; }
    { key = "sun"; name = "Sun"; }
  ];

  mkOnCalendars = timesByDay:
    concatLists (map (d:
      let times = lib.attrByPath [ d.key ] [] timesByDay;
      in map (t: "${d.name} ${t}") times
    ) weekdays);

  userExists = u: lib.hasAttr u config.users.users;

  logoutScript = ../scripts/user-logout-and-lockout.sh;
  enableScript = ../scripts/user-login-enable.sh;

in {
  options.services.userLockoutSchedule = {

    users = mkOption {
      type = types.attrsOf (types.submodule ({ ... }: {
        options = {
          enable =
            (mkEnableOption "This user's lockout schedule.")
            // { default = true; }
          ;
          schedule = mkOption {
            type = types.submodule {
              options = builtins.listToAttrs (map (day: {
                name = day.key;
                value = mkOption {
                  type = types.submodule {
                    options = {
                      logoutAt = mkOption {
                        type = types.listOf types.str;
                        default = [];
                        description = ''
                          Times to force logout on ${day.name} (HH:MM).
                        '';
                      };
                      enableAt = mkOption {
                        type = types.listOf types.str;
                        default = [];
                        description = ''
                          Times to re-enable login on ${day.name} (HH:MM).
                        '';
                      };
                    };
                  };
                  default = {};
                };
              }) weekdays);
            };
            default = {};
          };
        };
      }));
      default = {};
    };
  };

  config = let
    cfg = config.services.userLockoutSchedule;
  in (
    let
      hostname = config.networking.hostName;

      activeUsers = lib.filterAttrs
        (uName: uCfg: uCfg.enable && lib.hasAttr uName config.users.users)
        cfg.users;

      onCalsLogout = uCfg:
        mkOnCalendars (lib.mapAttrs (_: v: v.logoutAt) uCfg.schedule);
      onCalsEnable = uCfg:
        mkOnCalendars (lib.mapAttrs (_: v: v.enableAt) uCfg.schedule);

      mkLogoutService = uName: uCfg: {
        enable = true;
        description = "Force logout and lock ${uName}";
        path = [
          pkgs.bash
          pkgs.coreutils
          # For `pkill`.
          pkgs.procps
          # For `usermod`.
          pkgs.shadow
        ];
        serviceConfig = {
          # Needs to kill other users' processes, which only root can do.
          User = "root";
          Type = "oneshot";
          ExecStart = "${logoutScript} ${uName}";
        };
      };

      mkEnableService = uName: uCfg: {
        enable = true;
        description = "Re-enable login for ${uName}";
        path = [
          pkgs.bash
          # For `usermod`.
          pkgs.shadow
        ];
        serviceConfig = {
          # Needs to kill other users' processes, which only root can do.
          User = "root";
          Type = "oneshot";
          ExecStart = "${enableScript} ${uName}";
        };
      };

      mkLogoutTimer = uName: uCfg: {
        wantedBy = [ "timers.target" ];
        timerConfig = {
          Persistent = true;
          OnCalendar = onCalsLogout uCfg;
        };
      };

      mkEnableTimer = uName: uCfg: {
        wantedBy = [ "timers.target" ];
        timerConfig = {
          Persistent = true;
          OnCalendar = onCalsEnable uCfg;
        };
      };

      # Build attrsets directly at the leaves
      logoutServices = lib.mapAttrs' (uName: uCfg:
        lib.nameValuePair "userLockout-logout-${uName}" (mkLogoutService uName uCfg)
      ) activeUsers;

      enableServices = lib.mapAttrs' (uName: uCfg:
        lib.nameValuePair "userLockout-enable-${uName}" (mkEnableService uName uCfg)
      ) activeUsers;

      logoutTimers = lib.mapAttrs' (uName: uCfg:
        lib.nameValuePair "userLockout-logout-${uName}" (mkLogoutTimer uName uCfg)
      ) activeUsers;

      enableTimers = lib.mapAttrs' (uName: uCfg:
        lib.nameValuePair "userLockout-enable-${uName}" (mkEnableTimer uName uCfg)
      ) activeUsers;

    in {
      systemd.services = logoutServices // enableServices;
      systemd.timers   = logoutTimers   // enableTimers;
    }
  );
}
