################################################################################
# Anchors a shifting WAN IP to a DNS A record.
#
# I don't have a permanent IP.  While it doesn't change much, it does change.
# Update the IP of the DNS record periodically.
#
# At some point I would like some kind of monitoring to tell me if this job is
# passing or failing.
################################################################################
{ config, pkgs, ... }: {
  age.secrets = {
    godaddy-api-key = {
      rekeyFile = ../secrets/godaddy-api-key.age;
    };
    godaddy-api-secret = {
      rekeyFile = ../secrets/godaddy-api-secret.age;
    };
  };
  systemd.timers.dns-dynamic-ip-home = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      # OnBootSec = "15m";
      OnUnitActiveSec = "15m";
    };
  };
  systemd.services.dns-dynamic-ip-home = {
    enable = true;
    serviceConfig = {
      ExecStart = let
        # Unfortunately the documentation generators out there are incomplete
        # for this function.  See
        # https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/trivial-builders/default.nix#L175
        # for all of the parameters availble.
        script = pkgs.writeShellApplication {
          name = "dns-dynamic-ip-home";
          runtimeInputs = [ pkgs.curl ];
          excludeShellChecks = [
            "SC2154"
          ];
          text = builtins.readFile ../scripts/dns-dynamic-ip-home.sh;
        };
      in
        # TODO: Move hostname and domain into facts.nix.
        ''
        ${script}/bin/dns-dynamic-ip-home \
          --hostname vpn \
          --domain 'logustus.com' \
          --api-key-file "${config.age.secrets.godaddy-api-key.path}" \
          --secret-file "${config.age.secrets.godaddy-api-secret.path}"
        '';
      Type = "oneshot";
      # TODO: Make this less sloppy.
      User = "root";
    };
    wants = [ "run-agenix.d.mount" ];
  };
}
