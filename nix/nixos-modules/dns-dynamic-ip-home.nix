################################################################################
# Anchors a shifting WAN IP to a DNS A record.
#
# I don't have a permanent IP.  While it doesn't change much, it does change.
# Update the IP of the DNS record periodically.
#
# At some point I would like some kind of monitoring to tell me if this job is
# passing or failing.
################################################################################
{ config, lib, pkgs, ... }: let
  user = "dynamic-dns";
in {
  age.secrets = {
    porkbun-api-key = {
      group = user;
      mode = "0440";
      rekeyFile = ../secrets/porkbun-api-key.age;
    };
    porkbun-api-secret = {
      group = user;
      mode = "0440";
      rekeyFile = ../secrets/porkbun-api-secret.age;
    };
  };
  systemd.timers.dns-dynamic-ip-home = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      # Without this, it won't start on boot too.
      OnBootSec = "15m";
      OnUnitActiveSec = "15m";
    };
  };
  # Activate the timer on a switch/rebuild.
  system.activationScripts.dns-dynamic-ip-home-activate = let
    service-name = "dns-dynamic-ip-home";
    systemctl = "${pkgs.systemd}/bin/systemctl";
  in {
    text = ''
      if ${systemctl} is-enabled --quiet ${service-name}.service; then
        echo "Triggering ${service-name}.service after rebuild."
        ${systemctl} start ${service-name}.service
      fi
    '';
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
          runtimeInputs = [ pkgs.dness ];
          excludeShellChecks = [
            "SC2154"
          ];
          text = builtins.readFile ../scripts/dns-dynamic-ip-home.sh;
        };
        # TODO: Move hostname and domain into facts.nix.
        tomlFile = (pkgs.formats.toml {}).generate;
        configFile = tomlFile "dness-conf.toml" {
          log = {
            level = "debug";
          };
          ip_resolver = "opendns";
          domains = [
            {
              type = "porkbun";
              key = "{{API_KEY}}";
              secret = "{{API_SECRET}}";
              domain = "logustus.com";
              records = [
                "vpn"
              ];
            }
          ];
        };
        api-key-file = config.age.secrets.porkbun-api-key.path;
        api-secret-file = config.age.secrets.porkbun-api-secret.path;
      in
        # Remember that systemd units don't use backslashes to break apart long
        # lines.  You just have to live with them, or wrap them into additional
        # scripts.  dns-dynamic-ip-home is already a wrapper around dness, so we
        # don't want to wrap it further.
        # lib.strings.concatStringsSep " " [
        #   "${script}/bin/dns-dynamic-ip-home"
        #   ''--config-file "${config}"''
        #   ''--api-key-file "${config.age.secrets.godaddy-api-key.path}"''
        #   ''--secret-file "${config.age.secrets.godaddy-api-secret.path}"''
        # ];
        lib.strings.concatStringsSep " " [
          "${script}/bin/dns-dynamic-ip-home"
          ''--config-file "${configFile}"''
          ''--api-key-file "${api-key-file}"''
          ''--secret-file "${api-secret-file}"''
        ];
      Type = "oneshot";
      User = user;
    };
    wants = [ "run-agenix.d.mount" ];
  };
  users.users.${user} = {
    isSystemUser = true;
    group = user;
  };
  users.groups.${user} = {};
}
