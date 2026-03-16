################################################################################
# Exports per-lease DHCP data as Prometheus textfile metrics.
#
# The google/dnsmasq_exporter only exposes aggregate statistics; it cannot
# expose per-lease details (MAC address, IP, hostname).  This config bridges
# that gap by parsing the dnsmasq leases file directly and writing Prometheus
# textfile format that the node_exporter textfile collector will serve.
#
# Produced metrics:
#   dhcp_lease_expiry     - Unix timestamp when the DHCP lease expires.
#   dhcp_lease_first_seen - Unix timestamp when this MAC first appeared in
#                           the leases file.  Resets when the device
#                           disconnects and the lease is removed, so it
#                           approximates "when did this device most recently
#                           join?"
################################################################################
{ lib, pkgs, ... }: let
  textfileDir = "/var/lib/dhcp-lease-exporter";

  leaseMetricsScript = pkgs.writeShellScript "dhcp-lease-metrics" ''
    set -euo pipefail

    LEASES_FILE="/var/lib/dnsmasq/dnsmasq.leases"
    DIR="${textfileDir}"
    FIRST_SEEN_DB="''${DIR}/first-seen.db"
    TMP_OUTPUT="''${DIR}/dhcp-leases.prom.tmp"
    OUTPUT="''${DIR}/dhcp-leases.prom"

    declare -A first_seen
    if [[ -f "''${FIRST_SEEN_DB}" ]]; then
      while IFS='=' read -r mac ts; do
        [[ -n "''${mac}" ]] && first_seen["''${mac}"]="''${ts}"
      done < "''${FIRST_SEEN_DB}"
    fi

    declare -A current_macs

    ${pkgs.coreutils}/bin/cat > "''${TMP_OUTPUT}" << 'PROM_HEADER'
# HELP dhcp_lease_expiry Unix timestamp when the DHCP lease expires.
# TYPE dhcp_lease_expiry gauge
# HELP dhcp_lease_first_seen Unix timestamp when this MAC first appeared in DHCP leases.
# TYPE dhcp_lease_first_seen gauge
PROM_HEADER

    if [[ -f "''${LEASES_FILE}" ]]; then
      while read -r expiry mac ip hostname _; do
        [[ -z "''${mac}" ]] && continue
        # IPv6 entries use a different identifier format without colons.
        [[ "''${mac}" =~ ^([0-9a-fA-F]{2}:){5}[0-9a-fA-F]{2}$ ]] || continue

        [[ "''${hostname}" == "*" ]] && hostname=""

        # Strip double-quotes to keep Prometheus label syntax valid.
        hostname="''${hostname//\"/}"

        oui=$(echo "''${mac//:}" | ${pkgs.coreutils}/bin/cut -c1-6 | ${pkgs.coreutils}/bin/tr 'a-z' 'A-Z')
        vendor=$(${pkgs.gnugrep}/bin/grep -m1 "^''${oui} " ${pkgs.nmap}/share/nmap/nmap-mac-prefixes | ${pkgs.coreutils}/bin/cut -d' ' -f2- || true)
        vendor="''${vendor:-unknown}"
        vendor="''${vendor//\"/}"

        current_macs["''${mac}"]=1

        # $EPOCHSECONDS is the time this script first observed this MAC in the
        # current lease period, not the lease expiry.  It resets to "now" when
        # the lease disappears and the device reconnects.
        if [[ -z "''${first_seen["''${mac}"]+x}" ]]; then
          first_seen["''${mac}"]="$EPOCHSECONDS"
        fi

        ts="''${first_seen["''${mac}"]}"
        labels="mac=\"''${mac}\",ip=\"''${ip}\",hostname=\"''${hostname}\",vendor=\"''${vendor}\""
        echo "dhcp_lease_expiry{''${labels}} ''${expiry}" >> "''${TMP_OUTPUT}"
        echo "dhcp_lease_first_seen{''${labels}} ''${ts}" >> "''${TMP_OUTPUT}"
      done < "''${LEASES_FILE}"
    fi

    for mac in "''${!first_seen[@]}"; do
      [[ -z "''${current_macs["''${mac}"]+x}" ]] && unset "first_seen[''${mac}]"
    done

    {
      for mac in "''${!first_seen[@]}"; do
        echo "''${mac}=''${first_seen["''${mac}"]}"
      done
    } > "''${DIR}/first-seen.db.tmp"
    ${pkgs.coreutils}/bin/mv "''${DIR}/first-seen.db.tmp" "''${FIRST_SEEN_DB}"

    # Atomic replacement prevents node_exporter from reading a partial file.
    ${pkgs.coreutils}/bin/mv "''${TMP_OUTPUT}" "''${OUTPUT}"
  '';
in {
  # The textfile directory is 0755 root:root; files written by this service
  # (running as root, umask 022) are 0644 root:root and thus readable by
  # node_exporter's unprivileged service user.
  systemd.tmpfiles.rules = [
    "d ${textfileDir} 0755 root root -"
  ];

  # Point node_exporter's textfile collector at the output directory.
  # prometheus-client.nix no longer disables the textfile collector, so no
  # override is required here.
  services.prometheus.exporters.node.extraFlags = [
    "--collector.textfile.directory=${textfileDir}"
  ];

  systemd.services.dhcp-lease-metrics = {
    description = "Generate DHCP lease Prometheus textfile metrics.";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${leaseMetricsScript}";
      # Restrict filesystem access to only what the script requires.
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      NoNewPrivileges = true;
      ReadOnlyPaths = [ "/var/lib/dnsmasq" ];
      ReadWritePaths = [ textfileDir ];
    };
  };

  systemd.timers.dhcp-lease-metrics = {
    description = "Periodically refresh DHCP lease Prometheus textfile metrics.";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      # Run once shortly after boot, then every 30 seconds.
      OnBootSec = "30s";
      OnUnitActiveSec = "30s";
    };
  };
}
