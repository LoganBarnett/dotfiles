################################################################################
# WireGuard Status - Grafana dashboard.
#
# Provides panels for monitoring WireGuard peer connectivity and bandwidth.
# Peer display names are derived from facts rather than WireGuard config
# comments (which cannot be emitted from Nix expressions), and are applied as
# field overrides keyed on the public key string.
#
# 1. Bandwidth per peer (RX/TX).
# 2. Bandwidth per interface (RX/TX).
# 3. Last handshake time per peer.
################################################################################
{ lib, facts, ... }:
{
  uid = "wireguard";
  title = "WireGuard Status, Bandwidth, etc.";
  tags = [ "wireguard" ];
  timezone = "browser";
  refresh = "10s";
  schemaVersion = 36;
  panels =
    let
      # Each value comes in as "$public_key $type" (e.g. "foobarkey RX").  One
      # entry for RX and one for TX.  We match both exact strings to apply
      # display-name overrides.
      peer-overrides-with-transmission = lib.pipe facts.network.users [
        (lib.attrsets.mapAttrsToList (name: user: user.devices))
        lib.lists.flatten
        (lib.lists.filter (d: d.vpn))
        (lib.lists.map (
          d:
          d
          // {
            public-key = lib.strings.trim (
              builtins.readFile ../secrets/${d.host-id}-wireguard-client.pub
            );
          }
        ))
        (lib.lists.map (
          d:
          lib.lists.map
            (x: {
              matcher = {
                id = "byName";
                options = "${d.public-key} ${x}";
              };
              properties = [
                {
                  id = "displayName";
                  value = "${d.host-id} ${x}";
                }
              ];
            })
            [
              "RX"
              "TX"
            ]
        ))
        lib.lists.flatten
      ];
      peer-overrides = lib.pipe facts.network.users [
        (lib.attrsets.mapAttrsToList (name: user: user.devices))
        lib.lists.flatten
        (lib.lists.filter (d: d.vpn))
        (lib.lists.map (
          d:
          d
          // {
            public-key = lib.strings.trim (
              builtins.readFile ../secrets/${d.host-id}-wireguard-client.pub
            );
          }
        ))
        (lib.lists.map (d: {
          matcher = {
            id = "byName";
            options = "${d.public-key}";
          };
          properties = [
            {
              id = "displayName";
              value = "${d.host-id}";
            }
          ];
        }))
        lib.lists.flatten
      ];
    in
    [
      {
        title = "WireGuard Bandwidth per Peer";
        type = "timeseries";
        datasource = "Prometheus";
        targets = [
          {
            expr = "rate(wireguard_received_bytes_total[5m])";
            legendFormat = "{{public_key}} RX";
            refId = "A";
          }
          {
            expr = "rate(wireguard_sent_bytes_total[5m])";
            legendFormat = "{{public_key}} TX";
            refId = "B";
          }
        ];
        fieldConfig = {
          defaults = {
            unit = "Bps";
            decimals = 2;
          };
          overrides = peer-overrides-with-transmission;
        };
        gridPos = {
          x = 0;
          y = 0;
          w = 12;
          h = 9;
        };
      }
      {
        title = "WireGuard Bandwidth per Interface";
        type = "timeseries";
        datasource = "Prometheus";
        targets = [
          {
            expr = "sum by (interface) (rate(wireguard_received_bytes_total[5m]))";
            legendFormat = "{{interface}} RX";
            refId = "A";
          }
          {
            expr = "sum by (interface) (rate(wireguard_sent_bytes_total[5m]))";
            legendFormat = "{{interface}} TX";
            refId = "B";
          }
        ];
        fieldConfig = {
          defaults = {
            unit = "bps";
            decimals = 2;
          };
        };
        gridPos = {
          x = 12;
          y = 0;
          w = 12;
          h = 9;
        };
      }
      {
        title = "Peer Last Handshake (Seconds Ago)";
        type = "bargauge";
        datasource = "Prometheus";
        targets = [
          {
            expr = "time() - wireguard_latest_handshake_seconds";
            legendFormat = "{{public_key}}";
            refId = "A";
          }
        ];
        fieldConfig = {
          defaults = {
            unit = "s";
            min = 0;
            max = 600;
            thresholds = {
              mode = "absolute";
              steps = [
                {
                  color = "green";
                  value = null;
                }
                {
                  color = "yellow";
                  value = 30;
                }
                {
                  color = "red";
                  value = 300;
                }
              ];
            };
          };
          overrides = peer-overrides;
        };
        options = {
          displayMode = "gradient";
          orientation = "horizontal";
          showUnfilled = true;
        };
        gridPos = {
          h = 9;
          w = 24;
          x = 0;
          y = 9;
        };
      }
    ];
}
