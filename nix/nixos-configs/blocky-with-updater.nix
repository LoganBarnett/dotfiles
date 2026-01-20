################################################################################
# Integrates blocky-lists-updater with the existing Blocky DNS configuration.
#
# This configuration enables dynamic blocklist management by:
# 1. Setting up blocky-lists-updater to fetch and aggregate remote blocklists
# 2. Configuring Blocky to consume the aggregated lists via HTTP
# 3. Supporting local custom lists that can be dynamically updated
#
# The lists-updater serves files via HTTP on port 8081, and Blocky fetches them
# from there. When lists change, the updater calls Blocky's /api/lists/refresh
# endpoint to reload without restart.
################################################################################
{ config, facts, host-id, lib, pkgs, ... }: let
  inherit (lib) optionals;
  inherit (lib.attrsets) mapAttrs' mapAttrsToList;
  inherit (lib.lists) any flatten filter unique;

  # URL where blocky-lists-updater serves aggregated lists.
  listsBaseUrl = "http://localhost:${toString config.services.blocky-lists-updater.webPort}/downloaded";
  watchListsBaseUrl = "http://localhost:${toString config.services.blocky-lists-updater.webPort}/watch";
in {
  imports = [
    ../nixos-configs/blocky.nix
    ../nixos-modules/blocky-lists-updater.nix
  ];

  services.blocky-lists-updater = {
    enable = true;
    blockyUrl = "http://localhost:4000";
    webPort = 8081;

    # Remote sources to download and aggregate.
    sources = {
      ads = [
        "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"
        "https://big.oisd.nl/domainswild"
        "https://blocklistproject.github.io/Lists/ads.txt"
        "https://blocklistproject.github.io/Lists/tracking.txt"
      ];
      adult = [
        "https://blocklistproject.github.io/Lists/porn.txt"
      ];
      malware = [
        "https://blocklistproject.github.io/Lists/abuse.txt"
        "https://blocklistproject.github.io/Lists/crypto.txt"
        "https://blocklistproject.github.io/Lists/fraud.txt"
        "https://blocklistproject.github.io/Lists/phishing.txt"
        "https://blocklistproject.github.io/Lists/piracy.txt"
        "https://blocklistproject.github.io/Lists/ransomware.txt"
        "https://blocklistproject.github.io/Lists/scam.txt"
      ];
      social = [
        "https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/social-only/hosts"
      ];
      video = [
        "https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/fakenews/hosts"
      ];
    };

    # Local watch lists for dynamic additions (e.g., from your classifier).
    watchLists = {
      # This file can be updated dynamically and will trigger Blocky refresh.
      classifier-blocks = "";
    };

    # Update lists daily.
    updateInterval = 86400;
    initialDelay = 60;
    logLevel = "INFO";
  };

  # Override Blocky configuration to use the updater-served lists.
  services.blocky.settings.blocking = {
    blackLists = {
      ads = [ "${listsBaseUrl}/ads.txt" ];
      adult = [ "${listsBaseUrl}/adult.txt" ];
      malware = [ "${listsBaseUrl}/malware.txt" ];
      social = [ "${listsBaseUrl}/social.txt" ];
      video = [ "${listsBaseUrl}/video.txt" ];
      # Add the classifier watch list as a dynamic source.
      classifier = [ "${watchListsBaseUrl}/classifier-blocks.txt" ];
    };
    # Keep the existing clientGroupsBlock configuration from blocky.nix.
    # It will inherit from the parent import.
  };

  # Open the updater's web port for local access.
  networking.firewall.allowedTCPPorts = [ 8081 ];
}
