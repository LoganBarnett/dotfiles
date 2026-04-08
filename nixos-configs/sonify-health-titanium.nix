{
  config,
  facts,
  pkgs,
  ...
}:
{
  services.https.fqdns."titanium-sonify.${facts.network.domain}" = {
    serviceNameForSocket = "sonify-health";
  };

  services.sonify-health = {
    enable = true;
    logLevel = "debug";
    oidc = {
      enable = true;
      baseUrl = "https://titanium-sonify.${facts.network.domain}";
      issuer = "https://authelia.${facts.network.domain}";
      clientId = "titanium-sonify";
      clientSecretFile = config.age.secrets.titanium-sonify-oidc-client-secret.path;
    };
    audioDevice = "CARD=PCH";
    heartbeat = {
      slot = 1;
      cycleDurationSecs = 14;
      checks = [
        {
          name = "internal";
          command = "${pkgs.fping}/bin/fping -q -t 4000 -r 1 192.168.254.254 192.168.254.9 silicon.proton";
        }
        {
          name = "external";
          command = "${pkgs.fping}/bin/fping -q -t 4000 -r 1 208.67.222.222 9.9.9.9 resolver1.opendns.com api.anthropic.com";
        }
      ];
    };
    drone.metrics = [
      {
        name = "gpu";
        command = "${pkgs.gawk}/bin/awk 'BEGIN{getline v < \"${config.hardware.amdGpuCard.sysfsPath}/gpu_busy_percent\"; printf \"%.2f\\n\", v/100}'";
        resultMode = "stdout";
        register = "low";
      }
    ];
  };
}
