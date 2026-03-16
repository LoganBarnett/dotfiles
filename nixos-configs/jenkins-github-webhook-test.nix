################################################################################
# Give me a Jenkins instance to test things like jj and
# github-to-jenkins-webhook.
################################################################################
{ config, flake-inputs, ... }: {

  imports = [
    flake-inputs.github-to-jenkins-webhook.nixosModules.default
  ];

  nixpkgs.overlays = [
    flake-inputs.github-to-jenkins-webhook.overlays.default
  ];

  age.secrets.github-to-jenkins-webhook-secret = {
    generator.script = "long-passphrase";
  };

  services.github-to-jenkins-webhook = {
    enable = true;
    githubSecretFile = config.age.secrets.github-to-jenkins-webhook-secret.path;
    jenkinsUrl = "http://localhost:${toString config.services.jenkins.port}";
    logLevel = "debug";
    # TODO: This conflicts with the default port of Jenkins on 8080.
    port = 8081;
  };

  services.jenkins = {
    enable = true;
  };

  services.https.fqdns."jenkins.proton" = {
    enable = true;
    internalPort = config.services.jenkins.port;
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "logustus+acme@gmail.com";
  };

  services.nginx = {
    enable = true;
    virtualHosts."webhook.logustus.com" = {
      forceSSL = true;
      enableACME = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:${
          toString config.services.github-to-jenkins-webhook.port
        }";
        # proxyWebsockets = true;
      };
    };
  };

  # 80 needed by ACME.
  networking.firewall.allowedTCPPorts = [ 80 443 ];

}
