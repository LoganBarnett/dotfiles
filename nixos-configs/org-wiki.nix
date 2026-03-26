################################################################################
# Org-mode wiki served at wiki.proton.
#
# Content lives in a local git clone at /var/lib/org-wiki-web/content, pushed
# to gitea.proton/logan/wiki after each save and pulled on push webhooks.
################################################################################
{
  config,
  facts,
  lib,
  pkgs,
  ...
}:
let
  domain = "wiki.${facts.network.domain}";
  contentRepo = "/var/lib/org-wiki-web/content";
  gitea-url = "ssh://git@gitea.${facts.network.domain}:2222/logan/wiki.git";
in
{
  age.secrets.org-wiki-web-ssh-key = {
    generator.script = "ssh-ed25519-with-pub";
  };

  services.org-wiki-web = {
    enable = true;
    contentRepo = contentRepo;
    contentRemote = "origin";
    cacheDir = "/var/cache/org-wiki-web";
    siteTitle = "Wiki";
    commitAuthorName = "Wiki";
    commitAuthorEmail = "wiki@${facts.network.domain}";
    oidcIssuer = "https://authelia.${facts.network.domain}";
    oidcClientId = "wiki";
    oidcClientSecretFile = config.age.secrets.wiki-oidc-client-secret.path;
    baseUrl = "https://${domain}";
    sshKeyFile = config.age.secrets.org-wiki-web-ssh-key.path;
  };

  services.https.fqdns."${domain}" = {
    serviceNameForSocket = "org-wiki-web";
  };

  # Ensure agenix secrets are decrypted before the service starts.
  systemd.services.org-wiki-web = {
    after = [ "run-agenix.d.mount" ];
    requires = [ "run-agenix.d.mount" ];
  };

  # Initialise the content repository on first boot.  If the repository
  # already exists the script is a no-op.  On first run it tries to clone
  # from gitea; if the remote repo is not yet accessible it falls back to a
  # local git init with an initial commit.
  systemd.services.org-wiki-web-init = {
    description = "Initialise org-wiki content repository";
    wantedBy = [ "org-wiki-web.service" ];
    before = [ "org-wiki-web.service" ];
    after = [
      "local-fs.target"
      "run-agenix.d.mount"
    ];
    requires = [ "run-agenix.d.mount" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      User = "org-wiki-web";
      Group = "org-wiki-web";
      StateDirectory = "org-wiki-web";
      LoadCredential = [
        "ssh-key:${config.age.secrets.org-wiki-web-ssh-key.path}"
      ];
    };
    environment = {
      GIT_SSH_COMMAND =
        "ssh -i /run/credentials/org-wiki-web-init.service/ssh-key"
        + " -o StrictHostKeyChecking=accept-new"
        + " -o UserKnownHostsFile=/var/lib/org-wiki-web/.ssh/known_hosts";
    };
    script = ''
      mkdir --parents /var/lib/org-wiki-web/.ssh
      if [ ! -d ${lib.escapeShellArg contentRepo}/.git ]; then
        if ${pkgs.git}/bin/git clone \
            ${lib.escapeShellArg gitea-url} \
            ${lib.escapeShellArg contentRepo}; then
          echo "Cloned wiki from ${gitea-url}"
        else
          ${pkgs.git}/bin/git init -b main ${lib.escapeShellArg contentRepo}
          ${pkgs.git}/bin/git \
            -C ${lib.escapeShellArg contentRepo} \
            remote add origin ${lib.escapeShellArg gitea-url}
          printf '#+title: Wiki\n' \
            > ${lib.escapeShellArg contentRepo}/index.org
          ${pkgs.git}/bin/git \
            -C ${lib.escapeShellArg contentRepo} \
            -c user.name="Wiki" \
            -c user.email="wiki@${facts.network.domain}" \
            commit -m "init: seed wiki" index.org
        fi
      fi
    '';
  };

  # Add the wiki cache directory to tmpfiles so it exists before the service
  # starts.
  systemd.tmpfiles.rules = [
    "d /var/cache/org-wiki-web 0750 org-wiki-web org-wiki-web -"
  ];
}
