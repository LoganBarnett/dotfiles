################################################################################
# Configures tea CLI for Gitea interaction.
#
# This config manages non-sensitive settings for tea. The authentication token
# must be configured separately through `tea logins add`.
################################################################################
{ lib, pkgs, config, ... }:
let
  teaConfigPath = "${config.home.homeDirectory}/Library/Application Support/tea/config.yml";
in {
  imports = [
    ../agnostic-modules/configuration-lens.nix
  ];

  # Ensure the tea config directory and minimal file exist before augeas runs.
#   home.activation.teaConfigInit = lib.hm.dag.entryBefore ["fileLenses"] ''
#     config_dir="${config.home.homeDirectory}/Library/Application Support/tea"
#     config_file="${teaConfigPath}"

#     # Ensure the config directory exists.
#     mkdir -p "$config_dir"

#     # Create minimal config if it doesn't exist.
#     if [ ! -f "$config_file" ]; then
#       cat > "$config_file" <<'EOF'
# logins:
#   - name: gitea
#     url: https://gitea.proton
#     default: true
#     ssh_host: gitea.proton
#     ssh_key: $HOME/.ssh/id_rsa
#     insecure: false
#     ssh_certificate_principal: ""
#     ssh_agent: false
#     ssh_key_agent_pub: ""
#     version_check: true
#     user: logan
# preferences:
#   editor: false
#   flag_defaults:
#     remote: gitea
# EOF
#     fi
#   '';

  # Use yq to ensure non-sensitive settings are present.
  # yq only touches the specified paths, preserving token and other
  # sensitive fields.
  fileLenses = {
    tea-gitea-name = {
      filePath = teaConfigPath;
      documentPath = ".logins[0].name";
      operation = "set";
      value = ''"gitea"'';
      transform = "yaml";
    };
    tea-gitea-url = {
      filePath = teaConfigPath;
      documentPath = ".logins[0].url";
      operation = "set";
      value = ''"https://gitea.proton"'';
      transform = "yaml";
    };
    tea-gitea-default = {
      filePath = teaConfigPath;
      documentPath = ".logins[0].default";
      operation = "set";
      value = "true";
      transform = "yaml";
    };
    tea-gitea-ssh-host = {
      filePath = teaConfigPath;
      documentPath = ".logins[0].ssh_host";
      operation = "set";
      value = ''"gitea.proton"'';
      transform = "yaml";
    };
    tea-gitea-ssh-key = {
      filePath = teaConfigPath;
      documentPath = ".logins[0].ssh_key";
      operation = "set";
      value = ''"/Users/logan/.ssh/id_rsa"'';
      transform = "yaml";
    };
    tea-gitea-insecure = {
      filePath = teaConfigPath;
      documentPath = ".logins[0].insecure";
      operation = "set";
      value = "true";
      transform = "yaml";
    };
    tea-gitea-ssh-cert-principal = {
      filePath = teaConfigPath;
      documentPath = ".logins[0].ssh_certificate_principal";
      operation = "set";
      value = ''""'';
      transform = "yaml";
    };
    tea-gitea-ssh-agent = {
      filePath = teaConfigPath;
      documentPath = ".logins[0].ssh_agent";
      operation = "set";
      value = "true";
      transform = "yaml";
    };
    tea-gitea-ssh-key-agent-pub = {
      filePath = teaConfigPath;
      documentPath = ".logins[0].ssh_key_agent_pub";
      operation = "set";
      value = ''""'';
      transform = "yaml";
    };
    tea-gitea-version-check = {
      filePath = teaConfigPath;
      documentPath = ".logins[0].version_check";
      operation = "set";
      value = "false";
      transform = "yaml";
    };
    tea-gitea-user = {
      filePath = teaConfigPath;
      documentPath = ".logins[0].user";
      operation = "set";
      value = ''"logan"'';
      transform = "yaml";
    };
    tea-preferences-editor = {
      filePath = teaConfigPath;
      documentPath = ".preferences.editor";
      operation = "set";
      value = "false";
      transform = "yaml";
    };
  };
}
