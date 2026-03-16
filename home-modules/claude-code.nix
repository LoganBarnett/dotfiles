################################################################################
# The `apiKeyHelper` configuration option in ~/.claude/settings.json requires a
# path to a program (not a program invocation), and thus cannot include
# parameters.  All this module does is connects `apiKeyHelper` with `pass`.
################################################################################
{ config, flake-inputs, lib, pkgs, system, ... }:

let
  cfg = config.programs.claude-code;

  apiKeyScript = pkgs.writeShellScript "claude-code-api-key" ''
    ${pkgs.pass}/bin/pass show ${lib.escapeShellArg cfg.passApiKey}
  '';
in
{
  options.programs.claude-code = {
    passApiKey = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Name of the secret in pass containing the Anthropic API key.
      '';
      example = "anthropic-claude-api-key";
    };
  };

  config = lib.mkIf (cfg.passApiKey != null) {
    programs.claude-code.settings.apiKeyHelper = toString apiKeyScript;
  };
}
