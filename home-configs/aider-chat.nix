################################################################################
# Manage the user's local configuration for Aider.
################################################################################
{ facts, lib, ... }:
{
  home.file.".aider.conf.yml".text = lib.generators.toYAML { } {
    openai-api-base = "https://ollama.${facts.network.domain}/v1";
    openai-api-key = "ollama";
    model = "deepseek-coder:1.3b";
  };
}
