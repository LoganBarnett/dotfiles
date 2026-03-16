################################################################################
# Manage the user's local configuration for Aider.
################################################################################
{ lib, ... }: {
  home.file.".aider.conf.yml".text = lib.generators.toYAML {} {
    openai-api-base = "https://ollama.proton/v1";
    openai-api-key = "ollama";
    model = "deepseek-coder:1.3b";
  };
}
