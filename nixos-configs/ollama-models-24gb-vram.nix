{ ... }:
{
  imports = [
    ./ollama-models-12gb-vram.nix
  ];
  services.ollama.loadModels = [
    # Q4 estimates (params × 600 MB + 1500 MB overhead):
    # gemma3:27b  → ~17.7 GB
    # qwen2.5:32b → ~20.7 GB
    # deepseek-r1:32b → ~20.7 GB
    "deepseek-r1:32b"
    "gemma3:27b"
    "qwen2.5:32b"
  ];
}
