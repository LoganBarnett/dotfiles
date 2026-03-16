{ ... }:
{
  imports = [
    ./ollama-models-8gb-vram.nix
  ];
  services.ollama.loadModels = [
    "gemma3:12b"
    "qwen2.5:14b"
  ];
}
