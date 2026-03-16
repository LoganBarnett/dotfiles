{ ... }:
{
  services.ollama.loadModels = [
    # Uncensored model with no safety fine-tuning.
    "dolphin3:8b"
    "gemma3:4b"
    "llama3.1:8b"
    "llama3.2:3b"
    "mistral:7b"
    "phi4-mini:latest"
  ];
}
