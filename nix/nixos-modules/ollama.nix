{ config, ... }: {
  imports = [
    ./pytorch-bin.nix
  ];
  services.ollama = {
    enable = true;
    acceleration =
      if config.nixpkgs.config.cudaSupport
      then "cuda"
      else (
        if config.nixpkgs.config.rocmSupport
        then "rocm"
        else config.services.ollama.acceleration
      );
    # package = pkgs.ollama-cuda;
    loadModels = [
      "gemma"
      "llama2-uncensored"
      # Are they good?  Let's see.
      "llama3.3"
      "deepseek-r1"
      # Different from gemma?
      "gemma3"
      # Try out some reasoning stuff.
      "qwq"
    ];
  };
  # Currently broken, I think.
  services.open-webui.enable = false;
}
