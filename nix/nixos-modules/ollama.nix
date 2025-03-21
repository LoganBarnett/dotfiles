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
    ];
  };
  # Currently broken, I think.
  services.open-webui.enable = false;
}
