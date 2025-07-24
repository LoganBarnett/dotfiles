{ config, lib, pkgs, ... }: {
  imports = [
    ./pytorch-bin.nix
    ./unfree-predicates.nix
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
      # "gemma"
      # "llama2-uncensored"
      # # Are they good?  Let's see.
      # "llama3.3"
      # "deepseek-r1"
      # # Different from gemma?
      # "gemma3"
      # # Try out some reasoning stuff.
      # "qwq"
    ];
    environmentVariables = {
      # This is speaking directly about CORS origins but it might also be
      # enforcing server-side CORS, which is just unheard of and frankly
      # outrageous.  I still need to confirm this is what it's doing.
      OLLAMA_ORIGINS = "*";
      OLLAMA_LOG_LEVEL = "debug";
      # Want to see why you're getting a 403?  This isn't deep enough.
      OLLAMA_DEBUG = "30";
    };
  };
  services.https = {
    fqdns."ollama.proton" = {
      internalPort = config.services.ollama.port;
    };
  };
  services.nginx.virtualHosts."ollama.proton".locations."/" = {
    # Turn these off, this made a big difference for me.
    recommendedProxySettings = false;
    recommendedUwsgiSettings = false;
    # It is unknown how many of these are needed, but since the prior
    # "recommended" settings are off now, I think we need them.  No
    # experimentation done yet to confirm.
    extraConfig = ''
      proxy_set_header Origin "";
      proxy_set_header Connection "";
      proxy_set_header Upgrade "";
      proxy_set_header Host 127.0.0.1;
    '';
  };
  allowUnfreePackagePredicates = [
    (pkg: builtins.elem (lib.getName pkg) [
      "open-webui"
    ])
  ];
  nixpkgs.overlays = [
    (final: prev: {
      pythonPackageExtensions = [(py-final: py-prev: {
        einops = py-prev.overrideAttrs {
          doCheck = false;
        };
      })];
    })
  ];
  # Currently broken, I think.
  # services.open-webui = {
  #   enable = true;
  #   package = pkgs.callPackage ../derivations/open-webui-delegated.nix {};
  # };
}
