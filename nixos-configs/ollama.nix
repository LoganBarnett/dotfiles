{
  config,
  facts,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ../nixos-configs/pytorch-bin.nix
  ];
  services.ollama = {
    enable = true;
    loadModels = [ ];
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
    fqdns."ollama.${facts.network.domain}" = {
      internalPort = config.services.ollama.port;
    };
  };
  services.nginx.virtualHosts."ollama.${facts.network.domain}".locations."/" = {
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
  # ollama-model-loader starts after ollama.service reaches exec, but CUDA
  # initialisation means the HTTP port isn't immediately ready.  Poll until
  # it responds before launching the pull jobs so the unit doesn't fail and
  # trigger the Restart cycle during deployment activation.
  systemd.services.ollama-model-loader.serviceConfig.ExecStartPre =
    let
      host = config.services.ollama.host;
      port = toString config.services.ollama.port;
    in
    pkgs.writeShellScript "wait-for-ollama" ''
      until ${pkgs.curl}/bin/curl -sf http://${host}:${port}/ > /dev/null 2>&1; do
        sleep 1
      done
    '';

  allowUnfreePackagePredicates = [
    (
      pkg:
      builtins.elem (lib.getName pkg) [
        "open-webui"
      ]
    )
  ];
  # nixpkgs.overlays = [
  #   (final: prev: {
  #     pythonPackageExtensions = [(py-final: py-prev: {
  #       einops = py-prev.overrideAttrs {
  #         doCheck = false;
  #       };
  #     })];
  #   })
  # ];
  # Currently broken, I think.
  # services.open-webui = {
  #   enable = true;
  #   package = pkgs.callPackage ../derivations/open-webui-delegated.nix {};
  # };
}
