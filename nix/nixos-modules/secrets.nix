{
  flake-inputs,
  system,
  host-id,
  host-public-key ? null,
  host-public-key-file ? null
}: { pkgs, lib, ... }: {
  nixpkgs.overlays = [
    flake-inputs.agenix.overlays.default
    # This lets us include the agenix-rekey package.
    flake-inputs.agenix-rekey.overlays.default
  ];
  age.rekey = {
    hostPubkey = if host-public-key != null
                 then host-public-key
                 else (lib.fileContents host-public-key-file);
    masterIdentities = [
      ../secrets/agenix-master-key
    ];
    # Must be relative to the flake.nix file.
    localStorageDir = (builtins.trace "localStorageDir" (lib.debug.traceVal ../secrets/rekeyed/${host-id}));
    generatedSecretsDir = (builtins.trace "generatedSecretsDir" (lib.debug.traceVal ../secrets/generated/${host-id}));
    # These fields are labeled as missing with:
    #  The option `age.rekey.userFlake' does not exist. Definition values:
    # userFlake = flake-inputs.self;
    # nodes = flake-inputs.self.nixosConfigurations;
    storageMode = "local";
  };
  imports = [
    flake-inputs.agenix.nixosModules.default
    flake-inputs.agenix-rekey.nixosModules.default
  ];
  environment.systemPackages = [
    # This should remain out because agenix-rekey brings in agenix - or at least
    # the bits of it we are interested in.
    # flake-inputs.agenix.packages.${system}.default
    # pkgs.agenix-rekey
  ];
}
