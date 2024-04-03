{ flake-inputs, system, host-id }: { pkgs, ... }: {
  nixpkgs.overlays = [
    # This lets us include the agenix-rekey package.
    flake-inputs.agenix-rekey.overlays.default
  ];
  rekey.secrets = (import ../secrets/secrets.nix);
  # rekey.nixosConfigurations = flake-inputs.self.nixosConfigurations;
  # rekey.nodes = flake-inputs.self.nixosConfigurations;
  # rekey.userFlake = flake-inputs.self;
  age = {
    secrets = (import ../secrets/secrets.nix );
  # age.secrets = ../secrets/secrets.nix;
    rekey = {
      masterIdentities = [
        ./agenix-master-key.pub
      ];
      # Must be relative to the flake.nix file.
      # localStorageDir = ../. + "/secrets/${host-id}";
      # localStorageDir = ../. + "/secrets";
      localStorageDir = ../. + "/secrets/rekeyed/${host-id}";
      generatedSecretsDir = ../. + "/secrets/generated/${host-id}";
      # These fields are labeled as missing with:
      #  The option `age.rekey.userFlake' does not exist. Definition values:
      # userFlake = flake-inputs.self;
      # nodes = flake-inputs.self.nixosConfigurations;
      storageMode = "local";
    };
  };
  imports = [
    flake-inputs.agenix-rekey.nixosModules.default
  ];
  # agenix-rekey = agenix-rekey.configure {
  #   # Must be relative to the flake.nix file.
  #   localStorageDir = ../. + "/secrets/${self.nixosConfigurations.config.networking.hostName}";
  #   userFlake = self;
  #   nodes = self.nixosConfigurations;
  #   storageMode = "local";
  # };
  environment.systemPackages = [
    # Just importing the package directly is enough for our purposes.
    # flake-inputs.agenix.packages.${system}.default
    pkgs.agenix-rekey
  ];
}
