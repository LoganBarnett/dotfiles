################################################################################
# optnix is a fast, terminal-based options searcher for Nix module systems.
# It provides search capabilities for nix-darwin, NixOS, home-manager, and
# flake-parts options.
################################################################################
{ flake-inputs, pkgs, ... }: {
  # Install optnix as a system package.
  # Configuration via programs.optnix may be available if optnix provides a
  # darwin module.  Check optnix documentation for advanced configuration.
  environment.systemPackages = [
    flake-inputs.optnix.packages.${pkgs.system}.default
  ];
}
