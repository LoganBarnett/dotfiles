# Enables Nix Flakes.
{ ... } : {
  nix.settings = {
    # Enable flakes and new 'nix' command.
    experimental-features = "nix-command flakes";
  };
}
