################################################################################
# A series of tools for helping manage the Nix store.
#
# Typically we want everything declarative but sometimes the store gets into a
# wonky state and we need tools to help us un-wonky it.
################################################################################
{ pkgs, ... }: {
  environment.systemPackages = [
    (pkgs.writeShellApplication {
      name = "nix-store-lock-clean";
      text = builtins.readFile ../scripts/nix-store-lock-clean.sh;
    })
  ];
}
