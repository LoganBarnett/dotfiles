################################################################################
# Optimizes storage in the store by de-duplicating entries, I guess.
################################################################################
{ ... } : {
  nix.settings = {
    # Deduplicate and optimize nix store.
    auto-optimise-store = true;
  };
}
