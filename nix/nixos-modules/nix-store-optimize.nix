################################################################################
# Optimizes storage in the store by de-duplicating entries, I guess.
################################################################################
{ ... } : {
  nix.settings = {
    # Deduplicate and optimize nix store.
    # There might be some assumptions here that are not safe.  All I have on
    # that presently is this post:
    # https://realjenius.com/2023/07/21/corrupt-store/
    # It claims other tickets are present that worry about it.  I have found
    # https://github.com/NixOS/nix/issues/1281 but it does not seem like hard
    # evidence.  I have had broken hashes during a `nix-store --verify
    # --check-contents`.  I also have other compiler errors.  I've had physical
    # memory issues in the past, but those have passed lots of tests.
    # auto-optimise-store = false;
    auto-optimise-store = true;
  };
}
