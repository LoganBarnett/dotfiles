################################################################################
# Replaces the built-in hex secret generator in agenix-rekey with a generator
# that accepts an optional length.
#
# This assumes agenix-rekey is working out of
# https://github.com/oddlama/agenix-rekey/pull/75 or the work itself is merged
# already.
################################################################################
{ lib, pkgs, ... }: {
  age.generators.hex = lib.mkForce ({
    decrypt,
    deps,
    file,
    name,
    pkgs,
    secret,
    ...
  }:
    # 24 was the original default.
    lib.traceVal "${pkgs.openssl}/bin/openssl rand -hex ${
      toString (secret.settings.length or 24)
    }"
  );
}
