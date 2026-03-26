################################################################################
# Signal gets its own section just because it's special.  I don't mean that
# kindly.
#
# I can't use the normal nixpkgs for this, becuase Signal insists on
# frequently updating itself.  This update will never work due to
# immutability (thank goodness), but it still means we need to have a
# means of getting latest.  This is because Signal Desktop disables
# itself after an update becomes available.  Meanies.
################################################################################
{ flake-inputs, system, ... }:
final: prev: {
  signal-desktop-bin =
    prev.signal-desktop-bin
    # Ugh this is rather hopeless.  Nixpkgs drifts behind so some hero needs to
    # keep it updated, and I'm not that hero.
    .overrideAttrs
      (
        old:
        let
          statics = (import ../static.nix).signal-desktop-bin;
          inherit (statics) version hash;
        in
        {
          inherit version;
          src = final.fetchurl {
            url = "https://updates.signal.org/desktop/signal-desktop-mac-universal-${version}.dmg";
            inherit hash;
          };
        }
      );
}
