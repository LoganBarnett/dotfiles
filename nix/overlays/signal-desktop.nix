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
{ flake-inputs, system, ... }: final: prev: let
  # The import is required - we cannot use legacyPackages like we normally would
  # because we need to override allowUnfreePredicate, which wouldn't be the same
  # as the predicates used in our "main" imported nixpkgs.
  # nixpkgs-latest = import flake-inputs.nixpkgs-latest {
  #   inherit system;
  #   # https://github.com/signalapp/Signal-Desktop/blob/main/LICENSE
  #   # It's because it uses Apple's non-redistributable emoji package, which is
  #   # kind of funny because isn't this redistribution?  See:
  #   # https://github.com/NixOS/nixpkgs/blob/b5d21ab69d341ff8d06498d3b39b38160533293f/pkgs/by-name/si/signal-desktop/signal-desktop-darwin.nix#L49
  #   config.allowUnfreePredicate = (pkg: builtins.elem (final.lib.getName pkg) [
  #     "signal-desktop"
  #     "signal-desktop-bin"
  #   ]);
  # };
in {
  signal-desktop-bin = prev
    .signal-desktop-bin
    # Ugh this is rather hopeless.  Nixpkgs drifts behind so some hero needs to
    # keep it updated, and I'm not that hero.
    .overrideAttrs (old: let
      version = "7.72.1";
    in {
      inherit version;
      src = final.fetchurl {
        url = "https://updates.signal.org/desktop/signal-desktop-mac-universal-${version}.dmg";
        hash = "sha256-X1x9GObrTIf+5zxY9cfsDVhYeJIhRZ3KChrQxTo5J3E=";
      };
    })
  ;
}
