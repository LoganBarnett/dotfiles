################################################################################
# MakeMKV overlay to use version and hashes from static.nix.
#
# MakeMKV consists of two components that must match versions:
# - makemkv-oss: Open source component
# - makemkv-bin: Proprietary binary component
#
# This allows easy updates via scripts/makemkv-update.sh without having to
# wait for nixpkgs to catch up.
################################################################################
final: prev: let
  statics = (import ../static.nix).makemkv;
  inherit (statics) version;
in {
  makemkv = prev.makemkv.overrideAttrs (old: let
    srcs.bin = final.fetchurl {
      urls = [
        "http://www.makemkv.com/download/makemkv-bin-${version}.tar.gz"
        "http://www.makemkv.com/download/old/makemkv-bin-${version}.tar.gz"
      ];
      hash = statics.bin.hash;
    };
    srcs.oss = final.fetchurl {
      urls = [
        "http://www.makemkv.com/download/makemkv-oss-${version}.tar.gz"
        "http://www.makemkv.com/download/old/makemkv-oss-${version}.tar.gz"
      ];
      hash = statics.oss.hash;
    };
  in {
    inherit version;
    srcs = final.lib.attrValues srcs;
  });
}
