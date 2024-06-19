# Work around build error: https://github.com/NixOS/nixpkgs/issues/317055
# This applies to a zig build failure.
# zig is used by ncdu.
final: prev: {
  cmake = prev.cmake.overrideAttrs {
    strictDeps = false;
  };
  libclang = prev.libclang.overrideAttrs {
    strictDeps = false;
  };
  zig = prev.zig.overrideAttrs {
    strictDeps = false;
  };
}
