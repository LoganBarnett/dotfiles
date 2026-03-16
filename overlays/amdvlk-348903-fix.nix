# Address the build issue seen here:
# https://github.com/NixOS/nixpkgs/issues/348903
# Credit to @unkn.  The code below is adapted from their comment:
# https://github.com/NixOS/nixpkgs/issues/348903#issuecomment-2417988004
final: prev: {
  my_glslang = prev.glslang.overrideAttrs (finalAttrs: oldAttrs: {
    version = "15.0.0";
    src = final.fetchFromGitHub {
      owner = "KhronosGroup";
      repo = "glslang";
      rev = "refs/tags/${finalAttrs.version}";
      hash = "sha256-QXNecJ6SDeWpRjzHRTdPJHob1H3q2HZmWuL2zBt2Tlw=";
    };
    # Effectively revert the change seen here:
    # https://github.com/NixOS/nixpkgs/commit/c3948c21ef00fbf8ef02f3c2922e84ba72e8a62b#diff-21ee06ef20d156bedb03da22e28850e6043b04a00706a796e7e427fce44500e9R31
    cmakeFlags = [];
  });

  amdvlk = prev.amdvlk.override {
    glslang = final.my_glslang;
  };
}
