final: prev: {
  # This follows the suggestion by @wegank here:
  # https://github.com/NixOS/nixpkgs/pull/283524#discussion_r1465828416
  # This works around the binary_function / __binary_function Boost issue seen
  # here: https://github.com/NixOS/nixpkgs/issues/341632
  # Before opening a PR I need to:
  # 1. Revert prusa-slicer.nix to its original version (perhaps make a copy,
  #    because there are interesting changes from another PR).
  # 2. Ensure it actually works (not just moves past my build error).
  # 3. Formatting?
  # 4. Cherry pick my commit adding me as a maintainer from my ComfyUI branch.
  # 5. Identify if the WebKit addition is due to the new version or if it just
  #    never built on aarch64-darwin.
  prusa-slicer = prev.callPackage ../derivations/prusa-slicer.nix {
    stdenv = if prev.stdenv.isDarwin then prev.overrideLibcxx prev.darwin.apple_sdk_11_0.llvmPackages_14.stdenv else prev.stdenv;
  };
}
