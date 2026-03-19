{
  nixos-rebuild,
  writeShellApplication,
}:
let
  # nixos-rebuild uses ssh:// (legacy nix-store --import/export over SSH pipe)
  # when copying the closure to the build host.  Under load, this protocol
  # fails with "Bad file descriptor".  Patching it to ssh-ng:// uses the Nix
  # daemon protocol instead, which is persistent and reliable.
  patched-nixos-rebuild = nixos-rebuild.overrideAttrs (old: {
    postInstall = (old.postInstall or "") + ''
      substituteInPlace $out/bin/nixos-rebuild \
        --replace-fail '"ssh://$buildHost"' '"ssh-ng://$buildHost"'
    '';
  });
in
writeShellApplication {
  name = "proton-deploy";
  runtimeInputs = [ patched-nixos-rebuild ];
  text = builtins.readFile ../scripts/proton-deploy;
}
