{ pkgs ? import <nixpkgs> {}, lib, makeWrapper, stdenv }:
# We use `rec` here so that we can reuse the `buildInputs` variable.
#
# While `rec` is considered an anti-pattern by some, in this case it means that
# we can override the variable and everything will still work.
stdenv.mkDerivation rec {
  name = "battlescribe-update-data";
  version = "0.1";
  src = ./battlescribe-update-data.sh;
  nativeBuildInputs = [ makeWrapper];
  buildInputs = [ pkgs.lastversion ];
  ## Source unpacking
  # Nix will assume we are using a tarball of some sort by default, and try to
  # unpack the shell script, which obviously fails, so we need to handle this
  # ourselves.
  #
  # A real shell script we want to package probably comes with a directory,
  # likely bundled in a tarball, so this wouldn't normally be necessary.
  #
  # See also https://nixos.org/manual/nixpkgs/stable/#ssec-unpack-phase
  unpackCmd = ''
    # $curSrc is the variable that contains the path to our source.
    mkdir src

    # We rename the file here, because when nix adds files to the store it adds
    # a hash, which obviously we don't want for our shell script.
    cp $curSrc src/${name}.sh
  '';
  # Note that we don't have a build phase.  You'd think that we would need one
  # to patch our shebang, but actually, `stdenv.mkDerivation` patches all
  # shebangs it can find by default in the `fixupPhase` (which comes after the
  # build phase), so we don't have to worry about patching shebangs.
  #
  # See also https://nixos.org/manual/nixpkgs/stable/#ssec-fixup-phase
  installPhase = ''
    runHook preInstall
    # Before we wrap it, we need to actually install our script.
    #
    # For reference, -D creates leading directories, and m755 makes it
    # root-read-write-exec, all other users read-exec.  This is pretty standard
    # for binaries, because this allows root to easily delete the file if
    # need-be.
    #
    # In practice, on NixOS, the file will be read-only for all users, but it's
    # the thought that counts.
    #
    # See also `man install`
    #
    install -Dm755 ${name}.sh $out/bin/${name}.sh

    # This is where we create the wrapping script that sets PATH.
    #
    # Note the single quotes around our makeBinPath - after all, this is just an
    # argument to a binary executed by bash. While nix paths can technically
    # never result in splitting, this makes it very explicit that we're putting
    # something in bash args that may need escaping otherwise.
    #
    # See also https://nixos.org/manual/nixpkgs/stable/#fun-wrapProgram
    #
    wrapProgram $out/bin/${name}.sh --prefix PATH : '${lib.makeBinPath buildInputs}'
    runHook postInstall
  '';
}
