# TLATER writes an excellent tutorial here on taking a simple script from
# something such as Bash and exposing it via Nix - depedencies included
# regardless of ecosystem.  That can be found here:
# https://discourse.nixos.org/t/packaging-a-bash-script-issues/20224/3
# This derivation is virtually untouched from the example, save some renaming to
# keep it from colliding with the shell built-in `test`.

# This derivation expects to be built with `pkgs.callPackage`.
#
# If you build it any other way, just refer to these via `pkgs.stdenv`
# and such.
{
  lib,
  stdenv,
  makeWrapper,
  curl,
}: let
  # Import from lib.
  inherit (lib) makeBinPath;
in
  # We use `rec` here so that we can reuse the `buildInputs` variable.
  #
  # While `rec` is considered an anti-pattern by some, in this case it
  # means that we can override the variable and everything will still
  # work.
  stdenv.mkDerivation rec {
    ## Metadata
    name = "test-script";
    version = "0.1";

    ## Source
    # This adds our shell script directly to the nix store. I'd advise
    # against doing this in practice, but rather using the tarball or
    # source directory that you're packaging. If we didn't import the
    # file directly, we could completely skip the `unpackCmd` below.
    #
    # If your script is tiny enough that you don't need all this
    # packaging, just use `writeShellApplication` instead, but this
    # derivation is written for educational purposes!
    src = ./test-script.sh;
    ##

    ## Dependencies
    #
    # See also https://nixos.org/manual/nixpkgs/stable/#variables-specifying-dependencies
    # Fair warning, this is all about cross-compilation, which you
    # hardly care about for shell scripts.
    #
    # `nativeBuildInputs` are the build inputs that will run on the
    # build host natively, and are expected to create files that will
    # run on the target host.
    #
    # The distinction is practically pointless for `makeWrapper`,
    # because it produces shell scripts that are target-independent, but
    # for completeness' sake I split it out.
    nativeBuildInputs = [makeWrapper];

    # These are the build inputs that are actually expected to run on
    # the target host.
    #
    # *Normally* nix will inspect the output, and make sure that any of
    # these whose paths end up in the output will also be installed as
    # runtime dependencies. Since we're packaging a shell script,
    # however, this doesn't work, because we rely on $PATH, so we need
    # to use `makeWrapper`.
    #
    # I still like putting `buildInputs` in these kinds of packages
    # because it's very explicit, and who knows, maybe one day nix will
    # be smart enough to propagate these kinds of dependencies.
    buildInputs = [curl];
    #
    # I get these wrong all the time, so if @Nobbz comes and scolds me,
    # sorry!
    ##

    ## Source unpacking
    # Nix will assume we are using a tarball of some sort by default,
    # and try to unpack the shell script, which obviously fails, so we
    # need to handle this ourselves.
    #
    # A real shell script we want to package probably comes with a
    # directory, likely bundled in a tarball, so this wouldn't normally
    # be necessary.
    #
    # See also https://nixos.org/manual/nixpkgs/stable/#ssec-unpack-phase
    unpackCmd = ''
      # $curSrc is the variable that contains the path to our source.
      mkdir test-src

      # We rename the file here, because when nix adds files to the
      # store it adds a hash, which obviously we don't want for our
      # shell script.
      cp $curSrc test-src/test-script.sh
    '';
    ##

    # Note that we don't have a build phase. You'd think that we would
    # need one to patch our shebang, but actually,
    # `stdenv.mkDerivation` patches all shebangs it can find by
    # default in the `fixupPhase` (which comes after the build phase),
    # so we don't have to worry about patching shebangs.
    #
    # See also https://nixos.org/manual/nixpkgs/stable/#ssec-fixup-phase

    ## Installation
    installPhase = ''
      # Before we wrap it, we need to actually install our script.
      #
      # For reference, -D creates leading directories, and m755 makes
      # it root-read-write-exec, all other users read-exec. This is
      # pretty standard for binaries, because this allows root to
      # easily delete the file if need-be.
      #
      # In practice, on NixOS, the file will be read-only for all
      # users, but it's the thought that counts.
      #
      # See also `man install`
      #
      install -Dm755 test-script.sh $out/bin/test-script.sh

      # This is where we create the wrapping script that sets PATH.
      #
      # Note the single quotes around our makeBinPath - after all, this
      # is just an argument to a binary executed by bash. While nix
      # paths can technically never result in splitting, this makes it
      # very explicit that we're putting something in bash args that may
      # need escaping otherwise.
      #
      # See also https://nixos.org/manual/nixpkgs/stable/#fun-wrapProgram
      #
      wrapProgram $out/bin/test-script.sh --prefix PATH : '${makeBinPath buildInputs}'
    '';
    ##
  }
