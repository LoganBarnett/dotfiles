################################################################################
# Nix strangely does not, when using flakes, use the actual flakes when doing
# builds via tools such as `nix-env' and friends.  Some tooling out there (such
# as agenix-rekey) wants to build things so it can use them - they aren't
# pre-built I suppose.  It uses the root channel (that is, the channel that the
# root user has (`sudo nix-channel --list` to observe - an entry means the root
# channel is present).
#
# In any case, we need to instruct Nix to actually use the flake inputs and the
# rest of our configuration (such as our overlays).  This is critical for fixing
# bugs in our builds.
# This file is an adaptation of the following link, with a notable change that
# it can support nix-darwin too.
# https://github.com/NobbZ/nixos-config/blob/99df721f491a6c33a12d7e82e09659935f15ab2b/nixos/modules/flake.nix
################################################################################
{
  # unstable,
  # nixpkgs-2105,
  # nixpkgs-2111,
  nix,
  nixpkgs,
  programsdb,
  ...
}: { config, lib, options, pkgs, ... }: let
  base = "/etc/nixpkgs/channels";
  nixpkgsPath = "${base}/nixpkgs";
  # Additional versions of nixpkgs should be added thusly:
  # nixpkgs2105Path = "${base}/nixpkgs2105";
  # nixpkgs2111Path = "${base}/nixpkgs2111";
in (lib.mkMerge [
  # mkIf doesn't work for this because of how it recurses or something.
  # While this this is not in nix-darwin (yet), it would be nice to turn it on
  # when it is.  So instead of checking the platform, check the option.
  # TODO: see new ways to set conditional values in my Nix README for how to set
  # this intelligently.
  # (lib.optionalAttrs (options.programs?command-not-found) {
  #   # Not in nix-darwin yet.
  #   programs.command-not-found.dbPath =
  #     programsdb.packages.${pkgs.system}.programs-sqlite;
  # })
  # So systemd in macOS.
  (lib.optionalAttrs (options?systemd) {
    systemd.tmpfiles.rules = [
      "L+ ${nixpkgsPath}     - - - - ${nixpkgs}"
      # "L+ ${nixpkgs2105Path} - - - - ${nixpkgs-2105}"
      # "L+ ${nixpkgs2111Path} - - - - ${nixpkgs-2111}"
    ];
  })
  {
    nix = {
      package = pkgs.nixVersions.latest;
      # Old way of getting this package?  Doesn't work.
      # package = lib.mkDefault nix.packages.${pkgs.system}.nix;
      # Enable flakes and new 'nix' command.
      settings.experimental-features = [
        "nix-command"
        "flakes"
      ];
      # TODO: Determine if this is automatic now or not.
      # registry.nixpkgs.flake = nixpkgs;
      # registry.nixpkgs2105.flake = nixpkgs-2105;
      # registry.nixpkgs2111.flake = nixpkgs-2111;
      nixPath = [
        "nixpkgs=${nixpkgsPath}"
        # "nixpkgs2105=${nixpkgs2105Path}"
        # "nixpkgs2111=${nixpkgs2111Path}"
        "/nix/var/nix/profiles/per-user/root/channels"
      ];
    };
  }
])
