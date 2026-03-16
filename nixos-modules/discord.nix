{ lib, pkgs, ... }: {
  imports = [
    ../nixos-modules/configuration-lens.nix
  ];
  allowUnfreePackagePredicates = [
    (pkg: builtins.elem (lib.getName pkg) [ "discord" ])
  ];
  environment.systemPackages = [
    # Chat but also voice/video.  Works great with games, but can serve many
    # other purposes.  Do not include in steam-gaming.nix since the kids needn't
    # have it just yet.
    pkgs.discord
  ];
  # TODO: This is flawed.  I need to move this to a home manager activation so
  # the correct user will own the file.  I also need to put some smarts in to
  # ensure the directory.  I also need to ensure that the activation script
  # happens _after_ the files get a chance to be laid down.
  # For now I have the file where I want it to be, so this will need to be
  # revisited later.
  fileLenses = {
    discord-skip-update-check = {
      filePath = "/home/logan/.config/discord/settings.json";
      # This wound up being pretty ugly.  We might be better off with `jq` for
      # JSON.  The JSON plugin for Augeas isn't even documented.  This is the
      # best form of documentation for the plugin currently:
      # https://github.com/hercules-team/augeas/issues/559
      # It suggests using the print subcommand to determine the document
      # structure, and using "/const" as a path suffix to set boolean or null
      # values.  I suppose that would include numbers too.  The `.=` thing is
      # from Augeas' internal language (https://augeas.net/docs/language.html).
      documentPath = "/dict/entry/[.= \"SKIP_HOST_UPDATE\"]/const";
      operation = "set";
      value = "true";
      transform = "Json.lns";
    };
  };
}
