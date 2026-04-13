################################################################################
# discord overlay to use version and hash from static.nix.
#
# Discord forces updates and disables itself when too far behind, just like
# Signal.  This lets us update independently of nixpkgs via
# scripts/discord-update.
################################################################################
final: prev:
let
  statics = (import ../static.nix).discord;
  platform =
    if prev.stdenv.hostPlatform.isDarwin then statics.darwin else statics.linux;
  inherit (platform) version hash;
in
{
  discord = prev.discord.overrideAttrs (old: {
    inherit version;
    src = final.fetchurl {
      url =
        if prev.stdenv.hostPlatform.isDarwin then
          "https://dl.discordapp.net/apps/osx/${version}/Discord.dmg"
        else
          "https://dl.discordapp.net/apps/linux/${version}/discord-${version}.tar.gz";
      inherit hash;
    };
  });
}
