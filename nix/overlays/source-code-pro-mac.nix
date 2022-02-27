# A simple derivation that installs the Source Code Pro font on macOS.
#
# The built-in derivation
# (https://github.com/NixOS/nixpkgs/blob/master/pkgs/data/fonts/source-code-pro/default.nix)
# installs to a shared font directory that works for Linux distributions, but on
# macOS it is expected to go to ~/Library/Fonts. I don't know what the perfect
# Nix solution is here, but we just write it to ~/Library/Fonts.
self: super: {
  source-code-pro-mac = let
    version = "2.038";
  in
  (super.source-code-pro-mac or {}) // super.fetchzip {
      #pname = "source-code-pro-mac";
      #version = version;
      name = "source-code-pro-mac-${version}";

      url =
      "https://github.com/adobe-fonts/source-code-pro/releases/download/${version}R-ro%2F1.058R-it%2F1.018R-VAR/OTF-source-code-pro-${version}R-ro-1.058R-it.zip";

      postFetch = ''
        mkdir -p $out/Library/Fonts
        unzip -j $downloadedFile \*.otf -d $out/Library/Fonts
      '';

      sha256 = "B5pUO20EbLUGSYrYxXjVS0TlLkgK1EGleIrmicdfmTo=";
    meta = {
      description = "Monospaced font family for user interface and coding environments";
     # Not sure how to add myself as a maintainer. I give up.
     # There is
     # https://nixos.wiki/wiki/Nixpkgs/Contributing#Becoming_a_Nixpkgs_maintainer
     # but it's for maintaining nixpkgs entirely and not a one-off overlay.
     # The distinction between a standard derivation and an overlay remains
     # distressing.
     maintainers = [ ];
    #  maintainers = with self.lib.maintainers; [ loganbarnett ];
      platforms = with self.lib.platforms; darwin;
      homepage = "https://adobe-fonts.github.io/source-code-pro/";
      license = self.lib.licenses.ofl;
    };
  };
}
