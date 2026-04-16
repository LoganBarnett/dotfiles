{ pkgs, ... }:
{
  services.gpg-agent = {
    enable = true;
    enableZshIntegration = true;
    defaultCacheTtl = 3600;
    # Effectively permanent cache for automation that needs persistent access.
    # Expect a reboot long before this year-long TTL would expire naturally.
    # TODO: Move this to the M-C* host only, since it's only relevant there.
    maxCacheTtl = 34560000;
    # home-manager does not auto-detect the platform for pinentry, so we
    # select the native macOS dialog on Darwin and fall back to the generic
    # curses interface elsewhere.
    pinentry.package =
      if pkgs.stdenv.isDarwin then pkgs.pinentry_mac else pkgs.pinentry-curses;
    extraConfig = ''
      allow-emacs-pinentry
      allow-loopback-pinentry
    '';
  };
}
