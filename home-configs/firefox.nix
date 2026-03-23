################################################################################
# Firefox browser configuration with managed extensions and preferences.
#
# Extensions are sourced from NUR (github:nix-community/NUR) via the
# rycee/nur-expressions addon collection.
#
# The prefers-color-scheme override ensures websites correctly receive the
# dark mode signal — without it, Firefox on macOS does not propagate the OS
# dark mode preference to websites via the prefers-color-scheme media query,
# even when the OS and browser are both set to dark mode.
################################################################################
{ pkgs, ... }:
{
  programs.firefox = {
    enable = true;
    package = pkgs.firefox-bin;
    profiles.default = {
      isDefault = true;
      extensions.packages =
        let
          addons = pkgs.nur.repos.rycee.firefox-addons;
        in
        [
          addons."cookies-txt"
          addons."don-t-fuck-with-paste"
          addons.ghosttext
          addons."tab-counter-plus"
          addons.vimium
        ];
      settings = {
        # 0 = dark, 1 = light, 2 = follow OS (broken on macOS — OS reports
        # light despite being in dark mode).
        "layout.css.prefers-color-scheme.content-override" = 0;
      };
    };
  };
}
