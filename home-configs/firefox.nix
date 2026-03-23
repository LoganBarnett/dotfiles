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
#
# profiles.ini is managed by home-manager as a read-only Nix store symlink.
# Firefox 67+ tries to write an [Install<hash>] section to it on first
# launch; the hash is derived from the Firefox binary path and changes with
# every Nix store update, so it cannot be pre-computed statically.  Setting
# browser.profiles.enabled = false disables the per-installation profile
# management UI (added in Firefox 129) so Firefox silently falls back to
# StartWithLastProfile / Default=1 without showing a picker.  The failed
# background write of the Install section does not cause a crash.
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
        # Disable per-installation profile management UI (Firefox 129+) so
        # Firefox uses StartWithLastProfile / Default=1 without a picker,
        # even when profiles.ini has no [Install<hash>] section.
        "browser.profiles.enabled" = false;
        # Suppress the first-run wizard.  Firefox compares
        # homepage_override.mstone against the current build milestone; "ignore"
        # unconditionally skips the new-install/upgrade welcome page.  Each Nix
        # store update changes the binary path, which Firefox treats as a fresh
        # installation, triggering the wizard on every deploy without these.
        "browser.startup.homepage_override.mstone" = "ignore";
        # Do not prompt to become the default browser.
        "browser.shell.checkDefaultBrowser" = false;
        # Accept the data-reporting / telemetry terms so the TOU dialog does
        # not appear.  Version 2 is the current accepted policy version.
        "datareporting.policy.dataSubmissionPolicyAcceptedVersion" = 2;
        # Tell Firefox that the import/migration wizard has already run.
        "browser.migration.version" = 2;
        # Disable the post-first-run "later run" onboarding experience.
        "browser.laterrun.enabled" = false;
        # Disable Pocket entirely — controls the "Popular Today" stories
        # section on the new-tab page.  The activity-stream prefs below are
        # also set but Pocket must be disabled at the extension level too.
        "extensions.pocket.enabled" = false;
        # Remove the "Popular Today" Pocket stories section from new-tab.
        "browser.newtabpage.activity-stream.feeds.section.topstories" = false;
        "browser.newtabpage.activity-stream.showSponsored" = false;
        # Remove the "Top Sites" shortcuts row (pre-populated app suggestions)
        # from the new-tab page entirely.
        "browser.newtabpage.activity-stream.feeds.topsites" = false;
        # Remove the default app shortcut suggestions ("Top Sites") that
        # Firefox pre-populates on new-tab.  Setting to an empty string clears
        # the built-in list; any sites the user explicitly pins are unaffected.
        "browser.newtabpage.activity-stream.default.sites" = "";
        "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
        # Suppress Firefox 130+ "Try Profiles" onboarding notification that
        # appears inside the browser window after first launch.  The
        # browser.profiles.enabled = false set above also suppresses the
        # profile-picker dialog; this pref targets the in-browser banner.
        "browser.profiles.createdByDevEdition" = false;
      };
    };
  };
}
