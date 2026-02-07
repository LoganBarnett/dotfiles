{ ... }: {
  # Configure journalctl to display timestamps in ISO-8601 format by default.
  #
  # Note: systemd/journald has no configuration file setting to change the
  # default output format. The format is controlled by command-line flags.
  # We use shell aliases to make ISO-8601 the default for interactive use.

  # Create shell alias for journalctl with ISO-8601 timestamps.
  # This makes `journalctl` always use ISO-8601 format in interactive shells.
  programs.bash.shellAliases = {
    journalctl = "journalctl --output=short-iso";
  };

  programs.zsh.shellAliases = {
    journalctl = "journalctl --output=short-iso";
  };

  programs.fish.shellAliases = {
    journalctl = "journalctl --output=short-iso";
  };
}
