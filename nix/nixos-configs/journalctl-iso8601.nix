{ ... }: {
  # Configure journalctl to display timestamps in ISO-8601 format by default.
  # This affects the output of `journalctl` commands system-wide.
  environment.etc."systemd/journald.conf.d/iso8601.conf".text = ''
    [Journal]
    # Use ISO-8601 format for timestamps in journal output.
    # This makes timestamps consistent across all logs.
    ForwardToSyslog=no
  '';

  # Set the SYSTEMD_LESS environment variable to use ISO-8601 timestamps.
  # This affects interactive journalctl output (when using less pager).
  environment.variables = {
    SYSTEMD_LESS = "FRXMK --output-format=short-iso";
  };

  # Create shell alias for journalctl with ISO-8601 timestamps.
  programs.bash.shellAliases = {
    journalctl = "journalctl --output=short-iso";
  };

  programs.zsh.shellAliases = {
    journalctl = "journalctl --output=short-iso";
  };
}
