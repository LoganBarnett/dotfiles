{ ... }: {
  # Configure timestamps to use ISO-8601 format system-wide.
  #
  # Systemd respects the LC_TIME locale setting for timestamp formatting.
  # Using en_DK.UTF-8 (English with Danish conventions) gives us ISO-8601
  # timestamps in journalctl and other programs, while keeping English text.
  #
  # This affects:
  # - journalctl output
  # - systemctl status timestamps
  # - date command output
  # - Any program that respects LC_TIME

  i18n.extraLocaleSettings = {
    LC_TIME = "en_DK.UTF-8";
  };
}
