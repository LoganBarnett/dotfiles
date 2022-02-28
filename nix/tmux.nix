{
  enable = true;
  aggressiveResize = true;
  # Start window index at 1.
  baseIndex = 1;
  # Fix escape delay.
  escapeTime = 0;
  extraConfig = builtins.readFile ../tmux.conf;
}
