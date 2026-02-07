################################################################################
# Headless code development environment configuration.
#
# This configuration provides a terminal-based development environment suitable
# for running Claude Code and other coding tools in a headless server context.
# Includes tmux for terminal multiplexing to manage multiple coding sessions.
################################################################################
{ lib, pkgs, ... }: {
  allowUnfreePackagePredicates = [
    (pkg: builtins.elem (lib.getName pkg) [ "claude-code" ])
  ];
  environment.systemPackages = [
    # Claude Code - agentic coding tool that lives in your terminal, understands
    # your codebase, and helps you code faster.
    pkgs.claude-code
    # tmux - terminal multiplexer for managing multiple terminal sessions.
    # Allows detaching and reattaching to sessions, which is essential for
    # headless development work.
    pkgs.tmux
    # Alternative terminal multiplexer if preferred: pkgs.screen
  ];
}
