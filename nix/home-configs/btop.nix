################################################################################
# Lay down the configuration for btop.
#
# See also ../agnostic-configs/btop.nix for packages and fonts pertaining to
# btop.
################################################################################
{ lib, pkgs, ... }: {
  # home.file.".config/btop/btop.conf".text = lib.optional pkgs.stdenv.isDarwin ''
  #   # Make this work with Terminal.app
  #   lowcolor = True
  # '';

  # Detect if we're using Terminal.app, as it doesn't support "truecolor".  You
  # can detect if your terminal supports true color with this:
  # printf "\x1b[38;2;255;100;0mTRUECOLOR\x1b[0m\n"
  # If you see orange text, you have truecolor.  Plain text means you don't have
  # it.
  programs.zsh.shellAliases = {
    btop = ''
      if [ "$TERM_PROGRAM" = "Apple_Terminal" ]; then
        command btop --low-color "$@"
      else
        command btop "$@"
      fi
    '';
  };
}
