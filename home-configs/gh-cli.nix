################################################################################
# Install the gh CLI for managing repos over GitHub.
################################################################################
{ pkgs, lib, ... }:
{
  programs.gh = {
    enable = true;
    settings = {
      git_protocol = "ssh";
    };
    extensions = [
      pkgs.gh-s
    ];
  };
}
