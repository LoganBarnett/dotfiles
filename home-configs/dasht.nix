################################################################################
# dasht – offline API documentation browser using Dash docsets.
#
# Replaces the proprietary Dash.app (Kapeli) with an open-source, terminal-
# based alternative that uses the same docset format.  Docsets are managed via
# `dasht-docsets-install` and rendered in w3m or the default browser.
################################################################################
{ pkgs, ... }:
{
  home.packages = [ pkgs.dasht ];
}
