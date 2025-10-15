################################################################################
# Use this for any headed workstation.
#
# On a workstation, I want to perform actual work tasks with my computer (not
# like `$WORK` but instead just constructive things.
#
# Much consolidation is needed.
################################################################################
{ pkgs, ... }: {
  environment.systemPackages = [
    # An interactive LLM runner with MCP support.  Written in Go, so no runtime
    # and weird build dependencies.  It supports various debugging options as
    # well.  See https://github.com/mark3labs/mcphost for details.
    pkgs.mcphost
  ];

}
