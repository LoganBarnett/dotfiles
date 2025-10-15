################################################################################
# Use this for any headed workstation.
#
# On a workstation, I want to perform actual work tasks with my computer (not
# like `$WORK` but instead just constructive things.
#
# Much consolidation is needed.  Particularly let's also nab what we see on
# ../nixos-modules/user-can-develop.nix.
################################################################################
{ pkgs, ... }: {
  environment.systemPackages = [
    # An interactive LLM runner with MCP support.  Written in Go, so no runtime
    # and weird build dependencies.  It supports various debugging options as
    # well.  See https://github.com/mark3labs/mcphost for details.
    (pkgs.mcphost.overrideAttrs (old: let
      version = "0.31.1";
    in {
      inherit version;
      src = pkgs.fetchFromGitHub {
        owner = "mark3labs";
        repo = "mcphost";
        rev = "v${version}";
        hash = "sha256-WvoBQLrs9lcbaTueyIPI89Y2t37mKxlE0tOczLhw8Bg=";
      };
      # There seems to be some files expected to be laid down for the tests to
      # work, such as `$HOME/.mcphost.yml`.  Just skip them until a hero can fix
      # all of it.
      doCheck = false;
      vendorHash = "sha256-xZ7JbX1sAt2ZtgSMm86MQEJWwjL17O9LMsjOmkhzWt0=";
    }))
  ];
}
