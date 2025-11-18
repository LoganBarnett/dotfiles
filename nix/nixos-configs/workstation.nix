################################################################################
# Use this for any headed workstation.
#
# On a workstation, I want to perform actual work tasks with my computer (not
# like `$WORK` but instead just constructive things.
#
# Much consolidation is needed.  Particularly let's also nab what we see on
# ../nixos-modules/user-can-develop.nix.
################################################################################
{ flake-inputs, lib, pkgs, system, ... }: let
  pkgs-latest = import flake-inputs.nixpkgs-latest {
    inherit system;
  };
  npm-generate-package-lock-json = pkgs.writeShellApplication {
    name = "npm-generate-package-lock-json";
    text = builtins.readFile ../scripts/npm-generate-package-lock-json.sh;
    runtimeInputs = [
      # (pkgs.nodejs_22.override { enableNpm = true; })
      pkgs.nodejs
    ];
  };
  mcphost = pkgs.mcphost.overrideAttrs (old: let
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
  });
  # This one is a little more tricky.  There is a "copilot" package that is an
  # AWS fork (I think).  The version on nixpkgs stable is pointing at an old
  # repo, which they have renamed.  So to track this properly, I have a copy of
  # the current repo derivation in ../derivations/github-copilot-cli.nix.  But
  # even that isn't current.  To keep things current, update the version and
  # hashes there.  The wiki instructions found in
  # https://wiki.nixos.org/wiki/Node.js#Override_NodeJS_package do not work well
  # because they require the package.json and package-lock.json files to be
  # readily available.
  github-copilot-cli = pkgs.callPackage ../derivations/github-copilot-cli.nix {};
in {

  allowUnfreePackagePredicates = [
    (pkg: builtins.elem (lib.getName pkg) [
      "github-copilot-cli"
    ])
  ];

  imports = [
    ../agnostic-configs/btop.nix
  ];

  environment.systemPackages = [
    # A universal text/code formatter in Rust, so it's fast and no baggage.
    # This is really good, because the things you must do to get prettier.js to
    # format your Ruby code are a little nutty.  At least in a "secure" world
    # where netcat is considered dangerous...
    # Use `dprint fmt <file>` to format or `dprint check <file>` to do a no-op
    # version.
    pkgs.dprint
    # Effectively a plantuml replacement.  Write flow charts and similar
    # diagrams using a text based language.  Supports a variety of outputs
    # including SVG, PNG, and ASCII text.
    pkgs-latest.d2
    github-copilot-cli
    # Manage Jira from the command line, like a scholar.
    pkgs-latest.jira-cli-go
    # An interactive LLM runner with MCP support.  Written in Go, so no runtime
    # and weird build dependencies.  It supports various debugging options as
    # well.  See https://github.com/mark3labs/mcphost for details.
    mcphost
    npm-generate-package-lock-json
  ];

}
