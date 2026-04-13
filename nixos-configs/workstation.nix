################################################################################
# Use this for any headed workstation.
#
# On a workstation, I want to perform actual work tasks with my computer (not
# like `$WORK` but instead just constructive things.
#
# Much consolidation is needed.  Particularly let's also nab what we see on
# ../nixos-configs/user-can-develop.nix.
################################################################################
{
  flake-inputs,
  lib,
  pkgs,
  system,
  ...
}:
let
  npm-generate-package-lock-json = pkgs.writeShellApplication {
    name = "npm-generate-package-lock-json";
    text = builtins.readFile ../scripts/npm-generate-package-lock-json;
    runtimeInputs = [
      # (pkgs.nodejs_22.override { enableNpm = true; })
      pkgs.nodejs
    ];
  };
  mcphost = pkgs.mcphost.overrideAttrs (
    old:
    let
      version = "0.31.1";
    in
    {
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
    }
  );
  # This one is a little more tricky.  There is a "copilot" package that is an
  # AWS fork (I think).  The version on nixpkgs stable is pointing at an old
  # repo, which they have renamed.  So to track this properly, I have a copy of
  # the current repo derivation in ../derivations/github-copilot-cli.nix.  But
  # even that isn't current.  To keep things current, update the version and
  # hashes there.  The wiki instructions found in
  # https://wiki.nixos.org/wiki/Node.js#Override_NodeJS_package do not work well
  # because they require the package.json and package-lock.json files to be
  # readily available.
  github-copilot-cli = pkgs.callPackage ../derivations/github-copilot-cli.nix { };
  # nix-editor upstream uses cargoLock.lockFile which produces a dangling
  # Cargo.lock symlink in cargo-vendor-dir under Nix >= 2.33.  Rebuild from
  # the flake input source using cargoHash until upstream fixes it.
  nix-editor = pkgs.rustPlatform.buildRustPackage {
    pname = "nix-editor";
    version = "0.3.0";
    src = flake-inputs.nix-editor;
    cargoHash = "sha256-t9QkcRY3viyuDuzxVxT/jWUJ4YPN1riLK9pRK4CRkpo=";
  };
  signal-desktop-update = pkgs.writeShellApplication {
    name = "signal-desktop-update";
    text = builtins.readFile ../scripts/signal-desktop-update;
    runtimeInputs = [
      nix-editor
      pkgs.curl
      pkgs.jq
      pkgs.nix
    ];
  };
  firefox-bin-update = pkgs.writeShellApplication {
    name = "firefox-bin-update";
    text = builtins.readFile ../scripts/firefox-bin-update;
    runtimeInputs = [
      nix-editor
      pkgs.curl
      pkgs.jq
      pkgs.nix
    ];
  };
  yt-dlp-update = pkgs.writeShellApplication {
    name = "yt-dlp-update";
    text = builtins.readFile ../scripts/yt-dlp-update;
    runtimeInputs = [
      nix-editor
      pkgs.curl
      pkgs.jq
      pkgs.nix
    ];
  };
  zoom-us-update = pkgs.writeShellApplication {
    name = "zoom-us-update";
    text = builtins.readFile ../scripts/zoom-us-update;
    runtimeInputs = [
      nix-editor
      pkgs.curl
      pkgs.gnugrep
      pkgs.gnused
      pkgs.coreutils
      pkgs.nix
    ];
  };
  bgutil-pot-update = pkgs.writeShellApplication {
    name = "bgutil-pot-update";
    text = builtins.readFile ../scripts/bgutil-pot-update;
    runtimeInputs = [
      nix-editor
      pkgs.curl
      pkgs.jq
      pkgs.nix
    ];
  };
  makemkv-update = pkgs.writeShellApplication {
    name = "makemkv-update";
    text = builtins.readFile ../scripts/makemkv-update;
    runtimeInputs = [
      nix-editor
      pkgs.curl
      pkgs.gnugrep
      pkgs.coreutils
      pkgs.nix
    ];
  };
  claude-code-update = pkgs.writeShellApplication {
    name = "claude-code-update";
    text = builtins.readFile ../scripts/claude-code-update;
    runtimeInputs = [
      nix-editor
      pkgs.curl
      pkgs.jq
      pkgs.coreutils
      pkgs.gnutar
      pkgs.nix
      pkgs.nodejs
    ];
  };
in
{

  allowUnfreePackagePredicates = [
    (
      pkg:
      builtins.elem (lib.getName pkg) [
        "github-copilot-cli"
      ]
    )
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
    pkgs.d2
    github-copilot-cli
    # Manage Jira from the command line, like a scholar.
    pkgs.jira-cli-go
    # An interactive LLM runner with MCP support.  Written in Go, so no runtime
    # and weird build dependencies.  It supports various debugging options as
    # well.  See https://github.com/mark3labs/mcphost for details.
    mcphost
    # Allow surgical edits to static/literal Nix files.  Great for doing things
    # like bumping a version and hash automatically.
    nix-editor
    npm-generate-package-lock-json
    signal-desktop-update
    firefox-bin-update
    yt-dlp-update
    zoom-us-update
    bgutil-pot-update
    makemkv-update
    claude-code-update
  ];

}
