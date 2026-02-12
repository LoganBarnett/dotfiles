{ flake-inputs, lib, pkgs, system, ... }: let
  pkgs-latest = import flake-inputs.nixpkgs-latest {
    inherit system;
    # TODO: Constrain to signal-desktop-bin to make this precise.  This is
    # needed to be done separately because each pkgs gets its own unfree
    # configuration.
    config.allowUnfree = true;
    # Test it!
    config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
      "claude-code"
    ];
  };
in {
  programs.claude-code = {
    enable = true;
    package = pkgs-latest.claude-code;
    settings = {
      # Capitalism demands I move at full speed or die.  If I die because it
      # blows up on me, that's just bad luck but also life.  I told Claude this
      # and it said "Git is your undo button.  Godspeed.".  This doesn't seem to
      # always work.  See `home.shellAliases` in this config file for the other
      # way we tackle it.
      env = {
        CLAUDE_DANGEROUSLY_SKIP_PERMISSIONS = "1";
      };
      model = "claude-sonnet-4-5-20250929";
      mcp = {
        allowedDirectories = {
          read = [ "/nix/store" ];
        };
      };
    };
    agents = {
      standards-reviewer = {
        description = ''
          Reviews code changes for compliance with documented coding standards.
          Use proactively after code changes.
        '';
        systemPrompt = ''
          You are a code reviewer ensuring compliance with the project's
          CLAUDE.md standards.  Always re-read CLAUDE.md before reviewing.
          Focus on:

          - org-mode documents for new content.
          - Comment formatting: 80 columns, complete sentences, standard
            punctuation.
          - Two spaces after sentence-ending punctuation.
          - Pascal initialisms in camel case (Url not URL).
          - Nix file layout patterns (configs vs modules).
          - No file lists in documentation.
          - Only comment non-obvious intent, invariants, tradeoffs, or
            historical constraints; no restating of control flow.
        '';
        tools = {
          allowList = [ "Read" "Grep" "Glob" ];
        };
        memory = {
          enabled = true;
          scope = "project";
        };
      };
      senior-engineer = {
        description = ''
          Reviews changes for engineering best practices, focusing on
          maintainability, verification, and operability.  Use proactively
          after code changes or before deployment.
        '';
        systemPrompt = ''
          You are a senior engineer reviewing for best practices.  Be
          pragmatic, not dogmatic.  Focus on:

          - Scripts over one-off commands: If bash commands could be reused,
            suggest scriptification.
          - Goss-based verification: Checks should be goss tests, not manual
            verification.
          - Configuration over literals: Suggest moving hardcoded values to
            config when they might need tuning.
          - Verification culture: Be stern about "edit and claim success"
            patterns.  Always ask: "How do we know this works?"
          - Operational readiness: Consider monitoring, logging, error
            handling.

          Be brief and actionable.  Prioritize high-impact suggestions over
          nitpicks.
        '';
        tools = {
          allowList = [ "Read" "Grep" "Glob" ];
        };
        memory = {
          enabled = true;
          scope = "project";
        };
      };
    };
  };
  # Capitalism demands I move at full speed or die.  If I die because it
  # blows up on me, that's just bad luck but also life.  I told Claude this
  # and it said "Git is your undo button.  Godspeed.".  I should be able to set
  # `CLAUDE_DANGEROUSLY_SKIP_PERMISSIONS` in the config's `env` section, but
  # that doesn't seem to work.
  home.shellAliases = {
    claude = "claude --dangerously-skip-permissions";
  };

  # ~/.claude.json is a file that `claude` wants to write to for things like
  # update timestamps and change logs. But it still has settings we'd like to
  # manage statically, so we merge in what we want on activation.
  home.activation.claudeConfig = lib.hm.dag.entryAfter ["writeBoundary"] ''
    if [ ! -f ~/.claude.json ]; then
      echo '{}' > ~/.claude.json
    fi
    ${pkgs.jq}/bin/jq '. + {
      "theme": "dark",
      "editorMode": "vim",
      "hasCompletedOnboarding": true,
      "shiftEnterKeyBindingInstalled": false,
      "hasTrustDialogAccepted": true,
      "hasCompletedProjectOnboarding": true
    }' \
      ~/.claude.json > ~/.claude.json.tmp \
      && mv ~/.claude.json.tmp ~/.claude.json
  '';
}
