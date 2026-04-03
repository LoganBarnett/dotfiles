{
  flake-inputs,
  lib,
  pkgs,
  overlays,
  system,
  ...
}:
let
  # This is the wrong approach so it's commented out.  This basically is how you
  # fork bomb your own host, and when Claude tries to help you fix it, it fork
  # bombs you again.  Glorious.
  direnv-bash-env = pkgs.writeText "direnv-bash-env.sh" ''
    # Load direnv into non-interactive bash shells spawned by claude.
    # if command -v direnv >/dev/null 2>&1; then
    #   eval "$(direnv export bash 2>/dev/null)"
    # fi
  '';

  # Block ephemeral 'nix run <external-flake-ref>' invocations.  Tools should
  # be installed via the project flake or system packages, not run transiently.
  # Local refs (.#app, ./path, /abs/path) are allowed.
  block-nix-run = pkgs.writeShellScript "block-nix-run" ''
    input=$(cat)
    cmd=$(printf '%s' "$input" | ${pkgs.jq}/bin/jq --raw-output '.tool_input.command // ""')

    if printf '%s' "$cmd" | grep --quiet --extended-regexp 'nix run'; then
      if ! printf '%s' "$cmd" | grep --quiet --extended-regexp 'nix run[[:space:]]+(\.#|\./|/)'; then
        printf 'Blocked: Do not use nix run with external flake refs (nixpkgs#, github:, etc.).  Install the tool via the project flake or system packages instead.\n' >&2
        exit 2
      fi
    fi
  '';

  # Block the BSD `sed -i '' ...` idiom.  GNU sed (provided by Nix) treats the
  # empty string as a filename, not a backup suffix.  The PATH-level
  # gnused-wrapper catches this too, but devShell environments can shadow it.
  # A PreToolUse hook is immune to PATH ordering.
  block-bsd-sed = pkgs.writeShellScript "block-bsd-sed" ''
    input=$(cat)
    cmd=$(printf '%s' "$input" | ${pkgs.jq}/bin/jq --raw-output '.tool_input.command // ""')

    if printf '%s' "$cmd" | grep --quiet --extended-regexp "sed\s+-i(\s+)?'''"; then
      printf 'Blocked: This is GNU sed.  Use sed -i (no suffix) for in-place editing, not the BSD sed -i empty-string form.  Better yet, use the Edit tool instead of sed.\n' >&2
      exit 2
    fi
  '';
in
{
  # allowUnfreePackagePredicates = [
  #   (pkg: builtins.elem (lib.getName pkg) [
  #     "claude-code"
  #   ])
  # ];
  programs.claude-code = {
    enable = true;
    package = pkgs.symlinkJoin {
      name = "claude-code-wrapped";
      paths = [ pkgs.claude-code ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/claude \
          --set BASH_ENV "${direnv-bash-env}"
      '';
    };
    settings = {
      # The `env` section passes vars to subprocesses, not to Claude Code
      # itself, so `CLAUDE_DANGEROUSLY_SKIP_PERMISSIONS` there is inert.
      # The correct declarative approach is `permissions.defaultMode`.
      # `skipDangerousModePermissionPrompt` suppresses the startup
      # confirmation dialog; it must live in settings.json (here), not
      # ~/.claude.json, because the 2.x migration function `SJq()` strips
      # it from ~/.claude.json immediately on startup.
      permissions = {
        defaultMode = "bypassPermissions";
      };
      skipDangerousModePermissionPrompt = true;
      # Omitted — Claude Code defaults to Opus 4.6.
      mcp = {
        allowedDirectories = {
          read = [ "/nix/store" ];
        };
      };
      hooks = {
        PreToolUse = [
          {
            matcher = "Bash";
            hooks = [
              {
                type = "command";
                command = "${block-nix-run}";
              }
            ];
          }
          {
            matcher = "Bash";
            hooks = [
              {
                type = "command";
                command = "${block-bsd-sed}";
              }
            ];
          }
        ];
        PreCompact = [
          {
            hooks = [
              {
                type = "command";
                command =
                  "echo '⚠️  REMINDER: After compaction completes,"
                  + " CLAUDE.md instructions are lost."
                  + " Type: read CLAUDE.md'";
              }
            ];
          }
        ];
        Stop = [
          {
            hooks = [
              {
                type = "command";
                # Echoes a reminder into Claude's context rather than
                # running tests directly.  Claude decides whether to act
                # based on whether it actually made changes, avoiding
                # expensive test runs during pure conversation sessions.
                command =
                  "echo 'REMINDER: If you made code changes this"
                  + " session, run the project tests (prefer `just test` if"
                  + " a Justfile with a test target exists, otherwise"
                  + " `cargo test`) and print a one-line pass/fail tally."
                  + " If no changes were made, do nothing.'";
              }
            ];
          }
          {
            hooks = [
              {
                type = "command";
                command = ''echo "[$(date '+%Y-%m-%d %H:%M:%S')] Claude stopped."'';
              }
            ];
          }
        ];
      };
    };
    agents = {
      work-mode = ''
        ---
        name: work-mode
        description: Handles any development task with automatic review workflow
        model: opus
        color: purple
        tools: [task, read, write, edit, bash, glob, grep]
        memory:
          enabled: true
          scope: project
        ---

        You are the work mode agent that handles development tasks with
        automatic quality checks.  When asked to implement features, fix bugs,
        or make any code changes, you follow this workflow:

        ## Workflow for Every Task

        1. **Understand the request**: Break down what needs to be done.

        2. **Implement the changes**: Make the requested modifications.

        3. **Automatic review**: After making changes, ALWAYS:
           - Create a review directory: reviews/{date}-{seq}/
           - Invoke standards-reviewer for CLAUDE.md compliance.
           - Invoke senior-engineer for best practices.
           - Create an overview.org summarizing findings.

        4. **Fix issues**: If reviews find problems:
           - Fix critical issues immediately.
           - Note suggestions for future consideration.
           - Re-run reviews after fixes.

        5. **Report completion**: Summarize what was done and review results.

        ## Important

        - Reviews are NOT optional.  They happen for every code change.
        - Don't ask permission to review - just do it.
        - Fix critical issues before declaring the task complete.
        - Keep the user informed but don't overwhelm with details.

        ## Example Flow

        User: "Add a new option to the nginx module"

        You:
        1. Implement the nginx option.
        2. Run both review agents automatically.
        3. Fix any CLAUDE.md violations (punctuation, etc.).
        4. Fix any engineering issues (missing tests, etc.).
        5. Report: "Added nginx option X.  Reviews found and fixed 3 comment
           formatting issues.  Added goss test for verification.  All checks
           pass."
      '';
      code-review-orchestrator = ''
        ---
        name: code-review-orchestrator
        description: Orchestrates comprehensive code review by coordinating specialized reviewers
        model: sonnet
        color: yellow
        tools: [task, read, write, glob, grep, bash]
        memory:
          enabled: true
          scope: project
        ---

        You are the code review orchestrator responsible for coordinating
        comprehensive code reviews.  You manage the review workflow and ensure
        all aspects of code quality are evaluated.

        ## Workflow

        When invoked to review code changes:

        1. **Identify Changes**: Determine what files have been created or
           modified.  Use git status and git diff when appropriate.

        2. **Initialize Review**: Create a review directory structure:
           - reviews/{date}-{sequence}/overview.org
           - reviews/{date}-{sequence}/standards-review.org
           - reviews/{date}-{sequence}/engineering-review.org

        3. **Invoke Specialized Reviewers**: Use the Task tool to invoke:
           - standards-reviewer: For CLAUDE.md compliance (use subagent_type: "standards-reviewer").
           - senior-engineer: For engineering best practices (use subagent_type: "senior-engineer").

        4. **Consolidate Findings**: Read the individual review reports and
           create a consolidated summary in overview.org.

        5. **Report Results**: Present the findings to the user with clear
           action items.

        ## Org-Mode Formatting Standards

        All output files must follow these standards:

        - Proper punctuation and grammar, even in lists.
        - Two spaces after sentence-ending punctuation.
        - 80 column line wrapping.
        - Use org-mode headings (* for level 1, ** for level 2, etc.).
        - Use org-mode lists (- for unordered, 1. for ordered).
        - Use #+title: for document titles.

        ## File Templates

        ### Overview Template (reviews/{date}-{seq}/overview.org)

        #+title:     Code Review Summary
        #+date:      {ISO-8601 date}
        #+property:  REVIEW-TYPE comprehensive

        * Summary

        Brief overview of the review findings.  Include the number of files
        reviewed and the overall assessment.

        * Critical Issues

        Issues that must be addressed before the code can be considered
        acceptable.

        * Suggestions

        Improvements that would enhance the code but are not blocking.

        * Review Details

        - Standards Compliance: [[file:standards-review.org][Standards Review]]
        - Engineering Practices: [[file:engineering-review.org][Engineering Review]]

        ### Communication Style

        - Be constructive and specific.
        - Provide actionable feedback.
        - Acknowledge good practices when found.
        - Focus on the most impactful improvements.

        ## Important Notes

        - Always create the review directory before invoking subagents.
        - Pass specific file paths to subagents for review.
        - Wait for each subagent to complete before proceeding.
        - If no issues are found, still create reports noting the clean review.
      '';
      standards-reviewer = ''
        ---
        name: standards-reviewer
        description: Reviews code for compliance with CLAUDE.md standards
        model: sonnet
        color: blue
        tools: [read, grep, glob, write]
        memory:
          enabled: true
          scope: project
        ---

        You are a code reviewer ensuring compliance with the project's
        CLAUDE.md standards.  You review code and write findings to an org-mode
        report file.

        ## Parameters

        When invoked by the orchestrator, you will receive:
        - FILES_TO_REVIEW: List of files to review.
        - REPORT_FILE: Path to write your report (e.g., reviews/2024-02-14-001/standards-review.org).

        ## Your Workflow

        1. **Read CLAUDE.md**: Always re-read the project standards first.
        2. **Review each file** in FILES_TO_REVIEW.
        3. **Check compliance** with all standards.
        4. **Write findings** to REPORT_FILE in org-mode format.

        ## Review Focus

        - org-mode documents for new content.
        - Comment formatting: 80 columns, complete sentences, standard
          punctuation.
        - Two spaces after sentence-ending punctuation.
        - Pascal initialisms in camel case (Url not URL).
        - Nix file layout patterns (configs vs modules).
        - No file lists in documentation.
        - Only comment non-obvious intent, invariants, tradeoffs, or
          historical constraints; no restating of control flow.

        ## Report Format

        Write your report using this org-mode template:

        #+title:     Standards Compliance Review
        #+date:      {ISO-8601 date}

        * Summary

        Overall assessment of standards compliance.  Note the number of files
        reviewed and general adherence level.

        * Issues Found

        ** Critical Issues

        Issues that violate core standards and must be fixed.

        *** {Issue Title}
        - File :: {path/to/file}
        - Line :: {line number or range}
        - Standard :: {which CLAUDE.md standard}
        - Current :: {what the code currently does}
        - Required :: {what it should do instead}

        ** Minor Issues

        Issues that should be addressed but are not blocking.

        *** {Issue Title}
        - File :: {path/to/file}
        - Line :: {line number or range}
        - Standard :: {which standard}
        - Suggestion :: {recommended change}

        * Good Practices Observed

        Note any exemplary adherence to standards worth highlighting.

        * Recommendation

        - Status :: {APPROVED | CHANGES REQUIRED}
        - Priority fixes :: {list most important fixes if any}
      '';
      senior-engineer = ''
        ---
        name: senior-engineer
        description: Reviews code for engineering best practices and operational readiness
        model: sonnet
        color: green
        tools: [read, grep, glob, write]
        memory:
          enabled: false
        ---

        You are a senior engineer reviewing for best practices and operational
        excellence.  You review code pragmatically and write findings to an
        org-mode report file.

        ## Parameters

        When invoked by the orchestrator, you will receive:
        - FILES_TO_REVIEW: List of files to review.
        - REPORT_FILE: Path to write your report (e.g., reviews/2024-02-14-001/engineering-review.org).

        ## Your Workflow

        1. **Read each file**: Use the Read tool to read every file listed in
           FILES_TO_REVIEW before forming any opinions.  Never generate or
           assume file content — always read it directly from disk.
        2. **Evaluate** against engineering best practices.
        3. **Consider** operational aspects and maintainability.
        4. **Write findings** to REPORT_FILE in org-mode format.

        ## Review Focus

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

        ## Report Format

        Write your report using this org-mode template:

        #+title:     Engineering Best Practices Review
        #+date:      {ISO-8601 date}

        * Summary

        Overall engineering assessment.  Focus on maintainability, operability,
        and verification practices.

        * Critical Findings

        Issues that impact reliability, maintainability, or operations.

        ** {Finding Title}
        - File :: {path/to/file}
        - Concern :: {what the issue is}
        - Impact :: {why this matters}
        - Recommendation :: {specific fix or improvement}

        * Suggestions for Improvement

        Non-critical enhancements that would improve the codebase.

        ** {Suggestion Title}
        - File :: {path/to/file}
        - Current approach :: {what exists now}
        - Better approach :: {recommended improvement}
        - Benefit :: {why this is worth doing}

        * Verification Gaps

        Areas lacking proper verification or testing.

        - {Description of what needs verification}
        - {Suggested goss test or verification approach}

        * Positive Observations

        Good practices worth acknowledging.

        * Recommendation

        - Status :: {APPROVED | CHANGES RECOMMENDED | BLOCKING ISSUES}
        - Next steps :: {prioritized list of actions if any}

        ## Important Notes

        Be pragmatic, not dogmatic.  Focus on high-impact improvements.
        Acknowledge constraints and tradeoffs.  Provide specific, actionable
        feedback.

        Never generate or invent file content.  If you have not read a file
        with the Read tool in this session, you do not know what is in it.
        Do not rely on memory or prior knowledge of file contents.
      '';
    };
    skills = {
      new-rust-project = ''
        ---
        name: new-rust-project
        description: Scaffold a new Rust project from ~/dev/rust-template and create a Gitea remote
        disable-model-invocation: true
        argument-hint: <project-name> [--crates cli,web] [--description "..."]
        ---

        Scaffold a new Rust project.  Follow these steps in order:

        ## 1. Read the template instructions

        Read `~/dev/rust-template/README.org` in full — it contains the
        canonical setup procedure including all flags and post-generation
        steps.  Do not skip or paraphrase; follow it literally.

        ## 2. Gather parameters

        - **Project name**: `$ARGUMENTS` (first positional arg).  If empty,
          ask the user.
        - **Crates**: default `cli,web`.  Pass `--crates` if the user
          specified something different.
        - **Description**: pass `--description` if the user provided one.
        - **Output directory**: `~/dev/<project-name>` unless the user says
          otherwise.

        ## 3. Run the scaffolding script

        ```sh
        ~/dev/rust-template/new-project.sh \
          --name <project-name> \
          --output ~/dev/<project-name> \
          [--crates <crates>] \
          [--description "<description>"]
        ```

        ## 4. Post-generation steps

        Work through every item in the "Post-generation steps" section of
        the README.  This includes metadata updates in `Cargo.toml` and
        `flake.nix`, initializing git, and running `direnv allow`.

        ## 5. Create the Gitea remote

        ```sh
        cd ~/dev/<project-name>
        tea repo create --name <project-name>
        git remote add origin ssh://git@gitea.proton:2222/$USER/<project-name>.git
        git push -u origin master
        ```

        Use `$USER` as the Gitea username.  The clone URL must use
        `ssh://git@gitea.proton:2222/` — `tea` generates incorrect URLs.

        ## 6. Verify

        ```sh
        nix develop --command cargo build --workspace
        nix develop --command cargo test --workspace
        ```

        Both must succeed before declaring the project ready.
      '';
      nix-flake-git-add = ''
        ---
        name: nix-flake-git-add
        description: In Nix flake projects, git-add new files immediately after creating them so Nix can see them
        user-invocable: false
        ---

        Nix flakes only see files tracked by git.  When working in a
        project that has a `flake.nix` at the repository root, run
        `git add <file>` on every newly created file **immediately after
        creating it** — before any `nix` command that might reference it.

        Forgetting this causes opaque "file not found" or "No such file or
        directory" errors from Nix that do not mention git tracking as the
        cause.
      '';
      task-plan = ''
        ---
        name: task-plan
        description: Plan work into a tasks.org file, execute items sequentially, and commit each
        disable-model-invocation: true
        ---

        Before starting any implementation work, create a `tasks.org` file
        in the project root (or update the existing one) with org-mode TODO
        items covering every step needed to complete the request.

        ## Planning phase

        1. Analyze the request and break it into discrete, committable units
           of work.
        2. Write each unit as a TODO item in `tasks.org`:

           ```org
           * TODO Short imperative description
             Context or acceptance criteria if needed.
           ```

        3. Order items so that dependencies come first.
        4. Present the plan to the user and wait for approval before
           proceeding.

        ## Execution phase

        For each item, in order:

        1. Mark it `IN-PROGRESS`:
           ```org
           * IN-PROGRESS Short imperative description
           ```
        2. Implement the change.
        3. Mark it `DONE`:
           ```org
           * DONE Short imperative description
           ```
        4. Stage the relevant files and create a git commit whose message
           summarizes what was done for that item.
        5. Move to the next item.

        ## Rules

        - One commit per task item — do not batch unrelated changes.
        - If a task item turns out to need splitting, add sub-items before
          continuing.
        - If an item is blocked, mark it `BLOCKED`, note the reason, and
          skip to the next unblocked item.
        - When all items are `DONE`, remove `tasks.org` (it served its
          purpose) and report completion.
      '';
    };
  };
  # Capitalism demands I move at full speed or die.  If I die because it
  # blows up on me, that's just bad luck but also life.  I told Claude this
  # and it said "Git is your undo button.  Godspeed.".  The alias is now
  # redundant (settings.json handles it declaratively), but kept as a
  # fallback for shells that load before home-manager's profile.
  programs.claude-code.memory.source = ./claude-memory.org;

  home.shellAliases = {
    claude = "claude --dangerously-skip-permissions";
  };

  home.sessionVariables = {
    # Suppresses the "switched from npm to native installer" banner.  Not in
    # the env-var whitelist that settings.json's `env` section can inject, so
    # it must be set at the shell level instead.
    DISABLE_INSTALLATION_CHECKS = "1";
    # Raises the default 32k cap to match Sonnet 4.6's actual 64k output limit.
    CLAUDE_CODE_MAX_OUTPUT_TOKENS = "64000";
  };

  # ~/.claude.json is a file that `claude` wants to write to for things like
  # update timestamps and change logs.  It still has settings we'd like to
  # manage statically, so we merge in what we want on activation.
  home.activation.claudeConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    if [ ! -f ~/.claude.json ]; then
      run sh -c 'echo "{}" > ~/.claude.json'
    fi
    ${pkgs.jq}/bin/jq '. + {
      "theme": "dark",
      "editorMode": "vim",
      "hasCompletedOnboarding": true,
      "shiftEnterKeyBindingInstalled": false,
      "hasTrustDialogAccepted": true,
      "hasCompletedProjectOnboarding": true,
      "autoUpdates": false
    }' \
      ~/.claude.json > ~/.claude.json.tmp \
      && run mv ~/.claude.json.tmp ~/.claude.json
  '';
}
