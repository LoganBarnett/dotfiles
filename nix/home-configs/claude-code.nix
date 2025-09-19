{ lib, ... }: {
  home.file.".claude/settings.json" = builtins.toJSON {
    # We can't just use a shell expression here.  It must be an executable
    # program that takes zero arguments.
    apiKeyHelper = "~/bin/claude-code-api-key.sh";
    model = "claude-opus-4-1-20250805";
  };
}
