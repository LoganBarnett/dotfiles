{ lib, ... }: {
  home.file.".claude/settings.json".text = builtins.toJSON {
    # We can't just use a shell expression here.  It must be an executable
    # program that takes zero arguments.
    apiKeyHelper = "~/bin/claude-code-api-key.sh";
    # This doesn't actually work but it is listed in the global configuration
    # (claude config list --global).
    editorMode = "vim";
    model = "claude-opus-4-1-20250805";
  };
}
