{ lib, ... }: {
  home.file.".claude/settings.json".text = builtins.toJSON {
    # We can't just use a shell expression here.  It must be an executable
    # program that takes zero arguments.
    apiKeyHelper = "~/bin/claude-code-api-key.sh";
    # This doesn't actually work but it is listed in the global configuration
    # (claude config list --global).
    editorMode = "vim";
    model = "claude-sonnet-4-5-20250929";
    # model = "claude-opus-4-1-20250805";
    mcp = {
      allowedDirectories = {
        read = [
          # Allowing specific directories to be read is kind of a hassle,
          # especially if we're looping over builds which will generally produce
          # new directories in the store.  Just allow reading the whole thing,
          # since nothing should ever be sensitive in there anyways.
          "/nix/store"
        ];
      };
    };
  };
}
