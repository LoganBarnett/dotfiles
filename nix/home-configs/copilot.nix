{ pkgs, ... }: {
  home.packages = [
    # TODO: This would be a handy, generic tool.
    (pkgs.writeShellApplication {
      name = "copilot-tools";
      text = ''
        copilot --prompt 'Show me what tools you have available to you.'
      '';
    })
  ];
}
