{ pkgs, ... }: {
  home.packages = [
    # TODO: This would be a handy, generic tool.
    (pkgs.writeShellApplication {
      name = "copilot-tools";
      text = builtins.readFile ../scripts/copilot-tools.sh;
    })
  ];
}
