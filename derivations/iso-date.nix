{
  nodejs,
  pkgs,
  writeShellApplication,
}:
let
  # Store the Node.js source separately so the shell wrapper can reference it
  # by store path rather than embedding the script in a heredoc (which would
  # interfere with the script's use of process.stdin).
  iso-date-js = pkgs.writeTextFile {
    name = "iso-date.js";
    text = builtins.readFile ../scripts/iso-date;
  };
in
writeShellApplication {
  name = "iso-date";
  runtimeInputs = [ nodejs ];
  text = ''
    exec node ${iso-date-js} "$@"
  '';
}
