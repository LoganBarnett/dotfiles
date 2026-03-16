{
  gawk,
  git,
  openssh,
  rsync,
  writeShellApplication,
}:
writeShellApplication {
  name = "remote-deploy";
  runtimeInputs = [
    gawk
    git
    openssh
    rsync
  ];
  # The script has several unquoted variable expansions and non-standard
  # constructs that shellcheck rejects.
  checkPhase = "";
  text = builtins.readFile ../scripts/remote-deploy;
}
