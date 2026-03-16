{
  curl,
  jq,
  writeShellApplication,
}:
writeShellApplication {
  name = "secret-server-token-get";
  runtimeInputs = [
    curl
    jq
  ];
  # The script sources ~/.bash-logging at runtime, which Home Manager manages.
  # Shellcheck cannot follow the dynamic source path.
  checkPhase = "";
  text = builtins.readFile ../scripts/secret-server-token-get;
}
