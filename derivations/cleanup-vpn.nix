{
  bash,
  coreutils,
  writeShellApplication,
  ...
}:

writeShellApplication {
  name = "cleanup-vpn";
  runtimeInputs = [
    bash
    coreutils
  ];
  text = builtins.readFile ../scripts/cleanup-vpn;
}
