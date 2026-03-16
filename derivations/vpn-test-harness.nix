{
  bash,
  coreutils,
  writeShellApplication,
  ...
}:

writeShellApplication {
  name = "vpn-test-harness";
  runtimeInputs = [
    bash
    coreutils
  ];
  text = builtins.readFile ../scripts/vpn-test-harness;
}
