{
  bash,
  coreutils,
  writeShellApplication,
  ...
}:

writeShellApplication {
  name = "vpn-test-harness-recover";
  runtimeInputs = [
    bash
    coreutils
  ];
  text = builtins.readFile ../scripts/vpn-test-harness-recover;
}
