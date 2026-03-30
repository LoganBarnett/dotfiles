{
  bash,
  coreutils,
  inetutils,
  writeShellApplication,
  ...
}:

writeShellApplication {
  name = "vpn-test-harness-recover";
  runtimeInputs = [
    bash
    coreutils
    inetutils
  ];
  text = builtins.readFile ../scripts/vpn-test-harness-recover;
}
