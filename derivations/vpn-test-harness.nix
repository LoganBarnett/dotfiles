{
  bash,
  bind,
  coreutils,
  inetutils,
  writeShellApplication,
  ...
}:

writeShellApplication {
  name = "vpn-test-harness";
  runtimeInputs = [
    bash
    bind
    coreutils
    inetutils
  ];
  text = builtins.readFile ../scripts/vpn-test-harness;
}
