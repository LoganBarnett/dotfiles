{
  bash,
  coreutils,
  curl,
  gnugrep,
  nettools,
  openssh,
  writeShellApplication,
  ...
}:

writeShellApplication {
  name = "test-vpn-connectivity";
  runtimeInputs = [
    bash
    coreutils
    curl
    gnugrep
    nettools
    openssh
  ];
  text = builtins.readFile ../scripts/test-vpn-connectivity;
}
