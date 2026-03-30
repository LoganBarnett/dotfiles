{
  bash,
  bind,
  coreutils,
  curl,
  gnugrep,
  inetutils,
  nettools,
  openssh,
  writeShellApplication,
  ...
}:

writeShellApplication {
  name = "test-vpn-connectivity";
  runtimeInputs = [
    bash
    bind
    coreutils
    curl
    gnugrep
    inetutils
    nettools
    openssh
  ];
  text = builtins.readFile ../scripts/test-vpn-connectivity;
}
