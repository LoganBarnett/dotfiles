{
  gnused,
  pass,
  writeShellApplication,
  ...
}:
writeShellApplication {
  name = "passn";
  runtimeInputs = [
    gnused
    pass
  ];
  text = builtins.readFile ../scripts/passn;
}
