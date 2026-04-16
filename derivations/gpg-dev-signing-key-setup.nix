{
  gnupg,
  rage,
  writeShellApplication,
  ...
}:
writeShellApplication {
  name = "gpg-dev-signing-key-setup";
  runtimeInputs = [
    gnupg
    rage
  ];
  text = builtins.readFile ../scripts/gpg-dev-signing-key-setup;
}
