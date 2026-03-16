{
  git,
  image-create,
  openssh,
  writeShellApplication,
}:
writeShellApplication {
  name = "rpi-host-new";
  runtimeInputs = [
    git
    image-create
    openssh
  ];
  text = builtins.readFile ../scripts/rpi-host-new;
}
