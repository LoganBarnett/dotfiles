{
  disk-detachable,
  image-create,
  zstd,
  writeShellApplication,
  ...
}:
writeShellApplication {
  name = "image-deploy";
  runtimeInputs = [
    disk-detachable
    image-create
    zstd
  ];
  text = builtins.readFile ../scripts/image-deploy;
}
