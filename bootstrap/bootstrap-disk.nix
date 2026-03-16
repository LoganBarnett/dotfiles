{
  ...
}: {
  name = "bootstrap-disk";
  installPhase = ''
    cp ${./bootstrap-disk.sh} $out/bin
  '';
}
