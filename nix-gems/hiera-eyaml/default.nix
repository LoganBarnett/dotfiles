{ bundlerApp, libxml2 }: bundlerApp {
  pname = "hiera-eyaml";
  gemdir = ./.;
  exes = [ "eyaml" ];

  buildInputs = [
    libxml2
  ];
}
