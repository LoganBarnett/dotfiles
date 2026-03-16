{ bundlerApp }: bundlerApp {
  pname = "hiera-eyaml";
  gemdir = ./.;
  exes = [ "eyaml" ];

}
