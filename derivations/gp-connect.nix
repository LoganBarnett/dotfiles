{ bash, gpclient, writeShellApplication, ... }: let
  name = "gp-connect";
  script = "${name}.sh";
in writeShellApplication {
  inherit name;
  runtimeInputs = [ bash gpclient ];
  text = builtins.readFile ../scripts/${script};
}
