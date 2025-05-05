# TODO: This could be made parameterized pretty easily.  Figure it out.
{ bash, writeShellApplication, ... }: let
  name = "ethernet-restart";
  script = "${name}.sh";
in writeShellApplication {
  inherit name;
  runtimeInputs = [ bash ];
  text = builtins.readFile ../scripts/${script};
}
