{ writeShellApplication, sox }:
writeShellApplication {
  name = "sonification-test";
  runtimeInputs = [ sox ];
  text = builtins.readFile ./sonification-test;
}
