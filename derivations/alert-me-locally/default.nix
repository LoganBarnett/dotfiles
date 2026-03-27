{ writeShellApplication, sox }:
writeShellApplication {
  name = "alert-me-locally";
  runtimeInputs = [ sox ];
  text = builtins.readFile ./alert-me-locally;
}
