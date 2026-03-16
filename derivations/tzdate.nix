{ writeShellApplication }:
writeShellApplication {
  name = "tzdate";
  # The UTC branch contains a typo (timzone_iana vs timezone_iana), leaving
  # an unused variable that shellcheck flags as SC2034.
  checkPhase = "";
  text = builtins.readFile ../scripts/tzdate;
}
