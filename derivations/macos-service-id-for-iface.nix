{ writeShellApplication }:
writeShellApplication {
  name = "macos-service-id-for-iface";
  # The script embeds scutil invocations inside awk via process substitution,
  # which shellcheck cannot follow.
  checkPhase = "";
  text = builtins.readFile ../scripts/macos-service-id-for-iface;
}
