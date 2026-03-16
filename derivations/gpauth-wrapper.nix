{
  bash,
  gpclient,
  writeShellApplication,
  ...
}:

# Wrapper for gpauth that allows running as root by delegating to PRIMARY_USER.
# This is used by gpclient when the monitor service runs as root, since gpauth
# refuses to run as root for security reasons.
writeShellApplication {
  name = "gpauth";
  runtimeInputs = [ bash ];
  text = ''
    # If running as root and PRIMARY_USER is set, delegate to that user.
    if [ "$(id -u)" -eq 0 ] && [ -n "$PRIMARY_USER" ]; then
      exec sudo -u "$PRIMARY_USER" ${gpclient}/bin/gpauth "$@"
    else
      # Running as user, call gpauth directly
      exec ${gpclient}/bin/gpauth "$@"
    fi
  '';
}
