final: prev: {
  # Override gpauth to support running as root.
  # The wrapper delegates to sudo -u when running as root, since gpauth refuses
  # to run as root for security reasons.
  gpauth = prev.gpauth.overrideAttrs (oldAttrs: {
    # Replace the gpauth binary with our wrapper
    postInstall = (oldAttrs.postInstall or "") + ''
            # Save the real gpauth
            mv $out/bin/gpauth $out/bin/gpauth-real

            # Create wrapper that uses sudo -u when running as root
            cat > $out/bin/gpauth <<'EOF'
      #!/usr/bin/env bash
      # Wrapper for gpauth to allow running as root by delegating to PRIMARY_USER.

      if [ "$(id -u)" -eq 0 ] && [ -n "$PRIMARY_USER" ]; then
        # Running as root, delegate to PRIMARY_USER
        exec sudo -u "$PRIMARY_USER" "$(dirname "$0")/gpauth-real" "$@"
      else
        # Running as user, call gpauth directly
        exec "$(dirname "$0")/gpauth-real" "$@"
      fi
      EOF
            chmod +x $out/bin/gpauth
    '';
  });
}
