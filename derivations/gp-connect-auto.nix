{
  bash,
  callPackage,
  coreutils,
  expect,
  gpclient,
  openconnect,
  pass,
  python3,
  writeShellApplication,
  ...
}:

let
  # Python with Playwright
  pythonWithPlaywright = python3.withPackages (
    ps: with ps; [
      playwright
    ]
  );

  # Pass with OTP extension
  passWithOtp = pass.withExtensions (exts: [ exts.pass-otp ]);

  # Python scripts as separate files in the package.
  authScript = builtins.readFile ../scripts/gp-auth-headless;
  ptyScript = builtins.readFile ../scripts/gp-connect-pty;

  # vpnc-script as a derivation so gpclient references a stable Nix store
  # path.  darwin-rebuild switch updates it immediately without needing a
  # daemon restart or VPN reconnect.
  vpncScriptPkg = callPackage ./vpnc-script-macos.nix { };

  name = "gp-connect-auto";
in
writeShellApplication {
  inherit name;

  runtimeInputs = [
    bash
    coreutils
    expect
    gpclient
    passWithOtp
    pythonWithPlaywright
  ];

  # The bash script with embedded Python script
  text = ''
    # Install Playwright browsers on first run (in user's home directory)
    if [ ! -d "$HOME/.cache/ms-playwright" ]; then
      echo "Installing Playwright browsers (first run only)..."
      ${pythonWithPlaywright}/bin/playwright install chromium
    fi

    # Make Python scripts available for this session as temp files — they
    # are only needed while gp-connect-auto runs and can be safely deleted
    # on exit.
    #
    # The vpnc-script is a Nix derivation referenced by store path so that
    # darwin-rebuild switch updates it immediately without needing a daemon
    # restart or VPN reconnect.
    AUTH_SCRIPT=$(mktemp)
    PTY_SCRIPT=$(mktemp)
    cat > "$AUTH_SCRIPT" << 'AUTH_EOF'
    ${authScript}
    AUTH_EOF
    cat > "$PTY_SCRIPT" << 'PTY_EOF'
    ${ptyScript}
    PTY_EOF
    chmod +x "$AUTH_SCRIPT" "$PTY_SCRIPT"
    export GP_VPNC_SCRIPT="${vpncScriptPkg}/bin/vpnc-script-macos"

    # Create wrappers for the Python scripts
    # shellcheck disable=SC2329  # Function invoked indirectly from gp-connect-pty.py
    gp-auth-headless.py() {
      ${pythonWithPlaywright}/bin/python3 "$AUTH_SCRIPT" "$@"
    }
    gp-connect-pty.py() {
      ${pythonWithPlaywright}/bin/python3 "$PTY_SCRIPT" "$@"
    }
    export -f gp-auth-headless.py
    export -f gp-connect-pty.py

    # Clean up temp scripts on exit.  The vpnc-script lives in the Nix
    # store and does not need cleanup.
    trap 'rm -f "$AUTH_SCRIPT" "$PTY_SCRIPT"' EXIT

    # Path to openconnect's HIP report CSD wrapper, baked in at build time so
    # that gpclient can submit the host integrity check required to unblock the
    # VPN data plane.
    GP_CSD_WRAPPER="${openconnect}/libexec/openconnect/hipreport.sh"
    export GP_CSD_WRAPPER

    # Now run the main script
    ${builtins.readFile ../scripts/gp-connect-auto}
  '';
}
