{
  bash,
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

  # Python scripts and custom vpnc script as separate files in the package
  authScript = builtins.readFile ../scripts/gp-auth-headless;
  ptyScript = builtins.readFile ../scripts/gp-connect-pty;
  vpncScript = builtins.readFile ../scripts/vpnc-script-macos;

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

    # Make Python scripts and vpnc script available in PATH for the duration of this script
    AUTH_SCRIPT=$(mktemp)
    PTY_SCRIPT=$(mktemp)
    VPNC_SCRIPT=$(mktemp)
    cat > "$AUTH_SCRIPT" << 'AUTH_EOF'
    ${authScript}
    AUTH_EOF
    cat > "$PTY_SCRIPT" << 'PTY_EOF'
    ${ptyScript}
    PTY_EOF
    cat > "$VPNC_SCRIPT" << 'VPNC_EOF'
    ${vpncScript}
    VPNC_EOF
    chmod +x "$AUTH_SCRIPT" "$PTY_SCRIPT" "$VPNC_SCRIPT"
    export GP_VPNC_SCRIPT="$VPNC_SCRIPT"

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

    # Setup cleanup trap
    trap 'rm -f "$AUTH_SCRIPT" "$PTY_SCRIPT" "$VPNC_SCRIPT"' EXIT

    # Path to openconnect's HIP report CSD wrapper, baked in at build time so
    # that gpclient can submit the host integrity check required to unblock the
    # VPN data plane.
    GP_CSD_WRAPPER="${openconnect}/libexec/openconnect/hipreport.sh"
    export GP_CSD_WRAPPER

    # Now run the main script
    ${builtins.readFile ../scripts/gp-connect-auto}
  '';
}
