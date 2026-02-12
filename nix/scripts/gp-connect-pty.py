#!/usr/bin/env python3
"""
GlobalProtect connection automation using PTY.

This script creates a pseudo-TTY for gpclient to bypass TTY checks,
monitors output for authentication URLs, runs headless browser automation,
and sends the callback back to gpclient.
"""

import os
import pty
import re
import select
import subprocess
import sys
import time

def log(message):
    """Log with timestamp to stderr."""
    print(f"[gp-pty] {message}", file=sys.stderr, flush=True)

def run_gpclient_with_pty(gp_cmd, username, password, totp_code, auth_script):
    """Run gpclient with a pseudo-TTY and handle authentication."""

    # Create pseudo-TTY
    master, slave = pty.openpty()

    log(f"Starting gpclient: {gp_cmd}")

    # Start gpclient with the slave PTY as stdin/stdout/stderr
    process = subprocess.Popen(
        gp_cmd,
        shell=True,
        stdin=slave,
        stdout=slave,
        stderr=slave,
        close_fds=True
    )

    # Close slave in parent process
    os.close(slave)

    # Make master non-blocking
    import fcntl
    flags = fcntl.fcntl(master, fcntl.F_GETFL)
    fcntl.fcntl(master, fcntl.F_SETFL, flags | os.O_NONBLOCK)

    auth_handled = False
    output_buffer = ""

    try:
        while True:
            # Check if process is still running
            if process.poll() is not None:
                log(f"gpclient exited with code {process.returncode}")
                break

            # Wait for output from gpclient
            ready, _, _ = select.select([master], [], [], 0.1)

            if ready:
                try:
                    data = os.read(master, 1024).decode('utf-8', errors='replace')
                    if data:
                        # Print output to stdout
                        print(data, end='', flush=True)
                        output_buffer += data

                        # Look for authentication URL (only process once)
                        if not auth_handled and re.search(r'http://[0-9.]+:[0-9]+/[a-f0-9-]+', data):
                            auth_url_match = re.search(r'http://[0-9.]+:[0-9]+/[a-f0-9-]+', output_buffer)
                            if auth_url_match:
                                auth_url = auth_url_match.group(0)
                                auth_handled = True

                                log(f"Authentication URL detected: {auth_url}")
                                log("Running headless browser automation...")

                                # Run the authentication script via Python (same interpreter as this script)
                                try:
                                    result = subprocess.run(
                                        [sys.executable, auth_script, auth_url, username, password, totp_code],
                                        capture_output=True,
                                        text=True,
                                        timeout=120
                                    )

                                    if result.returncode == 0 and result.stdout.strip():
                                        callback_url = result.stdout.strip()
                                        log(f"Callback URL received ({len(callback_url)} chars)")
                                        log("Sending callback to gpclient...")

                                        # Send callback to gpclient
                                        os.write(master, (callback_url + "\n").encode('utf-8'))
                                        time.sleep(1)

                                        # Send Enter to select default gateway
                                        log("Sending Enter to select default gateway...")
                                        os.write(master, b"\n")

                                    else:
                                        log(f"Authentication script failed: {result.stderr}")
                                        process.terminate()
                                        return 1

                                except subprocess.TimeoutExpired:
                                    log("Authentication script timed out")
                                    process.terminate()
                                    return 1
                                except Exception as e:
                                    log(f"Error running authentication script: {e}")
                                    process.terminate()
                                    return 1

                except OSError:
                    # No more data
                    pass

    except KeyboardInterrupt:
        log("Interrupted by user")
        process.terminate()
        return 130

    finally:
        os.close(master)

    # Wait for process to finish
    process.wait()
    return process.returncode

if __name__ == "__main__":
    if len(sys.argv) < 6:
        print("Usage: gp-connect-pty.py <gp_cmd> <username> <password> <totp_code> <auth_script>", file=sys.stderr)
        sys.exit(1)

    gp_cmd = sys.argv[1]
    username = sys.argv[2]
    password = sys.argv[3]
    totp_code = sys.argv[4]
    auth_script = sys.argv[5]

    exit_code = run_gpclient_with_pty(gp_cmd, username, password, totp_code, auth_script)
    sys.exit(exit_code)
