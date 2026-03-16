# GlobalProtect VPN Automation - Session Context

## Problem Solved

Safari could not resolve internal `.pvt` domains (e.g., `stash.americas.nwea.pvt`) when
connected to GlobalProtect VPN, even though `curl` worked fine.  The error was "Safari
can't find the server," returned instantly.

## Root Cause

macOS uses **scoped DNS resolution**.  Safari queries DNS servers associated with specific
network interfaces.  gpclient was configuring DNS at
`State:/Network/Service/utun5/DNS` but without `SupplementalMatchDomains` that
included the `.nwea.pvt` domains, so Safari's scoped resolver never used the VPN DNS
servers for those domains.

Additionally, `gpclient` on macOS does **not** call vpnc-scripts even when `--script` is
passed — it silently ignores them.

## Solution

Created `scripts/fix-vpn-dns-scoping.sh` which:

1. Finds the active VPN tunnel interface (utun with a 10.89.x.x IP)
2. Uses `sudo /usr/sbin/scutil` to write DNS configuration including
   `SupplementalMatchDomains` covering `.nwea.pvt`, `.nwea.pvt`, `.nweacolo.pvt`, and
   all the work domains
3. Notifies the system to reconfigure DNS

This script is called automatically by `gp-monitor` after each successful VPN connection.

## Files Changed

### New files

- `nix/scripts/fix-vpn-dns-scoping.sh` — the DNS fix script
- `nix/derivations/fix-vpn-dns-scoping.nix` — nix derivation wrapping the script
- `nix/scripts/vpnc-script-macos.sh` — macOS vpnc-script (not called by gpclient but
  kept for documentation/future use)

### Modified files

- `nix/darwin.nix` — added `sudo /usr/sbin/scutil` to `security.sudo.extraConfig`
  NOPASSWD rules
- `nix/derivations/gp-monitor.nix` — added `fix-vpn-dns-scoping` as a runtime dep
- `nix/scripts/gp-monitor.sh` — calls `fix-vpn-dns-scoping` after VPN connects
- `nix/derivations/gp-connect-auto.nix` — added vpnc-script wiring (unused by gpclient)
- `nix/scripts/gp-connect-auto.sh` — passes `--script` to gpclient (ignored on macOS)

## Key Technical Details

- **scutil path**: `/usr/sbin/scutil` is the correct absolute path for sudo rules
- **Service path for DNS**: `State:/Network/Service/<tundev>/DNS`
- **The magic key**: `SupplementalMatchDomains` must list all VPN domains for
  `dns-sd` (and therefore Safari) to route those queries through VPN DNS servers
- **Test command**: `dns-sd -G v4 stash.americas.nwea.pvt` — should resolve to
  10.210.16.252 when working
- **VPN interface detection**: `ifconfig | grep -B 3 "inet 10.89" | grep "^utun"`

## Architecture

```
launchd daemon: org.nixos.globalprotect-monitor
  → gp-monitor (bash)
    → gp-connect-auto (bash, runs as root via sudo -E)
      → gp-connect-pty.py (PTY wrapper)
        → gpclient connect --browser remote --script <vpnc-script>
          (gpclient ignores --script on macOS)
    → fix-vpn-dns-scoping (called after VPN connects)
      → sudo /usr/sbin/scutil (sets SupplementalMatchDomains)
```

## Sudoers Rules (darwin.nix)

```
logan.barnett ALL=(root) NOPASSWD: /run/current-system/sw/bin/darwin-rebuild
logan.barnett ALL=(root) NOPASSWD: SETENV: /run/current-system/sw/bin/gp-connect-auto
logan.barnett ALL=(root) NOPASSWD: /usr/sbin/scutil
```

## Pending / Open Items

- Changes are staged in git but not yet committed
- The vpnc-script approach is dead code — gpclient on macOS silently ignores `--script`.
  Could be removed in a cleanup pass, or kept for documentation.
- If VPN changes gateways or DNS servers change, `fix-vpn-dns-scoping.sh` has them
  hardcoded and will need updating

## Verification

After VPN connects, run:

```bash
dns-sd -G v4 stash.americas.nwea.pvt
# Should resolve to 10.210.16.252 within a few seconds
```

If it shows "No Such Record", re-run `fix-vpn-dns-scoping` manually (it's in PATH via
the nix store when the monitor package is active, or run the script directly with bash).
