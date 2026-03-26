################################################################################
# Compact agenix activation script.
#
# The upstream agenix module (ryantm/agenix age.nix) rebuilds the IDENTITIES
# array inside every per-secret block:
#
#   IDENTITIES=()
#   for identity in /etc/ssh/ssh_host_rsa_key ...; do ...
#   done
#
# On a host with ~100 secrets that per-block template adds ~250 bytes x ~100 =
# ~25 KB of duplicate shell code.  The entire activationScript string is passed
# as an environment variable to the nixos-system derivation builder; Linux
# enforces MAX_ARG_STRLEN = PAGE_SIZE * 32 = 131 072 bytes per env var.  A
# large config can easily exceed this limit and cause deployment to fail with
# "Argument list too long".
#
# This module replaces the agenixInstall activation script with a functionally
# identical version that:
#   1. Builds IDENTITIES once before decrypting any secrets.
#   2. Evaluates the secretType.path vs secretsDir/name comparison at Nix
#      evaluation time (not bash runtime), removing the always-false
#      `[ "X" != "X" ]` shell conditionals that the original generates for
#      default-path secrets.
#
# Importing this module is a no-op on Darwin (agenix uses launchd there, not
# system.activationScripts) and when no secrets are defined.
#
# ## Upstream status (as of 2026-03)
#
# This bug is unresolved upstream.  ryantm/agenix issue #227 ("error:
# executing '.../bin/bash': Argument list too long", filed Dec 2023) was
# closed by the reporter after 2+ years with no maintainer response.  PR #228
# ("Introduce installSecretFn to shorten script when many secrets are used")
# took the same approach as this module — hoisting IDENTITIES out of the
# per-secret loop — but was abandoned in Aug 2025 due to maintainer inactivity;
# the author released the code to the public domain.  A broader nixpkgs fix
# (NixOS/nixpkgs PR #274765, passing activation scripts as files rather than
# env vars) has been open since Dec 2023 and is also stalled.
#
# Until one of those lands, this module must remain in place.
################################################################################
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.age;

  # Hoist IDENTITIES construction out of the per-secret loop.
  buildIdentitiesOnce = ''
    IDENTITIES=()
    for identity in ${toString cfg.identityPaths}; do
      test -r "$identity" || continue
      test -s "$identity" || continue
      IDENTITIES+=(-i)
      IDENTITIES+=("$identity")
    done
    test "''${#IDENTITIES[@]}" -eq 0 && echo "[agenix] WARNING: no readable identities found!"
  '';

  # Per-secret block without the redundant IDENTITIES setup.  The path
  # comparison is resolved at Nix evaluation time so that the always-false
  # `[ "X" != "X" ]` shell guards are omitted entirely for default-path
  # secrets.
  installSecret =
    secretType:
    let
      nonDefaultPath = secretType.path != "${cfg.secretsDir}/${secretType.name}";
    in
    ''
      ${
        if secretType.symlink then
          ''_truePath="${cfg.secretsMountPoint}/$_agenix_generation/${secretType.name}"''
        else
          ''_truePath="${secretType.path}"''
      }
      TMP_FILE="$_truePath.tmp"
      mkdir -p "$(dirname "$_truePath")"
      ${lib.optionalString nonDefaultPath ''mkdir -p "$(dirname "${secretType.path}")"''}
      (
        umask u=r,g=,o=
        test -f "${secretType.file}" || echo '[agenix] WARNING: encrypted file ${secretType.file} does not exist!'
        test -d "$(dirname "$TMP_FILE")" || echo "[agenix] WARNING: $(dirname "$TMP_FILE") does not exist!"
        LANG=${
          config.i18n.defaultLocale or "C"
        } ${cfg.ageBin} --decrypt "''${IDENTITIES[@]}" -o "$TMP_FILE" "${secretType.file}"
      )
      chmod ${secretType.mode} "$TMP_FILE"
      mv -f "$TMP_FILE" "$_truePath"
      ${lib.optionalString (
        secretType.symlink && nonDefaultPath
      ) ''ln -sfT "${cfg.secretsDir}/${secretType.name}" "${secretType.path}"''}
    '';

  testIdentities = lib.concatMapStrings (path: ''
    test -f ${path} || echo '[agenix] WARNING: config.age.identityPaths entry ${path} not present!'
  '') cfg.identityPaths;

  # Copied verbatim from agenix age.nix so this module is self-contained.
  cleanupAndLink = ''
    _agenix_generation="$(basename "$(readlink ${cfg.secretsDir})" || echo 0)"
    (( ++_agenix_generation ))
    echo "[agenix] symlinking new secrets to ${cfg.secretsDir} (generation $_agenix_generation)..."
    ln -sfT "${cfg.secretsMountPoint}/$_agenix_generation" ${cfg.secretsDir}

    (( _agenix_generation > 1 )) && {
      echo "[agenix] removing old secrets (generation $(( _agenix_generation - 1 )))..."
      rm -rf "${cfg.secretsMountPoint}/$(( _agenix_generation - 1 ))"
    }
  '';

  compactInstallSecrets = lib.concatStringsSep "\n" (
    [ "echo '[agenix] decrypting secrets...'" ]
    ++ [ testIdentities ]
    ++ [ buildIdentitiesOnce ]
    ++ (map installSecret (builtins.attrValues cfg.secrets))
    ++ [ cleanupAndLink ]
  );
in
# Apply only on Linux; agenix uses launchd on Darwin and does not set
# system.activationScripts at all.
lib.mkIf (cfg.secrets != { } && !pkgs.stdenv.isDarwin) {
  system.activationScripts.agenixInstall.text = lib.mkForce compactInstallSecrets;
}
