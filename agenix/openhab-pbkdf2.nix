################################################################################
# An agenix-rekey generator that hashes an OpenHAB admin password using
# PBKDF2WithHmacSHA512 and outputs a users.json-compatible JSON object.
#
# Parameters match OpenHAB's UserRegistryImpl.java constants:
#   - Algorithm: PBKDF2WithHmacSHA512
#   - Iterations: 65536 (PASSWORD_ITERATIONS)
#   - Key length: 512 bits / 64 bytes (KEY_LENGTH)
#   - Salt length: 512 bits / 64 bytes (KEY_LENGTH / 8)
#
# OpenHAB stores the salt as a Base64 string and hashes using the UTF-8 bytes
# of that Base64 string directly (not the decoded bytes).  See
# UserRegistryImpl.hash(): `byte[] bytes = salt.getBytes()`.
#
# Expects exactly one dependency: the plaintext admin password secret.
# Outputs a JSON object suitable for writing to OpenHAB's jsondb/users.json.
################################################################################
{ lib, pkgs, ... }:
{
  age.generators.openhab-pbkdf2 =
    {
      decrypt,
      deps,
      file,
      name,
      pkgs,
      secret,
      ...
    }:
    let
      passwordDep = builtins.head deps;
      # Python script written to the Nix store for clean quoting.
      script = pkgs.writeText "openhab-pbkdf2.py" ''
        import hashlib, base64, os, json, sys
        password = sys.argv[1].strip()
        salt_bytes = os.urandom(64)
        salt_b64 = base64.b64encode(salt_bytes).decode()
        # OpenHAB hashes using the UTF-8 bytes of the Base64 salt string, not
        # the decoded bytes.  Match UserRegistryImpl.hash(): salt.getBytes().
        hash_bytes = hashlib.pbkdf2_hmac('sha512', password.encode(), salt_b64.encode(), 65536, 64)
        hash_b64 = base64.b64encode(hash_bytes).decode()
        print(json.dumps({
          'admin': {
            'class': 'org.openhab.core.auth.ManagedUser',
            'value': {
              'name': 'admin',
              'passwordHash': hash_b64,
              'passwordSalt': salt_b64,
              'roles': ['administrator'],
              'sessions': [],
              'apiTokens': [],
            }
          }
        }))
      '';
    in
    ''
      ${pkgs.python3}/bin/python3 ${script} \
        "$(${decrypt} ${lib.escapeShellArg passwordDep.file})"
    '';
}
