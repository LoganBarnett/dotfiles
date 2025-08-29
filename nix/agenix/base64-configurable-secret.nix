{ lib, pkgs, ... }: {
  # Standard Base64 (with padding, no line breaks).
  # settings.length = number of random BYTES before encoding.
  age.generators.base64 = lib.mkForce ({
    decrypt, deps, file, name, pkgs, secret, ...
  }:
    ''
      ${pkgs.openssl}/bin/openssl rand --base64 ${
        toString (secret.settings.length or 32)
      } | tr -d '\n'
    ''
  );

  # Base64URL (URL-safe, typically without padding), good for opaque client
  # secrets.  settings.length = number of random BYTES.
  age.generators.base64url = lib.mkForce ({
    decrypt, deps, file, name, pkgs, secret, ...
  }:
    # `basenc` with --wrap=0 helps make this URL safe (RFC-4648).
    ''
      ${pkgs.openssl}/bin/openssl rand ${
        toString (secret.settings.length or 60)
      } | ${pkgs.coreutils}/bin/basenc --base64url --wrap=0
    ''
  );
}
