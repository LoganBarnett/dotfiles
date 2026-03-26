################################################################################
# Generator for an Ed25519 DKIM private key in PKCS#8 PEM form.
#
# To extract the public key for the DNS TXT record after generation:
#   openssl pkey -in <(rage --decrypt <secret>.age) -pubout \
#     | grep -v '^-----' | tr -d '\n'
################################################################################
{ pkgs, ... }:
{
  age.generators.stalwart-dkim-key =
    { pkgs, ... }:
    ''
      ${pkgs.openssl}/bin/openssl genpkey -algorithm Ed25519
    '';
}
